-- | Path construction DSL.
--
-- Build paths with a monadic-style interface using @PathBuilder@:
--
-- @
-- heart :: Path
-- heart = buildPath $ do
--   startAt (V2 0 (-40))
--   cubicTo (V2 (-50) (-80)) (V2 (-100) (-20)) (V2 0 30)
--   cubicTo (V2 100 (-20)) (V2 50 (-80)) (V2 0 (-40))
--   closePath
-- @
module GBVector.Path
  ( -- * Path Builder
    PathBuilder,
    buildPath,

    -- * Builder Operations
    startAt,
    lineTo,
    cubicTo,
    quadTo,
    arcTo,
    closePath,

    -- * Convenience Constructors
    polylinePath,
    polygonPath,
  )
where

import GBVector.Types (ArcParams, Path (..), Segment (..), V2 (..))

-- ---------------------------------------------------------------------------
-- Path Builder
-- ---------------------------------------------------------------------------

-- | Accumulated state for building a path.
data BuilderState = BuilderState
  { bsStart :: !V2,
    bsSegments :: ![Segment],
    bsClosed :: !Bool
  }

-- | A path builder that accumulates segments.
newtype PathBuilder a = PathBuilder (BuilderState -> (a, BuilderState))

instance Functor PathBuilder where
  fmap f (PathBuilder g) = PathBuilder $ \s ->
    let (a, st) = g s in (f a, st)

instance Applicative PathBuilder where
  pure a = PathBuilder $ \s -> (a, s)
  PathBuilder f <*> PathBuilder x = PathBuilder $ \s ->
    let (fab, s1) = f s
        (a, s2) = x s1
     in (fab a, s2)

instance Monad PathBuilder where
  PathBuilder m >>= k = PathBuilder $ \s ->
    let (a, s1) = m s
        PathBuilder n = k a
     in n s1

-- | Run a t'PathBuilder' to produce a t'Path'.
buildPath :: PathBuilder () -> Path
buildPath (PathBuilder f) =
  let ((), st) = f emptyState
   in Path
        { pathStart = bsStart st,
          pathSegments = reverse (bsSegments st),
          pathClosed = bsClosed st
        }

-- ---------------------------------------------------------------------------
-- Builder Operations
-- ---------------------------------------------------------------------------

-- | Set the starting point of the path.
startAt :: V2 -> PathBuilder ()
startAt v = PathBuilder $ \s -> ((), s {bsStart = v})

-- | Add a straight line to the given point.
lineTo :: V2 -> PathBuilder ()
lineTo v = PathBuilder $ \s ->
  ((), s {bsSegments = LineTo v : bsSegments s})

-- | Add a cubic bezier curve with two control points and an endpoint.
cubicTo :: V2 -> V2 -> V2 -> PathBuilder ()
cubicTo c1 c2 end = PathBuilder $ \s ->
  ((), s {bsSegments = CubicTo c1 c2 end : bsSegments s})

-- | Add a quadratic bezier curve with one control point and an endpoint.
quadTo :: V2 -> V2 -> PathBuilder ()
quadTo c end = PathBuilder $ \s ->
  ((), s {bsSegments = QuadTo c end : bsSegments s})

-- | Add an elliptical arc to the given endpoint.
arcTo :: ArcParams -> V2 -> PathBuilder ()
arcTo params end = PathBuilder $ \s ->
  ((), s {bsSegments = ArcTo params end : bsSegments s})

-- | Close the path back to the start point.
closePath :: PathBuilder ()
closePath = PathBuilder $ \s -> ((), s {bsClosed = True})

-- ---------------------------------------------------------------------------
-- Convenience Constructors
-- ---------------------------------------------------------------------------

-- | Build an open path through a list of points.
-- Returns an empty path if the list has fewer than two points.
polylinePath :: [V2] -> Path
polylinePath [] = emptyPath
polylinePath [_] = emptyPath
polylinePath (start : rest) =
  Path
    { pathStart = start,
      pathSegments = map LineTo rest,
      pathClosed = False
    }

-- | Build a closed path through a list of points.
-- Returns an empty path if the list has fewer than three points.
polygonPath :: [V2] -> Path
polygonPath [] = emptyPath
polygonPath [_] = emptyPath
polygonPath [_, _] = emptyPath
polygonPath (start : rest) =
  Path
    { pathStart = start,
      pathSegments = map LineTo rest,
      pathClosed = True
    }

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

origin :: V2
origin = V2 0 0

emptyState :: BuilderState
emptyState =
  BuilderState
    { bsStart = origin,
      bsSegments = [],
      bsClosed = False
    }

emptyPath :: Path
emptyPath = Path origin [] False
