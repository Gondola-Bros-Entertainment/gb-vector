# CLAUDE.md

## Rules

- **Only commit and push when explicitly instructed.** Never amend commits. Never add `Co-Authored-By` headers.
- Run `cabal test` and `cabal build --ghc-options="-Werror"` before considering any change complete.
- **`-Wall -Wcompat` clean.** All code compiles without warnings under `-Wall -Wcompat`. Warnings are errors — no exceptions, no suppressions.
- **ALWAYS run `ormolu -m inplace` AND `hlint` on changed files before committing.**

## Haskell Style

- **Pure by default.** Everything is a pure function. IO lives in exactly one place: `SVG.hs` for `writeSvg`. Color math, path geometry, bezier subdivision, boolean operations, SVG serialization — all pure. No `IORef`, no `STRef`, no mutable state anywhere.
- **Let the types guide.** Write the type signature first. If the type is right, the implementation follows. If the implementation is awkward, the type is wrong. Types are the design — code is the consequence.
- **Parametricity.** Write the most general type that works. The more polymorphic the signature, the fewer places for bugs to hide.
- **Total functions only.** Never use `head`, `tail`, `!!`, `fromJust`, `read`, or any partial function. Use pattern matching, `maybe`, `either`, safe alternatives. Every function must handle every input.
- **Strict by default.** Bang patterns on all data fields and accumulators. Strict `foldl'` over lazy `foldl`. No space leaks. Do not over-strict function arguments — only fields and accumulators need bangs.
- **Types encode invariants.** `V2` for points, `Path` for geometry, `Element` for the scene tree. Exhaustive pattern matches on `Element` and `Segment`. Make illegal states unrepresentable. No stringly-typed code, no boolean blindness.
- **Composition is the API.** `Element` is a recursive sum type — style and transforms are constructors wrapping children. Users compose with function application (`translate 50 50 $ fill gold $ circle 30`) or left-to-right with `(&)`. No builder objects, no mutable canvas, no imperative drawing commands.
- **Lawful abstractions.** `Semigroup`/`Monoid` on `Element` via `EGroup`. Every typeclass instance must respect its laws.
- **Small, composable functions.** Each function does one thing. Compose pipelines of pure transforms. If a function is hard to name, it's doing too much.
- **Named constants.** No magic numbers or hardcoded strings anywhere. Every tolerance, precision, default radius, and SVG attribute name gets a name.
- **No prime-mark variables.** Never use `x'`, `x''`, `s'`, etc. Use descriptive names: `subdivided`, `flattened`, `offset`, `clipped`, `rng1`/`rng2`.
- **Idiomatic patterns.** Guards over nested if/then/else. Pattern matching over boolean checks. `where` for local definitions. Explicit export lists on all modules. No orphan instances.
- **Clean module structure.** Group by responsibility. Each module has a clear single purpose, an explicit export list, and a documented role.

## Dependencies

**`base` + `text` only.** Both are GHC boot libraries. Zero external dependencies. This constraint is non-negotiable — it keeps the library lightweight, fast to compile, and free of transitive dependency churn.

## Context

Pure Haskell SVG vector graphics library. Third library in the Gondola Bros Hackage ecosystem (`gb-sprite`: raster, `gb-synth`: audio, `gb-vector`: vector). Generates SVG from a composable `Element` tree — no XML library, no IO, just pure functions building `Text`.
