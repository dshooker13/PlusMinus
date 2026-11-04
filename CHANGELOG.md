# Changelog

## 2.0.0 (2026-11-03)

The single `PlusMinus.wl` file has become a paclet with tests and CI. Code that loaded the old file keeps working: `PlusMinus.wl` at the repository root now loads the new package.

### New

- `PmQ`: does an expression contain ± or ∓?
- `PmMap`: apply any function to both branches, e.g. `PmMap[D[#, x] &, expr]`.
- `PmThread`: like `PmArithmetic`, but takes any number of arguments.
- `PmSimplify` and `PmFullSimplify`, with optional assumptions.
- `PmEqualQ`: True/False version of `PmCompare`.
- `PmFlip`: swaps ± and ∓.
- `PmSolve`: runs `Solve` and merges a pair of solutions into one ± solution (quadratic formula and similar).
- `PmToInterval`: the `Interval` spanned by the two branches of a numeric expression.
- `"Simplifier"` option on every function that recombines branches. It defaults to `Simplify` (`Expand` for `PmExpand`).
- Unary `±x`, n-ary `a ± b ± c` and nested `a ± (b ± c)` are now understood everywhere.
- `PmArithmetic` is the correctly spelled name. `PmArithmatic` still works as an alias.

### Fixed

- `PmExpand` failed on expressions without ±. It now just expands them.
- `PmExpand` did not understand ∓. It now handles ∓ like every other function.
- `PmBase` gave wrong answers whenever ± sat inside a nonlinear expression: `PmBase[(a ± b)^2]` returned `a^2` instead of `a^2 + b^2`, and `PmPm` inherited the error. Both now use `(p + m)/2` and `(p - m)/2` of the branches, which is right for any expression, and simplify the result.
- `PmCompare` returned `Thread[True]` instead of `{True, True}` when the two parts were identical.
- `PmArithmatic::usage` was never set (missing `=`), and its usage string had a stray comment marker.
- The usage messages for `PmToList` and `PmPm` described the wrong functions.
- `PmFromList` now gives a message on input that is not a two-element list, instead of failing with `Part` errors.

### Changed

- `PmExpand` now expands the two parts of the result instead of simplifying them, so they stay expanded. Use `PmExpand[expr, "Simplifier" -> Simplify]` for the old behaviour.
- `PmCommands` is now built on `Fold` instead of a `For` loop. Its results are unchanged.

## 1.0 (2024)

The original `PlusMinus.wl`: `PmExpand`, `PmFromList`, `PmToList`, `PmBase`, `PmPm`, `PmParts`, `PmCompare`, `PmArithmatic`, `PmCommands`.
