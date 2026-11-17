# Changelog

## 2.1.0 (unreleased)

From an audit of the package. Results only change where they used to be wrong, except for `PmBranches` and the time-limited default simplifier.

### New

- `PmBranches[p, m]`: returned when the branches can't be written as `a ± b` (an infinite or Indeterminate branch, branches with different conditions, Booleans, strings, intervals, `$Failed`, rules with different left sides, or structures of different shapes). The branch-by-branch functions understand it. `PmParts`, `PmBase`, `PmPm` and `PmCompare` give `::nobase` and `$Failed` for branches without a base and ± part.
- `PmSolve` solves equations that already contain ± branch by branch, so every root is kept. Roots that don't depend on the sign come out plain.
- `Scripts/BuildPaclet.wls` builds a paclet archive with only the package files.
- `Tests/Properties.wlt`: property tests against an independent reference implementation.

### Fixed

- Equations, inequalities, logical combinations, rules and lists were averaged when recombining: `PmSimplify[a ± b == c]` and `PmSimplify[PmSolve[...]]` gave half-sums of equations and rules. They are now combined element by element.
- One infinite branch made both halves infinite: `PmArithmetic[2, 1 ± 1, Divide]` gave `{Indeterminate, Indeterminate}` as branches.
- Branches with different conditions (`ConditionalExpression`) were merged under both conditions, so a valid branch became `Undefined`. This also lost real roots in `PmSolve`.
- `PmSolve[x^2 == 4 ± 1, x]` returned 2 of the 4 roots.
- `PmSolve` gave `Less::nord` for complex roots, added a second message when `Solve` failed, and said "2 solution(s), not 2" when the solutions were for different variables.
- A trailing rule argument was taken as an option: `PmThread[ReplaceAll, x ± 3x, x -> 2]` dropped the rule.
- `PmSimplify`/`PmFullSimplify` passed any extra argument straight to `Simplify`, leaving unevaluated `Simplify[...]` inside the result.
- Wrong calls returned unevaluated with no message. They now give `::pmargs`.
- Public symbols weren't protected, so `PmToList = 5` broke every function, and reloading kept stale definitions.
- An inexact zero half was kept: `PmFromList[{1.5, 1.5}]` gave `1.5 ± 0.`.
- A sign inside a `SparseArray` was invisible. `PlusMinus[]` was never resolved. Held expressions got `1*(Plus[b])` junk.
- The test runner silently skipped 11 tests, never checked expected messages, and exited 0 after a syntax error, an abort or an `Exit[]` in a test file. It now uses `TestReport` and checks the number of tests run. Several tests couldn't fail; they now can.

### Removed

- `PmToInterval` (removed in c5d1f71, listed by mistake under 2.0.0). `a ± b` means the two algebraic objects `a + b` and `a - b`, not a range, so turning it into an `Interval` contradicted the package's model. Its tests and example are removed too.

### Changed

- The default `"Simplifier"` is `Simplify` with a 5 second limit per half. Use `"Simplifier" -> Simplify` for no limit.
- `PmEqualQ` compares branches directly, and an expression is always `PmEqualQ` to itself.
- The README no longer describes a CI workflow or Mathics3 support; neither existed.

## 2.0.0 (2026-11-03)

The single `PlusMinus.wl` file has become a paclet with tests. Code that loaded the old file keeps working: `PlusMinus.wl` at the repository root now loads the new package.

### New

- `PmQ`: does an expression contain ± or ∓?
- `PmMap`: apply any function to both branches, e.g. `PmMap[D[#, x] &, expr]`.
- `PmThread`: like `PmArithmetic`, but takes any number of arguments.
- `PmSimplify` and `PmFullSimplify`, with optional assumptions.
- `PmEqualQ`: True/False version of `PmCompare`.
- `PmFlip`: swaps ± and ∓.
- `PmSolve`: runs `Solve` and merges a pair of solutions into one ± solution (quadratic formula and similar).
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
