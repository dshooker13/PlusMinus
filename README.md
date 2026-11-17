# PMFunctions

**Algebra with ± and ∓ in the Wolfram Language.**

Mathematica has `PlusMinus` (±) and `MinusPlus` (∓), but they are only notation. `Expand[(a ± b)^2]` leaves the expression as it is, and you can't simplify, differentiate or solve with it. PMFunctions gives ± a concrete meaning so you can:

```wolfram
PmExpand[(a ± b)^2]
(* a^2 + b^2 ± 2 a b *)

PmArithmetic[a ± b, c ± d, Divide]
(* (a c - b d)/(c^2 - d^2) ± (b c - a d)/(c^2 - d^2) *)

PmMap[D[#, x] &, x^2 ± x^3]
(* 2 x ± 3 x^2 *)

PmSolve[x^2 + b x + c == 0, x]
(* {{x -> -b/2 ± Sqrt[b^2 - 4 c]/2}} *)
```
Remember that ± here means two discrete values, not a statistical uncertainty.

## How it works

An expression containing ± stands for two **branches**:

| | every ± becomes | every ∓ becomes |
|---|---|---|
| `+` branch | `+` | `-` |
| `-` branch | `-` | `+` |

All signs in one expression flip **together**, which is the convention in the quadratic formula and in perturbation theory. So `(a ± b) + (c ± d)` means `{a + b + c + d, a - b + c - d}`, not four combinations.

Every function follows the same three steps:

1. Split the expression into branches `{p, m}` (`PmToList`).
2. Operate on each branch.
3. Recombine as `(p + m)/2 ± (p - m)/2` (`PmFromList`).

Some branches can't be recombined that way:

- **Equations, inequalities, rules, lists and associations** of the same shape are combined element by element. `PmSimplify[a ± b == c]` gives `a ± b == c`.
- **Infinite or conditional branches, Booleans, strings and intervals** are kept as `PmBranches[p, m]`. For example, `PmArithmetic[2, 1 ± 1, Divide]` gives `PmBranches[1, ComplexInfinity]`, because averaging an infinite branch with a finite one would lose the finite one. Every function understands `PmBranches`, so you can keep working with the result.

## Installation

Requires Mathematica or Wolfram Engine 12.1 or later.

**From a clone** (recommended while developing):

```wolfram
PacletDirectoryLoad["/path/to/PMFunctions"];
Needs["PMFunctions`"]
```

**Install permanently** (survives restarts). First build the paclet archive from the repository root:

```bash
wolframscript -file Scripts/BuildPaclet.wls
```

This writes `build/PMFunctions-<version>.paclet`, containing only the package files. Then install it:

```wolfram
PacletInstall["/path/to/PMFunctions/build/PMFunctions-2.1.0.paclet", ForceVersionInstall -> True];
Needs["PMFunctions`"]
```

Don't call `CreatePacletArchive` on the repository folder directly: it would also pack the tests, scripts and any untracked files.

**Old style:** `Get["/path/to/PMFunctions/PlusMinus.wl"]` still works, so existing notebooks keep running.

## Function reference

In the input column, `a ± b` is typed with `Esc +- Esc` (or written `PlusMinus[a, b]`), and `∓` with `Esc -+ Esc`.

| Function | What it does | Example | Result |
|---|---|---|---|
| `PmToList[expr]` | Split into branches | `PmToList[a ± b]` | `{a + b, a - b}` |
| `PmFromList[{p, m}]` | Recombine branches | `PmFromList[{a + b, a - b}]` | `a ± b` |
| `PmBranches[p, m]` | Branches kept apart (see above) | `PmToList[PmBranches[1, Infinity]]` | `{1, Infinity}` |
| `PmQ[expr]` | Does `expr` contain ± or ∓? | `PmQ[x + (a ± b)]` | `True` |
| `PmBase[expr]` | Part shared by both branches | `PmBase[(a ± b)^2]` | `a^2 + b^2` |
| `PmPm[expr]` | Coefficient of ± | `PmPm[(a ± b)^2]` | `2 a b` |
| `PmParts[expr]` | `{PmBase, PmPm}` | `PmParts[a ∓ b]` | `{a, -b}` |
| `PmExpand[expr]` | Expand | `PmExpand[(a ± b)^2]` | `a^2 + b^2 ± 2 a b` |
| `PmSimplify[expr, assum, opts]` | Simplify, with optional assumptions and Simplify options | `PmSimplify[(a^2 - b^2)/(a - b) ± c]` | `a + b ± c` |
| `PmFullSimplify[expr, assum, opts]` | FullSimplify | `PmFullSimplify[Gamma[x + 1]/Gamma[x] ± 1]` | `x ± 1` |
| `PmMap[f, expr]` | Apply `f` to both branches | `PmMap[D[#, x] &, x^2 ± x^3]` | `2 x ± 3 x^2` |
| `PmArithmetic[x, y, op]` | `op[x, y]` branch by branch | `PmArithmetic[a ± b, c ± d, Times]` | `a c + b d ± (a d + b c)` |
| `PmThread[f, x, y, ...]` | `f[x, y, ...]` branch by branch | `PmThread[Plus, a ± b, c ± d, e ± f]` | `a + c + e ± (b + d + f)` |
| `PmCommands[expr, {f, args...}, ...]` | Chain of commands on both branches | see below | |
| `PmCompare[x, y]` | `{base == base, pm == pm}` | `PmCompare[a ± b, a ± c]` | `{True, b == c}` |
| `PmEqualQ[x, y]` | Are both parts equal? | `PmEqualQ[a ± b, a ∓ (-b)]` | `True` |
| `PmFlip[expr]` | Swap ± and ∓ | `PmFlip[a ± b]` | `a ∓ b` |
| `PmSolve[eqns, vars, ...]` | Solve, merging two solutions into one | `PmSolve[x^2 - 2x - 1 == 0, x]` | `{{x -> 1 ± Sqrt[2]}}` |

### Chains of commands

`PmCommands` runs each `{f, args...}` as `f[branch, args...]` on both branches, in order:

```wolfram
PmCommands[(a b^2 + c b) ± (f b + g b^3 + y^2), {Series, {b, 0, 1}, {y, 0, 2}}, {Normal}]
(* b c ± (b f + y^2) *)
```

The braces around `{Normal}` are required. For a single function, `PmMap` is shorter.

### The "Simplifier" option

Every function that recombines branches (`PmFromList`, `PmParts`, `PmBase`, `PmPm`, `PmMap`, `PmThread`, `PmArithmetic`, `PmCommands`, `PmExpand`) accepts `"Simplifier" -> f`. `f` is applied to both halves of the result.

The default is `Simplify` with a 5 second limit per half: if `Simplify` takes longer, that half is returned unsimplified. (`PmExpand` uses `Expand`.) Use `"Simplifier" -> Simplify` for no time limit, `Identity` to skip simplifying large expressions, or `FullSimplify` for harder cases:

```wolfram
PmArithmetic[a ± b, c ± d, Times, "Simplifier" -> Identity]
```

### Things to know

- `PmFromList[{a - b, a + b}]` gives `a ± (-b)`, not `a ∓ b`. The two mean the same thing; use `PmFlip` if you prefer ∓.
- `PmArithmetic[..., Divide]` puts both halves over a common denominator like `c^2 - d^2`. If you later substitute values that make it zero (say `d -> c`), the result is infinite on both branches, even though one branch was finite. Substitute before dividing, or use `PmToList` on the result first.
- Everything assumes commutative algebra. Normal-order noncommutative objects before using these functions.
- `PmSolve` merges solutions only when `Solve` returns exactly two. Otherwise it gives a message and returns the plain `Solve` result. If the equations already contain ±, each branch is solved separately and the solutions keep that sign: `PmSolve[x^2 == 4 ± 1, x]` gives two ± solutions covering all four roots.
- `PmEqualQ` returns `False` when `Simplify` can't prove the two expressions equal; that doesn't mean they are different. It compares branches in order, so `a ± b` and `a ∓ b` differ unless `b` is 0. Like `Simplify`, it uses generic rules: `(x^2 - 1)/(x - 1)` counts as equal to `x + 1`.
- Inside held expressions (`Hold`, `HoldForm`) the signs are replaced but nothing is evaluated. Patterns are rewritten too: `PmToList[PlusMinus[x_, y_] :> x]` gives rules for `x_ + y_` and `x_ - y_`.
- Functions called with the wrong arguments give a `::pmargs` message and return unevaluated, like built-in functions.

## Running the tests

The tests are ordinary `VerificationTest` files in `Tests/`. Run them all from the repository root:

```bash
wolframscript -file Scripts/RunTests.wls
```

The script runs each file with `TestReport` and exits with status 1 if any test fails, if a file doesn't parse, or if a file runs a different number of tests than it contains. You can also run a single file from a notebook with `TestReport["/path/to/PMFunctions/Tests/Branches.wlt"]`. Each file loads the package from its own checkout.

`Tests/Properties.wlt` checks the main functions on random expressions, comparing them with an independent reference implementation of the branch rules in `Tests/PropertyOracle.wl`.

## Repository layout

```
PMFunctions/
├── PacletInfo.wl           paclet metadata
├── Kernel/PMFunctions.wl   the package
├── PlusMinus.wl            compatibility loader for the old file name
├── Tests/*.wlt             unit and property tests (VerificationTest)
├── Tests/TestUtils.wl      shared test helpers
├── Tests/PropertyOracle.wl reference branch evaluator for the property tests
├── Scripts/RunTests.wls    test runner
├── Scripts/BuildPaclet.wls builds build/PMFunctions-<version>.paclet
└── Examples/Examples.wl    a runnable tour of every function
```

## License

MIT. See [LICENSE](LICENSE). Originally written in 2024 by Daniel Hooker at CSU Fresno.
