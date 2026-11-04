# PMFunctions

[![Tests](https://github.com/OWNER/PMFunctions/actions/workflows/tests.yml/badge.svg)](https://github.com/OWNER/PMFunctions/actions/workflows/tests.yml)

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

## Installation

Requires Mathematica or Wolfram Engine 12.1 or later.

**From a clone** (recommended while developing):

```wolfram
PacletDirectoryLoad["/path/to/PMFunctions"];
Needs["PMFunctions`"]
```

**Install permanently** (survives restarts):

```wolfram
PacletInstall[CreatePacletArchive["/path/to/PMFunctions"], ForceVersionInstall -> True];
Needs["PMFunctions`"]
```

**Old style:** `Get["/path/to/PMFunctions/PlusMinus.wl"]` still works, so existing notebooks keep running.

## Function reference

In the input column, `a ± b` is typed with `Esc +- Esc` (or written `PlusMinus[a, b]`), and `∓` with `Esc -+ Esc`.

| Function | What it does | Example | Result |
|---|---|---|---|
| `PmToList[expr]` | Split into branches | `PmToList[a ± b]` | `{a + b, a - b}` |
| `PmFromList[{p, m}]` | Recombine branches | `PmFromList[{a + b, a - b}]` | `a ± b` |
| `PmQ[expr]` | Does `expr` contain ± or ∓? | `PmQ[x + (a ± b)]` | `True` |
| `PmBase[expr]` | Part shared by both branches | `PmBase[(a ± b)^2]` | `a^2 + b^2` |
| `PmPm[expr]` | Coefficient of ± | `PmPm[(a ± b)^2]` | `2 a b` |
| `PmParts[expr]` | `{PmBase, PmPm}` | `PmParts[a ∓ b]` | `{a, -b}` |
| `PmExpand[expr]` | Expand | `PmExpand[(a ± b)^2]` | `a^2 + b^2 ± 2 a b` |
| `PmSimplify[expr, assum]` | Simplify, with optional assumptions | `PmSimplify[(a^2 - b^2)/(a - b) ± c]` | `a + b ± c` |
| `PmFullSimplify[expr, assum]` | FullSimplify | | |
| `PmMap[f, expr]` | Apply `f` to both branches | `PmMap[D[#, x] &, x^2 ± x^3]` | `2 x ± 3 x^2` |
| `PmArithmetic[x, y, op]` | `op[x, y]` branch by branch | `PmArithmetic[a ± b, c ± d, Times]` | `a c + b d ± (a d + b c)` |
| `PmThread[f, x, y, ...]` | `f[x, y, ...]` branch by branch | `PmThread[Plus, a ± b, c ± d, e ± f]` | `a + c + e ± (b + d + f)` |
| `PmCommands[expr, {f, args...}, ...]` | Chain of commands on both branches | see below | |
| `PmCompare[x, y]` | `{base == base, pm == pm}` | `PmCompare[a ± b, a ± c]` | `{True, b == c}` |
| `PmEqualQ[x, y]` | Are both parts equal? | `PmEqualQ[a ± b, a ∓ (-b)]` | `True` |
| `PmFlip[expr]` | Swap ± and ∓ | `PmFlip[a ± b]` | `a ∓ b` |
| `PmSolve[eqns, vars]` | Solve, merging two solutions into one | `PmSolve[x^2 - 2x - 1 == 0, x]` | `{{x -> 1 ± Sqrt[2]}}` |
| `PmToInterval[expr]` | Interval spanned by numeric branches | `PmToInterval[3 ± 1/2]` | `Interval[{5/2, 7/2}]` |

### Chains of commands

`PmCommands` runs each `{f, args...}` as `f[branch, args...]` on both branches, in order:

```wolfram
PmCommands[(a b^2 + c b) ± (f b + g b^3 + y^2), {Series, {b, 0, 1}, {y, 0, 2}}, {Normal}]
(* b c ± (b f + y^2) *)
```

The braces around `{Normal}` are required. For a single function, `PmMap` is shorter.

### The "Simplifier" option

Every function that recombines branches (`PmFromList`, `PmParts`, `PmBase`, `PmPm`, `PmMap`, `PmThread`, `PmArithmetic`, `PmCommands`, `PmExpand`) accepts `"Simplifier" -> f`. `f` is applied to both halves of the result. The default is `Simplify` (`Expand` for `PmExpand`). Use `Identity` to skip simplification on large expressions, or `FullSimplify` for harder cases:

```wolfram
PmArithmetic[a ± b, c ± d, Times, "Simplifier" -> Identity]
```

### Things to know

- `PmFromList[{a - b, a + b}]` gives `a ± (-b)`, not `a ∓ b`. The two mean the same thing; use `PmFlip` if you prefer ∓.
- `PmArithmetic[..., Divide]` produces denominators like `c^2 - d^2`, which are invalid when `c^2 == d^2`.
- Everything assumes commutative algebra. Normal-order noncommutative objects before using these functions.
- `PmSolve` merges solutions only when `Solve` returns exactly two. Otherwise it gives a message and returns the plain `Solve` result.

## Running the tests

The tests are ordinary `VerificationTest` files in `Tests/`. Run them all from the repository root:

```bash
wolframscript -file Scripts/RunTests.wls   # Mathematica / Wolfram Engine
mathics -q -f Scripts/RunTests.wls          # Mathics3 (pip install mathics3), no license needed
```

or run a single file from a notebook with `TestReport["Tests/Branches.wlt"]`.

The script exits with status 1 if any test fails. Tests whose `TestID` starts with `WolframOnly:` are skipped on Mathics3, whose `Simplify` cannot handle some assumptions.

### Continuous integration

`.github/workflows/tests.yml` runs on every push and pull request:

- **Mathics3**: always runs, needs no setup.
- **Wolfram Engine** (optional): runs against the real kernel once you add an on-demand license entitlement. In Mathematica, run `CreateLicenseEntitlement[<|"LicenseExpiration" -> Quantity[1, "Hours"], "EntitlementExpiration" -> Quantity[1, "Years"]|>]["EntitlementID"]` and save the ID as the repository secret `WOLFRAMSCRIPT_ENTITLEMENTID` (Settings → Secrets and variables → Actions). On-demand kernels are billed to your Wolfram **Service Credits**; the free Wolfram Engine for Developers license can't be used here. Without the secret this job is skipped instead of failing.

## Repository layout

```
PMFunctions/
├── PacletInfo.wl           paclet metadata
├── Kernel/PMFunctions.wl   the package
├── PlusMinus.wl            compatibility loader for the old file name
├── Tests/*.wlt             unit tests (VerificationTest)
├── Scripts/RunTests.wls    test runner for Wolfram Engine and Mathics3
├── Examples/Examples.wl    a runnable tour of every function
└── .github/workflows/      CI
```

## License

MIT. See [LICENSE](LICENSE). Originally written in 2024 by Daniel Hooker at CSU Fresno.
