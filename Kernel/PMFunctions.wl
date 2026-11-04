(* ::Package:: *)

(* ::Title:: *)
(*PMFunctions*)

(* ::Text:: *)
(*Algebra with \[PlusMinus] (PlusMinus) and \[MinusPlus] (MinusPlus) in the Wolfram Language.*)
(**)
(*(c) 2024-2026 Daniel Hooker. Released under the MIT License (see LICENSE).*)
(**)
(*Mathematica treats PlusMinus and MinusPlus as purely typographical: a \[PlusMinus] b has no*)
(*built-in meaning, so you cannot expand, simplify or do arithmetic with it.*)
(*This package gives \[PlusMinus] a concrete meaning and builds algebra on top of it.*)
(**)
(*Model: an expression containing \[PlusMinus]/\[MinusPlus] stands for a pair of "branches",*)
(*  the "+" branch, where every \[PlusMinus] becomes + and every \[MinusPlus] becomes -,*)
(*  the "-" branch, where every \[PlusMinus] becomes - and every \[MinusPlus] becomes +.*)
(*All signs in one expression are correlated (they flip together), which is the usual*)
(*convention in e.g. the quadratic formula or in perturbation theory.*)
(**)
(*Every operation works the same way: split into branches, operate on each branch,*)
(*then recombine {p, m} into  (p+m)/2 \[PlusMinus] (p-m)/2.*)
(**)
(*Note: functions here assume commutative algebra. Normal-order any noncommutative*)
(*objects before handing them to this package.*)


BeginPackage["PMFunctions`"]


(* ::Section:: *)
(*Public symbols*)


PmQ::usage = "PmQ[expr] gives True if expr contains \[PlusMinus] or \[MinusPlus], and False otherwise.";

PmToList::usage = "PmToList[a \[PlusMinus] b] returns the branches {a + b, a - b}.
PmToList[a \[MinusPlus] b] returns {a - b, a + b}.
Every \[PlusMinus]/\[MinusPlus] in the expression is replaced consistently, so PmToList[(a \[PlusMinus] b)^2] gives {(a + b)^2, (a - b)^2}.";

PmFromList::usage = "PmFromList[{p, m}] combines the branches p and m into (p + m)/2 \[PlusMinus] (p - m)/2.
PmFromList[{a + b, a - b}] returns a \[PlusMinus] b. Identical branches return a plain expression, and opposite branches {x, -x} return \[PlusMinus]x.
The option \"Simplifier\" (default Simplify) is applied to both halves.";

PmBase::usage = "PmBase[a \[PlusMinus] b] returns a, the part of the expression that is the same on both branches.
For a general expression it returns (p + m)/2, where {p, m} = PmToList[expr].";

PmPm::usage = "PmPm[a \[PlusMinus] b] returns b, the coefficient of \[PlusMinus]. PmPm[a \[MinusPlus] b] returns -b.
For a general expression it returns (p - m)/2, where {p, m} = PmToList[expr].";

PmParts::usage = "PmParts[a \[PlusMinus] b] returns {a, b}. PmParts[a \[MinusPlus] b] returns {a, -b}.";

PmMap::usage = "PmMap[f, expr] applies f to both branches of expr and recombines the result.
PmMap[D[#, x] &, x^2 \[PlusMinus] x^3] returns 2 x \[PlusMinus] 3 x^2.";

PmThread::usage = "PmThread[f, expr1, expr2, ...] evaluates f[expr1, expr2, ...] branch by branch and recombines the result.
PmThread[Plus, a \[PlusMinus] b, c \[PlusMinus] d] returns a + c \[PlusMinus] (b + d).";

PmArithmetic::usage = "PmArithmetic[a \[PlusMinus] b, c \[PlusMinus] d, op] evaluates op[a \[PlusMinus] b, c \[PlusMinus] d] branch by branch. op can be Plus, Times, Subtract, Divide, Power, or any function of two arguments.
Example: PmArithmetic[a \[PlusMinus] b, c \[PlusMinus] d, Divide] returns (a c - b d)/(c^2 - d^2) \[PlusMinus] (b c - a d)/(c^2 - d^2). Watch out for c^2 == d^2.";

PmArithmatic::usage = "PmArithmatic is the old (misspelled) name of PmArithmetic. It is kept so existing notebooks keep working; use PmArithmetic in new code.";

PmCommands::usage = "PmCommands[expr, {f1, args1...}, {f2, args2...}, ...] applies a chain of commands to both branches of expr, then recombines.
Each command {f, args...} is evaluated as f[branch, args...].
Example: PmCommands[a b^2 + c b \[PlusMinus] (f b + g b^3 + y^2), {Series, {b, 0, 1}, {y, 0, 2}}, {Normal}] runs Series[#, {b, 0, 1}, {y, 0, 2}] and then Normal on each branch. The braces around {Normal} are required.";

PmExpand::usage = "PmExpand[expr] expands both branches of expr and returns the result in the form a \[PlusMinus] b with a and b expanded.
PmExpand[(a \[PlusMinus] b)^2] returns a^2 + b^2 \[PlusMinus] 2 a b.";

PmSimplify::usage = "PmSimplify[expr] simplifies an expression containing \[PlusMinus]/\[MinusPlus].
PmSimplify[expr, assum] simplifies using assumptions, exactly like Simplify.";

PmFullSimplify::usage = "PmFullSimplify[expr] and PmFullSimplify[expr, assum] work like PmSimplify, but use FullSimplify.";

PmCompare::usage = "PmCompare[a \[PlusMinus] b, c \[PlusMinus] d] returns {a == c, b == d}, with each comparison simplified.";

PmEqualQ::usage = "PmEqualQ[expr1, expr2] gives True if the two expressions have provably equal branches, and False otherwise.";

PmFlip::usage = "PmFlip[expr] swaps the two branches of expr, turning every \[PlusMinus] into \[MinusPlus] and vice versa.";

PmSolve::usage = "PmSolve[eqns, vars] solves eqns with Solve and, when there are exactly two solutions, merges them into a single \[PlusMinus] solution.
PmSolve[x^2 + b x + c == 0, x] returns {{x -> -b/2 \[PlusMinus] Sqrt[b^2 - 4 c]/2}}. Any extra arguments are passed on to Solve.";

PmToInterval::usage = "PmToInterval[expr] returns Interval[{min, max}] spanning the two branches of a numeric expression.
PmToInterval[3 \[PlusMinus] 1/2] returns Interval[{5/2, 7/2}].";


(* ::Subsection:: *)
(*Messages*)


PmFromList::arg = "Expected a list of two branches {p, m}, but got `1`.";
PmSolve::nopair = "Solve returned `1` solution(s), not 2, so they cannot be merged into one \[PlusMinus] solution. Returning the result of Solve unchanged.";
PmToInterval::nonum = "The branches `1` are not both numeric, so no Interval can be formed.";


Begin["`Private`"]


(* ::Section:: *)
(*Branches*)


(* s is +1 for the "+" branch and -1 for the "-" branch.
   a \[PlusMinus] b \[PlusMinus] c is PlusMinus[a, b, c], read as a + s (b + c).
   ReplaceRepeated also handles nested signs such as a \[PlusMinus] (b \[PlusMinus] c). *)
branchRules[s_] := {
	PlusMinus[x_] :> s x,
	PlusMinus[x_, y__] :> x + s Plus[y],
	MinusPlus[x_] :> -s x,
	MinusPlus[x_, y__] :> x - s Plus[y]
};

branch[expr_, s_] := expr //. branchRules[s]


PmQ[expr_] := !FreeQ[expr, _PlusMinus | _MinusPlus]

PmToList[expr_] := {branch[expr, 1], branch[expr, -1]}


(* ::Section:: *)
(*Recombining branches*)


$simplifierOptions = {"Simplifier" -> Simplify};

halves[{p_, m_}, simp_] := {simp[(p + m)/2], simp[(p - m)/2]}

combine[{base_, pm_}] := Which[
	pm === 0, base,
	base === 0, PlusMinus[pm],
	True, PlusMinus[base, pm]
]

Options[PmFromList] = $simplifierOptions;

PmFromList[{p_, m_}, OptionsPattern[]] := combine[halves[{p, m}, OptionValue["Simplifier"]]]

PmFromList[other_, OptionsPattern[]] := (Message[PmFromList::arg, Short[other]]; $Failed)


(* ::Section:: *)
(*Taking expressions apart*)


Options[PmParts] = $simplifierOptions;
PmParts[expr_, OptionsPattern[]] := halves[PmToList[expr], OptionValue["Simplifier"]]

Options[PmBase] = $simplifierOptions;
PmBase[expr_, opts : OptionsPattern[]] := First[PmParts[expr, opts]]

Options[PmPm] = $simplifierOptions;
PmPm[expr_, opts : OptionsPattern[]] := Last[PmParts[expr, opts]]

PmFlip[expr_] := expr /. {PlusMinus -> MinusPlus, MinusPlus -> PlusMinus}


(* ::Section:: *)
(*Operating on branches*)


Options[PmMap] = $simplifierOptions;
PmMap[f_, expr_, opts : OptionsPattern[]] := PmFromList[f /@ PmToList[expr], opts]

Options[PmThread] = $simplifierOptions;
PmThread[f_, args___, opts : OptionsPattern[]] := Module[{branches},
	branches = PmToList /@ {args};
	PmFromList[{f @@ branches[[All, 1]], f @@ branches[[All, 2]]}, opts]
]

Options[PmArithmetic] = $simplifierOptions;
PmArithmetic[x_, y_, op_, opts : OptionsPattern[]] := PmThread[op, x, y, opts]

(* Backwards-compatible alias for the original spelling. *)
PmArithmatic[args___] := PmArithmetic[args]

(* Each command {f, args...} is run as f[branch, args...]. *)
applyCommand[branches_List, {f_, args___}] := Map[f[#, args] &, branches]

Options[PmCommands] = $simplifierOptions;
PmCommands[expr_, commands___List, opts : OptionsPattern[]] :=
	PmFromList[Fold[applyCommand, PmToList[expr], {commands}], opts]


(* ::Section:: *)
(*Expanding and simplifying*)


Options[PmExpand] = {"Simplifier" -> Expand};
PmExpand[expr_, opts : OptionsPattern[]] := PmFromList[Expand /@ PmToList[expr], "Simplifier" -> OptionValue["Simplifier"]]

PmSimplify[expr_, rest___] := PmFromList[PmToList[expr], "Simplifier" -> (Simplify[#, rest] &)]

PmFullSimplify[expr_, rest___] := PmFromList[PmToList[expr], "Simplifier" -> (FullSimplify[#, rest] &)]


(* ::Section:: *)
(*Comparing*)


(* MapThread rather than Thread[l1 == l2]: identical lists would make l1 == l2 evaluate to a single True. *)
PmCompare[x_, y_] := Simplify[MapThread[Equal, {PmParts[x], PmParts[y]}]]

PmEqualQ[x_, y_] := TrueQ[And @@ PmCompare[x, y]]


(* ::Section:: *)
(*Solving*)


(* True for expressions that print with a leading minus sign, e.g. -3, -x, -Sqrt[2]/2. *)
negativeNumberQ[e_] := NumericQ[e] && TrueQ[e < 0]
leadingMinusQ[e_] := MatchQ[e, _?negativeNumberQ | Times[_?negativeNumberQ, __]]

(* The order of Solve's solutions is arbitrary, so we may swap them freely. We swap so that
   the \[PlusMinus] part of the first variable has no leading minus sign: x -> 1 \[PlusMinus] Sqrt[2]
   rather than x -> 1 \[PlusMinus] -Sqrt[2]. The same order is used for every variable, so the
   signs stay correlated between variables. *)
PmSolve[eqns_, vars_, rest___] := Module[{sols, keys, pair},
	sols = Solve[eqns, vars, rest];
	If[!MatchQ[sols, {{__Rule}, {__Rule}}] || Sort[sols[[1, All, 1]]] =!= Sort[sols[[2, All, 1]]],
		Message[PmSolve::nopair, If[ListQ[sols], Length[sols], 0]];
		Return[sols]
	];
	keys = sols[[1, All, 1]];
	pair = {keys /. sols[[1]], keys /. sols[[2]]};
	If[leadingMinusQ[Last[halves[pair[[All, 1]], Simplify]]], pair = Reverse[pair]];
	{MapThread[#1 -> PmFromList[{#2, #3}] &, {keys, pair[[1]], pair[[2]]}]}
]


(* ::Section:: *)
(*Numeric helpers*)


PmToInterval[expr_] := Module[{b = PmToList[expr]},
	If[And @@ (NumericQ /@ b),
		Interval[{Min[b], Max[b]}],
		Message[PmToInterval::nonum, b]; $Failed
	]
]


End[]

EndPackage[]
