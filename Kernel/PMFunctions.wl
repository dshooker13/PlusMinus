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


(* Start from a clean slate, so that loading the package again (Get after Needs, or a newer
   copy) replaces every earlier definition. The public symbols are protected at the end. *)
Unprotect["PMFunctions`*", "PMFunctions`Private`*"];
ClearAll["PMFunctions`*", "PMFunctions`Private`*"];


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
PmSimplify[expr, assum] simplifies using assumptions, and PmSimplify[expr, assum, opts] also passes Simplify options such as TimeConstraint, exactly like Simplify.";

PmFullSimplify::usage = "PmFullSimplify[expr], PmFullSimplify[expr, assum] and PmFullSimplify[expr, assum, opts] work like PmSimplify, but use FullSimplify.";

PmCompare::usage = "PmCompare[a \[PlusMinus] b, c \[PlusMinus] d] returns {a == c, b == d}, with each comparison simplified.";

PmEqualQ::usage = "PmEqualQ[expr1, expr2] gives True if the two expressions have provably equal branches, and False otherwise.";

PmFlip::usage = "PmFlip[expr] swaps the two branches of expr, turning every \[PlusMinus] into \[MinusPlus] and vice versa.";

PmSolve::usage = "PmSolve[eqns, vars] solves eqns with Solve and, when there are exactly two solutions, merges them into a single \[PlusMinus] solution.
PmSolve[x^2 + b x + c == 0, x] returns {{x -> -b/2 \[PlusMinus] Sqrt[b^2 - 4 c]/2}}. Any extra arguments are passed on to Solve.";



(* ::Subsection:: *)
(*Messages*)


PmFromList::arg = "Expected a list of two branches {p, m}, but got `1`.";
General::pmargs = "`1` is not a valid call. Expected `2`.";
PmSolve::nopair = "Solve returned `1` solution(s), not 2, so they cannot be merged into one \[PlusMinus] solution. Returning the result of Solve unchanged.";


Begin["`Private`"]


(* ::Section:: *)
(*Branches*)


(* s is +1 for the "+" branch and -1 for the "-" branch.
   PlusMinus[a, b, c] is read as a + s (b + c). Typed input a \[PlusMinus] b \[PlusMinus] c parses as the
   nested PlusMinus[PlusMinus[a, b], c], which gives the same branches.
   ReplaceRepeated resolves nested signs such as a \[PlusMinus] (b \[PlusMinus] c). *)
branchRules[s_] := {
	PlusMinus[x_] :> s x,
	PlusMinus[x_, y__] :> x + s Plus[y],
	MinusPlus[x_] :> -s x,
	MinusPlus[x_, y__] :> x - s Plus[y]
};

branch[expr_, s_] := expr //. branchRules[s]

(* Internal code calls these private helpers rather than the public functions, so a user
   definition for a public symbol can't change how the rest of the package behaves. *)
branches[expr_] := {branch[expr, 1], branch[expr, -1]}

PmQ[expr_] := !FreeQ[expr, _PlusMinus | _MinusPlus]

PmToList[expr_] := branches[expr]


(* ::Section:: *)
(*Recombining branches*)


$simplifierOptions = {"Simplifier" -> Simplify};

(* Only "Simplifier" -> f counts as an option. A general OptionsPattern[] after args___
   would also swallow a trailing data argument that happens to be a rule, as in
   PmThread[ReplaceAll, expr, x -> 2]. *)
simplifierOptionQ[(Rule | RuleDelayed)[name_String, _]] := name === "Simplifier"
simplifierOptionQ[(Rule | RuleDelayed)[name_Symbol, _]] := SymbolName[name] === "Simplifier"
simplifierOptionQ[_] := False

simplifier[f_, {opts___}] := OptionValue[f, {opts}, "Simplifier"]

halves[{p_, m_}, simp_] := {simp[(p + m)/2], simp[(p - m)/2]}

combine[{base_, pm_}] := Which[
	pm === 0, base,
	base === 0, PlusMinus[pm],
	True, PlusMinus[base, pm]
]

recombine[{p_, m_}, simp_] := combine[halves[{p, m}, simp]]

Options[PmFromList] = $simplifierOptions;

PmFromList[{p_, m_}, opts___?simplifierOptionQ] := recombine[{p, m}, simplifier[PmFromList, {opts}]]

PmFromList[other : Except[{_, _}], ___?simplifierOptionQ] := (Message[PmFromList::arg, Short[other]]; $Failed)


(* ::Section:: *)
(*Taking expressions apart*)


Options[PmParts] = $simplifierOptions;
PmParts[expr_, opts___?simplifierOptionQ] := halves[branches[expr], simplifier[PmParts, {opts}]]

Options[PmBase] = $simplifierOptions;
PmBase[expr_, opts___?simplifierOptionQ] := First[halves[branches[expr], simplifier[PmBase, {opts}]]]

Options[PmPm] = $simplifierOptions;
PmPm[expr_, opts___?simplifierOptionQ] := Last[halves[branches[expr], simplifier[PmPm, {opts}]]]

PmFlip[expr_] := expr /. {PlusMinus -> MinusPlus, MinusPlus -> PlusMinus}


(* ::Section:: *)
(*Operating on branches*)


Options[PmMap] = $simplifierOptions;
PmMap[f_, expr_, opts___?simplifierOptionQ] := recombine[f /@ branches[expr], simplifier[PmMap, {opts}]]

Options[PmThread] = $simplifierOptions;
PmThread[f_, args___, opts___?simplifierOptionQ] := threadBranches[f, {args}, simplifier[PmThread, {opts}]]

threadBranches[f_, args_List, simp_] := Module[{b = branches /@ args},
	recombine[{f @@ b[[All, 1]], f @@ b[[All, 2]]}, simp]
]

Options[PmArithmetic] = $simplifierOptions;
PmArithmetic[x_, y_, op_, opts___?simplifierOptionQ] := threadBranches[op, {x, y}, simplifier[PmArithmetic, {opts}]]

(* Backwards-compatible alias for the original spelling. *)
PmArithmatic[args___] := PmArithmetic[args]

(* Each command {f, args...} is run as f[branch, args...]. *)
applyCommand[b_List, {f_, args___}] := Map[f[#, args] &, b]

Options[PmCommands] = $simplifierOptions;
PmCommands[expr_, commands___List, opts___?simplifierOptionQ] :=
	recombine[Fold[applyCommand, branches[expr], {commands}], simplifier[PmCommands, {opts}]]


(* ::Section:: *)
(*Expanding and simplifying*)


Options[PmExpand] = {"Simplifier" -> Expand};
PmExpand[expr_, opts___?simplifierOptionQ] := recombine[Expand /@ branches[expr], simplifier[PmExpand, {opts}]]

(* An optional assumption, then options of the underlying Simplify/FullSimplify. Anything
   else (such as "Simplifier", which these two functions don't take) is a wrong call. *)
validOptionsQ[f_, opts_List] := FilterRules[opts, Except[Options[f]]] === {}

PmSimplify[expr_, assum : Repeated[Except[_?OptionQ], {0, 1}], opts : OptionsPattern[]] /; validOptionsQ[Simplify, {opts}] :=
	recombine[branches[expr], Simplify[#, assum, opts] &]

PmFullSimplify[expr_, assum : Repeated[Except[_?OptionQ], {0, 1}], opts : OptionsPattern[]] /; validOptionsQ[FullSimplify, {opts}] :=
	recombine[branches[expr], FullSimplify[#, assum, opts] &]


(* ::Section:: *)
(*Comparing*)


(* MapThread rather than Thread[l1 == l2]: identical lists would make l1 == l2 evaluate to a single True. *)
compare[x_, y_] := Simplify[MapThread[Equal, {halves[branches[x], Simplify], halves[branches[y], Simplify]}]]

PmCompare[x_, y_] := compare[x, y]

PmEqualQ[x_, y_] := TrueQ[And @@ compare[x, y]]


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
	{MapThread[#1 -> recombine[{#2, #3}, Simplify] &, {keys, pair[[1]], pair[[2]]}]}
]


(* ::Section:: *)
(*Argument checking*)


(* For each function: the form shown in the error message, and its ArgumentsPattern.
   PmArithmatic is left out: its alias definition already accepts any arguments and hands
   them to PmArithmetic, which does the checking.
   A call that matches none of a function's definitions gives General::pmargs and returns
   unevaluated, like a built-in function called with the wrong arguments. *)
$signatures = {
	PmQ -> {"PmQ[expr]", {_}},
	PmToList -> {"PmToList[expr]", {_}},
	PmFromList -> {"PmFromList[{p, m}, \"Simplifier\" -> f]", {_, OptionsPattern[]}},
	PmBase -> {"PmBase[expr, \"Simplifier\" -> f]", {_, OptionsPattern[]}},
	PmPm -> {"PmPm[expr, \"Simplifier\" -> f]", {_, OptionsPattern[]}},
	PmParts -> {"PmParts[expr, \"Simplifier\" -> f]", {_, OptionsPattern[]}},
	PmMap -> {"PmMap[f, expr, \"Simplifier\" -> f]", {_, _, OptionsPattern[]}},
	PmThread -> {"PmThread[f, expr1, expr2, ..., \"Simplifier\" -> f]", {_, ___, OptionsPattern[]}},
	PmArithmetic -> {"PmArithmetic[expr1, expr2, op, \"Simplifier\" -> f]", {_, _, _, OptionsPattern[]}},
	PmCommands -> {"PmCommands[expr, {f1, args1...}, {f2, args2...}, ..., \"Simplifier\" -> f]", {_, ___, OptionsPattern[]}},
	PmExpand -> {"PmExpand[expr, \"Simplifier\" -> f]", {_, OptionsPattern[]}},
	PmSimplify -> {"PmSimplify[expr], PmSimplify[expr, assum] or PmSimplify[expr, assum, opts] with Simplify options", {_, _., OptionsPattern[]}},
	PmFullSimplify -> {"PmFullSimplify[expr], PmFullSimplify[expr, assum] or PmFullSimplify[expr, assum, opts] with FullSimplify options", {_, _., OptionsPattern[]}},
	PmCompare -> {"PmCompare[expr1, expr2]", {_, _}},
	PmEqualQ -> {"PmEqualQ[expr1, expr2]", {_, _}},
	PmFlip -> {"PmFlip[expr]", {_}},
	PmSolve -> {"PmSolve[eqns, vars, ...]", {_, _, ___}}
};

badCall[f_, call_, expected_] := (Message[MessageName[f, "pmargs"], call, expected]; False)

Scan[
	Function[{rule}, With[{f = First[rule], form = rule[[2, 1]], pattern = rule[[2, 2]]},
		SyntaxInformation[f] = Join[
			{"ArgumentsPattern" -> pattern},
			If[MemberQ[pattern, Verbatim[OptionsPattern][]] && f =!= PmSimplify && f =!= PmFullSimplify, {"OptionNames" -> {"\"Simplifier\""}}, {}]
		];
		f[args___] := Null /; badCall[f, HoldForm[f[args]], form]
	]],
	$signatures
];


End[]


SetAttributes[Evaluate[Names["PMFunctions`*"]], Protected];


EndPackage[]
