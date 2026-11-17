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
(*So a \[PlusMinus] b is the two algebraic objects a + b and a - b. It is never a range, an interval*)
(*or a statistical uncertainty, and nothing in this package treats it as one.*)
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


PmQ::usage = "PmQ[expr] gives True if expr contains \[PlusMinus], \[MinusPlus] or PmBranches, and False otherwise.";

PmBranches::usage = "PmBranches[p, m] stands for an expression whose \"+\" branch is p and whose \"-\" branch is m.
Functions return it instead of a \[PlusMinus] b when the branches can't be safely written as (p + m)/2 \[PlusMinus] (p - m)/2: when a branch is infinite, Indeterminate or a ConditionalExpression, or when the branches are not ordinary values (True/False, strings, Interval objects, or equations, rules and lists of different shapes).
PmToList, PmFromList, PmMap, PmThread, PmSimplify, PmFlip, PmEqualQ and the other functions that work branch by branch understand PmBranches, so results can be passed on. PmParts, PmBase, PmPm and PmCompare give a message instead, because such branches have no base and \[PlusMinus] part.";

PmToList::usage = "PmToList[a \[PlusMinus] b] returns the branches {a + b, a - b}.
PmToList[a \[MinusPlus] b] returns {a - b, a + b}.
Every \[PlusMinus]/\[MinusPlus] in the expression is replaced consistently, so PmToList[(a \[PlusMinus] b)^2] gives {(a + b)^2, (a - b)^2}.";

PmFromList::usage = "PmFromList[{p, m}] combines the branches p and m into (p + m)/2 \[PlusMinus] (p - m)/2.
PmFromList[{a + b, a - b}] returns a \[PlusMinus] b. Identical branches return a plain expression, and opposite branches {x, -x} return \[PlusMinus]x.
Equations, inequalities, rules, lists and associations of the same shape are combined element by element, so PmFromList[{x == a + b, x == a - b}] returns x == a \[PlusMinus] b.
Branches that can't be safely combined are returned as PmBranches[p, m].
The option \"Simplifier\" is applied to both halves. The default is Simplify with a 5 second limit per half; use \"Simplifier\" -> Simplify for no limit or Identity to skip simplifying.";

PmBase::usage = "PmBase[a \[PlusMinus] b] returns a, the part of the expression that is the same on both branches.
For a general expression it returns (p + m)/2, where {p, m} = PmToList[expr].";

PmPm::usage = "PmPm[a \[PlusMinus] b] returns b, the coefficient of \[PlusMinus]. PmPm[a \[MinusPlus] b] returns -b.
For a general expression it returns (p - m)/2, where {p, m} = PmToList[expr].";

PmParts::usage = "PmParts[a \[PlusMinus] b] returns {a, b}. PmParts[a \[MinusPlus] b] returns {a, -b}.
PmParts, PmBase and PmPm need both branches to be ordinary values (or lists of them). For an infinite or conditional branch, equations, rules or PmBranches of such, they give a message and $Failed.";

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

PmCompare::usage = "PmCompare[a \[PlusMinus] b, c \[PlusMinus] d] returns {a == c, b == d}, with each comparison simplified. If an expression can't be split into a base and a \[PlusMinus] part (see PmParts), it gives a message and $Failed; PmEqualQ can still compare such expressions.
The option \"Simplifier\" (default: Simplify with a 5 second limit) is used for the parts and the comparisons.";

PmEqualQ::usage = "PmEqualQ[expr1, expr2] gives True if Simplify can prove, within 5 seconds per branch, that the two expressions have equal branches, and False otherwise. False means \"not proven equal\", not \"proven different\". Branches are compared in order, so a \[PlusMinus] b and a \[MinusPlus] b are different unless b is 0. Simplify uses generic rules, so (x^2 - 1)/(x - 1) counts as equal to x + 1.";

PmFlip::usage = "PmFlip[expr] swaps the two branches of expr, turning every \[PlusMinus] into \[MinusPlus] and vice versa.";

PmSolve::usage = "PmSolve[eqns, vars] solves eqns with Solve and, when there are exactly two solutions, merges them into a single \[PlusMinus] solution.
PmSolve[x^2 + b x + c == 0, x] returns {{x -> -b/2 \[PlusMinus] Sqrt[b^2 - 4 c]/2}}. Extra arguments are passed on to Solve, except the option \"Simplifier\" (default: Simplify with a 5 second limit), which is used to merge solutions.
If eqns already contain \[PlusMinus], each branch is solved separately. Repeated roots count once, and solutions that don't depend on the sign are returned as they are. The others are paired, a solution of the \"+\" equations with one of the \"-\" equations: real before complex, then by numeric value when possible. Each pair is a valid solution for the equations' sign, but the pairing is one choice among several. PmSolve[x^2 == 4 \[PlusMinus] 1, x] returns two \[PlusMinus] solutions, one with branches -Sqrt[5] and -Sqrt[3] and one with Sqrt[5] and Sqrt[3]. If a branch has no solutions, the result is PmBranches[sols+, sols-].";



(* ::Subsection:: *)
(*Messages*)


PmFromList::arg = "Expected a list of two branches {p, m}, but got `1`.";
General::pmargs = "`1` is not a valid call. Expected `2`.";
General::nobase = "The branches `1` can't be split into a base and a \[PlusMinus] part, because a branch is infinite, conditional or not an ordinary value. Use PmToList to get the branches.";
PmSolve::nopair = "Solve returned `1` solution(s), not 2, so they cannot be merged into one \[PlusMinus] solution. Returning the result of Solve unchanged.";
PmSolve::vars = "The two solutions give values for different variables (`1` and `2`), so they cannot be merged into one \[PlusMinus] solution. Returning the result of Solve unchanged.";


Begin["`Private`"]


(* ::Section:: *)
(*Branches*)


(* s is +1 for the "+" branch and -1 for the "-" branch.
   PlusMinus[a, b, c] is read as a + s (b + c). Typed input a \[PlusMinus] b \[PlusMinus] c parses as the
   nested PlusMinus[PlusMinus[a, b], c], which gives the same branches.
   ReplaceRepeated resolves nested signs such as a \[PlusMinus] (b \[PlusMinus] c). *)
(* The rules for each branch are written out rather than built from s, so that inside held
   expressions they leave a + b rather than a + 1*(Plus[b]). An empty PlusMinus[] is 0. *)
branchRules[1] = {
	PlusMinus[] :> 0, MinusPlus[] :> 0,
	PlusMinus[x_] :> x,
	PlusMinus[x_, y__] :> Plus[x, y],
	MinusPlus[x_] :> -x,
	MinusPlus[x_, y_] :> x - y,
	MinusPlus[x_, y__] :> x - Plus[y],
	PmBranches[p_, _] :> p
};
branchRules[-1] = {
	PlusMinus[] :> 0, MinusPlus[] :> 0,
	PlusMinus[x_] :> -x,
	PlusMinus[x_, y_] :> x - y,
	PlusMinus[x_, y__] :> x - Plus[y],
	MinusPlus[x_] :> x,
	MinusPlus[x_, y__] :> Plus[x, y],
	PmBranches[_, m_] :> m
};

(* A SparseArray doesn't expose its elements to pattern matching, so a sign stored in one
   would be invisible. Such arrays are converted to ordinary lists first. *)
normalizeSparse[expr_] := If[FreeQ[expr, _SparseArray], expr, expr /. sa_SparseArray :> Normal[sa]]

branch[expr_, s_] := normalizeSparse[expr] //. branchRules[s]

(* Internal code calls these private helpers rather than the public functions, so a user
   definition for a public symbol can't change how the rest of the package behaves. *)
branches[expr_] := {branch[expr, 1], branch[expr, -1]}

pmQ[expr_] := !FreeQ[normalizeSparse[expr], _PlusMinus | _MinusPlus | _PmBranches]

PmQ[expr_] := pmQ[expr]

PmToList[expr_] := branches[expr]


(* ::Section:: *)
(*Recombining branches*)


(* The default simplifier gives up after 5 seconds per half and returns that half as it is.
   Plain Simplify can take minutes on small inputs: a three-term trigonometric sum took 50 s.
   Use "Simplifier" -> Simplify for no limit, or Identity to skip simplifying. *)
$defaultSimplifier = TimeConstrained[Simplify[#], 5, #] &;
$simplifierOptions = {"Simplifier" -> $defaultSimplifier};

(* Only "Simplifier" -> f counts as an option. A general OptionsPattern[] after args___
   would also swallow a trailing data argument that happens to be a rule, as in
   PmThread[ReplaceAll, expr, x -> 2]. *)
simplifierOptionQ[(Rule | RuleDelayed)[name_String, _]] := name === "Simplifier"
simplifierOptionQ[(Rule | RuleDelayed)[name_Symbol, _]] := SymbolName[name] === "Simplifier"
simplifierOptionQ[{__?simplifierOptionQ}] := True
simplifierOptionQ[_] := False

simplifier[f_, {opts___}] := OptionValue[f, Flatten[{opts}], "Simplifier"]

halves[{p_, m_}, simp_] := {simp[(p + m)/2], simp[(p - m)/2]}

(* zeroQ also catches inexact zeros such as 0. and zero quantities, so PmFromList[{1.5, 1.5}]
   gives 1.5 rather than 1.5 \[PlusMinus] 0., and equal Quantity branches don't get "\[PlusMinus] 0 m". *)
zeroQ[e_Quantity] := zeroQ[QuantityMagnitude[e]]
zeroQ[e_] := e === 0 || (NumericQ[e] && TrueQ[e == 0])

(* With machine numbers, averaging two roots like {Sqrt[2.], -Sqrt[2.]} leaves rounding noise
   (-1.1*10^-16) in one half. A half that is below the precision of the other is noise. *)
noiseQ[e_, other_] := MachineNumberQ[e] && MachineNumberQ[other] && Abs[e] <= 10 $MachineEpsilon Abs[other]

combine[{base_, pm_}] := Which[
	zeroQ[pm] || noiseQ[pm, base], base,
	zeroQ[base] || noiseQ[base, pm], PlusMinus[pm],
	True, PlusMinus[base, pm]
]

(* Recombining is where (p + m)/2 \[PlusMinus] (p - m)/2 can go wrong, so it is done in these ways:
   - Identical branches that aren't plain values are returned as they are, without
     evaluating anything held inside them (such as the right side of :>).
   - Branches with the same structure (lists, associations with the same keys, rules with
     the same left side, equations, inequalities and logical combinations of them) are
     combined element by element: averaging two equations or two rules is meaningless.
     Relations and logical expressions are simplified as a whole first; if a branch
     becomes True or False, the branches are kept apart.
   - Branches that can't be halved safely are kept as PmBranches[p, m]. With an infinite
     branch, (p + m)/2 and (p - m)/2 are both infinite and the finite branch is lost. With
     different conditions, Simplify merges the halves under both conditions, so a valid
     branch becomes Undefined. (Branches with the same condition are combined under it.)
   - Everything else is halved and simplified. *)
$logicalHeads = And | Or | Not | Xor | Nand | Nor | Implies | Equivalent;
$relationalHeads = Equal | Unequal | Less | LessEqual | Greater | GreaterEqual | Inequality | $logicalHeads;
$structuralHeads = List | Rule | $relationalHeads;

sameStructureQ[p_Association, m_Association] := Sort[Keys[p]] === Sort[Keys[m]]
(* A rule with a different left side is a different rule, not the same rule with another value. *)
sameStructureQ[Rule[k1_, _], Rule[k2_, _]] := k1 === k2
(* Inequality[a, Less, b, LessEqual, c]: the relation symbols must match, only the operands combine. *)
(* Parts are taken from List @@ p: p[[2 ;; ;; 2]] would build and evaluate a new Inequality. *)
sameStructureQ[p_Inequality, m_Inequality] := Length[p] === Length[m] && (List @@ p)[[2 ;; ;; 2]] === (List @@ m)[[2 ;; ;; 2]]
sameStructureQ[p_, m_] := Head[p] === Head[m] && MatchQ[Head[p], $structuralHeads] && Length[p] === Length[m]

(* Values that can't be halved. Only the value itself and its top-level terms are checked:
   an Infinity deep inside, as in f[Infinity] or a series at Infinity, is harmless. *)
(* Values that can't be halved and rebuilt exactly:
   - non-finite values, found through arithmetic (y + x Infinity) and Piecewise pieces, but
     not inside other functions: f[Infinity] or a series at Infinity is harmless.
     Overflow[] and Underflow[] evaluate to special Reals, so they are compared with SameQ.
   - objects whose arithmetic doesn't undo exactly, anywhere in the value: Interval and
     CenteredInterval widen, Around combines errors as if they were independent.
   - things that aren't values at all. *)
$nonFinite = _DirectedInfinity | Indeterminate | Undefined | _ConditionalExpression;
$inexactObjects = _Interval | _CenteredInterval | _Around;
$nonValue = True | False | $Failed | _Missing | _String | _Association | _RuleDelayed | _DateObject |
	_Element | _NotElement | _Exists | _ForAll | $structuralHeads[___];

nonFiniteQ[e_] := MatchQ[e, $nonFinite] || e === Overflow[] || e === Underflow[] ||
	(MatchQ[e, _Plus | _Times | _Power] && AnyTrue[List @@ e, nonFiniteQ]) ||
	(MatchQ[e, _Piecewise] && AnyTrue[Append[e[[1, All, 1]], If[Length[e] > 1, e[[2]], 0]], nonFiniteQ])

unsafeQ[e_] := MatchQ[e, $nonValue] || nonFiniteQ[e] || !FreeQ[e, $inexactObjects]

(* Halving two machine numbers of very different size cancels: {1.*^20, 1.} would give
   5.*^19 \[PlusMinus] 5.*^19, whose "-" branch is 0. If the halves don't rebuild the branches,
   the branches are kept apart. Exact values always rebuild, so only inexact numbers are checked. *)
rebuildsQ[{p_, m_}, {base_, pm_}] := !(NumericQ[p] && NumericQ[m] && Precision[{p, m}] < Infinity) ||
	TrueQ[base + pm == p && base - pm == m]

recombine[{p_, m_}, _] /; p === m && unsafeQ[p] := p
recombine[{p_Association, m_Association}, simp_] /; sameStructureQ[p, m] :=
	Association[KeyValueMap[#1 -> recombine[{#2, m[#1]}, simp] &, p]]
recombine[{p : $relationalHeads[___], m_}, simp_] /; sameStructureQ[p, m] := With[{sp = simp[p], sm = simp[m]},
	If[MatchQ[sp, True | False] || MatchQ[sm, True | False],
		If[sp === sm, sp, PmBranches[sp, sm]],
		Head[p] @@ MapThread[If[MatchQ[Head[p], $logicalHeads], logicalOperand, recombine][{#1, #2}, simp] &, {List @@ p, List @@ m}]
	]
]

(* An operand of And, Or, ... is a truth value. Two different ones are kept apart rather than
   averaged ((p + r)/2 \[PlusMinus] (p - r)/2 is meaningless for truth values); relations inside are
   combined as usual. *)
logicalOperand[{a_, b_}, simp_] := Which[
	a === b, a,
	sameStructureQ[a, b], recombine[{a, b}, simp],
	True, PmBranches[simp[a], simp[b]]
]
(* The left side of a rule is a pattern to match, not a value: it is kept exactly as it is. *)
recombine[{Rule[k_, v1_], Rule[k_, v2_]}, simp_] := Rule[k, recombine[{v1, v2}, simp]]
recombine[{p_, m_}, simp_] /; sameStructureQ[p, m] :=
	Head[p] @@ MapThread[recombine[{#1, #2}, simp] &, {List @@ p, List @@ m}]
recombine[{ConditionalExpression[p_, c_], ConditionalExpression[m_, c_]}, simp_] :=
	ConditionalExpression[recombine[{p, m}, simp], c]
recombine[{p_, m_}, simp_] /; unsafeQ[p] || unsafeQ[m] := PmBranches[simp[p], simp[m]]
recombine[{p_, m_}, simp_] := With[{h = halves[{p, m}, simp]},
	If[rebuildsQ[{p, m}, h], combine[h], PmBranches[simp[p], simp[m]]]
]

Options[PmFromList] = $simplifierOptions;

PmFromList[{p_, m_}, opts___?simplifierOptionQ] := recombine[{p, m}, simplifier[PmFromList, {opts}]]

PmFromList[other : Except[{_, _}], ___?simplifierOptionQ] := (Message[PmFromList::arg, Short[other]]; $Failed)


(* ::Section:: *)
(*Taking expressions apart*)


(* A base and a \[PlusMinus] part exist only when both branches are ordinary values, or lists of
   them. For anything else (an infinite branch, equations, rules, PmBranches of such) the
   halves would be meaningless, so these functions give General::nobase and $Failed. *)
(* The pair is checked, not each branch alone: a list branch with a scalar branch, or lists of
   different lengths, would otherwise be broadcast or fail inside Plus. *)
splittableQ[{p_List, m_List}] := Length[p] === Length[m] && And @@ MapThread[splittableQ[{#1, #2}] &, {p, m}]
splittableQ[{p_, m_}] := !ListQ[p] && !ListQ[m] && !unsafeQ[p] && !unsafeQ[m] && rebuildsQ[{p, m}, halves[{p, m}, Identity]]

parts[f_, expr_, simp_] := With[{b = branches[expr]},
	If[splittableQ[b], halves[b, simp], Message[MessageName[f, "nobase"], Short[b]]; $Failed]
]

Options[PmParts] = $simplifierOptions;
PmParts[expr_, opts___?simplifierOptionQ] := parts[PmParts, expr, simplifier[PmParts, {opts}]]

Options[PmBase] = $simplifierOptions;
PmBase[expr_, opts___?simplifierOptionQ] := Replace[parts[PmBase, expr, simplifier[PmBase, {opts}]], {b_, _} :> b]

Options[PmPm] = $simplifierOptions;
PmPm[expr_, opts___?simplifierOptionQ] := Replace[parts[PmPm, expr, simplifier[PmPm, {opts}]], {_, pm_} :> pm]

(* Flipping maps s to -s: swap \[PlusMinus] and \[MinusPlus], and swap the arguments of PmBranches.
   The heads are swapped through temporary symbols, and every replacement is a literal, so it
   also works inside held expressions (Hold, Function, the right side of :>), where nothing
   on the right-hand side of a rule would be evaluated. //. reaches PmBranches nested inside
   the arguments of another one. *)
flip[expr_] := Module[{pm, mp, br},
	normalizeSparse[expr] /. {PlusMinus -> mp, MinusPlus -> pm, PmBranches -> br} /.
		{mp -> MinusPlus, pm -> PlusMinus} //. br[p_, m_] :> PmBranches[m, p] /. br -> PmBranches
]

PmFlip[expr_] := flip[expr]


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


(* PmCompare and PmEqualQ simplify with the same 5 second limit as the default simplifier:
   plain Simplify took over 100 s on a small trigonometric sum. A comparison that isn't
   decided in time stays an unevaluated equation (PmCompare) or counts as not proven (PmEqualQ). *)
Options[PmCompare] = $simplifierOptions;

(* MapThread rather than Thread[l1 == l2]: identical lists would make l1 == l2 evaluate to a single True. *)
PmCompare[x_, y_, opts___?simplifierOptionQ] := With[{simp = simplifier[PmCompare, {opts}]},
	With[{px = parts[PmCompare, x, simp], py = parts[PmCompare, y, simp]},
		If[px === $Failed || py === $Failed, $Failed, simp /@ MapThread[Equal, {px, py}]]
	]
]

(* Branches are compared directly rather than through PmCompare's parts, so expressions
   with an infinite branch, equations or PmBranches can still be compared. *)
PmEqualQ[x_, y_] := x === y || AllTrue[MapThread[Equal, {branches[x], branches[y]}], TrueQ[$defaultSimplifier[#]] &]


(* ::Section:: *)
(*Solving*)


(* True for expressions that print with a leading minus sign, e.g. -3, -x, -Sqrt[2]/2, -b - c,
   -I, -1 + 2 I. A number is compared by its real part, or by its imaginary part when the
   real part is 0. (Comparing a complex number with e < 0 gives Less::nord.) *)
negativeNumberQ[e_] := NumericQ[e] && With[{re = Re[e], im = Im[e]},
	TrueQ[If[TrueQ[re == 0], im < 0, re < 0]]
]
leadingMinusQ[ConditionalExpression[e_, _]] := leadingMinusQ[e]
leadingMinusQ[e_Plus] := AllTrue[List @@ e, leadingMinusQ]
leadingMinusQ[e_] := MatchQ[e, _?negativeNumberQ | Times[_?negativeNumberQ, __]]

ruleListsQ[sols_] := MatchQ[sols, {{(_Rule) ..} ..}]

(* The order of Solve's solutions is arbitrary, so we may swap them freely. We swap so that
   the first nonzero \[PlusMinus] part has no leading minus sign: x -> 1 \[PlusMinus] Sqrt[2] rather
   than x -> 1 \[PlusMinus] -Sqrt[2]. The same order is used for every variable, so the signs stay
   correlated between variables. *)
mergePair[{sol1_, sol2_}, simp_] := Module[{keys = sol1[[All, 1]], pair, pms},
	pair = {keys /. sol1, keys /. sol2};
	pms = MapThread[Last[halves[{#1, #2}, simp]] &, pair];
	If[leadingMinusQ[SelectFirst[pms, # =!= 0 &, 0]], pair = Reverse[pair]];
	{MapThread[#1 -> recombine[{#2, #3}, simp] &, {keys, pair[[1]], pair[[2]]}]}
]

(* "Simplifier" -> f is PmSolve's own option; every other extra argument goes to Solve. *)
Options[PmSolve] = $simplifierOptions;

PmSolve[eqns_, vars_, rest___] /; AnyTrue[{rest}, simplifierOptionQ] :=
	Block[{$solveSimplifier = simplifier[PmSolve, Select[{rest}, simplifierOptionQ]]},
		PmSolve[eqns, vars, Sequence @@ Select[{rest}, !simplifierOptionQ[#] &]]
	]

$solveSimplifier := simplifier[PmSolve, {}]

PmSolve[eqns_, vars_, rest___] /; !pmQ[eqns] := Module[{sols = Solve[eqns, vars, rest]},
	Which[
		(* Solve couldn't solve it and has already said why. *)
		!ListQ[sols], sols,
		Length[sols] =!= 2 || !ruleListsQ[sols],
			Message[PmSolve::nopair, Length[sols]]; sols,
		Sort[sols[[1, All, 1]]] =!= Sort[sols[[2, All, 1]]],
			Message[PmSolve::vars, sols[[1, All, 1]], sols[[2, All, 1]]]; sols,
		True, mergePair[SortBy[#, First] & /@ sols, $solveSimplifier]
	]
]

(* Equations that contain \[PlusMinus] have their own sign. Solving them in one go would tie that
   sign to the sign merging Solve's pair of roots, and lose half the solutions. Instead each
   branch is solved and the solutions are paired: a solution of the "+" equation with one
   of the "-" equation, so every pair is a valid solution for the equations' sign.
   - Repeated roots are counted once, and solutions found on both branches don't depend on
     the sign: they are returned as they are.
   - The rest are paired in order: real solutions before complex ones, then by real part and
     imaginary part, when they are numeric; otherwise in Solve's order. Any such pairing is
     valid; it is one choice among several.
   - If one branch has more remaining solutions than the other, its extra ones are paired with
     solutions of the other branch that were already used (each pair is still valid, and every
     solution still appears).
   - If a branch has no solutions at all, or paired solutions are for different variables,
     nothing can be paired: the result is PmBranches[sols+, sols-]. *)
rootOrder[sol_] := With[{v = N[sol[[All, 2]]]},
	If[VectorQ[v, NumericQ], {0, Boole[Max[Abs[Im[v]]] > 10^-10], Re[v], Im[v]}, {1}]
]

padTo[list_, n_, pool_] := If[Length[list] >= n, list, Join[list, PadRight[{}, n - Length[list], pool]]]

PmSolve[eqns_, vars_, rest___] /; pmQ[eqns] := Module[{sols, plus, minus, common, n},
	sols = Solve[#, vars, rest] & /@ branches[eqns];
	If[!AllTrue[sols, ListQ] || MemberQ[sols, {}], Return[PmBranches @@ sols]];
	{plus, minus} = DeleteDuplicates /@ Map[SortBy[#, First] &, sols, {2}];
	common = Select[plus, MemberQ[minus, #] &];
	{plus, minus} = SortBy[Select[#, !MemberQ[common, #] &], rootOrder] & /@ {plus, minus};
	n = Max[Length[plus], Length[minus]];
	{plus, minus} = {padTo[plus, n, Join[common, plus]], padTo[minus, n, Join[common, minus]]};
	If[!And @@ MapThread[#1[[All, 1]] === #2[[All, 1]] &, {plus, minus}], Return[PmBranches @@ sols]];
	Join[common, MapThread[recombine[{#1, #2}, $solveSimplifier] &, {plus, minus}]]
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
	PmCompare -> {"PmCompare[expr1, expr2, \"Simplifier\" -> f]", {_, _, OptionsPattern[]}},
	PmEqualQ -> {"PmEqualQ[expr1, expr2]", {_, _}},
	PmFlip -> {"PmFlip[expr]", {_}},
	PmSolve -> {"PmSolve[eqns, vars, ..., \"Simplifier\" -> f]", {_, _, ___}}
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
