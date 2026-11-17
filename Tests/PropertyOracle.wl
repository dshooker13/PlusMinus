(* ::Package:: *)

(* Property-based checks for PMFunctions, used by Tests/Properties.wlt.

   pmRef is a reference branch evaluator written independently of the package: a structural
   recursion over the expression tree, not the package's ReplaceRepeated rules.
   s = +1 is the "+" branch and s = -1 the "-" branch.
     PlusMinus[]         -> 0
     PlusMinus[x]        -> s x
     PlusMinus[x, y...]  -> x + s (y + ...)
     MinusPlus[x]        -> -s x
     MinusPlus[x, y...]  -> x - s (y + ...)
     PmBranches[p, m]    -> p when s = +1, m when s = -1

   A property is checked on random samples: symbols are replaced by random rationals and
   the two sides are compared value by value. pmProperty returns a list of problems, so a
   passing property gives {}. A trial counts as a failure when the values differ, when the
   check times out, or when evaluating the functions under test emits a message. It counts
   as skipped only when the reference value itself is singular, and a property fails if
   more than a quarter of its trials are skipped, so it can't pass by checking nothing.

   All names start with pm so they can't collide with a user's symbols when a test file is
   run in a notebook. *)

BeginPackage["PMPropertyOracle`", {"PMFunctions`"}];

pmRef::usage = "pmRef[e] gives the two branches {p, m} of e using the independent reference evaluator.";
pmRefSelfTest::usage = "pmRefSelfTest[] is True if pmRef agrees with hand-checked branches.";
pmProperty::usage = "pmProperty[gen, check, trials, seed] runs check[gen[], subs] trials times and returns a list of problems ({} if the property holds). check returns True, False or \"skip\".";
pmNumEq::usage = "pmNumEq[tested, reference, subs] substitutes subs and compares value by value: True, False, or \"skip\" when the reference value is singular.";
pmRandPmExpr::usage = "pmRandPmExpr[d] is a random expression of depth d containing at least one sign.";
pmRandPlain::usage = "pmRandPlain[d] is a random expression of depth d without signs.";
pmRandExpr::usage = "pmRandExpr[d] is a random expression of depth d.";
pmRandStructured::usage = "pmRandStructured[d] is a random equation, inequality, rule, list, PmBranches, Sqrt or Exp built around expressions with signs.";
pmPairGen::usage = "pmPairGen[] is a random pair {e1, e2} of expressions that may or may not be equivalent.";
pmBadQ::usage = "pmBadQ[v] is True if v contains a singular value.";
pmSolveCheck::usage = "pmSolveCheck[{eqn, x}, subs] checks PmSolve on an equation containing \[PlusMinus]: every returned pair solves the matching branch of the equation, and together they give every root.";
pmRandSolveCase::usage = "pmRandSolveCase[] is a random {equation, x} with \[PlusMinus] in a coefficient.";
$pmMapFunctions::usage = "Functions used to test PmMap.";

Begin["`Private`"];

$vars = {x, y, z};

refBranch[PlusMinus[], _] := 0;
refBranch[MinusPlus[], _] := 0;
refBranch[PlusMinus[u_], s_] := s refBranch[u, s];
refBranch[PlusMinus[u_, v__], s_] := refBranch[u, s] + s Total[refBranch[#, s] & /@ {v}];
refBranch[MinusPlus[u_], s_] := -s refBranch[u, s];
refBranch[MinusPlus[u_, v__], s_] := refBranch[u, s] - s Total[refBranch[#, s] & /@ {v}];
refBranch[PmBranches[u_, v_], s_] := refBranch[If[s === 1, u, v], s];
refBranch[e_?AtomQ, _] := e;
refBranch[h_[args___], s_] := refBranch[h, s] @@ (refBranch[#, s] & /@ {args});
pmRef[e_] := {refBranch[e, 1], refBranch[e, -1]};

pmRefSelfTest[] := And[
	pmRef[PlusMinus[a, b]] === {a + b, a - b},
	pmRef[MinusPlus[a, b]] === {a - b, a + b},
	pmRef[PlusMinus[x]] === {x, -x},
	pmRef[x + PlusMinus[]] === {x, x},
	pmRef[PlusMinus[a, b, c]] === {a + b + c, a - b - c},
	pmRef[PlusMinus[a, PlusMinus[b, c]]] === {a + b + c, a - b + c},
	pmRef[PlusMinus[a, MinusPlus[b, c]]] === {a + b - c, a - b - c},
	pmRef[PmBranches[PlusMinus[a, b], c]] === {a + b, c},
	Expand[pmRef[PlusMinus[a, b] MinusPlus[c, d]]] === Expand[{(a + b) (c - d), (a - b) (c + d)}]
];

randAtom[] := RandomChoice[{4, 3, 2} -> {RandomChoice[$vars], RandomInteger[{-3, 3}], RandomInteger[{1, 4}]/RandomInteger[{1, 3}]}];

randPm[d_] := With[{h = RandomChoice[{3, 1} -> {PlusMinus, MinusPlus}], n = RandomChoice[{2, 6, 2} -> {1, 2, 3}]},
	h @@ Table[pmRandExpr[d - 1], n]];

pmRandExpr[d_] := If[d <= 0, randAtom[],
	RandomChoice[{3, 3, 3, 2, 3} -> {
		randAtom[] &,
		Plus[pmRandExpr[d - 1], pmRandExpr[d - 1]] &,
		Times[pmRandExpr[d - 1], pmRandExpr[d - 1]] &,
		Power[pmRandExpr[d - 1], RandomChoice[{-1, 2, 2, 3}]] &,
		randPm[d] &
	}][]];

okExpr[e_] := FreeQ[e, ComplexInfinity | Indeterminate | DirectedInfinity];
pmRandPmExpr[d_] := Module[{e}, While[(e = Quiet[pmRandExpr[d]]; !PmQ[e] || !okExpr[e])]; e];
pmRandPlain[d_] := Module[{e}, While[(e = Quiet[pmRandExpr[d]]; PmQ[e] || !okExpr[e])]; e];

(* Structures the package combines element by element or keeps as PmBranches. Relations
   only compare real-valued sides (no Sqrt inside them), so they evaluate to True/False. *)
pmRandStructured[d_] := Module[{e},
	While[(e = Quiet[RandomChoice[{
			pmRandPmExpr[d] &,
			(pmRandPmExpr[d - 1] == pmRandExpr[d - 1]) &,
			(pmRandPmExpr[d - 1] < pmRandExpr[d - 1]) &,
			(pmRandPmExpr[d - 1] > 0 && pmRandExpr[d - 1] < 1) &,
			(x -> pmRandPmExpr[d - 1]) &,
			{pmRandPmExpr[d - 1], pmRandExpr[d - 1]} &,
			PmBranches[pmRandExpr[d - 1], pmRandPmExpr[d - 1]] &,
			Sqrt[pmRandPmExpr[d - 1]] &,
			Exp[pmRandPmExpr[d - 1]/4] &
		}][]]; !PmQ[e] || !okExpr[e])];
	e
];

randSubs[] := Thread[$vars -> Table[RandomInteger[{-9, 9}]/RandomInteger[{1, 7}] + RandomChoice[{0, 1/11, 1/13}], Length[$vars]]];

pmBadQ[v_] := !FreeQ[v, ComplexInfinity | Indeterminate | DirectedInfinity | Overflow | Underflow | Undefined];

(* Value-by-value comparison of substituted results. *)
valEq[l_?NumericQ, r_?NumericQ] := TrueQ[Quiet[Simplify[l - r] == 0]] || TrueQ[Quiet[Abs[N[l - r, 40]] < 10^-25]]
valEq[l_, r_] /; !AtomQ[l] && !AtomQ[r] && Head[l] === Head[r] && Length[l] === Length[r] :=
	And @@ MapThread[valEq, {List @@ l, List @@ r}]
valEq[l_, r_] := l === r || TrueQ[Quiet[Simplify[l == r]]]

pmNumEq[lhs_, rhs_, subs_] := Module[{l, r},
	r = Quiet[rhs /. subs];
	If[pmBadQ[r], Return["skip"]];
	l = Quiet[lhs /. subs];
	If[pmBadQ[l] || PmQ[l] || PmQ[r], Return[False]];
	valEq[l, r]
];

(* A trial that emits messages is rerun quietly with the same substitution: if the reference
   value turns out to be singular (division by zero on a branch, say), the messages were
   expected and the trial is a skip; otherwise the messages count as a failure. *)
pmProperty[gen_, check_, trials_Integer, seed_Integer] := Module[{fail = 0, skip = 0, timeout = 0, msg = 0, r, sample, subs},
	SeedRandom[seed];
	Do[
		sample = Quiet[gen[]]; subs = randSubs[];
		(* Check still sees messages that the surrounding Quiet keeps off the screen. *)
		r = TimeConstrained[Quiet[Check[check[sample, subs], "messages"]], 20, "timeout"];
		If[r === "messages" && TimeConstrained[Quiet[check[sample, subs]], 20, "timeout"] === "skip", r = "skip"];
		Switch[r,
			True, Null,
			"skip", skip++,
			"timeout", timeout++,
			"messages", msg++,
			_, fail++
		],
		{trials}];
	Join[
		If[fail > 0, {ToString[fail] <> " of " <> ToString[trials] <> " trials failed"}, {}],
		If[timeout > 0, {ToString[timeout] <> " trials timed out"}, {}],
		If[msg > 0, {ToString[msg] <> " trials emitted messages"}, {}],
		If[skip > trials/4, {ToString[skip] <> " of " <> ToString[trials] <> " trials skipped (more than a quarter)"}, {}]
	]
];

$pmMapFunctions = {(#^2 + x #) &, (1/(1 + #^2)) &, (# - 3) &, (2 #) &, Expand, Together, (# /. x -> y) &};

pmPairGen[] := Module[{e = pmRandPmExpr[2]},
	RandomChoice[{
		{e, PmExpand[e]}, {e, PmFlip[PmFlip[e]]}, {e, PmFlip[e]},
		{e, e + RandomChoice[$vars]}, {e, pmRandPmExpr[2]}, {e, PmFromList[Reverse[pmRef[e]]]},
		{e, PmFromList[pmRef[e]]}
	}]];

(* PmSolve on x^2 + b x + c == 0 or (x - r1)(x - r2)(x - r3) == 0 with a sign in a coefficient
   or a root. Coefficients are random rationals, fixed by the sample, not by subs. *)
rat[] := RandomInteger[{-6, 6}] + RandomChoice[{0, 1/2, 1/3}];
pmRandSolveCase[] := RandomChoice[{
	{X^2 + PlusMinus[rat[], rat[]] X + rat[] == 0, X} &,
	{X^2 + rat[] X + PlusMinus[rat[], rat[]] == 0, X} &,
	{(X - rat[]) (X - PlusMinus[rat[], rat[]]) (X - rat[]) == 0, X} &,
	{X^2 == PlusMinus[rat[], rat[]] X, X} &
}][];

rootSet[eq_, v_] := Sort[N[v /. Solve[eq, v], 30]];

pmSolveCheck[{eqn_, v_}, _] := Module[{sols, eqs = pmRef[eqn], ok},
	sols = PmSolve[eqn, v];
	If[Head[sols] === PmBranches,
		(* Unpaired: each side must be exactly the solutions of its branch. *)
		Return[And @@ MapThread[Sort[N[v /. #1, 30]] == rootSet[#2, v] &, {List @@ sols, eqs}]]
	];
	If[!ListQ[sols], Return[False]];
	If[sols === {}, Return[AllTrue[eqs, rootSet[#, v] === {} &]]];
	ok = AllTrue[sols, Function[sol, With[{vals = pmRef[v /. sol]},
		And @@ MapThread[PossibleZeroQ[Subtract @@ #2 /. v -> #1] &, {vals, eqs}]
	]]];
	(* Together the pairs must give every root of each branch. *)
	ok && And @@ MapThread[
		Complement[Round[rootSet[#2, v], 10^-15], Round[N[Sort[DeleteDuplicates[#1]], 30], 10^-15]] === {} &,
		{Transpose[pmRef[v /. #] & /@ sols], eqs}
	]
];

End[];
EndPackage[];
