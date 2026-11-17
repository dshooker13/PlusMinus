(* ::Package:: *)

(* Property-based checks for PMFunctions, used by Tests/Properties.wlt.

   refBranch is a reference branch evaluator written independently of the package: a
   structural recursion over the expression tree, not the package's ReplaceRepeated rules.
   s = +1 is the "+" branch and s = -1 the "-" branch.
     PlusMinus[x]        -> s x
     PlusMinus[x, y...]  -> x + s (y + ...)
     MinusPlus[x]        -> -s x
     MinusPlus[x, y...]  -> x - s (y + ...)
     PmBranches[p, m]    -> p when s = +1, m when s = -1
   Properties are checked numerically: symbols are replaced by random rationals, and the
   two sides are compared exactly (or to 25 digits when transcendental). *)

BeginPackage["PMPropertyOracle`", {"PMFunctions`"}];

ref::usage = "ref[e] gives the two branches {p, m} of e using the independent reference evaluator.";
refSelfTest::usage = "refSelfTest[] is True if ref agrees with hand-checked branches.";
property::usage = "property[gen, check, trials, seed] runs check[gen[], subs] trials times and returns the number of failures.";
numEq::usage = "numEq[lhs, rhs, subs] is True, False or \"skip\" (singular sample).";
randPmExpr::usage = "randPmExpr[d] is a random expression of depth d containing at least one sign.";
randPlain::usage = "randPlain[d] is a random expression of depth d without signs.";
randExpr::usage = "randExpr[d] is a random expression of depth d.";
pairGen::usage = "pairGen[] is a random pair {e1, e2} of expressions that may or may not be equivalent.";
badQ::usage = "badQ[v] is True if v contains a singular value.";
$mapFunctions::usage = "Functions used to test PmMap.";

Begin["`Private`"];

$vars = {x, y, z};

refBranch[PlusMinus[u_], s_] := s refBranch[u, s];
refBranch[PlusMinus[u_, v__], s_] := refBranch[u, s] + s Total[refBranch[#, s] & /@ {v}];
refBranch[MinusPlus[u_], s_] := -s refBranch[u, s];
refBranch[MinusPlus[u_, v__], s_] := refBranch[u, s] - s Total[refBranch[#, s] & /@ {v}];
refBranch[PmBranches[u_, v_], s_] := refBranch[If[s === 1, u, v], s];
refBranch[e_?AtomQ, _] := e;
refBranch[h_[args___], s_] := refBranch[h, s] @@ (refBranch[#, s] & /@ {args});
ref[e_] := {refBranch[e, 1], refBranch[e, -1]};

refSelfTest[] := And[
	ref[PlusMinus[a, b]] === {a + b, a - b},
	ref[MinusPlus[a, b]] === {a - b, a + b},
	ref[PlusMinus[x]] === {x, -x},
	ref[PlusMinus[a, b, c]] === {a + b + c, a - b - c},
	ref[PlusMinus[a, PlusMinus[b, c]]] === {a + b + c, a - b + c},
	ref[PlusMinus[a, MinusPlus[b, c]]] === {a + b - c, a - b - c},
	Expand[ref[PlusMinus[a, b] MinusPlus[c, d]]] === Expand[{(a + b) (c - d), (a - b) (c + d)}]
];

randAtom[] := RandomChoice[{4, 3, 2} -> {RandomChoice[$vars], RandomInteger[{-3, 3}], RandomInteger[{1, 4}]/RandomInteger[{1, 3}]}];

randPm[d_] := With[{h = RandomChoice[{3, 1} -> {PlusMinus, MinusPlus}], n = RandomChoice[{2, 6, 2} -> {1, 2, 3}]},
	h @@ Table[randExpr[d - 1], n]];

randExpr[d_] := If[d <= 0, randAtom[],
	RandomChoice[{3, 3, 3, 2, 3} -> {
		randAtom[] &,
		Plus[randExpr[d - 1], randExpr[d - 1]] &,
		Times[randExpr[d - 1], randExpr[d - 1]] &,
		Power[randExpr[d - 1], RandomChoice[{-1, 2, 2, 3}]] &,
		randPm[d] &
	}][]];

okExpr[e_] := FreeQ[e, ComplexInfinity | Indeterminate | DirectedInfinity];
randPmExpr[d_] := Module[{e}, While[(e = Quiet[randExpr[d]]; !PmQ[e] || !okExpr[e])]; e];
randPlain[d_] := Module[{e}, While[(e = Quiet[randExpr[d]]; PmQ[e] || !okExpr[e])]; e];

randSubs[] := Thread[$vars -> Table[RandomInteger[{-9, 9}]/RandomInteger[{1, 7}] + RandomChoice[{0, 1/11, 1/13}], Length[$vars]]];

badQ[v_] := !FreeQ[v, ComplexInfinity | Indeterminate | DirectedInfinity | Overflow | Underflow];

numEq[lhs_, rhs_, subs_] := Module[{l, r},
	l = Quiet[lhs /. subs]; r = Quiet[rhs /. subs];
	If[badQ[l] || badQ[r], Return["skip"]];
	If[PmQ[l] || PmQ[r], Return[False]];
	TrueQ[Quiet[Simplify[l - r] === (0 l)]] || TrueQ[Quiet[Max[Abs[N[l - r, 40]]] < 10^-25]]
];

property[gen_, check_, trials_Integer, seed_Integer] := Module[{fail = 0, r},
	SeedRandom[seed];
	Do[
		r = TimeConstrained[Quiet[check[gen[], randSubs[]]], 20, "skip"];
		If[r =!= True && r =!= "skip", fail++],
		{trials}];
	fail
];

$mapFunctions = {(#^2 + x #) &, (1/(1 + #^2)) &, (# - 3) &, (2 #) &, Expand, Together, (# /. x -> y) &};

pairGen[] := Module[{e = randPmExpr[2]},
	RandomChoice[{
		{e, PmExpand[e]}, {e, PmFlip[PmFlip[e]]}, {e, PmFlip[e]},
		{e, e + RandomChoice[$vars]}, {e, randPmExpr[2]}, {e, PmFromList[Reverse[ref[e]]]},
		{e, PmFromList[ref[e]]}
	}]];

End[];
EndPackage[];
