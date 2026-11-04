(* Tests for PmMap, PmCommands, PmExpand, PmSimplify and PmFullSimplify. *)

Needs["PMFunctions`"]

pmSameQ[x_, y_] := Which[
	Head[x] === PlusMinus && Head[y] === PlusMinus && Length[x] === Length[y],
		And @@ MapThread[TrueQ[Simplify[#1 - #2] == 0] &, {List @@ x, List @@ y}],
	Head[x] =!= PlusMinus && Head[y] =!= PlusMinus,
		TrueQ[Simplify[x - y] == 0],
	True, False
]


(* ::Section:: *)
(*PmMap*)


VerificationTest[
	PmMap[D[#, x] &, PlusMinus[x^2, x^3]],
	PlusMinus[2 x, 3 x^2],
	TestID -> "PmMap-derivative"
]

VerificationTest[
	PmMap[#^2 &, PlusMinus[a, b], "Simplifier" -> Expand],
	PlusMinus[a^2 + b^2, 2 a b],
	TestID -> "PmMap-square"
]

VerificationTest[
	PmMap[# /. x -> 2 &, PlusMinus[x, 3 x]],
	PlusMinus[2, 6],
	TestID -> "PmMap-substitution"
]


(* ::Section:: *)
(*PmCommands*)


VerificationTest[
	PmCommands[PlusMinus[a b^2 + c b, f b + g b^3 + y^2], {Series, {b, 0, 1}, {y, 0, 2}}, {Normal}],
	PlusMinus[b c, b f + y^2],
	SameTest -> pmSameQ,
	TestID -> "PmCommands-series-then-normal"
]

VerificationTest[
	PmCommands[PlusMinus[x^2, x], {D, x}, {ReplaceAll, x -> 3}],
	PlusMinus[6, 1],
	TestID -> "PmCommands-chain"
]

VerificationTest[
	PmCommands[PlusMinus[a, b]],
	PlusMinus[a, b],
	TestID -> "PmCommands-no-commands"
]

VerificationTest[
	PmCommands[PlusMinus[a, b], {Power, 2}, "Simplifier" -> Expand],
	PlusMinus[a^2 + b^2, 2 a b],
	TestID -> "PmCommands-simplifier-option"
]


(* ::Section:: *)
(*PmExpand*)


VerificationTest[PmExpand[PlusMinus[a, b]^2], PlusMinus[a^2 + b^2, 2 a b], TestID -> "PmExpand-square"]
VerificationTest[PmExpand[PlusMinus[a, b]^3], PlusMinus[a^3 + 3 a b^2, 3 a^2 b + b^3], TestID -> "PmExpand-cube"]
VerificationTest[PmExpand[MinusPlus[a, b]^2], PlusMinus[a^2 + b^2, -2 a b], TestID -> "PmExpand-minusplus"]
VerificationTest[PmExpand[(a + b)^2], a^2 + 2 a b + b^2, TestID -> "PmExpand-no-pm"]
VerificationTest[PmExpand[PlusMinus[x]^2], x^2, TestID -> "PmExpand-pm-squared-away"]

VerificationTest[
	PmExpand[(x + PlusMinus[y]) (x + MinusPlus[y])],
	x^2 - y^2,
	TestID -> "PmExpand-difference-of-squares"
]


(* ::Section:: *)
(*PmSimplify / PmFullSimplify*)


VerificationTest[
	PmSimplify[PlusMinus[(a^2 - b^2)/(a - b), 2 Sin[x] Cos[x] - Sin[2 x] + c]],
	PlusMinus[a + b, c],
	SameTest -> pmSameQ,
	TestID -> "PmSimplify-basic"
]

VerificationTest[
	PmSimplify[PlusMinus[a + b, c], b == 0],
	PlusMinus[a, c],
	TestID -> "PmSimplify-assumptions"
]

VerificationTest[
	PmSimplify[PlusMinus[Sqrt[x^2], y], x > 0],
	PlusMinus[x, y],
	TestID -> "WolframOnly:PmSimplify-positive-assumption"
]

VerificationTest[
	PmFullSimplify[PlusMinus[a, b]^2 - PlusMinus[a, b]^2],
	0,
	TestID -> "PmFullSimplify-zero"
]
