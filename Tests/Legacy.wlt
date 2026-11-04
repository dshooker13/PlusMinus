(* Backwards compatibility: the examples documented in the original 2024 PlusMinus.wl
   package must keep giving the same answers. *)

Needs["PMFunctions`"]

pmSameQ[x_, y_] := Which[
	Head[x] === PlusMinus && Head[y] === PlusMinus && Length[x] === Length[y],
		And @@ MapThread[TrueQ[Simplify[#1 - #2] == 0] &, {List @@ x, List @@ y}],
	Head[x] =!= PlusMinus && Head[y] =!= PlusMinus,
		TrueQ[Simplify[x - y] == 0],
	True, False
]


VerificationTest[PmFromList[{a + b, a - b}], PlusMinus[a, b], TestID -> "legacy-PmFromList"]
VerificationTest[PmFromList[{a - b, a + b}], PlusMinus[a, -b], TestID -> "legacy-PmFromList-reversed-gives-pm-of-minus-b"]
VerificationTest[PmToList[PlusMinus[a, b]], {a + b, a - b}, TestID -> "legacy-PmToList"]
VerificationTest[PmToList[MinusPlus[a, b]], {a - b, a + b}, TestID -> "legacy-PmToList-minusplus"]
VerificationTest[PmBase[PlusMinus[a, b]], a, TestID -> "legacy-PmBase"]
VerificationTest[PmPm[PlusMinus[a, b]], b, TestID -> "legacy-PmPm"]
VerificationTest[PmPm[MinusPlus[a, b]], -b, TestID -> "legacy-PmPm-minusplus"]
VerificationTest[PmParts[PlusMinus[a, b]], {a, b}, TestID -> "legacy-PmParts"]
VerificationTest[PmParts[MinusPlus[a, b]], {a, -b}, TestID -> "legacy-PmParts-minusplus"]
VerificationTest[PmCompare[PlusMinus[a, b], PlusMinus[a, b]], {True, True}, TestID -> "legacy-PmCompare"]

VerificationTest[
	PmArithmatic[PlusMinus[a, b], PlusMinus[c, d], Divide],
	PlusMinus[(a c - b d)/(c^2 - d^2), (b c - a d)/(c^2 - d^2)],
	SameTest -> pmSameQ,
	TestID -> "legacy-PmArithmatic-Divide-example"
]

VerificationTest[
	PmCommands[PlusMinus[a b^2 + c b, f b + g b^3 + y^2], {Series, {b, 0, 1}, {y, 0, 2}}, {Normal}],
	PlusMinus[b c, b f + y^2],
	SameTest -> pmSameQ,
	TestID -> "legacy-PmCommands-example"
]

VerificationTest[PmExpand[PlusMinus[a, b]^2], PlusMinus[a^2 + b^2, 2 a b], SameTest -> pmSameQ, TestID -> "legacy-PmExpand"]

(* The original PmExpand failed on expressions without \[PlusMinus]; now it just expands. *)
VerificationTest[PmExpand[(a + b)^2], a^2 + 2 a b + b^2, TestID -> "legacy-PmExpand-no-pm-fixed"]
