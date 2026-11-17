(* Tests for PmBase, PmPm, PmParts, PmFlip, PmCompare and PmEqualQ. *)

Get[FileNameJoin[{DirectoryName[$TestFileName], "TestUtils.wl"}]]


(* ::Section:: *)
(*PmBase / PmPm / PmParts*)


VerificationTest[PmBase[PlusMinus[a, b]], a, TestID -> "PmBase-basic"]
VerificationTest[PmPm[PlusMinus[a, b]], b, TestID -> "PmPm-basic"]
VerificationTest[PmParts[PlusMinus[a, b]], {a, b}, TestID -> "PmParts-basic"]

VerificationTest[PmBase[MinusPlus[a, b]], a, TestID -> "PmBase-minusplus"]
VerificationTest[PmPm[MinusPlus[a, b]], -b, TestID -> "PmPm-minusplus"]
VerificationTest[PmParts[MinusPlus[a, b]], {a, -b}, TestID -> "PmParts-minusplus"]

VerificationTest[PmParts[PlusMinus[x]], {0, x}, TestID -> "PmParts-unary"]
VerificationTest[PmParts[x + y], {x + y, 0}, TestID -> "PmParts-no-pm"]

VerificationTest[
	PmParts[PlusMinus[a, b] + c],
	{a + c, b},
	TestID -> "PmParts-shifted"
]

VerificationTest[
	PmParts[PlusMinus[a, b]^2],
	{a^2 + b^2, 2 a b},
	TestID -> "PmParts-of-square"
]

VerificationTest[
	PmParts[PlusMinus[a, b]^2, "Simplifier" -> Identity],
	{((a + b)^2 + (a - b)^2)/2, ((a + b)^2 - (a - b)^2)/2},
	TestID -> "PmParts-simplifier-option"
]

(* The 2.0.0 fix: PmBase/PmPm used to give a^2 and 2 a b only for linear input. *)
VerificationTest[PmBase[PlusMinus[a, b]^2], a^2 + b^2, TestID -> "PmBase-nonlinear"]
VerificationTest[PmPm[PlusMinus[a, b]^2], 2 a b, TestID -> "PmPm-nonlinear"]
VerificationTest[PmBase[PlusMinus[a, b]^2, "Simplifier" -> Identity], ((a + b)^2 + (a - b)^2)/2, TestID -> "PmBase-simplifier-option"]
VerificationTest[PmPm[PlusMinus[a, b]^2, "Simplifier" -> Identity], ((a + b)^2 - (a - b)^2)/2, TestID -> "PmPm-simplifier-option"
]

VerificationTest[
	PmParts[2 PlusMinus[a, b]],
	{2 a, 2 b},
	TestID -> "PmParts-scaled"
]


(* ::Section:: *)
(*PmFlip*)


VerificationTest[PmFlip[PlusMinus[a, b]], MinusPlus[a, b], TestID -> "PmFlip-basic"]
VerificationTest[PmFlip[MinusPlus[a, b]], PlusMinus[a, b], TestID -> "PmFlip-back"]

VerificationTest[
	PmFlip[PlusMinus[a, MinusPlus[b, c]]],
	MinusPlus[a, PlusMinus[b, c]],
	TestID -> "PmFlip-nested"
]

VerificationTest[
	PmToList[PmFlip[PlusMinus[a, b]^2 + c]],
	Reverse[PmToList[PlusMinus[a, b]^2 + c]],
	TestID -> "PmFlip-swaps-branches"
]


(* ::Section:: *)
(*PmCompare / PmEqualQ*)


VerificationTest[PmCompare[PlusMinus[a, b], PlusMinus[a, b]], {True, True}, TestID -> "PmCompare-equal"]
VerificationTest[PmCompare[PlusMinus[a, b], PlusMinus[a, c]], {True, b == c}, TestID -> "PmCompare-different-pm"]

VerificationTest[
	PmCompare[PlusMinus[a, b]^2, PlusMinus[a^2 + b^2, 2 a b]],
	{True, True},
	TestID -> "PmCompare-needs-simplification"
]

VerificationTest[PmEqualQ[PlusMinus[a, b], MinusPlus[a, -b]], True, TestID -> "PmEqualQ-minusplus"]
VerificationTest[PmEqualQ[PlusMinus[a, b], PlusMinus[a, -b]], False, TestID -> "PmEqualQ-not-equal"]
VerificationTest[PmEqualQ[PlusMinus[x], PlusMinus[0, x]], True, TestID -> "PmEqualQ-unary"]
VerificationTest[PmEqualQ[PlusMinus[a, b]^2, PlusMinus[a^2 + b^2, 2 a b]], True, TestID -> "PmEqualQ-needs-expansion"]
VerificationTest[PmEqualQ[PlusMinus[x, y] MinusPlus[x, y], x^2 - y^2], True, TestID -> "PmEqualQ-difference-of-squares"]
VerificationTest[PmEqualQ[PlusMinus[a, b]^2, MinusPlus[a^2 + b^2, 2 a b]], False, TestID -> "PmEqualQ-flipped-sign-is-different"]


(* ::Section:: *)
(*Branches without a base and \[PlusMinus] part*)


VerificationTest[PmParts[PmBranches[1, ComplexInfinity]], $Failed, {PmParts::nobase}, TestID -> "PmParts-infinite-branch"]
VerificationTest[PmBase[x == PlusMinus[a, b]], $Failed, {PmBase::nobase}, TestID -> "PmBase-equation"]
VerificationTest[PmPm[x -> PlusMinus[a, b]], $Failed, {PmPm::nobase}, TestID -> "PmPm-rule"]
VerificationTest[PmCompare[PmBranches[1, Infinity], PmBranches[2, Infinity]], $Failed, {PmCompare::nobase, PmCompare::nobase}, TestID -> "PmCompare-infinite-branches"]
VerificationTest[PmEqualQ[PmBranches[1, Infinity], PmBranches[2, Infinity]], False, TestID -> "PmEqualQ-infinite-branches-differ"]
VerificationTest[PmParts[{PlusMinus[a, b], c}], {{a, c}, {b, 0}}, TestID -> "PmParts-list-of-values"]


(* ::Section:: *)
(*PmFlip in held expressions and arrays*)


(* Flipping used to leave unevaluated private code inside held expressions, losing the sign. *)
VerificationTest[PmFlip[Hold[PlusMinus[a, b]]], Hold[MinusPlus[a, b]], TestID -> "PmFlip-Hold"]
VerificationTest[PmToList[PmFlip[HoldForm[PlusMinus[a, b]]]], {HoldForm[a - b], HoldForm[a + b]}, TestID -> "PmFlip-HoldForm-branches"]
VerificationTest[PmFlip[x :> PlusMinus[a, b]], x :> MinusPlus[a, b], TestID -> "PmFlip-RuleDelayed"]
VerificationTest[PmFlip[Function[x, PlusMinus[x, 1]]], Function[x, MinusPlus[x, 1]], TestID -> "PmFlip-Function"]
VerificationTest[PmFlip[Hold[PmBranches[1 + 1, PmBranches[a, b]]]], Hold[PmBranches[PmBranches[b, a], 1 + 1]], TestID -> "PmFlip-held-nested-PmBranches"]
VerificationTest[PmFlip[PmFlip[Hold[PlusMinus[a, MinusPlus[b, c]]]]], Hold[PlusMinus[a, MinusPlus[b, c]]], TestID -> "PmFlip-twice-held"]
VerificationTest[PmToList[PmFlip[SparseArray[{1 -> PlusMinus[a, b]}, 2]]], {{a - b, 0}, {a + b, 0}}, TestID -> "PmFlip-SparseArray"]


(* ::Section:: *)
(*Branches of different shapes have no parts*)


VerificationTest[PmParts[PmBranches[{1, 2}, 3]], $Failed, {PmParts::nobase}, TestID -> "PmParts-list-and-scalar"]
VerificationTest[PmBase[PmBranches[{1, 2}, {1, 2, 3}]], $Failed, {PmBase::nobase}, TestID -> "PmBase-lists-of-different-length"]
VerificationTest[PmCompare[PmBranches[{1, 2}, 3], PmBranches[{1, 2}, {3, 3}]], $Failed, {PmCompare::nobase}, TestID -> "PmCompare-different-shapes"]
VerificationTest[PmParts[PmBranches[1.*^20, 1.]], $Failed, {PmParts::nobase}, TestID -> "PmParts-cancelling-machine-numbers"]
VerificationTest[PmParts[PmBranches[{1, 2}, {3, 4}]], {{2, 3}, {-1, -1}}, TestID -> "PmParts-lists-of-same-length"]
