(* Tests for PmQ, PmToList and PmFromList: splitting into branches and recombining. *)

Needs["PMFunctions`"]


(* ::Section:: *)
(*PmQ*)


VerificationTest[PmQ[PlusMinus[a, b]], True, TestID -> "PmQ-plusminus"]
VerificationTest[PmQ[MinusPlus[a, b]], True, TestID -> "PmQ-minusplus"]
VerificationTest[PmQ[x^2 + Sin[PlusMinus[a, b]]], True, TestID -> "PmQ-nested"]
VerificationTest[PmQ[a + b], False, TestID -> "PmQ-plain"]
VerificationTest[PmQ[3], False, TestID -> "PmQ-number"]


(* ::Section:: *)
(*PmToList*)


VerificationTest[PmToList[PlusMinus[a, b]], {a + b, a - b}, TestID -> "PmToList-basic"]
VerificationTest[PmToList[MinusPlus[a, b]], {a - b, a + b}, TestID -> "PmToList-minusplus"]
VerificationTest[PmToList[PlusMinus[x]], {x, -x}, TestID -> "PmToList-unary"]
VerificationTest[PmToList[MinusPlus[x]], {-x, x}, TestID -> "PmToList-unary-minusplus"]
VerificationTest[PmToList[PlusMinus[a, b, c]], {a + b + c, a - b - c}, TestID -> "PmToList-nary"]
VerificationTest[PmToList[a + b], {a + b, a + b}, TestID -> "PmToList-no-pm"]

VerificationTest[
	PmToList[PlusMinus[a, b]^2],
	{(a + b)^2, (a - b)^2},
	TestID -> "PmToList-inside-power"
]

VerificationTest[
	PmToList[PlusMinus[a, b] + PlusMinus[c, d]],
	{a + b + c + d, a - b + c - d},
	TestID -> "PmToList-signs-are-correlated"
]

VerificationTest[
	PmToList[PlusMinus[a, b] MinusPlus[c, d]],
	{(a + b) (c - d), (a - b) (c + d)},
	TestID -> "PmToList-mixed-plusminus-minusplus"
]

VerificationTest[
	PmToList[PlusMinus[a, PlusMinus[b, c]]],
	{a + b + c, a - b + c},
	TestID -> "PmToList-nested-pm"
]


(* ::Section:: *)
(*PmFromList*)


VerificationTest[PmFromList[{a + b, a - b}], PlusMinus[a, b], TestID -> "PmFromList-basic"]
VerificationTest[PmFromList[{a - b, a + b}], PlusMinus[a, -b], TestID -> "PmFromList-reversed"]
VerificationTest[PmFromList[{x, x}], x, TestID -> "PmFromList-equal-branches"]
VerificationTest[PmFromList[{x, -x}], PlusMinus[x], TestID -> "PmFromList-opposite-branches"]
VerificationTest[PmFromList[{3, 1}], PlusMinus[2, 1], TestID -> "PmFromList-numbers"]

VerificationTest[
	PmFromList[{(a + b)^2, (a - b)^2}, "Simplifier" -> Expand],
	PlusMinus[a^2 + b^2, 2 a b],
	TestID -> "PmFromList-simplifier-option"
]

VerificationTest[
	PmFromList[{p, m}, "Simplifier" -> Identity],
	PlusMinus[(p + m)/2, (p - m)/2],
	TestID -> "PmFromList-identity-simplifier"
]

VerificationTest[
	PmFromList[{a, b, c}],
	$Failed,
	{PmFromList::arg},
	TestID -> "PmFromList-wrong-length"
]


(* ::Section:: *)
(*Round trips*)


VerificationTest[PmFromList[PmToList[PlusMinus[a, b]]], PlusMinus[a, b], TestID -> "roundtrip-plusminus"]
VerificationTest[PmFromList[PmToList[MinusPlus[a, b]]], PlusMinus[a, -b], TestID -> "roundtrip-minusplus"]
VerificationTest[PmToList[PmFromList[{p, m}]], {p, m}, TestID -> "roundtrip-list"]
