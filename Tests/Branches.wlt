(* Tests for PmQ, PmToList and PmFromList: splitting into branches and recombining. *)

Get[FileNameJoin[{DirectoryName[$TestFileName], "TestUtils.wl"}]]


(* ::Section:: *)
(*PmQ*)


VerificationTest[PmToList[MinusPlus[a, b, c]], {a - b - c, a + b + c}, TestID -> "PmToList-nary-minusplus"]
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
	PmFromList[{(a + b)^2, (a - b)^2}, "Simplifier" -> Factor],
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
(* PmToList substitutes without simplifying (by design), so compare after Simplify. *)
VerificationTest[Simplify[PmToList[PmFromList[{p, m}]]], {p, m}, TestID -> "roundtrip-list"]


(* ::Section:: *)
(*Recombining branches that aren't ordinary values*)


(* Equations, inequalities, rules and lists are combined element by element, not averaged. *)
VerificationTest[PmSimplify[PlusMinus[a, b] == c], PlusMinus[a, b] == c, TestID -> "recombine-equation"]
VerificationTest[PmMap[Identity, PlusMinus[a, b] < c], PlusMinus[a, b] < c, TestID -> "recombine-inequality"]
VerificationTest[PmThread[List, x -> PlusMinus[1, 2]], {x -> PlusMinus[1, 2]}, TestID -> "recombine-rule-in-list"]
VerificationTest[PmSimplify[PmSolve[x^2 - 2 x - 1 == 0, x]], {{x -> PlusMinus[1, Sqrt[2]]}}, TestID -> "recombine-PmSolve-output"]
VerificationTest[PmFromList[{<|"a" -> 1, "b" -> 5|>, <|"b" -> 5, "a" -> 3|>}], <|"a" -> PlusMinus[2, -1], "b" -> 5|>, TestID -> "recombine-association"]
VerificationTest[PmFromList[{{1, 2}, {1, 2}}], {1, 2}, TestID -> "recombine-equal-lists"]

(* Branches that can't be safely halved are kept as PmBranches. *)
VerificationTest[PmFromList[{True, False}], PmBranches[True, False], TestID -> "recombine-booleans"]
VerificationTest[PmFromList[{True, True}], True, TestID -> "recombine-equal-booleans"]
VerificationTest[PmFromList[{{1, 2}, {3}}], PmBranches[{1, 2}, {3}], TestID -> "recombine-lists-of-different-length"]

(* One infinite branch used to make both halves infinite, losing the finite branch. *)
VerificationTest[PmArithmetic[2, PlusMinus[1, 1], Divide], PmBranches[1, ComplexInfinity], {Divide::infy}, TestID -> "recombine-singular-branch"]
VerificationTest[PmToList[PmMap[Limit[#, x -> 0] &, PlusMinus[1/x^2, 1/x^2]]], {Infinity, 0}, TestID -> "recombine-infinite-branch"]

(* Different conditions on the two branches used to be merged, so both became Undefined. *)
VerificationTest[
	PmToList[PmMap[Integrate[#, {x, 0, 1}] &, x^PlusMinus[n, 1]]] /. n -> -1/2,
	{2/3, Undefined},
	TestID -> "recombine-conditional-branches"
]


(* ::Section:: *)
(*PmBranches*)


VerificationTest[PmToList[PmBranches[a, b]], {a, b}, TestID -> "PmBranches-PmToList"]
VerificationTest[PmToList[PmBranches[PlusMinus[a, b], c]], {a + b, c}, TestID -> "PmBranches-nested-sign"]
VerificationTest[PmQ[PmBranches[1, 2]], True, TestID -> "PmBranches-PmQ"]
VerificationTest[PmFlip[PmBranches[a, PlusMinus[b, c]]], PmBranches[MinusPlus[b, c], a], TestID -> "PmBranches-PmFlip"]
VerificationTest[PmMap[1/# &, PmBranches[1, ComplexInfinity]], PlusMinus[1/2, 1/2], TestID -> "PmBranches-back-to-plusminus"]
VerificationTest[PmEqualQ[PmBranches[1, Infinity], PmBranches[1, Infinity]], True, TestID -> "PmBranches-PmEqualQ"]
VerificationTest[PmToList[PmFromList[{Infinity, x}]], {Infinity, x}, TestID -> "PmBranches-roundtrip-infinite"]
VerificationTest[PmToList[PmFromList[{Indeterminate, ConditionalExpression[x, x > 0]}]], {Indeterminate, ConditionalExpression[x, x > 0]}, TestID -> "PmBranches-roundtrip-indeterminate-conditional"]
