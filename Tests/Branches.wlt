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


(* ::Section:: *)
(*Edge cases*)


VerificationTest[PmFromList[{1.5, 1.5}], 1.5, TestID -> "inexact-equal-branches"]
VerificationTest[PmFromList[{1.5, -1.5}], PlusMinus[1.5], TestID -> "inexact-opposite-branches"]
VerificationTest[PmQ[SparseArray[{1 -> PlusMinus[a, b]}, 2]], True, TestID -> "SparseArray-PmQ"]
VerificationTest[PmToList[SparseArray[{1 -> PlusMinus[a, b]}, 2]], {{a + b, 0}, {a - b, 0}}, TestID -> "SparseArray-PmToList"]
VerificationTest[PmToList[x + PlusMinus[]], {x, x}, TestID -> "empty-PlusMinus-is-zero"]
VerificationTest[PmToList[HoldForm[PlusMinus[a, b]]], {HoldForm[a + b], HoldForm[a - b]}, TestID -> "held-expression-clean-branches"]
VerificationTest[PmToList[HoldForm[MinusPlus[a, b, c]]], {HoldForm[a - (b + c)], HoldForm[a + b + c]}, TestID -> "held-expression-nary"]
VerificationTest[PmFromList[{Interval[{1, 2}], Interval[{3, 4}]}], PmBranches[Interval[{1, 2}], Interval[{3, 4}]], TestID -> "Interval-branches-kept"]


(* ::Section:: *)
(*What counts as unsafe*)


(* An Infinity deep inside a value is harmless; only an infinite value or term is unsafe. *)
VerificationTest[PmSimplify[PlusMinus[f[Infinity], a]], PlusMinus[f[Infinity], a], TestID -> "deep-infinity-is-safe"]
VerificationTest[Head[PmMap[Series[#, {x, Infinity, 3}] &, PlusMinus[1/x, 1/x^2]]], PlusMinus, TestID -> "series-at-infinity-is-safe"]
VerificationTest[PmFromList[{Infinity + a, b}], PmBranches[Infinity + a, b], TestID -> "infinite-term-is-unsafe"]

(* Branches with the same condition are combined under it. *)
VerificationTest[
	PmMap[Integrate[#, {x, 0, 1}] &, PlusMinus[x^n, 2 x^n]],
	ConditionalExpression[PlusMinus[1/(1 + n), 2/(1 + n)], Re[n] > -1],
	TestID -> "same-condition-combined"
]
VerificationTest[Head[PmMap[Integrate[#, {x, 0, 1}] &, PlusMinus[x^n, 2 x^n]]], ConditionalExpression, TestID -> "same-condition-stays-conditional"]

(* Rules with different left sides are different rules. *)
VerificationTest[PmFromList[{x -> 1, y -> 2}], PmBranches[x -> 1, y -> 2], TestID -> "rules-with-different-keys"]
VerificationTest[PmFromList[{{a -> 1}, {b -> 1}}], {PmBranches[a -> 1, b -> 1]}, TestID -> "rule-lists-with-different-keys"]

(* Logical combinations are combined element by element; other non-values are kept apart. *)
VerificationTest[PmSimplify[PlusMinus[x, 1] > 0 && y > 0], PlusMinus[x, 1] > 0 && y > 0, TestID -> "logical-and"]
VerificationTest[PmToList[PmMap[Identity, Inequality[1, Less, PlusMinus[x, 1], LessEqual, 2]]], {Inequality[1, Less, x + 1, LessEqual, 2], Inequality[1, Less, x - 1, LessEqual, 2]}, TestID -> "inequality-chain"]
VerificationTest[PmFromList[{$Failed, 1}], PmBranches[$Failed, 1], TestID -> "failed-branch"]
VerificationTest[PmFromList[{Missing["NotAvailable"], 1}], PmBranches[Missing["NotAvailable"], 1], TestID -> "missing-branch"]

(* A relation that simplifies to True or False on a branch is decided, not rewritten. *)
VerificationTest[PmSimplify[x + PlusMinus[1, 1] < x + 3], True, TestID -> "relation-true-on-both-branches"]
VerificationTest[PmSimplify[x + PlusMinus[2, 2] < x + 3], PmBranches[False, True], TestID -> "relation-decided-differently"]

(* Identical held branches come back untouched, without evaluating the held part. *)
VerificationTest[PmFromList[{x :> RandomInteger[10], x :> RandomInteger[10]}], x :> RandomInteger[10], TestID -> "identical-held-branches"]
VerificationTest[PmFromList[{x :> 1, x :> 2}], PmBranches[x :> 1, x :> 2], TestID -> "different-delayed-rules"]

(* "Simplifier" may also be given inside a list, as OptionsPattern allows. *)
VerificationTest[PmFromList[{p, m}, {"Simplifier" -> Identity}], PlusMinus[(p + m)/2, (p - m)/2], TestID -> "option-in-a-list"]
VerificationTest[PmThread[Join, {a}, {}], {a}, TestID -> "empty-list-is-data-not-options"]


(* ::Section:: *)
(*\[PlusMinus] is two algebraic objects, not a range*)


(* A range reading would give something like Interval[{0, 1}]; the two objects are 1 and 1. *)
VerificationTest[PmToList[PlusMinus[0, 1]^2], {1, 1}, TestID -> "two-objects-not-a-range"]
VerificationTest[PmExpand[PlusMinus[0, 1]^2], 1, TestID -> "two-objects-square-collapses"]
(* The order of the objects matters: a \[PlusMinus] b and a \[MinusPlus] b are different, which they wouldn't be as ranges. *)
VerificationTest[PmEqualQ[PlusMinus[a, b], MinusPlus[a, b]], False, TestID -> "two-objects-are-ordered"]
VerificationTest[Names["PMFunctions`*Interval*"], {}, TestID -> "no-interval-functions"]


(* ::Section:: *)
(*Values whose halves don't rebuild the branches*)


(* An infinity reached through arithmetic or a Piecewise piece used to turn the finite branch into Indeterminate. *)
VerificationTest[PmToList[PmMap[# /. t -> Infinity &, y - PlusMinus[1, 1] x t]] /. {x -> 1, y -> 3}, {-Infinity, 3}, TestID -> "nested-infinity-kept"]
VerificationTest[PmFromList[{y - x Infinity, y}], PmBranches[y - x Infinity, y], TestID -> "infinity-in-a-product-term"]
VerificationTest[Quiet[PmToList[PmMap[PiecewiseExpand[1/Max[#, 0]] &, PlusMinus[a, 1]]] /. a -> 1/2], {2/3, ComplexInfinity}, TestID -> "infinite-piecewise-piece"]
VerificationTest[PmFromList[{Overflow[], 1}], PmBranches[Overflow[], 1], TestID -> "overflow-branch"]

(* Machine numbers of very different size: the halves cancel, so the branches are kept. *)
VerificationTest[PmToList[PmFromList[{1.*^20, 1.}]], {1.*^20, 1.}, TestID -> "machine-numbers-very-different-size"]
VerificationTest[PmFromList[{3., 1.}], PlusMinus[2., 1.], TestID -> "machine-numbers-ordinary"]
(* Averaging the two inexact roots leaves -1.1*10^-16 of rounding noise as the base; it is dropped. *)
VerificationTest[With[{v = x /. First[PmSolve[x^2 - 2. == 0, x]]}, {Head[v], Length[v], Abs[v[[1]] - Sqrt[2.]] < 10^-14}], {PlusMinus, 1, True}, TestID -> "no-rounding-noise-base"]

(* Around and nested Interval objects don't rebuild under arithmetic. *)
VerificationTest[PmFromList[{Around[1, 0.1], Around[3, 0.3]}], PmBranches[Around[1, 0.1], Around[3, 0.3]], TestID -> "Around-branches-kept"]
VerificationTest[Head[PmFromList[{Sqrt[x + Interval[{0, 1}]], Sqrt[x + Interval[{1, 2}]]}]], PmBranches, TestID -> "nested-Interval-kept"]
VerificationTest[Head[PmFromList[{DateObject[{2024, 1, 1}], DateObject[{2024, 1, 3}]}]], PmBranches, TestID -> "DateObject-branches-kept"]
VerificationTest[PmFromList[{Quantity[1, "Meters"], Quantity[1, "Meters"]}], Quantity[1, "Meters"], TestID -> "equal-Quantity-branches"]
VerificationTest[PmFromList[{Quantity[3, "Meters"], Quantity[1, "Meters"]}], PlusMinus[Quantity[2, "Meters"], Quantity[1, "Meters"]], TestID -> "Quantity-branches"]

(* The left side of a rule is matched literally, so it is never simplified. *)
VerificationTest[PmFromList[{(x^2 - 1)/(x - 1) -> a, (x^2 - 1)/(x - 1) -> b}], (x^2 - 1)/(x - 1) -> PlusMinus[(a + b)/2, (a - b)/2], TestID -> "rule-key-kept-literally"]

(* Truth values inside And/Or are kept apart, never averaged. *)
VerificationTest[PmFromList[{p && q, r && q}], PmBranches[p, r] && q, TestID -> "logical-operands-kept-apart"]
VerificationTest[PmToList[PmFromList[{p && q, r && q}]] /. {p -> True, r -> False, q -> True}, {True, False}, TestID -> "logical-operands-evaluate"]
VerificationTest[PmSimplify[PlusMinus[x, 1] > 0 && y > 0], PlusMinus[x, 1] > 0 && y > 0, TestID -> "relations-in-logical-still-combined"]


(* ::Section:: *)
(*Input without a sign still goes through the simplifier*)


VerificationTest[PmSimplify[(a^2 - b^2)/(a - b)], a + b, TestID -> "PmSimplify-sign-free-input"]
VerificationTest[PmFromList[{(a + b)^2, (a + b)^2}, "Simplifier" -> Expand], a^2 + 2 a b + b^2, TestID -> "PmFromList-equal-branches-simplified"]
VerificationTest[PmMap[#^2 &, a + b, "Simplifier" -> Expand], a^2 + 2 a b + b^2, TestID -> "PmMap-sign-free-input"]


(* ::Section:: *)
(*Which branches are kept apart*)


VerificationTest[Head[PmFromList[{Inequality[a, Less, b, Less, c], Inequality[a, LessEqual, b, Less, c]}]], PmBranches, TestID -> "Inequality-different-relations"]
VerificationTest[PmFromList[{Undefined, 1}], PmBranches[Undefined, 1], TestID -> "Undefined-branch"]
VerificationTest[PmFromList[{"a", "b"}], PmBranches["a", "b"], TestID -> "string-branches"]
VerificationTest[PmFromList[{Element[x, Reals], Element[x, Integers]}], PmBranches[Element[x, Reals], Element[x, Integers]], TestID -> "Element-branches"]
VerificationTest[PmFromList[{<|"a" -> 1|>, <|"b" -> 2|>}], PmBranches[<|"a" -> 1|>, <|"b" -> 2|>], TestID -> "associations-with-different-keys"]
VerificationTest[PmSimplify[PlusMinus[Sin[x]^2 + Cos[x]^2, y] == 1 + y], PmBranches[True, y == 0], TestID -> "relation-decided-on-one-branch"]
VerificationTest[PmFromList[{Infinity, (a^2 - b^2)/(a - b)}], PmBranches[Infinity, a + b], TestID -> "kept-branches-still-simplified"]
VerificationTest[PmEqualQ[PmBranches[Indeterminate, 1], PmBranches[Indeterminate, 1]], True, TestID -> "PmEqualQ-identical-singular"]
