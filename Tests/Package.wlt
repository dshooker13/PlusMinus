(* Package behavior: protection, reloading, argument checking and options. *)

Get[FileNameJoin[{DirectoryName[$TestFileName], "TestUtils.wl"}]]


(* ::Section:: *)
(*Protection and reloading*)


VerificationTest[
	Complement[Names["PMFunctions`*"], Select[Names["PMFunctions`*"], MemberQ[Attributes[#], Protected] &]],
	{},
	TestID -> "public-symbols-protected"
]

VerificationTest[PmToList = 5, 5, {Set::wrsym}, TestID -> "cannot-assign-to-public-symbol"]
VerificationTest[PmToList[PlusMinus[a, b]], {a + b, a - b}, TestID -> "still-works-after-assignment-attempt"]

(* Every public function goes through private helpers, so user definitions attached to public
   symbols (after Unprotect) can't change how the others behave. The hijacks use specific
   patterns, which a reload would only remove by clearing; the reload at the end of the same
   test checks that, and keeps the hijacks from leaking into later tests. *)
VerificationTest[
	Module[{kernel = FileNameJoin[{ParentDirectory[DirectoryName[$TestFileName]], "Kernel", "PMFunctions.wl"}], during, after},
		Unprotect[PmToList, PmQ, PmFromList];
		PmToList[PlusMinus[a, b]] := "hijacked"; PmQ[_] := False; PmFromList[{_, _}] := "hijacked";
		Protect[PmToList, PmQ, PmFromList];
		during = {PmMap[#^2 &, PlusMinus[a, b]], PmExpand[PlusMinus[a, b]^2], Length[PmSolve[x^2 == PlusMinus[4, 1], x]]};
		Get[kernel];
		after = {PmToList[PlusMinus[a, b]], PmQ[PlusMinus[a, b]], PmFromList[{a + b, a - b}]};
		{during, after}
	],
	{{PlusMinus[a^2 + b^2, 2 a b], PlusMinus[a^2 + b^2, 2 a b], 2}, {{a + b, a - b}, True, PlusMinus[a, b]}},
	TestID -> "internal-calls-and-reload"
]


(* ::Section:: *)
(*Argument checking*)


VerificationTest[PmQ[], PmQ[], {PmQ::pmargs}, TestID -> "PmQ-no-arguments"]
VerificationTest[PmToList[a, b], PmToList[a, b], {PmToList::pmargs}, TestID -> "PmToList-two-arguments"]
VerificationTest[PmArithmetic[PlusMinus[a, b], c], PmArithmetic[PlusMinus[a, b], c], {PmArithmetic::pmargs}, TestID -> "PmArithmetic-missing-op"]
VerificationTest[PmArithmatic[PlusMinus[a, b], c], PmArithmetic[PlusMinus[a, b], c], {PmArithmetic::pmargs}, TestID -> "PmArithmatic-missing-op"]
VerificationTest[PmSolve[x^2 == 1], PmSolve[x^2 == 1], {PmSolve::pmargs}, TestID -> "PmSolve-missing-vars"]
VerificationTest[PmCompare[PlusMinus[a, b]], PmCompare[PlusMinus[a, b]], {PmCompare::pmargs}, TestID -> "PmCompare-one-argument"]
VerificationTest[PmFromList[{p, m}, 5], PmFromList[{p, m}, 5], {PmFromList::pmargs}, TestID -> "PmFromList-bad-second-argument"]

(* A misspelled option is a wrong call, not a silently ignored rule. *)
VerificationTest[
	PmMap[f, PlusMinus[a, b], "Simplifer" -> Identity],
	PmMap[f, PlusMinus[a, b], "Simplifer" -> Identity],
	{PmMap::pmargs},
	TestID -> "PmMap-misspelled-option"
]

VerificationTest[SyntaxInformation[PmMap], {"ArgumentsPattern" -> {_, _, OptionsPattern[]}, "OptionNames" -> {"\"Simplifier\""}}, TestID -> "PmMap-syntax-information"]


(* ::Section:: *)
(*Options of PmSimplify and PmFullSimplify*)


VerificationTest[PmSimplify[PlusMinus[Sqrt[x^2], y], Assumptions -> x > 0], PlusMinus[x, y], TestID -> "PmSimplify-Assumptions-option"]
VerificationTest[PmSimplify[PlusMinus[Sqrt[x^2], y], x > 0, TimeConstraint -> 10], PlusMinus[x, y], TestID -> "PmSimplify-assumption-and-option"]
VerificationTest[PmFullSimplify[PlusMinus[Gamma[x + 1]/Gamma[x], 1], TimeConstraint -> 10], PlusMinus[x, 1], TestID -> "PmFullSimplify-option-without-assumption"]

VerificationTest[
	PmSimplify[PlusMinus[a, b], "Simplifier" -> Identity],
	PmSimplify[PlusMinus[a, b], "Simplifier" -> Identity],
	{PmSimplify::pmargs},
	TestID -> "PmSimplify-rejects-Simplifier-option"
]

VerificationTest[
	PmFullSimplify[PlusMinus[a, b], x > 0, 7],
	PmFullSimplify[PlusMinus[a, b], x > 0, 7],
	{PmFullSimplify::pmargs},
	TestID -> "PmFullSimplify-rejects-extra-argument"
]


(* ::Section:: *)
(*Default simplifier*)


(* Plain Simplify took about 50 s on a three-term trig sum; the default gives up after 5 s
   per half. Checked by value rather than by timing, which would depend on the machine. *)
VerificationTest[
	Union[Options[#, "Simplifier"] & /@ {PmFromList, PmParts, PmBase, PmPm, PmMap, PmThread, PmArithmetic, PmCommands, PmCompare, PmSolve}],
	{{"Simplifier" -> (TimeConstrained[Simplify[#1], 5, #1] &)}},
	TestID -> "default-simplifier-is-time-limited"
]
VerificationTest[Options[PmExpand, "Simplifier"], {"Simplifier" -> Expand}, TestID -> "PmExpand-default-simplifier"]

(* A half that takes longer than the limit comes back unsimplified (and still correct). *)
VerificationTest[
	PmFromList[{(a^2 - b^2)/(a - b) + c, (a^2 - b^2)/(a - b) - c}, "Simplifier" -> (TimeConstrained[(Pause[1]; Simplify[#]), 0.1, #] &)],
	PlusMinus[(a^2 - b^2)/(a - b), c],
	TestID -> "timed-out-half-returned-unsimplified"
]

VerificationTest[PmMap[#^2 &, PlusMinus[a, b], "Simplifier" -> Simplify], PlusMinus[a^2 + b^2, 2 a b], TestID -> "plain-Simplify-still-available"]


(* ::Section:: *)
(*Options of every function*)


VerificationTest[PmFromList[{p, m}, Simplifier -> Identity], PlusMinus[(p + m)/2, (p - m)/2], TestID -> "Simplifier-as-a-symbol"]
VerificationTest[PmFromList[{p, m}, "Simplifier" :> Identity], PlusMinus[(p + m)/2, (p - m)/2], TestID -> "Simplifier-as-RuleDelayed"]
VerificationTest[PmFromList[{a, b}, {}], PlusMinus[(a + b)/2, (a - b)/2], TestID -> "empty-option-list"]
VerificationTest[PmExpand[PlusMinus[a, b]^2, "Simplifier" -> Identity], PlusMinus[(2 a^2 + 2 b^2)/2, 2 a b], TestID -> "PmExpand-Simplifier-option"]
VerificationTest[PmArithmetic[PlusMinus[a, b], 2, Power, "Simplifier" -> Identity], PlusMinus[((a + b)^2 + (a - b)^2)/2, ((a + b)^2 - (a - b)^2)/2], TestID -> "PmArithmetic-Simplifier-option"]
VerificationTest[PmCompare[PlusMinus[a, b]^2, PlusMinus[a^2 + b^2, 2 a b], "Simplifier" -> Simplify], {True, True}, TestID -> "PmCompare-Simplifier-option"]


(* ::Section:: *)
(*Every function rejects a wrong call*)


VerificationTest[
	Quiet[List @@ Map[Function[c, Check[c; "no message", "message"], HoldAll],
		Hold[PmQ[], PmToList[], PmFromList[], PmBase[], PmPm[], PmParts[], PmMap[f], PmThread[], PmArithmetic[a, b],
			PmCommands[], PmExpand[], PmSimplify[], PmFullSimplify[], PmCompare[a], PmEqualQ[a], PmFlip[], PmSolve[a]]]],
	ConstantArray["message", 17],
	TestID -> "every-function-messages-on-wrong-call"
]

VerificationTest[
	Complement[Names["PMFunctions`*"], Select[Names["PMFunctions`*"], SyntaxInformation[Symbol[#]] =!= {} &]],
	{"PmArithmatic", "PmBranches"},
	TestID -> "every-function-has-syntax-information"
]


(* ::Section:: *)
(*Arrays*)


VerificationTest[Head[PmMap[2 # &, SparseArray[{1 -> 1}, 3]]], SparseArray, TestID -> "SparseArray-without-sign-stays-sparse"]
VerificationTest[PmQ[IdentityMatrix[2000, SparseArray]], False, TestID -> "PmQ-large-SparseArray"]
VerificationTest[MaxMemoryUsed[PmQ[IdentityMatrix[3000, SparseArray]]] < 10^7, True, TestID -> "PmQ-SparseArray-no-dense-copy"]
VerificationTest[PmMap[Identity, PlusMinus[{{1, 2}, {3, 4}}, 1]], {{PlusMinus[1, 1], PlusMinus[2, 1]}, {PlusMinus[3, 1], PlusMinus[4, 1]}}, TestID -> "exact-array-elementwise"]
VerificationTest[First[AbsoluteTiming[PmMap[Identity, PlusMinus[RandomInteger[9, {300, 300}], 1]]]] < 1, True, TestID -> "exact-array-fast"]
VerificationTest[PmFromList[{{1, 2}, {1, 5}}], {1, PlusMinus[7/2, -3/2]}, TestID -> "exact-array-equal-elements"]
VerificationTest[
	PmCompare[PlusMinus[a, b]^2, PlusMinus[a^2 + b^2, 2 a b], "Simplifier" -> Identity],
	{((a + b)^2 + (a - b)^2)/2 == (2 a^2 + 2 b^2)/2, ((a + b)^2 - (a - b)^2)/2 == 2 a b},
	TestID -> "PmCompare-uses-its-Simplifier"
]
