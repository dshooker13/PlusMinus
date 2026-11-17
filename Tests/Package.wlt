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

(* Every public function goes through private helpers, so a user definition attached to a
   public symbol (after Unprotect) can't change how the others behave. *)
VerificationTest[
	Unprotect[PmToList]; PmToList[_] := "hijacked"; Protect[PmToList];
	{PmMap[#^2 &, PlusMinus[a, b]], PmExpand[PlusMinus[a, b]^2]},
	{PlusMinus[a^2 + b^2, 2 a b], PlusMinus[a^2 + b^2, 2 a b]},
	TestID -> "internal-calls-do-not-use-public-symbols"
]

(* Reloading replaces every earlier definition, including the one added above. *)
VerificationTest[
	Get[FileNameJoin[{ParentDirectory[DirectoryName[$TestFileName]], "Kernel", "PMFunctions.wl"}]];
	PmToList[PlusMinus[a, b]],
	{a + b, a - b},
	TestID -> "reload-clears-old-definitions"
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
