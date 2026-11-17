(* Property-based tests: each property is checked on random expressions against an
   independent reference branch evaluator (see PropertyOracle.wl). Each test expects {},
   the empty list of problems; otherwise it lists failures, timeouts, messages, or too many
   skipped trials. The seeds are fixed, so a failure always reproduces. *)

Get[FileNameJoin[{DirectoryName[$TestFileName], "TestUtils.wl"}]]
Get[FileNameJoin[{DirectoryName[$TestFileName], "PropertyOracle.wl"}]]


(* ::Section:: *)
(*The oracle itself*)


VerificationTest[pmRefSelfTest[], True, TestID -> "oracle-reference-self-test"]

(* The oracle must be able to fail: swapped branches, a singular result and a message all count. *)
VerificationTest[pmProperty[pmRandPmExpr[3] &, pmNumEq[PmToList[#1], Reverse[pmRef[#1]], #2] &, 50, 1] =!= {}, True, TestID -> "oracle-detects-wrong-branches"]
VerificationTest[pmProperty[pmRandPmExpr[3] &, pmNumEq[{ComplexInfinity, 0} + 0 #1, pmRef[#1], #2] &, 50, 1] =!= {}, True, TestID -> "oracle-detects-singular-results"]
VerificationTest[Quiet[pmProperty[pmRandPmExpr[3] &, (Message[General::stop, 1]; pmNumEq[pmRef[#1], pmRef[#1], #2]) &, 20, 1]] =!= {}, True, TestID -> "oracle-detects-messages"]
VerificationTest[pmProperty[pmRandPmExpr[3] &, "skip" &, 20, 1] =!= {}, True, TestID -> "oracle-detects-too-many-skips"]


(* ::Section:: *)
(*Branches*)


VerificationTest[pmProperty[pmRandPmExpr[3] &, pmNumEq[PmToList[#1], pmRef[#1], #2] &, 100, 101], {}, TestID -> "prop-PmToList-matches-reference"]
VerificationTest[pmProperty[{pmRandPlain[2], pmRandPlain[2]} &, pmNumEq[PmToList[PmFromList[#1]], #1, #2] &, 100, 102], {}, TestID -> "prop-roundtrip-from-branches"]
VerificationTest[pmProperty[pmRandPmExpr[3] &, pmNumEq[pmRef[PmFromList[PmToList[#1]]], pmRef[#1], #2] &, 100, 103], {}, TestID -> "prop-roundtrip-from-expression"]
VerificationTest[pmProperty[pmRandStructured[3] &, pmNumEq[pmRef[PmFromList[PmToList[#1]]], pmRef[#1], #2] &, 100, 121], {}, TestID -> "prop-roundtrip-structured"]


(* ::Section:: *)
(*Parts*)


VerificationTest[pmProperty[pmRandPmExpr[3] &, Module[{b = PmBase[#1], p = PmPm[#1]}, pmNumEq[{b + p, b - p}, pmRef[#1], #2]] &, 100, 104], {}, TestID -> "prop-PmBase-PmPm-reconstruct-branches"]
VerificationTest[pmProperty[pmRandPmExpr[2] &, pmNumEq[PmParts[#1], {PmBase[#1], PmPm[#1]}, #2] &, 100, 105], {}, TestID -> "prop-PmParts-is-base-and-pm"]
VerificationTest[pmProperty[pmRandPmExpr[3] &, pmNumEq[pmRef[PmFlip[#1]], Reverse[pmRef[#1]], #2] &, 100, 106], {}, TestID -> "prop-PmFlip-swaps-branches"]
VerificationTest[pmProperty[pmRandStructured[3] &, pmNumEq[pmRef[PmFlip[#1]], Reverse[pmRef[#1]], #2] &, 100, 122], {}, TestID -> "prop-PmFlip-swaps-structured-branches"]


(* ::Section:: *)
(*Operating on branches*)


VerificationTest[
	pmProperty[{RandomChoice[$pmMapFunctions], pmRandPmExpr[2]} &, pmNumEq[pmRef[PmMap[#1[[1]], #1[[2]]]], #1[[1]] /@ pmRef[#1[[2]]], #2] &, 100, 107],
	{},
	TestID -> "prop-PmMap-branchwise"
]

VerificationTest[pmProperty[pmRandStructured[3] &, pmNumEq[pmRef[PmMap[Identity, #1]], pmRef[#1], #2] &, 100, 123], {}, TestID -> "prop-PmMap-structured"]

VerificationTest[
	pmProperty[{RandomChoice[{Plus, Times}], pmRandPmExpr[2], pmRandExpr[2]} &,
		With[{op = #1[[1]], a = #1[[2]], b = #1[[3]]}, pmNumEq[pmRef[PmThread[op, a, b]], MapThread[op, {pmRef[a], pmRef[b]}], #2]] &, 100, 108],
	{},
	TestID -> "prop-PmThread-two-args-branchwise"
]

VerificationTest[
	pmProperty[{pmRandPmExpr[1], pmRandPmExpr[1], pmRandPmExpr[1]} &, pmNumEq[pmRef[PmThread[Times, Sequence @@ #1]], Times @@@ Transpose[pmRef /@ #1], #2] &, 100, 109],
	{},
	TestID -> "prop-PmThread-three-args-branchwise"
]

VerificationTest[
	pmProperty[{pmRandPmExpr[2], pmRandPmExpr[2]} &, pmNumEq[pmRef[PmArithmetic[#1[[1]], #1[[2]], Divide]], pmRef[#1[[1]]]/pmRef[#1[[2]]], #2] &, 100, 110],
	{},
	TestID -> "prop-PmArithmetic-Divide-branchwise"
]

VerificationTest[pmProperty[pmRandPmExpr[2] &, pmNumEq[pmRef[PmCommands[#1, {Expand}, {Power, 2}]], pmRef[#1]^2, #2] &, 100, 111], {}, TestID -> "prop-PmCommands-branchwise"]


(* ::Section:: *)
(*Expanding and simplifying*)


VerificationTest[pmProperty[pmRandPmExpr[3] &, pmNumEq[pmRef[PmExpand[#1]], pmRef[#1], #2] &, 100, 112], {}, TestID -> "prop-PmExpand-preserves-branches"]
VerificationTest[pmProperty[pmRandPmExpr[3] &, pmNumEq[pmRef[PmSimplify[#1]], pmRef[#1], #2] &, 100, 113], {}, TestID -> "prop-PmSimplify-preserves-branches"]
VerificationTest[pmProperty[pmRandStructured[2] &, pmNumEq[pmRef[PmSimplify[#1]], pmRef[#1], #2] &, 100, 124], {}, TestID -> "prop-PmSimplify-preserves-structured-branches"]
VerificationTest[pmProperty[pmRandPmExpr[2] &, pmNumEq[pmRef[PmFullSimplify[#1]], pmRef[#1], #2] &, 100, 114], {}, TestID -> "prop-PmFullSimplify-preserves-branches"]


(* ::Section:: *)
(*Comparing*)


(* Soundness: PmEqualQ may say False for equal expressions it can't prove equal, but it
   must never say True for expressions whose branches differ. *)
VerificationTest[
	pmProperty[pmPairGen[] &,
		Which[
			pmBadQ[Quiet[pmRef /@ #1]], "skip",
			PmEqualQ[#1[[1]], #1[[2]]], pmNumEq[pmRef[#1[[1]]], pmRef[#1[[2]]], #2],
			True, True
		] &, 100, 115],
	{},
	TestID -> "prop-PmEqualQ-sound"
]

VerificationTest[pmProperty[pmRandPmExpr[2] &, If[pmBadQ[Quiet[pmRef[#1]]], "skip", PmEqualQ[#1, PmExpand[#1]]] &, 100, 116], {}, TestID -> "prop-PmEqualQ-recognizes-expansion"]


(* ::Section:: *)
(*Solving*)


(* Every returned pair solves the matching branch of the equation, and together the pairs
   give every root of both branches. *)
VerificationTest[pmProperty[pmRandSolveCase[] &, pmSolveCheck, 60, 131], {}, TestID -> "prop-PmSolve-sign-in-equation"]
