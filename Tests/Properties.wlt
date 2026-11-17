(* Property-based tests: each property is checked on 100 random expressions against an
   independent reference branch evaluator (see PropertyOracle.wl). Every test expects 0
   failures. The seeds are fixed, so a failure always reproduces. *)

Get[FileNameJoin[{DirectoryName[$TestFileName], "TestUtils.wl"}]]
Get[FileNameJoin[{DirectoryName[$TestFileName], "PropertyOracle.wl"}]]


(* ::Section:: *)
(*The oracle itself*)


VerificationTest[refSelfTest[], True, TestID -> "oracle-reference-self-test"]

(* The oracle must be able to fail: comparing against swapped branches has to be caught. *)
VerificationTest[
	property[randPmExpr[3] &, numEq[PmToList[#1], Reverse[ref[#1]], #2] &, 50, 1] > 10,
	True,
	TestID -> "oracle-detects-wrong-branches"
]


(* ::Section:: *)
(*Branches*)


VerificationTest[
	property[randPmExpr[3] &, numEq[PmToList[#1], ref[#1], #2] &, 100, 101],
	0,
	TestID -> "prop-PmToList-matches-reference"
]

VerificationTest[
	property[{randPlain[2], randPlain[2]} &, numEq[PmToList[PmFromList[#1]], #1, #2] &, 100, 102],
	0,
	TestID -> "prop-roundtrip-from-branches"
]

VerificationTest[
	property[randPmExpr[3] &, numEq[ref[PmFromList[PmToList[#1]]], ref[#1], #2] &, 100, 103],
	0,
	TestID -> "prop-roundtrip-from-expression"
]


(* ::Section:: *)
(*Parts*)


VerificationTest[
	property[randPmExpr[3] &, Module[{b = PmBase[#1], p = PmPm[#1]}, numEq[{b + p, b - p}, ref[#1], #2]] &, 100, 104],
	0,
	TestID -> "prop-PmBase-PmPm-reconstruct-branches"
]

VerificationTest[
	property[randPmExpr[2] &, numEq[PmParts[#1], {PmBase[#1], PmPm[#1]}, #2] &, 100, 105],
	0,
	TestID -> "prop-PmParts-is-base-and-pm"
]

VerificationTest[
	property[randPmExpr[3] &, numEq[ref[PmFlip[#1]], Reverse[ref[#1]], #2] &, 100, 106],
	0,
	TestID -> "prop-PmFlip-swaps-branches"
]


(* ::Section:: *)
(*Operating on branches*)


VerificationTest[
	property[{RandomChoice[$mapFunctions], randPmExpr[2]} &,
		numEq[ref[PmMap[#1[[1]], #1[[2]]]], #1[[1]] /@ ref[#1[[2]]], #2] &, 100, 107],
	0,
	TestID -> "prop-PmMap-branchwise"
]

VerificationTest[
	property[{RandomChoice[{Plus, Times}], randPmExpr[2], randExpr[2]} &,
		With[{op = #1[[1]], a = #1[[2]], b = #1[[3]]},
			numEq[ref[PmThread[op, a, b]], MapThread[op, {ref[a], ref[b]}], #2]] &, 100, 108],
	0,
	TestID -> "prop-PmThread-two-args-branchwise"
]

VerificationTest[
	property[{randPmExpr[1], randPmExpr[1], randPmExpr[1]} &,
		numEq[ref[PmThread[Times, Sequence @@ #1]], Times @@@ Transpose[ref /@ #1], #2] &, 100, 109],
	0,
	TestID -> "prop-PmThread-three-args-branchwise"
]

VerificationTest[
	property[{randPmExpr[2], randPmExpr[2]} &,
		numEq[ref[PmArithmetic[#1[[1]], #1[[2]], Divide]], ref[#1[[1]]]/ref[#1[[2]]], #2] &, 100, 110],
	0,
	TestID -> "prop-PmArithmetic-Divide-branchwise"
]

VerificationTest[
	property[randPmExpr[2] &, numEq[ref[PmCommands[#1, {Expand}, {Power, 2}]], ref[#1]^2, #2] &, 100, 111],
	0,
	TestID -> "prop-PmCommands-branchwise"
]


(* ::Section:: *)
(*Expanding and simplifying*)


VerificationTest[
	property[randPmExpr[3] &, numEq[ref[PmExpand[#1]], ref[#1], #2] &, 100, 112],
	0,
	TestID -> "prop-PmExpand-preserves-branches"
]

VerificationTest[
	property[randPmExpr[3] &, numEq[ref[PmSimplify[#1]], ref[#1], #2] &, 100, 113],
	0,
	TestID -> "prop-PmSimplify-preserves-branches"
]

VerificationTest[
	property[randPmExpr[2] &, numEq[ref[PmFullSimplify[#1]], ref[#1], #2] &, 100, 114],
	0,
	TestID -> "prop-PmFullSimplify-preserves-branches"
]


(* ::Section:: *)
(*Comparing*)


(* Soundness: PmEqualQ may say False for equal expressions it can't prove equal, but it
   must never say True for expressions whose branches differ. *)
VerificationTest[
	property[pairGen[] &, If[PmEqualQ[#1[[1]], #1[[2]]], numEq[ref[#1[[1]]], ref[#1[[2]]], #2], True] &, 100, 115],
	0,
	TestID -> "prop-PmEqualQ-sound"
]

VerificationTest[
	property[randPmExpr[2] &, If[badQ[Quiet[ref[#1]]], "skip", PmEqualQ[#1, PmExpand[#1]]] &, 100, 116],
	0,
	TestID -> "prop-PmEqualQ-recognizes-expansion"
]
