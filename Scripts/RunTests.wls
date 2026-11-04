#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Runs every Tests/*.wlt file and exits with status 1 if any test fails.

   Wolfram Engine / Mathematica:   wolframscript -file Scripts/RunTests.wls
   Mathics3 (free, no license):    mathics -q -f Scripts/RunTests.wls

   The .wlt files are ordinary VerificationTest files, so you can also run them in a
   notebook with TestReport["Tests/Branches.wlt"]. This script uses its own small runner
   instead of TestReport so that it behaves the same on both kernels.

   Tests whose TestID starts with "WolframOnly:" are skipped on Mathics3, whose
   Simplify/FullSimplify are weaker than Mathematica's. *)


$repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
$Path = Prepend[$Path, FileNameJoin[{$repoRoot, "Kernel"}]];
Needs["PMFunctions`"];

$isMathics = StringContainsQ[$Version, "Mathics"];
$passed = 0;
$skipped = 0;
$failures = {};
$currentFile = "";

SetAttributes[runTest, HoldAllComplete];

runTest[input_, expected_, rest___] := Module[
	{opts, expectsMessages, sameTest, id, actual, exp, hadMessages = False},

	opts = Cases[{rest}, _Rule | _RuleDelayed];
	expectsMessages = Length[{rest}] > Length[opts];
	sameTest = Replace[SameTest, Append[opts, SameTest -> SameQ]];
	id = Replace[TestID, Append[opts, TestID -> ToString[HoldForm[input], InputForm]]];

	If[$isMathics && StringQ[id] && StringMatchQ[id, "WolframOnly:*"],
		$skipped++;
		Return[Null]
	];

	(* Mathics3's Check reports a message after any earlier Simplify call in the same
	   evaluation, so on Mathics3 unexpected messages are printed but not counted. *)
	actual = Which[
		expectsMessages, Quiet[input],
		$isMathics, input,
		True, Check[input, hadMessages = True; $Failed]
	];
	exp = expected;

	If[!hadMessages && TrueQ[sameTest[actual, exp]],
		$passed++,
		AppendTo[$failures, {$currentFile, id}];
		Print["  FAIL ", id];
		Print["    expected: ", ToString[exp, InputForm]];
		Print["    actual:   ", ToString[actual, InputForm]];
		If[hadMessages, Print["    (unexpected messages were generated)"]]
	]
]


$testFiles = FileNames["*.wlt", FileNameJoin[{$repoRoot, "Tests"}]];

Print["PMFunctions test run on ", If[$isMathics, "Mathics3", "Wolfram Language " <> ToString[$VersionNumber]]];
Print[""];

Do[
	$currentFile = FileNameTake[file];
	Module[{before = $passed, failedBefore = Length[$failures]},
		Block[{VerificationTest = runTest}, Get[file]];
		Print[$currentFile, ": ", $passed - before, " passed, ", Length[$failures] - failedBefore, " failed"]
	],
	{file, $testFiles}
];

Print[""];
Print["Total: ", $passed, " passed, ", Length[$failures], " failed, ", $skipped, " skipped"];

Exit[If[$failures === {} && $passed > 0, 0, 1]]
