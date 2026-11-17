#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Runs every Tests/*.wlt file with TestReport and exits with status 1 unless every test
   in every file ran and passed.

       wolframscript -file Scripts/RunTests.wls

   Safeguards, each added because an earlier runner reported success when it shouldn't have:
   - The package is loaded through its PacletInfo.wl (PacletDirectoryLoad), so the paclet
     metadata is exercised, not just the Kernel file.
   - TestReport checks expected messages and fails tests that emit unexpected ones.
   - Each file's VerificationTest expressions are counted before it runs. If TestReport runs a
     different number (a syntax error that truncates the file, an early Abort, a test that
     was silently skipped), the file counts as failed.
   - An Abort, Exit[] or Quit[], or a file that can't be parsed, counts as a failure instead
     of ending the run. *)


$repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
PacletDirectoryLoad[$repoRoot];
Needs["PMFunctions`"];

$testFiles = FileNames["*.wlt", FileNameJoin[{$repoRoot, "Tests"}]];
$passed = 0;
$failedFiles = {};

(* Number of VerificationTest calls written in a file, or $Failed if it doesn't parse. *)
expectedCount[file_] := Module[{held},
	(* The explicit "Package" format matters: auto-detection fails on some .wlt files. *)
	(* Parse in a throwaway context so the symbols it creates don't shadow the test files' own. *)
	held = Block[{$Context = "PMTestRunnerScratch`", $ContextPath = {"System`"}},
		Quiet[Import[file, {"Package", "HeldExpressions"}]]
	];
	If[!ListQ[held] || MemberQ[held, $Failed], Return[$Failed]];
	(* Every VerificationTest call, including several on one line separated by semicolons. *)
	Count[held, HoldPattern[VerificationTest[___]], Infinity]
]

failureLine[test_TestObject] := Module[{id = test["TestID"], outcome = test["Outcome"]},
	"  FAIL " <> ToString[id] <> " (" <> ToString[outcome] <> ")\n" <>
	"    expected: " <> ToString[test["ExpectedOutput"], InputForm] <> "\n" <>
	"    actual:   " <> ToString[test["ActualOutput"], InputForm] <>
	If[outcome === "MessagesFailure" || outcome === "MessageFailure",
		"\n    expected messages: " <> ToString[test["ExpectedMessages"], InputForm] <>
		"\n    actual messages:   " <> ToString[test["ActualMessages"], InputForm],
		""
	]
]

runFile[file_] := Module[{name = FileNameTake[file], expected, report, ran, ok, failures},
	expected = expectedCount[file];
	(* An Exit[] or Quit[] in a test file would end the run with status 0, so they are caught. *)
	report = Block[{Exit = Throw[{"Exit called", ##}, "pmexit"] &, Quit = Throw[{"Quit called", ##}, "pmexit"] &},
		Catch[CheckAbort[TestReport[file], $Aborted], "pmexit"]
	];

	If[Head[report] =!= TestReportObject,
		Print[name, ": could not run (", ToString[Short[report], InputForm], ")"];
		AppendTo[$failedFiles, name];
		Return[]
	];

	ran = report["TestsSucceededCount"] + report["TestsFailedCount"];
	failures = Values[Join @@ Values[report["TestsFailed"]]];
	Scan[Print[failureLine[#]] &, failures];

	ok = report["TestsFailedCount"] === 0 && IntegerQ[expected] && ran === expected;
	Print[name, ": ", report["TestsSucceededCount"], " passed, ", report["TestsFailedCount"], " failed",
		Which[
			expected === $Failed, " (file did not parse)",
			ran =!= expected, " (" <> ToString[expected] <> " written, " <> ToString[ran] <> " ran)",
			True, ""
		]
	];
	$passed += report["TestsSucceededCount"];
	If[!ok, AppendTo[$failedFiles, name]]
]


Print["PMFunctions test run on Wolfram Language ", $VersionNumber];
Print[""];
Scan[runFile, $testFiles];
Print[""];
Print["Total: ", $passed, " passed; ",
	If[$failedFiles === {}, "all files OK", "problems in " <> StringRiffle[$failedFiles, ", "]]];

Exit[If[$failedFiles === {} && $passed > 0, 0, 1]]
