(* ::Package:: *)

(* Shared helpers for Tests/*.wlt. Each test file loads this first with
       Get[FileNameJoin[{DirectoryName[$TestFileName], "TestUtils.wl"}]]
   It always loads the package from this checkout's Kernel folder, never through paclet
   resolution: PacletDirectoryLoad + Needs would silently pick an installed PMFunctions
   with a higher version, and the tests would check the wrong code. *)

Get[FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "Kernel", "PMFunctions.wl"}]];

BeginPackage["PMTestUtils`", {"PMFunctions`"}];

pmSameQ::usage = "pmSameQ[x, y] is True if x and y are both \[PlusMinus] expressions with the same number of arguments and each argument agrees after Simplify, or if neither is a \[PlusMinus] expression and they agree after Simplify. Use it where the exact form Simplify returns doesn't matter.";

Begin["`Private`"];

pmSameQ[x_, y_] := Which[
	Head[x] === PlusMinus && Head[y] === PlusMinus && Length[x] === Length[y],
		And @@ MapThread[TrueQ[Simplify[#1 - #2] == 0] &, {List @@ x, List @@ y}],
	Head[x] =!= PlusMinus && Head[y] =!= PlusMinus,
		TrueQ[Simplify[x - y] == 0],
	True, False
]

End[];
EndPackage[];
