(* ::Package:: *)

(* Shared helpers for Tests/*.wlt. Each test file loads this first with
       Get[FileNameJoin[{DirectoryName[$TestFileName], "TestUtils.wl"}]]
   It loads PMFunctions from this checkout if no PMFunctions is loaded yet, so
   TestReport["Tests/Branches.wlt"] works in a fresh kernel. *)

If[!MemberQ[$Packages, "PMFunctions`"],
	PacletDirectoryLoad[ParentDirectory[DirectoryName[$InputFileName]]]
];
Needs["PMFunctions`"];

BeginPackage["PMTestUtils`", {"PMFunctions`"}];

pmSameQ::usage = "pmSameQ[x, y] is True if x and y are both \[PlusMinus] expressions with the same number of arguments and each argument agrees after Simplify, or if neither is a \[PlusMinus] expression and they agree after Simplify. Use it where the exact form Simplify returns doesn't matter.";

branchesSameQ::usage = "branchesSameQ[x, y] is True if x and y have the same two branches after Simplify, whatever form they are written in.";

Begin["`Private`"];

pmSameQ[x_, y_] := Which[
	Head[x] === PlusMinus && Head[y] === PlusMinus && Length[x] === Length[y],
		And @@ MapThread[TrueQ[Simplify[#1 - #2] == 0] &, {List @@ x, List @@ y}],
	Head[x] =!= PlusMinus && Head[y] =!= PlusMinus,
		TrueQ[Simplify[x - y] == 0],
	True, False
]

branchesSameQ[x_, y_] := Simplify[PmToList[x] - PmToList[y]] === {0, 0}

End[];
EndPackage[];
