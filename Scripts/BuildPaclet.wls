#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Builds build/PMFunctions-<version>.paclet from the files the paclet needs:
   PacletInfo.wl, Kernel/, LICENSE and README.md.

       wolframscript -file Scripts/BuildPaclet.wls

   CreatePacletArchive on the repository itself would also pack Tests/, Scripts/,
   Examples/ and any untracked files such as a .venv, so the files are copied to a
   temporary staging folder first. Install the result with
       PacletInstall["build/PMFunctions-<version>.paclet", ForceVersionInstall -> True] *)

$repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
$staging = CreateDirectory[];
$stage = CreateDirectory[FileNameJoin[{$staging, "PMFunctions"}]];

CopyFile[FileNameJoin[{$repoRoot, #}], FileNameJoin[{$stage, #}]] & /@ {"PacletInfo.wl", "LICENSE", "README.md"};
CopyDirectory[FileNameJoin[{$repoRoot, "Kernel"}], FileNameJoin[{$stage, "Kernel"}]];

$out = FileNameJoin[{$repoRoot, "build"}];
If[!DirectoryQ[$out], CreateDirectory[$out]];
$archive = CreatePacletArchive[$stage, $out];
DeleteDirectory[$staging, DeleteContents -> True];

If[StringQ[$archive] && FileExistsQ[$archive],
	Print["Built ", $archive]; Exit[0],
	Print["Build failed: ", $archive]; Exit[1]
]
