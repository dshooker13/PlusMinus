(* ::Package:: *)

(* Compatibility loader. The package now lives in Kernel/PMFunctions.wl.
   This file exists so that notebooks which did Get["PlusMinus.wl"] keep working. *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "Kernel", "PMFunctions.wl"}]]
