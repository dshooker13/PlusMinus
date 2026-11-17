(* ::Package:: *)

(* A tour of PMFunctions. Run it with
     wolframscript -file Examples/Examples.wl
   or open it in Mathematica and evaluate the cells one at a time. *)

(* $InputFileName is empty when cells are evaluated in a notebook; NotebookDirectory[]
   then gives the folder of this file. *)
PacletDirectoryLoad[ParentDirectory[If[$InputFileName =!= "", DirectoryName[$InputFileName], NotebookDirectory[]]]];
Needs["PMFunctions`"];

show[label_String, expr_] := Print[label, "\n    ", expr, "\n"];


(* ::Section:: *)
(*Splitting and recombining*)


show["Branches of (a \[PlusMinus] b)^2:", PmToList[PlusMinus[a, b]^2]];
show["Recombine {a + b, a - b}:", PmFromList[{a + b, a - b}]];
show["Base and \[PlusMinus] part of (a \[PlusMinus] b)^2:", PmParts[PlusMinus[a, b]^2, "Simplifier" -> Expand]];
show["Just the base, and just the \[PlusMinus] part:", {PmBase[PlusMinus[a, b]^2], PmPm[PlusMinus[a, b]^2]}];
show["Does x + (a \[PlusMinus] b) contain a sign?", PmQ[x + PlusMinus[a, b]]];
show["Flip the signs of a \[PlusMinus] b (swaps the branches):", PmFlip[PlusMinus[a, b]]];


(* ::Section:: *)
(*Algebra*)


show["Expand (a \[PlusMinus] b)^3:", PmExpand[PlusMinus[a, b]^3]];
show["(x \[PlusMinus] y)(x \[MinusPlus] y):", PmExpand[PlusMinus[x, y] MinusPlus[x, y]]];
show["Divide (a \[PlusMinus] b) by (c \[PlusMinus] d):", PmArithmetic[PlusMinus[a, b], PlusMinus[c, d], Divide]];
show["Add three \[PlusMinus] quantities:", PmThread[Plus, PlusMinus[a, b], PlusMinus[c, d], PlusMinus[e, f]]];
show["Simplify, with an assumption:", PmSimplify[PlusMinus[Sqrt[x^2], (a^2 - b^2)/(a - b)], x > 0]];
show["FullSimplify reduces Gamma[x + 1]/Gamma[x]:", PmFullSimplify[PlusMinus[Gamma[x + 1]/Gamma[x], 1]]];


(* ::Section:: *)
(*Calculus*)


show["d/dx of x^2 \[PlusMinus] x^3:", PmMap[D[#, x] &, PlusMinus[x^2, x^3]]];
show["Series then Normal on each branch:",
	PmCommands[PlusMinus[a b^2 + c b, f b + g b^3 + y^2], {Series, {b, 0, 1}, {y, 0, 2}}, {Normal}]];


(* ::Section:: *)
(*Solving*)


show["The quadratic formula:", PmSolve[x^2 + b x + c == 0, x]];
show["x^2 - 2x - 1 == 0:", PmSolve[x^2 - 2 x - 1 == 0, x]];


(* ::Section:: *)
(*Comparing*)


show["Is a \[PlusMinus] b the same as a \[MinusPlus] (-b)?", PmEqualQ[PlusMinus[a, b], MinusPlus[a, -b]]];
show["Compare base and \[PlusMinus] part of (a \[PlusMinus] b)^2 with a^2 + b^2 \[PlusMinus] 2 a b:", PmCompare[PlusMinus[a, b]^2, PlusMinus[a^2 + b^2, 2 a b]]];


(* ::Section:: *)
(*When the branches can't be written as a \[PlusMinus] b*)


show["Equations are combined side by side:", PmSimplify[PlusMinus[a, b] == c]];
show["2/(1 \[PlusMinus] 1) has an infinite branch, so the branches are kept:", Quiet[PmArithmetic[2, PlusMinus[1, 1], Divide]]];
show["PmBranches works like any other expression with a sign:", PmToList[PmMap[1/# &, PmBranches[1, Infinity]]]];
