(* ::Package:: *)

BeginPackage["PMFunctions`"]
(*(c) 2024 by Daniel Hooker, CSU Fresno. 
Email: dshooker@mail.fresnostate.edu, dshooker13@gmail.com
A simple package for handling expressions which use \[PlusMinus].
Mathematica doesn't allow you to actually perform algebra on \[PlusMinus] for some reason, so I've been workin on adding it.*)


PmExpand::usage = "PmExpand[expr] will expand expr with \[PlusMinus] and return the result."
PmFromList::usage = "PmFromList[{a+b,a-b}] returns a\[PlusMinus]b"
PmToList::usage = "PmFromList[a\[PlusMinus]b] returns {a+b,a-b}"
PmBase::usage = "PmBase[a\[PlusMinus]b] returns a"
PmPm::usage = "PmBase[a\[PlusMinus]b] returns b"
PmParts::usage = "PmParts[a\[PlusMinus]b] and returns {a,b}"
PmCompare::usage = "PmCompare[a\[PlusMinus]b,c\[PlusMinus]d] retuns a list {a==c,b==d} and attempts to evaluate it"
PmArithmatic::usage "PmArithmatic[a\[PlusMinus]b, c\[PlusMinus]d, operation] performs  arithmatic according to operation. Times, Plus, Divide, Subtract, Etc. are all possible options*)
(*example:\[IndentingNewLine]PmArithmatic[a\[PlusMinus]b,c\[PlusMinus]d,Divide]\[IndentingNewLine]will calculate \!\(\*FractionBox[\(a \[PlusMinus] b\), \(c \[PlusMinus] d\)]\),\[IndentingNewLine]which it simplifies and returns as\[IndentingNewLine]\!\(\*FractionBox[\(a\\\ c - b\\\ d\), \(\*SuperscriptBox[\(c\), \(2\)] - \*SuperscriptBox[\(d\), \(2\)]\)]\)\[PlusMinus]\!\(\*FractionBox[\(b\\\ c - a\\\ d\), \(\*SuperscriptBox[\(c\), \(2\)] - \*SuperscriptBox[\(d\), \(2\)]\)]\)\[IndentingNewLine]note that this can and will run into issues when \!\(\*SuperscriptBox[\(c\), \(2\)]\)==\!\(\*SuperscriptBox[\(d\), \(2\)]\)\[IndentingNewLine]*)"
PmCommands::usage ="Performs a chain of commands on func, and returns the result. Easier to learn by examples than explaining.\[IndentingNewLine]PmCommands[a \!\(\*SuperscriptBox[\(b\), \(2\)]\)+c b\[PlusMinus](f b+g \!\(\*SuperscriptBox[\(b\), \(3\)]\)+\!\(\*SuperscriptBox[\(y\), \(2\)]\)),{Series,{b,0,1},{y,0,2}},{Normal}]\[IndentingNewLine]Does Series[func,{b,0,1},{y,0,2}], Then applies Normal[] to the result. Note that the {} around Normal is necessary\[IndentingNewLine]Each command you want to use should follow the format {command, series of aguments to pass into command}"

Begin["`Private`"]

PmExpand[expr_]:=Module[
	{var,var2,xx,yy},
	var=expr/.PlusMinus[xx_,yy_]->{xx+yy,xx-yy};
	var=Expand[var];
	If[var[[1]]-var[[2]]==0,var2=Simplify[(var[[1]]+var[[2]])/2],var2=PlusMinus[Simplify[(var[[1]]+var[[2]])/2],Simplify[(var[[1]]-var[[2]])/2]]];
	var2
]


(*plus minus functionality*)
(*Note, mixing noncommutative functions into here likely will not work. Make sure that whatever you are feeding into the following commands has been normal ordered and is now just numbers, parameters, variables, etc. that are all commutative*)

(*takes a list {a+b,a-b}, and returns a\[PlusMinus]b*)
(*{a-b,a+b} will return a\[PlusMinus]-b, it is not worth the effort to make this return as a\[MinusPlus]b*)
PmFromList[expr_]:=Switch[expr[[1]],expr[[2]],expr[[1]],-expr[[2]],\[PlusMinus]expr[[1]],_,Simplify[((expr[[1]]+expr[[2]])/2)]\[PlusMinus]Simplify[((expr[[1]]-expr[[2]])/2)]]

(*takes a\[PlusMinus]b, and returns a list {a+b,a-b}*)
(*also will take a\[MinusPlus]b and returns {a-b,a+b}*)
PmToList[expr_]:=Module[{xx,yy,var1,var2},(
var1=expr/.{PlusMinus[xx_,yy_]->xx+yy,MinusPlus[xx_,yy_]->xx-yy};
var2=expr/.{PlusMinus[xx_,yy_]->xx-yy,MinusPlus[xx_,yy_]->xx+yy};
{var1,var2}
)
]

(*takes in a\[PlusMinus]b and returns a*)
PmBase[func_]:=Module[{xx,yy},
(func/.{PlusMinus[xx_,yy_]->xx,MinusPlus[xx_,yy_]->xx})
]

(*takes in a\[PlusMinus]b and returns b*)
(*also takes in a\[MinusPlus]b and returns -b*)
PmPm[func_]:=Module[{var,xx,yy},
(var=func-PmBase[func];(*remove the base, leaving us with just \[PlusMinus] and \[MinusPlus]*)
var/.{PlusMinus[xx_,yy_]->xx+yy,MinusPlus[xx_,yy_]->xx-yy})](*make the replacements needed*)


(*takes in a\[PlusMinus]b and returns {a,b}.*)
(*also takes a\[MinusPlus]b and returns {a,-b}*)
PmParts[func_]:={PmBase[func],PmPm[func]}

(*takes a\[PlusMinus]b and c\[PlusMinus]d as arguments and returns {a==c,b==d}*)
PmCompare[func1_,func2_]:=Module[{var1,var2},
(var1=PmParts[func1];
var2=PmParts[func2];
{var1[[1]]==var2[[1]]//Simplify,var1[[2]]==var2[[2]]//Simplify}
)
]

(*takes a\[PlusMinus]b and c\[PlusMinus]d as arguments and performs arithmatic according to operation. Times, Plus, Divide, Subtract, Etc. are all possible options*)
(*example:
PmArithmatic[a\[PlusMinus]b,c\[PlusMinus]d,Divide]
will calculate (a\[PlusMinus]b)/(c\[PlusMinus]d),
which it simplifies and returns as
(a c-b d)/(c^2-d^2)\[PlusMinus](b c-a d)/(c^2-d^2)
note that this can and will run into issues when c^2==d^2
*)
PmArithmatic[func1_,func2_,operation_]:=Module[
{var1,var2},
var1=PmToList[func1];
var2=PmToList[func2];
{var1,var2}={Apply[operation,{var1[[1]],var2[[1]]}],Apply[operation,{var1[[2]],var2[[2]]}]};
Return[PmFromList[{var1,var2}]]
]

(*performs a chain of commands on func, and returns the result. Easier to learn by examples than explaining*)
(*PmCommands[a b^2+c b\[PlusMinus](f b+g b^3+y^2),{Series,{b,0,1},{y,0,2}},{Normal}]
Does Series[func,{b,0,1},{y,0,2}], Then applies Normal[] to the result. Note that the {} around Normal is necessary*)
(*Each command you want to use should follow the format {command, series of aguments to pass into command}*)
PmCommands[func_,commands___]:=Module[
{list,ii},
(
list=PmToList[func];
For[ii=1,ii<=Length[{commands}],ii++,
(list={Apply[{commands}[[ii]][[1]],Prepend[{commands}[[ii]][[2;;-1]],list[[1]]]],Apply[{commands}[[ii]][[1]],Prepend[{commands}[[ii]][[2;;-1]],list[[2]]]]};)
];
PmFromList[list]
)
]


End[]
EndPackage[]


