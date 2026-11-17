(* Tests for PmSolve. *)

Get[FileNameJoin[{DirectoryName[$TestFileName], "TestUtils.wl"}]]


(* ::Section:: *)
(*PmSolve*)


VerificationTest[
	x /. First[PmSolve[x^2 + b x + c == 0, x]],
	PlusMinus[-b/2, Sqrt[b^2 - 4 c]/2],
	SameTest -> pmSameQ,
	TestID -> "PmSolve-quadratic-formula"
]

VerificationTest[
	PmSolve[x^2 == 2, x],
	{{x -> PlusMinus[Sqrt[2]]}},
	TestID -> "PmSolve-pure-square-root"
]

VerificationTest[
	PmSolve[x^2 - 2 x - 1 == 0, x],
	{{x -> PlusMinus[1, Sqrt[2]]}},
	TestID -> "PmSolve-numeric-coefficients"
]

VerificationTest[
	Sort[Transpose[PmToList /@ ({x, y} /. First[PmSolve[{x + y == 1, x^2 + y^2 == 1}, {x, y}]])]],
	{{0, 1}, {1, 0}},
	TestID -> "PmSolve-system-keeps-signs-correlated"
]

VerificationTest[
	PmPm[x /. First[PmSolve[{x + y == 1, x^2 + y^2 == 1}, {x, y}]]],
	1/2,
	TestID -> "PmSolve-system-positive-pm-on-first-variable"
]

VerificationTest[
	Length[PmSolve[x^3 == 1, x]],
	3,
	{PmSolve::nopair},
	TestID -> "PmSolve-three-roots-returned-unchanged"
]

VerificationTest[
	PmSolve[x + 1 == 0, x],
	{{x -> -1}},
	{PmSolve::nopair},
	TestID -> "PmSolve-one-root-returned-unchanged"
]

VerificationTest[
	PmSolve[x^2 + 1 == 0, x, Reals],
	{},
	{PmSolve::nopair},
	TestID -> "PmSolve-passes-domain-to-Solve"
]


(* ::Section:: *)
(*Equations that already contain a sign*)


(* The sign in the equation used to be tied to the sign merging Solve's roots, which kept
   2 of the 4 roots of x^2 == 4 \[PlusMinus] 1. Each solution now keeps the equation's sign. *)
VerificationTest[
	Sort[Simplify[PmToList[x /. PmSolve[x^2 == PlusMinus[4, 1], x]]]],
	Sort[{{-Sqrt[5], Sqrt[5]}, {-Sqrt[3], Sqrt[3]}}],
	TestID -> "PmSolve-sign-in-equation-keeps-all-roots"
]

VerificationTest[PmSolve[x + 1 == PlusMinus[2, 1], x], {{x -> PlusMinus[1, 1]}}, TestID -> "PmSolve-sign-in-linear-equation"]

(* Different numbers of solutions on the two branches can't be paired up. *)
VerificationTest[
	PmSolve[x^2 == PlusMinus[0, 1], x, Reals],
	PmBranches[{{x -> -1}, {x -> 1}}, {}],
	TestID -> "PmSolve-sign-in-equation-unequal-counts"
]


(* ::Section:: *)
(*Choosing the sign and reporting problems*)


(* Complex roots used to give Less::nord from the sign check. *)
VerificationTest[PmSolve[x^2 + 1 == 0, x], {{x -> PlusMinus[I]}}, TestID -> "PmSolve-complex-roots-no-message"]
VerificationTest[PmSolve[x^2 + 2 x + 5 == 0, x], {{x -> PlusMinus[-1, 2 I]}}, TestID -> "PmSolve-complex-roots-with-base"]

(* When Solve can't solve the equation it says why; PmSolve adds no message of its own. *)
VerificationTest[PmSolve[Sin[x] + x^x == Cos[x^2 + x], x], Solve[Sin[x] + x^x == Cos[x^2 + x], x], {Solve::nsmet}, TestID -> "PmSolve-unsolvable-no-extra-message"]

VerificationTest[
	PmSolve[x y == 0 && x + y == 0, {x, y}],
	{{x -> 0, y -> 0}},
	{PmSolve::nopair},
	TestID -> "PmSolve-one-solution-message"
]

(* A real root with a condition used to be lost: both solutions became Undefined. *)
VerificationTest[
	PmToList[x /. First[PmSolve[(x^2 == a && x > 0) || (x == Log[b] && x < 0), x, Reals]]] /. {a -> 4, b -> 1/2},
	{2, Log[1/2]},
	SameTest -> (Sort[Simplify[#1]] === Sort[Simplify[#2]] &),
	TestID -> "PmSolve-conditional-roots-kept"
]

(* Two solutions for different variables used to be reported as "2 solution(s), not 2". *)
VerificationTest[PmSolve[x y == 0, {x, y}], {{x -> 0}, {y -> 0}}, {Solve::svars, PmSolve::vars}, TestID -> "PmSolve-different-variables-message"]
