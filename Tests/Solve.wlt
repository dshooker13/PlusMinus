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
