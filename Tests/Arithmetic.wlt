(* Tests for PmArithmetic, PmArithmatic (legacy name) and PmThread. *)

Get[FileNameJoin[{DirectoryName[$TestFileName], "TestUtils.wl"}]]

(* Two \[PlusMinus] expressions are "the same" if both are PlusMinus (or both are not)
   and their arguments agree after Simplify. This keeps tests independent of the exact
   form Simplify happens to return. *)


(* ::Section:: *)
(*PmArithmetic*)


VerificationTest[
	PmArithmetic[PlusMinus[a, b], PlusMinus[c, d], Plus],
	PlusMinus[a + c, b + d],
	SameTest -> pmSameQ,
	TestID -> "PmArithmetic-Plus"
]

VerificationTest[
	PmArithmetic[PlusMinus[a, b], PlusMinus[c, d], Subtract],
	PlusMinus[a - c, b - d],
	SameTest -> pmSameQ,
	TestID -> "PmArithmetic-Subtract"
]

VerificationTest[
	PmArithmetic[PlusMinus[a, b], PlusMinus[c, d], Times],
	PlusMinus[a c + b d, a d + b c],
	SameTest -> pmSameQ,
	TestID -> "PmArithmetic-Times"
]

VerificationTest[
	PmArithmetic[PlusMinus[a, b], PlusMinus[c, d], Divide],
	PlusMinus[(a c - b d)/(c^2 - d^2), (b c - a d)/(c^2 - d^2)],
	SameTest -> pmSameQ,
	TestID -> "PmArithmetic-Divide"
]

VerificationTest[
	PmArithmetic[PlusMinus[a, b], MinusPlus[c, d], Plus],
	PlusMinus[a + c, b - d],
	SameTest -> pmSameQ,
	TestID -> "PmArithmetic-with-minusplus"
]

VerificationTest[
	PmArithmetic[PlusMinus[a, b], c, Times],
	PlusMinus[a c, b c],
	SameTest -> pmSameQ,
	TestID -> "PmArithmetic-with-plain-expression"
]

VerificationTest[
	PmArithmetic[PlusMinus[a, b], PlusMinus[a, -b], Plus],
	2 a,
	TestID -> "PmArithmetic-pm-cancels"
]

VerificationTest[
	PmArithmetic[PlusMinus[3, 1], PlusMinus[5, 2], Times],
	PlusMinus[17, 11],
	TestID -> "PmArithmetic-numeric"
]


(* ::Section:: *)
(*Legacy spelling*)


VerificationTest[
	PmArithmatic[PlusMinus[a, b], PlusMinus[c, d], Times],
	PmArithmetic[PlusMinus[a, b], PlusMinus[c, d], Times],
	TestID -> "PmArithmatic-alias"
]


(* ::Section:: *)
(*PmThread*)


VerificationTest[
	PmThread[Plus, PlusMinus[a, b], PlusMinus[c, d], PlusMinus[e, f]],
	PlusMinus[a + c + e, b + d + f],
	SameTest -> pmSameQ,
	TestID -> "PmThread-three-arguments"
]

VerificationTest[
	PmThread[Power, PlusMinus[a, b], 2, "Simplifier" -> Identity],
	PlusMinus[((a + b)^2 + (a - b)^2)/2, ((a + b)^2 - (a - b)^2)/2],
	TestID -> "PmThread-power-with-option"
]

VerificationTest[
	PmThread[f, PlusMinus[a, b]],
	PlusMinus[(f[a + b] + f[a - b])/2, (f[a + b] - f[a - b])/2],
	SameTest -> pmSameQ,
	TestID -> "PmThread-unknown-function"
]


(* ::Section:: *)
(*Rule arguments are data, not options*)


(* A trailing rule used to be swallowed as an option, so ReplaceAll got only one argument. *)
VerificationTest[PmThread[ReplaceAll, PlusMinus[x, 3 x], x -> 2], PlusMinus[2, 6], TestID -> "PmThread-rule-argument"]
VerificationTest[PmThread[ReplaceAll, x^2 + y, {x -> PlusMinus[1, a]}], PlusMinus[1 + a^2 + y, 2 a], TestID -> "PmThread-rule-list-argument"]
VerificationTest[PmArithmetic[PlusMinus[a, x], x -> 2, ReplaceAll], PlusMinus[a, 2], TestID -> "PmArithmetic-rule-argument"]

VerificationTest[
	PmThread[ReplaceAll, PlusMinus[x, 3 x], x -> 2, "Simplifier" -> (# + z &)],
	PlusMinus[2 + z, 6 + z],
	TestID -> "PmThread-rule-argument-and-option"
]
