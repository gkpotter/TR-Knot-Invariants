(* ::Package:: *)

BeginPackage["L2R`"];

Begin["`Private`"];

$ProjectBaseDirectory = FileNameJoin[{$UserBaseDirectory,"Applications","TR-Knot-Invariants"}]

<<Symbolic`

Do[
Get[FileNameJoin[{$ProjectBaseDirectory, "L2R", data}]],
{data,{"A.wl", "F01.wl", "F02.wl", "W02.wl","Weierstrass.wl", "dXi.wl"}}
];

End[];

EndPackage[];

$ContextPath = DeleteCases[$ContextPath, "L2R`"];