(* ::Package:: *)

BeginPackage["K41`"];

Begin["`Private`"];

$ProjectBaseDirectory = FileNameJoin[{$UserBaseDirectory,"Applications","TR-Knot-Invariants"}]

<<Symbolic`

Do[
Get[FileNameJoin[{$ProjectBaseDirectory, "K41", data}]],
{data,{"A.wl", "F01.wl", "F02.wl", "W02.wl", "Weierstrass.wl", "dXi.wl"}}
];

End[];

EndPackage[];

$ContextPath = DeleteCases[$ContextPath, "K41`"];