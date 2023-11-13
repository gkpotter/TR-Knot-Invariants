(* ::Package:: *)

BeginPackage["L2R`"];


F01::usage = "\!\(\*RowBox[{\"F01\", \"[\", RowBox[{StyleBox[\"a\", \"TI\", StripOnInput -> False], \",\", \
StyleBox[\"l\", \"TI\", StripOnInput -> False]}], \"]\"}]\) \
is the coefficient \!\(\*RowBox[{SubscriptBox[\"F\", \"\\\"0,1\\\"\"], \"[\", \
RowBox[{\"a\", \",\", \"l\"}], \"]\"}]\) from \!\(\*RowBox[{\"y\", \"=\",\"yP[a]\", RowBox[{\"Exp\", \"[\",UnderoverscriptBox[\"\[Sum]\", \
RowBox[{\"l\", \"=\", \"s\"}], \"\[Infinity]\"], \
RowBox[{RowBox[{SubscriptBox[\"F\", \"\\\"0,1\\\"\"], \"[\", \
RowBox[{\"a\", \",\", \"l\"}], \"]\"}], \" \", \
TemplateBox[{\"\[Zeta]\", \"a\", RowBox[{\"l\", \"-\", \"r\"}]}, \
\"Subsuperscript\", SyntaxForm -> SubsuperscriptBox]}],\"]\"}]}]\).";

F01MaxDegree;

Begin["`Private`"];

F01Coefficients = {{0, 0, I (26 + 32 Sqrt[2])^(1/4), -(1/
   2), -(1/24) I (-(89819/2) + 32104 Sqrt[2])^(1/4), 0, (
  I (26 + 32 Sqrt[2])^(1/4) (-67 + 96 Sqrt[2]))/3840, 0, (
  I (26 + 32 Sqrt[2])^(1/4) Sqrt[-61465157 + 44234656 Sqrt[2]])/(
  43008 Sqrt[14])}, {0, 0, (26 + 32 Sqrt[2])^(1/4), -(1/2), 
  1/24 (-(89819/2) + 32104 Sqrt[2])^(1/4), 
  0, ((26 + 32 Sqrt[2])^(1/4) (-67 + 96 Sqrt[2]))/3840, 
  0, -(((26 + 32 Sqrt[2])^(1/4) Sqrt[-61465157 + 44234656 Sqrt[2]])/(
   43008 Sqrt[14]))}, {0, 0, -I (26 - 32 Sqrt[2])^(1/4), -(1/2), 
  1/24 (-(89819/2) - 32104 Sqrt[2])^(1/4), 
  0, ((-1)^(3/4) (-26 + 32 Sqrt[2])^(1/4) (67 + 96 Sqrt[2]))/3840, 
  0, -((-(1510634458235435/2) - 534396528098728 Sqrt[2])^(1/4)/
   43008)}, {0, 0, (26 - 32 Sqrt[2])^(
  1/4), -(1/2), -(1/24) I (-(89819/2) - 32104 Sqrt[2])^(1/4), 
  0, -(((26 - 32 Sqrt[2])^(1/4) (67 + 96 Sqrt[2]))/3840), 0, (
  I (-(1510634458235435/2) - 534396528098728 Sqrt[2])^(1/4))/43008}};

F01MaxDegree = Min[Map[Length, F01Coefficients]];

Do[
    F01[a, l] = Indexed[F01Coefficients, {a, l}];,
    {a,1,4},
    {l,1,F01MaxDegree}
]

End[];


EndPackage[];
