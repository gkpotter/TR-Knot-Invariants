(* ::Package:: *)

BeginPackage["K41`dXi`"];


dXi::usage = "";
dzd\[Zeta]::usage = "";

dXiRule::usage = "Symbolic rules for dXi."
zP;
dXiSimplify;
Begin["`Private`"];

Needs["K41`Weierstrass`"];
Needs["Symbolic`"]

$$dXi[a_,k_][i_] := Hold[
        Simplify[1 / k!D[$P[z[i] - z[\[Zeta]]] z'[\[Zeta]], {\[Zeta], k - 1}] /. {
            \[Zeta] -> 0}/.{
            Derivative[l_][z][0]->$dzd\[Zeta][a,l],
            z[0]->zP[a]
        }]
    ]

dXiRule[a_,l_] := ReleaseHold[$dXi[a,l][i_] -> $$dXi[a,l][i]];
dXiSimplify[expr_] := Module[{newExpr},
                        newExpr = expr/.{$dXi[a_,l_][i_] -> $$dXi[a,l][i]};
                        ReleaseHold[newExpr]
                    ]



dzd\[Zeta][a_, l_] :=
    Indexed[dzd\[Zeta]List, {a, l}] (l - 1)!

dzd\[Zeta]List = {{Root[1 + 3 #^4& , 2, 0], 0, (-1)^(1/4)/(8 3^(3/4)), 
  0, -((83 (-1)^(3/4))/(384 3^(1/4))), 
  0, -((7 (-1)^(1/4))/(1024 3^(3/4))), 0, (42511 (-1)^(3/4))/(
  1474560 3^(1/4)), 0, -((92581 (-1)^(1/4))/(11796480 3^(3/4))), 
  0, -((396671 (-1)^(3/4))/(146800640 3^(1/4))), 0, (
  20042933 (-1)^(1/4))/(6341787648 3^(3/4)), 0, (
  701878789 (-1)^(3/4))/(3382286745600 3^(1/4))}, {-(-(1/3))^(1/4), 
  0, -(1/8) (-(1/3))^(3/4), 0, 83/384 (-(1/3))^(1/4), 0, (
  7 (-(1/3))^(3/4))/1024, 0, -((42511 (-(1/3))^(1/4))/1474560), 0, (
  92581 (-(1/3))^(3/4))/11796480, 0, (396671 (-(1/3))^(1/4))/
  146800640, 0, -((20042933 (-(1/3))^(3/4))/6341787648), 
  0, -((701878789 (-(1/3))^(1/4))/3382286745600)}, {I/5^(1/4), 0, (
  11 I)/(8 5^(3/4)), 0, (209 I)/(1920 5^(1/4)), 
  0, -((119 I)/(3072 5^(3/4))), 0, -((53 I)/(32768 5^(1/4))), 0, (
  1398637 I)/(58982400 5^(3/4)), 0, (763858579 I)/(
  99090432000 5^(1/4)), 0, (5118725531 I)/(792723456000 5^(3/4)), 0, (
  109087332257 I)/(152202903552000 5^(1/4))}, {1/5^(1/4), 
  0, -(11/(8 5^(3/4))), 0, 209/(1920 5^(1/4)), 0, 119/(3072 5^(3/4)), 
  0, -(53/(32768 5^(1/4))), 0, -(1398637/(58982400 5^(3/4))), 0, 
  763858579/(99090432000 5^(1/4)), 
  0, -(5118725531/(792723456000 5^(3/4))), 0, 109087332257/(
  152202903552000 5^(1/4))}};

End[];


EndPackage[];
