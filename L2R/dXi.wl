(* ::Package:: *)

BeginPackage["L2R`"];


dXi::usage = "";
dzd\[Zeta]::usage = "";
dXiRule::usage = "Symbolic rules for dXi."
zP;
dXiSimplify;
Begin["`Private`"];

Needs["Symbolic`"]

$$dXi[a_,k_][i_] := Hold[
        Simplify[1 / k!D[$P[z[i] - z[\[Zeta]]] z'[\[Zeta]], {\[Zeta], k - 1}] /. {
            \[Zeta] -> 0}/.{
            Derivative[l_][z][0]->$dzd\[Zeta][a,l],
            z[0]->zP[a]
        }]
    ]$dz[i]

dXiRule[a_,l_] := ReleaseHold[$dXi[a,l][Global`i_] -> $$dXi[a,l][Global`i]];

dXiSimplify[expr_] := Module[{newExpr},
                        newExpr = expr/.{$dXi[a_,l_][i_] -> $$dXi[a,l][i]};
                        ReleaseHold[newExpr]
                    ]



dzd\[Zeta][a_, l_] :=
    Indexed[dzd\[Zeta]List, {a, l}] (l - 1)!

dzd\[Zeta]List = {{Root[-1 + 20 #^4 + 28 #^8& , 5, 0], 
  0, -((I (-221213 + 199124 Sqrt[2])^(1/4))/(8 14^(3/4))), 0, (
  5 I (26 + 32 Sqrt[2])^(1/4) (-2146 + 1371 Sqrt[2]))/75264, 0, (
  18322303 I (26 + 32 Sqrt[2])^(1/4))/(
  12288 Sqrt[75179380783 + 53186377574 Sqrt[2]])}, {Root[-1 + 20 #^4 + 
   28 #^8& , 1, 0], 0, (-221213 + 199124 Sqrt[2])^(1/4)/(8 14^(3/4)), 
  0, (5 (26 + 32 Sqrt[2])^(1/4) (-2146 + 1371 Sqrt[2]))/75264, 
  0, -((18322303 (26 + 32 Sqrt[2])^(1/4))/(
   12288 Sqrt[75179380783 + 53186377574 Sqrt[2]]))}, {Root[-1 + 
   20 #^4 + 28 #^8& , 7, 0], 0, (-221213 - 199124 Sqrt[2])^(1/4)/(
  8 14^(3/4)), 0, (
  5 (-1)^(3/4) (-26 + 32 Sqrt[2])^(1/4) (2146 + 1371 Sqrt[2]))/75264, 
  0, -(((26 - 32 Sqrt[2])^(1/4) Sqrt[
    75179380783 + 53186377574 Sqrt[2]])/(
   602112 Sqrt[7]))}, {(1/14 (-5 - 4 Sqrt[2]))^(1/4), 
  0, -((I (-221213 - 199124 Sqrt[2])^(1/4))/(8 14^(3/4))), 
  0, -((5 (26 - 32 Sqrt[2])^(1/4) (2146 + 1371 Sqrt[2]))/75264), 0, (
  I (26 - 32 Sqrt[2])^(1/4) Sqrt[75179380783 + 53186377574 Sqrt[2]])/(
  602112 Sqrt[7])}};

End[];


EndPackage[];
