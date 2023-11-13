(* ::Package:: *)

BeginPackage["K41`"];


W02::usage = "\!\(\*RowBox[{\"W02\", \"[\", RowBox[{SubscriptBox[StyleBox[\"x\", \
\"TI\"], \"1\"], \",\", SubscriptBox[StyleBox[\"y\", \"TI\"], \"1\"], \",\", \
SubscriptBox[StyleBox[\"x\", \"TI\"], \"2\"], \",\", SubscriptBox[StyleBox[\"y\", \"TI\"], \"2\"]}], \"]\"}]\) \
is the function \!\(\*RowBox[{SubscriptBox[\"W\", \"\\\"0,2\\\"\"], \"[\", RowBox[{SubscriptBox[\"x\", \"1\"], \
\",\", SubscriptBox[\"x\", \"2\"], \",\", SubscriptBox[\"y\", \"1\"], \",\", \
SubscriptBox[\"y\", \"2\"]}], \"]\"}]\) from the fundamental bidifferential \!\(\*RowBox[{SubscriptBox[\"\[Omega]\", \"\\\"0,2\\\"\"], \"=\", RowBox[{RowBox[{SubscriptBox[\"W\", \
\"\\\"0,2\\\"\"], \"[\", RowBox[{SubscriptBox[\"x\", \"1\"], \",\", SubscriptBox[\"x\", \
\"2\"], \",\", SubscriptBox[\"y\", \"1\"], \",\", SubscriptBox[\"y\", \"2\"]}], \"]\"}], \" \
\", SubscriptBox[\"dx\", \"1\"], \" \", SubscriptBox[\"dx\", \"2\"]}]}]\).";


Begin["`Private`"];

W02[x1_, y1_, x2_, y2_] :=
    (12 - 12 x1 - 19 x1^2 + 7 x1^4 - 12 x2 + 2 x1 x2 + 12 x1^2 x2 + 10
         x1^3 x2 - 19 x2^2 + 12 x1 x2^2 + 38 x1^2 x2^2 + 12 x1^3 x2^2 - 19 x1
        ^4 x2^2 + 10 x1 x2^3 + 12 x1^2 x2^3 + 2 x1^3 x2^3 - 12 x1^4 x2^3 + 7 
        x2^4 - 19 x1^2 x2^4 - 12 x1^3 x2^4 + 12 x1^4 x2^4 - 12 x1^2 y1 + 12 x1
        ^2 x2 y1 + 24 x1^2 x2^2 y1 + 12 x1^2 x2^3 y1 - 12 x1^2 x2^4 y1 - 12 x2
        ^2 y2 + 12 x1 x2^2 y2 + 24 x1^2 x2^2 y2 + 12 x1^3 x2^2 y2 - 12 x1^4 x2
        ^2 y2 + 24 x1^2 x2^2 y1 y2) / (12 (x1 - x2) ^ 2 (1 - x1 - 2 x1^2 - x1
        ^3 + x1^4 - 2 x1^2 y1) (1 - x2 - 2 x2^2 - x2^3 + x2^4 - 2 x2^2 y2));

End[];


EndPackage[];
