(* ::Package:: *)

BeginPackage["L2R`"];


A::usage = "\!\(\*RowBox[{\"A\", \"[\", RowBox[{StyleBox[\"x\", \"TI\"], \",\", \
StyleBox[\"y\", \"TI\"]}], \"]\"}]\) is the A-polynomial where \!\(\*RowBox[{\"x\", \"=\", TemplateBox[{\"m\", \"2\"}, \"Superscript\", SyntaxForm -> \
SuperscriptBox]}]\) and \!\(\*RowBox[{\"y\", \"=\", \"l\"}]\).";

dydx::usage = "";

xP::usage = "\!\(\*RowBox[{\"xP\", \"[\", StyleBox[\"a\", \"TI\"], \"]\"}]\) is the \!\(\"x\"\) value of the ramification point \!\(\*SubscriptBox[\"p\", \"a\"]\).";

yP::usage = "\!\(\*RowBox[{\"yP\", \"[\", StyleBox[\"a\", \"TI\"], \"]\"}]\) is the \!\(\"y\"\) value of the ramification point \!\(\*SubscriptBox[\"p\", \"a\"]\).";

R::usage = "\!\(\*RowBox[{\"R\", \"[\", StyleBox[\"a\", \"TI\"], \"]\"}]\) is the ramification type \!\(\*RowBox[{\"(\", RowBox[{SubscriptBox[\"r\", \"a\"], \",\", SubscriptBox[\"s\", \"a\"]}], \
\")\"}]\).";

RamificationProfile::usage = "\!\(\"RamificationProfile\"\) is the ramification profile \!\(\*RowBox[{\"(\",RowBox[{\"(\", RowBox[{SubscriptBox[\"r\", \"1\"], \",\", SubscriptBox[\"s\", \"1\"]}], \
\")\"}], \",\", RowBox[{\"(\", RowBox[{SubscriptBox[\"r\", \"2\"], \",\", SubscriptBox[\"s\", \"2\"]}], \
\")\"}],\",\",\" ...\",\")\"}]\).";

r::usage = "\!\(\*RowBox[{\"r\", \"[\", StyleBox[\"a\", \"TI\"], \"]\"}]\) is the order of ramification.";

s::usage = "\!\(\*RowBox[{\"s\", \"[\", StyleBox[\"a\", \"TI\"], \"]\"}]\) is the \!\(\*SubscriptBox[\"s\", \"a\"]\) from the ramification type \!\(\*RowBox[{\"(\", RowBox[{SubscriptBox[\"r\", \"a\"], \",\", SubscriptBox[\"s\", \"a\"]}], \
\")\"}]\).";


Begin["`Private`"];

A[x_, y_] :=
    x + (-1 + 2 x + 2 x^2 - x^3) y + x^2 y^2

dydx[x_, y_] := (1 + 2 y + 4 x y - 3 x^2 y + 2 x y^2)/(1 - 2 x - 2 x^2 + x^3 - 
 2 x^2 y);
    

xPValues = {1/2 + Sqrt[2] - 1/2 Sqrt[5 + 4 Sqrt[2]], 
 1/2 (1 + 2 Sqrt[2] + Sqrt[5 + 4 Sqrt[2]]), 
 1/2 (1 - 2 Sqrt[2] - I Sqrt[-5 + 4 Sqrt[2]]), 
 1/2 (1 - 2 Sqrt[2] + I Sqrt[-5 + 4 Sqrt[2]])};

yPValues = {1/2 (1 + Sqrt[2] + Sqrt[-1 + 2 Sqrt[2]]), 
 1/2 (1 + Sqrt[2] - Sqrt[-1 + 2 Sqrt[2]]), 
 1/2 (1 - Sqrt[2] - I Sqrt[1 + 2 Sqrt[2]]), 
 1/2 (1 - Sqrt[2] + I Sqrt[1 + 2 Sqrt[2]])};

xP[a_] :=
    Indexed[xPValues, a]

yP[a_] :=
    Indexed[yPValues, a]

RamificationProfile = {{2, 3}, {2, 3}, {2, 3}, {2, 3}};

R[a_] :=
    Indexed[RamificationProfile, a];

r[a_] :=
    Indexed[RamificationProfile, {a, 1}];

s[a_] :=
    Indexed[RamificationProfile, {a, 2}]


End[];


EndPackage[];
