(* ::Package:: *)

BeginPackage["K41`"];


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
    x^2 + (-1 + x + 2 x^2 + x^3 - x^4) y + x^2 y^2

dydx[x_, y_] :=
    (2 x + y + 4 x y + 3 x^2 y - 4 x^3 y + 2 x y^2) / (1 - x - 2 x^2 
        - x^3 + x^4 - 2 x^2 y)

xP[a_] :=
    Indexed[xPValues, a]

xPValues = {-(1/2) - (I Sqrt[3]) / 2, -(1/2) + (I Sqrt[3]) / 2, 3/2 -
     Sqrt[5] / 2, 3/2 + Sqrt[5] / 2};

yP[a_] :=
    Indexed[yPValues, a]

yPValues = {-1, -1, 1, 1};

R[a_] :=
    Indexed[RamificationProfile, a];

r[a_] :=
    Indexed[RamificationProfile, {a, 1}];

s[a_] :=
    Indexed[RamificationProfile, {a, 2}]

RamificationProfile = {{2, 3}, {2, 3}, {2, 3}, {2, 3}};

End[];


EndPackage[];
