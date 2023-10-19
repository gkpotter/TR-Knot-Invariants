(* ::Package:: *)

BeginPackage["K41`Weierstrass`"];


P::usage = "\!\(\*RowBox[{\"P\", \"[\", \"z\",\"[\", RowBox[{StyleBox[\"x\", \"TI\"], \",\", \
StyleBox[\"y\", \"TI\"]}], \"]\",\"]\"}]\) is the Weierstrass elliptic function in terms of \!\(x\) and \!\(y\) satisfying \!\(\*RowBox[{RowBox[{\"A\", \"[\", RowBox[{\"x\", \",\", \"y\"}], \"]\"}], \"=\", \"0\"}]\).
\!\(\*RowBox[{\"P'\", \"[\", \"z\",\"[\", RowBox[{StyleBox[\"x\", \"TI\"], \",\", StyleBox[\"y\", \"TI\"]}], \"]\",\"]\"}]\) is \!\(\*RowBox[{SubscriptBox[\"\[PartialD]\", RowBox[{\"z\"}]], \"P\"}]\) in terms of \!\(x\) and \!\(y\).";

g2::usage = "\!\(g2\) is the coefficient \!\(\*SubscriptBox[\"g\", \"2\"]\) of the Weierstrass model \!\(\*RowBox[{SuperscriptBox[\"y\", \"2\"], \"=\", RowBox[{RowBox[{\"4\", \" \", SuperscriptBox[\"x\", \"3\"]}], \"-\", \
RowBox[{SubscriptBox[\"g\", \"2\"], \" \", \"x\"}], \"-\", SubscriptBox[\"g\", \"3\"]}]}]\).";
g3::usage = "\!\(g3\) is the coefficient \!\(\*SubscriptBox[\"g\", \"3\"]\) of the Weierstrass model \!\(\*RowBox[{SuperscriptBox[\"y\", \"2\"], \"=\", RowBox[{RowBox[{\"4\", \" \", SuperscriptBox[\"x\", \"3\"]}], \"-\", \
RowBox[{SubscriptBox[\"g\", \"2\"], \" \", \"x\"}], \"-\", SubscriptBox[\"g\", \"3\"]}]}]\).";
z::usage = "\!\(z\) is the elliptic coordinate.";
Ell::usage = "";
zBP::usage = "";
o::usage = "";
ySeries::usage = "";
dzdx::usage = "";


Print["Note: The base point for the Abel-Jacobi map has been chosen to be \!\(\*RowBox[{SubscriptBox[\"o\", \"2\"], \"=\", RowBox[{\"(\", \
RowBox[{\"0\", \",\", RowBox[{\"\[ImaginaryI]\", \" \", \
SqrtBox[\"3\"]}]}], \")\"}]}]\) in \!\(\*RowBox[{SuperscriptBox[\"\[Pi]\", RowBox[{\"-\", \"1\"}]], \"(\", \
RowBox[{\"(\", RowBox[{\"0\", \",\", RowBox[{\"\[ImaginaryI]\", \" \", \
SqrtBox[\"3\"]}]}], \")\"}], \")\"}]\)."]


Begin["`Private`"];
Needs["K41`W02`"]

P[z[x1_,y1_] - z[x2_,y2_]]:=Evaluate[Simplify[W02[x1,y1,x2,y2]/(dzdx[x1,y1]dzdx[x2,y2])]]

{g2, g3} = {1/12, -161 / 216};

Ell[X_, Y_] :=
    Y^2 - (4 X^3 - g2 X - g3)

P[z[x_, y_]] :=
    (1 + 6 I Sqrt[3] + x (16 - 6 I Sqrt[3] - I x (-x (16 I - 6 Sqrt[3
        ] + (I + 6 Sqrt[3]) x) + 12 Sqrt[3] (1 + y)))) / (12 (-1 + x) ^ 3 (1 
        + x))

P'[z[x_, y_]] :=
    (3 + I Sqrt[3] + (-3 - 5 I Sqrt[3]) x + (-3 + 5 I Sqrt[3]) x^3 + 
        (3 - I Sqrt[3]) x^4 - 6 x^2 (1 + y)) / (2 (-1 + x) ^ 4)

dzdx[x_, y_] :=
    (1 - x^2) / (-1 + x + x^3 - x^4 + 2 x^2 (1 + y))

P[zBP[i_] - zBP[j_]] :=
    Indexed[{{Infinity, -(7/12), 1/24 (7 - 3 I Sqrt[15]), 1/24 (7 + 3 I Sqrt[
        15])}, {-(7/12), Infinity, 1/24 (7 + 3 I Sqrt[15]), 1/24 (7 - 3 I Sqrt[15])},
         {1/24 (7 - 3 I Sqrt[15]), 1/24 (7 + 3 I Sqrt[15]), Infinity, -(7/12)}, {1/24
         (7 + 3 I Sqrt[15]), 1/24 (7 - 3 I Sqrt[15]), -(7/12), Infinity}}, {i, j}]

P'[zBP[i_] - zBP[j_]] :=
    0

o[1] = {0, I Sqrt[3]};

o[2] = {0, -I Sqrt[3]};

ySeries[x_, o[1]] :=
    -1 + I * Sqrt[3] * (-1 + x) + (1/2) * (3 - I * Sqrt[3]) * (-1 + x
        ) ^ 2 + (1/6) * I * (9 * I + Sqrt[3]) * (-1 + x) ^ 3 + 2 * (-1 + x) ^
         4 + (-(5/2) - (7 * I) / (6 * Sqrt[3])) * (-1 + x) ^ 5 + (3 + (8 * I)
         / (3 * Sqrt[3])) * (-1 + x) ^ 6 + (-(7/2) - (85 * I) / (18 * Sqrt[3]
        )) * (-1 + x) ^ 7 + (4 + (67 * I) / (9 * Sqrt[3])) * (-1 + x) ^ 8 + (
        -(9/2) - (599 * I) / (54 * Sqrt[3])) * (-1 + x) ^ 9 + (5 + (16 * I) /
         Sqrt[3]) * (-1 + x) ^ 10 + (-(11/2) - (3673 * I) / (162 * Sqrt[3])) * (
        -1 + x) ^ 11 + (6 + (2579 * I) / (81 * Sqrt[3])) * (-1 + x) ^ 12 + (-
        (13/2) - (2407 * I) / (54 * Sqrt[3])) * (-1 + x) ^ 13 + (7 + (5059 * 
        I) / (81 * Sqrt[3])) * (-1 + x) ^ 14 + (-(15/2) - (42685 * I) / (486 
        * Sqrt[3])) * (-1 + x) ^ 15

ySeries[x_, o[2]] :=
    -1 - I * Sqrt[3] * (x - 1) + (1/2) * (3 + I * Sqrt[3]) * (x - 1) ^
         2 - (1/6) * I * (-9 * I + Sqrt[3]) * (x - 1) ^ 3 + 2 * (x - 1) ^ 4 +
         (-(5/2) + (7 * I) / (6 * Sqrt[3])) * (x - 1) ^ 5 + (3 - (8 * I) / (3
         * Sqrt[3])) * (x - 1) ^ 6 + (-(7/2) + (85 * I) / (18 * Sqrt[3])) * (
        x - 1) ^ 7 + (4 - (67 * I) / (9 * Sqrt[3])) * (x - 1) ^ 8 + (-(9/2) +
         (599 * I) / (54 * Sqrt[3])) * (x - 1) ^ 9 + (5 - (16 * I) / Sqrt[3]) * (
        x - 1) ^ 10 + (-(11/2) + (3673 * I) / (162 * Sqrt[3])) * (x - 1) ^ 11 +
         (6 - (2579 * I) / (81 * Sqrt[3])) * (x - 1) ^ 12 + (-(13/2) + (2407 
        * I) / (54 * Sqrt[3])) * (x - 1) ^ 13 + (7 - (5059 * I) / (81 * Sqrt[
        3])) * (x - 1) ^ 14 + (-(15/2) + (42685 * I) / (486 * Sqrt[3])) * (x 
        - 1) ^ 15

End[];


EndPackage[];
