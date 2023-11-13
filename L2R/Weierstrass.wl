(* ::Package:: *)

BeginPackage["L2R`"];


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



Begin["`Private`"];

P[z[x1_,y1_] - z[x2_,y2_]]:=Evaluate[Simplify[W02[x1,y1,x2,y2]/(dzdx[x1,y1]dzdx[x2,y2])]]

{g2, g3} = {25/12, -(253/216)};

Ell[X_, Y_] :=
    Y^2 - (4 X^3 - g2 X - g3)

P[z[x_, y_]] :=
    1/(12 (-1 + x)^3) (5 + 6 I Sqrt[7] + 27 x - 12 I Sqrt[7] x - 27 x^2 - 
   12 I Sqrt[7] x^2 - 5 x^3 + 6 I Sqrt[7] x^3 - 12 I Sqrt[7] x^2 y)

P'[z[x_, y_]] :=
    (1/(2 (-1 + x)^5))(-7 - I Sqrt[7] + x^2 (21 - 13 I Sqrt[7] + 14 y) + 
  2 x (7 + 5 I Sqrt[7] + 2 I Sqrt[7] y) + 
  x^5 (7 - I Sqrt[7] + 8 I Sqrt[7] y) - 
  I x^3 (-21 I + 17 Sqrt[7] + 24 Sqrt[7] y + 4 Sqrt[7] y^2) - 
  2 I x^4 (-7 I - 5 Sqrt[7] + (-7 I + 6 Sqrt[7]) y + 4 Sqrt[7] y^2))

dzdx[x_, y_] :=
    (x - 1)/(-2 y x^2 + x^3 - 2 x^2 - 2 x + 1)

P[zBP[i_] - zBP[j_]] := Null;

P'[zBP[i_] - zBP[j_]] := Null;

o[1] = Null;

o[2] = Null;

ySeries[x_, o[1]] := Null;

ySeries[x_, o[2]] := Null;

End[];


EndPackage[];
