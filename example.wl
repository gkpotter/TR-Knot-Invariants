(* ::Package:: *)

(* ::Title:: *)
(*Example*)


(* ::Section:: *)
(*Loading Data*)


ProjectDirectory = If[$Notebooks,NotebookDirectory[],Directory[]];
Import[ProjectDirectory <> "Symbolic/Symbols.wl"];

LoadData[dir_, names_?ArrayQ] :=
    Do[Import[ProjectDirectory <> dir <> "/" <> name <> ".wl"], {name, names}]


LoadData["K4_1", {"A", "F01", "F02", "W02", "Weierstrass", "dXi"}]
Import[ProjectDirectory <> "TR/R={(2,3),(2,3),(2,3),(2,3)}.wl"]


(* ::Section:: *)
(*Documentation*)


?K41`Weierstrass`*


(* ::Section:: *)
(*Exact Calculations*)


x\[Zeta][a_] :=
    xP[a] Exp[2 \[Zeta] ^ r[a] / r[a]];

y\[Zeta][a_] :=
    yP[a] Exp[Sum[F01[a, l] \[Zeta] ^ (l - r[a]), {l, s[a], 10}]];


dzdx[x\[Zeta][1],y\[Zeta][1]]+O[\[Zeta]]^5//FullSimplify


?dXiRule


dXiRule[1,3]/.{
$dzd\[Zeta][a_,l_]->dzd\[Zeta][a,l]
}/.{
	($P'')[z-z[0]]->6$P[z-z[0]]^2-g2/2
}//FullSimplify//Expand


wRule[0,3]


X=4


?X


dXiRule


(* ::Section:: *)
(*Symbolic Calculations*)


(* ::Text:: *)
(*Every variable of the form $var is a purely symbolic variable. Rules for $var can be generated using varRule and expressions involving $var can be simplified using varSimplify.*)


$w[0,3]


wRule[0,3]


$w[0,3]//wSimplify


$w[0,3]//wSimplify//dXiSimplify
