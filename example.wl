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


?F02


(* ::Section:: *)
(*Exact Calculations*)


x\[Zeta][a_] :=
    xP[a] Exp[2 \[Zeta] ^ r[a] / r[a]];

y\[Zeta][a_] :=
    yP[a] Exp[Sum[F01[a, l] \[Zeta] ^ (l - r[a]), {l, s[a], F01MaxDegree}]+F01Tail[\[Zeta]]\[Zeta]^(F01MaxDegree-r[a]+1)]


(* ::Text:: *)
(*Expand dzdx as a series in \[Zeta] near the first ramification point P[1].*)


dzdx[x\[Zeta][1],y\[Zeta][1]]D[x\[Zeta][1],\[Zeta]]+O[\[Zeta]]^10//FullSimplify


(* ::Text:: *)
(*Verify dzd\[Zeta]  values. *)


dzd\[Zeta]Table = Table[
CoefficientList[dzdx[x\[Zeta][a],y\[Zeta][a]]D[x\[Zeta][a],\[Zeta]]+O[\[Zeta]]^17//FullSimplify,\[Zeta]]
,{a,1,4}]


dzd\[Zeta]Table-Table[dzd\[Zeta][a,l]/(l-1)!,{a,1,4},{l,1,17}]


(* ::Section:: *)
(*Symbolic Calculations*)


(* ::Text:: *)
(*Every variable of the form $var is a purely symbolic variable. Rules for $var can be generated using varRule and expressions involving $var can be simplified using varSimplify.*)


$w[0,3]


wRule[0,3]


$w[0,3]//wSimplify


$w[0,3]//wSimplify//dXiSimplify


(* ::Text:: *)
(*Calculate w[0,3] as a function of x and y.*)


Terms = List@@($w[0,3]//wSimplify//dXiSimplify)


wEvaluated[0,3]=Plus@@Map[
	FullSimplify[#/.{
		$dzd\[Zeta]->dzd\[Zeta],
		$F01->F01,
		$P[z[i_]-zP[a_]]->FullSimplify[P[z[x[i],y[i]]-z[xP[a],yP[a]]]]
	}]&,
	Terms
]
