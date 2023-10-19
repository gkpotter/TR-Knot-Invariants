(* ::Package:: *)

(* ::Title:: *)
(*Example*)


(* ::Section:: *)
(*Loading Data*)


DataURL = NotebookDirectory[] <> "./K4_1/";

LoadData[name_] :=
    Import[DataURL <> name <> ".wl"]

LoadData[names_?ArrayQ] :=
    Do[LoadData[name], {name, names}]


LoadData[{"A", "F01", "F02", "W02", "Weierstrass", "w", "dXi"}]


(* ::Section:: *)
(*Documentation*)


?K41`Weierstrass`P


?F01


(* ::Section:: *)
(*Calculations*)


x\[Zeta][a_] :=
    xP[a] Exp[2 \[Zeta] ^ r[a] / r[a]];

y\[Zeta][a_] :=
    yP[a] Exp[Sum[F01[a, l] \[Zeta] ^ (l - r[a]), {l, s[a], 10}]];


dzdx[x\[Zeta][1],y\[Zeta][1]]+O[\[Zeta]]^5//FullSimplify


dXi[z][3]


dXi[z][3]/.{Derivative[k_][z][0]->dzd\[Zeta][1,k]}/.{(P^\[Prime]\[Prime])[z-z[0]]->6P[z-z[0]]^2-g2/2}//FullSimplify//Expand
