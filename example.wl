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


LoadData[{"A", "F01", "F02", "W02", "Weierstrass", "w"}]


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
