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


LoadData[{"A", "F01", "F02", "W02"}]


(* ::Section:: *)
(*Documentation*)


?K41`*


?F02


(* ::Section:: *)
(*Tests*)


x\[Zeta][a_, \[Zeta]_] :=
    xP[a] Exp[2 \[Zeta] ^ r[a] / r[a]];

y\[Zeta][a_, \[Zeta]_] :=
    yP[a] Exp[Sum[F01[a, l] \[Zeta] ^ (l - r[a]), {l, s[a], 10}]];


(* ::Text:: *)
(*Verify F01[a,l]:*)


A[x\[Zeta][1, \[Zeta]], y\[Zeta][1, \[Zeta]]] + O[\[Zeta]] ^ 10 // Normal


(* ::Text:: *)
(*Verify F02[a,l]:*)


Series[Sum[F02[1, 3, l1, l2] \[Zeta]1 ^ (l1 - 1) \[Zeta]2 ^ (l2 - 1), {l1, 1, 3},
     {l2, 1, 3}] - W02[x\[Zeta][1, \[Zeta]1], y\[Zeta][1, \[Zeta]1], x\[Zeta][3, \[Zeta]2], y\[Zeta][3, \[Zeta]2]] D[x\[Zeta][1,
     \[Zeta]1], \[Zeta]1] D[x\[Zeta][3, \[Zeta]2], \[Zeta]2], {\[Zeta]1, 0, 2}, {\[Zeta]2, 0, 2}] //
Quiet //
Normal
