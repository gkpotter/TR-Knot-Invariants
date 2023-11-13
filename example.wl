(* ::Package:: *)

(* ::Title:: *)
(*Example*)


(* ::Section:: *)
(*Loading Data*)


$ProjectBaseDirectory = FileNameJoin[{$UserBaseDirectory,"Applications","TR-Knot-Invariants"}];


If[
!MemberQ[$Path, $ProjectBaseDirectory],
AppendTo[$Path, $ProjectBaseDirectory]
];


<<Symbolic`
<<K41`
<<L2R`


(* ::Section:: *)
(*Documentation*)


?K41`*


?L2R`*


(* ::Section:: *)
(*Exact Calculations*)


Begin["K41`"];

x\[Zeta][a_] :=
    xP[a] Exp[2 \[Zeta] ^ r[a] / r[a]];

y\[Zeta][a_] :=
    yP[a] Exp[Sum[F01[a, l] \[Zeta] ^ (l - r[a]), {l, s[a], F01MaxDegree}]]
    
dzdx[x\[Zeta][1],y\[Zeta][1]]D[x\[Zeta][1],\[Zeta]]+O[\[Zeta]]^15//FullSimplify

dzd\[Zeta]Table = Table[
CoefficientList[dzdx[x\[Zeta][a],y\[Zeta][a]]D[x\[Zeta][a],\[Zeta]]+O[\[Zeta]]^15//FullSimplify,\[Zeta]]
,{a,1,4}];

dzd\[Zeta]Table-Table[dzd\[Zeta][a,l]/(l-1)!,{a,1,4},{l,1,15}]

End[];


(* ::Section:: *)
(*Symbolic Calculations*)


(* ::Text:: *)
(*Every variable of the form $var is a purely symbolic variable. Rules for $var can be generated using varRule and expressions involving $var can be simplified using varSimplify.*)


<<TR`


TRInvariants[{{2,3},{2,3},{2,3},{2,3}}]["wRule"][1,1]//L2R`dXiSimplify


(* ::Text:: *)
(*Calculate w[0,3] as a function of x and y.*)


Begin["L2R`"];
Terms = (TRInvariants[{{2,3},{2,3},{2,3},{2,3}}]["wRule"][1,1]//dXiSimplify)/.{
		$dzd\[Zeta]->dzd\[Zeta],
		$F01->F01}
End[];
