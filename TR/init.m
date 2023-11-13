(* ::Package:: *)

BeginPackage["TR`"];

TRInvariants;

Begin["`Private`"];

$ProjectBaseDirectory = FileNameJoin[{$UserBaseDirectory,"Applications","TR-Knot-Invariants"}]

<<Symbolic`

TRInvariants[{{2,3},{2,3},{2,3},{2,3}}]:=Module[{C},
    C["wSimplify"][expr_] := Module[{newExpr},
        newExpr = expr /.{$w[g_,n_]->$$w[g,n]};
        newExpr
    ];

    C["wRule"][g_,n_]:=$w[g,n]->$$w[g,n];


    $$w[0,3] = -1 $dXi[1, 1][1] $dXi[1, 1][2] $dXi[1, 1][3]/$F01[1, 3] - 
    $dXi[2, 1][1] $dXi[2, 1][2] $dXi[2, 1][3]/$F01[2, 3] - 
    $dXi[3, 1][1] $dXi[3, 1][2] $dXi[3, 1][3]/$F01[3, 3] - 
    $dXi[4, 1][1] $dXi[4, 1][2] $dXi[4, 1][3]/$F01[4, 3];

    $$w[1,1] = -1/2 $F02[1, 1, 1, 1] $dXi[1, 1][1]/$F01[1, 3] - 
   1/2 $F02[2, 2, 1, 1] $dXi[2, 1][1]/$F01[2, 3] - 
   1/2 $F02[3, 3, 1, 1] $dXi[3, 1][1]/$F01[3, 3] - 
   1/2 $F02[4, 4, 1, 1] $dXi[4, 1][1]/$F01[4, 3] - 1/8 $dXi[1, 3][1]/$F01[1, 3] + 
   1/8 $F01[1, 5] $dXi[1, 1][1]/$F01[1, 3]^2 - 1/8 $dXi[2, 3][1]/$F01[2, 3] + 
   1/8 $F01[2, 5] $dXi[2, 1][1]/$F01[2, 3]^2 - 1/8 $dXi[3, 3][1]/$F01[3, 3] + 
   1/8 $F01[3, 5] $dXi[3, 1][1]/$F01[3, 3]^2 - 1/8 $dXi[4, 3][1]/$F01[4, 3] + 
   1/8 $F01[4, 5] $dXi[4, 1][1]/$F01[4, 3]^2;

    C
]

End[];

EndPackage[];
