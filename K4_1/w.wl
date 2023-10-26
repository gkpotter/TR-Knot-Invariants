(* ::Package:: *)

BeginPackage["K41`w`"];


w::usage = "";

$dXi;
$F01;


Begin["`Private`"];

w[0, 3] = -1 $dXi[1, 1] $dXi[1, 1] $dXi[1, 1]/$F01[1, 3] - 
   $dXi[2, 1] $dXi[2, 1] $dXi[2, 1]/$F01[2, 3] - 
   $dXi[3, 1] $dXi[3, 1] $dXi[3, 1]/$F01[3, 3] - 
   $dXi[4, 1] $dXi[4, 1] $dXi[4, 1]/$F01[4, 3];
w[1, 1] = -1/2 Phi[1, 1, 1, 1] $dXi[1, 1]/$F01[1, 3] - 
   1/2 Phi[2, 2, 1, 1] $dXi[2, 1]/$F01[2, 3] - 
   1/2 Phi[3, 3, 1, 1] $dXi[3, 1]/$F01[3, 3] - 
   1/2 Phi[4, 4, 1, 1] $dXi[4, 1]/$F01[4, 3] - 1/8 $dXi[1, 3]/$F01[1, 3] + 
   1/8 $F01[1, 5] $dXi[1, 1]/$F01[1, 3]^2 - 1/8 $dXi[2, 3]/$F01[2, 3] + 
   1/8 $F01[2, 5] $dXi[2, 1]/$F01[2, 3]^2 - 1/8 $dXi[3, 3]/$F01[3, 3] + 
   1/8 $F01[3, 5] $dXi[3, 1]/$F01[3, 3]^2 - 1/8 $dXi[4, 3]/$F01[4, 3] + 
   1/8 $F01[4, 5] $dXi[4, 1]/$F01[4, 3]^2;
w[0, 4] = 
  3 $dXi[1, 1] $dXi[1, 3] $dXi[1, 1] $dXi[1, 1]/$F01[1, 3]^2 + 
   3 $dXi[1, 1] $dXi[1, 1] $dXi[1, 1] $dXi[1, 3]/$F01[1, 3]^2 + 
   3 $dXi[1, 1] $dXi[1, 1] $dXi[1, 3] $dXi[1, 1]/$F01[1, 3]^2 + 
   3 $dXi[1, 3] $dXi[1, 1] $dXi[1, 1] $dXi[1, 1]/$F01[1, 3]^2 - 
   3 $F01[1, 5] $dXi[1, 1] $dXi[1, 1] $dXi[1, 1] $dXi[1, 1]/$F01[1, 3]^3 + 
   3 $dXi[2, 3] $dXi[2, 1] $dXi[2, 1] $dXi[2, 1]/$F01[2, 3]^2 + 
   3 $dXi[2, 1] $dXi[2, 3] $dXi[2, 1] $dXi[2, 1]/$F01[2, 3]^2 + 
   3 $dXi[2, 1] $dXi[2, 1] $dXi[2, 1] $dXi[2, 3]/$F01[2, 3]^2 + 
   3 $dXi[2, 1] $dXi[2, 1] $dXi[2, 3] $dXi[2, 1]/$F01[2, 3]^2 - 
   3 $F01[2, 5] $dXi[2, 1] $dXi[2, 1] $dXi[2, 1] $dXi[2, 1]/$F01[2, 3]^3 + 
   3 $dXi[3, 3] $dXi[3, 1] $dXi[3, 1] $dXi[3, 1]/$F01[3, 3]^2 + 
   3 $dXi[3, 1] $dXi[3, 1] $dXi[3, 1] $dXi[3, 3]/$F01[3, 3]^2 + 
   3 $dXi[3, 1] $dXi[3, 1] $dXi[3, 3] $dXi[3, 1]/$F01[3, 3]^2 + 
   3 $dXi[3, 1] $dXi[3, 3] $dXi[3, 1] $dXi[3, 1]/$F01[3, 3]^2 - 
   3 $F01[3, 5] $dXi[3, 1] $dXi[3, 1] $dXi[3, 1] $dXi[3, 1]/$F01[3, 3]^3 + 
   3 $dXi[4, 1] $dXi[4, 3] $dXi[4, 1] $dXi[4, 1]/$F01[4, 3]^2 + 
   3 $dXi[4, 1] $dXi[4, 1] $dXi[4, 1] $dXi[4, 3]/$F01[4, 3]^2 + 
   3 $dXi[4, 1] $dXi[4, 1] $dXi[4, 3] $dXi[4, 1]/$F01[4, 3]^2 + 
   3 $dXi[4, 3] $dXi[4, 1] $dXi[4, 1] $dXi[4, 1]/$F01[4, 3]^2 - 
   3 $F01[4, 5] $dXi[4, 1] $dXi[4, 1] $dXi[4, 1] $dXi[4, 1]/$F01[4, 3]^3 + 
   3 Phi[1, 1, 1, 1] $dXi[1, 1] $dXi[1, 1] $dXi[1, 1] $dXi[1, 1]/
     $F01[1, 3]^2 + 
   Phi[1, 2, 1, 1] $dXi[2, 1] $dXi[1, 1] $dXi[2, 1][
     3] $dXi[1, 1]/($F01[1, 3] $F01[2, 3]) + 
   Phi[1, 2, 1, 1] $dXi[2, 1] $dXi[2, 1] $dXi[1, 1][
     3] $dXi[1, 1]/($F01[1, 3] $F01[2, 3]) + 
   Phi[1, 2, 1, 1] $dXi[1, 1] $dXi[2, 1] $dXi[2, 1][
     3] $dXi[1, 1]/($F01[1, 3] $F01[2, 3]) + 
   Phi[1, 2, 1, 1] $dXi[2, 1] $dXi[1, 1] $dXi[1, 1][
     3] $dXi[2, 1]/($F01[1, 3] $F01[2, 3]) + 
   Phi[1, 2, 1, 1] $dXi[1, 1] $dXi[1, 1] $dXi[2, 1][
     3] $dXi[2, 1]/($F01[1, 3] $F01[2, 3]) + 
   Phi[1, 2, 1, 1] $dXi[1, 1] $dXi[2, 1] $dXi[1, 1][
     3] $dXi[2, 1]/($F01[1, 3] $F01[2, 3]) + 
   Phi[1, 3, 1, 1] $dXi[3, 1] $dXi[1, 1] $dXi[3, 1][
     3] $dXi[1, 1]/($F01[1, 3] $F01[3, 3]) + 
   Phi[1, 3, 1, 1] $dXi[1, 1] $dXi[3, 1] $dXi[1, 1][
     3] $dXi[3, 1]/($F01[1, 3] $F01[3, 3]) + 
   Phi[1, 3, 1, 1] $dXi[3, 1] $dXi[1, 1] $dXi[1, 1][
     3] $dXi[3, 1]/($F01[1, 3] $F01[3, 3]) + 
   Phi[1, 3, 1, 1] $dXi[1, 1] $dXi[1, 1] $dXi[3, 1][
     3] $dXi[3, 1]/($F01[1, 3] $F01[3, 3]) + 
   Phi[1, 3, 1, 1] $dXi[1, 1] $dXi[3, 1] $dXi[3, 1][
     3] $dXi[1, 1]/($F01[1, 3] $F01[3, 3]) + 
   Phi[1, 3, 1, 1] $dXi[3, 1] $dXi[3, 1] $dXi[1, 1][
     3] $dXi[1, 1]/($F01[1, 3] $F01[3, 3]) + 
   Phi[1, 4, 1, 1] $dXi[1, 1] $dXi[1, 1] $dXi[4, 1][
     3] $dXi[4, 1]/($F01[1, 3] $F01[4, 3]) + 
   Phi[1, 4, 1, 1] $dXi[4, 1] $dXi[1, 1] $dXi[1, 1][
     3] $dXi[4, 1]/($F01[1, 3] $F01[4, 3]) + 
   Phi[1, 4, 1, 1] $dXi[4, 1] $dXi[1, 1] $dXi[4, 1][
     3] $dXi[1, 1]/($F01[1, 3] $F01[4, 3]) + 
   Phi[1, 4, 1, 1] $dXi[1, 1] $dXi[4, 1] $dXi[1, 1][
     3] $dXi[4, 1]/($F01[1, 3] $F01[4, 3]) + 
   Phi[1, 4, 1, 1] $dXi[1, 1] $dXi[4, 1] $dXi[4, 1][
     3] $dXi[1, 1]/($F01[1, 3] $F01[4, 3]) + 
   Phi[1, 4, 1, 1] $dXi[4, 1] $dXi[4, 1] $dXi[1, 1][
     3] $dXi[1, 1]/($F01[1, 3] $F01[4, 3]) + 
   3 Phi[2, 2, 1, 1] $dXi[2, 1] $dXi[2, 1] $dXi[2, 1] $dXi[2, 1]/
     $F01[2, 3]^2 + 
   Phi[2, 3, 1, 1] $dXi[3, 1] $dXi[2, 1] $dXi[2, 1][
     3] $dXi[3, 1]/($F01[2, 3] $F01[3, 3]) + 
   Phi[2, 3, 1, 1] $dXi[3, 1] $dXi[3, 1] $dXi[2, 1][
     3] $dXi[2, 1]/($F01[2, 3] $F01[3, 3]) + 
   Phi[2, 3, 1, 1] $dXi[2, 1] $dXi[3, 1] $dXi[3, 1][
     3] $dXi[2, 1]/($F01[2, 3] $F01[3, 3]) + 
   Phi[2, 3, 1, 1] $dXi[2, 1] $dXi[3, 1] $dXi[2, 1][
     3] $dXi[3, 1]/($F01[2, 3] $F01[3, 3]) + 
   Phi[2, 3, 1, 1] $dXi[2, 1] $dXi[2, 1] $dXi[3, 1][
     3] $dXi[3, 1]/($F01[2, 3] $F01[3, 3]) + 
   Phi[2, 3, 1, 1] $dXi[3, 1] $dXi[2, 1] $dXi[3, 1][
     3] $dXi[2, 1]/($F01[2, 3] $F01[3, 3]) + 
   Phi[2, 4, 1, 1] $dXi[2, 1] $dXi[2, 1] $dXi[4, 1][
     3] $dXi[4, 1]/($F01[2, 3] $F01[4, 3]) + 
   Phi[2, 4, 1, 1] $dXi[4, 1] $dXi[2, 1] $dXi[2, 1][
     3] $dXi[4, 1]/($F01[2, 3] $F01[4, 3]) + 
   Phi[2, 4, 1, 1] $dXi[4, 1] $dXi[2, 1] $dXi[4, 1][
     3] $dXi[2, 1]/($F01[2, 3] $F01[4, 3]) + 
   Phi[2, 4, 1, 1] $dXi[2, 1] $dXi[4, 1] $dXi[2, 1][
     3] $dXi[4, 1]/($F01[2, 3] $F01[4, 3]) + 
   Phi[2, 4, 1, 1] $dXi[4, 1] $dXi[4, 1] $dXi[2, 1][
     3] $dXi[2, 1]/($F01[2, 3] $F01[4, 3]) + 
   Phi[2, 4, 1, 1] $dXi[2, 1] $dXi[4, 1] $dXi[4, 1][
     3] $dXi[2, 1]/($F01[2, 3] $F01[4, 3]) + 
   3 Phi[3, 3, 1, 1] $dXi[3, 1] $dXi[3, 1] $dXi[3, 1] $dXi[3, 1]/
     $F01[3, 3]^2 + 
   Phi[3, 4, 1, 1] $dXi[4, 1] $dXi[4, 1] $dXi[3, 1][
     3] $dXi[3, 1]/($F01[3, 3] $F01[4, 3]) + 
   Phi[3, 4, 1, 1] $dXi[4, 1] $dXi[3, 1] $dXi[4, 1][
     3] $dXi[3, 1]/($F01[3, 3] $F01[4, 3]) + 
   Phi[3, 4, 1, 1] $dXi[3, 1] $dXi[4, 1] $dXi[4, 1][
     3] $dXi[3, 1]/($F01[3, 3] $F01[4, 3]) + 
   Phi[3, 4, 1, 1] $dXi[3, 1] $dXi[4, 1] $dXi[3, 1][
     3] $dXi[4, 1]/($F01[3, 3] $F01[4, 3]) + 
   Phi[3, 4, 1, 1] $dXi[3, 1] $dXi[3, 1] $dXi[4, 1][
     3] $dXi[4, 1]/($F01[3, 3] $F01[4, 3]) + 
   Phi[3, 4, 1, 1] $dXi[4, 1] $dXi[3, 1] $dXi[3, 1][
     3] $dXi[4, 1]/($F01[3, 3] $F01[4, 3]) + 
   3 Phi[4, 4, 1, 1] $dXi[4, 1] $dXi[4, 1] $dXi[4, 1] $dXi[4, 1]/
     $F01[4, 3]^2;
w[1, 2] = 
  25/24 Phi[1, 1, 1, 3] $dXi[1, 1] $dXi[1, 1]/$F01[1, 3]^2 + 
   3/2 Phi[1, 1, 1, 1] $dXi[1, 3] $dXi[1, 1]/$F01[1, 3]^2 + 
   3/2 Phi[1, 1, 1, 1] $dXi[1, 1] $dXi[1, 3]/$F01[1, 3]^2 - 
   13/8 $F01[1, 5] Phi[1, 1, 1, 1] $dXi[1, 1] $dXi[1, 1]/$F01[1, 3]^3 + 
   25/24 Phi[2, 2, 1, 3] $dXi[2, 1] $dXi[2, 1]/$F01[2, 3]^2 + 
   3/2 Phi[2, 2, 1, 1] $dXi[2, 1] $dXi[2, 3]/$F01[2, 3]^2 + 
   3/2 Phi[2, 2, 1, 1] $dXi[2, 3] $dXi[2, 1]/$F01[2, 3]^2 - 
   13/8 $F01[2, 5] Phi[2, 2, 1, 1] $dXi[2, 1] $dXi[2, 1]/$F01[2, 3]^3 + 
   25/24 Phi[3, 3, 1, 3] $dXi[3, 1] $dXi[3, 1]/$F01[3, 3]^2 + 
   3/2 Phi[3, 3, 1, 1] $dXi[3, 1] $dXi[3, 3]/$F01[3, 3]^2 + 
   3/2 Phi[3, 3, 1, 1] $dXi[3, 3] $dXi[3, 1]/$F01[3, 3]^2 - 
   13/8 $F01[3, 5] Phi[3, 3, 1, 1] $dXi[3, 1] $dXi[3, 1]/$F01[3, 3]^3 + 
   25/24 Phi[4, 4, 1, 3] $dXi[4, 1] $dXi[4, 1]/$F01[4, 3]^2 + 
   3/2 Phi[4, 4, 1, 1] $dXi[4, 1] $dXi[4, 3]/$F01[4, 3]^2 + 
   3/2 Phi[4, 4, 1, 1] $dXi[4, 3] $dXi[4, 1]/$F01[4, 3]^2 - 
   13/8 $F01[4, 5] Phi[4, 4, 1, 1] $dXi[4, 1] $dXi[4, 1]/$F01[4, 3]^3 + 
   5/8 $dXi[1, 1] $dXi[1, 5]/$F01[1, 3]^2 + 
   5/8 $dXi[1, 5] $dXi[1, 1]/$F01[1, 3]^2 + 
   3/8 $dXi[1, 3] $dXi[1, 3]/$F01[1, 3]^2 - 
   5/8 $F01[1, 7] $dXi[1, 1] $dXi[1, 1]/$F01[1, 3]^3 - 
   3/4 $F01[1, 5] $dXi[1, 3] $dXi[1, 1]/$F01[1, 3]^3 - 
   3/4 $F01[1, 5] $dXi[1, 1] $dXi[1, 3]/$F01[1, 3]^3 + 
   3/4 $F01[1, 5]^2 $dXi[1, 1] $dXi[1, 1]/$F01[1, 3]^4 + 
   5/8 $dXi[2, 5] $dXi[2, 1]/$F01[2, 3]^2 + 
   5/8 $dXi[2, 1] $dXi[2, 5]/$F01[2, 3]^2 + 
   3/8 $dXi[2, 3] $dXi[2, 3]/$F01[2, 3]^2 - 
   5/8 $F01[2, 7] $dXi[2, 1] $dXi[2, 1]/$F01[2, 3]^3 - 
   3/4 $F01[2, 5] $dXi[2, 1] $dXi[2, 3]/$F01[2, 3]^3 - 
   3/4 $F01[2, 5] $dXi[2, 3] $dXi[2, 1]/$F01[2, 3]^3 + 
   3/4 $F01[2, 5]^2 $dXi[2, 1] $dXi[2, 1]/$F01[2, 3]^4 + 
   5/8 $dXi[3, 5] $dXi[3, 1]/$F01[3, 3]^2 + 
   5/8 $dXi[3, 1] $dXi[3, 5]/$F01[3, 3]^2 + 
   3/8 $dXi[3, 3] $dXi[3, 3]/$F01[3, 3]^2 - 
   5/8 $F01[3, 7] $dXi[3, 1] $dXi[3, 1]/$F01[3, 3]^3 - 
   3/4 $F01[3, 5] $dXi[3, 1] $dXi[3, 3]/$F01[3, 3]^3 - 
   3/4 $F01[3, 5] $dXi[3, 3] $dXi[3, 1]/$F01[3, 3]^3 + 
   3/4 $F01[3, 5]^2 $dXi[3, 1] $dXi[3, 1]/$F01[3, 3]^4 + 
   5/8 $dXi[4, 5] $dXi[4, 1]/$F01[4, 3]^2 + 
   5/8 $dXi[4, 1] $dXi[4, 5]/$F01[4, 3]^2 + 
   3/8 $dXi[4, 3] $dXi[4, 3]/$F01[4, 3]^2 - 
   5/8 $F01[4, 7] $dXi[4, 1] $dXi[4, 1]/$F01[4, 3]^3 - 
   3/4 $F01[4, 5] $dXi[4, 1] $dXi[4, 3]/$F01[4, 3]^3 - 
   3/4 $F01[4, 5] $dXi[4, 3] $dXi[4, 1]/$F01[4, 3]^3 + 
   3/4 $F01[4, 5]^2 $dXi[4, 1] $dXi[4, 1]/$F01[4, 3]^4 + 
   Phi[1, 1, 1, 1]^2 $dXi[1, 1] $dXi[1, 1]/$F01[1, 3]^2 + 
   1/2 Phi[1, 1, 1, 1] Phi[1, 2, 1, 1] $dXi[2, 1][
     1] $dXi[2, 1]/($F01[1, 3] $F01[2, 3]) + 
   1/2 Phi[1, 1, 1, 1] Phi[1, 3, 1, 1] $dXi[3, 1][
     1] $dXi[3, 1]/($F01[1, 3] $F01[3, 3]) + 
   1/2 Phi[1, 1, 1, 1] Phi[1, 4, 1, 1] $dXi[4, 1][
     1] $dXi[4, 1]/($F01[1, 3] $F01[4, 3]) + 
   1/2 Phi[1, 2, 1, 1] Phi[2, 2, 1, 1] $dXi[1, 1][
     1] $dXi[1, 1]/($F01[1, 3] $F01[2, 3]) + 
   Phi[2, 2, 1, 1]^2 $dXi[2, 1] $dXi[2, 1]/$F01[2, 3]^2 + 
   1/2 Phi[2, 2, 1, 1] Phi[2, 3, 1, 1] $dXi[3, 1][
     1] $dXi[3, 1]/($F01[2, 3] $F01[3, 3]) + 
   1/2 Phi[2, 2, 1, 1] Phi[2, 4, 1, 1] $dXi[4, 1][
     1] $dXi[4, 1]/($F01[2, 3] $F01[4, 3]) + 
   1/2 Phi[1, 3, 1, 1] Phi[3, 3, 1, 1] $dXi[1, 1][
     1] $dXi[1, 1]/($F01[1, 3] $F01[3, 3]) + 
   1/2 Phi[2, 3, 1, 1] Phi[3, 3, 1, 1] $dXi[2, 1][
     1] $dXi[2, 1]/($F01[2, 3] $F01[3, 3]) + 
   Phi[3, 3, 1, 1]^2 $dXi[3, 1] $dXi[3, 1]/$F01[3, 3]^2 + 
   1/2 Phi[3, 3, 1, 1] Phi[3, 4, 1, 1] $dXi[4, 1][
     1] $dXi[4, 1]/($F01[3, 3] $F01[4, 3]) + 
   1/2 Phi[1, 4, 1, 1] Phi[4, 4, 1, 1] $dXi[1, 1][
     1] $dXi[1, 1]/($F01[1, 3] $F01[4, 3]) + 
   1/2 Phi[2, 4, 1, 1] Phi[4, 4, 1, 1] $dXi[2, 1][
     1] $dXi[2, 1]/($F01[2, 3] $F01[4, 3]) + 
   1/2 Phi[3, 4, 1, 1] Phi[4, 4, 1, 1] $dXi[3, 1][
     1] $dXi[3, 1]/($F01[3, 3] $F01[4, 3]) + 
   Phi[4, 4, 1, 1]^2 $dXi[4, 1] $dXi[4, 1]/$F01[4, 3]^2 + 
   1/2 Phi[1, 2, 1, 1]^2 $dXi[1, 1] $dXi[2, 1]/($F01[1, 3] $F01[2, 3]) + 
   1/2 Phi[1, 2, 1, 1]^2 $dXi[2, 1] $dXi[1, 1]/($F01[1, 3] $F01[2, 3]) + 
   1/2 Phi[1, 3, 1, 1]^2 $dXi[3, 1] $dXi[1, 1]/($F01[1, 3] $F01[3, 3]) + 
   1/2 Phi[1, 3, 1, 1]^2 $dXi[1, 1] $dXi[3, 1]/($F01[1, 3] $F01[3, 3]) + 
   1/2 Phi[1, 4, 1, 1]^2 $dXi[1, 1] $dXi[4, 1]/($F01[1, 3] $F01[4, 3]) + 
   1/2 Phi[1, 4, 1, 1]^2 $dXi[4, 1] $dXi[1, 1]/($F01[1, 3] $F01[4, 3]) + 
   1/2 Phi[2, 3, 1, 1]^2 $dXi[3, 1] $dXi[2, 1]/($F01[2, 3] $F01[3, 3]) + 
   1/2 Phi[2, 3, 1, 1]^2 $dXi[2, 1] $dXi[3, 1]/($F01[2, 3] $F01[3, 3]) + 
   1/2 Phi[2, 4, 1, 1]^2 $dXi[2, 1] $dXi[4, 1]/($F01[2, 3] $F01[4, 3]) + 
   1/2 Phi[2, 4, 1, 1]^2 $dXi[4, 1] $dXi[2, 1]/($F01[2, 3] $F01[4, 3]) + 
   1/2 Phi[3, 4, 1, 1]^2 $dXi[4, 1] $dXi[3, 1]/($F01[3, 3] $F01[4, 3]) + 
   1/2 Phi[3, 4, 1, 1]^2 $dXi[3, 1] $dXi[4, 1]/($F01[3, 3] $F01[4, 3]) + 
   1/24 Phi[1, 2, 1, 3] $dXi[1, 1] $dXi[1, 1]/($F01[1, 3] $F01[2, 3]) - 
   1/8 $F01[2, 5] Phi[1, 2, 1, 1] $dXi[1, 1][
     1] $dXi[1, 1]/($F01[1, 3] $F01[2, 3]^2) + 
   1/24 Phi[1, 3, 1, 3] $dXi[1, 1] $dXi[1, 1]/($F01[1, 3] $F01[3, 3]) - 
   1/8 $F01[3, 5] Phi[1, 3, 1, 1] $dXi[1, 1][
     1] $dXi[1, 1]/($F01[1, 3] $F01[3, 3]^2) + 
   1/24 Phi[1, 4, 1, 3] $dXi[1, 1] $dXi[1, 1]/($F01[1, 3] $F01[4, 3]) - 
   1/8 $F01[4, 5] Phi[1, 4, 1, 1] $dXi[1, 1][
     1] $dXi[1, 1]/($F01[1, 3] $F01[4, 3]^2) + 
   1/24 Phi[2, 1, 1, 3] $dXi[2, 1] $dXi[2, 1]/($F01[1, 3] $F01[2, 3]) - 
   1/8 $F01[1, 5] Phi[1, 2, 1, 1] $dXi[2, 1][
     1] $dXi[2, 1]/($F01[1, 3]^2 $F01[2, 3]) + 
   1/24 Phi[2, 3, 1, 3] $dXi[2, 1] $dXi[2, 1]/($F01[2, 3] $F01[3, 3]) - 
   1/8 $F01[3, 5] Phi[2, 3, 1, 1] $dXi[2, 1][
     1] $dXi[2, 1]/($F01[2, 3] $F01[3, 3]^2) + 
   1/24 Phi[2, 4, 1, 3] $dXi[2, 1] $dXi[2, 1]/($F01[2, 3] $F01[4, 3]) - 
   1/8 $F01[4, 5] Phi[2, 4, 1, 1] $dXi[2, 1][
     1] $dXi[2, 1]/($F01[2, 3] $F01[4, 3]^2) + 
   1/24 Phi[3, 1, 1, 3] $dXi[3, 1] $dXi[3, 1]/($F01[1, 3] $F01[3, 3]) - 
   1/8 $F01[1, 5] Phi[1, 3, 1, 1] $dXi[3, 1][
     1] $dXi[3, 1]/($F01[1, 3]^2 $F01[3, 3]) + 
   1/24 Phi[3, 2, 1, 3] $dXi[3, 1] $dXi[3, 1]/($F01[2, 3] $F01[3, 3]) - 
   1/8 $F01[2, 5] Phi[2, 3, 1, 1] $dXi[3, 1][
     1] $dXi[3, 1]/($F01[2, 3]^2 $F01[3, 3]) + 
   1/24 Phi[3, 4, 1, 3] $dXi[3, 1] $dXi[3, 1]/($F01[3, 3] $F01[4, 3]) - 
   1/8 $F01[4, 5] Phi[3, 4, 1, 1] $dXi[3, 1][
     1] $dXi[3, 1]/($F01[3, 3] $F01[4, 3]^2) + 
   1/24 Phi[4, 1, 1, 3] $dXi[4, 1] $dXi[4, 1]/($F01[1, 3] $F01[4, 3]) - 
   1/8 $F01[1, 5] Phi[1, 4, 1, 1] $dXi[4, 1][
     1] $dXi[4, 1]/($F01[1, 3]^2 $F01[4, 3]) + 
   1/24 Phi[4, 2, 1, 3] $dXi[4, 1] $dXi[4, 1]/($F01[2, 3] $F01[4, 3]) - 
   1/8 $F01[2, 5] Phi[2, 4, 1, 1] $dXi[4, 1][
     1] $dXi[4, 1]/($F01[2, 3]^2 $F01[4, 3]) + 
   1/24 Phi[4, 3, 1, 3] $dXi[4, 1] $dXi[4, 1]/($F01[3, 3] $F01[4, 3]) - 
   1/8 $F01[3, 5] Phi[3, 4, 1, 1] $dXi[4, 1][
     1] $dXi[4, 1]/($F01[3, 3]^2 $F01[4, 3]);
End[];


EndPackage[];
