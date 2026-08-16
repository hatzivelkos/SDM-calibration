(* ::Package:: *)

(* ============================================================ *)
(* File: src/Pareto.wl                                         *)
(* Purpose: Generic Pareto dominance and Pareto-front utilities *)
(*                                                              *)
(* Point representation:                                       *)
(*   {d, L, i}                                                  *)
(* where                                                        *)
(*   d = compromise value                                       *)
(*       - numeric for finite p                                 *)
(*       - vector for the SDM p -> Infinity limit               *)
(*   L = legitimacy value                                       *)
(*   i = candidate index                                        *)
(*                                                              *)
(* Smaller d is better; larger L is better.                     *)
(*                                                              *)
(* This module is independent of the particular legitimacy      *)
(* signal.                                                       *)
(* ============================================================ *)


ClearAll[
  CriterionPointQ,
  CriterionPoints,
  DCompare,
  DLeq,
  DLt,
  DEqualQ,
  DominatesQ,
  SameCriterionCoordinatesQ,
  ParetoFrontCandidates,
  ParetoFront,
  FrontSorted,
  ChordEndpoints
];


(* ------------------------------------------------------------ *)
(* Compromise-value comparison                                  *)
(* ------------------------------------------------------------ *)

(* Return:
     -1  if d1 < d2
      0  if d1 = d2
      1  if d1 > d2

   Numeric values use the ordinary ordering.
   Lists use lexicographic ordering.
*)

DCompare[d1_?NumericQ, d2_?NumericQ] :=
  Which[
    d1 < d2, -1,
    d1 > d2,  1,
    True,      0
  ];


DCompare[d1_List, d2_List] /;
    Length[d1] == Length[d2] &&
    VectorQ[d1, NumericQ] &&
    VectorQ[d2, NumericQ] :=
  Module[{k},

    k = SelectFirst[
      Range[Length[d1]],
      !TrueQ[d1[[#]] == d2[[#]]] &,
      Missing["Equal"]
    ];

    If[
      MissingQ[k],
      0,
      Which[
        d1[[k]] < d2[[k]], -1,
        d1[[k]] > d2[[k]],  1,
        True,                 0
      ]
    ]
  ];


DCompare[_, _] :=
  $Failed;


DLeq[d1_, d2_] :=
  Module[{c = DCompare[d1, d2]},
    IntegerQ[c] && c <= 0
  ];


DLt[d1_, d2_] :=
  Module[{c = DCompare[d1, d2]},
    IntegerQ[c] && c < 0
  ];


DEqualQ[d1_, d2_] :=
  Module[{c = DCompare[d1, d2]},
    IntegerQ[c] && c == 0
  ];


(* ------------------------------------------------------------ *)
(* Candidate criterion points                                   *)
(* ------------------------------------------------------------ *)

CriterionPointQ[pt_] :=
  ListQ[pt] &&
  Length[pt] == 3 &&
  (
    NumericQ[pt[[1]]] ||
    (
      ListQ[pt[[1]]] &&
      VectorQ[pt[[1]], NumericQ]
    )
  ) &&
  NumericQ[pt[[2]]] &&
  IntegerQ[pt[[3]]] &&
  pt[[3]] >= 1;


CriterionPoints::dim =
  "Compromise and legitimacy vectors must have the same positive length.";

CriterionPoints::compromise =
  "Invalid compromise vector.";

CriterionPoints::legitimacy =
  "Legitimacy values must be numeric.";


(* Build candidate-level points from a compromise vector and a
   legitimacy vector.

   Example:
     CriterionPoints[{5,1,9}, {1,2,0}]
       -> {{5,1,1},{1,2,2},{9,0,3}}
*)

CriterionPoints[compromise_List, legitimacy_List] := Module[
  {m, numericCompromiseQ, vectorCompromiseQ},

  m = Length[compromise];

  If[
    m < 1 || Length[legitimacy] =!= m,
    Message[CriterionPoints::dim];
    Return[$Failed]
  ];

  If[
    !VectorQ[legitimacy, NumericQ],
    Message[CriterionPoints::legitimacy];
    Return[$Failed]
  ];

  numericCompromiseQ =
    VectorQ[compromise, NumericQ];

  vectorCompromiseQ =
    AllTrue[compromise, ListQ] &&
    Length[DeleteDuplicates[Length /@ compromise]] == 1 &&
    AllTrue[compromise, VectorQ[#, NumericQ] &];

  If[
    !(numericCompromiseQ || vectorCompromiseQ),
    Message[CriterionPoints::compromise];
    Return[$Failed]
  ];

  MapThread[
    {#1, #2, #3} &,
    {
      compromise,
      legitimacy,
      Range[m]
    }
  ]
];


(* ------------------------------------------------------------ *)
(* Pareto dominance                                             *)
(* ------------------------------------------------------------ *)

(* x dominates y when:
     compromise(x) <= compromise(y),
     legitimacy(x) >= legitimacy(y),
   and at least one comparison is strict.
*)

DominatesQ[x_?CriterionPointQ, y_?CriterionPointQ] :=
  DLeq[x[[1]], y[[1]]] &&
  x[[2]] >= y[[2]] &&
  (
    DLt[x[[1]], y[[1]]] ||
    x[[2]] > y[[2]]
  );


(* Two candidates may generate the same geometric point.
   Candidate index is deliberately ignored here.
*)

SameCriterionCoordinatesQ[
  x_?CriterionPointQ,
  y_?CriterionPointQ
] :=
  DEqualQ[x[[1]], y[[1]]] &&
  TrueQ[x[[2]] == y[[2]]];


(* ------------------------------------------------------------ *)
(* Candidate-level nondominance                                 *)
(* ------------------------------------------------------------ *)

(* Keeps candidate labels. If two candidates have identical
   coordinates, both are nondominated at this stage.
*)

ParetoFrontCandidates[pts_List] /;
    AllTrue[pts, CriterionPointQ] :=
  Select[
    pts,
    Function[x,
      !AnyTrue[
        pts,
        DominatesQ[#, x] &
      ]
    ]
  ];


(* ------------------------------------------------------------ *)
(* Geometric Pareto front                                       *)
(* ------------------------------------------------------------ *)

(* Sort front from the compromise endpoint toward the
   legitimacy endpoint:
     increasing d,
     then decreasing L,
     then increasing candidate index.
*)

FrontSorted[front_List] /;
    AllTrue[front, CriterionPointQ] :=
  Sort[
    front,
    Function[{x, y},
      Module[{c = DCompare[x[[1]], y[[1]]]},

        Which[
          c < 0,
            True,

          c > 0,
            False,

          x[[2]] > y[[2]],
            True,

          x[[2]] < y[[2]],
            False,

          True,
            x[[3]] < y[[3]]
        ]
      ]
    ]
  ];


(* ParetoFront represents the geometric front used in the paper.

   If several candidates have identical {d,L} coordinates,
   they represent one geometric point. The smallest candidate
   index is retained only as a deterministic representative.
*)

ParetoFront[pts_List] /;
    AllTrue[pts, CriterionPointQ] :=
  Module[{candidateFront, uniqueFront},

    candidateFront =
      ParetoFrontCandidates[pts];

    (* smallest candidate index represents coincident points *)
    candidateFront =
      SortBy[candidateFront, Last];

    uniqueFront =
      DeleteDuplicates[
        candidateFront,
        SameCriterionCoordinatesQ
      ];

    FrontSorted[uniqueFront]
  ];


(* ------------------------------------------------------------ *)
(* Pareto-front endpoints                                       *)
(* ------------------------------------------------------------ *)

ChordEndpoints::empty =
  "Cannot determine endpoints of an empty Pareto front.";


(* Returns:
     {compromise endpoint, legitimacy endpoint}

   For a one-point front the two endpoints coincide.
*)

ChordEndpoints[front_List] /;
    AllTrue[front, CriterionPointQ] :=
  Module[{F, maxL, compromiseEndpoint, legitimacyEndpoint},

    If[
      front === {},
      Message[ChordEndpoints::empty];
      Return[$Failed]
    ];

    F = FrontSorted[front];

    compromiseEndpoint =
      First[F];

    maxL =
      Max[F[[All, 2]]];

    (* Since F is ordered by increasing compromise value,
       this also gives the minimum-d representative among
       points attaining maximal legitimacy. *)
    legitimacyEndpoint =
      First @ Select[
        F,
        TrueQ[#[[2]] == maxL] &
      ];

    {
      compromiseEndpoint,
      legitimacyEndpoint
    }
  ];