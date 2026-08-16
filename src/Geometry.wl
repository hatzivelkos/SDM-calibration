(* ::Package:: *)

(* ============================================================ *)
(* File: src/Geometry.wl                                       *)
(* Purpose: Normalized compromise-legitimacy geometry           *)
(*                                                              *)
(* Requires:                                                    *)
(*   Profile.wl                                                 *)
(*   Signals.wl                                                 *)
(*   Pareto.wl                                                  *)
(*                                                              *)
(* Finite-p geometric coordinates:                              *)
(*                                                              *)
(*   x_i(p) = d_p(i)^(1/p) / max_j d_p(j)^(1/p)                *)
(*   y_i    = L(i) / max_j L(j)                                *)
(*                                                              *)
(* Smaller x is better; larger y is better.                     *)
(*                                                              *)
(* The legitimacy signal is supplied through                    *)
(* LegitimacyVector and is therefore replaceable without        *)
(* changing the geometric machinery.                            *)
(* ============================================================ *)


ClearAll[
  NormalizedCandidatePoints,
  ParetoGeometry,
  ChordValue,
  SignedAreaFromFront,
  SignedAreaDetails
];


(* ------------------------------------------------------------ *)
(* Normalized candidate coordinates                             *)
(* ------------------------------------------------------------ *)

NormalizedCandidatePoints::finite =
  "Geometric coordinates are defined only for finite p >= 1.";

NormalizedCandidatePoints::signal =
  "Could not construct legitimacy signal `1`.";

NormalizedCandidatePoints::scale =
  "Normalization failed because a coordinate scale is zero or invalid.";


(* Returns:
     {{x_1,y_1,1}, ..., {x_m,y_m,m}}

   The candidate index is retained as the third component.
*)

NormalizedCandidatePoints[
  A_?ProfileQ,
  p_,
  legitimacySignal_: "TopSupport"
] := Module[
  {d, L, rootedD, dMax, lMax, m},

  If[
    p === Infinity || !NumericQ[p] || p < 1,
    Message[NormalizedCandidatePoints::finite];
    Return[$Failed]
  ];

  d = DpVector[A, p];
  L = LegitimacyVector[A, legitimacySignal];

  If[
    d === $Failed || L === $Failed,
    Message[NormalizedCandidatePoints::signal, legitimacySignal];
    Return[$Failed]
  ];

  m = CandidateCount[A];

  If[
    Length[d] =!= m || Length[L] =!= m,
    Message[NormalizedCandidatePoints::scale];
    Return[$Failed]
  ];

  rootedD = N[d^(1/p)];

  dMax = Max[rootedD];
  lMax = Max[N[L]];

  If[
    !NumericQ[dMax] || !NumericQ[lMax] ||
    dMax <= 0 || lMax <= 0,
    Message[NormalizedCandidatePoints::scale];
    Return[$Failed]
  ];

  MapThread[
    {#1/dMax, #2/lMax, #3} &,
    {
      rootedD,
      N[L],
      Range[m]
    }
  ]
];


(* ------------------------------------------------------------ *)
(* Pareto geometry at fixed finite p                            *)
(* ------------------------------------------------------------ *)

(* Returns both the full normalized candidate cloud and its
   geometric Pareto front.

   ParetoFront removes duplicate geometric points while retaining
   one deterministic candidate representative.
*)

ParetoGeometry[
  A_?ProfileQ,
  p_,
  legitimacySignal_: "TopSupport"
] := Module[
  {points, front},

  points =
    NormalizedCandidatePoints[
      A,
      p,
      legitimacySignal
    ];

  If[points === $Failed, Return[$Failed]];

  front = ParetoFront[points];

  <|
    "p" -> N[p],
    "legitimacySignal" -> legitimacySignal,
    "points" -> points,
    "front" -> front,
    "frontSize" -> Length[front]
  |>
];


(* ------------------------------------------------------------ *)
(* Baseline chord                                               *)
(* ------------------------------------------------------------ *)

(* Value of the affine chord through points a and b at x.

   Points have representation {x,y,i}.
*)

ChordValue::vertical =
  "The baseline chord is vertical or numerically collapsed.";


ChordValue[
  x_?NumericQ,
  a_?CriterionPointQ,
  b_?CriterionPointQ
] := Module[
  {xA, yA, xB, yB},

  {xA, yA} = a[[1 ;; 2]];
  {xB, yB} = b[[1 ;; 2]];

  If[
    Abs[xB - xA] < 10^-14,
    Message[ChordValue::vertical];
    Return[$Failed]
  ];

  yA + (yB - yA) (x - xA)/(xB - xA)
];


(* ------------------------------------------------------------ *)
(* Signed area                                                  *)
(* ------------------------------------------------------------ *)

(* Signed trapezoidal area between a normalized Pareto-front
   polyline and the chord joining its compromise and legitimacy
   endpoints.

   Positive contributions correspond to front segments above
   the chord; negative contributions correspond to segments
   below it.
*)

SignedAreaFromFront[
  front_List
] /; AllTrue[front, CriterionPointQ] := Module[
  {
    F,
    endpoints,
    a,
    b,
    xA,
    xB,
    offsets
  },

  F = FrontSorted[front];

  (* One- and two-point fronts have zero internal area. *)
  If[Length[F] <= 2, Return[0.]];

  endpoints = ChordEndpoints[F];

  If[endpoints === $Failed, Return[$Failed]];

  {a, b} = endpoints;

  xA = a[[1]];
  xB = b[[1]];

  (* A nontrivial Pareto front should not have a vertical
     endpoint chord. Keep a numerical safeguard nevertheless. *)
  If[Abs[xB - xA] < 10^-14, Return[0.]];

  offsets =
    Table[
      F[[k, 2]] - ChordValue[F[[k, 1]], a, b],
      {k, Length[F]}
    ];

  N @ Sum[
    ((offsets[[k]] + offsets[[k + 1]])/2) *
      (F[[k + 1, 1]] - F[[k, 1]]),
    {k, 1, Length[F] - 1}
  ]
];


(* ------------------------------------------------------------ *)
(* Full signed-area diagnostics                                 *)
(* ------------------------------------------------------------ *)

(* Main fixed-p geometric diagnostic used later by calibration.

   The output contains:
     - normalized candidate cloud
     - normalized Pareto front
     - front size
     - chord endpoints
     - signed offsets from the chord
     - signed area
*)

SignedAreaDetails[
  A_?ProfileQ,
  p_,
  legitimacySignal_: "TopSupport"
] := Module[
  {
    geom,
    front,
    endpoints,
    a,
    b,
    offsets,
    area
  },

  geom =
    ParetoGeometry[
      A,
      p,
      legitimacySignal
    ];

  If[geom === $Failed, Return[$Failed]];

  front = geom["front"];

  If[front === {},
    Return[$Failed]
  ];

  endpoints = ChordEndpoints[front];

  If[endpoints === $Failed, Return[$Failed]];

  {a, b} = endpoints;

  offsets =
    If[
      Length[front] <= 2 || Abs[b[[1]] - a[[1]]] < 10^-14,

      ConstantArray[0., Length[front]],

      Table[
        N[
          front[[k, 2]] -
          ChordValue[front[[k, 1]], a, b]
        ],
        {k, Length[front]}
      ]
    ];

  area =
    SignedAreaFromFront[front];

  Join[
    geom,
    <|
      "endpointCompromise" -> a,
      "endpointLegitimacy" -> b,
      "signedOffsets" -> offsets,
      "areaSigned" -> area
    |>
  ]
];