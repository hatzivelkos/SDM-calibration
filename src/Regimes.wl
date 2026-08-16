(* ::Package:: *)

(* ============================================================ *)
(* File: src/Regimes.wl                                        *)
(* Purpose: Geometric regime classification                     *)
(*                                                              *)
(* Requires:                                                    *)
(*   Profile.wl                                                 *)
(*   Signals.wl                                                 *)
(*   Pareto.wl                                                  *)
(*   Geometry.wl                                                *)
(*                                                              *)
(* Geometric regimes are defined only by the largest Pareto     *)
(* front observed on a fixed finite classification grid.        *)
(*                                                              *)
(*   Regime I   : maximal front size = 1                        *)
(*   Regime II  : maximal front size = 2                        *)
(*   Regime III : maximal front size >= 3                       *)
(*                                                              *)
(* This classification is distinct from calibration status.     *)
(* A multi-point profile need not have positive maximal signed  *)
(* area. Calibration is handled separately in Calibration.wl.   *)
(* ============================================================ *)


ClearAll[
  RegimeLabelFromFrontSize,
  RegimeNumberFromFrontSize,
  BuildRegimeGridU,
  FrontDetailsAtP,
  FrontDetailsOnGrid,
  GeometricRegimeSummary
];


(* ------------------------------------------------------------ *)
(* Regime labels                                                *)
(* ------------------------------------------------------------ *)

RegimeLabelFromFrontSize[n_Integer?Positive] :=
  Which[
    n == 1,
      "one-point",

    n == 2,
      "two-point",

    n >= 3,
      "multi-point"
  ];


RegimeNumberFromFrontSize[n_Integer?Positive] :=
  Which[
    n == 1,
      1,

    n == 2,
      2,

    n >= 3,
      3
  ];


(* ------------------------------------------------------------ *)
(* Fixed classification grid                                   *)
(* ------------------------------------------------------------ *)

BuildRegimeGridU::range =
  "Search range must satisfy 1 <= pMin < pMax.";

BuildRegimeGridU::size =
  "Grid size must be an integer of at least 2.";


(* The classification grid is uniform in u = 1/p. *)

BuildRegimeGridU[
  pMin_: 1.,
  pMax_: 20.,
  gridSize_: 26
] := Module[
  {pMinN, pMaxN, uMin, uMax},

  pMinN = N[pMin];
  pMaxN = N[pMax];

  If[
    !NumericQ[pMinN] ||
    !NumericQ[pMaxN] ||
    pMinN < 1 ||
    pMinN >= pMaxN,

    Message[BuildRegimeGridU::range];
    Return[$Failed]
  ];

  If[
    !IntegerQ[gridSize] ||
    gridSize < 2,

    Message[BuildRegimeGridU::size];
    Return[$Failed]
  ];

  uMin = N[1/pMaxN];
  uMax = N[1/pMinN];

  N @ Subdivide[
    uMin,
    uMax,
    gridSize - 1
  ]
];


(* ------------------------------------------------------------ *)
(* Front structure at one finite p                              *)
(* ------------------------------------------------------------ *)

FrontDetailsAtP[
  A_?ProfileQ,
  p_,
  legitimacySignal_: "TopSupport"
] := Module[
  {geom},

  geom =
    ParetoGeometry[
      A,
      p,
      legitimacySignal
    ];

  If[
    geom === $Failed ||
    !AssociationQ[geom],
    Return[$Failed]
  ];

  <|
    "p" ->
      N[p],

    "legitimacySignal" ->
      legitimacySignal,

    "frontSize" ->
      geom["frontSize"],

    "regime" ->
      RegimeLabelFromFrontSize[
        geom["frontSize"]
      ],

    "front" ->
      geom["front"]
  |>
];


(* ------------------------------------------------------------ *)
(* Front structure over a fixed grid                            *)
(* ------------------------------------------------------------ *)

Options[FrontDetailsOnGrid] = {
  "pMin" -> 1.,
  "pMax" -> 20.,
  "GridSize" -> 26,
  "GridU" -> Automatic,
  "LegitimacySignal" -> "TopSupport",
  "KeepFronts" -> False
};


FrontDetailsOnGrid[
  A_?ProfileQ,
  opts : OptionsPattern[]
] := Module[
  {
    pMin,
    pMax,
    gridSize,
    gridU,
    legitimacySignal,
    keepFronts,
    uMin,
    uMax,
    pOfU,
    rows
  },

  pMin =
    N[OptionValue["pMin"]];

  pMax =
    N[OptionValue["pMax"]];

  gridSize =
    OptionValue["GridSize"];

  legitimacySignal =
    OptionValue["LegitimacySignal"];

  keepFronts =
    TrueQ[OptionValue["KeepFronts"]];


  If[
    !NumericQ[pMin] ||
    !NumericQ[pMax] ||
    pMin < 1 ||
    pMin >= pMax,

    Message[BuildRegimeGridU::range];
    Return[$Failed]
  ];

  uMin =
    N[1/pMax];

  uMax =
    N[1/pMin];


  gridU =
    OptionValue["GridU"];

  If[
    gridU === Automatic,

    gridU =
      BuildRegimeGridU[
        pMin,
        pMax,
        gridSize
      ];

    If[
      gridU === $Failed,
      Return[$Failed]
    ];
  ];


  gridU =
    Sort @
    DeleteDuplicates @
    Select[
      N @ gridU,
      NumericQ[#] &&
      uMin <= # <= uMax &
    ];


  If[
    gridU === {},
    Return[$Failed]
  ];


  pOfU[u_?NumericQ] :=
    N[1/u];


  rows =
    Table[
      Module[
        {p, det},

        p =
          pOfU[u];

        det =
          FrontDetailsAtP[
            A,
            p,
            legitimacySignal
          ];

        If[
          det === $Failed,
          Return[$Failed]
        ];

        If[
          keepFronts,

          Join[
            <|"u" -> u|>,
            det
          ],

          <|
            "u" -> u,
            "p" -> det["p"],
            "frontSize" -> det["frontSize"],
            "regime" -> det["regime"]
          |>
        ]
      ],
      {u, gridU}
    ];


  If[
    MemberQ[rows, $Failed],
    Return[$Failed]
  ];


  <|
    "legitimacySignal" ->
      legitimacySignal,

    "gridU" ->
      gridU,

    "gridP" ->
      Lookup[rows, "p"],

    "rows" ->
      rows
  |>
];


(* ------------------------------------------------------------ *)
(* Profile-level geometric regime summary                       *)
(* ------------------------------------------------------------ *)

Options[GeometricRegimeSummary] =
  Options[FrontDetailsOnGrid];


GeometricRegimeSummary[
  A_?ProfileQ,
  opts : OptionsPattern[]
] := Module[
  {
    gridDetails,
    rows,
    sizes,
    maxSize,
    maxPositions,
    regime,
    regimeNumber
  },

  gridDetails =
    FrontDetailsOnGrid[
      A,
      opts
    ];

  If[
    gridDetails === $Failed,
    Return[$Failed]
  ];

  rows =
    gridDetails["rows"];

  sizes =
    Lookup[
      rows,
      "frontSize"
    ];


  If[
    sizes === {} ||
    !VectorQ[sizes, IntegerQ],

    Return[$Failed]
  ];


  maxSize =
    Max[sizes];

  regime =
    RegimeLabelFromFrontSize[
      maxSize
    ];

  regimeNumber =
    RegimeNumberFromFrontSize[
      maxSize
    ];


  maxPositions =
    Flatten @
    Position[
      sizes,
      maxSize
    ];


  <|
    "legitimacySignal" ->
      gridDetails["legitimacySignal"],

    "regime" ->
      regime,

    "regimeNumber" ->
      regimeNumber,

    "maxFrontSize" ->
      maxSize,

    "frontSizeAtP1" ->
      Last[sizes],

    "regimeAtP1" ->
      RegimeLabelFromFrontSize[
        Last[sizes]
      ],

    "pValuesWithMaxFrontSize" ->
      Lookup[
        rows[[maxPositions]],
        "p"
      ],

    "frontSizesOnGrid" ->
      sizes,

    "gridU" ->
      gridDetails["gridU"],

    "gridP" ->
      gridDetails["gridP"],

    "gridDetails" ->
      If[
        TrueQ[OptionValue["KeepFronts"]],
        rows,
        Missing["NotRequested"]
      ]
  |>
];