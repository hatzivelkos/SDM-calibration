(* ::Package:: *)

(* ============================================================ *)
(* File: src/Calibration.wl                                    *)
(* Purpose: Profile-dependent calibration of the SDM parameter  *)
(*                                                              *)
(* Requires:                                                    *)
(*   Profile.wl                                                 *)
(*   Signals.wl                                                 *)
(*   Pareto.wl                                                  *)
(*   Geometry.wl                                                *)
(*                                                              *)
(* The calibration criterion is the signed area of the          *)
(* normalized compromise-legitimacy Pareto front.               *)
(*                                                              *)
(* Search is performed in the transformed coordinate            *)
(*   u = 1/p                                                    *)
(* over a finite interval p in [pMin,pMax].                     *)
(*                                                              *)
(* Local refinement is incumbent-preserving: a refinement step  *)
(* can improve or retain the current best score, but can never  *)
(* replace it by a strictly worse point.                        *)
(* ============================================================ *)


ClearAll[
  CalibrationScore,
  CalibrationEvaluation,
  CalibrateP
];


(* ------------------------------------------------------------ *)
(* Fixed-p calibration criterion                                *)
(* ------------------------------------------------------------ *)

(* Signed-area value at one finite p. *)

CalibrationScore[
  A_?ProfileQ,
  p_,
  legitimacySignal_: "TopSupport"
] := Module[
  {det},

  det =
    SignedAreaDetails[
      A,
      p,
      legitimacySignal
    ];

  If[
    !AssociationQ[det] ||
    !KeyExistsQ[det, "areaSigned"] ||
    !NumericQ[det["areaSigned"]],
    Return[$Failed]
  ];

  det["areaSigned"]
];


(* Full fixed-p geometric diagnostic. *)

CalibrationEvaluation[
  A_?ProfileQ,
  p_,
  legitimacySignal_: "TopSupport"
] :=
  SignedAreaDetails[
    A,
    p,
    legitimacySignal
  ];


(* ------------------------------------------------------------ *)
(* Search for calibrated p                                      *)
(* ------------------------------------------------------------ *)

Options[CalibrateP] = {

  (* finite search interval *)
  "pMin" -> 1.,
  "pMax" -> 20.,

  (* legitimacy coordinate *)
  "LegitimacySignal" -> "TopSupport",

  (* initial grid, uniform in u = 1/p *)
  "InitialGridSize" -> 26,
  "GridU" -> Automatic,

  (* local refinement *)
  "RefineSteps" -> 3,
  "RefineFactor" -> 6,
  "InitialRefineHalfWidthU" -> 0.1,

  (* deterministic tie-breaking between equal maxima *)
  "TieBreak" -> "SmallerP",

  (* positive-area threshold *)
  "DegenerateEps" -> 10^-12,

  (* optional diagnostic output *)
  "KeepTrace" -> False
};


CalibrateP::range =
  "Search range must satisfy 1 <= pMin < pMax.";

CalibrateP::grid =
  "The search grid in u=1/p is invalid or empty.";

CalibrateP::signal =
  "Could not evaluate legitimacy signal `1`.";

CalibrateP::tie =
  "Unknown TieBreak option `1`. Using \"SmallerP\".";


CalibrateP[
  A_?ProfileQ,
  opts : OptionsPattern[]
] := Module[
  {
    pMin,
    pMax,
    uMin,
    uMax,
    legitimacySignal,
    initialGridSize,
    gridU,
    refineSteps,
    refineFactor,
    halfWidth,
    tieBreak,
    eps,
    keepTrace,

    pOfU,
    evalU,
    scoreKey,
    degenerateOutput,

    scored,
    allTrace,
    best,
    initialBest,
    uBest,
    win,
    step,
    candU,
    candidateScored,
    finalDetails,
    finalArea,
    finalFrontSize,
    status
  },


  (* ---------------------------------------------------------- *)
  (* Read and validate options                                  *)
  (* ---------------------------------------------------------- *)

  pMin =
    N[
      OptionValue["pMin"]
    ];

  pMax =
    N[
      OptionValue["pMax"]
    ];


  If[
    !NumericQ[pMin] ||
    !NumericQ[pMax] ||
    pMin < 1 ||
    pMin >= pMax,

    Message[
      CalibrateP::range
    ];

    Return[
      $Failed
    ]
  ];


  uMin =
    N[
      1/pMax
    ];

  uMax =
    N[
      1/pMin
    ];


  legitimacySignal =
    OptionValue[
      "LegitimacySignal"
    ];


  initialGridSize =
    OptionValue[
      "InitialGridSize"
    ];


  refineSteps =
    OptionValue[
      "RefineSteps"
    ];


  refineFactor =
    OptionValue[
      "RefineFactor"
    ];


  halfWidth =
    N[
      OptionValue[
        "InitialRefineHalfWidthU"
      ]
    ];


  tieBreak =
    OptionValue[
      "TieBreak"
    ];


  eps =
    N[
      OptionValue[
        "DegenerateEps"
      ]
    ];


  keepTrace =
    TrueQ[
      OptionValue[
        "KeepTrace"
      ]
    ];


  If[
    !MemberQ[
      {
        "SmallerP",
        "LargerP"
      },
      tieBreak
    ],

    Message[
      CalibrateP::tie,
      tieBreak
    ];

    tieBreak =
      "SmallerP";
  ];


  (* ---------------------------------------------------------- *)
  (* Initial grid                                               *)
  (* ---------------------------------------------------------- *)

  gridU =
    OptionValue[
      "GridU"
    ];


  If[
    gridU === Automatic,

    If[
      !IntegerQ[initialGridSize] ||
      initialGridSize < 2,

      Message[
        CalibrateP::grid
      ];

      Return[
        $Failed
      ]
    ];


    gridU =
      N @
        Subdivide[
          uMin,
          uMax,
          initialGridSize - 1
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

    Message[
      CalibrateP::grid
    ];

    Return[
      $Failed
    ]
  ];


  (* ---------------------------------------------------------- *)
  (* Coordinate conversion                                      *)
  (* ---------------------------------------------------------- *)

  pOfU[
    u_?NumericQ
  ] :=
    N[
      1/u
    ];


  (* ---------------------------------------------------------- *)
  (* Evaluate one search point                                  *)
  (* ---------------------------------------------------------- *)

  evalU[
    u_?NumericQ
  ] := Module[
    {
      p,
      det,
      area
    },


    p =
      pOfU[
        u
      ];


    det =
      CalibrationEvaluation[
        A,
        p,
        legitimacySignal
      ];


    If[
      det === $Failed ||
      !AssociationQ[det] ||
      !KeyExistsQ[
        det,
        "areaSigned"
      ] ||
      !NumericQ[
        det["areaSigned"]
      ],

      Return[
        <|
          "u" -> u,
          "p" -> p,
          "area" -> -Infinity
        |>
      ]
    ];


    area =
      N[
        det["areaSigned"]
      ];


    <|
      "u" -> u,
      "p" -> p,
      "area" -> area
    |>
  ];


  (* ---------------------------------------------------------- *)
  (* Deterministic maximization                                 *)
  (* ---------------------------------------------------------- *)

  scoreKey[
    row_Association
  ] :=
    Switch[

      tieBreak,

      "SmallerP",
        {
          -row["area"],
           row["p"],
           row["u"]
        },

      "LargerP",
        {
          -row["area"],
          -row["p"],
           row["u"]
        }
    ];


  (* ---------------------------------------------------------- *)
  (* Coarse search                                              *)
  (* ---------------------------------------------------------- *)

  scored =
    evalU /@
      gridU;


  allTrace =
    scored;


  If[
    AllTrue[
      scored,
      Lookup[
        #,
        "area",
        -Infinity
      ] === -Infinity &
    ],

    Message[
      CalibrateP::signal,
      legitimacySignal
    ];

    Return[
      $Failed
    ]
  ];


  best =
    First @
      SortBy[
        scored,
        scoreKey
      ];


  initialBest =
    best;


  (* ---------------------------------------------------------- *)
  (* Standardized degenerate output                             *)
  (* ---------------------------------------------------------- *)

  degenerateOutput[] :=
    <|
      "status" ->
        "degenerate",

      "legitimacySignal" ->
        legitimacySignal,

      "pStar" ->
        Missing["Degenerate"],

      "uStar" ->
        Missing["Degenerate"],

      "areaStar" ->
        Missing["Degenerate"],

      "bestInitialP" ->
        initialBest["p"],

      "bestInitialArea" ->
        initialBest["area"],

      "searchParameters" -> <|
        "pMin" -> pMin,
        "pMax" -> pMax,
        "initialGridSize" -> Length[gridU],
        "refineSteps" -> refineSteps,
        "refineFactor" -> refineFactor,
        "initialRefineHalfWidthU" -> halfWidth,
        "tieBreak" -> tieBreak,
        "degenerateEps" -> eps
      |>,

      "searchTrace" ->
        If[
          keepTrace,

          DeleteDuplicatesBy[
            allTrace,
            Lookup[
              #,
              "u"
            ] &
          ],

          Missing[
            "NotRequested"
          ]
        ]
    |>;


  (* ---------------------------------------------------------- *)
  (* Degenerate calibration outcome on the initial grid         *)
  (* ---------------------------------------------------------- *)

  If[
    best["area"] <= eps,

    Return[
      degenerateOutput[]
    ]
  ];


  (* ---------------------------------------------------------- *)
  (* Local refinement around best coarse-grid value             *)
  (* ---------------------------------------------------------- *)

  uBest =
    best["u"];


  win = {
    Max[
      uMin,
      uBest - halfWidth
    ],

    Min[
      uMax,
      uBest + halfWidth
    ]
  };


  Do[

    step =
      (win[[2]] - win[[1]]) /
        refineFactor;


    If[
      step <= 0,
      Break[]
    ];


    candU =
      N @
        Range[
          win[[1]],
          win[[2]],
          step
        ];


    (* The incumbent uBest is inserted explicitly.
       This prevents a refinement grid from omitting the
       previously best search point. *)

    candU =
      Sort @
        DeleteDuplicates @
          Join[
            candU,
            {
              win[[1]],
              win[[2]],
              uBest
            }
          ];


    candU =
      Select[
        candU,
        uMin <= # <= uMax &
      ];


    candidateScored =
      evalU /@
        candU;


    allTrace =
      Join[
        allTrace,
        candidateScored
      ];


    (* Incumbent-preserving maximization:
       the previous best remains an explicit candidate even if
       floating-point grid construction were to miss its u value.
       Hence the objective value cannot decrease during
       refinement. *)

    best =
      First @
        SortBy[
          Join[
            {
              best
            },
            candidateScored
          ],
          scoreKey
        ];


    uBest =
      best["u"];


    win = {
      Max[
        uMin,
        uBest - step
      ],

      Min[
        uMax,
        uBest + step
      ]
    },

    {
      refineSteps
    }
  ];


  (* ---------------------------------------------------------- *)
  (* Final calibrated geometry                                  *)
  (* ---------------------------------------------------------- *)

  finalDetails =
    CalibrationEvaluation[
      A,
      best["p"],
      legitimacySignal
    ];


  If[
    finalDetails === $Failed ||
    !AssociationQ[finalDetails] ||
    !KeyExistsQ[
      finalDetails,
      "areaSigned"
    ] ||
    !NumericQ[
      finalDetails["areaSigned"]
    ] ||
    !KeyExistsQ[
      finalDetails,
      "frontSize"
    ],

    Return[
      $Failed
    ]
  ];


  finalArea =
    N[
      finalDetails["areaSigned"]
    ];


  finalFrontSize =
    finalDetails["frontSize"];


  (* Final consistency guard.
     A calibrated result must have genuinely positive area and
     therefore a nondegenerate Pareto front with at least three
     distinct geometric points. *)

  If[
    finalArea <= eps ||
    !IntegerQ[finalFrontSize] ||
    finalFrontSize < 3,

    Return[
      degenerateOutput[]
    ]
  ];


  status =
    "calibrated";


  (* ---------------------------------------------------------- *)
  (* Output                                                     *)
  (* ---------------------------------------------------------- *)

  <|
    "status" ->
      status,

    "legitimacySignal" ->
      legitimacySignal,

    "pStar" ->
      best["p"],

    "uStar" ->
      best["u"],

    "areaStar" ->
      finalArea,

    "frontSizeAtPStar" ->
      finalFrontSize,

    "frontAtPStar" ->
      finalDetails["front"],

    "detailsAtPStar" ->
      finalDetails,

    "searchParameters" -> <|
      "pMin" -> pMin,
      "pMax" -> pMax,
      "initialGridSize" -> Length[gridU],
      "refineSteps" -> refineSteps,
      "refineFactor" -> refineFactor,
      "initialRefineHalfWidthU" -> halfWidth,
      "tieBreak" -> tieBreak,
      "degenerateEps" -> eps
    |>,

    "searchTrace" ->
      If[
        keepTrace,

        DeleteDuplicatesBy[
          allTrace,
          Lookup[
            #,
            "u"
          ] &
        ],

        Missing[
          "NotRequested"
        ]
      ]
  |>
];