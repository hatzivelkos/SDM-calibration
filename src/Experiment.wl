(* ::Package:: *)

(* ============================================================ *)
(* File: src/Experiment.wl                                     *)
(* Purpose: Reproducible simulation experiment runner           *)
(*                                                              *)
(* Requires:                                                    *)
(*   Profile.wl                                                 *)
(*   Signals.wl                                                 *)
(*   Pareto.wl                                                  *)
(*   Geometry.wl                                                *)
(*   Calibration.wl                                             *)
(*   Regimes.wl                                                 *)
(*   Degenerate.wl                                              *)
(*   Generators.wl                                              *)
(*                                                              *)
(* The module combines conceptually distinct components:        *)
(*                                                              *)
(*   profile generation                                         *)
(*   geometric-regime classification                            *)
(*   signed-area calibration                                    *)
(*   auxiliary diagnostics for degenerate instances             *)
(*                                                              *)
(* Geometric regime and calibration status are always recorded  *)
(* separately. Degenerate diagnostics never replace pStar.      *)
(* ============================================================ *)


ClearAll[
  SDMWinnerAtP,
  AnalyzeProfile,
  RunTwoPoleExperimentCell,
  RunTwoPoleExperimentGrid,
  AggregateExperimentRows
];


(* ------------------------------------------------------------ *)
(* SDM winner at a fixed finite p                               *)
(* ------------------------------------------------------------ *)

SDMWinnerAtP::finite =
  "SDMWinnerAtP requires a finite parameter p >= 1.";

SDMWinnerAtP::signal =
  "Could not evaluate legitimacy signal `1`.";


(* Winner:
     1. minimize d_p
     2. among ties, maximize legitimacy
     3. among remaining ties, minimize candidate index

   The legitimacy tie-break uses the same legitimacy signal as
   the calibration geometry.
*)

SDMWinnerAtP[
  A_?ProfileQ,
  p_,
  legitimacySignal_: "TopSupport"
] := Module[
  {
    d,
    L,
    m,
    minD,
    minSet
  },

  If[
    p === Infinity ||
    !NumericQ[p] ||
    p < 1,

    Message[
      SDMWinnerAtP::finite
    ];

    Return[$Failed]
  ];


  d =
    DpVector[
      A,
      p
    ];


  L =
    LegitimacyVector[
      A,
      legitimacySignal
    ];


  If[
    d === $Failed ||
    L === $Failed,

    Message[
      SDMWinnerAtP::signal,
      legitimacySignal
    ];

    Return[$Failed]
  ];


  m =
    CandidateCount[A];


  minD =
    Min[d];


  minSet =
    Select[
      Range[m],
      TrueQ[
        d[[#]] == minD
      ] &
    ];


  First @
    SortBy[
      minSet,
      {
        -L[[#]],
        #
      } &
    ]
];


(* ------------------------------------------------------------ *)
(* Analysis of one profile                                      *)
(* ------------------------------------------------------------ *)

Options[AnalyzeProfile] = {

  (* legitimacy specification *)
  "LegitimacySignal" -> "TopSupport",

  (* calibration/regime search range *)
  "pMin" -> 1.,
  "pMax" -> 20.,

  (* fixed initial classification/calibration grid *)
  "InitialGridSize" -> 26,
  "GridU" -> Automatic,

  (* calibration refinement *)
  "RefineSteps" -> 3,
  "RefineFactor" -> 6,
  "InitialRefineHalfWidthU" -> 0.1,

  (* calibration conventions *)
  "TieBreak" -> "SmallerP",
  "DegenerateEps" -> 10^-12,

  (* retained diagnostics *)
  "KeepCalibrationTrace" -> False,
  "KeepRegimeDetails" -> False,

  (* auxiliary diagnostic for degenerate profiles *)
  "IncludeDegenerateDiagnostic" -> True,

  "DiagnosticFinitePMax" -> 40.,
  "DiagnosticGridIntervals" -> 40,
  "DiagnosticRefineSteps" -> 4,
  "DiagnosticRefineFactor" -> 8,
  "DiagnosticIncludeInfinity" -> True
};


AnalyzeProfile[
  A_?ProfileQ,
  opts : OptionsPattern[]
] := Module[
  {
    legitimacySignal,

    pMin,
    pMax,
    initialGridSize,
    gridU,

    refineSteps,
    refineFactor,
    halfWidth,
    tieBreak,
    eps,

    keepCalibrationTrace,
    keepRegimeDetails,

    includeDegenerateDiagnostic,
    diagnosticFinitePMax,
    diagnosticGridIntervals,
    diagnosticRefineSteps,
    diagnosticRefineFactor,
    diagnosticIncludeInfinity,

    regime,
    calibration,

    status,
    pStar,
    areaStar,
    winner,
    frontSizeAtPStar,

    degenerateDiagnostic,
    firstSwitch,

    winnerAtPMin,
    winnerAtInfinity,
    firstWinnerSwitchP,
    firstWinnerSwitchStatus,
    firstWinnerSwitchWinner
  },


  (* ---------------------------------------------------------- *)
  (* Options                                                    *)
  (* ---------------------------------------------------------- *)

  legitimacySignal =
    OptionValue[
      "LegitimacySignal"
    ];


  pMin =
    OptionValue[
      "pMin"
    ];

  pMax =
    OptionValue[
      "pMax"
    ];


  initialGridSize =
    OptionValue[
      "InitialGridSize"
    ];

  gridU =
    OptionValue[
      "GridU"
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
    OptionValue[
      "InitialRefineHalfWidthU"
    ];


  tieBreak =
    OptionValue[
      "TieBreak"
    ];

  eps =
    OptionValue[
      "DegenerateEps"
    ];


  keepCalibrationTrace =
    TrueQ[
      OptionValue[
        "KeepCalibrationTrace"
      ]
    ];

  keepRegimeDetails =
    TrueQ[
      OptionValue[
        "KeepRegimeDetails"
      ]
    ];


  includeDegenerateDiagnostic =
    TrueQ[
      OptionValue[
        "IncludeDegenerateDiagnostic"
      ]
    ];


  diagnosticFinitePMax =
    OptionValue[
      "DiagnosticFinitePMax"
    ];

  diagnosticGridIntervals =
    OptionValue[
      "DiagnosticGridIntervals"
    ];

  diagnosticRefineSteps =
    OptionValue[
      "DiagnosticRefineSteps"
    ];

  diagnosticRefineFactor =
    OptionValue[
      "DiagnosticRefineFactor"
    ];

  diagnosticIncludeInfinity =
    TrueQ[
      OptionValue[
        "DiagnosticIncludeInfinity"
      ]
    ];


  (* ---------------------------------------------------------- *)
  (* Geometric regime                                           *)
  (* ---------------------------------------------------------- *)

  regime =
    GeometricRegimeSummary[
      A,

      "pMin" ->
        pMin,

      "pMax" ->
        pMax,

      "GridSize" ->
        initialGridSize,

      "GridU" ->
        gridU,

      "LegitimacySignal" ->
        legitimacySignal,

      "KeepFronts" ->
        keepRegimeDetails
    ];


  If[
    regime === $Failed,
    Return[$Failed]
  ];


  (* ---------------------------------------------------------- *)
  (* Signed-area calibration                                    *)
  (* ---------------------------------------------------------- *)

  calibration =
    CalibrateP[
      A,

      "pMin" ->
        pMin,

      "pMax" ->
        pMax,

      "LegitimacySignal" ->
        legitimacySignal,

      "InitialGridSize" ->
        initialGridSize,

      "GridU" ->
        gridU,

      "RefineSteps" ->
        refineSteps,

      "RefineFactor" ->
        refineFactor,

      "InitialRefineHalfWidthU" ->
        halfWidth,

      "TieBreak" ->
        tieBreak,

      "DegenerateEps" ->
        eps,

      "KeepTrace" ->
        keepCalibrationTrace
    ];


  If[
    calibration === $Failed,
    Return[$Failed]
  ];


  status =
    calibration[
      "status"
    ];


  (* ---------------------------------------------------------- *)
  (* Calibrated profile                                         *)
  (* ---------------------------------------------------------- *)

  If[
    status === "calibrated",

    pStar =
      calibration[
        "pStar"
      ];

    areaStar =
      calibration[
        "areaStar"
      ];

    frontSizeAtPStar =
      calibration[
        "frontSizeAtPStar"
      ];


    winner =
      SDMWinnerAtP[
        A,
        pStar,
        legitimacySignal
      ];


    If[
      winner === $Failed,
      Return[$Failed]
    ];


    (* Degenerate diagnostics are not applicable here. *)

    degenerateDiagnostic =
      Missing[
        "NotApplicable"
      ];

    winnerAtPMin =
      Missing[
        "NotApplicable"
      ];

    winnerAtInfinity =
      Missing[
        "NotApplicable"
      ];

    firstWinnerSwitchP =
      Missing[
        "NotApplicable"
      ];

    firstWinnerSwitchStatus =
      Missing[
        "NotApplicable"
      ];

    firstWinnerSwitchWinner =
      Missing[
        "NotApplicable"
      ],


    (* -------------------------------------------------------- *)
    (* Degenerate calibration outcome                           *)
    (* -------------------------------------------------------- *)

    pStar =
      Missing[
        "Degenerate"
      ];

    areaStar =
      Missing[
        "Degenerate"
      ];

    frontSizeAtPStar =
      Missing[
        "Degenerate"
      ];

    winner =
      Missing[
        "Degenerate"
      ];


    If[
      includeDegenerateDiagnostic,

      degenerateDiagnostic =
        DegenerateDiagnostic[
          A,

          "pMin" ->
            pMin,

          "FinitePMax" ->
            diagnosticFinitePMax,

          "GridIntervals" ->
            diagnosticGridIntervals,

          "RefineSteps" ->
            diagnosticRefineSteps,

          "RefineFactor" ->
            diagnosticRefineFactor,

          "LegitimacySignal" ->
            legitimacySignal,

          "IncludeInfinity" ->
            diagnosticIncludeInfinity
        ];


      If[
        degenerateDiagnostic === $Failed,
        Return[$Failed]
      ];


      firstSwitch =
        Lookup[
          degenerateDiagnostic,
          "firstWinnerSwitch",
          <||>
        ];


      winnerAtPMin =
        Lookup[
          degenerateDiagnostic,
          "winnerAtPMin",
          Missing["NotAvailable"]
        ];


      winnerAtInfinity =
        Lookup[
          degenerateDiagnostic,
          "winnerAtInfinity",
          Missing["NotAvailable"]
        ];


      firstWinnerSwitchP =
        If[
          AssociationQ[firstSwitch],

          Lookup[
            firstSwitch,
            "pSwitch",
            Missing["NotAvailable"]
          ],

          Missing["NotAvailable"]
        ];


      firstWinnerSwitchStatus =
        If[
          AssociationQ[firstSwitch],

          Lookup[
            firstSwitch,
            "status",
            Missing["NotAvailable"]
          ],

          Missing["NotAvailable"]
        ];


      firstWinnerSwitchWinner =
        If[
          AssociationQ[firstSwitch],

          Lookup[
            firstSwitch,
            "winnerAfterSwitch",
            Missing["NotAvailable"]
          ],

          Missing["NotAvailable"]
        ],


      (* diagnostic explicitly disabled *)

      degenerateDiagnostic =
        Missing[
          "NotRequested"
        ];

      winnerAtPMin =
        Missing[
          "NotRequested"
        ];

      winnerAtInfinity =
        Missing[
          "NotRequested"
        ];

      firstWinnerSwitchP =
        Missing[
          "NotRequested"
        ];

      firstWinnerSwitchStatus =
        Missing[
          "NotRequested"
        ];

      firstWinnerSwitchWinner =
        Missing[
          "NotRequested"
        ]
    ]
  ];


  (* ---------------------------------------------------------- *)
  (* Output                                                     *)
  (* ---------------------------------------------------------- *)

  <|

    (* profile dimensions *)

    "n" ->
      VoterCount[A],

    "m" ->
      CandidateCount[A],


    (* legitimacy specification *)

    "legitimacySignal" ->
      legitimacySignal,


    (* geometric regime *)

    "geometricRegime" ->
      regime[
        "regime"
      ],

    "regimeNumber" ->
      regime[
        "regimeNumber"
      ],

    "maxFrontSize" ->
      regime[
        "maxFrontSize"
      ],

    "frontSizeAtP1" ->
      regime[
        "frontSizeAtP1"
      ],

    "regimeAtP1" ->
      regime[
        "regimeAtP1"
      ],


    (* calibration status *)

    "calibrationStatus" ->
      status,

    "pStar" ->
      pStar,

    "areaStar" ->
      areaStar,

    "frontSizeAtPStar" ->
      frontSizeAtPStar,

    "selectedCandidate" ->
      winner,


    (* auxiliary degenerate diagnostics *)

    "winnerAtPMin" ->
      winnerAtPMin,

    "winnerAtInfinity" ->
      winnerAtInfinity,

    "firstWinnerSwitchP" ->
      firstWinnerSwitchP,

    "firstWinnerSwitchStatus" ->
      firstWinnerSwitchStatus,

    "firstWinnerSwitchWinner" ->
      firstWinnerSwitchWinner,


    (* component diagnostics *)

    "regimeSummary" ->
      regime,

    "calibrationSummary" ->
      calibration,

    "degenerateDiagnostic" ->
      degenerateDiagnostic

  |>
];


(* ------------------------------------------------------------ *)
(* One experimental parameter cell                              *)
(* ------------------------------------------------------------ *)

Options[RunTwoPoleExperimentCell] =
  Options[
    AnalyzeProfile
  ];


(* This function does not set its own seed.

   Seed control belongs to RunTwoPoleExperimentGrid, so that the
   complete experiment uses one continuous reproducible random
   stream across all parameter cells.
*)

RunTwoPoleExperimentCell[
  reps_Integer?Positive,
  n_Integer?Positive,
  m_Integer?Positive,
  lambda_?NumericQ,
  eta_Integer?NonNegative,
  opts : OptionsPattern[]
] /; m >= 2 := Module[
  {
    profiles,
    analysisOpts,
    rows
  },


  profiles =
    GenerateTwoPoleSample[
      reps,
      n,
      m,
      lambda,
      eta,

      "Seed" ->
        Automatic,

      "ReturnMetadata" ->
        False
    ];


  If[
    profiles === $Failed,
    Return[$Failed]
  ];


  analysisOpts =
    FilterRules[
      {opts},
      Options[
        AnalyzeProfile
      ]
    ];


  rows =
    MapIndexed[
      Function[
        {A, idx},

        Module[
          {analysis},

          analysis =
            AnalyzeProfile[
              A,
              Sequence @@ analysisOpts
            ];


          If[
            analysis === $Failed,
            Return[$Failed]
          ];


          Join[
            <|
              "model" ->
                "TwoPole",

              "rep" ->
                idx[[1]],

              "lambda" ->
                N[lambda],

              "eta" ->
                eta
            |>,
            analysis
          ]
        ]
      ],
      profiles
    ];


  If[
    MemberQ[
      rows,
      $Failed
    ],
    Return[$Failed]
  ];


  rows
];


(* ------------------------------------------------------------ *)
(* Full experimental grid                                       *)
(* ------------------------------------------------------------ *)

Options[RunTwoPoleExperimentGrid] =
  Join[
    Options[
      AnalyzeProfile
    ],
    {
      "Seed" -> 123456
    }
  ];


RunTwoPoleExperimentGrid::seed =
  "Seed must be an integer or Automatic.";

RunTwoPoleExperimentGrid::grid =
  "The experiment parameter lists must be nonempty.";


(* A single seed controls the complete experiment.

   The random generator is not restarted for individual parameter
   cells. All profiles in the complete grid therefore belong to
   one reproducible Monte Carlo stream.

   BlockRandom localizes this stream so that running the complete
   experiment does not alter the external Wolfram Language random
   state.
*)

RunTwoPoleExperimentGrid[
  reps_Integer?Positive,
  n_Integer?Positive,
  mList_List,
  lambdaList_List,
  etaList_List,
  opts : OptionsPattern[]
] := Module[
  {
    seed,
    analysisOpts,
    run,
    rows
  },


  seed =
    OptionValue[
      "Seed"
    ];


  If[
    !(
      seed === Automatic ||
      IntegerQ[seed]
    ),

    Message[
      RunTwoPoleExperimentGrid::seed
    ];

    Return[$Failed]
  ];


  If[
    mList === {} ||
    lambdaList === {} ||
    etaList === {},

    Message[
      RunTwoPoleExperimentGrid::grid
    ];

    Return[$Failed]
  ];


  analysisOpts =
    FilterRules[
      {opts},
      Options[
        AnalyzeProfile
      ]
    ];


  run[] :=
    Flatten[
      Table[

        RunTwoPoleExperimentCell[
          reps,
          n,
          m,
          lambda,
          eta,
          Sequence @@ analysisOpts
        ],

        {m, mList},
        {lambda, lambdaList},
        {eta, etaList}
      ],

      3
    ];


  rows =
    If[
      seed === Automatic,

      run[],

      BlockRandom[
        SeedRandom[seed];
        run[]
      ]
    ];


  If[
    !ListQ[rows] ||
    MemberQ[
      rows,
      $Failed
    ],

    Return[$Failed]
  ];


  (* Attach experiment-level seed to every raw row. *)

  Map[
    Join[
      <|
        "experimentSeed" ->
          seed
      |>,
      #
    ] &,
    rows
  ]
];


(* ------------------------------------------------------------ *)
(* Aggregate summaries                                          *)
(* ------------------------------------------------------------ *)

AggregateExperimentRows[
  rows_List
] := Module[
  {
    nRows,

    regimeCounts,
    calibrationCounts,

    calibratedRows,
    degenerateRows,

    pStars,
    areas,
    maxFrontSizes,

    switchPs,
    nDegenerateWithSwitch
  },


  If[
    rows === {},

    Return[
      <|
        "nProfiles" -> 0
      |>
    ]
  ];


  nRows =
    Length[
      rows
    ];


  regimeCounts =
    Counts[
      Lookup[
        rows,
        "geometricRegime",
        Missing["NoRegime"]
      ]
    ];


  calibrationCounts =
    Counts[
      Lookup[
        rows,
        "calibrationStatus",
        Missing["NoStatus"]
      ]
    ];


  calibratedRows =
    Select[
      rows,

      Lookup[
        #,
        "calibrationStatus",
        Missing["NoStatus"]
      ] === "calibrated" &
    ];


  degenerateRows =
    Select[
      rows,

      Lookup[
        #,
        "calibrationStatus",
        Missing["NoStatus"]
      ] === "degenerate" &
    ];


  pStars =
    Select[
      DeleteMissing[
        Lookup[
          calibratedRows,
          "pStar",
          Missing["NoPStar"]
        ]
      ],
      NumericQ
    ];


  areas =
    Select[
      DeleteMissing[
        Lookup[
          calibratedRows,
          "areaStar",
          Missing["NoArea"]
        ]
      ],
      NumericQ
    ];


  maxFrontSizes =
    Select[
      DeleteMissing[
        Lookup[
          rows,
          "maxFrontSize",
          Missing["NoFrontSize"]
        ]
      ],
      NumericQ
    ];


  switchPs =
    Select[
      DeleteMissing[
        Lookup[
          degenerateRows,
          "firstWinnerSwitchP",
          Missing["NoSwitch"]
        ]
      ],
      NumericQ
    ];


  nDegenerateWithSwitch =
    Count[
      Lookup[
        degenerateRows,
        "firstWinnerSwitchStatus",
        Missing["NoSwitchStatus"]
      ],
      "SwitchDetected" |
      "SwitchDetectedAtInfinity"
    ];


  <|

    "nProfiles" ->
      nRows,


    (* geometric regimes *)

    "geometricRegimeCounts" ->
      regimeCounts,

    "geometricRegimeFrequencies" ->
      Map[
        N[#/nRows] &,
        regimeCounts
      ],


    (* calibration status *)

    "calibrationStatusCounts" ->
      calibrationCounts,

    "calibrationStatusFrequencies" ->
      Map[
        N[#/nRows] &,
        calibrationCounts
      ],

    "nCalibrated" ->
      Length[
        calibratedRows
      ],

    "nDegenerate" ->
      Length[
        degenerateRows
      ],

    "calibratedFrequency" ->
      N[
        Length[calibratedRows] /
        nRows
      ],


    (* calibrated p summaries *)

    "medianPStar" ->
      If[
        pStars === {},

        Missing[
          "NoData"
        ],

        N[
          Median[
            pStars
          ]
        ]
      ],

    "meanPStar" ->
      If[
        pStars === {},

        Missing[
          "NoData"
        ],

        N[
          Mean[
            pStars
          ]
        ]
      ],


    (* signed-area summary *)

    "medianAreaStar" ->
      If[
        areas === {},

        Missing[
          "NoData"
        ],

        N[
          Median[
            areas
          ]
        ]
      ],


    (* Pareto-front richness *)

    "meanMaxFrontSize" ->
      If[
        maxFrontSizes === {},

        Missing[
          "NoData"
        ],

        N[
          Mean[
            maxFrontSizes
          ]
        ]
      ],


    (* degenerate threshold diagnostics *)

    "nDegenerateWithWinnerSwitch" ->
      nDegenerateWithSwitch,

    "winnerSwitchFrequencyGivenDegenerate" ->
      If[
        degenerateRows === {},

        Missing[
          "NoDegenerateProfiles"
        ],

        N[
          nDegenerateWithSwitch /
          Length[degenerateRows]
        ]
      ],

    "medianFirstWinnerSwitchP" ->
      If[
        switchPs === {},

        Missing[
          "NoFiniteSwitch"
        ],

        N[
          Median[
            switchPs
          ]
        ]
      ]

  |>
];