(* ::Package:: *)

(* ============================================================ *)
(* File: src/Tables.wl                                         *)
(* Purpose: Experiment postprocessing and table export          *)
(*                                                              *)
(* Requires:                                                    *)
(*   Experiment.wl                                              *)
(*                                                              *)
(* This module does not recompute profiles, geometry, regimes,  *)
(* calibration, or degenerate diagnostics. It only summarizes   *)
(* and exports results already produced by Experiment.wl.       *)
(* ============================================================ *)


ClearAll[
  NumericValuesOnly,
  MeanOrMissing,
  MedianOrMissing,
  QuantileOrMissing,
  CSVValue,

  RawExperimentRow,
  RawExperimentTable,

  ExperimentCellSummary,
  SummarizeExperimentCells,
  CellSummaryTable,

  ExperimentRowsToDataset,
  CellSummariesToDataset,

  ExportRawExperimentCSV,
  ExportCellSummaryCSV
];


(* ------------------------------------------------------------ *)
(* Basic helpers                                                *)
(* ------------------------------------------------------------ *)

NumericValuesOnly[x_List] :=
  Select[
    DeleteMissing[x],
    NumericQ
  ];


MeanOrMissing[x_List] := Module[
  {v},

  v =
    NumericValuesOnly[x];

  If[
    v === {},
    Missing["NoData"],
    N[
      Mean[v]
    ]
  ]
];


MedianOrMissing[x_List] := Module[
  {v},

  v =
    NumericValuesOnly[x];

  If[
    v === {},
    Missing["NoData"],
    N[
      Median[v]
    ]
  ]
];


QuantileOrMissing[
  x_List,
  q_?NumericQ
] := Module[
  {v},

  v =
    NumericValuesOnly[x];

  If[
    v === {},
    Missing["NoData"],
    N[
      Quantile[
        v,
        q
      ]
    ]
  ]
];


(* Convert values to simple CSV-compatible scalar entries. *)

CSVValue[x_] :=
  Which[
    MissingQ[x],
      "",

    x === Infinity,
      "Infinity",

    x === -Infinity,
      "-Infinity",

    True,
      x
  ];


(* ------------------------------------------------------------ *)
(* Flat raw experiment rows                                     *)
(* ------------------------------------------------------------ *)

(* The published raw table contains only scalar fields.

   Nested diagnostic associations such as regimeSummary,
   calibrationSummary, and degenerateDiagnostic are deliberately
   omitted. They remain available in the in-memory experiment
   results but are not required for the reproducibility CSV.
*)

RawExperimentRow[
  row_Association
] :=
  <|

    (* experiment metadata *)

    "experimentSeed" ->
      Lookup[
        row,
        "experimentSeed",
        Missing["NotAvailable"]
      ],

    "model" ->
      Lookup[
        row,
        "model",
        Missing["NotAvailable"]
      ],

    "rep" ->
      Lookup[
        row,
        "rep",
        Missing["NotAvailable"]
      ],

    "n" ->
      Lookup[
        row,
        "n",
        Missing["NotAvailable"]
      ],

    "m" ->
      Lookup[
        row,
        "m",
        Missing["NotAvailable"]
      ],

    "lambda" ->
      Lookup[
        row,
        "lambda",
        Missing["NotAvailable"]
      ],

    "eta" ->
      Lookup[
        row,
        "eta",
        Missing["NotAvailable"]
      ],


    (* legitimacy specification *)

    "legitimacySignal" ->
      Lookup[
        row,
        "legitimacySignal",
        Missing["NotAvailable"]
      ],


    (* geometric regime *)

    "geometricRegime" ->
      Lookup[
        row,
        "geometricRegime",
        Missing["NotAvailable"]
      ],

    "regimeNumber" ->
      Lookup[
        row,
        "regimeNumber",
        Missing["NotAvailable"]
      ],

    "maxFrontSize" ->
      Lookup[
        row,
        "maxFrontSize",
        Missing["NotAvailable"]
      ],

    "regimeAtP1" ->
      Lookup[
        row,
        "regimeAtP1",
        Missing["NotAvailable"]
      ],

    "frontSizeAtP1" ->
      Lookup[
        row,
        "frontSizeAtP1",
        Missing["NotAvailable"]
      ],


    (* calibration *)

    "calibrationStatus" ->
      Lookup[
        row,
        "calibrationStatus",
        Missing["NotAvailable"]
      ],

    "pStar" ->
      Lookup[
        row,
        "pStar",
        Missing["NotAvailable"]
      ],

    "areaStar" ->
      Lookup[
        row,
        "areaStar",
        Missing["NotAvailable"]
      ],

    "frontSizeAtPStar" ->
      Lookup[
        row,
        "frontSizeAtPStar",
        Missing["NotAvailable"]
      ],

    "selectedCandidate" ->
      Lookup[
        row,
        "selectedCandidate",
        Missing["NotAvailable"]
      ],


    (* auxiliary diagnostics for degenerate profiles *)

    "winnerAtPMin" ->
      Lookup[
        row,
        "winnerAtPMin",
        Missing["NotAvailable"]
      ],

    "winnerAtInfinity" ->
      Lookup[
        row,
        "winnerAtInfinity",
        Missing["NotAvailable"]
      ],

    "firstWinnerSwitchP" ->
      Lookup[
        row,
        "firstWinnerSwitchP",
        Missing["NotAvailable"]
      ],

    "firstWinnerSwitchStatus" ->
      Lookup[
        row,
        "firstWinnerSwitchStatus",
        Missing["NotAvailable"]
      ],

    "firstWinnerSwitchWinner" ->
      Lookup[
        row,
        "firstWinnerSwitchWinner",
        Missing["NotAvailable"]
      ]

  |>;


RawExperimentTable[
  rows_List
] :=
  RawExperimentRow /@
    rows;


(* ------------------------------------------------------------ *)
(* Summary of one experimental cell                             *)
(* ------------------------------------------------------------ *)

ExperimentCellSummary::empty =
  "Cannot summarize an empty experiment cell.";


ExperimentCellSummary[
  rows_List
] := Module[
  {
    first,
    nRows,

    regimes,
    statuses,

    regimeCounts,
    statusCounts,

    calibratedRows,
    degenerateRows,
    multiRows,
    multiDegenerateRows,

    pStars,
    areas,
    maxFrontSizes,

    switchStatuses,
    switchPs,

    nOne,
    nTwo,
    nMulti,

    nCalibrated,
    nDegenerate,

    nMultiDegenerate,

    nDegenerateWithSwitch,
    nDegenerateWithFiniteSwitch,
    nDegenerateWithInfinitySwitch
  },


  If[
    rows === {},

    Message[
      ExperimentCellSummary::empty
    ];

    Return[$Failed]
  ];


  first =
    First[
      rows
    ];

  nRows =
    Length[
      rows
    ];


  (* ---------------------------------------------------------- *)
  (* Geometric regimes                                          *)
  (* ---------------------------------------------------------- *)

  regimes =
    Lookup[
      rows,
      "geometricRegime",
      Missing["NoRegime"]
    ];


  regimeCounts =
    Counts[
      regimes
    ];


  nOne =
    Lookup[
      regimeCounts,
      "one-point",
      0
    ];

  nTwo =
    Lookup[
      regimeCounts,
      "two-point",
      0
    ];

  nMulti =
    Lookup[
      regimeCounts,
      "multi-point",
      0
    ];


  (* ---------------------------------------------------------- *)
  (* Calibration status                                         *)
  (* ---------------------------------------------------------- *)

  statuses =
    Lookup[
      rows,
      "calibrationStatus",
      Missing["NoStatus"]
    ];


  statusCounts =
    Counts[
      statuses
    ];


  nCalibrated =
    Lookup[
      statusCounts,
      "calibrated",
      0
    ];

  nDegenerate =
    Lookup[
      statusCounts,
      "degenerate",
      0
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


  (* ---------------------------------------------------------- *)
  (* Bridge: geometric regime vs calibration status             *)
  (* ---------------------------------------------------------- *)

  multiRows =
    Select[
      rows,

      Lookup[
        #,
        "geometricRegime",
        Missing["NoRegime"]
      ] === "multi-point" &
    ];


  multiDegenerateRows =
    Select[
      multiRows,

      Lookup[
        #,
        "calibrationStatus",
        Missing["NoStatus"]
      ] === "degenerate" &
    ];


  nMultiDegenerate =
    Length[
      multiDegenerateRows
    ];


  (* ---------------------------------------------------------- *)
  (* Calibrated-profile values                                  *)
  (* ---------------------------------------------------------- *)

  pStars =
    Lookup[
      calibratedRows,
      "pStar",
      Missing["NoPStar"]
    ];


  areas =
    Lookup[
      calibratedRows,
      "areaStar",
      Missing["NoArea"]
    ];


  maxFrontSizes =
    Lookup[
      rows,
      "maxFrontSize",
      Missing["NoFrontSize"]
    ];


  (* ---------------------------------------------------------- *)
  (* Degenerate winner-switch diagnostics                       *)
  (* ---------------------------------------------------------- *)

  switchStatuses =
    Lookup[
      degenerateRows,
      "firstWinnerSwitchStatus",
      Missing["NoSwitchStatus"]
    ];


  nDegenerateWithFiniteSwitch =
    Count[
      switchStatuses,
      "SwitchDetected"
    ];


  nDegenerateWithInfinitySwitch =
    Count[
      switchStatuses,
      "SwitchDetectedAtInfinity"
    ];


  nDegenerateWithSwitch =
    nDegenerateWithFiniteSwitch +
    nDegenerateWithInfinitySwitch;


  (* NumericQ automatically excludes Infinity, so this vector
     contains only finite detected switching thresholds. *)

  switchPs =
    NumericValuesOnly[
      Lookup[
        degenerateRows,
        "firstWinnerSwitchP",
        Missing["NoSwitchP"]
      ]
    ];


  (* ---------------------------------------------------------- *)
  (* Output                                                     *)
  (* ---------------------------------------------------------- *)

  <|

    (* parameter-cell metadata *)

    "experimentSeed" ->
      Lookup[
        first,
        "experimentSeed",
        Missing["NotAvailable"]
      ],

    "model" ->
      Lookup[
        first,
        "model",
        Missing["NotAvailable"]
      ],

    "n" ->
      Lookup[
        first,
        "n",
        Missing["NotAvailable"]
      ],

    "m" ->
      Lookup[
        first,
        "m",
        Missing["NotAvailable"]
      ],

    "lambda" ->
      Lookup[
        first,
        "lambda",
        Missing["NotAvailable"]
      ],

    "eta" ->
      Lookup[
        first,
        "eta",
        Missing["NotAvailable"]
      ],

    "legitimacySignal" ->
      Lookup[
        first,
        "legitimacySignal",
        Missing["NotAvailable"]
      ],

    "reps" ->
      nRows,


    (* geometric regimes *)

    "nOnePoint" ->
      nOne,

    "nTwoPoint" ->
      nTwo,

    "nMultiPoint" ->
      nMulti,

    "onePointFrequency" ->
      N[
        nOne / nRows
      ],

    "twoPointFrequency" ->
      N[
        nTwo / nRows
      ],

    "multiPointFrequency" ->
      N[
        nMulti / nRows
      ],

    "meanMaxFrontSize" ->
      MeanOrMissing[
        maxFrontSizes
      ],


    (* calibration status *)

    "nCalibrated" ->
      nCalibrated,

    "nDegenerate" ->
      nDegenerate,

    "calibratedFrequency" ->
      N[
        nCalibrated / nRows
      ],

    "degenerateFrequency" ->
      N[
        nDegenerate / nRows
      ],


    (* geometric-regime / calibration bridge *)

    "nMultiPointDegenerate" ->
      nMultiDegenerate,

    "multiPointDegenerateFrequency" ->
      N[
        nMultiDegenerate / nRows
      ],

    "calibratedGivenMultiPoint" ->
      If[
        nMulti == 0,

        Missing[
          "NoMultiPoint"
        ],

        N[
          (
            nMulti -
            nMultiDegenerate
          ) /
          nMulti
        ]
      ],


    (* pStar distribution over calibrated profiles *)

    "meanPStar" ->
      MeanOrMissing[
        pStars
      ],

    "medianPStar" ->
      MedianOrMissing[
        pStars
      ],

    "q1PStar" ->
      QuantileOrMissing[
        pStars,
        1/4
      ],

    "q3PStar" ->
      QuantileOrMissing[
        pStars,
        3/4
      ],

    "minPStar" ->
      Module[
        {v},

        v =
          NumericValuesOnly[
            pStars
          ];

        If[
          v === {},
          Missing["NoData"],
          Min[v]
        ]
      ],

    "maxPStar" ->
      Module[
        {v},

        v =
          NumericValuesOnly[
            pStars
          ];

        If[
          v === {},
          Missing["NoData"],
          Max[v]
        ]
      ],


    (* signed-area distribution over calibrated profiles *)

    "meanAreaStar" ->
      MeanOrMissing[
        areas
      ],

    "medianAreaStar" ->
      MedianOrMissing[
        areas
      ],


    (* degenerate winner-switch diagnostics *)

    "nDegenerateWithWinnerSwitch" ->
      nDegenerateWithSwitch,

    "nDegenerateWithFiniteWinnerSwitch" ->
      nDegenerateWithFiniteSwitch,

    "nDegenerateWithInfinityWinnerSwitch" ->
      nDegenerateWithInfinitySwitch,

    "winnerSwitchFrequencyGivenDegenerate" ->
      If[
        nDegenerate == 0,

        Missing[
          "NoDegenerateProfiles"
        ],

        N[
          nDegenerateWithSwitch /
          nDegenerate
        ]
      ],

    "finiteWinnerSwitchFrequencyGivenDegenerate" ->
      If[
        nDegenerate == 0,

        Missing[
          "NoDegenerateProfiles"
        ],

        N[
          nDegenerateWithFiniteSwitch /
          nDegenerate
        ]
      ],

    "meanFirstWinnerSwitchP" ->
      MeanOrMissing[
        switchPs
      ],

    "medianFirstWinnerSwitchP" ->
      MedianOrMissing[
        switchPs
      ],

    "q1FirstWinnerSwitchP" ->
      QuantileOrMissing[
        switchPs,
        1/4
      ],

    "q3FirstWinnerSwitchP" ->
      QuantileOrMissing[
        switchPs,
        3/4
      ]

  |>
];


(* ------------------------------------------------------------ *)
(* Summaries over all parameter cells                           *)
(* ------------------------------------------------------------ *)

(* A cell is uniquely identified by:
     experimentSeed,
     model,
     n,
     m,
     lambda,
     eta,
     legitimacySignal.

   This will also allow future robustness results using different
   legitimacy signals to coexist in the same result collection.
*)

SummarizeExperimentCells[
  rows_List
] := Module[
  {
    cellKey,
    groups,
    summaries
  },


  If[
    rows === {},
    Return[{}]
  ];


  cellKey[
    row_Association
  ] :=
    {
      Lookup[
        row,
        "experimentSeed",
        Missing["NotAvailable"]
      ],

      Lookup[
        row,
        "model",
        Missing["NotAvailable"]
      ],

      Lookup[
        row,
        "n",
        Missing["NotAvailable"]
      ],

      Lookup[
        row,
        "m",
        Missing["NotAvailable"]
      ],

      Lookup[
        row,
        "lambda",
        Missing["NotAvailable"]
      ],

      Lookup[
        row,
        "eta",
        Missing["NotAvailable"]
      ],

      Lookup[
        row,
        "legitimacySignal",
        Missing["NotAvailable"]
      ]
    };


  groups =
    GatherBy[
      rows,
      cellKey
    ];


  summaries =
    ExperimentCellSummary /@
      groups;


  If[
    MemberQ[
      summaries,
      $Failed
    ],
    Return[$Failed]
  ];


  SortBy[
    summaries,

    {
      Lookup[
        #,
        "m",
        Infinity
      ],

      Lookup[
        #,
        "lambda",
        Infinity
      ],

      Lookup[
        #,
        "eta",
        Infinity
      ],

      ToString[
        Lookup[
          #,
          "legitimacySignal",
          ""
        ]
      ]
    } &
  ]
];


CellSummaryTable[
  rows_List
] :=
  SummarizeExperimentCells[
    rows
  ];


(* ------------------------------------------------------------ *)
(* Dataset views                                                *)
(* ------------------------------------------------------------ *)

ExperimentRowsToDataset[
  rows_List
] :=
  Dataset[
    RawExperimentTable[
      rows
    ]
  ];


CellSummariesToDataset[
  rows_List
] :=
  Dataset[
    SummarizeExperimentCells[
      rows
    ]
  ];


(* ------------------------------------------------------------ *)
(* CSV export helpers                                           *)
(* ------------------------------------------------------------ *)

ExportRawExperimentCSV::empty =
  "No raw experiment rows were supplied for export.";

ExportCellSummaryCSV::empty =
  "No experiment rows were supplied for cell-summary export.";


ExportRawExperimentCSV[
  path_String,
  rows_List
] := Module[
  {
    flat,
    columns,
    table
  },


  If[
    rows === {},

    Message[
      ExportRawExperimentCSV::empty
    ];

    Return[$Failed]
  ];


  flat =
    RawExperimentTable[
      rows
    ];


  columns =
    Keys[
      First[
        flat
      ]
    ];


  table =
    Prepend[
      (
        CSVValue /@
          Lookup[
            #,
            columns
          ]
      ) & /@
        flat,

      columns
    ];


  Export[
    path,
    table,
    "CSV"
  ]
];


ExportCellSummaryCSV[
  path_String,
  rows_List
] := Module[
  {
    summaries,
    columns,
    table
  },


  If[
    rows === {},

    Message[
      ExportCellSummaryCSV::empty
    ];

    Return[$Failed]
  ];


  summaries =
    SummarizeExperimentCells[
      rows
    ];


  If[
    summaries === $Failed ||
    summaries === {},

    Return[$Failed]
  ];


  columns =
    Keys[
      First[
        summaries
      ]
    ];


  table =
    Prepend[
      (
        CSVValue /@
          Lookup[
            #,
            columns
          ]
      ) & /@
        summaries,

      columns
    ];


  Export[
    path,
    table,
    "CSV"
  ]
];