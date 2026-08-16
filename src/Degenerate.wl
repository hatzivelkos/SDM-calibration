(* ::Package:: *)

(* ============================================================ *)
(* File: src/Degenerate.wl                                     *)
(* Purpose: Threshold diagnostics for degenerate calibration     *)
(*                                                              *)
(* Requires:                                                    *)
(*   Profile.wl                                                 *)
(*   Signals.wl                                                 *)
(*   Pareto.wl                                                  *)
(*                                                              *)
(* This module does NOT determine whether a profile is           *)
(* geometrically degenerate. That is handled by Regimes.wl      *)
(* and Calibration.wl.                                         *)
(*                                                              *)
(* Its only role is to provide an auxiliary diagnostic for       *)
(* profiles for which endogenous calibration fails:             *)
(*                                                              *)
(*   the first detected value of p at which the SDM winner       *)
(*   differs from the winner at the lower endpoint pMin.        *)
(*                                                              *)
(* The search is performed in u = 1/p and may include u = 0,    *)
(* interpreted as the SDM limit p = Infinity.                   *)
(* ============================================================ *)


ClearAll[
  SDMWinnerExtended,
  BuildDiagnosticGridU,
  FirstWinnerSwitch,
  DegenerateDiagnostic
];


(* ------------------------------------------------------------ *)
(* SDM winner for finite p and p = Infinity                     *)
(* ------------------------------------------------------------ *)

SDMWinnerExtended::p =
  "Parameter p must satisfy p >= 1 or p = Infinity.";

SDMWinnerExtended::signal =
  "Could not evaluate legitimacy signal `1`.";


(* Winner rule:
     1. minimize the SDM compromise value;
     2. among compromise ties, maximize legitimacy;
     3. among remaining ties, minimize candidate index.

   For finite p the compromise value is d_p.
   For p = Infinity it is the lexicographically ordered
   SDM-limit vector {N_{m-1},...,N_0}.
*)

SDMWinnerExtended[
  A_?ProfileQ,
  p_,
  legitimacySignal_: "TopSupport"
] := Module[
  {
    d,
    L,
    candidates,
    betterQ
  },


  If[
    !(
      p === Infinity ||
      (
        NumericQ[p] &&
        p >= 1
      )
    ),

    Message[
      SDMWinnerExtended::p
    ];

    Return[$Failed]
  ];


  d =
    SDMCompromiseVector[
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
      SDMWinnerExtended::signal,
      legitimacySignal
    ];

    Return[$Failed]
  ];


  candidates =
    Range[
      CandidateCount[A]
    ];


  betterQ[i_, j_] := Module[
    {c},

    c =
      DCompare[
        d[[i]],
        d[[j]]
      ];


    Which[
      c === $Failed,
        False,

      c < 0,
        True,

      c > 0,
        False,

      L[[i]] > L[[j]],
        True,

      L[[i]] < L[[j]],
        False,

      True,
        i < j
    ]
  ];


  First @
    Sort[
      candidates,
      betterQ
    ]
];


(* ------------------------------------------------------------ *)
(* Diagnostic grid                                              *)
(* ------------------------------------------------------------ *)

BuildDiagnosticGridU::range =
  "Diagnostic range must satisfy 1 <= pMin < finitePMax.";

BuildDiagnosticGridU::size =
  "GridIntervals must be a positive integer.";


(* Default:
     pMin       = 1
     finitePMax = 40
     intervals  = 40

   The finite grid is uniform in u = 1/p.

   If includeInfinity is True, u=0 is added and interpreted
   as p=Infinity.
*)

BuildDiagnosticGridU[
  pMin_: 1.,
  finitePMax_: 40.,
  gridIntervals_: 40,
  includeInfinity_: True
] := Module[
  {
    pMinN,
    finitePMaxN,
    uMax,
    uMin,
    grid
  },


  pMinN =
    N[pMin];

  finitePMaxN =
    N[finitePMax];


  If[
    !NumericQ[pMinN] ||
    !NumericQ[finitePMaxN] ||
    pMinN < 1 ||
    pMinN >= finitePMaxN,

    Message[
      BuildDiagnosticGridU::range
    ];

    Return[$Failed]
  ];


  If[
    !IntegerQ[gridIntervals] ||
    gridIntervals < 1,

    Message[
      BuildDiagnosticGridU::size
    ];

    Return[$Failed]
  ];


  uMax =
    N[
      1/pMinN
    ];

  uMin =
    N[
      1/finitePMaxN
    ];


  grid =
    N @
      Subdivide[
        uMax,
        uMin,
        gridIntervals
      ];


  If[
    TrueQ[includeInfinity],

    grid =
      Join[
        grid,
        {0.}
      ]
  ];


  Sort @
    DeleteDuplicates[
      grid
    ]
];


(* ------------------------------------------------------------ *)
(* First SDM-winner switch                                      *)
(* ------------------------------------------------------------ *)

Options[FirstWinnerSwitch] = {

  "pMin" -> 1.,
  "FinitePMax" -> 40.,

  "GridU" -> Automatic,
  "GridIntervals" -> 40,

  "RefineSteps" -> 4,
  "RefineFactor" -> 8,

  "LegitimacySignal" -> "TopSupport",

  "IncludeInfinity" -> True
};


FirstWinnerSwitch::range =
  "Diagnostic range must satisfy 1 <= pMin < finitePMax.";

FirstWinnerSwitch::grid =
  "The diagnostic grid is invalid or empty.";

FirstWinnerSwitch::refine =
  "RefineSteps must be a nonnegative integer and RefineFactor must be an integer greater than 1.";


FirstWinnerSwitch[
  A_?ProfileQ,
  opts : OptionsPattern[]
] := Module[
  {
    pMin,
    finitePMax,
    gridU,
    gridIntervals,
    refineSteps,
    refineFactor,
    legitimacySignal,
    includeInfinity,

    uMax,
    uMin,
    pOfU,

    baselineWinner,
    uList,
    u,
    currentWinner,

    uFail,
    uSucc,

    win,
    step,
    candU,

    pSwitch,
    winnerAfterSwitch
  },


  pMin =
    N[
      OptionValue["pMin"]
    ];


  finitePMax =
    N[
      OptionValue["FinitePMax"]
    ];


  gridIntervals =
    OptionValue["GridIntervals"];


  refineSteps =
    OptionValue["RefineSteps"];


  refineFactor =
    OptionValue["RefineFactor"];


  legitimacySignal =
    OptionValue["LegitimacySignal"];


  includeInfinity =
    TrueQ[
      OptionValue["IncludeInfinity"]
    ];


  If[
    !NumericQ[pMin] ||
    !NumericQ[finitePMax] ||
    pMin < 1 ||
    pMin >= finitePMax,

    Message[
      FirstWinnerSwitch::range
    ];

    Return[$Failed]
  ];


  If[
    !IntegerQ[refineSteps] ||
    refineSteps < 0 ||
    !IntegerQ[refineFactor] ||
    refineFactor <= 1,

    Message[
      FirstWinnerSwitch::refine
    ];

    Return[$Failed]
  ];


  uMax =
    N[
      1/pMin
    ];


  uMin =
    N[
      1/finitePMax
    ];


  (* ---------------------------------------------------------- *)
  (* Build or validate grid                                     *)
  (* ---------------------------------------------------------- *)

  gridU =
    OptionValue["GridU"];


  If[
    gridU === Automatic,

    gridU =
      BuildDiagnosticGridU[
        pMin,
        finitePMax,
        gridIntervals,
        includeInfinity
      ];

    If[
      gridU === $Failed,
      Return[$Failed]
    ],

    (* custom grid *)

    gridU =
      Select[
        N @ gridU,
        NumericQ[#] &&
        0 <= # <= uMax &
      ];


    gridU =
      Join[
        gridU,
        {
          uMax,
          uMin
        }
      ];


    If[
      includeInfinity,
      gridU =
        Join[
          gridU,
          {0.}
        ],
      gridU =
        Select[
          gridU,
          # >= uMin &
        ]
    ];


    gridU =
      Sort @
        DeleteDuplicates[
          gridU
        ]
  ];


  If[
    gridU === {},

    Message[
      FirstWinnerSwitch::grid
    ];

    Return[$Failed]
  ];


  (* ---------------------------------------------------------- *)
  (* u -> p                                                     *)
  (* ---------------------------------------------------------- *)

  pOfU[u_?NumericQ] :=
    If[
      Abs[u] < 10^-15,
      Infinity,
      N[1/u]
    ];


  (* ---------------------------------------------------------- *)
  (* Baseline winner                                            *)
  (* ---------------------------------------------------------- *)

  baselineWinner =
    SDMWinnerExtended[
      A,
      pMin,
      legitimacySignal
    ];


  If[
    baselineWinner === $Failed,
    Return[$Failed]
  ];


  (* ---------------------------------------------------------- *)
  (* Coarse scan from small p toward large p                    *)
  (* ---------------------------------------------------------- *)

  (* Increasing p corresponds to decreasing u. *)

  uList =
    Reverse @
      Sort[
        gridU
      ];


  uFail =
    uMax;


  uSucc =
    Missing[
      "NotFound"
    ];


  Do[

    (* pMin itself defines the baseline and is not a switch. *)

    If[
      Abs[u - uMax] < 10^-15,
      Continue[]
    ];


    currentWinner =
      SDMWinnerExtended[
        A,
        pOfU[u],
        legitimacySignal
      ];


    If[
      currentWinner === $Failed,
      Return[$Failed]
    ];


    If[
      currentWinner =!= baselineWinner,

      uSucc =
        u;

      Break[],

      uFail =
        u
    ],

    {u, uList}
  ];


  (* ---------------------------------------------------------- *)
  (* No switch detected                                         *)
  (* ---------------------------------------------------------- *)

  If[
    MissingQ[uSucc],

    Return[
      <|

        "status" ->
          "NoSwitchDetected",

        "legitimacySignal" ->
          legitimacySignal,

        "pMin" ->
          pMin,

        "baselineWinner" ->
          baselineWinner,

        "pSwitch" ->
          Missing["NoSwitchDetected"],

        "uBoundary" ->
          Missing["NoSwitchDetected"],

        "winnerAfterSwitch" ->
          Missing["NoSwitchDetected"],

        "winnerAtInfinity" ->
          If[
            includeInfinity,

            SDMWinnerExtended[
              A,
              Infinity,
              legitimacySignal
            ],

            Missing["NotEvaluated"]
          ],

        "searchParameters" -> <|
          "finitePMax" -> finitePMax,
          "gridIntervals" -> gridIntervals,
          "refineSteps" -> refineSteps,
          "refineFactor" -> refineFactor,
          "includeInfinity" -> includeInfinity
        |>

      |>
    ]
  ];


  (* ---------------------------------------------------------- *)
  (* Refine the first detected switch boundary                  *)
  (* ---------------------------------------------------------- *)

  (* uFail is still on the baseline-winner side.
     uSucc is on the changed-winner side.

     Since p increases when u decreases:
         uSucc < uFail.
  *)

  win =
    Sort[
      {
        uSucc,
        uFail
      }
    ];


  Do[

    step =
      (
        win[[2]] -
        win[[1]]
      ) /
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


    candU =
      Sort @
        DeleteDuplicates @
        Join[
          candU,
          {
            win[[1]],
            win[[2]]
          }
        ];


    uFail =
      win[[2]];


    uSucc =
      Missing[
        "NotFound"
      ];


    (* Scan from smaller p toward larger p,
       i.e. from larger u toward smaller u. *)

    Do[

      currentWinner =
        SDMWinnerExtended[
          A,
          pOfU[u],
          legitimacySignal
        ];


      If[
        currentWinner === $Failed,
        Return[$Failed]
      ];


      If[
        currentWinner =!= baselineWinner,

        uSucc =
          u;

        Break[],

        uFail =
          u
      ],

      {
        u,
        Reverse[candU]
      }
    ];


    If[
      MissingQ[uSucc],
      Break[]
    ];


    win =
      Sort[
        {
          uSucc,
          uFail
        }
      ],

    {refineSteps}
  ];


  (* ---------------------------------------------------------- *)
  (* Final threshold estimate                                   *)
  (* ---------------------------------------------------------- *)

  If[
    MissingQ[uSucc],

    Return[
      <|
        "status" -> "RefinementFailed",
        "legitimacySignal" -> legitimacySignal,
        "pMin" -> pMin,
        "baselineWinner" -> baselineWinner,
        "pSwitch" -> Missing["RefinementFailed"]
      |>
    ]
  ];


  pSwitch =
    pOfU[
      uSucc
    ];


  winnerAfterSwitch =
    SDMWinnerExtended[
      A,
      pSwitch,
      legitimacySignal
    ];


  <|

    "status" ->
      If[
        pSwitch === Infinity,
        "SwitchDetectedAtInfinity",
        "SwitchDetected"
      ],

    "legitimacySignal" ->
      legitimacySignal,

    "pMin" ->
      pMin,

    "baselineWinner" ->
      baselineWinner,

    "pSwitch" ->
      pSwitch,

    "uBoundary" ->
      uSucc,

    "winnerAfterSwitch" ->
      winnerAfterSwitch,

    "winnerAtInfinity" ->
      If[
        includeInfinity,

        SDMWinnerExtended[
          A,
          Infinity,
          legitimacySignal
        ],

        Missing["NotEvaluated"]
      ],

    "searchParameters" -> <|
      "finitePMax" -> finitePMax,
      "gridIntervals" -> gridIntervals,
      "refineSteps" -> refineSteps,
      "refineFactor" -> refineFactor,
      "includeInfinity" -> includeInfinity
    |>

  |>
];


(* ------------------------------------------------------------ *)
(* Degenerate-profile diagnostic wrapper                        *)
(* ------------------------------------------------------------ *)

Options[DegenerateDiagnostic] =
  Options[
    FirstWinnerSwitch
  ];


(* This wrapper deliberately does not test whether the profile
   is degenerate. The calling layer already knows the calibration
   status.

   It returns the baseline winner, the SDM-limit winner, and the
   first detected winner-switch threshold.
*)

DegenerateDiagnostic[
  A_?ProfileQ,
  opts : OptionsPattern[]
] := Module[
  {
    legitimacySignal,
    pMin,
    switch
  },


  legitimacySignal =
    OptionValue[
      "LegitimacySignal"
    ];


  pMin =
    OptionValue[
      "pMin"
    ];


  switch =
    FirstWinnerSwitch[
      A,
      opts
    ];


  If[
    switch === $Failed,
    Return[$Failed]
  ];


  <|

    "legitimacySignal" ->
      legitimacySignal,

    "winnerAtPMin" ->
      SDMWinnerExtended[
        A,
        pMin,
        legitimacySignal
      ],

    "winnerAtInfinity" ->
      SDMWinnerExtended[
        A,
        Infinity,
        legitimacySignal
      ],

    "firstWinnerSwitch" ->
      switch

  |>
];