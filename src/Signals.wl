(* ::Package:: *)

(* ============================================================ *)
(* File: src/Signals.wl                                        *)
(* Purpose: Profile-derived compromise and support signals      *)
(*                                                              *)
(* Requires:                                                    *)
(*   Profile.wl                                                 *)
(*                                                              *)
(* This module contains raw profile signals only.               *)
(* Normalization, Pareto geometry, and calibration are handled  *)
(* in later modules.                                            *)
(*                                                              *)
(* Available support signals:                                   *)
(*   "TopSupport"                                               *)
(*   "PairwiseSupport"                                          *)
(* ============================================================ *)


ClearAll[
  LossMatrix,

  TopSupport,
  PairwiseMargins,
  PairwiseSupport,
  LegitimacyVector,

  DpVector,
  DInfinitySDMVector,
  SDMCompromiseVector
];


(* ------------------------------------------------------------ *)
(* Rank losses                                                  *)
(* ------------------------------------------------------------ *)

(* Loss of candidate i for voter v:
     ell_v(i) = rank_v(i) - 1

   Hence losses lie in {0,...,m-1}.
*)

LossMatrix[
  A_?ProfileQ
] :=
  A - 1;


(* ------------------------------------------------------------ *)
(* Direct top support                                           *)
(* ------------------------------------------------------------ *)

(* Number of voters ranking each candidate first. *)

TopSupport[
  A_?ProfileQ
] :=
  Count[#, 1] & /@
    Transpose[A];


(* ------------------------------------------------------------ *)
(* Pairwise majority margins                                    *)
(* ------------------------------------------------------------ *)

(* PairwiseMargins[A][[i,j]] is

       # voters preferring i to j
       -
       # voters preferring j to i.

   Positive values favor candidate i.
   The matrix is antisymmetric and has zero diagonal.
*)

PairwiseMargins[
  A_?ProfileQ
] := Module[
  {
    m,
    margins
  },

  m =
    CandidateCount[A];


  margins =
    ConstantArray[
      0,
      {m, m}
    ];


  Do[

    margins[[i, j]] =
      Count[
        MapThread[
          Less,
          {
            A[[All, i]],
            A[[All, j]]
          }
        ],
        True
      ] -
      Count[
        MapThread[
          Greater,
          {
            A[[All, i]],
            A[[All, j]]
          }
        ],
        True
      ];


    margins[[j, i]] =
      -margins[[i, j]],

    {i, 1, m - 1},
    {j, i + 1, m}
  ];


  margins
];


(* ------------------------------------------------------------ *)
(* Pairwise support                                             *)
(* ------------------------------------------------------------ *)

(* Normalized Copeland-style pairwise support:

     1      for a pairwise majority win,
     1/2    for a pairwise tie,
     0      for a pairwise majority loss.

   The score is divided by m-1 and therefore lies in [0,1].

   With an odd number of voters, pairwise ties cannot occur,
   but the 1/2 convention keeps the function well-defined for
   general profiles.
*)

PairwiseSupport[
  A_?ProfileQ
] := Module[
  {
    margins,
    m,
    score
  },

  margins =
    PairwiseMargins[A];


  m =
    CandidateCount[A];


  score[x_] :=
    Which[
      x > 0,
        1.,

      x == 0,
        0.5,

      x < 0,
        0.
    ];


  Table[
    Total[
      score /@
        Delete[
          margins[[i]],
          i
        ]
    ] /
      (m - 1),

    {i, 1, m}
  ]
];


(* ------------------------------------------------------------ *)
(* Unified support-signal interface                             *)
(* ------------------------------------------------------------ *)

LegitimacyVector::signal =
  "Unknown support signal `1`.";


(* The historical function name LegitimacyVector is retained as
   the common pipeline interface.

   The scientific interpretation of each signal is handled
   outside this source module.
*)

LegitimacyVector[
  A_?ProfileQ,
  signal_: "TopSupport"
] :=
  Switch[
    signal,

    "TopSupport",
      TopSupport[A],

    "PairwiseSupport",
      PairwiseSupport[A],

    _,
      Message[
        LegitimacyVector::signal,
        signal
      ];

      $Failed
  ];


(* ------------------------------------------------------------ *)
(* Finite-p SDM compromise signal                               *)
(* ------------------------------------------------------------ *)

(* Unrooted SDM divergence:

       d_p(i) = Sum_v ell_v(i)^p

   Smaller values are better.
*)

DpVector[
  A_?ProfileQ,
  p_
] /;
    p =!= Infinity &&
    NumericQ[p] &&
    p >= 1 :=
  Module[
    {loss},

    loss =
      LossMatrix[A];

    Total /@
      Transpose[
        loss^p
      ]
  ];


(* ------------------------------------------------------------ *)
(* SDM limit as p -> Infinity                                   *)
(* ------------------------------------------------------------ *)

(* For candidate i, return

       {N_{m-1}(i), N_{m-2}(i), ..., N_0(i)},

   where N_r(i) is the number of voters assigning loss r.

   These vectors are compared lexicographically, with smaller
   vectors preferred.
*)

DInfinitySDMVector[
  A_?ProfileQ
] := Module[
  {
    loss,
    m,
    candidateLosses
  },

  loss =
    LossMatrix[A];

  m =
    CandidateCount[A];

  candidateLosses =
    Transpose[
      loss
    ];


  Table[
    Table[
      Count[
        candidateLosses[[i]],
        r
      ],
      {r, m - 1, 0, -1}
    ],
    {i, 1, m}
  ]
];


(* ------------------------------------------------------------ *)
(* Unified SDM compromise interface                             *)
(* ------------------------------------------------------------ *)

(* Finite p returns numerical divergences.
   p = Infinity returns the lexicographic SDM-limit vectors.
*)

SDMCompromiseVector[
  A_?ProfileQ,
  p_
] :=
  Which[

    p === Infinity,
      DInfinitySDMVector[A],

    NumericQ[p] &&
    p >= 1,
      DpVector[
        A,
        p
      ],

    True,
      $Failed
  ];