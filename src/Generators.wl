(* ::Package:: *)

(* ============================================================ *)
(* File: src/Generators.wl                                     *)
(* Purpose: Preference-profile generators                       *)
(*                                                              *)
(* Requires:                                                    *)
(*   Profile.wl                                                 *)
(*                                                              *)
(* The main generator used in the paper is the two-pole model   *)
(* with opposite reference orders and sequential random         *)
(* adjacent swaps.                                              *)
(*                                                              *)
(* Random generation is separated from all signal, geometry,    *)
(* and calibration calculations.                                *)
(* ============================================================ *)


ClearAll[
  AdjacentSwapStep,
  PerturbPermutation,
  TwoPoleOrders,
  TwoPoleBlockSizes,
  MakeTwoPoleProfile,
  GenerateTwoPoleSample
];


(* ------------------------------------------------------------ *)
(* Adjacent-swap perturbation                                   *)
(* ------------------------------------------------------------ *)

(* Apply one random adjacent swap to a permutation written
   from best to worst.

   One of the Length[perm]-1 adjacent position pairs is selected
   uniformly at random.
*)

AdjacentSwapStep[perm_List] := Module[
  {q, k, m},

  q = perm;
  m = Length[q];

  If[
    m <= 1,
    Return[q]
  ];

  k =
    RandomInteger[
      {1, m - 1}
    ];

  q[[{k, k + 1}]] =
    q[[{k + 1, k}]];

  q
];


(* Apply eta sequential random adjacent swaps.

   Each new swap acts on the permutation produced by the
   preceding swaps. Consequently, the same adjacent pair may
   be selected more than once and an earlier swap may be undone.
*)

PerturbPermutation[
  perm_List,
  eta_Integer?NonNegative
] :=
  Nest[
    AdjacentSwapStep,
    perm,
    eta
  ];


(* ------------------------------------------------------------ *)
(* Two-pole reference orders                                    *)
(* ------------------------------------------------------------ *)

(* Canonical opposite pole orders on candidates {1,...,m}. *)

TwoPoleOrders[
  m_Integer?Positive
] /; m >= 2 :=
  <|
    "poleA" -> Range[m],
    "poleB" -> Reverse[Range[m]]
  |>;


(* ------------------------------------------------------------ *)
(* Bloc sizes                                                   *)
(* ------------------------------------------------------------ *)

TwoPoleBlockSizes::lambda =
  "The bloc-share parameter lambda must lie in the interval [0,1].";


(* The first bloc contains Round[lambda n] voters.
   The remaining voters belong to the second bloc.
*)

TwoPoleBlockSizes[
  n_Integer?Positive,
  lambda_?NumericQ
] := Module[
  {nA, nB},

  If[
    !TrueQ[0 <= lambda <= 1],
    Message[TwoPoleBlockSizes::lambda];
    Return[$Failed]
  ];

  nA =
    Round[
      lambda n
    ];

  nB =
    n - nA;

  <|
    "nA" -> nA,
    "nB" -> nB
  |>
];


(* ------------------------------------------------------------ *)
(* One two-pole profile                                         *)
(* ------------------------------------------------------------ *)

Options[MakeTwoPoleProfile] = {
  "PoleA" -> Automatic,
  "PoleB" -> Automatic,
  "ReturnMetadata" -> False
};


MakeTwoPoleProfile::pole =
  "PoleA and PoleB must each be permutations of the candidate set {1,...,m}.";


(* Generate one two-pole polarized profile.

   Parameters:
     n       number of voters
     m       number of candidates
     lambda  share of bloc A
     eta     number of adjacent swaps applied to every ballot

   Output:
     by default, a rank matrix;
     with "ReturnMetadata" -> True, an association containing
     the profile and generator parameters.
*)

MakeTwoPoleProfile[
  n_Integer?Positive,
  m_Integer?Positive,
  lambda_?NumericQ,
  eta_Integer?NonNegative,
  opts : OptionsPattern[]
] /; m >= 2 := Module[
  {
    poleA,
    poleB,
    sizes,
    nA,
    nB,
    permsA,
    permsB,
    A
  },

  poleA =
    OptionValue["PoleA"];

  poleB =
    OptionValue["PoleB"];


  If[
    poleA === Automatic,
    poleA = Range[m]
  ];

  If[
    poleB === Automatic,
    poleB = Reverse[poleA]
  ];


  (* Both pole orders must be genuine permutations of the same
     candidate set. *)

  If[
    !ListQ[poleA] ||
    !ListQ[poleB] ||
    Sort[poleA] =!= Range[m] ||
    Sort[poleB] =!= Range[m],

    Message[MakeTwoPoleProfile::pole];
    Return[$Failed]
  ];


  sizes =
    TwoPoleBlockSizes[
      n,
      lambda
    ];

  If[
    sizes === $Failed,
    Return[$Failed]
  ];


  nA =
    sizes["nA"];

  nB =
    sizes["nB"];


  (* Generate perturbed best-to-worst permutation rows. *)

  permsA =
    Table[
      PerturbPermutation[
        poleA,
        eta
      ],
      {nA}
    ];

  permsB =
    Table[
      PerturbPermutation[
        poleB,
        eta
      ],
      {nB}
    ];


  (* Convert to the project's internal rank-matrix format. *)

  A =
    ToRankMatrix[
      Join[
        permsA,
        permsB
      ],
      "PermutationRows"
    ];


  If[
    A === $Failed,
    Return[$Failed]
  ];


  If[
    TrueQ[
      OptionValue["ReturnMetadata"]
    ],

    <|
      "profile" -> A,
      "n" -> n,
      "m" -> m,
      "lambda" -> N[lambda],
      "eta" -> eta,
      "poleA" -> poleA,
      "poleB" -> poleB,
      "nA" -> nA,
      "nB" -> nB
    |>,

    A
  ]
];


(* ------------------------------------------------------------ *)
(* Monte Carlo sample                                           *)
(* ------------------------------------------------------------ *)

Options[GenerateTwoPoleSample] = {
  "PoleA" -> Automatic,
  "PoleB" -> Automatic,
  "Seed" -> Automatic,
  "ReturnMetadata" -> False
};


GenerateTwoPoleSample::seed =
  "Seed must be an integer or Automatic.";


(* Generate reps independent profiles for one parameter cell.

   If "Seed" -> integer is supplied, BlockRandom localizes the
   random state. The same call with the same seed therefore
   reproduces the same sample without altering the external
   Wolfram Language random state.
*)

GenerateTwoPoleSample[
  reps_Integer?Positive,
  n_Integer?Positive,
  m_Integer?Positive,
  lambda_?NumericQ,
  eta_Integer?NonNegative,
  opts : OptionsPattern[]
] /; m >= 2 := Module[
  {
    poleA,
    poleB,
    resolvedPoleA,
    resolvedPoleB,
    seed,
    returnMetadata,
    generate,
    profiles,
    sizes
  },

  poleA =
    OptionValue["PoleA"];

  poleB =
    OptionValue["PoleB"];

  seed =
    OptionValue["Seed"];

  returnMetadata =
    TrueQ[
      OptionValue["ReturnMetadata"]
    ];


  If[
    !(
      seed === Automatic ||
      IntegerQ[seed]
    ),

    Message[
      GenerateTwoPoleSample::seed
    ];

    Return[$Failed]
  ];


  (* Resolve pole orders once so that generation and metadata
     always use exactly the same reference orders. *)

  resolvedPoleA =
    If[
      poleA === Automatic,
      Range[m],
      poleA
    ];

  resolvedPoleB =
    If[
      poleB === Automatic,
      Reverse[resolvedPoleA],
      poleB
    ];


  (* Validate resolved poles before entering the Monte Carlo loop. *)

  If[
    Sort[resolvedPoleA] =!= Range[m] ||
    Sort[resolvedPoleB] =!= Range[m],

    Message[MakeTwoPoleProfile::pole];
    Return[$Failed]
  ];


  generate[] :=
    Table[
      MakeTwoPoleProfile[
        n,
        m,
        lambda,
        eta,
        "PoleA" -> resolvedPoleA,
        "PoleB" -> resolvedPoleB,
        "ReturnMetadata" -> False
      ],
      {reps}
    ];


  profiles =
    If[
      seed === Automatic,

      generate[],

      BlockRandom[
        SeedRandom[seed];
        generate[]
      ]
    ];


  If[
    MemberQ[profiles, $Failed],
    Return[$Failed]
  ];


  If[
    returnMetadata,

    sizes =
      TwoPoleBlockSizes[
        n,
        lambda
      ];

    If[
      sizes === $Failed,
      Return[$Failed]
    ];

    <|
      "profiles" -> profiles,
      "model" -> "TwoPole",
      "reps" -> reps,
      "n" -> n,
      "m" -> m,
      "lambda" -> N[lambda],
      "eta" -> eta,
      "nA" -> sizes["nA"],
      "nB" -> sizes["nB"],
      "poleA" -> resolvedPoleA,
      "poleB" -> resolvedPoleB,
      "seed" -> seed
    |>,

    profiles
  ]
];