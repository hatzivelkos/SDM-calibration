(* ::Package:: *)

(* ============================================================ *)
(* File: src/Profile.wl                                        *)
(* Purpose: Basic profile representation and validation         *)
(*                                                              *)
(* Internal profile representation:                             *)
(*   A[[v,i]] = rank assigned by voter v to candidate i         *)
(*                                                              *)
(* Candidate ranks are integers 1,...,m and every row of A      *)
(* must be a permutation of Range[m].                           *)
(*                                                              *)
(* This module has no dependencies on other project modules.    *)
(* ============================================================ *)


ClearAll[
  ProfileQ,
  ToRankMatrix,
  ProfileDimensions,
  VoterCount,
  CandidateCount
];


(* ------------------------------------------------------------ *)
(* Profile validation                                           *)
(* ------------------------------------------------------------ *)

ProfileQ[A_] := Module[
  {dims, n, m},

  If[!MatrixQ[A], Return[False]];

  dims = Dimensions[A];

  (* Exclude empty/ragged/non-two-dimensional inputs. *)
  If[Length[dims] =!= 2, Return[False]];

  {n, m} = dims;

  If[n < 1 || m < 2, Return[False]];

  (* Every entry must be an integer rank. *)
  If[!VectorQ[Flatten[A], IntegerQ], Return[False]];

  (* Each voter row must contain every rank 1,...,m exactly once. *)
  And @@ (Sort[#] === Range[m] & /@ A)
];


(* ------------------------------------------------------------ *)
(* Conversion to the internal rank-matrix representation        *)
(* ------------------------------------------------------------ *)

ToRankMatrix::bad =
  "Invalid profile data for form `1`.";

ToRankMatrix::form =
  "Unknown profile representation `1`. Supported forms are \
\"RankMatrix\" and \"PermutationRows\".";


ToRankMatrix[data_, form_: "RankMatrix"] := Module[
  {dims, n, m, validPermutationRowsQ, A},

  Switch[form,

    "RankMatrix",

      If[
        ProfileQ[data],
        data,
        Message[ToRankMatrix::bad, "RankMatrix"];
        $Failed
      ],


    "PermutationRows",

      (* A permutation row lists candidates from best to worst,
         e.g. {2,1,3} means M2 > M1 > M3. *)

      If[!MatrixQ[data],
        Message[ToRankMatrix::bad, "PermutationRows"];
        Return[$Failed]
      ];

      dims = Dimensions[data];

      If[Length[dims] =!= 2,
        Message[ToRankMatrix::bad, "PermutationRows"];
        Return[$Failed]
      ];

      {n, m} = dims;

      validPermutationRowsQ =
        n >= 1 &&
        m >= 2 &&
        VectorQ[Flatten[data], IntegerQ] &&
        And @@ (Sort[#] === Range[m] & /@ data);

      If[!validPermutationRowsQ,
        Message[ToRankMatrix::bad, "PermutationRows"];
        Return[$Failed]
      ];

      (* Ordering gives the inverse permutation:
         the position of candidate i in the best-to-worst row. *)
      A = Ordering /@ data;

      If[
        ProfileQ[A],
        A,
        Message[ToRankMatrix::bad, "PermutationRows"];
        $Failed
      ],


    _,

      Message[ToRankMatrix::form, form];
      $Failed
  ]
];


(* ------------------------------------------------------------ *)
(* Basic profile dimensions                                     *)
(* ------------------------------------------------------------ *)

ProfileDimensions[A_?ProfileQ] :=
  Dimensions[A];


VoterCount[A_?ProfileQ] :=
  First[Dimensions[A]];


CandidateCount[A_?ProfileQ] :=
  Last[Dimensions[A]];