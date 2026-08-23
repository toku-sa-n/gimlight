From Stdlib Require Import Lia BinPos PeanoNat Compare_dec Pnat.
Require Import Dimensions Direction Map
  DimensionsApi MapApi DirectionApi PositionApi.

Module Make
    (Dimensions : DimensionsApi)
    (Map : MapApi Dimensions)
    (Direction : DirectionApi) : PositionApi Dimensions Map Direction.

Definition coordinate (limit : positive) : Set :=
  { value : nat | value < Pos.to_nat limit }.

Record t (map : Map.t) : Set := {
  x : coordinate (Dimensions.width (Map.dimensions map));
  y : coordinate (Dimensions.height (Map.dimensions map))
}.

Definition x_value {map : Map.t} (position : t map) : nat :=
  proj1_sig (x map position).

Definition y_value {map : Map.t} (position : t map) : nat :=
  proj1_sig (y map position).

Definition in_bounds {map : Map.t} (position : t map) : Prop :=
  x_value position < Pos.to_nat (Dimensions.width (Map.dimensions map)) /\
  y_value position < Pos.to_nat (Dimensions.height (Map.dimensions map)).

Lemma coordinate_in_bounds {limit : positive} (value : coordinate limit) :
  proj1_sig value < Pos.to_nat limit.
Proof.
  exact (proj2_sig value).
Qed.

Definition coordinate_left {limit : positive}
    (value : coordinate limit) : coordinate limit.
Proof.
  destruct value as [value value_bound].
  destruct (Nat.eq_dec value 0) as [at_left | not_at_left].
  - exact (exist _ value value_bound).
  - exists (value - 1).
    lia.
Defined.

Definition coordinate_right {limit : positive}
    (value : coordinate limit) : coordinate limit.
Proof.
  destruct value as [value value_bound].
  destruct (lt_dec (S value) (Pos.to_nat limit)) as [can_move | at_right].
  - exact (exist _ (S value) can_move).
  - exact (exist _ value value_bound).
Defined.

Definition coordinate_up {limit : positive}
    (value : coordinate limit) : coordinate limit.
Proof.
  exact (coordinate_left value).
Defined.

Definition coordinate_down {limit : positive}
    (value : coordinate limit) : coordinate limit.
Proof.
  exact (coordinate_right value).
Defined.

Lemma centered_coordinate (limit : positive) :
  Pos.to_nat limit / 2 < Pos.to_nat limit.
Proof.
  apply Nat.div_lt.
  - apply Pos2Nat.is_pos.
  - lia.
Qed.

Definition centeredOn (map : Map.t) : t map.
Proof.
  refine {| x := exist _
      (Pos.to_nat (Dimensions.width (Map.dimensions map)) / 2) _;
    y := exist _
      (Pos.to_nat (Dimensions.height (Map.dimensions map)) / 2) _ |}.
  - apply centered_coordinate.
  - apply centered_coordinate.
Defined.

Definition move (map : Map.t) (direction : Direction.t)
    (position : t map) : t map.
Proof.
  destruct direction.
  - refine {| x := coordinate_left (x map position); y := y map position |}.
  - refine {| x := coordinate_right (x map position); y := y map position |}.
  - refine {| x := x map position; y := coordinate_up (y map position) |}.
  - refine {| x := x map position; y := coordinate_down (y map position) |}.
Defined.

Theorem position_in_bounds {map : Map.t} (position : t map) :
  in_bounds position.
Proof.
  unfold in_bounds, x_value, y_value.
  split.
  - apply coordinate_in_bounds.
  - apply coordinate_in_bounds.
Qed.

Theorem centeredOn_in_bounds (map : Map.t) :
  in_bounds (centeredOn map).
Proof.
  apply position_in_bounds.
Qed.

Theorem move_preserves_bounds (map : Map.t) (direction : Direction.t)
    (position : t map) :
  in_bounds (move map direction position).
Proof.
  apply position_in_bounds.
Qed.

End Make.

Module Position : PositionApi Dimensions Map Direction :=
  Make Dimensions Map Direction.
