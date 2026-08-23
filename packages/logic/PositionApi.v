From Stdlib Require Import BinPos.
Require Import DimensionsApi MapApi DirectionApi.

Module Type PositionApi
    (Dimensions : DimensionsApi)
    (Map : MapApi Dimensions)
    (Direction : DirectionApi).

Record t (map : Map.t) : Set := {
  x : { value : nat |
    value < Pos.to_nat (Dimensions.width (Map.dimensions map)) };
  y : { value : nat |
    value < Pos.to_nat (Dimensions.height (Map.dimensions map)) }
}.

Parameter x_value : forall {map : Map.t}, t map -> nat.

Parameter y_value : forall {map : Map.t}, t map -> nat.

Parameter in_bounds : forall {map : Map.t}, t map -> Prop.

Parameter centeredOn : forall (map : Map.t), t map.

Parameter move : forall (map : Map.t) (direction : Direction.t),
  t map -> t map.

Axiom position_in_bounds : forall {map : Map.t} (position : t map),
  in_bounds position.

Axiom centeredOn_in_bounds : forall (map : Map.t),
  in_bounds (centeredOn map).

Axiom move_preserves_bounds : forall (map : Map.t) (direction : Direction.t)
    (position : t map),
  in_bounds (move map direction position).

End PositionApi.
