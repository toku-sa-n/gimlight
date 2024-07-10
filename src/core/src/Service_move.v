Set Default Goal Selector "!".

From Coq Require Import ZArith.
From Gimlight Require Import Model_direction.
From Gimlight Require Import Model_map.
From Gimlight Require Import Model_component_position.

Open Scope positive_scope.

Definition move (p : position) (d : direction) (m : map) : option position :=
  match d with
  |North=>if p.(x)+1 <? m.(width) then Some {|x:=p.(x)+1;y:=p.(y)|} else None
  |South=>if p.(x) <? m.(width) then Some {|x:=p.(x)-1;y:=p.(y)|} else None
  |East=>if p.(y)+1 <? m.(height) then Some {|x:=p.(x);y:=p.(y)+1|} else None
  |West=>if p.(y) <? m.(height) then Some {|x:=p.(x);y:=p.(y)-1|} else None
  end.

