Set Default Goal Selector "!".

From Coq Require Import ZArith.
From Gimlight Require Import Model_direction.
From Gimlight Require Import Model_map.
From Gimlight Require Import Model_component_position.

Open Scope N_scope.

Definition move (p : position) (d : direction) (m : map) : option position :=
  match d with
  |North=>if 0 <? p.(y) then Some {|x:=p.(x);y:=p.(y)-1|} else None
  |South=>if p.(y) <? m.(height)-1 then Some {|x:=p.(x);y:=p.(y)+1|} else None
  |West=>if 0 <? p.(x) then Some {|x:=p.(x)-1;y:=p.(y)|} else None
  |East=>if p.(x) <? m.(width)-1 then Some {|x:=p.(x)+1;y:=p.(y)|} else None
  end.

Theorem move_west_cancelled : forall (p : position) (m : map),
  p.(x) = 0 -> move p West m = None.
Proof.
  intros p m H.
  unfold move.
  rewrite H.
  reflexivity.
Qed.

Theorem move_west_success : forall (p : position) (m : map),
  p.(x) > 0 -> let p' := move p West m in
  exists nx, p' = Some {|x:=nx;y:=p.(y)|} /\ nx = p.(x) - 1.
Proof.
  intros p m H.
  exists (p.(x)-1). 
  split.
  - simpl.
    apply N.gt_lt in H.
    apply N.ltb_lt in H.
    rewrite H.
    reflexivity.
  - reflexivity.
Qed.

Theorem move_east_cancelled : forall (p : position) (m : map),
  p.(x) = m.(width) - 1 -> move p East m = None.
Proof.
  intros p m H.
  unfold move.
  rewrite H.
  rewrite N.ltb_irrefl.
  reflexivity.
Qed.

Theorem move_east_success : forall (p : position) (m : map),
  p.(x) < m.(width) - 1 -> let p' := move p East m in
  exists nx, p' = Some {|x:=nx;y:=p.(y)|} /\ nx = p.(x) + 1.
Proof.
  intros p m H.
  exists (p.(x)+1).
  split.
  - simpl.
    apply N.ltb_lt in H.
    rewrite H.
    reflexivity.
  - reflexivity.
Qed.

Theorem move_north_cancelled : forall (p : position) (m : map),
  p.(y) = 0 -> move p North m = None.
Proof.
  intros p m H.
  unfold move.
  rewrite H.
  reflexivity.
Qed.

Theorem move_north_success : forall (p : position) (m : map),
  p.(y) > 0 -> let p' := move p North m in
  exists ny, p' = Some {|x:=p.(x);y:=ny|} /\ ny = p.(y) - 1.
Proof.
  intros p m H.
  exists (p.(y)-1).
  split.
  - simpl.
    apply N.gt_lt in H.
    apply N.ltb_lt in H.
    rewrite H.
    reflexivity.
  - reflexivity.
Qed.

Theorem move_south_cancelled : forall (p : position) (m : map),
  p.(y) = m.(height) - 1 -> move p South m = None.
Proof.
  intros p m H.
  unfold move.
  rewrite H.
  rewrite N.ltb_irrefl.
  reflexivity.
Qed.

Theorem move_south_success : forall (p : position) (m : map),
  p.(y) < m.(height) - 1 -> let p' := move p South m in
  exists ny, p' = Some {|x:=p.(x);y:=ny|} /\ ny = p.(y) + 1.
Proof.
  intros p m H.
  exists (p.(y)+1).
  split.
  - simpl.
    apply N.ltb_lt in H.
    rewrite H.
    reflexivity.
  - reflexivity.
Qed.
