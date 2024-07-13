Set Default Goal Selector "!".

From Coq Require Import MSets.MSetAVL.
From Coq Require Import Structures.OrdersEx.
From Coq Require Import ZArith.ZArith.
From Gimlight Require Import Model_entity.

Open Scope N_scope.

Module EntitySet := Make(N_as_OT).

Definition entity_generator := EntitySet.t.

Definition mk_entity_generator : entity_generator := 
  EntitySet.empty.

Definition max_entities:=1_000_000_000.

Definition try_mk_entity (e : entity) (eg : entity_generator) : option (entity * entity_generator) :=
  if EntitySet.mem e eg
    then None
    else Some (e, EntitySet.add e eg).

Fixpoint mk_entity_rec (counter : positive)(e : entity) (eg : entity_generator) {struct counter}
  : option (entity * entity_generator) :=
  match counter with
  | xI q => mk_entity_rec (xO q) (N.succ e) eg
  | xO q => mk_entity_rec q (N.succ e) eg
  | xH => None
  end.

Fixpoint mk_entity_rec (counter : positive)(e : entity) (eg : entity_generator) {struct counter}
  : option (entity * entity_generator) :=
  match try_mk_entity e eg with
  | Some (e', eg') => Some (e', eg')
  | None => 
      match counter with 
      | xI q => mk_entity_rec (xO q) (N.succ e) eg
      | xO q => mk_entity_rec q (N.succ e) eg
      | xH => None
      end
  end.
