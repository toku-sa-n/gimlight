Set Default Goal Selector "!".

From Coq Require Import MSets.MSetAVL.
From Coq Require Import Structures.OrdersEx.
From Coq Require Import Numbers.BinNums.

From Gimlight Require Import Model_entity.

Module EntitySet := Make(N_as_OT).

Definition entity_generator := EntitySet.t.

Definition mk_entity_generator : entity_generator := 
  EntitySet.empty.

Definition try_mk_entity (e : entity) (eg : entity_generator) : option entity_generator :=
  if EntitySet.mem e eg then None
  else Some (EntitySet.add e eg).
