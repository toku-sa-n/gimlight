Set Default Goal Selector "!".

Parameter t : Type -> Type.

Parameter ret : forall {A : Type}, A -> t A.
Parameter bind : forall {A B : Type}, t A -> (A -> t B) -> t B.

Axiom bind_ret_law : forall (A B : Type) (x : A) (f : A -> t B),
  bind (ret x) f = f x.

Axiom bind_assoc_law : forall (A B C : Type) (m : t A) (f : A -> t B) (g : B -> t C),
  bind (bind m f) g = bind m (fun x => bind (f x) g).
