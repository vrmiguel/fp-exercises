Inductive bool: Type :=
  | true
  | false.

Definition negate (b: bool): bool :=
  match b with
  | true => false
  | false => true
  end.

Definition and (a: bool) (b: bool): bool :=
  match a with
  | true => b
  | false => false
  end.

Definition or (a: bool) (b: bool): bool :=
  match a with
  | true => true
  | false => b
  end.


Example test_or: (or true false) = true.
Proof. simpl. reflexivity. Qed.

(* Exercise -- Implement nand *)

Definition nand (a:bool) (b:bool) : bool :=
  match a with 
  | false => true
  | true => (negate b)
  end.







