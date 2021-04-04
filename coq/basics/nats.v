Fixpoint factorial (n:nat) : nat :=
  match n with
  | O => 1
  | S prev => mult n (factorial prev)
  end.

Example test_factorial1: (factorial 3) = 6.
Proof. simpl. reflexivity. Qed.
Example test_factorial2: (factorial 5) = (mult 10 12).
Proof. simpl. reflexivity. Qed.



