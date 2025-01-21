/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda
-/
import Lean

set_option autoImplicit false
open Lean renaming Name → ame
open IO in
def hello : IO Unit :=
  println "Hello, world!"

universe u
variable {α : Type u}

theorem eq_trans_sym {a b c : α} : a = b → b = c → c = a := by
  intro h₁ h₂
  rw [h₁, h₂]

theorem simp_test (x y z : Nat) (p : Nat → Prop) (h : p (x * y))
    : p ((x + 0) * (0 + y * 1 + z * 0)) := by
  simp
  assumption

theorem dsimp_test (x y z : Nat) (p : Nat → Prop) (h : p (x * y))
    : p ((x + 0) * (0 + y * 1 + z * 0)) := by
  let h₁ := Nat.mul_zero
  simp_all

theorem rw_test (x y z : Nat) (p : Nat → Prop) (h : p (x * y))
    : p ((x + 0) * (0 + y * 1 + z * 0)) := by
  rw [Nat.mul_zero]
  repeat rw [Nat.add_zero]
  rw [Nat.zero_add, Nat.mul_one]
  assumption

theorem rcases_test {x : Nat} (h : ∃ k, x = 2 * k) : ∃ k, x + 2 = 2 * k := by
  rcases h with ⟨k, hk⟩
  rw [hk]
  exact ⟨k + 1, rfl⟩

theorem cdot_test : (∀ A, A → A) ∧ True := by
  constructor
  · intro A x
    exact x
  · constructor

theorem coe_test (n : Nat) (h : n = 0) : ((↑n : Int) = 0) := by simp [h]

theorem comp_test (x : Nat) (h : ∃ k, x = 2 * k) : ∃ k, x + 4 = 2 * k :=
  rcases_test (rcases_test h)

/-- pow' x n = x ^ (n + 1) -/
def pow' [Mul α] (x : α) : Nat → α
  | .zero => x
  | .succ n => pow' x n * x

theorem pow'_succ [Mul α] (x : α) (n : Nat) : pow' x n.succ = (pow' x n) * x := rfl

structure FermatTriple (k : Nat) : Type where
  x : Nat
  y : Nat
  z : Nat
  eqn : x ^ k + y ^ k = z ^ k

example : FermatTriple 2 := ⟨ 3, 4, 5, rfl ⟩

namespace Option

inductive IsSome : Option α → Prop where
  | of_some : ∀ {a : α}, IsSome (some a)

inductive _root_.Sum.IsRight {β : Type _} : α ⊕ β → Prop where
  | of_right : ∀ {b : β}, IsRight (.inr b)

/-- A none is not a some -/
theorem neg_is_some_none : ¬IsSome (α := α) none := by sorry

instance : DecidablePred (IsSome (α := α)) := fun a =>
  match a with
  | some _ => .isTrue .of_some
  | none => .isFalse neg_is_some_none

end Option

theorem map_length (f : α → α) (l : List α) : (l.map f).length = l.length := by
  induction l with | nil | cons _ _ ih => _
  rfl
  unfold List.map
  unfold List.length
  rewrite [ih]
  rfl
