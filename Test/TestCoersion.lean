import Mathlib
-- import Analyzer.Process.Augmentation
open ZMod


-- #elab c:command

lemma cube_of_castHom_ne_zero {n : ZMod 9} :
    castHom (show 3 ∣ 9 by sorry) (ZMod 3) n ≠ 0 → n ^ 3 = 1 ∨ n ^ 3 = 8 := by
  revert n; decide

lemma cube_o2f_not_dvd1 {n : ℤ} (h : ¬3 ∣ n) : (n : ZMod 9) ^ 3 = 1 ∨ (n : ZMod 9) ^ 3 = 8 :=
  by
  apply cube_of_castHom_ne_zero
  rw [map_intCast]
  rw [Ne]
  rw [ZMod.intCast_zmod_eq_zero_iff_dvd]
  assumption
