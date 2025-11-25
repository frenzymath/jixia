import Lean
open Lean

def setPPOptions (opts : Lean.Options) : Lean.Options :=
  opts
    |>.set pp.notation.name false
    |>.set pp.fieldNotation.name false
    |>.set pp.fullNames.name true
    |>.set pp.coercions.types.name true
