import Lean
open Lean

def setPPOptions (opts : Lean.Options) : Lean.Options :=
  opts
    |>.set pp.notation.name true
    |>.set pp.fieldNotation.name true
    |>.set pp.fullNames.name false
    |>.set pp.coercions.types.name false
