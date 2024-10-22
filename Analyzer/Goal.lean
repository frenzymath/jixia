/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda
-/
import Lean
import Metalib.Info
import Analyzer.Types

open Lean Elab Meta Tactic

namespace Analyzer.Goal

-- see Meta.ppGoal
def fromMVar (goal : MVarId) (extraFun : MVarId → MetaM (Option Json) := fun _ => pure none) : MetaM Goal :=
  withEnableInfoTree false <| goal.withContext do
    let lctx ← getLCtx
    let lctx : LocalContext := lctx.sanitizeNames.run' { options := ← getOptions }
    let mut context := Array.mkEmpty lctx.size
    for ldecl in lctx do
      if ldecl.isImplementationDetail then
        continue
      let var ← match ldecl with
      | .cdecl _ id name type .. => do
        let type ← instantiateMVars type
        pure {
          id := id.name,
          name := name.simpMacroScopes,
          type := (← ppExpr type).pretty,
          value? := none,
          isProp := (← inferType type).isProp,
        }
      | .ldecl _ id name type value .. => do
        let type ← instantiateMVars type
        pure {
          id := id.name,
          name := name.simpMacroScopes,
          type := (← ppExpr type).pretty,
          value? := (← ppExpr value).pretty,
          isProp := (← inferType type).isProp,
        }
      context := context.push var
    let type ← goal.getType
    let tag ← goal.getTag
    let extra? ← extraFun goal
    return {
      tag,
      context,
      mvarId := goal.name,
      type := (← ppExpr type).pretty,
      isProp := (← inferType type).isProp,
      extra?,
    }

def fromTactic (extraFun : MVarId → MetaM (Option Json) := fun _ => pure none) : TacticM (Array Goal) := do
  (← getUnsolvedGoals).toArray.mapM fun mvar => Goal.fromMVar mvar extraFun

end Analyzer.Goal
