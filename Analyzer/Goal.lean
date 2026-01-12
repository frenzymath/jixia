/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda
-/
import Lean
import Metalib.Info
import Analyzer.Types

open Lean Elab Tactic PrettyPrinter
open Meta hiding ppExpr

namespace Analyzer.Goal

def printContext : MetaM (Array Variable) := do
  let lctx ← getLCtx
  let lctx : LocalContext := lctx.sanitizeNames.run' { options := ← getOptions }
  let mut lctxCopied := LocalContext.mk lctx.fvarIdToDecl lctx.decls lctx.auxDeclToFullName
  let mut context := Array.mkEmpty lctx.size
  for ldecl in lctx do
    if ldecl.isImplementationDetail then
      continue
    let var ← match ldecl with
    | .cdecl _ id name type bi .. => do
    lctxCopied := lctxCopied.erase id
      let type ← instantiateMVars type
      pure {
        id := id.name,
        name := name.simpMacroScopes,
        binderInfo? := some bi,
        type := (← ppExpr type).pretty,
        value? := none,
        isProp := (← inferType type).isProp,
        isLet := ldecl.isLet
        isReferencedLater := lctxCopied.contains id
      }

    | .ldecl _ id name type value .. => do
      lctxCopied := lctxCopied.erase id
      let type ← instantiateMVars type
      let value ← try
        pure <| some (← ppExpr value).pretty
      catch _ => pure none
      pure {
        id := id.name,
        name := name.simpMacroScopes,
        binderInfo? := none,
        type := (← ppExpr type).pretty,
        value? := value,
        isProp := (← inferType type).isProp,
        isLet := ldecl.isLet,
        isReferencedLater := lctxCopied.contains id
      }
    context := context.push var
  return context

-- see Meta.ppGoal
def fromMVar (goal : MVarId) (extraFun : MVarId → MetaM (Option Json) := fun _ => pure none) : MetaM Goal :=
  goal.withContext do
    let context ← printContext
    let type ← goal.getType
    let tag ← goal.getTag
    let extra? ← extraFun goal
    return {
      tag,
      context,
      mvarId := goal.name,
      type := (← ppTerm (← delab type)).pretty,
      isProp := (← inferType type).isProp,
      pp := (← ppGoal goal).pretty,
      extra?,
    }

def fromTactic (extraFun : MVarId → MetaM (Option Json) := fun _ => pure none) : TacticM (Array Goal) := do
  (← getUnsolvedGoals).toArray.mapM fun mvar => Goal.fromMVar mvar extraFun

end Analyzer.Goal
