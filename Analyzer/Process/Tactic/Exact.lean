/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Kokic
-/
import Lean.CoreM
import Lean.MetavarContext
import Lean.Elab.Tactic.Basic
import Lean.Elab.Tactic.ElabTerm
import Lean.Meta.Tactic.Intro
import Lean.Meta.Tactic.LibrarySearch

open Lean Elab Meta Tactic
open LibrarySearch

namespace Analyzer.Process.Tactic.Exact

inductive GoalHint
  | exact : String → GoalHint
  | suggestions : List String → GoalHint

instance : ToString GoalHint where
  toString
    | .exact s => s
    | .suggestions s => s.toString

def toNamingMessage (ref : Syntax) (msgData : MessageData) : CoreM String := do
  let pos    := ref.getPos?.getD 0
  let endPos := ref.getTailPos?.getD pos
  let fileMap ← getFileMap
  let msgData ← addMessageContext msgData

  let msg : Message := {
    fileName := (← getFileName),
    pos := fileMap.toPosition pos,
    endPos := fileMap.toPosition endPos,
    data := msgData,
    severity := MessageSeverity.information
  }

  let coreContext : Core.Context ← read
  let namingContext : NamingContext := {
      currNamespace := coreContext.currNamespace,
      openDecls := coreContext.openDecls
  }
  let data : MessageData := MessageData.withNamingContext namingContext msg.data
  let msg : Message := { msg with data := data }
  let namingMessage := ← msg.toString
  return namingMessage

def getGoalExactHint (ref : Syntax)
                     (required: Option (Array (TSyntax `term)) := none)
    : TacticM GoalHint := do
  let mvar ← getMainGoal
  let (_, goal) ← mvar.intros
  let tacticM : TacticM GoalHint := do
    let required := (← (required.getD #[]).mapM getFVarId).toList.map .fvar
    let tactic := fun exfalso =>
      solveByElim required (exfalso := exfalso) (maxDepth := 6)
    let allowFailure := fun (g : MVarId) => do
      let g ← g.withContext (instantiateMVars (.mvar g))
      return required.all fun e => e.occurs g
    match ← librarySearch goal tactic allowFailure with
    | none =>
      let expr := (← instantiateMVars (mkMVar mvar)).headBeta
      let s ← toNamingMessage ref m!"{expr}"
      return GoalHint.exact s
    | some suggestions =>
      let results ← suggestions.mapM fun (_, suggestionMCtx) =>
        withMCtx suggestionMCtx do
          let expr := (← instantiateMVars (mkMVar mvar)).headBeta
          toNamingMessage ref m!"{expr}"
      admitGoal goal
      return GoalHint.suggestions results.toList
  return ← goal.withContext tacticM

end Analyzer.Process.Tactic.Exact
