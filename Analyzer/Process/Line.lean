/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda
-/
import Lean
import Analyzer.Types
import Analyzer.Goal

open Lean Elab Meta Command

namespace Analyzer.Process.Line

-- see Lean.Server.FileWorker.getInteractiveTermGoal
def getGoalsAt (infoTree : InfoTree) (fileMap : FileMap) (pos : String.Pos.Raw) : IO (Array Goal) :=
  let goals := infoTree.goalsAt? fileMap pos
  goals.toArray.flatMapM fun r => Goal.fromTactic.runWithInfo r.useAfter r.ctxInfo r.tacticInfo

def onLoad : CommandElabM Unit :=
  enableInfoTree

def getResult : CommandElabM (Array LineInfo) := do
  let fileMap ← getFileMap
  let trees := (← getInfoTrees).toArray
  fileMap.positions.mapM fun p => do
    return {
      start := p,
      state := ← trees.flatMapM fun tree => getGoalsAt tree fileMap p
    }

end Analyzer.Process.Line
