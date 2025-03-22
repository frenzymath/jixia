/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda
-/
import Lean
import Analyzer.Types

open Lean Elab Command

namespace Analyzer.Process.Module

def getResult : CommandElabM ModuleInfo := do
  let env ← getEnv
  let imports := env.header.imports.map fun i => i.module
  let docstring := getMainModuleDoc env |>.toArray |>.map fun d => d.doc
  return {
    imports,
    docstring,
  }

end Analyzer.Process.Module
