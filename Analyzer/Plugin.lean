/-
Copyright (c) 2024 BICMR@PKU. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
Authors: Tony Beta Lambda
-/
import Lean
import Analyzer.Process.Module
import Analyzer.Process.Declaration
import Analyzer.Process.Symbol
import Analyzer.Process.Elaboration
import Analyzer.Process.Line
import Analyzer.Process.Augmentation

open Lean

namespace Analyzer.Process

structure Plugin where
  getResult : Name
  onLoad : Option Name

protected def plugins : Array (Name × Plugin) := #[
  (`module, ⟨ ``Module.getResult, none ⟩),
  (`declaration, ⟨ ``Declaration.getResult, ``Declaration.onLoad ⟩),
  (`elaboration, ⟨ ``Elaboration.getResult, ``Elaboration.onLoad ⟩),
  (`line, ⟨ ``Line.getResult, ``Line.onLoad ⟩),
  (`augmentation, ⟨``Augmentation.getResult, ``Augmentation.onLoad⟩)
]

end Analyzer.Process
