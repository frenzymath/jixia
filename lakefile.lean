import Lake

open Lake DSL

require metalib from git "https://github.com/reaslab/metalib.git" @ "main"
require Cli from git "https://github.com/leanprover/lean4-cli.git" @ "main"

require mathlib from git
  "https://github.com/leanprover-community/mathlib4" @ "v4.13.0"

package jixia where
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]

lean_lib Analyzer
@[default_target]
lean_exe jixia where
  root := `Main
  supportInterpreter := true
