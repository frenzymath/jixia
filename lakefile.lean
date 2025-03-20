import Lake

open Lake DSL

require metalib from git "https://github.com/reaslab/metalib.git" @ "main"
require Cli from git "https://github.com/leanprover/lean4-cli.git" @ s!"v{Lean.versionStringCore}"

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
