# jixia

A static analysis tool for Lean 4.

jixia is a new static analysis tool for Lean 4 with two main purposes in mind: building a Lean-aware IDE and extracting useful data for machine learning.

This project is part of BICMR@PKU AI for math program.

*jixia* stands for *稷下*, where [稷下学宫](https://en.wikipedia.org/wiki/Jixia_Academy) was historically located.

### Features

- Non-instrusive:  No change need to be made on the target file.  This improves cache utilization, notably on mathlib4.
- Single-file analysis
- Source-level info:  Includes information such as source range for each defined function, their arguments and return
  types, etc.
- Easy to extend:  jixia's plugin-based design makes it easy to extend while keeping all the advantages above.

### Information available

jixia comes with several plugins.
- Import: list of imported modules.
- Declaration: source-level info about each declaration (`def`, `theorem`, `inductive`, etc.).
- Symbol: info about symbols (or _constants_ in Lean 4 terminology) after elaboration, including their types and
  reference graph.
- Elaboration: info about the elaboration process, including tactic info.
- Line: proof state at the beginning of each line, as displayed in VSCode infoview.
- AST: a full dump of parsed commands.

### Running jixia

1. Clone the jixia source code, for example to `~/jixia`.
2. Build jixia.  `cd` into `~/jixia` then run `lake build`.  This will generate the executable file, usually located at `~/jixia/.lake/build/bin/jixia`.
3. Run the analyzer.

For a single file with no external dependencies, i.e., without an accompanying `lakefile.toml`/`lakefile.lean`, it must be compiled first:
```sh
lake env lean -o Example.olean Example.lean
```
then, running jixia with
```sh
~/jixia/.lake/build/bin/jixia -d Example.decl.json -s Example.sym.json -e Example.elab.json -l Example.lines.json Example.lean
```
will generate the corresponding json files from the declaration, symbol, elaboration, and line plugins.  If a flag is
omitted, the corresponding plugin will not run.

If you are analyzing a file within a project, you must first build it by running `lake build` (or, if your project is mathlib4 or depends on it, run `lake exe cache get` then `lake build`) in your project root, i.e., where `lakefile.toml`/`lakefile.lean` is located.  Then run
```sh
lake env ~/jixia/.lake/build/bin/jixia -d Example.decl.json -s Example.sym.json -e Example.elab.json -l Example.lines.json Example.lean
```
in your project root.

### Notes

##### Initializers

If your file contains `initialize` commands, you may need to use the `-i` flag to enable the execution of
initializers. In particular, you should include this flag when analyzing mathlib4.

##### Compiler Compatibility

jixia must be built with the *exact* same Lean version as the file or project being analyzed.  Check the target
project's `lean-toolchain`, then checkout the matching jixia release branch (recommended) or tag and run `lake build` before analyzing files.  For example, use the `release/v4.28.0-rc1` branch for projects on `leanprover/lean4:v4.28.0-rc1`.

Do not reuse a jixia executable built for another Lean version, even a nearby one; this can produce errors such as `invalid header`, missing constants, or other internal elaboration failures.

### FAQ

- `Unknown module prefix ... `: use `lake env`.

- `... cannot evaluate [init] declaration`: use `-i` flag.

- `... failed to read file ..., invalid header`: the Lean version used to build jixia must exactly match that of the target file/project.
