# jixia

A static analysis tool for Lean 4.

jixia is a new static analysis tool for Lean 4 with two main purposes in mind: building a Lean-aware IDE and extracting
useful data for machine learning.

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

If you are analyzing a single file with no external dependencies, i.e., without an accompanying `lakefile.toml`/`lakefile.lean`:
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

jixia must be built with the *exact* same version of Lean as the file to be analyzed.  jixia is
known to be compatible with Lean v4.16.0.  To run jixia on a different version of Lean, try checking out a commit with a tag close to that version number, and then manually modify `lean-toolchain` before building jixia.  Some functionality may be missing, and the output format may differ significantly.

### FAQ

- `Unknown module prefix ... `: use `lake env`.

- `... cannot evaluate [init] declaration`: use `-i` flag.

- `... failed to read file ..., invalid header`: the Lean version used to build jixia must exactly match that of the target file/project.
