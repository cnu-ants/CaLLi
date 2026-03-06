
# CaLLi 0.4v

CaLLi is an OCaml analysis library for LLVM IR.

It provides:
- LLVM IR preprocessing and transformation utilities
- customizable abstract domains, contexts, and transfer functions
- analyzer generators based on CaLLi IR
- an optional web-based monitor for interactive analysis inspection

## Requirements

- OCaml 4.13.1 or later
- Dune
- LLVM
- Node.js and npm (optional, only for the web monitor frontend)

## Installation

### 1) Using OPAM 
For now, OPAM installation is not supported. Only versions 0.2 and earlier are available through OPAM.

```bash
opam install calli
```

### 2) Building from source
```
dune build
opam install .
```
## Web Monitor (Optional)
CaLLi 0.4v includes an optional web monitor for inspecting CFG/ICFG structure, worklists, selected states, environments, breakpoints, and step-by-step execution.
To build the frontend once:
```
cd monitor
npm install
npm run build
```
Then run your analyzer in web mode:
```
dune exec ./example/analyzer.exe -- <target.bc> --web --calli-home /path/to/CaLLi --target <function_name>
```
You can also set the CaLLi root directory through an environment variable:
```
export CALLI_HOME=/path/to/CaLLi
dune exec ./example/analyzer.exe -- <target.bc> --web --target <function_name>
```
By default, the web server listens on port 8080.
Open:
http://localhost:8080

## Usage
Read our [Getting Started](https://github.com/cnu-ants/CaLLi/blob/main/example/README.md) page for details.

## License
CaLLi is released under the MIT License.

