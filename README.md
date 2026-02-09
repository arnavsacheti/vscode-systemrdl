# SystemRDL for Visual Studio Code

[![CI](https://github.com/SystemRDL/vscode-systemrdl/actions/workflows/ci.yml/badge.svg)](https://github.com/SystemRDL/vscode-systemrdl/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Language support for [SystemRDL](https://www.accellera.org/downloads/standards/systemrdl) files with [PeakRDL](https://peakrdl.readthedocs.io/) integration, powered by a Python-based language server.

## Features

- **Syntax Highlighting** — Full TextMate grammar and semantic token support for `.rdl` files
- **Diagnostics** — Real-time error checking via the SystemRDL compiler
- **Autocomplete & IntelliSense** — Context-aware completions for components, properties, and parameters
- **Inlay Hints** — Displays resolved parameter types and values inline
- **Semantic Highlighting** — Distinguishes components, instances, and properties with custom token types
- **PeakRDL Build Integration** — Run PeakRDL generators (`regblock`, `html`, `cheader`, etc.) directly from VS Code
- **Build on Save** — Optionally auto-generate outputs when `.rdl` files are saved

## Requirements

- VS Code 1.78.0 or later
- [Python extension for VS Code](https://marketplace.visualstudio.com/items?itemName=ms-python.python)
- Python 3.9 or later

## Installation

Install from the VS Code Marketplace by searching for **SystemRDL**, or install a `.vsix` file from the [Releases](https://github.com/SystemRDL/vscode-systemrdl/releases) page:

```
code --install-extension systemrdl.vsix
```

## Extension Settings

### Core

| Setting                       | Default        | Description                                              |
|-------------------------------|----------------|----------------------------------------------------------|
| `systemrdl.args`              | `[]`           | Additional arguments passed to the language server       |
| `systemrdl.path`              | `[]`           | Path to a custom SystemRDL tool binary                   |
| `systemrdl.interpreter`       | `[]`           | Path to the Python interpreter for the language server   |
| `systemrdl.importStrategy`    | `useBundled`   | Where to import `systemrdl-compiler` from                |
| `systemrdl.showNotifications` | `off`          | Controls when notifications are shown                    |
| `systemrdl.includePaths`      | `[]`           | Additional include paths for resolving imports           |

### PeakRDL

| Setting                              | Default      | Description                                              |
|--------------------------------------|--------------|----------------------------------------------------------|
| `systemrdl.peakrdl.buildOnSave`      | `false`      | Auto-run PeakRDL generation on save                      |
| `systemrdl.peakrdl.buildConfigPath`  | `""`         | Path to `peakrdl.toml` config file                       |
| `systemrdl.peakrdl.outputDirectory`  | `generated`  | Default output directory for generated files             |
| `systemrdl.peakrdl.generators`       | `[]`         | List of PeakRDL generators to run                        |

### Editor

| Setting                                   | Default | Description                              |
|-------------------------------------------|---------|------------------------------------------|
| `systemrdl.inlayHints.parameterTypes`     | `true`  | Show inlay hints for parameter types     |
| `systemrdl.inlayHints.parameterValues`    | `true`  | Show inlay hints for resolved values     |
| `systemrdl.semanticHighlighting`          | `true`  | Enable semantic syntax highlighting      |
| `systemrdl.diagnostics.enabled`           | `true`  | Enable real-time diagnostics             |

## Commands

| Command                        | Description                  |
|--------------------------------|------------------------------|
| `SystemRDL: Restart Server`    | Restart the language server  |
| `SystemRDL: Run PeakRDL Build` | Run PeakRDL code generation |

## PeakRDL Configuration

Create a `peakrdl.toml` in your workspace root to configure code generation. See the included [`peakrdl.toml.example`](peakrdl.toml.example) for a starting point.

## Development

### Prerequisites

- Node.js >= 18.17.0
- npm >= 8.19.0
- Python 3.9+
- [nox](https://nox.thea.codes/)

### Setup

```bash
python -m pip install nox
nox --session setup
npm install
```

### Building

```bash
npm run compile       # Development build
npm run package       # Production build
npm run vsce-package  # Build VSIX package
```

### Testing

```bash
nox --session tests
```

### Linting

```bash
nox --session lint          # Python (pylint, black, isort) + TypeScript (eslint)
npm run lint                # TypeScript only
npm run format-check        # Prettier formatting check
```

### Debugging

Use the **Debug Extension and Python** compound launch configuration in VS Code for full debugging of both the TypeScript extension and the Python language server.

## License

[MIT](LICENSE)

[pygls]: https://github.com/openlawlibrary/pygls
