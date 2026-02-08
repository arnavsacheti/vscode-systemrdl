# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License.
"""SystemRDL Language Server implementation using systemrdl-compiler."""
from __future__ import annotations

import copy
import json
import os
import pathlib
import re
import subprocess
import sys
import tempfile
import traceback
from typing import Any, Dict, List, Optional, Sequence, Tuple


# **********************************************************
# Update sys.path before importing any bundled libraries.
# **********************************************************
def update_sys_path(path_to_add: str, strategy: str) -> None:
    """Add given path to `sys.path`."""
    if path_to_add not in sys.path and os.path.isdir(path_to_add):
        if strategy == "useBundled":
            sys.path.insert(0, path_to_add)
        elif strategy == "fromEnvironment":
            sys.path.append(path_to_add)


# Ensure that we can import LSP libraries, and other bundled libraries.
update_sys_path(
    os.fspath(pathlib.Path(__file__).parent.parent / "libs"),
    os.getenv("LS_IMPORT_STRATEGY", "useBundled"),
)

# **********************************************************
# Imports needed for the language server.
# **********************************************************
import lsp_jsonrpc as jsonrpc
import lsp_utils as utils
import lsprotocol.types as lsp
from pygls import server, uris, workspace

WORKSPACE_SETTINGS = {}
GLOBAL_SETTINGS = {}
RUNNER = pathlib.Path(__file__).parent / "lsp_runner.py"

MAX_WORKERS = 5
LSP_SERVER = server.LanguageServer(
    name="SystemRDL Language Server", version="0.1.0", max_workers=MAX_WORKERS
)

TOOL_MODULE = "systemrdl"
TOOL_DISPLAY = "SystemRDL"

# **********************************************************
# SystemRDL compiler integration
# **********************************************************

# Cache for compiled RDL models per file
_rdl_cache: Dict[str, Any] = {}
# Cache for document symbols (definitions, references)
_symbol_cache: Dict[str, Dict[str, Any]] = {}

# SystemRDL keywords for autocomplete
SYSTEMRDL_KEYWORDS = [
    "addrmap", "regfile", "reg", "field", "signal", "enum", "struct",
    "constraint", "mem", "property", "component", "type",
    "alias", "external", "internal", "abstract",
    "default", "if", "else", "for", "foreach", "while", "return",
    "const", "parameter", "localparam",
    "bit", "longint", "boolean", "string",
    "unsigned", "signed",
    "accesstype", "addressingtype", "onreadtype", "onwritetype",
    "true", "false",
    "encode", "inside", "all",
]

# SystemRDL built-in property names
SYSTEMRDL_PROPERTIES = [
    "sw", "hw", "reset", "resetsignal",
    "we", "wel", "swwe", "swwel",
    "hwenable", "hwmask", "haltmask", "haltenable",
    "sticky", "stickybit", "intr", "enable", "mask", "halt",
    "next", "overflow", "underflow",
    "incr", "decr", "incrvalue", "decrvalue",
    "incrsaturate", "decrsaturate", "incrthreshold", "decrthreshold",
    "saturate", "threshold", "counter",
    "compact", "regalign", "sharedextbus",
    "singlepulse", "paritycheck",
    "anded", "ored", "xored", "fieldwidth",
    "swmod", "swacc",
    "onread", "onwrite", "precedence",
    "desc", "name", "ispresent",
    "addressing", "alignment",
    "bigendian", "littleendian", "bridge",
    "accesswidth", "regwidth",
    "mementries", "memwidth",
    "rsvdset", "rsvdsetX",
    "hdl_path", "hdl_path_gate",
    "dontcompare", "donttest", "errextbus",
]

# Access type enum values
SYSTEMRDL_ACCESS_TYPES = [
    "rw", "wr", "r", "w", "rw1", "w1", "na",
]

SYSTEMRDL_ONREAD_TYPES = [
    "rclr", "rset", "ruser",
]

SYSTEMRDL_ONWRITE_TYPES = [
    "woset", "woclr", "wot", "wzs", "wzc", "wzt", "wclr", "wset", "wuser",
]

SYSTEMRDL_ADDRESSING_TYPES = [
    "compact", "regalign", "fullalign",
]

# Component type descriptions for hover info
COMPONENT_DESCRIPTIONS = {
    "addrmap": "Address map - Top-level container that defines the address space and maps registers into it.",
    "regfile": "Register file - A grouping of registers that can be reused and instantiated.",
    "reg": "Register - Defines a hardware register with one or more fields.",
    "field": "Field - A bit-field within a register, defining specific bits.",
    "signal": "Signal - An external input/output signal used for hardware connectivity.",
    "enum": "Enumeration - Defines named constants for use in field encoding.",
    "struct": "Struct - A user-defined structured data type.",
    "constraint": "Constraint - Defines rules for legal field values.",
    "mem": "Memory - Defines a memory region in the address map.",
}

PROPERTY_DESCRIPTIONS = {
    "sw": "Software access type (rw, r, w, na). Defines how software can access this field.",
    "hw": "Hardware access type (rw, r, w, na). Defines how hardware can access this field.",
    "reset": "Reset value. Specifies the value of the field after reset.",
    "resetsignal": "Specifies the reset signal used for this component.",
    "we": "Write enable. Specifies a signal that gates software writes.",
    "wel": "Write enable level. Active-low version of `we`.",
    "desc": "Description string for documentation purposes.",
    "name": "Display name for the component.",
    "next": "Specifies the next value source for the field from hardware.",
    "intr": "Marks this field as an interrupt field.",
    "enable": "Specifies the enable signal for an interrupt field.",
    "mask": "Specifies the mask signal for an interrupt field.",
    "halt": "Specifies the halt signal for an interrupt field.",
    "sticky": "Makes the field retain its value (sticky interrupt).",
    "stickybit": "Makes each bit in the field sticky.",
    "counter": "Marks the field as a hardware counter.",
    "incr": "Signal to increment the counter.",
    "decr": "Signal to decrement the counter.",
    "overflow": "Overflow indicator for counter fields.",
    "underflow": "Underflow indicator for counter fields.",
    "singlepulse": "Field is automatically cleared after being written to 1.",
    "swmod": "Indicates if software modifies this field.",
    "swacc": "Indicates if software accesses this field.",
    "onread": "Action taken on read (rclr, rset, ruser).",
    "onwrite": "Action taken on write (woset, woclr, wot, wzs, wzc, wzt, wclr, wset, wuser).",
    "precedence": "Defines priority when both software and hardware write simultaneously.",
    "addressing": "Addressing mode for the address map (compact, regalign, fullalign).",
    "regwidth": "Width of registers in bits (default 32).",
    "accesswidth": "Minimum software access width in bits.",
    "bigendian": "Address map uses big-endian byte order.",
    "littleendian": "Address map uses little-endian byte order.",
}


def _try_compile_rdl(source: str, file_path: str) -> Tuple[Optional[Any], List[lsp.Diagnostic]]:
    """Try to compile RDL source and return (root, diagnostics).

    Since systemrdl-compiler only supports compile_file(path) and has no
    in-memory compile API, we write the current buffer text to a temp file
    so that diagnostics always reflect the editor's unsaved state.
    """
    diagnostics: List[lsp.Diagnostic] = []
    try:
        import systemrdl  # type: ignore
    except ImportError:
        return None, diagnostics

    tmp_path = None
    try:
        # Write the in-memory source to a temp file so the compiler sees
        # the current buffer content, not the (possibly stale) on-disk file.
        tmp_dir = os.path.dirname(file_path) or None
        with tempfile.NamedTemporaryFile(
            mode="w",
            suffix=".rdl",
            dir=tmp_dir,
            delete=False,
            encoding="utf-8",
        ) as tmp:
            tmp.write(source)
            tmp_path = tmp.name

        rdlc = systemrdl.RDLCompiler()
        rdlc.compile_file(tmp_path)
        root = rdlc.elaborate()
        _rdl_cache[file_path] = root
        return root, diagnostics
    except Exception as e:
        # Replace temp file path in error messages so diagnostics
        # reference the original file, not the ephemeral temp file.
        error_text = str(e)
        if tmp_path:
            error_text = error_text.replace(tmp_path, file_path)
        diag = _parse_compiler_error(error_text, file_path)
        if diag:
            diagnostics.extend(diag)
        else:
            diagnostics.append(
                lsp.Diagnostic(
                    range=lsp.Range(
                        start=lsp.Position(line=0, character=0),
                        end=lsp.Position(line=0, character=0),
                    ),
                    message=error_text,
                    severity=lsp.DiagnosticSeverity.Error,
                    source=TOOL_MODULE,
                )
            )
        return None, diagnostics
    finally:
        if tmp_path:
            try:
                os.unlink(tmp_path)
            except OSError:
                pass


def _parse_compiler_error(error_str: str, file_path: str) -> List[lsp.Diagnostic]:
    """Parse systemrdl-compiler error messages into LSP diagnostics."""
    diagnostics = []

    # Common error patterns from systemrdl-compiler
    # Pattern: "filepath:line:col: error: message"
    pattern = re.compile(
        r"(?:.*?):(\d+):(\d+):\s*(error|warning|info):\s*(.*?)(?:\n|$)",
        re.MULTILINE | re.IGNORECASE,
    )

    for match in pattern.finditer(error_str):
        line = max(int(match.group(1)) - 1, 0)
        col = max(int(match.group(2)) - 1, 0)
        severity_str = match.group(3).lower()
        message = match.group(4).strip()

        severity_map = {
            "error": lsp.DiagnosticSeverity.Error,
            "warning": lsp.DiagnosticSeverity.Warning,
            "info": lsp.DiagnosticSeverity.Information,
        }

        diagnostics.append(
            lsp.Diagnostic(
                range=lsp.Range(
                    start=lsp.Position(line=line, character=col),
                    end=lsp.Position(line=line, character=col),
                ),
                message=message,
                severity=severity_map.get(severity_str, lsp.DiagnosticSeverity.Error),
                source=TOOL_MODULE,
            )
        )

    # Also try simpler pattern: "line N: message"
    if not diagnostics:
        simple_pattern = re.compile(r"line\s+(\d+)", re.IGNORECASE)
        match = simple_pattern.search(error_str)
        if match:
            line = max(int(match.group(1)) - 1, 0)
            diagnostics.append(
                lsp.Diagnostic(
                    range=lsp.Range(
                        start=lsp.Position(line=line, character=0),
                        end=lsp.Position(line=line, character=0),
                    ),
                    message=error_str.strip(),
                    severity=lsp.DiagnosticSeverity.Error,
                    source=TOOL_MODULE,
                )
            )

    return diagnostics


def _extract_symbols_from_source(source: str, uri: str) -> Dict[str, Any]:
    """Extract symbol information from RDL source using regex-based parsing.

    This provides go-to-definition, find references, and completion support
    without requiring a full compilation pass.
    """
    symbols: Dict[str, Any] = {
        "definitions": {},  # name -> {line, col, kind, type_name, parent}
        "references": {},   # name -> [{line, col}]
        "parameters": {},   # name -> {line, col, type, default_value}
        "instances": {},    # name -> {line, col, type_name}
        "components": [],   # [{name, line, col, kind, children}]
    }

    lines = source.split("\n")

    # Pattern for component definitions:
    # addrmap name { ... }
    # reg name { ... }
    comp_def_re = re.compile(
        r"\b(addrmap|regfile|reg|field|signal|enum|struct|constraint|mem)\s+"
        r"([a-zA-Z_][a-zA-Z0-9_]*)\s*(?:#\s*\(.*?\))?\s*\{",
        re.DOTALL,
    )

    # Pattern for component instances:
    # type_name instance_name;
    # type_name instance_name @ 0x100;
    # type_name instance_name[N];
    inst_re = re.compile(
        r"\b([a-zA-Z_][a-zA-Z0-9_]*)\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*"
        r"(?:\[.*?\])?\s*(?:@\s*[^\s;]+)?\s*;"
    )

    # Pattern for parameter definitions
    param_re = re.compile(
        r"\b(parameter|localparam)\s+(bit|longint|boolean|string|unsigned|signed"
        r"|longint\s+unsigned|longint\s+signed)?\s*"
        r"(?:\[.*?\])?\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*([^;]+);"
    )

    # Pattern for property assignments
    prop_assign_re = re.compile(
        r"\b([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*"
    )

    # Pattern for enum entries
    enum_entry_re = re.compile(
        r"\b([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*(\d+|0x[0-9a-fA-F]+)\s*;"
    )

    # First pass: find all definitions
    for line_num, line_text in enumerate(lines):
        stripped = line_text.strip()

        # Skip comments
        if stripped.startswith("//"):
            continue

        # Component definitions
        for m in comp_def_re.finditer(line_text):
            comp_type = m.group(1)
            comp_name = m.group(2)
            col = m.start(2)
            symbols["definitions"][comp_name] = {
                "line": line_num,
                "col": col,
                "kind": comp_type,
                "uri": uri,
            }
            symbols["components"].append({
                "name": comp_name,
                "line": line_num,
                "col": col,
                "kind": comp_type,
            })

        # Parameter definitions
        for m in param_re.finditer(line_text):
            param_keyword = m.group(1)
            param_type = (m.group(2) or "").strip()
            param_name = m.group(3)
            param_default = m.group(4).strip()
            col = m.start(3)
            symbols["parameters"][param_name] = {
                "line": line_num,
                "col": col,
                "type": param_type if param_type else "bit",
                "default_value": param_default,
                "keyword": param_keyword,
                "uri": uri,
            }
            symbols["definitions"][param_name] = {
                "line": line_num,
                "col": col,
                "kind": "parameter",
                "uri": uri,
            }

        # Instance patterns (only if not a keyword line)
        if not any(stripped.startswith(k) for k in SYSTEMRDL_KEYWORDS):
            for m in inst_re.finditer(line_text):
                type_name = m.group(1)
                inst_name = m.group(2)
                # Skip if type_name is a keyword
                if type_name in SYSTEMRDL_KEYWORDS or type_name in SYSTEMRDL_PROPERTIES:
                    continue
                col = m.start(2)
                symbols["instances"][inst_name] = {
                    "line": line_num,
                    "col": col,
                    "type_name": type_name,
                    "uri": uri,
                }

    # Second pass: find references (all identifier usages)
    ident_re = re.compile(r"\b([a-zA-Z_][a-zA-Z0-9_]*)\b")
    in_block_comment = False
    for line_num, line_text in enumerate(lines):
        # Handle block comments
        if "/*" in line_text:
            in_block_comment = True
        if "*/" in line_text:
            in_block_comment = False
            continue
        if in_block_comment:
            continue

        # Remove line comments
        comment_idx = line_text.find("//")
        if comment_idx >= 0:
            line_text = line_text[:comment_idx]

        # Remove string literals
        line_text = re.sub(r'"[^"]*"', '""', line_text)

        for m in ident_re.finditer(line_text):
            name = m.group(1)
            if name in SYSTEMRDL_KEYWORDS or name in SYSTEMRDL_PROPERTIES:
                continue
            if name not in symbols["references"]:
                symbols["references"][name] = []
            symbols["references"][name].append({
                "line": line_num,
                "col": m.start(1),
            })

    return symbols


# **********************************************************
# Semantic Token Support
# **********************************************************

# Semantic token types (must match order in legend)
SEMANTIC_TOKEN_TYPES = [
    "namespace",      # 0 - addrmap
    "class",          # 1 - component types (reg, regfile, etc.)
    "enum",           # 2 - enum
    "type",           # 3 - type references
    "parameter",      # 4 - parameters
    "variable",       # 5 - instances
    "property",       # 6 - properties
    "enumMember",     # 7 - enum values
    "keyword",        # 8 - keywords
    "number",         # 9 - numbers
    "string",         # 10 - strings
    "comment",        # 11 - comments
    "operator",       # 12 - operators
    "macro",          # 13 - preprocessor directives
]

SEMANTIC_TOKEN_MODIFIERS = [
    "declaration",    # 0
    "definition",     # 1
    "readonly",       # 2
    "modification",   # 3
]


def _compute_semantic_tokens(source: str, uri: str) -> List[int]:
    """Compute semantic tokens for the given source.

    Returns encoded tokens as a flat list of integers.
    Each token is 5 integers: deltaLine, deltaStart, length, tokenType, tokenModifiers.
    """
    tokens: List[Tuple[int, int, int, int, int]] = []
    lines = source.split("\n")

    in_block_comment = False

    for line_num, line_text in enumerate(lines):
        col = 0

        # Handle block comments
        while col < len(line_text):
            if in_block_comment:
                end_idx = line_text.find("*/", col)
                if end_idx >= 0:
                    tokens.append((line_num, col, end_idx + 2 - col, 11, 0))
                    col = end_idx + 2
                    in_block_comment = False
                else:
                    tokens.append((line_num, col, len(line_text) - col, 11, 0))
                    col = len(line_text)
                continue

            start_idx = line_text.find("/*", col)
            line_comment_idx = line_text.find("//", col)

            # Line comment takes precedence if it comes first
            if line_comment_idx >= 0 and (start_idx < 0 or line_comment_idx < start_idx):
                tokens.append((line_num, line_comment_idx, len(line_text) - line_comment_idx, 11, 0))
                break

            if start_idx >= 0:
                # Process text before block comment
                _tokenize_segment(line_text[col:start_idx], line_num, col, tokens, uri)
                end_idx = line_text.find("*/", start_idx + 2)
                if end_idx >= 0:
                    tokens.append((line_num, start_idx, end_idx + 2 - start_idx, 11, 0))
                    col = end_idx + 2
                else:
                    tokens.append((line_num, start_idx, len(line_text) - start_idx, 11, 0))
                    in_block_comment = True
                    col = len(line_text)
                continue

            # No more comments, tokenize the rest of the line
            _tokenize_segment(line_text[col:], line_num, col, tokens, uri)
            break

    # Sort by position
    tokens.sort(key=lambda t: (t[0], t[1]))

    # Encode as delta format
    result: List[int] = []
    prev_line = 0
    prev_start = 0
    for line_n, start, length, token_type, token_modifiers in tokens:
        delta_line = line_n - prev_line
        delta_start = start - prev_start if delta_line == 0 else start
        result.extend([delta_line, delta_start, length, token_type, token_modifiers])
        prev_line = line_n
        prev_start = start

    return result


def _tokenize_segment(
    text: str, line_num: int, col_offset: int,
    tokens: List[Tuple[int, int, int, int, int]], uri: str
) -> None:
    """Tokenize a segment of a line (no comments in this segment)."""
    # Skip strings
    string_re = re.compile(r'"[^"]*"')
    for m in string_re.finditer(text):
        tokens.append((line_num, col_offset + m.start(), m.end() - m.start(), 10, 0))

    # Remove strings for identifier analysis
    clean = string_re.sub(lambda m: " " * (m.end() - m.start()), text)

    # Preprocessor directives
    preproc_re = re.compile(r"`(include|define|undef|ifdef|ifndef|else|endif|line|pragma)\b")
    for m in preproc_re.finditer(clean):
        tokens.append((line_num, col_offset + m.start(), m.end() - m.start(), 13, 0))

    # Numbers (hex, binary, decimal, verilog-style)
    num_re = re.compile(r"\b(0[xX][0-9A-Fa-f_]+|0[bB][01_]+|\d+'[bBhHdDoO][0-9A-Fa-f_xXzZ]+|\d[\d_]*)\b")
    for m in num_re.finditer(clean):
        tokens.append((line_num, col_offset + m.start(), m.end() - m.start(), 9, 0))

    # Keywords and identifiers
    ident_re = re.compile(r"\b([a-zA-Z_][a-zA-Z0-9_]*)\b")
    # Get symbols for this file
    symbols = _symbol_cache.get(uri, {})
    definitions = symbols.get("definitions", {})
    parameters = symbols.get("parameters", {})

    component_types = {"addrmap", "regfile", "reg", "field", "signal", "enum", "struct", "constraint", "mem"}

    for m in ident_re.finditer(clean):
        name = m.group(1)
        start = col_offset + m.start()
        length = m.end() - m.start()

        if name in component_types:
            tokens.append((line_num, start, length, 1, 0))
        elif name in {"parameter", "localparam", "const", "property", "component",
                       "type", "alias", "external", "internal", "abstract",
                       "default", "if", "else", "for", "foreach", "while", "return",
                       "inside", "encode", "all"}:
            tokens.append((line_num, start, length, 8, 0))
        elif name in {"bit", "longint", "boolean", "string", "unsigned", "signed",
                       "accesstype", "addressingtype", "onreadtype", "onwritetype"}:
            tokens.append((line_num, start, length, 3, 0))
        elif name in {"true", "false"}:
            tokens.append((line_num, start, length, 9, 0))
        elif name in SYSTEMRDL_PROPERTIES:
            tokens.append((line_num, start, length, 6, 0))
        elif name in (SYSTEMRDL_ACCESS_TYPES + SYSTEMRDL_ONREAD_TYPES +
                       SYSTEMRDL_ONWRITE_TYPES + SYSTEMRDL_ADDRESSING_TYPES):
            tokens.append((line_num, start, length, 7, 0))
        elif name in parameters:
            modifier = 1 if parameters[name].get("line") == line_num else 0
            tokens.append((line_num, start, length, 4, modifier))
        elif name in definitions:
            defn = definitions[name]
            if defn.get("kind") == "enum":
                tokens.append((line_num, start, length, 2, 0))
            elif defn.get("kind") in component_types:
                modifier = 1 if defn.get("line") == line_num else 0
                tokens.append((line_num, start, length, 1, modifier))
            else:
                tokens.append((line_num, start, length, 5, 0))


# **********************************************************
# LSP Feature Handlers
# **********************************************************

@LSP_SERVER.feature(lsp.TEXT_DOCUMENT_DID_OPEN)
def did_open(params: lsp.DidOpenTextDocumentParams) -> None:
    """LSP handler for textDocument/didOpen request."""
    document = LSP_SERVER.workspace.get_document(params.text_document.uri)
    _update_symbols(document)
    diagnostics = _get_diagnostics(document)
    LSP_SERVER.publish_diagnostics(document.uri, diagnostics)


@LSP_SERVER.feature(lsp.TEXT_DOCUMENT_DID_SAVE)
def did_save(params: lsp.DidSaveTextDocumentParams) -> None:
    """LSP handler for textDocument/didSave request."""
    document = LSP_SERVER.workspace.get_document(params.text_document.uri)
    _update_symbols(document)
    diagnostics = _get_diagnostics(document)
    LSP_SERVER.publish_diagnostics(document.uri, diagnostics)

    # Trigger PeakRDL build on save if configured
    settings = _get_settings_by_document(document)
    if settings.get("peakrdlBuildOnSave", False):
        _run_peakrdl_build(document, settings)


@LSP_SERVER.feature(lsp.TEXT_DOCUMENT_DID_CLOSE)
def did_close(params: lsp.DidCloseTextDocumentParams) -> None:
    """LSP handler for textDocument/didClose request."""
    document = LSP_SERVER.workspace.get_document(params.text_document.uri)
    LSP_SERVER.publish_diagnostics(document.uri, [])
    # Clean up caches
    file_path = uris.to_fs_path(document.uri)
    _rdl_cache.pop(file_path, None)
    _symbol_cache.pop(document.uri, None)


@LSP_SERVER.feature(lsp.TEXT_DOCUMENT_DID_CHANGE)
def did_change(params: lsp.DidChangeTextDocumentParams) -> None:
    """LSP handler for textDocument/didChange request."""
    document = LSP_SERVER.workspace.get_document(params.text_document.uri)
    _update_symbols(document)
    diagnostics = _get_diagnostics(document)
    LSP_SERVER.publish_diagnostics(document.uri, diagnostics)


def _update_symbols(document: workspace.Document) -> None:
    """Update the symbol cache for a document."""
    try:
        symbols = _extract_symbols_from_source(document.source, document.uri)
        _symbol_cache[document.uri] = symbols
    except Exception:
        log_error(f"Error extracting symbols: {traceback.format_exc()}")


def _get_diagnostics(document: workspace.Document) -> List[lsp.Diagnostic]:
    """Get diagnostics for a document using systemrdl-compiler."""
    file_path = uris.to_fs_path(document.uri)
    _, diagnostics = _try_compile_rdl(document.source, file_path)
    return diagnostics


# **********************************************************
# Hover support
# **********************************************************

@LSP_SERVER.feature(lsp.TEXT_DOCUMENT_HOVER)
def hover(params: lsp.TextDocumentPositionParams) -> Optional[lsp.Hover]:
    """LSP handler for textDocument/hover request."""
    document = LSP_SERVER.workspace.get_document(params.text_document.uri)
    word = _get_word_at_position(document.source, params.position)
    if not word:
        return None

    # Check if it's a component keyword
    if word in COMPONENT_DESCRIPTIONS:
        return lsp.Hover(
            contents=lsp.MarkupContent(
                kind=lsp.MarkupKind.Markdown,
                value=f"**{word}** (component type)\n\n{COMPONENT_DESCRIPTIONS[word]}",
            )
        )

    # Check if it's a property
    if word in PROPERTY_DESCRIPTIONS:
        return lsp.Hover(
            contents=lsp.MarkupContent(
                kind=lsp.MarkupKind.Markdown,
                value=f"**{word}** (property)\n\n{PROPERTY_DESCRIPTIONS[word]}",
            )
        )

    # Check if it's a known symbol
    symbols = _symbol_cache.get(document.uri, {})

    # Check parameters
    if word in symbols.get("parameters", {}):
        param = symbols["parameters"][word]
        param_type = param.get("type", "bit")
        default_val = param.get("default_value", "")
        keyword = param.get("keyword", "parameter")
        hover_text = f"**{word}** ({keyword})\n\n"
        hover_text += f"- Type: `{param_type}`\n"
        if default_val:
            hover_text += f"- Default: `{default_val}`\n"
        return lsp.Hover(
            contents=lsp.MarkupContent(
                kind=lsp.MarkupKind.Markdown,
                value=hover_text,
            )
        )

    # Check definitions
    if word in symbols.get("definitions", {}):
        defn = symbols["definitions"][word]
        kind = defn.get("kind", "unknown")
        hover_text = f"**{word}** ({kind})\n\n"
        if kind in COMPONENT_DESCRIPTIONS:
            hover_text += COMPONENT_DESCRIPTIONS[kind]
        hover_text += f"\n\nDefined at line {defn['line'] + 1}"
        return lsp.Hover(
            contents=lsp.MarkupContent(
                kind=lsp.MarkupKind.Markdown,
                value=hover_text,
            )
        )

    # Check instances
    if word in symbols.get("instances", {}):
        inst = symbols["instances"][word]
        type_name = inst.get("type_name", "unknown")
        hover_text = f"**{word}** (instance of `{type_name}`)\n\n"
        hover_text += f"Instantiated at line {inst['line'] + 1}"
        return lsp.Hover(
            contents=lsp.MarkupContent(
                kind=lsp.MarkupKind.Markdown,
                value=hover_text,
            )
        )

    # Check if it's an access type
    if word in SYSTEMRDL_ACCESS_TYPES:
        return lsp.Hover(
            contents=lsp.MarkupContent(
                kind=lsp.MarkupKind.Markdown,
                value=f"**{word}** (access type)\n\nSoftware/hardware access mode.",
            )
        )

    if word in SYSTEMRDL_ONREAD_TYPES:
        return lsp.Hover(
            contents=lsp.MarkupContent(
                kind=lsp.MarkupKind.Markdown,
                value=f"**{word}** (onread type)\n\nAction performed when the field is read.",
            )
        )

    if word in SYSTEMRDL_ONWRITE_TYPES:
        return lsp.Hover(
            contents=lsp.MarkupContent(
                kind=lsp.MarkupKind.Markdown,
                value=f"**{word}** (onwrite type)\n\nAction performed when the field is written.",
            )
        )

    return None


# **********************************************************
# Go-to Definition
# **********************************************************

@LSP_SERVER.feature(lsp.TEXT_DOCUMENT_DEFINITION)
def definition(params: lsp.TextDocumentPositionParams) -> Optional[lsp.Location]:
    """LSP handler for textDocument/definition request."""
    document = LSP_SERVER.workspace.get_document(params.text_document.uri)
    word = _get_word_at_position(document.source, params.position)
    if not word:
        return None

    symbols = _symbol_cache.get(document.uri, {})

    # Check definitions
    if word in symbols.get("definitions", {}):
        defn = symbols["definitions"][word]
        target_uri = defn.get("uri", document.uri)
        return lsp.Location(
            uri=target_uri,
            range=lsp.Range(
                start=lsp.Position(line=defn["line"], character=defn["col"]),
                end=lsp.Position(line=defn["line"], character=defn["col"] + len(word)),
            ),
        )

    # Check parameters
    if word in symbols.get("parameters", {}):
        param = symbols["parameters"][word]
        target_uri = param.get("uri", document.uri)
        return lsp.Location(
            uri=target_uri,
            range=lsp.Range(
                start=lsp.Position(line=param["line"], character=param["col"]),
                end=lsp.Position(line=param["line"], character=param["col"] + len(word)),
            ),
        )

    # Check instances - go to the type definition
    if word in symbols.get("instances", {}):
        inst = symbols["instances"][word]
        type_name = inst.get("type_name", "")
        if type_name in symbols.get("definitions", {}):
            defn = symbols["definitions"][type_name]
            target_uri = defn.get("uri", document.uri)
            return lsp.Location(
                uri=target_uri,
                range=lsp.Range(
                    start=lsp.Position(line=defn["line"], character=defn["col"]),
                    end=lsp.Position(line=defn["line"], character=defn["col"] + len(type_name)),
                ),
            )

    return None


# **********************************************************
# Find References
# **********************************************************

@LSP_SERVER.feature(lsp.TEXT_DOCUMENT_REFERENCES)
def references(params: lsp.ReferenceParams) -> Optional[List[lsp.Location]]:
    """LSP handler for textDocument/references request."""
    document = LSP_SERVER.workspace.get_document(params.text_document.uri)
    word = _get_word_at_position(document.source, params.position)
    if not word:
        return None

    symbols = _symbol_cache.get(document.uri, {})
    refs = symbols.get("references", {}).get(word, [])

    if not refs:
        return None

    locations = []
    for ref in refs:
        locations.append(
            lsp.Location(
                uri=document.uri,
                range=lsp.Range(
                    start=lsp.Position(line=ref["line"], character=ref["col"]),
                    end=lsp.Position(line=ref["line"], character=ref["col"] + len(word)),
                ),
            )
        )

    return locations if locations else None


# **********************************************************
# Document Symbols
# **********************************************************

@LSP_SERVER.feature(lsp.TEXT_DOCUMENT_DOCUMENT_SYMBOL)
def document_symbol(
    params: lsp.DocumentSymbolParams,
) -> Optional[List[lsp.DocumentSymbol]]:
    """LSP handler for textDocument/documentSymbol request."""
    document = LSP_SERVER.workspace.get_document(params.text_document.uri)
    symbols = _symbol_cache.get(document.uri, {})

    result: List[lsp.DocumentSymbol] = []

    symbol_kind_map = {
        "addrmap": lsp.SymbolKind.Module,
        "regfile": lsp.SymbolKind.Package,
        "reg": lsp.SymbolKind.Class,
        "field": lsp.SymbolKind.Field,
        "signal": lsp.SymbolKind.Event,
        "enum": lsp.SymbolKind.Enum,
        "struct": lsp.SymbolKind.Struct,
        "constraint": lsp.SymbolKind.Interface,
        "mem": lsp.SymbolKind.Array,
        "parameter": lsp.SymbolKind.Constant,
    }

    # Add component definitions
    for comp in symbols.get("components", []):
        kind = symbol_kind_map.get(comp["kind"], lsp.SymbolKind.Variable)
        sym_range = lsp.Range(
            start=lsp.Position(line=comp["line"], character=comp["col"]),
            end=lsp.Position(line=comp["line"], character=comp["col"] + len(comp["name"])),
        )
        result.append(
            lsp.DocumentSymbol(
                name=comp["name"],
                kind=kind,
                range=sym_range,
                selection_range=sym_range,
                detail=comp["kind"],
            )
        )

    # Add parameters
    for name, param in symbols.get("parameters", {}).items():
        param_range = lsp.Range(
            start=lsp.Position(line=param["line"], character=param["col"]),
            end=lsp.Position(line=param["line"], character=param["col"] + len(name)),
        )
        result.append(
            lsp.DocumentSymbol(
                name=name,
                kind=lsp.SymbolKind.Constant,
                range=param_range,
                selection_range=param_range,
                detail=f"{param.get('keyword', 'parameter')} {param.get('type', 'bit')}",
            )
        )

    # Add instances
    for name, inst in symbols.get("instances", {}).items():
        inst_range = lsp.Range(
            start=lsp.Position(line=inst["line"], character=inst["col"]),
            end=lsp.Position(line=inst["line"], character=inst["col"] + len(name)),
        )
        result.append(
            lsp.DocumentSymbol(
                name=name,
                kind=lsp.SymbolKind.Variable,
                range=inst_range,
                selection_range=inst_range,
                detail=f"instance of {inst.get('type_name', 'unknown')}",
            )
        )

    return result if result else None


# **********************************************************
# Completion
# **********************************************************

@LSP_SERVER.feature(
    lsp.TEXT_DOCUMENT_COMPLETION,
    lsp.CompletionOptions(trigger_characters=[".", "="]),
)
def completion(params: lsp.CompletionParams) -> Optional[lsp.CompletionList]:
    """LSP handler for textDocument/completion request."""
    document = LSP_SERVER.workspace.get_document(params.text_document.uri)
    symbols = _symbol_cache.get(document.uri, {})

    items: List[lsp.CompletionItem] = []

    # Get the current line context
    lines = document.source.split("\n")
    line_num = params.position.line
    if line_num < len(lines):
        line_text = lines[line_num][:params.position.character]
    else:
        line_text = ""

    # After '=' sign, suggest values
    if line_text.rstrip().endswith("="):
        prop_match = re.search(r"\b(\w+)\s*=$", line_text.rstrip())
        if prop_match:
            prop_name = prop_match.group(1)
            if prop_name in ("sw", "hw"):
                for at in SYSTEMRDL_ACCESS_TYPES:
                    items.append(lsp.CompletionItem(
                        label=at,
                        kind=lsp.CompletionItemKind.EnumMember,
                        detail="access type",
                    ))
            elif prop_name == "onread":
                for ort in SYSTEMRDL_ONREAD_TYPES:
                    items.append(lsp.CompletionItem(
                        label=ort,
                        kind=lsp.CompletionItemKind.EnumMember,
                        detail="onread type",
                    ))
            elif prop_name == "onwrite":
                for owt in SYSTEMRDL_ONWRITE_TYPES:
                    items.append(lsp.CompletionItem(
                        label=owt,
                        kind=lsp.CompletionItemKind.EnumMember,
                        detail="onwrite type",
                    ))
            elif prop_name == "addressing":
                for adt in SYSTEMRDL_ADDRESSING_TYPES:
                    items.append(lsp.CompletionItem(
                        label=adt,
                        kind=lsp.CompletionItemKind.EnumMember,
                        detail="addressing type",
                    ))
            else:
                # Suggest boolean values
                items.append(lsp.CompletionItem(label="true", kind=lsp.CompletionItemKind.Keyword))
                items.append(lsp.CompletionItem(label="false", kind=lsp.CompletionItemKind.Keyword))
        return lsp.CompletionList(is_incomplete=False, items=items)

    # After '.', suggest properties
    if "." in line_text:
        for prop in SYSTEMRDL_PROPERTIES:
            desc = PROPERTY_DESCRIPTIONS.get(prop, "")
            items.append(lsp.CompletionItem(
                label=prop,
                kind=lsp.CompletionItemKind.Property,
                detail="SystemRDL property",
                documentation=desc if desc else None,
            ))
        return lsp.CompletionList(is_incomplete=False, items=items)

    # Default: suggest keywords, component types, properties, and local symbols
    for kw in SYSTEMRDL_KEYWORDS:
        detail = ""
        if kw in COMPONENT_DESCRIPTIONS:
            detail = "component type"
        elif kw in {"parameter", "localparam", "const"}:
            detail = "storage modifier"
        elif kw in {"bit", "longint", "boolean", "string", "unsigned", "signed"}:
            detail = "type"
        else:
            detail = "keyword"
        items.append(lsp.CompletionItem(
            label=kw,
            kind=lsp.CompletionItemKind.Keyword,
            detail=detail,
        ))

    for prop in SYSTEMRDL_PROPERTIES:
        desc = PROPERTY_DESCRIPTIONS.get(prop, "")
        items.append(lsp.CompletionItem(
            label=prop,
            kind=lsp.CompletionItemKind.Property,
            detail="property",
            documentation=desc if desc else None,
        ))

    # Add user-defined symbols
    for name, defn in symbols.get("definitions", {}).items():
        kind = defn.get("kind", "")
        if kind in {"addrmap", "regfile", "reg", "field", "signal", "struct", "mem"}:
            items.append(lsp.CompletionItem(
                label=name,
                kind=lsp.CompletionItemKind.Class,
                detail=f"{kind} definition",
            ))
        elif kind == "enum":
            items.append(lsp.CompletionItem(
                label=name,
                kind=lsp.CompletionItemKind.Enum,
                detail="enum definition",
            ))
        elif kind == "parameter":
            items.append(lsp.CompletionItem(
                label=name,
                kind=lsp.CompletionItemKind.Constant,
                detail="parameter",
            ))

    return lsp.CompletionList(is_incomplete=False, items=items)


# **********************************************************
# Semantic Tokens
# **********************************************************

@LSP_SERVER.feature(
    lsp.TEXT_DOCUMENT_SEMANTIC_TOKENS_FULL,
    lsp.SemanticTokensOptions(
        legend=lsp.SemanticTokensLegend(
            token_types=SEMANTIC_TOKEN_TYPES,
            token_modifiers=SEMANTIC_TOKEN_MODIFIERS,
        ),
        full=True,
    ),
)
def semantic_tokens_full(params: lsp.SemanticTokensParams) -> Optional[lsp.SemanticTokens]:
    """LSP handler for textDocument/semanticTokens/full request."""
    document = LSP_SERVER.workspace.get_document(params.text_document.uri)
    _update_symbols(document)
    data = _compute_semantic_tokens(document.source, document.uri)
    return lsp.SemanticTokens(data=data)


# **********************************************************
# Inlay Hints (Parameter Type Annotations)
# **********************************************************

@LSP_SERVER.feature(lsp.TEXT_DOCUMENT_INLAY_HINT)
def inlay_hints(params: lsp.InlayHintParams) -> Optional[List[lsp.InlayHint]]:
    """LSP handler for textDocument/inlayHint request."""
    document = LSP_SERVER.workspace.get_document(params.text_document.uri)
    symbols = _symbol_cache.get(document.uri, {})
    hints: List[lsp.InlayHint] = []

    # Add type hints for parameter declarations
    for name, param in symbols.get("parameters", {}).items():
        param_type = param.get("type", "")
        default_val = param.get("default_value", "")
        line = param.get("line", 0)
        col = param.get("col", 0)

        # Type hint after parameter name
        if param_type:
            hints.append(lsp.InlayHint(
                position=lsp.Position(line=line, character=col + len(name)),
                label=f": {param_type}",
                kind=lsp.InlayHintKind.Type,
                padding_left=False,
                padding_right=True,
            ))

        # Value hint
        if default_val:
            hints.append(lsp.InlayHint(
                position=lsp.Position(line=line, character=col + len(name)),
                label=f" = {default_val}",
                kind=lsp.InlayHintKind.Parameter,
                padding_left=True,
                padding_right=False,
            ))

    # Add type hints for instance references to parameters
    lines = document.source.split("\n")
    param_re = re.compile(r"#\s*\(\s*\.(\w+)\s*\(\s*([^)]+)\s*\)")
    for line_num, line_text in enumerate(lines):
        if line_num < params.range.start.line or line_num > params.range.end.line:
            continue
        for m in param_re.finditer(line_text):
            param_name = m.group(1)
            param_value = m.group(2).strip()
            if param_name in symbols.get("parameters", {}):
                param_info = symbols["parameters"][param_name]
                param_type = param_info.get("type", "")
                if param_type:
                    hints.append(lsp.InlayHint(
                        position=lsp.Position(
                            line=line_num,
                            character=m.start(2),
                        ),
                        label=f"{param_type}: ",
                        kind=lsp.InlayHintKind.Type,
                        padding_left=False,
                        padding_right=False,
                    ))

    return hints if hints else None


# **********************************************************
# PeakRDL Build Integration
# **********************************************************

def _run_peakrdl_build(
    document: workspace.Document,
    settings: Dict[str, Any],
) -> None:
    """Run PeakRDL build for the given document."""
    file_path = uris.to_fs_path(document.uri)
    cwd = settings.get("workspaceFS", os.path.dirname(file_path))

    # Try to find peakrdl.toml config
    config_path = settings.get("peakrdlBuildConfigPath", "")
    if not config_path:
        config_path = os.path.join(cwd, "peakrdl.toml")

    generators = settings.get("peakrdlGenerators", [])
    output_dir = settings.get("peakrdlOutputDirectory", "generated")

    if not os.path.isabs(output_dir):
        output_dir = os.path.join(cwd, output_dir)

    # If we have a TOML config, parse it
    if os.path.isfile(config_path):
        try:
            _run_peakrdl_from_config(config_path, file_path, cwd)
            log_to_output(f"PeakRDL build completed for {file_path}")
            return
        except Exception as e:
            log_error(f"PeakRDL build failed: {e}")
            return

    # Otherwise use the generators list from settings
    if not generators:
        log_to_output("No PeakRDL generators configured. Set systemrdl.peakrdl.generators or provide a peakrdl.toml.")
        return

    for gen in generators:
        try:
            cmd = ["peakrdl", gen, file_path, "-o", os.path.join(output_dir, gen)]
            log_to_output(f"Running: {' '.join(cmd)}")
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                cwd=cwd,
                timeout=60,
            )
            if result.returncode != 0:
                log_error(f"PeakRDL {gen} failed:\n{result.stderr}")
            else:
                log_to_output(f"PeakRDL {gen} succeeded for {file_path}")
        except FileNotFoundError:
            log_error("PeakRDL CLI not found. Install it with: pip install peakrdl")
            break
        except subprocess.TimeoutExpired:
            log_error(f"PeakRDL {gen} timed out for {file_path}")
        except Exception as e:
            log_error(f"PeakRDL {gen} error: {e}")


def _run_peakrdl_from_config(
    config_path: str, source_file: str, cwd: str
) -> None:
    """Run PeakRDL build using a TOML configuration file."""
    try:
        import tomllib  # Python 3.11+
    except ImportError:
        try:
            import tomli as tomllib  # type: ignore
        except ImportError:
            log_error("TOML parsing not available. Install tomli or use Python 3.11+.")
            return

    with open(config_path, "rb") as f:
        config = tomllib.load(f)

    generate_entries = config.get("generate", [])
    if not generate_entries:
        log_to_output("No [[generate]] entries found in peakrdl.toml")
        return

    for entry in generate_entries:
        input_file = entry.get("input", "")
        outputs = entry.get("outputs", [])
        params = entry.get("params", {})
        output_dir = entry.get("output_dir", "generated")

        # Only build if the saved file matches the input
        input_path = os.path.join(cwd, input_file) if not os.path.isabs(input_file) else input_file
        if os.path.normpath(input_path) != os.path.normpath(source_file):
            continue

        if not os.path.isabs(output_dir):
            output_dir = os.path.join(cwd, output_dir)

        for output_type in outputs:
            cmd = ["peakrdl", output_type, input_path, "-o", os.path.join(output_dir, output_type)]
            # Add parameter overrides
            for pname, pval in params.items():
                cmd.extend(["-P", f"{pname}={pval}"])

            try:
                log_to_output(f"Running: {' '.join(cmd)}")
                result = subprocess.run(
                    cmd,
                    capture_output=True,
                    text=True,
                    cwd=cwd,
                    timeout=60,
                )
                if result.returncode != 0:
                    log_error(f"PeakRDL {output_type} failed:\n{result.stderr}")
                else:
                    log_to_output(f"PeakRDL {output_type} succeeded for {input_file}")
            except FileNotFoundError:
                log_error("PeakRDL CLI not found. Install it with: pip install peakrdl")
                return
            except subprocess.TimeoutExpired:
                log_error(f"PeakRDL {output_type} timed out for {input_file}")
            except Exception as e:
                log_error(f"PeakRDL {output_type} error: {e}")


# **********************************************************
# Custom command: Run PeakRDL Build
# **********************************************************

@LSP_SERVER.command("systemrdl.peakrdl.build")
def peakrdl_build_command(ls: server.LanguageServer, args: Any) -> None:
    """Execute PeakRDL build command."""
    # Get the active document from args or first workspace
    if args and len(args) > 0:
        uri = args[0]
        document = ls.workspace.get_document(uri)
    else:
        # Try to find an RDL file in workspace
        log_to_output("PeakRDL build: No document specified.")
        return

    settings = _get_settings_by_document(document)
    _run_peakrdl_build(document, settings)


# **********************************************************
# Formatting support (placeholder for future)
# **********************************************************

@LSP_SERVER.feature(lsp.TEXT_DOCUMENT_FORMATTING)
def formatting(params: lsp.DocumentFormattingParams) -> Optional[List[lsp.TextEdit]]:
    """LSP handler for textDocument/formatting request."""
    # SystemRDL doesn't have a standard formatter yet.
    # Return None to indicate no formatting changes.
    return None


# **********************************************************
# Utility functions
# **********************************************************

def _get_word_at_position(source: str, position: lsp.Position) -> Optional[str]:
    """Get the word at the given position in the source."""
    lines = source.split("\n")
    if position.line >= len(lines):
        return None

    line = lines[position.line]
    col = position.character

    if col >= len(line):
        return None

    # Find word boundaries
    start = col
    while start > 0 and (line[start - 1].isalnum() or line[start - 1] == "_"):
        start -= 1

    end = col
    while end < len(line) and (line[end].isalnum() or line[end] == "_"):
        end += 1

    word = line[start:end]
    if not word or not (word[0].isalpha() or word[0] == "_"):
        return None

    return word


# **********************************************************
# Required Language Server Initialization and Exit handlers.
# **********************************************************

@LSP_SERVER.feature(lsp.INITIALIZE)
def initialize(params: lsp.InitializeParams) -> None:
    """LSP handler for initialize request."""
    log_to_output(f"CWD Server: {os.getcwd()}")

    paths = "\r\n   ".join(sys.path)
    log_to_output(f"sys.path used to run Server:\r\n   {paths}")

    GLOBAL_SETTINGS.update(**params.initialization_options.get("globalSettings", {}))

    settings = params.initialization_options["settings"]
    _update_workspace_settings(settings)
    log_to_output(
        f"Settings used to run Server:\r\n{json.dumps(settings, indent=4, ensure_ascii=False)}\r\n"
    )
    log_to_output(
        f"Global settings:\r\n{json.dumps(GLOBAL_SETTINGS, indent=4, ensure_ascii=False)}\r\n"
    )

    # Check if systemrdl-compiler is available
    try:
        import systemrdl  # type: ignore
        log_to_output(f"systemrdl-compiler version: {systemrdl.__version__}")
    except ImportError:
        log_warning(
            "systemrdl-compiler not found. Some features (diagnostics, go-to-definition) "
            "will have limited functionality. Install with: pip install systemrdl-compiler"
        )


@LSP_SERVER.feature(lsp.EXIT)
def on_exit(_params: Optional[Any] = None) -> None:
    """Handle clean up on exit."""
    jsonrpc.shutdown_json_rpc()


@LSP_SERVER.feature(lsp.SHUTDOWN)
def on_shutdown(_params: Optional[Any] = None) -> None:
    """Handle clean up on shutdown."""
    jsonrpc.shutdown_json_rpc()


def _get_global_defaults():
    return {
        "path": GLOBAL_SETTINGS.get("path", []),
        "interpreter": GLOBAL_SETTINGS.get("interpreter", [sys.executable]),
        "args": GLOBAL_SETTINGS.get("args", []),
        "importStrategy": GLOBAL_SETTINGS.get("importStrategy", "useBundled"),
        "showNotifications": GLOBAL_SETTINGS.get("showNotifications", "off"),
    }


def _update_workspace_settings(settings):
    if not settings:
        key = os.getcwd()
        WORKSPACE_SETTINGS[key] = {
            "cwd": key,
            "workspaceFS": key,
            "workspace": uris.from_fs_path(key),
            **_get_global_defaults(),
        }
        return

    for setting in settings:
        key = uris.to_fs_path(setting["workspace"])
        WORKSPACE_SETTINGS[key] = {
            "cwd": key,
            **setting,
            "workspaceFS": key,
        }


def _get_settings_by_path(file_path: pathlib.Path):
    workspaces = {s["workspaceFS"] for s in WORKSPACE_SETTINGS.values()}

    while file_path != file_path.parent:
        str_file_path = str(file_path)
        if str_file_path in workspaces:
            return WORKSPACE_SETTINGS[str_file_path]
        file_path = file_path.parent

    setting_values = list(WORKSPACE_SETTINGS.values())
    return setting_values[0]


def _get_document_key(document: workspace.Document):
    if WORKSPACE_SETTINGS:
        document_workspace = pathlib.Path(document.path)
        workspaces = {s["workspaceFS"] for s in WORKSPACE_SETTINGS.values()}

        while document_workspace != document_workspace.parent:
            if str(document_workspace) in workspaces:
                return str(document_workspace)
            document_workspace = document_workspace.parent

    return None


def _get_settings_by_document(document: workspace.Document | None):
    if document is None or document.path is None:
        return list(WORKSPACE_SETTINGS.values())[0]

    key = _get_document_key(document)
    if key is None:
        key = os.fspath(pathlib.Path(document.path).parent)
        return {
            "cwd": key,
            "workspaceFS": key,
            "workspace": uris.from_fs_path(key),
            **_get_global_defaults(),
        }

    return WORKSPACE_SETTINGS[str(key)]


# *****************************************************
# Logging and notification.
# *****************************************************
def log_to_output(
    message: str, msg_type: lsp.MessageType = lsp.MessageType.Log
) -> None:
    LSP_SERVER.show_message_log(message, msg_type)


def log_error(message: str) -> None:
    LSP_SERVER.show_message_log(message, lsp.MessageType.Error)
    if os.getenv("LS_SHOW_NOTIFICATION", "off") in ["onError", "onWarning", "always"]:
        LSP_SERVER.show_message(message, lsp.MessageType.Error)


def log_warning(message: str) -> None:
    LSP_SERVER.show_message_log(message, lsp.MessageType.Warning)
    if os.getenv("LS_SHOW_NOTIFICATION", "off") in ["onWarning", "always"]:
        LSP_SERVER.show_message(message, lsp.MessageType.Warning)


def log_always(message: str) -> None:
    LSP_SERVER.show_message_log(message, lsp.MessageType.Info)
    if os.getenv("LS_SHOW_NOTIFICATION", "off") in ["always"]:
        LSP_SERVER.show_message(message, lsp.MessageType.Info)


# *****************************************************
# Start the server.
# *****************************************************
if __name__ == "__main__":
    LSP_SERVER.start_io()
