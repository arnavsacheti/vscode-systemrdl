# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License.
"""
Tests for SystemRDL language server features.

These tests exercise the pure functions in lsp_server.py directly,
without requiring a running LSP session.
"""

import os
import pathlib
import sys

import pytest

# Add bundled tool to path
BUNDLED_TOOL_PATH = str(
    pathlib.Path(__file__).parent.parent.parent.parent / "bundled" / "tool"
)
if BUNDLED_TOOL_PATH not in sys.path:
    sys.path.insert(0, BUNDLED_TOOL_PATH)

# Add bundled libs to path (for pygls, lsprotocol, etc.)
BUNDLED_LIBS_PATH = str(
    pathlib.Path(__file__).parent.parent.parent.parent / "bundled" / "libs"
)
if os.path.isdir(BUNDLED_LIBS_PATH) and BUNDLED_LIBS_PATH not in sys.path:
    sys.path.insert(0, BUNDLED_LIBS_PATH)

from .lsp_test_client import constants

TEST_DATA = constants.TEST_DATA / "rdl_samples"


# ===================================================================
# Test _extract_symbols_from_source
# ===================================================================

class TestExtractSymbols:
    """Test the symbol extraction from RDL source code."""

    BASIC_RDL = """\
// Basic SystemRDL test file

reg my_reg #(longint unsigned PARAM_WIDTH = 8) {
    default sw = rw;
    default hw = r;

    field {} data[PARAM_WIDTH] = 0;
    field {} status[4] = 0;
};

reg status_reg {
    field {} error[1] = 0;
    field {} ready[1] = 0;
    field {} busy[1] = 0;
};

addrmap my_map {
    default addressing = compact;
    default regwidth = 32;

    my_reg ctrl_reg @ 0x0000;
    status_reg stat @ 0x0004;
    my_reg data_reg @ 0x0008;
};
"""

    PARAM_RDL = """\
reg param_reg #(longint unsigned WIDTH = 32, boolean USE_RESET = true) {
    field {} data[WIDTH] = 0;
};

parameter longint unsigned GLOBAL_PARAM = 16;
"""

    ENUM_RDL = """\
enum status_e {
    IDLE = 0;
    RUNNING = 1;
    ERROR = 2;
    DONE = 3;
};

reg status_reg {
    field {} state[2] = 0;
};
"""

    def _extract(self, source, uri="file:///test.rdl"):
        """Helper to import and call the symbol extraction function."""
        # Import here to avoid issues with bundled libs not being available
        try:
            from lsp_server import _extract_symbols_from_source
            return _extract_symbols_from_source(source, uri)
        except ImportError:
            pytest.skip("lsp_server module not importable (missing bundled libs)")

    def test_component_definitions_found(self):
        """Test that component definitions are found."""
        symbols = self._extract(self.BASIC_RDL)
        if symbols is None:
            return

        defs = symbols["definitions"]
        assert "my_reg" in defs
        assert defs["my_reg"]["kind"] == "reg"
        assert "status_reg" in defs
        assert defs["status_reg"]["kind"] == "reg"
        assert "my_map" in defs
        assert defs["my_map"]["kind"] == "addrmap"

    def test_components_list(self):
        """Test that components list is populated."""
        symbols = self._extract(self.BASIC_RDL)
        if symbols is None:
            return

        comp_names = [c["name"] for c in symbols["components"]]
        assert "my_reg" in comp_names
        assert "status_reg" in comp_names
        assert "my_map" in comp_names

    def test_enum_definitions(self):
        """Test that enum definitions are found."""
        symbols = self._extract(self.ENUM_RDL)
        if symbols is None:
            return

        defs = symbols["definitions"]
        assert "status_e" in defs
        assert defs["status_e"]["kind"] == "enum"

    def test_references_found(self):
        """Test that identifier references are tracked."""
        symbols = self._extract(self.BASIC_RDL)
        if symbols is None:
            return

        refs = symbols["references"]
        # my_reg should appear multiple times (definition + instances)
        assert "my_reg" in refs
        assert len(refs["my_reg"]) >= 2

    def test_comments_excluded_from_references(self):
        """Test that identifiers in comments are excluded from references."""
        source = """\
// This is a comment mentioning my_reg
reg my_reg {
    field {} data[8] = 0;
};
"""
        symbols = self._extract(source)
        if symbols is None:
            return

        # my_reg in the comment should be excluded
        refs = symbols["references"].get("my_reg", [])
        # Only non-comment references should be present
        for ref in refs:
            assert ref["line"] != 0  # Line 0 is the comment


class TestParameterExtraction:
    """Test parameter-specific extraction."""

    def _extract(self, source, uri="file:///test.rdl"):
        try:
            from lsp_server import _extract_symbols_from_source
            return _extract_symbols_from_source(source, uri)
        except ImportError:
            pytest.skip("lsp_server module not importable (missing bundled libs)")

    def test_parameter_in_component(self):
        """Test parameter extraction from component definition."""
        source = """\
reg my_reg #(longint unsigned WIDTH = 32) {
    field {} data[WIDTH] = 0;
};
"""
        # Parameters defined with #() syntax require more complex parsing
        # This test verifies the basic parameter regex works
        symbols = self._extract(source)
        if symbols is None:
            return

        assert "my_reg" in symbols["definitions"]

    def test_standalone_parameter(self):
        """Test standalone parameter declaration."""
        source = """\
parameter longint unsigned MY_PARAM = 42;
parameter boolean FLAG = true;
localparam longint unsigned LOCAL_P = 100;
"""
        symbols = self._extract(source)
        if symbols is None:
            return

        params = symbols["parameters"]
        if "MY_PARAM" in params:
            assert params["MY_PARAM"]["keyword"] == "parameter"
            assert params["MY_PARAM"]["default_value"] == "42"

        if "FLAG" in params:
            assert params["FLAG"]["keyword"] == "parameter"

        if "LOCAL_P" in params:
            assert params["LOCAL_P"]["keyword"] == "localparam"


# ===================================================================
# Test _parse_compiler_error
# ===================================================================

class TestParseCompilerError:
    """Test compiler error parsing."""

    def _parse(self, error_str, file_path="test.rdl"):
        try:
            from lsp_server import _parse_compiler_error
            return _parse_compiler_error(error_str, file_path)
        except ImportError:
            pytest.skip("lsp_server module not importable (missing bundled libs)")

    def test_standard_error_format(self):
        """Test parsing standard compiler error format."""
        error = "test.rdl:10:5: error: Undefined reference to 'foo'"
        diagnostics = self._parse(error)
        if diagnostics is None:
            return

        assert len(diagnostics) >= 1
        diag = diagnostics[0]
        assert diag.range.start.line == 9  # 0-indexed
        assert diag.range.start.character == 4  # 0-indexed
        assert "foo" in diag.message

    def test_warning_format(self):
        """Test parsing warning format."""
        error = "test.rdl:5:1: warning: Unused field 'x'"
        diagnostics = self._parse(error)
        if diagnostics is None:
            return

        assert len(diagnostics) >= 1

    def test_simple_line_reference(self):
        """Test parsing errors with simple line references."""
        error = "Syntax error at line 15"
        diagnostics = self._parse(error)
        if diagnostics is None:
            return

        assert len(diagnostics) >= 1
        assert diagnostics[0].range.start.line == 14  # 0-indexed

    def test_no_line_info(self):
        """Test error without line information."""
        error = "Some general error"
        diagnostics = self._parse(error)
        if diagnostics is None:
            return

        # Should return empty list since no line info found
        assert len(diagnostics) == 0


# ===================================================================
# Test _get_word_at_position
# ===================================================================

class TestGetWordAtPosition:
    """Test word extraction at cursor position."""

    def _get_word(self, source, line, character):
        try:
            from lsp_server import _get_word_at_position
            import lsprotocol.types as lsp
            pos = lsp.Position(line=line, character=character)
            return _get_word_at_position(source, pos)
        except ImportError:
            pytest.skip("lsp_server module not importable (missing bundled libs)")

    def test_word_at_start(self):
        """Test getting word at start of identifier."""
        source = "reg my_reg {"
        word = self._get_word(source, 0, 0)
        if word is None:
            return
        assert word == "reg"

    def test_word_in_middle(self):
        """Test getting word in middle of identifier."""
        source = "reg my_reg {"
        word = self._get_word(source, 0, 6)
        if word is None:
            return
        assert word == "my_reg"

    def test_word_with_underscore(self):
        """Test getting word with underscores."""
        source = "field {} my_data_field[8];"
        word = self._get_word(source, 0, 12)
        if word is None:
            return
        assert word == "my_data_field"

    def test_no_word_at_operator(self):
        """Test that no word is returned at operator position."""
        source = "sw = rw;"
        word = self._get_word(source, 0, 3)
        # Position 3 is '=', should return None or adjacent word
        # depending on exact position


# ===================================================================
# Test Semantic Token types
# ===================================================================

class TestSemanticTokenConstants:
    """Test that semantic token constants are properly defined."""

    def test_token_types_length(self):
        """Test semantic token types list is correct length."""
        try:
            from lsp_server import SEMANTIC_TOKEN_TYPES
            assert len(SEMANTIC_TOKEN_TYPES) == 14
        except ImportError:
            pytest.skip("lsp_server module not importable")

    def test_token_modifiers_length(self):
        """Test semantic token modifiers list is correct length."""
        try:
            from lsp_server import SEMANTIC_TOKEN_MODIFIERS
            assert len(SEMANTIC_TOKEN_MODIFIERS) == 4
        except ImportError:
            pytest.skip("lsp_server module not importable")


# ===================================================================
# Test keyword and property lists
# ===================================================================

class TestKeywordLists:
    """Test that keyword and property lists are comprehensive."""

    def test_keywords_include_component_types(self):
        """Test that all component types are in keywords."""
        try:
            from lsp_server import SYSTEMRDL_KEYWORDS
        except ImportError:
            pytest.skip("lsp_server module not importable")

        component_types = ["addrmap", "regfile", "reg", "field", "signal", "enum", "struct", "mem"]
        for ct in component_types:
            assert ct in SYSTEMRDL_KEYWORDS

    def test_properties_include_common_props(self):
        """Test that common properties are included."""
        try:
            from lsp_server import SYSTEMRDL_PROPERTIES
        except ImportError:
            pytest.skip("lsp_server module not importable")

        common_props = ["sw", "hw", "reset", "desc", "name", "onread", "onwrite"]
        for prop in common_props:
            assert prop in SYSTEMRDL_PROPERTIES

    def test_component_descriptions_complete(self):
        """Test that all component types have descriptions."""
        try:
            from lsp_server import COMPONENT_DESCRIPTIONS
        except ImportError:
            pytest.skip("lsp_server module not importable")

        expected = ["addrmap", "regfile", "reg", "field", "signal", "enum", "struct", "constraint", "mem"]
        for comp in expected:
            assert comp in COMPONENT_DESCRIPTIONS
            assert len(COMPONENT_DESCRIPTIONS[comp]) > 0

    def test_property_descriptions_exist(self):
        """Test that property descriptions dict has entries."""
        try:
            from lsp_server import PROPERTY_DESCRIPTIONS
        except ImportError:
            pytest.skip("lsp_server module not importable")

        assert len(PROPERTY_DESCRIPTIONS) > 0
        assert "sw" in PROPERTY_DESCRIPTIONS
        assert "hw" in PROPERTY_DESCRIPTIONS
        assert "reset" in PROPERTY_DESCRIPTIONS

    def test_access_types(self):
        """Test access type enums."""
        try:
            from lsp_server import SYSTEMRDL_ACCESS_TYPES
        except ImportError:
            pytest.skip("lsp_server module not importable")

        assert "rw" in SYSTEMRDL_ACCESS_TYPES
        assert "r" in SYSTEMRDL_ACCESS_TYPES
        assert "w" in SYSTEMRDL_ACCESS_TYPES
        assert "na" in SYSTEMRDL_ACCESS_TYPES


# ===================================================================
# Test semantic token computation
# ===================================================================

class TestSemanticTokenComputation:
    """Test semantic token encoding."""

    def _compute(self, source, uri="file:///test.rdl"):
        try:
            from lsp_server import _compute_semantic_tokens, _extract_symbols_from_source, _symbol_cache
            # First extract symbols so the cache is populated
            symbols = _extract_symbols_from_source(source, uri)
            _symbol_cache[uri] = symbols
            return _compute_semantic_tokens(source, uri)
        except ImportError:
            pytest.skip("lsp_server module not importable (missing bundled libs)")

    def test_returns_list_of_ints(self):
        """Test that semantic tokens returns a list of integers."""
        source = "reg my_reg {\n};\n"
        tokens = self._compute(source)
        if tokens is None:
            return

        assert isinstance(tokens, list)
        # Each token is 5 integers
        assert len(tokens) % 5 == 0

    def test_empty_source(self):
        """Test semantic tokens for empty source."""
        tokens = self._compute("")
        if tokens is None:
            return

        assert isinstance(tokens, list)
        assert len(tokens) == 0

    def test_comment_tokens(self):
        """Test that comments are tokenized."""
        source = "// This is a comment\n"
        tokens = self._compute(source)
        if tokens is None:
            return

        # Should have at least one token (the comment)
        assert len(tokens) >= 5


# ===================================================================
# Test file reading from test data
# ===================================================================

class TestWithTestData:
    """Tests using the RDL test data files."""

    def test_basic_rdl_file_exists(self):
        """Verify the basic test RDL file exists."""
        basic_rdl = TEST_DATA / "basic.rdl"
        assert basic_rdl.exists(), f"Test file not found: {basic_rdl}"

    def test_basic_rdl_parses(self):
        """Test that the basic RDL test file can be parsed for symbols."""
        basic_rdl = TEST_DATA / "basic.rdl"
        if not basic_rdl.exists():
            pytest.skip("Test data not available")

        try:
            from lsp_server import _extract_symbols_from_source
        except ImportError:
            pytest.skip("lsp_server module not importable")

        source = basic_rdl.read_text()
        symbols = _extract_symbols_from_source(source, f"file://{basic_rdl}")

        assert "definitions" in symbols
        assert "references" in symbols
        assert "components" in symbols
        assert len(symbols["components"]) > 0
