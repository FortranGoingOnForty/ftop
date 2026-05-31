# Sprint 05: fgof-toml Library

## Goal
Build a standalone, reusable TOML v1.0 parser as a new fgof ecosystem library. Supports reading TOML config files and theme files. Pure Fortran, no C dependencies. This blocks configurable layout and theming.

## Status: NOT STARTED

## Targets

### TOML v1.0 Specification Coverage
- [ ] Key/value pairs: `key = "value"`
- [ ] Bare keys, quoted keys, dotted keys
- [ ] String types: basic (`"..."`), literal (`'...'`), multiline basic, multiline literal
- [ ] Integer types: decimal, hex, octal, binary, with underscores
- [ ] Float types: standard, exponent, special (inf, nan)
- [ ] Boolean: `true`, `false`
- [ ] Date/time: offset datetime, local datetime, local date, local time
- [ ] Arrays: `[1, 2, 3]`, mixed types allowed per spec
- [ ] Tables: `[table]`, `[parent.child]`
- [ ] Inline tables: `{key = "value", ...}`
- [ ] Array of tables: `[[array]]`
- [ ] Comments: `# comment`

### API Design
- [ ] `type :: toml_document` — root parsed document
- [ ] `type :: toml_value` — variant type (string/int/float/bool/datetime/array/table)
- [ ] `type :: toml_table` — key-value collection
- [ ] `type :: toml_array` — ordered value collection
- [ ] `parse_file(path, document, error)` — parse from file
- [ ] `parse_string(content, document, error)` — parse from string
- [ ] Query API with dotted paths:
  - [ ] `document%get_string("section.key", default)` -> string
  - [ ] `document%get_integer("section.key", default)` -> integer
  - [ ] `document%get_float("section.key", default)` -> real
  - [ ] `document%get_boolean("section.key", default)` -> logical
  - [ ] `document%get_table("section")` -> toml_table
  - [ ] `document%get_array("section.key")` -> toml_array
  - [ ] `document%has_key("section.key")` -> logical
- [ ] Error reporting: line number, column, descriptive message

### Repository Setup
- [ ] New repository: `fgof-toml` under FortranGoingOnForty org
- [ ] fpm manifest (`fpm.toml`)
- [ ] CMakeLists.txt (for ftop integration)
- [ ] Test suite with TOML test corpus

### Implementation
- [ ] Lexer: tokenize TOML source into token stream
  - [ ] Token types: key, string, integer, float, boolean, datetime, equals, dot, comma, lbracket, rbracket, lbrace, rbrace, newline, comment, EOF
  - [ ] Handle escape sequences in strings
  - [ ] Handle multiline strings
- [ ] Parser: recursive descent parser producing AST
  - [ ] Top-level key/value pairs
  - [ ] Table headers
  - [ ] Nested tables
  - [ ] Array of tables
  - [ ] Inline tables and arrays
- [ ] Validation: type checking, duplicate key detection, proper nesting

## Definition of Done
- Parses all valid TOML v1.0 documents correctly
- Rejects all invalid TOML documents with descriptive errors
- Passes the TOML test suite (toml-test or equivalent subset)
- Query API retrieves values by dotted path with correct types
- Compiles with gfortran, no C dependencies
- Published as fgof-toml with fpm manifest

## Dependencies
- Sprint 00 (fgof-string may be useful for string handling)

## Testing
- **Corpus**: run against TOML test suite (valid + invalid cases)
- **Unit**: lexer token output for known inputs
- **Unit**: parser AST structure for known inputs
- **Unit**: query API returns correct values/types/defaults
- **Edge cases**: empty document, deeply nested tables, large arrays, long strings, all escape sequences
- **Error messages**: verify line/column numbers in error output

## Pitfalls
- **TOML spec complexity**: TOML looks simple but has many edge cases (multiline strings with line-ending backslash, dotted key semantics, array-of-tables merging). Reference the spec meticulously.
- **Fortran string handling**: Fortran strings are fixed-length. Use allocatable character variables or fgof-string throughout. Never assume a maximum key/value length.
- **Date/time parsing**: TOML datetime types are complex. For ftop's needs, storing them as strings may suffice initially. Full datetime type support can come later.
- **Memory management**: the parsed document tree needs proper deallocation. Fortran's `final` procedures or manual cleanup subroutines.
- **Unicode in keys/values**: TOML allows Unicode in quoted keys and string values. Fortran operates on bytes; ensure UTF-8 passthrough works correctly.

## Deferred
- TOML serialization (writing TOML files) — not needed for ftop v1.0
- Full datetime type support — store as strings initially

## Notes
- Consider using fgof-string for string manipulation in the lexer/parser.
- The toml-f library exists and is well-tested. We're building our own per the project vision, but study toml-f's test cases and edge case handling for reference.
- This sprint can run in parallel with Sprints 03 and 04 since it's an independent library.
