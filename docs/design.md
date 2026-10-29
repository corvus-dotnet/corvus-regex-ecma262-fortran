# corvus-regex-ecma262 for Fortran: design

An outline of the module as it is: what it offers, how it is built and why, and how it is tested and measured. The
[getting-started page](getting-started.md) shows how to use it.

## 1. What it is

A Fortran module, `corvus_regex_ecma262`, of regular expressions with ECMA-262's grammar and answers (ECMAScript
2025), over UTF-8 (`character`) and UTF-16 (`integer(int16)` arrays). It is a binding, through `ISO_C_BINDING`, to the
corvus-regex-ecma262 C library, built from the Rust crate, whose engine it uses as it is.

**Why ECMA-262.** JSON Schema, OpenAPI and many wire formats specify their patterns as ECMAScript regular
expressions. Fortran has no regular expressions of its own, and POSIX `regex.h`, reached through C interoperability,
has its own grammar and its own meaning for `\d`, `\w`, `\s` and `.`, so a translation to it can never be exact. An
engine that reads ECMA-262 directly, held to V8's answers over a large data set (section 6), needs no translation.

## 2. Public surface

**Names.** A `regex_t` with type-bound procedures `compile`, `test`, `find`, `exec` (into a `captures_t`), `find_all`,
`exec_all`, `count`, `replace`, `replace_all` and `split`, each a generic over UTF-8, UTF-16 and a checked
`utf8_text_t`, taking the position as `start` where JavaScript keeps `lastIndex`. A `match_t` is a `start` and an
`end`, from 1 and inclusive, so that `text(m%start:m%end)` is the match, as Fortran indexes a string.

**Flags.** ECMA-262's, as a string, and nothing else.

**Edition and Unicode version.** By default a pattern is read with today's grammar (`CRE_EDITION_CURRENT`) and V8's
version of Unicode (`CRE_UNICODE_DEFAULT`, 17 today). `compile`'s `edition` and `unicode_version` override either,
independently: `CRE_EDITION_ES2020` is the grammar JSON Schema 2020-12 names, and the versions are `CRE_UNICODE_13`,
`_17`, `_18` and `_LATEST`. Both are resolved when the pattern is compiled.

**Errors.** A procedure that can fail takes the optional arguments `stat` and `errmsg`, as the intrinsic statements
do: `stat` is `CRE_OK` or the C library's status (`CRE_INVALID_PATTERN`, `CRE_INVALID_FLAGS`, `CRE_PATTERN_TOO_LARGE`,
`CRE_INVALID_UTF8`, `CRE_MATCH_LIMIT`), and `errmsg` its message. When `stat` is absent, a failure stops the program
with the message (`error stop`).

**ECMA-262 algorithms.** `replace` substitutes as `GetSubstitution` does, replacing every match only with `g`;
`replace_all` always does. `split` follows `RegExp.prototype[Symbol.split]`, each `piece_t` with `defined = .false.`
for a group that did not take part.

**Input.** `character` text is UTF-8, checked before it is read and refused when it is not, never read as U+FFFD;
`utf8_text` checks a text once. An `integer(int16)` array is UTF-16 code units, lone surrogates and all. Positions are
in the text's units. Nothing is transcoded.

**Limits.** The C library's: a match timeout in seconds per pattern or for every pattern (`set_default_match_timeout`),
with a `timeout` of its own on `test`, `find` and `exec`; the size limits; and no stack overflow.

## 3. Engine

The engine is the Rust crate's, which
[its design](https://github.com/corvus-dotnet/corvus-regex-ecma262-rs/blob/main/docs/design.md) describes, reached
through the C library ([its design](https://github.com/corvus-dotnet/corvus-regex-ecma262-rs/blob/main/docs/design-c.md)).
The module's work is between Fortran and the C library:

- **Handles.** A `regex_t` and a `captures_t` hold the C library's handles and free them when they are finalized.
  Assigning one `regex_t` to another compiles the same pattern again, so no two own one handle; a `captures_t` may not
  be copied, and remembers the pattern it was made for. A compiled `regex_t` is immutable, so any number of threads
  use one at once.
- **Text.** A `character` argument is passed to the C library by reference with its length, with no copy; a string the
  C library returns is copied into an allocatable `character`.
- **Positions.** The C library's 0-based, end-exclusive offsets are turned into Fortran's 1-based, inclusive ones.

## 4. The C library and the build

The module is built against one release of the C library (0.2.3 or later): with fpm, its `lib` directory on the
linker's path; with CMake, its CMake package on `CMAKE_PREFIX_PATH`, and the target `corvus_regex_ecma262::fortran`.
CI and the Dockerfile name the version and each archive's SHA-256. The API reference (`docs/api.md`) is generated from
the module by `tools/docs/gen-api.ps1`, which CI checks is current.

## 5. Targets and layout

gfortran 11 and later and Intel ifx, on Linux (x64 and arm64), macOS and Windows, as an fpm package and a CMake package.

- `src/corvus_regex_ecma262.f90`: the module; `test/`: the API test; `example/`: the README's example.
- `tools/dataset`: the test-data runner; `tools/jsonlite`: the JSON reader the tools share; `tools/docs`: the API
  reference's generator; `tools/bench`: the benchmark harness.

## 6. Testing

**The ECMA-262 test data.** [`ecma262-regex-test-data`](https://github.com/corvus-dotnet/ecma262-regex-test-data)
holds V8's answers: validity under each grammar, `test` answers, the positions of `exec`, the global search, `y`,
`replace` and `split`, and every `exec` call of test262. `tools/dataset` runs all of it through the module, over UTF-8
and UTF-16, with none wrong, refused or at a limit, and as the data's own scripts write it for Unicode 13 and 18 and
for ECMAScript 2020.

**Other tests.** The API test and the README's example, with fpm (gfortran with `-Wall -Wextra -std=f2018 -Werror
-fcheck=all`, and ifx) and with CMake, where the getting-started page's programs run too; and a consumer of the
installed CMake package.

## 7. Benchmarks

The harness image, `ghcr.io/corvus-dotnet/corvus-regex-ecma262-fortran`, runs the Industry workloads of
`dotnet/performance` over this module (UTF-8 and UTF-16), POSIX `regex.h` and V8. The published figures are on
[the benchmark page](https://corvus-oss.org/corvus-regex-ecma262-benchmarks/), which runs every engine's image each
night; this repository publishes none of its own.

Three rows are not like for like with an engine that is not ECMA-262's, as the price of its rules under u: `.*` (`.`
excludes CR, so the CRLF text has more matches), `Sher[a-z]+|Hol[a-z]+` under i (the search must also find the long s
and the Kelvin sign), and `[a-q][^u-z]{13}x` (a supplementary character is two units, so the distance to the `x` is
not fixed).

## 8. Differences a Fortran user will meet

- Positions are from 1 and inclusive, as Fortran indexes a string, where `regex.h` gives offsets from 0.
- A failure without `stat` stops the program, as an intrinsic statement's does.
- The grammar is ECMA-262's, not POSIX's, and a match is the first `exec` finds, not the leftmost longest.

The [getting-started page](getting-started.md) has the full list.
