# corvus-regex-ecma262 for Fortran

Regular expressions with the grammar and the answers of ECMA-262, the `RegExp` of JavaScript, for Fortran: a binding,
through `ISO_C_BINDING`, to the [corvus-regex-ecma262](https://github.com/corvus-dotnet/corvus-regex-ecma262-rs) C
library, as an fpm package and a CMake package, for gfortran 11 and later and Intel ifx.

- Every flag: `d`, `g`, `i`, `m`, `s`, `u`, `v` and `y`, with the grammar of each. Without `u` or `v` a pattern is
  read by the Annex B grammar and matched by UTF-16 code unit, as ECMA-262 matches it; with them, by code point. The
  `v` flag has nested classes, the difference and the intersection of classes, strings in a class and the properties
  of strings.
- ECMAScript 2025: lookbehind, named groups (and a name shared between alternatives), modifier groups such as
  `(?i:...)`, and every Unicode property ECMA-262 lists, from Unicode 17 tables the library carries.
- UTF-8 text in a `character` variable (positions in bytes) and UTF-16 text in an `integer(int16)` array (positions
  in code units, lone surrogates and all), each matched as it is held.
- A pattern without a backreference is matched in time proportional to its size times the text, and the single
  searches take a timeout, so neither a pattern nor a text from outside can make a program hang.

**Compliance.** Over the [ECMA-262 test data](https://github.com/corvus-dotnet/ecma262-regex-test-data) (what V8 says
about 117,889 patterns and 5,883,170 texts, `exec`, the global search, `replace` and `split` among them, and the
patterns and texts of test262, judged by test262's Unicode 17 answers where V8 has Unicode 16), through this binding
over UTF-16 and UTF-8, it gives 26,913,813 right answers and none wrong, refuses no valid pattern, accepts no invalid
one and reaches no limit. CI runs the whole data.

## Install

The binding needs the C library: download the archive for your platform from the
[C library's release](https://github.com/corvus-dotnet/corvus-regex-ecma262-rs/releases) and unpack it.

With fpm, depend on this repository, and put the C library's `lib` directory on the linker's path (`LIBRARY_PATH`,
or `--link-flag "-L<lib>"`) and, for the shared library, the loader's (`LD_LIBRARY_PATH`):

```toml
[dependencies]
corvus-regex-ecma262 = { git = "https://github.com/corvus-dotnet/corvus-regex-ecma262-fortran", tag = "v0.1.1" }
```

With CMake, give the C library's directory in `CMAKE_PREFIX_PATH`, then add this directory (or install it and
`find_package(corvus_regex_ecma262_fortran)`) and link `corvus_regex_ecma262::fortran`.

## Getting started

```fortran
program basic
  use corvus_regex_ecma262
  implicit none
  type(regex_t) :: date, separator
  type(captures_t) :: caps
  type(match_t) :: m
  type(piece_t), allocatable :: pieces(:)
  character(*), parameter :: text = "due 2026-10"
  integer :: i

  ! The flags are those of new RegExp(pattern, flags).
  call date%compile("(?<year>\d{4})-(?<month>\d{2})", "g")

  print "(l1)", date%test(text)                                     ! T
  if (date%exec(text, caps)) then
    m = caps%named("year")
    print "(a)", text(m%start:m%end)                                ! 2026
  end if
  print "(i0)", date%count("2026-10, 2027-01")                      ! 2

  ! replace replaces every match with the g flag, and the first without it.
  print "(a)", date%replace("2026-10, 2027-01", "$<month>/$1")      ! 10/2026, 01/2027

  ! split splices in what the groups captured.
  call separator%compile("\s*(;)\s*")
  call separator%split("a ; b;c", pieces)
  print "(*(a, :, ' '))", (pieces(i)%value, i = 1, size(pieces))     ! a ; b ; c
end program basic
```

## The API

| | |
|---|---|
| `call re%compile(pattern, flags, stat, errmsg)` | Compiles a pattern as `new RegExp(pattern, flags)` does. |
| `is_valid(pattern, flags)` | Whether a pattern is valid, without compiling it. |
| `re%test(text, start, timeout)` | `RegExp.prototype.test`, with `lastIndex` at `start - 1` (at `start` alone with `y`). |
| `re%find(text, match, start, timeout)` | The first match at or after `start`, as `exec` finds it: whether there is one, and `match`. |
| `re%exec(text, captures, start, timeout)` | The first match with what each group captured, by number and by name, in a `captures_t`. |
| `call re%find_all(text, matches)` | Every match of a global search (`matchAll`): an array of `match_t`. |
| `call re%exec_all(text, groups)` | Every match with its groups: `groups(g, k)` is group `g` (0 the whole match) of the `k`-th. |
| `re%count(text)` | How many matches a global search finds, without making them. |
| `re%replace(text, replacement)` | `String.prototype.replace`: the first match, or every match with `g`. |
| `re%replace_all(text, replacement)` | `String.prototype.replaceAll`: every match. |
| `call re%split(text, pieces, limit)` | `String.prototype.split`: an array of `piece_t` (`piece16_t` over UTF-16). |
| `re%source()`, `re%flags()`, `re%group_count()`, `re%group_name_count()`, `re%group_name(i)` | What the pattern is. |
| `caps%group(g)`, `caps%named(name)`, `caps%count()` | The groups of the last match of `exec`. |
| `library_version()`, `binding_version` | The C library's and this binding's versions. |

Each matching procedure takes a `character(*)` (UTF-8) or an `integer(int16)` array (UTF-16), and timeouts are in
seconds. A `match_t` has `start` and `end`, from 1 and inclusive, so `text(m%start:m%end)` is the match (an empty match
ends one before it starts), and `matched`, false for a group that did not take part. A `piece_t` has `value` and
`defined`, false for a group that did not take part.

**Half characters.** Without `u` or `v` a match can take half of a character beyond the Basic Multilingual Plane.
Over UTF-8 its start or end is then the character's third byte; over UTF-16 it is the lone half, as in JavaScript. A
replacement or a split that leaves half a character writes it as U+FFFD over UTF-8.

**Errors.** A procedure that can fail takes the optional `stat` and `errmsg`, as the intrinsic statements do: `stat`
is `CRE_OK`, `CRE_INVALID_PATTERN`, `CRE_INVALID_FLAGS`, `CRE_PATTERN_TOO_LARGE`, `CRE_INVALID_UTF8`,
`CRE_MATCH_LIMIT` (a match that reached its timeout or its cap on backtracking memory) or `CRE_INVALID_ARGUMENT`.
When `stat` is absent, a failure stops the program with its message.

**Handles.** A `regex_t` and a `captures_t` free what they hold when they are finalized. A `regex_t` is immutable once
compiled and may be used from any number of threads; assigning one to another compiles the same pattern again. A
`captures_t` is used by one thread at a time, and may not be copied.

## Benchmarks

The Industry workloads of dotnet/performance, this binding against the C library's POSIX regex (through
`ISO_C_BINDING`), fortran-regex and V8, are on the central page,
[corvus-oss.org/corvus-regex-ecma262-benchmarks](https://corvus-oss.org/corvus-regex-ecma262-benchmarks/), measured
nightly on one runner with every other Corvus ECMA-262 engine.

## Repository

- `src/corvus_regex_ecma262.f90`: the module; `test/`: the API test; `example/`: the README's example.
- `tools/dataset`: the test-data runner (`fpm run --profile release -- <data> wrong.tsv`, from that directory).
- `tools/bench`: the benchmark harness and the script the image runs; `Dockerfile`: the image the central page runs.
- `tools/jsonlite`: the small JSON reader the tools share, which decodes every escape of the test data exactly.

## Releasing

A release is a GitHub release tagged `v<version>`, the version in `fpm.toml` (and in `binding_version`,
`CMakeLists.txt` and the API test, which CI checks). An fpm project depends on the tag; the release publishes the
harness image with the release's tag.

## Licence

Apache-2.0. See [LICENSE](LICENSE).
