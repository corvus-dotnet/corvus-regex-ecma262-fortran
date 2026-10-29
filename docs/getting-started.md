# Getting started with Fortran

## Introduction

`corvus-regex-ecma262` for Fortran is a module, `corvus_regex_ecma262`, whose patterns answer as ECMAScript does.
Fortran has no regular expressions of its own, and the POSIX `regex.h` a program reaches through C interoperability
has its own grammar and its own meaning for `\d`, `\w`, `\s` and `.`; this module has ECMA-262's, so a pattern written
for JavaScript, or taken from a JSON Schema, matches the same texts here as it does there. It is a binding, through
`ISO_C_BINDING`, to the corvus-regex-ecma262 C library, built from the Rust crate and held to V8's answers on the
ECMA-262 test data.

This guide covers:

- **Setup**: the C library, then the module with fpm or CMake.
- **Patterns and flags**: the flags are ECMA-262's letters, and what each one does.
- **The edition and the version of Unicode**: the grammar of ECMAScript 2020, and the Unicode tables a pattern reads.
- **Matching**: `test`, `find`, `exec`, `find_all`, `count`, `replace` and `split`, and a `captures_t` reused from
  match to match.
- **Timeouts**: the limit of time and of memory a match may take.
- **UTF-8 and UTF-16**: `character` text as UTF-8, checked once with `utf8_text`, and `integer(int16)` arrays as UTF-16.
- **Handles, stat and errmsg**: what a handle frees, and how a failure is reported.
- **Differences from regex.h**: what to expect when the two meet.

## Setup

The module needs the C library: unpack the archive for the platform (Linux with glibc or musl, macOS and Windows, x64
and arm64) from the [C library's release](https://github.com/corvus-dotnet/corvus-regex-ecma262-rs/releases), and put
its `lib` directory on the linker's path (and, for the shared library, the loader's). Then, with fpm, add the package to `fpm.toml`:

```
[dependencies]
corvus-regex-ecma262 = { git = "https://github.com/corvus-dotnet/corvus-regex-ecma262-fortran", tag = "v0.2.3" }
```

or, with CMake, give the C library's directory in `CMAKE_PREFIX_PATH`, add this repository (or install it and
`find_package(corvus_regex_ecma262_fortran)`) and link `corvus_regex_ecma262::fortran`. It is built and tested with
gfortran 11 and later and with Intel ifx.

```fortran
use corvus_regex_ecma262
```

## Patterns and flags

A pattern is written as it would be in JavaScript, and the flags are the letters `RegExp` takes, as a string:

| Flag | Meaning |
|---|---|
| `d` | Accepted for completeness; indices are always kept. |
| `g` | `replace` replaces every match; without it, the first, as `String.prototype.replace` does. |
| `i` | Canonicalize of the mode in force: simple case folding with `u` or `v`, `toUpperCase` without. Never the locale. |
| `m` | `^` and `$` at line terminators. |
| `s` | The dot matches line terminators. |
| `u` | The `u` flag grammar; matched by code point. |
| `v` | Set notation and properties of strings; matched by code point. `u` and `v` together is an error, as in ECMA-262. |
| `y` | A match only at the start position. |

```fortran
program patterns
  use corvus_regex_ecma262
  implicit none
  type(regex_t) :: a, b, c

  call a%compile("\p{Script=Greek}+", "u")
  call b%compile("[[a-z]--[aeiou]]+", "v")
  call c%compile("^(?<year>\d{4})-(?<month>\d{2})$", "u")

  if (.not. a%test("αβγ")) error stop "Greek"
  if (.not. b%test("rhythm")) error stop "no vowels"
  if (c%group_name(1) /= "year" .or. c%group_name(2) /= "month") error stop "the names"
end program patterns
```

With no flag the pattern is read with the Annex B grammar and matched by UTF-16 code unit, which is what ECMA-262 does
with no flag. JSON Schema's reading, the `u` flag first and Annex B when that refuses, is two compiles with `stat`,
which stops nothing, each with the match timeout, so no default timeout is needed:

```fortran
program schema
  use corvus_regex_ecma262
  implicit none
  type(regex_t) :: unicode, annex_b

  call schema_pattern("^\p{L}+$", unicode)
  if (unicode%flags() /= "u") error stop "read with u"
  ! An identity escape, which only Annex B allows.
  call schema_pattern("\-", annex_b)
  if (annex_b%flags() /= "") error stop "read with Annex B"

contains

  subroutine schema_pattern(pattern, re)
    character(*), intent(in) :: pattern
    type(regex_t), intent(inout) :: re
    integer :: stat
    character(:), allocatable :: errmsg
    call re%compile(pattern, "u", stat=stat, timeout=1.0d0)
    if (stat /= CRE_OK) call re%compile(pattern, "", stat=stat, errmsg=errmsg, timeout=1.0d0)
    if (stat /= CRE_OK) error stop errmsg
  end subroutine schema_pattern
end program schema
```

A pattern that is not valid is `CRE_INVALID_PATTERN`, with the reason in `errmsg`; invalid flags are
`CRE_INVALID_FLAGS`. Every pattern has deterministic limits of size, and `CRE_PATTERN_TOO_LARGE` says when one is
passed. Without `stat`, a pattern that does not compile stops the program with the message.

```fortran
program errors
  use corvus_regex_ecma262
  implicit none
  type(regex_t) :: re
  integer :: stat
  character(:), allocatable :: errmsg

  call re%compile("a{2,1}", "u", stat=stat, errmsg=errmsg)
  if (stat /= CRE_INVALID_PATTERN .or. len(errmsg) == 0) error stop "a pattern that is not valid"
  call re%compile("a", "uv", stat=stat)
  if (stat /= CRE_INVALID_FLAGS) error stop "u with v"
end program errors
```

## The edition and the version of Unicode

JSON Schema 2020-12 names the grammar of ECMAScript 2020. `edition=CRE_EDITION_ES2020` refuses what later editions
added, and nothing else: the `d` and `v` flags, modifier groups such as `(?i:...)`, a group name used by two groups,
and a property name or value that is not in the 2020 tables (such as `\p{Script=Yezidi}`). A pattern it accepts has
the same answers as in today's grammar.

The version of Unicode is chosen apart from the edition: `unicode_version=` is `CRE_UNICODE_13`, `CRE_UNICODE_17`,
`CRE_UNICODE_18`, `CRE_UNICODE_LATEST` (18 today) or `CRE_UNICODE_DEFAULT`, the version of the V8 the answers are held
to (17 today). It decides the members of each `\p{...}`, the characters of a group name and the case folding of the
`i` flag. `unicode_version()` reports the version a pattern was compiled with. Both are read when the pattern is
compiled, so a match costs the same whichever is chosen.

```fortran
program edition
  use corvus_regex_ecma262
  implicit none
  type(regex_t) :: greek

  ! ECMAScript 2020 as its edition was published, with the Unicode of its year.
  call greek%compile("^\p{Script=Greek}+$", "u", timeout=1.0d0, edition=CRE_EDITION_ES2020, &
                     unicode_version=CRE_UNICODE_13)
  if (.not. greek%test("αβγ") .or. greek%unicode_version() /= 13) error stop "Greek"

  ! Vithkuqi is in Unicode 14 and later, so not in Unicode 13.
  if (is_valid("\p{Script=Vithkuqi}", "u", unicode_version=CRE_UNICODE_13)) error stop "no Vithkuqi in Unicode 13"
end program edition
```

## Matching

The procedures are those of a `RegExp`, with the position as `start` where JavaScript keeps `lastIndex`. Positions are
from 1, and a `match_t`'s `start` and `end` are inclusive, so that `text(m%start:m%end)` is the match:

```fortran
program matching
  use corvus_regex_ecma262
  implicit none
  type(regex_t) :: date, separator
  type(captures_t) :: caps
  type(match_t) :: m
  type(match_t), allocatable :: all(:)
  type(piece_t), allocatable :: pieces(:)
  character(*), parameter :: text = "due 2026-10, then 2027-01"

  call date%compile("(?<year>\d{4})-(?<month>\d{2})", "gu")
  if (.not. date%test(text)) error stop "a match"
  if (.not. date%find(text, m)) error stop "the first match"
  if (m%start /= 5 .or. m%end /= 11 .or. text(m%start:m%end) /= "2026-10") error stop "where it is"

  ! The groups, by number and by name.
  if (.not. date%exec(text, caps)) error stop "the groups"
  m = caps%named("year")
  if (text(m%start:m%end) /= "2026") error stop "the year"
  m = caps%group(2)
  if (text(m%start:m%end) /= "10") error stop "the month"

  ! Every match of a global search, as matchAll makes it.
  call date%find_all(text, all)
  if (size(all) /= 2 .or. date%count(text) /= 2) error stop "the global search"

  ! replace replaces every match with the g flag, and the first without it, with every $ form of ECMA-262.
  if (date%replace(text, "$<month>/$1") /= "due 10/2026, then 01/2027") error stop "replace"

  ! split puts what the groups captured between the pieces; a group that did not take part is not defined.
  call separator%compile("\s*(;)\s*")
  call separator%split("a ; b;c", pieces)
  if (size(pieces) /= 5 .or. pieces(2)%value /= ";" .or. pieces(5)%value /= "c") error stop "split"
end program matching
```

`find(text, m, start)` is `exec` with `lastIndex` set to `start - 1`, and `y` anchors it there. `find_all`, `replace`
and `split` allocate what they return. `test`, `find`, `count` and `exec` into a `captures_t` made once and reused from
match to match allocate nothing once the thread has read a match with as many groups.

A `regex_t` is immutable once compiled, so any number of threads may use one at once; a `captures_t` is used by one
thread at a time.

## Timeouts

A match has no limit by default. A pattern compiled with a `timeout` (in seconds) abandons a match that reaches it, or
that reaches the cap on backtracking memory the timeout brings, with `CRE_MATCH_LIMIT`, whatever the pattern:

```fortran
program timeouts
  use corvus_regex_ecma262
  implicit none
  type(regex_t) :: slow
  integer :: stat
  logical :: matched

  call slow%compile("^(?:(a+)\1*)+$", "", timeout=0.1d0)
  if (abs(slow%match_timeout() - 0.1d0) > 1d-9) error stop "the timeout"
  matched = slow%test(repeat("a", 60) // "b", stat=stat)
  if (stat /= CRE_MATCH_LIMIT) error stop "the match did not stop at its timeout"
end program timeouts
```

A pattern compiled without a timeout takes `default_match_timeout()`, which is none (0) unless
`set_default_match_timeout` sets it. A program that matches patterns or texts from outside should set one, for example
with `call set_default_match_timeout(1.0d0)` when it starts. `test`, `find` and `exec` take a `timeout` of their own
for one match.

A pattern without a backreference is matched in time linear in the text, and no pattern can overflow the stack, so the
timeout is what bounds the others.

## UTF-8 and UTF-16

`character` text is UTF-8, matched as its bytes with no transcoding, and positions are bytes. It is checked before it
is read: text that is not UTF-8 is `CRE_INVALID_UTF8`, with `last_error_offset()` where it stops being UTF-8, and is
never matched or read as U+FFFD. `utf8_text` checks a text once, for a text matched many times. Every procedure that
matches takes an `integer(int16)` array of UTF-16 code units too, lone surrogates and all, with positions in code
units.

```fortran
program encodings
  use corvus_regex_ecma262
  use, intrinsic :: iso_fortran_env, only: int16
  implicit none
  type(regex_t) :: dot_b
  type(match_t) :: m
  integer :: stat
  logical :: matched
  ! "a😀b" as UTF-16: the emoji is a surrogate pair.
  integer(int16), parameter :: units(4) = [int(z'61', int16), int(z'D83D', int16), int(z'DE00', int16), &
                                            int(z'62', int16)]

  call dot_b%compile(".b", "u")
  if (.not. dot_b%find("a😀b", m) .or. m%start /= 2 .or. m%end /= 6) error stop "bytes of UTF-8"
  if (.not. dot_b%find(units, m) .or. m%start /= 2 .or. m%end /= 4) error stop "UTF-16 code units"
  if (.not. dot_b%test(utf8_text("a😀b"))) error stop "a text checked once"

  matched = dot_b%test(char(255), stat=stat)
  if (stat /= CRE_INVALID_UTF8 .or. last_error_offset() /= 0) error stop "text that is not UTF-8"
end program encodings
```

Under the `u` and `v` flags a character is a code point in both encodings, so a class, a dot or a quantifier takes a
four-byte sequence as one character. With no flag ECMA-262 matches by UTF-16 code unit; over UTF-8 the module keeps
that meaning, so a match can start or end between the halves of a character beyond the Basic Multilingual Plane, at
the character's third byte.

## Handles, stat and errmsg

- **Handles.** A `regex_t` and a `captures_t` free what they hold when they are finalized. Assigning one `regex_t` to
  another compiles the same pattern again; a `captures_t` may not be copied.
- **stat and errmsg.** A procedure that can fail takes the optional arguments `stat` and `errmsg`, as the intrinsic
  statements do: `stat` is `CRE_OK` or the status of the failure, and `errmsg` its message. When `stat` is absent, a
  failure stops the program with the message (`error stop`).

## Differences from regex.h

The procedures follow `RegExp`; the meaning follows ECMA-262. The differences a user coming from POSIX `regex.h`
through C interoperability will meet:

- **The grammar.** ECMA-262's, not POSIX's basic or extended grammar: lookahead, lookbehind, backreferences, named
  groups, modifier groups and Unicode property escapes, with no `[[:alpha:]]` classes. The flags are a separate
  string.
- **The match.** The first match `exec` finds, leftmost and by priority of alternatives, not POSIX's leftmost longest.
- **The character classes.** `\d` is `[0-9]`, `\w` is the 63 ASCII word characters, `\s` is ECMAScript's white space
  and line terminators, and `.` excludes the four line terminators (LF, CR, U+2028, U+2029), whatever the locale.
- **Replacement and split.** `replace` takes ECMA-262's template (`$1`, `$<name>`, `$&`, `` $` ``, `$'`), and `split`
  is JavaScript's; `regex.h` has neither.
- **Positions.** From 1 and inclusive, as Fortran indexes a string, where `regex.h` gives offsets from 0.
- **Timeouts.** `regex.h` has no limit on a match's time. Here a pattern without a backreference is matched in time
  linear in the text, and a timeout bounds the others.
