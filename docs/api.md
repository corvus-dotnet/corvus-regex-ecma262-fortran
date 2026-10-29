# API reference

The module `corvus_regex_ecma262`, generated from its public declarations and their comments by
`tools/docs/gen-api.ps1`. [Getting started](getting-started.md) shows it at work.

## Conventions

ECMA-262 (JavaScript) regular expressions for Fortran: a binding to the corvus-regex-ecma262 C library, through
ISO_C_BINDING, with the grammar and the answers of RegExp.

A pattern is compiled once into a regex_t, which matches any number of texts, from any number of threads. Text is
UTF-8 in a character variable, positions in bytes, or UTF-16 in an integer(int16) array, positions in code units
(which may hold lone surrogates). Positions are from 1, and a match_t's start and end are inclusive, so that
text(m%start:m%end) is the match; an empty match ends one before it starts.

Every handle frees what it holds when it is finalized. Assigning one regex_t to another compiles the same pattern
again; a captures_t may not be copied.

A procedure that can fail takes the optional arguments stat and errmsg, as the intrinsic statements do: stat is
CRE_OK or the status of the failure, and errmsg its message. When stat is absent, a failure stops the program
with the message (error stop).

## Constants

### binding_version

```fortran
character(*), parameter, public :: binding_version = "0.2.5"
```

The version of this binding.

### CRE_EDITION_CURRENT, CRE_EDITION_ES2020

```fortran
integer, parameter, public :: CRE_EDITION_CURRENT = 0
integer, parameter, public :: CRE_EDITION_ES2020 = 1
```

The edition of ECMA-262 whose grammar a pattern is read with (the C library's CRE_EDITION_): today's, the default,
or ECMAScript 2020's, which JSON Schema 2020-12 names. The 2020 grammar refuses what later editions added: the d
and v flags, modifier groups, a name given to more than one group, and property names not in its tables.

### CRE_UNICODE_DEFAULT, CRE_UNICODE_13, CRE_UNICODE_17, CRE_UNICODE_18, CRE_UNICODE_LATEST

```fortran
integer, parameter, public :: CRE_UNICODE_DEFAULT = 0
integer, parameter, public :: CRE_UNICODE_13 = 13
integer, parameter, public :: CRE_UNICODE_17 = 17
integer, parameter, public :: CRE_UNICODE_18 = 18
integer, parameter, public :: CRE_UNICODE_LATEST = int(z'7FFFFFFF')
```

The version of Unicode whose tables a pattern is read with, independent of the edition (the C library's
CRE_UNICODE_): the default, which is the version of the V8 the answers are held to, 13, 17, 18, or the latest the
library carries.

### unicode_versions, default_unicode_version, latest_unicode_version

```fortran
integer, parameter, public :: unicode_versions(3) = [13, 17, 18]
integer, parameter, public :: default_unicode_version = 17
integer, parameter, public :: latest_unicode_version = 18
```

The versions of Unicode a pattern can be read with; the one CRE_UNICODE_DEFAULT stands for; and the one
CRE_UNICODE_LATEST stands for.

### CRE_OK, CRE_INVALID_ARGUMENT, CRE_INVALID_UTF8, CRE_INVALID_PATTERN, CRE_INVALID_FLAGS, CRE_PATTERN_TOO_LARGE, CRE_MATCH_LIMIT, CRE_PANIC

```fortran
integer, parameter, public :: CRE_OK = 0
integer, parameter, public :: CRE_INVALID_ARGUMENT = 1
integer, parameter, public :: CRE_INVALID_UTF8 = 2
integer, parameter, public :: CRE_INVALID_PATTERN = 3
integer, parameter, public :: CRE_INVALID_FLAGS = 4
integer, parameter, public :: CRE_PATTERN_TOO_LARGE = 5
integer, parameter, public :: CRE_MATCH_LIMIT = 6
integer, parameter, public :: CRE_PANIC = 7
```

The outcome of a call (cre_status).

## Types

### match_t

```fortran
type :: match_t
  integer(int64) :: start = 0
  integer(int64) :: end = -1
  logical :: matched = .false.
end type match_t
```

Where a match or a group is, from 1, start and end inclusive: text(m%start:m%end). A group that did not take
part has matched = .false. (and start 0, end -1).

### piece_t

```fortran
type :: piece_t
  character(:), allocatable :: value
  logical :: defined = .false.
end type piece_t
```

A piece of a split of UTF-8 text: defined = .false. for a group that did not take part (undefined in the array
split returns).

### piece16_t

```fortran
type :: piece16_t
  integer(int16), allocatable :: value(:)
  logical :: defined = .false.
end type piece16_t
```

A piece of a split of UTF-16 text.

### utf8_text_t

```fortran
type :: utf8_text_t
end type utf8_text_t
```

A UTF-8 text, checked once when utf8_text makes it, which regex_t's methods search without checking it again: a
character argument is checked on every call, a read of the whole text each time. Only utf8_text gives one a text,
so it always holds UTF-8; one never assigned from utf8_text is searched as an empty text.

### regex_t

```fortran
type :: regex_t
  ! (procedures below)
end type regex_t
```

A compiled regular expression. Immutable once compiled: any number of threads may use one at once.

#### regex_t%compile

```fortran
subroutine regex_compile(self, pattern, flags, stat, errmsg, timeout, edition, unicode_version)
  class(regex_t), intent(inout) :: self
  character(*), intent(in) :: pattern
  character(*), intent(in), optional :: flags
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  real(real64), intent(in), optional :: timeout
  integer, intent(in), optional :: edition, unicode_version
end subroutine regex_compile
```

Compiles a pattern with the flags new RegExp(pattern, flags) takes, any of dgimsuvy. On failure stat is
CRE_INVALID_PATTERN, CRE_INVALID_FLAGS, CRE_PATTERN_TOO_LARGE or CRE_INVALID_UTF8, and the regular expression is
not compiled. timeout is the time in seconds each match may take before it is abandoned with CRE_MATCH_LIMIT (0
for none); without it the regular expression takes default_match_timeout(), which is none unless set. A timeout
also caps the backtracking memory of a match; without one a match has no limit of time or of memory, so give a
pattern or a text from outside one. edition is the grammar the pattern is read with (a CRE_EDITION_ value,
CRE_EDITION_CURRENT without it), and unicode_version the version of Unicode whose tables it is read with (a
CRE_UNICODE_ value, CRE_UNICODE_DEFAULT without it); under CRE_EDITION_ES2020 the d and v flags are
CRE_INVALID_FLAGS, and another value of either is CRE_INVALID_ARGUMENT. With stat present a failure is not an
error stop, so compile with stat and a timeout is the form that checks a pattern without a default timeout.

#### regex_t%is_compiled

```fortran
logical function regex_is_compiled(self)
  class(regex_t), intent(in) :: self
end function regex_is_compiled
```

Whether the regular expression is compiled.

#### regex_t%source

```fortran
function regex_source(self) result(text)
  class(regex_t), intent(in) :: self
  character(:), allocatable :: text
end function regex_source
```

The source of the pattern, as it was given ("" when not compiled).

#### regex_t%flags

```fortran
function regex_flags(self) result(text)
  class(regex_t), intent(in) :: self
  character(:), allocatable :: text
end function regex_flags
```

The flags as RegExp.prototype.flags writes them, in the order dgimsuvy.

#### regex_t%match_timeout

```fortran
real(real64) function regex_match_timeout(self)
end function regex_match_timeout
```

The time in seconds each match may take before it is abandoned with CRE_MATCH_LIMIT (0 for none, and when not
compiled).

#### regex_t%edition

```fortran
integer function regex_edition(self)
  class(regex_t), intent(in) :: self
end function regex_edition
```

The edition whose grammar the pattern was read with, a CRE_EDITION_ value (CRE_EDITION_CURRENT when not compiled).

#### regex_t%unicode_version

```fortran
integer function regex_unicode_version(self)
  class(regex_t), intent(in) :: self
end function regex_unicode_version
```

The version of Unicode whose tables the pattern was read with, resolved: 13, 17 or 18 (0 when not compiled).

#### regex_t%group_count

```fortran
integer function regex_group_count(self)
  class(regex_t), intent(in) :: self
end function regex_group_count
```

The number of capturing groups (a match has one more, the whole match, group 0).

#### regex_t%group_name_count

```fortran
integer function regex_group_name_count(self)
  class(regex_t), intent(in) :: self
end function regex_group_name_count
```

The number of distinct names of the groups. Two groups in separate alternatives may share a name.

#### regex_t%group_name

```fortran
function regex_group_name(self, index) result(name)
  class(regex_t), intent(in) :: self
  integer, intent(in) :: index
  character(:), allocatable :: name
end function regex_group_name
```

The name index (from 1), in the order of the first group of each name ("" when there is none).

#### regex_t%test

```fortran
function test_utf8(self, text, start, timeout, stat, errmsg) result(matched)
  class(regex_t), intent(in) :: self
  character(*), intent(in) :: text
  integer(int64), intent(in), optional :: start
  real(real64), intent(in), optional :: timeout
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  logical :: matched
end function test_utf8

function test_utf16(self, text, start, timeout, stat, errmsg) result(matched)
  class(regex_t), intent(in) :: self
  integer(int16), intent(in), contiguous :: text(:)
  integer(int64), intent(in), optional :: start
  real(real64), intent(in), optional :: timeout
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  logical :: matched
end function test_utf16

function test_text(self, text, start, timeout, stat, errmsg) result(matched)
  class(regex_t), intent(in) :: self
  type(utf8_text_t), intent(in) :: text
  integer(int64), intent(in), optional :: start
  real(real64), intent(in), optional :: timeout
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  logical :: matched
end function test_text
```

Whether the pattern matches the text at or after start (at start alone with the y flag), as RegExp.prototype.test
with lastIndex at start - 1. timeout limits the time the match may take, in seconds: a match that reaches it is
CRE_MATCH_LIMIT.

#### regex_t%find

```fortran
function find_utf8(self, text, match, start, timeout, stat, errmsg) result(found)
  class(regex_t), intent(in) :: self
  character(*), intent(in) :: text
  type(match_t), intent(out) :: match
  integer(int64), intent(in), optional :: start
  real(real64), intent(in), optional :: timeout
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  logical :: found
end function find_utf8

function find_utf16(self, text, match, start, timeout, stat, errmsg) result(found)
  class(regex_t), intent(in) :: self
  integer(int16), intent(in), contiguous :: text(:)
  type(match_t), intent(out) :: match
  integer(int64), intent(in), optional :: start
  real(real64), intent(in), optional :: timeout
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  logical :: found
end function find_utf16

function find_text(self, text, match, start, timeout, stat, errmsg) result(found)
  class(regex_t), intent(in) :: self
  type(utf8_text_t), intent(in) :: text
  type(match_t), intent(out) :: match
  integer(int64), intent(in), optional :: start
  real(real64), intent(in), optional :: timeout
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  logical :: found
end function find_text
```

The first match at or after start, as exec finds it with lastIndex at start - 1: whether there is one, and match,
where it is.

#### regex_t%exec

```fortran
function exec_utf8(self, text, captures, start, timeout, stat, errmsg) result(found)
  class(regex_t), intent(in) :: self
  character(*), intent(in) :: text
  type(captures_t), intent(inout) :: captures
  integer(int64), intent(in), optional :: start
  real(real64), intent(in), optional :: timeout
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  logical :: found
end function exec_utf8

function exec_utf16(self, text, captures, start, timeout, stat, errmsg) result(found)
  class(regex_t), intent(in) :: self
  integer(int16), intent(in), contiguous :: text(:)
  type(captures_t), intent(inout) :: captures
  integer(int64), intent(in), optional :: start
  real(real64), intent(in), optional :: timeout
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  logical :: found
end function exec_utf16

function exec_text(self, text, captures, start, timeout, stat, errmsg) result(found)
  class(regex_t), intent(in) :: self
  type(utf8_text_t), intent(in) :: text
  type(captures_t), intent(inout) :: captures
  integer(int64), intent(in), optional :: start
  real(real64), intent(in), optional :: timeout
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  logical :: found
end function exec_text
```

The first match at or after start with what each group captured, by number and by name, in captures: whether
there is one.

#### regex_t%find_all

```fortran
subroutine find_all_utf8(self, text, matches, stat, errmsg)
  class(regex_t), intent(in) :: self
  character(*), intent(in) :: text
  type(match_t), allocatable, intent(out) :: matches(:)
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
end subroutine find_all_utf8

subroutine find_all_utf16(self, text, matches, stat, errmsg)
  class(regex_t), intent(in) :: self
  integer(int16), intent(in), contiguous :: text(:)
  type(match_t), allocatable, intent(out) :: matches(:)
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
end subroutine find_all_utf16

subroutine find_all_text(self, text, matches, stat, errmsg)
  class(regex_t), intent(in) :: self
  type(utf8_text_t), intent(in) :: text
  type(match_t), allocatable, intent(out) :: matches(:)
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
end subroutine find_all_text
```

Every match of a global search, as matchAll makes it: after an empty match the search goes on one character
later (one code unit without u or v), and with the y flag each match must start where the one before it ended.
Whether or not the pattern has the g flag, every match is found.

#### regex_t%exec_all

```fortran
subroutine exec_all_utf8(self, text, groups, stat, errmsg)
  class(regex_t), intent(in) :: self
  character(*), intent(in) :: text
  type(match_t), allocatable, intent(out) :: groups(:, :)
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
end subroutine exec_all_utf8

subroutine exec_all_utf16(self, text, groups, stat, errmsg)
  class(regex_t), intent(in) :: self
  integer(int16), intent(in), contiguous :: text(:)
  type(match_t), allocatable, intent(out) :: groups(:, :)
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
end subroutine exec_all_utf16

subroutine exec_all_text(self, text, groups, stat, errmsg)
  class(regex_t), intent(in) :: self
  type(utf8_text_t), intent(in) :: text
  type(match_t), allocatable, intent(out) :: groups(:, :)
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
end subroutine exec_all_text
```

Every match of a global search with what each group captured: groups(g, k) is group g (0 the whole match) of the
k-th match.

#### regex_t%count

```fortran
function count_utf8(self, text, stat, errmsg) result(n)
  class(regex_t), intent(in) :: self
  character(*), intent(in) :: text
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  integer(int64) :: n
end function count_utf8

function count_utf16(self, text, stat, errmsg) result(n)
  class(regex_t), intent(in) :: self
  integer(int16), intent(in), contiguous :: text(:)
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  integer(int64) :: n
end function count_utf16

function count_text(self, text, stat, errmsg) result(n)
  class(regex_t), intent(in) :: self
  type(utf8_text_t), intent(in) :: text
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  integer(int64) :: n
end function count_text
```

How many matches a global search finds, none of them made.

#### regex_t%replace

```fortran
function replace_utf8(self, text, replacement, stat, errmsg) result(value)
  class(regex_t), intent(in) :: self
  character(*), intent(in) :: text
  character(*), intent(in) :: replacement
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  character(:), allocatable :: value
end function replace_utf8

function replace_utf16(self, text, replacement, stat, errmsg) result(value)
  class(regex_t), intent(in) :: self
  integer(int16), intent(in), contiguous :: text(:), replacement(:)
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  integer(int16), allocatable :: value(:)
end function replace_utf16

function replace_text(self, text, replacement, stat, errmsg) result(value)
  class(regex_t), intent(in) :: self
  type(utf8_text_t), intent(in) :: text
  character(*), intent(in) :: replacement
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  character(:), allocatable :: value
end function replace_text
```

The text with the first match replaced, or every match with the g flag (String.prototype.replace). In the
replacement, $& is the match, $\` and $' the text before and after it, $1 to $99 a group, $\<name> a named group, and
$$ a dollar sign. A half character left by a match without u or v is written as U+FFFD.

#### regex_t%replace_all

```fortran
function replace_all_utf8(self, text, replacement, stat, errmsg) result(value)
  class(regex_t), intent(in) :: self
  character(*), intent(in) :: text
  character(*), intent(in) :: replacement
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  character(:), allocatable :: value
end function replace_all_utf8

function replace_all_utf16(self, text, replacement, stat, errmsg) result(value)
  class(regex_t), intent(in) :: self
  integer(int16), intent(in), contiguous :: text(:), replacement(:)
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  integer(int16), allocatable :: value(:)
end function replace_all_utf16

function replace_all_text(self, text, replacement, stat, errmsg) result(value)
  class(regex_t), intent(in) :: self
  type(utf8_text_t), intent(in) :: text
  character(*), intent(in) :: replacement
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  character(:), allocatable :: value
end function replace_all_text
```

The text with every match replaced (String.prototype.replaceAll), whether or not the pattern has the g flag.

#### regex_t%split

```fortran
subroutine split_utf8(self, text, pieces, limit, stat, errmsg)
  class(regex_t), intent(in) :: self
  character(*), intent(in) :: text
  type(piece_t), allocatable, intent(out) :: pieces(:)
  integer, intent(in), optional :: limit
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
end subroutine split_utf8

subroutine split_utf16(self, text, pieces, limit, stat, errmsg)
  class(regex_t), intent(in) :: self
  integer(int16), intent(in), contiguous :: text(:)
  type(piece16_t), allocatable, intent(out) :: pieces(:)
  integer, intent(in), optional :: limit
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
end subroutine split_utf16

subroutine split_text(self, text, pieces, limit, stat, errmsg)
  class(regex_t), intent(in) :: self
  type(utf8_text_t), intent(in) :: text
  type(piece_t), allocatable, intent(out) :: pieces(:)
  integer, intent(in), optional :: limit
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
end subroutine split_text
```

The text split around the matches (String.prototype.split): what each group captured is spliced in after the
piece before it (defined = .false. for a group that did not take part), an empty match at the start or the end
of the text splits nothing, and there are at most limit pieces when limit is given.

### captures_t

```fortran
type :: captures_t
  ! (procedures below)
end type captures_t
```

What a match captured, by number and by name, from regex_t%exec: made for one regular expression, reused from
match to match. Used by one thread at a time.

#### captures_t%count

```fortran
integer function captures_count(self)
  class(captures_t), intent(in) :: self
end function captures_count
```

The number of groups of the last match, group 0 among them, or 0 when the last search found none.

#### captures_t%group

```fortran
function captures_group(self, group) result(match)
  class(captures_t), intent(in) :: self
  integer, intent(in) :: group
  type(match_t) :: match
end function captures_group
```

The group of the last match (0 is the whole match); matched = .false. when it did not take part, there is no such
group, or the last search found no match.

#### captures_t%named

```fortran
function captures_named(self, name) result(match)
  class(captures_t), intent(in) :: self
  character(*), intent(in) :: name
  type(match_t) :: match
end function captures_named
```

The group of the name in the last match (of the groups that share it, the one that took part).

## Procedures

### is_valid

```fortran
function is_valid(pattern, flags, stat, errmsg, edition, unicode_version) result(valid)
  character(*), intent(in) :: pattern
  character(*), intent(in), optional :: flags
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  integer, intent(in), optional :: edition, unicode_version
  logical :: valid
end function is_valid
```

Whether a pattern is valid with the flags, without compiling it, in the grammar of edition (a CRE_EDITION_ value,
CRE_EDITION_CURRENT without it) and under unicode_version (a CRE_UNICODE_ value, CRE_UNICODE_DEFAULT without it).
A pattern over a limit of size is not valid here. Another value of edition or unicode_version is
CRE_INVALID_ARGUMENT.

### library_version

```fortran
function library_version() result(version)
  character(:), allocatable :: version
end function library_version
```

The C library's version, such as "0.2.5".

### last_error_offset

```fortran
function last_error_offset() result(offset)
  integer(int64) :: offset
end function last_error_offset
```

The byte offset that goes with the most recent failure on this thread: where invalid UTF-8 was found, counted
from 0 (0 otherwise).

### utf8_text

```fortran
function utf8_text(text, stat, errmsg) result(checked)
  character(*), intent(in) :: text
  integer, intent(out), optional :: stat
  character(:), allocatable, intent(out), optional :: errmsg
  type(utf8_text_t) :: checked
end function utf8_text
```

The text as a utf8_text_t, checked to be UTF-8 here, once: regex_t's methods then search it without checking it
again. A text that is not UTF-8 is CRE_INVALID_UTF8 (last_error_offset is where it stops being UTF-8), and the
result holds an empty text.

### set_default_match_timeout

```fortran
subroutine set_default_match_timeout(seconds)
  real(real64), intent(in) :: seconds
end subroutine set_default_match_timeout
```

Sets the match timeout, in seconds, of every regular expression compiled from now on without one of its own (0
for none, the default). It is the process-wide default of .NET's REGEX_DEFAULT_MATCH_TIMEOUT: a program that
matches patterns or texts from outside should set one when it starts. A regular expression already compiled keeps
its own.

### default_match_timeout

```fortran
real(real64) function default_match_timeout()
end function default_match_timeout
```

The match timeout, in seconds, of a regular expression compiled now without one of its own (0 for none).
