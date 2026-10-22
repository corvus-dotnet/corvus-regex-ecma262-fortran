!> ECMA-262 (JavaScript) regular expressions for Fortran: a binding to the corvus-regex-ecma262 C library, through
!> ISO_C_BINDING, with the grammar and the answers of RegExp.
!>
!> A pattern is compiled once into a regex_t, which matches any number of texts, from any number of threads. Text is
!> UTF-8 in a character variable, positions in bytes, or UTF-16 in an integer(int16) array, positions in code units
!> (which may hold lone surrogates). Positions are from 1, and a match_t's start and end are inclusive, so that
!> text(m%start:m%end) is the match; an empty match ends one before it starts.
!>
!> Every handle frees what it holds when it is finalized. Assigning one regex_t to another compiles the same pattern
!> again; a captures_t may not be copied.
!>
!> A procedure that can fail takes the optional arguments stat and errmsg, as the intrinsic statements do: stat is
!> CRE_OK or the status of the failure, and errmsg its message. When stat is absent, a failure stops the program
!> with the message (error stop).
module corvus_regex_ecma262
  use, intrinsic :: iso_c_binding, only: c_bool, c_char, c_f_pointer, c_int16_t, c_int32_t, c_int64_t, c_null_ptr, &
                                         c_ptr, c_size_t, c_associated
  use, intrinsic :: iso_fortran_env, only: error_unit, int16, int64, real64
  implicit none
  private

  public :: regex_t, captures_t, match_t, piece_t, piece16_t
  public :: is_valid, library_version, last_error_offset

  !> The version of this binding.
  character(*), parameter, public :: binding_version = "0.1.2"

  ! The outcome of a call (cre_status).
  integer, parameter, public :: CRE_OK = 0
  integer, parameter, public :: CRE_INVALID_ARGUMENT = 1
  integer, parameter, public :: CRE_INVALID_UTF8 = 2
  integer, parameter, public :: CRE_INVALID_PATTERN = 3
  integer, parameter, public :: CRE_INVALID_FLAGS = 4
  integer, parameter, public :: CRE_PATTERN_TOO_LARGE = 5
  integer, parameter, public :: CRE_MATCH_LIMIT = 6
  integer, parameter, public :: CRE_PANIC = 7

  ! The C library's CRE_UNSET (SIZE_MAX) and CRE_SPLIT_ALL (0xFFFFFFFF), as Fortran's signed integers hold them.
  integer(c_size_t), parameter :: unset = -1_c_size_t
  integer(c_int32_t), parameter :: split_all = -1_c_int32_t

  !> Where a match or a group is, from 1, start and end inclusive: text(m%start:m%end). A group that did not take
  !> part has matched = .false. (and start 0, end -1).
  type :: match_t
    integer(int64) :: start = 0
    integer(int64) :: end = -1
    logical :: matched = .false.
  end type match_t

  !> A piece of a split of UTF-8 text: defined = .false. for a group that did not take part (undefined in the array
  !> split returns).
  type :: piece_t
    character(:), allocatable :: value
    logical :: defined = .false.
  end type piece_t

  !> A piece of a split of UTF-16 text.
  type :: piece16_t
    integer(int16), allocatable :: value(:)
    logical :: defined = .false.
  end type piece16_t

  ! The C library's strings and positions.
  type, bind(c) :: cre_str
    type(c_ptr) :: ptr = c_null_ptr
    integer(c_size_t) :: len = 0
  end type cre_str

  type, bind(c) :: cre_pieces
    type(c_ptr) :: items = c_null_ptr
    integer(c_size_t) :: len = 0
    type(c_ptr) :: internal = c_null_ptr
  end type cre_pieces

  type, bind(c) :: cre_match
    integer(c_size_t) :: start = unset
    integer(c_size_t) :: end = unset
  end type cre_match

  !> A compiled regular expression. Immutable once compiled: any number of threads may use one at once.
  type :: regex_t
    private
    type(c_ptr) :: handle = c_null_ptr
  contains
    procedure :: compile => regex_compile
    procedure :: is_compiled => regex_is_compiled
    procedure :: source => regex_source
    procedure :: flags => regex_flags
    procedure :: group_count => regex_group_count
    procedure :: group_name_count => regex_group_name_count
    procedure :: group_name => regex_group_name
    procedure, private :: test_utf8, test_utf16
    generic :: test => test_utf8, test_utf16
    procedure, private :: find_utf8, find_utf16
    generic :: find => find_utf8, find_utf16
    procedure, private :: exec_utf8, exec_utf16
    generic :: exec => exec_utf8, exec_utf16
    procedure, private :: find_all_utf8, find_all_utf16
    generic :: find_all => find_all_utf8, find_all_utf16
    procedure, private :: exec_all_utf8, exec_all_utf16
    generic :: exec_all => exec_all_utf8, exec_all_utf16
    procedure, private :: count_utf8, count_utf16
    generic :: count => count_utf8, count_utf16
    procedure, private :: replace_utf8, replace_utf16
    generic :: replace => replace_utf8, replace_utf16
    procedure, private :: replace_all_utf8, replace_all_utf16
    generic :: replace_all => replace_all_utf8, replace_all_utf16
    procedure, private :: split_utf8, split_utf16
    generic :: split => split_utf8, split_utf16
    generic :: assignment(=) => regex_assign
    procedure, private :: regex_assign
    final :: regex_final
  end type regex_t

  !> What a match captured, by number and by name, from regex_t%exec: made for one regular expression, reused from
  !> match to match. Used by one thread at a time.
  type :: captures_t
    private
    type(c_ptr) :: handle = c_null_ptr
    ! The regular expression it was made for: its handle, and its source and flags, since a handle freed and another
    ! compiled may have the same address.
    type(c_ptr) :: regex = c_null_ptr
    character(:), allocatable :: source, flags
  contains
    procedure :: count => captures_count
    procedure :: group => captures_group
    procedure :: named => captures_named
    generic :: assignment(=) => captures_assign
    procedure, private :: captures_assign
    final :: captures_final
  end type captures_t

  interface
    function cre_version() bind(c, name="cre_version") result(version)
      import :: cre_str
      type(cre_str) :: version
    end function cre_version

    function cre_last_error_message() bind(c, name="cre_last_error_message") result(message)
      import :: cre_str
      type(cre_str) :: message
    end function cre_last_error_message

    function cre_last_error_offset() bind(c, name="cre_last_error_offset") result(offset)
      import :: c_size_t
      integer(c_size_t) :: offset
    end function cre_last_error_offset

    function cre_regex_new(pattern, pattern_len, flags, flags_len, out) bind(c, name="cre_regex_new") result(status)
      import :: c_char, c_size_t, c_ptr, c_int32_t
      character(kind=c_char), intent(in) :: pattern(*), flags(*)
      integer(c_size_t), value :: pattern_len, flags_len
      type(c_ptr), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_new

    subroutine cre_regex_free(regex) bind(c, name="cre_regex_free")
      import :: c_ptr
      type(c_ptr), value :: regex
    end subroutine cre_regex_free

    function cre_regex_is_valid(pattern, pattern_len, flags, flags_len, out) bind(c, name="cre_regex_is_valid") &
        result(status)
      import :: c_char, c_size_t, c_bool, c_int32_t
      character(kind=c_char), intent(in) :: pattern(*), flags(*)
      integer(c_size_t), value :: pattern_len, flags_len
      logical(c_bool), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_is_valid

    function cre_regex_source(regex) bind(c, name="cre_regex_source") result(text)
      import :: c_ptr, cre_str
      type(c_ptr), value :: regex
      type(cre_str) :: text
    end function cre_regex_source

    function cre_regex_flags(regex) bind(c, name="cre_regex_flags") result(text)
      import :: c_ptr, cre_str
      type(c_ptr), value :: regex
      type(cre_str) :: text
    end function cre_regex_flags

    function cre_regex_group_count(regex) bind(c, name="cre_regex_group_count") result(n)
      import :: c_ptr, c_size_t
      type(c_ptr), value :: regex
      integer(c_size_t) :: n
    end function cre_regex_group_count

    function cre_regex_group_name_count(regex) bind(c, name="cre_regex_group_name_count") result(n)
      import :: c_ptr, c_size_t
      type(c_ptr), value :: regex
      integer(c_size_t) :: n
    end function cre_regex_group_name_count

    function cre_regex_group_name(regex, index) bind(c, name="cre_regex_group_name") result(text)
      import :: c_ptr, c_size_t, cre_str
      type(c_ptr), value :: regex
      integer(c_size_t), value :: index
      type(cre_str) :: text
    end function cre_regex_group_name

    function cre_regex_is_match(regex, text, len, out) bind(c, name="cre_regex_is_match") result(status)
      import :: c_ptr, c_char, c_size_t, c_bool, c_int32_t
      type(c_ptr), value :: regex
      character(kind=c_char), intent(in) :: text(*)
      integer(c_size_t), value :: len
      logical(c_bool), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_is_match

    function cre_regex_is_match_limit(regex, text, len, limit, out) bind(c, name="cre_regex_is_match_limit") &
        result(status)
      import :: c_ptr, c_char, c_size_t, c_int64_t, c_bool, c_int32_t
      type(c_ptr), value :: regex
      character(kind=c_char), intent(in) :: text(*)
      integer(c_size_t), value :: len
      integer(c_int64_t), value :: limit
      logical(c_bool), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_is_match_limit

    function cre_regex_is_match_at(regex, text, len, start, out) bind(c, name="cre_regex_is_match_at") result(status)
      import :: c_ptr, c_char, c_size_t, c_bool, c_int32_t
      type(c_ptr), value :: regex
      character(kind=c_char), intent(in) :: text(*)
      integer(c_size_t), value :: len, start
      logical(c_bool), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_is_match_at

    function cre_regex_find_limit(regex, text, len, start, limit, match, found) bind(c, name="cre_regex_find_limit") &
        result(status)
      import :: c_ptr, c_char, c_size_t, c_int64_t, c_bool, c_int32_t, cre_match
      type(c_ptr), value :: regex
      character(kind=c_char), intent(in) :: text(*)
      integer(c_size_t), value :: len, start
      integer(c_int64_t), value :: limit
      type(cre_match), intent(out) :: match
      logical(c_bool), intent(out) :: found
      integer(c_int32_t) :: status
    end function cre_regex_find_limit

    function cre_regex_captures_limit(regex, text, len, start, limit, captures, found) &
        bind(c, name="cre_regex_captures_limit") result(status)
      import :: c_ptr, c_char, c_size_t, c_int64_t, c_bool, c_int32_t
      type(c_ptr), value :: regex
      character(kind=c_char), intent(in) :: text(*)
      integer(c_size_t), value :: len, start
      integer(c_int64_t), value :: limit
      type(c_ptr), value :: captures
      logical(c_bool), intent(out) :: found
      integer(c_int32_t) :: status
    end function cre_regex_captures_limit

    function cre_regex_find_iter(regex, text, len, out) bind(c, name="cre_regex_find_iter") result(status)
      import :: c_ptr, c_char, c_size_t, c_int32_t
      type(c_ptr), value :: regex
      character(kind=c_char), intent(in) :: text(*)
      integer(c_size_t), value :: len
      type(c_ptr), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_find_iter

    function cre_regex_captures_iter(regex, text, len, out) bind(c, name="cre_regex_captures_iter") result(status)
      import :: c_ptr, c_char, c_size_t, c_int32_t
      type(c_ptr), value :: regex
      character(kind=c_char), intent(in) :: text(*)
      integer(c_size_t), value :: len
      type(c_ptr), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_captures_iter

    function cre_regex_replace(regex, text, len, replacement, replacement_len, out) bind(c, name="cre_regex_replace") &
        result(status)
      import :: c_ptr, c_char, c_size_t, c_int32_t, cre_str
      type(c_ptr), value :: regex
      character(kind=c_char), intent(in) :: text(*), replacement(*)
      integer(c_size_t), value :: len, replacement_len
      type(cre_str), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_replace

    function cre_regex_replace_all(regex, text, len, replacement, replacement_len, out) &
        bind(c, name="cre_regex_replace_all") result(status)
      import :: c_ptr, c_char, c_size_t, c_int32_t, cre_str
      type(c_ptr), value :: regex
      character(kind=c_char), intent(in) :: text(*), replacement(*)
      integer(c_size_t), value :: len, replacement_len
      type(cre_str), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_replace_all

    function cre_regex_split(regex, text, len, limit, out) bind(c, name="cre_regex_split") result(status)
      import :: c_ptr, c_char, c_size_t, c_int32_t, cre_pieces
      type(c_ptr), value :: regex
      character(kind=c_char), intent(in) :: text(*)
      integer(c_size_t), value :: len
      integer(c_int32_t), value :: limit
      type(cre_pieces), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_split

    function cre_regex_is_match_utf16(regex, text, len, out) bind(c, name="cre_regex_is_match_utf16") result(status)
      import :: c_ptr, c_int16_t, c_size_t, c_bool, c_int32_t
      type(c_ptr), value :: regex
      integer(c_int16_t), intent(in) :: text(*)
      integer(c_size_t), value :: len
      logical(c_bool), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_is_match_utf16

    function cre_regex_is_match_limit_utf16(regex, text, len, limit, out) bind(c, name="cre_regex_is_match_limit_utf16") &
        result(status)
      import :: c_ptr, c_int16_t, c_size_t, c_int64_t, c_bool, c_int32_t
      type(c_ptr), value :: regex
      integer(c_int16_t), intent(in) :: text(*)
      integer(c_size_t), value :: len
      integer(c_int64_t), value :: limit
      logical(c_bool), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_is_match_limit_utf16

    function cre_regex_is_match_at_utf16(regex, text, len, start, out) bind(c, name="cre_regex_is_match_at_utf16") &
        result(status)
      import :: c_ptr, c_int16_t, c_size_t, c_bool, c_int32_t
      type(c_ptr), value :: regex
      integer(c_int16_t), intent(in) :: text(*)
      integer(c_size_t), value :: len, start
      logical(c_bool), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_is_match_at_utf16

    function cre_regex_find_limit_utf16(regex, text, len, start, limit, match, found) &
        bind(c, name="cre_regex_find_limit_utf16") result(status)
      import :: c_ptr, c_int16_t, c_size_t, c_int64_t, c_bool, c_int32_t, cre_match
      type(c_ptr), value :: regex
      integer(c_int16_t), intent(in) :: text(*)
      integer(c_size_t), value :: len, start
      integer(c_int64_t), value :: limit
      type(cre_match), intent(out) :: match
      logical(c_bool), intent(out) :: found
      integer(c_int32_t) :: status
    end function cre_regex_find_limit_utf16

    function cre_regex_captures_limit_utf16(regex, text, len, start, limit, captures, found) &
        bind(c, name="cre_regex_captures_limit_utf16") result(status)
      import :: c_ptr, c_int16_t, c_size_t, c_int64_t, c_bool, c_int32_t
      type(c_ptr), value :: regex
      integer(c_int16_t), intent(in) :: text(*)
      integer(c_size_t), value :: len, start
      integer(c_int64_t), value :: limit
      type(c_ptr), value :: captures
      logical(c_bool), intent(out) :: found
      integer(c_int32_t) :: status
    end function cre_regex_captures_limit_utf16

    function cre_regex_find_iter_utf16(regex, text, len, out) bind(c, name="cre_regex_find_iter_utf16") result(status)
      import :: c_ptr, c_int16_t, c_size_t, c_int32_t
      type(c_ptr), value :: regex
      integer(c_int16_t), intent(in) :: text(*)
      integer(c_size_t), value :: len
      type(c_ptr), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_find_iter_utf16

    function cre_regex_captures_iter_utf16(regex, text, len, out) bind(c, name="cre_regex_captures_iter_utf16") &
        result(status)
      import :: c_ptr, c_int16_t, c_size_t, c_int32_t
      type(c_ptr), value :: regex
      integer(c_int16_t), intent(in) :: text(*)
      integer(c_size_t), value :: len
      type(c_ptr), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_captures_iter_utf16

    function cre_regex_replace_utf16(regex, text, len, replacement, replacement_len, out) &
        bind(c, name="cre_regex_replace_utf16") result(status)
      import :: c_ptr, c_int16_t, c_size_t, c_int32_t, cre_str
      type(c_ptr), value :: regex
      integer(c_int16_t), intent(in) :: text(*), replacement(*)
      integer(c_size_t), value :: len, replacement_len
      type(cre_str), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_replace_utf16

    function cre_regex_replace_all_utf16(regex, text, len, replacement, replacement_len, out) &
        bind(c, name="cre_regex_replace_all_utf16") result(status)
      import :: c_ptr, c_int16_t, c_size_t, c_int32_t, cre_str
      type(c_ptr), value :: regex
      integer(c_int16_t), intent(in) :: text(*), replacement(*)
      integer(c_size_t), value :: len, replacement_len
      type(cre_str), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_replace_all_utf16

    function cre_regex_split_utf16(regex, text, len, limit, out) bind(c, name="cre_regex_split_utf16") result(status)
      import :: c_ptr, c_int16_t, c_size_t, c_int32_t, cre_pieces
      type(c_ptr), value :: regex
      integer(c_int16_t), intent(in) :: text(*)
      integer(c_size_t), value :: len
      integer(c_int32_t), value :: limit
      type(cre_pieces), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_regex_split_utf16

    function cre_captures_new(regex, out) bind(c, name="cre_captures_new") result(status)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: regex
      type(c_ptr), intent(out) :: out
      integer(c_int32_t) :: status
    end function cre_captures_new

    subroutine cre_captures_free(captures) bind(c, name="cre_captures_free")
      import :: c_ptr
      type(c_ptr), value :: captures
    end subroutine cre_captures_free

    function cre_captures_len(captures) bind(c, name="cre_captures_len") result(n)
      import :: c_ptr, c_size_t
      type(c_ptr), value :: captures
      integer(c_size_t) :: n
    end function cre_captures_len

    function cre_captures_get(captures, group, match) bind(c, name="cre_captures_get") result(found)
      import :: c_ptr, c_size_t, c_bool, cre_match
      type(c_ptr), value :: captures
      integer(c_size_t), value :: group
      type(cre_match), intent(out) :: match
      logical(c_bool) :: found
    end function cre_captures_get

    function cre_captures_name(captures, name, len, match) bind(c, name="cre_captures_name") result(found)
      import :: c_ptr, c_char, c_size_t, c_bool, cre_match
      type(c_ptr), value :: captures
      character(kind=c_char), intent(in) :: name(*)
      integer(c_size_t), value :: len
      type(cre_match), intent(out) :: match
      logical(c_bool) :: found
    end function cre_captures_name

    function cre_matches_next_batch(matches, out, capacity, count) bind(c, name="cre_matches_next_batch") &
        result(status)
      import :: c_ptr, c_size_t, c_int32_t, cre_match
      type(c_ptr), value :: matches
      type(cre_match), intent(inout) :: out(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: count
      integer(c_int32_t) :: status
    end function cre_matches_next_batch

    function cre_matches_next_captures_batch(matches, out, capacity, count) &
        bind(c, name="cre_matches_next_captures_batch") result(status)
      import :: c_ptr, c_size_t, c_int32_t, cre_match
      type(c_ptr), value :: matches
      type(cre_match), intent(inout) :: out(*)
      integer(c_size_t), value :: capacity
      integer(c_size_t), intent(out) :: count
      integer(c_int32_t) :: status
    end function cre_matches_next_captures_batch

    subroutine cre_matches_free(matches) bind(c, name="cre_matches_free")
      import :: c_ptr
      type(c_ptr), value :: matches
    end subroutine cre_matches_free

    ! A cre_string and a cre_string16 have the layout of a cre_str: a pointer and a length.
    subroutine cre_string_free(string) bind(c, name="cre_string_free")
      import :: cre_str
      type(cre_str), intent(inout) :: string
    end subroutine cre_string_free

    subroutine cre_string16_free(string) bind(c, name="cre_string16_free")
      import :: cre_str
      type(cre_str), intent(inout) :: string
    end subroutine cre_string16_free

    subroutine cre_pieces_free(pieces) bind(c, name="cre_pieces_free")
      import :: cre_pieces
      type(cre_pieces), intent(inout) :: pieces
    end subroutine cre_pieces_free

    subroutine cre_pieces16_free(pieces) bind(c, name="cre_pieces16_free")
      import :: cre_pieces
      type(cre_pieces), intent(inout) :: pieces
    end subroutine cre_pieces16_free
  end interface

contains

  !> The C library's version, such as "0.1.2".
  function library_version() result(version)
    character(:), allocatable :: version
    version = from_str(cre_version())
  end function library_version

  !> The byte offset that goes with the most recent failure on this thread: where invalid UTF-8 was found, counted
  !> from 0 (0 otherwise).
  function last_error_offset() result(offset)
    integer(int64) :: offset
    offset = int(cre_last_error_offset(), int64)
  end function last_error_offset

  !> Whether a pattern is valid with the flags, without compiling it. A pattern over a limit of size is not valid here.
  function is_valid(pattern, flags, stat, errmsg) result(valid)
    character(*), intent(in) :: pattern
    character(*), intent(in), optional :: flags
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    logical :: valid
    character(:), allocatable :: message
    logical(c_bool) :: out
    ! The flags passed as they are (a deferred-length copy draws a false maybe-uninitialized warning from gfortran 11).
    if (present(flags)) then
      call report(cre_regex_is_valid(pattern, len(pattern, c_size_t), flags, len(flags, c_size_t), out), stat, message)
    else
      call report(cre_regex_is_valid(pattern, len(pattern, c_size_t), "", 0_c_size_t, out), stat, message)
    end if
    valid = logical(out)
    if (present(errmsg)) errmsg = message
  end function is_valid

  ! A copy of a UTF-8 string the C library owns.
  function from_str(text) result(value)
    type(cre_str), intent(in) :: text
    character(:), allocatable :: value
    character(kind=c_char), pointer :: chars(:)
    integer(int64) :: i, n
    n = int(text%len, int64)
    allocate (character(n) :: value)
    if (n == 0 .or. .not. c_associated(text%ptr)) return
    call c_f_pointer(text%ptr, chars, [n])
    do i = 1, n
      value(i:i) = chars(i)
    end do
  end function from_str

  ! A copy of a UTF-16 string the C library owns.
  function from_str16(text) result(value)
    type(cre_str), intent(in) :: text
    integer(int16), allocatable :: value(:)
    integer(c_int16_t), pointer :: units(:)
    integer(int64) :: n
    n = int(text%len, int64)
    allocate (value(n))
    if (n == 0 .or. .not. c_associated(text%ptr)) return
    call c_f_pointer(text%ptr, units, [n])
    value = units
  end function from_str16

  ! A position from the C library as a match_t: from 1, the end inclusive.
  pure function to_match(m) result(r)
    type(cre_match), intent(in) :: m
    type(match_t) :: r
    if (m%start == unset) return
    r%start = int(m%start, int64) + 1
    r%end = int(m%end, int64)
    r%matched = .true.
  end function to_match

  ! A time limit in seconds as the C library's milliseconds (0 is none there, so the least limit is 1 ms).
  pure function limit_of(timeout) result(ms)
    real(real64), intent(in) :: timeout
    integer(c_int64_t) :: ms
    ms = max(1_c_int64_t, int(ceiling(timeout * 1000.0_real64), c_int64_t))
  end function limit_of

  ! Reports a status through stat, and its message through message (empty for CRE_OK), or stops the program with the
  ! message when stat is absent. The caller copies message to its errmsg: an optional deferred-length character
  ! argument is never passed on to another procedure, which gfortran 11 does not do correctly.
  subroutine report(status, stat, message)
    integer(c_int32_t), intent(in) :: status
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out) :: message
    if (present(stat)) stat = int(status)
    message = ""
    if (status == CRE_OK) return
    message = from_str(cre_last_error_message())
    if (len(message) == 0) message = status_name(int(status))
    if (.not. present(stat)) then
      write (error_unit, "(a)") "corvus_regex_ecma262: " // message
      error stop 1
    end if
  end subroutine report

  ! A failure the binding finds itself (a regular expression that is not compiled), reported as CRE_INVALID_ARGUMENT.
  subroutine report_misuse(what, stat, message)
    character(*), intent(in) :: what
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out) :: message
    if (present(stat)) stat = CRE_INVALID_ARGUMENT
    message = what
    if (.not. present(stat)) then
      write (error_unit, "(a)") "corvus_regex_ecma262: " // what
      error stop 1
    end if
  end subroutine report_misuse

  function status_name(status) result(name)
    integer, intent(in) :: status
    character(:), allocatable :: name
    select case (status)
    case (CRE_INVALID_ARGUMENT); name = "invalid argument"
    case (CRE_INVALID_UTF8); name = "invalid UTF-8"
    case (CRE_INVALID_PATTERN); name = "invalid pattern"
    case (CRE_INVALID_FLAGS); name = "invalid flags"
    case (CRE_PATTERN_TOO_LARGE); name = "pattern too large"
    case (CRE_MATCH_LIMIT); name = "match limit"
    case (CRE_PANIC); name = "internal error"
    case default; name = "failure"
    end select
  end function status_name

  ! The zero-based start the C library takes from a start from 1 (1 when absent).
  pure function start_of(start) result(s)
    integer(int64), intent(in), optional :: start
    integer(c_size_t) :: s
    s = 0
    if (present(start)) s = int(max(start, 1_int64) - 1, c_size_t)
  end function start_of

  ! ---- regex_t ----

  !> Compiles a pattern with the flags new RegExp(pattern, flags) takes, any of dgimsuvy. On failure stat is
  !> CRE_INVALID_PATTERN, CRE_INVALID_FLAGS, CRE_PATTERN_TOO_LARGE or CRE_INVALID_UTF8, and the regular expression is
  !> not compiled.
  subroutine regex_compile(self, pattern, flags, stat, errmsg)
    class(regex_t), intent(inout) :: self
    character(*), intent(in) :: pattern
    character(*), intent(in), optional :: flags
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    character(:), allocatable :: message
    integer(c_int32_t) :: status
    call cre_regex_free(self%handle)
    self%handle = c_null_ptr
    if (present(flags)) then
      status = cre_regex_new(pattern, len(pattern, c_size_t), flags, len(flags, c_size_t), self%handle)
    else
      status = cre_regex_new(pattern, len(pattern, c_size_t), "", 0_c_size_t, self%handle)
    end if
    call report(status, stat, message)
    if (present(errmsg)) errmsg = message
  end subroutine regex_compile

  !> Whether the regular expression is compiled.
  logical function regex_is_compiled(self)
    class(regex_t), intent(in) :: self
    regex_is_compiled = c_associated(self%handle)
  end function regex_is_compiled

  !> The source of the pattern, as it was given ("" when not compiled).
  function regex_source(self) result(text)
    class(regex_t), intent(in) :: self
    character(:), allocatable :: text
    text = from_str(cre_regex_source(self%handle))
  end function regex_source

  !> The flags as RegExp.prototype.flags writes them, in the order dgimsuvy.
  function regex_flags(self) result(text)
    class(regex_t), intent(in) :: self
    character(:), allocatable :: text
    text = from_str(cre_regex_flags(self%handle))
  end function regex_flags

  !> The number of capturing groups (a match has one more, the whole match, group 0).
  integer function regex_group_count(self)
    class(regex_t), intent(in) :: self
    regex_group_count = int(cre_regex_group_count(self%handle))
  end function regex_group_count

  !> The number of distinct names of the groups. Two groups in separate alternatives may share a name.
  integer function regex_group_name_count(self)
    class(regex_t), intent(in) :: self
    regex_group_name_count = int(cre_regex_group_name_count(self%handle))
  end function regex_group_name_count

  !> The name index (from 1), in the order of the first group of each name ("" when there is none).
  function regex_group_name(self, index) result(name)
    class(regex_t), intent(in) :: self
    integer, intent(in) :: index
    character(:), allocatable :: name
    name = ""
    if (index < 1) return
    name = from_str(cre_regex_group_name(self%handle, int(index - 1, c_size_t)))
  end function regex_group_name

  !> Whether the pattern matches the text at or after start (at start alone with the y flag), as RegExp.prototype.test
  !> with lastIndex at start - 1. timeout limits the time the match may take, in seconds: a match that reaches it is
  !> CRE_MATCH_LIMIT.
  function test_utf8(self, text, start, timeout, stat, errmsg) result(matched)
    class(regex_t), intent(in) :: self
    character(*), intent(in) :: text
    integer(int64), intent(in), optional :: start
    real(real64), intent(in), optional :: timeout
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    logical :: matched
    character(:), allocatable :: message
    logical(c_bool) :: out
    type(cre_match) :: m
    integer(c_int32_t) :: status
    ! From the start the match need not be found to its end; from elsewhere only find takes a limit.
    if (present(timeout) .and. .not. present(start)) then
      status = cre_regex_is_match_limit(self%handle, text, len(text, c_size_t), limit_of(timeout), out)
    else if (present(timeout)) then
      status = cre_regex_find_limit(self%handle, text, len(text, c_size_t), start_of(start), limit_of(timeout), m, out)
    else if (present(start)) then
      status = cre_regex_is_match_at(self%handle, text, len(text, c_size_t), start_of(start), out)
    else
      status = cre_regex_is_match(self%handle, text, len(text, c_size_t), out)
    end if
    call report(status, stat, message)
    matched = logical(out)
    if (present(errmsg)) errmsg = message
  end function test_utf8

  function test_utf16(self, text, start, timeout, stat, errmsg) result(matched)
    class(regex_t), intent(in) :: self
    integer(int16), intent(in), contiguous :: text(:)
    integer(int64), intent(in), optional :: start
    real(real64), intent(in), optional :: timeout
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    logical :: matched
    character(:), allocatable :: message
    logical(c_bool) :: out
    type(cre_match) :: m
    integer(c_int32_t) :: status
    if (present(timeout) .and. .not. present(start)) then
      status = cre_regex_is_match_limit_utf16(self%handle, text, size(text, kind=c_size_t), limit_of(timeout), out)
    else if (present(timeout)) then
      status = cre_regex_find_limit_utf16(self%handle, text, size(text, kind=c_size_t), start_of(start), &
                                          limit_of(timeout), m, out)
    else if (present(start)) then
      status = cre_regex_is_match_at_utf16(self%handle, text, size(text, kind=c_size_t), start_of(start), out)
    else
      status = cre_regex_is_match_utf16(self%handle, text, size(text, kind=c_size_t), out)
    end if
    call report(status, stat, message)
    matched = logical(out)
    if (present(errmsg)) errmsg = message
  end function test_utf16

  !> The first match at or after start, as exec finds it with lastIndex at start - 1: whether there is one, and match,
  !> where it is.
  function find_utf8(self, text, match, start, timeout, stat, errmsg) result(found)
    class(regex_t), intent(in) :: self
    character(*), intent(in) :: text
    type(match_t), intent(out) :: match
    integer(int64), intent(in), optional :: start
    real(real64), intent(in), optional :: timeout
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    logical :: found
    character(:), allocatable :: message
    logical(c_bool) :: out
    type(cre_match) :: m
    integer(c_int64_t) :: limit
    limit = 0
    if (present(timeout)) limit = limit_of(timeout)
    call report(cre_regex_find_limit(self%handle, text, len(text, c_size_t), start_of(start), limit, m, out), stat, &
                message)
    found = logical(out)
    match = to_match(m)
    if (present(errmsg)) errmsg = message
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
    character(:), allocatable :: message
    logical(c_bool) :: out
    type(cre_match) :: m
    integer(c_int64_t) :: limit
    limit = 0
    if (present(timeout)) limit = limit_of(timeout)
    call report(cre_regex_find_limit_utf16(self%handle, text, size(text, kind=c_size_t), start_of(start), limit, m, &
                                           out), stat, message)
    found = logical(out)
    match = to_match(m)
    if (present(errmsg)) errmsg = message
  end function find_utf16

  ! Makes captures ready for this regular expression: kept when it was made for a regular expression at the same
  ! address with the same source and flags (whose groups and names are the same), made again otherwise.
  subroutine prepare(self, captures, status)
    class(regex_t), intent(in) :: self
    type(captures_t), intent(inout) :: captures
    integer(c_int32_t), intent(out) :: status
    status = CRE_OK
    if (c_associated(captures%handle) .and. c_associated(captures%regex, self%handle)) then
      if (same(cre_regex_source(self%handle), captures%source) .and. &
          same(cre_regex_flags(self%handle), captures%flags)) return
    end if
    call cre_captures_free(captures%handle)
    captures%handle = c_null_ptr
    captures%regex = self%handle
    captures%source = self%source()
    captures%flags = self%flags()
    status = cre_captures_new(self%handle, captures%handle)
  end subroutine prepare

  ! Whether a string the C library owns is value, read in place.
  logical function same(text, value)
    type(cre_str), intent(in) :: text
    character(:), allocatable, intent(in) :: value
    character(kind=c_char), pointer :: chars(:)
    integer(int64) :: i, n
    same = .false.
    if (.not. allocated(value)) return
    n = int(text%len, int64)
    if (n /= len(value, int64)) return
    same = .true.
    if (n == 0) return
    call c_f_pointer(text%ptr, chars, [n])
    do i = 1, n
      if (chars(i) /= value(i:i)) then
        same = .false.
        return
      end if
    end do
  end function same

  !> The first match at or after start with what each group captured, by number and by name, in captures: whether
  !> there is one.
  function exec_utf8(self, text, captures, start, timeout, stat, errmsg) result(found)
    class(regex_t), intent(in) :: self
    character(*), intent(in) :: text
    type(captures_t), intent(inout) :: captures
    integer(int64), intent(in), optional :: start
    real(real64), intent(in), optional :: timeout
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    logical :: found
    character(:), allocatable :: message
    logical(c_bool) :: out
    integer(c_int32_t) :: status
    integer(c_int64_t) :: limit
    limit = 0
    if (present(timeout)) limit = limit_of(timeout)
    out = .false.
    call prepare(self, captures, status)
    if (status == CRE_OK) then
      status = cre_regex_captures_limit(self%handle, text, len(text, c_size_t), start_of(start), limit, &
                                        captures%handle, out)
    end if
    call report(status, stat, message)
    found = logical(out)
    if (present(errmsg)) errmsg = message
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
    character(:), allocatable :: message
    logical(c_bool) :: out
    integer(c_int32_t) :: status
    integer(c_int64_t) :: limit
    limit = 0
    if (present(timeout)) limit = limit_of(timeout)
    out = .false.
    call prepare(self, captures, status)
    if (status == CRE_OK) then
      status = cre_regex_captures_limit_utf16(self%handle, text, size(text, kind=c_size_t), start_of(start), limit, &
                                              captures%handle, out)
    end if
    call report(status, stat, message)
    found = logical(out)
    if (present(errmsg)) errmsg = message
  end function exec_utf16

  ! Every match of a global search (iter, freed here), the whole match of each.
  subroutine collect(iter, matches, status)
    type(c_ptr), intent(in) :: iter
    type(match_t), allocatable, intent(out) :: matches(:)
    integer(c_int32_t), intent(inout) :: status
    type(cre_match) :: batch(256)
    type(match_t), allocatable :: grown(:)
    integer(c_size_t) :: n
    integer(int64) :: k, i
    allocate (matches(64))
    k = 0
    do while (status == CRE_OK)
      status = cre_matches_next_batch(iter, batch, size(batch, kind=c_size_t), n)
      if (status /= CRE_OK .or. n == 0) exit
      if (k + n > size(matches)) then
        allocate (grown(max(2 * size(matches, kind=int64), k + n)))
        grown(1:k) = matches(1:k)
        call move_alloc(grown, matches)
      end if
      do i = 1, n
        matches(k + i) = to_match(batch(i))
      end do
      k = k + n
      if (n < size(batch)) exit
    end do
    call cre_matches_free(iter)
    matches = matches(1:k)
  end subroutine collect

  !> Every match of a global search, as matchAll makes it: after an empty match the search goes on one character
  !> later (one code unit without u or v), and with the y flag each match must start where the one before it ended.
  !> Whether or not the pattern has the g flag, every match is found.
  subroutine find_all_utf8(self, text, matches, stat, errmsg)
    class(regex_t), intent(in) :: self
    character(*), intent(in) :: text
    type(match_t), allocatable, intent(out) :: matches(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    character(:), allocatable :: message
    type(c_ptr) :: iter
    integer(c_int32_t) :: status
    status = cre_regex_find_iter(self%handle, text, len(text, c_size_t), iter)
    call collect(iter, matches, status)
    call report(status, stat, message)
    if (present(errmsg)) errmsg = message
  end subroutine find_all_utf8

  subroutine find_all_utf16(self, text, matches, stat, errmsg)
    class(regex_t), intent(in) :: self
    integer(int16), intent(in), contiguous :: text(:)
    type(match_t), allocatable, intent(out) :: matches(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    character(:), allocatable :: message
    type(c_ptr) :: iter
    integer(c_int32_t) :: status
    status = cre_regex_find_iter_utf16(self%handle, text, size(text, kind=c_size_t), iter)
    call collect(iter, matches, status)
    call report(status, stat, message)
    if (present(errmsg)) errmsg = message
  end subroutine find_all_utf16

  ! Every match of a global search made with its groups (iter, freed here): groups(0:width - 1, k) for the k-th.
  subroutine collect_groups(iter, width, groups, status)
    type(c_ptr), intent(in) :: iter
    integer, intent(in) :: width
    type(match_t), allocatable, intent(out) :: groups(:, :)
    integer(c_int32_t), intent(inout) :: status
    type(cre_match), allocatable :: batch(:)
    type(match_t), allocatable :: grown(:, :)
    integer(c_size_t) :: n
    integer(int64) :: k, i, g
    integer, parameter :: per_batch = 64
    allocate (batch(width * per_batch))
    allocate (groups(0:width - 1, 16))
    k = 0
    do while (status == CRE_OK)
      status = cre_matches_next_captures_batch(iter, batch, size(batch, kind=c_size_t), n)
      if (status /= CRE_OK .or. n == 0) exit
      if (k + n > size(groups, 2)) then
        allocate (grown(0:width - 1, max(2 * size(groups, 2, kind=int64), k + n)))
        grown(:, 1:k) = groups(:, 1:k)
        call move_alloc(grown, groups)
      end if
      do i = 1, n
        do g = 0, width - 1
          groups(g, k + i) = to_match(batch((i - 1) * width + g + 1))
        end do
      end do
      k = k + n
      if (n < per_batch) exit
    end do
    call cre_matches_free(iter)
    ! Trimmed to the matches found, keeping group 0 at index 0 (an assignment of a section would number it from 1).
    allocate (grown(0:width - 1, k))
    grown = groups(:, 1:k)
    call move_alloc(grown, groups)
  end subroutine collect_groups

  !> Every match of a global search with what each group captured: groups(g, k) is group g (0 the whole match) of the
  !> k-th match.
  subroutine exec_all_utf8(self, text, groups, stat, errmsg)
    class(regex_t), intent(in) :: self
    character(*), intent(in) :: text
    type(match_t), allocatable, intent(out) :: groups(:, :)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    character(:), allocatable :: message
    type(c_ptr) :: iter
    integer(c_int32_t) :: status
    status = cre_regex_captures_iter(self%handle, text, len(text, c_size_t), iter)
    call collect_groups(iter, self%group_count() + 1, groups, status)
    call report(status, stat, message)
    if (present(errmsg)) errmsg = message
  end subroutine exec_all_utf8

  subroutine exec_all_utf16(self, text, groups, stat, errmsg)
    class(regex_t), intent(in) :: self
    integer(int16), intent(in), contiguous :: text(:)
    type(match_t), allocatable, intent(out) :: groups(:, :)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    character(:), allocatable :: message
    type(c_ptr) :: iter
    integer(c_int32_t) :: status
    status = cre_regex_captures_iter_utf16(self%handle, text, size(text, kind=c_size_t), iter)
    call collect_groups(iter, self%group_count() + 1, groups, status)
    call report(status, stat, message)
    if (present(errmsg)) errmsg = message
  end subroutine exec_all_utf16

  ! How many matches a global search (iter, freed here) finds.
  function tally(iter, status) result(n)
    type(c_ptr), intent(in) :: iter
    integer(c_int32_t), intent(inout) :: status
    integer(int64) :: n
    type(cre_match) :: batch(256)
    integer(c_size_t) :: k
    n = 0
    do while (status == CRE_OK)
      status = cre_matches_next_batch(iter, batch, size(batch, kind=c_size_t), k)
      if (status /= CRE_OK) exit
      n = n + k
      if (k < size(batch)) exit
    end do
    call cre_matches_free(iter)
  end function tally

  !> How many matches a global search finds, none of them made.
  function count_utf8(self, text, stat, errmsg) result(n)
    class(regex_t), intent(in) :: self
    character(*), intent(in) :: text
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    integer(int64) :: n
    character(:), allocatable :: message
    type(c_ptr) :: iter
    integer(c_int32_t) :: status
    status = cre_regex_find_iter(self%handle, text, len(text, c_size_t), iter)
    n = tally(iter, status)
    call report(status, stat, message)
    if (present(errmsg)) errmsg = message
  end function count_utf8

  function count_utf16(self, text, stat, errmsg) result(n)
    class(regex_t), intent(in) :: self
    integer(int16), intent(in), contiguous :: text(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    integer(int64) :: n
    character(:), allocatable :: message
    type(c_ptr) :: iter
    integer(c_int32_t) :: status
    status = cre_regex_find_iter_utf16(self%handle, text, size(text, kind=c_size_t), iter)
    n = tally(iter, status)
    call report(status, stat, message)
    if (present(errmsg)) errmsg = message
  end function count_utf16

  !> The text with the first match replaced, or every match with the g flag (String.prototype.replace). In the
  !> replacement, $& is the match, $` and $' the text before and after it, $1 to $99 a group, $<name> a named group, and
  !> $$ a dollar sign. A half character left by a match without u or v is written as U+FFFD.
  function replace_utf8(self, text, replacement, stat, errmsg) result(value)
    class(regex_t), intent(in) :: self
    character(*), intent(in) :: text, replacement
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    character(:), allocatable :: value
    character(:), allocatable :: message
    type(cre_str) :: out
    call report(cre_regex_replace(self%handle, text, len(text, c_size_t), replacement, len(replacement, c_size_t), out), &
                stat, message)
    value = from_str(out)
    call cre_string_free(out)
    if (present(errmsg)) errmsg = message
  end function replace_utf8

  function replace_utf16(self, text, replacement, stat, errmsg) result(value)
    class(regex_t), intent(in) :: self
    integer(int16), intent(in), contiguous :: text(:), replacement(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    integer(int16), allocatable :: value(:)
    character(:), allocatable :: message
    type(cre_str) :: out
    call report(cre_regex_replace_utf16(self%handle, text, size(text, kind=c_size_t), replacement, &
                                        size(replacement, kind=c_size_t), out), stat, message)
    value = from_str16(out)
    call cre_string16_free(out)
    if (present(errmsg)) errmsg = message
  end function replace_utf16

  !> The text with every match replaced (String.prototype.replaceAll), whether or not the pattern has the g flag.
  function replace_all_utf8(self, text, replacement, stat, errmsg) result(value)
    class(regex_t), intent(in) :: self
    character(*), intent(in) :: text, replacement
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    character(:), allocatable :: value
    character(:), allocatable :: message
    type(cre_str) :: out
    call report(cre_regex_replace_all(self%handle, text, len(text, c_size_t), replacement, &
                                      len(replacement, c_size_t), out), stat, message)
    value = from_str(out)
    call cre_string_free(out)
    if (present(errmsg)) errmsg = message
  end function replace_all_utf8

  function replace_all_utf16(self, text, replacement, stat, errmsg) result(value)
    class(regex_t), intent(in) :: self
    integer(int16), intent(in), contiguous :: text(:), replacement(:)
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    integer(int16), allocatable :: value(:)
    character(:), allocatable :: message
    type(cre_str) :: out
    call report(cre_regex_replace_all_utf16(self%handle, text, size(text, kind=c_size_t), replacement, &
                                            size(replacement, kind=c_size_t), out), stat, message)
    value = from_str16(out)
    call cre_string16_free(out)
    if (present(errmsg)) errmsg = message
  end function replace_all_utf16

  !> The text split around the matches (String.prototype.split): what each group captured is spliced in after the
  !> piece before it (defined = .false. for a group that did not take part), an empty match at the start or the end
  !> of the text splits nothing, and there are at most limit pieces when limit is given.
  subroutine split_utf8(self, text, pieces, limit, stat, errmsg)
    class(regex_t), intent(in) :: self
    character(*), intent(in) :: text
    type(piece_t), allocatable, intent(out) :: pieces(:)
    integer, intent(in), optional :: limit
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    character(:), allocatable :: message
    type(cre_pieces) :: out
    type(cre_str), pointer :: items(:)
    integer(c_int32_t) :: most
    integer(int64) :: i
    most = split_all
    if (present(limit)) most = int(max(limit, 0), c_int32_t)
    call report(cre_regex_split(self%handle, text, len(text, c_size_t), most, out), stat, message)
    allocate (pieces(out%len))
    if (out%len > 0) then
      call c_f_pointer(out%items, items, [out%len])
      do i = 1, size(pieces, kind=int64)
        pieces(i)%defined = c_associated(items(i)%ptr)
        pieces(i)%value = from_str(items(i))
      end do
    end if
    call cre_pieces_free(out)
    if (present(errmsg)) errmsg = message
  end subroutine split_utf8

  subroutine split_utf16(self, text, pieces, limit, stat, errmsg)
    class(regex_t), intent(in) :: self
    integer(int16), intent(in), contiguous :: text(:)
    type(piece16_t), allocatable, intent(out) :: pieces(:)
    integer, intent(in), optional :: limit
    integer, intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: errmsg
    character(:), allocatable :: message
    type(cre_pieces) :: out
    type(cre_str), pointer :: items(:)
    integer(c_int32_t) :: most
    integer(int64) :: i
    most = split_all
    if (present(limit)) most = int(max(limit, 0), c_int32_t)
    call report(cre_regex_split_utf16(self%handle, text, size(text, kind=c_size_t), most, out), stat, message)
    allocate (pieces(out%len))
    if (out%len > 0) then
      call c_f_pointer(out%items, items, [out%len])
      do i = 1, size(pieces, kind=int64)
        pieces(i)%defined = c_associated(items(i)%ptr)
        pieces(i)%value = from_str16(items(i))
      end do
    end if
    call cre_pieces16_free(out)
    if (present(errmsg)) errmsg = message
  end subroutine split_utf16

  ! Assignment compiles the same pattern again: a regex_t holds the C library's handle, which has no copy.
  subroutine regex_assign(self, other)
    class(regex_t), intent(inout) :: self
    type(regex_t), intent(in) :: other
    character(:), allocatable :: message
    integer :: stat
    call cre_regex_free(self%handle)
    self%handle = c_null_ptr
    if (.not. c_associated(other%handle)) return
    call self%compile(other%source(), other%flags(), stat)
    if (stat /= CRE_OK) call report_misuse("a compiled pattern did not compile again", message=message)
  end subroutine regex_assign

  subroutine regex_final(self)
    type(regex_t), intent(inout) :: self
    call cre_regex_free(self%handle)
    self%handle = c_null_ptr
  end subroutine regex_final

  ! ---- captures_t ----

  !> The number of groups of the last match, group 0 among them, or 0 when the last search found none.
  integer function captures_count(self)
    class(captures_t), intent(in) :: self
    captures_count = int(cre_captures_len(self%handle))
  end function captures_count

  !> The group of the last match (0 is the whole match); matched = .false. when it did not take part, there is no such
  !> group, or the last search found no match.
  function captures_group(self, group) result(match)
    class(captures_t), intent(in) :: self
    integer, intent(in) :: group
    type(match_t) :: match
    type(cre_match) :: m
    if (group < 0) return
    if (cre_captures_get(self%handle, int(group, c_size_t), m)) match = to_match(m)
  end function captures_group

  !> The group of the name in the last match (of the groups that share it, the one that took part).
  function captures_named(self, name) result(match)
    class(captures_t), intent(in) :: self
    character(*), intent(in) :: name
    type(match_t) :: match
    type(cre_match) :: m
    if (cre_captures_name(self%handle, name, len(name, c_size_t), m)) match = to_match(m)
  end function captures_named

  subroutine captures_assign(self, other)
    class(captures_t), intent(inout) :: self
    type(captures_t), intent(in) :: other
    character(:), allocatable :: message
    if (c_associated(other%handle) .or. c_associated(self%handle)) then
      call report_misuse("a captures_t may not be copied", message=message)
    end if
  end subroutine captures_assign

  subroutine captures_final(self)
    type(captures_t), intent(inout) :: self
    call cre_captures_free(self%handle)
    self%handle = c_null_ptr
    self%regex = c_null_ptr
  end subroutine captures_final
end module corvus_regex_ecma262
