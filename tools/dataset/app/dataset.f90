! Runs this binding over the ECMA-262 regular expression test data (github.com/corvus-dotnet/ecma262-regex-test-data),
! judged by the data's own rule ("How a runner judges an engine" in its README) exactly as the C library's runner judges
! it:
!
!   fpm run --profile release -- <data directory> <wrong-cases file> [--files <name>]     (from tools/dataset)
!
! Each record is asked about under each grammar V8 found it valid in: the record's flags with u added (the unicode
! answers), the record's flags alone (the legacy answers) and, in the file that has them, the flags with v added (the
! unicode_sets answers). A text is matched as UTF-16 (positions in code units, as the data gives them) and as UTF-8
! (positions in bytes); a text with a lone surrogate has no UTF-8 form and is matched as UTF-16 alone. Records marked
! v8_deviation are left out. Any difference from V8 is a wrong answer; a refusal of a valid pattern, an acceptance of an
! invalid one, and a match that reaches its limit are counted apart and listed too. captures.jsonl is judged on exec,
! the global search, the sticky flag, replace and split. A match is given 2 seconds. ill-formed-utf8.jsonl holds bytes
! that are not UTF-8, each one answer over UTF-8 under each grammar the pattern is valid in, whose right answer is a
! refusal: CRE_INVALID_UTF8.
!
! The records are read by tools/jsonlite, which decodes a lone surrogate as a number past the last code point of
! Unicode (0x110000 + its offset from U+D800) and a NUL as 0x110800, so that none is taken for a character of the text
! (test262's texts hold U+F0000); each is turned back into its code unit (or byte) wherever the text is used, and a
! lone surrogate in a pattern is read as U+FFFD, as the C library's runner reads it.
program dataset
  use, intrinsic :: iso_fortran_env, only: int16, int64, real64, error_unit, output_unit
  use jsonlite, only: node_t, parse, member, item, size_of, json_null, json_array, json_object, json_logical, &
                      json_string, lone_first, lone_last, nul_mark
  use corvus_regex_ecma262
  implicit none

  real(real64), parameter :: limit = 2.0_real64
  character(*), parameter :: replacement = "<$&|$1|$2|$10|$<a>|$$|$`|$'>"

  type :: line_t
    character(:), allocatable :: s
  end type line_t

  integer(int64) :: right = 0, wrong = 0, refused = 0, accepted = 0, limited = 0, errors = 0
  integer(int64) :: records = 0, skipped = 0
  type(line_t), allocatable :: wrongs(:)
  integer :: nwrong = 0

  call main()

contains

  subroutine main()
    character(:), allocatable :: data, out, only, name
    character(4096) :: arg
    type(line_t), allocatable :: files(:)
    integer :: i, u, nargs
    integer(int64) :: t0, t1, rate, r0, w0, f0, a0, l0
    nargs = command_argument_count()
    if (nargs < 2) then
      write (error_unit, "(a)") "usage: dataset <data directory> <wrong-cases file> [--files <name>]"
      error stop 2
    end if
    call get_command_argument(1, arg)
    data = trim(arg)
    call get_command_argument(2, arg)
    out = trim(arg)
    only = ""
    if (nargs >= 4) then
      call get_command_argument(4, arg)
      only = trim(arg)
    end if
    allocate (wrongs(1024))
    files = list_files(data)
    call system_clock(t0, rate)
    do i = 1, size(files)
      name = files(i)%s
      if (len(only) > 0 .and. name /= only) cycle
      r0 = right; w0 = wrong; f0 = refused; a0 = accepted; l0 = limited
      call judge_file(data // "/" // name, name)
      call system_clock(t1)
      ! The name in 24 columns, on the left, as the Rust runner writes it, and in full when it is longer.
      write (output_unit, "(a, ' right ', i10, '  wrong ', i6, '  refused-valid ', i4, '  accepted-invalid ', i4, " // &
             "'  limit ', i4, '  ', f6.1, 's')") name // repeat(" ", max(0, 24 - len(name))), right - r0, wrong - w0, &
             refused - f0, accepted - a0, limited - l0, real(t1 - t0) / real(rate)
      flush (output_unit)
    end do
    call sort_lines(wrongs(1:nwrong))
    open (newunit=u, file=out, status="replace", action="write", access="stream", form="unformatted")
    do i = 1, nwrong
      write (u) wrongs(i)%s // achar(10)
    end do
    close (u)
    call system_clock(t1)
    write (output_unit, "(/, 'records ', i0, ' (skipped ', i0, ' deviations)  right ', i0, '  wrong ', i0, " // &
           "'  refused-valid ', i0, '  accepted-invalid ', i0, '  limit ', i0, '  errors ', i0, '  in ', f0.1, 's')") &
           records, skipped, right, wrong, refused, accepted, limited, errors, real(t1 - t0) / real(rate)
    if (wrong + refused + accepted + errors + limited /= 0) error stop 1
  end subroutine main

  ! The .jsonl files of a directory, in order.
  function list_files(dir) result(files)
    character(*), intent(in) :: dir
    type(line_t), allocatable :: files(:)
    character(:), allocatable :: listing, line
    integer :: u, ios
    character(1024) :: buffer
    listing = "dataset-files.txt"
    call execute_command_line("ls -1 '" // dir // "' > " // listing)
    allocate (files(0))
    open (newunit=u, file=listing, action="read")
    do
      read (u, "(a)", iostat=ios) buffer
      if (ios /= 0) exit
      line = trim(buffer)
      if (len(line) > 6) then
        if (line(len(line) - 5:) == ".jsonl") files = [files, line_t(line)]
      end if
    end do
    close (u, status="delete")
    call sort_lines(files)
  end function list_files

  subroutine judge_file(path, name)
    character(*), intent(in) :: path, name
    character(:), allocatable :: text
    integer(int64) :: at, next, n
    integer :: u
    open (newunit=u, file=path, action="read", access="stream", form="unformatted")
    inquire (unit=u, size=n)
    allocate (character(n) :: text)
    read (u) text
    close (u)
    at = 1
    do while (at <= n)
      next = index(text(at:), achar(10), kind=int64)
      if (next == 0) next = n - at + 2
      if (next > 1) call judge_line(text(at:at + next - 2), name)
      at = at + next
    end do
  end subroutine judge_file

  subroutine judge_line(line, name)
    character(*), intent(in) :: line, name
    type(node_t), target :: r
    type(node_t), pointer :: p
    logical :: ok
    call parse(line, r, ok)
    if (.not. ok) then
      write (error_unit, "(a)") "a line of " // name // " is not JSON"
      error stop 2
    end if
    if (flag_of(r, "v8_deviation")) then
      skipped = skipped + 1
      return
    end if
    records = records + 1
    p => member(r, "pattern")
    if (.not. associated(p)) return
    if (p%kind /= json_string) return
    if (name == "captures.jsonl") then
      call judge_captures(r, p%s)
    else if (name == "ill-formed-utf8.jsonl") then
      call judge_ill_formed(r, p%s)
    else
      call judge_tests(r, p%s, name)
    end if
  end subroutine judge_line

  logical function flag_of(n, path)
    type(node_t), intent(in), target :: n
    character(*), intent(in) :: path
    type(node_t), pointer :: p
    p => member(n, path)
    flag_of = .false.
    if (associated(p)) flag_of = p%kind == json_logical .and. p%b
  end function flag_of

  ! A string member ("" and found = .false. when there is none).
  function text_of(n, path, found) result(s)
    type(node_t), intent(in), target :: n
    character(*), intent(in) :: path
    logical, intent(out) :: found
    character(:), allocatable :: s
    type(node_t), pointer :: p
    p => member(n, path)
    s = ""
    found = .false.
    if (.not. associated(p)) return
    if (p%kind /= json_string) return
    s = p%s
    found = .true.
  end function text_of

  ! The code points of UTF-8 text.
  function code_points(s) result(cps)
    character(*), intent(in) :: s
    integer, allocatable :: cps(:)
    integer :: i, n, k, b
    allocate (cps(len(s)))
    i = 1
    k = 0
    n = len(s)
    do while (i <= n)
      b = iachar(s(i:i))
      k = k + 1
      if (b < 128) then
        cps(k) = b
        i = i + 1
      else if (b < 224) then
        cps(k) = ior(ishft(iand(b, 31), 6), iand(iachar(s(i + 1:i + 1)), 63))
        i = i + 2
      else if (b < 240) then
        cps(k) = ior(ior(ishft(iand(b, 15), 12), ishft(iand(iachar(s(i + 1:i + 1)), 63), 6)), &
                     iand(iachar(s(i + 2:i + 2)), 63))
        i = i + 3
      else
        cps(k) = ior(ior(ior(ishft(iand(b, 7), 18), ishft(iand(iachar(s(i + 1:i + 1)), 63), 12)), &
                         ishft(iand(iachar(s(i + 2:i + 2)), 63), 6)), iand(iachar(s(i + 3:i + 3)), 63))
        i = i + 4
      end if
    end do
    cps = cps(1:k)
  end function code_points

  elemental logical function is_lone(cp)
    integer, intent(in) :: cp
    is_lone = cp >= lone_first .and. cp <= lone_last
  end function is_lone

  ! Whether a text holds a lone surrogate (written as UTF-8 writes 0x110000 and on, from the byte F4, which Plane 16's
  ! code points start with too).
  logical function lone(s)
    character(*), intent(in) :: s
    lone = .false.
    if (index(s, achar(244)) == 0) return
    lone = any(is_lone(code_points(s)))
  end function lone

  ! The UTF-16 code units of a text as read.
  function units(s) result(u)
    character(*), intent(in) :: s
    integer(int16), allocatable :: u(:)
    integer, allocatable :: cps(:)
    integer :: i, k, c
    cps = code_points(s)
    allocate (u(2 * size(cps)))
    k = 0
    do i = 1, size(cps)
      c = cps(i)
      if (is_lone(c)) then
        k = k + 1
        u(k) = to16(int(z'D800') + c - lone_first)
      else if (c == nul_mark) then
        k = k + 1
        u(k) = 0
      else if (c > int(z'FFFF')) then
        u(k + 1) = to16(int(z'D800') + ishft(c - int(z'10000'), -10))
        u(k + 2) = to16(int(z'DC00') + iand(c - int(z'10000'), int(z'3FF')))
        k = k + 2
      else
        k = k + 1
        u(k) = to16(c)
      end if
    end do
    u = u(1:k)
  end function units

  elemental integer(int16) function to16(v)
    integer, intent(in) :: v
    to16 = int(merge(v - 65536, v, v > 32767), int16)
  end function to16

  elemental integer function from16(v)
    integer(int16), intent(in) :: v
    from16 = merge(int(v) + 65536, int(v), v < 0)
  end function from16

  function utf8_of(c) result(s)
    integer, intent(in) :: c
    character(:), allocatable :: s
    if (c < 128) then
      s = achar(c)
    else if (c < 2048) then
      s = achar(ior(192, ishft(c, -6))) // achar(ior(128, iand(c, 63)))
    else if (c < 65536) then
      s = achar(ior(224, ishft(c, -12))) // achar(ior(128, iand(ishft(c, -6), 63))) // achar(ior(128, iand(c, 63)))
    else
      s = achar(ior(240, ishft(c, -18))) // achar(ior(128, iand(ishft(c, -12), 63))) // &
          achar(ior(128, iand(ishft(c, -6), 63))) // achar(ior(128, iand(c, 63)))
    end if
  end function utf8_of

  ! The UTF-8 of a text as read, a lone surrogate as U+FFFD (as String::from_utf16_lossy writes it) and a NUL as a NUL.
  function lossy(s) result(out)
    character(*), intent(in) :: s
    character(:), allocatable :: out
    integer, allocatable :: cps(:)
    integer :: i
    if (index(s, achar(244)) == 0) then
      out = s
      return
    end if
    cps = code_points(s)
    out = ""
    do i = 1, size(cps)
      if (is_lone(cps(i))) then
        out = out // utf8_of(int(z'FFFD'))
      else if (cps(i) == nul_mark) then
        out = out // achar(0)
      else
        out = out // utf8_of(cps(i))
      end if
    end do
  end function lossy

  ! The UTF-16 code units of s, printable ASCII as it is and anything else as \uXXXX.
  function escape(s) result(out)
    character(*), intent(in) :: s
    character(:), allocatable :: out
    integer(int16), allocatable :: u(:)
    character(:), allocatable :: buffer
    integer :: i, v, k
    character(6) :: e
    u = units(s)
    ! Into a buffer of the most it can take (six characters a unit), not grown a character at a time.
    allocate (character(6 * size(u)) :: buffer)
    k = 0
    do i = 1, size(u)
      v = from16(u(i))
      if (v >= 32 .and. v < 127 .and. v /= 92) then
        buffer(k + 1:k + 1) = achar(v)
        k = k + 1
      else
        write (e, "('\u', z4.4)") v
        buffer(k + 1:k + 6) = to_lower(e)
        k = k + 6
      end if
    end do
    out = buffer(1:k)
  end function escape

  function to_lower(s) result(out)
    character(*), intent(in) :: s
    character(len(s)) :: out
    integer :: i
    out = s
    do i = 1, len(s)
      if (s(i:i) >= "A" .and. s(i:i) <= "Z") out(i:i) = achar(iachar(s(i:i)) + 32)
    end do
  end function to_lower

  function shown(pattern) result(out)
    character(*), intent(in) :: pattern
    character(:), allocatable :: out
    out = escape(lossy(pattern))
  end function shown

  ! For each UTF-16 code unit index of a text (and its end), the UTF-8 byte offset; the low half of a pair is its
  ! character's third byte. Allocated here, indexed from 0 as the data's positions are (a function result would be
  ! numbered from 1 once assigned).
  subroutine byte_offsets(s, at)
    character(*), intent(in) :: s
    integer(int64), allocatable, intent(out) :: at(:)
    integer, allocatable :: cps(:)
    integer :: i, k
    integer(int64) :: b
    cps = code_points(s)
    allocate (at(0:size(cps) + count(cps > int(z'FFFF') .and. cps /= nul_mark)))
    k = 0
    b = 0
    do i = 1, size(cps)
      if (cps(i) > int(z'FFFF') .and. cps(i) /= nul_mark) then
        at(k) = b
        at(k + 1) = b + 2
        k = k + 2
        b = b + 4
      else
        at(k) = b
        k = k + 1
        if (cps(i) == nul_mark .or. cps(i) < 128) then
          b = b + 1
        else if (cps(i) < 2048) then
          b = b + 2
        else
          b = b + 3
        end if
      end if
    end do
    at(k) = b
  end subroutine byte_offsets

  subroutine note(line)
    character(*), intent(in) :: line
    type(line_t), allocatable :: grown(:)
    if (nwrong == size(wrongs)) then
      allocate (grown(2 * size(wrongs)))
      grown(1:nwrong) = wrongs(1:nwrong)
      call move_alloc(grown, wrongs)
    end if
    nwrong = nwrong + 1
    wrongs(nwrong)%s = line
  end subroutine note

  ! A judgement of captures.jsonl; the place (the pattern and its flags, and the text, escaped) is written out only for
  ! a wrong answer.
  subroutine mark(ok, head, text, what)
    logical, intent(in) :: ok
    character(*), intent(in) :: head, text, what
    if (ok) then
      right = right + 1
    else
      wrong = wrong + 1
      call note("WRONG" // achar(9) // "captures.jsonl" // achar(9) // head // achar(9) // escape(text) // achar(9) // &
                what)
    end if
  end subroutine mark

  subroutine judge_tests(r, pattern, name)
    type(node_t), intent(in), target :: r
    character(*), intent(in) :: pattern, name
    character(:), allocatable :: flags, head, mode_flags, message
    character(12) :: answers(3)
    character(1) :: suffixes(3)
    type(node_t), pointer :: tests, test, answer, text
    type(regex_t) :: re
    logical :: found, valid
    integer :: m, nmodes, i, stat
    flags = text_of(r, "flags", found)
    suffixes = ["u", " ", "v"]
    answers = [character(12) :: "unicode", "legacy", "unicode_sets"]
    nmodes = merge(3, 2, associated(member(r, "valid.unicode_sets")))
    tests => member(r, "tests")
    do m = 1, nmodes
      mode_flags = flags // trim(suffixes(m))
      valid = flag_of(r, "valid." // trim(answers(m)))
      head = name // achar(9) // "/" // shown(pattern) // "/" // mode_flags
      call re%compile(lossy(pattern), mode_flags, stat=stat, errmsg=message)
      if (stat /= CRE_OK) then
        if (stat /= CRE_INVALID_PATTERN .and. stat /= CRE_PATTERN_TOO_LARGE) errors = errors + 1
        if (valid) then
          refused = refused + 1
          call note("REFUSED" // achar(9) // head // achar(9) // message)
        end if
        cycle
      end if
      if (.not. valid) then
        accepted = accepted + 1
        call note("ACCEPTED-INVALID" // achar(9) // head)
        cycle
      end if
      if (.not. associated(tests)) cycle
      do i = 1, size_of(tests)
        test => item(tests, i)
        answer => member(test, trim(answers(m)))
        text => member(test, "text")
        if (.not. associated(answer) .or. .not. associated(text)) cycle
        if (answer%kind /= json_logical .or. text%kind /= json_string) cycle
        call ask_test(re, head, text%s, "utf16", answer%b)
        if (.not. lone(text%s)) call ask_test(re, head, text%s, "utf8", answer%b)
      end do
    end do
  end subroutine judge_tests

  ! A record of ill-formed-utf8.jsonl: under each grammar the pattern is valid in, each test's bytes (in hexadecimal)
  ! are one answer over UTF-8, and the right one is CRE_INVALID_UTF8.
  subroutine judge_ill_formed(r, pattern)
    type(node_t), intent(in), target :: r
    character(*), intent(in) :: pattern
    character(*), parameter :: name = "ill-formed-utf8.jsonl"
    character(:), allocatable :: flags, head, mode_flags, message, bytes
    character(12) :: answers(3)
    character(1) :: suffixes(3)
    type(node_t), pointer :: tests, hex
    type(regex_t) :: re
    logical :: found, valid, got
    integer :: m, nmodes, i, k, stat, byte
    flags = text_of(r, "flags", found)
    suffixes = ["u", " ", "v"]
    answers = [character(12) :: "unicode", "legacy", "unicode_sets"]
    nmodes = merge(3, 2, associated(member(r, "valid.unicode_sets")))
    tests => member(r, "tests")
    do m = 1, nmodes
      mode_flags = flags // trim(suffixes(m))
      valid = flag_of(r, "valid." // trim(answers(m)))
      head = name // achar(9) // "/" // shown(pattern) // "/" // mode_flags
      call re%compile(lossy(pattern), mode_flags, stat=stat, errmsg=message)
      if (stat /= CRE_OK) then
        if (stat /= CRE_INVALID_PATTERN .and. stat /= CRE_PATTERN_TOO_LARGE) errors = errors + 1
        if (valid) then
          refused = refused + 1
          call note("REFUSED" // achar(9) // head // achar(9) // message)
        end if
        cycle
      end if
      if (.not. valid) then
        accepted = accepted + 1
        call note("ACCEPTED-INVALID" // achar(9) // head)
        cycle
      end if
      if (.not. associated(tests)) cycle
      do i = 1, size_of(tests)
        hex => member(item(tests, i), "bytes")
        if (.not. associated(hex)) cycle
        if (hex%kind /= json_string) cycle
        allocate (character(len(hex%s) / 2) :: bytes)
        do k = 1, len(bytes)
          read (hex%s(2 * k - 1:2 * k), "(z2)") byte
          bytes(k:k) = achar(byte)
        end do
        got = re%test(bytes, timeout=limit, stat=stat, errmsg=message)
        deallocate (bytes)
        if (stat == CRE_INVALID_UTF8) then
          right = right + 1
        else if (stat == CRE_MATCH_LIMIT) then
          limited = limited + 1
          call note("LIMIT" // achar(9) // head // achar(9) // hex%s // achar(9) // "utf8")
        else if (stat /= CRE_OK) then
          errors = errors + 1
          call note("ERROR" // achar(9) // head // achar(9) // hex%s // achar(9) // "utf8" // achar(9) // message)
        else
          wrong = wrong + 1
          call note("WRONG" // achar(9) // head // achar(9) // hex%s // achar(9) // "utf8" // achar(9) // &
                    "read as UTF-8 (" // merge("true ", "false", got) // "), not refused")
        end if
      end do
    end do
  end subroutine judge_ill_formed

  subroutine ask_test(re, head, text, enc, expected)
    type(regex_t), intent(in) :: re
    character(*), intent(in) :: head, text, enc
    logical, intent(in) :: expected
    logical :: got
    integer :: stat
    character(:), allocatable :: message
    if (enc == "utf16") then
      got = re%test(units(text), timeout=limit, stat=stat, errmsg=message)
    else
      got = re%test(lossy(text), timeout=limit, stat=stat, errmsg=message)
    end if
    if (stat == CRE_MATCH_LIMIT) then
      limited = limited + 1
      call note("LIMIT" // achar(9) // head // achar(9) // escape(text) // achar(9) // enc // achar(9) // &
                merge("memory", "time  ", index(message, "memory") > 0))
    else if (stat /= CRE_OK) then
      errors = errors + 1
      call note("ERROR" // achar(9) // head // achar(9) // escape(text) // achar(9) // enc // achar(9) // message)
    else if (got .eqv. expected) then
      right = right + 1
    else
      wrong = wrong + 1
      call note("WRONG" // achar(9) // head // achar(9) // escape(text) // achar(9) // enc // achar(9) // &
                "expected " // merge("true ", "false", expected))
    end if
  end subroutine ask_test

  ! Whether the groups of a match (from 1, inclusive ends) are the expected [start, end] pairs (from 0, exclusive ends;
  ! null for a group that did not take part), the expected positions mapped by at when it is given. expected is null
  ! (or absent) for no match.
  logical function same_groups(expected, got, matched, at)
    type(node_t), pointer, intent(in) :: expected
    type(match_t), intent(in) :: got(0:)
    logical, intent(in) :: matched
    integer(int64), intent(in), optional :: at(0:)
    integer :: i
    logical :: array
    array = .false.
    if (associated(expected)) array = expected%kind == json_array
    if (.not. array .or. .not. matched) then
      same_groups = .not. array .and. .not. matched
      return
    end if
    same_groups = .false.
    if (size_of(expected) /= size(got)) return
    do i = 1, size_of(expected)
      if (.not. same_span(item(expected, i), got(i - 1), at)) return
    end do
    same_groups = .true.
  end function same_groups

  logical function same_span(expected, got, at)
    type(node_t), intent(in) :: expected
    type(match_t), intent(in) :: got
    integer(int64), intent(in), optional :: at(0:)
    integer(int64) :: s, e
    if (expected%kind == json_null) then
      same_span = .not. got%matched
      return
    end if
    same_span = .false.
    if (.not. got%matched .or. size_of(expected) /= 2) return
    s = expected%items(1)%i
    e = expected%items(2)%i
    if (present(at)) then
      same_span = got%start - 1 == at(s) .and. got%end == at(e)
    else
      same_span = got%start - 1 == s .and. got%end == e
    end if
  end function same_span

  ! The groups of the last match in captures, 0 to the number of groups.
  function groups_of(caps, count) result(g)
    type(captures_t), intent(in) :: caps
    integer, intent(in) :: count
    type(match_t), allocatable :: g(:)
    integer :: i
    allocate (g(0:count - 1))
    do i = 0, count - 1
      g(i) = caps%group(i)
    end do
  end function groups_of

  ! Whether the first matches of a global search (their whole matches) are those of all, as the C runner judges them:
  ! the first twenty, all of them when there are fewer.
  logical function judge_all(found, all, at)
    type(match_t), intent(in) :: found(:)
    type(node_t), intent(in) :: all
    integer(int64), intent(in), optional :: at(0:)
    integer :: n, k, total
    total = size_of(all)
    n = 0
    judge_all = .false.
    do k = 1, size(found)
      if (n >= total) exit
      if (all%items(n + 1)%kind /= json_array) return
      if (size_of(all%items(n + 1)) < 1) return
      if (.not. same_span(all%items(n + 1)%items(1), found(k), at)) return
      n = n + 1
    end do
    judge_all = .not. ((total < 20 .and. n /= total) .or. (total == 20 .and. n < 20))
  end function judge_all

  logical function same_split(expected, got)
    type(node_t), pointer, intent(in) :: expected
    type(piece16_t), intent(in) :: got(:)
    integer(int16), allocatable :: u(:)
    integer :: i
    same_split = .false.
    if (.not. associated(expected)) return
    if (expected%kind /= json_array .or. size_of(expected) /= size(got)) return
    do i = 1, size(got)
      if (expected%items(i)%kind == json_string) then
        if (.not. got(i)%defined) return
        u = units(expected%items(i)%s)
        if (size(u) /= size(got(i)%value)) return
        if (any(u /= got(i)%value)) return
      else if (got(i)%defined) then
        return
      end if
    end do
    same_split = .true.
  end function same_split

  logical function same_units(got, expected)
    integer(int16), intent(in) :: got(:)
    character(*), intent(in) :: expected
    integer(int16), allocatable :: u(:)
    u = units(expected)
    same_units = size(u) == size(got)
    if (same_units) same_units = all(u == got)
  end function same_units

  subroutine judge_mode(head, a, text, re, g, y)
    character(*), intent(in) :: head, text
    type(node_t), intent(in), target :: a
    type(regex_t), intent(in) :: re, g, y
    type(node_t), pointer :: exec, all, sticky
    type(captures_t) :: caps
    type(match_t), allocatable :: found(:), groups(:, :)
    type(piece16_t), allocatable :: pieces(:)
    integer(int16), allocatable :: u(:)
    integer(int64), allocatable :: at(:)
    character(:), allocatable :: utf8, expected, message
    logical :: has_lone, matched, ok, have
    integer :: stat, n, k, total, width
    u = units(text)
    has_lone = lone(text)
    if (.not. has_lone) then
      call byte_offsets(text, at)
      utf8 = lossy(text)
    end if
    width = re%group_count() + 1
    exec => member(a, "exec")
    matched = re%exec(u, caps, timeout=limit, stat=stat, errmsg=message)
    if (stat == CRE_MATCH_LIMIT) then
      limited = limited + 1
      call note("LIMIT" // achar(9) // "captures.jsonl" // achar(9) // head // achar(9) // escape(text))
      return
    else if (stat /= CRE_OK) then
      errors = errors + 1
      call note("ERROR" // achar(9) // "captures.jsonl" // achar(9) // head // achar(9) // escape(text) // achar(9) // &
                message)
      return
    end if
    call mark(same_groups(exec, groups_of(caps, merge(caps%count(), width, matched)), matched), head, text, "exec")
    if (.not. has_lone) then
      matched = re%exec(utf8, caps)
      call mark(same_groups(exec, groups_of(caps, merge(caps%count(), width, matched)), matched, at), head, text, &
                "exec-utf8")
    end if

    all => member(a, "all")
    ok = associated(all)
    if (ok) ok = all%kind == json_array
    if (ok) then
      call g%exec_all(u, groups)
      total = size_of(all)
      n = 0
      do k = 1, size(groups, 2)
        if (n >= total) exit
        if (.not. same_groups(item(all, n + 1), groups(:, k), .true.)) then
          ok = .false.
          exit
        end if
        n = n + 1
      end do
      if ((total < 20 .and. n /= total) .or. (total == 20 .and. n < 20)) ok = .false.
      call g%find_all(u, found)
      ok = ok .and. judge_all(found, all)
    end if
    call mark(ok, head, text, "all")
    if (.not. has_lone) then
      ok = associated(all)
      if (ok) ok = all%kind == json_array
      if (ok) then
        call g%find_all(utf8, found)
        ok = judge_all(found, all, at)
      end if
      call mark(ok, head, text, "all-utf8")
    end if

    if (len(text) > 0) then
      sticky => member(a, "sticky")
      ! At UTF-16 index 1; over UTF-8, where that is a position of the text (not between the halves of a character).
      matched = y%exec(u, caps, start=2_int64)
      call mark(same_groups(sticky, groups_of(caps, merge(caps%count(), width, matched)), matched), head, text, "sticky")
      if (.not. has_lone) then
        ok = at(1) >= len(utf8)
        if (.not. ok) ok = iand(iachar(utf8(at(1) + 1:at(1) + 1)), 192) /= 128
        if (ok) then
          matched = y%exec(utf8, caps, start=at(1) + 1)
          call mark(same_groups(sticky, groups_of(caps, merge(caps%count(), width, matched)), matched, at), head, &
                    text, "sticky-utf8")
        end if
      end if
    end if

    expected = text_of(a, "replace", have)
    call mark(have .and. same_units(re%replace(u, units(replacement)), expected), head, text, "replace")
    if (.not. has_lone) then
      call mark(have .and. re%replace(utf8, replacement) == lossy(expected), head, text, "replace-utf8")
    end if
    expected = text_of(a, "replaceAll", have)
    call mark(have .and. same_units(g%replace(u, units(replacement)), expected), head, text, "replaceAll")
    if (.not. has_lone) then
      call mark(have .and. g%replace(utf8, replacement) == lossy(expected), head, text, "replaceAll-utf8")
    end if

    call re%split(u, pieces)
    call mark(same_split(member(a, "split"), pieces), head, text, "split")
    call re%split(u, pieces, limit=3)
    call mark(same_split(member(a, "split3"), pieces), head, text, "split3")
  end subroutine judge_mode

  subroutine judge_captures(r, pattern)
    type(node_t), intent(in), target :: r
    character(*), intent(in) :: pattern
    character(:), allocatable :: flags, mode_flags, head, head_pattern
    type(node_t), pointer :: tests, test, a, text
    type(regex_t) :: re, g, y
    logical :: found, ok
    integer :: i, m, s1, s2, s3
    character(7) :: modes(2)
    flags = text_of(r, "flags", found)
    head_pattern = shown(pattern)
    modes = [character(7) :: "unicode", "legacy"]
    tests => member(r, "tests")
    if (.not. associated(tests)) return
    do m = 1, 2
      if (.not. flag_of(r, "valid." // trim(modes(m)))) cycle
      mode_flags = flags // merge("u", " ", m == 1)
      mode_flags = trim(mode_flags)
      call re%compile(lossy(pattern), mode_flags, stat=s1)
      call g%compile(lossy(pattern), mode_flags // "g", stat=s2)
      call y%compile(lossy(pattern), mode_flags // "y", stat=s3)
      ok = s1 == CRE_OK .and. s2 == CRE_OK .and. s3 == CRE_OK
      head = "/" // head_pattern // "/" // mode_flags
      do i = 1, size_of(tests)
        test => item(tests, i)
        if (flag_of(test, "v8_deviation")) cycle
        text => member(test, "text")
        a => member(test, trim(modes(m)))
        ! A mode whose answer is not an object (null where V8 gave none) is not judged.
        if (.not. associated(text) .or. .not. associated(a)) cycle
        if (text%kind /= json_string .or. a%kind /= json_object) cycle
        if (.not. ok) then
          refused = refused + 1
          call note("REFUSED" // achar(9) // "captures.jsonl" // achar(9) // head // achar(9) // escape(text%s))
          cycle
        end if
        call judge_mode(head, a, text%s, re, g, y)
      end do
    end do
  end subroutine judge_captures

  ! Sorts lines by their bytes (a merge sort).
  recursive subroutine sort_lines(lines)
    type(line_t), intent(inout) :: lines(:)
    type(line_t), allocatable :: left(:), right_half(:)
    integer :: n, mid, i, j, k
    n = size(lines)
    if (n < 2) return
    mid = n / 2
    left = lines(1:mid)
    right_half = lines(mid + 1:n)
    call sort_lines(left)
    call sort_lines(right_half)
    i = 1
    j = 1
    do k = 1, n
      if (j > size(right_half)) then
        lines(k) = left(i)
        i = i + 1
      else if (i > size(left)) then
        lines(k) = right_half(j)
        j = j + 1
      else if (llt(right_half(j)%s, left(i)%s)) then
        lines(k) = right_half(j)
        j = j + 1
      else
        lines(k) = left(i)
        i = i + 1
      end if
    end do
  end subroutine sort_lines
end program dataset
