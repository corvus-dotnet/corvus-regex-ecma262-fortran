! The Industry benchmark from Fortran: each workload counted by each engine, in nanoseconds per count, under the
! interleaved protocol every Corvus ECMA-262 engine's harness follows (docs/benchmarks/README.md in
! corvus-regex-ecma262-rs), a port of the C harness's driver (tools/c/industry.c there):
!
!   industry <workloads.json> [--engines corvus-utf8,corvus-utf16,posix,fortran-regex,v8] [--v8 <industry.mjs>]
!            [--rounds 3] [--seconds 0.7] [--passes 7] [--spread 0.05] [--filter <substring of the id>] [--out <json>]
!
! The workloads are the outer loop, each workload's rounds next and the engines innermost, their order turned by one
! each round, so that a workload's rounds are seconds apart, as its engines are. Each figure is a child process of its
! own (`industry --one`, or `node industry.mjs --one` for V8), which warms for two fifths of its seconds, then times
! `passes` passes of as many counts as fit the rest and reports their median. A workload's figure for an engine is the
! median of its rounds, with the fastest and the slowest round as its spread; a figure whose spread, (slowest - fastest)
! / median, is over --spread is listed in the run's facts (spread_over) and on stderr. A figure that does not finish in
! 30 s is stopped and recorded as giving no result within 30 s, and is not run in the workload's later rounds.
!
! The engines, each counting every match of a global search as a Fortran program would:
! - corvus-utf8 and corvus-utf16: this binding with the workload's flags and u, over the text as a utf8_text_t (UTF-8,
!   checked once when it is made, as a Fortran program that searches a text more than once makes it) and as an
!   integer(int16) array (UTF-16), with regex_t%count;
! - posix: the C library's POSIX regex (regcomp and regexec, extended syntax) through ISO_C_BINDING, which does not
!   claim ECMA-262, given the pattern as it is with REG_ICASE for i and REG_NEWLINE for m, a search going on after
!   each match (one byte on after an empty one);
! - fortran-regex: the fortran-regex package (a port of tiny-regex-c), which does not claim ECMA-262, the same way;
! - v8: Node's RegExp (--v8), with gu added.
!
! The texts are those of the reference run, in CORVUS_REGEX_TEXTS (or ~/.local/share/corvus-regex-benchmarks); the run
! unzips each once into a directory of its own (CORVUS_REGEX_PLAIN), which the children read. POSIX only: the benchmark
! runs on Linux, in the image.
program industry
  use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_loc, c_null_char, c_size_t
  use, intrinsic :: iso_fortran_env, only: int16, int32, int64, real64, error_unit, output_unit
  use jsonlite, only: node_t, parse, member, item, size_of, json_number, json_string
  use corvus_regex_ecma262
  use regex_module, only: fregex => regex, regex_pattern, parse_pattern
  implicit none

  ! glibc's regex flags.
  integer(c_int), parameter :: reg_extended = 1, reg_icase = 2, reg_newline = 4, reg_notbol = 1
  ! The release of fortran-regex fpm.toml pins.
  character(*), parameter :: fortran_regex_version = "1.1.2"
  ! A figure stopped at the limit, which the page shows as giving no result within 30 s.
  character(*), parameter :: stopped_message = "did not produce a result within 30 s"
  character(*), parameter :: stopped = '{"error":"' // stopped_message // '","limit":30}'

  type, bind(c) :: regmatch_t
    integer(c_int) :: rm_so, rm_eo
  end type regmatch_t

  interface
    function regcomp(preg, pattern, cflags) bind(c, name="regcomp") result(r)
      import :: c_ptr, c_char, c_int
      type(c_ptr), value :: preg
      character(kind=c_char), intent(in) :: pattern(*)
      integer(c_int), value :: cflags
      integer(c_int) :: r
    end function regcomp

    function regexec(preg, string, nmatch, pmatch, eflags) bind(c, name="regexec") result(r)
      import :: c_ptr, c_size_t, c_int, regmatch_t
      type(c_ptr), value :: preg, string
      integer(c_size_t), value :: nmatch
      type(regmatch_t), intent(inout) :: pmatch(*)
      integer(c_int), value :: eflags
      integer(c_int) :: r
    end function regexec
  end interface

  type :: string_t
    character(:), allocatable :: s
  end type string_t

  call main()

contains

  function argument(i) result(value)
    integer, intent(in) :: i
    character(:), allocatable :: value
    integer :: n
    call get_command_argument(i, length=n)
    allocate (character(n) :: value)
    call get_command_argument(i, value)
  end function argument

  function environment(name, default) result(value)
    character(*), intent(in) :: name, default
    character(:), allocatable :: value
    integer :: n, status
    call get_environment_variable(name, length=n, status=status)
    if (status /= 0 .or. n == 0) then
      value = default
      return
    end if
    allocate (character(n) :: value)
    call get_environment_variable(name, value)
  end function environment

  function read_file(path) result(text)
    character(*), intent(in) :: path
    character(:), allocatable :: text
    integer :: u
    integer(int64) :: n
    open (newunit=u, file=path, action="read", access="stream", form="unformatted")
    inquire (unit=u, size=n)
    allocate (character(n) :: text)
    if (n > 0) read (u) text
    close (u)
  end function read_file

  function plain_name(file) result(name)
    character(*), intent(in) :: file
    character(:), allocatable :: name
    name = file
    if (len(name) > 3) then
      if (name(len(name) - 2:) == ".gz") name = name(:len(name) - 3)
    end if
  end function plain_name

  ! The text of a workload's file, from the unzipped copy, without a byte order mark.
  function load_text(file) result(text)
    character(*), intent(in) :: file
    character(:), allocatable :: text
    text = read_file(environment("CORVUS_REGEX_PLAIN", ".") // "/" // plain_name(file))
    if (len(text) >= 3) then
      if (text(1:3) == achar(239) // achar(187) // achar(191)) text = text(4:)
    end if
  end function load_text

  ! The UTF-16 code units of UTF-8 text.
  function utf16_of(s) result(u)
    character(*), intent(in) :: s
    integer(int16), allocatable :: u(:)
    integer(int64) :: i, n, k
    integer :: b, c
    n = len(s, int64)
    allocate (u(n))
    i = 1
    k = 0
    do while (i <= n)
      b = iachar(s(i:i))
      if (b < 128) then
        c = b
        i = i + 1
      else if (b < 224) then
        c = ior(ishft(iand(b, 31), 6), iand(iachar(s(i + 1:i + 1)), 63))
        i = i + 2
      else if (b < 240) then
        c = ior(ior(ishft(iand(b, 15), 12), ishft(iand(iachar(s(i + 1:i + 1)), 63), 6)), iand(iachar(s(i + 2:i + 2)), 63))
        i = i + 3
      else
        c = ior(ior(ior(ishft(iand(b, 7), 18), ishft(iand(iachar(s(i + 1:i + 1)), 63), 12)), &
                    ishft(iand(iachar(s(i + 2:i + 2)), 63), 6)), iand(iachar(s(i + 3:i + 3)), 63))
        i = i + 4
      end if
      if (c > 65535) then
        u(k + 1) = int(int(z'D800') + ishft(c - 65536, -10) - 65536, int16)
        u(k + 2) = int(int(z'DC00') + iand(c - 65536, 1023) - 65536, int16)
        k = k + 2
      else
        k = k + 1
        u(k) = int(merge(c - 65536, c, c > 32767), int16)
      end if
    end do
    u = u(1:k)
  end function utf16_of

  function now() result(t)
    integer(int64) :: t, rate
    call system_clock(t, rate)
    t = int(real(t, real64) * (1.0e9_real64 / real(rate, real64)), int64)
  end function now

  ! The counts of one engine, by the engine's name: the count, or -1 with a message for a pattern it refuses.
  subroutine count_of(engine, re, text, checked, units, preg, buffer, fre, n, message)
    character(*), intent(in) :: engine
    type(regex_t), intent(in) :: re
    character(*), intent(in) :: text
    type(utf8_text_t), intent(in) :: checked
    ! Contiguous, as regex_t%count takes it: otherwise each count would copy the text first.
    integer(int16), intent(in), contiguous :: units(:)
    type(c_ptr), intent(in) :: preg
    character(kind=c_char), intent(in), target :: buffer(:)
    type(regex_pattern), intent(in) :: fre
    integer(int64), intent(out) :: n
    character(:), allocatable, intent(inout) :: message
    type(regmatch_t) :: m(1)
    integer(int64) :: p, total
    integer :: idx, length
    n = 0
    select case (engine)
    case ("corvus-utf8")
      n = re%count(checked)
    case ("corvus-utf16")
      n = re%count(units)
    case ("posix")
      p = 0
      total = size(buffer, kind=int64) - 1
      do while (p <= total)
        if (regexec(preg, c_loc(buffer(p + 1)), 1_c_size_t, m, merge(reg_notbol, 0_c_int, p > 0)) /= 0) exit
        n = n + 1
        if (m(1)%rm_eo == m(1)%rm_so) then
          p = p + m(1)%rm_eo + 1
        else
          p = p + m(1)%rm_eo
        end if
      end do
    case ("fortran-regex")
      p = 1
      do while (p <= len(text, int64))
        idx = fregex(text(p:), fre, length)
        if (idx < 0) then
          message = "fortran-regex refuses the pattern"
          n = -1
          return
        end if
        if (idx == 0) exit
        n = n + 1
        p = p + idx - 1 + max(length, 1)
      end do
    end select
  end subroutine count_of

  function has(flags, c) result(yes)
    character(*), intent(in) :: flags, c
    logical :: yes
    yes = index(flags, c) > 0
  end function has

  ! One workload on one engine in this process: a line of JSON with its figures, or with the error.
  subroutine one(path, index_, engine, seconds, passes)
    character(*), intent(in) :: path, engine
    integer, intent(in) :: index_, passes
    real(real64), intent(in) :: seconds
    type(node_t), target :: workloads
    type(node_t), pointer :: w
    character(:), allocatable :: pattern, flags, file, text, message, f
    integer(int16), allocatable :: units(:)
    type(regex_t) :: re
    type(utf8_text_t) :: checked
    type(regex_pattern) :: fre
    character(kind=c_char), allocatable, target :: buffer(:), preg(:)
    integer(c_int) :: cflags
    integer(int64) :: expected, got, reps, per_pass, t0, warm_end, i
    real(real64), allocatable :: times(:)
    integer :: stat, k
    logical :: found
    call parse(read_file(path), workloads, found)
    if (index_ >= size_of(workloads)) then
      print "(a, i0, a)", '{"error":"no workload ', index_, '"}'
      return
    end if
    w => item(workloads, index_ + 1)
    pattern = string_of(w, "pattern")
    flags = string_of(w, "flags")
    file = string_of(w, "file")
    text = load_text(file)
    allocate (units(0))
    allocate (buffer(1), preg(1))
    message = ""
    select case (engine)
    case ("corvus-utf8", "corvus-utf16")
      f = flags
      if (.not. has(f, "u") .and. .not. has(f, "v")) f = f // "u"
      call re%compile(pattern, f, stat=stat, errmsg=message)
      if (stat /= CRE_OK) then
        print "(a)", '{"error":"' // json_escape(message) // '"}'
        return
      end if
      if (engine == "corvus-utf8") checked = utf8_text(text)
      if (engine == "corvus-utf16") units = utf16_of(text)
    case ("posix")
      cflags = reg_extended
      if (has(flags, "i")) cflags = ior(cflags, reg_icase)
      if (has(flags, "m")) cflags = ior(cflags, reg_newline)
      deallocate (preg)
      allocate (preg(1024))
      if (regcomp(c_loc(preg), pattern // c_null_char, cflags) /= 0) then
        print "(a)", '{"error":"regcomp refuses the pattern"}'
        return
      end if
      deallocate (buffer)
      allocate (buffer(len(text) + 1))
      do i = 1, len(text, int64)
        buffer(i) = text(i:i)
      end do
      buffer(len(text) + 1) = c_null_char
    case ("fortran-regex")
      fre = parse_pattern(pattern)
    case default
      print "(a)", '{"error":"unknown engine ' // engine // '"}'
      return
    end select
    call count_of(engine, re, text, checked, units, c_loc(preg), buffer, fre, expected, message)
    if (expected < 0) then
      print "(a)", '{"error":"' // json_escape(message) // '"}'
      return
    end if
    warm_end = now() + int(seconds * 0.4e9_real64, int64)
    reps = 0
    do while (now() < warm_end)
      call count_of(engine, re, text, checked, units, c_loc(preg), buffer, fre, got, message)
      if (got /= expected) then
        print "(a)", '{"error":"the count changed"}'
        return
      end if
      reps = reps + 1
    end do
    per_pass = max(1_int64, nint(real(reps, real64) * 0.6_real64 / passes, int64))
    allocate (times(passes))
    do k = 1, passes
      t0 = now()
      do i = 1, per_pass
        call count_of(engine, re, text, checked, units, c_loc(preg), buffer, fre, got, message)
      end do
      times(k) = real(now() - t0, real64) / real(per_pass, real64)
    end do
    call sort_reals(times)
    print "(a, f0.1, a, f0.1, a, f0.1, a, i0, a, i0, a)", '{"median":', times(passes / 2 + 1), ',"min":', times(1), &
      ',"max":', times(passes), ',"count":', expected, ',"reps":', per_pass * passes, '}'
  end subroutine one

  subroutine sort_reals(a)
    real(real64), intent(inout) :: a(:)
    integer :: i, j
    real(real64) :: t
    do i = 2, size(a)
      t = a(i)
      j = i - 1
      do while (j >= 1)
        if (a(j) <= t) exit
        a(j + 1) = a(j)
        j = j - 1
      end do
      a(j + 1) = t
    end do
  end subroutine sort_reals

  real(real64) function median(a)
    real(real64), intent(in) :: a(:)
    real(real64), allocatable :: b(:)
    integer :: n
    b = a
    call sort_reals(b)
    n = size(b)
    if (mod(n, 2) == 1) then
      median = b(n / 2 + 1)
    else
      median = (b(n / 2) + b(n / 2 + 1)) / 2
    end if
  end function median

  function json_escape(s) result(out)
    character(*), intent(in) :: s
    character(:), allocatable :: out
    integer :: i, c
    character(6) :: e
    out = ""
    do i = 1, len(s)
      c = iachar(s(i:i))
      if (s(i:i) == '"' .or. s(i:i) == "\") then
        out = out // "\" // s(i:i)
      else if (c < 32) then
        write (e, "('\u', z4.4)") c
        out = out // e
      else
        out = out // s(i:i)
      end if
    end do
  end function json_escape

  function real_text(x) result(s)
    real(real64), intent(in) :: x
    character(:), allocatable :: s
    character(32) :: b
    write (b, "(f0.4)") x
    s = trim(adjustl(b))
    if (s(1:1) == ".") s = "0" // s
  end function real_text

  ! A percentage with one decimal, as the other harnesses write it.
  function percent_text(x) result(s)
    real(real64), intent(in) :: x
    character(:), allocatable :: s
    character(32) :: b
    write (b, "(f0.1)") x
    s = trim(adjustl(b))
    if (s(1:1) == ".") s = "0" // s
    s = s // "%"
  end function percent_text

  function int_text(x) result(s)
    integer(int64), intent(in) :: x
    character(:), allocatable :: s
    character(24) :: b
    write (b, "(i0)") x
    s = trim(b)
  end function int_text

  ! The first line a shell command writes ("" when none).
  function first_line(command) result(line)
    character(*), intent(in) :: command
    character(:), allocatable :: line, out, text
    integer :: k
    out = environment("TMPDIR", "/tmp") // "/industry-fortran-line.txt"
    call execute_command_line("(" // command // ") > " // out // " 2>/dev/null")
    text = read_file(out)
    k = index(text, achar(10))
    line = text
    if (k > 0) line = text(:k - 1)
  end function first_line

  ! A string member of a JSON object ("" when there is none).
  function string_of(n, name) result(s)
    type(node_t), intent(in), target :: n
    character(*), intent(in) :: name
    character(:), allocatable :: s
    type(node_t), pointer :: p
    s = ""
    p => member(n, name)
    if (.not. associated(p)) return
    if (p%kind == json_string) s = p%s
  end function string_of

  ! One figure in a child process: the child's line of JSON, parsed, or one with the error (stopped, at the limit).
  subroutine figure(self_path, v8, path, index_, engine, seconds, passes, line)
    character(*), intent(in) :: self_path, v8, path, engine, seconds, passes
    integer, intent(in) :: index_
    type(node_t), intent(out) :: line
    character(:), allocatable :: out, text, command, last
    character(16) :: k
    integer :: status, nl
    logical :: ok
    write (k, "(i0)") index_
    out = environment("TMPDIR", "/tmp") // "/industry-fortran-figure.txt"
    if (engine == "v8") then
      command = "timeout 30 node '" // v8 // "' --one '" // path // "' " // trim(k) // " " // seconds // " " // passes
    else
      command = "timeout 30 '" // self_path // "' --one '" // path // "' " // trim(k) // " " // engine // " " // &
                seconds // " " // passes
    end if
    call execute_command_line(command // " > " // out // " 2>/dev/null", exitstat=status)
    if (status == 124) then
      call parse(stopped, line, ok)
      return
    end if
    text = read_file(out)
    do while (len(text) > 0)
      if (text(len(text):len(text)) /= achar(10)) exit
      text = text(:len(text) - 1)
    end do
    nl = index(text, achar(10), back=.true.)
    last = text(nl + 1:)
    if (len(last) == 0) then
      call parse('{"error":"the process ended without figures"}', line, ok)
      return
    end if
    call parse(last, line, ok)
    if (.not. ok) call parse('{"error":"the process wrote no figures"}', line, ok)
  end subroutine figure

  integer function to_int(s)
    character(*), intent(in) :: s
    read (s, *) to_int
  end function to_int

  real(real64) function to_real(s)
    character(*), intent(in) :: s
    read (s, *) to_real
  end function to_real

  function option(name, default) result(value)
    character(*), intent(in) :: name, default
    character(:), allocatable :: value
    integer :: i
    value = default
    do i = 2, command_argument_count() - 1
      if (argument(i) == "--" // name) value = argument(i + 1)
    end do
  end function option

  subroutine split_list(s, items)
    character(*), intent(in) :: s
    type(string_t), allocatable, intent(out) :: items(:)
    integer :: a, b
    allocate (items(0))
    a = 1
    do
      b = index(s(a:), ",")
      if (b == 0) then
        items = [items, string_t(s(a:))]
        exit
      end if
      items = [items, string_t(s(a:a + b - 2))]
      a = a + b
    end do
  end subroutine split_list

  subroutine main()
    character(:), allocatable :: path, v8, seconds, passes, filter, out, plain, texts, id, file, wid, e, results, rows
    character(:), allocatable :: over, differ, v8_engine, line_error, self_path, facts, entry, each_text
    type(string_t), allocatable :: engines(:), ids(:)
    type(node_t), target :: workloads, line
    type(node_t), pointer :: w, p
    real(real64), allocatable :: each(:, :, :)
    integer, allocatable :: nfig(:, :)
    integer(int64), allocatable :: counts(:, :)
    ! V8's count of each workload as the workloads carry it (v8Count, from the central run), or -1, for a run without V8.
    integer(int64), allocatable :: v8_counts(:)
    type(string_t), allocatable :: errors(:, :)
    integer :: rounds, nw, ne, k, c, rnd, turn, ei, i, u
    real(real64) :: limit, m, spread, x
    integer(int64) :: n, oracle
    logical :: found, have
    integer, allocatable :: chosen(:)
    if (command_argument_count() >= 1) then
      if (argument(1) == "--one") then
        k = to_int(argument(3))
        x = to_real(argument(5))
        i = to_int(argument(6))
        call one(argument(2), k, argument(4), x, i)
        return
      end if
    end if
    call get_command_argument(0, length=k)
    allocate (character(k) :: self_path)
    call get_command_argument(0, self_path)
    if (index(self_path, "/") == 0) self_path = first_line("command -v " // self_path)
    path = argument(1)
    v8 = option("v8", "")
    seconds = option("seconds", "0.7")
    passes = option("passes", "7")
    filter = option("filter", "")
    out = option("out", "")
    rounds = to_int(option("rounds", "3"))
    limit = to_real(option("spread", "0.05"))
    if (len(option("engines", "")) > 0) then
      call split_list(option("engines", ""), engines)
    else if (len(v8) > 0) then
      call split_list("corvus-utf8,corvus-utf16,posix,fortran-regex,v8", engines)
    else
      call split_list("corvus-utf8,corvus-utf16,posix,fortran-regex", engines)
    end if
    ne = size(engines)
    call parse(read_file(path), workloads, found)
    nw = size_of(workloads)
    allocate (ids(nw), chosen(0), v8_counts(nw))
    ! The texts unzipped once, for every child.
    texts = environment("CORVUS_REGEX_TEXTS", environment("HOME", ".") // "/.local/share/corvus-regex-benchmarks")
    plain = first_line("mktemp -d")
    do k = 1, nw
      w => item(workloads, k)
      id = string_of(w, "id")
      ids(k)%s = id
      file = string_of(w, "file")
      v8_counts(k) = -1
      p => member(w, "v8Count")
      if (associated(p)) then
        if (p%kind == json_number) v8_counts(k) = p%i
      end if
      call execute_command_line("[ -f '" // plain // "/" // plain_name(file) // "' ] || gzip -dc '" // texts // "/" // &
                                file // "' > '" // plain // "/" // plain_name(file) // "'")
      if (len(filter) == 0 .or. index(id, filter) > 0) chosen = [chosen, k]
    end do
    call execute_command_line("true")
    call set_plain(plain)
    allocate (each(rounds, ne, nw), nfig(ne, nw), counts(ne, nw), errors(ne, nw))
    nfig = 0
    counts = -1
    do k = 1, size(errors, 1)
      do i = 1, size(errors, 2)
        errors(k, i)%s = ""
      end do
    end do
    v8_engine = ""
    do c = 1, size(chosen)
      k = chosen(c)
      do rnd = 0, rounds - 1
        do turn = 0, ne - 1
          ei = mod(turn + rnd, ne) + 1
          e = engines(ei)%s
          ! A figure stopped at the limit in an earlier round is not run again.
          if (errors(ei, k)%s == stopped_message) cycle
          call figure(self_path, v8, path, k - 1, e, seconds, passes, line)
          p => member(line, "median")
          have = associated(p)
          if (have) have = p%kind == json_number
          if (have) then
            nfig(ei, k) = nfig(ei, k) + 1
            each(nfig(ei, k), ei, k) = p%x
            p => member(line, "count")
            if (associated(p)) counts(ei, k) = p%i
          else
            line_error = string_of(line, "error")
            if (len(line_error) == 0) line_error = "a round gave no figure"
            errors(ei, k)%s = line_error
            write (error_unit, "(a)") e // " " // ids(k)%s // " " // line_error
          end if
          if (e == "v8") then
            line_error = string_of(line, "engine")
            if (len(line_error) > 0) v8_engine = line_error
          end if
        end do
      end do
      write (error_unit, "(a, i0, a, i0, a)") "workload ", c, " of ", size(chosen), " done"
    end do
    call execute_command_line("rm -rf '" // plain // "'")

    over = ""
    differ = ""
    results = ""
    do ei = 1, ne
      e = engines(ei)%s
      rows = ""
      do c = 1, size(chosen)
        k = chosen(c)
        wid = json_escape(ids(k)%s)
        if (nfig(ei, k) < rounds) then
          if (len(errors(ei, k)%s) == 0) errors(ei, k)%s = "a round gave no figure"
          if (errors(ei, k)%s == stopped_message) then
            entry = stopped
          else
            entry = '{"error":"' // json_escape(errors(ei, k)%s) // '"}'
          end if
        else
          m = median(each(1:rounds, ei, k))
          spread = 0
          if (m > 0) spread = (maxval(each(1:rounds, ei, k)) - minval(each(1:rounds, ei, k))) / m
          if (spread > limit) then
            over = over // merge("; ", "  ", len(over) > 0) // e // " " // ids(k)%s // " " // percent_text(spread * 100)
            write (error_unit, "(a)") "SPREAD " // e // " " // ids(k)%s // " " // percent_text(spread * 100)
          end if
          n = counts(ei, k)
          each_text = ""
          do i = 1, rounds
            each_text = each_text // merge(",", " ", i > 1) // real_text(each(i, ei, k))
          end do
          entry = '{"median":' // real_text(m) // ',"min":' // real_text(minval(each(1:rounds, ei, k))) // &
                  ',"max":' // real_text(maxval(each(1:rounds, ei, k))) // ',"count":' // int_text(n) // &
                  ',"rounds":' // int_text(int(rounds, int64)) // ',"spread":' // real_text(spread) // &
                  ',"each":[' // each_text // ']'
          ! Without V8 in the run (V8=0), its count as the workloads carry it.
          oracle = v8_counts(k)
          do i = 1, ne
            if (engines(i)%s == "v8") oracle = counts(i, k)
          end do
          if (oracle >= 0 .and. n /= oracle .and. e /= "v8") then
            differ = differ // merge("; ", "  ", len(differ) > 0) // e // " " // ids(k)%s // " " // int_text(n) // &
                     " vs V8 " // int_text(oracle)
            write (error_unit, "(a)") "COUNT  " // e // " " // ids(k)%s // " " // int_text(n) // " against V8's " // &
              int_text(oracle)
            if (e == "posix" .or. e == "fortran-regex") then
              entry = entry // ',"differs":"' // e // " " // int_text(n) // ", V8 " // int_text(oracle) // '"}'
            else
              entry = '{"error":"counted ' // int_text(n) // ' where V8 counts ' // int_text(oracle) // '"}'
            end if
          else
            entry = entry // "}"
          end if
        end if
        rows = rows // merge(",", " ", len(rows) > 0) // '"' // wid // '":' // entry
      end do
      results = results // merge(",", " ", len(results) > 0) // '"' // e // '":{' // rows // "}"
    end do
    facts = '"cpu":"' // json_escape(first_line("grep -m1 'model name' /proc/cpuinfo | cut -d: -f2- | sed 's/^ *//'")) // &
      '","cores":"' // first_line("nproc --all") // &
      '","affinity":"' // first_line("grep Cpus_allowed_list /proc/self/status | cut -d: -f2 | tr -d '\t '") // &
      '","os":"' // json_escape(first_line("uname -sr")) // &
      '","load":"' // first_line("cut -d' ' -f1 /proc/loadavg") // &
      '","fortran":"' // json_escape(first_line("gfortran --version | head -1")) // &
      '","commit":"' // json_escape(environment("CORVUS_COMMIT", first_line("git rev-parse HEAD"))) // &
      '","date":"' // first_line("date -u +%Y-%m-%dT%H:%M:%SZ") // &
      '","seconds":"' // seconds // '","passes":"' // passes // &
      '","runner":"' // json_escape(environment("RUNNER_NAME", "")) // &
      '","runner_os":"' // json_escape(environment("RUNNER_OS", "")) // &
      '","image":"' // json_escape(trim(environment("ImageOS", "") // " " // environment("ImageVersion", ""))) // &
      '","corvus-regex-ecma262":"' // library_version() // &
      '","binding":"' // binding_version // &
      '","posix":"' // json_escape(first_line("getconf GNU_LIBC_VERSION | cut -d' ' -f2")) // &
      '","fortran-regex":"' // fortran_regex_version // &
      '","rounds":"' // int_text(int(rounds, int64)) // '","protocol":"interleaved","spread_limit":"' // &
      real_text(limit) // '","spread_over":"' // json_escape(trim(adjustl(over))) // &
      '","counts_differ":"' // json_escape(trim(adjustl(differ))) // '","v8":"' // json_escape(v8_engine) // '"'
    if (len(out) > 0) then
      open (newunit=u, file=out, status="replace", action="write", access="stream", form="unformatted")
      write (u) '{"facts":{' // facts // '},"end":"' // first_line("date -u +%Y-%m-%dT%H:%M:%SZ") // &
        '","results":{' // results // '}}' // achar(10)
      close (u)
    else
      write (output_unit, "(a)") '{"facts":{' // facts // '},"results":{' // results // '}}'
    end if
  end subroutine main

  ! CORVUS_REGEX_PLAIN for the children: set through the environment of the commands they are run with.
  subroutine set_plain(plain)
    character(*), intent(in) :: plain
    interface
      function setenv(name, value, overwrite) bind(c, name="setenv") result(r)
        import :: c_char, c_int
        character(kind=c_char), intent(in) :: name(*), value(*)
        integer(c_int), value :: overwrite
        integer(c_int) :: r
      end function setenv
    end interface
    integer(c_int) :: r
    r = setenv("CORVUS_REGEX_PLAIN" // c_null_char, plain // c_null_char, 1_c_int)
  end subroutine set_plain
end program industry
