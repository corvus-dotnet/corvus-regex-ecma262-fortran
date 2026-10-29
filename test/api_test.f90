! The binding's API against the answers V8 gives: compiling, testing, finding, captures by number and by name, the
! global search, counting, replacing and splitting, over UTF-8 (as characters and as a utf8_text_t checked once) and
! UTF-16, the half characters of a match without u
! or v, and the failures reported through stat; and classes nested as deep as allowed, on a small stack.
program api_test
  use, intrinsic :: iso_fortran_env, only: int16, int64, real64, error_unit
  use corvus_regex_ecma262
  implicit none
  integer :: failures = 0
  character(16) :: role

  ! The test run again as a child process, on a small stack (see deep_classes).
  call get_command_argument(1, role)
  if (role == "deep-classes") then
    call deep_classes_here()
    if (failures > 0) error stop 1
    stop
  end if

  call compiling()
  call testing()
  call finding()
  call capturing()
  call searching()
  call replacing()
  call splitting()
  call checked_texts()
  call utf16()
  call half_characters()
  call limits()
  call timeouts()
  call copying()
  call editions()
  call readme()
  call deep_classes()

  if (failures > 0) then
    write (error_unit, "(i0, a)") failures, " checks failed"
    error stop 1
  end if
  print "(a)", "all API checks passed"

contains

  subroutine check(ok, what)
    logical, intent(in) :: ok
    character(*), intent(in) :: what
    if (.not. ok) then
      failures = failures + 1
      write (error_unit, "(a)") "FAILED: " // what
    end if
  end subroutine check

  ! The UTF-16 code units of ASCII text.
  function u16(s) result(units)
    character(*), intent(in) :: s
    integer(int16), allocatable :: units(:)
    integer :: i
    allocate (units(len(s)))
    do i = 1, len(s)
      units(i) = int(iachar(s(i:i)), int16)
    end do
  end function u16

  subroutine compiling()
    type(regex_t) :: re
    integer :: stat
    character(:), allocatable :: msg
    call check(.not. re%is_compiled(), "not compiled at first")
    call re%compile("(?<year>\d{4})-(?<month>\d{2})(-(\d{2}))?", "gu")
    call check(re%is_compiled(), "compiled")
    call check(re%source() == "(?<year>\d{4})-(?<month>\d{2})(-(\d{2}))?", "source")
    call check(re%flags() == "gu", "flags")
    call check(re%group_count() == 4, "group count")
    call check(re%group_name_count() == 2, "name count")
    call check(re%group_name(1) == "year" .and. re%group_name(2) == "month" .and. re%group_name(3) == "", "names")
    call re%compile("a", "yig")
    call check(re%flags() == "giy", "flags in order")
    call check(is_valid("a+", "u") .and. .not. is_valid("a{2,1}", "u") .and. is_valid("a{2,1"), "is_valid")
    call re%compile("(", stat=stat, errmsg=msg)
    call check(stat == CRE_INVALID_PATTERN .and. len(msg) > 0 .and. .not. re%is_compiled(), "invalid pattern")
    call re%compile("\p{Nope}", "u", stat=stat)
    call check(stat == CRE_INVALID_PATTERN, "invalid property")
    call re%compile("a", "uv", stat=stat)
    call check(stat == CRE_INVALID_FLAGS, "invalid flags")
    call re%compile(repeat("(", 1001) // repeat(")", 1001), stat=stat)
    call check(stat == CRE_PATTERN_TOO_LARGE, "too large")
    call check(len(library_version()) >= 5, "library version")
    call check(binding_version == "0.2.5", "binding version")
  end subroutine compiling

  subroutine testing()
    type(regex_t) :: re, sticky
    integer :: stat
    call re%compile("o+")
    call check(re%test("foo") .and. .not. re%test("bar"), "test")
    call sticky%compile("o", "y")
    call check(.not. sticky%test("foo") .and. sticky%test("foo", start=2_int64), "sticky")
    call check(.not. re%test("fo" // char(195), stat=stat) .and. stat == CRE_INVALID_UTF8, "invalid UTF-8")
    call check(last_error_offset() == 2, "error offset")
  end subroutine testing

  subroutine finding()
    type(regex_t) :: re
    type(match_t) :: m
    character(*), parameter :: text = "a foo, a boo"
    call re%compile("o+")
    call check(re%find(text, m), "found")
    call check(m%matched .and. m%start == 4 .and. m%end == 5 .and. text(m%start:m%end) == "oo", "where")
    call check(re%find(text, m, start=6_int64) .and. m%start == 11, "from start")
    call check(.not. re%find("a", m) .and. .not. m%matched, "not found")
    call re%compile("(?=b)")
    call check(re%find("ab", m) .and. m%start == 2 .and. m%end == 1, "empty match ends before it starts")
  end subroutine finding

  subroutine capturing()
    type(regex_t) :: re
    type(captures_t) :: caps
    type(match_t) :: m
    character(*), parameter :: text = "on 2027-01-31"
    call re%compile("(?<year>\d{4})-(?<month>\d{2})(-(\d{2}))?", "u")
    call check(re%exec(text, caps), "exec")
    call check(caps%count() == 5, "count")
    m = caps%group(0)
    call check(text(m%start:m%end) == "2027-01-31", "group 0")
    m = caps%named("month")
    call check(text(m%start:m%end) == "01", "named")
    m = caps%group(4)
    call check(text(m%start:m%end) == "31", "group 4")
    m = caps%group(9)
    call check(.not. m%matched, "no group 9")
    m = caps%named("nope")
    call check(.not. m%matched, "no group named nope")
    call check(re%exec("due 2026-10", caps), "exec again")
    m = caps%group(3)
    call check(.not. m%matched, "a group that did not take part")
    call check(.not. re%exec("none", caps) .and. caps%count() == 0, "no match")
    call re%compile("(?<d>a)|(?<d>b)", "u")
    call check(re%exec("b", caps), "exec with a shared name")
    m = caps%named("d")
    call check(m%start == 1, "a shared name")
  end subroutine capturing

  subroutine searching()
    type(regex_t) :: re
    type(match_t), allocatable :: all(:), groups(:, :)
    call re%compile("\d+", "g")
    call re%find_all("1 22 333", all)
    call check(size(all) == 3, "three matches")
    call check(all(2)%start == 3 .and. all(2)%end == 4 .and. all(3)%start == 6 .and. all(3)%end == 8, "where they are")
    call re%find_all("", all)
    call check(size(all) == 0, "none")
    call check(re%count(repeat("1 ", 1000)) == 1000, "count")
    call re%compile("a", "y")
    call check(re%count("aab") == 2 .and. re%count("baa") == 0, "sticky count")
    call re%compile("(\d)(\d)?")
    call re%exec_all("x1y22z3", groups)
    call check(size(groups, 1) == 3 .and. size(groups, 2) == 3, "groups shape")
    call check(.not. groups(2, 1)%matched .and. groups(2, 2)%start == 5 .and. groups(0, 3)%start == 7, "groups")
    call re%compile("", "gu")
    call check(re%count("😀") == 2, "empty matches by code point")
    call re%compile("", "g")
    call check(re%count("😀") == 3, "empty matches by code unit")
  end subroutine searching

  subroutine replacing()
    type(regex_t) :: date, first
    call date%compile("(?<y>\d{4})-(?<m>\d{2})", "g")
    call check(date%replace("2026-10, 2027-01", "$<m>/$1") == "10/2026, 01/2027", "template")
    call first%compile("\d+")
    call check(first%replace("1 2 3", "[$&]") == "[1] 2 3", "first")
    call check(first%replace_all("1 2 3", "[$&]") == "[1] [2] [3]", "all")
    call check(first%replace("a1b", "<$`|$'|$$>") == "a<a|b|$>b", "before, after, dollar")
    call check(first%replace("none", "x") == "none", "unchanged")
  end subroutine replacing

  subroutine splitting()
    type(regex_t) :: re
    type(piece_t), allocatable :: pieces(:)
    call re%compile("\s*(;)\s*")
    call re%split("a ; b;c", pieces)
    call check(size(pieces) == 5, "five pieces")
    call check(pieces(1)%value == "a" .and. pieces(2)%value == ";" .and. pieces(5)%value == "c", "pieces")
    call re%split("a;b;c", pieces, limit=2)
    call check(size(pieces) == 2, "limit")
    call re%compile("(x)?,")
    call re%split("a,b", pieces)
    call check(size(pieces) == 3 .and. .not. pieces(2)%defined .and. pieces(3)%value == "b", "undefined piece")
    call re%split("", pieces)
    call check(size(pieces) == 1 .and. pieces(1)%defined .and. len(pieces(1)%value) == 0, "empty text")
  end subroutine splitting

  ! Each method over a utf8_text_t finds what it finds over the same characters, and utf8_text refuses text that is not
  ! UTF-8, where it stops being UTF-8.
  subroutine checked_texts()
    type(regex_t) :: re, digits
    type(utf8_text_t) :: t, never
    type(match_t) :: m, m2
    type(match_t), allocatable :: all(:), all2(:), groups(:, :), groups2(:, :)
    type(captures_t) :: caps, caps2
    type(piece_t), allocatable :: pieces(:), pieces2(:)
    character(*), parameter :: text = "on 2026-10 and 2027-01, 😀 ok"
    character(:), allocatable :: message
    integer :: stat
    t = utf8_text(text)
    call re%compile("(?<y>\d{4})-(?<m>\d{2})", "gu")
    call check(re%test(t) .eqv. re%test(text), "test")
    call check(re%test(t, start=10_int64) .eqv. re%test(text, start=10_int64), "test from start")
    call check(re%test(t, timeout=1.0_real64) .eqv. re%test(text, timeout=1.0_real64), "test with a timeout")
    call check(re%find(t, m, start=5_int64) .and. re%find(text, m2, start=5_int64), "find")
    call check(m%start == m2%start .and. m%end == m2%end, "find where")
    call check(re%exec(t, caps) .and. re%exec(text, caps2), "exec")
    m = caps%named("m")
    m2 = caps2%named("m")
    call check(m%start == m2%start .and. m%end == m2%end, "exec named")
    call re%find_all(t, all)
    call re%find_all(text, all2)
    call check(size(all) == 2 .and. size(all2) == 2 .and. all(2)%start == all2(2)%start, "find_all")
    call re%exec_all(t, groups)
    call re%exec_all(text, groups2)
    call check(size(groups, 1) == size(groups2, 1) .and. size(groups, 2) == size(groups2, 2) .and. &
               groups(2, 2)%start == groups2(2, 2)%start, "exec_all")
    call check(re%count(t) == re%count(text) .and. re%count(t) == 2, "count")
    call check(re%replace(t, "$<m>/$1") == re%replace(text, "$<m>/$1"), "replace")
    call check(re%replace_all(t, "[$&]") == re%replace_all(text, "[$&]"), "replace_all")
    call digits%compile("\s*,\s*")
    call digits%split(t, pieces)
    call digits%split(text, pieces2)
    call check(size(pieces) == 2 .and. size(pieces2) == 2 .and. pieces(2)%value == pieces2(2)%value, "split")
    ! Text that is not UTF-8 is refused where it stops being UTF-8, and the result holds an empty text.
    t = utf8_text("ab" // char(226) // char(130) // "c", stat=stat, errmsg=message)
    call check(stat == CRE_INVALID_UTF8 .and. last_error_offset() == 2 .and. len(message) > 0, "not UTF-8")
    call check(re%count(t) == 0, "the refused text is empty")
    ! One never assigned from utf8_text is searched as an empty text.
    call check(.not. re%test(never) .and. re%count(never) == 0, "never assigned")
  end subroutine checked_texts

  subroutine utf16()
    type(regex_t) :: re
    type(match_t) :: m
    type(match_t), allocatable :: found(:)
    type(piece16_t), allocatable :: pieces(:)
    integer(int16), parameter :: lone(3) = [int(z'61', int16), int(z'D800', int16), int(z'62', int16)]
    integer(int16), parameter :: emoji(2) = [int(z'D83D', int16), int(z'DE00', int16)]
    call re%compile("o+")
    call check(re%find(u16("a foo"), m) .and. m%start == 4 .and. m%end == 5, "utf16 find")
    call re%compile("^...$")
    call check(re%test(lone), "a lone surrogate is a unit")
    call re%compile("\ud800")
    call check(re%find(lone, m) .and. m%start == 2 .and. m%end == 2, "a lone surrogate matched")
    call re%compile("^..$")
    call check(re%test(emoji), "a pair is two units without u")
    call re%compile("^.$", "u")
    call check(re%test(emoji), "a pair is one character with u")
    call re%compile("\d+", "g")
    call re%find_all(u16("x1y22z333"), found)
    call check(size(found) == 3 .and. found(3)%start == 7 .and. found(3)%end == 9, "utf16 find_all")
    call check(re%count(u16(repeat("1 ", 100))) == 100, "utf16 count")
    call check(all(re%replace(u16("a1b"), u16("<$&>")) == u16("a<1>b")), "utf16 replace")
    call re%compile(",")
    call re%split(u16("a,b"), pieces)
    call check(size(pieces) == 2 .and. all(pieces(2)%value == u16("b")), "utf16 split")
  end subroutine utf16

  subroutine half_characters()
    type(regex_t) :: re
    type(match_t) :: m
    ! Without u a character beyond the Basic Multilingual Plane is two code units, and . takes one: over UTF-8 the
    ! match ends at the character's third byte.
    call re%compile("^.")
    call check(re%find("😀x", m) .and. m%start == 1 .and. m%end == 2, "half a character")
    call check(re%replace("😀", "x") == "x" // char(239) // char(191) // char(189), "the other half as U+FFFD")
    call re%compile("^.", "u")
    call check(re%find("😀x", m) .and. m%end == 4, "the whole character with u")
  end subroutine half_characters

  subroutine limits()
    type(regex_t) :: slow
    integer :: stat
    logical :: matched
    call slow%compile("^(a+)+\1$")
    matched = slow%test(repeat("a", 64) // "!", timeout=0.005_real64, stat=stat)
    call check(stat == CRE_MATCH_LIMIT .and. .not. matched, "match limit")
    call slow%compile("^(a+)+$")
    call check(.not. slow%test(repeat("a", 64) // "!", timeout=1.0_real64), "linear without a backreference")
  end subroutine limits

  subroutine timeouts()
    type(regex_t) :: timed, own, none, defaulted, counted
    type(captures_t) :: caps
    type(match_t) :: m
    type(match_t), allocatable :: all(:)
    type(piece_t), allocatable :: pieces(:)
    character(:), allocatable :: long, out
    integer :: stat
    integer(int64) :: n
    logical :: matched
    long = repeat("a", 60) // "b"
    call timed%compile("^(?:(a+)\1*)+$", "u", timeout=0.03_real64)
    call check(abs(timed%match_timeout() - 0.03_real64) < 1e-9_real64, "a pattern's timeout")
    ! Every method that matches gives its match the pattern's timeout.
    matched = timed%test(long, stat=stat)
    call check(stat == CRE_MATCH_LIMIT, "the timeout, test")
    matched = timed%find(long, m, stat=stat)
    call check(stat == CRE_MATCH_LIMIT, "the timeout, find")
    matched = timed%exec(long, caps, stat=stat)
    call check(stat == CRE_MATCH_LIMIT, "the timeout, exec")
    call timed%find_all(long, all, stat=stat)
    call check(stat == CRE_MATCH_LIMIT, "the timeout, find_all")
    n = timed%count(long, stat=stat)
    call check(stat == CRE_MATCH_LIMIT, "the timeout, count")
    out = timed%replace(long, "x", stat=stat)
    call check(stat == CRE_MATCH_LIMIT, "the timeout, replace")
    out = timed%replace_all(long, "x", stat=stat)
    call check(stat == CRE_MATCH_LIMIT, "the timeout, replace_all")
    call timed%split(long, pieces, stat=stat)
    call check(stat == CRE_MATCH_LIMIT, "the timeout, split")
    matched = timed%test(utf8_text(long), stat=stat)
    call check(stat == CRE_MATCH_LIMIT, "the timeout, test of a checked text")
    call check(timed%test("aaaa"), "answered within the timeout")
    call check(timed%replace("aaaa", "<$&>") == "<aaaa>", "replace within the timeout")
    ! A timeout caps the backtracking memory too.
    call counted%compile("(?:a?){2147483647}", timeout=3600.0_real64)
    matched = counted%test("", stat=stat)
    call check(stat == CRE_MATCH_LIMIT, "the memory cap")
    ! The default: none unless set, taken by a pattern compiled without one, and kept by one compiled before.
    call check(abs(default_match_timeout() - 0.0_real64) < 1e-9_real64, "no default timeout")
    call set_default_match_timeout(0.03_real64)
    call check(abs(default_match_timeout() - 0.03_real64) < 1e-9_real64, "the default timeout")
    call defaulted%compile("^(?:(a+)\1*)+$", "u")
    call own%compile("a", timeout=5.0_real64)
    call none%compile("a", timeout=0.0_real64)
    call set_default_match_timeout(0.0_real64)
    call check(abs(default_match_timeout() - 0.0_real64) < 1e-9_real64, "the default timeout, reset")
    call check(abs(defaulted%match_timeout() - 0.03_real64) < 1e-9_real64, "a pattern takes the default")
    matched = defaulted%test(long, stat=stat)
    call check(stat == CRE_MATCH_LIMIT, "the default timeout, test")
    call check(abs(own%match_timeout() - 5.0_real64) < 1e-9_real64, "a pattern keeps its own")
    call check(abs(none%match_timeout() - 0.0_real64) < 1e-9_real64, "a pattern with none")
  end subroutine timeouts

  subroutine copying()
    type(regex_t) :: a, b
    call a%compile("x+", "gi")
    b = a
    call check(b%is_compiled() .and. b%source() == "x+" .and. b%flags() == "gi", "assignment compiles again")
    call check(b%count("xXx x") == 2, "the copy matches")
    ! A copy keeps the pattern's timeout, edition and version of Unicode.
    call a%compile("a", "u", timeout=5.0_real64, edition=CRE_EDITION_ES2020, unicode_version=CRE_UNICODE_13)
    b = a
    call check(abs(b%match_timeout() - 5.0_real64) < 1e-9_real64, "a copy keeps the timeout")
    call check(b%edition() == CRE_EDITION_ES2020 .and. b%unicode_version() == 13, "a copy keeps the settings")
    b = b
    call check(b%is_compiled() .and. b%unicode_version() == 13, "a copy assigned to itself")
  end subroutine copying

  ! The edition and the version of Unicode: each independent of the other, and resolved when the pattern is compiled.
  subroutine editions()
    character(*), parameter :: l10570 = char(240) // char(144) // char(149) // char(176)
    character(*), parameter :: l10597 = char(240) // char(144) // char(150) // char(151)
    character(*), parameter :: a7c0 = char(234) // char(159) // char(128), a7c1 = char(234) // char(159) // char(129)
    integer(int16), parameter :: u10570(2) = [int(z'D801', int16), int(z'DD70', int16)]
    integer(int16), parameter :: u10597(2) = [int(z'D801', int16), int(z'DD97', int16)]
    integer(int16), parameter :: ua7c0(1) = [int(z'A7C0', int16)], ua7c1(1) = [int(z'A7C1', int16)]
    character(*), parameter :: refused(8) = [character(32) :: "(?i:a)", "(?-i:a)", "(?s:.)", "(?<a>x)|(?<a>y)", &
      "(?<a>x)|(?<a>y)", "\p{Script=Yezidi}", "\p{sc=Vith}", "\p{Script_Extensions=Toto}"]
    character(*), parameter :: refused_flags(8) = [character(2) :: "", "i", "u", "", "u", "u", "u", "u"]
    character(*), parameter :: accepted(7) = [character(32) :: "\p{Script=Greek}", "\p{L}\p{gc=Lu}\p{Lowercase}", &
      "\P{scx=Latn}", "\p{Script=Yezidi}", "(?<a>x)(?<b>y)\k<a>", "(?<=a)(?<!b)", "(?:i)"]
    character(*), parameter :: accepted_flags(7) = [character(2) :: "u", "u", "iu", "", "u", "s", ""]
    character(*), parameter :: backrefs(3) = [character(16) :: "(.)\1", "(?<=\1(.))", "(?:x|(.))+\1$"]
    character(*), parameter :: two_flags(4) = [character(2) :: "d", "v", "dv", "gv"]
    type(regex_t) :: re
    integer :: stat, i, k, v
    character(:), allocatable :: text, twice, flags
    integer(int16), allocatable :: text16(:)
    call re%compile("a")
    call check(re%edition() == CRE_EDITION_CURRENT .and. re%unicode_version() == 17, "the defaults")
    call check(default_unicode_version == 17 .and. latest_unicode_version == 18, "the default and latest versions")
    call check(all(unicode_versions == [13, 17, 18]), "the versions")
    call re%compile("a", unicode_version=CRE_UNICODE_DEFAULT)
    call check(re%unicode_version() == 17, "the default resolved")
    call re%compile("a", unicode_version=CRE_UNICODE_13)
    call check(re%unicode_version() == 13, "13 resolved")
    call re%compile("a", unicode_version=CRE_UNICODE_17)
    call check(re%unicode_version() == 17, "17 resolved")
    call re%compile("a", unicode_version=CRE_UNICODE_18)
    call check(re%unicode_version() == 18, "18 resolved")
    call re%compile("a", unicode_version=CRE_UNICODE_LATEST)
    call check(re%unicode_version() == latest_unicode_version, "the latest resolved")
    call re%compile("a", stat=stat, unicode_version=12)
    call check(stat == CRE_INVALID_ARGUMENT .and. .not. re%is_compiled(), "a version not carried")
    call re%compile("a", stat=stat, edition=2)
    call check(stat == CRE_INVALID_ARGUMENT, "an edition not known")
    call check(.not. is_valid("a", stat=stat, unicode_version=19) .and. stat == CRE_INVALID_ARGUMENT, &
      "a version not carried, is_valid")

    ! Each version has its own scripts.
    call check(.not. is_valid("\p{Script=Vithkuqi}", "u", unicode_version=CRE_UNICODE_13), "Vithkuqi under 13")
    call check(is_valid("\p{Script=Vithkuqi}", "u"), "Vithkuqi by default")
    call check(is_valid("\p{Script=Yezidi}", "u", unicode_version=CRE_UNICODE_13), "Yezidi under 13")
    call check(.not. is_valid("\p{Script=Jurchen}", "u"), "Jurchen by default")
    call check(is_valid("\p{scx=Jurc}", "u", unicode_version=CRE_UNICODE_18), "Jurchen under 18")
    call check(is_valid("\p{Script=Jurchen}", "u", unicode_version=CRE_UNICODE_LATEST), "Jurchen under the latest")
    call re%compile("\p{Script=Vithkuqi}", "u", stat=stat, unicode_version=CRE_UNICODE_13)
    call check(stat == CRE_INVALID_PATTERN, "Vithkuqi under 13 refused")

    ! A property and case folding are the version's.
    call re%compile("^\p{Ll}$", "u")
    call check(re%test(l10597), "Ll by default")
    call re%compile("^\p{Ll}$", "u", unicode_version=CRE_UNICODE_13)
    call check(.not. re%test(l10597), "Ll under 13")
    call re%compile(l10570, "iu")
    call check(re%test(l10597), "folding by default")
    call re%compile(l10570, "iu", unicode_version=CRE_UNICODE_13)
    call check(.not. re%test(l10597), "folding under 13")
    call re%compile(a7c0, "i")
    call check(re%test(a7c1), "simple folding by default")
    call re%compile(a7c0, "i", unicode_version=CRE_UNICODE_13)
    call check(.not. re%test(a7c1), "simple folding under 13")

    ! A case-insensitive backreference folds as its version folds, over UTF-8 and UTF-16.
    do k = 1, 2
      if (k == 1) then
        flags = "iu"
        text = l10570 // l10597
        twice = l10570 // l10570
        text16 = [u10570, u10597]
      else
        flags = "i"
        text = a7c0 // a7c1
        twice = a7c0 // a7c0
        text16 = [ua7c0, ua7c1]
      end if
      do i = 1, size(backrefs)
        do v = 17, 18
          call re%compile(trim(backrefs(i)), flags, unicode_version=v)
          call check(re%test(text), trim(backrefs(i)) // " /" // flags // " folds")
          call check(re%test(text16), trim(backrefs(i)) // " /" // flags // " folds over UTF-16")
        end do
        call re%compile(trim(backrefs(i)), flags, unicode_version=CRE_UNICODE_13)
        call check(.not. re%test(text), trim(backrefs(i)) // " /" // flags // " under 13")
        call check(.not. re%test(text16), trim(backrefs(i)) // " /" // flags // " under 13 over UTF-16")
        call check(re%test(twice), trim(backrefs(i)) // " /" // flags // " under 13, the same letter twice")
      end do
    end do

    ! The grammar of 2020 refuses what later editions added, and accepts what it had.
    do i = 1, size(refused)
      call re%compile(trim(refused(i)), trim(refused_flags(i)), stat=stat, edition=CRE_EDITION_ES2020, &
        unicode_version=CRE_UNICODE_LATEST)
      call check(stat == CRE_INVALID_PATTERN, trim(refused(i)) // " refused by 2020")
      call check(.not. is_valid(trim(refused(i)), trim(refused_flags(i)), edition=CRE_EDITION_ES2020, &
        unicode_version=CRE_UNICODE_LATEST), trim(refused(i)) // " not valid in 2020")
      call re%compile(trim(refused(i)), trim(refused_flags(i)), stat=stat, unicode_version=CRE_UNICODE_LATEST)
      call check(stat == CRE_OK, trim(refused(i)) // " today")
    end do
    do i = 1, size(accepted)
      call re%compile(trim(accepted(i)), trim(accepted_flags(i)), stat=stat, edition=CRE_EDITION_ES2020, &
        unicode_version=CRE_UNICODE_13)
      call check(stat == CRE_OK .and. re%edition() == CRE_EDITION_ES2020 .and. re%unicode_version() == 13, &
        trim(accepted(i)) // " accepted by 2020")
    end do
    do i = 1, size(two_flags)
      call re%compile("a", trim(two_flags(i)), stat=stat, edition=CRE_EDITION_ES2020)
      call check(stat == CRE_INVALID_FLAGS, trim(two_flags(i)) // " refused by 2020")
      call check(.not. is_valid("a", trim(two_flags(i)), edition=CRE_EDITION_ES2020), &
        trim(two_flags(i)) // " not valid")
      call re%compile("a", trim(two_flags(i)), stat=stat)
      call check(stat == CRE_OK, trim(two_flags(i)) // " today")
    end do

    ! The settings and a timeout go together; compile with stat and a timeout checks a pattern without a default.
    call re%compile("^(?:(a+)\1*)+$", "u", timeout=0.03_real64, edition=CRE_EDITION_ES2020, &
      unicode_version=CRE_UNICODE_13)
    call check(abs(re%match_timeout() - 0.03_real64) < 1e-9_real64, "the settings with a timeout")
    call check(.not. re%test(repeat("a", 60) // "b", stat=stat) .and. stat == CRE_MATCH_LIMIT, "the settings' timeout")
    call re%compile("(?i:a)", stat=stat, timeout=0.03_real64, edition=CRE_EDITION_ES2020)
    call check(stat == CRE_INVALID_PATTERN .and. .not. re%is_compiled(), "a check with a timeout")
    call set_default_match_timeout(0.03_real64)
    call re%compile("a", edition=CRE_EDITION_ES2020, unicode_version=CRE_UNICODE_18)
    call set_default_match_timeout(0.0_real64)
    call check(abs(re%match_timeout() - 0.03_real64) < 1e-9_real64, "the settings take the default timeout")
  end subroutine editions

  ! The README's example.
  subroutine readme()
    type(regex_t) :: date
    type(captures_t) :: caps
    type(match_t) :: m
    type(piece_t), allocatable :: pieces(:)
    type(regex_t) :: separator, schema
    integer :: stat
    character(*), parameter :: text = "due 2026-10"
    call date%compile("(?<year>\d{4})-(?<month>\d{2})", "g")
    call check(date%test(text), "readme test")
    call check(date%exec(text, caps), "readme exec")
    m = caps%named("year")
    call check(text(m%start:m%end) == "2026", "readme year")
    call check(date%count("2026-10, 2027-01") == 2, "readme count")
    call check(date%replace("2026-10, 2027-01", "$<month>/$1") == "10/2026, 01/2027", "readme replace")
    call separator%compile("\s*(;)\s*")
    call separator%split("a ; b;c", pieces)
    call check(size(pieces) == 5 .and. pieces(3)%value == "b", "readme split")
    call schema%compile("^\p{L}+$", "u", stat=stat, timeout=1.0_real64, edition=CRE_EDITION_ES2020, &
                        unicode_version=CRE_UNICODE_13)
    call check(stat == CRE_OK .and. schema%test("caf" // char(195) // char(169)), "readme schema")
  end subroutine readme

  ! With v, a class may hold a class, and the core reads each level of nesting by recursion: a pattern nested as deep
  ! as allowed must be read on the core's own deep stack, not the caller's, so that it compiles on a small stack (a host
  ! language's thread, or a main thread under a small ulimit -s), and one nested deeper must be refused, not overflow
  ! the stack and abort the process. Fortran makes no threads of its own, so this runs the test again as a child
  ! process whose main thread has a 256 KB stack; a crash there is its exit status. Where there is no ulimit
  ! (Windows), it runs here, on the main thread's stack. It needs the C library 0.1.2 or later (0.1.1 counted only
  ! groups when choosing the stack, and the child crashes).
  subroutine deep_classes()
    character(:), allocatable :: self
    character(16) :: os
    integer :: n, exitstat, cmdstat
    call get_environment_variable("OS", os)
    if (os == "Windows_NT") then
      call deep_classes_here()
      return
    end if
    call get_command_argument(0, length=n)
    allocate (character(n) :: self)
    call get_command_argument(0, self)
    exitstat = -1
    call execute_command_line("ulimit -s 256 && exec '" // self // "' deep-classes", exitstat=exitstat, &
                              cmdstat=cmdstat)
    call check(cmdstat == 0 .and. exitstat == 0, "nested classes on a 256 KB stack (the child process failed)")
  end subroutine deep_classes

  subroutine deep_classes_here()
    type(regex_t) :: re
    integer :: stat
    call re%compile(repeat("[", 1000) // "a" // repeat("]", 1000), "v", stat=stat)
    call check(stat == CRE_OK, "classes nested 1000 deep compile")
    if (stat == CRE_OK) call check(re%test("a") .and. .not. re%test("b"), "classes nested 1000 deep match")
    call re%compile(repeat("[", 200000), "v", stat=stat)
    call check(stat == CRE_PATTERN_TOO_LARGE .and. .not. re%is_compiled(), "classes nested 200000 deep are too large")
  end subroutine deep_classes_here
end program api_test
