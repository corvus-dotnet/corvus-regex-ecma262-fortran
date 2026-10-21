! The README's example: compile a pattern, test, read the groups of a match by name, count, replace and split.
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
