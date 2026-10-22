!> A small JSON reader for this repository's tools: the test data's records and the benchmark's workloads and figures.
!> It reads a whole text into a tree of node_t, decoding every string's escapes to UTF-8 as it goes. A lone surrogate
!> escape is decoded as a number past the last code point of Unicode (0x110000 + its offset from U+D800) and a NUL
!> escape as 0x110800, each written as UTF-8 writes a code point there, so that a string that holds either is known and
!> no character of the text is taken for one (json-fortran, with its default character kind, keeps \u escapes as they
!> are and cannot tell \\u0041 from A once read).
module jsonlite
  use, intrinsic :: iso_fortran_env, only: int64, real64
  implicit none
  private

  public :: node_t, parse, member, item, size_of

  integer, parameter, public :: json_null = 0, json_logical = 1, json_number = 2, json_string = 3, json_array = 4, &
                                json_object = 5
  integer, parameter, public :: lone_first = int(z'110000'), lone_last = int(z'1107FF'), nul_mark = int(z'110800')

  !> A JSON value: its kind, and for a logical b, a number x (and i, when whole), a string s; an array's or an object's
  !> items, an object's with their names in key.
  type :: node_t
    integer :: kind = json_null
    logical :: b = .false.
    real(real64) :: x = 0
    integer(int64) :: i = 0
    character(:), allocatable :: s
    character(:), allocatable :: key
    type(node_t), allocatable :: items(:)
  end type node_t

contains

  !> Reads a JSON text into root: ok = .false. when it is not JSON.
  subroutine parse(text, root, ok)
    character(*), intent(in) :: text
    type(node_t), intent(out) :: root
    logical, intent(out) :: ok
    integer :: at
    at = 1
    ok = .true.
    call value(text, at, root, ok)
    if (ok) then
      call blank(text, at)
      ok = at > len(text)
    end if
  end subroutine parse

  !> The member of an object named by a path of names separated by dots, or null() when there is none.
  function member(n, path) result(p)
    type(node_t), intent(in), target :: n
    character(*), intent(in) :: path
    type(node_t), pointer :: p
    integer :: dot, k
    character(:), allocatable :: rest, name
    p => n
    rest = path
    do while (len(rest) > 0)
      dot = index(rest, ".")
      if (dot == 0) then
        name = rest
        rest = ""
      else
        name = rest(:dot - 1)
        rest = rest(dot + 1:)
      end if
      if (p%kind /= json_object) then
        p => null()
        return
      end if
      do k = 1, size(p%items)
        if (p%items(k)%key == name) exit
      end do
      if (k > size(p%items)) then
        p => null()
        return
      end if
      p => p%items(k)
    end do
  end function member

  !> The k-th item of an array (from 1).
  function item(n, k) result(p)
    type(node_t), intent(in), target :: n
    integer, intent(in) :: k
    type(node_t), pointer :: p
    p => n%items(k)
  end function item

  !> The number of items of an array or an object (0 for anything else).
  integer function size_of(n)
    type(node_t), intent(in) :: n
    size_of = 0
    if (allocated(n%items)) size_of = size(n%items)
  end function size_of

  subroutine blank(text, at)
    character(*), intent(in) :: text
    integer, intent(inout) :: at
    do while (at <= len(text))
      select case (text(at:at))
      case (" ", achar(9), achar(10), achar(13))
        at = at + 1
      case default
        return
      end select
    end do
  end subroutine blank

  recursive subroutine value(text, at, n, ok)
    character(*), intent(in) :: text
    integer, intent(inout) :: at
    type(node_t), intent(inout) :: n
    logical, intent(inout) :: ok
    integer :: count, k
    call blank(text, at)
    if (at > len(text)) then
      ok = .false.
      return
    end if
    select case (text(at:at))
    case ('"')
      n%kind = json_string
      call string(text, at, n%s, ok)
    case ("{", "[")
      n%kind = merge(json_object, json_array, text(at:at) == "{")
      ! Counted first, so that the items are allocated once and read in place.
      count = items_in(text, at)
      allocate (n%items(count))
      at = at + 1
      do k = 1, count
        call blank(text, at)
        if (n%kind == json_object) then
          call string(text, at, n%items(k)%key, ok)
          if (.not. ok) return
          call blank(text, at)
          at = at + 1  ! the colon
        end if
        call value(text, at, n%items(k), ok)
        if (.not. ok) return
        call blank(text, at)
        at = at + 1  ! a comma, or the closing bracket
      end do
      if (count == 0) then
        call blank(text, at)
        at = at + 1
      end if
    case ("t")
      n%kind = json_logical
      n%b = .true.
      at = at + 4
    case ("f")
      n%kind = json_logical
      at = at + 5
    case ("n")
      n%kind = json_null
      at = at + 4
    case default
      call number(text, at, n, ok)
    end select
  end subroutine value

  ! The number of items of the array or object that starts at text(at:at).
  integer function items_in(text, at)
    character(*), intent(in) :: text
    integer, intent(in) :: at
    integer :: i, depth
    logical :: any
    items_in = 0
    depth = 0
    any = .false.
    i = at + 1
    do while (i <= len(text))
      select case (text(i:i))
      case ('"')
        if (depth == 0) any = .true.
        i = i + 1
        do while (i <= len(text))
          if (text(i:i) == "\") then
            i = i + 2
            cycle
          end if
          if (text(i:i) == '"') exit
          i = i + 1
        end do
      case ("{", "[")
        if (depth == 0) any = .true.
        depth = depth + 1
      case ("}", "]")
        if (depth == 0) exit
        depth = depth - 1
      case (",")
        if (depth == 0) items_in = items_in + 1
      case (" ", achar(9), achar(10), achar(13))
      case default
        if (depth == 0) any = .true.
      end select
      i = i + 1
    end do
    if (any) items_in = items_in + 1
  end function items_in

  subroutine number(text, at, n, ok)
    character(*), intent(in) :: text
    integer, intent(inout) :: at
    type(node_t), intent(inout) :: n
    logical, intent(inout) :: ok
    integer :: start, ios
    start = at
    do while (at <= len(text))
      if (index("+-0123456789.eE", text(at:at)) == 0) exit
      at = at + 1
    end do
    if (at == start) then
      ok = .false.
      return
    end if
    n%kind = json_number
    read (text(start:at - 1), *, iostat=ios) n%x
    if (ios /= 0) then
      ok = .false.
      return
    end if
    n%i = nint(n%x, int64)
  end subroutine number

  ! A string, from its opening quotation mark, decoded: a lone surrogate as 0x110000 + its offset from U+D800, a NUL as
  ! 0x110800.
  subroutine string(text, at, s, ok)
    character(*), intent(in) :: text
    integer, intent(inout) :: at
    character(:), allocatable, intent(out) :: s
    logical, intent(inout) :: ok
    character(:), allocatable :: buffer
    integer :: start, finish, k, j, v, w, cp
    character(1) :: c
    start = at + 1
    ! The closing quotation mark, past escapes.
    finish = start
    do while (finish <= len(text))
      if (text(finish:finish) == "\") then
        finish = finish + 2
        cycle
      end if
      if (text(finish:finish) == '"') exit
      finish = finish + 1
    end do
    if (finish > len(text)) then
      ok = .false.
      return
    end if
    at = finish + 1
    if (index(text(start:finish - 1), "\") == 0) then
      s = text(start:finish - 1)
      return
    end if
    allocate (character(finish - start) :: buffer)
    k = 0
    j = start
    do while (j < finish)
      c = text(j:j)
      if (c /= "\") then
        k = k + 1
        buffer(k:k) = c
        j = j + 1
        cycle
      end if
      c = text(j + 1:j + 1)
      select case (c)
      case ("u")
        v = hex(text(j + 2:j + 5))
        j = j + 6
        cp = v
        if (v >= int(z'D800') .and. v <= int(z'DBFF') .and. j + 5 < finish) then
          if (text(j:j + 1) == "\u") then
            w = hex(text(j + 2:j + 5))
            if (w >= int(z'DC00') .and. w <= int(z'DFFF')) then
              cp = int(z'10000') + ishft(v - int(z'D800'), 10) + (w - int(z'DC00'))
              j = j + 6
            end if
          end if
        end if
        if (cp == 0) then
          cp = nul_mark
        else if (cp >= int(z'D800') .and. cp <= int(z'DFFF')) then
          cp = lone_first + cp - int(z'D800')
        end if
        call put_utf8(buffer, k, cp)
      case ("n")
        k = k + 1
        buffer(k:k) = achar(10)
        j = j + 2
      case ("t")
        k = k + 1
        buffer(k:k) = achar(9)
        j = j + 2
      case ("r")
        k = k + 1
        buffer(k:k) = achar(13)
        j = j + 2
      case ("b")
        k = k + 1
        buffer(k:k) = achar(8)
        j = j + 2
      case ("f")
        k = k + 1
        buffer(k:k) = achar(12)
        j = j + 2
      case default
        ! A backslash, a quotation mark or a solidus: the character itself.
        k = k + 1
        buffer(k:k) = c
        j = j + 2
      end select
    end do
    s = buffer(1:k)
  end subroutine string

  integer function hex(s)
    character(*), intent(in) :: s
    integer :: ios
    read (s, "(z4)", iostat=ios) hex
    if (ios /= 0) hex = 0
  end function hex

  ! Appends a code point's UTF-8 to the first k characters of buffer (the escape it was read from is longer).
  subroutine put_utf8(buffer, k, c)
    character(*), intent(inout) :: buffer
    integer, intent(inout) :: k
    integer, intent(in) :: c
    if (c < 128) then
      buffer(k + 1:k + 1) = achar(c)
      k = k + 1
    else if (c < 2048) then
      buffer(k + 1:k + 2) = achar(ior(192, ishft(c, -6))) // achar(ior(128, iand(c, 63)))
      k = k + 2
    else if (c < 65536) then
      buffer(k + 1:k + 3) = achar(ior(224, ishft(c, -12))) // achar(ior(128, iand(ishft(c, -6), 63))) // &
                            achar(ior(128, iand(c, 63)))
      k = k + 3
    else
      buffer(k + 1:k + 4) = achar(ior(240, ishft(c, -18))) // achar(ior(128, iand(ishft(c, -12), 63))) // &
                            achar(ior(128, iand(ishft(c, -6), 63))) // achar(ior(128, iand(c, 63)))
      k = k + 4
    end if
  end subroutine put_utf8
end module jsonlite
