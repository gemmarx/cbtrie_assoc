
module util_string_array
  implicit none

! Both types below makes simple wrappers of allocatable array.
! The purpose is to make allocatable array of allocatable objects.
! It may be better to use differed length character for "strarray" type if your compiler is able to deal with it in type structures. (e.g. gcc-4.9 or later)
  type strarray
    character, allocatable :: c(:)
    contains
      procedure :: put => put_str
      procedure :: get => get_str
  end type strarray

  type bytearray
    integer(1), allocatable :: c(:)
  end type bytearray

contains
  subroutine put_str(self, str)
    class(strarray), intent(inout) :: self
    character(*), intent(in) :: str
    self%c = split(str)
  end subroutine put_str

  function get_str(self)
    class(strarray), intent(inout) :: self
    character(:), allocatable :: get_str
    get_str = join(self%c)
  end function get_str

  function split(str) result(arr)
    character(*), intent(in) :: str
    character :: arr(len_trim(str))
    integer :: i
    do i=1, len_trim(str)
      arr(i) = str(i:i)
    end do
  end function split

  function join(arr) result(str)
    character, intent(in) :: arr(:)
    character :: str*(size(arr))
    integer :: i
    do i=1, size(arr)
      str(i:i) = arr(i)
    end do
  end function join

  character function uc(ch)
    character, intent(in) :: ch
    integer :: i
    uc = ch
    i = iachar(ch)
    if(97.le.i .and. i.le.122) uc = achar(-32+i)
  end function uc

  character function lc(ch)
    character, intent(in) :: ch
    integer :: i
    lc = ch
    i = iachar(ch)
    if(65.le.i .and. i.le.90) lc = achar(32+i)
  end function lc

! reorder "A-Z" and "a-z"
  function reorder_case(i) result(code)
    integer, intent(in) :: i  !ascii code
    integer :: code
    code = i
    if(65.le.i .and. i.le.90) then
      code = 65+2*(-65+i)
      if(90.lt.code) code = 6+code
    else if(97.le.i .and. i.le.122) then
      code = 66+2*(-65+(-32+i))
      if(90.lt.code) code = 6+code
    end if
  end function reorder_case

  character(8) function byte_to_bitchar(byte)
    integer(1), intent(in) :: byte
    write(byte_to_bitchar, '(b8.8)') byte
  end function byte_to_bitchar

  function str_to_byte(str) result(seq)
    character(*), intent(in) :: str
    integer(1), allocatable :: seq(:)
    integer :: i, n
    n = len_trim(str)
    allocate(seq(n))
    do i=1, n
      seq(i) = reorder_case(iachar(str(i:i)))
    end do
  end function str_to_byte

! Functions named "*to*" are converters between numeric types and character string
  function ntos(num) result(str)
    class(*), intent(in) :: num
    character :: fix*255
    character(:), allocatable :: str
    select type(num)
    type is(integer)
      write(fix, *), num
    type is(real)
      write(fix, *), num
    type is(double precision)
      write(fix, *), num
    type is(complex)
      write(fix, *), num
    type is(complex(kind(0d0)))
      write(fix, *), num
    end select
    str = trim(adjustl(fix))
  end function ntos

  function stoi(str) result(num)
    character(*), intent(in) :: str
    integer :: num
    read(str, *), num
  end function stoi

  function stor(str) result(num)
    character(*), intent(in) :: str
    real :: num
    read(str, *), num
  end function stor

  function stodp(str) result(num)
    character(*), intent(in) :: str
    double precision :: num
    read(str, *), num
  end function stodp

  function stocp(str) result(num)
    character(*), intent(in) :: str
    complex :: num
    read(str, *), num
  end function stocp

  function stocd(str) result(num)
    character(*), intent(in) :: str
    complex(kind(0d0)) :: num
    read(str, *), num
  end function stocd
end module util_string_array





