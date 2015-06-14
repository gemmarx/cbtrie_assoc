!
! "strarray" type provides a wrapper of allocatable array of character(1) instead of variable length character string.
! It may be better to use differed length character if your compiler is able to deal with it in type structures. (e.g. gcc-4.9 or later)
!

!
! Functions named "*to*" are converters between numeric types and character string
!


module util_string_array
  implicit none
  type strarray
    character, allocatable :: c(:)
  end type strarray

contains
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

  integer function get_crit_pos(seq1, seq2)
    character, intent(in) :: seq1(:), seq2(:)
    integer :: i
    do i=1, min(size(seq1), size(seq2))
      if(seq1(i).ne.seq2(i)) then
        get_crit_pos = i
        return
      end if
    end do
    get_crit_pos = 0
  end function get_crit_pos

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





