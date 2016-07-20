
! Functions named "*to*" are converters between numeric types and character string
module util_converter
    implicit none

contains
    function ntos(src)
        class(*), intent(in) :: src
        character :: fix*256
        character(:), allocatable :: ntos
        select type(src)
        type is(character(*))
            ntos = src
            return
        type is(integer)
            write(fix, *), src
        type is(real)
            write(fix, *), src
        type is(double precision)
            write(fix, *), src
        type is(complex)
            write(fix, *), src
        type is(complex(kind(0d0)))
            write(fix, *), src
        end select
        ntos = trim(adjustl(fix))
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
end module util_converter

