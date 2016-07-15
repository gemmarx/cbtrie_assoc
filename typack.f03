
module class_typack
    use util_converter
    implicit none
    private

    logical, save :: inited=.false.
    byte, save :: HCHAR, HINT, HREAL, HDBLE, HCMPLX, HDCMPLX

    type, public :: typack
        logical :: given=.false.
        byte, private, allocatable :: v(:)
    contains
        procedure :: put, get, tpack, drop, get_type
        procedure :: is_character, is_real, is_double, &
            is_integer, is_complex, is_dcomplex, is_numeric
        procedure :: get_str, get_real, get_double, &
            get_integer, get_complex, get_dcomplex
        generic :: tunpack => &
            get_integer, get_real, get_double, &
            get_complex, get_dcomplex, get_str
    end type typack

contains
    subroutine init_header()
        byte, allocatable :: h(:)
        if(inited) return
        h = get_byte(int(z'a0'))
        HCHAR = h(size(h))
        h = get_byte(int(z'd3'))
        HINT = h(size(h))
        h = get_byte(int(z'ca'))
        HREAL = h(size(h))
        h = get_byte(int(z'cb'))
        HDBLE = h(size(h))
        h = get_byte(int(z'd7'))
        HCMPLX = h(size(h))
        h = get_byte(int(z'd8'))
        HDCMPLX = h(size(h))
        inited = .true.
    end subroutine init_header

    subroutine drop(self)
        class(typack), intent(inout) :: self
        if(allocated(self%v)) deallocate(self%v)
        allocate(self%v(0))
        self%given = .false.
    end subroutine drop

    subroutine put(self, seq)
        class(typack), intent(inout) :: self
        byte, intent(in) :: seq(:)
        call init_header
        call self%drop
        self%v = seq
        self%given = .true.
    end subroutine put

    function get(self)
        class(typack), intent(in) :: self
        byte :: get(size(self%v))
        get = self%v
    end function get

    subroutine tpack(self, v)
        class(typack), intent(inout) :: self
        class(*), intent(in) :: v
        byte, allocatable :: b(:)
        integer :: n

        call init_header
        call self%drop

        b = get_byte(v)
        select type(v)
        type is(character(*))
            b = [HCHAR,b]
        type is(integer)
            b = [HINT,b]
        type is(real)
            b = [HREAL,b]
        type is(double precision)
            b = [HDBLE,b]
        type is(complex)
            b = [HCMPLX,b]
        type is(complex(kind(0d0)))
            b = [HDCMPLX,b]
        end select

        self%v = b
        self%given = .true.
    end subroutine tpack

    logical function is_character(self)
        class(typack), intent(in) :: self
        is_character = .false.
        if(.not.self%given) return
        call init_header
        if(HCHAR.eq.self%v(1)) is_character = .true.
    end function is_character

    logical function is_integer(self)
        class(typack), intent(in) :: self
        is_integer = .false.
        if(.not.self%given) return
        call init_header
        if(HINT.eq.self%v(1)) is_integer = .true.
    end function is_integer

    logical function is_real(self)
        class(typack), intent(in) :: self
        is_real = .false.
        if(.not.self%given) return
        call init_header
        if(HREAL.eq.self%v(1)) is_real = .true.
    end function is_real

    logical function is_double(self)
        class(typack), intent(in) :: self
        is_double = .false.
        if(.not.self%given) return
        call init_header
        if(HDBLE.eq.self%v(1)) is_double = .true.
    end function is_double

    logical function is_complex(self)
        class(typack), intent(in) :: self
        is_complex = .false.
        if(.not.self%given) return
        call init_header
        if(HCMPLX.eq.self%v(1)) is_complex = .true.
    end function is_complex

    logical function is_dcomplex(self)
        class(typack), intent(in) :: self
        is_dcomplex = .false.
        if(.not.self%given) return
        call init_header
        if(HDCMPLX.eq.self%v(1)) is_dcomplex = .true.
    end function is_dcomplex

    logical function is_numeric(self)
        class(typack), intent(in) :: self
        is_numeric = .false.
        if(.not.self%given) return
        call init_header
        if(HINT.eq.self%v(1) .or. &
           HREAL.eq.self%v(1) .or. &
           HDBLE.eq.self%v(1) .or. &
           HCMPLX.eq.self%v(1) .or. &
           HDCMPLX.eq.self%v(1)) is_numeric = .true.
    end function is_numeric

    function get_type(self)
        class(typack), intent(in) :: self
        character(:), allocatable :: get_type
        byte :: h

        get_type = 'no value'
        if(.not.self%given) return
        call init_header

        h = self%v(1)
        if(HCHAR.eq.h) then
            get_type = 'character'
        else if(HINT.eq.h) then
            get_type = 'integer'
        else if(HREAL.eq.h) then
            get_type = 'real'
        else if(HDBLE.eq.h) then
            get_type = 'double'
        else if(HCMPLX.eq.h) then
            get_type = 'complex'
        else if(HDCMPLX.eq.h) then
            get_type = 'dcomplex'
        else
            get_type = 'unknown'
        end if
    end function get_type

    function get_str(self, char_mold)
        class(typack), intent(in) :: self
        character(*), intent(in), optional :: char_mold
        integer :: i,n
        character(:), allocatable :: get_str
        byte :: h

        if(.not.self%given) return
        call init_header

        h = self%v(1)
        if(HCHAR.eq.h) then
            n = -1 + size(self%v)
            allocate(character(n)::get_str)
            forall(i=1:n) get_str(i:i) = transfer(self%v(1+i),' ')
        else if(HINT.eq.h) then
            get_str = ntos(self%tunpack(0))
        else if(HREAL.eq.h) then
            get_str = ntos(self%tunpack(0e0))
        else if(HDBLE.eq.h) then
            get_str = ntos(self%tunpack(0d0))
        else if(HCMPLX.eq.h) then
            get_str = ntos(self%tunpack((0,0)))
        else if(HDCMPLX.eq.h) then
            get_str = ntos(self%tunpack((0d0,0d0)))
        end if
    end function get_str

    function get_integer(self, mold) result(num)
        class(typack), intent(in) :: self
        integer, intent(in) :: mold
        integer :: n,num
        if(.not.self%given) return
        n = size(self%v)
        if(is_little_endian()) then
            num = transfer(self%v(n:2:-1),num)
        else
            num = transfer(self%v(2:n),num)
        end if
    end function get_integer

    function get_real(self, mold) result(num)
        class(typack), intent(in) :: self
        real, intent(in) :: mold
        integer :: n
        real :: num
        if(.not.self%given) return
        n = size(self%v)
        if(is_little_endian()) then
            num = transfer(self%v(n:2:-1),num)
        else
            num = transfer(self%v(2:n),num)
        end if
    end function get_real

    function get_double(self, mold) result(num)
        class(typack), intent(in) :: self
        double precision, intent(in) :: mold
        integer :: n
        double precision :: num
        if(.not.self%given) return
        n = size(self%v)
        if(is_little_endian()) then
            num = transfer(self%v(n:2:-1),num)
        else
            num = transfer(self%v(2:n),num)
        end if
    end function get_double

    function get_complex(self, mold) result(num)
        class(typack), intent(in) :: self
        complex, intent(in) :: mold
        complex, parameter :: ei=(0e0,1e0)
        integer :: n
        complex :: num
        if(.not.self%given) return
        n = size(self%v)
        if(is_little_endian()) then
            num = transfer(self%v(n:2:-1),num)
            num = aimag(num) + ei*real(num)
        else
            num = transfer(self%v(2:n),num)
        end if
    end function get_complex

    function get_dcomplex(self, mold) result(num)
        class(typack), intent(in) :: self
        complex(kind(0d0)), intent(in) :: mold
        complex(kind(0d0)), parameter :: di=(0d0,1d0)
        integer :: n
        complex(kind(0d0)) :: num
        if(.not.self%given) return
        n = size(self%v)
        if(is_little_endian()) then
            num = transfer(self%v(n:2:-1),num)
            num = aimag(num) + di*real(num)
        else
            num = transfer(self%v(2:n),num)
        end if
    end function get_dcomplex

    function get_byte(v) result(b)
        class(*), intent(in) :: v
        byte, allocatable :: b(:)
        integer :: n

        n = sizeof(v)
        allocate(b(n))

        select type(v)
        type is(character(*))
            b = str_to_byte(v)
        type is(integer)
            b = transfer(v,b)
            if(is_little_endian()) b = b(n:1:-1)
        type is(real)
            b = transfer(v,b)
            if(is_little_endian()) b = b(n:1:-1)
        type is(double precision)
            b = transfer(v,b)
            if(is_little_endian()) b = b(n:1:-1)
        type is(complex)
            if(is_little_endian()) then
                b = [transfer(aimag(v),b), transfer(real(v),b)]
                b = b(n:1:-1)
            else
                b = [transfer(real(v),b), transfer(aimag(v),b)]
            end if
        type is(complex(kind(0d0)))
            if(is_little_endian()) then
                b = [transfer(aimag(v),b), transfer(real(v),b)]
                b = b(n:1:-1)
            else
                b = [transfer(real(v),b), transfer(aimag(v),b)]
            end if
        end select
    end function get_byte

    logical function is_little_endian()
        logical, save :: judged=.false., have_little_end=.false.
        integer, parameter :: i=1
        byte, allocatable :: b(:)
        if(judged) then
            is_little_endian = have_little_end
        else
            allocate(b(sizeof(i)))
            b = transfer(i,b)
            if(1.eq.b(1)) have_little_end = .true.
            judged = .true.
        end if
    end function is_little_endian

    function str_to_byte(str) result(seq)
        character(*), intent(in) :: str
        byte :: seq(len_trim(str))
        integer :: i,n
        n = len_trim(str)
        forall(i=1:n) seq(i) = iachar(str(i:i))
    end function str_to_byte
end module class_typack

