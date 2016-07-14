
module class_typack
    use util_converter
    implicit none
    private

    logical, save :: inited=.false.
    byte, save :: HCHAR, HINT, HREAL, HDBLE, HCMPLX, HDCMPLX

    type, public :: typack
        byte, private, allocatable :: v(:)
        logical :: given=.false.
    contains
        procedure :: put, get, tpack, unpack_num, drop, &
            length, get_type, get_str, &
            is_character, is_real, is_double, &
            is_integer, is_complex, is_dcomplex, is_numeric
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
        byte, allocatable :: b(:), h(:)
        integer :: i,n

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

    integer function length(self)
        class(typack), intent(in) :: self
        if(.not.self%given) return
        length = -1 + size(self%v)
    end function length

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

    function get_str(self)
        class(typack), intent(in) :: self
        integer :: i,n
        real :: r
        double precision :: d
        complex :: p
        double complex :: x
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
            call self%unpack_num(n)
            get_str = ntos(n)
        else if(HREAL.eq.h) then
            call self%unpack_num(r)
            get_str = ntos(r)
        else if(HDBLE.eq.h) then
            call self%unpack_num(d)
            get_str = ntos(d)
        else if(HCMPLX.eq.h) then
            call self%unpack_num(p)
            get_str = ntos(p)
        else if(HDCMPLX.eq.h) then
            call self%unpack_num(x)
            get_str = ntos(x)
        end if
    end function get_str

    subroutine unpack_num(self, num)
        class(typack), intent(in) :: self
        class(*), intent(inout) :: num
        complex, parameter :: ei=(0e0,1e0)
        integer :: i,n

        if(.not.self%given) return
        n = size(self%v)

        select type(num)
        type is(integer)
            if(is_little_endian()) then
                num = transfer(self%v(n:2:-1),num)
            else
                num = transfer(self%v(2:n),num)
            end if
        type is(real)
            if(is_little_endian()) then
                num = transfer(self%v(n:2:-1),num)
            else
                num = transfer(self%v(2:n),num)
            end if
        type is(double precision)
            if(is_little_endian()) then
                num = transfer(self%v(n:2:-1),num)
            else
                num = transfer(self%v(2:n),num)
            end if
        type is(complex)
            if(is_little_endian()) then
                num = transfer(self%v(n:2:-1),num)
                num = aimag(num) + ei*real(num)
            else
                num = transfer(self%v(2:n),num)
            end if
        type is(complex(kind(0d0)))
            if(is_little_endian()) then
                num = transfer(self%v(n:2:-1),num)
                num = dimag(num) + ei*dble(num)
            else
                num = transfer(self%v(2:n),num)
            end if
        end select
    end subroutine unpack_num

    function get_byte(v) result(b)
        class(*), intent(in) :: v
        byte, allocatable :: b(:)
        integer :: i,n

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

