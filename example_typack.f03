
program main
    use class_typack
    implicit none
    integer :: i
    real :: r
    double precision :: d
    complex :: p
    double complex :: x
    character, allocatable :: c(:)
    character(:), allocatable :: s
    type(typack) :: tpk
    byte, allocatable :: b(:)

    call tpk%drop
    print *, tpk%is_numeric()

    call tpk%tpack('foo')
    print *, tpk%is_character()
    print *, tpk%is_integer()
    print *, tpk%get()
    print *, tpk%get_str()

    call tpk%tpack((999999999))
    print *, tpk%is_integer()
    print *, tpk%is_real()
    print *, tpk%get()
    call tpk%unpack_num(i)
    print *, i

    call tpk%tpack(2.23620679)
    print *, tpk%is_real()
    print *, tpk%get()
    call tpk%unpack_num(r)
    print *, r

    call tpk%tpack(1.192d-12)
    print *, tpk%is_double()
    print *, tpk%get()
    call tpk%unpack_num(d)
    print *, d

    call tpk%tpack((257e-2,55e-1))
    print *, tpk%is_complex()
    print *, tpk%get()
    call tpk%unpack_num(p)
    print *, p

    call tpk%tpack((3.14d-7,2.71d-28))
    print *, tpk%is_dcomplex()
    print *, tpk%get()
    call tpk%unpack_num(x)
    print *, x
end program main

