
program main
    use class_typack
    implicit none
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
    print *, tpk%tunpack()
    print *, tpk%tunpack('')

    call tpk%tpack((999999999))
    print *, tpk%is_integer()
    print *, tpk%is_real()
    print *, tpk%get()
    print *, tpk%tunpack(0)

    call tpk%tpack(2.23620679)
    print *, tpk%is_real()
    print *, tpk%get()
    print *, tpk%tunpack(0e0)

    call tpk%tpack(1.192d-12)
    print *, tpk%is_double()
    print *, tpk%get()
    print *, tpk%tunpack(0d0)

    call tpk%tpack((257e-2,55e-1))
    print *, tpk%is_complex()
    print *, tpk%get()
    print *, tpk%tunpack((0,0))

    call tpk%tpack((3.14d-7,2.71d-28))
    print *, tpk%is_dcomplex()
    print *, tpk%get()
    print *, tpk%tunpack((0d0,0d0))
end program main

