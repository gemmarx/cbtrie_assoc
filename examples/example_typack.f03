
program main
    use class_typack
    implicit none
    character, allocatable :: c(:)
    character(:), allocatable :: s
    type(typack) :: tpk
    byte, allocatable :: b(:)

    call tpk%drop
    print *, tpk%is_numeric()

    call tpk%enpack('foo')
    print *, tpk%is_character()
    print *, tpk%is_integer()
    print *, tpk%get()
    print *, tpk%depack()
    print *, tpk%depack('')

    call tpk%enpack((999999999))
    print *, tpk%is_integer()
    print *, tpk%is_real()
    print *, tpk%get()
    print *, tpk%depack(0)

    call tpk%enpack(2.23620679)
    print *, tpk%is_real()
    print *, tpk%get()
    print *, tpk%depack(0e0)

    call tpk%enpack(1.192d-12)
    print *, tpk%is_double()
    print *, tpk%get()
    print *, tpk%depack(0d0)

    call tpk%enpack((257e-2,55e-1))
    print *, tpk%is_complex()
    print *, tpk%get()
    print *, tpk%depack((0,0))

    call tpk%enpack((3.14d-7,2.71d-28))
    print *, tpk%is_dcomplex()
    print *, tpk%get()
    print *, tpk%depack((0d0,0d0))
end program main

