
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
    type(typack) :: str
    byte, allocatable :: v(:)

    call str%tpack('hoge')
    print *, str%is_character()
    print *, str%get()
    print *, str%get_str()

    call str%tpack('qwertyuiopasdfghjklzxcvbnm1234567890')
    print *, str%is_character()
    print *, str%get()
    print *, str%get_str()

    call str%tpack('qwertyuiopasdfghjklzxcvbnm1234567890 &
                    qwertyuiopasdfghjklzxcvbnm1234567890 &
                    qwertyuiopasdfghjklzxcvbnm1234567890 &
                    qwertyuiopasdfghjklzxcvbnm1234567890 &
                    qwertyuiopasdfghjklzxcvbnm1234567890 &
                    qwertyuiopasdfghjklzxcvbnm1234567890 &
                    qwertyuiopasdfghjklzxcvbnm1234567890 &
                    qwertyuiopasdfghjklzxcvbnm1234567890 &
                    qwertyuiopasdfghjklzxcvbnm1234567890 &
    ')
    print *, str%is_character()
    print *, str%get()
    print *, str%get_str()

    call str%tpack('日本語で書いた、書いたどー？書きおったわ！さてこんなもんかしら。')
    print *, str%is_character()
    print *, str%get()
    print *, str%get_str()

    call str%tpack((999999999))
    print *, str%is_integer()
    print *, str%get()
    call str%tunpack(i)
    print *, i

    call str%tpack(2.23620679)
    print *, str%is_real()
    print *, str%get()
    call str%tunpack(r)
    print *, r

    call str%tpack(1.192d-12)
    print *, str%is_double()
    print *, str%get()
    call str%tunpack(d)
    print *, d

    call str%tpack((257e-2,55e-1))
    print *, str%is_complex()
    print *, str%get()
    call str%tunpack(p)
    print *, p

    call str%tpack((3.14d-7,2.71d-28))
    print *, str%is_dcomplex()
    print *, str%get()
    call str%tunpack(x)
    print *, x
end program main

