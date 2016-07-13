
program main
    use class_string
    implicit none
    integer :: i,n
    character, allocatable :: c(:)
    character(:), allocatable :: s
    type(string) :: str
    type(string), allocatable :: w(:)

    call str%put('Hoge')
    s = str%get()
    c = str%chars()
    print *, len(s)
    print *, size(c)
    print *, str%uc()
    print *, str%lc()

    call str%put('Hoge Fuga Piyo')
    allocate(w(str%nword()))
    call str%words(w)

    print *, size(w)
    do i=1, size(w)
        print *, w(i)%get()
    end do

    call str%put(0.0000038)
    print *, str%get_real()
end program main

