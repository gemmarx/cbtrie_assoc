
program main
    use class_deque
    implicit none
    integer :: i,n
    type(deque) :: q
    character(:), allocatable :: v

    call q%init
    call q%push('hoge')
    call q%push('fuga')
    call q%push('piyo')
    call q%shift(v)
    print *, v
    call q%push('hoge')
    call q%push('fuga')
    call q%push('piyo')
    call q%pop(v)
    print *, v
    call q%push('hoge')
    call q%push('fuga')
    call q%push('piyo')
    call q%shift(v)
    print *, v
    call q%shift(v)
    print *, v
    call q%shift(v)
    print *, v
    call q%shift(v)
    print *, v
    call q%clip

    print *, q%nelm()
    call q%shift(v)
    print *, v

    print *, q%nelm()
    call q%shift(v)
    print *, v

    print *, q%nelm()
    if(q%is_empty()) then
        print *, 'Empty!'
    else
        call q%shift(v)
        print *, v
    end if

    print *, q%nelm()
    if(q%is_empty()) then
        print *, 'Empty!'
    else
        call q%shift(v)
        print *, v
    end if
end program main

