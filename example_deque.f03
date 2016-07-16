
program main
    use class_deque
    implicit none
    integer :: i
    character :: c
    character(:), allocatable :: v
    type(deque) :: q

    !call init_random
    !call q%init
    !do i=1,1000
    !    c = archar()
    !    call q%push(c)
    !end do
    !print *
    !do while(0.lt.q%nelm())
    !    call q%shift(v)
    !    print *, q%nelm(), q%last, v
    !end do
    !call q%drop

    call q%init
    call q%push('foo')
    call q%push('bar')
    call q%push('baz')
    call q%shift(v)
    print *, v
    call q%push('foo')
    call q%push('bar')
    call q%push('baz')
    call q%pop(v)
    print *, v
    call q%push('foo')
    call q%push('bar')
    call q%push('baz')
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

contains
    character function archar()
        integer :: i, d0=65, dx=90
        double precision :: p
        call random_number(p)
        i = int(d0 + (1 + dx - d0) * p)
        archar = trim(adjustl(achar(i)))
    end function archar

    !subroutine init_random
    !    integer :: n,fd
    !    integer, save, allocatable :: seed(:)
    !    call random_seed(size=n)
    !    if(.not.allocated(seed)) allocate(seed(n))
    !    open(newunit=fd,file='/dev/urandom',form='UNFORMATTED')
    !    read(fd) seed
    !    close(fd)
    !    call random_seed(put=seed)
    !end subroutine init_random
end program main

