
program main
    use class_typack
    use class_assoc_cbtrie
    implicit none
    integer :: i
    double complex :: x
    character(:), allocatable :: c
    type(assoc) :: kvs
    type(typack) :: tpk
    type(typack), allocatable :: ks(:)

    call kvs%init
    call kvs%put('aaaaaaaaa', 'foofoobaz')
    call kvs%put('aaaaaa', 'foobaz')
    call kvs%put('aaaaaaaa', 'foofoobar')
    call kvs%put('多バイト文字', 'ＴＯ ＥＲＡＳＥ')
    call kvs%del('aaaaaa')
    call kvs%put('aa', 'bar')
    call kvs%put('aba', 'baz')
    call kvs%del('多バイト文字')
    call kvs%put('a', 'foo')
    call kvs%put('aaaaa', 'foobar')
    call kvs%put('多バイト文字', '日式漢字')
    call kvs%put('aaaaaaa', 'foofoofoo')
    call kvs%del('a')
    call kvs%put('aaaaaaaaaa', 'foobarfoo')
    call kvs%put('aaaa', 'foofoo')

    call kvs%put('#', 777)
    call kvs%put(88, 3.14)
    call kvs%put(1e0, (1.732d0, 2.236d0))

    print *, kvs%get('#',i)
    print *, kvs%get(88)
    print *, kvs%get(88, 0.0)
    call tpk%enpack(1.000)
    print *, kvs%get(tpk, x)

    print *
    call kvs%keys(ks)
    do i=1, size(ks)
        print *, ks(i)%get_str(), ' => ', &
            kvs%get_type(ks(i)), ':  ', &
            kvs%get(ks(i))
    end do

    print *
    print *, "The number of KV-Pairs:", kvs%nelm()

    print *
    print *, "Have 'aa'?  ", kvs%have('aa')
    print *, "Have 'aaa'?  ", kvs%have('aaa')
    print *, "Have 'uuu'?  ", kvs%have('uuu')

    print *
    tpk = kvs%first()
    print *, "First:  ", tpk%get_str()
    tpk = kvs%next(kvs%first())
    print *, "Next of First:  ", tpk%get_str()
    tpk = kvs%next('ab')
    print *, "Next of 'ab':  ", tpk%get_str()

    print *,
    tpk = kvs%prev('ab')
    print *, "Prev of 'ab':  ", tpk%get_str()
    tpk = kvs%prev(kvs%last())
    print *, "Prev of Last:  ", tpk%get_str()
    tpk = kvs%last()
    print *, "Last:  ", tpk%get_str()

    call kvs%drop

end program main

