
program main
    use class_typack
    use class_assoc_cbtrie
    implicit none
    integer :: i
    real :: r
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

    call kvs%put(88, 3.14)
    call kvs%put(1e0, (1.732d0, 2.236d0))

    print *, kvs%get_str(88)
    call tpk%tpack(88)
    call kvs%get_num(tpk, r)
    print *, r

    call kvs%keys(ks)
    do i=1, size(ks)
        print *, ks(i)%get_str(), &
            ' => ', kvs%get_type(ks(i)), &
            ':  ', kvs%get_str(ks(i))
    end do

    print *, kvs%have('aa')
    print *, kvs%have('aaa')
    print *, kvs%have('uuu')
    print *, kvs%get_str('aa')
    call kvs%put('aa', 'uuura')
    print *, kvs%get_str('aa')
    call kvs%del('aa')
    print *, kvs%get_str('aa')
    call kvs%put('aa', 'ooora')
    print *, kvs%get_str('aa')
    call kvs%put('aaa', 'toctoc')
    print *, kvs%get_str('aaa')
    print *, kvs%get_str('多バイト文字')

    print *, kvs%first()
    print *, kvs%next(kvs%first())
    print *, kvs%next("")
    print *, kvs%next('aa')
    print *, kvs%next('real_num')
    print *, kvs%next('多バイト文')
    print *, kvs%next('多バイト文字')
    print *, kvs%next('多バイト文字文')

    print *, kvs%prev("")
    print *, kvs%prev('aa')
    print *, kvs%prev('real_num')
    print *, kvs%prev('多バイト文')
    print *, kvs%prev('多バイト文字')
    print *, kvs%prev('多バイト文字文')
    print *, kvs%last()
    print *, kvs%prev(kvs%last())

    call kvs%drop

end program main

