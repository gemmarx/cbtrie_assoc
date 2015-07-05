
program main
  use util_string_array
  use assoc_critbit_trie
  implicit none
  integer :: i
  type(assoc) :: myassoc
  type(strarray), allocatable :: mykeys(:)


  call myassoc%init
  call myassoc%put('aaaaaaaaa', 'foofoobaz')
  call myassoc%put('aaaaaa', 'foobaz')
  call myassoc%put('aaaaaaaa', 'foofoobar')
  call myassoc%put('多バイト文字', 'ＴＯ ＥＲＡＳＥ')
  call myassoc%del('aaaaaa')
  call myassoc%put('aa', 'bar')
  call myassoc%put('aba', 'baz')
  call myassoc%del('多バイト文字')
  call myassoc%put('a', 'foo')
  call myassoc%put('aaaaa', 'foobar')
  call myassoc%put('多バイト文字', '日式漢字')
  call myassoc%put('aaaaaaa', 'foofoofoo')
  call myassoc%del('a')
  call myassoc%put('aaaaaaaaaa', 'foobarfoo')
  call myassoc%put('aaaa', 'foofoo')

  call myassoc%put('real_num', ntos(5.12d-2))
  call myassoc%put('complex_num', ntos((1.2, 3.4)))

  call myassoc%keys(mykeys)
  do i=1, size(mykeys)
    print *, mykeys(i)%get()
  end do


  print *, myassoc%have('aa')
  print *, myassoc%have('aaa')
  print *, myassoc%have('uuu')
  print *, myassoc%get('aa')
  call myassoc%put('aa', 'uuura')
  print *, myassoc%get('aa')
  call myassoc%del('aa')
  print *, myassoc%get('aa')
  call myassoc%put('aa', 'ooora')
  print *, myassoc%get('aa')
  call myassoc%put('aaa', 'toctoc')
  print *, myassoc%get('aaa')
  print *, myassoc%get('多バイト文字')


  print *, stodp(myassoc%get('real_num'))
  print *, stocp(myassoc%get('complex_num'))


  print *, myassoc%first()
  print *, myassoc%next(myassoc%first())
  print *, myassoc%next("")
  print *, myassoc%next('aa')
  print *, myassoc%next('real_num')
  print *, myassoc%next('多バイト文')
  print *, myassoc%next('多バイト文字')
  print *, myassoc%next('多バイト文字文')

  print *, myassoc%prev("")
  print *, myassoc%prev('aa')
  print *, myassoc%prev('real_num')
  print *, myassoc%prev('多バイト文')
  print *, myassoc%prev('多バイト文字')
  print *, myassoc%prev('多バイト文字文')
  print *, myassoc%last()
  print *, myassoc%prev(myassoc%last())


  call myassoc%fin


  call myassoc%init('ascii')
  call myassoc%put('aaa', '1')
  call myassoc%put('aAa', '1')
  call myassoc%put('Aaa', '1')
  call myassoc%put('aab', '1')
  call myassoc%put('aaB', '1')
  print *, ""
  print *, 'ascii(default) order'
  call myassoc%keys(mykeys)
  do i=1, size(mykeys)
    print *, mykeys(i)%get()
  end do
  call myassoc%fin

  call myassoc%init('mesh')
  call myassoc%put('aaa', '1')
  call myassoc%put('aAa', '1')
  call myassoc%put('Aaa', '1')
  call myassoc%put('aab', '1')
  call myassoc%put('aaB', '1')
  print *, ""
  print *, 'mesh(zip) order'
  call myassoc%keys(mykeys)
  do i=1, size(mykeys)
    print *, mykeys(i)%get()
  end do
  call myassoc%fin

  call myassoc%init('ignore')
  call myassoc%put('aaa', '1')
  call myassoc%put('aAa', '1')
  call myassoc%put('Aaa', '1')
  call myassoc%put('aab', '1')
  call myassoc%put('aaB', '1')
  print *, ""
  print *, 'case ignore'
  call myassoc%keys(mykeys)
  do i=1, size(mykeys)
    print *, mykeys(i)%get()
  end do
  call myassoc%fin

end program main


