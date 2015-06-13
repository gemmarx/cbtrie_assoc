
program main
  use assoc_critbit_trie
  use util_string_array
  implicit none
  integer :: i
  type(assoc) :: myassoc
  type(strarray), allocatable :: mykeys(:)


  call myassoc%init
  call myassoc%put('aaaaaaaaa', 'foofoobaz')
  call myassoc%put('aaaaaa', 'foobaz')
  call myassoc%put('aaaaaaaa', 'foofoobar')
  call myassoc%del('aaaaaa')
  call myassoc%put('aa', 'bar')
  call myassoc%put('aba', 'baz')
  call myassoc%put('a', 'foo')
  call myassoc%put('aaaaa', 'foobar')
  call myassoc%put('aaaaaaa', 'foofoofoo')
  call myassoc%del('a')
  call myassoc%put('aaaaaaaaaa', 'foobarfoo')
  call myassoc%put('aaaa', 'foofoo')

  call myassoc%keys(mykeys)
  do i=1, size(mykeys)
    print *, mykeys(i)%c
  end do


  print *, myassoc%have('aa')
  print *, myassoc%have('aaa')
  print *, myassoc%have('uuu')
  print *, myassoc%get('aa')
  call myassoc%put('aa', 'hogera')
  print *, myassoc%get('aa')
  call myassoc%del('aa')
  print *, myassoc%get('aa')
  call myassoc%put('aa', 'piyora')
  print *, myassoc%get('aa')
  call myassoc%put('aaa', 'toctoc')
  print *, myassoc%get('aaa')

end program main


