
program main
  use assoc_critbit_trie
  use util_string_array
  implicit none
  integer :: i,j,k,r, g(50)
  type(strarray) :: h(20)
  type(assoc) :: myassoc
  logical :: mybool
  type(strarray), allocatable :: mykeys(:)
  character(:), allocatable :: myval

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

  call myassoc%retrieve(str_to_bit('aa'),0,i,j)
  print *, i,j

  r = myassoc%cbt%root
  print *, r

  i=7
  j=5
  k=6
  g(1) = myassoc%cbt%t(i)%dat
  g(2) = myassoc%cbt%t(i)%n0
  g(3) = myassoc%cbt%t(i)%n1
  g(4) = myassoc%cbt%t(i)%up
  g(5) = myassoc%cbt%t(j)%dat
  g(6) = myassoc%cbt%t(j)%n0
  g(7) = myassoc%cbt%t(j)%n1
  g(8) = myassoc%cbt%t(j)%up
  g(9) = myassoc%cbt%t(k)%dat
  g(10) = myassoc%cbt%t(k)%n0
  g(11) = myassoc%cbt%t(k)%n1
  g(12) = myassoc%cbt%t(k)%up
  print *, g(1:4)
  print *, g(5:8)
  print *, g(9:12)

  i=1
  j=1
  k=1
  h(1) = myassoc%kvs%sk(i)
  h(2) = myassoc%kvs%bk(i)
  h(3) = myassoc%kvs%sk(j)
  h(4) = myassoc%kvs%bk(j)
  h(5) = myassoc%kvs%sk(k)
  h(6) = myassoc%kvs%bk(k)
  print *, h(1)%c
  print *, h(2)%c
  print *, h(3)%c
  print *, h(4)%c
  print *, h(5)%c
  print *, h(6)%c


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

  call myassoc%retrieve(str_to_bit('aa'),0,i,j)
  print *, i,j

end program main


