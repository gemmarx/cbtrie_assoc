
program main
  use assoc_critbit_trie
  implicit none
  character(:), allocatable :: myval
  type(assoc) :: myassoc

  call myassoc%init
  call myassoc%add('a', 'foo')
  call myassoc%add('aa', 'bar')
  call myassoc%rm('a')
  myval = myassoc%get('aa')
  print *, myval
end program main


