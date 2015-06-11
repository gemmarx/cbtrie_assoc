
program main
  use assoc_critbit_trie
  implicit none
  integer :: i,j
  character(:), allocatable :: mykey, myval
  type(assoc) :: t

  call t%init
  call t%add('a', 'v')
  call t%add('aa', 'vv')
  call t%rm('a')
  myval = t%get('aa')
  print *, myval
end program main


