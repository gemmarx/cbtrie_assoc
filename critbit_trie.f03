
module const_whole
  implicit none
  integer, parameter :: TRIE_SIZE=32  !initial number of key-value pairs
  character(8), parameter :: NULLTERM='00000000'  !special octet for string termination
end module const_whole

module class_resource_pool
  implicit none
  private
  type, public :: respool
    integer, allocatable :: que(:)
    integer :: ind, last
  contains
    procedure :: init, acquire, release, expand
  end type respool

contains
  subroutine init(self, num)
    class(respool), intent(inout) :: self
    integer, intent(in) :: num
    integer :: i
    self%que = ([(i,i=1,num)])
    self%ind = 1
    self%last = num
  end subroutine init

  subroutine acquire(self, idx)
    class(respool), intent(inout) :: self
    integer, intent(out) :: idx
    idx = self%que(self%ind)
    self%ind = 1+self%ind
    if(self%last.eq.self%ind) call self%expand
  end subroutine acquire

  subroutine release(self, idx)
    class(respool), intent(inout) :: self
    integer, intent(in) :: idx
    self%ind = -1+self%ind
    self%que(self%ind) = idx
  end subroutine release

  subroutine expand(self)
    class(respool), intent(inout) :: self
    integer, allocatable :: tmp(:)
    integer :: i, n
    n = size(self%que)
    tmp = ([(i, i=1, 2*n)])
    tmp(1:n) = self%que
    self%que = tmp
    self%last = 2*n
  end subroutine expand
end module class_resource_pool

module util_string_array
  implicit none
  type strarray
    character, allocatable :: c(:)
  end type strarray

contains
  function split(str) result(arr)
    character(*), intent(in) :: str
    character :: arr(len_trim(str))
    integer :: i
    do i=1, len_trim(str)
      arr(i) = str(i:i)
    end do
  end function split

  function join(arr) result(str)
    character, intent(in) :: arr(:)
    character :: str*(size(arr))
    integer :: i
    do i=1, size(arr)
      str(i:i) = arr(i)
    end do
  end function join
end module util_string_array

module class_trie
  use class_resource_pool
  implicit none
  private
  type node
    integer :: n0, n1, up, dat
  end type node
  type, public, extends(respool) :: trie
    type(node), allocatable :: t(:)
  contains
    procedure :: init, expand, cp, retrieve
  end type trie

contains
  subroutine init(self, num)
    class(trie), intent(inout) :: self
    integer, intent(in) :: num
    allocate(self%t(num))
    self%t(1)%dat = 0
    call self%respool%init(num)
  end subroutine init

  subroutine expand(self)
    class(trie), intent(inout) :: self
    type(node), allocatable :: tmp(:)
    integer :: n
    n = size(self%t)
    tmp = self%t
    deallocate(self%t)
    allocate(self%t(2*n))
    self%t(1:n) = tmp
    call self%respool%expand
  end subroutine expand

  subroutine cp(self, src, dest, do_cp_up)
    class(trie), intent(inout) :: self
    integer, intent(in) :: src, dest
    logical, optional :: do_cp_up
    self%t(dest)%n0 = self%t(src)%n0
    self%t(dest)%n1 = self%t(src)%n1
    self%t(dest)%dat = self%t(src)%dat
    if(present(do_cp_up)) self%t(dest)%up = self%t(src)%up
  end subroutine cp

  recursive function retrieve(self, bseq, node) result(nearleaf)
    class(trie), intent(inout) :: self
    character, intent(in) :: bseq(:)
    integer :: node, dat, next, nearleaf
    dat = self%t(node)%dat
    if(0.lt.dat) then
      nearleaf = node
    else
      if('0'.eq.bseq(-dat)) then
        next = self%t(node)%n0
      else
        next = self%t(node)%n1
      end if
      nearleaf = self%retrieve(bseq, next)
    end if
  end function retrieve
end module class_trie

module class_kv_arrays
  use util_string_array
  use class_resource_pool
  implicit none
  private
  type, public, extends(respool) :: kvarrs
    type(strarray), allocatable :: sk(:), bk(:), v(:)
  contains
    procedure :: init, expand
  end type kvarrs

contains
  subroutine init(self, num)
    class(kvarrs), intent(inout) :: self
    integer, intent(in) :: num
    allocate(self%sk(num), self%bk(num), self%v(num))
    call self%respool%init(num)
  end subroutine init

  subroutine expand(self)
    class(kvarrs), intent(inout) :: self
    call expand1(self%sk)
    call expand1(self%bk)
    call expand1(self%v)
    call self%respool%expand
  end subroutine expand
  
  subroutine expand1(a)
    type(strarray), intent(inout), allocatable :: a(:)
    type(strarray), allocatable :: tmp(:)
    integer :: n
    n = size(a)
    tmp = a
    deallocate(a)
    allocate(a(2*n))
    a(1:n) = tmp
  end subroutine expand1
end module class_kv_arrays

module assoc_critbit_trie
  use const_whole
  use util_string_array
  use class_trie
  use class_kv_arrays
  implicit none
  private
  type, public :: assoc
    type(trie) :: cbt
    type(kvarrs) :: kvs
  contains
    procedure :: init, add, get, rm
  end type assoc

contains
  subroutine init(self)
    class(assoc), intent(inout) :: self
    call self%cbt%init(2*TRIE_SIZE)
    call self%kvs%init(TRIE_SIZE)
  end subroutine init

  character(8) function char_to_oct(ch)
    character(*), intent(in) :: ch
    write(char_to_oct, '(b8.8)') iachar(ch)
  end function char_to_oct

  function str_to_bit(str) result(bitseq)
    character(*), intent(in) :: str
    character(:), allocatable :: bitstr
    character, allocatable :: bitseq(:)
    integer :: i
    bitstr=""
    do i=1, len_trim(str)
      bitstr = bitstr//char_to_oct(str(i:i))
    end do
    bitstr = bitstr//NULLTERM
    bitseq = split(bitstr)
  end function str_to_bit 

  subroutine add(self, k, v)
    class(assoc), intent(inout) :: self
    character(*), intent(in) :: k, v
    integer :: src, dest, new, srckv, kvloc, cdigit
    character, allocatable :: bseq(:)
    character :: cbit

    bseq = str_to_bit(k)
    call self%cbt%acquire(new)

    if(0.ne.self%cbt%t(1)%dat) then
      src = self%cbt%retrieve(bseq,1)
      srckv = self%cbt%t(src)%dat
      if(all(bseq.eq.self%kvs%bk(srckv)%c)) then
        call self%cbt%release(new)
        self%kvs%v(srckv)%c = split(v)
        return
      end if
      call self%cbt%acquire(dest)
      self%cbt%t(dest)%dat = srckv
      self%cbt%t(dest)%up = src
      self%cbt%t(new)%up = src
      cdigit = get_crit_digit(bseq, self%kvs%bk(srckv)%c)
      cbit = bseq(cdigit)
      self%cbt%t(src)%dat = -cdigit
      if('0'.eq.cbit) then
        self%cbt%t(src)%n0 = new
        self%cbt%t(src)%n1 = dest
      else
        self%cbt%t(src)%n0 = dest
        self%cbt%t(src)%n1 = new
      end if
    end if

    call self%kvs%acquire(kvloc)
    self%cbt%t(new)%dat = kvloc
    self%kvs%sk(kvloc)%c = split(k)
    self%kvs%bk(kvloc)%c = bseq
    self%kvs%v(kvloc)%c = split(v)
  end subroutine add

  subroutine rm(self, key)
    class(assoc), intent(inout) :: self
    character(*), intent(in) :: key
    character, allocatable :: bseq(:)
    integer :: leaf, kvloc, knot, pair

    bseq = str_to_bit(key)
    leaf = self%cbt%retrieve(bseq,1)
    kvloc = self%cbt%t(leaf)%dat
    if(all(bseq.eq.self%kvs%bk(kvloc)%c)) then
      if(0.gt.self%cbt%t(1)%dat) then
        knot = self%cbt%t(leaf)%up
        if(leaf.eq.self%cbt%t(knot)%n0) then
          pair = self%cbt%t(knot)%n1
        else
          pair = self%cbt%t(knot)%n0
        end if
        call self%cbt%cp(pair, knot)
        call self%cbt%release(pair)
      end if
      call self%cbt%release(leaf)
      call self%kvs%release(kvloc)
    end if
  end subroutine rm

  function get(self, key)
    class(assoc), intent(inout) :: self
    character(*), intent(in) :: key
    character, allocatable :: bseq(:)
    character(:), allocatable :: get
    integer :: near, kvloc
    bseq = str_to_bit(key)
    near = self%cbt%retrieve(bseq,1)
    kvloc = self%cbt%t(near)%dat
    if(all(bseq.eq.self%kvs%bk(kvloc)%c)) get = join(self%kvs%v(kvloc)%c)
  end function get

  integer function get_crit_digit(seq1, seq2)
    character, intent(in) :: seq1(:), seq2(:)
    integer :: i=1
    do while(seq1(i).eq.seq2(i))
      i=1+i
    end do
    get_crit_digit = i
  end function get_crit_digit
end module assoc_critbit_trie














