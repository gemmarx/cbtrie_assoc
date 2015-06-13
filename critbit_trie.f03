
module param_whole
  implicit none
  integer, parameter :: TRIE_SIZE=32  !initial number of key-value pairs
  character(8), parameter :: NULLTERM='00000000'  !special octet as string termination
end module param_whole

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

  integer function get_crit_pos(seq1, seq2)
    character, intent(in) :: seq1(:), seq2(:)
    integer :: i
    do i=1, min(size(seq1), size(seq2))
      if(seq1(i).ne.seq2(i)) then
        get_crit_pos = i
        return
      end if
    end do
    get_crit_pos = 0
  end function get_crit_pos
end module util_string_array

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

module class_trie
  use class_resource_pool
  implicit none
  private
  type node
    integer :: n0, n1, up, dat
  end type node
  type, public, extends(respool) :: trie
    type(node), allocatable :: t(:)
    integer :: root
  contains
    procedure :: init, expand, release, part_smallest
  end type trie

contains
  subroutine init(self, num)
    class(trie), intent(inout) :: self
    integer, intent(in) :: num
    allocate(self%t(num))
    self%root = 1
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

  subroutine release(self, idx)
    class(trie), intent(inout) :: self
    integer, intent(in) :: idx
    self%t(idx)%n0 = 0
    self%t(idx)%n1 = 0
    self%t(idx)%up = 0
    self%t(idx)%dat = 0
    call self%respool%release(idx)
  end subroutine release

  recursive integer function part_smallest(self, node)
    class(trie), intent(inout) :: self
    integer, intent(in) :: node
    if(0.lt.self%t(node)%dat) then
      part_smallest = node
    else
      part_smallest = self%part_smallest(self%t(node)%n0)
    end if
  end function part_smallest
end module class_trie

module class_kv_arrays
  use util_string_array
  use class_resource_pool
  implicit none
  private
  type, public, extends(respool) :: kvarrs
    type(strarray), allocatable :: sk(:), bk(:), v(:)
  contains
    procedure :: init, expand, release
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

  subroutine release(self, idx)
    class(kvarrs), intent(inout) :: self
    integer, intent(in) :: idx
    self%sk(idx)%c = ("")
    self%bk(idx)%c = ("")
    self%v(idx)%c = ("")
    call self%respool%release(idx)
  end subroutine release
end module class_kv_arrays

module assoc_critbit_trie
  use param_whole
  use util_string_array
  use class_trie
  use class_kv_arrays
  implicit none
  !private
  type, public :: assoc
    type(trie) :: cbt
    type(kvarrs) :: kvs
  contains
    procedure :: init, free, put, get, del, have, keys
    !procedure, private :: get_crit_digit, retrieve, is_same_key
    procedure :: get_crit_digit, retrieve, is_same_key
  end type assoc

contains
  subroutine init(self)
    class(assoc), intent(inout) :: self
    call self%cbt%init(2*TRIE_SIZE)
    call self%kvs%init(TRIE_SIZE)
  end subroutine init

  subroutine free(self)
    class(assoc), intent(inout) :: self
    deallocate(self%cbt%que, self%cbt%t, self%kvs%que, self%kvs%sk, self%kvs%bk, self%kvs%v)
  end subroutine free

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

  integer function get_crit_digit(self, bseq, node)
    class(assoc), intent(inout) :: self
    character, intent(in) :: bseq(:)
    integer, intent(in) :: node
    character, allocatable :: cseq(:)
    integer :: small, kvloc
    small = self%cbt%part_smallest(node)
    kvloc = self%cbt%t(small)%dat
    cseq  = self%kvs%bk(kvloc)%c
    get_crit_digit = get_crit_pos(bseq, cseq)
  end function get_crit_digit

  recursive subroutine retrieve(self, bseq, node0, near, cpos)
    class(assoc), intent(inout) :: self
    character, intent(in) :: bseq(:)
    integer, intent(in) :: node0
    integer, intent(out) :: near, cpos
    character, allocatable :: cutseq(:)
    integer :: node, crit, next
    if(0.eq.node0) then
      node = self%cbt%root
    else
      node = node0
    end if
    near = node
    crit = -self%cbt%t(node)%dat
    if(0.eq.crit) return
    cpos = get_crit_digit(self, bseq, node)
    if(0.gt.crit) return
    if(0.lt.cpos .and. cpos.lt.crit) return
    if('0'.eq.bseq(crit)) then
      next = self%cbt%t(node)%n0
    else
      next = self%cbt%t(node)%n1
    end if
    call self%retrieve(bseq, next, near, cpos)
  end subroutine retrieve

  logical function is_same_key(self, bseq, node)
    class(assoc), intent(inout) :: self
    character, intent(in) :: bseq(:)
    integer, intent(in) :: node
    integer :: w
    is_same_key = .false.
    w = self%cbt%t(node)%dat
    if(0.ge.w) return
    if(all(bseq.eq.self%kvs%bk(w)%c)) is_same_key = .true.
  end function is_same_key

  subroutine put(self, k, v)
    class(assoc), intent(inout) :: self
    character(*), intent(in) :: k, v
    integer :: src, node, new, up, kvloc, cpos
    character, allocatable :: bseq(:)
    character :: cbit

    bseq = str_to_bit(k)
    call self%cbt%acquire(new)

    if(0.ne.self%cbt%t(self%cbt%root)%dat) then
      call self%retrieve(bseq,0,src,cpos)
      if(self%is_same_key(bseq, src)) then
        call self%cbt%release(new)
        self%kvs%v(self%cbt%t(src)%dat)%c = split(v)
        return
      end if
      call self%cbt%acquire(node)
      up = self%cbt%t(src)%up
      cbit = bseq(cpos)
      if(0.eq.up) then
        self%cbt%root = node
      else if(src.eq.self%cbt%t(up)%n0) then
        self%cbt%t(up)%n0 = node
      else
        self%cbt%t(up)%n1 = node
      end if
      if('0'.eq.cbit) then
        self%cbt%t(node)%n0 = new
        self%cbt%t(node)%n1 = src
      else
        self%cbt%t(node)%n0 = src
        self%cbt%t(node)%n1 = new
      end if
      self%cbt%t(node)%up = up
      self%cbt%t(node)%dat = -cpos
      self%cbt%t(src)%up = node
      self%cbt%t(new)%up = node
    end if

    call self%kvs%acquire(kvloc)
    self%cbt%t(new)%dat = kvloc
    self%kvs%sk(kvloc)%c = split(k)
    self%kvs%bk(kvloc)%c = bseq
    self%kvs%v(kvloc)%c = split(v)
  end subroutine put

  subroutine del(self, key)
    class(assoc), intent(inout) :: self
    character(*), intent(in) :: key
    character, allocatable :: bseq(:)
    integer :: leaf, kvloc, up, upup, root, fellow, cpos

    bseq = str_to_bit(key)
    root = self%cbt%root
    call self%retrieve(bseq,0,leaf,cpos)
    kvloc = self%cbt%t(leaf)%dat
    if(0.ge.kvloc) return
    if(any(bseq.ne.self%kvs%bk(kvloc)%c)) return
    if(0.gt.self%cbt%t(root)%dat) then
      up = self%cbt%t(leaf)%up
      if(leaf.eq.self%cbt%t(up)%n0) then
        fellow = self%cbt%t(up)%n1
      else
        fellow = self%cbt%t(up)%n0
      end if
      if(root.eq.up) then
        self%cbt%root = fellow
        self%cbt%t(fellow)%up = 0
      else
        upup = self%cbt%t(up)%up
        if(up.eq.self%cbt%t(upup)%n0) then
          self%cbt%t(upup)%n0 = fellow
        else
          self%cbt%t(upup)%n1 = fellow
        end if
        self%cbt%t(fellow)%up = upup
      end if
      call self%cbt%release(up)
    end if
    call self%cbt%release(leaf)
    call self%kvs%release(kvloc)
  end subroutine del

  function get(self, key)
    class(assoc), intent(inout) :: self
    character(*), intent(in) :: key
    character(:), allocatable :: get
    call get1(self, key, get)
  end function get

  logical function have(self, key)
    class(assoc), intent(inout) :: self
    character(*), intent(in) :: key
    character(:), allocatable :: val
    call get1(self, key, val, have)
  end function have

  subroutine get1(self, key, val, exist)
    class(assoc), intent(inout) :: self
    character(*), intent(in) :: key
    character, allocatable :: bseq(:)
    character(:), allocatable :: val
    logical, optional :: exist
    integer :: near, kvloc, cpos
    bseq = str_to_bit(key)
    call self%retrieve(bseq,0,near,cpos)
    kvloc = self%cbt%t(near)%dat
    if(present(exist)) exist = .false.
    if(0.ge.kvloc) return
    if(all(bseq.eq.self%kvs%bk(kvloc)%c)) then
      if(present(exist)) exist = .true.
      val = join(self%kvs%v(kvloc)%c)
    end if
  end subroutine get1

  subroutine keys(self, v)
    class(assoc), intent(inout) :: self
    type(strarray), intent(out), allocatable :: v(:)
    integer, allocatable :: ar(:)
    integer :: n, i=1
    n = -1+self%kvs%ind
    allocate(v(n), ar(n))
    call push_key(self%cbt,self%cbt%root,ar,i)
    do i=1,n
      v(i) = self%kvs%sk(self%cbt%t(ar(i))%dat)
    end do
  end subroutine keys

  recursive subroutine push_key(cbt, k, ar, i)
    type(trie), intent(in) :: cbt
    integer, intent(in) :: k
    integer, intent(inout) :: ar(:), i
    if(0.lt.cbt%t(k)%dat) then
      ar(i) = k
      i = 1+i
    else
      call push_key(cbt, cbt%t(k)%n0, ar, i)
      call push_key(cbt, cbt%t(k)%n1, ar, i)
    end if
  end subroutine push_key
end module assoc_critbit_trie














