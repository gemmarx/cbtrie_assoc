
module param_whole
  implicit none
  integer, parameter :: TRIE_SIZE=32  !initial number of key-value pairs
  character(*), parameter :: DEFAULT_CASE_ORDER='ASCII' !default sort order of keys
                           !'MESH', 'ASCII' or 'IGNORE' can be selected.
end module param_whole

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
    procedure :: init, expand, part_smallest, part_greatest, next, prev
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

  recursive integer function part_smallest(self, node)
    class(trie), intent(in) :: self
    integer, intent(in) :: node
    if(0.le.self%t(node)%dat) then
      part_smallest = node
    else
      part_smallest = self%part_smallest(self%t(node)%n0)
    end if
  end function part_smallest

  recursive integer function part_greatest(self, node)
    class(trie), intent(in) :: self
    integer, intent(in) :: node
    if(0.le.self%t(node)%dat) then
      part_greatest = node
    else
      part_greatest = self%part_greatest(self%t(node)%n1)
    end if
  end function part_greatest

  recursive integer function next(self, node)
    class(trie), intent(in) :: self
    integer, intent(in) :: node
    integer :: up
    next=0
    if(node.eq.self%root) return
    up = self%t(node)%up
    if(node.eq.self%t(up)%n0) then
      next = self%part_smallest(self%t(up)%n1)
    else
      next = self%next(up)
    end if
  end function next

  recursive integer function prev(self, node)
    class(trie), intent(in) :: self
    integer, intent(in) :: node
    integer :: up
    prev=0
    if(node.eq.self%root) return
    up = self%t(node)%up
    if(node.eq.self%t(up)%n1) then
      prev = self%part_greatest(self%t(up)%n0)
    else
      prev = self%prev(up)
    end if
  end function prev
end module class_trie

module class_kv_arrays
  use util_string_array
  use class_resource_pool
  implicit none
  private
  type, public, extends(respool) :: kvarrs
    type(strarray), allocatable :: sk(:), v(:)
    type(bytearray), allocatable :: bk(:)
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
    call expand1(self%v)
    call expand2(self%bk)
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

  subroutine expand2(a)
    type(bytearray), intent(inout), allocatable :: a(:)
    type(bytearray), allocatable :: tmp(:)
    integer :: n
    n = size(a)
    tmp = a
    deallocate(a)
    allocate(a(2*n))
    a(1:n) = tmp
  end subroutine expand2

  subroutine release(self, idx)
    class(kvarrs), intent(inout) :: self
    integer, intent(in) :: idx
    deallocate(self%sk(idx)%c)
    deallocate(self%bk(idx)%c)
    deallocate(self%v(idx)%c)
    call self%respool%release(idx)
  end subroutine release
end module class_kv_arrays

module assoc_critbit_trie
  use param_whole
  use util_string_array
  use intf_case_order
  use class_trie
  use class_kv_arrays
  implicit none
  private
  type, public :: assoc
    type(trie) :: cbt
    type(kvarrs) :: kvs
    procedure(corder), pointer, nopass :: cord => null()
  contains
    procedure :: init, fin, put, get, del, have, keys, dump, next, prev
    procedure, private :: get_crit_digit, retrieve, is_same_key, reorder_case
  end type assoc

contains
  subroutine init(self, case_order)
    class(assoc), intent(inout) :: self
    character(*), intent(in), optional :: case_order
    character(:), allocatable :: sp

    call self%cbt%init(2*TRIE_SIZE)
    call self%kvs%init(TRIE_SIZE)

    sp = DEFAULT_CASE_ORDER
    if(present(case_order)) sp = case_order
    sp = join(uc(split(sp)))
    select case(sp)
    case('ASCII')
      self%cord => no_reorder
    case('MESH')
      self%cord => mesh_case
    case('IGNORE')
      self%cord => ignore_case
    case default
      self%cord => no_reorder
    end select
  end subroutine init

  subroutine fin(self)
    class(assoc), intent(inout) :: self
    deallocate(self%cbt%que, self%cbt%t, self%kvs%que, self%kvs%sk, self%kvs%bk, self%kvs%v)
  end subroutine fin

  subroutine dump(self)
    class(assoc), intent(inout) :: self
    integer, allocatable :: ar(:)
    byte, allocatable :: br(:)
    integer :: n, i, j, g(5), cell

    write(*, '(a)', advance='no'), 'root node:  '
    print *, self%cbt%root
    n = -1+self%cbt%ind; i=1
    allocate(ar(n))
    call push_key(self%cbt,self%cbt%root,ar,i,.true.)
    print *, 'node dat n0 n1 up'
    do i=1, n
      g(1) = ar(i)
      g(2) = self%cbt%t(ar(i))%dat
      g(3) = self%cbt%t(ar(i))%n0
      g(4) = self%cbt%t(ar(i))%n1
      g(5) = self%cbt%t(ar(i))%up
      print *, g(1:5)
    end do
    n = -1+self%kvs%ind; i=1
    deallocate(ar)
    allocate(ar(n))
    call push_key(self%cbt,self%cbt%root,ar,i)
    print *, 'cell  key  value'
    do i=1, n
      cell = self%cbt%t(ar(i))%dat
      write(*, '(i0,a)', advance='no'), cell, '   '
      write(*, '(a)', advance='no'), self%kvs%sk(cell)%get()
      print *, '=> ', self%kvs%v(cell)%get()
      br = self%kvs%bk(cell)%c
      do j=1, size(br)
        write(*, '(a)', advance='no') ' '
        write(*, '(a)', advance='no') byte_to_bitchar(br(j))
      end do
      print *, ""
    end do
  end subroutine

  integer function get_crit_digit(self, bseq, node)
    class(assoc), intent(in) :: self
    byte, intent(in) :: bseq(:)
    integer, intent(in) :: node
    byte, allocatable :: cseq(:)
    integer :: small, kvloc, bz, cz, i, k
    byte :: b, c, x
    small = self%cbt%part_smallest(node)
    kvloc = self%cbt%t(small)%dat
    cseq  = self%kvs%bk(kvloc)%c
    bz=size(bseq); cz=size(cseq)
    do k=1, max(bz, cz)
      b=bseq(k); if(bz.lt.k) b=0
      c=cseq(k); if(cz.lt.k) c=0
      x = ieor(b,c)
      do i=1, 8
        if(btest(x, 8-i)) then
          get_crit_digit = 8*(-1+k)+i
          return
        end if
      end do
    end do
    get_crit_digit = 0
  end function get_crit_digit

  logical function test_cbit(bseq, cpos)
    byte, intent(in) :: bseq(:)
    integer, intent(in) :: cpos
    integer :: m, n
    if(cpos.gt.8*size(bseq)) then
      test_cbit = .false.
      return
    end if
    m=(-1+cpos)/8
    n=mod(cpos,8); if(0.eq.n) n=8
    test_cbit = btest(bseq(1+m), 8-n)
  end function test_cbit

  elemental byte function reorder_case(self, i)
    class(assoc), intent(in) :: self
    byte, intent(in) :: i
    reorder_case = self%cord(i)
  end function reorder_case

  recursive subroutine retrieve(self, bseq, node0, near, cpos)
    class(assoc), intent(in) :: self
    byte, intent(in) :: bseq(:)
    integer, intent(in) :: node0
    integer, intent(out) :: near
    integer, intent(out) :: cpos
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
    if(test_cbit(bseq, crit)) then
      next = self%cbt%t(node)%n1
    else
      next = self%cbt%t(node)%n0
    end if
    call self%retrieve(bseq, next, near, cpos)
  end subroutine retrieve

  logical function is_same_key(self, bseq, node)
    class(assoc), intent(in) :: self
    byte, intent(in) :: bseq(:)
    integer, intent(in) :: node
    byte, allocatable :: cseq(:)
    integer :: w
    is_same_key = .false.
    w = self%cbt%t(node)%dat
    if(0.ge.w) return
    cseq = self%kvs%bk(w)%c
    if(size(bseq).ne.size(cseq)) return
    if(all(bseq.eq.cseq)) is_same_key = .true.
  end function is_same_key

  subroutine put(self, k, v)
    class(assoc), intent(inout) :: self
    character(*), intent(in) :: k, v
    integer :: src, node, new, up, kvloc, cpos
    byte, allocatable :: bseq(:)

    bseq = self%reorder_case(str_to_byte(k))
    call self%cbt%acquire(new)
    if(0.ne.self%cbt%t(self%cbt%root)%dat) then
      call self%retrieve(bseq,0,src,cpos)
      if(self%is_same_key(bseq, src)) then
        call self%cbt%release(new)
        kvloc = self%cbt%t(src)%dat
        call self%kvs%sk(kvloc)%put(k)
        call self%kvs%v(kvloc)%put(v)
        return
      end if
      call self%cbt%acquire(node)
      up = self%cbt%t(src)%up
      if(0.eq.up) then
        self%cbt%root = node
      else if(src.eq.self%cbt%t(up)%n0) then
        self%cbt%t(up)%n0 = node
      else
        self%cbt%t(up)%n1 = node
      end if
      if(test_cbit(bseq, cpos)) then
        self%cbt%t(node)%n0 = src
        self%cbt%t(node)%n1 = new
      else
        self%cbt%t(node)%n0 = new
        self%cbt%t(node)%n1 = src
      end if
      self%cbt%t(node)%up = up
      self%cbt%t(node)%dat = -cpos
      self%cbt%t(src)%up = node
      self%cbt%t(new)%up = node
    end if

    call self%kvs%acquire(kvloc)
    self%cbt%t(new)%n0 = 0
    self%cbt%t(new)%n1 = 0
    self%cbt%t(new)%dat = kvloc
    self%kvs%bk(kvloc)%c = bseq
    call self%kvs%sk(kvloc)%put(k)
    call self%kvs%v(kvloc)%put(v)
  end subroutine put

  subroutine del(self, key)
    class(assoc), intent(inout) :: self
    character(*), intent(in) :: key
    byte, allocatable :: bseq(:)
    integer :: leaf, kvloc, up, upup, root, fellow, cpos

    bseq = self%reorder_case(str_to_byte(key))
    root = self%cbt%root
    call self%retrieve(bseq,0,leaf,cpos)
    kvloc = self%cbt%t(leaf)%dat
    if(0.ge.kvloc) return
    if(.not.self%is_same_key(bseq,leaf)) return
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
    class(assoc), intent(in) :: self
    character(*), intent(in) :: key
    character(:), allocatable :: get
    call get1(self, key, val=get)
  end function get

  logical function have(self, key)
    class(assoc), intent(in) :: self
    character(*), intent(in) :: key
    call get1(self, key, have)
  end function have

  subroutine get1(self, key, exist, val)
    class(assoc), intent(in) :: self
    character(*), intent(in) :: key
    byte, allocatable :: bseq(:)
    logical, optional :: exist
    character(:), allocatable, optional :: val
    integer :: near, kvloc, dummy
    bseq = self%reorder_case(str_to_byte(key))
    call self%retrieve(bseq,0,near,dummy)
    kvloc = self%cbt%t(near)%dat
    if(present(exist)) exist = .false.
    if(self%is_same_key(bseq, near)) then
      if(present(exist)) exist = .true.
      if(present(val)) val = join(self%kvs%v(kvloc)%c)
    end if
  end subroutine get1

  subroutine keys(self, v)
    class(assoc), intent(in) :: self
    type(strarray), intent(out), allocatable :: v(:)
    integer, allocatable :: ar(:)
    integer :: n, i
    n = -1+self%kvs%ind; i=1
    allocate(v(n), ar(n))
    call push_key(self%cbt,self%cbt%root,ar,i)
    do i=1,n
      v(i) = self%kvs%sk(self%cbt%t(ar(i))%dat)
    end do
  end subroutine keys

  recursive subroutine push_key(cbt, k, ar, i, all_node)
    type(trie), intent(in) :: cbt
    integer, intent(in) :: k
    integer, intent(inout) :: ar(:), i
    logical, intent(in), optional :: all_node
    if(0.lt.cbt%t(k)%dat) then
      ar(i)=k; i=1+i
    else
      if(present(all_node)) then
        ar(i)=k; i=1+i
      end if
      call push_key(cbt, cbt%t(k)%n0, ar, i, all_node)
      call push_key(cbt, cbt%t(k)%n1, ar, i, all_node)
    end if
  end subroutine push_key

  function next(self, key)
    class(assoc), intent(in) :: self
    character(*), intent(in) :: key
    character(:), allocatable :: next
    byte, allocatable :: bseq(:)
    integer :: node, near, cpos
    bseq = self%reorder_case(str_to_byte(key))
    call self%retrieve(bseq,0,near,cpos)
    if(self%is_same_key(bseq, near) .or. test_cbit(bseq, cpos)) then
      node = self%cbt%next(near)
    else
      node = self%cbt%part_smallest(near)
    end if
    next=""
    if(0.eq.node) return
    next = join(self%kvs%sk(self%cbt%t(node)%dat)%c)
  end function next

  function prev(self, key)
    class(assoc), intent(in) :: self
    character(*), intent(in) :: key
    character(:), allocatable :: prev
    byte, allocatable :: bseq(:)
    integer :: node, near, cpos
    bseq = self%reorder_case(str_to_byte(key))
    call self%retrieve(bseq,0,near,cpos)
    if(self%is_same_key(bseq, near) .or. .not.test_cbit(bseq, cpos)) then
      node = self%cbt%prev(near)
    else
      node = self%cbt%part_greatest(near)
    end if
    prev=""
    if(0.eq.node) return
    prev = join(self%kvs%sk(self%cbt%t(node)%dat)%c)
  end function prev
end module assoc_critbit_trie








