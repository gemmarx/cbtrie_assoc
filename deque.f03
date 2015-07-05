
module class_deque
  use util_string_array
  implicit none
  private
  type, public :: deque
    type(strarray), allocatable :: que(:)
    integer :: head, tail, last
  contains
    procedure :: init, dump, expand, clip, push, pop, shift
  end type deque

contains
  subroutine init(self, n0)
    class(deque), intent(inout) :: self
    integer, intent(in) :: n0
    integer :: n
    self%head = 0
    self%tail = 0
    if(allocated(self%que)) deallocate(self%que)
    n=n0; if(2.gt.n) n=2
    allocate(self%que(n))
    self%last = n
  end subroutine init

  subroutine dump(self)
    class(deque), intent(inout) :: self
    integer :: i
    print *, 'head tail last'
    print *, self%head, self%tail, self%last
    if(0.eq.self%head) return
    do i=self%tail, self%head
      write(*, '(i0,a)', advance='no') i, '  '
      print *, self%que(i)%get()
    end do
  end subroutine dump

  subroutine expand(self)
    class(deque), intent(inout) :: self
    type(strarray), allocatable :: tmp(:)
    integer :: h, t, m, n
    h=self%head; t=self%tail
    m = 1+h-t
    n = size(self%que)
    if(m.lt.n/2) then
      self%que(1:m) = self%que(t:h)
    else
      allocate(tmp(2*n))
      tmp(1:m) = self%que(t:h)
      self%que = tmp
      self%last = 2*n
    end if
    self%head = m
    self%tail = 1
  end subroutine expand

  subroutine clip(self)
    class(deque), intent(inout) :: self
    type(strarray), allocatable :: tmp(:)
    integer :: h, t, m, n
    h=self%head; t=self%tail
    m = 1+h-t
    allocate(tmp(2*m))
    if(0.ne.h) then
      tmp(1:m) = self%que(t:h)
      self%head = m
      self%tail = 1
    end if
    self%que = tmp
    self%last = 2*m
  end subroutine clip

  subroutine push(self, v)
    class(deque), intent(inout) :: self
    character(*), intent(in) :: v
    integer :: i
    if(self%last.eq.1+self%head) call self%expand
    i = 1+self%head
    call self%que(i)%put(v)
    self%head = i
    if(0.eq.self%tail) self%tail = 1
  end subroutine push

  subroutine pop(self, v)
    class(deque), intent(inout) :: self
    character(:), intent(out), allocatable :: v
    integer :: i
    i = self%head
    if(0.eq.i) return
    v = self%que(i)%get()
    if(i.eq.self%tail) then
      self%head = 0
      self%tail = 0
    else
      self%head = -1+i
    end if
  end subroutine pop

  subroutine shift(self, v)
    class(deque), intent(inout) :: self
    character(:), intent(out), allocatable :: v
    integer :: i
    i = self%tail
    if(0.eq.i) return
    v = self%que(i)%get()
    if(i.eq.self%head) then
      self%head = 0
      self%tail = 0
    else
      self%tail = 1+i
    end if
  end subroutine shift
end module class_deque



