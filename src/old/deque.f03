
module class_deque
    use class_string
    implicit none
    private

    character, parameter :: NULLCHAR=char(0)

    type, public :: deque
        type(string), private, allocatable :: que(:)
        integer :: head, tail, last
    contains
        procedure :: &
            init, drop, dump, expand, clip, &
            push, pop, shift, nelm, is_empty
    end type deque

contains
    subroutine init(self, n0)
        class(deque), intent(inout) :: self
        integer, intent(in), optional :: n0
        integer :: n
        self%head = 0
        self%tail = 0
        if(allocated(self%que)) deallocate(self%que)
        n = 2
        if(present(n0)) then
            if(0.lt.n0) n = n0
        end if
        allocate(self%que(n))
        self%last = n
    end subroutine init

    subroutine drop(self)
        class(deque), intent(inout) :: self
        self%head = 0
        self%tail = 0
        self%last = 0
        if(allocated(self%que)) deallocate(self%que)
    end subroutine drop

    subroutine dump(self)
        class(deque), intent(in) :: self
        integer :: i
        print *, 'head tail last'
        print *, self%head, self%tail, self%last
        if(0.eq.self%head) return
        do i=self%tail, self%head
            write(*, '(i0,a)', advance='no') i, '  '
            print *, self%que(i)%get()
        end do
    end subroutine dump

    logical function is_empty(self)
        class(deque), intent(in) :: self
        is_empty = .false.
        if(0.eq.self%nelm()) is_empty = .true.
    end function is_empty

    integer function nelm(self)
        class(deque), intent(in) :: self
        nelm = 1 - self%tail + self%head
        if(0.eq.self%head) nelm=0
    end function nelm

    subroutine expand(self)
        class(deque), intent(inout) :: self
        type(string), allocatable :: tmp(:)
        integer :: h, t, m, n
        h=self%head; t=self%tail
        m = 1+h-t
        n = size(self%que)
        if(m.lt.n/2) then
            self%que(1:m) = self%que(t:h)
        else
            tmp = self%que(t:h)
            deallocate(self%que)
            allocate(self%que(2*n))
            self%que(1:m) = tmp
            self%last = 2*n
        end if
        self%head = m
        self%tail = 1
    end subroutine expand

    subroutine clip(self)
        class(deque), intent(inout) :: self
        type(string), allocatable :: tmp(:)
        integer :: h, t, m, n
        h=self%head ; t=self%tail
        m = 1+h-t
        if(0.ne.h) tmp = self%que(t:h)
        deallocate(self%que)
        allocate(self%que(2*m))
        if(0.ne.h) then
            self%que(1:m) = tmp
            self%head = m
            self%tail = 1
        end if
        self%last = 2*m
    end subroutine clip

    subroutine push(self, v)
        class(deque), intent(inout) :: self
        character(*), intent(in) :: v
        integer :: i
        if(self%last.le.1+self%head) call self%expand
        i = 1+self%head
        call self%que(i)%put(v)
        self%head = i
        if(0.eq.self%tail) self%tail = 1
    end subroutine push

    subroutine pop(self, v)
        class(deque), intent(inout) :: self
        character(:), intent(out), allocatable :: v
        integer :: i
        real :: p,r

        i = self%head
        if(0.eq.i) then
            v = NULLCHAR
            return
        end if

        v = self%que(i)%get()
        if(i.eq.self%tail) then
            self%head = 0
            self%tail = 0
        else
            self%head = -1+i
        end if

        r = 1.0/(10 + 0.5*self%nelm())
        call random_number(p)
        if(r.gt.p) call self%clip
    end subroutine pop

    subroutine shift(self, v)
        class(deque), intent(inout) :: self
        character(:), intent(out), allocatable :: v
        integer :: i
        real :: p,r

        i = self%tail
        if(0.eq.i) then
            v = NULLCHAR
            return
        end if
        v = self%que(i)%get()
        if(i.eq.self%head) then
            self%head = 0
            self%tail = 0
        else
            self%tail = 1+i
        end if

        r = 1.0/(10 + 0.5*self%nelm())
        call random_number(p)
        if(r.gt.p) call self%clip
    end subroutine shift
end module class_deque



