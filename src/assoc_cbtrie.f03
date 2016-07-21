
module param_whole
    implicit none
    integer, parameter :: TRIE_SIZE=32  !initial number of key-value pairs
end module param_whole

module class_resource_pool
    implicit none
    private

    type, public :: respool
        integer, allocatable :: que(:)
        integer :: ind, last
    contains
        procedure :: init, drop, acquire, release, expand
    end type respool

contains
    subroutine init(self, num)
        class(respool), intent(inout) :: self
        integer, intent(in) :: num
        integer :: i
        if(allocated(self%que)) deallocate(self%que)
        self%que = ([(i,i=1,num)])
        self%ind = 1
        self%last = num
    end subroutine init

    subroutine drop(self)
        class(respool), intent(inout) :: self
        if(allocated(self%que)) deallocate(self%que)
        self%ind = 0
        self%last = 0
    end subroutine drop

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
        procedure :: init, drop, expand, part_smallest, part_greatest, next, prev
    end type trie

contains
    subroutine init(self, num)
        class(trie), intent(inout) :: self
        integer, intent(in) :: num
        integer :: i
        if(allocated(self%t)) call self%drop
        allocate(self%t(num))
        do i=1,num
            self%t(i)%n0 = 0
            self%t(i)%n1 = 0
            self%t(i)%up = 0
            self%t(i)%dat = 0
        end do
        self%root = 1
        call self%respool%init(num)
    end subroutine init

    subroutine drop(self)
        class(trie), intent(inout) :: self
        if(allocated(self%t)) deallocate(self%t)
        self%root = 0
        call self%respool%drop
    end subroutine drop

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
    use class_typack
    use class_resource_pool
    implicit none
    private

    type, public, extends(respool) :: kvarrs
        type(typack), allocatable :: bk(:), bv(:)
    contains
        procedure :: init, drop, expand, release
    end type kvarrs

contains
    subroutine init(self, num)
        class(kvarrs), intent(inout) :: self
        integer, intent(in) :: num
        if(allocated(self%bk)) call self%drop
        allocate(self%bk(num), self%bv(num))
        call self%respool%init(num)
    end subroutine init

    subroutine drop(self)
        class(kvarrs), intent(inout) :: self
        if(allocated(self%bk)) deallocate(self%bk)
        if(allocated(self%bv)) deallocate(self%bv)
        call self%respool%drop
    end subroutine drop

    subroutine expand(self)
        class(kvarrs), intent(inout) :: self
        call expand1(self%bk)
        call expand1(self%bv)
        call self%respool%expand
    end subroutine expand

    subroutine expand1(a)
        type(typack), intent(inout), allocatable :: a(:)
        type(typack), allocatable :: tmp(:)
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
        call self%bk(idx)%drop
        call self%bv(idx)%drop
        call self%respool%release(idx)
    end subroutine release
end module class_kv_arrays

module class_assoc_cbtrie
    use param_whole
    use class_typack
    use class_trie
    use class_kv_arrays
    implicit none
    private

    type, public :: assoc
        type(trie) :: cbt
        type(kvarrs) :: kvs
    contains
        procedure :: init, drop, put, del, have, keys, dump, defrag, &
            next, prev, first, last, get_type, nelm
        procedure, private :: get_crit_digit, retrieve, is_same_key
        procedure, private :: get_obj, get_str, get_real, get_double, &
            get_integer, get_complex, get_dcomplex
        generic :: get => get_obj, get_str, get_real, get_double, &
            get_integer, get_complex, get_dcomplex
    end type assoc

contains
    subroutine init(self)
        class(assoc), intent(inout) :: self
        call self%cbt%init(2*TRIE_SIZE)
        call self%kvs%init(TRIE_SIZE)
    end subroutine init

    subroutine drop(self)
        class(assoc), intent(inout) :: self
        call self%cbt%drop
        call self%kvs%drop
    end subroutine drop

    subroutine dump(self)
        class(assoc), intent(in) :: self
        integer, allocatable :: ar(:)
        byte, allocatable :: br(:)
        integer :: n, i, j, g(5), cell

        if(0.eq.self%nelm()) then
            print *, 'Empty'
            return
        end if
        print *, 'root node:', self%cbt%root
        n = -1 + self%cbt%ind
        allocate(ar(n))
        i=1 ; call push_key(self%cbt,self%cbt%root,ar,i,.true.)
        print *, 'node dat n0 n1 up'
        do i=1, n
            g(1) = ar(i)
            g(2) = self%cbt%t(ar(i))%dat
            g(3) = self%cbt%t(ar(i))%n0
            g(4) = self%cbt%t(ar(i))%n1
            g(5) = self%cbt%t(ar(i))%up
            print *, g(1:5)
        end do
        n = -1 + self%kvs%ind
        deallocate(ar)
        allocate(ar(n))
        i=1 ; call push_key(self%cbt,self%cbt%root,ar,i)
        print *, 'cell  key  value'
        do i=1, n
            cell = self%cbt%t(ar(i))%dat
            write(*, '(i0,2a)', advance='no'), cell, '   ', &
                                               self%kvs%bk(cell)%get_str()
            print *, '=> ', self%kvs%bv(cell)%get_str()
            br = self%kvs%bk(cell)%get()
            do j=1, size(br)
                write(*, '(2a)', advance='no') ' ', byte_to_bitchar(br(j))
            end do
            print *
        end do
    end subroutine dump

    character(8) function byte_to_bitchar(byte)
        byte, intent(in) :: byte
        write(byte_to_bitchar, '(b8.8)') byte
    end function byte_to_bitchar

    integer function get_crit_digit(self, bseq, node)
        class(assoc), intent(in) :: self
        byte, intent(in) :: bseq(:)
        integer, intent(in) :: node
        byte, allocatable :: cseq(:)
        integer :: small, kvloc, bz, cz, i, k
        byte :: b, c, x
        small = self%cbt%part_smallest(node)
        kvloc = self%cbt%t(small)%dat
        cseq  = self%kvs%bk(kvloc)%get()
        bz=size(bseq); cz=size(cseq)
        do k=1, max(bz, cz)
            b=0; if(bz.ge.k) b=bseq(k)
            c=0; if(cz.ge.k) c=cseq(k)
            x = ieor(b,c)
            do i=7, 0, -1
                if(btest(x,i)) then
                    get_crit_digit = -i + 8*k
                    return
                end if
            end do
        end do
        get_crit_digit = 0
    end function get_crit_digit

    logical function test_cbit(bseq, cpos)
        byte, intent(in) :: bseq(:)
        integer, intent(in) :: cpos
        integer :: p, m, n
        if(cpos.gt.8*size(bseq)) then
            test_cbit = .false.
            return
        end if
        p = -1 + cpos
        m = 1 + p/8
        n = 7 -mod(p,8)
        test_cbit = btest(bseq(m),n)
    end function test_cbit

    recursive subroutine retrieve(self, bseq, node0, near, cpos0)
        class(assoc), intent(in) :: self
        byte, intent(in) :: bseq(:)
        integer, intent(in) :: node0
        integer, intent(out) :: near
        integer, intent(out), optional :: cpos0
        integer :: node, crit, next, cpos
        if(0.eq.node0) then
            node = self%cbt%root
        else
            node = node0
        end if
        near = node
        crit = -self%cbt%t(node)%dat
        if(0.eq.crit) return
        cpos = get_crit_digit(self, bseq, node)
        if(present(cpos0)) cpos0 = cpos
        if(0.gt.crit) return
        if(0.lt.cpos .and. cpos.lt.crit) return
        if(test_cbit(bseq, crit)) then
            next = self%cbt%t(node)%n1
        else
            next = self%cbt%t(node)%n0
        end if
        call self%retrieve(bseq, next, near, cpos0)
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
        cseq = self%kvs%bk(w)%get()
        if(size(bseq).ne.size(cseq)) return
        if(all(bseq.eq.cseq)) is_same_key = .true.
    end function is_same_key

    function pack_or_get(v)
        class(*), intent(in) :: v
        byte, allocatable :: pack_or_get(:)
        type(typack) :: tpk
        select type(v)
        class is(typack)
            pack_or_get = v%get()
        class default
            call tpk%enpack(v)
            pack_or_get = tpk%get()
        end select
    end function pack_or_get

    subroutine put(self, k, v)
        class(assoc), intent(inout) :: self
        class(*), intent(in) :: k, v
        integer :: src, node, new, up, kvloc, cpos
        byte, allocatable :: bseq(:)
        
        bseq = pack_or_get(k)
        call self%cbt%acquire(new)
        if(0.ne.self%cbt%t(self%cbt%root)%dat) then
            call self%retrieve(bseq,0,src,cpos)
            if(self%is_same_key(bseq, src)) then
                call self%cbt%release(new)
                kvloc = self%cbt%t(src)%dat
                call self%kvs%bv(kvloc)%enpack(v)
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
        call self%kvs%bk(kvloc)%put(bseq)
        call self%kvs%bv(kvloc)%put(pack_or_get(v))
    end subroutine put

    subroutine del(self, key)
        class(assoc), intent(inout) :: self
        class(*), intent(in) :: key
        byte, allocatable :: bseq(:)
        integer :: leaf, kvloc, up, upup, root, fellow, cpos

        if(0.eq.self%nelm()) return
        bseq = pack_or_get(key)
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

        if(0.eq.self%nelm()) call self%init
        if(TRIE_SIZE.lt.self%kvs%ind .and. &
           4*self%kvs%ind.lt.self%kvs%last) call self%defrag
    end subroutine del

    function get_type(self, key)
        class(assoc), intent(in) :: self
        class(*), intent(in) :: key
        logical :: being
        character(:), allocatable :: get_type
        type(typack) :: bv
        call get1(self, key, being, bv)
        if(being) get_type = bv%get_type()
    end function get_type

    type(typack) function get_obj(self, key, mold)
        class(assoc), intent(in) :: self
        class(*), intent(in) :: key
        type(typack), intent(in) :: mold
        logical :: being
        call get1(self, key, val=get_obj)
    end function get_obj

    integer function get_integer(self, key, mold)
        class(assoc), intent(in) :: self
        class(*), intent(in) :: key
        integer :: mold
        logical :: being
        type(typack) :: bv
        call get1(self, key, being, bv)
        if(being) get_integer = bv%depack(mold)
    end function get_integer

    real function get_real(self, key, mold)
        class(assoc), intent(in) :: self
        class(*), intent(in) :: key
        real :: mold
        logical :: being
        type(typack) :: bv
        call get1(self, key, being, bv)
        if(being) get_real = bv%depack(mold)
    end function get_real

    double precision function get_double(self, key, mold)
        class(assoc), intent(in) :: self
        class(*), intent(in) :: key
        double precision :: mold
        logical :: being
        type(typack) :: bv
        call get1(self, key, being, bv)
        if(being) get_double = bv%depack(mold)
    end function get_double

    complex function get_complex(self, key, mold)
        class(assoc), intent(in) :: self
        class(*), intent(in) :: key
        complex :: mold
        logical :: being
        type(typack) :: bv
        call get1(self, key, being, bv)
        if(being) get_complex = bv%depack(mold)
    end function get_complex

    complex(kind(0d0)) function get_dcomplex(self, key, mold)
        class(assoc), intent(in) :: self
        class(*), intent(in) :: key
        complex(kind(0d0)) :: mold
        logical :: being
        type(typack) :: bv
        call get1(self, key, being, bv)
        if(being) get_dcomplex = bv%depack(mold)
    end function get_dcomplex

    function get_str(self, key, char_mold)
        class(assoc), intent(in) :: self
        class(*), intent(in) :: key
        character(*), intent(in), optional :: char_mold
        logical :: being
        character(:), allocatable :: get_str
        type(typack) :: bv
        call get1(self, key, being, bv)
        if(being) get_str = bv%get_str()
    end function get_str

    logical function have(self, key)
        class(assoc), intent(in) :: self
        class(*), intent(in) :: key
        call get1(self, key, have)
    end function have

    subroutine get1(self, key, being, val)
        class(assoc), intent(in) :: self
        class(*), intent(in) :: key
        byte, allocatable :: bseq(:)
        logical, optional :: being
        type(typack), optional :: val
        integer :: near, kvloc
        
        if(0.eq.self%nelm()) then
            if(present(being)) being = .false.
            if(present(val)) call val%drop
            return
        end if
        bseq = pack_or_get(key)
        call self%retrieve(bseq,0,near)
        kvloc = self%cbt%t(near)%dat
        if(present(being)) being = .false.
        if(self%is_same_key(bseq, near)) then
            if(present(being)) being = .true.
            if(present(val)) val = self%kvs%bv(kvloc)
        end if
    end subroutine get1

    integer function nelm(self)
        class(assoc), intent(in) :: self
        nelm = -1 + self%kvs%ind
    end function nelm

    subroutine keys(self, v)
        class(assoc), intent(in) :: self
        type(typack), intent(out), allocatable :: v(:)
        integer, allocatable :: ar(:)
        integer :: n, i
        n = -1+self%kvs%ind; i=1
        if(allocated(v)) deallocate(v)
        allocate(v(n), ar(n))
        call push_key(self%cbt,self%cbt%root,ar,i)
        do i=1,n
            call v(i)%put(self%kvs%bk(self%cbt%t(ar(i))%dat)%get())
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
        class(*), intent(in) :: key
        type(typack) :: next
        byte, allocatable :: bseq(:)
        integer :: node, near, cpos
        
        call next%drop
        if(0.eq.self%nelm()) return
        bseq = pack_or_get(key)
        call self%retrieve(bseq,0,near,cpos)
        if(self%is_same_key(bseq, near) .or. test_cbit(bseq, cpos)) then
            node = self%cbt%next(near)
        else
            node = self%cbt%part_smallest(near)
        end if
        if(0.eq.node) return
        call next%put(self%kvs%bk(self%cbt%t(node)%dat)%get())
    end function next

    function prev(self, key)
        class(assoc), intent(in) :: self
        class(*), intent(in) :: key
        type(typack) :: prev
        byte, allocatable :: bseq(:)
        integer :: node, near, cpos
        
        call prev%drop
        if(0.eq.self%nelm()) return
        bseq = pack_or_get(key)
        call self%retrieve(bseq,0,near,cpos)
        if(self%is_same_key(bseq, near) .or. .not.test_cbit(bseq, cpos)) then
            node = self%cbt%prev(near)
        else
            node = self%cbt%part_greatest(near)
        end if
        if(0.eq.node) return
        call prev%put(self%kvs%bk(self%cbt%t(node)%dat)%get())
    end function prev

    function first(self)
        class(assoc), intent(in) :: self
        type(typack) :: first
        integer :: root, node
        root = self%cbt%root
        call first%drop
        if(0.eq.self%nelm()) return
        node = self%cbt%part_smallest(root)
        call first%put(self%kvs%bk(self%cbt%t(node)%dat)%get())
    end function first

    function last(self)
        class(assoc), intent(in) :: self
        type(typack) :: last
        integer :: root, node
        root = self%cbt%root
        call last%drop
        if(0.eq.self%nelm()) return
        node = self%cbt%part_greatest(root)
        call last%put(self%kvs%bk(self%cbt%t(node)%dat)%get())
    end function last

    subroutine defrag(self)
        class(assoc), intent(inout) :: self
        integer :: i,n,kvloc
        integer, allocatable :: ar(:)
        type(typack), allocatable :: ks(:), vs(:)

        n = self%nelm()
        allocate(ar(n),ks(n),vs(n))
        i=1 ; call push_key(self%cbt,self%cbt%root,ar,i)
        do i=1,n
            kvloc = self%cbt%t(ar(i))%dat
            call ks(i)%put(self%kvs%bk(kvloc)%get())
            call vs(i)%put(self%kvs%bv(kvloc)%get())
        end do

        call self%drop
        call self%init
        do i=1,n
            call self%put(ks(i),vs(i))
        end do
    end subroutine defrag
end module class_assoc_cbtrie

