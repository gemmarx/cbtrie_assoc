
module class_string
    use util_converter
    implicit none
    private
    character, parameter :: &
        DELIMITER(1:4)=[' ', char(9), char(10), char(13)]

    type, public :: string
        character(:), private, allocatable :: c
    contains
        procedure :: drop, put, get, length, chars, uc, lc, &
            nword, words, get_real
    end type string

contains
    subroutine drop(self)
        class(string), intent(inout) :: self
        if(allocated(self%c)) deallocate(self%c)
    end subroutine drop

    subroutine put(self, src)
        class(string), intent(inout) :: self
        class(*), intent(in) :: src
        self%c = ntos(src)
    end subroutine put

    function get(self)
        class(string), intent(in) :: self
        character :: get*(len(self%c))
        get = self%c
    end function get

    subroutine put_num(self, num)
        class(string), intent(inout) :: self
        class(*), intent(in) :: num
        self%c = ntos(num)
    end subroutine put_num

    real function get_real(self)
        class(string), intent(in) :: self
        read(self%c, *), get_real
    end function get_real

    integer function length(self)
        class(string), intent(in) :: self
        length = len(self%c)
    end function length

    function uc(self)
        class(string), intent(in) :: self
        integer :: i
        character :: uc*(len(self%c))
        uc = chars_to_str(to_upper( &
                [(self%c(i:i), i=1, len(self%c))] &
             ))
    end function uc

    function lc(self)
        class(string), intent(in) :: self
        integer :: i
        character :: lc*(len(self%c))
        lc = chars_to_str(to_lower( &
                [(self%c(i:i), i=1, len(self%c))] &
             ))
    end function lc

    function chars(self)
        class(string), intent(in) :: self
        character :: chars(len(self%c))
        integer :: i
        chars = [(self%c(i:i), i=1, len(self%c))]
    end function chars

    integer function nword(self)
        class(string), intent(in) :: self
        integer :: i,j,n
        n = len(self%c)
        j=0 ; nword=0
        do i=1,n
            if(j.gt.i) cycle
            if(any(DELIMITER.eq.(self%c(i:i)))) cycle
            do j=1+i,n
                if(any(DELIMITER.eq.(self%c(j:j)))) exit
            end do
            nword = 1 + nword
        end do
    end function nword

    ! words(ws)
    !   gets all words into ws
    !   needed size of ws will be got through nword()
    !       allocate(ws(str%nword()))
    !       call str%words(ws)
    ! words(ws, n)
    !   (n>0) => gets n words from head
    !   (n=0) => gets all words
    !   and, the number of words got indeed is given in n
    subroutine words(self, ws, n)
        class(string), intent(in) :: self
        type(string), intent(out) :: ws(:)
        integer, intent(inout), optional :: n
        integer :: i, j, limit, str_end, word_cnt

        str_end = len_trim(self%c)
        limit = size(ws)
        if(present(n)) then
            if(0.lt.n .and. n.lt.limit) limit=n
        end if

        j=0 ; word_cnt=0
        do i=1,str_end
            if(limit.le.word_cnt) exit
            if(j.gt.i) cycle
            if(any(DELIMITER.eq.(self%c(i:i)))) cycle
            do j=1+i,str_end
                if(any(DELIMITER.eq.(self%c(j:j)))) exit
            end do
            word_cnt = 1 + word_cnt
            ws(word_cnt)%c = self%c(i:-1+j)
        end do
        if(present(n)) n = word_cnt
    end subroutine words

    function chars_to_str(arr)
        character, intent(in) :: arr(:)
        character :: chars_to_str*(size(arr))
        integer :: i
        forall(i=1:size(arr)) chars_to_str(i:i) = arr(i)
    end function chars_to_str

    pure elemental character function to_upper(ch)
        character, intent(in) :: ch
        integer :: i
        to_upper = ch
        i = iachar(ch)
        if(97.le.i .and. i.le.122) to_upper = achar(-32+i)
    end function to_upper

    pure elemental character function to_lower(ch)
        character, intent(in) :: ch
        integer :: i
        to_lower = ch
        i = iachar(ch)
        if(65.le.i .and. i.le.90) to_lower = achar(32+i)
    end function to_lower
end module class_string

