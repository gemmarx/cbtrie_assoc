
module class_string
    use util_converter
    implicit none
    private
    character, parameter :: &
        DELIMITER(1:4)=[' ', achar(9), achar(10), achar(13)]

    type, public :: string
        character(:), allocatable :: c
    contains
        procedure :: drop, put, get, chars, uc, lc, &
            words, get_real
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

    real function get_real(self)
        class(string), intent(in) :: self
        read(self%c, *), get_real
    end function get_real

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

    ! words(ws)
    !   get all words
    ! words(ws, n)
    !   get n words from head
    function words(self, n)
        class(string), intent(in) :: self
        integer, intent(in), optional :: n
        integer :: i, j, limit, str_end, word_cnt
        type(string), allocatable :: words(:)

        str_end = len_trim(self%c)
        limit=0 ; if(present(n)) limit=n

        allocate(words(0))
        j=0 ; word_cnt=0
        do i=1,str_end
            if(present(n) .and. limit.le.word_cnt) exit
            if(j.gt.i) cycle
            if(any(DELIMITER.eq.(self%c(i:i)))) cycle
            do j=1+i,str_end
                if(any(DELIMITER.eq.(self%c(j:j)))) exit
            end do
            word_cnt = 1 + word_cnt
            words = [words, string(self%c(i:-1+j))]
        end do
    end function words

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

