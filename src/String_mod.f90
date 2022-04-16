module String_mod
    implicit none
contains
    subroutine getLowerCase(StrVec,StrVecLowerCase,lenStrVec) bind(C, name="getLowerCase")
        use, intrinsic :: iso_c_binding, only: c_char, c_null_char, c_size_t
        use, intrinsic :: iso_fortran_env, only: IK => int32
        integer(c_size_t), intent(in), value        :: lenStrVec
        character(len=1,kind=C_char), intent(in)    :: StrVec(lenStrVec)
        character(len=1,kind=C_char), intent(inout) :: StrVecLowerCase(lenStrVec)
        integer(IK), parameter                      :: duc = ichar('A') - ichar('a')
        character                                   :: ch
        integer(IK)                                 :: i
        write(*,"(*(g0))") "From Inside Fortran@getLowerCase(): StrVec = ", (StrVec(i),i=1,lenStrVec)
        write(*,"(*(g0))") "From Inside Fortran@getLowerCase(): lenStrVec = ", lenStrVec
        do i = 1, lenStrVec
            ch = StrVec(i)
            if (ch>='A' .and. ch<='Z') ch = char(ichar(ch)-duc)
            StrVecLowerCase(i) = ch
        end do
    end subroutine getLowerCase
end module String_mod