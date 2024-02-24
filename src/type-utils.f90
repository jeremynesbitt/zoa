
module type_utils
! THis module should only contain methods that have no dependencies
    
    contains
 
    function real2str(val, precision) result(strOut)
        real*8 :: val
        character(len=23) :: strOut
        integer, optional :: precision

        if(present(precision)) then
           write(strOut, '(F9.'//trim(int2str(precision))//')') val
           !write(strOut, '(F9.4)') val
        else
            !write(strOut, '(D23.15)') val
            write(strOut, '(F9.5)') val
        end if

        strOut = adjustl(strOut)
    
      end function
    
      function str2real8(strIpt) result(val)
        use iso_fortran_env, only: real64
        character(len=*) :: strIpt
        real(kind=real64) :: val
        character(len=23) :: strR
    
        PRINT *, "strIpt is ", strIpt
    
        write(strR, *) strIpt
        strR = adjustl(strR)
        PRINT *, "strR is ", strR
        read(strR, '(D23.15)') val
        read(strR, '(F9.5)') val
        PRINT *, "Output val is ", val
    
      end function
    
      function str2int(strIpt) result(val)
        integer :: val
        character(len=*) :: strIpt
        character(len=80) :: strB
    
        write(strB, '(A3)') strIpt
        ! Not sure if this is needed
        strB = adjustl(strB)
        read(strB, '(I3)') val
    
      end function
    
      function int2str(ipt) result(strInt)
        integer :: ipt
        character(len=20) :: strInt
    
        write(strInt, '(I20)') ipt
    
        strInt = ADJUSTL(strInt)
    
      end function

      function int2char(iptInt) result(outputChar)
        integer, intent(in) :: iptInt
        character(len=80) :: outputChar
        write(outputChar, '(I3)') iptInt
    
        outputChar = adjustl(outputChar)
    
      end function      


end module