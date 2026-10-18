
module type_utils
  ! THis module should only contain methods that have no dependencies
      
      type string
         character(len=:), allocatable :: s
      end type string    
  
      contains

      ! Compact text for a value in a user-level command or lens file: up to
      ! 9 significant digits, no trailing zeros ("10", "7.5", "0.30899999",
      ! "1.2E-07").  Keeps long lists (YAN/WTF/VUY with 10 fields, WL ...)
      ! short enough for the command line while round-tripping typed values.
      function compactReal(v) result(s)
          use iso_fortran_env, only: real64
          implicit none
          real(real64), intent(in) :: v
          character(len=32) :: s
          integer :: ie, k
          character(len=32) :: mant, expo

          if (v == 0.0_real64) then
            s = '0'
            return
          end if
          if (abs(v) >= 1.0d-4 .and. abs(v) < 1.0d9) then
            write(s, '(F0.9)') v
            ! keep at most 9 significant digits
            write(s, '(G0.9)') v
            s = adjustl(s)
            if (index(s, 'E') > 0) then
              write(s, '(F0.9)') v
            end if
          else
            write(s, '(ES16.8E2)') v
          end if
          s = adjustl(s)
          ie = index(s, 'E')
          if (ie > 0) then
            mant = s(1:ie-1); expo = s(ie:)
          else
            mant = s; expo = ''
          end if
          if (index(mant, '.') > 0) then
            k = len_trim(mant)
            do while (k > 1 .and. mant(k:k) == '0')
              k = k - 1
            end do
            if (mant(k:k) == '.') k = k - 1
            mant = mant(1:k)
          end if
          if (mant(1:1) == '.') mant = '0'//mant
          if (mant(1:2) == '-.') mant = '-0'//mant(2:)
          s = trim(mant)//trim(expo)
      end function compactReal

  
   
      function real2str(val, precision, sci) result(strOut)
          use iso_fortran_env, only: real64, real32
          use iso_c_binding, only: c_double
  
          implicit none
  
          class(*) :: val
          real(kind=real64) :: valDP
          real(kind=real32) :: valSP
          character(len=23) :: strOut
          character(len=80) :: strOutDP
          integer, optional :: precision
          logical, optional :: sci
  
          select type(val)
          type is (real(real64))
            valDP = val
  
            if(present(sci)) then
              write(strOut, '(D23.10)') valDP
              strOut = adjustl(strOut)
              return
            end if
  
            
            if(present(precision)) then
              write(strOut, '(F9.'//trim(int2str(precision))//')') valDP
              !write(strOut, '(F9.4)') val
           else
               !
               write(strOut, '(F9.5)') valDP
           end if          

           ! Sometimes will get **** if the precision is wrong.  
           if (strOut(1:3) == '***') then 
            write(strOutDP, *) valDP
            strOutDP = adjustl(strOutDP)
            strOut = strOutDP(1:len(strOut))
           end if

            type is (real(real32))
            valSP = val
  
            if(present(sci)) then
              write(strOut, '(D23.10)') valSP
              strOut = adjustl(strOut)
              return
            end if          
            if(present(precision)) then
              write(strOut, '(F9.'//trim(int2str(precision))//')') valSP
              !write(strOut, '(F9.4)') val
           else
               !write(strOut, '(D23.15)') val
               write(strOut, '(F9.5)') valSP
           end if    

           ! Sometimes will get **** if the precision is wrong.  
           if (strOut(1:3) == '***') then 
            write(strOut, *) valSP
           end if
  
      end select
          
      
          strOut = adjustl(strOut)
      
        end function
      
        function str2real8(strIpt) result(val)
          use iso_fortran_env, only: real64
          character(len=*) :: strIpt
          real(kind=real64) :: val
          !character(len=23) :: strR
      
  
          read(strIpt,*) val

      
        end function
      
        function str2int(strIpt) result(intVal)
          use iso_fortran_env, only: real64, real32
          implicit none
          integer :: intVal
          real(kind=real64) :: tstReal
          character(len=*) :: strIpt
          character(len=80) :: strB
          integer :: ios

          ! Converting directly to int crashes when there are a lot of significant digits,
          ! so convert to real64 and then use internal Fortran funtion to convert to INT.
          ! Guard empty/non-numeric input (iostat) so a bad token returns 0 rather than
          ! aborting the program with an End-of-file runtime error.
          read(strIpt, *, iostat=ios) tstReal
          if (ios /= 0) tstReal = 0.0_real64
          intVal = INT(tstReal)

          ! write(strB, '(A4)') strIpt
        
          ! ! Not sure if this is needed
          ! strB = adjustl(strB)
          ! !read(strB, '(I4)') val
          ! print *, "strB is ", strB
          ! !print *, "va"
          ! read(strB, *) intVal
      
        end function
      
        function bool2str(ipt) result(strBool)
  
          logical :: ipt
          character(len=20) :: strBool
  
          if (ipt) then 
            strBool = trim(int2str(1))
          else
            strBool = trim(int2str(0))
          end if
  
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
        
        function blankStr(strLen) result(blnk)
          implicit none
          character(:), allocatable :: blnk
          integer :: strLen, i
      
          allocate(character(len=strLen) :: blnk)
          do i=1,strLen
              blnk(i:i) = ' '
          end do 
      
        end function

        function int2ptr(ipt) result(ptr)
          integer, target, intent(in) :: ipt
          integer, pointer :: ptr
  
          ptr=>ipt
          
        end function


  end module