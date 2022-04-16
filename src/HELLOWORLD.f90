
        
        SUBROUTINE HELLOWORLD(string) bind(C, name="helloworldfortran")
        use iso_c_binding, only: c_char
        IMPLICIT NONE
        character(kind=c_char) :: string(*)

!        USE GLOBALS




!        INTEGER CATLIST

!        CALL INITKDP

!        WRITE ( *, '(a)' ) "Done with Init KDP"
        WRITE ( *, '(a)' ) '  Hello, world this is fortran!'
        END
