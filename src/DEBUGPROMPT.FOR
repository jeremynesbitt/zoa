       SUBROUTINE DEBUGPROMPT()    !Obtain a text in reply.
Concocted by R.N.McLean (whom God preserve), December MM.

        WRITE( *,2)
2       FORMAT ('> ', $ )
        READ (*, '(a)') QUERY
        PRINT *, "INPUT = ", QUERY


       RETURN           !Take that.
      END           !Others interpret the reply.
