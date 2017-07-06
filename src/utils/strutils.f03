!A module that does string utilities
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!July 5th, 2017    Eshed       First working version
!


MODULE strutils

  IMPLICIT NONE
  PUBLIC
CONTAINS

  FUNCTION lowercase(chr) RESULT (res)
    !Adapted from http://rosettacode.org/wiki/String_case#Fortran
    CHARACTER(*), INTENT(IN) :: chr
    CHARACTER(LEN(chr)) :: res
    INTEGER :: i

    DO i= 1,LEN(chr)
       SELECT CASE (chr(i:i))
       CASE ('A':'Z')
          res(i:i) = ACHAR(IACHAR(chr(i:i)) + 32)
       CASE DEFAULT
          res(i:i) = chr(i:i)
       END SELECT

    END DO

  END FUNCTION lowercase

  ELEMENTAL FUNCTION uppercase(chr) RESULT (res)
    !Adapted from http://rosettacode.org/wiki/String_case#Fortran
    CHARACTER, INTENT(IN) :: chr
    CHARACTER :: res

    SELECT CASE (chr)
    CASE ('a':'z')
       res = ACHAR(IACHAR(chr) - 32)
    CASE DEFAULT
       res = chr
    END SELECT

  END FUNCTION uppercase 
  
END MODULE strutils
