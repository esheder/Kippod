!A module for auxiliary math functions that are used somewhere in the code but are kind of
!standalone.
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!July 3rd, 2017    Eshed       First working version
!


MODULE MathUtils
CONTAINS
  !Grabbed off the internet from a guy called Alan Mille, from a forum at:
  !http://computer-programming-forum.com/49-fortran/d61738fbe43347b5.htm
  FUNCTION hash(text) RESULT(hashed) 
    IMPLICIT NONE 
    CHARACTER (LEN=*), INTENT(IN) :: text 
    INTEGER                       :: hashed     ! 32-bit integers assumed 
    INTEGER, PARAMETER :: magic_numb = Z'5D3A9F33' 
    INTEGER            :: i, j 
    hashed = 0                              ! Note: B was not initialized before 
    DO i = 1, LEN_TRIM(text) 
       j = MOD(i-1, 4) * 8 
       hashed = IEOR( hashed, ISHFT( ICHAR( text(i:i) ), j ) ) 
    END DO
    hashed = ABS( IEOR( hashed, magic_numb ) )    ! Why take the absolute value? 
    RETURN 
  END FUNCTION hash

END MODULE MathUtils
