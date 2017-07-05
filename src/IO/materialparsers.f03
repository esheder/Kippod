!A module specifically to read a cip-like material section
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!

MODULE MaterialParsers
  USE Seclists
  USE Exceptions
  USE Parsers
  USE Materials
  USE LineLists
  USE Parameters
  USE Lists

  IMPLICIT NONE
  PRIVATE

  INTEGER, PARAMETER :: MATLINE_LEN=5

  TYPE, PUBLIC, EXTENDS(Parser) :: MaterialParser
     ! A parser that takes the material section of a cip-like file and stores information for
     ! creation by a material constructor.

     TYPE(SectionList), POINTER :: raw_sec
     TYPE(Material), DIMENSION(:), ALLOCATABLE :: materials

   CONTAINS
     PROCEDURE, PASS :: parse => parse_lines
     PROCEDURE, PASS :: destructor
  END TYPE MaterialParser

CONTAINS

  SUBROUTINE destructor(self)
    !Destroy this object.
    !
    CLASS(MaterialParser), INTENT(INOUT) :: self

    IF (ASSOCIATED(self%raw_sec)) THEN
       DEALLOCATE(self%raw_sec)
    END IF

    IF (ALLOCATED(self%materials)) THEN
       DEALLOCATE(self%materials)
    END IF
  END SUBROUTINE destructor

  SUBROUTINE parse_lines(self)
    !Parse the material lines to create materials.
    !
    CLASS(MaterialParser), INTENT(INOUT) :: self
    CLASS(LineList), POINTER :: line
    CLASS(LineList), POINTER :: words
    TYPE(Error) :: err
    INTEGER :: i, j, num_lines, set, burn
    CHARACTER(MX_BFR) :: msg, name, path

    ALLOCATE(self%materials(num_lines))
    line => self%raw_sec%fline

    DO i=1, num_lines
       line => ListDowncast(line%next)
       words => breakdown(line%line, err)
       IF (err%catch()) CALL stop_badline(i, line%line, err)
       
       IF (LEN(words) .NE. MATLINE_LEN) THEN
          WRITE(msg, 100) 'Wrong word count in material line. CNT was ', LEN(words), &
                        & 'but must be ', MATLINE_LEN
          CALL err%raise(msg)
          CALL err%print()
          STOP(2)
       END IF
       
       !If you get here that means we have the right number of words.
       name = words%line
       words => ListDowncast(words%next)
       path = words%line
       words => ListDowncast(words%next)
       READ(words%line, '(I10)') set
       words => ListDowncast(words%next)
       READ(words%line, '(I10)') burn
       
       CALL self%materials(i)%init(name, path, set, burn, err)
       IF (err%catch()) CALL stop_badline(i, line%line, err)

    END DO

100 FORMAT('(1X, A, I3, A, I3)')

  CONTAINS
    SUBROUTINE stop_badline(i, line, err)
      INTEGER, INTENT(IN) :: i
      CHARACTER(*), INTENT(IN) :: line
      CLASS(Error), INTENT(IN) :: err
      
      WRITE(*, '(1X, A, I3, A, A)') 'Failed on line no. ', i, &
           & ' of the materials section. Line was:\n', line
      CALL err%print()
      STOP(2)
      
    END SUBROUTINE stop_badline
       
  END SUBROUTINE parse_lines

  FUNCTION breakdown(line, err) RESULT(wordlist)
    !TODO: WRITE THIS FUNCTION
    CLASS(LineList), POINTER :: wordlist
    CHARACTER(*), INTENT(IN) :: line
    CLASS(Error), INTENT(INOUT) :: err

  END FUNCTION breakdown
     

END MODULE MaterialParsers
