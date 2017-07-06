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
  USE StrUtils

  IMPLICIT NONE
  PRIVATE

  INTEGER, PARAMETER :: MATLINE_LEN = 4
  CHARACTER(*), PARAMETER :: FLPTH = 'file'
  CHARACTER(*), PARAMETER :: SETNM = 'set'
  CHARACTER(*), PARAMETER :: BURNFLG = 'burn'

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
       CALL self%raw_sec%destructor()
       DEALLOCATE(self%raw_sec)
    END IF

    IF (ALLOCATED(self%materials)) THEN
       DEALLOCATE(self%materials)
    END IF
  END SUBROUTINE destructor

  SUBROUTINE parse_lines(self, err)
    !Parse the material lines to create materials.
    !
    CLASS(MaterialParser), INTENT(INOUT) :: self
    CLASS(LineList), POINTER :: line
    CLASS(LineList), POINTER :: words
    TYPE(Error), INTENT(OUT) :: err
    INTEGER :: i, j, k, num_lines, set, burn
    CHARACTER(MX_BFR) :: msg, name, path, typ, frmt

    num_lines = LEN(self%raw_sec)
    ALLOCATE(self%materials(num_lines))
    line => self%raw_sec%fline

    lns:DO i=1, num_lines
       line => ListDowncast(line%next)
       words => split(TRIM(ADJUSTL(line%line)))
       IF (LEN(words) .NE. MATLINE_LEN) THEN
          frmt = '(1X, A, I3, 1X, A, I3, 1X, A, A)'
          WRITE(msg, frmt) 'Wrong word count in material line. CNT was ', LEN(words), &
                        & 'but must be ', MATLINE_LEN, 'Line was: ', TRIM(line%line)
          CALL err%raise(msg)
          CALL err%print()
          RETURN
       END IF
       
       !If you get here that means we have the right number of words.
       name = words%line
       words => ListDowncast(words%next)
       wrds:  DO j=1, MATLINE_LEN - 1
          msg = words%line
          words => ListDowncast(words%next)
          k = INDEX(msg, '=')
          IF (k .EQ. 0) THEN
             frmt = '(1X, A, A, A, I3, A, A)'
             WRITE(msg, frmt) 'Word was not made up of A=B. Word was: ', TRIM(msg), &
                           & ' in material line no. ', i, '. The full line was:', TRIM(words%line)
             CALL err%raise(msg)
             CALL err%print()
             RETURN
          ELSE
             typ = msg(1:k-1)
             msg = msg(k+1:)
          END IF

          WRITE(frmt, '(A, I10, A)') "(I", MX_BFR, ")"
          SELECT CASE (lowercase(TRIM(typ)))
          CASE (FLPTH)
             path = msg
          CASE (SETNM)
             READ(msg, frmt) set
          CASE (BURNFLG)
             READ(msg, frmt) burn
          CASE DEFAULT
             frmt = '(1X, A, A, A, I3, A, A)'
             WRITE(msg, frmt) 'Unrecognized material attribute ', lowercase(TRIM(typ)), &
                            & ' at line no. ', i, '. The line was:', TRIM(words%line)
             CALL err%raise(msg)
             CALL err%print()
             RETURN
          END SELECT
       END DO wrds
       
       !CALL self%materials(i)%init(name, path, set, burn, err)
       IF (err%catch()) CALL stop_badline(i, line%line, err)

    END DO lns


  CONTAINS
    SUBROUTINE stop_badline(i, line, err)
      INTEGER, INTENT(IN) :: i
      CHARACTER(*), INTENT(IN) :: line
      CLASS(Error), INTENT(IN) :: err
      
      WRITE(*, '(1X, A, I3, A, A)') 'Failed on line no. ', i, &
           & ' of the materials section. Line was:', TRIM(line)
      CALL err%print()
      STOP(2)
      
    END SUBROUTINE stop_badline
       
  END SUBROUTINE parse_lines
     

END MODULE MaterialParsers
