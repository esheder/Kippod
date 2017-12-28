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

  TYPE, PUBLIC, EXTENDS(DeflineParser) :: MaterialParser
     ! A parser that takes the material section of a cip-like file and stores information for
     ! creation by a material constructor.

     TYPE(LineList), POINTER :: raw_sec
     TYPE(Material), DIMENSION(:), ALLOCATABLE :: materials

   CONTAINS
     PROCEDURE, PASS :: parse => parse_lines
     PROCEDURE, PASS :: destroy
  END TYPE MaterialParser

CONTAINS

  SUBROUTINE destroy(self)
    !Destroy this object.
    !
    CLASS(MaterialParser), INTENT(INOUT) :: self

    IF (ASSOCIATED(self%raw_sec)) THEN
       CALL self%raw_sec%destroy()
       DEALLOCATE(self%raw_sec)
    END IF

    IF (ALLOCATED(self%materials)) THEN
       DEALLOCATE(self%materials)
    END IF
  END SUBROUTINE destroy

  SUBROUTINE parse_lines(self, sec, err)
    !Parse the material lines to create materials.
    !
    CLASS(MaterialParser), INTENT(INOUT) :: self
    CLASS(LineList), TARGET, INTENT(IN) :: sec
    CLASS(Exception), INTENT(OUT) :: err
    TYPE(ValueError) :: err2
    CLASS(LineList), POINTER :: line
    CLASS(LineList), POINTER :: words
    INTEGER :: i, j, num_lines, set, burn
    CHARACTER(:), ALLOCATABLE :: msg, name, path, typ, val
    CHARACTER(MX_BFR) :: emsg, frmt

    IF (ASSOCIATED(self%raw_sec)) DEALLOCATE(self%raw_sec)
    self%raw_sec => sec
    num_lines = LEN(self%raw_sec)
    ALLOCATE(self%materials(num_lines-1))
    line => self%raw_sec
    
    NULLIFY(words)
    lns:DO i=1, num_lines
       line => ListDowncast(line%next)
       IF (ASSOCIATED(words)) THEN
          CALL words%destroy()
          NULLIFY(words)
       END IF
       words => self%breakline(line%line)
       IF (LEN(words) .NE. MATLINE_LEN) THEN
          frmt = '(1X, A, I3, 1X, A, I3, 1X, A, A)'
          WRITE(emsg, frmt) 'Wrong word count in material line. CNT was ', LEN(words), &
               & 'but must be ', MATLINE_LEN, 'Line was: ', TRIM(line%line)
          CALL err%raise(emsg)
          CALL err%print()
          RETURN
       END IF
       
       !If you get here that means we have the right number of words.
       name = words%line
       words => ListDowncast(words%next)
       wrds:  DO j=1, MATLINE_LEN - 1
          IF (ALLOCATED(msg)) DEALLOCATE(msg)
          IF (ALLOCATED(val)) DEALLOCATE(val)
          IF (ALLOCATED(typ)) DEALLOCATE(typ)
          msg = words%line
          words => ListDowncast(words%next)
          CALL self%breakdef(msg, err2, typ, val)
          IF (err2%catch()) THEN
             CALL err2%print()
             frmt = '(1X, A, I3, A, A)'
             WRITE(*, frmt) 'This happened in material line ', i, '. Line was: ', TRIM(line%line)
             CALL err%raise(err2%get_msg())
             RETURN
          END IF

          WRITE(frmt, '(A, I10, A)') "(I", MX_BFR, ")"
          SELECT CASE (lowercase(TRIM(typ)))
          CASE (FLPTH)
             path = val
          CASE (SETNM)
             READ(val, frmt) set
          CASE (BURNFLG)
             READ(val, frmt) burn
          CASE DEFAULT
             frmt = '(1X, A, A, A, I3, A, A)'
             WRITE(emsg, frmt) 'Unrecognized material attribute ', lowercase(TRIM(typ)), &
                            & ' at line no. ', i, '. The line was:', TRIM(words%line)
             CALL err%raise(emsg)
             CALL err%print()
             RETURN
          END SELECT
       END DO wrds
       
       !CALL self%materials(i)%init(name, path, set, burn, err)
       IF (ASSOCIATED(words)) THEN
          CALL words%destroy()
          NULLIFY(words)
       END IF
       IF (ASSOCIATED(line)) THEN
          CALL line%destroy()
          NULLIFY(words)
       END IF

    END DO lns
       
  END SUBROUTINE parse_lines
     

END MODULE MaterialParsers
