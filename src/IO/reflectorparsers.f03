!A module specifically to read a cip-like reflector section.
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!
!

MODULE ReflectorParsers
  USE Parsers
  USE Reflectors
  USE SecLists
  USE LineLists
  USE Lists
  USE Exceptions
  USE Parameters
  USE StrUtils
  
  IMPLICIT NONE
  PRIVATE

  INTEGER, PARAMETER :: REFLINE_LEN = 6
  CHARACTER(*), PARAMETER :: DIREC='direction'
  CHARACTER(*), PARAMETER :: MAT='material'
  CHARACTER(*), PARAMETER :: WIDTHC='width'
  CHARACTER(*), PARAMETER :: MESHP='meshpoints'
  CHARACTER(*), PARAMETER :: PLACE='placement'

  TYPE, PUBLIC, EXTENDS(DeflineParser) :: ReflectorsParser
     !A worker that reads the reflector section and obtains the information needed to build the
     !core reflectors.
     !
     PRIVATE

     TYPE(LineList), POINTER :: raw_sec
     TYPE(Reflector), DIMENSION(:), ALLOCATABLE :: reflectors

   CONTAINS
     PROCEDURE, PASS :: parse => parse_lines

  END TYPE ReflectorsParser

CONTAINS

  SUBROUTINE parse_lines(self, sec, err)
    !Parse the section
    CLASS(ReflectorsParser), INTENT(INOUT) :: self
    CLASS(LineList), TARGET, INTENT(IN) :: sec
    CLASS(LineList), POINTER :: line
    CLASS(LineList), POINTER :: words
    CLASS(Exception), INTENT(OUT) :: err
    TYPE(ValueError) :: err2
    INTEGER :: i, j, num_lines, mesh, loc
    REAL(SRK) :: width
    CHARACTER(:), ALLOCATABLE :: frmt, name, material, typ, msg
    CHARACTER(2) :: direct

    !Get rid of previous information
    IF (ASSOCIATED(self%raw_sec)) DEALLOCATE(self%raw_sec)
    self%raw_sec => sec
    num_lines = LEN(sec)
    ALLOCATE(self%reflectors(num_lines-1))
    line => self%raw_sec

    lns:DO i=1, num_lines
       line => ListDowncast(line%next)
       words => self%breakline(line%line)
       IF (LEN(words) .NE. REFLINE_LEN) THEN
          frmt = '(1X, A, I3, 1X, A, I3, 1X, A, A)'
          WRITE(msg, frmt) 'Wrong word count in reflector line. CNT was ', LEN(words), &
                        & 'but must be ', REFLINE_LEN, 'Line was: ', TRIM(line%line)
          CALL err%raise(msg)
          RETURN
       END IF

       name = words%line
       words => ListDowncast(words%next)
       wrds:  DO j=1, REFLINE_LEN - 1
          msg = words%line
          words => ListDowncast(words%next)
          CALL self%breakdef(msg, err2, typ, msg)
          IF (err2%catch()) THEN
             CALL err2%print()
             frmt = '(1X, A, I3, A, A)'
             WRITE(*, frmt) 'This happened in reflector line ', i, '. Line was: ', TRIM(line%line)
             CALL err%raise(err2%get_msg())
             RETURN
          END IF

          SELECT CASE (lowercase(TRIM(typ)))
          CASE (DIREC)
             direct = msg
          CASE (MAT)
             material = msg
          CASE (WIDTHC)
             READ(msg, *) width 
          CASE (MESHP)
             READ(msg, *) mesh
          CASE (PLACE)
             READ(msg, *) loc
          CASE DEFAULT
             frmt = '(1X, A, A, A, I3, A, A)'
             WRITE(msg, frmt) 'Unrecognized reflector attribute ', lowercase(TRIM(typ)), &
                            & ' at line no. ', i, '. The line was:', TRIM(words%line)
             CALL err%raise(msg)
             RETURN
          END SELECT
       END DO wrds

       CALL self%reflectors(i)%init(name, direct, material, width, mesh, loc)
       
    END DO lns
       
    
  END SUBROUTINE parse_lines

END MODULE ReflectorParsers
