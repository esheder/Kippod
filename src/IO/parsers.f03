!A module where all the general input parsers will be stored. Different modules will contain
!the actual parsers, as this is more of some groundwork.
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!July 3rd 2017     Eshed       First working version
!

MODULE Parsers
  USE Exceptions
  USE MathUtils
  USE Parameters
  USE LineLists
  IMPLICIT NONE
  PRIVATE

  INTEGER, PARAMETER :: F_CLOSED = 0
  INTEGER, PARAMETER :: F_OPEN = 1

  TYPE, PUBLIC, ABSTRACT :: Parser
     !A worker that parses a piece of information.
     PRIVATE
     
  END TYPE Parser

  TYPE, PUBLIC, ABSTRACT, EXTENDS(Parser) :: DeflineParser
     !A worker that breaks down definition lines to get information.
     PRIVATE
   CONTAINS
     PROCEDURE, PASS, NON_OVERRIDABLE :: breakline
     PROCEDURE, PASS, NON_OVERRIDABLE :: breakdef

  END TYPE DeflineParser

  TYPE, PUBLIC, ABSTRACT, EXTENDS(Parser) :: FileParser
     !A Worker that works with a file
     PRIVATE
     CHARACTER(:), ALLOCATABLE :: path
     INTEGER :: pipe = -1
     INTEGER :: status = F_CLOSED
   CONTAINS
     PROCEDURE, PASS, NON_OVERRIDABLE :: associate_file
     PROCEDURE, PASS, NON_OVERRIDABLE :: open_file
     PROCEDURE, PASS, NON_OVERRIDABLE :: close_file
     PROCEDURE(laparse), PASS, DEFERRED :: parse_file
  END TYPE FileParser

  ABSTRACT INTERFACE
     SUBROUTINE laparse(self, err)
       IMPORT FileParser
       IMPORT Exception
       CLASS(FileParser), INTENT(INOUT) :: self
       CLASS(Exception), INTENT(OUT) :: err
     END SUBROUTINE laparse
  END INTERFACE

CONTAINS


  !FileParser Subroutines
  
  SUBROUTINE associate_file(self, path)
    CLASS(FileParser), INTENT(INOUT) :: self
    CHARACTER(*), INTENT(IN) :: path

    self%path = path
    self%pipe = hash(self%path)

  END SUBROUTINE associate_file

  SUBROUTINE open_file(self, err)
    CLASS(FileParser), INTENT(INOUT) :: self
    CLASS(IOError), INTENT(OUT) :: err
    INTEGER :: i

    IF (self%status .EQ. F_CLOSED) THEN
       OPEN(UNIT=self%pipe, FILE=self%path, STATUS='OLD', IOSTAT=i, FORM='UNFORMATTED', &
            RECL=MX_BFR, POSITION='REWIND', ACTION='READ')

       IF (i .NE. 0) THEN
          CALL err%raise('Failed to open file. See code for more info.')
          CALL err%set_priority(ERRCODE_FAIL)
          CALL err%set_info(self%path, i)
       ELSE
          self%status = F_OPEN
       END IF
    ELSE
       CALL err%raise('Attempted to open a non-closed file.')
       CALL err%set_priority(ERRCODE_FAIL)
       CALL err%set_info(self%path, i)
    END IF
    
  END SUBROUTINE open_file

  SUBROUTINE close_file(self, err)
    CLASS(FileParser), INTENT(INOUT) :: self
    CLASS(IOError), INTENT(OUT) :: err
    INTEGER :: i

    IF (self%status .EQ. F_OPEN) THEN
       CLOSE(UNIT=self%pipe, IOSTAT=i, STATUS='KEEP')
       IF (i .NE. 0) THEN
          CALL err%raise('Failed to close file. See code for more info.')
          CALL err%set_priority(ERRCODE_FAIL)
          CALL err%set_info(self%path, i)
       ELSE
          self%status = F_CLOSED
       END IF
          
    ELSE
       CALL err%raise('Attempted to close an unopen file.')
       CALL err%set_priority(ERRCODE_FAIL)
       CALL err%set_info(self%path, 0)
    END IF

  END SUBROUTINE close_file



  !DefParser Subroutines

  FUNCTION breakline(self, line) RESULT(sent)
    CLASS(DeflineParser), INTENT(IN) :: self
    CHARACTER(*), INTENT(IN) :: line
    TYPE(LineList), POINTER :: sent

    sent => split(TRIM(ADJUSTL(line)))

  END FUNCTION breakline

  SUBROUTINE breakdef(self, def, err, typ, msg)
    CLASS(DeflineParser), INTENT(IN) :: self
    CHARACTER(*), INTENT(IN) :: def
    CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: typ, msg
    TYPE(ValueError), INTENT(OUT) :: err
    INTEGER :: k
    k = INDEX(def, '=')
    IF (k .EQ. 0) THEN
       WRITE(msg, '(1X, A, A)') 'Word was not made up of A=B. Word was: ', TRIM(def)
       CALL err%raise(msg)
       err%value = TRIM(def)
       RETURN
    ELSE
       typ = def(1:k-1)
       msg = def(k+1:)
    END IF
    
  END SUBROUTINE breakdef

END MODULE Parsers
