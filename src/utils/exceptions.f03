!A module that creates some semblance of error and exception handling.
!
!Written by Eshed Magali
!List of changes:
!DATE              AUTHOR      CHANGE LOG
!July 3rd, 2017    Eshed       First working version
!

MODULE Exceptions
  USE Parameters
  IMPLICIT NONE
  PRIVATE

  INTEGER, PARAMETER, PUBLIC :: ERRCODE_GENERAL = 1
  INTEGER, PARAMETER, PUBLIC :: ERRCODE_URGENT = 2
  INTEGER, PARAMETER, PUBLIC :: ERRCODE_FAIL = 3

  TYPE, PUBLIC :: Exception
     !A general exception. You know what these are: They arise, but they might not be actual
     !errors. They're just things that interrupt the runtime.
     PRIVATE
     CHARACTER(:), ALLOCATABLE :: msg
     LOGICAL :: flag = .FALSE.
   CONTAINS
     PROCEDURE, PUBLIC, PASS :: raise => raise_exception
     PROCEDURE, PUBLIC, PASS :: catch => catch_exception
     PROCEDURE, PUBLIC, PASS :: print => print_exception
     PROCEDURE, PUBLIC, PASS :: get_msg => get_message
     PROCEDURE, PUBLIC, PASS :: del => del
  END TYPE Exception


  
  TYPE, PUBLIC, EXTENDS(Exception) :: Error
     !A general error. These are actual problems that force a stop. This means something could
     !not be performed.
     PRIVATE
     INTEGER :: code = ERRCODE_GENERAL
   CONTAINS
     PROCEDURE, PUBLIC, PASS :: set_priority
     PROCEDURE, PUBLIC, PASS :: get_priority
  END TYPE Error

  TYPE, PUBLIC, EXTENDS(Error) :: TypeError
     !An error concenring an operation on the wrong type.
  END TYPE TypeError


  TYPE, PUBLIC, EXTENDS(Error) :: ValueError
     !An error because the value of a variable wasn't right somehow

     CHARACTER(:), PUBLIC, ALLOCATABLE :: value

  END TYPE ValueError

  
  TYPE, PUBLIC, EXTENDS(Error) :: IOError
     !An error concenrning Input/Output OS errors. Mostly errors with files.
     INTEGER :: IOSTAT
     CHARACTER(:), ALLOCATABLE :: fpath
     
   CONTAINS
     PROCEDURE, PUBLIC, PASS :: set_info => set_fileIO
     PROCEDURE, PUBLIC, PASS :: print => print_IOError
  END TYPE IOError
  
CONTAINS


  
  FUNCTION get_message(self) RESULT (msg)
    CLASS(Exception), INTENT(IN) :: self
    CHARACTER(:), ALLOCATABLE :: msg
    msg = self%msg
  END FUNCTION get_message

  SUBROUTINE del(self)
    CLASS(Exception), INTENT(INOUT) :: self
    IF (ALLOCATED(self%msg)) DEALLOCATE(self%msg)
    self%flag = .FALSE.

  END SUBROUTINE del
  
  SUBROUTINE raise_exception(self, msg)
    CLASS(Exception), INTENT(INOUT) :: self
    CHARACTER(*), INTENT(IN) :: msg
    self%msg = TRIM(msg)
    self%flag = .TRUE.

  END SUBROUTINE raise_exception

  FUNCTION catch_exception(self) RESULT (flg)
    CLASS(Exception), INTENT(IN) :: self
    LOGICAL :: flg
    flg = self%flag
  END FUNCTION catch_exception

  SUBROUTINE set_priority(self, code)
    CLASS(Error), INTENT(INOUT) :: self
    INTEGER, INTENT(IN) :: code

    self%code = code

  END SUBROUTINE set_priority

  SUBROUTINE get_priority(self, code)
    CLASS(Error), INTENT(INOUT) :: self
    INTEGER, INTENT(OUT) :: code

    code = self%code

  END SUBROUTINE get_priority

  SUBROUTINE set_fileIO(self, path, iocode)
    !Sets the information from the file open/close error into the error object.
    CLASS(IOError), INTENT(INOUT) :: self
    CHARACTER(*), INTENT(IN) :: path
    INTEGER, INTENT(IN) :: iocode

    self%fpath = TRIM(path)
    self%IOSTAT = iocode

  END SUBROUTINE set_fileIO

  SUBROUTINE print_exception(self)
    CLASS(Exception), INTENT(IN) :: self

    WRITE(*, *) TRIM(self%msg)
  END SUBROUTINE print_exception

  SUBROUTINE print_IOError(self)
    CLASS(IOError), INTENT(IN) :: self
    CHARACTER(4) :: cd

    WRITE(cd, "(1X,I3)") self%IOSTAT

    WRITE(*,*) TRIM(self%msg)
    WRITE(*,*) 'Failed on file: ' // TRIM(self%fpath)
    WRITE(*,*) 'Status code was: ' // cd 
  END SUBROUTINE print_IOError
     
END MODULE Exceptions
