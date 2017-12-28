!A module that defines a physical core reflector.
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!
!

MODULE Reflectors

  USE Materials
  USE Parameters
  USE Exceptions
  IMPLICIT NONE
  PRIVATE
  
  TYPE, PUBLIC :: Reflector
     !The reflector class
     PRIVATE

     TYPE(Material), POINTER :: mat
     CHARACTER(2) :: direc
     CHARACTER(:), ALLOCATABLE :: name
     CHARACTER(:), ALLOCATABLE :: mat_name
     REAL(SRK) :: width
     INTEGER :: meshp
     INTEGER :: placement

   CONTAINS
     PROCEDURE, PASS :: init
     PROCEDURE, PASS :: set_material

  END TYPE Reflector

CONTAINS

  SUBROUTINE init(self, name, dir, mat, width, mesh, loc)
    CLASS(Reflector), INTENT(INOUT) :: self
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(2), INTENT(IN) :: dir !Direction in core
    CHARACTER(LEN=*), INTENT(IN) :: mat
    REAL(KIND=SRK), INTENT(IN) :: width
    INTEGER, INTENT(IN) :: mesh, loc

    self%name = name
    self%mat_name = mat
    self%direc = dir
    self%width = width
    self%meshp = mesh
    self%placement = loc

  END SUBROUTINE init

  SUBROUTINE set_material(self, mats, err)
    CLASS(Reflector), INTENT(INOUT) :: self
    TYPE(Material), DIMENSION(:), INTENT(IN), TARGET :: mats
    TYPE(Error), INTENT(OUT) :: err
    INTEGER :: i
    CHARACTER(:), ALLOCATABLE :: msg
    DO i=1, SIZE(mats)
       IF (mats(i)%name .EQ. self%mat_name) THEN
          self%mat => mats(i)
          RETURN
       END IF
    END DO

    WRITE(msg, '(1X, A, A, A)') 'Could not find material ', self%mat_name, ' in list of materials'
    CALL err%raise(msg)
    CALL err%set_priority(ERRCODE_FAIL)
    
  END SUBROUTINE set_material

END MODULE Reflectors
