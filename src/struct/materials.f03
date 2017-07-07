!A module that contains the definition of a reactor material. Materials are basically homogenized
!unitcells. These are connected with specific cross section sets.
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!

MODULE Materials
  USE Exceptions
  USE Parameters
  
  IMPLICIT NONE
  PRIVATE

  TYPE, PUBLIC :: Material
     !TODO: Finish this definition

     CHARACTER(MX_BFR) :: name

   CONTAINS
     PROCEDURE, PASS :: init
  END TYPE Material

CONTAINS

  SUBROUTINE init(self, name, libpath, setnum, burn, err)
    ! TODO: Finish this routine
    CLASS(Material), INTENT(INOUT) :: self
    CHARACTER(*), INTENT(IN) :: name, libpath
    INTEGER, INTENT(IN) :: setnum, burn
    CLASS(Error), INTENT(OUT) :: err
  END SUBROUTINE init

END MODULE Materials
