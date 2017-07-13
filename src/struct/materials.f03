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
     !A material is a data type that contains the metadata and data of a given core component.
     !Fuel rods are made up of fuel regions, spacer regions and the like, often homogenized with
     !the sorrounding moderator. This is basically a way to get information for the cross sections
     !using a core section state, such as temperature, burnup or boron content.
     !TODO: Finish this definition
     PRIVATE
     CHARACTER(:), ALLOCATABLE, PUBLIC :: name
     CHARACTER(:), DIMENSION(:), ALLOCATABLE :: files
     INTEGER, DIMENSION(:), ALLOCATABLE :: sets
     LOGICAL :: burnable

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
