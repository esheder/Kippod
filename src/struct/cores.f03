!A module that contains the definition of a reactor core. The core structure includes information
!about the geometry, like if it's a hex core, a cartesian core and so on. It is also assigned
!reflector objects and its grid property is a matrix that will hold all the rods in the core.
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!

MODULE Cores
  USE Parameters
  USE Rods
  USE Reflectors

  IMPLICIT NONE
  PRIVATE
  
  CHARACTER(LEN=*),PARAMETER :: modName='Core'
  
  TYPE, PUBLIC :: Core
     !A core is a data type that contains the information required to build a full core.
     !> Name of the core
     CHARACTER(LEN=MX_NM) :: name
     !> Name of the type of geometry. This will be used to be able to tell if this is cartesian or
     !> or hexagonal or anything else.
     CHARACTER(LEN=MX_NM) :: geom
     !> Boundary conditions outside the core reflectors. Something has to be picked, right?
     CHARACTER(LEN=MX_NM), DIMENSION(3) :: bc
     !> Reflector objects in this core. Typically one would have reflectors all around the core.
     TYPE(Reflector), DIMENSION(:), ALLOCATABLE :: refs
     !> A 2D matrix of where rods are inserted. in a 2D problem those rods will be 0D each, and
     !> a 1D core is done by having just one line of these with perfect reflectors.
     TYPE(Rod), DIMENSION(:,:), ALLOCATABLE :: grid
     !> Rated core power in MW. Used for normalization purposes mostly
     REAL(KIND=SRK) :: pwr
     !> Pitch between rod centers in x and in y directions.
     REAL(KIND=SRK), DIMENSION(2) :: pitch
   CONTAINS
     PROCEDURE, PUBLIC, PASS :: init => init_core
     PROCEDURE, PUBLIC, PASS :: destroy => clear_core
     
  END TYPE Core

CONTAINS

  !
  !-------------------------------------------------------------------------------
  !> @brief Initializes a core object
  !> @param self The Core object
  !> @param name The name of the core
  !> @param geom The geometry type of the desired core. See current options in documentation.
  !> @param dimen The dimensions of the core grid (how many rods in each row and how many rows)
  !> @param pitch The size of each rod in the grid
  !> @param bc Boundary conditions for the core outside the reflectors.
  !>

  SUBROUTINE init_core(self, name, geom, dimen, pitch, bc)
    CLASS(Core), INTENT(INOUT) :: self
    CHARACTER(LEN=*), INTENT(IN) :: name
    CHARACTER(LEN=*), INTENT(IN) :: geom
    INTEGER, DIMENSION(2), INTENT(IN) :: dimen
    REAL(KIND=SRK), DIMENSION(2), INTENT(IN) :: pitch
    CHARACTER(LEN=*), DIMENSION(3), INTENT(IN) :: bc
    CHARACTER(LEN=*), PARAMETER :: myName='init_core'

    self%name = name
    self%geom = geom
    ALLOCATE(self%grid(dimen(1),dimen(2)))
    self%pitch = pitch
    self%bc = bc
  END SUBROUTINE init_core

  !
  !-------------------------------------------------------------------------------
  !> @brief Clears a core
  !> @param self The core object
  !>
  SUBROUTINE clear_core(self)
    CLASS(Core), INTENT(INOUT) :: self
    CHARACTER(LEN=*), PARAMETER :: myName='clear_core'

    DEALLOCATE(self%grid) !Rods are not destroyed here to allow for modelling waste disposal
    
    DEALLOCATE(self%refs) !Refs are destroyed here because reflectors are core dependent!
    

  END SUBROUTINE clear_core
  

END MODULE Cores
