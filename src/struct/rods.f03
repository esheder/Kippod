! Module for defining a general rod object
!   All units are base SI.
!   Written by Zachary Dodson at first
!List of changes:
!DATE              AUTHOR      CHANGE LOG
!
MODULE Rods
  USE Parameters
  USE Materials
  IMPLICIT NONE
  PRIVATE
  !
  CHARACTER(LEN=*),PARAMETER :: modName='Rod'
  TYPE, PUBLIC :: Rod
    !> Length of the rod (m).
    REAL(SRK) :: len
    !> Diameter of the rod (m).
    REAL(SRK) :: dia
    !> List of materials in the rod from bottom to top.
    TYPE(Material), DIMENSION(:), ALLOCATABLE :: materials
    !> Heights of each material in the rod from bottom to top (corresponds to
    !> materials in mats).
    REAL(SRK), DIMENSION(:), ALLOCATABLE :: heights
    !> Temperatures of each material in this rod. Will probably be fairly even across the core but
    !> allows for future coupling, though that feature might require a change in how this is stored
    REAL(SRK), DIMENSION(:), ALLOCATABLE :: temperatures
    CONTAINS
      PROCEDURE, PUBLIC, PASS :: init => init_Rod
      PROCEDURE, PUBLIC, PASS :: clear => clear_Rod
  ENDTYPE Rod
!
!===============================================================================
  CONTAINS
!
!-------------------------------------------------------------------------------
!> @brief Initializes a Rod object.
!> @param self The Rod object.
!> @param length The length of the rod (m).
!> @param diameter The diameter of the rod (m).
!> @param mats Vector of materials.
!> @param heights Vector describing the heights of each materials in the rod. 
!>
    SUBROUTINE init_Rod(self, length, diameter, mats, heights)
      CLASS(Rod),INTENT(INOUT) :: self
      REAL(SRK) :: length,diameter
      CLASS(Material), DIMENSION(:), INTENT(IN) :: mats
      REAL(SRK), DIMENSION(:) :: heights
      CHARACTER(LEN=*), PARAMETER :: myName='init_Rod'

      self%len = length
      self%dia = diameter
      self%materials = mats
      self%heights = heights
    ENDSUBROUTINE init_Rod
!
!-------------------------------------------------------------------------------
!> @brief Clears a Rod object.
!> @param self The Rod object.
!>
    SUBROUTINE clear_Rod(self)
      CLASS(Rod),INTENT(INOUT) :: self
      CHARACTER(LEN=*),PARAMETER :: myName='clear_Rod'

      DEALLOCATE(self%materials)
      DEALLOCATE(self%heights)
    ENDSUBROUTINE clear_Rod
    !
ENDMODULE Rods
