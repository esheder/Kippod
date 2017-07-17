!A module that defines what an isotope is.
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!
!

MODULE Isotopes
  USE Trees
  USE RealLists
  USE Exceptions
  USE Parameters
  
  IMPLICIT NONE
  PRIVATE

  REAL(SRK), PARAMETER :: STABLE = -1.0 !Halflife of a stable "decay".
  
  TYPE, PUBLIC, EXTENDS(TreeNode) :: IsoTreeNode
     !An isotope decay chain node
     PRIVATE
     CHARACTER(:), ALLOCATABLE :: name
     INTEGER(SIK) :: ID
     CLASS(RealList), POINTER :: halflives
     CLASS(RealList), POINTER :: yields
   CONTAINS
     PROCEDURE, PASS :: add_isochild
  END TYPE IsoTreeNode

  TYPE, PUBLIC :: Isotope
     !An isotope
     CLASS(IsoTreeNode), ALLOCATABLE :: decay_chains
     CHARACTER(:), ALLOCATABLE :: name
     INTEGER(SIK) :: ID
     REAL(SRK) :: weight !Atomic weight
  END TYPE Isotope

CONTAINS

  SUBROUTINE add_isochild(self, iso, hflf, yield)
    CLASS(IsoTreeNode), INTENT(INOUT) :: self
    CLASS(IsoTreeNode), INTENT(INOUT) :: iso
    REAL(SRK), INTENT(IN), OPTIONAL :: hflf
    REAL(SRK), INTENT(IN), OPTIONAL :: yield
    TYPE(RealList) :: hfnode
    TYPE(RealList) :: ynode
    TYPE(Error) :: err

    IF (PRESENT(hflf)) THEN
       hfnode%val = hflf
    ELSE
       hfnode%val = STABLE
    END IF
    IF (PRESENT(yield)) THEN
       ynode%val = yield
    ELSE
       ynode%val = 1.0
    END IF
    
    CALL self%add_child(iso, err)
    IF (err%catch()) THEN
       CALL err%print()
       STOP(1)
    END IF
    
    CALL self%halflives%append(hfnode)
    CALL self%yields%append(ynode)
  END SUBROUTINE add_isochild
    
       
  
END MODULE Isotopes
