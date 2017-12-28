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
  USE Lists
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
     PROCEDURE, PASS :: destroy => chain_destroy
  END TYPE IsoTreeNode

  TYPE, PUBLIC :: Isotope
     !An isotope
     CLASS(IsoTreeNode), DIMENSION(:), ALLOCATABLE :: decay_chains
     CHARACTER(:), ALLOCATABLE :: name
     INTEGER(SIK) :: ID
     REAL(SRK) :: weight !Atomic weight
   CONTAINS
     PROCEDURE, PASS :: destroy => iso_destroy
     PROCEDURE, PASS :: init => iso_init
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
    
  SUBROUTINE iso_destroy(self)
    !Destructor of an isotope
    CLASS(Isotope), INTENT(INOUT) :: self
    CLASS(IsoTreeNode), POINTER :: p
    INTEGER :: i
    IF (ALLOCATED(self%decay_chains)) THEN
       DO i=1, SIZE(self%decay_chains)
          CALL self%decay_chains(i)%destroy()
       END DO
       NULLIFY(p)
       DEALLOCATE(self%decay_chains)
    END IF

    IF (ALLOCATED(self%name)) DEALLOCATE(self%name)

  END SUBROUTINE iso_destroy

  SUBROUTINE iso_init(self)
    !TODO: finish this
    CLASS(Isotope), INTENT(INOUT) :: self
  END SUBROUTINE iso_init

  SUBROUTINE chain_destroy(self)
    !Destructor of an isotope tree
    CLASS(IsoTreeNode), INTENT(INOUT) :: self

    IF (ASSOCIATED(self%halflives)) THEN
       CALL self%halflives%destroy()
       NULLIFY(self%halflives)
    END IF
    IF (ASSOCIATED(self%yields)) THEN
       CALL self%yields%destroy()
       NULLIFY(self%yields)
    END IF
    IF (ALLOCATED(self%name)) DEALLOCATE(self%name)
    CALL tree_destroy(self)

  END SUBROUTINE chain_destroy
  
END MODULE Isotopes
