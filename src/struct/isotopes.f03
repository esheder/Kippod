!A module that defines what an isotope is.
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!
!

MODULE Isotopes
  Use Trees
  USE Parameters
  
  IMPLICIT NONE
  PRIVATE


  TYPE, PUBLIC, EXTENDS(TreeNode) :: IsoTreeNode
     !An isotope decay chain node
     PRIVATE
     CHARACTER(:), ALLOCATABLE :: name
     INTEGER :: ID
     LOGICAL :: decay
     REAL(SRK) :: halflife
     REAL(SRK), DIMENSION(:), ALLOCATABLE :: yields
  END TYPE IsoTreeNode

  TYPE, PUBLIC :: Isotope
     !An isotope
     CLASS(IsoTreeNode), ALLOCATABLE :: decay_chains
     CHARACTER(:), ALLOCATABLE :: name
     INTEGER, ALLOCATABLE :: ID
  END TYPE Isotope
  
END MODULE Isotopes
