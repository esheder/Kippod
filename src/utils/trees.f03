!A module specifically to define tree structures that hold data.
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!
!

MODULE TreeS
  USE Lists
  
  IMPLICIT NONE
  PRIVATE

  TYPE, PUBLIC :: TreeNode
     !A node in a tree structure.
     !
     CLASS(TreeNode), POINTER :: parent => NULL()
     CLASS(TreeList), POINTER :: children => NULL()

   CONTAINS
     PROCEDURE, PASS :: init
  END TYPE TreeNode

  TYPE, PUBLIC, EXTENDS(LinkedList) :: TreeList
     !A linked list of tree nodes.
     !
     CLASS(TreeNode), POINTER :: val
     
  END TYPE TreeList

CONTAINS

  SUBROUTINE init(self, parent, children)
    !
    CLASS(TreeNode), INTENT(INOUT) :: self
    CLASS(TreeNode), INTENT(IN), OPTIONAL, TARGET :: parent
    CLASS(TreeList), INTENT(IN), OPTIONAL, TARGET :: children

    IF (PRESENT(parent)) self%parent => parent
    IF (PRESENT(children)) self%children => children

  END SUBROUTINE init

END MODULE Trees
