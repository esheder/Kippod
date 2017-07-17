!A module specifically to define tree structures that hold data.
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!
!

MODULE TreeS
  USE Lists
  USE Exceptions
  
  IMPLICIT NONE
  PRIVATE

  TYPE, PUBLIC, ABSTRACT :: TreeNode
     !A node in a tree structure.
     !
     CLASS(TreeNode), POINTER :: parent => NULL()
     CLASS(TreeList), POINTER :: children => NULL()

   CONTAINS
     PROCEDURE, PASS :: init
     PROCEDURE, PASS :: root
     PROCEDURE, PASS :: add_child
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

  FUNCTION root(self) RESULT (node)
    !Get the root of a tree from its node.
    CLASS(TreeNode), TARGET, INTENT(IN) :: self
    CLASS(TreeNode), POINTER :: node

    node => self
    DO WHILE (ASSOCIATED(node%parent))
       node => node%parent
    END DO
  END FUNCTION root

  SUBROUTINE add_child(self, child, err)
    CLASS(TreeNode), TARGET, INTENT(INOUT) :: self
    CLASS(TreeNode), TARGET, INTENT(INOUT) :: child
    TYPE(TreeList) :: node
    TYPE(Error), INTENT(OUT) :: err
    IF (ASSOCIATED(child%parent)) THEN
       CALL err%raise('Child already has a parent but a new one was assigned to it!')
       RETURN
    END IF
    child%parent => self
    IF (ASSOCIATED(self%children%val)) THEN
       CALL node%init()
       node%val => child
       CALL self%children%append(node)
    ELSE
       self%children%val => child
    END IF

  END SUBROUTINE add_child

END MODULE Trees
