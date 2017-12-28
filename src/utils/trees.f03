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
     PROCEDURE, PASS :: destroy => tree_destroy
     PROCEDURE, PASS :: root
     PROCEDURE, PASS :: add_child
  END TYPE TreeNode

  TYPE, PUBLIC, EXTENDS(LinkedList) :: TreeList
     !A linked list of tree nodes.
     !
     CLASS(TreeNode), POINTER :: val

   CONTAINS
     PROCEDURE, PASS :: destroy => list_destroy
     
  END TYPE TreeList

CONTAINS

  SUBROUTINE init(self, parent, children)
    !Initialize a root
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
    !Add a new child to this node
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

  RECURSIVE SUBROUTINE tree_destroy(self)
    !Destroy a tree from this point downwards
    CLASS(TreeNode), INTENT(INOUT) :: self
    CLASS(TreeList), POINTER :: p

    p => self%children
    DO WHILE (ASSOCIATED(p))
       CALL p%val%destroy()
       SELECT TYPE(e=>p%next)
       CLASS IS (TreeList)
          p => e
       CLASS DEFAULT
          p => NULL()
       END SELECT
    END DO
    NULLIFY(self%parent)
    CALL self%children%destroy()
    
  END SUBROUTINE tree_destroy

  RECURSIVE SUBROUTINE list_destroy(self)
    !Destroy a list of trees. Don't touch the trees.
    CLASS(TreeList), TARGET, INTENT(INOUT) :: self
    CLASS(TreeList), POINTER :: p

    p => self
    DO WHILE (ASSOCIATED(p))
       SELECT TYPE (e=>p%next)
       CLASS IS (TreeList)
          CALL e%destroy()
       CLASS DEFAULT
          p => NULL()
       END SELECT
       DEALLOCATE(p%next)
       NULLIFY(p%val)
    END DO

  END SUBROUTINE list_destroy

END MODULE Trees
