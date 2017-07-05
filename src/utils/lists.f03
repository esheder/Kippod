!A module that contains different kinds of linked lists that will be used later for I/O.
!
!Written by Eshed Magali
!List of changes:
!DATE              AUTHOR      CHANGE LOG
!July 3rd, 2017    Eshed       First working version
!July 5th, 2017    Eshed       Moved linelist to its own file.
!

MODULE Lists
  USE Exceptions
  IMPLICIT NONE
  PRIVATE
  
  TYPE, PUBLIC :: LinkedList
     !A general linked list that contains nothing. Will always be downcast for actual use.
     PRIVATE
     CLASS(LinkedList), PUBLIC, POINTER :: next => NULL()
     CLASS(LinkedList), POINTER :: first => NULL()
     CLASS(LinkedList), POINTER :: last => NULL()
   CONTAINS
     PROCEDURE, PASS :: destructor
     PROCEDURE, PASS :: init
     PROCEDURE, PASS :: append
  END TYPE LinkedList

  INTERFACE LinkedList
     MODULE PROCEDURE constructor_LinkedList
  END INTERFACE LinkedList
  
CONTAINS

  SUBROUTINE init(self)
    CLASS(LinkedList), TARGET, INTENT(INOUT) :: self

    self%first => self
    self%last => self
    self%next => NULL()
    
  END SUBROUTINE init
  
  RECURSIVE SUBROUTINE destructor(self)
    CLASS(LinkedList), INTENT(INOUT) :: self
    IF (ASSOCIATED(self%next)) THEN
       CALL self%next%destructor()
       DEALLOCATE(self%next)
    END IF
  END SUBROUTINE destructor

  SUBROUTINE append(self, node)
    !Current implementation allows the creation of a looping chain. Beware of infinite loops.
    CLASS(LinkedList), INTENT(INOUT) :: self
    CLASS(LinkedList), TARGET, INTENT(INOUT) :: node
    CLASS(LinkedList), POINTER :: p

    p => self%first

    DO WHILE (ASSOCIATED(p%next))
       p%last => node%last
       p => p%next
    END DO
    
    p%next => node
    p => node

    DO WHILE (ASSOCIATED(p%next))
       p%first => self%first
       p => p%next
    END DO
    
  END SUBROUTINE append

  FUNCTION constructor_LinkedList() RESULT(node)
    Class(LinkedList), POINTER :: node

    ALLOCATE(node)
    CALL node%init()
  END FUNCTION constructor_LinkedList
  
END MODULE Lists
