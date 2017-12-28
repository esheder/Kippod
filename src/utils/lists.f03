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
  PUBLIC :: LEN
  
  TYPE, PUBLIC :: LinkedList
     !A general linked list that contains nothing. Will always be downcast for actual use.
     PRIVATE
     CLASS(LinkedList), PUBLIC, POINTER :: next => NULL()
     CLASS(LinkedList), PUBLIC, POINTER :: first => NULL()
     CLASS(LinkedList), PUBLIC, POINTER :: last => NULL()
   CONTAINS
     PROCEDURE, PASS :: destroy => destroy_list
     PROCEDURE, PASS :: init
     PROCEDURE, PASS :: append
  END TYPE LinkedList

  INTERFACE LinkedList
     MODULE PROCEDURE constructor_LinkedList
  END INTERFACE LinkedList

  INTERFACE LEN
     MODULE PROCEDURE length
  END INTERFACE LEN
  
CONTAINS

  SUBROUTINE init(self)
    CLASS(LinkedList), TARGET, INTENT(INOUT) :: self

    self%first => self
    self%last => self
    self%next => NULL()
    
  END SUBROUTINE init
  
  RECURSIVE SUBROUTINE destroy_list(self)
    CLASS(LinkedList), TARGET, INTENT(INOUT) :: self
    IF (ASSOCIATED(self%next)) THEN
       CALL self%next%destroy()
       DEALLOCATE(self%next)
    END IF
  END SUBROUTINE destroy_list

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

  FUNCTION length(list) RESULT(cnt)
    !Checks for length. Assumes non-cyclic chains.
    INTEGER :: cnt
    CLASS(LinkedList), POINTER, INTENT(IN) :: list
    CLASS(LinkedList), POINTER :: ptr
    CLASS(LinkedList), POINTER :: eptr

    cnt = 0
    IF (.NOT. ASSOCIATED(list)) RETURN
    ptr => list%first
    DO WHILE (ASSOCIATED(ptr))
       cnt = cnt + 1
       ptr => ptr%next
    END DO

  END FUNCTION length

  SUBROUTINE listpop(self, res)
    !This isn't a procedure for lists because for some reason we can't assume self would be a
    !pointer to a list, so we can't just do regular pointer actions here.
    CLASS(LinkedList), POINTER, INTENT(INOUT) :: self
    CLASS(LinkedList), POINTER, OPTIONAL, INTENT(OUT) :: res
    CLASS(LinkedList), POINTER :: newf

    newf => self%next
    self => newf
    IF (PRESENT(res)) THEN
       res => self%first
       res%last => NULL()
       res%next => NULL()
    ELSE
       DEALLOCATE(self%first)
    END IF

    DO WHILE (ASSOCIATED(self))
       self%first => newf
       self => self%next
    END DO
    
    self => newf

  END SUBROUTINE listpop
  
END MODULE Lists
