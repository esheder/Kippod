!A module that contains different kinds of linked lists that will be used later for I/O.
!
!Written by Eshed Magali
!List of changes:
!DATE              AUTHOR      CHANGE LOG
!July 3rd, 2017    Eshed       First working version
!
!

MODULE Lists
  USE Exceptions
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: LineList, constructor_LineList
  INTEGER, PUBLIC, PARAMETER :: MX_BFR = 500
  
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
  
  TYPE, PUBLIC, EXTENDS(LinkedList) :: LineList
     !A linked list node that contains a string.
     PRIVATE
     CHARACTER(MX_BFR), PUBLIC :: line = ''
  END TYPE LineList
  
  INTERFACE LineList
     MODULE PROCEDURE constructor_LineList
  END INTERFACE LineList

  INTERFACE LinkedList
     MODULE PROCEDURE constructor_LinkedList
  END INTERFACE LinkedList
  
  INTERFACE ListDowncast
     MODULE PROCEDURE downto_linelist_err

     MODULE PROCEDURE downto_linelist
  END INTERFACE ListDowncast
  
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
  
  FUNCTION constructor_LineList(line) RESULT(node)
    CLASS(LineList), POINTER :: node
    CHARACTER(*), INTENT(IN) :: line

    ALLOCATE(node)
    CALL node%init()
    node%line = line
    
  END FUNCTION constructor_LineList
  



  SUBROUTINE downto_linelist_err(list, nlist, err)
    CLASS(LinkedList), TARGET, INTENT(IN) :: list
    TYPE(LineList), POINTER, INTENT(OUT) :: nlist
    CLASS(TypeError), INTENT(OUT) :: err
    
    SELECT TYPE(e=>list)
    TYPE IS (LineList)
       nlist => e
    CLASS DEFAULT
       nlist => null()
       CALL err%raise('Tried to downcast something that wasn''t a LineList into a LineList')
       CALL err%set_priority(ERRCODE_FAIL)
    END SELECT
    
  END SUBROUTINE downto_linelist_err
  
  SUBROUTINE downto_linelist(list, nlist)
    CLASS(LinkedList), TARGET, INTENT(IN) :: list
    TYPE(LineList), POINTER, INTENT(OUT) :: nlist
    
    SELECT TYPE(e=>list)
    TYPE IS (LineList)
       nlist => e
    CLASS DEFAULT
       nlist => null()
    END SELECT
    
  END SUBROUTINE downto_linelist
END MODULE Lists
