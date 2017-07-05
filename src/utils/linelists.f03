!A module that contains the linked list where each node is a string.
!
!Written by Eshed Magali
!List of changes:
!DATE              AUTHOR      CHANGE LOG
!July 3rd, 2017    Eshed       First working version
!

MODULE linelists
  USE Exceptions
  USE Lists
  USE Parameters
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: LineList, constructor_LineList
  
  TYPE, PUBLIC, EXTENDS(LinkedList) :: LineList
     !A linked list node that contains a string.
     PRIVATE
     CHARACTER(MX_BFR), PUBLIC :: line = ''
  END TYPE LineList
  
  INTERFACE LineList
     MODULE PROCEDURE constructor_LineList
  END INTERFACE LineList
  
  INTERFACE ListDowncast
     MODULE PROCEDURE downto_linelist_err

     MODULE PROCEDURE downto_linelist
  END INTERFACE ListDowncast

CONTAINS
  
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

END MODULE linelists
