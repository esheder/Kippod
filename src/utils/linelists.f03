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
  PUBLIC :: LineList, constructor_LineList, ListDowncast, split
  
  TYPE, PUBLIC, EXTENDS(LinkedList) :: LineList
     !A linked list node that contains a string.
     PRIVATE
     CHARACTER(:), PUBLIC, ALLOCATABLE :: line 
   CONTAINS
     PROCEDURE, PASS :: print => print_lines
     PROCEDURE, PASS, PUBLIC :: destroy => destroy
  END TYPE LineList
  
  INTERFACE LineList
     MODULE PROCEDURE constructor_LineList
  END INTERFACE LineList
  
  INTERFACE ListDowncast
     MODULE PROCEDURE downto_linelist_fun
  END INTERFACE ListDowncast

CONTAINS

  RECURSIVE SUBROUTINE destroy(self)
    CLASS(LineList), TARGET, INTENT(INOUT) :: self
    IF (ALLOCATED(self%line)) DEALLOCATE(self%line)
    IF (ASSOCIATED(self%next)) THEN
       CALL self%next%destroy()
       DEALLOCATE(self%next)
    END IF
  END SUBROUTINE destroy
  
  FUNCTION constructor_LineList(line) RESULT(node)
    CLASS(LineList), POINTER :: node
    CHARACTER(*), INTENT(IN) :: line

    NULLIFY(node)
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

  FUNCTION downto_linelist_fun(list) RESULT(nlist)
    CLASS(LinkedList), TARGET, INTENT(IN) :: list
    TYPE(LineList), POINTER :: nlist
    
    SELECT TYPE(e=>list)
    TYPE IS (LineList)
       nlist => e
    CLASS DEFAULT
       nlist => null()
    END SELECT
    
  END FUNCTION downto_linelist_fun

  
  FUNCTION split(line) RESULT(wordlist)
    !Assumes the line is left adjusted and trimmed.
    CHARACTER(*), INTENT(IN) :: line
    CLASS(LineList), POINTER :: wordlist
    CLASS(LineList), POINTER :: tmp
    INTEGER :: s,e

    s = 1
    NULLIFY(wordlist)
    NULLIFY(tmp)
    DO WHILE (s .LE. LEN(line))
       e = INDEX(line(s:), ' ') - 1
       IF (e .LE. 0) e = LEN(line(s:))
       IF (s .EQ. 1) THEN
          wordlist => LineList(line(s:s+e))
       ELSE
          tmp => LineList(line(s:s+e))
          CALL wordlist%append(tmp)
       END IF
       s = s + e + 1
    END DO

    NULLIFY(tmp)
    wordlist => ListDowncast(wordlist%first)

  END FUNCTION split

  SUBROUTINE print_lines(self)
    CLASS(LineList), INTENT(IN) :: self
    CLASS(LineList), POINTER :: p

    p => ListDowncast(self%first)
    DO WHILE (ASSOCIATED(p))
       WRITE(*, *) TRIM(p%line)
       p => ListDowncast(p%next)
    END DO
  END SUBROUTINE print_lines

END MODULE linelists
