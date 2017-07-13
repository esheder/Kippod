!A module that contains the section lines lists class.
!
!Written by Eshed Magali
!List of changes:
!DATE              AUTHOR      CHANGE LOG
!July 5th, 2017    Eshed       First working version
!

MODULE Seclists
  USE Lists
  USE LineLists
  USE Parameters
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: SectionList

  TYPE, PUBLIC, EXTENDS(LinkedList) :: SectionList
     ! A Linked list node that contains a line-list.
     PRIVATE
     CLASS(LineList), POINTER, PUBLIC :: fline
     CHARACTER(:), PUBLIC, ALLOCATABLE :: name
   CONTAINS
     PROCEDURE, PASS :: destructor => destroy
  END TYPE SectionList
  
  INTERFACE SectionList
     MODULE PROCEDURE new_Section
  END INTERFACE SectionList

CONTAINS

  FUNCTION new_Section(lines, name) RESULT(sec)
    CLASS(SectionList), POINTER :: sec
    CLASS(LineList), TARGET, INTENT(IN) :: lines
    CHARACTER(*), INTENT(IN), OPTIONAL :: name

    ALLOCATE(sec)
    CALL sec%init()
    sec%fline => lines
    IF (PRESENT(name)) sec%name = name
    
  END FUNCTION new_Section

  
  RECURSIVE SUBROUTINE destroy(self)
    CLASS(SectionList) , INTENT(INOUT):: self
    IF (ASSOCIATED(self%fline)) THEN
       CALL self%fline%destructor()
       DEALLOCATE(self%fline)
       IF (ALLOCATED(self%name)) DEALLOCATE(self%name)
    END IF
    IF (ASSOCIATED(self%next)) THEN
       CALL self%next%destructor()
       DEALLOCATE(self%next)
    END IF
  END SUBROUTINE destroy

END MODULE Seclists
