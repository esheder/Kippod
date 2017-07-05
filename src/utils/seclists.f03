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
  PUBLIC :: Section

  TYPE, PUBLIC, EXTENDS(LinkedList) :: Section
     ! A Linked list node that contains a line-list.
     PRIVATE
     CLASS(LineList), POINTER :: fline
     CHARACTER(MX_BFR), PUBLIC :: name
   CONTAINS
  END TYPE Section

  INTERFACE Section
     MODULE PROCEDURE new_Section
  END INTERFACE Section

CONTAINS

  FUNCTION new_Section(lines, name) RESULT(sec)
    CLASS(Section), POINTER :: sec
    CLASS(LineList), TARGET, INTENT(IN) :: lines
    CHARACTER(*), INTENT(IN), OPTIONAL :: name

    ALLOCATE(sec)
    CALL sec%init()
    sec%fline => lines
    IF (PRESENT(name)) sec%name = name
    
  END FUNCTION new_Section

END MODULE Seclists
