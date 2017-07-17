!A module that contains different kinds of linked lists that will be used later for I/O.
!
!Written by Eshed Magali
!List of changes:
!DATE              AUTHOR      CHANGE LOG
!

MODULE RealLists
  USE Lists
  USE Parameters

  IMPLICIT NONE
  PRIVATE

  TYPE, PUBLIC, EXTENDS(LinkedList) :: RealList
     !A linked list of real numbers
     REAL(SRK), PUBLIC :: val
  END TYPE RealList

CONTAINS
  FUNCTION DowncastTo(lst) RESULT(rlst)
    CLASS(LinkedList), TARGET, INTENT(IN) :: lst
    CLASS(RealList), POINTER :: rlst

    SELECT TYPE(e=>lst)
    TYPE IS (RealList)
       rlst => e
    CLASS DEFAULT
       rlst => NULL()
    END SELECT

  END FUNCTION DowncastTo

END MODULE RealLists
