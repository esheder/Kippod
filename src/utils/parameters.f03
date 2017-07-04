!Module that contains global parameters that cannot be changed by a user because they are too
!ingrained into the software. This should be as empty as possible.
!Written by Eshed Magali
!List of changes:
!DATE              AUTHOR      CHANGE LOG
!June 19, 2017     Eshed       First working version  
!

MODULE Parameters
  IMPLICIT NONE

  PUBLIC
  INTEGER, PARAMETER :: SRK = SELECTED_REAL_KIND(13,20) !Selected Real Kind.
  INTEGER, PARAMETER :: SIK = SELECTED_INT_KIND(10)     !Selected Integer Kind.
  INTEGER, PARAMETER :: MX_PATHLENGTH = 500             !Maximum of characters in a file path
END MODULE Parameters
