!A module specifically to read lines decribing which isotopes decay/transmute into other isotopes
!and the like.
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!
!

MODULE IsotopeParsers
  USE Isotopes
  USE Parsers
  USE LineLists
  USE Exceptions
  IMPLICIT NONE
  PRIVATE


  TYPE, PUBLIC, EXTENDS(DeflineParser) :: IsotopeParser
     !A worker that reads the lines that define the isotopes and generates the relevant data.
     CLASS(Isotope), ALLOCATABLE :: isos

   CONTAINS
     PROCEDURE, PASS :: parse => parse_iso
  END TYPE IsotopeParser
  
CONTAINS
  
  SUBROUTINE parse_iso(self, sec, err)
    !Parse the isotope lines to create isotopes
    CLASS(IsotopeParser), INTENT(INOUT) :: self
    CLASS(LineList), TARGET, INTENT(IN) :: sec
    CLASS(Exception), INTENT(OUT) :: err
  END SUBROUTINE parse_iso
END MODULE IsotopeParsers
