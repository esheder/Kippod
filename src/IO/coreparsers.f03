!A module specifically to read a cip-like input file made up of sections to describe a core.
!
!Written by Eshed Magali
!List of Changes:
!DATE              AUTHOR      CHANGE LOG
!
!

MODULE CoreParsers
  USE Parameters
  USE Parsers
  USE MaterialParsers
  USE ReflectorParsers
  USE Exceptions
  USE Lists
  USE LineLists
  USE Seclists
  IMPLICIT NONE
  PRIVATE

  CHARACTER(*), PARAMETER :: SECEND='}'
  CHARACTER(*), PARAMETER :: SECMAT='{materials'
  CHARACTER(*), PARAMETER :: SECREF='{reflectors'
  CHARACTER(*), PARAMETER :: SECISO='{isotopes'
  CHARACTER(*), PARAMETER :: SECROD='{rods'
  CHARACTER(*), PARAMETER :: SECLOAD='{coremap'
  CHARACTER(*), PARAMETER :: SECOPERT='{cycles'
  CHARACTER(*), PARAMETER :: SECOPTS='{options'
  CHARACTER(*), PARAMETER :: EMPTY=''
  
  TYPE, PUBLIC, EXTENDS(FileParser) :: CoreFileParser
     !A Worker that reads the general input that defines the core in stages. This is just the
     !first step, as sections must be parsed with a set of specialized string parsers.
     PRIVATE
     !TYPE(SectionParser) :: sec_prsr
     TYPE(MaterialParser), POINTER :: mat_prsr
     TYPE(ReflectorsParser), POINTER :: ref_prsr
     !TYPE(IsotopeParser) :: iso_prsr
     !TYPE(RodsParser) :: rod_prsr
     !TYPE(LoadingParser) :: load_prsr
     !TYPE(OperationParser) :: oprt_prsr
     !TYPE(OptionsParser) :: opt_prsr
     TYPE(SectionList), POINTER :: secs => NULL()
   CONTAINS
     PROCEDURE, PASS :: parse_file => core_parse
     PROCEDURE, PASS :: init
  END TYPE CoreFileParser

CONTAINS

  SUBROUTINE init(self)
    CLASS(CoreFileParser), INTENT(INOUT) :: self
    !Empty right now

  END SUBROUTINE init

  SUBROUTINE core_parse(self, err)
    !Parse the core file into a bunch of sections, and then parse each section using its specific
    !parser.

    CLASS(CoreFileParser), INTENT(INOUT) :: self
    CLASS(Exception), INTENT(OUT) :: err
    TYPE(IOError) :: err2
    CLASS(LineList), POINTER :: lines
    CLASS(LineList), POINTER :: p
    CLASS(DeflineParser), POINTER :: prsr
    CHARACTER(MX_BFR) :: line
    INTEGER :: io, pp
    LOGICAL :: restart

    CALL self%open_file(err2)
    IF (err2%catch()) THEN
       CALL err2%print()
       STOP(1)
    END IF
    
    READ(UNIT=self%pipe, FMT=*, IOSTAT=io) line
    line = clean(line)
    DO WHILE (io .EQ. 0)
       restart = .TRUE.
       line = clean(line)
       SELECT CASE (TRIM(ADJUSTL(line)))
       CASE (SECMAT)
          prsr => self%mat_prsr
       CASE (SECREF)
          prsr => self%ref_prsr
       CASE (SECISO)
!          prsr => self%iso_prsr
       CASE (SECROD)
!          prsr => self%rod_prsr
       CASE (SECLOAD)
!          prsr => self%load_prsr
       CASE (SECOPERT)
!          prsr => self%oprt_prsr
       CASE (SECOPTS)
!          prsr => self%opt_prsr
       CASE (SECEND)
          restart = .FALSE.
          CALL prsr%parse(lines, err)
          IF (err%catch()) THEN
             CALL err%print()
             STOP(1)
          END IF
       CASE (EMPTY)
          restart = .FALSE.
       CASE DEFAULT
          p => LineList(line)
          CALL lines%append(p)
          restart = .FALSE.
       END SELECT

       IF (restart) THEN
          CALL lines%destroy()
          lines => LineList(TRIM(line))
       END IF
          
       READ(self%pipe, *, IOSTAT=io) line
    END DO

    CALL self%close_file(err2)
    IF (err2%catch()) THEN
       CALL err2%print()
       STOP(1)
    END IF

  END SUBROUTINE core_parse

  FUNCTION clean(bfr) RESULT (ln)
    CHARACTER(*), INTENT(IN) :: bfr
    CHARACTER(:), ALLOCATABLE :: ln
    INTEGER :: k
    !First clear all after comments. Comments by !, as # is saved for other things.
    k = INDEX(bfr, '!')
    IF (k .GT. 0) THEN
       ln = TRIM(ADJUSTL(bfr(1:k-1)))
    ELSE
       ln = TRIM(ADJUSTL(bfr))
    END IF
  END FUNCTION clean

END MODULE CoreParsers
