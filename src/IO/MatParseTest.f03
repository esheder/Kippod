PROGRAM MatParseTest
  
  USE MaterialParsers
  USE Seclists
  USE LineLists
  USE Lists
  USE Exceptions

  IMPLICIT NONE

  TYPE(SectionList), POINTER :: sec
  TYPE(LineList), POINTER :: fln
  TYPE(LineList), POINTER :: ln
  TYPE(MaterialParser) :: mp
  TYPE(Error) :: err

  ALLOCATE(sec)
  CALL sec%init()
  fln => LineList('{materials')
  ln => LineList('My line is stupid and fails')
  CALL fln%append(ln)
  
  sec%fline => fln
  sec%name = 'Materials'

  mp%raw_sec => sec
  CALL mp%parse(err)
  IF (err%catch()) CONTINUE

  CALL mp%destructor()

  fln => LineList('{materials')
  ln => LineList('FUEL file=/home/eshedm/Yourmom/Crossections.ext set=1 burn=1')
  CALL fln%append(ln)

  ALLOCATE(sec)
  CALL sec%init()
  
  sec%fline => fln

  mp%raw_sec => sec
  CALL mp%parse(err)

END PROGRAM MatParseTest
