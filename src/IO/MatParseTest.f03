PROGRAM MatParseTest
  
  USE MaterialParsers
  USE Seclists
  USE LineLists
  USE Lists
  USE Exceptions

  IMPLICIT NONE

  TYPE(LineList), POINTER :: fln => null()
  TYPE(LineList), POINTER :: ln => null()
  TYPE(MaterialParser) :: mp
  TYPE(ValueError) :: err

  fln => LineList('{materials')
  ln => LineList('My line is stupid and fails')
  CALL fln%append(ln)
  
  CALL mp%parse(fln, err)
  IF (err%catch()) CONTINUE
  CALL err%del()

  CALL mp%destroy()

  !IF (ASSOCIATED(fln)) WRITE(*,*) 'poop'
  fln => LineList('{materials')
  ln => LineList('FUEL file=/home/eshedm/Yourmom/Crossections.ext set=1 burn=1')
  CALL fln%append(ln)

  CALL mp%parse(fln, err)

  CALL mp%destroy()

END PROGRAM MatParseTest
