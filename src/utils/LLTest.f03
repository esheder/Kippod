PROGRAM LLTest
  USE Lists
  USE LineLists
  USE Parameters
  IMPLICIT NONE
  
  CLASS(LinkedList), POINTER :: root
  CLASS(LinkedList), POINTER :: cur
  CLASS(LineList), POINTER :: p
  INTEGER :: i
  
  root => LineList('First line is for root')
  cur => root
  SELECT TYPE(e=>cur%next)
  TYPE IS (LineList)
     PRINT*, 'LineList'
  TYPE IS (LinkedList)
     PRINT*, 'LinkedList'
  CLASS DEFAULT
     PRINT*, 'Unknown'
  END SELECT
  DO i = 1, 5
     cur%next => LineList("This is a continued line")
     cur => cur%next
  END DO
  cur => root
  
  DO WHILE (ASSOCIATED(cur%next))
     SELECT TYPE(e=>cur)
     TYPE IS (LineList)
        p => e
     CLASS DEFAULT
     END SELECT
     PRINT*, TRIM(p%line)
     cur => cur%next
  END DO
  
  PRINT*, "Selected real kind:", SRK
  PRINT*, "Selected integer kind:", SIK

  CALL root%destroy()
  DEALLOCATE(root)
  NULLIFY(root)
  NULLIFY(cur)
  NULLIFY(p)

END PROGRAM LLTest
