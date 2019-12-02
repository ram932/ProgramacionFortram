  
PROGRAM Paralelepipedo
IMPLICIT NONE

 REAL :: a, b, c, v
  PRINT *, 'Ingresar los valores a, b, c para calcular el volumen del paralelepipedo'
  READ *, a, b, c
  PRINT *, 'Volumen del paralelepipedo', v(a,b,c)

END PROGRAM Paralelepipedo

FUNCTION v(x,y,z)

IMPLICIT NONE

 REAL :: v !Tipo Function
 REAL, INTENT( IN ) :: x, y, z
 
v= x*y*z

END FUNCTION v
