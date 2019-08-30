program distanciamaxima
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927

  ! definimos las variables
  real :: a, v
  real :: dmax
 

  ! Leer valores para el ángulo a, y rapidez v
  write(*,*) 'Dame el angulo y rapidez iniciales para obtener la distancia maxima'
  read(*,*) a, v

  ! convirtiendo ángulo a radianes
  a = a * pi / 180.0
  
  ! la ecuacion para calcular la altura maxima hmax
  
  dmax = v **2 * sin(2*a)/ g
  
 ! escribiendo el resultado en la pantalla
  write(*,*) 'dmax: ',dmax
  

end program distanciamaxima
