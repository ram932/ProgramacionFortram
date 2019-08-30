program tiempovuelo
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927

  ! definimos las variables
  real :: a, v
  real :: Tt
 

  ! Leer valores para el ángulo a, y rapidez v
  write(*,*) 'Dame el angulo y rapidez iniciales para obtener el tiempo total de vuelo'
  read(*,*) a, v

  ! convirtiendo ángulo a radianes
  a = a * pi / 180.0
  
  ! la ecuacion para el tiempo total de vuelo Tt
  
  Tt = 2 * v * sin(a)/ g
  
 ! escribiendo el resultado en la pantalla
  write(*,*) 'Tt: ',Tt
  

end program tiempovuelo
