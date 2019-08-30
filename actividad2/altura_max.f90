program alturamaxima
  implicit none

  ! definimos constantes
  real, parameter :: g = 19.6
  real, parameter :: pi = 3.1415927

  ! definimos las variables
  real :: a, v
  real :: hmax
 

  ! Leer valores para el ángulo a, y rapidez v
  write(*,*) 'Dame el angulo y rapidez iniciales para obtener la altura maxima'
  read(*,*) a, v

  ! convirtiendo ángulo a radianes
  a = a * pi / 180.0
  
  ! la ecuacion para calcular la altura maxima hmax
  
  hmax = v **2* sin(a)**2/ g
  
 ! escribiendo el resultado en la pantalla
  write(*,*) 'hmax: ',hmax
  

end program alturamaxima
