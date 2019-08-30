program projectile
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927

  ! definimos las variables
  real :: a, t, u, x, y
  real :: theta, v, vx, vy

  ! Leer valores para el ángulo a, el tiempo t, y la velocidad inicial u desde la terminal
  write(*,*) 'Dame el ángulo, el tiempo y la rapidez inicial'
  read(*,*) a, t, u

  ! convirtiendo ángulo a radianes
  a = a * pi / 180.0
  
  ! las ecuaciones de la posición en x y y
  
  x = u * cos(a) * t
  y = u * sin(a) * t - 0.5 * g * t * t

  ! La velocidad al tiempo t
  vx = u * cos(a)
  vy = u * sin(a) - g * t
  v = sqrt(vx * vx + vy * vy)
  theta = atan(vy / vx) * 180.0 / pi
 
 ! escribiendo el resultado en la pantalla
  write(*,*) 'x: ',x,'  y: ',y
  write(*,*) 'v: ',v,'  theta: ',theta

end program projectile
