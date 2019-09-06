program projectile
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927
  integer :: n, npasos, m

  ! definimos las variables
  real :: a, t, u, x, y, dt, angulo, dangulo

  ! output data into a file 
  open(11, file = 'salida.dat', status = 'unknown') 
       
  
  

  ! Leer valores la velocidad inicial u, y el tiempo de vuelo y el angulo a desde la terminal
  write(*,*) 'Dame la velocidad inicial, el angulo y el numerop de pasos'
  read(*,*) u, a, npasos
! convirtiendo ángulo a radianes
       a = a * pi / 180.0
       dt= 0.1

      

   ! Loop
     
      
    
    dangulo = 15.0
    
    dangulo = dangulo * pi / 180.0
    
    do m = 1, 6
    
    angulo = float(m) * dangulo

     do n = 0, npasos
      t= float(n)* dt
      x = u * cos(angulo) * t
      y = u * sin(angulo) * t - 0.5 * g * t * t
       
       print *, n, x, y
       write(11,*) x, y

      if (y<0.0) exit

      end do 

    write(11,*) " "

     end do


 
    


end program projectile
