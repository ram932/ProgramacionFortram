program pade
implicit none

!declaracion de variables
real(kind=8), external :: exp_f02
real(kind=8) :: x, y, error, exp_a
integer :: i

!salida de datos

open (11, file = 'exp_f02.dat')

  do i = -3141592, 3141592, 1000
    x= i * 0.000001
     y = exp_f02(x)
      exp_a= exp(x)
       error = exp_a -(y/exp_a)
        print*, x, error !resultados 
  end do

print*, ''

close (11)

end program pade

!===================

function exp_f02(x)

!===================

implicit none

real (kind=8), intent(in):: x
real (kind=8) :: exp_f02, w, v

w = 1.00

v = 1.00 - x + (x**2.00) * (1.00/2.00)

exp_f02 = w/v



end function exp_f02