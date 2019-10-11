program pade
implicit none

real(kind=8), external :: seno_p
real(kind=8) :: seno_z, x, y, error_p
integer :: i

open (11, file = 'errorseno_p')
  
   do i = 0, 31415926, 1000
     x = i * 0.0000001
      y = seno_p(x)
       seno_z=Sin(x)
        error_p = seno_z - (y / seno_z)
         print*, x, error_p
   end do
   
   close (11)
   
end program pade

!===================

function seno_p(x)

!===================

implicit none

real (kind=8), intent(in):: x
real (kind=8) :: seno_p, seno_w, seno_v

seno_w = x - (x**3) * (2363.0/18183.0) + (x**5) * (12671.0/4363920.0)

seno_v = 1 + (x**2) * (445.0/12122.0) + (x**4) * (601.0/872784.0) + (x**6)*(121.0/16662240.0)

seno_p = seno_w/seno_v

end function seno_p
