Program Euler
 implicit none
!declaracion de variables
  real:: ANG, h, B, t, y, w_0
  integer:: j
  real,dimension(2):: M
  real,parameter:: g = 9.81
  real,parameter:: l = 9.81
  !open
 open(1, file="euler.dat")
 
   w_0 = sqrt(g/l)	
   print*," Ingrese el angulo y el numero de pasos"
   read(*,*) ANG, h
 
   do j=0,5000
    t=float(j)* h
     if(t>6.3) exit
      y=ANG*cos(w_0*t)
       print*, t, y
        write(1,*) t,y,1
   end do

  write(1,*) " "
  ANG=B
   do j=0,5000
    t=float(j)*h
    if(t>6.3) exit
   call Matriz(ANG, w_0, h, l, g, M)
     write(1,*) t, M(1), 2
    ANG = M(1)
    w_0 = M(2)
   end do

  close(1)

!error
print*, "Error", abs((B-ANG)/B)

End Program Euler


subroutine Matriz(ANG, w_0, h, g, l, M)
  implicit none

  real,intent(in):: ANG, w_0, h, g, l
  real,dimension(2),intent(out):: M
  real:: a_1, a_2, w_2, w
  real,dimension(2):: P_1
  real,dimension(2):: P_2

  a_1 = ANG
    W = w_0
     a_2 = h*w
      w_2 = -h * g / l *a
       P_1 = (/a, w/)
        P_2 = (/a_2, w_2/)

    M = P_1 + P_2

end subroutine Matriz


