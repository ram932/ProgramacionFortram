program aproximacion_taylor
implicit none

 !Declaracion de variables
 real(kind=8),external:: ln_aprox  
 real(kind=8)::x,y
 integer:: n,i

   !Abrimos un documento para obtener la grafica
   open(11,file='ln.dat')
    !polinomio de grado 4 al 16
    do n=4,16,1
       if (n>4 .and. n<7) cycle
        if (n>7 .and. n<11) cycle
         if (n>11 .and. n<16) cycle

     !para x de -250 a 250
       do i=-250,250,1
        x=0.01*i         
         y=ln_aprox(x,n)  
          print*,x,y          
           write(11,*) x,y
       end do
        print*,' ' 
         write(11,*) ' '    
    end do
   close (11)

end program aproximacion_taylor

!==========================
function ln_aprox(x,n)   
!==========================
implicit none

 !valores externos
 real(kind=8),intent(in):: x 
 integer,intent(in):: n      
 real(kind=8):: ln_aprox    
 !variables
 real(kind=8):: t,s,a,b,c
 integer:: j

  s=0
  !grado 0 a n
   do j=1,n,1 
    a=(-1.0)**(j+1)
     b=x**j     
      c=j        

      t=a*(b/c)      
      s=s+t 
   end do

    ln_aprox=s

end function ln_aprox


