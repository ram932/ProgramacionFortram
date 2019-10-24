program aproximacion_taylor
implicit none

 !Declaracion de variables
 REAL(kind=8),external:: senotaylor
 REAL(kind=8)::y,x
 INTEGER:: n,i,a



  
   !Abrimos un open para obtener la grafica
   open(11, file = 'salida.dat', status = 'unknown') 
    ! polinomio grado 1, 3, 5, 7, 9 y 11

    do n=1,11,2
       
      
       ! para el valor de la x yendo desde -100 hasta 100
       do i=-100,100,1
         x= 0.1 * i   
         y=senotaylor(x,n)       
         print*, x,y
          write(11,*) x, y
       end do
         write(11,*) ''    
    end do
   close (11)

end program aproximacion_taylor

!==========================
function senotaylor(x,n)   
!==========================
implicit none
!declaracion de variables
 real(kind=8),intent(in):: x 
 integer,intent(in):: n      
 real(kind=8):: senotaylor
 real(kind=8)::a,b,c,d, s, t
 integer:: j

  s=0 

   ! de 0 hasta n
        do j=0,n,1 !taylor
          a=(-1.0)**j 
          b=2*j+1     
          c=x**b
           d=b     
            
factorial:do
           d=d-1
            b=b*d
             if(d==1) exit 
              if(d==0) then 
                  b=1        
                 exit
              end if   
           end do factorial
            

    !sumatoria

     t=(a/b)*c      
     s=s+t
   end do !taylor

    senotaylor=s

end function senotaylor





