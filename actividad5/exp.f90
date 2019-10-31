program Aproximacion_taylor
implicit none

 !Declaracion de variables 
 real(kind=8),external:: exp_aprox
 real(kind=8)::y,x
 integer:: n,i

   open(11,file='exponencial.dat')
    !hasta 8 terminos
    do n=1,8,1

       ! para x 
       do i=-500,500,1
        x=0.01 * i       
         y=exp_aprox(x,n)
          print*,x,y                    
           write(11,*) x,y
       end do
        write(11,*) ' '    
    end do
   close (11)

end program Aproximacion_taylor

!==========================
function exp_aprox(x,n)   
!==========================
implicit none

 !declaracion de variables
 real(kind=8),intent(in):: x
 integer,intent(in):: n      
 real(kind=8):: exp_aprox   
 real(kind=8):: s,t,c,b,a
 integer:: j

  s=0 ! suma

   
 Aproximacion_taylor:do j=0,n,1 
                         b=x**j 
                          a=j    
                           c=a       
     Factorial:do !calculamos el factorial
                  if(c==0) then 
                     a=1         
                      exit      
                end if   
                  c=c-1             
                   a=a*c
                    if(c==1) exit 
                     if(c==0) then 
                      a=1       
                      exit
                end if   
               end do Factorial

                  t=b/a    
                  s=s+t  
                     end do Aproximacion_taylor

    exp_aprox=s 

end function exp_aprox
