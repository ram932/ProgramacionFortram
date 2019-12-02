PROGRAM ONE_D_MOTION
IMPLICIT NONE
  
! Program for the motion of a particle subject to an external force f(x) = -x.
! We have divided the total time 2*pi into 10000 intervals with an equal time
! step. The position and velocity of the particle are written out at every 500
! steps.

 INTEGER, PARAMETER :: N=10001,IN=500
 INTEGER :: I
 REAL :: PI,DT
 REAL, DIMENSION (N):: T,V,X

! Assign constants, initial position, and initial velocity

  PI   = 4.0*ATAN(1.0)
  DT   = 2.0*PI/FLOAT(N-1)
  X(1) = 0.0
  T(1) = 0.0
  V(1) = 1.0

! Recursion for position and velocity at later time

   DO I = 1, N-1
     T(I+1) = DT*I
     X(I+1) = X(I)+V(I)*DT
     V(I+1) = V(I)-X(I)*DT
   END DO

! Write the position and velocity every 500 steps
  OPEN (6,FILE='problemados.dat')
     WRITE (6,"(3F16.8)") (T(I),X(I),V(I),I=1,N,IN)
    CLOSE (6)
END PROGRAM ONE_D_MOTION
