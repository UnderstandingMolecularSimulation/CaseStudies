**==velocs.spg  processed by SPAG 4.52O  at 15:46 on 28 Mar 1996
 
      SUBROUTINE VELOCS(Temp)
c
c
c  Simple velocity scaling (only used during equilbration!!!!)
c
c   Temp (input) : target velocity
c
 
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'veloc.inc'
 
      DOUBLE PRECISION Temp, v2, f
      INTEGER i
 
 
c     --rescale velocities
      v2 = 0
      DO i = 1, NPART
         v2 = v2 + VX(i)*VX(i) + VY(i)*VY(i) + VZ(i)*VZ(i)
      END DO
      v2 = v2/(3*NPART)
      f = SQRT(Temp/v2)
      DO i = 1, NPART
         VX(i) = VX(i)*f
         VY(i) = VY(i)*f
         VZ(i) = VZ(i)*f
      END DO
      RETURN
      END
