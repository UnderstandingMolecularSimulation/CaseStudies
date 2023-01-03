**==sample.spg  processed by SPAG 4.52O  at 16:41 on 22 Jul 1996
      SUBROUTINE SAMPLE(I, En, Vir, Enla, Lambda)
c     ---write quantities to file
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INTEGER I
      DOUBLE PRECISION En, enp, Vir, press, Enla, Lambda
 
      IF (NPART.NE.0) THEN
         enp = En/DBLE(NPART)
         press = (NPART/VOL)/BETA + Vir/(3.D0*VOL)
      ELSE
         enp = 0.D0
         press = 0.D0
      END IF
      WRITE (66, *) I, (Enla/Lambda)/NPART
      RETURN
      END
