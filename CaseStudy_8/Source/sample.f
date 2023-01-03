**==sample.spg  processed by SPAG 4.52O  at 10:45 on  5 Jun 1996
 
      SUBROUTINE SAMPLE(I, En, Vir)
c     writes quantities to file
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INCLUDE 'potential.inc'
      INTEGER I
      DOUBLE PRECISION En, enp, Vir, press, CORP, vol, rho
 
      IF (NPART.NE.0) THEN
         enp = En/DBLE(NPART)
         vol = BOX**3
         rho = NPART/vol
         press = rho/BETA + Vir/(3.D0*vol)
         IF (TAILCO) press = press + CORP(RC, rho)
      ELSE
         rho = 0.D0
         enp = 0.D0
         press = 0.D0
      END IF
      WRITE (66, *) I, SNGL(enp), SNGL(press), SNGL(rho)
      RETURN
      END
