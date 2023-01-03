**==sample.spg  processed by SPAG 4.52O  at 18:54 on 27 Mar 1996
 
      SUBROUTINE SAMPLE(I, En, Vir)
c
c      write quantities (pressure and energy) to file
c
c
c  Ener (input) : total energy
c  Vir  (input) : total virial
c
c
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
         press = (NPART/vol)/BETA + Vir/(3*vol)
         rho = NPART/vol
         IF (TAILCO) press = press + CORP(RC, rho)
      ELSE
         enp = 0
         press = 0
      END IF
      WRITE (66, *) I, enp, press
      RETURN
      END
