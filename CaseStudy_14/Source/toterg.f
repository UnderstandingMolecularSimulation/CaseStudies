**==toterg.spg  processed by SPAG 4.52O  at 10:35 on 12 Jul 1996
      SUBROUTINE TOTERG(Ener, Vir)
c     ---calculates total energy
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'system.inc'
 
      DOUBLE PRECISION xi, yi, zi, Ener, eni, CORU, viri, Vir, rho
      INTEGER i, jb
 
      Ener = 0
      Vir = 0
      DO i = 1, NPART - 1
         xi = X(i)
         yi = Y(i)
         zi = Z(i)
         jb = i + 1
         CALL ENERI(xi, yi, zi, i, jb, eni, viri)
         Ener = Ener + eni
         Vir = Vir + viri
      END DO
c     ---add tail corrections
      IF (TAILCO) THEN
         rho = NPART/(BOX**3)
         Ener = Ener + NPART*CORU(RC, rho)
      END IF
      RETURN
      END
