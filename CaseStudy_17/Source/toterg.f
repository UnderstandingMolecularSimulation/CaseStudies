**==toterg.spg  processed by SPAG 4.52O  at 16:41 on 22 Jul 1996
      SUBROUTINE TOTERG(Ener, Vir, Enla, Lambda, Cmc)
c     ---calculates total energy
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
 
      DOUBLE PRECISION xi, yi, zi, Ener, eni, viri, Vir, Enla, Lambda, 
     &                 dum
      LOGICAL overlap, Cmc
      INTEGER i, jb, idum
 
      Ener = 0
      Vir = 0
      DO i = 1, NPART - 1
         xi = X(i)
         yi = Y(i)
         zi = Z(i)
         jb = i + 1
         CALL ENERI(overlap, xi, yi, zi, i, jb, eni, viri)
         IF (overlap) THEN
            WRITE (6, *) ' overlap toterg particle ', i
            STOP
         END IF
         Ener = Ener + eni
         Vir = Vir + viri
      END DO
c     ---calculate energy with Einstein lattice:
      idum = 0
      dum = 0.D0
      IF (Lambda.GT.0) CALL ENLAT(.FALSE., idum, dum, dum, dum, Enla, 
     &                            Lambda, Cmc)
      RETURN
      END
 
 
 
 
 
 
