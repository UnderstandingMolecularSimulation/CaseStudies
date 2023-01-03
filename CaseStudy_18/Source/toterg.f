**==toterg.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE TOTERG(Etot, Evib, Enlj, Vir)
      IMPLICIT NONE
      INCLUDE 'particle.inc'
      INTEGER i, j, ib, jb
      DOUBLE PRECISION uvib, r, dx, dy, dz, Etot, xi, yi, zi, Enlj, 
     &                 Evib, eni, viri, virvib, Vir
 
      Evib = 0
      Enlj = 0
      Vir = 0
      DO i = 1, NPART
         ib = i
         DO j = 1, ELL
            xi = X(i, j)
            yi = Y(i, j)
            zi = Z(i, j)
c           ---determine intra molecular interactions
            jb = j
            CALL ENERI(xi, yi, zi, i, j, ib, jb, eni, viri)
            Enlj = Enlj + eni
            Vir = Vir + viri
            IF (j.GE.2) THEN
c              ---internal interactions:
               dx = xi - X(i, j-1)
               dy = yi - Y(i, j-1)
               dz = zi - Z(i, j-1)
               r = SQRT(dx*dx+dy*dy+dz*dz)
               CALL VIB(uvib, virvib, r)
               Evib = Evib + uvib
               Vir = Vir + virvib
            END IF
         END DO
      END DO
      Etot = Enlj + Evib
      RETURN
      END
 
