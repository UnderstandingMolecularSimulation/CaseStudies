**==eneri.spg  processed by SPAG 4.52O  at 18:10 on 19 Jul 1996
      SUBROUTINE ENERI(Xi, Yi, Zi, I, Jb, En, Vir, Ib)
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
 
      DOUBLE PRECISION Xi, Yi, Zi, En, dx, dy, dz, r2, Vir, virij, enij
      INTEGER I, j, Jb, Ib
c
      En = 0.D0
      Vir = 0.D0
      DO j = Jb, NPART
         IF (ID(j).EQ.Ib) THEN
            IF (j.NE.I) THEN
               dx = Xi - X(j)
               dy = Yi - Y(j)
               dz = Zi - Z(j)
               IF (dx.GT.HBOX(Ib)) THEN
                  dx = dx - BOX(Ib)
               ELSE
                  IF (dx.LT.-HBOX(Ib)) dx = dx + BOX(Ib)
               END IF
               IF (dy.GT.HBOX(Ib)) THEN
                  dy = dy - BOX(Ib)
               ELSE
                  IF (dy.LT.-HBOX(Ib)) dy = dy + BOX(Ib)
               END IF
               IF (dz.GT.HBOX(Ib)) THEN
                  dz = dz - BOX(Ib)
               ELSE
                  IF (dz.LT.-HBOX(Ib)) dz = dz + BOX(Ib)
               END IF
               r2 = dx*dx + dy*dy + dz*dz
               CALL ENER(enij, virij, r2, Ib)
               En = En + enij
               Vir = Vir + virij
            END IF
         END IF
      END DO
      Vir = Vir
      RETURN
      END
