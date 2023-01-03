**==neigcell.spg  processed by SPAG 4.52O  at 10:32 on 18 Oct 1996
      SUBROUTINE NEIGCELL(Ic, Ncell)
c     determines the neigh neighbours
 
      IMPLICIT NONE
      INTEGER ix, iy, iz, Ncell, Ic, in, itel, icx, icy, icz, iccx, 
     &        iccy, iccz
      DIMENSION Ncell(*)
      INCLUDE 'parameter.inc'
      INCLUDE 'nlist.inc'
 
      itel = 0
      icz = Ic/(NCEL*NCEL)
      icy = (Ic-icz*NCEL*NCEL)/NCEL
      icx = (Ic-icy*NCEL-icz*NCEL*NCEL)
      DO iz = -1, 1
         iccz = icz + iz
         IF (iccz.LT.0) THEN
            iccz = iccz + NCEL
         ELSE IF (iccz.GE.NCEL) THEN
            iccz = iccz - NCEL
         END IF
         DO iy = -1, 1
            iccy = icy + iy
            IF (iccy.LT.0) THEN
               iccy = iccy + NCEL
            ELSE IF (iccy.GE.NCEL) THEN
               iccy = iccy - NCEL
            END IF
            DO ix = -1, 1
               iccx = icx + ix
               IF (iccx.LT.0) THEN
                  iccx = iccx + NCEL
               ELSE IF (iccx.GE.NCEL) THEN
                  iccx = iccx - NCEL
               END IF
               in = (iccx+iccy*NCEL+iccz*NCEL*NCEL)
               itel = itel + 1
               Ncell(itel) = in
            END DO
         END DO
      END DO
      RETURN
      END
