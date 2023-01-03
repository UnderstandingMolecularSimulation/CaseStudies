**==neigcell.spg  processed by SPAG 4.52O  at 16:41 on 22 Jul 1996
 
 
 
 
 
 
      SUBROUTINE NEIGCELL(Ic, Ncell)
c     determines the neigh neighbours
 
      IMPLICIT NONE
      INTEGER ix, iy, iz, Ncell, Ic, in, itel, icx, icy, icz, iccx, 
     &        iccy, iccz
      DIMENSION Ncell(*)
      INCLUDE 'parameter.inc'
      INCLUDE 'nlist.inc'
 
      itel = 0
      icz = Ic/(NCELY*NCELY)
      icy = (Ic-icz*NCELY*NCELY)/NCELX
      icx = (Ic-icy*NCELX-icz*NCELY*NCELY)
      DO iz = -1, 1
         iccz = icz + iz
         IF (iccz.LT.0) THEN
            iccz = iccz + NCELZ
         ELSE IF (iccz.GE.NCELZ) THEN
            iccz = iccz - NCELZ
         END IF
         DO iy = -1, 1
            iccy = icy + iy
            IF (iccy.LT.0) THEN
               iccy = iccy + NCELY
            ELSE IF (iccy.GE.NCELY) THEN
               iccy = iccy - NCELY
            END IF
            DO ix = -1, 1
               iccx = icx + ix
               IF (iccx.LT.0) THEN
                  iccx = iccx + NCELX
               ELSE IF (iccx.GE.NCELX) THEN
                  iccx = iccx - NCELX
               END IF
               in = (iccx+iccy*NCELX+iccz*NCELY*NCELY)
               itel = itel + 1
               Ncell(itel) = in
            END DO
         END DO
      END DO
      RETURN
      END
