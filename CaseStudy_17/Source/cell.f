**==cell.spg  processed by SPAG 4.52O  at 16:41 on 22 Jul 1996
 
      FUNCTION CELL(Xi, Yi, Zi)
c     determines cell number for position xi,yi,zi
c     particles are not put back in box so
c     periodic boundary conditions have to be
c     taken into account here
      IMPLICIT NONE
      INTEGER ix, iy, iz, CELL
      DOUBLE PRECISION Xi, Yi, Zi, xd, yd, zd
      INCLUDE 'parameter.inc'
      INCLUDE 'nlist.inc'
      INCLUDE 'system.inc'
 
      xd = Xi
      yd = Yi
      zd = Zi
c     ---reduce to simulation box
      IF (xd.LT.0) THEN
         xd = xd + BOXX*(INT(-xd/BOXX)+1)
      ELSE IF (xd.GT.BOXX) THEN
         xd = xd - BOXX*INT(xd/BOXX)
      END IF
      IF (yd.LT.0) THEN
         yd = yd + BOXY*(INT(-yd/BOXY)+1)
      ELSE IF (yd.GT.BOXY) THEN
         yd = yd - BOXY*INT(yd/BOXY)
      END IF
      IF (zd.LT.0) THEN
         zd = zd + BOXZ*(INT(-zd/BOXZ)+1)
      ELSE IF (zd.GT.BOXZ) THEN
         zd = zd - BOXZ*INT(zd/BOXZ)
      END IF
 
      ix = INT(xd*FACTX)
      iy = INT(yd*FACTY)
      iz = INT(zd*FACTZ)
      CELL = ix + iy*NCELX + iz*NCELY*NCELY
      RETURN
      END
