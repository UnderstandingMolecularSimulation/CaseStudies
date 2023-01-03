**==dist2.spg  processed by SPAG 4.52O  at 10:32 on 18 Oct 1996
      FUNCTION DIST2(Xi, Yi, Zi, J)
c     calculates the distance between xi and particle j
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      DOUBLE PRECISION Xi, Yi, Zi, xr, yr, zr, DIST2
      INTEGER J
 
 
      xr = Xi - X(J)
      IF (xr.GT.HBOX) THEN
         xr = xr - BOX
      ELSE IF (xr.LT.-HBOX) THEN
         xr = xr + BOX
      END IF
      yr = Yi - Y(J)
      IF (yr.GT.HBOX) THEN
         yr = yr - BOX
      ELSE IF (yr.LT.-HBOX) THEN
         yr = yr + BOX
      END IF
      zr = Zi - Z(J)
      IF (zr.GT.HBOX) THEN
         zr = zr - BOX
      ELSE IF (zr.LT.-HBOX) THEN
         zr = zr + BOX
      END IF
      DIST2 = xr*xr + yr*yr + zr*zr
      RETURN
      END
