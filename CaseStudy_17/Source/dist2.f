**==dist2.spg  processed by SPAG 4.52O  at 16:41 on 22 Jul 1996
      FUNCTION DIST2(Xi, Yi, Zi, J)
c     calculates the distance between xi and particle j
c     particles are not put back to box,
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      DOUBLE PRECISION Xi, Yi, Zi, xr, yr, zr, DIST2
      INTEGER J
 
      xr = Xi - X(J)
      xr = xr - BOXX*ANINT(xr/BOXX)
      yr = Yi - Y(J)
      yr = yr - BOXY*ANINT(yr/BOXY)
      zr = Zi - Z(J)
      zr = zr - BOXZ*ANINT(zr/BOXZ)
      DIST2 = xr*xr + yr*yr + zr*zr
      RETURN
      END
