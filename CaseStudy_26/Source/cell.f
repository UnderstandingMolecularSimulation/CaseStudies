**==cell.spg  processed by SPAG 4.52O  at 10:32 on 18 Oct 1996
 
      FUNCTION CELL(X, Y, Z)
c     determines cell number for position x,y,z
      IMPLICIT NONE
      INTEGER ix, iy, iz, CELL
      DOUBLE PRECISION X, Y, Z
      INCLUDE 'parameter.inc'
      INCLUDE 'nlist.inc'
 
      ix = INT(X*FACTN)
      iy = INT(Y*FACTN)
      iz = INT(Z*FACTN)
      CELL = ix + iy*NCEL + iz*NCEL*NCEL
      RETURN
      END
