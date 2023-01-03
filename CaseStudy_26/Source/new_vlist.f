**==new_vlist.spg  processed by SPAG 4.52O  at 10:32 on 18 Oct 1996
      SUBROUTINE NEW_VLIST
c     makes the Verlet list
 
      IMPLICIT NONE
 
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'verlet.inc'
      INCLUDE 'nlist.inc'
      INCLUDE 'method.inc'
 
      DOUBLE PRECISION DIST2
      INTEGER i, j, ic, CELL, ncell, inn, in
      DIMENSION ncell(NEIgh)
 
      DO i = 1, NPART
         NLIST(i) = 0
         XV(i) = X(i)
         YV(i) = Y(i)
         ZV(i) = Z(i)
      END DO
 
      IF (NEIGHLIST) THEN
         CALL NEW_NLIST
         DO i = 1, NPART - 1
c           ---determine cell number
            ic = CELL(X(i), Y(i), Z(i))
c           ---determine neighbour cells
            CALL NEIGCELL(ic, ncell)
c           ---loop over neighbours and same cell
            DO inn = 1, NEIgh
               in = ncell(inn)
               j = HOC(in)
               DO WHILE (j.NE.0)
                  IF (j.NE.i.AND.j.GT.i) THEN
                     IF (DIST2(X(i),Y(i),Z(i),j).LT.RV2) THEN
                        NLIST(i) = NLIST(i) + 1
                        NLIST(j) = NLIST(j) + 1
                        LIST(i, NLIST(i)) = j
                        LIST(j, NLIST(j)) = i
                     END IF
                  END IF
                  j = LL(j)
               END DO
            END DO
         END DO
      ELSE
         DO i = 1, NPART - 1
            DO j = i + 1, NPART
               IF (DIST2(X(i),Y(i),Z(i),j).LT.RV2) THEN
                  NLIST(i) = NLIST(i) + 1
                  NLIST(j) = NLIST(j) + 1
                  LIST(i, NLIST(i)) = j
                  LIST(j, NLIST(j)) = i
               END IF
            END DO
         END DO
      END IF
      RETURN
      END
