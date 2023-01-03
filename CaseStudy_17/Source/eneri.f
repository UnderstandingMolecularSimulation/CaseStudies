**==eneri.spg  processed by SPAG 4.52O  at 16:41 on 22 Jul 1996
      SUBROUTINE ENERI(Overlap, Xi, Yi, Zi, I, Jb, En, Vir)
c     calculates the energy of particle i, using the Neighbour-list
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'nlist.inc'
      INCLUDE 'method.inc'
 
      DOUBLE PRECISION Xi, Yi, Zi, En, Vir, DIST2, r2
      INTEGER I, j, Jb, ncell, CELL, ic, inn, in
      LOGICAL Overlap
      DIMENSION ncell(NEIgh)
c
      En = 0.D0
      Vir = 0.D0
      Overlap = .FALSE.
      IF (NEIGHLIST) THEN
c        ---determine cell number
         ic = CELL(Xi, Yi, Zi)
c        ---determine neighbour cells
         CALL NEIGCELL(ic, ncell)
c        ---loop over neighbours and same cell
         DO inn = 1, NEIgh
            in = ncell(inn)
            j = HOC(in)
            DO WHILE (j.NE.0.AND..NOT.Overlap)
c              ---test wether j >= jb and j =/ i
               IF (j.NE.I.AND.j.GE.Jb) THEN
                  r2 = DIST2(Xi, Yi, Zi, j)
                  IF (r2.LT.1.0D0) Overlap = .TRUE.
               END IF
c              ---next particle in the cell
               j = LL(j)
            END DO
         END DO
      ELSE
c        ---simple energy calculation without lists
         DO j = Jb, NPART
            IF (j.NE.I) THEN
               r2 = DIST2(Xi, Yi, Zi, j)
               IF (r2.LT.1.0D0) Overlap = .TRUE.
            END IF
         END DO
      END IF
      RETURN
      END
