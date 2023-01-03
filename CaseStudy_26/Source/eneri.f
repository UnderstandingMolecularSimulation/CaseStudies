**==eneri.spg  processed by SPAG 4.52O  at 10:32 on 18 Oct 1996
      SUBROUTINE ENERI(Xi, Yi, Zi, I, Jb, En, Vir)
c     calculates the energy of particle i, using the Neighbour-list
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'nlist.inc'
      INCLUDE 'verlet.inc'
      INCLUDE 'method.inc'
 
      DOUBLE PRECISION Xi, Yi, Zi, En, Vir, virij, enij, DIST2, r2
      INTEGER I, j, Jb, ncell, CELL, ic, inn, in, jj
      DIMENSION ncell(NEIgh)
c
      En = 0
      Vir = 0
      IF (NEIGHLIST.AND..NOT.VERLETLIST) THEN
c        ---determine cell number
         ic = CELL(Xi, Yi, Zi)
c        ---determine neighbour cells
         CALL NEIGCELL(ic, ncell)
c        ---loop over neighbours and same cell
         DO inn = 1, NEIgh
            in = ncell(inn)
            j = HOC(in)
            DO WHILE (j.NE.0)
c              ---test wether j >= jb and j =/ i
               IF (j.NE.I.AND.j.GE.Jb) THEN
                  r2 = DIST2(Xi, Yi, Zi, j)
                  CALL ENER(enij, virij, r2)
                  En = En + enij
                  Vir = Vir + virij
               END IF
c              ---next particle in the cell
               j = LL(j)
            END DO
         END DO
      ELSE IF (VERLETLIST) THEN
c        ---use Verlet-list
         DO jj = 1, NLIST(I)
            j = LIST(I, jj)
c           ---test wether j >= jb and j =/ i
            IF (j.NE.I.AND.j.GE.Jb) THEN
               r2 = DIST2(Xi, Yi, Zi, j)
               CALL ENER(enij, virij, r2)
               En = En + enij
               Vir = Vir + virij
            END IF
         END DO
      ELSE
c        ---simple energy calculation without lists
         DO j = Jb, NPART
            IF (j.NE.I) THEN
               r2 = DIST2(Xi, Yi, Zi, j)
               CALL ENER(enij, virij, r2)
               En = En + enij
               Vir = Vir + virij
            END IF
         END DO
      END IF
      RETURN
      END
