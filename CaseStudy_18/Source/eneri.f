**==eneri.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE ENERI(Xi, Yi, Zi, I, J, Ib, Jb, Eni, Viri)
      IMPLICIT NONE
      INCLUDE 'particle.inc'
      INCLUDE 'par.inc'
      INTEGER ii, jj, Ib, Jb, I, J
      DOUBLE PRECISION Xi, Yi, Zi, Eni, Viri, dx, dy, dz, enij, virij, 
     &                 r2
 
      Eni = 0
      Viri = 0
      DO ii = Ib, NPART
         DO jj = 1, ELL
c           ---exclude:
c              --interactions of the same molecules (ii=i) with atoms jj=>jb
c              --exclude bonded interactions (ii=i) and abs(jj-i)=1 or 0
            IF (ii.NE.I.OR.((jj.GE.Jb).AND.(ABS(jj-J).GT.1))) THEN
               dx = Xi - X(ii, jj)
               dy = Yi - Y(ii, jj)
               dz = Zi - Z(ii, jj)
               dx = dx - BOX*ANINT(dx*BOXI)
               dy = dy - BOX*ANINT(dy*BOXI)
               dz = dz - BOX*ANINT(dz*BOXI)
               r2 = dx*dx + dy*dy + dz*dz
               CALL ENERLJ(enij, virij, r2)
               Eni = Eni + enij
               Viri = Viri + virij
            END IF
         END DO
      END DO
      RETURN
      END
