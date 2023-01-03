**==grow.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE GROW(Newconf, W, K, Xn, Yn, Zn, O, Ib, Eni, Viri)
      IMPLICIT NONE
      INCLUDE 'particle.inc'
      INCLUDE 'par.inc'
      LOGICAL Newconf
 
      INTEGER i, O, K, j, n, Ib, ih
      DOUBLE PRECISION RANF, W, l, bx, by, bz, sumw, Eni, Viri
      DOUBLE PRECISION wt(KMAx), ent(KMAx), virt(KMAx)
      DOUBLE PRECISION Xn(*), Yn(*), Zn(*)
      DOUBLE PRECISION xt(KMAx), yt(KMAx), zt(KMAx)
 
      W = 1
      Eni = 0
      Viri = 0
      IF (.NOT.Newconf) THEN
c        ---select old configuration
         O = INT(RANF()*NPART) + 1
         Ib = INT(RANF()*ELL) + 1
c        ---select which part needs to be conserved
      END IF
      ih = 0
      DO i = 1, Ib - 1
         Xn(i) = X(O, i)
         Yn(i) = Y(O, i)
         Zn(i) = Z(O, i)
      END DO
c     ---start to grow molecule
      DO i = Ib, ELL
         IF (i.EQ.1) THEN
            IF (Newconf) THEN
c              ---generate first position at random
               Xn(1) = RANF()*BOX
               Yn(1) = RANF()*BOX
               Zn(1) = RANF()*BOX
            ELSE
               Xn(1) = X(O, 1)
               Yn(1) = Y(O, 1)
               Zn(1) = Z(O, 1)
            END IF
c           ---Calculate Rosenbluth factor first atom
            CALL ENERR(Xn(1), Yn(1), Zn(1), Xn, Yn, Zn, O, i, Eni, Viri)
            W = K*EXP(-BETA*Eni)
         ELSE
c           ---generate next atom
            sumw = 0
            DO j = 1, K
               IF (.NOT.Newconf.AND.j.EQ.1) THEN
c                 ---use actual position as trial orientation
                  xt(1) = X(O, i)
                  yt(1) = Y(O, i)
                  zt(1) = Z(O, i)
               ELSE
c                 ---generate vector on unit sphere
                  CALL RANOR(bx, by, bz)
c                 ---generate bond length
                  CALL BONDL_G(l)
c                 ---determine new position
                  xt(j) = Xn(i-1) + bx*l
                  yt(j) = Yn(i-1) + by*l
                  zt(j) = Zn(i-1) + bz*l
               END IF
c              ---determine energy new position
               CALL ENERR(xt(j), yt(j), zt(j), Xn, Yn, Zn, O, i, ent(j), 
     &                    virt(j))
c              ---determine Rosenbluth factor
               wt(j) = EXP(-BETA*ent(j))
               sumw = sumw + wt(j)
            END DO
            W = W*sumw
            IF (i.NE.1) THEN
               IF (Newconf) THEN
c                 ---select one of the k trial positions
                  CALL SELECT(wt, sumw, n)
c                 ---add new position to chain
                  Xn(i) = xt(n)
                  Yn(i) = yt(n)
                  Zn(i) = zt(n)
               ELSE
                  Xn(i) = X(O, i)
                  Yn(i) = Y(O, i)
                  Zn(i) = Z(O, i)
                  n = 1
               END IF
c              ---energy of selected position
               Eni = Eni + ent(n)
               Viri = Viri + virt(n)
            END IF
         END IF
      END DO
      RETURN
      END
