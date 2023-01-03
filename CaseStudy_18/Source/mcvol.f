**==mcvol.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE MCVOL(Attempt, Acc, En, Evib, Elj, Vir, Vmax)
c     attempts to change the volume
      IMPLICIT NONE
      INCLUDE 'particle.inc'
      INCLUDE 'par.inc'
      DOUBLE PRECISION enn, En, RANF, Vir, virn, boxn, vo, vn, lnvn, 
     &                 Vmax, f, arg, eljn, evibn, Elj, Evib, dx, dy, dz
      INTEGER Attempt, Acc, i, ellh, j
 
      Attempt = Attempt + 1
c     ---calulate new volume by making random walk in ln V
      vo = BOX*BOX*BOX
      lnvn = LOG(vo) + (RANF()-0.5D0)*Vmax
      vn = EXP(lnvn)
      boxn = vn**(1.D0/3.D0)
c     ---determine new coordinates
      f = boxn/BOX
c     ---use particle close to the middle
      ellh = MAX(1, ELL/2)
      DO i = 1, NPART
         dx = X(i, ellh)*f - X(i, ellh)
         dy = Y(i, ellh)*f - Y(i, ellh)
         dz = Z(i, ellh)*f - Z(i, ellh)
         DO j = 1, ELL
            X(i, j) = X(i, j) + dx
            Y(i, j) = Y(i, j) + dy
            Z(i, j) = Z(i, j) + dz
         END DO
      END DO
      BOX = boxn
      HBOX = 0.5D0*BOX
      BOXI = 1/BOX
c     ---calculate new total energy
      CALL TOTERG(enn, evibn, eljn, virn)
c     ---acceptance:
      arg = EXP(-BETA*(P*(vn-vo)-(NPART+1)*LOG(vn/vo)/BETA+enn-En))
      IF (RANF().LT.arg) THEN
c        ---accepted
         Acc = Acc + 1
         En = enn
         Elj = eljn
         Evib = evibn
         Vir = virn
      ELSE
c        ---restore the old configuration
         f = 1/f
         DO i = 1, NPART
            dx = X(i, ellh)*f - X(i, ellh)
            dy = Y(i, ellh)*f - Y(i, ellh)
            dz = Z(i, ellh)*f - Z(i, ellh)
            DO j = 1, ELL
               X(i, j) = X(i, j) + dx
               Y(i, j) = Y(i, j) + dy
               Z(i, j) = Z(i, j) + dz
            END DO
         END DO
         BOX = boxn*f
         HBOX = 0.5D0*BOX
         BOXI = 1/BOX
      END IF
      RETURN
      END
