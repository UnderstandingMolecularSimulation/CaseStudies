**==mcmove.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE MCMOVE(Move, Acc, Etot, Evib, Enlj, Vir, Dx)
c ---displace a chain
      IMPLICIT NONE
      INCLUDE 'particle.inc'
      INCLUDE 'par.inc'
      INTEGER o, Move, i
      DOUBLE PRECISION RANF, Etot, ennew, enold, Evib, Enlj, eni, viri
      DOUBLE PRECISION xn(ELLmax), yn(ELLmax), zn(ELLmax), Dx, arg, dxy, 
     &                 dxx, dxz, Vir, virnew, virold
      INTEGER Acc
 
c     ---perform a cbmc move in which a chain
c     ----is regrown at a random position
      Move = Move + 1
c     ---select a chain at random
      o = INT(RANF()*NPART) + 1
c     ---determine energy of this chain
      enold = 0
      virold = 0
      DO i = 1, ELL
         xn(i) = X(o, i)
         yn(i) = Y(o, i)
         zn(i) = Z(o, i)
         CALL ENERR(xn(i), yn(i), zn(i), xn, yn, zn, o, i, eni, viri)
         enold = enold + eni
         virold = virold + viri
      END DO
c     ---new position
      dxx = (RANF()-0.5D0)*Dx
      dxy = (RANF()-0.5D0)*Dx
      dxz = (RANF()-0.5D0)*Dx
      ennew = 0
      virnew = 0
      DO i = 1, ELL
         xn(i) = X(o, i) + dxx
         yn(i) = Y(o, i) + dxy
         zn(i) = Z(o, i) + dxz
         CALL ENERR(xn(i), yn(i), zn(i), xn, yn, zn, o, i, eni, viri)
         ennew = ennew + eni
         virnew = virnew + viri
      END DO
c     ---acceptance test
      arg = EXP(-BETA*(ennew-enold))
      IF (RANF().LT.arg) THEN
c        ---accepted
         Acc = Acc + 1
         dxx = 0
         IF (xn(1).GT.BOX) dxx = -BOX
         IF (xn(1).LT.0) dxx = BOX
         dxy = 0
         IF (yn(1).GT.BOX) dxy = -BOX
         IF (yn(1).LT.0) dxy = BOX
         dxz = 0
         IF (zn(1).GT.BOX) dxz = -BOX
         IF (zn(1).LT.0) dxz = BOX
         DO i = 1, ELL
            X(o, i) = xn(i) + dxx
            Y(o, i) = yn(i) + dxy
            Z(o, i) = zn(i) + dxz
         END DO
         Enlj = Enlj + (ennew-enold)
         Etot = Evib + Enlj
         Vir = Vir + (virnew-virold)
      END IF
      RETURN
      END
