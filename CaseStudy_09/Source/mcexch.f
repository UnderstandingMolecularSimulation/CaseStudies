**==mcexch.spg  processed by SPAG 4.52O  at 18:49 on  6 Jun 1996
 
 
      SUBROUTINE MCEXCH(En, Vir, Attempt, Nacc, Iseed)
c
c     attempts to exchange a particle with the reservoir
c
c
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'grand.inc'
      DOUBLE PRECISION enn, eno, En, RANF, xn, yn, zn, viro, virn, Vir, 
     &                 CORU, arg, vol, rhoo, rhon
      INTEGER o, Attempt, Nacc, jb, Iseed
 
      Attempt = Attempt + 1
      vol = BOX*BOX*BOX
      rhoo = NPART/vol
c     ---select to add of delete a particle
      IF (RANF(Iseed).LE.0.5D0) THEN
c        ---add a particle at a random position
         xn = RANF(Iseed)*BOX
         yn = RANF(Iseed)*BOX
         zn = RANF(Iseed)*BOX
         o = NPART + 1
         jb = 1
c        ---determine energy of this particle
         CALL ENERI(xn, yn, zn, o, jb, enn, virn)
c        ---tail correction
         IF (TAILCO) THEN
            rhon = (NPART+1)/vol
            enn = enn + ((NPART+1)*CORU(RC,rhon)-NPART*CORU(RC,rhoo))
         END IF
c        ---acceptance test:
         arg = ZZ*vol*EXP(-BETA*enn)/(NPART+1)
         IF (RANF(Iseed).LT.arg) THEN
c           --accepted
            Nacc = Nacc + 1
            En = En + enn
            Vir = Vir + virn
            NPART = NPART + 1
            IF (NPART.GT.NPMax) THEN
               PRINT *, ' ERROR: array npmax too small'
               STOP
            END IF
            X(NPART) = xn
            Y(NPART) = yn
            Z(NPART) = zn
         END IF
      ELSE
         IF (NPART.EQ.0) THEN
            RETURN
         END IF
c        ---delete a randomly selected particle
         o = INT(RANF(Iseed)*NPART) + 1
         jb = 1
         CALL ENERI(X(o), Y(o), Z(o), o, jb, eno, viro)
c        ---particle is removed, so new energy
         enn = -eno
         virn = -viro
c        ---tail correction
         IF (TAILCO) THEN
            rhon = (NPART-1)/vol
            enn = enn + ((NPART-1)*CORU(RC,rhon)-NPART*CORU(RC,rhoo))
         END IF
c        ---acceptance test:
         arg = NPART*EXP(-BETA*enn)/(ZZ*vol)
         IF (RANF(Iseed).LT.arg) THEN
c           --accepted
            Nacc = Nacc + 1
            En = En + enn
            Vir = Vir + virn
            X(o) = X(NPART)
            Y(o) = Y(NPART)
            Z(o) = Z(NPART)
            NPART = NPART - 1
         END IF
      END IF
      RETURN
      END
