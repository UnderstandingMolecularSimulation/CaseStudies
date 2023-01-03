**==mcmove.spg  processed by SPAG 4.52O  at 18:10 on 19 Jul 1996
      SUBROUTINE MCMOVE(En, Vir, Attempt, Nacc, Dr, Iseed)
c     attempts to displace a randomly selected particle
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      DOUBLE PRECISION enn, eno, En, RANF, xn, yn, zn, viro, virn, Dr, 
     &                 Vir
      DIMENSION En(*), Vir(*)
      INTEGER o, Attempt, Nacc, jb, Iseed, ido
 
      Attempt = Attempt + 1
      jb = 1
c     ---select a particle at random
      o = INT(NPART*RANF(Iseed)) + 1
      ido = ID(o)
c     ---calculate energy old configuration
      CALL ENERI(X(o), Y(o), Z(o), o, jb, eno, viro, ido)
c     ---give particle a random displacement
      xn = X(o) + (RANF(Iseed)-0.5D0)*Dr
      yn = Y(o) + (RANF(Iseed)-0.5D0)*Dr
      zn = Z(o) + (RANF(Iseed)-0.5D0)*Dr
c     ---calculate energy new configuration:
      CALL ENERI(xn, yn, zn, o, jb, enn, virn, ido)
c     ---acceptance test
      IF (RANF(Iseed).LT.EXP(-BETA*(enn-eno))) THEN
c        --accepted
         Nacc = Nacc + 1
         En(ido) = En(ido) + (enn-eno)
         Vir(ido) = Vir(ido) + (virn-viro)
c        ---put particle in simulation box
         IF (xn.LT.0) xn = xn + BOX(ido)
         IF (xn.GT.BOX(ido)) xn = xn - BOX(ido)
         IF (yn.LT.0) yn = yn + BOX(ido)
         IF (yn.GT.BOX(ido)) yn = yn - BOX(ido)
         IF (zn.LT.0) zn = zn + BOX(ido)
         IF (zn.GT.BOX(ido)) zn = zn - BOX(ido)
         X(o) = xn
         Y(o) = yn
         Z(o) = zn
      END IF
      RETURN
      END
