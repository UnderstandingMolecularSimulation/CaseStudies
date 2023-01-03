**==mcmove.spg  processed by SPAG 4.52O  at 10:35 on 12 Jul 1996
 
 
      SUBROUTINE MCMOVE(En, Vir, Attempt, Nacc, Dr, Ii)
c     attempts to displace a randomly selected particle
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      DOUBLE PRECISION enn, eno, En, RANF, xn, yn, zn, viro, virn, Vir, 
     &                 Dr
      INTEGER o, Attempt, Nacc, jb, Ii
 
      Attempt = Attempt + 1
      jb = 1
c     ---select a particle at random
      o = INT(NPART*RANF()) + 1
c     ---calculate energy old configuration
      CALL ENERI(X(o), Y(o), Z(o), o, jb, eno, viro)
c     ---use this energy to sample histogram
c      --not during equilibration (ii.eq.1)
      IF (Ii.EQ.2) CALL DISTRI(1, .FALSE., .TRUE., eno)
c     ---give particle a random displacement
      xn = X(o) + (RANF()-0.5D0)*Dr
      yn = Y(o) + (RANF()-0.5D0)*Dr
      zn = Z(o) + (RANF()-0.5D0)*Dr
c     ---calculate energy new configuration:
      CALL ENERI(xn, yn, zn, o, jb, enn, virn)
c     ---acceptance test
      IF (RANF().LT.EXP(-BETA*(enn-eno))) THEN
c        --accepted
         Nacc = Nacc + 1
         En = En + (enn-eno)
         Vir = Vir + (virn-viro)
c        ---put particle in simulation box
         IF (xn.LT.0) xn = xn + BOX
         IF (xn.GT.BOX) xn = xn - BOX
         IF (yn.LT.0) yn = yn + BOX
         IF (yn.GT.BOX) yn = yn - BOX
         IF (zn.LT.0) zn = zn + BOX
         IF (zn.GT.BOX) zn = zn - BOX
         X(o) = xn
         Y(o) = yn
         Z(o) = zn
      END IF
      RETURN
      END
