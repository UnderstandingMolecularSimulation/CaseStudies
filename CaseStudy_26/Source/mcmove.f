**==mcmove.spg  processed by SPAG 4.52O  at 10:32 on 18 Oct 1996
 
 
 
 
      SUBROUTINE MCMOVE(En, Vir, Attempt, Nacc, Dr)
c     ---attempts to displace a randomly selected particle
c        using Linked-lists
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INCLUDE 'method.inc'
      INCLUDE 'verlet.inc'
 
      DOUBLE PRECISION enn, eno, En, RANF, xn, yn, zn, viro, virn, Vir, 
     &                 xo, yo, zo, del, Dr
      INTEGER o, Attempt, Nacc, jb, CELL
 
      Attempt = Attempt + 1
      jb = 1
c     ---select a particle at random
      o = INT(NPART*RANF()) + 1
c     ---calculate energy old configuration
      IF (VERLETLIST) THEN
c        ---test for making new list
         del = (X(o)-XV(o))**2 + (Y(o)-YV(o))**2 + (Z(o)-ZV(o))**2
         IF (del.GT.SKIN2) CALL NEW_VLIST
      END IF
      CALL ENERI(X(o), Y(o), Z(o), o, jb, eno, viro)
c     ---give particle a random displacement
      xn = X(o) + (RANF()-0.5D0)*Dr
      yn = Y(o) + (RANF()-0.5D0)*Dr
      zn = Z(o) + (RANF()-0.5D0)*Dr
      IF (VERLETLIST) THEN
c        ---test for making new list
         del = (xn-XV(o))**2 + (yn-YV(o))**2 + (zn-ZV(o))**2
         IF (del.GT.SKIN2) CALL NEW_VLIST
c        ---test whether displacement is not too large
         del = (xn-XV(o))**2 + (yn-YV(o))**2 + (zn-ZV(o))**2
         IF (del.GT.SKIN2) THEN
            WRITE (6, *) 'ERROR:Displacement too large for Verlet '
            STOP
         END IF
      END IF
c     ---put particle in simulation box
      IF (xn.LT.0) xn = xn + BOX
      IF (xn.GT.BOX) xn = xn - BOX
      IF (yn.LT.0) yn = yn + BOX
      IF (yn.GT.BOX) yn = yn - BOX
      IF (zn.LT.0) zn = zn + BOX
      IF (zn.GT.BOX) zn = zn - BOX
c     ---calculate energy new configuration:
      CALL ENERI(xn, yn, zn, o, jb, enn, virn)
c     ---acceptance test
      IF (RANF().LT.EXP(-BETA*(enn-eno))) THEN
c        --accepted
         Nacc = Nacc + 1
         En = En + (enn-eno)
         Vir = Vir + (virn-viro)
         xo = X(o)
         yo = Y(o)
         zo = Z(o)
         X(o) = xn
         Y(o) = yn
         Z(o) = zn
         IF (NEIGHLIST.AND..NOT.VERLETLIST) THEN
c           ---- test whether new position is in a different cell:
            IF (CELL(xo,yo,zo).NE.CELL(xn,yn,zn)) CALL NEW_NLIST
         END IF
      END IF
      RETURN
      END
