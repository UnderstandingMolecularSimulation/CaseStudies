**==enerr.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE ENERR(Xi, Yi, Zi, Xn, Yn, Zn, I, J, Eni, Viri)
c
c     Determine energy with other molecules and part
c     of the chain that has been grown:
c
c      xi,yi,zi : trial position
c      xn,yn,zn : part of the chain that has been grow
c      i        : molecule that is considered
c      j        : atom that is added
c      eni      : energy trial position
c      viri     : virial trial position
c
      IMPLICIT NONE
      INCLUDE 'particle.inc'
      INCLUDE 'par.inc'
      INTEGER jj, ib, jb, I, J
      DOUBLE PRECISION Xi, Yi, Zi, Eni, Viri, dx, dy, dz, enij, virij, 
     &                 r2
      DOUBLE PRECISION Xn(*), Yn(*), Zn(*)
 
c     ---determine interactions with the other molecules
      ib = 1
c     ---exclude interactions within a molecule
      jb = ELL + 1
      CALL ENERI(Xi, Yi, Zi, I, J, ib, jb, Eni, Viri)
c     ---determine interactions with part of the molecule that
c      --has already been grown
      DO jj = 1, J - 2
         dx = Xi - Xn(jj)
         dy = Yi - Yn(jj)
         dz = Zi - Zn(jj)
         dx = dx - BOX*ANINT(dx*BOXI)
         dy = dy - BOX*ANINT(dy*BOXI)
         dz = dz - BOX*ANINT(dz*BOXI)
         r2 = dx*dx + dy*dy + dz*dz
         CALL ENERLJ(enij, virij, r2)
         Eni = Eni + enij
         Viri = Viri + virij
      END DO
      RETURN
      END
