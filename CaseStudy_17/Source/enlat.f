**==enlat.spg  processed by SPAG 4.52O  at 16:41 on 22 Jul 1996
 
 
      SUBROUTINE ENLAT(New, O, Xn, Yn, Zn, Enl, Lambda, Cmc)
c     calculates energy with Einstein lattice
 
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'einst.inc'
      LOGICAL New, Cmc
      INTEGER i, O
      DOUBLE PRECISION Xn, Yn, Zn, Enl, Lambda, dxcmt, dycmt, dzcmt
 
      IF (Cmc) THEN
c        ---simulate with fixed centre of mass
         IF (New) THEN
c           ---update center of mass for new position
            dxcmt = DXCM + (Xn-X(O))/NPART
            dycmt = DYCM + (Yn-Y(O))/NPART
            dzcmt = DZCM + (Zn-Z(O))/NPART
         ELSE
c           ---old position: old centre of mass
            dxcmt = DXCM
            dycmt = DYCM
            dzcmt = DZCM
         END IF
      ELSE
c        ---free centre of mass shift is zero:
         dxcmt = 0.D0
         dycmt = 0.D0
         dzcmt = 0.D0
      END IF
c     ---calculate energy
      Enl = 0.D0
      DO i = 1, NPART
         IF (New.AND.i.EQ.O) THEN
c           ---use new position for particle i
            Enl = Enl + Lambda*((Xn-X0(i)-dxcmt)**2+(Yn-Y0(i)-dycmt)
     &            **2+(Zn-Z0(i)-dzcmt)**2)
         ELSE
c           ---use old position for particle i
            Enl = Enl + Lambda*((X(i)-X0(i)-dxcmt)**2+(Y(i)-Y0(i)-dycmt)
     &            **2+(Z(i)-Z0(i)-dzcmt)**2)
         END IF
      END DO
      RETURN
      END
