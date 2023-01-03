**==corp.spg  processed by SPAG 4.52O  at 18:54 on 27 Mar 1996
 
      FUNCTION CORP(R, Rho)
c
c     tail correction for the pressure:
c
c  CORP (output) tail correction pressure
c  R    (input)  cutoff radius
c  Rho  (input)  density
c
 
      IMPLICIT NONE
      INCLUDE 'potential.inc'
      DOUBLE PRECISION sig3, ri3, R, CORP, Rho
 
      sig3 = SIG2*SQRT(SIG2)
      ri3 = sig3/(R*R*R)
      CORP = 4*PI*EPS4*(Rho**2)*sig3*(2*ri3*ri3*ri3/9-ri3/3)
      RETURN
      END
