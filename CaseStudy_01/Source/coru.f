**==coru.spg  processed by SPAG 4.52O  at 18:54 on 27 Mar 1996
      FUNCTION CORU(R, Rho)
c
c     tail correction for the energy:
c
c  CORU (output) energy tail correction
c  R    (input)  cutoff radius
c  Rho  (input)  density
c
 
      IMPLICIT NONE
      INCLUDE 'potential.inc'
      DOUBLE PRECISION sig3, ri3, R, CORU, Rho
 
      sig3 = SIG2*SQRT(SIG2)
      ri3 = sig3/(R*R*R)
      CORU = 2*PI*EPS4*(Rho*sig3)*(ri3*ri3*ri3/9-ri3/3)
      RETURN
      END
