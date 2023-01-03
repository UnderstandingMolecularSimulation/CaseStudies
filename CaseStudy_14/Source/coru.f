**==coru.spg  processed by SPAG 4.52O  at 10:35 on 12 Jul 1996
      FUNCTION CORU(R, Rho)
      IMPLICIT NONE
      INCLUDE 'potential.inc'
      DOUBLE PRECISION sig3, ri3, R, CORU, Rho
 
      sig3 = SIG2*SQRT(SIG2)
      ri3 = sig3/(R*R*R)
      CORU = 2*PI*EPS4*(Rho*sig3)*(ri3*ri3*ri3/9-ri3/3)
      RETURN
      END
