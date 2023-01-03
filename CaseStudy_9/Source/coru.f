**==coru.spg  processed by SPAG 4.52O  at 18:49 on  6 Jun 1996
      FUNCTION CORU(R, Rho)
      IMPLICIT NONE
      INCLUDE 'potential.inc'
      DOUBLE PRECISION sig3, ri3, R, CORU, Rho
 
      sig3 = SIG2*SQRT(SIG2)
      ri3 = sig3/(R*R*R)
      CORU = 2.D0*PI*EPS4*(Rho*sig3)*(ri3*ri3*ri3/9.D0-ri3/3.D0)
      RETURN
      END
