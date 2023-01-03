**==corp.spg  processed by SPAG 4.52O  at 09:34 on 20 Jun 1996
      FUNCTION CORP(R, Rho)
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      DOUBLE PRECISION ri3, R, CORP, Rho
 
      ri3 = 1/(R*R*R)
      CORP = 4*PI*4*(Rho**2)*(2*ri3*ri3*ri3/9-ri3/3)
      RETURN
      END
