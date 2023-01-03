**==force.spg  processed by SPAG 4.52O  at 14:19 on 24 Jun 1996
      SUBROUTINE FORCE(Fx, X, En)
c
c     Deterime the force:
c     Potential U = 0.5 x^2
c
      IMPLICIT NONE
      DOUBLE PRECISION X, En, Fx
 
      En = 0.5D0*X*X
      Fx = -X
      RETURN
      END
