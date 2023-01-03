**==integrate.spg  processed by SPAG 4.52O  at 14:19 on 24 Jun 1996
      SUBROUTINE INTEGRATE(X, Ux, Delt, T, En0)
c
c    integrate equations of motion:
c
      IMPLICIT NONE
      DOUBLE PRECISION X, en, fx, Ux, Delt, T, ent, enk
      DOUBLE PRECISION En0
      SAVE fx
      DATA fx/0.D0/
 
c     ---velocity Verlet algorithm
      X = X + Delt*Ux + Delt*Delt*fx/2
      Ux = Ux + Delt*fx/2
      CALL FORCE(fx, X, en)
      Ux = Ux + Delt*fx/2
      enk = Ux**2/2
 
      T = T + Delt
      ent = en + enk
      WRITE (21, '(2(2x,f7.4),2x,e8.2,2(2x,f7.4))') X, Ux, 
     &       ABS((ent-En0)/En0), ent, enk
      RETURN
      END
