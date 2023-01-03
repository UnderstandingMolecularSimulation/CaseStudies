**==andersen.spg  processed by SPAG 4.52O  at 14:19 on 24 Jun 1996
      SUBROUTINE ANDERSEN(X, Ux, Delt, T, En0)
c
c    integrate equations of motion:
c    Anderson stochastic coupling plus
c    velocity Verlet
c
      IMPLICIT NONE
      DOUBLE PRECISION X, en, fx, Ux, Delt, T, ent, enk
      DOUBLE PRECISION En0, sigma, GASDEV
      INCLUDE 'nvt.inc'
      INCLUDE 'andersen.inc'
      INTEGER it
      SAVE fx, it
      DATA fx, it/0.D0, 0/
 
c     ---velocity Verlet algorithm
c     --first step
      X = X + Delt*Ux + Delt*Delt*fx/2
      Ux = Ux + Delt*fx/2
 
      CALL FORCE(fx, X, en)
 
c     ---second step
      Ux = Ux + Delt*fx/2
      enk = Ux**2/2
      T = T + Delt
      ent = en + enk
      IF (MOD(it,10000).EQ.0) WRITE (21, 
     &                               '(2(2x,f7.4),2x,e8.2,2(2x,f7.4))')
     &                               X, Ux, ABS((ent-En0)/En0), ent, enk
c     ---Andersen method: perform collision with bath
      it = it + 1
      IF (MOD(it,NCOLL).EQ.0) THEN
         sigma = 1/TEMP
         Ux = GASDEV(sigma, ISEED)
      END IF
      RETURN
      END
