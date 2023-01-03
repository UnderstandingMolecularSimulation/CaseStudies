**==bondl_g.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE BONDL_G(L)
c
c     ---generate a randon number with distribution:
c        l*l*exp(-((l-l0)**2)/(2*sigma**2))
c        sigma=sqrt(1./(2*beta*kv))
c     ---method: Allen and tildesley: pg 349--350
 
      IMPLICIT NONE
c ---model and system parameters
      INCLUDE 'par.inc'
      DOUBLE PRECISION L, sigma, a, RANF
      LOGICAL ready
 
c ---exp[-beta*0.5*kv(l-l0)^2]=exp[-(l-l0)^2/(2 sigma^2)]
      sigma = SQRT(1/(BETA*KV))
c     ---assume that probability of generating an l more than
c     ----3 times the standard deviations is very small
      a = (L0+3*sigma)**2
      ready = .FALSE.
      DO WHILE (.NOT.ready)
         CALL GAUSS(sigma, L0, L)
         IF (RANF().LE.(L*L/a)) ready = .TRUE.
      END DO
      RETURN
      END
