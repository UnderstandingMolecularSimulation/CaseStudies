**==sample.spg  processed by SPAG 4.52O  at 18:10 on 19 Jul 1996
 
      SUBROUTINE SAMPLE(I, En, Vir)
c     writes quantities to file
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INCLUDE 'potential.inc'
      INTEGER I, ib
      DOUBLE PRECISION En(*), enp(2), Vir(*), press(2), CORP, vol, 
     &                 rho(2)
 
      DO ib = 1, 2
         vol = BOX(ib)**3
         rho(ib) = NPBOX(ib)/vol
         press(ib) = rho(ib)/BETA + Vir(ib)/(3.D0*vol)
         IF (TAILCO) press(ib) = press(ib) + CORP(RC(ib), rho(ib))
         IF (NPBOX(ib).NE.0) THEN
            enp(ib) = En(ib)/DBLE(NPBOX(ib))
         ELSE
            enp(ib) = 0.D0
         END IF
      END DO
      WRITE (66, *) I, SNGL(enp(1)), SNGL(enp(2)), SNGL(press(1)), 
     &              SNGL(press(2)), SNGL(rho(1)), SNGL(rho(2))
      WRITE (44, '(2(i6,f10.2))') NPBOX(1), BOX(1)**3, NPBOX(2), BOX(2)
     &                            **3
      RETURN
      END
