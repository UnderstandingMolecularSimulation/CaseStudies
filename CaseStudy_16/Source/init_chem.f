**==init_chem.spg  processed by SPAG 4.52O  at 18:10 on 19 Jul 1996
      SUBROUTINE INIT_CHEM(Switch)
c     ---initialize and calculate chemical potentials
c
      IMPLICIT NONE
      INTEGER Switch, ib
      INCLUDE 'chem.inc'
      INCLUDE 'system.inc'
      IF (Switch.EQ.0) THEN
c        ---initialize
         DO ib = 1, 2
            CHP(ib) = 0
            ICHP(ib) = 0
         END DO
      ELSE IF (Switch.EQ.2) THEN
c        ---print final results
         DO ib = 1, 2
            IF (ICHP(ib).NE.0) THEN
               CHP(ib) = -LOG(CHP(ib)/ICHP(ib))/BETA
            END IF
         END DO
         WRITE (6, 99001) (ICHP(1)+ICHP(2))/2, CHP(1), CHP(2)
      ELSE
         STOP 'error: init_chem'
      END IF
99001 FORMAT (' chemical potentials : ', /, ' number of samples : ', 
     &        i12, /, ' box 1 ', f7.3, /, ' box 2 ', f7.3, /)
      END
