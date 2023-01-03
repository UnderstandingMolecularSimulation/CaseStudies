**==widom.spg  processed by SPAG 4.52O  at 10:35 on 12 Jul 1996
 
      SUBROUTINE WIDOM(Switch, Nghost)
c
c     add test particles to determine the chemical potential
c     switch = 0 initialize
c     switch = 1 update
c     switch = 2 write data to file
c
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'system.inc'
 
      DOUBLE PRECISION xtest, ytest, ztest, entest, virdum, RANF, wtest, 
     &                 rho, CORU
      INTEGER i, jb, o, Nghost, Switch, itest
      SAVE itest, wtest
 
      IF (Switch.EQ.1) THEN
c        ---add test particle
         o = 0
         jb = 1
         DO i = 1, Nghost
            xtest = BOX*RANF()
            ytest = BOX*RANF()
            ztest = BOX*RANF()
c           ---determine energy of particle at this position
            CALL ENERI(xtest, ytest, ztest, o, jb, entest, virdum)
            IF (TAILCO) THEN
               rho = NPART/(BOX**3)
               entest = entest + 2*CORU(RC, rho)
            END IF
            itest = itest + 1
            wtest = wtest + EXP(-BETA*entest)
c           ---sample histogram
            CALL DISTRI(1, .TRUE., .FALSE., entest)
         END DO
      ELSE IF (Switch.EQ.0) THEN
c        ---initialize
         itest = 0
         wtest = 0
      ELSE IF (Switch.EQ.2) THEN
c        ---write results
         IF (itest.NE.0) THEN
            WRITE (6, 99001) itest, -LOG(wtest/itest)
         END IF
      ELSE
         STOP 'error:widom'
      END IF
      RETURN
99001 FORMAT (' Results Widom test particle method : '/, 
     &        ' Number of samples         :', i12, 
     &        ' Excess chemical potential :', f12.3)
      END
