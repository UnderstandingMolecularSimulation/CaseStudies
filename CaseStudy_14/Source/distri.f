**==distri.spg  processed by SPAG 4.52O  at 10:35 on 12 Jul 1996
      SUBROUTINE DISTRI(Switch, Testpart, Addtail, Eni)
c
c     make histogram of real and test particle energies
c     switch = 0 initialize
c            = 1 update
c            = 2 write data to file
c     testpart  if .true. particle is a ghost
c               if .false. particel is a real particle
c     addtail   if .true. eni includes tail correction
c               if .false. tail correction has to be added
 
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'system.inc'
 
      INTEGER NHIs, ntest, nreal, Switch
      LOGICAL Testpart, Addtail
      PARAMETER (NHIs=200)
      DOUBLE PRECISION umax, umin, factu, Eni, rho, CORU, enhelp
      INTEGER fu(0:NHIs), gu(0:NHIs), iu, i
      SAVE ntest, nreal, umax, umin, factu, fu, gu
 
      IF (Switch.EQ.1) THEN
c        ---udate histograms
         IF (TAILCO) THEN
c           ---add tail corrections if not yet added
            IF (Addtail) THEN
               rho = NPART/(BOX**3)
               enhelp = Eni + 2*CORU(RC, rho)
            ELSE
               enhelp = Eni
            END IF
         ELSE
            enhelp = Eni
         END IF
         iu = INT((enhelp-umin)*factu)
         IF (Testpart) THEN
            ntest = ntest + 1
            IF (iu.GT.0.AND.iu.LE.NHIs) fu(iu) = fu(iu) + 1
         ELSE
            nreal = nreal + 1
            IF (iu.GT.0.AND.iu.LE.NHIs) gu(iu) = gu(iu) + 1
         END IF
      ELSE IF (Switch.EQ.0) THEN
c        ---initialize
         umax = 20D0
         umin = -20D0
         factu = NHIs/(umax-umin)
         DO i = 1, NHIs
            fu(i) = 0
            gu(i) = 0
         END DO
         nreal = 0
         ntest = 0
      ELSE IF (Switch.EQ.2) THEN
c        ---write to file
         WRITE (6, *) ' write results f-g sampling '
         WRITE (6, *) ' Number of samples test particles ', ntest
         WRITE (6, *) ' Number of samples real particles ', nreal
         DO i = 1, NHIs
            enhelp = umin + (i+0.5)/factu
            WRITE (77, *) enhelp, REAL(fu(i))/ntest, 
     &                    LOG(REAL(fu(i))/ntest) - enhelp*BETA/2
            WRITE (78, *) enhelp, REAL(gu(i))/nreal, 
     &                    LOG(REAL(gu(i))/nreal) + enhelp*BETA/2
         END DO
      ELSE
         STOP 'error switch: distri'
      END IF
      RETURN
      END
