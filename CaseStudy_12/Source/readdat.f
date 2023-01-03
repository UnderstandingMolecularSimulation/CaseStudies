**==readdat.spg  processed by SPAG 4.52O  at 14:19 on 24 Jun 1996
      SUBROUTINE READDAT(X, Ux, Delt, Tmax, Isamp)
c
c  read data from disk
c
c     X     (output): intitial position oscillator
c     Ux    (output): intitial velocity oscillator
c     Delt  (output): time step integration
c     Tmax  (output): total simulation time
c     Isamp (output): sample frequency
 
      IMPLICIT NONE
      DOUBLE PRECISION X, Ux, Delt, Tmax
      INTEGER i, Isamp
      INCLUDE 'andersen.inc'
      INCLUDE 'nosehoover.inc'
      INCLUDE 'nvt.inc'
 
      READ (15, *)
      READ (15, *) X, Ux
      READ (15, *)
      READ (15, *) Delt, Tmax, Isamp
      READ (15, *)
      READ (15, *) NVT, TEMP
      READ (15, *)
      READ (15, *) METHT
c --- Input Andersen method (metht=1)
      READ (15, *)
      READ (15, *)
      READ (15, *) NCOLL
c --- Input Nose-Hoover method (metht=2)
c
c     M=1 : standard Nose-Hoover method
c     M>1 : Nose Hoover cahins
c
      READ (15, *)
      READ (15, *)
      READ (15, *) FH, M
      READ (15, *) (QH(i), i=1, M)
 
      WRITE (6, *) '****** Harmonic oscillator *******'
      WRITE (6, *) ' Velocity Verlet algorithm '
      IF (NVT) THEN
         WRITE (6, *) ' NVT ensemble '
         IF (METHT.EQ.1) THEN
            WRITE (6, *) ' Andersen thermostat '
            ISEED = 3456734
            CALL RANTEST(ISEED)
         ELSE IF (METHT.EQ.2) THEN
            WRITE (6, *) ' Nose-Hoover thermostat '
            WRITE (6, 99003) M, (QH(i), i=1, M)
            DO i = 1, M
               IF (i.EQ.1) THEN
                  XI(i) = 1/QH(i)
                  XI(i) = 0.D0
                  PXI(i) = (Ux*Ux-FH*TEMP)/QH(i)
               ELSE
                  XI(i) = 0.D0
                  PXI(i) = (QH(i-1)*XI(i-1)**2-TEMP)/QH(i)
               END IF
               SC(i) = 0.D0
               PSC(i) = XI(i)
            END DO
         ELSE
            STOP 'method not there'
         END IF
      ELSE
         WRITE (6, *) ' Micro canonical ensemble '
      END IF
      WRITE (6, 99001) X, Ux, Delt, Tmax, Isamp
      RETURN
99001 FORMAT (' Initial position        :', f7.2, /, 
     &        ' Initial velocity        :', f7.2, /, 
     &        ' Time step               :', f7.4, /, 
     &        ' Total simulation time   :', f8.1, /, 
     &        ' Sampling frequency      :', i5)
99002 FORMAT (' Coupling constant Q     :', f5.2)
99003 FORMAT (' Number of Noose-Hoover chains: ', i4, /, 
     &        '  Coupling constants Q   :', 10(2x,f5.2))
      END
