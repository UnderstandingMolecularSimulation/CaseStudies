**==harm.spg  processed by SPAG 4.52O  at 14:19 on 24 Jun 1996
      PROGRAM HARM
c________________________________________________________________________
c
c   Understanding Molecular Simulations: From Algorithms to Applications
c
c                 Daan Frenkel  and   Berend Smit
c
c  We make no warranties, express or implied, that the programs contained
c  in this work are free of error, or that they will meet your requirements
c  for any particular application. They should not be relied on for solving
c  problems whose incorrect solution could results in injury, damage, or
c  loss of property. The authors and publishers disclaim all liability for
c  direct or consequential damages resulting from your use of the programs
c
c__________________________________________________________________________
c
c                 Case Study 12: Harmonic Oscillator (I)
c                 Case Study 13: Harmonic Oscillator (II)
c
c___________________________________________________________________________
 
      IMPLICIT NONE
      DOUBLE PRECISION x, ux, delt, tmax, t, tmax10, tprompt, en0
      INTEGER isamp
      INCLUDE 'nvt.inc'
      INCLUDE 'nosehoover.inc'
      INCLUDE 'andersen.inc'
 
      CALL READDAT(x, ux, delt, tmax, isamp)
      CALL TOTERG(x, ux, en0)
      WRITE (6, 99001) en0
      t = 0
      tmax10 = tmax/10
      tprompt = tmax10
      DO WHILE (t.LE.tmax)
         IF (.NOT.NVT) THEN
            CALL INTEGRATE(x, ux, delt, t, en0)
         ELSE IF (METHT.EQ.1) THEN
c              ---Andersen thermostat
            CALL ANDERSEN(x, ux, delt, t, en0)
         ELSE IF (METHT.EQ.2) THEN
c              ---Nose-Hoover thermostat
            CALL NHCHAIN(x, ux, delt, t, isamp)
         ELSE
            STOP 'methT not present'
         END IF
         IF (t.GT.tprompt) THEN
            tprompt = tprompt + tmax10
            WRITE (6, *) ' time ', t, ' out of ', tmax
         END IF
      END DO
      STOP
99001 FORMAT (' Total energy initial conf: ', f8.4)
      END
