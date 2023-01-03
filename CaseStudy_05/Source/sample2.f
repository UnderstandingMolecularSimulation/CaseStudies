**==sample2.spg  processed by SPAG 4.52O  at 15:46 on 28 Mar 1996
      SUBROUTINE SAMPLE2(Switch, Delt)
c
c    Determine the mean square displacement using Algorithm 9
c
c    Switch (input) = 1: sample averages
c                   = 0: initialize varibales
c                   = 2: writes results to disk
c    Delt   (input) time step md simulation
 
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'veloc.inc'
      INCLUDE 'samp.inc'
 
      INTEGER Switch, IBMax, NBLock
      PARAMETER (NBLock=10, IBMax=20)
      INTEGER telav(IBMax, NBLock), i, iblock, j, ib, tvacf, in, 
     &        ibl(IBMax), iblm, inp, ii, inmax, ihbmax
      DOUBLE PRECISION vxb(IBMax, NBLock, NPMax), 
     &                 vyb(IBMax, NBLock, NPMax), 
     &                 vzb(IBMax, NBLock, NPMax), avb(IBMax, NBLock), 
     &                 r2asum, dtime, delx, dely, delz, Delt, time, 
     &                 thmax
 
      SAVE tvacf, vxb, vyb, vzb, telav, dtime, avb, ibl, iblm
 
      IF (Switch.EQ.1) THEN
         tvacf = tvacf + 1
c        ---determine current maximum number of blocks: iblm
         iblm = 1
         ii = tvacf/NBLock
         DO WHILE (ii.NE.0)
            iblm = iblm + 1
            ii = ii/NBLock
c           ---test maximu time not longer than tdifmax:
            time = dtime*(NBLock**(iblm))
            IF (time.GT.TDIFMAX) ii = 0
         END DO
c        ---limit the maximum number of blocks
         iblm = MIN(iblm, IBMax)
         DO ib = 1, iblm
            iblock = NBLock**(ib-1)
c           ---test for blocking operation
            IF (MOD(tvacf,iblock).EQ.0) THEN
               ibl(ib) = ibl(ib) + 1
c              ---limit to length n (=NBLock)
               inmax = MIN(ibl(ib), NBLock)
c              ---for error calculation
               r2asum = 0
               DO i = 1, NPART
                  IF (ib.EQ.1) THEN
c                    ---zero block: ordinary velocity
                     delx = VX(i)
                     dely = VY(i)
                     delz = VZ(i)
                  ELSE
c                    ---(i)th block: coarsed velocity previous block
                     delx = vxb(ib-1, 1, i)
                     dely = vyb(ib-1, 1, i)
                     delz = vzb(ib-1, 1, i)
                  END IF
                  DO in = 1, inmax
                     inp = in
                     IF (inmax.EQ.NBLock) inp = in + 1
                     IF (in.LT.inmax) THEN
                        vxb(ib, in, i) = vxb(ib, inp, i) + delx
                        vyb(ib, in, i) = vyb(ib, inp, i) + dely
                        vzb(ib, in, i) = vzb(ib, inp, i) + delz
                     ELSE
                        vxb(ib, in, i) = delx
                        vyb(ib, in, i) = dely
                        vzb(ib, in, i) = delz
                     END IF
                  END DO
                  DO in = 1, inmax
                     telav(ib, in) = telav(ib, in) + 1
                     avb(ib, in) = avb(ib, in)
     &                             + (vxb(ib,inmax-in+1,i)*dtime)
     &                             **2 + (vyb(ib,inmax-in+1,i)*dtime)
     &                             **2 + (vzb(ib,inmax-in+1,i)*dtime)**2
                     IF (in.EQ.1) r2asum = r2asum + 
     &                   (vxb(ib,inmax-in+1,i)*dtime)
     &                   **2 + (vyb(ib,inmax-in+1,i)*dtime)
     &                   **2 + (vzb(ib,inmax-in+1,i)*dtime)**2
                  END DO
               END DO
               r2asum = r2asum/NPART
c              ---print mean square displacement to file for t=1,10,100,etc
               WRITE (79+ib, *) dtime*(NBLock**(ib-1)), r2asum
            END IF
         END DO
      ELSE IF (Switch.EQ.0) THEN
         tvacf = 0
         dtime = NSAMP*Delt
         DO ib = 1, IBMax
            ibl(ib) = 0
            DO j = 1, NBLock
               telav(ib, j) = 0
               avb(ib, j) = 0
               DO i = 1, NPART
                  vxb(ib, j, i) = 0
                  vyb(ib, j, i) = 0
                  vzb(ib, j, i) = 0
               END DO
            END DO
         END DO
      ELSE IF (Switch.EQ.2) THEN
c        ---write results to file
         thmax = 0
         ihbmax = 0
         DO ib = 1, MIN(IBMax, iblm)
            DO j = 2, MIN(ibl(ib), NBLock)
               IF (telav(ib,j).NE.0) THEN
                  WRITE (70, 99002) j*dtime*(NBLock**(ib-1)), avb(ib, j)
     &                              /telav(ib, j), telav(ib, j)
                  IF (j*dtime*(NBLock**(ib-1)).GT.thmax) THEN
                     ihbmax = telav(ib, j)
                     thmax = j*dtime*(NBLock**(ib-1))
                  END IF
               END IF
            END DO
         END DO
         WRITE (6, *) ' Diffusion calculated with order-n scheme '
         WRITE (6, 99001) 2*dtime, telav(1, 2), thmax, ihbmax
 
      ELSE
         STOP 'Error (sample.f) switch'
      END IF
 
      RETURN
99001 FORMAT (' Number of samples for tmin = ', f8.3, ' is : ', i10, /, 
     &        ' Number of samples for tmax = ', f8.3, ' is : ', i10)
99002 FORMAT (2(2x,e14.4), 2x, i10)
      END
