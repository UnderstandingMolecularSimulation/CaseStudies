**==md.spg  processed by SPAG 4.52O  at 15:46 on 28 Mar 1996
      PROGRAM MD
c________________________________________________________________________
c
c   Understanding Molecular Simulations: From Algorithms to Applications
c
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
c
c   Case Study 4:  Static properties of the Lennard-Jones fluid
c
c__________________________________________________________________________
c
      IMPLICIT NONE
      INTEGER nstep, nstep10, step
      LOGICAL scale
      DOUBLE PRECISION en, ent, vir, virt, enk, time, enpot, delt, tmax, 
     &                 enkt, tequil, temprsq
c --common blocks declaration:
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'veloc.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'system.inc'
      INCLUDE 'samp.inc'
      DOUBLE PRECISION fx(NPMax), fy(NPMax), fz(NPMax)
 
 
      WRITE (6, *) '**************** MC_NPT ***************'
c     ---initialize sysem
      CALL INIT(delt, tmax, tequil, temprsq, scale)
c     ---total energy of the system
      CALL TOTERG(en, vir, enk)
      WRITE (6, 99001) en - enk, enk, en + enk, vir
      step = 0
      time = 0
      IF (SAMP1) CALL SAMPLE(0, step, en, vir, enk, delt)
      IF (SAMP2) CALL SAMPLE2(0, delt)
      nstep = INT(tmax/delt)
      nstep10 = INT(nstep/10)
      IF (nstep.EQ.0) nstep10 = 0
      DO WHILE (time.LT.tmax)
         CALL FORCE(fx, fy, fz, enpot, vir)
         CALL SOLVE(fx, fy, fz, enk, delt)
         time = time + delt
         en = enpot + enk
         step = step + 1
         IF (time.LT.tequil) THEN
            IF (scale) THEN
               IF (MOD(step,20).EQ.0) CALL VELOCS(temprsq)
            END IF
c           ---if system equilbrated sample averages:
         ELSE IF (MOD(step,NSAMP).EQ.0) THEN
            IF (SAMP1) CALL SAMPLE(1, step, en, vir, enk, delt)
            IF (SAMP2) CALL SAMPLE2(1, delt)
         END IF
         IF (MOD(step,nstep10).EQ.0) THEN
            WRITE (6, *) '======>> Done ', SNGL(time), ' out of ', 
     &                   SNGL(tmax), en
c           ---write intermediate configuration to file
            CALL STORE(8)
         END IF
      END DO
      CALL TOTERG(ent, virt, enkt)
      IF (SAMP1) CALL SAMPLE(2, step, en, vir, enk, delt)
      IF (SAMP2) CALL SAMPLE2(2, delt)
      WRITE (6, 99002) ent, virt
      CALL STORE(21)
      STOP
 
99001 FORMAT (' Total pot. energy in. conf.       : ', f12.5, /, 
     &        ' Total kinetic energy in. conf.    : ', f12.5, /, 
     &        ' Total energy in. conf.            : ', f12.5, /, 
     &        ' Total virial initial configuration: ', f12.5)
99002 FORMAT (' Total energy end of simulation    : ', f12.5, /, 
     &        ' Total virial end of simulation    : ', f12.5)
 
      END
