**==cbmc.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      PROGRAM CBMC
c
c  Configurational-bias Monte Carlo of a system of N chains
c  with lenght ell
c
      IMPLICIT NONE
      INCLUDE 'particle.inc'
      INTEGER ncycl, ic, move, k, switch, iseed, i, movedis, accdis, 
     &        ndisp, ncbmc, icc, nvol
      DOUBLE PRECISION etot, etest, evib, enlj, evibt, enljt, dx, ran, 
     &                 RANF, vir, virt, dv, succ
      INTEGER acc(ELLmax), isamp, accvol, movevol
 
c     --- set and test random number generator
      iseed = 0
      CALL RANTEST(iseed)
c     ---initialise program
      CALL INIT(ncycl, k, ndisp, ncbmc, nvol, isamp, dx, dv)
c     ---caculate total energy:
      CALL TOTERG(etot, evib, enlj, vir)
      WRITE (6, 99001) etot, evib, enlj, vir
c     ---set counters to zero
      movevol = 0
      accvol = 0
      movedis = 0
      accdis = 0
      succ = 50.D0
      CALL ADJUST(movedis, accdis, dx, movevol, accvol, dv, succ)
      move = 0
      DO i = 1, ELL
         acc(i) = 0
      END DO
      switch = 0
      CALL SAMPLE(switch, etot, enlj, vir)
      switch = 1
      DO ic = 1, ncycl
c        ---perform cbmc move
         DO icc = 1, ndisp + ncbmc + nvol
            ran = RANF()*(ndisp+ncbmc+nvol)
            IF (ran.LE.ncbmc) THEN
               CALL MCCBMC(k, move, acc, etot, evib, enlj, vir)
            ELSE IF (ran.LE.(ndisp+ncbmc)) THEN
               CALL MCMOVE(movedis, accdis, etot, evib, enlj, vir, dx)
            ELSE
               CALL MCVOL(movevol, accvol, etot, evib, enlj, vir, dv)
            END IF
         END DO
c     ---take a sample
         IF (MOD(ic,isamp).EQ.0) CALL SAMPLE(switch, etot, enlj, vir)
         IF (MOD(ic,ncycl/5).EQ.0) THEN
            WRITE (6, *) '======>> Done ', ic, ' out of ', ncycl
c              ---adjust maximum displacements
            CALL ADJUST(movedis, accdis, dx, movevol, accvol, dv, succ)
c              ---store configuration on disk
            CALL STORE(21, dx, dv)
         END IF
      END DO
c     ---test energy
      CALL TOTERG(etest, evibt, enljt, virt)
      PRINT *, ' test energies: ', SNGL(etest), SNGL(etot), 
     &      SNGL(etest-etot)
      PRINT *, ' test harmonic: ', SNGL(evibt), SNGL(evib), 
     &      SNGL(evibt-evib)
      PRINT *, ' test LJ      : ', SNGL(enljt), SNGL(enlj), 
     &      SNGL(enljt-enlj)
      PRINT *, ' test virial  : ', SNGL(virt), SNGL(vir), SNGL(virt-vir)
      PRINT *, ' Displacement: # moves: ', movedis, ' accepted : ', 
     &      accdis
      PRINT *, ' Vol. changes: # moves: ', movevol, ' accepted : ', 
     &      accvol
      DO i = 1, ELL
         PRINT *, ' # moves: ', move, ' accepted : ', acc(i)
      END DO
c     ---print results
      switch = 2
      CALL SAMPLE(switch, etot, enlj, vir)
c     ---store configuration on disk
      CALL STORE(21, dx, dv)
      STOP
99001 FORMAT (' Initial Energies  ':, /, '   Total energy    ':, f9.3, 
     &        /, '   vibr. energy    ':, f9.3, /, 
     &        '   LJ.   energy    ':, f9.3, /, '   Total virial    ':, 
     &        f9.3)
      END
