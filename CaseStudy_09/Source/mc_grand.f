**==mc_grand.spg  processed by SPAG 4.52O  at 18:49 on  6 Jun 1996
      PROGRAM MC_GRAND
c
c  Calculate equation of state using Grand-canonical Monte Carlo
c  for Lennard-Jones fluid
c
      IMPLICIT NONE
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'system.inc'
      INCLUDE 'grand.inc'
      INTEGER iseed, equil, prod, nsamp, ii, icycl, ndispl, attempt, 
     &        nacc, ncycl, nmoves, imove, nexch, acce, atte, nsampav
      DOUBLE PRECISION en, ent, vir, virt, dr, RANF, ran, muex, rhoav, 
     &                 mu
 
      WRITE (6, *) '**************** MC_GRAND ***************'
c     ---initialize sysem
      CALL READDAT(equil, prod, nsamp, ndispl, dr, nexch, iseed)
c     ---initialise and test random number generator
      CALL RANTEST(iseed)
      nmoves = ndispl + nexch
c     ---total energy of the system
      CALL TOTERG(en, vir)
      WRITE (6, 99001) en, vir
c     ---start MC-cycle
      DO ii = 1, 2
c        --- ii=1 equilibration
c        --- ii=2 production
         IF (ii.EQ.1) THEN
            ncycl = equil
            IF (ncycl.NE.0) WRITE (6, *) ' Start equilibration '
         ELSE
            IF (ncycl.NE.0) WRITE (6, *) ' Start production '
            ncycl = prod
         END IF
         attempt = 0
         nacc = 0
         atte = 0
         acce = 0
         rhoav = 0
         nsampav = 0
c        ---intialize the subroutine that adjust the maximum displacement
         CALL ADJUST(attempt, nacc, dr)
         DO icycl = 1, ncycl
            DO imove = 1, nmoves
               ran = RANF(iseed)*nmoves
               IF (ran.LT.ndispl) THEN
c                 ---attempt to displace a particle
                  CALL MCMOVE(en, vir, attempt, nacc, dr, iseed)
               ELSE
c                 ---attempt to exchange a particle with the bath
                  CALL MCEXCH(en, vir, atte, acce, iseed)
               END IF
            END DO
            IF (ii.EQ.2) THEN
c              ---sample averages
               IF (MOD(icycl,nsamp).EQ.0) THEN
                  CALL SAMPLE(icycl, en, vir)
c                   ---to determine exess chem. potential
                  nsampav = nsampav + 1
                  rhoav = rhoav + NPART/(BOX*BOX*BOX)
               END IF
            END IF
            IF (MOD(icycl,ncycl/5).EQ.0) THEN
               WRITE (6, *) '======>> Done ', icycl, ' out of ', ncycl, 
     &                      NPART
c              ---write intermediate configuration to file
               CALL STORE(8, dr)
c              ---adjust maximum displacements
               CALL ADJUST(attempt, nacc, dr)
            END IF
         END DO
         IF (ncycl.NE.0) THEN
            IF (attempt.NE.0) WRITE (6, 99003) attempt, nacc, 
     &                               100.*FLOAT(nacc)/FLOAT(attempt)
            IF (atte.NE.0) WRITE (6, 99004) atte, acce, 100.*FLOAT(acce)
     &                            /FLOAT(atte)
c           ---test total energy
            CALL TOTERG(ent, virt)
            IF (ABS(ent-en).GT.1.D-6) THEN
               WRITE (6, *) 
     &                    ' ######### PROBLEMS ENERGY ################ '
            END IF
            IF (ABS(virt-vir).GT.1.D-6) THEN
               WRITE (6, *) 
     &                    ' ######### PROBLEMS VIRIAL ################ '
            END IF
            WRITE (6, 99002) ent, en, ent - en, virt, vir, virt - vir
c           ---determine excess chemical potential reservoir:
            IF (rhoav.NE.0.AND.nsampav.NE.0) THEN
               muex = LOG(ZZ)/BETA - LOG(rhoav/nsampav)/BETA
               mu = muex + LOG(rhoav/nsampav)/BETA
               WRITE (6, 99005) muex, mu
            END IF
         END IF
      END DO
      CALL STORE(21, dr)
      STOP
 
99001 FORMAT (' Total energy initial configuration: ', f12.5, /, 
     &        ' Total virial initial configuration: ', f12.5)
99002 FORMAT (' Total energy end of simulation    : ', f12.5, /, 
     &        '       running energy              : ', f12.5, /, 
     &        '       difference                  :  ', e12.5, /, 
     &        ' Total virial end of simulation    : ', f12.5, /, 
     &        '       running virial              : ', f12.5, /, 
     &        '       difference                  :  ', e12.5)
99003 FORMAT (' Number of att. to displ. a part.  : ', i10, /, 
     &        ' success: ', i10, '(= ', f5.2, '%)')
99004 FORMAT (' Number of att. to exch. part      : ', i10, /, 
     &        ' success: ', i10, '(= ', f5.2, '%)')
99005 FORMAT (' Excess chemical potential (res.)  : ', f12.5, /, 
     &        ' Chemical potential        (res.)  : ', f12.5)
      END
