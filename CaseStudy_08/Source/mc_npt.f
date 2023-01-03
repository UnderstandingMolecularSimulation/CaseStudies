**==mc_npt.spg  processed by SPAG 4.52O  at 10:45 on  5 Jun 1996
      PROGRAM MC_NPT
      IMPLICIT NONE
      INTEGER iseed, equil, prod, nsamp, ii, icycl, ndispl, attempt, 
     &        nacc, ncycl, nmoves, imove, nvol, accv, attv
      DOUBLE PRECISION en, ent, vir, virt, vmax, dr, RANF, ran, succ
c --common blocks declaration:
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'npt.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'system.inc'
 
      WRITE (6, *) '**************** MC_NPT ***************'
c     ---initialize sysem
      CALL READDAT(equil, prod, nsamp, ndispl, dr, nvol, vmax, iseed, 
     &             succ)
      nmoves = ndispl + nvol
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
         attv = 0
         accv = 0
c        ---intialize the subroutine that adjust the maximum displacement
         CALL ADJUST(attempt, nacc, dr, attv, accv, vmax, succ)
         DO icycl = 1, ncycl
            DO imove = 1, nmoves
               ran = RANF(iseed)*(ndispl+nvol)
               IF (ran.LT.ndispl) THEN
c                 ---attempt to displace a particle
                  CALL MCMOVE(en, vir, attempt, nacc, dr, iseed)
               ELSE
c                 ---attempt to change the volume
                  CALL MCVOL(en, vir, attv, accv, vmax, iseed)
               END IF
            END DO
            IF (ii.EQ.2) THEN
c              ---sample averages
               IF (MOD(icycl,nsamp).EQ.0) CALL SAMPLE(icycl, en, vir)
            END IF
            IF (MOD(icycl,ncycl/5).EQ.0) THEN
               WRITE (6, *) '======>> Done ', icycl, ' out of ', ncycl
c              ---write intermediate configuration to file
               CALL STORE(8, dr, vmax)
c              ---adjust maximum displacements
               CALL ADJUST(attempt, nacc, dr, attv, accv, vmax, succ)
            END IF
         END DO
         IF (ncycl.NE.0) THEN
            IF (attempt.NE.0) WRITE (6, 99003) attempt, nacc, 
     &                               100.*FLOAT(nacc)/FLOAT(attempt)
            IF (attv.NE.0) WRITE (6, 99004) attv, accv, 100.*FLOAT(accv)
     &                            /FLOAT(attv)
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
         END IF
      END DO
      CALL STORE(21, dr, vmax)
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
99004 FORMAT (' Number of att. to chan. volume    : ', i10, /, 
     &        ' success: ', i10, '(= ', f5.2, '%)')
      END
