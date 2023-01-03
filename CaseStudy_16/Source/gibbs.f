**==gibbs.spg  processed by SPAG 4.52O  at 18:10 on 19 Jul 1996
      PROGRAM GIBBS
c     ---Gibbs-ensemble simulation of the Lennard-Joned fluid
      IMPLICIT NONE
      INTEGER iseed, equil, prod, nsamp, ii, icycl, ndispl, attempt, 
     &        nacc, ncycl, nmoves, imove, nvol, accv, attv, ib, nswap, 
     &        accsw, attsw
      DOUBLE PRECISION en(2), ent(2), vir(2), virt(2), vmax, dr, RANF, 
     &                 ran, succ
c --common blocks declaration:
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'npt.inc'
      INCLUDE 'potential.inc'
      INCLUDE 'system.inc'
 
      WRITE (6, *) '**************** MC_NPT ***************'
c     ---initialize sysem
      CALL READDAT(equil, prod, nsamp, ndispl, dr, nvol, vmax, nswap, 
     &             iseed, succ)
      nmoves = ndispl + nvol + nswap
c     ---total energy of the system
      DO ib = 1, 2
         CALL TOTERG(en(ib), vir(ib), ib)
         WRITE (6, 99001) ib, en(ib), vir(ib)
      END DO
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
         attsw = 0
         accsw = 0
c        ---initialize calculation chemical potential
         CALL INIT_CHEM(0)
c        ---intialize the subroutine that adjust the maximum displacement
         CALL ADJUST(attempt, nacc, dr, attv, accv, vmax, succ)
         DO icycl = 1, ncycl
            DO imove = 1, nmoves
               ran = RANF(iseed)*(ndispl+nvol+nswap)
               IF (ran.LT.ndispl) THEN
c                 ---attempt to displace a particle
                  CALL MCMOVE(en, vir, attempt, nacc, dr, iseed)
               ELSE IF (ran.LT.ndispl+nvol) THEN
c                 ---attempt to change the volume
                  CALL MCVOL(en, vir, attv, accv, vmax, iseed)
               ELSE
c                 ---attemp to exchange particles
                  CALL MCSWAP(en, vir, attsw, accsw, iseed)
 
               END IF
            END DO
            IF (ii.EQ.2) THEN
c              ---sample averages
               IF (MOD(icycl,nsamp).EQ.0) CALL SAMPLE(icycl, en, vir)
            END IF
            IF (MOD(icycl,ncycl/5).EQ.0) THEN
               WRITE (6, *) '======>> Done ', icycl, ' out of ', ncycl, 
     &                      NPBOX(1), NPBOX(2)
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
            IF (attsw.NE.0) WRITE (6, 99005) attsw, accsw, 
     &                             100.*FLOAT(accsw)/FLOAT(attsw)
            DO ib = 1, 2
c              ---test total energy
               CALL TOTERG(ent(ib), virt(ib), ib)
               IF (ABS(ent(ib)-en(ib)).GT.1.D-6) THEN
                  WRITE (6, *) 
     &                    ' ######### PROBLEMS ENERGY ################ '
               END IF
               IF (ABS(virt(ib)-vir(ib)).GT.1.D-6) THEN
                  WRITE (6, *) 
     &                    ' ######### PROBLEMS VIRIAL ################ '
               END IF
               WRITE (6, 99002) ib, ent(ib), en(ib), ent(ib) - en(ib), 
     &                          virt(ib), vir(ib), virt(ib) - vir(ib)
            END DO
c           ---calculation chemical potential
            CALL INIT_CHEM(2)
         END IF
      END DO
      CALL STORE(21, dr, vmax)
      STOP
 
99001 FORMAT (' box : ', i3, /, 
     &        ' Total energy initial configuration : ', f12.5, /, 
     &        ' Total virial initial configuration : ', f12.5)
99002 FORMAT (' box : ', i3, /, ' Total energy end of simulation   : ', 
     &        f12.5, /, '       running energy             : ', f12.5, 
     &        /, '       difference                 :  ', e12.5, /, 
     &        ' Total virial end of simulation   : ', f12.5, /, 
     &        '       running virial             : ', f12.5, /, 
     &        '       difference                 :  ', e12.5)
99003 FORMAT (' Number of att. to displ. a part.  : ', i10, /, 
     &        ' success: ', i10, '(= ', f5.2, '%)')
99004 FORMAT (' Number of att. to chan. volume    : ', i10, /, 
     &        ' success: ', i10, '(= ', f5.2, '%)')
99005 FORMAT (' Number of att. to exchange part.  : ', i10, /, 
     &        ' success: ', i10, '(= ', f5.2, '%)')
      END
