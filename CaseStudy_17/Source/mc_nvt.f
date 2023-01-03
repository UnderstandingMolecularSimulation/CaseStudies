**==mc_nvt_solid.spg  processed by SPAG 4.52O  at 16:41 on 22 Jul 1996
 
 
 
 
 
 
 
      PROGRAM MC_NVT_SOLID
c
c  Program to determine the free energy of a hard-sphere solid
c  using the lattice coupling method of Frenkel and Ladd
c
      IMPLICIT NONE
      INTEGER iseed, equil, prod, nsamp, ii, icycl, ndispl, attempt, 
     &        nacc, ncycl, nmoves, imove
      DOUBLE PRECISION en, ent, vir, virt, dr, lambda, ena, enat, succ
      LOGICAL cmc
c --common blocks declaration:
      INCLUDE 'parameter.inc'
      INCLUDE 'conf.inc'
      INCLUDE 'method.inc'
      INCLUDE 'system.inc'
      INCLUDE 'einst.inc'
      INCLUDE 'nlist.inc'
 
c     ---initialize sysem
      CALL READDAT(equil, prod, nsamp, ndispl, dr, lambda, succ, cmc, 
     &             iseed)
      nmoves = ndispl
c     ---total energy of the system
      CALL TOTERG(en, vir, ena, lambda, cmc)
      WRITE (6, 99001) en, vir, ena
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
c        ---intialize the subroutine that adjust the maximum displacement
         CALL ADJUST(attempt, nacc, dr, succ)
         DO icycl = 1, ncycl
            DO imove = 1, nmoves
c              ---attempt to displace a particle
               CALL MCMOVE(en, vir, attempt, nacc, dr, ena, lambda, cmc, 
     &                     iseed)
            END DO
            IF (ii.EQ.2) THEN
c              ---sample averages
               IF (MOD(icycl,nsamp).EQ.0)
     &             CALL SAMPLE(icycl, en, vir, ena, lambda)
            END IF
            IF (MOD(icycl,ncycl/5).EQ.0) THEN
               WRITE (6, *) '======>> Done ', icycl, ' out of ', ncycl
c              ---write intermediate configuration to file
               CALL STORE(8, dr)
c              ---adjust maximum displacements
               CALL ADJUST(attempt, nacc, dr, succ)
            END IF
         END DO
         IF (ncycl.NE.0) THEN
            IF (attempt.NE.0) WRITE (6, 99003) attempt, nacc, 
     &                               100.*FLOAT(nacc)/FLOAT(attempt)
c           ---test total energy
            CALL TOTERG(ent, virt, enat, lambda, cmc)
            IF (ABS(ent-en).GT.1.D-6) THEN
               WRITE (6, *) 
     &                    ' ######### PROBLEMS ENERGY ################ '
            END IF
            IF (ABS(virt-vir).GT.1.D-6) THEN
               WRITE (6, *) 
     &                    ' ######### PROBLEMS VIRIAL ################ '
            END IF
            IF (ABS(enat-ena).GT.1.D-6) THEN
               WRITE (6, *) 
     &                    ' ######### PROBLEMS Lattice ener ########## '
            END IF
            WRITE (6, 99002) ent, en, ent - en, virt, vir, virt - vir, 
     &                       enat, ena, enat - ena
         END IF
      END DO
      CALL STORE(21, dr)
      STOP
 
99001 FORMAT (' Total energy initial configuration: ', f12.5, /, 
     &        ' Energy Einstein lattice           : ', f12.5, /, 
     &        ' Total virial initial configuration: ', f12.5)
99002 FORMAT (' Total energy end of simulation    : ', f12.5, /, 
     &        '       running energy              : ', f12.5, /, 
     &        '       difference                  :  ', e12.5, /, 
     &        ' Total virial end of simulation    : ', f12.5, /, 
     &        '       running virial              : ', f12.5, /, 
     &        '       difference                  :  ', e12.5, /, 
     &        ' Total lattice energy end sim.     : ', f12.5, /, 
     &        '       running energy              : ', f12.5, /, 
     &        '       difference                  :  ', e12.5)
99003 FORMAT (' Number of att. to displ. a part.  : ', i10, /, 
     &        ' success: ', i10, '(= ', f5.2, '%)')
      END
