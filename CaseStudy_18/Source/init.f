**==init.spg  processed by SPAG 4.52O  at 11:03 on 18 Oct 1996
      SUBROUTINE INIT(Ncycl, K, Ndisp, Ncbmc, Nvol, Isamp, Dx, Dv)
      IMPLICIT NONE
      INCLUDE 'particle.inc'
      INCLUDE 'par.inc'
      INCLUDE 'potential.inc'
      LOGICAL lattice
      INTEGER Ncycl, K, Ndisp, Ncbmc, Nvol, Isamp, i, j
      DOUBLE PRECISION temp, rho, vol, rc, Dx, Dv
 
c     ---read input data
      READ (15, *)
      READ (15, *) lattice, Ncycl
      READ (15, *)
      READ (15, *) ELL, NPART, BOX, temp, P
      READ (15, *)
      READ (15, *) KV, L0, K, rc, Dx, Dv
      READ (15, *)
      READ (15, *) Ndisp, Ncbmc, Nvol, Isamp
 
      IF (lattice) THEN
c     ---start from lattice
         PRINT *, ' Start configuration from lattice '
         CALL INITLAT
      ELSE
c     ---start from file
         READ (11, *) BOX, HBOX
         READ (11, *) NPART, ELL
         READ (11, *) Dx, Dv
         DO i = 1, NPART
            DO j = 1, ELL
               READ (11, *) X(i, j), Y(i, j), Z(i, j)
            END DO
         END DO
         REWIND (11)
      END IF
      IF (Nvol.NE.0) WRITE (6, *) ' ***** NPT-simulations ********'
c     ---determine variables
      BETA = 1/temp
      vol = BOX*BOX*BOX
      HBOX = BOX/2
      BOXI = 1/BOX
      rho = NPART/vol
      RC2 = rc*rc
      IF (SHIft) ECUT = 4*((1/rc**12)-(1/rc**6))
c     ---print input
      WRITE (6, 99001) NPART, ELL, temp, P, rho, rho*ELL, BOX, KV, 
     &                 Ncycl, Ndisp, Ncbmc, Nvol, Dx, Dv, K
      RETURN
99001 FORMAT (' ************ Program CBMC *************** ', ////, 
     &        '  Number of particles         : ', i6, /, 
     &        '  Chain length                : ', i6, /, 
     &        '  Temperature                 : ', f6.2, /, 
     &        '  Pressure                    : ', f6.2, /, 
     &        '  Density                     : ', f6.3, /, 
     &        '  Segment density             : ', f6.3, /, 
     &        '  Box length                  : ', f6.2, /, 
     &        '  Bond-vibration K            : ', f6.2, /, 
     &        '  Number of Monte Carlo cycles: ', i6, /, 
     &        '  Number of disp per cycle    : ', i6, /, 
     &        '  Number of CBMC per cycle    : ', i6, /, 
     &        '  Number of vol.ch. per cycle : ', i6, /, 
     &        '  Max displ                   : ', f6.3, /, 
     &        '  Max vol step                : ', f6.3, /, 
     &        '  Number of trial orientations: ', i6, ///)
      END
