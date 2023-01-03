**==block.spg  processed by SPAG 4.52O  at 16:45 on 27 Sep 1995
      PROGRAM BLOCK
c
c calculates block averages using the method of Flyberg and Petersen
c JCP 91 (1989) pg 461
c
      IMPLICIT NONE
      INTEGER i, ii, j, data, nblok, idum, NBLokm, NSAmpm, opbl, nb20
      PARAMETER (NBLokm=150000, NSAmpm=10)
      DOUBLE PRECISION bdata(NBLokm, NSAmpm), av(NSAmpm), sav(NSAmpm), 
     &                 sum(NSAmpm), ssum(NSAmpm), svv(NSAmpm), 
     &                 avv(NSAmpm)
      CHARACTER*6 name(NSAmpm)
 
      WRITE (6, *) ' ***** Calculate block averages ************'
      READ (31, *)
      READ (31, *) data
      PRINT *, '  number of averages ', data
      DO i = 1, data
         READ (31, *) name(i)
      END DO
      DO j = 1, data
         av(j) = 0
         sav(j) = 0
      END DO
      nblok = 0
c      do i=1,15000
      DO i = 1, NBLokm
         READ (32, *, END=100) idum, (bdata(i,j), j=1, data)
         nblok = nblok + 1
         DO j = 1, data
            av(j) = av(j) + bdata(i, j)
            sav(j) = sav(j) + bdata(i, j)*bdata(i, j)
         END DO
      END DO
 100  idum = 0
      nb20 = nblok/20
      DO j = 1, data
         avv(j) = 0.D0
         svv(j) = 0.D0
      END DO
      DO ii = 1, 20
         DO j = 1, data
            sum(j) = 0.D0
            ssum(j) = 0.D0
         END DO
         DO i = 1, nb20
            idum = idum + 1
            DO j = 1, data
               sum(j) = sum(j) + bdata(idum, j)
               ssum(j) = ssum(j) + bdata(idum, j)**2
            END DO
         END DO
         DO j = 1, data
            avv(j) = avv(j) + sum(j)/nb20
            svv(j) = svv(j) + ssum(j)/nb20
         END DO
         WRITE (6, 99004) ii, (sum(j)/(nblok/20.D0), j=1, data)
      END DO
      DO j = 1, data
         avv(j) = avv(j)/20
         svv(j) = svv(j)/20 - avv(j)*avv(j)
         WRITE (6, 99003) name(j), avv(j), SQRT(svv(j)/20.D0)
      END DO
 
      WRITE (6, *) '  number of blocks ', nblok
      DO j = 1, data
         av(j) = av(j)/nblok
         sav(j) = (sav(j)/nblok) - av(j)*av(j)
c        ---estimate: <c0/(n-1)>
         av(j) = SQRT(sav(j)/(nblok-1))
         sav(j) = av(j)/SQRT(2.*(nblok-1.))
      END DO
 
      opbl = 0
      WRITE (6, 99002) opbl, (av(j), sav(j), j=1, data)
c
c perform block transformations
c
      DO WHILE (nblok.GE.4)
         nblok = nblok/2
         i = 1
         DO j = 1, data
            av(j) = 0
            sav(j) = 0
         END DO
         DO ii = 1, nblok
            DO j = 1, data
               bdata(ii, j) = (bdata(i,j)+bdata(i+1,j))/2.D0
               av(j) = av(j) + bdata(ii, j)
               sav(j) = sav(j) + bdata(ii, j)*bdata(ii, j)
            END DO
            i = i + 2
         END DO
         DO j = 1, data
            av(j) = av(j)/nblok
            sav(j) = (sav(j)/nblok) - av(j)*av(j)
         END DO
         DO j = 1, data
            av(j) = SQRT(sav(j)/(nblok-1))
            sav(j) = av(j)/SQRT(2.*(nblok-1.))
         END DO
         opbl = opbl + 1
         WRITE (6, 99002) opbl, (av(j), sav(j), j=1, data)
      END DO
      STOP
 
 
99001 FORMAT (i10, 30(e12.4))
99002 FORMAT (1x, i9, 30(1x,f8.4))
99003 FORMAT (' ######', 1x, a6, 2x, f16.8, '   ', f16.8)
99004 FORMAT (' block data ', 1x, i4, 30(1x,f10.6))
      END
 
 
 
 
 
 
 
