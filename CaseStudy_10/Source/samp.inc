c samp.inc
      integer nlmax
      parameter (nlmax=500)
      INTEGER iout1,igr,iout2,ntvacf,it0,nsamp,itstress0,iout3
      common/samp1/nsamp,iout1,igr,iout2,ntvacf,it0
     &     ,itstress0,iout3

c     nsamp  : frequency with which subroutine sample is called 
c     iout1  : fortran file number output radial distribution function
c     igr    : sample frequency radial distribution function
c              (total sample frequency is: nsamp*igr)
c     iout2  : fortran file number output diffusion results 
c     ntvacf : sample frequency velocity autocorrelation function
c             (total sample frequency is: nsamp*ntvacf)
c     it0    : frequency for a new t=0 in velocity autocorrelation function
c     itstress0: frequency for a new t=0 in stress tensor correlation
c     iout3  : fortran file number output stress tensor correlation
