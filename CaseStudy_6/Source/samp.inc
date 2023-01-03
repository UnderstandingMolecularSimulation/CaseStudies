c samp.inc
      INTEGER iout1,igr,iout2,ntvacf,it0,nsamp,itstress0,iout3
     &        ,iout4
      LOGICAL SAMP1,SAMP2
      DOUBLE PRECISION TDIFMAX
      common/sampp1/nsamp,igr,ntvacf,it0,itstress0
      common/outfiles/iout1,iout2,iout3,iout4
      COMMON/sampp2/TDIFMAX,SAMP1,SAMP2
c
c     nsamp   = sample frequency: every nsamp timestep the "sample"
c                routines are called
c     igr     = (x nsamp) sample frequency radial distribution function;
c     ntvactf = (x nsamp) sample frequency velocity auto correlation
c                  function
c     it0     = (x nsamp x ntvacf) sample frequency time zero for
c               velocity auto correlation function   
c     itstress0 = (x nsamp) sample frequency stress tensor
c
c     SAMP1 = .true. conventional scheme to determine diffusion is used
c     SAMP2 = .true. order N scheme to determine diffusion is used
c     TDIFMAX = maximum time to determine diffusion
c
c     iout1   = output file for radial distribution function
c     iout2   = output file for velocity autocorrelation function and
c               mean square displacement
c     iout3   = output file for stress tensor
c     iout4   = output file for velocity autocorrelation function
c               with error bars
c     
c     

