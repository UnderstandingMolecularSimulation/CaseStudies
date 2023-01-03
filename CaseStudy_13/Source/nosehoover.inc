c nosehoover.inc
      integer fh,mmax,m
      parameter (mmax=10)
      double precision xi,pxi,qh,sc,psc
      common/nosehc/xi(mmax),pxi(mmax),qh(mmax),fh,m
      common/nosehs/sc(mmax),psc(mmax)

c     xi(nmax) : friction coefficients
c     pxi(nmax): derivative friction coefficients
c     qh(mmax) : Nose-Hoover masses for the mmax chains
c     fh       : number of degrees of freedom (see text)
c     m        : number of Nose-Hoover chains

c     sc(nmax) : Nose's s variable 
c     psc(nmax): derivative Nose's s variable 
c
