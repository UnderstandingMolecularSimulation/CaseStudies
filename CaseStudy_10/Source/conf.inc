c conf.inc
      double precision x,y,z,nu
      integer npart
      common/conf1/x(npmax),y(npmax),z(npmax),nu,npart
c
c     x(i),y(i),z(i)    : position particle i
c     nu                : parameter Poisson distribution
c     npart             : actual number of particles
