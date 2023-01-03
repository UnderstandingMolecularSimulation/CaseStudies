c conf.inc
      double precision x,y,z
      integer npart
      common/conf1/x(npmax),y(npmax),z(npmax),npart
c
c     x(i),y(i),z(i)    : position particle i
c     npart             : actual number of particles
