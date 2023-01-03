c conf.inc
      double precision x,y,z
      integer npart,id,npbox
      common/conf1/x(npmax),y(npmax),z(npmax),id(npmax),npart
      common/conf2/npbox(2)
c
c     x(i),y(i),z(i)    : position particle i
c     id(i)             : box 1 or 2
c     npart             : actual number of particles
c     npbox(i)          : number of particles in box i
