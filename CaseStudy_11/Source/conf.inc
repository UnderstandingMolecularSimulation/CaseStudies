c conf.inc 
      double precision x,y,z,q,s,ps
      integer npart,g
      common/conf1/x(npmax),y(npmax),z(npmax),npart
      common/conf2/q,s,ps,g
c
c     x(i),y(i),z(i)    : position particle i
c     npart             : actual number of particles
c     q                 : Nose Hoover Mass
c     g                 : number of degrees of freedom
c     s,ps              : Nose-Hoover coupling parameter
