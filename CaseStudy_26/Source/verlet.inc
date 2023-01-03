c verlet.inc
      double precision xv,yv,zv,rv,rv2,skin,skin2
      integer nlist,list
      common/verl1/xv(npmax),yv(npmax),zv(npmax),rv,rv2
     $         ,skin,skin2
      common/verl2/nlist(npmax),list(npmax,npmax)
c
c     xv(i),yv(i),zv(i)     : position of particle i at the time
c                             the list was made
c     rv                    : Verlet radius
c     rv2=rv*rv
c     skin =rv-rc           : difference Verlet radius and cut-off
c     skin2=skin*skin 
c     nlist(i)              : number of particles in Verlet list
c                             particle i
c     list(i,j)             : Verlet-list of particle i
