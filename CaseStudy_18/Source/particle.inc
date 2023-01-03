        integer npmax,ellmax,ell,npart,kmax
        parameter (npmax=100,ellmax=400,kmax=100)
        double precision x,y,z
        common/pos1/ell,npart
        common/pos/ x(npmax,ellmax),y(npmax,ellmax),z(npmax,ellmax)
