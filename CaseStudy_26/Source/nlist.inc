c nlist.inc
      double precision factn 
      integer ncel,hoc,ll,celmax,ncelt,neigh
      parameter (celmax=10000,neigh=27)
      common/nlist1/factn,ncel,ncelt
      common/nlist2/ll(npmax),hoc(0:celmax)
c        ll(i)     : linked list particle i
c        hoc(ic)   : head of chain cell ic
c        factn     : used to find celnumber
c        ncel      : number of cells in x, y or z direction
c        ncelt     : total number of cells
c        celmax    : maximum number of cells (change and 
c                    recompile if larger number is needed)
c        neigh     : number of cells for interactions (in
c                    3-dim per definition 27)
