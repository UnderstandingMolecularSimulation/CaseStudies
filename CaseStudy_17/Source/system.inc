c   system.inc
      double precision temp,beta,hbox
     +                ,boxx,boxy,boxz,vol
      common/sys1/temp,beta,boxx,boxy,boxz,hbox,vol
c
c     boxi      : simulation box length i-dim.
c     hbox     : 0.5 * min(boxx,boxy,boxz)
c     temp     : temperature
c     beta     : 1/temp
