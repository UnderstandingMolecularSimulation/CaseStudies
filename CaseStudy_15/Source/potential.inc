c potential.inc 
      double precision eps4,sig2,mass,ecut,rc2,pi
     $             ,eps48,rc
      logical tailco,shift
      common/pot1/pi,eps4,eps48,sig2,mass,rc,rc2,ecut
     $        ,tailco,shift
c
c     pi        : pi = 3.14 ..
c     eps4      : 4 * epsilon 
c     eps48     : 48 * epsilon
c     (epsilon) : energy parameter Lennard-Jones potential
c     sig2      : sigma*sigma
c     (sigma)   : size parameter Lennard-Jones potenital
c     mass      : mass of the molecules
c     rc        : cut-off radius of the potenial
c     rc2       : rc * rc
c     ecut      : energy at cut-off radius
c     tailco    : .true. apply tail corrections
c     shift     : .true. shift the potential
