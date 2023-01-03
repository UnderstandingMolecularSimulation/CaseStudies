Cccccccccccccccccccccccccccccc
C	System Variables     C
Cccccccccccccccccccccccccccccc

      Integer Ncx,Ncy,Ncz,Ncell,Npart,Nspring,Aspring(Maxtotal),
     &     Molecule,Smolecule(Maxtotal),Tmolecule(Maxtotal),
     &     Type(Maxtotal),Nstep,Niter,Dcx(14),Dcy(14),Dcz(14),
     &     Mlll,Nprint,Numcom,Ninit,Nuall(Maxcom)

      Double Precision Xx(Maxtotal),Yy(Maxtotal),Zz(Maxtotal),
     &     Vx(Maxtotal),Vy(Maxtotal),Vz(Maxtotal),Boxlx,Boxly,
     &     Boxlz,Hbx,Hby,Hbz,Rlx,Rly,Rlz,Press,Surface,Sigmat,
     &     Gamma,Timestep,Halftime,Bondkb,Beta,Fx(Maxtotal),
     &     Fy(Maxtotal),Fz(Maxtotal),Phrep(Maxtp,Maxtp),Upot

      Common /Srt12q/ Xx,Yy,Zz,Vx,Vy,Vz,Boxlx,Boxly,Boxlz,Hbx,
     &     Hby,Hbz,Rlx,Rly,Rlz,Press,Surface,Sigmat,Gamma,
     &     Timestep,Halftime,Bondkb,Beta,Fx,Fy,Fz,Phrep

      Common /Srt13q/ Upot,Ncx,Ncy,Ncz,Ncell,Npart,Nspring,Aspring

      Common /Srt14q/ Molecule,Smolecule,Tmolecule,Type,Nstep,Niter,
     &     Dcx,Dcy,Dcz,Mlll,Nprint,Numcom,Ninit,Nuall
