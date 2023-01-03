C     System.Inc
C
C     Information About The System
C
C     Maxpart        : Maximum Number Of Particles
C
C     Fxxl/Fyyl/Fzzl : Lj Forces
C     Fxxs/Fyys/Fzzs : Bond-Stretching Forces
C     Vxx/Vyy/Vzz    : Velocities
C     Vxf/Vyf/Vzf    : Old Velocities
C     Rxx/Ryy/Rzz    : Positions
C     Rxf/Ryf/Rzf    : Old Positions
C     Xoud/Youd/Zoud : Parameters From Previious Verlet List
C     Box            : Boxlength
C     Hbox           : Half Of The Box-Length
C
C     Tstep          : Timestep
C     Nrespa         : Number Of Respa Steps
C
C     Bondl          : Bond-Length
C     Bondkb         : Force Constant
C
C     Lcons          : Do We Use Constraints ?
C     Lrespa         : Do We Use Respa ?
C     Linit          : Do We Use A Previous Configuration From Disk ?
C
C     Nstep          : Number Of Integration Steps
C     Ninit          : Number Of Initialization Steps
C     Npart          : Number Of Particles
C
C     Ukin           : Kinetic Energy
C     Upotl          : Lj Potential Energy
C     Upots          : Stretch Energy
C     Temp           : Temperature
C
C     Nfree          : Degrees Of Freedom
C
C     Rcutsq         : Cut-Off Radius
C     Rcut2          : Second Cut-Off Radius Respa
C     Rcut2sq        : Rcut2*Rcut2
C     Rminlab        : Cut-Off For Respa
C     Lambda         : Respa Parameter
C     Ecut           : Cut-Off Energy
C
C     Point1/2       : Verlet List Information
C     List1/2        : Verlet List Information
C     Rver           : Verlet List Information

      Integer Maxpart,Maxlist

      Parameter (Maxpart = 250)
      Parameter (Maxlist = 60000)

      Double Precision Fxxl(Maxpart),Fyyl(Maxpart),Fzzl(Maxpart),
     &                 Fxxs(Maxpart),Fyys(Maxpart),Fzzs(Maxpart),
     &                 Vxx(Maxpart),Vyy(Maxpart),Vzz(Maxpart),
     &                 Vxf(Maxpart),Vyf(Maxpart),Vzf(Maxpart),
     &                 Xoud(Maxpart),Youd(Maxpart),Zoud(Maxpart),
     &                 Rxx(Maxpart),Ryy(Maxpart),Rzz(Maxpart),
     &                 Rxf(Maxpart),Ryf(Maxpart),Rzf(Maxpart),
     &                 Tstep,Box,Hbox,Bondl,Bondkb,Rcutsq,Ecut,
     &                 Ukin,Upotl,Temp,Upots,Rcut2,Lambda,Rcut2sq,
     &                 Rminlab,Rver,Tx(-13:13),Ty(-13:13),Tz(-13:13)

      Integer          Nstep,Npart,Ninit,Nrespa,Nfree,Idum,
     &                 Point1(Maxpart),
     &                 Point2(Maxpart),List1(Maxlist),List2(Maxlist),
     &                 Imgg1(Maxlist),Imgg2(Maxlist)

      Logical          Lrespa,Lcons,Linit

      Common /Syss/    Tx,Ty,Tz,Fxxl,Fyyl,Fzzl,Fxxs,Fyys,Fzzs,Vxx,Vyy,
     &                 Vzz,Vxf,Vyf,Vzf,Xoud,Youd,Zoud,Rxx,Ryy,Rzz,Rxf,
     &                 Ryf,Rzf,Tstep,Box,Hbox,Bondl,Bondkb,Rcut2,Lambda,
     &                 Rcutsq,Rcut2sq,Rminlab,Ecut,Ukin,Upotl,Rver,Temp,
     &                 Upots,Nstep,Npart,Ninit,Nrespa,Imgg1,Imgg2,Nfree,
     &                 Idum,Point1,Point2,List1,List2,Lrespa,Lcons,Linit
