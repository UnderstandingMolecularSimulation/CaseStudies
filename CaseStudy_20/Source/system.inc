Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Global Variables                                            C
C                                                                 C 
C     Npmax   = Maximum Number Of Chains                          C
C     Ell     = Maximum Number Of Beads Per Chain                 C
C     Kmax    = Maximum Number Of Trial Directions                C
C     Maxtree = Maximum Number Of Elements In The Tree            C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Integer          Npmax,Ellmax,Kmax,Maxtree

      Parameter       (Npmax   = 160)
      Parameter       (Ellmax  = 50)
      Parameter       (Kmax    = 16)
      Parameter       (Maxtree = 1000000)
      
      Integer          Ell,Nchain,Nrecoil,Parent(Maxtree),
     &                 Nchild(Maxtree),Ipoint,D_Second

      Logical          Lrecoil

      Double Precision X(Ellmax,Npmax),Y(Ellmax,Npmax),
     &                 Z(Ellmax,Npmax),Bondl,Beta,Box,
     &                 Boxi,Rc2,Rccc,Thetat

      Common/Ppar/     X,Y,Z,Bondl,Rccc,Beta,Box,Boxi,Rc2,
     &                 Thetat,D_Second,Ell,Nchain,Nrecoil,
     &                 Parent,Nchild,Ipoint,Lrecoil
