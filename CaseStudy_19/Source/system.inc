      Integer          Ellmax,Kmax

      Parameter       (Ellmax  = 1000)
      Parameter       (Kmax    = 1000)
      
      Integer          Ell,Nchoi
      Logical          Ladvanced
      
      Double Precision X(Ellmax),Y(Ellmax),Z(Ellmax),
     &                 Beta,Utot,Kv

      Common /Ppar/    Kv,Utot,X,Y,Z,Beta,Ell,Nchoi,
     &                 Ladvanced
