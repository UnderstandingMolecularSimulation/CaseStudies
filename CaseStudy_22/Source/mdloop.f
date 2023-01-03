      Subroutine Mdloop
      Implicit None
 
      Include 'system.inc'
 
      Integer I,Kkk
      Double Precision Utot,Impx,Impy,Impz,Sume,
     &     Sumnum,Ezero,Warm,Ttime,Xx
 
      Sume = 0.0d0
      Sumnum = 0.0d0
      Ezero = 0.0d0
      Xx = 0.0d0
 
      Write (6,'(2a)') '  Tstep         Utot         Ukin',
     &     '        Upotl        Upots         Temp'
 
      Do I = 1,Nstep
 
Cccccccccccccccccccccccccccc
C     Calculate The Force  C
Cccccccccccccccccccccccccccc
 
         If(I.Eq.Ninit) Xx = Ttime()

         If (I.Le.Ninit .Or. (.Not.Lrespa)) Then
 
            Do Kkk = 1,Nrespa
               Call Force(.True.,.True.)
               Call Integrate(I)
            Enddo
         Else
            If(I.Eq.(Ninit+1)) Call Force(.True.,.True.)
            Call Integrate(I)
         Endif
 
         Utot = Ukin + Upotl + Upots
 
         If (I.Eq.(Ninit+5)) Then
            Ezero = Utot
         Elseif(I.Gt.(Ninit+5)) Then
            Sumnum = Sumnum + 1.0d0
            Sume = Sume + Dabs((Utot-Ezero)/Ezero)
         Endif
 
         Warm = 2.0d0*Ukin/Dble(Nfree)
 
         If (Mod(I,100).Eq.0) Write (6,'(I7,8(1x,E12.5))') 
     &        I,Utot,Ukin,Upotl,Upots,Warm
 
         If (Warm.Gt.100.0d0) Then
            Write (6,*) 'System Is Going To Explode !!! Exit !!!'
            Stop
         Endif
 
      Enddo
 
      Impx = 0.0d0
      Impy = 0.0d0
      Impz = 0.0d0
 
      Do I = 1,Npart
         Impx = Impx + Vxx(I)
         Impy = Impy + Vyy(I)
         Impz = Impz + Vzz(I)
      Enddo
 
      Impx = Dabs(Impx)/Dble(Npart)
      Impy = Dabs(Impy)/Dble(Npart)
      Impz = Dabs(Impz)/Dble(Npart)
      Impx = Max(Impx,Impy,Impz)
 
      Write (6,*)
      Write (6,*) 'Elapsed Time/Length   : ',(Ttime()-Xx)/
     &     (Dble(Nstep-Ninit)*Tstep*Dble(Nrespa))

      Write (6,*) 'Final Impulse         : ',Impx
 
      If (Sumnum.Gt.0.5d0) Write (6,*) 'Energy Drift          : ',
     &     Sume/Sumnum
 
      Do I = 1,Npart
         Write (23,'(6g20.10)') Rxx(I),Ryy(I),Rzz(I),
     &        Vxx(I),Vyy(I),Vzz(I)
      Enddo
 
      Return
      End
