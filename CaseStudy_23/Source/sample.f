      Subroutine Sample(Switch,Ttt)
      Implicit None

      Include 'system.inc'

      Integer          Maxx,I,Ttt,Switch
      Parameter(Maxx=50000)

      Double Precision Kt(Maxx),Ks(Maxx)
      Save Kt,Ks

      If (Switch.Eq.1) Then

C     Initialize Everything

         Do I=1,Maxx
            Kt(I) = 0.0d0
            Ks(I) = 0.0d0
         Enddo

      Elseif(Switch.Eq.2) Then

C     Sample Results

         If(Ttt.Le.Maxx) Then

            If(Xpos.Gt.Qstar) Then
               Kt(Ttt) = Kt(Ttt) + Thetan
            Endif

            Ks(Ttt) = Ks(Ttt) + 1.0d0
         Endif

      Else

C     Write Results

         Do I=1,Maxx
            If(I.Le.Nstep) Then

               If(Ks(I).Gt.0.5d0) Write(27,*) Dble(I-1),Kt(I)/Ks(I)

            Endif
         Enddo

      Endif

      Return
      End
