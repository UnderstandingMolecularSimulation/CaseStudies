      Subroutine Sample_Avhb(Switch)
      Implicit None

      Include 'system.inc'
      
      Integer          Switch,J,Mymodel
      Logical          Lready
      Double Precision Gg1(Maxtraject),Gg2
      Character*5      Short
      Character*13     Name1

      Save Gg1,Gg2,Mymodel
      Data Mymodel/0/

      If(Switch.Eq.1) Then

         Gg2 = 0.0d0

         Do J=1,Maxtraject
            Gg1(J) = 0.0d0
         Enddo
         
      Elseif(Switch.Eq.2) Then
      
         Gg2 = Gg2 + 1.0d0
         
         Lready = .False.

         Do J=1,Nslice
            If(Lb_Old(J)) Then
               Gg1(J) = Gg1(J) + 1.0d0
               Lready = .True.
            Endif
         Enddo
         
         If(.Not.La_Old(1)) Then
            Write(6,*) 'Error ! Initial Path Is Not In A !!!'
            Write(6,*) 'Error In Sample_Avhb !!!'
            Call Exitt(2)
         Elseif(.Not.Lready) Then
            Write(6,*) 'Error ! Path Is Never In B !!!'
            Write(6,*) 'Error In Sample_Avhb !!!'
            Call Exitt(2)
         Endif
      
      Elseif(Switch.eq.3) Then

         Mymodel = Mymodel + 1

         Call Convert_Num(Mymodel,Short)

         Name1 = 'avhb'//short//'.dat'
                  
         Open(32,File=Name1)

         Do J=1,Nslice
            If(Gg2.Gt.0.5d0) Then
               Write(32,'(I7,E20.10)') J,Gg1(J)/Gg2
            Else
               Write(32,'(I7,A)') J,'   0.0d0'
            Endif
         Enddo
         
         Close(32)
      Endif

      Return
      End
