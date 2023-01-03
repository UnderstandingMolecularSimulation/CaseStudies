      Subroutine Writepath
      Implicit None

      Include 'system.inc'
      
Ccccccccccccccccccccccccccc
C     Write Path To Disk  C
Ccccccccccccccccccccccccccc

      Integer          Mymodel,J
      Logical          Lready
      Double Precision Myorder(Maxtraject),Myorder2
      Character*1      Nm
      Character*5      Short
      Character*15     Name1

      Save Mymodel
      Data Mymodel/0/

      Mymodel = Mymodel + 1

      Lready = .False.

      If(.Not.La_Old(1)) Then
         Write(6,*) 'Error In Writepath !!'
         Write(6,*) 'Initial Point Not In A !!!'
         Call Exitt(2)
      Endif

      Call Convert_Num(Mymodel,Short)
      
      Name1 = 'mypath'//short//'.res'
            
      Call Order(Myorder)

      Open(32,File=Name1)
      
      Do J=1,Nslice

         If(La_Old(J)) Then
            Nm     = 'A'
         Elseif(Lb_Old(J)) Then
            Nm     = 'B'
            Lready = .True.
         Else
            Nm     = 'X'
         Endif

         Write(32,'(4(1x,E25.15),1x,A,I5,F9.3)') 
     &        Xxold(J),
     &        Yyold(J),
     &        Vxold(J),
     &        Vyold(J),
     &        Nm,J,
     &        Myorder(J)

      Enddo
      
      Close(32)
      
      If(Lumbrella) Then
            
         Rxx = Xxold(Nslice)
         Ryy = Yyold(Nslice)
                                          
         Vxx = Vxold(Nslice)
         Vyy = Vyold(Nslice)
                     
         Call In_Slice(Lready,Myorder2)

         If(.Not.Lready) Then
            Write(6,*) 'Path Is Not In Slice !!!'
            Write(6,*) 'Error Writepath (Umbrella Samp) !!!'
            Write(6,*) 'Order : ',Myorder2
            Write(6,*) 'Lmin  : ',Lmin
            Write(6,*) 'Lmax  : ',Lmax
            Call Exitt(2)
         Endif

      Else

         If(.Not.Lready) Then
            Write(6,*) 'Error In Writepath !! Never In B !!!'
            Call Exitt(2)
         Endif 
      Endif
      
      Return
      End
