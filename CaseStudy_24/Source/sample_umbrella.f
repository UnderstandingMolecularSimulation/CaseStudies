      Subroutine Sample_Umbrella(Switch)
      Implicit None

Cccccccccccccccccccccccccccccccccccccccccc
C     Umbrella Sampling Of P(Lambda,T)   C
C     Calculate The Probability That A   C
C     Molecule Is At A Given Position    C
C     Provided That It Is In The Slice   C
Cccccccccccccccccccccccccccccccccccccccccc

      Include 'system.inc'
            
      Logical          Lslice
      Integer          Switch,I,Mymodel
      Double Precision Gg1(-Maxumbrel:Maxumbrel),
     &                 Order,Gg2,Dddx
      Character*5      Short
      Character*17     Name1

      Save Gg1,Gg2,Dddx,Mymodel
      Data Mymodel/0/

      If(Switch.Eq.1) Then

         Do I=-Maxumbrel,Maxumbrel
            Gg1(I) = 0.0d0
         Enddo
         
         Gg2  = 0.0d0
         Dddx = 1.0d0/Maxdslice
         
      Elseif(Switch.Eq.2) Then

         Rxx = Xxold(Nslice)
         Ryy = Yyold(Nslice)
                                 
         Vxx = Vxold(Nslice)
         Vyy = Vyold(Nslice)
         
         Call In_Slice(Lslice,Order)

         Gg2 = Gg2 + 1.0d0

         If(Lslice) Then
            I = Idint(Dddx*Order)

            If(I.Lt.-Maxumbrel.Or.I.Gt.Maxumbrel) Then
               Write(6,*) 'Order Isoutof Range !!!'
               Write(6,*) 'Order : ',Order
               Write(6,*) 'I     : ',I
               Call Exitt(2)
            Else
               If(I.Eq.0) Then
                  Gg1(I) = Gg1(I) + 0.5d0
               Else
                  Gg1(I) = Gg1(I) + 1.0d0
               Endif
            Endif
         Else
            Write(6,*) 'Path Is Not In Slice  !!!'
            Write(6,*) 'Error Sample_Umbrella !!!'
            Call Exitt(2)
         Endif
         
      Else

         Mymodel = Mymodel + 1

         Call Convert_Num(Mymodel,Short)

         Name1 = 'umbrella'//short//'.dat'

         Open(32,File=Name1)

         Do I=-Maxumbrel,Maxumbrel
            If(Gg1(I).Gt.0.5d0) Then
               Write(32,'(I7,E20.10)') I,Gg1(I)/Gg2
            Endif
         Enddo

         Close(32)
      
      Endif

      Return
      End
