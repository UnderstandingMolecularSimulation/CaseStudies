      Subroutine Readpath
      Implicit None

      Include 'system.inc'
      
Cccccccccccccccccccccccccccc
C     Read Path From Disk  C
Cccccccccccccccccccccccccccc

      Integer          J
      Logical          In_A,In_B,Lready
      Double Precision Order,Upot,Fx,Fy

      Open(32,File='pathold.res')
        
      Do J=1,Nslice
      
         Read(32,*) Xxold(J),Yyold(J),
     &        Vxold(J),Vyold(J)
     
         Xxtra(J) = Xxold(J)
         Yytra(J) = Yyold(J)
                                 
         Vxtra(J) = Vxold(J)
         Vytra(J) = Vyold(J)
                  
         Rxx = Xxold(J)
         Ryy = Yyold(J)
                            
         Vxx = Vxold(J)
         Vyy = Vyold(J)

         Call Force(Rxx,Ryy,Upot,Fx,Fy)
      
         Eeold(J)  = 0.5d0*(Vxx**2 + Vyy**2) + Upot
         La_Old(J) = In_A()
         Lb_Old(J) = In_B()
      Enddo

      Close(32)

Cccccccccccccccccccccccccc
C     Check Some Things  C
Cccccccccccccccccccccccccc

      If(.Not.La_Old(1)) Then
         Write(6,*) 'Initial Path Does Not Start In A !!!'
         Write(6,*) 'Error Readpath'
         Call Exitt(2)
      Endif

      If(Lumbrella) Then
          
         Rxx = Xxold(Nslice)
         Ryy = Yyold(Nslice)
                                    
         Vxx = Vxold(Nslice)
         Vyy = Vyold(Nslice)
               
         Call In_Slice(Lready,Order)

         If(.Not.Lready) Then
            Write(6,*) 'Path Is Not In Slice !!!'
            Write(6,*) 'Error Readpath For Umbrella Sampling !!!'
            Write(6,*) 'Order : ',Order
            Write(6,*) 'Lmin  : ',Lmin
            Write(6,*) 'Lmax  : ',Lmax
            Call Exitt(2)
         Endif

      Else

         Lready = .False.

         Do J=1,Nslice
            If(Lb_Old(J)) Lready = .True.
         Enddo

         If(.Not.Lready) Then
            Write(6,*) 'Path Is Never In B !!!'
            Write(6,*) 'Error Readpath For Normal Pathsampling !!!'
            Call Exitt(2)
         Endif
      Endif

      Return
      End
