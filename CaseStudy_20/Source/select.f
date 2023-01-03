      Subroutine Select(W,Ichoi,Num)
      Implicit None

      Include 'system.inc'

Ccccccccccccccccccccccccccccccccccccccccccccccc
C     Select A Trial Segment I Acording To    C
C     P(I) = Exp(A(I))                        C
C                                             C
C     Num  = Number Of Trial Segments         C
C                                             C
C     Special Care Is Taken To Avoid          C
C     Numerical Instabilities                 C
Ccccccccccccccccccccccccccccccccccccccccccccccc
      
      Integer          Ichoi,Num
      Double Precision Sumw,Cumw,Ws,Ran_Uniform,W(Kmax)

      Sumw  = 0.0d0
      Cumw  = W(1)

      Do Ichoi=1,Num
         Cumw = Max(Cumw,W(Ichoi))
      Enddo

      Do Ichoi=1,Num
         W(Ichoi) = W(Ichoi) - Cumw
         W(Ichoi) = Dexp(W(Ichoi))
         Sumw     = Sumw + W(Ichoi)
      Enddo
 
      Ws    = Ran_Uniform()*Sumw
      Cumw  = W(1)
      Ichoi = 1

      If(Num.Ne.1) Then
         Do While(Cumw.Lt.Ws)
            Ichoi = Ichoi + 1
            Cumw  = Cumw + W(Ichoi)
         Enddo
      Endif

      Return
      End
