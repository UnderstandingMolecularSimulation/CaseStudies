      Subroutine Shake
      Implicit None
 
      Include 'system.inc'
 
      Integer I,Ncons,Nfreeze,Icycle,K
      Logical Lfreeze(Maxpart)
 
      Double Precision Esig,Dxx(Maxpart),Dyy(Maxpart),
     &     Dzz(Maxpart),Dxt,Dyt,Dzt,Bond2,Ibond,Distance,Gamma
 
      Ncons = Npart/2
      Bond2 = Bondl*Bondl
      Ibond = 1.0d0/Bondl
 
      Do I = 1,Npart - 1,2
         Dxx(I) = Rxf(I) - Rxf(I + 1)
         Dyy(I) = Ryf(I) - Ryf(I + 1)
         Dzz(I) = Rzf(I) - Rzf(I + 1)
 
         Lfreeze(I) = .False.
      Enddo
 
      Nfreeze = 0
 
      Do Icycle = 1,1000
 
         Do I = 1,Npart - 1,2
 
            If (.Not.Lfreeze(I)) Then
 
               K = I + 1
 
               Esig = 0.0d0
 
               Dxt = Rxx(I) - Rxx(K)
               Dyt = Ryy(I) - Ryy(K)
               Dzt = Rzz(I) - Rzz(K)
 
               Distance = Bond2 - 
     &              (Dxt*Dxt + Dyt*Dyt + Dzt*Dzt)
 
               Esig = Dabs(Distance)*Ibond
 
               If (Esig.Lt.1.0d-8) Then
                  Lfreeze(I) = .True.
                  Nfreeze = Nfreeze + 1
 
                  If (Nfreeze.Eq.Ncons) Return
               Endif
 
               Gamma = 0.25d0*Distance/(Dxt*Dxx(I) + 
     &              Dyt*Dyy(I) + Dzt*Dzz(I))
 
               Rxx(I) = Rxx(I) + Gamma*Dxx(I)
               Ryy(I) = Ryy(I) + Gamma*Dyy(I)
               Rzz(I) = Rzz(I) + Gamma*Dzz(I)
 
               Rxx(K) = Rxx(K) - Gamma*Dxx(I)
               Ryy(K) = Ryy(K) - Gamma*Dyy(I)
               Rzz(K) = Rzz(K) - Gamma*Dzz(I)
 
            Endif
         Enddo
      Enddo
 
      Write (6,*) 'Shake Algorithm Did Not Converge In 1000 Steps !!!'
      Stop
      End
