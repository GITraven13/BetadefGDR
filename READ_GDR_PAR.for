      SUBROUTINE READ_GDR_PAR(iaz,iaa,areact,arefer,keyslomlo)
C     *****************************************************************
C     *       FORTRAN77 code for reading the GDR parameters tables    *
C     *       gdr&errors_slo_exp-1GS.dat gdr&errors_mlo_exp-1GS.dat   *
C     *       ---------------------------------                       *
C     *   The input parameters appearing as arguments:                *
C     *   --------------------------------------------                *
C     *                                                               *
C     *   iaz = atomic number;                                        *
C     *                                                               *
C     *   iaa = mass number; 0 indicates a natural isotopic           *
c     *         composition;                                          *
C     *                                                               *
C     *   areact  = type of experimental data used in fitting;        *
C     *                                                               *
C     *   arefer =short references on the experimental data used      *
C     *           in the fitting;                                     *
C     *                                                               *
C     *   keyslomlo =1  ! 1-SLO GDR parameters, other value- MLO GDR  *
C     *   parameters                                                  *
C     *       ---------------------------------                       *
C     *       The output:                                             *
C     *COMMON /GDRPAR/E1,G1,S1,E2,G2,S2,DE1,DG1,DS1,DE2,DG2,DS2,KEYslo*
C     * E1  = energy of the first component of the GDR, MeV           *
C     *          (the first line of the entry);                       *
C     * DE1 = uncertainty (one-sigma standard deviation, MeV) of the  *
C     *        energy in the nearest                                  * 
C     *             upper line (the second line of the entry);        *
C     * G1 = width of the first component of the GDR, MeV             *
C     *       (the first line of the entry);                          *
C     * DG1 = uncertainty (one-sigma standard deviation, MeV)         *
C     *        of the width in the nearest                            *
C     *             upper line (the second line of the entry);        *
C     * S1 = strength of the first component of the GDR in            *
C     *        units of the Thomas-Reiche-Kuhn(TRK) sum rule CS_TRK   *
C     *        (the first line of the entry); the GDR strength Sr1 is *
C     *        related to the peak value CSr1 of the first            *
C     *        cross-section component CSr1=2*CS_TRK*Sr1/Wr1/PI,      *
C     *        CS_TRK= 60 NZ/A = 15 A (1-I^2)[mb], I=(N-Z)/A;         *
C     *        (the values of CS_TRK for Zr-90 and Pb-208 are         *
C     *        used for  Zr-0 and Pb-0);                              *
C     * DS1 = uncertainty (one-sigma standard deviation) of the       *
C     *        strength in the nearest upper line (the second line    *
C     *        of the entry);                                         *
C     * E2 = energy of the second component of the GDR, MeV           *
C     *        (the first line of the entry);                         *
C     * DE2 = uncertainty (one-sigma standard deviation, MeV) of      *
C     *        the energy in the nearest  upper line (the second      *
C     *        line of the entry);                                    *
C     * G2 = width of the second component of the GDR, MeV (the       *
C     *        first line of the entry);                              *
C     * DG2 = uncertainty (one-sigma standard deviation, MeV) of the  *
C     *        width in the nearest upper line (the second line of    *
C     *        the entry);                                            *
C     * S2 = strength of the second component of the GDR in units     *
C     *        of the TRK sum rule (the first line of the entry);     *
C     *        the GDR strength Sr2 is related to the peak value CSr2 *
C     *        of the second cross-section component                  *
C     *        CSr2=2*CS_TRK*Sr2/Wr2/PI; (the values of CS_TRK for    *
C     *        Zr-90 and Pb-208 are used for  Zr-0 and Pb-0);         *
C     * DS2 = uncertainty (one-sigma standard deviation) of the       *
C     *        strength in the nearest upper line (the second line    *
C     *        of the entry);                                         *
C     * KEYdata = 1 or 0- if there is or no data in table             *
C     *   --------------------------------------------                *
C     *****************************************************************
      DOUBLE PRECISION E1,G1,S1,E2,G2,S2,DE1,DG1,DS1,DE2,DG2,DS2
     &  ,EgR1,GamR1,SigR1,EgR2,GamR2,SigR2,SigF,Emin,Emax
     &  ,dEgR1,dGamR1,dSigR1,dEgR2,dGamR2,dSigR2,dSigF,Ea,Eb
      INTEGER keyslomlo,i,iaz,iaa,iz,ia,KEYdata,KEYdataFirst
     &  ,KEYdataFirsttemp,KeyID
      CHARACTER El*2,react*3,refer*7,areact*3,arefer*7
     &,acheck1*1,acheck2*1,acheck3*2,acheck4*2
      COMMON /GDRPAR/E1,G1,S1,E2,G2,S2,DE1,DG1,DS1,DE2,DG2,DS2,Ea,Eb
     &   ,KEYdata
      COMMON /CHKFRST/KEYdataFirst
c      keyslomlo=1  ! 1-SLO GDR parameters, other value- MLO GDR parameters
      IF (keyslomlo.EQ.0) THEN
        RETURN
      ELSEIF (keyslomlo.EQ.1) THEN
        OPEN(66,FILE='.\PARAMETERS\gdr&errors_slo.dat',STATUS='old')
c        OPEN(66,FILE='.\PARAMETERS\gdr&errors_slo_2017_07_03.dat',
c     &  STATUS='old')
      ELSEIF (keyslomlo.EQ.2) THEN
        OPEN(66,FILE='.\PARAMETERS\gdr&errors_mlo.dat',STATUS='old')
c        OPEN(66,FILE='.\PARAMETERS\gdr&errors_mlo_2017_07_03.dat',
c     &  STATUS='old')
      ELSE
        RETURN
      ENDIF
    3 FORMAT(2I4,1X,A2,1X,A3,2(2F8.2,F8.3),1X,F8.3,2X,
     &   F4.1,3X,F4.1,2X,A7)
    4 FORMAT(15x,2(2F8.2,F8.3),1X,F8.3)
    5 FORMAT(//////////)
      REWIND 66
      READ(66,5)
      E1=0.D0
      G1=0.D0
      S1=0.D0
      E2=0.D0
      G2=0.D0
      S2=0.D0
      DE1=0.D0
      DG1=0.D0
      DS1=0.D0
      DE2=0.D0
      DG2=0.D0
      DS2=0.D0   
      KEYdata=0
      KEYdataFirst=0
      KEYdataFirsttemp=1   
      KeyID=1 ! chaeck the ID number
      DO i = 1,1000
         READ(66, 3, END = 2, ERR = 2)iz,ia,El,react,EgR1,
     &     GamR1,SigR1,EgR2,GamR2,SigR2,SigF,Emin,Emax,refer
         READ(66, 4, END = 2, ERR = 2)dEgR1,
     &     dGamR1,dSigR1,dEgR2,dGamR2,dSigR2,dSigF
c         IF ((ABS(iaa-ia).LT.0.1).AND.    ! read only first appearing data in GDR par. table
c     &      (ABS(iaz-iz).LT.0.1)) THEN
         IF ((ABS(iaa-ia).LT.0.1).AND.    ! read defined data in GDR par. table
     &      (ABS(iaz-iz).LT.0.1)) THEN
c     &     .AND.(areact.EQ.react)
           IF(arefer.EQ.refer)  THEN
           IF (KeyID.EQ.1) THEN
c --------- for check the Id identifications BEGIN
            READ(react,'(1x,A1)')acheck1
            K1=INDEX(acheck1,'b')
            K2=INDEX(acheck1,'a')
            K3=INDEX(acheck1,' ')
            READ(areact,'(1x,A1)')acheck2  
            K4=INDEX(acheck2,'b')
            K5=INDEX(acheck2,'a')
            K6=INDEX(acheck2,' ')
            IF (((K1==1).OR.(K2==1).OR.(K3==1)).AND.
     &       ((K4==1).OR.(K5==1).OR.(K6==1))) THEN 
               READ(react,'(A1)')acheck1
               READ(areact,'(A1)')acheck2  
               IF (INDEX(acheck1,acheck2)==1) THEN 
                  GOTO 112
               ELSE
                  GOTO 113
               ENDIF
            ELSE 
             IF ((K4==1).OR.(K5==1).OR.(K6==1)) THEN 
              ELSE
               READ(react,'(A2)')acheck3
               READ(areact,'(A2)')acheck4
               IF (INDEX(acheck3,acheck4)==1) THEN 
                  GOTO 112
               ELSE
                  GOTO 113
               ENDIF
             ENDIF
            ENDIF
            ENDIF
c --------- for check the Id identifications END
  112       CONTINUE       
            E1=EgR1
            G1=GamR1
            S1=SigR1
            E2=EgR2
            G2=GamR2
            S2=SigR2
            DE1=dEgR1
            DG1=dGamR1
            DS1=dSigR1
            DE2=dEgR2
            DG2=dGamR2
            DS2=dSigR2
            areact=react
            arefer=refer
            Ea=Emin
            Eb=Emax
            KEYdata=1
            KEYdataFirst=KEYdataFirsttemp
            GOTO 2    ! first one value used
           ELSE
  113        CONTINUE  
             KEYdataFirsttemp=0
           ENDIF
         ENDIF
         
      ENDDO
  2   CONTINUE
      CLOSE(66)
C -------------------------------------------------
      RETURN
      END


      SUBROUTINE READ_GDR_PAR_SYS(iaz,iaa,beta,keyslomlo)
C     *****************************************************************
C     *       FORTRAN77 code for calculation of the GDR parameters if *
C     *       there are no datain GDR table                           *
C     *       gdr&errors_slo_exp-1GS.dat gdr&errors_mlo_exp-1GS.dat   *

C     *       ---------------------------------                       *
C     *   The input parameters appearing as arguments:                *
C     *   --------------------------------------------                *
C     *   iaz = atomic number;                                        *
C     *                                                               *
C     *   iaa = mass number; 0 indicates a natural isotopic           *
c     *         composition;                                          *
C     *   keyslomlo =1  ! 1-SLO GDR parameters, other value- MLO GDR  *
C     *   parameters                                                  *
C     *   --------------------------------------------                *
C     *****************************************************************
C     *       ---------------------------------                       *
C     *       The output:                                             *
C     *       COMMON /GDRSYS/EG0,GW0,CS0                              *
      
      DOUBLE PRECISION  EAlphapar1,DEAlphapar1,EAlphapar2,DEAlphapar2,
     &   E0,G0,S0,A,Z,PI,GAlphapar1,DGAlphapar1,SAlphapar1,beta,alpha2,
     &   DSAlphapar1,E1,G1,S1,E2,G2,S2,DE1,DG1,DS1,DE2,DG2,DS2,Ea,Eb
     &  ,gw0,alambda,a0,b0,csa,csb,cs0,e1h,e2h,cs1h,cs2h,gw1h,gw2h
     &  ,Ealim,Eblim
      INTEGER keyslomlo,KEYcorrean,iaa,iaz,KEYdata,NG
      COMMON /GDRSYS/E0,G0,S0
      COMMON /GDRPAR/E1,G1,S1,E2,G2,S2,DE1,DG1,DS1,DE2,DG2,DS2,
     &  Ealim,Eblim
     &   ,KEYdata 
      DATA PI/3.14159265359D0/
C        Systematics of GDR parameters in approximation
C        of spherical nucleus
c         E0 = 31.2/aa3 + 20.6/SQRT(aa3)
c         G0 = 0.026*E0**1.91
c         S0 = 1.2D0
      KEYcorrean=1  ! 1 - systematic from Journal of the Korean Physical Society, Vol. 59, No. 2, August 2011, p.1514
      A=1.D0*iaa ! confert to double precision
      Z=1.D0*iaz ! confert to double precision
      EAlphapar1=31.2D0
      EAlphapar2=20.6D0
      GAlphapar1=0.026D0
      DEAlphapar1=0.D0
      DEAlphapar2=0.D0
      DGAlphapar1=0.D0
      SAlphapar1=1.2D0
      DSAlphapar1=0.0D0
c      RETURN
      IF (keyslomlo.EQ.0) THEN
      RETURN
      ELSEIF (keyslomlo.EQ.1) THEN    ! SLO
        EAlphapar1=0.27469D+02
        DEAlphapar1=0.87148D-02
        EAlphapar2=0.22063D+02
        DEAlphapar2=0.38861D-02
        GAlphapar1= 0.26914D-01
        DGAlphapar1= 0.36372D-04
        SAlphapar1=0.12224D+01
        DSAlphapar1=0.18948D-02
      ELSEIF (keyslomlo.EQ.2) THEN    ! SMLO
         EAlphapar1=0.28690D+02
         DEAlphapar1=0.95275D-02
         EAlphapar2=0.21731D+02
         DEAlphapar2=0.42253D-02
         GAlphapar1=0.27691D-01
         DGAlphapar1=0.39534D-04
         SAlphapar1=0.12669D+01
         DSAlphapar1=0.20658D-02
      ELSE
      RETURN
      ENDIF
      E0= EAlphapar1/A**(1.D0/3)+EAlphapar2/A**(1.D0/6)
      G0=GAlphapar1*E0**(1.91D0)
      S0= SAlphapar1
      IF (KEYcorrean.EQ.1) THEN
       E0= 156.47D0*SQRT(1-((A-2*Z)/A)**2)/A**(1.D0/3)
     &  /SQRT(1.D0+15.13D0/A**(1.D0/3))
       GAlphapar1=2.509D-2
       G0=GAlphapar1*E0**(1.91D0)
       S0= SAlphapar1       
      ENDIF
      
c------For deformed nuclei begin      
c       E2=E0*(1+0.5D0*Sqrt(5.D0/(4*PI))*beta)
c       E1=E0*(1-Sqrt(5.D0/(4*PI))*beta)
c       G2=G0
c       G1=G2
c       IF (beta.GT.0) THEN
c         S1=S0/3
c         S2=2*S0/3
c       ELSE
c         S1=2*S0/3
c         S2=S0/3       
c       ENDIF
       
       
c------------------
C    *****************************************************
C    * Global systematics of the GDR parameters with     *
C    * the the use of classical sum rule with correction *
C    *****************************************************
c      alpha2=5.D0/(4*PI)*beta  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      alpha2=Sqrt(5.D0/(4*PI))*beta      
      gw0=G0
      cs0 =S0*120.D0*(A-Z)*Z/(A*PI*gw0)
      e0=E0
      IF(ABS(alpha2).GT.0.001)THEN
         ngh = 2
         alambda = (1. + 0.6*alpha2**2 + 2.*alpha2**3/35.)**0.3333333
         a0 = (1. + alpha2)/alambda
         b0 = (1. - 0.5*alpha2)/alambda
         eb = e0*(1. - 1.51E-02*(a0 + b0)*(a0 - b0))/b0
         ea = eb/(0.911*a0/b0 + 0.089)
         csa = cs0/3.
         csb = cs0*2./3.
         e1h = ea
         e2h = eb
         cs1h = csa
         cs2h = csb
         IF(ea.GT.eb)THEN
            e1h = eb
            e2h = ea
            cs1h = csb
            cs2h = csa
         ENDIF
         gw1h = GAlphapar1*e1h**1.91
         gw2h = GAlphapar1*e2h**1.91
         
         cs1h = cs1h/(120.D0*(A-Z)*Z/(A*PI*gw1h))   ! convert gigma1 to S1
         cs2h = cs2h/(120.D0*(A-Z)*Z/(A*PI*gw2h))   ! convert gigma2 to S2     
      ELSE
         ngh = 1
         e1h = e0
         gw1h = gw0
c         cs1h = cs0
         cs1h=cs0/(120.D0*(A-Z)*Z/(A*PI*gw1h))    ! convert gigma0 to S0
         e2h = 0
         gw2h = 0
         cs2h = 0
      ENDIF
      keygdrdata=0
      IF(keygdrdata.EQ.0) THEN
            NG = ngh
            E1 = e1h
            S1 = cs1h
c            S1=cs1/(120.D0*(A-Z)*Z/(A*PI*gw1))
            G1 = gw1h
            E2 = e2h
            S2 = cs2h
c            S2=cs2/(120.D0*(A-Z)*Z/(A*PI*gw2))
            G2 = gw2h
      ENDIF
c------For deformed nuclei end         
      RETURN
      END