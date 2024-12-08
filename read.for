      PROGRAM reaExfor;
      IMPLICIT NONE
      DOUBLE PRECISION E1,G1,S1,E2,G2,S2,DE1,DG1,DS1,DE2,DG2,DS2
     &  ,EgR1,GamR1,SigR1,EgR2,GamR2,SigR2,SigF,Emin,Emax
     &  ,dEgR1,dGamR1,dSigR1,dEgR2,dGamR2,dSigR2,dSigF,Ea,Eb
     &  ,Eaa,Ebb,betaSLO,betaSMLO,PI,aaa,Zdeflib,Adeflib,Betadeflib
     &  ,betadef,betapri,Zpri,Apri,BetapriD,betayush
     &  ,BetayushD,DEaa,DEbb,y,dfy,fact1,fact2,DbetaSLO,DbetaSMLO
     &  ,E1s,G1s,S1s,E2s,G2s,S2s,DE1s,DG1s,DS1s,DE2s
     &  ,DG2s,DS2s,Eas,Ebs,beta,BetaramanD,DBetaramanD
     &  ,betaraman,dbetaraman,betaSMLOn,betaSLOn,DbetaSLOn,DbetaSMLOn
      INTEGER keyslomlo,i,iaz,iaa,iz,ia,KEYdata,KEYdataFirst
     &  ,KEYdataFirsttemp,KeyID,ik,ikendpri,ikenddeflib,ikendyush
     &  ,iaaa,iZyush,iAyush,iZraman,iAraman,ikendraman,izt,iat
      CHARACTER El*2,react*3,refer*7,areact*3,arefer*7
     &,acheck1*1,acheck2*1,acheck3*2,acheck4*2
      DIMENSION Zdeflib(10000),Adeflib(10000)
     &     ,Betadeflib(10000),Zpri(500),Apri(500),
     &     BetapriD(500),iZyush(500),iAyush(500),
     &     BetayushD(500),iZraman(500),iAraman(500),
     &     BetaramanD(500),DBetaramanD(500)
      COMMON /GDRPAR/E1s,G1s,S1s,E2s,G2s,S2s,DE1s,DG1s,DS1s,DE2s
     &   ,DG2s,DS2s,Eas,Ebs
     &   ,KEYdata
      COMMON /CHKFRST/KEYdataFirst
      DATA PI/3.14159265359D0/

    3 FORMAT(2I4,1X,A2,1X,A3,2(2F8.2,F8.3),1X,F8.3,2X,
     &   F4.1,3X,F4.1,2X,A7)
    4 FORMAT(15x,2(2F8.2,F8.3),1X,F8.3)
    5 FORMAT(//////////)

 

c  -------------begin Deformation reading--------------------------
       betadef=0.D0
       OPEN(93, FILE = '.\PARAMETERS\DEFLIB.DAT', STATUS = 'old')
       REWIND(93)
       READ(93,99005)
99005  FORMAT(////)
       DO ik = 1,10000
         READ(93,825,END=53,ERR=53)Zdeflib(ik),Adeflib(ik),aaa,
     &     Betadeflib(ik)
	   ikenddeflib=ik
       ENDDO
  825  FORMAT (2F4.0,2F7.3)
   53  CONTINUE
       CLOSE(93)



       betapri=0.D0
       OPEN(93,FILE='.\PARAMETERS\pritychenko-all.txt',STATUS = 'old')
       REWIND(93)
       READ(93,99006)
99006  FORMAT()
       DO ik = 1,500
         READ(93,*,END=54,ERR=54)Apri(ik),Zpri(ik),
     &     BetapriD(ik)
	   ikendpri=ik
       ENDDO
   54  CONTINUE
       CLOSE(93)



       betayush=0.D0
       OPEN(93,FILE='.\PARAMETERS\yush_All_cut.txt',STATUS = 'old')
       REWIND(93)
       READ(93,99007)
99007  FORMAT()
       DO ik = 1,500
         READ(93,*,END=55,ERR=55)iZyush(ik),iAyush(ik),iaaa,
     &     BetayushD(ik)
	   ikendyush=ik
       ENDDO
c  825  FORMAT (2F4.0,2F7.3)
   55  CONTINUE
       CLOSE(93)


       betaraman=0.D0
       OPEN(93,FILE='.\PARAMETERS\gs-deformations-exp.dat',
     & STATUS = 'old')
       REWIND(93)
       READ(93,99008)
99008  FORMAT(///)
       DO ik = 1,500
         READ(93,'(2i4,4x,2f8.4)',END=56,ERR=56)iZraman(ik),iAraman(ik),
     &     BetaramanD(ik),DBetaramanD(ik)
	   ikendraman=ik
       ENDDO
c  825  FORMAT (2F4.0,2F7.3)
   56  CONTINUE
       CLOSE(93)

      
      OPEN(77,FILE='.\PARAMETERS\betapar.dat')  
      WRITE(77,'(2A4,1X,A2,1X,A3,13A10)')'#  Z','  A','El ','reac',
     &    'betaSLO','DbetaSLO','betaSMLO','DbetaSMLO','betadef',
     &    'betapri','betayush','betaRam','DbetaRam'
     &   ,'betaSLOn','DbSLOn','betaSMLOn','DbSMLOn'

      OPEN(78,FILE='.\PARAMETERS\betapar_first.dat')  
      WRITE(78,'(2A4,1X,A2,1X,A3,13A10)')'#  Z','  A','El ','reac',
     &    'betaSLO','DbetaSLO','betaSMLO','DbetaSMLO','betadef',
     &    'betapri','betayush','betaRam','DbetaRam'
     &   ,'betaSLOn','DbSLOn','betaSMLOn','DbSMLOn'

      OPEN(79,FILE='.\PARAMETERS\betapar_first_diff.dat')  
      WRITE(79,'(2A4,1X,A2,1X,A3,13A10)')'#  Z','  A','El ','reac',
     &    'betaSLO','DbetaSLO','betaSMLO','DbetaSMLO','betadef',
     &    'betapri','betayush','betaRam','DbetaRam'
     &   ,'betaSLOn','DbSLOn','betaSMLOn','DbSMLOn'

      OPEN(88,FILE='.\PARAMETERS\gdrSLOapartest.dat')  
      WRITE(88,'(2A4,1X,A2,1X,A3,8A10)')'#  Z','  A','El ','reac',
     &    'E1','DE1','E2','DE2','E1def',
     &    'DE1def','E2def','DE2def'

      OPEN(89,FILE='.\PARAMETERS\gdrSMLOapartest.dat')  
      WRITE(89,'(2A4,1X,A2,1X,A3,8A10)')'#  Z','  A','El ','reac',
     &    'E1','DE1','E2','DE2','E1def',
     &    'DE1def','E2def','DE2def'
        
c      keyslomlo=1  ! 1-SLO GDR parameters, other value- MLO GDR parameters
c      IF (keyslomlo.EQ.0) THEN
c        RETURN
c      ELSEIF (keyslomlo.EQ.1) THEN
        OPEN(66,FILE='.\PARAMETERS\gdr&errors_slo.dat',STATUS='old')
c      ELSEIF (keyslomlo.EQ.2) THEN
        OPEN(67,FILE='.\PARAMETERS\gdr&errors_mlo.dat',STATUS='old')
c      ELSE
c        RETURN
c      ENDIF
      REWIND 66
      REWIND 67
      READ(66,5)
      READ(67,5)
      DO i = 1,1000
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
         READ(66, 3, END = 2, ERR = 2)iz,ia,El,react,EgR1,
     &     GamR1,SigR1,EgR2,GamR2,SigR2,SigF,Emin,Emax,refer
         READ(66, 4, END = 2, ERR = 2)dEgR1,
     &     dGamR1,dSigR1,dEgR2,dGamR2,dSigR2,dSigF
     
      IF (ia.LT.1)  THEN
c	      keynatural=1
          IF (iz.EQ.10) iaa=20    ! Ne-0 90.48
          IF (iz.EQ.14) iaa=28    ! Si-0 92.23
          IF (iz.EQ.16) iaa=32    ! S-0  95.02
          IF (iz.EQ.18) iaa=40    ! Ar-0 99.60
          IF (iz.EQ.19) iaa=39    ! K-0  93.25
          IF (iz.EQ.20) iaa=40    ! Ca-0 96.94
          IF (iz.EQ.26) iaa=56    ! Fe-0 91.75
          IF (iz.EQ.8)  iaa=16     ! O-0  99.76
          IF (iz.EQ.6)  iaa=12     ! C-0  98.89
          IF (iz.EQ.92) iaa=238   ! U-0  99.27
          IF (iz.EQ.3)  iaa=7      ! Li-0 92.41
          IF (iz.EQ.28) iaa=58    ! Ni-0 68.08
          IF (iz.EQ.40) iaa=90    ! Zr-0 51.45
          IF (iz.EQ.82) iaa=208   ! Pb-0 52.40
          IF (iz.EQ.12) iaa=24    ! Mg-0 78.99
          IF (iz.EQ.56) iaa=138   ! Ba-0 71.698
          IF (iz.EQ.37) iaa=85    ! Rb-0 72.17
          IF (iz.EQ.38) iaa=88    ! Sr-0 82.58
          IF (iz.EQ.47) iaa=107   ! Ag-0 51.839
          IF (iz.EQ.51) iaa=121   ! Sb-0 57.21
          IF (iz.EQ.58) iaa=140   ! Ce-0 88.45
          IF (iz.EQ.60) iaa=142   ! Nd-0 27.20
          IF (iz.EQ.52) iaa=130   ! Te-0 34.08
          IF (iz.EQ.78) iaa=195   ! Pt-0 33.832
          IF (iz.EQ.75) iaa=187   ! Re-0 62.6
          IF (iz.EQ.74) iaa=184   ! W-0 30.64
          IF (iz.EQ.68) iaa=166   ! Er-0 33.5
          IF (iz.EQ.29) iaa=63    ! Cu-0 69.17
          IF (iz.EQ.46) iaa=106   ! Pd-0 27.33
          IF (iz.EQ.48) iaa=114   ! Cd-0 28.73
          IF (iz.EQ.77) iaa=193   ! Ir-0 62.70
      ELSE
          iaa=ia
      ENDIF
     
     
     
     
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
         IF (S2.LT.1.D-3) THEN 
           betaSLO=0.D0
           DbetaSLO=0.D0
           GOTO 111
         ENDIF  
         IF (S1.LT.S2) THEN 
c         IF ((S1+DS1).LT.(S2-DS2)) THEN 
             Eaa=E1
             Ebb=E2
             DEaa=DE1
             DEbb=DE2
         ELSE
             Eaa=E2
             Ebb=E1  
             DEaa=DE2
             DEbb=DE1
         ENDIF
c       as in Lebidy            
c         betaSMLO =DSQRT(5.D0/(4*PI))*((Eaa/Ebb-0.089D0)/0.911D0-1.D0)/
c     &     ((Eaa/Ebb-0.089D0)/1.822D0+1.D0)  
         y=Eaa/Ebb
         fact1=(0.911D0-0.089D0/2)
         fact2=DSQRT(4*PI/5.D0)
         betaSLO =fact2*(1-y)/
     &     (y*fact1+0.5D0)
         y=Eaa/Ebb
         dfy=-fact2*(1.D0/(y*fact1+0.5D0)+
     &    (1.D0-y)*fact1/(y*fact1+0.5D0)**2)
         DbetaSLO =DSQRT((dfy/Ebb*DEaa)**2+(dfy*Eaa/(Ebb**2)*DEbb)**2) 
         
         betaSLOn =fact2*4.D0/3*(Ebb-Eaa)/(Ebb+Eaa)/0.92D0 
         DbetaSLOn =0.D0
         DbetaSLOn =fact2*4.D0/3/0.92D0*
     &   DSQRT((((-1)/(Ebb+Eaa)+(Ebb-Eaa)/((Ebb+Eaa)**2))*DEaa)**2+
     &         (((1)/(Ebb+Eaa)-(Ebb-Eaa)/((Ebb+Eaa)**2))*DEbb)**2)   
     
     
  111    CONTINUE 
  
         keyslomlo=1    ! 1-SLO,2-SMLO
         beta=betaSLO
         beta=betadef           
         Call READ_GDR_PAR_SYS(iz,iaa,beta,keyslomlo)
         WRITE(88,'(2I4,1X,A2,1X,A3,8F10.4)')iz,ia,El,react,
     &    E1,DE1,E2,DE2,E1s,DE1s,E2s,DE2s   
    
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
         READ(67, 3, END = 2, ERR = 2)iz,ia,El,react,EgR1,
     &     GamR1,SigR1,EgR2,GamR2,SigR2,SigF,Emin,Emax,refer
         READ(67, 4, END = 2, ERR = 2)dEgR1,
     &     dGamR1,dSigR1,dEgR2,dGamR2,dSigR2,dSigF
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
         IF (S2.LT.1.D-3) THEN 
           betaSMLO=0.D0
           DbetaSMLO=0.D0
           GOTO 112
         ENDIF  
         IF (S1.LT.S2) THEN 
c        IF ((S1+DS1).LT.(S2-DS2)) THEN         
             Eaa=E1
             Ebb=E2
             DEaa=DE1
             DEbb=DE2
         ELSE
             Eaa=E2
             Ebb=E1  
             DEaa=DE2
             DEbb=DE1
         ENDIF
c       as in Lebidy            
c         betaSMLO =DSQRT(5.D0/(4*PI))*((Eaa/Ebb-0.089D0)/0.911D0-1.D0)/
c     &     ((Eaa/Ebb-0.089D0)/1.822D0+1.D0)  
         y=Eaa/Ebb
         fact1=(0.911D0-0.089D0/2)
         fact2=DSQRT(4*PI/5.D0)
         betaSMLO =fact2*(1-y)/
     &     (y*fact1+0.5D0)
         y=Eaa/Ebb
         dfy=-fact2*(1.D0/(y*fact1+0.5D0)+
     &    (1.D0-y)*fact1/(y*fact1+0.5D0)**2)
         DbetaSMLO =DSQRT((dfy/Ebb*DEaa)**2+(dfy*Eaa/(Ebb**2)*DEbb)**2) 
         
         betaSMLOn =fact2*4.D0/3*(Ebb-Eaa)/(Ebb+Eaa)/0.92D0   
         DbetaSMLOn =0.D0
         DbetaSMLOn =fact2*4.D0/3/0.92D0*
     &   DSQRT(((-1/(Ebb+Eaa)+(Ebb-Eaa)/((Ebb+Eaa)**2))*DEaa)**2+
     &         ((1/(Ebb+Eaa)-(Ebb-Eaa)/((Ebb+Eaa)**2))*DEbb)**2)           
         
  112    CONTINUE 
  
  
         betadef=-7.D0
         DO ik = 1,ikenddeflib
           IF ((ABS(iaa-Adeflib(ik)).LT.0.1).AND.
     &       (ABS(iz-Zdeflib(ik)).LT.0.1)) THEN
             betadef=Betadeflib(ik)
           ENDIF
         ENDDO
         
        betapri=-7.D0
         DO ik = 1,ikendpri
           IF ((ABS(iaa-Apri(ik)).LT.0.1).AND.
     &       (ABS(iz-Zpri(ik)).LT.0.1)) THEN
             betapri=BetapriD(ik)
           ENDIF
         ENDDO
         
         betayush=-7.D0
         DO ik = 1,ikendyush
           IF ((ABS(iaa-iAyush(ik)).LT.0.1).AND.
     &       (ABS(iz-iZyush(ik)).LT.0.1)) THEN
             betayush=BetayushD(ik)
           ENDIF
         ENDDO


         betaraman=-7.D0
         dbetaraman=0.D0
         DO ik = 1,ikendraman
           IF ((ABS(iaa-iAraman(ik)).LT.0.1).AND.
     &       (ABS(iz-iZraman(ik)).LT.0.1)) THEN
             betaraman=BetaramanD(ik)
             dbetaraman=DBetaramanD(ik)             
           ENDIF
         ENDDO         
         
         
       IF ((MOD(iz,2) .eq. 0).AND.(MOD(iaa,2) .eq. 0))   THEN
         
       IF (((iaa.GE.152).AND.(iaa.LE.190)).OR.(iaa.GE.224))   THEN 
       
       
         WRITE(77,'(2I4,1X,A2,1X,A3,13F10.4)')iz,iaa,El,react,
     &    betaSLO,DbetaSLO,betaSMLO,DbetaSMLO,betadef,betapri,betayush,
     &    betaraman,dbetaraman,betaSLOn,DbetaSLOn,betaSMLOn,DbetaSMLOn  
        

         IF ((ABS(iz-izt).LT.0.1).AND.(ABS(iaa-iat).LT.0.1)) THEN
         ELSE
         WRITE(78,'(2I4,1X,A2,1X,A3,13F10.4)')iz,iaa,El,react,
     &    betaSLO,DbetaSLO,betaSMLO,DbetaSMLO,betadef,betapri,betayush,
     &    betaraman,dbetaraman,betaSLOn,DbetaSLOn,betaSMLOn,DbetaSMLOn  
         izt=iz
         iat=iaa
           IF  (((betaSLO*betadef).LT.0).OR.
     &      (((betayush*betadef).LT.0).AND.(betayush.GT.-5)).OR.
     &      ((betaSMLO*betadef).LT.0)) THEN
            WRITE(79,'(2I4,1X,A2,1X,A3,13F10.4)')iz,iaa,El,react,
     &    betaSLO,DbetaSLO,betaSMLO,DbetaSMLO,betadef,betapri,betayush,
     &    betaraman,dbetaraman,betaSLOn,DbetaSLOn,betaSMLOn,DbetaSMLOn 
           ENDIF
         ENDIF
          
        ENDIF          
        ENDIF


         keyslomlo=2    ! 1-SLO,2-SMLO
         beta=betaSMLO  
         beta=betadef  
         Call READ_GDR_PAR_SYS(iz,iaa,beta,keyslomlo)
         WRITE(89,'(2I4,1X,A2,1X,A3,8F10.4)')iz,iaa,El,react,
     &    E1,DE1,E2,DE2,E1s,DE1s,E2s,DE2s  

     
      ENDDO
  2   CONTINUE
      CLOSE(66)
      CLOSE(67)      
      CLOSE(77)
      CLOSE(88)








      STOP
      END
      
      
C
      FUNCTION SMAT(Iz)
C
C-----RETURNS CHEMICAL SYMBOL OF AN ELEMENT WITH Z=IZ
C
C
C
C Dummy arguments
C
      INTEGER Iz
      CHARACTER*2 SMAT
C
C Local variables
C
      CHARACTER*2 mat(0:110)
C
C
C
C
C Dummy arguments
C
C
C Local variables
C
C
C
      DATA mat/'n ', 'p ', 'He', 'Li', 'Be', 'B ', 'C ', 'N ', 'O ',
     &     'F ', 'Ne', 'Na', 'Mg', 'Al', 'Si', 'P ', 'S ', 'Cl', 'Ar',
     &     'K ', 'Ca', 'Sc', 'Ti', 'V ', 'Cr', 'Mn', 'Fe', 'Co', 'Ni',
     &     'Cu', 'Zn', 'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr',
     &     'Y ', 'Zr', 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd',
     &     'In', 'Sn', 'Sb', 'Te', 'I ', 'Xe', 'Cs', 'Ba', 'La', 'Ce',
     &     'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er',
     &     'Tm', 'Yb', 'Lu', 'Hf', 'Ta', 'W ', 'Re', 'Os', 'Ir', 'Pt',
     &     'Au', 'Hg', 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra',
     &     'Ac', 'Th', 'Pa', 'U ', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf',
     &     'Es', 'Fm', 'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Ns', 'Hs',
     &     'Mt', '??'/
      SMAT = mat(Iz)
      RETURN
      END
       
      FUNCTION READSN(Iz,Ia)   
      DOUBLE PRECISION  Sn,READSN
      INTEGER Iz,Ia,ik,iat,izt,i 
      COMMON /FORSN/KEYSn
c      KEYSn=2 ! 1-Sn; 2-S2n
      Sn=0.D0
      IF (KEYSn.EQ.1) THEN
       OPEN(4,FILE='.\PARAMETERS\rct2-16.txt',STATUS='old')
      ELSE
       OPEN(4,FILE='.\PARAMETERS\rct1-16.txt',STATUS='old')
      ENDIF 
      REWIND 4
      READ(4,'(/////////////////////////////////////////)')
      DO i=1,5000 
         ik=1
         READ(4,'(I1,I3,4X,I3,F11.2)', END = 3, ERR = 3)ik,iat,izt  
         IF ((ABS(izt-Iz).LT.0.1).AND.(ABS(iat-Ia).LT.0.1)
     &    .AND.(ik.EQ.0))	THEN
          BACKSPACE 4
          READ(4,'(I1,I3,4X,I3,F11.2)', END = 3, ERR = 3)ik,iat,izt,Sn         
          GOTO 3
         ENDIF 
      ENDDO
    3 CONTINUE 
      READSN=Sn*1.D-3
      CLOSE(2)
      RETURN
      END
           