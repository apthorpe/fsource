C                                                                       00000010
C     SOFIRE TWOCELL CODE  SECONDARY ABOVE PRIMARY                      00000020
C        REVISED 7-28-72                                                00000030
C                                                                       00000040
      COMMON/SAVE/STSC(300),STGASC(300),STGASS(300),STWC1(300),STWS1(30000000050
     1),SXMC(300),SPGASC(300),STSC1(300),ST(300)                        00000060
C                                                                       00000070
      COMMON/COMA/TSC(500),TFC1(500),TFC2(500),TFC3(500),TFC4(500),TWC1(00000080
     1500),TWC2(500),TWC3(500),TWC4(500),TGASC(500),TWS1(500),TWS2(500),00000090
     2TWS3(500),TWS4(500),TGASS(500),PGASC(500),PGASS(500),XMC(500),XMAS00000100
     3S(500),XMASST(500),T(500),RHOGC(500),RHOGS(500),TSC1(500),TSC2(50000000110
     4),TSC3(500),TSC4(500)                                             00000120
C                                                                       00000130
      COMMON TSCI,TSC1I,TSC2I,TSC3I,TSC4I, TFC1I,TFC2I,TFC3I,TFC4I,TWC1I00000140
     1 ,TWC2I ,TWC3I ,TWC4I,TWS1I,TWS2I, TWS3I,TWS4I,TGASCI,PGASCI,CO   00000150
     2 ,TGASSI,PGASSI,TI   ,TA   ,PA   , XMAX ,ANA   ,QC   ,QO    ,Q1   00000160
     3 ,A1    ,A2    ,A3   ,A4   ,A6   , VOLAC,VOLAS ,AF   ,AF3   ,AWC  00000170
     4 ,F1    ,F4    ,XL   ,HIN  ,X    , S    ,SOD   ,PRT1 ,PRT2  ,PRT3 00000180
     5 ,ZNAS  ,ZNAS1 ,ZNAS2,ZNAS3,ZNAS4, ZF1  ,ZF12  ,ZF2  ,ZF3   ,ZF4  00000190
     6 ,XWC1  ,XWC12 ,XWC2 ,XWC3 ,XWC4 , XWS1 ,XWS12 ,XWS2 ,XWS3  ,XWS4 00000200
     7 ,CPAC  ,CPS   ,CPF1 ,CPF2 ,CPF3 , CPF4 ,CPWC1 ,CPWC2,CPWC3 ,CPWC400000210
     8 ,CPWS1 ,CPWS2 ,CPWS3,CPWS4,AKS  , AKF1 ,AKF12 ,AKF2 ,AKF3  ,AKF4 00000220
     9 ,AKWC1 ,AKWC12,AKWC2,AKWC3,AKWC4, AKWS1,AKWS12,AKWS2,AKWS3 ,AKWS400000230
     X ,RHOA  ,RHS   ,RHF1 ,RHF2 ,RHF3 , RHF4 ,RHWC1 ,RHWC2,RHWC3 ,RHWC400000240
     X ,RHWS1,RHWS2,RHWS3,RHWS4,PDLEAK,BLDGDP,PATCON                    00000250
C                                                                       00000260
      COMMON/COMB/QCONV1,QCONV2,QCONV4,QRADC,QRODC,QRAD1,QRAD2,QRAD4,SUM00000270
     1C,SUMS,WC,WS,CC,CS,ALPHA,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12,Y100000280
     23,Y15,Y16,Y17,Y18,CAPSC,CAPSC1,CAPSC2,CAPSC3,CAPSC4,CAPFC1,CAPFC2,00000290
     3CAPFC3,CAPFC4,CAPC1,CAPC2,CAPC3,CAPC4,CAPS1,CAPS2,CAPS3,CAPS4,DTMI00000300
     4N                                                                 00000310
C                                                                       00000320
      DIMENSION ORD(14,9),O1(14),O2(14),O3(14),O4(14),O5(14),O6(14),O7(100000330
     14),O8(14),O9(14),DATA(300)
C                                                                       00000350
      COMMON/TITL/TITLE(18)                                             00000360
C                                                                       00000370
      EQUIVALENCE(DATA(1),STSC(1)),(ORD(1),O1(1)),(ORD(1,2),O2(1)),(ORD(00000380
     11,3),O3(1)),(ORD(1,4),O4(1)),(ORD(1,5),O5(1)),(ORD(1,6),O6(1)),(OR00000390
     2D(1,7),O7(1)),(ORD(1,8),O8(1)),(ORD(1,9),O9(1))                   00000400
      DATA                                                              00000410
     1 O1  /'    ','    ',' CEL','L SO','DIUM',' SUR','F. T','EMPE',
     1'RATU','RE (','DEG.','F)  ','    ','    '/,
     2 O2  /'    ','    ',' CEL','L GA','S   ','   T','EMPE','RATU',
     2'RE (','DEG.','F)  ','    ','    ','    '/,
     3 O3  /'    ','    ',' DOM','E GA','S   ','   T','EMPE','RATU',
     3'RE (','DEG.','F)  ','    ','    ','    '/,
     4 O4  /'    ','    ','CELL',' WAL','L   ','   T','EMPE','RATU',
     4'RE (','DEG.','F)  ','    ','    ','    '/,
     5 O5  /'    ','    ','DOME',' WAL','L   ','   T','EMPE','RATU',
     5'RE (','DEG.','F)  ','    ','    ','    '/,
     6 O6  /'    ','    ',' CEL','L SO','DIUM',' BUR','NING',' RAT',
     6'E (L','BS/H','R)  ','    ','    ','    '/,
     7 O7  /'    ','    ',' CEL','L   ','GAS ','   P','RESS','URE ',
     7'    ','(PSI','G)  ','    ','    ','    '/,
     8 O8  /'    ','    ',' CEL','L SO','DIUM','   T','EMPE','RATU',
     8'RE (','DEG.','F)  ','    ','    ','    '/,
     9 O9  /'    ','    ','    ','  TI','ME  ','    ','    ','    ',
     9'   (','HRS)','    ','    ','    ','    '/
      WPA=0.
      CALL SETUP                                                        00000520
    1 CALL EP_READ                                                      00000530
      WRITE (6,33)                                                      00000540
   33 FORMAT (1H1)                                                      00000550
      I=1                                                               00000560
      M=1                                                               00000570
      TSC(1)=TSCI                                                       00000580
      TSC1(1)=TSC1I                                                     00000590
      TSC2(1)=TSC2I                                                     00000600
      TSC3(1)=TSC3I                                                     00000610
      TSC4(1)=TSC4I                                                     00000620
      TFC1(1)=TFC1I                                                     00000630
      TFC2(1)=TFC2I                                                     00000640
      TFC3(1)=TFC3I                                                     00000650
      TFC4(1)=TFC4I                                                     00000660
      TWC1(1)=TWC1I                                                     00000670
      TWC2(1)=TWC2I                                                     00000680
      TWC3(1)=TWC3I                                                     00000690
      TWC4(1)=TWC4I                                                     00000700
      TGASC(1)=TGASCI                                                   00000710
      TWS1(1)=TWS1I                                                     00000720
      TWS2(1)=TWS2I                                                     00000730
      TWS3(1)=TWS3I                                                     00000740
      TWS4(1)=TWS4I                                                     00000750
      TGASS(1)=TGASSI                                                   00000760
      PGASC(1)=PGASCI                                                   00000770
      PGASS(1)=PGASSI                                                   00000780
      RIN=53.3                                                          00000790
      RHOGCI=PGASCI/(RIN*TGASCI)                                        00000800
      RHOGSI=PGASSI/(RIN*TGASSI)                                        00000810
      RHOGC(1)=RHOGCI                                                   00000820
      RHOGS(1)=RHOGSI                                                   00000830
      XMC(1)=0.0                                                        00000840
      SUMC=0.                                                           00000850
      SUMS=0.                                                           00000860
      T(1)=TI                                                           00000870
      GIN=32.2                                                          00000880
      GINH=4.17E+08                                                     00000890
      CPAS=CPAC                                                         00000900
      IX1=PRT1                                                          00000910
      IX2=PRT2                                                          00000920
      IX3=PRT3                                                          00000930
      WS=RHOGSI*VOLAS                                                   00000940
      WC=RHOGCI*VOLAC                                                   00000950
C                                                                       00000960
      WIN=0.0                                                           00000970
      WEXT=0.0                                                          00000980
      F3=0.0                                                            00000990
      CC=CO                                                             00001000
      CS=CO                                                             00001010
      XX1=2.0*AKS*A3                                                    00001020
      XX2=CPS*A3*RHS                                                    00001030
      Y1=1.0/(ZNAS4/(AKS*A1*2.0)+ZF1/(AKF1*A1*2.0))                     00001040
      Y2=1.0/(ZF1/(AKF1*A1*2.0)+ZF12/(AKF12*A1*2.0)+ZF2/(AKF2*A1*2.0))  00001050
      Y3 =1.0/(ZF2 /(AKF2*A1*2.0)+ZF3 /(AKF3*A1*2.0))                   00001060
      Y4 =1.0/(ZF3 /(AKF3*A1*2.0)+ZF4 /(AKF4*A1*2.0))                   00001070
      Y5=AKF4*A1*2.0/ZF4                                                00001080
      Y6=1.0/(XWC1/(AKWC1*A2*2.0)+XWC12/(AKWC12*A2)+XWC2/(AKWC2*A2*2.0))00001090
      Y7 =1.0/(XWC2/(AKWC2*A2*2.0)+XWC3/(AKWC3*A2*2.0))                 00001100
      Y8 =1.0/(XWC3/(AKWC3*A2*2.0)+XWC4/(AKWC4*A2*2.0))                 00001110
      Y9=AKWC4*A2*2.0/XWC4                                              00001120
      Y10=XX1/(ZNAS+ZNAS1)                                              00001130
      Y11=XX1/(ZNAS1+ZNAS2)                                             00001140
      Y12=XX1/(ZNAS2+ZNAS3)                                             00001150
      Y13=XX1/(ZNAS3+ZNAS4)                                             00001160
      Y15=1./(XWS1/(AKWS1*A6*2.0)+XWS12/(AKWS12*A6)+XWS2/(AKWS2*A6*2.0))00001170
      Y16=1.0/(XWS2/(AKWS2*A6*2.0)+XWS3/(AKWS3*A6*2.0))                 00001180
      Y17=1.0/(XWS3/(AKWS3*A6*2.0)+XWS4/(AKWS4*A6*2.0))                 00001190
      Y18=AKWS4*A6*2.0/XWS4                                             00001200
      CAPSC=XX2*ZNAS                                                    00001210
      CAPSC1=XX2*ZNAS1                                                  00001220
      CAPSC2=XX2*ZNAS2                                                  00001230
      CAPSC3=XX2*ZNAS3                                                  00001240
      CAPSC4=XX2*ZNAS4                                                  00001250
      CAPFC1=CPF1*A1*ZF1*RHF1                                           00001260
      CAPFC2=CPF2*A1*ZF2*RHF2                                           00001270
      CAPFC3=CPF3*A1*ZF3*RHF3                                           00001280
      CAPFC4=CPF4*A1*ZF4*RHF4                                           00001290
      CAPC1=CPWC1*A2*XWC1*RHWC1                                         00001300
      CAPC2=CPWC2*A2*XWC2*RHWC2                                         00001310
      CAPC3=CPWC3*A2*XWC3*RHWC3                                         00001320
      CAPC4=CPWC4*A2*XWC4*RHWC4                                         00001330
      CAPS1=CPWS1*A6*XWS1*RHWS1                                         00001340
      CAPS2=CPWS2*A6*XWS2*RHWS2                                         00001350
      CAPS3=CPWS3*A6*XWS3*RHWS3                                         00001360
      CAPS4=CPWS4*A6*XWS4*RHWS4                                         00001370
C                                                                       00001380
      T1=0.5*(TGASC(1)+TSC(1))                                          00001390
      T2=0.5*(TGASC(1)+TWC1(1))                                         00001400
      T4=0.5*(TGASS(1)+TWS1(1))                                         00001410
      B1=1.0/T1                                                         00001420
      B2=1.0/T2                                                         00001430
      B4=1.0/T4                                                         00001440
      D1=((4.94E-05*T1+0.0188)/(RHOGC(1)*3600.0))**2                    00001450
      D2=((4.94E-05*T2+0.0188)/(RHOGC(1)*3600.0))**2                    00001460
      D4=((4.94E-05*T4+0.0188)/(RHOGC(1)*3600.0))**2                    00001470
      AK1=0.014+1.92E-05*(T1-460.0)                                     00001480
      AK2=0.014+1.92E-05*(T2-460.0)                                     00001490
      AK4=0.014+1.92E-05*(T4-460.0)                                     00001500
      EXPC1=(GIN*B1/D1*ABS(TSC(1)-TGASC(1)))**0.3333                    00001510
      EXPC2=(GIN*B2/D2*ABS(TGASC(1)-TWC1(1)))**0.333                    00001520
      EXPC4=(GIN*B4/D4*ABS(TGASS(1)-TWS1(1)))**0.333                    00001530
      QCONV1=0.14*A1*AK1*(TSC(1)-TGASC(1))*EXPC1                        00001540
      QCONV2=0.27*A2*AK2*(TGASC(1)-TWC1(1))*EXPC2                       00001550
      QCONV4=0.27*A6*AK4*(TGASS(1)-TWS1(1))*EXPC4                       00001560
      QRODC=AF3*0.1714E-08*(TSC(1)**4-TGASC(1)**4)*A3                   00001570
      QRAD1=F1*0.1714E-08*(TFC1(1)**4-TFC2(1)**4)*AWC                   00001580
      QRADC=AF*0.1714E-08*(TSC(1)**4-TWC1(1)**4)*A3                     00001590
      QRAD2=F1*0.1714E-08*(TWC1(1)**4-TWC2(1)**4)*A2                    00001600
      QRAD4=F4*0.1714E-08*(TWS1(1)**4-TWS2(1)**4)*A6                    00001610
      YCONV4=QCONV4/(TGASS(1)-TWS1(1))                                  00001620
      YRAD4=QRAD4/(TWS1(1)-TWS2(1))                                     00001630
      YRAD2=QRAD2/(TWC1(1)-TWC2(1))                                     00001640
      YRADC=QRADC/(TSC(1)-TWC1(1))                                      00001650
      YCONV2=QCONV2/(TGASC(1)-TWC1(1))                                  00001660
      YCONV1=QCONV1/(TSC(1)-TGASC(1))                                   00001670
      YRODC=QRODC/(TSC(1)-TGASC(1))                                     00001680
      YRAD1=QRAD1/(TFC1(1)-TFC2(1))                                     00001690
      DELGC=RHOGC(1)*CPAC*VOLAC/(YRODC+YCONV1+YCONV2)                   00001700
      DELGS=RHOGS(1)*CPAS*VOLAS/YCONV4                                  00001710
      DELSC=CAPSC/(Y10+YRODC+YCONV1+YRADC)                              00001720
      DELSC1=CAPSC1/(Y10+Y11)                                           00001730
      DELSC2=CAPSC2/(Y11+Y12)                                           00001740
      DELSC3=CAPSC3/(Y12+Y13)                                           00001750
      DELSC4=CAPSC4/(Y13+Y1)                                            00001760
      DELF1=CAPFC1/(Y2+YRAD1+Y1)                                        00001770
      DELF2=CAPFC2/(Y2+YRAD1+Y3)                                        00001780
      DELF3=CAPFC3/(Y3+Y4)                                              00001790
      DELF4=CAPFC4/(Y4+Y5)                                              00001800
      DELC1=CAPC1/(YRADC+YCONV2+YRAD2+Y6)                               00001810
      DELC2=CAPC2/(Y6+YRAD2+Y7)                                         00001820
      DELC3=CAPC3/(Y7+Y8)                                               00001830
      DELC4=CAPC4/(Y8+Y9)                                               00001840
      DELS1=CAPS1/(YCONV4+Y15+YRAD4)                                    00001850
      DELS2=CAPS2/(Y15+YRAD4+Y16)                                       00001860
      DELS3=CAPS3/(Y16+Y17)                                             00001870
      DELS4=CAPS4/(Y17+Y18)                                             00001880
      WRITE(6,50) T(I),XMC(I),QCONV1,QCONV2,QRODC,QCONV4,QRAD1,QRADC,QRA00001890
     1D2,QRAD4,YCONV4,YRAD4,YRAD2,YRADC,YCONV2,YCONV1,YRODC,YRAD1,DELGC,00001900
     2DELGS,DELSC,DELSC1,DELSC2,DELSC3,DELSC4,DELF1,DELF2,DELF3,DELF4,DE00001910
     3LC1,DELC2,DELC3,DELC4,DELS1,DELS2,DELS3,DELS4,WC,WS,RHOGC(I),RHOGS00001920
     X(I),PGASC(I),PGASS(I)                                             00001930
   50 FORMAT('0',24X,'TIME=',1P1E12.4,'    XMC =',E12.4//'  QCONV1=',E1200001940
     1.4/'  QCONV2=',E12.4,'   QRODC=',E12.4,'  QCONV4=',E12.4,'   QRAD100001950
     2=',E12.4/'   QRADC=',E12.4,'   QRAD2=',E12.4,'   QRAD4=',E12.4,'  00001960
     3YCONV4=',E12.4,'   YRAD4=',E12.4/'   YRAD2=',E12.4,'   YRADC=',E1200001970
     4.4,'  YCONV2=',E12.4,'  YCONV1=',E12.4,'   YRODC=',E12.4/'   YRAD100001980
     5=',E12.4,'   DELGC=',E12.4,'   DELGS=',E12.4,'   DELSC=',E12.4,'  00001990
     6DELSC1=',E12.4/'  DELSC2=',E12.4,'  DELSC3=',E12.4,'  DELSC4=',E1200002000
     7.4,'   DELF1=',E12.4,'   DELF2=',E12.4/'   DELF3=',E12.4,'   DELF400002010
     8=',E12.4,'   DELC1=',E12.4,'   DELC2=',E12.4,'   DELC3=',E12.4/'  00002020
     9 DELC4=',E12.4,'   DELS1=',E12.4,'   DELS2=',E12.4,'   DELS3=',E1200002030
     X.4,'   DELS4=',E12.4/'     WC=',E12.4,'     WS=',E12.4,' RHOGC=', 00002040
     XE12.4,'  RHOGS=',E12.4,'  PGASC=',E12.4/'  PGASS=',E12.4/)        00002050
C                                                                       00002060
   75 IF(T(I).LE.1.0) II=IX1                                            00002070
      IF(T(I).GT.1.0) II=IX2                                            00002080
      IF(T(I).GT.3.0) II=IX3                                            00002090
      DO 1000  I=2,II                                                   00002100
      J=I-1                                                             00002110
      YRODC=QRODC/(TSC(J)-TGASC(J))                                     00002120
      YCONV1=QCONV1/(TSC(J)-TGASC(J))                                   00002130
      YCONV2=QCONV2/(TGASC(J)-TWC1(J))                                  00002140
      DELSC1=CAPSC1/(Y10+Y11)                                           00002150
      DELSC2=CAPSC2/(Y11+Y12)                                           00002160
      DELSC3=CAPSC3/(Y12+Y13)                                           00002170
      DELSC4=CAPSC4/(Y13+Y1)                                            00002180
      YCONV4=QCONV4/(TGASS(J)-TWS1(J))                                  00002190
      YRAD4=QRAD4/(TWS1(J)-TWS2(J))                                     00002200
      YRAD2=QRAD2/(TWC1(J)-TWC2(J))                                     00002210
      YRADC=QRADC/(TSC(J)-TWC1(J))                                      00002220
      YRAD1=QRAD1/(TFC1(J)-TFC2(J))                                     00002230
      DELGS=RHOGS(J)*CPAS*VOLAS/YCONV4                                  00002240
      DELF1=CAPFC1/(Y2+YRAD1+Y1)                                        00002250
      DELF2=CAPFC2/(Y2+YRAD1+Y3)                                        00002260
      DELC1=CAPC1/(YRADC+YCONV2+YRAD2+Y6)                               00002270
      DELC2=CAPC2/(Y6+YRAD2+Y7)                                         00002280
      DELS1=CAPS1/(YCONV4+Y15+YRAD4)                                    00002290
      DELS2=CAPS2/(Y15+YRAD4+Y16)                                       00002300
      DTMIN1=CAPSC/(YRODC+YCONV1+Y10+YRADC)                             00002310
      DTMIN2=RHOGC(J)*CPAC*VOLAC/(YRODC+YCONV1+YCONV2)                  00002320
      DT=AMIN1(DTMIN1,DTMIN2,DELF1,DELF2,DELF3,DELF4,DELSC1,DELSC2,DELSC00002330
     13,DELSC4,DELC1,DELC2,DELC3,DELS1,DELS2,DELS3,DELS4)               00002340
      DTMIN=DT*X                                                        00002350
      T(I)=T(J)+DTMIN                                                   00002360
      T10=0.5*(TGASC(J)+TGASS(J))                                       00002370
      B10=1.0/T10                                                       00002380
      C10=0.5*(RHOGC(J)+RHOGS(J))                                       00002390
      ALPHA=3.58E-03*(T10-460.0)+0.6                                    00002400
      D10=((4.94E-05*T10+0.0188)/(C10*3600.0))**2                       00002410
      V10=(.07753/HIN)*(XL/HIN)**0.333*ALPHA*(HIN**3*GIN*B10/D10*ABS(TGA00002420
     1SC(J)-TGASS(J)))**0.55                                            00002430
      W=A4*V10*DTMIN*RHOGS(J)                                           00002440
      VW=W/RHOGS(J)                                                     00002450
      CC=(WC*CC+W*CS)/(WC+W)                                            00002460
      TGASC(J)=(WC*TGASC(J)+W*TGASS(J))/(WC+W)                          00002470
      WC=WC+W                                                           00002480
      WS=WS-W                                                           00002490
 1340 RHOGC(J)=WC/VOLAC                                                 00002500
      RHOGS(J)=WS/VOLAS                                                 00002510
C     PGASC(J)=RHOGC(J)*RIN*TGASC(J)                                    00002520
      IF(PDLEAK.GT.0.0) GO TO 200                                       00002530
C     PGASS(J)=RHOGS(J)*RIN*TGASS(J)                                    00002540
  200 CALL CELL (J)                                                     00002550
      IF(PDLEAK.EQ.0.0) GO TO 1330                                      00002560
C  STATEMENT 1329 TO 1330 CALCS REQD EXHAUST TO MAINTAIN STEADY PRESS   00002570
      IF(T(I).GT.24.0) PATCON=0.0                                       00002580
      PA=PA-PATCON*DTMIN                                                00002590
      PGASS(I)=PA-BLDGDP                                                00002600
      PGASC(I)=PGASS(J)-1.5*RHOGS(J)*V10**2/(2.0*GINH)+RHOGS(J)*HIN     00002610
      WCA=PGASC(I)*VOLAC/(RIN*TGASC(I))                                 00002620
      W1=WC-WCA                                                         00002630
      VW1=W1*TGASS(J)/(RHOGC(J)*TGASC(J))                               00002640
      WIN=PDLEAK*((PA-PGASS(I))/BLDGDP)**0.5*RHOA*60.0*DTMIN            00002650
      VWIN=WIN*TGASS(J)/(RHOA*TA)                                       00002660
      DG1=(WIN*(TGASS(J)-TA)+W1*(TGASS(J)-TGASC(I)))/(WS+WIN+W1-0.5*(WEX00002670
     1T-W))                                                             00002680
      DG1=-DG1                                                          00002690
      B4=1.0/T4                                                         00002700
      D4=((4.94E-05*T4+0.0188)/(RHOGS(J)*3600.0))**2                    00002710
      AK4=0.014+1.92E-05*(T4-460.0)                                     00002720
      EXPC4=(GIN*B4/D4*ABS(TGASS(J)-TWS1(J)))**0.333                    00002730
      QCONV4=0.27*A6*AK4*(TGASS(J)-TWS1(J))*EXPC4                       00002740
      DG2=DTMIN/(RHOGS(J)*CPAS*VOLAS)*(-QCONV4)                         00002750
      TGASS(I)=TGASS(J)+DG1+DG2                                         00002760
      VS1=VOLAS*TGASS(I)/TGASS(J)                                       00002770
      F3=(VS1-VOLAS+VW1+VWIN-VW)/(DTMIN*60.0)                           00002780
      WEXT=F3*60.0*DTMIN*RHOGS(J)                                       00002790
      WC=WC-W1                                                          00002800
      RHOGC(I)=WC/VOLAC                                                 00002810
      RHOGS(J)=(WS+WIN    +W1-WEXT)/VOLAS                               00002820
      CS=(CS*(WS-WEXT  )+CC*W1+CO* WIN     )/(WS+WIN    +W1-WEXT  )     00002830
      WS=RHOGS(J)*VOLAS                                                 00002840
      GO TO 1331                                                        00002850
 1330 PGASC(I)=PGASS(J)-1.5*RHOGS(J)*V10**2/(2.0*GINH)+RHOGS(J)*HIN     00002860
      WCA=PGASC(I)*VOLAC/(RIN*TGASC(I))                                 00002870
      W1=WC-WCA                                                         00002880
      CS=(WS*CS+W1*CC)/(WS+W1)                                          00002890
      TGASS(J)=(WS*TGASS(J)+W1*TGASC(I))/(WS+W1)                        00002900
      WC=WC-W1                                                          00002910
      WS=WS+W1                                                          00002920
      RHOGC(I)=WC/VOLAC                                                 00002930
      RHOGS(J)=WS/VOLAS                                                 00002940
      B4=1.0/T4                                                         00002950
      D4=((4.94E-05*T4+0.0188)/(RHOGS(J)*3600.0))**2                    00002960
      AK4=0.014+1.92E-05*(T4-460.0)                                     00002970
      EXPC4=(GIN*B4/D4*ABS(TGASS(J)-TWS1(J)))**0.333                    00002980
      QCONV4=0.27*A6*AK4*(TGASS(J)-TWS1(J))*EXPC4                       00002990
 1331 CONTINUE                                                          00003000
      QRAD4=F4*0.1714E-08*(TWS1(J)**4-TWS2(J)**4)*A6                    00003010
      IF(PDLEAK.NE.0.0) GO TO 1332                                      00003020
      DTGASS=DTMIN/(RHOGS(J)*CPAS*VOLAS)*(-QCONV4)                      00003030
      TGASS(I)=TGASS(J)+DTGASS                                          00003040
 1332 DTWS1=DTMIN/CAPS1*(QCONV4-Y15*(TWS1(J)-TWS2(J))-QRAD4)            00003050
      TWS1(I)=TWS1(J)+DTWS1                                             00003060
      T4=0.5*(TGASS(I)+TWS1(I))                                         00003070
      DTWS2=DTMIN/CAPS2*(Y15*(TWS1(J)-TWS2(J))-Y16*(TWS2(J)-TWS3(J))+QRA00003080
     1D4)                                                               00003090
      TWS2(I)=TWS2(J)+DTWS2                                             00003100
      DTWS3=DTMIN/CAPS3*(Y16*(TWS2(J)-TWS3(J))-Y17*(TWS3(J)-TWS4(J)))   00003110
      TWS3(I)=TWS3(J)+DTWS3                                             00003120
      DTWS4=DTMIN/CAPS4*(Y17*(TWS3(J)-TWS4(J))-Y18*(TWS4(J)-TA))        00003130
      TWS4(I)=TWS4(J)+DTWS4                                             00003140
      IF(PDLEAK.GT.0.0)  GO TO 1334                                     00003150
 1333 PGASS(I)=RHOGS(J)*RIN*TGASS(I)                                    00003160
 1334 RHOGS(I)=RHOGS(J)                                                 00003170
 1000 CONTINUE                                                          00003180
      TSC(1)=TSC(II)                                                    00003190
      TSC1(1)=TSC1(II)                                                  00003200
      TSC2(1)=TSC2(II)                                                  00003210
      TSC3(1)=TSC3(II)                                                  00003220
      TSC4(1)=TSC4(II)                                                  00003230
      TFC1(1)=TFC1(II)                                                  00003240
      TFC2(1)=TFC2(II)                                                  00003250
      TFC3(1)=TFC3(II)                                                  00003260
      TFC4(1)=TFC4(II)                                                  00003270
      TWC1(1)=TWC1(II)                                                  00003280
      TWC2(1)=TWC2(II)                                                  00003290
      TWC3(1)=TWC3(II)                                                  00003300
      TWC4(1)=TWC4(II)                                                  00003310
      TGASC(1)=TGASC(II)                                                00003320
      TWS1(1)=TWS1(II)                                                  00003330
      TWS2(1)=TWS2(II)                                                  00003340
      TWS3(1)=TWS3(II)                                                  00003350
      TWS4(1)=TWS4(II)                                                  00003360
      TGASS(1)=TGASS(II)                                                00003370
      PGASC(1)=PGASC(II)                                                00003380
      PGASS(1)=PGASS(II)                                                00003390
      RHOGS(1)=RHOGS(II)                                                00003400
      RHOGC(1)=RHOGC(II)                                                00003410
      XMC(1)=XMC(II)                                                    00003420
      T(1)=T(II)                                                        00003430
      I=1                                                               00003440
      WRITE(6,191) T(I),XMC(I),TSC(I),TFC1(I),TFC2(I),TFC3(I),TFC4(I),TW00003450
     1C1(I),TWC2(I),TWC3(I),TWC4(I),TGASC(I),TWS1(I),TWS2(I),TWS3(I),TWS00003460
     24(I),TGASS(I),PGASC(I),QCONV1,QCONV2,QCONV4,QRADC,W,PGASS(I),SUMC,00003470
     3SUMS,WC,WS,QRODC,QRAD1,QRAD2,QRAD4,CC,CS,TSC1(I),ALPHA,V10,W1     00003480
     4,DTMIN1,DTMIN2,DTMIN,TSC2(I),TSC3(I),TSC4(I),WIN,WPA,WEXT,F3,PA   00003490
  191 FORMAT('0',24X,'TIME =',1P1E12.4,'    XMC =',E12.4,'    TSC =',E1200003500
     1.4//'   TFC1=',E12.4,'   TFC2 =',E12.5,'   TFC3 =',E12.4,'   TFC4 00003510
     2=',E12.4,'   TWC1 =',E12.4/'   TWC2 =',E12.4,'   TWC3 =',E12.4,'  00003520
     3 TWC4 =',E12.4,'   TGASC=',E12.4,'   TWS1 =',E12.4/'   TWS2 =',E1200003530
     4.4,'   TWS3 =',E12.4,'   TWS4 =',E12.4,'   TGASS=',E12.4,'   PGASC00003540
     5=',E12.4/'  QCONV1=',E12.4,'  QCONV2=',E12.4,'  QCONV4=',E12.4,'  00003550
     6 QRADC=',E12.4,'    W   =',E12.4/'  PGASS =',E12.4,'   SUMC =',E1200003560
     7.4,'   SUMS =',E12.4,'     WC =',E12.4,'     WS =',E12.4/'  QRODC 00003570
     8=',E12.4,'  QRAD1 =',E12.4,'  QRAD2 =',E12.4,'  QRAD4 =',E12.4,'  00003580
     9  CC  =',E12.4/'    CS  =',E12.4,'   TSC1 =',E12.4,'   ALPHA=',E1200003590
     A.4,'    V10 =',E12.4,'    W1  =',E12.4/'  DTMIN1=',E12.4,'  DTMIN200003600
     B=',E12.4,'  DTMIN =',E12.4,'   TSC2 =',E12.4,'   TSC3 =',E12.4/'  00003610
     C TSC4=',E12.4,'     WIN=',E12.4,'     WPA=',E12.4,'    WEXT=',E12.00003620
     D4,'      F3=',E12.4/'      PA=',E12.4/)                           00003630
      STSC(M)= TSC(1)-460.0                                             00003640
      STSC1(M)=TSC1(1)-460.0                                            00003650
      STGASC(M)= TGASC(1)-460.0                                         00003660
      STGASS(M)= TGASS(1)-460.0                                         00003670
      STWC1(M)= TWC1(1)-460.0                                           00003680
      STWS1(M)= TWS1(1)-460.0                                           00003690
      SXMC(M)= XMC(1)                                                   00003700
      SPGASC(M)=(PGASC(1)-PA)/144.0                                     00003710
      ST(M)= T(1)                                                       00003720
      IF(T(1).GE.XMAX) GO TO 499                                        00003730
      M=M+1                                                             00003740
      GO TO 75                                                          00003750
  499 DO 600 L=1,8                                                      00003760
      I=(L-1)*300+1                                                     00003770
  600 CONTINUE
C 600 CALL AICRT3 (0,KY(L),ST,STSC(I),M,1,2,2,27,TITLE,O9,ORD(1,L),1,1, 00003780
C    116.0,16.0,1,ST(1),ST(M),1,YLI,YUI)                                00003790
      GO TO 1                                                           00003800
      END                                                               00003810
      SUBROUTINE CELL(J)                                                00003820
C     REVISED 11-21-69                                                  00003830
C                                                                       00003840
      COMMON/COMA/TSC(500),TFC1(500),TFC2(500),TFC3(500),TFC4(500),TWC1(00003850
     1500),TWC2(500),TWC3(500),TWC4(500),TGASC(500),TWS1(500),TWS2(500),00003860
     2TWS3(500),TWS4(500),TGASS(500),PGASC(500),PGASS(500),XMC(500),XMAS00003870
     3S(500),XMASST(500),T(500),RHOGC(500),RHOGS(500),TSC1(500)         00003880
     4,TSC2(500),TSC3(500),TSC4(500)                                    00003890
C                                                                       00003900
      COMMON TSCI,TSC1I,TSC2I,TSC3I,TSC4I, TFC1I,TFC2I,TFC3I,TFC4I,TWC1I00003910
     1 ,TWC2I ,TWC3I ,TWC4I,TWS1I,TWS2I, TWS3I,TWS4I,TGASCI,PGASCI,CO   00003920
     2 ,TGASSI,PGASSI,TI   ,TA   ,PA   , XMAX ,ANA   ,QC   ,QO    ,Q1   00003930
     3 ,A1    ,A2    ,A3   ,A4   ,A6   , VOLAC,VOLAS ,AF   ,AF3   ,AWC  00003940
     4 ,F1    ,F4    ,XL   ,HIN  ,X    , S    ,SOD   ,PRT1 ,PRT2  ,PRT3 00003950
     5 ,ZNAS  ,ZNAS1 ,ZNAS2,ZNAS3,ZNAS4, ZF1  ,ZF12  ,ZF2  ,ZF3   ,ZF4  00003960
     6 ,XWC1  ,XWC12 ,XWC2 ,XWC3 ,XWC4 , XWS1 ,XWS12 ,XWS2 ,XWS3  ,XWS4 00003970
     7 ,CPAC  ,CPS   ,CPF1 ,CPF2 ,CPF3 , CPF4 ,CPWC1 ,CPWC2,CPWC3 ,CPWC400003980
     8 ,CPWS1 ,CPWS2 ,CPWS3,CPWS4,AKS  , AKF1 ,AKF12 ,AKF2 ,AKF3  ,AKF4 00003990
     9 ,AKWC1 ,AKWC12,AKWC2,AKWC3,AKWC4, AKWS1,AKWS12,AKWS2,AKWS3 ,AKWS400004000
     X ,RHOA  ,RHS   ,RHF1 ,RHF2 ,RHF3 , RHF4 ,RHWC1 ,RHWC2,RHWC3 ,RHWC400004010
     X ,RHWS1,RHWS2,RHWS3,RHWS4,PDLEAK,BLDGDP,PATCON                    00004020
C                                                                       00004030
      COMMON/COMB/QCONV1,QCONV2,QCONV4,QRADC,QRODC,QRAD1,QRAD2,QRAD4,SUM00004040
     1C,SUMS,WC,WS,CC,CS,ALPHA,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12,Y100004050
     23,Y15,Y16,Y17,Y18,CAPSC,CAPSC1,CAPSC2,CAPSC3,CAPSC4,CAPFC1,CAPFC2,00004060
     3CAPFC3,CAPFC4,CAPC1,CAPC2,CAPC3,CAPC4,CAPS1,CAPS2,CAPS3,CAPS4,DTMI00004070
     4N                                                                 00004080
C                                                                       00004090
      I=J+1                                                             00004100
      GIN=32.2                                                          00004110
      T1=0.5*(TGASC(J)+TSC(J))                                          00004120
      T2=0.5*(TGASC(J)+TWC1(J))                                         00004130
      B1=1.0/T1                                                         00004140
      B2=1.0/T2                                                         00004150
      D1=((4.94E-05*T1+0.0188)/(RHOGC(J)*3600.0))**2                    00004160
      D2=((4.94E-05*T2+0.0188)/(RHOGC(J)*3600.0))**2                    00004170
      AK1=0.014+1.92E-05*(T1-460.0)                                     00004180
      AK2=0.014+1.92E-05*(T2-460.0)                                     00004190
      EXPC1=(GIN*B1/D1*ABS(TSC(J)-TGASC(J)))**0.3333                    00004200
      EXPC2=(GIN*B2/D2*ABS(TGASC(J)-TWC1(J)))**0.3333                   00004210
      QCONV1=0.14*A1*AK1*(TSC(J)-TGASC(J))*EXPC1                        00004220
      QCONV2=0.27*A2*AK2*(TGASC(J)-TWC1(J))*EXPC2                       00004230
      QRADC=AF*0.1714E-08*(TSC(J)**4-TWC1(J)**4)*A3                     00004240
      QRODC=AF3*0.1714E-08*(TSC(J)**4-TGASC(J)**4)*A3                   00004250
      QRAD1=F1*0.1714E-08*(TFC1(J)**4-TFC2(J)**4)*AWC                   00004260
      QRAD2=F1*0.1714E-08*(TWC1(J)**4-TWC2(J)**4)*A2                    00004270
      IF((SOD-SUMS).LE.0.1) GO TO 100                                   00004280
      DIFFC=241.57/(132.0+T1/1.8)*(T1/493.2)**2.5                       00004290
      HFC=0.140*DIFFC*EXPC1                                             00004300
      XMC(I)=CC*ANA*HFC*RHOGC(J)                                        00004310
      WOXC=XMC(I)*A3*DTMIN/ANA                                          00004320
      SUMC=WOXC+SUMC                                                    00004330
      CC=(WC*CC-WOXC)/(WC-WOXC)                                         00004340
      WC=WC-WOXC                                                        00004350
      SUMS=SUMC*ANA                                                     00004360
  100 IF((SOD-SUMS).LE.0.1) XMC(I)=0.0                                  00004370
      DTGASC=DTMIN/(RHOGC(J)*CPAC*VOLAC)*(QCONV1-QCONV2+QRODC)          00004380
      TGASC(I)=TGASC(J)+DTGASC                                          00004390
      DTSC=DTMIN/CAPSC*(XMC(I)*A3*QC-QCONV1-Y10*(TSC(J)-TSC1(J))-QRODC-Q00004400
     1RADC)                                                             00004410
      TSC(I)=TSC(J)+DTSC                                                00004420
      ST=S*DTMIN                                                        00004430
      CAPSC1=CAPSC1+ST                                                  00004440
      DTSC1=DTMIN/CAPSC1*(Y10*(TSC(J)-TSC1(J))-Y11*(TSC1(J)-TSC2(J)))   00004450
      TSC1(I)=TSC1(J)+DTSC1                                             00004460
      QRAD1=F1*0.1714E-08*(TFC1(J)**4-TFC2(J)**4)*AWC                   00004470
      CAPSC2=CAPSC2+ST                                                  00004480
      DTSC2=DTMIN/CAPSC2*(Y11*(TSC1(J)-TSC2(J))-Y12*(TSC2(J)-TSC3(J)))  00004490
      TSC2(I)=TSC2(J)+DTSC2                                             00004500
      CAPSC3=CAPSC3+ST                                                  00004510
      DTSC3=DTMIN/CAPSC3*(Y12*(TSC2(J)-TSC3(J))-Y13*(TSC3(J)-TSC4(J)))  00004520
      TSC3(I)=TSC3(J)+DTSC3                                             00004530
      CAPSC4=CAPSC4+ST                                                  00004540
      DTSC4=DTMIN/CAPSC4*(Y13*(TSC3(J)-TSC4(J))-Y1*(TSC4(J)-TFC1(J)))   00004550
      TSC4(I)=TSC4(J)+DTSC4                                             00004560
      DTFC1=DTMIN/CAPFC1*(Y1*(TSC4(J)-TFC1(J))-Y2*(TFC1(J)-TFC2(J))-QO-Q00004570
     1RAD1)                                                             00004580
      TFC1(I)=TFC1(J)+DTFC1                                             00004590
      DTFC2=DTMIN/CAPFC2*(Y2*(TFC1(J)-TFC2(J))-Y3*(TFC2(J)-TFC3(J))+QRAD00004600
     11)                                                                00004610
      TFC2(I)=TFC2(J)+DTFC2                                             00004620
      DTFC3=DTMIN/CAPFC3*(Y3*(TFC2(J)-TFC3(J))-Y4*(TFC3(J)-TFC4(J)))    00004630
      TFC3(I)=TFC3(J)+DTFC3                                             00004640
      DTFC4=DTMIN/CAPFC4*(Y4*(TFC3(J)-TFC4(J))-Y5*(TFC4(J)-TA))         00004650
      TFC4(I)=TFC4(J)+DTFC4                                             00004660
      QRAD2=F1*0.1714E-08*(TWC1(J)**4-TWC2(J)**4)*A2                    00004670
      DTWC1=DTMIN/CAPC1*(QCONV2-Y6*(TWC1(J)-TWC2(J))+QRADC-Q1-QRAD2)    00004680
      TWC1(I)=TWC1(J)+DTWC1                                             00004690
      DTWC2=DTMIN/CAPC2*(Y6*(TWC1(J)-TWC2(J))-Y7*(TWC2(J)-TWC3(J))+QRAD200004700
     1)                                                                 00004710
      TWC2(I)=TWC2(J)+DTWC2                                             00004720
      DTWC3=DTMIN/CAPC3*(Y7*(TWC2(J)-TWC3(J))-Y8*(TWC3(J)-TWC4(J)))     00004730
      TWC3(I)=TWC3(J)+DTWC3                                             00004740
      DTWC4=DTMIN/CAPC4*(Y8*(TWC3(J)-TWC4(J))-Y9*(TWC4(J)-TA))          00004750
      TWC4(I)=TWC4(J)+DTWC4                                             00004760
      RHOGC(I)=WC/VOLAC                                                 00004770
      RETURN                                                            00004780
      END                                                               00004790
      SUBROUTINE SETUP                                                  00004800
      COMMON //PUTINS(120)                                              00004810
      COMMON/TITL/TITLE                                                 00004820
      DIMENSION SAVINS(120),LABEL(120),TITLE(18)                        00004830
      REAL*8 LABEL                                                      00004840
    1 ND=120                                                            00004850
   10 CALL READIN(ND,LABEL,PUTINS,SAVINS,TITLE)                         00004860
      RETURN                                                            00004870
      END                                                               00004880
      SUBROUTINE FORED (DATA,N)                                         00004890
      DIMENSION DATA(1),ER(5),CARD(15)                                  00004900
C                                                                       00004910
C     RELOCATABLE FLOATING POINT DATA INPUT ROUTINE.                    00004920
C     LOC1,INDEX OF FIRST PIECE OF DATA ON CARD,GOES IN COLUMNS 7-12.   00004930
C     DATA(N) GOES IN COLS. 13-24, DATA(N) IN COLSS. 25-36,ETC          00004940
C     DATA LEFT BLANK IS NOT CHANGED, BUT MUST WRITE 0.0 FOR ZERO,NOT .000004950
C     MUST WRITE A NON-ZERO  N IN COL.1 OF THE LAST DATA CARD.          00004960
C                                                                       00004970
      DATA BLANK/1H /                                                   00004980
      X = 1.0                                                           00004990
   10 READ(5,11) N,LOC,LOC1,(ER(I),I=1,5),(CARD(I),I=1,15)              00005000
   11 FORMAT(I1,I5,I6,5E12.8,T13,15A4)                                  00005010
      LOC = LOC + LOC1                                                  00005020
      IF (LOC)20,20,30                                                  00005030
   20 WRITE (6,21)                                                      00005040
   21 FORMAT (46H1NEGATIVE OR ZERO LOCATION ON FORED DATA CARD.  )      00005050
      CALL EXIT                                                         00005060
   30 DO 60 I=1,5                                                       00005070
       IF(ER(I)) 50,40,50                                               00005080
   40 II=(I-1)*2 +I                                                     00005090
      IF(CARD(II).EQ.BLANK .AND. CARD(II+1).EQ.BLANK .AND. CARD(II+2)   00005100
     1 .EQ.BLANK ) GO TO 60                                             00005110
   50 L=LOC+I-1                                                         00005120
      DATA(L)=ER(I)                                                     00005130
   60 CONTINUE                                                          00005140
      IF(N)70,10,70                                                     00005150
   70 RETURN                                                            00005160
      END                                                               00005170
      SUBROUTINE READIN(ND,LABEL,PUTINS,SAVINS,TITLE)                   00005180
      DIMENSION LABEL(ND),PUTINS(ND),SAVINS(ND),TITLE(18),LABL(5)       00005190
      REAL*8 LABEL,LABL,BLANK
      DATA BLANK/'        '/
  100 DO 101 I=1,ND                                                     00005220
      LABEL(I) = BLANK                                                  00005230
  101 SAVINS(I) = 0.0                                                   00005240
  110 READ (5,111)N,NA,(LABL(K),K=1,5)                                  00005250
  111 FORMAT (I1,I5,6X,5(A8,4X))                                        00005260
      I = NA                                                            00005270
      DO 120 K=1,5                                                      00005280
      IF (LABL(K).EQ.BLANK) GO TO 120                                   00005290
      LABEL(I) = LABL(K)                                                00005300
  120 I= I+1                                                            00005310
      IF (N.EQ.0) GO TO 110                                             00005320
      RETURN                                                            00005330
C   *   *   *   *   *   *   *   *   *                                   00005340
      ENTRY EP_READ                                                     00005350
  200 READ (5,201) TITLE                                                00005360
  201 FORMAT (18A4)                                                     00005370
      WRITE (6,202) TITLE                                               00005380
  202 FORMAT (1H1,10X,18A4)                                             00005390
      DO 210 I=1,ND                                                     00005400
  210 PUTINS(I) = SAVINS(I)                                             00005410
      CALL FORED (PUTINS,N)                                             00005420
      IF (N.GT.1.AND.N.LT.9) GO TO 230                                  00005430
      DO 220 I=1,ND                                                     00005440
  220 SAVINS(I) = PUTINS(I)                                             00005450
      WRITE (6,221)                                                     00005460
  221 FORMAT ('0 THESE INPUTS HAVE BEEN STORED AS A REFERENCE SET')     00005470
      GO TO 240                                                         00005480
  230 WRITE (6,231)                                                     00005490
  231 FORMAT('0 DATA FOR THIS CASE ONLY. REFERENCE DATA UNCHANGED')     00005500
  240 DO 250 NA=1,ND,5                                                  00005510
      LA = NA                                                           00005520
      LB = NA+4                                                         00005530
      WRITE (6,241) (LABEL(L),L=LA,LB)                                  00005540
  241 FORMAT (1H0,7X,5(9X,A8))                                          00005550
      WRITE (6,242) LA,(PUTINS(L),L=LA,LB)                              00005560
  242 FORMAT(I6,4X,1P5E17.5)                                            00005570
  250 CONTINUE                                                          00005580
      IF (N.EQ.9) GO TO 200                                             00005590
      RETURN                                                            00005600
      END                                                               00005610
