;First Run with SPOLY
PC EQU 0
          INCLUDE   common.inc
          INCLUDE   cons.inc
          INCLUDE   NDATDD.INC
;Compute 
          INCLUDE   NSHPDD.INC
          INCLUDE   NMRGDD.INC
        INCLUDE   CONSACCT.inc
         include   nacddd.inc
         INCLUDE   NOWNDD.INC
         INCLUDE   NDAT3DD.INC
;patch1.6
          include   compdd.inc
          include   cntdd.inc
.         include   nmlrdd.inc
;patch1.6
;Compute
          INCLUDE   NADJDD.INC
          INCLUDE   NORDDD.INC
;begin patch 1.8    
;         INCLUDE   NINVDD.INC
          INCLUDE   ninvdd.inc
          INCLUDE   NINVACDDD.INC
;end patch 1.8      
          INCLUDE   NJSTDD.INC
MONTHINCREPORT      PFILE     
PRTFILENAME         DIM       50
PRINTNAME DIM       50

Arial8    font
        create  Arial8,"Arial",size=8
Arial9    font
        create  Arial9,"Arial",size=9
Arial10    font
        create  Arial10,"Arial",size=10
Arial11    font
        create  Arial11,"Arial",size=11
Arial12    font
        create  Arial12,"Arial",size=12
Arial14    font
        create  Arial14,"Arial",size=14
Arial16    font
        create  Arial16,"Arial",size=16
TimesNew6 font
          create  TimesNew6,"Times New Roman",size=7
TimesNew10          font
          create  TimesNew10,"Times New Roman",size=10
TimesNew10I         font
          create  TimesNew10I,"Times New Roman",size=10,Italic
Header1   form      9
Header2   form      9
Header3   form      9
Header4   form      9
Header5   form      9
Header6   form      9
Header7   form      9
Header9   form      9
Header10  form      9
Header11  form      9
Header12  form      9
Header13  form      9
Header14  form      9
Header15  form      9
Header16  form      9
Header17  form      9
Header18  form      9
Release   INIT      "1.8"      DLH      09March2005 Invoice Conversion
;Release  INIT      "1.7.1"    DMB      04OCT2004 Added code to loop through ajustments until the 9th adj is read even if it doesn't exist.
.Release  INIT      "1.7"      ASH      06AUG2004 Logo Conversion
.Release  INIT      "1.6"      DMB 26MAY2004      Mailer COnversion
.Release  INIT      "1.5"     DMB 01MAR2004 Added code to cut a year of fiscal reports.
.Release  INIT      "1.4"     DMB 30DEC20003 Added code to copy from c:\work to e:\data\income
.Release  INIT      "1.0"     Initial Release     of BO Monthy income report for clients
SingleSpaced        FORM      "160"
OneandahalfSpaced   FORM      "240"
DoubleSpaced        FORM      "320"
LgBoxHeight         FORM      "6751"
;LgBoxHeight        FORM      "6451"
;LgBoxHeight        FORM      "7491"
SmBoxHeight         FORM      "410"
;SmBoxHeight        FORM      "490"
;HalfsmboxHeight FORM         "245"
HalfsmboxHeight FORM          "205"
;MonthTextSmall     FORM      "480"
TotBoxHeight        Form      9
totBoxRow FORM      9
BegRowLine          FORM      9
BoxOneLeft          FORM      "0"
BoxOneRight         FORM      "500"
;BoxOneRight        FORM      "600"
BoxTwoLeft          FORM      "650"
BoxTwoRight         FORM      "2750"
BoxTwoLeft1         FORM      "1700"
BoxTwoRight1        FORM      "2750"
BoxThreeLeft        FORM      "2800"
BoxThreeRight       FORM      "6400"
BoxFourLeft         FORM      "6600"
;BoxFourRight       FORM      "7200"
BoxFourRight        FORM      "7100"
;BoxFiveLeft        FORM      "7250"
BoxFiveLeft         FORM      "7150"
;BoxFiveRight       FORM      "9500"
BoxFiveRight        FORM      "9400"
;BoxFiveLeft1       FORM      "8375"
BoxFiveLeft1        FORM      "8275"
;BoxFiveRight1      FORM      "9500"
BoxFiveRight1       FORM      "9400"
;BoxSixLeft         FORM      "9550"
BoxSixLeft          FORM      "9450"
;BoxSixRight        FORM      "10500"
BoxSixRight         FORM      "10400"
;List 
BoxTwoVert          FORM      "1700"
BoxThreeVert1       FORM      "4000"
BoxThreeVert2       FORM      "5200"
BoxFiveVert         FORM      "8275"
;BoxFiveVert        FORM      "8375"
;For Text Align close to but not on Vertical Line
BoxTwoVertText                FORM      "1690"
BoxThreeVert1Text   FORM      "3990"
BoxThreeVert2Text   FORM      "5150"
BoxFiveVertText               FORM      "8265"

BoxTwoRightText     FORM      "2740"
BoxThreeRightText   FORM      "6390"
BoxFiveRightText    FORM      "9390"
BoxSixRightText     FORM      "10390"
LtGrey    color
          create ltgrey=220:220:220
;Report Description
          move "5250" to Header1
;Description of Fiscal Year 1
          move      "2400" to Header2
;Description of Fiscal Year 2
          move      "8000" to Header3
;Description of Fiscal Year 3
          move      "1600" to Header4
;Description of Fiscal Year 4
          move      "3500" to Header5
;Description of Fiscal Year 1
          move      "5500" to Header6
;Description of Fiscal Year 2
          move      "8000" to Header7
;Cell Header equations
;Header9
          sub BoxTwoleft,BoxTworight,n9
          div c4 in n9,n8
          add BoxTwoLeft,n8,Header9
;Header10
          sub BoxTwoleft,BoxTworight,n9
          div c4 in n9,n8
          mult c3,n8
          add BoxTwoLeft,n8,Header10
;Header11
          sub BoxThreeleft,BoxThreeRight,n9
          div c3 in n9,n8
          div c2,n8
          add BoxThreeLeft,n8,Header11
;Header12
          sub BoxThreeleft,BoxThreeRight,n9
          div c3 in n9,n8
          div c2,n8
          mult      c3,n8
          add BoxThreeLeft,n8,Header12
;Header13
          sub BoxThreeleft,BoxThreeRight,n9
          div c3 in n9,n8
          div c2,n8
          mult      c5,n8
          add BoxThreeLeft,n8,Header13
;Header14
          sub BoxFiveleft,BoxFiveright,n9
          div c4 in n9,n8
          add BoxFiveLeft,n8,Header14
;Header15
          sub BoxFiveleft,BoxFiveright,n9
          div c4 in n9,n8
          mult c3,n8
          add BoxFiveLeft,n8,Header15
;Header16
          sub BoxSixleft,BoxSixright,n9
          div c2 in n9,n8
          add BoxSixLeft,n8,Header16
;Data Vars
BegFiscCur          FORM      5
EndFiscCur          FORM      5
BegFiscPrev         FORM      5
EndFiscPrev         FORM      5

JAN1      FORM      15
JAN2      FORM      15
FEB1      FORM      15
FEB2      FORM      15
MAR1      FORM      15
MAR2      FORM      15
APR1      FORM      15
APR2      FORM      15
MAY1      FORM      15
MAY2      FORM      15
JUN1      FORM      15
JUN2      FORM      15
JUL1      FORM      15
JUL2      FORM      15
AUG1      FORM      15
AUG2      FORM      15
SEP1      FORM      15
SEP2      FORM      15
OCT1      FORM      15
OCT2      FORM      15
NOV1      FORM      15
NOV2      FORM      15
DEC1      FORM      15
DEC2      FORM      15
OrdTOT1   FORM      15
OrdTOT2   FORM      15
JANRENT1  FORM      15
JANEXCH1  FORM      15
JANRENT2  FORM      15
JANEXCH2  FORM      15
FEBRENT1  FORM      15
FEBEXCH1  FORM      15
FEBRENT2  FORM      15
FEBEXCH2  FORM      15
MARRENT1  FORM      15
MAREXCH1  FORM      15
MARRENT2  FORM      15
MAREXCH2  FORM      15
APRRENT1  FORM      15
APREXCH1  FORM      15
APRRENT2  FORM      15
APREXCH2  FORM      15
MAYRENT1  FORM      15
MAYEXCH1  FORM      15
MAYRENT2  FORM      15
MAYEXCH2  FORM      15
JUNRENT2  FORM      15
JUNEXCH2  FORM      15
JUNRENT1  FORM      15
JUNEXCH1  FORM      15
JULRENT2  FORM      15
JULEXCH2  FORM      15
JULRENT1  FORM      15
JULEXCH1  FORM      15
AUGRENT2  FORM      15
AUGEXCH2  FORM      15
AUGRENT1  FORM      15
AUGEXCH1  FORM      15
SEPRENT2  FORM      15
SEPEXCH2  FORM      15
SEPRENT1  FORM      15
SEPEXCH1  FORM      15
OCTRENT2  FORM      15
OCTEXCH2  FORM      15
OCTRENT1  FORM      15
OCTEXCH1  FORM      15
NOVRENT2  FORM      15
NOVEXCH2  FORM      15
NOVRENT1  FORM      15
NOVEXCH1  FORM      15
DECRENT1  FORM      15
DECEXCH1  FORM      15
DECRENT2  FORM      15
DECEXCH2  FORM      15
EXCHTOT1  FORM      15
RENTTOT1  FORM      15
EXCHTOT2  FORM      15
RENTTOT2  FORM      15
;AP1
AP1JAN1   FORM      10.2
AP1JAN2   FORM      10.2
AP1FEB1   FORM      10.2
AP1FEB2   FORM      10.2
AP1MAR1   FORM      10.2
AP1MAR2   FORM      10.2
AP1APR1   FORM      10.2
AP1APR2   FORM      10.2
AP1MAY1   FORM      10.2
AP1MAY2   FORM      10.2
AP1JUN1   FORM      10.2
AP1JUN2   FORM      10.2
AP1JUL1   FORM      10.2
AP1JUL2   FORM      10.2
AP1AUG1   FORM      10.2
AP1AUG2   FORM      10.2
AP1SEP1   FORM      10.2
AP1SEP2   FORM      10.2
AP1OCT1   FORM      10.2
AP1OCT2   FORM      10.2
AP1NOV1   FORM      10.2
AP1NOV2   FORM      10.2
AP1DEC1   FORM      10.2
AP1DEC2   FORM      10.2
AP1TOT1   FORM      10.2
AP1TOT2   FORM      10.2
;Proj
ProjJAN1  FORM      10.2
ProjJAN2  FORM      10.2
ProjFEB1  FORM      10.2
ProjFEB2  FORM      10.2
ProjMAR1  FORM      10.2
ProjMAR2  FORM      10.2
ProjAPR1  FORM      10.2
ProjAPR2  FORM      10.2
ProjMAY1  FORM      10.2
ProjMAY2  FORM      10.2
ProjJUN1  FORM      10.2
ProjJUN2  FORM      10.2
ProjJUL1  FORM      10.2
ProjJUL2  FORM      10.2
ProjAUG1  FORM      10.2
ProjAUG2  FORM      10.2
ProjSEP1  FORM      10.2
ProjSEP2  FORM      10.2
ProjOCT1  FORM      10.2
ProjOCT2  FORM      10.2
ProjNOV1  FORM      10.2
ProjNOV2  FORM      10.2
ProjDEC1  FORM      10.2
ProjDEC2  FORM      10.2
ProjTOT1  FORM      10.2
ProjTOT2  FORM      10.2
;Var
VarJAN1   FORM      10.2
VarJAN2   FORM      10.2
VarFEB1   FORM      10.2
VarFEB2   FORM      10.2
VarMAR1   FORM      10.2
VarMAR2   FORM      10.2
VarAPR1   FORM      10.2
VarAPR2   FORM      10.2
VarMAY1   FORM      10.2
VarMAY2   FORM      10.2
VarJUN1   FORM      10.2
VarJUN2   FORM      10.2
VarJUL1   FORM      10.2
VarJUL2   FORM      10.2
VarAUG1   FORM      10.2
VarAUG2   FORM      10.2
VarSEP1   FORM      10.2
VarSEP2   FORM      10.2
VarOCT1   FORM      10.2
VarOCT2   FORM      10.2
VarNOV1   FORM      10.2
VarNOV2   FORM      10.2
VarDEC1   FORM      10.2
VarDEC2   FORM      10.2
VarTOT1   FORM      10.2
VarTOT2   FORM      10.2

;For compute
mrgsw    dim       1
shipsw   dim       1
;compute
          move      "61600",ProjJAN1
          move      "61600",ProjJAN2
          move      "41200",ProjFEB1
          move      "41200",ProjFEB2
          move      "38300",ProjMAR1
          move      "15300",ProjMAR2
          move      "7100",ProjAPR1
          move      "56800",ProjAPR2
          move      "44800",ProjMAY1
          move      "29300",ProjMAY2
          move      "8700",ProjJUN1
          move      "57900",ProjJUN2
          move      "70100",ProjJUL1
          move      "10000",ProjJUL2
          move      "57000",ProjAUG1
          move      "60000",ProjAUG2
          move      "34300",ProjSEP1
          move      "47500",ProjSEP2
          move      "65400",ProjOCT1
          move      "53500",ProjOCT2
          move      "67200",ProjNOV1
          move      "76900",ProjNOV2
          move      "85900",ProjDEC1
          move      "85900",ProjDEC2
          add ProjJAN1,ProjTOT1         
          add ProjFEB1,ProjTOT1
          add ProjMAR1,ProjTOT1
          add ProjAPR1,ProjTOT1
          add ProjMAY1,ProjTOT1
          add ProjJUN1,ProjTOT1
          add ProjJUL1,ProjTOT1
          add ProjAUG1,ProjTOT1
          add ProjSEP1,ProjTOT1
          add ProjOCT1,ProjTOT1
          add ProjNOV1,ProjTOT1
          add ProjDEC1,ProjTOT1
          add ProjJAN2,ProjTOT2         
          add ProjFEB2,ProjTOT2
          add ProjMAR2,ProjTOT2
          add ProjAPR2,ProjTOT2
          add ProjMAY2,ProjTOT2
          add ProjJUN2,ProjTOT2
          add ProjJUL2,ProjTOT2
          add ProjAUG2,ProjTOT2
          add ProjSEP2,ProjTOT2
          add ProjOCT2,ProjTOT2
          add ProjNOV2,ProjTOT2
          add ProjDEC2,ProjTOT2

;
DUMQTY    FORM      10
TMPVAR    FORM      10
RQTY      FORM      10
EXQTY     FORM      10
NUM102    FORM      10.2
AP1TOT    FORM      10.2


;
;Maksing Vars
mask20      init    "ZZZ,ZZZ,ZZZ,ZZZ,ZZ9"         ;formatting vars
Dim20a      dim     20            ;formatting vars
mask16      init    "Z,ZZZ,ZZZ,ZZZ.99"        ;formatting vars
Dim16a      dim     16            ;formatting vars

.Patch1.0
INCLISTS  FILE      
LNUM    DIM     6                 
DATEBY  DIM     1                
LTYPE   DIM         1                
LMONTH  FORM    2                
REP1    DIM     1                
REP2    DIM     1                
REP3    DIM     1                
LYEAR   FORM    4                
RECIPIENT       DIM     255   
COMMENTS                DIM     255

                   
LVARS     VARLIST LNUM:
          DATEBY:  
          LTYPE:
          LMONTH:   
          REP1:    
          REP2:    
          REP3:    
          LYEAR:   
          RECIPIENT:
          COMMENTS 
ManualFlag          DIM       1
FiscMonth FORM      2
mask19      init    "($ZZZ,ZZZ,ZZZ.99)"        ;formatting vars
Dim19a      dim     19            ;formatting vars
OPENPAY   FORM      10.2
NUM1021   FORM      10.2
NUM1022   FORM      10.2
HoldAP1   form 10.2
.patch1.3
TODAYIS   FORM      5
.START PATCH 1.7 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.7 ADDED LOGIC
          clock timestamp,timestamp
          unpack timestamp,str2,yy,mm,dd
          call      cvtjul
          move juldays  to TODAYIS
.Patch1.3

.patch1.4
          pack      PRINTNAME,timestamp
          pack      prtfilename with "c:\work\pdf\",printname,".lst"

.patch1.4 comment out
.         move "IncRev.lst",PRINTNAME
.         move      "c:\work\IncRev.lst" to prtfilename
.patch1.4
          PRTOPEN MONTHINCREPORT,"PDF995",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
          PRTPAGE MONTHINCREPORT;*UNITS=*HIENGLISH:
                          *ORIENT=*LANDSCAPE;
          OPEN      INCLISTS,"INCLISTS"

;Order Search
;Get Fiscal Year
.patch1.2


          call      paint
.patch1.3
DATEIS
          move      "Y" to str1
        PACK      str10 FROM mm,SLASH,dd,SLASH,yy  
        KEYIN     *P10:6,*DV,str10," OK? ",*t20,str1
        CMATCH    "N" TO str1
        GOTO      WhichWay IF NOT EQUAL
        KEYIN     *P10:6,*+,mm,"/",dd,"/",yy
        PACK      str10 FROM mm,SLASH,dd,SLASH,yy
          call        cvtjul
          move      juldays  to TODAYIS
          goto DateIS
.patch1.3


WhichWay
        MOVE      "A" to str1
        KEYIN     *ES,*P10:8,*EF,"M",*white,"anual or ",*YELLOW,"(A)uto: ",*T15,*RV,STR1
          if (str1 = "M")
                    move YES to ManualFlag
                    move "M" to REP1
NewList
                  KEYIN     *P10:10,*EF,*white,"List:  ",LNUM
                    GOTO      NEWLIST IF EOS
                    if (LNUM = "")
                              goto NewList
                    else
                              move lnum to NDATFLD
                              REP       " 0" IN NDATFLD
                              move c1 to ndatpath
                              CALL NDATKEY
                              if over
                                        clear LNUM
                                        Goto NEWLIST
                              endif
                    endif
        DISPLAY     *P10:12,*EF,*white,"List:  ",LNUM,"   ",OLSTNAME
         MOVE      "O" to str1
GetDate
         KEYIN     *P10:14,*EF,"Maildate Or ",*YELLOW,"(O)","rder date: ",*RV,DATEBY
                    goto      getdate if ((DATEBY <> "M") and (DATEBY <> "O"))

Basis     
         KEYIN     *P10:14,*EF,*white,"Accrual (By ",*YELLOW,"[I]",*white,"nvoice) Or ","Cash (",*YELLOW,"[C]",*White,"heck Date) Basis: ",*RV,LTYPE
                    GOTO      BASIS IF EOS
                    goto      BASIS if ((LTYPE <> "I") and (LTYPE <> "C"))
NewYear   
                    KEYIN     *P1:1,*P10:16,*JR,"ENTER STARTING 4 - DIGIT YEAR FOR TWO YEAR ANALYSIS ",*RV,str4
                    GOTO      NEWYear IF EOS
                    TYPE      str4
                    GOTO      NewYear IF NOT EQUAL
                    COUNT     n2,str4
                    move      str4 to lyear
                    goto      NEWYEAR if (n2 <> c4)

NewMonth  
                    move      "01" to LMONTH
                    KEYIN     *P1:1,*P10:18,*JR,"ENTER STARTING FISCAL YEAR'S STARTING MONTH  ",*RV,str2
                    GOTO      NEWMONTH IF EOS
                    TYPE      STR2
                    GOTO      NewMonth IF NOT EQUAL
                    REP       " 0" IN STR2
                    move      str2 to LMONTH
                    GOTO      MANUALLIST
          elseif (str1 <> "A")
                    Goto WhichWay
          endif


.patch1.2

.PATCH1.2
.READIT -moved below
.patch1.3
READITa
          READ      INCLISTS,SEQ;LVARS
          GOTO endprogram IF OVER
          GOTO READITa IF (REP1 <> "R")
.         GOTO READIT IF (REP1 <> "M")
        DISPLAY     *P10:10,*EF,*white,"List:  ",LNUM,"   ",OLSTNAME
READIT
.patch1.3
          clock timestamp,timestamp
          unpack timestamp,str4,n2
          move str4 to LYEAR
.         if (LMONTH < n2)
.                   add c1 to lyear
.         endif 
.For Fiscal
MANUALLIST
                    move LMONTH to fiscmonth
                    MOVE LMONTH TO MM
                    MOVE      "01" TO DD
.Patch1.5
                    if (Lmonth <> C1)
                              move LYEAR to n4
                              sub c1 from n4
                              move n4 to LYEAR
                    endif
.PATCH1.5
                    unpack LYEAR,str2,YY
;Year 1
                    call      cvtjul
                    move juldays to BegFiscCur
                    sub       c1 from juldays
                    call      cvtgreg
                    CLEAR     N2
                    MOVE      yy TO N2
                    pack str4 with CC,YY
                    move str4 to n4
                    add c1,n4
                    unpack n4,str2,n2
                    MOVE N2 TO YY
                    CALL CVTJUL
                    move juldays to EndFiscCur 
Year2
;Beg
                    move BegfiscCur to JULDAYS
                    call cvtgreg
                    pack str4 with CC,YY
                    move str4 to n4
                    sub c1,n4
                    unpack n4,str2,n2
                    move n2 to YY
                    rep zfill,YY
                    call cvtjul
                    move juldays to BegFiscPrev
;End
                    move EndfiscCur to JULDAYS
                    call cvtgreg
                    pack str4 with CC,YY
                    move str4 to n4
                    sub c1,n4
                    unpack n4,str2,n2
                    move n2 to YY
                    rep zfill,YY
                    call cvtjul
                    move juldays,EndFiscPrev
.patch1.1
          move lnum to NDATFLD
          move c1 to ndatpath
          CALL NDATKEY
          STOP IF OVER
        DISPLAY     *P10:10,*EF,*white,"List:  ",LNUM,"   ",OLSTNAME
.patch1.1
IncHeader
.START PATCH 1.7 REPLACED LOGIC
.         prtpage MONTHINCREPORT;*p0:0,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,*ulon,*boldon,"Names ",*boldoff;
..        prtpage MONTHINCREPORT;*p0:7750,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,*ulon,*boldon,"Names ",*boldoff;
.         prtpage MONTHINCREPORT;*font=TimesNew10I,*boldon,*ulon,"in the News ",*uloff,*boldoff;
.         add singlespaced to row
.         prtpage MONTHINCREPORT;*p0:160,*font=TimesNew6,*ll,"C A L I F O R N I A   I N C .";
          add singlespaced to row
          prtpage   MONTHINCREPORT;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.7 REPLACED LOGIC

          move "300" to row

          prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial14,*ll,*boldon,OLSTNAME,*boldoff;
          add       OneandahalfSpaced to row
          if (LTYPE = "I")
.         if (LSTNUM = "011507")
                    prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,"MONTHLY LIST INCOME/VOLUME REPORT - REPORTED ON AN ACCRUED BASIS (BY INVOICE DATE)";
          else
                    prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,"MONTHLY LIST INCOME/VOLUME REPORT - REPORTED ON A CASH BASIS (BY CHECK DATE)";

          endif
.prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,"MONTHLY LIST INCOME/VOLUME REPORT - REPORTED ON A CASH BASIS (BY CHECK DATE)";
          add       Doublespaced to row
;Header2
.prtpage MONTHINCREPORT;*pHeader11:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"FISCAL YEAR 2003 (01/01/03 - 12/31/03)",*boldoff;
          move BegFiscCur to juldays 
          call      cvtgreg
          pack      str10 with mm,"/",DD,"/",YY   
          move EndFiscCur to juldays 
          call      cvtgreg
          pack taskname with "FISCAL YEAR ",CC,YY," (",str10," - ",MM,"/",DD,"/",YY,")"
          prtpage MONTHINCREPORT;*pHeader11:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,TASKNAME,*boldoff;

;Header3
          move BegFiscPrev to juldays 
          call      cvtgreg
          pack      str10 with mm,"/",DD,"/",YY   
          move EndFiscPrev to juldays 
          call      cvtgreg
          pack taskname with "FISCAL YEAR ",CC,YY," (",str10," - ",MM,"/",DD,"/",YY,")"
          prtpage MONTHINCREPORT;*pHeader15:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,taskname,*boldoff;
.         prtpage MONTHINCREPORT;*pHeader15:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"FISCAL YEAR 2002 (01/01/02 - 12/31/02)",*boldoff;
;         add       OneandaHalfSpaced to row
          add       Doublespaced to row
;Header4
          prtpage MONTHINCREPORT;*pBoxTwoVert:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"Volume",*boldoff;
;Header5
          prtpage MONTHINCREPORT;*pHeader11:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"Income",*boldoff;
;Header6
          prtpage MONTHINCREPORT;*pBoxFiveVert:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"Income",*boldoff;
;Header7
          prtpage MONTHINCREPORT;*pBoxFiveVert:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"Volume",*boldoff;
ExcelGrid
          
          add       OneandaHalfSpaced to row
          move      row to BegRowLine
;Volume 1
          prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight:BoxTwoLeft:BoxTwoRight
;Income 1
          prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight:BoxThreeLeft:BoxThreeRight
;Volume2
          prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight:BoxFiveLeft:BoxFiveRight
;Actual
          prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight:BoxSixLeft:BoxSixRight
;Rent exchange halfboxes
;Add Shading - Using YELLOW for now
;;
          move row to n9
          move begrowline to row
          move begrowline to n8
          add  halfsmboxheight to n8
          clear n2
          loop      
                    
                    add       c1 to n2
                    prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*bgcolor=*YELLOW,*RECT=Row:n8:BoxTwoLeft1:BoxTwoRight
                    prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*bgcolor=*YELLOW,*RECT=Row:n8:BoxFiveLeft1:BoxFiveRight
          until     (n2 = "13")
                    add       smboxheight to row
                    add       smboxheight to n8
          repeat
          move      n9 to row
;;EndShading

;
;Using Boxes above in order to use shading
;                   add HalfsmboxHeight,row
;                   prtpage MONTHINCREPORT;*pBoxTwoLeft1:row,*pensize=15,*overlayon,*line=BoxTwoRight1:row;
;                   prtpage MONTHINCREPORT;*pBoxFiveLeft1:row,*pensize=15,*overlayon,*line=BoxFiveRight1:row;
;                   sub HalfsmboxHeight,row
;End comment out
TotalsSection
;Volume 1
          add lgboxheight,totBoxRow
          add "100",totBoxRow
          add smboxheight,totboxrow,TotBoxHeight
;
;Rent exchange halfboxes

;
          prtpage MONTHINCREPORT;*pensize=10,*FILL=*ON,*BGCOLOR=*WHITE,*RECT=TotBoxROW:TotBoxHeight:BoxOneLeft:BoxOneRight
;         prtpage MONTHINCREPORT;*pensize=10,*RECT=TotBoxROW:TotBoxHeight:BoxOneLeft:BoxOneRight
          prtpage MONTHINCREPORT;*pensize=10,*RECT=TotBoxROW:TotBoxHeight:BoxFourLeft:BoxFourRight

          prtpage MONTHINCREPORT;*pensize=15,*RECT=TotBoxROW:TotBoxHeight:BoxTwoLeft:BoxTwoRight
;Income 1
          prtpage MONTHINCREPORT;*pensize=15,*RECT=TotBoxROW:TotBoxHeight:BoxThreeLeft:BoxThreeRight
;Volume2
          prtpage MONTHINCREPORT;*pensize=15,*RECT=TotBoxROW:TotBoxHeight:BoxFiveLeft:BoxFiveRight
;Actual
          prtpage MONTHINCREPORT;*pensize=15,*RECT=TotBoxROW:TotBoxHeight:BoxSixLeft:BoxSixRight

;Rent exchange halfboxes
                    clear n8
                    add HalfsmboxHeight,totboxrow,n8
                    prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*bgcolor=*YELLOW,*RECT=totboxrow:n8:BoxTwoLeft1:BoxTwoRight
                    prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*bgcolor=*YELLOW,*RECT=totboxrow:n8:BoxFiveLeft1:BoxFiveRight
;                   prtpage MONTHINCREPORT;*pBoxTwoLeft1:totboxrow,*pensize=15,*overlayon,*line=BoxTwoRight1:totboxrow;
;                   prtpage MONTHINCREPORT;*pBoxFiveLeft1:totboxrow,*pensize=15,*overlayon,*line=BoxFiveRight1:totboxrow;
;                   sub HalfsmboxHeight,totboxrow

;Vertical Lines For Totals
          prtpage MONTHINCREPORT;*pBoxTwoVert:TotBoxROW,*bgcolor=*WHITE,*pensize=10,*line=BoxTwoVert:TotBoxHeight;
          prtpage MONTHINCREPORT;*pBoxThreeVert1:TotBoxROW,*pensize=10,*line=BoxThreeVert1:TotBoxHeight;
          prtpage MONTHINCREPORT;*pBoxThreeVert2:TotBoxROW,*pensize=10,*line=BoxThreeVert2:TotBoxHeight;
          prtpage MONTHINCREPORT;*pBoxFiveVert:TotBoxROW,*pensize=10,*line=BoxFiveVert:TotBoxHeight;
;;
;Title Cell
          move row to n9
;Topmost
          add "10" to Row
          prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Revised",*boldoff;
          sub       "10",Row
;Top

          add       "40" to row
          prtpage MONTHINCREPORT;*pHeader9:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Total",*boldoff;
          prtpage MONTHINCREPORT;*pHeader10:row,*bgcolor=*YELLOW,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Exch. Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader14:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Total",*boldoff;
          prtpage MONTHINCREPORT;*pHeader15:row,*bgcolor=*YELLOW,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Exch Volume",*boldoff;
          sub       "40" to row
;TopMiddle
          add       "120" to row
          prtpage MONTHINCREPORT;*pHeader12:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Projections",*boldoff;
          sub       "120" to row
;Middle Centered
          add singlespaced to row
          prtpage MONTHINCREPORT;*pHeader11:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Projections",*boldoff;
;         prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Projections",*boldoff;
          prtpage MONTHINCREPORT;*pHeader13:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Actual",*boldoff;
          prtpage MONTHINCREPORT;*pHeader16:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Actual",*boldoff;
;Second Line
          add "80" to row
;         add singlespaced to row
          prtpage MONTHINCREPORT;*pHeader9:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader10:row,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Rental Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader14:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader15:row,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Rental Volume",*boldoff;
;         prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Revised as of 8/13/03",*boldoff;
          add "20" to row
          prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Revised as of 8/13/03",*boldoff;
          move n9 to row



;Month Boxes
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pensize=10,*RECT=ROW:LgBoxHeight:BoxOneLeft:BoxOneRight
          prtpage MONTHINCREPORT;*pensize=10,*RECT=ROW:LgBoxHeight:BoxFourLeft:BoxFourRight
;Months Inserted
          Clear N2
          move row to n9
          add       halfsmboxheight to row
          add       "40" to row
          loop      

                    add       c1 to n2
                    load      str3 with n2,"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"
                    prtpage MONTHINCREPORT;*p250:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,str3;
                    prtpage MONTHINCREPORT;*p6850:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,str3;
                    add       SmBoxHeight to row
          until     (n2 = "12")
          repeat
;Total Inserted
          add       halfsmboxheight to Totboxrow
          add       "40" to Totboxrow
                    prtpage MONTHINCREPORT;*p250:TotBoxRow,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*ulon,*boldon,"Total";
                    prtpage MONTHINCREPORT;*p6850:TotBoxRow,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*ulon,*boldon,"Total";

          sub       halfsmboxheight to Totboxrow
          sub       "40" to Totboxrow
          move n9 to row
;Horizontal Lines
          Clear N2
          loop      
                  prtpage MONTHINCREPORT;*pBoxOneLeft:row,*pensize=10,*line=BoxOneRight:row;
                  prtpage MONTHINCREPORT;*pBoxTwoLeft:row,*pensize=10,*line=BoxTwoRight:row;
                    prtpage MONTHINCREPORT;*pBoxThreeLeft:row,*pensize=10,*line=BoxThreeRight:row;
                  prtpage MONTHINCREPORT;*pBoxFourLeft:row,*pensize=10,*line=BoxFourRight:row;
                  prtpage MONTHINCREPORT;*pBoxFiveLeft:row,*pensize=10,*line=BoxFiveRight:row;
          prtpage MONTHINCREPORT;*pBoxSixLeft:row,*pensize=10,*line=BoxSixRight:row;
                    add       c1 to n2
;                   add       HalfsmboxHeight,row
;                 prtpage MONTHINCREPORT;*pBoxTwoLeft1:row,*pensize=10,*line=BoxTwoRight1:row;
;                 prtpage MONTHINCREPORT;*pBoxFiveLeft1:row,*pensize=10,*overlayon,*line=BoxFiveRight1:row;
;                   sub       HalfsmboxHeight,row
                    add       SmBoxHeight to row
          until     (n2 = "12")
          repeat
;Vertical Lines
          prtpage MONTHINCREPORT;*pBoxTwoVert:BegRowLine,*pensize=10,*line=BoxTwoVert:LgBoxHeight;
          prtpage MONTHINCREPORT;*pBoxThreeVert1:BegRowLine,*pensize=10,*line=BoxThreeVert1:LgBoxHeight;
          prtpage MONTHINCREPORT;*pBoxThreeVert2:BegRowLine,*pensize=10,*line=BoxThreeVert2:LgBoxHeight;
          prtpage MONTHINCREPORT;*pBoxFiveVert:BegRowLine,*pensize=10,*line=BoxFiveVert:LgBoxHeight;
;Fill Months in

.Order Search
          MOVE      C1 TO NORDPATH
          Pack      NORDFLD2,"02L",LSTNUM
          call      nordaim
          IF NOT OVER
                    IF (DATEBY = "M")
                              MOVE      OMDTEM,MM
                              MOVE      OMDTEY,YY
                              MOVE      OMDTED,DD
                              call      cvtjul
                    else
                              MOVE      OODTEM,MM
                              MOVE      OODTEY,YY
                              MOVE      OODTED,DD
                              call      cvtjul
                    endif
                    Goto Enter
          else
                    if (manualflag <> "Y")
                              goto READIT1
                    else
                              goto ENDPROGRAM
                    endif
          Endif
;Last Years Orders
          Loop      
                    call      nordkg
                    clear     rqty
                    clear     exqty
          until     OVER
                    IF NOT OVER
.patch1.2
                    IF (DATEBY = "M")
                              MOVE      OMDTEM,MM
                              MOVE      OMDTEY,YY
                              MOVE      OMDTED,DD
                              call      cvtjul
                    else
                              MOVE      OODTEM,MM
                              MOVE      OODTEY,YY
                              MOVE      OODTED,DD
                              call      cvtjul
                    endif
.                             MOVE      OODTEM,MM
.                             MOVE      OODTEY,YY
.                             MOVE      OODTED,DD
.                             call      cvtjul
.patch1.2
                    endif
Enter
.Patch1.3
                    goto SKIP if (Juldays > TODAYIS)
.Patch1.3
                    if ((Juldays >= BegFiscPrev) & (Juldays  <=  EndFiscPrev))
                              if (OSTAT = "B" or OSTAT = "0")
                                        clear TMPVAR
                                        if (oexqty > "0")
                                                  move oexqty to n9
                                                  move      oqty to n10
                                                  sub n9 from n10,TMPVAR 
                                                  move      TMPVAR to RQTY
                                                  move      n9 to EXQTY
                                                            add       EXQTY to EXCHTOT2 
                                                            add       EXQTY to ORDTOT2
                                                            add       RQTY to RENTTOT2 
                                                            add       RQTY to ORDTOT2
                                        else
                                                  reset excodes
                                                  scan oelcode in excodes
                                                  if equal
                                                            move oqty to EXQTY
                                                            add       EXQTY to EXCHTOT2 
                                                            add       EXQTY to ORDTOT2
                                                  else
                                                            move oqty to RQTY
                                                            add       RQTY to RENTTOT2 
                                                            add       RQTY to ORDTOT2
                                                  endif
                                        endif
                                                  call      cvtgreg   
                                                  clear n2
.For Fiscal
                                                  move mm to n2
                                                  If (FiscMonth <> c1)
                                                            If (fiscmonth = 2)
                                                                      load      MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
                                                            elseif (fiscmonth = 3) 
                                                                      load      MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
                                                            elseif (fiscmonth = 4) 
                                                                      load      MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
                                                            elseif (fiscmonth = 5) 
                                                                      load      MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
                                                            elseif (fiscmonth = 6) 
                                                                      load      MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
                                                            elseif (fiscmonth = 7) 
                                                                      load      MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
                                                            elseif (fiscmonth = 8) 
                                                                      load      MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
                                                            elseif (fiscmonth = 9) 
                                                                      load      MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
                                                            elseif (fiscmonth = 10) 
                                                                      load      MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
                                                            elseif (fiscmonth = 11) 
                                                                      load      MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
                                                            elseif (fiscmonth = 12) 
                                                                      load      MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
                                                            endif
.                                                           If (fiscmonth = 2)
.                                                                     load      MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
.                                                           elseif (fiscmonth = 3) 
.                                                                     load      MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
.                                                           elseif (fiscmonth = 4) 
.                                                                     load      MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
.                                                           elseif (fiscmonth = 5) 
.                                                                     load      MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
.                                                           elseif (fiscmonth = 6) 
.                                                                     load      MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
.                                                           elseif (fiscmonth = 7) 
.                                                                     move mm to n2
.                                                                     load      MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
.                                                           elseif (fiscmonth = 8) 
.                                                                     load      MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
.                                                           elseif (fiscmonth = 9) 
.                                                                     load      MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
.                                                           elseif (fiscmonth = 10) 
.                                                                     load      MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
.                                                           elseif (fiscmonth = 11) 
.                                                                     load      MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
.                                                           elseif (fiscmonth = 12) 
.                                                                     load      MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
.                                                           endif
                                                  EndIf
                                                  move mm to n2
.
;Qty 
                                                  clear dumqty
                                                  clear n9
                                                  clear str15
                                                  load      str15,n2,JAN2,FEB2,MAR2,APR2,MAY2,JUN2,JUL2,AUG2,SEP2,OCT2,NOV2,DEC2  
                                                  move      str15 to dumqty
                                                  move      oqty to n9
                                                  add                   n9 to dumqty
                                                  move dumqty to str15
                                                  store     str15,n2,JAN2,FEB2,MAR2,APR2,MAY2,JUN2,JUL2,AUG2,SEP2,OCT2,NOV2,DEC2  
;Rental
                                                  clear dumqty
                                                  clear n9
                                                  clear str15
                                                  load      str15,n2,JANRENT2,FEBRENT2,MARRENT2,APRRENT2,MAYRENT2,JUNRENT2,JULRENT2,AUGRENT2,SEPRENT2,OCTRENT2,NOVRENT2,DECRENT2    
                                                  move      str15 to dumqty
                                                  move      rqty to n9
                                                  add                   n9 to dumqty
                                                  move dumqty to str15
                                                  store     str15,n2,JANRENT2,FEBRENT2,MARRENT2,APRRENT2,MAYRENT2,JUNRENT2,JULRENT2,AUGRENT2,SEPRENT2,OCTRENT2,NOVRENT2,DECRENT2    
;Exch
                                                  clear dumqty
                                                  clear n9
                                                  clear str15
                                                  load      str15,n2,JANEXCH2,FEBEXCH2,MAREXCH2,APREXCH2,MAYEXCH2,JUNEXCH2,JULEXCH2,AUGEXCH2,SEPEXCH2,OCTEXCH2,NOVEXCH2,DECEXCH2    
                                                  move      str15 to dumqty
                                                  move      exqty to n9
                                                  add                   n9 to dumqty
                                                  move dumqty to str15
                                                  store     str15,n2,JANEXCH2,FEBEXCH2,MAREXCH2,APREXCH2,MAYEXCH2,JUNEXCH2,JULEXCH2,AUGEXCH2,SEPEXCH2,OCTEXCH2,NOVEXCH2,DECEXCH2    
                              endif
                    elseif (Juldays >= BegFiscCur & Juldays  <=  EndFiscCur)
                              if (OSTAT = "B" or OSTAT = "0")
                                        clear TMPVAR
                                        if (oexqty > "0")
                                                  move oexqty to n9
                                                  move      oqty to n10
                                                  sub n9 from n10,TMPVAR 
                                                  move      TMPVAR to RQTY
                                                  move      n9 to EXQTY
                                                            add       EXQTY to EXCHTOT1 
                                                            add       EXQTY to ORDTOT1
                                                            add       RQTY to RENTTOT1 
                                                            add       RQTY to ORDTOT1
                                        else
                                                  reset excodes
                                                  scan oelcode in excodes
                                                  if equal
                                                            move oqty to EXQTY
                                                            add       EXQTY to EXCHTOT1
                                                            add       EXQTY to ORDTOT1
                                                  else
                                                            move oqty to RQTY
                                                            add       RQTY to RENTTOT1
                                                            add       RQTY to ORDTOT1
                                                  endif
                                        endif
                                                  call      cvtgreg   
.For Fiscal
                                                  If (FiscMonth <> c1)
                                                            If (fiscmonth = 2)
                                                                      load      MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
                                                            elseif (fiscmonth = 3) 
                                                                      load      MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
                                                            elseif (fiscmonth = 4) 
                                                                      load      MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
                                                            elseif (fiscmonth = 5) 
                                                                      load      MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
                                                            elseif (fiscmonth = 6) 
                                                                      load      MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
                                                            elseif (fiscmonth = 7) 
                                                                      move mm to n2
                                                                      load      MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
                                                            elseif (fiscmonth = 8) 
                                                                      load      MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
                                                            elseif (fiscmonth = 9) 
                                                                      load      MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
                                                            elseif (fiscmonth = 10) 
                                                                      load      MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
                                                            elseif (fiscmonth = 11) 
                                                                      load      MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
                                                            elseif (fiscmonth = 12) 
                                                                      load      MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
                                                            endif
                                                  EndIf
                                                  move      mm to n2
;Qty 
                                                  clear dumqty
                                                  clear n9
                                                  clear str15
                                                  load      str15,n2,JAN1,FEB1,MAR1,APR1,MAY1,JUN1,JUL1,AUG1,SEP1,OCT1,NOV1,DEC1  
                                                  move      str15 to dumqty
                                                  move      oqty to n9
                                                  add       N9 to dumqty
                                                  move dumqty to str15
                                                  store     str15,n2,JAN1,FEB1,MAR1,APR1,MAY1,JUN1,JUL1,AUG1,SEP1,OCT1,NOV1,DEC1  
;Rental
                                                  clear dumqty
                                                  clear n9
                                                  clear str15
                                                  load      str15,n2,JANRENT1,FEBRENT1,MARRENT1,APRRENT1,MAYRENT1,JUNRENT1,JULRENT1,AUGRENT1,SEPRENT1,OCTRENT1,NOVRENT1,DECRENT1    
                                                  move      str15 to dumqty
                                                  move      rqty to n9
                                                  add       n9 to dumqty
                                                  move dumqty to str15
                                                  store     str15,n2,JANRENT1,FEBRENT1,MARRENT1,APRRENT1,MAYRENT1,JUNRENT1,JULRENT1,AUGRENT1,SEPRENT1,OCTRENT1,NOVRENT1,DECRENT1    
;Exch
                                                  clear dumqty
                                                  clear n9
                                                  clear str15
                                                  load      str15,n2,JANEXCH1,FEBEXCH1,MAREXCH1,APREXCH1,MAYEXCH1,JUNEXCH1,JULEXCH1,AUGEXCH1,SEPEXCH1,OCTEXCH1,NOVEXCH1,DECEXCH1    
                                                  move      str15 to dumqty
                                                  move      exqty to n9
                                                  add       n9 to dumqty
                                                  move dumqty to str15
                                                  store     str15,n2,JANEXCH1,FEBEXCH1,MAREXCH1,APREXCH1,MAYEXCH1,JUNEXCH1,JULEXCH1,AUGEXCH1,SEPEXCH1,OCTEXCH1,NOVEXCH1,DECEXCH1    
                              Endif
                    endif
SKIP
                    call INVOICEPART
.
.
          repeat 
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       OneandahalfSpaced to row
        move mask20 to dim20a
        edit JAN1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit FEB1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit MAR1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit APR1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit MAY1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit JUN1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row                                                           
        move mask20 to dim20a
        edit JUL1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit AUG1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit SEP1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit OCT1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit NOV1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit DEC1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;

;RentVolumeYear1
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       OneandahalfSpaced to row
        move mask20 to dim20a
        edit JANRENT1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit FEBRENT1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit MARRENT1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit APRRENT1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit MAYRENT1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit JUNRENT1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit JULRENT1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit AUGRENT1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit SEPRENT1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit OCTRENT1 to dim20a         
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit NOVRENT1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit DECRENT1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;

;ExchVolumeYear1
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       "40" to row
        move        mask20 to dim20a
        edit        JANEXCH1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*bgcolor=*YELLOW,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move        mask20 to dim20a
        edit        FEBEXCH1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move        mask20 to dim20a
        edit        MAREXCH1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move        mask20 to dim20a
        edit        APREXCH1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move        mask20 to dim20a
        edit        MAYEXCH1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move        mask20 to dim20a
        edit        JUNEXCH1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move        mask20 to dim20a
        edit        JULEXCH1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move        mask20 to dim20a
        edit        AUGEXCH1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move        mask20 to dim20a
        edit        SEPEXCH1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move        mask20 to dim20a
        edit        OCTEXCH1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move        mask20 to dim20a
        edit        NOVEXCH1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move        mask20 to dim20a
        edit        DECEXCH1 to dim20a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
;Projections
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       OneandahalfSpaced to row
ProjJAN1
        move mask19 to dim19a
        edit ProjJAN1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*bgcolor=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjFEB1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjMAR1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjAPR1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjMAY1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjJUN1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row       
        move mask19 to dim19a
        edit ProjJUL1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjAUG1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjSEP1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjOCT1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjNOV1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjDEC1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
;Revised Projections
;Projections
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       OneandahalfSpaced to row
        move mask19 to dim19a
        edit ProjJAN2 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjFEB2 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjMAR2 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjAPR2 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjMAY2 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjJUN2 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row       
        move mask19 to dim19a
        edit ProjJUL2 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjAUG2 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjSEP2 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjOCT2 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjNOV2 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit ProjDEC2 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
;Actual
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       OneandahalfSpaced to row
        move mask19 to dim19a
        edit AP1JAN1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1FEB1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1MAR1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1APR1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1MAY1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1JUN1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row                                                           
        move mask19 to dim19a
        edit AP1JUL1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1AUG1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1SEP1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1OCT1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1NOV1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1DEC1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
;Previous Year
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       OneandahalfSpaced to row
        move mask20 to dim20a
        edit JAN2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit FEB2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit MAR2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit APR2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit MAY2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit JUN2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row                                                           
        move mask20 to dim20a
        edit JUL2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit AUG2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit SEP2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit OCT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit NOV2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit DEC2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;

;RentVolumeYear2
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       OneandahalfSpaced to row
        move mask20 to dim20a
        edit JANRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,JANRENT2;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit FEBRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,FEBRENT2;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit MARRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,MARRENT2;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit APRRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,APRRENT2;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit MAYRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,MAYRENT2;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit JUNRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,JUNRENT2;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit JULRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,JULRENT2;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit AUGRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,AUGRENT2;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit SEPRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,SEPRENT2;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit OCTRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,OCTRENT2;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit NOVRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,NOVRENT2;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit DECRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,DECRENT2;

;ExchVolumeYear2
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       "40" to row
        move mask20 to dim20a
        edit JANEXCH2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*bgcolor=*YELLOW,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit FEBEXCH2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit        MAREXCH2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit APREXCH2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit MAYEXCH2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit JUNEXCH2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit JULEXCH2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit AUGEXCH2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit SEPEXCH2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit OCTEXCH2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit NOVEXCH2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit DECEXCH2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
;Previous Year Acutals
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       OneandahalfSpaced to row
        move mask19 to dim19a
        edit AP1JAN2 to dim19a
          prtpage MONTHINCREPORT;*pBoxSixRightText:row,*bgcolor=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1FEB2 to dim19a
          prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1MAR2 to dim19a
          prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1APR2 to dim19a
          prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1MAY2 to dim19a
          prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1JUN2 to dim19a
          prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row       
        move mask19 to dim19a
        edit AP1JUL2 to dim19a
          prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1AUG2 to dim19a
          prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1SEP2 to dim19a
          prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1OCT2 to dim19a
          prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1NOV2 to dim19a
          prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1DEC2 to dim19a
          prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;


Totals ;End Of Page
                    add       "40" to Totboxrow
                  move mask20 to dim20a
                  edit EXCHTOT1 to dim20a
                    prtpage MONTHINCREPORT;*pBoxTwoRightText:TotBoxRow,*bgcolor=*YELLOW,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim20a;
                  move mask20 to dim20a
                  edit EXCHTOT2 to dim20a
                    prtpage MONTHINCREPORT;*pBoxFiveRightText:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim20a;
                    sub       "40" to Totboxrow

                    add       halfsmboxheight to Totboxrow
                    add       "40" to Totboxrow
                  move mask20 to dim20a
                  edit ORDTOT1 to dim20a
                    prtpage MONTHINCREPORT;*pBoxTwoVertText:TotBoxRow,*bgcolor=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim20a;
                  move mask20 to dim20a
                  edit RENTTOT1 to dim20a
                    prtpage MONTHINCREPORT;*pBoxTwoRightText:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim20a;
                    clear     DIM20a
                  move mask19 to dim19a
          edit ProjTot1 to dim19a
                    prtpage MONTHINCREPORT;*pBoxThreeVert1Text:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,Dim19a;
                    clear     DIM20a
                  move mask19 to dim19a
          edit ProjTot2 to dim19a
                    prtpage MONTHINCREPORT;*pBoxThreeVert2Text:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,Dim19a;
                  move mask19 to dim19a
                  edit AP1TOT1 to dim19a
                    prtpage MONTHINCREPORT;*pBoxThreeRightText:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim19a;
                  move mask20 to dim20a
                  edit ORDTOT2 to dim20a
                    prtpage MONTHINCREPORT;*pBoxFiveVertText:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim20a;
                  move mask20 to dim20a
                  edit RENTTOT2 to dim20a
                    prtpage MONTHINCREPORT;*pBoxFiveRightText:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim20a;
                  move mask19 to dim19a
                  edit AP1TOT2 to dim19a
                    prtpage MONTHINCREPORT;*pBoxSixRightText:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim19a;
          sub       halfsmboxheight to Totboxrow
          sub       "40" to Totboxrow
;;
.patch1.0
.patch1.1 Open Payables
          move      todayis to juldays
          call      cvtgreg
          pack      str10 with mm,"/",dd,"/",cc,yy
.         clock timestamp,TIMESTAMP
.         unpack    timestamp,str4,str2,dd
.         call      trim using str3
.         pack      str10 with str2,"/",dd,"/",str4

          if (LTYPE = "C")
.                   Move row to N9
                    move      TotBoxRow to Row
                    add       smboxheight to ROW
                    add       singlespaced to Row
                  move        mask19 to dim19a
          edit      OPENPAY to dim19a
                    call      trim using          dim19a
                    pack Taskname with "Payables = ",dim19a, " as of ",str10
                    prtpage MONTHINCREPORT;*pBoxOneLeft:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,taskname;
          else
                    move      TotBoxRow to Row
                    add       smboxheight to ROW
                    add       singlespaced to Row
                    pack Taskname with "*As of ",str10
                    prtpage MONTHINCREPORT;*pBoxOneLeft:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,taskname;
          endif

.patch1.2
          if (ManualFlag <> YES)
;;

.patch1.3
READIT1
                    READ      INCLISTS,SEQ;LVARS
                    GOTO endprogram IF OVER
                    GOTO READIT1 IF (REP1 <> "M")
          DISPLAY     *P10:10,*EF,*white,"List:  ",LNUM,"   ",OLSTNAME
                  prtpage   MONTHINCREPORT;*NEWPAGE:
                              *ORIENT=*LANDSCAPE
                    CALL CLEARVARS
                    GOTO READIT
.patch1.3
          ENDIF
          
ENDPROGRAM
          prtclose  MONTHINCREPORT
          prtplay             PRTFILENAME,"PDF995"
.patch1.4
          if (manualflag <> YES)
                    pack str55,PRINTNAME,".pdf"
                    clear taskname
                    call getwinver
                    If        (osflag = c1 | osflag = c5)
                              append  "!c:\winnt\system32\cmd.exe /c ",taskname
                    ElseIf    (osflag = c3 | osflag = c4)
                              append  "!c:\command.com /c ",taskname
                    ElseIf    (osflag = c6)
                              append  "!c:\windows\system32\cmd.exe /c ",taskname
                    Endif
                    append  " copy ",taskname
                    append  "c:\work\pdf\",taskname
                    append  str55,taskname
                    append  " ",taskname
                    append  NTWKPATH5,taskname
                    append  "INCOME\",taskname
                    append  "SOI",taskname
                    append  str55,taskname
                    reset     taskname
                    execute   taskname
          endif
.endpatch1.4
          stop
.patch1.0
IncFooter

InvoicePart
                    MOVE      OLRN TO NINVFLD
                    If        (OSTAT = "B" | OSTAT="Q")
                              move      c1 to ninvpath
                              CALL      NINVKEY

                              if Not OVER
;JDpatch
                                        CMATCH    "0" TO STATB                ;Open and still have to check for "14" Adjustment
;For invoice accrued basis
                                        If Equal
;For invoice Date
.patch1.1 Open Payables
.                                                 if (LTYPE = "I")
.patch1.1 Open Payables
;;;;;;;;;;;;;
;Compute
                                                  if (ap2 <= c0)
                                                            MOVE      YES TO SUBPPSW
                                                            MOVE      OLRN to nmrgfld
                                                            REP       ZFILL IN NMRGFLD
                                                            move      c0 to nmrgrqty
                                                            move      c0 to nmrgiqty
                                                            move      c0 to nmrgnet
                                                            move      no to mrgsw
                                                  move      no to shipsw
                                                            CALL      NMRGKEY
                                                            if        not over
                                                                      move      yes to mrgsw
                                                                      endif
                                                                      MOVE      NordFLD to nshpfld
                                                                      REP       ZFILL IN NshpFLD
                                                                      CALL      NshpKEY
                                                                      if        not over
                                                                                move      yes to shipsw
                                                                      endif
                                                                      call      wipecvars
                                                                      move      c1 to ndatpath
                                                                      move      olnum to ndatfld
                                                                      call      ndatkey
                                                                      move      lrn to nshpfld
                                                            call      nshpkey
               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
;               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
                                                                      CALL      COMPUTE
                                                                      move      ap to ap1
;;;;;;;;;;C   - may have to use this hold for master adjustment reconcile
                                                                      clear     holdap1
                                                                      move      ap to holdap1
;;;;;;;;;;C
                                                            else
                                                                      move ap2 to ap1
;;;;;;;;;;C   - may have to use this hold for master adjustment reconcile
                                                                      clear     holdap1
                                                                      move      ap2 to holdap1
;;;;;;;;;;C
                                                             endif
.patch1.1 Open Payables
.                                                 endif
.patch1.1 Open Payables
;Compute
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
                                                  move      "01" to n2
                                                  move      n2 to str2
                                                  rep       zfill in str2
                                                  CLEAR     NJSTFLD
                                                  PACK      NJSTFLD FROM INVNUM,str2
                                                  rep       zfill in njstfld
                                                  clear     num1022
;For Invoice Accrual Basis
.patch1.1 Open Payables
.                                       if (LTYPE = "I")
.patch1.1 Open Payables
                                                  CALL      NJSTKEY
                                                  if Not Over
;COmpute
                                                            if (ap2 <= c0)
;;;;;;;;;;C                                                                               add jstap1 to ap1
;master adj check
                                                                      add       jstap1 to num1022
;master adj check

                                                            else
;;;;;;;;;;C                                                                               add jstap2 to ap1
;master adj check
                                                                      add       jstap2 to num1022
;master adj check
                                                            endif
                                                  else
.Patch1.1 Open Payables
                                                            if (LTYPE = "C")
                                                                      add ap1 to OPENPAY
                                                                      CALL      NJSTKEY
                                                                      RETURN    if OVER
                                                            else
.Patch1.1                                         
                                                                      goto Buckets
.patch1.1
                                                            endif
.patch1.1
                                                  endif
;COmpute
.Patch1.1 Open Payables
.                                       else
.Patch1.1 Open Payables
;For Invoice Accrual Basis
.Patch1.1 Open Payables
.                                                           CALL      NJSTKEY
.                                                           RETURN    if OVER
.Patch1.1 Open Payables
;;;;;;;;;;Invoice Accrual
.Patch1.1 Open Payables
.                                                 endif
.Patch1.1 Open Payables
;For Invoice Accrual Basis

                                                  MATCH     "14" TO JSTREASN
                                                  IF      EQUAL
                                                            move jstap1 to ap1
                                                            mult seq to ap1
                                                            if (ap1 < c0)
                                                                      mult seq by ap1
                                                            endif
.Patch1.1 Open Payables
                                                            add ap1 to OPENPAY
.                                                           if (LTYPE = "C")
.                                                                               return
.                                                           endif
.Patch1.1
;Grab Adj Date - 
                                                            unpack JSTDATE,str2,YY,MM,DD
                                                            move      MM to n2
                                                            Goto      ManualBuckets
                                                  ENDIF
.Patch 1.7.1 Logic Added
                                                  clear n2
                                                  for n2,"2","9"
.                                                 loop
.                                                           add       c1 to n2
.Patch 1.7.1 Logic Added
                                                            move      n2 to str2
                                                            rep       zfill in str2
                                                            CLEAR     NJSTFLD
                                                            PACK      NJSTFLD FROM INVNUM,str2
                                                            rep       zfill in njstfld
                                                            CALL      NJSTKEY
.Patch 1.7.1 Logic Added
.                                                 until over
.Patch 1.7.1 Logic Added
;For Invoice Accrual 
.Patch1.1 Open Pay
.                                                           if (LTYPE = "I")
.Patch1.1 Open Pay
                                                            if (ap2 <= c0)
;;;;;;;;;;C                                                                               add jstap1 to ap1
;master adj check
                                                                      add       jstap1 to num1022
;master adj check
                                                            else
;;;;;;;;;;C                                                                               add jstap2 to ap1
;master adj check
                                                                      add       jstap2 to num1022
;master adj check
                                                            endif
.Patch1.1 Open Pay
.                                                           endif
.Patch1.1 Open Pay
;For INvoice Accrual

                                                          MATCH     "14" TO JSTREASN
                                                            IF      EQUAL
                                                                      move jstap1 to ap1
                                                                      mult seq to ap1
                                                                      if (ap1 < c0)
                                                                                mult seq by ap1
                                                                      endif
.Patch1.1 Open Payables
                                                                      add ap1 to OPENPAY
.                                                                     if (LTYPE = "C")
.                                                                                         return
.                                                                     endif
.Patch1.1
;Grab Adj Date - 
                                                                      unpack JSTDATE,str2,YY,MM,DD
                                                                      move      MM to n2
                                                                      Goto      ManualBuckets
                                                            ENDIF
                                                  repeat
;For invoice accrual
.patch1.1 Open Payables
.                                       if (LTYPE = "I")
.patch1.1 Open Payables
;;;;;;;;;;C
                                                  if (ap2 <= c0)
;;;;;;;;;;C
                                                            move holdap1 to ap1
                                                            MOVE      OLRN TO NADJFLD
                                                            REP       ZFILL IN NADJFLD
                                                            CALL      NADJKEY
                                                            add       aspayad1 to ap1
                                                  else
                                                            move   holdap1 to ap1
                                                            MOVE      OLRN TO NADJFLD
                                                            REP       ZFILL IN NADJFLD
                                                            CALL      NADJKEY
                                                            add       aspayad2 to ap1
;;;;;;;;;;C                                                         
                                                  endif
;;;;;;;;;;C

                                                  return if (ap1 < 0)
.Patch1.1 Open Payables
                                                  add ap1 to OPENPAY
                                                  if (LTYPE = "C")
                                                            return
                                                  endif
.Patch1.1
                                                  goto      Buckets
.patch1.1 Open Payables
                                        endif
.patch1.1 Open Payables
;for invoice accrual
.patch1.1 Open Payables
.                             endif
.patch1.1 Open Payables
                                        CMATCH    "P" TO STATB                ;PAID
                                        If Equal
;;;;;;;;;;;;;
;Compute
                                                  if (ap2 <= c0)
                                                            MOVE      YES TO SUBPPSW
                                                  MOVE      OLRN to nmrgfld
                                                  REP       ZFILL IN NMRGFLD
                                                  move      c0 to nmrgrqty
                                                  move      c0 to nmrgiqty
                                                  move      c0 to nmrgnet
                                                  move      no to mrgsw
                                                  move      no to shipsw
                                                  CALL      NMRGKEY
                                                  if        not over
                                                            move      yes to mrgsw
                                                  endif
                                                  MOVE      NordFLD to nshpfld
                                                  REP       ZFILL IN NshpFLD
                                                  CALL      NshpKEY
                                                  if        not over
                                                            move      yes to shipsw
                                                  endif
                                                  call      wipecvars
                                                  move      c1 to ndatpath
                                                  move      olnum to ndatfld
                                                   call      ndatkey
                                                  move      lrn to nshpfld
                                                  call      nshpkey
                                                  CALL      COMPUTE
                                                            move      ap to ap1
;;;;;;;;;;C   - may have to use this hold for master adjustment reconcile
                                                            clear     holdap1
                                                            move      ap to holdap1
;;;;;;;;;;C
                                                   else
                                                            move ap2 to ap1
                                                   endif
;Compute
;;;;;;;;;;;;;;
                                                  move      "01" to n2
                                                  move      n2 to str2
                                                  rep       zfill in str2
                                                  CLEAR               NUM1022
                                                  CLEAR     NJSTFLD
                                                  PACK      NJSTFLD FROM INVNUM,str2
                                                  rep       zfill in njstfld
                                                  CALL      NJSTKEY
                                                  if not over
;Compute
                                                            if (ap2 <= c0)
.....
                                                                      if (JSTREASN <> "14")    ..add for exception test
.....
                                                                                add jstap1 to ap1
.....
                                                                      endif                      .add for exception test
;master adj check
                                                                                add       jstap1 to num1022
;master adj check
.....
                                                            else
                                                                      add jstap2 to ap1
;master adj check
                                                                                add       jstap2 to num1022
;master adj check
                                                            endif
;Compute
                                                            
;Comment out for exception Test
.....                                                     MATCH     "14" TO JSTREASN
.....                                                     IF      EQUAL
.....                                                                 move jstap1 to ap1
.....                                                                 mult seq to ap1
.....                                                                 if (ap1 < c0)
.....                                                                           mult seq by ap1
.....                                                                 endif
;Grab Adj Date - 
.....                                                                 unpack JSTDATE,str2,YY,MM,DD
.....                                                                 move      MM to n2
.....                                                                 Goto      ManualBuckets
.....                                                       ENDIF
;Comment out for exception Test
.Patch 1.7.1 Logic Added
                                                  clear n2
                                                  for n2,"2","9"
.                                                           loop
.                                                                     add       c1 to n2
.Patch 1.7.1 Logic Added
                                                                      move      n2 to str2
                                                                      rep       zfill in str2
                                                                      CLEAR     NJSTFLD
                                                                      PACK      NJSTFLD FROM INVNUM,str2
                                                                      rep       zfill in njstfld
                                                                      CALL      NJSTKEY
.Patch 1.7.1 Comment Out
.                                                           until over
.Patch 1.7.1 Comment Out
;compute
                                                                      if (ap2 <= c0)
.....
                                                                                if (JSTREASN <> "14")               ..add for exception test
.....
                                                                                          add jstap1 to ap1
.....
                                                                                endif                                .add for exception test
;master adj check
                                                                                add       jstap1 to num1022
;master adj check
.....
                                                                      else
                                                                                add jstap2 to ap1
;master adj check
                                                                                add       jstap2 to num1022
;master adj check
                                                                      endif
;compute
.....                                                               MATCH     "14" TO JSTREASN
.....                                                               IF      EQUAL
.....                                                                           move jstap1 to ap1
.....                                                                           mult seq to ap1
.....                                                                           if (ap1 < c0)
.....                                                                                     mult seq by ap1
.....                                                                           endif
;Grab Adj Date - 
.....                                                                 unpack JSTDATE,str2,YY,MM,DD
.....                                                                 move      MM to n2
.....                                                                 Goto      ManualBuckets
.....                                                                 ENDIF
                                                            repeat

.....Code Added for exception test of the infamous "14"
;;;;;;;;;;;;;;
                                                  move      "01" to n2
                                                  move      n2 to str2
                                                  rep       zfill in str2
                                                  CLEAR     NJSTFLD
                                                  PACK      NJSTFLD FROM INVNUM,str2
                                                  rep       zfill in njstfld
                                                  CALL      NJSTKEY
                                                  if not over
;Compute
;Added code for exception Test
                                                            if (ap2 <= c0)
                                                                      clear num102
                                                                    MATCH     "14" TO JSTREASN
                                                                    IF      EQUAL
                                                                                move jstap1 to num102
...........
                                                                      ENDIF
..........
.Patch 1.7.1 Logic Added
                                                  clear n2
                                                  for n2,"2","9"
.                                                                     loop
.                                                                               add       c1 to n2
.Patch 1.7.1 Logic Added
                                                                                move      n2 to str2
                                                                                rep       zfill in str2
                                                                                CLEAR     NJSTFLD
                                                                                PACK      NJSTFLD FROM INVNUM,str2
                                                                                rep       zfill in njstfld
                                                                                CALL      NJSTKEY
.Patch 1.7.1 Comment Out
.                                                                     until over
.Patch 1.7.1 Comment Out
                                                                                          if (JSTREASN = "14")
                                                                                                    add jstap1 to num102
                                                                                          endif
                                                                      repeat
                                                                      mult seq to num102
                                                                      if (num102 < c0)
                                                                                mult seq by num102
                                                                      endif
;;;;;;;;;;C
                                                                      move    holdap1 to ap1
                                                                      MOVE      OLRN TO NADJFLD
                                                                      REP       ZFILL IN NADJFLD
                                                                      CALL      NADJKEY
                                                                      add       aspayad1 to ap1
;;;;;;;;;;C
                                                                      sub       num102 from ap1,num1021
                                                                      if (num1021 <> c0)
                                                                                add num102,ap1
;;;;;;;;;;C
                                                                      else
                                                                                move holdap1 to ap1
                                                                                MOVE      OLRN TO NADJFLD
                                                                                REP       ZFILL IN NADJFLD
                                                                                CALL      NADJKEY
                                                                                add       aspayad1 to ap1
;;;;;;;;;;C
                                                                      endif
;;;;;;;;;;C
.                                                                     else
..
                                                                      if (num102 = c0)
..
                                                                                move holdap1 to ap1
                                                                                MOVE      OLRN TO NADJFLD
                                                                                REP       ZFILL IN NADJFLD
                                                                                CALL      NADJKEY
                                                                                add       aspayad1 to ap1
                                                                      endif
;;;;;;;;;;C
;;;;;;;;;;C
                                                            else
                                                                      move holdap1 to ap1
                                                                      MOVE      OLRN TO NADJFLD
                                                                      REP       ZFILL IN NADJFLD
                                                                      CALL      NADJKEY
                                                                      add       aspayad2 to ap1
;;;;;;;;;;C
                                                            endif
                                                  endif
                                        endif
......................................
Buckets
                                        move      CHK1DTEM to N2
                                        move      CHK1DTEM to MM
                                        move      CHK1DTED to DD
                                        move      CHK1DTEY to YY
ManualBuckets
;patchfor by invoice date
                                        if (LTYPE = "I")
                                                  move      INVDTEM to N2
                                                  move      INVDTEM to MM
                                                  move      INVDTED to DD
                                                  move      INVDTEY to YY
                                        endif
                                        call      CVTJUL
;;;Master Adjustment Check
                                        MOVE      OLRN TO NADJFLD
                                        REP       ZFILL IN NADJFLD
                                        CALL      NADJKEY
                                        if (AP2 <= C0)
                                                  COMPARE   ASPAYAD1,NUM1022
                                                  if NOT EQUAL
                                                            pack taskname,"The Maser AP1 Adj: ",ASPAYAD1," Does not Equal the detail Adj: ",NUM1022," For LR: ",OLRN
                                                            call emailtrouble
                                                  endif
                                        else
                                                  COMPARE   ASPAYAD2,NUM1022
                                                  if NOT EQUAL
                                                            pack taskname,"The Maser AP2 Adj: ",ASPAYAD2," Does not Equal the detail Adj: ",NUM1022," For LR: ",OLRN
                                                            call emailtrouble
                                                  endif
                                        endif
;;
;endpatch
;do not allow for neg
                                        return if (ap1 < 0)
.Patch1.3
                                        return if (Juldays > TODAYIS)
.Patch1.3
;IncomePart
                                                  if (Juldays >= BegFiscCur & Juldays  <=  EndFiscCur)
;
.For Fiscal
                                                            If (FiscMonth <> c1)
                                                                      If (fiscmonth = 2)
                                                                                load      MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
                                                                      elseif (fiscmonth = 3) 
                                                                                load      MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
                                                                      elseif (fiscmonth = 4) 
                                                                                load      MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
                                                                      elseif (fiscmonth = 5) 
                                                                                load      MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
                                                                      elseif (fiscmonth = 6) 
                                                                                load      MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
                                                                      elseif (fiscmonth = 7) 
                                                                                move mm to n2
                                                                                load      MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
                                                                      elseif (fiscmonth = 8) 
                                                                                load      MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
                                                                      elseif (fiscmonth = 9) 
                                                                                load      MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
                                                                      elseif (fiscmonth = 10) 
                                                                                load      MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
                                                                      elseif (fiscmonth = 11) 
                                                                                load      MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
                                                                      elseif (fiscmonth = 12) 
                                                                                load      MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
                                                                      endif

                                                            EndIf
                                                            move mm to n2
.
                                                            clear dumqty
                                                            clear n9
                                                            clear str15
                                                            load      num102,n2,AP1JAN1,AP1FEB1,AP1MAR1,AP1APR1,AP1MAY1,AP1JUN1,AP1JUL1,AP1AUG1,AP1SEP1,AP1OCT1,AP1NOV1,AP1DEC1     
                                                            add ap1 to num102
                                                            add       ap1 to AP1TOT1
                                                            store     num102,n2,AP1JAN1,AP1FEB1,AP1MAR1,AP1APR1,AP1MAY1,AP1JUN1,AP1JUL1,AP1AUG1,AP1SEP1,AP1OCT1,AP1NOV1,AP1DEC1     
                                                  elseif ((Juldays >= BegFiscPrev) & (Juldays  <=  EndFiscPrev))
;IncomePart
.For Fiscal
                                                            If (FiscMonth <> c1)
                                                                      If (fiscmonth = 2)
                                                                                load      MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
                                                                      elseif (fiscmonth = 3) 
                                                                                load      MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
                                                                      elseif (fiscmonth = 4) 
                                                                                load      MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
                                                                      elseif (fiscmonth = 5) 
                                                                                load      MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
                                                                      elseif (fiscmonth = 6) 
                                                                                load      MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
                                                                      elseif (fiscmonth = 7) 
                                                                                move mm to n2
                                                                                load      MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
                                                                      elseif (fiscmonth = 8) 
                                                                                load      MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
                                                                      elseif (fiscmonth = 9) 
                                                                                load      MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
                                                                      elseif (fiscmonth = 10) 
                                                                                load      MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
                                                                      elseif (fiscmonth = 11) 
                                                                                load      MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
                                                                      elseif (fiscmonth = 12) 
                                                                                load      MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
                                                                      endif
                                                            EndIf
                                                            move mm to n2
                                                            clear dumqty
                                                            clear n9
                                                            clear str15
                                                            load      num102,n2,AP1JAN2,AP1FEB2,AP1MAR2,AP1APR2,AP1MAY2,AP1JUN2,AP1JUL2,AP1AUG2,AP1SEP2,AP1OCT2,AP1NOV2,AP1DEC2     
                                                            add ap1 to num102
                                                            add       ap1 to AP1TOT2
                                                            store     num102,n2,AP1JAN2,AP1FEB2,AP1MAR2,AP1APR2,AP1MAY2,AP1JUN2,AP1JUL2,AP1AUG2,AP1SEP2,AP1OCT2,AP1NOV2,AP1DEC2     
                                                  Endif
                                        endif
                              ENDIF
                    Endif
                    return
CLEARVARS
          Clear BegFiscCur              
          Clear EndFiscCur              
          Clear BegFiscPrev             
          Clear EndFiscPrev             

          Clear JAN1                     
          Clear JAN2                     
          Clear FEB1                     
          Clear FEB2                     
          Clear MAR1                     
          Clear MAR2                     
          Clear APR1                     
          Clear APR2                     
          Clear MAY1                     
          Clear MAY2                     
          Clear JUN1                     
          Clear JUN2                     
          Clear JUL1                     
          Clear JUL2                     
          Clear AUG1                     
          Clear AUG2                     
          Clear SEP1                     
          Clear SEP2                     
          Clear OCT1                     
          Clear OCT2                     
          Clear NOV1                     
          Clear NOV2                     
          Clear DEC1                     
          Clear DEC2                     
          Clear OrdTOT1                  
          Clear OrdTOT2                  
          Clear JANRENT1                 
          Clear JANEXCH1                 
          Clear JANRENT2                 
          Clear JANEXCH2                 
          Clear FEBRENT1                 
          Clear FEBEXCH1                 
          Clear FEBRENT2                 
          Clear FEBEXCH2                 
          Clear MARRENT1                 
          Clear MAREXCH1                 
          Clear MARRENT2                 
          Clear MAREXCH2                 
          Clear APRRENT1                 
          Clear APREXCH1                 
          Clear APRRENT2                 
          Clear APREXCH2                 
          Clear MAYRENT1                 
          Clear MAYEXCH1                 
          Clear MAYRENT2                 
          Clear MAYEXCH2                 
          Clear JUNRENT2                 
          Clear JUNEXCH2                 
          Clear JUNRENT1                 
          Clear JUNEXCH1                 
          Clear JULRENT2                 
          Clear JULEXCH2                 
          Clear JULRENT1                 
          Clear JULEXCH1                 
          Clear AUGRENT2                 
          Clear AUGEXCH2                 
          Clear AUGRENT1                 
          Clear AUGEXCH1                 
          Clear SEPRENT2                 
          Clear SEPEXCH2                 
          Clear SEPRENT1                 
          Clear SEPEXCH1                 
          Clear OCTRENT2                 
          Clear OCTEXCH2                 
          Clear OCTRENT1                 
          Clear OCTEXCH1                 
          Clear NOVRENT2                 
          Clear NOVEXCH2                 
          Clear NOVRENT1                 
          Clear NOVEXCH1                 
          Clear DECRENT1                 
          Clear DECEXCH1                 
          Clear DECRENT2                 
          Clear DECEXCH2                 
          Clear EXCHTOT1                 
          Clear RENTTOT1                 
          Clear EXCHTOT2                 
          Clear RENTTOT2                 
;AP1
          Clear AP1JAN1                 
          Clear AP1JAN2                  
          Clear AP1FEB1                  
          Clear AP1FEB2                  
          Clear AP1MAR1                  
          Clear AP1MAR2                  
          Clear AP1APR1                  
          Clear AP1APR2                  
          Clear AP1MAY1                  
          Clear AP1MAY2                  
          Clear AP1JUN1                  
          Clear AP1JUN2                  
          Clear AP1JUL1                  
          Clear AP1JUL2                  
          Clear AP1AUG1                  
          Clear AP1AUG2                  
          Clear AP1SEP1                  
          Clear AP1SEP2                  
          Clear AP1OCT1                  
          Clear AP1OCT2                  
          Clear AP1NOV1                  
          Clear AP1NOV2                  
          Clear AP1DEC1                  
          Clear AP1DEC2                  
          Clear AP1TOT1                  
          Clear AP1TOT2                  
;Proj
          Clear ProjJAN1                 
          Clear ProjJAN2                 
          Clear ProjFEB1                 
          Clear ProjFEB2                 
          Clear ProjMAR1                 
          Clear ProjMAR2                 
          Clear ProjAPR1                 
          Clear ProjAPR2                 
          Clear ProjMAY1                 
          Clear ProjMAY2                 
          Clear ProjJUN1                 
          Clear ProjJUN2                 
          Clear ProjJUL1                 
          Clear ProjJUL2                 
          Clear ProjAUG1                 
          Clear ProjAUG2                 
          Clear ProjSEP1                 
          Clear ProjSEP2                 
          Clear ProjOCT1                 
          Clear ProjOCT2                 
          Clear ProjNOV1                 
          Clear ProjNOV2                 
          Clear ProjDEC1                 
          Clear ProjDEC2                 
          Clear ProjTOT1                 
          Clear ProjTOT2       
           
          CLEAR     OPENPAY
          CLEAR     DUMQTY    
          CLEAR     TMPVAR    
          CLEAR     RQTY      
          CLEAR     EXQTY     
          CLEAR     NUM102    
          CLEAR     AP1TOT    
          CLEAR     NUM1021   
          CLEAR     NUM1022   
          CLEAR     FISCMONTH 
          clear     totboxrow
          clear     totboxheight
          RETURN
EmailTrouble
                    move    "Houston We May have a problem",SmtpSubject Subject
;.   Set the text message that is send with the attachments
                    move    Taskname,SmtpTextMessage(1)   Array <Text message >
                    move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
                    move    "NTS4",SmtpEmailServer                   Address of email serverc
                    clear   smtpemailaddress
                    append  "Jduenas",SmtpEmailAddress
                    append  "@nincal.com",SmtpEmailAddress
                    reset   smtpemailaddress
                    move    "Jduenas",SmtpUserName                                User name
;   Set the destinations of the email. Max 100 (Mime spec)
                    move    smtpemailaddress,SmtpDestinations(1,1)
                    move    "Jduenas",SmtpDestinations(1,2)
                    move    "1",SmtpDestIndexLast                          originators UserName
                    move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
                    clear   SmtpLogFile                                         'Clear' disables the LogFile
                    move    "1",SmtpProgress                                    Enable progress bars
                    call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
                    if not equal
                            pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
                                "Status Code ",SmtpStatus," - ",SmtpStatusText
                            move    "Error Sending Message",SmtpSubject Subject
                            move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
                            call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
                    endif
                    return
          INCLUDE   NORDIO.INC
          INCLUDE   NDATIO.INC
;compute
          INCLUDE   NSHPIO.INC
          INCLUDE   NMRGIO.INC
          INCLUDE   NDAT3IO.INC
         include   nacdIO.inc

;compute
;begin patch 1.8    
;         INCLUDE   COMPUTE.INC
          INCLUDE   compute.inc
;         INCLUDE   NINVIO.INC
          INCLUDE   ninvio.inc
          INCLUDE   NINVACDIO.INC
;end patch 1.8      
          INCLUDE   NJSTIO.INC
          INCLUDE   NADJIO.INC
          INCLUDE   COMLOGIC.INC