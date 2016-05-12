.Standard Copy
.Do not touch
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
.patch1.5
          include   compdd.inc
          include   cntdd.inc
.         include   nmlrdd.inc
;patch1.5

.patch1.4
          include   NUSEDD.INC
.patch1.4
;Compute
          INCLUDE   NADJDD.INC
          INCLUDE   NORDDD.INC
;begin patch 1.9
;         INCLUDE   NINVDD.INC
          Include   NInvACddd.inc
          INCLUDE   ninvdd.inc
;end patch 1.9
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
Release   INIT      "1.91"              DMB       18JAN2006       Added code to show date and name of person who runs a manual income report
.Release  INIT      "1.9"               DLH       9March2005          Invoice Conversion
;Release  INIT      "1.8"               DMB       24JAN2004 Added code to use recipient var 
.Release  INIT      "1.7.2"    
.Release  INIT      "1.7.1"    DMB      04OCT2004 Added code to loop through ajustments until the 9th adj is read even if it doesn't exist.
.Release  INIT      "1.7"      ASH      06AUG2004 Logo Conversion
.Release  INIT      "1.61"     DMB 10AUG2004      added code to skip to next year when fiscal and current or greater than fiscal month
.Release  INIT      "1.6"      DMB 11JUN2004      added code to fix minor bugs/switch to pdf995
;Release  INIT      "1.5"      DMB 26MAY2004      Mailer COnversion
.Release  INIT      "1.4"     Separating clients to indiv pdf's email care to si and interfaith to sk
;Release  INIT      "1.0"     Initial Release     of BO Monthy income report for clients
SingleSpaced        FORM      "160"
OneandahalfSpaced   FORM      "240"
DoubleSpaced        FORM      "320"
LgBoxHeight         FORM      "6951"
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
BoxOneLeft          FORM      "1200"
BoxOneRight         FORM      "1700"
;BoxOneRight        FORM      "600"
BoxTwoLeft          FORM      "1750"
BoxTwoRight         FORM      "3850"
BoxTwoLeft1         FORM      "2800"
BoxTwoRight1        FORM      "3850"
BoxThreeLeft        FORM      "3900"
BoxThreeRight       FORM      "5000"
.BoxThreeRight      FORM      "6400"
.BoxFourLeft        FORM      "6600"
BoxFourLeft         FORM      "5500"
;BoxFourRight       FORM      "7200"
BoxFourRight        FORM      "6000"
.BoxFourRight       FORM      "7100"
;BoxFiveLeft        FORM      "7250"
BoxFiveLeft         FORM      "6050"
.BoxFiveLeft        FORM      "7150"
;BoxFiveRight       FORM      "9500"
BoxFiveRight        FORM      "8300"
.BoxFiveRight       FORM      "9400"
;BoxFiveLeft1       FORM      "8375"
.BoxFiveLeft1       FORM      "8275"
BoxFiveLeft1        FORM      "7175"
;BoxFiveRight1      FORM      "9500"
.BoxFiveRight1      FORM      "9400"
BoxFiveRight1       FORM      "8700"
;BoxSixLeft         FORM      "9550"
.BoxSixLeft         FORM      "9450"
BoxSixLeft          FORM      "8350"
;BoxSixRight        FORM      "10500"
.BoxSixRight        FORM      "10400"
.BoxSixRight        FORM      "8000"
BoxSixRight         FORM      "9550"
;List 
BoxTwoVert          FORM      "2800"
BoxThreeVert1       FORM      "5000"
BoxThreeVert2       FORM      "6200"
.BoxFiveVert        FORM      "8275"
BoxFiveVert         FORM      "7175"
;BoxFiveVert        FORM      "8375"
;For Text Align close to but not on Vertical Line
BoxTwoVertText                FORM      "2790"
BoxThreeVert1Text   FORM      "4990"
BoxThreeVert2Text   FORM      "6150"
.BoxFiveVertText              FORM      "8265"
BoxFiveVertText               FORM      "7165"
BoxTwoRightText     FORM      "3840"
BoxThreeRightText   FORM      "4990"
.BoxThreeRightText  FORM      "6390"
.BoxFiveRightText   FORM      "9390"
.BoxFiveRightText   FORM      "9390"
BoxFiveRightText    FORM      "8290"
.BoxSixRightText    FORM      "10390"
.BoxSixRightText    FORM      "7990"
BoxSixRightText     FORM      "9540"
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
.         div c3 in n9,n8
          div c2,n9,n8
.         div c2,n8
.         mult      c5,n8
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

;IncomeDate Vars
BegINCCur FORM      5
EndINCCur FORM      5
BegINCPrev          FORM      5
EndINCPrev          FORM      5

HoldLstName         Dim 55

;
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


;
DUMQTY    FORM      10
TMPVAR    FORM      10
RQTY      FORM      10
EXQTY     FORM      10
NUM102    FORM      10.2
AP1TOT    FORM      10.2
FiscMonth FORM      2
;
;Maksing Vars
mask20      init    "ZZZ,ZZZ,ZZZ,ZZZ,ZZ9"         ;formatting vars
Dim20a      dim     20            ;formatting vars
mask16      init    "Z,ZZZ,ZZZ,ZZZ.99"        ;formatting vars
Dim16a      dim     16            ;formatting vars
RECIPIENTHOLD       DIM       255

.patch1.1
.Patch1.2
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
.patch1.2


mask19      init    "($ZZZ,ZZZ,ZZZ.99)"        ;formatting vars
Dim19a      dim     19            ;formatting vars
OPENPAY   FORM      10.2
.>Patch 1.91 Var Added to keep track of Date Ran
DateRan dim         10
.>Patch 1.91 Var Added to keep track of Date Ran

NUM1021   FORM      10.2
NUM1022   FORM      10.2
HoldAP1   form 10.2
;For compute
mrgsw    dim       1
shipsw   dim       1
;compute
.patch1.3
TODAYIS   FORM      5
.START PATCH 1.7 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.Patch 1.8
Lyearsave DIM       4
.Patch 1.8
.END PATCH 1.7 ADDED LOGIC
          clock  timestamp,timestamp
          unpack timestamp,str2,yy,mm,dd
          call   cvtjul
          move   juldays  to TODAYIS
.>Patch 1.91 Code Added
          pack   DATERAN with mm,slash,dd,slash,str2,yy
.>Patch 1.91 Code Ended       
.Patch 1.8 Code Added
          pack lyearsave,Str2,yy
          move lyearsave to lyear
.Patch 1.8
          
.patch1.3

.patch1.4
          pack      PRINTNAME,timestamp
.endpatch1.4

.patch1.1

.patch1.4commentout
.         move "standinc.lst",PRINTNAME
;         move      "c:\work\standinc.lst" to prtfilename
          pack      prtfilename with "c:\work\pdf\",printname,".lst"
.patch1.4commentout
          OPEN      INCLISTS,"INCLISTS"

.patch1.4
          trap RetryPrint if SPOOL
.endpatch1.4
;patch1.6
          PRTOPEN MONTHINCREPORT,"PDF995",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
;patch1.6
.         PRTOPEN MONTHINCREPORT,"Acrobat Distiller",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
          PRTPAGE MONTHINCREPORT;*UNITS=*HIENGLISH:
                          *ORIENT=*LANDSCAPE;
.patch1.4
          TrapClr   SPOOL
.endpatch1.4
.patch1.1
;Get Fiscal Year
.patch1.1
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
.Patch 1.8 Code Added
                              pack lyearsave,Str2,yy
                              move lyearsave to lyear
.Patch 1.8
          call      cvtjul
          move juldays  to TODAYIS
          goto DateIS
.patch1.3

WhichWay
        MOVE      "A" to str1
        KEYIN     *ES,*P10:8,*EF,"M",*white,"anual or ",*cyan,"(A)uto: ",*T15,*RV,STR1
          if (str1 = "M")
                    move YES to ManualFlag
                    move "M" to REP1
NewList
                  KEYIN     *P10:10,*EF,*white,"List:  ",LNUM
                    GOTO      NEWLIST IF EOS
                    if (LNUM = "")
                              goto NewList
                    else
                    call      zfillit using lnum
                    move lnum to NDATFLD
;patch1.4
                              call zfillit using ndatfld
;patch1.4
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
         KEYIN     *P10:14,*EF,"Maildate Or ",*cyan,"(O)","rder date: ",*RV,DATEBY
                    goto      getdate if ((DATEBY <> "M") and (DATEBY <> "O"))

Basis     
         KEYIN     *P10:14,*EF,*white,"Accrual (By ",*CYAN,"[I]",*white,"nvoice) Or ","Cash (",*cyan,"[C]",*White,"heck Date) Basis: ",*RV,LTYPE
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
          GOTO READITa IF (REP1 <> "M")
.         GOTO READIT IF (REP1 <> "M")
        DISPLAY     *P10:10,*EF,*white,"List:  ",LNUM,"   ",OLSTNAME
READIT
.Patch 1.9
          move RECIPIENT to RECIPIENTHOLD
.Patch 1.9
.patch1.3
.Patch 1.8 logic replaced
          move lyearsave to lyear
.patch 1.8
.Patch 1.8 Comment Out
..        clock timestamp,timestamp
..        unpack timestamp,str4,n2
..        move str4 to LYEAR
.Patch1.61
.         if (Lmonth <> C1)
.                   if (n2 >= LMONTH)
.                             add c1 to lyear
.                   endif
.         endif
.Patch1.61
.Patch 1.8 Comment Out
.         if (LMONTH < n2)
.                   add c1 to lyear
.         endif 
.For Fiscal
MANUALLIST
                    move LMONTH to fiscmonth
                    MOVE LMONTH TO MM
                    MOVE      "01" TO DD
.Patch1.5 Subtract a year if fiscal.
.Patch 1.8 Comment Out
.                   if (Lmonth <> C1)
.                             move LYEAR to n4
.                             sub c1 from n4
.                             move n4 to LYEAR
.                   endif
.Patch 1.8 Comment Out
.Patch1.5
                    unpack LYEAR,str2,YY
.patch1.2
.                   clock timestamp,timestamp     
.                   UNPACK TIMESTAMP,STR4
.                   move str4 to LYEAR
.                   MOVE  "07" to LMONTH
.                   MOVE "011507" to LNUM
.                   move "I" to LTYPE

 
.patch1.1
.For Fiscal
.patch1.2
.                   move LMONTH to fiscmonth
.                   MOVE LMONTH TO MM
.                   MOVE      "01" TO DD
.                   unpack LYEAR,str2,YY
.patch1.2
;Year 1
                    call      cvtjul
.Patch 1.8 Added Logic
;Patch 3.8  replacement for code about
;If this is a fiscal year report and todayis[report date] is less than latest fiscal date than do not add add another year to it
;i.e. if the fiscal month is jul 04 and the report date is mar 04 then we must sub a year for jul 03 - jun 04 FY
;i.e. if the fiscal month is jul 04 and the report date is aug 04 then we leave alone to create jul 04 - jun 05 FY
                    if (Lmonth <> C1)
                              if (todayis < juldays)
                                        unpack LYEAR,str2,n2
                                        sub c1 from n2
                                        move n2 to YY
                                        rep zfill,YY
                                        call      cvtjul
                              endif
                    endif 
.Patch 1.8 Added Logic
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
          move olstname to  HoldLstName
.patch1.1


IncHeader
.         PRTOPEN MONTHINCREPORT,"Acrobat Distiller",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
.         PRTPAGE MONTHINCREPORT;*UNITS=*HIENGLISH:
.                          *ORIENT=*LANDSCAPE;
;         prtpage MONTHINCREPORT;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font8,"Date:";
;         clock   timestamp,str8
;         unpack  str8,str2,yy,mm,dd
;         clear   str10
;         pack    str10,mm,slash,dd,slash,str2,yy
;         prtpage MONTHINCREPORT;*font=font8,str10;
.START PATCH 1.7 REPLACED LOGIC
.         prtpage MONTHINCREPORT;*p0:0,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,*ulon,*boldon,"Names ",*boldoff;
..        prtpage MONTHINCREPORT;*p0:7750,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,*ulon,*boldon,"Names ",*boldoff;
.         prtpage MONTHINCREPORT;*font=TimesNew10I,*boldon,*ulon,"in the News ",*uloff,*boldoff;
.         add singlespaced to row
.         prtpage MONTHINCREPORT;*p0:160,*font=TimesNew6,*ll,"C A L I F O R N I A   I N C .";
..        prtpage MONTHINCREPORT;*p0:7910,*font=TimesNew6,*ll,"C A L I F O R N I A   I N C .";
          add singlespaced to row
          prtpage   MONTHINCREPORT;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.7 REPLACED LOGIC

          move "500" to row
          call trim using Olstname
          prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial14,*ll,*boldon,OLSTNAME,*boldoff;
.         prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial14,*ll,*boldon,"INTERFAITH ALLIANCE",*boldoff;
.         prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial14,*ll,*boldon,"CARE",*boldoff;
          add       OneandahalfSpaced to row
          if (LTYPE = "I")
.         if (LSTNUM = "011507")
                    prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,"MONTHLY LIST INCOME/VOLUME REPORT - REPORTED ON AN ACCRUED BASIS (BY INVOICE DATE)";
          else
                    prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,"MONTHLY LIST INCOME/VOLUME REPORT - REPORTED ON A CASH BASIS (BY CHECK DATE)";

          endif
          add       Doublespaced to row
;Header2
          move BegFiscCur to juldays 
          call      cvtgreg
          pack      str10 with mm,"/",DD,"/",YY   
          move EndFiscCur to juldays 
          call      cvtgreg
          pack taskname with "FISCAL YEAR ",CC,YY," (",str10," - ",MM,"/",DD,"/",YY,")"
          prtpage MONTHINCREPORT;*pHeader10:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,TASKNAME,*boldoff;
.         prtpage MONTHINCREPORT;*pHeader10:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"FISCAL YEAR 2003 (01/01/03 - 12/31/03)",*boldoff;
;Header3
          move BegFiscPrev to juldays 
          call      cvtgreg
          pack      str10 with mm,"/",DD,"/",YY   
          move EndFiscPrev to juldays 
          call      cvtgreg
          pack taskname with "FISCAL YEAR ",CC,YY," (",str10," - ",MM,"/",DD,"/",YY,")"
          prtpage MONTHINCREPORT;*pHeader15:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,taskname,*boldoff;
          add       Doublespaced to row
;Header4
          prtpage MONTHINCREPORT;*pHeader10:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader15:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"Volume",*boldoff;
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
TotalsSection
;Volume 1
          add lgboxheight,totBoxRow
          add "100",totBoxRow
          add smboxheight,totboxrow,TotBoxHeight
;
;Rent exchange halfboxes

;
          prtpage MONTHINCREPORT;*pensize=10,*FILL=*ON,*BGCOLOR=*WHITE,*RECT=TotBoxROW:TotBoxHeight:BoxOneLeft:BoxOneRight
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
          prtpage MONTHINCREPORT;*pBoxFiveVert:TotBoxROW,*pensize=10,*line=BoxFiveVert:TotBoxHeight;
;;
;Title Cell
          move row to n9
;Topmost
.         add "10" to Row
.         prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Revised",*boldoff;
.         sub       "10",Row
;Top

          add       "40" to row
          prtpage MONTHINCREPORT;*pHeader9:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Total",*boldoff;
          prtpage MONTHINCREPORT;*pHeader10:row,*bgcolor=*YELLOW,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Exch. Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader14:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Total",*boldoff;
          prtpage MONTHINCREPORT;*pHeader15:row,*bgcolor=*YELLOW,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Exch Volume",*boldoff;
          sub       "40" to row
;TopMiddle
;Middle Centered
          add singlespaced to row
.         prtpage MONTHINCREPORT;*pHeader11:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Projections",*boldoff;
;         prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Projections",*boldoff;
          prtpage MONTHINCREPORT;*pHeader13:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Actual",*boldoff;
          prtpage MONTHINCREPORT;*pHeader16:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Actual",*boldoff;
;Second Line
          add "80" to row
          prtpage MONTHINCREPORT;*pHeader9:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader10:row,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Rental Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader14:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader15:row,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Rental Volume",*boldoff;
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
                    if (FiscMonth = c1)
                              load      str3 with n2,"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"
                    else
                                        If (fiscmonth = 2)
                                                  load      str3,n2,"FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN"
                                        elseif (fiscmonth = 3) 
                                                  load      str3,n2,"MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB"
                                        elseif (fiscmonth = 4) 
                                                  load      str3,n2,"APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR"
                                        elseif (fiscmonth = 5) 
                                                  load      str3,n2,"MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR"
                                        elseif (fiscmonth = 6) 
                                                  load      str3,n2,"JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY"
                                        elseif (fiscmonth = 7) 
                                                  load      str3,n2,"JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN"
                                        elseif (fiscmonth = 8) 
                                                  load      str3,n2,"AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL"
                                        elseif (fiscmonth = 9) 
                                                  load      str3,n2,"SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG"
                                        elseif (fiscmonth = 10) 
                                                  load      str3,n2,"OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"
                                        elseif (fiscmonth = 11) 
                                                  load      str3,n2,"NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT"
                                        elseif (fiscmonth = 12) 
                                                  load      str3,n2,"DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV"
                                        endif
                    endif
                    prtpage MONTHINCREPORT;*p1450:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,str3;
                    prtpage MONTHINCREPORT;*p5750:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,str3;
                    add       SmBoxHeight to row
          until     (n2 = "12")
          repeat
;Total Inserted
          add       halfsmboxheight to Totboxrow
          add       "40" to Totboxrow
                    prtpage MONTHINCREPORT;*p1450:TotBoxRow,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*ulon,*boldon,"Total";
.                   prtpage MONTHINCREPORT;*p6850:TotBoxRow,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*ulon,*boldon,"Total";
                    prtpage MONTHINCREPORT;*p5750:TotBoxRow,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*ulon,*boldon,"Total";
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
          prtpage MONTHINCREPORT;*pBoxFiveVert:BegRowLine,*pensize=10,*line=BoxFiveVert:LgBoxHeight;
;Fill Months in

.Order Search
          MOVE      C1 TO NORDPATH
          Pack      NORDFLD2,"02L",LSTNUM
          call      nordaim
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
.                   MOVE      OODTEM,MM
.                   MOVE      OODTEY,YY
.                   MOVE      OODTED,DD
.                   call      cvtjul
                    Goto Enter
.patch1.2
          else
.patch1.2
                    if (manualflag <> "Y")
                              goto READIT1
                    else
                              goto ENDPROGRAM
                    endif
.                   goto EndProgram
.patch1.2
          Endif
;Last Years Orders
          Loop      
                    call      nordkg
                              if ((oodtem = "09") & (oodtey="03"))
temper
                              reset olrn
                              endif
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
.                                                                     move mm to n2
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
                    call      INVOICEPART
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
.         move       BegRowLine to row
.         add       SmBoxHeight to row
.         add       OneandahalfSpaced to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*bgcolor=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row       
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
;Revised Projections
;Projections
.         move       BegRowLine to row
.         add       SmBoxHeight to row
.         add       OneandahalfSpaced to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row       
.         prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,"00,000,000";
;Actual
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       OneandahalfSpaced to row
        move mask19 to dim19a
        edit AP1JAN1 to dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*bgcolor=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim19a;
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
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit FEBRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit MARRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit APRRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit MAYRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit JUNRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit JULRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit AUGRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit SEPRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit OCTRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit NOVRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit DECRENT2 to dim20a
          prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;

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
;                 move mask20 to dim20a
;                 edit ORDTOT1 to dim20a
.                   prtpage MONTHINCREPORT;*pBoxThreeVert1Text:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,"Total";
                    clear     DIM20a
;                 move mask20 to dim20a
;                 edit ORDTOT1 to dim20a
.                   prtpage MONTHINCREPORT;*pBoxThreeVert2Text:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,"Total";
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
.>Patch 1.91 Code Added       
                    if (ManualFlag = YES)
                              call getuser
                              call trim using nuseuser
                              clear taskname
                              pack taskname with "Report requested by ",nuseuser," ","on ",DateRan
                              prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,taskname;
                    Endif
.>Patch 1.91 Code Added                 
          else
                    move      TotBoxRow to Row
                    add       smboxheight to ROW
                    add       singlespaced to Row
                    pack Taskname with "*As of ",str10
                    prtpage MONTHINCREPORT;*pBoxOneLeft:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,taskname;
.>Patch 1.91 Code Added                 
                    if (ManualFlag = YES)
                              call getuser
                              call trim using nuseuser                
                              pack taskname with "Report requested by ",nuseuser," on ",DateRan               
                              prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,taskname;
                    endif
.>Patch 1.91 Code Added                 
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
.patch1.4commentout
;   prtpage   MONTHINCREPORT;*NEWPAGE:
;                             *ORIENT=*LANDSCAPE
.patch1.4
                    CALL CLEARVARS
.patch1.4
                    prtclose  MONTHINCREPORT
;patch1.6
                    prtplay             PRTFILENAME,"PDF995"
.                   prtplay             PRTFILENAME,"Acrobat Distiller"
                    pause c10
;patch1.6
                    call emailreport
.Patch 1.9
                    move RECIPIENT to RECIPIENTHOLD
.Patch 1.9
                    clock timestamp,timestamp
                    pack      PRINTNAME,timestamp
                    pack      prtfilename with "c:\work\pdf\",printname,".lst"
                    PRTOPEN MONTHINCREPORT,"PDF995",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
.                   PRTOPEN MONTHINCREPORT,"Acrobat Distiller",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
                    PRTPAGE MONTHINCREPORT;*UNITS=*HIENGLISH:
                                                                                                    *ORIENT=*LANDSCAPE;
.patch1.4                          
                    GOTO READIT
.patch1.3
          ENDIF
          
ENDPROGRAM
          prtclose  MONTHINCREPORT
;patch1.6
          prtplay             PRTFILENAME,"PDF995"
          pause c10
;patch1.6
.         prtplay             PRTFILENAME,"Acrobat Distiller"
          call emailreport
          stop
.patch1.2
.patch1.1
.         prtclose  MONTHINCREPORT
.         prtplay             PRTFILENAME,"Acrobat Distiller"
.ENDPROGRAM
.         stop
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
.Patch 1.7.1 Comment Out
.                                                 until over
.Patch 1.7.1 Comment Out
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
.                                                                               move mm to n2
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
.                                                           clear dumqty
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
.                                                                               move mm to n2
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
.patch1.4
EmailReport
          if (ManualFlag = YES)
.>Patch 1.91 Comment Out - We find out who user is above              
.                   move      c1 to nusepath
.                   clock     port to str3
.                   unpack     str3 into str2,str1
.                   pack       str3 from str1,str2
.                   MOVE      str3 TO NUSEFLD .removed FOR TESTING only
.                   REP       ZFILL IN NUSEFLD
.                   CALL      NUSEKEY
.                   goto      userng if over
.                   scan      "INVALID" in nuseuser
.                   goto      userng if equal
.>Patch 2.3 Comment Out - We find out who user is above               
                    squeeze    nuseuser,nuseuser
                    move    "1",SmtpDestIndexLast  
          else
.patch1.4
                    if (LSTNUM = "011507") 
                              move "JoseDuenas" to nuseuser
.Patch 1.8
                              call trim using recipient
.Patch 1.9
.                             move recipient,SmtpDestinations(2,1)
                              move recipienthold,SmtpDestinations(2,1)
.Patch 1.9
.                             move "SuzieMcGuire@nincal.com",SmtpDestinations(2,1)
.Patch 1.8
.                             move "SteveKehrli@nincal.com",SmtpDestinations(2,1)
.                             move "InformationServices@nincal.com",SmtpDestinations(2,1)
.                             move "SteveKehrli@nincal.com",SmtpDestinations(2,1)
.                             move "SteveKehrli",SmtpDestinations(2,2)
.                             move "SteveKehrli",SmtpDestinations(2,2)
.Add Suzie Per SI 01/03/04
.Patch 1.8

.Patch 1.9
                              call trim using recipienthold
                              move recipienthold,SmtpDestinations(2,2)
.                             call trim using recipient
.                             move recipient,SmtpDestinations(2,2)
.Patch 1.9
.Patch 1.8
.                             move "InformationServices",SmtpDestinations(2,1)
.                             move "",SmtpDestinations(2,2)
                              move "2",SmtpDestIndexLast  
.                             move"SandraInouye" to nuseuser
                    else
.                             move "SteveKehrli" to nuseuser
.Patch 1.8
.Patch 1.9
                              call trim using recipienthold
                              move recipienthold,nuseuser
.                             call trim using recipient
.                             move recipient,nuseuser
.Patch 1.9
.                             move "DavidBaca",SmtpDestinations(2,1)
.                             move "DavidBaca" to nuseuser
..
.Patch 1.8
.                             move "SteveKehrli" to nuseuser
                              move "",SmtpDestinations(2,1)
                              move "",SmtpDestinations(2,2)
                              move  "1",SmtpDestIndexLast  
.Patch 1.8
.patch1.4
                    endif
          endif
.patch1.4
          pack str55,PRINTNAME,".pdf"
.Patch 3.8 Comment Out
.         if (manualflag = YES)
.                   pack   SmtpSubject,"Here is your Income PDF File for ",HoldLstName
.         else
                    pack   SmtpSubject,"Here is your Income PDF File for ",HoldLstName
.         endif
.Patch 3.8 Comment Out
;.   Set the text message that is send with the attachments
          move    str55,SmtpTextMessage(1)   Array <Text message >
          move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
          move    "NTS4",SmtpEmailServer                   Address of email serverc
          clear   smtpemailaddress
          append  nuseuser,SmtpEmailAddress
          append  "@nincal.com",SmtpEmailAddress
          reset   smtpemailaddress
          move    nuseuser,SmtpUserName                                User name
          move    nuseuser,SmtpUserFullName                                User name
;   Set the destinations of the email. Max 100 (Mime spec)
.patch1.4
          move    smtpemailaddress,SmtpDestinations(1,1)
          move    nuseuser,SmtpDestinations(1,2)
.         move    "1",SmtpDestIndexLast  
.patch1.4

.                        originators UserName
          move    str55,SmtpAttachments(1,1)                     Attached file name
          if (manualflag <> YES)
                    move    "c:\work\pdf",SmtpAttachments(1,2)           Path to attached file name
.                   move    "c:\work",SmtpAttachments(1,2)           Path to attached file name
          else
.Patch 1.8 Comment Out No longer works here - custom setup 
.                    if (nuseuser = "STEVEKEHRLI")                    .Steve is running report manually
.Already put on his desktop no need to send to him
.                                       stop
.         move    "c:\docume~1\skehrli\desktop",SmtpAttachments(1,2)           Path to attached file name
.                   else
                move    "c:\work\pdf",SmtpAttachments(1,2)           Path to attached file name
.                   endif
.Patch 1.8 End Comment Out
          endif
          move    "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
          clear   SmtpLogFile                                         'Clear' disables the LogFile
          move    "1",SmtpProgress                                    Enable progress bars
          call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
          if not equal
                    pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
                    "Status Code ",SmtpStatus," - ",SmtpStatusText
                    move    "PDF File not found",SmtpSubject Subject
                    move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
                    call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
          else 
.patch3.3
                    if (manualflag <> YES)
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
.                             append  "c:\work\",taskname
                              append  str55,taskname
                              append  " ",taskname
                              append  NTWKPATH5,taskname
                              append  "INCOME\",taskname
;patch1.6
                              append  lstnum,taskname
                              append  "_",taskname
.                             append  "2yr",taskname
                              append  str55,taskname
;patch1.6
                              reset     taskname
                              execute   taskname
                    endif
.patch3.3
          endif
          return

RetryPrint
   trap ExitThisProgram if SPOOL
          PRTOPEN MONTHINCREPORT,"",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
          trapclr   SPOOL
          return
.>Patch 1.91 Begin Routine
GetUser
          move      c1 to nusepath
          clock       port to str3
          unpack    str3 into str2,str1
          pack      str3 from str1,str2
          MOVE      str3 TO NUSEFLD .removed FOR TESTING only
          REP       ZFILL IN NUSEFLD
          CALL      NUSEKEY
          goto      userng if over
          scan      "INVALID" in nuseuser
          goto      userng if equal
          return
.>Patch 1.91 End Routine
userng
          clear     taskname
          append    "I'm sorry I've lost track of who you are,",taskname
          append    NewLine,taskname
          append    "Please leave the program and try again!",taskname
          reset     taskname
          alert     caution,taskname,result
          shutdown
   stop
ExitThisProgram
          stop
.endpatch1.4
                    
                    
                    
          INCLUDE   NORDIO.INC
          INCLUDE   NDATIO.INC
.patch1.4
          include   NUSEIO.INC
.patch1.4
;compute
          INCLUDE   NSHPIO.INC
          INCLUDE   NMRGIO.INC
          INCLUDE   NDAT3IO.INC
         include   nacdIO.inc

;compute
;begin patch 1.9
;         INCLUDE   NINVIO.INC
;         INCLUDE   COMPUTE.INC
          INCLUDE   compute.inc
          INCLUDE   ninvio.inc
          Include   NInvACdio.inc
;begin patch 1.9
          INCLUDE   NJSTIO.INC
          INCLUDE   NADJIO.INC
          INCLUDE   COMLOGIC.INC
