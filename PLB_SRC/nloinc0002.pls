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
;Patch3.6
.         include   nmlrdd.inc
          include   compdd.inc
          include   cntdd.inc
;patch3.6
;Compute
          INCLUDE   NADJDD.INC
          INCLUDE   NORDDD.INC
;begin patch 4.0
;         INCLUDE   NINVDD.INC
          INCLUDE   ninvdd.inc
               INclude        NInvAcddd.inc
;end patch 4.0
          INCLUDE   NJSTDD.INC
.patch3.1
          include   NUSEDD.INC
.patch3.1
MONTHINCREPORT      PFILE     
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
.patch3.1
userinfo dim       500
userlogn dim       7
userlogw dim       7
BEGIN    FORM      2
LAST     FORM      2
;Patch 3.8 Hold Variable to diffentiate which year to run
RepDateJul          form      5
;Patch 3.8
.patch3.1
Release             INIT      "4.2"      DMB      18JAN2006    Added code to show date and name of person who runs a manual income report
.Release  init                  "4.1"          JD 04Jan2006 Clear compute vars.
;Release  init                  "4.0"          DLH 9Mar2005 Invoice Conversion
;Release  init "3.9" DMB 24N2005 Added code to use recipient var
.Release  init "3.8" DMB 10JAN2005 Added code to adjust years printed for fiscal years.
.Release  init "3.7.1" DMB 04OCT2004 Added code to loop through ajustments until the 9th adj is read even if it doesn't exist.       also added code to adjust for fy runs.
.Release INIT "3.7" ASH 06AUG2004       Logo conversion
.Release INIT "3.6" DMB 26MAY2004       Mailer COnversion
.Release INIT "3.5" 14JAN2003 Modify Code to define range with date given
.Release INIT "3.4" 14JAN2003 Added Code to print out reports individually and email
.Release INIT "3.3" 30 DEC2003 Added code to copy pdf to \\nins1\e\data\income
.Release  INIT      "3.2"     11DEC2003 Added code to not email if steve running
.Release  INIT      "3.1"     11DEC2003 Added code to email report back to user if manual
.RerRelease         INIT      "3.0"     11NOV2003 Moved Patch 2.0 before ap1 is added to year totals.
.Release  INIT      "1.0"     Initial Release     of BO Monthy income report for clients
SingleSpaced        FORM      "160"
OneandahalfSpaced   FORM      "240"
DoubleSpaced        FORM      "320"
.LgBoxHeight        FORM      "3521"
LgBoxHeight         FORM      "3441"
;         move      "3581" to LgBoxHeight(1)

;LgBoxHeight        FORM      "6451"
;LgBoxHeight        FORM      "7491"
SmBoxHeight         FORM      "350"
;SmBoxHeight        FORM      "490"
;HalfsmboxHeight FORM         "245"
HalfsmboxHeight FORM          "175"
QuartersmboxHeight FORM       "87"
;MonthTextSmall     FORM      "480"
TotBoxHeight        Form      9
totBoxRow FORM      9
BegRowLine          FORM      9
BegRowLine2         FORM      9
          add       LgBoxHeight,SMBOXHEIGHT,BEGROWLINE2
          add       SMBOXHEIGHT,BEGROWLINE2
BegRowLineDiff      FORM      9
LgBoxHeight2        FORM      5
          mult smboxheight,c6,Lgboxheight2
          add       begrowline2,lgboxheight2
          
.Patch 3.8 Add code for List Name to show up in subject
HoldLstName         Dim 55
.Patch 3.8
;BoxOneLeft         FORM      "0"
BoxOneLeft          FORM      "328"
;BoxOneRight        FORM      "500"
BoxOneRight         FORM      "3576"
;BoxOneRight        FORM      "600"
BoxTwoLeft          FORM      "3626"
;BoxTwoRight        FORM      "2750"
BoxTwoRight         FORM      "6874"
;BoxThreeLeft       FORM      "2800"
BoxThreeLeft        FORM      "6924"
;BoxThreeRight      FORM      "6400"
BoxThreeRight       FORM      "10172"
;;BoxFourLeft       FORM      5,(3),"5400","",""
;BoxFourLeft        FORM      "6600"
;BoxFourRight       FORM      "7200"
;BoxFourRight       FORM      "5800"
;BoxFiveLeft        FORM      "7250"
;BoxFiveLeft        FORM      "7150"
;BoxFiveLeft        FORM      "5850"
;BoxFiveRight       FORM      "9500"
;BoxFiveRight       FORM      "9400"
;BoxFiveRight       FORM      "7690"
;BoxFiveLeft1       FORM      "8375"
;BoxFiveLeft1       FORM      "8275"
;BoxFiveLeft1       FORM      "6770"
;BoxFiveRight1      FORM      "9500"
;BoxFiveRight1      FORM      "9400"
;BoxFiveRight1      FORM      "7690"
;BoxSixLeft         FORM      "9550"
;BoxSixLeft         FORM      "9450"
;BoxSixLeft         FORM      "7740"
;BoxSixRight        FORM      "10500"
;BoxSixRight        FORM      "10500"
;List 
BoxOneVert      FORM    5(3)
          sub BoxOneleft,BoxOneright,n9
          div c4 in n9,n8
          add BoxOneLeft,n8,BoxOneVert(1)
;         move "1062",BoxOneVert(1)

          sub BoxOneleft,BoxOneright,n9
          div c4 in n9,n8
          mult c2,n8
          add BoxOneLeft,n8,BoxOneVert(2)
;         move "1924",BoxOneVert(2)
          sub BoxOneleft,BoxOneright,n9
          div c4 in n9,n8
          mult c3,n8
          add BoxOneLeft,n8,BoxOneVert(3)
;         move "2786",BoxOneVert(3)
;BoxTwoVert         FORM      "1700"
;BoxTwoVert         FORM      "1370"
BoxTwoVert      FORM    5(3)
          sub BoxTwoleft,BoxTworight,n9
          div c4 in n9,n8
          mult c1,n8
          add BoxTwoLeft,n8,BoxTwoVert(1)
;         move "4560",BoxTwoVert(1)
          sub BoxTwoleft,BoxTworight,n9
          div c4 in n9,n8
          mult c2,n8
          add BoxTwoLeft,n8,BoxTwoVert(2)
;         move "5422",BoxTwoVert(2)
          sub BoxTwoleft,BoxTworight,n9
          div c4 in n9,n8
          mult c3,n8
          add BoxTwoLeft,n8,BoxTwoVert(3)
;         move "6284",BoxTwoVert(3)
;BoxThreeVert1      FORM      "4000"
;BoxThreeVert1      FORM      "3260"
;BoxThreeVert2      FORM      "5200"
;BoxThreeVert2      FORM      "4180"
BoxThreeVert    FORM    5(3)
          sub BoxThreeleft,BoxThreeright,n9
          div c4 in n9,n8
          mult c1,n8
          add BoxThreeLeft,n8,BoxThreeVert(1)

          sub BoxThreeleft,BoxThreeright,n9
          div c4 in n9,n8
          mult c2,n8
          add BoxThreeLeft,n8,BoxThreeVert(2)

          sub BoxThreeleft,BoxThreeright,n9
          div c4 in n9,n8
          mult c3,n8
          add BoxThreeLeft,n8,BoxThreeVert(3)
;         move "7854",BoxThreeVert(1)
;         move "8716",BoxThreeVert(2)
;         move "9578",BoxThreeVert(3)
;BoxFiveVert        FORM      "8275"
;BoxFiveVert        FORM      "6770"
;BoxFiveVert        FORM      "8375"
;BoxSixVert1        FORM      "8660"
;BoxSixVert2        FORM      "9580"


;For Text Align close to but not on Vertical Line
;BoxTwoVertText               FORM      "1690"
;BoxTwoVertText               FORM      "1360"
BoxOneVertText          FORM    5(3)
          sub       "20" from BoxOneVert(1),BoxOneVertText(1)
          sub       "20" from BoxOneVert(2),BoxOneVertText(2)
          sub       "20" from BoxOneVert(3),BoxOneVertText(3)

;         move      "1052",BoxOneVertText(1)
;         move      "1914",BoxOneVertText(2)
;         move      "2776",BoxOneVertText(3)
BoxTwoVertText          FORM    5(3)
          sub       "20" from BoxTwoVert(1),BoxTwoVertText(1)
          sub       "20" from BoxTwoVert(2),BoxTwoVertText(2)
          sub       "20" from BoxTwoVert(3),BoxTwoVertText(3)
;         move      "4550",BoxTwoVertText(1)
;         move      "4512",BoxTwoVertText(2)
;         move      "6274",BoxTwoVertText(3)
BoxThreeVertText          FORM    5(3)
;         move      "7844",BoxThreeVertText(1)
;         move      "8706",BoxThreeVertText(2)
;         move      "9568",BoxThreeVertText(3)
          sub       "20" from BoxThreeVert(1),BoxThreeVertText(1)
          sub       "20" from BoxThreeVert(2),BoxThreeVertText(2)
          sub       "20" from BoxThreeVert(3),BoxThreeVertText(3)
;BoxTwoVertText          FORM    "1360"
;BoxThreeVert1Text  FORM      "3990"
;BoxThreeVert1Text  FORM      "3250"
;BoxThreeVert2Text  FORM      "5150"
;BoxThreeVert2Text  FORM      "4170"
;BoxFiveVertText              FORM      "8265"
;BoxFiveVertText              FORM      "6760"
;BoxSixVert1Text              FORM      "8650"
;BoxSixVert2Text              FORM      "9570"
BoxOneLeftText      FORM                5
          add       "20" from BoxOneLeft,BoxOneLeftText
BoxOneRightText     FORM                5
          sub       "20" from BoxOneRight,BoxOneRightText
;BoxOneRightText    FORM                "3638"
;BoxTwoRightText    FORM      "2740"
BoxTwoLeftText      FORM                5
          add       "20" from BoxTwoLeft,BoxTwoLeftText
BoxTwoRightText               FORM      5
;BoxTwoRightText              FORM      "6936"
          sub       "20" from BoxTwoRight,BoxTwoRightText
;BoxThreeRightText  FORM      "6390"
;BoxThreeRightText  FORM      "9434"
BoxThreeLeftText    FORM                5
          add       "20" from BoxThreeLeft,BoxThreeLeftText
BoxThreeRightText   FORM      5
          sub       "20" from BoxThreeRight,BoxThreeRightText
;BoxFiveRightText   FORM      "9390"
;BoxFiveRightText   FORM      "7680"
;BoxSixRightText    FORM      "10390"
;BoxSixRightText    FORM      "10490"
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
          sub BoxOneleft,BoxOneright,n9
          div c8 in n9,n8
          mult c3,n8
          add BoxOneLeft,n8,Header9
;Header10
          sub BoxOneleft,BoxOneright,n9
          div c8 in n9,n8
          mult c5,n8
          add BoxOneLeft,n8,Header10
;Header11
          sub BoxOneleft,BoxOneright,n9
          div c8 in n9,n8
          mult c7,n8
          add BoxOneLeft,n8,Header11
;Header12
          sub BoxTwoleft,BoxTworight,n9
          div c8 in n9,n8
          mult c3,n8
          add BoxTwoLeft,n8,Header12
;Header13
          sub BoxTwoleft,BoxTworight,n9
          div c8 in n9,n8
          mult c5,n8
          add BoxTwoLeft,n8,Header13
;Header14
          sub BoxTwoleft,BoxTworight,n9
          div c8 in n9,n8
          mult c7,n8
          add BoxTwoLeft,n8,Header14
;Header15
          sub BoxThreeleft,BoxThreeright,n9
          div c8 in n9,n8
          mult c3,n8
          add BoxThreeLeft,n8,Header15
;Header16
          sub BoxThreeleft,BoxThreeright,n9
          div c8 in n9,n8
          mult c5,n8
          add BoxThreeLeft,n8,Header16
;Header17
          sub BoxThreeleft,BoxThreeright,n9
          div c8 in n9,n8
          mult c7,n8
          add BoxThreeLeft,n8,Header17
;Data Vars
BegFisc   FORM      5(6)
EndFisc   FORM      5(6)
BegQtrFisc FORM     5(6,4)
EndQtrFisc FORM     5(6,4)


FISCYEARFLAG        FORM      1
FISCQTRFLAG         FORM      1

JAN    FORM    15(6)

FEB    FORM    15(6)

MAR    FORM    15(6)

APR    FORM    15(6)

MAY    FORM    15(6)

JUN    FORM    15(6)

JUL    FORM    15(6)

AUG    FORM    15(6)

SEP    FORM    15(6)

OCT    FORM    15(6)

NOV    FORM    15(6)

DEC    FORM    15(6)

OrdTOT FORM    15(6)
QTRTOT    FORM      15(6,4)
JANRENT        FORM    15(6)
JANEXCH        FORM    15(6)

FEBRENT        FORM    15(6)
FEBEXCH        FORM    15(6)

MARRENT        FORM    15(6)
MAREXCH        FORM    15(6)

APRRENT        FORM    15(6)
APREXCH        FORM    15(6)

MAYRENT        FORM    15(6)
MAYEXCH        FORM    15(6)

JUNRENT        FORM    15(6)
JUNEXCH        FORM    15(6)

JULRENT        FORM    15(6)
JULEXCH        FORM    15(6)

AUGRENT        FORM    15(6)
AUGEXCH        FORM    15(6)

SEPRENT        FORM    15(6)
SEPEXCH        FORM    15(6)

OCTRENT        FORM    15(6)
OCTEXCH        FORM    15(6)

NOVRENT        FORM    15(6)
NOVEXCH        FORM    15(6)

DECRENT        FORM    15(6)
DECEXCH        FORM    15(6)

EXCHTOT        FORM    15(6)
RENTTOT        FORM    15(6)

EXCHQTRTOT        FORM    15(6,4)
RENTQTRTOT        FORM    15(6,4)
;AP1
AP1JAN FORM    10.2(6)
AP1FEB FORM    10.2(6)
AP1MAR FORM    10.2(6)
AP1APR FORM    10.2(6)
AP1MAY FORM    10.2(6)
AP1JUN FORM    10.2(6)
AP1JUL FORM    10.2(6)
AP1AUG FORM    10.2(6)
AP1SEP FORM    10.2(6)
AP1OCT FORM    10.2(6)
AP1NOV FORM    10.2(6)
AP1DEC FORM    10.2(6)
AP1TOT FORM    10.2(6)

AP1QTRTOT FORM    10.2(6,4)
;Proj
;ProjJAN        FORM    10.2(6)
;ProjFEB        FORM    10.2(6)
;ProjMAR        FORM    10.2(6)
;ProjAPR        FORM    10.2(6)
;ProjMAY        FORM    10.2(6)
;ProjJUN        FORM    10.2(6)
;ProjJUL        FORM    10.2(6)
;ProjAUG        FORM    10.2(6)
;ProjSEP        FORM    10.2(6)
;ProjOCT        FORM    10.2(6)
;ProjNOV        FORM    10.2(6)
;ProjDEC        FORM    10.2(6)
;ProjTOT        FORM    10.2(6)
;Var
;VarJAN1  FORM      10.2(6)
;VarFEB1  FORM      10.2(6)
;VarMAR1  FORM      10.2(6)
;VarAPR1  FORM      10.2(6)
;VarMAY1  FORM      10.2(6)
;VarJUN1  FORM      10.2(6)
;VarJUL1  FORM      10.2(6)
;VarAUG1  FORM      10.2(6)
;VarSEP1  FORM      10.2(6)
;VarOCT1  FORM      10.2(6)
;VarNOV1  FORM      10.2(6)
;VarDEC1  FORM      10.2(6)
;VarTOT1  FORM      10.2(6)

;For compute
mrgsw    dim       1
shipsw   dim       1
;compute
;         move      "22350",ProjJAN1
;         move      "27111",ProjJAN2
;         move      "19447",ProjFEB1
;         move      "7961",ProjFEB2
;         move      "14786",ProjMAR1
;         move      "47289",ProjMAR2
;         move      "21619",ProjAPR1
;         move      "14604",ProjAPR2
;         move      "84894",ProjMAY1
;         move      "67277",ProjMAY2
;         move      "89529",ProjJUN1
;         move      "75534",ProjJUN2
;         move      "89726",ProjJUL1
;         move      "110000",ProjJUL2
;         move      "80575",ProjAUG1
;         move      "80000",ProjAUG2
;         move      "52395",ProjSEP1
;         move      "20000",ProjSEP2
;         move      "62830",ProjOCT1
;         move      "61000",ProjOCT2
;         move      "63345",ProjNOV1
;         move      "61500",ProjNOV2
;         move      "58504",ProjDEC1
;         move      "56800",ProjDEC2
;         add ProjJAN1,ProjTOT1         
;         add ProjFEB1,ProjTOT1
;         add ProjMAR1,ProjTOT1
;         add ProjAPR1,ProjTOT1
;         add ProjMAY1,ProjTOT1
;         add ProjJUN1,ProjTOT1
;         add ProjJUL1,ProjTOT1
;         add ProjAUG1,ProjTOT1
;         add ProjSEP1,ProjTOT1
;         add ProjOCT1,ProjTOT1
;         add ProjNOV1,ProjTOT1
;         add ProjDEC1,ProjTOT1
;         add ProjJAN2,ProjTOT2         
;         add ProjFEB2,ProjTOT2
;         add ProjMAR2,ProjTOT2
;         add ProjAPR2,ProjTOT2
;         add ProjMAY2,ProjTOT2
;         add ProjJUN2,ProjTOT2
;         add ProjJUL2,ProjTOT2
;         add ProjAUG2,ProjTOT2
;         add ProjSEP2,ProjTOT2
;         add ProjOCT2,ProjTOT2
;         add ProjNOV2,ProjTOT2
;         add ProjDEC2,ProjTOT2
;
DUMQTY    FORM      10
TMPVAR    FORM      10
RQTY      FORM      10
EXQTY     FORM      10
NUM102    FORM      10.2
NUM1021   FORM      10.2
NUM1022   FORM      10.2
.Patch1.1
OPENPAY   FORM      10.2
.Patch1.1
;AP1TOT   FORM      10.2(6)

FISCMONTH FORM      2
;
;Maksing Vars
mask20      init    "ZZZ,ZZZ,ZZZ,ZZZ,ZZ9"         ;formatting vars
Dim20a      dim     20            ;formatting vars
mask16      init    "Z,ZZZ,ZZZ,ZZZ.99"        ;formatting vars
Dim16a      dim     16            ;formatting vars
mask19      init    "($ZZZ,ZZZ,ZZZ.99)"        ;formatting vars
Dim19a      dim     19            ;formatting vars
HoldAP1   form 10.2
.PATCH2.0
ManualFlag          DIM       1
TODAYIS   FORM      5
.patch3.5
FORCEDAY  DIM 4
.patch3.5
.>Patch 4.2 Var Added to keep track of Date Ran
DateRan dim         10
.>Patch 4.2 Var Added to keep track of Date Ran
          clock timestamp,timestamp
          unpack timestamp,str2,yy,mm,dd
          call      cvtjul
          move juldays  to TODAYIS
.>Patch 4.2 Code Added
          pack DATERAN with mm,slash,dd,slash,str2,yy
.>Patch 2.2 Code Ended        
.patch2.0
.
.         clock timestamp,timestamp
.START PATCH 3.7 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 3.7 ADDED LOGIC
.patch3.2
          pack      PRINTNAME,timestamp
.         move "Qtrinc.lst",PRINTNAME
.endpatch3.2
.testing
          pack      prtfilename,"c:\work\PDF\",PRINTNAME,".lst"
.         PRTOPEN MONTHINCREPORT,"\\NINs2\Laser6",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
.         PRTPAGE MONTHINCREPORT;*UNITS=*HIENGLISH:
.                          *ORIENT=*LANDSCAPE;
.patch3.2
          trap RetryPrint if SPOOL
.endpatch3.2
          OPEN      INCLISTS,"INCLISTS"
          PRTOPEN MONTHINCREPORT,"\\NINs2\Laser6",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
          PRTPAGE MONTHINCREPORT;*UNITS=*HIENGLISH:
                          *ORIENT=*LANDSCAPE;
.patch3.2
          TrapClr   SPOOL
.endpatch3.2
.patch2.0
   move "NLOINC0002",Program
          CALL      PAINT
DATEIS
          move      "Y" to str1
.patch3.5
.                     PACK      str10 FROM mm,SLASH,dd,SLASH,yy        
                      PACK      str12 FROM mm,SLASH,dd,SLASH,str2,yy   
                      PACK       FORCEDAY,str2,YY
                      KEYIN     *P10:6,*DV,str12," OK? ",*t20,str1
        CMATCH    "N" TO str1
        GOTO      Whichway IF NOT EQUAL
.        KEYIN     *P10:6,*+,mm,"/",dd,"/",yy
        KEYIN     *P10:6,*+,mm,"/",dd,"/",FORCEDAY
.                     PACK      str10 FROM mm,SLASH,dd,SLASH,yy
                              UNPACK    FORCEDAY,str2,YY
                              PACK      str12 FROM mm,SLASH,dd,SLASH,str2,yy
.patch3.5
        call        cvtjul
                      move juldays  to TODAYIS
               goto DateIS
WhichWay
        MOVE      "A" to str1
        KEYIN     *ES,*P10:8,*EF,"M",*white,"anual or ",*cyan,"(A)uto: ",*T15,*RV,STR1
          if (str1 = "M")
                    move YES to ManualFlag
                    move "Q" to REP1

NewList
                  KEYIN     *P10:10,*white,"List:  ",LNUM
                    GOTO      NEWLIST IF EOS
                    if (LNUM = "")
                              goto NewList
                    else
                              call zfillit using lnum
                              move lnum to NDATFLD
                              REP       " 0" IN NDATFLD
                              call zfillit using NDATFLD
                              move c1 to ndatpath
                              CALL NDATKEY
                              if over
                                        clear LNUM
                                        Goto NEWLIST
                              endif
.patch3.4
          call      OpenPrintFile
.patch3.4
                    endif
        DISPLAY     *P10:10,*white,"List:  ",LNUM,"   ",OLSTNAME
         MOVE      "O" to DATEBY
GetDate
         KEYIN     *P10:14,*EF,"Maildate Or ",*cyan,"(O)","rder date: ",*RV,DATEBY
                    goto      getdate if ((DATEBY <> "M") and (DATEBY <> "O"))
Basis     
         KEYIN     *P10:14,*EF,*white,"Accrual (By ",*CYAN,"[I]",*white,"nvoice) Or ","Cash (",*cyan,"[C]",*White,"heck Date) Basis: ",*RV,LTYPE
                    GOTO      BASIS IF EOS
                    goto      BASIS if ((LTYPE <> "I") and (LTYPE <> "C"))
NewYear   
                    KEYIN     *P1:1,*P10:16,*JR,"ENTER STARTING 4 - DIGIT BEGINNING YEAR FOR 6 YEAR ANALYSIS  ",*RV,str4
                    GOTO      NEWYear IF EOS
                    TYPE      str4
                    GOTO      NewYear IF NOT EQUAL
                    COUNT     n2,str4
                    goto      NEWYEAR if (n2 <> c4)
      move  str4 to lyear
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



READITa
          READ      INCLISTS,SEQ;LVARS
          GOTO endprogram IF OVER
          GOTO READITa IF (REP1 <> "Q")
        DISPLAY     *P10:10,*white,"List:  ",LNUM,"   ",OLSTNAME

READIT
.patch3.4
   call   OpenPrintFile
.patch3.4
.Patch2.0
.READIT
.         READ      INCLISTS,SEQ;LVARS
.         GOTO endprogram IF OVER
.         GOTO READIT IF (REP1 <> "Q")
;
MANUALLIST
;Get Fiscal Year
.patch3.5
.                   clock timestamp,timestamp     
.                   UNPACK TIMESTAMP,STR4
.Patch 3.7.1
                    UNPACK TIMESTAMP,STR4,n2
.Patch 3.7.1
                    move FORCEDAY to N4
.Patch 3.7.1
;Patch 3.8
;If this report is a fiscal year report and the present month is greater than the fiscal month then add a year cause we need to show next fiscal year
;;        if (Lmonth <> C1)
;;                  if (n2 >= LMONTH)
;;                            add c1 to n4
;;                  endif
;;        endif
.Patch 3.7.1
.                   MOVE STR4 TO N4
.patch3.5
;;        if (Lmonth <> C1)
;Patch 3.8
;;                  if (n2 >= LMONTH)
;;                            sub c1 from n4
;;                  endif
;;        endif
;Patch 3.8
                    SUB       LYEAR from n4,N5
                    IF (N5 < C5)
                              SUB N5 FROM C5,N6
                              ADD       N6 TO N4
                    unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    ELSE
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    ENDIF
;                   move      "20030101",str8
;                   move      "20031231",str9
.                   move      "20030101",str8
.                   move      "20031231",str9
.                   unpack    str8,str2,yy,mm,dd
.For Fiscal
                    move LMONTH to fiscmonth
                    MOVE LMONTH TO MM
                    MOVE      "01" TO DD
;Year 1
                    call      cvtjul
;Patch 3.8  replacement for code about
;If this is a fiscal year report and todayis[report date] is less than latest fiscal date than do not add add another year to it
;i.e. if the fiscal month is jul 04 and the report date is mar 04 then we must sub a year for jul 03 - jun 04 FY
;i.e. if the fiscal month is jul 04 and the report date is aug 04 then we leave alone to create jul 04 - jun 05 FY

                    if (Lmonth <> C1)
                              if (todayis < juldays)
                                        sub c1 from n2
                                        move n2 to YY
                                        rep zfill,YY
                                        call      cvtjul
                              endif
                    endif
;Patch 3.8
                    move juldays to BegFisc(1)
                    sub       c1 from juldays
                    call      cvtgreg
                    CLEAR     N2
                    MOVE      yy TO N2
                    pack str4 with CC,YY
                    move str4 to n4
.                   if (mm = "12" & DD = "31")
                              add c1,n4
.                   endif
                    unpack n4,str2,n2
                    MOVE N2 TO YY
                    CALL CVTJUL
                    move juldays to EndFisc(1) 
;1st Qtr
                    move begfisc(1) to JULDAYS
                    move      begfisc(1) to  BegQtrFisc(1,1)
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(1,2)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(1,1)
;2nd Qtr
                    move      BegQtrFisc(1,2) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(1,3)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(1,2)
;3rd Qtr
                    move      BegQtrFisc(1,3) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(1,4)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(1,3)
;4th Qtr
                    move      BegQtrFisc(1,4) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    sub c1 from juldays
                    move juldays to EndQtrFisc(1,4)
tease
Year2
;Beg
                    move begfisc(1) to JULDAYS
                    call cvtgreg
                    pack str4 with CC,YY
                    move str4 to n4
                    sub c1,n4
                    unpack n4,str2,n2
                    move n2 to YY
                    rep zfill,YY
                    call cvtjul
                    move juldays to BegFisc(2)
;End
                    move endfisc(1) to JULDAYS
                    call cvtgreg
                    pack str4 with CC,YY
                    move str4 to n4
                    sub c1,n4
                    unpack n4,str2,n2
                    move n2 to YY
                    rep zfill,YY
                    call cvtjul
                    move juldays,EndFisc(2)

;1st Qtr
                    move begfisc(2) to JULDAYS
                    move      begfisc(2) to  BegQtrFisc(2,1)
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(2,2)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(2,1)
;2nd Qtr
                    move      BegQtrFisc(2,2) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(2,3)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(2,2)
;3rd Qtr
                    move      BegQtrFisc(2,3) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(2,4)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(2,3)
;4th Qtr
                    move      BegQtrFisc(2,4) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    sub c1 from juldays
                    move juldays to EndQtrFisc(2,4)


Year3
;Beg
                    move begfisc(2) to JULDAYS
                    call cvtgreg
                    pack str4 with CC,YY
                    move str4 to n4
                    sub c1,n4
                    unpack n4,str2,n2
                    move n2 to YY
                    rep zfill,YY
                    call cvtjul
                    move juldays to BegFisc(3)
;End
                    move endfisc(2) to JULDAYS
                    call cvtgreg
                    pack str4 with CC,YY
                    move str4 to n4
                    sub c1,n4
                    unpack n4,str2,n2
                    move n2 to YY
                    rep zfill,YY
                    call cvtjul
                    move juldays,EndFisc(3)
;1st Qtr
                    move begfisc(3) to JULDAYS
                    move      begfisc(3) to  BegQtrFisc(3,1)
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(3,2)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(3,1)
;2nd Qtr
                    move      BegQtrFisc(3,2) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(3,3)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(3,2)
;3rd Qtr
                    move      BegQtrFisc(3,3) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(3,4)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(3,3)
;4th Qtr
                    move      BegQtrFisc(3,4) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    sub c1 from juldays
                    move juldays to EndQtrFisc(3,4)
Year4
;Beg
                    move begfisc(3) to JULDAYS
                    call cvtgreg
                    pack str4 with CC,YY
                    move str4 to n4
                    sub c1,n4
                    unpack n4,str2,n2
                    move n2 to YY
                    rep zfill,YY
                    call cvtjul
                    move juldays to BegFisc(4)
;End
                    move endfisc(3) to JULDAYS
                    call cvtgreg
                    pack str4 with CC,YY
                    move str4 to n4
                    sub c1,n4
                    unpack n4,str2,n2
                    move n2 to YY
                    rep zfill,YY
                    call cvtjul
                    move juldays,EndFisc(4)
;1st Qtr
                    move begfisc(4) to JULDAYS
                    move      begfisc(4) to  BegQtrFisc(4,1)
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(4,2)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(4,1)
;2nd Qtr
                    move      BegQtrFisc(4,2) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(4,3)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(4,2)
;3rd Qtr
                    move      BegQtrFisc(4,3) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(4,4)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(4,3)
;4th Qtr
                    move      BegQtrFisc(4,4) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    sub c1 from juldays
                    move juldays to EndQtrFisc(4,4)
Year5
;Beg
                    move begfisc(4) to JULDAYS
                    call cvtgreg
                    pack str4 with CC,YY
                    move str4 to n4
                    sub c1,n4
                    unpack n4,str2,n2
                    move n2 to YY
                    rep zfill,YY
                    call cvtjul
                    move juldays to BegFisc(5)
;End
                    move endfisc(4) to JULDAYS
                    call cvtgreg
                    pack str4 with CC,YY
                    move str4 to n4
                    sub c1,n4
                    unpack n4,str2,n2
                    move n2 to YY
                    call cvtjul
                    rep zfill,YY
                    move juldays,EndFisc(5)
;1st Qtr
                    move begfisc(5) to JULDAYS
                    move      begfisc(5) to  BegQtrFisc(5,1)
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(5,2)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(5,1)
;2nd Qtr
                    move      BegQtrFisc(5,2) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(5,3)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(5,2)
;3rd Qtr
                    move      BegQtrFisc(5,3) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(5,4)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(5,3)
;4th Qtr
                    move      BegQtrFisc(5,4) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    sub c1 from juldays
                    move juldays to EndQtrFisc(5,4)
Year6
;Beg
                    move begfisc(5) to JULDAYS
                    call cvtgreg
                    pack str4 with CC,YY
                    move str4 to n4
                    sub c1,n4
                    unpack n4,str2,n2
                    move n2 to YY
                    rep zfill,YY
                    call cvtjul
                    move juldays to BegFisc(6)
;End
                    move endfisc(5) to JULDAYS
                    call cvtgreg
                    pack str4 with CC,YY
                    move str4 to n4
                    sub c1,n4
                    unpack n4,str2,n2
                    move n2 to YY
                    rep zfill,YY
                    call cvtjul
                    move juldays,EndFisc(6)
;1st Qtr
                    move begfisc(6) to JULDAYS
                    move      begfisc(6) to  BegQtrFisc(6,1)
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(6,2)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(6,1)
;2nd Qtr
                    move      BegQtrFisc(6,2) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(6,3)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(6,2)
;3rd Qtr
                    move      BegQtrFisc(6,3) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              add c1,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    move      juldays to BegQtrFisc(6,4)
                    sub c1 from juldays
                    move juldays to EndQtrFisc(6,3)
;4th Qtr
                    move      BegQtrFisc(6,4) to Juldays
                    call cvtgreg
                    move      MM to n2
                    add c3 to n2
                    if (n2 > "12")
                              sub "12",n2
                              move      n2 to MM
                              clear n2
                              move YY to n2
                              pack str4 with CC,YY
                              move str4 to n4
                              add c1,n4
                              unpack n4,str2,n2
                              move n2 to YY
                              rep zfill,YY
                    else
                                        move n2 to MM
                    endif
                    call cvtjul
                    sub c1 from juldays
                    move juldays to EndQtrFisc(6,4)




.                   move      "20020701",str8
.                   move      "20030630",str9
.                   move      "20020101",str8
.                   move      "20021231",str9
.                   unpack    str8,cc,yy,mm,dd
.                   call      cvtjul
.                   move juldays to BegFiscPrev 
.                   unpack    str9,cc,yy,mm,dd
.                   call      cvtjul
.                   move juldays to EndFiscPrev


          move lnum to NDATFLD
          move c1 to ndatpath
          CALL NDATKEY
          STOP IF OVER
        DISPLAY     *P10:10,*white,"List:  ",LNUM,"   ",OLSTNAME
;
.Patch 3.8
          move olstname to  HoldLstName
.Patch 3.8




IncHeader
          clear     row
.Patch1.2
.START PATCH 3.7 REPLACED LOGIC
.         prtpage MONTHINCREPORT;*p0:0,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,*ulon,*boldon,"Names ",*boldoff;
..        prtpage MONTHINCREPORT;*p0:7750,*ALIGNMENT=*LEFT,*font=TimesNew10,*ll,*ulon,*boldon,"Names ",*boldoff;
.         prtpage MONTHINCREPORT;*font=TimesNew10I,*boldon,*ulon,"in the News ",*uloff,*boldoff;
.         prtpage MONTHINCREPORT;*p0:160,*font=TimesNew6,*ll,"C A L I F O R N I A   I N C .";
..        prtpage MONTHINCREPORT;*p0:7910,*font=TimesNew6,*ll,"C A L I F O R N I A   I N C .";
          prtpage   MONTHINCREPORT;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 3.7 REPLACED LOGIC
.Patch1.2
;         prtpage MONTHINCREPORT;*pColumn:row,*ALIGNMENT=*Left,*font=,"Confidential";
;         prtpage MONTHINCREPORT;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font8,"Date:";
;         clock   timestamp,str8
;         unpack  str8,str2,yy,mm,dd
;         clear   str10
;         pack    str10,mm,slash,dd,slash,str2,yy
;         prtpage MONTHINCREPORT;*font=font8,str10;
          MOVE "300" to row
          call      trim using olstname
          prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial14,*ll,*boldon,OLSTNAME,*boldoff;
          add       OneandahalfSpaced to row
          prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial12,*boldon,*ll,"QUARTERLY LIST INCOME/VOLUME REPORT";
          add       OneandahalfSpaced to row
.         add       Doublespaced to row
;Header2
          move      begfisc(1) to juldays
;         move BegFiscPrev to juldays 
          call      cvtgreg
          pack      str10 with mm,"/",DD          
;         move EndFiscPrev to juldays 
          move      endfisc(1) to juldays
          call      cvtgreg
          if (LTYPE = "C")
                    pack taskname with "REPORTED ON A CASH BASIS (BY CHECK DATE) FOR ","FISCAL YEAR ",str10," TO ",MM,"/",DD
          elseif (LTYPE = "I")
                    pack taskname with "REPORTED ON AN ACCRUED BASIS (BY INVOICE DATE) FOR ","FISCAL YEAR ",str10," TO ",MM,"/",DD
          endif
          prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial12,*boldon,*ll,TASKNAME,*boldoff;
.         prtpage MONTHINCREPORT;*pHeader11:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"FISCAL YEAR 2003 (01/01/03 - 12/31/03)",*boldoff;
;Header3
;         move BegFiscCur to juldays 
;         call      cvtgreg
;         pack      str10 with mm,"/",DD,"/",YY   
;         move EndFiscCur to juldays 
;         call      cvtgreg
;         pack taskname with "FISCAL YEAR ",CC,YY," (",str10," - ",MM,"/",DD,"/",YY,")"
;         prtpage MONTHINCREPORT;*pHeader11:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,taskname,*boldoff;
;         prtpage MONTHINCREPORT;*pHeader15:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"FISCAL YEAR 2002 (01/01/02 - 12/31/02)",*boldoff;
;         add       OneandaHalfSpaced to row
          add       Doublespaced to row
;         prtpage MONTHINCREPORT;*pBoxOneLeft:row,*ALIGNMENT=*LEFT,*font=Arial11,*ll,*boldon,"Year:",*boldoff;
;         prtpage MONTHINCREPORT;*pBoxTwoLeft:row,*ALIGNMENT=*LEFT,*font=Arial11,*ll,*boldon,"Year:",*boldoff;
;         prtpage MONTHINCREPORT;*pBoxThreeLeft:row,*ALIGNMENT=*LEFT,*font=Arial11,*ll,*boldon,"Year:",*boldoff;
;Old Header
          move      begfisc(6) to juldays
          call      cvtgreg
          pack      str10 with mm,"/",DD,"/",YY   
          move      endfisc(6) to juldays
          call      cvtgreg
          pack taskname with "FISCAL YEAR ",str10," TO ",MM,"/",DD,"/",YY

          prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,Taskname,*boldoff;
          move      begfisc(5) to juldays
          call      cvtgreg
          pack      str10 with mm,"/",DD,"/",YY   
          move      endfisc(5) to juldays
          call      cvtgreg
          pack taskname with "FISCAL YEAR ",str10," TO ",MM,"/",DD,"/",YY
          prtpage MONTHINCREPORT;*pBoxTwoVertText(2):row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,Taskname,*boldoff;
          move      begfisc(4) to juldays
          call      cvtgreg
          pack      str10 with mm,"/",DD,"/",YY   
          move      endfisc(4) to juldays
          call      cvtgreg
          pack taskname with "FISCAL YEAR ",str10," TO ",MM,"/",DD,"/",YY
          prtpage MONTHINCREPORT;*pBoxThreeVertText(2):row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,Taskname,*boldoff;
          add       OneandaHalfSpaced to row
          move      row to BegRowLine
          sub       BegRowline from BegRowline2,BegRowLineDiff
;Second Row
          move      begrowline2 to row
          sub       OneandahalfSpaced from row
          move      begfisc(3) to juldays
          call      cvtgreg
          pack      str10 with mm,"/",DD,"/",YY   
          move      endfisc(3) to juldays
          call      cvtgreg
          pack taskname with "FISCAL YEAR ",str10," TO ",MM,"/",DD,"/",YY
          prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,Taskname,*boldoff;
          move      begfisc(2) to juldays
          call      cvtgreg
          pack      str10 with mm,"/",DD,"/",YY   
          move      endfisc(2) to juldays
          call      cvtgreg
          pack taskname with "FISCAL YEAR ",str10," TO ",MM,"/",DD,"/",YY
          prtpage MONTHINCREPORT;*pBoxTwoVertText(2):row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,Taskname,*boldoff;
          move      begfisc(1) to juldays
          call      cvtgreg
          pack      str10 with mm,"/",DD,"/",YY   
          move      endfisc(1) to juldays
          call      cvtgreg
          pack taskname with "FISCAL YEAR ",str10," TO ",MM,"/",DD,"/",YY
          prtpage MONTHINCREPORT;*pBoxThreeVertText(2):row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,Taskname,*boldoff;

ExcelGrid
          
          move Begrowline to row
;Year 1
          prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight:BoxOneLeft:BoxOneRight
;Year2
          prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight:BoxTwoLeft:BoxTwoRight
;Year3
          prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight:BoxThreeLeft:BoxThreeRight

;
;Year 4
          move row to n9
          move Begrowline2 to row
          prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight2:BoxOneLeft:BoxOneRight
;Year5
          prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight2:BoxTwoLeft:BoxTwoRight
;Year6
          prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight2:BoxThreeLeft:BoxThreeRight
          move n9 to row
;
;Exchange Boxes
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
                    prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*overlayoff,*bgcolor=*YELLOW,*RECT=Row:n8:BoxOneVert(2):BoxOneVert(3)
                    prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*overlayoff,*bgcolor=*YELLOW,*RECT=Row:n8:BoxTwoVert(2):BoxTwoVert(3)
                    prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*overlayoff,*bgcolor=*YELLOW,*RECT=Row:n8:BoxThreeVert(2):BoxThreeVert(3)
          until     (n2 = "6")
                    add       smboxheight to row
                    add       smboxheight to n8
          repeat
          move      n9 to row
;;EndShading
;;
          move row to n9
          move begrowline2 to row
          move begrowline2 to n8
          add  halfsmboxheight to n8
          clear n2
          loop      
                    
                    add       c1 to n2
                    prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*overlayoff,*bgcolor=*YELLOW,*RECT=Row:n8:BoxOneVert(2):BoxOneVert(3)
                    prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*overlayoff,*bgcolor=*YELLOW,*RECT=Row:n8:BoxTwoVert(2):BoxTwoVert(3)
                    prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*overlayoff,*bgcolor=*YELLOW,*RECT=Row:n8:BoxThreeVert(2):BoxThreeVert(3)
          until     (n2 = "6")
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
.         add lgboxheight,totBoxRow
.         add "100",totBoxRow
.         add smboxheight,totboxrow,TotBoxHeight
;
;Rent exchange halfboxes

;
.         prtpage MONTHINCREPORT;*pensize=10,*FILL=*ON,*BGCOLOR=*WHITE,*RECT=TotBoxROW:TotBoxHeight:BoxOneLeft:BoxOneRight
;         prtpage MONTHINCREPORT;*pensize=10,*RECT=TotBoxROW:TotBoxHeight:BoxOneLeft:BoxOneRight
.         prtpage MONTHINCREPORT;*pensize=10,*RECT=TotBoxROW:TotBoxHeight:BoxFourLeft:BoxFourRight

.         prtpage MONTHINCREPORT;*pensize=15,*RECT=TotBoxROW:TotBoxHeight:BoxTwoLeft:BoxTwoRight
;Income 1
.         prtpage MONTHINCREPORT;*pensize=15,*RECT=TotBoxROW:TotBoxHeight:BoxThreeLeft:BoxThreeRight
;Volume2
.         prtpage MONTHINCREPORT;*pensize=15,*RECT=TotBoxROW:TotBoxHeight:BoxFiveLeft:BoxFiveRight
;Actual
.         prtpage MONTHINCREPORT;*pensize=15,*RECT=TotBoxROW:TotBoxHeight:BoxSixLeft:BoxSixRight

;Rent exchange halfboxes
.                   clear n8
.                   add HalfsmboxHeight,totboxrow,n8
.                   prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*bgcolor=*YELLOW,*RECT=totboxrow:n8:BoxTwoLeft1:BoxTwoRight
.                   prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*bgcolor=*YELLOW,*RECT=totboxrow:n8:BoxFiveLeft1:BoxFiveRight
;                   prtpage MONTHINCREPORT;*pBoxTwoLeft1:totboxrow,*pensize=15,*overlayon,*line=BoxTwoRight1:totboxrow;
;                   prtpage MONTHINCREPORT;*pBoxFiveLeft1:totboxrow,*pensize=15,*overlayon,*line=BoxFiveRight1:totboxrow;
;                   sub HalfsmboxHeight,totboxrow

;Vertical Lines For Totals
.         prtpage MONTHINCREPORT;*pBoxTwoVert:TotBoxROW,*bgcolor=*WHITE,*pensize=10,*line=BoxTwoVert:TotBoxHeight;
.         prtpage MONTHINCREPORT;*pBoxThreeVert1:TotBoxROW,*pensize=10,*line=BoxThreeVert1:TotBoxHeight;
.         prtpage MONTHINCREPORT;*pBoxThreeVert2:TotBoxROW,*pensize=10,*line=BoxThreeVert2:TotBoxHeight;
.         prtpage MONTHINCREPORT;*pBoxFiveVert:TotBoxROW,*pensize=10,*line=BoxFiveVert:TotBoxHeight;
.         prtpage MONTHINCREPORT;*pBoxSixVert1:TotBoxROW,*pensize=10,*line=BoxSixVert1:TotBoxHeight;
.         prtpage MONTHINCREPORT;*pBoxSixVert2:TotBoxROW,*pensize=10,*line=BoxSixVert2:TotBoxHeight;
;Title Cell
          move row to n9
;Topmost
.         add "10" to Row
;         prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Revised",*boldoff;
.         sub       "10",Row
;Top

          add       "20" to row
          prtpage MONTHINCREPORT;*pHeader9:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Total",*boldoff;
          prtpage MONTHINCREPORT;*pHeader10:row,*bgcolor=*YELLOW,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Exch. Volume",*boldoff;
          if (LTYPE = "C")
                    prtpage MONTHINCREPORT;*pHeader11:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Checks",*boldoff;
          endif
          prtpage MONTHINCREPORT;*pHeader12:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Total",*boldoff;
          prtpage MONTHINCREPORT;*pHeader13:row,*bgcolor=*YELLOW,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Exch. Volume",*boldoff;
          if (LTYPE = "C")
                    prtpage MONTHINCREPORT;*pHeader14:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Checks",*boldoff;
          endif
          prtpage MONTHINCREPORT;*pHeader15:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Total",*boldoff;
          prtpage MONTHINCREPORT;*pHeader16:row,*bgcolor=*YELLOW,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Exch. Volume",*boldoff;
          if (LTYPE = "C")
                    prtpage MONTHINCREPORT;*pHeader17:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Checks",*boldoff;
          endif
          sub       "20" to row
;TopMiddle
.         add       "120" to row
;         prtpage MONTHINCREPORT;*pHeader12:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Projections",*boldoff;
.         sub       "120" to row
;Middle Centered
          add singlespaced to row
.         prtpage MONTHINCREPORT;*pHeader11:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Projections",*boldoff;
.         prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Actual",*boldoff;
.         prtpage MONTHINCREPORT;*pHeader13:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Variance",*boldoff;
.         prtpage MONTHINCREPORT;*pHeader16:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Projections",*boldoff;
.         prtpage MONTHINCREPORT;*pHeader17:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Actual",*boldoff;
.         prtpage MONTHINCREPORT;*pHeader18:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Variance",*boldoff;
;Second Line
          add "20" to row
.         add oneandahalfspaced to row
          prtpage MONTHINCREPORT;*pHeader9:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial8,*ll,*boldon,"Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader10:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial8,*ll,*boldon,"Rent Volume",*boldoff;
          if (LTYPE = "C")
                    prtpage MONTHINCREPORT;*pHeader11:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Paid",*boldoff;
          elseif (LTYPE = "I")
                    prtpage MONTHINCREPORT;*pHeader11:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Invoiced",*boldoff;
          endif
          prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial8,*ll,*boldon,"Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader13:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial8,*ll,*boldon,"Rent Volume",*boldoff;
          if (LTYPE = "C")
                    prtpage MONTHINCREPORT;*pHeader14:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Paid",*boldoff;
          elseif (LTYPE = "I")
                    prtpage MONTHINCREPORT;*pHeader14:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Invoiced",*boldoff;
          endif
          prtpage MONTHINCREPORT;*pHeader15:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial8,*ll,*boldon,"Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader16:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial8,*ll,*boldon,"Rent Volume",*boldoff;
          if (LTYPE = "C")
                    prtpage MONTHINCREPORT;*pHeader17:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Paid",*boldoff;
          elseif (LTYPE = "I")
                    prtpage MONTHINCREPORT;*pHeader17:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Invoiced",*boldoff;
          endif
.         prtpage MONTHINCREPORT;*pHeader9:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial10,*ll,*boldon,"Volume",*boldoff;
.         prtpage MONTHINCREPORT;*pHeader10:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Rental Volume",*boldoff;
.         prtpage MONTHINCREPORT;*pHeader14:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial10,*ll,*boldon,"Volume",*boldoff;
.         prtpage MONTHINCREPORT;*pHeader15:row,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Rental Volume",*boldoff;
;         prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Revised as of 8/13/03",*boldoff;
          add "20" to row
;         prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Revised as of 8/13/03",*boldoff;
          move n9 to row

;CellHeaders2
          move begrowline2 to row
          add       "20" to row
          prtpage MONTHINCREPORT;*pHeader9:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Total",*boldoff;
          prtpage MONTHINCREPORT;*pHeader10:row,*bgcolor=*YELLOW,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Exch. Volume",*boldoff;
          if (LTYPE = "C")
                    prtpage MONTHINCREPORT;*pHeader11:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Checks",*boldoff;
          endif
          prtpage MONTHINCREPORT;*pHeader12:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Total",*boldoff;
          prtpage MONTHINCREPORT;*pHeader13:row,*bgcolor=*YELLOW,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Exch. Volume",*boldoff;
          if (LTYPE = "C")
                    prtpage MONTHINCREPORT;*pHeader14:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Checks",*boldoff;
          endif
          prtpage MONTHINCREPORT;*pHeader15:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Total",*boldoff;
          prtpage MONTHINCREPORT;*pHeader16:row,*bgcolor=*YELLOW,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Exch. Volume",*boldoff;
          if (LTYPE = "C")
                    prtpage MONTHINCREPORT;*pHeader17:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Checks",*boldoff;
          endif
          sub       "20" to row
          add singlespaced to row
;Second Line
          add "20" to row
.         add oneandahalfspaced to row
          prtpage MONTHINCREPORT;*pHeader9:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial8,*ll,*boldon,"Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader10:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial8,*ll,*boldon,"Rent Volume",*boldoff;
          if (LTYPE = "C")
                    prtpage MONTHINCREPORT;*pHeader11:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Paid",*boldoff;
          elseif (LTYPE = "I") 
                    prtpage MONTHINCREPORT;*pHeader11:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Invoiced",*boldoff;
          endif
          prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial8,*ll,*boldon,"Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader13:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial8,*ll,*boldon,"Rent Volume",*boldoff;
          if (LTYPE = "C")
                    prtpage MONTHINCREPORT;*pHeader14:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Paid",*boldoff;
          elseif (LTYPE = "I")
                    prtpage MONTHINCREPORT;*pHeader14:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Invoiced",*boldoff;
          endif
          prtpage MONTHINCREPORT;*pHeader15:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial8,*ll,*boldon,"Volume",*boldoff;
          prtpage MONTHINCREPORT;*pHeader16:row,*ALIGNMENT=*CENTER,*bgcolor=*WHITE,*font=Arial8,*ll,*boldon,"Rent Volume",*boldoff;
          if (LTYPE = "C")
                    prtpage MONTHINCREPORT;*pHeader17:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Paid",*boldoff;
          elseif (LTYPE = "I")
                    prtpage MONTHINCREPORT;*pHeader17:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Invoiced",*boldoff;
          endif
          add "20" to row
          move n9 to row
;Month Boxes
.         add       SmBoxHeight to row
.         prtpage MONTHINCREPORT;*pensize=10,*RECT=ROW:LgBoxHeight:BoxOneLeft:BoxOneRight
.         prtpage MONTHINCREPORT;*pensize=10,*RECT=ROW:LgBoxHeight:BoxFourLeft:BoxFourRight
;Months Inserted
.         Clear N2
.         move row to n9
.         add       halfsmboxheight to row
.         add       "40" to row
.         loop      
;
.                   add       c1 to n2
.                   if (FiscMonth = c1)
.                             load      str3 with n2,"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"
.                   else
.                                       If (fiscmonth = 2)
.                                                 load      str3,n2,"FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN"
.                                       elseif (fiscmonth = 3) 
.                                                 load      str3,n2,"MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB"
.                                       elseif (fiscmonth = 4) 
.                                                 load      str3,n2,"APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR"
.                                       elseif (fiscmonth = 5) 
.                                                 load      str3,n2,"MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR"
.                                       elseif (fiscmonth = 6) 
.                                                 load      str3,n2,"JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY"
.                                       elseif (fiscmonth = 7) 
.                                                 load      str3,n2,"JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN"
.                                       elseif (fiscmonth = 8) 
.                                                 load      str3,n2,"AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL"
.                                       elseif (fiscmonth = 9) 
.                                                 load      str3,n2,"SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","OCT"
.                                       elseif (fiscmonth = 10) 
.                                                 load      str3,n2,"OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","OCT","SEP"
.                                       elseif (fiscmonth = 11) 
.                                                 load      str3,n2,"NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","OCT","SEP","OCT"
.                                       elseif (fiscmonth = 12) 
.                                                 load      str3,n2,"DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","OCT","SEP","OCT","NOV"
.                                       endif
.                   endif
;
;                   add       c1 to n2
;                   load      str3 with n2,"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"
.                   prtpage MONTHINCREPORT;*p350:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,str3;
.                   prtpage MONTHINCREPORT;*p5600:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,str3;
.                   add       SmBoxHeight to row
.         until     (n2 = "12")
.         repeat
;Total Inserted
.         add       halfsmboxheight to Totboxrow
.         add       "40" to Totboxrow
.                   prtpage MONTHINCREPORT;*p350:TotBoxRow,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*ulon,*boldon,"Total";
.                   prtpage MONTHINCREPORT;*p5600:TotBoxRow,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*ulon,*boldon,"Total";

.         sub       halfsmboxheight to Totboxrow
.         sub       "40" to Totboxrow
.         move n9 to row


.         prtpage MONTHINCREPORT;*pBoxThreeVert1:BegRowLine,*pensize=10,*line=BoxThreeVert1:LgBoxHeight;
.         prtpage MONTHINCREPORT;*pBoxThreeVert2:BegRowLine,*pensize=10,*line=BoxThreeVert2:LgBoxHeight;
.         prtpage MONTHINCREPORT;*pBoxFiveVert:BegRowLine,*pensize=10,*line=BoxFiveVert:LgBoxHeight;
.         prtpage MONTHINCREPORT;*pBoxSixVert1:BegRowLine,*pensize=10,*line=BoxSixVert1:LgBoxHeight;
.         prtpage MONTHINCREPORT;*pBoxSixVert2:BegRowLine,*pensize=10,*line=BoxSixVert2:LgBoxHeight;
;Fill Months in
;         prtclose monthincreport




;Order Search
          MOVE      C1 TO NORDPATH
          Pack      NORDFLD2,"02L",lSTNUM
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
.                   MOVE      OODTEM,MM
.                   MOVE      OODTEY,YY
.                   MOVE      OODTED,DD
.                   call      cvtjul
                    Goto Enter
          else
.PATCH1.3
                    if (manualflag <> "Y")
                              goto READIT1
                    else
                              goto ENDPROGRAM
                    endif
.PATCH1.3

.                   goto HITIT
          Endif
;Last Years Orders
          Loop      
                    call      nordkg

          until     OVER

                    IF NOT OVER
.PATCH2.0
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
.PATCH2.0
.                             MOVE      OODTEM,MM
.                             MOVE      OODTEY,YY
.                             MOVE      OODTED,DD
.                             call      cvtjul
                    endif
Enter
                    clear     rqty
                    clear     exqty
                    if ((Juldays >= BegFisc(1)) & (Juldays  <=  EndFisc(1)))
                              move c1 to FiscYearFlag
                    elseif ((Juldays >= BegFisc(2)) & (Juldays  <=  EndFisc(2)))
                              move c2 to FiscYearFlag
                    elseif ((Juldays >= BegFisc(3)) & (Juldays  <=  EndFisc(3)))
                              move c3 to FiscYearFlag
                    elseif ((Juldays >= BegFisc(4)) & (Juldays  <=  EndFisc(4)))
                              move c4 to FiscYearFlag
                    elseif ((Juldays >= BegFisc(5)) & (Juldays  <=  EndFisc(5)))
                              move c5 to FiscYearFlag
                    elseif ((Juldays >= BegFisc(6)) & (Juldays  <=  EndFisc(6)))
                              move c6 to FiscYearFlag
                    else
                              clear FISCYEARFLAG
                              Goto NotInRange
                    endif


.Patch2.0
                    goto notinrange if (Juldays > TODAYIS)
.Patch2.0
;                   if ((Juldays >= BegFiscPrev) & (Juldays  <=  EndFiscPrev))
                              if (OSTAT = "B" or OSTAT = "0")
                                        clear TMPVAR
                                        if (oexqty > "0")
                                                  move oexqty to n9
                                                  move      oqty to n10
                                                  sub n9 from n10,TMPVAR 
                                                  move      TMPVAR to RQTY
                                                  move      n9 to EXQTY
                                                  if (fiscyearflag = c1)
                                                            add       EXQTY to EXCHTOT(1) 
                                                            add       EXQTY to ORDTOT(1)
                                                            add       RQTY to RENTTOT(1)
                                                            add       RQTY to ORDTOT(1)
                                                  elseif (fiscyearflag = c2)
                                                            add       EXQTY to EXCHTOT(2) 
                                                            add       EXQTY to ORDTOT(2)
                                                            add       RQTY to RENTTOT(2)
                                                            add       RQTY to ORDTOT(2)
                                                  elseif (fiscyearflag = c3)
                                                            add       EXQTY to EXCHTOT(3) 
                                                            add       EXQTY to ORDTOT(3)
                                                            add       RQTY to RENTTOT(3)
                                                            add       RQTY to ORDTOT(3)
                                                  elseif (fiscyearflag = c4)
                                                            add       EXQTY to EXCHTOT(4) 
                                                            add       EXQTY to ORDTOT(4)
                                                            add       RQTY to RENTTOT(4)
                                                            add       RQTY to ORDTOT(4)
                                                  elseif (fiscyearflag = c5)
                                                            add       EXQTY to EXCHTOT(5) 
                                                            add       EXQTY to ORDTOT(5)
                                                            add       RQTY to RENTTOT(5)
                                                            add       RQTY to ORDTOT(5)
                                                  elseif (fiscyearflag = c6)
                                                            add       EXQTY to EXCHTOT(6) 
                                                            add       EXQTY to ORDTOT(6)
                                                            add       RQTY to RENTTOT(6)
                                                            add       RQTY to ORDTOT(6)
                                                  endif
                                        else
                                                  reset excodes
                                                  scan oelcode in excodes
                                                  if equal
                                                            move oqty to EXQTY
                                                            if (fiscyearflag = C1)
                                                            add       EXQTY to EXCHTOT(1)
                                                            add       EXQTY to ORDTOT(1)
                                                            elseif (fiscyearflag = C2)
                                                            add       EXQTY to EXCHTOT(2)
                                                            add       EXQTY to ORDTOT(2)
                                                            elseif (fiscyearflag = C3)
                                                            add       EXQTY to EXCHTOT(3)
                                                            add       EXQTY to ORDTOT(3)
                                                            elseif (fiscyearflag = C4)
                                                            add       EXQTY to EXCHTOT(4)
                                                            add       EXQTY to ORDTOT(4)
                                                            elseif (fiscyearflag = C5)
                                                            add       EXQTY to EXCHTOT(5)
                                                            add       EXQTY to ORDTOT(5)
                                                            elseif (fiscyearflag = C6)
                                                            add       EXQTY to EXCHTOT(6)
                                                            add       EXQTY to ORDTOT(6)
                                                            endif
                                                  else
                                                            move oqty to RQTY
                                                            if (fiscyearflag = C1)
                                                                      add       RQTY to RENTTOT(1)
                                                                      add       RQTY to ORDTOT(1)
                                                            elseif (fiscyearflag = C2)
                                                                      add       RQTY to RENTTOT(2)
                                                                      add       RQTY to ORDTOT(2)
                                                            elseif (fiscyearflag = C3)
                                                                      add       RQTY to RENTTOT(3)
                                                                      add       RQTY to ORDTOT(3)
                                                            elseif (fiscyearflag = C4)
                                                                      add       RQTY to RENTTOT(4)
                                                                      add       RQTY to ORDTOT(4)
                                                            elseif (fiscyearflag = C5)
                                                                      add       RQTY to RENTTOT(5)
                                                                      add       RQTY to ORDTOT(5)
                                                            elseif (fiscyearflag = C6)
                                                                      add       RQTY to RENTTOT(6)
                                                                      add       RQTY to ORDTOT(6)
                                                            endif
                                                  endif
                                        endif
                                                  call      cvtgreg   
                                                  clear n2
                                                  IF (DATEBY = "M")
                                                            MOVE      OMDTEM,N2
                                                  else
                                                            MOVE      OODTEM,N2
                                                  endif
.                                                 move      oodtem to n2
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
;                                                           If (fiscmonth = 2)
;                                                                     load      MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
;                                                           elseif (fiscmonth = 3) 
;                                                                     load      MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
;                                                           elseif (fiscmonth = 4) 
;                                                                     load      MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
;                                                           elseif (fiscmonth = 5) 
;                                                                     load      MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
;                                                           elseif (fiscmonth = 6) 
;                                                                     load      MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
;                                                           elseif (fiscmonth = 7) 
;                                                                     move mm to n2
;                                                                     load      MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
;                                                           elseif (fiscmonth = 8) 
;                                                                     load      MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
;                                                           elseif (fiscmonth = 9) 
;                                                                     load      MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
;                                                           elseif (fiscmonth = 10) 
;                                                                     load      MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
;                                                           elseif (fiscmonth = 11) 
;                                                                     load      MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
;                                                           elseif (fiscmonth = 12) 
;                                                                     load      MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
;                                                           endif
                                                  EndIf
                                                  move mm to n2
;                                                 call      cvtgreg   
;                                                 clear n2
;                                                 move      mm to n2
;Qty 
                                                  clear dumqty
                                                  clear n9
                                                  clear str15
                                                  if (fiscyearflag = C1)
                                                            load      str15,n2,JAN(1),FEB(1),MAR(1),APR(1),MAY(1),JUN(1),JUL(1),AUG(1),SEP(1),OCT(1),NOV(1),DEC(1)        
                                                  elseif (fiscyearflag = C2)
                                                            load      str15,n2,JAN(2),FEB(2),MAR(2),APR(2),MAY(2),JUN(2),JUL(2),AUG(2),SEP(2),OCT(2),NOV(2),DEC(2)        
                                                  elseif (fiscyearflag = C3)
                                                            load      str15,n2,JAN(3),FEB(3),MAR(3),APR(3),MAY(3),JUN(3),JUL(3),AUG(3),SEP(3),OCT(3),NOV(3),DEC(3)        
                                                  elseif (fiscyearflag = C4)
                                                            load      str15,n2,JAN(4),FEB(4),MAR(4),APR(4),MAY(4),JUN(4),JUL(4),AUG(4),SEP(4),OCT(4),NOV(4),DEC(4)        
                                                  elseif (fiscyearflag = C5)
                                                            load      str15,n2,JAN(5),FEB(5),MAR(5),APR(5),MAY(5),JUN(5),JUL(5),AUG(5),SEP(5),OCT(5),NOV(5),DEC(5)        
                                                  elseif (fiscyearflag = C6)
                                                            load      str15,n2,JAN(6),FEB(6),MAR(6),APR(6),MAY(6),JUN(6),JUL(6),AUG(6),SEP(6),OCT(6),NOV(6),DEC(6)        
                                                  endif
                                                  move      str15 to dumqty
                                                  move      oqty to n9
                                                  add       n9 to dumqty
                                                  move dumqty to str15
                                                  if (fiscyearflag = C1)
                                                            store     str15,n2,JAN(1),FEB(1),MAR(1),APR(1),MAY(1),JUN(1),JUL(1),AUG(1),SEP(1),OCT(1),NOV(1),DEC(1)        
                                                  elseif (fiscyearflag = C2)
                                                            store     str15,n2,JAN(2),FEB(2),MAR(2),APR(2),MAY(2),JUN(2),JUL(2),AUG(2),SEP(2),OCT(2),NOV(2),DEC(2)        
                                                  elseif (fiscyearflag = C3)
                                                            store     str15,n2,JAN(3),FEB(3),MAR(3),APR(3),MAY(3),JUN(3),JUL(3),AUG(3),SEP(3),OCT(3),NOV(3),DEC(3)        
                                                  elseif (fiscyearflag = C4)
                                                            store     str15,n2,JAN(4),FEB(4),MAR(4),APR(4),MAY(4),JUN(4),JUL(4),AUG(4),SEP(4),OCT(4),NOV(4),DEC(4)        
                                                  elseif (fiscyearflag = C5)
                                                            store     str15,n2,JAN(5),FEB(5),MAR(5),APR(5),MAY(5),JUN(5),JUL(5),AUG(5),SEP(5),OCT(5),NOV(5),DEC(5)        
                                                  elseif (fiscyearflag = C6)
                                                            store     str15,n2,JAN(6),FEB(6),MAR(6),APR(6),MAY(6),JUN(6),JUL(6),AUG(6),SEP(6),OCT(6),NOV(6),DEC(6)        
                                                  endif
;                                                 store     str15,n2,JAN2,FEB2,MAR2,APR2,MAY2,JUN2,JUL2,AUG2,SEP2,OCT2,NOV2,DEC2  
;Rental
                                                  clear dumqty
                                                  clear n9
                                                  clear str15
                                                  if (fiscyearflag = C1)
                                                            load      str15,n2,JANRENT(1),FEBRENT(1),MARRENT(1),APRRENT(1),MAYRENT(1),JUNRENT(1),JULRENT(1),AUGRENT(1),SEPRENT(1),OCTRENT(1),NOVRENT(1),DECRENT(1)          
                                                  elseif (fiscyearflag = C2)
                                                            load      str15,n2,JANRENT(2),FEBRENT(2),MARRENT(2),APRRENT(2),MAYRENT(2),JUNRENT(2),JULRENT(2),AUGRENT(2),SEPRENT(2),OCTRENT(2),NOVRENT(2),DECRENT(2)          
                                                  elseif (fiscyearflag = C3)
                                                            load      str15,n2,JANRENT(3),FEBRENT(3),MARRENT(3),APRRENT(3),MAYRENT(3),JUNRENT(3),JULRENT(3),AUGRENT(3),SEPRENT(3),OCTRENT(3),NOVRENT(3),DECRENT(3)          
                                                  elseif (fiscyearflag = C4)
                                                            load      str15,n2,JANRENT(4),FEBRENT(4),MARRENT(4),APRRENT(4),MAYRENT(4),JUNRENT(4),JULRENT(4),AUGRENT(4),SEPRENT(4),OCTRENT(4),NOVRENT(4),DECRENT(4)          
                                                  elseif (fiscyearflag = C5)
                                                            load      str15,n2,JANRENT(5),FEBRENT(5),MARRENT(5),APRRENT(5),MAYRENT(5),JUNRENT(5),JULRENT(5),AUGRENT(5),SEPRENT(5),OCTRENT(5),NOVRENT(5),DECRENT(5)          
                                                  elseif (fiscyearflag = C6)
                                                            load      str15,n2,JANRENT(6),FEBRENT(6),MARRENT(6),APRRENT(6),MAYRENT(6),JUNRENT(6),JULRENT(6),AUGRENT(6),SEPRENT(6),OCTRENT(6),NOVRENT(6),DECRENT(6)          
                                                  endif
;                                                 load      str15,n2,JANRENT2,FEBRENT2,MARRENT2,APRRENT2,MAYRENT2,JUNRENT2,JULRENT2,AUGRENT2,SEPRENT2,OCTRENT2,NOVRENT2,DECRENT2    
                                                  move      str15 to dumqty
                                                  move      rqty to n9
                                                  add                   n9 to dumqty
                                                  move dumqty to str15
                                                  if (fiscyearflag = C1)
                                                            store     str15,n2,JANRENT(1),FEBRENT(1),MARRENT(1),APRRENT(1),MAYRENT(1),JUNRENT(1),JULRENT(1),AUGRENT(1),SEPRENT(1),OCTRENT(1),NOVRENT(1),DECRENT(1)          
                                                  elseif (fiscyearflag = C2)
                                                            store     str15,n2,JANRENT(2),FEBRENT(2),MARRENT(2),APRRENT(2),MAYRENT(2),JUNRENT(2),JULRENT(2),AUGRENT(2),SEPRENT(2),OCTRENT(2),NOVRENT(2),DECRENT(2)          
                                                  elseif (fiscyearflag = C3)
                                                            store     str15,n2,JANRENT(3),FEBRENT(3),MARRENT(3),APRRENT(3),MAYRENT(3),JUNRENT(3),JULRENT(3),AUGRENT(3),SEPRENT(3),OCTRENT(3),NOVRENT(3),DECRENT(3)          
                                                  elseif (fiscyearflag = C4)
                                                            store     str15,n2,JANRENT(4),FEBRENT(4),MARRENT(4),APRRENT(4),MAYRENT(4),JUNRENT(4),JULRENT(4),AUGRENT(4),SEPRENT(4),OCTRENT(4),NOVRENT(4),DECRENT(4)          
                                                  elseif (fiscyearflag = C5)
                                                            store     str15,n2,JANRENT(5),FEBRENT(5),MARRENT(5),APRRENT(5),MAYRENT(5),JUNRENT(5),JULRENT(5),AUGRENT(5),SEPRENT(5),OCTRENT(5),NOVRENT(5),DECRENT(5)          
                                                  elseif (fiscyearflag = C6)
                                                            store     str15,n2,JANRENT(6),FEBRENT(6),MARRENT(6),APRRENT(6),MAYRENT(6),JUNRENT(6),JULRENT(6),AUGRENT(6),SEPRENT(6),OCTRENT(6),NOVRENT(6),DECRENT(6)          
                                                  endif
;                                                 store     str15,n2,JANRENT2,FEBRENT2,MARRENT2,APRRENT2,MAYRENT2,JUNRENT2,JULRENT2,AUGRENT2,SEPRENT2,OCTRENT2,NOVRENT2,DECRENT2    
;Exch
                                                  clear dumqty
                                                  clear n9
                                                  clear str15
                                                  if (fiscyearflag = C1)
                                                            load      str15,n2,JANEXCH(1),FEBEXCH(1),MAREXCH(1),APREXCH(1),MAYEXCH(1),JUNEXCH(1),JULEXCH(1),AUGEXCH(1),SEPEXCH(1),OCTEXCH(1),NOVEXCH(1),DECEXCH(1)          
                                                  elseif (fiscyearflag = C2)
                                                            load      str15,n2,JANEXCH(2),FEBEXCH(2),MAREXCH(2),APREXCH(2),MAYEXCH(2),JUNEXCH(2),JULEXCH(2),AUGEXCH(2),SEPEXCH(2),OCTEXCH(2),NOVEXCH(2),DECEXCH(2)          
                                                  elseif (fiscyearflag = C3)
                                                            load      str15,n2,JANEXCH(3),FEBEXCH(3),MAREXCH(3),APREXCH(3),MAYEXCH(3),JUNEXCH(3),JULEXCH(3),AUGEXCH(3),SEPEXCH(3),OCTEXCH(3),NOVEXCH(3),DECEXCH(3)          
                                                  elseif (fiscyearflag = C4)
                                                            load      str15,n2,JANEXCH(4),FEBEXCH(4),MAREXCH(4),APREXCH(4),MAYEXCH(4),JUNEXCH(4),JULEXCH(4),AUGEXCH(4),SEPEXCH(4),OCTEXCH(4),NOVEXCH(4),DECEXCH(4)          
                                                  elseif (fiscyearflag = C5)
                                                            load      str15,n2,JANEXCH(5),FEBEXCH(5),MAREXCH(5),APREXCH(5),MAYEXCH(5),JUNEXCH(5),JULEXCH(5),AUGEXCH(5),SEPEXCH(5),OCTEXCH(5),NOVEXCH(5),DECEXCH(5)          
                                                  elseif (fiscyearflag = C6)
                                                            load      str15,n2,JANEXCH(6),FEBEXCH(6),MAREXCH(6),APREXCH(6),MAYEXCH(6),JUNEXCH(6),JULEXCH(6),AUGEXCH(6),SEPEXCH(6),OCTEXCH(6),NOVEXCH(6),DECEXCH(6)          
                                                  endif

;                                                 load      str15,n2,JANEXCH2,FEBEXCH2,MAREXCH2,APREXCH2,MAYEXCH2,JUNEXCH2,JULEXCH2,AUGEXCH2,SEPEXCH2,OCTEXCH2,NOVEXCH2,DECEXCH2    
                                                  move      str15 to dumqty
                                                  move      exqty to n9
                                                  add                   n9 to dumqty
                                                  move dumqty to str15
                                                  if (fiscyearflag = C1)
                                                            store     str15,n2,JANEXCH(1),FEBEXCH(1),MAREXCH(1),APREXCH(1),MAYEXCH(1),JUNEXCH(1),JULEXCH(1),AUGEXCH(1),SEPEXCH(1),OCTEXCH(1),NOVEXCH(1),DECEXCH(1)          
                                                  elseif (fiscyearflag = C2)
                                                            store     str15,n2,JANEXCH(2),FEBEXCH(2),MAREXCH(2),APREXCH(2),MAYEXCH(2),JUNEXCH(2),JULEXCH(2),AUGEXCH(2),SEPEXCH(2),OCTEXCH(2),NOVEXCH(2),DECEXCH(2)          
                                                  elseif (fiscyearflag = C3)
                                                            store     str15,n2,JANEXCH(3),FEBEXCH(3),MAREXCH(3),APREXCH(3),MAYEXCH(3),JUNEXCH(3),JULEXCH(3),AUGEXCH(3),SEPEXCH(3),OCTEXCH(3),NOVEXCH(3),DECEXCH(3)          
                                                  elseif (fiscyearflag = C4)
                                                            store     str15,n2,JANEXCH(4),FEBEXCH(4),MAREXCH(4),APREXCH(4),MAYEXCH(4),JUNEXCH(4),JULEXCH(4),AUGEXCH(4),SEPEXCH(4),OCTEXCH(4),NOVEXCH(4),DECEXCH(4)          
                                                  elseif (fiscyearflag = C5)
                                                            store     str15,n2,JANEXCH(5),FEBEXCH(5),MAREXCH(5),APREXCH(5),MAYEXCH(5),JUNEXCH(5),JULEXCH(5),AUGEXCH(5),SEPEXCH(5),OCTEXCH(5),NOVEXCH(5),DECEXCH(5)          
                                                  elseif (fiscyearflag = C6)
                                                            store     str15,n2,JANEXCH(6),FEBEXCH(6),MAREXCH(6),APREXCH(6),MAYEXCH(6),JUNEXCH(6),JULEXCH(6),AUGEXCH(6),SEPEXCH(6),OCTEXCH(6),NOVEXCH(6),DECEXCH(6)          
                                                  endif
;                                                 store     str15,n2,JANEXCH2,FEBEXCH2,MAREXCH2,APREXCH2,MAYEXCH2,JUNEXCH2,JULEXCH2,AUGEXCH2,SEPEXCH2,OCTEXCH2,NOVEXCH2,DECEXCH2    
                              endif
;                                                 store     str15,n2,JANEXCH2,FEBEXCH2,MAREXCH2,APREXCH2,MAYEXCH2,JUNEXCH2,JULEXCH2,AUGEXCH2,SEPEXCH2,OCTEXCH2,NOVEXCH2,DECEXCH2    
;;
;Invoice Part
.                                       MOVE      OLRN TO NINVFLD
.                                       If        (OSTAT = "B" | OSTAT="Q")
.                                                 move      c1 to ninvpath
.                                                 CALL      NINVKEY
.                                                 if Not OVER
.                                                           CMATCH    "P" TO STATB                ;PAID
.                                                           If Equal
.                                                                               move      "01" to n2
.                                                                               move      n2 to str2
.                                                                               rep       zfill in str2
.                                                                               CLEAR     NJSTFLD
.                                                                     PACK      NJSTFLD FROM INVNUM,str2
.                                                                               rep       zfill in njstfld
.                                                                               CALL      NJSTKEY
.                                                                               if not over
.                                                                                         add jstap1 to ap1
.                                                                                         loop
.                                                                                                   add       c1 to n2
.                                                                                                   move      n2 to str2
.                                                                                                   rep       zfill in str2
.                                                                                                   CLEAR     NJSTFLD
.                                                                                                   PACK      NJSTFLD FROM INVNUM,str2
.                                                                                                   rep       zfill in njstfld
.                                                                                                   CALL      NJSTKEY
.                                                                                         until over
.                                                                                                   add jstap1 to ap1
.                                                                                         repeat
.                                                                               endif
.                                                                               move      CHK1DTEM to N2
;IncomePart
.                                                                               clear dumqty
.                                                                               clear n9
.                                                                               clear str15
.                                                                               load      num102,n2,AP1JAN2,AP1FEB2,AP1MAR2,AP1APR2,AP1MAY2,AP1JUN2,AP1JUL2,AP1AUG2,AP1SEP2,AP1OCT2,AP1NOV2,AP1DEC2     
.                                                                               add ap1 to num102
.                                                                               add       ap1 to AP1TOT2
.                                                                               store     num102,n2,AP1JAN2,AP1FEB2,AP1MAR2,AP1APR2,AP1MAY2,AP1JUN2,AP1JUL2,AP1AUG2,AP1SEP2,AP1OCT2,AP1NOV2,AP1DEC2     
;;;;
.                                                           Endif
.                                                 Endif
.                                       Endif
;;
.                   MOVE      OODTEM,MM
.                   MOVE      OODTEY,YY
.                   MOVE      OODTED,DD
.                   call      cvtjul
.                   elseif ((Juldays >= BegFiscCur) & (Juldays  <=  EndFiscCur))
;                   elseif (Juldays >= BegFiscCur & Juldays  <=  EndFiscCur)
.                             if (OSTAT = "B" or OSTAT = "0")
.                                       clear TMPVAR
.                                       if (oexqty > "0")
.                                                 move oexqty to n9
.                                                 move      oqty to n10
.                                                 sub n9 from n10,TMPVAR 
..                                                move      TMPVAR to RQTY
.                                                 move      n9 to EXQTY
.                                                           add       EXQTY to EXCHTOT1 
.                                                           add       EXQTY to ORDTOT1
.                                                           add       RQTY to RENTTOT1 
.                                                           add       RQTY to ORDTOT1
.                                       else
.                                                 reset excodes
.                                                 scan oelcode in excodes
.                                                 if equal
..                                                          move oqty to EXQTY
.                                                           add       EXQTY to EXCHTOT1
.                                                           add       EXQTY to ORDTOT1
.                                                 else
.                                                           move oqty to RQTY
.                                                           add       RQTY to RENTTOT1
.                                                           add       RQTY to ORDTOT1
.                                                 endif
.                                       endif
.                                                 call      cvtgreg   
.                                                 if (mm = "01")
.                                                           reset     mm
.                                                 endif
;
.For Fiscal
.                                                 If (FiscMonth <> c1)
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
.                                                 EndIf
.                                                 move      mm to n2
;Qty 
.                                                 clear dumqty
.                                                 clear n9
.                                                 clear str15
.                                                 load      str15,n2,JAN1,FEB1,MAR1,APR1,MAY1,JUN1,JUL1,AUG1,SEP1,OCT1,NOV1,DEC1  
.                                                 move      str15 to dumqty
.                                                 move      oqty to n9
.                                                 add       N9 to dumqty
.                                                 move dumqty to str15
.                                                 store     str15,n2,JAN1,FEB1,MAR1,APR1,MAY1,JUN1,JUL1,AUG1,SEP1,OCT1,NOV1,DEC1  
;Rental
.                                                 clear dumqty
.                                                 clear n9
.                                                 clear str15
.                                                 load      str15,n2,JANRENT1,FEBRENT1,MARRENT1,APRRENT1,MAYRENT1,JUNRENT1,JULRENT1,AUGRENT1,SEPRENT1,OCTRENT1,NOVRENT1,DECRENT1    
.                                                 move      str15 to dumqty
.                                                 move      rqty to n9
.                                                 add       n9 to dumqty
.                                                 move dumqty to str15
.                                                 store     str15,n2,JANRENT1,FEBRENT1,MARRENT1,APRRENT1,MAYRENT1,JUNRENT1,JULRENT1,AUGRENT1,SEPRENT1,OCTRENT1,NOVRENT1,DECRENT1    
;Exch
.                                                 clear dumqty
.                                                 clear n9
.                                                 clear str15
.                                                 load      str15,n2,JANEXCH1,FEBEXCH1,MAREXCH1,APREXCH1,MAYEXCH1,JUNEXCH1,JULEXCH1,AUGEXCH1,SEPEXCH1,OCTEXCH1,NOVEXCH1,DECEXCH1    
.                                                 move      str15 to dumqty
.                                                 move      exqty to n9
.                                                 add       n9 to dumqty
.                                                 move dumqty to str15
.                                                 store     str15,n2,JANEXCH1,FEBEXCH1,MAREXCH1,APREXCH1,MAYEXCH1,JUNEXCH1,JULEXCH1,AUGEXCH1,SEPEXCH1,OCTEXCH1,NOVEXCH1,DECEXCH1    
.                             endif
;;
.                   endif
NotInRange
          call      INVOICEPART

.
          repeat 
;Yr1
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
          prtpage MONTHINCREPORT;*pBoxOneLeftText:row,*ALIGNMENT=*Left,*font=Arial9,*ll,"1st Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxOneLeftText:row,*ALIGNMENT=*Left,*font=Arial9,*ll,"2nd Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxOneLeftText:row,*ALIGNMENT=*Left,*font=Arial9,*ll,"3rd Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxOneLeftText:row,*ALIGNMENT=*Left,*font=Arial9,*ll,"4th Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxOneLeftText:row,*ALIGNMENT=*Left,*font=Arial10,*ll,"Totals";
;Yr2
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
          prtpage MONTHINCREPORT;*pBoxTwoLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"1st Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxTwoLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"2nd Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxTwoLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"3rd Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxTwoLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"4th Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxTwoLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"TOTALS";
;Yr3
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
          prtpage MONTHINCREPORT;*pBoxThreeLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"1st Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxThreeLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"2nd Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxThreeLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"3rd Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxThreeLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"4th Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxThreeLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"TOTALS";
          move      row to n9
;Yr4
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
          prtpage MONTHINCREPORT;*pBoxOneLeftText:row,*ALIGNMENT=*Left,*font=Arial9,*ll,"1st Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxOneLeftText:row,*ALIGNMENT=*Left,*font=Arial9,*ll,"2nd Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxOneLeftText:row,*ALIGNMENT=*Left,*font=Arial9,*ll,"3rd Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxOneLeftText:row,*ALIGNMENT=*Left,*font=Arial9,*ll,"4th Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxOneLeftText:row,*ALIGNMENT=*Left,*font=Arial9,*ll,"Totals";
;Yr5
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
          prtpage MONTHINCREPORT;*pBoxTwoLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"1st Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxTwoLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"2nd Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxTwoLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"3rd Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxTwoLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"4th Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxTwoLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"TOTALS";
;Yr6
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
          prtpage MONTHINCREPORT;*pBoxThreeLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"1st Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxThreeLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"2nd Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxThreeLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"3rd Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxThreeLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"4th Quarter";
          add       SmBoxHeight to row
          prtpage MONTHINCREPORT;*pBoxThreeLeftText:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,"TOTALS";
          move n9 to row


;Quarter Part

          Add       JAN(1),QTRTOT(1,1)
          Add       JAN(2),QTRTOT(2,1)
          Add       JAN(3),QTRTOT(3,1)
          Add       JAN(4),QTRTOT(4,1)
          Add       JAN(5),QTRTOT(5,1)
          Add       JAN(6),QTRTOT(6,1)

          Add       FEB(1),QTRTOT(1,1)
          Add       FEB(2),QTRTOT(2,1)
          Add       FEB(3),QTRTOT(3,1)
          Add       FEB(4),QTRTOT(4,1)
          Add       FEB(5),QTRTOT(5,1)
          Add       FEB(6),QTRTOT(6,1)

          Add       MAR(1),QTRTOT(1,1)
          Add       MAR(2),QTRTOT(2,1)
          Add       MAR(3),QTRTOT(3,1)
          Add       MAR(4),QTRTOT(4,1)
          Add       MAR(5),QTRTOT(5,1)
          Add       MAR(6),QTRTOT(6,1)

          Add       APR(1),QTRTOT(1,2)
          Add       APR(2),QTRTOT(2,2)
          Add       APR(3),QTRTOT(3,2)
          Add       APR(4),QTRTOT(4,2)
          Add       APR(5),QTRTOT(5,2)
          Add       APR(6),QTRTOT(6,2)

          Add       MAY(1),QTRTOT(1,2)
          Add       MAY(2),QTRTOT(2,2)
          Add       MAY(3),QTRTOT(3,2)
          Add       MAY(4),QTRTOT(4,2)
          Add       MAY(5),QTRTOT(5,2)
          Add       MAY(6),QTRTOT(6,2)


          Add       JUN(1),QTRTOT(1,2)
          Add       JUN(2),QTRTOT(2,2)
          Add       JUN(3),QTRTOT(3,2)
          Add       JUN(4),QTRTOT(4,2)
          Add       JUN(5),QTRTOT(5,2)
          Add       JUN(6),QTRTOT(6,2)

          Add       JUL(1),QTRTOT(1,3)
          Add       JUL(2),QTRTOT(2,3)
          Add       JUL(3),QTRTOT(3,3)
          Add       JUL(4),QTRTOT(4,3)
          Add       JUL(5),QTRTOT(5,3)
          Add       JUL(6),QTRTOT(6,3)

          Add       AUG(1),QTRTOT(1,3)
          Add       AUG(2),QTRTOT(2,3)
          Add       AUG(3),QTRTOT(3,3)
          Add       AUG(4),QTRTOT(4,3)
          Add       AUG(5),QTRTOT(5,3)
          Add       AUG(6),QTRTOT(6,3)

          Add       SEP(1),QTRTOT(1,3)
          Add       SEP(2),QTRTOT(2,3)
          Add       SEP(3),QTRTOT(3,3)
          Add       SEP(4),QTRTOT(4,3)
          Add       SEP(5),QTRTOT(5,3)
          Add       SEP(6),QTRTOT(6,3)

          Add       OCT(1),QTRTOT(1,4)
          Add       OCT(2),QTRTOT(2,4)
          Add       OCT(3),QTRTOT(3,4)
          Add       OCT(4),QTRTOT(4,4)
          Add       OCT(5),QTRTOT(5,4)
          Add       OCT(6),QTRTOT(6,4)

          Add       NOV(1),QTRTOT(1,4)
          Add       NOV(2),QTRTOT(2,4)
          Add       NOV(3),QTRTOT(3,4)
          Add       NOV(4),QTRTOT(4,4)
          Add       NOV(5),QTRTOT(5,4)
          Add       NOV(6),QTRTOT(6,4)

          Add       DEC(1),QTRTOT(1,4)
          Add       DEC(2),QTRTOT(2,4)
          Add       DEC(3),QTRTOT(3,4)
          Add       DEC(4),QTRTOT(4,4)
          Add       DEC(5),QTRTOT(5,4)
          Add       DEC(6),QTRTOT(6,4)



;EXCHQTRTOT
          Add       JANEXCH(1),EXCHQTRTOT(1,1)
          Add       JANEXCH(2),EXCHQTRTOT(2,1)
          Add       JANEXCH(3),EXCHQTRTOT(3,1)
          Add       JANEXCH(4),EXCHQTRTOT(4,1)
          Add       JANEXCH(5),EXCHQTRTOT(5,1)
          Add       JANEXCH(6),EXCHQTRTOT(6,1)

          Add       FEBEXCH(1),EXCHQTRTOT(1,1)
          Add       FEBEXCH(2),EXCHQTRTOT(2,1)
          Add       FEBEXCH(3),EXCHQTRTOT(3,1)
          Add       FEBEXCH(4),EXCHQTRTOT(4,1)
          Add       FEBEXCH(5),EXCHQTRTOT(5,1)
          Add       FEBEXCH(6),EXCHQTRTOT(6,1)

          Add       MAREXCH(1),EXCHQTRTOT(1,1)
          Add       MAREXCH(2),EXCHQTRTOT(2,1)
          Add       MAREXCH(3),EXCHQTRTOT(3,1)
          Add       MAREXCH(4),EXCHQTRTOT(4,1)
          Add       MAREXCH(5),EXCHQTRTOT(5,1)
          Add       MAREXCH(6),EXCHQTRTOT(6,1)

          Add       APREXCH(1),EXCHQTRTOT(1,2)
          Add       APREXCH(2),EXCHQTRTOT(2,2)
          Add       APREXCH(3),EXCHQTRTOT(3,2)
          Add       APREXCH(4),EXCHQTRTOT(4,2)
          Add       APREXCH(5),EXCHQTRTOT(5,2)
          Add       APREXCH(6),EXCHQTRTOT(6,2)

          Add       MAYEXCH(1),EXCHQTRTOT(1,2)
          Add       MAYEXCH(2),EXCHQTRTOT(2,2)
          Add       MAYEXCH(3),EXCHQTRTOT(3,2)
          Add       MAYEXCH(4),EXCHQTRTOT(4,2)
          Add       MAYEXCH(5),EXCHQTRTOT(5,2)
          Add       MAYEXCH(6),EXCHQTRTOT(6,2)


          Add       JUNEXCH(1),EXCHQTRTOT(1,2)
          Add       JUNEXCH(2),EXCHQTRTOT(2,2)
          Add       JUNEXCH(3),EXCHQTRTOT(3,2)
          Add       JUNEXCH(4),EXCHQTRTOT(4,2)
          Add       JUNEXCH(5),EXCHQTRTOT(5,2)
          Add       JUNEXCH(6),EXCHQTRTOT(6,2)

          Add       JULEXCH(1),EXCHQTRTOT(1,3)
          Add       JULEXCH(2),EXCHQTRTOT(2,3)
          Add       JULEXCH(3),EXCHQTRTOT(3,3)
          Add       JULEXCH(4),EXCHQTRTOT(4,3)
          Add       JULEXCH(5),EXCHQTRTOT(5,3)
          Add       JULEXCH(6),EXCHQTRTOT(6,3)

          Add       AUGEXCH(1),EXCHQTRTOT(1,3)
          Add       AUGEXCH(2),EXCHQTRTOT(2,3)
          Add       AUGEXCH(3),EXCHQTRTOT(3,3)
          Add       AUGEXCH(4),EXCHQTRTOT(4,3)
          Add       AUGEXCH(5),EXCHQTRTOT(5,3)
          Add       AUGEXCH(6),EXCHQTRTOT(6,3)

          Add       SEPEXCH(1),EXCHQTRTOT(1,3)
          Add       SEPEXCH(2),EXCHQTRTOT(2,3)
          Add       SEPEXCH(3),EXCHQTRTOT(3,3)
          Add       SEPEXCH(4),EXCHQTRTOT(4,3)
          Add       SEPEXCH(5),EXCHQTRTOT(5,3)
          Add       SEPEXCH(6),EXCHQTRTOT(6,3)

          Add       OCTEXCH(1),EXCHQTRTOT(1,4)
          Add       OCTEXCH(2),EXCHQTRTOT(2,4)
          Add       OCTEXCH(3),EXCHQTRTOT(3,4)
          Add       OCTEXCH(4),EXCHQTRTOT(4,4)
          Add       OCTEXCH(5),EXCHQTRTOT(5,4)
          Add       OCTEXCH(6),EXCHQTRTOT(6,4)

          Add       NOVEXCH(1),EXCHQTRTOT(1,4)
          Add       NOVEXCH(2),EXCHQTRTOT(2,4)
          Add       NOVEXCH(3),EXCHQTRTOT(3,4)
          Add       NOVEXCH(4),EXCHQTRTOT(4,4)
          Add       NOVEXCH(5),EXCHQTRTOT(5,4)
          Add       NOVEXCH(6),EXCHQTRTOT(6,4)

          Add       DECEXCH(1),EXCHQTRTOT(1,4)
          Add       DECEXCH(2),EXCHQTRTOT(2,4)
          Add       DECEXCH(3),EXCHQTRTOT(3,4)
          Add       DECEXCH(4),EXCHQTRTOT(4,4)
          Add       DECEXCH(5),EXCHQTRTOT(5,4)
          Add       DECEXCH(6),EXCHQTRTOT(6,4)

;RENTQTRTOT
          Add       JANRENT(1),RENTQTRTOT(1,1)
          Add       JANRENT(2),RENTQTRTOT(2,1)
          Add       JANRENT(3),RENTQTRTOT(3,1)
          Add       JANRENT(4),RENTQTRTOT(4,1)
          Add       JANRENT(5),RENTQTRTOT(5,1)
          Add       JANRENT(6),RENTQTRTOT(6,1)

          Add       FEBRENT(1),RENTQTRTOT(1,1)
          Add       FEBRENT(2),RENTQTRTOT(2,1)
          Add       FEBRENT(3),RENTQTRTOT(3,1)
          Add       FEBRENT(4),RENTQTRTOT(4,1)
          Add       FEBRENT(5),RENTQTRTOT(5,1)
          Add       FEBRENT(6),RENTQTRTOT(6,1)

          Add       MARRENT(1),RENTQTRTOT(1,1)
          Add       MARRENT(2),RENTQTRTOT(2,1)
          Add       MARRENT(3),RENTQTRTOT(3,1)
          Add       MARRENT(4),RENTQTRTOT(4,1)
          Add       MARRENT(5),RENTQTRTOT(5,1)
          Add       MARRENT(6),RENTQTRTOT(6,1)

          Add       APRRENT(1),RENTQTRTOT(1,2)
          Add       APRRENT(2),RENTQTRTOT(2,2)
          Add       APRRENT(3),RENTQTRTOT(3,2)
          Add       APRRENT(4),RENTQTRTOT(4,2)
          Add       APRRENT(5),RENTQTRTOT(5,2)
          Add       APRRENT(6),RENTQTRTOT(6,2)

          Add       MAYRENT(1),RENTQTRTOT(1,2)
          Add       MAYRENT(2),RENTQTRTOT(2,2)
          Add       MAYRENT(3),RENTQTRTOT(3,2)
          Add       MAYRENT(4),RENTQTRTOT(4,2)
          Add       MAYRENT(5),RENTQTRTOT(5,2)
          Add       MAYRENT(6),RENTQTRTOT(6,2)


          Add       JUNRENT(1),RENTQTRTOT(1,2)
          Add       JUNRENT(2),RENTQTRTOT(2,2)
          Add       JUNRENT(3),RENTQTRTOT(3,2)
          Add       JUNRENT(4),RENTQTRTOT(4,2)
          Add       JUNRENT(5),RENTQTRTOT(5,2)
          Add       JUNRENT(6),RENTQTRTOT(6,2)

          Add       JULRENT(1),RENTQTRTOT(1,3)
          Add       JULRENT(2),RENTQTRTOT(2,3)
          Add       JULRENT(3),RENTQTRTOT(3,3)
          Add       JULRENT(4),RENTQTRTOT(4,3)
          Add       JULRENT(5),RENTQTRTOT(5,3)
          Add       JULRENT(6),RENTQTRTOT(6,3)

          Add       AUGRENT(1),RENTQTRTOT(1,3)
          Add       AUGRENT(2),RENTQTRTOT(2,3)
          Add       AUGRENT(3),RENTQTRTOT(3,3)
          Add       AUGRENT(4),RENTQTRTOT(4,3)
          Add       AUGRENT(5),RENTQTRTOT(5,3)
          Add       AUGRENT(6),RENTQTRTOT(6,3)

          Add       SEPRENT(1),RENTQTRTOT(1,3)
          Add       SEPRENT(2),RENTQTRTOT(2,3)
          Add       SEPRENT(3),RENTQTRTOT(3,3)
          Add       SEPRENT(4),RENTQTRTOT(4,3)
          Add       SEPRENT(5),RENTQTRTOT(5,3)
          Add       SEPRENT(6),RENTQTRTOT(6,3)

          Add       OCTRENT(1),RENTQTRTOT(1,4)
          Add       OCTRENT(2),RENTQTRTOT(2,4)
          Add       OCTRENT(3),RENTQTRTOT(3,4)
          Add       OCTRENT(4),RENTQTRTOT(4,4)
          Add       OCTRENT(5),RENTQTRTOT(5,4)
          Add       OCTRENT(6),RENTQTRTOT(6,4)

          Add       NOVRENT(1),RENTQTRTOT(1,4)
          Add       NOVRENT(2),RENTQTRTOT(2,4)
          Add       NOVRENT(3),RENTQTRTOT(3,4)
          Add       NOVRENT(4),RENTQTRTOT(4,4)
          Add       NOVRENT(5),RENTQTRTOT(5,4)
          Add       NOVRENT(6),RENTQTRTOT(6,4)

          Add       DECRENT(1),RENTQTRTOT(1,4)
          Add       DECRENT(2),RENTQTRTOT(2,4)
          Add       DECRENT(3),RENTQTRTOT(3,4)
          Add       DECRENT(4),RENTQTRTOT(4,4)
          Add       DECRENT(5),RENTQTRTOT(5,4)
          Add       DECRENT(6),RENTQTRTOT(6,4)

;AP1

          Add       AP1JAN(1),AP1QTRTOT(1,1)
          Add       AP1JAN(2),AP1QTRTOT(2,1)
          Add       AP1JAN(3),AP1QTRTOT(3,1)
          Add       AP1JAN(4),AP1QTRTOT(4,1)
          Add       AP1JAN(5),AP1QTRTOT(5,1)
          Add       AP1JAN(6),AP1QTRTOT(6,1)

          Add       AP1FEB(1),AP1QTRTOT(1,1)
          Add       AP1FEB(2),AP1QTRTOT(2,1)
          Add       AP1FEB(3),AP1QTRTOT(3,1)
          Add       AP1FEB(4),AP1QTRTOT(4,1)
          Add       AP1FEB(5),AP1QTRTOT(5,1)
          Add       AP1FEB(6),AP1QTRTOT(6,1)

          Add       AP1MAR(1),AP1QTRTOT(1,1)
          Add       AP1MAR(2),AP1QTRTOT(2,1)
          Add       AP1MAR(3),AP1QTRTOT(3,1)
          Add       AP1MAR(4),AP1QTRTOT(4,1)
          Add       AP1MAR(5),AP1QTRTOT(5,1)
          Add       AP1MAR(6),AP1QTRTOT(6,1)

          Add       AP1APR(1),AP1QTRTOT(1,2)
          Add       AP1APR(2),AP1QTRTOT(2,2)
          Add       AP1APR(3),AP1QTRTOT(3,2)
          Add       AP1APR(4),AP1QTRTOT(4,2)
          Add       AP1APR(5),AP1QTRTOT(5,2)
          Add       AP1APR(6),AP1QTRTOT(6,2)

          Add       AP1MAY(1),AP1QTRTOT(1,2)
          Add       AP1MAY(2),AP1QTRTOT(2,2)
          Add       AP1MAY(3),AP1QTRTOT(3,2)
          Add       AP1MAY(4),AP1QTRTOT(4,2)
          Add       AP1MAY(5),AP1QTRTOT(5,2)
          Add       AP1MAY(6),AP1QTRTOT(6,2)


          Add       AP1JUN(1),AP1QTRTOT(1,2)
          Add       AP1JUN(2),AP1QTRTOT(2,2)
          Add       AP1JUN(3),AP1QTRTOT(3,2)
          Add       AP1JUN(4),AP1QTRTOT(4,2)
          Add       AP1JUN(5),AP1QTRTOT(5,2)
          Add       AP1JUN(6),AP1QTRTOT(6,2)

          Add       AP1JUL(1),AP1QTRTOT(1,3)
          Add       AP1JUL(2),AP1QTRTOT(2,3)
          Add       AP1JUL(3),AP1QTRTOT(3,3)
          Add       AP1JUL(4),AP1QTRTOT(4,3)
          Add       AP1JUL(5),AP1QTRTOT(5,3)
          Add       AP1JUL(6),AP1QTRTOT(6,3)

          Add       AP1AUG(1),AP1QTRTOT(1,3)
          Add       AP1AUG(2),AP1QTRTOT(2,3)
          Add       AP1AUG(3),AP1QTRTOT(3,3)
          Add       AP1AUG(4),AP1QTRTOT(4,3)
          Add       AP1AUG(5),AP1QTRTOT(5,3)
          Add       AP1AUG(6),AP1QTRTOT(6,3)

          Add       AP1SEP(1),AP1QTRTOT(1,3)
          Add       AP1SEP(2),AP1QTRTOT(2,3)
          Add       AP1SEP(3),AP1QTRTOT(3,3)
          Add       AP1SEP(4),AP1QTRTOT(4,3)
          Add       AP1SEP(5),AP1QTRTOT(5,3)
          Add       AP1SEP(6),AP1QTRTOT(6,3)

          Add       AP1OCT(1),AP1QTRTOT(1,4)
          Add       AP1OCT(2),AP1QTRTOT(2,4)
          Add       AP1OCT(3),AP1QTRTOT(3,4)
          Add       AP1OCT(4),AP1QTRTOT(4,4)
          Add       AP1OCT(5),AP1QTRTOT(5,4)
          Add       AP1OCT(6),AP1QTRTOT(6,4)

          Add       AP1NOV(1),AP1QTRTOT(1,4)
          Add       AP1NOV(2),AP1QTRTOT(2,4)
          Add       AP1NOV(3),AP1QTRTOT(3,4)
          Add       AP1NOV(4),AP1QTRTOT(4,4)
          Add       AP1NOV(5),AP1QTRTOT(5,4)
          Add       AP1NOV(6),AP1QTRTOT(6,4)

          Add       AP1DEC(1),AP1QTRTOT(1,4)
          Add       AP1DEC(2),AP1QTRTOT(2,4)
          Add       AP1DEC(3),AP1QTRTOT(3,4)
          Add       AP1DEC(4),AP1QTRTOT(4,4)
          Add       AP1DEC(5),AP1QTRTOT(5,4)
          Add       AP1DEC(6),AP1QTRTOT(6,4)



;Variance Part
.         sub       ProjJAN1,AP1JAN1,VarJan1
.         sub       ProjFEB1,AP1FEB1,VarFeb1
.         sub       ProjMAR1,AP1MAR1,VarMAR1
.         sub       ProjAPR1,AP1APR1,VarAPR1
.         sub       ProjMAY1,AP1MAY1,VarMAY1
.         sub       ProjJUN1,AP1JUN1,VarJUN1
.         sub       ProjJUL1,AP1JUL1,VarJUL1
.         sub       ProjAUG1,AP1AUG1,VarAUG1
.         sub       ProjSEP1,AP1SEP1,VarSEP1
.         sub       ProjOCT1,AP1OCT1,VarOCT1
.         sub       ProjNOV1,AP1NOV1,VarNOV1
.         sub       ProjDEC1,AP1DEC1,VarDEC1
.         sub       ProjJAN2,AP1JAN2,VarJAN2
.         sub       ProjFEB2,AP1FEB2,VarFEB2
.         sub       ProjMAR2,AP1MAR2,VarMAR2
.         sub       ProjAPR2,AP1APR2,VarAPR2
.         sub       ProjMAY2,AP1MAY2,VarMAY2
.         sub       ProjJUN2,AP1JUN2,VarJUN2
.         sub       ProjJUL2,AP1JUL2,VarJUL2
.         sub       ProjAUG2,AP1AUG2,VarAUG2
.         sub       ProjSEP2,AP1SEP2,VarSEP2
.         sub       ProjOCT2,AP1OCT2,VarOCT2
.         sub       ProjNOV2,AP1NOV2,VarNOV2
.         sub       ProjDEC2,AP1DEC2,VarDEC2

.         add       VARJAN1,VARTOT1
.         add       VARFEB1,VARTOT1
.         add       VARMAR1,VARTOT1
.         add       VARAPR1,VARTOT1
.         add       VARMAY1,VARTOT1
.         add       VARJUN1,VARTOT1
.         add       VARJUL1,VARTOT1
.         add       VARAUG1,VARTOT1
.         add       VARSEP1,VARTOT1
.         add       VAROCT1,VARTOT1
.         add       VARNOV1,VARTOT1     
.         add       VARDEC1,VARTOT1
.         add       VARJAN2,VARTOT2
.         add       VARFEB2,VARTOT2
.         add       VARMAR2,VARTOT2
.         add       VARAPR2,VARTOT2
.         add       VARMAY2,VARTOT2
.         add       VARJUN2,VARTOT2
.         add       VARJUL2,VARTOT2
.         add       VARAUG2,VARTOT2
.         add       VARSEP2,VARTOT2
.         add       VAROCT2,VARTOT2
.         add       VARNOV2,VARTOT2     
.         add       VARDEC2,VARTOT2
 

Yr1
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
        move mask20 to dim20a
        edit QTRTOT(6,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(6,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(6,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(6,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit ORDTOT(6) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;

;
Yr4
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
        move mask20 to dim20a
        edit QTRTOT(3,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(3,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(3,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(3,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit ORDTOT(3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
;

Yr2
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
        move mask20 to dim20a
        edit QTRTOT(5,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(5,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(5,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(5,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit ORDTOT(5) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
;
Yr5
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
        move mask20 to dim20a
        edit QTRTOT(2,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(2,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(2,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(2,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit ORDTOT(2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
;
Yr3
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
        move mask20 to dim20a
        edit QTRTOT(4,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(4,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(4,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(4,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit ORDTOT(4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;

Yr6
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
        move mask20 to dim20a
        edit QTRTOT(1,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(1,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(1,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit QTRTOT(1,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit ORDTOT(1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;

.         prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit MAY1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxOneVertText(2):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit JUN1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row                                                           
.        move mask20 to dim20a
.        edit JUL1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit AUG1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit SEP1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit OCT1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit NOV1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit DEC1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;

;RentVolumeYear1
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
          add       "20" to Row
        move mask20 to dim20a
        edit RENTQTRTOT(6,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(6,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(6,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(6,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTTOT(6) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;

;
;RentVolumeYear4
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
          add       "20" to Row
        move mask20 to dim20a
        edit RENTQTRTOT(3,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(3,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(3,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(3,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTTOT(3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
;

;RentVolumeYear2
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
          add       "20" to Row
        move mask20 to dim20a
        edit RENTQTRTOT(5,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(5,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(5,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(5,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTTOT(5) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;

;
;RentVolumeYear5
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
          add       "20" to Row
        move mask20 to dim20a
        edit RENTQTRTOT(2,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(2,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(2,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(2,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTTOT(2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
;
;RentVolumeYear3
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
          add       "20" to Row
        move mask20 to dim20a
        edit RENTQTRTOT(4,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(4,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(4,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(4,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTTOT(4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;


;RentVolumeYear6
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
          add       "20" to Row
        move mask20 to dim20a
        edit RENTQTRTOT(1,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(1,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(1,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTQTRTOT(1,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit RENTTOT(1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;

.         move       BegRowLine to row
.         add       SmBoxHeight to row
.         add       OneandahalfSpaced to row
.        move mask20 to dim20a
.        edit JANRENT1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit FEBRENT1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit MARRENT1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit APRRENT1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit MAYRENT1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit JUNRENT1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit JULRENT1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit AUGRENT1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit SEPRENT1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit OCTRENT1 to dim20a        
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit NOVRENT1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move mask20 to dim20a
.        edit DECRENT1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;

;ExchVolumeYear1
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       "20" to row
.         add       SingleSpaced to row
.         add       "40" to Row
        move mask20 to dim20a
        edit EXCHQTRTOT(6,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*bgcolor=*YELLOW,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(6,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(6,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(6,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHTOT(6) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
;
;ExchVolumeYear4
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       "20" to row
.         add       SingleSpaced to row
.         add       "40" to Row
        move mask20 to dim20a
        edit EXCHQTRTOT(3,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*bgcolor=*YELLOW,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(3,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(3,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(3,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHTOT(3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxOneVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
;
;EXCHVolumeYear2
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       "20" to row
        move mask20 to dim20a
        edit EXCHQTRTOT(5,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(5,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(5,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(5,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHTOT(5) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
;
;EXCHVolumeYear5
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       "20" to row
        move mask20 to dim20a
        edit EXCHQTRTOT(2,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(2,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(2,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(2,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHTOT(2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxTwoVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
;
;EXCHVolumeYear3
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       "20" to row
        move mask20 to dim20a
        edit EXCHQTRTOT(4,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(4,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(4,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(4,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHTOT(4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;

;EXCHVolumeYear6
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       "20" to row
        move mask20 to dim20a
        edit EXCHQTRTOT(1,1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(1,2) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(1,3) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHQTRTOT(1,4) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
          add       SmBoxHeight to row
        move mask20 to dim20a
        edit EXCHTOT(1) to dim20a
          call      trim using          dim20a
          prtpage MONTHINCREPORT;*pBoxThreeVertText(3):row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;


.         move       BegRowLine to row
.         add       SmBoxHeight to row
.         add       "40" to row
.        move       mask20 to dim20a
.        edit       JANEXCH1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*bgcolor=*YELLOW,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move       mask20 to dim20a
.        edit       FEBEXCH1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move       mask20 to dim20a
.        edit       MAREXCH1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move       mask20 to dim20a
.        edit       APREXCH1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move       mask20 to dim20a
.        edit       MAYEXCH1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move       mask20 to dim20a
.        edit       JUNEXCH1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move       mask20 to dim20a
.        edit       JULEXCH1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move       mask20 to dim20a
.        edit       AUGEXCH1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move       mask20 to dim20a
.        edit       SEPEXCH1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move       mask20 to dim20a
.        edit       OCTEXCH1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move       mask20 to dim20a
.        edit       NOVEXCH1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
.         add       SmBoxHeight to row
.        move       mask20 to dim20a
.        edit       DECEXCH1 to dim20a
.         call      trim using          dim20a
.         prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim20a;
;
;Proj
.         move       BegRowLine to row
.         add       SmBoxHeight to row
.         add       OneandahalfSpaced to row
.        move mask16 to dim16a
.        edit ProjJAN1 to dim16a
.         call      trim using          dim16a
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*BGCOLOR=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim16a;
.         add       SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjFEB1 to dim16a
.         call      trim using          dim16a
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim16a;
.         add       SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjMAR1 to dim16a
.         call      trim using          dim16a
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim16a;
.         add       SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjAPR1 to dim16a
.         call      trim using          dim16a
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim16a;
.         add       SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjMAY1 to dim16a
.         call      trim using          dim16a
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim16a;
.         add       SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjJUN1 to dim16a
.         call      trim using          dim16a
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim16a;
.         add       SmBoxHeight to row                                                           
.        move mask16 to dim16a
.        edit ProjJUL1 to dim16a
.         call      trim using          dim16a
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim16a;
.         add       SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjAUG1 to dim16a
.         call      trim using          dim16a
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim16a;
.         add       SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjSEP1 to dim16a
.         call      trim using          dim16a
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim16a;
.         add       SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjOCT1 to dim16a
.         call      trim using          dim16a
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim16a;
.         add       SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjNOV1 to dim16a
.         call      trim using          dim16a
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim16a;
.         add       SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjDEC1 to dim16a
.         call      trim using          dim16a
.         prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim16a;
;
;Variance
.         move       BegRowLine to row
.         add       SmBoxHeight to row
.         add       OneandahalfSpaced to row
.        move mask18 to dim18a
.        edit VarJAN1 to dim18a
.         squeeze   dim18a,dim18a       
.         call      trim using          dim18a
.         prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*BGCOLOR=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim18a;
.         add       SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarFEB1 to dim18a
.         squeeze   dim18a,dim18a       
.         prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim18a;
.         add       SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarMAR1 to dim18a
.         squeeze   dim18a,dim18a       
.         prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim18a;
.         add       SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarAPR1 to dim18a
.         squeeze   dim18a,dim18a       
.         prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim18a;
.         add       SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarMAY1 to dim18a
.         squeeze   dim18a,dim18a       
.         prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim18a;
.         add       SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarJUN1 to dim18a
.         squeeze   dim18a,dim18a       
.         prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim18a;
.         add       SmBoxHeight to row                                                           
.        move mask18 to dim18a
.        edit VarJUL1 to dim18a
.         squeeze   dim18a,dim18a       
.         prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim18a;
.         add       SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarAUG1 to dim18a
.         squeeze   dim18a,dim18a       
.         prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim18a;
.         add       SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarSEP1 to dim18a
.         squeeze   dim18a,dim18a       
.         prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim18a;
.         add       SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarOCT1 to dim18a
.         squeeze   dim18a,dim18a       
.         prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim18a;
.         add       SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarNOV1 to dim18a
.         squeeze   dim18a,dim18a       
.         prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim18a;
.         add       SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarDEC1 to dim18a
.         squeeze   dim18a,dim18a       
.         prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim18a;
.
;Actual
Year1
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
.         add       OneandahalfSpaced to row
        move mask19 to dim19a
        edit AP1QTRTOT(6,1) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxOneRightText:row,*bgcolor=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(6,2) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxOneRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(6,3) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxOneRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(6,4) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxOneRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1TOT(6) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxOneRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
;
;Year4
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
.         add       OneandahalfSpaced to row
        move mask19 to dim19a
        edit AP1QTRTOT(3,1) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxOneRightText:row,*bgcolor=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(3,2) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxOneRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(3,3) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxOneRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(3,4) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxOneRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1TOT(3) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxOneRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
;
;Year2
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
.         add       OneandahalfSpaced to row
        move mask19 to dim19a
        edit AP1QTRTOT(5,1) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(5,2) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(5,3) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(5,4) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1TOT(5) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;

;Year5
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
.         add       OneandahalfSpaced to row
        move mask19 to dim19a
        edit AP1QTRTOT(2,1) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(2,2) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(2,3) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(2,4) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1TOT(2) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
;Year3
          move       BegRowLine to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
.         add       OneandahalfSpaced to row
        move mask19 to dim19a
        edit AP1QTRTOT(4,1) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(4,2) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(4,3) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(4,4) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1TOT(4) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
;
;Year6
          move       BegRowLine2 to row
          add       SmBoxHeight to row
          add       SingleSpaced to row
.         add       OneandahalfSpaced to row
        move mask19 to dim19a
        edit AP1QTRTOT(1,1) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(1,2) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(1,3) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1QTRTOT(1,4) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          add       SmBoxHeight to row
        move mask19 to dim19a
        edit AP1TOT(1) to dim19a
          call      trim using          dim19a
          prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,dim19a;
          call lines
.patch1.1 Open Payables
.         if (LTYPE = "C")
..                  Move row to N9
.                   move      TotBoxRow to Row
.                   add       smboxheight to ROW
.                   add       singlespaced to Row
.                 move        mask19 to dim19a
. .                 edit      OPENPAY to dim19a
.                   call      trim using          dim19a
.                   pack Taskname with "Payables = ",dim19a, " as of ",str10
.                   prtpage MONTHINCREPORT;*pBoxOneLeft:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,taskname;
.         else
.                   move      TotBoxRow to Row
.                   add       smboxheight to ROW
.                   add       singlespaced to Row
.                   pack Taskname with "*As of ",str10
.                   prtpage MONTHINCREPORT;*pBoxOneLeft:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,taskname;
.         endif
;;;
          move      todayis to juldays
          call      cvtgreg

          pack      str10 with mm,"/",dd,"/",cc,yy
          if (LTYPE = "C")
                    Move row to N9
                    move      LgBoxHeight2 to Row
                    add       singlespaced to Row
                  move        mask19 to dim19a
          edit      OPENPAY to dim19a
                    call      trim using          dim19a
                    pack Taskname with "Payables = ",dim19a, " as of ",str10
                    prtpage MONTHINCREPORT;*pBoxOneLeft:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,taskname;
.>Patch 4.2 Code Added        
                    if (ManualFlag = YES)
                              call getuser
                              call trim using nuseuser
                              clear taskname
                              pack taskname with "Report requested by ",nuseuser," ","on ",DateRan
                              prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,taskname;
                    Endif
.>Patch 4.2 Code Added                  
          else
                    move      LgBoxHeight2 to Row
                    add       singlespaced to Row
                    pack Taskname with "*As of ",str10
                    prtpage MONTHINCREPORT;*pBoxOneLeft:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,taskname;
.>Patch 4.2 Code Added                  
                    if (ManualFlag = YES)
                              call getuser
                              call trim using nuseuser                
                              pack taskname with "Report requested by ",nuseuser," on ",DateRan               
                              prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,taskname;
                    endif
.>Patch 4.2 Code Added                            
          endif
.patch1.1
;
;;
..HITIT
.         prtclose  MONTHINCREPORT
.         prtplay             PRTFILENAME,"\\NINs2\Laser2"
..        prtpage   MONTHINCREPORT;*NEWPAGE:
..                  *ORIENT=*LANDSCAPE
..        CALL CLEARVARS
..        GOTO READIT
..        
          if (ManualFlag <> YES)
READIT1
                    READ      INCLISTS,SEQ;LVARS
                    GOTO FinalRun IF OVER
                    GOTO READIT1 IF (REP1 <> "Q")
          DISPLAY     *P10:10,*white,"List:  ",LNUM,"   ",OLSTNAME
.patch3.4
.                 prtpage   MONTHINCREPORT;*NEWPAGE:
.                             *ORIENT=*LANDSCAPE
                    CALL CLEARVARS
                    CALL ENDPROGRAM

.Patch3.4
                    GOTO READIT
          ENDIF
ENDPROGRAM
          prtclose  MONTHINCREPORT
.testing
          prtplay             PRTFILENAME,"PDF995"
          pause     c10
.patch3.1
          if (ManualFlag = YES)
.>Patch 4.2 Comment Out - We find out who user is above     
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
.>Patch 4.2 Comment Out - We find out who user is above               
                    squeeze    nuseuser,nuseuser
          else
.Patch 3.9
            call trim using recipient
     move Recipient to nuseuser
.     move "DavidBaca" to nuseuser
.Patch 3.9
.      move "SteveKehrli" to nuseuser
          endif
.patch3.2
EmailReport
   pack str55,PRINTNAME,".pdf"
.Patch 3.8
.         if (manualflag = YES)
.                   move    "Here is your Income PDF File for ",olstname,SmtpSubject Subject
.         else
                    pack   SmtpSubject,"Here is your Income PDF File for ",HoldLstName
.      move    "Here is your Are your Income PDF Files for the Quarter.",SmtpSubject Subject
.Patch 3.8
.         endif
;.   Set the text message that is send with the attachments
          move    str55,SmtpTextMessage(1)   Array <Text message >
          move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
          move    "NTS4",SmtpEmailServer                   Address of email serverc
          clear   smtpemailaddress
          append  nuseuser,SmtpEmailAddress
          append  "@nincal.com",SmtpEmailAddress
          reset   smtpemailaddress
          move    nuseuser,SmtpUserName                                User name
;   Set the destinations of the email. Max 100 (Mime spec)
          move    smtpemailaddress,SmtpDestinations(1,1)
          move    nuseuser,SmtpDestinations(1,2)
          move    "1",SmtpDestIndexLast                          originators UserName
          move    str55,SmtpAttachments(1,1)                     Attached file name
.Patch 3.8 Commented out SK no longer works here custom setup
.         if (manualflag <> YES)
                    move    "c:\work\pdf",SmtpAttachments(1,2)           Path to attached file name
.         else          
.                    if (nuseuser = "STEVEKEHRLI")                    .Steve is running report manually
.Already put on his desktop no need to send to him
.                                       stop
.         move    "c:\docume~1\skehrli\desktop",SmtpAttachments(1,2)           Path to attached file name
.                   else
.               move    "c:\work\pdf",SmtpAttachments(1,2)           Path to attached file name
.                   endif
.         endif
.Patch 3.8
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
                              append  str55,taskname
                              append  " ",taskname
                              append  NTWKPATH5,taskname
                              append  "INCOME\",taskname
                              append  "Qtr",taskname
.Patch 3.8
                              append  "_",taskname
                              append  lstnum,taskname
                              append  "_",taskname
.Patch 3.8
                              append  str55,taskname
                              reset     taskname
                              execute   taskname
.Patch3.4
                              return
.Patch3.4
                    endif
.patch3.3
                    erase str55
          endif
.endpatch3.2
          ;end patch 1.6


.patch3.1
.         prtplay             PRTFILENAME,"\\NINs2\Laser6"
          if (MANUALFLAG = "Y")
                    stop
          endif
InvoicePart
;Invoice Part
                    MOVE      OLRN TO NINVFLD
                    If        (OSTAT = "B" | OSTAT="Q")
                              move      c1 to ninvpath
                              CALL      NINVKEY
                              if Not OVER
.                             match     "397891", lrn
.                             call      debugger if equal
.                             match     "342489", lrn
.                             call      debugger if equal
.                             match     "338819", lrn
.                             call      debugger if equal
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
                                                                      packkey   nshpfld from lrn
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

.patch4.1
               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
               call           NinvAcdTst
               Call           NInvAcdRecLoad
.patch4.1
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
.Patch 3.7.1 Logic Added
                                                  clear n2
                                                  for n2,"2","9"
.                                                 loop
.                                                           add       c1 to n2
.Patch 3.7.1 Logic Added
                                                            move      n2 to str2
                                                            rep       zfill in str2
                                                            CLEAR     NJSTFLD
                                                            PACK      NJSTFLD FROM INVNUM,str2
                                                            rep       zfill in njstfld
                                                            CALL      NJSTKEY
.Patch 3.7.1 Comment Out
.                                                 until over
.Patch 3.7.1 Comment Out
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
;JDpatch
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
;begin dave mucking 12July 05
               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
               call           NinvAcdTst
               Call           NInvAcdRecLoad
;end dave mucking 12July 05
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
.Patch 3.7.1 Logic Added
                                                            clear n2
                                                            for n2,"2","9"
.                                                           loop
.                                                                     add       c1 to n2
.Patch 3.7.1 Logic Added
                                                                      move      n2 to str2
                                                                      rep       zfill in str2
                                                                      CLEAR     NJSTFLD
                                                                      PACK      NJSTFLD FROM INVNUM,str2
                                                                      rep       zfill in njstfld
                                                                      CALL      NJSTKEY
.Patch 3.7.1 Comment Out
.                                                           until over
.Patch 3.7.1 Comment Out
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
..Change to continue loop to see if any manual past first read
                                                                      ENDIF
..End Change..
.Patch 3.7.1 Logic Added
                                                                      clear n2
                                                                      for n2,"2","9"
.
.                                                                     loop
.                                                                               add       c1 to n2
.Patch 3.7.1 Logic Added
                                                                                move      n2 to str2
                                                                                rep       zfill in str2
                                                                                CLEAR     NJSTFLD
                                                                                PACK      NJSTFLD FROM INVNUM,str2
                                                                                rep       zfill in njstfld
                                                                                CALL      NJSTKEY
.Patch 3.7.1 Comment Out
.                                                                     until over
.Patch 3.7.1 Comment Out
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
..Change to continue loop to see if any manual past first read
.                                                                     else
..
                                                            if (num102 = c0)
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
.....Code Added for the exception test of the infamous "14"

.....
                              endif
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

                                                                      call      CVTJUL
;do not allow for neg
                                                            return if (ap1 < 0)
.IncomePart
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
;                                                           If (fiscmonth = 2)
;                                                                     load      MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
;                                                           elseif (fiscmonth = 3) 
;                                                                     load      MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
;                                                           elseif (fiscmonth = 4) 
;                                                                     load      MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
;                                                           elseif (fiscmonth = 5) 
;                                                                     load      MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
;                                                           elseif (fiscmonth = 6) 
;                                                                     load      MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
;                                                           elseif (fiscmonth = 7) 
;                                                                     move mm to n2
;                                                                     load      MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
;                                                           elseif (fiscmonth = 8) 
;                                                                     load      MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
;                                                           elseif (fiscmonth = 9) 
;                                                                     load      MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
;                                                           elseif (fiscmonth = 10) 
;                                                                     load      MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
;                                                           elseif (fiscmonth = 11) 
;                                                                     load      MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
;                                                           elseif (fiscmonth = 12) 
;                                                                     load      MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
;                                                           endif
                                                  EndIf
                                                            move mm to n2
.Patch2.0
                                                  RETURN if (Juldays > TODAYIS)
.Patch2.0
                                                  if ((Juldays >= BegFisc(1)) & (Juldays  <=  EndFisc(1)))
                                                            move c1 to FiscYearFlag
                                                            add  ap1 to ap1tot(1)
                                                  elseif ((Juldays >= BegFisc(2)) & (Juldays  <=  EndFisc(2)))
                                                            move c2 to FiscYearFlag
                                                            add  ap1 to ap1tot(2)
                                                  elseif ((Juldays >= BegFisc(3)) & (Juldays  <=  EndFisc(3)))
                                                            move c3 to FiscYearFlag
                                                            add  ap1 to ap1tot(3)
                                                  elseif ((Juldays >= BegFisc(4)) & (Juldays  <=  EndFisc(4)))
                                                            move c4 to FiscYearFlag
                                                            add  ap1 to ap1tot(4)
                                                  elseif ((Juldays >= BegFisc(5)) & (Juldays  <=  EndFisc(5)))
                                                            move c5 to FiscYearFlag
                                                            add  ap1 to ap1tot(5)
                                                  elseif ((Juldays >= BegFisc(6)) & (Juldays  <=  EndFisc(6)))
                                                            move c6 to FiscYearFlag
                                                            add  ap1 to ap1tot(6)
                                                  else
                                                            move C0 to FiscYearFlag
                                                  endif
;                                                 if (Juldays >= BegFiscCur & Juldays  <=  EndFiscCur)
.Patch2.0
.                                                 RETURN if (Juldays > TODAYIS)
.Patch2.0
                                                            clear dumqty
                                                            clear n9
                                                            clear str15
                                                            if (fiscyearflag = C1)
                                                                      load      num102,n2,AP1JAN(1),AP1FEB(1),AP1MAR(1),AP1APR(1),AP1MAY(1),AP1JUN(1),AP1JUL(1),AP1AUG(1),AP1SEP(1),AP1OCT(1),AP1NOV(1),AP1DEC(1) 
                                                            elseif (fiscyearflag = C2)
                                                                      load      num102,n2,AP1JAN(2),AP1FEB(2),AP1MAR(2),AP1APR(2),AP1MAY(2),AP1JUN(2),AP1JUL(2),AP1AUG(2),AP1SEP(2),AP1OCT(2),AP1NOV(2),AP1DEC(2) 
                                                            elseif (fiscyearflag = C3)
                                                                      load      num102,n2,AP1JAN(3),AP1FEB(3),AP1MAR(3),AP1APR(3),AP1MAY(3),AP1JUN(3),AP1JUL(3),AP1AUG(3),AP1SEP(3),AP1OCT(3),AP1NOV(3),AP1DEC(3) 
                                                            elseif (fiscyearflag = C4)
                                                                      load      num102,n2,AP1JAN(4),AP1FEB(4),AP1MAR(4),AP1APR(4),AP1MAY(4),AP1JUN(4),AP1JUL(4),AP1AUG(4),AP1SEP(4),AP1OCT(4),AP1NOV(4),AP1DEC(4) 
                                                            elseif (fiscyearflag = C5)
                                                                      load      num102,n2,AP1JAN(5),AP1FEB(5),AP1MAR(5),AP1APR(5),AP1MAY(5),AP1JUN(5),AP1JUL(5),AP1AUG(5),AP1SEP(5),AP1OCT(5),AP1NOV(5),AP1DEC(5) 
                                                            elseif (fiscyearflag = C6)
                                                                      load      num102,n2,AP1JAN(6),AP1FEB(6),AP1MAR(6),AP1APR(6),AP1MAY(6),AP1JUN(6),AP1JUL(6),AP1AUG(6),AP1SEP(6),AP1OCT(6),AP1NOV(6),AP1DEC(6) 
                                                            endif


.                                                           load      num102,n2,AP1JAN1,AP1FEB1,AP1MAR1,AP1APR1,AP1MAY1,AP1JUN1,AP1JUL1,AP1AUG1,AP1SEP1,AP1OCT1,AP1NOV1,AP1DEC1     
                                                            add ap1 to num102
.                                                           add       ap1 to AP1TOT
                                                            if (fiscyearflag = C1)
                                                                      store     num102,n2,AP1JAN(1),AP1FEB(1),AP1MAR(1),AP1APR(1),AP1MAY(1),AP1JUN(1),AP1JUL(1),AP1AUG(1),AP1SEP(1),AP1OCT(1),AP1NOV(1),AP1DEC(1) 
                                                            elseif (fiscyearflag = C2)
                                                                      store     num102,n2,AP1JAN(2),AP1FEB(2),AP1MAR(2),AP1APR(2),AP1MAY(2),AP1JUN(2),AP1JUL(2),AP1AUG(2),AP1SEP(2),AP1OCT(2),AP1NOV(2),AP1DEC(2) 
                                                            elseif (fiscyearflag = C3)
                                                                      store     num102,n2,AP1JAN(3),AP1FEB(3),AP1MAR(3),AP1APR(3),AP1MAY(3),AP1JUN(3),AP1JUL(3),AP1AUG(3),AP1SEP(3),AP1OCT(3),AP1NOV(3),AP1DEC(3) 
                                                            elseif (fiscyearflag = C4)
                                                                      store     num102,n2,AP1JAN(4),AP1FEB(4),AP1MAR(4),AP1APR(4),AP1MAY(4),AP1JUN(4),AP1JUL(4),AP1AUG(4),AP1SEP(4),AP1OCT(4),AP1NOV(4),AP1DEC(4) 
                                                            elseif (fiscyearflag = C5)
                                                                      store     num102,n2,AP1JAN(5),AP1FEB(5),AP1MAR(5),AP1APR(5),AP1MAY(5),AP1JUN(5),AP1JUL(5),AP1AUG(5),AP1SEP(5),AP1OCT(5),AP1NOV(5),AP1DEC(5) 
                                                            elseif (fiscyearflag = C6)
                                                                      store     num102,n2,AP1JAN(6),AP1FEB(6),AP1MAR(6),AP1APR(6),AP1MAY(6),AP1JUN(6),AP1JUL(6),AP1AUG(6),AP1SEP(6),AP1OCT(6),AP1NOV(6),AP1DEC(6) 
                                                            endif
.                                                           store     num102,n2,AP1JAN1,AP1FEB1,AP1MAR1,AP1APR1,AP1MAY1,AP1JUN1,AP1JUL1,AP1AUG1,AP1SEP1,AP1OCT1,AP1NOV1,AP1DEC1     
.                                                 elseif ((Juldays >= BegFiscPrev) & (Juldays  <=  EndFiscPrev))
.                                                           move mm to n2
.                                                                     clear dumqty
.                                                                     clear n9
.                                                                     clear str15
.                                                                     load      num102,n2,AP1JAN2,AP1FEB2,AP1MAR2,AP1APR2,AP1MAY2,AP1JUN2,AP1JUL2,AP1AUG2,AP1SEP2,AP1OCT2,AP1NOV2,AP1DEC2     
.                                                                     add ap1 to num102
.                                                                     add       ap1 to AP1TOT2
.                                                                     store     num102,n2,AP1JAN2,AP1FEB2,AP1MAR2,AP1APR2,AP1MAY2,AP1JUN2,AP1JUL2,AP1AUG2,AP1SEP2,AP1OCT2,AP1NOV2,AP1DEC2     
.                                                 endif

;;;
                                        Endif
                              Endif
                    Endif
                    return


Lines
;Horizontal Lines
          move begrowline to row
          move c0 to n2
          loop      
                    if (n2= "4")
                              add       SmBoxHeight to row
                    prtpage MONTHINCREPORT;*pBoxOneLeft:row,*pensize=20,*line=BoxOneRight:row;
                            prtpage MONTHINCREPORT;*pBoxTwoLeft:row,*pensize=20,*line=BoxTwoRight:row;
                              prtpage MONTHINCREPORT;*pBoxThreeLeft:row,*pensize=20,*line=BoxThreeRight:row;
;
                              add       BegRowLineDiff to row
                    prtpage MONTHINCREPORT;*pBoxOneLeft:row,*pensize=20,*line=BoxOneRight:row;
                            prtpage MONTHINCREPORT;*pBoxTwoLeft:row,*pensize=20,*line=BoxTwoRight:row;
                              prtpage MONTHINCREPORT;*pBoxThreeLeft:row,*pensize=20,*line=BoxThreeRight:row;
                              sub       BegRowLineDiff to row
                    elseif (n2= "0")
                              add       SmBoxHeight to row
                    prtpage MONTHINCREPORT;*pBoxOneLeft:row,*pensize=20,*line=BoxOneRight:row;
                            prtpage MONTHINCREPORT;*pBoxTwoLeft:row,*pensize=20,*line=BoxTwoRight:row;
                              prtpage MONTHINCREPORT;*pBoxThreeLeft:row,*pensize=20,*line=BoxThreeRight:row;
;
                              add       BegRowLineDiff to row
                    prtpage MONTHINCREPORT;*pBoxOneLeft:row,*pensize=20,*line=BoxOneRight:row;
                            prtpage MONTHINCREPORT;*pBoxTwoLeft:row,*pensize=20,*line=BoxTwoRight:row;
                              prtpage MONTHINCREPORT;*pBoxThreeLeft:row,*pensize=20,*line=BoxThreeRight:row;
                              sub       BegRowLineDiff to row
;
                    else
                              add       SmBoxHeight to row
                    prtpage MONTHINCREPORT;*pBoxOneLeft:row,*pensize=10,*line=BoxOneRight:row;
                            prtpage MONTHINCREPORT;*pBoxTwoLeft:row,*pensize=10,*line=BoxTwoRight:row;
                              prtpage MONTHINCREPORT;*pBoxThreeLeft:row,*pensize=10,*line=BoxThreeRight:row;
;
                              add       BegRowLineDiff to row
                    prtpage MONTHINCREPORT;*pBoxOneLeft:row,*pensize=10,*line=BoxOneRight:row;
                            prtpage MONTHINCREPORT;*pBoxTwoLeft:row,*pensize=10,*line=BoxTwoRight:row;
                              prtpage MONTHINCREPORT;*pBoxThreeLeft:row,*pensize=10,*line=BoxThreeRight:row;
                              sub       BegRowLineDiff to row
;
                    endif
.                 prtpage MONTHINCREPORT;*pBoxFourLeft:row,*pensize=10,*line=BoxFourRight:row;
.                 prtpage MONTHINCREPORT;*pBoxFiveLeft:row,*pensize=10,*line=BoxFiveRight:row;
.         prtpage MONTHINCREPORT;*pBoxSixLeft:row,*pensize=10,*line=BoxSixRight:row;
                    add       c1 to n2
;                   add       HalfsmboxHeight,row
;                 prtpage MONTHINCREPORT;*pBoxTwoLeft1:row,*pensize=10,*line=BoxTwoRight1:row;
;                 prtpage MONTHINCREPORT;*pBoxFiveLeft1:row,*pensize=10,*overlayon,*line=BoxFiveRight1:row;
;                   sub       HalfsmboxHeight,row

          until     (n2 = "5")
          repeat
;Vertical Lines
                    prtpage MONTHINCREPORT;*pBoxOneVert(1):BegRowLine,*pensize=10,*line=BoxOneVert(1):LgBoxHeight;
          prtpage MONTHINCREPORT;*pBoxOneVert(2):BegRowLine,*pensize=10,*line=BoxOneVert(2):LgBoxHeight;
          prtpage MONTHINCREPORT;*pBoxOneVert(3):BegRowLine,*pensize=10,*line=BoxOneVert(3):LgBoxHeight;

          prtpage MONTHINCREPORT;*pBoxTwoVert(1):BegRowLine,*pensize=10,*line=BoxTwoVert(1):LgBoxHeight;
          prtpage MONTHINCREPORT;*pBoxTwoVert(2):BegRowLine,*pensize=10,*line=BoxTwoVert(2):LgBoxHeight;
          prtpage MONTHINCREPORT;*pBoxTwoVert(3):BegRowLine,*pensize=10,*line=BoxTwoVert(3):LgBoxHeight;

          prtpage MONTHINCREPORT;*pBoxThreeVert(1):BegRowLine,*pensize=10,*line=BoxThreeVert(1):LgBoxHeight;
          prtpage MONTHINCREPORT;*pBoxThreeVert(2):BegRowLine,*pensize=10,*line=BoxThreeVert(2):LgBoxHeight;
          prtpage MONTHINCREPORT;*pBoxThreeVert(3):BegRowLine,*pensize=10,*line=BoxThreeVert(3):LgBoxHeight;

;Vertical Lines2
                    prtpage MONTHINCREPORT;*pBoxOneVert(1):BegRowLine2,*pensize=10,*line=BoxOneVert(1):LgBoxHeight2;
          prtpage MONTHINCREPORT;*pBoxOneVert(2):BegRowLine2,*pensize=10,*line=BoxOneVert(2):LgBoxHeight2;
          prtpage MONTHINCREPORT;*pBoxOneVert(3):BegRowLine2,*pensize=10,*line=BoxOneVert(3):LgBoxHeight2;

          prtpage MONTHINCREPORT;*pBoxTwoVert(1):BegRowLine2,*pensize=10,*line=BoxTwoVert(1):LgBoxHeight2;
          prtpage MONTHINCREPORT;*pBoxTwoVert(2):BegRowLine2,*pensize=10,*line=BoxTwoVert(2):LgBoxHeight2;
          prtpage MONTHINCREPORT;*pBoxTwoVert(3):BegRowLine2,*pensize=10,*line=BoxTwoVert(3):LgBoxHeight2;

          prtpage MONTHINCREPORT;*pBoxThreeVert(1):BegRowLine2,*pensize=10,*line=BoxThreeVert(1):LgBoxHeight2;
          prtpage MONTHINCREPORT;*pBoxThreeVert(2):BegRowLine2,*pensize=10,*line=BoxThreeVert(2):LgBoxHeight2;
          prtpage MONTHINCREPORT;*pBoxThreeVert(3):BegRowLine2,*pensize=10,*line=BoxThreeVert(3):LgBoxHeight2;
          return

CLEARVARS
          CLEAR     JAN
          CLEAR     FEB
          CLEAR     MAR
          CLEAR     APR
          CLEAR     MAY
          CLEAR     JUN
          CLEAR     JUL
          CLEAR     AUG
          CLEAR     SEP
          CLEAR     OCT
          CLEAR     NOV
          CLEAR     DEC
          CLEAR     ORDTOT
          CLEAR     QTRTOT
          CLEAR     JANRENT
          CLEAR     FEBRENT
          CLEAR     MARRENT
          CLEAR     APRRENT
          CLEAR     MAYRENT
          CLEAR     JUNRENT
          CLEAR     JULRENT
          CLEAR     AUGRENT
          CLEAR     SEPRENT
          CLEAR     OCTRENT
          CLEAR     NOVRENT
          CLEAR     DECRENT
          CLEAR     RENTQTRTOT
          CLEAR     RENTTOT
          CLEAR     JANEXCH
          CLEAR     FEBEXCH
          CLEAR     MAREXCH
          CLEAR     APREXCH
          CLEAR     MAYEXCH
          CLEAR     JUNEXCH
          CLEAR     JULEXCH
          CLEAR     AUGEXCH
          CLEAR     SEPEXCH
          CLEAR     OCTEXCH
          CLEAR     NOVEXCH
          CLEAR     DECEXCH
          CLEAR     EXCHTOT
          CLEAR     EXCHQTRTOT
          CLEAR     AP1JAN
          CLEAR     AP1FEB
          CLEAR     AP1MAR
          CLEAR     AP1APR
          CLEAR     AP1MAY
          CLEAR     AP1JUN
          CLEAR     AP1JUL
          CLEAR     AP1AUG
          CLEAR     AP1SEP
          CLEAR     AP1OCT
          CLEAR     AP1NOV
          CLEAR     AP1DEC
          CLEAR     AP1TOT
          CLEAR     AP1QTRTOT
          CLEAR     BegFisc
          CLEAR     BegQTRFisc
          CLEAR     FISCYEARFLAG
          CLEAR     FISCQTRFLAG
          clear     begrowlinediff
          CLEAR     OPENPAY
          RETURN
               
EmailTrouble
                    move    "Houston We May have a problem",SmtpSubject Subject
;.   Set the text message that is send with the attachments
                    move    Taskname,SmtpTextMessage(1)   Array <Text message >
                    move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
                    move    "NTS4",SmtpEmailServer                   Address of email serverc
                    clear   smtpemailaddress
                    append  "InformationServices",SmtpEmailAddress
                    append  "@nincal.com",SmtpEmailAddress
                    reset   smtpemailaddress
                    move    "InformationServices",SmtpUserName                                User name
;   Set the destinations of the email. Max 100 (Mime spec)
                    move    smtpemailaddress,SmtpDestinations(1,1)
                    move    "InformationServices",SmtpDestinations(1,2)
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

.patch3.1
debugger
         return
userng
          clear     taskname
          append    "I'm sorry I've lost track of who you are,",taskname
          append    NewLine,taskname
          append    "Please leave the program and try again!",taskname
          reset     taskname
          alert     caution,taskname,result
          shutdown
   stop
.patch3.1
.>Patch 4.2
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
.>Patch 4.2
.patch3.2
RetryPrint
   trap ExitThisProgram if SPOOL
          PRTOPEN MONTHINCREPORT,"",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
          trapclr   SPOOL
          return

ExitThisProgram
          stop
.Patch3.2
.patch3.4
OpenPrintFile
          bump timestamp,8
          pack      PRINTNAME,LNUM,timestamp
.testing
          pack      prtfilename with "c:\work\PDF\",printname,".lst"
          PRTOPEN MONTHINCREPORT,"PDF995",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
          PRTPAGE MONTHINCREPORT;*UNITS=*HIENGLISH:
                          *ORIENT=*LANDSCAPE;
          return
FINALRUN
          call      ENDPROGRAM
          stop
.Patch3.4
IncFooter
          INCLUDE   NORDIO.INC
          INCLUDE   NDATIO.INC
;compute
          INCLUDE   NSHPIO.INC
          INCLUDE   NMRGIO.INC
          INCLUDE   NDAT3IO.INC
         include   nacdIO.inc
;compute
          INCLUDE   NADJIO.INC
;begin patch 4.0
;         INCLUDE   NINVIO.INC
;         INCLUDE   COMPUTE.INC
          INCLUDE   compute.inc
          INCLUDE   ninvio.inc
               INclude        NInvAcdio.inc
;end patch 4.0
          INCLUDE   NJSTIO.INC
.patch3.1
          include   NUSEIO.INC
.patch3.1
          INCLUDE   COMLOGIC.INC