............................................................................
.
. PROGRAM    : Nadj0006
. DATE       : 03/20/95
. AUTHOR     : D.L. Herrick
. DESCRIPTION: PRODUCES NIN Daily ADJUSTMENT REGISTER.
.
............................................................................
.
PC       EQU       0
         INCLUDE   COMMON.inc
         include   hp.inc
.
release  init      "1.81"        DLH printer issue
Reldate   Init      "april 2011"
.release  init      "1.8"        25Oct07 DLH PLI
.see previous release for old changes
.release  init      "1.7"        2007Jun05 DLH oslspern
.release  init      "1.6"        31MAY05 DMB  NJSTDD CONV
.release  init      "1.5"        03AUG04 ASH LOGO CONVERSION
.release  init      "1.4"        27MAY02 ASH MAILER CONVERSION
.release  init      "1.3"        13Oct99 DLH add code to handle nin income
.release  init      "1.2"        14Sep98 ASH Mailer Y2K File expansion
.release  init      "1.1"        11decc95 DLH skip reprints
.RELEASE  INIT      "1.0"  
.
DATE     DIM       8
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
PAGE     FORM      5
PRTLINES FORM      2
TAR      FORM      8.2
TAP1     FORM      8.2
TAP2     FORM      8.2
TAP      FORM      8.2        TOTAL AP1+AP2
TNININC  FORM      8.2
.Begin patch 1.8
TAP3     FORM      8.2
TxNININC  FORM      8.2
PLTAP3     FORM      8.2
PLTxNININC  FORM      8.2
.end patch 1.8
TLRINC   FORM      8.2
TLRINC1  FORM      8.2        BROKERAGE EXCHANGE
TLRINC2  FORM      8.2        LIST MANAGEMENT EXCHANGE
TLRINC3  FORM      8.2        BROKERAGE RENTAL
TLRINC4  FORM      8.2        LIST MANAGEMENT RENTAL & TOTALS PRINT
TSTAX    FORM      7.2
TCTAX    FORM      7.2
TPOST    FORM      7.2
R11      FORM      7.2
R12      FORM      7.2
R14      FORM      7.2
R16      FORM      7.2
R20      FORM      7.2
R21      FORM      7.2
R22      FORM      7.2
R23      FORM      7.2
R24      FORM      7.2
R25AR    FORM      7.2
R25AP    FORM      7.2
R25LR    FORM      7.2
R26      FORM      7.2
R27      form      7.2
R29      form      7.2
R30      FORM      7.2
R31      FORM      7.2
R35AR         form          9.2
R35AP         form          9.2
R35LR         form          9.2
r36AR         form          9.2
r36AP         form          9.2
r36LR         form          9.2
r37AR         form          9.2
r37AP         form          9.2
r37LR         form          9.2
plTAR      FORM      8.2
PLTAP1     FORM      8.2
PLTAP2     FORM      8.2
PLTAP      FORM      8.2        TOTAL AP1+AP2
PLTNININC  FORM      8.2
PLTLRINC   FORM      8.2
PLTLRINC1  FORM      8.2        BROKERAGE EXCHANGE
PLTLRINC2  FORM      8.2        LIST MANAGEMENT EXCHANGE
PLTLRINC3  FORM      8.2        BROKERAGE RENTAL
PLTLRINC4  FORM      8.2        LIST MANAGEMENT RENTAL & TOTALS PRINT
PLTSTAX    FORM      7.2
PLTCTAX    FORM      7.2
PLTPOST    FORM      7.2
PLR11      FORM      7.2
PLR12      FORM      7.2
PLR14      FORM      7.2
PLR16      FORM      7.2
PLR20      FORM      7.2
PLR21      FORM      7.2
PLR22      FORM      7.2
PLR23      FORM      7.2
PLR24      FORM      7.2
PLR25AR    FORM      7.2
PLR25AP    FORM      7.2
PLR25LR    FORM      7.2
PLR26      FORM      7.2
PLR27      form      7.2
PLR29      form      7.2
PLR30      FORM      7.2
PLR31      FORM      7.2
PLR35AR         form          9.2
PLR35AP         form          9.2
PLR35LR         form          9.2
PLr36AR         form          9.2
PLr36AP         form          9.2
PLr36LR         form          9.2
PLr37AR         form          9.2
PLr37AP         form          9.2
PLr37LR         form          9.2
PRFILE        pfile
.Column Defs
Header1   form    9
Title1   form    9
Title2   form    9
Title3   form    9
.Column8  form    9
.Column9  form    9
.Column10 form    9
.Column11 form    9
Column4R  form    9
Column5R  form    9
Column6R form    9
Column7R form    9
Column8R form    9
Column9R form    9
Column10R form    9
font8    font
        create  font8,"Times New Roman",size=8
font8i    font
        create  font8i,"Times New Roman",size=8,italic        
.end patch 1.8
TINVQTY  FORM      9
DATEMASK INIT      "XX/XX/XX"
DATEPRT1 DIM       8
DATEPRT2 DIM       8
M1       INIT      " JANUARY  "
M2       INIT      "FEBRUARY  "
M3       INIT      "  MARCH   "
M4       INIT      "  APRIL   "
M5       INIT      "   MAY    "
M6       INIT      "  JUNE    "
M7       INIT      "  JULY    "
M8       INIT      " AUGUST   "
M9       INIT      "SEPTEMBER "
M10      INIT      " OCTOBER  "
M11      INIT      "NOVEMBER  "
M12      INIT      "DECEMBER  "
.M1       INIT      " JANUARY  "
.M2       INIT      "FEBRUARY  "
.M3       INIT      "  MARCH   "
.M4       INIT      "  APRIL   "
.M5       INIT      "   MAY    "
.M6       INIT      "  JUNE    "
.M7       INIT      "  JULY    "
.M8       INIT      " AUGUST   "
.M9       INIT      "SEPTEMBER "
.M10      INIT      " OCTOBER  "
.M11      INIT      "NOVEMBER  "
.M12      INIT      "DECEMBER  "
MONTH    DIM       18
PREFLAG  DIM       1
NAME1    DIM       45
NAME2    DIM       45
.
AR       FORM      9.2       A/R
AP1      FORM      7.2       A/P1
AP2      FORM      7.2       A/P2
.begin patch 1.8
AP3      FORM      7.2       A/P3            = *lr
.end patch 1.8
AP       FORM      7.2       A/P1 + A/P2
LRINC    FORM      7.2       LR INCOME
NINC     FORM      7.2       NIN INCOME
.begin patch 1.8
XNINC     FORM      7.2       NIN INCOME
.end patch 1.8
GROSS    FORM      7.2       GROSS BILLING
CTAX     FORM      7.2       CITY TAX
STAX     FORM      7.2       STATE TAX
TAXES    FORM      7.2       TOTAL TAXES
ARWOPP   FORM      7.2       A/R WITHOUT PRE-PAYMENT
SHIP     FORM      7.2       TOTAL SHIPPING
SELECT   FORM      7.2       TOTAL SELECTIONS
POST     FORM      7.2
............................................................................
+
         INCLUDE   CONS.inc
+
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
+
         INCLUDE   NBILDD.inc    BILL-TO
+
         INCLUDE   NORDDD.inc
+
         INCLUDE   NJSTDD.inc    ADJUSTMENT DETAIL
+
         INCLUDE   NJSTCLDD.inc  ADJUSTMENT APPLY
.
+...........................................................................
         MOVE      "Daily Adjustment Register" TO STITLE
         MATCH     "Nadj0006" TO PROGRAM        *CHAINED FROM DSINIT?
         IF        NOT EQUAL                    *NO
         MOVE      "Nadj0006 " TO PROGRAM
         MOVE      "ninpadj2" TO INPNAME
         MOVE      "NADJ2REG" TO PRTNAME
         MOVE      "Names in the News" TO COMPNME
         ENDIF
         CALL      PAINT
.begin patch 1.8         
              move    "4250" to Header1
        move    "250",column
        move    "675",column1
        move    "3100",column2
        move    "3600",column3
        move    "4700",column4R
        move    "5400",column5R
        move    "6100",column6R
        move    "6600",column7R
        move    "7100",column8R
        move    "7700",column9R
        move    "7900",column10R        

.
.end patch 1.8         
         MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
DATEOK   call      GETDATE
.         KEYIN     *P10:10,"DATE OK? ",*T05,STR1
         CMATCH    NO TO STR1
         IF        EQUAL
         KEYIN     *P10:12,*EL,"MM/DD/YY":
                   *P10:12,*+,*ZF,*JR,MM,*DV,SLASH,*ZF,*JR,DD:
                   *DV,SLASH,*ZF,*JR,YY,*-
         PACK      DATE FROM MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         DISPLAY   *P10:12,TODAY,*EL
         GOTO      DATEOK
         ENDIF
         DISPLAY   *P10:10,*EL,*P10:12,*EL
         GOTO      BEGIN
GETDATE  CLOCK     DATE TO DATE
         IFNZ      PC
         MOVE      "ZZ/ZZ/ZZ" TO TODAY
         MOVE      DATE TO N6
         EDIT      N6 TO TODAY
         MOVE      C0 TO N6
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         XIF
         RETURN
.
BEGIN    UNPACK    TODAY TO MM,STR1,DD,STR1,YY
         MOVE      MM TO N2
         clear     Month
         LOAD      MONTH USING N2 FROM M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12
.         SCAN      "??" IN MONTH
.         GOTO      PRTGET IF NOT EQUAL
.         BUMP      MONTH BY -1
.         LENSET    MONTH
         EndSET    MONTH
         append    dd to month
         append    " " to month
          Append    "20",MOnth
         APPEND    YY TO MONTH
         RESET     MONTH
         SETLPTR   MONTH
.
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : ":
                   *P01:09,"Adjust Date : "
.
INPGET   TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         GOTO      PRTGET
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET           
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL TO PRTNAME
         GOTO      START IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH PDRIVE,PRTNAME
.begin patch 1.8
.         SPLOPEN   PRTFILE
          call      GetWinVer
          if (osflag >= c6)
                    PRTOPEN   prfile,"\\nins2\Laser2",PRTFILE
          else   .(osflag = c0)         .Don't know prompt for printer
                    PRTOPEN   prfile,"","A"
          endif



.               PRTOPEN prfile,"\\NINS2\laser2",PRTFILE

.               PRTOPEN prfile,"\\NTS0\Laser8","NADJ2REG.LST",Spoolfile="NADJ2REG.LST"
.               PRTOPEN prfile,"-",PRTName
               PRTPAGE prfile;*UNITS=*HIENGLISH:
                         *ORIENT=*PORTRAIT:
                          *Duplex=2;                                                 

.begin patch 1.8
         DISPLAY   *P15:07,PRTNAME
         GOTO      START
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
START    
.>Patch 1.6
          MOVE      INPNAME TO NJSTNAMESEQ
.MOVE      INPNAME TO NJSTNAME
.>Patch 1.6

         CALL      HEADING
.
RDADJ    CALL      NJSTSEQ
         GOTO      EOJ IF OVER
         ADD       C1 TO N5
         DISPLAY   *P15:08,N5:
                   *P15:09,JSTDATE
.
         cmatch    "R" to jststat      .11dec95 dlh skip reprints
         goto      rdadj if equal         
         PACK      STR8 WITH JSTMLR,JSTCNT,JSTBILTO
         MATCH     STR8 TO NBILFLD
         GOTO      DTLPRT IF EQUAL
         MOVE      STR8 TO NBILFLD
         CALL      NBILKEY
         GOTO      GETMLR IF OVER
         MOVE      BILNAME TO NAME1
         MOVE      BILCOMP TO NAME2
         GOTO      DTLPRT
GETMLR   PACK      MKEY FROM NBILFLD
         CALL      NMLRKEY
         MOVE      MNAME TO NAME1
         MOVE      MCOMP TO NAME2
.
DTLPRT   MOVE      DATEMASK TO DATEPRT1
         unpack    jstdate into str2,yy,mm,dd
         pack      str6 from mm,dd,yy
         EDIT      str6  TO DATEPRT1
         MOVE      DATEMASK TO DATEPRT2
         EDIT      JSTINVDT TO DATEPRT2
         REPLACE   "0 " IN JSTISTAT
.
         CALL      NJSTCALC
.
         MOVE      C1 TO NORDPATH
         PACK      NORDFLD WITH JSTLR
         CALL      NORDKEY
         DISPLAY   *P01:19,OLRN,B1,O1DES
.
CHK11    MATCH     "11" TO JSTREASN              .AP
         GOTO      CHK12 IF NOT EQUAL
.begin patch 1.8
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId2 = "P")                        
          ADD       AP TO PLR11
          Else
          ADD       AP TO R11
          endif
.         ADD       Ap TO R11
.end patch 1.8
         GOTO      DTLPRT1
.
CHK12    MATCH     "12" TO JSTREASN               .LR
         GOTO      CHK14 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId2 = "P")                        
          ADD       LRinc TO PLR12
          Else
          ADD       LRinc TO R12
          endif

.         ADD       lrinc TO R12
         GOTO      DTLPRT1
.
chk14    MATCH     "14" TO JSTREASN               .AP
         GOTO      CHK16 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId2 = "P")                        
          ADD       AP TO PLR14
          Else
          ADD       AP TO R14
          endif
.         ADD       AP TO R14
         GOTO      DTLPRT1
.
CHK16    MATCH     "16" TO JSTREASN               .AR
         GOTO      CHK20 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId = "P")                         
          ADD       AR TO PLR16
          Else
          ADD       AR TO R16
          endif
.         ADD       AR TO R16
         GOTO      DTLPRT1
.
CHK20    MATCH     "20" TO JSTREASN               .Postage
         GOTO      CHK21 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
          if        (OcompId = "P" or Ocompid2 = "P")                           
          ADD       Post TO PLR20
          Else
          ADD       Post TO R20
          endif
.         ADD       POST TO R20
         GOTO      DTLPRT1
.
CHK21    MATCH     "21" TO JSTREASN               .AP
         GOTO      CHK22 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId2 = "P")                        
          ADD       AP TO PLR21
          Else
          ADD       AP TO R21
          endif
.         ADD       AP TO R21
         GOTO      DTLPRT1
.
CHK22    MATCH     "22" TO JSTREASN
         GOTO      CHK23 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId = "P")                         
          ADD       AR TO PLR22
          Else
          ADD       AR TO R22
          endif
.         ADD       AR TO R22
         GOTO      DTLPRT1
.
CHK23    MATCH     "23" TO JSTREASN
         GOTO      CHK24 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId2 = "P")                        
          ADD       AP TO PLR23
          Else
          ADD       AP TO R23
          endif
.         ADD       AP TO R23
         GOTO      DTLPRT1
.
CHK24    MATCH     "24" TO JSTREASN
         GOTO      CHK25 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId2 = "P")                        
          ADD       AP TO PLR24
          Else
          ADD       AP TO R24
          endif
.         ADD       AP TO R24
         GOTO      DTLPRT1
.
CHK25    MATCH     "25" TO JSTREASN
         GOTO      CHK26 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId = "P")                         
          ADD       LRINC TO PLR25LR
          ADD       AP TO PLR25AP
          ADD       AR TO PLR25AR
          Else
          ADD       LRINC TO R25LR
          ADD       AP TO R25AP
          ADD       AR TO R25AR
          endif

.         ADD       LRINC TO R25LR
.         ADD       AP TO R25AP
.         ADD       AR TO R25AR
         GOTO      DTLPRT1
.
CHK26    MATCH     "26" TO JSTREASN
         GOTO      CHK27 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId = "P")                         
          ADD       LRINC TO PLR26
          else
          ADD       LRINC TO R26
          endif
         ADD       LRINC TO R26
         GOTO      DTLPRT1
.
CHK27    MATCH     "27" TO JSTREASN
         GOTO      CHK29 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId = "P")                         
          ADD       Ar TO PLR27
          Else
          ADD       Ar TO R27
          endif
.                   ADD       Ar TO R27
         GOTO      DTLPRT1
.
CHK29    MATCH     "28" TO JSTREASN
         GOTO      CHK30 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId2 = "P")                        
          ADD       Ap TO PLR29
          Else
          ADD       Ap TO R29
.                   ADD       Ap TO R29
          endif
         GOTO      DTLPRT1
.
CHK30    MATCH     "30" TO JSTREASN
         GOTO      CHK31 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId = "P")                         
          ADD       LRINC TO PLR30
          Else
          ADD       LRINC TO R30
          endif
.                   ADD       LRINC TO R30
         GOTO      DTLPRT1
.
CHK31    MATCH     "31" TO JSTREASN
         GOTO      DTLPRT1 IF NOT EQUAL
.Ocompid effects LR/AR   id2 effects ap
.         if        (OcompId = "P" or Ocompid2 = "P")                           
          if        (OcompId = "P")                         
          ADD       LRINC TO PLR31
          Else
          ADD       LRINC TO R31
          endif
.                   ADD       LRINC TO R31
CHK35    MATCH     "35" TO JSTREASN
         GOTO      CHK36 IF NOT EQUAL
.begin patch 3.82
          if        (OcompId = "P")                         
         ADD       LRINC TO PLR35LR
         ADD       AP TO PLR35AP
         ADD       AR TO PLR35AR

          else
         ADD       LRINC TO R35LR
         ADD       AP TO R35AP
         ADD       AR TO R35AR
          endif
.end patch 3.82
         
         GOTO      DTLPRT1

.
CHK36    MATCH     "36" TO JSTREASN
         GOTO      CHK37 IF NOT EQUAL
          if        (OcompId = "P")                         
         ADD       LRINC TO PLR36LR
         ADD       AP TO PLR36AP
         ADD       AR TO PLR36AR
          else
         ADD       LRINC TO R36LR
         ADD       AP TO R36AP
         ADD       AR TO R36AR
          endif
         GOTO      DTLPRT1
CHK37    MATCH     "37" TO JSTREASN
         GOTO      DTLPRT1 IF NOT EQUAL
          if        (Ocompid2 = "P")                        
         ADD       LRINC TO PLR37LR
         ADD       AP TO PLR37AP
         ADD       AR TO R37AR
          Else
         ADD       LRINC TO R37LR
         ADD       AP TO R37AP
         ADD       AR TO R37AR
          endif
.end patch 1.8
DTLPRT1  RESET     EXCODES
         PACK      STR2 FROM OSALES10,OSALES
         MOVE      C0 TO N2
         MOVE      STR2 TO N2
         COMPARE   C0 TO N2
         IF        EQUAL
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         IF        EQUAL
         MOVE      C1 TO N2
         MOVE      C2 TO OELCODE
         ELSE     
         MOVE      C6 TO N2
         GOTO      EXQUES
         ENDIF
         ENDIF
EXQUES   RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         IF        EQUAL                   *EXCHANGE
         BRANCH    N2 OF BRK,LstM,BRK,BRK,BRK,LSTM,BRK,BRK,BRK,BRK:
                  BRK,BRK,BRK,BRK,BRK,BRK,BRK,BRK,LSTM,BRK:
                   BRK,BRK,BRK,BRK,BRK,BRK,BRK,BRK,BRK,BRK:
                   BRK,BRK,BRK,BRK,BRK
LSTM     ADD       LRINC TO TLRINC2
         GOTO      DTLPRT2
BRK      ADD       LRINC TO TLRINC1
                ELSE                       *RENTAL
         BRANCH    N2 OF BRK1,Lstm1,BRK1,BRK1,BRK1,LSTM1,BRK1,BRK1,BRK1,BRK1:
                   BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,LSTM1,BRK1:
                   BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1,BRK1:
                   BRK1,BRK1,BRK1,BRK1,BRK1
LSTM1    ADD       LRINC TO TLRINC4
         GOTO      DTLPRT2
BRK1     ADD       LRINC TO TLRINC3
         ENDIF
.
DTLPRT2
          COMPARE  "9900" to ROW
          CALL      HEADING IF NOT LESS
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,JSTMLR,SLASH,JSTCNT;               
              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ll,NAME1;                    
              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*ll,DATEPRT1;                 
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,JSTINVNO,DASH,JSTSUBNO;                                 
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ll,AR;                   
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,AP1;                  
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,LRINC;                  
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,CTAX;                   
              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ll,STAX;            
              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ll,POST;    
              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Left,*ll,JSTREASN,JSTISTAT;
        add     eightlpi,row   
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,JSTLR;              
              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ll,NAME2;                                   
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,AP2;         
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,NINC;                                  
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,"< NIN";                   
              prtpage prfile;*pColumn10R:row,*ALIGNMENT=*Left,*ll,JSTCD;
        add     eightlpi,row                 
              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ll,O1DES;           
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,"LR >";                   
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,AP3;         
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ll,xNINC;                                  
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ll,"< NIN";                   
        add     eightlpi,row  
        add     eightlpi,row          
.begin patch 3.82
          if        (OcompId <> "P" and Ocompid2 <> "P")              .all NIN  
          ADD                 LRINC  TO TLRINC
          ADD                 NINC   TO TNININC
          Elseif      (OcompId = "P" and Ocompid2 = "P")                        .all PLI
          ADD                 LRINC  TO PLTLRINC
          ADD                 NINC   TO PLTNININC
          Elseif    (OcompId = "P")
          ADD                 LRINC  TO PLTLRINC
          ADD                 NINC   TO PLTNININC
          else
          ADD                 NINC   TO TNININC
          ADD                 LRINC  TO TLRINC                    .for now dump to nin
          endif
.end patch 3.82

..        COMPARE   C60 TO PRTLINES
.         CALL      HEADING IF NOT LESS
.         PRINT     *N,*01,JSTMLR,SLASH,JSTCNT:
.                      *10,NAME1:
.                      *37,DATEPRT1:
.                      *46,JSTINVNO,DASH,JSTSUBNO:
.                      *64,AR:
.                      *79,AP1:
.                      *89,LRINC:
.                      *99,CTAX:
.                      *108,STAX:
.                      *118,POST:
.                      *129,JSTREASN,JSTISTAT:
.                   *N:
.                      *1,JSTLR:
.                      *10,NAME2:
.                      *79,AP2:
.                      *89,ninc," Nin":
.                      *129,JSTCD:
..begin patch 1.8
.                   *N,*10,O1DES:
.                      *79,AP3," *LR":
.                      *89,Xninc," *Nin"
..                   *N,*10,O1DES
.
.         ADD       C4 TO PRTLINES
.end patch 1.8
.
         ADD       AR     TO TAR
         ADD       AP1    TO TAP1
         ADD       AP2    TO TAP2
.Begin patch 1.8
          ADD       AP3    TO TAP3
          ADD       XNINC   TO TXNININC
.         ADD       LRINC  TO TLRINC
.end patch 1.8

         ADD       STAX   TO TSTAX
         ADD       CTAX   TO TCTAX
         ADD       POST   TO TPOST
         MOVE      C0     TO AR
         MOVE      C0     TO AP
         MOVE      C0     TO AP1
         MOVE      C0     TO AP2
         MOVE      C0     TO AP3
         MOVE      C0     TO LRINC
         MOVE      C0     TO NINC
         MOVE      C0     TO XNINC
         MOVE      C0     TO STAX
         MOVE      C0     TO CTAX
         MOVE      C0     TO POST
.
         GOTO      RDADJ
.
EOJ     
         CALL      HEADING 
         SUB       TAP FROM TAP
         ADD       TAP1 TO TAP
         ADD       TAP2 TO TAP
         ADD       TLRINC2 TO TLRINC4
..begin patch 1.8
.         PRINT     *N:
.                   *N,*01,MONTH,":":
.                      *65,TAR:
.                      *78,TAP1:
.                      *89,TLRINC:
.                      *101,TCTAX:
.                      *110,TSTAX:
.                      *120,TPOST:
.                   *N,*78,TAP2:
.                      *89,TNININC, "NIN":
.                   *N,*78,TAP3," *LR":
.                      *89,TxNININC," *NIN":
.                   *N,*78,"-----------":
.                   *N,*01,N5,B1,"ADJUSTMENTS",*78,TAP:
.                   *N:
.                   *N,*01,"TOTAL LRINC BROKERAGE RENTAL: ",*88,TLRINC3:
.                   *N,*01,"TOTAL LRINC BROKERAGE XCHNGE: ",*88,TLRINC1:
.                   *N,*01,"TOTAL LRINC LIST MANAGEMENT : ",*88,TLRINC4:
.                   *N,*01,"     AP REASON CODE 11 TOTAL: ",R11:
.                   *N,*01,"  LRINC REASON CODE 12 TOTAL: ",R12:
.                   *N,*01,"     AP REASON CODE 14 TOTAL: ",R14:
.                   *N,*01,"     AR REASON CODE 16 TOTAL: ",R16:
.                   *N,*01,"POSTAGE REASON CODE 20 TOTAL: ",R20:
.                   *N,*01,"     AP REASON CODE 21 TOTAL: ",R21:
.                   *N,*01,"     AR REASON CODE 22 TOTAL: ",R22:
.                   *N,*01,"     AP REASON CODE 23 TOTAL: ",R23:
.                   *N,*01,"     AP REASON CODE 24 TOTAL: ",R24:
.                   *N,*01,"     AR REASON CODE 25 TOTAL: ",R25AR:
.                   *N,*01,"     AP REASON CODE 25 TOTAL: ",R25AP:
.                   *N,*01,"  LRINC REASON CODE 25 TOTAL: ",R25LR:
.                   *N,*01,"  LRINC REASON CODE 26 TOTAL: ",R26:
.                   *N,*01,"Prepay  REASON CODE 27 TOTAL: ",R27:
.                   *N,*01,"  LRINC REASON CODE 30 TOTAL: ",R30:
.                   *N,*01,"  LRINC REASON CODE 31 TOTAL: ",R31:
.                   *F
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,MONTH,":";                   
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,TAR;                                       
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,TAP1;                                                     
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,TLRINC;                                                                  
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,TCTAX;                                                                                  
                prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,TSTAX;                                                                                                 
                prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,TPOST;                                                                                                 
          add     eightlpi,row                 
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,TAP2;      
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,TNININC;
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,"< NIN";                                                                                  
          add     eightlpi,row                                
.BEGIN PATCH 3.82
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,"LR >";                                                     
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,TAP3;      
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,TXNININC;
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,"< NIN";                                                                                  
          add     eightlpi,row                                
.END PATCH 3.82
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,"------------";          
          add     eightlpi,row                 
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,N5,B1,"ADJUSTMENTS";                              
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,TAP;                    
          add     eightlpi,row                 
          add     eightlpi,row                           
          prtpage prfile;*pcolumn:row,*line=column10R:row;          
          add     eightlpi,row  
          add     eightlpi,row            
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"TOTAL LRINC BROKERAGE RENTAL: ";
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,TLRINC3;     
          add     eightlpi,row                 
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"TOTAL LRINC BROKERAGE XCHNGE: ";
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,TLRINC1;   
          add     eightlpi,row                 
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"TOTAL LRINC LIST MANAGEMENT : ";
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,TLRINC4;                    
          add     eightlpi,row                 
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 11 TOTAL: ";                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R11;                   
          add     eightlpi,row                           
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 12 TOTAL: ";                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R12;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 14 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R14;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 16 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R16;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"POSTAGE REASON CODE 20 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R20;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 21 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R21;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 22 TOTAL: ";                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R22;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 23 TOTAL: ";                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R23;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 24 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R24;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 25 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R25AR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 25 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R25AP;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 25 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R25LR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 26 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R26;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"Prepay  REASON CODE 27 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R27;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"REASON CODE 29 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R29;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 30 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R30;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 31 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R31;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 35 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R35AR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 35 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R35AP;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 35 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R35LR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 36 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R36AR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 36 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R36AP;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 36 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R36LR;                 
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 37 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R37AR;                 
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 37 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R37AP;                 
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 37 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,R37LR;                 
          add     eightlpi,row                                                         
          add     eightlpi,row  
          prtpage prfile;*pcolumn:row,*line=column10R:row;                    
          add     eightlpi,row
          add     eightlpi,row                           
          
                
          call      Heading   
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"Pacific Lists",":";                   
          add     eightlpi,row                 
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,PLTAR;                                       
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,PLTAP1;                                                     
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,PLTLRINC;                                                                  
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,PLTCTAX;                                                                                  
                prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,PLTSTAX;                                                                                                 
                prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,PLTPOST;                                                                                                 
          add     eightlpi,row                 
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,PLTAP2;      
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,PLTNININC;
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,"< NIN";                                                                                  

.BEGIN PATCH 3.82
          add     eightlpi,row                                
                prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,"LR >";                                                     
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,PLTAP3;      
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,PLTXNININC;
                prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,"< NIN";                                                                                  
.ENd PATCH 3.82


          add     eightlpi,row                                
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,"------------";          
          add     eightlpi,row                 
.                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,B1,"ADJUSTMENTS";                              
                prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,PLTAP;                    
          add     eightlpi,row                 
          add     eightlpi,row                           
          prtpage prfile;*pcolumn:row,*line=column10R:row;          
          add     eightlpi,row  
          add     eightlpi,row            
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"TOTAL LRINC BROKERAGE RENTAL: ";
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,PLTLRINC3;     
          add     eightlpi,row                 
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"TOTAL LRINC BROKERAGE XCHNGE: ";
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,PLTLRINC1;   
          add     eightlpi,row                 
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,"TOTAL LRINC LIST MANAGEMENT : ";
                prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,PLTLRINC4;                    
          add     eightlpi,row                 
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 11 TOTAL: ";                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR11;                   
          add     eightlpi,row                           
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 12 TOTAL: ";                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR12;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 14 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR14;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 16 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR16;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"POSTAGE REASON CODE 20 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR20;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 21 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR21;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 22 TOTAL: ";                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR22;                   
          add     eightlpi,row                                     
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 23 TOTAL: ";                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR23;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 24 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR24;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 25 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR25AR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 25 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR25AP;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 25 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR25LR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 26 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR26;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"Prepay  REASON CODE 27 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR27;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  REASON CODE 29 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR29;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 30 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR30;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 31 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR31;                   
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 35 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR35AR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 35 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR35AP;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 35 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR35LR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 36 TOTAL: ";                                       
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR36AR;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 36 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR36AP;                 
          add     eightlpi,row                                               
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 36 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR36LR;                 
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AR REASON CODE 37 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR37AR;                 
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"     AP REASON CODE 37 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR37AP;                 
          add     eightlpi,row                                                         
                prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,"  LRINC REASON CODE 37 TOTAL: ";                                                      
                prtpage prfile;*pColumn3:row,*ALIGNMENT=*Right,*ll,PLR37LR;                 
          add     eightlpi,row                                                         
          add     eightlpi,row  
          prtpage prfile;*pcolumn:row,*line=column10R:row;                    
          add     eightlpi,row
          add     eightlpi,row                                                       
          
         release
              prtclose prfile 
                   
         BRANCH    PRTFLAG TO END
.         print     hpreset
.         release
.         SPLCLOSE
.         CALL      REMVTOF
.end patch 1.8
         GOTO      END
+............................................................................
.
HEADING  ADD       C1 TO PAGE
.begin patch 1.8
         compare    c1 to page
              if not equal
               PRTPAGE prfile;*NEWPAGE:
               *UNITS=*HIENGLISH:
                       *ORIENT=*PORTRAIT:
               *Duplex=2                                                   
              endif

         compare    c1 to page
         if         equal
.         print      hp17ptch,hpdupl,hptop,*f
.         PRINT     HPtmsr17,hpdupl,hptop:                .compressed
.                   033,"&l66P":               page length
.                   033,"&l65F":               number lines
.                   *f
         endif
              clear     row
        move      "200",row
        prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8i,"Confidential";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*p7050:row,*ALIGNMENT=*LEFT,*font=font8,"Date:",str10;        
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"Names in the News",*boldoff;           
        add     eightlpi,row        
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"Daily Adjustment Register",*boldoff;         
        add     eightlpi,row        
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,MONTH,*boldoff;          
        add     eightlpi,row                
        add     eightlpi,row        
        prtpage prfile;*p7400:row,*ALIGNMENT=*Left,*font=font8,*ll,"Page:",Page;                  
        add     eightlpi,row        
        add     eightlpi,row  
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"Client",*boldoff;         
              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*boldon,*ll,"Credit",*boldoff;                              
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"Invoice",*boldoff;                                            
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*boldon,*ll,"Accounts",*boldoff;                          
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*boldon,*ll,"Accounts",*boldoff;                          
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*boldon,*ll,"LR/NIN",*boldoff;                            
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*boldon,*ll,"City",*boldoff;                              
              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*boldon,*ll,"State",*boldoff;                      
              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*boldon,*ll,"Our",*boldoff;         
        add     eightlpi,row          
              prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"LR##",*ULOFF,*boldoff;             
              prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Bill-To/List",*ULOFF,*boldoff;                          
              prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Date",*ULOFF,*boldoff;                   
              prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"Number",*ULOFF,*boldoff;                                
              prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Receivable",*ULOFF,*boldoff;                          
              prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Payable",*ULOFF,*boldoff;                             
              prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Income",*ULOFF,*boldoff;                              
              prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Tax",*ULOFF,*boldoff;                  
              prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Tax",*ULOFF,*boldoff;           
              prtpage prfile;*pColumn9R:row,*ALIGNMENT=*Right,*ULON,*boldon,*ll,"Postage",*ULOFF,*boldoff;                      
        add     eightlpi,row  
        add     eightlpi,row  

.         PRINT        *f,*n,"CONFIDENTIAL":
.                      *54,"NAMES IN THE NEWS":
.                      *119,"DATE: ",TODAY:
.                   *N,*52,"Daily Adjustment Register":
.                      *119,"PAGE:    ",PAGE:
.                   *N,*58,MONTH:
.                   *N:
.                   *N,*01,"CLIENT##":
.                      *10,"BILL-TO/LIST":
.                      *38,"CREDIT":
.                      *47,"INVOICE":
.                      *67,"ACCOUNTS":
.                      *81,"ACCOUNTS":
.                      *95,"LR":
.                      *105,"CITY":
.                      *113,"STATE":
.                      *122,"OUR":
.                   *N:
.                      *3,"LR##":
.                      *39,"DATE":
.                      *48,"NUMBER":
.                      *66,"RECEIVABLE":
.                      *82,"PAYABLE":
.                      *93,"INCOME":
.                      *105,"TAX":
.                      *114,"TAX":
.                      *121,"POSTAGE"
.         MOVE      C8 TO PRTLINES
.end patch 1.8
         RETURN
NJSTCALC
         ADD       jstar TO AR
         ADD       jstar TO ARWOPP
         ADD       jstap1 TO AP1
         ADD       jstap1 TO AP
.
         ADD       jstap2 TO AP2
         ADD       jstap2 TO AP
.begin patch 1.8
         ADD       jstap3 TO AP3
         ADD       jstXninc to XNINC
.end patch 1.8
.
         ADD       jstlrinc to lRINC
         ADD       jstNINinc to NINC
.
.
.
          add       jststax to taxes
          add       jststax to stax
.
          add       jstctax to taxes
          add       jstctax to ctax
.
         add        jstpost to post
.
         RETURN
+...........................................................................
.
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
+
         INCLUDE   NBILIO.inc    BILL-TO
+
         INCLUDE   NORDIO.inc
+
         INCLUDE   NJSTIO.inc    ADJUSTMENT DETAIL
+
+
         INCLUDE   COMLOGIC.inc

