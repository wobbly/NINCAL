PC       EQU       0
         INC       COMMON.inc
         include   cons.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NDATDD.inc
.patch3.95
                                        include   compdd.inc
                                        include   cntdd.inc
.         INC       NMLRDD.inc
.patch3.95
.Patch 3.97
                              include winapi.inc
.Patch 3.97
         include   ncntdd.inc
release  init      "5.6"          DLH   Convert from prtpage/pdf to Excel
reldate   Init      "25 July 2012"
.release  init      "5.51"          DLH   Yearly Update
.reldate   Init      "12 December 2011"
.release  init      "5.5"          DLH   Yearly Update
.reldate   Init      "04 January 2011"
.release  init      "5.4"          DLH   Yearly Update  add Susan's printer
.reldate   Init      "09 November 2010"
.release  init      "5.3"          DLH   Copyright
.reldate   Init      "19 August 2010"
.release  init      "5.2"          DLH   Yearly Update
.reldate   Init      "24 September 2009"
.release  init      "5.1"          DLH   sendmail
.reldate   Init      "24 April 2008"
.release  init      "5.0"          DLH  08FEB2008 NO Mas
.release  init      "4.0"          JD   27Mar2007 Yearly Update
.release  init      "3.99"        DMB   20JAN2006 Yearly Update
.release  init      "3.98"        DMB   19JAN2006 Fix to correctly display numeric formatting        
.release  init      "3.97"        DMB   13JAN2005 Yearly UPdate
.release  init      "3.96"        ASH   06AUG2004 Logo Conversion
.release  init      "3.95"        DMB   26MAY2004 Mailer Conversion
.Release   INIT       "3.94"         04/28/03 Added option for the order/maildate selection to be made by nord006b
.Release   INIT       "3.yr6"         03/20/03 Added pdf option and clean up code to allow plbserv to do prtopen (does not like \\NINs2\Laser? just doing laser?
.Release   INIT       "3.yr5"         08/06/02 Added New FY/calendar Year
.Release   INIT       "3.yr4"         07/23/02 Added code for osflag for XP
.Release   INIT       "3.9"         05/02/02 Centered Header for List Name
.Release   INIT       "3.8"         01/17/01 Allows for multiples list to be picked
.Release   INIT       "3.7"         01/15/01 Made totals tighter and allowed one more row before going to new page for totals
.Release   INIT       "3.6"         Fixed bug which did not print footer or pg# on pg previous to total page
.Release   INIT       "3.5"        Added trapping function for spool error
.                                  Added Code for OS and where to print
.Release   INIT       "3.4"        .DB 24Aug2001 Added var to dsinit to carry over prin area
.Release   INIT      "3.3"         .DB 21Aug2001  modification of multiple copy
.Release   INIT      "3.2a"        .DB 21Aug2001  multiple copy capabilities added
.Release   INIT      "3.1"         .DB 20Aug2001  a,b Added Copyright to listsum,added font9,
.                                                            trim # of mlr's per page for aesthetics
.Release  INIT      "3.0"          New Listsum print program added
.release  init      "2.6"          .JD31jul01 added 2002.
.release  init      "2.5"          .DLH06Feb01 fix to  2001.
.release  init      "2.4"          .JD05sep00 added 2001.
.release  init      "2.3"          JD20apr99 fixed date mode for fiscal.
.release  init      "2.2b"         JD  29JAN99 skip pending orders.
.RELEASE  INIT      "2.2"          ASH 11JAN99 NINORD Y2K, File expansion
.release  init      "2.1"          ASH 21SEP98 NINMLR Y2K File expansion
.release  init      "2.0"          JD  07aug98 add fiscal 99
.release  init      "1.9"          JD  04aug97 add fiscal 98
.release  init      "1.8"          DLH 20sep96 add fiscal 97
.RELEASE  INIT      "1.7"          JD  26jan95  fiscal 96.
.RELEASE  INIT      "1.6"        JD  16may94  fiscal 95.
.RELEASE  INIT      "1.5"        JD  22MAR94 added exch/rent totals for year.
.RELEASE  INIT      "1.4"       DLH 12jan94 fix fiscal 94.
.RELEASE  INIT      "1.3"       DLH 09NOVyr6 USE DSINIT, CHAIN FROM UTIL.
.
.RELEASE  INIT      "1.2"       DLH 24FEByr5 MLR/DAT INCLUDES.
.                              AND 19yr5 DATA
.RELEASE  INIT      "1.1"      D. HERRICK 07MARyr4

.>Patch 3.98 Variable added
mask15      init    "(Z,ZZZ,ZZZ,ZZ9)"        ;formatting vars
Dim15a      dim     15            ;formatting vars
.>Patch 3.98
.Begin patch 5.6
.to find version of excel
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.to find version of excel
books   automation
book    automation
sheets  automation
sheet   automation
ex      automation      class="Excel.Application"
.Variant objects used to talk to outside applications
xlRowHeight         variant
VT_R8     EQU 5           .Double - 8 byte Real
.Booleans
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom85  variant
.Formatting vars needed
AlignLeft     integer 4,"0xffffefdd"
SheetsDefault integer 4,"0x00000000"
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
MedThick integer 4,"0xFFFFEFD6"
xlColumnWidth variant
VT_R8a         EQU 5           .Double - 8 byte Real

.
range     dim       20
range2    dim       20
ExcelFlag FOrm      1
FileCheck FIle
trapcount form      4
.end patch 5.6


ListFle file
.Patch 3.97
.timestamp1 dim  16
.time1   form    16
.time2   form    16
.time3   form    16
.Patch 3.97
RESULT2 FORM    9
RESULT3 FORM    9
.Files to open
prfile  pfile
.Testing only
.=========================================
.PRTNAME form    1
.==========================================
INPUT    DIM       30
ORDMO   FORM       2
INFILE   FILE      VAR=498
.OUTPUT   IFILE     KEYLEN=80,FIX=5yr4     .KEY= LIST NUMBER + MAILER NAM
.begin patch 4.1
.OUTPUT   IFILE     KEYLEN=80,FIX=681     .KEY= LIST NUMBER + MAILER NAM
OUTPUT   IFILE     KEYLEN=80,FIX=271    .KEY= LIST NUMBER + MAILER NAM
.end patch 4.1
dteflag form      1                    1=maildate 2=orderdate
.begin patch 5.4
.begin patch 5.5
.yrOne     init      "05"
.yrTwo     init      "06"
.yrThree   init      "07"
.yrFOur    init      "08"
.yrFive    init      "09"
.yrSix     init      "10"
.begin patch 5.51
.YrZero    Init      "05"
.yrOne     init      "06"
.yrTwo     init      "07"
.yrThree   init      "08"
.yrFOur    init      "09"
.yrFive    init      "10"
.yrSix     init      "11"
YrZero    Init      "06"
yrOne     init      "07"
yrTwo     init      "08"
yrThree   init      "09"
yrFOur    init      "10"
yrFive    init      "11"
yrSix     init      "12"
.end patch 5.51
.end patch 5.5
.yrOne     init      "04"
.yrTwo     init      "05"
.yrThree   init      "06"
.yrFOur    init      "07"
.yrFive    init      "08"
.yrSix     init      "09"
.end patch 5.4
.yrOne     init      "03"
.yrTwo     init      "04"
.yrThree   init      "05"
.yrFOur    init      "06"
.yrFive    init      "07"
.yrSix     init      "08"
.end patch 5.2
c12      form      "12"
datechk  dim       2
dateyy   dim       2
QUANT    FORM      9
Xquant   form      9
rquant   form      9
TORDQTYR FORM      9
KEY      DIM       6
BRANCH   FORM      2
START     FORM     "00"
FISCMO   DIM       2
MODE     FORM      1
FUNCBR   FORM      "0"       PROGRAM CONTROL BRANCH.
c14      form      "14"
c15      form      "15"
.Update
c16      form      "16"
c17      form      "17"
.Patch 3.97
c18      form      "18"
.Patch 3.97
.Patch 3.99
c19      form      "19"
.Patch 3.99
.Patch 4.1
c21      form      "21"
.Patch 4.1
HoldLst  dim       35
newlst   form      1                             .list break?  need to create a new excel sheet if break
lstrow   form      "840"
.===========================================================================

.===========================================================================

.hexeight integer 4,"4294967295"
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
font7   font
font8   font
font9   font
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
        create  font6,"Arial",size=14
        create  font7,"Times New Roman",size=9
        create  font8,"Times New Roman",size=10
        create  font9,"Times New Roman",size=10,italic
badfaxflag dim     1            =Y if Facsys printer not defined
.patch3.yr4
.osflag   form   1          1=win 95,98, 2=NT
bosflag   form   1          1=win 95,98, 2=NT
.patch3.yr4
.==============================================================================
columna  form    9
Title1 form    9
Title2 form    9
Title3 form    9
Title4 form    9
column5a form    9
column6a form    9
column7a form    9


.Fiscal/Calendar Flg
.Calflg   form    1
Newpg    form    1
.Fiscal Headers
.UPdate
.Patch 3.99
.Patch 3.97
.FY1 init "FY 00/01"
.Patch 4.1
.FY1 init "FY 01/02"
.FY2 init "FY 02/03"
.FY3 init "FY 03/04"
.FY4 init "FY 04/05"
.FY5 init "FY 05/06"
.FY6 init "FY 06/07"
.begin patch 5.4
.begin patch 5.5
.FY1       init "FY 04/05"
.FY2       init "FY 05/06"
.FY3       init "FY 06/07"
.FY4       init "FY 07/08"
.FY5       init "FY 08/09"
.FY6       init "FY 09/10"
.begin patch 5.51
FY1       init "FY 06/07"
FY2       init "FY 07/08"
FY3       init "FY 08/09"
FY4       init "FY 09/10"
FY5       init "FY 10/11"
FY6       init "FY 11/12"
.FY1       init "FY 05/06"
.FY2       init "FY 06/07"
.FY3       init "FY 07/08"
.FY4       init "FY 08/09"
.FY5       init "FY 09/10"
.FY6       init "FY 10/11"
.end patch 5.51
.end patch 5.5
.begin patch 5.2
.FY1       init "FY 03/04"
.FY2       init "FY 04/05"
.FY3       init "FY 05/06"
.FY4       init "FY 06/07"
.FY5       init "FY 07/08"
.FY6       init "FY 08/09"
.end patch 5.4
.FY1 init "FY 02/03"
.FY2 init "FY 03/04"
.FY3 init "FY 04/05"
.FY4 init "FY 05/06"
.FY5 init "FY 06/07"
.FY6 init "FY 07/08"
.end patch 5.2
.Calendar Headers
.UPdate
.YR1 init "2002"
.Patch 4.1
.begin patch 5.4
.begin patch 5.5
.YR1 init "2006"
.YR2 init "2007"
.YR3 init "2008"
.YR4 init "2009"
.YR5 init "2010"
.begin patch 5.51
YR1 init "2008"
YR2 init "2009"
YR3 init "2010"
YR4 init "2011"
YR5 init "2012"
.YR1 init "2007"
.YR2 init "2008"
.YR3 init "2009"
.YR4 init "2010"
.YR5 init "2011"
.end patch 5.51
.end patch 5.5
.begin patch 5.2
.YR1 init "2005"
.YR2 init "2006"
.YR3 init "2007"
.YR4 init "2008"
.YR5 init "2009"
.end patch 5.4
.YR1 init "2004"
.YR2 init "2005"
.YR3 init "2006"
.YR4 init "2007"
.YR5 init "2008"
.end patch 5.2
.Patch 3.97
.Patch 3.99
.==============================================================================
.Output FIle
.fill                      1-1
.OLIST    DIM       35         2-36
.OMLR     DIM       55        37-91
Oyr1      FORM      10        81-90       yr1 NAMES (MD)
Xyr1      FORM      10        91-100       yr1 NAMES (MD)
Ryr1      FORM      10       101-110      yr1 NAMES (MD)
Oyr2      FORM      10       111-120      yr2 NAMES (MD)
Xyr2      FORM      10       121-130      yr2 NAMES (MD)
Ryr2      FORM      10       131-140      yr2 NAMES (MD)
Oyr3      FORM      10       141-150      yr3 NAMES (Md)
Xyr3      FORM      10       151-160      yr3 NAMES (MD)
Ryr3      FORM      10       161-170      yr3 NAMES (MD)
Oyr4      FORM      10       171-180      yr4 NAMES (Md)
Xyr4      FORM      10       181-190      yr4 NAMES (MD)
Ryr4      FORM      10       191-200      yr4 NAMES (MD)
Oyr5      FORM      10       201-210      yr5 NAMES (Md)
Xyr5      FORM      10       211-220      yr5 NAMES (MD)
Ryr5      FORM      10       221-230      yr5 NAMES (MD)
Oyr6      FORM      10       231-240      yr6 NAMES (Md)
Xyr6      FORM      10       241-250      yr6 NAMES (MD)
Ryr6      FORM      10       251-271      yr6 NAMES (MD)



.================================================================================
.Patch3.8
splfle    dim     45
spldir    init    "c:\work\listsum"
listnum   dim      3
spoolname DIM     30
RowCount  form     9
PgCnt     form     9
Copy      form     4

KEY80    DIM       80         KEY VAR.
MlrTot   form      10         Mailer Total Order
TotOrd   form      10         TOTAL Order

OYR01     form      10         01 Order Grand Total
Xyr01     form      10         01 Xchange Grand Total
RYR01     form      10         01 Rental  Grand Total
OYR02     form      10         02 Order Grand Total
XYR02     form      10         02 Xchange Grand Total
RYR02     form      10         02 Rental  Grand Total
OYR03     form      10         03 Order Grand Total
XYR03     form      10         03 Xchange Grand Total
RYR03     form      10         03 Rental  Grand Total
OYR04     form      10         04 Order Grand Total
XYR04     form      10         04 Xchange Grand Total
RYR04     form      10         04 Rental  Grand Total
OYR05     form      10         05 Order Grand Total
XYR05     form      10         05 Xchange Grand Total
RYR05     form      10         05 Rental  Grand Total
OYR06     form      10         06 Order Grand Total
XYR06     form      10         06 Xchange Grand Total
RYR06     form      10         06 Rental  Grand Total

OTOT     form      10         ORDER   GRAND Total
XTOT     form      10         Xchange Grand Total
RTOT     form      10         Rental  Grand Total
.=============================================================================
.patch3.yr6
PDFOPT   DIM        1
FMONTH   FORM       2
LUSER    DIM        10
.patch3.yr6


.Sort Parameters=======================================================
INDAT    init  "LISTSUM.DAT"   .File to be sorted
OUTSRT   init  "LISTSUM.SRT"   .Partial Name of sorted Output file for ncsh
LSTSRT   init  "2-36,37-81"   .Sort
SRTDIR   init  "c:\work\"                         ."
SORTFLE  dim    100                          .Var to pack file names of sort
.==========================================================================


         move      c3 to ncntpath
.Patch 3.2a........................
.Var for # of copies
         move      inits to Copy
.patch3.yr6
         move levels to PDFOPT
         move prio to FMONTH
         move user to LUSER
.patch3.yr6
....................................
         MOVE      FUNC TO FUNCBR
         CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        EOS                 .NO
         move      c1 to dteflag
         MOVE      "NORD0028a" TO PROGRAM
         MOVE      "Names In The News" TO COMPNME
         KEYIN     *P10:12,*EL,"INPUT FILE NAME : ",INPUT
         endif
         MOVE      "LIST SUMMARY PREPARATION" TO STITLE
         CALL      PAINT
         MOVE      "EXIT" TO PF5
         CALL      FUNCDISP
         TRAP      EOJ IF F5
.
INPGET   TRAP      INPNG GIVING ERROR IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         MOVE      INPNAME TO input
         close     testfile
         GOTO      process
INPNG    NORETURN
         TRAPCLR   IO
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *DV,ERROR:
                   *P15:06,INPNAME
         GOTO      INPGET
.
process  MOVE     C1 TO NDATPATH
         OPEN      INFILE,INPUT,READ
         TRAP      NOOUT IF IO
         IFNZ      PC
         OPEN      OUTPUT,"LISTSUMMARY:PRINT",EXCLUSIVE
         XIF
         IFZ       PC
         OPEN      OUTPUT,"C:\work\LISTSUM",EXCLUSIVE
         XIF
         TRAPCLR   IO
        Display     *P10:14,*EL,"OUTPUT FILE ALREADY EXISTS ",*CYAN,"Preparing a New One"
         CLOSE     OUTPUT
NOOUT    TRAPCLR   IO
         IFNZ      PC
         PREPARE   OUTPUT,"LISTSUMMARY:PRINT"
         XIF
         IFZ       PC
         PREPARE   OUTPUT,"C:\work\LISTSUM","C:\work\listsum","80","271"
         XIF
         NORETURN
CHECK
         compare   funcbr to c1
         IF        EQUAL
         MOVE      C1 TO MODE
         move      fmonth to fiscmo
         rep       zfill,fiscmo
         CALL      FISCAL
                    pause c2
         ELSE
         MOVE      C2 TO MODE
         ENDIF
CHECK2

          MOVE      CURSYS to str1
         if (str1 = "1")
            DISPLAY   *P10:12,"Select Mail Date"
         endif
         if (str1 = "2")
            DISPLAY   *P10:12,"Select Order Date"
         endif
         move      str1 to dteflag
         branch    dteflag to input,input
         move      c1 to dteflag
INPUT
         CALL      READ
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
         MATCH     OLNUM TO LSTNUM
         IF        NOT EQUAL
         MOVE      OLNUM TO NDATFLD
         CALL      NDATKEY
         ENDIF
         BRANCH    MODE OF OUTPUT,OUTPUT2
READ     READ      INFILE,SEQ;ORDVARS
         GOTO      EOJ IF OVER
         ADD       C1 TO N8
         DISPLAY   *P10:14,*EF,"RECORDS PROCESSED ",N8
.begin patch 2.2b
         if        (olrn = "509167" or olrn = "505639" or olrn = "671302" or olrn = "678629")
test
         call      debug
         endif
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      read IF EQUAL     YES, skip.
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      read IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       LCR order ?
         GOTO      read IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       Cancelled LCR order ?
         GOTO      read IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
.end patch 2.2b
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      READ IF EQUAL
         RETURN
.fiscal
OUTPUT
.Start Patch #2.1 - Increase file and key size to reflect increase in MCOMP
.         PACK      KEY60 FROM OLSTNAME,MCOMP
         PACK      KEY80 FROM OLSTNAME,MCOMP
.End Patch #2.1 - Increase file and key size to reflect increase in MCOMP
         MOVE      C0 TO QUANT
         MOVE      C0 TO RQUANT
         MOVE      C0 TO XQUANT
         MOVE      C0 TO TORDQTYR
         MOVE      OQTY TO QUANT
         CALL      RENTXCHG
.Start Patch #2.3 - replaced date
         compare   c2 to dteflag
         if        equal
         move      oodtey to omdtey
         move      oodtem to omdtem
         endif
.End Patch #2.3 - replaced date
         MOVE      C0 TO BRANCH
.YEARLY
.testing  
          if        (omdtey = "05")
          call      Debug
          endif
          MOVe      OMDTEM,OrdMO
          IF        (Omdtey = YRZero & ORdMO >= Start)       .1st fiscal
          MOVE      C1,Branch
          Elseif    (Omdtey = YROne & ORdMO < Start)         .1st fiscal
          MOVE      C1,Branch
          Elseif    (OMdtey = YRone & ORdMO >= Start)         .2nd fiscal
          MOVE      C2,Branch
          Elseif    (Omdtey = YRTwo & ORdMO < Start)       .   2nd fiscal
          MOVE      C2,Branch
          Elseif    (OMdtey = YrTwo & ORdMO >= Start)         .3rd fiscal
          MOVE      C3,Branch
          Elseif    (Omdtey = YrTHree & ORdMO < Start)       .3rd fiscal
          MOVE      C3,Branch
          Elseif    (OMdtey = YrThree & ORdMO >= Start)       .4th fiscal
          MOVE      C4,Branch
          Elseif    (Omdtey = YrFour & ORdMO < Start)       .4th fiscal
          MOVE      C4,Branch
          Elseif    (OMdtey = YrFour & ORdMO >= Start)       .5th fiscal
          MOVE      C5,Branch
          Elseif    (Omdtey = YrFive & ORdMO < Start)       .5th fiscal
          MOVE      C5,Branch
          Elseif    (OMdtey = YrFive & ORdMO >= Start)       .6th fiscal
          MOVE      C6,Branch
          Elseif    (Omdtey = YRSix & ORdMO < Start)       .6th fiscal
          MOVE      C6,Branch
          Endif
.         MATCH     "03" TO OMDTEY
.         IF        EQUAL
.        MOVE       OMDTEM TO ORDMO
.         COMPARE   START TO ORDMO
.         IF        NOT LESS
.         MOVE      C1 TO BRANCH
.         ENDIF
.         ENDIF
..
.         MATCH     "04" TO OMDTEY
.         IF        EQUAL
.         MOVE      OMDTEM TO ORDMO
.         COMPARE   START TO ORDMO
.         IF        NOT LESS
.         MOVE      C2 TO BRANCH
.          ELSE
.         MOVE      C1 TO BRANCH
.        ENDIF
.        ENDIF
..
.        MATCH     "05" TO OMDTEY
.         IF        EQUAL
.         MOVE      OMDTEM TO ORDMO
.        COMPARE    START TO ORDMO
.        IF         NOT LESS
.         MOVE      C3 TO BRANCH
.         ELSE
.         MOVE      C2 TO BRANCH
.         ENDIF
.         ENDIF
..
.         MATCH     "06" TO OMDTEY
.         IF        EQUAL
.         MOVE      OMDTEM TO ORDMO
.         COMPARE   START TO ORDMO
.         IF        NOT LESS
.         MOVE      C4 TO BRANCH
.          ELSE
.        MOVE     C3 TO BRANCH
.         ENDIF
.         ENDIF
..
.         MATCH     "07" TO OMDTEY
.         IF        EQUAL
.         MOVE      OMDTEM TO ORDMO
.         COMPARE   START TO ORDMO
.         IF        NOT LESS
.         MOVE      C5 TO BRANCH
.          ELSE
.         MOVE      C4 TO BRANCH
.         ENDIF
.         ENDIF
.
.         MATCH     "08" TO OMDTEY
.         IF        EQUAL
.         MOVE      OMDTEM TO ORDMO
.         COMPARE   START TO ORDMO
.         IF        NOT LESS
.         MOVE      C6 TO BRANCH
.          ELSE
.         MOVE      C5 TO BRANCH
.         ENDIF
.         ENDIF
.
..
          BRANCH    BRANCH OF OK,OK,OK,OK,OK,OK
         GOTO      INPUT
OUTPUT2
         PACK      KEY80 FROM OLSTNAME,MCOMP
         MOVE      C0 TO QUANT
         MOVE      C0 TO BRANCH
         MOVE      C0 TO RQUANT
         MOVE      C0 TO XQUANT
         MOVE      C0 TO TORDQTYR
         MOVE      OQTY TO QUANT
         CALL      RENTXCHG
         compare   c1 to dteflag
         if        equal
         move      omdtey to datechk
         else
         move      oodtey to datechk
         endif
         COMPARE   C1 TO DTEFLAG
         IF        EQUAL
         move      omdtem to mm
         move      omdted to dd
         move      omdtey to yy
         ELSE
         move      oodtem to mm
         move      oodted to dd
         move      oodtey to yy
         endif
         move      c1 to branch
dateloop load      dateyy from branch of YRZero, yrone,yrtwo,yrthree,yrfour,yrfive,yrsix
.end patch 4.1
.;update .Patch 3.97 3.99
         match     datechk to dateyy
         goto      ok if equal
         add       c1 to branch
          Branch    Branch of dateloop,dateloop,dateloop,dateloop,dateloop,dateloop
          goto      input
.both calendar and FIscal use this section
OK
         READ      OUTPUT,KEY80;B1,KEY80,Oyr1,Xyr1,Ryr1,Oyr2,Xyr2,Ryr2,Oyr3,Xyr3,Ryr3:
                   Oyr4,Xyr4,Ryr4,Oyr5,Xyr5,Ryr5,Oyr6,Xyr6,Ryr6
         GOTO      WRITE IF OVER
         CALL      ADD
         UPDATE    OUTPUT;B1,KEY80,Oyr1,Xyr1,Ryr1,Oyr2,Xyr2,Ryr2,Oyr3,Xyr3,Ryr3:
                   Oyr4,Xyr4,Ryr4,Oyr5,Xyr5,Ryr5,Oyr6,Xyr6,Ryr6
         GOTO      INPUT
WRITE
         PACK      KEY80 FROM OLSTNAME,MCOMP
         MOVE      C0 TO Oyr1
         MOVE      C0 TO Oyr2
         MOVE      C0 TO Oyr3
         MOVE      C0 TO Oyr4
         MOVE      C0 TO Oyr5
         MOVE      C0 TO Oyr6
         MOVE      C0 TO Xyr1
         MOVE      C0 TO Xyr2
         MOVE      C0 TO Xyr3
         MOVE      C0 TO Xyr4
         MOVE      C0 TO Xyr5
         MOVE      C0 TO Xyr6
         MOVE      C0 TO Ryr1
         MOVE      C0 TO Ryr2
         MOVE      C0 TO Ryr3
         MOVE      C0 TO Xyr6
         MOVE      C0 TO Ryr4
         MOVE      C0 TO Ryr5
         MOVE      C0 TO Ryr6
         CALL      ADD
         WRITE     OUTPUT,KEY80;B1,KEY80,Oyr1,Xyr1,Ryr1,Oyr2,Xyr2,Ryr2,Oyr3,Xyr3,Ryr3:
                   Oyr4,Xyr4,Ryr4,Oyr5,Xyr5,Ryr5,Oyr6,Xyr6,Ryr6
         GOTO      INPUT
.UPdate
.Patch 3.99
.Patch 3.97
ADD      BRANCH    BRANCH OF Oyr1,Oyr2,Oyr3,Oyr4,Oyr5,Oyr6
         NORETURN
         GOTO      INPUT
Oyr1      ADD       QUANT TO Oyr1
         ADD       XQUANT TO Xyr1
         ADD       RQUANT TO Ryr1
         RETURN
Oyr2      ADD       QUANT TO Oyr2
         ADD       XQUANT TO Xyr2
         ADD       RQUANT TO Ryr2
         RETURN
Oyr3      ADD       QUANT TO Oyr3
         ADD       XQUANT TO Xyr3
         ADD       RQUANT TO Ryr3
         RETURN
Oyr4      ADD       QUANT TO Oyr4
         ADD       XQUANT TO Xyr4
         ADD       RQUANT TO Ryr4
         RETURN
Oyr5      ADD       QUANT TO Oyr5
         ADD       XQUANT TO Xyr5
         ADD       RQUANT TO Ryr5
         RETURN
Oyr6      ADD       QUANT TO Oyr6
         ADD       XQUANT TO Xyr6
         ADD       RQUANT TO Ryr6
         RETURN
FISCAL
.          alert caution,"Start fiscal",result
          KEYIN     *P1:1,*ES,*P10:15,*JR,"ENTER STARTING MONTH FISCAL YEAR ",*t15,*RV,*dv,FISCMO
.          alert caution,"End fiscal",result
.FISCAL   KEYIN     *P1:1,*ES,*P10:15,*JR,"ENTER STARTING MONTH FISCAL YEAR ":
.                   FISCMO
         GOTO      FISCAL IF EOS
         TYPE      FISCMO
         GOTO      FISCAL IF NOT EQUAL
         REP       " 0" IN FISCMO
         MOVE      FISCMO TO START
         RETURN
.
RENTXCHG MOVE      OQTY TO TORDQTYR
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         GOTO      EXCHANGE IF EQUAL
         GOTO      RENT
EXCHANGE
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         COMPARE   C0 TO N9
         GOTO      SPLIT IF NOT EQUAL
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         ADD       n9 TO XQUANT
         RETURN
SPLIT
         ADD       N9 TO XQUANT
         SUB       N9 FROM TORDQTYR
         MOVE      C0 TO N9
         MOVE      TORDQTYR TO N9
         ADD       N9 TO RQUANT
         RETURN
RENT
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         ADD       N9 TO RQUANT
         RETURN
.End Patch #2.2 - replaced var
.
.debug
         return
EOJ      CLOSE     INFILE
         CLOSE     OUTPUT
         NORETURN
.         shutdown  "cls"
.         STOP


.===============================================================================
+..............................................................................
.         move      "Listsum Spool Sect" TO PROGRAM
.         MOVE      "Test - PRINT" TO STITLE
.         move      "Names In Then News Ca Inc" to compnme
.         CLOCK     DATE TO TODAY
.         MOVE      "EXIT" TO PF5
.         CALL      PAINT


START
        getinfo  system,str6
        unpack   str6 into str1,str2
        unpack   str2 into str1
        move     c0 to bosflag
..0 = unknown
..1 = Windows NT
..2 = WIN32s Windows 3.1x (obsolete)
..3 = Window 95
..4 = Window 98
..5 = Windows 2000
..6 = XP
..8 = Windows CE
        if (str1 = "3" or str1 = "4")
                  move     c1 to bosflag
        endif
.patch3.yr4
        if (str1 = "1" or str1 = "5" or str1 = "6")
.        if (str1 = "1" or str1 = "5")
.patch3.yr4
          move     c2 to bosflag
        endif

.***************************************************
         pack   SortFle,srtdir,indat,comma,srtdir,outsrt
.Sort and secondary sort on list
           pack   taskname,sortfle,";",lstsrt
.****************************************************
        sort   taskname
        if over
               alert caution,S$ERROR$,result,"No Sort"
        endif
        clear n3
.                       pack      taskname from NTWKPATH2,"sort32 ","c:\listsum.dat ","c:\listsum.srt ":
.                         "/s (37,45,alp,a)"
.                      execute   taskname

.Rerun
.Set up columns
        move    "100",column
        move    "3100",column1
        move    "4100",column2
        move    "5100",column3
        move    "6100",column4
        move    "7100",column5
        move    "8100",column6
        move    "9500",column7
.       7860 row position of pg #

        move    "100",columna
        move    "5260",Title1
.        move    "4000",Title1
        move    "9000",Title2
        move    "9500",Title3
        move    "5260",Title4
.        move    "4500",Title4
        move    "7275",column5a
        move    "8275",column6a
        move    "9675",column7a
          OPEN      LISTFLE,"C:\work\LISTSUM.SRT",READ
Rerun
.       move c1 to calflg
        Sub  Xyr01,Xyr01
        Sub  Xyr02,Xyr02
        Sub  Xyr03,Xyr03
        Sub  Xyr04,Xyr04
        Sub  Xyr05,Xyr05
        Sub  Xyr06,Xyr06
        Sub  Ryr01,Ryr01
        Sub  Ryr02,Ryr02
        Sub  Ryr03,Ryr03
        Sub  Ryr04,Ryr04
        Sub  Ryr05,Ryr05
        Sub  Ryr06,Ryr06
        Sub  Oyr01,Oyr01
        Sub  Oyr02,Oyr02
        Sub  Oyr03,Oyr03
        Sub  Oyr04,Oyr04
        Sub  Oyr05,Oyr05
        Sub  Oyr06,Oyr06
        Sub      N8,N8
        move  C0 to newpg
.Patch3.4
        Trap SPOOL1 giving error if SPOOL
        move comment to cntprint
        if (cntprint = c0)
                move c3 to cntprint
        endif
.EndPatch3.4

        Display    *P10:11,*EF,*cyan,"Printing in Process!!"
.patch3.yr6
.         pack splfle,spldir,listnum,str4,".lst"
                    clock timestamp,timestamp
          pack splfle,ntwkpath1,luser,timestamp,".lst"
                    pack spoolname,luser,timestamp
.patch3.yr6
.===========================================================================
.Patch3.5
.Patch 3.97
.begin patch 5.6
.do excel crud, get out
.Create the Variant objects
.Booleans
                    create  OTRUE,VarType=VT_BOOL,VarValue=1
                    create  OFALSE,VarType=VT_BOOL,VarValue=0
                    create  Zoom85,VarType=VT_I4,VarValue=1
.
                    create    xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
                    create  ex
          getprop ex,*SheetsInNewWorkbook=SheetsDefault
                  setprop ex,*SheetsInNewWorkbook=C1
                    setprop ex,*WindowState=xlMinimized
.get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
.get exel version info

.Create Workbooks collection
                    getprop ex,*Workbooks=books
.Create/Add a single Workbook
                    books.add
                    books.item giving book using 1
.Create Worksheets collection
                    getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created
                    sheets.item giving sheet using 1
                    setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
                    sheet.range("A1:E1").Merge
                    sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
          Goto      Page
.          endif
.end patch 5.6


.Begin patch 5.6
.                    if (pdfopt = YES)
..>Patch 3.99 Logic Addition for PDF Quality Control
.        call       GetPDFPath
.                    pack      str45 from PDFPATH,"\res\pdf995.ini"
. 
.                   call      "GU$INI;WRITE_TO_INI" USING str45:
.                             "Parameters":
.                             "ProcessPDF":
.                             "\\nins1\e\apps\winbatch\Del995flag.exe":
.                              result
.                              if (result = C0)
..Prepare Flag file
.                                        pack      str45 from PDFPATH,"\flag.dat"
.                                        prep      tempfile,str45
.                                        write     tempfile,SEQ;"flag set"
.                                        close     tempfile
.                              endif
.                              PRTOPEN prfile,"PDF995","LstSum.pdf"
.                    else
.
.                          if (cntprint = "1" | cntprint = "3")      .Laser6
.                                  if (bosflag = c2)
.
.                                        PRTOPEN prfile,"\\NINs2\Laser6",spoolname,noprint,spoolfile=splfle
.
.                                  else
.                                        PRTOPEN prfile,"Laser6",spoolname,noprint,spoolfile=splfle
.                                  endif
.                              elseif (CNTPRINT = 5)     .Susan
.                                    if (bosflag = c2)         .nt
.                                            PRTOPEN prfile,"@Kyocera FS-1030D",spoolname,noprint,spoolfile=splfle
.                                    elseif (bosflag = c6)         .XP
.                                            PRTOPEN prfile,"@Kyocera FS1030D",spoolname,noprint,spoolfile=splfle
.                                    elseif (bosflag = c1)         .win 95 98
.                                            PRTOPEN prfile,"@KYOCERAS",spoolname,noprint,spoolfile=splfle
.                                    else   .(osflag = c0)         .Don't know prompt for printer
.                                            PRTOPEN prfile,"@",spoolname,noprint,spoolfile=splfle
.                                    endif
.                          else                                    .Laser3 = Default
.                                  if (bosflag = c2)
.                                        PRTOPEN prfile,"\\NINs2\Laser3 Blankstock",spoolname,noprint,spoolfile=splfle
.                                  else
.                                            PRTOPEN prfile,"Laser3 Blankstock",spoolname,noprint,spoolfile=splfle
.                              endif
.                    endif
.                    endif
.end patch 5.6
PAGE
        if (newlst = c1)
                clear pgcnt
                clear rowcount
          Sub  Xyr01,Xyr01
          Sub  Xyr02,Xyr02
          Sub  Xyr03,Xyr03
          Sub  Xyr04,Xyr04
          Sub  Xyr05,Xyr05
          Sub  Xyr06,Xyr06
          Sub  Ryr01,Ryr01
          Sub  Ryr02,Ryr02
          Sub  Ryr03,Ryr03
          Sub  Ryr04,Ryr04
          Sub  Ryr05,Ryr05
          Sub  Ryr06,Ryr06
          Sub  Oyr01,Oyr01
          Sub  Oyr02,Oyr02
          Sub  Oyr03,Oyr03
          Sub  Oyr04,Oyr04
          Sub  Oyr05,Oyr05
          Sub  Oyr06,Oyr06
                clear OTOT
                clear XTOT
                clear RTOT
                clear mlrtot
                clear totord
        endif
.PAGE
        add c1 to Pgcnt
.begin patch 5.6
.          if (pgcnt = c1)        
.          prtpage prfile;*UNITS=*HIENGLISH:
.                       *ORIENT=*LANDSCAPE;        
.          else
.          prtpage prfile;*NEWPAGE:
.                 *UNITS=*HIENGLISH:
.                       *ORIENT=*LANDSCAPE;
.        endif
                    if        (pgcnt = c1)
.Header information
                    setprop   sheet.Range("A3"),*Value=Str45
                    setprop   sheet.range("A3").Font,*Bold="True"                               
                    setprop   sheet.Range("A4"),*Value="Names Mailed on"
                    setprop   sheet.range("A4").Font,*Bold="True"                               
                    setprop   sheet.Range("A5"),*Value="Mailer"
                    setprop   sheet.range("A5").Font,*Bold="True"                               
        clock timestamp,str8
        unpack str8,str2,yy,mm,dd
        clear str10
        pack  str10,mm,slash,dd,slash,str2,yy

                    setprop   sheet.Range("B5"),*Value="Date"
                    setprop   sheet.Range("C5"),*Value=Str10
.Header Formatting
                    setprop   sheet.Range("A5:I5"),*HorizontalAlignment=xlAlignCenter
                    setprop   sheet.Range("A5:I5").Font,*Bold="True"
                    sheet.range("A5:G5").BorderAround using *LineStyle=1,*Weight=MedThick
.
                    move      C6,HowMany2                      .starting row
                    
                    endif

.end patch 5.6
.
.
.        clear   row
.        move    "300",row
.        prtpage prfile;*pcolumn:row,*font=font12,"Confidential";
.        prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News";
.        prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date:";
.        clock timestamp,str8
.        unpack str8,str2,yy,mm,dd
.        clear str10
.        pack  str10,mm,slash,dd,slash,str2,yy
.        prtpage prfile;*pTitle3:row,*font=font12,str10;
.       add     eightlpi,row
.       add     eightlpi,row
..       add     eightlpi,row
.        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,*ULON,*ll,"Names Mailed on",*ULOFF;
.       add     eightlpi,row
.       add     eightlpi,row
.       add     eightlpi,row
.       add     eightlpi,row
.       add     eightlpi,row
.       add     eightlpi,row
.       prtpage prfile;*pcolumn:row,*font=font12,*boldon,*ULON,"Mailer",*ULOFF,*boldoff;
.       goto Fiscal
        If (Mode = C2)
                goto Calendar
        Endif

Fiscal1
.Begin patch 5.6
          setprop   sheet.Range("C7"),*Value=FY1
          setprop   sheet.Range("D7"),*Value=FY2
          setprop   sheet.Range("E7"),*Value=FY3
          setprop   sheet.Range("F7"),*Value=FY4
          setprop   sheet.Range("G7"),*Value=FY5
          setprop   sheet.Range("H7"),*Value=FY6
          setprop   sheet.Range("I7"),*Value="Total"
          setprop   sheet.range("C7:I7").Font,*Bold="True"                               
          move      C8,HowMany2                      .starting row

.       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY1,*uloff,*boldoff;
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY2,*uloff,*boldoff;
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY3,*uloff,*boldoff;
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY4,*uloff,*boldoff;
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY5,*uloff,*boldoff;
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY6,*uloff,*boldoff;
.       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Total",*uloff,*boldoff;
.end patch 5.6
       goto begin

Calendar
.Begin patch 5.6
          setprop   sheet.Range("C7"),*Value=YR1
          setprop   sheet.Range("D7"),*Value=YR2
          setprop   sheet.Range("E7"),*Value=YR3
          setprop   sheet.Range("F7"),*Value=YR4
          setprop   sheet.Range("G7"),*Value=YR5
          setprop   sheet.Range("H7"),*Value="Total"
          setprop   sheet.range("C7:H7").Font,*Bold="True"                               
          move      C8,HowMany2                      .starting row


.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR1,*uloff,*boldoff;
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR2,*uloff,*boldoff;
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR3,*uloff,*boldoff;
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR4,*uloff,*boldoff;
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR5,*uloff,*boldoff;
.       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Total",*uloff,*boldoff;
.end patch 5.6





Begin
       clear Rowcount
       move c1 to RowCount
       if (newlst = c1)            .if lst diff from previous reads skip file read and print new lst
           clear newlst
           goto checkLst
       endif
       loop
       ADD       C1 TO N8

       DISPLAY   *P10:14,*EF,"Records Processed ",N8
.Update
       READ      LISTFLE,SEQ;B1,KEY80,Oyr1,Xyr1,Ryr1,Oyr2,Xyr2,Ryr2,Oyr3,Xyr3,Ryr3:
                   Oyr4,Xyr4,Ryr4,Oyr5,Xyr5,Ryr5,Oyr6,Xyr6,Ryr6
       until over
CheckLst
       UNPACK KEY80,str35,str45
       if (holdlst = "")
                move str35 to holdlst
                goto lsttitl
       endif
       if (holdlst <> str35)
          move c1 to newlst
          goto lastpage
       endif

LSTTITL
       UNPACK KEY80,str35,str45
       move str35 to str40
       call trim using str40
                     pack str55 with "List: ",str40
       call trim using str55
.begin patch 5.6
          move      howmany2,str9
          call      Trim using str9
          pack      str12,"C",str9
          setprop   sheet.Range(str12),*Value=STR55
          setprop   sheet.range(Str12).Font,*Bold="True"                               
.       prtpage prfile;*pTitle1:lstrow,*ALIGNMENT=*CENTER,*font=font12,*boldon,*ll,str55,*boldoff;
.end patch 5.6
.Calendar2
.OrderTotal
       clear MLRTOT
        If (MODE = C1)
.       if (calflg = c0)
.UPdate
                    ADD oyr1 to MlrTot
                    ADD oYR2 to MlrTot
                    ADD oYR3 to MlrTot
                    ADD oYR4 to MlrTot
                    ADD oYR5 to MlrTot
                    ADD oYR6 to MlrTot

                    if (MLRTOT > c0)
                              ADD MLRTOT to TotOrd
                              goto TotOrd
                else
                        goto Row2
                    endif
       else
.UPdate
.Patch 3.99
.Patch 3.97
                    add oYR2 to MlrTot
                    add oYR3 to MlrTot
                    add oYR4 to MlrTot
                    add oYR5 to MlrTot
                    add oYR6 to MlrTot
.end patch 4.1
.Patch 3.97
.Patch 3.99
                    if (MLRTOT > c0)
                              ADD MLRTOT to TotOrd
                else
                        goto Row2
                endif
        Endif
TotOrd
.Total Orders Yearly
        If (MODE = C1)        .fiscal
       ADD oYR1 to OYR01
       ADD oYR2 to OYR02
       ADD oYR3 to OYR03
       ADD oYR4 to OYR04
       ADD oYR5 to OYR05
       ADD oYR6 to OYR06
.exchng
       ADD XYR1 to Xyr01
       ADD XYR2 to Xyr02
       ADD XYR3 to Xyr03
       ADD XYR4 to Xyr04
       ADD XYR5 to Xyr05
       ADD XYR6 to Xyr06
.rent
       ADD RYR1 to RYR01
       ADD RYR2 to RYR02
       ADD RYR3 to RYR03
       ADD RYR4 to RYR04
       ADD RYR5 to RYR05
       ADD RYR6 to RYR06

       else                             .calendar

       ADD oYR2 to OYR02
       ADD oYR3 to OYR03
       ADD oYR4 to OYR04
       ADD oYR5 to OYR05
       ADD oYR6 to OYR06
.Exchg
       ADD XYR2 to Xyr02
       ADD XYR3 to Xyr03
       ADD XYR4 to Xyr04
       ADD XYR5 to Xyr05
       ADD XYR6 to XYR06
.Rent
       ADD RYR2 to RYR02
       ADD RYR3 to RYR03
       ADD RYR4 to RYR04
       ADD RYR5 to RYR05
       ADD RYR6 to RYR06
       endif


Print
       add     eightlpi,row
       add     eightlpi,row
       UNPACK KEY80,str35,str45
.begin patch 5.6
          add       c1,howmany
          move      howmany2,str9
          call      Trim using str9
          pack      str12,"A",str9
          setprop   sheet.Range(str12),*Value=str45
.       prtpage prfile;*pcolumn:row,*font=font7,Str45;
.end patch 5.6

   If (MODE = C1)       .fiscal
          move mask15 to dim15a
          edit OYR1 to dim15a
          call trim using     dim15a
.begin patch 5.6
          add       c1,howmany
          move      howmany2,str9
          call      Trim using str9
          pack      str12,"C",str9
          setprop   sheet.Range(str12),*Value=Dim15a
.          prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;          
       

          move mask15 to dim15a
          edit OYR2 to dim15a
          call trim using     dim15a
          pack      str12,"D",str9
          setprop   sheet.Range(str12),*Value=Dim15a
.          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;          

          move mask15 to dim15a
          edit OYR3 to dim15a
          call trim using     dim15a
          pack      str12,"E",str9
          setprop   sheet.Range(str12),*Value=Dim15a
.          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;          

          move mask15 to dim15a
          edit OYR4 to dim15a
          call trim using     dim15a
          pack      str12,"F",str9
          setprop   sheet.Range(str12),*Value=Dim15a
.          prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;          

          move mask15 to dim15a
          edit OYR5 to dim15a
          call trim using     dim15a
          pack      str12,"G",str9
          setprop   sheet.Range(str12),*Value=Dim15a
.          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;          
          
          move mask15 to dim15a
          edit OYR6 to dim15a
          call trim using     dim15a
          pack      str12,"H",str9
          setprop   sheet.Range(str12),*Value=Dim15a
.          prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;          

          move mask15 to dim15a
          edit MLRTOT to dim15a
          call trim using     dim15a
          pack      str12,"I",str9
          setprop   sheet.Range(str12),*Value=Dim15a
.          prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;          
          
          Else                   .calendar
          move mask15 to dim15a
          edit OYR2 to dim15a
          call trim using     dim15a
          add       c1,howmany
          move      howmany2,str9
          call      Trim using str9
          pack      str12,"C",str9
          setprop   sheet.Range(str12),*Value=Dim15a
.          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;          

          move mask15 to dim15a
          edit OYR3 to dim15a
          call trim using     dim15a
          pack      str12,"D",str9
          setprop   sheet.Range(str12),*Value=Dim15a
.          prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;          

          move mask15 to dim15a
          edit OYR4 to dim15a
          call trim using     dim15a
          pack      str12,"E",str9
          setprop   sheet.Range(str12),*Value=Dim15a
.          prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;          

          move mask15 to dim15a
          edit OYR5 to dim15a
          call trim using     dim15a
          pack      str12,"F",str9
          setprop   sheet.Range(str12),*Value=Dim15a
.          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;          
          
          move mask15 to dim15a
          edit OYR6 to dim15a
          call trim using     dim15a
          pack      str12,"G",str9
          setprop   sheet.Range(str12),*Value=Dim15a
.          prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;          
          
          move mask15 to dim15a
          edit MlrTot to dim15a
          call trim using     dim15a
          pack      str12,"H",str9
          setprop   sheet.Range(str12),*Value=Dim15a
.          prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;          
.End patch 5.6          
          Endif        
Row
          add c1 to RowCount


Row2
       if (ROWCOUNT = "23")
           add     eightlpi,row
           add     eightlpi,row
          move "7750",row
.Begin patch 5.6          
.          prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
.          prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
.          prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
.                              "©","1991-2012, Names in the News";
.
.           goto Page
.End patch 5.6          
       endif 

.Clear Fields

       clear    OYR1
       clear    OYR2
       clear    OYR3
       clear    OYR4
       clear    OYR5
       clear    OYR6

       clear    XYR1
       clear    XYR2
       clear    XYR3
       clear    XYR4
       clear    XYR5
       clear    XYR6

       clear    RYR1
       clear    RYR2
       clear    RYR3
       clear    RYR4
       clear    RYR5
       clear    RYR6

        repeat
        
LastPage
       if (ROWCOUNT < "20")

       goto totals

        else


       move "7750",row
.Begin patch 5.6          
.       prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
.       prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
.       prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
.                              "©","1991-2012, Names in the News";
                        move c1 to newpg
                        add c1 to Pgcnt
.                    prtpage prfile;*NEWPAGE:
.                           *UNITS=*HIENGLISH:
.                                     *ORIENT=*LANDSCAPE;
          pack      Str55 from "©","1991-2012, Names in the News"
          add       c3,howmany
          move      howmany2,str9
          call      Trim using str9
          pack      str12,"A",str9
          setprop   sheet.Range(str12),*Value=Str55

.End patch 5.6          

          goto Totals
       endif

Totals
       if (newpg = c1)
.Begin patch 5.6          
                  clear   row
          move    "300",row
.                  prtpage prfile;*pcolumn:row,*font=font12,"Confidential";
.          prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News";
.                  prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date:";
.          clock timestamp,str8
.                  unpack str8,str2,yy,mm,dd
.          clear str10
.                  pack  str10,mm,slash,dd,slash,str2,yy
.          prtpage prfile;*pTitle3:row,*font=font12,str10;
.                  add     eightlpi,row
.                  add     eightlpi,row
.                  prtpage prfile;*pTitle4:row,*ALIGNMENT=*Center,*font=font12,*ULON,"Names Mailed on",*ULOFF;
.                  add     eightlpi,row
.                  add     eightlpi,row
.                move holdlst to str40
.                call trim using str40
.                                                   pack str55 with "List: ",str40
.                call trim using str55
.                prtpage prfile;*pTitle1:row,*ALIGNMENT=*Center,*font=font12,*boldon,str55,*boldoff;
.                    move "1380",row
       else
.End patch 5.6          
                goto    TOTAL1
       endif
       if (MODE = c1)

TotFiscal
.Begin patch 5.6          
          add       c2,howmany
          move      howmany2,str9
          call      Trim using str9
          pack      str12,"C",str9
          setprop   sheet.Range(str12),*Value=FY1
          pack      str12,"D",str9
          setprop   sheet.Range(str12),*Value=FY2
          pack      str12,"E",str9
          setprop   sheet.Range(str12),*Value=FY3
          pack      str12,"F",str9
          setprop   sheet.Range(str12),*Value=FY4
          pack      str12,"G",str9
          setprop   sheet.Range(str12),*Value=FY5
          pack      str12,"H",str9
          setprop   sheet.Range(str12),*Value=FY6
          pack      str12,"I",str9
          setprop   sheet.Range(str12),*Value="Total"

.       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY1,*uloff,*boldoff;
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY2,*uloff,*boldoff;
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY3,*uloff,*boldoff;
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY4,*uloff,*boldoff;
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY5,*uloff,*boldoff;
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY6,*uloff,*boldoff;
.       prtpage prfile;*pcolumn7:row,*font=font12,*boldon,*ulon,"Total",*uloff,*boldoff;
.End patch 5.6          

          Else

TotCalendar
.Begin patch 5.6          
          add       c3,howmany
          move      howmany2,str9
          call      Trim using str9
          pack      str12,"D",str9
          setprop   sheet.Range(str12),*Value=YR1
          pack      str12,"E",str9
          setprop   sheet.Range(str12),*Value=YR2
          pack      str12,"F",str9
          setprop   sheet.Range(str12),*Value=YR3
          pack      str12,"G",str9
          setprop   sheet.Range(str12),*Value=YR4
          pack      str12,"H",str9
          setprop   sheet.Range(str12),*Value=YR5
          pack      str12,"I",str9
          setprop   sheet.Range(str12),*Value="Total"
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR1,*uloff,*boldoff;
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR2,*uloff,*boldoff;
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR3,*uloff,*boldoff;
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR4,*uloff,*boldoff;
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR5,*uloff,*boldoff;
.       prtpage prfile;*pcolumn7:row,*font=font12,*boldon,*ulon,"Total",*uloff,*boldoff;
.End patch 5.6          
          
          endif

Total1
       if (newpg = c1)
                 add     eightlpi,row
                 add     eightlpi,row
                 add     eightlpi,row
                 add     eightlpi,row
       else
                 add     eightlpi,row
                 add     eightlpi,row
       endif
.Begin patch 5.6          
          add       c2,howmany
          move      howmany2,str9
          call      Trim using str9
          pack      str12,"A",str9
          setprop   sheet.Range(str12),*Value="Grand Totals"
.       prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ULON,"Grand Totals",*ULOFF,*boldoff;
.End patch 5.6          

        If (MODE = C1)              .fiscal

        move mask15 to dim15a
        edit OYR01 to dim15a
          call trim using     dim15a
.Begin patch 5.6          
          pack      str12,"C",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit OYR02 to dim15a
          call trim using     dim15a
          pack      str12,"D",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit OYR03 to dim15a
          call trim using     dim15a
           pack      str12,"E",str9
          setprop   sheet.Range(str12),*Value=dim15a
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit OYR04 to dim15a
          call trim using     dim15a
          pack      str12,"F",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit OYR05 to dim15a
          call trim using     dim15a
          pack      str12,"G",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit OYR06 to dim15a
          call trim using     dim15a
          pack      str12,"H",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

          Else

        move mask15 to dim15a
        edit OYR02 to dim15a
          call trim using     dim15a
          pack      str12,"D",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  
        move mask15 to dim15a
        edit OYR03 to dim15a
          call trim using     dim15a
          pack      str12,"E",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  
        move mask15 to dim15a
        edit OYR04 to dim15a
          call trim using     dim15a
          pack      str12,"F",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  
        move mask15 to dim15a
        edit OYR05 to dim15a
          call trim using     dim15a
          pack      str12,"G",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  
        move mask15 to dim15a
        edit OYR06 to dim15a
          call trim using     dim15a
          pack      str12,"H",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

          Endif

Next
        If (MODE = C1)               .fiscal
.    If (CALFLG = c0)
       add OYR01 to OTOT
       add OYR02 to OTOT
       add OYR03 to OTOT
       add OYR04 to OTOT
       add OYR05 to OTOT
       add OYR06 to OTOT
       goto TOT

          Else                           .calendar

       add OYR02 to OTOT
       add OYR03 to OTOT
       add OYR04 to OTOT
       add OYR05 to OTOT
       add OYR06 to OTOT
       
       endif
TOT
        move mask15 to dim15a
        edit OTOT to dim15a
          call trim using     dim15a
          pack      str12,"I",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  
       add     eightlpi,row
       add     eightlpi,row
          add       c2,howmany
          move      howmany2,str9
          call      Trim using str9
          pack      str12,"A",str9
          setprop   sheet.Range(str12),*Value="Exchange Totals"
.       prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ULON,"Exchange Totals",*ULOFF,*boldoff;

        If (MODE = C1)                    .fiscal

        move mask15 to dim15a
        edit Xyr01 to dim15a
          call trim using     dim15a
           pack      str12,"C",str9
          setprop   sheet.Range(str12),*Value=dim15a
.       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit Xyr02 to dim15a
          call trim using     dim15a
            pack      str12,"D",str9
          setprop   sheet.Range(str12),*Value=dim15a
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit Xyr03 to dim15a
          call trim using     dim15a
           pack      str12,"E",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit Xyr04 to dim15a
          call trim using     dim15a
           pack      str12,"F",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit Xyr05 to dim15a
          call trim using     dim15a
           pack      str12,"G",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit Xyr06 to dim15a
          call trim using     dim15a
           pack      str12,"H",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  
          goto Xyear

          Else            .calendar

        move mask15 to dim15a
        edit Xyr02 to dim15a
          call trim using     dim15a
           pack      str12,"D",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit Xyr03 to dim15a
          call trim using     dim15a
           pack      str12,"E",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit Xyr04 to dim15a
          call trim using     dim15a
           pack      str12,"F",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit Xyr05 to dim15a
          call trim using     dim15a
           pack      str12,"G",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit XYR06 to dim15a
          call trim using     dim15a
           pack      str12,"H",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

          Endif
Xyear
        If (MODE = C1)
.       If (CALFLG = c0)
       clear XTOT
       add Xyr01 to XTOT
       add Xyr02 to XTOT
       add Xyr03 to XTOT
       add Xyr04 to XTOT
       add Xyr05 to XTOT
       add Xyr06 to XTOT
       goto RYear

       Endif
       clear XTOT
       add XYR02 to XTOT
       add Xyr03 to XTOT
       add Xyr04 to XTOT
       add Xyr05 to XTOT
       add Xyr06 to XTOT
RYEAR
        move mask15 to dim15a
        edit XTOT to dim15a
          call trim using     dim15a
           pack      str12,"I",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  
       add     eightlpi,row
       add     eightlpi,row
          add       c2,howmany
          move      howmany2,str9
          call      Trim using str9
          pack      str12,"A",str9
          setprop   sheet.Range(str12),*Value="Rental Totals"
.       prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ULON,"Rental Totals",*ULOFF,*boldoff;
        If (MODE = C1)
        move mask15 to dim15a
        edit RYR01 to dim15a
          call trim using     dim15a
          pack      str12,"C",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit RYR02 to dim15a
          call trim using     dim15a
           pack      str12,"D",str9
          setprop   sheet.Range(str12),*Value=dim15a
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit RYR03 to dim15a
          call trim using     dim15a
          pack      str12,"E",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit RYR04 to dim15a
          call trim using     dim15a
          pack      str12,"F",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit RYR05 to dim15a
          call trim using     dim15a
          pack      str12,"G",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit RYR06 to dim15a
          call trim using     dim15a
          pack      str12,"H",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  
        goto Rent2
       ENDIF
        move mask15 to dim15a
        edit RYR02 to dim15a
          call trim using     dim15a
          pack      str12,"D",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit RYR03 to dim15a
          call trim using     dim15a
          pack      str12,"E",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  
 
        move mask15 to dim15a
        edit RYR04 to dim15a
          call trim using     dim15a
          pack      str12,"F",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit RYR05 to dim15a
          call trim using     dim15a
          pack      str12,"G",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  

        move mask15 to dim15a
        edit RYR06 to dim15a
          call trim using     dim15a
          pack      str12,"H",str9
          setprop   sheet.Range(str12),*Value=dim15a
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;  
Rent2
        If (MODE = C1)
.       If (CALFLG = c0)

       add RYR01 to RTOT
       add RYR02 to RTOT
       add RYR03 to RTOT
       add RYR04 to RTOT
       add RYR05 to RTOT
       add RYR06 to RTOT
       goto LstPg
       Endif
       add RYR02 to RTOT
       add RYR03 to RTOT
       add RYR04 to RTOT
       add RYR05 to RTOT
       add RYR06 to RTOT
LstPg
      if (RTOT > c0)
       clear str10
       move RTOT to Str10
       clear str13
       call FormatNumeric using str10,str13,comma
          pack      str12,"I",str9
          setprop   sheet.Range(str12),*Value=str13
.
.       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,str13,*ULOFF,*boldoff;
      else
          pack      str12,"I",str9
          setprop   sheet.Range(str12),*Value=RTOT
.       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,RTOT,*ULOFF,*boldoff;
      endif
.Patch 1.1
       move "7750",row
          add       c2,howmany
          move      howmany2,str9
          call      Trim using str9
          pack      str55 from "©","1991-2012, Names in the News"
          pack      str12,"A",str9
          setprop   sheet.Range(str12),*Value=STR55
.       prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
.       prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
.       prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
.                              "©","1991-2012, Names in the News";

End1
       Display    *P10:11,*EF,*cyan,"Printing Finished!!"
.       PRTCLOSE prfile
                    clock timestamp,timestamp

                              clear   taskname
                              if        (#ver = c1)
                              Pack      Taskname from "c:\work\",splfle,timestamp,".xlsx"
                              else
                              Pack      Taskname from "c:\work\",splfle,timestamp,".xls"
                              endif
                              erase     taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
.                              trap    TrapExcelObject if Object
                              book.saveas giving N9 using *Filename=taskname
                              trapclr Object
.
                              setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.Clean up after myself
                              destroy   OTRUE
.I was getting Excel.exe errors before I included following line.
.All automation objects need to be destroyed before you close down spreadsheet!
                              destroy sheet
                              destroy sheets
                              destroy book
                              destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
                              setprop ex,*DisplayAlerts=OFALSE
                              destroy   OFALSE
                              setprop ex,*SheetsInNewWorkbook=SheetsDefault
                              ex.quit
                              destroy ex
.Email new XLS to User
                              move    "Here is your Summary in Excel",MailSubjct
                              pack      MailBOdy,"Input File:  ",INPNAME
                              pack      Mailto from Luser,"@nincal.com"
                              pack      MailFrom from Luser,"@nincal.com"
                              Pack      MailAttach from taskname
                              Move      c0,TrapCount                   .reset
.end patch 5.6
       Pause c1
CheckFile

                              trap      WaitForEnd giving error if IO
                              open      FileCheck,MailAttach,Exclusive          
                              Close     FIleCHeck
                             
                              call      SendMail

        clear n3
        if (copy = c0)
                goto End2
        Endif
.======================================================================
.Patch3.5
.........................................................
.test formatnumeric OS

        if (bosflag = c2)
                goto looper2
        endif
...........................................................
.==============================================================
.OS TEST
Looper1
.patch3.yr6
        if (pdfopt = YES)
                  goto pdfthis
        endif
.patch3.yr6
        loop
          until (N3 = COPY)
.begin patch 5.6
.                if (cntprint = "1" | cntprint = "3")      .Laser6
.                              PRTPLAY "c:\work\listsum.lst","Laser6"
.                Elseif     (cntprint = "5" )      .Susan
.                              PRTPLAY "c:\work\listsum.lst","NIN0010 KYOCERAS"
.               Elseif (cntprint = "7")      .
.                              PRTPLAY splfle,"Kyocera FS-C5030N (KX) on NIN0100"
.
.                else
.                              PRTPLAY splfle,"\\NINs2\Laser3 Blankstock"
.                endif
                add c1 to N3
.end patch 5.6
        repeat
.       goto End2
        goto Checknew
.================================================================
Looper2
.patch3.yr6
        if (pdfopt = YES)
                  goto pdfthis
        endif
.patch3.yr6
        loop
          until (N3 = COPY)
.begin patch 5.6
.                if (cntprint = "1" | cntprint = "3")      .Laser6
.                              PRTPLAY splfle,"\\NINs2\Laser6"
.                Elseif     (cntprint = "5" )      .Susan
.                              PRTPLAY "c:\work\listsum.lst","NIN0010 KYOCERAS"
.               Elseif (cntprint = "7")      .
.                              PRTPLAY splfle,"Kyocera FS-C5030N (KX) on NIN0100"
.                else
.                              PRTPLAY splfle,"\\NINs2\Laser3 Blankstock"
.                endif
.enf patch 5.6
                add c1 to N3
        repeat
checknew
       if (newlst = c1)
                if (pdfopt <> YES)
                     erase splfle
                endif
..               erase "c:\work\mlrsum.lst"
                Pause c1
                move str35 to holdlst
                add c1 to n4
                move n4 to str4
                call trim using str4
                goto rerun
       endif

End2
        trapclr Spool
        noreturn
        Display    *P10:11,*EF,*cyan,"Printing Finished!!"
        Pause c1
        Display    *P10:11,*EF,*cyan,"Good Bye!!!!!!!!!!!"
        Pause c1
        if (pdfopt <> YES)
                     erase splfle
         endif
        shutdown  "cls"
          stop

Spool1
         Display    *P10:11,*EF,*cyan,error
.3.yr6 : added for plbserver runs does not like "\\NINs2\Laser?"         gets s10 if s10 try this
         Display    *P10:11,*EF,*cyan,"Trying Again"
         Pause c2
         TRAPCLR   SPOOL
         Trap SPOOL2 giving error if SPOOL
         SCAN      "S10" IN ERROR
         if equal
.begin patch 5.6
.                if (cntprint = "1" | cntprint = "3")      .Laser6
.                    PRTOPEN prfile,"Laser6",spoolname,noprint,spoolfile=splfle
.                Elseif (CntPrint = "5") .susan    
.                    PRTOPEN prfile,"KYOCERAS",spoolname,noprint,spoolfile=splfle
.                Elseif    (cntprint = "7")      .
.                    PRTOPEN prfile,"Kyocera FS-C5030N (KX) on NIN0100",spoolname,noprint,spoolfile=splfle
.                else
.                    PRTOPEN prfile,"Laser3",spoolname,noprint,spoolfile=splfle
.                endif
         endif
.end patch 5.6
         return
SPOOL2
         Display    *P10:11,*EF,*cyan,error
.3.yr6

         PAUSE c2
         CLOSE     INFILE
         CLOSE     OUTPUT
         NORETURN
         shutdown  "cls"
         STOP
.================================================
.patch3.yr6
pdfthis
.Patch 3.99
          call      GetPDfPAth
          pack      str45 from PDFPATH,"\flag.dat"
          pack      APIFileName,STR45,hexzero
          loop
                    call      FindFirstFile
                    until (APIResult = 0 | APIResult = hexeight)
                    pause     "1"
          repeat
          pause     "2"
          move    "Here is your PDF File",MailSubjct
          Clear     MailBody
          append    "lstsum.pdf",Mailbody
          append    CRLF,Mailbody
          reset     Mailbody
          pack      Mailto from Luser,"@nincal.com"
          pack      MailFrom from Luser,"@nincal.com"
          pack      MailAttach from "c:\work\pdf\lstsum.pdf"
          call      SendMail
.Clean up afterwards
          pause c7
          erase     "c:\work\pdf\lstsum.pdf"
.                             endif
         goto checknew
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Call Report - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Pack      MailTO from User,"@nincal.com"
                    Move      "dherric@nincal.com",MailCC
                    append    CRLF,MailBOdy
                    append    mailattach,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    endif
          
                    goto      checkfile
IOTrap
          move      C1,N1
          return

        include   ncntio.inc
          include   compio.inc
          include   cntio.inc
         INCLUDE   NDATIO.inc
        include comlogic.inc

