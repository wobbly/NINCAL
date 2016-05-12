*******************************************************************************
*PURPOSE -   OUTPUT FLAT FILE FOR USE WITH dBASE III+ AND R&R (MLRSUM)
*            TO    PRODUCE MAILER USAGE SUMMARY REPORT. SEE NOTEBOOK.
*
*INPUT   -   SEQ, ORDER RECORDS (diskin pulled as requested).
*******************************************************************************
PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NDATDD.inc
.patch3.96
                                        include   compdd.inc
                                        include   cntdd.inc
.         INC       NMLRDD.inc
.patch3.96
         include   ncntdd.inc
release  init      "3.97"        DLH    29July2004          Copywrite footer
;release  init      "3.96"        DMB   26MAY2004 Mailer Conversion
.Release   INIT       "3.95"          10/27/03 Added New Year
;Release   INIT       "3.94"         04/28/03 Added option for the order/maildate selection to be made by nord006b
;Release   INIT       "3.93"         03/20/03 Added pdf option and clean up code to allow plbserv to do prtopen (does not like \\nts0\laser? just doing laser?
;Release   INIT       "3.92"          DMB 08/06/02 New Fiscal\Calendar Year
;Release   INIT       "3.91"          DMB 07/23/02 added code for XP in osflag
.Release   INIT       "3.9"          DB 05/02/02 Changed to names mailed by - headers centered a little more
.Release   INIT       "3.8"          .DB 12DEC2001 Now prints with multiple mailers selected
.Release   INIT       "3.7"         Fixed bug which did not print footer or pg# on pg previous to total page
.Release   INIT       "3.6"        Added code to print on different OS
.Release   INIT       "3.4"         Added trapping function for spool error
.                                  Added Code for OS and where to print
.Release   INIT       "3.3"         .DB 24Aug2001 Added var to dsinit to carry over prin area
.Release   INIT       "3.2"         .DB 21Aug2001  modification of multiple copy
.Release   INIT      "3.1"         .DB 21Aug2001  multiple copy capabilities added
.Release  init       "3.0"           DB  21Aug01 New Mlrsum Print pg Added
.release  init       "2.9"          JD  24jan00 added fiscal 01
.release  init       "2.8"          JD  24jan00 added fiscal 00
.Release  init       "2.7"          JD20apr99 fixed date mode for fiscal.
.Release  init       "2.6b"          JD  29JAN99 skip pending orders.
.RELEASE  INIT       "2.6"         ASH 11JAN99 NINORD Y2K, File expansion
.release  init       "2.5"         ASH 21Sep98 NINMLR Y2K File expansion
.release  init       "2.4"          JD  02oct98 added fiscal 99.
.release  init       "2.3"         JD  04feb98 add branch for fiscal.
.release  init       "2.2"         JD  07nov97 add fiscal 98
.RELEASE  INIT      "2.1"         DLH0  20sep96 added fiscal 97.
.RELEASE  INIT      "2.0"         JD   16jan96 fixed write.
.RELEASE  INIT      "1.9"         JD   26SEP95 ADDED CHECK FOR ZERO \DATE.
.RELEASE  INIT      "1.8"        DLH  21Sep95 order date option.
.RELEASE  INIT      "1.7"        jd   26jan95  add fiscal 96.
.RELEASE  INIT      "1.6"        jd   16may94 added rent/exch totals year.
.RELEASE  INIT      "1.5"        jd  11may94 add fiscal 95.
.RELEASE  INIT      "1.4"       DLH 12jan94 fix fiscal 94.
.RELEASE  INIT      "1.3"       DLH 09NOV93 USE DSINIT, CHAIN FROM UTIL.
.
.RELEASE  INIT     "1.2"       JD 270193 - ADDED FISCAL YEAR BREAK OUT
.RELEASE  INIT     "1.1"       DLH 06MAR92 - ADDED LIST UNIVERSE
.RELEASE  INIT      "1.0"       DLH 24FEB92
INPUT    DIM       30
.Start Patch #2.6 - INCREASED VAR
.ADDED LOTSA BYTES TO FILE.  UNSURE OF FILE SIZE OF 328?????
.BUMPED UP TO "DISKIN" FILESIZE
.INFILE   FILE      VAR=328
INFILE   FILE      VAR=498
.END Patch #2.6 - INCREASED VAR
.Start Patch #2.5 - remmed and replaced line due to file expansion
;UPdate
OUTPUT   IFILE     KEYLEN=80,FIX=591     .KEY= MAILER NAME + LIST NAME
.OUTPUT   IFILE     KEYLEN=80,FIX=561     .KEY= MAILER NAME + LIST NAME
.OUTPUT   IFILE     KEYLEN=80,FIX=561     .KEY= MAILER NAME + LIST NAME
.OUTPUT   IFILE     KEYLEN=80,FIX=501     .KEY= MAILER NAME + LIST NAME
.OUTPUT   IFILE     KEYLEN=60,FIX=421     .KEY= MAILER NAME + LIST NAME
.End Patch #2.5 - remmed and replaced line due to file expansion
.OUTPUT   IFILE     KEYLEN=60,FIX=331     .KEY= MAILER NAME + LIST NAME
.
HOLDMLR  DIM       7
..............
.OUTPUT FILE.
.Start Patch #2.5 - remmed and replaced line due to file expansion
.KEY60    DIM       60         KEY VAR.
KEY80    DIM       80         KEY VAR.
.End Patch #2.5 - remmed and replaced line due to file expansion
.B1       DIM        1        1-1
OMLR     DIM       45         2-26
OLIST    DIM       35        27-61
;patch to pass year
c16      FORM      "16"
c17      FORM      "17"
;UPdate
O88      FORM      10        62-71      88 NAMES (MD)
X88      FORM      10        72-81      88 NAMES (MD)
R88      FORM      10        81-91      88 NAMES (MD)
O89      FORM      10        91-101     89  NAMES (MD)
X89      FORM      10       101-111     89 NAMES (MD)
R89      FORM      10       111-121     89 NAMES (MD)
O90      FORM      10       121-131     90 NAMES (Md)
X90      FORM      10       131-141     90 NAMES (MD)
R90      FORM      10       141-151     90 NAMES (MD)
O91      FORM      10       151-161     91 NAMES (Md)
X91      FORM      10       101-171     91 NAMES (MD)
R91      FORM      10       111-181     91 NAMES (MD)
O92      FORM      10       121-191     92 NAMES (Md)
X92      FORM      10       101-211     92 NAMES (MD)
R92      FORM      10       111-221     92 NAMES (MD)
O93      FORM      10       121-231     93 NAMES (Md)
X93      FORM      10       101-241     93 NAMES (MD)
R93      FORM      10       111-251     93 NAMES (MD)
O94      FORM      10       121-261     94 NAMES (Md)
X94      FORM      10       101-271     94 NAMES (MD)
R94      FORM      10       111-281     94 NAMES (MD)
O95      FORM      10       132-291     95 NAMES (MD)
X95      FORM      10       101-301     95 NAMES (MD)
R95      FORM      10       111-311     95 NAMES (MD)
O96      FORM      10       132-321     96 NAMES (MD)
X96      FORM      10       101-331     96 NAMES (MD)
R96      FORM      10       111-341     96 NAMES (MD)
O97      FORM      10       132-351     96 NAMES (MD)
X97      FORM      10       101-361     96 NAMES (MD)
R97      FORM      10       111-371     96 NAMES (MD)
O98      FORM      10
X98      FORM      10
R98      FORM      10
O99      FORM      10
X99      FORM      10
R99      FORM      10
O00      FORM      10                     (Md)
X00      FORM      10                     (MD)
R00      FORM      10                     (MD)
O01      FORM      10                     (Md)
X01      FORM      10                     (MD)
R01      FORM      10                     (MD)
O02      FORM      10                     (MD)
X02      FORM      10                     (MD)
R02      FORM      10                     (MD)
O03      FORM      10                     (MD)
X03      FORM      10                     (MD)
R03      FORM      10                     (MD)
O04      FORM      10                     (MD)
X04      FORM      10                     (MD)
R04      FORM      10                     (MD)
.UNIVERSE FORM      9       142-150      LIST UNIVERSE
.
.Start Patch #2.6 - replaced var
.QUANT    FORM      7
.Xquant   form      7
.rquant   form      7
.TORDQTYR FORM      7
QUANT    FORM      9
Xquant   form      9
rquant   form      9
TORDQTYR FORM      9
.End Patch #2.6 - replaced var
BRANCH   FORM      2
START     FORM     "00"
FISCMO   DIM       2
ORDMO    FORM      2
MODE     FORM      1
count    form      5
dteflag form      1                    1=maildate 2=orderdate
;UPdate
yr88     init      "88"
yr89     init      "89"
yr90     init      "90"
yr91     init      "91"
yr92     init      "92"
yr93     init      "93"
yr94     init      "94"
yr95     init      "95"
yr96     init      "96"
yr97     init      "97"
yr98     init      "98"
yr99     init      "99"
yr00     init      "00"
yr01     init      "01"
yr02     init      "02"
yr03     init      "03"
yr04     init      "04"
datechk  dim       2
dateyy   dim       2
c12      form      "12"
FUNCBR   FORM      "0"       PROGRAM CONTROL BRANCH.
c14      form      "14"
c15      form      "15"
.==============================================================================
ListFle  file
prfile   pfile
;patch3.91
bosflag   form   1          1=win 95,98, 2=NT
;patch3.91
Title1   form    9
Title2   form    9
Title3   form    9
Title4   form    9
mlrrow   form    9
holdNmlr dim    45
.========================================================================================
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
.===========================================================================
.FLAGS
.Fiscal/Calendar Flg
Newpg    form    1
.Flag to see if a new report needs to be started for new mlr
newmlr   form    1
RowCount form    9
PgCnt    form    9
copy     form    4

splfle    dim     45
spoolname DIM     30
spldir     init    "c:\work\mlrsum"
mlrnum   dim     3
.==========================================================================
.Fiscal Headers
;UPdate
FY1 init "FY 98/99"
FY2 init "FY 99/00"
FY3 init "FY 00/01"
FY4 init "FY 01/02"
FY5 init "FY 02/03"
FY6 init "FY 03/04"
.Calendar Headers
;UPdate
YR1 init "2000"
YR2 init "2001"
YR3 init "2002"
YR4 init "2003"
YR5 init "2004"
.================================================================================
.KEY80    DIM       80         KEY VAR.
LstTot   form      10         Mailer Total Order
TotOrd   form      10         TOTAL Order
OYR95     form      10         95 Order Grand Total
XYR95     form      10         95 Xchange Grand Total
RYR95     form      10         95Rental  Grand Total
.=====================================================
OYR96     form      10         96 Order Grand Total
XYR96     form      10         96 Xchange Grand Total
RYR96     form      10         96Rental  Grand Total
.=====================================================
OYR97     form      10         97 Order Grand Total
XYR97     form      10         97 Xchange Grand Total
RYR97     form      10         97Rental  Grand Total
.=====================================================
OYR98     form      10         98 Order Grand Total
XYR98     form      10         98 Xchange Grand Total
RYR98     form      10         98Rental  Grand Total
.=====================================================
OYR99     form      10         99 Order Grand Total
XYR99     form      10         99Xchange Grand Total
RYR99     form      10         99Rental  Grand Total
.=====================================================
OYR00     form      10         00 Order Grand Total
XYR00     form      10         00Xchange Grand Total
RYR00     form      10         00Rental  Grand Total
.=====================================================
OYR01     form      10         01 Order Grand Total
XYR01     form      10         01 Xchange Grand Total
RYR01     form      10         01 Rental  Grand Total
.=====================================================
OYR02     form      10         02 Order Grand Total
XYR02     form      10         02Xchange Grand Total
RYR02     form      10         02Rental  Grand Total
.=====================================================
OTOT     form      10         ORDER   GRAND Total
XTOT     form      10         Xchange Grand Total
RTOT     form      10         Rental  Grand Total
.=============================================================================
.Sort Parameters=======================================================
INDAT    init  "MLRSUM.DAT"   .File to be sorted
OUTSRT   init  "MLRSUM.SRT"   .Partial Name of sorted Output file for ncsh
MLRSRT   init  "2-46,47-81"   .Sort
SRTDIR   init  "c:\"
SORTFLE  dim    100                          .Var to pack file names of sort

;patch3.93
PDFOPT   DIM        1
FMONTH   FORM       2
LUSER    DIM        10
;patch3.93
.==========================================================================
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nts0\c\netutils\NIN logo black outline.jpg"
          CALL      PAGE
          PRTCLOSE PRFILE
          SHUTDOWN

         move      c3 to ncntpath
;patch3.93
         move levels to PDFOPT
         move prio to FMONTH
         move user to LUSER
;patch3.93
         move      inits to copy       .CHAINED FROM DSINIT WITH INFO-copies
         MOVE      FUNC TO FUNCBR      .CHAINED FROM DSINIT WITH INFO-mode
         CMATCH    B1 TO PROGRAM
         IF        EOS                 .NO
                   move      c1 to dteflag
                   MOVE      "NORD0027" TO PROGRAM
                   MOVE      "Names In The News Ca Inc." TO COMPNME
                   KEYIN     *P10:12,*EL,"INPUT FILE NAME : ",INPUT
         endif
         MOVE      "MAILER SUMMARY PREPARATION" TO STITLE
         CALL      PAINT
         MOVE      "EXIT" TO PF5
         CALL      FUNCDISP
         KEYIN     *CL
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
         OPEN      OUTPUT,"MLRSUMMARY:PRINT",EXCLUSIVE
         XIF
         IFZ       PC
         OPEN      OUTPUT,"C:\MLRSUM",EXCLUSIVE
         XIF
         TRAPCLR   IO
         move      "P" to str1
         KEYIN     *P10:14,"OUTFILE EXISTS (A)dd, (P)repare new- ":
                   "DEFAULT ? ",*T1,STR1
         CMATCH    "A" TO STR1
         GOTO      INPUT IF EQUAL
         CLOSE     OUTPUT
NOOUT    TRAPCLR   IO
         IFNZ      PC
         PREPARE   OUTPUT,"MLRSUMMARY:PRINT"
         XIF
         IFZ       PC
.Start Patch #2.5 - file expanded
;Update
          PREPARE   OUTPUT,"C:\MLRSUM","C:\mlrsum","80","591"
.          PREPARE   OUTPUT,"C:\MLRSUM","C:\mlrsum","80","561"
;          PREPARE   OUTPUT,"C:\MLRSUM","C:\mlrsum","80","531"
.End Patch #2.5 - file expanded
         XIF
         NORETURN
CHECK
.         KEYIN     *P10:12,*EF,"MODIFY START FISCAL YEAR ? : ",*T15,STR1
.         CMATCH    YES TO STR1
.for testing......................
.         move     c1 to funcbr
...................................
         compare   funcbr to c1
         IF        EQUAL
         MOVE      C1 TO MODE
;patch3.93
         move      fmonth to fiscmo
         rep       zfill,fiscmo
;patch3.93
         CALL      FISCAL
         ELSE
         MOVE      C2 TO MODE
         ENDIF
CHECK2
;         MOVE      "M" to str1
;         KEYIN     *P10:12,*EF,*cyan,"M",*white,"aildate (O)rder date: ",*T15,*RV,STR1
;         rep       "M1O2m1o2" in STR1
;patch3.94
         MOVE      CURSYS to str1
         if (str1 = "1")
            DISPLAY   *P10:12,"Select Mail Date"
         endif
         if (str1 = "2")
            DISPLAY   *P10:12,"Select Order Date"
         endif
;patch3.94
         move      str1 to dteflag
         branch    dteflag to input,input
         move      c1 to dteflag
INPUT
         CALL      READ
         PACK      MKEY FROM OMLRNUM,OCOBN
         MATCH     HOLDMLR TO MKEY
         IF        NOT EQUAL
         MOVE      MKEY TO HOLDMLR
         CALL      NMLRKEY
         ENDIF
         MATCH     OLNUM TO LSTNUM
         IF        NOT EQUAL
         MOVE      OLNUM TO NDATFLD
         CALL      NDATKEY
         ENDIF
         BRANCH    MODE OF OUTPUT,OUTPUT2
         DISPLAY   *P15:10,"I'M LOST!!!! STOP"
         STOP
READ     READ      INFILE,SEQ;ORDVARS
         if over
         GOTO      Close1 IF OVER

.         GOTO      EOJ IF OVER
         endif
         ADD       C1 TO count
         DISPLAY   *P10:14,"RECORDS PROCESSED ",count,b1,olrn
.begin patch 2.6
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      read IF EQUAL     YES, skip.
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      read IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       LCR order ?
         GOTO      read IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       Cancelled LCR order ?
         GOTO      read IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
.end patch 2.6
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      READ IF EQUAL
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         GOTO      READ IF EQUAL
         RETURN
OUTPUT
.Start Patch #2.5 - remmed and replaced line due to file expansion
.         PACK      KEY60 FROM MCOMP,OLSTNAME
         PACK      KEY80 FROM MCOMP,OLSTNAME
.End Patch #2.5 - remmed and replaced line due to file expansion
         MOVE      C0 TO QUANT
         MOVE      C0 TO RQUANT
         MOVE      C0 TO XQUANT
         MOVE      C0 TO TORDQTYR
         MOVE      OQTY TO QUANT
         CALL      RENTXCHG
         MOVE      C0 TO BRANCH
.         match     "00" to omdtey
.         if        equal
.         DISPLAY   *P16:10,*B,"RECORD SKIPPED MAIL DATE ZERO!!"
.         GOTO      INPUT
.         ENDIF
.Start Patch #2.3 - replaced date
         compare   c2 to dteflag
         if        equal
         move      oodtey to omdtey
         move      oodtem to omdtem
         endif
.End Patch #2.3 - replaced date
         MATCH     "88" TO OMDTEY
         IF        EQUAL
         MOVE       OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C1 TO BRANCH
         ENDIF
         ENDIF
.
         MATCH     "89" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C2 TO BRANCH
          ELSE
         MOVE      C1 TO BRANCH
        ENDIF
        ENDIF
.
        MATCH     "90" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
        COMPARE    START TO ORDMO
        IF         NOT LESS
         MOVE      C3 TO BRANCH
         ELSE
         MOVE      C2 TO BRANCH
         ENDIF
         ENDIF
.
         MATCH     "91" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C4 TO BRANCH
          ELSE
        MOVE     C3 TO BRANCH
         ENDIF
         ENDIF
.
         MATCH     "92" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C5 TO BRANCH
          ELSE
         MOVE      C4 TO BRANCH
         ENDIF
         ENDIF
.
         MATCH     "93" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C6 TO BRANCH
          ELSE
         MOVE      C5 TO BRANCH
         ENDIF
         ENDIF
         MATCH     "94" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C7 TO BRANCH
          ELSE
         MOVE      C6 TO BRANCH
         ENDIF
         ENDIF
         MATCH     "95" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C8 TO BRANCH
          ELSE
         MOVE      C7 TO BRANCH
         ENDIF
         ENDIF

         MATCH     "96" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C9 TO BRANCH
          ELSE
         MOVE      C8 TO BRANCH
         ENDIF
         ENDIF

         MATCH     "97" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C10 TO BRANCH
          ELSE
         MOVE      C9 TO BRANCH
         ENDIF
         ENDIF
.
        MATCH     "98" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C11 TO BRANCH
          ELSE
         MOVE      C10 TO BRANCH
         ENDIF
         ENDIF
.
        MATCH     "99" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C12 TO BRANCH
          ELSE
         MOVE      C11 TO BRANCH
         ENDIF
         ENDIF
.
        MATCH     "00" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C13 TO BRANCH
          ELSE
         MOVE      C12 TO BRANCH
         ENDIF
         ENDIF
.
        MATCH     "01" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C14 TO BRANCH
          ELSE
         MOVE      C13 TO BRANCH
         ENDIF
         ENDIF

        MATCH     "02" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C15 TO BRANCH
          ELSE
         MOVE      C14 TO BRANCH
         ENDIF
         ENDIF
..
;Update
        MATCH     "03" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C16 TO BRANCH
          ELSE
         MOVE      C15 TO BRANCH
         ENDIF
         ENDIF
;Update
        MATCH     "04" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C17 TO BRANCH
          ELSE
         MOVE      C16 TO BRANCH
         ENDIF
         ENDIF

;Update

;Update




          BRANCH    BRANCH OF OK,OK,OK,OK,OK,OK,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok
         GOTO      INPUT
OUTPUT2
.Start Patch #2.5 - remmed and replaced line due to file expansion
.         PACK      KEY60 FROM MCOMP,OLSTNAME
         PACK      KEY80 FROM MCOMP,OLSTNAME
.End Patch #2.5 - remmed and replaced line due to file expansion
         MOVE      C0 TO QUANT
         MOVE      C0 TO BRANCH
         MOVE      C0 TO RQUANT
         MOVE      C0 TO XQUANT
         MOVE      C0 TO TORDQTYR
         MOVE      OQTY TO QUANT
         CALL      RENTXCHG
.......
         compare   c1 to dteflag
         if        equal
         move      omdtey to datechk
         else
         move      oodtey to datechk
         endif
.         match     "00" to DATECHK
.         if        equal
.         COMPARE   C1 TO DTEFLAG
.         IF        EQUAL
.         DISPLAY   *P16:10,*B,*BLINKON,OLRN," SKIPPED MAIL DATE ZERO!!",*BLINKOFF
.         GOTO      INPUT
.         ELSE
.         DISPLAY   *P16:10,*B,*BLINKON,OLRN," SKIPPED ORDER DATE ZERO!!",*BLINKOFF
.         GOTO      INPUT
.         ENDIF
.         ENDIF
         COMPARE   C1 TO DTEFLAG
         IF        EQUAL
         move      omdtem to mm
         move      omdted to dd
         move      omdtey to yy
.         compare   c0 to dateflag
.         if        not equal
.         DISPLAY   *P16:10,*B,*BLINKON,*red,OLRN," SKIPPED MAIL DATE !!",omdtem,slash:
.                   oodted,slash,oodtey,*black,"Beavis",*white:
.                   *BLINKOFF
.         endif
.         GOTO      INPUT
         ELSE
         move      oodtem to mm
         move      oodted to dd
         move      oodtey to yy
.         compare   c0 to dateflag
.         if        not equal
.         DISPLAY   *P16:10,*B,*BLINKON,*red,OLRN," SKIPPED ORDER DATE !!",oodtem,slash:
.                   oodted,slash,oodtey,*black,"Beavis",*white:
.                   *BLINKOFF
.         endif
.         GOTO      INPUT
         ENDIF
.
         move      c1 to branch
;UPdate
dateloop load      dateyy from branch of yr88,yr89,yr90,yr91,yr92,yr93,yr94:
                   yr95,yr96,yr97,yr98,yr99,yr00,yr01,yr02,yr03,yr04
         match     datechk to dateyy
         goto      ok if equal
         add       c1 to branch
;update
         compare   "18" to branch
         goto      input if equal              .outside range
         goto      dateloop                   .try again
.
.old code
         MATCH     "88" TO OMDTEY
         IF        EQUAL
         MOVE      C1 TO BRANCH
         ENDIF
         MATCH     "89" TO OMDTEY
         IF        EQUAL
         MOVE      C2 TO BRANCH
         ENDIF
         MATCH     "90" TO OMDTEY
         IF        EQUAL
         MOVE      C3 TO BRANCH
         ENDIF
         MATCH     "91" TO OMDTEY
         IF        EQUAL
         MOVE      C4 TO BRANCH
         ENDIF
         MATCH     "92" TO OMDTEY
         IF        EQUAL
         MOVE      C5 TO BRANCH
         ENDIF
         MATCH     "93" TO OMDTEY
         IF        EQUAL
         MOVE      C6 TO BRANCH
         ENDIF
         MATCH     "94" TO OMDTEY
         IF        EQUAL
         MOVE      C7 TO BRANCH
         ENDIF
         MATCH     "95" TO OMDTEY
         IF        EQUAL
         MOVE      C8 TO BRANCH
         ENDIF
         MATCH     "96" TO OMDTEY
         IF        EQUAL
         MOVE      C9 TO BRANCH
         ENDIF
         MATCH     "97" TO OMDTEY
         IF        EQUAL
         MOVE      C10 TO BRANCH
         ENDIF
         MATCH     "98" TO OMDTEY
         IF        EQUAL
         MOVE      C11 TO BRANCH
         ENDIF
         MATCH     "99" TO OMDTEY
         IF        EQUAL
         MOVE      C12 TO BRANCH
         ENDIF
.
        MATCH     "00" TO OMDTEY
         IF        EQUAL
         MOVE      C13 TO BRANCH
         ENDIF
.
         MATCH     "01" TO OMDTEY
         IF        EQUAL
         MOVE      C14 TO BRANCH
         ENDIF
..oldcode
         MATCH     "02" TO OMDTEY
         IF        EQUAL
         MOVE      C15 TO BRANCH
         ENDIF
;update
         MATCH     "03" TO OMDTEY
         IF        EQUAL
         MOVE      C16 TO BRANCH
         ENDIF
;update
         MATCH     "04" TO OMDTEY
         IF        EQUAL
         MOVE      C17 TO BRANCH
         ENDIF
;update
         BRANCH    BRANCH OF OK,OK,OK,OK,OK,OK,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok

         GOTO      INPUT
.OK       READ      OUTPUT,KEY60;B1,KEY60,O88,O89,O90,O91,O92,O93,O94,O95
OK
.Start Patch #2.5 - remmed and replaced line due to file expansion
.         READ      OUTPUT,KEY60;B1,KEY60,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98
;update
         READ      OUTPUT,KEY80;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01:
                   o02,x02,r02,o03,x03,r03,o04,x04,r04
.End Patch #2.5 - remmed and replaced line due to file expansion
         GOTO      WRITE IF OVER
         CALL      ADD
.         UPDATE    OUTPUT;B1,KEY60,O88,O89,O90,O91,O92,O93,o94,o95,UNIVERSE
.Start Patch #2.5 - remmed and replaced line due to file expansion
.         UPDATE    OUTPUT;B1,KEY60,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98
;update
         UPDATE    OUTPUT;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01:
                   o02,x02,r02,o03,x03,r03,o04,x04,r04
.End Patch #2.5 - remmed and replaced line due to file expansion
         GOTO      INPUT
WRITE
.Start Patch #2.5 - remmed and replaced line due to file expansion
.         PACK      KEY60 FROM MCOMP,OLSTNAME
         PACK      KEY80 FROM MCOMP,OLSTNAME
.End Patch #2.5 - remmed and replaced line due to file expansion
         MOVE      C0 TO O88
         MOVE      C0 TO O89
          MOVE     C0 TO O90
         MOVE      C0 TO O91
         MOVE      C0 TO O92
         MOVE      C0 TO O93
         MOVE      C0 TO O94
         MOVE      C0 TO O95
         MOVE      C0 TO O96
         MOVE      C0 TO O97
         MOVE      C0 TO O98
         MOVE      C0 TO O99
         MOVE      C0 TO O00
         MOVE      C0 TO O01
         MOVE      C0 TO O02
         MOVE      C0 TO O03
         MOVE      C0 TO O04
         MOVE      C0 TO X88
         MOVE      C0 TO R88
         MOVE      C0 TO X89
         MOVE      C0 TO R89
         MOVE      C0 TO X90
         MOVE      C0 TO R90
         MOVE      C0 TO X91
         MOVE      C0 TO R91
         MOVE      C0 TO X92
         MOVE      C0 TO R92
         MOVE      C0 TO X93
         MOVE      C0 TO R93
         MOVE      C0 TO X94
         MOVE      C0 TO R94
         MOVE      C0 TO X95
         MOVE      C0 TO R95
         MOVE      C0 TO X96
         MOVE      C0 TO R96
         MOVE      C0 TO X97
         MOVE      C0 TO R97
         MOVE      C0 TO X98
         MOVE      C0 TO R98
         MOVE      C0 TO X99
         MOVE      C0 TO R99
         MOVE      C0 TO X00
         MOVE      C0 TO R00
         MOVE      C0 TO X01
         MOVE      C0 TO R01
         MOVE      C0 TO X02
         MOVE      C0 TO R02
;Update
         MOVE      C0 TO X03
         MOVE      C0 TO R03
;Update
         MOVE      C0 TO X04
         MOVE      C0 TO R04
         CALL      ADD
.         WRITE     OUTPUT,KEY60;B1,KEY60,O88,O89,O90,O91,O92,O93,o94,o95,UNIVERSE
.Start Patch #2.5 - remmed and replaced line due to file expansion
.         WRITE     OUTPUT,KEY60;B1,KEY60,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98
;UPdate
         WRITE     OUTPUT,KEY80;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01:
                   o02,x02,r02,o03,x03,r03,o04,x04,r04
.End Patch #2.5 - remmed and replaced line due to file expansion
         GOTO      INPUT
ADD
;Update
         BRANCH    BRANCH OF O88,O89,O90,O91,O92,O93,o94,o95,o96,o97,o98,o99,o00,o01,o02,o03,o04
         NORETURN
         GOTO      INPUT
O88      ADD       QUANT TO O88
         ADD       XQUANT TO X88
         ADD       RQUANT TO R88
         RETURN
O89      ADD       QUANT TO O89
         ADD       XQUANT TO X89
         ADD       RQUANT TO R89
         RETURN
O90      ADD       QUANT TO O90
         ADD       XQUANT TO X90
         ADD       RQUANT TO R90
         RETURN
O91      ADD       QUANT TO O91
         ADD       XQUANT TO X91
         ADD       RQUANT TO R91
         RETURN
O92      ADD       QUANT TO O92
         ADD       XQUANT TO X92
         ADD       RQUANT TO R92
         RETURN
O93      ADD       QUANT TO O93
         ADD       XQUANT TO X93
         ADD       RQUANT TO R93
         RETURN
O94      ADD       QUANT TO O94
         ADD       XQUANT TO X94
         ADD       RQUANT TO R94
         RETURN
O95      ADD       QUANT TO O95
         ADD       XQUANT TO X95
         ADD       RQUANT TO R95
         RETURN
O96      ADD       QUANT TO O96
         ADD       XQUANT TO X96
         ADD       RQUANT TO R96
         RETURN
O97      ADD       QUANT TO O97
         ADD       XQUANT TO X97
         ADD       RQUANT TO R97
         RETURN
O98      ADD       QUANT TO O98
         ADD       XQUANT TO X98
         ADD       RQUANT TO R98
         RETURN
O99      ADD       QUANT TO O99
         ADD       XQUANT TO X99
         ADD       RQUANT TO R99
         RETURN
O00      ADD       QUANT TO O00
         ADD       XQUANT TO X00
         ADD       RQUANT TO R00
         RETURN
O01      ADD       QUANT TO O01
         ADD       XQUANT TO X01
         ADD       RQUANT TO R01
         RETURN
O02      ADD       QUANT TO O02
         ADD       XQUANT TO X02
         ADD       RQUANT TO R02
         RETURN

O03      ADD       QUANT TO O03
         ADD       XQUANT TO X03
         ADD       RQUANT TO R03
         RETURN
O04      ADD       QUANT TO O04
         ADD       XQUANT TO X04
         ADD       RQUANT TO R04
         RETURN
.
FISCAL
          KEYIN     *P1:1,*ES,*P10:15,*JR,"ENTER STARTING MONTH FISCAL YEAR ",*t15,*RV,*dv,FISCMO
;         KEYIN     *P1:1,*ES,*P10:15,*JR,"ENTER STARTING MONTH FISCAL YEAR ":
;                   FISCMO
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
.Start patch #2.6 - replaced var
.         MOVE      C0 TO N7
.         MOVE      OEXQTY TO N7
.         COMPARE   C0 TO N7
.         GOTO      SPLIT IF NOT EQUAL
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         ADD       n7 TO XQUANT
.         RETURN
.SPLIT
.         ADD       N7 TO XQUANT
.         SUB       N7 FROM TORDQTYR
.         MOVE      C0 TO N7
.         MOVE      TORDQTYR TO N7
.         ADD       N7 TO RQUANT
.         RETURN
.RENT
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         ADD       N7 TO RQUANT
.         RETURN
.
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
CLOSE1
        CLOSE     INFILE
         CLOSE     OUTPUT
.End patch #2.6 - replaced var
.===============================================
.         move      "Mlrsum Spool Sect" TO PROGRAM
.         MOVE      "Test - PRINT" TO STITLE
.         move      "Names In Then News Ca Inc" to compnme
.         CLOCK     DATE TO TODAY
.         MOVE      "EXIT" TO PF5
.         CALL      PAINT
.===============================================

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
;patch3.91
        if (str1 = "1" or str1 = "5" or str1 = "6")
;        if (str1 = "1" or str1 = "5")
;patch3.91
          move     c2 to bosflag
        endif
.***************************************************
         pack   SortFle,srtdir,indat,comma,srtdir,outsrt
.Sort and secondary sort on list
           pack   taskname,sortfle,";",mlrsrt
.****************************************************
.       pack      taskname from NTWKPATH2,"sort32 ","c:\Mlrsum.dat ","c:\Mlrsum.srt ":
.                         "/s (47,35,alp,a)"
        sort   taskname
.                      execute   taskname
        if over
               alert caution,S$ERROR$,result,"No Sort"
        endif
        clear n3
.Open sorted file
          OPEN      LISTFLE,"C:\MLRSUM.SRT",SHARE
.Set up columns and title positions
        move    "100",column
        move    "3100",column1
        move    "4100",column2
        move    "5100",column3
        move    "6100",column4
        move    "7100",column5
        move    "8100",column6
        move    "9500",column7
.       7860 row position of pg #
        move    "5260",Title1
.        move    "4000",Title1
        move    "9000",Title2
        move    "9500",Title3
        move    "5260",Title4
.        move    "4500",Title4
        move    "840",mlrrow
Rerun
          Clear OYR95
        Clear XYR95
        Clear RYR95
          Clear OYR96
        Clear XYR96
        Clear RYR96
          Clear OYR97
        Clear XYR97
        Clear RYR97
        Clear OYR98
        Clear XYR98
        Clear RYR98
        Clear OYR99
        Clear XYR99
        Clear RYR99
        Clear OYR00
        Clear XYR00
        Clear RYR00
        Clear OYR01
        Clear XYR01
        Clear RYR01
        Clear OYR02
        Clear XYR02
        Clear RYR02
        clear N8
        move  C0 to newpg
.Patch3.3
        Trap Spool1 giving error if SPOOL
        move comment to cntprint
        if (cntprint = c0)
                move c3 to cntprint
        endif
.EndPatch3.3
        Display    *P10:11,*EF,*cyan,"Printing in Process!!"
.Patch3.5
;patch3.93
                    clock timestamp,timestamp
          pack splfle,ntwkpath1,luser,timestamp,".lst"
                    pack spoolname,luser,timestamp
;         pack splfle,spldir,mlrnum,str4,".lst"
;patch3.93
                if (cntprint = "1" | cntprint = "3")      .Laser6

                        if (bosflag = c2)
                              PRTOPEN prfile,"\\NTS0\Laser6",spoolname,noprint,spoolfile=splfle
.                             PRTOPEN prfile,"\\NTS0\Laser6","Listsum",noprint,spoolfile="c:\work\listsum.lst"
                        else
                              PRTOPEN prfile,"Laser6",spoolname,noprint,spoolfile=splfle
.                             PRTOPEN prfile,"Laser6","Listsum",noprint,spoolfile="c:\work\listsum.lst"
                        endif
                else                                    .Laser3 = Default
                        if (bosflag = c2)
                              PRTOPEN prfile,"\\NTS0\Laser3 Blankstock",spoolname,noprint,spoolfile=splfle
.                             PRTOPEN prfile,"\\NTS0\Laser3 Blankstock","Listsum",noprint,spoolfile="c:\work\listsum.lst"
                        else
                                  PRTOPEN prfile,"Laser3 Blankstock",spoolname,noprint,spoolfile=splfle
.                                 PRTOPEN prfile,"Laser3 Blankstock","Listsum",noprint,spoolfile="c:\work\listsum.lst"
                        endif
                endif
.===========================================================================
        clear PgCnt
PAGE
                              PRTOPEN prfile,"\\NTS0\Laser2",spoolname
        if (newmlr = c1)
                clear pgcnt
                clear rowcount
                clear OYR95
                clear XYR95
                clear RYR95
                clear OYR96
                clear XYR96
                clear RYR96
                clear OYR97
                clear XYR97
                clear RYR97
                clear OYR98
                clear XYR98
                clear RYR98
                clear OYR99
                clear XYR99
                clear RYR99
                clear OYR00
                clear XYR00
                clear RYR00
                clear OYR01
                clear XYR01
                clear RYR01
                clear OYR02
                clear XYR02
                clear RYR02
                clear OTOT
                clear XTOT
                clear RTOT
                clear lsttot
                clear totord
        endif
        add c1 to Pgcnt
        prtpage prfile;*NEWPAGE:
                 *UNITS=*HIENGLISH:
                       *ORIENT=*LANDSCAPE;
        clear   row
        move    "300",row
        prtpage prfile;*pcolumn:row,*font=font12,"Confidential";
        prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News California Inc";
.       prtpage prfile;*pTitle1:row,*ALIGNMENT=*Left,*font=font12,"Names in the News California Inc";
        prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date:";
        clock timestamp,str8
        unpack str8,str2,yy,mm,dd
        clear str10
        pack  str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*pTitle3:row,*font=font12,str10;
       add     eightlpi,row
       add     eightlpi,row
       prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,*ULON,*ll,"Names Mailed By",*ULOFF;
.        prtpage prfile;*pTitle4:row,*font=font12,*ULON,"Names Mailed on",*ULOFF;
       add     eightlpi,row
       add     eightlpi,row
       UNPACK KEY80,str45,str35
.===========
.-                    pack str55 with "Mailer: ",holdnmlr
.-                    call trim using str55
        prtpage prfile;*pTitle1:mlrrow,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,str55,*boldoff;
.       prtpage prfile;*pTitle1:mlrrow,*ALIGNMENT=*Left,*font=font12,*boldon,"Mailer: ",*boldoff;
.       prtpage prfile;*font=font12,*boldon,*ll,holdnmlr,*boldoff;
.===========
       add     eightlpi,row
       add     eightlpi,row
       add     eightlpi,row
       add     eightlpi,row
       prtpage prfile;*pcolumn:row,*font=font12,*boldon,*ULON,"List Name",*ULOFF,*boldoff;
        If (Mode = C2)
.                goto Calendar
        Endif
Fiscal1
.Headers for fiscal year
       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY1,*uloff,*boldoff;
       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY2,*uloff,*boldoff;
       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY3,*uloff,*boldoff;
       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY4,*uloff,*boldoff;
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY5,*uloff,*boldoff;
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY6,*uloff,*boldoff;
       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Total",*uloff,*boldoff;




           prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
           prtpage prfile;*font=font12,*ALIGNMENT=*Left,*ll,PgCnt;
           prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
                              "©","1992-2012, Names in the News";

          return
       goto begin

Calendar
.Headers for calendar year
       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR1,*uloff,*boldoff;
       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR2,*uloff,*boldoff;
       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR3,*uloff,*boldoff;
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR4,*uloff,*boldoff;
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR5,*uloff,*boldoff;
       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Total",*uloff,*boldoff;
Begin
       clear Rowcount
       move c1 to RowCount
       if (newmlr = c1)            .if mlr diff from previous reads skip file read and print new mlr
           clear newmlr
           goto checkmlr
       endif
       loop
       ADD       C1 TO N8
       DISPLAY   *P10:14,*EF,"Records Processed ",N8
;Update
       READ      LISTFLE,SEQ;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01:
                   o02,x02,r02,o03,x03,r03,o04,x04,r04
       until over

Checkmlr
       UNPACK KEY80,str45,str35
       if (holdnmlr = "")
                move str45 to holdnmlr
                goto mlrtitl
       endif
       if (holdnmlr <> str45)
          move c1 to newmlr
          goto lastpage
       endif

MLRTITL
                     pack str55 with "Mailer: ",holdnmlr
                     call trim using str55
       prtpage prfile;*pTitle1:mlrrow,*ALIGNMENT=*CENTER,*font=font12,*ll,*OVERLAYOFF,*boldon,str55,*boldoff;
.       prtpage prfile;*pTitle1:mlrrow,*ALIGNMENT=*Left,*font=font12,*boldon,"Mailer: ",*boldoff;
.             prtpage prfile;*font=font12,*boldon,*ll,holdnmlr,*boldoff;
.OrderTotal
        clear LstTot
        If (MODE = C1)
.FISCAL
;UPdate
                    ADD o98 to lstTot
                    ADD o99 to lstTot
                    ADD o00 to lstTot
                    ADD o01 to lstTot
                    ADD o02 to lstTot
                    ADD o03 to lstTot
                    if (lstTot > c0)                      .if list total is 0 for all years skip
                              ADD lstTot to TotOrd
                              goto TotOrd
                else
                        goto Row2
                    endif
       else
.Calendar
;Update
                    add o00 to lstTot
                    add o01 to lstTot
                    add o02 to lstTot
                    add o03 to lstTot
                    add o04 to lstTot

                    if (lstTot > c0)
                              ADD lstTot to TotOrd
                else
                        goto Row2
                endif
        Endif
TotOrd
.Total Orders Yearly

        If (MODE = C1)
;Update
                 ADD o98 to OYR96
                 ADD o99 to OYR97
                 ADD o00 to OYR98
                 ADD o01 to OYR99
                 ADD o02 to OYR00
                 ADD o03 to OYR01
                 goto Exchg
        else
;Update
                 ADD o00 to OYR98
                 ADD o01 to OYR99
                 ADD o02 to OYR00
                 ADD o03 to OYR01
                 ADD o04 to OYR02
          endif
.Exchange Total-yearly
Exchg
        If (MODE = C1)
;Update
                 ADD X98 to XYR96
                 ADD X99 to XYR97
                 ADD X00 to XYR98
                 ADD X01 to XYR99
                 ADD X02 to XYR00
                 ADD X03 to XYR01
               goto Rent1
        endif
;Update
        ADD X00 to XYR98
        ADD X01 to XYR99
        ADD X02 to XYR00
        ADD X03 to XYR01
        ADD X04 to XYR02
Rent1
.Rental Total-yearly
        If (MODE = C1)
;Update
                 ADD R98 to RYR96
                 ADD R99 to RYR97
                 ADD R00 to RYR98
                 ADD R01 to RYR99
                 ADD R02 to RYR00
                 ADD R03 to RYR01
                 goto Print
       endif
;Update
       ADD R00 to RYR98
       ADD R01 to RYR99
       ADD R02 to RYR00
       ADD R03 to RYR01
       ADD R04 to RYR02
Print
       add     eightlpi,row
       add     eightlpi,row
       UNPACK KEY80,str45,str35
       prtpage prfile;*pcolumn:row,*font=font7,Str35;
        If (MODE = C1)
.==================================================
;Update
       if (O98 > c0)
                    clear str10
                    move O98 to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                 prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
       else
                 prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,O98;
       endif
;Update
       if (O99 > c0)
                    clear str10
                    move O99 to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                  prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
       else
                  prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,O99;
       endif
;Update
       if (O00 > c0)
                    clear str10
                    move O00 to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                  prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
       else
                  prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,O00;
       endif
;Update
       if (O01 > c0)
                    clear str10
                    move O01 to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                  prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
       else
                  prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,O01;
       endif
;Update
       if (O02 > c0)
                    clear str10
                    move O02 to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                  prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
       else
                  prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,O02;
       endif
;Update
       if (O03 > c0)
                    clear str10
                    move O03 to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                  prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
       else
                  prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,O03;
       endif
       if (lstTot > c0)
                    clear str10
                    move lstTot to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                  prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,Str13;
       else
                    prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,lstTot;
       endif
       goto Row
    Endif
;Update
       if (O00 > c0)
                    clear str10
                    move O00 to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                  prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
       else
                  prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,O00;
       endif
;Update
       if (O01 > c0)
                    clear str10
                    move O01 to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                 prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
       else
                 prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,O01;
       endif
;Update
       if (O02 > c0)
                    clear str10
                    move O02 to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                  prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
       else
                  prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,O02;
       endif
;Update
       if (O03 > c0)
                    clear str10
                    move O03 to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                  prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
       else
                  prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,O03;
       endif
;Update
       if (O04 > c0)
                    clear str10
                    move O04 to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                 prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
       else
                  prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,O04;
       endif
       if (lstTot > c0)
                    clear str10
                    move lstTot to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                  prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
       else
                  prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,lstTot;
       Endif

Row
       add c1 to RowCount


Row2
.Begin Patch1.1b
.Reduced Rows on page to make room for Copyright label
       if (ROWCOUNT = "23")
.       if (ROWCOUNT = "24")
           add     eightlpi,row
           add     eightlpi,row
           move "7750",row
.End Patch1.1b
.Begin Patch 1.1a
           prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
           prtpage prfile;*font=font12,*ALIGNMENT=*Left,*ll,PgCnt;
           prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
                              "©","1992-2012, Names in the News";
;                             "©","1992-2004, Names in the News/CA";
.End Patch 1.1a
          RETURN
           goto Page
       endif
;update
.Clear Fields
       clear    O97
       clear    X97
       clear    R97
       clear    O98
       clear    X98
       clear    R98
       clear    O99
       clear    X99
       clear    R99
       clear    O00
       clear    X00
       clear    R00
       clear    O01
       clear    X01
       clear    R01
       clear    O02
       clear    X02
       clear    R02
       clear    O03
       clear    X03
       clear    R03
       clear    O04
       clear    X04
       clear    R04
       repeat
LastPage
       if (ROWCOUNT < "18")
                 goto totals
       else
.Patch3.6==================================================================
                 move "7750",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
                 prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
                              "©","1992-2012, Names in the News";
;                             "©","1992-2004, Names in the News/CA";
.EndPatch3.6====================================================================
               move c1 to newpg
.Patch3.6a========================================================================
               add c1 to Pgcnt
.EndPatch3.6a====================================================================
               prtpage prfile;*NEWPAGE:
                           *UNITS=*HIENGLISH:
                                     *ORIENT=*LANDSCAPE;
          clear   row
                  move    "300",row
                  prtpage prfile;*pcolumn:row,*font=font12,"Confidential";
                  prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News California Inc";
.                 prtpage prfile;*pTitle1:row,*ALIGNMENT=*Left,*font=font12,"Names in the News California Inc";
                  prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date:";
          clock timestamp,str8
                  unpack str8,str2,yy,mm,dd
                  clear str10
                  pack  str10,mm,slash,dd,slash,str2,yy
                  prtpage prfile;*pTitle3:row,*font=font12,str10;
                  add     eightlpi,row
                  add     eightlpi,row
                  prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,*ULON,*ll,"Names Mailed By",*ULOFF;
.                 prtpage prfile;*pTitle4:row,*font=font12,*ULON,"Names Mailed on",*ULOFF;
                  add     eightlpi,row
                    add     eightlpi,row
                         pack str55 with "Mailer: ",holdnmlr
                         call trim using str55
           prtpage prfile;*pTitle1:mlrrow,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,str55,*boldoff;
.                 prtpage prfile;*pTitle1:mlrrow,*ALIGNMENT=*Left,*font=font12,*boldon,"Mailer: ",*boldoff;
.                 prtpage prfile;*font=font12,*boldon,*ll,holdnmlr,*boldoff;
                  UNPACK KEY80,str45,str35
                  add     eightlpi,row
                  add     eightlpi,row
                  add     eightlpi,row
                  add     eightlpi,row
                  goto Totals
       endif
Totals
       if (newpg = c1)
                    move "1380",row
       else
                    add     eightlpi,row
                    add     eightlpi,row
                    add     eightlpi,row
                    add     eightlpi,row
       endif
       if (MODE = c2)
                goto TotCalendar
       Endif
TotFiscal
.Headers for grand totals
       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY1,*uloff,*boldoff;
       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY2,*uloff,*boldoff;
       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY3,*uloff,*boldoff;
       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY4,*uloff,*boldoff;
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY5,*uloff,*boldoff;
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY6,*uloff,*boldoff;
       prtpage prfile;*pcolumn7:row,*font=font12,*boldon,*ulon,"Total",*uloff,*boldoff;
       goto Total1


TotCalendar
.Headers for grand totals
       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR1,*uloff,*boldoff;
       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR2,*uloff,*boldoff;
       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR3,*uloff,*boldoff;
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR4,*uloff,*boldoff;
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR5,*uloff,*boldoff;
       prtpage prfile;*pcolumn7:row,*font=font12,*boldon,*ulon,"Total",*uloff,*boldoff;

Total1
       add     eightlpi,row
       add     eightlpi,row
       add     eightlpi,row
       add     eightlpi,row
       prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ULON,"Grand Totals",*ULOFF,*boldoff;
       If (MODE = C1)
.===========================================
.Year Totals -Fiscal
       if (OYR96 > c0)
          clear str10
          move OYR96 to Str10
          clear str13
          call FormatNumeric using str10,str13,comma
        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
       else
        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR96,*ULOFF,*boldoff;
       endif
       if (OYR97 > c0)
          clear str10
          move OYR97 to Str10
          clear str13
          call FormatNumeric using str10,str13,comma
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
       else
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR97,*ULOFF,*boldoff;
       endif
       if (OYR98 > c0)
          clear str10
          move OYR98 to Str10
          clear str13
          call FormatNumeric using str10,str13,comma
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
       else
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR98,*ULOFF,*boldoff;
       endif
       if (OYR99 > c0)
          clear str10
          move OYR99 to Str10
          clear str13
          call FormatNumeric using str10,str13,comma
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
       else
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR99,*ULOFF,*boldoff;
       endif
       if (OYR00 > c0)
          clear str10
          move OYR00 to Str10
          clear str13
          call FormatNumeric using str10,str13,comma
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
       else
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR00,*ULOFF,*boldoff;
       endif
       if (OYR01 > c0)
          clear str10
          move OYR01 to Str10
          clear str13
          call FormatNumeric using str10,str13,comma
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
       else
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR01,*ULOFF,*boldoff;
       endif
       goto Next
    endif
.==========================================================================
.Year Totals-Calendar
       if (OYR98 > c0)
          clear str10
          move OYR98 to Str10
          clear str13
          call FormatNumeric using str10,str13,comma
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
       else
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR98,*ULOFF,*boldoff;
       endif

       if (OYR99 > c0)
          clear str10
          move OYR99 to Str10
          clear str13
          call FormatNumeric using str10,str13,comma
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
       else
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR99,*ULOFF,*boldoff;
       endif

       if (OYR00 > c0)
          clear str10
          move OYR00 to Str10
          clear str13
          call FormatNumeric using str10,str13,comma
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
       else
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR00,*ULOFF,*boldoff;
       endif

       if (OYR01 > c0)
          clear str10
          move OYR01 to Str10
          clear str13
          call FormatNumeric using str10,str13,comma
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
       else
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR01,*ULOFF,*boldoff;
       endif

       if (OYR02 > c0)
          clear str10
          move OYR02 to Str10
          clear str13
          call FormatNumeric using str10,str13,comma
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
       else
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR02,*ULOFF,*boldoff;
       endif

Next
       If (MODE = C1)
.add year order total for grand total-fiscal
          add OYR96 to OTOT
          add OYR97 to OTOT
          add OYR98 to OTOT
          add OYR99 to OTOT
          add OYR00 to OTOT
          add OYR01 to OTOT
          goto TOT
       Endif
.add year order total for grand total-Calendar
       add OYR98 to OTOT
       add OYR99 to OTOT
       add OYR00 to OTOT
       add OYR01 to OTOT
       add OYR02 to OTOT
TOT
.Grand Total
       if (OTOT > c0)
        clear str10
        move OTOT to Str10
        clear str13
        call FormatNumeric using str10,str13,comma
        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
       else
        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OTOT,*ULOFF,*boldoff;
       endif
       add     eightlpi,row
       add     eightlpi,row
       prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ULON,"Exchange Totals",*ULOFF,*boldoff;
       If (MODE = C1)
.=========================================================
        if (XYR96 > c0)
               clear str10
                move XYR96 to Str10
             clear str13
               call FormatNumeric using str10,str13,comma
               prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
        else
               prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR96,*ULOFF;
        endif
        if (XYR97 > c0)
               clear str10
             move XYR97 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR97,*ULOFF;
       endif
       if (XYR98 > c0)
             clear str10
             move XYR98 to Str10
             clear str13
             call  FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,OYR98,*ULOFF;
       endif
       if (XYR99 > c0)
             clear str10
             move XYR99 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR99,*ULOFF;
       endif
       if (XYR00 > c0)
             clear str10
             move XYR00 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR00,*ULOFF;
       endif
       if (XYR01 > c0)
             clear str10
             move XYR01 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR01,*ULOFF;
       endif
       goto Xyear
   Endif
.====================================================
       if (XYR98 > c0)
             clear str10
               move XYR98 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
               prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,OYR98,*ULOFF;
       endif
       if (XYR99 > c0)
             clear str10
             move XYR99 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR99,*ULOFF;
       endif
       if (XYR00 > c0)
             clear str10
             move XYR00 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR00,*ULOFF;
       endif
       if (XYR01 > c0)
             clear str10
             move XYR01 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR01,*ULOFF;
       endif
       if (XYR02 > c0)
             clear str10
             move XYR02 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR02,*ULOFF;
       endif
Xyear
       If (MODE = C1)
             clear XTOT
             add XYR96 to XTOT
             add XYR97 to XTOT
             add XYR98 to XTOT
             add XYR99 to XTOT
             add XYR00 to XTOT
             add XYR01 to XTOT
             goto RYear
       Endif
       clear XTOT
       add XYR98 to XTOT
       add XYR99 to XTOT
       add XYR00 to XTOT
       add XYR01 to XTOT
       add XYR02 to XTOT
RYEAR
       if (XTOT > c0)
             clear str10
             move XTOT to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
        else
             prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,XTOT,*ULOFF,*boldoff;
        endif
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ULON,"Rental Totals",*ULOFF,*boldoff;
        If (MODE = C1)
             if (RYR96 > c0)
                    clear str10
                    move RYR96 to Str10
                    clear str13
                    call FormatNumeric using str10,str13,comma
                    prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
             else
                    prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR96,*ULOFF;
               endif
         if (RYR97 > c0)
             clear str10
               move RYR97 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
         else
             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR97,*ULOFF;
         endif
         if (RYR98 > c0)
             clear str10
             move RYR98 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
         else
             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR98,*ULOFF;
         endif
         if (RYR99 > c0)
             clear str10
               move RYR99 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
         else
             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR99,*ULOFF;
         endif
         if (RYR00 > c0)
             clear str10
             move RYR00 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
         else
             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR00,*ULOFF;
         endif
         if (RYR01 > c0)
             clear str10
             move RYR01 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
         else
             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR01,*ULOFF;
         Endif
         goto Rent2
       ENDIF
       if (RYR98 > c0)
             clear str10
             move RYR98 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR98,*ULOFF;
       endif
       if (RYR99 > c0)
             clear str10
             move RYR99 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR99,*ULOFF;
       endif
       if (RYR00 > c0)
             clear str10
             move RYR00 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR00,*ULOFF;
       endif
       if (RYR01 > c0)
             clear str10
             move RYR01 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR01,*ULOFF;
       Endif
       if (RYR02 > c0)
             clear str10
             move RYR02 to Str10
             clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
       else
             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR02,*ULOFF;
       endif
Rent2
       If (MODE = C1)
             add RYR96 to RTOT
             add RYR97 to RTOT
             add RYR98 to RTOT
             add RYR99 to RTOT
             add RYR00 to RTOT
             add RYR01 to RTOT
             goto LstPg
       Endif
       add RYR98 to RTOT
       add RYR99 to RTOT
       add RYR00 to RTOT
       add RYR01 to RTOT
       add RYR02 to RTOT
LstPg
       if (RTOT > c0)
               clear str10
               move RTOT to Str10
               clear str13
             call FormatNumeric using str10,str13,comma
             prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
       else
             prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,RTOT,*ULOFF,*boldoff;
       endif
.Patch 1.1
       move "7750",row
       prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
       prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
       prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
                              "©","1992-2012, Names in the News";
;                             "©","1992-2004, Names in the News/CA";
End1
       PRTCLOSE prfile
       Pause c1
.Patch3.2.................................................
        clear n3
        if (copy = c0)
                goto End2
        Endif
.Patch3.5
.........................................................
.test formatnumeric OS
        if (bosflag = c2)
                goto looper2
        endif
.==============================================================
.Printer Area Test
Looper1
;patch3.93
        if (pdfopt = YES)
                  goto pdfthis
        endif
;patch3.93
        loop
          until (N3 = COPY)

                if (cntprint = "1" | cntprint = "3")      .Laser6
                              PRTPLAY splfle,"Laser6"
.                             PRTPLAY "c:\work\listsum.lst","Laser6"
                else
                              PRTPLAY splfle,"Laser3 Blankstock"
.                             PRTPLAY "c:\work\listsum.lst","Laser3 Blankstock"
                endif
                add c1 to N3
        repeat
        goto checknew
.================================================================
Looper2
;patch3.93
        if (pdfopt = YES)
                  goto pdfthis
        endif
;patch3.93
        loop
          until (N3 = COPY)
                if (cntprint = "1" | cntprint = "3")      .Laser6
                              PRTPLAY splfle,"\\NTS0\Laser6"
.                             PRTPLAY "c:\work\listsum.lst","\\NTS0\Laser6"
                else
                              PRTPLAY splfle,"\\NTS0\Laser3 Blankstock"
.                             PRTPLAY "c:\work\listsum.lst","\\NTS0\Laser3 Blankstock"
                endif
                add c1 to N3
        repeat
checknew
       if (newmlr = c1)
;patch3.93
                if (pdfopt <> YES)
                     erase splfle
                endif
;patch3.93
;                erase splfle
;.               erase "c:\work\mlrsum.lst"
                Pause c1
                move str45 to holdnmlr
                add c1 to n4
                move n4 to str4
                call trim using str4
                goto rerun
       endif
End2
        trapclr spool
        NORETURN
        Display    *P10:11,*EF,*cyan,"Printing Finished!!"
        Pause c1
        Display    *P10:11,*EF,*cyan,"Good Bye!!!!!!!!!!!"
;patch3.93
        if (pdfopt <> YES)
                     erase splfle
         endif
;        erase splfle
;patch3.93
;.       erase      "c:\work\mlrsum.lst"
        Pause c1
        shutdown  "cls"
          stop
EOJ
         CLOSE     INFILE
         CLOSE     OUTPUT
         NORETURN
         shutdown  "cls"
         STOP

.Patch3.5
.===================================================
Spool1
         Display    *P10:11,*EF,*cyan,error
         PAUSE c2
;3.93 : added for plbserver runs does not like "\\nts0\laser?"         gets s10 if s10 try this
         Display    *P10:11,*EF,*cyan,"Trying Again"
         Pause c2
         TRAPCLR   SPOOL
         Trap SPOOL2 giving error if SPOOL
         SCAN      "S10" IN ERROR
         if equal
                if (cntprint = "1" | cntprint = "3")      .Laser6
                    PRTOPEN prfile,"Laser6",spoolname,noprint,spoolfile=splfle
                else
                    PRTOPEN prfile,"Laser3",spoolname,noprint,spoolfile=splfle
                endif
         endif
         return
SPOOL2
         Display    *P10:11,*EF,*cyan,error
;3.93
         CLOSE     INFILE
         CLOSE     OUTPUT
         NORETURN
         shutdown  "cls"
         STOP
.================================================
;patch3.93
pdfthis
         clear     taskname
         append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 f:\apps\winbatch\butil job=O34 INfile=",TASKNAME
         APPEND    spoolname TO TASKNAME
         APPEND    " B=",TASKNAME
         APPEND    luser TO TASKNAME
         reset     taskname
;          alert caution,taskname,result
         EXECUTE   TASKNAME
         goto checknew
;patch3.93
.******************************************************
         include   ncntio.inc
.patch3.96
                                        include   compio.inc
                                        include   cntio.inc
.         INCLUDE   NMLRIO.INC
.patch3.96
         INCLUDE   NDATIO.INC
         INCLUDE   COMLOGIC.INC

