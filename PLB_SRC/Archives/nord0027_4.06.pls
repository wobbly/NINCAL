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
.Patch 3.99
                              include winapi.inc
.Patch 3.99
         include   ncntdd.inc
release  init      "4.06"       DLH     New Year Update - always PDF
reldate   Init      "28 Jan 2010"
.release  init      "4.05"       JD      21May2009  New Year Update
.reldate   Init      "21 May 2009"
.release  init      "4.04"       DLH     SendMail
.reldate   Init      "24 April 2008"
.release  init      "4.03"       DLH    09JAN2008 New Year Update
.release  init      "4.02.2"       JD   21Dec2007 Added SA's printer option
.release  init      "4.02.1"        DLH 24May2007 Yearly Update         cleanup 
.release  init      "4.02"         JD   10JAN2007 Yearly Update        
.release  init      "4.01"        DMB   20JAN2006 Yearly Update        
.release  init      "4.00"        DMB   20JAN2006 Corrected Formating Bug
.release  init      "3.99"        DMB   13JAN2005 Yearly Update
.release  init      "3.98"        ASH   06AUG2004 Logo Conversion
.release  init      "3.97"        DLH   29July2004          Copywrite footer
.release  init      "3.96"        DMB   26MAY2004 Mailer Conversion
.Release   INIT       "3.95"          10/27/03 Added New Year
.Release   INIT       "3.94"         04/28/03 Added option for the order/maildate selection to be made by nord006b
.Release   INIT       "3.93"         03/20/03 Added pdf option and clean up code to allow plbserv to do prtopen (does not like \\SRV2008a\laser? just doing laser?
.Release   INIT       "3.92"          DMB 08/06/02 New Fiscal\Calendar Year
.Release   INIT       "3.91"          DMB 07/23/02 added code for XP in osflag
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
.UPdate
.Patch 3.99 Yearly Update
.begin patch 4.05
.OUTPUT   IFILE     KEYLEN=80,FIX=691   .KEY= MAILER NAME + LIST NAME
.begin patch 4.06
.OUTPUT   IFILE     KEYLEN=80,FIX=801   .KEY= MAILER NAME + LIST NAME
OUTPUT   IFILE     KEYLEN=80,FIX=831   .KEY= MAILER NAME + LIST NAME
.enD patch 4.06
.end patch 4.05
.begin patch 4.03
.OUTPUT   IFILE     KEYLEN=80,FIX=691   .KEY= MAILER NAME + LIST NAME
.OUTPUT   IFILE     KEYLEN=80,FIX=771   .KEY= MAILER NAME + LIST NAME
.end patch 4.03
.OUTPUT   IFILE     KEYLEN=80,FIX=681   .KEY= MAILER NAME + LIST NAME
.Patch 3.99 Yearly Update
.OUTPUT   IFILE     KEYLEN=80,FIX=561     .KEY= MAILER NAME + LIST NAME
.OUTPUT   IFILE     KEYLEN=80,FIX=561     .KEY= MAILER NAME + LIST NAME
.OUTPUT   IFILE     KEYLEN=80,FIX=501     .KEY= MAILER NAME + LIST NAME
.OUTPUT   IFILE     KEYLEN=60,FIX=421     .KEY= MAILER NAME + LIST NAME
.End Patch #2.5 - remmed and replaced line due to file expansion
.OUTPUT   IFILE     KEYLEN=60,FIX=331     .KEY= MAILER NAME + LIST NAME
.
HOLDMLR  DIM       7
.>Patch 4.0 Code Added to fix numeric formatting
mask15      init    "(Z,ZZZ,ZZZ,ZZ9)"        ;formatting vars
Dim15a      dim     15            ;formatting vars
.>Patch 4.0
.patch to pass year
c16      FORM      "16"
c17      FORM      "17"
c18      FORM      "18"
.Patch 4.01
c19      FORM      "19"
.Patch 4.01
.Patch 4.05
c21      FORM      "21"
.Patch 4.05
.UPdate
..............
.OUTPUT FILE.
.Start Patch #2.5 - remmed and replaced line due to file expansion
.KEY60    DIM       60         KEY VAR.
KEY80    DIM       80         KEY VAR.
.End Patch #2.5 - remmed and replaced line due to file expansion
.B1       DIM        1        1-1
OMLR     DIM       45         2-46
OLIST    DIM       35        47-81
O88      FORM      10        82-91      88 NAMES (MD)
X88      FORM      10        72-101      88 NAMES (MD)
R88      FORM      10        81-111      88 NAMES (MD)
O89      FORM      10        91-121     89  NAMES (MD)
X89      FORM      10       101-131     89 NAMES (MD)
R89      FORM      10       111-141     89 NAMES (MD)
O90      FORM      10       121-151     90 NAMES (Md)
X90      FORM      10       131-161     90 NAMES (MD)
R90      FORM      10       141-171     90 NAMES (MD)
O91      FORM      10       151-181     91 NAMES (Md)
X91      FORM      10       101-191     91 NAMES (MD)
R91      FORM      10       111-201     91 NAMES (MD)
O92      FORM      10       121-211     92 NAMES (Md)
X92      FORM      10       101-221     92 NAMES (MD)
R92      FORM      10       111-231     92 NAMES (MD)
O93      FORM      10       121-241     93 NAMES (Md)
X93      FORM      10       101-251     93 NAMES (MD)
R93      FORM      10       111-261     93 NAMES (MD)
O94      FORM      10       121-271     94 NAMES (Md)
X94      FORM      10       101-281     94 NAMES (MD)
R94      FORM      10       111-291     94 NAMES (MD)
O95      FORM      10       132-301     95 NAMES (MD)
X95      FORM      10       101-311     95 NAMES (MD)
R95      FORM      10       111-321     95 NAMES (MD)
O96      FORM      10       132-331     96 NAMES (MD)
X96      FORM      10       101-341     96 NAMES (MD)
R96      FORM      10       111-351     96 NAMES (MD)
O97      FORM      10       132-361     96 NAMES (MD)
X97      FORM      10       101-371     96 NAMES (MD)
R97      FORM      10       111-381     96 NAMES (MD)
O98      FORM      10                -391
X98      FORM      10                -401
R98      FORM      10                -411
O99      FORM      10                -421
X99      FORM      10                -431
R99      FORM      10                -441
O00      FORM      10          -451           (Md)
X00      FORM      10          -461           (MD)
R00      FORM      10          -471           (MD)
O01      FORM      10          -481          (Md)
X01      FORM      10          -491           (MD)
R01      FORM      10          -501           (MD)
O02      FORM      10          -511           (MD)
X02      FORM      10          -521           (MD)
R02      FORM      10          -531           (MD)
O03      FORM      10          -541           (MD)
X03      FORM      10          -551           (MD)
R03      FORM      10          -561           (MD)
O04      FORM      10          -571          (MD)
X04      FORM      10          -581           (MD)
R04      FORM      10          -601           (MD)
.Patch 3.99 Yearly Update
O05      FORM      10          -610           (MD)
X05      FORM      10          -621           (MD)
R05      FORM      10          -631           (MD)
.Patch 3.99 Yearly Update
.Patch 4.01
O06      FORM      10          -641           (MD)
X06      FORM      10          -651           (MD)
R06      FORM      10          -661           (MD)
.Patch 4.01
.Patch 4.02
O07      FORM      10          -671           (MD)
X07      FORM      10          -681           (MD)
R07      FORM      10          -691          (MD)
.Patch 4.02
.Begin Patch 4.03
O08      FORM      10          -701           (MD)
X08      FORM      10          -711           (MD)
R08      FORM      10          -721          (MD)
.End Patch 4.03
.Begin Patch 4.05
O09      FORM      10          -722           (MD)
X09      FORM      10          -731           (MD)
R09      FORM      10          -741          (MD)
.End Patch 4.05
.Begin Patch 4.06
O10      FORM      10          -752           (MD)
X10      FORM      10          -761           (MD)
R10      FORM      10          -771          (MD)
.End Patch 4.06
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
.UPdate
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
.Patch 3.99 Yearly Update
yr05                init      "05"
.Patch 3.99 Yearly Update
.Patch 4.01
yr06                init      "06"
.Patch 4.01
.Patch 4.02
yr07      init      "07"
.Patch 4.02
.Begin Patch 4.03
yr08      init      "08"
.End Patch 4.03
.Begin Patch 4.05
yr09      init      "09"
.End Patch 4.05
.Begin Patch 4.06
yr10      init      "10"
.End Patch 4.06
datechk  dim       2
dateyy   dim       2
c12      form      "12"
FUNCBR   FORM      "0"       PROGRAM CONTROL BRANCH.
c14      form      "14"
c15      form      "15"
.==============================================================================
ListFle  file
prfile   pfile
..patch3.91
.bosflag   form   1          1=win 95,98, 2=NT
..patch3.91
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
.UPdate

.Patch 3.99 Yearly Update
.FY1 init "FY 00/01"
.FY2 init "FY 01/02"
.FY3 init "FY 02/03"
.FY4 init "FY 03/04"
.FY5 init "FY 04/05"
.FY6 init "FY 05/06"
.Patch 3.99 Yearly Update
.Patch 4.02 Yearly Update
.begin patch 4.05
.FY1 init "FY 01/02"
.FY2 init "FY 02/03"
.FY3 init "FY 03/04"
.FY4 init "FY 04/05"
.FY5 init "FY 05/06"
.FY6 init "FY 06/07"
.begin patch 4.06
.FY1 init "FY 03/04"
.FY2 init "FY 04/05"
.FY3 init "FY 05/06"
.FY4 init "FY 06/07"
.FY5 init "FY 07/08"
.FY6 init "FY 08/09"
FY1 init "FY 04/05"
FY2 init "FY 05/06"
FY3 init "FY 06/07"
FY4 init "FY 07/08"
FY5 init "FY 08/09"
FY6 init "FY 09/10"
.end patch 4.06
.begin patch 4.05
.begin patch 4.03
.FY1 init "FY 01/02"
.FY2 init "FY 02/03"
.FY3 init "FY 03/04"
.FY4 init "FY 04/05"
.FY5 init "FY 05/06"
.FY6 init "FY 06/07"
.FY1 init "FY 02/03"
.FY2 init "FY 03/04"
.FY3 init "FY 04/05"
.FY4 init "FY 05/06"
.FY5 init "FY 06/07"
.FY6 init "FY 07/08"
.end patch 4.03
.Patch 4.02 Yearly Update
.Patch 4.01

.Calendar Headers
.UPdate
.Patch 3.99 Yearly Update
.Patch 4.01
.YR1 init "2002"
.YR2 init "2003"
.YR3 init "2004"
.YR4 init "2005"
.YR5 init "2006"
.Patch 4.01
.Patch 4.01
.Begin patch 4.05
.YR1 init "2003"
.YR2 init "2004"
.YR3 init "2005"
.YR4 init "2006"
.YR5 init "2007"
.Begin patch 4.06
.YR1 init "2005"
.YR2 init "2006"
.YR3 init "2007"
.YR4 init "2008"
.YR5 init "2009"
YR1 init "2006"
YR2 init "2007"
YR3 init "2008"
YR4 init "2009"
YR5 init "2010"
.end patch 4.06
.end patch 4.05
.Begin patch 4.03
.YR1 init "2003"
.YR2 init "2004"
.YR3 init "2005"
.YR4 init "2006"
.YR5 init "2007"
.YR1 init "2004"
.YR2 init "2005"
.YR3 init "2006"
.YR4 init "2007"
.YR5 init "2008"
.end patch 4.03
.Patch 4.01
.Patch 3.99 Yearly Update
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
XYR02     form      10         02 Xchange Grand Total
RYR02     form      10         02 Rental  Grand Total
.=====================================================
OTOT     form      10         ORDER   GRAND Total
XTOT     form      10         Xchange Grand Total
RTOT     form      10         Rental  Grand Total
.=============================================================================
.Sort Parameters=======================================================
INDAT    init  "MLRSUM.DAT"   .File to be sorted
OUTSRT   init  "MLRSUM.SRT"   .Partial Name of sorted Output file for ncsh
MLRSRT   init  "2-46,47-81"   .Sort
SRTDIR   init  "c:\work\"               ."
SORTFLE  dim    100                          .Var to pack file names of sort

.patch3.93
PDFOPT   DIM        1
FMONTH   FORM       2
LUSER    DIM        10
.patch3.93
.Patch 3.99
timestamp1 dim  16
time1   form    16
time2   form    16
time3   form    16
.Patch 3.99
.==========================================================================
         move      c3 to ncntpath
.patch3.93
         move levels to PDFOPT
         move prio to FMONTH
         move user to LUSER
.patch3.93
.Begin patch 4.06
.          Move      Yes,PDfopt
.end patch 4.06
         move      inits to copy       .CHAINED FROM DSINIT WITH INFO-copies
         MOVE      FUNC TO FUNCBR      .CHAINED FROM DSINIT WITH INFO-mode
         CMATCH    B1 TO PROGRAM
         IF        EOS                 .NO
                   move      c1 to dteflag
                   MOVE      "NORD0027" TO PROGRAM
                   MOVE      "Names In The News" TO COMPNME
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
         OPEN      OUTPUT,"C:\work\MLRSUM",EXCLUSIVE
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
.Update
.Patch 3.99 Yearly Update
.Patch 4.01
.          PREPARE   OUTPUT,"C:\work\MLRSUM","C:\work\mlrsum","80","621"
.          PREPARE   OUTPUT,"C:\work\MLRSUM","C:\work\mlrsum","80","681"
.Patch 4.01          
.Patch 4.01
.          PREPARE   OUTPUT,"C:\work\MLRSUM","C:\work\mlrsum","80","621"
.          PREPARE   OUTPUT,"C:\work\MLRSUM","C:\work\mlrsum","80","681"
.Patch 4.01          
.Patch 4.02
.          PREPARE   OUTPUT,"C:\work\MLRSUM","C:\work\mlrsum","80","621"
.Begin Patch 4.05
.          PREPARE   OUTPUT,"C:\work\MLRSUM","C:\work\mlrsum","80","741"
.Begin Patch 4.06
          PREPARE   OUTPUT,"C:\work\MLRSUM","C:\work\mlrsum","80","831"
.          PREPARE   OUTPUT,"C:\work\MLRSUM","C:\work\mlrsum","80","801"
.enD Patch 4.06
.end Patch 4.05
.Begin Patch 4.03
.          PREPARE   OUTPUT,"C:\work\MLRSUM","C:\work\mlrsum","80","741"
.          PREPARE   OUTPUT,"C:\work\MLRSUM","C:\work\mlrsum","80","771"
.end Patch 4.03
.Patch 4.02          
.          PREPARE   OUTPUT,"C:\work\MLRSUM","C:\work\mlrsum","80","561"
.          PREPARE   OUTPUT,"C:\MLRSUM","C:\mlrsum","80","531"
.Patch 3.99 Yearly Update
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
.patch3.93
         move      fmonth to fiscmo
         rep       zfill,fiscmo
.patch3.93
         CALL      FISCAL
         ELSE
         MOVE      C2 TO MODE
         ENDIF
CHECK2
.         MOVE      "M" to str1
.         KEYIN     *P10:12,*EF,*cyan,"M",*white,"aildate (O)rder date: ",*T15,*RV,STR1
.         rep       "M1O2m1o2" in STR1
.patch3.94
         MOVE      CURSYS to str1
         if (str1 = "1")
            DISPLAY   *P10:12,"Select Mail Date"
         endif
         if (str1 = "2")
            DISPLAY   *P10:12,"Select Order Date"
         endif
.patch3.94
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
.Update
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
.Update
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

.Update
        MATCH     "05" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C18 TO BRANCH
          ELSE
         MOVE      C17 TO BRANCH
         ENDIF
         ENDIF
.Update
.Update
.Patch 4.01
        MATCH     "06" TO OMDTEY
         IF        EQUAL
                   MOVE      OMDTEM TO ORDMO
                   COMPARE   START TO ORDMO
                   IF        NOT LESS
                             MOVE      C19 TO BRANCH
                    ELSE
                             MOVE      C18 TO BRANCH
                   ENDIF
         ENDIF
.Patch 4.01         


.Patch 4.02
        MATCH     "07" TO OMDTEY
         IF        EQUAL
                   MOVE      OMDTEM TO ORDMO
                   COMPARE   START TO ORDMO
                   IF        NOT LESS
                             MOVE      CC TO BRANCH
                    ELSE
                             MOVE      C19 TO BRANCH
                   ENDIF
         ENDIF
.Patch 4.02
.Begin Patch 4.03
        MATCH     "08" TO OMDTEY
         IF        EQUAL
                   MOVE      OMDTEM TO ORDMO
                   COMPARE   START TO ORDMO
                   IF        NOT LESS
                             MOVE      CC TO BRANCH
                    ELSE
                             MOVE      C19 TO BRANCH
                   ENDIF
         ENDIF
.End Patch 4.03

.Begin Patch 4.05
        MATCH     "09" TO OMDTEY
         IF        EQUAL
                   MOVE      OMDTEM TO ORDMO
                   COMPARE   START TO ORDMO
                   IF        NOT LESS
                             MOVE      CC TO BRANCH
                    ELSE
                             MOVE      C19 TO BRANCH
                   ENDIF
         ENDIF
.End Patch 4.05
.Begin Patch 4.06
        MATCH     "10" TO OMDTEY
         IF        EQUAL
                   MOVE      OMDTEM TO ORDMO
                   COMPARE   START TO ORDMO
                   IF        NOT LESS
                             MOVE      CC TO BRANCH
                    ELSE
                             MOVE      C19 TO BRANCH
                   ENDIF
         ENDIF
.End Patch 4.06
          BRANCH    BRANCH OF OK,OK,OK,OK,OK,OK,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok
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
.UPdate
.begin patch 4.02.1
dateloop load      dateyy from branch of yr88,yr89,yr90,yr91,yr92,yr93,yr94:
.begin patch 4.03
.                   yr95,yr96,yr97,yr98,yr99,yr00,yr01,yr02,yr03,yr04,yr05,yr06,yr07,yr08
.                   yr95,yr96,yr97,yr98,yr99,yr00,yr01,yr02,yr03,yr04,yr05,yr06,yr07
.end patch 4.03
.begin patch 4.05
.begin patch 4.06
.                   yr95,yr96,yr97,yr98,yr99,yr00,yr01,yr02,yr03,yr04,yr05,yr06,yr07,yr08,yr09
                   yr95,yr96,yr97,yr98,yr99,yr00,yr01,yr02,yr03,yr04,yr05,yr06,yr07,yr08,yr09,Yr10
.end patch 4.06
.                   yr95,yr96,yr97,yr98,yr99,yr00,yr01,yr02,yr03,yr04,yr05,yr06,yr07
.end patch 4.05
.                   yr95,yr96,yr97,yr98,yr99,yr00,yr01,yr02,yr03,yr04,yr05,yr06
         match     datechk to dateyy
         goto      ok if equal
         add       c1 to branch
.update
.         compare   "20" to branch
.begin patch 4.03
.         compare   "21" to branch
.         compare   "22" to branch
.end patch 4.03
.begin patch 4.05
.         compare   "21" to branch
.begin patch 4.06
.         compare   "23" to branch
         compare   "24" to branch
.end patch 4.06
.end patch 4.05
         goto      input if equal              .outside range
         goto      dateloop                   .try again
.end patch 4.02.1
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

         MATCH     "03" TO OMDTEY
         IF        EQUAL
         MOVE      C16 TO BRANCH
         ENDIF         
.update
         MATCH     "04" TO OMDTEY
         IF        EQUAL
         MOVE      C17 TO BRANCH
         ENDIF
.update
         MATCH     "05" TO OMDTEY
         IF        EQUAL
         MOVE      C18 TO BRANCH
         ENDIF
.update
         MATCH     "06" TO OMDTEY
         IF        EQUAL
         MOVE      C19 TO BRANCH
         ENDIF
.update
         BRANCH    BRANCH OF OK,OK,OK,OK,OK,OK,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok

         GOTO      INPUT
.OK       READ      OUTPUT,KEY60;B1,KEY60,O88,O89,O90,O91,O92,O93,O94,O95
OK
.Start Patch #2.5 - remmed and replaced line due to file expansion
.         READ      OUTPUT,KEY60;B1,KEY60,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98
.update
.begin patch 4.02.1  ,o07,x07,r07
         READ      OUTPUT,KEY80;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01:
.begin patch 4.03
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07
.end patch 4.03
.begin patch 4.05
                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08:
.begin patch 4.06
.                   o09,x09,r09
                   o09,x09,r09,o10,x10,r10
.end patch 4.06
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07
.end patch 4.05
.End Patch #2.5 - remmed and replaced line due to file expansion
         GOTO      WRITE IF OVER
         CALL      ADD
.         UPDATE    OUTPUT;B1,KEY60,O88,O89,O90,O91,O92,O93,o94,o95,UNIVERSE
.Start Patch #2.5 - remmed and replaced line due to file expansion
.         UPDATE    OUTPUT;B1,KEY60,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98
.update
.begin patch 4.02.1  ,o07,x07,r07
         UPDATE    OUTPUT;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01:
.begin patch 4.03
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07
.end patch 4.03
.begin patch 4.05
                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08:
.begin patch 4.06
.                   o09,x09,r09
                   o09,x09,r09,o10,x10,r10
.end patch 4.06
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07
.end patch 4.05
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
.update         
         MOVE      C0 TO O05
         MOVE      C0 TO O06         
.begin patch 4.03
         MOVE      C0 TO O07
         MOVE      C0 TO O08              
.end patch 4.03
.begin patch 4.05
         MOVE      C0 TO O09
.end patch 4.05
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
.Update
         MOVE      C0 TO X03
         MOVE      C0 TO R03
.Update
         MOVE      C0 TO X04
         MOVE      C0 TO R04
.Update
         MOVE      C0 TO X05
         MOVE      C0 TO R05
.Patch 4.01
         MOVE      C0 TO X06
         MOVE      C0 TO R06
.Patch 4.01
.Patch 4.02
         MOVE      C0 TO X07
         MOVE      C0 TO R07
.Patch 4.02
.Begin Patch 4.03
         MOVE      C0 TO X08
         MOVE      C0 TO R08
.End Patch 4.03
.Begin Patch 4.05
         MOVE      C0 TO X09
         MOVE      C0 TO R09
.End Patch 4.05
.Begin Patch 4.06
         MOVE      C0 TO o10
         MOVE      C0 TO R10
         move       c0,x10
.End Patch 4.06
         CALL      ADD
.         WRITE     OUTPUT,KEY60;B1,KEY60,O88,O89,O90,O91,O92,O93,o94,o95,UNIVERSE
.Start Patch #2.5 - remmed and replaced line due to file expansion
.         WRITE     OUTPUT,KEY60;B1,KEY60,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98
.UPdate
.Patch 4.01
.         WRITE     OUTPUT,KEY80;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01:
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06
.Patch 4.01                   
.Patch 4.02
         WRITE     OUTPUT,KEY80;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01:
.begin patch 4.03                   
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07
.end patch 4.03                   
.begin patch 4.05  
                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08:
.begin patch 4.06
.                   o09,x09,r09
                   o09,x09,r09,o10,x10,r10
.end patch 4.06
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07
.end patch 4.05
.Patch 4.02                   
.End Patch #2.5 - remmed and replaced line due to file expansion
         GOTO      INPUT
ADD
.Update
.Patch 4.01                   
.Begin Patch 4.03                   
.         BRANCH    BRANCH OF O88,O89,O90,O91,O92,O93,o94,o95,o96,o97,o98,o99,o00,o01,o02,o03,o04,o05,o06,o07,o08
.         BRANCH    BRANCH OF O88,O89,O90,O91,O92,O93,o94,o95,o96,o97,o98,o99,o00,o01,o02,o03,o04,o05,o06,o07
.end Patch 4.03                   
.Begin Patch 4.05
.Begin Patch 4.06
.         BRANCH    BRANCH OF O88,O89,O90,O91,O92,O93,o94,o95,o96,o97,o98,o99,o00,o01,o02,o03,o04,o05,o06,o07,o08,o09
         BRANCH    BRANCH OF O88,O89,O90,O91,O92,O93,o94,o95,o96,o97,o98,o99,o00,o01,o02,o03,o04,o05,o06,o07,o08,o09:
                    o10
.end Patch 4.06
              
.         BRANCH    BRANCH OF O88,O89,O90,O91,O92,O93,o94,o95,o96,o97,o98,o99,o00,o01,o02,o03,o04,o05,o06,o07
.end Patch 4.05
.Patch 4.01                            
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
O05      ADD       QUANT TO O05
         ADD       XQUANT TO X05
         ADD       RQUANT TO R05
         RETURN
.Patch 4.01    
O06      ADD       QUANT TO O06
         ADD       XQUANT TO X06
         ADD       RQUANT TO R06
         RETURN
.Patch 4.01                   
.Patch 4.02    
O07      ADD       QUANT TO O07
        ADD       XQUANT TO X07
         ADD       RQUANT TO R07
         RETURN
.Patch 4.02                   
.Begin Patch 4.03
O08      ADD       QUANT TO O08
        ADD       XQUANT TO X08
         ADD       RQUANT TO R08
         RETURN
.end Patch 4.03                   
.Begin Patch 4.05
O09      ADD       QUANT TO O09
        ADD       XQUANT TO X09
         ADD       RQUANT TO R09
         RETURN
.end Patch 4.05                 
.Begin Patch 4.06
O10      ADD       QUANT TO O10
        ADD       XQUANT TO X10
         ADD       RQUANT TO R10
         RETURN
.end Patch 4.06
.
FISCAL
          KEYIN     *P1:1,*ES,*P10:15,*JR,"ENTER STARTING MONTH FISCAL YEAR ",*t15,*RV,*dv,FISCMO
.         KEYIN     *P1:1,*ES,*P10:15,*JR,"ENTER STARTING MONTH FISCAL YEAR ":
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
.        move     c0 to bosflag
..0 = unknown
..1 = Windows NT
..2 = WIN32s Windows 3.1x (obsolete)
..3 = Window 95
..4 = Window 98
..5 = Windows 2000
..6 = XP
..8 = Windows CE
.        if (str1 = "3" or str1 = "4")
.                  move     c1 to bosflag
.        endif
..patch3.91
.        if (str1 = "1" or str1 = "5" or str1 = "6")
..        if (str1 = "1" or str1 = "5")
..patch3.91
.          move     c2 to bosflag
.        endif
          call      GetWinVer


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
          OPEN      LISTFLE,"C:\\work\MLRSUM.SRT",SHARE
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
.patch3.93
                    clock timestamp,timestamp
          pack splfle,"c:\work\",luser,timestamp,".lst"
                    pack spoolname,luser,timestamp
.         pack splfle,spldir,mlrnum,str4,".lst"
.patch3.93
.Patch 3.99
          call      debug
          if (pdfopt = YES)
.>Patch 3.99 Logic Addition for PDF Quality Control
                              call      "GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
                              "Parameters":
                              "ProcessPDF":
                              "\\nins1\e\apps\plb\code\pdftest.bat":
                              result
                              if (result = C0)
.Prepare Flag file
                                        prep      tempfile,"c:\progra~1\pdf995\flag.dat"
                                        write     tempfile,SEQ;"flag set"
                                        close     tempfile
                              endif
.>Patch 3.99 Logic Addition for PDF Quality Control         
                              PRTOPEN prfile,"PDF995","MlrSum.pdf"
          else
.Patch 3.99
.Patch 4.02.2
                    if (cntprint = "5")      .Susan
.                              PRTOPEN prfile,"\\NIN0032\KYOCERAS",spoolname,noprint,spoolfile=splfle
                                  PRTOPEN prfile,"",spoolname,spoolfile=splfle
.                              if (osflag = c2)         .nt
.                                  PRTOPEN prfile,"@Kyocera FS1030D",spoolname,noprint,spoolfile=splfle
.                              elseif (osflag = c6)         .XP
.                                      PRTOPEN prfile,"@Kyocera FS1030D",spoolname,noprint,spoolfile=splfle
.                              elseif (osflag = c1)         .win 95 98
.                                      PRTOPEN prfile,"@KYOCERAS",spoolname,noprint,spoolfile=splfle
.                              else   .(osflag = c0)         .Don't know prompt for printer
.                                      PRTOPEN prfile,"@",spoolname,noprint,spoolfile=splfle
.                              endif

                              goto   prtnext
                    Elseif    (cntprint = "7")      .
.                              PRTOPEN prfile,"Kyocera Mita FS-C5016N on vistatest",spoolname,noprint,spoolfile=splfle
                              PRTOPEN prfile,"Kyocera FS-C5030N (KPDL) on vistatest",spoolname,noprint,spoolfile=splfle
                                    if (osflag = c2 or Osflag = c6)         .nt +
                                            PRTOPEN prfile,"Kyocera FS-C5030N (KPDL) on vistatest",spoolname,noprint,spoolfile=splfle
                                    elseif (osflag = c1)         .win 95 98
                                            PRTOPEN prfile,"@KYOCERAM",spoolname,noprint,spoolfile=splfle
                                    else   .(osflag = c0)         .Don't know prompt for printer
                                            PRTOPEN prfile,"@",spoolname,noprint,spoolfile=splfle
                                    endif
                              goto   prtnext               .KyoceraM
.                 endif 
                              else
.Patch 4.02.2
                    
                if (cntprint = "1" | cntprint = "3")      .Laser6


                        if (osflag = c1 | Osflag = C5 | osflag = c6)         .nt win2k Xp
                                PRTOPEN prfile,"\\srv2008a\Laser6",spoolname,noprint,spoolfile=splfle
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN Prfile,"Laser6",spoolname,noprint,spoolfile=splfle
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN PRfile,"-",spoolname,noprint,spoolfile=splfle
                        endif

.                        if (bosflag = c2)
.                              PRTOPEN prfile,"\\SRV2008a\laser6",spoolname,noprint,spoolfile=splfle
..                             PRTOPEN prfile,"\\SRV2008a\laser6","Listsum",noprint,spoolfile="c:\work\listsum.lst"
.                        else
.                              PRTOPEN prfile,"Laser6",spoolname,noprint,spoolfile=splfle
..                             PRTOPEN prfile,"Laser6","Listsum",noprint,spoolfile="c:\work\listsum.lst"
.                        endif
                else                                    .Laser3 = Default
                        if (osflag = c1 | Osflag = C5 | osflag = c6)         .nt win2k Xp
                                PRTOPEN prfile,"\\SRV2008a\laser3",spoolname,noprint,spoolfile=splfle
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN Prfile,"Laser3",spoolname,noprint,spoolfile=splfle
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN PRfile,"-",spoolname,noprint,spoolfile=splfle
                        endif

.                        if (bosflag = c2)
.                              PRTOPEN prfile,"\\SRV2008a\laser3 Blankstock",spoolname,noprint,spoolfile=splfle
..                             PRTOPEN prfile,"\\SRV2008a\laser3 Blankstock","Listsum",noprint,spoolfile="c:\work\listsum.lst"
.                        else
.                                  PRTOPEN prfile,"Laser3 Blankstock",spoolname,noprint,spoolfile=splfle
..                                 PRTOPEN prfile,"Laser3 Blankstock","Listsum",noprint,spoolfile="c:\work\listsum.lst"
.                        endif
.Patch 4.02.2
                            endif        
.Patch 4.02.2
                endif
.Patch 3.99
                                        endif
prtnext
                          prtpage prfile;*UNITS=*HIENGLISH:
                                        *ORIENT=*LANDSCAPE;
.Patch 3.99
.===========================================================================
        clear PgCnt
PAGE
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
        if (pgcnt = c1)
          prtpage prfile;*UNITS=*HIENGLISH:
                       *ORIENT=*LANDSCAPE;        
        else
          prtpage prfile;*NEWPAGE:
                 *UNITS=*HIENGLISH:
                       *ORIENT=*LANDSCAPE;
          endif                       
        clear   row
        move    "300",row
        prtpage prfile;*pcolumn:row,*font=font12,"Confidential";
.START PATCH 3.98 REPLACED LOGIC
.        prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News California Inc";
        prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News";
.END PATCH 3.98 REPLACED LOGIC
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
.        prtpage prfile;*pTitle1:mlrrow,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,str55,*boldoff;
.       prtpage prfile;*pTitle1:mlrrow,*ALIGNMENT=*Left,*font=font12,*boldon,"Mailer: ",*boldoff;
.       prtpage prfile;*font=font12,*boldon,*ll,holdnmlr,*boldoff;
.===========
       add     eightlpi,row
       add     eightlpi,row
       add     eightlpi,row
       add     eightlpi,row
       prtpage prfile;*pcolumn:row,*font=font12,*boldon,*ULON,"List Name",*ULOFF,*boldoff;
        If (Mode = C2)
                goto Calendar
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
.Update
.Patch 4.01
.       READ      LISTFLE,SEQ;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01:
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06
.Patch 4.01                   
.Patch 4.02
       READ      LISTFLE,SEQ;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01:
.begin patch 4.03                   
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07
.end patch 4.03                   
.begin patch 4.05
                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08:
.begin patch 4.06
.                   o09,x09,r09
                   o09,x09,r09,o10,x10,r10
.begin patch 4.06
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07
.end patch 4.05
.Patch 4.02                   
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
.UPdate
.Patch 4.01
.                   ADD o00 to lstTot
.                   ADD o01 to lstTot
.                   ADD o02 to lstTot
.                   ADD o03 to lstTot
.                   ADD o04 to lstTot
.                   ADD o05 to lstTot
.Patch 4.01                   
.Patch 4.02
.Begin  Patch 4.03
.                   ADD o01 to lstTot
.                   ADD o02 to lstTot
.                   ADD o03 to lstTot
.                   ADD o04 to lstTot
.                   ADD o05 to lstTot
.                   ADD o06 to lstTot
.Patch 4.02                   
.                    ADD o02 to lstTot
.                    ADD o03 to lstTot
.                    ADD o04 to lstTot
.                    ADD o05 to lstTot
.                    ADD o06 to lstTot
.                    ADD o07 to lstTot
.End Patch 4.03               
.Begin  Patch 4.05
.                   ADD o01 to lstTot
.                   ADD o02 to lstTot
.                   ADD o03 to lstTot
.                   ADD o04 to lstTot
.                   ADD o05 to lstTot
.                   ADD o06 to lstTot
.Patch 4.02                   
.begin patch 4.06
.                    ADD o03 to lstTot
                    ADD o04 to lstTot
                    ADD o05 to lstTot
                    ADD o06 to lstTot
                    ADD o07 to lstTot
                    ADD o08 to lstTot
                    ADD o09 to lstTot
.end patch 4.06
.End Patch 4.05               
                    if (lstTot > c0)                      .if list total is 0 for all years skip
                              ADD lstTot to TotOrd
                              goto TotOrd
                else
                        goto Row2
                    endif
       else
.Calendar
.Update
.Patch 4.01
.                   add o02 to lstTot
.                   add o03 to lstTot
.                   add o04 to lstTot
.                   add o05 to lstTot
.                   add o06 to lstTot
.Patch 4.01
.Patch 4.02
.Begin Patch 4.03
.                   add o03 to lstTot
.                   add o04 to lstTot
.                   add o05 to lstTot
.                   add o06 to lstTot
.                   add o07 to lstTot
.Patch 4.02

.                    add o04 to lstTot
.                    add o05 to lstTot
.                    add o06 to lstTot
.                    add o07 to lstTot
.                    add o08 to lstTot
.End Patch 4.03
.Begin Patch 4.05
.                   add o03 to lstTot
.                   add o04 to lstTot
.                   add o05 to lstTot
.                   add o06 to lstTot
.                   add o07 to lstTot
.Patch 4.02

.begin patch 4.06
.                    add o05 to lstTot
                    add o06 to lstTot
                    add o07 to lstTot
                    add o08 to lstTot
                    add o09 to lstTot
                    add o10 to lstTot
.end patch 4.06
.End Patch 4.05
                    if (lstTot > c0)
                              ADD lstTot to TotOrd
                else
                        goto Row2
                endif
        Endif
TotOrd
.Total Orders Yearly

        If (MODE = C1)
.Patch 4.01        
.Update
.                ADD o00 to OYR96
.                ADD o01 to OYR97
.                ADD o02 to OYR98
.                ADD o03 to OYR99
.                ADD o04 to OYR00
.                ADD o05 to OYR01
.Patch 4.01                
.Patch 4.02
.Update
.begin patch 4.03
.                ADD o01 to OYR96
.                ADD o02 to OYR97
.                ADD o03 to OYR98
.                ADD o04 to OYR99
.                ADD o05 to OYR00
.                ADD o06 to OYR01
.Patch 4.02
.                 ADD o02 to OYR96
.                 ADD o03 to OYR97
.                 ADD o04 to OYR98
.                 ADD o05 to OYR99
.                 ADD o06 to OYR00
.                 ADD o07 to OYR01
.end Patch 4.03            
.begin patch 4.05
.                ADD o01 to OYR96
.                ADD o02 to OYR97
.                ADD o03 to OYR98
.                ADD o04 to OYR99
.                ADD o05 to OYR00
.                ADD o06 to OYR01
.Patch 4.02
.begin patch 4.06
.                 ADD o03 to OYR96
.                 ADD o04 to OYR97
.                 ADD o05 to OYR98
.                 ADD o06 to OYR99
.                 ADD o07 to OYR00
.                 ADD o08 to OYR01
                 ADD o04 to OYR96
                 ADD o05 to OYR97
                 ADD o06 to OYR98
                 ADD o07 to OYR99
                 ADD o08 to OYR00
                 ADD o09 to OYR01
.end patch 4.06
.end Patch 4.05    
                 goto Exchg
        else
.Update
.Patch 4.01
.                ADD o02 to OYR98
.                ADD o03 to OYR99
.                ADD o04 to OYR00
.                ADD o05 to OYR01
.                ADD o06 to OYR02
.Patch 4.01                
.Patch 4.02
.Begin Patch 4.03
.                ADD o03 to OYR98
.                ADD o04 to OYR99
.                ADD o05 to OYR00
.                ADD o06 to OYR01
.                ADD o07 to OYR02
.Patch 4.02                
.                 ADD o04 to OYR98
.                 ADD o05 to OYR99
.                 ADD o06 to OYR00
.                 ADD o07 to OYR01
.                 ADD o08 to OYR02
.end Patch 4.03            
.Begin Patch 4.05
.                ADD o03 to OYR98
.                ADD o04 to OYR99
.                ADD o05 to OYR00
.                ADD o06 to OYR01
.                ADD o07 to OYR02
.Patch 4.02                
.begin patch 4.06
.                 ADD o05 to OYR98
.                 ADD o06 to OYR99
.                 ADD o07 to OYR00
.                 ADD o08 to OYR01
.                 ADD o09 to OYR02
                 ADD o06 to OYR98
                 ADD o07 to OYR99
                 ADD o08 to OYR00
                 ADD o09 to OYR01
                 ADD o10 to OYR02
.end patch 4.06
.end Patch 4.05          
          endif
.Exchange Total-yearly
Exchg
        If (MODE = C1)
.Update
.Patch 4.01
.                ADD X00 to XYR96
.                ADD X01 to XYR97
.                ADD X02 to XYR98
.                ADD X03 to XYR99
.                ADD X04 to XYR00
.                ADD X05 to XYR01
.Patch 4.01                
.Patch 4.02
.begin patch 4.03
.                ADD X01 to XYR96
.                ADD X02 to XYR97
.                ADD X03 to XYR98
.                ADD X04 to XYR99
.                ADD X05 to XYR00
.                ADD X06 to XYR01
.Patch 4.02  
.                 ADD X02 to XYR96
.                 ADD X03 to XYR97
.                 ADD X04 to XYR98
.                 ADD X05 to XYR99
.                 ADD X06 to XYR00
.                 ADD X07 to XYR01
.end patch 4.03
.begin patch 4.05
.begin patch 4.06
.                 ADD X03 to XYR96
.                 ADD X04 to XYR97
.                 ADD X05 to XYR98
.                 ADD X06 to XYR99
.                 ADD X07 to XYR00
.                 ADD X08 to XYR01
                 ADD X05 to XYR96
                 ADD X06 to XYR97
                 ADD X07 to XYR98
                 ADD X08 to XYR99
                 ADD X09 to XYR00
                 ADD X09 to XYR01
.end patch 4.06
.end patch 4.05
               goto Rent1
        endif
.Update
.Patch 4.01
.        ADD X02 to XYR98
.        ADD X03 to XYR99
.        ADD X04 to XYR00
.        ADD X05 to XYR01
.        ADD X06 to XYR02
.Patch 4.01        
.begin Patch 4.03
.        ADD X03 to XYR98
.        ADD X04 to XYR99
.        ADD X05 to XYR00
.        ADD X06 to XYR01
.        ADD X07 to XYR02
.Patch 4.01
.        ADD X04 to XYR98
.        ADD X05 to XYR99
.        ADD X06 to XYR00
.        ADD X07 to XYR01
.        ADD X08 to XYR02
.end Patch 4.03
.begin Patch 4.05
.        ADD X03 to XYR98
.        ADD X04 to XYR99
.        ADD X05 to XYR00
.        ADD X06 to XYR01
.        ADD X07 to XYR02
.Patch 4.01
.begin patch 4.06
.        ADD X05 to XYR98
.        ADD X06 to XYR99
.        ADD X07 to XYR00
.        ADD X08 to XYR01
.        ADD X09 to XYR02
        ADD X06 to XYR98
        ADD X07 to XYR99
        ADD X08 to XYR00
        ADD X09 to XYR01
        ADD X10 to XYR02
.end patch 4.06
.end Patch 4.05
Rent1
.Rental Total-yearly
        If (MODE = C1)
.Update
.Patch 4.01
.                ADD R00 to RYR96
.                ADD R01 to RYR97
.                ADD R02 to RYR98
.                ADD R03 to RYR99
.                ADD R04 to RYR00
.                ADD R05 to RYR01
.Patch 4.01                
.Patch 4.02
.Begin patch 4.03
.                ADD R01 to RYR96
.                ADD R02 to RYR97
.                ADD R03 to RYR98
.                ADD R04 to RYR99
.                ADD R05 to RYR00
.                ADD R06 to RYR01
.Patch 4.02                
.                 ADD R02 to RYR96
.                 ADD R03 to RYR97
.                 ADD R04 to RYR98
.                 ADD R05 to RYR99
.                 ADD R06 to RYR00
.                 ADD R07 to RYR01
.end patch 4.03
.Begin patch 4.05
.                ADD R01 to RYR96
.                ADD R02 to RYR97
.                ADD R03 to RYR98
.                ADD R04 to RYR99
.                ADD R05 to RYR00
.                ADD R06 to RYR01
.Patch 4.02                
.begin patch 4.06
.                 ADD R03 to RYR96
.                 ADD R04 to RYR97
.                 ADD R05 to RYR98
.                 ADD R06 to RYR99
.                 ADD R07 to RYR00
.                 ADD R08 to RYR01
                 ADD R04 to RYR96
                 ADD R05 to RYR97
                 ADD R06 to RYR98
                 ADD R07 to RYR99
                 ADD R08 to RYR00
                 ADD R09 to RYR01
.end patch 4.06
.end patch 4.05
                 goto Print
       endif
.Update
.Patch 4.01
.       ADD R02 to RYR98
.       ADD R03 to RYR99
.       ADD R04 to RYR00
.       ADD R05 to RYR01
.       ADD R06 to RYR02
.Patch 4.01       
.Patch 4.02
.begin Patch 4.03
.       ADD R03 to RYR98
.       ADD R04 to RYR99
.       ADD R05 to RYR00
.       ADD R06 to RYR01
.       ADD R07 to RYR02
.Patch 4.02       
.       ADD R04 to RYR98
.       ADD R05 to RYR99
.       ADD R06 to RYR00
.       ADD R07 to RYR01
.       ADD R08 to RYR02
.end Patch 4.03
.begin Patch 4.05
.       ADD R03 to RYR98
.       ADD R04 to RYR99
.       ADD R05 to RYR00
.       ADD R06 to RYR01
.       ADD R07 to RYR02
.Patch 4.02       
.begin Patch 4.06
.       ADD R05 to RYR98
.       ADD R06 to RYR99
.       ADD R07 to RYR00
.       ADD R08 to RYR01
.       ADD R09 to RYR02
       ADD R06 to RYR98
       ADD R07 to RYR99
       ADD R08 to RYR00
       ADD R09 to RYR01
       ADD R10 to RYR02
.end Patch 4.06
.end Patch 4.05
Print
       add     eightlpi,row
       add     eightlpi,row
       UNPACK KEY80,str45,str35
       prtpage prfile;*pcolumn:row,*font=font7,Str35;
        If (MODE = C1)
.==================================================
.Update
.>Patch 4.0 End Code Commented Out
.       if (O99 > c0)
.                   clear str10
.                   move O99 to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.                prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,O99;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified

        move mask15 to dim15a
.Patch 4.02
        edit O00 to dim15a
.Patch 4.02
.Patch 4.01        
        edit O01 to dim15a
.Patch 4.01        
          call trim using     dim15a
          prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.>Patch 4.0 End Code Modified        
.Update
.>Patch 4.0 End Code Commented Out
.       if (O00 > c0)
.                   clear str10
.                   move O00 to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                 prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.                 prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,O00;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
.Patch 4.02
.        edit O01 to dim15a
.Patch 4.02        
.Patch 4.01        
.begin Patch 4.03
.        edit O02 to dim15a
.Patch 4.01        
.        edit O03 to dim15a
.end Patch 4.03
.begin Patch 4.05
.        edit O02 to dim15a
.Patch 4.01        
.begin Patch 4.06
.        edit O04 to dim15a
        edit O05 to dim15a
.end Patch 4.06
.end Patch 4.05
          call trim using     dim15a
          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.>Patch 4.0 End Code Modified          
.Update
.>Patch 4.0 End Code Commented Out
.       if (O01 > c0)
.                   clear str10
.                   move O01 to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                 prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.                 prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,O01;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
.Patch 4.01        
.        edit O02 to dim15a
.Patch 4.01        
.Patch 4.02        
.begin Patch 4.03
.        edit O03 to dim15a
.Patch 4.02        
.        edit O04 to dim15a
.end Patch 4.03
.begin Patch 4.05
.        edit O03 to dim15a
.Patch 4.02        
.begin Patch 4.06
.        edit O05 to dim15a
        edit O06 to dim15a
.end Patch 4.06
.end Patch 4.05
          call trim using     dim15a
          prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.>Patch 4.0 End Code Modified                 
.Update
.>Patch 4.0 End Code Commented Out
.       if (O02 > c0)
.                   clear str10
.                   move O02 to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                 prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.                 prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,O02;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
.Patch 4.01        
.        edit O03 to dim15a
.Patch 4.01        
.Patch 4.02
.begin Patch 4.03
.        edit O04 to dim15a
.        edit O05 to dim15a
.end Patch 4.03
.begin Patch 4.05
.        edit O04 to dim15a
.begin Patch 4.06
.        edit O06 to dim15a
        edit O07 to dim15a
.end Patch 4.06
.end Patch 4.5
.Patch 4.02        
          call trim using     dim15a
          prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.>Patch 4.0 End Code Modified         
.Update
.>Patch 4.0 End Code Commented Out
.       if (O03 > c0)
.                   clear str10
.                   move O03 to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                 prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.                 prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,O03;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
.Patch 4.01        
.        edit O04 to dim15a
.Patch 4.01        
.Patch 4.02
.begin Patch 4.03
.        edit O05 to dim15a
.        edit O06 to dim15a
.end Patch 4.03
.begin Patch 4.03
.        edit O05 to dim15a
.begin Patch 4.06
.        edit O07 to dim15a
        edit O08 to dim15a
.end Patch 4.06
.end Patch 4.05
.Patch 4.02        
          call trim using     dim15a
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.>Patch 4.0 End Code Modified         
.Update
.>Patch 4.0 End Code Commented Out
.       if (O04 > c0)
.                   clear str10
.                   move O04 to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                 prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.                 prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,O04;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
.Patch 4.01
.        edit O05 to dim15a
.Patch 4.01
.Patch 4.02
.begin Patch 4.03
.        edit O06 to dim15a
.        edit O07 to dim15a
.end Patch 4.03
.begin Patch 4.05
.        edit O05 to dim15a
.begin Patch 4.06
.        edit O08 to dim15a
        edit O09 to dim15a
.end Patch 4.06
.end Patch 4.05
.Patch 4.02        
          call trim using     dim15a
          prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.>Patch 4.0 End Code Modified          
.>Patch 4.0 End Code Commented Out
.       if (lstTot > c0)
.                   clear str10
.                   move lstTot to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                 prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,Str13;
.       else
.                   prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,lstTot;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit lstTot to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.>Patch 4.0 End Code Modified          
       goto Row
    Endif
.Update
.>Patch 4.0 End Code Commented Out
.       if (O01 > c0)
.                   clear str10
.                   move O01 to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                 prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.                 prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,O01;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
.Patch 4.01
.        edit O02 to dim15a
.Patch 4.01        
.Patch 4.02
.begin Patch 4.03
.        edit O03 to dim15a
.        edit O04 to dim15a
.end Patch 4.03
.begin Patch 4.05
.        edit O03 to dim15a
.begin Patch 4.06
.        edit O05 to dim15a
        edit O06 to dim15a
.begin Patch 4.06
.end Patch 4.05
.Patch 4.02        
          call trim using     dim15a
          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.>Patch 4.0 End Code Modified        
.Update
.>Patch 4.0 End Code Commented Out
.       if (O02 > c0)
.                   clear str10
.                   move O02 to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.                prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,O02;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
.Patch 4.01        
.        edit O03 to dim15a
.Patch 4.01        
.Patch 4.02
.begin Patch 4.03
.        edit O04 to dim15a
.        edit O05 to dim15a
.end Patch 4.03
.begin Patch 4.05
.        edit O04 to dim15a
.begin Patch 4.06
.        edit O06 to dim15a
        edit O07 to dim15a
.end Patch 4.06
.end Patch 4.05
.Patch 4.02        
          call trim using     dim15a
          prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.>Patch 4.0 End Code Modified        
.Update
.>Patch 4.0 End Code Commented Out
.       if (O03 > c0)
.                   clear str10
.                   move O03 to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                 prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.                 prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,O03;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
.Patch 4.01        
.        edit O04 to dim15a
.Patch 4.01        
.Patch 4.02        
.begin Patch 4.03
.        edit O05 to dim15a
.        edit O06 to dim15a
.end Patch 4.03
.begin Patch 4.05
.        edit O05 to dim15a
.begin Patch 4.06
.        edit O07 to dim15a
        edit O08 to dim15a
.end Patch 4.06
.end Patch 4.05
.Patch 4.02        
          call trim using     dim15a
          prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.>Patch 4.0 End Code Modified         
.Update
.>Patch 4.0 End Code Commented Out
.       if (O04 > c0)
.                   clear str10
.                   move O04 to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                 prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.                 prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,O04;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
.Patch 4.01        
.        edit O05 to dim15a
.Patch 4.01        
.Patch 4.02        
.begin Patch 4.03
.        edit O06 to dim15a
.        edit O07 to dim15a
.end Patch 4.03
.begin Patch 4.05
.        edit O06 to dim15a
.begin Patch 4.06
.        edit O08 to dim15a
        edit O09 to dim15a
.end Patch 4.06
.end Patch 4.05
.Patch 4.02        
          call trim using     dim15a
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.>Patch 4.0 End Code Modified         
.Update
.>Patch 4.0 End Code Commented Out
.       if (O05 > c0)
.                   clear str10
.                   move O05 to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.                 prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,O05;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
.Patch 4.01        
.        edit O06 to dim15a
.Patch 4.01        
.Patch 4.02
.begin Patch 4.03
.        edit O07 to dim15a
.        edit O08 to dim15a
.end Patch 4.03
.begin Patch 4.05
.        edit O07 to dim15a
.begin Patch 4.06
.        edit O09 to dim15a
        edit O10 to dim15a
.end Patch 4.06
.end Patch 4.05
.Patch 4.02        
          call trim using     dim15a
          prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.>Patch 4.0 End Code Modified  
.>Patch 4.0 End Code Commented Out
.       if (lstTot > c0)
.                   clear str10
.                   move lstTot to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                 prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.                 prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,lstTot;
.       Endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit lstTot to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.>Patch 4.0 End Code Modified  
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
.Update
.Patch 4.02
.                             "�","1992-2006, Names in the News";
.begin patch 4.03
.                             "�","1992-2007, Names in the News";
.                              "�","1992-2008, Names in the News";
.end patch 4.03
.begin patch 4.05
.                             "�","1992-2007, Names in the News";
.begin patch 4.06
                              "�","1992-2015, Names in the News";
.end patch 4.06
.end patch 4.05
.Patch 4.02
.                             "�","1992-2004, Names in the News/CA";
.End Patch 1.1a
           goto Page
       endif
.update
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
       clear    O05
       clear    X05
       clear    R05
.Patch 4.01
       clear    O06
       clear    X06
       clear    R06
.Patch 4.01
.Patch 4.02
       clear    O07
       clear    X07
       clear    R07
.Patch 4.02
.begin patch 4.03
       clear    O08
       clear    X08
       clear    R08
.end patch 4.03
.begin patch 4.05
       clear    O09
       clear    X09
       clear    R09
.end patch 4.05
.begin patch 4.06
       clear    O10
       clear    X10
       clear    R10
.end patch 4.06
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
.Patch 4.02
.                             "�","1992-2006, Names in the News";
.begin patch 4.03
.                             "�","1992-2007, Names in the News";
.                              "�","1992-2008, Names in the News";
.end patch 4.03
.begin patch 4.05
.                             "�","1992-2007, Names in the News";
.begin patch 4.06
                              "�","1992-2010, Names in the News";
.enD patch 4.06
.end patch 4.05
.Patch 4.02
.                             "�","1992-2004, Names in the News/CA";
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
.START PATCH 3.98 REPLACED LOGIC
.                 prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News California Inc";
                  prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News";
.END PATCH 3.98 REPLACED LOGIC
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
.>Patch 4.0 End Code Commented Out
.       if (OYR96 > c0)
.         clear str10
.         move OYR96 to Str10
.         clear str13
.         call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR96,*ULOFF,*boldoff;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit OYR96 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
.>Patch 4.0 End Code Modified        
.>Patch 4.0 End Code Commented Out
.       if (OYR97 > c0)
.         clear str10
.         move OYR97 to Str10
.         clear str13
.         call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR97,*ULOFF,*boldoff;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit OYR97 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
.>Patch 4.0 End Code Modified          
.>Patch 4.0 End Code Commented Out
.       if (OYR98 > c0)
.         clear str10
.         move OYR98 to Str10
.         clear str13
.         call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR98,*ULOFF,*boldoff;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit OYR98 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
.>Patch 4.0 End Code Modified         
.>Patch 4.0 End Code Commented Out
.       if (OYR99 > c0)
.         clear str10
.         move OYR99 to Str10
.         clear str13
.         call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR99,*ULOFF,*boldoff;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit OYR99 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
.>Patch 4.0 End Code Modified          
.>Patch 4.0 End Code Commented Out
.       if (OYR00 > c0)
.         clear str10
.         move OYR00 to Str10
.         clear str13
.         call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR00,*ULOFF,*boldoff;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit OYR00 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
.>Patch 4.0 End Code Modified         
.>Patch 4.0 End Code Commented Out
.       if (OYR01 > c0)
.         clear str10
.         move OYR01 to Str10
.         clear str13
.         call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR01,*ULOFF,*boldoff;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit OYR01 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
.>Patch 4.0 End Code Modified         
       goto Next
    endif
.==========================================================================
.Year Totals-Calendar
.>Patch 4.0 End Code Commented Out
.       if (OYR98 > c0)
.         clear str10
.         move OYR98 to Str10
.         clear str13
.         call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR98,*ULOFF,*boldoff;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit OYR98 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
.>Patch 4.0 End Code Modified  
.>Patch 4.0 End Code Commented Out
.       if (OYR99 > c0)
.         clear str10
.         move OYR99 to Str10
.         clear str13
.         call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR99,*ULOFF,*boldoff;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit OYR99 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
.>Patch 4.0 End Code Modified 
.>Patch 4.0 End Code Commented Out
.       if (OYR00 > c0)
.         clear str10
.         move OYR00 to Str10
.         clear str13
.         call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR00,*ULOFF,*boldoff;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit OYR00 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
.>Patch 4.0 End Code Modified 
.>Patch 4.0 End Code Commented Out
.       if (OYR01 > c0)
.         clear str10
.         move OYR01 to Str10
.         clear str13
.         call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR01,*ULOFF,*boldoff;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit OYR01 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
.>Patch 4.0 End Code Modified 
.>Patch 4.0 End Code Commented Out
.       if (OYR02 > c0)
.         clear str10
.         move OYR02 to Str10
.         clear str13
.         call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR02,*ULOFF,*boldoff;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit OYR02 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
.>Patch 4.0 End Code Modified        

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
.>Patch 4.0 End Code Commented Out
.       if (OTOT > c0)
.        clear str10
.        move OTOT to Str10
.        clear str13
.        call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OTOT,*ULOFF,*boldoff;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit OTOT to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
.>Patch 4.0 End Code Modified        
       add     eightlpi,row
       add     eightlpi,row
       prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ULON,"Exchange Totals",*ULOFF,*boldoff;
       If (MODE = C1)
.=========================================================
.>Patch 4.0 End Code Commented Out
.        if (XYR96 > c0)
.              clear str10
.               move XYR96 to Str10
.             clear str13
.              call FormatNumeric using str10,str13,comma
.              prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.        else
.              prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR96,*ULOFF;
.        endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit XYR96 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified          
.>Patch 4.0 End Code Commented Out
.        if (XYR97 > c0)
.              clear str10
.             move XYR97 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR97,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit XYR97 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified         
.>Patch 4.0 End Code Commented Out
.       if (XYR98 > c0)
.             clear str10
.             move XYR98 to Str10
.             clear str13
.             call  FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,OYR98,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit XYR98 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified        
.>Patch 4.0 End Code Commented Out
.       if (XYR99 > c0)
.             clear str10
.             move XYR99 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR99,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit XYR99 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified         
.>Patch 4.0 End Code Commented Out
.       if (XYR00 > c0)
.             clear str10
.             move XYR00 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR00,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit XYR00 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified        
.>Patch 4.0 End Code Commented Out
.       if (XYR01 > c0)
.             clear str10
.             move XYR01 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR01,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit XYR01 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified          
       goto Xyear
   Endif
.====================================================
.>Patch 4.0 End Code Commented Out
.       if (XYR98 > c0)
.             clear str10
.              move XYR98 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.              prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,OYR98,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit XYR98 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified          
.>Patch 4.0 End Code Commented Out
.       if (XYR99 > c0)
.             clear str10
.             move XYR99 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR99,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit XYR99 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified 
.>Patch 4.0 End Code Commented Out
.       if (XYR00 > c0)
.             clear str10
.             move XYR00 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR00,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit XYR00 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified 
.>Patch 4.0 End Code Commented Out
.       if (XYR01 > c0)
.             clear str10
.             move XYR01 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR01,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit XYR01 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified       
.>Patch 4.0 End Code Commented Out
.       if (XYR02 > c0)
.             clear str10
.             move XYR02 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR02,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit XYR02 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified         
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
.>Patch 4.0 End Code Commented Out
.       if (XTOT > c0)
.             clear str10
.             move XTOT to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.        else
.             prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,XTOT,*ULOFF,*boldoff;
.        endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit XTOT to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF,*boldoff;
.>Patch 4.0 End Code Modified           
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*ULON,"Rental Totals",*ULOFF,*boldoff;
        If (MODE = C1)
.>Patch 4.0 End Code Commented Out        
.             if (RYR96 > c0)
.                   clear str10
.                   move RYR96 to Str10
.                   clear str13
.                   call FormatNumeric using str10,str13,comma
.                   prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.             else
.                   prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR96,*ULOFF;
.              endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit RYR96 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified                
.>Patch 4.0 End Code Commented Out
.         if (RYR97 > c0)
.             clear str10
.              move RYR97 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.         else
.             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR97,*ULOFF;
.         endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit RYR97 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified           
.>Patch 4.0 End Code Commented Out
.         if (RYR98 > c0)
.             clear str10
.             move RYR98 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.         else
.             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR98,*ULOFF;
.         endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit RYR98 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified              
.>Patch 4.0 End Code Commented Out
.         if (RYR99 > c0)
.             clear str10
.              move RYR99 to Str10
.             clear str13
..             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.         else
.             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR99,*ULOFF;
.         endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit RYR99 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified           
.>Patch 4.0 End Code Commented Out
.         if (RYR00 > c0)
.             clear str10
.             move RYR00 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.         else
.             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR00,*ULOFF;
.         endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit RYR00 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified           
.>Patch 4.0 End Code Commented Out
.         if (RYR01 > c0)
.             clear str10
.             move RYR01 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.         else
.             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR01,*ULOFF;
.         Endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit RYR01 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified           
         goto Rent2
       ENDIF
.>Patch 4.0 End Code Commented Out       
.       if (RYR98 > c0)
.             clear str10
.             move RYR98 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR98,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit RYR98 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified          
.>Patch 4.0 End Code Commented Out
.       if (RYR99 > c0)
.             clear str10
.             move RYR99 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR99,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit RYR99 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified         
.>Patch 4.0 End Code Commented Out
.       if (RYR00 > c0)
.             clear str10
.             move RYR00 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR00,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit RYR00 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified         
.>Patch 4.0 End Code Commented Out
.       if (RYR01 > c0)
.             clear str10
.             move RYR01 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR01,*ULOFF;
.       Endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit RYR01 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified        
.>Patch 4.0 End Code Commented Out
.       if (RYR02 > c0)
.             clear str10
.             move RYR02 to Str10
.             clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.             prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR02,*ULOFF;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit RYR02 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified           
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
.>Patch 4.0 End Code Commented Out
.       if (RTOT > c0)
.              clear str10
.              move RTOT to Str10
.              clear str13
.             call FormatNumeric using str10,str13,comma
.             prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.             prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,RTOT,*ULOFF,*boldoff;
.       endif
.>Patch 4.0 End Code Commented Out
.>Patch 4.0 Code Modified
        move mask15 to dim15a
        edit RTOT to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.>Patch 4.0 End Code Modified        
.Patch 1.1
.update
       move "7750",row
       prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
       prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
       prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
.Patch 4.02
.                             "�","1992-2006, Names in the News";
.begin patch 4.03
.                             "�","1992-2007, Names in the News";
.                              "�","1992-2008, Names in the News";
.end patch 4.03
.begin patch 4.05
.                             "�","1992-2007, Names in the News";
.begin patch 4.06
                              "�","1992-2010, Names in the News";
.enD patch 4.06
.end patch 4.05
.Patch 4.02
.                             "�","1992-2004, Names in the News/CA";
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
.        if (bosflag = c2)
        if (osflag = c2)
                goto looper2
        endif
.==============================================================
.Printer Area Test
Looper1
.patch3.93
        if (pdfopt = YES)
                  goto pdfthis
        endif
.patch3.93
        loop
          until (N3 = COPY)
.Patch 4.02.2
                    if (cntprint = "5")      .Susan
.                              PRTPLAY splfle,"-"
.                              PRTPLAY splfle," "
.                              PRTPLAY splfle,"Kyocera FS1030D"
                    goto   checker
                    Elseif (cntprint = "7")      .
                              PRTPLAY splfle,"Kyocera FS-C5030N (KPDL) on vistatest"
                    goto   checker
                        else
.Patch 4.02.2
                if (cntprint = "1" | cntprint = "3")      .Laser6
                        if (osflag = c1 | Osflag = C5 | osflag = c6)         .nt win2k Xp
                              PRTPLAY splfle,"\\SRV2008a\laser6"
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                              PRTPLAY splfle,"\Laser6"
                        else   .(osflag = c0)         .Don't know prompt for printer
                              PRTPLAY splfle,"-"
                        endif
.                              PRTPLAY splfle,"Laser6"
..                             PRTPLAY "c:\work\listsum.lst","Laser6"
                else
                        if (osflag = c1 | Osflag = C5 | osflag = c6)         .nt win2k Xp
                              PRTPLAY splfle,"\\SRV2008a\laser3"
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                              PRTPLAY splfle,"\Laser3"
                        else   .(osflag = c0)         .Don't know prompt for printer
                              PRTPLAY splfle,"-"
                        endif
.                              PRTPLAY splfle,"Laser3 Blankstock"
.                             PRTPLAY "c:\work\listsum.lst","Laser3 Blankstock"
.Patch 4.02.2
                        endif
.Patch 4.02.2
                endif
                add c1 to N3
        repeat
checker
        goto checknew
.================================================================
Looper2
.patch3.93
        if (pdfopt = YES)
                  goto pdfthis
        endif
.patch3.93
        loop
          until (N3 = COPY)
.Patch 4.02.2
                    if (cntprint = "5")      .Susan
.                              PRTPLAY splfle,"\\PLI0007\KYOCERAS"
                              goto   checknew
                    Elseif (cntprint = "7")      .
                              PRTPLAY splfle,"Kyocera Mita FS-C5016N on vistatest"
                    goto   checknew
                        else
.Patch 4.02.2
                if (cntprint = "1" | cntprint = "3")      .Laser6
                              PRTPLAY splfle,"\\SRV2008a\laser6"
.                             PRTPLAY "c:\work\listsum.lst","\\SRV2008a\laser6"
                else
                              PRTPLAY splfle,"\\SRV2008a\laser3 Blankstock"
.                             PRTPLAY "c:\work\listsum.lst","\\SRV2008a\laser3 Blankstock"
.Patch 4.02.2
                            endif
.Patch 4.02.2
                endif
                add c1 to N3
        repeat
checknew
       if (newmlr = c1)
.patch3.93
                if (pdfopt <> YES)
                     erase splfle
                endif
.patch3.93
.                erase splfle
..               erase "c:\work\mlrsum.lst"
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
.patch3.93
        if (pdfopt <> YES)
                     erase splfle
         endif
.        erase splfle
.patch3.93
..       erase      "c:\work\mlrsum.lst"
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
         PAUSE c10
.3.93 : added for plbserver runs does not like "\\SRV2008a\laser?"         gets s10 if s10 try this
         Display    *P10:11,*EF,*cyan,"Trying Again"
         Pause c10
         TRAPCLR   SPOOL
         Trap SPOOL2 giving error if SPOOL
         SCAN      "S10" IN ERROR
         if equal
                if (cntprint = "1" | cntprint = "3")      .Laser6
                        if (osflag = c1 | Osflag = C5 | osflag = c6)         .nt win2k Xp
                                PRTOPEN prfile,"\\SRV2008a\laser6",spoolname,noprint,spoolfile=splfle
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN Prfile,"Laser6",spoolname,noprint,spoolfile=splfle
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN PRfile,"-",spoolname,noprint,spoolfile=splfle
                        endif
.                    PRTOPEN prfile,"Laser6",spoolname,noprint,spoolfile=splfle
                else
                        if (osflag = c1 | Osflag = C5 | osflag = c6)         .nt win2k Xp
                                PRTOPEN prfile,"\\SRV2008a\laser3",spoolname,noprint,spoolfile=splfle
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN Prfile,"Laser3",spoolname,noprint,spoolfile=splfle
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN PRfile,"-",spoolname,noprint,spoolfile=splfle
                        endif
.                    PRTOPEN prfile,"Laser3",spoolname,noprint,spoolfile=splfle
                endif
         endif
         return
SPOOL2
         Display    *P10:11,*EF,*cyan,error
         pause      "10"
.3.93
         CLOSE     INFILE
         CLOSE     OUTPUT
         NORETURN
         shutdown  "cls"
         STOP
.================================================
.patch3.93
pdfthis
.Patch 4.01
.Patch 3.99
          pack      APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
          loop
                    call      FindFirstFile
                    until (APIResult = 0 | APIResult = hexeight)
                    pause     "1"
          repeat
          pause     "2"
.          erase     "c:\progra~1\pdf995\flag.dat" 
.Patch 3.99
.Patch 4.01
.Patch 3.99
.Patch 4.01 Comment Out
.                   pack      APIFileName,"c:\work\pdf\mlrsum.pdf",hexzero
.                   call      FindFirstFile
.                   if (APIResult <> 0 & APIResult <> hexeight)
.                             clock   timestamp,timestamp1
.                             move    timestamp1,time1
.                             loop
.                                       move      C0,N1
.                                       trap      IOTrap if IO
.                                       open      tempfile,"c:\work\pdf\mlrsum.pdf"
.                                       trapclr   IO
.                                       if (N1 = C0)
.                                                 close     tempfile
.                                                 break
.                                       endif
.                                       clock   timestamp,timestamp
.                                       move    timestamp,time2
.                                       sub     time1,time2,time3
.                                       if (time3 > 2000) .20 Seconds Maximum
.                                                 break
.                                       endif
.                             repeat
.Patch 4.01 Comment Out
                              move    "Here is your PDF File",MailSubjct
.   Set the text message that is send with the attachments
                              Clear     MailBody
                              move    "mlrsum.pdf",Mailbody
                              Pack      MAilTO from Luser,"@nincal.com"
                              Pack      MAilFrom from Luser,"@nincal.com"
                              Pack      MailAttach from "c:\work\pdf\mlrsum.pdf"
                              call      SendMail
.Clean up afterwards
                              pause     c7
                              erase     "c:\work\pdf\mlrsum.pdf"
.                             endif
..         clear     taskname
..         append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nins1\c\lanbat~2 f:\apps\winbatch\butil job=O34 INfile=",TASKNAME
..         APPEND    spoolname TO TASKNAME
..         APPEND    " B=",TASKNAME
..         APPEND    luser TO TASKNAME
..         reset     taskname
.          alert caution,taskname,result
..         EXECUTE   TASKNAME
.Patch 3.99
         goto checknew

.Patch 3.99
IOTrap
          move      C1,N1
          return
.Patch 3.99
.patch3.93
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
