.*******************************************************************************
.*PURPOSE -   (MLRSUM)
.*            PRODUCE MAILER USAGE SUMMARY REPORT.
.*
.*INPUT   -   SEQ, ORDER RECORDS (diskin pulled as requested).
.*******************************************************************************
PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NDATDD.inc
          include   compdd.inc
          include   cntdd.inc
          include winapi.inc
         include   ncntdd.inc
release  init      "5.06"       DLH     New Year - Yearly Update
reldate   Init      "2016 January 5"
.release  init      "5.05"       DLH     New Year - Yearly Update
.reldate   Init      "2015 January 5"
.release  init      "5.04"          DLH     added Flags= to prtopen with PDF:
.reldate   Init      "2014 March 25"
.release  init      "5.03"       DLH     New Year - Yearly Update
.reldate   Init      "2014 January 2"
.release  init      "5.02"       DLH     New Year - Yearly Update, Sunbelt PDF
.reldate   Init      "2013 May 2"
.release  init      "5.01"       DLH     New Year - Yearly Update
.reldate   Init      "12 December 2011"
.release  init      "5.0"       DLH     New Year - Update rewrite get rid of all the unused data only need 6 years max
.reldate   Init      "03 Jan 2011"
.See archives for previous changes
.RELEASE  INIT      "1.0"       DLH 24FEB92
INPUT    DIM       30
INFILE   FILE      VAR=498
.begin patch 5.0
.OUTPUT   IFILE     KEYLEN=80,FIX=831   .KEY= MAILER NAME + LIST NAME
OUTPUT   IFILE     KEYLEN=80,FIX=261   .KEY= MAILER NAME + LIST NAME
.end patch 5.0
.
HOLDMLR  DIM       7
mask15      init    "(Z,ZZZ,ZZZ,ZZ9)"        .;formatting vars
Dim15a      dim     15            .;formatting vars
c16      FORM      "16"
c17      FORM      "17"
c18      FORM      "18"
c19      FORM      "19"
c21      FORM      "21"
..............
.OUTPUT FILE.
KEY80    DIM       80         KEY VAR.
OMLR     DIM       45         2-46
OLIST    DIM       35        47-81
.begin patch 5.0
Oyr1      FORM      10        82-91      year1 NAMES (MD)
Xyr1      FORM      10        72-101      year1 NAMES (MD)
Ryr1      FORM      10        81-111      year1 NAMES (MD)
Oyr2      FORM      10        91-121      NAMES (MD)
Xyr2      FORM      10       101-131      NAMES (MD)
Ryr2      FORM      10       111-141      NAMES (MD)
Oyr3      FORM      10       121-151      NAMES (Md)
Xyr3      FORM      10       131-161      NAMES (MD)
Ryr3      FORM      10       141-171      NAMES (MD)
Oyr4      FORM      10       151-181      NAMES (Md)
Xyr4      FORM      10       101-191      NAMES (MD)
Ryr4      FORM      10       111-201      NAMES (MD)
Oyr5      FORM      10       121-211      NAMES (Md)
Xyr5      FORM      10       101-221      NAMES (MD)
Ryr5      FORM      10       111-231      NAMES (MD)
Oyr6      FORM      10       121-241      NAMES (Md)
Xyr6      FORM      10       101-251      NAMES (MD)
Ryr6      FORM      10       111-261      NAMES (MD)

.O88      FORM      10        82-91      88 NAMES (MD)
.X88      FORM      10        72-101      88 NAMES (MD)
.R88      FORM      10        81-111      88 NAMES (MD)
.O89      FORM      10        91-121     89  NAMES (MD)
.X89      FORM      10       101-131     89 NAMES (MD)
.R89      FORM      10       111-141     89 NAMES (MD)
.O90      FORM      10       121-151     90 NAMES (Md)
.X90      FORM      10       131-161     90 NAMES (MD)
.R90      FORM      10       141-171     90 NAMES (MD)
.O91      FORM      10       151-181     91 NAMES (Md)
.X91      FORM      10       101-191     91 NAMES (MD)
.R91      FORM      10       111-201     91 NAMES (MD)
.O92      FORM      10       121-211     92 NAMES (Md)
.X92      FORM      10       101-221     92 NAMES (MD)
.R92      FORM      10       111-231     92 NAMES (MD)
.O93      FORM      10       121-241     93 NAMES (Md)
.X93      FORM      10       101-251     93 NAMES (MD)
.R93      FORM      10       111-261     93 NAMES (MD)
.O94      FORM      10       121-271     94 NAMES (Md)
.X94      FORM      10       101-281     94 NAMES (MD)
.R94      FORM      10       111-291     94 NAMES (MD)
.O95      FORM      10       132-301     95 NAMES (MD)
.X95      FORM      10       101-311     95 NAMES (MD)
.R95      FORM      10       111-321     95 NAMES (MD)
.O96      FORM      10       132-331     96 NAMES (MD)
.X96      FORM      10       101-341     96 NAMES (MD)
.R96      FORM      10       111-351     96 NAMES (MD)
.O97      FORM      10       132-361     96 NAMES (MD)
.X97      FORM      10       101-371     96 NAMES (MD)
.R97      FORM      10       111-381     96 NAMES (MD)
.O98      FORM      10                -391
.X98      FORM      10                -401
.R98      FORM      10                -411
.O99      FORM      10                -421
.X99      FORM      10                -431
.R99      FORM      10                -441
.O00      FORM      10          -451           (Md)
.X00      FORM      10          -461           (MD)
.R00      FORM      10          -471           (MD)
.O01      FORM      10          -481          (Md)
.X01      FORM      10          -491           (MD)
.R01      FORM      10          -501           (MD)
.O02      FORM      10          -511           (MD)
.X02      FORM      10          -521           (MD)
.R02      FORM      10          -531           (MD)
.O03      FORM      10          -541           (MD)
.X03      FORM      10          -551           (MD)
.R03      FORM      10          -561           (MD)
.O04      FORM      10          -571          (MD)
.X04      FORM      10          -581           (MD)
.R04      FORM      10          -601           (MD)
..Patch 3.99 Yearly Update
.O05      FORM      10          -610           (MD)
.X05      FORM      10          -621           (MD)
.R05      FORM      10          -631           (MD)
..Patch 3.99 Yearly Update
..Patch 4.01
.O06      FORM      10          -641           (MD)
.X06      FORM      10          -651           (MD)
.R06      FORM      10          -661           (MD)
..Patch 4.01
..Patch 4.02
.O07      FORM      10          -671           (MD)
.X07      FORM      10          -681           (MD)
.R07      FORM      10          -691          (MD)
..Patch 4.02
..Begin Patch 4.03
.O08      FORM      10          -701           (MD)
.X08      FORM      10          -711           (MD)
.R08      FORM      10          -721          (MD)
..End Patch 4.03
..Begin Patch 4.05
.O09      FORM      10          -722           (MD)
.X09      FORM      10          -731           (MD)
.R09      FORM      10          -741          (MD)
..End Patch 4.05
..Begin Patch 4.06
.O10      FORM      10          -752           (MD)
.X10      FORM      10          -761           (MD)
.R10      FORM      10          -771          (MD)
..End Patch 4.06
.end patch 5.0
QUANT    FORM      9
Xquant   form      9
rquant   form      9
TORDQTYR FORM      9
BRANCH   FORM      2
START     FORM     "00"
FISCMO   DIM       2
ORDMO    FORM      2
MODE     FORM      1                    .1=fiscal, 2=calendar
count    form      5
dteflag form      1                     .1=maildate 2=orderdate
.begin patch 5.0
.yr88     init      "88"
.yr89     init      "89"
.yr90     init      "90"
.yr91     init      "91"
.yr92     init      "92"
.yr93     init      "93"
.yr94     init      "94"
.yr95     init      "95"
.yr96     init      "96"
.yr97     init      "97"
.yr98     init      "98"
.yr99     init      "99"
.yr00     init      "00"
.UPDATE...............................................
.begin patch 5.02
.begin patch 5.03
.yr01      init      "07"
.yr02      init      "08"
.yr03      init      "09"
.yr04      init      "10"
.yr05      init      "11"
.yr06      init      "12"
.yr07      init      "13"
.begin patch 5.05
.begin patch 5.06
yr01      init      "10"
yr02      init      "11"
yr03      init      "12"
yr04      init      "13"
yr05      init      "14"
yr06      init      "15"
yr07      init      "16"
.end patch 5.06
.end patch 5.05
.end patch 5.03
.begin patch 5.01
.yr01      init      "06"
.yr02      init      "07"
.yr03      init      "08"
.yr04      init      "09"
.yr05      init      "10"
.yr06      init      "11"
.yr07      init      "12"
.end patch 5.02
.yr01      init      "05"
.yr02      init      "06"
.yr03      init      "07"
.yr04      init      "08"
.yr05      init      "09"
.yr06      init      "10"
.yr07      init      "11"
.end patch 5.01
.yr07      init      "07"
.yr08      init      "08"
.yr09      init      "09"
.yr10      init      "10"
.end patch 5.0
datechk  dim       2
dateyy   dim       2
c12      form      "12"
FUNCBR   FORM      "0"       PROGRAM CONTROL BRANCH.
c14      form      "14"
c15      form      "15"
.==============================================================================
ListFle  file
prfile   pfile
Title1   form    9
Title2   form    9
Title3   form    9
Title4   form    9
mlrrow   form    9
holdNmlr dim    45
.========================================================================================
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
.needed for printing to PDF for non native fonts and things like Copyright to appear correctly in the pdf
PDF_FLAGS_WIN_ANSI_ENCODING EQU 128
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

.begin patch 5.0
.FY1 init "FY 04/05"
.FY2 init "FY 05/06"
.FY3 init "FY 06/07"
.FY4 init "FY 07/08"
.FY5 init "FY 08/09"
.FY6 init "FY 09/10"
.begin patch 5.0
.begin patch 5.02
.begin patch 5.03
.FY1 init "FY 07/08"
.FY2 init "FY 08/09"
.FY3 init "FY 09/10"
.FY4 init "FY 10/11"
.FY5 init "FY 11/12"
.FY6 init "FY 12/13"
.begin patch 5.05..>Update Yearly<<<<<<<<<<<
.begin patch 5.06
FY1 init "FY 11/12"
FY2 init "FY 12/13"
FY3 init "FY 13/14"
FY4 init "FY 14/15"
FY5 init "FY 15/16"
FY6 init "FY 16/17"
.end patch 5.06
.end patch 5.05
.end patch 5.03
.FY1 init "FY 06/07"
.FY2 init "FY 07/08"
.FY3 init "FY 08/09"
.FY4 init "FY 09/10"
.FY5 init "FY 10/11"
.FY6 init "FY 11/12"
.end patch 5.02
.FY1 init "FY 05/06"
.FY2 init "FY 06/07"
.FY3 init "FY 08/09"
.FY4 init "FY 08/09"
.FY5 init "FY 09/10"
.FY6 init "FY 10/11"
.end patch 5.01
.end patch 5.0
.Calendar Headers
.UPdate
....>Update Yearly<<<<<<<<<<<
.begin patch 5.05
.begin patch 5.06
YR1 init "2012"
YR2 init "2013"
YR3 init "2014"
YR4 init "2015"
YR5 init "2016"
.end patch 5.06
.begin patch 5.0
.YR1 init "2006"
.YR2 init "2007"
.YR3 init "2008"
.YR4 init "2009"
.YR5 init "2010"
.begin patch 5.0
.YR1 init "2009"
.YR2 init "2010"
.YR3 init "2011"
.YR4 init "2012"
.YR5 init "2013"
.end patch 5.3
.YR1 init "2007"
.YR2 init "2008"
.YR3 init "2009"
.YR4 init "2010"
.YR5 init "2011"
.end patch 5.01
.end patch 5.0
.================================================================================
.KEY80    DIM       80         KEY VAR.
LstTot   form      10         Mailer Total Order
TotOrd   form      10         TOTAL Order
.begin patch 5.0
.OYR95     form      10         95 Order Grand Total
.XYR95     form      10         95 Xchange Grand Total
.RYR95     form      10         95Rental  Grand Total
.=====================================================
OYRG01     form      10         Year 1 Order Grand Total
XYRG01     form      10          Xchange Grand Total
RYRG01     form      10         Rental  Grand Total
.=====================================================
OYRG02     form      10          Order Grand Total
XYRG02     form      10          Xchange Grand Total
RYRG02     form      10         Rental  Grand Total
.=====================================================
OYRG03     form      10          Order Grand Total
XYRG03     form      10          Xchange Grand Total
RYRG03     form      10         Rental  Grand Total
.=====================================================
OYRG04     form      10          Order Grand Total
XYRG04     form      10         Xchange Grand Total
RYRG04     form      10         Rental  Grand Total
.=====================================================
OYRG05     form      10          Order Grand Total
XYRG05     form      10         Xchange Grand Total
RYRG05     form      10         Rental  Grand Total
.=====================================================
OYRG06     form      10          Order Grand Total
XYRG06     form      10          Xchange Grand Total
RYRG06     form      10          Rental  Grand Total
.=====================================================
OYRG07     form      10        year 7  Order Grand Total
XYRG07     form      10          Xchange Grand Total
RYRG07     form      10          Rental  Grand Total
.end patch 5.0
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

PDFOPT   DIM        1
FMONTH   FORM       2
LUSER    DIM        10
.timestamp1 dim  16
.time1   form    16
.time2   form    16
.time3   form    16
.==========================================================================
         move      c3 to ncntpath
         move levels to PDFOPT
         move prio to FMONTH
         move user to LUSER
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
          Trap      Inter if INTERRUPT 
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
.begin patch 5.0
.          PREPARE   OUTPUT,"C:\work\MLRSUM","C:\work\mlrsum","80","831"
          PREPARE   OUTPUT,"C:\work\MLRSUM","C:\work\mlrsum","80","261"
.end patch 5.0
         NORETURN
CHECK
...................................
         compare   funcbr to c1
         IF        EQUAL
         MOVE      C1 TO MODE
         move      fmonth to fiscmo
         rep       zfill,fiscmo
         CALL      FISCAL
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
           if         (olrn = "725917")
           call       debug
           endif
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
         BRANCH    MODE OF OUTPUT,OUTPUT2                         .1=fiscal, 2=calendar
         DISPLAY   *P15:10,"I'M LOST!!!! STOP"
         STOP
READ     READ      INFILE,SEQ;ORDVARS
         if over
         GOTO      Close1 IF OVER

         endif
         ADD       C1 TO count
         DISPLAY   *P10:14,"RECORDS PROCESSED ",count,b1,olrn
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      read IF EQUAL     YES, skip.
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      read IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       LCR order ?
         GOTO      read IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       Cancelled LCR order ?
         GOTO      read IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      READ IF EQUAL
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         GOTO      READ IF EQUAL
         RETURN
.1=fiscal
OUTPUT
         PACK      KEY80 FROM MCOMP,OLSTNAME
         MOVE      C0 TO QUANT
         MOVE      C0 TO RQUANT
         MOVE      C0 TO XQUANT
         MOVE      C0 TO TORDQTYR
         MOVE      OQTY TO QUANT
         CALL      RENTXCHG
         MOVE      C0 TO BRANCH
         compare   c2 to dteflag
         if        equal
         move      oodtey to omdtey
         move      oodtem to omdtem
         endif
.begin patch 5.0
         MOVE      C0 TO BRANCH

          MOVe      OMDTEM,OrdMO
          IF        (Omdtey = yr01 & ORdMO >= Start)       .1st fiscal
          MOVE      C1,Branch
          Elseif    (Omdtey = yr02 & ORdMO < Start)                .1st fiscal
          MOVE      C1,Branch
          Elseif    (OMdtey = yr02 & ORdMO >= Start)       .2nd fiscal
          MOVE      C2,Branch
          Elseif    (Omdtey = yr03 & ORdMO < Start)       .2nd fiscal
          MOVE      C2,Branch
          Elseif    (OMdtey = yr03 & ORdMO >= Start)       .3rd fiscal
          MOVE      C3,Branch
          Elseif    (Omdtey = yr04 & ORdMO < Start)       .3rd fiscal
          MOVE      C3,Branch
          Elseif    (OMdtey = yr04 & ORdMO >= Start)       .4th fiscal
          MOVE      C4,Branch
          Elseif    (Omdtey = yr05 & ORdMO < Start)       .4th fiscal
          MOVE      C4,Branch
          Elseif    (OMdtey = yr05 & ORdMO >= Start)       .5th fiscal
          MOVE      C5,Branch
          Elseif    (Omdtey = yr06 & ORdMO < Start)       .5th fiscal
          MOVE      C5,Branch
          Elseif    (OMdtey = yr06 & ORdMO >= Start)       .6th fiscal
          MOVE      C6,Branch
          Elseif    (Omdtey = YR07 & ORdMO < Start)       .6th fiscal
          MOVE      C6,Branch
          Endif





.         MATCH     yr01 TO OMDTEY
.         IF        EQUAL
.         MOVE       OMDTEM TO ORDMO
.         COMPARE   START TO ORDMO
.         IF        NOT LESS
.         MOVE      C1 TO BRANCH
.         ENDIF
.         ENDIF
.
.         MATCH     yr02 TO OMDTEY
.         IF        EQUAL
.         MOVE      OMDTEM TO ORDMO
.         COMPARE   START TO ORDMO
.         IF        NOT LESS
.         MOVE      C2 TO BRANCH
.          ELSE
.         MOVE      C1 TO BRANCH
.        ENDIF
.        ENDIF
.        
.         MATCH     yr03 TO OMDTEY
.         IF        EQUAL
.         MOVE      OMDTEM TO ORDMO
.         COMPARE   START TO ORDMO
.         IF        NOT LESS
.         MOVE      C3 TO BRANCH
.          ELSE
.         MOVE      C2 TO BRANCH
.        ENDIF
.        ENDIF
.        
.         MATCH     yr04 TO OMDTEY
.         IF        EQUAL
.         MOVE      OMDTEM TO ORDMO
.         COMPARE   START TO ORDMO
.         IF        NOT LESS
.         MOVE      C4 TO BRANCH
.          ELSE
.         MOVE      C3 TO BRANCH
.        ENDIF
.        ENDIF
.        
.         MATCH     yr05 TO OMDTEY
.         IF        EQUAL
.         MOVE      OMDTEM TO ORDMO
.         COMPARE   START TO ORDMO
.         IF        NOT LESS
.         MOVE      C5 TO BRANCH
.          ELSE
.         MOVE      C4 TO BRANCH
.        ENDIF
.        ENDIF
.        
.         MATCH     yr06 TO OMDTEY
.         IF        EQUAL
.         MOVE      OMDTEM TO ORDMO
.         COMPARE   START TO ORDMO
.         IF        NOT LESS
.         MOVE      C6 TO BRANCH
.          ELSE
.         MOVE      C5 TO BRANCH
.        ENDIF
.        ENDIF
.sample of old code had 1988 to present

.         MATCH     "88" TO OMDTEY
.         IF        EQUAL
.         MOVE       OMDTEM TO ORDMO
.         COMPARE   START TO ORDMO
.         IF        NOT LESS
.         MOVE      C1 TO BRANCH
.         ENDIF
.         ENDIF
.
.         MATCH     "89" TO OMDTEY
.         IF        EQUAL
.         MOVE      OMDTEM TO ORDMO
.         COMPARE   START TO ORDMO
.         IF        NOT LESS
.         MOVE      C2 TO BRANCH
.          ELSE
.         MOVE      C1 TO BRANCH
.        ENDIF
.        ENDIF
.end of sample
          BRANCH    BRANCH OF OK,OK,OK,OK,OK,OK
.end patch 5.0
         GOTO      INPUT
.calendar
OUTPUT2
         PACK      KEY80 FROM MCOMP,OLSTNAME
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
         COMPARE   C1 TO DTEFLAG
         IF        EQUAL
         move      omdtem to mm
         move      omdted to dd
         move      omdtey to yy
         ELSE
         move      oodtem to mm
         move      oodted to dd
         move      oodtey to yy
         ENDIF
.
         move      c1 to branch
.UPdate
.dateloop load      dateyy from branch of yr01,yr02,yr03,yr04,yr05,yr06
dateloop load      dateyy from branch of yr02,yr03,yr04,yr05,yr06,yr07
         match     datechk to dateyy
         goto      ok if equal
         add       c1 to branch
         compare   c7 to branch
         goto      input if equal              .outside range
         goto      dateloop                   .try again
.
OK
.begin patch 5.0
         READ      OUTPUT,KEY80;B1,KEY80,Oyr1,Xyr1,Ryr1,Oyr2,Xyr2,Ryr2,Oyr3,Xyr3,Ryr3:
                   Oyr4,Xyr4,Ryr4,Oyr5,Xyr5,Ryr5,Oyr6,Xyr6,Ryr6
         GOTO      WRITE IF OVER
         CALL      ADD
         UPDATE    OUTPUT;B1,KEY80,Oyr1,Xyr1,Ryr1,Oyr2,Xyr2,Ryr2,Oyr3,Xyr3,Ryr3:
                   Oyr4,Xyr4,Ryr4,Oyr5,Xyr5,Ryr5,Oyr6,Xyr6,Ryr6

.         O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01:
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08:
.                   o09,x09,r09,o10,x10,r10
.End Patch 5.0 
         GOTO      INPUT
WRITE
         PACK      KEY80 FROM MCOMP,OLSTNAME
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
         MOVE      C0 TO Ryr4
         MOVE      C0 TO Ryr5
         MOVE      C0 TO Ryr6
         CALL      ADD
         WRITE     OUTPUT,KEY80;B1,KEY80,Oyr1,Xyr1,Ryr1,Oyr2,Xyr2,Ryr2,Oyr3,Xyr3,Ryr3:
                   Oyr4,Xyr4,Ryr4,Oyr5,Xyr5,Ryr5,Oyr6,Xyr6,Ryr6
         GOTO      INPUT
ADD
         BRANCH    BRANCH OF yr1,yr2,yr3,yr4,yr5,yr6

         NORETURN
         GOTO      INPUT
yr1      ADD       QUANT TO Oyr1
         ADD       XQUANT TO Xyr1
         ADD       RQUANT TO Ryr1
         RETURN
yr2      ADD       QUANT TO Oyr2
         ADD       XQUANT TO Xyr2
         ADD       RQUANT TO Ryr2
         RETURN
yr3      ADD       QUANT TO Oyr3
         ADD       XQUANT TO Xyr3
         ADD       RQUANT TO Ryr3
         RETURN
yr4      ADD       QUANT TO Oyr4
         ADD       XQUANT TO Xyr4
         ADD       RQUANT TO Ryr4
         RETURN
yr5      ADD       QUANT TO Oyr5
         ADD       XQUANT TO Xyr5
         ADD       RQUANT TO Ryr5
         RETURN
yr6      ADD       QUANT TO Oyr6
         ADD       XQUANT TO Xyr6
         ADD       RQUANT TO Ryr6
         RETURN
.
FISCAL
          KEYIN     *P1:1,*ES,*P10:15,*JR,"ENTER STARTING MONTH FISCAL YEAR ",*t15,*RV,*dv,FISCMO
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
          call      GetWinVer


.***************************************************
         pack   SortFle,srtdir,indat,comma,srtdir,outsrt
.Sort and secondary sort on list
           pack   taskname,sortfle,";",mlrsrt
.****************************************************
        sort   taskname
        if over
               alert caution,S$ERROR$,result,"No Sort"
        endif
        clear n3
.Open sorted file
          OPEN      LISTFLE,"C:\work\MLRSUM.SRT",SHARE
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
        Clear OYRG01
        Clear XYRG01
        Clear RYRG01
        Clear OYRG02
        Clear XYRG02
        Clear RYRG02
        Clear OYRG03
        Clear XYRG03
        Clear RYRG03
        Clear OYRG04
        Clear XYRG04
        Clear RYRG04
        Clear OYRG05
        Clear XYRG05
        Clear RYRG05
        Clear OYRG06
        Clear XYRG06
        Clear RYRG06
        Clear OYRG07
        Clear XYRG07
        Clear RYRG07
        clear N8
        move  C0 to newpg
.
        Trap Spool1 giving error if SPOOL
        move comment to cntprint
        if (cntprint = c0)
                move c3 to cntprint
        endif
.
        Display    *P10:11,*EF,*cyan,"Printing in Process!!"
.
                    clock timestamp,timestamp
          pack splfle,"c:\work\",luser,timestamp,".lst"
                    pack spoolname,luser,timestamp
.
.          call      debug
          if (pdfopt = YES)
.begin patch 5.02
.                    Call      GetPDFPATH
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
.                              PRTOPEN prfile,"PDF995","MlrSum.pdf"
                              PRTOPEN prfile,"PDF:","c:\work\pdf\MlrSum.pdf",Flags=PDF_FLAGS_WIN_ANSI_ENCODING
.end patch 5.02
          else
                    if (cntprint = "5")      .Susan
                                  PRTOPEN prfile,"",spoolname,spoolfile=splfle

                              goto   prtnext
                    Elseif    (cntprint = "7")      .
                              PRTOPEN prfile,"Kyocera FS-C5030N (KX) on NIN0100",spoolname,noprint,spoolfile=splfle
                                    if (osflag = c2 or Osflag = c6)         .nt +
                                            PRTOPEN prfile,"Kyocera FS-C5030N (KX) on NIN0100",spoolname,noprint,spoolfile=splfle
                                    elseif (osflag = c1)         .win 95 98
                                            PRTOPEN prfile,"@KYOCERAM",spoolname,noprint,spoolfile=splfle
                                    else   .(osflag = c0)         .Don't know prompt for printer
                                            PRTOPEN prfile,"@",spoolname,noprint,spoolfile=splfle
                                    endif
                              goto   prtnext               .KyoceraM
                              else
                    
                if (cntprint = "1" | cntprint = "3")      .Laser6


                        if (osflag = c1 | Osflag = C5 | osflag = c6 or Osflag = c8 or Osflag = c9)         .nt win2k Xp
                                PRTOPEN prfile,"\\NINs2\Laser6",spoolname,noprint,spoolfile=splfle
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN Prfile,"Laser6",spoolname,noprint,spoolfile=splfle
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN PRfile,"-",spoolname,noprint,spoolfile=splfle
                        endif

                else                                    .Laser3 = Default
                        if (osflag >= c6)
                                PRTOPEN prfile,"\\NINs2\Laser3 Blankstock",spoolname,noprint,spoolfile=splfle
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN Prfile,"Laser3",spoolname,noprint,spoolfile=splfle
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN PRfile,"-",spoolname,noprint,spoolfile=splfle
                        endif
                            endif        
                endif
                                        endif
prtnext
                          prtpage prfile;*UNITS=*HIENGLISH:
                                        *ORIENT=*LANDSCAPE;
.===========================================================================
        clear PgCnt
PAGE
        if (newmlr = c1)
                clear pgcnt
                clear rowcount
                clear OYRG01
                clear XYRG01
                clear RYRG01
                clear OYRG02
                clear XYRG02
                clear RYRG02
                clear OYRG03
                clear XYRG03
                clear RYRG03
                clear OYRG04
                clear XYRG04
                clear RYRG04
                clear OYRG05
                clear XYRG05
                clear RYRG05
                clear OYRG06
                clear XYRG06
                clear RYRG06
                clear OYRG07
                clear XYRG07
                clear RYRG07
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
        prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News";
        prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date:";
        clock timestamp,str8
        unpack str8,str2,yy,mm,dd
        clear str10
        pack  str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*pTitle3:row,*font=font12,str10;
       add     eightlpi,row
       add     eightlpi,row
       prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,*ULON,*ll,"Names Mailed By",*ULOFF;
       add     eightlpi,row
       add     eightlpi,row
       UNPACK KEY80,str45,str35
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
       READ      LISTFLE,SEQ;B1,KEY80,Oyr1,Xyr1,Ryr1,Oyr2,Xyr2,Ryr2,Oyr3,Xyr3,Ryr3:
                   Oyr4,Xyr4,Ryr4,Oyr5,Xyr5,Ryr5,Oyr6,Xyr6,Ryr6
.       O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01:
.                   o02,x02,r02,o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08:
.                   o09,x09,r09,o10,x10,r10
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
.OrderTotal
        clear LstTot
        If (MODE = C1)
.FISCAL
                    ADD oyr1 to lstTot
                    ADD oyr2 to lstTot
                    ADD oyr3 to lstTot
                    ADD oyr4 to lstTot
                    ADD oyr5 to lstTot
                    ADD oyr6 to lstTot
                    if (lstTot > c0)                      .if list total is 0 for all years skip
                              ADD lstTot to TotOrd
                              goto TotOrd
                else
                        goto Row2
                    endif
       else
.Calendar
                    add oyr2 to lstTot
                    add oyr3 to lstTot
                    add oyr4 to lstTot
                    add oyr5 to lstTot
                    add oyr6 to lstTot
                    if (lstTot > c0)
                              ADD lstTot to TotOrd
                else
                        goto Row2
                endif
        Endif
TotOrd
.Total Orders Yearly

        If (MODE = C1)
                 ADD oyr1 to OYRG01
                 ADD oyr2 to OYRG02
                 ADD oyr3 to OYRG03
                 ADD oyr4 to OYRG04
                 ADD oyr5 to OYRG05
                 ADD oyr6 to OYRG06
                 goto Exchg
        else
.Update
                 ADD oyr2 to OYRG03
                 ADD oyr3 to OYRG04
                 ADD oyr4 to OYRG05
                 ADD oyr5 to OYRG06
                 ADD oyr6 to OYRG07
          endif
.Exchange Total-yearly
Exchg
        If (MODE = C1)
                 ADD Xyr1 to XYRG01
                 ADD Xyr2 to XYRG02
                 ADD Xyr3 to XYRG03
                 ADD Xyr4 to XYRG04
                 ADD Xyr5 to XYRG05
                 ADD Xyr6 to XYRG06
               goto Rent1
        endif
.Update
        ADD Xyr2 to XYRG03
        ADD Xyr3 to XYRG04
        ADD Xyr4 to XYRG05
        ADD Xyr5 to XYRG06
        ADD Xyr6 to XYRG07
Rent1
.Rental Total-yearly
        If (MODE = C1)
                 ADD Ryr1 to RYRG01
                 ADD Ryr2 to RYRG02
                 ADD Ryr3 to RYRG03
                 ADD Ryr4 to RYRG04
                 ADD Ryr5 to RYRG05
                 ADD Ryr6 to RYRG06
                 goto Print
       endif
.Update
       ADD Ryr2 to RYRG03
       ADD Ryr3 to RYRG04
       ADD Ryr4 to RYRG05
       ADD Ryr5 to RYRG06
       ADD Ryr6 to RYRG07
Print
       add     eightlpi,row
       add     eightlpi,row
       UNPACK KEY80,str45,str35
       prtpage prfile;*pcolumn:row,*font=font7,Str35;
        If (MODE = C1)
.==================================================

        move mask15 to dim15a
        edit Oyr1 to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.Update
        move mask15 to dim15a
        edit Oyr2 to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
        move mask15 to dim15a
        edit Oyr3 to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.Update
        move mask15 to dim15a
        edit Oyr4 to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
        move mask15 to dim15a
        edit Oyr5 to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.Update
        move mask15 to dim15a
        edit Oyr6 to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
        move mask15 to dim15a
        edit lstTot to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
       goto Row
    Endif
.Update
        move mask15 to dim15a
        edit Oyr2 to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.Update
        move mask15 to dim15a
        edit Oyr3 to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.Update
        move mask15 to dim15a
        edit Oyr4 to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.Update
        move mask15 to dim15a
        edit Oyr5 to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
.Update
        move mask15 to dim15a
        edit Oyr6 to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
        move mask15 to dim15a
        edit lstTot to dim15a
          call trim using     dim15a
          prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;
Row
       add c1 to RowCount


Row2
.Reduced Rows on page to make room for Copyright label
       if (ROWCOUNT = "23")
           add     eightlpi,row
           add     eightlpi,row
           move "7750",row
           prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
           prtpage prfile;*font=font12,*ALIGNMENT=*Left,*ll,PgCnt;
           prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
.Update
                              "©","1992-2016, Names in the News";
           goto Page
       endif
.update
.Clear Fields
       clear    Oyr1
       clear    Xyr1
       clear    Ryr1
       clear    Oyr2
       clear    Xyr2
       clear    Ryr2
       clear    Oyr3
       clear    Xyr3
       clear    Ryr3
       clear    Oyr4
       clear    Xyr4
       clear    Ryr5
       clear    Oyr6
       clear    Xyr6
       clear    Ryr6
       repeat
LastPage
       if (ROWCOUNT < "18")
                 goto totals
       else
                 move "7750",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
                 prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
.update
                              "©","1992-2016, Names in the News";
               move c1 to newpg
               add c1 to Pgcnt
               prtpage prfile;*NEWPAGE:
                           *UNITS=*HIENGLISH:
                                     *ORIENT=*LANDSCAPE;
          clear   row
                  move    "300",row
                  prtpage prfile;*pcolumn:row,*font=font12,"Confidential";
                  prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News";
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
        move mask15 to dim15a
        edit OYRG01 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
        move mask15 to dim15a
        edit OYRG02 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
        move mask15 to dim15a
        edit OYRG03 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
        move mask15 to dim15a
        edit OYRG04 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
        move mask15 to dim15a
        edit OYRG05 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
        move mask15 to dim15a
        edit OYRG06 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
       goto Next
    endif
        move mask15 to dim15a
        edit OYRG03 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
        move mask15 to dim15a
        edit OYRG04 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
        move mask15 to dim15a
        edit OYRG05 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
        move mask15 to dim15a
        edit OYRG06 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
        move mask15 to dim15a
        edit OYRG07 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;

Next
       If (MODE = C1)
.add year order total for grand total-fiscal
          add OYRG01 to OTOT
          add OYRG02 to OTOT
          add OYRG03 to OTOT
          add OYRG04 to OTOT
          add OYRG05 to OTOT
          add OYRG06 to OTOT
          goto TOT
       Endif
.add year order total for grand total-Calendar
       add OYRG03 to OTOT
       add OYRG04 to OTOT
       add OYRG05 to OTOT
       add OYRG06 to OTOT
       add OYRG07 to OTOT
TOT
.Grand Total
        move mask15 to dim15a
        edit OTOT to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;
       add     eightlpi,row
       add     eightlpi,row
       prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ULON,"Exchange Totals",*ULOFF,*boldoff;
       If (MODE = C1)
        move mask15 to dim15a
        edit XYRG01 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit XYRG02 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit XYRG03 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit XYRG04 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit XYRG05 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit XYRG06 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
       goto Xyear
   Endif
        move mask15 to dim15a
        edit XYRG03 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit XYRG04 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit XYRG05 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit XYRG06 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit XYRG07 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
Xyear
       If (MODE = C1)
             clear XTOT
             add XYRG01 to XTOT
             add XYRG02 to XTOT
             add XYRG03 to XTOT
             add XYRG04 to XTOT
             add XYRG05 to XTOT
             add XYRG06 to XTOT
             goto RYear
       Endif
       clear XTOT
       add XYRG03 to XTOT
       add XYRG04 to XTOT
       add XYRG05 to XTOT
       add XYRG06 to XTOT
       add XYRG07 to XTOT
RYEAR
        move mask15 to dim15a
        edit XTOT to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF,*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*ULON,"Rental Totals",*ULOFF,*boldoff;
        If (MODE = C1)
        move mask15 to dim15a
        edit RYRG01 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit RYRG02 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit RYRG03 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit RYRG04 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit RYRG05 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit RYRG06 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
         goto Rent2
       ENDIF
        move mask15 to dim15a
        edit RYRG03 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit RYRG04 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit RYRG05 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit RYRG06 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
        move mask15 to dim15a
        edit RYRG07 to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
Rent2
       If (MODE = C1)
             add RYRG01 to RTOT
             add RYRG02 to RTOT
             add RYRG03 to RTOT
             add RYRG04 to RTOT
             add RYRG05 to RTOT
             add RYRG06 to RTOT
             goto LstPg
       Endif
       add RYRG03 to RTOT
       add RYRG04 to RTOT
       add RYRG05 to RTOT
       add RYRG06 to RTOT
       add RYRG07 to RTOT
LstPg
        move mask15 to dim15a
        edit RTOT to dim15a
          call trim using     dim15a
        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,*ll,dim15a,*ULOFF;
.update
       move "7750",row
       prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
       prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
       prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
                             "©","1992-2016, Names in the News";
End1
       PRTCLOSE prfile
       Pause c1
        clear n3
        if (copy = c0)
                goto End2
        Endif
        if (osflag = c2)
                goto looper2
        endif
.==============================================================
.Printer Area Test
Looper1
        if (pdfopt = YES)
                  goto pdfthis
        endif
        loop
          until (N3 = COPY)
                    if (cntprint = "5")      .Susan
                    goto   checker
                    Elseif (cntprint = "7")      .
                              PRTPLAY splfle,"Kyocera FS-C5030N (KX) on NIN0100"
                    goto   checker
                        else
                if (cntprint = "1" | cntprint = "3")      .Laser6
                        if (osflag >= c6)         .
                              PRTPLAY splfle,"\\NINs2\Laser6"
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                              PRTPLAY splfle,"\Laser6"
                        else   .(osflag = c0)         .Don't know prompt for printer
                              PRTPLAY splfle,"-"
                        endif
                else
                        if (osflag >= c6)         .nt win2k Xp
                              PRTPLAY splfle,"\\NINs2\Laser3 Blankstock"
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                              PRTPLAY splfle,"\Laser3"
                        else   .(osflag = c0)         .Don't know prompt for printer
                              PRTPLAY splfle,"-"
                        endif
                        endif
                endif
                add c1 to N3
        repeat
checker
        goto checknew
.================================================================
Looper2
        if (pdfopt = YES)
                  goto pdfthis
        endif
        loop
          until (N3 = COPY)
                    if (cntprint = "5")      .Susan
.                              PRTPLAY splfle,"\\PLI0007\KYOCERAS"
                              goto   checknew
                    Elseif (cntprint = "7")      .
                              PRTPLAY splfle,"Kyocera FS-C5030N (KX) on NIN0100"
                    goto   checknew
                        else
                if (cntprint = "1" | cntprint = "3")      .Laser6
                              PRTPLAY splfle,"\\NINs2\Laser6"
                else
                              PRTPLAY splfle,"\\NINs2\Laser3 Blankstock"
                            endif
                endif
                add c1 to N3
        repeat
checknew
       if (newmlr = c1)
                if (pdfopt <> YES)
                     erase splfle
                endif
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
        if (pdfopt <> YES)
                     erase splfle
         endif
        Pause c1
        shutdown  "cls"
          stop
EOJ
         CLOSE     INFILE
         CLOSE     OUTPUT
         NORETURN
         shutdown  "cls"
         STOP

.===================================================
Spool1
         Display    *P10:11,*EF,*cyan,error
         PAUSE c10
         Display    *P10:11,*EF,*cyan,"Trying Again"
         Pause c10
         TRAPCLR   SPOOL
         Trap SPOOL2 giving error if SPOOL
         SCAN      "S10" IN ERROR
         if equal
                if (cntprint = "1" | cntprint = "3")      .Laser6
                        if (osflag >= c6)         .
                                PRTOPEN prfile,"\\NINs2\Laser6",spoolname,noprint,spoolfile=splfle
                        elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                PRTOPEN Prfile,"Laser6",spoolname,noprint,spoolfile=splfle
                        else   .(osflag = c0)         .Don't know prompt for printer
                                PRTOPEN PRfile,"-",spoolname,noprint,spoolfile=splfle
                        endif
.                    PRTOPEN prfile,"Laser6",spoolname,noprint,spoolfile=splfle
                else
                        if (osflag >= c6)         .
                                PRTOPEN prfile,"\\NINs2\Laser3 Blankstock",spoolname,noprint,spoolfile=splfle
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
pdfthis
.begin patch 5.02
.          call      GetPDfPAth
.          pack      str45 from PDFPATH,"\flag.dat"
.          pack      APIFileName,STR45,hexzero
.          loop
.                    call      FindFirstFile
.                    until (APIResult = 0 | APIResult = hexeight)
.                    pause     "1"
.          repeat
          pause     "5"
.          pause     "2"
.end patch 5.02
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
         goto checknew
Inter     
          Trapclr  INTERRUPT 
               alert caution,S$ERROR$,result,"Sorry that would RUIN the report!!"
          Trap      Inter if INTERRUPT 
          return

IOTrap
          move      C1,N1
          return
.******************************************************
         include   ncntio.inc
          include   compio.inc
          include   cntio.inc
         INCLUDE   NDATIO.INC
         INCLUDE   COMLOGIC.INC

