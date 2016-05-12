PC       EQU       0
         INC       COMMON.inc
         include   cons.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NDATDD.inc
.patch3.95
				include	compdd.inc
				include	cntdd.inc
.         INC       NMLRDD.inc
.patch3.95
.Patch 3.97
			include winapi.inc
.Patch 3.97
         include   ncntdd.inc
release  init      "4.1"          DLH 	08Jan2008	New Year Update
.release  init      "4.0"          JD	27Mar2007	Yearly Update
.release  init      "3.99"        DMB	20JAN2006	Yearly Update
.release  init      "3.98"        DMB	19JAN2006	Fix to correctly display numeric formatting        
.release  init      "3.97"        DMB	13JAN2005	Yearly UPdate
.release  init      "3.96"        ASH	06AUG2004	Logo Conversion
.release  init      "3.95"        DMB	26MAY2004	Mailer Conversion
;Release   INIT       "3.94"         04/28/03 Added option for the order/maildate selection to be made by nord006b
;Release   INIT       "3.93"         03/20/03 Added pdf option and clean up code to allow plbserv to do prtopen (does not like \\nts0\laser? just doing laser?
;Release   INIT       "3.92"         08/06/02 Added New FY/calendar Year
;Release   INIT       "3.91"         07/23/02 Added code for osflag for XP
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
.				  		 trim # of mlr's per page for aesthetics
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
.RELEASE  INIT      "1.3"       DLH 09NOV93 USE DSINIT, CHAIN FROM UTIL.
.
.RELEASE  INIT      "1.2"       DLH 24FEB92 MLR/DAT INCLUDES.
.                              AND 1992 DATA
.RELEASE  INIT      "1.1"      D. HERRICK 07MAR91

.>Patch 3.98 Variable added
mask15      init    "(Z,ZZZ,ZZZ,ZZ9)"        ;formatting vars
Dim15a      dim     15	    ;formatting vars
.>Patch 3.98


ListFle file
.Patch 3.97
timestamp1 dim  16
time1   form    16
time2   form    16
time3   form    16
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
.OUTPUT   IFILE     KEYLEN=80,FIX=591     .KEY= LIST NUMBER + MAILER NAM
.begin patch 4.1
.OUTPUT   IFILE     KEYLEN=80,FIX=681     .KEY= LIST NUMBER + MAILER NAM
OUTPUT   IFILE     KEYLEN=80,FIX=771     .KEY= LIST NUMBER + MAILER NAM
.end patch 4.1
OLIST    DIM       35         1-35
OMLR     DIM       25        36-60
dteflag form      1                    1=maildate 2=orderdate
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
;Update
yr03     init      "03"
yr04     init      "04"
.Patch 3.97
yr05     init      "05"
.Patch 3.97
.Patch 3.99
yr06     init      "06"
.Patch 3.99
.Patch 3.99
yr07     init      "07"
.Patch 3.99
.Begin Patch 4.1
yr08     init      "08"
.end Patch 4.1
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
;Update
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
newlst   form      1
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
;patch3.91
.osflag   form   1          1=win 95,98, 2=NT
bosflag   form   1          1=win 95,98, 2=NT
;patch3.91
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
;UPdate
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
FY1 init "FY 02/03"
FY2 init "FY 03/04"
FY3 init "FY 04/05"
FY4 init "FY 05/06"
FY5 init "FY 06/07"
FY6 init "FY 07/08"
.Calendar Headers
;UPdate
.YR1 init "2002"
.Patch 4.1
YR1 init "2004"
YR2 init "2005"
YR3 init "2006"
YR4 init "2007"
YR5 init "2008"
.Patch 3.97
.Patch 3.99
.==============================================================================
O88      FORM      10        61-70       88 NAMES (MD)
X88      FORM      10        71-80       88 NAMES (MD)
R88      FORM      10        81-90       88 NAMES (MD)
O89      FORM      10        91-100      89 NAMES (MD)
X89      FORM      10       101-110      89 NAMES (MD)
R89      FORM      10       111-120      89 NAMES (MD)
O90      FORM      10       121-130      90 NAMES (Md)
X90      FORM      10       131-140      90 NAMES (MD)
R90      FORM      10       111-150      90 NAMES (MD)
O91      FORM      10       121-160      91 NAMES (Md)
X91      FORM      10       101-170      91 NAMES (MD)
R91      FORM      10       111-180      91 NAMES (MD)
O92      FORM      10       121-190      92 NAMES (Md)
X92      FORM      10       101-200      92 NAMES (MD)
R92      FORM      10       111-210      92 NAMES (MD)
O93      FORM      10       121-220      93 NAMES (Md)
X93      FORM      10       101-230      93 NAMES (MD)
R93      FORM      10       111-240      93 NAMES (MD)
O94      FORM      10       121-250      94 NAMES (Md)
X94      FORM      10       101-260      94 NAMES (MD)
R94      FORM      10       111-270      94 NAMES (MD)
O95      FORM      10       121-280      95 NAMES (Md)
X95      FORM      10       101-290      95 NAMES (MD)
R95      FORM      10       111-300      95 NAMES (MD)
O96      FORM      10       121-310      96 NAMES (Md)
X96      FORM      10       101-320      96 NAMES (MD)
R96      FORM      10       111-330      96 NAMES (MD)
O97      FORM      10       121-340      97 NAMES (Md)
X97      FORM      10       101-350      97 NAMES (MD)
R97      FORM      10       111-360      97 NAMES (MD)
.
O98      FORM      10       360-390      98 NAMES (Md)
X98      FORM      10                    98 NAMES (MD)
R98      FORM      10                    98 NAMES (MD)
O99      FORM      10       390-420      99 NAMES (Md)
X99      FORM      10                    99 NAMES (MD)
R99      FORM      10                    99 NAMES (MD)
O00      FORM      10       390-420      00 NAMES (Md)
X00      FORM      10                    00 NAMES (MD)
R00      FORM      10                    00 NAMES (MD)
O01      FORM      10                    01 NAMES (Md)
X01      FORM      10                    01 NAMES (MD)
R01      FORM      10                    01 NAMES (MD)
O02      FORM      10                    02 NAMES (Md)
X02      FORM      10                    02 NAMES (MD)
R02      FORM      10                    02 NAMES (MD)
;UPdate
O03      FORM      10                    03 NAMES (Md)
X03      FORM      10                    03 NAMES (MD)
R03      FORM      10                    03 NAMES (MD)
;UPdate
O04      FORM      10                    04 NAMES (Md)
X04      FORM      10                    04 NAMES (MD)
R04      FORM      10                    04 NAMES (MD)
.Patch 3.97
;UPdate
O05      FORM      10                    04 NAMES (Md)
X05      FORM      10                    04 NAMES (MD)
R05      FORM      10                    04 NAMES (MD)
.Patch 3.97
.Patch 3.99
;UPdate
O06      FORM      10                    04 NAMES (Md)
X06      FORM      10                    04 NAMES (MD)
R06      FORM      10                    04 NAMES (MD)
.Patch 3.99
.Patch 4.0
.;UPdate
O07      FORM      10                    04 NAMES (Md)
X07      FORM      10                    04 NAMES (MD)
R07      FORM      10                    04 NAMES (MD)
.Patch 4.0
.Begin Patch 4.1
.;UPdate
O08      FORM      10                    2008 NAMES (Md)
X08      FORM      10                    2008 NAMES (MD)
R08      FORM      10                    2008 NAMES (MD)
.End Patch 4.1



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

OYR96     form      10         96 Order Grand Total
XYR96     form      10         96 Xchange Grand Total
RYR96     form      10         96Rental  Grand Total
OYR97     form      10         97 Order Grand Total
XYR97     form      10         97 Xchange Grand Total
RYR97     form      10         97Rental  Grand Total
OYR98     form      10         98 Order Grand Total
XYR98     form      10         98 Xchange Grand Total
RYR98     form      10         98Rental  Grand Total
OYR99     form      10         99 Order Grand Total
XYR99     form      10         99Xchange Grand Total
RYR99     form      10         99Rental  Grand Total
OYR00     form      10         00 Order Grand Total
XYR00     form      10         00Xchange Grand Total
RYR00     form      10         00Rental  Grand Total
OYR01     form      10         01 Order Grand Total
XYR01     form      10         01 Xchange Grand Total
RYR01     form      10         01 Rental  Grand Total
OYR02     form      10         02 Order Grand Total
XYR02     form      10         02Xchange Grand Total
RYR02     form      10         02Rental  Grand Total

OTOT     form      10         ORDER   GRAND Total
XTOT     form      10         Xchange Grand Total
RTOT     form      10         Rental  Grand Total
.=============================================================================
;patch3.93
PDFOPT   DIM        1
FMONTH   FORM       2
LUSER    DIM        10
;patch3.93


.Sort Parameters=======================================================
INDAT    init  "LISTSUM.DAT"   .File to be sorted
OUTSRT   init  "LISTSUM.SRT"   .Partial Name of sorted Output file for ncsh
LSTSRT   init  "2-36,37-81"   .Sort
SRTDIR   init  "c:\work\"
SORTFLE  dim    100                          .Var to pack file names of sort
.==========================================================================


         move      c3 to ncntpath
.Patch 3.2a........................
.Var for # of copies
         move      inits to Copy
;patch3.93
         move levels to PDFOPT
         move prio to FMONTH
         move user to LUSER
;patch3.93
....................................
         MOVE      FUNC TO FUNCBR
         CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        EOS                 .NO
         move      c1 to dteflag
         MOVE      "NORD0028" TO PROGRAM
         MOVE      "Names In The News Ca Inc." TO COMPNME
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
.         KEYIN     *P10:14,*EL,"OUTPUT FILE ALREADY EXISTS (A)dd or (":
.                   *CYAN,"P",*WHITE,")repare new one ",*T5,STR1
.         CMATCH    "A" TO STR1
.         GOTO      CHECK IF EQUAL
         CLOSE     OUTPUT
NOOUT    TRAPCLR   IO
         IFNZ      PC
         PREPARE   OUTPUT,"LISTSUMMARY:PRINT"
         XIF
         IFZ       PC
.Start Patch #2.1 - Increase file and key size to reflect increase in MCOMP
.         PREPARE   OUTPUT,"C:\LISTSUM","C:\listsum","60","451"
;UPdate
.Patch 3.99
.Patch 3.97
.         PREPARE   OUTPUT,"C:\work\LISTSUM","C:\work\listsum","80","681"
.Begin Patch 4.1
         PREPARE   OUTPUT,"C:\work\LISTSUM","C:\work\listsum","80","771"
.Patch 4.0
.         PREPARE   OUTPUT,"C:\work\LISTSUM","C:\work\listsum","80","741"
.end Patch 4.1
.         PREPARE   OUTPUT,"C:\work\LISTSUM","C:\work\listsum","80","651"
.         PREPARE   OUTPUT,"C:\LISTSUM","C:\listsum","80","621"
.Patch 3.97
.Patch 3.99
;         PREPARE   OUTPUT,"C:\LISTSUM","C:\listsum","80","591"
;         PREPARE   OUTPUT,"C:\LISTSUM","C:\listsum","80","561"
.End Patch #2.1 - Increase file and key size to reflect increase in MCOMP
         XIF
         NORETURN
CHECK
;.         KEYIN     *P10:12,*EF,"MODIFY START FISCAL YEAR ? : ",*t15,STR1
;.         CMATCH    YES TO STR1
;.==================================
;.Testing Only
;.        move c1 to funcbr
;.========================================
         compare   funcbr to c1
         IF        EQUAL
         MOVE      C1 TO MODE
;patch3.93
         move      fmonth to fiscmo
         rep       zfill,fiscmo
;patch3.93
;         alert     caution,mode,result,fiscmo
         CALL      FISCAL
		pause c2
         ELSE
         MOVE      C2 TO MODE
         ENDIF
;patch3.94
CHECK2
;         MOVE      "M" to str1
;         rep       "M1O2m1o2" in STR1

          MOVE      CURSYS to str1
         if (str1 = "1")
            DISPLAY   *P10:12,"Select Mail Date"
         endif
         if (str1 = "2")
            DISPLAY   *P10:12,"Select Order Date"
         endif
;         KEYIN     *P10:12,*EF,*cyan,"M",*white,"aildate (O)rder date: ",*T15,*RV,STR1
;         rep       "M1O2m1o2" in STR1
;patch3.94
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
.         if        (olrn = "489803")
test
.         call      debug
.         endif
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

         MATCH     "94" TO OMDTEY                  .dh 12jan94
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C7 TO BRANCH
          ELSE
         MOVE      C6 TO BRANCH
         ENDIF
         ENDIF                                  .end dh12jan94

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
.
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
.Patch 3.97
.update
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
.Patch 3.97
.Patch 3.99
.update
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
.Patch 3.99
.Patch 4.0
.update
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
.Patch 4.0
.Patch 4.1
.update
        MATCH     "08" TO OMDTEY
         IF        EQUAL
         MOVE      OMDTEM TO ORDMO
         COMPARE   START TO ORDMO
         IF        NOT LESS
         MOVE      C21 TO BRANCH
          ELSE
         MOVE      CC TO BRANCH
         ENDIF
         ENDIF
.Patch 4.1
..
          BRANCH    BRANCH OF OK,OK,OK,OK,OK,OK,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,OK,ok,ok,ok,ok
         GOTO      INPUT
OUTPUT2
.Start Patch #2.1 - Increase file and key size to reflect increase in MCOMP
.         PACK      KEY60 FROM OLSTNAME,MCOMP
         PACK      KEY80 FROM OLSTNAME,MCOMP
.End Patch #2.1 - Increase file and key size to reflect increase in MCOMP
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
         ELSE
         move      oodtem to mm
         move      oodted to dd
         move      oodtey to yy
         endif
         move      c1 to branch
dateloop load      dateyy from branch of yr88,yr89,yr90,yr91,yr92,yr93,yr94:
.begin patch 4.1
.                   yr95,yr96,yr97,yr98,yr99,yr00,yr01,yr02,yr03,yr04,yr05,yr06,yr07               
                   yr95,yr96,yr97,yr98,yr99,yr00,yr01,yr02,yr03,yr04,yr05,yr06,yr07,YR08              
.end patch 4.1
.;update .Patch 3.97 3.99
         match     datechk to dateyy
         goto      ok if equal
         add       c1 to branch
.begin patch 2.5
.         compare   c14 to branch
.Patch 3.99
.         compare   "20" to branch               .DLH 06Feb01
.Patch 3.99         
.Begin Patch 4.1
.Patch 4.0         
         compare   "22" to branch               
.Patch 4.0
.         compare   "21" to branch               .DLH 06Feb01
.End Patch 4.1         
;         compare   "16" to branch               .DLH 06Feb01
.end patch 2.5
         goto      input if equal              .outside range
         goto      dateloop                   .try again
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
.
         MATCH     "98" TO OMDTEY
         IF        EQUAL
         MOVE      C11 TO BRANCH
         ENDIF

         MATCH     "99" TO OMDTEY
         IF        EQUAL
         MOVE      C12 TO BRANCH
         ENDIF
         MATCH     "00" TO OMDTEY
         IF        EQUAL
         MOVE      C13 TO BRANCH
         ENDIF
         MATCH     "01" TO OMDTEY
         IF        EQUAL
         MOVE      c14 TO BRANCH
         ENDIF
         MATCH     "02" TO OMDTEY
         IF        EQUAL
         MOVE      c15 TO BRANCH
         ENDIF
         MATCH     "03" TO OMDTEY
         IF        EQUAL
         MOVE      c16 TO BRANCH
         ENDIF
         MATCH     "04" TO OMDTEY
         IF        EQUAL
         MOVE      c17 TO BRANCH
         ENDIF
         MATCH     "05" TO OMDTEY
         IF        EQUAL
         MOVE      c18 TO BRANCH
         ENDIF
.Patch 3.99
         MATCH     "06" TO OMDTEY
         IF        EQUAL
         MOVE      c19 TO BRANCH
         ENDIF
.Patch 3.99
.Patch 4.0
         MATCH     "07" TO OMDTEY
         IF        EQUAL
         MOVE      cc TO BRANCH
         ENDIF
.Patch 4.0
.oldcode
.Patch 3.99
         BRANCH    BRANCH OF OK,OK,OK,OK,OK,OK,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok
.Patch 3.99         
         GOTO      INPUT
.OK       READ      OUTPUT,KEY60;B1,KEY60,O88,O89,O90,O91,O92,O93,o94
OK
.Start Patch #2.1 - Increase file and key size to reflect increase in MCOMP
.         READ      OUTPUT,KEY60;B1,KEY60,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99
;UPdate
.Patch 3.99
.Patch 3.97
.         READ      OUTPUT,KEY80;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01,o02,x02,r02:
.                   o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06
.Patch 3.97
.Patch 3.99
.Patch 4.0
         READ      OUTPUT,KEY80;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01,o02,x02,r02:
.begin patch 4.1
.                   o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07
                   o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08
.end patch 4.1
.Patch 4.0
.end Patch #2.1 - Increase file and key size to reflect increase in MCOMP
         GOTO      WRITE IF OVER
         CALL      ADD
.         UPDATE    OUTPUT;B1,KEY60,O88,O89,O90,O91,O92,O93,o94
.Start Patch #2.1 - Increase file and key size to reflect increase in MCOMP
.         UPDATE    OUTPUT;B1,KEY60,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99
;UPdate
.Patch 3.99
.Patch 3.97
.         UPDATE    OUTPUT;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01,o02,x02,r02:
.                   o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06
.Patch 3.97
.Patch 3.99
.Patch 4.0
         UPDATE    OUTPUT;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01,o02,x02,r02:
.begin patch 4.1
.                   o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07
                   o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08
.end patch 4.1
.Patch 4.0
.End Patch #2.1 - Increase file and key size to reflect increase in MCOMP
         GOTO      INPUT
WRITE
.Start Patch #2.1 - Increase file and key size to reflect increase in MCOMP
.         PACK      KEY60 FROM OLSTNAME,MCOMP
         PACK      KEY80 FROM OLSTNAME,MCOMP
.End Patch #2.1 - Increase file and key size to reflect increase in MCOMP
;UPdate
         MOVE      C0 TO O88
         MOVE      C0 TO O89
         MOVE      C0 TO O90
         MOVE      C0 TO O91
         MOVE      C0 TO O92
         MOVE      C0 TO O93
         MOVE      C0 TO O94
         MOVE      C0 TO O95
         MOVE      C0 TO O96
         MOVE      C0 TO O97
         move      c0 to O98
         move      c0 to O99
         move      c0 to o00
         move      c0 to o01
         move      c0 to o02
         move      c0 to o03
         move      c0 to o04
.Patch 3.97
         move      c0 to o05
.Patch 3.97
.Patch 3.99
         move      c0 to o06
.Patch 3.99
.Patch 4.0
         move      c0 to o07
.Patch 4.0
;Update
.Begin patch 4.1
         move      c0 to o08
.End Patch 4.1
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
         move      c0 to x98
         move      c0 to r98
         move      c0 to x99
         move      c0 to r99
         move      c0 to x00
         move      c0 to r00
         move      c0 to x01
         move      c0 to r01
         move      c0 to x02
         move      c0 to r02
         move      c0 to x03
         move      c0 to r03
         move      c0 to x04
         move      c0 to r04
.Patch 3.97
         move      c0 to x05
         move      c0 to r05
.Patch 3.97
.Patch 3.99
         move      c0 to x06
         move      c0 to r06
.Patch 3.99
.Patch 4.0
         move      c0 to x07
         move      c0 to r07
.Patch 4.0
.Begin Patch 4.1
         move      c0 to x08
         move      c0 to r08
.End Patch 4.1
         CALL      ADD
.Start Patch #2.1 - Increase file and key size to reflect increase in MCOMP
.         WRITE     OUTPUT,KEY60;B1,KEY60,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98,x99,r99
;update
.Patch 3.99
.Patch 3.97
.         WRITE     OUTPUT,KEY80;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01,o02,x02,r02:
.                   o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06
.Patch 3.97
.Patch 3.99
.Patch 4.0
         WRITE     OUTPUT,KEY80;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01,o02,x02,r02:
.begin patch 4.1
.                   o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07
                   o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08
.end patch 4.1
.Patch 4.0
.Start Patch #2.1 - Increase file and key size to reflect increase in MCOMP
         GOTO      INPUT
;UPdate
.Patch 3.99
.Patch 3.97
ADD      BRANCH    BRANCH OF O88,O89,O90,O91,O92,O93,o94,o95,o96,o97,o98,o99,o00,o01,o02,o03,o04,o05,o06,o07,o08
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
.
O98      ADD       QUANT TO O98
         ADD       XQUANT TO X98
         ADD       RQUANT TO R98
         RETURN
.
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
;UPdate
O03      ADD       QUANT TO O03
         ADD       XQUANT TO X03
         ADD       RQUANT TO R03
         RETURN
O04      ADD       QUANT TO O04
         ADD       XQUANT TO X04
         ADD       RQUANT TO R04
         RETURN
.Patch 3.97
O05      ADD       QUANT TO O05
         ADD       XQUANT TO X05
         ADD       RQUANT TO R05
         RETURN
.Patch 3.97
.
.Patch 3.99
O06      ADD       QUANT TO O06
         ADD       XQUANT TO X06
         ADD       RQUANT TO R06
         RETURN
.Patch 3.99
.Patch 4.0
O07      ADD       QUANT TO O07
         ADD       XQUANT TO X07
         ADD       RQUANT TO R07
         RETURN
.Patch 4.0
.Begin Patch 4.1
O08      ADD       QUANT TO O08
         ADD       XQUANT TO X08
         ADD       RQUANT TO R08
         RETURN
.End Patch 4.1
FISCAL
;          alert caution,"Start fiscal",result
          KEYIN     *P1:1,*ES,*P10:15,*JR,"ENTER STARTING MONTH FISCAL YEAR ",*t15,*RV,*dv,FISCMO
;          alert caution,"End fiscal",result
;FISCAL   KEYIN     *P1:1,*ES,*P10:15,*JR,"ENTER STARTING MONTH FISCAL YEAR ":
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
.Start Patch #2.2 - replaced var
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
;patch3.91
        if (str1 = "1" or str1 = "5" or str1 = "6")
;        if (str1 = "1" or str1 = "5")
;patch3.91
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
.          	    pack      taskname from NTWKPATH2,"sort32 ","c:\listsum.dat ","c:\listsum.srt ":
.                         "/s (37,45,alp,a)"
.	             execute   taskname

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
        clear     N8
        move  C0 to newpg
.         move      portn to ncntfld1
.         rep       zfill in ncntfld1
.         call      ncntkey
.         if        over
.         	move      c3 to cntprint
.         endif
.Patch3.4
        Trap SPOOL1 giving error if SPOOL
        move comment to cntprint
        if (cntprint = c0)
                move c3 to cntprint
        endif
.EndPatch3.4

        Display    *P10:11,*EF,*cyan,"Printing in Process!!"
;patch3.93
; 	pack splfle,spldir,listnum,str4,".lst"
                    clock timestamp,timestamp
	pack splfle,ntwkpath1,luser,timestamp,".lst"
                    pack spoolname,luser,timestamp
;patch3.93
.===========================================================================
.Patch3.5
.Patch 3.97

		if (pdfopt = YES)
.>Patch 3.99 Logic Addition for PDF Quality Control
			call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
			"Parameters":
			"ProcessPDF":
			"\\nts0\c\apps\plb\code\pdftest.bat":
			result
			if (result = C0)
.Prepare Flag file
				prep	tempfile,"c:\progra~1\pdf995\flag.dat"
				write	tempfile,SEQ;"flag set"
				close	tempfile
			endif
.>Patch 3.99 Logic Addition for PDF Quality Control		
	              	PRTOPEN prfile,"PDF995","LstSum.pdf"
		else

	                if (cntprint = "1" | cntprint = "3")      .Laser6
;.                        PRTOPEN prfile,"\\NTS0\Laser6","Listsum"
	                        if (bosflag = c2)

	                        	PRTOPEN prfile,"\\nts0\Laser6",spoolname,noprint,spoolfile=splfle

;.                        	PRTOPEN prfile,"\\NTS0\Laser6","Listsum",noprint,spoolfile="c:\work\listsum.lst"
	                        else
;.                        	PRTOPEN prfile,"Laser6","Listsum",noprint,spoolfile="c:\work\listsum.lst"
	                        	PRTOPEN prfile,"Laser6",spoolname,noprint,spoolfile=splfle
	                        endif
	                else                                    .Laser3 = Default
	                        if (bosflag = c2)
;.                        PRTOPEN prfile,"\\NTS0\Laser3 Blankstock","Listsum"
	                        	PRTOPEN prfile,"\\NTS0\Laser3 Blankstock",spoolname,noprint,spoolfile=splfle
;.                        	PRTOPEN prfile,"\\NTS0\Laser3 Blankstock","Listsum",noprint,spoolfile="c:\work\listsum.lst"
	                        else
		                        PRTOPEN prfile,"Laser3 Blankstock",spoolname,noprint,spoolfile=splfle
;.	                        PRTOPEN prfile,"Laser3 Blankstock","Listsum",noprint,spoolfile="c:\work\listsum.lst"
                        	endif
                	endif
		endif
.Patch 3.97
;.===========================================================================
;.....        move c1 to prtname
;.                if (cntprint = "1" | cntprint = "3")      .Laser6
;.....                        PRTOPEN prfile,"\\NTS0\Laser6","Listsum"
;.                        PRTOPEN prfile,"\\NTS0\Laser6","Listsum",noprint,spoolfile="c:\work\listsum.lst"
;.                else                                    .Laser3 = Default
;.....                        PRTOPEN prfile,"\\NTS0\Laser3 Blankstock","Listsum"
;.                        PRTOPEN prfile,"\\NTS0\Laser3 Blankstock","Listsum",noprint,spoolfile="c:\work\listsum.lst"
;.                endif
;.        clear PgCnt
;.	OPEN      LISTFLE,"C:\LISTSUM.SRT",SHARE

PAGE
        if (newlst = c1)
                clear pgcnt
                clear rowcount
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
                clear mlrtot
                clear totord
        endif
.        clear     N8
.        ADD       C1 TO N8
.        DISPLAY   *P10:14,*EF,"Records Processed ",N8
.        READ      LISTFLE,SEQ;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01,o02,x02,r02
.        if over
.                GOTO LASTPAGE
.        	goto End1
.        endif
. UNPACK KEY80,str35,str45
.PAGE
        add c1 to Pgcnt
	if (pgcnt = c1)        
        	prtpage prfile;*UNITS=*HIENGLISH:
                       *ORIENT=*LANDSCAPE;	
	else
        	prtpage prfile;*NEWPAGE:
        	       *UNITS=*HIENGLISH:
                       *ORIENT=*LANDSCAPE;
        endif
.
.        if (SecondReq = YES)
.                PRTPAGE PRFILE;*PICTRECT=*OFF,*PICT=2655:8575:column:7600:pict1
.        endif
.
        clear   row
        move    "300",row
        prtpage prfile;*pcolumn:row,*font=font12,"Confidential";
.START PATCH 3.96 REPLACED LOGIC
.        prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News California Inc";
        prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News";
.END PATCH 3.96 REPLACED LOGIC
.        prtpage prfile;*pTitle1:row,*ALIGNMENT=*Left,*font=font12,"Names in the News California Inc";
        prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date:";
        clock timestamp,str8
        unpack str8,str2,yy,mm,dd
        clear str10
        pack  str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*pTitle3:row,*font=font12,str10;
       add     eightlpi,row
       add     eightlpi,row
.       add     eightlpi,row
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,*ULON,*ll,"Names Mailed on",*ULOFF;
.        prtpage prfile;*pTitle4:row,*font=font12,*ULON,*ll,"Names Mailed on",*ULOFF;
       add     eightlpi,row
       add     eightlpi,row
.       prtpage prfile;*pTitle1:row,*ALIGNMENT=*Left,*font=font12,*boldon,"List: ",*boldoff;
.       UNPACK KEY80,str35,str45
.       prtpage prfile;*ALIGNMENT=*Left,*font=font12,*boldon,str35,*boldoff;
       add     eightlpi,row
       add     eightlpi,row
       add     eightlpi,row
       add     eightlpi,row
       prtpage prfile;*pcolumn:row,*font=font12,*boldon,*ULON,"Mailer",*ULOFF,*boldoff;
.       goto Fiscal
        If (Mode = C2)
                goto Calendar
        Endif
.       Branch CALFLG to Calendar

Fiscal1
.       prtpage prfile;*pcolumn:row,*font=font12,*boldon,*ULON,"Mailer",*ULOFF,*boldoff;
       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY1,*uloff,*boldoff;
       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY2,*uloff,*boldoff;
       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY3,*uloff,*boldoff;
       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY4,*uloff,*boldoff;
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY5,*uloff,*boldoff;
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY6,*uloff,*boldoff;
       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Total",*uloff,*boldoff;
       goto begin

Calendar
.       prtpage prfile;*pcolumn:row,*font=font12,*boldon,*ULON,"Mailer",*ULOFF,*boldoff;

       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR1,*uloff,*boldoff;
       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR2,*uloff,*boldoff;
       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR3,*uloff,*boldoff;
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR4,*uloff,*boldoff;
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR5,*uloff,*boldoff;
       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Total",*uloff,*boldoff;





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
;Update
.Patch 3.99
.Patch 3.97
.       READ      LISTFLE,SEQ;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01,o02,x02,r02:
.                   o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06
.Patch 3.97
.Patch 3.99
.Patch 4.0
       READ      LISTFLE,SEQ;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01,o02,x02,r02:
.begin patch 4.1
.                   o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07
                   o03,x03,r03,o04,x04,r04,o05,x05,r05,o06,x06,r06,o07,x07,r07,o08,x08,r08
.end patch 4.1
.Patch 4.0
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
.       prtpage prfile;*pTitle1:lstrow,*ALIGNMENT=*Left,*font=font12,*boldon,"List: ",*boldoff;
       UNPACK KEY80,str35,str45
       move str35 to str40
       call trim using str40
		 pack str55 with "List: ",str40
       call trim using str55
       prtpage prfile;*pTitle1:lstrow,*ALIGNMENT=*CENTER,*font=font12,*boldon,*ll,str55,*boldoff;
.       prtpage prfile;*ALIGNMENT=*Left,*font=font12,*boldon,*ll,str40,*boldoff;
.            prtpage prfile;*pTitle1:mlrrow,*ALIGNMENT=*Left,*font=font12,*boldon,"Mailer: ",*boldoff;
.	    prtpage prfile;*font=font12,*boldon,*ll,holdnmlr,*boldoff;



.       if over
.                GOTO LASTPAGE
.        	goto End1
.       endif
.       Until (RowCount = "24")
.       until (row = "8130")
.       add     eightlpi,row
.       add     eightlpi,row
.Calendar2
.       UNPACK KEY80,str35,str45
.       prtpage prfile;*pcolumn:row,*font=font7,Str45;
.OrderTotal
       clear MLRTOT
        If (MODE = C1)
.       if (calflg = c0)
;UPdate
.Patch 3.99
.Patch 3.97
.Patch 4.0
.       		ADD o00 to MlrTot
.begin patch 4.1
.       		ADD o01 to MlrTot
.Patch 4.0
       		ADD o02 to MlrTot
       		ADD o03 to MlrTot
       		ADD o04 to MlrTot
       		ADD o05 to MlrTot
       		ADD o06 to MlrTot
       		ADD o07 to MlrTot
.       		ADD o08 to MlrTot
.end patch 4.1
.Patch 3.97
.Patch 3.99
       		if (MLRTOT > c0)
       			ADD MLRTOT to TotOrd
       			goto TotOrd
                else
                        goto Row2
       		endif
       else
;UPdate
.Patch 3.99
.Patch 3.97
.       		add o02 to MlrTot
.begin patch 4.1
.       		add o03 to MlrTot
       		add o04 to MlrTot
       		add o05 to MlrTot
       		add o06 to MlrTot
       		add o07 to MlrTot
       		add o08 to MlrTot
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
        If (MODE = C1)
.       if (calflg = c0)
;UPdate
.Patch 3.99
.Patch 3.97
.       ADD o00 to OYR96
.       ADD o01 to OYR97
.       ADD o02 to OYR98
.       ADD o03 to OYR99
.       ADD o04 to OYR00
.       ADD o05 to OYR01
.Patch 3.97
.Begin Patch 4.1
.Patch 4.0
.       ADD o01 to OYR96
.       ADD o02 to OYR97
.       ADD o03 to OYR98
.       ADD o04 to OYR99
.       ADD o05 to OYR00
.       ADD o06 to OYR01
       ADD o02 to OYR96
       ADD o03 to OYR97
       ADD o04 to OYR98
       ADD o05 to OYR99
       ADD o06 to OYR00
       ADD o07 to OYR01
.end Patch 4.1
       goto Exchg
.       add O97 To OYR97
       else
;UPdate
.Patch 3.99
.Patch 3.97
.       ADD o02 to OYR98
.       ADD o03 to OYR99
.       ADD o04 to OYR00
.       ADD o05 to OYR01
.       ADD o06 to OYR02
.Patch 3.97
.Patch 3.99
.Patch 4.0
.Begin Patch 4.1
.       ADD o03 to OYR98
.       ADD o04 to OYR99
.       ADD o05 to OYR00
.       ADD o06 to OYR01
.       ADD o07 to OYR02
       ADD o04 to OYR98
       ADD o05 to OYR99
       ADD o06 to OYR00
       ADD o07 to OYR01
       ADD o08 to OYR02
.end Patch 4.1
       endif
.Exchange Total-yearly
.       CLEAR XTOT
Exchg
        If (MODE = C1)
.       if (calflg = c0)
;Update
.Patch 3.99
.Patch 3.97
.       ADD X00 to XYR96
.       ADD X01 to XYR97
.       ADD X02 to XYR98
.       ADD X03 to XYR99
.       ADD X04 to XYR00
.       ADD X05 to XYR01
.Patch 4.0
.begin patch 4.1 
.       ADD X01 to XYR96
.       ADD X02 to XYR97
.       ADD X03 to XYR98
.       ADD X04 to XYR99
.       ADD X05 to XYR00
.       ADD X06 to XYR01
       ADD X02 to XYR96
       ADD X03 to XYR97
       ADD X04 to XYR98
       ADD X05 to XYR99
       ADD X06 to XYR00
       ADD X07 to XYR01
.end patch 4.1 
.Patch 3.97
.Patch 3.99
.       add X97 To XYR97
       goto Rent1
       endif
;UPdate
.Patch 3.99
.Patch 3.97
.       ADD X02 to XYR98
.       ADD X03 to XYR99
.       ADD X04 to XYR00
.       ADD X05 to XYR01
.       ADD X06 to XYR02
.Patch 4.0
.begin patch 4.1
.       ADD X03 to XYR98
.       ADD X04 to XYR99
.       ADD X05 to XYR00
.       ADD X06 to XYR01
.       ADD X07 to XYR02
       ADD X04 to XYR98
       ADD X05 to XYR99
       ADD X06 to XYR00
       ADD X07 to XYR01
       ADD X08 to XYR02
.end patch 4.1
.Patch 3.97
.Patch 3.99
.       ADD XTEMP to XTOT
Rent1
.Rental Total-yearly
.       CLEAR RTOT


        If (MODE = C1)
.       if (calflg = c0)
.       add O97 To RYR97
;UPdate
.Patch 3.99
.Patch 3.97
.       ADD R00 to RYR96
.       ADD R01 to RYR97
.       ADD R02 to RYR98
.       ADD R03 to RYR99
.       ADD R04 to RYR00
.       ADD R05 to RYR01
.patch 4.0
.begin patch 4.1
.       ADD R01 to RYR96
.       ADD R02 to RYR97
.       ADD R03 to RYR98
.       ADD R04 to RYR99
.       ADD R05 to RYR00
.       ADD R06 to RYR01
       ADD R02 to RYR96
       ADD R03 to RYR97
       ADD R04 to RYR98
       ADD R05 to RYR99
       ADD R06 to RYR00
       ADD R07 to RYR01
.end patch 4.1
       goto Print
       endif
.Patch 3.97
.Patch 3.99
;UPdate
.Patch 3.99
.Patch 3.97
.       ADD R02 to RYR98
.       ADD R03 to RYR99
.       ADD R04 to RYR00
.       ADD R05 to RYR01
.       ADD R06 to RYR02
.Patch 4.0
.begin patch 4.1
.       ADD R03 to RYR98
.       ADD R04 to RYR99
.       ADD R05 to RYR00
.       ADD R06 to RYR01
.       ADD R07 to RYR02
       ADD R04 to RYR98
       ADD R05 to RYR99
       ADD R06 to RYR00
       ADD R07 to RYR01
       ADD R08 to RYR02
.begin patch 4.1
.Patch 3.97
.Patch 3.99
.       add RTEMP to RTOT


Print
       add     eightlpi,row
       add     eightlpi,row
       UNPACK KEY80,str35,str45
       prtpage prfile;*pcolumn:row,*font=font7,Str45;
.       clear MLRTOT
   If (MODE = C1)
.    if (calflg = c0)
;UPdate
.Patch 3.97
.>Patch 3.98 Code Commented Out
.       if (O99 > c0)
.       	clear str10
.       	move O99 to Str10
.       	clear str13
.      	call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,O99;
.       endif
.>Patch 3.98 End Code Commented Out
.>Patch 3.98 Code Modified
        move mask15 to dim15a
.Patch 3.99        
.        edit O00 to dim15a
.Patch 3.99        
.Patch 4.0
.Begin Patch 4.1
.        edit O01 to dim15a
        edit O02 to dim15a
.End Patch 4.1
.Patch 4.0
	call trim using	dim15a
       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;	
       
.>Patch 3.98 End Code Modified 
;UPdate
.Patch 3.97
.>Patch 3.98 Code Commented Out
.       if (O00 > c0)
.       	clear str10
.       	move O00 to Str10
.       	clear str13
.      	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
..       ADD o98 to MlrTot
.       else
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,O00;
.       endif
.>Patch 3.98 Code Commented Out       
.>Patch 3.98 Code Modified

        move mask15 to dim15a
.Patch 3.99             
.        edit O01 to dim15a
.Patch 3.99             
.Patch 4.0
.Begin Patch 4.1
.        edit O02 to dim15a
        edit O03 to dim15a
.end Patch 4.1
.Patch 4.0    
	call trim using	dim15a
       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;	
       
.>Patch 3.98 End Code Modified 
;UPdate
.>Patch 3.98 Code Commented Out   
.       if (O01 > c0)
.       	clear str10
.       	move O01 to Str10
.       	clear str13
.      	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
..       ADD o99 to MlrTot
.       else
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,O01;
.       endif
.>Patch 3.98 Code Commented Out       
.>Patch 3.98 Code Modified

        move mask15 to dim15a
.Patch 3.99             
.        edit O02 to dim15a
.Patch 3.99             
.Patch 4.0    
.Begin Patch 4.1
.        edit O03 to dim15a
        edit O04 to dim15a
.end Patch 4.1
.Patch 4.0    
	call trim using	dim15a
       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;	
       
.>Patch 3.98 End Code Modified        
;UPdate
.>Patch 3.98 Code Commented Out       
.       if (O02 > c0)
.       	clear str10
.       	move O02 to Str10
.       	clear str13
.      	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
..       ADD o00 to MlrTot
.       else
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,O02;
.       endif
.>Patch 3.98 Code Commented Out       
.>Patch 3.98 Code Modified

        move mask15 to dim15a
.Patch 3.99             
.        edit O03 to dim15a
.Patch 3.99             
.Patch 4.0
.Begin Patch 4.1
.        edit O04 to dim15a
        edit O05 to dim15a
.end Patch 4.1
.Patch 4.0    
	call trim using	dim15a
       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;	
.>Patch 3.98 End Code Modified        
;UPdate
.>Patch 3.98 Code Commented Out 
.       if (O03 > c0)
.       	clear str10
.       	move O03 to Str10
.       	clear str13
.      	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
..       ADD o01 to MlrTot
.       else
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,O03;
.       endif
.>Patch 3.98 Code Commented Out       
.>Patch 3.98 Code Modified
.        move mask15 to dim15a
.Patch 3.99
.        edit O04 to dim15a
.Patch 3.99             
.Patch 4.0
.Begin Patch 4.1
.        edit O05 to dim15a
        edit O06 to dim15a
.end Patch 4.1
.Patch 4.0    
	call trim using	dim15a
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;	
.>Patch 3.98 End Code Modified        
;UPdate
.>Patch 3.98 Code Commented Out 
.       if (O04 > c0)
.       	clear str10
.       	move O04 to Str10
.       	clear str13
.      	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
..       ADD o02 to MlrTot
..       ADD MLRTOT to TotOrd
.       else
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,O04;
.       endif
.>Patch 3.98 Code Commented Out       
.>Patch 3.98 Code Modified
        move mask15 to dim15a
.Patch 3.99             
.        edit O05 to dim15a
.Patch 3.99             
.Patch 4.0
.Begin Patch 4.1
.       edit O06 to dim15a
        edit O07 to dim15a
.end Patch 4.1
.Patch 4.0        
	call trim using	dim15a
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;	
.>Patch 3.98 End Code Modified         
.>Patch 3.98 Code Commented Out       
.       if (MLRTOT > c0)
.       	clear str10
.       	move MLRTOT to Str10
.       	clear str13
.      	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,Str13;
.       else
.        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,MlrTot;
.       endif
.        goto Row
.    Endif
.>Patch 3.98 Code Commented Out       
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit MLRTOT to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;	
        goto Row        
    Endif        
.>Patch 3.98 End Code Modified 
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,O97;
;UPdate
.Patch 3.97
.>Patch 3.98 Code Commented Out       
.       if (O01 > c0)
.       	clear str10
.       	move O01 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,O01;
..       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,O98;
..       ADD o98 to MlrTot
.       endif
.>Patch 3.98 Code Commented Out       
.>Patch 3.98 Code Modified
        move mask15 to dim15a
.Patch 3.99             
.        edit O02 to dim15a
.Patch 3.99             
.Patch 4.0
.Begin Patch 4.1
.        edit O03 to dim15a
        edit O04 to dim15a
.end Patch 4.1
.Patch 4.0    
	call trim using	dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;	
.>Patch 3.98 End Code Modified        
;UPdate
.>Patch 3.98 Code Commented Out       
.       if (O02 > c0)
.       	clear str10
.       	move O02 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
..       ADD o99 to MlrTot
.       else
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,O02;
.       endif
.>Patch 3.98 Code Commented Out       
.>Patch 3.98 Code Modified
        move mask15 to dim15a
.Patch 3.99         
.        edit O03 to dim15a
.Patch 3.99         
.Patch 4.0
.Begin Patch 4.1
.        edit O04 to dim15a
        edit O05 to dim15a
.end Patch 4.1
.Patch 4.0    
	call trim using	dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;	
.>Patch 3.98 End Code Modified         
;UPdate
.>Patch 3.98 Code Commented Out       
.       if (O03 > c0)
.       	clear str10
.       	move O03 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,O03;
.       endif
.>Patch 3.98 Code Commented Out       
.>Patch 3.98 Code Modified
        move mask15 to dim15a
.Patch 3.99         
.        edit O04 to dim15a
.Patch 3.99         
.Patch 4.0
.Begin Patch 4.1
.        edit O05 to dim15a
        edit O06 to dim15a
.end Patch 4.1
.Patch 4.0    
	call trim using	dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;	
.>Patch 3.98 End Code Modified  
.       ADD o00 to MlrTot
;UPdate
.>Patch 3.98 Code Commented Out 
.       if (O04 > c0)
.       	clear str10
.       	move O04 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,O04;
.       endif
.>Patch 3.98 Code Commented Out       
.>Patch 3.98 Code Modified
        move mask15 to dim15a
.Patch 3.99         
.        edit O06 to dim15a
.Patch 3.99         
.Patch 4.0
.Begin Patch 4.1
.        edit O06 to dim15a
        edit O07 to dim15a
.end Patch 4.1
.Patch 4.0       
	call trim using	dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;	
.>Patch 3.98 End Code Modified 
.       ADD o01 to MlrTot
;UPdate
.>Patch 3.98 Code Commented Out       
.       if (O05 > c0)
.       	clear str10
.       	move O05 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,O05;
.       endif
.>Patch 3.98 Code Commented Out       
.>Patch 3.98 Code Modified
        move mask15 to dim15a
.Patch 3.99         
.        edit O06 to dim15a
.Patch 3.99         
.Patch 4.0    
.Begin Patch 4.1
.        edit O07 to dim15a
        edit O08 to dim15a
.end Patch 4.1
.Patch 4.0    
	call trim using	dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;	
.>Patch 3.98 End Code Modified        
.       ADD o02 to MlrTot
.       ADD MLRTOT to TotOrd
.>Patch 3.98 Code Commented Out 
.       if (MlrTot > c0)
.       	clear str10
.       	move MlrTOT to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,str13;
.       else
.        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,MlrTot;
.       Endif
.>Patch 3.98 Code Commented Out        
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit MlrTot to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,dim15a;	
.>Patch 3.98 End Code Modified  
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
       prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
;Update
       prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
.Begin Patch 4.1
.Patch 4.0
.;       			"©","1991-2006, Names in the News";
.       			"©","1991-2007, Names in the News";
       			"©","1991-2008, Names in the News";
.end Patch 4.1
.Patch 4.0
.;       			"©","1991-2003, Names in the News/CA";
.           prtpage prfile;*p9500:row,*font=font12,"Page ";
.       	   prtpage prfile;*font=font12,PgCnt;

.End Patch 1.1a
           goto Page
       endif

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
;Update
       clear    O04
       clear    X04
       clear    R04
.Patch 3.97
       clear    O05
       clear    X05
       clear    R05
.Patch 3.99 
       clear    O06
       clear    X06
       clear    R06

.Patch 4.0
       clear    O07
       clear    X07
       clear    R07
.Begin Patch 4.1
       clear    O08
       clear    X08
       clear    R08
.end Patch 4.1
.       ADD       C1 TO N8
.       DISPLAY   *P10:14,*EF,"Records Processed ",N8
.       READ      LISTFLE,SEQ;B1,KEY80,O88,X88,R88,O89,X89,R89,O90,X90,R90:
.                   O91,X91,R91,O92,X92,R92,O93,X93,R93,O94,X94,R94,o95,x95,r95:
.                   o96,x96,r96,o97,x97,r97,o98,x98,r98,o99,x99,r99,o00,x00,r00,o01,x01,r01,o02,x02,r02

.       repeat until over
        repeat
LastPage
.Patch3.6
.       if (ROWCOUNT < "18")
       if (ROWCOUNT < "20")
.EndPatch3.6
.           add     eightlpi,row
.           add     eightlpi,row

       goto totals
.           move "7860",row
.           prtpage prfile;*p9500:row,*font=font12,"Page ";
.       	   prtpage prfile;*p9500:row,*font=font12,PgCnt;
.           goto end1
        else

.Patch3.6=======================================================================

       move "7750",row
       prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
       prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
;update
.Patch 3.97
       prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
;       			"©","1991-2006, Names in the News";
       			"©","1991-2008, Names in the News";
;       			"©","1992-2004, Names in the News/CA";
.EndPatch3.6====================================================================

                        move c1 to newpg
.Patch3.6=======================================================================
                        add c1 to Pgcnt
.EndPatch3.6====================================================================
                	prtpage prfile;*NEWPAGE:
        		       *UNITS=*HIENGLISH:
                       	       *ORIENT=*LANDSCAPE;
                goto Totals
       endif
.           move "7860",row
.           prtpage prfile;*p9500:row,*font=font12,"Page ";
.       	   prtpage prfile;*font=font12,PgCnt;
.           goto end1

.        prtpage prfile;*NEWPAGE:
.        	       *UNITS=*HIENGLISH:
.                       *ORIENT=*LANDSCAPE;

Totals
       if (newpg = c1)
	        clear   row
        	move    "300",row
	        prtpage prfile;*pcolumn:row,*font=font12,"Confidential";
.START PATCH 3.96 REPLACED LOGIC
.        	prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News California Inc";
        	prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News";
.END PATCH 3.96 REPLACED LOGIC
.        	prtpage prfile;*pTitle1:row,*ALIGNMENT=*Left,*font=font12,*ll,"Names in the News California Inc";
	        prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date:";
        	clock timestamp,str8
	        unpack str8,str2,yy,mm,dd
        	clear str10
	        pack  str10,mm,slash,dd,slash,str2,yy
        	prtpage prfile;*pTitle3:row,*font=font12,str10;
	        add     eightlpi,row
       	        add     eightlpi,row
.	        add     eightlpi,row
	        prtpage prfile;*pTitle4:row,*ALIGNMENT=*Center,*font=font12,*ULON,"Names Mailed on",*ULOFF;
.	        prtpage prfile;*pTitle4:row,*font=font12,*ULON,"Names Mailed on",*ULOFF;
	        add     eightlpi,row
	        add     eightlpi,row
.	        prtpage prfile;*pTitle1:row,*ALIGNMENT=*Left,*font=font12,*boldon,"List: ",*boldoff;
.	        UNPACK KEY80,str35,str45
                move holdlst to str40
                call trim using str40
					 pack str55 with "List: ",str40
                call trim using str55
                prtpage prfile;*pTitle1:row,*ALIGNMENT=*Center,*font=font12,*boldon,str55,*boldoff;
.		          prtpage prfile;*ALIGNMENT=*Left,*font=font12,*boldon,str40,*boldoff;
       		move "1380",row
       else
.		add     eightlpi,row
.		add     eightlpi,row
.		add     eightlpi,row
.		add     eightlpi,row
.patch3.7
                goto    TOTAL1
       endif
       if (MODE = c2)
                goto TotCalendar
       Endif

.       branch Calflg to TotCalendar
TotFiscal
       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY1,*uloff,*boldoff;
       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY2,*uloff,*boldoff;
       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY3,*uloff,*boldoff;
       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY4,*uloff,*boldoff;
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY5,*uloff,*boldoff;
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,FY6,*uloff,*boldoff;
       prtpage prfile;*pcolumn7:row,*font=font12,*boldon,*ulon,"Total",*uloff,*boldoff;
       goto Total1


TotCalendar
       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR1,*uloff,*boldoff;
       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR2,*uloff,*boldoff;
       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR3,*uloff,*boldoff;
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR4,*uloff,*boldoff;
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,YR5,*uloff,*boldoff;
       prtpage prfile;*pcolumn7:row,*font=font12,*boldon,*ulon,"Total",*uloff,*boldoff;

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
       prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ULON,"Grand Totals",*ULOFF,*boldoff;
        If (MODE = C1)
.>Patch 3.98 Code Commented Out          
.     If (CALFLG = c0)
.       if (OYR96 > c0)
.       	clear str10
.       	move OYR96 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR96,*ULOFF,*boldoff;
.       endif
.>Patch 3.98 Code Commented Out       
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit OYR96 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified        
.>Patch 3.98 Code Commented Out       
.       if (OYR97 > c0)
.       	clear str10
.       	move OYR97 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR97,*ULOFF,*boldoff;
.       endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit OYR97 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified           
.>Patch 3.98 Code Commented Out              
.       if (OYR98 > c0)
.       	clear str10
.       	move OYR98 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR98,*ULOFF,*boldoff;
.       endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit OYR98 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified         
.>Patch 3.98 Code Commented Out              
.       if (OYR99 > c0)
.       	clear str10
.       	move OYR99 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR99,*ULOFF,*boldoff;
.       endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit OYR99 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified          
.>Patch 3.98 Code Commented Out              
.       if (OYR00 > c0)
.       	clear str10
.       	move OYR00 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR00,*ULOFF,*boldoff;
.       endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit OYR00 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified         
.>Patch 3.98 Code Commented Out              
.       if (OYR01 > c0)
.       	clear str10
.       	move OYR01 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR01,*ULOFF,*boldoff;
.       endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit OYR01 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified         
       goto Next
    endif
.>Patch 3.98 Code Commented Out                  
.       if (OYR98 > c0)
.       	clear str10
.       	move OYR98 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR98,*ULOFF,*boldoff;
.       endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit OYR98 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified          
.>Patch 3.98 Code Commented Out              
.       if (OYR99 > c0)
.       	clear str10
.       	move OYR99 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR99,*ULOFF,*boldoff;
.       endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit OYR99 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified         
.>Patch 3.98 Code Commented Out              
.       if (OYR00 > c0)
.       	clear str10
.       	move OYR00 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR00,*ULOFF,*boldoff;
.       endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit OYR00 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified 
.>Patch 3.98 Code Commented Out 
.       if (OYR01 > c0)
.       	clear str10
.       	move OYR01 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR01,*ULOFF,*boldoff;
.       endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit OYR01 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified
.>Patch 3.98 Code Commented Out              
.       if (OYR02 > c0)
.       	clear str10
.       	move OYR02 to Str10
.       	clear str13
.       	call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OYR02,*ULOFF,*boldoff;
.       endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit OYR02 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified       

Next
        If (MODE = C1)
.    If (CALFLG = c0)
       add OYR96 to OTOT
       add OYR97 to OTOT
       add OYR98 to OTOT
       add OYR99 to OTOT
       add OYR00 to OTOT
       add OYR01 to OTOT
       goto TOT
    Endif
       add OYR98 to OTOT
       add OYR99 to OTOT
       add OYR00 to OTOT
       add OYR01 to OTOT
       add OYR02 to OTOT
TOT
.>Patch 3.98 Code Commented Out              
.       if (OTOT > c0)
.        clear str10
.        move OTOT to Str10
.        clear str13
.        call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,OTOT,*ULOFF,*boldoff;
.       endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit OTOT to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified         
       add     eightlpi,row
       add     eightlpi,row
       prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ULON,"Exchange Totals",*ULOFF,*boldoff;
        If (MODE = C1)
.   If (CALFLG = c0)
.>Patch 3.98 Code Commented Out              
.      if (XYR96 > c0)
.       clear str10
.       move XYR96 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR96,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit XYR96 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified        
.>Patch 3.98 Code Commented Out              
.      if (XYR97 > c0)
.       clear str10
.       move XYR97 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR97,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit XYR97 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified       
.>Patch 3.98 Code Commented Out              
.      if (XYR98 > c0)
.       clear str10
.       move XYR98 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR98,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit XYR98 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified        
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR98,*ULOFF;
.>Patch 3.98 Code Commented Out              
.      if (XYR99 > c0)
.       clear str10
.       move XYR99 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR99,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit XYR99 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified       
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR99,*ULOFF;
.>Patch 3.98 Code Commented Out              
.      if (XYR00 > c0)
.       clear str10
.       move XYR00 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR00,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit XYR00 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified        
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR00,*ULOFF;
.>Patch 3.98 Code Commented Out              
.      if (XYR01 > c0)
.       clear str10
.       move XYR01 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR01,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit XYR01 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified       
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR01,*ULOFF;
       goto Xyear
   Endif
.>Patch 3.98 Code Commented Out    
.      if (XYR98 > c0)
.       clear str10
.       move XYR98 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR98,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit XYR98 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified       
.>Patch 3.98 Code Commented Out              
.      if (XYR99 > c0)
.       clear str10
.       move XYR99 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.       else
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR99,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit XYR99 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified             
.>Patch 3.98 Code Commented Out              
.      if (XYR00 > c0)
.       clear str10
.       move XYR00 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR00,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit XYR00 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified         
.>Patch 3.98 Code Commented Out              
.      if (XYR01 > c0)
.       clear str10
.       move XYR01 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR01,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit XYR01 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified        
.>Patch 3.98 Code Commented Out              
.      if (XYR02 > c0)
.       clear str10
.       move XYR02 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,XYR02,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit XYR02 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified       
Xyear
        If (MODE = C1)
.       If (CALFLG = c0)
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
.>Patch 3.98 Code Commented Out              
.      if (XTOT > c0)
.        clear str10
.        move XTOT to Str10
.        clear str13
.        call FormatNumeric using str10,str13,comma
.        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,str13,*ULOFF,*boldoff;
.       else
.        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,XTOT,*ULOFF,*boldoff;
.       endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit XTOT to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified         
       add     eightlpi,row
       add     eightlpi,row
       prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ULON,"Rental Totals",*ULOFF,*boldoff;
        If (MODE = C1)
.       If (CALFLG = c0)
.>Patch 3.98 Code Commented Out              
.      if (RYR96 > c0)
.       clear str10
.       move RYR96 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR96,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit RYR96 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified         
.>Patch 3.98 Code Commented Out              
.      if (RYR97 > c0)
.       clear str10
.       move RYR97 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR97,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit RYR97 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified       
.>Patch 3.98 Code Commented Out              
.      if (RYR98 > c0)
.       clear str10
.       move RYR98 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR98,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit RYR98 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified      
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR98,*ULOFF;
.>Patch 3.98 Code Commented Out              
.      if (RYR99 > c0)
.       clear str10
.       move RYR99 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.     else
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR99,*ULOFF;
.     endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit RYR99 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified       
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR99,*ULOFF;
.>Patch 3.98 Code Commented Out              
.      if (RYR00 > c0)
.       clear str10
.       move RYR00 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.     else
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR00,*ULOFF;
.     endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit RYR00 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified      
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR00,*ULOFF;
.>Patch 3.98 Code Commented Out              
.      if (RYR01 > c0)
.       clear str10
.       move RYR01 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR01,*ULOFF;
.      Endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit RYR01 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified        
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR01,*ULOFF;
        goto Rent2
       ENDIF
.>Patch 3.98 Code Commented Out                     
.      if (RYR98 > c0)
.       clear str10
.       move RYR98 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR98,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit RYR98 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified       
.>Patch 3.98 Code Commented Out              
.      if (RYR99 > c0)
.       clear str10
.       move RYR99 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.     else
.       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR99,*ULOFF;
.     endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit RYR99 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified      
.>Patch 3.98 Code Commented Out              
.      if (RYR00 > c0)
.       clear str10
.       move RYR00 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.     else
.       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR00,*ULOFF;
.     endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit RYR00 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified       
.>Patch 3.98 Code Commented Out              
.      if (RYR01 > c0)
.       clear str10
.       move RYR01 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR01,*ULOFF;
.      Endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit RYR01 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified        
.>Patch 3.98 Code Commented Out              
.      if (RYR02 > c0)
.       clear str10
.       move RYR02 to Str10
.       clear str13
.       call FormatNumeric using str10,str13,comma
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,str13,*ULOFF;
.      else
.       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*ULON,RYR02,*ULOFF;
.      endif
.>Patch 3.98 Code Commented Out              
.>Patch 3.98 Code Modified
        move mask15 to dim15a
        edit RYR02 to dim15a
	call trim using	dim15a
        prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,dim15a,*ULOFF,*boldoff;	
.>Patch 3.98 End Code Modified        
Rent2
        If (MODE = C1)
.       If (CALFLG = c0)

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
       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,str13,*ULOFF,*boldoff;
      else
       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font8,*boldon,*ULON,*ll,RTOT,*ULOFF,*boldoff;
      endif
.Patch 1.1
       move "7750",row
       prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
       prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
       prtpage prfile;*p7510:row,*font=font9,*ALIGNMENT=*Left:
;       			"©","1991-2006, Names in the News";
       			"©","1991-2008, Names in the News";
;       			"©","1991-2004, Names in the News/CA";
.EndPatch 1.1
.       move "7860",row
.       prtpage prfile;*p9500:row,*font=font12,"Page ";
.       prtpage prfile;*font=font12,PgCnt;

End1
       Display    *P10:11,*EF,*cyan,"Printing Finished!!"
       PRTCLOSE prfile
;       alert caution,"were closing",result
       Pause c1
.Patch3.3.................................................
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
;patch3.93
        if (pdfopt = YES)
                  goto pdfthis
        endif
;patch3.93
        loop
        	until (N3 = COPY)

                if (cntprint = "1" | cntprint = "3")      .Laser6
			PRTPLAY "c:\work\listsum.lst","Laser6"
                else
			PRTPLAY "c:\work\listsum.lst","Laser3 Blankstock"
                endif
                add c1 to N3
        repeat
.       goto End2
        goto Checknew
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
;.			PRTPLAY "c:\work\listsum.lst","\\NTS0\Laser6"
                else
			PRTPLAY splfle,"\\NTS0\Laser3 Blankstock"
;.			PRTPLAY "c:\work\listsum.lst","\\NTS0\Laser3 Blankstock"
                endif
                add c1 to N3
        repeat
;.======================================================================
;.        loop
;.        	until (N3 = COPY)
;.                if (cntprint = "1" | cntprint = "3")      .Laser6
;.			PRTPLAY "c:\work\listsum.lst","\\NTS0\Laser6"
;.                else
;.			PRTPLAY "c:\work\listsum.lst","\\NTS0\Laser3 Blankstock"
;.                endif
;.                add c1 to N3
;.        repeat
;.        trapclr Spool
;...............................................................
;.Patch3.2a
;.Loop to do multiple copies
;.        add c1 to n3
;.        if (copy = c0)
;.                goto End2
;.        endif
;.	if (n3 <> copy)
;.
;.
;.
;.       	goto Rerun
;.        endif
;............................
checknew
       if (newlst = c1)
                if (pdfopt <> YES)
                     erase splfle
                endif
;.               erase "c:\work\mlrsum.lst"
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
;        erase splfle
;.        erase "c:\work\listsum.lst"
        shutdown  "cls"
	stop

.Patch3.5
.===================================================
Spool1
         Display    *P10:11,*EF,*cyan,error
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

         PAUSE c2
         CLOSE     INFILE
         CLOSE     OUTPUT
         NORETURN
         shutdown  "cls"
         STOP
.================================================
;patch3.93
pdfthis
.Patch 3.99
	pack	APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
	loop
		call	FindFirstFile
		until (APIResult = 0 | APIResult = hexeight)
		pause	"1"
	repeat
	pause	"2"
	erase	"c:\progra~1\pdf995\flag.dat"	
.Patch 3.99
.Patch 3.97
.Patch 3.99 Comment Out
.		pack	APIFileName,"c:\work\pdf\lstsum.pdf",hexzero
.		call	FindFirstFile
.		if (APIResult <> 0 & APIResult <> hexeight)
.			clock   timestamp,timestamp1
.			move    timestamp1,time1
.			loop
.				move	C0,N1
.				trap	IOTrap if IO
.				open	tempfile,"c:\work\pdf\lstsum.pdf"
.				trapclr	IO
.				if (N1 = C0)
.					close	tempfile
.					break
.				endif
.				clock   timestamp,timestamp
.				move    timestamp,time2
.				sub     time1,time2,time3
.				if (time3 > 2000) .20 Seconds Maximum
.					break
.				endif
.			repeat
.Patch 3.99 Comment Out
	move    "Here is your PDF File",SmtpSubject Subject
.   Set the text message that is send with the attachments
	move    "lstsum.pdf",SmtpTextMessage(1)   Array <Text message >
	move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
	move    "NTS4",SmtpEmailServer                   Address of email serverc
	clear   smtpemailaddress
	append  luser,SmtpEmailAddress
	append  "@nincal.com",SmtpEmailAddress
	reset   smtpemailaddress
	move    luser,SmtpUserName                                User name
	move    luser,SmtpUserFullName                                User name
.   Set the destinations of the email. Max 100 (Mime spec)
	move    smtpemailaddress,SmtpDestinations(1,1)
	move    luser,SmtpDestinations(1,2)
	move    "1",SmtpDestIndexLast                          originators UserName
	move    "lstsum.pdf",SmtpAttachments(1,1)                     Attached file name
	move    "C:\WORK\pdf",SmtpAttachments(1,2)           Path to attached file name
	move    "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
	move    "C:\work\eMail.Log",SmtpLogFile          Path/filename to Log all socket read/writes
	clear   SmtpLogFile                                         'Clear' disables the LogFile
	move    "1",SmtpProgress                                    Enable progress bars
	call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
	if not equal
		pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
		"Status Code ",SmtpStatus," - ",SmtpStatusText
		move    "PDF File not found",SmtpSubject Subject
		move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
		call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
	                Alert  Stop,Mess,F1,"PL/B emailer - ERROR"
	else
.	                Alert  Stop,"eMail transmitted OK",F1,"PL/B emailer"
	endif
.Clean up afterwards
	pause c7
	erase	"c:\work\pdf\lstsum.pdf"
.			endif
.Patch 3.97
.         clear     taskname
.         append    "c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 f:\apps\winbatch\butil job=O34 INfile=",TASKNAME
.         APPEND    spoolname TO TASKNAME
.         APPEND    " B=",TASKNAME
.         APPEND    luser TO TASKNAME
.         reset     taskname
;          alert caution,taskname,result
.         EXECUTE   TASKNAME
.Patch 3.97
         goto checknew
;patch3.93
.................................................
.Patch 3.99
IOTrap
	move	C1,N1
	return
.Patch 3.99

        include   ncntio.inc
.patch3.95
				include	compio.inc
				include	cntio.inc
.         INCLUDE   NMLRIO.inc
.patch3.95
         INCLUDE   NDATIO.inc
        include comlogic.inc

