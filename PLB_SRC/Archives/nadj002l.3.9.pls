PC       EQU       0
         INC       COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   CONSacct.inc
         INCLUDE   NORDDD.inc
.START PATCH 3.71 REPLACED LOGIC
.         INCLUDE   NMLRDD.inc
.         include   nbrkdd.inc
	INCLUDE	COMPDD.inc
	INCLUDE	CNTDD.inc
.END PATCH 3.71 REPLACED LOGIC
         INCLUDE   NBILDD.inc
         INCLUDE   NJSTDD.inc
         include   ndatdd.inc
         INCLUDE   NOWNDD.inc
         INCLUDE   NAcdDD.inc
         INCLUDE   NADJDD.inc
;begin patch 3.8
;         INCLUDE   NINVDD.inc
         INCLUDE   	ninvdd.inc
         include	Ninvacddd.inc
;end patch 3.8
         include   ndat3dd.inc
         INCLUDE   NPAYDD.INC
.begin patch 3.4
         include   nmrgdd.inc
         include   nshpdd.inc
.end patch  3.4
.begin patch 4.2
	INclude	Nadjres.inc
.begin patch 4.2
.START PATCH 3.7 ADDED LOGIC
	INCLUDE	NSEL2DD.INC
.END PATCH 3.7 ADDED LOGIC
         INCLUDE   HP.INC
release 	init    	"4.2"	DLH MOve adj codes to INclude
reldate	Init	"23 September 08"
.release 	init    	"4.1"	DLH 1AUG2007 pli
.release 	init    	"4.0"	DLH 1AUG2007 pli
.release init    "3.9"	DLH 30Jul2007 code 29
.release init    "3.84"	DLH 6Jun2007 Oslspern update
.release init    "3.83"	ASH 16MAY2006 RECONCILIATION OF LANGUAGE BETWEEN:  NCSH002A, NORD002L, NINV002L, NADJ002L, NORD0024, NORD024B
.release  init      "3.82"        JD	2006APR21 Added Net Arrangement note to MSWORD DOC.
.release  init      "3.81"        ASH	2005NOV18	Patch from Invoice FIle Conversion
.release  init      "3.8"        DLH	2005March02	Invoice FIle Conversion
.release  init      "3.79"        ASH	11JAN2005	Added code to prevent Inactive Contacts from printing.
.release  init      "3.78"              DMB  04JAN2005 Added code to correctly display splits qty and per m charge
.release  init      "3.77"              DMB  14OCT2004 Added code for Volume discount verbage w/o: 277
.release  init      "3.76"              ASH  05AUG2004 Logo Conversion
.release  init      "3.75"              JD  14Jul2004 fixed L/O copy.
.release  init      "3.72"             DMB 21JUN2004 Changed mlr reads to company reads to allow for long address lengths
.release  init      "3.71"             ASH 27MAY2004 MAILER CONVERSION
.release  init      "3.7"             ASH 26JAN04 DATACARD CONVERSION
.release  init      "3.66"             ASH 06MAR03 ADDED OPTION TO PRINT INVOICE VIA MS WORD
.release  init      "3.65"             JD14sep03 only print 1 copy
.release  init      "3.64"             JD07sep01 added adjres code 36.
.release   init      "3.63"           23mar2001 JD Fixed adj # print on mlr copy daily run.
.release   init      "3.63"          03OCT2000 ASH NEW SERVER ADDED
.release   init      "3.61"          13APr00 JD forced z3 on mailer contact field.
.release   init      "3.61"          15Mar00 DLH
.release   init      "3.6"          25oct99 DLH Use new PLB Form
.release   init      "3.5.1"          14oct99 DLH date handling was wrong
.release  init      "3.5"            26Apr99 DLH ninadj & nadjust y2k
.release  init      "3.4"            26Apr99 DLH nininv y2k
.release  init      "3.3"            28Dec98 ASH NINORD Y2K, File expansion VAR CHANGE IN CONSACCT.INC/COMPUTE.INC
.release  init      "3.2"            09sep96 DLH add reason codes 33 & 34
.release  init      "3.1a"           23jul96 jd if rdclr=yes we are hosed.
.release  init      "3.1"           19jul95 DLH hot print
.release  init      "3.0"           16dec94 dlh
.release  init      "2.8"           28NOV94  CHK FOR PAY-TO ON OWNER COPY.
.release  init      "2.7"          02nov94 dlh add mlr bill direct.
.RELEASE  INIT      "2.6"           print own add,
.RELEASE  INIT      "2.5"           jd 18aug94 convert to laser.
.RELEASE   INIT      "2.4"          JD  USE BROKER INCLUDE.
.RELEASE   INIT      "2.3"         JD  31MAR93 ADJ PRINT FOR NEW FORMS.
.                                 ALSO ADDED PRINT DUE OWNER.
.RELEASE  INIT      "2.2"         DLH 03MAR93 STAX PROB, OVER PROB, CLEAR
.                                CVTFLD BEFORE MOVE OV VAR'S (WRONG AMOUNT PRTD)
.RELEASE  INIT      "2.1"         DLH 08FEB93 FIXED PRINT TAB POSITIONS.
.RELEASE  INIT      "2.0"        DLH 19MAR92 NORDXX, NMLRXX, NBILXX, NJSTXX
.begin patch 4.1
GreyFIll       Color
NoFIll         Color                   .White
colornum form	24
.end patch 4.1
COMNT    DIM       25
COMNT1   DIM       21
REASON   FORM      1
ADJDATE  DIM       8
INVDATE  DIM       8
DATE     DIM       8
LOCAL    INIT      "LOCAL"
HOTFLAG  FORM       1                 "1=daily print, 2=hot print"
PRTFLAG  DIM       1
.Start patch #3.3 - increase var, although var does not seem to be used!!!!
.MAILDATE DIM       8
MAILDATE DIM       10
.End patch #3.3 - increase var, although var does not seem to be used!!!!
apflag   form      1
.START Patch #3.3 - increased var
.ARMASK   DIM       13
.AP1MASK  DIM       13
.AP2MASK  DIM       13
.M$GROSS  DIM       13
ARMASK   DIM       15
AP1MASK  DIM       15
AP2MASK  DIM       15
M$GROSS  DIM       15
.END Patch #3.3 - increased var
PSMASK   DIM       9
STMASK   DIM       10
CTYMASK  DIM       10
LRMASK   DIM       11
TYPIST   DIM       2             TYPIST INITIALS
.begin patch 3.5
.amendno  form      1
amendno  form      2
.end patch 3.5
.
.begin  patch 3.5
shipsw    dim       1
mrgsw     dim       1
.edn patch 3.5
.begin patch 3.4
.Invoice ADDITIONAL CHARGE DESCRIPTION.
..
.ADDESC1  DIM       35
.ADDESC2  DIM       35
.ADDESC3  DIM       35
.ADDESC4  DIM       35
.ADDESC5  DIM       35
.ADDESC6  DIM       35
.ADDESC7  DIM       35
..ADDESC8  DIM       35
.ADDESC9  DIM       35
.ADDESC10 DIM       35
..
.ATPRT    DIM       1
.AT1      DIM       1      ADDITION CHARGE PRINT LINE VAR.
.AT2      DIM       1      ADDITION CHARGE PRINT LINE VAR.
.AT3      DIM       1      ADDITION CHARGE PRINT LINE VAR.
..AT4      DIM       1      ADDITION CHARGE PRINT LINE VAR.
.AT5      DIM       1      ADDITION CHARGE PRINT LINE VAR.
.AT6      DIM       1      ADDITION CHARGE PRINT LINE VAR.
.AT7      DIM       1      ADDITION CHARGE PRINT LINE VAR.
.AT8      DIM       1      ADDITION CHARGE PRINT LINE VAR.
.AT9      DIM       1      ADDITION CHARGE PRINT LINE VAR.
.AT10     DIM       1      ADDITION CHARGE PRINT LINE VAR.
..
...ADDITIONAL CHARGE RATE.
..
.ADD$RTE  DIM       6
.ADD$RT1  DIM       6
.ADD$RT2  DIM       6
.ADD$RT3  DIM       6
.ADD$RT4  DIM       6
.ADD$RT5  DIM       6
..ADD$RT6  DIM       6
.ADD$RT7  DIM       6
.ADD$RT8  DIM       6
.ADD$RT9  DIM       6
.ADD$RT10 DIM       6
..
.. TOTAL ADDITIONAL CHARGE
..
...START Patch #3.3 - increased var
..ADD$1    DIM       13
..ADD$2    DIM       13
..ADD$3    DIM       13
..ADD$4    DIM       13
..ADD$5    DIM       13
..ADD$6    DIM       13
..ADD$7    DIM       13
...ADD$8    DIM       13
..ADD$9    DIM       13
..ADD$10   DIM       13
....
.ADD$1    DIM       15
.ADD$2    DIM       15
.ADD$3    DIM       15
.ADD$4    DIM       15
..ADD$5    DIM       15
.ADD$6    DIM       15
.ADD$7    DIM       15
.ADD$8    DIM       15
.ADD$9    DIM       15
.ADD$10   DIM       15
..END Patch #3.3 - increased var
..
.end patch 3.4
.begin patch 4.2
...adjustment reason code descriptions
..
.adjres1  init      "Adjustment to Quantity"
.adjres2  init      "Shipping"
.adjres3  init      "Selection fee"
.adjres4  init      "Running Charges"
.adjres5  init      "Change in price"
.adjres6  init      " "
.adjres7  init      "Adjust Within A/P "
.adjres8  init      "Adjust A/R & LR "
.adjres9  init      "Adjust A/P & LR"
.adjres10  init      "Cancel entire Bill"
.adjres11  init      "No invoice A/P to Lr "
.adjres12  init      "Late Lo inv LR to A/P "
.adjres13  init      "Adjustment of Income"
.adjres14  init      "Advance Payment to LO"
.adjres15  init      "Adjustment of Tax"
.adjres16  init      "Short Payment"
.adjres17  init      "Commission"
.adjres18  init      "Postage"
.adjres19  init      "Direct Payment to LO"
.adjres20  init      " "
.adjres21  init      "Advance Payment to LO"
.adjres22  init      "Reduction of A/R"
.adjres23  init      "Reduction of A/P (Contra)"
.adjres24  init      "Discount Earned"
.adjres25  init      "Additional Billing"
.adjres26  init      "Write off of A/R"
.adjres27  init      "Prepayment"
.adjres28  init      "Write off of A/P"
.adjres29  init      "Canadian withholding tax"
..adjres29  init      "Canadian 10% withholding tax"
..adjres29  init      "                           "
.adjres30  init      "Taking Credit-Original open"
.adjres31  init      "Credit Transfer"
.adjres32  init      "Refund Credit Taken"
.adjres33  init      "Cancelled/Billing Adjusted "
.adjres34  init      "Adj due to Order Change    "
.adjres35  init      "Court Imposed Bankruptcy Charge"
.adjres36  init      "Bankruptcy, Un-collectible A/R "
.Adjres37  Init      "Void Check"
.adjres99  init      "Entry Correction"
.end patch 4.2
.
.ADj CHARGE DESCRIPTION.
.
nadjtext  DIM       35
adjdesc1  DIM       35
adjdesc2  DIM       35
adjdesc3  DIM       35
adjdesc4  DIM       35
adjdesc5  DIM       35
adjdesc6  DIM       35
adjdesc7  DIM       35
adjdesc8  DIM       35
adjdesc9  DIM       35
. TOTAL ADj CHARGE
.
.START Patch #3.3 - increased var
.adj$1    DIM       13
.adj$2    DIM       13
.adj$3    DIM       13
.adj$4    DIM       13
.adj$5    DIM       13
.adj$6    DIM       13
.adj$7    DIM       13
.adj$8    DIM       13
.adj$9    DIM       13
.MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
adj$1    DIM       15
adj$2    DIM       15
adj$3    DIM       15
adj$4    DIM       15
adj$5    DIM       15
adj$6    DIM       15
adj$7    DIM       15
adj$8    DIM       15
adj$9    DIM       15
.begin patch 3.4
.MASK92   INIT      "ZZZ,ZZZ,ZZZ.ZZ-"
.MASK32   INIT      "ZZZ.ZZ-"
MASK9    INIT      "ZZZ,ZZZ,ZZZ"
M$AddQTY    DIM       11
M$QTY    DIM       11
.M$QTY    DIM       9
.M$QTYx   DIM       9
M$QTYx   DIM       11
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
.end patch 3.4
.end Patch #3.3 - increased var
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
.START Patch #3.3 - increased var
.M$AR     DIM       13
M$AR     DIM       15
.end Patch #3.3 - increased var
M$PPM    DIM       6
M$PPMx   DIM       6
M$AP1    DIM       13
SHORT    DIM       59      *NET/SHORT INSTRUCTIONS.
GUARPRT  DIM       34      *GUARANTY PRINT LINE.
FORMFLAG FORM      1      1=MAILER, 2=OWNER/manager
FORM2    FORM      2
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
.START Patch #3.3 - increased var
.formar1  form      7.2
formar1  form      9.2
.END Patch #3.3 - increased var
.
.CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
.MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
.NUM10    FORM      10             NUMERIC WORK FIELD FOR CONVERSION.
.MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
.
HUNDRD   FORM      "100"
FORM52   FORM      5.2
FORM42   FORM      4.2
FORM62   FORM      6.2
.START Patch #3.3 - increased var
.FORMAP1  FORM      7.2
FORMAP1  FORM      9.2
.END Patch #3.3 - increased var
TIME     DIM       8
COUNT    FORM      5
CAREOF   DIM       3
PAYTOO   DIM       25
PAYCHK   FORM      1
PAYKEY   DIM        5
amendnum  dim       2
.
NADJUST  FILE
adjnumber dim       2
.Start Patch 3.72
HOLD	DIM	500
HOLD2	DIM	304
OLDBRKNEWCOMP external "COMP001A;OldBrktoNewComp"
.End Patch 3.72
.
.START PATCH 3.66 - ADDED LOGIC
TCell	Automation
TCell2	Automation
Table1	Automation
TRange	Automation
Range	Automation
CRRet	Init    0x0D
CRTab	Init	0x09
TWidth	form	1.2
wdTexture15Percent integer 4,"0x96"
BordStart form	9
BordEnd	form	9
PgStart	form	9
.Variant objects used to talk to outside applications
.See PL/B help in order to understand use of Variant objects.
.
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant

AutPtr	Automation	^
DimPtr	Dim		^
MSWordFlag form		1
.START PATCH 3.76 ADDED LOGIC
NINLogo	PICT
FirstPage	form	5
Laser		pfile
.
Font08		font
Font09I		font
Font09B		font
Font09BI	font
Font010		font
Font014B	font
Font014BI	font
Font07dot5I	font
sevenfive	form	"7.5"
.Begin Patch 4.0
Font07                 font
FontO7		font
FontO18B	font
.end patch 4.0
.END PATCH 3.76 ADDED LOGIC
.Patch 3.77 Logic Added
PERCENT  FORM      4.2
CALCmPER FORM      7.4
TEMP     FORM      8.4
.Patch 3.77
VerticalPos         Form           5
.
.Patch 3.82 Logic Added
netcomnt1 dim      70
netcomnt2 dim      70
netcomnt3 dim      70
.Patch 3.82 Logic Added
.
PrintMSWordInvoice Routine AutPtr,DimPtr
.START PATCH 3.76 ADDED LOGIC
	call	GetWinVer
	CREATE	NINLogo=3:13:30:50:
		"\\nts0\c\netutils\NIN logo black outline.jpg"
..Create fonts to be used
	create	Font08,"Times New Roman",size=8
	create	Font09I,"Times New Roman",size=9,Italic
	create	Font09B,"Times New Roman",size=9,Bold
	create	Font09BI,"Times New Roman",size=9,Bold,Italic
	create	Font010,"Times New Roman",size=10
	create	Font014B,"Times New Roman",size=14,Bold
	create	Font014BI,"Times New Roman",size=14,Bold,Italic
	create	Font07dot5I,"Times New Roman",size=sevenfive,Italic
.END PATCH 3.76 ADDED LOGIC
.Begin Patch 4.0
	create	font07,"Times New Roman",size=7
	create	fontO7,"Times New Roman",size=7
	create	fontO18B,"Times New Roman",size=18,Bold
.End Patch 4.0

	move	C0,MSWordFlag
	call	Trim using INPNAME
	if (INPNAME = "")	.Called from NCSH002A - USE OLE LOGIC
		pack	INPNAME,DimPtr
		move	C1,MSWordFlag
	        create  OTRUE,VarType=VT_BOOL,VarValue=1
	        create  OFALSE,VarType=VT_BOOL,VarValue=0
	endif
.END PATCH 3.66 - ADDED LOGIC
.MAIN
         MOVE      "Names In The News Ca" TO COMPNME
         MOVE      "PRINT DAILY ADJUSTMENTS" TO STITLE
         CALL      PAINT
         MOVE      C1 TO NORDPATH
         move      c1 to nbrkpath       .set access to isi.
.
.         move      c4 to NINVOUTFLAG
.begin patch 3.4
         move      c1 to NINVOUTFLAG             .new compute.inc  this old code falling thru and printing detail. No No.
         move      c1 to NINVFRMFLAG              .mailer/remit copies some LO & internal charges remain hidden
.end patch 3.4
         CLOCK     TIME TO TIME
         DISPLAY   *P20:04,"START TIME : ",TIME
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      DATE FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         XIF
         MATCH     "NINPADJ2" TO INPNAME        *DAILY PRINT?
         IF        EQUAL                        *YES
                 MOVE      C1 TO HOTFLAG        .NOT HOT
.START PATCH 3.76 ADDED LOGIC
.Daily Print no longer done!!!
		shutdown "cls"
.END PATCH 3.76 ADDED LOGIC
         ELSE
                 MOVE      C2 TO HOTFLAG        .HOT
                 unpack    INPNAME TO NjstfLD,typist
                 MOVE      "HOT PRINT ADJUSTMENTS" TO STITLE
                 rep       zfill in njstfld
.dlh fix for missing jd code?
                 unpack    njstfld into str6,adjnumber
.end dlh fix for missing jd code?

         ENDIF
         move      c1 to formflag
         MOVE      DATE TO TODAY
         CALL      PAINT
         MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : ":
                   *P01:09,"Today's Date : "
.
INPGET
         BRANCH    hotflag OF OPENINP,READLIVE
openinp  TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         CLOSE     TESTFILE
         OPEN      NADJUST,INPNAME
.         OPEN      NADJUST,INPNAME,READ
.        open      njstflsq,inpname,read
.          move      c1 to njstflag
          GOTO      PRTGET
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
READLIVE
         DISPLAY   *P15:06,INPNAME
.begin patch 3.61
.         move      c1 to njstflag
.end patch 3.61
         REP       ZFILL IN NjstFLD
         call      njstkey
.
PRTGET
         BRANCH    hotflag OF DLYPRT,HOTPRT
DLYPRT   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL TO PRTNAME
         GOTO      TESTQUES IF EQUAL
         MOVE      C2 TO PRTFLAG
.START PATCH 3.76 REPLACED LOGIC
.         PACK      PRTFILE WITH PDRIVE,PRTNAME
.         SPLOPEN   PRTFILE
.         DISPLAY   *P15:07,PRTNAME
        if (osflag = c1 | osflag = c5 | osflag = c6)         .nt
                PRTOPEN Laser,"\\NTS0\Laser2","FAXFILE.PRN"
	elseif (osflag = C3 | osflag = C4)
                PRTOPEN Laser,"Laser2","FAXFILE.PRN"
        else   .(osflag = c0)         .Don't know prompt for printer
                PRTOPEN Laser,"-","FAXFILE.PRN"
        endif
.END PATCH 3.76 REPLACED LOGIC
         GOTO      TESTQUES
.
.START PATCH 3.66 - REPLACED LOGIC
.HOTPRT   PACK       PRTFILE FROM PDRIVE,PRTNAME
.         SPLOPEN    PRTFILE
.         PRINT     hpreset,hpport:
.                   033,"&l66P":               page length
.                   033,"&l65F":               number lines
.                   033,"&l1E",033,"&a0c0R"     top margin * print position
HOTPRT
	if (MSWordFlag = C0)
.START PATCH 3.76 REPLACED LOGIC
.		PACK	PRTFILE,PDRIVE,PRTNAME
.		SPLOPEN	PRTFILE
.		PRINT	hpreset,hpport:
.			033,"&l66P":               page length
.			033,"&l65F":               number lines
.			033,"&l1E",033,"&a0c0R"     top margin * print position
	        if (osflag = c1 | osflag = c5 | osflag = c6)         .nt
	                PRTOPEN Laser,"\\NTS0\Laser2","FAXFILE.PRN"
		elseif (osflag = C3 | osflag = C4)
	                PRTOPEN Laser,"Laser2","FAXFILE.PRN"
	        else   .(osflag = c0)         .Don't know prompt for printer
	                PRTOPEN Laser,"-","FAXFILE.PRN"
	        endif
.END PATCH 3.76 REPLACED LOGIC
	endif
.END PATCH 3.66 - REPLACED LOGIC
         GOTO       PROCESS
.
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
TESTQUES
.
        goto       testprt
READADJ
.begin patch 3.5
.         FILEPI    1;nadjust
.         READ      nadjust,SEQ;jstvars
         FILEPI    1;nadjust
         READ      nadjust,SEQ;jstvars,typist
         goto      done if over          .02mar93 dlh
.         BUMP      JSTFIL6 BY  4
.         MOVE      JSTFIL6 TO TYPIST
.end patch 3.5
         move      jstsubno to amendnum
.
PROCESS  ADD       C1 TO COUNT
         DISPLAY   *P16:08,COUNT,b2,jstreasn
.         match      "27" to jstreasn
.         goto       readadj if equal
.         match      "14" to jstreasn
.         goto       readadj if equal
         MOVE      JSTCRCT TO REASON
         BRANCH    REASON OF SALES,CORRECT,SHORT
         CLEAR     COMNT
         GOTO      REST
SALES    MOVE      "*SALES BOOK CORRECTION*" TO COMNT
         GOTO      REST
CORRECT  MOVE      "***CORRECTED INVOICE***" TO COMNT
         GOTO      REST
SHORT    MOVE      "***  SHORT PAYMENT  ***" TO COMNT
REST     MOVE      JSTLR TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
         PACK      MKEY FROM OMLRNUM,z3
.Begin Patch 3.72
.           call      nmlrkey
			pack		compfld3 from mkey
 			call		COMPKEY3
.End Patch 3.72
.START PATCH 3.79 ADDED LOGIC
			if (CNCTINACTIVE = "T")
				clear	cnctfname
			endif
.END PATCH 3.79 ADDED LOGIC
         PACK      NBILFLD FROM MKEY,JSTBILTO
.         CALL      READBILL
.begin patch 3.5.1
.         UNPACK    JSTDATE INTO MM,DD,YY
.         PACK      ADJDATE FROM MM,SLASH,DD,SLASH,YY
.         UNPACK    JSTINVDT INTO MM,DD,YY
.         PACK      INVDATE FROM MM,SLASH,DD,SLASH,YY
         UNPACK    JSTDATE INTO cc,YY,MM,DD
         PACK      ADJDATE FROM MM,SLASH,DD,SLASH,YY
         UNPACK    JSTINVDT INTO cc,YY,MM,DD
         PACK      INVDATE FROM MM,SLASH,DD,SLASH,YY
.end patch 3.5.1
.
.Patch 3.77 Logic Added
           PACK      STR2 FROM OSALES10,OSALES
           REP       ZFILL IN STR2
           MOVE      NO TO LSTMSW
.begin patch 3.84
.	if	(str2 = "06" or str2 = "02" or Str2 = "19")
.begin patch 4.00
	if	(str2 = "06" or str2 = "02" or Str2 = "19" or Str2 = "27" or STr2 = "28")
	Move	Yes,LstmSW
.           MATCH     "06" TO STR2
.           IF        EQUAL
.	           MOVE      YES TO LSTMSW            *LIST MANAGEMENT.
.           ELSE
.              MATCH     "19" TO STR2
.              IF        EQUAL
.   	           MOVE      YES TO LSTMSW
.              endif
.end patch 3.84
           endif
.Patch 3.77 Logic Added
         MOVE      OLNUM TO NDATFLD
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY

         MOVE      JSTLR TO NINVFLD
         REP       ZFILL IN NINVFLD
         MOVE      C1 TO NINVPATH
         CALL      NINVKEY
         IF        OVER
         DISPLAY   *P10:15,"NO INV FOUND!!!",*W2
         ENDIF

.begin patch 3.4
         MOVE      YES TO SUBPPSW
         MOVE      NordFLD to nmrgfld
         REP       ZFILL IN NMRGFLD
         MOVE      NordFLD to nshpfld
         REP       ZFILL IN NshpFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      no to mrgsw
         CALL      NMRGKEY
         if        not over
         move      yes to mrgsw
         endif
         move      no to shipsw
         CALL      NshpKEY
         if        not over
         move      yes to shipsw
         endif

         call      wipecvars
.end patch 3.4
		call	Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
;
	call      compute
.
         move      c0 to formar1
         move      formar to formar1
.
.Begin Patch 3.72
.           call      nmlrkey
			pack		compfld3 from mkey
	  call		COMPKEY3
.         call      readmlr
.End Patch 3.72
.START PATCH 3.79 ADDED LOGIC
			if (CNCTINACTIVE = "T")
				clear	cnctfname
			endif
.END PATCH 3.79 ADDED LOGIC
         move      "01" to mm
         move      "01" to dd
         move      "95" to yy
         call      cvtjul
         move      juldays to str5
         move      invdtem to mm
         move      invdted to dd
         move      invdtey to yy
         call      cvtjul
         move      str5 to n5
         sub       juldays from n5
         goto      readbill if not less
         CLEAR     BRCOMP
         CLEAR     BRaddr
         CLEAR     BRcity
         CLEAR     BRstate
         CLEAR     BRzip
         CLEAR     NBRKFLD
         move      b3 to careof
         PACK      NBRKFLD FROM IBRKNUM,IBRKCNT
         CMATCH    B1 TO NBRKFLD
         goto      GOON IF EOS
;Begin Patch 3.72
			call	OLDBRKNEWCOMP using NBRKFLD,HOLD
			goto 	goon if (hold = "")
.         call      nbrkkey
.         goto      goon if over
;End Patch 3.72
         pack      mkey from mlrn,z3
.Begin Patch 3.72
.           call      nmlrkey
			pack		compfld3 from mkey
        	call		COMPKEY3
.End Patch 3.72
.START PATCH 3.79 ADDED LOGIC
			if (CNCTINACTIVE = "T")
				clear	cnctfname
			endif
.END PATCH 3.79 ADDED LOGIC
         cmatch    yes to mbildrct            .bill direct?  11/2 dlh
         if         equal            .yes
.Begin Patch 3.72
.         call      readmlr
.End Patch 3.72
         goto      goon
         endif
.Begin Patch 3.72  Code Added
			MOVE COMPCOMP to CNCTFNAME
			reset hold to 7
			move hold to COMPCOMP
			reset hold to 62
			move hold to COMPADDR
			reset hold to 132
			move hold to COMPCITY
			reset hold to 162
			move hold to COMPSTATE
			reset hold to 164
			move hold to COMPZIP
			reset hold
.         move      mcomp to bilname
.         move      BRCOMP to bilcomp
.         move      BRaddr to biladdr
.         move      BRcity to bilcity
.         move     BRstate to bilstate
.         move     BRzip to bilzip
         move     "C/O" to careof
.End Patch 3.72
GOON     MOVE      NINVFLD TO NADJFLD
         CALL      NADJKEY
.Start Patch #3.3 - replaced var
.         MOVE      MASK72 TO ARMASK
.         MOVE      C0 TO form72
         MOVE      MASK92 TO ARMASK
         MOVE      C0 TO CMPT92
.End Patch #3.3 - replaced var
.begin patch 3.5
.         MOVE      C0 TO FORM82
.         MOVE      C0 TO CVTFLD
.         MOVE      asrecadj TO CVTFLD
.         CALL      CVT
.         MOVE      CVTFLD TO FORM82
.         DIV       HUNDRD INTO FORM82
.Start Patch #3.3 - replaced var
.         ADD       FORM82 TO FORM72
.         add       form72 to formar
          add      asrecadj to cmpt92
.         ADD       FORM82 TO CMPT92
.end patch 3.5
         add       CMPT92 to formar
.END Patch #3.3 - replaced var
         EDIT      FORMar TO ARMASK
.Start Patch #3.3 - replaced var
.         MOVE      MASK72 TO AP1MASK
.         MOVE      C0 TO FORM72
         MOVE      MASK92 TO AP1MASK
         MOVE      C0 TO CMPT92
.END Patch #3.3 - replaced var
.begin patch 3.5
.         MOVE      C0 TO CVTFLD
.         MOVE      c0 TO FORMAP1
.         MOVE      JSTAP1 TO CVTFLD
.         MOVE      ASPAYAD1 TO CVTFLD
.         CALL      CVT
.Start Patch #3.3 - replaced var
.         MOVE      CVTFLD TO FORM72
.         MOVE      CVTFLD TO CMPT92
          move      aspayad1 to cmpt92
.END Patch #3.3 - replaced var
.         MOVE      C0 TO CVTFLD
.         MOVE      AP1 TO CVTFLD
.         CALL      CVT
.         MOVE      CVTFLD TO FORMAP1
         move       ap to formap1
.end patch 3.5
.Start Patch #3.3 - replaced var
.         ADD       FORM72 FROM FORMAP1
.         DIV       HUNDRD INTO FORMAP1
.         MOVE      FORMAP1 TO FORM72
.         EDIT      FORM72 TO AP1MASK
.         MOVE      MASK72 TO AP2MASK
.         MOVE      C0 TO FORM72
.
.begin patch 3.5
         ADD       CMPT92 FROM FORMAP1
.         DIV       HUNDRD INTO FORMAP1
         MOVE      FORMAP1 TO CMPT92
         EDIT      CMPT92 TO AP1MASK
         MOVE      MASK92 TO AP2MASK
         MOVE      C0 TO CMPT92
.END Patch #3.3 - replaced var
.         MOVE      C0 TO CVTFLD
.         MOVE      C0 TO FORMAP2
..         MOVE      JSTAP2 TO CVTFLD
.         MOVE      ASPAYAD2 TO CVTFLD
.       CALL      CVT
.Start Patch #3.3 - replaced var
.         MOVE      CVTFLD TO FORM72
.         MOVE      CVTFLD TO CMPT92
         move       aspayad2 to cmpt92
.end patch 3.5

.END Patch #3.3 - replaced var
.begin patch 3.5
.         MOVE      C0 TO CVTFLD
.         MOVE      AP2 TO CVTFLD
.         CALL      CVT
.         MOVE      CVTFLD TO FORMAP2
          move      ap2 to formap2
.end patch 3.5

.Start Patch #3.3 - replaced var
.         ADD       FORM72 FROM FORMAP2
.         DIV       HUNDRD INTO FORMAP2
.         MOVE      FORMAP2 TO FORM72
.         EDIT      FORM72 TO AP2MASK
.
.begin patch 3.5
         ADD       CMPT92 FROM FORMAP2
.         DIV       HUNDRD INTO FORMAP2
.end patch 3.5
         MOVE      FORMAP2 TO CMPT92
         EDIT      CMPT92 TO AP2MASK
.END Patch #3.3 - replaced var
.begin patch 3.5
.         MOVE      C0 TO FORM42
.         MOVE      C0 TO CVTFLD
.         MOVE      JSTPOST TO CVTFLD
.         CALL      CVT
.         MOVE      CVTFLD TO FORM42
.         DIV       HUNDRD INTO FORM42
         MOVE      MASK42 TO PSMASK
.         EDIT      FORM42 TO PSMASK
         edit      jstpost to psmask
         MOVE      MASK52 TO STMASK
.         MOVE      C0 TO CVTFLD
.         MOVE      JSTSTAX TO CVTFLD
.         CALL      CVT
.         MOVE      CVTFLD TO FORM52
.         DIV       HUNDRD INTO FORM52
.         EDIT      FORM52 TO STMASK
          edit     jststax to stmask
.         MOVE      MASK52 TO CTYMASK
.         MOVE      C0 TO FORM52
.         MOVE      C0 TO CVTFLD
.         MOVE      JSTCTAX TO CVTFLD
.         CALL      CVT
.         MOVE      CVTFLD TO FORM52
.         DIV       HUNDRD INTO FORM52
.         EDIT      FORM52 TO CTYMASK
         edit      jstctax to ctymask
.         MOVE      MASK62 TO LRMASK
.         MOVE      C0 TO FORM62
.         MOVE      C0 TO CVTFLD.
.         MOVE      JSTLRINC TO CVTFLD
.         CALL      CVT
.         MOVE      CVTFLD TO FORM62
.         DIV       HUNDRD INTO FORM62
.         EDIT      FORM62 TO LRMASK
          edit     jstlrinc to lrmask
.end patch 3.5
.Start patch #3.3 - increase var, although var does not seem to be used!!!!
.         PACK      MAILDATE FROM OMDTEM,SLASH,OMDTED,SLASH,OMDTEY
         PACK      MAILDATE FROM OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
.End patch #3.3 - increase var, although var does not seem to be used!!!!
         CLEAR     COMNT1
         CMATCH    "D" TO JSTCD
         GOTO      DB IF EQUAL
         GOTO      CR IF NOT EQUAL
DB       BRANCH    REASON OF PRINT
         MOVE      "ADDITIONAL AMOUNT DUE" TO COMNT1
         GOTO      PRINT
CR       BRANCH    REASON OF PRINT
         MOVE      "CREDIT DUE" TO COMNT1
.
PRINT    CALL      OWNPREP
         COMPARE   C0 TO FORMAP2
         IF        EQUAL
            compare   c0 to formap1
            if        equal
            move      c1 to apflag
            else
            move      c2 to apflag
            endif
         GOTO      det
         ELSE
         MOVE      AP2MASK,AP1MASK
         move      c2 to apflag
         ENDIF
det
.begin patch 3.4
.         MOVE      QTYSHP TO n7
.         MOVE      MASK7 TO M$QTY
.         EDIT      n7 TO M$QTY
         MOVE      QTYbild TO n9
         MOVE      MASK9 TO M$QTY
         EDIT      n9 TO M$QTY
.end patch 3.4
.
.Start Patch #3.3 - replaced var
.         MOVE      PPM TO FORM72
.         MOVE      MASK32 TO M$PPM
.         DIVIDE    HUND INTO FORM72
.         MOVE      FORM72 TO FORM32

.         EDIT      FORM32 TO M$PPM
..
.         MOVE      MASK72 TO M$GROSS
.         EDIT      GROSS TO M$GROSS
..
.         MOVE      MASK72 TO M$AR
....
.begin patch 3.4
.         MOVE      PPM TO CMPT92
.         MOVE      MASK32 TO M$PPM
.         DIVIDE    HUND INTO CMPT92
.         MOVE      CMPT92 TO FORM32
.         EDIT      FORM32 TO M$PPM
           MOVE      PPM TO CMPT92
           MOVE      MASK32 TO M$PPM
           MOVE      CMPT92 TO FORM32
           EDIT      FORM32 TO M$PPM

.end patch 3.4
.
         MOVE      MASK92 TO M$GROSS
         EDIT      GROSS TO M$GROSS
.
         MOVE      MASK92 TO M$AR
.END Patch #3.3 - replaced var
         EDIT      FORMAR1 TO M$AR
.
         call      charges
.
FORMS    branch    formflag of prtmlr,prtown
.START PATCH 3.66 - REPLACED LOGIC
.prtmlr   PRINT     033,"&a0c0R";
..begin patch 3.6
..                   033,"&f4y3X",*L:             invoke macro.
.        call        prtinvfrm
..end patch 3.6
prtmlr
	if (MSWordFlag = 0)
.START PATCH 3.76 REPLACED LOGIC
.		PRINT	033,"&a0c0R";
		call	prtinvfrm
.begin patch 4.1
		call	PrtmlrboxGui
.		call	prtinvfrm2
.end patch 4.1
.END PATCH 3.76 REPLACED LOGIC
	endif
.END PATCH 3.66 - REPLACED LOGIC
         compare   c2 to hotflag
         if        equal
         move      adjnumber to jstsubno
         else
         move      amendnum to jstsubno
         endif
.START PATCH 3.66 - REPLACED LOGIC
.         PRint     *L:
.                   *l,*l,*l,*l,*l,*l:
.                   *L,*L,*l,*l,*l:
.                   hpt025,hpdtch85,hpitalic,"Date:",hpt425,"Invoice##":
.                   hpt600,"Mailer's P.O.##",hpdtch10,hpuprght:
.                   hpt125,adjdate:
..                   hpt500,invnum,dash,jstsubno,hpt700,OMLRPON:
.                   hpt475,invnum,dash,jstsubno,hpt700,OMLRPON:
.                   *L,hpt025,hpdtch85,hpitalic,"Client ##",hpdtch10,hpuprght:
.                  hpt125,MLRN,"/",COBN,"-",BILLTN:
..                   hpt125,jstMLR:
.                   *l,hpt125,bilname:
.                   *L,hpt075,careof,hpt125,bilcomp,hpt600,hpdtch85,hpitalic:
.                   "NIN L.R. ##",hpdtch10,hpuprght,hpt700,lrn:
.                   *l,hpt125,bilADDR:
.                   *l,hpt125,bilCITY," ",bilSTATE," ",bilZIP;
..Start patch #3.3 - ADDED CENTURY
..        PRINT      *N,hpt600,hpdtch85,hpitalic:
..                   "Mail Date:",hpdtch10,hpuprght,hpt700:
..                   OMDTEM,"/",OMDTED,"/",OMDTEY,hpdtch10,hpuprght:
..                   *l,*l,*l,*l,hpt125,comnt:
..                   *L,hpt025,hpdtch85,hpitalic,"Mailer's Offer:":
..                   hpdtch10,hpuprght:
..                   hpt125,OODES:
..                   *L,*L,hpt025,hpdtch85,hpitalic,"Key:",hpdtch10,hpuprght:
..                   hpt125,OMLRKY:
..                   *L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
..                   hpt125,O1DES:
..                   *l,hpt125,o2DES,hpt725,jstreasn:
..                   *L,*l,hpt025,hpdtch85,hpbon,hpunon,"Original Billing":
..                   hpboff,hpunoff:
..                   *L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
..                   hpt425,"$ Per M",hpt600,"Amount Due",hpdtch10,hpuprght:
..                   hpt175,M$QTY,hpt500,hpfixed,"@",M$PPM,hpt625,M$GRoss
.        PRINT      *N,hpt600,hpdtch85,hpitalic:
.                   "Mail Date:",hpdtch10,hpuprght,hpt700:
.                   OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY,hpdtch10,hpuprght:
.                   *l,*l,*l,*l,hpt125,comnt:
.                   *L,hpt025,hpdtch85,hpitalic,"Mailer's Offer:":
.                   hpdtch10,hpuprght:
.                   hpt125,OODES:
.                   *L,*L,hpt025,hpdtch85,hpitalic,"Key:",hpdtch10,hpuprght:
.                   hpt125,OMLRKY:
.                   *L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
.                   hpt125,O1DES:
.                   *l,hpt125,o2DES,hpt725,jstreasn:
.                   *L,*l,hpt025,hpdtch85,hpbon,hpunon,"Original Billing":
.                   hpboff,hpunoff:
.                   *L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
.                   hpt425,"$ Per M",hpt600,"Amount Due",hpdtch10,hpuprght:
.                   hpt175,M$QTY,hpt500,hpfixed,"@",M$PPM,hpt625,M$GRoss
.......................
.START PATCH 3.7 ADDED LOGIC
	packkey	NSEL2FLD,"1",LRN
	move	"NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	O2DES,NSEL2NAME
	endif
.END PATCH 3.7 ADDED LOGIC
	if (MSWordFlag = 0)
.START PATCH 3.76 REPLACED LOGIC
.		PRint	*L:
.			*l,*l,*l,*l,*l,*l:
.			*L,*L,*l,*l,*l:
.			hpt025,hpdtch85,hpitalic,"Date:",hpt425,"Invoice##":
.			hpt600,"Mailer's P.O.##",hpdtch10,hpuprght:
.			hpt125,adjdate:
.			hpt475,invnum,dash,jstsubno,hpt700,OMLRPON:
.			*L,hpt025,hpdtch85,hpitalic,"Client ##",hpdtch10,hpuprght:
.			hpt125,MLRN,"/",COBN,"-",BILLTN:
..Begin Patch 3.72
.			*l,hpt125,CNCTFNAME:
..			*l,hpt125,bilname:
.			*L,hpt075,careof,hpt125,COMPCOMP,hpt600,hpdtch85,hpitalic:
..			*L,hpt075,careof,hpt125,bilcomp,hpt600,hpdtch85,hpitalic:
..End Patch 3.72
.			"NIN L.R. ##",hpdtch10,hpuprght,hpt700,lrn:
..Begin Patch 3.72
.			*l,hpt125,COMPADDR:
.			*l,hpt125,COMPCITY," ",COMPSTATE," ",COMPZIP;
..			*l,hpt125,bilADDR:
..			*l,hpt125,bilCITY," ",bilSTATE," ",bilZIP;
..End Patch 3.72
..START PATCH 3.7 REPLACED LOGIC
..		PRINT	*N,hpt600,hpdtch85,hpitalic:
..			"Mail Date:",hpdtch10,hpuprght,hpt700:
..			OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY,hpdtch10,hpuprght:
..			*l,*l,*l,*l,hpt125,comnt:
..			*L,hpt025,hpdtch85,hpitalic,"Mailer's Offer:":
..			hpdtch10,hpuprght:
..			hpt125,OODES:
..			*L,*L,hpt025,hpdtch85,hpitalic,"Key:",hpdtch10,hpuprght:
..			hpt125,OMLRKY:
..			*L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
..			hpt125,O1DES:
..			*l,hpt125,o2DES,hpt725,jstreasn:
..			*L,*l,hpt025,hpdtch85,hpbon,hpunon,"Original Billing":
..			hpboff,hpunoff:
..			*L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
..			hpt425,"$ Per M",hpt600,"Amount Due",hpdtch10,hpuprght:
..			hpt175,M$QTY,hpt500,hpfixed,"@",M$PPM,hpt625,M$GRoss
.		PRINT	*N,hpt600,hpdtch85,hpitalic:
.			"Mail Date:",hpdtch10,hpuprght,hpt700:
.			OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY,hpdtch10,hpuprght:
.			*l,*l,*l,*l,hpt125,comnt:
.			*L,hpt025,hpdtch85,hpitalic,"Mailer's Offer:":
.			hpdtch10,hpuprght:
.			hpt125,OODES:
.			*L,*L,hpt025,hpdtch85,hpitalic,"Key:",hpdtch10,hpuprght:
.			hpt125,OMLRKY:
.			*L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
.			hpt125,O1DES:
.			*l,hpt125,NSEL2NAME,hpt725,jstreasn:
.			*L,*l,hpt025,hpdtch85,hpbon,hpunon,"Original Billing":
.			hpboff,hpunoff:
.			*L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
.			hpt425,"$ Per M",hpt600,"Amount Due",hpdtch10,hpuprght:
.			hpt175,M$QTY,hpt500,hpfixed,"@",M$PPM,hpt625,M$GRoss
..END PATCH 3.7 REPLACED LOGIC
.............................................................................
		call	Trim using COMPCITY
		Prtpage	Laser;*font=Font09B,*p=1500:2212,adjdate:
			*p=5000:2212,invnum,dash,jstsubno:
			*p=6750:2212,OMLRPON:
			*p=1500:2400,Mlrn,"/",Cobn,"-",Billtn:
			*p=1500:2525,CNCTFNAME:
			*p=1150:2650,CareOf:
			*p=1500:2650,COMPCOMP:
			*p=6750:2712,LRN:
			*p=1500:2775,COMPADDR:
			*p=1500:2900,*ll,COMPCITY,", ",COMPSTATE," ",COMPZIP
		Prtpage	Laser;*p=6750:3212,OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY:
			*p=1500:3602,*font=Font010,comnt,*font=Font09B:
			*p=1500:3800,OODES:
			*p=1500:4050,OMLRKY:
			*p=1500:4300,O1DES:
			*p=1500:4440,NSEL2NAME:
			*p=4250:4440,jstreasn:
			*p=2500:4900,*ALIGNMENT=*right,*overlayoff,M$Qty:
			*p=5000:4900,"@":
			*p=5475:4900,M$PPM:
;Patch 3.77 Logic Added
			*p=7500:4900,M$GRoss,*overlayon,*ALIGNMENT=*Left
.			*p=7500:4900,M$GRoss,*overlayon,*ALIGNMENT=*Left:
.
        if         (onetfm <> "F" & onetFM <> "M")
        move       c1 to netflag
        endif

	  		if (netflag = 2 or netflag = 3)
	         move      c0 to calcmper
   	      MOVE      NMRGNET TO CMPT94
      	   MOVE      NMRGIQTY TO N7
         	DIVIDE    N7 INTO CMPT94
	         MULT      "100" BY CMPT94
   	      MOVE      C0 TO PERCENT
      	   ADD       CMPT94 TO PERCENT
		      move      c0 to temp
      	   move      irnetper to temp
         	compare   c3 to netflag            .flat???
	         if        equal
   	   	   move      irnetper to percent         .yes print stated flat rate
      	   else                              .no print calced rate
		         compare   temp to percent           .use bigger %
		         if        less
		      	   move      temp to percent
      		   endif
	         endif
	         cmatch    no to lstmsw
   	      if        equal               .its a flat discounted brokerage client
.START PATCH 3.83 REPLACED LOGIC
.	   	      Prtpage	Laser;*p=4585:4412,*ALIGNMENT=*right,Irnetper,*ALIGNMENT=*Left,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"
	   	      Prtpage	Laser;*p=4585:4412,*ALIGNMENT=*right,Irnetper,*ALIGNMENT=*Left,"% Net Arrangement, Run charge @",inetrc,slash,"M"
.END PATCH 3.83 REPLACED LOGIC
				else
.					 Prtpage	Laser;*p=3785:4300,"Volume Discount - No Deducts, No CV required.":
.					 *p=4585:4412,Irnetper,"% Vol Name Arrangement, Run charge @",inetrc,slash,"M"
.					 Prtpage	Laser;*p=3785:4587,"Volume Discount - No Deducts, No CV required.":
.					 *p=4585:4712,Irnetper,"% Vol Name Arrangement, Run charge @",inetrc,slash,"M"
.START PATCH 3.83 REPLACED LOGIC
.					 Prtpage	Laser;*p=4585:3602,"Volume Discount - No Deducts, No CV required.":
.					 *p=4585:3727,Irnetper,*ALIGNMENT=*Left,"% Vol Name Arrangement, Run charge @",inetrc,slash,"M"
					 Prtpage	Laser;*p=4585:3602,"% Net Arrangement, Run charge @",inetrc,slash,"M":
					 *p=4585:3727,Irnetper,*ALIGNMENT=*Left,"No Deducts, No CV required."
.END PATCH 3.83 REPLACED LOGIC
				endif
			endif



;Patch 3.77 Logic Added
.END PATCH 3.76 REPLACED LOGIC
	else
.MS Word Version
.Patch 3.82 Logic Added
.
        clear      netcomnt1
	clear      netcomnt2
        clear      netcomnt3
.
        if         (onetfm <> "F" & onetFM <> "M")
        move       c1 to netflag
        endif

	  		if (netflag = 2 or netflag = 3)
	         move      c0 to calcmper
   	      MOVE      NMRGNET TO CMPT94
      	   MOVE      NMRGIQTY TO N7
         	DIVIDE    N7 INTO CMPT94
	         MULT      "100" BY CMPT94
   	      MOVE      C0 TO PERCENT
      	   ADD       CMPT94 TO PERCENT
		      move      c0 to temp
      	   move      irnetper to temp
         	compare   c3 to netflag            .flat???
	         if        equal
   	   	   move      irnetper to percent         .yes print stated flat rate
      	   else                              .no print calced rate
		         compare   temp to percent           .use bigger %
		         if        less
		      	   move      temp to percent
      		   endif
	         endif
	         cmatch    no to lstmsw
   	      if        equal               .its a flat discounted brokerage client
.Patch 3.82 Logic Added
		       pack      netcomnt1 from Irnetper,"% Net Name Arrangement, Run charge @",inetrc,slash,"M"
.Patch 3.82 Logic Added
				else
.START PATCH 3.83 REPLACED LOGIC
.	   	      pack      netcomnt2 from "Volume Discount - No Deducts, No CV required."
.	   	      pack      netcomnt3 from Irnetper,"% Vol Name Arrangement, Run charge @",inetrc,slash,"M"
	   	      pack      netcomnt2 from Irnetper,"% Net Arrangement, Run charge @",inetrc,slash,"M"
	   	      pack      netcomnt3 from "No Deducts, No CV required."
.END PATCH 3.83 REPLACED LOGIC
				endif
			endif

.Note:  Setting a property will apply that property for all subsequent text!!
.	The End of the Range is always implied to be the last text that was entered.
		AutPtr.Sections.Add
		getprop	AutPtr.Sections(2),*Range=Range
.START PATCH 3.76 REPLACED LOGIC
.		getprop	Range,*End=PgStart
		getprop	Range,*Start=PgStart
		AutPtr.Shapes.AddPicture using *Filename="\\nts0\c\netutils\Logocolornotag.jpg",*Anchor=Range
		AutPtr.Shapes(2).ScaleHeight using ".65",OFALSE
		AutPtr.Shapes(2).ScaleWidth using ".65",OFALSE
.Need to set up the RelativeVerticalPosition in order for Top property to behave properly
.I have set the RelativeVerticalPosition to "wdRelativeVerticalPositionPage"=1
		setprop	AutPtr.Shapes(2),*RelativeVerticalPosition=1,*Top="30"
.END PATCH 3.76 REPLACED LOGIC
		setprop Range.Font,*Name="Times New Roman"			.Default Font Name
.START PATCH 3.76 REPLACED LOGIC
.		setprop Range.Font,*Size=12					.Default Font Size
		setprop Range.Font,*Size=14					.Default Font Size
.END PATCH 3.76 REPLACED LOGIC
		setprop AutPtr.Sections(2).PageSetup,*TopMargin=30		.Default Top Margin
		setprop AutPtr.Sections(2).PageSetup,*BottomMargin=0		.Default Bottom Margin
		setprop AutPtr.Sections(2).PageSetup,*LeftMargin=30		.Default Left Margin
		setprop AutPtr.Sections(2).PageSetup,*RightMargin=35		.Default Right Margin
.START PATCH 3.76 REPLACED LOGIC
.		Range.InsertAfter using "Names"
.		setprop Range,*Bold=OTRUE
.		setprop Range.Font,*Size=20
..
.		Range.InsertAfter using " in the News"
.		add	C6,PgStart,howmany
.		setprop	Range,*Start=howmany
.		getprop	Range,*End=result
.		setprop Range,*Bold=OFALSE
.		setprop Range,*Italic=OTRUE
.		setprop	Range,*Start=PgStart
.		setprop Range,*Underline=OTRUE
..
.		Range.InsertAfter using CRRet
.		Range.InsertAfter using "C A L I F O R N I A   I N C"
.		setprop	Range,*Start=result
.		getprop	Range,*End=result
.		setprop Range.Font,*Size=14
.		setprop Range,*Underline=OFALSE
.		setprop Range,*Italic=OFALSE
...............................
		Range.InsertAfter using CRRet
		Range.InsertAfter using CRRet
		Range.InsertAfter using CRRet
.END PATCH 3.76 REPLACED LOGIC
		Range.InsertAfter using CRRet
		getprop	Range,*End=result
		Range.InsertAfter using CRRet
		getprop	Range,*End=BordStart
		Range.InsertAfter using "Invoice"
		getprop	Range,*End=BordEnd
		setprop	Range,*Start=result
		setprop Range,*Bold=OTRUE
		setprop Range,*Italic=OTRUE
.
		Range.InsertAfter using CRRet
		getprop	Range,*End=result
		setprop	Range,*Start=result
		Range.InsertAfter using CRRet
.Table Number 1
		getprop	Range,*End=result
		setprop	Range,*Start=result
		getprop	Range,*End=N9
		Range.InsertAfter using CRRet
.Make sure CR is in proper format
		setprop	Range,*Start=result
		setprop	Range.Font,*Size=10
		setprop Range,*Bold=OFALSE
		setprop Range,*Italic=OFALSE
		setprop	Range,*Start=result
.
		AutPtr.Range giving TRange using result,N9
		AutPtr.Tables.Add giving Table1 using TRange,9,8
		setprop	Table1.Range.Font,*Size=10
		setprop Table1.Range,*Bold=OFALSE
		setprop Table1.Range,*Italic=OFALSE
.
		Table1.Cell giving TCell using 2,2
		TCell.Range.InsertAfter using "Date:"
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using 2,4
		TCell.Range.InsertAfter using adjdate
		Table1.Cell giving TCell using 2,5
		TCell.Range.InsertAfter using "Invoice ##"
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using 2,6
		TCell.Range.InsertAfter using invnum
		TCell.Range.InsertAfter using dash
		TCell.Range.InsertAfter using jstsubno
		Table1.Cell giving TCell using 2,7
		TCell.Range.InsertAfter using "Mailer's P.O. ##"
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using 2,8
		TCell.Range.InsertAfter using OMLRPON
.
		Table1.Cell giving TCell using 3,2
		TCell.Range.InsertAfter using "Client ##"
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using 3,4
		TCell.Range.InsertAfter using MLRN
		TCell.Range.InsertAfter using "/"
		TCell.Range.InsertAfter using COBN
		TCell.Range.InsertAfter using "-"
		TCell.Range.InsertAfter using BILLTN
.
		Table1.Cell giving TCell using 4,4
.Begin Patch 3.72
.		TCell.Range.InsertAfter using bilname
		TCell.Range.InsertAfter using CNCTFNAME
.End Patch 3.72
.
		Table1.Cell giving TCell using 5,3
		TCell.Range.InsertAfter using careof
		Table1.Cell giving TCell using 5,4
.Begin Patch 3.72
		TCell.Range.InsertAfter using COMPCOMP
.		TCell.Range.InsertAfter using bilcomp
.End Patch 3.72
		Table1.Cell giving TCell using 5,7
		TCell.Range.InsertAfter using "NIN LR ##"
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using 5,8
		TCell.Range.InsertAfter using lrn
.
		Table1.Cell giving TCell using 6,4
.Begin Patch 3.72
		TCell.Range.InsertAfter using COMPADDR
.End Patch 3.72
.
.Begin Patch 3.72
		call	Trim using COMPCITY
.		call	Trim using bilCITY
		Table1.Cell giving TCell using 7,4
.		TCell.Range.InsertAfter using bilCITY
		TCell.Range.InsertAfter using COMPCITY
		TCell.Range.InsertAfter using ", "
.		TCell.Range.InsertAfter using bilSTATE
		TCell.Range.InsertAfter using COMPSTATE
		TCell.Range.InsertAfter using " "
.		TCell.Range.InsertAfter using bilZIP
		TCell.Range.InsertAfter using COMPZIP
.End Patch 3.72
.
		Table1.Cell giving TCell using 8,7
		TCell.Range.InsertAfter using "Mail Date:"
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using 8,8
		TCell.Range.InsertAfter using OMDTEM
		TCell.Range.InsertAfter using "/"
		TCell.Range.InsertAfter using OMDTED
		TCell.Range.InsertAfter using "/"
		TCell.Range.InsertAfter using OMDTEC
		TCell.Range.InsertAfter using OMDTEY
.Format Table 1
		move	".47",TWidth
		setprop	Table1.Columns(1),*Width=TWidth
		move	".62",TWidth
		setprop	Table1.Columns(2),*Width=TWidth
		move	".42",TWidth
		setprop	Table1.Columns(3),*Width=TWidth
		move	"1.5",TWidth
		setprop	Table1.Columns(4),*Width=TWidth
		move	"1",TWidth
		setprop	Table1.Columns(5),*Width=TWidth
		move	"1.5",TWidth
		setprop	Table1.Columns(6),*Width=TWidth
		move	"1.2",TWidth
		setprop	Table1.Columns(7),*Width=TWidth
		move	"1.17",TWidth
		setprop	Table1.Columns(8),*Width=TWidth
.
		setprop	Table1.Borders,*Enable=OFALSE		.Make sure no other Borders are applied
		setprop	Table1.Borders,*OutsideLineStyle=1	.1 = Single Line
.Set up Alignment in first table.
.Note:  Table1 is a temporary object, and ends up linked to third Table.  If more are added, I will need to change following code.
.THIS MUST BE DONE BEFORE CELL MERGING!!!!!!!
.0 = Align Left
.1 = Align Center
.2 = Align Right
		getprop	Table1.Rows,*Count=result
		for N9,C1,result
			Table1.Cell giving TCell using N9,2
			setprop	TCell.Range.Paragraphs(1),*Alignment=0
		repeat
		for N9,C1,result
			Table1.Cell giving TCell using N9,3
			setprop	TCell.Range.Paragraphs(1),*Alignment=0
		repeat
		for N9,C1,result
			Table1.Cell giving TCell using N9,4
			setprop	TCell.Range.Paragraphs(1),*Alignment=0
		repeat
		for N9,C1,result
			Table1.Cell giving TCell using N9,5
			setprop	TCell.Range.Paragraphs(1),*Alignment=0
		repeat
		for N9,C1,result
			Table1.Cell giving TCell using N9,6
			setprop	TCell.Range.Paragraphs(1),*Alignment=0
		repeat
		for N9,C1,result
			Table1.Cell giving TCell using N9,7
			setprop	TCell.Range.Paragraphs(1),*Alignment=0
		repeat
		for N9,C1,result
			Table1.Cell giving TCell using N9,8
			setprop	TCell.Range.Paragraphs(1),*Alignment=0
		repeat
.
		Table1.Cell giving TCell using 4,4
		Table1.Cell giving TCell2 using 4,8
		TCell.Merge using TCell2
.
		Table1.Cell giving TCell using 5,4
		Table1.Cell giving TCell2 using 5,6
		TCell.Merge using TCell2
.
		Table1.Cell giving TCell using 6,4
		Table1.Cell giving TCell2 using 6,8
		TCell.Merge using TCell2
.
		Table1.Cell giving TCell using 7,4
		Table1.Cell giving TCell2 using 7,8
		TCell.Merge using TCell2
.Table Number 2
		getprop	Range,*End=result
		setprop	Range,*Start=result
		getprop	Range,*End=N9
		Range.InsertAfter using CRRet
.Make sure CR is in proper format
		setprop	Range,*Start=result
		setprop	Range.Font,*Size=10
		setprop Range,*Bold=OFALSE
		setprop Range,*Italic=OFALSE
		setprop	Range,*Start=result
.
		AutPtr.Range giving TRange using result,N9
		AutPtr.Tables.Add giving Table1 using TRange,10,3
		setprop	Table1.Range.Font,*Size=10
		setprop Table1.Range,*Bold=OFALSE
		setprop Table1.Range,*Italic=OFALSE
.
.Patch 3.82 start Logic Added
		Table1.Cell giving TCell using 1,3
                clear   taskname
		pack	taskname,comnt,crtab,crtab,crtab,netcomnt1,netcomnt2
		TCell.Range.InsertAfter using taskname
		Table1.Cell giving TCell using 2,3
		clear   taskname
		pack	taskname,crtab,crtab,crtab,crtab,crtab,crtab,netcomnt3
		TCell.Range.InsertAfter using taskname
.		TCell.Range.InsertAfter using comnt
.Patch 3.82 start Logic Added

.
		Table1.Cell giving TCell using 3,2
		TCell.Range.InsertAfter using "Mailer's Offer"
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using 3,3
		TCell.Range.InsertAfter using OODES
.
		Table1.Cell giving TCell using 5,2
		TCell.Range.InsertAfter using "Key:"
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using 5,3
		TCell.Range.InsertAfter using OMLRKY
.
		Table1.Cell giving TCell using 7,2
		TCell.Range.InsertAfter using "List:"
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using 7,3
		TCell.Range.InsertAfter using O1DES
.
		Table1.Cell giving TCell using 8,3
.START PATCH 3.7 REPLACED LOGIC
.		TCell.Range.InsertAfter using o2DES
		TCell.Range.InsertAfter using NSEL2NAME
.END PATCH 3.7 REPLACED LOGIC
.NOTE:  NEXT CODE IS FOR REFERENCE.  JSTREASN USED TO BE PRINTED, BUT IT NO LONGER APPEARS ON THE FORM.
.HPT725 ENDS UP BEING OFF THE RIGHT MARGIN IN OLD PRINT OUT!!
.			*L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
.			hpt125,O1DES:
.			*l,hpt125,o2DES,hpt725,jstreasn:
.Format Table 2
		move	".47",TWidth
		setprop	Table1.Columns(1),*Width=TWidth
		move	"1.03",TWidth
		setprop	Table1.Columns(2),*Width=TWidth
		move	"6.37",TWidth
		setprop	Table1.Columns(3),*Width=TWidth
.
		setprop	Table1.Borders,*Enable=OFALSE		.Make sure no other Borders are applied
		setprop	Table1.Borders,*OutsideLineStyle=1	.1 = Single Line
.Set up Alignment in second table.
.Note:  Table1 is a temporary object, and ends up linked to third Table.  If more are added, I will need to change following code.
.THIS MUST BE DONE BEFORE CELL MERGING!!!!!!!
.0 = Align Left
.1 = Align Center
.2 = Align Right
		getprop	Table1.Rows,*Count=result
		for N9,C1,result
			Table1.Cell giving TCell using N9,2
			setprop	TCell.Range.Paragraphs(1),*Alignment=0
		repeat
		for N9,C1,result
			Table1.Cell giving TCell using N9,3
			setprop	TCell.Range.Paragraphs(1),*Alignment=0
		repeat
.Table Number 3
		getprop	Range,*End=result
		setprop	Range,*Start=result
		getprop	Range,*End=N9
		Range.InsertAfter using CRRet
		AutPtr.Range giving TRange using result,N9
		AutPtr.Tables.Add giving Table1 using TRange,32,9
		setprop	Table1.Range.Font,*Size=10
		setprop Table1.Range,*Bold=OFALSE
		setprop Table1.Range,*Italic=OFALSE
.
		Table1.Cell giving TCell using 1,2
		TCell.Range.InsertAfter using "Original Billing"
		setprop	TCell.Range,*Bold=OTRUE
		setprop	TCell.Range,*Underline=OTRUE
....
		move	C0,N9
		move	irexqty,N9
		compare	C0,N9
		if not equal
			move	MASK9,M$QTYX
			edit	N9,M$QTYx
			move	MASK32,M$PPMX
			move	iexppm,form32
			edit	FORM32,M$PPMX
		endif
....
		Table1.Cell giving TCell using 2,2
		TCell.Range.InsertAfter using "Quantity Addressed"
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using 2,3
		TCell.Range.InsertAfter using M$QTY
		Table1.Cell giving TCell using 2,4
		TCell.Range.InsertAfter using "$ Per M"
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using 2,5
		TCell.Range.InsertAfter using "@"
		Table1.Cell giving TCell using 2,6
		TCell.Range.InsertAfter using M$PPM
		Table1.Cell giving TCell using 2,7
		TCell.Range.InsertAfter using "Amount Due"
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using 2,8
		TCell.Range.InsertAfter using M$GRoss
	endif
.END PATCH 3.66 - REPLACED LOGIC
.End patch #3.3 - ADDED CENTURY
.begin patch 3.4
.        move       c0 to n7
.        move       irexqty to n7
.        compare    c0 to n7
.        if         not equal
.         MOVE      MASK7 TO M$QTYX
.         EDIT      n7 TO M$QTYx
        move       c0 to n9
        move       irexqty to n9
        compare    c0 to n9
        if         not equal
         MOVE      MASK9 TO M$QTYX
         EDIT      n9 TO M$QTYx
.end patch 3.4
.Start Patch #3.3 - replaced var
.         move      c0 to form72
.         MOVE      iexPPM TO FORM72
.         MOVE      MASK32 TO M$PPMX
.         DIVIDE    HUND INTO FORM72
.         MOVE      FORM72 TO FORM32
.
.begin patch 3.4
.         move      c0 to CMPT92
.         MOVE      iexPPM TO CMPT92
.DLH 27Jun00  works better if the mask gets moved to the print variable
         MOVE      MASK32 TO M$PPMX
.End DLH 27Jun00  works better if the mask gets moved to the print variable
.         DIVIDE    HUND INTO CMPT92
.         MOVE      CMPT92 TO FORM32
          move      iexppm to form32
.end patch 3.4
.END Patch #3.3 - replaced var
         EDIT      FORM32 TO M$PPMX
.START PATCH 3.66 - REPLACED LOGIC
.        print      hpt175,hpdtch10,hpuprght,M$QTYX,hpt500,hpfixed,"@",M$PPMX
		if (MSWordFlag = 0)
.START PATCH 3.76 REPLACED LOGIC
.			print	hpt175,hpdtch10,hpuprght,M$QTYX,hpt500,hpfixed,"@",M$PPMX
;Patch 3.78
			Prtpage	Laser;*p=2500:5025,*ALIGNMENT=*right,*overlayoff,M$QtyX:
				*p=5000:5025,"@":
				*p=5475:5025,M$PPMX,*overlayon,*ALIGNMENT=*Left
;Patch 3.78
.END PATCH 3.76 REPLACED LOGIC
		endif
.END PATCH 3.66 - REPLACED LOGIC
        endif
.                  *L,*L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
.                   hpt425,"$ PerM",hpt600,"Amount Due",hpdtch10,hpuprght;
.START PATCH 3.66 - REPLACED LOGIC
.        print      hpdtch10,*14,ADDESC1,hpt500,hpfixed,hpfixed,AT1,ADD$RT1,hpt625,ADD$1:
.                   *L,hpdtch10,*14,ADDESC2,hpt500,hpfixed,AT2,ADD$RT2,hpt625,ADD$2:
.                   *L,hpdtch10,*14,ADDESC3,hpt500,hpfixed,AT3,ADD$RT3,hpt625,ADD$3:
.                   *L,hpdtch10,*14,ADDESC4,hpt500,hpfixed,AT4,ADD$RT4,hpt625,ADD$4:
.                   *L,hpdtch10,*14,ADDESC5,hpt500,hpfixed,AT5,ADD$RT5,hpt625,ADD$5:
.                   *L,hpdtch10,*14,ADDESC6,hpt500,hpfixed,AT6,ADD$RT6,hpt625,ADD$6:
.                   *L,hpdtch10,*14,ADDESC7,hpt500,hpfixed,AT7,ADD$RT7,hpt625,ADD$7:
.                   *L,hpdtch10,*14,ADDESC8,hpt500,hpfixed,AT8,ADD$RT8,hpt625,ADD$8:
.                   *L,hpdtch10,*14,ADDESC9,hpt500,hpfixed,AT9,ADD$RT9,hpt625,ADD$9:
.                   *L,hpdtch10,*14,ADDESC10,hpt500,hpfixed,AT10,ADD$RT10,hpt625,ADD$10:
.                   *l,hpt500,"Original Total Due ",hpt625,hpfixed,M$AR,hpdtch10:
.                   *L,hpt025,hpdtch85,hpbon,hpunon,"Adjustments to Billing":
.                   hpboff,hpunoff:
.                   hpdtch10,*n,*14,adjdesc1,hpt500,hpfixed,hpfixed,hpt625,adj$1:
.                   *L,hpdtch10,*14,adjdesc2,hpt500,hpfixed,hpt625,adj$2:
.                   *L,hpdtch10,*14,adjdesc3,hpt500,hpfixed,hpt625,adj$3:
.                   *L,hpdtch10,*14,adjdesc4,hpt500,hpfixed,hpt625,adj$4:
.                   *L,hpdtch10,*14,adjdesc5,hpt500,hpfixed,hpt625,adj$5:
.                   *L,hpdtch10,*14,adjdesc6,hpt500,hpfixed,hpt625,adj$6:
.                   *L,hpdtch10,*14,adjdesc7,hpt500,hpfixed,hpt625,adj$7:
.                   *L,hpdtch10,*14,adjdesc8,hpt500,hpfixed,hpt625,adj$8:
.                   *L,hpdtch10,*14,adjdesc9,hpt500,hpfixed,hpt625,adj$9:
.                   *L:
.                   *L,*L,*L,hpdtch10:
.                   *L,*12:
.                   hpt625,033,"*c360A":      width
.                   033,"*c65B":       height
.                   033,"*c2G":        area fill%
.                   033,"*c2P":          print it shaded
.                   *l,hpt550,"Total Due ",hpt625:
.                   hpfixed,ARmask,hpdtch10:
.                   *L,*12:
.                   *N,*12,hpdtch85,hpitalic,hpbon,"Payment due upon ":
.                   "receipt. Please return copy with payment.":
.                   hpt525,hpboff,"Member Direct Marketing Association":
.                   hpdtch10,hpuprght,hpt750,TYPIST:
.                   *L:
.                   *L,*L,*L,*l,*FLUSH
.         PRINT     033,"&a0c0R";
..begin patch 3.6
..                   033,"&f4y3X",*L:             invoke macro.
..begin patch 3.65
.         goto       wipevars
..end patch 3.65
.        call        prtinvfrm
..end patch 3.6
.        PRint      *l:
.                   *l,*l,*l,*l,*l,*l:
.                   *L,*L,*l,*l,*l:
.                   hpt025,hpdtch85,hpitalic,"Date:",hpt425,"Invoice##":
.                   hpt600,"Mailer's P.O.##",hpdtch10,hpuprght:
.                   hpt125,adjdate:
..                   hpt500,invnum,dash,amendnum,hpt700,OMLRPON:
.                   hpt475,invnum,dash,jstsubno,hpt700,OMLRPON:
.                   *L,hpt025,hpdtch85,hpitalic,"Client ##",hpdtch10,hpuprght:
.                  hpt125,MLRN,"/",COBN,"-",BILLTN:
..                   hpt125,jstMLR:
.                   *l,hpt125,bilname:
.                   *L,hpt075,careof,hpt125,bilcomp,hpt600,hpdtch85,hpitalic:
.                   "NIN L.R. ##",hpdtch10,hpuprght,hpt700,lrn:
.                   *l,hpt125,bilADDR:
.                   *l,hpt125,bilCITY," ",bilSTATE," ",bilZIP;
..Start patch #3.3 - ADDED CENTURY
..        PRINT      *N,hpt600,hpdtch85,hpitalic:
..                   "Mail Date:",hpdtch10,hpuprght,hpt700:
..                   OMDTEM,"/",OMDTED,"/",OMDTEY,hpdtch10,hpuprght:
..                   *l,*l,*l,*l,hpt125,comnt:
..                   *L,hpt025,hpdtch85,hpitalic,"Mailer's Offer:":
..                   hpdtch10,hpuprght:
..                   hpt125,OODES:
..                   *L,*L,hpt025,hpdtch85,hpitalic,"Key:",hpdtch10,hpuprght:
..                   hpt125,OMLRKY:
..                   *L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
..                   hpt125,O1DES:
..                   *l,hpt125,o2DES,hpt725,jstreasn:
..                   *L,*l,hpt025,hpdtch85,hpbon,hpunon,"Original Billing":
..                   hpboff,hpunoff:
..                   *L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
..                   hpt425,"$ Per M",hpt600,"Amount Due",hpdtch10,hpuprght:
..                   hpt175,M$QTY,hpt500,hpfixed,"@",M$PPM,hpt625,M$GRoss
.        PRINT      *N,hpt600,hpdtch85,hpitalic:
.                   "Mail Date:",hpdtch10,hpuprght,hpt700:
.                   OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY,hpdtch10,hpuprght:
.                   *l,*l,*l,*l,hpt125,comnt:
.                   *L,hpt025,hpdtch85,hpitalic,"Mailer's Offer:":
.                   hpdtch10,hpuprght:
.                   hpt125,OODES:
.                   *L,*L,hpt025,hpdtch85,hpitalic,"Key:",hpdtch10,hpuprght:
.                   hpt125,OMLRKY:
.                   *L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
.                   hpt125,O1DES:
.                   *l,hpt125,o2DES,hpt725,jstreasn:
.                   *L,*l,hpt025,hpdtch85,hpbon,hpunon,"Original Billing":
.                   hpboff,hpunoff:
.                   *L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
.                   hpt425,"$ Per M",hpt600,"Amount Due",hpdtch10,hpuprght:
.                   hpt175,M$QTY,hpt500,hpfixed,"@",M$PPM,hpt625,M$GRoss
...................................
.START PATCH 3.7 ADDED LOGIC
	packkey	NSEL2FLD,"1",LRN
	move	"NSEL2KEY-2",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	O2DES,NSEL2NAME
	endif
.END PATCH 3.7 ADDED LOGIC
.START PATCH 3.81 REPLACED LOGIC
.	if (MSWordFlag = 0)
.END PATCH 3.81 REPLACED LOGIC
.START PATCH 3.76 REPLACED LOGIC
.		print	hpdtch10,*14,ADDESC1,hpt500,hpfixed,hpfixed,AT1,ADD$RT1,hpt625,ADD$1:
.			*L,hpdtch10,*14,ADDESC2,hpt500,hpfixed,AT2,ADD$RT2,hpt625,ADD$2:
.			*L,hpdtch10,*14,ADDESC3,hpt500,hpfixed,AT3,ADD$RT3,hpt625,ADD$3:
.			*L,hpdtch10,*14,ADDESC4,hpt500,hpfixed,AT4,ADD$RT4,hpt625,ADD$4:
.			*L,hpdtch10,*14,ADDESC5,hpt500,hpfixed,AT5,ADD$RT5,hpt625,ADD$5:
.			*L,hpdtch10,*14,ADDESC6,hpt500,hpfixed,AT6,ADD$RT6,hpt625,ADD$6:
.			*L,hpdtch10,*14,ADDESC7,hpt500,hpfixed,AT7,ADD$RT7,hpt625,ADD$7:
.			*L,hpdtch10,*14,ADDESC8,hpt500,hpfixed,AT8,ADD$RT8,hpt625,ADD$8:
.			*L,hpdtch10,*14,ADDESC9,hpt500,hpfixed,AT9,ADD$RT9,hpt625,ADD$9:
.			*L,hpdtch10,*14,ADDESC10,hpt500,hpfixed,AT10,ADD$RT10,hpt625,ADD$10:
.			*l,hpt500,"Original Total Due ",hpt625,hpfixed,M$AR,hpdtch10:
.			*L,hpt025,hpdtch85,hpbon,hpunon,"Adjustments to Billing":
.			hpboff,hpunoff:
.			hpdtch10,*n,*14,adjdesc1,hpt500,hpfixed,hpfixed,hpt625,adj$1:
.			*L,hpdtch10,*14,adjdesc2,hpt500,hpfixed,hpt625,adj$2:
.			*L,hpdtch10,*14,adjdesc3,hpt500,hpfixed,hpt625,adj$3:
.			*L,hpdtch10,*14,adjdesc4,hpt500,hpfixed,hpt625,adj$4:
.			*L,hpdtch10,*14,adjdesc5,hpt500,hpfixed,hpt625,adj$5:
.			*L,hpdtch10,*14,adjdesc6,hpt500,hpfixed,hpt625,adj$6:
.			*L,hpdtch10,*14,adjdesc7,hpt500,hpfixed,hpt625,adj$7:
.			*L,hpdtch10,*14,adjdesc8,hpt500,hpfixed,hpt625,adj$8:
.			*L,hpdtch10,*14,adjdesc9,hpt500,hpfixed,hpt625,adj$9:
.			*L:
.			*L,*L,*L,hpdtch10:
.			*L,*12:
.			hpt625,033,"*c360A":      width
.			033,"*c65B":       height
.			033,"*c2G":        area fill%
.			033,"*c2P":          print it shaded
.			*l,hpt550,"Total Due ",hpt625:
.			hpfixed,ARmask,hpdtch10:
.			*L,*12:
.			*N,*12,hpdtch85,hpitalic,hpbon,"Payment due upon ":
.			"receipt. Please return copy with payment.":
.			hpt525,hpboff,"Member Direct Marketing Association":
.			hpdtch10,hpuprght,hpt750,TYPIST:
.			*L:
.			*L,*L,*L,*l,*FLUSH
.		PRINT	033,"&a0c0R";
.		goto	wipevars
.		call	prtinvfrm
.		PRint	*l:
.			*l,*l,*l,*l,*l,*l:
.			*L,*L,*l,*l,*l:
.			hpt025,hpdtch85,hpitalic,"Date:",hpt425,"Invoice##":
.			hpt600,"Mailer's P.O.##",hpdtch10,hpuprght:
.			hpt125,adjdate:
.			hpt475,invnum,dash,jstsubno,hpt700,OMLRPON:
.			*L,hpt025,hpdtch85,hpitalic,"Client ##",hpdtch10,hpuprght:
.			hpt125,MLRN,"/",COBN,"-",BILLTN:
..Begin Patch 3.72
..			*l,hpt125,bilname:
.			*l,hpt125,CNCTFNAME:
..			*L,hpt075,careof,hpt125,bilcomp,hpt600,hpdtch85,hpitalic:
.			*L,hpt075,careof,hpt125,COMPCOMP,hpt600,hpdtch85,hpitalic:
.			"NIN L.R. ##",hpdtch10,hpuprght,hpt700,lrn:
..			*l,hpt125,bilADDR:
.			*l,hpt125,COMPADDR:
..			*l,hpt125,bilCITY," ",bilSTATE," ",bilZIP;
.			*l,hpt125,COMPCITY," ",COMPSTATE," ",COMPZIP;
..End Patch 3.72
..START PATCH 3.7 REPLACED LOGIC
..		PRINT	*N,hpt600,hpdtch85,hpitalic:
..			"Mail Date:",hpdtch10,hpuprght,hpt700:
..			OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY,hpdtch10,hpuprght:
..			*l,*l,*l,*l,hpt125,comnt:
..			*L,hpt025,hpdtch85,hpitalic,"Mailer's Offer:":
..			hpdtch10,hpuprght:
..			hpt125,OODES:
..			*L,*L,hpt025,hpdtch85,hpitalic,"Key:",hpdtch10,hpuprght:
..			hpt125,OMLRKY:
..			*L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
..			hpt125,O1DES:
..			*l,hpt125,o2DES,hpt725,jstreasn:
..			*L,*l,hpt025,hpdtch85,hpbon,hpunon,"Original Billing":
..			hpboff,hpunoff:
..			*L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
..			hpt425,"$ Per M",hpt600,"Amount Due",hpdtch10,hpuprght:
..			hpt175,M$QTY,hpt500,hpfixed,"@",M$PPM,hpt625,M$GRoss
.		PRINT	*N,hpt600,hpdtch85,hpitalic:
.			"Mail Date:",hpdtch10,hpuprght,hpt700:
.			OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY,hpdtch10,hpuprght:
.			*l,*l,*l,*l,hpt125,comnt:
.			*L,hpt025,hpdtch85,hpitalic,"Mailer's Offer:":
.			hpdtch10,hpuprght:
.			hpt125,OODES:
.			*L,*L,hpt025,hpdtch85,hpitalic,"Key:",hpdtch10,hpuprght:
.			hpt125,OMLRKY:
.			*L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
.			hpt125,O1DES:
.			*l,hpt125,NSEL2NAME,hpt725,jstreasn:
.			*L,*l,hpt025,hpdtch85,hpbon,hpunon,"Original Billing":
.			hpboff,hpunoff:
.			*L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
.			hpt425,"$ Per M",hpt600,"Amount Due",hpdtch10,hpuprght:
.			hpt175,M$QTY,hpt500,hpfixed,"@",M$PPM,hpt625,M$GRoss
.END PATCH 3.7 REPLACED LOGIC
..................................................
;Patch 3.78
.		move	"5025",row
.START PATCH 3.81 ADDED LOGIC
	if (MSWordFlag = 0)
.END PATCH 3.81 ADDED LOGIC
		move	"5025",verticalpos
.START PATCH 3.81 ADDED LOGIC
	else
		move	C3,howmany
	endif
.END PATCH 3.81 ADDED LOGIC
;Patch 3.78
	add	sixlpi,row
	FOR AcdRecCount,"1","15"
		MOve	NInvAcdRec(AcdRecCount).NinvAcdNumRec,NinvAcdNum
		MOve	NInvAcdRec(AcdRecCount).NinvAcdCodeRec,NinvAcdCode
		MOve	NInvAcdRec(AcdRecCount).NinvAcdRateRec,NinvAcdRate
		MOve	NInvAcdRec(AcdRecCount).NInvAcdPercRec,NInvAcdPerc
		MOve	NInvAcdRec(AcdRecCount).NINVAcdANINCDRec,NINVAcdANINCD
		MOve	NInvAcdRec(AcdRecCount).NINvAcdqtyRec,str9
		MOve	NInvAcdRec(AcdRecCount).NINvAcdTotalRec,str15
		MOve	NInvAcdRec(AcdRecCount).NinvAcdAextcdRec,NinvAcdAextcd
		MOve	NInvAcdRec(AcdRecCount).NinvAcdRateTRec,NinvAcdRateT
		MOve	NInvAcdRec(AcdRecCount).NINvAcdDescRec,nacdtext
		move	NINVAcdANINCD to str3
		call	trim using str3
		move	c0 to anincd
		move	str3 to Anincd
		If (NinvacdNum = "")
			Break
		endif
.
.			if (ninvoutflag = 1 & Ninvfrmflag = c3 & NInvAcdCode = "041")
		if (NInvAcdCode = "041")
			Goto DontPrintDet
		endif                           *
.
.			if (ninvoutflag = 1 & Ninvfrmflag = c3 & NInvAcdCode = "085")
		if (NInvAcdCode = "085")
			Goto DontPrintDet
		endif                           *
.			if (ninvoutflag = 1 & NinvFrmflag = c1 & (ANINCD = c1 or ANINCD = c3 or ANINCD = c4 ))
		if (ANINCD = c1 or ANINCD = c3 or ANINCD = c4 )
			Goto DontPrintDet               ;mailer copies do not print codes 1,3, or 4
		endif                           *
.			if (ninvoutflag = 1 & Elstcde = "C" & NinvFrmflag = c3 & NInvAcdCode = "003")
		if (Elstcde = "C" & NinvFrmflag = c3 & NInvAcdCode = "003")
			Goto DontPrintDet
		endif                           *
		clear	str10
		If (NinvAcdRate > 0)
			move	mask72 to str12
			edit	NinvAcdRate to str12
			Move	"@" to Str1
		else
			Clear	str1
			clear	str12
		endif
		move	"ZZZ.9999" to str8
		edit	NInvAcdPerc to str8
		cLEAR	m$ADDQTY
		If (str9 <> "")
			MOVE	MASK9 TO M$AddQTY
			MOVE	str9 TO N9
			Edit	N9 to M$AddQTY
		ENDIF
		add	"150" to VerticalPos
.			prtpage        Laser;*p800:VerticalPos,*font=Font09B,nacdtext,*p5000:VerticalPos,"@":
.START PATCH 3.81 ADDED LOGIC
		if (MSWordFlag = 0)
.END PATCH 3.81 ADDED LOGIC
			prtpage	Laser;*p800:VerticalPos,*font=Font09B,nacdtext,*p5000:VerticalPos,str1:
				*p=4500:VerticalPos,*ALIGNMENT=*right,M$AddQty,*ALIGNMENT=*Left:
.				*p=5475:VerticalPos,*ALIGNMENT=*right,NInvAcdRate,*ALIGNMENT=*Left:
				*p=5475:VerticalPos,*ALIGNMENT=*right,str12,*ALIGNMENT=*Left:
				*p=7500:VerticalPos,*ALIGNMENT=*right,str15,*ALIGNMENT=*Left
.START PATCH 3.81 ADDED LOGIC
		else
			add	C1,howmany
.
			call	Trim using nacdtext
			Table1.Cell giving TCell using howmany,2
			TCell.Range.InsertAfter using nacdtext
			setprop	TCell.Range,*Bold=OTRUE
.
			call	Trim using M$AddQty
			Table1.Cell giving TCell using howmany,4
			TCell.Range.InsertAfter using M$AddQty
			setprop	TCell.Range,*Bold=OTRUE
.
			Table1.Cell giving TCell using howmany,5
			TCell.Range.InsertAfter using str1
			setprop	TCell.Range,*Bold=OTRUE
.
			call	Trim using str12
			Table1.Cell giving TCell using howmany,6
			TCell.Range.InsertAfter using str12
			setprop	TCell.Range,*Bold=OTRUE
.
			call	Trim using str15
			Table1.Cell giving TCell using howmany,8
			TCell.Range.InsertAfter using str15
			setprop	TCell.Range,*Bold=OTRUE
		endif
.END PATCH 3.81 ADDED LOGIC
DontPrintDet
	repeat
.START PATCH 3.81 ADDED LOGIC
	if (MSWordFlag = 0)
.END PATCH 3.81 ADDED LOGIC
.               if             (NINVFrmFlag = c3)
.               Prtpage        Laser;*ALIGNMENT=*Left,*p=7600:10166,Typist
.               else
.               Prtpage        Laser;*p=7500:9916,*ALIGNMENT=*right,M$AR,*ALIGNMENT=*Left,*p=7600:9916,Typist
.               endif

;
;.
.               Return
.		Prtpage	Laser;*p=800:row,ADDESC1:
.			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT1:
.			*p=5475:row,ADD$RT1:
.			*p=7500:row,ADD$1,*ALIGNMENT=*left,*overlayon
.		add	sixlpi,row
.		Prtpage	Laser;*p=800:row,ADDESC2:
.			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT2:
.			*p=5475:row,ADD$RT2:
.			*p=7500:row,ADD$2,*ALIGNMENT=*left,*overlayon
.		add	sixlpi,row
.		Prtpage	Laser;*p=800:row,ADDESC3:
.			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT3:
.			*p=5475:row,ADD$RT3:
.			*p=7500:row,ADD$3,*ALIGNMENT=*left,*overlayon
.		add	sixlpi,row
.		Prtpage	Laser;*p=800:row,ADDESC4:
.			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT4:
.			*p=5475:row,ADD$RT4:
.			*p=7500:row,ADD$4,*ALIGNMENT=*left,*overlayon
.		add	sixlpi,row
.		Prtpage	Laser;*p=800:row,ADDESC5:
.			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT5:
.			*p=5475:row,ADD$RT5:
.			*p=7500:row,ADD$5,*ALIGNMENT=*left,*overlayon
.		add	sixlpi,row
.		Prtpage	Laser;*p=800:row,ADDESC6:
.			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT6:
.			*p=5475:row,ADD$RT6:
.			*p=7500:row,ADD$6,*ALIGNMENT=*left,*overlayon
.		add	sixlpi,row
.		Prtpage	Laser;*p=800:row,ADDESC7:
.			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT7:
.			*p=5475:row,ADD$RT7:
.			*p=7500:row,ADD$7,*ALIGNMENT=*left,*overlayon
.		add	sixlpi,row
.		Prtpage	Laser;*p=800:row,ADDESC8:
.			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT8:
.			*p=5475:row,ADD$RT8:
.			*p=7500:row,ADD$8,*ALIGNMENT=*left,*overlayon
.		add	sixlpi,row
.		Prtpage	Laser;*p=800:row,ADDESC9:
.			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT9:
.			*p=5475:row,ADD$RT9:
.			*p=7500:row,ADD$9,*ALIGNMENT=*left,*overlayon
.		add	sixlpi,row
.		Prtpage	Laser;*p=800:row,ADDESC10:
.			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT10:
.			*p=5475:row,ADD$RT10:
.			*p=7500:row,ADD$10,*ALIGNMENT=*left,*overlayon
		move	"7500",row
		sub	sixlpi,row
		Prtpage	Laser;*p=5000:row,"Original Total Due:":
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,M$AR,*ALIGNMENT=*left,*overlayon
		move	"7500",row
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc1:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$1,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc2:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$2,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc3:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$3,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc4:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$4,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc5:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$5,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc6:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$6,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc7:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$7,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc8:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$8,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc9:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$9,*ALIGNMENT=*left,*overlayon

		prtpage	Laser;*p=7500:9916,*ALIGNMENT=*right,*overlayoff,ARmask,*ALIGNMENT=*left,*overlayon:
			*p=7600:9916,Typist
		goto wipevars
.END PATCH 3.76 REPLACED LOGIC
	else
.MS Word Version
.START PATCH 3.81 REPLACED LOGIC
.		Table1.Cell giving TCell using 4,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using ADDESC1
.		Table1.Cell giving TCell using 4,5
.		TCell.Range.InsertAfter using AT1
.		Table1.Cell giving TCell using 4,6
.		TCell.Range.InsertAfter using ADD$RT1
.		Table1.Cell giving TCell using 4,8
.		TCell.Range.InsertAfter using ADD$1
..
.		Table1.Cell giving TCell using 5,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using ADDESC2
.		Table1.Cell giving TCell using 5,5
.		TCell.Range.InsertAfter using AT2
.		Table1.Cell giving TCell using 5,6
.		TCell.Range.InsertAfter using ADD$RT2
.		Table1.Cell giving TCell using 5,8
.		TCell.Range.InsertAfter using ADD$2
..
.		Table1.Cell giving TCell using 6,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using ADDESC3
.		Table1.Cell giving TCell using 6,5
.		TCell.Range.InsertAfter using AT3
.		Table1.Cell giving TCell using 6,6
.		TCell.Range.InsertAfter using ADD$RT3
.		Table1.Cell giving TCell using 6,8
.		TCell.Range.InsertAfter using ADD$3
..
.		Table1.Cell giving TCell using 7,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using ADDESC4
.		Table1.Cell giving TCell using 7,5
.		TCell.Range.InsertAfter using AT4
.		Table1.Cell giving TCell using 7,6
.		TCell.Range.InsertAfter using ADD$RT4
.		Table1.Cell giving TCell using 7,8
.		TCell.Range.InsertAfter using ADD$4
..
.		Table1.Cell giving TCell using 8,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using ADDESC5
.		Table1.Cell giving TCell using 8,5
.		TCell.Range.InsertAfter using AT5
.		Table1.Cell giving TCell using 8,6
.		TCell.Range.InsertAfter using ADD$RT5
.		Table1.Cell giving TCell using 8,8
.		TCell.Range.InsertAfter using ADD$5
..
.		Table1.Cell giving TCell using 9,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using ADDESC6
.		Table1.Cell giving TCell using 9,5
.		TCell.Range.InsertAfter using AT6
.		Table1.Cell giving TCell using 9,6
.		TCell.Range.InsertAfter using ADD$RT6
.		Table1.Cell giving TCell using 9,8
.		TCell.Range.InsertAfter using ADD$6
..
.		Table1.Cell giving TCell using 10,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using ADDESC7
.		Table1.Cell giving TCell using 10,5
.		TCell.Range.InsertAfter using AT7
.		Table1.Cell giving TCell using 10,6
.		TCell.Range.InsertAfter using ADD$RT7
.		Table1.Cell giving TCell using 10,8
.		TCell.Range.InsertAfter using ADD$7
..
.		Table1.Cell giving TCell using 11,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using ADDESC8
.		Table1.Cell giving TCell using 11,5
.		TCell.Range.InsertAfter using AT8
.		Table1.Cell giving TCell using 11,6
.		TCell.Range.InsertAfter using ADD$RT8
.		Table1.Cell giving TCell using 11,8
.		TCell.Range.InsertAfter using ADD$8
..
.		Table1.Cell giving TCell using 12,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using ADDESC9
.		Table1.Cell giving TCell using 12,5
.		TCell.Range.InsertAfter using AT9
.		Table1.Cell giving TCell using 12,6
.		TCell.Range.InsertAfter using ADD$RT9
.		Table1.Cell giving TCell using 12,8
.		TCell.Range.InsertAfter using ADD$9
..
.		Table1.Cell giving TCell using 13,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using ADDESC10
.		Table1.Cell giving TCell using 13,5
.		TCell.Range.InsertAfter using AT10
.		Table1.Cell giving TCell using 13,6
.		TCell.Range.InsertAfter using ADD$RT10
.		Table1.Cell giving TCell using 13,8
.		TCell.Range.InsertAfter using ADD$10
..Totals
.		Table1.Cell giving TCell using 15,7
.		TCell.Range.InsertAfter using "Original Total Due"
.		setprop	TCell.Range.Font,*Size=11
.		Table1.Cell giving TCell using 15,8
.		TCell.Range.InsertAfter using M$AR
..Next Section of Table 3
.		Table1.Cell giving TCell using 16,2
.		TCell.Range.InsertAfter using "Adjustments to Billing"
.		setprop	TCell.Range,*Bold=OTRUE
.		setprop	TCell.Range,*Underline=OTRUE
..
.		Table1.Cell giving TCell using 17,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using adjdesc1
.		Table1.Cell giving TCell using 17,8
.		TCell.Range.InsertAfter using adj$1
..
.		Table1.Cell giving TCell using 18,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using adjdesc2
.		Table1.Cell giving TCell using 18,8
.		TCell.Range.InsertAfter using adj$2
..
.		Table1.Cell giving TCell using 19,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using adjdesc3
.		Table1.Cell giving TCell using 19,8
.		TCell.Range.InsertAfter using adj$3
..
.		Table1.Cell giving TCell using 20,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using adjdesc4
.		Table1.Cell giving TCell using 20,8
.		TCell.Range.InsertAfter using adj$4
..
.		Table1.Cell giving TCell using 21,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using adjdesc5
.		Table1.Cell giving TCell using 21,8
.		TCell.Range.InsertAfter using adj$5
..
.		Table1.Cell giving TCell using 22,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using adjdesc6
.		Table1.Cell giving TCell using 22,8
.		TCell.Range.InsertAfter using adj$6
..
.		Table1.Cell giving TCell using 23,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using adjdesc7
.		Table1.Cell giving TCell using 23,8
.		TCell.Range.InsertAfter using adj$7
..
.		Table1.Cell giving TCell using 24,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using adjdesc8
.		Table1.Cell giving TCell using 24,8
.		TCell.Range.InsertAfter using adj$8
..
.		Table1.Cell giving TCell using 25,2
.		TCell.Range.InsertAfter using "     "
.		TCell.Range.InsertAfter using adjdesc9
.		Table1.Cell giving TCell using 25,8
.		TCell.Range.InsertAfter using adj$9
..Grand Totals
.		Table1.Cell giving TCell using 30,7
.		TCell.Range.InsertAfter using "Total Due"
.		Table1.Cell giving TCell using 30,8
.		TCell.Range.InsertAfter using ARmask
.		setprop	TCell.Range.Shading,*Texture=wdTexture15Percent
..
.		Table1.Cell giving TCell using 32,2
.		TCell.Range.InsertAfter using "Payment due upon receipt.  Please return copy with payment."
.		setprop	TCell.Range,*Bold=OTRUE
.		setprop	TCell.Range,*Italic=OTRUE
.		Table1.Cell giving TCell using 32,6
.		TCell.Range.InsertAfter using "Member Direct Marketing Association"
.		setprop	TCell.Range,*Italic=OTRUE
.		Table1.Cell giving TCell using 32,9
.		TCell.Range.InsertAfter using TYPIST
....................................................
		if (howmany > 13)
			add	C2,howmany
		else
			move	"15",howmany
		endif
.Totals
		Table1.Cell giving TCell using howmany,7
		TCell.Range.InsertAfter using "Original Total Due"
		setprop	TCell.Range.Font,*Size=11
		Table1.Cell giving TCell using howmany,8
		TCell.Range.InsertAfter using M$AR
.Next Section of Table 3
		add	C1,howmany
		Table1.Cell giving TCell using howmany,2
		TCell.Range.InsertAfter using "Adjustments to Billing"
		setprop	TCell.Range,*Bold=OTRUE
		setprop	TCell.Range,*Underline=OTRUE
.
		add	C1,howmany
		Table1.Cell giving TCell using howmany,2
		TCell.Range.InsertAfter using "     "
		TCell.Range.InsertAfter using adjdesc1
		Table1.Cell giving TCell using howmany,8
		TCell.Range.InsertAfter using adj$1
.
		add	C1,howmany
		Table1.Cell giving TCell using howmany,2
		TCell.Range.InsertAfter using "     "
		TCell.Range.InsertAfter using adjdesc2
		Table1.Cell giving TCell using howmany,8
		TCell.Range.InsertAfter using adj$2
.
		add	C1,howmany
		Table1.Cell giving TCell using howmany,2
		TCell.Range.InsertAfter using "     "
		TCell.Range.InsertAfter using adjdesc3
		Table1.Cell giving TCell using howmany,8
		TCell.Range.InsertAfter using adj$3
.
		add	C1,howmany
		Table1.Cell giving TCell using howmany,2
		TCell.Range.InsertAfter using "     "
		TCell.Range.InsertAfter using adjdesc4
		Table1.Cell giving TCell using howmany,8
		TCell.Range.InsertAfter using adj$4
.
		add	C1,howmany
		Table1.Cell giving TCell using howmany,2
		TCell.Range.InsertAfter using "     "
		TCell.Range.InsertAfter using adjdesc5
		Table1.Cell giving TCell using howmany,8
		TCell.Range.InsertAfter using adj$5
.
		add	C1,howmany
		Table1.Cell giving TCell using howmany,2
		TCell.Range.InsertAfter using "     "
		TCell.Range.InsertAfter using adjdesc6
		Table1.Cell giving TCell using howmany,8
		TCell.Range.InsertAfter using adj$6
.
		add	C1,howmany
		Table1.Cell giving TCell using howmany,2
		TCell.Range.InsertAfter using "     "
		TCell.Range.InsertAfter using adjdesc7
		Table1.Cell giving TCell using howmany,8
		TCell.Range.InsertAfter using adj$7
.
		add	C1,howmany
		Table1.Cell giving TCell using howmany,2
		TCell.Range.InsertAfter using "     "
		TCell.Range.InsertAfter using adjdesc8
		Table1.Cell giving TCell using howmany,8
		TCell.Range.InsertAfter using adj$8
.
		add	C1,howmany
		Table1.Cell giving TCell using howmany,2
		TCell.Range.InsertAfter using "     "
		TCell.Range.InsertAfter using adjdesc9
		Table1.Cell giving TCell using howmany,8
		TCell.Range.InsertAfter using adj$9
.Grand Totals
		add	C5,howmany
		Table1.Cell giving TCell using howmany,7
		TCell.Range.InsertAfter using "Total Due"
		Table1.Cell giving TCell using howmany,8
		TCell.Range.InsertAfter using ARmask
		setprop	TCell.Range.Shading,*Texture=wdTexture15Percent
.
		add	C2,howmany
		Table1.Cell giving TCell using howmany,2
		TCell.Range.InsertAfter using "Payment due upon receipt.  Please return copy with payment."
		setprop	TCell.Range,*Bold=OTRUE
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using howmany,6
		TCell.Range.InsertAfter using "Member Direct Marketing Association"
		setprop	TCell.Range,*Italic=OTRUE
		Table1.Cell giving TCell using howmany,9
		TCell.Range.InsertAfter using TYPIST
.END PATCH 3.81 REPLACED LOGIC
.Format Table 3
		move	".47",TWidth
		setprop	Table1.Columns(1),*Width=TWidth
		move	"2.09",TWidth
		setprop	Table1.Columns(2),*Width=TWidth
		move	".84",TWidth
		setprop	Table1.Columns(3),*Width=TWidth
		move	".72",TWidth
		setprop	Table1.Columns(4),*Width=TWidth
		move	".25",TWidth
		setprop	Table1.Columns(5),*Width=TWidth
		move	".67",TWidth
		setprop	Table1.Columns(6),*Width=TWidth
		move	"1.45",TWidth
		setprop	Table1.Columns(7),*Width=TWidth
		move	".9",TWidth
		setprop	Table1.Columns(8),*Width=TWidth
		move	".50",TWidth
		setprop	Table1.Columns(9),*Width=TWidth
.
		setprop	Table1.Borders,*Enable=OFALSE		.Make sure no other Borders are applied
		setprop	Table1.Borders,*OutsideLineStyle=1	.1 = Single Line
.Set up Alignment in last table.
.Note:  Table1 is a temporary object, and ends up linked to third Table.  If more are added, I will need to change following code.
.THIS MUST BE DONE BEFORE CELL MERGING!!!!!!!
.0 = Align Left
.1 = Align Center
.2 = Align Right
		getprop	Table1.Rows,*Count=result
		sub	C1,result
		for N9,C1,result
			Table1.Cell giving TCell using N9,2
			setprop	TCell.Range.Paragraphs(1),*Alignment=0
		repeat
		for N9,C1,result
			Table1.Cell giving TCell using N9,3
			setprop	TCell.Range.Paragraphs(1),*Alignment=0
		repeat
		Table1.Cell giving TCell using 2,3
		setprop	TCell.Range.Paragraphs(1),*Alignment=2
		for N9,C1,result
			Table1.Cell giving TCell using N9,4
			setprop	TCell.Range.Paragraphs(1),*Alignment=1
		repeat
		for N9,C1,result
			Table1.Cell giving TCell using N9,6
			setprop	TCell.Range.Paragraphs(1),*Alignment=2
		repeat
		for N9,C1,result
			Table1.Cell giving TCell using N9,7
			setprop	TCell.Range.Paragraphs(1),*Alignment=1
		repeat
		for N9,C1,result
			Table1.Cell giving TCell using N9,8
			setprop	TCell.Range.Paragraphs(1),*Alignment=2
		repeat
.
		for N8,"4","15"
			Table1.Cell giving TCell using N8,2
			Table1.Cell giving TCell2 using N8,3
			TCell.Merge using TCell2
		repeat
		for N8,"17","29"
			Table1.Cell giving TCell using N8,2
			Table1.Cell giving TCell2 using N8,3
			TCell.Merge using TCell2
		repeat
		Table1.Cell giving TCell using 32,6
		Table1.Cell giving TCell2 using 32,8
		TCell.Merge using TCell2
		Table1.Cell giving TCell using 32,2
		Table1.Cell giving TCell2 using 32,4
		TCell.Merge using TCell2
.
.START PATCH 3.76 REMOVED LOGIC
.		getprop	Range,*End=result
.		Range.InsertAfter using "1300 Clay Street, "
..Prep to format later.
.		getprop	Range,*End=N9
.		Range.InsertAfter using "11th"
.		getprop	Range,*End=N8
.		Range.InsertAfter using " floor, Oakland, CA 94612-1429 · (415) 989-3350 · FAX (415) 433-7796"
.		setprop	Range,*Start=PgStart
.		setprop Range.Font,*Name="Times New Roman"	.Default Font Name
.		sub	C1,result
.		setprop	Range,*Start=result
.		setprop Range.Font,*Size=12			.Default Font Size
.		setprop Range,*Bold=OFALSE
.		setprop Range,*Italic=OFALSE
..Format some stuff
.		setprop	Range,*Start=N9
.		setprop	Range,*End=N8
.		Range.AutoFormat
.END PATCH 3.76 REMOVED LOGIC
.Reset the Range
		setprop	Range,*Start=PgStart
.Format Rows which need to be centered
.0 = Align Left
.1 = Align Center
.2 = Align Right
.**I did not set up any Paragraphs, so each line is interpreted as its' own Paragraph.
.Each Cell in each Table is also, inherently, its own Paragraph!!
.START PATCH 3.76 REPLACED LOGIC
.		setprop	Range.Paragraphs(1),*Alignment=1
.		setprop	Range.Paragraphs(2),*Alignment=1
.		setprop	Range.Paragraphs(3),*Alignment=1
.		setprop	Range.Paragraphs(4),*Alignment=1
.		getprop	Range.Paragraphs,*Count=result
.		setprop	Range.Paragraphs(result),*Alignment=1
		setprop	Range.Paragraphs(6),*Alignment=1
.END PATCH 3.76 REPLACED LOGIC
.Place Border around "Invoice" language
		sub	C1,BordEnd
		sub	C1,BordStart
.If I do not make sure the Range is specifically only around the text, then the Border
.will appear for the whole line.  Border collection will assume I am speaking of a Paragraph.
		setprop	Range,*Start=BordStart
		setprop	Range,*End=BordEnd
	 	setprop	Range.Borders,*OutsideLineStyle=1	.1 = Single Line
	endif
.END PATCH 3.66 - REPLACED LOGIC
.End patch #3.3 - ADDED CENTURY
.begin patch 3.4
        move       c0 to n9
        move       irexqty to n9
        compare    c0 to n9
        if         not equal
         MOVE      MASK9 TO M$QTYX
         EDIT      n9 TO M$QTYx
.end patch 3.4
.Start Patch #3.3 - replaced var
.         move      c0 to form72
.         MOVE      iexPPM TO FORM72
.         MOVE      MASK32 TO M$PPMX
.         DIVIDE    HUND INTO FORM72
.         MOVE      FORM72 TO FORM32
.
         move      c0 to CMPT92
.begin patch 3.4
.         MOVE      iexPPM TO CMPT92
.         MOVE      MASK32 TO M$PPMX
.         DIVIDE    HUND INTO CMPT92
.         MOVE      CMPT92 TO FORM32
.         EDIT      FORM32 TO M$PPMX
         EDIT      iexppm TO M$PPMX
.end patch 3.4
.END Patch #3.3 - replaced var
.START PATCH 3.66 - REPLACED LOGIC
.        print      hpt175,hpdtch10,hpuprght,M$QTYX,hpt500,hpfixed,"@",M$PPMX
	if (MSWordFlag = 0)
.START PATCH 3.76 REPLACED LOGIC
.		print	hpt175,hpdtch10,hpuprght,M$QTYX,hpt500,hpfixed,"@",M$PPMX
;Patch 3.78
		Prtpage	Laser;*p=2500:5025,*ALIGNMENT=*right,*overlayoff,M$QtyX:
			*p=5000:5025,"@":
			*p=5475:5025,M$PPMX,*overlayon,*ALIGNMENT=*Left
;Patch 3.78
.END PATCH 3.76 REPLACED LOGIC
	else
.MS Word Version
	endif
.END PATCH 3.66 - REPLACED LOGIC
        endif
.                  *L,*L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
.                   hpt425,"$ PerM",hpt600,"Amount Due",hpdtch10,hpuprght;
.START PATCH 3.66 - REPLACED LOGIC
.        print      hpdtch10,*14,ADDESC1,hpt500,hpfixed,hpfixed,AT1,ADD$RT1,hpt625,ADD$1:
.                   *L,hpdtch10,*14,ADDESC2,hpt500,hpfixed,AT2,ADD$RT2,hpt625,ADD$2:
.                   *L,hpdtch10,*14,ADDESC3,hpt500,hpfixed,AT3,ADD$RT3,hpt625,ADD$3:
.                   *L,hpdtch10,*14,ADDESC4,hpt500,hpfixed,AT4,ADD$RT4,hpt625,ADD$4:
.                   *L,hpdtch10,*14,ADDESC5,hpt500,hpfixed,AT5,ADD$RT5,hpt625,ADD$5:
.                   *L,hpdtch10,*14,ADDESC6,hpt500,hpfixed,AT6,ADD$RT6,hpt625,ADD$6:
.                   *L,hpdtch10,*14,ADDESC7,hpt500,hpfixed,AT7,ADD$RT7,hpt625,ADD$7:
.                   *L,hpdtch10,*14,ADDESC8,hpt500,hpfixed,AT8,ADD$RT8,hpt625,ADD$8:
.                   *L,hpdtch10,*14,ADDESC9,hpt500,hpfixed,AT9,ADD$RT9,hpt625,ADD$9:
.                   *L,hpdtch10,*14,ADDESC10,hpt500,hpfixed,AT10,ADD$RT10,hpt625,ADD$10:
.                   *l,hpt500,"Original Total Due ",hpt625,hpfixed,M$AR,hpdtch10:
.                   *L,hpt025,hpdtch85,hpbon,hpunon,"Adjustments to Billing":
.                   hpboff,hpunoff:
.                   hpdtch10,*n,*14,adjdesc1,hpt500,hpfixed,hpfixed,hpt625,adj$1:
.                   *L,hpdtch10,*14,adjdesc2,hpt500,hpfixed,hpt625,adj$2:
.                   *L,hpdtch10,*14,adjdesc3,hpt500,hpfixed,hpt625,adj$3:
.                   *L,hpdtch10,*14,adjdesc4,hpt500,hpfixed,hpt625,adj$4:
.                   *L,hpdtch10,*14,adjdesc5,hpt500,hpfixed,hpt625,adj$5:
.                   *L,hpdtch10,*14,adjdesc6,hpt500,hpfixed,hpt625,adj$6:
.                   *L,hpdtch10,*14,adjdesc7,hpt500,hpfixed,hpt625,adj$7:
.                   *L,hpdtch10,*14,adjdesc8,hpt500,hpfixed,hpt625,adj$8:
.                   *L,hpdtch10,*14,adjdesc9,hpt500,hpfixed,hpt625,adj$9:
.                   *L:
.                   *L,*L,*L,hpdtch10:
.                   *L,*12:
.                   hpt625,033,"*c360A":      width
.                   033,"*c65B":       height
.                   033,"*c2G":        area fill%
.                   033,"*c2P":          print it shaded
.                   *l,hpt550,"Total Due ",hpt625:
.                   hpfixed,ARmask,hpdtch10:
.                   *L,*12:
.                   *N,*12,hpdtch85,hpitalic,hpbon,"Payment due upon ":
.                   "receipt. Please return copy with payment.":
.                   hpt525,hpboff,"Member Direct Marketing Association":
.                   hpdtch10,hpuprght,hpt750,TYPIST:
.                   *L:
.                   *L,*L,*L,*l,*FLUSH
.         GOTO      WIPEVARS
.................
	if (MSWordFlag = 0)
.START PATCH 3.76 REPLACED LOGIC
.		print	hpdtch10,*14,ADDESC1,hpt500,hpfixed,hpfixed,AT1,ADD$RT1,hpt625,ADD$1:
.			*L,hpdtch10,*14,ADDESC2,hpt500,hpfixed,AT2,ADD$RT2,hpt625,ADD$2:
.			*L,hpdtch10,*14,ADDESC3,hpt500,hpfixed,AT3,ADD$RT3,hpt625,ADD$3:
.			*L,hpdtch10,*14,ADDESC4,hpt500,hpfixed,AT4,ADD$RT4,hpt625,ADD$4:
.			*L,hpdtch10,*14,ADDESC5,hpt500,hpfixed,AT5,ADD$RT5,hpt625,ADD$5:
.			*L,hpdtch10,*14,ADDESC6,hpt500,hpfixed,AT6,ADD$RT6,hpt625,ADD$6:
.			*L,hpdtch10,*14,ADDESC7,hpt500,hpfixed,AT7,ADD$RT7,hpt625,ADD$7:
.			*L,hpdtch10,*14,ADDESC8,hpt500,hpfixed,AT8,ADD$RT8,hpt625,ADD$8:
.			*L,hpdtch10,*14,ADDESC9,hpt500,hpfixed,AT9,ADD$RT9,hpt625,ADD$9:
.			*L,hpdtch10,*14,ADDESC10,hpt500,hpfixed,AT10,ADD$RT10,hpt625,ADD$10:
.			*l,hpt500,"Original Total Due ",hpt625,hpfixed,M$AR,hpdtch10:
.			*L,hpt025,hpdtch85,hpbon,hpunon,"Adjustments to Billing":
.			hpboff,hpunoff:
.			hpdtch10,*n,*14,adjdesc1,hpt500,hpfixed,hpfixed,hpt625,adj$1:
.			*L,hpdtch10,*14,adjdesc2,hpt500,hpfixed,hpt625,adj$2:
.			*L,hpdtch10,*14,adjdesc3,hpt500,hpfixed,hpt625,adj$3:
.			*L,hpdtch10,*14,adjdesc4,hpt500,hpfixed,hpt625,adj$4:
.			*L,hpdtch10,*14,adjdesc5,hpt500,hpfixed,hpt625,adj$5:
.			*L,hpdtch10,*14,adjdesc6,hpt500,hpfixed,hpt625,adj$6:
.			*L,hpdtch10,*14,adjdesc7,hpt500,hpfixed,hpt625,adj$7:
.			*L,hpdtch10,*14,adjdesc8,hpt500,hpfixed,hpt625,adj$8:
.			*L,hpdtch10,*14,adjdesc9,hpt500,hpfixed,hpt625,adj$9:
.			*L:
.			*L,*L,*L,hpdtch10:
.			*L,*12:
.			hpt625,033,"*c360A":      width
.			033,"*c65B":       height
.			033,"*c2G":        area fill%
.			033,"*c2P":          print it shaded
.			*l,hpt550,"Total Due ",hpt625:
.			hpfixed,ARmask,hpdtch10:
.			*L,*12:
.			*N,*12,hpdtch85,hpitalic,hpbon,"Payment due upon ":
.			"receipt. Please return copy with payment.":
.			hpt525,hpboff,"Member Direct Marketing Association":
.			hpdtch10,hpuprght,hpt750,TYPIST:
.			*L:
.			*L,*L,*L,*l,*FLUSH
............................................................................
;Patch 3.78
		move	"5025",row
;Patch 3.78
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC1:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT1:
			*p=5475:row,ADD$RT1:
			*p=7500:row,ADD$1,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC2:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT2:
			*p=5475:row,ADD$RT2:
			*p=7500:row,ADD$2,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC3:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT3:
			*p=5475:row,ADD$RT3:
			*p=7500:row,ADD$3,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC4:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT4:
			*p=5475:row,ADD$RT4:
			*p=7500:row,ADD$4,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC5:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT5:
			*p=5475:row,ADD$RT5:
			*p=7500:row,ADD$5,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC6:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT6:
			*p=5475:row,ADD$RT6:
			*p=7500:row,ADD$6,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC7:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT7:
			*p=5475:row,ADD$RT7:
			*p=7500:row,ADD$7,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC8:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT8:
			*p=5475:row,ADD$RT8:
			*p=7500:row,ADD$8,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC9:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT9:
			*p=5475:row,ADD$RT9:
			*p=7500:row,ADD$9,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC10:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT10:
			*p=5475:row,ADD$RT10:
			*p=7500:row,ADD$10,*ALIGNMENT=*left,*overlayon
		move	"7500",row
		sub	sixlpi,row
		Prtpage	Laser;*p=5000:row,"Original Total Due:":
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,M$AR,*ALIGNMENT=*left,*overlayon
		move	"7500",row
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc1:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$1,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc2:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$2,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc3:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$3,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc4:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$4,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc5:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$5,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc6:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$6,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc7:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$7,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc8:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$8,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc9:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$9,*ALIGNMENT=*left,*overlayon
.
		prtpage	Laser;*p=7500:9916,*ALIGNMENT=*right,*overlayoff,ARmask,*ALIGNMENT=*left,*overlayon:
			*p=7600:9916,Typist
.END PATCH 3.76 REPLACED LOGIC
		GOTO WIPEVARS
	else
.MS Word Version
.All taken care of before this point!!
		call	wipevar1
	endif
.END PATCH 3.66 - REPLACED LOGIC
prtown
.Begin Patch 3.75
        CLEAR     ADDESC1
         CLEAR     ADDESC2
         CLEAR     ADDESC3
         CLEAR     ADDESC4
         CLEAR     ADDESC5
         CLEAR     ADDESC6
         CLEAR     ADDESC7
         CLEAR     ADDESC8
         CLEAR     ADDESC9
         CLEAR     ADDESC10
         CLEAR     ADD$1
         CLEAR     ADD$2
         CLEAR     ADD$3
         CLEAR     ADD$4
         CLEAR     ADD$5
         CLEAR     ADD$6
         CLEAR     ADD$7
         CLEAR     ADD$8
         CLEAR     ADD$9
         CLEAR     ADD$10
         CLEAR     ADD$RTE
         CLEAR     ADD$RT1
         CLEAR     ADD$RT2
         CLEAR     ADD$RT3
         CLEAR     ADD$RT4
         CLEAR     ADD$RT5
         CLEAR     ADD$RT6
         CLEAR     ADD$RT7
         CLEAR     ADD$RT8
         CLEAR     ADD$RT9
         CLEAR     ADD$RT10
         CLEAR     SHORT
         CLEAR     GUARPRT
         CLEAR     AT1
         CLEAR     AT2
         CLEAR     AT3
         CLEAR     AT4
         CLEAR     AT5
         CLEAR     AT6
         CLEAR     AT7
         CLEAR     AT8
         CLEAR     AT9
         CLEAR     AT10
         move      c3 to NINVFRMFLAG              .mailer/remit copies some LO & internal charges remain hidden
			move      "C" to elstcde                 force so Betsy Crones/non exclusive don't print Commission.
			call      compute
.End Patch 3.75
         branch    apflag of wipevars
         PACK      MKEY FROM MLRN,z3
.Begin Patch 3.72
.        call      nmlrkey
			pack		compfld3 from mkey
   	call		COMPKEY3
.End Patch 3.72
.START PATCH 3.79 ADDED LOGIC
			if (CNCTINACTIVE = "T")
				clear	cnctfname
			endif
.END PATCH 3.79 ADDED LOGIC
.START PATCH 3.66 - REPLACED LOGIC
.         PRINT     033,"&a0c0R";
..begin patch 3.6
..                   033,"&f4y3X",*L:             invoke macro.
.        call        prtinvfrm
..end patch 3.6
.        print      *L:
.                   *l:
.                   *L,*L,*l,*l,*l,*l:
.                   hpdtch10,hpitalic:
.                   hpt275,033,"*c900A":      width
.                   033,"*c125B":       height
.                   033,"*c2G":        area fill%
.                   033,"*c2P":          print it shaded
.                   *l,hpt285,hpdtch85,hpitalic,"Amount Due: ",ownocpy,*l,hpt475,ap1mask,*l:
.                   *L,hpt025,hpdtch85,hpitalic,"Date:",hpt425,"Invoice##":
.                   hpt600,"Mailer's P.O.##",hpdtch10,hpuprght:
.                   hpt125,adjdate:
..                   hpt500,invnum,dash,amendnum,hpt700,OMLRPON:
..                   hpt475,invnum,dash,jstsubno,hpt700,OMLRPON:
.                   hpt475,invnum,dash,amendnum,hpt700,OMLRPON:
.                   *L,hpt025,hpdtch85,hpitalic,"Owner ##",hpdtch10,hpuprght:
.                   hpt125,ownlon:
.                   *L,hpt125,ownlonm:
.                   *L,hpt125,ownocpy,hpt600,hpdtch85,hpitalic:
.                   "NIN L.R. ##",hpdtch10,hpuprght,hpt700,lrn:
.                   *L,hpt125,ownlosa:
.                   *L,hpt125,ownlocty," ",ownlos," ",ownlozc:
.                   *L,hpt025,hpdtch85,hpitalic,"Client ",hpdtch10,hpuprght:
.                   hpt125,mcOMP;
..                  hpt125,jstMLR:
..                  *l,hpt125,mNAME:
..                  *L,hpt075,careof,hpt125,mcOMP,hpt600,hpdtch85,hpitalic:
..                  "NIN L.R. ##",hpdtch10,hpuprght,hpt700,lrn:
..                  *l,hpt125,mADDR:
..                  *l,hpt125,mCITY," ",mSTATE," ",mZIP;
..Start patch #3.3 - ADDED CENTURY
..        PRINT      hpt600,hpdtch85,hpitalic:
..                   "Mail Date:",hpdtch10,hpuprght,hpt700:
..                   OMDTEM,"/",OMDTED,"/",OMDTEY,hpdtch10,hpuprght:
..                   *l,*l,*l,*l,hpt125,comnt:
..                   *L,hpt025,hpdtch85,hpitalic,"Mailer's Offer:":
..                   hpdtch10,hpuprght:
..                   hpt125,OODES:
..                   *L,*L,hpt025,hpdtch85,hpitalic,"Key:",hpdtch10,hpuprght:
..                   hpt125,OMLRKY:
..                   *L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
..                   hpt125,O1DES:
..                   *l,hpt125,o2DES,hpt725,jstreasn:
..                   *L,*l,hpt025,hpdtch85,hpbon,hpunon,"Original Billing":
..                   hpboff,hpunoff:
..                   *L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
..                   hpt425,"$ Per M",hpt600,"Amount Due",hpdtch10,hpuprght:
..                   hpt175,M$QTY,hpt500,hpfixed,"@",M$PPM,hpt625,M$GRoss
.        PRINT      hpt600,hpdtch85,hpitalic:
.                   "Mail Date:",hpdtch10,hpuprght,hpt700:
.                   OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY,hpdtch10,hpuprght:
.                   *l,*l,*l,*l,hpt125,comnt:
.                   *L,hpt025,hpdtch85,hpitalic,"Mailer's Offer:":
.                   hpdtch10,hpuprght:
.                   hpt125,OODES:
.                   *L,*L,hpt025,hpdtch85,hpitalic,"Key:",hpdtch10,hpuprght:
.                   hpt125,OMLRKY:
.                   *L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
.                   hpt125,O1DES:
.                   *l,hpt125,o2DES,hpt725,jstreasn:
.                   *L,*l,hpt025,hpdtch85,hpbon,hpunon,"Original Billing":
.                   hpboff,hpunoff:
.                   *L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
.                   hpt425,"$ Per M",hpt600,"Amount Due",hpdtch10,hpuprght:
.                   hpt175,M$QTY,hpt500,hpfixed,"@",M$PPM,hpt625,M$GRoss
.
.....................
.START PATCH 3.7 ADDED LOGIC
	packkey	NSEL2FLD,"1",LRN
	move	"NSEL2KEY-3",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	O2DES,NSEL2NAME
	endif
.END PATCH 3.7 ADDED LOGIC
	if (MSWordFlag = 0)
.START PATCH 3.76 REPLACED LOGIC
.		PRINT	033,"&a0c0R";
.		call	prtinvfrm
.		print	*L:
.			*l:
.			*L,*L,*l,*l,*l,*l:
.			hpdtch10,hpitalic:
.			hpt275,033,"*c900A":      width
.			033,"*c125B":       height
.			033,"*c2G":        area fill%
.			033,"*c2P":          print it shaded
.			*l,hpt285,hpdtch85,hpitalic,"Amount Due: ",ownocpy,*l,hpt475,ap1mask,*l:
.			*L,hpt025,hpdtch85,hpitalic,"Date:",hpt425,"Invoice##":
.			hpt600,"Mailer's P.O.##",hpdtch10,hpuprght:
.			hpt125,adjdate:
.			hpt475,invnum,dash,jstsubno,hpt700,OMLRPON:
.			*L,hpt025,hpdtch85,hpitalic,"Owner ##",hpdtch10,hpuprght:
.			hpt125,ownlon:
.			*L,hpt125,ownlonm:
.			*L,hpt125,ownocpy,hpt600,hpdtch85,hpitalic:
.			"NIN L.R. ##",hpdtch10,hpuprght,hpt700,lrn:
.			*L,hpt125,ownlosa:
.			*L,hpt125,ownlocty," ",ownlos," ",ownlozc:
.			*L,hpt025,hpdtch85,hpitalic,"Client ",hpdtch10,hpuprght:
.			hpt125,mcOMP;
..START PATCH 3.7 REPLACED LOGIC
..		PRINT	hpt600,hpdtch85,hpitalic:
..			"Mail Date:",hpdtch10,hpuprght,hpt700:
..			OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY,hpdtch10,hpuprght:
..			*l,*l,*l,*l,hpt125,comnt:
..			*L,hpt025,hpdtch85,hpitalic,"Mailer's Offer:":
..			hpdtch10,hpuprght:
..			hpt125,OODES:
..			*L,*L,hpt025,hpdtch85,hpitalic,"Key:",hpdtch10,hpuprght:
..			hpt125,OMLRKY:
..			*L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
..			hpt125,O1DES:
..			*l,hpt125,o2DES,hpt725,jstreasn:
..			*L,*l,hpt025,hpdtch85,hpbon,hpunon,"Original Billing":
..			hpboff,hpunoff:
..			*L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
..			hpt425,"$ Per M",hpt600,"Amount Due",hpdtch10,hpuprght:
..			hpt175,M$QTY,hpt500,hpfixed,"@",M$PPM,hpt625,M$GRoss
.		PRINT	hpt600,hpdtch85,hpitalic:
.			"Mail Date:",hpdtch10,hpuprght,hpt700:
.			OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY,hpdtch10,hpuprght:
.			*l,*l,*l,*l,hpt125,comnt:
.			*L,hpt025,hpdtch85,hpitalic,"Mailer's Offer:":
.			hpdtch10,hpuprght:
.			hpt125,OODES:
.			*L,*L,hpt025,hpdtch85,hpitalic,"Key:",hpdtch10,hpuprght:
.			hpt125,OMLRKY:
.			*L,*L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
.			hpt125,O1DES:
.			*l,hpt125,NSEL2NAME,hpt725,jstreasn:
.			*L,*l,hpt025,hpdtch85,hpbon,hpunon,"Original Billing":
.			hpboff,hpunoff:
.			*L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
.			hpt425,"$ Per M",hpt600,"Amount Due",hpdtch10,hpuprght:
.			hpt175,M$QTY,hpt500,hpfixed,"@",M$PPM,hpt625,M$GRoss
..END PATCH 3.7 REPLACED LOGIC
............................................................................
		call	prtinvfrm
.begin patch 4.1
		call	PrtOwnerboxGui
		
.		call	prtinvfrm3
.end patch 4.1
.Regular print out stuff
		call	Trim using ownlocty
		Prtpage	Laser;*font=Font09B,*p=1500:2212,adjdate:
			*p=5000:2212,invnum,dash,jstsubno:
			*p=6750:2212,OMLRPON:
			*p=1500:2400,ownlon:
			*p=1500:2525,ownlonm:
			*p=1500:2650,ownocpy:
			*p=6750:2712,LRN:
			*p=1500:2775,ownlosa:
			*p=1500:2900,*ll,ownlocty,", ",ownlos," ",ownlozc:
			*p=1500:3150,COMPCOMP
		Prtpage	Laser;*p=6750:3212,OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY:
			*p=1500:3602,*font=Font010,comnt,*font=Font09B:
			*p=1500:3800,OODES:
			*p=1500:4050,OMLRKY:
			*p=1500:4300,O1DES:
			*p=1500:4440,NSEL2NAME:
			*p=4250:4440,jstreasn:
			*p=2500:4900,*ALIGNMENT=*right,*overlayoff,M$Qty:
			*p=5000:4900,"@":
			*p=5475:4900,M$PPM:
			*p=7500:4900,M$GRoss,*overlayon,*ALIGNMENT=*Left
.END PATCH 3.76 REPLACED LOGIC
	else
.MS Word Version
	endif
.END PATCH 3.66 - REPLACED LOGIC
.End patch #3.3 - ADDED CENTURY
.begin patch 3.4
.        move       c0 to n7
.        move       irexqty to n7
.        compare    c0 to n7
.        if         not equal
.         MOVE      MASK7 TO M$QTYX
.         EDIT      n7 TO M$QTYx
        move       c0 to n9
        move       irexqty to n9
        compare    c0 to n9
        if         not equal
         MOVE      MASK9 TO M$QTYX
         EDIT      n9 TO M$QTYx
.end patch 3.4
.Start Patch #3.3 - replaced var
.         move      c0 to form72
.         MOVE      iexPPM TO FORM72
.         MOVE      MASK32 TO M$PPMX
.         DIVIDE    HUND INTO FORM72
.         MOVE      FORM72 TO FORM32
.
.begin patch 3.4
.         move      c0 to CMPT92
.         MOVE      iexPPM TO CMPT92
.         MOVE      MASK32 TO M$PPMX
.         DIVIDE    HUND INTO CMPT92
.         MOVE      CMPT92 TO FORM32
          move      iexppm to form32
.end patch 3.4
.END Patch #3.3 - replaced var
         EDIT      FORM32 TO M$PPMX
.START PATCH 3.66 - REPLACED LOGIC
.        print      hpt175,hpdtch10,hpuprght,M$QTYX,hpt500,hpfixed,"@",M$PPMX
	if (MSWordFlag = 0)
.START PATCH 3.76 REPLACED LOGIC
.		print	hpt175,hpdtch10,hpuprght,M$QTYX,hpt500,hpfixed,"@",M$PPMX
;Patch 3.78
		Prtpage	Laser;*p=2500:5025,*ALIGNMENT=*right,*overlayon,M$QtyX:
			*p=5025:4900,"@":
			*p=5475:4900,M$PPMX,*overlayoff,*ALIGNMENT=*Left
;Patch 3.78
.END PATCH 3.76 REPLACED LOGIC
	else
.MS Word Version
	endif
.END PATCH 3.66 - REPLACED LOGIC
        endif
.                  *L,*L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
.                   hpt425,"$ PerM",hpt600,"Amount Due",hpdtch10,hpuprght;
.START PATCH 3.66 - REPLACED LOGIC
.        print      hpdtch10,*14,ADDESC1,hpt500,hpfixed,hpfixed,AT1,ADD$RT1,hpt625,ADD$1:
.                   *L,hpdtch10,*14,ADDESC2,hpt500,hpfixed,AT2,ADD$RT2,hpt625,ADD$2:
.                   *L,hpdtch10,*14,ADDESC3,hpt500,hpfixed,AT3,ADD$RT3,hpt625,ADD$3:
.                   *L,hpdtch10,*14,ADDESC4,hpt500,hpfixed,AT4,ADD$RT4,hpt625,ADD$4:
.                   *L,hpdtch10,*14,ADDESC5,hpt500,hpfixed,AT5,ADD$RT5,hpt625,ADD$5:
.                   *L,hpdtch10,*14,ADDESC6,hpt500,hpfixed,AT6,ADD$RT6,hpt625,ADD$6:
.                   *L,hpdtch10,*14,ADDESC7,hpt500,hpfixed,AT7,ADD$RT7,hpt625,ADD$7:
.                   *L,hpdtch10,*14,ADDESC8,hpt500,hpfixed,AT8,ADD$RT8,hpt625,ADD$8:
.                   *L,hpdtch10,*14,ADDESC9,hpt500,hpfixed,AT9,ADD$RT9,hpt625,ADD$9:
.                   *L,hpdtch10,*14,ADDESC10,hpt500,hpfixed,AT10,ADD$RT10,hpt625,ADD$10:
.                   *l,hpt500,hpt625,hpfixed,hpdtch10:
.                   *L,hpt025,hpdtch85,hpbon,hpunon,"Adjustments to Billing":
.                   hpboff,hpunoff:
.                   hpdtch10,*n,*14,adjdesc1,hpt500,hpfixed,hpfixed,hpt625,adj$1:
.                   *L,hpdtch10,*14,adjdesc2,hpt500,hpfixed,hpt625,adj$2:
.                   *L,hpdtch10,*14,adjdesc3,hpt500,hpfixed,hpt625,adj$3:
.                   *L,hpdtch10,*14,adjdesc4,hpt500,hpfixed,hpt625,adj$4:
.                   *L,hpdtch10,*14,adjdesc5,hpt500,hpfixed,hpt625,adj$5:
.                   *L,hpdtch10,*14,adjdesc6,hpt500,hpfixed,hpt625,adj$6:
.                   *L,hpdtch10,*14,adjdesc7,hpt500,hpfixed,hpt625,adj$7:
.                   *L,hpdtch10,*14,adjdesc8,hpt500,hpfixed,hpt625,adj$8:
.                   *L,hpdtch10,*14,adjdesc9,hpt500,hpfixed,hpt625,adj$9:
.                   *L:
.                   *L,*L,*L,hpdtch10:
.                   *L,*12:
.                   *L,*12:
.                   hpdtch10:
.                   *L,*12:
.                   *N,*12,hpdtch85,hpitalic,hpbon:
.                   hpt525,hpboff,"Member Direct Marketing Association":
.                   hpdtch10,hpuprght,hpt750,TYPIST:
.                   *L:
.                   *L,*L,*L,*l,*FLUSH
.....................
	if (MSWordFlag = 0)
.START PATCH 3.76 REPLACED LOGIC
.		print	hpdtch10,*14,ADDESC1,hpt500,hpfixed,hpfixed,AT1,ADD$RT1,hpt625,ADD$1:
.			*L,hpdtch10,*14,ADDESC2,hpt500,hpfixed,AT2,ADD$RT2,hpt625,ADD$2:
.			*L,hpdtch10,*14,ADDESC3,hpt500,hpfixed,AT3,ADD$RT3,hpt625,ADD$3:
.			*L,hpdtch10,*14,ADDESC4,hpt500,hpfixed,AT4,ADD$RT4,hpt625,ADD$4:
.			*L,hpdtch10,*14,ADDESC5,hpt500,hpfixed,AT5,ADD$RT5,hpt625,ADD$5:
.			*L,hpdtch10,*14,ADDESC6,hpt500,hpfixed,AT6,ADD$RT6,hpt625,ADD$6:
.			*L,hpdtch10,*14,ADDESC7,hpt500,hpfixed,AT7,ADD$RT7,hpt625,ADD$7:
.			*L,hpdtch10,*14,ADDESC8,hpt500,hpfixed,AT8,ADD$RT8,hpt625,ADD$8:
.			*L,hpdtch10,*14,ADDESC9,hpt500,hpfixed,AT9,ADD$RT9,hpt625,ADD$9:
.			*L,hpdtch10,*14,ADDESC10,hpt500,hpfixed,AT10,ADD$RT10,hpt625,ADD$10:
.			*l,hpt500,hpt625,hpfixed,hpdtch10:
.			*L,hpt025,hpdtch85,hpbon,hpunon,"Adjustments to Billing":
.			hpboff,hpunoff:
.			hpdtch10,*n,*14,adjdesc1,hpt500,hpfixed,hpfixed,hpt625,adj$1:
.			*L,hpdtch10,*14,adjdesc2,hpt500,hpfixed,hpt625,adj$2:
.			*L,hpdtch10,*14,adjdesc3,hpt500,hpfixed,hpt625,adj$3:
.			*L,hpdtch10,*14,adjdesc4,hpt500,hpfixed,hpt625,adj$4:
.			*L,hpdtch10,*14,adjdesc5,hpt500,hpfixed,hpt625,adj$5:
.			*L,hpdtch10,*14,adjdesc6,hpt500,hpfixed,hpt625,adj$6:
.			*L,hpdtch10,*14,adjdesc7,hpt500,hpfixed,hpt625,adj$7:
.			*L,hpdtch10,*14,adjdesc8,hpt500,hpfixed,hpt625,adj$8:
.			*L,hpdtch10,*14,adjdesc9,hpt500,hpfixed,hpt625,adj$9:
.			*L:
.			*L,*L,*L,hpdtch10:
.			*L,*12:
.			*L,*12:
.			hpdtch10:
.			*L,*12:
.			*N,*12,hpdtch85,hpitalic,hpbon:
.			hpt525,hpboff,"Member Direct Marketing Association":
.			hpdtch10,hpuprght,hpt750,TYPIST:
.			*L:
.			*L,*L,*L,*l,*FLUSH
;Patch 3.78
		move	"5025",row
;Patch 3.78
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC1:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT1:
			*p=5475:row,ADD$RT1:
			*p=7500:row,ADD$1,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC2:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT2:
			*p=5475:row,ADD$RT2:
			*p=7500:row,ADD$2,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC3:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT3:
			*p=5475:row,ADD$RT3:
			*p=7500:row,ADD$3,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC4:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT4:
			*p=5475:row,ADD$RT4:
			*p=7500:row,ADD$4,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC5:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT5:
			*p=5475:row,ADD$RT5:
			*p=7500:row,ADD$5,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC6:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT6:
			*p=5475:row,ADD$RT6:
			*p=7500:row,ADD$6,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC7:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT7:
			*p=5475:row,ADD$RT7:
			*p=7500:row,ADD$7,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC8:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT8:
			*p=5475:row,ADD$RT8:
			*p=7500:row,ADD$8,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC9:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT9:
			*p=5475:row,ADD$RT9:
			*p=7500:row,ADD$9,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,ADDESC10:
			*p=5000:row,*ALIGNMENT=*right,*overlayoff,AT10:
			*p=5475:row,ADD$RT10:
			*p=7500:row,ADD$10,*ALIGNMENT=*left,*overlayon
		move	"7500",row
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc1:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$1,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc2:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$2,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc3:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$3,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc4:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$4,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc5:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$5,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc6:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$6,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc7:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$7,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc8:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$8,*ALIGNMENT=*left,*overlayon
		add	sixlpi,row
		Prtpage	Laser;*p=800:row,adjdesc9:
			*p=7500:row,*ALIGNMENT=*right,*overlayoff,adj$9,*ALIGNMENT=*left,*overlayon
.
		prtpage	Laser;*p=7500:9916,*ALIGNMENT=*right,*overlayoff,ARmask,*ALIGNMENT=*left,*overlayon:
			*p=7600:9916,Typist
		return
.END PATCH 3.76 REPLACED LOGIC
	else
.MS Word Version
	endif
.END PATCH 3.66 - REPLACED LOGIC

WIPEVARS
         branch    hotflag of wipevar1,done
wipevar1 CLEAR     ADjDESC1
         CLEAR     adjdESC2
         CLEAR     adjdESC3
         CLEAR     adjdESC4
         CLEAR     adjdESC5
         CLEAR     adjdESC6
         CLEAR     adjdESC7
         CLEAR     adjdESC8
         CLEAR     adjdESC9
         CLEAR     adj$1
         CLEAR     adj$2
         CLEAR     adj$3
         CLEAR     adj$4
         CLEAR     adj$5
         CLEAR     adj$6
         CLEAR     adj$7
         CLEAR     adj$8
         CLEAR     adj$9
         CLEAR     ADDESC1
         CLEAR     ADDESC2
         CLEAR     ADDESC3
         CLEAR     ADDESC4
         CLEAR     ADDESC5
         CLEAR     ADDESC6
         CLEAR     ADDESC7
         CLEAR     ADDESC8
         CLEAR     ADDESC9
         CLEAR     ADDESC10
         CLEAR     ADD$1
         CLEAR     ADD$2
         CLEAR     ADD$3
         CLEAR     ADD$4
         CLEAR     ADD$5
         CLEAR     ADD$6
         CLEAR     ADD$7
         CLEAR     ADD$8
         CLEAR     ADD$9
         CLEAR     ADD$10
         CLEAR     ADD$RTE
         CLEAR     ADD$RT1
         CLEAR     ADD$RT2
         CLEAR     ADD$RT3
         CLEAR     ADD$RT4
         CLEAR     ADD$RT5
         CLEAR     ADD$RT6
         CLEAR     ADD$RT7
         CLEAR     ADD$RT8
         CLEAR     ADD$RT9
         CLEAR     ADD$RT10
         CLEAR     SHORT
         CLEAR     GUARPRT
         CLEAR     AT1
         CLEAR     AT2
         CLEAR     AT3
         CLEAR     AT4
         CLEAR     AT5
         CLEAR     AT6
         CLEAR     AT7
         CLEAR     AT8
         CLEAR     AT9
         CLEAR     AT10
         clear     amendnum
.START PATCH 3.66 - REPLACED LOGIC
	if (MSWordFlag <> 0)
.MS Word Version
		return
	endif
.END PATCH 3.66 - REPLACED LOGIC
         BRANCH    HOTFLAG TO READADJ,DONE
         GOTO      readadj
.
testprt
.START PATCH 3.76 REMOVED LOGIC
.	PRINT     hpreset:
.                   hpport:
.                   033,"&l66P":               page length
.                   033,"&l65F":
.                   033,"&l1E",033,"&a0c0R";     top margin * print position
..begin patch 3.6
..                   033,"&f4y3X",*L:             invoke macro.
.        call        prtinvfrm
..end patch 3.6
.
.        Print      *L:
.                   *l,*l,*l,*l,*l,*l:
.                   *L,*L,*L,*L,*L:
.                   *L,*28,hpdtch85,hpitalic,"Amount Due: OWNER  $$$.$$":
.                   *L,*L,*L,hpt025,hpdtch85,hpitalic,"Date:",hpt425,"Invoice##":
.                   hpt600,"Mailer's P.O.##",hpdtch10,hpuprght:
.                   *28,"MM","/","DD":
.                   "/","YY",hpt500,"XXXXXX",hpt700,"XXXXXX":
.                   *L,hpt025,hpdtch85,hpitalic,"Client ##",hpdtch10,hpuprght:
.                   *28,"XXXX","/","XXX","-","X":
.                   *L,*28,"Mailer Contact":
.                   *L,*28,"Mailer Company Name",hpt600,hpdtch85,hpitalic:
.                   "NIN L.R. ##",hpdtch10,hpuprght,hpt700,"XXXXXX":
.                   *L,*28,"Street Address":
.                   *L,*28,"City," ","State"," ","Zip";
.        PRINT      *N,hpt600,hpdtch85,hpitalic:
.                   "Mail Date:",hpdtch10,hpuprght,hpt700:
.                   "MM","/","DD","/","YY",hpdtch10,hpuprght:
.                   *L,*L,*L:
.                   *L,hpt025,hpdtch85,hpitalic,"Mailer's Offer:":
.                   hpdtch10,hpuprght:
.                   *28,"Mailers Offer......":
.                   *L,*L,hpt025,hpdtch85,hpitalic,"Key:",hpdtch10,hpuprght:
.                   *24,"Mailers Key......":
.                   *L,hpt025,hpdtch85,hpitalic,"List:",hpdtch10,hpuprght:
.                   *28,"List Name................":
.                   *L,*28,"List Select...........":
.                   *L,*L,hpt025,hpdtch85,hpitalic,"Quantity Addressed":
.                   hpt425,"$ Per M",hpt600,"Amount Due",hpdtch10,hpuprght:
.                   hpt175,"99999",hpt500,hpfixed,"@","$$.$$",hpt625,"  Acc Rec":
.                   *L,hpdtch10,*14,"Additional Charge 1",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Additional Charge 2",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Additional Charge 3",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Additional Charge 4",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Additional Charge 5",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Additional Charge 6",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Additional Charge 7",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Additional Charge 8",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Additional Charge 9",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Additional Charge 10",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Adjustment Charges 1",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Adjustment Charges 2",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Adjustment Charges 3",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Adjustment Charges 4",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Adjustment Charges 5",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Adjustment Charges 6",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Adjustment Charges 7",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Adjustment Charges 8",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *L,hpdtch10,*14,"Adjustment Charges 9",hpt500,hpfixed,hpfixed,"@","$$$.$$",hpt625,"Amount $,$$":
.                   *l,*l,*l,hpdtch10:
.                   *L,*12,"Note- List Owner requires payment of this":
.                   *L,*12,"Invoice before additional orders for this":
.                   *L,*12,"Mailer can be processed.":
.                   hpt550,"Total Due ",hpt625,hpfixed,"$,$$$.$$",hpdtch10:
.                   *N,*12,hpdtch85,hpitalic,hpbon,"Payment due upon ":
.                   "receipt. Please return copy with payment.":
.                   hpt525,hpboff,"Member Direct Marketing Association":
.                   hpdtch10,hpuprght,hpt750,"XX":
.                   *L:
.                   *L,*L,*L,*l,*l,*FLUSH
.END PATCH 3.76 REMOVED LOGIC
         GOTO      readadj
.
READBILL
;Begin Patch 3.72
         bump      cobn by 2
         move      cobn to billtn
         reset     cobn
         PACK      NBILFLD FROM MLRN,COBN,BILLTN
         CALL      NBILKEY
         if        not over
	         MOVE      B3 TO CAREOF
	         MOVE      BILNAME TO CNCTFNAME
   	      move      bilcomp to COMPCOMP
      	   move    BILaddr to COMPADDR
         	move    BILcity to COMPCITY
	         move    BILState to COMPSTATE
   	      move    BILzip to COMPZIP
         endif
.         PACK      NBILFLD FROM MKEY,JSTBILTO
.         CALL      NBILKEY
.End Patch 3.72
         goto      goon
.
.
.Begin Patch 3.72 Comment Out
READMLR
.			MOVE      MNAME TO BILNAME
.         MOVE      MCOMP TO BILCOMP
.         MOVE      MADDR TO BILADDR
.         MOVE      MCITY TO BILCITY
.         MOVE      MSTATE TO BILSTATE
.         MOVE      MZIP TO BILZIP
.End Patch 3.72 Comment Out
         RETURN
.
.begin patch 3.5

.CVT - CONVERT FROM COMPUTATIONAL.
.   ENTER WITH CVTFLD
.   LEAVE WITH CVTFLD
.
.CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
.         RETURN    IF EQUAL                      ITS OK.
.FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",OLRN
.         RETURN                                POP THE STACK.
..
..CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
..
OWNPREP  MOVE      OLON TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         MOVE      PAYTN TO STR1
         MOVE      PAYTN TO PAYCHK
         PACK      PAYKEY FROM NOWNFLD,STR1
         REP       ZFILL IN PAYKEY
         MOVE      PAYKEY TO NPAYFLD
         REP       ZFILL IN NPAYFLD
         CLEAR     PCOMP                   *PCBUS DOES NOT CLEAR ON OVER.
         CLEAR      PNAME
         CLEAR      PSTREET
         CLEAR      PCITY
         CLEAR      PSTATE
         CLEAR      PZIP
         CALL      NPAYKEY
         CALL      NOWNKEY
         CMATCH    B1 TO PCOMP
         IF        NOT EOS
         COMPARE   C0 TO PAYCHK
         IF        EQUAL
         MOVE      PCOMP TO OWNOCPY
         MOVE      PNAME TO OWNLONM
         MOVE      PSTREET TO OWNLOSA
         MOVE      PCITY TO OWNLOCTY
         MOVE      PSTATE TO OWNLOS
         MOVE      PZIP TO OWNLOZC
         ENDIF
         ENDIF
         RETURN
.
charges  move      c0 to amendno
         move      c0 to index
.
nextadj  add      c1 to amendno
         add      c1 to index
         pack      njstfld from invnum,amendno
         rep       zfill in njstfld
         display   *p1:23,*el,njstfld
         call      njstkey
         goto      lstchrg if over
.begin patch 3.5
.         move      jstar to cvtfld
.         CALL      CVT
.         MOVE      CVTFLD TO FORM82
.         DIV       HUNDRD INTO FORM82

..Start Patch #3.3 - replaced var
..         move      c0 to form72
..         ADD       FORM82 TO FORM72
..         add       form72 to formar
         move      c0 to CMPT92
.         ADD       FORM82 TO CMPT92
         ADD       jstar TO CMPT92
         add       CMPT92 to formar
.end patch 3.5
.END Patch #3.3 - replaced var
.
OUTadj1
         move     c0 to n2
         match    "99" to jstreasn
         if       equal
         move     adjres99 to nadjtext
         goto     chrgbr
         endif
         move     jstreasn to n2
         clear   nadjtext
         load     nadjtext from n2 of adjres1,adjres2,adjres3,adjres4,adjres5:
                 adjres6,adjres7,adjres8,adjres9,adjres10,adjres11,adjres12:
                 adjres13,adjres14,adjres15,adjres16,adjres17,adjres18,adjres19:
                 adjres20,adjres21,adjres22,adjres23,adjres24,adjres25,adjres26:
                 adjres27,adjres28,adjres29,adjres30,adjres31,adjres32,adjres33:
                 adjres34,adjres35,adjres36,adjres37
.
chrgbr
         BRANCH    INDEX OF CHARG1,CHARG2,CHARG3,CHARG4,CHARG5,CHARG6:
                   CHARG7,CHARG8,CHARG9
         GOTO      lstCHRG
.
.Start Patch #3.3 - replaced var
.CHARG1
.         MOVE      MASK72 TO adj$1
.         EDIT      FORM72 TO adj$1
.         MOVE      nadjtext TO adjdesc1
.         GOTO      nextadj
.CHARG2
.         MOVE      MASK72 TO adj$2
.         EDIT      FORM72 TO adj$2
.         MOVE      nadjtext TO adjdesc2
.         GOTO      nextadj
.CHARG3
.         MOVE      MASK72 TO adj$3
.         EDIT      FORM72 TO adj$3
.         MOVE      nadjtext TO adjdesc3
.         GOTO      nextadj
.CHARG4
.         MOVE      MASK72 TO adj$4
.         EDIT      FORM72 TO adj$4
.         MOVE      nadjtext TO adjdesc4
.         GOTO      nextadj
.CHARG5
.         MOVE      MASK72 TO adj$5
.         EDIT      FORM72 TO adj$5
.         MOVE      nadjtext TO adjdesc5
.         GOTO      nextadj
.CHARG6
.         MOVE      MASK72 TO adj$6
.         EDIT      FORM72 TO adj$6
.         MOVE      nadjtext TO adjdesc6
.         GOTO      nextadj
.CHARG7
.         MOVE      MASK72 TO adj$7
.         EDIT      FORM72 TO adj$7
.         MOVE      nadjtext TO adjdesc7
.         GOTO      nextadj
.CHARG8
.         MOVE      MASK72 TO adj$8
.         EDIT      FORM72 TO adj$8
.         MOVE      nadjtext TO adjdesc8
.         GOTO      nextadj
.CHARG9
.         MOVE      MASK72 TO adj$9
.         EDIT      FORM72 TO adj$9
.         MOVE      nadjtext TO adjdesc9
.         GOTO      nextadj
....
CHARG1
         MOVE      MASK92 TO adj$1
         EDIT      CMPT92 TO adj$1
         MOVE      nadjtext TO adjdesc1
         GOTO      nextadj
CHARG2
         MOVE      MASK92 TO adj$2
         EDIT      CMPT92 TO adj$2
         MOVE      nadjtext TO adjdesc2
         GOTO      nextadj
CHARG3
         MOVE      MASK92 TO adj$3
         EDIT      CMPT92 TO adj$3
         MOVE      nadjtext TO adjdesc3
         GOTO      nextadj
CHARG4
         MOVE      MASK92 TO adj$4
         EDIT      CMPT92 TO adj$4
         MOVE      nadjtext TO adjdesc4
         GOTO      nextadj
CHARG5
         MOVE      MASK92 TO adj$5
         EDIT      CMPT92 TO adj$5
         MOVE      nadjtext TO adjdesc5
         GOTO      nextadj
CHARG6
         MOVE      MASK92 TO adj$6
         EDIT      CMPT92 TO adj$6
         MOVE      nadjtext TO adjdesc6
         GOTO      nextadj
CHARG7
         MOVE      MASK92 TO adj$7
         EDIT      CMPT92 TO adj$7
         MOVE      nadjtext TO adjdesc7
         GOTO      nextadj
CHARG8
         MOVE      MASK92 TO adj$8
         EDIT      CMPT92 TO adj$8
         MOVE      nadjtext TO adjdesc8
         GOTO      nextadj
CHARG9
         MOVE      MASK92 TO adj$9
         EDIT      CMPT92 TO adj$9
         MOVE      nadjtext TO adjdesc9
         GOTO      nextadj
.END Patch #3.3 - replaced var
.
lstchrg
         match      "27" to jstreasn
         goto       readadj if equal
         match      "14" to jstreasn
         goto       readadj if equal
         return
+.............................................................................
.begin patch 3.4
.COMPUTE
.         MOVE      c0 TO SELECT
.         MOVE      c0 TO SHIP
.         CLEAR     PREPAYSW
.         MOVE      c0 TO FORMAP2
.         MOVE      c0 TO SAVEAP
.         MOVE      c0 TO TAXES
.         MOVE      c0 TO BRKCOM
.         MOVE      c0 TO CMPCOM
.         MOVE      c0 TO LRINC
.         MOVE      c0 TO AP
.         MOVE      c0 TO SVECOM
.         MOVE      c0 TO PREPAY
.         MOVE      c0 TO POST
.         MOVE      c0 TO GROSS
.         MOVE      c0 TO FORMAR
.         MOVE      c0 TO PRICE
.         MOVE      C0 TO PRICEx
.         MOVE      c0 TO AMOUNT
.         MOVE      c0 TO AMOUNTX
.         MOVE      c0 TO SVEACR
.         MOVE      c0 TO CANUSE
..START Patch #3.3 - replaced var
..         MOVE      c0 TO FORM72
.         MOVE      c0 TO CMPT92
..END Patch #3.3 - replaced var
.         MOVE      c0 TO FORM72X
.         MOVE      c0 TO FORM73
..Start Patch #3.3 - replaced var
..         MOVE      c0 TO FORM74
..         MOVE      c0 TO FORM74X
.         MOVE      c0 TO CMPT94
.         MOVE      c0 TO CMPT94X
..END Patch #3.3 - replaced var
.         MOVE      c0 TO FORM32
..
..START Patch #3.3 - replaced var
..         MOVE      QTYSHP TO FORM74
..         COMPARE   c0 TO FORM74
...         GOTO      FNINCD IF EQUAL
..         DIVIDE    THOUS INTO FORM74
..         MOVE      FORM74 TO AMOUNT              QUANTITY BILLED
...
..         MOVE      iRexqty TO FORM74x              .split qty
. .        DIVIDE    THOUS INTO FORM74x
. .        MOVE      FORM74x TO AMOUNTX            .split QUANTITY BILLED
. .
..         MOVE      c0 TO FORM72
..         MOVE      PPM TO FORM72
..         COMPARE   c0 TO FORM72
..         GOTO      FNINCD IF EQUAL
..         DIVIDE    HUND INTO FORM72
. .        MOVE      FORM72 TO PRICE               PRICE PER M
. .
.        MOVE      QTYSHP TO CMPT94
.        COMPARE   c0 TO CMPT94
.        GOTO      FNINCD IF EQUAL
.         DIVIDE    THOUS INTO CMPT94
.         MOVE      CMPT94 TO AMOUNT              QUANTITY BILLED
..
.         MOVE      iRexqty TO CMPT94x              .split qty
.         DIVIDE    THOUS INTO CMPT94x
.         MOVE      CMPT94x TO AMOUNTX            .split QUANTITY BILLED
.
.         MOVE      c0 TO CMPT92
.         MOVE      PPM TO CMPT92
.         COMPARE   c0 TO CMPT92
.         GOTO      FNINCD IF EQUAL
.         DIVIDE    HUND INTO CMPT92
.         MOVE      CMPT92 TO PRICE               PRICE PER M
..END Patch #3.3 - replaced var
..
..         MOVE      C0 TO FORM72x                  exchange
.        MOVE      iexppm TO FORM72x              portion
.        DIVIDE    HUND INTO FORM72x              of the
.        MOVE      FORM72x TO PRICEX              split PRICE PER M
..
..START Patch #3.3 - replaced var
..         MULT      PRICE BY FORM74
..         MOVE      FORM74 TO GROSS               GROSS BILLING.
...         MOVE      FORM74 TO FORMAR                  ACCOUNTS RECEIVABLE.
. .        MOVE      FORM74 TO SVEACR              WORKING AR.
...
. .        MULT      PRICEx BY FORM74x             .split portion
. .        add       FORM74x TO GROSS               GROSS BILLING.
...         add       FORM74x TO FORMAR                  ACCOUNTS RECEIVABLE.
...must add after a/p calced - endchrgs.
. .        add       FORM74x TO SVEACR              WORKING AR.
......
.         MULT      PRICE BY CMPT94
.         MOVE      CMPT94 TO GROSS               GROSS BILLING.
.         MOVE      CMPT94 TO FORMAR                  ACCOUNTS RECEIVABLE.
.         MOVE      CMPT94 TO SVEACR              WORKING AR.
..
.         MULT      PRICEx BY CMPT94x             .split portion
.         add       CMPT94x TO GROSS               GROSS BILLING.
...         add       CMPT94x TO FORMAR                  ACCOUNTS RECEIVABLE.
..must add after a/p calced - endchrgs.
.         add       CMPT94x TO SVEACR              WORKING AR.
..END Patch #3.3 - replaced var
..
.FNINCD   MOVE      c0 TO N1                      CLEAR BRANCH VAR.
.         MOVE      PAYCODE TO N1                     PAYABLE CODE
.         BRANCH    N1 TO CHKCHRGS,FNINCD2,CHKCHRGS,FNINCD4
...FNINCD2  MOVE      c0 TO FORM32
. .        MOVE      COMMPCT TO FORM32
...FNINCD2  COMPARE   "0" TO FORM32                    c0 COMMISSION?
. .        COMPARE   "0" TO FORM32                    c0 COMMISSION?
. .        GOTO      CHKCHRGS IF EQUAL
. .        DIVIDE    HUND INTO FORM32
. .        MOVE      FORMAR TO LRINC
. .        MULT      FORM32 BY LRINC               LR INCOME.
.. .        MOVE      LRINC TO CMPCOM
.         GOTO      CHKCHRGS
.FNINCD4  MOVE      FORMAR TO LRINC
..
.CHKCHRGS MOVE      "0" TO INDEX
.NEXTCHRG ADD       "1" TO INDEX
.         CLEAR     PREPAYSW
.         COMPARE   "11" TO INDEX
..         GOTO      ENDCHRGS IF EQUAL
.         LOAD      STR14 FROM INDEX OF ADDCHG1,ADDCHG2,ADDCHG3,ADDCHG4,ADDCHG5:
.                   ADDCHG6,ADDCHG7,ADDCHG8,ADDCHG9,ADDCHG10
.         UNPACK    STR14 INTO ADDCODE,STR12
.         UNPACK    STR12 TO STR7,AEXTCD,STR3,STR1
.         MOVE      " " TO ATPRT
.         MATCH     "    " TO ADDCODE
.         GOTO      ENDCHRGS IF EQUAL
...
.         MOVE      ADDCODE TO NACDFLD
.         CALL      nacdkey      GET ADD CHARGE DESC
..
..
.         MOVE      c0 TO ACAMT
.         MOVE      c0 TO ACCMPR
.         MOVE      c0 TO ANINCD
...
..START Patch #3.3 - replaced var
..         MOVE      STR7 TO FORM72
..         DIVIDE    HUND BY FORM72
..         MOVE      FORM72 TO ACAMT
.         MOVE      STR7 TO CMPT92
.         DIVIDE    HUND BY CMPT92
.         MOVE      CMPT92 TO ACAMT
...END Patch #3.3 - replaced var
..
.         MOVE      STR3 TO FORM32
.         DIVIDE    HUND BY FORM32
.         MOVE      c0 TO ACCMPR
.         MOVE      FORM32 TO ACCMPR
..
.         MOVE      c0 TO ANINCD             CLEAR BRANCH
..         MOVE      STR1 TO ANINCD
..
.         MOVE      ADDCODE TO CODENUM
.         BRANCH    CODENUM TO CD01:      01    BROKER COMMISION
.                              CD02:      02    BROKER COMMISSION
.                              CD03:      03    broker commission
.                              CD04:      04    TAXES
.                              CD05:      05    TAXES
..                              CD06:      06    TAXES
.                              CD07:      07    TAXES
.                              CD08:      08    TAXES
.                              CD09:      09    TAXES
.                              CD10:      10    TAXES
.                              CD11:      11    TAXES
.                              SLCTPERM:  12    INDUSTRY SELECT (NY=BROKER COMM)
.                              SLCTPERM:  13    SELECTIONS
..                              SLCTPERM:  14    SELECTIONS
.                              SLCTPERM:  15    SELECTIONS
.                              SLCTPERM:  16    SELECTIONS
.                              SLCTPERM:  17    SELECTIONS
.                              SLCTPERM:  18    SELECTIONS
.                              SLCTPERM:  19    SELECTIONS
.                              SLCTPERM:  20    SELECTIONS
.                              SLCTPERM:  21    SELECTIONS
..                              SLCTPERM:  22    SELECTIONS
.                              SLCTPERM:  23    SELECTIONS
.                              SLCTPERM:  24    SELECTIONS
.                              SLCTPERM:  25    SELECTIONS
.                              SLCTPERM:  26    SELECTIONS
.                              SLCTPERM:  27    SELECTIONS
.                              SLCTPERM:  28    SELECTIONS
.                              SLCTPERM:  29    SELECTIONS
..                              SLCTPERM:  30    SELECTIONS
.                              SLCTPERM:  31    SELECTIONS
.                              SLCTPERM:  32    SELECTIONS
.                              SLCTPERM:  33    SELECTIONS
.                              SLCTPERM:  34    SELECTIONS
.                              SLCTPERM:  35    SELECTIONS
.                              SLCTPERM:  36    SELECTIONS
.                              SLCTPERM:  37    SELECTIONS
..                              SLCTPERM:  38    SELECTIONS
.                              SLCTPERM:  39    SELECTIONS
.                              SLCTPERM:  40    SELECTIONS
.                              SLCTPERM:  41    SELECTIONS
.                              SLCTPERM:  42    SELECTIONS
.                              SLCTPERM:  43    SELECTIONS
.                              SLCTPERM:  44    SELECTIONS
.                              SLCTPERM:  45    SELECTIONS
..                              SLCTPERM:  46    SELECTIONS
.                              SHIPPERM:  47    SHIPPING
.                              SHIPPERM:  48    SHIPPING
.                              SLCTPERM:  49    SELECTIONS
.                              SLCTPERM:  50    SELECTIONS
.                              SLCTFLAT:  51    MIN FLAT. (NY=GROSS BILLING)
.                              SLCTFLAT:  52    SELECTIONS
.                              SLCTFLAT:  53    SELECTIONS
..                              SHIPFLAT:  54    SHIPPING
.                              SLCTFLAT:  55    SELECTIONS
.                              SLCTFLAT:  56    SELECTIONS
.                              SLCTFLAT:  57    SELECTIONS
.                              TAXFLAT:   58    TAXES
.                              SLCTFLAT:  59    SELECTIONS
.                              SHIPFLAT:  60    SHIPPING
.                              SLCTFLAT:  61    SELECTIONS
..                              SHIPFLAT:  62    SHIPPING
.                              SLCTFLAT:  63    SELECTIONS
.                              SLCTFLAT:  64    SELECTIONS
.                              SLCTFLAT:  65    SELECTIONS
.                              SLCTFLAT:  66    SELECTIONS
.                              SLCTFLAT:  67    EXCHANGE FEE
.                              SLCTFLAT:  68    SELECTIONS
.                              SHIPFLAT:  69    SHIPPING
..                              SLCTFLAT:  70    SELECTIONS
.                              SHIPFLAT:  71    SHIPPING
.                              CD72:      72    OUR POSTAGE
.                              SHIPFLAT:  73    SHIPPING
.                              SHIPFLAT:  74    SHIPPING
.                              SHIPFLAT:  75    SHIPPING
.                              SHIPFLAT:  76    SHIPPING
.                              SHIPFLAT:  77    SHIPPING
..                              SHIPFLAT:  78    SHIPPING
.                              SHIPFLAT:  79    SHIPPING
.                              SHIPFLAT:  80    SHIPPING
.                              SLCTFLAT:  81    SELECTIONS
.                              SHIPFLAT:  82    SHIPPING
.                              SHIPFLAT:  83    SHIPPING
.                              SLCTFLAT:  84    SELECTIONS
.                              SLCTFLAT:  85    SELECTIONS
..                              SLCTFLAT:  86    SELECTIONS
.                              SLCTFLAT:  87    SELECTIONS
.                              SLCTFLAT:  88    SELECTIONS
.                              SLCTFLAT:  89    SELECTIONS
.                              SLCTFLAT:  90    SELECTIONS
.                              SLCTFLAT:  91    SELECTIONS
.                              TAXFLAT:   92    TAXES
.                              TAXFLAT:   93    TAXES
..                              TAXFLAT:   94    TAXES
.                              CD95:      95    BROKER COMMISSION
.                              CD96:      96    PRE-PAYMENTS
.                              CD97:      97    BROKER COMMISSION
.                              SLCTFLAT:  98    SELECTIONS
.                              CREDIT   99    CREDIT
.         GOTO      CD00
..
..SLCTPERM
..START Patch #3.3 - replaced var
..         MOVE      QTYSHP TO FORM74
..         DIVIDE    THOUS BY FORM74
..         MULTIPLY  ACAMT BY FORM74
..         MOVE      FORM74 TO AMOUNT
..
.         MOVE      QTYSHP TO CMPT94
..         DIVIDE    THOUS BY CMPT94
.         MULTIPLY  ACAMT BY CMPT94
.         MOVE      CMPT94 TO AMOUNT
..END Patch #3.3 - replaced var
.         ADD       AMOUNT TO SELECT
..lets handle a split
.         cmatch    yes to mcopies
.         goto      slctprmx if not equal
..         reset fulhouse
.         scan  ownctn in fulhouse         .applicable fulifiment house?
.         if        equal                         .yes, lets check for affected selects.
.         compare   "15" to codenum               .sex select?
.         goto       slctprmx if equal            .yes, calc it
.         compare   "17" to codenum               .key  select?
.         goto       slctprmx if equal            .yes, calc it
.         compare   "19" to codenum               .split  select?
..         goto       slctprmx if equal            .yes, calc it
.         compare   "34" to codenum               .recency  select?
.         goto       slctprmx if equal            .yes, calc it
.         compare   "42" to codenum               .ps labels  select?
.         goto       slctprmx if equal            .yes, calc it
.         endif
.         goto      sumamt               .none of the above - exit.
.slctprmx
..         reset fulhouse
.         scan  ownctn in fulhouse         .applicable fulifiment house?
.         if         equal                 take rental only.
..START Patch #3.3 - replaced var
..         move      c0 to form74x
..         MOVE      iRexqty TO FORM74x
..         DIVIDE    THOUS BY FORM74x
..         MULTIPLY  ACAMT BY FORM74x
...         add       FORM74x TO AMOUNT
..         ADD       form74x TO SELECT
....
.         move      c0 to CMPT94x
.         MOVE      iRexqty TO CMPT94x
.         DIVIDE    THOUS BY CMPT94x
.         MULTIPLY  ACAMT BY CMPT94x
.         add       CMPT94x TO AMOUNT
..         ADD       CMPT94x TO SELECT
..END Patch #3.3 - replaced var
.         endif
.         GOTO      SUMAMT
..
.SHIPPERM
..START Patch #3.3 - replaced var
..         MOVE      QTYSHP TO FORM74
...         DIVIDE    THOUS BY FORM74
..         MULTIPLY  ACAMT BY FORM74
..         MOVE      FORM74 TO AMOUNT
..         ADD       AMOUNT TO SHIP
...lets handle a split
..         MOVE      iRexqty TO FORM74x
..         DIVIDE    THOUS BY FORM74x
..         MULTIPLY  ACAMT BY FORM74x
...         add       FORM74x TO AMOUNT
..         ADD       form74x TO ship
....
.         MOVE      QTYSHP TO CMPT94
.         DIVIDE    THOUS BY CMPT94
.         MULTIPLY  ACAMT BY CMPT94
.         MOVE      CMPT94 TO AMOUNT
.         ADD       AMOUNT TO SHIP
...lets handle a split
.         MOVE      iRexqty TO CMPT94x
.         DIVIDE    THOUS BY CMPT94x
.         MULTIPLY  ACAMT BY CMPT94x
.         add       CMPT94x TO AMOUNT
.         ADD       CMPT94x TO ship
..END Patch #3.3 - replaced var
..
..         GOTO      SUMAMT
..
.SLCTFLAT MOVE      ACAMT TO AMOUNT
.         ADD       AMOUNT TO SELECT
.         GOTO      SUMAMT
..
.SHIPFLAT MOVE      ACAMT TO AMOUNT
.         ADD       AMOUNT TO SHIP
..         GOTO      SUMAMT
..
.TAXFLAT  MOVE      ACAMT TO AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
.CREDIT
.         MOVE      ACAMT TO AMOUNT
..         MULT      SEQ BY AMOUNT
.         GOTO      SUMAMT
..
.. ACAMT IS NOT IN CORRECT FORM, IT SHOULD HAVE 3 DEC PLACES.
.. USE ITS ALPHA EQUIVLENT FROM THE UNPACK ABOVE.
..
.CD00     MATCH     "0331" TO MLRN
.         GOTO      CD00CONT IF NOT EQUAL
..         PACK      STR4 WITH INVDTEY,INVDTEM
.         MATCH     "8601" TO STR4
.         GOTO      OUTCHRGS IF EQUAL
.CD00CONT MOVE      STR7 TO FORM73
.         DIVIDE    THOUS BY FORM73
.         MULTIPLY  PRICE BY FORM73
.         MULTIPLY  ACCMPR BY FORM73
.         MULTIPLY  "-1" BY FORM73
..         MOVE      FORM73 TO AMOUNT
..         KEYIN     *P1:24,*EL,"AMOUNT=",*DV,AMOUNT,STR1
..
..         ADD       AMOUNT TO SVEACR
..         MOVE      SVEACR TO LRINC
..         KEYIN     *P1:24,*EL,"AR BEFORE=",*DV,AR,STR1
.         ADD       AMOUNT TO FORMAR
..         KEYIN     *P1:24,*EL,"AR=",*DV,AR,STR1
...         MOVE      FORMAR TO LRINC
..
..         MOVE      COMMPCT TO FORM32
..         DIVIDE    HUND INTO FORM32
..         MULTIPLY  FORM32 BY LRINC
..         MOVE      SVEACR TO GROSS
..         MOVE      LRINC TO CMPCOM
.         ADD       AMOUNT TO LRINC
...         KEYIN     *P1:24,*EL,"FORM32=",*DV,FORM32,STR1
..         KEYIN     *P1:24,*EL,"LR=",*DV,OLRN," LRINC=",*DV,LRINC,STR1
.         GOTO      ADDAMT
..
.CD01     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".10" BY AMOUNT
.         ADD       AMOUNT TO BRKCOM
.         move      brkcom to amount
..         MULTIPLY  "-1" BY AMOUNT
.         ADD       AMOUNT TO LRINC
.         GOTO      SUMAMT
..
.CD02     MOVE      SVEACR TO AMOUNT
..         MULTIPLY  ".20" BY AMOUNT
.         MULTIPLY  ".05" BY AMOUNT
.         ADD       AMOUNT TO BRKCOM
..         move      brkcom to amount
.         MULTIPLY  "-1" BY AMOUNT
.         ADD       AMOUNT TO LRINC
.         GOTO      SUMAMT
..
.CD03     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".20" BY AMOUNT
.         ADD       AMOUNT TO BRKCOM
..         move      brkcom to amount
.         MULTIPLY  "-1" BY AMOUNT
.         ADD       AMOUNT TO LRINC
.         GOTO      SUMAMT
..        MULTIPLY  ".0525" BY AMOUNT
..        ADD       AMOUNT TO TAXES
..        GOTO      SUMAMT
..
...CD04     MOVE      SVEACR TO AMOUNT
..         MULTIPLY  ".0400" BY AMOUNT
..         ADD       AMOUNT TO TAXES
..         GOTO      SUMAMT
..
.CD04     MOVE      SVEACR TO AMOUNT
.         MOVE      AMOUNT TO CANUSE        *CANADIAN USE TAX
.         SUB       LRINC FROM CANUSE
..         MULT      ".10" BY CANUSE
.         MULT      "-1" BY CANUSE
.         MOVE      CANUSE TO AMOUNT
.         GOTO      SUMAMT
..
.CD05     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0500" BY AMOUNT
.         ADD       AMOUNT TO TAXES
..         GOTO      SUMAMT
..
.CD06     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0600" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
.CD07     MOVE      SVEACR TO AMOUNT
..         MULTIPLY  ".0800" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
.CD08     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0700" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
...
.CD09     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0625" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
.CD10     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0725" BY AMOUNT
..         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
.CD11     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0825" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
..
..CD12     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".15" BY AMOUNT
.         ADD       AMOUNT TO BRKCOM
.         MULTIPLY  "-1" BY AMOUNT
.         ADD       AMOUNT TO LRINC
.         GOTO      SUMAMT
..
.CD51     MOVE      ACAMT TO AMOUNT
..         ADD       AMOUNT TO GROSS
.         GOTO      SUMAMT
..
.CD72     MOVE      ACAMT TO AMOUNT
.         ADD       AMOUNT TO SHIP
.         ADD       AMOUNT TO FLOAMT
.         ADD       AMOUNT TO POST
.         GOTO      SUMAMT
...
.CD95     ADD       ACAMT TO BRKCOM
.         MOVE      ACAMT TO AMOUNT
.         MULTIPLY  "-1" BY AMOUNT
.         GOTO      SUMAMT
..
.CD96     MOVE      ACAMT TO AMOUNT
.         ADD       AMOUNT TO PREPAY
..         MOVE      YES TO PREPAYSW
.         GOTO      OUTCHRGS
..         GOTO      SUMAMT
..
.CD97     ADD       ACAMT TO BRKCOM
.         MOVE      ACAMT TO AMOUNT
.         MULTIPLY  "-1" BY AMOUNT
.         GOTO      SUMAMT
...
.SUMAMT   MOVE      AMOUNT TO SVECOM
.         COMPARE   "0" TO ACCMPR
.         GOTO      ADDAMT IF EQUAL
.         MULTIPLY  ACCMPR BY SVECOM
.         ADD       SVECOM TO LRINC
..START Patch #3.3 - replaced var
..         MOVE      AMOUNT TO FORM72
...         SUB       SVECOM FROM FORM72
..         MOVE      FORM72 TO SVECOM
.         MOVE      AMOUNT TO CMPT92
.         SUB       SVECOM FROM CMPT92
.         MOVE      CMPT92 TO SVECOM
..END Patch #3.3 - replaced var
.******
..
..ADDAMT   BRANCH    ANINCD TO ANINCD1,ANINCD2
..ANINCD0 - ADD. CHG CODE = ' ', ADD. CHG. TO AR.
.ANINCD0  ADD       AMOUNT TO FORMAR
.         GOTO      OUTCHRGS
..ANINCD1 - ADD. CHG. CODE - '1', ADD. CHG. TO A/P
.ANINCD1  CMATCH    "2" TO PAYCODE
.         GOTO      ADDCOM IF EQUAL
.         ADD       AMOUNT TO AP
..         GOTO      OUTCHRGS
.ADDCOM   ADD       SVECOM TO AP
.         GOTO      OUTCHRGS
.ANINCD2  ADD       AMOUNT TO FORMAR
.         GOTO      ANINCD1
..
.OUTCHRGS
.         compare   c1 to formflag        * mailer copy ?  9/21/94 dlh
..         if        equal                 * yes, so if charge is marked hidden
.         BRANCH    ANINCD TO NEXTCHRG    * DON'T PRINT CHARGE
.         endif                           *
.         compare   c2 to formflag        * owner copy ?  11/09/94 dlh
.         if        equal                 * yes,
.         match     "41" to addcode       * broker commission code ?
.         goto      nextchrg if equal     * yes, don't print.
.         endif                           *
..         CMATCH    "C" TO ELSTCDE
.         IF        EQUAL
.         compare   c2 to formflag        * owner copy ?  11/09/94 dlh
.         if        equal                 * yes,
.         match     "03" to addcode       * commission code ?
.         goto      nextchrg if equal     * yes, don't print.
.         endif                           *
.         endif
..         MOVE      c0 TO FORM2
.         MOVE      ADDCODE TO FORM2
.         COMPARE   FORM2 TO ELEVEN
.         GOTO      CLEARAT IF NOT LESS
.         COMPARE   FORM2 TO FIFTY1
.         GOTO      LOADAT IF NOT LESS
.         GOTO      CLEARAT
.LOADAT   MOVE      AT TO ATPRT      *LOAD PRICE PER M INTO PRINTLINE.
..         MOVE      MASK32 TO ADD$RTE
.         MOVE      ACAMT TO FORM32
.         EDIT      FORM32 TO ADD$RTE
.         GOTO      OUTCHRG1
.CLEARAT  MOVE      B1 TO ATPRT    *FLAT RATE CLEAR PRICE PER M IN PRINTLINE.
.         CLEAR     ADD$RTE
.         GOTO      OUTCHRG1
.OUTCHRG1
...START Patch #3.3 - replaced var
..         MOVE      c0 TO FORM72
..         ADD       AMOUNT TO FORM72
.         MOVE      c0 TO CMPT92
.         ADD       AMOUNT TO CMPT92
..END Patch #3.3 - replaced var
.         CMATCH    YES TO PREPAYSW        *PREPAYMENT ?
.         CALL      SUBPRE IF EQUAL          *YES
..         BRANCH    INDEX OF CHARGE1,CHARGE2,CHARGE3,CHARGE4,CHARGE5,CHARGE6:
.                   CHARGE7,CHARGE8,CHARGE9,CHARGE10
.         GOTO      NEXTCHRG
..
.SUBPRE
..START Patch #3.3 - replaced var
..         MULT      "-1" BY FORM72
.         MULT      "-1" BY CMPT92
...END Patch #3.3 - replaced var
.         RETURN
..
..START Patch #3.3 - replaced var
..CHARGE1
..         MOVE      MASK72 TO ADD$1
..         EDIT      FORM72 TO ADD$1
..         MOVE      NACDTEXT TO ADDESC1
...         MOVE      ATPRT TO AT1
..         MOVE      ADD$RTE TO ADD$RT1
..         GOTO      NEXTCHRG
..CHARGE2
..         MOVE      MASK72 TO ADD$2
..         EDIT      FORM72 TO ADD$2
..         MOVE      NACDTEXT TO ADDESC2
..         MOVE      ATPRT TO AT2
...         MOVE      ADD$RTE TO ADD$RT2
..         GOTO      NEXTCHRG
..CHARGE3
..         MOVE      MASK72 TO ADD$3
..         EDIT      FORM72 TO ADD$3
..         MOVE      NACDTEXT TO ADDESC3
..         MOVE      ATPRT TO AT3
..         MOVE      ADD$RTE TO ADD$RT3
...         GOTO      NEXTCHRG
..CHARGE4
..         MOVE      MASK72 TO ADD$4
..         EDIT      FORM72 TO ADD$4
..         MOVE      NACDTEXT TO ADDESC4
..         MOVE      ADD$RTE TO ADD$RT4
..         MOVE      ATPRT TO AT4
..         GOTO      NEXTCHRG
...CHARGE5
..         MOVE      MASK72 TO ADD$5
..         EDIT      FORM72 TO ADD$5
..         MOVE      NACDTEXT TO ADDESC5
..         MOVE      ADD$RTE TO ADD$RT5
..         MOVE      ATPRT TO AT5
..         GOTO      NEXTCHRG
..CHARGE6
...         MOVE      MASK72 TO ADD$6
..         EDIT      FORM72 TO ADD$6
..         MOVE      NACDTEXT TO ADDESC6
..         MOVE      ATPRT TO AT6
..         MOVE      ADD$RTE TO ADD$RT6
..         GOTO      NEXTCHRG
..CHARGE7
..         MOVE      MASK72 TO ADD$7
...         EDIT      FORM72 TO ADD$7
..         MOVE      NACDTEXT TO ADDESC7
..         MOVE      ADD$RTE TO ADD$RT7
..         MOVE      ATPRT TO AT7
..         GOTO      NEXTCHRG
..CHARGE8
..         MOVE      MASK72 TO ADD$8
..         EDIT      FORM72 TO ADD$8
...         MOVE      NACDTEXT TO ADDESC8
..         MOVE      ATPRT TO AT8
..         MOVE      ADD$RTE TO ADD$RT8
..         GOTO      NEXTCHRG
..CHARGE9
..         MOVE      MASK72 TO ADD$9
..         EDIT      FORM72 TO ADD$9
..         MOVE      ATPRT TO AT9
...         MOVE      NACDTEXT TO ADDESC9
..         MOVE      ADD$RTE TO ADD$RT9
..         GOTO      NEXTCHRG
..CHARGE10
..         MOVE      MASK72 TO ADD$10
..         EDIT      FORM72 TO ADD$10
..         MOVE      ATPRT TO AT10
..         MOVE      NACDTEXT TO ADDESC10
...         MOVE      ADD$RTE TO ADD$RT10
..         GOTO      NEXTCHRG
.....
.CHARGE1
.         MOVE      MASK92 TO ADD$1
.         EDIT      CMPT92 TO ADD$1
.         MOVE      NACDTEXT TO ADDESC1
.         MOVE      ATPRT TO AT1
..         MOVE      ADD$RTE TO ADD$RT1
.         GOTO      NEXTCHRG
.CHARGE2
.         MOVE      MASK92 TO ADD$2
.         EDIT      CMPT92 TO ADD$2
.         MOVE      NACDTEXT TO ADDESC2
.         MOVE      ATPRT TO AT2
.         MOVE      ADD$RTE TO ADD$RT2
..         GOTO      NEXTCHRG
.CHARGE3
.         MOVE      MASK92 TO ADD$3
.         EDIT      CMPT92 TO ADD$3
.         MOVE      NACDTEXT TO ADDESC3
.         MOVE      ATPRT TO AT3
.         MOVE      ADD$RTE TO ADD$RT3
.         GOTO      NEXTCHRG
..CHARGE4
.         MOVE      MASK92 TO ADD$4
.         EDIT      CMPT92 TO ADD$4
.         MOVE      NACDTEXT TO ADDESC4
.         MOVE      ADD$RTE TO ADD$RT4
.         MOVE      ATPRT TO AT4
.         GOTO      NEXTCHRG
.CHARGE5
..         MOVE      MASK92 TO ADD$5
.         EDIT      CMPT92 TO ADD$5
.         MOVE      NACDTEXT TO ADDESC5
.         MOVE      ADD$RTE TO ADD$RT5
.         MOVE      ATPRT TO AT5
.         GOTO      NEXTCHRG
.CHARGE6
.         MOVE      MASK92 TO ADD$6
..         EDIT      CMPT92 TO ADD$6
.         MOVE      NACDTEXT TO ADDESC6
.         MOVE      ATPRT TO AT6
.         MOVE      ADD$RTE TO ADD$RT6
.         GOTO      NEXTCHRG
.CHARGE7
.         MOVE      MASK92 TO ADD$7
.         EDIT      CMPT92 TO ADD$7
..         MOVE      NACDTEXT TO ADDESC7
.         MOVE      ADD$RTE TO ADD$RT7
.         MOVE      ATPRT TO AT7
.         GOTO      NEXTCHRG
.CHARGE8
.         MOVE      MASK92 TO ADD$8
.         EDIT      CMPT92 TO ADD$8
.         MOVE      NACDTEXT TO ADDESC8
..         MOVE      ATPRT TO AT8
.         MOVE      ADD$RTE TO ADD$RT8
.         GOTO      NEXTCHRG
.CHARGE9
.         MOVE      MASK92 TO ADD$9
.         EDIT      CMPT92 TO ADD$9
.         MOVE      ATPRT TO AT9
.         MOVE      NACDTEXT TO ADDESC9
..         MOVE      ADD$RTE TO ADD$RT9
.         GOTO      NEXTCHRG
.CHARGE10
.         MOVE      MASK92 TO ADD$10
.         EDIT      CMPT92 TO ADD$10
.         MOVE      ATPRT TO AT10
.         MOVE      NACDTEXT TO ADDESC10
.         MOVE      ADD$RTE TO ADD$RT10
..         GOTO      NEXTCHRG
..END Patch #3.3 - replaced var
..
.ENDCHRGS MOVE      c0 TO N1              CLEAR BRANCH VAR.
.         MOVE      PAYCODE TO N1
.         BRANCH    N1 TO PAYCD1,PAYCD2,PAYCD3
..PAYCD0 - PAYCODE = '0' OR ' ', SO GROSS=A/P
.PAYCD0   MOVE      FORMAR TO AP
..         SUBTRACT  LRINC FROM AP
.         SUBTRACT  TAXES FROM AP
.         SUBTRACT  PREPAY FROM FORMAR
..START Patch #3.3 - replaced var
..         add       form74x to formar
.         add       CMPT94x to formar
..END Patch #3.3 - replaced var
.         RETURN
...PAYCD1 - PAYCODE = '1', NININCOME = GROSS - A/P - TAXES.
.PAYCD1   MOVE      FORMAR TO NININC
.         SUBTRACT  AP FROM NININC
.         SUBTRACT  TAXES FROM NININC
.         SUBTRACT  POST FROM NININC
.         SUBTRACT  PREPAY FROM FORMAR
..START Patch #3.3 - replaced var
..         add       form74x to formar
..         add       CMPT94x to formar
..END Patch #3.3 - replaced var
.         RETURN
...PAYCD2 - PAYCODE = '2', 2- A/P'S, LRINCOME =
..PAYCD2   MOVE      FORMAR TO FORM72
..         MOVE      AP TO SAVEAP
..         SUBTRACT  AP FROM FORM72
..         MOVE      FORM72 TO AP
...         SUBTRACT  LRINC FROM AP
..         SUBTRACT  PREPAY FROM FORMAR
..         MOVE      AP TO FORMAP2
..         MOVE      SAVEAP TO AP
..         RETURN
..PAYCD2 - PAYCODE = '2', 2- A/P'S,
..         A/P1 AS DEFINED IN INVOICE RECORD.
.PAYCD2
...START Patch #3.3 - replaced var
..         MOVE      c0 TO FORM72
..         MOVE      AP1 TO FORM72
..         DIV       HUND INTO FORM72
..         MOVE      FORM72 TO SAVEAP
..         MOVE      SAVEAP TO AP
..         MOVE      FORMAR TO FORM72
..         SUBTRACT  AP FROM FORM72
...         MOVE      AP2 TO FORMAP2
..         DIV       HUND INTO FORMAP2
..         SUB       FORMAP2 FROM FORM72
..         SUBTRACT  PREPAY FROM FORMAR
..         MOVE      FORM72 TO LRINC
....
.         MOVE      c0 TO CMPT92
.         MOVE      AP1 TO CMPT92
..         DIV       HUND INTO CMPT92
.         MOVE      CMPT92 TO SAVEAP
.         MOVE      SAVEAP TO AP
.         MOVE      FORMAR TO CMPT92
.         SUBTRACT  AP FROM CMPT92
.         MOVE      AP2 TO FORMAP2
.         DIV       HUND INTO FORMAP2
.         SUB       FORMAP2 FROM CMPT92
..         SUBTRACT  PREPAY FROM FORMAR
.         MOVE      CMPT92 TO LRINC
..END Patch #3.3 - replaced var
.         SUB       TAXES FROM LRINC
.         SUB       POST FROM LRINC
..START Patch #3.3 - replaced var
..         add       form74x to formar
.         add       CMPT94x to formar
...END Patch #3.3 - replaced var
.         RETURN
.PAYCD3
..START Patch #3.3 - replaced var
..         MOVE      c0 TO FORM72
..         MOVE      AP1 TO FORM72
..         DIVIDE     HUND INTO FORM72
..         MOVE      FORM72 TO AP
...
.         MOVE      c0 TO CMPT92
.         MOVE      AP1 TO CMPT92
.         DIVIDE     HUND INTO CMPT92
.         MOVE      CMPT92 TO AP
..END Patch #3.3 - replaced var
.         MOVE      FORMAR TO LRINC
.         SUBTRACT  AP FROM LRINC
...         MOVE      AR TO CMPCOM
..        SUBTRACT  FORM72 FROM CMPCOM
.         SUBTRACT  TAXES FROM LRINC
.         SUBTRACT  POST FROM LRINC
.        SUBTRACT  PREPAY FROM FORMAR
..START Patch #3.3 - replaced var
..         add       form74x to formar
.         add       CMPT94x to formar
...END Patch #3.3 - replaced var
.         RETURN
..
.end patch 3.4
.*......................................................................
..
DONE
.START PATCH 3.66 ADDED LOGIC
	if (MSWordFlag = 1)
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.word.exe will still be running.
		destroy	Range
		clear	INPNAME
		return
	endif
.END PATCH 3.66 ADDED LOGIC
.START PATCH 3.76 REPLACED LOGIC
.         PRINT     *FLUSH;
	if (HOTFLAG = C2)        .HOT
		if (LSTNUM = "013260")
			call	PrtOwn
		endif
		prtclose Laser
		shutdown  "cls"
	endif
	prtclose Laser
.END PATCH 3.76 REPLACED LOGIC
         BRANCH    FORMFLAG TO MLREOJ
         SPLCLOSE
         CLOCK     TIME TO TIME
         DISPLAY   *P20:07,"DONE TIME : ",TIME
         shutdown  "cls"
         STOP
MLREOJ   move       C2 TO FORMFLAG
         display   *p10:14,"OWNer COPY"
         COMPARE   C1 TO HOTFLAG
         IF        EQUAL
         CLOSE     nadjust
.START PATCH 3.76 REPLACED LOGIC
.         splclose
.         OPEN      NADJUST,INPNAME
..START PATCH 3.63 REPLACED LOGIC
..         SPLOPEN   "\\nts0\d\DATA\Nadj2OWN.LST"
.         PACK      STR35,NTWKPATH1,"Nadj2OWN.LST"
.         SPLOPEN   STR35
..END PATCH 3.63 REPLACED LOGIC
.         PRINT     hpreset,hpport:
.                   033,"&l66P":               page length
.                   033,"&l65F":               number lines
.                   033,"&l1E",033,"&a0c0R"     top margin * print position
.......................................................................
        if (osflag = c1 | osflag = c5 | osflag = c6)         .nt
                PRTOPEN Laser,"\\NTS0\Laser2","FAXFILE.PRN"
	elseif (osflag = C3 | osflag = C4)
                PRTOPEN Laser,"Laser2","FAXFILE.PRN"
        else   .(osflag = c0)         .Don't know prompt for printer
                PRTOPEN Laser,"-","FAXFILE.PRN"
        endif
.END PATCH 3.76 REPLACED LOGIC
.
         move       C0 TO COUNT
         goto      readadj
         ELSE
.START PATCH 3.76 REMOVED LOGIC
.         splclose
..START PATCH 3.63 REPLACED LOGIC
..         SPLOPEN   "\\nts0\d\DATA\HOTLOADJ.LST"
.         PACK      STR35,NTWKPATH1,"HOTLOADJ.LST"
.         SPLOPEN   STR35
..END PATCH 3.63 REPLACED LOGIC
.         PRINT     hpreset,hpport:
.                   033,"&l66P":               page length
.                   033,"&l65F":               number lines
.                   033,"&l1E",033,"&a0c0R"     top margin * print position
.END PATCH 3.76 REMOVED LOGIC
         move       C0 TO COUNT
         GOTO       PRTOWN
         ENDIF
................................................................
.begin patch 3.6
prtinvfrm
.//print horiz line top of page     .note pattern 2G-horiz line 1G  vert line
.//                         pattern   height in dots (length)   terminate in A for horiz. B for vert
.//                         /           /                  vert width in dots      *cxA would be hor width
.//                        /           /                  /         vert position in dots
.//                       /           /                  /         /
.//                 033,"*c2G",033,"*c5825.2000A",033,"*c3B",033,"*p63.40y2.50X":
.//                 033,"*c0P";                                            \
.                           \                                               Horiz pos in dots
.                            start printing in black
.
.START PATCH 3.76 REPLACED LOGIC
.         print   033,"&l1E",033,"&a0c0R":
.                   033,"*p880x75Y":
.                   033,"(8U",033,"(s1p18.00v0s+3b5T","Names":
.                   b2,033,"(8U",033,"(s1p18.00v1s-3b5T","in the News":
.                   033,"*p760x95Y",033,"*c900a02b0p":
.                   033,"*p880x135Y":
.               033,"(8U",033,"(s1p09.00v0s-2b5T","C  A  L  I  F  O  R  N  I  A        I  N  C .":
..begin patch 3.61
..               033,"*p659x189Y":
..               033,"(8U",033,"(s1p08.00v0s-2b5T","One Bush Street, San Francisco, CA 94104 ":
.               033,"*p822.75x189Y":
.               033,"(8U",033,"(s1p08.00v0s-2b5T","1300 Clay Street 11th Floor, Oakland, CA 94612-1429 ":
.               033,"*p927.5x229Y":
.               " 415-989-3350 ",bullet," Fax 415-433-7796":
..               bullet," 415-989-3350 ",bullet," Fax 415-433-7796":
..end patch 3.61
.               033,"*c2G",033,"*c225.0A",033,"*c3B",033,"*p1088.0x337.5Y",033,"*c0P":            .box top
.               033,"*c2G",033,"*c225.0A",033,"*c3B",033,"*p1088.0x412.5Y",033,"*c0P":            .box bottom
.               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p1088.0x337.5Y",033,"*c0P":            .box line left
.               033,"*c1G",033,"*c75.00B",033,"*c3A",033,"*p1313.0x337.5Y",033,"*c0P":            .box line right
.               033,"*p1117.5x395Y":
.               033,"(8U",033,"(s1p13.00v1s+3b5T","Invoice":
.               033,"*c2G",033,"*c2343.75A",033,"*c3B",033,"*p600.0y2.5X",033,"*c0P":            .horizontal lines
.               033,"*c2G",033,"*c2343.75A",033,"*c3B",033,"*p1144.0y2.5X",033,"*c0P":
.               033,"*c2G",033,"*c2343.75A",033,"*c3B",033,"*p1500.0y.5X",033,"*c0P":
.               033,"*c2G",033,"*c2343.75A",033,"*c3B",033,"*p3150.0y2.5X",033,"*c0P":
.               033,"*c1G",033,"*c506.25B",033,"*c3A",033,"*p600.0y2.5X",033,"*c0P":            .vert lines top left
.               033,"*c1G",033,"*c506.25B",033,"*c3A",033,"*p600.0y2346.25X",033,"*c0P":            .vert lines top right
.               033,"*c1G",033,"*c262.5B",033,"*c3A",033,"*p1144.0y2.5X",033,"*c0P":            .vert lines mid left
.               033,"*c1G",033,"*c262.5B",033,"*c3A",033,"*p1144.0y2346.25X",033,"*c0P":            .vert lines mid right
.               033,"*c1G",033,"*c1650.0B",033,"*c3A",033,"*p1500.0y2.5X",033,"*c0P":            .vert lines bot left
.               033,"*c1G",033,"*c1650.0B",033,"*c3A",033,"*p1500.0y2346.25X",033,"*c0P":            .vert lines bot right
.                033,"*p0x0Y",*l,*l:                                                                   .reset position for inv
.               "&l3E&f1S&f1X&f10X";
..                 033,"*c5825.2000H",033,"&a6956.40v2.50H",033,"*c0P":
..                 033,"*c1G",033,"*c6900.2000V",033,"*c3A",033,"&a63.40v2.50H":
..                 033,"*c0P",033,"*c240.00V",033,"&a63.40v5000.00H":
..                 033,"*c0P",033,"*c6900.2000V",033,"&a63.40v5750.00H":
..                 033,"*c0P",033,"&f1S",033,"&f0S",033,"*c2G",033,"*c5825.2000H",033,"*c3B",033,"&a303.40v2.50H":
..                 033,"*c0P",033,"&f1S",033,"&f0S",033,"*c2G",033,"*c1420.0000H",033,"*c3B",033,"&a543.40v2.50H":
..                 033,"*c0P",033,"&f1S",033,"&f0S",033,"*c2G",033,"*c1420.0000H",033,"*c3B",033,"&a783.40v2.50H":
...                 033,"*c0P",033,"&f1S",033,"&f0S",033,"*c2G",033,"*c1420.0000H",033,"*c3B",033,"&a1023.40v2.50H":
..                 033,"*c0P",033,"&f1S",033,"&f0S",033,"*c1G",033,"*c6660.2000V",033,"*c3A",033,"&a303.40v1425.00H":
...                 033,"*c0P":
..                 033,"*p775x2825Y":
..                    033,"(8U",033,"(s1p07.00v0s-2b5T":
..                  "Full payment is required on orders cancelled after the maildate. If exchange, status will remain as ordered.":
..                  033,"*p775x2850Y":
..                  "Orders cancelled by mailer prior to mail date are subject to a $50.00 processing fee.":
..                   033,"*p0x3000Y":
..                   033,"*p40x3000Y":
..               033,"*p0x0Y":
..               "&l3E&f1S&f1X&f10X",hpcour,hp10pt,hpfixed;
..................................................................
	if (FirstPage > 0)
		prtpage	Laser;*NewPage
	endif
.begin patch 4.1
.begin patch 4.0
	prtpage	Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon
.	IF	(OCompID = "P"and formflag = c1 or Ocompid2 = "P" and Formflag = c2)
.	call	debug
.	prtpage	Laser;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
.		*p=500:343,*font=font07,"1300 Clay St. 11th Floor":
.		*p=400:443,"Oakland, CA 94612-1429":
.		*p=335:543,"415-945-9450 ","·"," Fax 415-945-9451":
.		*p=335:643,"A Division of Names in the News"
.	Else
.	prtpage               Laser;*Pictrect=*off,*PICT=0:1000:375:3375:NINLogo
.	endif
.end patch 4.1
.                              *Pictrect=*off,*PICT=0:1000:375:3375:NINLogo:
.	prtpage	Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
.		*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo:
	PRtPage	Laser;*RECT=1115:1340:3750:4500:
		*p=3785:1125,*font=Font014BI,"Invoice":
		*p=500:1962,*font=Font08,*line=7580:1962:                      ;top Hori line
		*p=500:1962,*line=500:3462:                          ;left side (top)
		*p=7580:1962,*line=7580:3462:                        ;right side (top)
		*p=500:3522,*line=7580:3522:                      ;Middle Hori line
		*p=500:3522,*line=500:4522:                          ;left side (Middle)
		*p=7580:3522,*line=7580:4522:                        ;right side (Middle)
		*RECT=4582:10332:500:7580:                           ;bottom section drawn as rectangle
		*p5625:10150,*font=Font09i,"Member Direct Marketing Association"
.end patch 4.0
	prtpage	Laser;*p750:2212,*font=Font07Dot5I,"Date:":
		*p4250:2212,"Invoice##:":
		*p5750:2212,"Mailer's P.O.:":
		*p5750:2712,"NIN LR ##:":
		*p5750:3212,"Mail Date:":
		*p750:3800,"Mailer's Offer:":
		*p750:4050,"Key:":
		*p750:4300,"List:":
		*p750:4712,*font=Font09B,*ULOn,"Original Billing",*font=Font07Dot5I,*ULOff:
		*p750:4900,"Quantity Addressed:":
		*p=4250:4900,"$ Per M":
		*p=5750:4900,"Amount Due:":
		*p750:7500,*font=Font09B,*ULOn,"Adjustments to Billing",*font=Font07Dot5I,*ULOff:
		*font=Font07Dot5I
	add	C1,FirstPage
	return

.prtinvfrm2
..Client Version Info
.	prtpage	Laser;*p=3650:1625,*font=Font014b,"Client Copy":
.		*p750:2400,*font=Font07Dot5I,"Client##:":
.		*p515:10150,*font=Font09Bi,"Payment due upon receipt. Please return copy with payment.":
.		*p=5750:9916,*font=Font09B,"Total Due:"
.	return
.prtinvfrm3
..List Owner Version Info
.	Prtpage	Laser;*p=2750:1650,*font=Font07Dot5I,"Amount Due: ",ownocpy:
.		*p=5100:1800,ap1mask:
.		*p750:2400,*font=Font07Dot5I,"Owner##:":
.		*p750:3150,"Client:"
.	return
.END PATCH 3.76 REPLACED LOGIC
.end patch 3.6
.begin patch 4.1
PRtNINLogo
              prtpage               Laser;*Pictrect=*off,*PICT=0:1000:375:3375:NINLogo
	return
PRtPLILogo
	prtpage	Laser;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
		*p=500:343,*font=font07,"1300 Clay St. 11th Floor":
		*p=400:443,"Oakland, CA 94612-1429":
		*p=335:543,"415-945-9450 ","·"," Fax 415-945-9451":
		*p=335:643,"A Division of Names in the News"
	return

..prtmlrboxGui Routine Laser
prtmlrboxGui
	clear	str2
	pack	str2 from osales10,osales
	IF	(Ocompid = "P")
	call	PrtPLILOgo
	Elseif	(Ocompid2 = "P" & (str2 = "27" | str2 = "28"))
	call	PrtPLILOgo
	else
	call	PrtNINLOgo
	endif
	prtpage	Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                        *p=3650:1625,*font=Font014b,"Client Copy":
		*p750:2400,*font=Font07Dot5I,"Client##:":
		*p515:10150,*font=Font09Bi,"Payment due upon receipt. Please return copy with payment.":
		*p=5750:9916,*font=Font09B,"Total Due:"
               return
..prtownerboxGui Routine Laser
prtownerboxGui
	IF	(Ocompid = "P" & OCompid2  <> "N")
	call	PrtPLILOgo
	ElseIF	(Ocompid2 = "P")
	call	PrtPLILOgo
	else
	call	PrtNINLOgo
	endif
.               getitem	GreyFill,0,colornum
               prtpage         Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon:
                               *Fill=*On:
                               *Bgcolor=Colornum:
                              *RECT=1527:1902:2652:5652:
                              *Fill=*Off
               getitem	NoFill,0,colornum
               prtpage        Laser;*Bgcolor=Colornum:
		*p=2750:1650,*font=Font07Dot5I,"Amount Due: ",ownocpy:
		*p=5100:1800,ap1mask:
		*p750:2400,*font=Font07Dot5I,"Owner##:":
		*p750:3150,"Client:"
	REturn
.end patch 4.1
..............................................................................
.
         INCLUDE   NORDIO.inc
.START PATCH 3.71 REPLACED LOGIC
.         INCLUDE   NMLRIO.inc
.         include   nbrkio.inc
	INCLUDE	COMPIO.inc
	INCLUDE	CNTIO.inc
.END PATCH 3.71 REPLACED LOGIC
         INCLUDE   NBILIO.inc
         INCLUDE   NOWNIO.inc
        INCLUDE   NAcdio.inc
.begin patch 3.5
         include   nadjio.inc
         INCLUDE   NJSTIO.inc
.end patch 3.5
         INCLUDE   NPAYIO.INC
.begin patch 3.4
;begin patch 3.8
         include   	compute.inc
         INCLUDE   	ninvio.inc
         include	Ninvacdio.inc
;         INCLUDE   NINVIO.inc
;end patch 3.8
         include   nmrgio.inc
         include   nshpio.inc
.end patch 3.4
         include   ndatio.inc
         include   ndat3io.inc
.START PATCH 3.7 ADDED LOGIC
	INCLUDE	NSEL2IO.INC
.END PATCH 3.7 ADDED LOGIC
        INCLUDE   COMLOGIC.inc

