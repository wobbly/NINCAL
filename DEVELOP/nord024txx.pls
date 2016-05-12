..
.. PURPOSE - READS INPUT FILE (NINORD) AND SPOOLS
..           RECORDS FOR TRANSMISSION TO target anaylsys xML FORMAT.
..
PC       EQU       0
         INCLUDE    COMMON.inc
         INCLUDE   CONS.inc
.patch2.75
				include	compdd.inc
				include	cntdd.inc
.         INCLUDE   NMLRDD.inc
.patch2.75
..START PATCH 2.53 REPLACED LOGIC
..         INCLUDE   CONTACT1.inc
         INCLUDE   NCNTDD.inc
.END PATCH 2.53 REPLACED LOGIC
         INCLUDE   NORDDD.inc
         INCLUDE   NRTNDD.inc
         INCLUDE   NOWNDD.inc
         INCLUDE   NCRCDD.inc
         INCLUDE   NSPIDD.inc
         INCLUDE   MEDIA.inc
         INCLUDE   SHIPPING.inc
..START PATCH 2.4 - ADDED LOGIC
         INCLUDE   NOFRDD.INC
..END PATCH 2.4 - ADDED LOGIC
..START PATCH 2.5 - ADDED LOGIC
         INCLUDE   NSPEDD.INC
..END PATCH 2.5 - ADDED LOGIC
..START PATCH 2.5 - ADDED LOGIC
         INCLUDE   HP.INC
..END PATCH 2.5 - ADDED LOGIC
..START PATCH 2.56 ADDED LOGIC
        INCLUDE WINAPI.INC
..END PATCH 2.56 ADDED LOGIC
.START PATCH 2.76 REMOVED LOGIC
.;.START PATCH 2.62 ADDED LOGIC
.	INCLUDE	NFULDD.INC
.;.END PATCH 2.62 ADDED LOGIC
.END PATCH 2.76 REMOVED LOGIC
.START PATCH 2.74A ADDED LOGIC
	INCLUDE	NSEL2DD.INC
	INCLUDE	NSEL3DD.INC
	INCLUDE	NADDDD.INC
	INCLUDE	NSLTDD.INC
	INCLUDE	NREFDD.INC
	INCLUDE	NMODDD.INC
.END PATCH 2.74A ADDED LOGIC
..
Release	       Init	      "2.78"	       DMB            12OCT2006  Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.Release	       Init	      "2.77"	       DMB            15SEP2006  Changed directory where xml files are created.
.Release	       Init	      "2.76"	       DMS            22JUN2006  FULFILLMENT CONVERSION
.release  init      "2.74"        ASH   10JUL06  Adjusted logic for determining exchanges
.Release        Init           "2.73"           JD             08jul2003  write continuation lr info occode = 1
.
. OTHER  VARIABLES.
. ....................
.START PATCH 2.72 ADDED LOGIC
DimPtr	DIM	^
.END PATCH 2.72 ADDED LOGIC
SPCL     DIM       2               *SPECIAL INSTRUCTION KEY
.START PATCH 2.5 - VARS NOW OBSOLETE
.DESC0L1  DIM       47          *FREE FORM SPECIAL INSTRUCTION
.DESC0L2  DIM       47          *FREE FORM SPECIAL INSTRUCTION
.DESC991  DIM       47          *FREE FORM SPECIAL INSTRUCTION
.DESC992  DIM       47          *FREE FORM SPECIAL INSTRUCTION
.DESC981  DIM       47          *FREE FORM SPECIAL INSTRUCTION
.DESC982  DIM       47          *FREE FORM SPECIAL INSTRUCTION
.END PATCH 2.5 - VARS NOW OBSOLETE
SPCL1    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL2    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL3    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL4    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL5    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL6    DIM       2           *SPECIAL INSTRUCTION CODE
.
REVTXT   INIT      "Revised: "
CANTXT   INIT      "**CANCELLED** : "
BILDTXT  INIT      "**Billed Order**"
REVDATA  DIM       30
BILDDATA DIM       16
.
. FILES.
. ......
targORD   FILE               .TARG ORDER INPUT FILE.
.Start Patch #2.3 - increased file size
.Added 2 bytes to file
.LABEL    FILE      VAR=115,UNCOMP  *SPOOL FILE FOR LABELS.
.START PATCH 2.56 REPLACED LOGIC
.LABEL    FILE      VAR=137,UNCOMP  *SPOOL FILE FOR LABELS.
.END PATCH 2.56 REPLACED LOGIC
.End Patch #2.3 - increased file size
.START PATCH 2.5 - NOW READING FROM IO
.OUTSP    IFILE     KEYLEN=6,VAR=288
.END PATCH 2.5 - NOW READING FROM IO
BADORD   FILE
.Start Patch #2.2 - remmed and replaced, increased file size to reflect NINMLR expansion
.NAMFILE  FILE      FIX=163
.WEEKFILE FILE      FIX=122
.Start Patch #2.3 - increased file size
.NAMFILE  FILE      FIX=183
.WEEKFILE FILE      FIX=142
NAMFILE  FILE      FIX=185      .Added 2 bytes to file
WEEKFILE FILE      FIX=146      .Added 4 bytes to file
.Start Patch #2.3 - increased file size
.End Patch #2.2 - remmed and replaced, increased file size to reflect NINMLR expansion
.START PATCH 2.6 REPLACED LOGIC
.SAVEFILE FILE
SAVEFILE IFILE
.END PATCH 2.6 REPLACED LOGIC
.
tdmcstat dim       1
THREE    FORM      "3"
FOUR     FORM      "4"
FIVE     FORM      "5"
SIX      FORM      "6"
SEVEN    FORM      "7"
EIGHT    FORM      "8"
ANS      DIM       1
FILE     FORM      2         BRANCHING CONSTANT FOR I/O TRAPS
DATE     DIM       8         'MM/DD/YY'.
DAY      DIM       2
OFFEROUT DIM       11       FOR OUTPUT OF OFFER, SUPPRESSED IF NO OFFER SELCTD.
COUNT    FORM      5
SPCOUNT  FORM      5
ORIG099  DIM       1
ANY099   DIM       1
WORK06   DIM       6
WORK47   DIM       47
WK247    DIM       47
.begin patch 2.7
Nfield52 form      5.2
.begin patch 2.71
InstructionCounter            form           2
str100         Dim            100
.end patch 2.71
.end patch 2.7
NFIELD23 FORM      3.2                  (NUMERIC WORK FIELD)
NFIELD4  FORM      4
MLRKEY   DIM       7
.STATUS   DIM       15
.START PATCH 2.5 - INCREASED VAR
.V1       FORM      1
V1       FORM      2
.END PATCH 2.5 - INCREASED VAR
MAIL1    DIM       25                       INFOR-
MAIL2    DIM       25                       MATION
MAIL3    DIM       25                       READ
.MAILCC   DIM       17                       MAILER
EXCHANGE DIM       15         *USED FOR ORDER PRINT
TEST     DIM       15         *USED FOR ORDER PRINT
F3       DIM       3         *USED FOR ORDER PRINT
F2       DIM       2         *USED FOR ORDER PRINT
ENTIRE   DIM       1        *USED FOR ORDER PRINT
SAMPLE   DIM       26       *USED FOR ORDER PRINT
SPCL7    DIM       2         *ORDER FILLER
SPCL8    DIM       2         *
SPCL9    DIM       2         *
CORTN    DIM       3         *USED FOR ORDER PRINT
CONT     DIM       23        *USED FOR ORDER PRINT
CONT1    DIM       20        *USED FOR ORDER PRINT
.Start Patch #2.3 - increase vars
.CONTDTE  DIM       8         *USED FOR ORDER PRINT
.CONTQTY  DIM       9         *USED FOR ORDER PRINT
.QTYMSK   INIT      "Z,ZZ9,999"    *USED FOR ORDER PRINT
.QTYOUT   DIM       9         *USED FOR ORDER PRINT
.QTYNUM   FORM      7         *USED FOR ORDER PRINT, QTY FORMATING.
.
CONTDTE  DIM       10        *USED FOR ORDER PRINT
CONTQTY  DIM       11        *USED FOR ORDER PRINT
QTYMSK   INIT      "ZZZ,ZZ9,999"    *USED FOR ORDER PRINT
QTYMSK2  INIT      "Z,ZZ9,999"    *USED FOR ORDER PRINT
QTYOUT   DIM       11        *USED FOR ORDER PRINT
QTYNUM   FORM      9         *USED FOR ORDER PRINT, QTY FORMATING.
QTYNUM2  FORM      7         *USED FOR ORDER PRINT, QTY FORMATING.
.end Patch #2.3 - increase vars
qtyout2  dim       9         * just for tdmc lolfile
MEDMEMO  DIM       25        *USED FOR ORDER PRINT, ON MAG TAPE.
COMSLCT  DIM       25        *USED FOR ORDER PRINT, COMSELECT ORDERS.
REPRT    DIM       15        *USED FOR ORDER PRINT, REPRINTED ORDERS.
.                            *AND CANCELLED ORDERS, REPRINT IMPLIED.
PRTPO    DIM       7         *USED FOR ORDER LABEL PRINT
LBOFR    DIM       25        *USED FOR ORDER LABEL PRINT.
LROUT    DIM       6         *USED FOR ORDER & LABEL PRINT.
LRMASK   INIT      "ZZZZZ9"
LRNUM    FORM      6
COUNTR   FORM      4
BUFFER   FORM      "4"
RTNSTRNG DIM       6
RTNCHEK  FORM      4
PRICECK  DIM       5
DESC     DIM       12
DESC2     DIM      3
TDMCSW   INIT      "N"
MEDTYPE  DIM        1
save     dim        47
prtlines form       2
c33      form       "33"
c66      form       "66"
lastlr   dim        6
lastlabl dim        6
salenumb dim        2
fullCNT      DIM       34
cnt          dim       20
.START PATCH 2.53 REMOVED LOGIC
.cntphone     dim       14
.END PATCH 2.53 REMOVED LOGIC
.START PATCH 2.61 REPLACED LOGIC
.intrnet  dim        24                .print contact's internet address
intrnet  dim        46                .print contact's internet address
.END PATCH 2.61 REPLACED LOGIC
BEGIN    FORM      2
LAST     FORM      2
loltype  dim       1
loldes   form      1
lolcodes init       "DXLRQ"
.START PATCH 2.56 REPLACED LOGIC
.dim25b   dim       25
dim45b   dim       45
.END PATCH 2.56 REPLACED LOGIC
compm    dim       25
.START PATCH 2.5 - ADDED VARS
holdstr  dim      758
.START PATCH 2.7 ADDED LOGIC
line     dim      55
.END PATCH 2.7 ADDED LOGIC
line1    dim      55
line2    dim      55
line3    dim      55
line4    dim      55
line5    dim      55
line6    dim      55
line7    dim      55
line8    dim      55
line9    dim      55
line10   dim      55
line11   dim      55
line12   dim      55
line13   dim      55
line14   dim      55
line15   dim      55
CARR     INIT     0x7f
carrfill dim      2
.END PATCH 2.5 - ADDED VARS
.START PATCH 2.56 - ADDED VARS
font2   font
columnA form    9
columnB form    9
columnC form    9
row1    form    9
row2    form    9
.END PATCH 2.56 - ADDED VARS
.begin patch 2.7
XMLFile        File
//.Patch 2.77
.XMLFileName    Dim            45
XMLFileName        Dim             255
XMLRealFileName    Dim             255
//.Patch 2.77
.end patch 2.7
.START PATCH 2.74A - ADDED LOGIC
PackData DataList
.END PATCH 2.74A - ADDED LOGIC
.START PATCH 09/22/2004 ADDED LOGIC
startfp	form	9
endfp	form	9
leftparan init	"("
str100a	Dim	100

.Patch 2.77 Var for longer file names,path
Taskname1 dim 255


.Patch 2.77
.END PATCH 09/22/2004 ADDED LOGIC
. .............................................................................
* PROGRAM MAIN.
* *************
.
. OPEN FILES.
. ...........
         MOVE      "NORD00??" TO PROGRAM
         MOVE      "Names in the News Ca" TO COMPNME
         MOVE      "Target SPOOL PROGRAM" TO STITLE
         CALL       PAINT
.START PATCH 2.56 ADDED LOGIC
         create    font2,"Arial",size=9
         MOVE      C0,HowMany
.         MOVE      "500",column1
.         MOVE      "1700",column2
.         MOVE      "1300",column3
.         MOVE      "4200",column4
.         MOVE      "5400",column5
.         MOVE      "5000",column6
.
         MOVE      "750",column1
         MOVE      "1950",column2
         MOVE      "1550",column3
         MOVE      "4500",column4
         MOVE      "5700",column5
         MOVE      "5300",column6
         MOVE      "650",row1
         MOVE      "650",row2
.END PATCH 2.56 ADDED LOGIC
.START PATCH 2.64 ADDED LOGIC
	call	GetWinVer
.               Goto testXml
..END PATCH 2.64 ADDED LOGIC
         TRAP      IO IF IO
         DISPLAY   *P1:24,"OPENING FILES"
         MOVE      C1 TO FILE
         IFNZ      PC
         OPEN      targORD,"\\nts1\e\data\diskin53.dat",EXCLUSIVE
.START PATCH 2.56 REPLACED LOGIC
.         PREPARE   LABEL,"TRIPLEX1/OUT:PRINT",PREPARE
.START PATCH 2.64 ADDED LOGIC
...         OPEN      SAVEFILE,"PIDIORDS"
         XIF
         IFZ      PC
         OPEN      targORD,"\\nts1\e\data\diskin53.dat",EXCLUSIVE
...         OPEN      SAVEFILE,"PIDIORDS"
         ADD       C1 TO FILE
.begin patch 2.7
.START PATCH 2.74A ADDED LOGIC
.Create work var
	create	PackData=1:1:1:1
.END PATCH 2.74A ADDED LOGIC

.	call	CreateXMLFile
.	call	CleanFaxfile
         ADD       C1 TO FILE
CLOCK    CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      DATE FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         XIF
	move	DATE,str6
...         WRITE     SAVEFILE,str6;DATE,"***********************************"
.END PATCH 2.6 REPLACED LOGIC
. ....................................................
. DISS - DISPLAY PROGRAM NAME.
. ............................
DISS     MOVE      "NORD0024T" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "TARGET SPOOL PROGRAM" TO STITLE
         CALL       PAINT
         MOVE       "ABORT" TO PF5
         CALL       FUNCDISP
         TRAP       ABORT IF F5
.
+ *****************************************************************************
. ORD - ORDER SECTION.
. ....................
ORD
READO    MOVE      SEVEN TO FILE
         filepi    1;targord
         READ      targORD,SEQ;ordvars
         GOTO      EOJ IF OVER
         ADD       C1 TO COUNTR
         DISPLAY   *P10:12,*EL,"COUNT READ ",COUNTR,b1,olrn
.			match     "0031" to nfulnum
.			goto      reado if not equal
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      reado IF EQUAL     YES, Do not write, leave in file.
.note cancodes also updated to skip cancelled pending orders.
.end patch 2.1
         reset     cancodes
.bandage DLH ASH 14DEC99   until cancodes bug resolved
         cmatch    "z" to ostat      .cancelled LCR skip
         goto      reado if equal
.End bandage DLH ASH 14DEC99   until cancodes bug resolved
         scan      ostat in cancodes
         goto      reado  if equal
         cmatch    "R" in ostat
         goto      reado if equal
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 2.4 - NEW LOGIC
.START PATCH 2.74A ADDED LOGIC
	packkey	NSEL2FLD,"1",OLRN
	move	"NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	O2DES,NSEL2NAME
		unpack	OPPM,str3,str2
		pack	str6,str3,".",str2
		rep	zfill,str6
		move	str6,NSEL2PRICE
		move	"/M",NMODDESC
	else
		pack	NMODFLD,NSEL2DESC
		rep	zfill,NMODFLD
		move	"NMODKEY",Location
		pack	KeyLocation,"Key: ",NMODFLD
		call	NMODKEY
		if over
			move	"/M",NMODDESC
		endif
	endif
.END PATCH 2.74A ADDED LOGIC
         pack      salenumb from osales10,osales
         CMATCH    " " TO OLRN
         GOTO      READO IF EOS
         GOTO      READO IF EQUAL
         MOVE      C0 TO QTYNUM
         MOVE      C0 TO QTYNUM2
         MOVE      OQTY TO QTYNUM
         MOVE      OQTY TO QTYNUM2
         COMPARE   C0 TO QTYNUM
         GOTO      READO IF EQUAL
.START PATCH 2.51 - ADDED LOGIC
         match     "0001",ortnnum
         goto      reado if equal
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
         PACK      QTYOUT FROM QTYMSK
         EDIT      QTYNUM TO QTYOUT
         PACK      QTYOUT2 FROM QTYMSK2
         EDIT      QTYNUM2 TO QTYOUT2
.         UNPACK    OPPM INTO F3,F2
         ADD       C1 TO COUNT
         move      OLRN,NSPEFLD
         move      "READO-NSPEKEY",Location
         call      NSPEKEY
.END PATCH 2.5 - NOW READING FROM IO
.
. PRINT VARIABLES FROM ACCESSED RECORD
.
.
         MOVE      " " TO EXCHANGE
         MOVE      " " TO ENTIRE
         MOVE      " " TO TEST
         MOVE      " " TO CONT
         MOVE      " " TO CONT1
         MOVE      " " TO REPRT
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
         CALL      NOMLR IF OVER
         MOVE      OLON TO NOWNFLD
         MOVE      "10" TO FILE
         REP       " 0" IN NOWNFLD
         CALL      NOWNKEY
.START PATCH 2.62 REPLACED LOGIC
.         SCAN      "TDMC" IN OWNCTN
.         GOTO      NOTDMC IF NOT EQUAL

checkful
.Start Patch 1.34 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
.     call	Trim using OWNCTN
     call	Trim using OFULLFIL
.End Patch 1.34 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	     
.START PATCH 2.76 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNctn
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"READO-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.		match "0031" to nfulnum
.		goto  good1 if equal
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.   	endif
.Start Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
.	if (OWNCTN <> "")
.		pack	COMPFLD6,OWNctn
.		rep	zfill,COMPFLD6
.		move	C1,COMPPATH
.		move	"READO-COMPKEY6",Location
.		pack	KeyLocation,COMPFLD6
.		call	COMPKEY6
.		if over
.			clear	COMPFLD6
.			clear	COMPCOMP
.		else
.			if (COMPSVBFLG = "T")
.				match "0031" to COMPFLD6
.				goto  good1 if equal
.			else
.				clear	COMPFLD6
.				clear	COMPCOMP
.			endif
.
.		endif
.
.	else
.		clear	COMPFLD6
.		clear	COMPCOMP
.   	endif
.End Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	   	
.Start Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
	if (OFULLFIL <> "")
		pack	COMPFLD,OFULLFIL
		call 	zfillit using COMPFLD
		move	C1,COMPPATH
		move	"READO-COMPKEY",Location
		pack	KeyLocation,COMPFLD
		call	COMPKEY
		if over
			clear	COMPFLD
			clear	COMPCOMP
		else
				match "009411" to COMPFLD
				goto  good1 if equal
		endif

	else
		clear	COMPFLD
		clear	COMPCOMP
   	endif
.End Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	   	
.END PATCH 2.76 REPLACED LOGIC
      goto  reado
.END PATCH 2.62 REPLACED LOGIC
.89AUG28 - IF RNT=0, THEN REUSE OF LR AND NOT TC'D.
good1
         CLEAR     REVDATA
         MOVE      OLRN TO NCRCFLD
         CALL      NCRCKEY
         IF        NOT OVER
NCRCLOOP CMATCH    "C" TO NCRCCODE
         IF        EQUAL
         PACK      REVDATA FROM CANTXT,NCRCMM,SLASH,NCRCDD,SLASH,ncrccc,NCRCYY
         ELSE
         PACK      REVDATA FROM REVTXT,NCRCMM,SLASH,NCRCDD,SLASH,ncrccc,NCRCYY
         ENDIF
         CALL      NCRCKS
         GOTO      NCRCEXIT IF OVER
         MATCH     NCRCFLD TO NCRCKEY
         GOTO      NCRCEXIT IF NOT EQUAL
         GOTO      NCRCLOOP
         ENDIF
NCRCEXIT MOVE      "0" TO NFIELD4
         MOVE      ORTNNUM TO NFIELD4
         BRANCH    NFIELD4 OF NOTDMC
         ADD       C1 TO SPCOUNT
        DISPLAY   *P14:20,*EL,"Target ORDERS SPOOLED : ",SPCOUNT
         compare   c1 to spcount
         if        not equal
         call      firstord
         endif
         move      c0 to prtlines
         move      olrn to lastlr
.         unpack   oppm into f3,f2
         CMATCH    " " TO OFOCODE
         GOTO      MEDIAEX IF EQUAL          *NO MEDIA SELECT
         GOTO      MEDIAEX IF EOS            * NO MEDIA SELECT
         MOVE      C0 TO NFIELD23
         TYPE      OFOCODE
         MOVE      OFOCODE TO SAVE         *SAVE VARIABLE
         GOTO      MED10 IF NOT EQUAL
         MOVE      OFOCODE TO NFIELD23
         GOTO      DIS27
MED10    REP       "A0B1C2D3E4F5G6H7I8J9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED20 IF NOT EQUAL
         MOVE      OFOCODE TO NFIELD23
         ADD       C10 TO NFIELD23
         GOTO      DIS27
MED20    REP       "K0L1M2N3O4P5Q6R7S8T9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED30 IF NOT EQUAL
         MOVE      OFOCODE TO NFIELD23
         ADD       "20" TO NFIELD23
         GOTO      DIS27
MED30    REP       "U0V1X2Y3Z4" IN OFOCODE
         MOVE      OFOCODE TO NFIELD23
         ADD       "30" TO NFIELD23
DIS27    MOVE      MED0 TO MEDIA
         LOAD      MEDIA FROM NFIELD23 OF MED1,MED2,MED3,MED4,MED5:
                   MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
                   MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
                   MED23,MED24,MED25,MED26,MED27,MED28,MED29
         CLEAR     MEDTYPE
         MOVE      YES TO MEDTYPE
         LOAD      MEDTYPE FROM NFIELD23 OF NO,NO,NO,NO,NO,NO,YES,YES,NO:
                   YES,YES,YES,YES,YES,YES,YES,YES,NO,YES,NO,NO,NO,YES,NO,NO,NO,NO,NO
.END PATCH 2.55 REPLACED LOGIC
MEDIAEX  CLEAR     SAMPLE
         CALL      SAMPLE
         CLEAR     RTCNTCT
         CLEAR     COMSLCT
         CLEAR     RTCOMP
.START PATCH 2.56 REPLACED LOGIC
.         clear     str25
.         clear     dim25b
         clear     str45
         clear     dim45b
.END PATCH 2.56 REPLACED LOGIC
         CLEAR     RTCITY
         CLEAR     RTSTATE
         CLEAR     RTZIP
         CLEAR     CORTN
         CLEAR     CONTDTE
         CLEAR     CONT
         CLEAR     CONTQTY
.         CLEAR     MEDMEMO
         MATCH     "C" TO OCOMSLCT            *COMSELECT ORDER?
         CALL      COMSLCT IF EQUAL           *YES
         MATCH     "L" TO OCOMSLCT            *LIFESTYLE OVERLAY?
         CALL      LIFESTYL IF EQUAL          *YES
         MATCH     "I" TO OCOMSLCT            *IC SYSTEMS OVERLAY?
         CALL      ICSYSTEM IF EQUAL          *YES
         PACK      QTYOUT FROM QTYMSK
         MOVE      OQTY TO QTYNUM
         EDIT      QTYNUM TO QTYOUT
         PACK      QTYOUT2 FROM QTYMSK2
         move      oqty to qtynum2
         EDIT      QTYNUM2 TO QTYOUT2
         MATCH     "1" TO OCCODE
         CALL      CONTIN IF EQUAL            *CONTINUATION ORDER.
         MATCH     "2" TO OCCODE
         CALL      CONTIN1 IF EQUAL           *CONTINUATION ORDER/NO OMIT.
         BUMP      OODNUM BY 4
         MOVE      OODNUM TO OFFEROUT
         MOVE      ORTNNUM TO NRTNFLD
         CALL      NRTNKEY
         MATCH     "2531" TO ORTNNUM
         IF        NOT EQUAL
         MATCH     RTCOMP TO MCOMP
         CALL      CHNGRET IF NOT EQUAL
         ENDIF
.
.ON THESE MAILERS THE OFFER DESC. MUST BE USED ON THE RETURN-TO CONTACT
.LINE    677-CMS, 210-COPLON, 53-ANACAPA, 702-MAZEL, 965-ORAM.
.
         MATCH     "0677" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         MATCH     "0210" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         MATCH     "0053" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         MATCH     "0702" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         MATCH     "0965" TO OMLRNUM       *USE MLR.OFR DESC ON RET-TO?
         CALL      USEOFR IF EQUAL         *YES.
         CMATCH    "R" TO ANS               *REPRINT ?
         CALL      REPRT IF EQUAL             *YES.
         CMATCH    "X" TO OSTAT               *CANCELLED ORDER?
         CALL      CANCLLED IF EQUAL
TEST     MOVE      "0" TO NFIELD23           *CLEAR FIELD
         MOVE      OTOCODE TO NFIELD23
.START PATCH 2.57 - REPLACED LOGIC
.         BRANCH    NFIELD23 TO TESTYES
         BRANCH    NFIELD23 TO TESTYES,TESTYES
.END PATCH 2.57 - REPLACED LOGIC
         GOTO      EXCHANGE
TESTYES  MOVE      "X" TO TEST
EXCHANGE
.START PATCH 2.58 - ADDED LOGIC
	 MOVE	   C0,NFIELD23
.END PATCH 2.58 - ADDED LOGIC
	 MOVE      OELCODE TO NFIELD23
         BRANCH    NFIELD23 TO ENTRENT,EXCHANG1,ENTIRE
         GOTO      OPRINT1            *NO CODE!!!!
SAMPLE   move     C0,nfield23
         MOVE      OSCODE TO NFIELD23
         BRANCH    NFIELD23 OF SAM1,SAM2,SAM3
         MOVE      "  " TO SAMPLE
         RETURN
SAM1     MOVE      "SAMPLE ENCLOSED" TO SAMPLE
         RETURN
SAM2     MOVE      "SAMPLE TO FOLLOW" TO SAMPLE
         RETURN
SAM3     MOVE      "SAMPLE PREVIOUSLY CLEARED" TO SAMPLE
         RETURN
ENTRENT  MOVE      "X" TO ENTIRE
         MOVE      "        " TO EXCHANGE
         GOTO      OPRINT1
.EXCHANG1 TYPE      OEXQTY
.         GOTO      OPRINT1 IF NOT EOS
EXCHANG1
.Start Patch #2.3 - INCREASE VAR
.         MATCH     "       " TO OEXQTY
         MATCH     "         " TO OEXQTY
.END Patch #2.3 - INCREASE VAR
         GOTO      OPRINT1 IF NOT EQUAL
         MOVE      "EXCHANGE" TO EXCHANGE
         GOTO      OPRINT1
.ENTIRE   TYPE      OEXQTY
.         GOTO      OPRINT1 IF NOT EOS
ENTIRE
.Start Patch #2.3 - INCREASE VAR
.         MATCH     "       " TO OEXQTY
         MATCH     "         " TO OEXQTY
.END Patch #2.3 - INCREASE VAR
         GOTO      OPRINT1 IF NOT EQUAL
         MOVE      "EXCHANGE" TO EXCHANGE
         MOVE      "X" TO ENTIRE
OPRINT1
         PACK      LROUT FROM LRMASK
         MOVE      OLRN TO LRNUM
         EDIT      LRNUM TO LROUT
         move       c0 to n2
         move       onetper to n2
         compare    c0 to n2
         MATCH     "         " TO OEXQTY
.END Patch #2.3 - INCREASE VAR
         GOTO      OPRINT2 IF EOS
         GOTO      OPRINT2 IF EQUAL
.START PATCH 2.56 - REPLACED LOGIC
.         PRINT     *27,"SEE BELOW";
.         PRINT     hpt300,"SEE BELOW";
.END PATCH 2.56 - REPLACED LOGIC
         GOTO      OPRINT3
OPRINT2  MATCH     "EXCHANGE" TO EXCHANGE
         GOTO      REALPPM IF NOT EQUAL
         PACK      PRICECK FROM F3,PERIOD,F2
         MATCH     "   .00" TO PRICECK
         GOTO      REALPPM IF NOT EQUAL
         GOTO      OPRINT3
REALPPM
OPRINT3
         MATCH     "  " TO OSHP
.END Patch #2.3 - INCREASED VAR
         GOTO      NOSHIP IF EQUAL
         GOTO      NOSHIP IF EOS
         move      C0 to nfield23
         MOVE      OSHP TO NFIELD23
         MOVE      SHIP0 TO SHIPdesc
         LOAD      SHIPdesc FROM NFIELD23 OF SHIP1,SHIP2,SHIP3,SHIP4,SHIP5:
                   SHIP6,SHIP7,SHIP8,SHIP9
         GOTO      OPRINT4
NOSHIP   CLEAR     SHIPdesc
OPRINT4
DISREGO
         MOVE      OLRN TO NSPEFLD
         REP       ZFILL,NSPEFLD
         MOVE      "DISREGO-NSPEKEY",Location
         CALL      NSPEKEY
         MOVE      OLRN TO NSPEFLD
         REP       ZFILL,NSPEFLD
         MOVE      "DISREGO-NSPEKEY",Location
         CALL      NSPEKEY
         call      TRIM using DESC002
         call      PARSITUP using line1,DESC002,C1
         call      PARSITUP using line2,DESC002,C1
         call      PARSITUP using line3,DESC002,C1
         call      PARSITUP using line4,DESC002,C1
         call      PARSITUP using line5,DESC002,C1
         call      PARSITUP using line6,DESC002,C1
         call      PARSITUP using line7,DESC002,C1
         call      PARSITUP using line8,DESC002,C1
         call      PARSITUP using line9,DESC002,C1
         call      PARSITUP using line10,DESC002,C1
         call      PARSITUP using line11,DESC002,C1
         call      PARSITUP using line12,DESC002,C1
         call      PARSITUP using line13,DESC002,C1
         call      PARSITUP using line14,DESC002,C1
.....Following logic removed as decision was made to print DESC001 (XSTAT) in last two lines....
.         call      PARSITUP using line15,DESC002,C1
         MOVE      C0 TO V1
.START PATCH 2.7 ADDED LOGIC
         MOVE      line1,line
.END PATCH 2.7 ADDED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO                           SPEC INSTRUC ROUTINE
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line2,line1
         MOVE      line2,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line3,line1
         MOVE      line3,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line4,line1
         MOVE      line4,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line5,line1
         MOVE      line5,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line6,line1
         MOVE      line6,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line7,line1
         MOVE      line7,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line8,line1
         MOVE      line8,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line9,line1
         MOVE      line9,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line10,line1
         MOVE      line10,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line11,line1
         MOVE      line11,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
.START PATCH 2.7 REPLACED LOGIC
.         MOVE      line12,line1
         MOVE      line12,line
.END PATCH 2.7 REPLACED LOGIC
         ADD       C1,V1
         CALL      SPCLNSTO
         GOTO      TYPIST
SPCLNSTO
         return
REUSE
         RETURN
TYPIST
        clear   intrnet
        pack    NCNTFLD,OCOCODE
        move    "NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
        call    NCNTKEY
        if not over
                scan    "BILLING",CNTNAME
                goto cntexit if equal
		move	CNTNAME,str35
		call	RemoveChar using str35,B1
		pack	intrnet,str35,"@NINCAL.COM"
cntexit         reset   CNTNAME
        endif
REG
.PRINT     *N,*L,*L,hpt0125,ODOWJ,*F
writelol
...         WRITE     SAVEFILE,OLRN;OLRN,tdmcstat
.		call	LoadXMLFile
.END PATCH 2.72 REPALCED LOGIC
.               endif
              goto          doxml
PreReplacit
               Move           "&" to str1
               move           "&amp;" to str6                        ;must be 1st one
               call           ReplaceIt Using str100,str1,str6
               Move           "<" to str1
               move           "&lt;" to str6
               call           ReplaceIt Using str100,str1,str6
               Move           ">" to str1
               move           "&gt;" to str6
               call           ReplaceIt Using str100,str1,str6
               Move           "#"" to str1
               move           "&quot;" to str6
               call           ReplaceIt Using str100,str1,str6
               Move           "'" to str1
               move           "&apos;" to str6
               call           ReplaceIt Using str100,str1,str6
               Return

DOXML
		move	ORTNNUM,NRTNFLD
		move	"XML-NRTNKEY",Location
		pack	KeyLocation,"Key: ",NRTNFLD
		call	NRTNKEY
.
		clear	MEDIA
		cmatch	" ",OFOCODE
		goto	MEDIAEXXML IF EQUAL		*NO MEDIA SELECT
		goto	MEDIAEXXML IF EOS		* NO MEDIA SELECT
		move	C0,NFIELD23
		type	OFOCODE
		goto	MED10XML IF NOT EQUAL
		move	OFOCODE,NFIELD23
		goto	DIS27XML
MED10XML
		rep	"A0B1C2D3E4F5G6H7I8J9",OFOCODE
		type	OFOCODE
		goto	MED20XML IF NOT EQUAL
		move	OFOCODE,NFIELD23
		add	C10,NFIELD23
		goto	DIS27XML
MED20XML
		rep	"K0L1M2N3O4P5Q6R7S8T9",OFOCODE
		type	OFOCODE
		goto	MED30XML IF NOT EQUAL
		move	OFOCODE,NFIELD23
		add	"20",NFIELD23
		goto	DIS27XML
MED30XML
		rep	"U0V1X2Y3Z4",OFOCODE
		move	OFOCODE,NFIELD23
		add	"30",NFIELD23
DIS27XML
		move	MED0,MEDIA
		load	MEDIA,NFIELD23,MED1,MED2,MED3,MED4,MED5:
			MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
			MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
			MED23,MED24,MED25,MED26,MED27,MED28,MED29
MEDIAEXXML
		clear	SHIPDESC
		call	Trim using OSHP
		if (OSHP <> "")
			move	C0,nfield23
			move	OSHP,NFIELD23
			move	SHIP0,SHIPDESC
			load	SHIPDESC FROM NFIELD23 OF SHIP1,SHIP2,SHIP3,SHIP4,SHIP5:
				SHIP6,SHIP7,SHIP8,SHIP9
		endif
		clear	EXCHANGE
		move	C0,NFIELD23
		move	OELCODE,NFIELD23
		if (NFIELD23 = 2 | NFIELD23 = 3)
			move	"EXCHANGE",EXCHANGE
		endif
		pack	NCNTFLD,OCOCODE
		move	"XML-NCNTKEY",Location
		pack	KeyLocation,"Key: ",NCNTFLD
		call	NCNTKEY
		call	CreateXMLFile
		call	LoadXMLFile
.		call	CloseXMLFile
.	endif
      goto  reado
CreateXMLFile
	clock	timestamp,timestamp
	clear	str45
	unpack	timestamp,str8,str6
.	pack	str45,"c:\work\tar.",str8,".t",str6,".jdTst"
//Patch 2.77 Replaced Logic
.	pack	str45,"\\nts1\e\data\tar.",str8,".t",str6,".Pretouch"
	pack	Taskname,"\\nts1\e\STORAGE\EXPORT\009411\Orders\tar.",str8,".t",str6,".Pretouch"

.	prepare	XMLFile,str45
	prepare	XMLFile,taskname
//Patch 2.77 End		
	write	Xmlfile,seq;"<nin>"
	weof	xmlfile,seq
	close	xmlfile
//Patch 2.77 Replaced Logic		
.	clear	str45
	clear	XmlRealFileName
.	pack	str45,"\\nts1\e\data\tar.",str8,".t",str6,".xml"
	pack	XmlRealFileName,"\\nts1\e\STORAGE\EXPORT\009411\Orders\tar.",str8,".t",str6,".xml"
//Patch 2.77 Replaced Logic
.	pack	str45,"\\nts1\e\data\Tar.",str8,".t",str6,".tst"

	clear	XmLFIleName
//Patch 2.77 Replaced Logic	
.	pack	XmlFileName,"\\nts1\e\data\tar.",str8,".t",str6,"."
	pack	XmlFileName,"\\nts1\e\STORAGE\EXPORT\009411\Orders\tar.",str8,".t",str6,"."
//Patch 2.77 Replaced Logic	
.	pack	XmlFileName,"\\nts1\e\data\tar.",str8,".t",str6,"."
	prepare	XMLFile,XmlRealFileName
	write	Xmlfile,seq;"<nin>"
	return

LoadXMLFile
	write	Xmlfile,seq;"<lforder>"
	write	Xmlfile,seq;"<lrnum>",OLRN,"</lrnum>"
	write	Xmlfile,seq;"<orderdate>",OODTEC,OODTEY,OODTEM,OODTED,"</orderdate>"
	clear	str100
	move	MCOMP,str100
	call	PreReplacit
	write	Xmlfile,seq;"<mailername>",str100,"</mailername>"
	clear	str100
	move	O1DES,str100
	call	PreReplacit
	write	Xmlfile,seq;"<listname>",str100,"</listname>"
	clear	str100
.START PATCH 2.74A REPLACED LOGIC
.	move	O2DES,str100
	move	NSEL2NAME,str100
.END PATCH 2.74A REPLACED LOGIC
	call	PreReplacit
.START PATCH 9/22/2004 REPLACED LOGIC
.	write	Xmlfile,seq;"<selection>",str100,"</selection>"
	call	SelectRep
checkw
.	if (NSEL2PRICE <> C0)
.	unpack	NSEL2PRICE,str5,str3
.	call	FormatNumeric using str5,str6
.	pack	str9,str6,str3
.	endif
	write	Xmlfile,seq;"<selection>"
	write	Xmlfile,seq;"<type>"
	write	Xmlfile,seq;str100
	write	Xmlfile,seq;"</type>"
	write	Xmlfile,seq;"<months>"
	write	Xmlfile,seq;str25
	write	Xmlfile,seq;"</months>"
	write	Xmlfile,seq;"<amount>"
	write	Xmlfile,seq;NSEL2PRICE
.	write	Xmlfile,seq;b1,str9
	write	Xmlfile,seq;"</amount>"
	write	Xmlfile,seq;"</selection>"
.END PATCH 9/22/2004 REPLACED LOGIC
	write	Xmlfile,seq;"<qty>",OQTY,"</qty>"
	clear	str100
	move	OMLRKY,str100
	call	PreReplacit
	write	Xmlfile,seq;"<keycode>",str100,"</keycode>"
	if (MEDIA = "" | MEDIA = " ")
		move	"******",MEDIA
	endif
	clear	str100
	move	MEDIA,str100
	call	PreReplacit
	write	Xmlfile,seq;"<media>",str100,"</media>"
	if (ORTNNUM = "0001")
		write	Xmlfile,seq;"<shipcompany>","Reuse Order!!!","</shipcompany>"
	else
		clear	str100
		move  dim45b,str100
.		move	RTCNTCT,str100
		call	PreReplacit
		write	Xmlfile,seq;"<shipcontact>",str100,"</shipcontact>"
		clear	str100
		move	RTCOMP,str100
		call	PreReplacit
		write	Xmlfile,seq;"<shipcompany>",str100,"</shipcompany>"
		clear	str100
		move	RTADDR,str100
		call	PreReplacit
		write	Xmlfile,seq;"<shipaddr>",str100,"</shipaddr>"
		write	Xmlfile,seq;"<shipcity>",RTCITY,"</shipcity>"
		write	Xmlfile,seq;"<shipstate>",RTSTATE,"</shipstate>"
		write	Xmlfile,seq;"<shipzip>",RTZIP,"</shipzip>"
	endif
	write	Xmlfile,seq;"<maildate>",OMDTEC,OMDTEY,OMDTEM,OMDTED,"</maildate>"
	write	Xmlfile,seq;"<shipdate>",ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED,"</shipdate>"
	write	Xmlfile,seq;"<shipvia>",SHIPDESC,"</shipvia>"
	move	C0,nfield52
.START PATCH 2.74A REPLACED LOGIC
.	move	C0,nfield52
.	move	OPPM,nfield52
.	mult	".01",nfield52
.	write	Xmlfile,seq;"<oppm>",nfield52,"</oppm>"
	write	Xmlfile,seq;"<oppm>",NSEL2PRICE,"</oppm>"
	write	Xmlfile,seq;"<selprice>",NSEL2SPRICE,"</selprice>"
.END PATCH 2.74A REPLACED LOGIC
.START PATCH 2.74 REPLACED LOGIC
.	if (Exchange = "EXCHANGE")
	if (OELCODE = "2" | OELCODE = "3")
.END PATCH 2.74 REPLACED LOGIC
		write	Xmlfile,seq;"<exchange>","EXCHANGE","</exchange>"
	endif
.START PATCH 2.74A ADDED LOGIC
	write	Xmlfile,seq;"<specialselections>"
	PackData.GetCount giving N10
	if (N10 > C0)
		move	C0,N1
		for result,"1",N10
			getitem	PackData,result,NREFDESC
			move	NREFDESC,str100
			if (str100 <> "")
				call	PreReplacit
			endif
			write	Xmlfile,seq;str100
		repeat
	endif
	write	Xmlfile,seq;"</specialselections>"
.END PATCH 2.74A ADDED LOGIC
	write	Xmlfile,seq;"<specialinstructions>"
	for InstructionCOunter,"1","14"
		clear	str100
		load	str100,InstructionCounter,Line1,Line2,Line3,Line4,Line5,Line6,Line7:
			Line8,line9,line10,line11,line12,line13,line14
		if (str100 <> "")
			call	PreReplacit
		endif
		write	Xmlfile,seq;str100
	repeat
	write	Xmlfile,seq;"</specialinstructions>"
.	if (occode = "2")         continuation omit
.start patch 2.73
	if (occode = "1")         continuation omit
.end  patch 2.73
		write	Xmlfile,seq;"<opu>"
		write	Xmlfile,seq;"<opulr>",OLRNCO,"</opulr>"
		write	Xmlfile,seq;"<opudate>",OODTECOC,OODTECOY,OODTECOM,OODTECOD,"</opudate>"
		write	Xmlfile,seq;"<opuqty>",OQTYCO,"</opuqty>"
		write	Xmlfile,seq;"</opu>"
	endif
	write	Xmlfile,seq;"<contact>",CNTNAME,"</contact>"
	if (OELCODE = "1" | OELCODE = "3")
		write	Xmlfile,seq;"<entirelist>","TRUE"
		write	Xmlfile,seq;"</entirelist>"
	endif
endxml
	write	Xmlfile,seq;"</lforder>"
	write	Xmlfile,seq;"</nin>"
	weof	XMLFile,seq
	close	XMLfile
.Patch 2.77 Vars replaced for longer file names	
.	clear	str55
	clear	Taskname
.	pack	str55,XMLFileName,"Pretouch"
	pack	taskname,XMLFileName,"Pretouch"
	clear	str45
	clear	taskname1
.	pack	str45,XMLFileName,"Touch"
	pack	taskname1,XMLFileName,"Touch"
.Patch 2.77	
   pause     "3"
.Patch 2.77    Vars replaced for longer file names
.	rename	str55,str45
	rename	Taskname,Taskname1
	erase   Taskname1
.Patch 2.77	
	return

CloseXMLFile
.	write	Xmlfile,seq;"</nin>"
.	weof	XMLFile,seq
.	close	XMLfile
.	clear	str55
.	pack	str55,XMLFileName,"JDtst"
.	clear	str45
.	pack	str45,XMLFileName,"Tst"
.	rename	str55,str45
	return
.END PATCH 2.72 ADDED LOGIC - MOVED INTO SELF-CONTAINED ROUTINES

MERGY
PRNTLABL
NOPO
         RETURN
CHNGRET
         clear     dim45b
         move      mcomp  to dim45b
.END PATCH 2.56 REPLACED LOGIC
         MOVE      "C/O" TO CORTN
         RETURN
.
. USEOFR - PRINT OFFER DESC AS RETURN-TO CONTACT.
USEOFR
         clear      dim45b
         move       OFDESC to dim45b
.END PATCH 2.56 REPLACED LOGIC
         MOVE      "C/O" TO CORTN
         RETURN
.
. CONTIN - CONTINUATION ORDER, INCLUDE EXTRA INFORMATION.
CONTIN
.         DISPLAY   *P1:24,*EL,"CONTIN",*B;
         MOVE      "X" TO CONT
.Start Patch #2.3 - added century
.         PACK      CONTDTE FROM OODTECOM,SLASH,OODTECOD,SLASH,OODTECOY
         PACK      CONTDTE FROM OODTECOM,SLASH,OODTECOD,SLASH,OODTECOC,OODTECOY
.END Patch #2.3 - added century
         MOVE      QTYMSK TO CONTQTY
         MOVE      OQTYCO TO QTYNUM
         EDIT      QTYNUM TO CONTQTY
         RETURN
. CONTIN1 - CONTINUATION ORDER, NO OMIT.
CONTIN1
.         DISPLAY   *P1:24,*EL,"CONTIN NO OMIT",*B;
         MOVE      "X" TO CONT1
         RETURN
.
. REPRT - REPRINT ORDER, PRINT AT TOP.
REPRT
.         MOVE      "*** REPRINT ***" TO REPRT
         RETURN
.
. CANCLLED - CANCELLED ORDER, PRINT AT TOP.
CANCLLED
.         MOVE      "**CANCELLED**" TO REPRT
         RETURN
.
. MEDMEMO - MAG TAPE ORDER, INCLUDE ADDITION INFO.
.MEDMEMO  MOVE      "INCLUDE LAYOUT & DUMP." TO MEDMEMO
.MEDMEMO  MOVE      MED0 TO MEDIA
.         RETURN
.
.
. COMSLCT - COMSELECT ORDER.
COMSLCT  MOVE      "**CC: CONSUMER DIRECT" TO COMSLCT
         RETURN
LIFESTYL MOVE      "CC:LIFESTYLE SELECTOR" TO COMSLCT
         RETURN
ICSYSTEM  MOVE      "**CC: IC SYSTEMS **" TO COMSLCT
         RETURN
NOTDMC
.         WRITE     BADORD,SEQEOF;OLRN," ",DATE
         DISPLAY   *P1:24,*EL,*HON,"NON-TRIPLEX ORDER",*HOFF,*B,*B,*B,*B,*B;
         GOTO      READO
.
NOORD
         DISPLAY     *W2,*R,*P1:24,*EL,"****SPOOLING STOPED!!!!!!":
                   *P20:24,*R,*HON,*EL,"NO RECORD FOUND FOR LR##",OLRN
        KEYIN     *P78:24,*HOFF,STR1;
        GOTO      READO

FIRSTORD
.	call	CreateXMLFile
          return
.END PATCH 2.56 - REPLACED LOGIC
NOMLR
         MOVE      "NO SUCH MAILER" TO MCOMP
         RETURN
.
.patch2.75
				include	compio.inc
				include	cntio.inc
.         INCLUDE   NMLRIO.inc
.patch2.75
         INCLUDE   NOWNIO.inc
         INCLUDE   NORDIO.inc
         INCLUDE   NCRCIO.inc
         INCLUDE   NRTNIO.inc
         INCLUDE   NSPIIO.inc
.START PATCH 2.4 - ADDED LOGIC
         INCLUDE   NOFRIO.INC
.END PATCH 2.4 - ADDED LOGIC
.START PATCH 2.5 - ADDED LOGIC
         INCLUDE   NSPEIO.INC
.END PATCH 2.5 - ADDED LOGIC
         INCLUDE   COMLOGIC.inc
ABORT    DISPLAY   *P1:24,*HON,*BLINKON,*RED,"JOB ABORTED",*B,*B,*B,*W5,*B
EOJ
eoj4
         IFNZ      PC
.         FLUSH     TDMCORD
.         FLUSH     BADORD
.START PATCH 2.56 REMMED LOGIC
.         FLUSH     LABEL
.END PATCH 2.56 REMMED LOGIC
         FLUSH     NAMFILE
         XIF
         CLOSE     targord
.         WEOF      BADORD,SEQEOF
.         CLOSE     BADORD
.START PATCH 2.6 REPLACED LOGIC
.         WEOF      SAVEFILE,SEQEOF
.END PATCH 2.6 REPLACED LOGIC
...         CLOSE     SAVEFILE
         SPLCLOSE
         MOVE      "-1" TO SEQ
.START PATCH 2.56 REMMED LOGIC
.         WEOF      LABEL,SEQ
.END PATCH 2.56 REMMED LOGIC
.	call	CloseXMLFile
         STOP
IO
         TRAPCLR    IO
         NORETURN
         TRAP      IO IF IO
         BRANCH    FILE OF ONE,TWO,THREE,FOUR,FIVE:
                   SIX,SEVEN,EIGHT,NINE,TEN,TWELVE
ZERO
         DISPLAY   *P1:24,"UNDEFINED IO ERROR",*W2;
         GOTO      IOEXIT
ONE
         DISPLAY   *P1:24,"TDMCORD.OUT FILE ERROR",*W2;
         GOTO      IOEXIT
TWO
         DISPLAY   *P1:24,"TRIPLEX1.OUT FILE ERROR",*W2;
         GOTO      IOEXIT
THREE
         DISPLAY   *P1:24,"BADORD FILE ERROR",*W2;
         GOTO      IOEXIT
FOUR
         DISPLAY   *P1:24," FILE ERROR",*W2;
         GOTO      IOEXIT
FIVE
         DISPLAY   *P1:24," FILE ERROR",*W2;
         GOTO      IOEXIT
SIX
         DISPLAY   *P1:24," FILE ERROR",*W2;
         GOTO      IOEXIT
SEVEN
         GOTO      IOEXIT
EIGHT
         DISPLAY   *P1:24," LABEL SPOOL FILE ERROR",*W2;
         KEYIN     *R,*P1:24,"DO YOU WANT ME TO CREATE FILE ",ANS;
         CMATCH    "Y" TO ANS
         GOTO      PREPLABL IF EQUAL
         CMATCH    "N" TO ANS
         GOTO      EIGHT IF NOT EQUAL
         GOTO      IOEXIT
NINE
         DISPLAY   *P1:24,"BAD ORDER FILE ERROR",*W2;
         GOTO      IOEXIT
TEN      DISPLAY   *P1:24,*EL,"ORDER SPOOL FILE ERROR",*W2
         GOTO      IOEXIT
ELEVEN   DISPLAY   *P1:24,*EL,"ORDER FILE ERROR",*W2
         GOTO      IOEXIT
TWELVE   DISPLAY   *P1:24,*EL,"NINSPEC FILE ERROR",*W2
         GOTO      IOEXIT
IOEXIT
         KEYIN     *P60:24,*B,ANS;
         CMATCH    "Q" TO ANS
         GOTO      IOEXIT1 IF EQUAL
         BRANCH    FILE OF ONE,TWO,THREE,FOUR,FIVE,SIX,SEVEN,EIGHT
         GOTO      ZERO
IOEXIT1  TRAPCLR   IO
         SHUTDOWN  "ALERT"
PREPLABL
	call	CleanFaxfile
         GOTO      CLOCK
.START PATCH 2.64 ADDED LOGIC
CleanFaxfile
	return
.END PATCH 2.64 ADDED LOGIC

.START PATCH 09/22/2004 ADDED LOGIC
SelectRep
	pack	str25,B55
	scan	leftparan,str100
	if equal
		bump	str100
		movefptr str100,startfp
		move	C0,endfp
		loop
			move	str100,str1
			type	str1
			if not equal
.START PATCH 09/22/2004 REPLACED LOGIC
.				break
				if (str1 <> DASH)
					break
				endif
.END PATCH 09/22/2004 REPLACED LOGIC
			endif
			movefptr str100,endfp
			bump	str100
		repeat
		if (endfp >= startfp)
			reset	str100,startfp
			setlptr	str100,endfp
			move	str100,str25
			reset	str100
			setlptr	str100
			pack	str35,"(",str25
			call	ReplaceIt using str100,str35,leftparan
			pack	str100,str100,B55,B55
.
			move	str100,str100a
			rep	lowup,str100a


			move	"MOS",str6
			scan	str6,str100a
			if equal
				movefptr str100a,result
				add	C2,result,howmany
				reset	str100,result
				setlptr	str100,howmany
				move	str100,str6
				reset	str100
				setlptr	str100
				call	ReplaceIt using str100,str6,B1
				pack	str100,str100,B55,B55
			else
				move	"MONTHS",str6
				scan	str6,str100a
				if equal
					movefptr str100a,result
					add	C5,result,howmany
					reset	str100,result
					setlptr	str100,howmany
					move	str100,str6
					reset	str100
					setlptr	str100
					call	ReplaceIt using str100,str6,B1
					pack	str100,str100,B55,B55
				else
					move	"MONTH",str6
					scan	str6,str100a
					if equal
						movefptr str100a,result
						add	C4,result,howmany
						reset	str100,result
						setlptr	str100,howmany
						move	str100,str6
						reset	str100
						setlptr	str100
						call	ReplaceIt using str100,str6,B1
						pack	str100,str100,B55,B55
					endif
				endif
			endif
		endif
		reset	str100
		setlptr	str100
		move	"( ",str2
		call	ReplaceIt using str100,str2,leftparan
		pack	str100,str100,B55,B55
		move	"( ",str2
		call	ReplaceIt using str100,str2,leftparan
		pack	str100,str100,B55,B55
		move	"(/",str2
		call	ReplaceIt using str100,str2,leftparan
		pack	str100,str100,B55,B55
		move	"( ",str2
		call	ReplaceIt using str100,str2,leftparan
		pack	str100,str100,B55,B55
		move	"( ",str2
		call	ReplaceIt using str100,str2,leftparan
		pack	str100,str100,B55,B55
		move	"~",str1
		move	"()",str2
		call	ReplaceIt using str100,str2,B55
		pack	str100,str100,B55,B55
		move	"( )",str3
		call	ReplaceIt using str100,str3,B55
		pack	str100,str100,B55,B55
		reset	str100
		setlptr	str100
	endif
	return
.END PATCH 09/22/2004 ADDED LOGIC

.START PATCH 2.53 ADDED LOGIC
         INCLUDE   NCNTIO.inc
.END PATCH 2.53 ADDED LOGIC
.START PATCH 2.76 REMOVED LOGIC
.;START PATCH 2.62 ADDED LOGIC
.	INCLUDE	NFULIO.INC
.;END PATCH 2.62 ADDED LOGIC
.END PATCH 2.76 REMOVED LOGIC
.START PATCH 2.74A ADDED LOGIC
	INCLUDE	NSEL2IO.INC
	INCLUDE	NSEL3IO.INC
	INCLUDE	NADDIO.INC
	INCLUDE	NSLTIO.INC
	INCLUDE	NREFIO.INC
	INCLUDE	NMODIO.INC
.END PATCH 2.74A ADDED LOGIC
