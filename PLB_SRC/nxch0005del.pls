PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
;Patch1.9
			include compdd.inc
			include	cntdd.inc
;        INCLUDE   NMLRDD.inc
;patch1.9
.START PATCH 1.9.1 ADDED LOGIC
	INCLUDE	NXNGDD.INC
	INCLUDE	NXCHDD.INC
.END PATCH 1.9.1 ADDED LOGIC
.
. PROGRAM TO BE USED WITH TESTEXCH/DBC TO REPAIR FAULTY ACCOUNTS
. 08/03/82
RELEASE  INIT      "2.0"      DMB	18JUN2005	FM IP CHG
.RELEASE  INIT      "1.9.1"      ASH	10MAY2005	NINXNUM/NINXCHNG Conversion
.RELEASE  INIT      "1.9"      DMB	26MAY2004	Mailer Conversion
.RELEASE  INIT      "1.8"      ASH 11APR2001 FILE ADDED TO FILE MANAGER
.                               Get rid of dupes
.RELEASE  INIT      "1.7"      ASH 08JUN99 NINXCHNG Y2K FILE CONVERSION
.RELEASE  INIT      "1.6"      JD  12JAN99 added option to clear Inactive accounts
.RELEASE  INIT      "1.5"     DLH 13MAR92
.
. FILES
. ......
INPUT    FILE          ; USED TO ADD SORTED EXSTAT INTO FILES, AFTER DELETION
.                       OF OLD RECORDS.
.START PATCH 1.7 - REPLACED LOGIC
.XCHNG    IFILE     KEYLEN=13,FIXED=90
.XLRNG    IFILE     KEYLEN=6,FIXED=90
.START PATCH 1.9.1 REPLACED LOGIC
.XCHNG    IFILE     KEYLEN=13,FIXED=96
.XLRNG    IFILE     KEYLEN=6,FIXED=96
..END PATCH 1.7 - REPLACED LOGIC
.         IFNZ      PC
.ACCOUNTA AFILE     FIXED=22
.         XIF
.         IFZ      PC
.ACCOUNTA AFILE    14,2,,,FIXED=22
.         XIF
..
.
.MLR1     DIM       4     MAILER NUMBER ONE
.MLR2     DIM       4     MAILER NUMBER TWO
.ENTRY    FORM      5     ENTRY NUMBER
.FLAG     DIM       1     BLANK SPACE
.ACCKEY   DIM       8     KEY FOR NINXNUM FILE
.EXKEY    DIM       13    KEY FOR NINXCHNG FILE
.LRKEY    DIM       6     KEY FOR NINLRXNG IFILE.
.XCHCOMNT  DIM       30
.SAVAKEY  DIM       8     SAVE ACCKEY FOR COMPARE
.SAVEKEY  DIM       13    SAVE EXKEY FOR COMPARE
.BLANK13A DIM       13
...............................
XCHNG    IFILE     KEYLEN=17,FIXED=200
XLRNG    IFILE     KEYLEN=6,FIXED=200
ACCOUNTA AFILE     FIXED=30
ACCOUNTS FILE      FIXED=30
.
.
MLR1     DIM       6     MAILER NUMBER ONE
MLR2     DIM       6     MAILER NUMBER TWO
LRKEY    DIM       6     KEY FOR NINLRXNG IFILE.
SAVAKEY  DIM       12    SAVE ACCKEY FOR COMPARE
BLANK17A DIM       17
.END PATCH 1.9.1 REPLACED LOGIC
SAVENTRY FORM      5     SAVE ENTRY NUMBER FOR COMPARE
ANS      DIM       1
NEWFLAG DIM       1
TWO      FORM      "2"
.START PATCH 1.9.1 REPLACED LOGIC
.MLRSW    DIM       1
.MALER1   DIM       4
.MALER2   DIM       4
MALER1   DIM       6
MALER2   DIM       6
.END PATCH 1.9.1 REPLACED LOGIC
ANTRY    DIM       5
KEY      DIM       6              ISAM KEY LR NUMBER
.
.START PATCH 1.9.1 REPLACED LOGIC
.OLD      DIM       4     MAILER## TO BE CHANGED
.NEW      DIM       4     NEW MAILER #
.NEWKEY   DIM       8     NEW KEY FOR ACCOUNT FILE
OLD      DIM       6     MAILER## TO BE CHANGED
NEW      DIM       6     NEW MAILER #
NEWKEY   DIM       12    NEW KEY FOR ACCOUNT FILE
.END PATCH 1.9.1 REPLACED LOGIC
BLANK08  DIM       8
.START PATCH 1.9.1 REPLACED LOGIC
.BLANK08A DIM       8     FIELD FOR ACCKEY DURING UPDATE READ
.NEWXKEY  DIM       13    NEW KEY FOR XCHNG FILE
.LR       DIM       6     ORDER LR NUMBER
..START PATCH 1.7 - REPLACED LOGIC
..QTY      FORM      7     ORDER QUANTITY
..USAGE1   FORM      9     RUNNING BAL.
..USAGE2   FORM      9
..LIST     DIM       6      LIST NUMBER
..DAT      DIM       6      ORDER DATE
.QTY      FORM      9     ORDER QUANTITY
.USAGE1   FORM      10    RUNNING BAL.
.USAGE2   FORM      10
.LIST     DIM       6      LIST NUMBER
.DAT      DIM       8      ORDER DATE
..END PATCH 1.7 - REPLACED LOGIC
.STAT     DIM       1      ORDER STATUS (C) IF CANCELLED.
.TYPE     DIM       2      TYPIST INITIALS
NEWXKEY  DIM       17    NEW KEY FOR XCHNG FILE
.END PATCH 1.9.1 REPLACED LOGIC
COUNT    FORM      5      NUMBER OF DETAIL ENTRIES PROCESSED
NEWENTRY FORM      5      NEW COUNTER NUMBER
FILL5    DIM       5      FILLER
ONE      FORM      "1"
ZERO     FORM      "00000"
.START PATCH 1.7 - REPLACED LOGIC
.NUSAGE1  FORM      9      USED TO CALC NEW RUNNING BAL OPTION 2.
.NUSAGE2  FORM      9      USED TO CALC NEW RUNNING BAL OPTION 2.
NUSAGE1  FORM      10     USED TO CALC NEW RUNNING BAL OPTION 2.
NUSAGE2  FORM      10     USED TO CALC NEW RUNNING BAL OPTION 2.
.END PATCH 1.7 - REPLACED LOGIC
BRANCH   FORM      1      USED FOR BRANCH OPTION 2.
FILEIN   DIM       13     INPUT FILE NAME OPTION 2.
AKEY1    INIT      "01L"
AKEY2    INIT      "02L"
.START PATCH 1.9.1 REPLACED LOGIC
.ACKEY1   DIM       7
.ACKEY2   DIM       7
.DELKEY1  DIM       7
.DELKEY2  DIM       7
.HOLDXKEY DIM       13
ACKEY1   DIM       9
ACKEY2   DIM       9
DELKEY1  DIM       9
DELKEY2  DIM       9
HOLDXKEY DIM       17
.END PATCH 1.9.1 REPLACED LOGIC
HOLDLR   DIM       6
.EXKEY    DIM       13    KEY FOR NINXCHNG FILE
.LRKEY    DIM       6     KEY FOR NINLRXNG IFILE.
.XCHCOMNT  DIM       30
.START PATCH 1.8 GET RID OF DUPE VARIABLES
.SAVAKEY  DIM       8     SAVE ACCKEY FOR COMPARE
.SAVEKEY  DIM       13    SAVE EXKEY FOR COMPARE
.BLANK13A DIM       13
.SAVENTRY FORM      5     SAVE ENTRY NUMBER FOR COMPARE
.ANS      DIM       1
.NEWFLAG DIM       1
.TWO      FORM      "2"
.MLRSW    DIM       1
.MALER1   DIM       4
.MALER2   DIM       4
.ANTRY    DIM       5
.KEY      DIM       6              ISAM KEY LR NUMBER
..
.OLD      DIM       4     MAILER## TO BE CHANGED
.NEW      DIM       4     NEW MAILER #
.NEWKEY   DIM       8     NEW KEY FOR ACCOUNT FILE
.BLANK08  DIM       8
.BLANK08A DIM       8     FIELD FOR ACCKEY DURING UPDATE READ
.NEWXKEY  DIM       13    NEW KEY FOR XCHNG FILE
.LR       DIM       6     ORDER LR NUMBER
..START PATCH 1.7 - REPLACED LOGIC
..QTY      FORM      7     ORDER QUANTITY
..USAGE1   FORM      9     RUNNING BAL.
..USAGE2   FORM      9
..LIST     DIM       6      LIST NUMBER
..DAT      DIM       6      ORDER DATE
.QTY      FORM      9     ORDER QUANTITY
.USAGE1   FORM      10    RUNNING BAL.
.USAGE2   FORM      10
.LIST     DIM       6      LIST NUMBER
.DAT      DIM       8      ORDER DATE
..END PATCH 1.7 - REPLACED LOGIC
.STAT     DIM       1      ORDER STATUS (C) IF CANCELLED.
.TYPE     DIM       2      TYPIST INITIALS
.COUNT    FORM      5      NUMBER OF DETAIL ENTRIES PROCESSED
.NEWENTRY FORM      5      NEW COUNTER NUMBER
.FILL5    DIM       5      FILLER
.ONE      FORM      "1"
.ZERO     FORM      "00000"
..START PATCH 1.7 - REPLACED LOGIC
..NUSAGE1  FORM      9      USED TO CALC NEW RUNNING BAL OPTION 2.
..NUSAGE2  FORM      9      USED TO CALC NEW RUNNING BAL OPTION 2.
.NUSAGE1  FORM      10     USED TO CALC NEW RUNNING BAL OPTION 2.
.NUSAGE2  FORM      10     USED TO CALC NEW RUNNING BAL OPTION 2.
..END PATCH 1.7 - REPLACED LOGIC
.BRANCH   FORM      1      USED FOR BRANCH OPTION 2.
.FILEIN   DIM       13     INPUT FILE NAME OPTION 2.
.AKEY1    INIT      "01L"
.AKEY2    INIT      "02L"
.ACKEY1   DIM       7
.ACKEY2   DIM       7
.DELKEY1  DIM       7
.DELKEY2  DIM       7
.HOLDXKEY DIM       13
.HOLDLR   DIM       6
.END PATCH 1.8 GET RID OF DUPE VARIABLES
.
.
         TRAP      IO IF IO
.START PATCH 1.8 REPLACED LOGIC
.         OPEN      ACCOUNTA,"NINXNUM"
.         OPEN      XCHNG,"NINXCHNG"
.         OPEN      XLRNG,"NINLRXNG"
.>Patch 2.0
.         OPEN      ACCOUNTA,"NINXNUM.AAM|20.20.30.103:502"
         OPEN      ACCOUNTs,"NINXNUM.dat|10.10.30.103:502"
         OPEN      ACCOUNTA,"NINXNUM.AAM|10.10.30.103:502"
.         OPEN      XCHNG,"NINXCHNG.ISI|20.20.30.103:502"
         OPEN      XCHNG,"NINXCHNG.ISI|10.10.30.103:502"
.         OPEN      XLRNG,"NINLRXNG.ISI|20.20.30.103:502"
         OPEN      XLRNG,"NINLRXNG.ISI|10.10.30.103:502"
.>Patch 2.0
.END PATCH 1.8 REPLACED LOGIC
         TRAPCLR   IO
         TRAP      EXIT IF F3
         TRAP      ESC IF F5
         MOVE      "NXCH5Del" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "Del EXCHANGES" TO STITLE
         MOVE      "EXIT" TO PF3
         MOVE      "ABORT" TO PF5
         CALL      PAINT
         CALL      FUNCDISP
.
KMLR1
         MOVE      ZERO,MLR1
         MOVE      ZERO,MLR2
         MOVE      ZERO,OLD
         MOVE      ZERO,NEW
         DISPLAY   *P10:14,"Processing !!! Mailer # 2616 ":
                   *P13:17,"1)DELETE ALL RECORDS":
                   *P13:22,"2) STOP";
         KEYIN     *P13:23,"YOUR SELECTION:",*DE,N1;
         BRANCH    N1 OF KMLR1A,EXIT
         BEEP
         GOTO      KMLR1
KMLR1A
	Read	AccountS,seq;ACCKEY,BLANK08,ENTRY,FLAG
	goto	Exit if over
	unpack	Acckey into mlr1,mlr2
	If	(mlr1 = "002616" or mlr2 = "002616")
	call	Delete
	endif
	goto	Kmlr1a
....................................
.
DELETE
         MOVE      ZERO,COUNT
         PACK      ACKEY1 FROM AKEY1,MLR1
         PACK      ACKEY2 FROM AKEY2,MLR2
         FILEPI    3;ACCOUNTA
         READ      ACCOUNTA,ACKEY1,ACKEY2;;
         DELETE    ACCOUNTA
         MOVE      ENTRY,SAVENTRY
         CLEAR     EXKEY
         APPEND    ACCKEY,EXKEY
         APPEND    ZERO,EXKEY
         RESET     EXKEY
DEL1
         REP       " 0" IN EXKEY
         filepi    1;xchng
         READ      XCHNG,EXKEY;NXCHVARS
         GOTO      OVERIDE IF OVER
         MOVE      EXKEY TO HOLDXKEY
         filepi    1;XLRNG
         READ      XLRNG,LR;NXCHVARS
         MATCH     HOLDXKEY TO EXKEY
         GOTO      DEL1OK IF EQUAL
DEL1QUES
         filepi    1;xlrng
         READKS    XLRNG;EXKEY,LR2,HOLDLR,USAGE1,USAGE2:
                   QTYFILL,QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         GOTO      OVERIDE IF OVER
         MATCH     HOLDLR TO LR
         GOTO      OVERIDE IF NOT EQUAL
         MATCH     HOLDXKEY TO EXKEY
         GOTO      DEL1QUES IF NOT EQUAL
DEL1OK   FILEPI    2;XCHNG,XLRNG
         DELETE    XCHNG,EXKEY
         DELETEK   XLRNG,LR
         COMPARE   COUNT,SAVENTRY
         GOTO      DONE IF EQUAL
         GOTO      DONE IF LESS
DEL2     ADD       ONE,COUNT
         CLEAR     EXKEY
         APPEND    ACCKEY,EXKEY
         APPEND    COUNT,EXKEY
         RESET     EXKEY
         REP       " 0",EXKEY
         GOTO      DEL1
.
OVERIDE  DISPLAY   *P1:24,*EL,"NO RECORD FOUND!!!":
                   *R,*P1:24,"IF THERE IS A GAP IN THE ENTRY COUNT":
                   "  HIT 'F5' TO STOP, ENTRY = ",COUNT;
         GOTO      DEL2
ESC      TRAPCLR   F5
         GOTO      exit
.
DONE
         MOVE      ZERO TO COUNT
         DISPLAY   *P2:24,*EL,"One Acount Done!!! ",acckey,*B;
         GOTO      KMLR1A
IO
         DISPLAY   *P12:24,*EL,"FILE NOT FOUND";
         PAUSE     TWO
         BEEP
         STOP
TRYAGAIN
         KEYIN     *P2:24,*EL,"WANT TO TRY AGAIN ?",ANS;
         CMATCH    YES,ANS
         GOTO      KMLR1 IF EQUAL
Exit      STOP
			include compio.inc
			include	cntio.inc
;         INCLUDE   NMLRIO.inc
;Patch1.9
         INC       MLRHELP.inc
.START PATCH 1.9.1 ADDED LOGIC
	INCLUDE	NXNGIO.INC
	INCLUDE	NXCHIO.INC
.END PATCH 1.9.1 ADDED LOGIC
	INCLUDE   COMLOGIC.inc