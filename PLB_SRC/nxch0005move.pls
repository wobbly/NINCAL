PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
;Patch1.9
                              include compdd.inc
                              include   cntdd.inc
;        INCLUDE   NMLRDD.inc
;patch1.9
.START PATCH 1.9.1 ADDED LOGIC
          INCLUDE   NXNGDD.INC
          INCLUDE   NXCHDD.INC
.END PATCH 1.9.1 ADDED LOGIC
.
. PROGRAM TO BE USED WITH TESTEXCH/DBC TO REPAIR FAULTY ACCOUNTS
. 08/03/82
RELEASE  INIT      "2.0"      DMB       18JUN2005 FM IP CHG
.RELEASE  INIT      "1.9.1"      ASH    10MAY2005 NINXNUM/NINXCHNG Conversion
.RELEASE  INIT      "1.9"      DMB      26MAY2004 Mailer Conversion
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
OLD      DIM       6     MAILER## TO BE CHANGED
NEW      DIM       6     NEW MAILER #
NEWKEY   DIM       12    NEW KEY FOR ACCOUNT FILE
BLANK08  DIM       8
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
         OPEN      ACCOUNTA,"NINXNUM.AAM|10.10.30.103:502"
         OPEN      ACCOUNTs,"NINXNUM.dat|10.10.30.103:502"
.         OPEN      XCHNG,"NINXCHNG.ISI|20.20.30.103:502"
         OPEN      XCHNG,"NINXCHNG.ISI|10.10.30.103:502"
.         OPEN      XLRNG,"NINLRXNG.ISI|20.20.30.103:502"
         OPEN      XLRNG,"NINLRXNG.ISI|10.10.30.103:502"
.>Patch 2.0
.END PATCH 1.8 REPLACED LOGIC
         TRAPCLR   IO
         TRAP      EXIT IF F3
         TRAP      ESC IF F5
         MOVE      "NXCH5Move" TO PROGRAM
         MOVE      "NIN" TO COMPNME
         MOVE      "MOVe EXCHANGES" TO STITLE
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
         DISPLAY   *P10:14,"WHAT NOW ? ":
                   *P13:18,"1)CHANGE MAILER From ## 1880 to ##7352":
                   *P13:22,"2) STOP";
         KEYIN     *P13:23,"YOUR SELECTION:",*DE,N1;
         BRANCH    N1 OF KMLR1A,EXIT
         BEEP
         GOTO      KMLR1
KMLR1A
          Read      AccountS,seq;ACCKEY,BLANK08,ENTRY,FLAG
          goto      Exit if over
          unpack    Acckey into mlr1,mlr2
          If        (mlr1 = "001880" or mlr2 = "001880")
          call      Moveit
          endif
          goto      Kmlr1a
....................................
MOveIt
         Display     *P13:20,*EF,"CHANGING MAILER From ## 2883 to ##5656"
          MOVe      "001880",Old
          Move      "007352",new
FOUROK
         PACK      ACKEY1 FROM AKEY1,MLR1
         PACK      ACKEY2 FROM AKEY2,mlr2
         MATCH     MLR1,OLD
         GOTO      NEWKEY1 IF EQUAL
         MATCH     MLR2,OLD
         GOTO      NEWKEY2 IF EQUAL
         BEEP
          Stop      .should never happen
.
NEWKEY1
         CLEAR     NEWKEY
         APPEND    NEW,NEWKEY
         APPEND    MLR2,NEWKEY
         RESET     NEWKEY
         MOVE      ACKEY1,DELKEY1
         MOVE      ACKEY2,DELKEY2
         PACK      ACKEY1 FROM AKEY1,NEW
         PACK      ACKEY2 FROM AKEY2,MLR2
         MOVE      NEWKEY,SAVAKEY
         GOTO      KCHANGE
.
NEWKEY2
         CLEAR     NEWKEY
         APPEND    MLR1,NEWKEY
         APPEND    NEW,NEWKEY
         MOVE      ACKEY1,DELKEY1
         MOVE      ACKEY2,DELKEY2
         PACK      ACKEY1 FROM AKEY1,MLR1
         PACK      ACKEY2 FROM AKEY2,NEW
         RESET     NEWKEY
         MOVE      NEWKEY,SAVAKEY
.
KCHANGE
. 1ST MAKE SURE NEWKEY IS NOT A DUP
         READ      ACCOUNTA,ACKEY1,ACKEY2;;
         GOTO      DUP IF NOT OVER
         MOVE      SAVAKEY,NEWKEY
         FILEPI    2;ACCOUNTA
         READ      ACCOUNTA,DELKEY1,DELKEY2;;
         DELETE    ACCOUNTA
         WRITE     ACCOUNTA;NEWKEY,BLANK08,ENTRY,FLAG
         MOVE      ENTRY,SAVENTRY
         MOVE      ZERO,COUNT
.
         PACK      NEWXKEY FROM NEWKEY,ZERO
         REP       " 0" IN NEWXKEY
.
.         CLEAR     EXKEY
         PACK      EXKEY FROM ACCKEY,ZERO
         REP       " 0" IN EXKEY
         GOTO      CHNGDET
.
CHNGDET
. CHANGE DETAIL RECORDS
.
         FILEPI    5;XCHNG
         READ      XCHNG,EXKEY;BLANK17A,LR2,LR,USAGE1,USAGE2:
                   QTYFILL,QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         GOTO      SHAFTED IF OVER
         DELETE    XCHNG,EXKEY
         WRITE     XCHNG,NEWXKEY;NEWXKEY,LR2,LR,USAGE1,USAGE2:
                   QTYFILL,QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT,XCHFILLER
         INSERT    XLRNG,LR
.
         COMPARE   COUNT,SAVENTRY
         GOTO      DONE IF EQUAL
         GOTO      NEXT
SHAFTED  DISPLAY   *P1:24,*EL,"DETAIL READ OVER ",EXKEY,*W4;
         GOTO      TRYAGAIN
.
NEXT     ADD       ONE,COUNT
         CLEAR     NEWXKEY
         APPEND    NEWKEY,NEWXKEY
         APPEND    COUNT,NEWXKEY
         RESET     NEWXKEY
         REP       " 0",NEWXKEY
.
         CLEAR     EXKEY
         APPEND    ACCKEY,EXKEY
         APPEND    COUNT,EXKEY
         RESET     EXKEY
         REP       " 0",EXKEY
.
         GOTO      CHNGDET
.
DUP
         DISPLAY   *P1:24,*EL,"THIS RECORD ALL READY EXISTS UNDER THE NEW":
                   " NUMBER Aborting",acckey,*B;
         KEYIN     *P76:24,ANS;
          Stop
.
.
ESC      TRAPCLR   F5
         GOTO      DOMORE
.
DONE
         MOVE      ZERO TO COUNT
         DISPLAY   *P2:24,*EL,"One Acount Done!!! ",acckey;
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
         STOP
DOMORE
         KEYIN    *P2:24,*EL,"WANT TO DO SOME MORE ?",ANS;
         CMATCH    YES,ANS
         GOTO      KMLR1 IF EQUAL
         STOP
EXIT
         STOP
TWO      TRAP      IO IF IO
         KEYIN    *P1:23,*EL,"THE OLD RECORDS MUST BE DELETED BEFORE":
                   " YOU CAN ADD THESE BACK IN OK? ",ANS,*P1:23,*EL;
         CMATCH    "Y" TO ANS
         GOTO      KMLR1 IF NOT EQUAL
         KEYIN     *P1:24,*EL,"INPUT FILE NAME : ",FILEIN;
         OPEN      INPUT,FILEIN
         TRAPCLR   IO
.
.START PATCH 1.9.1 REPLACED LOGIC
.         READ      INPUT,SEQ;MLR1,MLR2,FILL5,LR,USAGE1,USAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         READ      INPUT,SEQ;MLR1,MLR2,FILL5,LR2,LR,USAGE1,USAGE2:
                   QTYFILL,QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
.END PATCH 1.9.1 REPLACED LOGIC
         MOVE      " " TO STAT          *BEG BAL STATUS ALWAYS " ".
         MOVE      USAGE1 TO NUSAGE1
         MOVE      USAGE2 TO NUSAGE2
         MOVE      ZERO TO ENTRY
         MOVE      ZERO TO COUNT
         GOTO      WRITE
READ
.START PATCH 1.9.1 REPLACED LOGIC
.         READ      INPUT,SEQ;MLR1,MLR2,FILL5,LR,USAGE1,USAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         READ      INPUT,SEQ;MLR1,MLR2,FILL5,LR2,LR,USAGE1,USAGE2:
                   QTYFILL,QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
.END PATCH 1.9.1 REPLACED LOGIC
         GOTO      WRITE1 IF OVER
         ADD       ONE TO COUNT
         DISPLAY   *P12:12,*EL,"NUMBER IF RECORDS : ",COUNT;
         ADD       ONE TO ENTRY
         CMATCH    "C" TO STAT        *IF CANCELLED DO NOT ADD QTY.
         GOTO      WRITE IF EQUAL
         CMATCH    "R" TO STAT
         GOTO      WRITE IF EQUAL     *IF RENTAL DO NOT ADD QTY.
         MOVE      MLRSW TO BRANCH
         BRANCH    BRANCH OF MLR1,MLR2
MLR1     ADD       QTY TO NUSAGE1
         GOTO      WRITE
MLR2     ADD       QTY TO NUSAGE2
         GOTO      WRITE
WRITE
         FILEPI    5;XCHNG,XLRNG
         CLEAR     EXKEY
         PACK      EXKEY FROM MLR1,MLR2,ENTRY
         REP       " 0" IN EXKEY
.
.START PATCH 1.9.1 REPLACED LOGIC
.         WRITE     XCHNG,EXKEY;EXKEY,LR,NUSAGE1,NUSAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         WRITE     XCHNG,EXKEY;EXKEY,LR2,LR,NUSAGE1,NUSAGE2:
                   QTYFILL,QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT,XCHFILLER
.END PATCH 1.9.1 REPLACED LOGIC
         INSERT    XLRNG,LR
         GOTO      READ
.
WRITE1   UNPACK    EXKEY INTO ACCKEY
         UNPACK    ACCKEY INTO MLR1,MLR2
         PACK      ACKEY2 FROM AKEY2,MLR2
         PACK      ACKEY1 FROM AKEY1,MLR1
         FILEPI    3;ACCOUNTA
         READ      ACCOUNTA,ACKEY1,ACKEY2;;
         GOTO      WRITE1OK IF OVER
         UPDATE    ACCOUNTA;ACCKEY,"        ",ENTRY," "
         DISPLAY   *P1:24,*EL,*B,*B,"MASTER RECORD ALL READY ON FILE!!!";
         CLOSE     INPUT
         GOTO      DOMORE
         REP       " 0" IN ACCKEY
WRITE1OK WRITE     ACCOUNTA;ACCKEY,"        ",ENTRY," "
.         INSERT    ACCOUNTA
         DISPLAY   *P1:24,*EL,"JOB DONE ",*W2;
         CLOSE     INPUT
         GOTO      DOMORE
.
NOREC
         DISPLAY   *P20:23,*EL,"NO Record found!";
         GOTO      DOMORE
;Patch1.9
                              include compio.inc
                              include   cntio.inc
;         INCLUDE   NMLRIO.inc
;Patch1.9
         INC       MLRHELP.inc
.START PATCH 1.9.1 ADDED LOGIC
          INCLUDE   NXNGIO.INC
          INCLUDE   NXCHIO.INC
.END PATCH 1.9.1 ADDED LOGIC
          INCLUDE   COMLOGIC.inc