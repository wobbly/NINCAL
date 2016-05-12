PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
.START PATCH 1.3 ADDED LOGIC
	INCLUDE	NSEL2DD.INC
.END PATCH 1.3 ADDED LOGIC
. TELEORD - PROGRAM TO PULL ORDERS FOR TELECOMM
.
. CREATED SEPT 23 1986
.
RELEASE   INIT      "1.3"       ASH 12FEB2004 DATACARD CONVERSION
.RELEASE   INIT      "1.2"       ASH 02OCT2000 NEW SERVER ADDED
.RELEASE   INIT      "1.1"       DLH 2JUL92  FIXED IO ERROR DISPLAYS
.
.RELEASE  INIT      "1.X"       DLH 18MAR92
.
. FILES
. .....
INORD   FILE     FIXED=557,STATIC=12  DAILY PRINT FILE.
TELEORD  IFILE     KEYLEN=6
TELEMLR  IFILE     KEYLEN=7
CMSORD   IFILE     KEYLEN=10,FIXED=81
.
. TELEMLR FILE VARIABLES.
. ........................
.
TMLR     DIM       4         MLR NUMBER
TCNT     DIM       3         CONTACT NUMBER
TTEAM    DIM       1         TEAM CODE
TACRNYM  DIM       4         CLIENT ACRONYM
TIDENT   FORM      4         CLIENT IDENTIFIER, USED IN BRANCH
. CMSORD FILE VARIABLES.
. ......................
TRANCODE INIT      "09"          FOR ORDERS IT EQUALS '09'.   KEY
TEAM     DIM       1          CMS TEAM CODE.
CLIENT   DIM       4          FOUR BYTE ACRONYM.   KEY
BLANK1   DIM       1          "DISP" ON OTHER TRANSACTION TYPES.
PERD   DIM       1          '1', '2', OR '3'.   KEY
SEQUENC  DIM       3          '000' THRU '999'.   KEY
LRNUM    DIM       6          NAME IN THE NEWS LR#.
COSTPM   DIM       5          COST PER THOUSAND
AOQTY    DIM       6          QUANTITY OF ORDER.
AOMLDTE  DIM       4          MAIL DATE MMDD
KEY1     DIM       5          LEFT JUSTIFIED KEY.
KEY2     DIM       5
KEY3     DIM       5
KEY4     DIM       5
KEY5     DIM       5
KEY6     DIM       5
KEY7     DIM       5
KEY8     DIM       5
KEY9     DIM       5
FILL3    DIM       3          USED TO PAD RECORD LENGTH TO 81 BYTES.
CMSKEY   DIM       10       *TRANCODE,CLIENT,PERD,SEQ.
+ .............................................................................
. MAILER IDENTIFICATION.
. ......................
.
.     MAILER  COMPANY  NAME                 IDENT CODE.
.     ---------------------                 -----------
.
. CRAVER,MATHEWS,SMITH                         0001
. DIVOKY ANS ASSOCIATES                        0002
. MAL WARWICK AND ASSOCIATES                   0003
. .............................................................................
. OTHER PROGRAM VARIABLES.
. ........................
ANS      DIM       1
ZERO     FORM      "0"
TWO      FORM      "2"
THREE    FORM      "3"
FILE     FORM      1         *BRANCH INDICATOR FOR I/O ERRORS.
COUNTDMC FORM      4         *NUMBER OF TRIPLEX ORDERS FOUND.
COUNT1   FORM      4         *NUMBER OF CMS ORDERS
COUNT2   FORM      4         *NUMBER OF DIVOKY ORDERS
COUNT3   FORM      4         *NUMBER OF WARWICK ORDERS.
COUNT4   FORM      4         *NUMBER OF EPSILON
HOLDMLR  DIM       7         *CHECK FOR MAILER BREAK.
MLRKEY   DIM       7         *MAILER KEY FIELD
BRANCH   FORM      4         *BRANCH VARIABLE FOR CLIENT IDENT.FROM TELEMLR/TXT
TDMCSW   FORM      1         *BRANCH FOR TRIPLEX ORDERS 1=YES, TRIPLEX
TDMC0    INIT      "TDMC"
TDMC1    INIT      "TRIPLEX"
FILL37   DIM       37
SPCL6    DIM       2
SPCL7    DIM       2
SPCL8    DIM       2
SPCL9    DIM       2
DESC0L1  DIM       47
DESC0L2  DIM       47
DESC991  DIM       47
DESC992  DIM       47
DESC981  DIM       47
DESC982  DIM       47
FILL32   DIM       32
FORM     FORM      2          *HOLDS FORM POINTER VALUE.
FORM1    FORM      2          *HOLDS FORM POINTER VALUE.
HIDENT   FORM      4          *HOLD ID.
HOLDKEY  DIM       6
EPSW     FORM      1          *IF '1' - OWNER IS EPSILON.
+ .............................................................................
. PROGRAM MAIN
. ............
         MOVE      "NORD0016" TO PROGRAM
         MOVE      "Names In The News Ca Inc" TO COMPNME
         MOVE      "PREPARE TELECOM ORDERS" TO STITLE
         CALL      PAINT
         DISPLAY   *P14:20,"READY TO LOOK FOR TELECOM ORDERS";
         TRAP      IO GIVING ERROR IF IO
         MOVE      C1 TO FILE
.START PATCH 1.2 REPLACED LOGIC
.         OPEN      INORD,"g:\data\NPRINT",EXCLUSIVE
         PACK      STR35,NTWKPATH1,"NPRINT"
         OPEN      INORD,STR35,EXCLUSIVE
.END PATCH 1.2 REPLACED LOGIC
         ADD       C1 TO FILE
         OPEN      TELEMLR,"TELEMLR"
         ADD       C1 TO FILE
.         OPEN      CMSORD,"CMSOUT"
.         OPEN      CMSORD,"CMSREV"
         ADD       C1 TO FILE
         OPEN      TELEORD,"DIVORD"
         SUB       FILE FROM FILE
.         OPEN      DELETE,"TDMCDELETE"
.

INPUT    MOVE      C1 TO FILE
         READ      INORD,SEQ;ORDVARS:
                              SPCL7,DESC0L1,DESC0L2:
                              SPCL8,DESC991,DESC992:
                              SPCL9,DESC981,DESC982
         GOTO      EOJ IF OVER
         SUB       FILE FROM FILE
         MOVE      ZERO TO TDMCSW       *CLEAR TRIPLEX BRANCH
         MOVE      "0" TO EPSW          *CLEAR EPSILON SW.
         MATCH     "1547" TO OLON       *EPSILON THE OWNER?
         CALL      EPSILON IF EQUAL     *YES
         MATCH     "2191" TO OLON       *EPSILON THE OWNER?
         CALL      EPSILON IF EQUAL     *YES
         GOTO      CHECKMLR
.         MOVE      OLON TO OWNKEY
.         REP       " 0" IN OWNKEY
.         MOVE      "3" TO FILE
.         INCLUDE   INCLUDE.DOWNIO
.         INCLUDE   DOWNIO.inc
.         GOTO      INPUT IF OVER            *NOT A VALID OWNER.
.         SUB       FILE FROM FILE
.         GOTO      TEST
EPSILON  MOVE      C1 TO EPSW
         RETURN
.TEST     SCAN      TDMC0 IN OWNCTN          *TRIPLEX CCTO?
.         GOTO      GOODREC IF EQUAL        *YES
.         SCAN      TDMC1 IN OWNCTN          *TRIPLEX CCTO?
.         GOTO      CHECKMLR IF NOT EQUAL       *NO.
. ........................
. GOODREC - TRIPLEX ORDER.
.GOODREC  ADD       C1 TO COUNTDMC
.         DISPLAY   *P12:23,*EL,"NUMBER OF TRIPLEX ORDERS FOUND : ",COUNTDMC;
.         MOVE      C1 TO TDMCSW
.         GOTO      CHECKMLR
CHECKMLR PACK      MLRKEY FROM OMLRNUM,OCOBN
         REP       " 0" IN MLRKEY
         MATCH     MLRKEY TO HOLDMLR
         GOTO      WRTBRNCH IF EQUAL
         MOVE      MLRKEY TO HOLDMLR
         MOVE      "4" TO FILE
         READ      TELEMLR,MLRKEY;TMLR,TCNT,TEAM,TACRNYM,TIDENT
         CALL      NOMLR IF OVER
         SUB       FILE FROM FILE
.
WRTBRNCH BRANCH    TIDENT OF MLR1,MLR2,MLR3,MLR4
.         BRANCH    TDMCSW TO WRITDMC           *IF TRIPLEX OUTPUT
         GOTO      INPUT
.
NOMLR    MOVE      ZERO TO TIDENT
         RETURN
.
MLR1     ADD       C1 TO COUNT1
         DISPLAY   *P12:08,*EL,"CMS ORDERS : ",COUNT1;
         MOVE      "5" TO FILE
         GOTO      CMSEDIT
MLR2     COMPARE   TIDENT TO HIDENT     *CORRECT FILE ALL READY OPEN?
         GOTO      MLR2A IF EQUAL       *YES
         CLOSE     TELEORD
         MOVE      C4 TO FILE
         OPEN      TELEORD,"DIVORD"
MLR2A    ADD       C1 TO COUNT2
         MOVE      "6" TO FILE
         DISPLAY   *P12:09,*EL,"DIVOKY ORDERS : ",COUNT2;
         GOTO      WRITMLR
MLR3     COMPARE   TIDENT TO HIDENT     *CORRECT FILE ALL READY OPEN?
         GOTO      MLR3A IF EQUAL       *YES
         CLOSE     TELEORD
         MOVE      C5 TO FILE
         OPEN      TELEORD,"WARORD"
MLR3A    ADD       C1 TO COUNT3
         MOVE      C5 TO FILE
         DISPLAY   *P12:10,*EL,"WARWICK ORDERS : ",COUNT3;
         GOTO      WRITMLR
MLR4     COMPARE   TIDENT TO HIDENT     *CORRECT FILE ALL READY OPEN?
         GOTO      MLR4A IF EQUAL       *YES
         CLOSE     TELEORD
         MOVE      C6 TO FILE
         OPEN      TELEORD,"EPSORD"
MLR4A    ADD       C1 TO COUNT4
         MOVE      C6 TO FILE
         DISPLAY   *P12:10,*EL,"EPSILON ORDERS : ",COUNT4;
         MOVE      "0" TO EPSW
         GOTO      WRITMLR
OWNEPS   CLOSE     TELEORD
         MOVE      C6 TO FILE
         OPEN      TELEORD,"EPSORD"
         GOTO      MLR4A
. .............................................................................
. WRITMLR - OUTPUT ORDER INFORMATION. (EXCEPT CMS, HAS DIFFERENT FORMAT)
WRITMLR
         READ      TELEORD,OLRN;;
         GOTO      WRITMLR1 IF NOT OVER      *RECORD ALL READY IN FILE
.         GOTO      UPMLR IF NOT OVER
         WRITE     TELEORD,OLRN;OLRN,OSTAT
.WRITMLR1 BRANCH    TDMCSW OF WRITDMC
.         SUB       FILE FROM FILE
WRITMLR1
         GOTO      INPUT
.. ............................................................................
. WRITDMC
.WRITDMC  BRANCH    EPSW OF OWNEPS
.         MOVE      OLRN TO HOLDKEY
.         READ      TDMCORD,OLRN;;
.         GOTO      TDMCEXIT IF NOT OVER            *RECORD IS ALL READY IN FIL
.         MOVE      HOLDKEY TO OLRN
.         WRITE     TDMCORD,OLRN;OLRN,OSTAT
.WRITDMC1 SUB       FILE FROM FILE
.TDMCEXIT - IF RECORD HAS BEEN PREVIOUSLY DELETED FROM T/C FILE. REMOVE FROM
.           LOG BECAUSE IS NOW BEING SET UP FOR T/C.
.TDMCEXIT READ      DELETE,OLRN;;
.         GOTO      INPUT IF OVER
.         FILEPI    1;DELETE
.         DELETE    DELETE,OLRN
.         GOTO      INPUT
. .............................................................................
. CMSEDIT - VERIFY INFO CORRECT AND OUTPUT CMS ORDER INFO
CMSEDIT
         BUMP      OQTY,1                        *REMOVE LEADING BLANK.
         MOVE      OQTY TO AOQTY                 *MOVE TO CMS OUPUT FIELD.
         RESET     OQTY,0
         RESET     OQTY
         REP       " 0" IN AOQTY                 *ZERO FILL.
         PACK      AOMLDTE FROM OMDTEM,OMDTED    *GET MAIL DATE.
         UNPACK    OMLRPON INTO PERD,ANS,SEQUENC   *GET PERIOD & SEQUENCE.
         MOVE      OLRN TO LRNUM                     *GET NIN LR NUMBER.
.START PATCH 1.3 REPLACED LOGIC
.         MOVE      OPPM TO COSTPM                    *GET COST PER THOUSAND.
	packkey	NSEL2FLD,"1",OLRN
	move	"NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		unpack	OPPM,str3,str2
		pack	str6,str3,".",str2
		rep	zfill,str6
		move	str6,NSEL2PRICE
	endif
	unpack	NSEL2PRICE,str5,str1,str2
	bump	str5,2
	pack	COSTPM,str5,str2
.END PATCH 1.3 REPLACED LOGIC
         REP       " 0" IN COSTPM                *ZERO FILL.
         CLEAR     KEY1
         CLEAR     KEY2
         CLEAR     KEY3
         CLEAR     KEY4
         GOTO      KEYEXIT                 *KEYS NOW SUPPLIED BY CMS.1/2/85
         SCAN      DASH IN OMLRKY
         GOTO      KEY1 IF EQUAL
         GOTO      KEYEXIT IF EOS
         GOTO      KEYNG
. KEY1 - GET FIRST KEY
KEY1
         MOVEFPTR  OMLRKY INTO FORM
         BUMP      OMLRKY BY -1
         LENSET    OMLRKY
         RESET     OMLRKY
         UNPACK    OMLRKY INTO KEY1
         RESET     OMLRKY TO FORM
         SETLPTR   OMLRKY
         BUMP      OMLRKY BY 1
         MOVEFPTR  OMLRKY INTO FORM1
         GOTO      KEYEXIT IF EOS
         SCAN      DASH IN OMLRKY
         GOTO      KEY2A IF EQUAL                    *THERE IS A THIRD KEY ALSO
KEY2     PACK      KEY2 FROM OMLRKY
         GOTO      KEYEXIT
KEY2A
         MOVEFPTR  OMLRKY INTO FORM
         BUMP      OMLRKY BY -1
         LENSET    OMLRKY
         RESET     OMLRKY TO FORM1
         UNPACK    OMLRKY INTO KEY2
         RESET     OMLRKY TO FORM
         SETLPTR   OMLRKY
         BUMP      OMLRKY BY 1
         GOTO      KEYEXIT IF EOS
         MOVEFPTR  OMLRKY TO FORM1
         SCAN      DASH IN OMLRKY
         GOTO      KEY3A IF EQUAL                   *THERE IS A FOURTH KEY ALSO
KEY3     PACK      KEY3 FROM OMLRKY
         GOTO      KEYEXIT
KEY3A
         MOVEFPTR  OMLRKY INTO FORM
         BUMP      OMLRKY BY -1
         LENSET    OMLRKY
         RESET     OMLRKY TO FORM1
         UNPACK    OMLRKY INTO KEY3
         RESET     OMLRKY TO FORM
         SETLPTR   OMLRKY
         BUMP      OMLRKY BY 1
         PACK      KEY4 FROM OMLRKY
         RESET     OMLRKY
         SETLPTR   OMLRKY
         GOTO      KEYEXIT
.
. KEYNG - KEYS NOT IN APPROVED CMS FORMAT.
KEYNG
         UNPACK    OMLRKY INTO KEY1,KEY2,KEY3        *GET KEY INFO?
         GOTO      WRITE
KEYEXIT
         MOVE      "     " TO KEY1
         MOVE      "     " TO KEY2
         MOVE      "     " TO KEY3
         MOVE      "     " TO KEY4
         MOVE      "     " TO KEY5
         MOVE      "     " TO KEY6
         MOVE      "     " TO KEY7
         MOVE      "     " TO KEY8
         MOVE      "     " TO KEY9
         GOTO      WRITE
.
. WRITE - WRITE TO CMSORD.
WRITE
         MOVE      TACRNYM TO CLIENT
         PACK      CMSKEY FROM TRANCODE,CLIENT,PERD,SEQUENC
         FILEPI    4;CMSORD
         READ      CMSORD,CMSKEY;;
         GOTO      UPDATE IF NOT OVER
         WRITE     CMSORD,CMSKEY;"09",TEAM,CLIENT,BLANK1,PERD,SEQUENC:
                   LRNUM,COSTPM,AOQTY,AOMLDTE,KEY1,KEY2,KEY3,KEY4:
                   KEY5,KEY6,KEY7,KEY8,KEY9,FILL3
.         BRANCH    TDMCSW OF WRITDMC      *TRIPLEX ?
.         SUB       FILE FROM FILE
         GOTO      INPUT                  * NO.
UPDATE
         UPDATE    CMSORD;"09",TEAM,CLIENT,BLANK1,PERD,SEQUENC:
                   LRNUM,COSTPM,AOQTY,AOMLDTE,KEY1,KEY2,KEY3,KEY4:
                   KEY5,KEY6,KEY7,KEY8,KEY9,FILL3
.         BRANCH    TDMCSW OF WRITDMC      *TRIPLEX ?
.         SUB       FILE FROM FILE
         GOTO      INPUT                  * NO.
. .............................................................................
EOJ      CLOSE     TELEMLR
         CLOSE     TELEORD
.         SHUTDOWN  "CHAIN/OV1"
         STOP
IO       TRAPCLR   IO
         NORETURN
         DISPLAY   *P1:23,*EL,ERROR;
         BRANCH    FILE TO ONE,TWO,THREE,FOUR,FIVE,SIX
ZERO     DISPLAY   *P1:24,*EL,"UNKNOWN I/O ERROR",*B,*W2;
         GOTO      IOEXIT
ONE      DISPLAY   *P1:24,*EL,"ORDER PRINT FILE I/O ERROR",*B,*W2;
         GOTO      IOEXIT
TWO      DISPLAY   *P1:24,*EL,"NIN TELEMLR FILE I/O ERROR",*B,*W2;
         GOTO      IOEXIT
THREE    DISPLAY   *P1:24,*EL,"NIN CMSREV FILE I/O ERROR",*B,*W2;
         GOTO      IOEXIT
FOUR     DISPLAY   *P1:24,*EL,"NIN DIVORD FILE I/O ERROR",*B,*W2;
         GOTO      IOEXIT
FIVE     DISPLAY   *P1:24,*EL,"NIN WARORD FILE I/O ERROR",*B,*W2;
         GOTO      IOEXIT
SIX      DISPLAY   *P1:24,*EL,"NIN EPSORD FILE I/O ERROR",*B,*W2;
         GOTO      IOEXIT
IOEXIT   KEYIN     *P77:24,ANS;
         CMATCH    "Q" TO ANS
         STOP      IF EQUAL
         BRANCH    FILE TO ONE,TWO,THREE,FOUR,FIVE,SIX
         GOTO      ZERO
.START PATCH 1.3 ADDED LOGIC
	INCLUDE	NSEL2IO.INC
.END PATCH 1.3 ADDED LOGIC
         INCLUDE   COMLOGIC.inc
