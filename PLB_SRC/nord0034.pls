.NORD0037 - CREATED 12JUN92.
PC       EQU       0
.
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
.patch1.4
				include	compdd.inc
				include	cntdd.inc
.         INCLUDE   NMLRDD.inc
.patch1.4
         INCLUDE   OSLSPERN.inc
release 	init    "1.5"		08Mar2007  DLH	Oslspern.inc expansion
.release  init      "1.4"        DMB	26MAY2004	Mailer Conversion.
.RELEASE  INIT      "1.3"             02OCT2000 ASH NEW SERVER ADDED
.RELEASE  INIT      "1.2"             04JAN99 ASH NINORD Y2K, File expansion: did not increase QTYOLD & QTYNEW which
.                                                may need to be increased if OQTY ever fills its' new DIM 9 capacity!
.release  init      "1.1"             21SEP98 ASH NINMLR Y2K File expansion, increase file and key size to
.                                    reflect increase in MCOMP.  Note:  both key and file
.                                    size were totally screwy!!!!!!!
.RELEASE  INIT      "1.0"             07JUL92 DLH CHANGE DATABASE TO INCLUDE
.                                    ORDER MONTH & YEAR
.RELEASE  INIT      "PRE"             12JUN92 DLH PULL INFO BY SALES PERSON.
OSLS     DIM        25
.Start patch #1.1 - Increased file and key size
.File size and key were weird!!! did not match total var size!!! Only program to open this file!!!
.OUTPUT   IFILE      KEYLEN=58,UNCOMP,FIX=92
OUTPUT   IFILE      KEYLEN=55,UNCOMP,FIX=91
.End patch #1.1 - Increased file and key size
.OLNUM                        1-6
.
.Start patch #1.1 - Increased file and key size
.OUTKEY   DIM        58        7-62
.Old specs!!!!
.olnum    6  +
.mcomp    25 +
.oodtem   2  +
.oodtey   2  =
.-------------
.keylen  35!!!  not 58
OUTKEY   DIM        55        7-62
.New specs!!!!
.olnum    6  +
.mcomp    45 +
.oodtem   2  +
.oodtey   2  =
.-------------
.keylen  55
.End patch #1.1 - Increased file and key size
NUMOLD   FORM       6        63-68
QTYOLD   FORM       9        69-77
NUMNEW   FORM       6        78-83
QTYNEW   FORM       9        84-92
ANS      DIM        1
DATE     DIM        8
DATEMASK DIM        8
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
LASTYY   DIM        2
RUNLISTS INIT       "014477-005051-009766"
.
         IFZ       PC
.Start patch #1.1 - Increased file and key size
.         PREPARE   OUTPUT,"g:\DATA\SALESlo","g:\DATASALESlo","58","92"
.START PATCH 1.3 REPLACED LOGIC
.         PREPARE   OUTPUT,"g:\DATA\SALESlo","g:\DATASALESlo","55","91"
         PACK     STR35,NTWKPATH1,"SALESlo"
         PACK     STR45,NTWKPATH1,"SALESlo"
         PREPARE   OUTPUT,str35,str45,"55","91"
.END PATCH 1.3 REPLACED LOGIC
.End patch #1.1 - Increased file and key size
         XIF
         IFNZ      PC
         PREPARE   OUTPUT,"SALESlo:PRINT"
         XIF
. STARTING LR #.
         KEYIN     *P1:24,*EL,"HEAR WE GO";
.         MOVE      "185570" TO NORDFLD            change every jan.
         MOVE      "211248" TO NORDFLD            change every jan.
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         MOVE      DATE TO DATEMASK
         MOVE      DATE TO TODAY
         XIF
         TRAP      EOJ IF F5
         MOVE      "ABORT" TO PF5
         MOVE      "NORD0034" TO PROGRAM
         MOVE      "Names In The News Ca Inc"   TO COMPNME
         MOVE      "SALESPERSON X-REF" TO STITLE
         CALL       PAINT
         CALL       FUNCDISP
DATEDIS
         KEYIN     *P10:10,*DV,DATEMASK," OK? ",*t20,ANS
         CMATCH    "N" TO ANS
         GOTO      BEGIN IF NOT EQUAL
         KEYIN     *P10:10,*+,mm,"/",dd,"/",yy
         PACK      DATE FROM mm,SLASH,dd,SLASH,yy
         MOVE      DATE TO DATEMASK
         GOTO      DATEDIS
BEGIN
         MOVE      C0 TO N2
         MOVE      YY TO N2
         SUB       C1 FROM N2
         MOVE      N2 TO LASTYY
         MOVE      C1 TO NORDPATH
         MOVE      C1 TO NMLRPATH
.
         CALL      NORDKEY
         GOTO      EOJ IF OVER
         ADD       C1 TO N8
         GOTO      DETAIL
LOOP     CALL      NORDKS
         GOTO      EOJ IF OVER
         ADD       C1 TO N8
.
DETAIL   DISPLAY   *P10:12,"RECORDS PROCESSED ",N8
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES         *CANCELLED ?
         GOTO      LOOP IF EQUAL               *YES
         RESET     RUNLISTS
         SCAN      OLNUM IN RUNLISTS          *RUNNING CHARGE?
         GOTO      LOOP IF EQUAL              *YES
         move      "pl" to str2
         scan       ostat in str2
         goto       loop if equal        .Pending or LCR

.Start Patch #1.2 - increased var
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         COMPARE   C0 TO N7                 *REAL ORDER?
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         COMPARE   C0 TO N9                 *REAL ORDER?
.End Patch #1.2 - increased var
         GOTO      LOOP IF EQUAL            *NO.
         PACK      STR2 FROM OSALES10,OSALES
         MOVE      C0 TO N2
         MOVE      STR2 TO N2
         PACK      OSLS FROM B10,B10,B5
         MOVE      OSLS0 TO OSLS
         LOAD      OSLS FROM N2 OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5,OSLS6:
                   OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13,OSLS14:
                   OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21,OSLS22,osls23,osls24,osls25:
		osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
.
         PACK      STR7 FROM OMLRNUM,Z3
         MATCH     STR7 TO MKEY
         IF        NOT EQUAL
         MOVE      STR7 TO MKEY
         REP       ZFILL IN MKEY
         CALL      NMLRKEY
         ENDIF
         CLEAR     OUTKEY
         PACK      OUTKEY FROM Olnum,MCOMP,OODTEM,OODTEY
         READ      OUTPUT,OUTKEY;olnum,OUTKEY,NUMOLD,QTYOLD,NUMNEW,QTYNEW
         GOTO      WRITE IF OVER
         MATCH     OODTEY TO YY
         IF        EQUAL
         ADD       C1 TO NUMNEW
.Start Patch #1.2 - increased var
.         ADD       N7 TO QTYNEW
         ADD       N9 TO QTYNEW
.End Patch #1.2 - increased var
         GOTO      UPDATE
         ENDIF
         MATCH     OODTEY TO LASTYY
         IF        EQUAL
         ADD       C1 TO NUMOLD
.Start Patch #1.2 - increased var
.         ADD       N7 TO QTYOLD
         ADD       N9 TO QTYOLD
.End Patch #1.2 - increased var
         GOTO      UPDATE
         ENDIF  
         GOTO      LOOP
UPDATE   FILEPI    1;OUTPUT
         UPDATE    OUTPUT;olnum,OUTKEY,NUMOLD,QTYOLD,NUMNEW,QTYNEW
         GOTO      LOOP
WRITE    PACK      OUTKEY FROM Olnum,MCOMP,OODTEM,OODTEY       .RESTORE KEY
         PACK      STR2 FROM OSALES10,OSALES
         MOVE      C0 TO NUMNEW
         MOVE      C0 TO QTYNEW
         MOVE      C0 TO NUMOLD
         MOVE      C0 TO QTYOLD
         MATCH     OODTEY TO YY
         IF        EQUAL
         ADD       C1 TO NUMNEW
.Start Patch #1.2 - increased var
.         ADD       N7 TO QTYNEW
         ADD       N9 TO QTYNEW         
.End Patch #1.2 - increased var
         GOTO      WRITE1
         ENDIF
         MATCH     OODTEY TO LASTYY
         IF        EQUAL
         ADD       C1 TO NUMOLD
.Start Patch #1.2 - increased var
.         ADD       N7 TO QTYOLD
         ADD       N9 TO QTYOLD
.end Patch #1.2 - increased var
         GOTO      WRITE1
         ENDIF  
         GOTO      LOOP
WRITE1   FILEPI    1;OUTPUT
         WRITE     OUTPUT,OUTKEY;olnum,OUTKEY,NUMOLD,QTYOLD,NUMNEW,QTYNEW
         GOTO      LOOP
.
EOJ      STOP

         INCLUDE   NORDIO.inc
.patch1.4
				include	compio.inc
				include	cntio.inc
.         INCLUDE   NMLRIO.inc
.patch1.4
         INCLUDE   COMLOGIC.inc

