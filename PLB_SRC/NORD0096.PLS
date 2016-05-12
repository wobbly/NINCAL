PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INC       NORDDD.inc
         inc       ninvdd.inc
         inc       ndatdd.inc
         INCLUDE   NSPEDD.INC
	INCLUDE	COMPDD.inc
	INCLUDE	CNTDD.inc
.
RELEASE  INIT      "1.2"       ASH   13JUL99   NINSPE FILE CONVERSION
.RELEASE  INIT      "1.1"       DLH   18MAR92   INCLUDES, PCBUS
TDMCORD  FILE      
JDout    file
INFILE   FILE      VAR=7
TDMCOUT  IFILE     KEYLEN=6,VAR=288
KEY      DIM       6
ONE      FORM      "1"
READCNT  FORM      4
writupd  FORM      4
writCNT  FORM      4
spec1    form      1
LR       DIM       6
SPCL1    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL2    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL3    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL4    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL5    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL6    DIM       2           *SPECIAL INSTRUCTION CODE
SPCL7    DIM       2         *ORDER FILLER
SPCL8    DIM       2         *
SPCL9    DIM       2         *
.START PATCH 1.2 - ADDED VARS
DESC0L1  DIM       47       7-53      LINE 1 OF 00 INSTRUCTION
DESC0L2  DIM       47      54-100     LINE 2 OF 00 INSTRUCTION
DESC991  DIM       47     101-147     LINE 1 OF 99 INSTRUCTION
DESC992  DIM       47     148-194     LINE 2 OF 99 INSTRUCTION
DESC981  DIM       47     195-241     LINE 1 OF 98 INSTRUCTION
DESC982  DIM       47     242-288     LINE 2 OF 98 INSTRUCTION
.END PATCH 1.2 - ADDED VARS
.
.
         MOVE      "NORD0095" TO PROGRAM
         MOVE      "Names in the News Ca Inc" TO COMPNME
         MOVE      "Fix mlrs./inv's #'s" TO STITLE
         move      c1 to nordpath
         move      c1 to ninvpath
         move      c1 to ndatpath
         CALL      PAINT
         prepare   jdout,"c:\work\jdout2.dat"
         OPEN      TDMCORD,"c:\work\listmlr.sav"
.
READ
MainLoop read      tdmcord,seq;lr,b1,compnum
         goto      eoj if over
.         if        (Ostat <> "0" or OSTAT <> "B")
.         CMATCH    "p" TO OSTAT       Pending order ?
.         GOTO      read IF EQUAL     YES, skip.
.         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
.         GOTO      read IF EQUAL     YES, skip.
.         CMATCH    "l" TO OSTAT      lcr order ?
.         GOTO      read IF EQUAL     YES, skip.
.         CMATCH    "z" TO OSTAT       Cancelled LCR order ?
.         GOTO      read IF EQUAL     YES, skip.
         ADD       ONE TO READCNT
         DISPLAY   *P10:20,"NUMBER OF ORDERS READ : ",READCNT
	pack	COMPFLD,compnum
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
         write      jdout,seq;lr,b1,compoldmlr
         goto read
.         RESET     EXCODES
.         SCAN      OELCODE IN EXCODES
.         GOTO      RENT IF NOT EQUAL
.         GOTO      EXCHANGE IF EQUAL
.         GOTO      RENT
EXCHANGE 
.MOVE      "*EXCH*" TO EXCHTEXT
.         RESET     CANCODES
.         SCAN      OSTAT IN CANCODES
.         GOTO      DETAIL IF EQUAL
.Start Patch #3.0 - replaced var
.         MOVE      C0 TO N7
.         MOVE      OEXQTY TO N7
.         COMPARE   C0 TO N7
.         MOVE      C0 TO NUM9
.         MOVE      OEXQTY TO NUM9
.         COMPARE   C0 TO NUM9
.End Patch #3.0 - replaced var
.         IF        EQUAL
.         GOTO      SPLIT IF NOT EQUAL
.         move       "5265" to omlrnum
.         move       "000" to ocobn
. 	if         over
. 	move       "P" to ocompid
.        move       "019675",ndatfld
.        call       ndatkey
.        move       olstname to O1DES
.        move       "019675",olnum        
.	if         over
.	call       nordwrt
.	endif
.         DISPLAY   *P10:22,"NUMBER OF orders written  : ",writcnt
..         endif
..			move       lrn to ninvfld
..			call       ninvtst
..			if         over
..			call       ninvwrt
..			endif
..
.         GOTO      READ
.
eoj         CLOSE     TDMCORD
.	WEOF        jdout
	close       jdout
         shutdown   "cls"
         INCLUDE   COMLOGIC.inc
         include   ninvio.inc
	 include   ndatio.inc
	INCLUDE	COMPIO.inc
	INCLUDE	CNTIO.inc
        include    nordio.inc
         INCLUDE   NSPEIO.INC

