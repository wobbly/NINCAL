...........................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORD2DD.inc
.START PATCH 2.73.3 REPLACED LOGIC
.         INCLUDE   NMLRDD.INC
.         INCLUDE   NBRKDD.INC
	INCLUDE	COMPDD.inc
	INCLUDE	CNTDD.inc
.END PATCH 2.73.3 REPLACED LOGIC
         INCLUDE   NOWNDD.inc
         INCLUDE   NSHPDD.inc
         INCLUDE   NDATDD.inc
         include   nspedd.inc
         include   nrtndd.inc
         INCLUDE   NMRGDD.inc
         INCLUDE   NMOADD.inc
.         INCLUDE   NSPIDD.inc
         INCLUDE   NMOBDD.inc
         include   media.inc
         include   shipping.inc
         include   hp.inc
.START PATCH 2.2 - ADDED LOGIC
         include   nofrdd.inc
.END PATCH 2.2 - ADDED LOGIC
.START PATCH 2.81 REMOVED LOGIC
..START PATCH 2.71 ADDED LOGIC
.	INCLUDE	NFULDD.INC
..END PATCH 2.71 ADDED LOGIC
.END PATCH 2.81 REMOVED LOGIC
.START PATCH 2.73 - ADDED LOGIC
	INCLUDE	NSELDD.INC
	INCLUDE	NSEL2DD.INC
	INCLUDE	NTXTDD.INC
.END PATCH 2.73 - ADDED LOGIC
.START PATCH 2.73.1 - ADDED LOGIC
	INCLUDE	NSEL3DD.INC
	INCLUDE	NADDDD.INC
	INCLUDE	NSLTDD.INC
	INCLUDE	NREFDD.INC
	INCLUDE	NMODDD.INC
.END PATCH 2.73.1 - ADDED LOGIC
Release  init      "2.85"        JD   	23APR2007	Moved "Exchange" status print to catch exch/rent marked orders.
.Release  init      "2.84"       DMB   	12OCT2006	Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.release  init      "2.83"      DLH	18Sep2006	IF shipped take regardless of date
.				   IF date is due take even if not shipped
.release  init      "2.82"      DLH	12JUL2006	Turn off Epsilon Discount
.release  init	   "2.81"	DMS	22JUN2006	FULFILLMENT CONVERSION
.release  init      "2.80 "     JD	20JAN2006	Added check for Variance flag.(listmgmt only)
;release  init      "2.76"      JD	03JUN2005	changed input name to NINORDU.SRT.
;release  init      "2.75"      JD	09JUn2004	Refix required trim of datacard var Commper
;RELEASE  INIT      "2.73.3"    ASH	27MAY2004	MAILER CONVERSION
.				ASH 			Corrected logic around NINSEL3
.RELEASE  INIT      "2.73.2"            29APR04		DB FIXES
.RELEASE  INIT      "2.73.1"	ASH 	12APR04		DATACARD CONVERSION
.RELEASE  INIT      "2.73"    	ASH 	29jan04 	DATACARD CONVERSION
.RELEASE  INIT      "2.72"    	JD 	06AUG2002 	added lw robbins 12/m mailers.
.release  init      "2.71"    	ASH	05FEB2002 	NINFUL CONVERSION
.release  init      "2.7"     	DLH  	24OCT2001 	new date logig around no return dates
.release  init      "2.6"     	ASH 	02OCT2000 	NEW SERVER ADDED
.release  init      "2.5"     	ASH 	01JUN2000	Added Media types
.release  init      "2.4"     	DLH 	08Oct99 	additional rtn date checks
.release  init      "2.3"     	ASH 	26May99 	NINSPEC.DAT conversion
.RELEASE  INIT      "2.2"     	ASH 	07MAY99 	REPLACED OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.Release  Init      "2.1"     	DLH 	01Apr99 	new Shipping.inc
.RELEASE  INIT      "2.0"     	ASH 	08JAN99 	NINORD Y2K, File expansion
.Release  init      "1.9"     	DLH 	28Sep98 	added code to handle pending orders
.                            				See norddd.inc patch 5
.release   init     "1.8"       DLH 	09JUl98 	use datacard commission on brokerage rental estimates.
.release  init       "1.7"      DLH 	10Jun98 	add nspedd & io set locking flags off for mailer,datacard, & spec inst reads.
.release  init       "1.6"      DLH 	27Mar98 	Turned off read only
.                               			open as program is only one using file.
.release  init       "1.5"      DLH 	07Feb97 	new data card format
.release  init       "1.4"      JD  	02feb96 	only proces 2/1/96 forward
.release  init       "1.3"      JD  	21jun95 	check broker file if lstmgmt
.release  init       "1.2"      DLH 	27Feb95		new batch bill code
.Release   init      "1.1"      JD  	11jan95 	added nbrkdd. print brcomp
.                                     			print of net name.
.Release   init      "1.0"      DLH 	15may94 	initial release from nord013b
. .............................................................................
.
.
. FILES DESCRIPTIONS
.OUTSP    IFILE     KEYLEN=6,VAR=288
. ..................
. ...................................
.ACCOUNT1 IFILE     KEYLEN=7
. .....
. .............................................................................
.
.
. WORK VARIABLES
.
THOUS    FORM      "1000"
.SEQ      FORM      "-1"
HUND     FORM      "100"
PDATE    DIM       8
JOBBR    FORM      1               BRANCH FOR JOB TYPE SEPERATE,TOTAL
CHANGE   FORM      7.2
SAMEMLR  DIM       7
TOTALDOL FORM      7.2
YR       DIM       2
GROSS    FORM      7.2
.Start Patch #2.0 - replaced var
.TOTAL    FORM      7.2
.AR       FORM      7.2
TOTAL    FORM      9.2
AR       FORM      9.2
.END Patch #2.0 - replaced var
CODENUM  FORM      2
EXCLPRT  DIM       4
.
PROGNAME DIM       8
CHKJUL   FORM      5
.Start Patch #2.0 - replaced var
.UNBILAMT FORM      7.2
.FORM72   FORM      7.2
UNBILAMT FORM      9.2
FORM92   FORM      9.2
.Start Patch #2.0 - replaced var
FORM52   FORM      5.2
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10             NUMERIC WORK FIELD FOR CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
.Start Patch #2.0 - replaced var
.UNBILINC FORM      7.2
.UNLNC30  FORM      7.2
.UNLNC60  FORM      7.2
.UNLNC90  FORM      7.2
.UNLNC90P FORM      7.2
.UNBILTOT FORM      10.2
.LSTMTOT  FORM      7.2             TOTAL LIST MANAGEMENT DUE 30 DAYS
.BRKEXTOT FORM      7.2             TOTAL BROKERAGE EXCHANGE DUE 30 DAYS
.BRKRNTOT FORM      7.2             TOTAL BROKERAGE RENTAL DUE 30 DAYS
.
UNBILINC FORM      9.2
UNLNC30  FORM      9.2
UNLNC60  FORM      9.2
UNLNC90  FORM      9.2
UNLNC90P FORM      9.2
UNBILTOT FORM      12.2
LSTMTOT  FORM      9.2             TOTAL LIST MANAGEMENT DUE 30 DAYS
BRKEXTOT FORM      9.2             TOTAL BROKERAGE EXCHANGE DUE 30 DAYS
BRKRNTOT FORM      9.2             TOTAL BROKERAGE RENTAL DUE 30 DAYS
.END Patch #2.0 - replaced var
WRIT     FORM      1
printd   form     4
.
KEY      DIM       28
. .............................................................................
.
.
. PROGRAM VARIABLES
. .................
.
ANS      DIM       1
DATE     DIM       8
TIME     DIM       8
febdat   form      5
MDATE    FORM      5
JUNDATE  FORM      6
chkdate  form      6
chkdate2 dim       6
.Start Patch #2.0 - replaced var
.QTYCHK   FORM      6
QTYCHK   FORM      9
.end Patch #2.0 - replaced var
CHKMM    FORM      2                  HOLDS MONTH PARAMETER
TENDOLL  FORM      "99999"
NINEDOLL FORM      "199999"
SEVDOLL  FORM      "300000"
JUNEDAT  INIT      "060191"
..............................................................................
CHKYR    FORM      2                  HOLDS YEAR PARAMETER
PASS     FORM      3                  FOR JOBBR = 1 PASS = 1 = batch
.                                     FOR JOBBR = 1 PASS = 2 = Broker orders
.                                     FOR JOBBR = 1 pass = 3 - Non broker.
.                                     FOR JOBBR = 2 NOT  USED.
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
CHKMLR   DIM       4                  USED TO ELIMINATE DUP MAILER DISP.
COMPMLR  DIM       4
LINES    FORM      2
PAGE     FORM      5
result1  form      3
remain   form      3.1
PBREAK   FORM      "57"
SHIPPED  DIM       9
EXCHANGE DIM       9
COMSLCT  DIM       9
COUNTO   FORM      5                  NUMBER OF ORDERS READ.
COUNTO1  FORM      5                  NUMBER OF ORDERS CALCULATED.
COUNTI   FORM      5                  NUMBER OF INVOICES READ
COUNTI1  FORM      5                  NUMBER OF INVOICES CALCULATED
.Start Patch #2.0 - replaced var
.TOTALU   FORM      7.2
TOTALU   FORM      9.2
.END Patch #2.0 - replaced var
TDMCSW   DIM       1                  TRIPLEX INDICATOR Y=TRIPLEX, N=NOT
SPLITSW  DIM       1                  RENT/EXCHANGE SPLIT = 'Y'
.LSTMSW   DIM       1                  LIST MANAGEMENT INDICATOR.
SW30     DIM       1                  DUE FOR BILLING IN 30 DAYS = "Y"
SYSJDATE FORM      5
MRGE     DIM       8
AGEFLAG  FORM      1                  CHECK FOR MAILDATE AGE.
.
SPCL     DIM       2               .SPECIAL INSTRUCTION KEY
.DESC0L1  DIM       47          .FREE FORM SPECIAL INSTRUCTION
.DESC0L2  DIM       47          .FREE FORM SPECIAL INSTRUCTION
.DESC991  DIM       47          .FREE FORM SPECIAL INSTRUCTION
.DESC992  DIM       47          .FREE FORM SPECIAL INSTRUCTION
.DESC981  DIM       47          .FREE FORM SPECIAL INSTRUCTION
.DESC982  DIM       47          .FREE FORM SPECIAL INSTRUCTION
SPCL1    DIM       2           .SPECIAL INSTRUCTION CODE
SPCL2    DIM       2           .SPECIAL INSTRUCTION CODE
SPCL3    DIM       2           .SPECIAL INSTRUCTION CODE
SPCL4    DIM       2           .SPECIAL INSTRUCTION CODE
SPCL5    DIM       2           .SPECIAL INSTRUCTION CODE
SPCL6    DIM       2           .SPECIAL INSTRUCTION CODE
V1       FORM      1
WORK47   DIM       47
WK247    DIM       47
WORK06   DIM       6
.START PATCH 2.3 - ADDED VARS
holdstr  dim     758
line1    dim     127
line2    dim     127
line3    dim     127
line4    dim     127
line5    dim     127
line6    dim     127
line7    dim     127
CARR     INIT     0x7f
carrfill dim      2
lwrobb12 init     "2749-1822-0704-0974-1451-0127-0638"
.END PATCH 2.3 - ADDED VARS
.START PATCH 2.73 - ADDED LOGIC
TEXT1    DIM       47
.END PATCH 2.73 - ADDED LOGIC
.patch2.73.3
.str20	DIM	20
.patch2.73.3
.patch2.80
VARFLAG  FORM      1         Var Flag 1 yes.
DIFF     FORM      10
TENPER   FORM      10
vqty     form      9
.START PATCH 2.81 ADDED LOGIC
NFULCOMP	DIM	55
.END PATCH 2.81 ADDED LOGIC
.patch2.80
.
. .............................................................................
. MAINLINE
. .............................................................................
         TRAP      ABORT IF F5
         MOVE      "EXIT" TO PF5
         MOVE      "NORD013D" TO PROGRAM
         MOVE      "UNBILLED REPORT" TO STITLE
         MOVE      "DATE" TO PF3
         MOVE      "Names In The News" TO COMPNME
         IFNZ      PC
         MOVE      "NINORDU/SRT" TO NORDNAME
         CLOCK     DATE TO DATE
         MOVE      "99/99/99" TO PDATE
         EDIT      DATE TO PDATE
         MOVE      PDATE TO TODAY
         UNPACK    DATE INTO MM,DD,YY
         XIF
.
         IFZ       PC
.         MOVE      "NINORDU.SRT|20.20.30.103:502" TO NORDNAME
         MOVE      "NINORDU.SRT" TO NORDNAME
         CLOCK     DATE TO DATE
         MOVE      DATE TO PDATE
         MOVE      PDATE TO TODAY
         UNPACK    PDATE INTO MM,ANS,DD,ANS,YY
         XIF
         CLOCK     TIME TO TIME
         CALL      PAINT
         CALL      FUNCDISP
         GOTO      TRAPS
NOTHING  RETURN
KEYDATE
         KEYIN     *P10:12,"ENTER DATE ",*ZF,*JR,*EL,*+,MM,*DV,SLASH,DD:
                   *DV,SLASH,YY
         PACK      DATE FROM MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         MOVE      "99/99/99" TO PDATE
         EDIT      DATE TO PDATE
         CALL      PAINT
         CALL      FUNCDISP
         RETURN
TRAPS    TRAP      IO GIVING ERROR NORESET IF IO
         TRAP      KEYDATE IF F3
         DISPLAY   *P1:24,*EL,"OPENING FILES";
         TRAP      RANGE GIVING ERROR NORESET IF RANGE
         TRAP      FORMAT GIVING ERROR NORESET IF FORMAT
         TRAP      PARITY GIVING ERROR NORESET IF PARITY
         MOVE      "MASTER  ON ACCOUNT FILE " TO FERROR
.         open      nordfile,nordname,read
         open      nordfile,nordname
         move      c1 to nordflag
.patch2.80
         SCAN      "VAR" IN COMMENT
         IF        EQUAL
         MOVE      C1 TO VARFLAG
			endif
.patch2.80
.START PATCH 2.6 REPLACED LOGIC
.         SPLOPEN   "g:\DATA\UNBILL.LST"
         PACK      STR35,NTWKPATH1,"unbill.LST"
         SPLOPEN   STR35
.END PATCH 2.6 REPLACED LOGIC
         print     hptop,hpdupl,HPtmsr17,*f
............................................................................................................
.dlh changes of 10jun98

.        OPEN      OUTSP,"NINSPEC",SHARE

         move      c3 to ndatlock
         move      c3 to nmlrlock
         move      c3 to nspelock
............................................................................................................
.
.         OPEN      ACCOUNT1,"NINMOB",SHARE
         MOVE      "                    " TO FERROR
         KEYIN    *P1:24,*EL,"FILES OPEN 30 SEC'S TO CHANGE DATE",*T30,ANS;
.         PAUSE     "30"
         TRAP      NOTHING IF F3
         MOVE      B1 TO PF3
         CALL      FUNCDISP
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         CALL      CVTJUL
         MOVE      JULDAYS TO SYSJDATE
         TRAP      ABORT IF F5
..............................................................................
.for testing only.............................................................
..............................................................................
.
START    MOVE      C0 TO LINES

         MOVE      C0 TO TOTALDOL
         MOVE      "D" TO ANS
.         DISPLAY   *P1:1,*ES,*P24:3,"N A M E S   I N   T H E   N E W S ":
.                   *P18:05,"***   U N B I L L E D   R E P O R T   ***"
         KEYIN     *P28:09,"(D)efault or (A)ll ",*T30,*RV,ANS
         REP       "D1A2" IN ANS
         MOVE      ANS TO JOBBR
         BRANCH    JOBBR OF START1,START1         >1 OR 2 ?
         GOTO      START                            NO!
.
START1
.patch2.80
			compare   c1 to varflag
			if        equal
         MOVE      C2 TO PASS
			else
			move      c1 to pass
			endif
.         move      c2 to pass
.patch2.80
         SUB       TOTALU FROM TOTALU
         SUB       TOTAL FROM TOTAL
         MOVE      C0 TO UNLNC90P
         MOVE      C0 TO UNLNC90
         MOVE      C0 TO UNLNC60
         MOVE      C0 TO UNLNC30
         SUB       AR FROM AR
         SUB       UNBILINC FROM UNBILINC
         SUB       COUNTO FROM COUNTO
         SUB       COUNTO1 FROM COUNTO1
         SUB       COUNTI FROM COUNTI
         SUB       COUNTI1 FROM COUNTI1
         Sub	Printd from Printd
.
GETREC
.         DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G",*HOFF;
.
         CALL      NORD2SEQ
            If	(olrn = "627233")
            call	Debug
            endif
	IF (olrn = "539853")
TEST
		RESET OLRN
	ENDIF
         GOTO      EXIT IF OVER
.START PATCH 2.2 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 2.2 - NEW LOGIC
.START PATCH 2.73 REPLACED LOGIC
;;2.73.2
	move b15 to str15
	move b25 to nmoddesc

;;2.73.2
	packkey	NSEL2FLD,"1",OLRN
	move	"NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if not over
		move	NSEL2NAME,O2DES
;;Patch2.73.2

;
			pack	NMODFLD,NSEL2DESC
			rep	zfill,NMODFLD
			move	"NMODKEY",Location
			pack	KeyLocation,"Key: ",NMODFLD
			call	NMODKEY
			if over
				move	"/M",NMODDESC
.				move	"/M",str20
			else
				call	Trim using NMODDESC
				if (nmoddesc = "")
					move	"/M",NMODDESC
				endif
.				move	NMODDESC,str20
			endif
;
		if (NSEL2SPRICE > C0)
			unpack	NSEL2SPRICE,str5,str3
			call	FormatNumeric using str5,str6
			pack	str9,str6,str3
			pack	str15," @ ",str9,NMODDESC
			call trim using str15
		endif

;;2.73.2
	else
;;2.73.3
.		move	"/M",str20
		move	"/M",NMODDESC
;;2.73.3
		unpack	OPPM,str3,str2
		pack	str6,str3,".",str2
		rep	zfill,str6
		move	str6,NSEL2PRICE
	endif
.END PATCH 2.73 REPLACED LOGIC
         call      rotdial
         ADD       C1 TO COUNTO
         DISPLAY   *P1:24,*EL,COUNTO;
         CMATCH    "B" TO OSTAT    *BILLED?
         GOTO      GETREC IF EQUAL       .YES
.begin patch 1.9
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      getrec IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       Pending order ?
         GOTO      getrec IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       cancelled LCR order ?
         GOTO      Getrec IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
.end patch 1.9
         RESET     CANCODES               .RESET FORM POINTER.
         SCAN      OSTAT IN CANCODES       .CANCELLED?
         GOTO      GETREC IF EQUAL
.         MOVE      "02" TO MM
.         MOVE      "01" TO DD
.         MOVE      "96" TO YY
.         CALL      CVTJUL
.         move      juldays to febdat
.         MOVE      oodtem TO MM
.         MOVE      oodted TO DD
.         MOVE      oodtey TO YY
.         CALL      CVTJUL
.         compare   febdat to juldays
.         goto      getrec if less
         MOVE      OLON TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         MOVE      C1 TO NOWNPATH
         CALL      NOWNKEY
.START PATCH 2.71 ADDED LOGIC
.Start Patch 2.84 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
.	call	Trim using OWNCTN
	call	Trim using OFULLFIL
.End Patch 2.84 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.START PATCH 2.81 REMOVED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"GETREC-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.	endif
.Start Patch 2.84 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
.	if (OWNCTN <> "")
.		pack	COMPFLD6,OWNCTN
.		rep	zfill,COMPFLD6
.		move	C1,COMPPATH
.		move	"GETREC-COMPKEY6",Location
.		pack	KeyLocation,COMPFLD6
.		call	COMPKEY6
.		if over
.			clear	COMPFLD6
.			clear	NFULCOMP
.		else
.			if (COMPSVBFLG <> "T")
.				clear	COMPFLD6
.				clear	NFULCOMP
.			else
..				move	COMPCOMP,NFULCOMP
.			endif
.		endif
.	else	// OWNCTN = ""
.		clear	COMPFLD6
.		clear	NFULCOMP
.	endif
.End Patch 2.84 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.Start Patch 2.84 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
	if (OFULLFIL <> "")
		pack	COMPFLD,OFULLFIL
		call	zfillit using COMPFLD
		move	C1,COMPPATH
		move	"GETREC-COMPKEY",Location
		pack	KeyLocation,COMPFLD
		call	COMPKEY
		if over
			clear	COMPFLD
			clear	NFULCOMP
		else
			move	COMPCOMP,NFULCOMP
		endif
	else	// OFULLFIL = ""
		clear	COMPFLD
		clear	NFULCOMP
	endif
.End Patch 2.84 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.END PATCH 2.81 REMOVED LOGIC
.END PATCH 2.71 ADDED LOGIC
         PACK      STR2 FROM OSALES10,OSALES
         REP       ZFILL IN STR2
.
         MOVE      NO TO LSTMSW
          MOVE      C1 TO Nbrkpath
         PACK      nbrkfld FROM Obrknum,obrkcnt
         REP       ZFILL,nbrkfld
         clear     brsales
         CALL      nbrkkey
         move      c0 to n2
         move      brsales to n2
         compare   c6 to n2
         if        equal
         move       yes to lstmsw
         CLEAR      MCODE         .CLEAR VAR.
         ELSE
         MATCH     "00" TO STR2
         IF        EQUAL
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         IF        NOT EQUAL
         MOVE      YES TO LSTMSW
         ENDIF
         ENDIF
         ENDIF
.         ENDIF
         MOVE      NO TO OVER
PREPOWN  MOVE      OLON TO NOWNFLD
         CALL      NOWNKEY
.START PATCH 2.71 ADDED LOGIC
.Start Patch 2.84 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.			
.	call	Trim using OWNCTN
	call	Trim using OFULLFIL
.End Patch 2.84 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.			
.START PATCH 2.81 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"PREPOWN-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.	endif
.Start Patch 2.84 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
	if (OFULLFIL <> "")
		pack	COMPFLD,OFULLFIL
		call	zfillit using COMPFLD
		move	C1,COMPPATH
		move	"PREPOWN-COMPKEY",Location
		pack	KeyLocation,COMPFLD
		call	COMPKEY
		if over
			clear	COMPFLD
			clear	NFULCOMP
		else
			move	COMPCOMP,NFULCOMP
		endif
	else	// FULLFIL = ""
		clear	COMPFLD
		clear	NFULCOMP
	endif
.End Patch 2.84 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.END PATCH 2.81 REPLACED LOGIC
.END PATCH 2.71 ADDED LOGIC
         CALL      MLRREAD
         BRANCH    PASS OF pass1,pass2,pass3
pass1
         move      c1 to pass
         DISPLAY   *P01:05,"PASS ",pass
         match     yes to lstmsw
         goto      getrec if equal
         cmatch    "B" to mcode
.         goto      getrec if not equal
         goto      pass1a if equal
         cmatch    "A" to mcode
         goto      pass1a if equal
         goto      getrec
pass1a   MOVE      ORTNDTEM TO MM
         MOVE      ORTNDTED TO DD
         MOVE      ORTNDTEY TO YY
         clear     chkdate2
         pack      chkdate2 from ortndtem,ortndted,ortndtey
         move      c0 to chkdate
         move      chkdate2 to chkdate
.begin rev 2.7
.         compare   c0 to chkdate
              if            (chkdate = "0" or chkdate2 = "")
                            if            (ortnnum = "0001")
                            goto          contin                   .take it now it is a reuse
                            else
	              call          usemd
                            endif
              endif

.         call       USEMD IF EQUAL
.         clear     chkdate2
.         pack      chkdate2 from ortndtem,ortndted,ortndtey
.         MATCH     b6 TO chkdate2
.         call       USEMD IF EQUAL
.         call       USEMD IF Eos
.end rev 2.7
         CALL      CVTJUL
         GOTO      CHKDAYS
pass2
         move      c2 to pass
         DISPLAY   *P01:05,"PASS ",pass
	cmatch    yes to lstmsw
         goto      getrec if not equal
.         cmatch    "B" to mcode                .batch bill ? .list manage
.         if        equal                       .yes          .dont care
         clear     mcode
         MOVE      ORTNDTEM TO MM                           .if batch
         MOVE      ORTNDTED TO DD                           .dlh/jd 01jun94
         MOVE      ORTNDTEY TO YY
.patch2.80
			compare   c1 to varflag
			if        equal
			move      c0 to vqty
			move      oqty to vqty
			call      readship
         			scan      "*SHIPPED*",SHIPPED
.begin patch 2.83
.			goto      getrec if not equal
				If        Equal
				move      vqty to oqty
				MOVE      C0 TO N10
				Else
				GOto	Getrec
				Endif
.End patch 2.83
          MOVE      oQTY TO N10
         DIV       C10 INTO N10
         MOVE      N10 TO TENPER
         MOVE      SQUANT TO N10
         MOVE      oQTY TO DIFF
         SUB       N10 FROM DIFF
         COMPARE   C0 TO DIFF
         CALL      NEG IF LESS
         COMPARE   DIFF TO TENPER
         IF         NOT GREATER
			goto      okvar
			elseIf	(Jobbr = c1)
			goto      getrec               else we want all
			endif
			endif
Okvar
.patch2.80
.         MATCH     "00" TO ORTNDTEY
.         call       USEMD IF EQUAL
.         MATCH     " 0" TO ORTNDTEY
.         call       USEMD IF EQUAL
.         cMATCH     b1 TO ORTNDTEY
         clear     chkdate2
         pack      chkdate2 from ortndtem,ortndted,ortndtey
         move      c0 to chkdate
         move      chkdate2 to chkdate
.begin rev 2.7
.         compare   c0 to chkdate
              if            (chkdate = "0" or chkdate2 = "")
                            if            (ortnnum = "0001")
                            goto          contin                   .take it now it is a reuse
                            else
	              call          usemd
                            endif
              endif

.         call       USEMD IF EQUAL
.         clear     chkdate2
.         pack      chkdate2 from ortndtem,ortndted,ortndtey
.         MATCH     b6 TO chkdate2
.         call       USEMD IF EQUAL
.         call       USEMD IF Eos
.end rev 2.7

         CALL      CVTJUL
         goto       chkdays
pass3
.patch2.80
			compare   c1 to varflag
			goto      exit if equal
.patch2.80
         cmatch    yes to lstmsw
         goto      getrec if equal
         cmatch    "B" to mcode
         goto      getrec if equal
         cmatch    "A" to mcode
         goto      getrec if equal
         MOVE      ORTNDTEM TO MM
         MOVE      ORTNDTED TO DD
         MOVE      ORTNDTEY TO YY
         clear     chkdate2
         pack      chkdate2 from ortndtem,ortndted,ortndtey
         move      c0 to chkdate
         move      chkdate2 to chkdate
.begin rev 2.7
.         compare   c0 to chkdate
              if            (chkdate = "0" or chkdate2 = "")
                            if            (ortnnum = "0001")
                            goto          contin                   .take it now it is a reuse
                            else
	              call          usemd
                            endif
              endif

.         call       USEMD IF EQUAL
.         clear     chkdate2
.         pack      chkdate2 from ortndtem,ortndted,ortndtey
.         MATCH     b6 TO chkdate2
.         call       USEMD IF EQUAL
.         call       USEMD IF Eos
.end rev 2.7

         CALL      CVTJUL
         GOTO      CHKDAYS
.
CHKDAYS  MOVE      SYSJDATE TO CHKJUL
         ADD       C7 TO CHKJUL              PLUS 7 DAYS
         SUB       JULDAYS FROM CHKJUL
.         COMPARE    CHKJUL TO C2                DATE GREATER THAN 3 DAYS PRIOR
         COMPARE    CHKJUL TO C0                DATE GREATER THAN 7 DAYS AFTER
.begin patch 2.83
	If	Not Less
	scan      "*SHIPPED*" TO SHIPPED
		If Not Equal
			If	(Jobbr = c1)   .default job Else ALL
			goto	Getrec
			endif
		endif
.         GOTO      GETREC IF NOT LESS               YES
.         GOTO      CONTIN                        NO,PROCESS
	Endif
         GOTO      CONTIN                        NO,PROCESS
.End patch 2.83
.
USEMD    MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         return
.
CONTIN
         CALL      DETAIL
         GOTO      GETREC               .ADDITIONAL CRITERIA FAILED GET NEXT RE
.
DETAIL
.Start Patch #2.0 - replaced var
.         MOVE      C0 TO FORM72
         MOVE      C0 TO FORM92
.end Patch #2.0 - replaced var
         MOVE      C0 TO UNBILAMT
         MOVE      C0 TO UNBILINC
         MOVE      C0 TO FORM52
         MOVE      C0 TO AR
         CALL      MLRREAD
         CALL      READSHIP            .ORDER SHIPPED????
         CALL      READMRG
         CALL      GETCARD
         CALL      READRTN
.Start Patch #2.0 - replaced var
.         SUB       FORM72 FROM FORM72
.         SUB       FORM52 FROM FORM52
.         MOVE      OQTY TO FORM72
.         DIV       THOUS INTO FORM72
.         MOVE      OPPM TO FORM52
.         DIV       HUND INTO FORM52
.         MULT      FORM52 BY FORM72
.         MOVE      FORM72 TO UNBILINC
.
         SUB       FORM92 FROM FORM92
         SUB       FORM52 FROM FORM52
         MOVE      OQTY TO FORM92
         DIV       THOUS INTO FORM92
.START PATCH 2.73 REPLACED LOGIC
.         MOVE      OPPM TO FORM52
.         DIV       HUND INTO FORM52
	MOVE	NSEL2PRICE,FORM52
.END PATCH 2.73 REPLACED LOGIC
         MULT      FORM52 BY FORM92
         MOVE      FORM92 TO UNBILINC
.end Patch #2.0 - replaced var
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES             EXCHANGE ?
         GOTO      OKEX IF EQUAL
         GOTO      RENT
.OKEX - CHECK FOR SPLIT RENTAL/EXCHANGE.
OKEX
.Start Patch #2.0 - replaced var
.         MOVE      C0 TO FORM72
.         MOVE      OEXQTY TO FORM72
.         COMPARE   C0 TO FORM72            PURE EXCHANGE ?
         MOVE      C0 TO FORM92
         MOVE      OEXQTY TO FORM92
         COMPARE   C0 TO FORM92            PURE EXCHANGE ?
.end Patch #2.0 - replaced var
         IF        EQUAL                 YES.
         MOVE      C0 TO QTYCHK
         MOVE      NO TO SPLITSW
         MOVE      OQTY TO QTYCHK
.Start Patch #2.0 - replaced var
.         MOVE      QTYCHK TO FORM72
         MOVE      QTYCHK TO FORM92
.endtch #2.0 - replaced var
         CMATCH    YES TO LSTMSW
         IF        EQUAL
         MOVE      C0 TO UNBILINC
         GOTO      OK
         ELSE
.patch2.73.3
         MOVE      C0 TO UNBILINC
         GOTO      OK
.         GOTO      GETPRICE
.patch2.73.3
         ENDIF
         ELSE
         MOVE      YES TO SPLITSW
         CMATCH    YES TO LSTMSW
         IF        EQUAL
         MOVE      C0 TO UNBILAMT
         GOTO      RENTPART
         ELSE
         MOVE      C0 TO QTYCHK
         MOVE      OEXQTY TO QTYCHK
.Start Patch #2.0 - replaced var
.         MOVE      QTYCHK TO FORM72
         MOVE      QTYCHK TO FORM92
.end patch #2.0 - replaced var
         GOTO      GETPRICE
         ENDIF
         ENDIF
.
GETPRICE UNPACK    JUNEDAT INTO MM,DD,YY
         CALL      CVTJUL           .CONVERT JUNE 1ST'S DATE TO JULIAN
         MOVE      JULDAYS TO JUNDATE    .SAVE RESULT
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         CALL      CVTJUL           .CONVERT TODAY'S  DATE TO JULIAN
         MOVE      JULDAYS TO MDATE    .SAVE RESULT
         COMPARE   JUNDATE TO MDATE
         IF        NOT GREATER
         MOVE      C8 TO FORM52
         GOTO      CALCE
         ENDIF
         COMPARE   QTYCHK TO TENDOLL
         IF        NOT LESS
         MOVE      C10 TO FORM52
         GOTO      CALCE
         ENDIF
         COMPARE   QTYCHK TO NINEDOLL
         IF        NOT LESS
         MOVE      C9 TO FORM52
         GOTO      CALCE
         ENDIF
         COMPARE   SEVDOLL TO QTYCHK
         IF        NOT LESS
         MOVE      C7 TO FORM52
         ENDIF
         MOVE      C8 TO FORM52
CALCE
.Start Patch #2.72
        reset      lwrobb12
        match      "0638" to obrknum             .lwrobb
        if         equal
        scan       omlrnum in lwrobb12
        if         equal
        move       "12" to form52
        endif
        endif
.End Patch #2.72
.Start Patch #2.0 - replaced var
.         DIVIDE    THOUS INTO FORM72
.         MULTIPLY  FORM52 BY FORM72
.         MOVE      FORM72 TO UNBILAMT
         DIVIDE    THOUS INTO FORM92
         MULTIPLY  FORM52 BY FORM92
         MOVE      FORM92 TO UNBILAMT
.end Patch #2.0 - replaced var
         CMATCH    YES TO SPLITSW
         IF        EQUAL
         GOTO      RENTPART
         ELSE
.Start Patch #2.0 - replaced var
.         MOVE      FORM72 TO UNBILINC
         MOVE      FORM92 TO UNBILINC
.end Patch #2.0 - replaced var
         GOTO      OK
         ENDIF
.
.Start Patch #2.0 - replaced var
.RENTPART MOVE      C0 TO FORM72          SPLIT RENT/EXCHANGE
.         MOVE      C0 TO N7
.         MOVE      OQTY TO FORM72
.         MOVE      OEXQTY TO N7
.         SUBTRACT  N7 FROM FORM72           GET RENTAL PORTION
.         MULT      ".001" BY FORM72
.         MOVE      "65.00" TO FORM52          .ESTIMATED $.   (USE DATACARD?)
..         CALL      GETCARD
.         MULT      FORM52 BY FORM72
.         CMATCH    YES TO LSTMSW
.         IF        EQUAL
.         MULT      ".1" BY FORM72
RENTPART MOVE      C0 TO FORM92          SPLIT RENT/EXCHANGE
         MOVE      C0 TO N9
         MOVE      OQTY TO FORM92
         MOVE      OEXQTY TO N9
         SUBTRACT  N9 FROM FORM92           GET RENTAL PORTION
         MULT      ".001" BY FORM92
         MOVE      "65.00" TO FORM52          .ESTIMATED $.   (USE DATACARD?)
.         CALL      GETCARD
         MULT      FORM52 BY FORM92
         CMATCH    YES TO LSTMSW
         IF        EQUAL
         MULT      ".1" BY FORM92
.end Patch #2.0 - replaced var
         ELSE
.        display     *p1:24,"brk",obrknum
.begin patch 2.82
.         match      "0192" to obrknum
         match      "0**0" to obrknum           .cheating need to cleanup later
.end patch 2.82
         IF         EQUAL
.         display    *p1:24,*el,"EPSILON!!!!!!!!!"
         match      "20" to commper
           if         equal
.Start Patch #2.0 - replaced var
.           MULT      ".1" BY FORM72
           MULT      ".1" BY FORM92
.end Patch #2.0 - replaced var
           goto      ok
           ENDIF
         match      "30" to commper
            if         equal
.Start Patch #2.0 - replaced var
.            MULT      ".2" BY FORM72
..           display    *p1:24,*el,"form72 ",form72,b1,commper,*w2
            MULT      ".2" BY FORM92
.           display    *p1:24,*el,"form92 ",form92,b1,commper,*w2
.end Patch #2.0 - replaced var
            goto      ok
            endif
         ENDIF
.oddball use 10% commission
.Start Patch #2.0 - replaced var
.         MULT      ".1" BY FORM72
         MULT      ".1" BY FORM92
.end Patch #2.0 - replaced var
         goto      ok
         endif
.
        CMATCH    YES TO LSTMSW
         IF        EQUAL
.Start Patch #2.0 - replaced var
.         MULT      ".1" BY FORM72
         MULT      ".1" BY FORM92
.end Patch #2.0 - replaced var
         ELSE
         move       c0 to n32      .DLH USE Datacard info 09Jul98
         move       commper to n32
         mult       ".01" by n32
.Start Patch #2.0 - replaced var
.         mult       n32,form72
..         MULT      ".2" BY FORM72            ESTIMATED LR INCOME.
.         ENDIF
.         ADD       FORM72 TO UNBILAMT
         mult       n32,form92
.         MULT      ".2" BY FORM92            ESTIMATED LR INCOME.
         ENDIF
         ADD       FORM92 TO UNBILAMT
.end Patch #2.0 - replaced var
         MOVE      UNBILAMT TO UNBILINC
         GOTO      OK
.
GETCARD  MOVE      OLNUM TO NDATFLD
.         DISPLAY   *P1:24,"GETCARD",*W2;
         CLEAR     EXCLPRT
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY
         RETURN    IF OVER
         CMATCH    "C" TO ELSTCDE
         IF        EQUAL
         MOVE      "EXCL" TO EXCLPRT
         ENDIF
;begin patch 2.75
               call           Trim using Commper
;ARGH
;end patch 2.75
.         DISPLAY   *P1:24,TEXT1,*W2;
.START PATCH 2.73 REPLACED LOGIC
.         parse     textdata into text1 using " ~09",noskip,blankfill
.         SCAN      "EXCHANGE ONLY" IN TEXT1
.         RETURN    IF EQUAL                 NO USABLE $ RETURN
.         RESET     TEXT1
.         SCAN      "$" IN TEXT1
.         RETURN    IF NOT EQUAL        NO USABLE $ RETURN
.         BUMP      TEXT1 BY 1
.         PACK      STR2 FROM TEXT1
.         MOVE      STR2 TO FORM52
..         DISPLAY   *P1:24,STR2,B1,FORM52,*W2;
.         SCAN      "$" IN TEXT1        *DO WE HAVE CORRECT PRICE?
.         RETURN    IF NOT EQUAL            .YES.
.         CLEAR     STR2
.         BUMP      TEXT1 BY 1
.         PACK      STR2 FROM TEXT1       .NO, NOW WE DO!
.         MOVE      STR2 TO FORM52
	if (NDATCONV = "1")
.		if (NDATEXCH <> "1")
			pack	NSELFLD1,"01X",LSTNUM
			pack	NSELFLD2,"021XBASE"
			move	"NSELAIM",Location
			pack	KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
			call	NSELAIM
			if not over
				if (NSELEXC <> "2")
					move	C0,FORM52
					move	NSELPRICE,FORM52
				endif
			else
				goto DataCheckText
			endif
.		endif
	else
DataCheckText
		pack	NTXTFLD,LSTNUM,"1"
		move	"NTXTKEY",Location
		pack	KeyLocation,"Key: ",NTXTFLD
		call	NTXTKEY
		if not over
			move	NTXTTEXT,text1
			SCAN	"EXCHANGE ONLY" IN TEXT1
			RETURN IF EQUAL                 NO USABLE $ RETURN
			RESET	TEXT1
			SCAN	"$" IN TEXT1
			RETURN IF NOT EQUAL        NO USABLE $ RETURN
			BUMP	TEXT1 BY 1
			PACK	STR2 FROM TEXT1
			MOVE	STR2 TO FORM52
			SCAN	"$" IN TEXT1        .DO WE HAVE CORRECT PRICE?
			RETURN IF NOT EQUAL            .YES.
			CLEAR	STR2
			BUMP	TEXT1 BY 1
			PACK	STR2 FROM TEXT1       .NO, NOW WE DO!
			MOVE	STR2 TO FORM52
		else
			clear	text1
		endif
	endif
.END PATCH 2.73 REPLACED LOGIC
.         DISPLAY   *P1:24,STR2,B1,FORM52,*W2;
         RETURN
RENT     CMATCH    YES TO LSTMSW             LIST MANAGEMENT?
         IF        EQUAL
         MULT      ".1" BY UNBILINC            YES
         ELSE
.DLH 05Aug98 this section was missing Epsilon code
.and was using the variable form72 instead of unbilinc.   so rentals reflected AR instead of
.LR income on report details and totals.

        display     *p1:24,"brk",obrknum

.begin patch 2.81
.         match      "0192" to obrknum
         match      "0**0" to obrknum           .cheating need to cleanup later
.end patch 2.81
           IF         EQUAL
           display    *p1:24,*el,"EPSILON!!!!!!!!!"
           match      "20" to commper
            if         equal
            MULT      ".1" BY UNbilinc
            goto      ok
            ENDIF

           match      "30" to commper
           if         equal
           MULT      ".2" BY UNbilinc
           display    *p1:24,*el,"Unbilinc ",Unbilinc,b1,commper
           goto      ok
           ENDIF
.oddball use 10% commission
          MULT      ".1" BY Unbilinc
          goto      ok
          endif

         move       c0 to n32      .DLH USE Datacard info 09Jul98
         move       commper to n32
         mult       ".01" by n32
         mult       n32,unbilinc
.         MULT      ".2" BY FORM72            ESTIMATED LR INCOME.
         ENDIF
..
OK
.START PATCH 2.71 REPLACED LOGIC
.         SCAN      "TDMC" IN OWNCTN
.         IF        EQUAL
.         GOTO      TDMCYES1
.                   ELSE
.         GOTO      USEMD1
.         ENDIF
.
.Start Patch 2.84 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
.	scan	"TDMC",NFULCOMP
	match	"009406",OFULLFIL
.End Patch 2.84 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.			
	if equal
		goto TDMCYES1
	else
		goto USEMD1
	endif
.END PATCH 2.71 REPLACED LOGIC
TDMCYES1
.         MATCH     "00" TO ORTNDTEY
.         GOTO      USEMD1 IF EQUAL
.         MATCH     " 0" TO ORTNDTEY
.         GOTO      USEMD1 IF EQUAL
.         cMATCH     b1 TO ORTNDTEY
.         GOTO      USEMD1 IF Eos
         clear     chkdate2
         pack      chkdate2 from ortndtem,ortndted,ortndtey
         move      c0 to chkdate
         move      chkdate2 to chkdate
         compare   c0 to chkdate
         call       USEMD1 IF EQUAL
.         clear     chkdate2
.         pack      chkdate2 from ortndtem,ortndted,ortndtey
         MATCH     b6 TO chkdate2
         call       USEMD1 IF EQUAL
         call       USEMD1 IF Eos
         MOVE      ORTNDTEM TO MM
         MOVE      ORTNDTED TO DD
         MOVE      ORTNDTEY TO YY
         CALL      CVTJUL
         GOTO      CHKDAY1
USEMD1   MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         CALL      CVTJUL
.
CHKDAY1  MOVE      SYSJDATE TO CHKJUL
.
         SUBTRACT  CHKJUL FROM JULDAYS
         MOVE      NO TO SW30
.
         COMPARE   C31 TO JULDAYS
         GOTO      BILL30 IF LESS          .SHOULD BE BILLED THIS MONTH.
.
         COMPARE   "61" TO JULDAYS         .SHOULD BE BILLED BY END NEXT MON.
         GOTO      BILL60 IF LESS
.
         COMPARE   "91" TO JULDAYS         .
         GOTO      BILL90 IF LESS
         ADD       UNBILINC TO UNLNC90P
         GOTO      OK1
BILL30   ADD       UNBILINC TO UNLNC30
         MOVE      YES TO SW30
         GOTO      OK1
.
BILL60   ADD       UNBILINC TO UNLNC60
         GOTO      OK1
.
BILL90   ADD       UNBILINC TO UNLNC90
         GOTO      OK1
.
.Start Patch #2.0 - replaced var
.OK1      MOVE      UNBILINC TO FORM72
.         ADD       FORM72 TO TOTALU
.         ADD       FORM72 TO TOTAL
.         ADD       C1 TO COUNTO1
.         MOVE      FORM72 TO AR
OK1      MOVE      UNBILINC TO FORM92
         ADD       FORM92 TO TOTALU
         ADD       FORM92 TO TOTAL
         ADD       C1 TO COUNTO1
         MOVE      FORM92 TO AR
.end Patch #2.0 - replaced var
.
CHECK1   MATCH     YES TO SW30               DUE IN 30?
         IF        EQUAL                             YES
         MATCH     YES TO LSTMSW            LIST MANAGEMENT?
         IF        EQUAL
         ADD       UNBILINC TO LSTMTOT       YES
         ELSE
         RESET     EXCODES                        NO
         SCAN      OELCODE IN EXCODES           OK ITS BROKERAGE, EXCHANGE?
         IF        EQUAL
         ADD       UNBILINC TO BRKEXTOT          YES.
         ELSE
         ADD       UNBILINC TO BRKRNTOT          NO ITS RENTAL.
         ENDIF
         ENDIF
         ENDIF
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         GOTO      CHECKOK IF EQUAL
         CLEAR     EXCHANGE
         GOTO      DISSCOM
CHECKOK
         MOVE      "EXCHANGE" TO EXCHANGE
DISSCOM
         CLEAR     COMSLCT
         CMATCH    "C",OCOMSLCT
         IF        EQUAL
         MOVE      "COMSELECT" TO COMSLCT
         ENDIF
         CMATCH    "L",OCOMSLCT
         IF        EQUAL
         MOVE      "LIFESTYLE" TO COMSLCT
         ENDIF
         CMATCH    "I",OCOMSLCT
         IF        EQUAL
         MOVE      "IC SYSTEMS" TO COMSLCT
         ENDIF
         NORETURN
         
.check for maildate age if >= 2wks flag it.   dlh 20may93.
         MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         CALL      CVTJUL
         MOVE      C0 TO AGEFLAG
         MOVE      SYSJDATE TO CHKJUL
.begin patch 2.83    if maildate is past at all flag
	if	(Juldays <=SysJDate)
	move	c1 to AgeFlag
	endif
.         SUBTRACT  JULDAYS FROM CHKJUL
.         COMPARE   "13" TO CHKJUL
.         IF        GREATER
.         MOVE      C1 TO AGEFLAG
.         ENDIF
.end patch 2.83    if maildate is past at all flag
         GOTO      PRINT
.
MLRREAD  MOVE      C1 TO NMLRPATH
         PACK      MKEY FROM OMLRNUM,OCOBN
         REP       ZFILL,MKEY
         CALL      NMLRKEY
         CMATCH    "B" TO MCODE        .BATCH BILL ?
         IF         NOT EQUAL         .NO
         cmatch    "A" to mcode
         IF         NOT EQUAL         .NO
         CLEAR      MCODE         .CLEAR VAR.
         ENDIF
         endif
         RETURN
.
.
READSHIP CLEAR     SHIPPED
         MOVE      OLRN TO NSHPFLD
         CALL      NSHPKEY
         RETURN    IF OVER
         MOVE      "*SHIPPED*" TO SHIPPED
.Start Patch #2.0 - replaced var
.         MOVE      C0 TO N7
.         MOVE      SQUANT TO N7
.         COMPARE   C0 TO N7
         MOVE      C0 TO N9
         MOVE      SQUANT TO N9
         COMPARE   C0 TO N9
.end Patch #2.0 - replaced var
         IF        NOT EQUAL
         MOVE      SQUANT TO OQTY
         ENDIF
         RETURN
READRTN  MATCH     ORTNNUM TO NRTNFLD
         IF        NOT EQUAL
         MOVE      ORTNNUM TO NRTNFLD
         CALL      NRTNKEY
         ENDIF
         RETURN
READMRG
         CLEAR     MRGE
         MOVE      OLRN TO NMRGFLD
         rep       zfill in nmrgfld
         MOVE      C1 TO NMRGPATH
         CALL      NMRGKEY
         RETURN    IF OVER
         MOVE      "*MERGED*" TO MRGE
         RETURN
.
BALAN
.MONEY ON ACCOUNT ?     .ADD IN THE FUTURE??????
         MOVE      MKEY TO CHKMLR
         RESET     MKEY TO 4
         APPEND    Z3 TO MKEY
         RESET     MKEY
         MOVE      MKEY TO NMObFLD
         CALL      NMOBKEY
.         READ      ACCOUNT1,MKEY;MKEY,BALANCE
         COMPARE   C0 TO BALANCE
         GOTO      BALAN1 IF EQUAL
         GOTO      BALAN1 IF NOT LESS
         MULT      SEQ BY BALANCE
         ADD       BALANCE TO TOTALDOL
BALAN1   MOVE      NO TO OVER
         CALL      NMOBKS
.        READKS    ACCOUNT1;MKEY,BALANCE
         GOTO      PRINT IF OVER
         CMATCH    YES TO OVER
         GOTO      PRINT IF EQUAL
         MOVE      MKEY TO COMPMLR
         MATCH     CHKMLR TO COMPMLR
         GOTO      BALAN1 IF NOT EQUAL
         MULT      SEQ BY BALANCE
         ADD       BALANCE TO TOTALDOL
.
............................................................
.
CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
         RESET     MPCHARS
         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
         GOTO      CVTMP IF EQUAL                YES.
         RESET     CVTFLD                        NO.
         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
         RETURN    IF EQUAL                      ITS OK.
FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",OLRN
         NORETURN                                POP THE STACK.
         GOTO      GETREC                       GO BACK TO READ.
CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
         RESET     CVTFLD
         TYPE      CVTFLD                        VALID NUMERIC?
         GOTO      FORMERR IF NOT EQUAL          NO.
         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
         MULTIPLY  SEQ BY NUM10               CHANGE TO MINUS.
         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
         RETURN
.
.......................................................................
HEADER   ADD       C1 TO PAGE
         branch    pass of hd1,hd2,hd3
hd1      PRINT     *F,*l,*l,*1,"CONFIDENTIAL",*16,PROGRAM,*37,"N I N   U N B I L L":
                   " E D   'Batch'   O R D E R S   A S   O F",b2,"DATE:",PDATE:
                   *121,"PAGE ## ",PAGE
         goto      hdx
hd2      PRINT     *F,*l,*l,*1,"CONFIDENTIAL",*16,PROGRAM,*37,"N I N   U N B I L L":
                   " E D   'Broker'   O R D E R S   A S   O F",b2,"DATE:",PDATE:
                   *121,"PAGE ## ",PAGE
         goto      hdx
hd3     PRINT     *F,*l,*l,*1,"CONFIDENTIAL",*16,PROGRAM,*37,"N I N   U N B I L L":
                   " E D   O R D E R S   A S   O F",*89,"DATE:",PDATE:
                   *121,"PAGE ## ",PAGE
         goto      hdx
.hdx      print     *L,*1,"ORDER",*34,"MAILER NAME",*59,"MERGE",*68,"LIST":
hdx      print     *L,*1,"ORDER",*34,"MAILER NAME":
                   *87,"LIST":
                   *114,"MAIL",*124,"RETURN":
                   *L,*1,"DATE",*11,"MLR##",*26,"LR":
.                   *40,"OFFER",*59,"PURGE##",*68,"OWNER##":
                   *34,"OFFER",*81,"##":
                   *87,"OWNER NAME",*114,"DATE",*124:
                   "DATE":
                   *n,*1,*RPTCHAR,DASH:132
         MOVE      C7 TO LINES
         RETURN
.......................................................................
PRINT
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         COMPARE   C0 TO LINES
         CALL      HEADER IF EQUAL
         CLEAR     BRCOMP
         CLEAR     NBRKFLD
         PACK      NBRKFLD FROM OBRKNUM,OBRKCNT
         CALL      NBRKKEY
         if         over
         move      mccto to brcomp
         endif
.Start Patch #2.0 - ADD CENTURY
.         PRINT     *40,brcomp,*59,OMLRKY,*86,"RTN-TO: ",rtcomp:
.                   *L,*7,OODTEM,SLASH,OODTED,SLASH,OODTEY:
.                   *18,OMLRNUM,SLASH,OCOBN,B1,*27,hpbon,MCODE,hpboff,*29,OLRN:
.                   *40,ORDcNAME,*68,OLON,*86,OWNOCPY,*114,OMDTEM,SLASH:
.                   OMDTED,SLASH,OMDTEY,*124,ORTNDTEM,SLASH,ORTNDTED:
.                   SLASH,ORTNDTEY;
.         COMPARE   C1 TO AGEFLAG
.         IF        EQUAL
.         PRINT     *113,hpbon,STAR,OMDTEM,SLASH,OMDTED,SLASH,OMDTEY,STAR,hpboff;
.
		  add        c1 to printd
.        PRINT     *34,brcomp,*59,OMLRKY,*86,"RTN-TO: ",rtcomp:
	Call	Trim using RtCOmp
	IF	(rtcomp <> "")
	call	Debug
	PRint	*86,"RTN-TO: ",rtcomp
	Add	C1,Lines
	endif
	If	(lines >= Pbreak)
	call	Header
	call	Overrun
	endif
        PRINT     *1,"MP##",Omlrky,*34,brcomp,*81,OLON,*86,OWNOCPY,*113,OMDTEM,SLASH:
                   OMDTED,SLASH,OMDTEC,OMDTEY,*124,ORTNDTEM,SLASH,ORTNDTED:
                   SLASH,ORTNDTEC,ORTNDTEY;
	Add	C1,Lines
         COMPARE   C1 TO AGEFLAG
         IF        EQUAL
         PRINT     *112,hpbon,STAR,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY,STAR,hpboff;
.END Patch #2.0 - ADD CENTURY
         ENDIF
         call	      Trim using NFULCOMP
	If	(lines >= Pbreak)
	call	Header
	call	Overrun
	endif
	IF   	(ELSTCDE = "C")
         	Print     	*L,*1,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY:
                   	*12,OMLRNUM,SLASH,OCOBN,B1,*21,hpbon,MCODE,hpboff,*23,OLRN:
                   	*34,ORDcNAME,*86,*ll,hpbon,NFULCOMP,hpboff;
            Else
         	Print     	*L,*1,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY:
                   	*12,OMLRNUM,SLASH,OCOBN,B1,*21,hpbon,MCODE,hpboff,*23,OLRN:
                   	*34,ORDcNAME,*86,*ll,NFULCOMP;
            EndIF
	Add	C1,Lines
.START PATCH 2.2 - REPLACED LOGIC, OODES --> OFDESC
.         PRINT     *L,*8,EXCHANGE,*19,SHIPPED,*30,MRGE,*40,OODES,B1,EXCLPRT:
.                   *86,OWNCTN,B3,FORM52,"/M",*119,UNBILINC
.START PATCH 2.71 REPLACED LOGIC
.         PRINT     *L,*8,EXCHANGE,*19,SHIPPED,*30,MRGE,*40,OFDESC,B1,EXCLPRT:
.                   *86,OWNCTN,B3,FORM52,"/M",*119,UNBILINC
.         call	      Trim using NFULCOMP
.patch2.73.3
	If	(lines >= Pbreak)
	call	Header
	call	Overrun
	endif
.START PATCH 2.85 REPLACED LOGIC
         PRINT     *L,*12,HpBon,SHIPPED,HpBoff,*23,MRGE,*34,OFDESC:
                   *81,HPBON,ExclPRt,HPBOFF,*86,FORM52,NMODDESC,HPBON,EXCHANGE,HPBOFF,*119,UNBILINC
.         PRINT     *L,*1,EXCHANGE,*12,HpBon,SHIPPED,HpBoff,*23,MRGE,*34,OFDESC:
.                   *81,HPBON,ExclPRt,HPBOFF,*86,FORM52,NMODDESC,*119,UNBILINC
.END PATCH 2.85 REPLACED LOGIC
.                   *86,*ll,NFULCOMP,B3,FORM52,NMODDESC,*119,UNBILINC
.                   *86,*ll,NFULCOMP,B3,FORM52,"/M",*119,UNBILINC
	Add	c1,lines
.patch2.73.3
.                    B3,FORM52,"/M",*119,UNBILINC
.END PATCH 2.71 REPLACED LOGIC
.end PATCH 2.2 - REPLACED LOGIC, OODES --> OFDESC
         ADD       UNBILINC TO UNBILTOT
         IFZ       PC
.patch2.73.2
	If	(lines >= Pbreak)
	call	Header
	call	Overrun
	endif
         PRINT     *1,"LIST - ",O1DES,B1,O2DES,B1,HPBON,str15,HPBOFF,b1,COMSLCT,*109,"QUANTITY ";
	Add	c1,lines
.         PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,HPBON,str15,HPBOFF,b1,COMSLCT,*109,"QUANTITY ";
.         PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,*109,"QUANTITY ";
.patch2.73.2
.         PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,nfulcomp,*115,"QTY ";
.Start Patch #2.0 - replaced var
.         MOVE      "ZZZZZZZ" TO STR7
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         EDIT      N7 TO STR7
.         PRINT     STR7;
.         MOVE      "ZZZZZZZ" TO STR7
.         MOVE      C0 TO N7
.         MOVE      OEXQTY TO N7
.         EDIT      N7 TO STR7
.         PRINT     "/",STR7
.
         MOVE      "ZZZZZZZZZ" TO STR9
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         EDIT      N9 TO STR9
         PRINT     STR9;
         MOVE      "ZZZZZZZZZ" TO STR9
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         EDIT      N9 TO STR9
         PRINT     "/",STR9
.end Patch #2.0 - replaced var
         clear     media
         CMATCH    B1,OFOCODE
         GOTO      DISMED2 IF NOT EQUAL
         GOTO      ship
DISMED2  MOVE      C0 TO N2
         TYPE      OFOCODE
         GOTO      MED10 IF NOT EQUAL
         MOVE      OFOCODE TO N2
         GOTO      DIS27
MED10    REP       "A0B1C2D3E4F5G6H7I8J9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED20 IF NOT EQUAL
         MOVE      OFOCODE TO N2
         ADD       C10 TO N2
         GOTO      DIS27
MED20    REP       "K0L1M2N3O4P5Q6R7S8T9" IN OFOCODE
         TYPE      OFOCODE
         GOTO      MED30 IF NOT EQUAL
         MOVE      OFOCODE TO N2
         ADD       "20" TO N2
         GOTO      DIS27
MED30    REP       "U0V1X2Y3Z4" IN OFOCODE
         MOVE      OFOCODE TO N2
         ADD       "30" TO N2
DIS27    MOVE      MED0 TO MEDIA
.START PATCH 2.5 REPLACED LOGIC
.         LOAD      MEDIA FROM N2 OF MED1,MED2,MED3,MED4,MED5:
.                   MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
.                   MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
.                   MED23,MED24,MED25
         LOAD      MEDIA FROM N2 OF MED1,MED2,MED3,MED4,MED5:
                   MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
                   MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
                   MED23,MED24,MED25,MED26,MED27,MED28,MED29
.END PATCH 2.5 REPLACED LOGIC
ship     move      c0 to n2
         clear     shipdesc
.Start Patch #2.0 - INCREASED VAR
.         cmatch    b1 to oshp
         match    b2 to oshp
.END Patch #2.0 - INCREASED VAR
         goto      shipblnk if equal
         MOVE      OSHP TO N2
         move      ship0 to shipdesc
         load      shipdesc from N2 OF SHIP1,SHIP2,SHIP3,SHIP4,SHIP5,SHIP6:
                            SHIP7,SHIP8,SHIP9,ship10
         goto     shipprt
SHIPBLNK move     "NOTHING" to shipdesc
.
shipprt
.         COMPARE   PBREAK TO LINES
.         if        not less
	If	(Lines >= Pbreak)
         CALL      HEADER
         call      overrun
         endif
         print     *1,"Ship: ",MEDIA,*60,"Via: ",shipdesc
.         print     *4,MEDIA,*60,"Via: ",shipdesc
         add        c1 to lines
         move       c0 to n2
         move       onetper to n2
         compare    c0 to n2
         if         not equal
           cmatch    no to onetfm
            if        equal
.         	COMPARE   PBREAK TO LINES
.         		if        not less
		If	(Lines >= Pbreak)
         		CALL      HEADER
         		call      overrun
         		endif
            print     *4,"Per List Owner - Gross Billing":
                      " No Deductionss",*l
         add        c1 to lines
            endif
          cmatch     "F" to onetfm
           if         not equal
.         	COMPARE   PBREAK TO LINES
.         		if        not less
		If	(Lines >= Pbreak)
         		CALL      HEADER
         		call      overrun
         		endif
           print      *4,"Mailer Guarantees ",onetper:
                      "% payment on Gross Names Shipped":
                      *n,*4,"& will pay $",onetrc,"/m running charge on":
                      " unused names."
         add        c2 to lines
          else
	If	(Lines >= Pbreak)
         		CALL      HEADER
         		call      overrun
         		endif
           print      *4,onetper,"% Volume Discount":
                      *n
         add        c1 to lines
           endif
         else
         endif
.START PATCH 2.3 - OBSOLETE VARS
.         CLEAR     DESC0L1
.         CLEAR     DESC0L2
.         CLEAR     DESC991
.         CLEAR     DESC992
.         CLEAR     DESC981
.         CLEAR     DESC982
.END PATCH 2.3 - OBSOLETE VARS
.10jun98 DLH use include
.        filepi    1;outsp


.         READ      OUTSP,OLRN;WORK06,DESC0L1,DESC0L2,DESC991,DESC992:
.                   DESC981,DESC982
.
.START PATCH 2.73.1 - ADDED LOGIC
	clear	line1
	pack	NSEL3FLD1,"01X1",OLRN
	move	"NSEL3AIM",Location
	pack	KeyLocation,"Key: ",NSEL3FLD1
	call	NSEL3AIM
	loop
		until over
		clear	taskname
		if (NSEL3CODE = "A")
			pack	NADDFLD,OLNUM,NSEL3NUM
			move	"NADDKEY",Location
			pack	KeyLocation,"Key: ",NADDFLD
			call	NADDKEY
			if not over
				pack	NREFFLD,"A",NADDNUM
				move	"NREFKEY",Location
				pack	KeyLocation,"Key: ",NREFFLD
				call	NREFKEY
				call	Trim using NREFDESC
.START PATCH 2.73.3 REPLACED LOGIC
.				if (NADDPRICE = 0)
				if (NSEL3PRICE = 0)
.END PATCH 2.73.3 REPLACED LOGIC
					pack	taskname,NREFDESC
				else
					pack	NMODFLD,NADDDESC
					rep	zfill,NMODFLD
					move	"NMODKEY",Location
					pack	KeyLocation,"Key: ",NMODFLD
					call	NMODKEY
					call	Trim using NMODDESC
.START PATCH 2.73.3 REPLACED LOGIC
.					unpack	NADDPRICE,str5,str3
					unpack	NSEL3PRICE,str5,str3
.END PATCH 2.73.3 REPLACED LOGIC
					call	FormatNumeric using str5,str6
					pack	str9,str6,str3
					pack	taskname,NREFDESC,AT,str9,NMODDESC
				endif
			endif
		elseif (NSEL3CODE = "L")
			pack	NSLTFLD,OLNUM,NSEL3NUM
			move	"NSLTKEY",Location
			pack	KeyLocation,"Key: ",NSLTFLD
			call	NSLTKEY
			if not over
				pack	NREFFLD,"L",NSLTNUM
				move	"NREFKEY-2",Location
				pack	KeyLocation,"Key: ",NREFFLD
				call	NREFKEY
				call	Trim using NREFDESC
.START PATCH 2.73.3 REPLACED LOGIC
.				if (NSLTPRICE = 0)
				if (NSEL3PRICE = 0)
.END PATCH 2.73.3 REPLACED LOGIC
					pack	taskname,NREFDESC
				else
					pack	NMODFLD,NSLTDESC
					rep	zfill,NMODFLD
					move	"NMODKEY-2",Location
					pack	KeyLocation,"Key: ",NMODFLD
					call	NMODKEY
					call	Trim using NMODDESC
.START PATCH 2.73.3 REPLACED LOGIC
.					unpack	NSLTPRICE,str5,str3
					unpack	NSEL3PRICE,str5,str3
.END PATCH 2.73.3 REPLACED LOGIC
					call	FormatNumeric using str5,str6
					pack	str9,str6,str3
					pack	taskname,NREFDESC,AT,str9,NMODDESC
				endif
			endif
		endif
		movelptr line1,result
		movelptr taskname,howmany
		add	result,howmany,N10
		if (N10 > 124)
			reset	line1
			call	SPCLNSTO
			clear	line1
		elseif (taskname <> "")
			append	taskname,line1
			append	", ",line1
		endif
		move	"NSEL3KG",Location
		call	NSEL3KG
	repeat
	if (line1 <> "")
		reset	line1
		call	SPCLNSTO
		clear	line1
	endif
.END PATCH 2.73.1 - ADDED LOGIC
         move      olrn to nspefld
         call      nspekey
.end 10jun98
...................................................................................
         pack      holdstr,DESC001,DESC002
         pack      carrfill,CARR,B1
         rep       carrfill,holdstr
         call      TRIM using holdstr
         call      PARSITUP using line1,holdstr,C1
         call      PARSITUP using line2,holdstr,C1
         call      PARSITUP using line3,holdstr,C1
         call      PARSITUP using line4,holdstr,C1
         call      PARSITUP using line5,holdstr,C1
         call      PARSITUP using line6,holdstr,C1
         call      PARSITUP using line7,holdstr,C1
         CALL      SPCLNSTO                           SPEC INSTRUC ROUTINE
         Clear     Line1
         MOVE      line2,line1
         CALL      SPCLNSTO
         Clear     Line1
         MOVE      line3,line1
         CALL      SPCLNSTO
         Clear     Line1
         MOVE      line4,line1
         CALL      SPCLNSTO
         Clear     Line1
         MOVE      line5,line1
         CALL      SPCLNSTO
         Clear     Line1
         MOVE      line6,line1
         CALL      SPCLNSTO
         Clear     Line1
         MOVE      line7,line1
         CALL      SPCLNSTO
         Clear     Line1
         Clear     Line2
         Clear     Line3
         Clear     Line4
         Clear     Line5
         Clear     Line6
         Clear     Line7
         GOTO      ADDLNE
.
. ROUTINE FOR SPECIAL INSTRUCTION PRINT
.
SPCLNSTO
.line1 will be cleared in PARSITUP if necessary.  Calling Trim will reset past values!!!
         if (line1 <> "")
		If	(Lines >= Pbreak)
            	CALL      HEADER
            	cALL 	Overrun
            	endif
                   PRINT     *4,"**",*7,line1
                   add       c1 to lines
         endif
         return
.END PATCH 2.3 - REPLACED LOGIC
         XIF
         IFNZ      PC
.Patch2.73.3
	If	(Lines >= Pbreak)
           	CALL      HEADER
           	cALL 	Overrun
           	endif
         PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,HPBON,str15,HPBOFF,b1,COMSLCT,*109,"QUANTITY ";
.         PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,*109,"QUANTITY ";
.Patch2.73.3
.Start Patch #2.0 - replaced var
.         MOVE      "ZZZZZZZ" TO STR7
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         EDIT      N7 TO STR7
.         PRINT     STR7;
.         MOVE      "ZZZZZZZ" TO STR7
.         MOVE      C0 TO N7
.         MOVE      OEXQTY TO N7
.         EDIT      N7 TO STR7
.         PRINT     "/",STR7:
.                   *L,*1,"------------------------------":
.                   "------------------------------":
.                   "------------------------------":
.                   "------------------------------":
.                   "------------"
         MOVE      "ZZZZZZZZZ" TO STR9
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         EDIT      N9 TO STR9
         PRINT     STR9;
         MOVE      "ZZZZZZZZZ" TO STR9
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         EDIT      N9 TO STR9
         PRINT     "/",STR9:
                   *L,*1,"------------------------------":
                   "------------------------------":
                   "------------------------------":
                   "------------------------------":
                   "------------"
.END Patch #2.0 - replaced var
         XIF
addlne
	If	(Lines >= Pbreak)
           	CALL      HEADER
           	goto 	getrec
           	endif

         print     	*1,*RPTCHAR,DASH:132
         add	c1,lines

.         ADD       C5 TO LINES                       .detail lines
         GOTO      GETREC
.
overrun  print     *2,"(continued from previous page)"
         add       c1 to lines
         return
.
.START PATCH 2.3 - REMMED LOGIC
.LUKUP1   CLEAR     WORK47
.         CLEAR     WK247
.         MOVE      SPCL TO NSPIFLD
.         REP       " 0" IN NSPIFLD
.         CALL      NSPIKEY
.         IF        OVER
.         CLEAR     WORK47
.         CLEAR     WK247
.         RETURN
.         ENDIF
.         MOVE      INST1 TO WORK47
.         MOVE      INST2 TO WK247
.         RETURN
.END PATCH 2.3 - REMMED LOGIC
.
..............................................................................
.  EXIT
..............................................................................
.
NEG      MULT      SEQ BY DIFF
         RETURN

EXIT     BRANCH    PASS OF EXIT1A,EXIT1B,exit1c
EXIT1A
         COMPARE   c0 TO LINES
         CALL      HEADER IF equal
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         PRINT     *L,*1,"TOTAL UNBILLED INCOME - ",UNBILTOT:
                   *L,*1,"THIS REPORT DOES NOT INCLUDE ANY UNBILLED ORDERS":
                   " WITH A RETURN DATE GREATER THAN *7* DAYS AFTER TO THE":
                   " REPORT DATE.",*L:
                   *1,"IF THE RETURN DATE IS BLANK THEN THE MAIL-DATE IS USED."
         PRINT     *L,"START TIME ",TIME
         CLOCK     TIME TO TIME
         PRINT     *L,"END TIME   ",TIME," ",COUNTO,"- ORDERS EXAMINED",printd,"-  Orders Printed"
         SUB       COUNTO FROM COUNTO
	Sub	Printd,Printd
         move      page to remain
         div       c2 into remain
         move      c0 to result1
         add       remain to result1
         compare   result1 to remain
.         display   *p1:24,remain,b1,result1,*w5
         if        not equal
         print     *f
         endif
         MOVE      C0 TO UNBILTOT
         MOVE      C0 TO LINES
         MOVE      C0 TO PAGE
         MOVE      "ORDER FILE " TO FERROR
         CLOSE     NORDFILE
         MOVE      C0 TO NORDFLAG
         move      c2 to pass
         DISPLAY   *P01:05,"PASS ",pass
         GOTO      GETREC
EXIT1B   COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         COMPARE   c0 TO LINES
         CALL      HEADER IF equal

         PRINT     *L,*1,"TOTAL UNBILLED INCOME - ",UNBILTOT:
                   *L,*1,"THIS REPORT DOES NOT INCLUDE ANY UNBILLED ORDERS":
                   " WITH A Return Date 7 DAYS GREATER THAN THE REPORT DATE.":
	       *l,*1,"Unless we have shipping information": 	
                   *l,*1,"IF THE RETURN DATE IS BLANK THEN THE MAIL-DATE IS USED."
         PRINT     *L,"START TIME ",TIME
         CLOCK     TIME TO TIME
         PRINT     *L,"END TIME   ",TIME," ",COUNTO,"- ORDERS EXAMINED",printd,"-  Orders Printed"
         move      page to remain
         div       c2 into remain
         move      c0 to result1
         add       remain to result1
         compare   result1 to remain
.        display   *p1:24,remain,b1,result1
         if        not equal
         print     *f
         endif
.patch2.80
			compare   c1 to varflag
			goto      exit3 if equal
.patch2.80
	Sub	Printd,Printd
         SUB       COUNTO FROM COUNTO
         MOVE      C0 TO UNBILTOT
         MOVE      C0 TO LINES
         MOVE      C0 TO PAGE
         MOVE      "ORDER FILE " TO FERROR
         CLOSE     NORDFILE
         MOVE      C0 TO NORDFLAG
         move      c3 to pass
         DISPLAY   *P01:05,"PASS ",pass
         GOTO      GETREC
EXIT1c
         COMPARE   c0 TO LINES
         CALL      HEADER IF equal
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         PRINT     *L,*1,"TOTAL UNBILLED INCOME - ",UNBILTOT:
                   *L,*1,"THIS REPORT DOES NOT INCLUDE ANY UNBILLED ORDERS":
                   " WITH A RETURN DATE GREATER THAN *7* DAYS AFTER TO THE":
                   " REPORT DATE.",*L:
                   *1,"IF THE RETURN DATE IS BLANK THEN THE MAIL-DATE IS USED."
.                   " WITH A RETURN DATE GREATER THAN *3* DAYS PRIOR TO THE":
.                   " REPORT DATE.",*L:
.                  *1,"IF THE RETURN DATE IS BLANK THEN THE MAIL-DATE IS USED."
         PRINT     *L,"START TIME ",TIME
         CLOCK     TIME TO TIME
         PRINT     *L,"END TIME   ",TIME," ",COUNTO,"- ORDERS EXAMINED",printd,"-  Orders Printed"
         PRINT     *F
         goto      exit3
EXIT3    BEEP
         SPLCLOSE
         release
.         close     outsp
         shutdown  "cls"
ABORT    TRAPCLR   F5
         PRINT     *L,"*****JOB ABORTED BY OPERATOR"
         GOTO      EXIT3
..............................................................................
.  ERROR SUBROUTINES
..............................................................................
.
IO
         TRAPCLR   IO
         NORETURN
         DISPLAY   *P1:23,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!";
         BEEP
         PRINT     *L,"*** JOB ABORTED - I/O ERROR"
         KEYIN     *P70:24,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      EXIT IF EQUAL
         GOTO      IO
RANGE
         TRAPCLR   RANGE
         NORETURN
         DISPLAY   *P1:24,*EL,"RANGE ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         PRINT     *L,"*** JOB ABORTED - RANGE ERROR"
         KEYIN     *P70:24,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      EXIT IF EQUAL
         GOTO      RANGE
FORMAT
         TRAPCLR   FORMAT
         NORETURN
         DISPLAY   *P1:24,*EL,"FORMAT ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         PRINT     *L,"*** JOB ABORTED - FORMAT ERROR"
         KEYIN     *P70:24,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      EXIT IF EQUAL
         GOTO      FORMAT
PARITY
         TRAPCLR   PARITY
         NORETURN
         DISPLAY   *P1:24,*EL,"PARITY ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         PRINT     *L,"*** JOB ABORTED - PARITY ERROR"
         KEYIN     *P70:24,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      EXIT IF EQUAL
         GOTO      PARITY
         INCLUDE   NDATIO.inc
.START PATCH 2.73.3 REPLACED LOGIC
.         INCLUDE   NMLRIO.INC
.         INCLUDE   NBRKIO.INC
	INCLUDE	COMPIO.inc
	INCLUDE	CNTIO.inc
.END PATCH 2.73.3 REPLACED LOGIC
         INCLUDE   NSHPIO.inc
         INCLUDE   NORD2IO.inc
         INCLUDE   NOWNIO.inc
         INCLUDE   NMRGIO.inc
         INCLUDE   NMOAIO.inc
         INCLUDE   NMOBIO.inc
         include   nrtnio.inc
.         include   nspiio.inc
         include   nspeio.inc
.START PATCH 2.2 - ADDED LOGIC
         include   nofrio.inc
.END PATCH 2.2 - ADDED LOGIC
.START PATCH 2.81 REMOVED LOGIC
..START PATCH 2.71 ADDED LOGIC
.	INCLUDE	NFULIO.INC
..END PATCH 2.71 ADDED LOGIC
.END PATCH 2.81 REMOVED LOGIC
.START PATCH 2.73 - ADDED LOGIC
	INCLUDE	NSELIO.INC
	INCLUDE	NSEL2IO.INC
	INCLUDE	NTXTIO.INC
.END PATCH 2.73 - ADDED LOGIC
.START PATCH 2.73.1 - ADDED LOGIC
	INCLUDE	NSEL3IO.INC
	INCLUDE	NADDIO.INC
	INCLUDE	NSLTIO.INC
	INCLUDE	NREFIO.INC
	INCLUDE	NMODIO.INC
.END PATCH 2.73.1 - ADDED LOGIC
        INCLUDE   COMLOGIC.inc
