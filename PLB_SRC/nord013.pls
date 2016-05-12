* *****************************************************************************
* UNBILLED
* NAMES IN THE NEWS CALIF. UNBILLED REPORT PROGRAM      26FEB91
*
* CREATED FROM  AMOUNTDUE & NIN201/TEXT
* *****************************************************************************
*
PC       EQU       0
         INCLUDE   COMMON.inc
.START PATCH 3.0 - ADDED LOGIC
         INCLUDE   NOFRDD.INC
.END PATCH 3.0 - ADDED LOGIC
.START PATCH 3.54 REMOVED LOGIC
..START PATCH 3.42 ADDED LOGIC
.	INCLUDE	NFULDD.INC
..END PATCH 3.42 ADDED LOGIC
.START PATCH 3.54 REMOVED LOGIC
.START PATCH 3.45 ADDED LOGIC
	INCLUDE	NSELDD.INC
	INCLUDE	NSEL2DD.INC
	INCLUDE	NTXTDD.INC
.END PATCH 3.45 ADDED LOGIC
release  init      "3.55"	  DLH	  26SEp2007  PLI CONVERSION and replace smtp with sendmail
.release  init      "3.54"	  DMS	  22JUN2006  FULFILLMENT CONVERSION
.release  init      "3.53"         ASH     07APR2005  COMMPER CONVERSION
.release  init      "3.52"         JD      14Jan2005  Updated starting LR
;release  init      "3.51"        DLH	   08JUn2004  Refix required trim of datacard var Commper
;release  init      "3.50"        JD	   04JUn2004  check dsprog for differents runs.
;release  init      "3.46"        DMB	26MAY2004	Mailer Conversion
.RELEASE  INIT      "3.45"      29JAN2004 ASH DATACARD CONVERSION
.RELEASE  INIT      "3.44"      06AUG2002 JD added lw robbins 12/m mailers.
.RELEASE  INIT      "3.43"     04FEB2002 ASH	NINFUL CONVERSION
.RELEASE  INIT      "3.42"      08Mar02    added read of new mail date revision file.
.RELEASE  INIT      "3.41"     30Jan02    removed WS/added NWF 5.00/m pricing.
.RELEASE  INIT      "3.4"     15Oct01    DLH for reuse Orders with Blank return dates include in report instead of defaulting to maildate.
.                                       also email totals to DH
.RELEASE  INIT      "3.3"     23July01    DLH quick little patch to email a message
.RELEASE  INIT      "3.2"     19MAR01    ASH NINORD MOVED TO FILE MANAGER
.RELEASE  INIT      "3.1a"     04Jan00 jd  skip cancelled lcr's.
.RELEASE  INIT      "3.1"     08Oct99 DLH Added some more invalid rtn date checks.
.                            was skipping reuse orders?
.RELEASE  INIT      "3.0"     06MAY99 ASH Replaced OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.Release  init      "2.9"     06JAN99 ASH NINORD Y2K, File expansion
.Release  init      "2.81"     12jan99 jd changed start lr# search.
.Release  init      "2.8"     28Sep98 DLH added code to handle pending orders
.                            See norddd.inc patch 5
.Release   init      "2.7"             DLH 29jul98  starting lr 98.
.Release   init      "2.6"             DLH 09Jul98 commission on brk rents
.release  init      "2.51"            DLH 07May98 turn off locks on major reads
.release  init      "2.5"             JD  09FEB98  UPDATED STARTING LR#.
.release  init      "2.4"             JD  12DEC96 added more discount break.
.release  init      "2.3"             DLH 23Oct96 Add net code for straight rentals
.Release  init      "2.2"             JD  30mar95 added dawson brk # getprice
.RELEASE  INIT      "2.1"            JD  29JUN94 print to laser.
.RELEASE  INIT      "2.0"           JD  28SEP93 SPLC FLAT CHRGE $7.00 EXCHANGE.
.
.RELEASE  INIT      "1.9"          DLH 20MAY93 FLAG MAILDATE >= 2WKS OLD.
.
.RELEASE  INIT      "1.8"          DLH 26MAR93 ADDED BATCH BILLING BREAKOUT.
.
.RELEASE  INIT       "1.7"          DLH 27APR92  ADD CHAINING FROM DSINIT
.
.RELEASE   INIT      "1.6"         DLH 08APR92  LIFESTYLE & IC SYSTEMS.
.
.RELEASE  INIT      "1.5"         D. HERRICK 15JAN92  CONVERT MOST INCLUDES,
.                                ADD EXCLUSIVE FLAG (LIST).
.RELEASE  INIT      "1.5"         D. HERRICK  SEP91    ALLOW DATE CHANGE.
.RELEASE  INIT      "1.4"         D. HERRICK 12SEP91    ADD BREAK OUT OF 30
.                                DAY INCOME INTO BRKAGE XCHN/RENT, LIST MANAG.
.RELEASE  INIT      "1.3"         D. HERRICK 10SEP91    CHANGE AGEING ON (A)LL
.                                REPORT TO MATCH STANDARD..
.RELEASE  INIT      "1.2"         D. HERRICK 06AUG91    ADDED ZERO LR INC ON
.                                LIST MANAGEMENT EXCHANGES.
.RELEASE  INIT      "1.1"         D. HERRICK 01AUG91
.                                ADDED AGEING OF ESTIMATED INCOME ON ALL REP.
.                                ADDED INCLUDE COMLOGIC.inc
.
.RELEASE  INIT      "1.0"        DLH. 26FEB91 WRITTEN.
. .............................................................................
.
.
. FILES DESCRIPTIONS
. ..................
. ...................................
.ACCOUNT1 IFILE     KEYLEN=7
. .....
. .............................................................................
.
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NOWNDD.inc
         INCLUDE   NSHPDD.inc
.patch3.46
				include	compdd.inc
				include	cntdd.inc
.         INCLUDE   NMLRDD.inc
.patch3.46
         INCLUDE   NDATDD.inc
         include   nmrgdd.inc
         include   nmobdd.inc
         include   nmoadd.inc
         include   hp.inc
.START PATCH 3.42 ADDED LOGIC
         include   nmlddd.inc
.END PATCH 3.42 ADDED LOGIC
.
.l WORK VARIABLES
.
.START PATCH 3.53 REPLACED LOGIC
N34	FORM	3.4
.END PATCH 3.53 REPLACED LOGIC
CHANGE   FORM      7.2         CHANGE TO BE APPLIED TO BALANCE.
PRTFLAG  FORM      1
THOUS    FORM      "1000"
HUND     FORM      "100"
PDATE    DIM       8
JOBBR    FORM      1               BRANCH FOR JOB TYPE SEPERATE,TOTAL
.BALANCE  FORM      7.2
TOTALDOL FORM      7.2
YR       DIM       2
GROSS    FORM      7.2
.Start Patch #2.9 - replaced var
.TOTAL    FORM      7.2
.AR       FORM      7.2
TOTAL    FORM      9.2
AR       FORM      9.2
.END Patch #2.9 - replaced var
CODENUM  FORM      2
EXCLPRT  DIM       4
.
PROGNAME DIM       8
CHKJUL   FORM      5
chkdate  form      6
chkdate2 dim       6
.Start Patch #2.9 - replaced var
.UNBILAMT FORM      7.2
.FORM72   FORM      7.2
.net72    form      7.2            holding field while calcing net charges
UNBILAMT FORM      9.2
FORM92   FORM      9.2
net92    form      9.2            holding field while calcing net charges
.End Patch #2.9 - replaced var
form32   form      3.2
FORM52   FORM      5.2
card$    form      5.2
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10             NUMERIC WORK FIELD FOR CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
.Start Patch #2.9 - replaced var
.UNBILINC FORM      7.2
.UNLNC30  FORM      7.2
.UNLNC60  FORM      7.2
.UNLNC90  FORM      7.2
.UNLNC90P FORM      7.2
.UNBILTOT FORM      10.2
.LSTMTOT  FORM      7.2             TOTAL LIST MANAGEMENT DUE 30 DAYS
.BRKEXTOT FORM      7.2             TOTAL BROKERAGE EXCHANGE DUE 30 DAYS
.BRKRNTOT FORM      7.2             TOTAL BROKERAGE RENTAL DUE 30 DAYS
.LRBBE    FORM      9.2      TOTAL BATCH BILL LR EXCH PORTION
.LRBBR    FORM      9.2      TOTAL BATCH BILL LR RENT PORTION
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
LRBBE    FORM      11.2      TOTAL BATCH BILL LR EXCH PORTION
LRBBR    FORM      11.2      TOTAL BATCH BILL LR RENT PORTION
.END Patch #2.9 - replaced var
WRIT     FORM      1
.
BATCHBR  FORM      1       "0" =NO, "1" = YES.
.
AGEFLAG  FORM      1
KEY      DIM       28
Searching      Init           "S-E-A-R-C-H-I-N-G"
. .............................................................................
.
.
. PROGRAM VARIABLES
. .................
.
ANS      DIM       1
DATE     DIM       8
TIME     DIM       8
MDATE    FORM      5
JUNDATE  FORM      6
.Start Patch #2.9 - replaced var
.QTYCHK   FORM      6
QTYCHK   FORM      9
.END Patch #2.9 - replaced var
CHKMM    FORM      2                  HOLDS MONTH PARAMETER
TENDOLL  FORM      "99999"
NINEDOLL FORM      "199999"
SEVDOLL  FORM      "300000"
JUNEDAT  INIT      "060191"
+ *****************************************************************************
CHKYR    FORM      2                  HOLDS YEAR PARAMETER
PASS     FORM      3                  FOR JOBBR = 1 PASS = 1 = TRIPLEX
.                                     FOR JOBBR = 1 PASS = 2 = NOT TRIPLEX
.                                     FOR JOBBR = 2 NOT  USED.
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
CHKMLR   DIM       4                  USED TO ELIMINATE DUP MAILER DISP.
COMPMLR  DIM       4
LINES    FORM      2
PAGE     FORM      5
PBREAK   FORM      "59"
SHIPPED  DIM       9
EXCHANGE DIM       9
COMSLCT  DIM       9
COUNTO   FORM      6                  NUMBER OF ORDERS READ.
COUNTO1  FORM      5                  NUMBER OF ORDERS CALCULATED.
COUNTI   FORM      5                  NUMBER OF INVOICES READ
COUNTI1  FORM      5                  NUMBER OF INVOICES CALCULATED
.Start Patch #2.9 - replaced var
.TOTALU   FORM      7.2
TOTALU   FORM      9.2
.END Patch #2.9 - replaced var
TDMCSW   DIM       1                  TRIPLEX INDICATOR Y=TRIPLEX, N=NOT
SPLITSW  DIM       1                  RENT/EXCHANGE SPLIT = 'Y'
.LSTMSW   DIM       1                  LIST MANAGEMENT INDICATOR.
SW30     DIM       1                  DUE FOR BILLING IN 30 DAYS = "Y"
SYSJDATE FORM      5
specl    dim       1
lwrobb12 init     "2749-1822-0704-0974-1451-0127-0638"
.START PATCH 3.45 - ADDED LOGIC
TEXT1    DIM       47
.END PATCH 3.45 - ADDED LOGIC
.
. .............................................................................
. MAINLINE
. .............................................................................
         TRAP      ABORT IF F5
         MOVE      "EXIT" TO PF5
         MATCH     "NORD013" TO PROGRAM   .CHAINED FROM DSINIT?
         if         equal
         MOVE      TODAY TO PDATE
         UNPACK    PDATE INTO MM,STR1,DD,STR1,YY
         MOVE      FUNC TO STR1
         MOVE      FUNC TO JOBBR
         goto      input
         ENDIF
.         IF        NOT EQUAL              *NOT PROPERLY CHAINED FROM DSINIT
         MOVE      "NORD013" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "LOCAL" TO PRTNAME
.START PATCH 3.2 REPLACED LOGIC
.         move      "NINORD" to nordname
.         move      "NINORD.ISI|20.20.30.103:502" to nordname
.END PATCH 3.2 REPLACED LOGIC
         MOVE      C0 TO JOBBR
         IFNZ      PC
         CLOCK     DATE TO DATE
         MOVE      "99/99/99" TO PDATE
         EDIT      DATE TO PDATE
         MOVE      PDATE TO TODAY
         UNPACK    DATE INTO MM,DD,YY
         XIF
.
         IFZ       PC
         CLOCK     DATE TO PDATE
         MOVE      PDATE TO TODAY
         UNPACK    PDATE INTO MM,ANS,DD,ANS,YY
         XIF
.                ELSE
.         MOVE      TODAY TO PDATE
.         UNPACK    PDATE INTO MM,STR1,DD,STR1,YY
.         MOVE      FUNC TO STR1
.         MOVE      FUNC TO JOBBR
.         ENDIF

input
.         CMATCH    B1 TO INPNAME
.START PATCH 3.50
         Scan      "NINORD", inpname
         IF        not equal
         MOVE      INPNAME TO NORDNME1
         MOVE      inpname  TO NORDNAME
	else
         move      "NINORD.ISI|20.20.30.103:502" to nordname
         endif
.end PATCH 3.50
         CLOCK     TIME TO TIME
         MOVE      "UNBILLED REPORT" TO STITLE
         MOVE      "DATE" TO PF3
         CALL      PAINT
         CALL      FUNCDISP
         GOTO      PRTGET
NOTHING  RETURN
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     "LOCAL"  TO PRTNAME
         GOTO      TRAPS IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH pdrive,PRTNAME
.         PACK      PRTFILE WITH PRTNAME
         SPLOPEN   PRTFILE
         print     hptop,hpdupl,hp17ptch,*f
         DISPLAY   *P01:06,"Input File  :",*P15:06,Nordname
         DISPLAY   *P01:07,"Print File  :",*P15:07,PRTNAME
         GOTO      TRAPS
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET

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
.         MOVE      "MASTER  ON ACCOUNT FILE " TO FERROR
.         OPEN      ACCOUNT1,"NINMOB",SHARE
         MOVE      "                    " TO FERROR
         KEYIN    *P1:24,*EL,"FILES OPEN 30 SEC'S TO CHANGE DATE",*T30,STR1;
.         PAUSE     "30"
         TRAP      NOTHING IF F3
         MOVE      B1 TO PF3
         CALL      FUNCDISP
         UNPACK    TODAY INTO MM,STR1,DD,STR1,YY
         CALL      CVTJUL
         MOVE      JULDAYS TO SYSJDATE
         TRAP      ABORT IF F5
**************************************************
******** FIRST LR # OF 1998   *******************
.test 3june2002 dlh to match nord013x
.patch3.52
         MOVE      "450000" TO NORDFLD
.patch3.52
;         MOVE      "305233" TO NORDFLD
.         MOVE      "324000" TO NORDFLD
....................................turn of all locks on reads
         move      c3 to nordlock           order file
         move      c3 to nmlrlock           mailer file
         move      c3 to ndatlock           datacard file
............................................................................
         MOVE      C1 TO NORDPATH
         CALL      NORDTST
.
START    MOVE      C0 TO LINES
         MOVE      C0 TO TOTALDOL
         BRANCH    JOBBR TO DEF,ALL        .IF FROM DSINIT WE HAVE VALUE.
         MOVE      "D" TO STR1
.         DISPLAY   *P1:1,*ES,*P24:3,"N A M E S   I N   T H E   N E W S ":
.                   *P18:05,"***   U N B I L L E D   R E P O R T   ***"
         KEYIN     *P28:09,"(D)efault or (A)ll ",*T30,*RV,STR1
         REP       "D1A2" IN STR1
         MOVE      STR1 TO JOBBR
         BRANCH    JOBBR OF DEF,ALL         >1 OR 2 ?
         GOTO      START                            NO!
DEF     DISPLAY    *P28:09,*EL,"DEFAULTS SELECTED"
        GOTO  START1
.
ALL     DISPLAY    *P28:09,*EL,"ALL SELECTED"
        GOTO  START1
.
START1
         MOVE      C1 TO PASS
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
.
GETREC         MOVELPTR       sEARCHING TO N2
               IF             (N2 = 17)
               SETLPTR	SEARCHING TO C1
               ELSE
               ADD            C1 TO N2
               SETLPTR	SEARCHING TO N2
               ENDIF
               DISPLAY        *P09:24,*EL,*HON,Searching,*HOFF;
.
         CALL      NORDKS
         GOTO      EXIT IF OVER
              if            (olrn = "427288")
              call          debug
              endif
         Display              *p1:10,"Current LR ",*p15:10,Olrn
         ADD       C1 TO COUNTO
         DISPLAY   *P1:24,COUNTO;
         CMATCH    "B" TO OSTAT    *BILLED?
         GOTO      GETREC IF EQUAL       *YES
.begin patch 2.8
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      Getrec IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       LCR order ?
         GOTO      Getrec IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       cancelled LCR order ?
         GOTO      Getrec IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
.end patch 2.8
         RESET     CANCODES               *RESET FORM POINTER.
         SCAN      OSTAT IN CANCODES       *CANCELLED?
         GOTO      GETREC IF EQUAL
.START PATCH 3.0 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 3.0 - NEW LOGIC
.START PATCH 3.45 ADDED LOGIC
	packkey	NSEL2FLD,"1",OLRN
	move	"NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if not over
		move	NSEL2NAME,O2DES
	else
		unpack	OPPM,str3,str2
		pack	str6,str3,".",str2
		rep	zfill,str6
		move	str6,NSEL2PRICE
	endif
.END PATCH 3.45 ADDED LOGIC
.START PATCH 3.42
	pack	NMLDFLD1,"01X",OLRN
        clear   str8
	pack	str8,"99999999"
	call	NMLDAIM
	loop
		until over
		if (NMLDDATE < str8)
			move	NMLDDATE,str8
		endif
		call	NMLDKG
	repeat
	if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
         unpack     str8 into omdtec,omdtey,omdtem,omdted
	else
.Use current Mail Date
	endif
.end PATCH 3.42
.
         MOVE      OLON TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         CALL      NOWNKEY
         PACK      STR2 FROM OSALES10,OSALES
         REP       ZFILL IN STR2
         MOVE      NO TO LSTMSW
.begin patch 3.55
	If	(str2 = "06" | Str2 = "19"  | Str2 = "27"  | Str2 = "28")
.         MATCH     "06" TO STR2
.               IF        EQUAL
               MOVE      YES TO LSTMSW            *LIST MANAGEMENT.
.               ELSE
.               MATCH     "19" TO STR2
.                              IF        EQUAL
.                              MOVE      YES TO LSTMSW
                              ELSE
                              MATCH     "00" TO STR2
                                             IF        EQUAL
                                             RESET     RUNCODES
                                             SCAN      OLNUM IN RUNCODES
                                                            IF        NOT EQUAL
                                                            MOVE      YES TO LSTMSW
                                                            ENDIF
                                             ENDIF
.                              ENDIF
.end patch 3.55
         ENDIF
         MOVE      NO TO OVER
.
PREPOWN  MOVE      OLON TO NOWNFLD
         CALL      NOWNKEY
         COMPARE   C1 TO JOBBR               *WHICH JOB TYPE?
         GOTO      CONTIN IF NOT EQUAL         *ALL REPORT REPORT
.START PATCH 3.43 ADDED LOGIC
.         SCAN      "TDMC" IN OWNCTN
.         IF        EQUAL
.         BRANCH    PASS OF TDMCYES,GETREC      *TRIPLEX PASS, OR NON-TDMC
.
	call	Trim using OWNCTN
.START PATCH 3.54 REMOVED LOGIC
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
.	if (NFULFLD = "0026")
	if (OWNCTN <> "")
		pack	COMPFLD6,OWNCTN
		rep	zfill,COMPFLD6
		move	C1,COMPPATH
		move	"PREPOWN-COMPKEY6",Location
		pack	KeyLocation,COMPFLD6
		call	COMPKEY6
			if over
				clear	COMPFLD6
				clear	COMPCOMP
			else
				if (COMPSVBFLG <> "T")
					clear	COMPFLD6
					clear	COMPCOMP
				endif
			endif
	else	// OWNCTN = ""
		clear	COMPFLD6
		clear	COMPCOMP
	endif
	if (COMPFLD6 = "0026")
.END PATCH 3.54 REMOVED LOGIC
		BRANCH    PASS OF TDMCYES,GETREC      *TRIPLEX PASS, OR NON-TDMC
.END PATCH 3.43 ADDED LOGIC
TDMCYES
         clear     chkdate2
         pack      chkdate2 from ortndtem,ortndted,ortndtey
         move      c0 to chkdate
         move      chkdate2 to chkdate
.begin patch 3.4
.if no return date check for a reuse
.         compare   c0 to chkdate
              if            (chkdate = "0" or chkdate2 = "")
                            if            (ortnnum = "0001")
                            goto          contin                   .take it now it is a reuse
                            else
	              call          usemd
                            endif
.         call       USEMD IF EQUAL
              endif
.end patch 3.4
.         clear     chkdate2
.         pack      chkdate2 from ortndtem,ortndted,ortndtey
.begin patch 3.4
.if no return date check for a reuse
.         MATCH     b6 TO chkdate2
.         call       USEMD IF EQUAL
.         call       USEMD IF Eos
              if            (chkdate2 = b6 or chkdate2 = "" or chkdate2 = b1)
                            if            (ortnnum = "0001")
                            goto          contin                   .take it now it is a reuse
                            else
	              call          usemd
                            endif
.         call       USEMD IF EQUAL
              endif
.end patch 3.4

         CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         MOVE      ORTNDTEM TO MM
         MOVE      ORTNDTED TO DD
         MOVE      ORTNDTEY TO YY
         MOVE      ORTNDTEc TO cc
         CALL      CVTJUL
         GOTO      CHKDAYS
USEMD    CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         MOVE      OMDTEc TO CC
         CALL      CVTJUL
CHKDAYS  MOVE      SYSJDATE TO CHKJUL
         ADD       C7 TO CHKJUL              PLUS 7 DAYS
         SUB       JULDAYS FROM CHKJUL
.         COMPARE    CHKJUL TO C2                DATE GREATER THAN 3 DAYS PRIOR
         COMPARE    CHKJUL TO C0                DATE GREATER THAN 7 DAYS AFTER
         GOTO      GETREC IF NOT LESS               YES
         GOTO      CONTIN                        NO,PROCESS
         ELSE
         BRANCH    PASS OF GETREC,TDMCNO
         ENDIF
.
TDMCNO   CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
.         MOVE      C0 TO NYY
         MOVE      OMDTEY TO YY
         CALL      CVTJUL
         MOVE      SYSJDATE TO CHKJUL
         SUB       JULDAYS FROM CHKJUL
         COMPARE    CHKJUL TO SEQ              DATE GREATER THAN TODAY?
         GOTO      GETREC IF NOT LESS             YES
.
CONTIN   CALL      DETAIL
         GOTO      GETREC               *ADDITIONAL CRITERIA FAILED GET NEXT RE
.
DETAIL
.Start Patch #2.9 - replaced var
.         MOVE      C0 TO FORM72
         MOVE      C0 TO FORM92
.End Patch #2.9 - replaced var
         MOVE      C0 TO UNBILAMT
         MOVE      C0 TO UNBILINC
         MOVE      C0 TO FORM52
         MOVE      C0 TO AR
         move      c0 to commper
         clear     specl
         CALL      MLRREAD
         CALL      READSHIP            *ORDER SHIPPED????
         CALL      READMRG
         CALL      GETCARD
.Start Patch #2.9 - replaced var
.         SUB       FORM72 FROM FORM72
.         SUB       FORM52 FROM FORM52
.         MOVE      OQTY TO FORM72
.         DIV       THOUS INTO FORM72.
         SUB       FORM92 FROM FORM92
         SUB       FORM52 FROM FORM52
         MOVE      OQTY TO FORM92
         DIV       THOUS INTO FORM92
.End Patch #2.9 - replaced var
         call      special
         cmatch    yes to specl
         if        equal
         goto      calcsp       .got price fron special subroutine
         endif
.START PATCH 3.45 REPLACED LOGIC
.         MOVE      OPPM TO FORM52         .use price from order
.         DIV       HUND INTO FORM52
	MOVE	NSEL2PRICE,FORM52
               if             (form52 = 0 & lstmsw <> "Y" & OELCODE <> "3" & OELCODE <> "3")
               call           debug
               endif
               if             (Omlrnum = "9615")
               call           debug
               endif
.END PATCH 3.45 REPLACED LOGIC
calcsp
.Start Patch #2.9 - replaced var
.         MULT      FORM52 BY FORM72       .base proce
.         MOVE      FORM72 TO UNBILINC
         MULT      FORM52 BY FORM92       .base price
         MOVE      FORM92 TO UNBILINC
.END Patch #2.9 - replaced var
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES             EXCHANGE ?
         GOTO      OKEX IF EQUAL

         if        (onetfm="M" | onetfm="F")      .discounted order???
         move      onetper to form32              .yes calc it
         mult      ".01" by form32
.Start Patch #2.9 - replaced var
.         MOVE      OQTY TO FORM72
.         DIV       THOUS INTO FORM72
.         mult       form32 by form72               .percentage of billable names
.         move      form72 to net72                 .save it
.         MULT      FORM52 BY FORM72
.         MOVE      FORM72 TO UNBILINC               .base rental
.
         MOVE      OQTY TO FORM92
         DIV       THOUS INTO FORM92
         mult       form32 by form92               .percentage of billable names
         move      form92 to net92                 .save it
         MULT      FORM52 BY FORM92
         MOVE      FORM92 TO UNBILINC               .base rental
.END Patch #2.9 - replaced var
         endif

         GOTO      RENT
.OKEX - CHECK FOR SPLIT RENTAL/EXCHANGE.
OKEX
.Start Patch #2.9 - replaced var
.         MOVE      C0 TO FORM72
.         MOVE      OEXQTY TO FORM72
.         COMPARE   C0 TO FORM72            PURE EXCHANGE ?
         MOVE      C0 TO FORM92
         MOVE      OEXQTY TO FORM92
         COMPARE   C0 TO FORM92            PURE EXCHANGE ?
.END Patch #2.9 - replaced var
         IF        EQUAL                 YES.
          MOVE      C0 TO QTYCHK
          MOVE      NO TO SPLITSW
          MOVE      OQTY TO QTYCHK
.Start Patch #2.9 - replaced var
.          MOVE      QTYCHK TO FORM72
          MOVE      QTYCHK TO FORM92
.END Patch #2.9 - replaced var
          CMATCH    YES TO LSTMSW
....................
           IF        EQUAL
           MOVE      C0 TO UNBILINC
           GOTO      OK
           ELSE
           GOTO      GETPRICE
           ENDIF
....................
          ELSE
          MOVE      YES TO SPLITSW            .split
          CMATCH    YES TO LSTMSW
.............................
           IF        EQUAL
           MOVE      C0 TO UNBILAMT
           GOTO      RENTPART
           ELSE
           MOVE      C0 TO QTYCHK
           MOVE      OEXQTY TO QTYCHK
.Start Patch #2.9 - replaced var
.           MOVE      QTYCHK TO FORM72
           MOVE      QTYCHK TO FORM92
.END Patch #2.9 - replaced var
           GOTO      GETPRICE
           ENDIF
............................
          ENDIF
GETPRICE
         match     "0006" to omlrnum
         if        equal
         move      c7 to form52
         goto      calce
         endif

         cmatch    yes to specl
         if        equal
         goto      calce
         endif

         CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         UNPACK    JUNEDAT INTO MM,DD,YY
         CALL      CVTJUL           *CONVERT JUNE 1ST'S DATE TO JULIAN
         MOVE      JULDAYS TO JUNDATE    *SAVE RESULT
         CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         CALL      CVTJUL           *CONVERT TODAY'S  DATE TO JULIAN
         MOVE      JULDAYS TO MDATE    *SAVE RESULT
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
         GOTO      CALCE
         ENDIF

         MOVE      C8 TO FORM52

CALCE
.Start Patch #3.44
        reset      lwrobb12
        match      "0638" to obrknum             .lwrobb
        if         equal
        scan       omlrnum in lwrobb12
        if         equal
        move       "12" to form52
        endif
        endif
.End Patch #3.44

.Start Patch #2.9 - replaced var
.         DIVIDE    THOUS INTO FORM72
.         MULTIPLY  FORM52 BY FORM72
.         MOVE      FORM72 TO UNBILAMT
         DIVIDE    THOUS INTO FORM92
         MULTIPLY  FORM52 BY FORM92
         MOVE      FORM92 TO UNBILAMT
.END Patch #2.9 - replaced var
         CMATCH    YES TO SPLITSW
         IF        EQUAL
         GOTO      RENTPART
         ELSE
.Start Patch #2.9 - replaced var
.         MOVE      FORM72 TO UNBILINC
         MOVE      FORM92 TO UNBILINC
.END Patch #2.9 - replaced var
         GOTO      OK
         ENDIF
.
RENTPART
.Start Patch #2.9 - replaced var
.         MOVE      C0 TO FORM72          SPLIT RENT/EXCHANGE
.         MOVE      C0 TO N7
.         MOVE      OQTY TO FORM72
.         MOVE      OEXQTY TO N7
.         SUBTRACT  N7 FROM FORM72           GET RENTAL PORTION
.         MULT      ".001" BY FORM72
.
         MOVE      C0 TO FORM92          SPLIT RENT/EXCHANGE
         MOVE      C0 TO N9
         MOVE      OQTY TO FORM92
         MOVE      OEXQTY TO N9
         SUBTRACT  N9 FROM FORM92           GET RENTAL PORTION
         MULT      ".001" BY FORM92
.END Patch #2.9 - replaced var
         cmatch    yes to specl
         if        equal
         goto      calcsp2
         endif

         compare   c0 to card$
         if        equal
         MOVE      "65.00" TO FORM52          *ESTIMATED $.   (USE DATACARD?)
         else
         move      card$ to form52
         endif
.         CALL      GETCARD
.Start Patch #2.9 - replaced var
.         MULT      FORM52 BY FORM72
         MULT      FORM52 BY FORM92
.END Patch #2.9 - replaced var
.
.        display     *p1:24,"brk",obrknum
         match      "0192" to obrknum
         IF         EQUAL
.         display    *p1:24,*el,"EPSILON!!!!!!!!!"
.START PATCH 3.53 REPLACED LOGIC
.         match      "20" to commper
.         if         equal
	if (COMMPER = "20")
.END PATCH 3.53 REPLACED LOGIC
.Start Patch #2.9 - replaced var
.         MULT      ".1" BY FORM72
         MULT      ".1" BY FORM92
.END Patch #2.9 - replaced var
         goto      addamt
         ENDIF
.START PATCH 3.53 REPLACED LOGIC
.         match      "30" to commper
.         if         equal
	if (COMMPER = "30")
.END PATCH 3.53 REPLACED LOGIC
.Start Patch #2.9 - replaced var
.         MULT      ".2" BY FORM72
..         display    *p1:24,*el,"form72 ",form72,b1,commper,*w2
         MULT      ".2" BY FORM92
.         display    *p1:24,*el,"form92 ",form92,b1,commper,*w2
.END Patch #2.9 - replaced var
         goto      addamt
         ENDIF
.oddball use 10% commission
.Start Patch #2.9 - replaced var
.         MULT      ".1" BY FORM72
         MULT      ".1" BY FORM92
.END Patch #2.9 - replaced var
         goto      addamt
         endif
.
        CMATCH    YES TO LSTMSW
         IF        EQUAL
.Start Patch #2.9 - replaced var
.         MULT      ".1" BY FORM72
         MULT      ".1" BY FORM92
.END Patch #2.9 - replaced var
         ELSE
.START PATCH 3.53 REPLACED LOGIC
.         move       c0 to n32      .DLH USE Datacard info 09Jul98
.         move       commper to n32
.         mult       ".01" by n32
..Start Patch #2.9 - replaced var
..         mult       n32,form72
...         MULT      ".2" BY FORM72            ESTIMATED LR INCOME.
.         mult       n32,form92
................................
         move       c0 to n34      .DLH USE Datacard info 09Jul98
         move       commper to n34
         mult       ".01" by n34
         mult       n34,form92
.END PATCH 3.53 REPLACED LOGIC
.         MULT      ".2" BY FORM92            ESTIMATED LR INCOME.
.END Patch #2.9 - replaced var
         ENDIF

.Start Patch #2.9 - replaced var
.addamt   ADD       FORM72 TO UNBILAMT
addamt   ADD       FORM92 TO UNBILAMT
.END Patch #2.9 - replaced var
         MOVE      UNBILAMT TO UNBILINC
         GOTO      OK
.
.Start Patch #2.9 - replaced var
.calcsp2  MULT      FORM52 BY FORM72
.         ADD       FORM72 TO UNBILAMT
calcsp2  MULT      FORM52 BY FORM92
         ADD       FORM92 TO UNBILAMT
.END Patch #2.9 - replaced var
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
;ARGH - dlh
;begin patch 3.51
.START PATCH 3.53 REMOVED LOGIC
.               call           Trim using Commper
.END PATCH 3.53 REMOVED LOGIC
;ARGH
;end patch 3.51

.         DISPLAY   *P1:24,TEXT1,*W2;
.START PATCH 3.45 REPLACED LOGIC
.         parse     textdata into text1 using " ~09",noskip,blankfill
.         SCAN      "EXCHANGE ONLY" IN TEXT1
.         RETURN    IF EQUAL                 NO USABLE $ RETURN
.         RESET     TEXT1
.         SCAN      "$" IN TEXT1
.         RETURN    IF NOT EQUAL        NO USABLE $ RETURN
.         BUMP      TEXT1 BY 1
.         PACK      STR2 FROM TEXT1
.         move      c0 to card$
.         MOVE      STR2 TO card$
..         DISPLAY   *P1:24,STR2,B1,card$,*W2;
.         SCAN      "$" IN TEXT1        *DO WE HAVE CORRECT PRICE?
.         RETURN    IF NOT EQUAL            *YES.
.         CLEAR     STR2
.         BUMP      TEXT1 BY 1
.         PACK      STR2 FROM TEXT1       *NO, NOW WE DO!
.         move      c0 to card$
.         MOVE      STR2 TO card$
	if (NDATCONV = "1")
.		if (NDATEXCH <> "1")
			pack	NSELFLD1,"01X",LSTNUM
			pack	NSELFLD2,"021XBASE"
			move	"NSELAIM",Location
			pack	KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
			call	NSELAIM
			if not over
				if (NSELEXC <> "2")
					move	C0,card$
					move	NSELPRICE,card$
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
			move	c0 to card$
			MOVE	STR2 TO card$
			SCAN	"$" IN TEXT1        *DO WE HAVE CORRECT PRICE?
			RETURN IF NOT EQUAL            *YES.
			CLEAR	STR2
			BUMP	TEXT1 BY 1
			PACK	STR2 FROM TEXT1       *NO, NOW WE DO!
			move	c0 to card$
			MOVE	STR2 TO card$
		else
			clear	text1
		endif
	endif
.END PATCH 3.45 REPLACED LOGIC
.         DISPLAY   *P1:24,STR2,B1,card$,*W2;
         RETURN
RENT
         cmatch    yes to specl
         if        equal
         goto      ok
         endif

         CMATCH    YES TO LSTMSW             LIST MANAGEMENT?
         IF        EQUAL
         MULT      ".1" BY UNBILINC            YES
         ELSE
.DLH 05Aug98 this section was missing Epsilon code
.and was using the variable form72 instead of unbilinc.   so rentals reflected AR instead of
.LR income on report details and totals.

        display     *p1:22,"brk",obrknum

         match      "0192" to obrknum
           IF         EQUAL
           display    *p1:22,*el,"EPSILON!!!!!!!!!"
.START PATCH 3.53 REPLACED LOGIC
.           match      "20" to commper
.            if         equal
	if (COMMPER = "20")
.END PATCH 3.53 REPLACED LOGIC
            MULT      ".1" BY UNbilinc
            goto      ok
            ENDIF
.START PATCH 3.53 REPLACED LOGIC
.           match      "30" to commper
.           if         equal
	if (COMMPER = "30")
.END PATCH 3.53 REPLACED LOGIC
           MULT      ".2" BY UNbilinc
.START PATCH 3.53 REPLACED LOGIC
.           display    *p1:21,*el,"Unbilinc ",Unbilinc,b1,commper
           move		COMMPER,str6
           display    *p1:21,*el,"Unbilinc ",Unbilinc,b1,STR6
.END PATCH 3.53 REPLACED LOGIC
           goto      ok
           ENDIF
.oddball use 10% commission
          MULT      ".1" BY Unbilinc
          goto      ok
          endif
.START PATCH 3.53 REPLACED LOGIC
.         move       c0 to n32      .DLH USE Datacard info 09Jul98
.         move       commper to n32
.         mult       ".01" by n32
.         mult       n32,unbilinc
................................
         move       c0 to n34      .DLH USE Datacard info 09Jul98
         move       commper to n34
         mult       ".01" by n34
         mult       n34,unbilinc
.END PATCH 3.53 REPLACED LOGIC
.         MULT      ".2" BY FORM72            ESTIMATED LR INCOME.
         ENDIF
.
OK
         GOTO      USEMD1
TDMCYES1
         clear     chkdate2
         pack      chkdate2 from ortndtem,ortndted,ortndtey
         move      c0 to chkdate
         move      chkdate2 to chkdate
         compare   c0 to chkdate
.begin patch 3.4
.         call       USEMD1 IF EQUAL
.if no return date check for a reuse
              if            equal
                            if            (ortnnum = "0001")
                            goto          bill30                   .take it now it is a reuse
              else
	              call          usemd1
                            endif
.         call       USEMD IF EQUAL
              endif
.end patch 3.4

.         clear     chkdate2
.         pack      chkdate2 from ortndtem,ortndted,ortndtey
         MATCH     b6 TO chkdate2
.begin patch 3.4
.         call       USEMD1 IF EQUAL
.         call       USEMD1 IF Eos
.if no return date check for a reuse
              if            equal
                            if            (ortnnum = "0001")
                            goto          bill30                   .take it now it is a reuse
                            else
	              call          usemd1
                            endif
.         call       USEMD IF EQUAL
              endif
.end patch 3.4

.	 MATCH     "00" TO ORTNDTEY
.         GOTO      USEMD1 IF EQUAL
.	 MATCH     " 0" TO ORTNDTEY
.         GOTO      USEMD1 IF EQUAL
.	 CMATCH    b1 TO ORTNDTEY
.         GOTO      USEMD1 IF Eos
         CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         MOVE      ORTNDTEM TO MM
         MOVE      ORTNDTED TO DD
         MOVE      Ortndtec TO cc
         MOVE      ORTNDTEY TO YY
         CALL      CVTJUL
         GOTO      CHKDAY1
USEMD1   CLEAR     MM
         CLEAR     DD
         CLEAR     YY
         MOVE      OMDTEM TO MM
         MOVE      OMDTED TO DD
         MOVE      OMDTEY TO YY
         MOVE      OMDTEC TO CC
         CALL      CVTJUL
.
CHKDAY1  MOVE      SYSJDATE TO CHKJUL
.
         add       c1 to chkjul             .5/1/ dlh plus 1 day
         SUBTRACT  chkjul from JULdays
         MOVE      NO TO SW30
.
         COMPARE   c0 TO JULDAYS
         GOTO      BILL30 IF LESS          *SHOULD BE BILLED THIS MONTH.
.
         COMPARE   C31 TO JULDAYS
.         COMPARE   "61" TO JULDAYS         *SHOULD BE BILLED BY END NEXT MON.
         GOTO      BILL60 IF LESS
.
.         COMPARE   "91" TO JULDAYS         *
         COMPARE   "61" TO JULDAYS         *
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
.Start Patch #2.9 - replaced var
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
.END Patch #2.9 - replaced var
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
         COMPARE   C1 TO BATCHBR                 .BATCH ?
           IF      EQUAL                         .YES
           ADD     UNBILINC TO LRBBE
           ENDIF
         ELSE
         ADD       UNBILINC TO BRKRNTOT          NO ITS RENTAL.
         COMPARE   C1 TO BATCHBR                 .BATCH ?
           IF      EQUAL                         .YES
           ADD     UNBILINC TO LRBBR
           ENDIF
         ENDIF
         ENDIF
         ENDIF
         RESET     EXCODES
         if        (unbilinc > 3000)
	move          "Nord0013 - output" to MailSubjct
	Clear	Mailbody
	Append	Olrn,Mailbody
	Append	B1,Mailbody
	Append	"LR ##",Mailbody
	Append	CrLf,Mailbody
.
	append         "LR INCOME > 3000",Mailbody
	append         b1,Mailbody
	append         Unbilinc,Mailbody
	Append	CrLf,Mailbody
	Reset	Mailbody
        	move       "DHerric@nincal.com",Mailto
        	move       "DHerric@nincal.com",MailFrom
;        	call       SendMail
              winshow
         endif
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
         SUBTRACT  JULDAYS FROM CHKJUL
         COMPARE   "13" TO CHKJUL
         IF        GREATER
         MOVE      C1 TO AGEFLAG
         ENDIF
         GOTO      PRINT
.
MLRREAD
         PACK      MKEY FROM OMLRNUM,OCOBN
         REP       ZFILL,MKEY
         MOVE      C0 TO BATCHBR
         CALL      NMLRKEY
         CMATCH    "B" TO MCODE          .BATCH BILL ?
         IF        EQUAL                 .YES
           MOVE      C1 TO BATCHBR
           ELSE
         CMATCH    "A" TO MCODE          .BATCH BILL ?
         IF        EQUAL                 .YES
           MOVE      C1 TO BATCHBR
           else
           CLEAR   MCODE                   .MCODE WITH 'B' PRINTED ELSE IGNORE
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
.Start Patch #2.9 - replaced var
.PREPARING FOR FUTURE INCREASE OF SQUANT
.         MOVE      C0 TO N7
.         MOVE      SQUANT TO N7
.         COMPARE   C0 TO N7
         MOVE      C0 TO N9
         MOVE      SQUANT TO N9
         COMPARE   C0 TO N9
.END Patch #2.9 - replaced var
         IF        NOT EQUAL
         MOVE      SQUANT TO OQTY
         ENDIF
         RETURN
READMRG
         MOVE      OLRN TO NMRGFLD
         rep       zfill in nmrgfld
         MOVE      C1 TO NMRGPATH
         CALL      NMRGKEY
         RETURN    IF OVER
         move      nmrgiqty to oqty
         RETURN
special
.         move       no to specl
.         MATCH     "0020" TO OMLRNUM
.         IF         EQUAL
.         MOVE       C5 TO FORM52         *30jan02 turned off. full pricing 2/01.
.         move       yes to specl
.         return
.         ENDIF
.
         move       no to specl
         MATCH     "0170" TO OMLRNUM
         IF         EQUAL
         MOVE       C5 TO FORM52
         move       yes to specl
         return
         ENDIF

         MATCH     "0173" TO OMLRNUM
         IF         EQUAL
         MOVE       C5 TO FORM52
         move       yes to specl
         return
         ENDIF

         MATCH     "0179" TO OMLRNUM
         IF         EQUAL
         MOVE       C5 TO FORM52
         move       yes to specl
         return
         ENDIF

         MATCH     "0188" TO OMLRNUM
         IF         EQUAL
         MOVE       C5 TO FORM52
         move       yes to specl
         return
         ENDIF

         match      "0171" to obrknum
         IF         EQUAL
         MOVE       C5 TO FORM52
         move       yes to specl
         return
         ENDIF
.
         return
.......................................................................
BALAN
.MONEY ON ACCOUNT ?     *ADD IN THE FUTURE??????
         pack      nmoafld4 from obrknum,omlrnum
.         MOVE      MKEY TO CHKMLR
.         RESET     MKEY TO 4
.         APPEND    Z3 TO MKEY
.         RESET     MKEY
         move      c2 to nmobpath
         call      nmobkey
.         READ      ACCOUNT1,MKEY;MKEY,BALANCE
         COMPARE   C0 TO BALANCE
         GOTO      BALAN1 IF EQUAL
         GOTO      BALAN1 IF NOT LESS
         MULT      SEQ BY BALANCE
         ADD       BALANCE TO TOTALDOL
BALAN1
          goto      print
.         READKS    ACCOUNT1;MKEY,BALANCE
.         GOTO      PRINT IF OVER
         MOVE      MKEY TO COMPMLR
         MATCH     CHKMLR TO COMPMLR
         GOTO      BALAN1 IF NOT EQUAL
         MULT      SEQ BY BALANCE
         ADD       BALANCE TO TOTALDOL
.
*............................................................
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
*......................................................................
HEADER   ADD       C1 TO PAGE
         PRINT     *F,*1,"CONFIDENTIAL",*32,"N I N   U N B I L L":
                   " E D   O R D E R S   A S   O F",*84,"DATE:",PDATE:
                   *L,*1,PROGRAM,*121,"PAGE ## ",PAGE:
                   *L,*L,*7,"ORDER DATE",*23,"MLR##",*32,"LR":
                   *40,"MAILER NAME/OFFER",*68,"LIST OWNER##":
                   *87,"LIST OWNER NAME",*114,"MAIL DTE",*124:
                   "RETURN DT"
         MOVE      C4 TO LINES
         RETURN
*......................................................................
PRINT
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         COMPARE   C0 TO LINES
         CALL      HEADER IF EQUAL
.
.Start Patch #2.9 - add century
.         PRINT     *40,MCCTO:
.                   *L,*7,OODTEM,SLASH,OODTED,SLASH,OODTEY:
.                   *18,OMLRNUM,SLASH,OCOBN,*27,MCODE,*27,MCODE,*29,OLRN:
.                   *40,MCOMP,*74,OLON,*86,OWNOCPY,*114,OMDTEM,SLASH:
.                   OMDTED,SLASH,OMDTEY,*124,ORTNDTEM,SLASH,ORTNDTED:
.                   SLASH,ORTNDTEY;
         PRINT     *40,MCCTO:
                   *L,*7,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY:
                   *18,OMLRNUM,SLASH,OCOBN,*27,MCODE,*27,MCODE,*29,OLRN:
                   *40,MCOMP,*74,OLON,*86,OWNOCPY,*114,OMDTEM,SLASH:
                   OMDTED,SLASH,OMDTEC,OMDTEY,*124,ORTNDTEM,SLASH,ORTNDTED:
                   SLASH,ORTNDTEC,ORTNDTEY;
.end Patch #2.9 - add century
         COMPARE   C1 TO AGEFLAG
         IF        EQUAL
.Start Patch #2.9 - add century
.         PRINT     *113,STAR,OMDTEM,SLASH,OMDTED,SLASH,OMDTEY,STAR;
         PRINT     *113,STAR,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY,STAR;
.END Patch #2.9 - add century
         add       c1 to lines
         ENDIF
.START PATCH 3.43 ADDED LOGIC
.         PRINT     *L,*8,EXCHANGE,*22,SHIPPED,*40,OODES,B1,EXCLPRT:
.                   *86,OWNCTN,B3,FORM52,"/M",onetper,"%",*119,UNBILINC
.START PATCH 3.54 REPLACED LOGIC
.         PRINT     *L,*8,EXCHANGE,*22,SHIPPED,*40,OODES,B1,EXCLPRT:
.                   *86,NFULCOMP,B3,FORM52,"/M",onetper,"%",*119,UNBILINC
         PRINT     *L,*8,EXCHANGE,*22,SHIPPED,*40,OODES,B1,EXCLPRT:
                   *86,COMPCOMP,B3,FORM52,"/M",onetper,"%",*119,UNBILINC                    
.END PATCH 3.54 REPLACED LOGIC                   
.END PATCH 3.43 ADDED LOGIC
         ADD       UNBILINC TO UNBILTOT
         IFZ       PC
         PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,*109,"QUANTITY ";
.Start Patch #2.9 - replaced var
.         MOVE      "ZZZZZZZ" TO STR7
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         EDIT      N7 TO STR7
.         PRINT     *118,STR7;
.         PRINT     *118,STR7;
.         MOVE      "ZZZZZZZ" TO STR7
.         MOVE      C0 TO N7
.         MOVE      OEXQTY TO N7
.         EDIT      N7 TO STR7
.         PRINT     "/",STR7:
.                   *L,*1,*RPTCHAR,DASH:132
.         XIF
.         IFNZ      PC
.         PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,*109,"QUANTITY ";
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
..
         MOVE      "ZZZZZZZZZ" TO STR9
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         EDIT      N9 TO STR9
         PRINT     *118,STR9;
         PRINT     *118,STR9;
         MOVE      "ZZZZZZZZZ" TO STR9
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         EDIT      N9 TO STR9
         PRINT     "/",STR9:
                   *L,*1,*RPTCHAR,DASH:132
         XIF
         IFNZ      PC
         PRINT     *7,"LIST - ",O1DES,B1,O2DES,B1,COMSLCT,*109,"QUANTITY ";
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
.END Patch #2.9 - replaced var
         XIF
         ADD       C7 TO LINES
         GOTO      GETREC
         INCLUDE   NDATIO.inc
.patch3.46
				include	compio.inc
				include	cntio.inc
.         INCLUDE   NMLRIO.inc
.patch3.46
         INCLUDE   NSHPIO.inc
         INCLUDE   NORDIO.inc
         include   nmrgio.inc
         include   nmobio.inc
         include   nmoaio.inc
.START PATCH 3.0 - ADDED LOGIC
         INCLUDE   NOFRIO.INC
.END PATCH 3.0 - ADDED LOGIC
         INCLUDE   NOWNIO.inc
.START PATCH 3.54 REMOVED LOGIC
..START PATCH 3.43 ADDED LOGIC
.	INCLUDE	NFULIO.INC
..END PATCH 3.43 ADDED LOGIC
.END PATCH 3.54 REMOVED LOGIC
.START PATCH 3.42 ADDED LOGIC
         include   nmldio.inc
.END PATCH 3.42 ADDED LOGIC
.START PATCH 3.45 ADDED LOGIC
	INCLUDE	NSELIO.INC
	INCLUDE	NSEL2IO.INC
	INCLUDE	NTXTIO.INC
.END PATCH 3.45 ADDED LOGIC
         INCLUDE   COMLOGIC.inc
* ***************************************************************************
*  EXIT
* ****************************************************************************
.
EXIT     BRANCH    JOBBR OF EXIT1,EXIT2
EXIT1    BRANCH    PASS OF EXIT1A,EXIT1B
EXIT1A   MOVE      C2 TO PASS
         DISPLAY   *P01:23,"PASS TWO"
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
         PRINT     *L,"END TIME   ",TIME," ",COUNTO,"  ORDERS EXAMINED"
         SUB       COUNTO FROM COUNTO
         PRINT     *F
         MOVE      C0 TO UNBILTOT
         MOVE      C0 TO LINES
         MOVE      C0 TO PAGE
         MOVE      "ORDER FILE " TO FERROR
         CLOSE     NORDFILE
         MOVE      C0 TO NORDFLAG
*****************
*for testing only*************************************************
*****************
         MOVE      "151000" TO NORDFLD
         CALL      NORDTST
         GOTO      GETREC
EXIT1B   COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         PRINT     *L,*1,"TOTAL UNBILLED INCOME - ",UNBILTOT:
                   *L,*1,"THIS REPORT DOES NOT INCLUDE ANY UNBILLED ORDERS":
                   " WITH A MAIL DATE GREATER THAN THE REPORT DATE."
         PRINT     *L,"START TIME ",TIME
         CLOCK     TIME TO TIME
         PRINT     *L,"END TIME   ",TIME," ",COUNTO,"  ORDERS EXAMINED"
         PRINT     *F
         GOTO      EXIT3
EXIT2    DISPLAY   *P01:01,*ES,*P29:12,*HON,"B Y E !!!",*HOFF;
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         PRINT     *L,*1,"      TOTAL UNBILLED INCOME - ",UNBILTOT:
                   *L,*1,"TO BE BILLED WITHIN 30 DAYS -    ",UNLNC30:
                   *60,"BROKERAGE RENTAL 30 DAY   - ",BRKRNTOT:
                   *L,*1,"TO BE BILLED WITHIN 60 DAYS -    ",UNLNC60:
                   *60,"BROKERAGE EXCHANGE 30 DAY - ",BRKEXTOT:
                   *L,*1,"TO BE BILLED WITHIN 90 DAYS -    ",UNLNC90:
                   *60,"LIST MANAGEMENT 30 DAY    - ",LSTMTOT:
                   *L,*1,"TO BE BILLED WITHIN 90+ DAYS-    ",UNLNC90P:
                   *L,*1,"30 DAY BROKERAGE RENTAL BATCH -  ",LRBBR:
                   *L,*1,"30 DAY BROKERAGE EXCHANGE BATCH- ",LRBBE:
                   *L,*1,"THIS REPORT INCLUDES ALL UNBILLED ORDERS":
                   *L,*1,"ASSUMPTIONS:   ":
                   *L,*1,"LIST MANAGEMENT RENTAL COMMISSION 10%,":
                   " BROKERAGE RENTAL COMMISSION 20%":
                   *L,*1,"IF SPLIT RENTAL/EXCHANGE $/M PULLED FROM DATACARD,":
                   " IF $/M NOT ON DATACARD $65/M IS USED.":
                   *L,*1,"EXCHANGE VOLUME RATES ARE ACCOUNTED FOR":
                   *L,*1,"SHIPPED QUANTITY USED IF AVAILABLE",*L:
                   *L,*1,"START TIME ",TIME
         CLOCK     TIME TO TIME
         PRINT     *L,*1,"END TIME   ",TIME," ",COUNTO,"  ORDERS EXAMINED"
         PRINT     *F
              move          "Nord0013 - Totals output" to MailSubjct
.	
	Clear	Mailbody
	append         "Total Unbilled income       ",Mailbody
	append         Unbiltot,Mailbody
	Append	CRLF,Mailbody

	append         "Total 30 day                ",Mailbody
	append         Unlnc30,Mailbody
	Append	CRLF,Mailbody
.
	append         "30 day Brokerage rental     ",Mailbody
	append         Brkrntot,Mailbody
	Append	CRLF,Mailbody
.
	append         "30 day Brokerage Exchange   ",Mailbody
	append         BrkEXtot,Mailbody
	Append	CRLF,Mailbody
.
	append         "30 day List Management      ",Mailbody
	append         LstMtot,Mailbody
	Append	CRLF,Mailbody
.
	append         "60 day Total                ",Mailbody
	append         UNLNC60,Mailbody
	Append	CRLF,Mailbody
.
	append         "90 day Total                ",Mailbody
	append         UNLNC90,Mailbody
	Append	CRLF,Mailbody
.
	append         "90+ day Total               ",Mailbody
	append         UNLNC90P,Mailbody
	Append	CRLF,Mailbody
.
	append         "30 Batch Brokerage Rental   ",Mailbody
	append         LRBBR,Mailbody
	Append	CRLF,Mailbody
.
	append         "30 Batch Brokerage Exchange ",Mailbody
	append         LRBBE,Mailbody
	Append	CRLF,Mailbody

        	move       "DHerric@nincal.com" to MailFrom
        	move       "DHerric@nincal.com" to MailTo
	call	SendMail
              winshow
EXIT3    BEEP
         BRANCH    PRTFLAG OF STOP,EXIT4
EXIT4    SPLCLOSE
STOP     SHUTDOWN  "CLS"
.         STOP
ABORT    TRAPCLR   F5
         PRINT     *L,"*****JOB ABORTED BY OPERATOR"
         GOTO      EXIT3
* ***************************************************************************
*  ERROR SUBROUTINES
* ****************************************************************************
.
IO
         TRAPCLR   IO
         NORETURN
         DISPLAY   *P1:23,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!";
         BEEP
         PRINT     *L,"*** JOB ABORTED - I/O ERROR"
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
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
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
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
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
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
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT IF EQUAL
         GOTO      PARITY
