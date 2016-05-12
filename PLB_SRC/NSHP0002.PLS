PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORD2DD.inc
;Patch1.8
.         INCLUDE   NMLRDD.inc
              INCLUDE         compdd.inc
              include         cntdd.inc
;Patch1.8
         INCLUDE   NSHPDD.inc
         INCLUDE   NOWNDD.inc
         include   hp.inc
.START PATCH 1.5 - ADDED LOGIC
         include   nofrdd.inc
.END PATCH 1.5 - ADDED LOGIC
.START PATCH 1.81 REMOVED LOGIC
..START PATCH 1.71 ADDED LOGIC
.              INCLUDE         NFULDD.INC
..END PATCH 1.71 ADDED LOGIC
.END PATCH 1.81 REMOVED LOGIC
.START PATCH 1.72 ADDED LOGIC
              INCLUDE         NSEL2DD.INC
.END PATCH 1.72 ADDED LOGIC
.
. .............................................................................
Release       init            "1.83"         ASH            19JUN2007      PLI Inclusion
reldate       init            "JULY 19, 2007"
.Release       init            "1.82"         DMB            12OCT2006      Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.release       init            "1.81"         DMS            22JUN2006      Fulfillment Conversion
.release       init            "1.81"         DMS            22JUN2006      Fulfillment Conversion
.release       init            "1.80"         DMB            26MAY2004      Mailer Conversion
.release      init            "1.72"         ASH            29JAN2004  DATACARD CONVERSION
.reldate       init            "JANUARY 29, 2004"
.release  init      "1.71"              ASH 05FEB2002 NINFUL CONVERSION
.release  init      "1.7"              ASH 02OCT2000 NEW SERVER ADDED
.release  init      "1.6"              ASH 10JUN99 NINSHP Y2K, File expansion
.RELEASE  INIT      "1.5"              ASH 07MAY99 REPLACED OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.RELEASE  INIT      "1.4"              ASH 15JAN99 NINORD Y2K, File expansion
.release  init      "1.3"              DLH 15Jul98 Y2
.RELEASE  INIT      "1.2"              DLH 13MAR92 NEW INCLUDES/PCBUS
.RELEASE INIT      "1.0"               WRITTEN 8/5/91  JD.
;* NAMES IN THE NEWS SHIPPING REPORT PROGRAM      05AUG91
. .............................................................................
.
. WORK VARIABLES
.
HUND     FORM      "100"
PDATE    DIM       8
SHPDATE  DIM       8
TELEMASK INIT      "(999)999-9999"
.Start Patch #1.4 - increased vars
.ORDMASK INIT       "Z,ZZ9,999"
.ORDQTY   DIM       9
.QTYNUM   FORM      7
ORDMASK INIT       "ZZZ,ZZ9,999"
ORDQTY   DIM       11
QTYNUM   FORM      9
.End Patch #1.4 - increased vars
.START PATCH 1.6 - REPLACED LOGIC
.SHPMASK INIT       "Z,ZZ9,999"
.SHPQTY   DIM       9
.SHPNUM   FORM      7
SHPMASK INIT       "ZZZ,ZZ9,999"
SHPQTY   DIM       11
SHPNUM   FORM      9
.END PATCH 1.6 - REPLACED LOGIC
POSTMASK INIT     "$99.99"
SYSJDATE FORM      5
POSTAMT  DIM       6
POSTNUM  FORM      3.2
FORM42   FORM      4.2
JOBBR    FORM      1               BRANCH FOR JOB TYPE SEPERATE,TOTAL
YR       DIM       2
TELE1    DIM       13
CODENUM  FORM      2
.
PROGNAME DIM       8
.
. .............................................................................
.
.
. PROGRAM VARIABLES
. .................
.
ANS      DIM       1
DATE     DIM       8
TIME     DIM       8
PASS     FORM      3                  FOR JOBBR = 1 NOT SHIPPED
.                                     FOR JOBBR = 2 ALL ORDERS
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
HOLDOWN  DIM       4
LINES    FORM      2
PAGE     FORM      5
PBREAK   FORM      "46"
SHIPPED  DIM       9
EXCHANGE DIM       10
COMSLCT  DIM       9
COUNTO   FORM      5                  NUMBER OF ORDERS READ.
shipping   DIM       1
.START PATCH 1.81 ADDED LOGIC
NFULCOMP	DIM	55
.END PATCH 1.81 ADDED LOGIC
.
. .............................................................................
*******************************************************************************
. MAINLINE
. .............................................................................
         TRAP      EXIT IF F3
         MOVE      "EXIT" TO PF3
         MOVE      "NSHP0002" TO PROGRAM
         MOVE      "SHIPPING REPORT" TO STITLE
         MOVE      "NINCAL" TO COMPNME
         CALL      PAINT
         CALL      FUNCDISP
         MOVE      "DISKIN70" TO NORDNAME
         MOVE      "                    " TO FERROR
         move      c2 to nshplock
.START PATCH 1.7 REPLACED LOGIC
.         SPLOPEN   "g:\DATA\DISKIN70.LST"
         PACK   STR35,NTWKPATH1,"DISKIN70.LST"
         SPLOPEN   STR35
.END PATCH 1.7 REPLACED LOGIC
         PRINT     hp17ptch,hptop                .compressed
.         CLOCK     TIME TO TIME
*****************
*****************
.
START    IFNZ      PC
         CLOCK     DATE TO DATE
         MOVE      "99/99/99" TO PDATE
         EDIT      DATE TO PDATE
         UNPACK    DATE INTO MM,DD,YY
         CALL      CVTJUL
         MOVE      JULDAYS TO SYSJDATE
         XIF
.
         IFZ       PC
         CLOCK     DATE TO PDATE
         UNPACK    DATE INTO MM,ANS,DD,ANS,YY
         CALL      CVTJUL
         MOVE      JULDAYS TO SYSJDATE
         XIF
         MOVE      C0 TO LINES
         MOVE      "N" TO ANS
         KEYIN     *P28:09,"(N)ot SHIPPED  or (A)ll ",*T10,*RV,ANS
         REP       "N1A2" IN ANS
         MOVE      ANS TO JOBBR
         BRANCH    JOBBR OF START1,START1         >1 OR 2 ?
         GOTO      START                            NO!
.
START1
.
GETREC   DISPLAY   *P01:24,*EL,*HON,"R-E-A-D-I-N-G",*HOFF;
.
         CALL      NORD2SEQ
.         FILEPI    1;NINORD
.         READ      NINORD,SEQ;ORCODE,OSTAT,OMLRNUM,OLRN,OCOBN,OLNUM,OLON:
.                   OMLRPON,OQTY,OPPM,OMLRKY,OFOCODE,ORTNDTEM,ORTNDTED,ORTNDTEY
.                   OMDTEM,OMDTED,OMDTEY,OTOCODE:
.                   OSOTCODE,OCCODE,OLRNCO,OODTECOM,OODTECOD,OODTECOY:
.                   OQTYCO,OSPI,OBLANK:
.                 OELCODE,OODNUM,OODES,ORTNNUM,OTAPERET,OUQTY,OSALES10,OCOCODE:
.                   OODTEM,OODTED,OODTEY:
.                   OSCODE,OCOMSLCT,OSHP,O1DES,O2DES,OSALES:
.                   OREUSE,ODOWJ,OEXQTY,GUARCODE
.
         GOTO      EXIT IF OVER
.START PATCH 1.5 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 1.5 - NEW LOGIC
.START PATCH 1.72 ADDED LOGIC
              packkey         NSEL2FLD,"1",OLRN
              move            "NSEL2KEY",Location
              pack            KeyLocation,"Key: ",NSEL2FLD
              call            NSEL2KEY
              if over
               move           O2DES,NSEL2NAME
              endif
.END PATCH 1.72 ADDED LOGIC
         ADD       C1 TO COUNTO
         DISPLAY   *P15:10,*EL,"RECORDS IN = ",COUNTO;
         RESET     CANCODES               *RESET FORM POINTER.
         SCAN      OSTAT IN CANCODES       *CANCELLED?
         GOTO      GETREC IF EQUAL
         move      "pxlz" to str4
         scan      ostat in str4
         goto      getrec if equal
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         GOTO      GETREC IF EQUAL
         CALL      READSHIP
         MOVE      OLON TO NOWNFLD
         REP       ZFILL IN NOWNFLD
         CALL      NOWNKEY
.START PATCH 1.71 REPLACED LOGIC
.         SCAN      "TDMC" IN OWNCTN
.Start Patch 1.34 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
.              call            Trim using OWNCTN
              call            Trim using OFULLFIL
.End Patch 1.34 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	              
.START PATCH 1.81 REPLACED LOGIC
.              if (OWNCTN <> "")
.               pack           NFULFLD,OWNCTN
.               rep            zfill,NFULFLD
.               move           C1,NFULPATH
.               move           "GETREC-NFULKEY",Location
.               pack           KeyLocation,NFULFLD
.               call           NFULKEY
.              else
.               clear          NFULFLD
.               clear          NFULCOMP
.              endif
.              scan            "TDMC",NFULCOMP
.Start Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
.              if (OWNCTN <> "")
.		       pack           COMPFLD6,OWNCTN
.		       rep            zfill,COMPFLD6
.		       move           C1,COMPPATH
.		       move           "GETREC-COMPKEY6",Location
.		       pack           KeyLocation,COMPFLD6
.		       call           COMPKEY6
.			if over
.				clear	NFULCOMP
.			else
.				if (COMPSVBFLG <> "T")
.					clear	NFULCOMP
.				else
.					move	COMPCOMP,NFULCOMP
.				endif
.			endif
.              else	// OWNCTN = ""
.			clear	NFULCOMP
.              endif
.              scan	"TDMC",NFULCOMP
.	       GOTO      GETREC IF EQUAL
.End Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	              
.Start Patch 1.34 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	              
		if (OFULLFIL <> "")
		        pack	COMPFLD,OFULLFIL 
		        call	zfillit using COMPFLD
		        move	C1,COMPPATH
		        move	"GETREC-COMPKEY",Location
		        pack	KeyLocation,COMPFLD
		        call	COMPKEY		
			if over
				clear	NFULCOMP
			else
				move	COMPCOMP,NFULCOMP
			endif
		else	// OFULLFIL = ""
			clear	NFULCOMP
		endif
		Goto	GETREC If (OFULLFIL = "009406")	.Donnelley/Triplex/TDMC
.End Patch 1.34 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	              
 
.END PATCH 1.81 REMOVED LOGIC
.END PATCH 1.71 REPLACED LOGIC
         MATCH     NOWNFLD TO HOLDOWN
         CALL      BREAK IF NOT EQUAL
         CALL      MLRREAD
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         GOTO      CHECKOK IF EQUAL
         CLEAR     EXCHANGE
         GOTO      DISSCOM
CHECKOK
         MOVE      "*EXCHANGE*" TO EXCHANGE
DISSCOM
         CLEAR     COMSLCT
         CMATCH    "C",OCOMSLCT
         IF        EQUAL
         MOVE      "COMSELECT" TO COMSLCT
         ENDIF
         NORETURN
         GOTO      PRINT
.
MLRREAD
         PACK      MKEY FROM OMLRNUM,OCOBN
         REP       ZFILL,MKEY
         CALL      NMLRKEY
         CALL      OVER IF OVER
         RETURN
.
.
READSHIP CLEAR     SHIPPED
         MOVE      OLRN TO NSHPFLD
         rep       zfill in nshpfld
         CALL      NSHPKEY
         IF        OVER
         MOVE      NO TO shipping
         RETURN
         ELSE
         MOVE      YES TO shipping
         RETURN
         ENDIF
.
.
*......................................................................
HEADER   ADD       C1 TO PAGE
         MOVE      TELEMASK TO TELE1
         EDIT      OWNTELE TO TELE1
.START PATCH 1.71 REPLACED LOGIC
.         PRINT     *F,*1,"CONFIDENTIAL",*32,"N A M E S   I N   T H E ":
.                   " N E W S   S H I P M E N T  R E P O R T",*119,"DATE:":
.                   PDATE:
.                   *L,*119,"PAGE ## ",PAGE:
.                   *L,*L,*7,"LIST OWNER ##",*21,OLON:
.                   *L,*21,OWNLONM:
.                   *L,*21,OWNOCPY,TELE1:
.                   *L,*21,OWNCTN
         PRINT     *F,*1,"CONFIDENTIAL",*32,"N A M E S   I N   T H E ":
                   " N E W S   S H I P M E N T  R E P O R T",*119,"DATE:":
                   PDATE:
                   *L,*119,"PAGE ## ",PAGE:
                   *L,*L,*7,"LIST OWNER ##",*21,OLON:
                   *L,*21,OWNLONM:
                   *L,*21,OWNOCPY,TELE1:
                   *L,*21,NFULCOMP
.END PATCH 1.71 REPLACED LOGIC
         PRINT     *N,*50,"RTN/MAIL",*60,"QUANTITY",*72,"SHIPPED":
                   *81,"POSTAGE":
                   *L,*1,"LR ##",*10,"MAILER/BROKER/OFFER/LIST",*52,"DATE":
                   *60,"ORD/SHIP",*74,"DATE",*81,"AMOUNT":
                   *93,"SHIPPING INFORMATION"
         MOVE      C10 TO LINES
         RETURN
*......................................................................
PRINT
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         COMPARE   C0 TO LINES
         CALL      HEADER IF EQUAL
         COMPARE    C1 TO JOBBR
         IF        EQUAL
         CMATCH    YES TO shipping
         IF        EQUAL
         GOTO      GETREC
         ENDIF
         ENDIF
         MOVE      ORDMASK TO ORDQTY
         MOVE      OQTY TO QTYNUM
         EDIT      QTYNUM TO ORDQTY
         CMATCH    YES TO shipping
         IF        EQUAL
         MOVE      "99/99/99" TO SHPDATE
         unpack    sdate into cc,yy,mm,dd
.         MOVE      SDATE TO N6
         pack      str6 from mm,dd,yy
         move     str6 to n6
         EDIT      N6 TO SHPDATE
         MOVE      POSTMASK TO POSTAMT
         MOVE      C0 TO FORM42
         MOVE      SPOST TO FORM42
         DIV       HUND INTO FORM42
         MOVE      FORM42 TO POSTNUM
         EDIT      POSTNUM TO POSTAMT
         MOVE      SHPMASK TO SHPQTY
         MOVE      C0 TO SHPNUM
         MOVE      SQUANT TO SHPNUM
         EDIT      SHPNUM TO SHPQTY
         ELSE
         MOVE      B8 TO SHPDATE
         MOVE      B8 TO POSTAMT
         MOVE      B9 TO SHPQTY
         ENDIF
DETAIL
.Start Patch #1.4 - increased vars
.         PRINT     *L,*1,OLRN,*50,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEY:
.                   *60,ORDQTY,*71,SHPDATE,*82,POSTAMT,*91,SINFO:
.                   *L,*10,MCOMP,*50,OMDTEM,SLASH,OMDTED,SLASH,OMDTEY:
.                   *60,SHPQTY,*109,COMSLCT,*119,EXCHANGE:
.                   *L,*10,OODES:
.                   *L,*1,OLNUM,*10,O1DES:
.                   *L,*10,O2DES:
.                   *L
.START PATCH 1.5 - REPLACED LOGIC, OODES --> OFDESC
.         PRINT     *L,*1,OLRN,*48,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY:
.                   *58,ORDQTY,*71,SHPDATE,*82,POSTAMT,*91,SINFO:
.                   *L,*10,MCOMP,*48,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY:
.                   *60,SHPQTY,*109,COMSLCT,*119,EXCHANGE:
.                   *L,*10,OODES:
.                   *L,*1,OLNUM,*10,O1DES:
.                   *L,*10,O2DES:
.                   *L
.START PATCH 1.72 ADDED LOGIC
.         PRINT     *L,*1,OLRN,*48,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY:
.                   *58,ORDQTY,*71,SHPDATE,*82,POSTAMT,*91,SINFO:
.                   *L,*10,MCOMP,*48,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY:
.                   *60,SHPQTY,*109,COMSLCT,*119,EXCHANGE:
.                   *L,*10,OFDESC:
.                   *L,*1,OLNUM,*10,O1DES:
.                   *L,*10,O2DES:
.                   *L
.START PATCH 1.83 REPLACED LOGIC
.         PRINT     *L,*1,OLRN,*48,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY:
.                   *58,ORDQTY,*71,SHPDATE,*82,POSTAMT,*91,SINFO:
.                   *L,*10,MCOMP,*48,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY:
.                   *60,SHPQTY,*109,COMSLCT,*119,EXCHANGE:
.                   *L,*10,OFDESC:
.                   *L,*1,OLNUM,*10,O1DES:
.                   *L,*10,NSEL2NAME:
.                   *L
	if (OCompID = "P" | OCompID2 = "P")
		move	" (PLI)",str6
	else
		clear	str6
	endif
         PRINT     *L,*1,OLRN,str6,*48,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY:
                   *58,ORDQTY,*71,SHPDATE,*82,POSTAMT,*91,SINFO:
                   *L,*10,MCOMP,*48,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY:
                   *60,SHPQTY,*109,COMSLCT,*119,EXCHANGE:
                   *L,*10,OFDESC:
                   *L,*1,OLNUM,*10,O1DES:
                   *L,*10,NSEL2NAME:
                   *L
.END PATCH 1.83 REPLACED LOGIC
.END PATCH 1.72 ADDED LOGIC
.end PATCH 1.5 - REPLACED LOGIC, OODES --> OFDESC
.End Patch #1.4 - increased vars
         ADD       C6 TO LINES
         GOTO      GETREC
BREAK
         CALL      NOWNKEY
.Start Patch 1.34 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	         
.START PATCH 1.71 ADDED LOGIC
.              call            Trim using OWNCTN
	 call	Trim using OFULLFIL
.START PATCH 1.81 REPLACED LOGIC
.End Patch 1.34 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
.              if (OWNCTN <> "")
.               pack           NFULFLD,OWNCTN
.               rep            zfill,NFULFLD
.               move           C1,NFULPATH
.               move           "BREAK-NFULKEY",Location
.               pack           KeyLocation,NFULFLD
.               call           NFULKEY
.              else
.               clear          NFULFLD
.               clear          NFULCOMP
.              endif
.Start Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	              
.              if (OWNCTN <> "")
.		       pack           COMPFLD6,OWNCTN
.		       rep            zfill,COMPFLD6
.		       move           C1,COMPPATH
.		       move           "BREAK-COMPKEY6",Location
.		       pack           KeyLocation,COMPFLD6
.		       call           COMPKEY6
.		       if over
.		       		clear          NFULCOMP
.		       else
.		       		if (COMPSVBFLG <> "T")
.		       			clear          NFULCOMP
.		       		else
.		       			move	COMPCOMP,NFULCOMP
.		       		endif
.		       endif
.              else	// OWNCTN = ""
.		       clear          NFULCOMP
.              endif
.End Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	              
.Start Patch 1.34 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	              
              if (OFULLFIL <> "")
		       pack     COMPFLD,OFULLFIL
		       call	zfillit using COMPFLD
		       move     C1,COMPPATH
		       move     "BREAK-COMPKEY",Location
		       pack     KeyLocation,COMPFLD
		       call     COMPKEY
		       if over
		       		clear	NFULCOMP
		       else
	       			move	COMPCOMP,NFULCOMP
		       endif
              else	// OWNCTN = ""
		       clear          NFULCOMP
              endif
.End Patch 1.34 Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	              
.END PATCH 1.81 REPLACED LOGIC
.END PATCH 1.71 ADDED LOGIC
         CALL      HEADER
         MOVE      NOWNFLD TO HOLDOWN
         RETURN
* ***************************************************************************
*  EXIT
* ****************************************************************************
EXIT
          SPLCLOSE
          shutdown
          STOP
EXIT1     shutdown
          STOP
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
         KEYIN     *P70:24,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      EXIT1 IF EQUAL
         GOTO      IO
RANGE
         TRAPCLR   RANGE
         NORETURN
         DISPLAY   *P1:24,*EL,"RANGE ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      EXIT1 IF EQUAL
         GOTO      RANGE
FORMAT
         TRAPCLR   FORMAT
         NORETURN
         DISPLAY   *P1:24,*EL,"FORMAT ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      EXIT1 IF EQUAL
         GOTO      FORMAT
PARITY
         TRAPCLR   PARITY
         NORETURN
         DISPLAY   *P1:24,*EL,"PARITY ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      EXIT1 IF EQUAL
         GOTO      PARITY
..........................................................................
         INCLUDE   NSHPIO.inc
         INCLUDE   NOWNIO.inc
;Patch1.8
.         INCLUDE   NMLRIO.inc
                              INCLUDE        compio.inc
                              include        cntio.inc
;Patch1.8
         INCLUDE   NORD2IO.inc
.START PATCH 1.5 - ADDED LOGIC
         include   nofrio.inc
.END PATCH 1.5 - ADDED LOGIC
.START PATCH 1.81 REMOVED LOGIC
..START PATCH 1.71 ADDED LOGIC
.              INCLUDE         NFULIO.INC
..END PATCH 1.71 ADDED LOGIC
.END PATCH 1.81 REMOVED LOGIC
.START PATCH 1.72 ADDED LOGIC
              INCLUDE         NSEL2IO.INC
.END PATCH 1.72 ADDED LOGIC
         INCLUDE   COMLOGIC.inc
