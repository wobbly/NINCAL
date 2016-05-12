. TDMCREV - PROGRAM TO REVIEW AND DELETE RECORDS DESTINED TO BE TC'D
.            TO TRIPLEX.
.
. .....................
. RECORD TYPES INCLUDE:
.
.       1)ORDER.
.       2)INPUT FILE - LR NUMBER & STATUS.
. .............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
.patch2.75
				include	compdd.inc
				include	cntdd.inc
.         INCLUDE   NMLRDD.inc
.patch2.75
         INCLUDE   NOWNDD.inc
         INCLUDE   NRTNDD.inc
         include   MEDIA.inc
.START PATCH 2.6 - ADDED LOGIC
         INCLUDE   NOFRDD.INC
.END PATCH 2.6 - ADDED LOGIC
.START PATCH 2.77 REMOVED LOGIC
..START PATCH 2.73 ADDED LOGIC
.	INCLUDE	NFULDD.INC
..END PATCH 2.73 ADDED LOGIC
.END PATCH 2.77 REMOVED LOGIC
.START PATCH 2.74 ADDED LOGIC
	INCLUDE	NSEL2DD.INC
	INCLUDE	NMODDD.INC
.END PATCH 2.74 ADDED LOGIC
.
release	 init	   "2.78"	 DMB    12OCT2006 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.release	 init	   "2.77"	 DMS    21JUN2006 	Fulfillment Conversion
.release  init      "2.76"        DMB	18JUN2005 FM IP CHG
.release  init      "2.75"        DMB	26MAY2004	Mailer Conversion
.RELEASE   INIT      "2.74"           ASH  29JAN2004  DATACARD CONVERSION
.RELEASE   INIT      "2.73"           ASH  04FEB2002	NINFUL CONVERSION
.RELEASE   INIT      "2.72"           ASH  01FEB2002	ADDED TDMCORD.DAT TO FILE MANAGER
.RELEASE   INIT      "2.71"           JD  17JUL2001 reread file before update.
.RELEASE   INIT      "2.7"           ASH 01JUN2000 ASH Added Media types
.RELEASE   INIT      "2.6"           ASH 06MAY99 Replaced OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.                                               Plus posthumous correction for DISSORD section!!
.RELEASE   INIT      "2.5"           ASH 04JAN99 NINORD Y2K, File expansion
.release   init      "2.4"           ASH 15SEP98 NINMLR Y2K file expansion
.RELEASE  INIT      "2.3"             JD  17jan97   if cancelled order, send lol
.RELEASE  INIT      "2.2"            JD  11APR93   EXPANDED MEDIA.inc.
.RELEASE  INIT      "2.1"           DLH 08APR92   LIFESTYLE& IC SYSTEMS OVERLAY.
.
.RELEASE  INIT      "2.0"           DLH 17MAR92 INCLUDES: NORDXX,, NMLRXX,
.                                  REMOVE UNNEC. OFFER INCLUDES, NOWNXX,
.                                  NRTNXX,
.........................................
DESC0L1  DIM       47
DESC0L2  DIM       47
DESC991  DIM       47
DESC992  DIM       47
DESC981  DIM       47
DESC982  DIM       47
. .........................................
.
. FILE DEFINITIONS.
. .................
.TDMCORD  IFILE      *TRIPLEX ORDER FILE INDEXED ON LR##.
TDMCORD  IFILE     KEYLEN=6,VAR=7,COMP
.Start Patch #2.4 file expanded to compensate for expansion of MCOMP
.DELETE   IFILE     KEYLEN=6,FIX=76      *LR'S DELETED FROM T/C FILE.
DELETE   IFILE     KEYLEN=6,FIX=96      *LR'S DELETED FROM T/C FILE.
.End Patch #2.4 file expanded to compensate for expansion of MCOMP
.
. OTHER PROGRAM VARIABLES.
. ........................
.
ANS      DIM       1
FILE     FORM      1           BRANCHING CONSTANT FOR I/O TRAPS.
ZERO     FORM      "0"
ONE      FORM      "1"
TWO      FORM      "2"
THREE    FORM      "3"
MO1      DIM       2
DAY1     DIM       2
YR1      DIM       2
MO       DIM       2
DAY      DIM       2
YR       DIM       2
.CMSPEROD DIM       1
.SEQUENC  DIM       3
MERGE    DIM       1
TEAM     DIM       1
KEYMD    DIM       6           KEY FIELD FOR ORDER READ.
HOLDKEY  DIM       6           USED FOR RECORD VERIFICATION ORDERS & LCR'S.
MEDIANUM FORM      1           USED FOR TABLE READ.
.Start Patch #2.5 - increased var
.NWORK1   FORM      1           BRANCH STATEMENT FOR ORDER SECTION.
NWORK1   FORM      2           BRANCH STATEMENT FOR ORDER SECTION.
.End Patch #2.5 - increased var
F3       DIM       3           DISPLAY ORDER PRICE;
F2       DIM       2           DISPLAY ORDER PRICE;
STAT     DIM       1
DATE     DIM       8
NFIELD23 FORM      3.2
NUM      FORM      1
+ *****************************************************************************
. PROGRAM MAIN.
. .............
START    DISPLAY   *P1:1,*ES,*P24:01,"N A M E S   I N   T H E   N E W S":
                   *P30:2,"TRIPLEX REVIEW PROGRAM";
         TRAP      IO IF IO
         MOVE      ONE TO FILE
.START PATCH 2.72 REPLACED LOGIC
.         OPEN      TDMCORD,"TDMCORD"
.>Patch 2.76 Begin
.         OPEN      TDMCORD,"TDMCORD.isi|20.20.30.103:502"
         OPEN      TDMCORD,"TDMCORD.isi|10.10.30.103:502"
.>Patch 2.76 END         
.END PATCH 2.72 REPLACED LOGIC
         ADD       ONE TO FILE
         IFNZ      PC
         OPEN      DELETE,"TDMCDELETE"
         XIF
         IFZ      PC
         OPEN      DELETE,"TCDELETE"
         XIF
         MOVE      ZERO TO FILE
        TRAPCLR    IO
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,SLASH,DD,SLASH,YY
         MOVE      DATE TO TODAY
         XIF
         MOVE      C1 TO NORDPATH         .SET ACCESS TO ISAM KEY
+ *****************************************************************************
. ORDER SECTION.
. ..............
.START PATCH 2.6 - REPLACED LOGIC FOR DISPLAY PURPOSES
.DISSORD  DISPLAY   *ES,"(1)Mlr##:____ Brk##:___ (2)PO:_______ (3)Rtn-":
.                  *P44:1,"To##:____(5)List##:______(6)Owner##:____":
.                  *P01:07,"(4)Ofr##:___ __________________________________":
.                  "______ (7)Universe Qty:_______":
.                  *P35:08,"   List:___________________________________":
.                  *P01:09,"(9)Continuation?:_ LR:______       (8)Sel:____":
.                  "_______________________________":
.                  *P4:10,"Dat:__/__/__ Qty:______",*P34:10,"(20)Special ":
.                  "Instructions: __ __ __ __ __ __":
.                  *P01:12,"(10)Test?:_ Entire?:_ Exch?:_":
.                  *P01:13,"(11)Xsec?:_ Nth Nm?:_ Othr?:_":
.                  *P01:14,"(12)Order Qty:_______":
.                  *P01:15,"(13)Price /M :___.__":
.                  *P01:16,"(14)Key Info :________":
.                  *P01:17,"(24)Comselect?:_":
.                  *P01:18,"(15)Rtn:__/__/__ MD:__/__/__":
.                  *P01:19,"(23)Salesmn:_":
.                  *P01:20,"(16)Media  :_":
.                  *P01:21,"(25)Rt-tape:_":
.                  *P01:22,"(17)Sample :_":
.                  *P01:23,":(18)Ship   :_",*P1:24,"(19)Contact:_";
DISSORD  DISPLAY   *ES,"(1)Mlr##:____ Brk##:___ (2)PO:_______ (3)Rtn-":
                   *P44:1,"To##:____(5)List##:______(6)Owner##:____":
                   *P01:07,"(4)Ofr##:___ __________________________________":
                   "______ (7)Universe Qty:_______":
                   *P35:08,"   List:___________________________________":
                   *P01:09,"(9)Continuation?:_ LR:______       (8)Sel:____":
                   "_______________________________":
                   *P4:10,"Dat:__/__/____ Qty:_________",*P34:10,"(20)Special ":
                   "Instructions: __ __ __ __ __ __":
                   *P01:12,"(10)Test?:_ Entire?:_ Exch?:_":
                   *P01:13,"(11)Xsec?:_ Nth Nm?:_ Othr?:_":
                   *P01:14,"(12)Order Qty:_________":
                   *P01:15,"(13)Price /M :___.__":
                   *P01:16,"(14)Key Info :________":
                   *P01:17,"(24)Comselect?:_":
                   *P01:18,"(15)Rtn:__/__/____ MD:__/__/____":
                   *P01:19,"(23)Salesmn:_":
                   *P01:20,"(16)Media  :__":
                   *P01:21,"(25)Rt-tape:_":
                   *P01:22,"(17)Sample :_":
                   *P01:23,":(18)Ship   :__",*P1:24,"(19)Contact:_";
.END PATCH 2.6 - REPLACED LOGIC FOR DISPLAY PURPOSES
         GOTO      ENTERKO
.
TRYAGNO  DISPLAY   *P51:23,"not found, please try again.",*W2;
ENTERKO
         CLEAR     KEYMD
         KEYIN     *P35:23,*EL,"Enter L.R.##: ",*ZF,*JR,*RV,*T60,KEYMD;
         GOTO      START IF LESS
         CMATCH    " ",KEYMD
         GOTO      ENTERKO IF EOS
         MATCH     "00000*" TO KEYMD
         STOP      IF EQUAL
         MOVE      KEYMD TO HOLDKEY
.
READ     DISPLAY   *P1:24,*EL,"READING TDMC ORDER FILE";
         FILEPI    1;TDMCORD                                                                            
         READ      TDMCORD,KEYMD;KEYMD,STAT
         GOTO      TRYAGNO IF OVER
         MOVE      KEYMD TO NORDFLD
         CALL      NORDKEY
.
         GOTO      TRYAGNO IF OVER
.START PATCH 2.6 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 2.6 - NEW LOGIC
.START PATCH 2.74 ADDED LOGIC
	packkey	NSEL2FLD,"1",OLRN
	move	"NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if not over
		move	NSEL2QTY,str10
	else
		move	O2DES,NSEL2NAME
		move	OUQTY,str10
		unpack	OPPM,str3,str2
		pack	str6,str3,".",str2
		rep	zfill,str6
		move	str6,NSEL2PRICE
	endif
.END PATCH 2.74 ADDED LOGIC
.
+ DISPLAY VARIABLES FROM ACCESSED RECORD;
.
MATCHF3  DISPLAY   *P9:1,OMLRNUM,*P19:1,OCOBN,*P29:1,OMLRPON,*P48:1,ORTNNUM;
NMLR     PACK      MKEY FROM OMLRNUM,OCOBN
CM1      REP       " 0" IN MKEY
         CALL      NMLRKEY
.
.
         BUMP      OODNUM BY 4
.START PATCH 2.6 - REPLACED LOGIC, OODES --> OFDESC
.         DISPLAY   *P1:2,MNAME,*P1:3,MCOMP,*P1:4,MADDR,*P1:5:
.                   MCITY,    " ",MSTATE,MZIP,*P4:6,MCCTO:
.                   *P9:7,OODNUM," ",OODES,*P61:1,OLNUM,*P77:1,OLON;
         DISPLAY   *P1:2,MNAME,*P1:3,MCOMP,*P1:4,MADDR,*P1:5:
                   MCITY,    " ",MSTATE,MZIP,*P4:6,MCCTO:
                   *P9:7,OODNUM," ",OFDESC,*P61:1,OLNUM,*P77:1,OLON;
.END PATCH 2.6 - REPLACED LOGIC, OODES --> OFDESC
.
RDRTN    MOVE      ORTNNUM TO NRTNFLD
         CALL      NRTNKEY
         GOTO      DISRTN IF NOT OVER
         DISPLAY   *P28:3,"RETURN-TO RECORD NOT FOUND";
DISRTN
.START PATCH 2.6 - REPLACED LOGIC, OODES --> OFDESC
.         DISPLAY   *P28:2,OODES,*P28:3,RTCOMP,*P28:4,RTADDR,*P28:5,RTCITY:
.                   " ",RTSTATE,RTZIP;
         DISPLAY   *P28:2,OFDESC,*P28:3,RTCOMP,*P28:4,RTADDR,*P28:5,RTCITY:
                   " ",RTSTATE,RTZIP;
.END PATCH 2.6 - REPLACED LOGIC, OODES --> OFDESC
.
         MOVE      OLON TO NOWNFLD
         CALL      NOWNKEY
.START PATCH 2.73 REPLACED LOGIC
.         DISPLAY   *P56:2,OWNLONM,*P56:3,OWNOCPY,*P56:4,OWNLOSA,*P56:5:
.                   OWNLOCTY," ",OWNLOS,OWNLOZC,*P58:6,OWNCTN;
.Start Patch 2.78 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.	
.	call	Trim using OWNCTN
	call	Trim using OFULLFIL
.End Patch 2.78 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.START PATCH 2.77 REMOVED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"DISRTN-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.	endif
.	DISPLAY	*P56:2,OWNLONM,*P56:3,OWNOCPY,*P56:4,OWNLOSA,*P56:5:
.		OWNLOCTY," ",OWNLOS,OWNLOZC,*P58:6,NFULCOMP;
.Start Patch 2.78 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.	if (OWNCTN <> "")
.		pack	COMPFLD6,OWNCTN
.		rep	zfill,COMPFLD6
.		move	C1,COMPPATH
.		move	"DISRTN-COMPKEY6",Location
.		pack	KeyLocation,COMPFLD6
.		call	COMPKEY6
.			if over
.				clear	COMPFLD6
.				clear	COMPCOMP
.			else
.				if (COMPSVBFLG <> "T")
.					clear	COMPFLD6
.					clear	COMPCOMP
.				endif
.			endif
.	else 	// OWNCTN = ""
.		clear	COMPFLD6
.		clear	COMPCOMP
.	endif
.End Patch 2.78 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
.Start Patch 2.78 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
	if (OFULLFIL <> "")
		pack	COMPFLD,OFULLFIL
		call	zfillit using COMPFLD
		move	C1,COMPPATH
		move	"DISRTN-COMPKEY",Location
		pack	KeyLocation,COMPFLD
		call	COMPKEY
			if over
				clear	COMPFLD
				clear	COMPCOMP
			endif
	else 	// OFULLFIL = ""
		clear	COMPFLD
		clear	COMPCOMP
	endif
.End Patch 2.78 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.		
	DISPLAY	*P56:2,OWNLONM,*P56:3,OWNOCPY,*P56:4,OWNLOSA,*P56:5:
		OWNLOCTY," ",OWNLOS,OWNLOZC,*P58:6,COMPCOMP;
.END PATCH 2.77 REMOVED LOGIC
.END PATCH 2.73 REPLACED LOGIC
.
DISOWN2
.START PATCH 2.74 REPLACED LOGIC
.         DISPLAY   *P70:7,OUQTY,*P43:8,O1DES,*P43:9,O2DES,*P18:9,OCCODE;
         DISPLAY   *P70:7,str10,*P43:8,O1DES,*P43:9,NSEL2NAME,*P18:9,OCCODE;
.END PATCH 2.74 REPLACED LOGIC
         CMATCH    " " TO OCCODE
         GOTO      NOCONT IF EQUAL
         DISPLAY   *P18:9,"Y",*P23:9,OLRNCO;
.Start Patch #2.5 - ADD CENTURY
.         DISPLAY   *P8:10,OODTECOM,*P11:10,OODTECOM,*P14:10,OODTECOY:
.                   *P21:10,OQTYCO;
         DISPLAY   *P8:10,OODTECOM,*P11:10,OODTECOM,*P14:10,OODTECOC,OODTECOY:
                   *P23:10,OQTYCO;
.End Patch #2.5 - ADD CENTURY
         GOTO      BOTHDIS
NOCONT   DISPLAY   *P18:9,"N          ",*P4:10,"                       ";
BOTHDIS  CMATCH    " " TO OTOCODE                    TEST
         GOTO      DISNO IF EQUAL
         DISPLAY   *P11:12,"Y";
         GOTO      MATCH16
DISNO    DISPLAY   *P11:12,"N";
MATCH16  CMATCH    " " TO  OELCODE
         GOTO      DISNO2 IF EQUAL
         MOVE      OELCODE,NWORK1
         BRANCH    NWORK1 OF DISENT,DISEX,DISNTEX
DISENT   DISPLAY   *P21:12,"Y",*P29:12,"N";
         GOTO      BRIXO
DISEX    DISPLAY   *P21:12,"N",*P29:12,"Y";
         GOTO      BRIXO
DISNTEX  DISPLAY   *P21:12,"Y",*P29:12,"Y";
         GOTO      BRIXO
DISNO2   DISPLAY   *P21:12,"N",*P29:12,"N";
BRIXO    SUB       NWORK1,NWORK1
         MOVE      OSOTCODE,NWORK1
         BRANCH    NWORK1 OF DISXSEC,DISNTH,DISOTHR
         GOTO      DISQTYO
DISXSEC  DISPLAY   *P11:13,"Y",*P21:13," ",*P29:13," ";
         GOTO      DISQTYO
DISNTH   DISPLAY   *P21:13,"Y",*P11:13," ",*P29:13," ";
         GOTO      DISQTYO
DISOTHR  DISPLAY   *P29:13,"Y",*P11:13," ",*P21:13," ";
DISQTYO
.START PATCH 2.74 REPLACED LOGIC
.         MOVE      OPPM,F3
.         BUMP      OPPM BY 3
.         MOVE      OPPM,F2
.         RESET     OPPM
.         DISPLAY   *P15:14,OQTY,*P15:15,F3,".",F2,*P15:16,OMLRKY;
	unpack	NSEL2PRICE,str5,str3
	call	FormatNumeric using str5,str6
	DISPLAY	*P15:14,OQTY,*P15:15,str6,str3,*P15:16,OMLRKY;
.END PATCH 2.74 REPLACED LOGIC
.
.Start Patch #2.5 - ADD CENTURY
.         DISPLAY   *P9:18,ORTNDTEM,"/",ORTNDTED,"/",ORTNDTEY      (RETURN-DATE;
.         DISPLAY   *P21:18,OMDTEM,"/",OMDTED,"/",OMDTEY:     (MAIL-DATE);
.                   *P13:19,OSALES:
.                   *P13:20,OFOCODE
         DISPLAY   *P9:18,ORTNDTEM,"/",ORTNDTED,"/",ORTNDTEC,ORTNDTEY      (RETURN-DATE;
.
         DISPLAY   *P23:18,OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY:     (MAIL-DATE);
                   *P13:19,OSALES:
                   *P13:20,OFOCODE
.End Patch #2.5 - ADD CENTURY                   
                   
SELECT
         CMATCH    "C",OCOMSLCT
         IF        EQUAL
         DISPLAY   *P16:17,"COMSELECT";
         ENDIF
         CMATCH    "L",OCOMSLCT
         IF        EQUAL
         DISPLAY   *P16:17,"LIFESTYLE";
         ENDIF
         CMATCH    "I",OCOMSLCT
         IF        EQUAL
         DISPLAY   *P16:17,"IC SYSTEMS";
         ENDIF
.
DISMED
.Start Patch #2.5 - increase var
.         CMATCH    B1,OFOCODE
         MATCH    B2,OFOCODE
.End Patch #2.5 - increase var
         GOTO      DISMED2 IF NOT EQUAL
         GOTO      DIS27
DISMED2  MOVE      C0 TO NFIELD23
         TYPE      OFOCODE
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
.START PATCH 2.7 REPLACED LOGIC
.         LOAD      MEDIA FROM NFIELD23 OF MED1,MED2,MED3,MED4,MED5:
.                   MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
.                   MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
.                   MED23,MED24,MED25
         LOAD      MEDIA FROM NFIELD23 OF MED1,MED2,MED3,MED4,MED5:
                   MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
                   MED15,MED16,MED17,MED18,MED19,MED20,MED21,MED22:
                   MED23,MED24,MED25,MED26,MED27,MED28,MED29
.END PATCH 2.7 REPLACED LOGIC
         DISPLAY   *P15:20,*EL,MEDIA
.
DISF28   DISPLAY   *P13:22,OSCODE;
.TAPE RETURN
         DISPLAY   *P15:21,*EL,OTAPERET;
         CMATCH    " ",OSCODE
         GOTO      SAM0 IF EQUAL
         MOVE      OSCODE,NWORK1
         BRANCH    NWORK1 OF SAM1,SAM2,SAM3
SAM0     DISPLAY   *P15:22,*EL,"NOTHING";
         GOTO      DISF29
SAM1     DISPLAY   *P15:22,*EL," SAMPLE ENCLOSED";
         GOTO      DISF29
SAM2     DISPLAY   *P15:22,"SAMPLE TO FOLLOW";
         GOTO      DISF29
SAM3     DISPLAY   *P15:22,"SAMP.PREV.APPR'VD";
DISF29   DISPLAY   *P13:23,OSHP;
.Start Patch #2.5 - increased var
.         CMATCH    " ",OSHP
         MATCH     B2,OSHP
.End Patch #2.5 - increased var
         GOTO      SHIPBLNK IF EQUAL
         MOVE      OSHP TO NWORK1
         COMPARE   "0",NWORK1
         GOTO      SHIP0 IF EQUAL
         BRANCH    NWORK1 OF SHIP1,SHIP2,SHIP3,SHIP4,SHIP5,SHIP6:
                            SHIP7,SHIP8,SHIP9
SHIPBLNK DISPLAY   *P15:23,*EL,"NOTHING";
         GOTO      DISCONTC
SHIP0    DISPLAY   *P15:23,*EL,"SEE SPEC.INSTR.";
         GOTO      DISCONTC
SHIP1    DISPLAY   *P15:23,*EL,"FEDERAL/EXP - 2 DAY";
         GOTO      DISCONTC
SHIP2    DISPLAY   *P15:23,*EL,"UPS RED LABEL";
         GOTO      DISCONTC
SHIP3    DISPLAY   *P15:23,*EL,"FED EXP PRIORITY 1";
         GOTO      DISCONTC
SHIP4    DISPLAY   *P15:23,*EL,"UPS OR 1st CLASS";
         GOTO      DISCONTC
SHIP5    DISPLAY   *P15:23,*EL,"U.P.S. GROUND";
         GOTO      DISCONTC
SHIP6    DISPLAY   *P15:23,*EL,"UPS BLUE LABEL";
         GOTO      DISCONTC
SHIP7    DISPLAY   *P15:23,*EL,"AIR FREIGHT";
         GOTO      DISCONTC
SHIP8    DISPLAY   *P15:23,*EL,"MESSENGER";
         GOTO      DISCONTC
SHIP9    DISPLAY   *P15:23,*EL,"FEDERAL EXPRESS - STANDARD";
.
DISCONTC DISPLAY   *P13:24,OCOCODE;
.
. DISPLAY LR# AND SYSTEM DATE;
.
DISLRO   DISPLAY   *P35:21,*EL,"L.R.#: ",KEYMD;
.
.Start Patch #2.5 - ADD CENTURY
.         DISPLAY   *P49:21,"Order Date:  ",OODTEM,"/",OODTED,"/",OODTEY:
.                   *P71:21,"DJ#: ",ODOWJ;
         DISPLAY   *P49:21,"Order Date:  ",OODTEM,"/",OODTED,"/",OODTEC,OODTEY:
                   *P71:21,"DJ#: ",ODOWJ;
.End Patch #2.5 - ADD CENTURY
         rep       "R1L2Q3X4D506" in stat
         move      stat to num
         branch    num of roll,listd,quant,cancl,lol,regl
ROLL     DISPLAY   *P35:22,"ROLLOVER BEING RESENT TO TDMC",*W6;
         goto      distat
listd    DISPLAY   *P35:22,"LIST DESC BEING RESENT TO TDMC",*W6;
         goto      distat
quant     DISPLAY   *P35:22,"QTY CHANGE BEING RESENT TO TDMC",*W6;
         goto      distat
cancl    DISPLAY   *P35:22,"CANCELLED BEING RESENT TO TDMC",*W6;
         goto      distat
LOL      DISPLAY   *P35:22,"LOL BEING RESENT TO TDMC",*W6;
         goto      distat
REGL     DISPLAY   *P35:22,"ORDER BEING SENT TO TDMC",*W6;
DISTAT   CMATCH    "X" TO OSTAT
         GOTO      CANC IF EQUAL
         CMATCH    "Q" TO OSTAT
         GOTO      ISITO IF NOT EQUAL
CANC     DISPLAY   *P43:22,"CANCELLED",*W4;
         move       "X" to stat
.
. END OF DISPLAY, CANCEL OR MODIFY?;
.
ISITO    KEYIN     *P35:22,"Is this the record you wish to DELETE? ",ANS;
         CMATCH    "N" TO ANS
         GOTO      DISSORD IF EQUAL
         CMATCH    "Y" TO ANS
         GOTO      ISITO IF NOT EQUAL
         KEYIN     *P35:23,"Are you sure? ",ANS;
         CMATCH    "Y" TO ANS
         GOTO      ISITO IF NOT EQUAL
         FILEPI    1;TDMCORD
         READ      TDMCORD,KEYMD;KEYMD,STAT
         cmatch    "X" to stat
         if        not equal
         move      "D" to stat
         endif
         FILEPI    1;TDMCORD
         UPDATAB   tdmcord;*7,stat
.         READ      TDMCORD,KEYMD;;
.         DELETE    TDMCORD,KEYMD
         DISPLAY   *P1:24,*EL,*B,"RECORD UPDATED",*w2
         FILEPI    1;DELETE
         READ      DELETE,KEYMD;;
         GOTO      DISSORD IF NOT OVER
         MOVE      HOLDKEY TO KEYMD
         FILEPI    2;DELETE
         WRITE     DELETE,KEYMD;KEYMD,MCOMP,O1DES,DATE,TYPINIT
         GOTO      DISSORD
. .............................................................................
.
.
. IO - DISPLAY I/O ERROR MESSAGES.;
. ................................
IO       TRAPCLR   IO
         NORETURN
         BRANCH    FILE OF ONE,TWO
ZERO
         DISPLAY   *P1:24,*EL,*HON,*B,"UNIDENTIFIED I/O ERROR",*W2;
         GOTO      IOEXIT
ONE
         DISPLAY   *P1:24,*EL,*HON,*B,"TDMCORD I/O ERROR",*W2;
         GOTO      IOEXIT
TWO
         DISPLAY   *P1:24,*EL,*HON,*B,"TDMCDELETE I/O ERROR",*W2;
         GOTO      IOEXIT
IOEXIT   KEYIN     *P77:24,ANS;
         CMATCH    "Q" TO ANS
         STOP      IF EQUAL
         BRANCH    FILE TO ONE,TWO
         GOTO      ZERO
.
         INCLUDE   NORDIO.inc
         INCLUDE   NOWNIO.inc
.patch2.75
				include	compio.inc
				include	cntio.inc
.         INCLUDE   NMLRIO.inc
.patch2.75
         INCLUDE   NRTNIO.inc
.START PATCH 2.6 - ADDED LOGIC
         INCLUDE   NOFRIO.INC
.END PATCH 2.6 - ADDED LOGIC
.START PATCH 2.77 REMOVED LOGIC
..START PATCH 2.73 ADDED LOGIC
.	INCLUDE	NFULIO.INC
..END PATCH 2.73 ADDED LOGIC
.END PATCH 2.77 REMOVED LOGIC
.START PATCH 2.74 ADDED LOGIC
	INCLUDE	NSEL2IO.INC
.END PATCH 2.74 ADDED LOGIC
         INCLUDE   COMLOGIC.inc
