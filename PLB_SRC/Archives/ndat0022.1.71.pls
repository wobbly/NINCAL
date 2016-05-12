.NDAT0022 - DATACARD TRIPLEX BILLING MAINT/INQUIRY
.........
PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NDAT3DD.INC
         INCLUDE   NPASDD.INC
         INCLUDE   NDATDD.INC
         include   nowndd.inc
         include   consacct.inc
.START PATCH 1.5 ADDED LOGIC
.START PATCH 1.67 REPLACED LOGIC
.	include	nfuldd.inc
	include compdd.inc
	include cntdd.inc
.END PATCH 1.67 REPLACED LOGIC
.END PATCH 1.5 ADDED LOGIC

RELEASE  INIT	   "1.72"	  DLH      04Jan08  Rates Field for PLI
.RELEASE  INIT	   "1.71"	  DLH      13Feb07  Add MKTG
.RELEASE  INIT	   "1.70"	      JD   11OCT2006 FULFILLMENT CONVERSION/Datacard
.RELEASE  INIT	   "1.69"	     DMS   21JUN2006 FULFILLMENT CONVERSION
.RELEASE  INIT      "1.68"            DLH    31Aug2006 Add Frontline
.RELEASE  INIT      "1.67"            DLH    11Aug2006 Work Order 1102- Add Pidi MMI
.RELEASE  INIT      "1.66"            ASH   02MAR2005 Work Order 615 - Display Date fields
.RELEASE  INIT      "1.65"            JD   02Jul2004 new xmgtrate.
.RELEASE  INIT      "1.6"            ASH	28JAN2004 DATACARD CONVERSION
.RELEASE  INIT      "1.5"            ASH	04FEB2002 NINFUL CONVERSION
.RELEASE  INIT      "1.4"            dlh 30SEP99 Add code to check OWNCTN at update.
.RELEASE  INIT      "1.3"            dlh 28MAY96 CHECK FOR OVER ON LISTHELP
.release  init      "1.2"            DLH add dollar/date charge codes
.RELEASE  INIT      "R1.1"           JD  05AUG93 added password includes,keyin.
.RELEASE  INIT      "R1.0"          DLH 09MAR92.
WSW      DIM       1
KEYCOUNT FORM      1
AKEY1    INIT      "   "
AKEY2    INIT      " 2L"
QUES     INIT      "??????????"
mode     dim       1
.
         MOVE      "NDAT0022" TO PROGRAM
         MOVE      C1 TO NDATPATH
         MOVE      "Fulfillment BILLING STATUS" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         CALL      PAINT
         KEYIN     *P12:10,"Enter Password to Add or Modify, Hit Enter":
                   " for Inquiry",*T120,*UC,*EOFF,PASSWORD,*EON;
         CMATCH    B1 TO PASSWORD
         GOTO      NOGOOD IF EOS
         RESET     PASSWORD TO 5
         RESET     PASSWORD
         CLEAR     NPASFLD
         APPEND    "I",NPASFLD
         APPEND    PASSWORD,NPASFLD
         RESET     NPASFLD
         CALL      NPASKEY
         GOTO      GOOD IF NOT OVER
NOGOOD   DISPLAY   *B,*P17:11,"Password NOT VALID... Only Inquiry Allowed",*w2;
         move      "I" TO MODE
         goto       keylst
GOOD     DISPLAY   *P17:12,"Password ACCEPTED... Add and Modify Allowed.",*w2;
.
KEYLST   KEYIN     *p1:08,*ES,*P10:08,"LIST NUMBER: ",*ZF,*JR,NDATFLD
         SCAN      "*" IN NDATFLD
         STOP      IF EQUAL
         IF        EOS
         BEEP
         DISPLAY   *P1:24,*EL,"NUMBER REQUIRED!!!",*W3,*P1:24,*EL;
         GOTO      KEYLST
         ENDIF
         SCAN      "?" IN NDATFLD
         IF        EQUAL
         clear     lstnum                 .dlh 28may96
         clear     mlstname               .dlh 28may96
         CALL      LISTHELP
         MOVE      LSTNUM TO NDATFLD
         DISPLAY   *P23:12,NDATFLD
         ENDIF
         RESET     NDATFLD
         CALL      NDATKEY
         MOVE      NDATFLD TO NDAT3FLD
.         KEYIN     *P10:14,*EL,*DV,MLSTNAME," OK? ",STR1
         DISPLAY   *P10:10,*EL,MLSTNAME
         KEYIN     *P74:10,*EL," OK? ",STR1
         CMATCH    YES TO STR1
         GOTO      KEYLST IF NOT EQUAL
         MOVE      C2 TO N1
         REP       ZFILL IN NDAT3FLD
         CALL      NDAT3KEY
         IF        OVER
         DISPLAY   *P1:24,*EL,"RECORD NOT FOUND, LET'S ADD IT";
         MOVE      C1 TO N1
         endif
.START PATCH 1.6 REPLACED LOGIC
.         move      ownnum to nownfld
	unpack	ownnum,str2,str4
	move	str4,nownfld
.END PATCH 1.6 REPLACED LOGIC
.begin patch "1.70
.         move      c1 to nownpath
.         call      nownkey
         move      b1 to ndat3cde
.end patch "1.70"
.START PATCH 1.5 REPLACED LOGIC
.         scan      "FIDE" in ownctn
.         if         equal
.         move       "F" to ndat3cde
.         endif
.         scan      "TDMC" in ownctn
.         if         equal
.         move       b1 to ndat3cde
.         endif
.         scan      "ANTARES" in ownctn
.         if         equal
.         move       "J" to ndat3cde
.         endif
.         scan      "ANACAPA" in ownctn
.         if         equal
.         move       "A" to ndat3cde
.         endif
.....................
.begin patch "1.70"
.	call	Trim using OWNCTN
.end patch "1.70"
.START PATCH 1.67 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"KEYLST-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.	endif
.
.	if (NFULFLD = "0001")
.		move	"F",ndat3cde
.	else
.		scan	"FIDE",NFULCOMP
.		if equal
.			move	"F",ndat3cde
.		endif
.	endif
..
.	if (NFULFLD = "0026")
.		move	b1,ndat3cde
.	else
.		scan	" TDMC",NFULCOMP
.		if equal
.			move	b1,ndat3cde
.		endif
.	endif
..
.	if (NFULFLD = "0004")
.		move	"J",ndat3cde
.	else
.		scan	"ANTARES",NFULCOMP
.		if equal
.			move	"J",ndat3cde
.		endif
.	endif
..
.	if (NFULFLD = "0003")
.		move	"A",ndat3cde
.	else
.		scan	"ANACAPA",NFULCOMP
.		if equal
.			move	"A",ndat3cde
.		endif
.	endif
.	reset	NFULCOMP
.
;
.	if (OWNCTN <> "")
.		pack	COMPFLD6,OWNCTN
.		rep	zfill,COMPFLD6
.		move	C1,COMPPATH
.		move	"KEYLST-COMPKEY6",Location
.		pack	KeyLocation,COMPFLD6
.		call	COMPKEY6
.		if over
.			clear COMPCOMP
.			clear COMPFLD6
.		else
.			if (COMPSVBFLG <> "T")
.				clear COMPCOMP
.				clear COMPFLD6
.			endif
.		endif
.	else	// OWNCTN = ""
.		clear COMPCOMP
.		clear COMPFLD6
.	endif

.begin patch "1.70"
	call	debug	
		if (DATFUL <> "")
			packKEy	COMPFLD,DATFUL
			rep	zfill,COMPFLD
			move           C1,COMPPATH
			move	"Verify-COMPKEY",Location
			pack	KeyLocation,COMPFLD
			call	COMPKEY
			if over
			clear   compcomp
			else
				if (COMPSVBFLG <> "T")
			clear   compcomp
				endif
			endif
		else	// datful = ""
		clear   compcomp
		endif
;		
	if (COMPNUM = "009831")
		move	"F",ndat3cde
.	else
.		scan	"FIDE",COMPCOMP
.		if equal
.			move	"F",ndat3cde
.		endif
	endif
.
	if (COMPNUM = "009406")
		move	b1,ndat3cde
.	else
.		scan	"TDMC",COMPCOMP
.		if equal
.			move	b1,ndat3cde
.		endif
	endif
.
	if (COMPNUM = "009384")
		move	"J",ndat3cde
.	else
.		scan	"ANTARES",COMPCOMP
.		if equal
.			move	"J",ndat3cde
.		endif
	endif
.
	if (COMPNUM = "009383")
		move	"A",ndat3cde
.	else
.		scan	"ANACAPA",COMPCOMP
.		if equal
.			move	"A",ndat3cde
.		endif
	endif
.begin patch "1.67"
	if (COMPNUM = "009387" or COMPNUM = "009414" or COMPNUM = "009429")
		move	"P",ndat3cde
.	else
.		scan	"PIDI",COMPCOMP
.		if equal 
.			move	"P",ndat3cde
.		endif
	endif
.	
	if (COMPNUM = "009428")
		move	"M",ndat3cde
.	else
.		scan	"MMI",COMPCOMP
.		if equal 
.			move	"M",ndat3cde
.		endif
	endif
.end patch "1.67"
.begin patch "1.68"	
	if (COMPNUM = "009410")
		move	"R",ndat3cde
.	else
.		scan	"FRONTLINE",COMPCOMP
.		if equal 
.			move	"R",ndat3cde
.		endif
	endif
.begin patch "1.71"	
	if (COMPNUM = "004907")
		move	"K",ndat3cde
	endif
.end patch "1.71"
	
.end patch "1.70"
.END PATCH 1.67 REPLACED LOGIC

.END PATCH 1.5 REPLACED LOGIC
.START PATCH 1.6 REPLACED LOGIC
.         move      ownnum to ndat3own
	unpack	ownnum,str2,str4
	move	str4,ndat3own
.END PATCH 1.6 REPLACED LOGIC
.         ENDIF
         DISPLAY   *P10:12,"BILLING CODE ",NDATTDMC
         DISPLAY   *P10:14,"Dollar/Date Chrg ",NDATdolc
.START PATCH 1.66 REPLACED LOGIC
..START PATCH 1.5 REPLACED LOGIC
..         display   *p10:20,"For ",ownctn,b1,"code: ",ndat3cde
.         display   *p10:20,"For ",NFULCOMP,b1,"code: ",ndat3cde
..END PATCH 1.5 REPLACED LOGIC
.         display   *p10:21,"Exhange Management Fee?",ndat3exh
.         if        (ndat3exh = "Y")
.         reset     rateten
.         scan      ndatfld in rateten
.         if        equal
.         display   *p52:21," @10/m"
.         else
..begin patch 1.65
.         display   *p52:21," @4/m"
..         display   *p52:21," @2/m"
..end patch 1.65
.         endif
.         endif
..................................
.START PATCH 1.67 REPLACED LOGIC
.	display	*p10:19,"For ",NFULCOMP,b1,"code: ",ndat3cde
	display	*p10:16,"For ",COMPCOMP,b1,"code: ",ndat3cde
.END PATCH 1.67 REPLACED LOGIC
	display	*p10:18,"Exhange Management Fee?",ndat3exh
	if (ndat3exh = "Y")
	Display	*p52:18,"$",Ndat3ExRt,"/m"
.		reset	rateten
.		scan	ndatfld in rateten
.		if equal
.			display	*p52:20," @10/m"
.		else
.			display	*p52:20," @4/m"
.		endif
	endif
.
	clear	str10
	call	Trim using NDAT3EX1
	if (NDAT3EX1 <> "")
		count	result,NDAT3EX1
		if (result = 8)
			unpack	NDAT3EX1,MM,DD,str4
			pack	str10,MM,SLASH,DD,SLASH,str4
		endif
	endif
	display	*p10:21,"Start Date: ",str10
.
	clear	str10
	call	Trim using NDAT3EX2
	if (NDAT3EX2 <> "")
		count	result,NDAT3EX2
		if (result = 8)
			unpack	NDAT3EX2,MM,DD,str4
			pack	str10,MM,SLASH,DD,SLASH,str4
		endif
	endif
	display	*p52:21,"Stop Date: ",str10
.END PATCH 1.66 REPLACED LOGIC

KEYTDMC  DISPLAY   *P1:22,"' '=NOBILLING,'B'=WE BILL, 'R'entals/Splits":
                    " 'E'xchange Only"
         match     "I" to MODe
         goto       BCODE IF NOT EQUAL
         keyin      *p10:24,*el,"LIKE TO SEE ANOTHER ",STR1
         cmatch     YES TO STR1
         goto       eoj if not equal
         goto       keylst
BCODE    KEYIN     *P10:16,"BILLING CODE ",*RV,NDATTDMC
cCODE    keyin     *P10:18,"Dollar/Date Chrg ",*rv,NDATdolc
xCode    keyin     *p10:21,"Exhange Management Fee?",*rv,ndat3exh
         cmatch    no to ndat3exh
         if        equal
         keyin     *p10:22,"Date Stopped: mm/dd/ccyy ",*zf,*jr,mm:
                   "/",*zf,*jr,dd,"/",*zf,*jr,cc,*zf,*jr,yy
         pack      ndat3ex2 from mm,dd,cc,yy
         type      ndat3ex2
         if        not equal
         display   *p10:22,*el,"DATE ",ndat3ex2," Is not valid!!!!!",*b,*b,*w2
         goto      xcode
         endif
         goto      ok
         endif
         cmatch    yes to ndat3exh
         if        equal
         keyin    *p10:22,"Date Started: mm/dd/ccyy ",*zf,*jr,mm:
                   "/",*zf,*jr,dd,"/",*zf,*jr,cc,*zf,*jr,yy
         pack      ndat3ex1 from mm,dd,cc,yy
         clear     ndat3ex2
	Display	*p52:20,"$",Ndat3ExRt,"/m"
.         reset     rateten
.         scan      ndatfld in rateten
.         if        equal
.         display   *p52:21," @10/m"
.         else
..begin patch 1.65
.         display   *p52:21," @4/m"
..         display   *p52:21," @2/m"
..end patch 1.65
.         endif
         goto      ok
         endif
         goto      Xcode
OK       KEYIN     *P10:24,*EL,"EVERYTHING OK ? ",STR1
         CMATCH    NO TO STR1
         IF        EQUAL
         CALL      PAINT
         GOTO      KEYLST
         ENDIF
         BRANCH    N1 OF ADD,UPD
         STOP
ADD      MOVE      LSTNUM TO NDAT3KEY
         CALL      NDAT3WRT
         CALL      PAINT
         GOTO      KEYLST
UPD
.START PATCH 1.6 REPLACED LOGIC
.         move      ownnum to nownfld
	unpack	ownnum,str2,str4
	move	str4,nownfld
.END PATCH 1.6 REPLACED LOGIC

         move      c1 to nownpath
.         call      nownkey
.         move      b1 to ndat3cde
.START PATCH 1.5 REPLACED LOGIC
.         scan      "FIDE" in ownctn
.         if         equal
.         move       "F" to ndat3cde
.         endif
.         scan      "TDMC" in ownctn
.         if         equal
.         move       b1 to ndat3cde
.         endif
.         scan      "ANTARES" in ownctn
.         if         equal
.         move       "J" to ndat3cde
.         endif
.         scan      "ANACAPA" in ownctn
.         if         equal
.         move       "A" to ndat3cde
.         endif
.....................
.	call	Trim using OWNCTN
.START PATCH 1.67 REPLACED LOGIC
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"KEYLST-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.	endif
..
.	if (NFULFLD = "0001")
.		move	"F",ndat3cde
.	else
.		scan	"FIDE",NFULCOMP
.		if equal
.			move	"F",ndat3cde
.		endif
.	endif
..
.	if (NFULFLD = "0026")
.		move	b1,ndat3cde
.	else
.		scan	"TDMC",NFULCOMP
.		if equal
.			move	b1,ndat3cde
.		endif
.	endif
..
.	if (NFULFLD = "0004")
.		move	"J",ndat3cde
.	else
.		scan	"ANTARES",NFULCOMP
.		if equal
.			move	"J",ndat3cde
.		endif
.	endif
..
.	if (NFULFLD = "0003")
.		move	"A",ndat3cde
.	else
.		scan	"ANACAPA",NFULCOMP
.		if equal
.			move	"A",ndat3cde
.		endif
.	endif
.	reset	NFULCOMP

.CUT BELOW
.	if (OWNCTN <> "")
.		pack	NFULFLD,OWNCTN
.		rep	zfill,NFULFLD
.		move	C1,NFULPATH
.		move	"KEYLST-NFULKEY",Location
.		pack	KeyLocation,NFULFLD
.		call	NFULKEY
.	else
.		clear	NFULFLD
.		clear	NFULCOMP
.	endif
.CUT ABOVE
.	if (OWNCTN <> "")
.		pack 	COMPFLD6, OWNCTN
.		rep	zfill, COMPFLD6
.		move	C1,COMPPATH
.		move	"KEYLST2-COMPKEY6",Location
.		pack	KeyLocation,COMPFLD6
.		call	COMPKEY6
.		if over
.			clear COMPFLD6
.			clear COMPCOMP
.		else
.			if (COMPSVBFLG <> "T")
.				clear COMPFLD6
.				clear COMPCOMP
.			endif
.		endif
.	else  // OWNCTN = ""
.		clear COMPFLD6
.		clear COMPCOMP
.	endif
.begin patch "1.70"
	call	debug
		if (DATFUL <> "")
			packKey	COMPFLD,DATFUL
			rep	zfill,COMPFLD
			move           C1,COMPPATH
			move	"Verify-COMPKEY",Location
			pack	KeyLocation,COMPFLD
			call	COMPKEY
			if over
			else
				if (COMPSVBFLG <> "T")
				endif
			endif
		else	// datful = ""
		endif
;		
	if (COMPNUM = "009831")
		move	"F",ndat3cde
.	else
.		scan	"FIDE",COMPCOMP
.		if equal
.			move	"F",ndat3cde
.		endif
	endif
.
	if (COMPNUM = "009406")
		move	b1,ndat3cde
.	else
.		scan	"TDMC",COMPCOMP
.		if equal
.			move	b1,ndat3cde
.		endif
	endif
.
	if (COMPNUM = "009384")
		move	"J",ndat3cde
.	else
.		scan	"ANTARES",COMPCOMP
.		if equal
.			move	"J",ndat3cde
.		endif
	endif
.
	if (COMPNUM = "009383")
		move	"A",ndat3cde
.	else
.		scan	"ANACAPA",COMPCOMP
.		if equal
.			move	"A",ndat3cde
.		endif
	endif
.begin patch "1.67"
	if (COMPNUM = "009387" or COMPNUM = "009414" or COMPNUM = "009429")
		move	"P",ndat3cde
.	else
.		scan	"PIDI",COMPCOMP
.		if equal 
.			move	"P",ndat3cde
.		endif
	endif
.	
	if (COMPNUM = "009428")
		move	"M",ndat3cde
.	else
.		scan	"MMI",COMPCOMP
.		if equal 
.			move	"M",ndat3cde
.		endif
	endif
.end patch "1.67"
.begin patch "1.68"	
	if (COMPNUM = "009410")
		move	"R",ndat3cde
.	else
.		scan	"FRONTLINE",COMPCOMP
.		if equal 
.			move	"R",ndat3cde
.		endif
	endif
.end patch "1.68"
.begin patch "1.71"	
	if (COMPNUM = "004907")
		move	"K",ndat3cde
	endif
.end patch "1.71"
	
.end patch "1.70"

.end patch "1.68"

.	reset	COMPCOMP
.END PATCH 1.67 REPLACED LOGIC
.END PATCH 1.5 REPLACED LOGIC
.START PATCH 1.6 REPLACED LOGIC
.         move      ownnum to ndat3own
	unpack	ownnum,str2,str4
	move	str4,ndat3own
.END PATCH 1.6 REPLACED LOGIC
	 CALL      NDAT3UPD
         CALL      PAINT
         GOTO      KEYLST
EOJ
         STOP
         INCLUDE   NDAT3IO.INC
         INCLUDE   NDATIO.INC
         include   nownio.inc
         INCLUDE   LISTHELP.INC
         INCLUDE   NPASIO.INC
.START PATCH 1.5 ADDED LOGIC
.START PATCH 1.67 REPLACED LOGIC
.	include	nfulio.inc
	include compio.inc
	include cntio.inc
.END PATCH 1.67 REPLACED LOGIC
.END PATCH 1.5 ADDED LOGIC
         INCLUDE   COMLOGIC.INC

