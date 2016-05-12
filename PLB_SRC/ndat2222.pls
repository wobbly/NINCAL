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
.				and massive cleanup
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
	unpack	ownnum,str2,str4
	move	str4,nownfld
         move      b1 to ndat3cde
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
.		
	if 	(COMPNUM = "009831")
	move	"F",ndat3cde
	
	Elseif 	(COMPNUM = "009406")
	move	b1,ndat3cde
	
	Elseif 	(COMPNUM = "009384")
	move	"J",ndat3cde
.
	Elseif 	(COMPNUM = "009383")
	move	"A",ndat3cde

	Elseif 	(COMPNUM = "009387" or COMPNUM = "009414" or COMPNUM = "009429")
	move	"P",ndat3cde
.	
	Elseif 	(COMPNUM = "009428")
	move	"M",ndat3cde

	Elseif 	(COMPNUM = "009410")
	move	"R",ndat3cde

	ElseIf 	(COMPNUM = "004907")
	move	"K",ndat3cde
	endif
	unpack	ownnum,str2,str4
	move	str4,ndat3own
         	DISPLAY   *P10:12,"BILLING CODE ",NDATTDMC
         	DISPLAY   *P10:14,"Dollar/Date Chrg ",NDATdolc
	display	*p10:16,"For ",COMPCOMP,b1,"code: ",ndat3cde
	display	*p10:18,"Exchange Management Fee?",ndat3exh

	if (ndat3exh = "Y")
	Display	*p52:18,"$",Ndat3ExRt,"/m"
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
	display	*p10:20,"Start Date: ",str10
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
	display	*p52:20,"Stop Date: ",str10

KEYTDMC  DISPLAY   *P1:22,"' '=NOBILLING,'B'=WE BILL, 'R'entals/Splits":
                    " 'E'xchange Only"
         match     "I" to MODe
         goto       BCODE IF NOT EQUAL
         keyin      *p10:24,*el,"LIKE TO SEE ANOTHER ",STR1
         cmatch     YES TO STR1
         goto       eoj if not equal
         goto       keylst
BCODE    KEYIN     *P10:12,"BILLING CODE ",*RV,NDATTDMC
cCODE    keyin     *P10:14,"Dollar/Date Chrg ",*rv,NDATdolc
xCode    keyin     *p10:18,"Exchange Management Fee?",*rv,ndat3exh
         cmatch    no to ndat3exh
         if        equal
         keyin     *p10:20,"Date Stopped: mm/dd/ccyy ",*zf,*jr,mm:
                   "/",*zf,*jr,dd,"/",*zf,*jr,cc,*zf,*jr,yy
         pack      ndat3ex2 from mm,dd,cc,yy
         type      ndat3ex2
         if        not equal
         display   *p10:20,*el,"DATE ",ndat3ex2," Is not valid!!!!!",*b,*b,*w2
         goto      xcode
         endif
         goto      ok
         endif
         cmatch    yes to ndat3exh
         if        equal
         keyin    *p10:20,"Date Started: mm/dd/ccyy ",*zf,*jr,mm:
                   "/",*zf,*jr,dd,"/",*zf,*jr,cc,*zf,*jr,yy
         pack      ndat3ex1 from mm,dd,cc,yy
         clear     ndat3ex2
Rate	Keyin	*p52:20,"$",*RV,Ndat3ExRt,"/m"
	Display	*p52:20,"$",Ndat3ExRt,"/m"
	if	(ndat3exh = "Y" & Ndat3ExRt <= c0)
	Display	*p10:24,*el,"Price is Required!!!!!!!!!",*b,*w2
	goto	Rate
	endif
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
	unpack	ownnum,str2,str4
	move	str4,nownfld

         move      c1 to nownpath
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

	if 	(COMPNUM = "009831")
	move	"F",ndat3cde
	
	Elseif 	(COMPNUM = "009406")
	move	b1,ndat3cde
	
	Elseif 	(COMPNUM = "009384")
	move	"J",ndat3cde
.
	Elseif 	(COMPNUM = "009383")
	move	"A",ndat3cde

	Elseif 	(COMPNUM = "009387" or COMPNUM = "009414" or COMPNUM = "009429")
	move	"P",ndat3cde
.	
	Elseif 	(COMPNUM = "009428")
	move	"M",ndat3cde

	Elseif 	(COMPNUM = "009410")
	move	"R",ndat3cde

	ElseIf 	(COMPNUM = "004907")
	move	"K",ndat3cde
	endif

	unpack	ownnum,str2,str4
	move	str4,ndat3own
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
	include compio.inc
	include cntio.inc
         INCLUDE   COMLOGIC.INC

