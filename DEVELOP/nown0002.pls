PC       EQU       0
         INC       COMMON.INC
         INCLUDE   cONS.INC
         INCLUDE  NOWNDD.INC
         inc       hp.inc
.START PATCH 2.3 ADDED LOGIC
	INCLUDE	NFULDD.INC
.END PATCH 2.3 ADDED LOGIC
.START PATCH 2.4 ADDED LOGIC
	include	NUSEDD.INC
	include winapi.inc
.END PATCH 2.4 ADDED LOGIC
.
release  init      "2.4"         ASH 28JUN2005 PCL2PDF RETIREMENT
.release  init      "2.3"         ASH 05FEB2002 NINFUL CONVERSION
.release  init      "2.2"         option to print labels
.RELEASE  INIT      "2.1"         convert to laser
.RELEASE  INIT      "2.0"        DLH 17MAR92   INCLUDES, GENERAL CLEAN UP,
.                               CONVERT TO PCBUS.
.
.START PATCH 2.4 REPLACED LOGIC
prfile	pfile
userlogn dim	7
font2   font
font3   font
.END PATCH 2.4 REPLACED LOGIC
SYSDATE  DIM       8
DATEMASK INIT      "99/99/9999"
TELEMASK INIT      "(999)999-9999"
PAGE     FORM      5
DATE     DIM       8
LINES    FORM      2
ONE      FORM      "1"
TEN      FORM      "10"
COUNT    FORM      5
TABLE    FORM      1
.
NUM1     DIM       4      1-4    OWNER NUMBER. **KEY**
CNTCT1   DIM       25     5-29   OWNER CONTACT NAME.
COMP1    DIM       25    30-54   OWNER COMPANY NAME.
ADDR1    DIM       25    55-79   OWNER ADDRESS
CITY1    DIM       15    80-94   OWNER CITY.
STATE1   DIM       2     95-96   OWNER STATE.
ZIP1     DIM       10    97-106  OWNER ZIP. LEFT JUST.
NAME1    DIM       10   107-116  PASSWORD NAME
REVDT1   DIM       8    117-122  REVISED DATE.
TELE1    DIM       13
.START PATCH 2.3 REPLACED LOGIC
.CCTO1    DIM       10
CCTO1    DIM       45
.END PATCH 2.3 REPLACED LOGIC
TAX1     DIM       15
stat1    dim       6
gally1   dim       9
.
NUM2     DIM       4      1-4    OWNER NUMBER. **KEY**
CNTCT2   DIM       25     5-29   OWNER CONTACT NAME.
COMP2    DIM       25    30-54   OWNER COMPANY NAME.
ADDR2    DIM       25    55-79   OWNER ADDRESS
CITY2    DIM       15    80-94   OWNER CITY.
STATE2   DIM       2     95-96   OWNER STATE.
ZIP2     DIM       10    97-106  OWNER ZIP. LEFT JUST.
NAME2    DIM       10   107-116  PASSWORD NAME
REVDT2   DIM       8    117-122  REVISED DATE.
TELE2    DIM       13
.START PATCH 2.3 REPLACED LOGIC
.CCTO2    DIM       10
CCTO2    DIM       45
.END PATCH 2.3 REPLACED LOGIC
TAX2     DIM       15
stat2    dim       6
gally2   dim       9
.
NUM3     DIM       4      1-4    OWNER NUMBER. **KEY**
CNTCT3   DIM       25     5-29   OWNER CONTACT NAME.
COMP3    DIM       25    30-54   OWNER COMPANY NAME.
ADDR3    DIM       25    55-79   OWNER ADDRESS
CITY3    DIM       15    80-94   OWNER CITY.
STATE3   DIM       2     95-96   OWNER STATE.
ZIP3     DIM       10    97-106  OWNER ZIP. LEFT JUST.
NAME3    DIM       10   107-116  PASSWORD NAME
REVDT3   DIM       8    117-122  REVISED DATE.
TELE3    DIM       13
.START PATCH 2.3 REPLACED LOGIC
.CCTO3    DIM       10
CCTO3    DIM       45
.END PATCH 2.3 REPLACED LOGIC
TAX3     DIM       15
stat3    dim       6
gally3   dim       9
.
NUM4     DIM       4      1-4    OWNER NUMBER. **KEY**
CNTCT4   DIM       25     5-29   OWNER CONTACT NAME.
COMP4    DIM       25    30-54   OWNER COMPANY NAME.
ADDR4    DIM       25    55-79   OWNER ADDRESS
CITY4    DIM       15    80-94   OWNER CITY.
STATE4   DIM       2     95-96   OWNER STATE.
ZIP4     DIM       10    97-106  OWNER ZIP. LEFT JUST.
NAME4    DIM       10   107-116  PASSWORD NAME
REVDT4   DIM       8    117-122  REVISED DATE.
TELE4    DIM       13
.START PATCH 2.3 REPLACED LOGIC
.CCTO4    DIM       10
CCTO4    DIM       45
.END PATCH 2.3 REPLACED LOGIC
TAX4     DIM       15
stat4    dim       6
gally4   dim       9
.
.
DATE1    DIM       10
DATE2    DIM       10
DATE3    DIM       10
DATE4    DIM       10
repflag  form      1              .1=normal master listing
.                                  2=labels
.
.START PATCH 2.4 ADDED LOGIC
.	if (INPNAME = "")
.Only if running by itself!
.		pack	taskname,"\\nts1\e\data\text\NINOWN.dat,",NTWKPATH1,"OWNSRT.dat;32-56"
.		sort	taskname
.	endif
	CALL	PAINT
.END PATCH 2.4 ADDED LOGIC
	MOVE	"OWNSRT" TO NOWNNAME
	MOVE	C3 TO NOWNPATH       *SET ACCESS TO FLAT FILE.
.
.         move      c2 to repflag

	CLOCK	DATE TO DATE
	IFNZ PC
		MOVE	DATEMASK TO SYSDATE
		EDIT	DATE TO SYSDATE
	XIF
	IFZ PC
		MOVE	DATE TO SYSDATE
	XIF
	MOVE	SYSDATE TO TODAY

.START PATCH 2.4 REPLACED LOGIC
.	match	b8 to program
.	if equal
.		MOVE	"NOWN0002" TO PROGRAM
.		move	"ninown" TO PRTNAME
	call	Trim using PROGRAM
	if (PROGRAM = "")
		MOVE	"NOWN0002",PROGRAM
.		move	"ninown" TO PRTNAME
		move	"LOCAL",PRTNAME
		move	"1",FUNC
.END PATCH 2.4 REPLACED LOGIC
	endif
	MOVE	"NINCAL" TO COMPNME
	MOVE	"MASTER OWNER BOOK PRINT" TO STITLE
	match	"LOCAL" TO PRTNAME
	IF NOT EQUAL
.START PATCH 2.4 REPLACED LOGIC
.		pack	prtfile from pdrive,prtname
.		display	*p10:12,"Print File : ",prtfile
.		SPLOPEN	PRTfile
.		branch	repflag of spl1,spl2
.spl1
.		print	hpdupl,hp17ptch,*f
.		goto splexit
.spl2
.		PRINT	hpreset,hpport:
.			033,"&l66P":               page length
.			033,"&l65F":               number lines
.			033,"&l1E",033,"&a0c0R":     top margin * print position
.			hp17ptch
.splexit
.	ENDIF
.	CALL	PAINT
		if (FUNC = "2")
			call	OpenPrtFile
		else
			pack	prtfile from pdrive,prtname
			SPLOPEN	PRTfile
			branch	repflag of spl1,spl2
spl1
			print	hpdups,hp17ptch,*f
			goto splexit
spl2
			PRINT	hpreset,hpport:
				033,"&l66P":               page length
				033,"&l65F":               number lines
				033,"&l1E",033,"&a0c0R":     top margin * print position
				hp17ptch
splexit
		endif
	else
PDFQuestion
		clear	str1
		keyin	*P10:13,*EL,"PDF Format? : ",*B,str1
		if (str1 = "Y" | str1 = "y")
			if (PORTN = 0)
				alert	note,"You must provide a Port Number!",result
				keyin	*P10:14,*EL,"Port Number : ",*B,PORTN
				if (PORTN = 0)
					goto PDFQuestion
				endif
			endif
			move	C0,NUSEFLD
			move	C1,NUSEPATH
			move	PORTN,NUSEFLD
			rep	zfill,NUSEFLD
			call	NUSEKEY
			scan	"INVALID",NUSEUSER
			if equal
				alert	note,"Invalid Port Number for PDF creation!",result
				clear	PORTN
				goto PDFQuestion
			endif
			reset	NUSEUSER
			scan	"BILLING",NUSEUSER
			if equal
				alert	note,"Invalid Port Number for PDF creation!",result
				clear	PORTN
				goto PDFQuestion
			endif
			reset	NUSEUSER
			call	Trim using NUSEUSER
			move	NUSEUSER,str1
			loop
				bump	NUSEUSER,1
				cmatch	B1,NUSEUSER
				until equal
				until eos
			repeat
			if not eos
				bump	NUSEUSER,1
				move	NUSEUSER,str6
				clear	userlogn
				pack	userlogn,str1,str6
			endif
			reset	NUSEUSER
.
			call	OpenPrtFile
			move	"2",FUNC
		else
			keyin	*P10:14,*EL,"Are you Printing Labels? : ",*B,str1
			if (str1 = "y" | str1 = "Y")
				move      c2,repflag
			else
				move      c1,repflag
			endif
			PACK	PRTFILE FROM "c:\work\NINOWN.LST"
			SPLOPEN	PRTFILE
			branch	repflag of spl1,spl2
		endif
	ENDIF
	if (FUNC = "2")
		create  font2,"Times New Roman",size=14
.		create  font3,"Times New Roman",size=14,italic
		create  font3,"Times New Roman",size=14
.Set up columns
		move    "500",column
		move    "2375",column1
		move    "4375",column2
		move    "6375",column3
		prtpage prfile;*UNITS=*HIENGLISH,*font=font2;
	endif
	display	*p10:12,"Print File : ",prtfile
.END PATCH 2.4 REPLACED LOGIC
.
	CALL	HEADER
.
LOOP
	CALL	NOWNSEQ
* ...................................................................
	GOTO	EOJ IF OVER
.START PATCH 2.3 ADDED LOGIC
	call	Trim using OWNCTN
	if (OWNCTN <> "")
		pack	NFULFLD,OWNCTN
		rep	zfill,NFULFLD
		move	C1,NFULPATH
		move	"checktwo-NFULKEY",Location
		pack	KeyLocation,NFULFLD
.		call	NFULKEY
	else
		clear	NFULFLD
		clear	NFULCOMP
	endif
.END PATCH 2.3 ADDED LOGIC
	ADD	C1 TO COUNT
	ADD	C1 TO TABLE
	DISPLAY	*P10:12,*EL,"RECORDS PROCESSED : ",COUNT
.
	branch	repflag of table1,table2
table1
	BRANCH	TABLE OF ONE,TWO,THREE,FOUR
	DISPLAY	*P1:23,*BLINKON,*HON,"OOOPS",*B
	GOTO LOOP
table2
	BRANCH	TABLE OF ONE,TWO,THREE
	DISPLAY	*P1:23,*BLINKON,*HON,"OOOPS",*B
	GOTO LOOP
.
ONE
	MOVE	OWNLON TO NUM1
	MOVE	OWNLONM TO CNTCT1
	MOVE	OWNOCPY TO COMP1
	MOVE	OWNLOSA TO ADDR1
	MOVE	OWNLOCTY TO CITY1
	MOVE	OWNLOS TO STATE1
	MOVE	OWNLOZC TO ZIP1
	MOVE	OWNPASS TO NAME1
	MOVE	OWNRDTE TO REVDT1
.START PATCH 2.3 REPLACED LOGIC
.         MOVE      OWNCTN TO CCTO1
	MOVE	NFULCOMP,CCTO1
.END PATCH 2.3 REPLACED LOGIC
	MOVE	TELEMASK TO TELE1
	EDIT	OWNTELE TO TELE1
	MOVE	OWNTAXID TO TAX1
	clear	stat1
	cmatch	"I" to ownstat
	if equal
		move	"Inactive" to stat1
	endif
	clear	gally1
	cmatch	"T" to owngally
	if equal
		move	"Batch Lcr" to gally1
	endif
.
	GOTO LOOP
.
TWO
	MOVE	OWNLON TO NUM2
	MOVE	OWNLONM TO CNTCT2
	MOVE	OWNOCPY TO COMP2
	MOVE	OWNLOSA TO ADDR2
	MOVE	OWNLOCTY TO CITY2
	MOVE	OWNLOS TO STATE2
	MOVE	OWNLOZC TO ZIP2
	MOVE	OWNPASS TO NAME2
	MOVE	OWNRDTE TO REVDT2
.START PATCH 2.3 REPLACED LOGIC
.         MOVE      OWNCTN TO CCTO2
	MOVE	NFULCOMP,CCTO2
.END PATCH 2.3 REPLACED LOGIC
	MOVE	TELEMASK TO TELE2
	EDIT	OWNTELE TO TELE2
	MOVE	OWNTAXID TO TAX2
	clear	stat2
	cmatch	"I" to ownstat
	if equal
		move	"Inactive" to stat2
	endif
	clear	gally2
	cmatch	"T" to owngally
	if equal
		move	"Batch Lcr" to gally2
	endif
.
	GOTO LOOP
.
THREE
	MOVE	OWNLON TO NUM3
	MOVE	OWNLONM TO CNTCT3
	MOVE	OWNOCPY TO COMP3
	MOVE	OWNLOSA TO ADDR3
	MOVE	OWNLOCTY TO CITY3
	MOVE	OWNLOS TO STATE3
	MOVE	OWNLOZC TO ZIP3
	MOVE	OWNPASS TO NAME3
	MOVE	OWNRDTE TO REVDT3
.START PATCH 2.3 REPLACED LOGIC
.         MOVE      OWNCTN TO CCTO3
	MOVE	NFULCOMP,CCTO3
.END PATCH 2.3 REPLACED LOGIC
	MOVE	TELEMASK TO TELE3
	EDIT	OWNTELE TO TELE3
	MOVE	OWNTAXID TO TAX3
	clear	stat3
	cmatch	"I" to ownstat
	if equal
		move	"Inactive" to stat3
	endif
	clear	gally3
	cmatch	"T" to owngally
	if equal
		move	"Batch Lcr" to gally3
	endif
.
	compare	c2 to repflag
	if equal
		CALL	PRINT
		MOVE	C0 TO TABLE
		goto loop
	else
		GOTO LOOP
	endif
.
FOUR
	MOVE	OWNLON TO NUM4
	MOVE	OWNLONM TO CNTCT4
	MOVE	OWNOCPY TO COMP4
	MOVE	OWNLOSA TO ADDR4
	MOVE	OWNLOCTY TO CITY4
	MOVE	OWNLOS TO STATE4
	MOVE	OWNLOZC TO ZIP4
	MOVE	OWNPASS TO NAME4
	MOVE	OWNRDTE TO REVDT4
.START PATCH 2.3 REPLACED LOGIC
.         MOVE      OWNCTN TO CCTO4
	MOVE	NFULCOMP,CCTO4
.END PATCH 2.3 REPLACED LOGIC
	MOVE	TELEMASK TO TELE4
	EDIT	OWNTELE TO TELE4
	MOVE	OWNTAXID TO TAX4
	clear	stat4
	cmatch	"I" to ownstat
	if equal
		move	"Inactive" to stat4
	endif
	clear	gally4
	cmatch	"T" to owngally
	if equal
		move	"Batch Lcr" to gally4
	endif
.
	CALL	PRINT
	MOVE	C0 TO TABLE
	GOTO LOOP
.
PRINT
.START PATCH 2.4 REPLACED LOGIC
.	branch	repflag of dt1,dt2
.dt1
.	COMPARE	"56" TO LINES
.	CALL	HEADER IF GREATER
.	CALL	HEADER IF EQUAL
.	PRINT	*1,"## ",NUM1,*33,"## ",NUM2:
.		*65,"## ",NUM3,*97,"## ",NUM4:
.		*N:
.		*1,CNTCT1,*33,CNTCT2,*65,CNTCT3,*97,CNTCT4:
.		*N:
.		*1,COMP1,*33,COMP2,*65,COMP3,*97,COMP4:
.		*FLUSH;
.	PRINT	*1,COMP1,*33,COMP2,*65,COMP3,*97,COMP4:
.		*N:
.		*1,ADDR1,*33,ADDR2,*65,ADDR3,*97,ADDR4:
.		*N:
.		*1,CITY1,"  ",STATE1," ",ZIP1:
.		*33,CITY2,"  ",STATE2," ",ZIP2:
.		*65,CITY3,"  ",STATE3," ",ZIP3:
.		*97,CITY4,"  ",STATE4," ",ZIP4
.	MOVE	DATEMASK TO DATE1
.	MOVE	DATEMASK TO DATE2
.	MOVE	DATEMASK TO DATE3
.	MOVE	DATEMASK TO DATE4
.	EDIT	REVDT1 TO DATE1
.	EDIT	REVDT2 TO DATE2
.	EDIT	REVDT3 TO DATE3
.	EDIT	REVDT4 TO DATE4
.	PRINT	*1,"Updated : ",DATE1," ",NAME1:
.		*33,"Updated : ",DATE2," ",NAME2:
.		*65,"Updated : ",DATE3," ",NAME3:
.		*97,"Updated : ",DATE4," ",NAME4:
.		*N:
.		*1,TELE1," ",CCTO1:
.		*33,TELE2," ",CCTO2:
.		*65,TELE3," ",CCTO3:
.		*97,TELE4," ",CCTO4:
.		*N:
.		*1,"TAX ID: ",TAX1:
.		*33,"TAX ID: ",TAX2:
.		*65,"TAX ID: ",TAX3:
.		*97,"TAX ID: ",TAX4:
.		*n:
.		*1,stat1,b1,gally1:
.		*33,stat2,b1,gally2:
.		*65,stat3,b1,gally3:
.		*97,stat4,b1,gally4:
.		*l
..
.	ADD	c11 TO LINES
.	goto clearprt
.
.dt2
.	COMPARE	"60" TO LINES
.	CALL	HEADER IF GREATER
.	CALL	HEADER IF EQUAL
.	PRINT	*1,CNTCT1,*47,CNTCT2,*97,CNTCT3:
.		*N:
.		*1,COMP1,*47,COMP2,*97,COMP3:
.		*FLUSH;
.	PRINT	*1,COMP1,*47,COMP2,*97,COMP3:
.		*N:
.		*1,ADDR1,*47,ADDR2,*97,ADDR3:
.		*N:
.		*1,CITY1,"  ",STATE1," ",ZIP1:
.		*47,CITY2,"  ",STATE2," ",ZIP2:
.		*97,CITY3,"  ",STATE3," ",ZIP3
.	PRINT	*1:
.		*N
..
.	ADD	c6 TO LINES
............................
	MOVE	DATEMASK TO DATE1
	MOVE	DATEMASK TO DATE2
	MOVE	DATEMASK TO DATE3
	MOVE	DATEMASK TO DATE4
	EDIT	REVDT1 TO DATE1
	EDIT	REVDT2 TO DATE2
	EDIT	REVDT3 TO DATE3
	EDIT	REVDT4 TO DATE4
	if (FUNC = "2")
		if (row > 9000)
                	prtpage prfile;*NEWPAGE;
                	CALL	HEADER
                endif
		prtpage prfile;*pcolumn:row,"## ",NUM1;
		prtpage prfile;*pcolumn1:row,"## ",NUM2;
		prtpage prfile;*pcolumn2:row,"## ",NUM3;
		prtpage prfile;*pcolumn3:row,"## ",NUM4;
		add	sixlpi,row
		prtpage prfile;*pcolumn:row,CNTCT1;
		prtpage prfile;*pcolumn1:row,CNTCT2;
		prtpage prfile;*pcolumn2:row,CNTCT3;
		prtpage prfile;*pcolumn3:row,CNTCT4;
		add	sixlpi,row
		prtpage prfile;*pcolumn:row,COMP1;
		prtpage prfile;*pcolumn1:row,COMP2;
		prtpage prfile;*pcolumn2:row,COMP3;
		prtpage prfile;*pcolumn3:row,COMP4;
		add	sixlpi,row
		prtpage prfile;*pcolumn:row,ADDR1;
		prtpage prfile;*pcolumn1:row,ADDR2;
		prtpage prfile;*pcolumn2:row,ADDR3;
		prtpage prfile;*pcolumn3:row,ADDR4;
		add	sixlpi,row
		prtpage prfile;*pcolumn:row,CITY1,"  ",STATE1," ",ZIP1;
		prtpage prfile;*pcolumn1:row,CITY2,"  ",STATE2," ",ZIP2;
		prtpage prfile;*pcolumn2:row,CITY3,"  ",STATE3," ",ZIP3;
		prtpage prfile;*pcolumn3:row,CITY4,"  ",STATE4," ",ZIP4;
		add	sixlpi,row
		prtpage prfile;*pcolumn:row,"Updated : ",DATE1," ",NAME1;
		prtpage prfile;*pcolumn1:row,"Updated : ",DATE2," ",NAME2;
		prtpage prfile;*pcolumn2:row,"Updated : ",DATE3," ",NAME3;
		prtpage prfile;*pcolumn3:row,"Updated : ",DATE4," ",NAME4;
		add	sixlpi,row
		prtpage prfile;*pcolumn:row,TELE1," ",CCTO1;
		prtpage prfile;*pcolumn1:row,TELE2," ",CCTO2;
		prtpage prfile;*pcolumn2:row,TELE3," ",CCTO3;
		prtpage prfile;*pcolumn3:row,TELE4," ",CCTO4;
		add	sixlpi,row
		prtpage prfile;*pcolumn:row,"TAX ID: ",TAX1;
		prtpage prfile;*pcolumn1:row,"TAX ID: ",TAX2;
		prtpage prfile;*pcolumn2:row,"TAX ID: ",TAX3;
		prtpage prfile;*pcolumn3:row,"TAX ID: ",TAX4;
		add	sixlpi,row
		prtpage prfile;*pcolumn:row,stat1,b1,gally1;
		prtpage prfile;*pcolumn1:row,stat2,b1,gally2;
		prtpage prfile;*pcolumn2:row,stat3,b1,gally3;
		prtpage prfile;*pcolumn3:row,stat4,b1,gally4;
		add	sixlpi,row
		add	sixlpi,row
	else
		branch	repflag of dt1,dt2
dt1
		COMPARE	"56" TO LINES
		CALL	HEADER IF GREATER
		CALL	HEADER IF EQUAL
		PRINT	*1,"## ",NUM1,*33,"## ",NUM2:
			*65,"## ",NUM3,*97,"## ",NUM4:
			*N:
			*1,CNTCT1,*33,CNTCT2,*65,CNTCT3,*97,CNTCT4:
			*N:
			*1,COMP1,*33,COMP2,*65,COMP3,*97,COMP4:
			*FLUSH;
		PRINT	*1,COMP1,*33,COMP2,*65,COMP3,*97,COMP4:
			*N:
			*1,ADDR1,*33,ADDR2,*65,ADDR3,*97,ADDR4:
			*N:
			*1,CITY1,"  ",STATE1," ",ZIP1:
			*33,CITY2,"  ",STATE2," ",ZIP2:
			*65,CITY3,"  ",STATE3," ",ZIP3:
			*97,CITY4,"  ",STATE4," ",ZIP4
		PRINT	*1,"Updated : ",DATE1," ",NAME1:
			*33,"Updated : ",DATE2," ",NAME2:
			*65,"Updated : ",DATE3," ",NAME3:
			*97,"Updated : ",DATE4," ",NAME4:
			*N:
			*1,TELE1," ",CCTO1:
			*33,TELE2," ",CCTO2:
			*65,TELE3," ",CCTO3:
			*97,TELE4," ",CCTO4:
			*N:
			*1,"TAX ID: ",TAX1:
			*33,"TAX ID: ",TAX2:
			*65,"TAX ID: ",TAX3:
			*97,"TAX ID: ",TAX4:
			*n:
			*1,stat1,b1,gally1:
			*33,stat2,b1,gally2:
			*65,stat3,b1,gally3:
			*97,stat4,b1,gally4:
			*l
.
		ADD	c11 TO LINES
		goto clearprt
.
dt2
		COMPARE	"60" TO LINES
		CALL	HEADER IF GREATER
		CALL	HEADER IF EQUAL
		PRINT	*1,CNTCT1,*47,CNTCT2,*97,CNTCT3
;		PRINT	*1,hpfixed,CNTCT1,*46,CNTCT2,*96,CNTCT3
		PRINT	*1,COMP1,*47,COMP2,*97,COMP3
		PRINT	*1,ADDR1,*47,ADDR2,*97,ADDR3:
			*N:
			*1,CITY1,"  ",STATE1," ",ZIP1:
			*47,CITY2,"  ",STATE2," ",ZIP2:
			*97,CITY3,"  ",STATE3," ",ZIP3
		PRINT	*1:
			*N
.
		ADD	c6 TO LINES
	endif
.END PATCH 2.4 REPLACED LOGIC
.
clearprt
	CLEAR	NUM1
	CLEAR	NUM2
	CLEAR	NUM3
	CLEAR	NUM4
.
	CLEAR	CNTCT1
	CLEAR	CNTCT2
	CLEAR	CNTCT3
	CLEAR	CNTCT4
.
	CLEAR	COMP1
	CLEAR	COMP2
	CLEAR	COMP3
	CLEAR	COMP4
.
	CLEAR	ADDR1
	CLEAR	ADDR2
	CLEAR	ADDR3
	CLEAR	ADDR4
.
	CLEAR	CITY1
	CLEAR	CITY2
	CLEAR	CITY3
	CLEAR	CITY4
.
	CLEAR	STATE1
	CLEAR	STATE2
	CLEAR	STATE3
	CLEAR	STATE4
.
	CLEAR	ZIP1
	CLEAR	ZIP2
	CLEAR	ZIP3
	CLEAR	ZIP4
.
	CLEAR	NAME1
	CLEAR	NAME2
	CLEAR	NAME3
	CLEAR	NAME4
.
	CLEAR	REVDT1
	CLEAR	REVDT2
	CLEAR	REVDT3
	CLEAR	REVDT4
.
	CLEAR	CCTO1
	CLEAR	CCTO2
	CLEAR	CCTO3
	CLEAR	CCTO4
.
	CLEAR	TAX1
	CLEAR	TAX2
	CLEAR	TAX3
	CLEAR	TAX4
.
	RETURN
.
HEADER
.START PATCH 2.4 REPLACED LOGIC
.	branch	repflag of hd1,hd2
.hd1
.	MOVE	C3 TO LINES
.	ADD	C1 TO PAGE
.	PRINT	*F,*1,"CONFIDENTIAL":
.		*26,"* * *   N I N   M A S T E R   ":
.		"O W N E R   F I L E   * * *":
.		*119,"DATE: ",SYSDATE:
.		*N,*119,"PAGE: ",PAGE,*L
.	RETURN
.hd2
.	move	c1 to lines
.	print	*f;
...............................
	if (FUNC = "2")
		ADD	ONE TO PAGE
		move	"375",row
		prtpage prfile;*pcolumn:row,"CONFIDENTIAL";
		prtpage prfile;*p3000:row,"* * *   N I N   M A S T E R   O W N E R   F I L E   * * *";
		prtpage prfile;*p7000:row,"DATE: ",SYSDATE;
		add	sixlpi,row
		prtpage prfile;*p7000:row,"PAGE: ",PAGE;
		move	"1000",row
	else
		branch	repflag of hd1,hd2
hd1
		MOVE	C3 TO LINES
		ADD	C1 TO PAGE
		PRINT	*F,*1,"CONFIDENTIAL":
			*26,"* * *   N I N   M A S T E R   ":
			"O W N E R   F I L E   * * *":
			*119,"DATE: ",SYSDATE:
			*N,*119,"PAGE: ",PAGE,*L
		RETURN
hd2
		move	c3 to lines
		print	*f,*l,*l,*l;
	endif
.END PATCH 2.4 REPLACED LOGIC
	return
.
.START PATCH 2.4 ADDED LOGIC
OpenPrtFile
	call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
		"Parameters":
		"ProcessPDF":
		"\\nts0\c\apps\plb\code\pdftest.bat":
		result
	if (result = C0)
.Prepare Flag file
		prep	tempfile,"c:\progra~1\pdf995\flag.dat"
		write	tempfile,SEQ;"flag set"
		close	tempfile
	endif
.
	PRTOPEN	prfile,"PDF995","c:\work\pdf\NINOWN"
	return
.END PATCH 2.4 ADDED LOGIC
EOJ
	MATCH	"          " TO COMP1
	GOTO EOJ1 IF EOS
	GOTO EOJ1 IF EQUAL
	CALL	PRINT
EOJ1
.START PATCH 2.4 REPLACED LOGIC
.	PRINT	hPPORT,hpdupoff,hpreset,*FLUSH
	if (FUNC = "2")
		prtclose prfile
.Give the file a chance of rendering itself before updating the INI file.
		pack	APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
		loop
			call	FindFirstFile
			until (APIResult = 0 | APIResult = hexeight)
			pause	"1"
		repeat
		pause	"2"
	else
		PRINT	hPPORT,hpdupoff,hpreset,*FLUSH
		splclose
		if (repflag = C2)
.Put up message for Labels
			pack	taskname,"Your label file can be found at:",newline,"c:\work\NINOWN.lst"
			alert	note,taskname,result
		else
			match	"LOCAL" TO PRTNAME
			IF EQUAL
.Print out since not run by Batch program
				clear	taskname
				call	GetWinVer
				Path	Exist,"c:\windows"
				if over			.nt/2000
					append	"!c:\winnt\system32\cmd.exe /c ",taskname
				elseif (osflag = c6)	.XP
					append	"!c:\windows\system32\cmd.exe /c ",taskname
				else			.95/98
					append	"!c:\command.com /c ",taskname
				endif
				append	"copy c:\work\NINOWN.lst \\nts0\laser2",taskname
				reset	taskname
				execute	taskname
				erase	"c:\work\NINOWN.lst"
			endif
.Clean up afterwards
			erase	"c:\work\NINOWN.srt"
		endif
	ENDIF
.END PATCH 2.4 REPLACED LOGIC
	shutdown  "cls"
.chained from dsinit use shutdown not stop dlh
	stop

	INCLUDE   NOWNIO.INC
.START PATCH 2.3 ADDED LOGIC
	INCLUDE	NFULIO.INC
.END PATCH 2.3 ADDED LOGIC
.START PATCH 2.4 ADDED LOGIC
	include	NUSEIO.INC
.END PATCH 2.4 ADDED LOGIC
	INCLUDE   COMLOGIC.INC