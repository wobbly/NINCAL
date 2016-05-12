PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         include   nowndd.inc
         include   norddd.inc
.START PATCH 1.5 ADDED LOGIC
	include	nfuldd.inc
.END PATCH 1.5 ADDED LOGIC
.
RELEASE  INIT       "1.5"        ASH 05FEB2002  NINFUL CONVERSION
LIVETC   IFILE     KEYLEN=6,VAR=7
TDMCORD  IFILE     KEYLEN=6,VAR=7
INFILE   FILE
tdmcout2 ifile     KEYLEN=6,var=21
TDMCOUT  IFILE     KEYLEN=6,VAR=7
prfile   pfile
KEY      DIM       6
ONE      FORM      "1"
READCNT  FORM      10
MISSCNT  FORM      10
LR       DIM       6
check    form      5
check2   form      5 
check3   form      5
orddate  form      5
ODATE    DIM       6             .Order Date to be Printed
grpflg   form      1
clsflg   form      1
Rowcount form      3             .KEEP TRACK OF ROW PER PAGE
;osflag   form      1             . 1=win 95,98, 2=NT
Fulfil   DIM       6
Pfulfil  DIM       6
Liveflg  dim       2
YR       FORM      2
.
         MOVE      "NORD0043" to PROGRAM
         MOVE      "Names in the News Ca Inc" TO COMPNME
         open      LIVETC,"tdmcord.isi|20.20.30.103:502"
         OPEN      TDMCOUT,"tdmcNOTIFY"
         prepare   tdmcout2,"\\nts1\e\data\FAILED2.dat":
                            "\\nts1\e\data\FAILED2","6","21"
         OPEN      INFILE,"\\nts1\e\data\FAILED2"
         open      tdmcout2,"\\nts1\e\data\FAILED2"
.++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         MOVE      C1 TO NORDPATH        .SET ACCESS TO ISI.
.====================================================================================
..............................................................
.Menu
.Set Up Menu Bar
.mFile    menu
.mEdit    menu
.mOptions menu
.mHelp    menu
.Present Data for Menu Bar
.FData   init    "&File;&Print;Pre&view;-;E&xit"
.FData   init    "&File;E&xit"
.EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
.OData   init    "&Options;&Search;-;&Preferences"
.HData   init    "&Help;&About"
.====================================================================================
        move    "NORD0043.PLS",Wprognme
        move    "Check for Triplex Notification Records",Wfunction
        move    "Jose Duenas",Wauthor
        move    "1.0",Wrelease
        move    "March 2003",Wreldate
        call     paint
.abt     plform  About
.x       plform  Nord0043


.     winhide

.  formload x
.  formload abt`
.  create  NORD0043;mFile,FData
.  create  NORD0043;mEdit,EData,mFile
.  create  NORD0043;mOptions,OData,mEdit
.  create  NORD0043;mHelp,HData,mOptions
.Activate Menus
.FileGo leads to stop
.  activate mFile,FileGo,result
.Need this when it works
.  activate mEdit,EditGo,result
.  activate mOptions,OptionsGo,result
.Only a SubMenu under this one
.  activate mHelp,HelpGo,result

.set date for previous month
.	setitem Nord0043EditDate,0,""
.        setitem   Nord0043statmiss,0,"0"

.        loop
.        waitevent
.        repeat
.====================================================================================
Start
.        call OrderSetMouseBusy
        clear readcnt
        clear misscnt
.==================================================
.        getprop TRIORDRadioALL,SELGROUPID=grpflg         5- Brokerage 6-list mgnt 7-ALL
.==================================================
.	getitem Nord0043EditDate,0,str10
.        type 	str10
. 	if not equal
.Reformatting date
.        	call    Trim using str10
.	        call    RemoveChar using str10,SLASH
.        	call    RemoveChar using str10,dash
.            	if (str10 = "")
.	 	        alert    caution,"Need Valid Month,Day, and Year!!!!",result
.	                clear    str10
.			getitem  Nord0043EditDate,0,str10
.                        setfocus Nord0043EditDate
.                	return
.           	endif
.        endif

.        count   n3,str10
.        if ((n3 < c6)|(n3 > c8)|(n3 = c7))
. 	        alert   caution,"Need Valid Month,Day, and Year!!!!",result
.                return
.        endif
.        if (n3 = c8)
.	        unpack  str10,mm,dd,str2,yy
.	        pack    str10,mm,"/",dd,"/",str2,yy
.        endif
.        if (n3 = c6)
.	        unpack  str10,mm,dd,yy
.	        pack    str10,mm,"/",dd,"/",yy
.        endif
.        setitem Nord0043EditDate,0,str10
.=====================================================================================
	clock    timestamp,str8
        unpack   str8,str2,yy,mm,dd
        move     DD to n2
        sub 	   c1 from n2
        MOVE     N2 TO DD
        CALL     CVTJUL
        MOVE     JULDAYS TO CHECK3

         move      "03" to mm
         move      "21" to dd
         move      "03" to yy
         call      cvtjul
         move      c1 to clsflg
         move      juldays to check2
         MOVE      "460000" TO NORDFLD
;         MOVE      "480000" TO NORDFLD
         CALL      NORDKEY
read
         call      nordks
         GOTO      EOJ IF OVER
.
         ADD       ONE TO READCNT
.         move      readcnt to str11
.         setitem   Nord0043StatRecord,0,str11
         DISPLAY   *P10:12,"NUMBER OF ORDERS READ : ",READCNT
         if ((ostat = "0")|(ostat = "B"))
         else
                goto read
         endif
         if (oqty = "000000000")
                goto read
         endif
.         match     "0" to ostat
.         goto      read if not equal
         MOVE      OODTEM TO MM
         MOVE      OODTED TO DD
         MOVE      OODTEY TO YY
         pack      odate with mm,dd,yy
         CALL      CVTJUL
         MOVE      juldays TO ORDDATE
         MOVE      ORDDATE TO CHECK
         IF        (CHECK >CHECK2 & CHECK <= CHECK3)
         GOTO      CONT
         ELSE
         GOTO      READ
         ENDIF
CONT
         move      olon to nownfld
         call      nownkey
	call	Trim using OWNCTN
	if (OWNCTN <> "")
		pack	NFULFLD,OWNCTN
		rep	zfill,NFULFLD
		move	C1,NFULPATH
		move	"READ-NFULKEY",Location
		pack	KeyLocation,NFULFLD
		call	NFULKEY
	else
		clear	NFULFLD
		clear	NFULCOMP
	endif
	if (NFULFLD = "0026")
		reset	RUNCODES
		scan	OLNUM,RUNCODES
		if not equal
			move	"fulfill",fulfil
			goto writdmc
		endif
	else
		scan	"TDMC",NFULCOMP
		if equal
			reset	RUNCODES
			scan	OLNUM,RUNCODES
			if not equal
				move	"fulfill",fulfil
				goto writdmc
			endif
		else
			reset	NFULCOMP
			scan	"TRIPLEX",NFULCOMP
			if equal
				reset	RUNCODES
				scan	OLNUM,RUNCODES
				if not equal
					move	"fulfill",fulfil
					goto writdmc
				endif
			endif
		endif
	endif
.END PATCH 1.4 REPLACED LOGIC
.         goto      writdmc if equal
         goto      read
         
WRITDMC
         MATCH     "017717",OLNUM
         goto      read if equal
         MATCH     "018710",OLNUM
         goto      read if equal
         MATCH     "0002",OMLRNUM
         goto      read if equal
         MATCH     "0001",ORTNNUM
         goto      read if equal
         move      olrn to key
         FILEPI    1;TDMCOUT
         read      tdmcout,olrn;;
         goto      read if not over
.Check LiveTC file

         FILEPI    2;LIVETC
         move      key to olrn
         read      LIVETC,olrn;;
         goto      read if not over
writelr	 WRITE     TDMCOUT2,key;key,odate,fulfil,liveflg
         add       c1 to misscnt
         DISPLAY   *P10:16,"NUMBER OF MISSING NOTIFICATION ORDERS : ",MISSCNT
         GOTO      READ


EOJ
.===========================================================

        getinfo  system,str6
        unpack   str6 into str1,str2
        unpack   str2 into str1
        move     c0 to osflag
..0 = unknown
..1 = Windows NT
..2 = WIN32s Windows 3.1x (obsolete)
..3 = Window 95
..4 = Window 98
..5 = Windows 2000
..8 = Windows CE
        if (str1 = "3" or str1 = "4")
	        move     c1 to osflag
        endif
        if (str1 = "1" or str1 = "5")
        	move     c2 to osflag
        endif
.==================================================================
.===============================================================================
.         CLOSE     INFILE
CHKFLAG
        if (osflag = c2)
        	PRTOPEN prfile,"\\NTS0\Laser8","Failed2.dat"
        else
        	PRTOPEN prfile,"Laser8","Failed2.dat"
        endif
.=================================================================================
Page
        prtpage   prfile;*NEWPAGE:
        	         *UNITS=*HIENGLISH;
        clear     rowcount
        clear     row
        move      "300",row
.======================================================================

        clock timestamp,str16
        unpack str16,str8,str6
        unpack str8,str4,mm,dd
        pack   str10,mm,slash,dd,slash,str4
        clear  str8
        unpack str6,hh,mn,ss
        pack   str8,hh,colon,mn,colon,ss
        pack   str24,str10,b4,str8
        prtpage prfile;*p6100:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,str24,*ULOFF,*boldoff;
        add     eightlpi,row
        add     "60",row
.        if        (grpflg = c7)
        prtpage prfile;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Brokerage and List Management",*ULOFF,*boldoff;
.        endif
        add     eightlpi,row
        add     "60",row
        prtpage prfile;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Missing NOTICATION Triplex Orders",*ULOFF,*boldoff;
        add     eightlpi,row
        add     "60",row
        prtpage prfile;*p2000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"LR",*ULOFF,*boldoff;
        prtpage prfile;*p3000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Order Date",*ULOFF,*boldoff;
        prtpage prfile;*p4000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Ful/Ship",*ULOFF,*boldoff;
        prtpage prfile;*p5000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Status",*ULOFF,*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        add     "20",row
        loop
       		add    c1 to rowcount
	        read   infile,seq;str6,odate,str12
        until over

                unpack odate,mm,dd,yy
                pack   str8,mm,slash,dd,slash,yy
	        prtpage prfile;*p2000:row,*ALIGNMENT=*Left,*font=font12,str6;
	        prtpage prfile;*p3000:row,*ALIGNMENT=*Left,*font=font12,str8;
                unpack str12,Pfulfil,str2
	        prtpage prfile;*p4000:row,*ALIGNMENT=*Left,*font=font12,Pfulfil;
                if (str2 = "  ")
		        prtpage prfile;*p5000:row,*font=font12,*ALIGNMENT=*Left,"Not in Print File";
                else
		        prtpage prfile;*p5000:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,str2,*ULOFF,*boldoff;
                endif
        	add     eightlpi,row
	        add     "20",row
        	if (rowcount = "55")
                	goto page
	        endif
        repeat
        PRTCLOSE prfile

.========================================================================================

.	 call 	   OrderSetMouseFree
.         alert     note,"Job is Done!!!!!!!",result
.         move      c0 to clsflg
.         return
         STOP

         include   nordio.inc
         include   nownio.inc
.START PATCH 1.5 ADDED LOGIC
	include	nfulio.inc
.END PATCH 1.5 ADDED LOGIC
 
        INCLUDE   COMLOGIC.inc

