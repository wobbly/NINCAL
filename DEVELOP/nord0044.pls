PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         include   nowndd.inc
         include   norddd.inc
.START PATCH 1.5 ADDED LOGIC
	include	nfuldd.inc
.END PATCH 1.5 ADDED LOGIC
.
RELEASE  INIT       "1.71"         JD  20JAN2006 Updated to fax report back to Triplex/Donnelley
;RELEASE  INIT       "1.7"         DMB 18JUN2005 FM IP CHG
.RELEASE  INIT       "1.6"         jd  06MAY2004  ADDED GETWINVER.
;RELEASE  INIT       "1.5"        ASH 05FEB2002  NINFUL CONVERSION
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
.PATCH 1.71 
faxflag  dim       1
.PATCH 1.71 
.
         MOVE      "NORD0044" to PROGRAM
         MOVE      "Names in the News Ca Inc" TO COMPNME
;begin patch 1.6
         Call           GetWinVer
;end patch 1.6
;Temp
;         OPEN      INFILE,"\\nts1\e\data\FAILED2"
;                    goto eoj
;temp
.>Patch 1.7
.         open      LIVETC,"tdmcord.isi|20.20.30.103:502"
         open      LIVETC,"tdmcord.isi|10.10.30.103:502"
.>Patch 1.7
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
        move    "NORD0044.PLS",Wprognme
        move    "Check for Triplex Notification Records",Wfunction
        move    "Jose Duenas",Wauthor
        move    "1.0",Wrelease
        move    "March 2003",Wreldate
        call     paint

.        loop
.        waitevent
.        repeat
.====================================================================================
Start
.        call OrderSetMouseBusy
        clear readcnt
        clear misscnt
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
         MOVE      "600000" TO NORDFLD
;         MOVE      "460000" TO NORDFLD
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
         move      yes to faxflag
         DISPLAY   *P10:16,"NUMBER OF MISSING NOTIFICATION ORDERS : ",MISSCNT
         GOTO      READ


EOJ
.===========================================================
chkflag
.        getinfo  system,str6
.        unpack   str6 into str1,str2
.        unpack   str2 into str1
.        move     c0 to osflag
..0 = unknown
..1 = Windows NT
..2 = WIN32s Windows 3.1x (obsolete)
..3 = Window 95
..4 = Window 98
..5 = Windows 2000
..8 = Windows CE
.        if (str1 = "3" or str1 = "4")
.	        move     c1 to osflag
.        endif
.        if (str1 = "1" or str1 = "5" or str1 = "6")
.        	move     c2 to osflag
.        endif
;begin patch 1.6
               if             (osflag = c1 or Osflag = C5 or OsFlag = C6)         .nt or win2000 or Windows XP
.PATCH 1.71 
;        	PRTOPEN prfile,"\\NTS0\Laser8","Failed2.dat"
	PRTOPEN prfile,"faxfile",""
               Elseif         (osflag = c3 or OsFlag = C4)         .win 95 98
;        	PRTOPEN prfile,"Laser8","Failed2.dat"
	PRTOPEN prfile,"faxfile",""
.PATCH 1.71 
         Elseif         (osflag = c0)         .Don't know prompt for printer
               splopen        "","R"
               endif
;end patch  1.6
.==================================================================
.===============================================================================
.         CLOSE     INFILE
.        if (osflag = c2)
.        	PRTOPEN prfile,"\\NTS0\Laser8","Failed2.dat"
.        else
.        	PRTOPEN prfile,"Laser8","Failed2.dat"
.        endif
.=================================================================================
Page
.PATCH 1.71 
;        prtpage   prfile;*NEWPAGE:
;        	         *UNITS=*HIENGLISH;
.PATCH 1.71 
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
     	prtpage	prfile;*UNITS=*HIENGLISH;
.PATCH 1.71 
        prtpage prfile;*p1:1,*font=font12,"^[D14025376101^[NBrandi-TDMC  ^]";
		  add    eightlpi,row
.PATCH 1.71 
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

.PATCH 1.71 
                    cALL                getwinver
                    If                  (osflag = c1 | osflag = c5)
         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy ","C:\work\faxfile.prn \\nts0\LASER8 "
         Execute   TASKNAME
;         else
                    ElseIf              (osflag = c3 | osflag = c4)
         PACK      TASKNAME,"c:\command.com /c copy ","c:\work\faxfile.prn \\nts0\LASER8 "
         EXECUTE   TASKNAME
                    Elseif              (osflag = c6)
         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy ","c:\work\faxfile.prn \\nts0\LASER8 "
         Execute   TASKNAME
         endif
         cmatch    yes to faxflag
         if         equal
         if        (osflag = c1 | osflag = C5)
         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy /b ","c:\work\faxfile.prn \\nts2\fax "
         Execute   TASKNAME
         Elseif        (osflag = c3 | osflag = C4)
         PACK      TASKNAME,"c:\command.com /c copy ","c:\work\faxfile.prn \\nts2\fax "
         EXECUTE   TASKNAME
         Elseif        (osflag = c6)
         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy /b ","c:\work\faxfile.prn \\nts2\fax "
         Execute   TASKNAME
			endif
			endif
.PATCH 1.71 

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
