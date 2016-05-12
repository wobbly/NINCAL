	include	common.inc
	include	cons.inc
	include	nsel2dd.inc
	include	norddd.inc
	include	nloldd.inc
	include	ndatdd.inc
	include	ncmpdd.inc
	include	ncntdd.inc

release	init	"1.1"	ASH	09AUG2004	Logo Conversion
.release	init	"1.0"

prfile	pfile
FromDate dim	8
ToDate dim	8
Total	form	9
TotalX	form	9
TotalA	form	9
TotalAX	form	9
TotalB	form	9
TotalBX	form	9
.Contact	form	9(999)
.ContactX form	9(999)
ContactA form	9(999)
ContactAX form	9(999)
ContactB form	9(999)
ContactBX form	9(999)

font2   font
font5   font

.START PATCH 1.1 ADDED LOGIC
NINLogo	PICT
	CREATE	NINLogo=3:13:30:50:
		"\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.1 ADDED LOGIC
        create  font2,"Arial",size=9 
        create  font5,"Arial",size=11

	move	C1,NORDPATH
	move	C1,NLOLPATH
	move	C1,NDATPATH
	move	C1,NCMPPATH
	move	C1,NCNTPATH
	call	Paint
	display	*p10:7,"Hit F5 to Quit"
	trap	STOP IF F5
	display	*p10:8,"Date Range (Enter as MMDDYYYY or MM/DD/YYYY)"
KeyInFromDate
	keyin	*p10:10,"From : ",*t30,*KCON,str10,*KCOFF;
	call	RemoveChar using str10,SLASH
	call	Trim using str10
	if (str10 <> "")
		unpack	str10,MM,DD,CC,YY
		pack	str10,MM,SLASH,DD,SLASH,CC,YY
		pack	FromDate,CC,YY,MM,DD
		display	*p10:10,"From : ",str10;
		move	"Y",str1
		display	*p30:10,"Correct? ",str1;
		keyin	*p30:10,"Correct? ",*t10,*KCON,*rv,str1,*KCOFF;
		if (str1 <> "Y" & str1 <> "y")
			goto KeyInFromDate
		endif
	else
		move	"00000000",FromDate
		pack	str10,"          "
	endif
	display	*p10:10,"From : ",str10;
KeyInToDate
	keyin	*p10:11,"To   : ",*t30,*KCON,str10,*KCOFF;
	call	RemoveChar using str10,SLASH
	call	Trim using str10
	if (str10 <> "")
		unpack	str10,MM,DD,CC,YY
		pack	str10,MM,SLASH,DD,SLASH,CC,YY
		pack	ToDate,CC,YY,MM,DD
		display	*p10:11,"To   : ",str10;
		move	"Y",str1
		display	*p30:11,"Correct? ",str1;
		keyin	*p30:11,"Correct? ",*t10,*KCON,*rv,str1,*KCOFF;
		if (str1 <> "Y" & str1 <> "y")
			goto KeyInToDate
		endif
	else
		move	"99999999",ToDate
		pack	str10,"          "
	endif
	display	*p10:11,"To   : ",str10;
.	goto reportgo
	loop
		move	"NSEL2SEQ",Location
		pack	KeyLocation,"Key: Seq"
		call	NSEL2SEQ
		until over
		if (NSEL2DATE >= FromDate & NSEL2DATE <= ToDate)
			add	C1,howmany
			display	*p10:12,"Records Processed : ",howmany
			if (NSEL2CODE = "1")	.LR Records
				move	NSEL2LR,NORDFLD
				move	"NORDKEY",Location
				pack	KeyLocation,"Key: ",NORDFLD
				call	NORDKEY
				call	Trim using OCOCODE
				move	C0,N3
				move	OCOCODE,N3
				add	C1,N3
				pack	NDATFLD,OLNUM
			else			.LOL Records
				move	NSEL2LR,NLOLFLD
				move	"NLOLKEY",Location
				pack	KeyLocation,"Key: ",NLOLFLD
				call	NLOLKEY
				move	NLOLCNum,NCMPFLD
				move	"NCMPKEY",Location
				pack	KeyLocation,"Key: ",NCMPFLD
				call	NCMPKEY
				call	Trim using NCMPCnt
				move	C0,N3
				move	NCMPCnt,N3
				add	C1,N3
				pack	NDATFLD,NLOLList
			endif
			move	"NDATKEY",Location
			pack	KeyLocation,"Key: ",NDATFLD
			call	NDATKEY
			if (NSEL2NUM = "XXXX")
				add	C1,TotalX
				if (NDATCONV = "1")
					add	C1,ContactAX(N3)
					add	C1,TotalAX
				else
					add	C1,ContactBX(N3)
					add	C1,TotalBX
				endif
			else
				add	C1,Total
				if (NDATCONV = "1")
					add	C1,ContactA(N3)
					add	C1,TotalA
				else
					add	C1,ContactB(N3)
					add	C1,TotalB
				endif
			endif
		endif
	repeat
ReportGo
	call	GetWinVer
	if (osflag = c1 | Osflag = C5 | osflag = c6)         .nt win2k Xp
		PRTOPEN prfile,"\\NTS0\Laser2","FAXFILE.PRN"
	elseif (osflag = c3 | OSflag =c4)         .win 95 98
		PRTOPEN prfile,"Laser2","FAXFILE.PRN"
	else   .(osflag = c0)         .Don't know prompt for printer
		PRTOPEN prfile,"-","FAXFILE.PRN"
	endif
.Print Report
	display	*p10:12,"Creating Report"
        move    "500",column
        move    "2500",column1
        move    "3125",column2
        move    "3725",column3
        move    "5200",column4
        move    "5815",column5
        move    "6425",column6
.
	clock   timestamp,timestamp
	unpack  timestamp,CC,YY,MM,DD
	pack    str8,MM,SLASH,DD,SLASH,CC,YY
	prtpage prfile;*UNITS=*HIENGLISH;
	move    "300",row
.START PATCH 1.1 REPLACED LOGIC
.	prtpage prfile;*p2700:row,*font=font10,"Names";
.	prtpage prfile;*font=font11,"  in the News";
.	add     eightlpi,row
.	add     eightlpi,row
.	add     eightlpi,row
.	prtpage prfile;*p1000:row,*pensize=10,*line=7100:row;
.	add     "60",row
.	prtpage prfile;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.	prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
.....................
	prtpage	prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
	add     eightlpi,row
	add     eightlpi,row
	add     eightlpi,row
	add     "60",row
	add     eightlpi,row
.END PATCH 1.1 REPLACED LOGIC
	add     eightlpi,row
	add     eightlpi,row 
	add     eightlpi,row 
	prtpage prfile;*p3000:row,*font=font5,*boldon,"SELECT HISTORY REPORT",*boldoff;
	add     eightlpi,row 
	add     eightlpi,row 
	add     eightlpi,row 
.	prtpage prfile;*pcolumn:row,*font=font5,*boldon,*ALIGNMENT=*LEFT,"Contact";
.	prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,"Valid Selects";
.	prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,"Keyed-In Selects",*boldoff;
	unpack	FromDate,str4,MM,DD
	pack	str10,MM,SLASH,DD,SLASH,str4
	prtpage prfile;*pcolumn:row,"From Date: ",str10;
	add     eightlpi,row 
	unpack	ToDate,str4,MM,DD
	pack	str10,MM,SLASH,DD,SLASH,str4
	prtpage prfile;*pcolumn:row,"To Date:   ",str10;
	add     eightlpi,row 
	add     eightlpi,row 
	prtpage prfile;*pcolumn2:row,*boldon,"Converted Lists";
	prtpage prfile;*pcolumn5:row,"Un-Converted Lists";
	add     sixlpi,row 
	add     sixlpi,row 
	prtpage prfile;*pcolumn:row,*font=font5,"Contact";
	prtpage prfile;*pcolumn1:row,"Valid Selects";
	prtpage prfile;*pcolumn3:row,"Keyed-In Selects";
	prtpage prfile;*pcolumn4:row,"Valid Selects";
	prtpage prfile;*pcolumn6:row,"Keyed-In Selects",*boldoff;
	add     sixlpi,row
	prtpage prfile;*pcolumn:row,*pensize=20,*line=7800:row,*font=font2;
	add     eightlpi,row
.
test
	move	C0,howmany
	for N3,"1","999"
		if (ContactA(N3) > C0 | ContactAX(N3) > C0 | ContactB(N3) > C0 | ContactBX(N3) > C0)
			move	N3,N2
			sub	C1,N2
			move	N2,NCNTFLD
			rep	zfill,NCNTFLD
			move	"NCNTKEY",Location
			pack	KeyLocation,"Key: ",NCNTFLD
			call	NCNTKEY
			prtpage prfile;*pcolumn:row,CNTNAME;
			move	ContactA(N3),str9
			call	FormatNumeric using str9,str11
			call	Trim using str11
			prtpage prfile;*pcolumn1:row,str11;
			move	ContactAX(N3),str9
			call	FormatNumeric using str9,str11
			call	Trim using str11
			prtpage prfile;*pcolumn3:row,str11;
			move	ContactB(N3),str9
			call	FormatNumeric using str9,str11
			call	Trim using str11
			prtpage prfile;*pcolumn4:row,str11;
			move	ContactBX(N3),str9
			call	FormatNumeric using str9,str11
			call	Trim using str11
			prtpage prfile;*pcolumn6:row,str11;
			add     eightlpi,row
			add	C1,howmany
			display	*p10:14,"Records Processed : ",howmany
		endif
	repeat
	add     eightlpi,row
	add     eightlpi,row
	prtpage prfile;*pcolumn:row,"TOTAL";
	move	TotalA,str9
	call	FormatNumeric using str9,str11
	call	Trim using str11
	prtpage prfile;*pcolumn1:row,str11;
	move	TotalAX,str9
	call	FormatNumeric using str9,str11
	call	Trim using str11
	prtpage prfile;*pcolumn3:row,str11;
	move	TotalB,str9
	call	FormatNumeric using str9,str11
	call	Trim using str11
	prtpage prfile;*pcolumn4:row,str11;
	move	TotalBX,str9
	call	FormatNumeric using str9,str11
	call	Trim using str11
	prtpage prfile;*pcolumn6:row,str11;
.
	add     eightlpi,row
	add     eightlpi,row
	prtpage prfile;*pcolumn:row,"GRAND TOTAL";
	add     eightlpi,row
	prtpage prfile;*pcolumn:row,"Valid Selects";
	move	Total,str9
	call	FormatNumeric using str9,str11
	call	Trim using str11
	prtpage prfile;*pcolumn1:row,str11;
	add     eightlpi,row
	prtpage prfile;*pcolumn:row,"Keyed-In Selects";
	move	TotalX,str9
	call	FormatNumeric using str9,str11
	call	Trim using str11
	prtpage prfile;*pcolumn1:row,str11;
.
STOP
	prtclose prfile
	stop
	
	include	nsel2io.inc
	include	ncmpio.inc
	include	ndatio.inc
	include	nlolio.inc
	include	nordio.inc
	include	ncntio.inc
	include	comlogic.INC