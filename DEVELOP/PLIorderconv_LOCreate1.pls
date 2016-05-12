.............................................................
.....This program reads through Orders and determines........
.....if the List Owner information is represented.  If.......
.....not those entries are created...........................
.............................................................
.............................................................
	include common.inc
	include cons.inc
	include	nowndd.inc
	include	gnxtdd.inc
.	include	e:\nin\library\plb_src\conversion\nowndd.inc
.	include	e:\nin\library\plb_src\conversion\gnxtdd.inc
.
fORDERSIO     list
U_LEVEL     form	4    //00001-00004
PO_NUM      dim		12   //00005-00016
BATCHNUM    dim		12   //00017-00028
LISTNAME    dim		40   //00029-00068
TYPE	    dim		5    //00069-00073
USERID      dim		5    //00074-00078
CREATION    dim		8    //00079-00086
REVISION    dim		8    //00087-00094
PO_DATE     dim		8    //00095-00102
PO_SDTE     dim		8    //00103-00110
PO_WDTE     dim		8    //00111-00118
PO_MDTE     dim		8    //00119-00126
PO_MDTE2    dim		8    //00127-00134
PO_IDTE     dim		8    //00135-00142
INV_STAT    dim		2    //00143-00144
PO_HIST     dim		1    //00145-00145
PROFORMA    dim		1    //00146-00146
PO_SHIP     dim		1    //00147-00147
INV_NUM     dim		12   //00148-00159
LISTID      dim		12   //00160-00171
OWNERID     dim		12   //00172-00183
CLIENTID    dim		12   //00184-00195
MAILERID    dim		12   //00196-00207
SHIPTOID    dim		12   //00208-00219
COMPFID     dim		12   //00220-00231
SLSMANID    dim		12   //00232-00243
PROFRMID    dim		12   //00244-00255
EXCHID      dim		12   //00256-00267
PO_OCODE    dim		12   //00268-00279
PO_OFFER    dim		15   //00280-00294
PO_PRINT    form	2    //00295-00296
PO_REV      form	2    //00297-00298
PO_REVCD    dim		5    //00299-00303
PO_CAN      dim		1    //00304-00304
PO_CLR      dim		1    //00305-00305
PO_CCX      dim		1    //00306-00306
PO_LP       dim		1    //00307-00307
PO_USE      dim		1    //00308-00308
PO_NET      dim		1    //00309-00309
PO_EXCH     dim		1    //00310-00310
PO_MF_R     dim		1    //00311-00311
PO_MF_P     dim		1    //00312-00312
PO_REINV    form	2    //00313-00314
PO_PHONE    dim		17   //00315-00331
PO_FBILL    dim		15   //00332-00346
PO_CH_R     dim		10   //00347-00356
PO_CH_P     dim		10   //00357-00366
PO_QCOMM    dim		15   //00367-00381
PO_CONT     dim		20   //00382-00401
PO_CLORD    dim		20   //00402-00421
PO_MGCON    dim		15   //00422-00436
PO_ACDS1    dim		20   //00437-00456
PO_ACDS2    dim		20   //00457-00476
PO_ACDS3    dim		20   //00477-00496
PO_ACDS4    dim		20   //00497-00516
PO_ACDS5    dim		20   //00517-00536
PO_SPDS     dim		25   //00537-00561
PO_KEYCD    dim		15   //00562-00576
PO_LAB1     dim		25   //00577-00601
PO_LAB2     dim		25   //00602-00626
PO_CATTN    dim		20   //00627-00646
PO_SATTN    dim		20   //00647-00666
OWN_ATTN    dim		20   //00667-00686
PO_FATTN    dim		20   //00687-00706
PO_SCOMM    dim		30   //00707-00736
PO_MAT      dim		40   //00737-00776
PO_VIA      dim		30   //00777-00806
PO_SEG1     dim		40   //00807-00846
PO_SEG2     dim		40   //00847-00886
PO_OMIT     dim		75   //00887-00961
PO_AC_R1    form	7.2  //00962-00971
PO_AC_R2    form	7.2  //00972-00981
PO_AC_R3    form	7.2  //00982-00991
PO_AC_R4    form	7.2  //00992-01001
PO_AC_R5    form	7.2  //01002-01011
PO_AC_P1    form	7.2  //01012-01021
PO_AC_P2    form	7.2  //01022-01031
PO_AC_P3    form	7.2  //01032-01041
PO_AC_P4    form	7.2  //01042-01051
PO_AC_P5    form	7.2  //01052-01061
PO_SAMT     form	7.2  //01062-01071
PO_RAT_R    form	7.2  //01072-01081
PO_RAT_P    form	7.2  //01082-01091
PO_Q_ORD    form	10   //01092-01101
PO_Q_SHP    form	10   //01102-01111
EXQTYORD    form	10   //01112-01121
EXQTYSHP    form	10   //01122-01131
EXARRATE    form	7.2  //01132-01141
EXARPER     dim		1    //01142-01142
EXAPRATE    form	7.2  //01143-01152
EXAPPER     dim		1    //01153-01153
PO_PP_R     form	7.2  //01154-01163
PO_PP_P     form	7.2  //01164-01173
PO_RC_RAT   form	7.2  //01174-01183
PO_RC_PER   dim		1    //01184-01184
PO_BRK      form	3.1  //01185-01189
PO_MNG      form	3.1  //01190-01194
PO_MLR      form	3.1  //01195-01199
PO_NETPC    form	3.1  //01200-01204
AR_REMIT    dim		3    //01205-01207
AP_REMIT    dim		3    //01208-01210
FLAG1       dim		1    //01211-01211
FLAG2       dim		1    //01212-01212
FLAG3       dim		1    //01213-01213
FLAG4       dim		1    //01214-01214
FLAG5       dim		1    //01215-01215
		   listend
.
fINVCHDIO     record
POSTED      dim           1         //00001-00001
U_LEVEL     dim          4         //00002-00005
INV_NUM     dim           12        //00006-00017
PO_NUM      dim           12        //00018-00029
BATCHNUM    dim           12        //00030-00041
LISTID      dim           12        //00042-00053
OWNERID     dim           12        //00054-00065
CLIENTID    dim           12        //00066-00077
MAILERID    dim           12        //00078-00089
COMPFID     dim           12        //00090-00101
SHIPTOID    dim           12        //00102-00113
SLSMANID    dim           12        //00114-00125
EXCHID      dim           12        //00126-00137
PO_OCODE    dim           12        //00138-00149
PO_OFFER    dim           15        //00150-00164
AR_JOURN    dim           12        //00165-00176
AP_JOURN    dim           12        //00177-00188
CH_JOURN    dim           12        //00189-00200
LISTNAME    dim           40        //00201-00240
BILL_ID     dim           12        //00241-00252
BILLATTN    dim           20        //00253-00272
INV_DATE    dim           8         //00273-00280
PO_DATE     dim           8         //00281-00288
MAL_DATE    dim           8         //00289-00296
MAL_DATE2   dim           8         //00297-00304
DUE_DATE    dim           8         //00305-00312
AP_DUEDT    dim           8         //00313-00320
CREATION    dim           8         //00321-00328
REVISION    dim           8         //00329-00336
ORIGDATE    dim           8         //00337-00344
AR_CLOSE    dim           1         //00345-00345
AP_CLOSE    dim           1         //00346-00346
CH_CLOSE    dim           1         //00347-00347
AR_CDATE    dim           8         //00348-00355
AP_CDATE    dim           8         //00356-00363
CH_CDATE    dim           8         //00364-00371
CR_STAT     dim           5         //00372-00376
BROK_PC     dim          5       //00377-00381
MNGR_PC     dim          5       //00382-00386
MAIL_PC     dim          5       //00387-00391
AR_NET      dim          3         //00392-00394
AP_NET      dim          3         //00395-00397
AR_RUN      dim          3         //00398-00400
AP_RUN      dim          3         //00401-00403
AR_REF      dim           15        //00404-00418
AP_REF      dim           15        //00419-00433
PO_SEG1     dim           40        //00434-00473
PO_SEG2     dim           40        //00474-00513
PRINTED     dim          3         //00514-00516
REVISED     dim          3         //00517-00519
TYPE        dim           5         //00520-00524
PROFORMA    dim           1         //00525-00525
PO_LP       dim           1         //00526-00526
PO_USE      dim           1         //00527-00527
PO_SHIP     dim           1         //00528-00528
PO_CAN      dim           1         //00529-00529
PO_EXCH     dim           1         //00530-00530
PO_NET      dim           1         //00531-00531
CH_CALC     dim           1         //00532-00532
QTYSHIP     dim          10        //00533-00542
QTYCLEAN    dim          10        //00543-00552
AR_BQTY     dim          10        //00553-00562
AP_BQTY     dim          10        //00563-00572
AR_RCQTY    dim          10        //00573-00582
AP_RCQTY    dim          10        //00583-00592
AR_BRATE    dim          10       //00593-00602
AP_BRATE    dim          10       //00603-00612
AR_RCRAT    dim          10       //00613-00622
AP_RCRAT    dim          10       //00623-00632
AR_B_MF     dim           1         //00633-00633
AP_B_MF     dim           1         //00634-00634
AR_RCMF     dim           1         //00635-00635
AP_RCMF     dim           1         //00636-00636
AR_GBASE    dim          10       //00637-00646
AP_GBASE    dim          10       //00647-00656
AR_RC_GB    dim          10       //00657-00666
AP_RC_GB    dim          10       //00667-00676
AR_BCOMM    dim          5       //00677-00681
AP_BCOMM    dim          5       //00682-00686
AR_RC_PC    dim          5       //00687-00691
AP_RC_PC    dim          5       //00692-00696
AR_NBASE    dim          10       //00697-00706
AP_NBASE    dim          10       //00707-00716
AR_RC_NB    dim          10       //00717-00726
AP_RC_NB    dim          10       //00727-00736
AR_ACTOT    dim          10       //00737-00746
AP_ACTOT    dim          10       //00747-00756
AR_CMTOT    dim          10       //00757-00766
AP_CMTOT    dim          10       //00767-00776
AR_SUBT     dim          10       //00777-00786
AP_SUBT     dim          10       //00787-00796
CH_ACT      dim          10       //00797-00806
CH_ADD      dim          10       //00807-00816
AR_TOTAL    dim          10       //00817-00826
AP_TOTAL    dim          10       //00827-00836
AR_PAYMT    dim          10       //00837-00846
AP_PAYMT    dim          10       //00847-00856
CH_PAYMT    dim          10       //00857-00866
AR_BAL      dim          10       //00867-00876
AP_BAL      dim          10       //00877-00886
CH_BAL      dim          10       //00887-00896
AR_REFND    dim          10       //00897-00906
AP_REFND    dim          10       //00907-00916
CH_REFND    dim          10       //00917-00926
AR_CHECK    dim           15        //00927-00941
AP_CHECK    dim           15        //00942-00956
CH_CHECK    dim           15        //00957-00971
AR_ADATE    dim           8         //00972-00979
AP_ADATE    dim           8         //00980-00987
CH_ADATE    dim           8         //00988-00995
CLT_ATTN    dim           20        //00996-01015
OWN_ATTN    dim           20        //01016-01035
FLAG1       dim           1         //01036-01036
FLAG2       dim           1         //01037-01037
FLAG3       dim           1         //01038-01038
FLAG4       dim           1         //01039-01039
FLAG5       dim           1         //01040-01040
PO_KEYCD    dim           25        //01041-01065
PO_MGCON    dim           15        //01066-01080
EXQTYSHP    dim          10        //01081-01090
EXARRATE    dim          10        //01091-01100
EXARPER     dim           1         //01101-01101
EXARTOTAL   dim          12        //01102-01113
EXAPRATE    dim          10        //01114-01123
EXAPPER     dim           1         //01124-01124
EXAPTOTAL   dim          12        //01125-01136
ARXREMIT    dim           3         //01137-01139
APXREMIT    dim           3         //01140-01142
CHXREMIT    dim           3         //01143-01145
ARXCOMMENT  dim           50        //01146-01195
APXCOMMENT  dim           50        //01196-01245
CHXCOMMENT  dim           50        //01246-01295
ARXRATE     dim          12        //01296-01307
APXRATE     dim          12        //01308-01319
CHXRATE     dim          12        //01320-01331
ARXTOT      dim          12        //01332-01343
APXTOT      dim          12        //01344-01355
CHXTOT      dim          12        //01356-01367
              recordend
.
fNAMESIO     record
NAMEID      dim           12        //00001-00012
NAME        dim           40        //00013-00052
ATTN        dim           20        //00053-00072
ADDR1       dim           35        //00073-00107
ADDR2       dim           35        //00108-00142
ADDR3       dim           35        //00143-00177
CITY        dim           25        //00178-00202
STATE       dim           2         //00203-00204
ZIP         dim           10        //00205-00214
COUNTY      dim           20        //00215-00234
COUNTRY     dim           3         //00235-00237
PHONE       dim           17        //00238-00254
EXT         dim           4         //00255-00258
FAX         dim           17        //00259-00275
MODEM1      dim           17        //00276-00292
MODEM2      dim           17        //00293-00309
USERID      dim           15        //00310-00324
CREATION    dim           8         //00325-00332
REVISION    dim           8         //00333-00340
TYPE        dim           15        //00341-00355
ATTN2       dim           15        //00356-00370
ATTN3       dim           15        //00371-00385
GENUSE1     dim           1         //00386-00386
GENUSE2     dim           1         //00387-00387
GENUSE3     dim           1         //00388-00388
GENUSE4     dim           15        //00389-00403
GENUSE5     dim           15        //00404-00418
TAXID       dim           20        //00419-00438
NINID       dim           6         //00439-00444
              recordend
.
PLIFile		file	.Order Input File
PLIFileI	ifile	.PL NAMES query File
PLIFile2I	ifile	.Invoice Input File
PLIFile5I	ifile	.List Owner xRef File (from spreadsheet)
.
OutFile		file	.PL/NIN Xref file
OutFile2	file	.New Owner File Records
.
recordnum	form	9
recordnum2	form	9
date1		form	16
taskname2	dim	300
taskname3	dim	500
route		form	"0"
routedir	dim	3
ListViews	ListView
.
books   automation
book    automation
sheets  automation
sheet   automation
ex      automation      class="Excel.Application"
SheetsDefault   integer 4,"0x00000000"
AlignCenter integer 4,"0xffffeff4"
AlignTop integer 4,"0xFFFFEFC0"
xlMinimized integer 4,"0xFFFFEFD4"
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_R8	EQU 5           .Double - 8 byte Real
xlColumnWidth	variant
xlRowHeight	variant
xlRowHeight2	variant
.

release		init	"1.1"	07SEP2007	AH	Moved "P" to ONWLOC
.							Removed 5 year limit
.release		init	"1.0"	04SEP2007	AH	Initial release

.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create	xlColumnWidth,VarType=VT_R8,VarValue="8.5"
        create	xlRowHeight,VarType=VT_R8,VarValue="27.0"
        create	xlRowHeight2,VarType=VT_R8,VarValue="52.5"
.
	create  ListViews=110:180:10:160
	activate ListViews
	ListViews.InsertColumn using "",0,0
	ListViews.InsertColumn using "",0,1
.	
	clock	timestamp,timestamp
	unpack	timestamp,CC,YY,MM,DD
	call	CVTJUL
	//.sub 5 years
	sub	"1825",JULDAYS,date1
.	goto excelstuff
	move    "NOWNNXT",GNXTFLD
.Open Current PL/NIN mapping spreadsheet and create an indexed reference file from it
	DISPLAY   *P10:8,"Creating Prep Files...",N9
.
	erase	"c:\work\owner_NIN.dat"
	prep	OutFile,"c:\WORK\owner_NIN.dat",exclusive
	erase	"c:\work\NINOWNnew.dat"
	prep	OutFile2,"c:\WORK\NINOWNnew.dat",exclusive
.
OPENFILES
	pack	taskname3,"I need to open the latest LO mapping spreadsheet.",newline,"You will need to provide that file.",newline,"The lastest iteration is named 'LO Data Exception073107.xls'"
	alert	note,taskname3,result
.Open Excel application
        create  ex
        setprop ex,*WindowState=xlMinimized
        setprop ex,*Visible="True"
	setprop ex.CommandBars("Standard"),*Visible="True"
	setprop ex.CommandBars("Formatting"),*Visible="True"
	setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
        ex.GetOpenFileName giving taskname
.Create Workbooks collection
        getprop ex,*Workbooks=books
        books.open using taskname
        books.item giving book using 1
.Create Worksheets collection
        getprop book,*Sheets=sheets
        sheets.item giving sheet using 1
.
        move	".xls",str4
        scan	str4,taskname
        if equal
        	movefptr taskname,result
        	sub	C1,result
        	SETLPTR	taskname,result
        	reset	taskname
        endif
        pack	taskname,taskname,"_new.xls"
        sheet.saveas using taskname
.
	pack	taskname3,"I have created a new working version named:",newline,taskname,newline,"I will work with this file"
	alert	note,taskname3,result
.
	trap	Objecterr if Object
	move	C3,N9
	loop
		clear	str12
		clear	str6
		clear	str40
		DISPLAY   *P10:8,"LO Record Number: ",N9

		move	N9,str9
		call	Trim using str9
.
		pack	str15,"A",str9
		getprop sheet.range(str15),*Value=str12
		call	Trim str12
		until (str12 = "")
.
		pack	str15,"D",str9
		getprop sheet.range(str15),*Value=str6
.
		pack	str15,"E",str9
		getprop sheet.range(str15),*Value=str40
.
		call	Trim using str6
		if (str6 > "")
			type	str6
			if not equal
				//.We actually want to write these out, so that we know they are on the spreadsheet
				clear	str6
				//.goto SkipWrite
			else
				count	N2,str6
				if (N2 > 4)
					//.We actually want to write these out, so that we know they are on the spreadsheet
					clear	str6
					//.goto SkipWrite
				else
					call	Trim using str6
					move	C0,N4
					move	str6,N4
					move	N4,str4
					rep	zfill,str4
					move	str4,str6
				endif
			endif
		endif
.
SkipWrite
		move	str12,fNAMESIO.NAMEID
		move	str40,fNAMESIO.NAME
		move	str6,fNAMESIO.NINID
		write	OutFile,SEQ;fNAMESIO
		add	C1,N9
	repeat
	trapclr Object
	close	OutFile
.	
	DISPLAY   *P10:8,"Indexing List Owner XRef File..."	
	index	"c:\work\owner_NIN.dat c:\work\owner_NIN,L444 -1-12"
.	pack	taskname3,"!\\nts0\c\apps\plb\code\sunindex c:\work\owner_NIN.dat c:\work\owner_NIN,L444 -1-12"
.	execute	taskname3
	open PLIFile5I,"c:\work\owner_NIN.isi",exclusive
.
.Get PL NAMES file Input File - and then Index it!
	loop
		move	"names",str45
		pack	taskname,"c:\work\C01\ascii"
		GETFNAME OPEN,"Choose PLI Names file:",str45,taskname,"txt"
		until not over
		pack	taskname,"You must select a Names file!",newline,"Do you want to continue?"
		alert	plain,taskname,result
		if (result <> 1)
			shutdown
		endif
 	repeat
 	movelptr str45,howmany
 	move	".",str1
 	scan	str1,str45
 	if not over
 		movefptr str45,result
 		sub	C1,result
 		reset	str45
 		setlptr	str45,result
 		pack	str55,str45,".ISI"
 		setlptr	str45,howmany
 	endif
	DISPLAY   *P10:8,"Indexing Names File...                    "	
	pack	taskname3,"\\nts0\c\apps\plb\code\sunindex ",taskname,str45,B1,taskname,str55," -1-12"
	execute	taskname3
	pack	taskname2,taskname,str55
	open PLIFileI,taskname2,exclusive
.
StartRoute
.	move	C1,route
	if (route = C0)	.Brokerage route
		move	"C01",routedir
	else		.Management route
		move	"C02",routedir
	endif
.......Input Files - allow user interaction!!.......
.Get ORDER Input File
	pack	str55,"Choose PLI Order file from ",routedir,":"
	loop
		move	"orders",str45
		pack	taskname,"c:\work\",routedir,"\ascii"	."
		GETFNAME OPEN,str55,str45,taskname,"txt"
		until not over
		pack	taskname,"You must select an Order Input file!",newline,"Do you want to continue?"
		alert	plain,taskname,result
		if (result <> 1)
			shutdown
		endif
 	repeat
 	pack	taskname2,taskname,str45
        open PLIFile,taskname2,exclusive
.
.Get INVOICE Input File - and then Index it!
	pack	str55,"Choose PLI Invoice Header file from ",routedir,":"
	loop
		move	"invchd",str45
		pack	taskname,"c:\work\",routedir,"\ascii"	."
		GETFNAME OPEN,str55,str45,taskname,"txt"
		until not over
		pack	taskname,"You must select an Invoice Header file!",newline,"Do you want to continue?"
		alert	plain,taskname,result
		if (result <> 1)
			shutdown
		endif
 	repeat
 	movelptr str45,howmany
 	move	".",str1
 	scan	str1,str45
 	if not over
 		movefptr str45,result
 		sub	C1,result
 		reset	str45
 		setlptr	str45,result
 		pack	str55,str45,".ISI"
 		setlptr	str45,howmany
 	endif
	DISPLAY   *P10:8,"Indexing Invoice File..."	
	pack	taskname3,"!\\nts0\c\apps\plb\code\sunindex ",taskname,str45,B1,taskname,str55," -18-29"
	execute	taskname3
	pack	taskname2,taskname,str55
	open PLIFile2I,taskname2,exclusive
.
.Clear Display notice
	DISPLAY   *P10:8,"                                "
.
	loop
startloop
        	read PLIFile,seq;fORDERSIO
		until over
.
		add	C1,recordnum
		DISPLAY   *P10:8,"Record Number: ",recordnum
.Test logic
.		until (recordnum > 99)
.
.		if (COMPFID = "SOUNDSTRU   ")
.			CALL	DREWTEST
.		ELSE
.			GOTO STARTLOOP
.		ENDIF
.START PATCH 1.1 REMOVED LOGIC
..Added logic to only look at records within last 5 years
.		unpack	PO_DATE,CC,YY,MM,DD
.		call	CVTJUL
.		if (JULDAYS < date1)
.			GOTO STARTLOOP
.		endif
.		add	C1,recordnum2
.		DISPLAY   *P10:10,"Records within 5 years: ",recordnum2
.END PATCH 1.1 REMOVED LOGIC
.Exception Trapping Begins here!!
.Records entered here have been cleared for Exception!!
.Any Exceptions here need to be applied to Order Conversion AND Exchange Conversion!!
		if (PO_NUM = "03851       " | PO_NUM = "02368       ")
			//.Cleared by John on 7/24/2007
			GOTO STARTLOOP
		endif
		if (route = C0)
			//.Cleared by John on 7/12/2007
			if (PO_NUM = "03794       " | PO_NUM = "06173       " | PO_NUM = "25822       " | PO_NUM = "60174       ")
				GOTO STARTLOOP
			//.Cleared by John on 7/20/2007
			elseif (PO_NUM = "32767       " | PO_NUM = "23664       ")
				GOTO STARTLOOP
			endif
		else
			//.Cleared by John on 7/12/2007
			if (PO_NUM = "60174       ")
				GOTO STARTLOOP
			endif
		endif
.Now Read PLI Invoice file and refresh some vars.
		packkey	PO_NUM,PO_NUM
		read	PLIFile2I,PO_NUM;fINVCHDIO
		if not over
			move	fINVCHDIO.OWNERID,str12
			call	Trim using str12
			if (str12 <> "")
				reset	str12
				move	str12,OWNERID
			endif
		endif
......................
		call	Trim using PO_NUM
		if (PO_NUM = "")
			goto startloop
		endif
..List Owner Number..
		call	Trim using OWNERID
		if (OWNERID = "")
		elseif (OWNERID = "'"  | OWNERID = "NWPALATE")
		else
			packkey	OWNERID,OWNERID
			read	PLIFile5i,OWNERID;fNAMESIO
			if over
				read	PLIFilei,OWNERID;fNAMESIO
				if not over
					pack	str10,"New:"
					call	WriteToListView
				endif
			else
				move	fNAMESIO.NINID,str4
				call	Trim using str4
				if (str4 = "")
					pack	str10,"Updated:"
					call	WriteToListView
				else
					type	str4
					if not equal
						pack	str10,"Updated:"
						call	WriteToListView
					else
						move	C0,N4
						move	str4,N4
						if (N4 > C0)
							move	N4,str4
							rep	zfill,str4
							move	str4,NOWNFLD
							call	NOWNTST
							if over
								pack	str10,"Updated:"
								call	WriteToListView
							endif
						else
							pack	str10,"Updated:"
							call	WriteToListView
						endif
					endif
				endif
			endif
		endif
	repeat
	//.alert	note,"check numbers",result
	if (route = C0)
		close	PLIFile
		close	PLIFile2I
		add	C1,route
		move	C0,recordnum
		move	C0,recordnum2	
		goto StartRoute
	endif
.Clear the stack
	loop
		noreturn
		until over
	repeat
.Write out records
	ListViews.GetItemCount giving howmany
	if (howmany > 0)
		sub	C1,howmany
		for result,C0,howmany
			ListViews.GetItemText giving OWNERID using result
			ListViews.GetItemText giving str10 using result,1
			call	WriteToFile
		repeat
	endif
.
CleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        setprop ex,*DisplayAlerts=OFALSE
        destroy ex
.
	shutdown

WriteToListView
	ListViews.FindItem giving N9 using SEQ,OWNERID
	if (N9 = SEQ)
		ListViews.InsertItem giving N9 using OWNERID
		ListViews.SetItemText using N9,str10,1
	endif
	return

WriteToFile
	packkey	OWNERID,OWNERID
	read	PLIFilei,OWNERID;fNAMESIO
.Locate and Display next logical Owner Number
	call    GNXTKEY
	if over
.Terminate Program
		noreturn
		goto CleanUp
	endif
	bump    GNXTNUM,2
	move	C0,N4
	move    GNXTNUM,N4
	loop
		add     C1,N4
		move    N4,str4
		rep     zfill,str4
		pack    NOWNFLD,str4
		rep     zfill,NOWNFLD
		call    NOWNTST
		until over
	repeat
	pack	GNXTNUM,"00",NOWNFLD
	call	GNXTUPD
.Initialize varlist
	clear	OWNVARS
.
.START PATCH 1.1 REMOVED LOGIC
.	move	"B",OWNLOC
	move	"P",OWNLOC
.END PATCH 1.1 REMOVED LOGIC
	move	NOWNFLD,OWNLON
	move	fNAMESIO.ATTN,OWNLONM
	move	fNAMESIO.NAME,OWNOCPY
	move	fNAMESIO.ADDR1,OWNLOSA
	move	fNAMESIO.CITY,OWNLOCTY
	move	fNAMESIO.STATE,OWNLOS
	move	fNAMESIO.ZIP,OWNLOZC
	move	fNAMESIO.PHONE,str25
	call	Trim using str25
	call	RemoveChar using str25,DASH
	move	"(",str1
	call	RemoveChar using str25,str1
	move	")",str1
	call	RemoveChar using str25,str1
	type	str25
	if equal
		move	str25,OWNTELE
	endif
	move	fNAMESIO.TYPE,OWNPASS
	move	fNAMESIO.REVISION,str8
	call	Trim using str8
	unpack	str8,str4,MM,DD
	pack	OWNRDTE,MM,DD,str4
.owngally dim       1         154-154   'T'rue = lcr's get combined request
	move	fNAMESIO.TAXID,OWNTAXID
	move	fNAMESIO.FAX,str25
	call	Trim using str25
	call	RemoveChar using str25,DASH
	move	"(",str1
	call	RemoveChar using str25,str1
	move	")",str1
	call	RemoveChar using str25,str1
	type	str25
	if equal
		move	str25,OWNFAX
	endif
.
	write	OutFile2,SEQ;str10,OWNERID,OWNVARS
.
	call	Trim using OWNERID
	move	C3,N9
	loop
		DISPLAY   *P10:12,"LO Record Number: ",N9

		move	N9,str9
		call	Trim using str9
.
		pack	str15,"A",str9
		getprop sheet.range(str15),*Value=str12
		call	Trim str12
		if (str12 = "")	.Assumed last record!!
			DISPLAY   *P10:13,"New LO Record: ",OWNERID
.
			setprop sheet.range(str15),*Value=OWNERID
.
			pack	str15,"D",str9
			setprop sheet.range(str15),*Value=NOWNFLD
.
			pack	str15,"E",str9
			setprop sheet.range(str15),*Value=OWNOCPY
.
			pack	str15,"G",str9
			setprop sheet.range(str15),*Value="NEW"
.
			return
		elseif (str12 = OWNERID)
			DISPLAY   *P10:13,"Updating LO:   ",OWNERID
			pack	str15,"D",str9
			setprop sheet.range(str15),*Value=NOWNFLD
.
			pack	str15,"G",str9
			setprop sheet.range(str15),*Value="UPDATED"
			return
		endif
.
		add	C1,N9
	repeat
	RETURN
	
DREWTEST
	MOVE	STR1,STR1
	RETURN
.
ObjectErr
.	ex.quit
	trap ObjectErr if Object
	goto SkipWrite
	return
.
	include nownio.inc
	include	gnxtio.inc
	include comlogic.inc