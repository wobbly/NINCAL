Pc	Equ	0
	Include	Common.inc
	INclude	Cons.inc	
	include	MDCmaindd.inc
	include	MDcSegdd.inc
	Include	MDCMSCDD.inc	
	include	mdc035dd.inc
	include	mdc090dd.inc
	include	mdctxtdd.inc
	include	mdc060dd.inc
	Include	M2Ndd.inc
	Include	M2NLodd.inc
	Include	ndatdd.inc
	Include	Nowndd.inc
	Include	NTXTDD.inc
	Include	NADDDD.inc
	include	NArrdd.inc                .arrangement codes  mmm anything to do here??? I think Defunct DH
	Include	NSrcDD.inc                 
	Include	NModDD.inc                 .not touched yet
	INclude	Xls.inc
	INclude	NMDCMscDD.inc - additional info (currently from MIN) need to incorporate
	INClude	NMDCCATDD.inc - Min Category
          include         NSELdd.inc
          Include	NSltdd.inc
          include	Nrefdd.inc
	Include	Npasdd.inc
	INclude	Gnxtdd.inc
. backup files
	Include	ndatBdd.inc
	Include	nTxtBdd.inc
	Include	nAddBdd.inc
	include	NArrBdd.inc
	include	NSltBdd.inc
	include	NSrcBdd.inc
	Include	NModBDD.inc                 .not touched yet
.......................................................
.
Release	INit	"1.2"	DLH           Add more search options
REldate	Init	"18 March 2008"
.Release	INit	"PRE"	DLH
.REldate	Init	"11 October 2006"
hold2         dim             4500           .length of largest possible text record
STR46	Dim	46
OverFLow	Dim 	1
Count	Form	6
B50	Init	"                                              "
B45	Init	"                                         "
BaseFlag	Dim	1
SlctFlag	Dim	1
crnl       init      0177           .causes line feed cr in edittext
Carr    	init    	0x7f		.edittext carraige return line feed
Cyan	Color
colordim dim	8
arcode	dim	3
newOwn   form    4
...................................................................................
CaseChange external "SPELLCHECK;CaseChange"
...................................................................................

.THis program allows you to match Min 2 NIN records and update list information from Converted MIN List system CSV format live files
.First release kicks out "printed" reports only no update.
.
.open and read mdc_main.csv file read Min2NIN xref and  to MDC_Main.dat and 
.supplemental files so they can be cross referenced and applied.
HMList	Dim	6	Hold Min List #

ManuaLUpdate external "M2N0002;ManualUpdate"

.x             plform          NDAT0001
.NDAT001E 	plform               NDAT001E
mss1    	plform  Error
pass        plform          Passwrd
X 	plform               NDAT001E
        	formload 	X
            formload pass

.
ListEdit	EditTExt
	Create	Cyan=*Cyan
	Create	ListEdit=3:4:2:77,BORDER:
 	 	STYLE=3DON
	Activate	LIstEdit 	 	
	Setprop	ListEdit,Visible=0

.Min
        	ndat1eListView001.InsertColumn using "Key",50,1
        	ndat1eListView001.InsertColumn using "Name",400,2
        	ndat1eListView001.InsertColumn using "NIN ##",50,3
	ndat1eListView001.InsertColumnFgClr using *Index=4

.NIN
        	ndat1eListView002.InsertColumn using "Key",50,1
        	ndat1eListView002.InsertColumn using "Name",400,2
        	ndat1eListView002.InsertColumn using "MIN ##",50,3
	ndat1eListView002.InsertColumnFgClr using *Index=4
.min owner search	
	ndat1eListView003.InsertCOlumn Using "Key",50,1
	ndat1eListView003.InsertCOlumn Using "Company",200,2
	ndat1eListView003.InsertCOlumn Using "Contact",200,3
.Nin owner search	

	ndat1eListView004.InsertCOlumn Using "Key",50,1
	ndat1eListView004.InsertCOlumn Using "Company",200,2
	ndat1eListView004.InsertCOlumn Using "Contact",200,3

	Move	No,Passflag
	SetFocus	ndat1eEditText003
.
              loop
               waitevent
.                setitem timer,0,18000   .reset to 30 minutes
              repeat
LoadMinListView
              ndat1eListView001.InsertItem giving N9 using DDCNOA
              ndat1eListView001.SetItemText using N9,DTTL,1
              ndat1eListView001.SetItemText using N9,M2NNIN,2
              call	Trim using M2NNIN    
.	call	debug
	if	(M2NNIN <> "")

	move	"0xFF0000",colordim		.Red
	ndat1eListView001.SetItemText USING N9,colordim,4
	endif
	ndat1eListView001.Getitemcount giving Count
	Clear	str25
	pack	Str25,Count," - Found."
	Setitem	ndat1eStatText039,0,str25
 	REturn
UpdMinListView	
.first find it
	call	debug
.	ndat1eListView001.Finditem Giving N9 *Param=DDcnoa
	ndat1eListView001.Finditem Giving N9 Using *Start=seq,*Text=DDcnoa
	IF 	(n9 <> -1)
               ndat1eListView001.SetItemText using N9,M2NNIN,2
.begin patch 1.2               
	Else
	call	LoadMinListView
.end patch 1.2               
	endif
	return	

LoadNINListView
              ndat1eListView002.InsertItem giving N9 using Lstnum
              ndat1eListView002.SetItemText using N9,MLstName,1
              ndat1eListView002.SetItemText using N9,M2nMin,2
               call	Trim using M2NMIn
	if	(M2NMIN <> "")
	move	"0xFF0000",colordim		.Red
	ndat1eListView002.SetItemText USING N9,colordim,4
	endif
	ndat1eListView002.Getitemcount giving Count
	Clear	str25
	pack	Str25,Count," - Found."
	Setitem	ndat1eStatText040,0,str25
	
 	REturn
UpdNinListView	
.first find it
	call	debug
	ndat1eListView002.Finditem Giving N9 Using *Start=seq,*Text=LSTnum
	IF 	(n9 <> -1)
               ndat1eListView002.SetItemText using N9,M2NMIN,2
	Else
	call	LoadNinListView
	endif
	return	
 	
LoadMinDetail

.          ndat1eListView001.GetItemText giving STR6 using REsult *Subitem=c2
          ndat1eListView001.GetItemText giving STR6 using REsult
          packkey	Minfld,Str6
          call	Minkey
          packkey	M090fld,"00",Dlow
          call	M090key
          setitem	ndat1eEditText003,0,DTTL
          setitem	ndat1eStatText002,0,DDCNoa
          Setitem	ndat1eStatText016,0,UNIV
          Setitem	ndat1eStatText003,0,Dlow
          unpack    DDTR8,str2,YY,MM,DD
          clear	str10
	pack	Str10,MM,"/",dd,"/",str2,yy	
	setitem	NDat1EStatTExt020,0,str10
	
	call	LoadMinText
	call	Trim using Dlow
	Count	n2,Dlow
	if	(n2 = 1)
	pack	str5,"0000",dlow
	Elseif	(n2 = 2)
	pack	str5,"000",dlow
	Elseif	(n2 = 3)
	pack	str5,"00",dlow
	Elseif	(n2 = 4)
	pack	str5,"0",dlow
	Else
	pack	str5,Dlow
	endif
	Move	Str5,Dlow
	
	pack	Gkey,Dlow
	rep	Zfill,Gkey
	call	LoadMInLoDetail
.lets check for x-ref
	packkey	m2nfld,ddcnoa
	call	m2nkey
	if 	not over
	move	M2NNIN,Lstnum
	call	loadNInDetail
	else
.	call	ClearNInDetail
	endif
	REturn
LoadMInLODetail
	Packkey	M090fld,Gkey
.	ndat1eListView003.DeleteAllItems
	call	M090Key
	If	Not over
	Setitem	ndat1eStatText003,0,gKey
	SetItem	ndat1eStatText008,0,Gnam
	SetItem 	ndat1eStatText013,0,GCNT
	SetItem	ndat1eStatText009,0,GAD1
	SetItem	ndat1eStatText026,0,GCTY
	SetItem	ndat1eStatText027,0,GSTT
	SetItem	ndat1eStatText028,0,GZip
	Setitem 	ndat1eStatText003,0,Gkey
	clear	str13
	UNpack	GPH1,ARcode,str3,str5
	pack	str13,"(",Arcode,")",str3,dash,str5
	Setitem	ndat1eStatText035,0,str13
	clear	str13
	UNpack	GPHF,ARcode,str3,str5
	pack	str13,"(",Arcode,")",str3,dash,str5
	Setitem	ndat1eStatText036,0,str13
.	SetItem	ndat1eStatText030,0,str10
.lets check for x-ref
	packkey	m2nLOfld,c0,Gkey
	move	c1,m2nLopath
	call	m2nLokey
		if 	not over
		Unpack	M2NLoNIN,str2,str4
		Setitem	ndat1eStatText024,0,str4
		else
		Setitem	ndat1eStatText024,0," "
		endif
	Else
	Setitem	ndat1eStatText024,0," "
	SetItem	ndat1eStatText008,0," "
	SetItem	ndat1eStatText009,0," "
	SetItem	ndat1eStatText013,0," "
	SetItem	ndat1eStatText026,0," "
	SetItem	ndat1eStatText027,0," "
	SetItem	ndat1eStatText028,0," "
	SetItem	ndat1eStatText035,0," "
	SetItem	ndat1eStatText036,0," "
	Setitem 	ndat1eStatText003,0," "
	clear	GNam
	endif
	return

LoadMinText
	clear	Hold2
	Setitem	ndat1eEditText001,0,""

	packkey	MtxtFld2,DDCNOa,"   1"
	move	c2,Mtxtpath
	call	MTxtKey
	If	over
.error
	endif
	Match	TxtLin,b50                                  .Blank Line
		IF	Equal
		Clear	str46
		append	Carr,Hold2
		goto 	Endparse
		endif

	call	Trim Using TXTLIN
		count	n2,TXTLIN
		if	(n2 >= "46")
		reset	Txtlin
		Append	txtlin,Hold2
		Bump	Hold2,c1
		goto	endparse
		endif

	endset	TxtLin
	Cmatch 	".",TxtLin
		IF	equal
		reset	TxtLin
		Clear	Str46
		append    txtlin,str46
		append	Carr,str46
		Reset     str46
		append	str46,hold2
		goto	endparse
		endif

	Reset	TxtLin
	endset	TxtLin
	Cmatch 	":",TxtLin
		IF	equal
		reset	TxtLin
		append    txtlin,str46
		append	Carr,str46
		Reset     str46
		append	str46,hold2
		goto	endparse
		endif
		
	reset	Txtlin
	Append	txtlin,Hold2
	append	b1,hold2
	goto 	Endparse

	
	Loop
	call	MTxtKS
	Until	over
	IF	(DDCNoa = TXDCNO)	 .same list?????

		IF	(TxtTyp = "D")

	IF	(lstnum = "020496")
.	call	debug
	endif
	
		Match	TxtLin,b50                                  .Blank Line
			IF	Equal
			Clear	str46
			append	Carr,Hold2
			goto 	Endparse
			endif


		call	Trim Using TXTLIN
			count	n2,TXTLIN
			if	(n2 >= "46")
			reset	Txtlin
			Append	txtlin,Hold2
			append	b1,Hold2
			goto	endparse
			endif

		endset	TxtLin
		Cmatch 	".",TxtLin
			IF	equal
			reset	TxtLin
			Clear	Str46
			append    txtlin,str46
			append	Carr,str46
			Reset     str46
			append	str46,hold2
			goto	endparse
			endif

		Reset	TxtLin
		endset	TxtLin
		Cmatch 	":",TxtLin
			IF	equal
			reset	TxtLin
			append    txtlin,str46
			append	Carr,str46
			Reset     str46
			append	str46,hold2
			goto	endparse
			endif
		
		reset	Txtlin
		Append	txtlin,Hold2
		append	b1,hold2
		goto 	Endparse
	ElseIF	(TxtTyp = "O")
		Match	TxtLin,b50                                  .Blank Line
			IF	Equal
			append	CARR,hold2
			goto 	Endparse
			endif
		call	Trim using TxtLin
		if	(lstnum = "020496")
.		call	debug
		endif
		Endset 	TxtLin
		cmatch    ":",TxtLin
			if	equal
			reset	Txtlin
			Append	txtlin,Hold2
			append	CARR,Hold2
			goto	endparse
			endif
		Reset	TxtLin
		Endset 	TxtLin
		cmatch    ".",TxtLin
			if	equal
			reset	Txtlin
			Append	txtlin,Hold2
			append	CARR,HOld2
			goto	endparse
			endif
		Reset	TxtLin
		Count	n2,TxtLin
		if	(n2 <= "45")
			append	TxtLin,Hold2
			append	Carr,Hold2
			goto	endparse
.		Elseif	(N2 = "46")              .last change
.			append	TxtLin,Hold2
.			goto	endparse
		else
			append	txtlin,hold2
			Append	b1,hold2
			goto	endparse
		endif			
		endif
	
		
endparse		
		Clear	Str46
		Clear	Str50

	Else
	Break
	endif
	repeat
	Reset	Hold2
	Setitem	ndat1eEditText001,0,hold2
	return
	
LoadNINDetail
          packkey   Ndatfld,Lstnum
          call	NdatKey
	call	Trim using MLstname
	Unpack	Ownnum,str2,str4
	Call	LoadNINLODetail
          SetItem	ndat1eStatText018,0,Universe
          Setitem	ndat1eEditText004,0,MLstname
          Setitem	ndat1eStatText005,0,Lstnum
	clear	str2
	clear	yy
	clear	dd
	clear	mm
	clear	str10
          unpack    REVDATE,str2,YY,MM,DD
	pack	Str10,MM,"/",dd,"/",str2,yy	
	setitem	NDat1EStatTExt012,0,str10
          setitem         ndat1eEditText002,0,""         .Initialize object
          clear           NTXTTEXT
          clear           hold2
		call	debug
              for N2,C1,"15"
               pack           NTXTFLD,LSTNUM,N2
               rep		Zfill,NTxtFLd
               move           "D.Load-NTXTKEY",Location
               pack           KeyLocation,"Key: ",NTXTFLD
               call           NTXTKEY
               until over
               append         NTXTTEXT,hold2
              repeat
              if (hold2 <> "")
               reset          hold2
               call           Trim using hold2
              endif
              setitem         ndat1eEditText002,0,hold2
         
	Return
LoadNINLoDetail
	packkey 	Nownfld,str4
.	ndat1eListView004.DeleteAllItems
	Setitem	ndat1eStatText029,0," "
	call	Nownkey
	if 	Over
	Setitem	ndat1eStatText006,0," "
	Setitem	ndat1eStatText007,0," "
        	SetItem	ndat1eStatText014,0," "
          	SetItem	ndat1eStatText015,0," "
	SetItem	ndat1eStatText031,0," "
	SetItem	ndat1eStatText032,0," "
	SetItem	ndat1eStatText033,0," "
	Setitem	ndat1eStatText029,0," "
	Setitem	ndat1eStatText037,0," "
	Setitem	ndat1eStatText038,0," "
	Setitem	ndat1eStatText030,0," "
	Else
	Setitem	ndat1eStatText006,0,OWNLON
	Setitem	ndat1eStatText007,0,OWNLONM
        	SetItem	ndat1eStatText014,0,OWNOCPY
          	SetItem	ndat1eStatText015,0,OWNLOSA 
	SetItem	ndat1eStatText031,0,OWNLOCTY
	SetItem	ndat1eStatText032,0,OWNLOS 
	SetItem	ndat1eStatText033,0,OWNLOZC 
	unpack	OWNRDTE,mm,dd,str4
	pack	str10,mm,"/",dd,"/",str4
	clear	str13
	UNpack	OWNTELE,ARcode,str3,str5
	pack	str13,"(",Arcode,")",str3,dash,str5
	Setitem	ndat1eStatText037,0,str13
	clear	str13
	UNpack	OWNfax,ARcode,str3,str5
	pack	str13,"(",Arcode,")",str3,dash,str5
	Setitem	ndat1eStatText038,0,str13
	
	
.lets check for x-ref
	packkey	m2nLofld2,c0,c0,ownlon
	move	c2,m2nLopath
	call	m2nLokey
		if 	not over
			Setitem	ndat1eStatText029,0,M2nMin
		endif
	
	endif
	return
	
ClearNINDetail
          SetItem	ndat1eStatText014,0,""
          SetItem	ndat1eStatText015,0,""
          SetItem	ndat1eStatText018,0,""
          Setitem	ndat1eEditText004,0,""
          Setitem	ndat1eStatText005,0,""
          Setitem	ndat1eStatText012,0,""
          
	Return
.........................................................................	
.Create New NIN LO
CreateNINLO
        	clock   	date to str8
        	unpack  	str8,MM,STR1,DD,STR1,YY
        	pack    	OwnRDTE,MM,DD,CC,YY
	Move	"M",OwnLOc
	Clear	Ownblk
	MOve	GNAM,OwnoCpy	.Name
	MOve	GAD1,ownlosa       .Address 1
	MOve	GCTY,ownlocty        .City
	MOve	GSTT,ownlos
	MOve	GZIP,Ownlozc
	MOve	GCNT,OwnLonm        .Contact
	move	c0,ownnec

	MOve	GPH1,OwnTele        .Phone
	MOve	GPHF,OwnFax        .FAX
	if	(GDLT = "I")
	MOve	GDLT,Ownstat
	else	
	Clear	OWnstat
	endif
	Clear	Ownctn
	Clear	OwnGally
	Clear	OwnTaxid
	Clear	OwnFax2
	Clear	OwnTranFlag
	Clear	OwnEmail
	Move	Npasuser,OWNPASS
	call 	GetNewLo

	Call	NownWrt
	
	Return
GetNewLo
.Locate and Display next logical Owner Number
        move    "NOWNNXT",GNXTFLD
        call    GNXTKEY
        if over
.Change StatText Boxes For Error Message
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
                setitem ErrorMssgStat5,0,"  Error Getting Owner ID Number!"
                setitem ErrorMssgOK,0,"&Stop"
.Display Error Message
                setprop ErrorMssg,visible=1
.Reset StatText Boxes
                call    SetM2N3ErrorMssgDefault
.Terminate Subroutine
                return
        endif

        bump    GNXTNUM,2
        move    GNXTNUM,newOwn
        loop
                add     C1,newOwn
                move    newOwn,ownlon
                rep     zfill,ownlon
                pack    NOwnFLD,ownlon,"0000"
                rep     zfill,NOwnFLD
                call    NOwnTST
                until over
        repeat

	move	newOwn,str4
	rep	zfill,str4
	clear	GNXTNUM	
	append	B2,GNXTNUM
	append	newOwn,GNXTNUM
	reset	GNXTNUM
	rep	zfill,GNXTNUM
	call	GNXTUPD
        
        return
.........................................................................	
UpdateNINLO
.not yet implemented
	Return
.........................................................................	
SetM2N3ErrorMssgDefault
.Set Default 
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=1
        setprop ErrorMssgStat4,visible=1
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0,"To Search By :"
        setitem ErrorMssgStat2,0,"Enter "
        setitem ErrorMssgStat3,0,"To Search By Company Name:"
        setitem ErrorMssgStat4,0,"Enter Search Name"
        setitem ErrorMssgStat5,0,"      That Record Does Not Exist!"
        setitem ErrorMssgOK,0,"&OK"
        return
.........................................................................	
EOJ
	Stop

	Include	M2NIO.inc
	Include	M2NLoIO.inc
	INclude	MdcMainIO.inc
	Include	MDCSegIO.inc
	Include	MDCMSCIO.inc
	Include	MDC035IO.inc
	Include	MDC090IO.inc
	Include	MDCTXTIO.inc
	Include	MDC060IO.inc
	Include	Ndatio.inc
	Include	Nownio.inc
	Include	NTXTIO.inc
	Include	NADDIO.inc
	include	NArrIO.inc 
	include   NsrcIO.inc
	Include	NModio.inc                 .not touched yet
	INclude	NMDCMscIO.inc - additional info (currently from MIN) need to incorporate  backup ?
	INClude	NMDCCATIO.inc - Min Category                                              backup?  
          include         NSELIO.inc
          Include	NSltio.inc
          include	Nrefio.inc
	Include	Npasio.inc
	INclude	GnxtIO.inc
. backup files
	Include	ndatBIO.inc
	Include	nTxtBIO.inc
	include	NAddBIo.inc
	include	NArrBIO.inc
	include	NSltBIO.inc
	include   NSrcBIO.inc
	Include	NModBIO.inc                 .not touched yet
.......................................................

	INclude	Comlogic.inc