pc       equ       0
         include   common.inc
         include   cons.inc
         include   nwfdd.inc
         include   nwfXdd.inc
;patch2.4
			include	compdd.inc
			include	cntdd.inc
.         include   nmlrdd.inc
;patch2.4
         include   statsdd.inc
         include   norddd.inc
          include  hp.inc
.begin patch 1.3
           INCLUDE    NDATDD.inc
           INCLUDE    NXRFDD.inc
           INCLUDE    NXCHDD.inc
           INCLUDE    NXNGDD.inc
.end patch 1.3
.begin patch 1.4
.        include     tncxdd.inc
        include     SXrfdd.inc
.end patch 1.4
.begin patch 2.3
               include        SlctClndd.inc
.end patch 2.3
.
Release   init     "2.4.2"      14MAR2005 ASH Exchange File Conversion
.Release   init     "2.4.1"      27JAN2005 ASH NINSTATS Conversion
.Release   init     "2.4"      26MAY2004 DMB Mailer Conversion
.Release   init     "2.3"       07Feb02  DLH add "selection cleanup" at output
.Release   init     "2.2"    19NOV01 ASH CONVERTED STATSFILE
.Release   init     "2.1"    10Apr01 DLH use SXRFDD & IO instead of TNCdd & IO
.Release   init     "2.0"    30Jun00 DLH change from flat file dump to create spreadsheet
.release   init     "1.5"    27Jun00 DLH starting Maildate for pickoff option.
.release   init     "1.4"    4Apr00 DLH add track logic for TNC
.release   init     "1.3"    24Feb00 DLH add exstat and datacard last update fields
.release   init     "1.2"    20Jan00 DLH add formulas for projections
.five new variables written to output file   leave vars empty for NWF
.  1    weeks out    .col V
.  2    projected responses formula=IF("weeksout"<=3,G4/0.5,IF("weeksout"=4,G4*0.65,IF("weeksout"<7,G4*0.8,IF("weeksout"<11,G4*0.9,G4))))     .col W
.  3    projected revenue formula = projected responses(W) * aver gift(M)                                                                     .col X
.  4    projected response rate  = projecte responses / qty mailed                                                                            .col Y
.  5    percent of responses in  =IF("weeksout"<=3,0.5,IF("weeksout"=4,0.65,IF("weeksout"<7,0.8,IF("weeksout"<11,0.9,1))))                    .Col Z
.
..The Dawson rule
..
..Weeks out	   percent
..11+			100
..7-11		      90-95
..5-6			80
..4			65
..3			50

.
.
.release   init     "1.1"    11Aug99 DLH prompt for client
.release   init     "1.02"   08Oct98 ASH NINMLR,NINRTN,NINPAY Y2K Recompile
.release   init     "1.01"   08Jul98 DLH dump package info
.Release   init     "1.00"   Sep597 DLH primary key revision
..............................................................................
.common delimited output file
STATS    file
.
RECSIN   FORM      6
RECSout FORM      6
formula1 dim       25
formula2 dim       35
formula3 dim       60               .total cost       .15June98 DLH increase size
formula4 dim       30               .revenue/loss
formula5 dim       35
formula6 dim       120               .projected responses
formula7 dim       100               .projected revenue
formula8 dim       80               .projected response rate
formula9 dim       100               .Estimated responses in
formula10 dim      100               .Estimated Total Revenue/(Cost) in

STR256   DIM       300
.str13    dim       13
DECIMAL  FORM      8.6
PERCENT  INIT      "%"
cntrbflg FORM      "0"
track    dim        8
offer    dim         3
minusflg dim        1
mqty     dim       11
dim30    dim       30
mqtyb    dim       10
z96      dim        6
mmyy     dim        4
Akey1    dim        3
.Start Patch #1.02 - duplicate var declared in cons.inc
.dquote  init        "#""
.end Patch #1.02 - duplicate var declared in cons.inc
rpara    init       ")"
LPara    init       "("
RESP     dim        8
Rresp     dim        8
NCI      dim        8
pci      dim        8
CI       dim        8
revenue  dim        9
gift     dim        5
lstcpm   dim        10
tlst$     dim        12
pckcpm    dim        6
tpck      dim        12
totcpm    dim        9
mailcost  dim        10
unitpc    dim        8
totpc     dim        9
totcst    dim        15
totcst13  dim        13
totlcost  dim        11
netrev8   dim        8
netrev    dim        13
nrpci     dim        10
nrnci     dim        10
nrpcia    dim        9          was 8
nrncia    dim        9          was 8  dlh 03jun97
CstA9     dim        9
CstA      dim        10
pcaci     dim        10
Ncaci     dim        10
CTA       dim        31
cost$     dim        6
nqty      dim        12
inv       dim        10
lcpm      dim        7
.type      dim        1
NAMresp   dim        10
NAMrev    dim        10
IAMresp   dim        10
IaMrev    dim        10
Assocresp dim        9
assocrev  dim        9
assocrresp dim       8
basresp   dim        9
BRResp    dim        8
BREv      dim        9
TAresp    dim        9
inmailcpm dim        6
aggrindx  dim        4
premcost  dim        6
oldflag   form       1
page      form       4
lines     form       2
.weeksout  dim        5
.START PATCH 2.4.1 REPLACED LOGIC
.clientin  dim        4
clientin  dim        6
.END PATCH 2.4.1 REPLACED LOGIC
path      init       "\\nins1\e\data\statflat"
.path      init       "c:\data\statflat"
extension init       ".dat"
CountOut  form       5
TOTALROW  FORM       5
.begin patch 1.3
exbal    form      10
usage2c   dim       10
XFLAG    FORM          1
.end patch 1.3
.begin patch 1.5
startdate  form       8
datechk    form       8
datesw     dim        1         y=alldates n=start date
LODATE   FORM      5         julian low order date   (yyjjj)
HIDATE   FORM      5         julian high order date  (yyjjj)
daterng  dim       1
.end patch 1.5
.begin patch 2.0
.ComboPtr ComboBox ^
.WindPtr window  ^
.StrPtr  dim     ^
.In order to use any of the properties/methods associated with all parent objects
.of the Worksheet, I need to create automation objects for each of them.
.
.Look at Excel Object Model to understand heirarchy.  This can be found in hard
.documentation:  Microsoft Office 2000 Object Model Guide (found in MS Office 2000 Developers Edition).
.Software available via PL/B Designer - create a Container object on a form, create an Excel
.Spreadsheet, right click on Container object and Browse object.  This will invoke the PL/B Object
.Browser, which will give you SOME of the components of the Object Model.  To browse the Object
.Model in its entirety, open Excel.  Under Tools menu select Macro, select Visual Basic Editor.
.In the Visual Basic Editor screen, under the View menu, select Object Browser.  There you can
.view all of the objects/methods/properties in Excel.  Right clicking on an item will give you
.option to locate Help topics to see specifics.
.
.General heirarchy:
. Excel Application
.       Workbooks Collection (all open Workbooks)
.               Single Workbook
.                       Worksheets Collection (all Worksheets in this Workbook)
.                               Single Worksheet
.                                       SortColumn (a Single Column in that Worksheet used for sorting)
.
books    automation
book     automation
sheets   automation
sheet    automation
sortcol  automation
sortcol2 automation
sortcol3 automation
sortcol4 automation
ex       automation      class="Excel.Application"
.Variables needed by OrderLoadXSTAT
EFLAG   dim     1
AKey1A  init    "01L"
AKey2A  init    "02L"
Carr    init    0x7f
.
.Variant objects used to talk to outside applications
.See PL/B help in order to understand use of Variant objects.
.
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignLeft integer 4,"0xffffefdd"
.end patch   2.0
        CLOCK       DATE TO TODAY
.        PREPARE     stats,"\\nins1\e\data\STATFLAt.DAT"
        move       "stat0003" to program
        move       "Names in the News" to compnme
        MOVE       "DUMP STATS TO FLAT" TO STITLE
        call       paint
        move       c1 to statpath
        MOVE       C3 TO STATLOCK               .NO LOCKING
.        MOVE       C3 TO NMLRLOCK               .NO LOCKING
        MOVE       C3 TO NDATLOCK               .NO LOCKING
        MOVE       C3 TO NORDLOCK               .NO LOCKING
checkit
.START PATCH 2.4.1 REPLACED LOGIC
.        keyin      *p10:12,*el,"mailer ",*zf,*jr,str4
.        scan       star in str4
.        stop       if equal
.        pack       mkey from  str4,z3
.        move       c1 to nmlrpath
.        call       nmlrkey
.        keyin      *p10:14,*dv,mcomp," ok ? ",str1
.        cmatch     yes to str1
.        goto       checkit if not equal
.        move       str4 to clientin
.        rep        zfill in str4
.        clear       str55
.        pack       str55 from path,str4,extension
............................
        keyin      *p10:12,*el,"mailer ",*zf,*jr,str6
        scan       star in str6
        stop       if equal
	pack	COMPFLD,str6
	move	"checkit-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
        keyin      *p10:14,*dv,COMPcomp," ok ? ",str1
        cmatch     yes to str1
        goto       checkit if not equal
        move       str6 to clientin
        rep        zfill in str6
        clear       str55
        pack       str55 from path,str6,extension
.END PATCH 2.4.1 REPLACED LOGIC
.begin  patch  2.0
.        PREPARE     stats,str55,exclusive
.end  patch  2.0
        trap        format giving error if format
.begin patch 1.5
dates
        move        yes to datesw
        move        no to daterng
        keyin      *p10:14,*el,"All dates ? (Y) ",*rv,*t20,datesw
        cmatch     yes to datesw
        goto       Main if equal
.        keyin      *p10:16,*el,"Enter starting Mail date mmddccyy",*rv,*edit,str8
.dateok  keyin      *p60:16,"ok? ",str1
.        if          (str1 = yes)
.        unpack      str8 into mm,dd,cc,yy
.        pack        str8 from cc,yy,mm,dd
.        move        str8 to startdate
.begin patch 2.0
.        goto        looper
         keyin    *p1:15,*el,"beginning date ",*jr,*zf,mm,"/",*jr,*zf,dd,"/",*jr,*zf,yy
         call     cvtjul
         move     juldays to lodate
         keyin    *p1:16,*el,"ending date    ",*jr,*zf,mm,"/",*jr,*zf,dd,"/",*jr,*zf,yy
         call     cvtjul
         move     juldays to hidate
         move     yes to daterng
dateok  keyin      *p60:16,"ok? ",str1
        if          (str1 = yes)
        goto        main
.end patch 2.0
        endif
        goto        dates

..............................................................................
.main lets prepare the spreadsheet
main
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0

.
.Open Excel application
        create  ex
.Reset Default of Worksheets found in a Workbook
        setprop ex,*SheetsInNewWorkbook=C1
.Create Workbooks collection
        getprop ex,*Workbooks=books
.Create/Add a single Workbook
        books.add
        books.item giving book using 1
.Create Worksheets collection
        getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
        sheets.item giving sheet using 1
.Clear any garbage that might be present - this is redundant since we have created a new Worksheet
        sheet.range("A1","CC3000").Clear
.All objects except for the SortColumn have been created.
.
.
.....Campaign Header.....
.Header Column 1
.We could create a Range automation object but do not have to.
.Instead we use the Range property to dynamically return a Range object each time we want
.to dump in a value, or set another property of a cell(s).
.START PATCH 2.4.1 REPLACED LOGIC
.        call    Trim using MCOMP
.        setprop sheet.range("B1","B1"),*Value="Statistical History Maildates:"
.        setprop sheet.range("B2","B2"),*Value=MCOMP
        call    Trim using COMPCOMP
        setprop sheet.range("B1","B1"),*Value="Statistical History Maildates:"
        setprop sheet.range("B2","B2"),*Value=COMPCOMP
.END PATCH 2.4.1 REPLACED LOGIC
        setprop sheet.range("B3","B3"),*Value="Last Update:"
        setprop sheet.range("Z4","Z4"),*Value="Estimated"
        setprop sheet.range("AA4","AA4"),*Value="Projected"
        setprop sheet.range("A5","A5"),*Value="Source"
        setprop sheet.range("B5","B5"),*Value="Key"
        setprop sheet.range("C5","C5"),*Value="List Name"
        setprop sheet.range("d5","d5"),*Value="Select"
        setprop sheet.range("e5","e5"),*Value="Package"
        setprop sheet.range("f5","f5"),*Value="Maildate"
        setprop sheet.range("g5","g5"),*Value="Track"
        setprop sheet.range("h5","h5"),*Value="List"
        setprop sheet.range("i5","i5"),*Value="Quantity"
        setprop sheet.range("j5","j5"),*Value="Response"
        setprop sheet.range("k5","k5"),*Value="##"
        setprop sheet.range("m5","m5"),*Value="Average"
        setprop sheet.range("n5","n5"),*Value="List"
        setprop sheet.range("o5","o5"),*Value="In Mail"
        setprop sheet.range("p5","p5"),*Value="Total"
        setprop sheet.range("q5","q5"),*Value="Net"
        setprop sheet.range("r5","r5"),*Value="Cost To"
        setprop sheet.range("s5","s5"),*Value="NINCA"
        setprop sheet.range("v5","v5"),*Value="Projected"
        setprop sheet.range("w5","w5"),*Value="Projected"
        setprop sheet.range("x5","x5"),*Value="Projected"
        setprop sheet.range("y5","y5"),*Value="Projected"
        setprop sheet.range("z5","z5"),*Value="% Response"
        setprop sheet.range("aa5","aa5"),*Value="Total"
        setprop sheet.range("h6","h6"),*Value="Type"
        setprop sheet.range("i6","i6"),*Value="Mailed"
        setprop sheet.range("j6","j6"),*Value="Rate"
        setprop sheet.range("k6","k6"),*Value="Responses"
        setprop sheet.range("l6","l6"),*Value="Revenue"
        setprop sheet.range("m6","m6"),*Value="Gift"
        setprop sheet.range("n6","n6"),*Value="CPM"
        setprop sheet.range("o6","o6"),*Value="Cost/M"
        setprop sheet.range("p6","p6"),*Value="Cost"
        setprop sheet.range("q6","q6"),*Value="Revenue/(Loss)"
        setprop sheet.range("r6","r6"),*Value="Acquire"
        setprop sheet.range("s6","s6"),*Value="LR"
        setprop sheet.range("u6","u6"),*Value="Pkg/m"
        setprop sheet.range("v6","v6"),*Value="Weeks Out"
        setprop sheet.range("w6","w6"),*Value="Responses"
        setprop sheet.range("x6","x6"),*Value="Revenue"
        setprop sheet.range("y6","y6"),*Value="% Resp"
        setprop sheet.range("z6","z6"),*Value="In"
        setprop sheet.range("aa6","aa6"),*Value="Revenue/(Loss)"
.Set up Header Formatting
        setprop sheet.range("A5","aa6").Font,*Bold="True"
        sheet.range("A5","aa6").BorderAround using *LineStyle=1,*Weight=2
.
.....Records.....
.Records for this report always start at row 8

.end patch 2.0
..............................................................................
.looper lets get recordds
looper  CALL       STATSEQ
        goto       eoj if over
.START PATCH 2.2 ADDED LOGIC
	call	Trim using STATSRCE
.END PATCH 2.2 ADDED LOGIC
        if         (statsrce = "AHOMQ010301981")
        call       debug
        endif

        display    *p10:10,"records in ",recsin,b1,statsrce,b1,statmlr;
        add        c1 to recsin
        match      clientin to statmlr
        goto       looper if not equal
        if         (daterng = YES)
        unpack      statmdate into mm,dd,cc,yy
        call       cvtjul
         COMPARE   LODATE TO JULdays              CHECK IF IN DTE RNG.
         GOTO      looper IF LESS              NO. TRY NEXT ORDER.
         COMPARE   JULdays TO HIDATE
         GOTO      looper IF LESS              NO. TRY NEXT ORDER.
         goto      gotone
        endif
gotone
        display    *p10:12,"records out ",recsout;
        add        c1 to recsout
..............................................................................
        CLEAR      TRACK
.START PATCH 2.4.1 REPLACED LOGIC
.        if         (statmlr = "0170")
        if         (statmlr = "000913")
.END PATCH 2.4.1 REPLACED LOGIC
        move       "Assoc" to track
        SCAN       "contri" IN STATPANEL
	        IF         EQUAL
	        MOVE       "Contrib" to track
	        endif
	        reset      statpanel
	        SCAN       "CONTRI" IN STATPANEL
	        IF         EQUAL
	        MOVE       "Contrib" to track
	        endif
	        reset      statpanel
	        SCAN       "CARD" IN STATPANEL
	        IF         EQUAL
	        MOVE       "Contrib" to track
	        endif
	        reset      statpanel
	        SCAN       "Card" IN STATPANEL
	        IF         EQUAL
	        MOVE       "Contrib" to track
	        endif
	        reset      statpanel
	        SCAN       "Seeds" IN STATPANEL
	        IF         EQUAL
	        MOVE       "Contrib" to track
	        endif
	        reset      statpanel
	        SCAN       "Wrapping" IN STATPANEL
	        IF         EQUAL
	        MOVE       "Contrib" to track
	        endif
	        reset      statpanel
	        SCAN       "CALENDAR" IN STATPANEL
	        IF         EQUAL
	        MOVE       "Calendar" to track
        	endif
        endif
.begin patch 1.4
.START PATCH 2.4.1 REPLACED LOGIC
.        if         (statmlr = "0173")
        if         (statmlr = "000619")
.END PATCH 2.4.1 REPLACED LOGIC
.read tnc list xreference by ninlist number and move market code to track
        clear      track
.        move       c3 to tncxpath          .read by ninlist #
.        move       statlist to tncxfld2
.        call       tncxkey
        move       c3 to Statxpath          .read by ninlist #
        move       statlist to Statxfld2
.        call       tncxkey
        call       Statxkey
	        if         not over
                match      statmlr to statXmlr
	                if         equal
		        move       Statxmrkt to track
                        else
Statxloop               call       statxks
                        if         not over
	                  if   (statmlr = statxmlr and statxfld2 = statxlist)
.		                      if         equal
.			        move       tncxmrkt to track
			        move       statxmrkt to track
                               	        endif
                   	    endif
                         endif
                endif
        endif
.end patch 1.4
.........................................................................................................................
        type       statlr
        if         equal
        move       statlr to nordfld
        move       c1 to nordpath
        call       nordkey
        if         not over
        clear      str4
        clear      str3
        unpack     oodnum into str4,str3
        move       c0 to n3
        move       str3 to n3
.START PATCH 2.4.1 REPLACED LOGIC
.        if         (statmlr = "0170")
        if         (statmlr = "000913")
.END PATCH 2.4.1 REPLACED LOGIC
        compare    "14" to n3
        if         equal
        MOVE       "Contrib" to track
        endif
        compare    "15" to n3
        if         equal
        MOVE       "Basic" to track
        endif
        compare    "16" to n3
        if         equal
        MOVE       "Assoc" to track
        endif
        compare    "20" to n3
        if         equal
        MOVE       "Contrib" to track
        endif
        endif
        endif
        endif
..............................................................................
        compare     c1 to recsout
        if          equal
.START PATCH 2.4.1 REPLACED LOGIC
.        pack        mkey from statmlr,b3
.        replace     zfill in mkey
.        move        c1 to nmlrpath
.        call        nmlrkey
....................
	pack	COMPFLD,statmlr
	move	"Statxloop-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
.END PATCH 2.4.1 REPLACED LOGIC
        clear       str1
..............................................................................
.        write       stats,seq;*CDFON,mcomp,str1,str1,b1,b1,b1,b1,b1,b1,b1,b1:
.                    b1,b1,b1,b1,b1,b1,b1,b1,b1,b1,b1
.        write       stats,seq;*cdfon,b1,b1,b1,b1,b1,b1,b1,b1,b1,b1:
.                    b1,b1,b1,b1,b1,b1,b1,b1,b1,b1,b1
.        write       stats,seq;*cdfon,"Statistical History Maildates:":
.                    str1,str1,b1,b1,b1,b1,b1,b1,b1,b1:
.                    b1,b1,b1,b1,b1,b1,b1,b1,b1,b1,b1
.        write       stats,seq;*cdfon,"Last Update:":
.                    str1,str1,b1,b1,b1,b1,b1,b1,b1,b1:
.                    b1,b1,b1,b1,b1,b1,b1,b1,b1,b1,b1
.        write       stats,seq;*cdfon,b1:
.                    b1,b1,b1,b1,b1,b1,b1,b1,b1,b1:
.                    b1,b1,b1,b1,b1,b1,b1,b1,b1,b1,b1,b1,b1,b1,"Estimated","Projected"
.        write       stats,seq;*cdfon,"Source","Key","List Name","Select","Package","Maildate","Track","List","Quantity","Response","##",b1,"Average","List","In Mail","Total","Net","Cost To","NINCA",b1,b1,"Projected","Projected","Projected","Projected","% Response","Total"
.        write       stats,seq;*cdfon,b1,b1,b1,b1,b1,b1,b1,"Type","Mailed","Rate","Responses","Revenue","Gift","CPM","Cost/M","Cost","Revenue/(Loss)","Acquire","LR",b1,"PKG/m","Weeks Out","Responses","Revenue","% Resp","In","Revenue/(Loss)"
        move        c8 to CountOut
        endif
        clear       str5
        move        CountOut to str5
...............................................................................
.hopefully temp code.
.recalc weeksout as some bad data is in the file
.note skip if either statmdate or statpdate is missing
        cmatch      b1 to statmdate
        goto        da if eos
        cmatch      b1 to statpdate
        goto        da if eos
        clear       mm
        clear       dd
        clear       yy
        unpack      statpdate to mm,dd,str2,yy
        type        yy
        goto       da if not equal
        call       cvtjul
        move       juldays to n6                 .date of last returns
        clear       mm
        clear       dd
        clear       yy
        unpack      statmdate to mm,dd,str2,yy
        type        yy
        goto       da if not equal
        call       cvtjul
        sub        juldays from n6           .days between
        divide     c7 into n6                .get weeks
        move       n6 to statwkso
        if         (n6 < c0 )                 .have not mailed yet
        move       c0 to statwkso
        endif

..............................................................................
.response rate      .col J
DA      cmatch      b1 to str5
        if          equal
        bump        str5 by 1
        goto        Da
        endif
        clear       formula1
        append      "=k",formula1
        append      str5 to formula1
        append      "/i",formula1
        append      str5,formula1
.begin patch  2.0
.        append      "*100",formula1
.end patch  2.0
        reset       formula1

..............................................................................
.average gift  .col M
        clear       formula2
        append      "=if(k",formula2
        append      str5 to formula2
        append      ">0,",formula2
        append      "l",formula2
        append      str5 to formula2
        append      "/k",formula2
        append      str5,formula2
        append      ",0)",formula2
        reset       formula2

..............................................................................
.Total cost              .Col P
        clear       formula3
.dlh 29Mar00
.        append      "=(n",formula3
.        append      str5 to formula3
.        append      "*i",formula3
.        append      str5,formula3
.        append      "/1000)+o",formula3
        append      "=O",formula3
.end change
        append      str5,formula3
        append      "*i",formula3
        append      str5,formula3
        append      "/1000",formula3
        reset       formula3

..............................................................................
.Net REvenue/Loss          .Col Q
        clear       formula4
        append      "=l",formula4
        append      str5 to formula4
.        append      "-O",formula4
.dlh 01072000  o looks wrong should be column P .  ???
        append      "-P",formula4
        append      str5,formula4
        reset       formula4

..............................................................................
.cost to acquire       .Col R
        clear       Formula5
        append      "=if(k",formula5
        append      str5 to formula5
        append      ">0,",formula5
         append      "q",formula5
        append      str5 to formula5
        append      "/k",formula5
        append      str5,formula5
        append      ",0)",formula5
        reset       formula5
..............................................................................
.Projected Responses
.("weeksout"<7,G4/0.8,IF("weeksout"<11,G4/0.9,G4))))     .col W
        clear       formula6
        append      "=if(V",formula6
        append      str5 to formula6
        append      "=3,",formula6
         append      "k",formula6
        append      str5 to formula6
        append      "/0.5,IF(V",formula6
        append      str5 to formula6
        append      "=4,",formula6
         append      "k",formula6
        append      str5 to formula6
        append      "/0.65,IF(V",formula6
        append      str5,formula6
        append      "<7,",formula6
        append      "k",formula6
        append      str5 to formula6
        append      "/0.8,IF(V",formula6
        append      str5,formula6
        append      "<11,",formula6
        append      "k",formula6
        append      str5 to formula6
        append      "/0.9,K",formula6
        append      str5 to formula6
	append     "))))",formula6
        reset       formula6
..............................................................................
. projected revenue formula = projected responses(W) * aver gift(M)                                                                     .col X
        clear       formula7
        append      "=w",formula7
        append      str5 to formula7
        append      "*M",formula7
        append      str5,formula7
        reset       formula7
..............................................................................
. projected response rate  = projecte responses / qty mailed                                                                            .col Y
        clear       formula8
        append      "=W",formula8
        append      str5 to formula8
        append      "/I",formula8
        append      str5,formula8
        reset       formula8
..............................................................................
.  percent of responses in  =IF("weeksout"<=3,0.5,IF("weeksout"=4,0.65,IF("weeksout"<7,0.8,IF("weeksout"<11,0.9,1))))                    .Col Z
        clear       formula9
        append      "=IF(V",formula9
        append      str5 to formula9
        append      "<=3,0.5,IF(V",formula9
        append      str5,formula9
        append      "=4,0.65,IF(V",formula9
        append      str5,formula9
        append      "<7,0.8,IF(V",formula9
        append      str5,formula9
        append      "<11,0.9,1))))",formula9
        reset       formula9
..............................................................................
.projected total revenue/loss
        clear       formula10
        append      "=X",formula10
        append      str5 to formula10
        append      "-P",formula10
        append      str5,formula10
        reset       formula10
..............................................................................

.begin patch 2.3
               packkey        SlctClnFld from o2des
               call           SlctClnKey
               if             not over
               move           SlctClnText to Statsel          .clean select
               endif
.end patch 2.3

        rep         ", " in statsel
        rep         ", " in statldes
        rep         ", " in statsrce
        rep         ", " in statkycd
        rep         zfill in statmdate
        clear       mm
        clear       dd
        clear       str4
        unpack      statmdate into mm,dd,str4
        clear       str10
        pack        str10 from mm,slash,dd,slash,str4
.begin patch1.3
        call        getexch
.end patch1.3
..............................................................................
.begin patch 2.0
LoadDetail
.Prep work
        move    countout,str9
        call    Trim using str9
.
        clear   str7
        pack    str7,"A",str9
        setprop sheet.range(str7),*Value=statsrce,*HorizontalAlignment=AlignLeft
.
        clear   str7
        pack    str7,"B",str9
        setprop sheet.range(str7,str7),*Value=statkycd
.
        clear   str7
        pack    str7,"C",str9
        setprop sheet.range(str7,str7),*Value=statLdes,*HorizontalAlignment=AlignLeft
.
        clear   str7
        pack    str7,"D",str9
        setprop sheet.range(str7,str7),*Value=statSel,*HorizontalAlignment=AlignLeft
.
        clear   str7
        pack    str7,"E",str9
        setprop sheet.range(str7,str7),*Value=statPanel,*HorizontalAlignment=AlignLeft
.MAILDATE
        clear   str7
        pack    str7,"F",str9
        setprop sheet.range(str7,str7),*Value=str10,*NumberFormat="mm/dd/yyyy"
.
        clear   str7
        pack    str7,"G",str9
        setprop sheet.range(str7,str7),*Value=Track,*HorizontalAlignment=AlignLeft
.
        clear   str7
        pack    str7,"H",str9
        setprop sheet.range(str7,str7),*Value=statType
.
        clear   str7
        pack    str7,"I",str9
        setprop sheet.range(str7,str7),*Value=statMqty,*NumberFormat="##,####0_);[Red](##,####0)"
.
        clear   str7
        pack    str7,"J",str9
        setprop sheet.range(str7,str7),*Value=formula1,*NumberFormat="###.####%"
.
        clear   str7
        pack    str7,"K",str9
        setprop sheet.range(str7,str7),*Value=statRESp,*NumberFormat="##,####0_);[Red](##,####0)"
.
        clear   str7
        pack    str7,"L",str9
        setprop sheet.range(str7,str7),*Value=statREv,*NumberFormat="$##,####0_);[Red]($##,####0)"
.FORMULA2 average gift  .col M
.
        clear   str7
        pack    str7,"M",str9
        setprop sheet.range(str7,str7),*Value=formula2,*NumberFormat="$##,####0.00_);[Red]($##,####0.00)"
.
        clear   str7
        pack    str7,"N",str9
        setprop sheet.range(str7,str7),*Value=statlcpm,*NumberFormat="$##,####0.00_);[Red]($##,####0.00)"
.
        clear   str7
        pack    str7,"O",str9
        setprop sheet.range(str7,str7),*Value=statImcst,*NumberFormat="$##,####0.00_);[Red]($##,####0.00)"
.
        clear   str7
        pack    str7,"P",str9
        setprop sheet.range(str7,str7),*Value=Formula3,*NumberFormat="$##,####0_);[Red]($##,####0)"
.
        clear   str7
        pack    str7,"Q",str9
        setprop sheet.range(str7,str7),*Value=Formula4,*NumberFormat="$##,####0_);[Red]($##,####0)"
.
        clear   str7
        pack    str7,"R",str9
        setprop sheet.range(str7,str7),*Value=Formula5,*NumberFormat="$##,####0.00_);[Red]($##,####0.00)"
.
        clear   str7
        pack    str7,"S",str9
        setprop sheet.range(str7,str7),*Value=Statlr
.
        clear   str7
        pack    str7,"T",str9
        setprop sheet.range(str7,str7),*Value=Statpack
.
        clear   str7
        pack    str7,"U",str9
        setprop sheet.range(str7,str7),*Value=Statpckm,*NumberFormat="$##,####0_);[Red]($##,####0)"
.
        clear   str7
        pack    str7,"V",str9
        setprop sheet.range(str7,str7),*Value=Statwkso
.
        clear   str7
        pack    str7,"W",str9
        setprop sheet.range(str7,str7),*Value=Formula6,*NumberFormat="##,####0_);[Red](##,####0)"
.FORMULA7  projected revenue formula = projected responses(W) * aver gift(M)                                                                     .col X
        clear   str7
        pack    str7,"X",str9
        setprop sheet.range(str7,str7),*Value=Formula7,*NumberFormat="$##,####0_);[Red]($##,####0)"
.formula8 projected response rate
        clear   str7
        pack    str7,"Y",str9
        setprop sheet.range(str7,str7),*Value=Formula8,*NumberFormat="###.####%"

.
        clear   str7
        pack    str7,"Z",str9
        setprop sheet.range(str7,str7),*Value=Formula9,*NumberFormat="###.####%"
.FORMULA10 .Estimated Total Revenue/(Cost) in
        clear   str7
        pack    str7,"AA",str9
        setprop sheet.range(str7,str7),*Value=Formula10,*NumberFormat="$##,####0_);[Red]($##,####0)"
.
        clear   str7
        pack    str7,"BB",str9
        setprop sheet.range(str7,str7),*Value=Exbal,*NumberFormat="##,####0"
.
        clear   str7
        pack    str7,"CC",str9
        setprop sheet.range(str7,str7),*Value=Revdate,*NumberFormat="mm/dd/yyyy"
.
        clear   str7
        pack    str7,"DD",str9
        setprop sheet.range(str7,str7),*Value=statlist

.        WRITE      STATS,SEQ;*CDFON,*LL,STATSRCE,STATKYCD,STATLDES,STATSEL,statpanel,str10:
.                    TRACK:
.                   stattype,statmqty,formula1,statresp,statrev,formula2,statlcpm,statImcsT:
.                   formula3,formula4,formula5,STATLR:
.                   statpack,statpckm,statwkso,formula6,formula7,formula8,formula9,formula10:
.                   exbal,REVDATE,statlist
.end patch 2.0
          add        c1 to CountOut
          goto       looper
......................................................................................................................
.begin patch 1.3
.get exchange
getexch
.begin patch 2.0 temporary
	   return
.end patch 2.0 temporary
           MOVE      statlist TO NDATFLD
           rep       zfill in ndatfld
           MOVE        C1 TO NDATPATH
           CALL        NDATKEY
           clear     nxrffld                    .dlh 04feb98
           MOVE      statlist TO NXRFFLD
           REP       ZFILL IN NXRFFLD
           MOVE        C1 TO NXRFPATH
           CLEAR     NXRFMLR
           CALL      NXRFKEY
                   if        over
                   move      c0 to exbal
                   return                        *nothing more to do get out    DLH 23Jul97.
                   endif
           MOVE      C1 TO XFLAG
           CLEAR     NXNGFLD1
           APPEND    "01X" TO  NXNGFLD1
.START PATCH 2.4.1 REPLACED LOGIC - TEMPORARY PATCH
.           APPEND    statMLR TO NXNGFLD1
.START PATCH 2.4.2 REPLACED LOGIC
.	pack	COMPFLD,statmlr
.	move	"getexch-COMPKEY",Location
.	pack	KeyLocation,"Key: ",COMPFLD
.	call	COMPKEY
.	APPEND	COMPOLDMLR,NXNGFLD1
.................
	APPEND	statMLR TO NXNGFLD1
.END PATCH 2.4.2 REPLACED LOGIC
.END PATCH 2.4.1 REPLACED LOGIC - TEMPORARY PATCH
           RESET     NXNGFLD1
           CLEAR     NXNGFLD2
           APPEND    "02X" TO  NXNGFLD2
           APPEND    NXRFMLR TO NXNGFLD2
           RESET     NXNGFLD2
           CALL      NXNGAIM
                     IF        OVER
                     MOVE        C2 TO XFLAG
                     CLEAR     NXNGFLD1
                     APPEND    "01X" TO  NXNGFLD1
                     APPEND    NXRFMLR TO NXNGFLD1
                     RESET     NXNGFLD1
                     CLEAR     NXNGFLD2
                     APPEND    "02X" TO  NXNGFLD2
.START PATCH 2.4.1 REPLACED LOGIC - TEMPORARY PATCH
.                     APPEND    statMLR TO NXNGFLD2
.START PATCH 2.4.2 REPLACED LOGIC
.                     APPEND    COMPOLDMLR TO NXNGFLD2
                     APPEND    statMLR TO NXNGFLD2
.END PATCH 2.4.2 REPLACED LOGIC
.END PATCH 2.4.1 REPLACED LOGIC - TEMPORARY PATCH
                     RESET     NXNGFLD2
                     CALL      NXNGAIM
                               IF        OVER
                               move      c0 to exbal
                               RETURN
                               ENDIF
                     ENDIF
           MOVE        C1 TO NXCHPATH
           PACK      NXCHFLD1 FROM ACCKEY,ENTRY
           REP       ZFILL IN NXCHFLD1
           CALL      NXCHKEY
           if        over                   lets double check. 21sep93.
           move      c0 to exbal
           return
           endif
.          DISPLAY   *P1:16,*EL,NXCHFLD1,B2,USAGE1,B1,USAGE2,*W5
           SUB       USAGE1 FROM USAGE2
           COMPARE   C1 TO XFLAG
                     IF        EQUAL
                     COMPARE   USAGE2 TO C0
                     GOTO        EVEN  IF EQUAL
                             IF        NOT LESS
                               move      usage2 to usage2c
                                rep       "- " in  usage2c
                               move      usage2 to exbal
                     ELSE
                               move      usage2 to usage2c
                                rep       "- " in  usage2c
                               move      usage2 to exbal
                               ENDIF
          ELSE
                     COMPARE USAGE2 TO C0
                     GOTO      EVEN IF EQUAL
                             IF        NOT LESS
                               MULT      SEQ BY USAGE2
                               move      usage2 to usage2c
                                rep       "- " in  usage2c
                               move      usage2 to exbal
                     ELSE
                               move      c0 to usage2c
                               move      usage2 to usage2c
                                rep       "- " in  usage2c
                               move      usage2 to exbal
                               mult      seq by exbal        .JD20apr98
                               ENDIF
           ENDIF
           RETURN
EVEN
         move      c0 to exbal

           RETURN
.end patch 1.3

.................................................................
eoj
.begin patch 2.0
.lets only sort if we have records
          if          (recsout > 0)
.         weof        STATS,seqeof
.         close       STATS
.Sort by List Name
        pack    str7,"DD",str9
.        trap    errortrap if object
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
        getprop sheet.range("C8"),*Columns(1)=sortcol
        getprop sheet.range("D8"),*Columns(1)=sortcol2
        getprop sheet.range("F8"),*Columns(1)=sortcol3
.        getprop sheet.range("G8"),*Columns(1)=sortcol4
.Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
.        sheet.range("A8",str7).sort using *Key1=sortcol,*Order1=1,*Key2=sortcol2,*Order2=1,*Key3=sortcol3,*Order3=1,*Key4=sortcol4,*Order4=1
        sheet.range("A8",str7).sort using *Key1=sortcol,*Order1=1,*Key2=sortcol2,*Order2=1,*Key3=sortcol3
.......................
.add in total goodies here
.GO DOWN TWO ROWS FROM LAST ENTRY
.
        MOVE    COUNTOUT TO N5
        ADD     C2 TO N5
        MOVE    N5 TO TOTALROW
        MOVE    TOTALROW TO STR8
        CALL    TRIM USING STR8
.NUMBER OF RECORDS OUTPUT
        clear   str7
        pack    str7,"A",str8
        CLEAR   STR35
        APPEND  "=COUNTA(A8:A",STR35
        APPEND  STR9 TO STR35
        append  ")" to str35
        RESET   STR35
        setprop sheet.range(str7),*Value=str35,*HorizontalAlignment=AlignLeft
        clear   str7
        pack    str7,"B",str8
        setprop sheet.range(str7),*Value="Records",*HorizontalAlignment=AlignLeft

.
.COLUMN i TOTAL mAILED QTY
        clear   str7
        pack    str7,"I",str8
        CLEAR   STR35
        APPEND  "=SUM(I8:I",STR35
        APPEND  STR9 TO STR35
        RESET   STR35
        setprop sheet.range(str7),*Value=str35,*NumberFormat="##,####0"
.
.overall repsponse rate
        clear   str7
        pack    str7,"J",str8
        CLEAR   STR35
        APPEND  "=SUM(K" to str35
        append  str8 to str35
        append  "/I" to str35
        append  str8 to str35
        append  ")" to str35
        RESET   STR35
        setprop sheet.range(str7),*Value=str35,*NumberFormat="##.####%"

.COLUMN i TOTAL responses QTY
        clear   str7
        pack    str7,"K",str8
        CLEAR   STR35
        APPEND  "=SUM(K8:K",STR35
        APPEND  STR9 TO STR35
        RESET   STR35
        setprop sheet.range(str7),*Value=str35,*NumberFormat="##,####0"
.
.COLUMN L TOTAL
        clear   str7
        pack    str7,"L",str8
        CLEAR   STR35
        APPEND  "=SUM(L8:L",STR35
        APPEND  STR9 TO STR35
        RESET   STR35
        setprop sheet.range(str7),*Value=str35,*NumberFormat="$##,####0_);[Red]($##,####0)"
.
.COLUMN M TOTAL overall average gift
        clear      str7
        pack       str7,"M",str8
        clear      str35
        append      "=if(k",str35
        append      str8 to str35
        append      ">0,",str35
        append      "l",str35
        append      str8 to str35
        append      "/k",str35
        append      str8,str35
        append      ",0)",str35
        reset       str35
        setprop sheet.range(str7,str7),*Value=str35,*NumberFormat="$##,####0.00_);[Red]($##,####0.00)"
.
.Set up totals Formatting
        clear   str7
        pack    str7 from "A",str8
        pack    str9 from "AA",str8
        setprop sheet.range(str7,str9).Font,*Bold="True"
        sheet.range(str7,str9).BorderAround using *LineStyle=1,*Weight=3
        sheet.range("A1",str9).Columns.AutoFit
        endif
.


.......................................

.
StatFileNameSelect
        clear   taskname
        append  "\\nins1\d\USERS",taskname
        call    Trim using USER
        if (USER <> "")
                append  "\",taskname
                append  USER,taskname
        endif
        append  "\",taskname
        append  "History",taskname
        reset   taskname
        setprop ex,*DefaultFilePath=taskname
        ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
.
        if (taskname <> "0")
                movelptr taskname,N9
                reset   taskname,N9
                append  "xls",taskname
                reset   taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                trap    TrapStatObject if Object
                book.saveas giving N9 using *Filename=taskname
.
                trapclr Object
        endif
StatCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
        destroy sortcol
        destroy sortcol2
        destroy sortcol3
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        setprop ex,*DisplayAlerts=OFALSE
        ex.quit
        destroy ex
        stop

TrapStatobject
.This routine tripped when Saveas method is called.
.
.We are trapping for instances where the User has selected a filename that: 1) Already exists
.and is open by another instance of Excel. 2) Already exists but not open elsewhere.  This instance
.will provoke Excel to produce a message asking User if they want to overwrite the file.  If they
.answer No or Cancel they will come to this routine.  Answering Yes will overwrite the file at the
.Saveas method found in above code.
        noreturn
        move    taskname,str50
        getinfo exception,taskname
        unpack  taskname,str55,str55,str10,str55
        scan    "Cannot access",str55
        if equal
.Instance 1 - exists and open elsewhere
                pack    taskname,str50," already exists and is open!!",carr,"Select another Filename!!"
                alert   caution,taskname,result
.                goto CampaignFileNameSelect
        endif
.Send them back to select another File name and try to Save again.
        goto StatFileNameSelect

errortrap
.testing purposes
        getinfo exception,taskname
        return
         stop
.end patch 2.0
format   trapclr     format
         trap        format giving error if format
         keyin       *p1:23,*el,*dv,error,str1;
         noreturn
         goto        looper
.debug    return
..............................................................................
         include     nwfio.inc
         include     nwfXio.inc
         include    statsio.inc
         include    nordio.inc
;patch2.4
			include	compio.inc
			include	cntio.inc
.         include    nmlrio.inc
;patch1.4
.begin patch 1.3
           INCLUDE    NDATio.inc
           INCLUDE    NXRFio.inc
           INCLUDE    NXCHio.inc
           INCLUDE    NXNGio.inc
.end patch 1.3
.begin patch 1.4
.        include     tncxio.inc
        include     SXRfio.inc
.end patch 1.4
.begin patch 2.3
               include        SlctClnIO.inc
.end patch 2.3

         include    comlogic.inc
