PC      EQU     0

        include COMMON.INC
        include CONS.INC
.ADDES FOR SEARCH PLF........................
**************************************
.for testing speed
        include  norddd.inc
*************************************
         include nrtndd.inc
         include ncmpdd.inc
         include winapi.inc
         include ncntdd.inc
 	 include compdd.inc
	 include cntdd.inc
         include NDATDD.INC
         include nowndd.inc
         INCLUDE LOGDATA.inc
         include nusedd.inc
         include ninvdd.inc
.For grand total add exception for "0"
.testing small form
RELEASE  INIT      "1.38"       DMB	11JUL2005	Added Code to Allow Order or Maildate filtering
.RELEASE  INIT      "1.37"       DMB	05OCT2004	Added code to use pdf995
.RELEASE  INIT      "1.36"       ASH	09AUG2004	Logo Conversion
.RELEASE  INIT      "1.35"       DMB 26MAY2004 Mailer Coversion
;RELEASE   INIT       "1.34"         DMB  04/25/03  Fixed sortfile being created in e:\data to c:\work
;RELEASE   INIT       "1.34"         DMB  03/20/03 Added sorting option
;RELEASE   INIT       "1.33"         DMB  03/18/03 Added option to pdf
;RELEASE   INIT      "1.32"          DMB  09/06/02  Removed stupid patch for acutal patch using osver
;RELEASE   INIT      "1.31"         DMB  07/26/02   Added code for Getwinver and changed var name from osflag to bosflag
;RELEASE   INIT      "1.3"          DB  06/03/02   Added Timer Corrected Display issue
.RELEASE   INIT      "1.2"          DB  01/14/02   Added NWF variance -new sort testing-allowed for manual input of titles
.RELEASE   INIT      "1.1"          DB 01/09/02   Added code for parens instead of dashes for neg num, centered title, page#
.RELEASE   INIT      "1.0"          DB 12/14/01   Variance Report-added muliple passes-now testing for time
...............................................................
NINLogo	PICT
	CREATE	NINLogo=3:13:30:50:
		"\\nts0\c\netutils\logo color box only.jpg"
except1 form 4
Except File
Orderfile file
	prepare except,"c:\work\tester.dat"
STOPFLAG init "N"	
INVOICEFILE FILE
PORTNUM  DIM       3
ID       DIM       1
PORTX    FORM      3
DIM3     DIM       3
ANS      DIM       1
SFILE    sndfile
SoundFLag form   1
NAME     DIM       25
mask13      init    "(ZZZ,ZZZ,ZZ9)"         ;formatting vars
Dim13a      dim     13	    ;formatting vars
mask15      init    "(ZZZ,ZZZ,ZZZ.99)"         ;formatting vars
Dim15a      dim     16	    ;formatting vars
mask7       init    "(Z,ZZZ)"         ;formatting vars
Dim7a       dim     7	    ;formatting vars
sortvar     form    1
columnvar   form    1
objectcoll      Collection
rptcan     dim 1
;patch1.34
holdreport form 9
...............................................................
.DataExclusive file (DATAEXCL.DAT)
EXCLUDE   FILE
.
.Print file
PRFILE     PFILE
.Menu
.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu
.Set Up SubMenu for Options
sSearch submenu
.Present Data for Menu Bar
FData   init    "&File;&Print;-;E&xit"
.FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search;-;&Preferences"
HData   init    "&Help;&About"
.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To"
.===============================================================================
.Code for Order Stats
CB      init   "B"
CO      init   "0"
CQ      init   "Q"

.Hex for Yes no dialog
yesno1   integer  1,"0x000004"

str19 dim 19
.str20 dim 20
str21 dim 21
str22 dim 22
str23 dim 23
str60 dim 60
.Var to hold list num
Lst     	dim     6
.Form Vars
.===============================================================================
.HoldVars for Order Dates
BegDate1 	form   	6
EndDate1 	form   	6
BegDate2 	form   	6
EndDate2 	form   	6
OrdX     	form   	6
.===============================================================================
.Var to hold Ex Spilt Qty
EXSPL    	form   	9
TMPVAR   	form	9

.Totals for Indiv Mailer
OrdOne		form    4
OrdNam1 	form    9
OrdTwo  	form    4
OrdNam2 	form    9
OrdVar  	form    9
ExOne   	form    9
ExTwo   	form    9
ExVar   	form    9
Rnt1    	form    9
Rnt2    	form    9
RntVar  	form    9
INC1    	form    9.2
INC2    	form    9.2
INCVar  	form    9.2
**************************
ExFlg   	form    1
**************************
RentOrd1 	form 	4
RentOrd2 	form 	4
RentOrdVar 	form 	4
.===============================================================================
.Gran Totals for all Mailers
TOTOrdOne  	form    9
TOTOrdNam1 	form    9
TOTOrdTwo  	form    9
TOTOrdNam2 	form    9
TOTOrdVar  	form    9
TOTExOne   	form    9
TOTExTwo   	form    9
TOTExVar   	form    9
TOTRnt1    	form    9
TOTRnt2    	form    9
TOTRntVar  	form    9
TOTINC1    	form    9.2
TOTINC2    	form    9.2
TOTINCVar  	form    9.2
TOTRentOrd1 	form    4
TOTRentOrd2 	form    4
TOTRentOrdVar  	form    4
........................................................................
TMPINCAP1	form   10.2
INCAP1  	form   10.2
INCOPEN 	form   10.2
INCJULDAYS 	form 	5

.Flag for year1 or year2
YR         	form    1
.Hold Order date
orddate    	form    5
DITEM      	form    4       .Total Items in DataList
CITEM      	form    4       .Current Item in Datalist
DONE       	dim     1       .Other flag if to test if sequential read is finished
.======================================================================
.Defining Columns for Titles
Title1   	form    9
Title2   	form    9
Title3   	form    9
Title4   	form    9
Column8  	form    9
Column9  	form    9
Column10 	form    9
Column11 	form    9
Column12 	form    9
custom   	form  	1
.==============================================================================
.Vars for YTD - Under 25000 Variance
Low        	form    "-24999"
High       	form    "49999"
SUBOLD     	form    9            .Running Total of Names which meet criteria for past year
SUBNEW     	form    9            .Running Total of Names which meet criteria for current year
ORD1SUB    	form    4            .Running Total of #of order which meet criteria for past year
ORD2SUB    	form    4            .Running Total of #of order which meet criteria for current year
OTOTSUB    	form    9            .Total of Variance for all which meet criteria
.===============================================================================
PRTNAME1   	DIM    30                          .Name of printfile
PRTDIR     	INIT    "c:\work\"                 .Printfile directory
PRTFILE1   	DIM    45                          .full printfile string
.===============================================================================
.Print Vars
PgCnt		form    9             .COUNT OF PAGES
NEWPG    	FORM    1             .COUNTER TO SHOW- IF NEW PAGE PUT TOTALS AT TOP OF PAGE
COPY     	FORM    3             .Number of Copies
NegPrV   	DIM    18
PRNTV    	dim    14            .Dummy Var for Printing
TITYR1   	DIM     4
TITYR2   	DIM     4
RPAREN   	INIT    ")"
LPAREN   	INIT    "("
.===============================================================================
.Sort Parameters=======================================================
INDAT    	init  "nindate.dat"   .File to be sorted
OUTSRT   	init  "NORDVAR.DAT"   .Sorted Output file
ClintSrt 	dim     7             .Sort by client #
SORTFLE   	dim    70             .Var to pack file names of sort
HOLDDATE  	DIM     4             .Var used in sort to show greater then previous year of selected dates

;patch1.34
Radios          radio  (6)
Button          button (2)
StatTextBoxes   stattext (2)
;patch1.34
.Patch 1.37
PRTITLE	DIM	50
.Patch 1.37
PrintArea form 2
PrinterName DIM  50
.===============================================================================
font6    	font
        create  font6,"Times New Roman",size=9
font7    	font
        create  font7,"Times New Roman",size=11
font8    	font
        create  font8,"Times New Roman",size=10
font9    	font
        create  font9,"Times New Roman",size=10,italic

White    color
LTGray   color
timer1    timer
.===============================================================================
GetIncome external "NINC0008;GETINCOME"
        move    "NVAR0001.PLS",Wprognme
        move    "Variance Report",Wfunction
        move    "David Baca",Wauthor
        move    "1.0",Wrelease
        move    "September 26, 2005",Wreldate
.===============================================================================

.>Patch 1.38 Flag to hold which filter we will use maildate or order date
VARDateFlag	FORM 1
DateSrt	DIM	3
DateSort	DIM	4
.>Patch 1.38
datetype DIM 1
CODE14  DIM 1
;patch 1.34
rpt2     plform  Report2
;1.34
proces    plform  Processing
srch    plform  Search
abt     plform  About
x 	PLFORM  NVAR0001
.===============================================================================
        winhide
        FORMLOAD x
        formload srch
        formload abt
        formload proces
;patch1.34
        formload rpt2        
;patch1.34
.===============================================================================
  create  white=*white
  create  ltgray=*ltgray
.===============================================================================
  create  NVAR0001;mFile,FData
  create  NVAR0001;mEdit,EData,mFile
  create  NVAR0001;mOptions,OData,mEdit
  create  NVAR0001;mHelp,HData,mOptions
.Create SubMenu
  create  NVAR0001;sSearch,SData,mOptions,1
.Activate Menus
.FileGo leads to stop
  activate mFile,FileGo,result
.Need this when it works
  activate mEdit,EditGo,result
  activate mOptions,OptionsGo,result
.Only a SubMenu under this one
  activate mHelp,HelpGo,result
.Activate SubMenus
  activate sSearch,SearchGo,result
  
  
  
.===============================================================================
        NVAR0001ListView.InsertColumn using "Mailer",1,0
        NVAR0001ListView.InsertColumn using "Mlr Num",100,1
        NVAR0001ListView.InsertColumn using "Orders Yr1",100,2
        NVAR0001ListView.InsertColumn using "Names Yr1",100,3
        NVAR0001ListView.InsertColumn using "Orders Yr2",100,4
        NVAR0001ListView.InsertColumn using "Names Yr2",100,5
        NVAR0001ListView.InsertColumn using "Variance",100,6
        NVAR0001ListView.InsertColumn using "Ex Yr1",100,7
        NVAR0001ListView.InsertColumn using "Ex Yr2",100,8
        NVAR0001ListView.InsertColumn using "Ex Variance",100,9
        NVAR0001ListView.InsertColumn using "Rent Yr1",100,10
        NVAR0001ListView.InsertColumn using "Rent Yr2",100,11
	NVAR0001ListView.InsertColumn using "Rent Variance",100,12          
        NVAR0001ListView.InsertColumn using "Income Year 1",100,13
        NVAR0001ListView.InsertColumn using "Income Year 2",100,14
        NVAR0001ListView.InsertColumn using "Income Variance",100,15
        NVAR0001ListView.InsertColumn using "Rental Orders 1",100,16
        NVAR0001ListView.InsertColumn using "Rental Orders 2",100,17
        NVAR0001ListView.InsertColumn using "Rental Orders Variance",100,18
              
        NVAR0001ListView.setcolumnFORMat giving result using 0,0
        NVAR0001ListView.setcolumnFORMat giving result using 1,0
        NVAR0001ListView.setcolumnFORMat giving result using 2,0
        NVAR0001ListView.setcolumnFORMat giving result using 3,1
        NVAR0001ListView.setcolumnFORMat giving result using 4,0
        NVAR0001ListView.setcolumnFORMat giving result using 5,1
        NVAR0001ListView.setcolumnFORMat giving result using 6,1
        NVAR0001ListView.setcolumnFORMat giving result using 7,1
        NVAR0001ListView.setcolumnFORMat giving result using 8,1
        NVAR0001ListView.setcolumnFORMat giving result using 9,1
        NVAR0001ListView.setcolumnFORMat giving result using 10,1
        NVAR0001ListView.setcolumnFORMat giving result using 11,1
        NVAR0001ListView.setcolumnFORMat giving result using 12,1
        NVAR0001ListView.setcolumnFORMat giving result using 13,1
        NVAR0001ListView.setcolumnFORMat giving result using 14,1        
        NVAR0001ListView.setcolumnFORMat giving result using 15,1
        NVAR0001ListView.setcolumnFORMat giving result using 16,1        
        NVAR0001ListView.setcolumnFORMat giving result using 17,1        
        NVAR0001ListView.setcolumnFORMat giving result using 18,1
        NVAR0001ListView.setcolumnFORMat giving result using 19,1                



.........................................................................
        setitem  NVAR0001EditCopies,0,"1"
        setfocus NVAR0001EditList
        getinfo  system,str6
        unpack   str6 into str1,str2
        unpack   str2 into str1
        call getwinver
..0 = unknown
..1 = Windows NT
..2 = WIN32s Windows 3.1x (obsolete)
..3 = Window 95
..4 = Window 98
..5 = Windows 2000
..6 = Windows XP
..8 = Windows CE
.===============================================================================
       MOVE C1 TO NUSEPATH    *ACCESS BY PORTNUMBER
       call findport
***********************************************************
	loop
		waitevent
	repeat
************************************************************
Begin

         Call OrderSetMouseBusy
         CALL DISABLE

;.Grab Variables
.Lets us know whether we are sorting by mail/order date(based on order file) or check or invoice date(based on invoice file).
.currently option 5 is the only one offering check/invoice(based on invoice file) option
.=========================================================================
	clear n4
	getitem ComboBox001,n4,n3
	move n3 to HoldReport
	getitem ComboBox002,n4,vardateflag	
	if (holdreport <> c5)
		load datetype using vardateflag,"O","M"
	else
		load datetype using vardateflag,"C","I"			
	endif

        if (holdreport = c1)
	        call sortbox
	elseif (holdreport = c5)
		call sortbox2
        endif
.=========================================================================
.>This section will establish the sort criteria for the primary sort by year.
	if (datetype = "O")
		move "202" to DateSrt
		move  "202-205" to ClintSrt
	elseif (datetype = "M")
		move "74" to DateSrt
		move  "74-77" to ClintSrt
	elseif (datetype = "C")
		move "215" to DateSrt
		move  "215-218" to ClintSrt	
	elseif (datetype = "I")	
		move "130" to DateSrt
		move  "130-133" to ClintSrt
	endif
.The totals reports go thru this file in order to check our exclusives instead of checking the datalist option.	
	if (HoldReport > c2  and HoldReport < c5)
		pack   SortFle,NTWKPATH6,"DATAEXCL.EOQ",comma,prtdir,"DATAEXCL.SRT"
		pack   taskname,sortfle,";","12-46"
                sort   taskname
		OPEN   EXCLUDE,"DATAEXCL.SRT",READ
                goto COPE
        else
	        clear citem
	        clear ditem
	        getitem NVAR0001DataList,c1,n2
	        if (n2 = c0)
	                alert caution,"Datalist cannot be blank. Please add a list!",result,"Invalid"
	                call enable
	                setfocus NVAR0001EditList
	                Call OrderSetMousefree
	                move c0 to Exflg
	                return
	         endif
	         move n2 to DITEM
	endif	
COPE
         if (custom = c1)
                getitem NVAR0001EditTitle1,0,str4
                if (str4 = "")
			alert caution,"Need a valid Title!",result,"Need Title"
			setfocus NVAR0001EditTitle1
                	return
                else
	                getitem NVAR0001EditTitle2,0,str4
                	if (str4 = "")
				alert caution,"Need a valid Title!",result,"Need Title"
				setfocus NVAR0001EditTitle2
				return
                	endif
        	endif
       	 endif
       	 getitem NVAR0001EditCopies,0,str3
       	 move str3 to copy
       	 if (copy = c0)
               	alert caution,"Need a valid number of copies!!",result,"Invalid"
       		call enable
       		setfocus NVAR0001EditCopies
       		Call OrderSetMousefree
      		move c0 to Exflg
		return
         endif
;.===============================================================================
;.Beg Date 1 of Variance
         getitem NVAR0001EditBegDate1,0,str10
         Type    str10
         if not equal
		CALL removechar using str10,dash
		CALL removechar using str10,slash
         endif
         count   result in str10
         if (result < c8)
               call alerter
               call enable
               Setfocus NVAR0001EditBegDate1
               call OrderSetMouseFree
               move c0 to Exflg
               return
         endif
         move str10 to str8
         unpack str8,mm,dd,str2,yy
;************************************************
         pack holddate with str2,yy
         move holddate to n4
         sub c1 from n4
         move n4 to holddate
;************************************************
         if (str8 = "")
                clear   str10
                call alerter
                call enable
                Setfocus NVAR0001EditBegDate1
                call OrderSetMouseFree
                move c0 to Exflg
                return
         else
                 pack    str10,mm,"/",dd,"/",str2,yy
         endif
         setitem NVAR0001EditBegDate1,0,str10
         call    cvtjul
         move    juldays to BegDate1
;.===============================================================================
;.End Date 1 of Variance
         getitem NVAR0001EditEndDate1,0,str10
         Type    str10
         if not equal
		CALL removechar using str10,dash
	 	CALL removechar using str10,slash
         endif
         count   result in str10
         if (result < c8)
                call alerter
                call enable
                Setfocus NVAR0001EditEndDate1
                call OrderSetMouseFree
                move c0 to Exflg
                return
         endif
         move str10 to str8
         unpack str8,mm,dd,str2,yy
         if (str8 = "")
                clear   str10
                call alerter
                call enable
                Setfocus NVAR0001EditEndDate1
                call OrderSetMouseFree
                move c0 to Exflg
                return
        else
                pack    str10,mm,"/",dd,"/",str2,yy
        endif
        setitem NVAR0001EditEndDate1,0,str10
        call    cvtjul
        move    juldays to EndDate1
;.===============================================================================
;.Beg Date 2 of Variance
         getitem NVAR0001EditBegDate2,0,str10
         Type    str10
         if not equal
	 	CALL removechar using str10,dash
		CALL removechar using str10,slash
         endif
         count   result in str10
         if (result < c8)
                call alerter
                call enable
                Setfocus NVAR0001EditBegDate2
                call OrderSetMouseFree
                move c0 to Exflg
                return
         endif
         move str10 to str8
         unpack str8,mm,dd,str2,yy
         if (str8 = "")
                clear   str10
                call alerter
                call enable
                Setfocus NVAR0001EditBegDate2
                call OrderSetMouseFree
                move c0 to Exflg
                return
         else
                pack    str10,mm,"/",dd,"/",str2,yy
         endif
         setitem NVAR0001EditBegDate2,0,str10
         call    cvtjul
         move    juldays to BegDate2
;.===============================================================================
;.End Date 2 of Variance
         getitem NVAR0001EditEndDate2,0,str10
         Type    str10
         if not equal
                     CALL removechar using str10,dash
                     CALL removechar using str10,slash
         endif
         count   result in str10
         if (result < c8)
                call alerter
                call enable
                Setfocus NVAR0001EditEndDate2
                call OrderSetMouseFree
                move c0 to Exflg
                return
         endif
         move str10 to str8
         unpack str8,mm,dd,str2,yy
         if (str8 = "")
                clear   str10
                call alerter
                call enable
                Setfocus NVAR0001EditEndDate2
                call OrderSetMouseFree
                move c0 to Exflg
                return
         else
                pack    str10,mm,"/",dd,"/",str2,yy
         endif
         setitem NVAR0001EditEndDate2,0,str10
         call    cvtjul
         move    juldays to EndDate2
.Enable stop button
	setprop nvar0001stop,enabled=c1
.Get Printer
	getitem Nvar0001ComboBoxPrint,n4,PrintArea
	if (PrintArea = "1")
		move "\\NTS0\laser2" to PrinterName
	elseif (PrintArea = "2")
		move "\\NTS0\laser3" to PrinterName	
	elseif (PrintArea = "3")
		move "PDF995" to PrinterName	
	elseif (PrintArea = "4")	
		clear printername
	endif
	move c1 to nordpath		
********************************************************************
;.Creates Timers for DialogWait Message Box
         create timer1,60
         activate timer1,tome1,result
         setitem PROCSTATCOMMENT,0,"Program busy.....Feel Free to do other Work!!"
         setprop Process,visible=c1
********************************************************************
//OrderSort
	if (holdreport <> c5)
        	pack   SortFle,ntwkpath6,"ninord.dat",comma,prtdir,"nindate.srt"
	        pack   taskname,sortfle,";",clintsrt,comma,"s=",DateSrt,">","'",HOLDDATE,"'"
	        sort   taskname
	        move "nindate.srt",indat
	else
***********************************************************************************
//InvoiceSort
        	pack   SortFle,ntwkpath6,"nininv.dat",comma,prtdir,"nindate.srt"
	        pack   taskname,sortfle,";",clintsrt,comma,"s=",DateSrt,">","'",HOLDDATE,"'"
	        sort   taskname
	        move "nindate.srt",indat
	endif
***********************************************************************************
Sorter
.        close nordfile
        close OrderFILE
        call cleartots
        call CLEARNVARS
        call CLEARLVARS
        call CLEARSUBS
        call clearmisc
        call clearlist
        if ((holdreport > c2) and (holdreport < c5))
                move    no to done
                READ    EXCLUDE,SEQ;LST
                goto    test IF OVER
        else
                add c1 to citem
                GETITEM NVAR0001DataList,CITEM,lst
        endif
	pack   SortFle,prtdir,indat,comma,prtdir,outsrt
.Sorts out records for list and whether it's billed or live,and is = or > beginning year of query	 
	if (holdreport <> c5)	        
        	pack   taskname,sortfle,";",clintsrt,comma,"s=16=","'",LST,"'","&",DateSrt,">","'",HOLDDATE,"'","&2=","'",CB,"'","|","16=","'",LST,"'","&",DateSrt,">","'",HOLDDATE,"'","&2=","'",CO,"'"
	        sort   taskname
	        if over
	               alert caution,S$ERROR$,result,"No Sort"
	               call OrderSetMouseFree
	               move c0 to Exflg
	               return
	        endif	        	
	endif
        if (holdreport <> c5)
	        move c1 to ndatpath
	        move c3 to nordflag
	        pack    NORDNAME,PRTDIR,"NORDVAR"        
        	OPEN    OrderFILE,NORDNAME,READ
	        reposit Orderfile,c0
	ELSE    
		if (citem = c1)
    			OPEN INVOICEFILE,"c:\work\nindate.srt",READ
    		endif
	        reposit INVOICEFILE,c0    		
    	ENDIF
        
        
Start

    Loop

Read
****************************************************
;    CALL   ANIMATEIT
****************************************************
    if (holdreport <> c5)
    	call checkstop
	read OrderFILE,SEQ;ORDVARS       
.	call NORD2SEQ
	
    ELSE
    	call checkstop    
	READ INVOICEFILE,SEQ;INVVARS
    ENDIF
    until over
	call debug if (olrn = "564551")	
	call debug if (olrn = "564552")	    
    if (holdreport = c5)    
	move lrn to nordfld
	call nordkey
	call debug if (olrn = "537681")	
	call debug if (olrn = "564552")		
    endif

;.Order Status Check
;.ORDER CANNOT HAVE STATUS OF x,X,Q,p,z
         match  OLNUM with lst
         goto   read if not equal
         cmatch     CO with OSTAT
         goto CONT if equal
         cmatch     CB with OSTAT
         goto CONT if equal
        if (holdreport <> c5)
        	goto Read
        else
         	cmatch  CQ with OSTAT
         	goto CONT if equal
         	goto Read         
        ENDIF
CONT
;.Check MAILDATE OR ORDER DATE (DEFAULT) or if for income run check Checkdate or income
	if (holdreport <> c5)
		IF (VARDATEFLAG = C1)
			move      OODTEY to YY
			move      OODTEM to MM
			move      OODTED to DD
		ELSE
		        move      OMDTEY to YY
		        move      OMDTEM to MM
		        move      OMDTED to DD		
		ENDIF
	else
		IF (VARDATEFLAG = C1)
		        move      CHK1DTEY  to YY
		        move      CHK1DTEM  to MM
		        move      CHK1DTED  to DD
	        ELSE
			move      INVDTEY to YY
			move      INVDTEM to MM
			move      INVDTED to DD
		ENDIF		
	endif
        call    cvtjul
        MOVE    juldays TO ORDDATE
        if      ((ORDDATE > BegDate1) & (Orddate < EndDate1) | (ORDDATE = BegDate1) | (Orddate = EndDate1))
		move c1 to YR
                goto Listview
        endif
        if      ((ORDDATE > BegDate2) & (Orddate < EndDate2) | (ORDDATE = BegDate2) | (Orddate = EndDate2))
                move c2 to YR
                goto Listview
        else
                goto read
        endif

Listview
	if (holdreport = c5)
		move NO to Code14
		call getincome using lrn,ostat,datetype,CODE14,INCAP1,INCJULDAYS,INCOPEN
		if ((CODE14 <> YES) and (STATB = "0"))
.			call debug
			move c0 to INCAP1
		endif
		
	endif
        NVAR0001ListView.GetItemCount giving result
        sub c1 from result
        for n9,"0",result
        	NVAR0001ListView.GetItemText giving str4 using n9,1
        	match OMLRNUM to str4
                if equal
			call GETINFO
			nvar0001listview.deleteitem giving n1  using n9
        		nvar0001listview.InsertItem giving n7  using str45
			NVAR0001ListView.setitemText giving n8 using n7,str7,1
                    	move   OQTY to TMPVAR
                        move   OEXQTY to EXSPL
                    	Branch YR to YearOne,YearTwo
YEARONE
			move str4    to OrdOne
			move STR10   to OrdNam1
			move str13   to ExOne
			move str16   to Rnt1
			if (holdreport = c5)	
	        		add  c1      to OrdOne			
				if (incap1 > 0)
			        	add  TMPVAR  to OrdNam1			
.Income Rental Order Count
					move str22 to RentOrd1
		      			add  c1      to RentOrd1
		 			move RentOrd1 to str22	        									        	
			        endif
			else			
	        		add  c1      to OrdOne
			        add  TMPVAR  to OrdNam1
		        endif

		        reset excodes
	        	scan oelcode in excodes
			if   equal
				if (EXSPL > c0)
			        	sub  EXSPL from TMPVAR
				        add  TMPVAR  to RNT1
			        	add  EXSPL  to ExOne
.Income Rental Order Count
.	        			add  c1      to RentOrd1
.        				move RentOrd1 to str22	        			
		                else
        	                        add  TMPVAR  to ExOne
					add c1 to except1   
					write except,seq;*cdfon,olrn,oqty,incap1

	        	        endif
;.===============================================================================
                 	else
	                       add  TMPVAR  to Rnt1
.Income Rental Order Count
.        			add  c1      to RentOrd1	                       
.        			move RentOrd1 to str22
		        endif
			clear str4
		 	move ORDONE  to str4
			move OrdNam1 to STR10
			move EXONE   to str13
			move RNT1    to str16
			if (holdreport = c5)		
				move str19 to TMPincap1
				add TMPincap1 to incap1
				move incap1 to str19				
			endif
	 	 	call SETINFO
			goto READ
YEARTWO
			move str5    to OrdTwo
			move STR11   to OrdNam2
			move str14   to ExTwo
			move str17   to Rnt2

			if (holdreport = c5)	
	        	        add  c1      to OrdTwo
				if (incap1 > 0)
					add  TMPVAR  to OrdNam2				
.Income Rental Order Count
					move str23 to RentOrd2			
		      			add  c1      to RentOrd2
					move RentOrd2 to str23					
				endif
			else	
        	        	add  c1      to OrdTwo
				add  TMPVAR  to OrdNam2
			endif
			reset excodes
		 	scan oelcode in excodes
		 	if   equal
		 		if (EXSPL > c0)
		      			sub  EXSPL from TMPVAR
					add  TMPVAR  to RNT2
					add  EXSPL  to ExTwo
.Income Rental Order Count
.	        			add  c1      to RentOrd2
.        				move RentOrd2 to str23						
			        else
				        add  TMPVAR  to ExTwo
					add c1 to except1
					write except,seq;*cdfon,olrn,oqty,incap1					
                    		endif
	         	else
		        	add  TMPVAR  to Rnt2
.Income Rental Order Count
.	       			add  c1      to RentOrd2
.				move RentOrd2 to str23			        	
			endif
			move ORDTWO  to str5
			move OrdNam2 to STR11
			move EXTWO   to str14
			move RNT2    to str17
			if (holdreport = c5)		
				move str20 to TMPincap1
				add TMPincap1 to incap1
				move incap1 to str20
			endif		
			call SETINFO
	                goto read
		endif
        repeat
LVInsert
	if (holdreport=c5)
		goto SkipLV if (incap1 <= 0)
	endif

        call zfillit using omlrnum,C0
        pack mkey from omlrnum,Z3
        call NMLRKey
        NVAR0001ListView.InsertItem giving  N7 using MCOMP
        NVAR0001ListView.SetItemText giving N8 using N7,MKEY,1
        move c0 to EXSPL
        move OEXQTY to EXSPL
        move c1 to str1
        move c0 to tmpvar
        move OQTY to tmpvar
        add c0 to tmpvar
        move tmpvar to OQTY
        compare c1 to yr
        if equal
        	NVAR0001ListView.SetItemText giving N8 using n7,str1,2
        	NVAR0001ListView.SetItemText giving N8 using n7,OQTY,3
                reset excodes
                scan oelcode in excodes
.===============================================================================
                if   equal
                	if (EXSPL > c0)
				sub  EXSPL from TMPVAR
                                move tmpvar to str10
                                move exspl to str11
				NVAR0001ListView.SetItemText giving N8 using n7,str11,7
				NVAR0001ListView.SetItemText giving N8 using n7,str10,10
.Income - Starting Rental Order Count Year 1
				NVAR0001ListView.SetItemText giving N8 using n7,str1,16	
				NVAR0001ListView.SetItemText giving N8 using n7,"0",17	        										
	        	ELSE
				NVAR0001ListView.SetItemText giving N8 using n7,OQTY,7
				NVAR0001ListView.SetItemText giving N8 using n7,"0",10
				write except,seq;*cdfon,olrn,oqty,incap1
.Income - Starting Rental Order Count Year 1- Dont Care if it has income include it
				NVAR0001ListView.SetItemText giving N8 using n7,str1,16	        		
				NVAR0001ListView.SetItemText giving N8 using n7,"0",17					
                        endif
                else
	        		NVAR0001ListView.SetItemText giving N8 using n7,"0",7
	        		NVAR0001ListView.SetItemText giving N8 using n7,OQTY,10
.Income - Starting Rental Order Count Year 1
				NVAR0001ListView.SetItemText giving N8 using n7,str1,16	        		
				NVAR0001ListView.SetItemText giving N8 using n7,"0",17	        						
                endif
;.===============================================================================
        	NVAR0001ListView.setitemText giving N8 using n7,"0",4
        	NVAR0001ListView.setitemText giving N8 using n7,"0",5
        	NVAR0001ListView.setitemText giving N8 using n7,"0",6
        	NVAR0001ListView.setitemText giving N8 using n7,"0",8
        	NVAR0001ListView.setitemText giving N8 using n7,"0",9
        	NVAR0001ListView.setitemText giving N8 using n7,"0",11
        	NVAR0001ListView.setitemText giving N8 using n7,"0",12

		if (holdreport = c5)	
			move incap1 to str19		
        		NVAR0001ListView.setitemText giving N8 using n7,str19,13			
        		NVAR0001ListView.setitemText giving N8 using n7,"0",14        		
        		NVAR0001ListView.setitemText giving N8 using n7,"0",15        		
		endif
.===============================================================================
        ELSE
        	NVAR0001ListView.SetItemText giving N8 using n7,str1,4
        	NVAR0001ListView.SetItemText giving N8 using n7,OQTY,5
                reset excodes
                scan oelcode in excodes
       	        if   equal
                	if (EXSPL > c0)
				sub  EXSPL from TMPVAR
                                move tmpvar to str10
                                move exspl to str11
				NVAR0001ListView.SetItemText giving N8 using n7,str11,8
				NVAR0001ListView.SetItemText giving N8 using n7,str10,11
.Income - Starting Rental Order Count Year 2
				NVAR0001ListView.SetItemText giving N8 using n7,"0",16
				NVAR0001ListView.SetItemText giving N8 using n7,str1,17
                        else
				NVAR0001ListView.SetItemText giving N8 using n7,"0",11
				NVAR0001ListView.SetItemText giving N8 using n7,OQTY,8
				write except,seq;*cdfon,olrn,oqty,incap1				
.Income - Starting Rental Order Count Year 2 - Dont Care, if it has income include it
				NVAR0001ListView.SetItemText giving N8 using n7,"0",16
				NVAR0001ListView.SetItemText giving N8 using n7,str1,17	  				
                        endif
                else
                           	NVAR0001ListView.SetItemText giving N8 using n7,"0",8
	       			NVAR0001ListView.SetItemText giving N8 using n7,OQTY,11
.Income - Starting Rental Order Count Year 2
				NVAR0001ListView.SetItemText giving N8 using n7,"0",16
				NVAR0001ListView.SetItemText giving N8 using n7,str1,17	       			
                endif
.===============================================================================
        	NVAR0001ListView.SetItemText giving N8 using n7,"0",2
        	NVAR0001ListView.SetItemText giving N8 using n7,"0",3
        	NVAR0001ListView.setitemText giving N8 using n7,"0",6
        	NVAR0001ListView.setitemText giving N8 using n7,"0",7
        	NVAR0001ListView.setitemText giving N8 using n7,"0",9
        	NVAR0001ListView.setitemText giving N8 using n7,"0",10
        	NVAR0001ListView.setitemText giving N8 using n7,"0",12
		if (holdreport = c5)	
			move incap1 to str20		
        		NVAR0001ListView.setitemText giving N8 using n7,"0",13	
        		NVAR0001ListView.setitemText giving N8 using n7,str20,14        		
        		NVAR0001ListView.setitemText giving N8 using n7,"0",15        		
		endif        	
.===============================================================================
        ENDIF
SkipLV        
    Repeat
.===============================================================================
.=========================================================================
.        getprop NVAR0001RadioRntEx,SELGROUPID=PINT
.       1-Rent Exchange 2-Total
.=========================================================================
        call variancecount
Startprint
........................................................
.Position of Columns
        	move    "100",column
	if (holdreport <> c5)
        	move    "2400",column1
        	move    "3100",column2
        	move    "3600",column3
        	move    "4300",column4
        	move    "5200",column5
        	move    "5900",column6
        	move    "6800",column7
        	move    "7700",column8
        	move    "8600",column9
        	move    "9500",column10
        	move    "10400",column11
        	move    "11300",column12
        	move    "3000",Title1
        	move    "6800",Title2
        	move    "4250",Title3
        	move    "5250",Title4
	else
        	move    "3100",column1
        	move    "4000",column2
        	move    "4900",column3
        	move    "5800",column4
        	move    "6700",column5
        	move    "7600",column6
        	move    "8500",column7
        	move    "9400",column8
        	move    "10300",column9
        	move    "9500",column10
.        	move    "10400",column11
.        	move    "11300",column12
        	move    "4000",Title1
        	move    "6700",Title2
        	move    "7700",Title3
        	move    "5250",Title4	
	endif
        move  lst to ndatfld
        call  ndatkey
        clock timestamp,timestamp
        call trim using inits
	Pack Prtitle,inits,timestamp
        Pack  prtname1,Prtitle,".pdf"
        PACK  PRTFILE1,"c:\work\pdf\",PRTNAME1
        if (printarea=c3)
.>Patch 3.3 Logic Addition for PDF Quality Control
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
	endif
.>Patch 3.3 Logic Addition for PDF Quality Control        
        clear n9
        move osflag to str2
        Trap Spool1 giving error if SPOOL
       	PRTOPEN prfile,PrinterName,PRTITLE
        move osflag to str2
	call GETHEADERDATES
        BRANCH holdreport to Print1,Print2,Print2,Print2,Print1
Print1        

        prtpage   prfile;*UNITS=*HIENGLISH:
                         *ORIENT=*LANDSCAPE:
                         *Copies=Copy;
.===============================================================================
.Grab Number of Records in Listview
        NVAR0001ListView.GetItemCount giving result
        sub c1 from result
Page
        ADD     C1 TO PGCNT
        If (Pgcnt = c1)
		        prtpage   prfile;*ORIENT=*LANDSCAPE:
        	        *UNITS=*HIENGLISH
;don't remove had to fudge to get text to work properly on first page.        	        
.		        prtpage prfile;*p0:0," ";        
        else
        		prtpage   prfile;*NEWPAGE:
                         *ORIENT=*LANDSCAPE:
        	         *UNITS=*HIENGLISH       
        endif        
        goto incomeloop if (holdreport = c5)        
	call RentExchangeVarianceHeader
        loop
        	until (N9 > result)
                call getinfo
.Total Names
.Variance
	        prtpage prfile;*pColumn:row,*font=font6,*ALIGNMENT=*left,*ll,str45;
                move str10 to OrdNam1
                move str11 to OrdNam2
                add ORDNAM1 to TOTORDNAM1
                add ORDNAM2 to TOTORDNAM2
                sub ORDNAM1 from OrdNam2,OrdVar
                ADD ORDVAR to TOTORDVAR
                move ORDVAR to str12
                move mask13 to dim13a
                edit ordvar to dim13a
                squeeze dim13a,dim13a
                NVAR0001ListView.setitemText giving n8 using n9,dim13a,6
                if (ORDVAR = c0)
		        prtpage prfile;*pColumn5:row,*font=font6,*ALIGNMENT=*right,*ll,"0";
                else
		        prtpage prfile;*pColumn5:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;
                endif
.YR1
                move str4 to OrdOne
                add ORDONE to TOTORDONE
                if (ordone = c0)
                	move c0 to dim7a
		else
	               	move mask7 to dim7a
	               	edit OrdOne to dim7a
	               	squeeze dim7a,dim7a 
		endif	        
	        prtpage prfile;*pColumn1:row,*font=font6,*ALIGNMENT=*Left,*ll,Dim7a;		

                move mask13 to dim13a
                edit ordnam1 to dim13a
                squeeze dim13a,dim13a
		prtpage prfile;*pColumn2:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;
.YR2
                move str5 to OrdTwo
                add ORDTWO to TOTORDTWO
                if (ordtwo = c0)
                	move c0 to dim7a
                else
	               	move mask7 to dim7a
	               	edit OrdTwo to dim7a
	               	squeeze dim7a,dim7a                 
	        endif
	        prtpage prfile;*pColumn3:row,*font=font6,*ALIGNMENT=*Left,*ll,dim7a;
	        
                move mask13 to dim13a
                edit ordnam2 to dim13a
                squeeze dim13a,dim13a
		prtpage prfile;*pColumn4:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;
.Exchange Names
.Variance
                move str13 to EXONE
                move str14 to EXTWO
                ADD EXONE to TOTEXOne
                ADD EXTWO to TOTEXTWO
                sub EXONE from EXTWO,EXVAR

                move mask13 to dim13a
                edit exvar to dim13a
                call trim using dim13a
                NVAR0001ListView.setitemText giving n8 using n9,dim13a,9
                ADD EXVAR to TOTEXVAR
                move EXVAR to str15
                if (EXVAR = c0)
		        prtpage prfile;*pColumn8:row,*font=font6,*ALIGNMENT=*right,*ll,"0";
                else
                
	                move mask13 to dim13a
	                edit ExVar to dim13a
	                squeeze dim13a,dim13a                
		        prtpage prfile;*pColumn8:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;                
                endif
.YR1
		if (ExOne=c0)
			prtpage prfile;*pColumn6:row,*font=font6,*ALIGNMENT=*right,*ll,"0";
		else
                	move mask13 to dim13a
                	edit ExOne to dim13a
                	squeeze dim13a,dim13a
			prtpage prfile;*pColumn6:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;
		endif
.YR2
		if (ExTwo=c0)
			prtpage prfile;*pColumn7:row,*font=font6,*ALIGNMENT=*right,*ll,"0";		
		else
	                move mask13 to dim13a
	                edit ExTwo to dim13a
	                squeeze dim13a,dim13a
			prtpage prfile;*pColumn7:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;
		endif		
.Rental Names
.Variance
                move str16 to RNT1
                move str17 to RNT2
                ADD RNT1 TO TOTRNT1
                ADD RNT2 TO TOTRNT2
                sub RNT1 from RNT2,RNTVAR
                move mask13 to dim13a
                edit rntvar to dim13a
                squeeze dim13a,dim13a
                NVAR0001ListView.setitemText giving n8 using n9,dim13a,12
                ADD RNTVAR TO TOTRNTVAR
                move RNTVAR to str18
                if (RNTVAR = c0)
		        prtpage prfile;*pColumn11:row,*font=font6,*ALIGNMENT=*right,*ll,"0";
                else
		        prtpage prfile;*pColumn11:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;                
                endif
.YR1
                if (RNT1 = c0)
			prtpage prfile;*pColumn9:row,*font=font6,*ALIGNMENT=*right,*ll,"0";                
                else
	                move mask13 to dim13a
	                edit rnt1 to dim13a
	                squeeze dim13a,dim13a
			prtpage prfile;*pColumn9:row,*font=font6,*ALIGNMENT=*right,*ll,dim13a;                
		endif

.YR2
                if (RNT2 = c0)
			prtpage prfile;*pColumn10:row,*font=font6,*ALIGNMENT=*right,*ll,"0";                
                else
                	move mask13 to dim13a
                	edit rnt2 to dim13a
                	squeeze dim13a,dim13a                
			prtpage prfile;*pColumn10:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;
		endif
	        add     "100",row
	        add     eightlpi,row
.Footer
        	if (ROW >= "7550")
                        call Footer
                        add c1 to n9
                	goto page
                endif
                add  c1 to n9
        repeat
ENDLOOP

       if (ROW < "7450")
	       goto totals
       else
               call footer
               move c1 to newpg
               add  c1 to pgcnt
               prtpage prfile;*NEWPAGE:
                              *UNITS=*HIENGLISH:
                              *ORIENT=*LANDSCAPE;
               goto Totals
       endif

//
Incomeloop
	call IncomeVarianceHeader

        loop
        	until (N9 > result)
                call getinfo
.Mailer Name
	        prtpage prfile;*pColumn:row,*font=font6,*ALIGNMENT=*Left,*ll,str45;
	        
.Change from rent only to all order to allow for exceptions(exchange Records with income)	        
                move str10 to OrdNam1
                move str11 to OrdNam2
                add ORDNAM1 to TOTORDNAM1
                add ORDNAM2 to TOTORDNAM2
                sub ORDNAM1 from OrdNam2,OrdVar
                move mask13 to dim13a
                edit ordvar to dim13a
                squeeze dim13a,dim13a
                NVAR0001ListView.setitemText giving n8 using n9,dim13a,6
.                if (ORDVAR = c0)
.		        prtpage prfile;*pColumn5:row,*font=font6,*ALIGNMENT=*right,*ll,"0";
.                else
.		        prtpage prfile;*pColumn5:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;
.		 endif

.
.                move str16 to RNT1
.                move str17 to RNT2
.                ADD RNT1 TO TOTRNT1
.                ADD RNT2 TO TOTRNT2
.                sub RNT1 from RNT2,RNTVAR
.                move mask13 to dim13a
.                edit rntvar to dim13a
.                squeeze dim13a,dim13a
.                NVAR0001ListView.setitemText giving n8 using n9,dim13a,12


.Rental Order Variance 
		clear RentOrdVAR
                move str22 to RentOrd1
                move str23 to RentOrd2
                ADD RentOrd1 TO TOTRentOrd1
                ADD RentOrd2 TO TOTRentOrd2
                sub RentOrd1 from RentOrd2,RentOrdVAR    
                move mask7 to dim7a
                edit RentOrdvar to dim7a
                call trim using dim7a                
                NVAR0001ListView.setitemText giving n8 using n9,dim7a,18
                
.Income 
.Variance
                move str19 to INC1
                move str20 to INC2
                ADD INC1 TO TOTINC1
                ADD INC2 TO TOTINC2
                sub INC1 from INC2,INCVAR
                move mask15 to dim15a
                edit INCvar to dim15a
		squeeze dim15a,dim15a                
                NVAR0001ListView.setitemText giving n8 using n9,dim15a,15               

.UPdate for all orders not just rental
                if (INC1<=c0 and INC2<=c0)
                	goto Incomeloopcont
                endif
.                if ((RNT1<=c0 and RNT2<=c0)and (INC1<=c0 and INC2<=c0))
.                	goto Incomeloopcont
.                endif
                


.Rental Orders YR1                		
                if (RentOrd1 <> c0)
                	move mask7 to dim7a                
                	edit RentOrd1 to dim7a
                	squeeze dim7a,dim7a 
               	else
               		move "0" to dim7a
               	endif
		prtpage prfile;*pColumn1:row,*font=font6,*ALIGNMENT=*right,*ll,dim7a;


.Updated to Total Orders YR1 - possibility of Exchange Records
                move mask13 to dim13a
                edit ordnam1 to dim13a
                squeeze dim13a,dim13a
		prtpage prfile;*pColumn2:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;
.Rental YR1
.                move mask13 to dim13a
.                edit Rnt1 to dim13a
.		squeeze dim13a,dim13a
.		prtpage prfile;*pColumn2:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;


.Income YR1
                move mask15 to dim15a
                edit inc1 to dim15a
		squeeze dim15a,dim15a
		prtpage prfile;*pColumn3:row,*font=font6,*ALIGNMENT=*right,*ll,dim15a;

.Rental Orders YR2
                if (RentOrd2 <> c0)
                	move mask7 to dim7a
                	edit RentOrd2 to dim7a
                	squeeze dim7a,dim7a 
                else
               		move "0" to dim7a                	
                endif
		prtpage prfile;*pColumn4:row,*font=font6,*ALIGNMENT=*right,*ll,dim7a ;


.Updated to Total Orders YR2 - possibility of Exchange Records
                move mask13 to dim13a
                edit ordnam2 to dim13a
                squeeze dim13a,dim13a
		prtpage prfile;*pColumn5:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;
.Rental YR2
.                move mask13 to dim13a
.                edit Rnt2 to dim13a
.		squeeze dim13a,dim13a
.		prtpage prfile;*pColumn5:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;
		
.Income YR2
                move mask15 to dim15a
                edit inc2 to dim15a
		squeeze dim15a,dim15a
		prtpage prfile;*pColumn6:row,*font=font6,*ALIGNMENT=*right,*ll,dim15a;


.Rental Order Variance               
                ADD RentOrdVAR TO TOTRentOrdVAR
                if (RentOrdVAR <> c0)                
                	move mask7 to dim7a
                	edit RentOrdVAR to dim7a
			squeeze dim7a,dim7a                
		else
			move "0" to dim7a
		endif
    		prtpage prfile;*pColumn7:row,*font=font6,*ALIGNMENT=*right,*ll,dim7a;	

.Change to Order Variance to accomodate Exchange Records
                ADD ORDVAR to TOTORDVAR
                move mask13 to dim13a
                edit ordvar to dim13a
                squeeze dim13a,dim13a
                if (ORDVAR = c0)
		        prtpage prfile;*pColumn8:row,*font=font6,*ALIGNMENT=*right,*ll,"0";
                else
		        prtpage prfile;*pColumn8:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;
		endif
.Rental Variance               
.                ADD RNTVAR TO TOTRNTVAR
.                move mask13 to dim13a
.                edit rntvar to dim13a
.                squeeze dim13a,dim13a
.                if (RNTVAR = c0)
.		        prtpage prfile;*pColumn8:row,*font=font6,*ALIGNMENT=*right,*ll,"0";
.                else
.		        prtpage prfile;*pColumn8:row,*font=font6,*ALIGNMENT=*right,*ll,Dim13a;                
.                endif

.Income Names
.Variance
                ADD INCVAR TO TOTINCVAR
                move mask15 to dim15a
                edit INCVAR to dim15a
		squeeze dim15a,dim15a                
       		prtpage prfile;*pColumn9:row,*font=font6,*ALIGNMENT=*right,*ll,dim15a;		

	                
                

		
	        add     "100",row
	        add     eightlpi,row	        
.Footer
        	if (ROW >= "7550")
                        call Footer
                        add c1 to n9
                	goto page
                endif
Incomeloopcont                
                add  c1 to n9
	                
        repeat
ENDLOOP3

       if (ROW < "7450")
	       goto IncomeVarianceTotals
       else
               call footer
               move c1 to newpg
               add  c1 to pgcnt
               prtpage prfile;*NEWPAGE:
                              *UNITS=*HIENGLISH:
                              *ORIENT=*LANDSCAPE;
	       call IncomeVarianceHeader                             
               goto IncomeVarianceTotals
       endif
//

TOTALS
       if (newpg = c1)
       	       move "520",row
       endif
       add     eightlpi,row
       add     "50" to row
       prtpage prfile;*pColumn:row,*pensize=20,*line=Column11:row;        	
       add     "50" to row                 
               
               prtpage prfile;*pColumn:row,*font=font6,*ALIGNMENT=*Left,*boldon,*ULON,"Totals",*boldoff,*ULOFF;
.Total Orders  First
               move TOTORDONE to prntv
               call FormatNumeric using prntv,negprv,comma
               move negprv,prntv
               prtpage prfile;*pcolumn1:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.Total Names   First
               move TOTORDNAM1 to prntv
               call FormatNumeric using prntv,negprv,comma
               move negprv,prntv
               prtpage prfile;*pcolumn2:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.Tota1 Orders Last
               move TOTORDTWO to prntv
               call FormatNumeric using prntv,negprv,comma
               move negprv,prntv
               prtpage prfile;*pcolumn3:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.Total Names   Last
               move TOTORDNAM2 to prntv
               call FormatNumeric using prntv,negprv,comma
               move negprv,prntv
               prtpage prfile;*pcolumn4:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.Total Name Variance
               move TOTORDVAR to prntv
               call removechar using prntv,dash
               call FormatNumeric using prntv,negprv,comma
               move negprv to prntv
               if (TOTORDVAR = c0)
		 	prtpage prfile;*pcolumn5:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,"0",*boldoff;
               else
               		if (TOTORDVAR < c0)
                        	pack NegPrV,LPAREN,prntv,RPAREN
	 	   		prtpage prfile;*pcolumn5:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,negprv,*boldoff;
               		else
	 	   		prtpage prfile;*pcolumn5:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntv,*boldoff;
               		endif
               endif
.Total Exchange First
               move TOTExOne to prntv
               IF (TOTEXONE = c0)
	               prtpage prfile;*pcolumn6:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,"0",*boldoff;
               ELSE
	               call FormatNumeric using prntv,negprv,comma
        	       move negprv,prntv
	               prtpage prfile;*pcolumn6:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
               ENDIF
.Total Exchange Last
               move TOTExTwo to prntv
               IF (TOTEXTWO = c0)
	               prtpage prfile;*pcolumn7:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,"0",*boldoff;
               ELSE
	               call FormatNumeric using prntv,negprv,comma
        	       move negprv,prntv
	               prtpage prfile;*pcolumn7:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
               ENDIF
.Total Exchange Last Variance
               move c0 to prntv
               move TOTEXVAR to prntv
               call removechar using prntv,dash
               call FormatNumeric using prntv,negprv,comma
               move negprv to prntv
               if (TOTEXVAR = c0)
		 	prtpage prfile;*pcolumn8:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,"0",*boldoff;
               else
               		if (TOTEXVAR < c0)
                        	pack NegPrV,LPAREN,prntv,RPAREN
	 	   		prtpage prfile;*pcolumn8:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,negprv,*boldoff;
               		else
	 	   		prtpage prfile;*pcolumn8:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntv,*boldoff;
               		endif
               endif
.Total Rental First
               move TOTRnt1 to prntv
               IF (TOTRNT1 = c0)
	               prtpage prfile;*pcolumn9:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,"0",*boldoff;
               ELSE
	               call FormatNumeric using prntv,negprv,comma
        	       move negprv,prntv
	               prtpage prfile;*pcolumn9:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
               ENDIF
.Total Rental Last
               move TOTRNT2 to prntv
               IF (TOTRNT2 = c0)
	               prtpage prfile;*pcolumn9:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,"0",*boldoff;
               ELSE
	               call FormatNumeric using prntv,negprv,comma
        	       move negprv,prntv
	               prtpage prfile;*pcolumn10:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
               ENDIF
.Total Rental Last Variance
               move TOTRNTVAR to prntv
               call removechar using prntv,dash
               call FormatNumeric using prntv,negprv,comma
               move negprv to prntv
               if (TOTRNTVAR = c0)
		 	prtpage prfile;*pcolumn11:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,"0",*boldoff;
               else
	               if (TOTRNTVAR < c0)
                           pack NegPrV,LPAREN,prntv,RPAREN
	 		   prtpage prfile;*pcolumn11:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,negprv,*boldoff;
	               else
	 		   prtpage prfile;*pcolumn11:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntv,*boldoff;
        	       endif
               endif
	       add eightlpi,row       		
	       add "50" to row
	       prtpage prfile;*pColumn:row,*pensize=20,*line=Column11:row;                 
               call footer
               goto copies
IncomeVarianceTotals
.	if (newpg = c1)
.       	       move "520",row
.       	endif
	add     eightlpi,row
	add     "50" to row
       	prtpage prfile;*pColumn:row,*pensize=20,*line=Column9:row;        	
	add     "50" to row       	
	prtpage prfile;*pColumn:row,*font=font6,*ALIGNMENT=*Left,*boldon,*ULON,"Totals",*boldoff,*ULOFF;               

.Rental Orders YR1 	
        if (TOTRentOrd1 <> c0)
	        move mask7 to dim7a        
             	edit TOTRentOrd1 to dim7a
	      	call trim using dim7a 
      	else
  		move "0" to dim7a
      	endif	
	prtpage prfile;*pColumn1:row,*font=font6,*ALIGNMENT=*right,*ll,dim7a;


.Orders YR1 Change from Rental Only
        move TOTORDNAM1 to prntv
        call FormatNumeric using prntv,negprv,comma
        move negprv,prntv
	prtpage prfile;*pcolumn2:row,*font=font6,*ALIGNMENT=*Right,*ll,prntV;
.Rental YR1
.        move TOTRnt1 to prntv
.        IF (TOTRNT1 = c0)
.	        prtpage prfile;*pcolumn2:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,"0",*boldoff;
.        ELSE
.                call FormatNumeric using prntv,negprv,comma
.		move negprv,prntv
.	        prtpage prfile;*pcolumn2:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.	ENDIF	
.Income YR 1
        move mask15 to dim15a
        edit TOTinc1 to dim15a
	squeeze dim15a,dim15a
	prtpage prfile;*pColumn3:row,*font=font6,*ALIGNMENT=*right,*ll,dim15a;

.Rental Orders YR2
                if (TOTRentOrd2 <> c0)
                	move mask7 to dim7a
                	edit TOTRentOrd2 to dim7a
                	call trim using dim7a 
                else
               		move "0" to dim7a                	
                endif
		prtpage prfile;*pColumn4:row,*font=font6,*ALIGNMENT=*right,*ll,dim7a ;

.
.Total Orders YR 2 -Changed from Rental Only
               move TOTORDNAM2 to prntv
               call FormatNumeric using prntv,negprv,comma
               move negprv,prntv
               prtpage prfile;*pcolumn5:row,*font=font6,*ALIGNMENT=*Right,*ll,prntV;

.Total Rental YR 2
.               move TOTRNT2 to prntv
.               IF (TOTRNT2 = c0)
.	               prtpage prfile;*pcolumn5:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,"0",*boldoff;
.               ELSE
.	               call FormatNumeric using prntv,negprv,comma
.        	       move negprv,prntv
.	               prtpage prfile;*pcolumn5:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.               ENDIF

.Income YR2
                move mask15 to dim15a
                edit TOTinc2 to dim15a
		squeeze dim15a,dim15a
		prtpage prfile;*pColumn6:row,*font=font6,*ALIGNMENT=*right,*ll,dim15a;
		
.Rental Order Variance               
                if (TOTRentOrdVAR <> c0)                
                	move mask7 to dim7a
                	edit TOTRentOrdVAR to dim7a
			squeeze dim7a,dim7a                
		else
			move "0" to dim7a
		endif
    		prtpage prfile;*pColumn7:row,*font=font6,*ALIGNMENT=*right,*ll,dim7a;
    		
.Total Order Variance - Changed from Rental Only to allow for Exchange Only
               move TOTORDVAR to prntv
               call removechar using prntv,dash
               call FormatNumeric using prntv,negprv,comma
               move negprv to prntv
               if (TOTORDVAR = c0)
		 	prtpage prfile;*pcolumn8:row,*font=font6,*ALIGNMENT=*Right,*ll,"0";
               else
               		if (TOTORDVAR < c0)
                        	pack NegPrV,LPAREN,prntv,RPAREN
	 	   		prtpage prfile;*pcolumn8:row,*font=font6,*ALIGNMENT=*Right,*ll,negprv;
               		else
	 	   		prtpage prfile;*pcolumn8:row,*font=font6,*ALIGNMENT=*Right,*ll,prntv;
               		endif
               endif

.Total Rental Variance
.               move TOTRNTVAR to prntv
.               call removechar using prntv,dash
.               call FormatNumeric using prntv,negprv,comma
.               move negprv to prntv
.               if (TOTRNTVAR = c0)
.		 	prtpage prfile;*pcolumn8:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,"0",*boldoff;
.               else
.	               if (TOTRNTVAR < c0)
.                           pack NegPrV,LPAREN,prntv,RPAREN
.	 		   prtpage prfile;*pcolumn8:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,negprv,*boldoff;
.	               else
.	 		   prtpage prfile;*pcolumn8:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntv,*boldoff;
.        	       endif
.               endif               
.Total Income Variance
                if (TotINCVAR <> c0)
                	move mask15 to dim15a
                	edit TOTINCVAR to dim15a
			squeeze dim15a,dim15a                
       			prtpage prfile;*pColumn9:row,*font=font6,*ALIGNMENT=*right,*ll,dim15a;		
       		else
       			move "0" to dim15a
       			prtpage prfile;*pColumn9:row,*font=font6,*ALIGNMENT=*right,*ll,dim15a;		       			
       			
       		endif
		add     eightlpi,row       		
		add     "50" to row
	       	prtpage prfile;*pColumn:row,*pensize=20,*line=Column9:row;        	       		
       		
        call footer
        goto copies               
               
Print2
.===============================================================================
.Grab Number of Records in Listview
        NVAR0001ListView.GetItemCount giving result
        sub c1 from result
Page2
        ADD       C1 TO PGCNT
        If (Pgcnt = c1)
		        prtpage   prfile;*ORIENT=*PORTRAIT:
	                         *UNITS=*HIENGLISH;
.		        prtpage prfile;*p0:0," ";        
        else
        		prtpage   prfile;*NEWPAGE:
                         *ORIENT=*PORTRAIT:
                         *UNITS=*HIENGLISH;        
        endif
	call    VarianceTotalsHeader
        loop
reloop
        	until (N9 > result)
                call getinfo
.Total Names
.Variance
                move str10 to OrdNam1
                move str11 to OrdNam2
                add ORDNAM1 to TOTORDNAM1
                add ORDNAM2 to TOTORDNAM2
                sub ORDNAM1 from OrdNam2,OrdVar
                ADD ORDVAR to TOTORDVAR
                move ORDVAR to str12

                if (holdreport = C3)
                        if (ordvar > low)
		                move str5 to OrdTwo
                		add ORDTWO to TOTORDTWO
		                move str4 to OrdOne
                		add ORDONE to TOTORDONE
		                add  c1 to n9
                                goto reloop
                        else
                                call ADDLOW
                        endif
                endif
                if (holdreport = C4)
                        if (ordvar < high)
		                move str5 to OrdTwo
                		add ORDTWO to TOTORDTWO
		                move str4 to OrdOne
                		add ORDONE to TOTORDONE
		                add  c1 to n9
                                goto reloop
                        else
                                call ADDLOW
                        endif
                endif
	        prtpage prfile;*pColumn:row,*font=font6,*ALIGNMENT=*right,*ll,str45;
                if (ORDVAR = c0)
		        prtpage prfile;*pColumn7:row,*font=font6,*ALIGNMENT=*right,*ll,"0";
                else
	                if (ORDVAR < c0)
        	               	call removechar using str12,dash
        		        call FormatNumeric using str12,PRNTV,comma
                        	pack NegPrV,LPAREN,prntv,RPAREN
			        prtpage prfile;*pColumn7:row,*font=font6,*ALIGNMENT=*right,*ll,NegPrV;
			else
      	        		call FormatNumeric using str12,PRNTV,comma
	        		prtpage prfile;*pColumn7:row,*font=font6,*ALIGNMENT=*right,*ll,PrntV;
	                endif
                endif
.YR1
                move str4 to OrdOne
                add ORDONE to TOTORDONE
	        prtpage prfile;*pColumn3:row,*font=font6,*ALIGNMENT=*Left,*ll,Str4;
                call trim using str10
          	call FormatNumeric using str10,PRNTV,comma
		prtpage prfile;*pColumn4:row,*font=font6,*ALIGNMENT=*right,*ll,prntV;
.YR2
                move str5 to OrdTwo
                add ORDTWO to TOTORDTWO
	        prtpage prfile;*pColumn5:row,*font=font6,*ALIGNMENT=*Left,*ll,Str5;
                call trim using str11
          	call FormatNumeric using str11,PRNTV,comma
		prtpage prfile;*pColumn6:row,*font=font6,*ALIGNMENT=*right,*ll,prntV;
	        add     "100",row
	        add     eightlpi,row
        	if (ROW >= "10000")
                        call Footer2
                        add c1 to n9
                	goto page2
                endif
                add  c1 to n9
        repeat
        if (ROW < "10000")
		goto totals2
        else
               call footer2
               move c1 to newpg
               add  c1 to pgcnt
               prtpage prfile;*NEWPAGE:
                              *UNITS=*HIENGLISH:
                       	      *ORIENT=*PORTRAIT;
               goto Totals2
	endif



TOTALS2
       if (newpg = c1)
       	       move "520",row
       endif
               add     eightlpi,row
               add     "50" to row
               IF (holdreport > c2)
               	       prtpage prfile;*pColumn:row,*font=font6,*ALIGNMENT=*Left,*boldon,*ULON,"Variance Totals",*boldoff,*ULOFF;
.SUBTotal Orders First
	               move ORD1SUB to prntv
	               call FormatNumeric using prntv,negprv,comma
	               move negprv,prntv
	               prtpage prfile;*pcolumn3:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.SUBTotal Names First
	               move SUBOLD to prntv
	               call FormatNumeric using prntv,negprv,comma
	               move negprv,prntv
	               prtpage prfile;*pcolumn4:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.SUBTota1 Orders Last
	               move ORD2SUB to prntv
	               call FormatNumeric using prntv,negprv,comma
	               move negprv,prntv
	               prtpage prfile;*pcolumn5:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.SUBTotal Names Last
	               move SUBNEW to prntv
	               call FormatNumeric using prntv,negprv,comma
	               move negprv,prntv
	               prtpage prfile;*pcolumn6:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.SUBTotal Name Variance
	               move OTOTSUB to prntv
	               call removechar using prntv,dash
	               call FormatNumeric using prntv,negprv,comma
	               move negprv to prntv
	               if (OTOTSUB = c0)
			 	prtpage prfile;*pcolumn7:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,"0",*boldoff;
               else
               		if (OTOTSUB < c0)
                        	pack NegPrV,LPAREN,prntv,RPAREN
	 	   		prtpage prfile;*pcolumn7:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,negprv,*boldoff;
               		else
	 	   		prtpage prfile;*pcolumn7:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntv,*boldoff;
               		endif
               endif
        	       add     eightlpi,row
	               add     "50" to row
               endif
               prtpage prfile;*pColumn:row,*font=font6,*ALIGNMENT=*Left,*boldon,*ULON,"Totals",*boldoff,*ULOFF;
.Total Orders First
               move TOTORDONE to prntv
               call FormatNumeric using prntv,negprv,comma
               move negprv,prntv
               prtpage prfile;*pcolumn3:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.Total Names First
               move TOTORDNAM1 to prntv
               call FormatNumeric using prntv,negprv,comma
               move negprv,prntv
               prtpage prfile;*pcolumn4:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.Tota1 Orders Last
               move TOTORDTWO to prntv
               call FormatNumeric using prntv,negprv,comma
               move negprv,prntv
               prtpage prfile;*pcolumn5:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.Total Names Last
               move TOTORDNAM2 to prntv
               call FormatNumeric using prntv,negprv,comma
               move negprv,prntv
               prtpage prfile;*pcolumn6:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntV,*boldoff;
.Total Name Variance
               move TOTORDVAR to prntv
               call removechar using prntv,dash
               call FormatNumeric using prntv,negprv,comma
               move negprv to prntv
               if (TOTORDVAR = c0)
		 	prtpage prfile;*pcolumn7:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,"0",*boldoff;
               else 
               		if (TOTORDVAR < c0)
                        	pack NegPrV,LPAREN,prntv,RPAREN
.               	  		pack negprv,dash,prntv
	 	   		prtpage prfile;*pcolumn7:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,negprv,*boldoff;
               		else
	 	   		prtpage prfile;*pcolumn7:row,*font=font6,*ALIGNMENT=*Right,*boldon,*ll,prntv,*boldoff;
               		endif
               endif
               call footer2
COPIES
        PRTCLOSE prfile
        if (printarea=c3)
//pdf document	        
		pack	APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
		loop
			call	FindFirstFile
		until (APIResult = 0 | APIResult = hexeight)
			pause	"1"
		repeat
		pause	"2"
		erase	"c:\progra~1\pdf995\flag.dat"	 
		move      c1 to nusepath
		clock	port to str3
		unpack     str3 into str2,str1
		pack       str3 from str1,str2
		MOVE      str3 TO NUSEFLD .removed FOR TESTING only
		REP       ZFILL IN NUSEFLD
		CALL      NUSEKEY
		goto      userng if over
		scan      "INVALID" in nuseuser
		goto      userng if equal
		squeeze	 nuseuser,nuseuser
		pack str55,PRTITLE,".pdf"
		move    "Here is your variance report.",SmtpSubject Subject
;.   Set the text message that is send with the attachments
		move    str55,SmtpTextMessage(1)   Array <Text message >
		move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
		move    "NTS4",SmtpEmailServer                   Address of email serverc
		clear   smtpemailaddress
		append  nuseuser,SmtpEmailAddress
		append  "@nincal.com",SmtpEmailAddress
		reset   smtpemailaddress
		move    nuseuser,SmtpUserName                                User name
		move    nuseuser,SmtpUserFullName                                User Full name
;   Set the destinations of the email. Max 100 (Mime spec)
		move    smtpemailaddress,SmtpDestinations(1,1)
		move    nuseuser,SmtpDestinations(1,2)
		move    "1",SmtpDestIndexLast                          originators UserName
		move    str55,SmtpAttachments(1,1)                     Attached file name
		move    "c:\work\pdf",SmtpAttachments(1,2)           Path to attached file name
		move    "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
		clear   SmtpLogFile                                         'Clear' disables the LogFile
		move    "1",SmtpProgress                                    Enable progress bars
		call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
		if not equal
			pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
			"Status Code ",SmtpStatus," - ",SmtpStatusText
			move    "PDF File not found",SmtpSubject Subject
			move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
			call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
		ENDIF
	        clear   APIFileName
	        pack    APIFileName,PRTFILE1,hexzero
	        call    DeleteFile
.	        if (APIResult = 0 | APIResult = hexeight)
	
.	        endif		
	ENDIF
        if ((holdreport > c2) and (holdreport < c5))
                match DONE,NO
                goto sorter if equal
                if (DITEM > c1)
                        if (DITEM = CITEM)
                        	goto test
                        endif
                        goto SORTER
	        endif
                goto  test
        else
                if (DITEM > c1)
                        if (DITEM = CITEM)
                        	goto test
                        endif
                        goto SORTER
	        endif
                goto  test
        endif
Test
        close   exclude
        close   Orderfile
        close   INVOICEFILE
        erase   "c:\work\Nordvar.dat"
        erase   "c:\work\NINDATE.SRT"
        erase   "c:\work\DATAEXCL.SRT"
        move	c0 to Exflg
        call    enable
        call	OrderSetMouseFree
.Patch1.2
.        call click_NVAR0001Exit
        winshow              
        CHAIN     "NVAR0001" 
        stop 
.Endpatch1.2
        return
GETINFO
                call clearlvars
                call clearnvars
        	NVAR0001ListView.GetItemText giving STR45 using n9,0
        	NVAR0001ListView.GetItemText giving str7  using n9,1
        	NVAR0001ListView.GetItemText giving str4  using n9,2
        	NVAR0001ListView.GetItemText giving STR10 using n9,3
        	NVAR0001ListView.GetItemText giving STR5  using n9,4
        	NVAR0001ListView.GetItemText giving STR11 using n9,5
        	NVAR0001ListView.GetItemText giving STR12 using n9,6
        	NVAR0001ListView.GetItemText giving STR13 using n9,7
        	NVAR0001ListView.GetItemText giving STR14 using n9,8
        	NVAR0001ListView.GetItemText giving STR15 using n9,9
        	NVAR0001ListView.GetItemText giving STR16 using n9,10
        	NVAR0001ListView.GetItemText giving STR17 using n9,11
        	NVAR0001ListView.GetItemText giving STR18 using n9,12
		if (holdreport = c5)        	
	        	NVAR0001ListView.GetItemText giving STR19 using n9,13
	        	NVAR0001ListView.GetItemText giving STR20 using n9,14
	        	NVAR0001ListView.GetItemText giving STR21 using n9,15        	
	        	NVAR0001ListView.GetItemText giving STR22 using n9,16
	        	NVAR0001ListView.GetItemText giving STR23 using n9,17
	        	NVAR0001ListView.GetItemText giving STR24 using n9,18        	
		endif        	
                return

SETINFO
.        	NVAR0001ListView.setitemText giving STR45 using n9,0
        	NVAR0001ListView.setitemText giving N8 using n7,str7,1
                call trim using str4
        	NVAR0001ListView.setitemText giving N8 using n7,str4,2
        	NVAR0001ListView.setitemText giving N8 using n7,STR10,3
                call trim using str5
        	NVAR0001ListView.setitemText giving N8 using n7,STR5,4
        	NVAR0001ListView.setitemText giving N8 using n7,STR11,5
        	NVAR0001ListView.setitemText giving N8 using n7,STR12,6
        	NVAR0001ListView.setitemText giving N8 using n7,STR13,7
        	NVAR0001ListView.setitemText giving N8 using n7,STR14,8
        	NVAR0001ListView.setitemText giving N8 using n7,STR15,9
        	NVAR0001ListView.setitemText giving N8 using n7,STR16,10
        	NVAR0001ListView.setitemText giving N8 using n7,STR17,11
        	NVAR0001ListView.setitemText giving N8 using n7,STR18,12
		if (holdreport = c5)        	        	
	        	NVAR0001ListView.setitemText giving N8 using n7,STR19,13
	        	NVAR0001ListView.setitemText giving N8 using n7,STR20,14
	        	NVAR0001ListView.setitemText giving N8 using n7,STR21,15        	
	        	NVAR0001ListView.setitemText giving N8 using n7,STR22,16
	        	NVAR0001ListView.setitemText giving N8 using n7,STR23,17
	        	NVAR0001ListView.setitemText giving N8 using n7,STR24,18        	
	        endif
        	
                return


Alerter
        alert   caution,"Must be a Valid 8 digit field",n2,"Invalid Date"
	call OrderSetMouseFree
        move c0 to Exflg
        return
Disable
.DISABLES OBJECTS
	SETPROP NVAR0001EditList,ENABLED=C0
	SETPROP NVAR0001EditBegDate1,ENABLED=C0
	SETPROP NVAR0001EditEndDate1,ENABLED=C0
	SETPROP NVAR0001EditBegDate2,ENABLED=C0
	SETPROP NVAR0001EditEndDate2,ENABLED=C0
	SETPROP NVAR0001EditCopies,ENABLED=C0
	SETPROP NVAR0001DataList,ENABLED=C0
	SETPROP NVAR0001Remove,ENABLED=C0
	SETPROP NVAR0001Add,ENABLED=C0
	SETPROP NVAR0001EditTitle1,ENABLED=C0
	SETPROP NVAR0001EditTitle2,ENABLED=C0
	setprop ComboBox001,Enabled=c0
	setprop ComboBox002,Enabled=c0
	setprop Nvar0001ComboBoxPrint,Enabled=c0	
	setprop NVAR0001CheckCustom,Enabled=c0		

        RETURN

ENABLE
.ENABLES OBJECTS
	SETPROP NVAR0001EditList,ENABLED=C1
	SETPROP NVAR0001EditBegDate1,ENABLED=C1
	SETPROP NVAR0001EditEndDate1,ENABLED=C1
	SETPROP NVAR0001EditBegDate2,ENABLED=C1
	SETPROP NVAR0001EditEndDate2,ENABLED=C1
	SETPROP NVAR0001ListView,ENABLED=C1
	SETPROP NVAR0001EditCopies,ENABLED=C1
	SETPROP NVAR0001DataList,ENABLED=C1
	SETPROP NVAR0001DataList,ENABLED=C1
	SETPROP NVAR0001Remove,ENABLED=C1
	SETPROP NVAR0001Add,ENABLED=C1
	SETPROP NVAR0001EditTitle1,ENABLED=C1
	SETPROP NVAR0001EditTitle2,ENABLED=C1
	setprop ComboBox001,Enabled=c1
	setprop ComboBox002,Enabled=c1	
	setprop Nvar0001ComboBoxPrint,Enabled=c1	
	setprop NVAR0001CheckCustom,Enabled=c1
        RETURN

AddList

 	getitem NVAR0001DataList,c1,n2
 	getitem NVAR0001EditList,0,str6
        call zfillit using str6
 	if (str6 = "000000")
        	alert caution,"Item added to list cannot be blank. Please try again",result
                setfocus NVAR0001EditList
                move c0 to Exflg
        	return
        endif
	move str6 to ndatfld
	call ndatkey
	if over
	         alert   caution,"Invalid List '#'. Please try again",n2,"Incorrect List Number"
		 setitem  NVAR0001StatListName,0,""
	 	 setfocus Nvar0001EditList
                 move c0 to Exflg
		 return
	endif
       	setitem  NVAR0001EditList,0,ndatfld
	setitem  NVAR0001StatListName,0,olstname

       	insertitem NVAR0001DataList,9999,ndatfld
       	setfocus NVAR0001EditList
        move c0 to Exflg
       	return

DelList

 	getitem NVAR0001DataList,0,n2
 	if (n2=c0)
 		alert type=yesno1," Do you want to delete all of the entries in the datalist?", result
			if (result=6)    . 6 = yes , 7 = no
                		deleteitem NVAR0001DataList,n2
		                move c0 to Exflg
                        	return
                        else
                 		alert note,"Select a list in the DataList, then Remove.",result
		                move c0 to Exflg
                                return
			endif
 	endif

 	deleteitem NVAR0001DataList,n2
        move c0 to Exflg
 	return

ADDLOW
        move str4 to OrdOne
        add ORDONE to ORD1SUB
        move str5 to OrdTwo
        add ORDTWO to ORD2SUB
        add ORDNAM1 to SUBOLD
        add ORDNAM2 to SUBNEW
        sub ORDNAM1 from OrdNam2,OrdVar
        ADD ORDVAR to OTOTSUB
        return
CLEARTOTS
	move c0 to TOTOrdOne
	move c0 to TOTOrdNam1
	move c0 to TOTOrdTwo
	move c0 to TOTOrdNam2
	move c0 to TOTOrdVar
	move c0 to TOTExOne
	move c0 to TOTExTwo
	move c0 to TOTExVar
	move c0 to TOTRnt1
	move c0 to TOTRnt2
	move c0 to TOTRntVar
	move c0 to TOTInc1
	move c0 to TOTInc2
	move c0 to TOTIncVar
	move c0 to TOTRentOrd1	
	move c0 to TOTRentOrd2	
	move c0 to TOTRentOrdVar		
        return
CLEARNVARS
        move c0 to ORDONE
        move c0 to ORDNAM1
        move c0 to Ordtwo
        move c0 to OrdNam2
        move c0 to OrdVar
        move c0 to ExOne
        move c0 to ExTwo
        move c0 to ExVar
        move c0 to Rnt1
        move c0 to Rnt2
        move c0 to RntVar
        move c0 to Inc1
        move c0 to Inc2
        move c0 to IncVar        
	move c0 to RentOrd1	
	move c0 to RentOrd2	
	move c0 to RentOrdVar        
        move c0 to tmpvar
        move c0 to EXSPL
        return
CLEARLVARS
        move c0 to str4
        move c0 to str10
        move c0 to str5
        move c0 to str11
        move c0 to str12
        move c0 to str13
        move c0 to str14
        move c0 to str15
        move c0 to str16
        move c0 to str17
        move c0 to str18
        move c0 to str19
        move c0 to str20
	move c0 to str21        
	move c0 to str22
	move c0 to str23	
        return
CLEARSUBS
        move c0 to SUBOLD
        move c0 to SUBNEW
        move c0 to ORD1SUB
        move c0 to ORD2SUB
        move c0 to OTOTSUB 
        return
ClearMisc
        move c0 to pgcnt
        move c0 to newpg
        move c0 to Negprv
        move c0 to Prntv
        move c0 to lst
        clear Done
        return
Footer
        move "7750",row
	prtpage prfile;*pTitle4:row,*font=font8,*ALIGNMENT=*Center,*ll,PgCnt;
.	prtpage prfile;*font=font8,*ALIGNMENT=*Left,*ll,PgCnt;
        prtpage prfile;*pColumn:row,*font=font9,*ALIGNMENT=*Left:
	"©","1995-2015, Names in the News";
        return

Footer2
        move "10200",row
	prtpage prfile;*pTitle3:row,*font=font8,*ALIGNMENT=*CENTER,"Page ";
	prtpage prfile;*font=font8,*ALIGNMENT=*Left,*ll,PgCnt;
        prtpage prfile;*pcolumn8:row,*font=font9,*ALIGNMENT=*RIGHT:
	"©","1995-2015, Names in the News";
.END PATCH 1.36 REPLACED LOGIC
        return


ClearList
        NVAR0001ListView.deleteallitems
	return

.===============================================================================
FileGo
.Flag set to "N" if in Modify or New mode
.        branch result to FileGo1,FileGo2,FileGo3,FileGo3
        branch result to FileGo1,FileGo2,FileGo3
FileGo1
	call click_NVAR0001Run
        return
FileGo2
        RETURN
FileGo3
        call click_NVAR0001Exit
        RETURN
Optionsgo
        return
ViewGo
        return
EditGo
        return
HelpGo
        setprop AboutMssg,visible=1
        return
SearchGo
        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4
SearchGo1
.BROKER
.        move    C1,SrchFlag
.        call    SearchSetTitle
.        call    SearchSetVisible
        return
SearchGo2        
.LIST
        move    C2,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo3        
.MAILER
.        move    C3,SrchFlag
.        call    SearchSetTitle
.        call    SearchSetVisible
        return
SearchGo4
.SHIP-TO
.        move    C4,SrchFlag
.        call    SearchSetTitle
.        call    SearchSetVisible
        return
SearchLoad
.Called by SearchDataList_DoubleClick
.Only load if not in Inquiry mode
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4
SearchLoad1
.BROKER - not an option with this program
        return
SearchLoad2
.LIST
        unpack srchstr,str6,str1,str35,str1,str10
        setitem NVAR0001EditList,0,str6
        setitem NVAR0001StatListName,0,str35
.        setitem NrcoStatRevDate,0,str10
        setfocus NVAR0001EditList
        move c0 to Exflg
        return
SearchLoad3
.MAILER
.        unpack  Srchstr,str4,str1,str3,str1,str45
.        setitem NrcoMlrNo,0,str4
.        setitem NrcoEditMlrNme,0,str45
.        setfocus NrcoMlrNo
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
................................................................
.===============================================================================
OrderSetMouseBusy
        setmode *mcursor=*wait
        return
OrderSetMouseFree
        setmode *mcursor=*arrow
        return
VarianceCount
        clear n9
        NVAR0001ListView.GetItemCount giving result
        sub c1 from result
        loop
        	until (N9 > result)
                call getinfo
.Total Names
.Variance
;	        prtpage prfile;*pColumn:row,*font=font6,*ALIGNMENT=*right,*ll,str45;
                move str10 to OrdNam1
                move str11 to OrdNam2
                sub ORDNAM1 from OrdNam2,OrdVar
                add "100000000",ordvar
                move ordvar to dim13a
;patch1.34
;                    move mask13 to dim13a
;                    edit ordvar to dim13a
;                    call trim using dim13a
                    NVAR0001ListView.setitemText giving n8 using n9,dim13a,6
;patch1.34
.Exchange Names
.Variance
                move str13 to EXONE
                move str14 to EXTWO
                sub EXONE from EXTWO,EXVAR
;patch1.34
                add "100000000",exvar
                move exvar to dim13a
;                    move mask13 to dim13a
;                    edit exvar to dim13a
;                    call trim using dim13a
                NVAR0001ListView.setitemText giving n8 using n9,dim13a,9
;patch1.34
.Rental Names
.Variance
                move str16 to RNT1
                move str17 to RNT2
                sub RNT1 from RNT2,RNTVAR
;patch1.34
                add "100000000",rntvar
                move rntvar to dim13a
;                    move mask13 to dim13a
;                    edit rntvar to dim13a
;                    call trim using dim13a
                    NVAR0001ListView.setitemText giving n8 using n9,dim13a,12
;patch1.34
.Income
.Variance
                move str19 to INC1
                move str20 to INC2
                sub INC1 from INC2,INCVAR
;patch1.34
                add "100000000",INCvar
                move INCvar to dim13a
;                    move mask13 to dim13a
;                    edit rntvar to dim13a
;                    call trim using dim13a
                    NVAR0001ListView.setitemText giving n8 using n9,dim13a,15
;patch1.34
.Rental Order Variance
.Income
.Variance
                move str22 to RentOrd1
                move str23 to RentOrd2
                sub RentOrd1 from RentOrd2,RentOrdVAR
;patch1.34
                add "100000000",RentOrdVar
                move RentOrdVar to dim13a
;                    move mask13 to dim13a
;                    edit rntvar to dim13a
;                    call trim using dim13a
                    NVAR0001ListView.setitemText giving n8 using n9,dim13a,18
;patch1.34
                add c1 to n9
       repeat  
       
sortdb
	if (holdreport <> c5)
                if (columnvar = c1)
                         if (sortvar = c1)
                            NVAR0001ListView.SortColumn using *Column=6,*Type=3
                         else 
                            NVAR0001ListView.SortColumn using *Column=6,*Type=4        
                         endif
                elseif (columnvar = c2)
                         if (sortvar = c1)
                            NVAR0001ListView.SortColumn using *Column=9,*Type=3
                         else 
                            NVAR0001ListView.SortColumn using *Column=9,*Type=4        
                         endif
                elseif (columnvar = c3)
                         if (sortvar = c1)
                            NVAR0001ListView.SortColumn using *Column=12,*Type=3
                         else 
                            NVAR0001ListView.SortColumn using *Column=12,*Type=4        
                         endif
                elseif (columnvar = c0)
;Patch 1.39                
                         if (sortvar = c2)
                            NVAR0001ListView.SortColumn using *Column=0,*Type=2
                         else 
                            NVAR0001ListView.SortColumn using *Column=0,*Type=1        
                         endif
.                         if (sortvar = c1)
.                            NVAR0001ListView.SortColumn using *Column=0,*Type=1
.                         else 
.                            NVAR0001ListView.SortColumn using *Column=0,*Type=2        
.                         endif
;Patch 1.39                         
                endif
       else
                if (columnvar = c1)
                         if (sortvar = c1)
                            NVAR0001ListView.SortColumn using *Column=15,*Type=3
                         else 
                            NVAR0001ListView.SortColumn using *Column=15,*Type=4        
                         endif
                elseif (columnvar = c2)
                         if (sortvar = c1)
                            NVAR0001ListView.SortColumn using *Column=6,*Type=3
                         else 
                            NVAR0001ListView.SortColumn using *Column=6,*Type=4        
                         endif
                elseif (columnvar = c3)
                         if (sortvar = c1)
                            NVAR0001ListView.SortColumn using *Column=0,*Type=2
                         else 
                            NVAR0001ListView.SortColumn using *Column=0,*Type=1        
                         endif  
                endif
       endif
       
                goto startprint
       return
SortBox
        call    Report2DestroyObjects
        setprop report2,title="Sort Order"
        create  report2;Radios(1)=70:85:10:180,"Order Variance",groupid=10,objectid=1
        create  report2;Radios(2)=90:105:10:180,"Exchange Variance",groupid=10,objectid=2
        create  report2;Radios(3)=110:125:10:180,"Rental Variance",groupid=10,objectid=3
        create  report2;Radios(4)=130:145:10:180,"Default (Mailer Name)",groupid=10,objectid=0
        create  report2;Radios(5)=70:85:180:350,"Ascending",groupid=20,objectid=1
        create  report2;Radios(6)=90:105:180:350,"Descending",groupid=20,objectid=2
        activate Radios(1)
        activate Radios(2)
        activate Radios(3)
        activate Radios(4)
        activate Radios(5)
        activate Radios(6)
        create  report2;Button(2)=180:205:50:100,"&OK",default=1
        listins  ObjectColl,radios(1),radios(2),radios(3),radios(4),radios(5),radios(6),button(1),button(2)
        activate Button(2),GetSortInfo,result
        setitem radios(4),n1,c1       
        setitem radios(5),n1,c1 
        call ordersetmousefree
        setprop  report2,visible=1
GetsortInfo
        call ordersetmousebusy
        return If (rptcan = YES)
        getprop Radios(1),SELGROUPID=columnvar
        getprop Radios(5),SELGROUPID=sortvar
        setprop  report2,visible=0
        return
SortBox2
        call    Report2DestroyObjects
        setprop report2,title="Sort Order"
        create  report2;Radios(1)=70:85:10:180,"Income Variance(Default)",groupid=10,objectid=1
        create  report2;Radios(2)=90:105:10:180,"Order Variance",groupid=10,objectid=2
        create  report2;Radios(3)=110:125:10:180,"Mailer",groupid=10,objectid=3
;        create  report2;Radios(4)=130:145:10:180,"",groupid=10,objectid=0
        create  report2;Radios(5)=70:85:180:350,"Ascending",groupid=20,objectid=1
        create  report2;Radios(6)=90:105:180:350,"Descending",groupid=20,objectid=2
        activate Radios(1)
        activate Radios(2)
        activate Radios(3)
;        activate Radios(4)
        activate Radios(5)
        activate Radios(6)
        create  report2;Button(2)=180:205:50:100,"&OK",default=1
.        listins  ObjectColl,radios(1),radios(2),radios(3),radios(4),radios(5),radios(6),button(1),button(2)
        listins  ObjectColl,radios(1),radios(2),radios(3),radios(4),radios(5),radios(6),button(1),button(2)
        activate Button(2),GetSortInfo2,result
        setitem radios(1),n1,c1       
        setitem radios(5),n1,c1 
        call ordersetmousefree
        setprop  report2,visible=1
GetsortInfo2
        call ordersetmousebusy
        return If (rptcan = YES)
        getprop Radios(1),SELGROUPID=columnvar
        getprop Radios(5),SELGROUPID=sortvar
        setprop  report2,visible=0
        return        
check

        return
Report2DestroyObjects
        destroy ObjectColl      
        return
TOME1
	setprop process,visible=0
	destroy timer1
	return
.Headers
.Defining Header and Titles	
RentExchangeVarianceHeader
        clear   row
        move    "200",row
.        prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,"Confidential";
.Getting the Logo to fit in this small amount of space was a bear!!  Be careful if you decide to modify.
	prtpage	prfile;*units=*HIENGLISH,*Pictrect=*on,*PICT=200:650:200:1200:NINLogo;        
.        prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,"Confidential";
        prtpage prfile;*pcolumn10:row,*ALIGNMENT=*LEFT,*font=font8,"Date:";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*font=font8,str10;
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"VARIANCE REPORT";
        add     eightlpi,row
        add     "30",row
        move    olstname,str35
        call trim using str35
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,str35;
        add     eightlpi,row
        add     "30",row
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font8,*ll,str60;
        add     eightlpi,row   
        add     "100",row         
       	prtpage prfile;*pColumn:row,*pensize=20,*line=Column11:row;        
        add     "50",row
	prtpage prfile;*pColumn3:row,*font=font8,*ALIGNMENT=*Center,*boldon,"TOTAL NAMES";
        prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*Center,*boldon,"EXCHANGE NAMES";
        prtpage prfile;*pColumn10:row,*font=font8,*ALIGNMENT=*Center,*boldon,"RENTAL NAMES",*boldoff;
        add     eightlpi,row
	prtpage prfile;*pColumn:row,*font=font8,*ALIGNMENT=*left,*ll,"Mailer";        
        prtpage prfile;*p2440:row,*font=font8,*ALIGNMENT=*right,*ll,"Orders";
        prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,TITYR1;
        prtpage prfile;*p3650:row,*font=font8,*ALIGNMENT=*right,*ll,"Orders";
        prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,TITYR2;
        prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,"Variance";
        prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,TITYR1;
        prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,TITYR2;
        prtpage prfile;*pColumn8:row,*font=font8,*ALIGNMENT=*right,*ll,"Variance";
        prtpage prfile;*pColumn9:row,*font=font8,*ALIGNMENT=*right,*ll,TITYR1;
        prtpage prfile;*pColumn10:row,*font=font8,*ALIGNMENT=*right,*ll,TITYR2;
        prtpage prfile;*pColumn11:row,*font=font8,*ALIGNMENT=*right,*ll,"Variance";
        add     eightlpi,row
        add     "50",row        
       	prtpage prfile;*pColumn:row,*pensize=20,*line=Column11:row;                
        add     eightlpi,row       	
        add     "100",row
	return
VarianceTotalsHeader
        clear     row
        move      "200",row
        prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,"Confidential";
        prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font8,"Date:";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*font=font8,str10;
        prtpage prfile;*pTITLE3:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"LIST OWNER VARIANCE REPORT";
        add     eightlpi,row
        add     "30",row
        move    olstname,str35
        call    trim using str35
        prtpage prfile;*pTITLE3:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,str35;
        add     eightlpi,row
        add     "30",row
        prtpage prfile;*pTITLE3:row,*ALIGNMENT=*CENTER,*font=font8,*ll,str60;
        add     eightlpi,row        
        add     eightlpi,row
        add     "30",row
	prtpage prfile;*pcolumn:row,*font=font8,*ALIGNMENT=*right,*boldon,*ULON,*ll,"Mailer",*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pColumn3:row,*font=font8,*ALIGNMENT=*right,*ll,*boldon,*ULON,"No.",*ULOFF,*boldoff;
        prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,*boldon,*ULON,TITYR1,*ULOFF,*boldoff;
        prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,*boldon,*ULON,"No.",*ULOFF,*boldoff;
        prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,*boldon,*ULON,TITYR2,*ULOFF,*boldoff;
        prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,*boldon,*ULON,"Variance",*ULOFF,*boldoff;
        add     eightlpi,row
        add     "100",row
	return
IncomeVarianceHeader
        clear   row
        move    "200",row
.        prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8,"Confidential";
.Getting the Logo to fit in this small amount of space was a bear!!  Be careful if you decide to modify.
	prtpage	prfile;*units=*HIENGLISH,*Pictrect=*on,*PICT=200:650:200:1200:NINLogo;
	
        prtpage prfile;*pcolumn10:row,*ALIGNMENT=*LEFT,*font=font8,"Date:";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*font=font8,str10;
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"INCOME VARIANCE REPORT";
        add     eightlpi,row
        add     "30",row
        move    olstname,str35
        call trim using str35
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,str35;
        add     eightlpi,row
        add     "30",row
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font8,*ll,str60;
        add     eightlpi,row   
        add     "100",row         
       	prtpage prfile;*pColumn:row,*pensize=20,*line=Column9:row;        
        add     "50",row
	prtpage prfile;*pColumn3:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,TITYR1,*boldoff;
        prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*Right,*boldon,TITYR2,*boldoff;
        prtpage prfile;*pColumn8:row,*font=font8,*ALIGNMENT=*Right,*ll,*boldon,"Variance",*boldoff;
        add     eightlpi,row
	prtpage prfile;*pColumn:row,*font=font8,*ALIGNMENT=*Left,*ll,"Mailer";        
        prtpage prfile;*pColumn1:row,*font=font8,*ALIGNMENT=*right,*ll,"Orders";
        prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,"Quantity";
        prtpage prfile;*pColumn3:row,*font=font8,*ALIGNMENT=*right,*ll,"Income";
        prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,"Orders";
        prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,"Quantity";
        prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,"Income";
        prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,"Orders";
        prtpage prfile;*pColumn8:row,*font=font8,*ALIGNMENT=*right,*ll,"Quantity";
        prtpage prfile;*pColumn9:row,*font=font8,*ALIGNMENT=*right,*ll,"Income";
        add     eightlpi,row
        add     "50",row        
       	prtpage prfile;*pColumn:row,*pensize=20,*line=Column9:row;                
        add     eightlpi,row       	
        add     "100",row
	return
GetHeaderDates
.Packing of date Header
.Should read mm\dd\ccyy versus mm\dd\ccyy
        getitem NVAR0001EditBegDate1,0,str10
        move str10 to str30
        getitem NVAR0001EditEndDate1,0,str10
        move str10 to str24
        getitem NVAR0001EditBegDate2,0,str10
        move str10 to str25
        getitem NVAR0001EditEndDate2,0,str10
        move str10 to str29
        pack    str60,str30,b1,dash,b1,str24," versus ",str25,b1,dash,b1,str29,lparen,DateType,"D",rparen
        if (custom = c1)
                getitem NVAR0001EditTitle1,0,TITYR1
                getitem NVAR0001EditTitle2,0,TITYR2
        else
	        unpack  str24,b6,TITYR1
        	unpack  str29,b6,TITYR2
        endif
	return
Spool1
         move error to str9
;         alert caution,str9,result
;added for plbserver runs does not like "\\nts0\laser?"         gets s10 if s10 try this
         TRAPCLR   SPOOL
         Trap SPOOL2 giving error if SPOOL
         SCAN      "S10" IN ERROR
         if equal
.        	PRTOPEN prfile,"PDF995",PRTITLE,noprint,spoolfile=PRTFILE1
       		PRTOPEN prfile,PrinterName,PRTITLE
         endif
         return
SPOOL2
         move error to str9
         alert caution,str9,result,"No Connection. Please Select Another Printer."
       	 PRTOPEN prfile,"",PRTITLE         
         NORETURN
         shutdown  "cls"
         STOP
userng
	clear	taskname
	append	"I'm sorry I've lost track of who you are,",taskname
	append	NewLine,taskname
	append	"Please leave the program and try again!",taskname
	reset	taskname
	alert	caution,taskname,result
	shutdown
	stop
.Event to stop file read for report	
CheckStop
   	eventcheck
	if (stopflag = YES)
        	close   exclude
        	close   Orderfile
        	close   INVOICEFILE        	
        	erase   "c:\work\Nordvar.dat"
.        	if ((holdreport > c2) & (holdreport > c5))
	        erase   "c:\work\NINDATE.SRT"
	        erase   "c:\work\DATAEXCL.SRT"
.        	endif
                winshow              
                CHAIN     "NVAR0001" 
                stop 
        endif 	
.ADDED FOR SEARCH.PLF............................................
        include nrtnio.inc
        include searchio.inc      .contains logic for search.plf
        include ncmpio.inc
        include ncntio.inc
.patch1.35
			include	compio.inc
			include	cntio.inc
.        include NBRKIO.INC
.        include NMLRIO.INC
.patch1.35
        include NDATIO.INC
**************************************
.for testing speed
.        include  nord2io.inc
*************************************
         INCLUDE   PORTCALC.inc
         include   nuseio.inc
	include NORDIO.INC
        include nownio.inc
        include  ninvio.inc        
        include COMLOGIC.INC

