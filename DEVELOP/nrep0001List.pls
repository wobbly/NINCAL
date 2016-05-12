PC	EQU	0
........................................
. Program:      NREP0001.PLS
. Function:     Creates an Excel Spreadsheet with a summary of a cilent's usage in a given time period also shows the broker.
.               It also creates a secondary report with broker totals on a different sheet.
. Author:       David Baca
. Date:         September 12,2006
. Release:      1.0
. Notes:	This is a standalone program.
........................................
.
.Include Files
	include 	common.inc
        include 	cons.inc
        include		oslspern.inc
       // include		\\nts0\c\library\develop\adjacencysourcecode\src\user.io
	//Following DD is not used, but needed to include user.io
	//include		\\nts0\c\library\develop\adjacencysourcecode\src\guidgen.inc
	include norddd.inc
	include compdd.inc
	include cntdd.inc
	
	include ndatdd.inc
	include nxrfdd.inc
	//include nmlrdd.inc
	//include nbrkdd.inc
OldtoNewMlr external "COMP001A;OldtoNewMlr"	
	
.

Release 	Init	"1.0"
.
OSLS	dim	25
returnValue form 1  // for verifyDate routine in comlogic

ex      	automation      class="Excel.Application"
books   	automation
book    	automation
sheets  	automation
sheet   	automation
sheet2 		automation	// new sheet
range1		automation
sheetindex 	VARIANT
result2 	form	8
N15		form 15
.
brkMlr		dim	10
.brkMlr		dim	12
name	dim	65   // delete later
StartJulDate	form	5
EndJulDate	form	5
ListMFLAG	init 	" "  vs  sales
ORDERDateFLAG	init	" "   vs mail date
tempOQTY	dim	9
tempOEXQTY 	dim	9
RentQty		dim	9
formOQTY	form	9
formOEXQTY	form	9
formRentQty	form	9
totalOrderQty 	form	9
totalOEXQTY	form	9
rentalOrders	form	5
exchangeOrders 	form	5
totalRentQty	form	9
formBrk		form	9
totalBrk	form	10
prevBrk		dim	6

StopFlag 	form    1

.Variant objects used to talk to outside applications
.Booleans
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom85  variant
VT_R8	EQU 5           .Double - 8 byte Real
xlRowHeight	variant
.Formatting vars needed
SheetsDefault	integer 4,"0x00000000"
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlDouble    integer 4, "0x2"
xlAscending integer 4, "0x1"

xlLeft integer 4,"0xffffefDD"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"

KEYPAD2  INIT      "02L"

	create	SheetIndex,VarType=VT_I4   

x	plform		NREP0001
	formload	x
	winhide

.setup listview 
	NREP0001ListView.InsertColumn using "Broker.Mailer",75,0   
	NREP0001ListView.InsertColumn using "Mailer",75,1 // OMLRNUM
	NREP0001ListView.InsertColumn using "Broker",75,2 // OBRKNUM
	NREP0001ListView.InsertColumn using "Salesperson",75,3 //
	NREP0001ListView.InsertColumn using "Order Qty",75,4
	NREP0001ListView.InsertColumn using "Exchange Qty",100,5
	NREP0001ListView.InsertColumn using "Exchange Orders",100,6
	NREP0001ListView.InsertColumn using "Rental Qty",75,7
	NREP0001ListView.InsertColumn using "Rental Orders",75,8
	NREP0001ListView.InsertColumn using "Order Date",75,9
	NREP0001ListView.InsertColumn using "Mail Date",75,10
	NREP0001ListView.InsertColumn using "ComboJul",75,11

	NREP0001ListView.InsertColumn using "Mailer Number",75,12
.
	NREP0001LV2.InsertColumn using "Broker.Mailer",75,0   	
	NREP0001LV2.InsertColumn using "Salesperson",75,1
	NREP0001LV2.InsertColumn using "ComboJul",75,2
	
	loop
		eventwait
	repeat
	
NREP0001ButtonOK
	getprop NREP0001ButtonLM, value=N1
	if (N1= C1)
		move "YES", ListMFlag
	endif
	getprop NREP0001ButtonSales,value=N2
	if (N2= C1)
			move "NO", ListMFlag
	endif
	if (N1 <> C1 AND N2 <> C1)
		alert note, "Click for List Management or Sales report", result
		return
	endif
	getitem NREP0001EditSDate,0,str10
	call	TRIM using str10
	call 	RemoveChar, str10, SLASH
	if (str10 <> "")
		unpack str10,mm,dd,cc,yy
		call	CVTJUL
		move	JULDAYS, StartJulDate	
	else 
		move "00000", StartJulDate
	endif
	getitem NREP0001EditEDate,0,str10
	call	TRIM using str10
	call 	RemoveChar, str10, SLASH
	if (str10 <> "")
		unpack str10,mm,dd,cc,yy
		call	CVTJUL
		move	JULDAYS, EndJulDate	
	else 
		move "99999", EndJulDate
	endif
	getprop NREP0001RadioOrder, value=N1
		if (N1= C1)
			move "YES", OrderDateFlag
	endif
	getprop NREP0001RadioMail, value=N1
		if (N1= C1)
			move "NO", OrderDateFlag
	endif
	
	move	C0,StopFlag

	setprop	NREP0001ButtonStop,enabled=1,height=20
	setprop	NREP0001ButtonOK,enabled=0
	// disable fields, make hourglass appear
	//return
.NOW POPULATE LISTVIEW
FileList File
	Open Filelist,"c:\work\client.csv"



	loop
		Read Filelist,Seq;Str4
	until over
		clear str6
		call zfillit using str4
    		Call OldtoNewMlr using str4,str6
//make list for associated list to company
		move	C2 to NXRFPATH
		move	Str6,NXRFFLD2
		move	"C.LoadList-NXRFKEY",Location
		pack	KeyLocation,"Key: ",NXRFFLD2
		call	NXRFKEY
		if Not over
			loop
.				move	"000000",NXRFLIST
				move	NXRFLIST,NDATFLD
				move	C1,NDATPATH
				move	"C.LoadList-NDATKEY",Location
				pack	KeyLocation,"Key: ",NDATFLD
				call	NDATKEY
				if not over
					CLEAR     NORDFLD2
					CLEAR     NORDFLD1
					CLEAR     NORDFLD3
					clear     nordfld4
.				PACK      NORDFLD1 FROM KEYPAD1,QUES
					PACK      NORDFLD2 FROM KEYPAD2,NDATFLD
					MOVE      C2 TO NORDPATH
					CALL      NORDAIM
					If Not Over
				        	loop
									        	
			        	
			        	
							call oread			        	
							eventcheck
							if (Stopflag = c1)					        	
								stop
							Endif
							call NORDKG
						until over
						until (olnum <> NDATFLD)
						repeat
					Endif
				Endif			
	
			
			
				move	"C.LoadList-NXRFKS",Location
				pack	KeyLocation,"Key: ",NXRFFLD2
				call	NXRFKS
			until over
				match	str6,NXRFMLR
			until not equal		
			repeat
.no list association
		Endif
        repeat		
	
	
	goto excel

	
	
	




	

.	Move "NINORD" to NORDNAME
.	Call Nordseq
.	move C1 to NORDPATH
.	LOOP
OREAD
//	DB added will hopefully eliminate more records
		IF (OSTAT<>"B" AND OSTAT<>"0")	
			return
.			Goto skipRecord
		Endif
//Right Group?
		pack str2,OSALES10,OSALES
		rep zfill, str2
.		if (ListMFlag="Y")
.			if (str2="06")   // list management
.				// do nothing
.			else
.				goto skipRecord
.			endif
.		else	// brokerage
.			if (str2="06")
.				goto skipRecord
.			endif	
.		endif		
//Just check for associated list
.		reset listvar
.		scan olnum in listvar
.		if not equal
.			return
.			goto skiprecord
.		endif
		
//		
		call TRIM using OQTY
		if (OQTY="" OR OQTY="0")
			move "0",tempOQTY
		else 
			move OQTY, tempOQTY
		endif
		move tempOQTY,formOQTY	
		scan OLNUM in RUNCODES
			if equal
				return
.				goto skipRecord
			endif
		if (OrderDateFlag="Y")  // get record order date and convert to jul
			move OODTEC, cc
			move OODTEY, yy
			move OODTEM, mm
			move OODTED, dd
			call	CVTJUL
			if (JULDAYS <= EndJulDate && JULDAYS >= StartJulDate)
				// do nothing
			else
				return
.				goto skipRecord
			endif
		else // mail date selected
			move OMDTEC, cc
			move OMDTEY, yy
			move OMDTEM, mm
			move OMDTED, dd
			call	CVTJUL
			if (JULDAYS <= EndJulDate && JULDAYS >= StartJulDate)
				// do nothing
			else
				return			
.				goto skipRecord
			endif	
		endif

//		if (ORCODE<>"" AND formOQTY <> C0 AND (OSTAT="B" OR OSTAT="0"))
		if (formOQTY <> C0)
			CALL TRIM USING OBRKNUM

.			pack str8,OBRKNUM,OMLRNUM  // broker might be empty
.			pack str10,OLNUM,OMLRNUM  // broker might be empty
			pack str12,OLNUM,NXRFFLD2  // broker might be empty
.			NREP0001ListView.InsertItem giving result using str10
			NREP0001ListView.InsertItem giving result using str12
.			NREP0001ListView.SetItemText using result,OMLRNUM,1 // col2
			NREP0001ListView.SetItemText using result,NXRFFLD2,1 // col2
			
			NREP0001ListView.SetItemText using result,OLNUM,2 // col3
			pack str2,OSALES10,OSALES
			if (str2="06")   // list management
				NREP0001ListView.SetItemText using result,"List Management",3 // col4
			else   //  brokerage salesperson
				pack 	str2 FROM OSALES10,OSALES
				move	C0,N2
				Move	str2,N2
				pack 	OSLS from b10,b10,b5
				move 	OSLS0 to OSLS
				Load	OSLS from N2 of OSLS1,OSLS2,OSLS3,OSLS4,OSLS5,OSLS6,OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13,OSLS14,OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21,OSLS22
				NREP0001ListView.SetItemText using result,OSLS,3 // col4
			endif	
			call TRIM using OEXQTY
			if (OEXQTY="" OR OEXQTY="0")
				move "0",tempOEXQTY
			else
				move OEXQTY,tempOEXQTY
			endif
			move tempOEXQTY,formOEXQTY
			NREP0001ListView.SetItemText using result,OQTY,4 // col5
		//	. now exchange qty, then orders
			reset excodes
			scan OELCODE in excodes
			if equal	// oelcode is 2 or 3
				if (formOEXQTY > C0)  // split  
					sub formOEXQTY,formOQTY
					move formOQTY,RentQty
					NREP0001ListView.SetItemText using result,RentQty,7 // col8
					// RENTAL
					NREP0001ListView.SetItemText using result,tempOEXQTY,5  //exchange
					NREP0001ListView.SetItemText using result,"1",8              // splits considered rentals
				else // all exchange
						NREP0001ListView.SetItemText using result,tempOQTY,5 // col6
						NREP0001ListView.SetItemText using result,"1",6
				endif
			else // all rental
				move tempOQTY,RentQty
				NREP0001ListView.SetItemText using result,RentQty,7 // col8
				NREP0001ListView.SetItemText using result,"1",8
			endif
			move JULDAYS, str5 // records date - either order or mail
			NREP0001ListView.SetItemText using result,str5,11
			if (OrderDateFlag="Y") // order date
				pack str10,OODTEM,"/",OODTED,"/",OODTEC,OODTEY
				NREP0001ListView.SetItemText using result,str10,9
			else	// mail date
				pack str10,OMDTEM,"/",OMDTED,"/",OMDTEC,OMDTEY
				NREP0001ListView.SetItemText using result,str10,10
			endif


		endif
	return
.	
.skipRecord
.		Call Nordseq
.		until over
	
.			eventcheck
.			Until (Stopflag = c1)		
.	repeat	
	If (stopflag = c1)
		stop
	Endif
		
Excel 
.Create the Variant objects
.Booleans
	create  OTRUE,VarType=VT_BOOL,VarValue=1
	create  OFALSE,VarType=VT_BOOL,VarValue=0
	create  Zoom85,VarType=VT_I4,VarValue=1
	create	xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
	create  ex
	setprop ex,*WindowState=xlMinimized
	setprop ex,*Visible="False"
	
//.Reset Default of Worksheets found in a Workbook
        Getprop ex,*SheetsInNewWorkbook=SheetsDefault
        Setprop ex,*SheetsInNewWorkbook=C2
        
.Create Workbooks collection
	getprop ex,*Workbooks=books
.Create/Add a single Workbook
	books.add
	books.item giving book using 1
.Create Sheets collection
	getprop book,*workSheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
	sheets.item giving sheet using 1
        sheets.item giving sheet2 using 2 
.	setprop	sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
.	sheet.range("A1:E1").Merge
	sheet.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,190,60
	Move StartJulDate,juldays	
	Call Cvtgreg
	Pack str10 with mm,slash,dd,slash,cc,yy
	Move EndJulDate,juldays	
	Call Cvtgreg	
	Pack str11 with mm,slash,dd,slash,cc,yy
	Pack taskname,"Date range (",str10," - ",str11,")"
	setprop sheet.range("C4"), *Value=Taskname ,*HorizontalAlignment=xlAlignCenter	
	
.	setprop	sheet2.range("A1:A1").Rows,*RowHeight=xlRowHeight
.	sheet2.range("A1:E1").Merge
	sheet2.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,190,60
	Pack taskname,"Date range (",str10," - ",str11,")"
	setprop sheet2.range("C4"), *Value=Taskname ,*HorizontalAlignment=xlAlignCenter		
	
	
.Column Headers
	setprop sheet2.range("A8"),*Value="Broker",*HorizontalAlignment=xlAlignCenter
	setprop sheet2.range("B8"),*Value="Total",*HorizontalAlignment=xlAlignCenter
.
	setprop sheet.range("A8"), *Value="Salesperson" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("B8"), *Value="Broker" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("C8"), *Value="Client" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("D8"), *Value="Orders" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("E8"), *Value="Quantity" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("F8"), *Value="Exchange Orders" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("G8"), *Value="Exchange Quantity" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("H8"), *Value="Rental Orders" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("I8"), *Value="Rental Quantity" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("J8"), *Value="Mailer Number" ,*HorizontalAlignment=xlAlignCenter	
	setprop sheet.range("K8"), *Value="Broker Number" ,*HorizontalAlignment=xlAlignCenter	
.
	sheet2.range("A8:B8").BorderAround using *LineStyle=1,*Weight=3
	setprop sheet2.range("A8:B8").Font,*Name="Arial", *Size=10
	setprop sheet2.range("A8:B8").Font,*Bold="True"
.
	sheet.range("A8:I8").BorderAround using *LineStyle=1,*Weight=3
	setprop sheet.range("A8:I8").Font,*Name="Arial", *Size=10
	setprop sheet.range("A8:I8").Font,*Bold="True"
	move C10, N5 // for excel insertions, book	
.
	move C0, totalOrderQty
	move C0, totalOEXQTY
	move C0, rentalOrders
	move c0, totalRentQty
	move C0, exchangeOrders
	move seq,result
	move result,n9
	move "begin", brkMlr
.  now do sort on listview
	NREP0001ListView.SortColumn using 0,11  // case insensitive, alpha asc. sort on broker/mailer
. now loop and take totals
	//NREP0001ListView.GetNextItem giving result using C0, N9 // get 1st item in LV
	
	//NREP0001ListView.GetItemText giving str8 using result,0 // BRKMAILER IN STR8
	loop
		move result,n9
DAVE		NREP0001ListView.GetNextItem giving result using 0, N9 // get 1st item in LV
		until (result=seq)
		NREP0001ListView.GetItemText giving str12 using result,0  // str30 is most recent salesperson
		if (str12=brkMlr OR brkMlr="begin")  // same brkmailer combo
			// fall through	
		else
			// switch
			// sort new listview and get salesperson
			NREP0001LV2.SortColumn using 0,2  // sort on date
			NREP0001LV2.GetNextItem giving result2 using C0 // get 1st item in LV
			NREP0001LV2.GetItemText giving str30 using result2,1 // most recent salesperson

			call goExcel
			//alert note,"switch", N1
			NREP0001LV2.deleteallitems
			//move C0, totalOrderQty
			move C0, totalOEXQTY
			move C0, rentalOrders
			move c0, totalRentQty
			move C0, exchangeOrders
		endif
			move str12, brkMlr
			NREP0001LV2.InsertItem giving result2 using str12
			NREP0001ListView.GetItemText giving OMLRNUM using result,1
			NREP0001ListView.GetItemText giving OLNUM using result,2
			NREP0001ListView.GetItemText giving str30 using result,3
			NREP0001LV2.SetItemText using result2,str30,1 // col2	// salesperson
			NREP0001ListView.GetItemText giving str10 using result,4
			call TRIM using str10
			if (str10<>"")
				move str10, formOQTY
				add formOQTY, totalOrderQty
			endif
			NREP0001ListView.GetItemText giving str10 using result,5
			//NREP0001LV2.SetItemText using result2,str10,5 // col6 // exchange qty
			call TRIM using str10
			if (str10 <>"")
				move str10, formOEXQTY
				add formOEXQTY, totalOEXQTY
			endif
			NREP0001ListView.GetItemText giving str1 using result,6
			if (str1="1")
				add C1, exchangeOrders
			endif
			NREP0001ListView.GetItemText giving str10 using result,7  // rental qty
			//NREP0001LV2.SetItemText using result2,str10,7 // col8 // 
			call TRIM using str10
			if (str10<>"")
				move str10, formRentQty
				add formRentQty, totalRentQty
			endif
			NREP0001ListView.GetItemText giving str1 using result,8
			if (str1="1")
				add C1, rentalOrders			
			endif
			NREP0001ListView.GetItemText giving str5 using result,11 // juldate
			NREP0001LV2.SetItemText using result2,str5,2
				
			eventcheck
			Until (Stopflag = c1)		
	repeat	
	If (stopflag = c1)
		stop
	Endif

// NOW MAKE NEW LISTVIEW AND GET TOTALS...

	//return
ExitLoop	//alert note, "done with list view", result


	//return  // take out
	call goExcel  // for last entry in lv
	// now sort on broker num and go thru listview w/totals
	NREP0001ListView.SortColumn using 2,3  // 
	//return // take out
	move "begi",prevBrk
	move C0, formBrk
	move C0, totalBrk
	move C6, N5
	move seq,result
	move result,n9
	loop
		move result,n9
		NREP0001ListView.GetNextItem giving result using 0, N9 // get 1st item in LV
	until (result=seq)
		NREP0001ListView.GetItemText giving OLNUM using result,2
		if (OLNUM=prevBrk OR prevBrk="begi")  // same brkmailer combo
					// fall through	
		else	// single broker, or end of broker string
				call goExcelBook2
				move C0, formBrk
				//alert note,"switch", N1	
		endif
		move OLNUM, prevBrk
		NREP0001ListView.GetItemText giving str30 using result,4 // totals
		call TRIM using str30
		if (str30<>"")
			move str30, n11
			add N11,formBrk
			add formBrk, totalBrk
		endif
		eventcheck
	Until (Stopflag = c1)		
	repeat	
	If (stopflag = c1)
		stop
	Endif
	call goExcelBook2	// for last entry
	// code for excel sorts
		move N5, str10
		call trim using str10
		pack str15, "A", str10
		setprop sheet2.range(str15).Font,*Bold="True"
		setprop sheet2.range(str15),*Value="Total",*HorizontalAlignment=xlLeft
		pack str15, "B", str10
		setprop sheet2.range(str15).Font,*Bold="True"
		setprop sheet2.range(str15),*Value=totalOrderQty,*HorizontalAlignment=xlLeft
	call	goExcel2
	//shutdown
	return // for button return call  should this be here?


goExcel
// start filling excel sheet at row 6
	//move C6, N5   move up
	//loop
	//	if (ORCODE <> "")
			move N5, str10
			call trim using str10
			pack str15, "A", str10
			//call trim using fUserIO.fName
			
			call trim using STR30
.
			setprop sheet.range(str15),*Value=STR30,*HorizontalAlignment=xlLeft
			pack str15, "B", str10
			call TRIM using BRKMLR // USED TO BE OBRKNUM
			Call Trim using OLNUM			
.			if (BRKMLR<>"")
.				call zfillit, BRKMLR
.				 packkey   nbrkfld from BRKMLR
.				 clear     brknum
.				 clear     brcomp
.               			 call      nbrkkey
.			else
.				pack brcomp,""
.			endif
			If  (OLNUM <> "")
				call zfillit using OLNUM
				packkey NDATFLD,OLNUM
				move	C1,NDATPATH
				move	"C.LoadList-NDATKEY",Location
				pack	KeyLocation,"Key: ",NDATFLD
				call	NDATKEY
				
			Else
				pack MLSTNAME,""
			Endif
		
			
			setprop sheet.range(str15),*Value=MLSTNAME,*HorizontalAlignment=xlLeft
			pack str15, "C", str10
			call TRIM using OMLRNUM
			if (OMLRNUM<>"")
				rep zfill, OMLRNUM
				 packkey   mkey, OMLRNUM,z3
			         call      nmlrkey
			else
				pack mcomp,""
			endif
			
			setprop sheet.range(str15),*Value=mcomp,*HorizontalAlignment=xlLeft
			pack str15, "D", str10
			call trim using str10
			//call trim using fUserIO.userType
			move C0,N8
			add exchangeOrders,N8
			add rentalOrders,N8
			setprop sheet.range(str15),*Value=N8,*NumberFormat="##,####0",*HorizontalAlignment=xlLeft
			pack str15, "E", str10
			//call trim using fUserIO.companyNumber
			//move totalOrderQty, str30
			//call trim using str30
			move C0, N15   
			add totalRentQty,N15
			add totalOEXQTY,N15
banzai			setprop sheet.range(str15),*NumberFormat="##,####0", *Value=N15,*HorizontalAlignment=xlLeft
			pack str15, "F", str10
			//call trim using fUserIO.Organization
			//move exchangeOrders, str30
			//call TRIM using str30
			setprop sheet.range(str15),*Value=exchangeOrders,*NumberFormat="##,####0",*HorizontalAlignment=xlLeft
			pack str15, "G", str10
			call trim using str10
			//call trim using fUserIO.Address
			setprop sheet.range(str15),*Value=totalOEXQTY,*NumberFormat="##,####0",*HorizontalAlignment=xlLeft
			pack str15, "H", str10
			//call trim using fUserIO.City
			setprop sheet.range(str15),*Value=rentalOrders,*NumberFormat="##,####0",*HorizontalAlignment=xlLeft
			pack str15, "I", str10
			//call trim using fUserIO.State
			setprop sheet.range(str15),*Value=totalRentQty,*NumberFormat="##,####0",*HorizontalAlignment=xlLeft
			
			pack str15, "J", str10
			//call trim using fUserIO.State
			setprop sheet.range(str15),*Value=OMLRNUM,*HorizontalAlignment=xlLeft			
			pack str15, "K", str10
			//call trim using fUserIO.State
			setprop sheet.range(str15),*Value=OLNUM,*HorizontalAlignment=xlLeft						
			
			add C1, N5	// move to next column
	//	endif
	//	read	tempfile,SEQ;fUserIO
	//	until over
	//repeat
	//close tempfile
	return  // end goExcel
......Formatting.......
.Autofit
	
goExcelBook2
	move N5, str10
	call trim using str10
	pack str15, "A", str10
	call TRIM using prevBrk
	if (prevBrk<>"")
		call zfillit using prevBrk
		packkey compfld4,prevBrk
		clear compcomp
		call compkey2
.		call zfillit, prevBrk
.		 packkey   nbrkfld from prevBrk
.		 clear     brknum
.		 clear     brcomp
.		 call      nbrkkey
.		 If over
.		 	move nbrkfld to brcomp
.		 endif
		setprop sheet2.range(str15),*Value=compcomp,*HorizontalAlignment=xlLeft
	else
		setprop sheet2.range(str15),*Value="No Broker",*HorizontalAlignment=xlLeft
	endif
	pack str15, "B", str10
	setprop sheet2.range(str15),*Value=formBrk,*HorizontalAlignment=xlLeft
	add C1, N5
	return
goExcel2	move	N5,str5
	call	Trim using str5
	pack	str25,"A8:A",str5
	sheet.range(str25).Columns.Autofit
	sheet2.range(str25).Columns.Autofit
	pack	str25,"B8:B",str5
	sheet.range(str25).Columns.Autofit
	sheet2.range(str25).Columns.Autofit
	pack	str25,"C8:C",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"D8:D",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"E8:E",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"F8:F",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"G8:G",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"H8:H",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"I8:I",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"J8:J",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"K8:K",str5
	sheet.range(str25).Columns.Autofit	
.
.Remove Hyperlinks
	pack	str25,"I10:I",str5
.	getprop	sheet,*range(str25)=range1
.	range1.Hyperlinks.Add using range1,"http://www.nincal.com"
.	range1.Hyperlinks.Delete
.
CampaignFileNameSelect
	setprop ex,*Visible="True"
        clear   taskname
        move	"c:\work\",taskname	."
        setprop ex,*DefaultFilePath=taskname
        pack    taskname,taskname,"members"
        ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
        if (taskname <> "0")
                movelptr taskname,N9
                reset   taskname,N9
                append  "xls",taskname
                reset   taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                trap    TrapCampaignObject if Object
                book.saveas giving N9 using *Filename=taskname
                trapclr Object
        endif
CampaignCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        setprop ex,*DisplayAlerts=OFALSE
.	ex.quit
        destroy ex
        shutdown

TrapCampaignObject
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
                pack    taskname,str50," already exists and is open!!"
                alert   caution,taskname,result
.                goto CampaignFileNameSelect
        endif
.Send them back to select another File name and try to Save again.
        goto CampaignFileNameSelect

errortrap
.testing purposes
        getinfo exception,taskname
        return
.
NoFile
	shutdown
.
	include nordio.inc
	include compio.inc
	include cntio.inc
	include ndatio.inc
	include nxrfio.inc	
	//include nbrkio.inc
	//include nmlrio.inc
	include	comlogic.inc