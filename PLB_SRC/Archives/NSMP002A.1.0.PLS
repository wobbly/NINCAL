PC	EQU	0
........................................
. Program:      NSMP2A.PLS
. Function:     Create Excel file for NSMP0001.PLS
. Author:       David Strahan
. Date:         March 10,2006
. Release:      1.0
. Notes:	This Program requires a calling Program - NOT SELF CONTAINED!!
........................................
.
.Include Files
	include 	common.inc
        include 	cons.inc
        include 	compdd.inc
        include		cntdd.inc
	include    	NSMPDD.INC
.
	winhide // should this go somewhere else?
.
Release 	Init	"1.0"
.
ex      automation      class="Excel.Application"
books   automation
book    automation
sheets  automation
sheet   automation
range1	automation
.

RecordDate	dim	10
RecordEnterDate	dim	10
FileDate	dim	10
tempfile2	file
LASTWRT 	DIM 	20
.
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
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlDouble    integer 4, "0x2"

xlLeft integer 4,"0xffffefDD"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
.
.	make sure file exists
	trap NoFile if IO
	call	NSMPSEQ
	if over
		alert	note, "SAMPLES is empty", result
		shutdown
	elseif (NSMPMLR = "")
		alert	note, "SAMPLES is empty", result
		shutdown
	endif
.
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  Zoom85,VarType=VT_I4,VarValue=1
        create	xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
        create  ex
        setprop ex,*WindowState=xlMinimized
        setprop ex,*Visible="FALSE"
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
.
	setprop	sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
	sheet.range("A1:E1").Merge
	sheet.Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.Column Headers
	setprop sheet.range("A4"), *Value="Company Number" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("B4"), *Value="Company Name" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("C4"), *Value="Sample Number" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("D4"), *Value="Sample Descr" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("E4"), *Value="Sample Descr2" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("F4"), *Value="Record Date" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("G4"), *Value="Record Enter Date" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("H4"), *Value="File Date" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("I4"), *Value="Inactive" ,*HorizontalAlignment=xlAlignCenter

	sheet.range("A4:I4").BorderAround using *LineStyle=1,*Weight=3
	setprop sheet.range("A4:I4").Font,*Name="Arial", *Size=10
        setprop sheet.range("A4:I4").Font,*Bold="True"
Body

// start filling excel sheet at row 6
	move C6, N5
	loop
		//if (NSMPMLR <> "")
		type NSMPMLR
		if zero  // is NSMPMLR is a valid numeric format?
			move N5, str10
			call trim using str10
			pack str15, "A", str10
			call trim using NSMPMLR
			setprop sheet.range(str15),*NumberFormat="@",*Value=NSMPMLR,*HorizontalAlignment=xlLeft
			pack str15, "B", str10
			move 	NSMPMLR, COMPFLD
			move 	"NSMP002A-Body", Location
			pack	KeyLocation,"Key: ", COMPFLD
			call	COMPKEY
			call trim using COMPCOMP
			setprop sheet.range(str15),*Value=COMPCOMP,*HorizontalAlignment=xlLeft
			pack str15, "C", str10
			call trim using NSMPNUM
			setprop sheet.range(str15),*NumberFormat="@",*Value=NSMPNUM,*HorizontalAlignment=xlLeft
			pack str15, "D", str10
			call trim using str10
			call trim using NSMPDES1
			setprop sheet.range(str15),*Value=NSMPDES1,*HorizontalAlignment=xlLeft
			pack str15, "E", str10
			call trim using NSMPDES2
			setprop sheet.range(str15),*Value=NSMPDES2,*HorizontalAlignment=xlLeft
			pack str15, "F", str10
			call trim using NSMPDTE
			if (NSMPDTE<>"")
				unpack NSMPDTE, cc,str2,mm,dd
				clear RecordDate
				pack RecordDate, mm,"/",dd,"/",cc,str2
			endif
			setprop sheet.range(str15),*Value=RecordDate,*HorizontalAlignment=xlLeft
			pack str15, "G", str10
			call trim using str10
			call trim using NSMPDATE
			if (NSMPDATE<>"")
				unpack NSMPDATE, cc,str2,mm,dd
				clear RecordEnterDate
				pack RecordEnterDate, mm,"/",dd,"/",cc,str2
			endif
			setprop sheet.range(str15),*Value=RecordEnterDate,*HorizontalAlignment=xlLeft
.
			pack str15, "H", str10
			clear taskname
			pack taskname,"\\nins1\E\DATA\SAMPLES\s",NSMPMLR,NSMPNUM,".tif", hexzero
			clear LASTWRT
			FINDFILE taskname,WRITE=LASTWRT
			if  ZERO   // zero flag set when filename found
				clear FileDate
				if (LASTWRT<>"")
					clear cc
					clear str2
					clear mm
					clear dd
					unpack LASTWRT, cc,str2,mm,dd
					clear FileDate
					pack FileDate, mm,"/",dd,"/",cc,str2
				endif
			else
				move	"No Entry",FileDate
				// change font to red for row
				pack str15, "A", str10
				setprop sheet.range(str15).Font,*ColorIndex=3  // 3 = red
				pack str15, "B", str10
				setprop sheet.range(str15).Font,*ColorIndex=3
				pack str15, "C", str10
				setprop sheet.range(str15).Font,*ColorIndex=3
				pack str15, "D", str10
				setprop sheet.range(str15).Font,*ColorIndex=3
				pack str15, "E", str10
				setprop sheet.range(str15).Font,*ColorIndex=3
				pack str15, "F", str10
				setprop sheet.range(str15).Font,*ColorIndex=3
				pack str15, "G", str10
				setprop sheet.range(str15).Font,*ColorIndex=3
				pack str15, "H", str10
				setprop sheet.range(str15).Font,*ColorIndex=3
			endif
			setprop sheet.range(str15),*Value=FileDate,*HorizontalAlignment=xlLeft
//////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
.	the following code would use this include, list and variable:
.		include		winapi.inc
.	Systime		List
.	Wyear		integer	2
.	Wmonth		integer	2
.	Wdayoweek	integer	2
.	WDay		integer	2
.	WHour		integer	2
.	Wminute		integer	2
.	Wsecond		integer	2
.	Wmillisec	integer	2
.		listend
.	newyear		DIM	4
.
.	The following code demonstrates API calls GetFileTime and FileTimeToSystemTime
.	It is functional.  However, one cannot open a read only file, which
.	some of the samples are and need to be.  Thus, we were not able to use the code.
.
.			clear APIFileName
.			clear APIFileHandle
.			pack	taskname, "\\nins1\E\DATA\SAMPLES\s",NSMPMLR,NSMPNUM,".tif"
.			trap	errorTrap if IO
.			open 	tempfile2,taskname
.			getfile tempfile2,HANDLE=APIFILEHANDLE
.			call getfiletime
.			if (APIResult = 0 | APIResult = hexeight)
.				move "no entry", FileDate
.			else
.				clear FileDate
.				//clear newYear
.				move APILastWriteTime, APIFileTime
.				call FileTimeToSystemTime
.				unpack APISystemTime, Systime
.				move 	wmonth,result
.				move	result,str9
.				call	Trim using str9
.				move	str9,MM
.				move 	wday,result
.				move	result,str9
.				call 	Trim using str9
.				move 	str9, dd
.				move 	wyear, result
.				move	result, str9
.				call	Trim using str9
.				move	str9, newyear
.				pack FileDate, mm,"/",dd,"/",newyear
.			endif
.			close tempfile2
.			setprop sheet.range(str15),*Value=FileDate,*HorizontalAlignment=xlLeft
/////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////
			pack str15, "I", str10
			//call trim using NSMPINACTIVE  // unnecessary on a 1 byte field
  			if (NSMPINACTIVE="1")
  				move "Y", NSMPINACTIVE
  			endif
  			setprop sheet.range(str15),*Value=NSMPINACTIVE,*HorizontalAlignment=xlLeft
		else
			// insert code to insert blank record in excel spreadsheet
			move N5, str10
			call trim using str10
			pack str15, "A", str10
			setprop sheet.range(str15),*Value="Incorrect Record",*HorizontalAlignment=xlLeft
			setprop sheet.range(str15).Font,*ColorIndex=3  // 3 = red
			pack str15, "B", str10
			setprop sheet.range(str15),*Value="Please ",*HorizontalAlignment=xlLeft
			setprop sheet.range(str15).Font,*ColorIndex=3
			pack str15, "C", str10
			setprop sheet.range(str15),*Value="Examine",*HorizontalAlignment=xlLeft
			setprop sheet.range(str15).Font,*ColorIndex=3
		endif
		add C1, N5
		clear NSMPMLR
		call	NSMPSEQ
		until over
	repeat
	close tempfile
......Formatting.......
.Autofit
	move	N5,str5
	call	Trim using str5
	pack	str25,"A4:A",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"B4:B",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"C4:C",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"D4:D",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"E4:E",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"F4:F",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"G4:G",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"H4:H",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"I4:I",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"J4:J",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"K4:K",str5
	sheet.range(str25).Columns.Autofit
	pack	str25,"L4:L",str5
	sheet.range(str25).Columns.Autofit
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
	include compio.inc
	include	cntio.inc
	include NSMPIO.INC
	include	comlogic.inc