PC	EQU	0
........................................
. Program:      NMIN0002.PLS
. Function:     Create Excel file for determining use of Lists in 6 month period
. Author:       Andrew Harkins
. Date:         April 28,2006
. Release:      1.0
........................................
.
.Include Files
	include 	common.inc
        include 	cons.inc
        include		norddd.inc
        include		ndatdd.inc
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
October	form	5
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
	setprop sheet.range("A4"), *Value="List Number" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("B4"), *Value="List Name" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("C4"), *Value="Exclusive" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("D4"), *Value="Uses" ,*HorizontalAlignment=xlAlignCenter
	setprop sheet.range("E4"), *Value="Includes Orders" ,*HorizontalAlignment=xlAlignCenter

	sheet.range("A4:E4").BorderAround using *LineStyle=1,*Weight=3
	setprop sheet.range("A4:E4").Font,*Name="Arial", *Size=10
        setprop sheet.range("A4:E4").Font,*Bold="True"
.Body
	move	"10",MM
	move	"01",DD
	move	"20",CC
	move	"05",YY
	call	CVTJUL
	move	JULDAYS,October
.
	clock	timestamp,timestamp
	unpack	timestamp,CC,YY,MM,DD
	call	CVTJUL
	move	JULDAYS,N6
// start filling excel sheet at row 6
	move	C6, N5
	open	tempfile,"\\NTS1\E\DATA\TEXT\NINORDdrew.DAT",exclusive
	READ	tempfile,SEQEOF;ORDVARS
	move	"-4",SEQEOF
	loop
		READ	tempfile,SEQEOF;ORDVARS
		until over
		display	*p10:10,OLRN
		move	OODTEC,CC
		move	OODTEY,YY
		move	OODTEM,MM
		move	OODTED,DD
		call	CVTJUL
		sub	JULDAYS,N6,N7
tester
		if (N7 > "365")
			BREAK
		endif
		if (JULDAYS >= October)
			move	C0,N1
			for howmany,C6,N5
				move	howmany,str10
				call	Trim using str10
				pack	str15, "A", str10
				getprop sheet.range(str15),*Value=str6
				call	ZfillIt using str6
				if (str6 = OLNUM)
					pack	str15, "D", str10
					getprop sheet.range(str15),*Value=str9
					call	Trim using str9
					move	C0,result
					move	str9,result
					add	C1,result
					move	result,str9
					call	Trim using str9
					setprop sheet.range(str15),*Value=str9
					move	C1,N1
.
					if (OSTAT = "0" | OSTAT = "B" | OSTAT = "Q" | OSTAT = "X")
						pack	str15,"E",str10
						setprop sheet.range(str15),*Value=YES
					endif
				endif
			repeat
			if (N1 = C0)
			//Add new record
				move	N5,str5
				call	Trim using str5
				pack	str15,"A",str5
				setprop sheet.range(str15),*Value=OLNUM
				pack	NDATFLD,OLNUM
				call	NDATKEY
				pack	str15,"B",str5
				setprop sheet.range(str15),*Value=MLSTNAME
				if (ELSTCDE = "C")
					move	YES,str1
				else
					clear	str1
				endif
				pack	str15,"C",str5
				setprop sheet.range(str15),*Value=str1
				pack	str15,"D",str5
				setprop sheet.range(str15),*Value="1"
				if (OSTAT = "0" | OSTAT = "B" | OSTAT = "Q" | OSTAT = "X")
					pack	str15,"E",str5
					setprop sheet.range(str15),*Value=YES
				endif
				add	C1,N5
			endif
		endif
	repeat
	alert	note,OLRN,result
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
	include	nordio.inc
	include	ndatio.inc
	include	comlogic.inc