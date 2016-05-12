PC        EQU       0
........................................
. Program:      NWEB002A.PLS
. Function:     Create Excel file for user.dat
. Author:       David Strahan
. Date:         February 10,2006
. Release:      1.0
. Notes:  This Program requires a calling Program - NOT SELF CONTAINED!!
........................................
.
.Include Files
          include   common.inc
        include     cons.inc
.        include               \\Nts1\e\library\develop\adjacencysourcecode\src\user.io
        include     user.io
          //Following DD is not used, but needed to include user.io
.          include             \\Nts1\e\library\develop\adjacencysourcecode\src\guidgen.inc
          include    guidgen.inc
.
Release   Init      "1.1"     DLH  Excel
Reldate   Init      "30 April 2009"
.Release  Init      "1.0"
.
ex      automation      class="Excel.Application"
books   automation
book    automation
sheets  automation
sheet   automation
range1    automation
.
Name                dim       65        // fName + space + lName

.Variant objects used to talk to outside applications
.Booleans
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom85  variant
VT_R8     EQU 5           .Double - 8 byte Real
xlRowHeight         variant
.Formatting vars needed
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlDouble    integer 4, "0x2"

xlLeft integer 4,"0xffffefDD"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
.
.begin 1.1 to find version of excel  DH 04/02/09
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.end 1.1 to find version of excel
.Program MUST be called!!
.         shutdown

.NwebPrintUsers Routine
.         make sure file exists
          trap NoFile if IO
          open      tempfile,"\\nts1\e\data\text\user.dat"
          read      tempfile,SEQ;fUserIO
          if (fUserIO.userID = "")
                    alert     note, "USER.dat is empty", result
                    shutdown
          endif

.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  Zoom85,VarType=VT_I4,VarValue=1
        create      xlRowHeight,VarType=VT_R8,VarValue="75.0"
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
          setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
          sheet.range("A1:E1").Merge
          sheet.Shapes.AddPicture using "\\Nts1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.Column Headers
          setprop sheet.range("A4"), *Value="Name" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("B4"), *Value="User Name" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("C4"), *Value="Password" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("D4"), *Value="User Type" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("E4"), *Value="Company Number" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("F4"), *Value="Organization" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("G4"), *Value="Address" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("H4"), *Value="City" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("I4"), *Value="State" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("J4"), *Value="Zip" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("K4"), *Value="Telephone" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("L4"), *Value="Email" ,*HorizontalAlignment=xlAlignCenter

          sheet.range("A4:L4").BorderAround using *LineStyle=1,*Weight=3
          setprop sheet.range("A4:L4").Font,*Name="Arial", *Size=10
        setprop sheet.range("A4:L4").Font,*Bold="True"
.Body

// start filling excel sheet at row 6
          move C6, N5
          loop
                    if (fUserIO.userID <> "")
                              move N5, str10
                              call trim using str10
                              pack str15, "A", str10
                              call trim using fUserIO.fName
                              pack Name, fUserIO.fName, " ",fUserIO.lName
                              call trim using Name
.
                              setprop sheet.range(str15),*Value=Name,*HorizontalAlignment=xlLeft
                              pack str15, "B", str10
                              call trim using fUserIO.username
                              setprop sheet.range(str15),*Value=fUserIO.username,*HorizontalAlignment=xlLeft
                              pack str15, "C", str10
                              call trim using fUserIO.password
                              setprop sheet.range(str15),*Value=fUserIO.password,*HorizontalAlignment=xlLeft
                              pack str15, "D", str10
                              call trim using str10
                              call trim using fUserIO.userType
                              setprop sheet.range(str15),*Value=fUserIO.userType,*HorizontalAlignment=xlLeft
                              pack str15, "E", str10
                              call trim using fUserIO.companyNumber
                              setprop sheet.range(str15),*NumberFormat="@", *Value=fUserIO.companyNumber,*HorizontalAlignment=xlLeft
                              pack str15, "F", str10
                              call trim using fUserIO.Organization
                              setprop sheet.range(str15),*Value=fUserIO.Organization,*HorizontalAlignment=xlLeft
                              pack str15, "G", str10
                              call trim using str10
                              call trim using fUserIO.Address
                              setprop sheet.range(str15),*Value=fUserIO.Address,*HorizontalAlignment=xlLeft
                              pack str15, "H", str10
                              call trim using fUserIO.City
                              setprop sheet.range(str15),*Value=fUserIO.City,*HorizontalAlignment=xlLeft
                              pack str15, "I", str10
                              call trim using fUserIO.State
                              setprop sheet.range(str15),*Value=fUserIO.State,*HorizontalAlignment=xlLeft
                              pack str15, "J", str10
                              call trim using str10
                              call trim using fUserIO.Zip
                              setprop sheet.range(str15),*NumberFormat="@", *Value=fUserIO.Zip,*HorizontalAlignment=xlLeft
                              pack str15, "K", str10
                              call trim using fUserIO.Telephone
                              setprop sheet.range(str15),*Value=fUserIO.Telephone,*HorizontalAlignment=xlLeft
                              pack str15, "L", str10
                              call trim using fUserIO.Email
                              setprop sheet.range(str15),*Value=fUserIO.Email,*HorizontalAlignment=xlLeft
.
                              add C1, N5          // move to next column
                    endif
                    read      tempfile,SEQ;fUserIO
                    until over
          repeat
          close tempfile
......Formatting.......
.Autofit
          move      N5,str5
          call      Trim using str5
          pack      str25,"A4:A",str5
          sheet.range(str25).Columns.Autofit
          pack      str25,"B4:B",str5
          sheet.range(str25).Columns.Autofit
          pack      str25,"C4:C",str5
          sheet.range(str25).Columns.Autofit
          pack      str25,"D4:D",str5
          sheet.range(str25).Columns.Autofit
          pack      str25,"E4:E",str5
          sheet.range(str25).Columns.Autofit
          pack      str25,"F4:F",str5
          sheet.range(str25).Columns.Autofit
          pack      str25,"G4:G",str5
          sheet.range(str25).Columns.Autofit
          pack      str25,"H4:H",str5
          sheet.range(str25).Columns.Autofit
          pack      str25,"I4:I",str5
          sheet.range(str25).Columns.Autofit
          pack      str25,"J4:J",str5
          sheet.range(str25).Columns.Autofit
          pack      str25,"K4:K",str5
          sheet.range(str25).Columns.Autofit
          pack      str25,"L4:L",str5
          sheet.range(str25).Columns.Autofit
.
.Remove Hyperlinks
          pack      str25,"L6:L",str5
.         getprop   sheet,*range(str25)=range1
.         range1.Hyperlinks.Add using range1,"http://www.nincal.com"
.         range1.Hyperlinks.Delete
.
CampaignFileNameSelect
          setprop ex,*Visible="True"
        clear   taskname
        move        "c:\work\",taskname ."
        setprop ex,*DefaultFilePath=taskname
        pack    taskname,taskname,"members"
        ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
.begin 1.1 get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
.end 1.1 get exel version info
        if (taskname <> "0")
                movelptr taskname,N9
                reset   taskname,N9
.begin 1.1 get exel version info
          if        (#ver = c1)
                append  "xlsx",taskname
          Else
                append  "xls",taskname
          endif
                reset   taskname
.end 1.1 
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
.         ex.quit
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
          include   comlogic.inc