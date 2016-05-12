PC        EQU       0
........................................
. Program:      NWEB0002.PLS
. Function:     Print Button to Excel file for NWEB0002.PLS
. Author:       David Strahan
. Date:         February 8,2006
. Release:      1.0
. Notes:  This Program requires a calling program - NOT SELF-CONTAINED!!
........................................
.
.Include Files
          include   common.inc
        include     cons.inc
        include               compdd.inc
        include               cntdd.inc
        include     NWEBDD.INC
        include     NWEBDD2.INC
.
Release    INIT    "1.12"               DLH      .Excel 2013 *WindowState=xlMinimized
Reldate   Init      "2014 January 22"
.Release   Init      "1.1"     DLH Excel
.REldate   Init      "30 April 2009"
.Release  Init      "1.0"
.
FilePtr             file      ^
SEQ2      form      "-4"
usertype  dim       20 // hold user type value "A - Administrator"
.
ex      automation      class="Excel.Application"
books   automation
book    automation
sheets  automation
sheet   automation
range1    automation
.
MoDayYrTime         dim       16

.begin 1.1 to find version of excel  DH 04/02/09
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.end 1.1 to find version of excel
.Variant objects used to talk to outside applications
.Booleans
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom85  variant
VT_R8     EQU 5           .Double - 8 byte Real
xlRowHeight         variant
.
.Formatting vars needed
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlDouble    integer 4, "0x2"

xlLeft integer 4,"0xffffefDD"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
.Program MUST be called!!!
          shutdown
.
NWebPrintUsersRoutine routine FilePtr
.FilePtr established by calling program
.  make sure file exists
.         alert note, "you made it", result
          trap NoFile if IO
          read FilePtr,SEQEOF;;
          read FilePtr,SEQ2;NWEBRECORD.NWEBUSERNAME:
                              NWEBRECORD.NWEBUSERTYPE:
                              NWEBRECORD.NWEBCOMP:
                              NWEBRECORD.NWEBPROCESS:
                              MoDayYrTime:
                              NWEBRECORD.NWEBUSEREMAIL
          if (NWEBRECORD.NWEBUSERNAME = "")
                    alert note, "USER.dat is empty", result
                    return
          endif
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  Zoom85,VarType=VT_I4,VarValue=1
        create      xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
        create  ex
.begin patch 1.12
.          setprop ex,*WindowState=xlMinimized
.end patch 1.12
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
        setprop     sheet.pagesetup,*PrintGridlines=OTRUE
          setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
          sheet.range("A1:E1").Merge
          sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.ColumnHeaders
          setprop sheet.range("A4"), *Value="User Name" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("B4"), *Value="User Type" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("C4"), *Value="Company Number" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("D4"), *Value="Company Name" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("E4"), *Value="Process Accessed" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("F4"), *Value="Date" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("G4"), *Value="Email" ,*HorizontalAlignment=xlAlignCenter

          sheet.range("A4:G4").BorderAround using *LineStyle=1,*Weight=3
          setprop sheet.range("A4:G4").Font,*Name="Arial", *Size=10
        setprop sheet.range("A4:G4").Font,*Bold="True"
.Body
.
// start filling excel sheet at row 6
          move C6, N5
          //open    tempfile, "c:\work\NWEB0002.dat"
          loop
                    //read tempfile,SEQ;WebExcelList        // packing file into variables in list
          //until over
          if (NWEBRECORD.NWEBUSERNAME <> "")
                              move N5, str10
                              call trim using str10

                              pack str15, "A", str10
                              //call trim using str10
                              call trim using NWEBRECORD.NWEBUSERNAME

                              setprop sheet.range(str15),*Value=NWEBRECORD.NWEBUSERNAME,*HorizontalAlignment=xlLeft
                              pack str15, "B", str10


                              call trim using NWEBRECORD.NWEBUSERTYPE

                              if (NWEBRECORD.NWEBUSERTYPE="A")
                                        move "A - Administrator", usertype
                              else if (NWEBRECORD.NWEBUSERTYPE="C")
                                        move "C - Client", usertype
                              else if (NWEBRECORD.NWEBUSERTYPE="O")
                                        move "O - Consultant", usertype
                              else if (NWEBRECORD.NWEBUSERTYPE="B")
                                        move "B - Broker", usertype
                              else move NWEBRECORD.NWEBUSERTYPE, usertype       // whatever they had
                              endif
                              call trim using usertype

                              setprop sheet.range(str15),*Value=usertype,*HorizontalAlignment=xlLeft
                              pack str15, "C", str10
                              setprop sheet.range(str15),*NumberFormat="@", *Value=NWEBRECORD.NWEBCOMP,*HorizontalAlignment=xlLeft
                              pack str15, "D", str10
                              call trim using NWEBRECORD.NWEBCOMP
                              move      NWEBRECORD.NWEBCOMP,COMPFLD
                              move      "Body-COMPKEY",Location
                              pack      KeyLocation,"Key: ",COMPFLD
                              call      COMPKEY
                              call      Trim using COMPCOMP
                              setprop sheet.range(str15),*Value=COMPCOMP,*HorizontalAlignment=xlLeft
                              pack str15, "E", str10
                              call trim using NWEBRECORD.NWEBPROCESS
                              move      NWEBRECORD.NWEBPROCESS,NWEB2FLD
                              move      "Body-NWEB2KEY",Location
                              pack      KeyLocation,"Key: ",NWEB2FLD
                              call      NWEB2KEY
                              call      Trim using NWEB2RECORD.NWEB2NAME
                              setprop sheet.range(str15),*Value=NWEB2RECORD.NWEB2NAME,*HorizontalAlignment=xlLeft
                              pack str15, "F", str10
                              call trim using MoDayYrTime
                              setprop sheet.range(str15),*Value=MoDayYrTime,*HorizontalAlignment=xlLeft
                              pack str15, "G", str10
                              call trim using NWEBRECORD.NWEBUSEREMAIL
                              setprop sheet.range(str15),*Value=NWEBRECORD.NWEBUSEREMAIL,*HorizontalAlignment=xlLeft
.
                              add C1, N5          // move to next column
                    endif
                    read FilePtr,SEQ2;NWEBRECORD.NWEBUSERNAME:
                              NWEBRECORD.NWEBUSERTYPE:
                              NWEBRECORD.NWEBCOMP:
                              NWEBRECORD.NWEBPROCESS:
                              MoDayYrTime:
                              NWEBRECORD.NWEBUSEREMAIL
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
.
.Remove Hyperlinks
          pack      str25,"G6:G",str5
.         getprop   sheet,*range(str25)=range1
.         range1.Hyperlinks.Add using range1,"http://www.nincal.com"
.         range1.Hyperlinks.Delete
.
CampaignFileNameSelect
          setprop ex,*Visible="True"
        clear   taskname
.        move       "c:\work\",taskname
        move        "c:\",taskname                ."
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
          else
                append  "xls",taskname
          endif
.end 1.1 get exel version info
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
.         ex.quit
        destroy ex
        return

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
.
errortrap
.testing purposes
        getinfo exception,taskname
        return
.
Nofile
          noreturn
          return    // return to calling program
.
          include nwebio.inc
          include nwebio2.inc
        include     compio.inc
        include     cntio.inc
          include   comlogic.inc