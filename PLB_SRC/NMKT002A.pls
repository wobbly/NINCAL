PC        EQU       0
........................................
. Program:      NMKT002A.PLS
. Function:     Create Excel file for list markets
. Author:       David Strahan
. Date:         June 14,2006
. Release:      1.0
. Notes:
........................................
.
.Include Files
          include   common.inc
        include     cons.inc
        include               ndatdd.inc
        include               nseldd.inc
        include               ntxtdd.inc
.
Release    INIT    "1.1"               DLH      .Excel 2013 *WindowState=xlMinimized
Reldate   Init      "2014 January 22"
.Release   Init      "1.0"
.
ex      automation      class="Excel.Application"
books   automation
book    automation
sheets  automation
sheet   automation
range1    automation
.
//  vars go here
str50a    dim       50
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
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  Zoom85,VarType=VT_I4,VarValue=1
        create      xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
        create  ex
.begin patch 1.1
.        setprop ex,*WindowState=xlMinimized
.end patch 1.1
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
          sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.Column Headers
          setprop sheet.range("A4"), *Value="List" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("B4"), *Value="List Name" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("C4"), *Value="Base Select" ,*HorizontalAlignment=xlAlignCenter
.
          sheet.range("A4:C4").BorderAround using *LineStyle=1,*Weight=3
          setprop sheet.range("A4:C4").Font,*Name="Arial", *Size=10
        setprop sheet.range("A4:C4").Font,*Bold="True"
.Body
          move C6, N5  // start filling excel sheet at row6
          clock timestamp, newDate      // get today's date for compare
          unpack newdate, cc,yy,mm,dd
          call      CVTJUL
          move      JULDAYS, howmany  // howmany is today's date
// do seq read on data file
          close     ndatflist
          move      c0, ndatflag
          loop
                    move      "NMKT002A",Location
                    pack      KeyLocation,"Key: SEQ"
                    call      NDATSEQ
                    until over
                    // now use datvars
                    unpack REVDATE, cc,yy,mm,dd
                    call      CVTJUL  // JULDAYS is datacard date
                    sub       JULDAYS,howmany,result  // subtract datacard from today's date
                    if (result > 730)  // datacard is > 2 years old
                              goto SkipDataCard
                    endif
                    if (STATUS = "W" OR STATUS = "T")
                              goto SkipDataCard
                    endif
                    pack      NTXTFLD,LSTNUM,"1"
                    move      "NTXTKEY",Location
                    pack      KeyLocation,"Key: ",NTXTFLD
                    call      NTXTKEY
                    if over
                              clear     str50
                    else
                              move      NTXTTEXT,str50
                    endif
                    if (NDATOFF = "1")
                              goto SkipDataCard
                    else
                              if (str50 <> "")
                                        move      str50,str50a
                                        uppercase str50a
                                        move      "OFFICE USE ONLY",str25
                                        scan      str25,str50a
                                        if equal
                                                  goto SkipDataCard
                                        endif
                              endif
                    endif
.
                    call UseDataCard  // Datacard satisfies criteria!
.
SkipDataCard
          repeat
// here seq read of data file is over
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
.
.Remove Hyperlinks
          pack      str25,"L6:L",str5
.add hyperlink   DH 11 May 2008
.          GetProp  Sheet,*Cells=Range1
          Pack      Taskname from "=Hyperlink(#"http://www.namesinthenews.com#",#"Names in the News#")"
          setprop         sheet.Range(str25),*Formula=taskname
.add hyperlink   DH 11 May 2008
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
UseDataCard
          // start filling excel sheet at row 6
          move      N5, str10
          call      TRIM using str10
          pack      str15, "A",str10
          call      trim using LSTNUM
          setprop sheet.range(str15),*NumberFormat="@", *Value=LSTNUM,*HorizontalAlignment=xlLeft
          pack      str15, "B", str10
          call      TRIM using MLSTNAME
          setprop   sheet.range(str15),*Value=MLSTNAME,*HorizontalAlignment=xlLeft
// do aam read on select file
          pack      NSELFLD1,"01X",LSTNUM
          clear     NSELFLD3
          pack      NSELFLD2,"02X","BASE"
          move      "NSELAIM",Location
          pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
          call      NSELAIM
          if over
                    move      str50,NSELSNAME
                    pack      str15, "D", str10
                    setprop   sheet.range(str15),*Value="Select pulled from Free Text"
          endif
          pack      str15, "C", str10
          call      TRIM using NSELSNAME
          setprop   sheet.range(str15),*Value=NSELSNAME,*HorizontalAlignment=xlLeft
          clear nselsname
          add       C1,N5     // move to next row
          return

          include             ntxtio.inc
          include             nselio.inc
          include   ndatio.inc
          include             comlogic.inc