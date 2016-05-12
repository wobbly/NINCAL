        include common.inc
        include cons.inc
;patch1.3
        include compdd.inc
        include cntdd.inc
;        include nmlrdd.inc
;Patch1.3
          include   npkgdd.inc

Release    INIT    "1.41"               DLH      .Excel 2013 *WindowState=xlMinimized
Reldate   Init      "2014 January 22"
.Release  init      "1.4"     25OCT2004 ASH       CONVERTED MAILER PACKAGE TO 6 BYTES
.release init    "1.3"           25MAY04 DMB     Mailer Conversion
;release init    "1.2"           19NOV02 ASH     Added more new features, including Worksheet Menu Bar
.release init    "1.1"        ASH 15NOV2002       ADDED FEATURES
.release init    "1.0"        ASH 19JUN2002       DEVELOPMENT RELEASE

StrPtr    dim       ^
FrmPtr    form      ^
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
books   automation
book    automation
sheets  automation
sheet   automation
ex      automation      class="Excel.Application"
.Variant objects used to talk to outside applications
.See PL/B help in order to understand use of Variant objects.
.
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom85  variant
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignLeft integer 4,"0xffffefdd"
AlignRight integer 4,"0xffffefc8"
SheetsDefault   integer 4,"0x00000000"
xlLandscape integer 4,"0x2"                     .2
xlMinimized integer 4,"0xFFFFEFD4"
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
.Others
        
PackagePrintExcel Routine StrPtr,FrmPtr
.
.START PATCH 1.4 REPLACED LOGIC
.        pack    MKEY,StrPtr,"000"
.        move    C1,NMLRPATH
.        move    "CreateC.-NMLRKEY",Location
.        pack    KeyLocation,"Key: ",MKEY
.        call    NMLRKEY
.        if over
.                return
.        endif
          move      StrPtr,COMPFLD
          rep       zfill,COMPFLD
          move      "CreateC.-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    return
          elseif (COMPMLRFLG <> "T")
                    return
          endif
.END PATCH 1.4 REPLACED LOGIC
.
        create  Zoom85,VarType=VT_I4,VarValue=85
.Open Excel application
        create  ex
.begin patch 1.41
.        setprop ex,*WindowState=xlMinimized
.end patch 1.41
.START PATCH 1.2 REPLACED LOGIC
..START PATCH 1.1 ADDED LOGIC
.         setprop ex,*IgnoreRemoteRequests="True",*Interactive="False"
..END PATCH 1.1 ADDED LOGIC
        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
          setprop ex.CommandBars("Standard"),*Visible="True"
          setprop ex.CommandBars("Formatting"),*Visible="True"
          setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.END PATCH 1.2 REPLACED LOGIC
.Reset Default of Worksheets found in a Workbook
        getprop ex,*SheetsInNewWorkbook=SheetsDefault
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
        setprop sheet.PageSetup,*Orientation=xlLandscape
        setprop sheet.PageSetup,*CenterFooter=" Page &P of &N"
        setprop sheet.PageSetup,*Zoom=Zoom85
.
.....Report Header.....
        setprop sheet.range("B1"),*Value="Mailer:"
.START PATCH 1.4 REPLACED LOGIC
.        setprop sheet.range("C1"),*Value=MCOMP
        setprop sheet.range("C1"),*Value=COMPCOMP
.END PATCH 1.4 REPLACED LOGIC
        setprop sheet.range("B2"),*Value="Mlr ##:"
.START PATCH 1.4 REPLACED LOGIC
.        setprop sheet.range("C2"),*Value=MNUM,*HorizontalAlignment=AlignLeft
        setprop sheet.range("C2"),*Value=COMPNUM,*HorizontalAlignment=AlignLeft
.END PATCH 1.4 REPLACED LOGIC
          if (FrmPtr = 1)
                  setprop sheet.range("C4"),*Value="Master Packages Only"
          endif
.....Record Header.....
        setprop sheet.range("A6"),*Value="Package ##"       
        setprop sheet.range("B6"),*Value="ID"
        setprop sheet.range("C6"),*Value="Package Name"
        setprop sheet.range("D6"),*Value="Master?"
        setprop sheet.range("E6"),*Value="Master ##"
        setprop sheet.range("F6"),*Value="Package Date"
        setprop sheet.range("G6"),*Value="Notes"
        setprop sheet.PageSetup,*PrintTitleRows="1:6"
.
.....Records.....
          move      "8",howmany
          clear     NPKGFLD2
          clear     NPKGFLD3
          clear     NPKGFLD4
          clear     NPKGFLD5
.START PATCH 1.4 REPLACED LOGIC
.         pack      NPKGFLD1,"01X",MNUM
          pack      NPKGFLD1,"01X",COMPNUM
.END PATCH 1.4 REPLACED LOGIC
        move    C1,NPKGPATH
        move    "LoadDetail-NPKGAIM",Location
        pack    KeyLocation,"Key: ",NPKGFLD1
        call    NPKGAIM
          loop
                    until over
                    if (FrmPtr <> 1 | (FrmPtr = 1 & NPKGMaster = "1"))
                              move      howmany,str9
                              call      Trim using str9
                              pack      str10,"A",str9
                              call      Trim using NPKGNUM
                            setprop sheet.range(str10),*Value=NPKGNum       
                              pack      str10,"B",str9
                              call      Trim using NPKGID
                            setprop sheet.range(str10),*Value=NPKGID
                              pack      str10,"C",str9
                              call      Trim using NPKGPName
                            setprop sheet.range(str10),*Value=NPKGPName
                              if (NPKGMaster = "1")
                                        move      YES,str1
                              else
                                        clear     str1
                              endif
                              pack      str10,"D",str9
                            setprop sheet.range(str10),*Value=str1
                              pack      str10,"E",str9
                              call      Trim using NPKGMastNum
                            setprop sheet.range(str10),*Value=NPKGMastNum
                              unpack    NPKGDate,CC,YY,MM,DD
                              call      Trim using DD
                              if (DD <> "")
                                        pack      str11,MM,SLASH,DD,SLASH,CC,YY
                              else
                                        clear     str11
                              endif
                              pack      str10,"F",str9
                            setprop sheet.range(str10),*Value=str11
                              pack      str10,"G",str9
                              call      Trim using NPKGNotes
                            setprop sheet.range(str10),*Value=NPKGNotes
                              add       C1,howmany
                    endif
                  move    "LoadDetail-NPKGKG",Location
                  call    NPKGKG
          repeat
.Set up Header Formatting
        setprop sheet.range("A6","G6").Font,*Bold="True"
        sheet.range("A6","G6").BorderAround using *LineStyle=1,*Weight=2
        move    howmany,str10
        call    Trim using str10
        pack    str4,"A1"
        pack    str5,"A",str10
        sheet.range(str4,str5).Columns.Autofit
        pack    str4,"B1"
        pack    str5,"B",str10
        sheet.range(str4,str5).Columns.Autofit
        pack    str4,"C1"
        pack    str5,"C",str10
        sheet.range(str4,str5).Columns.Autofit
        pack    str4,"D1"
        pack    str5,"D",str10
        sheet.range(str4,str5).Columns.Autofit
        pack    str4,"E1"
        pack    str5,"E",str10
        sheet.range(str4,str5).Columns.Autofit
        pack    str4,"F1"
        pack    str5,"F",str10
        sheet.range(str4,str5).Columns.Autofit
        pack    str4,"G1"
        pack    str5,"G",str10
        sheet.range(str4,str5).Columns.Autofit
.
CampaignFileNameSelect
          pack      taskname,"c:\work\"                     ."
          setprop ex,*DefaultFilePath=taskname
          pack      taskname,"c:\work\Package",MNUM
............................................
        setmode *mcursor=*arrow
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
.START PATCH 1.1 ADDED LOGIC
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.END PATCH 1.1 ADDED LOGIC
.        book.printout
CampaignCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
          destroy Zoom85
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        setprop ex,*DisplayAlerts=OFALSE
        setprop ex,*SheetsInNewWorkbook=SheetsDefault
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
                pack    taskname,str50," already exists and is open!!",newline,"Select another Filename!!"
                alert   caution,taskname,result
.                goto CampaignFileNameSelect
        endif
.Send them back to select another File name and try to Save again.
        goto CampaignFileNameSelect
.        goto CampaignCleanUp

errortrap
.testing purposes
        getinfo exception,taskname
        return
;patch1.3
        include compio.inc
        include cntio.inc       
.       include nmlrio.inc
;Patch1.3
          include   npkgio.inc
        include comlogic.inc
