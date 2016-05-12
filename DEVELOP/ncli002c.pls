pc         equ 0
.Current Client Income report for April 2006 Board Meeting - LIST MANAGEMENT
        include common.inc
        include cons.inc
        include norddd.inc
           INCLUDE    COMPDD.inc
           INCLUDE    CNTDD.inc
           include    ndatdd.inc
           include    nowndd.inc
           INCLUDE    nmdldd.inc
           include    nusedd.inc
           include    nxrfdd.inc

release init    "1.0" ASH 15MAR2006 NEW RELEASE

tempfile2  ifile keylen=6,fixed=6

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
sheet  automation
ex      automation      class="Excel.Application"
.
books2   automation
book2    automation
sheets2  automation
sheet2  automation
ex2      automation      class="Excel.Application".
.
str9a      dim        9
str11a     dim        11
STR75      DIM        75
result2    form       9
.
colornum form         24
yellow     color
.Variant objects used to talk to outside applications
.See PL/B help in order to understand use of Variant objects.
.
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
VT_I8   EQU 5           .8 byte real
.VT_VARIANT EQU 12
Zoom100 variant
Zoom75  variant
Zoom85  variant
Zoom60  variant
Zoom87  variant
Zoom70  variant
Zoom97  variant
Zoom32  variant
Zoom125  variant
xlRowHeight           variant
VT_R8      EQU 5           .Double - 8 byte Real
.AcctType variant
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
xlLeft integer 4,"0xffffefDD"
xlTop integer 4,"0xffffefc0"
AlignRight integer 4,"0xffffefc8"
AlignCenter integer 4,"0xffffeff4"
AutoCalc integer 4,"0xffffeff7"
SheetsDefault integer 4,"0x00000000"
xlLandscape integer 4,"0x2"                     .2
xlMinimized integer 4,"0xFFFFEFD4"
xlNormal integer 4,"0xFFFFEFD1"
xlMaximized integer 4,"0xFFFFEFD7"
xlUnderlineStyleSingle integer 4,"0x2"
onehun     integer    4,"0x100"
DblLine integer 4,"0xffffefe9"
MedThick integer 4,"0xFFFFEFD6"
BorderHor integer 4,"0xc"
BorderVert integer 4,"0xb"
SReturn        init   0x0a                     ;soft return/line feed

           open       tempfile2,"c:\andrew\strategic\aprillostlm.isi"
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
.Others
        create  Zoom100,VarType=VT_I4,VarValue=100
        create  Zoom75,VarType=VT_I4,VarValue=75
        create  Zoom85,VarType=VT_I4,VarValue=85
        create  Zoom60,VarType=VT_I4,VarValue=60
.
        create  Zoom87,VarType=VT_I4,VarValue=87
        create  Zoom70,VarType=VT_I4,VarValue=70
        create  Zoom97,VarType=VT_I4,VarValue=97
        create  Zoom32,VarType=VT_I4,VarValue=32
        create  Zoom125,VarType=VT_I4,VarValue=125
.
           create     xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Colors
           create     yellow=255:255:160
.Initialize variables
.Open Excel application
        create  ex
           create  ex2
        setprop ex,*WindowState=xlMinimized
        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
        setprop ex2,*WindowState=xlMinimized
        setprop ex2,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
.Create Workbooks collection
        getprop ex,*Workbooks=books
        getprop ex2,*Workbooks=books2
.Create/Add a single Workbook
        books.open using "c:\andrew\strategic\lm.xls"
        books2.open using "c:\andrew\strategic\AprilBoardMeetingDraft2.xls"
        books.item giving book using 1
        books2.item giving book2 using 1
.Create Worksheets collection
        getprop book,*Sheets=sheets
        getprop book2,*Sheets=sheets2
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
        sheets.item giving sheet using 1
        //Second Sheet is used for LM current Clients!!
        sheets2.item giving sheet2 using 2
.
           move       C1,NDATPATH
           move       C1,NOWNPATH
           move       C1,NXRFPATH
.Detail Records
           clock timestamp,str4
           move       str4,N4
           move       C1,result
           move       C7,result2
           loop
                      move       result,str9
                      call       Trim using str9
                      move       result2,str9a
                      call       Trim using str9a
.List Number
                      pack       str11,"A",str9
                      getprop sheet.range(str11),*Value=NDATFLD
                      call       Trim using NDATFLD
                      until (NDATFLD = "")
                      call       ZFillIt using NDATFLD
                      call       NDATKEY
.
                      pack       NXRFFLD,LSTNUM
                      call       NXRFKEY
                      if not over
.Test to see if Client has already been lost!!!
                                 read       tempfile2,NXRFMLR;;
                                 if not over
TESTER
                                            goto EndLoop
                                 endif
                      endif
                      //
                      pack       str11a,"A",str9a
                      setprop sheet2.range(str11a),*NumberFormat="@",*Value=LSTNUM
.List Name - trimmed
                      call       Trim using MLSTNAME
                      pack       str11a,"B",str9a
                      setprop sheet2.range(str11a),*Value=MLSTNAME
.Owner Number
                      unpack     OWNNUM,str2,NOWNFLD
                      call       NOWNKEY
                      //
                      pack       str11a,"C",str9a
                      setprop sheet2.range(str11a),*NumberFormat="@",*Value=OWNLON
.Owner Name - trimmed
                      call       Trim using OWNOCPY
                      pack       str11a,"D",str9a
                      setprop sheet2.range(str11a),*Value=OWNOCPY
.Combined Address
                      //City
                      call       Trim using OWNLOCTY
                      //State
                      call       Trim using OWNLOS
                      //
                      if (OWNLOCTY = "" & OWNLOS = "")
                                 clear      taskname
                      else
                                 pack       taskname,OWNLOCTY,COMMA,B1,OWNLOS
                      endif
                      pack       str11a,"E",str9a
                      setprop sheet2.range(str11a),*Value=taskname
.Mailer
                      if (NXRFMLR = "")
                                 pack       NXRFMLR,"******"      .Force an OVER
                      endif
                      pack       COMPFLD,NXRFMLR
                      call       COMPKEY
                      //
                      pack       str11a,"F",str9a
                      setprop sheet2.range(str11a),*NumberFormat="@",*Value=COMPNUM
.Mailer Name - trimmed
                      call       Trim using COMPCOMP
                      pack       str11a,"G",str9a
                      setprop sheet2.range(str11a),*Value=COMPCOMP
.Combined Address
                      //City
                      call       Trim using COMPCITY
                      //State
                      call       Trim using COMPSTATE
                      //
                      if (COMPCITY = "" & COMPSTATE = "")
                                 clear      taskname
                      else
                                 pack       taskname,COMPCITY,COMMA,B1,COMPSTATE
                      endif
                      pack       str11a,"H",str9a
                      setprop sheet2.range(str11a),*Value=taskname
.LR Income
                      pack       str11,"C",str9
                      getprop sheet.range(str11),*Value=str25
                      call       Trim using str25
                      //
                      pack       str11a,"J",str9a
                      setprop sheet2.range(str11a),*Value=str25
.NIN Income
                      pack       str11,"D",str9
                      getprop sheet.range(str11),*Value=str25
                      call       Trim using str25
                      //
                      pack       str11a,"K",str9a
                      setprop sheet2.range(str11a),*Value=str25
.INcome Total
                      pack       str11,"E",str9
                      getprop sheet.range(str11),*Value=str25
                      call       Trim using str25
                      //
                      pack       str11a,"L",str9a
                      setprop sheet2.range(str11a),*Value=str25
.Calc. Total
                      pack       taskname,"=Sum(J",str9a,":K",str9a,")"
                      pack       str11a,"M",str9a
                      setprop sheet2.range(str11a),*Formula=taskname
.Unbilled
                      pack       str11,"F",str9
                      getprop sheet.range(str11),*Value=str25
                      call       Trim using str25
                      //
                      pack       str11a,"N",str9a
                      setprop sheet2.range(str11a),*Value=str25
.New Total
                      pack       taskname,"=Sum(M",str9a,":N",str9a,")"
                      pack       str11a,"O",str9a
                      setprop sheet2.range(str11a),*Formula=taskname
.Planner
                      pack       NMDLFLD,NDATFLD," "
                      call    NMDLKEY
                      if not over
                                 if (mdlPLAN <> "" AND mdlPLAN <> "  ")
                                            move    C2,NUSEPATH
                                            move    mdlPLAN,NUSEFLD2
                                            call    NUSEKEY
                                            if not over
                                                       pack    str25,NUSEUSER
                                            else
                                                       pack    str25,MDLCALL
                                            endif
                                 endif
                                 if (MDLCALL <> "" AND MDLCALL <> "  ")
                                            move    C2,NUSEPATH
                                            move    MDLCALL,NUSEFLD2
                                            call    NUSEKEY
                                            if not over
                                                       pack    str35,NUSEUSER
                                            else
                                                       pack    str35,MDLCALL
                                            endif
                                 endif
                      endif

                      pack       str11a,"P",str9a
                      setprop sheet2.range(str11a),*Value=str25
.Caller
                      pack       str11a,"Q",str9a
                      setprop sheet2.range(str11a),*Value=str35
.First Order/Order Number
                      pack       NORDFLD2,"02X",LSTNUM
                      call       NORDAIM
                      loop
                                 if not over
                                            if (OSALES10 = "0" & OSALES = "6")
                                                       pack       str15,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
                                                       break
                                            endif
                                 else
                                            clear      str15
                                            clear      OLRN
                                            break
                                 endif
                                 call       NORDKG
                      repeat
                      pack       str11a,"R",str9a
                      setprop sheet2.range(str11a),*Value=str15
                      pack       str11a,"S",str9a
                      setprop sheet2.range(str11a),*NumberFormat="@",*Value=OLRN
                      //
                      pack       str11a,"I",str9a
.=(DAYS360(C17,G2)/365)  representation of formula
                      pack       taskname,"=(DAYS360(R",str9a,",$G$2)/365)"
                      setprop sheet2.range(str11a),*Formula=taskname

                      add        C1,result2
EndLoop
                      add        C1,result
           repeat
.Formatting
.Numeric formatting
           pack       str25,"I7:I",str9a
           setprop sheet2.range(str25),*NumberFormat="_(0.00_)"
           pack       str25,"J7:O",str9a
           setprop sheet2.range(str25),*NumberFormat="_($* ##,####0.00_);_($* [RED](##,####0.00);_($* #"-#"_);_(@_)"
.Autofit
           pack       str25,"A6:A",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"B6:B",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"C6:C",str9a
        sheet2.range(str25).Columns.Autofit
           pack       str25,"D6:D",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"E6:E",str9a
        setprop sheet2.range(str25),*Wraptext=OFALSE
        sheet2.range(str25).Columns.Autofit
        setprop sheet2.range(str25),*Wraptext=OTRUE
           sheet2.range(str25).Columns.Autofit
           pack       str25,"F6:F",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"G6:G",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"H6:H",str9a
        setprop sheet2.range(str25),*Wraptext=OFALSE
        sheet2.range(str25).Columns.Autofit
        setprop sheet2.range(str25),*Wraptext=OTRUE
           sheet2.range(str25).Columns.Autofit
           pack       str25,"I6:I",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"J6:J",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"K6:K",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"L6:L",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"M6:M",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"N6:N",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"O6:O",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"P6:P",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"Q6:Q",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"R6:R",str9a
           sheet2.range(str25).Columns.Autofit
           pack       str25,"S6:S",str9a
           sheet2.range(str25).Columns.Autofit
.Alignment
           pack       str25,"A7:S",str9a
           setprop sheet2.range(str25),*HorizontalAlignment=xlLeft,*VerticalAlignment=xlTop
.
CampaignFileNameSelect
           move       "c:\work\",taskname              ."
           setprop ex2,*DefaultFilePath=taskname
           pack    taskname,taskname,"Client"
           setmode *mcursor=*arrow
           ex2.GetSaveAsFilename giving taskname using *InitialFilename=taskname
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
           setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
           setprop ex2,*IgnoreRemoteRequests="False",*Interactive="True"
.          book.printout
CampaignCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.          destroy sortcol
.          destroy sortcol1
           destroy sheet
           destroy book
           destroy books
           destroy sheet2
           destroy book2
           destroy books2
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
           setprop ex,*DisplayAlerts=OFALSE
           destroy ex
           setprop ex2,*DisplayAlerts=OFALSE
           destroy ex2
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

        include nordio.inc
           INCLUDE    COMPIO.inc
           INCLUDE    CNTIO.inc
           include    ndatio.inc
           include    nownio.inc
           INCLUDE    nmdlio.inc
           include    nuseio.inc
           include    nxrfio.inc
        include comlogic.inc
