        include common.inc
        include cons.inc
        include ncmpdd.inc
        include norddd.inc
        include nord4dd.inc
        include nord5dd.inc
        include nspe2dd.inc
        include npnddd.inc
        include nloldd.inc
.START PATCH 1.54 REPLACED LOGIC
.        include nmlrdd.inc
.        include nbrkdd.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
.END PATCH 1.54 REPLACED LOGIC
        include nrtndd.inc
        include nxrfdd.inc
        include nxngdd.inc
        include nxchdd.inc
        include ndatdd.inc
        include nofrdd.inc
        include nsmpdd.inc
        include ncntdd.inc
        include oslspern.inc
        include media.inc
.START PATCH 1.52 ADDED LOGIC
          INCLUDE   NSEL2DD.INC
.END PATCH 1.52 ADDED LOGIC

.2014 February reviewed for new pending status
Release    INIT    "1.56"               DLH      .Excel 2013 *WindowState=xlMinimized
Reldate   Init      "2014 January 22"
.release   init    "1.55.9"           DLH  turn of PL logo
.reldate   Init      "17 March 2010"
.release   init    "1.55.8"           DLH  Hyperlinks for lists
.reldate   Init      "13 May 2009"
.release init    "1.55.7"           12JUN2007 ASH  PLI Inclusion
.release init    "1.55.6"           08Mar2007 DLH Oslspern expansion
.release init    "1.55.5"           08DEC2004 ASH Reset settings perhaps applied by NCMP002A/NCMP002B
.release init    "1.55.4"           30NOV2004 ASH Increased Mailer field to 6 bytes
.release init    "1.55.3"           15NOV2004 ASH Work Order #69 - Added subtotal for Net Qty
.release init    "1.55.2"           02NOV2004 ASH SAMPLE FILE CONVERTED - INCREASED MAILER FIELD TO 6 BYTES
.release init    "1.55.1"           13SEP2004 ASH LOGO CONVERSION - FIRST PATCH
.release init    "1.55"           03AUG2004 ASH   LOGO CONVERSION
.release init    "1.54"           27MAY2004 ASH   MAILER CONVERSION
.release init    "1.53"           12MAY04 ASH     ADDED LOGIC TO SIMPLIFY USER INTERVENTION PREVENTION
.release init    "1.52"           26JAN04 ASH     DATACARD CONVERSION
.release init    "1.51"           19NOV02 ASH     Added more new features, including Worksheet Menu Bar
.release init    "1.5"           14NOV02 ASH     Added new features
.release init    "1.4"           28OCT02 ASH     Added NINCA logo - Idea of the Month
.release init    "1.3"           16OCT01 ASH     Accumulated Time Saviour requests
.release init    "1.2"           21MAR01 ASH     Updates from JVM/SM
.release init    "1.1"           16FEB01 ASH     Time Saviour meeting:
.                                               Change filename default
.                                               Open up spreadsheet automatically
.release init    "1.0"
.EXTERNAL ROUTINES FROM NORDTEST.PLC
CampOrderGetHistory external "NORDTEST;OrderGetHistory"
.
area1   form    10
area2   form    10
area3   form    10
N35     form    3.5
ComboPtr ComboBox ^
.WindPtr window  ^
StrPtr  dim     ^
StrPtr1 dim     ^
.START PATCH 1.55.7 ADDED LOGIC
StrPtr2 dim     ^
SReturn        init 0x0a                     ;soft return/line feed
.END PATCH 1.55.7 ADDED LOGIC
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
Range1    automation
.......................
.bars     automation
.bar      automation
.menubar INTEGER 2,"0x00000006"
.......................
sheets  automation
sheet   automation
sortcol automation
sortcol1 automation
ex      automation      class="Excel.Application"
.Variables needed by OrderLoadXSTAT
EFLAG   dim     1
AKey1A  init    "01L"
AKey2A  init    "02L"
Carr    init    0x7f    .Carriage Return used by PLB
firstrow form   9
MlrComment dim  255     .used in extraction of Carriage Return characters for Campaign Comments
.                        Field has length of 300 but Excel only allows length of 255 in a cell.
MlrComment1 dim 50      .So we need this field too.
LOLComment dim  150     .used in extraction of Carriage Return characters for External LOL Comments
LOLComment1 dim 150     .used in extraction of Carriage Return characters for Internal LOL Comments
.
SubFlag form    1
CALCPER  FORM      7.4
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
.START PATCH 1.55 ADDED LOGIC
VT_R8     EQU 5           .Double - 8 byte Real
xlRowHeight         variant
.END PATCH 1.55 ADDED LOGIC
.START PATCH 1.55.1 ADDED LOGIC
TopMargin variant
BottomMargin        variant
.END PATCH 1.55.1 ADDED LOGIC
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignLeft integer 4,"0xffffefdd"
AlignRight integer 4,"0xffffefc8"
.START PATCH 1.4 ADDED LOGIC
AlignCenter integer 4,"0xffffeff4"
.END PATCH 1.4 ADDED LOGIC
SheetsDefault   integer 4,"0x00000000"
xlLandscape integer 4,"0x2"                     .2
xlMinimized integer 4,"0xFFFFEFD4"
.START PATCH 1.2 ADDED LOGIC
xlUnderlineStyleSingle integer 4,"0x2"
.END PATCH 1.2 ADDED LOGIC
.Create the Variant objects
.Booleans
.START PATCH 1.55 MOVED LOGIC
.        create  OTRUE,VarType=VT_BOOL,VarValue=1
.        create  OFALSE,VarType=VT_BOOL,VarValue=0
.END PATCH 1.55 MOVED LOGIC
.Others
.START PATCH 1.3 REMMED LOGIC
.        create  Zoom85,VarType=VT_I4,VarValue=85
.END PATCH 1.3 REMMED LOGIC
.START PATCH 1.55.4 ADDED LOGIC
NCMPMLRHold         dim       4
.END PATCH 1.55.4 ADDED LOGIC

.START PATCH 1.55.7 REPLACED LOGIC
CreateCampaign Routine StrPtr,ComboPtr,StrPtr1,StrPtr2
.END PATCH 1.55.7 REPLACED LOGIC
.Initialize variables
        move    C0,SubFlag
.
        pack    NCMPFLD,StrPtr
        move    C1,NCMPPATH
        move    "CreateC.-NCMPKEY",Location
        pack    KeyLocation,"Key: ",NCMPFLD
        call    NCMPKEY
        if over
                return
        endif
                trap    TrapCampaignObject1 giving error if Object
.
.START PATCH 1.3 ADDED LOGIC
        create  Zoom85,VarType=VT_I4,VarValue=85
.END PATCH 1.3 ADDED LOGIC
.START PATCH 1.55 ADDED LOGIC
          create    OTRUE,VarType=VT_BOOL,VarValue=1
          create    OFALSE,VarType=VT_BOOL,VarValue=0
          create    xlRowHeight,VarType=VT_R8,VarValue="75.0"
.END PATCH 1.55 ADDED LOGIC
.START PATCH 1.55.1 ADDED LOGIC
."1" increment in Excel interface equals "1.3888" in OLE logic
          create    TopMargin,VarType=VT_R8,VarValue="18"             Roughly equals .25 inches:  18 * 1.388 = 25
          create    BottomMargin,VarType=VT_R8,VarValue="36"          Roughly equals .50 inches:  36 * 1.388 = 50
.END PATCH 1.55.1 ADDED LOGIC
.Open Excel application
        create  ex
.begin patch 1.56
.        setprop ex,*WindowState=xlMinimized
.end patch 1.56
.START PATCH 1.51 REPLACED LOGIC
..START PATCH 1.5 ADDED LOGIC
.         setprop ex,*IgnoreRemoteRequests="True",*Interactive="False"
..END PATCH 1.5 ADDED LOGIC
.START PATCH 1.53 REPLACED LOGIC
.        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
        setprop ex,*Visible="False"
.END PATCH 1.53 REPLACED LOGIC
          setprop ex.CommandBars("Standard"),*Visible="True"
          setprop ex.CommandBars("Formatting"),*Visible="True"
          setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.END PATCH 1.51 REPLACED LOGIC
........................
.         setprop ex,*AltStartupPath="C:\Documents and Settings\aharkin\application data\microsoft\office"
.         setprop ex,*DisplayFullScreen=OTRUE
........................
.Reset Default of Worksheets found in a Workbook
        getprop ex,*SheetsInNewWorkbook=SheetsDefault
        setprop ex,*SheetsInNewWorkbook=C1
........................
.         getprop ex,*CommandBars=bars
.         getprop   bars,*ActiveMenuBar=bar
.         setprop   bar,*Visible="True"
..        setprop   bar,*Position=menubar
........................
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
.START PATCH 1.3 ADDED LINE BACK IN - IT WAS ORIGINALLY COMMENTED OUT
        setprop sheet.PageSetup,*Zoom=Zoom85
.END PATCH 1.3 ADDED LINE BACK IN - IT WAS ORIGINALLY COMMENTED OUT
.START PATCH 1.55.1 ADDED LOGIC
        setprop sheet.PageSetup,*TopMargin=TopMargin
        setprop sheet.PageSetup,*BottomMargin=BottomMargin
        setprop sheet.PageSetup,*FooterMargin=TopMargin
.END PATCH 1.55.1 ADDED LOGIC
.Clear any garbage that might be present - this is redundant since we have created a new Worksheet
.DH test 03/14/08
.        sheet.range("A1","W40").Clear
.DH test 03/14/08
............................
.......My VB Function.......
.Public Function AddRange1(range1 As Range) As Long
.
.  For Each c In range1
.    If c.Font.Strikethrough = False Then
.        AddRange1 = AddRange1 + c.Value
.    End If
.  Next c
.  
.End Function
.
............................
............................
.Import above function AddRng.bas
.        ex.VBE.ActiveVBProject.VBComponents.Import using "f:\library\develop\tester.bas"
.        ex.VBE.ActiveVBProject.VBComponents.Import using "\\nts0\c\library\plb_src\addrange.bas"
        ex.VBE.ActiveVBProject.VBComponents.Import using "\\nins1\e\apps\plb\code\addrange.bas"
.All objects except for the SortColumn have been created.
.
.
.....Campaign Header.....
.Header Column 1
.We could create a Range automation object but do not have to.
.Instead we use the Range property to dynamically return a Range object each time we want
.to dump in a value, or set another property of a cell(s).
..................................
.START PATCH 1.4 REPLACED LOGIC
.        setprop sheet.range("B1"),*Value="Mailer:"
.        pack    MKEY,NCMPMLR,"000"
.        move    "CreateCamp.-NMLRKEY",Location
.        pack    KeyLocation,"Key: ",MKEY
.        call    NMLRKEY
.        call    Trim using MCOMP
.        setprop sheet.range("C1"),*Value=MCOMP
.        setprop sheet.range("B2"),*Value="Campaign:"
.        call    Trim using NCMPCNAME
.        setprop sheet.range("C2"),*Value=NCMPCNAME,*HorizontalAlignment=AlignLeft
.        setprop sheet.range("B3"),*Value="THRU:"
.        pack    NBRKFLD,NCMPBRK,NCMPBRKCNT
.        move    "CreateCamp.-NBRKKEY",Location
.        pack    KeyLocation,"Key: ",NBRKFLD
.        call    NBRKKEY
.        call    Trim using BRCOMP
.        setprop sheet.range("C3"),*Value=BRCOMP
...........Line Skip..........
..START PATCH 1.2 REPLACED LOGIC
..        setprop sheet.range("B5"),*Value="Media:"
..        move    " ",MEDIA
..        move    C0,N2
..        move    NCMPMEDIA,N2
..        move    N2,N3
..        add     C1,N2   .File begins with '0', Combo begins with '1' and  first item is null
..        if (N3 = 20)
..                move    C0,N2   .First item has blank filled string of MED20
..        else
..                if (N3 < 20)
..                        add     C1,N2   
..                endif 
..        endif
..        load    MEDIA from N2 of MED20,MED0,MED1,MED2,MED3,MED4,MED5:
..                MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
..                MED15,MED16,MED17,MED18,MED19,MED21,MED22:
..                MED23,MED24,MED25,MED26,MED27,MED28,MED29
..        call    Trim using MEDIA
..        setprop sheet.range("C5"),*Value=MEDIA
..        setprop sheet.range("B6"),*Value="Shipping:"
..        call    Trim using NCMPSHIP
..        clear   str55
..        if (NCMPSHIP <> "")
..                move    C0,N2
..                move    NCMPSHIP,N2
..                add     C2,N2
..                getitem ComboPtr,N2,str55
..        endif
..        setprop sheet.range("C6"),*Value=str55
..        setprop sheet.range("B7"),*Value="P.O.:"
..        setprop sheet.range("C7"),*Value=NCMPPO,*HorizontalAlignment=AlignLeft
............Line Skip..........
..        pack    NOFRFLD,NCMPMLR,NCMPOFFER
..        rep     zfill in NOFRFLD
..        move    "CreateCamp.-NOFRKEY",Location
..        pack    KeyLocation,"Key: ",NOFRFLD
..        call    NOFRKEY
..        if over
..                clear   OFDESC
..        endif
..        setprop sheet.range("B9"),*Value="Offer:"
..        call    Trim using OFDESC
..        setprop sheet.range("C9"),*Value=OFDESC
..        setprop sheet.range("B10"),*Value="Sample:"
..        pack    NSMPFLD,NCMPMLR,NCMPSAMPLE
..        rep     zfill in NSMPFLD
..        move    "CreateCamp.-NSMPKEY",Location
..        pack    KeyLocation,"Key: ",NSMPFLD
..        call    NSMPKEY
..        if over
..                clear   NSMPDES1
..        endif
..        call    Trim using NSMPDES1
..        setprop sheet.range("C10"),*Value=NSMPDES1
............Line Skip..........
..        setprop sheet.range("B12"),*Value="Comments:"
...Routine for simulating Carriage Return capability
..        move    "12",firstrow
..        call    Trim using NCMPCOMMENT
..        movelptr NCMPCOMMENT,N6
..        loop
..                move    firstrow,str9
..                call    Trim using str9
..                pack    str4,"C",str9
..                movefptr NCMPCOMMENT,N5
..                scan    carr,NCMPCOMMENT
..                if not equal
..                        setprop sheet.range(str4),*Value=NCMPCOMMENT
..                        add     C1,firstrow
..                        break
..                else
..Reset string to extract section prior to Carriage Return
..                        movefptr NCMPCOMMENT,N4
..                        if (N4 > N5)
..                                sub     C1,N4
..                        else
..                                if (N4 <> N6)
..                                        add     C1,N4
..                                endif
..                        endif
..                        setlptr NCMPCOMMENT,N4
..                        reset   NCMPCOMMENT,N5
..                        cmatch  carr,NCMPCOMMENT
..                        if equal
..                                if (N5 = N6 & N4 = N6)
..                                        add     C1,firstrow
..                                        break
..                                endif
..                                bump    NCMPCOMMENT
..                        else
..                                setprop sheet.range(str4),*Value=NCMPCOMMENT
...Reset string to start at next section.
..                                add     C2,N4,N5
..                                reset   NCMPCOMMENT,N5
..                        endif
..                        setlptr NCMPCOMMENT,N6
..                        add     C1,firstrow
..                endif
..        repeat
...Header Column 2
..        setprop sheet.range("E1"),*Value="Mailer ## :"
..        setprop sheet.range("F1"),*Value=NCMPMLR,*HorizontalAlignment=AlignLeft
..        setprop sheet.range("E2"),*Value="Campaign ## :"
..        setprop sheet.range("F2"),*Value=NCMPNUM,*HorizontalAlignment=AlignLeft
..        setprop sheet.range("E3"),*Value="List ## :"
..        move    C0,UNIVERSE
..        clear   NXRFFLD2
..        clear   NXRFLIST
..        move    MKEY,NXRFFLD2
..        rep     zfill,NXRFFLD2
..        move    C2,NXRFPATH
..        move    "CreateCamp.-NXRFKEY",Location
..        pack    KeyLocation,"Key: ",NXRFFLD2
..        call    NXRFKEY
..        if not over
..                move    NXRFLIST,NDATFLD
..                move    C1,NDATPATH
..                move    "CreateCamp.-NDATKEY",Location
..                pack    KeyLocation,"Key: ",NDATFLD
..                call    NDATKEY
..        endif
..        setprop sheet.range("F3"),*Value=NXRFLIST,*HorizontalAlignment=AlignLeft
..        setprop sheet.range("E4"),*Value="Universe:"
..        move    UNIVERSE,str9
..        setprop sheet.range("F4"),*Value=str9,*NumberFormat="##,####0_);[Red](##,####0)"
..        setprop sheet.range("E5"),*Value="Key Info:"
..        setprop sheet.range("F5"),*Value=NCMPKEY,*HorizontalAlignment=AlignLeft
............Line Skip..........
..        setprop sheet.range("E7"),*Value="Gross Qty:"
..        setprop sheet.range("F7"),*Value=NCMPQTY,*NumberFormat="##,####0_);[Red](##,####0)"
..        setprop sheet.range("E8"),*Value="Net Qty:"
..        setprop sheet.range("F8"),*Value=NCMPNETQTY,*NumberFormat="##,####0_);[Red](##,####0)"
...Header Column 3
..        setprop sheet.range("I1"),*Value="Ship To:"
..        clear   str55
..        move    NCMPSHIPTO,NRTNFLD
..        move    "CreateCamp.-NRTNKEY",Location
..        pack    KeyLocation,"Key: ",NRTNFLD
..        call    NRTNKEY
..        if over
..                clear   RTCOMP
..                clear   RTCNTCT
..                clear   RTADDR
..                clear   RTCITY
..                clear   RTSTATE
..                clear   RTZIP   
..        else
..                call    Trim using RTCITY
..                if (RTCITY <> "")
..                        append  RTCITY,str55
..                        append  COMMA,str55
..                endif
..                append  RTSTATE,str55
..                append  B1,str55
..                append  RTZIP,str55
..                reset   str55
..        endif
..        call    Trim using RTCOMP
..        setprop sheet.range("J1"),*Value=RTCOMP
..        setprop sheet.range("I2"),*Value="Attn:"
..        call    Trim using RTCNTCT
..        setprop sheet.range("J2"),*Value=RTCNTCT
..        call    Trim using RTADDR
..        setprop sheet.range("J3"),*Value=RTADDR
..        setprop sheet.range("J4"),*Value=str55
..        setprop sheet.range("I5"),*Value="Return Date:"
..        clear   str10
..        call    Trim using NCMPRDATE
..        if (NCMPRDATE <> "")
..                unpack  NCMPRDATE,str4,MM,DD
..                pack    str10,MM,SLASH,DD,SLASH,str4
..        endif
..        setprop sheet.range("J5"),*Value=str10,*HorizontalAlignment=AlignLeft
..        setprop sheet.range("I6"),*Value="Cut Off Date:"
..        clear   str10
..        call    Trim using NCMPCDATE
..        if (NCMPCDATE <> "")
..                unpack  NCMPCDATE,str4,MM,DD
..                pack    str10,MM,SLASH,DD,SLASH,str4
..        endif
..        setprop sheet.range("J6"),*Value=str10,*HorizontalAlignment=AlignLeft
..        setprop sheet.range("I7"),*Value="Mail Date:"
..        clear   str10
..        call    Trim using NCMPMDATE
..        if (NCMPMDATE <> "")
..                unpack  NCMPMDATE,str4,MM,DD
..                pack    str10,MM,SLASH,DD,SLASH,str4
..        endif
..        setprop sheet.range("J7"),*Value=str10,*HorizontalAlignment=AlignLeft
............Line Skip..........
..        setprop sheet.range("I9"),*Value="Contact:"
..        move    C1,NCNTPATH
..        move    NCMPCNT,NCNTFLD
..        move    "CreateCamp.-NCNTKEY",Location
..        pack    KeyLocation,"Key: ",NCNTFLD
..        call    NCNTKEY
..        if over
..                clear   CNTNAME
..        endif
..        call    Trim using CNTNAME
..        setprop sheet.range("J9"),*Value=CNTNAME
..        setprop sheet.range("I10"),*Value="Sales Person:"
..        move    NCMPPLANNER,N9
..        move    osls0,str45
..        load    str45 from N9 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
..                osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
..                osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
.                   osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
..        call    Trim using str45
..        setprop sheet.range("J10"),*Value=str45
..
..................................................................................................
.        setprop sheet.range("B5"),*Value="Mailer ## :"
.        setprop sheet.range("C5"),*Value=NCMPMLR,*HorizontalAlignment=AlignLeft
.        setprop sheet.range("B6"),*Value="Campaign ## :"
.        setprop sheet.range("C6"),*Value=NCMPNUM,*HorizontalAlignment=AlignLeft
.        setprop sheet.range("B7"),*Value="List ## :"
.        move    C0,UNIVERSE
.        clear   NXRFFLD2
.        clear   NXRFLIST
.        move    MKEY,NXRFFLD2
.        rep     zfill,NXRFFLD2
.        move    C2,NXRFPATH
.        move    "CreateCamp.-NXRFKEY",Location
.        pack    KeyLocation,"Key: ",NXRFFLD2
.        call    NXRFKEY
.        if not over
.                move    NXRFLIST,NDATFLD
.                move    C1,NDATPATH
.                move    "CreateCamp.-NDATKEY",Location
.                pack    KeyLocation,"Key: ",NDATFLD
.                call    NDATKEY
.        endif
.        setprop sheet.range("C7"),*Value=NXRFLIST,*HorizontalAlignment=AlignLeft
.        setprop sheet.range("B8"),*Value="Universe:"
.        move    UNIVERSE,str9
..START PATCH 1.2 REPLACED LOGIC
..        setprop sheet.range("C8"),*Value=str9,*NumberFormat="##,####0_);[Red](##,####0)"
.        setprop sheet.range("C8"),*Value=str9,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=AlignLeft
..END PATCH 1.2 REPLACED LOGIC
.        pack    NOFRFLD,NCMPMLR,NCMPOFFER
.        rep     zfill in NOFRFLD
.        move    "CreateCamp.-NOFRKEY",Location
.        pack    KeyLocation,"Key: ",NOFRFLD
.        call    NOFRKEY
.        if over
.                clear   OFDESC
.        endif
.        setprop sheet.range("B9"),*Value="Offer:"
.        call    Trim using OFDESC
.        setprop sheet.range("C9"),*Value=OFDESC
.        setprop sheet.range("B10"),*Value="Sample:"
.        pack    NSMPFLD,NCMPMLR,NCMPSAMPLE
.        rep     zfill in NSMPFLD
.        move    "CreateCamp.-NSMPKEY",Location
.        pack    KeyLocation,"Key: ",NSMPFLD
.        call    NSMPKEY
.        if over
.                clear   NSMPDES1
.        endif
.        call    Trim using NSMPDES1
.        setprop sheet.range("C10"),*Value=NSMPDES1
.        setprop sheet.range("B11"),*Value="P.O.:"
.        setprop sheet.range("C11"),*Value=NCMPPO,*HorizontalAlignment=AlignLeft
.        setprop sheet.range("B12"),*Value="Key Info:"
.        setprop sheet.range("C12"),*Value=NCMPKEY,*HorizontalAlignment=AlignLeft
.        setprop sheet.range("B13"),*Value="Comments:"
..Routine for simulating Carriage Return capability
.        move    "13",firstrow
.
.....................................
.START PATCH 1.55 REPLACED LOGIC
.        setprop sheet.range("A1"),*Value="Names in the News",*HorizontalAlignment=AlignCenter
.        setprop sheet.range("A1").Font,*Name="Times New Roman",*Underline=xlUnderlineStyleSingle,*Size=20
.        setprop sheet.range("A1").Characters(1,5).Font,*Bold="True"
.        setprop sheet.range("A1").Characters(7,11).Font,*Italic="True"
.        sheet.range("A1:C1").Merge
.        setprop sheet.range("A2"),*Value="C  A  L  I  F  O  R  N  I  A       I  N  C .",*HorizontalAlignment=AlignCenter
.        setprop sheet.range("A2").Font,*Name="Times New Roman",*Size=10
.        sheet.range("A2:C2").Merge
...................
.START PATCH 1.55.7 REPLACED LOGIC
.         setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
.         sheet.range("A1:E1").Merge
.         sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.begin patch 1.55.9
.          if (StrPtr2 = "P")
.                    clear taskname
.                    setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
.                    setprop sheet.range("A1"),*HorizontalAlignment=AlignCenter
.                    setprop sheet.range("A1").Font,*Name="Times New Roman",*Size=10,*Bold="True"
.                    append    "Pacific Lists, Inc.",taskname
.                    append    Sreturn,taskname
.                    append    "1300 Clay St. 11th Floor",taskname
.                    append    Sreturn,taskname
.                    append    "Oakland, CA 94612-1429",taskname
.                    append    Sreturn,taskname
.                    append    "415-945-9450 · Fax 415-945-9451",taskname
.                    append    Sreturn,taskname
.                    append    "A Division of Names in the News",taskname
.                    reset     taskname
.                    setprop   sheet.range("A1"),*Value=taskname
.                    setprop sheet.range("A1").Characters(1,19).Font,*Size=18
.                    sheet.range("A1:D1").Merge
.          else
                    setprop   sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
.DH 2015 Feb 27 Bombing standard report on the following .merge  testing without
.                    sheet.range("A1:E1").Merge
                    sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
.          endif
.end patch 1.55.9
.END PATCH 1.55.7 REPLACED LOGIC
.END PATCH 1.55 REPLACED LOGIC
.
        setprop sheet.range("B4"),*Value="Mailer:"
.START PATCH 1.55.4 REPLACED LOGIC
.        pack    MKEY,NCMPMLR,"000"
.        move    "CreateCamp.-NMLRKEY",Location
.        pack    KeyLocation,"Key: ",MKEY
.        call    NMLRKEY
.        call    Trim using MCOMP
.        setprop sheet.range("C4"),*Value=MCOMP
.................................
          pack      COMPFLD,NCMPMLR
          move    "CreateCamp.-COMPKEY",Location
          pack    KeyLocation,"Key: ",COMPFLD
          call    COMPKEY
          call    Trim using COMPCOMP
          setprop sheet.range("C4"),*Value=COMPCOMP
.Following used as a temporary measure until all files have had Mailer field converted
          move      COMPOLDMLR,NCMPMLRHold
.END PATCH 1.55.4 REPLACED LOGIC
        setprop sheet.range("B5"),*Value="Campaign:"
        call    Trim using NCMPCNAME
        setprop sheet.range("C5"),*Value=NCMPCNAME,*HorizontalAlignment=AlignLeft
        setprop sheet.range("B6"),*Value="THRU:"
.START PATCH 1.55.4 REPLACED LOGIC
.        pack    NBRKFLD,NCMPBRK,NCMPBRKCNT
.        move    "CreateCamp.-NBRKKEY",Location
.        pack    KeyLocation,"Key: ",NBRKFLD
.        call    NBRKKEY
.        call    Trim using BRCOMP
.        setprop sheet.range("C6"),*Value=BRCOMP
..................................
          pack      COMPFLD,NCMPBRK
          move    "CreateCamp.2-COMPKEY",Location
          pack    KeyLocation,"Key: ",COMPFLD
          call    COMPKEY
          call    Trim using COMPCOMP
          setprop sheet.range("C6"),*Value=COMPCOMP
.END PATCH 1.55.4 REPLACED LOGIC
..........Line Skip..........
        setprop sheet.range("B8"),*Value="Mailer ## :"
        setprop sheet.range("C8"),*Value=NCMPMLR,*HorizontalAlignment=AlignLeft
        setprop sheet.range("B9"),*Value="Campaign ## :"
        setprop sheet.range("C9"),*Value=NCMPNUM,*HorizontalAlignment=AlignLeft
        setprop sheet.range("B10"),*Value="List ## :"
        move    C0,UNIVERSE
        clear   NXRFFLD2
        clear   NXRFLIST
.START PATCH 1.55.4 REPLACED LOGIC
.        move    MKEY,NXRFFLD2
        move    NCMPMLRHold,NXRFFLD2
.END PATCH 1.55.4 REPLACED LOGIC
        rep     zfill,NXRFFLD2
        move    C2,NXRFPATH
        move    "CreateCamp.-NXRFKEY",Location
        pack    KeyLocation,"Key: ",NXRFFLD2
        call    NXRFKEY
        if not over
                move    NXRFLIST,NDATFLD
                move    C1,NDATPATH
                move    "CreateCamp.-NDATKEY",Location
                pack    KeyLocation,"Key: ",NDATFLD
                call    NDATKEY
        endif
        setprop sheet.range("C10"),*Value=NXRFLIST,*HorizontalAlignment=AlignLeft
        setprop sheet.range("B11"),*Value="Universe:"
.START PATCH 1.52 REPLACED LOGIC
.        move    UNIVERSE,str9
.        setprop sheet.range("C11"),*Value=str9,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=AlignLeft
        move    UNIVERSE,str10
        setprop sheet.range("C11"),*Value=str10,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=AlignLeft
.END PATCH 1.52 REPLACED LOGIC
.START PATCH 1.55.4 REPLACED LOGIC - TEMPORARY PATCH
.        pack    NOFRFLD,NCMPMLR,NCMPOFFER
        pack    NOFRFLD,NCMPMLRHold,NCMPOFFER
.END PATCH 1.55.4 REPLACED LOGIC - TEMPORARY PATCH
        rep     zfill in NOFRFLD
        move    "CreateCamp.-NOFRKEY",Location
        pack    KeyLocation,"Key: ",NOFRFLD
        call    NOFRKEY
        if over
                clear   OFDESC
        endif
        setprop sheet.range("B12"),*Value="Offer:"
        call    Trim using OFDESC
        setprop sheet.range("C12"),*Value=OFDESC
        setprop sheet.range("B13"),*Value="Sample:"
.START PATCH 1.55.4 REPLACED LOGIC
..START PATCH 1.55.2 REPLACED LOGIC
..        pack    NSMPFLD,NCMPMLR,NCMPSAMPLE
.         pack      COMPFLD3,NCMPMLR
.         move      "CreateCamp.-COMPKEY3",Location
.         pack      KeyLocation,"Key: ",COMPFLD3
.         call      COMPKEY3
.         if over
.                   clear     COMPNUM
.         endif
.        pack    NSMPFLD,COMPNUM,NCMPSAMPLE
..END PATCH 1.55.2 REPLACED LOGIC
          pack    NSMPFLD,NCMPMLR,NCMPSAMPLE
.END PATCH 1.55.4 REPLACED LOGIC
        rep     zfill in NSMPFLD
        move    "CreateCamp.-NSMPKEY",Location
        pack    KeyLocation,"Key: ",NSMPFLD
        call    NSMPKEY
        if over
                clear   NSMPDES1
        endif
        call    Trim using NSMPDES1
        setprop sheet.range("C13"),*Value=NSMPDES1
        setprop sheet.range("B14"),*Value="P.O.:"
        setprop sheet.range("C14"),*Value=NCMPPO,*HorizontalAlignment=AlignLeft
        setprop sheet.range("B15"),*Value="Key Info:"
        setprop sheet.range("C15"),*Value=NCMPKEY,*HorizontalAlignment=AlignLeft
        setprop sheet.range("B16"),*Value="Comments:"
.Routine for simulating Carriage Return capability
        move    "16",firstrow
.END PATCH 1.4 REPLACED LOGIC
        call    Trim using NCMPCOMMENT
        movelptr NCMPCOMMENT,N6
        loop
                move    firstrow,str9
                call    Trim using str9
                pack    str4,"C",str9
                movefptr NCMPCOMMENT,N5
                scan    carr,NCMPCOMMENT
                if not equal
                        setprop sheet.range(str4),*Value=NCMPCOMMENT
                        add     C1,firstrow
                        break
                else
.Reset string to extract section prior to Carriage Return
                        movefptr NCMPCOMMENT,N4
                        if (N4 > N5)
                                sub     C1,N4
                        else
                                if (N4 <> N6)
                                        add     C1,N4
                                endif
                        endif
                        setlptr NCMPCOMMENT,N4
                        reset   NCMPCOMMENT,N5
                        cmatch  carr,NCMPCOMMENT
                        if equal
                                if (N5 = N6 & N4 = N6)
                                        add     C1,firstrow
                                        break
                                endif
                                bump    NCMPCOMMENT
                        else
                                setprop sheet.range(str4),*Value=NCMPCOMMENT
.Reset string to start at next section.
                                add     C2,N4,N5
                                reset   NCMPCOMMENT,N5
                        endif
                        setlptr NCMPCOMMENT,N6
                        add     C1,firstrow
                endif
        repeat
.Header Column 2
.START PATCH 1.4 REPLACED LOGIC
.        setprop sheet.range("D5"),*Value="Ship To:"
.        setprop sheet.range("D5").Font,*Underline=xlUnderlineStyleSingle
.        clear   str55
.        move    NCMPSHIPTO,NRTNFLD
.        move    "CreateCamp.-NRTNKEY",Location
.        pack    KeyLocation,"Key: ",NRTNFLD
.        call    NRTNKEY
.        if over
.                clear   RTCOMP
.                clear   RTCNTCT
.                clear   RTADDR
.                clear   RTCITY
.                clear   RTSTATE
.                clear   RTZIP   
.        else
.                call    Trim using RTCITY
.                if (RTCITY <> "")
.                        append  RTCITY,str55
.                        append  COMMA,str55
.                endif
.                append  RTSTATE,str55
.                append  B1,str55
.                append  RTZIP,str55
.                reset   str55
.        endif
.        call    Trim using RTCOMP
.        setprop sheet.range("D6"),*Value=RTCOMP
.        call    Trim using RTCNTCT
.        setprop sheet.range("D7"),*Value=RTCNTCT
.        call    Trim using RTADDR
.        setprop sheet.range("D8"),*Value=RTADDR
.        setprop sheet.range("D9"),*Value=str55
...........Line Skip..........
.        move    " ",MEDIA
.        move    C0,N2
.        move    NCMPMEDIA,N2
.        move    N2,N3
.        add     C1,N2   .File begins with '0', Combo begins with '1' and  first item is null
.        if (N3 = 20)
.                move    C0,N2   .First item has blank filled string of MED20
.        else
.                if (N3 < 20)
.                        add     C1,N2   
.                endif 
.        endif
.        load    MEDIA from N2 of MED20,MED0,MED1,MED2,MED3,MED4,MED5:
.                MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
.                MED15,MED16,MED17,MED18,MED19,MED21,MED22:
.                MED23,MED24,MED25,MED26,MED27,MED28,MED29
.        call    Trim using MEDIA
.        setprop sheet.range("D11"),*Value=MEDIA
.        call    Trim using NCMPSHIP
.        clear   str55
.        if (NCMPSHIP <> "")
.                move    C0,N2
.                move    NCMPSHIP,N2
.                add     C2,N2
.                getitem ComboPtr,N2,str55
.        endif
.        setprop sheet.range("D12"),*Value=str55
..Header Column 3
.        setprop sheet.range("E5"),*Value="Return Date:"
.        clear   str10
.        call    Trim using NCMPRDATE
.        if (NCMPRDATE <> "")
.                unpack  NCMPRDATE,str4,MM,DD
.                pack    str10,MM,SLASH,DD,SLASH,str4
.        endif
.        setprop sheet.range("F5"),*Value=str10,*HorizontalAlignment=AlignLeft
.        setprop sheet.range("E6"),*Value="Cut Off Date:"
.        clear   str10
.        call    Trim using NCMPCDATE
.        if (NCMPCDATE <> "")
.                unpack  NCMPCDATE,str4,MM,DD
.                pack    str10,MM,SLASH,DD,SLASH,str4
.        endif
.        setprop sheet.range("F6"),*Value=str10,*HorizontalAlignment=AlignLeft
.        setprop sheet.range("E7"),*Value="Mail Date:"
.        clear   str10
.        call    Trim using NCMPMDATE
.        if (NCMPMDATE <> "")
.                unpack  NCMPMDATE,str4,MM,DD
.                pack    str10,MM,SLASH,DD,SLASH,str4
.        endif
.        setprop sheet.range("F7"),*Value=str10,*HorizontalAlignment=AlignLeft
..Header Column 4
.        setprop sheet.range("H5"),*Value="Gross Qty:"
.        setprop sheet.range("I5"),*Value=NCMPQTY,*NumberFormat="##,####0_);[Red](##,####0)"
.        setprop sheet.range("H6"),*Value="Net Qty:"
.        setprop sheet.range("I6"),*Value=NCMPNETQTY,*NumberFormat="##,####0_);[Red](##,####0)"
...........Line Skip..........
...........Line Skip..........
...........Line Skip..........
.        setprop sheet.range("H11"),*Value="Contact:"
.        move    C1,NCNTPATH
.        move    NCMPCNT,NCNTFLD
.        move    "CreateCamp.-NCNTKEY",Location
.        pack    KeyLocation,"Key: ",NCNTFLD
.        call    NCNTKEY
.        if over
.                clear   CNTNAME
.        endif
.        call    Trim using CNTNAME
.        setprop sheet.range("I11"),*Value=CNTNAME
.        setprop sheet.range("H12"),*Value="Sales:"
.        move    NCMPPLANNER,N9
.        move    osls0,str45
.        load    str45 from N9 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
.                osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
.                osls17,osls18,osls19,osls20,osls21,osls22
.        call    Trim using str45
.        setprop sheet.range("I12"),*Value=str45
.....................................
        setprop sheet.range("D8"),*Value="Ship To:"
        setprop sheet.range("D8").Font,*Underline=xlUnderlineStyleSingle
        clear   str55
        move    NCMPSHIPTO,NRTNFLD
        move    "CreateCamp.-NRTNKEY",Location
        pack    KeyLocation,"Key: ",NRTNFLD
        call    NRTNKEY
        if over
                clear   RTCOMP
                clear   RTCNTCT
                clear   RTADDR
                clear   RTCITY
                clear   RTSTATE
                clear   RTZIP   
        else
                call    Trim using RTCITY
                if (RTCITY <> "")
                        append  RTCITY,str55
                        append  COMMA,str55
                endif
                append  RTSTATE,str55
                append  B1,str55
                append  RTZIP,str55
                reset   str55
        endif
        call    Trim using RTCOMP
        setprop sheet.range("D9"),*Value=RTCOMP
        call    Trim using RTCNTCT
        setprop sheet.range("D10"),*Value=RTCNTCT
        call    Trim using RTADDR
        setprop sheet.range("D11"),*Value=RTADDR
        setprop sheet.range("D12"),*Value=str55
..........Line Skip..........
        move    " ",MEDIA
        move    C0,N2
        move    NCMPMEDIA,N2
        move    N2,N3
        add     C1,N2   .File begins with '0', Combo begins with '1' and  first item is null
        if (N3 = 20)
                move    C0,N2   .First item has blank filled string of MED20
        else
                if (N3 < 20)
                        add     C1,N2   
                endif 
        endif
        load    MEDIA from N2 of MED20,MED0,MED1,MED2,MED3,MED4,MED5:
                MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
                MED15,MED16,MED17,MED18,MED19,MED21,MED22:
                MED23,MED24,MED25,MED26,MED27,MED28,MED29
        call    Trim using MEDIA
        setprop sheet.range("D14"),*Value=MEDIA
        call    Trim using NCMPSHIP
        clear   str55
        if (NCMPSHIP <> "")
                move    C0,N2
                move    NCMPSHIP,N2
                add     C2,N2
                getitem ComboPtr,N2,str55
        endif
        setprop sheet.range("D15"),*Value=str55
.Header Column 3
        setprop sheet.range("E8"),*Value="Return Date:"
        clear   str10
        call    Trim using NCMPRDATE
        if (NCMPRDATE <> "")
                unpack  NCMPRDATE,str4,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,str4
        endif
        setprop sheet.range("F8"),*Value=str10,*HorizontalAlignment=AlignLeft
        setprop sheet.range("E9"),*Value="Cut Off Date:"
        clear   str10
        call    Trim using NCMPCDATE
        if (NCMPCDATE <> "")
                unpack  NCMPCDATE,str4,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,str4
        endif
        setprop sheet.range("F9"),*Value=str10,*HorizontalAlignment=AlignLeft
        setprop sheet.range("E10"),*Value="Mail Date:"
        clear   str10
        call    Trim using NCMPMDATE
        if (NCMPMDATE <> "")
                unpack  NCMPMDATE,str4,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,str4
        endif
        setprop sheet.range("F10"),*Value=str10,*HorizontalAlignment=AlignLeft
.Header Column 4
        setprop sheet.range("H8"),*Value="Gross Qty:"
        setprop sheet.range("I8"),*Value=NCMPQTY,*NumberFormat="##,####0_);[Red](##,####0)"
        setprop sheet.range("H9"),*Value="Net Qty:"
        setprop sheet.range("I9"),*Value=NCMPNETQTY,*NumberFormat="##,####0_);[Red](##,####0)"
..........Line Skip..........
..........Line Skip..........
..........Line Skip..........
        setprop sheet.range("H14"),*Value="Contact:"
        move    C1,NCNTPATH
        move    NCMPCNT,NCNTFLD
        move    "CreateCamp.-NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
        call    NCNTKEY
        if over
                clear   CNTNAME
        endif
        call    Trim using CNTNAME
        setprop sheet.range("I14"),*Value=CNTNAME
        setprop sheet.range("H15"),*Value="Sales:"
        move    NCMPPLANNER,N9
        move    osls0,str45
        load    str45 from N9 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
                osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
                osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
        call    Trim using str45
        setprop sheet.range("I15"),*Value=str45
.END PATCH 1.4 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
...................NEW LOGIC...................
.        sub     C1,firstrow,N10
.        move    N10,str10
.        call    Trim using str10
.        pack    str4,"J",str10
..Copy Header info to the Clipboard
.        sheet.range("A1",str4).Copy
.WordObj   Automation
.DocsObj   Automation
.DocObj    Automation
.RangeObj  Automation
. 
..               .
.. Start with the Word Application object
..
.        Create WordObj,Class="Word.Application"
..
.. Now get the documents object and create a new document
..
.        GetProp WordObj,*Documents=DocsObj
.        DocsObj.Add Giving DocObj
..
.. To open instead enable the following code
..
..         DocsObj.Open Giving DocObj Using "C:\WrdTest1.Doc"
..
.. Now get the actual word document and close the documents object
..
.        DocObj.Activate
.        GetProp DocObj,*Content=RangeObj
.        SetProp RangeObj,*Text="Hi there"
.        DocObj.SaveAs Using "C:\andrew\WrdTest1.Doc"
..        setprop WordObj,*DisplayAlerts=OFALSE
.        WordObj.Quit
.
..Clear Header info - We are using an imported Word object instead        
.        sheet.range("A1",str4).Clear
..................END NEW LOGIC.................
.
.....Record Header.....
        add     C1,firstrow
        move    firstrow,str9
        call    Trim using str9
        pack    str4,"A",str9
        setprop sheet.range(str4),*Value="List ##"       
        pack    str4,"B",str9
        setprop sheet.range(str4),*Value="LR ##"
        pack    str4,"C",str9
        setprop sheet.range(str4),*Value="List Name"
        pack    str4,"D",str9
        setprop sheet.range(str4),*Value="Select"
        pack    str4,"E",str9
        setprop sheet.range(str4),*Value="Universe"
        pack    str4,"F",str9
        setprop sheet.range(str4),*Value="Quantity"
.START PATCH 1.2 REPLACED LOGIC
.        pack    str4,"G",str9
.        setprop sheet.range(str4),*Value="Net Qty"
.        pack    str4,"H",str9
.        setprop sheet.range(str4),*Value="Net %"
.        pack    str4,"I",str9
.        setprop sheet.range(str4),*Value="Answer"
.
        pack    str4,"G",str9
        setprop sheet.range(str4),*Value="Answer"
        pack    str4,"H",str9
        setprop sheet.range(str4),*Value="Net %"
        pack    str4,"I",str9
        setprop sheet.range(str4),*Value="Net Qty"
.END PATCH 1.2 REPLACED LOGIC
        pack    str4,"J",str9
        setprop sheet.range(str4),*Value="Exchange Status"
        pack    str4,"K",str9
        setprop sheet.range(str4),*Value="Comments"
        pack    str4,"L",str9
        setprop sheet.range(str4),*Value="Internal Notes"
        pack    str4,"M",str9
        setprop sheet.range(str4),*Value="Status"
        pack    str4,"N",str9
        setprop sheet.range(str4),*Value="Sub-Status"
        pack    str4,"O",str9
        setprop sheet.range(str4),*Value="Mail Date"
        pack    str4,"Z",str9
        setprop sheet.range(str4),*Value="Cont/Test Sort Column"
.
        pack    str11,"1:",str9
        setprop sheet.PageSetup,*PrintTitleRows=str11
.
.....Records.....
.Records for this report always start 2 rows after Headers
        add     C2,firstrow,howmany
        move    NCMPFLD,NLOLFLD1
        move    C2,NLOLPATH
        move    "LoadLOLDetail-NLOLKEY",Location
        pack    KeyLocation,"Key: ",NLOLFLD1
.Clear Order variables used in List View
        clear   OLRN
        clear   OSTAT
        clear   OCLRSTAT
.START PATCH 1.55.4 REPLACED LOGIC - TEMPORARY PATCH
.        move    NCMPMLR,OMLRNUM
        move    NCMPMLRHold,OMLRNUM
.END PATCH 1.55.4 REPLACED LOGIC - TEMPORARY PATCH
        call    NLOLKEY
        if over
                goto LoadOrderDetail
        endif
        move    "LoadLOLDetail-NLOLKS",Location
        pack    KeyLocation,"Key: ",NLOLFLD1
        loop
                until (NLOLCNUM <> NLOLFLD1)
                move    NLOLLIST,OLNUM
                move    NLOLCOMMENT,DESC003
                move    NLOLCOMMENT1,DESC004
.START PATCH 1.52 ADDED LOGIC
                    pack      NSEL2FLD,"2",NLOLLOL
                    move      "NSEL2KEY",Location
                    pack      KeyLocation,"Key: ",NSEL2FLD
                    call      NSEL2KEY
                    if not over
                              move      NSEL2NAME,NLOLSELECT
                              move      NSEL2QTY,N10
                              move      N10,NLOLUNIVERSE
                              rep       zfill,NLOLUNIVERSE
                    endif
.END PATCH 1.52 ADDED LOGIC
                call    LoadDetail
                call    NLOLKS
                until over
        repeat
LoadOrderDetail
.NINORD records
        move    NCMPFLD,NORDFLDC
        move    C4,NORDPATH
        move    "LoadLOLDetail-NORDKEY",Location
        pack    KeyLocation,"Key: ",NORDFLDC
        call    NORDKEY
        if not over
                move    "LoadLOLDetail-NORDKS",Location
                pack    KeyLocation,"Key: ",NORDFLDC
                loop
                        until (OCAMP <> NORDFLDC)
                        call    OrderLoadLOLPackLOL
.                        clear   NLOLTEST
.                        if (OCCODE = "1")       .CONTINUATION
.                                move    "0",NLOLTEST
.                        elseif (OTOCODE = "1")  .TEST
.                                move    "1",NLOLTEST
.                        endif
                        move    "LoadO.Detail-NSPE2KEY",Location
                        pack    KeyLocation,"Key: ",OLRN
                        pack    NSPE2FLD,OLRN
                        call    NSPE2KEY
                        call    LoadDetail
.Need to reestablish NORDPATH at each iteration as above routines call other routines which MAY reset NORDPATH to C1
                        move    C4,NORDPATH
                        call    NORDKS
                        until over
                repeat
        endif
.
.....Format document.....
.Set up Header Formatting
        move    firstrow,str9
        call    Trim using str9
        pack    str4,"A",str9
        pack    str5,"Z",str9
        setprop sheet.range(str4,str5).Font,*Bold="True"
        sheet.range(str4,str5).BorderAround using *LineStyle=1,*Weight=2
.Create formulas
        add     C2,firstrow
        move    firstrow,str9
        call    Trim using str9
        clear   str25
        append  "=AddRange1(F",str25
.        append  "=Sum(F",str25
        append  str9,str25
        append  ":F",str25
        sub     C1,howmany
        move    howmany,str9
        call    Trim using str9
        append  str9,str25
        append  ")",str25
        reset   str25
        add     C2,howmany
        move    howmany,str9
        call    Trim using str9
        pack    str4,"E",str9
.START PATCH 1.55.3 REPLACED LOGIC
.        setprop sheet.range(str4),*Value="Totals:"
.        setprop sheet.range(str4).Font,*Bold="True"
          setprop sheet.range(str4),*Value="Total:"
          setprop sheet.range(str4).Font,*Bold="True"
          pack      str4,"H",str9
          setprop sheet.range(str4),*Value="Total Net:"
          setprop sheet.range(str4).Font,*Bold="True"
.END PATCH 1.55.3 REPLACED LOGIC
        pack    str4,"F",str9
..........
        move    howmany,area1
        setprop sheet.range(str4),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0)"
...............
.START PATCH 1.2 REPLACED LOGIC
.        pack    str4,"G",str9
.        rep     "FG",str25
        pack    str4,"I",str9
        rep     "FI",str25
.END PATCH 1.2 REPLACED LOGIC
...............
        setprop sheet.range(str4),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0)"
..............
.Sort by List Name
        pack    str4,"Z",str9
.        trap    errortrap if object
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
.Cont/Test
        move    firstrow,str9
        call    Trim using str9
        pack    str5,"Z",str9
        getprop sheet.range(str5),*Columns(1)=sortcol
.List Name
        move    firstrow,str9
        call    Trim using str9
        pack    str5,"C",str9
        getprop sheet.range(str5),*Columns(1)=sortcol1
.Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
        pack    str5,"A",str9
        sheet.range(str5,str4).sort using *Key1=sortcol,*Order1=1,*Key2=sortcol1,*Order2=1
.Pad Sorted rows with Descriptions
        sub     C2,howmany              .Set howmany to last record
        move    C0,N10
        move    firstrow,N10            .Set N10 to first record
        move    firstrow,N9             .Set N9 to first record in Sub-total sections
        clear   str8
        loop
                move    N10,str10
                call    Trim using str10
                pack    str5,"Z",str10
                getprop sheet.range(str5),*Value=str7
                call    Trim using str7
                until (str7 = "")
                if (str7 <> str8)
                        move    str7,str8
                        pack    str4,"A",str10
                        sheet.range(str4,str5).insert
...............
                        add     C1,area1
................
                        pack    str6,"C",str10
                        if (str7 = "0")
                                setprop sheet.range(str6),*Value="Continuations"
.START PATCH 1.2 REPLACED LOGIC
.                                setprop sheet.range(str6).Font,*Italic="True"
                                setprop sheet.range(str6).Font,*BOLD="True"
.END PATCH 1.2 REPLACED LOGIC
                                add     C1,N10,N9
                                add     C1,SubFlag
                        elseif (str7 = "1")
                                if (SubFlag > 0)
                                        sheet.range(str4,str5).insert
...............
                                        add     C1,area1
................
.Create Sub-total formulas
                                        clear   str25
                                        move    N9,str9
                                        call    Trim using str9
                                        append  "=AddRange1(F",str25
.                                        append  "=Sum(F",str25
                                        append  str9,str25
                                        append  ":F",str25
                                        sub     C1,N10
                                        move    N10,str11
                                        call    Trim using str11
                                        append  str11,str25
                                        append  ")",str25
                                        reset   str25
                                        pack    str4,"C",str10
                                        setprop sheet.range(str4),*Value="Qty Sub-Total:",*HorizontalAlignment=AlignRight
                                        setprop sheet.range(str4).Font,*Bold="True",*Strikethrough="False"
                                        pack    str4,"D",str10
.........
                                        move    str10,area2
                                        setprop sheet.range(str4),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0)"
..........
.START PATCH 1.55.3 ADDED LOGIC
                                        clear   str25
                                        append  "=AddRange1(I",str25
.                                        append  "=Sum(F",str25
                                        append  str9,str25
                                        append  ":I",str25
                                        append  str11,str25
                                        append  ")",str25
                                        reset   str25
                                        pack    str4,"G",str10
                                        setprop sheet.range(str4),*Value="Net Qty Sub-Total:",*HorizontalAlignment=AlignRight
                                        setprop sheet.range(str4).Font,*Bold="True",*Strikethrough="False"
                                        pack    str4,"H",str10
.........
                                        move    str10,area2
                                        setprop sheet.range(str4),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0)"
..........
.END PATCH 1.55.3 ADDED LOGIC
                                        add     C2,N10
                                endif
                                move    N10,str10
                                call    Trim using str10
                                pack    str6,"C",str10
                                setprop sheet.range(str6),*Value="ReTests"
.START PATCH 1.2 REPLACED LOGIC
.                                setprop sheet.range(str6).Font,*Italic="True"
                                setprop sheet.range(str6).Font,*BOLD="True",*Strikethrough="False"
.END PATCH 1.2 REPLACED LOGIC
                                add     C1,N10,N9
                                add     C1,howmany
                                add     C1,SubFlag
                        elseif (str7 = "2")
                                if (SubFlag > 0)
                                        sheet.range(str4,str5).insert
...............
                                       add     C1,area1
.                        if (area2 > c0)
.                        add     C1,area2
.                        endif
................
.Create Sub-total formulas
                                        clear   str25
                                        move    N9,str9
                                        call    Trim using str9
                                        append  "=AddRange1(F",str25
.                                        append  "=Sum(F",str25
                                        append  str9,str25
                                        append  ":F",str25
                                        sub     C1,N10
                                        move    N10,str11
                                        call    Trim using str11
                                        append  str11,str25
                                        append  ")",str25
                                        reset   str25
                                        pack    str4,"C",str10
                                        setprop sheet.range(str4),*Value="Qty Sub-Total:",*HorizontalAlignment=AlignRight
                                        setprop sheet.range(str4).Font,*Bold="True",*Strikethrough="False"
                                        pack    str4,"D",str10
.........
                                        move    str10,area3
                                        setprop sheet.range(str4),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0)"
.........
.START PATCH 1.55.3 ADDED LOGIC
                                        clear   str25
                                        append  "=AddRange1(I",str25
.                                        append  "=Sum(F",str25
                                        append  str9,str25
                                        append  ":I",str25
                                        append  str11,str25
                                        append  ")",str25
                                        reset   str25
                                        pack    str4,"G",str10
                                        setprop sheet.range(str4),*Value="Net Qty Sub-Total:",*HorizontalAlignment=AlignRight
                                        setprop sheet.range(str4).Font,*Bold="True",*Strikethrough="False"
                                        pack    str4,"H",str10
.........
                                        move    str10,area2
                                        setprop sheet.range(str4),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0)"
..........
.END PATCH 1.55.3 ADDED LOGIC
                                        add     C2,N10
                                endif
                                move    N10,str10
                                call    Trim using str10
                                pack    str6,"C",str10
                                setprop sheet.range(str6),*Value="Tests"
.START PATCH 1.2 REPLACED LOGIC
.                                setprop sheet.range(str6).Font,*Italic="True"
                                setprop sheet.range(str6).Font,*BOLD="True",*Strikethrough="False"
.END PATCH 1.2 REPLACED LOGIC
                                add     C1,N10,N9
                                add     C1,howmany
                        endif
                        add     C1,howmany
                        add     C1,N10
                endif
                add     C1,N10
                until (N10 > howmany)
        repeat
        move    N10,str10
        call    Trim using str10
        pack    str5,"Z",str10
        pack    str4,"A",str10
        sheet.range(str4,str5).insert
...............
                        add     C1,area1
.                        if (area2 > C0)
.                        add     C1,area2
.                        endif
.                        if (area3 > C0)
.                        add     C1,area3
.                        endif
                        
................
.Create Sub-total formula
        clear   str25
        move    N9,str9
        call    Trim using str9
        append  "=AddRange1(F",str25
.        append  "=Sum(F",str25
        append  str9,str25
        append  ":F",str25
        sub     C1,N10
        move    N10,str11
        call    Trim using str11
        append  str11,str25
        append  ")",str25
        reset   str25
        pack    str4,"C",str10
        setprop sheet.range(str4),*Value="Qty Sub-Total:",*HorizontalAlignment=AlignRight
        setprop sheet.range(str4).Font,*Bold="True",*Strikethrough="False"
        pack    str4,"D",str10
        setprop sheet.range(str4),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0)"
.START PATCH 1.55.3 ADDED LOGIC
        clear   str25
        append  "=AddRange1(I",str25
.         append  "=Sum(F",str25
        append  str9,str25
        append  ":I",str25
        append  str11,str25
        append  ")",str25
        reset   str25
        pack    str4,"G",str10
        setprop sheet.range(str4),*Value="Net Qty Sub-Total:",*HorizontalAlignment=AlignRight
        setprop sheet.range(str4).Font,*Bold="True",*Strikethrough="False"
        pack    str4,"H",str10
.........
        move    str10,area2
        setprop sheet.range(str4),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0)"
..........
.END PATCH 1.55.3 ADDED LOGIC
.START PATCH 1.55.3 MOVED LOGIC
..Following section is a bit funky.
..When my VB function is loaded, it will not calculate if values are applied after function is
..applied, so I have to reload it for all but the last instance, in which the formula is dropped
..into cell after all values have been loaded.
..This is a bit klunky, but I thought of no other alternative.
.        move    area1,str12
.        call    Trim using str12
.        pack    str4,"F",str12
.        getprop sheet.range(str4),*Formula=str25
.        setprop sheet.range(str4),*Formula=str25
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"G",str12
.        pack    str4,"I",str12
..END PATCH 1.2 REPLACED LOGIC
.        getprop sheet.range(str4),*Formula=str25
.        setprop sheet.range(str4),*Formula=str25
.        if (area2 > C0)
.                move    area2,str12
.                call    Trim using str12
.                pack    str4,"D",str12
.                getprop sheet.range(str4),*Formula=str25
.                setprop sheet.range(str4),*Formula=str25
.        endif
.        if (area3 > C0)
.                move    area3,str12
.                call    Trim using str12
.                pack    str4,"D",str12
.                getprop sheet.range(str4),*Formula=str25
.                setprop sheet.range(str4),*Formula=str25
.        endif
.END PATCH 1.55.3 MOVED LOGIC
.Get rid of Sort Column
        move    firstrow,str9
        call    Trim using str9
        pack    str4,"P1"
        pack    str5,"AZ",str10
        sheet.range(str4,str5).clear
.Set Column Widths for larger fields
.        move    firstrow,str9
.        call    Trim using str9
.Following is done so that the Total is included in Autofit.
.10 extra lines will actually cover much more than Total lines, but, what the hell.
        move    str10,N10
        add     "10",N10
        move    N10,str10
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
.START PATCH 1.2 REPLACED LOGIC
.        pack    str4,"G1"
.        pack    str5,"G",str10
.        sheet.range(str4,str5).Columns.Autofit
.        pack    str4,"I1"
.        pack    str5,"I",str10
.        sheet.range(str4,str5).Columns.Autofit
.
        pack    str4,"G1"
        pack    str5,"G",str10
        sheet.range(str4,str5).Columns.Autofit
        pack    str4,"H1"
        pack    str5,"H",str10
        sheet.range(str4,str5).Columns.Autofit
.END PATCH 1.2 REPLACED LOGIC
        pack    str4,"J1"
        pack    str5,"J",str10
        sheet.range(str4,str5).Columns.Autofit
        pack    str4,"L1"
        pack    str5,"L",str10
        sheet.range(str4,str5).Columns.Autofit
        pack    str4,"M1"
        pack    str5,"M",str10
        sheet.range(str4,str5).Columns.Autofit
        pack    str4,"N1"
        pack    str5,"N",str10
        sheet.range(str4,str5).Columns.Autofit
        pack    str4,"O1"
        pack    str5,"O",str10
        sheet.range(str4,str5).Columns.Autofit
.START PATCH 1.55.1 ADDED LOGIC
          sheet.Rows(2).Delete
.END PATCH 1.55.1 ADDED LOGIC
.START PATCH 1.55.3 MOVED LOGIC - DUE TO ABOVE DELETE!!
.Logo stuff screwed up area vars so I am resetting
          sub       C1,area1
          sub       C1,area2
          sub       C1,area3
.Following section is a bit funky.
.When my VB function is loaded, it will not calculate if values are applied after function is
.applied, so I have to reload it for all but the last instance, in which the formula is dropped
.into cell after all values have been loaded.
.This is a bit klunky, but I thought of no other alternative.
        move    area1,str12
        call    Trim using str12
        pack    str4,"F",str12
        getprop sheet.range(str4),*Formula=str25
        setprop sheet.range(str4),*Formula=str25
        pack    str4,"I",str12
        getprop sheet.range(str4),*Formula=str25
        setprop sheet.range(str4),*Formula=str25
        if (area2 > C0)
                move    area2,str12
                call    Trim using str12
                pack    str4,"D",str12
                getprop sheet.range(str4),*Formula=str25
                setprop sheet.range(str4),*Formula=str25
                pack    str4,"H",str12
                getprop sheet.range(str4),*Formula=str25
                setprop sheet.range(str4),*Formula=str25
        endif
        if (area3 > C0)
                move    area3,str12
                call    Trim using str12
                pack    str4,"D",str12
                getprop sheet.range(str4),*Formula=str25
                setprop sheet.range(str4),*Formula=str25
                pack    str4,"H",str12
                getprop sheet.range(str4),*Formula=str25
                setprop sheet.range(str4),*Formula=str25
        endif
.END PATCH 1.55.3 MOVED LOGIC
CampaignFileNameSelect
.START PATCH 1.53 REPLACED LOGIC
          setprop ex,*Visible="True"
.END PATCH 1.53 REPLACED LOGIC
        clear   taskname
        append  "\\nins1\d\USERS",taskname
        call    Trim using StrPtr1
        if (StrPtr1 <> "")
                append  "\",taskname
                append  StrPtr1,taskname
        endif
.        append  "\",taskname
..START PATCH 1.1 - REPLACED LOGIC
..        append  NCMPNUM,taskname
.        call    Trim using NCMPCNAME
.        append  NCMPCNAME,taskname
..END PATCH 1.1 - REPLACED LOGIC
.        reset   taskname
.        setprop ex,*DefaultFilePath=taskname
.START PATCH 1.1 - REPLACED LOGIC
.        append  NCMPNUM,taskname
.END PATCH 1.1 - REPLACED LOGIC
        append  "\",taskname                                          ."
        reset   taskname
        setprop ex,*DefaultFilePath=taskname
        call    Trim using NCMPCNAME
        pack    taskname,taskname,NCMPCNAME
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
.                sheet.saveas giving N9 using *Filename=taskname
                book.saveas giving N9 using *Filename=taskname
                trapclr Object

        endif
.START PATCH 1.5 ADDED LOGIC
.START PATCH 1.53 REPLACED LOGIC
.         setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.END PATCH 1.53 REPLACED LOGIC
.END PATCH 1.5 ADDED LOGIC
.START PATCH 1.55.5 ADDED LOGIC
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.END PATCH 1.55.5 ADDED LOGIC
.        book.printout
CampaignCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.START PATCH 1.3 ADDED LOGIC
          destroy Zoom85
.END PATCH 1.3 ADDED LOGIC
        destroy sortcol
        destroy sortcol1
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
.START PATCH 1.1 - REMMED LOGIC
.        ex.quit
.END PATCH 1.1 - REMMED LOGIC
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
                pack    taskname,str50," already exists and is open!!",carr,"Select another Filename!!"
                alert   caution,taskname,result
.                goto CampaignFileNameSelect
        endif
.Send them back to select another File name and try to Save again.
        goto CampaignFileNameSelect
.        goto CampaignCleanUp
TrapCampaignObject1
          Noreturn
          getinfo exception,taskname
          Move      "Object Error from ncmp0002",MailSubjct
          pack      Mailfrom from User,"@nincal.com"
.          Move      "JoseDuenas@nincal.com,Dherric@nincal.com,",MailTo
          Move      "Dherric@nincal.com,",MailTo
          Clear     Mailbody
          Append    error,mailbody
          append    CRLF,Mailbody
          Move      taskname,MailBody
          reset     Mailbody
          call      SendMail
          
errortrap
.testing purposes
        getinfo exception,taskname
        return

LoadDetail
.Prep work
        move    howmany,str9
        call    Trim using str9
.
        pack    str4,"A",str9
        setprop sheet.range(str4,str4),*Value=NLOLLIST,*HorizontalAlignment=AlignLeft
.
        pack    str4,"B",str9
        setprop sheet.range(str4,str4),*Value=OLRN,*HorizontalAlignment=AlignLeft
.
        pack    str4,"C",str9
        move    C1,NDATPATH
        move    NLOLLIST,NDATFLD
        move    "LoadLOLDetail-NDATKEY",Location
        pack    KeyLocation,"Key: ",NDATFLD
        call    NDATKEY
        if not over
                call    Trim using OLSTNAME
.                setprop sheet.range(str4,str4),*Value=OLSTNAME
          
                    if        ((ELSTCDE = "P" | ELSTCDE = "C") & NDATWEB <> "1")
                    Pack      Taskname from "=Hyperlink(#"http://www.ninlists.com/Datacards/Data",olnum,".htm","#",#"",OLSTNAME,"#")"
                    setprop         sheet.Range(str4,str4),*Formula=taskname
                    else
                    setprop sheet.range(str4,str4),*Value=OLSTNAME
                    endif

        endif
.
        pack    str4,"D",str9
        call    Trim using NLOLSELECT
        setprop sheet.range(str4,str4),*Value=NLOLSELECT
.
        pack    str4,"E",str9
        setprop sheet.range(str4),*NumberFormat="##,####0_);[Red](##,####0)"
        type    NLOLUNIVERSE
        if equal
                if (NLOLUNIVERSE <> "000000000")
                        setprop sheet.range(str4),*Value=NLOLUNIVERSE
                endif
        endif
.
        pack    str4,"F",str9
        setprop sheet.range(str4),*NumberFormat="##,####0_);[Red](##,####0)"
        type    NLOLQTY
        if equal
                if (NLOLQTY <> "000000000")
                        setprop sheet.range(str4),*Value=NLOLQTY
                endif
        endif
.
.START PATCH 1.2 REPLACED LOGIC
.        pack    str4,"G",str9
.        setprop sheet.range(str4),*NumberFormat="##,####0_);[Red](##,####0)"
.        type    NLOLNETQTY
.        if equal
.                if (NLOLNETQTY <> "000000000")
.                        setprop sheet.range(str4),*Value=NLOLNETQTY
.                endif
.        endif
..
.        pack    str4,"H",str9
.        setprop sheet.range(str4),*NumberFormat="##,####0.00%_);_(#"-#"_)"      ."##.####%"
.        move    C0,N35
.        move    NLOLNET,N35
.        if (N35 <> C0)
.                div     "100",N35
.                move    N35,str10
.                setprop sheet.range(str4),*Value=str10
.        endif
..
..        pack    str4,"I",str9
.        if (NLOLRENT = "1")
.                move    "Exchange",str15
.        elseif (NLOLRENT = "2")
.                move    "Rent",str15
.        elseif (NLOLRENT = "3")
.                move    "Exc/Rent",str15
.        else
.                clear   str15
.        endif
..        setprop sheet.range(str4,str4),*Value=str15
..
.        pack    str4,"I",str9
.        if (OSTAT = "" | OSTAT = "z" | (OSTAT = "l" & OHIST <> "E"))
..Clear this variable if LOL Or Cancelled/Denied - Later logic will determine proper display
..Clear this variable if LCR not Cleared.
.                clear    str15
.        else
.                if (OCLRSTAT = "1")
.                        move    "Exchange",str15
.                elseif (OCLRSTAT = "2")
.                        move    "Rent",str15
.                elseif (OCLRSTAT = "3")
.                        move    "Exc/Rent",str15
.                else
.Do nothing.  Live Orders should have previous instance of str15 used.
..                        clear   str15
.                endif
.        endif
.        setprop sheet.range(str4,str4),*Value=str15
......................
        if (NLOLRENT = "1")
                move    "Exchange",str15
        elseif (NLOLRENT = "2")
                move    "Rent",str15
        elseif (NLOLRENT = "3")
                move    "Exc/Rent",str15
        else
                clear   str15
        endif
        pack    str4,"G",str9
        if (OSTAT = "" | OSTAT = "z" | (OSTAT = "l" & OHIST <> "E"))
.Clear this variable if LOL Or Cancelled/Denied - Later logic will determine proper display
.Clear this variable if LCR not Cleared.
                clear    str15
        else
                if (OCLRSTAT = "1")
                        move    "Exchange",str15
                elseif (OCLRSTAT = "2")
                        move    "Rent",str15
                elseif (OCLRSTAT = "3")
                        move    "Exc/Rent",str15
                else
.Do nothing.  Live Orders should have previous instance of str15 used.
.                        clear   str15
                endif
        endif
        setprop sheet.range(str4,str4),*Value=str15
.
        pack    str4,"H",str9
        setprop sheet.range(str4),*NumberFormat="##,####0.00%_);_(#"-#"_)"      ."##.####%"
        move    C0,N35
.START PATCH 1.3 ADDED LOGIC
          call      Trim using NLOLNET
.END PATCH 1.3 ADDED LOGIC
        move    NLOLNET,N35
        if (N35 <> C0)
                div     "100",N35
                move    N35,str10
                setprop sheet.range(str4),*Value=str10
        endif
.
        pack    str4,"I",str9
        setprop sheet.range(str4),*NumberFormat="##,####0_);[Red](##,####0)"
        type    NLOLNETQTY
        if equal
                if (NLOLNETQTY <> "000000000")
                        setprop sheet.range(str4),*Value=NLOLNETQTY
                endif
        endif
.END PATCH 1.2 REPLACED LOGIC
.
        pack    str4,"J",str9
.START PATCH 1.55.4 REPLACED LOGIC - TEMPORARY PATCH
.        move    NCMPMLR,OMLRNUM
        move    NCMPMLRHold,OMLRNUM
.END PATCH 1.55.4 REPLACED LOGIC - TEMPORARY PATCH
        move    NLOLLIST,OLNUM
        call    OrderTestXSTAT
        call    Trim using taskname
...............................
        if (OLRN <> "" & (taskname = "" | taskname = "NO EXCHANGE HISTORY" | taskname = "No Exchange History" ))
.I can do the following as taskname=NULL and is not filled until after second pointer is used
.Check out subroutine and trace DimPtr/DimPtr1 to see what I mean.
.START PATCH 1.55.4 REPLACED LOGIC - TEMPORARY PATCH
.                pack    NORDFLD1,"01R",NCMPMLR
                pack    NORDFLD1,"01R",NCMPMLRHold
.END PATCH 1.55.4 REPLACED LOGIC - TEMPORARY PATCH
                pack    NORDFLD2,"02R",NLOLLIST
                clear   NORDFLD3
                clear   NORDFLD4
                call    CampOrderGetHistory using taskname,OLRN,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,C0
        endif
...............................
        setprop sheet.range(str4,str4),*Value=taskname
.
        pack    str4,"K",str9
        call    Trim using DESC004
        setprop sheet.range(str4,str4),*Value=DESC004
.
        pack    str4,"L",str9
        call    Trim using DESC003
        setprop sheet.range(str4,str4),*Value=DESC003
.
        pack    str4,"M",str9
        clear   str25
        if (OSTAT = "0")
                move    "Order",str25
        elseif (OSTAT = "p")
                move    "Pending Order",str25
        elseif (OSTAT = "x")
                move    "Cancelled Pending Order",str25
        elseif (OSTAT = "l")
                if (OCO2CODE <> "" & OCO2CODE <> "  ")
                        move    "In-House LCR",str25
                else
                        move    "LCR",str25
                endif
        elseif (OSTAT = "z")
                move    "Denied/Cancelled LCR",str25
        elseif (OSTAT = "B")
                move    "Billed Order",str25
        elseif (OSTAT = "Q")
                move    "Cancelled Billed Order",str25
        elseif (OSTAT = "X")
                move    "Cancelled Order",str25
        endif
        setprop sheet.range(str4,str4),*Value=str25
.
        clear   NPNDDESC
.NINORD5/NINORD4 File
        if (OSTAT = "l" | OSTAT = "z")
                move    OLRN,NORD5FLD
                rep     zfill,NORD5FLD
                move    "LoadDetail-NORD5KEY",Location
                pack    KeyLocation,"Key: ",NORD5FLD
                call    NORD5KEY
                if over
.                        move    "No Status Found!",NPNDDESC
                else
.NINPND File
.                        pack    NPNDFLD,OSTAT,NORD5STAT
                        pack    NPNDFLD,"l",NORD5STAT
                        rep     zfill,NPNDFLD
                        move    "LoadDetail,2-NPDNKEY",Location
                        pack    KeyLocation,"Key: ",NPNDFLD
                        call    NPNDKEY
                        if over
.                                move    "No Status Found!",NPNDDESC
                        else
                                if (OSTAT = "z" | (OSTAT = "l" & (NORD5STAT = "05" | NORD5STAT = "07")))
.START PATCH 1.2 REPLACED LOGIC
.                                        pack    str4,"I",str9
                                        pack    str4,"G",str9
.END PATCH 1.2 REPLACED LOGIC
                                        call    Trim using NPNDDESC
                                        setprop sheet.range(str4,str4),*Value=NPNDDESC
.Strike Through Quantities
                                        pack    str4,"F",str9
                                        setprop sheet.range(str4,str4).Font,*Strikethrough="True"
.START PATCH 1.2 REPLACED LOGIC
.                                        pack    str4,"G",str9
                                        pack    str4,"I",str9
.END PATCH 1.2 REPLACED LOGIC
                                        setprop sheet.range(str4,str4).Font,*Strikethrough="True"
.Strike Through List Names
                                        pack    str4,"C",str9
                                        setprop sheet.range(str4,str4).Font,*Strikethrough="True"
                                 endif
                        endif
                endif
        elseif (OSTAT = "p" | OSTAT = "x")
                move    OLRN,NORD4FLD
                rep     zfill,NORD4FLD
                move    "LoadDetail-NORD4KEY",Location
                pack    KeyLocation,"Key: ",NORD4FLD
                call    NORD4KEY
                if over
.                        move    "No Status Found!",NPNDDESC
                else
.NINPND File
                        pack    NPNDFLD,OSTAT,NORD4STAT
                        rep     zfill,NPNDFLD
                        move    "LoadDetail-NPDNKEY",Location
                        pack    KeyLocation,"Key: ",NPNDFLD
                        call    NPNDKEY
                        if over
.                                move    "No Status Found!",NPNDDESC
                        endif                
                endif
        endif
        call    Trim using NPNDDESC
        pack    str4,"N",str9
        setprop sheet.range(str4,str4),*Value=NPNDDESC
.
        pack    str4,"O",str9
        call    TRIM using NLOLMDATE
.START PATCH 1.3 REPLACED LOGIC
.        if (NLOLMDATE <> "")
.                unpack  NLOLMDATE,CC,YY,MM,DD
.                pack    str10,MM,SLASH,DD,SLASH,CC,YY
.        else
.                clear   str10
.        endif
          if (NLOLMDATE <> "" AND (OSTAT = "0" | OSTAT = "B" | OSTAT = "Q" | OSTAT = "X"))
                unpack  NLOLMDATE,CC,YY,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,CC,YY
        else
                clear   str10
        endif
.END PATCH 1.3 REPLACED LOGIC
        setprop sheet.range(str4,str4),*Value=str10,*HorizontalAlignment=AlignLeft
.
.Used for search purposes only
        pack    str4,"Z",str9
        if (NLOLTEST <> "")
                rep     " 0",NLOLTEST
        else
                move    C0,NLOLTEST
        endif
.START PATCH 1.2 ADDED LOGIC
        if (NLOLTEST = "1")  .TEST
                move    "2",NLOLTEST
        elseif (NLOLTEST = "2")  .RETEST
                move    "1",NLOLTEST
        endif
.END PATCH 1.2 ADDED LOGIC
        setprop sheet.range(str4,str4),*Value=NLOLTEST
.
        add     C1,howmany
        return
        
        include ncmpio.inc
        include nordio.inc
        include nord4io.inc
        include nord5io.inc
        include nspe2io.inc
        include nlolio.inc
        include nlolio2.inc
        include npndio.inc
.START PATCH 1.54 REPLACED LOGIC
.        include nmlrio.inc
.        include nbrkio.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
.END PATCH 1.54 REPLACED LOGIC
        include nrtnio.inc
        include nxrfio.inc
        include nxngio.inc
        include nxchio.inc
        include ndatio.inc
        include nofrio.inc
        include nsmpio.inc
        include ncntio.inc
.START PATCH 1.52 ADDED LOGIC
          INCLUDE   NSEL2IO.INC
.END PATCH 1.52 ADDED LOGIC
        include comlogic.inc
