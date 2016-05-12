.CAMPAIGN SPREADSHEET INCLUDING PROJECTIONS - THE NATURE CONSERVANCY TEMPLATE
        include common.inc
        include cons.inc
        include ncmpdd.inc
        include norddd.inc
.        include f:\library\develop\backups\norddd.inc
        include nord4dd.inc
        include nord5dd.inc
        include nspe2dd.inc
        include npnddd.inc
        include nloldd.inc
          include npkgdd.inc
          include   nprcdd.inc
          include   statsdd.inc
          include   statndd.inc
.START PATCH 1.33 REPLACED LOGIC
.        include nmlrdd.inc
.        include nbrkdd.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
.END PATCH 1.33 REPLACED LOGIC
        include nrtndd.inc
        include nxrfdd.inc
        include nxngdd.inc
        include nxchdd.inc
.        include f:\library\develop\backups\nxngdd.inc
.        include f:\library\develop\backups\nxchdd.inc
        include ndatdd.inc
        include nofrdd.inc
        include nsmpdd.inc
        include ncntdd.inc
        include oslspern.inc
        include media.inc
.START PATCH 1.32 ADDED LOGIC
          INCLUDE   NSEL2DD.INC
.END PATCH 1.32 ADDED LOGIC

Release    INIT    "1.53"               DLH      .Excel 2013 *WindowState=xlMinimized
Reldate   Init      "2014 January 22"
.release init    "1.5.2"                08Mar2007  DLH      Oslspern.inc expansion
.Release  init      "1.5.1"   27JAN2005 ASH       CONVERTED STATS FILE MAILER TO 6 BYTES
.Release  init      "1.5"     01DEC2004 ASH       CONVERTED CAMPAIGN MAILER TO 6 BYTES
.Release  init      "1.4"     25OCT2004 ASH       CONVERTED MAILER PACKAGE TO 6 BYTES
.release init    "1.33"           27MAY04 ASH     MAILER CONVERSION
.release init    "1.32"           26JAN04 ASH     DATACARD CONVERSION
.release init    "1.31"           19NOV02 ASH     Added more new features, including Worksheet Menu Bar
.release init    "1.3"           15NOV02 ASH     Added new features
.release init    "1.22"       ASH 19APR2002 REMOVED CANCELLED/DENIED RECORDS FROM SPREADSHEET
.release init    "1.21"       ASH 05APR2002 INCREASE SIZE OF TASKNAME2
.release init    "1.2"        ASH 27MAR2002 ADDED NET RECEIVED COLUMN
.                                             INCLUDED MASTER PACKAGE LOGIC
.release init    "1.1"        ASH 27FEB2002 FILTERED OUT PACKAGES THAT DON'T APPLY TO CURRENT CAMPAIGN - THUS SAVING TIME/SPACE
.                                             ADDED LOGIC TO FILTER OUT MASTER PACKAGE RECORD
.                                             ADDED VARIABLE TO INCREASE RANGE POSSIBILITIES
.release init    "1.0"        ASH 06SEP2001 NEW RELEASE
.EXTERNAL ROUTINES FROM NORDTEST.PLC
CampOrderGetHistory external "NORDTEST;OrderGetHistory"
.
.START PATCH 1.1 ADDED VAR
PackData DataList
howmany5  form      9
range1    dim       10
range2    dim       10
range3    dim       10
range4    dim       10
.END PATCH 1.1 ADDED VAR
N35     form    3.5
RecFlag   dim       1
.START PATCH 1.21 REPLACED LOGIC
.taskname2 dim      500
taskname2 dim       1000
.END PATCH 1.21 REPLACED LOGIC

ComboPtr ComboBox ^
.WindPtr window  ^
StrPtr  dim     ^
StrPtr1 dim     ^
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
sheet1  automation
sheet2  automation
sheet3  automation
sheet4  automation
sheet5  automation
sortcol automation
sortcol1 automation
ex      automation      class="Excel.Application"
.
mauve     color
colornum form       24
yellow    color
.Variables needed by OrderLoadXSTAT
EFLAG   dim     1
AKey1A  init    "01L"
AKey2A  init    "02L"
Carr    init    0x7f    .Carriage Return used by PLB
FirstRec form   8
LastRec   form      9
LastRec2 form       9
hdrrow    form      9
TotLine   form      9
FirstPack form      9
LastPack form       9
TotPack   form      9
MASTERMQTY form     10
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
.AcctType variant
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignLeft integer 4,"0xffffefdd"
AlignRight integer 4,"0xffffefc8"
AlignCenter integer 4,"0xffffeff4"
AutoCalc integer 4,"0xffffeff7"
SheetsDefault integer 4,"0x00000000"
xlLandscape integer 4,"0x2"                     .2
xlMinimized integer 4,"0xFFFFEFD4"
xlNormal integer 4,"0xFFFFEFD1"
xlMaximized integer 4,"0xFFFFEFD7"
xlUnderlineStyleSingle integer 4,"0x2"
onehun    integer   4,"0x100"
DblLine integer 4,"0xffffefe9"
MedThick integer 4,"0xFFFFEFD6"
BorderHor integer 4,"0xc"
BorderVert integer 4,"0xb"
.START PATCH 1.5 ADDED LOGIC
NCMPMLRHold         dim       4
.END PATCH 1.5 ADDED LOGIC        
        
CreateCampaignA Routine StrPtr,ComboPtr,StrPtr1
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
.Colors
        create  mauve=150:60:100
          getitem   mauve,0,colornum
          create    yellow=255:255:160
.Initialize variables
        pack    NCMPFLD,StrPtr
        move    C1,NCMPPATH
        move    "CreateC.-NCMPKEY",Location
        pack    KeyLocation,"Key: ",NCMPFLD
        call    NCMPKEY
        if over
                return
        endif
.START PATCH 1.1 ADDED LOGIC
.Create work var
          create    PackData=1:1:1:1
.END PATCH 1.1 ADDED LOGIC
.Open Excel application
        create  ex
.begin patch 1.53
.        setprop ex,*WindowState=xlMinimized
.end patch 1.53
.START PATCH 1.31 REPLACED LOGIC
..START PATCH 1.3 ADDED LOGIC
.         setprop ex,*IgnoreRemoteRequests="True",*Interactive="False"
..END PATCH 1.3 ADDED LOGIC
        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
          setprop ex.CommandBars("Standard"),*Visible="True"
          setprop ex.CommandBars("Formatting"),*Visible="True"
          setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.END PATCH 1.31 REPLACED LOGIC
.Reset Default of Worksheets found in a Workbook
        getprop ex,*SheetsInNewWorkbook=SheetsDefault
        setprop ex,*SheetsInNewWorkbook=C5
.Create Workbooks collection
        getprop ex,*Workbooks=books
.Create/Add a single Workbook
        books.add
        books.item giving book using 1
.Create Worksheets collection
        getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
        sheets.item giving sheet1 using 1
          setprop   sheet1,*Name="QUICKVIEW"
        sheets.item giving sheet2 using 2
          setprop   sheet2,*Name="ALPHA"
        sheets.item giving sheet3 using 3
          setprop   sheet3,*Name="PKG SORT"
        sheets.item giving sheet4 using 4
          setprop   sheet4,*Name="ALPHA Collapsed"
        sheets.item giving sheet5 using 5
          setprop   sheet5,*Name="Pkg Costs"
        setprop sheet1.PageSetup,*Orientation=xlLandscape
        setprop sheet2.PageSetup,*Orientation=xlLandscape
        setprop sheet3.PageSetup,*Orientation=xlLandscape
        setprop sheet4.PageSetup,*Orientation=xlLandscape
        setprop sheet5.PageSetup,*Orientation=xlLandscape
.        setprop sheet1.PageSetup,*CenterFooter=" Page &P of &N"
.Set Viewing Sizes
.As the first sheet is always inherently active I do not need to activate it
.    sheet1.activate
.As the Zoom property is automatically set to 100% I do not need to set it
.    setprop book.windows(1),*Zoom=Zoom100
          sheet2.activate
        setprop book.windows(1),*Zoom=Zoom75
          sheet3.activate
        setprop book.windows(1),*Zoom=Zoom85
          sheet4.activate
        setprop book.windows(1),*Zoom=Zoom60,*View=2        .Set Page Break Preview
          sheet5.activate
        setprop book.windows(1),*Zoom=Zoom75
.Reset focus on the first Worksheet
          sheet1.activate
.
.Set Printing Sizes
        setprop sheet1.PageSetup,*Zoom=Zoom87
        setprop sheet2.PageSetup,*Zoom=Zoom70
        setprop sheet3.PageSetup,*Zoom=Zoom97
        setprop sheet4.PageSetup,*Zoom=Zoom32
        setprop sheet5.PageSetup,*Zoom=Zoom125
.Clear any garbage that might be present - this is redundant since we have created a new Worksheet
.        sheet2.range("A1","IV1000").Clear
.Greatest column possible is "IV"
          setprop   sheet1.Range("A1","IV1000").Font,*Size=10
          setprop   sheet2.Range("A1","IV1000").Font,*Size=12
          setprop   sheet3.Range("A1","IV1000").Font,*Size=12
          setprop   sheet4.Range("A1","IV1000").Font,*Size=12
          setprop   sheet5.Range("A1","IV1000").Font,*Size=10

.....Campaign Header.....
.Header Column 1
.We could create a Range automation object but do not have to.
.Instead we use the Range property to dynamically return a Range object each time we want
.to dump in a value, or set another property of a cell(s).
..................................
.START PATCH 1.5 ADDED LOGIC
.        pack    MKEY,NCMPMLR,"000"
.        move    "CreateCamp.-NMLRKEY",Location
.        pack    KeyLocation,"Key: ",MKEY
.        call    NMLRKEY
.        call    Trim using MCOMP
.        setprop sheet2.range("D1"),*Value=MCOMP
.        setprop sheet2.range("D1").Font,*Bold="True"
.        setprop sheet3.range("D1"),*Value=MCOMP
.        setprop sheet3.range("D1").Font,*Bold="True"
.        setprop sheet4.range("B1"),*Value=MCOMP
.        setprop sheet4.range("B1").Font,*Bold="True"
...........................................
        pack    COMPFLD,NCMPMLR
        move    "CreateCamp.-COMPKEY",Location
        pack    KeyLocation,"Key: ",COMPFLD
        call    COMPKEY
        call    Trim using COMPCOMP
        setprop sheet2.range("D1"),*Value=COMPCOMP
        setprop sheet2.range("D1").Font,*Bold="True"
        setprop sheet3.range("D1"),*Value=COMPCOMP
        setprop sheet3.range("D1").Font,*Bold="True"
        setprop sheet4.range("B1"),*Value=COMPCOMP
        setprop sheet4.range("B1").Font,*Bold="True"
.Following used as a temporary measure until all files have had Mailer field converted
          move      COMPOLDMLR,NCMPMLRHold
.END PATCH 1.5 ADDED LOGIC
        call    Trim using NCMPCNAME
        setprop sheet2.range("D2"),*Value=NCMPCNAME
        setprop sheet2.range("D2").Font,*Bold="True"
        setprop sheet3.range("D2"),*Value=NCMPCNAME
        setprop sheet3.range("D2").Font,*Bold="True"
        setprop sheet4.range("B2"),*Value=NCMPCNAME
        setprop sheet4.range("B2").Font,*Bold="True"
..........Line Skip..........
          move      C0,N6
          move      NCMPMLR,N6
          move      N6,str6
          call      Trim using str6
          pack      str25,"MAILER##  ",str6
        setprop sheet2.range("D4"),*Value=str25
        setprop sheet3.range("D4"),*Value=str25
        setprop sheet4.range("B4"),*Value=str25
        move    C0,UNIVERSE
        clear   NXRFFLD2
        clear   NXRFLIST
.START PATCH 1.55 REPLACED LOGIC
.        move    MKEY,NXRFFLD2
        move    NCMPMLRHold,NXRFFLD2
.END PATCH 1.5 REPLACED LOGIC
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
          move      C0,N6
          move      NXRFLIST,N6
          move      N6,str6
          call      Trim using str6
          pack      str25,"LIST##  ",str6
        setprop sheet2.range("D5"),*Value=str25
        setprop sheet3.range("D5"),*Value=str25
        setprop sheet4.range("B5"),*Value=str25
.        setprop sheet2.range("B13"),*Value="Comments:"
..Routine for simulating Carriage Return capability
.        move    "13",firstrow
.        call    Trim using NCMPCOMMENT
.        movelptr NCMPCOMMENT,N6
.        loop
.                move    firstrow,str9
.                call    Trim using str9
.                pack    str4,"C",str9
.                movefptr NCMPCOMMENT,N5
.                scan    carr,NCMPCOMMENT
.                if not equal
.                        setprop sheet2.range(str4),*Value=NCMPCOMMENT
.                        add     C1,firstrow
.                        break
.                else
..Reset string to extract section prior to Carriage Return
.                        movefptr NCMPCOMMENT,N4
.                        if (N4 > N5)
.                                sub     C1,N4
.                        else
.                                if (N4 <> N6)
.                                        add     C1,N4
.                                endif
.                        endif
.                        setlptr NCMPCOMMENT,N4
.                        reset   NCMPCOMMENT,N5
.                        cmatch  carr,NCMPCOMMENT
.                        if equal
.                                if (N5 = N6 & N4 = N6)
.                                        add     C1,firstrow
.                                        break
.                                endif
.                                bump    NCMPCOMMENT
.                        else
.                                setprop sheet2.range(str4),*Value=NCMPCOMMENT
..Reset string to start at next section.
.                                add     C2,N4,N5
.                                reset   NCMPCOMMENT,N5
.                        endif
.                        setlptr NCMPCOMMENT,N6
.                        add     C1,firstrow
.                endif
.        repeat

.Header Column 2
..........Line Skip..........
        setprop sheet2.range("F2"),*Value="Alpha Sort"
        setprop sheet2.range("F2").Font,*Bold="True",*Italic="True"
        setprop sheet3.range("F2"),*Value="Package Sort"
        setprop sheet3.range("F2").Font,*Bold="True",*Italic="True"
        setprop sheet4.range("D2"),*Value="Alpha Collapsed Sort"
        setprop sheet4.range("D2").Font,*Bold="True",*Italic="True"
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
          pack      str55,"MEDIA:  ",MEDIA
        setprop sheet2.range("F4"),*Value=str55
        setprop sheet3.range("F4"),*Value=str55
        setprop sheet4.range("D4"),*Value=str55
        call    Trim using NCMPSHIP
        clear   str55
        if (NCMPSHIP <> "")
                move    C0,N2
                move    NCMPSHIP,N2
                add     C2,N2
                getitem ComboPtr,N2,str55
        endif
          pack      taskname,"SHIP VIA:  ",str55
        setprop sheet2.range("F5"),*Value=taskname
        setprop sheet3.range("F5"),*Value=taskname
        setprop sheet4.range("D5"),*Value=taskname
..........Line Skip..........
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
        endif
        call    Trim using RTCOMP
          pack      taskname,"SHIP TO:  ",RTCOMP
        setprop sheet2.range("F7"),*Value=taskname
        setprop sheet3.range("F7"),*Value=taskname
        setprop sheet4.range("D7"),*Value=taskname
          pack      str55,"MP/P.O.##:  ",NCMPPO
        setprop sheet2.range("F8"),*Value=str55
        setprop sheet3.range("F8"),*Value=str55
        setprop sheet4.range("D8"),*Value=str55
.Header Column 3
..........Line Skip..........
..........Line Skip..........
          pack      str55,"Order Date:  "
        setprop sheet2.range("I3"),*Value=str55
        setprop sheet3.range("I3"),*Value=str55
        call    Trim using NCMPRDATE
        if (NCMPRDATE <> "")
                unpack  NCMPRDATE,str4,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,str4
          else
                    clear     str10
        endif
          pack      str55,"Return Date:  ",str10
        setprop sheet2.range("I4"),*Value=str55
        setprop sheet3.range("I4"),*Value=str55
        call    Trim using NCMPMDATE
        if (NCMPMDATE <> "")
                unpack  NCMPMDATE,str4,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,str4
          else
                    clear     str10
        endif
          pack      str55,"Mail Date:  ",str10
        setprop sheet2.range("I5"),*Value=str55
        setprop sheet3.range("I5"),*Value=str55
..........Line Skip..........
        move    C1,NCNTPATH
        move    NCMPCNT,NCNTFLD
        move    "CreateCamp.-NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
        call    NCNTKEY
        if over
                clear   CNTNAME
        endif
        call    Trim using CNTNAME
          pack      taskname,"NIN CONTACT:  ",CNTNAME
        setprop sheet2.range("I7"),*Value=taskname
        setprop sheet3.range("I7"),*Value=taskname
        move    NCMPPLANNER,N9
        move    osls0,str45
        load    str45 from N9 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
                osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
                osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
        call    Trim using str45
          pack      taskname,"NIN SALES:  ",str45
        setprop sheet2.range("I8"),*Value=taskname
        setprop sheet3.range("I8"),*Value=taskname
.Header Column 4
..........Line Skip..........
..........Line Skip..........
..........Line Skip..........
        call    Trim using NCMPCDATE
        if (NCMPCDATE <> "")
                unpack  NCMPCDATE,str4,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,str4
          else
                    clear     str10
        endif
          pack      str55,"Cutoff:  ",str10
        setprop sheet2.range("K4"),*Value=str55
        setprop sheet3.range("K4"),*Value=str55
.Additional Information
.START PATCH 1.2 REPLACED LOGIC
.        setprop sheet2.range("AE6"),*Value="Note: All lists will include omit instructions",*HorizontalAlignment=AlignRight
.        setprop sheet2.range("AE7"),*Value="when universe warrants omit.",*HorizontalAlignment=AlignRight
.        setprop sheet3.range("AE6"),*Value="Note: All lists will include omit instructions",*HorizontalAlignment=AlignRight
.        setprop sheet3.range("AE7"),*Value="when universe warrants omit.",*HorizontalAlignment=AlignRight
..
.        setprop sheet2.range("AI2"),*Value="LIST COST per M",*HorizontalAlignment=AlignCenter
.        setprop sheet2.range("AI3"),*Value="CALCULATION TABLE",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range("AI2"),*Value="LIST COST per M",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range("AI3"),*Value="CALCULATION TABLE",*HorizontalAlignment=AlignCenter
..
.        setprop sheet2.range("AL5"),*Value="In Rental Column",*HorizontalAlignment=AlignRight
.        setprop sheet2.range("AL6"),*Value="N/A = List is Exchange Only",*HorizontalAlignment=AlignRight
.        setprop sheet3.range("AL5"),*Value="In Rental Column",*HorizontalAlignment=AlignRight
.        setprop sheet3.range("AL6"),*Value="N/A = List is Exchange Only",*HorizontalAlignment=AlignRight
............................
        setprop sheet2.range("AF6"),*Value="Note: All lists will include omit instructions",*HorizontalAlignment=AlignRight
        setprop sheet2.range("AF7"),*Value="when universe warrants omit.",*HorizontalAlignment=AlignRight
        setprop sheet3.range("AF6"),*Value="Note: All lists will include omit instructions",*HorizontalAlignment=AlignRight
        setprop sheet3.range("AF7"),*Value="when universe warrants omit.",*HorizontalAlignment=AlignRight
.
        setprop sheet2.range("AJ2"),*Value="LIST COST per M",*HorizontalAlignment=AlignCenter
        setprop sheet2.range("AJ3"),*Value="CALCULATION TABLE",*HorizontalAlignment=AlignCenter
        setprop sheet3.range("AJ2"),*Value="LIST COST per M",*HorizontalAlignment=AlignCenter
        setprop sheet3.range("AJ3"),*Value="CALCULATION TABLE",*HorizontalAlignment=AlignCenter
.
        setprop sheet2.range("AM5"),*Value="In Rental Column",*HorizontalAlignment=AlignRight
        setprop sheet2.range("AM6"),*Value="N/A = List is Exchange Only",*HorizontalAlignment=AlignRight
        setprop sheet3.range("AM5"),*Value="In Rental Column",*HorizontalAlignment=AlignRight
        setprop sheet3.range("AM6"),*Value="N/A = List is Exchange Only",*HorizontalAlignment=AlignRight
.END PATCH 1.2 REPLACED LOGIC
.....Record Header.....
          move      "11",FirstRec
          move      FirstRec,hdrrow
        move    FirstRec,str9
        call    Trim using str9
          getprop   sheet2.rows(FirstRec),*RowHeight=N32
          getprop   sheet3.rows(FirstRec),*RowHeight=N32
          getprop   sheet4.rows(FirstRec),*RowHeight=N32
          mult      C2,N32
          setprop   sheet2.rows(FirstRec),*RowHeight=N32
          setprop   sheet3.rows(FirstRec),*RowHeight=N32
          setprop   sheet4.rows(FirstRec),*RowHeight=N32
        pack    range1,"A",str9
        setprop sheet2.range(range1),*Value="ACTION",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="ACTION",*HorizontalAlignment=AlignCenter
        setprop sheet4.range(range1),*Value="TNC ##",*HorizontalAlignment=AlignCenter
        pack    range1,"B",str9
        setprop sheet2.range(range1),*Value="LCR",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="LCR",*HorizontalAlignment=AlignCenter
        setprop sheet4.range(range1),*Value="List ##",*HorizontalAlignment=AlignCenter
        pack    range1,"C",str9
        setprop sheet2.range(range1),*Value="TNC ##",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="TNC ##",*HorizontalAlignment=AlignCenter
        setprop sheet4.range(range1),*Value="List",*HorizontalAlignment=AlignCenter
        pack    range1,"D",str9
        setprop sheet2.range(range1),*Value="List ##",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="List ##",*HorizontalAlignment=AlignCenter
        setprop sheet4.range(range1),*Value="Select",*HorizontalAlignment=AlignCenter
        pack    range1,"E",str9
        setprop sheet2.range(range1),*Value="List",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="List",*HorizontalAlignment=AlignCenter
        setprop sheet4.range(range1),*Value="Reco Qty",*HorizontalAlignment=AlignCenter
        pack    range1,"F",str9
        setprop sheet2.range(range1),*Value="Select",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Select",*HorizontalAlignment=AlignCenter
        setprop sheet4.range(range1),*Value="Rec'd Qty",*HorizontalAlignment=AlignCenter
        pack    range1,"G",str9
        setprop sheet2.range(range1),*Value="Cost Mbr",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Cost Mbr",*HorizontalAlignment=AlignCenter
        setprop sheet4.range(range1),*Value="Exch Stat",*HorizontalAlignment=AlignCenter
        pack    range1,"H",str9
        setprop sheet2.range(range1),*Value="LAST USE",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="LAST USE",*HorizontalAlignment=AlignCenter
        setprop sheet4.range(range1),*Value="Aver Net",*HorizontalAlignment=AlignCenter
        pack    range1,"I",str9
        setprop sheet2.range(range1),*Value="Universe",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Universe",*HorizontalAlignment=AlignCenter
        setprop sheet4.range(range1),*Value="Net Names",*HorizontalAlignment=AlignCenter
        pack    range1,"J",str9
        setprop sheet2.range(range1),*Value="Package",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Package",*HorizontalAlignment=AlignCenter
        pack    range1,"K",str9
        setprop sheet2.range(range1),*Value="P Cost",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="P Cost",*HorizontalAlignment=AlignCenter
        pack    range1,"L",str9
        setprop sheet2.range(range1),*Value="Reco Qty",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Reco Qty",*HorizontalAlignment=AlignCenter
        pack    range1,"M",str9
        setprop sheet2.range(range1),*Value="Rec'd Qty",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Rec'd Qty",*HorizontalAlignment=AlignCenter
        pack    range1,"N",str9
        setprop sheet2.range(range1),*Value="Diff",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Diff",*HorizontalAlignment=AlignCenter
        pack    range1,"O",str9
        setprop sheet2.range(range1),*Value="In",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="In",*HorizontalAlignment=AlignCenter
        pack    range1,"P",str9
        setprop sheet2.range(range1),*Value="R/E",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="R/E",*HorizontalAlignment=AlignCenter
        pack    range1,"Q",str9
        setprop sheet2.range(range1),*Value="List CPM",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="List CPM",*HorizontalAlignment=AlignCenter
        pack    range1,"R",str9
        setprop sheet2.range(range1),*Value="Exch Stat",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Exch Stat",*HorizontalAlignment=AlignCenter
.START PATCH 1.2 ADDED LOGIC
.        pack    range1,"S",str9
.        setprop sheet2.range(range1),*Value="Net Req",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Net Req",*HorizontalAlignment=AlignCenter
.        pack    range1,"T",str9
.        setprop sheet2.range(range1),*Value="Aver Net",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Aver Net",*HorizontalAlignment=AlignCenter
.        pack    range1,"U",str9
.        setprop sheet2.range(range1),*Value="Net Names",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Net Names",*HorizontalAlignment=AlignCenter
.        pack    range1,"V",str9
.        setprop sheet2.range(range1),*Value="Resp R",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Resp R",*HorizontalAlignment=AlignCenter
.        pack    range1,"W",str9
.        setprop sheet2.range(range1),*Value="Rtns",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Rtns",*HorizontalAlignment=AlignCenter
.        pack    range1,"X",str9
.        setprop sheet2.range(range1),*Value="Gift",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Gift",*HorizontalAlignment=AlignCenter
.        pack    range1,"Y",str9
.        setprop sheet2.range(range1),*Value="Revenue",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Revenue",*HorizontalAlignment=AlignCenter
.        pack    range1,"Z",str9
.        setprop sheet2.range(range1),*Value="Prod Cost",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Prod Cost",*HorizontalAlignment=AlignCenter
.        pack    range1,"AA",str9
.        setprop sheet2.range(range1),*Value="Lst Cost",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Lst Cost",*HorizontalAlignment=AlignCenter
.        pack    range1,"AB",str9
.        setprop sheet2.range(range1),*Value="Tot Cost",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Tot Cost",*HorizontalAlignment=AlignCenter
.        pack    range1,"AC",str9
.        setprop sheet2.range(range1),*Value="Net +/-",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Net +/-",*HorizontalAlignment=AlignCenter
.        pack    range1,"AD",str9
.        setprop sheet2.range(range1),*Value="Market Code",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Market Code",*HorizontalAlignment=AlignCenter
.        pack    range1,"AE",str9
.        setprop sheet2.range(range1),*Value="Comments",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Comments",*HorizontalAlignment=AlignCenter
.        pack    range1,"AF",str9
.        setprop sheet2.range(range1),*Value="Ex Base",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Ex Base",*HorizontalAlignment=AlignCenter
.        pack    range1,"AG",str9
.        setprop sheet2.range(range1),*Value="Rent Base",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Rent Base",*HorizontalAlignment=AlignCenter
.        pack    range1,"AH",str9
.        setprop sheet2.range(range1),*Value="Select Fees",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Select Fees",*HorizontalAlignment=AlignCenter
.        pack    range1,"AI",str9
.        setprop sheet2.range(range1),*Value="Run Charge",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Run Charge",*HorizontalAlignment=AlignCenter
.        pack    range1,"AJ",str9
.        setprop sheet2.range(range1),*Value="Ship/Tape",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Ship/Tape",*HorizontalAlignment=AlignCenter
.        pack    range1,"AK",str9
.        setprop sheet2.range(range1),*Value="Ex Total",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Ex Total",*HorizontalAlignment=AlignCenter
.        pack    range1,"AL",str9
.        setprop sheet2.range(range1),*Value="Rent Total",*HorizontalAlignment=AlignCenter
.        setprop sheet3.range(range1),*Value="Rent Total",*HorizontalAlignment=AlignCenter
................................
        pack    range1,"S",str9
        setprop sheet2.range(range1),*Value="Net Rec",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Net Rec",*HorizontalAlignment=AlignCenter
        pack    range1,"T",str9
        setprop sheet2.range(range1),*Value="Net Req",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Net Req",*HorizontalAlignment=AlignCenter
        pack    range1,"U",str9
        setprop sheet2.range(range1),*Value="Aver Net",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Aver Net",*HorizontalAlignment=AlignCenter
        pack    range1,"V",str9
        setprop sheet2.range(range1),*Value="Net Names",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Net Names",*HorizontalAlignment=AlignCenter
        pack    range1,"W",str9
        setprop sheet2.range(range1),*Value="Resp R",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Resp R",*HorizontalAlignment=AlignCenter
        pack    range1,"X",str9
        setprop sheet2.range(range1),*Value="Rtns",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Rtns",*HorizontalAlignment=AlignCenter
        pack    range1,"Y",str9
        setprop sheet2.range(range1),*Value="Gift",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Gift",*HorizontalAlignment=AlignCenter
        pack    range1,"Z",str9
        setprop sheet2.range(range1),*Value="Revenue",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Revenue",*HorizontalAlignment=AlignCenter
        pack    range1,"AA",str9
        setprop sheet2.range(range1),*Value="Prod Cost",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Prod Cost",*HorizontalAlignment=AlignCenter
        pack    range1,"AB",str9
        setprop sheet2.range(range1),*Value="Lst Cost",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Lst Cost",*HorizontalAlignment=AlignCenter
        pack    range1,"AC",str9
        setprop sheet2.range(range1),*Value="Tot Cost",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Tot Cost",*HorizontalAlignment=AlignCenter
        pack    range1,"AD",str9
        setprop sheet2.range(range1),*Value="Net +/-",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Net +/-",*HorizontalAlignment=AlignCenter
        pack    range1,"AE",str9
        setprop sheet2.range(range1),*Value="Market Code",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Market Code",*HorizontalAlignment=AlignCenter
        pack    range1,"AF",str9
        setprop sheet2.range(range1),*Value="Comments",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Comments",*HorizontalAlignment=AlignCenter
        pack    range1,"AG",str9
        setprop sheet2.range(range1),*Value="Ex Base",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Ex Base",*HorizontalAlignment=AlignCenter
        pack    range1,"AH",str9
        setprop sheet2.range(range1),*Value="Rent Base",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Rent Base",*HorizontalAlignment=AlignCenter
        pack    range1,"AI",str9
        setprop sheet2.range(range1),*Value="Select Fees",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Select Fees",*HorizontalAlignment=AlignCenter
        pack    range1,"AJ",str9
        setprop sheet2.range(range1),*Value="Run Charge",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Run Charge",*HorizontalAlignment=AlignCenter
        pack    range1,"AK",str9
        setprop sheet2.range(range1),*Value="Ship/Tape",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Ship/Tape",*HorizontalAlignment=AlignCenter
        pack    range1,"AL",str9
        setprop sheet2.range(range1),*Value="Ex Total",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Ex Total",*HorizontalAlignment=AlignCenter
        pack    range1,"AM",str9
        setprop sheet2.range(range1),*Value="Rent Total",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1),*Value="Rent Total",*HorizontalAlignment=AlignCenter
.END PATCH 1.2 REPLACED LOGIC
.
        pack    str11,"1:",str9
        setprop sheet2.PageSetup,*PrintTitleRows=str11
        setprop sheet3.PageSetup,*PrintTitleRows=str11
        setprop sheet4.PageSetup,*PrintTitleRows=str11
.
.....Records.....
.Records for this report always start 2 rows after Headers
        add     C1,FirstRec,howmany
          move      howmany,howmany2
          add       C1,howmany,N9
.Set up Body Formatting
          getprop   sheet2.rows(howmany),*RowHeight=N32
          getprop   sheet3.rows(howmany),*RowHeight=N32
          getprop   sheet4.rows(howmany),*RowHeight=N32
          mult      C2,N32
          move      N9,str9
          call      Trim using str9
          pack      range1,"A",str9
          pack      range3,"IV1000"
          setprop   sheet2.range(range1,range3).rows,*RowHeight=N32
          setprop   sheet3.range(range1,range3).rows,*RowHeight=N32
          setprop   sheet4.range(range1,range3).rows,*RowHeight=N32
.
        move    NCMPFLD,NLOLFLD1
        move    C2,NLOLPATH
        move    "LoadLOLDetail-NLOLKEY",Location
        pack    KeyLocation,"Key: ",NLOLFLD1
.Clear Order variables used in List View
        clear   OLRN
        clear   OSTAT
        clear   OCLRSTAT
.START PATCH 1.5 REPLACED LOGIC
.        move    NCMPMLR,OMLRNUM
        move    NCMPMLRHold,OMLRNUM
.END PATCH 1.5 REPLACED LOGIC
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
                call    LoadDetail using C1
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
.START PATCH 1.22 ADDED LOGIC
                              if (OSTAT = "l")
                                        move      OLRN,NORD5FLD
                                        rep       zfill in NORD5FLD
                                        clear     NORD5STAT
                                        move      "LoadLOLDetail-NORD5KEY",Location
                                        pack      KeyLocation,"Key: ",NORD5FLD
                                        call      NORD5KEY            .get LCR info
                              endif
                              if (OSTAT = "z" | OSTAT = "X" | OSTAT = "Q" | (OSTAT = "l" & (NORD5STAT = "05" | NORD5STAT = "07")))
                              else
.END PATCH 1.22 ADDED LOGIC
                                  call    OrderLoadLOLPackLOL
                                  move    "LoadO.Detail-NSPE2KEY",Location
                                  pack    KeyLocation,"Key: ",OLRN
                                  pack    NSPE2FLD,OLRN
                                  call    NSPE2KEY
                                  call    LoadDetail using C0
.START PATCH 1.22 ADDED LOGIC
                              endif
.END PATCH 1.22 ADDED LOGIC
.Need to reestablish NORDPATH at each iteration as above routines call other routines which MAY reset NORDPATH to C1
                        move    C4,NORDPATH
                        call    NORDKS
                        until over
                repeat
        endif
.
.....Format document.....
.Set up Header Formatting
        move    FirstRec,str9
        call    Trim using str9
        pack    range1,"A",str9
.START PATCH 1.2 REPLACED LOGIC
.        pack    range2,"AL",str9
          pack    range2,"AM",str9
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range1,range2).Font,*Color=colornum
        sheet2.range(range1,range2).BorderAround using *LineStyle=1,*Weight=2
.START PATCH 1.2 REPLACED LOGIC
.        sheet2.range("AH2","AJ3").BorderAround using *LineStyle=1,*Weight=2
        sheet2.range("AI2","AK3").BorderAround using *LineStyle=1,*Weight=2
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet3.range(range1,range2).Font,*Color=colornum
        sheet3.range(range1,range2).BorderAround using *LineStyle=1,*Weight=2
.START PATCH 1.2 REPLACED LOGIC
.        sheet3.range("AH2","AJ3").BorderAround using *LineStyle=1,*Weight=2
        sheet3.range("AI2","AK3").BorderAround using *LineStyle=1,*Weight=2
.END PATCH 1.2 REPLACED LOGIC
        pack    range2,"I",str9
        setprop sheet4.range(range1,range2).Font,*Color=colornum
        sheet4.range(range1,range2).BorderAround using *LineStyle=1,*Weight=2
        add     C2,FirstRec
.Format Detail Information
          move    FirstRec,str9
        call    Trim using str9
        pack    range2,"A",str9 
        move    howmany,LastRec
        move    LastRec,str10
        call    Trim using str10
.START PATCH 1.2 REPLACED LOGIC
.         pack      range4,"AL",str10
          pack      range4,"AM",str10
.END PATCH 1.2 REPLACED LOGIC
        sheet2.range(range2,range4).BorderAround using *LineStyle=1,*Weight=2
        setprop     sheet2.range(range2,range4).Borders(BorderHor),*LineStyle=1
        setprop     sheet2.range(range2,range4).Borders(BorderVert),*LineStyle=1
.        sheet3.range(str5,str8).BorderAround using *LineStyle=1,*Weight=2
.        setprop    sheet3.range(str5,str8).Borders(BorderHor),*LineStyle=1
.        setprop    sheet3.range(str5,str8).Borders(BorderVert),*LineStyle=1
        move    howmany2,LastRec2
        move    LastRec2,str15
        call    Trim using str15
          pack      range4,"I",str15
        sheet4.range(range2,range4).BorderAround using *LineStyle=1,*Weight=2
        setprop     sheet4.range(range2,range4).Borders(BorderHor),*LineStyle=1
        setprop     sheet4.range(range2,range4).Borders(BorderVert),*LineStyle=1
        add     C2,howmany
          move      howmany,TotLine
        move    howmany,str9
        call    Trim using str9
.
        pack    range1,"F",str9
        setprop sheet2.range(range1),*Value="T O T A L S",*HorizontalAlignment=AlignCenter
        setprop sheet2.range(range1).Font,*Bold="True"
        setprop sheet3.range(range1),*Value="T O T A L S",*HorizontalAlignment=AlignCenter
        setprop sheet3.range(range1).Font,*Bold="True"
        add     C2,howmany2
        move    howmany2,str12
        call    Trim using str12
        pack    range1,"D",str12
        setprop sheet4.range(range1),*Value="T O T A L S",*HorizontalAlignment=AlignCenter
        setprop sheet4.range(range1).Font,*Bold="True"
..COST PER MEMBER
.START PATCH 1.2 REPLACED LOGIC
        pack    range1,"G",str9
..=IF(U52>0,IF(AC52<>0,AC52/W52,0),0)
.         pack      taskname,"=IF(U",str9,">0,IF(AC",str9,"<>0,AC",str9,"/W",str9,",0),0)"
.=IF(V52>0,IF(AD52<>0,AD52/X52,0),0)
          pack      taskname,"=IF(V",str9,">0,IF(AD",str9,"<>0,AD",str9,"/X",str9,",0),0)"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        setprop sheet2.range(range1),*Formula=taskname
        setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range1),*Formula=taskname
.UNIVERSE
        pack    range1,"I",str9
.=SUM(I17:I131)
          move    FirstRec,str11
        call    Trim using str11
          pack      str45,"=SUM(I",str11,":I",str10,")"
        setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0)"
        setprop sheet2.range(range1),*Formula=str45
        setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0)"
        setprop sheet3.range(range1),*Formula=str45
.RECO QTY
        pack    range1,"L",str9
.=SUM(L17:L131)
          rep     "IL",str45
        setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0)"
        setprop sheet2.range(range1),*Formula=str45
        setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0)"
        setprop sheet3.range(range1),*Formula=str45
        pack    range1,"E",str12
          pack      str45,"=SUM(E",str11,":E",str15,")"
        setprop sheet4.range(range1),*NumberFormat="##,####0_);(##,####0)"
        setprop sheet4.range(range1),*Formula=str45
.RECEIVED QTY
        pack    range1,"M",str9
.=SUM(M17:M131)
          pack      str45,"=SUM(M",str11,":M",str10,")"
        setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0)"
        setprop sheet2.range(range1),*Formula=str45
        setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0)"
        setprop sheet3.range(range1),*Formula=str45
        pack    range1,"F",str12
          pack      str45,"=SUM(F",str11,":F",str15,")"
        setprop sheet4.range(range1),*NumberFormat="##,####0_);(##,####0)"
        setprop sheet4.range(range1),*Formula=str45
.DIFFERENCE
        pack    range1,"N",str9
.=SUM(N17:N131)
          pack      str45,"=SUM(N",str11,":N",str10,")"
        setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0);_(* #"-#"_)"
        setprop sheet2.range(range1),*Formula=str45
        setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0);_(* #"-#"_)"
        setprop sheet3.range(range1),*Formula=str45
.LIST CPM
          pack      range1,"Q",str9
.=AVERAGE(Q17:Q130) 
          pack      str55,"=AVERAGE(Q",str11,":Q",str10,")"
        setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        setprop sheet2.range(range1),*Formula=str55
        setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range1),*Formula=str55
.AVERAGE NET
.START PATCH 1.2 REPLACED LOGIC
.         pack      range1,"T",str9
..=+U132/L132 
.         pack      str55,"=+U",str9,"/L",str9
          pack      range1,"U",str9
.=+V132/L132 
          pack      str55,"=+V",str9,"/L",str9
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range1),*NumberFormat="0.00%"
        setprop sheet2.range(range1),*Formula=str55
        setprop sheet3.range(range1),*NumberFormat="0.00%"
        setprop sheet3.range(range1),*Formula=str55
          pack      range1,"H",str12
          pack      str55,"=+I",str12,"/E",str12
        setprop sheet4.range(range1),*NumberFormat="0.00%"
        setprop sheet4.range(range1),*Formula=str55
.NET NAMES
.START PATCH 1.2 REPLACED LOGIC
.        pack    range1,"U",str9
..=SUM(U17:U131)
.         pack      str45,"=SUM(U",str11,":U",str10,")"
        pack    range1,"V",str9
.=SUM(V17:V131)
          pack      str45,"=SUM(V",str11,":V",str10,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0)"
        setprop sheet2.range(range1),*Formula=str45
        setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0)"
        setprop sheet3.range(range1),*Formula=str45
        pack    range1,"I",str12
          pack      str45,"=SUM(I",str11,":I",str15,")"
        setprop sheet4.range(range1),*NumberFormat="##,####0_);(##,####0)"
        setprop sheet4.range(range1),*Formula=str45
.RESPONSE RATE
.START PATCH 1.2 REPLACED LOGIC
.         pack      range1,"V",str9
..=+W132/U132 
.         pack      str55,"=+W",str9,"/U",str9
          pack      range1,"W",str9
.=+X132/V132 
          pack      str55,"=+X",str9,"/V",str9
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range1),*NumberFormat="0.00%"
        setprop sheet2.range(range1),*Formula=str55
        setprop sheet3.range(range1),*NumberFormat="0.00%"
        setprop sheet3.range(range1),*Formula=str55
.RETURNS
.START PATCH 1.2 REPLACED LOGIC
.        pack    range1,"W",str9
..=SUM(W17:W131)
.         pack      str45,"=SUM(W",str11,":W",str10,")"
        pack    range1,"X",str9
.=SUM(X17:X131)
          pack      str45,"=SUM(X",str11,":X",str10,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0)"
        setprop sheet2.range(range1),*Formula=str45
        setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0)"
        setprop sheet3.range(range1),*Formula=str45
.GIFT
.START PATCH 1.2 REPLACED LOGIC
.         pack      range1,"X",str9
..=+Y132/W132 
.         pack      str55,"=+Y",str9,"/W",str9
          pack      range1,"Y",str9
.=+Z132/X132 
          pack      str55,"=+Z",str9,"/X",str9
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        setprop sheet2.range(range1),*Formula=str55
        setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range1),*Formula=str55
.REVENUE
.START PATCH 1.2 REPLACED LOGIC
.        pack    range1,"Y",str9
..=SUM(Y17:Y131)
.         rep     "WY",str45
        pack    range1,"Z",str9
.=SUM(Z17:Z131)
          rep     "XZ",str45
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
        setprop sheet2.range(range1),*Formula=str45
        setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range1),*Formula=str45
.PRODUCTION COST
.START PATCH 1.2 REPLACED LOGIC
.        pack    range1,"Z",str9
..=SUM(Z17:Z131)
.         rep     "YZ",str45
        pack    range1,"AA",str9
.=SUM(AA17:AA131)
          pack      str45,"=SUM(AA",str11,":AA",str10,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
        setprop sheet2.range(range1),*Formula=str45
        setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range1),*Formula=str45
.LIST COST
.START PATCH 1.2 REPLACED LOGIC
.        pack    range1,"AA",str9
..=SUM(AA17:AA131)
.         pack      str45,"=SUM(AA",str11,":AA",str10,")"
        pack    range1,"AB",str9
.=SUM(AB17:AB131)
          pack      str45,"=SUM(AB",str11,":AB",str10,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
        setprop sheet2.range(range1),*Formula=str45
        setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range1),*Formula=str45
.TOTAL COST
.START PATCH 1.2 REPLACED LOGIC
.        pack    range1,"AB",str9
..=SUM(AB17:AB131)
.         pack      str45,"=SUM(AB",str11,":AB",str10,")"
        pack    range1,"AC",str9
.=SUM(AC17:AC131)
          pack      str45,"=SUM(AC",str11,":AC",str10,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
        setprop sheet2.range(range1),*Formula=str45
        setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range1),*Formula=str45
.NET +-
.START PATCH 1.2 REPLACED LOGIC
.        pack    range1,"AC",str9
..=Y132-AB132
.         pack      str45,"=Y",str9,"-AB",str9
        pack    range1,"AD",str9
.=Z132-AC132
          pack      str45,"=Z",str9,"-AC",str9
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
        setprop sheet2.range(range1),*Formula=str45
        setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range1),*Formula=str45
.Format Total Line
          pack    range1,"A",str9
.START PATCH 1.2 REPLACED LOGIC
.         pack    range2,"AL",str9
          pack    range2,"AM",str9
.END PATCH 1.2 REPLACED LOGIC
        setprop     sheet2.range(range1,range2).Borders(8),*LineStyle=1                             .Top Line
        setprop     sheet2.range(range1,range2).Borders(9),*LineStyle=DblLine             .Bottom Line
        setprop     sheet3.range(range1,range2).Borders(8),*LineStyle=1                             .Top Line
        setprop     sheet3.range(range1,range2).Borders(9),*LineStyle=DblLine             .Bottom Line
          pack    range1,"A",str12
          pack    range2,"I",str12
        setprop     sheet4.range(range1,range2).Borders(8),*LineStyle=1                             .Top Line
        setprop     sheet4.range(range1,range2).Borders(9),*LineStyle=DblLine             .Bottom Line
.
...........
.        move    howmany,area1
.        setprop sheet2.range(str4),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0)"
.        pack    str4,"I",str9
.        rep     "FI",str25
.        setprop sheet2.range(str4),*Formula=str25,*NumberFormat="##,####0_);[Red](##,####0)"
.SORT SHEET 2
.Sort by List Name
          move    LastRec,str9
          call      Trim using str9
.START PATCH 1.2 REPLACED LOGIC
.         pack      range1,"AL",str9
          pack      range1,"AM",str9
.END PATCH 1.2 REPLACED LOGIC
.        trap    errortrap if object
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
.List Name
        move    FirstRec,str9
        call    Trim using str9
        pack    range2,"E",str9
        getprop sheet2.range(range2),*Columns(1)=sortcol
.Package Name
        pack    range2,"J",str9
        getprop sheet2.range(range2),*Columns(1)=sortcol1
.Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
        pack    range2,"A",str9
        sheet2.range(range2,range1).sort using *Key1=sortcol,*Order1=1,*Key2=sortcol1,*Order2=1
.SORT SHEET 3
.Sort by Package Name
        move    howmany,str9
        call    Trim using str9
.START PATCH 1.2 REPLACED LOGIC
.         pack      range1,"AL",str9
          pack      range1,"AM",str9
.END PATCH 1.2 REPLACED LOGIC
.        trap    errortrap if object
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
.List Name
        move    FirstRec,str9
        call    Trim using str9
        pack    range2,"J",str9
        getprop sheet3.range(range2),*Columns(1)=sortcol
.Package Name
        pack    range2,"E",str9
        getprop sheet3.range(range2),*Columns(1)=sortcol1
.Key1 set to Package Name, Order1 set to 1(Ascending) or 2(Descending)
        pack    range2,"A",str9
        sheet3.range(range2,range1).sort using *Key1=sortcol,*Order1=1,*Key2=sortcol1,*Order2=1
.SORT SHEET 4
.Sort by Package Name
        move    howmany2,str12
        call    Trim using str12
          pack      range1,"I",str12
.        trap    errortrap if object
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
.List Name
        move    FirstRec,str9
        call    Trim using str9
        pack    range2,"C",str9
        getprop sheet4.range(range2),*Columns(1)=sortcol
.Key1 set to Package Name, Order1 set to 1(Ascending) or 2(Descending)
        pack    range2,"A",str9
        sheet4.range(range2,range1).sort using *Key1=sortcol,*Order1=1
...........
.Following is done so that the Total is included in Autofit.
..10 extra lines will actually cover much more than Total lines, but, what the hell.
        move    howmany,N10
        add     "1",N10
        move    N10,str10
        call    Trim using str10
        move    howmany2,N10
        add     "1",N10
        move    N10,str12
        call    Trim using str12
          move      hdrrow,str9
          call      Trim using str9
        pack    range1,"A",str9
        pack    range2,"A",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range2,"A",str12
        sheet4.range(range1,range2).Columns.Autofit
        pack    range1,"B",str9
        pack    range2,"B",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range2,"B",str12
        sheet4.range(range1,range2).Columns.Autofit
        pack    range1,"C",str9
        pack    range2,"C",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range2,"C",str12
        sheet4.range(range1,range2).Columns.Autofit
        pack    range1,"D",str9
        pack    range2,"D",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range2,"D",str12
        sheet4.range(range1,range2).Columns.Autofit
        pack    range1,"E",str9
        pack    range2,"E",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range2,"E",str12
        sheet4.range(range1,range2).Columns.Autofit
          pack    range1,"F",str9
        pack    range2,"F",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range2,"F",str12
        sheet4.range(range1,range2).Columns.Autofit
        pack    range1,"G",str9
        pack    range2,"G",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range2,"G",str12
        sheet4.range(range1,range2).Columns.Autofit
        pack    range1,"H",str9
        pack    range2,"H",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range2,"H",str12
        sheet4.range(range1,range2).Columns.Autofit
        pack    range1,"I",str9
        pack    range2,"I",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range2,"I",str12
        sheet4.range(range1,range2).Columns.Autofit
        pack    range1,"J",str9
        pack    range2,"J",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"K",str9
        pack    range2,"K",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"L",str9
        pack    range2,"L",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"M",str9
        pack    range2,"M",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"N",str9
        pack    range2,"N",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"O",str9
        pack    range2,"O",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"P",str9
        pack    range2,"P",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"Q",str9
        pack    range2,"Q",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"R",str9
        pack    range2,"R",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"S",str9
        pack    range2,"S",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"T",str9
        pack    range2,"T",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"U",str9
        pack    range2,"U",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"V",str9
        pack    range2,"V",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"W",str9
        pack    range2,"W",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"X",str9
        pack    range2,"X",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"Y",str9
        pack    range2,"Y",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"Z",str9
        pack    range2,"Z",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"AA",str9
        pack    range2,"AA",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"AB",str9
        pack    range2,"AB",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"AC",str9
        pack    range2,"AC",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"AD",str9
        pack    range2,"AD",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"AE",str9
        pack    range2,"AE",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"AF",str9
        pack    range2,"AF",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"AG",str9
        pack    range2,"AG",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"AH",str9
        pack    range2,"AH",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"AI",str9
        pack    range2,"AI",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"AJ",str9
        pack    range2,"AJ",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"AK",str9
        pack    range2,"AK",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
        pack    range1,"AL",str9
        pack    range2,"AL",str10
        sheet2.range(range1,range2).Columns.Autofit
        sheet3.range(range1,range2).Columns.Autofit
.START PATCH 1.2 ADDED LOGIC
        pack    range1,"AM",str9
        pack    range2,"AM",str10
        sheet2.range(range1,range2).Columns.Autofit
          sheet3.range(range1,range2).Columns.Autofit
.END PATCH 1.2 ADDED LOGIC
.Set Print Area
          pack      str25,"$A$1:$I$",str12
          setprop   sheet4.PageSetup,*PrintArea=str25
.Additional Fields
          add       C3,howmany
          move      howmany,str9
          call      Trim using str9
          pack      range1,"H",str9
        setprop sheet2.range(range1),*Value="P-Test = Package Test",*HorizontalAlignment=AlignRight
        setprop sheet3.range(range1),*Value="P-Test = Package Test",*HorizontalAlignment=AlignRight
          add       C1,howmany
          move      howmany,str9
          call      Trim using str9
          pack      range1,"H",str9
        setprop sheet2.range(range1),*Value="L-Test = List Test",*HorizontalAlignment=AlignRight
        setprop sheet3.range(range1),*Value="L-Test = List Test",*HorizontalAlignment=AlignRight
          add       C1,howmany
          move      howmany,str9
          call      Trim using str9
          pack      range1,"H",str9
        setprop sheet2.range(range1),*Value="S-Test = Select Test",*HorizontalAlignment=AlignRight
        setprop sheet3.range(range1),*Value="S-Test = Select Test",*HorizontalAlignment=AlignRight
          add       C1,howmany
          move      howmany,str9
          call      Trim using str9
          pack      range1,"H",str9
        setprop sheet2.range(range1),*Value="L-Retest = List Retest",*HorizontalAlignment=AlignRight
        setprop sheet3.range(range1),*Value="L-Retest = List Retest",*HorizontalAlignment=AlignRight
.Package Breakout
          add       C2,howmany
          move      howmany,str9
          call      Trim using str9
          pack      range1,"F",str9
        setprop sheet2.range(range1),*Value="PACKAGE BREAKOUT"
        setprop sheet3.range(range1),*Value="PACKAGE BREAKOUT"
.START PATCH 1.2 REPLACED LOGIC
.         pack      range2,"AL",str9
          pack      range2,"AM",str9
.END PATCH 1.2 REPLACED LOGIC
        setprop     sheet2.range(range1,range2).Borders(9),*LineStyle=1                             .Bottom Line
        setprop     sheet3.range(range1,range2).Borders(9),*LineStyle=1                             .Bottom Line
          add       C2,howmany
          move      howmany,FirstPack             .Marker for first Row of Packages
          clear     str2
          for       N9 from FirstRec to LastRec
                    move      N9,str9
                    call      Trim using str9
                    pack      range1,"J",str9
                    getprop sheet2.range(range1),*Value=NPKGPNAME
                    getprop sheet3.range(range1),*Value=NPKGPNAME
                    call      Trim using NPKGPNAME
                    if (NPKGPNAME <> "")
                              move      YES,str1
                              for       N8 from FirstPack to howmany
                                        move      N8,str8
                                        call      Trim using str8
                                        pack      range1,"F",str8
                                        getprop sheet2.range(range1),*Value=str45
                                        getprop sheet3.range(range1),*Value=str45
                                        call      Trim using str45
                                        if (NPKGPNAME = str45)
                                                  move      NO,str1
                                        endif
                              repeat
                              if (str1 = YES)
                                        if (str2 = YES)
                                                  add       C1,howmany
                                        else
                                                  move      YES,str2
                                        endif
                                        move      howmany,str9
                                        call      Trim using str9
                                        pack      range1,"F",str9
                              setprop sheet2.range(range1),*Value=NPKGPNAME
                              setprop sheet3.range(range1),*Value=NPKGPNAME
.START PATCH 1.2 REPLACED LOGIC
..=IF(ISERROR(SUM(AC142/W142)),"",SUM(AC142/W142))
.=IF(ISERROR(SUM(AD142/X142)),"",SUM(AD142/X142))
.END PATCH 1.2 REPLACED LOGIC
                                        pack      range1,"G",str9
.START PATCH 1.2 REPLACED LOGIC
.                                       pack      taskname,"=IF(ISERROR(SUM(AC",str9,"/W",str9,")),#"#",SUM(AC",str9,"/W",str9,"))"
                                        pack      taskname,"=IF(ISERROR(SUM(AD",str9,"/X",str9,")),#"#",SUM(AD",str9,"/X",str9,"))"
.END PATCH 1.2 REPLACED LOGIC
                                      setprop sheet2.range(range1),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                                      setprop sheet3.range(range1),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.=SUMIF($J$17:$J$130,"CR-rev-lab",$I$17:$I$130)
                                        move      FirstRec,str8
                                        call      Trim using str8
                                        move      LastRec,str10
                                        call      Trim using str10
                                        pack      range2,"I",str9
                                        pack      range1,"F",str9
                                        pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$I$",str8,":$I$",str10,")"
                                      setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
                                      setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
.=SUMIF($J$17:$J$130,"cr-rev-lab",$L$17:$L$130)
                                        pack      range2,"L",str9
                                        pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$L$",str8,":$L$",str10,")"
                              setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
                              setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
.START PATCH 1.2 REPLACED LOGIC
..=IF(ISERROR(+U142/L142),"",+U142/L142)
.                                       pack      range2,"T",str9
.                                       pack      taskname,"=IF(ISERROR(+U",str9,"/L",str9,"),#"#",+U",str9,"/L",str9,")"
.=IF(ISERROR(+V142/L142),"",+V142/L142)
                                        pack      range2,"U",str9
                                        pack      taskname,"=IF(ISERROR(+V",str9,"/L",str9,"),#"#",+V",str9,"/L",str9,")"
.END PATCH 1.2 REPLACED LOGIC
                              setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="0.00%"
                              setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="0.00%"
.START PATCH 1.2 REPLACED LOGIC
..=SUMIF($J$17:$J$130,"cr-rev-lab",$U$17:$U$130)
.                                       pack      range2,"U",str9
.                                       pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$U$",str8,":$U$",str10,")"
.=SUMIF($J$17:$J$130,"cr-rev-lab",$V$17:$V$130)
                                        pack      range2,"V",str9
                                        pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$V$",str8,":$V$",str10,")"
.END PATCH 1.2 REPLACED LOGIC
                              setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
                              setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
.START PATCH 1.2 REPLACED LOGIC
..=IF(ISERROR(+W142/U142),"",+W142/U142)
.                                       pack      range2,"V",str9
.                                       pack      taskname,"=IF(ISERROR(+W",str9,"/U",str9,"),#"#",+W",str9,"/U",str9,")"
.=IF(ISERROR(+X142/V142),"",+X142/V142)
                                        pack      range2,"W",str9
                                        pack      taskname,"=IF(ISERROR(+X",str9,"/V",str9,"),#"#",+X",str9,"/V",str9,")"
.END PATCH 1.2 REPLACED LOGIC
                              setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="0.00%"
                              setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="0.00%"
.START PATCH 1.2 REPLACED LOGIC
..=SUMIF($J$17:$J$130,"cr-rev-lab",$W$17:$W$130)
.                                       pack      range2,"W",str9
.                                       pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$W$",str8,":$W$",str10,")"
.=SUMIF($J$17:$J$130,"cr-rev-lab",$X$17:$X$130)
                                        pack      range2,"X",str9
                                        pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$X$",str8,":$X$",str10,")"
.END PATCH 1.2 REPLACED LOGIC
                              setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
                              setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
.START PATCH 1.2 REPLACED LOGIC
..=IF(ISERROR(+Y142/W142),"",+Y142/W142)
.                                       pack      range2,"X",str9
.                                       pack      taskname,"=IF(ISERROR(+Y",str9,"/W",str9,"),#"#",+Y",str9,"/W",str9,")"
.=IF(ISERROR(+Z142/X142),"",+Z142/X142)
                                        pack      range2,"Y",str9
                                        pack      taskname,"=IF(ISERROR(+Z",str9,"/X",str9,"),#"#",+Z",str9,"/X",str9,")"
.END PATCH 1.2 REPLACED LOGIC
                              setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                              setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.START PATCH 1.2 REPLACED LOGIC
..=SUMIF($J$17:$J$130,"cr-rev-lab",Y$17:Y$130)
.                                       pack      range2,"Y",str9
.                                       pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$Y$",str8,":$Y$",str10,")"
.=SUMIF($J$17:$J$130,"cr-rev-lab",Z$17:Z$130)
                                        pack      range2,"Z",str9
                                        pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$Z$",str8,":$Z$",str10,")"
.END PATCH 1.2 REPLACED LOGIC
                              setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                              setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.START PATCH 1.2 REPLACED LOGIC
..=SUMIF($J$17:$J$130,"cr-rev-lab",Z$17:Z$130)
.                                       pack      range2,"Z",str9
.                                       pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$Z$",str8,":$Z$",str10,")"
.=SUMIF($J$17:$J$130,"cr-rev-lab",AA$17:AA$130)
                                        pack      range2,"AA",str9
                                        pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$AA$",str8,":$AA$",str10,")"
.END PATCH 1.2 REPLACED LOGIC
                              setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                              setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.START PATCH 1.2 REPLACED LOGIC
..=SUMIF($J$17:$J$130,"cr-rev-lab",AA$17:AA$130)
.                                       pack      range2,"AA",str9
.                                       pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$AA$",str8,":$AA$",str10,")"
.=SUMIF($J$17:$J$130,"cr-rev-lab",AB$17:AB$130)
                                        pack      range2,"AB",str9
                                        pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$AB$",str8,":$AB$",str10,")"
.END PATCH 1.2 REPLACED LOGIC
                              setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                              setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.START PATCH 1.2 REPLACED LOGIC
..=SUMIF($J$17:$J$130,"cr-rev-lab",AB$17:AB$130)
.                                       pack      range2,"AB",str9
.                                       pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$AB$",str8,":$AB$",str10,")"
.=SUMIF($J$17:$J$130,"cr-rev-lab",AC$17:AC$130)
                                        pack      range2,"AC",str9
                                        pack      taskname,"=SUMIF($J$",str8,":$J$",str10,",",range1,",$AC$",str8,":$AC$",str10,")"
.END PATCH 1.2 REPLACED LOGIC
                              setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                              setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.START PATCH 1.2 REPLACED LOGIC
..=SUM(Y142-AB142)
.                                       pack      range2,"AC",str9
.                                       pack      str45,"=SUM(Y",str9,"-AB",str9,")"
.=SUM(Z142-AC142)
                                        pack      range2,"AD",str9
                                        pack      str45,"=SUM(Z",str9,"-AC",str9,")"
.END PATCH 1.2 REPLACED LOGIC
                              setprop sheet2.range(range2),*Formula=str45
                              setprop sheet3.range(range2),*Formula=str45
                              endif
                    endif
          repeat
.Format and Sort Package Breakout Section
....Sort
.Sort by Package Name
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
.Package Name
        move    FirstPack,str11
        call    Trim using str11
        pack    range3,"F",str11
        getprop sheet2.range(range3),*Columns(1)=sortcol
.Key1 set to Package Name, Order1 set to 1(Ascending) or 2(Descending)
          move      howmany,str10
          call      Trim using str10
.START PATCH 1.2 REPLACED LOGIC
.        pack    range4,"AC",str10
        pack    range4,"AD",str10
.END PATCH 1.2 REPLACED LOGIC
          sheet2.range(range3,range4).sort using *Key1=sortcol,*Order1=1
        getprop sheet3.range(range3),*Columns(1)=sortcol
          sheet3.range(range3,range4).sort using *Key1=sortcol,*Order1=1
....Format
          pack    range3,"F",str11
        sheet2.range(range3,range4).BorderAround using *LineStyle=1,*Weight=2
          if (str11 > str10)
          setprop   sheet2.range(range3,range4).Borders(BorderHor),*LineStyle=1
                  setprop     sheet2.range(range3,range4).Borders(BorderVert),*LineStyle=1
          endif
.        sheet3.range(str7,str8).BorderAround using *LineStyle=1,*Weight=2
.         if (str11 > str10)
.                 setprop     sheet3.range(str7,str8).Borders(BorderHor),*LineStyle=1
.         setprop   sheet3.range(str7,str8).Borders(BorderVert),*LineStyle=1
.         endif
.FirstPack = first Package Row,  LastPack = last Package Row
          move      howmany,LastPack
.
          add       C2,howmany
          move      howmany,TotPack
          move      howmany,str9
          call      Trim using str9
          pack      range1,"F",str9
        setprop sheet2.range(range1),*Value="PACKAGE TOTALS"
        setprop sheet3.range(range1),*Value="PACKAGE TOTALS"
.START PATCH 1.2 REPLACED LOGIC
.         pack      range3,"AC",str9
          pack      range3,"AD",str9
.END PATCH 1.2 REPLACED LOGIC
        setprop     sheet2.range(range1,range3).Borders(8),*LineStyle=1                             .Top Line
        setprop     sheet2.range(range1,range3).Borders(9),*LineStyle=1                             .Bottom Line
        setprop     sheet3.range(range1,range3).Borders(8),*LineStyle=1                             .Top Line
        setprop     sheet3.range(range1,range3).Borders(9),*LineStyle=1                             .Bottom Line
.START PATCH 1.2 REPLACED LOGIC
..=IF(ISERROR(SUM(AC149/W149)),"",SUM(AC149/W149))
.         pack      range2,"G",str9
.         pack      taskname,"=IF(ISERROR(SUM(AC",str9,"/W",str9,")),#"#",SUM(AC",str9,"/W",str9,"))"
.=IF(ISERROR(SUM(AD149/X149)),"",SUM(AD149/X149))
          pack      range2,"G",str9
          pack      taskname,"=IF(ISERROR(SUM(AD",str9,"/X",str9,")),#"#",SUM(AD",str9,"/X",str9,"))"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.=SUM(I142:I147)
          move      FirstPack,str11
          call      Trim using str11
          move      LastPack,str10
          call      Trim using str10
          pack      range2,"I",str9
          pack      taskname,"=SUM(I",str11,":I",str10,")"
        setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
        setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
.=SUM(L142:L147)
          pack      range2,"L",str9
          pack      taskname,"=SUM(L",str11,":L",str10,")"
        setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
        setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
.START PATCH 1.2 REPLACED LOGIC
..=IF(ISERROR(+U149/L149),"",+U149/L149)
.         pack      range2,"T",str9
.         pack      taskname,"=IF(ISERROR(+U",str9,"/L",str9,"),#"#",+U",str9,"/L",str9,")"
.=IF(ISERROR(+V149/L149),"",+V149/L149)
          pack      range2,"U",str9
          pack      taskname,"=IF(ISERROR(+V",str9,"/L",str9,"),#"#",+V",str9,"/L",str9,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="0.00%"
        setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="0.00%"
.START PATCH 1.2 REPLACED LOGIC
..=SUM(U142:U147)
.         pack      range2,"U",str9
.         pack      taskname,"=SUM(U",str11,":U",str10,")"
.=SUM(U142:U147)
          pack      range2,"V",str9
          pack      taskname,"=SUM(V",str11,":V",str10,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
        setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
.START PATCH 1.2 REPLACED LOGIC
..=IF(ISERROR(+W149/U149),"",+W149/U149)
.         pack      range2,"V",str9
.         pack      taskname,"=IF(ISERROR(+W",str9,"/U",str9,"),#"#",+W",str9,"/U",str9,")"
.=IF(ISERROR(+X149/V149),"",+X149/V149)
          pack      range2,"W",str9
          pack      taskname,"=IF(ISERROR(+X",str9,"/V",str9,"),#"#",+X",str9,"/V",str9,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="0.00%"
        setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="0.00%"
.START PATCH 1.2 REPLACED LOGIC
..=SUM(W142:W147)
.         pack      range2,"W",str9
.         pack      taskname,"=SUM(W",str11,":W",str10,")"
.=SUM(X142:X147)
          pack      range2,"X",str9
          pack      taskname,"=SUM(X",str11,":X",str10,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
        setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="##,####0_);(##,####0)"
.START PATCH 1.2 REPLACED LOGIC
..=IF(ISERROR(+Y149/W149),"",+Y149/W149)
.         pack      range2,"X",str9
.         pack      taskname,"=IF(ISERROR(+Y",str9,"/W",str9,"),#"#",+Y",str9,"/W",str9,")"
.=IF(ISERROR(+Z149/X149),"",+Z149/X149)
          pack      range2,"Y",str9
          pack      taskname,"=IF(ISERROR(+Z",str9,"/X",str9,"),#"#",+Z",str9,"/X",str9,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.START PATCH 1.2 REPLACED LOGIC
..=SUM(Y142:Y147)
.         pack      range2,"Y",str9
.         pack      taskname,"=SUM(Y",str11,":Y",str10,")"
.=SUM(Z142:Z147)
          pack      range2,"Z",str9
          pack      taskname,"=SUM(Z",str11,":Z",str10,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.START PATCH 1.2 REPLACED LOGIC
..=SUM(Z142:Z147)
.         pack      range2,"Z",str9
.         pack      taskname,"=SUM(Z",str11,":Z",str10,")"
.=SUM(AA142:AA147)
          pack      range2,"AA",str9
          pack      taskname,"=SUM(AA",str11,":AA",str10,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.START PATCH 1.2 REPLACED LOGIC
..=SUM(AA142:AA147)
.         pack      range2,"AA",str9
.         pack      taskname,"=SUM(AA",str11,":AA",str10,")"
.=SUM(AB142:AB147)
          pack      range2,"AB",str9
          pack      taskname,"=SUM(AB",str11,":AB",str10,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.START PATCH 1.2 REPLACED LOGIC
..=SUM(AB142:AB147)
.         pack      range2,"AB",str9
.         pack      taskname,"=SUM(AB",str11,":AB",str10,")"
.=SUM(AC142:AC147)
          pack      range2,"AC",str9
          pack      taskname,"=SUM(AC",str11,":AC",str10,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.START PATCH 1.2 REPLACED LOGIC
..=SUM(AC142-AC147)
.         pack      range2,"AC",str9
.         pack      taskname,"=SUM(Y",str9,"-AB",str9,")"
.=SUM(Z142-AC147)
          pack      range2,"AD",str9
          pack      taskname,"=SUM(Z",str9,"-AC",str9,")"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet2.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        setprop sheet3.range(range2),*Formula=taskname,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.
.Create info on Sheet1
          rep       lowup,NCMPCNAME
          pack      taskname,NCMPCNAME," QUICKVIEW"
          setprop   sheet1.range("B2"),*Value=taskname
..Formatting for above section
          setprop   sheet1.range("B2","H2").Interior,*ColorIndex=3              .'3' is index for Red
.
          setprop   sheet1.range("B4"),*Value="Traditional"
          setprop   sheet1.range("B5"),*Value="NonTraditional"
          setprop   sheet1.range("B7"),*Value="TOTAL"
          setprop   sheet1.range("B9"),*Value="Merge Dupes"
          setprop   sheet1.range("B10"),*Value="Lapsed"
          setprop   sheet1.range("B11"),*Value="Target"
          setprop   sheet1.range("B13"),*Value="TOTAL HOUSE/DUPES"
          setprop   sheet1.range("C4"),*Value="50000",*NumberFormat="##,####0_);(##,####0);#"-#""
          setprop   sheet1.range("C5"),*Value="",*NumberFormat="##,####0_);(##,####0);#"-#""
          setprop   sheet1.range("C7"),*Formula="=SUM(C4:C5)",*NumberFormat="##,####0_);(##,####0);#"-#""
          setprop   sheet1.range("C9"),*Value="",*NumberFormat="##,####0_);(##,####0);#"-#""
          setprop   sheet1.range("C10"),*Value="",*NumberFormat="##,####0_);(##,####0);#"-#""
          setprop   sheet1.range("C11"),*Value="20000",*NumberFormat="##,####0_);(##,####0);#"-#""
          setprop   sheet1.range("C13"),*Formula="=SUM(C9:C11)",*NumberFormat="##,####0_);(##,####0);#"-#""
          setprop   sheet1.range("C14"),*Formula="=SUM(C7-C13)",*NumberFormat="##,####0_);(##,####0);#"-#""
..Formatting for above section
          getitem   yellow,0,colornum
          setprop   sheet1.range("B7","C7").Interior,*Color=colornum                      .light yellow
          setprop   sheet1.range("B13","C13").Interior,*Color=colornum                              .light yellow
        sheet1.range("B4","C13").BorderAround using *Weight=MedThick
.
          setprop   sheet1.range("E6"),*Value="EX/R"
          setprop   sheet1.range("E7"),*Value="EXCHANGE"
          setprop   sheet1.range("E8"),*Value="RENT"
          move      FirstRec,str10
          call      Trim using str10
          move      LastRec,str11
          call      Trim using str11
          move      TotLine,str9
          call      Trim using str9
.START PATCH 1.2 REPLACED LOGIC
..=SUMIF(ALPHA!$P$13:$P$125,"E",ALPHA!$U$13:$U$125)/ALPHA!$U$127
.         pack      taskname,"=SUMIF(ALPHA!$P$",str10,":$P$",str11,",#"E#",ALPHA!$U$",str10,":$U$",str11,")/ALPHA!$U$",str9
.         setprop   sheet1.range("F7"),*Formula=taskname,*NumberFormat="0.00%"
..=SUMIF(ALPHA!$P$13:$P$125,"R",ALPHA!$U$13:$U$125)/ALPHA!$U127
.         pack      taskname,"=SUMIF(ALPHA!$P$",str10,":$P$",str11,",#"R#",ALPHA!$U$",str10,":$U$",str11,")/ALPHA!$U$",str9
.=SUMIF(ALPHA!$P$13:$P$125,"E",ALPHA!$V$13:$V$125)/ALPHA!$V$127
          pack      taskname,"=SUMIF(ALPHA!$P$",str10,":$P$",str11,",#"E#",ALPHA!$V$",str10,":$V$",str11,")/ALPHA!$V$",str9
          setprop   sheet1.range("F7"),*Formula=taskname,*NumberFormat="0.00%"
.=SUMIF(ALPHA!$P$13:$P$125,"R",ALPHA!$V$13:$V$125)/ALPHA!$V127
          pack      taskname,"=SUMIF(ALPHA!$P$",str10,":$P$",str11,",#"R#",ALPHA!$V$",str10,":$V$",str11,")/ALPHA!$V$",str9
.END PATCH 1.2 REPLACED LOGIC
          setprop   sheet1.range("F8"),*Formula=taskname,*NumberFormat="0.00%"
..Formatting for above section
        sheet1.range("E6","E8").BorderAround using *Weight=MedThick
        sheet1.range("E7","F8").BorderAround using *Weight=MedThick
        setprop     sheet1.range("E7","F7").Borders(9),*LineStyle=1                       .Bottom Line
          setprop   sheet1.Range("E7","F8").Font,*Size=12
.         
          setprop   sheet1.range("F12"),*Value="List CPM:"
          move      TotLine,str9
          call      Trim using str9
          pack      str45,"=ALPHA!Q",str9
          setprop   sheet1.range("F13"),*Formula=str45,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..Formatting for above section
        sheet1.range("F12","F13").BorderAround using *Weight=MedThick
          setprop   sheet1.Range("F12","F13").Font,*Size=12
.
          setprop   sheet1.range("B16"),*Value="PACKAGES"
          setprop   sheet1.range("C16"),*Value="GOAL"
          setprop   sheet1.range("D16"),*Value="ACTUAL"
          setprop   sheet1.range("E16"),*Value="DIFF"
          setprop   sheet1.range("F16"),*Value="CPM"
.Notes on Form Variables below:
. howmany = Currently row number
. FirstPack = Row number for first Package on Sheet2
. LastPack = Row number for last Package on Sheet2
. N9 = Increment variable for records between FirstPack and LastPack
. TotPack = Row number for Package Totals on Sheet2
. result = Row number for first Package on Sheet1
. N10 = Row number for last Package on Sheet1
          move      C0,N9
          move      FirstPack,N9
          move      "17",howmany
          add       C1,howmany,result
          loop
                    until (N9 > LastPack)
                    add       C1,howmany
                    move      howmany,str9
                    call      Trim using str9
                    pack      range1,"B",str9
                    move      N9,str10
                    call      Trim using str10
                    pack      str45,"=ALPHA!F",str10
                    setprop   sheet1.range(range1),*Formula=str45
                    pack      range1,"C",str9
.                   pack      str45,"=ALPHA!Q",str9
                    setprop   sheet1.range(range1),*Value="",*NumberFormat="##,####0_);(##,####0);#"-#""
                    pack      range1,"D",str9
.START PATCH 1.2 REPLACED LOGIC
.                   pack      str45,"=ALPHA!U",str10
                    pack      str45,"=ALPHA!V",str10
.END PATCH 1.2 REPLACED LOGIC
                    setprop   sheet1.range(range1),*Formula=str45,*NumberFormat="##,####0_);(##,####0);#"-#""
                    pack      range1,"E",str9
                    pack      str45,"=SUM(D",str9,"-C",str9,")"
                    setprop   sheet1.range(range1),*Formula=str45,*NumberFormat="##,####0_);(##,####0);#"-#""
                    pack      range1,"F",str9
                    pack      str45,"=ALPHA!G",str10
                    setprop   sheet1.range(range1),*Formula=str45,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                    add       C1,N9
          repeat
.First Row of Packages on this Sheet
          move      result,str11
          call      Trim using str11
.Last Row of Packages on this Sheet
          move      howmany,N10
          move      N10,str10
          call      Trim using str10
.
          add       C2,howmany
          move      howmany,str9
          call      Trim using str9
          pack      range1,"B",str9
          setprop   sheet1.range(range1),*Value="TOTAL OUTSIDE LIST"
          pack      range1,"C",str9
          pack      str45,"=SUM(C",str11,":C",str10,")"
          setprop   sheet1.range(range1),*Formula=str45,*NumberFormat="##,####0_);(##,####0);#"-#""
          pack      range1,"D",str9
          pack      str45,"=SUM(D",str11,":D",str10,")"
          setprop   sheet1.range(range1),*Formula=str45,*NumberFormat="##,####0_);(##,####0);#"-#""
          pack      range1,"E",str9
          pack      str45,"=SUM(D",str9,"-C",str9,")"
          setprop   sheet1.range(range1),*Formula=str45,*NumberFormat="##,####0_);(##,####0);#"-#""
          pack      range1,"F",str9
          move      TotPack,str9
          call      Trim using str9
          pack      str45,"=ALPHA!G",str9
          setprop   sheet1.range(range1),*Formula=str45,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..Formatting for above section
        setprop     sheet1.range("B16","F16").Borders(9),*LineStyle=1                     .Bottom Line
          move      howmany,str9
          call      Trim using str9
          pack      range1,"F",str9
        sheet1.range("B16",range1).BorderAround using *Weight=MedThick
          pack      range2,"B",str9
          setprop   sheet1.range(range2,range1).Interior,*Color=colornum                            .light yellow
          pack      range2,"B",str11
          pack      range1,"F",str10
          setprop   sheet1.range(range2,range1).Interior,*Color=colornum                            .light yellow
.
          add       C2,howmany
          move      howmany,str9
          call      Trim using str9
          pack      range2,"B",str9
          pack      range1,"H",str9
          setprop   sheet1.range(range1,range2).Interior,*ColorIndex=3                    .'3' is index for Red         
.
          add       C2,howmany
          move      howmany,str9
          call      Trim using str9
          pack      range1,"B",str9
          setprop   sheet1.range(range1),*Value="MARKET CODES"
          setprop   sheet1.Range(range1).Font,*Size=12
          pack      range1,"C",str9
          setprop   sheet1.range(range1),*Value="% of PLAN"
          pack      range1,"D",str9
          setprop   sheet1.range(range1),*Value="MKT CODE"
..Formatting for above section
          pack      range2,"B",str9
          sheet1.range(range2,range1).BorderAround using *Weight=MedThick
.
          move      howmany,str9
          call      Trim using str9
          pack      range1,"F",str9
          setprop   sheet1.range(range1),*Value="List Usage:"
          setprop   sheet1.Range(range1).Font,*Size=12
          add       C2,howmany,result
          move      result,str10
          call      Trim using str10
          pack      range1,"F",str10
          setprop   sheet1.range(range1),*Value="P-Test = ",*HorizontalAlignment=AlignRight
          move      FirstRec,str11
          call      Trim using str11
          move      LastRec,str12
          call      Trim using str12
          move      TotLine,str13
          call      Trim using str13
          pack      range1,"G",str10
.START PATCH 1.2 REPLACED LOGIC
..=SUMIF(ALPHA!$H$13:$H$125,"P-Test",ALPHA!$U$13:$U$125)/ALPHA!$U$127
.         pack      taskname,"=SUMIF(ALPHA!$H$",str11,":$H$",str12,",#"P-Test#",ALPHA!$U$",str11,":$U$",str12,")/ALPHA!$U$",str13
.=SUMIF(ALPHA!$H$13:$H$125,"P-Test",ALPHA!$V$13:$V$125)/ALPHA!$V$127
          pack      taskname,"=SUMIF(ALPHA!$H$",str11,":$H$",str12,",#"P-Test#",ALPHA!$V$",str11,":$V$",str12,")/ALPHA!$V$",str13
.END PATCH 1.2 REPLACED LOGIC
          setprop   sheet1.Range(range1),*Formula=taskname,*NumberFormat="0.00%",*HorizontalAlignment=AlignCenter
          setprop   sheet1.Range(range1).Font,*Size=12
          add       C1,result
          move      result,str10
          call      Trim using str10
          pack      range1,"F",str10
          setprop   sheet1.range(range1),*Value="L-Test = ",*HorizontalAlignment=AlignRight
          pack      range1,"G",str10
.START PATCH 1.2 REPLACED LOGIC
.         pack      taskname,"=SUMIF(ALPHA!$H$",str11,":$H$",str12,",#"L-Test#",ALPHA!$U$",str11,":$U$",str12,")/ALPHA!$U$",str13
          pack      taskname,"=SUMIF(ALPHA!$H$",str11,":$H$",str12,",#"L-Test#",ALPHA!$V$",str11,":$V$",str12,")/ALPHA!$V$",str13
.END PATCH 1.2 REPLACED LOGIC
          setprop   sheet1.Range(range1),*Formula=taskname,*NumberFormat="0.00%",*HorizontalAlignment=AlignCenter
          setprop   sheet1.Range(range1).Font,*Size=12
          add       C1,result
          move      result,str10
          call      Trim using str10
          pack      range1,"F",str10
          setprop   sheet1.range(range1),*Value="S-Test = ",*HorizontalAlignment=AlignRight
          pack      range1,"G",str10
.START PATCH 1.2 REPLACED LOGIC
.         pack      taskname,"=SUMIF(ALPHA!$H$",str11,":$H$",str12,",#"S-Test#",ALPHA!$U$",str11,":$U$",str12,")/ALPHA!$U$",str13
          pack      taskname,"=SUMIF(ALPHA!$H$",str11,":$H$",str12,",#"S-Test#",ALPHA!$V$",str11,":$V$",str12,")/ALPHA!$V$",str13
.END PATCH 1.2 REPLACED LOGIC
          setprop   sheet1.Range(range1),*Formula=taskname,*NumberFormat="0.00%",*HorizontalAlignment=AlignCenter
          setprop   sheet1.Range(range1).Font,*Size=12
          add       C1,result
          move      result,str10
          call      Trim using str10
          pack      range1,"F",str10
          setprop   sheet1.range(range1),*Value="L-Retest = ",*HorizontalAlignment=AlignRight
          pack      range1,"G",str10
.START PATCH 1.2 REPLACED LOGIC
.         pack      taskname,"=SUMIF(ALPHA!$H$",str11,":$H$",str12,",#"L-Retest#",ALPHA!$U$",str11,":$U$",str12,")/ALPHA!$U$",str13
          pack      taskname,"=SUMIF(ALPHA!$H$",str11,":$H$",str12,",#"L-Retest#",ALPHA!$V$",str11,":$V$",str12,")/ALPHA!$V$",str13
.END PATCH 1.2 REPLACED LOGIC
          setprop   sheet1.Range(range1),*Formula=taskname,*NumberFormat="0.00%",*HorizontalAlignment=AlignCenter
          setprop   sheet1.Range(range1).Font,*Size=12
..Formatting for above section
          add       C1,result
          move      result,str10
          call      Trim using str10
          pack      range1,"F",str9
          pack      range2,"G",str10
          sheet1.range(range1,range2).BorderAround using *Weight=MedThick
...........Formatting for whole Sheet
          move      howmany,str9
          call      Trim using str9
          pack      range1,"B1"
          pack      range2,"B",str9
        sheet1.range(range1,range2).Columns.Autofit
          pack      range1,"C1"
          pack      range2,"C",str9
        sheet1.range(range1,range2).Columns.Autofit
          pack      range1,"D1"
          pack      range2,"D",str9
        sheet1.range(range1,range2).Columns.Autofit
          pack      range1,"E1"
          pack      range2,"E",str9
        sheet1.range(range1,range2).Columns.Autofit
          pack      range1,"F1"
          pack      range2,"F",str9
        sheet1.range(range1,range2).Columns.Autofit
          pack      range1,"G1"
          pack      range2,"G",str9
        sheet1.range(range1,range2).Columns.Autofit
.Create info on Sheet5
          setprop   sheet5.range("B3").Font,*Size=16
          setprop   sheet5.range("B3"),*Value="Cost are estimates from reconciliations - less list cost"
          setprop   sheet5.range("B3").Font,*Bold="True"
          setprop   sheet5.range("B5"),*Value="Package"
          setprop   sheet5.range("C5"),*Value="Total CPM"
          
          move      "7",howmany
.START PATCH 1.1 REPLACED LOGIC
.         move      C1,NPKGPATH
.         clear     NPKGFLD2
.         clear     NPKGFLD3
.         clear     NPKGFLD4
.         pack      NPKGFLD1,"01X",NCMPMLR
.         move      "LoadSheet2-NPKGAIM",Location
.         pack      KeyLocation,"Key: ",NPKGFLD1
.         call      NPKGAIM
.         loop
.                   until over
.                   add       C1,howmany
.                   move      howmany,str9
.                   call      Trim using str9
.                   pack      str5,"A",str9
.                   call      Trim using NPKGID
.                   setprop   sheet5.range(str5),*Value=NPKGID,*HorizontalAlignment=AlignRight
.                   pack      str5,"B",str9
.                   call      Trim using NPKGPNAME
.                   setprop   sheet5.range(str5),*Value=NPKGPNAME
.                   move      C0,N8
.                   clear     str11
.                   move      C1,NPRCPATH
.                   pack      NPRCFLD1,"01X",NPKGMlr
.                   pack      NPRCFLD2,"02X",NPKGNum
.                   move      "LoadSheet2-NPRCAIM",Location
.                   pack      KeyLocation,"Key: ",NPRCFLD1,COMMA,NPRCFLD2
.                   call      NPRCAIM
.                   loop
.                             until over
.                             move      C0,result
.                             move      NPRCDATE,result
.                             if (result > N8)
..Select latest Date/Total
.                                       move      NPRCDate,N8
.                                       move      NPRCTotal,str11
.                             endif
.                             move      "LoadSheet2-NPRCKG",Location
.                             call      NPRCKG
.                   repeat
.                   pack      str5,"C",str9
.                   setprop   sheet5.range(str5),*Value=str11,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.                   pack      str5,"D",str9
.                   move      N8,str8
.                   unpack    str8,str4,MM,DD
.                   call      Trim using MM
.                   if (MM <> "")
.                             pack      str10,MM,SLASH,DD,SLASH,str4
.                   else
.                             clear     str10
.                   endif
.                   setprop   sheet5.range(str5),*Value=str10
.                   move      "LoadSheet2-NPKGKG",Location
.                   call      NPKGKG
.         repeat
.............................
          move      C1,NPKGPATH
          move      "LoadSheet2-NPKGKEY",Location
          PackData.GetCount giving howmany4
          if (howmany4 > C0)
                    for howmany5,"1",howmany4
                              getitem   PackData,howmany5,taskname
                              unpack    taskname,NPKGNUM,NPKGID,NPKGPNAME,NPRCDATE,NPRCTOTAL
                              add       C1,howmany
                              move      howmany,str9
                              call      Trim using str9
                              pack      range2,"A",str9
                              call      Trim using NPKGID
                              setprop   sheet5.range(range2),*Value=NPKGID,*HorizontalAlignment=AlignRight
                              pack      range2,"B",str9
                              call      Trim using NPKGPNAME
                              setprop   sheet5.range(range2),*Value=NPKGPNAME
                              move      NPRCTotal,str11
                              pack      range2,"C",str9
                              setprop   sheet5.range(range2),*Value=str11,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                              pack      range2,"D",str9
                              unpack    NPRCDATE,str4,MM,DD
                              call      Trim using MM
                              if (MM <> "")
                                        pack      str10,MM,SLASH,DD,SLASH,str4
                              else
                                        clear     str10
                              endif
                              setprop   sheet5.range(range2),*Value=str10
                    repeat
          endif
.END PATCH 1.1 REPLACED LOGIC
.Format Sheet5
        pack    range1,"A5"
        pack    range2,"A",str9
        sheet5.range(range1,range2).Columns.Autofit
        pack    range1,"B5"
        pack    range2,"B",str9
        sheet5.range(range1,range2).Columns.Autofit
        pack    range1,"C5"
        pack    range2,"C",str9
        sheet5.range(range1,range2).Columns.Autofit
        pack    range1,"D5"
        pack    range2,"D",str9
        sheet5.range(range1,range2).Columns.Autofit
.
          pack      range1,"A8"
          pack      range2,"C",str9
          setprop   sheet5.range(range1,range2).Interior,*ColorIndex=6                    .'6' is index for Yellow
.
CampaignFileNameSelect
        clear   taskname
        append  "\\nins1\d\USERS",taskname
        call    Trim using StrPtr1
        if (StrPtr1 <> "")
                append  "\",taskname
                append  StrPtr1,taskname
        endif
        append  "\",taskname
        reset   taskname
        setprop ex,*DefaultFilePath=taskname
        call    Trim using NCMPCNAME
        pack    taskname,taskname,NCMPCNAME
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
.START PATCH 1.3 ADDED LOGIC
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.END PATCH 1.3 ADDED LOGIC
.        book.printout
CampaignCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
        destroy sortcol
        destroy sortcol1
        destroy sheet1
        destroy sheet2
        destroy sheet3
        destroy sheet4
        destroy sheet5
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
.START PATCH 1.1 ADDED LOGIC
          destroy   PackData
.END PATCH 1.1 ADDED LOGIC
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

errortrap
.testing purposes
        getinfo exception,taskname
        return
        
LoadDetail Routine FrmPtr
.Pull all Projection Records
          move      NO,RecFlag
          move      "LoadDetail-STAT2AIM",Location
          if (FrmPtr = C0)    .LR Record
                    pack      STAT2FLD2,"01X",OLRN
          else                          .LOL Record
                    pack      STAT2FLD2,"01X",NLOLLOL
          endif
.START PATCH 1.32 ADDED LOGIC
          pack      STAT2FLD3,"02X",FrmPtr
.END PATCH 1.32 ADDED LOGIC
          pack      KeyLocation,"Key: ",STAT2FLD2
          call      STAT2AIM
          if over
                    move      YES,RecFlag
          else
                    move      FrmPtr,str1
                    if (str1 <> STATLOL)
                              move      YES,RecFlag
                    endif
          endif
          add       C1,howmany2
          move      howmany2,str12
          call      Trim using str12
          move      C0,MASTERMQTY
          loop
.Prep work
                  add     C1,howmany
                  move    howmany,str9
                  call    Trim using str9
..Set up Body Formatting
.                   getprop   sheet2.rows(howmany),*RowHeight=N32
.                   mult      C2,N32
.                   setprop   sheet2.rows(howmany),*RowHeight=N32
.ACTION
                  pack    range1,"A",str9
                  setprop sheet2.range(range1),*Value=""
                  setprop sheet3.range(range1),*Value=""
.LCR/LR
                    pack    range1,"B",str9
                  setprop sheet2.range(range1),*Value=OLRN
                  setprop sheet3.range(range1),*Value=OLRN
.TNC #
                  pack    range1,"C",str9
                  setprop sheet2.range(range1),*Value=""
                  setprop sheet3.range(range1),*Value=""
                  pack    range1,"A",str12
                  setprop sheet4.range(range1),*Value=""
.LIST #
                  pack    range1,"D",str9
                    call      Trim using NLOLLIST
                  setprop sheet2.range(range1),*Value=NLOLLIST
                  setprop sheet3.range(range1),*Value=NLOLLIST
                  pack    range1,"B",str12
                  setprop sheet4.range(range1),*Value=NLOLLIST
.LIST NAME
                  pack    range1,"E",str9
                  move    C1,NDATPATH
                  move    NLOLLIST,NDATFLD
                  move    "LoadDetail-NDATKEY",Location
                  pack    KeyLocation,"Key: ",NDATFLD
                  call    NDATKEY
                  if not over
                          call    Trim using OLSTNAME
                          setprop sheet2.range(range1),*Value=OLSTNAME
                          setprop sheet3.range(range1),*Value=OLSTNAME
                            pack    range1,"C",str12
                          setprop sheet4.range(range1),*Value=OLSTNAME
                  endif
.SELECT
                  pack    range1,"F",str9
                  call    Trim using NLOLSELECT
                  setprop sheet2.range(range1),*Value=NLOLSELECT
                  setprop sheet3.range(range1),*Value=NLOLSELECT
                  pack    range1,"D",str12
                  setprop sheet4.range(range1),*Value=NLOLSELECT
.COST PER MEMBER
.START PATCH 1.2 REPLACED LOGIC
..=IF(U52>0,IF(AC52<>0,AC52/W52,0),0)
.                 pack    range1,"G",str9
.                   pack      taskname,"=IF(U",str9,">0,IF(AC",str9,"<>0,AC",str9,"/W",str9,",0),0)"
.=IF(V52>0,IF(AD52<>0,AD52/X52,0),0)
                  pack    range1,"G",str9
                    pack      taskname,"=IF(V",str9,">0,IF(AD",str9,"<>0,AD",str9,"/X",str9,",0),0)"
.END PATCH 1.2 REPLACED LOGIC
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet2.range(range1),*Formula=taskname
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*Formula=taskname
.LAST USE
                  pack    range1,"H",str9
                  setprop sheet2.range(range1),*Value=""
                  setprop sheet3.range(range1),*Value=""
.UNIVERSE
                  pack    range1,"I",str9
                  setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0)"
                  setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0)"
                  type    NLOLUNIVERSE
                  if equal
                          if (NLOLUNIVERSE <> "000000000")
                                        call      Trim using NLOLUNIVERSE
                                  setprop sheet2.range(range1),*Value=NLOLUNIVERSE
                                  setprop sheet3.range(range1),*Value=NLOLUNIVERSE
                          endif
                  endif
.PACKAGE
.PACKAGE COST
                  pack    range1,"K",str9
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.RECO QTY
                  pack    range1,"L",str9
                  setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0)"
                  setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0)"
                  pack    range1,"E",str12
                  setprop sheet4.range(range1),*NumberFormat="##,####0_);(##,####0)"
.RECEIVED QTY
                  pack    range1,"M",str9
                  setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0)"
                  setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0)"
                  pack    range1,"F",str12
                  setprop sheet4.range(range1),*NumberFormat="##,####0_);(##,####0)"
.DIFFERENCE
.=IF(M13=0,,M13-L13)
                  pack    range1,"N",str9
                    pack      str45,"=IF(M",str9,"=0,,M",str9,"-L",str9,")"
                  setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0);_(* #"-#"_)"
                  setprop sheet2.range(range1),*Formula=str45
                  setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0);_(* #"-#"_)"
                  setprop sheet3.range(range1),*Formula=str45
.IN
                  pack    range1,"O",str9
                  setprop sheet2.range(range1),*Value="",*HorizontalAlignment=AlignCenter
                  setprop sheet3.range(range1),*Value="",*HorizontalAlignment=AlignCenter
.R/E
                  pack    range1,"P",str9
                if (NLOLRENT = "1")
                        move    "E",str1
                elseif (NLOLRENT = "2")
                        move    "R",str1
                    elseif (NLOLRENT = "3")
                        move    "S",str1
                    else
                              clear     str1
                endif
                  if (OSTAT = "" | OSTAT = "z" | (OSTAT = "l" & OHIST <> "E"))
.Clear this variable if LOL Or Cancelled/Denied - Later logic will determine proper display
.Clear this variable if LCR not Cleared.
                          clear    str1
                  else
                          if (OCLRSTAT = "1")
                                  move    "E",str1
                          elseif (OCLRSTAT = "2")
                                  move    "R",str1
                          elseif (OCLRSTAT = "3")
                                  move    "S",str1
                          else
.Do nothing.  Live Orders should have previous instance of str15 used.
.                                           clear   str1
                          endif
                  endif
                  setprop sheet2.range(range1),*Value=str1,*HorizontalAlignment=AlignCenter
                  setprop sheet3.range(range1),*Value=str1,*HorizontalAlignment=AlignCenter
.LIST COST PER/M
                  pack    range1,"Q",str9
.                   pack      str45,"=IF(P",str9,"=#"R#",AL",str9,",AK",str9,")"
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.                 setprop sheet2.range(range1),*Formula=str45
.XSTAT
                  pack    range1,"R",str9
.START PATCH 1.5 REPLACED LOGIC
.                 move    NCMPMLR,OMLRNUM
                  move    NCMPMLRHold,OMLRNUM
.END PATCH 1.5 REPLACED LOGIC
                  move    NLOLLIST,OLNUM
                  call    OrderTestXSTAT
                    clear     str25
                  call    Trim using taskname
                    if (taskname = "Exchange Status is Even")
                              move      "EVEN",str25
                    elseif (OLRN <> "" & (taskname = "" | taskname = "NO EXCHANGE HISTORY" | taskname = "No Exchange History" ))
.I can do the following as taskname=NULL and is not filled until after second pointer is used
.Check out subroutine and trace DimPtr/DimPtr1 to see what I mean.
.START PATCH 1.5 REPLACED LOGIC
.                         pack    NORDFLD1,"01R",NCMPMLR
                          pack    NORDFLD1,"01R",NCMPMLRHold
.END PATCH 1.5 REPLACED LOGIC
                          pack    NORDFLD2,"02R",NLOLLIST
                          clear   NORDFLD3
                          clear   NORDFLD4
                          call    CampOrderGetHistory using taskname,OLRN,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,C0
                              if (taskname = "NO EXCHANGE HISTORY - RENTAL CONTINUATION")
                                        move      "NPE/C",str25
                              else
                                        move      "NPE/T",str25
                              endif
                    elseif (EFLAG <> "0")
                          compare USAGE1,USAGE2
                          if less
                                  sub     USAGE2,USAGE1
                                  if (EFLAG = "1")
                                                  pack      str25,DASH,USAGE1
                                  else
                                                  pack      str25,USAGE1
                                  endif
                          elseif not equal       .Greater
                                  sub     USAGE1,USAGE2
                                  if (EFLAG = "1")
                                                  pack      str25,USAGE2
                                  else
                                                  pack      str25,DASH,USAGE2
                                  endif
                          endif
                  endif
                  setprop sheet2.range(range1),*NumberFormat="##,####0_);[Red](##,####0)"
                  setprop sheet2.range(range1),*Value=str25,*HorizontalAlignment=AlignRight
                  setprop sheet3.range(range1),*NumberFormat="##,####0_);[Red](##,####0)"
                  setprop sheet3.range(range1),*Value=str25,*HorizontalAlignment=AlignRight
                  pack    range1,"G",str12
                  setprop sheet4.range(range1),*NumberFormat="##,####0_);[Red](##,####0)"
                  setprop sheet4.range(range1),*Value=str25,*HorizontalAlignment=AlignRight
.START PATCH 1.2 REPLACED LOGIC
..NET REQUESTED
.                 pack    range1,"S",str9
.                 setprop sheet2.range(range1),*NumberFormat="0.00%"
.                 setprop sheet3.range(range1),*NumberFormat="0.00%"
..AVERAGE NET
.                 pack    range1,"T",str9
.                 setprop sheet2.range(range1),*NumberFormat="0.00%"
.                 setprop sheet3.range(range1),*NumberFormat="0.00%"
.                 pack    range1,"H",str12
.                 setprop sheet4.range(range1),*NumberFormat="0.00%"
..NET NAMES
.                 pack    range1,"U",str9
..=SUM(T14*M14)
..=IF(M98=0,L98*T98,M98*T98)
..                  pack      str45,"=SUM(T",str9,"*M",str9,")"
.                   pack      str45,"=IF(M",str9,"=0,L",str9,"*T",str9,",M",str9,"*T",str9,")"
.                 setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0)"
.                 setprop sheet2.range(range1),*Formula=str45
.                 setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0)"
.                 setprop sheet3.range(range1),*Formula=str45
.                 pack    range1,"I",str12
.                   pack      str45,"=IF(F",str12,"=0,E",str12,"*H",str12,",F",str12,"*H",str12,")"
.                 setprop sheet4.range(range1),*NumberFormat="##,####0_);(##,####0)"
.                 setprop sheet4.range(range1),*Formula=str45
..RESPONSE RATE
.                 pack    range1,"V",str9
.                 setprop sheet2.range(range1),*NumberFormat="0.00%"
.                 setprop sheet3.range(range1),*NumberFormat="0.00%"
..RETURNS
.                 pack    range1,"W",str9
..=ROUND(U13*V13,0)
.                   pack      str45,"=ROUND(U",str9,"*V",str9,",0)"
.                 setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0)"
.                 setprop sheet2.range(range1),*Formula=str45
.                 setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0)"
.                 setprop sheet3.range(range1),*Formula=str45
..GIFT
.                 pack    range1,"X",str9
.                 setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.                 setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..REVENUE
.                 pack    range1,"Y",str9
..=W13*X13
.                   pack      str45,"=W",str9,"*X",str9
.                 setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                 setprop sheet2.range(range1),*Formula=str45
.                 setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                 setprop sheet3.range(range1),*Formula=str45
..PROD COST
.                 pack    range1,"Z",str9
..=(U13*K13/1000)
..=IF(K13<>0,(U13*K13/1000),"")
.                   pack      str45,"=IF(K",str9,"<>0,(U",str9,"*K",str9,"/1000),#"#")"
.                 setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                 setprop sheet2.range(range1),*Formula=str45
.                 setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                 setprop sheet3.range(range1),*Formula=str45
..LIST COST
.                 pack    range1,"AA",str9
..=U16/1000*Q16
..=IF(U16<>0,U16/1000*Q16,"")
.                   pack      str45,"=IF(U",str9,"<>0,U",str9,"/1000*Q",str9,",#"#")"
.                 setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                 setprop sheet2.range(range1),*Formula=str45
.                 setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                 setprop sheet3.range(range1),*Formula=str45
..TOTAL COST
.                 pack    range1,"AB",str9
..=SUM(Z14:AA14)
.                   pack      str45,"=SUM(Z",str9,":AA",str9,")"
.                 setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                 setprop sheet2.range(range1),*Formula=str45
.                 setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                 setprop sheet3.range(range1),*Formula=str45
..NET +/-
.                 pack    range1,"AC",str9
..=Y13-AB13
.                   pack      str45,"=Y",str9,"-AB",str9
.                 setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                 setprop sheet2.range(range1),*Formula=str45
.                 setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                 setprop sheet3.range(range1),*Formula=str45
..MARKET CODE
.                 pack    range1,"AD",str9
.                 setprop sheet2.range(range1),*Value=""
.                 setprop sheet3.range(range1),*Value=""
..EXCHANGE BASE
.                 pack    range1,"AF",str9
.                 setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.                 setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..RENT BASE
.                 pack    range1,"AG",str9
.                 setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.                 setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..SELECT FEE
.                 pack    range1,"AH",str9
.                 setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.                 setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..RUN CHARGE
.                 pack    range1,"AI",str9
.                 setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.                 setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..SHIP/TAX
.                 pack    range1,"AJ",str9
.                 setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.                 setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..EXCHANGE TOTAL
.                 pack    range1,"AK",str9
..=AF15/T15
..=IF(AF15<>0,AF15/T15,"")
..=IF(ISERROR(AF15/T15),"",AF15/T15)
.                   pack      str45,"=IF(ISERROR(AF",str9,"/T",str9,"),#"#",AF",str9,"/T",str9,")"
.                 setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.                 setprop sheet2.range(range1),*Formula=str45
.                 setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.                 setprop sheet3.range(range1),*Formula=str45
..RENT TOTAL
.                 pack    range1,"AL",str9
..=IF(L16>0,(((AG16*0.8)*S16)+((1-S16)*AI16)+AH16+5)/T16+(AJ16/(U16/1000)),(((AG16*0.8)*S16)+((1-S16)*AI16)+AH16+5)/T16+(AJ16/1000))
..=IF(L16>0,IF(U16<>0,(((AG16*0.8)*S16)+((1-S16)*AI16)+AH16+5)/T16+(AJ16/(U16/1000)),""),IF(AJ16<>0,(((AG16*0.8)*S16)+((1-S16)*AI16)+AH16+5)/T16+(AJ16/1000)),"")
..=IF(L16>0,IF(U16<>0,((AG16*0.8*S16)+((1-S16)*AI16)+AH16+5)/T16+(AJ16/U16/1000),""),IF(AJ16<>0,((AG16*0.8*S16)+((1-S16)*AI16)+AH16+5)/T16+(AJ16/1000)),"")
..=IF(ISERROR(IF(L16>0,(((AG16*0.8)*S16)+((1-S16)*AI16)+AH16+5)/T16+(AJ16/(U16/1000)),(((AG16*0.8)*S16)+((1-S16)*AI16)+AH16+5)/T16+(AJ16/1000))),"",IF(L16>0,(((AG16*0.8)*S16)+((1-S16)*AI16)+AH16+5)/T16+(AJ16/(U16/1000)),(((AG16*0.8)*S16)+((1-S16)*AI16)+AH16+5)/T16+(AJ16/1000)))
..                  pack      taskname,"=IF(L",str9,">0,IF(U",str9,"<>0,(((AG",str9,"*0.8)*S",str9,")+((1-S",str9,")*AI",str9,")+AH",str9,"+5)/T",str9,"+(AJ",str9,"/(U",str9,"/1000)),#"#"),IF(AJ",str9,"<>0,(((AG",str9,"*0.8)*S",str9,")+((1-S",str9,")*AI",str9,")+AH",str9,"+5)/T",str9,"+(AJ",str9,"/1000)),#"#")"
..                  pack      taskname,"=IF(L",str9,">0,IF(U",str9,"<>0,((AG",str9,"*0.8*S",str9,")+((1-S",str9,")*AI",str9,")+AH",str9,"+5)/T",str9,"+(AJ",str9,"/U",str9,"/1000),#"#"),IF(AJ",str9,"<>0,((AG",str9,"*0.8*S",str9,")+((1-S",str9,")*AI",str9,")+AH",str9,"+5)/T",str9,"+(AJ",str9,"/1000)),#"#")"
.                   pack      taskname2,"=IF(ISERROR(IF(L",str9,">0,(((AG",str9,"*0.8)*S",str9,")+((1-S",str9,")*AI",str9,")+AH",str9,"+5)/T",str9,"+(AJ",str9,"/(U",str9,"/1000)),(((AG",str9,"*0.8)*S",str9,")+((1-S",str9,")*AI",str9,")+AH",str9,"+5)/T",str9,"+(AJ",str9,"/1000))),#"#",IF(L",str9,">0,(((AG",str9,"*0.8)*S",str9,")+((1-S",str9,")*AI",str9,")+AH",str9,"+5)/T",str9,"+(AJ",str9,"/(U",str9,"/1000)),(((AG",str9,"*0.8)*S",str9,")+((1-S",str9,")*AI",str9,")+AH",str9,"+5)/T",str9,"+(AJ",str9,"/1000)))"
.                 setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.                 setprop sheet2.range(range1),*Formula=taskname2
.                 setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.                 setprop sheet3.range(range1),*Formula=taskname2
..
..SHEET 4 GETS 'MASTER' QTY AND NET
.                 pack    range1,"E",str12
.                   if (NLOLQTY > "0")
.                             setprop sheet4.range(range1),*Value=NLOLQTY
.                endif
..AVERAGE NET
.                 pack    range1,"H",str12
.                   move      C0,N35
.                   move      NLOLNET,N35
.                   mult      ".01",N35
.                 setprop sheet4.range(range1),*Value=N35
.                   if (RecFlag = YES)
..RECO QTY
.                           pack    range1,"L",str9
.                   if (NLOLQTY > "0")
.                             setprop sheet2.range(range1),*Value=NLOLQTY
.                             setprop sheet3.range(range1),*Value=NLOLQTY
.                         endif
..AVERAGE NET
.                           pack    range1,"T",str9
.                             move      C0,N35
.                             move      NLOLNET,N35
.                             mult      ".01",N35
.                           setprop sheet2.range(range1),*Value=N35
.                           setprop sheet3.range(range1),*Value=N35
..
.                             break
.                   endif
..                  until (RecFlag = YES)
.                   move      FrmPtr,str1
.                   if (str1 = STATLOL)
..PACKAGE
..START PATCH 1.1 REPLACED LOGIC
..                          pack    str4,"J",str9
..                            move      C1,NPKGPATH
..                            pack      NPKGFLD,STATMLR,STATPCKNUM
..                            move      "LoadDetail-NPKGKEY",Location
..                            pack      KeyLocation,"Key: ",NPKGFLD
..                            call      NPKGKEY
..                            if over
..                                      clear     NPKGPNAME
..                            endif
..                            call      Trim using NPKGPNAME
..                          setprop sheet2.range(str4,str4),*Value=NPKGPNAME
..                          setprop sheet3.range(str4,str4),*Value=NPKGPNAME
...PACKAGE COST
..                          pack    str4,"K",str9
..                            unpack    STATPCKM,str5
..                            move      C0,JULDAYS
..                            move      str5,JULDAYS
..                            if (JULDAYS > C0)
..                                      call      CVTGREG
..                                      if (YY < "80")
..                                                move      "20",str2
..                                      else
..                                                move      "19",str2
..                                      endif
..                                      pack      str10,str2,YY,MM,DD
..                                      move      C1,NPRCPATH
..                                      pack      NPRCFLD,STATMLR,STATPCKNUM,str10
..                                      move      "LoadDetail-NPRCKEY",Location
..                                      pack      KeyLocation,"Key: ",NPRCFLD
..                                      call      NPRCKEY
..                                      if over
..                                                clear     NPRCTOTAL
..                                      endif
..                            else
..                                      clear     NPRCTOTAL
..                            endif
..                            move      NPRCTOTAL,str8
..                            call      Trim using str8
..                          setprop sheet2.range(str4),*Value=str8
..                          setprop sheet3.range(str4),*Value=str8
...............................................
.                             PackData.GetCount giving howmany4
.                             clear     str1
.                             for howmany5,"1",howmany4
.                                       getitem   PackData,howmany5,taskname
.                                       unpack    taskname,str6,NPKGID,NPKGPNAME,NPRCDATE,NPRCTOTAL
.                                       if (str6 = STATPCKNUM)
.                                                 move      YES,str1
.                                                 break
.                                       endif
.                             repeat
.                             if (str1 <> YES)
.                                       move      C1,NPKGPATH
.                                       pack      NPKGFLD,STATMLR,STATPCKNUM
.                                       move      "LoadDetail-NPKGKEY",Location
.                                       pack      KeyLocation,"Key: ",NPKGFLD
.                                       call      NPKGKEY
.                                       if over
.                                                 clear     NPKGPNAME
.                                       endif
.                                       if (NPKGMaster = "1")
.                                                 clear     NPKGPNAME
.                                                 clear     NPRCTOTAL
.                                       else
.                                                 call      Trim using NPKGPNAME
.                                                 unpack    STATPCKM,str5
.                                                 move      C0,JULDAYS
.                                                 move      str5,JULDAYS
.                                                 if (JULDAYS > C0)
.                                                           call      CVTGREG
.                                                           if (YY < "80")
.                                                                     move      "20",str2
.                                                           else
.                                                                     move      "19",str2
.                                                           endif
.                                                           pack      str10,str2,YY,MM,DD
.                                                           move      C1,NPRCPATH
.                                                           pack      NPRCFLD,STATMLR,STATPCKNUM,str10
.                                                           move      "LoadDetail-NPRCKEY",Location
.                                                           pack      KeyLocation,"Key: ",NPRCFLD
.                                                           call      NPRCKEY
.                                                           if over
.                                                                     clear     NPRCTOTAL
.                                                           endif
.                                                 else
.                                                           clear     NPRCTOTAL
.                                                 endif
.                                                 pack      taskname,NPKGNUM,NPKGID,NPKGPNAME,NPRCDATE,NPRCTOTAL
.                                                 insertitem PackData,999999,taskname
.                                       endif
.                             endif
.                           pack    range1,"J",str9
.                           setprop sheet2.range(range1),*Value=NPKGPNAME
.                           setprop sheet3.range(range1),*Value=NPKGPNAME
..PACKAGE COST
.                           pack    range1,"K",str9
.                             move      NPRCTOTAL,str8
.                             call      Trim using str8
.                           setprop sheet2.range(range1),*Value=str8
.                           setprop sheet3.range(range1),*Value=str8
..END PATCH 1.1 ADDED LOGIC
..RECO QTY
.                           pack    range1,"L",str9
.                   if (STATRECQTY <> C0)
.                             setprop sheet2.range(range1),*Value=STATRECQTY
.                             setprop sheet3.range(range1),*Value=STATRECQTY
.                         endif
..RECEIVED QTY
.                           pack    range1,"M",str9
.                 if (STATMQTY <> C0)
.                           setprop sheet2.range(range1),*Value=STATMQTY
.                           setprop sheet3.range(range1),*Value=STATMQTY
.                         endif
..Accumulate for Sheet4
.                           pack    range1,"F",str12
.                             add       STATMQTY,MASTERMQTY
.                 if (MASTERMQTY <> C0)
.                             setprop sheet4.range(range1),*Value=MASTERMQTY
.                         endif                   
..LIST COST PER/M
.                           pack    range1,"Q",str9
.                           setprop sheet2.range(range1),*Value=STATLCPM
.                           setprop sheet3.range(range1),*Value=STATLCPM
..NET REQUESTED
.                           pack    range1,"S",str9
.                             mult      ".01",STATNETREQ,N35
.                           setprop sheet2.range(range1),*Value=N35
.                           setprop sheet3.range(range1),*Value=N35
..AVERAGE NET
.                           pack    range1,"T",str9
.                             mult      ".01",STATAVGNET,N35
.                           setprop sheet2.range(range1),*Value=N35
.                           setprop sheet3.range(range1),*Value=N35
..RESPONSE RATE
.                           pack    range1,"V",str9
.                             mult      ".01",STATRESP2,N35
.                           setprop sheet2.range(range1),*Value=N35
.                           setprop sheet3.range(range1),*Value=N35
..GIFT
.                           pack    range1,"X",str9
.                   setprop sheet2.range(range1),*Value=STATGIFT
.                   setprop sheet3.range(range1),*Value=STATGIFT
..COMMENTS
.                             move      C1,STATNPATH
.                             pack      STATNFLD,STATLR,STATLOL,STATPCKNUM
.                             move      "LoadRec-STATNKEY",Location
.                             pack      KeyLocation,"Key: ",STATNFLD
.                             call      STATNKEY
.                             call      Trim using STATNNOTE
.                           pack    range1,"AE",str9
.                             pack      str2,NewLine,B1
.                             rep       str2,STATNNOTE
.                           setprop sheet2.range(range1),*Value=STATNNOTE
.                           setprop sheet3.range(range1),*Value=STATNNOTE
..EXCHANGE BASE
.                           pack    range1,"AF",str9
.                           setprop sheet2.range(range1),*Value=STATEXBASE
.                           setprop sheet3.range(range1),*Value=STATEXBASE
..RENT BASE
.                           pack    range1,"AG",str9
.                           setprop sheet2.range(range1),*Value=STATRBASE
.                           setprop sheet3.range(range1),*Value=STATRBASE
..SELECT FEE
.                           pack    range1,"AH",str9
.                   setprop sheet2.range(range1),*Value=STATSELFEE
.                   setprop sheet3.range(range1),*Value=STATSELFEE
..RUN CHARGE
.                           pack    range1,"AI",str9
.                           setprop sheet2.range(range1),*Value=STATRUN
.                           setprop sheet3.range(range1),*Value=STATRUN
..SHIP/TAX
.                           pack    range1,"AJ",str9
.                   setprop sheet2.range(range1),*Value=STATSHIP
.                   setprop sheet3.range(range1),*Value=STATSHIP
......................................
.NET RECEIVED
                  pack    range1,"S",str9
                  setprop sheet2.range(range1),*NumberFormat="0.00%"
                  setprop sheet3.range(range1),*NumberFormat="0.00%"
.NET REQUESTED
                  pack    range1,"T",str9
                  setprop sheet2.range(range1),*NumberFormat="0.00%"
                  setprop sheet3.range(range1),*NumberFormat="0.00%"
.AVERAGE NET
                  pack    range1,"U",str9
                  setprop sheet2.range(range1),*NumberFormat="0.00%"
                  setprop sheet3.range(range1),*NumberFormat="0.00%"
                  pack    range1,"H",str12
                  setprop sheet4.range(range1),*NumberFormat="0.00%"
.NET NAMES
                  pack    range1,"V",str9
.=SUM(U14*M14)
.=IF(M98=0,L98*U98,M98*U98)
.                   pack      str45,"=SUM(U",str9,"*M",str9,")"
                    pack      str45,"=IF(M",str9,"=0,L",str9,"*U",str9,",M",str9,"*U",str9,")"
                  setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0)"
                  setprop sheet2.range(range1),*Formula=str45
                  setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0)"
                  setprop sheet3.range(range1),*Formula=str45
                  pack    range1,"I",str12
                    pack      str45,"=IF(F",str12,"=0,E",str12,"*H",str12,",F",str12,"*H",str12,")"
                  setprop sheet4.range(range1),*NumberFormat="##,####0_);(##,####0)"
                  setprop sheet4.range(range1),*Formula=str45
.RESPONSE RATE
                  pack    range1,"W",str9
                  setprop sheet2.range(range1),*NumberFormat="0.00%"
                  setprop sheet3.range(range1),*NumberFormat="0.00%"
.RETURNS
                  pack    range1,"X",str9
.=ROUND(V13*W13,0)
                    pack      str45,"=ROUND(V",str9,"*W",str9,",0)"
                  setprop sheet2.range(range1),*NumberFormat="##,####0_);(##,####0)"
                  setprop sheet2.range(range1),*Formula=str45
                  setprop sheet3.range(range1),*NumberFormat="##,####0_);(##,####0)"
                  setprop sheet3.range(range1),*Formula=str45
.GIFT
                  pack    range1,"Y",str9
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.REVENUE
                  pack    range1,"Z",str9
.=X13*Y13
                    pack      str45,"=X",str9,"*Y",str9
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                  setprop sheet2.range(range1),*Formula=str45
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*Formula=str45
.PROD COST
                  pack    range1,"AA",str9
.=(V13*K13/1000)
.=IF(K13<>0,(V13*K13/1000),"")
                    pack      str45,"=IF(K",str9,"<>0,(V",str9,"*K",str9,"/1000),#"#")"
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                  setprop sheet2.range(range1),*Formula=str45
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*Formula=str45
.LIST COST
                  pack    range1,"AB",str9
.=V16/1000*Q16
.=IF(V16<>0,V16/1000*Q16,"")
                    pack      str45,"=IF(V",str9,"<>0,V",str9,"/1000*Q",str9,",#"#")"
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                  setprop sheet2.range(range1),*Formula=str45
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*Formula=str45
.TOTAL COST
                  pack    range1,"AC",str9
.=SUM(AA14:AB14)
                    pack      str45,"=SUM(AA",str9,":AB",str9,")"
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                  setprop sheet2.range(range1),*Formula=str45
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*Formula=str45
.NET +/-
                  pack    range1,"AD",str9
.=Z13-AC13
                    pack      str45,"=Z",str9,"-AC",str9
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                  setprop sheet2.range(range1),*Formula=str45
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*Formula=str45
.MARKET CODE
                  pack    range1,"AE",str9
                  setprop sheet2.range(range1),*Value=""
                  setprop sheet3.range(range1),*Value=""
.EXCHANGE BASE
                  pack    range1,"AG",str9
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.RENT BASE
                  pack    range1,"AH",str9
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.SELECT FEE
                  pack    range1,"AI",str9
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.RUN CHARGE
                  pack    range1,"AJ",str9
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.SHIP/TAX
                  pack    range1,"AK",str9
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.EXCHANGE TOTAL
                  pack    range1,"AL",str9
.=AG15/U15
.=IF(AG15<>0,AG15/U15,"")
.=IF(ISERROR(AG15/U15),"",AG15/U15)
                    pack      str45,"=IF(ISERROR(AG",str9,"/U",str9,"),#"#",AG",str9,"/U",str9,")"
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet2.range(range1),*Formula=str45
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*Formula=str45
.RENT TOTAL
                  pack    range1,"AM",str9
.=IF(ISERROR(IF(L1>0,IF(S1>0,(((AH1*0.8)*S1)+((1-S1)*AJ1)+AI1+5),(((AH1*0.8)*T1)+((1-T1)*AJ1)+AI1+5))/U1+(AK1/(V1/1000)),IF(S1>0,(((AH1*0.8)*S1)+((1-S1)*AJ1)+AI1+5),(((AH1*0.8)*T1)+((1-T1)*AJ1)+AI1+5))/U1+(AK1/1000))),"",IF(L1>0,IF(S1>0,(((AH1*0.8)*S1)+((1-S1)*AJ1)+AI1+5),(((AH1*0.8)*T1)+((1-T1)*AJ1)+AI1+5))/U1+(AK1/(V1/1000)),IF(S1>0,(((AH1*0.8)*S1)+((1-S1)*AJ1)+AI1+5),(((AH1*0.8)*T1)+((1-T1)*AJ1)+AI1+5))/U1+(AK1/1000)))
                    pack      taskname2,"=IF(ISERROR(IF(L",str9,">0,IF(S",str9,">0,(((AH",str9,"*0.8)*S",str9,")+((1-S",str9,")*AJ",str9,")+AI",str9,"+5),(((AH",str9,"*0.8)*T",str9,")+((1-T",str9,")*AJ",str9,")+AI",str9,"+5))/U",str9,"+(AK",str9,"/(V",str9,"/1000)),IF(S",str9,">0,(((AH",str9,"*0.8)*S",str9,")+((1-S",str9,")*AJ",str9,")+AI",str9,"+5),(((AH",str9,"*0.8)*T",str9,")+((1-T",str9,")*AJ",str9,")+AI",str9,"+5))/U",str9,"+(AK",str9,"/1000))),#"#",IF(L",str9,">0,IF(S",str9,">0,(((AH",str9,"*0.8)*S",str9,")+((1-S",str9,")*AJ",str9,")+AI",str9,"+5),(((AH",str9,"*0.8)*T",str9,")+((1-T",str9,")*AJ",str9,")+AI",str9,"+5))/U",str9,"+(AK",str9,"/(V",str9,"/1000)),IF(S",str9,">0,(((AH",str9,"*0.8)*S",str9,")+((1-S",str9,")*AJ",str9,")+AI",str9,"+5),(((AH",str9,"*0.8)*T",str9,")+((1-T",str9,")*AJ",str9,")+AI",str9,"+5))/U",str9,"+(AK",str9,"/1000)))"
                  setprop sheet2.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet2.range(range1),*Formula=taskname2
                  setprop sheet3.range(range1),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                  setprop sheet3.range(range1),*Formula=taskname2
.
.SHEET 4 GETS 'MASTER' QTY AND NET
                  pack    range1,"E",str12
                    if (NLOLQTY > "0")
                              setprop sheet4.range(range1),*Value=NLOLQTY
                endif
.AVERAGE NET
                  pack    range1,"H",str12
                    move      C0,N35
                    move      NLOLNET,N35
                    mult      ".01",N35
                  setprop sheet4.range(range1),*Value=N35
                    if (RecFlag = YES)
.RECO QTY
                            pack    range1,"L",str9
                    if (NLOLQTY > "0")
                              setprop sheet2.range(range1),*Value=NLOLQTY
                              setprop sheet3.range(range1),*Value=NLOLQTY
                          endif
.AVERAGE NET
                            pack    range1,"U",str9
                              move      C0,N35
                              move      NLOLNET,N35
                              mult      ".01",N35
                            setprop sheet2.range(range1),*Value=N35
                            setprop sheet3.range(range1),*Value=N35
.
                              break
                    endif
.                   until (RecFlag = YES)
                    move      FrmPtr,str1
                    if (str1 = STATLOL)
.PACKAGE
                              PackData.GetCount giving howmany4
                              clear     str1
                              for howmany5,"1",howmany4
                                        getitem   PackData,howmany5,taskname
                                        unpack    taskname,str6,NPKGID,NPKGPNAME,NPRCDATE,NPRCTOTAL
                                        if (str6 = STATPCKNUM)
                                                  move      YES,str1
                                                  break
                                        endif
                              repeat
                              if (str1 <> YES)
                                        move      C1,NPKGPATH
.START PATCH 1.5.1 REMOVED PREVIOUS, TEMPORARY PATCH
..START PATCH 1.4 REPLACED LOGIC
..                                      pack      NPKGFLD,STATMLR,STATPCKNUM
.                                       move      "LoadDetail-COMPKEY3",Location
.                                       pack      COMPFLD3,STATMLR
.                                       pack      KeyLocation,"Key: ",COMPFLD3
.                                       call      COMPKEY3
.                                       pack      NPKGFLD,COMPNUM,STATPCKNUM
..END PATCH 1.4 REPLACED LOGIC
                                        pack      NPKGFLD,STATMLR,STATPCKNUM
.END PATCH 1.5.1 REMOVED PREVIOUS, TEMPORARY PATCH
                                        move      "LoadDetail-NPKGKEY",Location
                                        pack      KeyLocation,"Key: ",NPKGFLD
                                        call      NPKGKEY
.START PATCH 1.2 REMOVED LOGIC
.                                       if over
.                                                 clear     NPKGPNAME
.                                       endif
.                                       if (NPKGMaster = "1")
.                                                 clear     NPKGPNAME
.                                                 clear     NPRCTOTAL
.                                       else
.                                                 call      Trim using NPKGPNAME
.END PATCH 1.2 REMOVED LOGIC
                                                  unpack    STATPCKM,str5
                                                  move      C0,JULDAYS
                                                  move      str5,JULDAYS
                                                  if (JULDAYS > C0)
                                                            call      CVTGREG
                                                            if (YY < "80")
                                                                      move      "20",str2
                                                            else
                                                                      move      "19",str2
                                                            endif
                                                            pack      str10,str2,YY,MM,DD
                                                            move      C1,NPRCPATH
.START PATCH 1.5.1 REMOVED PREVIOUS, TEMPORARY PATCH
..START PATCH 1.4 REPLACED LOGIC
..                                                          pack      NPRCFLD,STATMLR,STATPCKNUM,str10
.                                                           pack      NPRCFLD,COMPNUM,STATPCKNUM,str10
..END PATCH 1.4 REPLACED LOGIC
                                                            pack      NPRCFLD,STATMLR,STATPCKNUM,str10
.END PATCH 1.5.1 REMOVED PREVIOUS, TEMPORARY PATCH
                                                            move      "LoadDetail-NPRCKEY",Location
                                                            pack      KeyLocation,"Key: ",NPRCFLD
                                                            call      NPRCKEY
                                                            if over
                                                                      clear     NPRCTOTAL
                                                            endif
                                                  else
                                                            clear     NPRCTOTAL
                                                  endif
                                                  pack      taskname,NPKGNUM,NPKGID,NPKGPNAME,NPRCDATE,NPRCTOTAL
                                                  insertitem PackData,999999,taskname
.START PATCH 1.2 REMOVED LOGIC
.                                       endif
.END PATCH 1.2 REMOVED LOGIC
                              endif
                            pack    range1,"J",str9
                            setprop sheet2.range(range1),*Value=NPKGPNAME
                            setprop sheet3.range(range1),*Value=NPKGPNAME
.PACKAGE COST
.START PATCH 1.22 ADDED LOGIC
                              if (STATPCKM = C0)
                                        move      C0,NPRCTOTAL
                              endif
.END PATCH 1.22 ADDED LOGIC
                            pack    range1,"K",str9
                              move      NPRCTOTAL,str8
                              call      Trim using str8
                            setprop sheet2.range(range1),*Value=str8
                            setprop sheet3.range(range1),*Value=str8
.RECO QTY
                            pack    range1,"L",str9
                    if (STATRECQTY <> C0)
                              setprop sheet2.range(range1),*Value=STATRECQTY
                              setprop sheet3.range(range1),*Value=STATRECQTY
                          endif
.RECEIVED QTY
                            pack    range1,"M",str9
                  if (STATMQTY <> C0)
                            setprop sheet2.range(range1),*Value=STATMQTY
                            setprop sheet3.range(range1),*Value=STATMQTY
                          endif
.Accumulate for Sheet4
                            pack    range1,"F",str12
                              add       STATMQTY,MASTERMQTY
                  if (MASTERMQTY <> C0)
                              setprop sheet4.range(range1),*Value=MASTERMQTY
                          endif                   
.LIST COST PER/M
                            pack    range1,"Q",str9
                            setprop sheet2.range(range1),*Value=STATLCPM
                            setprop sheet3.range(range1),*Value=STATLCPM
.NET RECEIVED
                            pack    range1,"S",str9
                              mult      ".01",STATNETREC,N35
                            setprop sheet2.range(range1),*Value=N35
                            setprop sheet3.range(range1),*Value=N35
.NET REQUESTED
                            pack    range1,"T",str9
                              mult      ".01",STATNETREQ,N35
                            setprop sheet2.range(range1),*Value=N35
                            setprop sheet3.range(range1),*Value=N35
.AVERAGE NET
                            pack    range1,"U",str9
                              mult      ".01",STATAVGNET,N35
                            setprop sheet2.range(range1),*Value=N35
                            setprop sheet3.range(range1),*Value=N35
.RESPONSE RATE
                            pack    range1,"W",str9
                              mult      ".01",STATRESP2,N35
                            setprop sheet2.range(range1),*Value=N35
                            setprop sheet3.range(range1),*Value=N35
.GIFT
                            pack    range1,"Y",str9
                    setprop sheet2.range(range1),*Value=STATGIFT
                    setprop sheet3.range(range1),*Value=STATGIFT
.COMMENTS
                              move      C1,STATNPATH
                              pack      STATNFLD,STATLR,STATLOL,STATPCKNUM
                              move      "LoadRec-STATNKEY",Location
                              pack      KeyLocation,"Key: ",STATNFLD
                              call      STATNKEY
                              call      Trim using STATNNOTE
                            pack    range1,"AF",str9
                              pack      str2,NewLine,B1
                              rep       str2,STATNNOTE
                            setprop sheet2.range(range1),*Value=STATNNOTE
                            setprop sheet3.range(range1),*Value=STATNNOTE
.EXCHANGE BASE
                            pack    range1,"AG",str9
                            setprop sheet2.range(range1),*Value=STATEXBASE
                            setprop sheet3.range(range1),*Value=STATEXBASE
.RENT BASE
                            pack    range1,"AH",str9
                            setprop sheet2.range(range1),*Value=STATRBASE
                            setprop sheet3.range(range1),*Value=STATRBASE
.SELECT FEE
                            pack    range1,"AI",str9
                    setprop sheet2.range(range1),*Value=STATSELFEE
                    setprop sheet3.range(range1),*Value=STATSELFEE
.RUN CHARGE
                            pack    range1,"AJ",str9
                            setprop sheet2.range(range1),*Value=STATRUN
                            setprop sheet3.range(range1),*Value=STATRUN
.SHIP/TAX
                            pack    range1,"AK",str9
                    setprop sheet2.range(range1),*Value=STATSHIP
                    setprop sheet3.range(range1),*Value=STATSHIP
.END PATCH 1.2 REPLACED LOGIC
                    endif
                    move      "LoadDetail-STAT2KG",Location
                    pack      KeyLocation,"Key: ",STAT2FLD2
                    loop
                              call      STAT2KG
                              until over
                              move      FrmPtr,str1
                              until (str1 = STATLOL)
                    repeat
                    until over
          repeat
        return

drewer
          move      str1,str1
          return
        include ncmpio.inc
        include nordio.inc
        include nord4io.inc
        include nord5io.inc
        include nspe2io.inc
        include nlolio.inc
        include nlolio2.inc
          include npkgio.inc
          include   nprcio.inc
          include   statsio.inc
          include   statsio2.inc
          include   statnio.inc
        include npndio.inc
.START PATCH 1.33 REPLACED LOGIC
.        include nmlrio.inc
.        include nbrkio.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
.END PATCH 1.33 REPLACED LOGIC
        include nrtnio.inc
        include nxrfio.inc
        include nxngio.inc
        include nxchio.inc
        include ndatio.inc
        include nofrio.inc
        include nsmpio.inc
        include ncntio.inc
.START PATCH 1.32 ADDED LOGIC
          INCLUDE   NSEL2IO.INC
.END PATCH 1.32 ADDED LOGIC
        include comlogic.inc
