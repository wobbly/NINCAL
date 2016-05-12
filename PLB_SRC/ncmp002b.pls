.CAMPAIGN SPREADSHEET INCLUDING PROJECTIONS - NWF TEMPLATE
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
.         include   f:\library\develop\backups\nloldd.inc
          include npkgdd.inc
          include   nprcdd.inc
          include   statsdd.inc
.         include   f:\library\develop\backups\statsdd.inc
          include   statndd.inc
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
.        include f:\library\develop\backups\nxngdd.inc
.        include f:\library\develop\backups\nxchdd.inc
        include ndatdd.inc
        include nofrdd.inc
        include nsmpdd.inc
        include ncntdd.inc
        include oslspern.inc
        include media.inc
.START PATCH 1.53 ADDED LOGIC
          INCLUDE   NSEL2DD.INC
.END PATCH 1.53 ADDED LOGIC

Release    INIT    "1.60"               DLH      .Excel 2013 *WindowState=xlMinimized
Reldate   Init      "2014 January 22"
.release init    "1.59"                  12JUN2007  ASH      PLI Inclusiong
.release init    "1.58"                 08Mar2007  DLH      Oslspern.inc expansion
.Release  init      "1.57"    01DEC2004 ASH       CONVERTED CAMPAIGN MAILER TO 6 BYTES
.Release  init      "1.56"    25OCT2004 ASH       CONVERTED MAILER PACKAGE TO 6 BYTES
.release init    "1.55.1"           13SEP2004 ASH LOGO CONVERSION - FIRST PATCH
.release init    "1.55"           04AUG04 ASH     LOGO CONVERSION
.release init    "1.54"           2YMAY04 ASH     MAILER CONVERSION
.release init    "1.53"           26JAN04 ASH     DATACARD CONVERSION
.release init    "1.52.3"           08OCT03 ASH     ADDED MORE SMALL PATCHES AS PER HM
.release init    "1.52.2"           25SEP03 ASH     ADDED SMALL FUNCTION PATCHES AS PER HM
.release init    "1.52.1"           26NOV02 ASH     Added Feature to Workbook
.release init    "1.52"           19NOV02 ASH     Added more new features, including Worksheet Menu Bar
.release init    "1.51"           15NOV02 ASH     Added features
.release init    "1.5"           13NOV02 ASH     Calculation corrections
.release init    "1.4"           28OCT02 ASH     Added NINCA logo - Idea of the Month
.release  init      "1.3"     ASH 06SEP2002 ADDED VAR/LOGIC TO ALLOW MORE THAN 100 RECORDS
.release init    "1.2"        ASH 18APR2002 FIXES APPLIED
.release init    "1.1"        ASH 16APR2002 FIXES APPLIED
.release init    "1.0"        ASH 06SEP2001 NEW RELEASE
.EXTERNAL ROUTINES FROM NORDTEST.PLC
CampOrderGetHistory external "NORDTEST;OrderGetHistory"
CampOrderCalcUsage external "NORDTEST;OrderCalcUsage"
.START PATCH 1.1 ADDED VAR
.EXCEL will not allow a Tab Name to exceed 31 characters, so I must cut Package Name back
str31     dim       31
.END PATCH 1.1 ADDED VAR
.
N35     form    3.5
index     form      9
RecFlag   form      1
.START PATCH 1.3 ADDED LOGIC
range     dim       20
range2    dim       20
.END PATCH 1.3 ADDED LOGIC
ComboPtr ComboBox ^
.WindPtr window  ^
StrPtr  dim     ^
StrPtr1 dim     ^
.START PATCH 1.59 ADDED LOGIC
StrPtr2 dim     ^
SReturn        init 0x0a                     ;soft return/line feed
.END PATCH 1.59 ADDED LOGIC
DimPtr    dim       ^
FrmPtr    form      ^
FrmPtr2   form      ^
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
sheet     automation
sortcol automation
sortcol1 automation
ex      automation      class="Excel.Application"
Data1     datalist
.
mauve     color
colornum form       24
yellow    color
.Variables needed by OrderLoadXSTAT
EFLAG   dim     1
AKey1A  init    "01L"
AKey2A  init    "02L"
FirstRec form   9(50)
LastRec   form      9(50)
hdrrow    form      9(50)
PackPtr   form      4(50,2)   .Locations of: (Package Cost,Premium Cost/Premium Percentage)
CALCPER   FORM      7.4
.
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
Zoom70  variant
SheetIndex variant
SheetIndex2 variant
.START PATCH 1.55 ADDED LOGIC
xlRowHeight         variant
VT_R8     EQU 5           .Double - 8 byte Real
.END PATCH 1.55 ADDED LOGIC
.START PATCH 1.55.1 ADDED LOGIC
TopMargin variant
BottomMargin        variant
.END PATCH 1.55.1 ADDED LOGIC
.Formatting vars needed
.This constant was found in the Object Browser in Excel under the Help topic for the
.HorizontalAlignment property of the Range object.
AlignRight integer 4,"0xffffefc8"
AlignCenter integer 4,"0xffffeff4"
SheetsDefault integer 4,"0x00000000"
xlLandscape integer 4,"0x2"                     .2
xlMinimized integer 4,"0xFFFFEFD4"
MedThick integer 4,"0xFFFFEFD6"
.START PATCH 1.4 ADDED LOGIC
xlUnderlineStyleSingle integer 4,"0x2"
.END PATCH 1.4 ADDED LOGIC

.START PATCH 1.59 REPLACED LOGIC
.CreateCampaignB Routine StrPtr,ComboPtr,StrPtr1
CreateCampaignB Routine StrPtr,ComboPtr,StrPtr1,StrPtr2
.END PATCH 1.59 REPLACED LOGIC
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
.Others
        create  Zoom70,VarType=VT_I4,VarValue=70
.START PATCH 1.55 ADDED LOGIC
          create    xlRowHeight,VarType=VT_R8,VarValue="75.0"
.END PATCH 1.55 ADDED LOGIC
.START PATCH 1.55.1 ADDED LOGIC
."1" increment in Excel interface equals "1.3888" in OLE logic
          create    TopMargin,VarType=VT_R8,VarValue="18"             Roughly equals .25 inches:  18 * 1.388 = 25
          create    BottomMargin,VarType=VT_R8,VarValue="36"          Roughly equals .50 inches:  36 * 1.388 = 50
.END PATCH 1.55.1 ADDED LOGIC
.
          create    SheetIndex,VarType=VT_I4
          create    SheetIndex2,VarType=VT_I4
.Colors
        create  mauve=150:60:100
          getitem   mauve,0,colornum
          create    yellow=255:255:160
.Initialize variables
        packkey     NCMPFLD,StrPtr
        move    C1,NCMPPATH
        move    "CreateC.-NCMPKEY",Location
        pack    KeyLocation,"Key: ",NCMPFLD
        call    NCMPKEY
        if over
                return
        endif
.Open Excel application
        create  ex
.begin patch 1.60
.        setprop ex,*WindowState=xlMinimized
.end patch 1.60
.START PATCH 1.52 REPLACED LOGIC
..START PATCH 1.51 ADDED LOGIC
.         setprop ex,*IgnoreRemoteRequests="True",*Interactive="False"
..END PATCH 1.51 ADDED LOGIC
        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
          setprop ex.CommandBars("Standard"),*Visible="True"
          setprop ex.CommandBars("Formatting"),*Visible="True"
          setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.END PATCH 1.52 REPLACED LOGIC
.Reset Default of Worksheets found in a Workbook
        getprop ex,*SheetsInNewWorkbook=SheetsDefault
        setprop ex,*SheetsInNewWorkbook=C1
.Create Workbooks collection
        getprop ex,*Workbooks=books
.Create/Add a single Workbook
        books.add
        books.item giving book using 1
.START PATCH 1.52.1 ADDED LOGIC
          setprop   book,*PrecisionAsDisplayed="True"
.END PATCH 1.52.11 ADDED LOGIC
.Create Worksheets collection
        getprop book,*Sheets=sheets
.
          move      C0,index
          clear     FirstRec
          clear     LastRec
          clear     hdrrow
          destroy   Data1
          create    Data1=1:1:1:1
        move    NCMPFLD,NLOLFLD1
        move    C2,NLOLPATH
        move    "LoadRecs-NLOLKEY",Location
        pack    KeyLocation,"Key: ",NLOLFLD1
        call    NLOLKEY
        if not over
                  move    "LoadRecs-NLOLKS",Location
                  pack    KeyLocation,"Key: ",NLOLFLD1
                  loop
                          until (NLOLCNUM <> NLOLFLD1)
                              pack      STAT2FLD2,"01X",NLOLLOL
                              call      LoadStatDetail using C1
                          call    NLOLKS
                          until over
                  repeat
          endif
.NINORD records
        move    NCMPFLD,NORDFLDC
        move    C4,NORDPATH
        move    "LoadRecs-NORDKEY",Location
        pack    KeyLocation,"Key: ",NORDFLDC
        call    NORDKEY
        if not over
                move    "LoadRecs-NORDKS",Location
                pack    KeyLocation,"Key: ",NORDFLDC
                loop
                        until (OCAMP <> NORDFLDC)
                              call    OrderLoadLOLPackLOL
                              pack      STAT2FLD2,"01X",OLRN
                              call      LoadStatDetail using C0
                        call    NORDKS
                        until over
                repeat
        endif
.....FORMATTING FOR WHOLE DOCUMENT
          getprop   sheets,*Count=N8
          for       index,"1",N8
                    setprop   SheetIndex,VarValue=index
                    move      LastRec(index),str10
                    call      Trim using str10
.Total Lines
                    add       C2,LastRec(index),result
                    move      result,str11
                    call    Trim using str11
                    call      LoadStatsSubHeader using str11
                    add       C2,result
                    move      result,str11
                    call    Trim using str11
.START PATCH 1.3 REPLACED LOGIC
.                   pack    str5,"C",str11
.                   setprop sheets(SheetIndex).range(str5),*Value="T O T A L S",*HorizontalAlignment=AlignCenter
.                   setprop   sheets(SheetIndex).Range(str5).Font,*Size=8
                    pack    range,"C",str11
                    setprop sheets(SheetIndex).range(range),*Value="T O T A L S",*HorizontalAlignment=AlignCenter
                    setprop   sheets(SheetIndex).Range(range).Font,*Size=8
.END PATCH 1.3 REPLACED LOGIC
.
                    move      FirstRec(index),str9
                    call      Trim using str9
.
.START PATCH 1.3 REPLACED LOGIC
.                   pack    str5,"D",str11
.                   pack      str45,"=SUM(D",str9,":D",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="##,####0_);[Red](##,####0)" 
.
.                   pack    str5,"E",str11
.                   pack      str45,"=SUM(E",str9,":E",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="##,####0_);[Red](##,####0)"
..
.                   pack    str5,"I",str11

                    pack    range,"D",str11
                    pack      str45,"=SUM(D",str9,":D",str10,")"
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="##,####0_);[Red](##,####0)"
.
                    pack    range,"E",str11
                    pack      str45,"=SUM(E",str9,":E",str10,")"
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="##,####0_);[Red](##,####0)"
.
                    pack    range,"I",str11
.END PATCH 1.3 REPLACED LOGIC
.START PATCH 1.2 REPLACED LOGIC
..=+U92/(O92/1000)
.                   pack      str45,"=+U",str11,"/(O",str11,"/1000)"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..
.                   pack    str5,"N",str11
..=+O92/E92
.                   pack      str45,"=+O",str11,"/E",str11
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="0.00%"
..
.                   pack    str5,"O",str11
.                   pack      str45,"=SUM(O",str9,":O",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="##,####0_);[Red](##,####0)"
..
.                   pack    str5,"P",str11
..=+Q92/O92
.                   pack      str45,"=+Q",str11,"/O",str11
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="0.00%"
..
.                   pack    str5,"Q",str11
.                   pack      str45,"=SUM(Q",str9,":Q",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="##,####0_);[Red](##,####0)"
..
.                   pack    str5,"R",str11
..=IF(S92>0,+S92/Q92,0)
.                   pack      str45,"=IF(S",str11,"<>0,+S",str11,"/Q",str11,",0)"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
..
.                   pack    str5,"S",str11
.                   pack      str45,"=SUM(S",str9,":S",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                   setprop   sheets(SheetIndex).Range(str5).Font,*Size=8
..
.                   pack    str5,"T",str11
.                   pack      str45,"=SUM(T",str9,":T",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                   setprop   sheets(SheetIndex).Range(str5).Font,*Size=8
..
.                   pack    str5,"U",str11
.                   pack      str45,"=SUM(U",str9,":U",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                   setprop   sheets(SheetIndex).Range(str5).Font,*Size=8
..
.                   pack    str5,"V",str11
.                   pack      str45,"=SUM(V",str9,":V",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                   setprop   sheets(SheetIndex).Range(str5).Font,*Size=8
..
.                   pack    str5,"W",str11
..=S92-V92
.                   pack      str45,"=S",str11,"-V",str11
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                   setprop   sheets(SheetIndex).Range(str5).Font,*Size=8
..
.                   pack    str5,"X",str11
..=IF(Q92>0,IF(W92>0,W92/Q92,0),0)
.                   pack      str45,"=IF(Q",str11,"<>0,IF(W",str11,"<>0,W",str11,"/Q",str11,",0),0)"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..
.                   pack    str5,"AB",str11
.                   pack      str45,"=SUM(AB",str9,":AB",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AC",str11
.                   pack      str45,"=SUM(AC",str9,":AC",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AD",str11
.                   pack      str45,"=AC",str11,"/AB",str11
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AE",str11
.                   pack      str45,"=SUM(AE",str9,":AE",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AF",str11
.                   pack      str45,"=AE",str11,"/AC",str11
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AG",str11
.                   pack      str45,"=SUM(AG",str9,":AG",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AH",str11
.                   pack      str45,"=SUM(AH",str9,":AH",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AI",str11
.                   pack      str45,"=AH",str11,"/AC",str11
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
.
...............................
.START PATCH 1.3 REPLACED LOGIC
..=+Y92/(O92/1000)
.                   pack      str45,"=+Y",str11,"/(O",str11,"/1000)"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..
.                   pack    str5,"N",str11
..=+O92/E92
.                   pack      str45,"=+O",str11,"/E",str11
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="0.00%"
..
.                   pack    str5,"O",str11
.                   pack      str45,"=SUM(O",str9,":O",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="##,####0_);[Red](##,####0)"
..
.                   pack    str5,"P",str11
..=+S92/O92
.                   pack      str45,"=+S",str11,"/O",str11
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="0.00%"
..
.                   pack    str5,"S",str11
.                   pack      str45,"=SUM(S",str9,":S",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="##,####0_);[Red](##,####0)"
..
.                   pack    str5,"T",str11
..=IF(W92>0,+W92/S92,0)
.                   pack      str45,"=IF(W",str11,"<>0,+W",str11,"/S",str11,",0)"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
..
.                   pack    str5,"W",str11
.                   pack      str45,"=SUM(W",str9,":W",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                   setprop   sheets(SheetIndex).Range(str5).Font,*Size=8
..
.                   pack    str5,"X",str11
.                   pack      str45,"=SUM(X",str9,":X",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                   setprop   sheets(SheetIndex).Range(str5).Font,*Size=8
..
.                   pack    str5,"Y",str11
.                   pack      str45,"=SUM(Y",str9,":Y",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                   setprop   sheets(SheetIndex).Range(str5).Font,*Size=8
..
.                   pack    str5,"Z",str11
.                   pack      str45,"=SUM(Z",str9,":Z",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                   setprop   sheets(SheetIndex).Range(str5).Font,*Size=8
..
.                   pack    str5,"AA",str11
..=W92-Z92
.                   pack      str45,"=W",str11,"-Z",str11
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.                   setprop   sheets(SheetIndex).Range(str5).Font,*Size=8
..
.                   pack    str5,"AB",str11
..=IF(S92>0,IF(AA92>0,AA92/S92,0),0)
.                   pack      str45,"=IF(S",str11,"<>0,IF(AA",str11,"<>0,AA",str11,"/S",str11,",0),0)"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..
.                   pack    str5,"AF",str11
.                   pack      str45,"=SUM(AF",str9,":AF",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AG",str11
.                   pack      str45,"=SUM(AG",str9,":AG",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AH",str11
.                   pack      str45,"=AG",str11,"/AF",str11
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AI",str11
.                   pack      str45,"=SUM(AI",str9,":AI",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AJ",str11
.                   pack      str45,"=AI",str11,"/AG",str11
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AK",str11
.                   pack      str45,"=SUM(AK",str9,":AK",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AL",str11
.                   pack      str45,"=SUM(AL",str9,":AL",str10,")"
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str5,"AM",str11
.                   pack      str45,"=AL",str11,"/AG",str11
.                   setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.                   pack    str4,"AO",str9
.                   pack    str5,"AO",str10
..=AVERAGE(AB$18:AB$105)+100
.                   pack      str45,"=AVERAGE(AB$",str9,":AB$",str10,")+100"
.                   setprop sheets(SheetIndex).range(str4,str5),*Formula=str45
...............................
.=+Y92/(O92/1000)
                    pack      str45,"=+Y",str11,"/(O",str11,"/1000)"
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.
                    pack    range,"N",str11
.=+O92/E92
                    pack      str45,"=+O",str11,"/E",str11
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="0.00%"
.
                    pack    range,"O",str11
                    pack      str45,"=SUM(O",str9,":O",str10,")"
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="##,####0_);[Red](##,####0)"
.
.START PATCH 1.52.3 REPLACED LOGIC
.                   pack    range,"P",str11
                    pack    range,"R",str11
.END PATCH 1.52.3 REPLACED LOGIC
.=+S92/O92
                    pack      str45,"=+S",str11,"/O",str11
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="0.00%"
.
                    pack    range,"S",str11
                    pack      str45,"=SUM(S",str9,":S",str10,")"
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="##,####0_);[Red](##,####0)"
.
.START PATCH 1.52.3 REPLACED LOGIC
.                   pack    range,"T",str11
                    pack    range,"V",str11
.END PATCH 1.52.3 REPLACED LOGIC
.=IF(W92>0,+W92/S92,0)
                    pack      str45,"=IF(W",str11,"<>0,+W",str11,"/S",str11,",0)"
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.
                    pack    range,"W",str11
                    pack      str45,"=SUM(W",str9,":W",str10,")"
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                    setprop   sheets(SheetIndex).Range(range).Font,*Size=8
.
                    pack    range,"X",str11
                    pack      str45,"=SUM(X",str9,":X",str10,")"
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                    setprop   sheets(SheetIndex).Range(range).Font,*Size=8
.
                    pack    range,"Y",str11
                    pack      str45,"=SUM(Y",str9,":Y",str10,")"
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                    setprop   sheets(SheetIndex).Range(range).Font,*Size=8
.
                    pack    range,"Z",str11
                    pack      str45,"=SUM(Z",str9,":Z",str10,")"
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                    setprop   sheets(SheetIndex).Range(range).Font,*Size=8
.
                    pack    range,"AA",str11
.=W92-Z92
                    pack      str45,"=W",str11,"-Z",str11
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
                    setprop   sheets(SheetIndex).Range(range).Font,*Size=8
.
                    pack    range,"AB",str11
.=IF(S92>0,IF(AA92>0,AA92/S92,0),0)
                    pack      str45,"=IF(S",str11,"<>0,IF(AA",str11,"<>0,AA",str11,"/S",str11,",0),0)"
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.
.START PATCH 1.5 ADDED LOGIC
.INC M
                    pack    range,"AC",str11
.=IF(O19>0,IF(W19<>0,W19/O19*1000,0),0)
                    pack      taskname,"=IF(O",str11,">0,IF(W",str11,"<>0,W",str11,"/O",str11,"*1000,0),0)"
                    setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                    setprop sheets(SheetIndex).range(range),*Formula=taskname
.COST/M
                    pack    range,"AD",str11
.=IF(O20>0,IF(Z20<>0,Z20/O20*1000,0),0)
                    pack      taskname,"=IF(O",str11,">0,IF(Z",str11,"<>0,Z",str11,"/O",str11,"*1000,0),0)"
                    setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                    setprop sheets(SheetIndex).range(range),*Formula=taskname
.COST?$
                    pack    range,"AE",str11
.=IF(W19>0,IF(Z19<>0,Z19/W19,0),0)
                    pack      taskname,"=IF(W",str11,">0,IF(Z",str11,"<>0,Z",str11,"/W",str11,",0),0)"
                    setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
                    setprop sheets(SheetIndex).range(range),*Formula=taskname
.END PATCH 1.5 ADDED LOGIC
                    pack    range,"AF",str11
.START PATCH 1.5 REPLACED LOGIC
.                   pack      str45,"=SUM(AF",str9,":AF",str10,")"
                    pack      str45,"=(AF",str10,")"
.END PATCH 1.5 REPLACED LOGIC
                    setprop sheets(SheetIndex).range(range),*Formula=str45
.
                    pack    range,"AG",str11
.START PATCH 1.5 REPLACED LOGIC
.                   pack      str45,"=SUM(AG",str9,":AG",str10,")"
                    pack      str45,"=(AG",str10,")"
.END PATCH 1.5 REPLACED LOGIC
                    setprop sheets(SheetIndex).range(range),*Formula=str45
.
                    pack    range,"AH",str11
                    pack      str45,"=AG",str11,"/AF",str11
.START PATCH 1.5 REPLACED LOGIC
.                   setprop sheets(SheetIndex).range(range),*Formula=str45
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="0.00%"
.END PATCH 1.5 REPLACED LOGIC
.
                    pack    range,"AI",str11
.START PATCH 1.5 REPLACED LOGIC
.                   pack      str45,"=SUM(AI",str9,":AI",str10,")"
                    pack      str45,"=(AI",str10,")"
.END PATCH 1.5 REPLACED LOGIC
                    setprop sheets(SheetIndex).range(range),*Formula=str45
.
                    pack    range,"AJ",str11
                    pack      str45,"=AI",str11,"/AG",str11
.START PATCH 1.5 REPLACED LOGIC
.                   setprop sheets(SheetIndex).range(range),*Formula=str45
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.END PATCH 1.5 REPLACED LOGIC
.
                    pack    range,"AK",str11
.START PATCH 1.5 REPLACED LOGIC
.                   pack      str45,"=SUM(AK",str9,":AK",str10,")"
                    pack      str45,"=(AK",str10,")"
.END PATCH 1.5 REPLACED LOGIC
                    setprop sheets(SheetIndex).range(range),*Formula=str45
.
                    pack    range,"AL",str11
.START PATCH 1.5 REPLACED LOGIC
.                   pack      str45,"=SUM(AL",str9,":AL",str10,")"
                    pack      str45,"=(AL",str10,")"
.END PATCH 1.5 REPLACED LOGIC
                    setprop sheets(SheetIndex).range(range),*Formula=str45
.
                    pack    range,"AM",str11
                    pack      str45,"=AL",str11,"/AG",str11
.START PATCH 1.5 REPLACED LOGIC
.                   setprop sheets(SheetIndex).range(range),*Formula=str45
                    setprop sheets(SheetIndex).range(range),*Formula=str45,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.END PATCH 1.5 REPLACED LOGIC
.
                    pack    range,"AO",str9
                    pack    range2,"AO",str10
.=AVERAGE(AB$18:AB$105)+100
                    pack      str45,"=AVERAGE(AB$",str9,":AB$",str10,")+100"
                    setprop sheets(SheetIndex).range(range,range2),*Formula=str45
.END PATCH 1.3 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
.
.START PATCH 1.3 REPLACED LOGIC
.                   move      hdrrow(index),str9
.                   call      Trim using str9
.                   pack    str4,"A",str9
.                   pack    str5,"A",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"B",str9
.                   pack    str5,"B",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"C",str9
.                   pack    str5,"C",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"D",str9
.                   pack    str5,"D",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"E",str9
.                   pack    str5,"E",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"F",str9
.                   pack    str5,"F",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"G",str9
.                   pack    str5,"G",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"H",str9
.                   pack    str5,"H",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"I",str9
.                   pack    str5,"I",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"J",str9
.                   pack    str5,"J",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"K",str9
.                   pack    str5,"K",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"L",str9
.                   pack    str5,"L",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"M",str9
.                   pack    str5,"M",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"N",str9
.                   pack    str5,"N",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"O",str9
.                   pack    str5,"O",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"P",str9
.                   pack    str5,"P",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
............................
                    move      hdrrow(index),str9
                    call      Trim using str9
                    pack    range,"A",str9
                    pack    range2,"A",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"B",str9
                    pack    range2,"B",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"C",str9
                    pack    range2,"C",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"D",str9
                    pack    range2,"D",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"E",str9
                    pack    range2,"E",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"F",str9
                    pack    range2,"F",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"G",str9
                    pack    range2,"G",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"H",str9
                    pack    range2,"H",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"I",str9
                    pack    range2,"I",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"J",str9
                    pack    range2,"J",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"K",str9
                    pack    range2,"K",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"L",str9
                    pack    range2,"L",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"M",str9
                    pack    range2,"M",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"N",str9
                    pack    range2,"N",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"O",str9
                    pack    range2,"O",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"P",str9
                    pack    range2,"P",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
.END PATCH 1.3 REPLACED LOGIC
.START PATCH 1.2 REPLACED LOGIC
.                   pack    str4,"Q1"
.                   pack    str5,"Q",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"R",str9
.                   pack    str5,"R",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"S",str9
.                   pack    str5,"S",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"T1"
.                   pack    str5,"T",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"U",str9
.                   pack    str5,"U",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"V",str9
.                   pack    str5,"V",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"W",str9
.                   pack    str5,"W",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"X",str9
.                   pack    str5,"X",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"Y",str9
.                   pack    str5,"Y",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"Z",str9
.                   pack    str5,"Z",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AA",str9
.                   pack    str5,"AA",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AB",str9
.                   pack    str5,"AB",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AC",str9
.                   pack    str5,"AC",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AD",str9
.                   pack    str5,"AD",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AE",str9
.                   pack    str5,"AE",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AF",str9
.                   pack    str5,"AF",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AG",str9
.                   pack    str5,"AG",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AH",str9
.                   pack    str5,"AH",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AI",str9
.                   pack    str5,"AI",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AJ",str9
.                   pack    str5,"AJ",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AK",str9
.                   pack    str5,"AK",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AM",str9
.                   pack    str5,"AM",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
...........................
.START PATCH 1.3 REPLACED LOGIC
.                   pack    str4,"S1"
.                   pack    str5,"S",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"T",str9
.                   pack    str5,"T",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"W",str9
.                   pack    str5,"W",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"X1"
.                   pack    str5,"X",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"Y",str9
.                   pack    str5,"Y",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"Z",str9
.                   pack    str5,"Z",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AA",str9
.                   pack    str5,"AA",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AB",str9
.                   pack    str5,"AB",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AC",str9
.                   pack    str5,"AC",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AD",str9
.                   pack    str5,"AD",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AE",str9
.                   pack    str5,"AE",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AF",str9
.                   pack    str5,"AF",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AG",str9
.                   pack    str5,"AG",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AH",str9
.                   pack    str5,"AH",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AI",str9
.                   pack    str5,"AI",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AJ",str9
.                   pack    str5,"AJ",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AK",str9
.                   pack    str5,"AK",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AL",str9
.                   pack    str5,"AL",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AM",str9
.                   pack    str5,"AM",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AQ",str9
.                   pack    str5,"AQ",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AR",str9
.                   pack    str5,"AR",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AS",str9
.                   pack    str5,"AS",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
.                   pack    str4,"AT",str9
.                   pack    str5,"AT",str11
.                   sheets(SheetIndex).range(str4,str5).Columns.Autofit
..............
                    pack    range,"S1"
                    pack    range2,"S",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"T",str9
                    pack    range2,"T",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"W",str9
                    pack    range2,"W",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"X1"
                    pack    range2,"X",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"Y",str9
                    pack    range2,"Y",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"Z",str9
                    pack    range2,"Z",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AA",str9
                    pack    range2,"AA",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AB",str9
                    pack    range2,"AB",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AC",str9
                    pack    range2,"AC",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AD",str9
                    pack    range2,"AD",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AE",str9
                    pack    range2,"AE",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AF",str9
                    pack    range2,"AF",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AG",str9
                    pack    range2,"AG",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AH",str9
                    pack    range2,"AH",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AI",str9
                    pack    range2,"AI",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AJ",str9
                    pack    range2,"AJ",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AK",str9
                    pack    range2,"AK",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AL",str9
                    pack    range2,"AL",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AM",str9
                    pack    range2,"AM",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AQ",str9
                    pack    range2,"AQ",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AR",str9
                    pack    range2,"AR",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AS",str9
                    pack    range2,"AS",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
                    pack    range,"AT",str9
                    pack    range2,"AT",str11
                    sheets(SheetIndex).range(range,range2).Columns.Autofit
.END PATCH 1.3 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
.Hide Columns
.                   move      LastRec(index),str10
.                   call      Trim using str10
.START PATCH 1.3 REPLACED LOGIC
.                   pack    str4,"F1"
.                   pack    str5,"F",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"G1"
.                   pack    str5,"G",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"H1"
.                   pack    str5,"H",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"J1"
.                   pack    str5,"J",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"L1"
.                   pack    str5,"L",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"M1"
.                   pack    str5,"M",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.................
                    pack    range,"F1"
                    pack    range2,"F",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"G1"
                    pack    range2,"G",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"H1"
                    pack    range2,"H",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"J1"
                    pack    range2,"J",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"L1"
                    pack    range2,"L",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"M1"
                    pack    range2,"M",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
.END PATCH 1.3 REPLACED LOGIC
.START PATCH 1.2 REPLACED LOGIC
.                   pack    str4,"Y1"
.                   pack    str5,"Y",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"Z1"
.                   pack    str5,"Z",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AA1"
.                   pack    str5,"AA",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AB1"
.                   pack    str5,"AB",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AC1"
.                   pack    str5,"AC",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AD1"
.                   pack    str5,"AD",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AE1"
.                   pack    str5,"AE",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AF1"
.                   pack    str5,"AF",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AG1"
.                   pack    str5,"AG",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AH1"
.                   pack    str5,"AH",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AI1"
.                   pack    str5,"AI",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
............................
.START PATCH 1.3 REPLACED LOGIC
.                   pack    str4,"AC1"
.                   pack    str5,"AC",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AD1"
.                   pack    str5,"AD",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AE1"
.                   pack    str5,"AE",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AF1"
.                   pack    str5,"AF",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AG1"
.                   pack    str5,"AG",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AH1"
.                   pack    str5,"AH",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AI1"
.                   pack    str5,"AI",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AJ1"
.                   pack    str5,"AJ",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AK1"
.                   pack    str5,"AK",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AL1"
.                   pack    str5,"AL",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AM1"
.                   pack    str5,"AM",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AN1"
.                   pack    str5,"AN",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AO1"
.                   pack    str5,"AO",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.                   pack    str4,"AP1"
.                   pack    str5,"AP",str11
.                   setprop sheets(SheetIndex).range(str4,str5).Columns,*Hidden=OTRUE
.................
                    pack    range,"AC1"
                    pack    range2,"AC",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"AD1"
                    pack    range2,"AD",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"AE1"
                    pack    range2,"AE",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"AF1"
                    pack    range2,"AF",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"AG1"
                    pack    range2,"AG",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"AH1"
                    pack    range2,"AH",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"AI1"
                    pack    range2,"AI",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"AJ1"
                    pack    range2,"AJ",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"AK1"
                    pack    range2,"AK",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"AL1"
                    pack    range2,"AL",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"AM1"
                    pack    range2,"AM",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"AN1"
                    pack    range2,"AN",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"AO1"
                    pack    range2,"AO",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
                    pack    range,"AP1"
                    pack    range2,"AP",str11
                    setprop sheets(SheetIndex).range(range,range2).Columns,*Hidden=OTRUE
.END PATCH 1.3 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
..Sort by List Name
                    move      LastRec(index),str10
                    call      Trim using str10
.START PATCH 1.2 REPLACED LOGIC
.                   pack      str5,"AN",str10
.START PATCH 1.3 REPLACED LOGIC
.                   pack      str5,"AU",str10
                    pack      range2,"AU",str10
.END PATCH 1.3 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
.START PATCH 1.3 REPLACED LOGIC
..List Name
.                   move      FirstRec(index),str9
.                   call      Trim using str9
.                 pack    str4,"B",str9
.         getprop sheets(SheetIndex).range(str4),*Columns(1)=sortcol
..Select
.                 pack    str4,"C",str9
.         getprop sheets(SheetIndex).range(str4),*Columns(1)=sortcol1
..Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
.                 pack    str4,"A",str9
.         sheets(SheetIndex).range(str4,str5).sort using *Key1=sortcol,*Order1=1,*Key2=sortcol1,*Order2=1
......................
.List Name
                    move      FirstRec(index),str9
                    call      Trim using str9
                  pack    range,"B",str9
          getprop sheets(SheetIndex).range(range),*Columns(1)=sortcol
.Select
                  pack    range,"C",str9
          getprop sheets(SheetIndex).range(range),*Columns(1)=sortcol1
.Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
                  pack    range,"A",str9
          sheets(SheetIndex).range(range,range2).sort using *Key1=sortcol,*Order1=1,*Key2=sortcol1,*Order2=1
.END PATCH 1.3 REPLACED LOGIC
          repeat
.SORT WORKSHEETS USING A BUBBLE SORT
          getprop   sheets,*Count=N8
          for       index,"1",N8
                    setprop   SheetIndex,VarValue=index
                    for       N9,"1",N8
                              getprop   sheets(SheetIndex),*Name=NPKGPNAME
                              call      Trim using NPKGPNAME
                              setprop   SheetIndex2,VarValue=N9
                              getprop   sheets(SheetIndex2),*Name=taskname
                              call      Trim using taskname
                              if (taskname < NPKGPNAME)
                                        getprop   sheets,*Item(SheetIndex)=sheet
                                        sheets(SheetIndex2).Move using *Before=sheet
                                        move      taskname,NPKGPNAME
                              endif
                    repeat
.START PATCH 1.55.1 ADDED LOGIC
                    sheets(SheetIndex).Rows(2).Delete
.END PATCH 1.55.1 ADDED LOGIC
          repeat
.Rename last tab
          setprop   SheetIndex,VarValue=N8
          getprop   sheets(SheetIndex),*Name=NPKGPNAME
          if (NPKGPNAME = "{{{{{{{{{{Independent Records")
                    pack      NPKGPNAME,"Independent Records"
                    setprop   sheets(SheetIndex),*Name=NPKGPNAME
          endif

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
.START PATCH 1.51 ADDED LOGIC
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.END PATCH 1.51 ADDED LOGIC
CampaignCleanUp
.Clean up after myself
        destroy     OTRUE
.START PATCH 1.1 MOVED LOGIC
.        destroy    OFALSE
.END PATCH 1.1 MOVED LOGIC
        destroy     Zoom70
        destroy     SheetIndex
        destroy     SheetIndex2
        destroy     mauve
        destroy     yellow
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
        destroy sortcol
        destroy sortcol1
.START PATCH 1.1 ADDED LOGIC
.I was getting Excel.exe errors before I included following line.
.All automation objects need to be destroyed before you close down spreadsheet!
        destroy sheet
.END PATCH 1.1 ADDED LOGIC
        destroy sheets
        destroy book
        destroy books
          destroy   data1
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        setprop ex,*DisplayAlerts=OFALSE
.START PATCH 1.1 MOVED LOGIC
        destroy     OFALSE
.END PATCH 1.1 MOVED LOGIC
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

LoadStatDetail Routine FrmPtr
.START PATCH 1.53 ADDED LOGIC
          pack      STAT2FLD3,"02X",FrmPtr
.END PATCH 1.53 ADDED LOGIC
          move      "LoadStatsDet.-STAT2AIM",Location
          pack      KeyLocation,"Key: ",STAT2FLD2
          call      STAT2AIM
          if not over
                    move      C0,RecFlag
                    loop
                              if over
                                        if (RecFlag = C0)
                                                  goto Indies
                                        endif
                                        break
                              endif
                              if (FrmPtr = C1)    .LOL Records
                                        move      C1,str1
                              else                          .LR Records
                                        move      C0,str1
                              endif
                              if (str1 = STATLOL)
                                        move      C1,RecFlag
                                        move      C0,N9
                                        Data1.GetCount giving N8
                                        for       N10,C1,N8
                                                  getitem   Data1,N10,taskname
.START PATCH 1.1 REPLACED LOGIC
.                                                 unpack    taskname,NPKGPNAME,str9
                                                  unpack    taskname,str31,str9
.END PATCH 1.1 REPLACED LOGIC
                                                  call      Trim using str9
                                                  if (str9 = STATPCKNUM)
                                                            move      C1,N9
.START PATCH 1.1 REPLACED LOGIC
.                                                           call      Trim using NPKGPNAME
.                                                           getprop   sheets.Item(NPKGPNAME),*Index=SheetIndex

                                                            call      Trim using str31
                                                            getprop   sheets.Item(str31),*Index=SheetIndex
.END PATCH 1.1 REPLACED LOGIC
                                                            getprop   SheetIndex,VarValue=index
                                                            break
                                                  endif
                                        repeat
                                        if (N9 = 0)
.Package Sheet not created
                                                  move      C1,NPKGPATH
.START PATCH 1.57 REPLACED LOGIC
..START PATCH 1.56 REPLACED LOGIC
..                                                pack      NPKGFLD,NCMPMLR,STATPCKNUM
.                                                 move      "LoadStatsDet-COMPKEY3",Location
.                                                 pack      COMPFLD3,NCMPMLR
.                                                 pack      KeyLocation,"Key: ",COMPFLD3
.                                                 call      COMPKEY3
.                                                 pack      NPKGFLD,COMPNUM,STATPCKNUM
..END PATCH 1.56 REPLACED LOGIC
                                                  pack      NPKGFLD,NCMPMLR,STATPCKNUM
.END PATCH 1.57 REPLACED LOGIC
                                                  move      "LoadStatsDet-NPKGKEY",Location
                                                  pack      KeyLocation,"Key: ",NPKGFLD
                                                  call      NPKGKEY
                                                  if over
.Should never happen!!!
.START PATCH 1.1 REPLACED LOGIC
.                                                           clear     taskname
..START PATCH 02/27/2002 ADDED LOGIC
.                                                 elseif (NPKGMaster = "1")
.                                                           clear     taskname
..END PATCH 02/27/2002 ADDED LOGIC
.
                                                            move      "Unassociated Projections!",taskname
.END PATCH 1.1 REPLACED LOGIC
                                                  else
.START PATCH 1.1 REPLACED LOGIC
.                                                           pack      taskname,NPKGPNAME,NPKGNum
.Excel will not allow following characters in Sheet Name:  :\/?*[]
                                                            move      ": ",str2
                                                            rep       str2,NPKGPNAME
                                                            move      "\ ",str2
                                                            rep       str2,NPKGPNAME
                                                            move      "/ ",str2
                                                            rep       str2,NPKGPNAME
                                                            move      "? ",str2
                                                            rep       str2,NPKGPNAME
                                                            move      "* ",str2
                                                            rep       str2,NPKGPNAME
                                                            move      "[ ",str2
                                                            rep       str2,NPKGPNAME
                                                            move      "] ",str2
                                                            rep       str2,NPKGPNAME
                                                            move      NPKGPNAME,str31
                                                            pack      taskname,str31,NPKGNum
.END PATCH 1.1 REPLACED LOGIC
                                                  endif
                                                  insertitem Data1,99999,taskname
.First instance of this Package - create a new Worksheet
.Remember, this Workbook is created with a default of one Worksheet.  Index is initialized to C0.  So adding
.C1 will start us off with the first Worksheet.  All others will have to be created.
                                                  add       C1,index
                                                  if (index > C1)
                                                            getprop   sheets,*Count=N8
                                                            setprop   SheetIndex,VarValue=N8
                                                            getprop   sheets,*Item(SheetIndex)=sheet
                                                            sheets.Add.Move using *After=sheet
                                                            add       C1,N8,index
                                                  endif
                                                  setprop   SheetIndex,VarValue=index
                                                  call      LoadStatsHeader
                                        endif
.START PATCH 1.53 ADDED LOGIC
                                        if (FrmPtr = C1)    .LOL Records
                                                  pack      NSEL2FLD,"2",NLOLLOL
                                                  move      "NSEL2KEY",Location
                                                  pack      KeyLocation,"Key: ",NSEL2FLD
                                                  call      NSEL2KEY
                                                  if not over
                                                            move      NSEL2NAME,NLOLSELECT
                                                            move      NSEL2QTY,statseluni
                                                  endif
                                        else
                                                  move      NLOLUNIVERSE,statseluni
                                        endif
.END PATCH 1.53 ADDED LOGIC
                                        call      LoadStatsRecord using C1
                              endif
                              move      "LoadStatsDet-STAT2KG",Location
                              pack      KeyLocation,"Key: ",STAT2FLD2
                              call      STAT2KG
                    repeat
          else      .Independent Records
Indies
                    move      C0,N9
                    Data1.GetCount giving N8
                    for       N10,C1,N8
.START PATCH 1.1 REPLACED LOGIC
.                             getitem   Data1,N10,NPKGPNAME
.                             call      Trim using NPKGPNAME
.                             if (NPKGPNAME = "{{{{{{{{{{Independent Records")
.                                       move      C1,N9
.                                       call      Trim using NPKGPNAME
.                                       getprop   sheets.Item(NPKGPNAME),*Index=SheetIndex
.                                       getprop   SheetIndex,VarValue=index
.                                       break
.                             endif
                              getitem   Data1,N10,str31
                              call      Trim using str31
                              if (str31 = "{{{{{{{{{{Independent Records")
                                        move      C1,N9
                                        call      Trim using str31
                                        getprop   sheets.Item(str31),*Index=SheetIndex
                                        getprop   SheetIndex,VarValue=index
                                        break
                              endif
.END PATCH 1.1 REPLACED LOGIC
                    repeat
                    if (N9 = 0)
.START PATCH 1.1 REPLACED LOGIC
.                             pack      NPKGPNAME,"{{{{{{{{{{Independent Records",B55,B55,B55
.                             pack      taskname,NPKGPNAME,"XXXXXX"
.                             insertitem Data1,99999,NPKGPNAME
                              pack      str31,"{{{{{{{{{{Independent Records",B55
                              pack      taskname,str31,"XXXXXX"
                              insertitem Data1,99999,str31
.END PATCH 1.1 REPLACED LOGIC
.First instance of this Package - create a new Worksheet
.Remember, this Workbook is created with a default of one Worksheet.  Index is initialized to C0.  So adding
.C1 will start us off with the first Worksheet.  All others will have to be created.
                              add       C1,index
                              if (index > C1)
                                        getprop   sheets,*Count=N8
                                        setprop   SheetIndex,VarValue=N8
                                        getprop   sheets,*Item(SheetIndex)=sheet
                                        sheets.Add.Move using *After=sheet
                                        add       C1,N8,index
                              endif
                              setprop   SheetIndex,VarValue=index
                              call      LoadStatsHeader
                    endif
.START PATCH 1.53 ADDED LOGIC
                    if (FrmPtr = C1)    .LOL Records
                              pack      NSEL2FLD,"2",NLOLLOL
                              move      "NSEL2KEY",Location
                              pack      KeyLocation,"Key: ",NSEL2FLD
                              call      NSEL2KEY
                              if not over
                                        move      NSEL2NAME,NLOLSELECT
                                        move      NSEL2QTY,statseluni
                              endif
                    else
                              move      NLOLUNIVERSE,statseluni
                    endif
.END PATCH 1.53 ADDED LOGIC
                    call      LoadStatsRecord using C0
          endif
          return

LoadStatsHeader
.START PATCH 1.1 REPLACED LOGIC
.         call      Trim using NPKGPNAME
.         setprop   sheets(SheetIndex),*Name=NPKGPNAME
          call      Trim using str31
          setprop   sheets(SheetIndex),*Name=str31
.END PATCH 1.1 REPLACED LOGIC
.....Worksheet Formatting.....
        setprop sheets(SheetIndex).PageSetup,*Orientation=xlLandscape
        setprop sheets(SheetIndex).PageSetup,*Zoom=Zoom70
          setprop   sheets(SheetIndex).Range("A1","IV1000").Font,*Size=10
.START PATCH 1.55.1 ADDED LOGIC
        setprop sheets(SheetIndex).PageSetup,*TopMargin=TopMargin
        setprop sheets(SheetIndex).PageSetup,*BottomMargin=BottomMargin
        setprop sheets(SheetIndex).PageSetup,*FooterMargin=TopMargin
.END PATCH 1.55.1 ADDED LOGIC
.....Header Information.....
.START PATCH 1.57 REPLACED LOGIC
.        pack    MKEY,NCMPMLR,"000"
.         move    "CreateCamp.-NMLRKEY",Location
.        pack    KeyLocation,"Key: ",MKEY
.         call    NMLRKEY
.        call    Trim using MCOMP
.         pack      taskname,"MAILER:   ",MCOMP
..................................
          pack    COMPFLD,NCMPMLR
          move    "CreateCamp.-COMPKEY",Location
          pack    KeyLocation,"Key: ",COMPFLD
          call    COMPKEY
          call    Trim using COMPCOMP
          pack      taskname,"MAILER:   ",COMPCOMP
.END PATCH 1.57 REPLACED LOGIC
.START PATCH 1.4 REPLACED LOGIC
.         setprop   sheets(SheetIndex).Range("A2"),*Value=taskname
.         setprop   sheets(SheetIndex).range("A2").Font,*Bold="True"
.         pack      taskname,"THRU:   ???"
.         setprop   sheets(SheetIndex).Range("A3"),*Value=taskname
.         pack      taskname,"MAILER##:   ",NCMPMLR
.         setprop   sheets(SheetIndex).Range("A5"),*Value=taskname
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
.         move      C0,N6
.         move      NXRFLIST,N6
.         move      N6,str6
.         call      Trim using str6
.         pack      taskname,"LIST##:   ",str6
.         setprop   sheets(SheetIndex).Range("A6"),*Value=taskname
.         move      UNIVERSE,str9
.         call      FormatNumeric using str9,str11
.         pack      taskname,"UNIVERSE:   ",str11
.         setprop   sheets(SheetIndex).Range("A8"),*Value=taskname
.         pack      taskname,"OFFER:   ???"
.         setprop   sheets(SheetIndex).Range("A9"),*Value=taskname
.         pack      taskname,"Statistics Thru:   ???"
.         setprop   sheets(SheetIndex).Range("A11"),*Value=taskname
..
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
.         pack      taskname,"MEDIA:   ",MEDIA
.         setprop   sheets(SheetIndex).Range("C5"),*Value=taskname
.        call    Trim using NCMPSHIP
.        clear   str55
.        if (NCMPSHIP <> "")
.                         move    C0,N2
.                         move    NCMPSHIP,N2
.                         add     C2,N2
.                         getitem ComboPtr,N2,str55
.        endif
.         pack      taskname,"SHIP VIA:   ",str55
.         setprop   sheets(SheetIndex).Range("C6"),*Value=taskname
.        move    NCMPSHIPTO,NRTNFLD
.        move    "CreateCamp.-NRTNKEY",Location
.        pack    KeyLocation,"Key: ",NRTNFLD
.        call    NRTNKEY
.        if over
.                clear   RTCOMP
.                clear   RTCNTCT
.        endif
.        call    Trim using RTCOMP
.         pack      taskname,"SHIP TO:   ",RTCOMP
.         setprop   sheets(SheetIndex).Range("C8"),*Value=taskname
.         pack      taskname,"ATTN:   ",RTCNTCT
.         setprop   sheets(SheetIndex).Range("C9"),*Value=taskname
.         pack      taskname,"MP/P.O.##:   ",NCMPPO
.         setprop   sheets(SheetIndex).Range("C12"),*Value=taskname
.         pack      taskname,"NOTE:   ???"
.         setprop   sheets(SheetIndex).Range("C13"),*Value=taskname
.         setprop   sheets(SheetIndex).range("C13").Font,*Bold="True",*Italic="True"
..
.        call    Trim using NCMPRDATE
.        if (NCMPRDATE <> "")
.                unpack  NCMPRDATE,str4,MM,DD
.                pack    str10,MM,SLASH,DD,SLASH,str4
.         else
.                   clear     str10
.        endif
.         pack      taskname,"Return Date:   ",str10
.         setprop   sheets(SheetIndex).Range("D5"),*Value=taskname
.        call    Trim using NCMPMDATE
.        if (NCMPMDATE <> "")
.                unpack  NCMPMDATE,str4,MM,DD
.                pack    str10,MM,SLASH,DD,SLASH,str4
.         else
.                   clear     str10
.        endif
.         pack      taskname,"Mail Date:   ",str10
.         setprop   sheets(SheetIndex).Range("D6"),*Value=taskname
.        move    C1,NCNTPATH
.        move    NCMPCNT,NCNTFLD
.        move    "CreateCamp.-NCNTKEY",Location
.        pack    KeyLocation,"Key: ",NCNTFLD
.        call    NCNTKEY
.        if over
.                clear   CNTNAME
.        endif
.        call    Trim using CNTNAME
.         pack      taskname,"NIN CONTACT:   ",CNTNAME
.         setprop   sheets(SheetIndex).Range("D8"),*Value=taskname
.        move    NCMPPLANNER,N9
.        move    osls0,str45
.        load    str45 from N9 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
.                osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
.                osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
.                   osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
.        call    Trim using str45
.         pack      taskname,"NIN SALES:   ",str45
.         setprop   sheets(SheetIndex).Range("D9"),*Value=taskname
..
.         sheets(SheetIndex).range("P4","T11").BorderAround using *LineStyle=1,*Weight=MedThick
.         setprop   sheets(SheetIndex).Range("P4"),*Value="Cost Assumptions"
.         setprop   sheets(SheetIndex).Range("P4").Font,*Bold="True"
.         setprop   sheets(SheetIndex).Range("P5"),*Value="Printing:"
.         setprop   sheets(SheetIndex).Range("P6"),*Value="Postage:"
.         setprop   sheets(SheetIndex).Range("P7"),*Value="Mailing:"
.         setprop   sheets(SheetIndex).Range("P8"),*Value="Merge:"
.         setprop   sheets(SheetIndex).Range("P9"),*Value="Other:"
.         setprop   sheets(SheetIndex).Range("P10"),*Value="Total:"
.         setprop   sheets(SheetIndex).Range("P11"),*Value="Premium:"
..START PATCH 1.2 ADDED LOGIC
.         sheets(SheetIndex).range("P13","Q13").BorderAround using *LineStyle=1,*Weight=MedThick
.         setprop   sheets(SheetIndex).Range("P13"),*Value="RR Adj:"
..END PATCH 1.2 ADDED LOGIC
.         move      C0,str8
.         move      C1,NPRCPATH
.         pack      NPRCFLD1,"01X",NCMPMLR
.         pack      NPRCFLD2,"02X",NPKGNUM
.         move      "Load.Stats-NPRCAIM",Location
.         pack      KeyLocation,"Key: ",NPRCFLD1,COMMA,NPRCFLD2
.         call      NPRCAIM
.         loop
.                   until over
.                   if (str8 < NPRCDATE)
.                             move      NPRCDATE,str8
.                   endif
.                   move      "Load.Stats-NPRCKG",Location
.                   call      NPRCKG
.         repeat
.         pack      NPRCFLD,NCMPMLR,NPKGNUM,str8
.         move      "Load.Stats-NPRCKEY",Location
.         pack      KeyLocation,"Key: ",NPRCFLD
.         call      NPRCKEY
.         setprop   sheets(SheetIndex).Range("Q5"),*Value=NPRCPrint,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.         setprop   sheets(SheetIndex).Range("Q6"),*Value=NPRCPost,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.         setprop   sheets(SheetIndex).Range("Q7"),*Value="1",*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.         setprop   sheets(SheetIndex).Range("Q8"),*Value="1",*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.         setprop   sheets(SheetIndex).Range("Q9"),*Value="1",*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.         setprop   sheets(SheetIndex).Range("Q10"),*Value=NPRCTotal,*HorizontalAlignment=AlignCenter,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.        sheets(SheetIndex).range("Q10").BorderAround using *LineStyle=1,*Weight=MedThick
.        setprop sheets(SheetIndex).range("Q10").Font,*ColorIndex=3   .Red
.         setprop   sheets(SheetIndex).range("Q10").Font,*Bold="True"
.         setprop   sheets(SheetIndex).Range("Q11"),*Value=NPRCPremium,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..START PATCH 1.2 ADDED LOGIC
.         call      Trim using NCMPRate
.         move      C0,N32
.         move      NCMPRate,N32
.         if (N32 = 0)
.                   move      "1",NCMPRATE
.         endif
.         setprop   sheets(SheetIndex).Range("Q13"),*Value=NCMPRate,*NumberFormat="0.00"
..END PATCH 1.2 ADDED LOGIC
.         setprop   sheets(SheetIndex).Range("R11"),*Value="0",*NumberFormat="0.00%"
.         setprop   sheets(SheetIndex).Range("S5"),*Value="Net:",*HorizontalAlignment=AlignRight
.         setprop   sheets(SheetIndex).Range("S6"),*Value="Gift:",*HorizontalAlignment=AlignRight
.         setprop   sheets(SheetIndex).Range("S7"),*Value="%RTND:",*HorizontalAlignment=AlignRight
.         setprop   sheets(SheetIndex).Range("S8"),*Value="List Cost:",*HorizontalAlignment=AlignRight
.         setprop   sheets(SheetIndex).Range("T5"),*Value="1"
.         setprop   sheets(SheetIndex).Range("T6"),*Value="1",*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.         setprop   sheets(SheetIndex).Range("T7"),*Value="1",*NumberFormat="0.00%"
.         setprop   sheets(SheetIndex).Range("T8"),*Value="1",*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.         move      "10",PackPtr(index,1)
.         move      "11",PackPtr(index,2)
......Record Header Information.....
.         move      "13",FirstRec(index)
.        move    FirstRec(index),str9
.         call    Trim using str9
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"Y",str9
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"Z",str9                          
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AA",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AB",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AC",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AD",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AE",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AF",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AG",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AH",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AI",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
.........................
..START PATCH 1.3 REPLACED LOGIC
..        pack    str4,"AC",str9
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AD",str9                          
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AE",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AF",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AG",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AH",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AI",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AJ",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AK",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AL",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
..        pack    str4,"AM",str9                         
..        setprop sheets(SheetIndex).range(str4),*Value="HIDE"
......................
.         pack    range,"AC",str9
.         setprop sheets(SheetIndex).range(range),*Value="HIDE"
.         pack    range,"AD",str9                          
.         setprop sheets(SheetIndex).range(range),*Value="HIDE"
.         pack    range,"AE",str9                         
.         setprop sheets(SheetIndex).range(range),*Value="HIDE"
.         pack    range,"AF",str9                         
.         setprop sheets(SheetIndex).range(range),*Value="HIDE"
.         pack    range,"AG",str9                         
.         setprop sheets(SheetIndex).range(range),*Value="HIDE"
.         pack    range,"AH",str9                         
.         setprop sheets(SheetIndex).range(range),*Value="HIDE"
.         pack    range,"AI",str9                         
.         setprop sheets(SheetIndex).range(range),*Value="HIDE"
.         pack    range,"AJ",str9                         
.         setprop sheets(SheetIndex).range(range),*Value="HIDE"
.         pack    range,"AK",str9                         
.         setprop sheets(SheetIndex).range(range),*Value="HIDE"
.         pack    range,"AL",str9                         
.         setprop sheets(SheetIndex).range(range),*Value="HIDE"
.         pack    range,"AM",str9                         
.         setprop sheets(SheetIndex).range(range),*Value="HIDE"
..END PATCH 1.3 REPLACED LOGIC
..END PATCH 1.2 REPLACED LOGIC
.         move      "15",FirstRec(index)
.
...........................
.START PATCH 1.55 REPLACED LOGIC
.        setprop sheets(SheetIndex).range("A1"),*Value="Names in the News",*HorizontalAlignment=AlignCenter
.        setprop sheets(SheetIndex).range("A1").Font,*Name="Times New Roman",*Underline=xlUnderlineStyleSingle,*Size=20
.        setprop sheets(SheetIndex).range("A1").Characters(1,5).Font,*Bold="True"
.        setprop sheets(SheetIndex).range("A1").Characters(7,11).Font,*Italic="True"
.        sheets(SheetIndex).range("A1:C1").Merge
.        setprop sheets(SheetIndex).range("A2"),*Value="C  A  L  I  F  O  R  N  I  A       I  N  C .",*HorizontalAlignment=AlignCenter
.        setprop sheets(SheetIndex).range("A2").Font,*Name="Times New Roman",*Size=10
.        sheets(SheetIndex).range("A2:C2").Merge


.START PATCH 1.59 REPLACED LOGIC
.         setprop   sheets(SheetIndex).range("A1:A1").Rows,*RowHeight=xlRowHeight
.         sheets(SheetIndex).range("A1:E1").Merge
.         sheets(SheetIndex).Shapes.AddPicture using "\\nts0\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
          if (StrPtr2 = "P")
                    clear taskname
                    setprop   sheets(SheetIndex).range("A1").Rows,*RowHeight=xlRowHeight
                    setprop sheets(SheetIndex).range("A1"),*HorizontalAlignment=AlignCenter
                    setprop sheets(SheetIndex).range("A1").Font,*Name="Times New Roman",*Size=10,*Bold="True"
                    append    "Pacific Lists, Inc.",taskname
                    append    Sreturn,taskname
                    append    "180 Grand Ave. Suite 1365",taskname
                    append    Sreturn,taskname
                    append    "Oakland, CA 94612-3716",taskname
                    append    Sreturn,taskname
                    append    "415-945-9450 · Fax 415-945-9451",taskname
                    append    Sreturn,taskname
                    append    "A Division of Names in the News",taskname
                    reset     taskname
                    setprop   sheets(SheetIndex).range("A1"),*Value=taskname
                    setprop sheets(SheetIndex).range("A1").Characters(1,19).Font,*Size=18
                    sheets(SheetIndex).range("A1:D1").Merge
                    //refresh the data used in next section
                    pack      taskname,"MAILER:   ",COMPCOMP
          else
                    setprop   sheets(SheetIndex).range("A1:A1").Rows,*RowHeight=xlRowHeight
                    sheets(SheetIndex).range("A1:E1").Merge
                    sheets(SheetIndex).Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,240,75
          endif
.END PATCH 1.59 REPLACED LOGIC
.END PATCH 1.55 REPLACED LOGIC
.
          setprop   sheets(SheetIndex).Range("A4"),*Value=taskname
          setprop   sheets(SheetIndex).range("A4").Font,*Bold="True"
          pack      taskname,"THRU:   ???"
          setprop   sheets(SheetIndex).Range("A5"),*Value=taskname
          pack      taskname,"MAILER##:   ",NCMPMLR
          setprop   sheets(SheetIndex).Range("A7"),*Value=taskname
        move    C0,UNIVERSE
        clear   NXRFFLD2
        clear   NXRFLIST
.START PATCH 1.57 REPLACED LOGIC - TEMPORARY LOGIC
.        move    MKEY,NXRFFLD2
        move    COMPOLDMLR,NXRFFLD2
.END PATCH 1.57 REPLACED LOGIC - TEMPORARY LOGIC
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
          pack      taskname,"LIST##:   ",str6
          setprop   sheets(SheetIndex).Range("A8"),*Value=taskname
.START PATCH 1.53 REPLACED LOGIC
.         move      UNIVERSE,str9
.         call      FormatNumeric using str9,str11
.         pack      taskname,"UNIVERSE:   ",str11
.         setprop   sheets(SheetIndex).Range("A10"),*Value=taskname
          move      UNIVERSE,str10
          call      FormatNumeric using str10,str13
          pack      taskname,"UNIVERSE:   ",str13
          setprop   sheets(SheetIndex).Range("A10"),*Value=taskname
.END PATCH 1.53 REPLACED LOGIC
          pack      taskname,"OFFER:   ???"
          setprop   sheets(SheetIndex).Range("A11"),*Value=taskname
          pack      taskname,"Statistics Thru:   ???"
          setprop   sheets(SheetIndex).Range("A13"),*Value=taskname
.
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
          pack      taskname,"MEDIA:   ",MEDIA
          setprop   sheets(SheetIndex).Range("C7"),*Value=taskname
        call    Trim using NCMPSHIP
        clear   str55
        if (NCMPSHIP <> "")
                          move    C0,N2
                          move    NCMPSHIP,N2
                          add     C2,N2
                          getitem ComboPtr,N2,str55
        endif
          pack      taskname,"SHIP VIA:   ",str55
          setprop   sheets(SheetIndex).Range("C8"),*Value=taskname
        move    NCMPSHIPTO,NRTNFLD
        move    "CreateCamp.-NRTNKEY",Location
        pack    KeyLocation,"Key: ",NRTNFLD
        call    NRTNKEY
        if over
                clear   RTCOMP
                clear   RTCNTCT
        endif
        call    Trim using RTCOMP
          pack      taskname,"SHIP TO:   ",RTCOMP
          setprop   sheets(SheetIndex).Range("C10"),*Value=taskname
          pack      taskname,"ATTN:   ",RTCNTCT
          setprop   sheets(SheetIndex).Range("C11"),*Value=taskname
          pack      taskname,"MP/P.O.##:   ",NCMPPO
          setprop   sheets(SheetIndex).Range("C14"),*Value=taskname
          pack      taskname,"NOTE:   ???"
          setprop   sheets(SheetIndex).Range("C15"),*Value=taskname
          setprop   sheets(SheetIndex).range("C15").Font,*Bold="True",*Italic="True"
.
        call    Trim using NCMPRDATE
        if (NCMPRDATE <> "")
                unpack  NCMPRDATE,str4,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,str4
          else
                    clear     str10
        endif
          pack      taskname,"Return Date:   ",str10
          setprop   sheets(SheetIndex).Range("D7"),*Value=taskname
        call    Trim using NCMPMDATE
        if (NCMPMDATE <> "")
                unpack  NCMPMDATE,str4,MM,DD
                pack    str10,MM,SLASH,DD,SLASH,str4
          else
                    clear     str10
        endif
          pack      taskname,"Mail Date:   ",str10
          setprop   sheets(SheetIndex).Range("D8"),*Value=taskname
        move    C1,NCNTPATH
        move    NCMPCNT,NCNTFLD
        move    "CreateCamp.-NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD
        call    NCNTKEY
        if over
                clear   CNTNAME
        endif
        call    Trim using CNTNAME
          pack      taskname,"NIN CONTACT:   ",CNTNAME
          setprop   sheets(SheetIndex).Range("D10"),*Value=taskname
        move    NCMPPLANNER,N9
        move    osls0,str45
        load    str45 from N9 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
                osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
                osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
        call    Trim using str45
          pack      taskname,"NIN SALES:   ",str45
          setprop   sheets(SheetIndex).Range("D11"),*Value=taskname
.
          sheets(SheetIndex).range("P6","T13").BorderAround using *LineStyle=1,*Weight=MedThick
          setprop   sheets(SheetIndex).Range("P6"),*Value="Cost Assumptions"
          setprop   sheets(SheetIndex).Range("P6").Font,*Bold="True"
          setprop   sheets(SheetIndex).Range("P7"),*Value="Printing:"
          setprop   sheets(SheetIndex).Range("P8"),*Value="Postage:"
          setprop   sheets(SheetIndex).Range("P9"),*Value="Mailing:"
          setprop   sheets(SheetIndex).Range("P10"),*Value="Merge:"
          setprop   sheets(SheetIndex).Range("P11"),*Value="Other:"
          setprop   sheets(SheetIndex).Range("P12"),*Value="Total:"
          setprop   sheets(SheetIndex).Range("P13"),*Value="Premium:"
.START PATCH 1.52.2 REPLACED LOGIC
.         sheets(SheetIndex).range("P15","Q15").BorderAround using *LineStyle=1,*Weight=MedThick
          sheets(SheetIndex).range("P15","T15").BorderAround using *LineStyle=1,*Weight=MedThick
.END PATCH 1.52.2 REPLACED LOGIC
          setprop   sheets(SheetIndex).Range("P15"),*Value="RR Adj:"
.START PATCH 1.52.2 ADDED LOGIC
          setprop   sheets(SheetIndex).Range("S15"),*Value="AG Adj:"
          call      Trim using NCMPGift
          move      C0,N32
          move      NCMPGift,N32
          if (N32 = 0)
                    move      "1",NCMPGift
          endif
          setprop sheets(SheetIndex).range("T15"),*Value=NCMPGift,*NumberFormat="0.00"
.END PATCH 1.52.2 ADDED LOGIC
          move      C0,str8
          move      C1,NPRCPATH
.START PATCH 1.57 REPLACED LOGIC
..START PATCH 1.56 REPLACED LOGIC
..        pack      NPRCFLD1,"01X",NCMPMLR
.         move      "Load.Stats-COMPKEY3",Location
.         pack      COMPFLD3,NCMPMLR
.         pack      KeyLocation,"Key: ",COMPFLD3
.         call      COMPKEY3
.         pack      NPRCFLD1,"01X",COMPNUM
..END PATCH 1.56 REPLACED LOGIC
          pack      NPRCFLD1,"01X",NCMPMLR
.END PATCH 1.57 REPLACED LOGIC
          pack      NPRCFLD2,"02X",NPKGNUM
          move      "Load.Stats-NPRCAIM",Location
          pack      KeyLocation,"Key: ",NPRCFLD1,COMMA,NPRCFLD2
          call      NPRCAIM
          loop
                    until over
                    if (str8 < NPRCDATE)
                              move      NPRCDATE,str8
                    endif
                    move      "Load.Stats-NPRCKG",Location
                    call      NPRCKG
          repeat
.START PATCH 1.57 REPLACED LOGIC
..START PATCH 1.56 REPLACED LOGIC
..        pack      NPRCFLD,NCMPMLR,NPKGNUM,str8
.         pack      NPRCFLD,COMPNUM,NPKGNUM,str8
..EMD PATCH 1.56 REPLACED LOGIC
          pack      NPRCFLD,NCMPMLR,NPKGNUM,str8
.END PATCH 1.57 REPLACED LOGIC
          move      "Load.Stats-NPRCKEY",Location
          pack      KeyLocation,"Key: ",NPRCFLD
          call      NPRCKEY
          setprop   sheets(SheetIndex).Range("Q7"),*Value=NPRCPrint,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop   sheets(SheetIndex).Range("Q8"),*Value=NPRCPost,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop   sheets(SheetIndex).Range("Q9"),*Value="1",*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop   sheets(SheetIndex).Range("Q10"),*Value="1",*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop   sheets(SheetIndex).Range("Q11"),*Value="1",*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop   sheets(SheetIndex).Range("Q12"),*Value=NPRCTotal,*HorizontalAlignment=AlignCenter,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
        sheets(SheetIndex).range("Q12").BorderAround using *LineStyle=1,*Weight=MedThick
        setprop sheets(SheetIndex).range("Q12").Font,*ColorIndex=3    .Red
          setprop   sheets(SheetIndex).range("Q12").Font,*Bold="True"
          setprop   sheets(SheetIndex).Range("Q13"),*Value=NPRCPremium,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          call      Trim using NCMPRate
          move      C0,N32
          move      NCMPRate,N32
          if (N32 = 0)
                    move      "1",NCMPRATE
          endif
          setprop   sheets(SheetIndex).Range("Q15"),*Value=NCMPRate,*NumberFormat="0.00"
          setprop   sheets(SheetIndex).Range("R13"),*Value="0",*NumberFormat="0.00%"
          setprop   sheets(SheetIndex).Range("S7"),*Value="Net:",*HorizontalAlignment=AlignRight
          setprop   sheets(SheetIndex).Range("S8"),*Value="Gift:",*HorizontalAlignment=AlignRight
          setprop   sheets(SheetIndex).Range("S9"),*Value="%RTND:",*HorizontalAlignment=AlignRight
          setprop   sheets(SheetIndex).Range("S10"),*Value="List Cost:",*HorizontalAlignment=AlignRight
          setprop   sheets(SheetIndex).Range("T7"),*Value="1"
          setprop   sheets(SheetIndex).Range("T8"),*Value="1",*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop   sheets(SheetIndex).Range("T9"),*Value="1",*NumberFormat="0.00%"
          setprop   sheets(SheetIndex).Range("T10"),*Value="1",*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          move      "12",PackPtr(index,1)
          move      "13",PackPtr(index,2)
.....Record Header Information.....
          move      "15",FirstRec(index)
        move    FirstRec(index),str9
          call    Trim using str9
          pack    range,"AC",str9
          setprop sheets(SheetIndex).range(range),*Value="HIDE"
          pack    range,"AD",str9                          
          setprop sheets(SheetIndex).range(range),*Value="HIDE"
          pack    range,"AE",str9                         
          setprop sheets(SheetIndex).range(range),*Value="HIDE"
          pack    range,"AF",str9                         
          setprop sheets(SheetIndex).range(range),*Value="HIDE"
          pack    range,"AG",str9                         
          setprop sheets(SheetIndex).range(range),*Value="HIDE"
          pack    range,"AH",str9                         
          setprop sheets(SheetIndex).range(range),*Value="HIDE"
          pack    range,"AI",str9                         
          setprop sheets(SheetIndex).range(range),*Value="HIDE"
          pack    range,"AJ",str9                         
          setprop sheets(SheetIndex).range(range),*Value="HIDE"
          pack    range,"AK",str9                         
          setprop sheets(SheetIndex).range(range),*Value="HIDE"
          pack    range,"AL",str9                         
          setprop sheets(SheetIndex).range(range),*Value="HIDE"
          pack    range,"AM",str9                         
          setprop sheets(SheetIndex).range(range),*Value="HIDE"
          move      "17",FirstRec(index)
.END PATCH 1.4 REPLACED LOGIC
          move      FirstRec(index),hdrrow(index)
        move    FirstRec(index),str9
          call    Trim using str9
          call      LoadStatsSubHeader using str9
.Format Whole Header
.START PATCH 1.4 REPLACED LOGIC
.         move      "15",hdrrow(index)  .testing purposes
          move      "17",hdrrow(index)  .testing purposes
.END PATCH 1.4 REPLACED LOGIC
          sub       C1,hdrrow(index),N10
          move      N10,str10
          call      Trim using str10
.START PATCH 1.3 REPLACED LOGIC
.         pack    str4,"A1"
.         pack    str5,"A",str10
.         sheets(SheetIndex).range(str4,str5).Columns.Autofit
.         pack    str4,"B1"
.         pack    str5,"B",str10
.         sheets(SheetIndex).range(str4,str5).Columns.Autofit
.         pack    str4,"C1"
.         pack    str5,"C",str10
.         sheets(SheetIndex).range(str4,str5).Columns.Autofit
.         pack    str4,"D1"
.         pack    str5,"D",str10
.         sheets(SheetIndex).range(str4,str5).Columns.Autofit
.         pack    str4,"P5"
.         pack    str5,"P",str10
.         sheets(SheetIndex).range(str4,str5).Columns.Autofit
.         pack    str4,"Q1"
.         pack    str5,"Q",str10
.         sheets(SheetIndex).range(str4,str5).Columns.Autofit
.         pack    str4,"R5"
.         pack    str5,"R",str10
.         sheets(SheetIndex).range(str4,str5).Columns.Autofit
.         pack    str4,"S5"
.         pack    str5,"S",str10
.         sheets(SheetIndex).range(str4,str5).Columns.Autofit
.         pack    str4,"T1"
.         pack    str5,"T",str10
.         sheets(SheetIndex).range(str4,str5).Columns.Autofit
..
.         add       C1,FirstRec(index)
.         move      FirstRec(index),str9
.         call      Trim using str9
.         pack      str4,"A",str9
..START PATCH 1.2 REPLACED LOGIC
..        pack      str5,"AM",str9
.         pack      str5,"AT",str9
..END PATCH 1.2 REPLACED LOGIC
.         sheets(SheetIndex).range(str4,str5).BorderAround using *LineStyle=1,*Weight=2
..........................
          pack    range,"A1"
          pack    range2,"A",str10
          sheets(SheetIndex).range(range,range2).Columns.Autofit
          pack    range,"B1"
          pack    range2,"B",str10
          sheets(SheetIndex).range(range,range2).Columns.Autofit
          pack    range,"C1"
          pack    range2,"C",str10
          sheets(SheetIndex).range(range,range2).Columns.Autofit
          pack    range,"D1"
          pack    range2,"D",str10
          sheets(SheetIndex).range(range,range2).Columns.Autofit
          pack    range,"P5"
          pack    range2,"P",str10
          sheets(SheetIndex).range(range,range2).Columns.Autofit
          pack    range,"Q1"
          pack    range2,"Q",str10
          sheets(SheetIndex).range(range,range2).Columns.Autofit
          pack    range,"R5"
          pack    range2,"R",str10
          sheets(SheetIndex).range(range,range2).Columns.Autofit
          pack    range,"S5"
          pack    range2,"S",str10
          sheets(SheetIndex).range(range,range2).Columns.Autofit
          pack    range,"T1"
          pack    range2,"T",str10
          sheets(SheetIndex).range(range,range2).Columns.Autofit
.
          add       C1,FirstRec(index)
          move      FirstRec(index),str9
          call      Trim using str9
          pack      range,"A",str9
          pack      range2,"AT",str9
          sheets(SheetIndex).range(range,range2).BorderAround using *LineStyle=1,*Weight=2
.END PATCH 1.3 REPLACED LOGIC
          move      FirstRec(index),LastRec(index)
          add       C1,FirstRec(index)
          return

LoadStatsSubHeader Routine DimPtr
          move      DimPtr,str9
.START PATCH 1.3 REPLACED LOGIC
.         pack    str4,"A",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="List ##",*HorizontalAlignment=AlignCenter
.         pack    str4,"B",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="List",*HorizontalAlignment=AlignCenter
.         pack    str4,"C",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Select",*HorizontalAlignment=AlignCenter
.         pack    str4,"D",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Universe",*HorizontalAlignment=AlignCenter
.         pack    str4,"E",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Reco Qty",*HorizontalAlignment=AlignCenter
.         pack    str4,"F",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Qty Aprvd",*HorizontalAlignment=AlignCenter
.         pack    str4,"G",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="R/E",*HorizontalAlignment=AlignCenter
.         pack    str4,"H",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="List Cost",*HorizontalAlignment=AlignCenter
.         pack    str4,"I",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Avg $/m",*HorizontalAlignment=AlignCenter
.         pack    str4,"J",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Mail Date",*HorizontalAlignment=AlignCenter
.         pack    str4,"K",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Exch Stat",*HorizontalAlignment=AlignCenter
.         pack    str4,"L",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Net Req",*HorizontalAlignment=AlignCenter
.         pack    str4,"M",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Net Appr",*HorizontalAlignment=AlignCenter
.         pack    str4,"N",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Aver Net",*HorizontalAlignment=AlignCenter
.         pack    str4,"O",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Net Names",*HorizontalAlignment=AlignCenter
.         pack    str4,"P",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Resp R",*HorizontalAlignment=AlignCenter
..........................
          pack    range,"A",str9
          setprop   sheets(SheetIndex).range(range),*Value="List ##",*HorizontalAlignment=AlignCenter
          pack    range,"B",str9
          setprop   sheets(SheetIndex).range(range),*Value="List",*HorizontalAlignment=AlignCenter
          pack    range,"C",str9
          setprop   sheets(SheetIndex).range(range),*Value="Select",*HorizontalAlignment=AlignCenter
          pack    range,"D",str9
          setprop   sheets(SheetIndex).range(range),*Value="Universe",*HorizontalAlignment=AlignCenter
          pack    range,"E",str9
          setprop   sheets(SheetIndex).range(range),*Value="Reco Qty",*HorizontalAlignment=AlignCenter
          pack    range,"F",str9
          setprop   sheets(SheetIndex).range(range),*Value="Qty Aprvd",*HorizontalAlignment=AlignCenter
          pack    range,"G",str9
          setprop   sheets(SheetIndex).range(range),*Value="R/E",*HorizontalAlignment=AlignCenter
          pack    range,"H",str9
          setprop   sheets(SheetIndex).range(range),*Value="List Cost",*HorizontalAlignment=AlignCenter
          pack    range,"I",str9
          setprop   sheets(SheetIndex).range(range),*Value="Avg $/m",*HorizontalAlignment=AlignCenter
          pack    range,"J",str9
          setprop   sheets(SheetIndex).range(range),*Value="Mail Date",*HorizontalAlignment=AlignCenter
          pack    range,"K",str9
          setprop   sheets(SheetIndex).range(range),*Value="Exch Stat",*HorizontalAlignment=AlignCenter
          pack    range,"L",str9
          setprop   sheets(SheetIndex).range(range),*Value="Net Req",*HorizontalAlignment=AlignCenter
          pack    range,"M",str9
          setprop   sheets(SheetIndex).range(range),*Value="Net Appr",*HorizontalAlignment=AlignCenter
          pack    range,"N",str9
          setprop   sheets(SheetIndex).range(range),*Value="Aver Net",*HorizontalAlignment=AlignCenter
          pack    range,"O",str9
          setprop   sheets(SheetIndex).range(range),*Value="Net Names",*HorizontalAlignment=AlignCenter
          pack    range,"P",str9
          setprop   sheets(SheetIndex).range(range),*Value="Resp R",*HorizontalAlignment=AlignCenter
.END PATCH 1.3 REPLACED LOGIC
.START PATCH 1.2 REPLACED LOGIC
.        pack    str4,"Q",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Rtns",*HorizontalAlignment=AlignCenter
.        pack    str4,"R",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Gift",*HorizontalAlignment=AlignCenter
.        pack    str4,"S",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Revenue",*HorizontalAlignment=AlignCenter
.        pack    str4,"T",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Prod Cost",*HorizontalAlignment=AlignCenter
.        pack    str4,"U",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Lst Cost",*HorizontalAlignment=AlignCenter
.        pack    str4,"V",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Tot Cost",*HorizontalAlignment=AlignCenter
.        pack    str4,"W",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Net +/-",*HorizontalAlignment=AlignCenter
.        pack    str4,"X",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Cost Mbr",*HorizontalAlignment=AlignCenter
.        pack    str4,"Y",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Inc M",*HorizontalAlignment=AlignCenter
.        pack    str4,"Z",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Cost/M",*HorizontalAlignment=AlignCenter
.        pack    str4,"AA",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Cost?$",*HorizontalAlignment=AlignCenter
.        pack    str4,"AB",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="C Vol",*HorizontalAlignment=AlignCenter
.        pack    str4,"AC",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="C Resp",*HorizontalAlignment=AlignCenter
.        pack    str4,"AD",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="C RR",*HorizontalAlignment=AlignCenter
.        pack    str4,"AE",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="C Rev",*HorizontalAlignment=AlignCenter
.        pack    str4,"AF",str9
.        setprop sheets(SheetIndex).range(str4),*Value="C Gift",*HorizontalAlignment=AlignCenter
.        pack    str4,"AG",str9
.        setprop sheets(SheetIndex).range(str4),*Value="C Cost",*HorizontalAlignment=AlignCenter
.        pack    str4,"AH",str9
.        setprop sheets(SheetIndex).range(str4),*Value="C Net",*HorizontalAlignment=AlignCenter
.        pack    str4,"AI",str9
.        setprop sheets(SheetIndex).range(str4),*Value="C CPA",*HorizontalAlignment=AlignCenter
.        pack    str4,"AJ",str9
.        setprop sheets(SheetIndex).range(str4),*Value="Lst Upd",*HorizontalAlignment=AlignCenter
.        pack    str4,"AK",str9
.        setprop sheets(SheetIndex).range(str4),*Value="Usage of NWF",*HorizontalAlignment=AlignCenter
.        pack    str4,"AM",str9
.        setprop sheets(SheetIndex).range(str4),*Value="Comments",*HorizontalAlignment=AlignCenter
.        pack    str4,"AN",str9
.        setprop sheets(SheetIndex).range(str4),*Value="NIN##",*HorizontalAlignment=AlignCenter
..Format Record Header
.        pack    str4,"A",str9
.        pack    str5,"AM",str9
.        sheets(SheetIndex).range(str4,str5).BorderAround using *LineStyle=1,*Weight=2
.        pack    str5,"AK",str9
.        setprop sheets(SheetIndex).range(str4,str5).Font,*Color=colornum
.......................
.START PATCH 1.3 REPLACED LOGIC
.         pack    str4,"Q",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Adj. RR",*HorizontalAlignment=AlignCenter
.         pack    str4,"R",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Final RR",*HorizontalAlignment=AlignCenter
.         pack    str4,"S",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Rtns",*HorizontalAlignment=AlignCenter
.         pack    str4,"T",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Gift",*HorizontalAlignment=AlignCenter
.         pack    str4,"U",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Adj. Gift",*HorizontalAlignment=AlignCenter
.         pack    str4,"V",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Final Gift",*HorizontalAlignment=AlignCenter
.         pack    str4,"W",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Revenue",*HorizontalAlignment=AlignCenter
.         pack    str4,"X",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Prod Cost",*HorizontalAlignment=AlignCenter
.         pack    str4,"Y",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Lst Cost",*HorizontalAlignment=AlignCenter
.         pack    str4,"Z",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Tot Cost",*HorizontalAlignment=AlignCenter
.         pack    str4,"AA",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Net +/-",*HorizontalAlignment=AlignCenter
.         pack    str4,"AB",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Cost Mbr",*HorizontalAlignment=AlignCenter
.         pack    str4,"AC",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Inc M",*HorizontalAlignment=AlignCenter
.         pack    str4,"AD",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Cost/M",*HorizontalAlignment=AlignCenter
.         pack    str4,"AE",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="Cost?$",*HorizontalAlignment=AlignCenter
.         pack    str4,"AF",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="C Vol",*HorizontalAlignment=AlignCenter
.         pack    str4,"AG",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="C Resp",*HorizontalAlignment=AlignCenter
.         pack    str4,"AH",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="C RR",*HorizontalAlignment=AlignCenter
.         pack    str4,"AI",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="C Rev",*HorizontalAlignment=AlignCenter
.         pack    str4,"AJ",str9
.         setprop sheets(SheetIndex).range(str4),*Value="C Gift",*HorizontalAlignment=AlignCenter
.         pack    str4,"AK",str9
.         setprop sheets(SheetIndex).range(str4),*Value="C Cost",*HorizontalAlignment=AlignCenter
.         pack    str4,"AL",str9
.         setprop sheets(SheetIndex).range(str4),*Value="C Net",*HorizontalAlignment=AlignCenter
.         pack    str4,"AM",str9
.         setprop sheets(SheetIndex).range(str4),*Value="C CPA",*HorizontalAlignment=AlignCenter
.         pack    str4,"AN",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="CPD (CPD+100)",*HorizontalAlignment=AlignCenter
.         pack    str4,"AO",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="MLG CPD",*HorizontalAlignment=AlignCenter
.         pack    str4,"AP",str9
.         setprop   sheets(SheetIndex).range(str4),*Value="CPD Index",*HorizontalAlignment=AlignCenter
.         pack    str4,"AQ",str9
.         setprop sheets(SheetIndex).range(str4),*Value="Lst Upd",*HorizontalAlignment=AlignCenter
.         pack    str4,"AR",str9
.         setprop sheets(SheetIndex).range(str4),*Value="Usage of NWF",*HorizontalAlignment=AlignCenter
.         pack    str4,"AT",str9
.         setprop sheets(SheetIndex).range(str4),*Value="Comments",*HorizontalAlignment=AlignCenter
.         pack    str4,"AU",str9
.         setprop sheets(SheetIndex).range(str4),*Value="NIN##",*HorizontalAlignment=AlignCenter
..Format Record Header
.         pack    str4,"A",str9
.         pack    str5,"AT",str9
.         sheets(SheetIndex).range(str4,str5).BorderAround using *LineStyle=1,*Weight=2
.         pack    str5,"AR",str9
.         setprop sheets(SheetIndex).range(str4,str5).Font,*Color=colornum
......................
          pack    range,"Q",str9
          setprop   sheets(SheetIndex).range(range),*Value="Adj. RR",*HorizontalAlignment=AlignCenter
          pack    range,"R",str9
          setprop   sheets(SheetIndex).range(range),*Value="Final RR",*HorizontalAlignment=AlignCenter
          pack    range,"S",str9
          setprop   sheets(SheetIndex).range(range),*Value="Rtns",*HorizontalAlignment=AlignCenter
          pack    range,"T",str9
          setprop   sheets(SheetIndex).range(range),*Value="Gift",*HorizontalAlignment=AlignCenter
          pack    range,"U",str9
          setprop   sheets(SheetIndex).range(range),*Value="Adj. Gift",*HorizontalAlignment=AlignCenter
          pack    range,"V",str9
          setprop   sheets(SheetIndex).range(range),*Value="Final Gift",*HorizontalAlignment=AlignCenter
          pack    range,"W",str9
          setprop   sheets(SheetIndex).range(range),*Value="Revenue",*HorizontalAlignment=AlignCenter
          pack    range,"X",str9
          setprop   sheets(SheetIndex).range(range),*Value="Prod Cost",*HorizontalAlignment=AlignCenter
          pack    range,"Y",str9
          setprop   sheets(SheetIndex).range(range),*Value="Lst Cost",*HorizontalAlignment=AlignCenter
          pack    range,"Z",str9
          setprop   sheets(SheetIndex).range(range),*Value="Tot Cost",*HorizontalAlignment=AlignCenter
          pack    range,"AA",str9
          setprop   sheets(SheetIndex).range(range),*Value="Net +/-",*HorizontalAlignment=AlignCenter
          pack    range,"AB",str9
          setprop   sheets(SheetIndex).range(range),*Value="Cost Mbr",*HorizontalAlignment=AlignCenter
          pack    range,"AC",str9
          setprop   sheets(SheetIndex).range(range),*Value="Inc M",*HorizontalAlignment=AlignCenter
          pack    range,"AD",str9
          setprop   sheets(SheetIndex).range(range),*Value="Cost/M",*HorizontalAlignment=AlignCenter
          pack    range,"AE",str9
          setprop   sheets(SheetIndex).range(range),*Value="Cost?$",*HorizontalAlignment=AlignCenter
          pack    range,"AF",str9
          setprop   sheets(SheetIndex).range(range),*Value="C Vol",*HorizontalAlignment=AlignCenter
          pack    range,"AG",str9
          setprop   sheets(SheetIndex).range(range),*Value="C Resp",*HorizontalAlignment=AlignCenter
          pack    range,"AH",str9
          setprop   sheets(SheetIndex).range(range),*Value="C RR",*HorizontalAlignment=AlignCenter
          pack    range,"AI",str9
          setprop   sheets(SheetIndex).range(range),*Value="C Rev",*HorizontalAlignment=AlignCenter
          pack    range,"AJ",str9
          setprop sheets(SheetIndex).range(range),*Value="C Gift",*HorizontalAlignment=AlignCenter
          pack    range,"AK",str9
          setprop sheets(SheetIndex).range(range),*Value="C Cost",*HorizontalAlignment=AlignCenter
          pack    range,"AL",str9
          setprop sheets(SheetIndex).range(range),*Value="C Net",*HorizontalAlignment=AlignCenter
          pack    range,"AM",str9
          setprop sheets(SheetIndex).range(range),*Value="C CPA",*HorizontalAlignment=AlignCenter
          pack    range,"AN",str9
          setprop   sheets(SheetIndex).range(range),*Value="CPD (CPD+100)",*HorizontalAlignment=AlignCenter
          pack    range,"AO",str9
          setprop   sheets(SheetIndex).range(range),*Value="MLG CPD",*HorizontalAlignment=AlignCenter
          pack    range,"AP",str9
          setprop   sheets(SheetIndex).range(range),*Value="CPD Index",*HorizontalAlignment=AlignCenter
          pack    range,"AQ",str9
          setprop sheets(SheetIndex).range(range),*Value="Lst Upd",*HorizontalAlignment=AlignCenter
          pack    range,"AR",str9
          setprop sheets(SheetIndex).range(range),*Value="Usage of NWF",*HorizontalAlignment=AlignCenter
          pack    range,"AT",str9
          setprop sheets(SheetIndex).range(range),*Value="Comments",*HorizontalAlignment=AlignCenter
          pack    range,"AU",str9
          setprop sheets(SheetIndex).range(range),*Value="NIN##",*HorizontalAlignment=AlignCenter
.Format Record Header
          pack    range,"A",str9
          pack    range2,"AT",str9
          sheets(SheetIndex).range(range,range2).BorderAround using *LineStyle=1,*Weight=2
          pack    range2,"AR",str9
          setprop sheets(SheetIndex).range(range,range2).Font,*Color=colornum
.END PATCH 1.3 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
          return

LoadStatsRecord Routine FrmPtr2
          add       C1,LastRec(index)
          move      LastRec(index),str9
          call      Trim using str9
.START PATCH 1.3 REPLACED LOGIC
..LIST
.        pack    str4,"A",str9
.        setprop sheets(SheetIndex).range(str4),*Value=NLOLLIST
..LIST NAME
.         pack    str4,"B",str9
.         move    C1,NDATPATH
.         move    NLOLLIST,NDATFLD
.         move    "LoadDetail-NDATKEY",Location
.         pack    KeyLocation,"Key: ",NDATFLD
.         call    NDATKEY
.         if not over
.                   call    Trim using OLSTNAME
.                   setprop sheets(SheetIndex).range(str4),*Value=OLSTNAME
.         endif
..SELECT
.        pack    str4,"C",str9
.        call    Trim using NLOLSELECT
.        setprop sheets(SheetIndex).range(str4),*Value=NLOLSELECT
..UNIVERSE
.        pack    str4,"D",str9
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="##,####0_);(##,####0)"
..START PATCH 1.1 REPLACED LOGIC
..        type    NLOLUNIVERSE
..        if equal
..                if (NLOLUNIVERSE <> "000000000")
..                            call      Trim using NLOLUNIVERSE
..                        setprop sheets(SheetIndex).range(str4),*Value=NLOLUNIVERSE
..                endif
..        endif
.         if (statseluni > C0)
.                   setprop sheets(SheetIndex).range(str4),*Value=statseluni
.         endif
..END PATCH 1.1 REPLACED LOGIC
..RECO QTY
.        pack    str4,"E",str9
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="##,####0_);(##,####0)"
..RECEIVED QTY
.        pack    str4,"F",str9
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="##,####0_);(##,####0)"
..R/E
.        pack    str4,"G",str9
.         if (NLOLRENT = "1")
.                   move    "E",str1
.         elseif (NLOLRENT = "2")
.                   move    "R",str1
.         elseif (NLOLRENT = "3")
.                   move    "S",str1
.         else
.                   clear     str1
.         endif
.        if (OSTAT = "" | OSTAT = "z" | (OSTAT = "l" & OHIST <> "E"))
..Clear this variable if LOL Or Cancelled/Denied - Later logic will determine proper display
..Clear this variable if LCR not Cleared.
.                clear    str1
.        else
.                if (OCLRSTAT = "1")
.                        move    "E",str1
.                elseif (OCLRSTAT = "2")
.                        move    "R",str1
.                elseif (OCLRSTAT = "3")
.                        move    "S",str1
.                else
..Do nothing.  Live Orders should have previous instance of str1 used.
..                        clear   str1
.                endif
.        endif
.        setprop sheets(SheetIndex).range(str4),*Value=str1,*HorizontalAlignment=AlignCenter
..LIST COST PER/M
.        pack    str4,"H",str9
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..AVG ?/M
.        pack    str4,"I",str9
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..MAIL DATE
.        pack    str4,"J",str9
.         call      Trim using NLOLMDATE
.         if (NLOLMDATE <> "")
.                   unpack    NLOLMDATE,str2,YY,MM,DD
.                   pack      str10,MM,SLASH,DD,SLASH,str2,YY
.         else
.                   clear     str10
.         endif
.        setprop sheets(SheetIndex).range(str4),*Value=str10
..XSTAT
.        pack    str4,"K",str9
.        move    NCMPMLR,OMLRNUM
.        move    NLOLLIST,OLNUM
.        call    OrderTestXSTAT
.         clear     str25
.        call    Trim using taskname
.         if (taskname = "Exchange Status is Even")
.                   move      "EVEN",str25
.         elseif (OLRN <> "" & (taskname = "" | taskname = "NO EXCHANGE HISTORY" | taskname = "No Exchange History" ))
..I can do the following as taskname=NULL and is not filled until after second pointer is used
..Check out subroutine and trace DimPtr/DimPtr1 to see what I mean.
.                pack    NORDFLD1,"01R",NCMPMLR
.                pack    NORDFLD2,"02R",NLOLLIST
.                clear   NORDFLD3
.                clear   NORDFLD4
.                call    CampOrderGetHistory using taskname,OLRN,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,C0
.                   if (taskname = "NO EXCHANGE HISTORY - RENTAL CONTINUATION")
.                             move      "NPE/C",str25
.                   else
.                             move      "NPE/T",str25
.                   endif
.         elseif (EFLAG <> "0")
.                compare USAGE1,USAGE2
.                if less
.                        sub     USAGE2,USAGE1
.                        if (EFLAG = "1")
.                                       pack      str25,DASH,USAGE1
.                        else
.                                       pack      str25,USAGE1
.                        endif
.                elseif not equal       .Greater
.                        sub     USAGE1,USAGE2
.                        if (EFLAG = "1")
.                                       pack      str25,USAGE2
.                        else
.                                       pack      str25,DASH,USAGE2
.                        endif
.                endif
.        endif
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="##,####0_);[Red](##,####0)"
.        setprop sheets(SheetIndex).range(str4),*Value=str25,*HorizontalAlignment=AlignRight
..NET REQUESTED
.        pack    str4,"L",str9
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="0.00%"
..NET RECEIVED
.        pack    str4,"M",str9
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="0.00%"
..AVERAGE NET
.        pack    str4,"N",str9
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="0.00%"
..NET NAMES
.        pack    str4,"O",str9
..=IF(F18>0,IF(M18>0,F18*M18,IF(L18>0,F18*L18,F18*N18)),IF(M18>0,E18*M18,IF(L18>0,E18*L18,E18*N18)))
.         pack      taskname,"=IF(F",str9,">0,IF(M",str9,">0,F",str9,"*M",str9,",IF(L",str9,">0,F",str9,"*L",str9,",F",str9,"*N",str9,")),IF(M",str9,">0,E",str9,"*M",str9,",IF(L",str9,">0,E",str9,"*L",str9,",E",str9,"*N",str9,")))"
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="##,####0_);(##,####0)"
.        setprop sheets(SheetIndex).range(str4),*Formula=taskname
..RESPONSE RATE
.        pack    str4,"P",str9
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="0.00%"
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"Q",str9
...=ROUND(O19*P19,0)
..        pack      str45,"=ROUND(O",str9,"*P",str9,",0)"
..
..RR CHANGE
.         pack    str5,"Q",str9
.         pack      str45,"=$Q$13"
.         setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="0.00"
..RR ADJUSTED
.         pack    str5,"R",str9
..=Q18*P18
.         pack      str45,"=Q",str9,"*P",str9
.         setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="0.00%"
..RETURNS
.        pack    str4,"S",str9
..=ROUND(O19*P19,0)
.         pack      str45,"=ROUND(O",str9,"*P",str9,",0)"
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="##,####0_);(##,####0)"
.        setprop sheets(SheetIndex).range(str4),*Formula=str45
..GIFT
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"R",str9
.        pack    str4,"T",str9
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..START PATCH 1.2 ADDED LOGIC
..GIFT CHANGE
.         pack    str5,"U",str9
.         call      Trim using NCMPGift
.         move      C0,N32
.         move      NCMPGift,N32
.         if (N32 = 0)
.                   move      "1",NCMPGift
.         endif
.         setprop sheets(SheetIndex).range(str5),*Value=NCMPGift,*NumberFormat="0.00"
..ADJUSTED GIFT
.         pack    str5,"V",str9
..=T18*U18
.         pack      str45,"=T",str9,"*U",str9
.         setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
..END PATCH 1.2 ADDED LOGIC
..REVENUE
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"S",str9
...=Q19*R19
..        pack      str45,"=Q",str9,"*R",str9
.        pack    str4,"W",str9
..=S19*T19
.         pack      str45,"=S",str9,"*T",str9
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=str45
..PROD COST
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"T",str9
..        move      PackPtr(index,1),str5
..        move      PackPtr(index,2),str6
..        call      Trim using str5
..        call      Trim using str6
...=(O18*$Q$10/1000+Q18*$Q$11*$R$11)    //PackPtr(index,1) = row of Package Total  //PackPtr(index,2) = row of Premium Cost/Percentage
..        pack      taskname,"=(O",str9,"*$Q$",str5,"/1000+Q",str9,"*$Q$",str6,"*$R$",str6,")"
.        pack    str4,"X",str9
.         move      PackPtr(index,1),str5
.         move      PackPtr(index,2),str6
.         call      Trim using str5
.         call      Trim using str6
..=(O18*$Q$10/1000+Q18*$Q$11*$R$11)     //PackPtr(index,1) = row of Package Total  //PackPtr(index,2) = row of Premium Cost/Percentage
.         pack      taskname,"=(O",str9,"*$Q$",str5,"/1000+Q",str9,"*$Q$",str6,"*$R$",str6,")"
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=taskname
..LIST COST
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"U",str9
...=O18/1000*I18
...=IF(O16<>0,O16/1000*I16,"")
..        pack      str45,"=IF(O",str9,"<>0,O",str9,"/1000*I",str9,",#"#")"
.        pack    str4,"Y",str9
..=IF(O16<>0,O16/1000*I16,"")
.         pack      str45,"=IF(O".,str9,"<>0,O",str9,"/1000*I",str9,",#"#")"
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=str45
..TOTAL COST
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"V",str9
...=SUM(T18:U18)
..        pack      str45,"=SUM(T",str9,":U",str9,")"
.        pack    str4,"Z",str9
..=SUM(X18:Y18)
.         pack      str45,"=SUM(X",str9,":Y",str9,")"
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=str45
..NET +/-
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"W",str9
...=S17-V17
..        pack      str45,"=S",str9,"-V",str9
.        pack    str4,"AA",str9
..=W17-Z17
.         pack      str45,"=W",str9,"-Z",str9
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=str45
..COST PER MEMBER
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"X",str9
...=IF(O20>0,W20/Q20,0)
...=IF(Q20>0,IF(W20<>0,W20/Q20,0),0)
..        pack      taskname,"=IF(Q",str9,">0,IF(W",str9,"<>0,W",str9,"/Q",str9,",0),0)"
.        pack    str4,"AB",str9
..=IF(S20>0,IF(AA20<>0,AA20/S20,0),0)
.         pack      taskname,"=IF(S",str9,">0,IF(AA",str9,"<>0,AA",str9,"/S",str9,",0),0)"
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=taskname
..INC M
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"Y",str9
...=IF(Q19>0,S19/O19*1000,0)
...=IF(O19>0,IF(S19<>0,S19/O19*1000,0),0)
..        pack      taskname,"=IF(O",str9,">0,IF(S",str9,"<>0,S",str9,"/O",str9,"*1000,0),0)"
.        pack    str4,"AC",str9
..=IF(O19>0,IF(W19<>0,W19/O19*1000,0),0)
.         pack      taskname,"=IF(O",str9,">0,IF(W",str9,"<>0,W",str9,"/O",str9,"*1000,0),0)"
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=taskname
..COST/M
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"Z",str9
...=IF(O20>0,IF(V20<>0,V20/O20*1000,0),0)
..        pack      taskname,"=IF(O",str9,">0,IF(V",str9,"<>0,V",str9,"/O",str9,"*1000,0),0)"
.        pack    str4,"AD",str9
..=IF(O20>0,IF(Z20<>0,Z20/O20*1000,0),0)
.         pack      taskname,"=IF(O",str9,">0,IF(Z",str9,"<>0,Z",str9,"/O",str9,"*1000,0),0)"
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=taskname
..COST?$
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"AA",str9
...=IF(S19>0,V19/S19,0)
...=IF(S19>0,IF(V19<>0,V19/S19,0),0)
..        pack      taskname,"=IF(S",str9,">0,IF(V",str9,"<>0,V",str9,"/S",str9,",0),0)"
.        pack    str4,"AE",str9
..=IF(W19>0,IF(Z19<>0,Z19/W19,0),0)
.         pack      taskname,"=IF(W",str9,">0,IF(Z",str9,"<>0,Z",str9,"/W",str9,",0),0)"
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=taskname
..
.         sub       C1,LastRec(index),N9
.         move      N9,str10
.         call      Trim using str10
..C Vol
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"AB",str9
...=O17+AB16
..        pack      str45,"=O",str9,"+AB",str10
.        pack    str4,"AF",str9
..=O17+AF16
.         pack      str45,"=O",str9,"+AF",str10
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="##,####0_);(##,####0)"
.        setprop sheets(SheetIndex).range(str4),*Formula=str45
..C Resp
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"AC",str9
...=Q17+AC16
..        pack      str45,"=Q",str9,"+AC",str10
.         pack    str4,"AG",str9
..=S17+AG16
.         pack      str45,"=S",str9,"+AG",str10
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="##,####0_);(##,####0)"
.        setprop sheets(SheetIndex).range(str4),*Formula=str45
..C RR
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"AD",str9
...=AC17/AB17
..        pack      str45,"=AC",str9,"/AB",str9
.        pack    str4,"AH",str9
..=AG17/AF17
.         pack      str45,"=AG",str9,"/AF",str9
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="0.00%"
.        setprop sheets(SheetIndex).range(str4),*Formula=str45
..C Rev
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"AE",str9
...=S17+AE16
..        pack      str45,"=S",str9,"+AE",str10
.        pack    str4,"AI",str9
..=W17+AI16
.         pack      str45,"=W",str9,"+AI",str10
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=str45
..C Gift
..START PATCH 1.2 REPLACED LOGIC
.        pack    str4,"AJ",str9
..=AI17/AG17
.         pack      str45,"=AI",str9,"/AG",str9
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=str45
..C Cost
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"AG",str9
...=V17+AG16
..        pack      str45,"=V",str9,"+AG",str10
.        pack    str4,"AK",str9
..=Z17+AK16
.         pack      str45,"=Z",str9,"+AK",str10
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=str45
..C Net
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"AH",str9
...=AE17-AG17
..        pack      str45,"=AE",str9,"-AG",str9
.        pack    str4,"AL",str9
..=AI17-AK17
.         pack      str45,"=AI",str9,"-AK",str9
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=str45
..C CPA
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"AI",str9
...=(AE17-AG17)/AC17
..        pack      str45,"=(AE",str9,"-AG",str9,")/AC",str9
.        pack    str4,"AM",str9
..=(AI17-AK17)/AG17
.         pack      str45,"=(AI",str9,"-AK",str9,")/AG",str9
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.        setprop sheets(SheetIndex).range(str4),*Formula=str45
..START PATCH 1.2 ADDED LOGIC
..
.         pack    str5,"AN",str9
.         pack      str45,"=AB",str9,"+100"
.         setprop sheets(SheetIndex).range(str5),*Formula=str45
..
.         pack    str5,"AP",str9
..=AN18/AO18
.         pack      str45,"=AN",str9,"/AO",str9
.         setprop sheets(SheetIndex).range(str5),*Formula=str45,*NumberFormat="0.00"
..END PATCH 1.2 ADDED LOGIC
..LIST UPDATE
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"AJ",str9
.        pack    str4,"AQ",str9
..END PATCH 1.2 REPLACED LOGIC
..USAGE
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"AK",str9
.        pack    str4,"AR",str9
..END PATCH 1.2 REPLACED LOGIC
.         call      CampOrderCalcUsage using NCMPMLR,NLOLLIST,howmany,N7
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="##,####0_);(##,####0)"
.        setprop sheets(SheetIndex).range(str4),*Value=howmany
..START PATCH 1.2 ADDED LOGIC
.        pack    str4,"AS",str9
.        setprop sheets(SheetIndex).range(str4),*NumberFormat="##,####0_);(##,####0)"
.        setprop sheets(SheetIndex).range(str4),*Value=N7
..END PATCH 1.2 ADDED LOGIC
..NIN LR
..START PATCH 1.2 REPLACED LOGIC
..        pack    str4,"AN",str9
.        pack    str4,"AU",str9
..END PATCH 1.2 REPLACED LOGIC
.        setprop sheets(SheetIndex).range(str4),*Value=OLRN
.         if (FrmPtr2 = C0)   .LR/LOL Records without Projections
..RECO QTY
.                 pack    str4,"E",str9
.                   if (NLOLQTY > "0")
.                             setprop sheets(SheetIndex).range(str4),*Value=NLOLQTY
.                   endif
..AVERAGE NET
.                 pack    str4,"N",str9
.         setprop sheets(SheetIndex).range(str4),*NumberFormat="0.00%"
.                   move      C0,N35
.                   move      NLOLNET,N35
.                   mult      ".01",N35
.                 setprop sheets(SheetIndex).range(str4,str4),*Value=N35
.         else                          .Projection Records
..RECO QTY
.                 pack    str4,"E",str9
.                   if (NLOLQTY > "0")
.                             setprop sheets(SheetIndex).range(str4),*Value=STATRECQTY
.                   endif
..RECEIVED QTY
.                 pack    str4,"F",str9
.         setprop sheets(SheetIndex).range(str4),*Value=STATMQTY
..LIST COST
.                 pack    str4,"H",str9
.         setprop sheets(SheetIndex).range(str4),*Value=""
..AVG ?/M
.                 pack    str4,"I",str9
.         setprop sheets(SheetIndex).range(str4),*Value=STATLCPM
..NET REQUESTED
.                 pack    str4,"L",str9
.                   mult      ".01",STATNETREQ,N35
.         setprop sheets(SheetIndex).range(str4),*Value=N35
..NET RECEIVED
.                 pack    str4,"M",str9
.                   mult      ".01",STATNETREC,N35
.         setprop sheets(SheetIndex).range(str4),*Value=N35
..AVERAGE NET
.                 pack    str4,"N",str9
.                   mult      ".01",STATAVGNET,N35
.         setprop sheets(SheetIndex).range(str4),*Value=N35
..RESPONSE RATE
.                 pack    str4,"P",str9
.                   mult      ".01",STATRESP2,N35
.         setprop sheets(SheetIndex).range(str4),*Value=N35
..GIFT
..START PATCH 1.2 REPLACED LOGIC
..                pack    str4,"R",str9
.                 pack    str4,"T",str9
..END PATCH 1.2 REPLACED LOGIC
.         setprop sheets(SheetIndex).range(str4),*Value=STATGIFT
..COMMENTS
.                   move      C1,STATNPATH
.                   pack      STATNFLD,STATLR,STATLOL,STATPCKNUM
.                   move      "LoadRec-STATNKEY",Location
.                   pack      KeyLocation,"Key: ",STATNFLD
.                   call      STATNKEY
.                   call      Trim using STATNNOTE
..START PATCH 1.2 REPLACED LOGIC
..                pack    str4,"AM",str9
.                 pack    str4,"AT",str9
..END PATCH 1.2 REPLACED LOGIC
.                   pack      str2,NewLine,B1
.                   rep       str2,STATNNOTE
.         setprop sheets(SheetIndex).range(str4),*Value=STATNNOTE
.         endif
.............................
.LIST
          pack    range,"A",str9
          setprop sheets(SheetIndex).range(range),*Value=NLOLLIST
.LIST NAME
          pack    range,"B",str9
          move    C1,NDATPATH
          move    NLOLLIST,NDATFLD
          move    "LoadDetail-NDATKEY",Location
          pack    KeyLocation,"Key: ",NDATFLD
          call    NDATKEY
          if not over
                    call    Trim using OLSTNAME
                    setprop sheets(SheetIndex).range(range),*Value=OLSTNAME
          endif
.SELECT
          pack    range,"C",str9
          call    Trim using NLOLSELECT
          setprop sheets(SheetIndex).range(range),*Value=NLOLSELECT
.UNIVERSE
          pack    range,"D",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="##,####0_);(##,####0)"
          if (statseluni > C0)
                    setprop sheets(SheetIndex).range(range),*Value=statseluni
          endif
.RECO QTY
          pack    range,"E",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="##,####0_);(##,####0)"
.RECEIVED QTY
          pack    range,"F",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="##,####0_);(##,####0)"
.R/E
          pack    range,"G",str9
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
.Do nothing.  Live Orders should have previous instance of str1 used.
.                        clear   str1
                    endif
          endif
          setprop sheets(SheetIndex).range(range),*Value=str1,*HorizontalAlignment=AlignCenter
.LIST COST PER/M
          pack    range,"H",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.AVG ?/M
          pack    range,"I",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.MAIL DATE
          pack    range,"J",str9
          call      Trim using NLOLMDATE
          if (NLOLMDATE <> "")
                    unpack    NLOLMDATE,str2,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,str2,YY
          else
                    clear     str10
          endif
          setprop sheets(SheetIndex).range(range),*Value=str10
.XSTAT
          pack    range,"K",str9
.START PATCH 1.57 REPLACED LOGIC - TEMPORARY PATCH
.         move    NCMPMLR,OMLRNUM
          move    COMPOLDMLR,OMLRNUM
.END PATCH 1.57 REPLACED LOGIC
          move    NLOLLIST,OLNUM
          call    OrderTestXSTAT
          clear     str25
          call    Trim using taskname
          if (taskname = "Exchange Status is Even")
                    move      "EVEN",str25
          elseif (OLRN <> "" & (taskname = "" | taskname = "NO EXCHANGE HISTORY" | taskname = "No Exchange History" ))
.I can do the following as taskname=NULL and is not filled until after second pointer is used
.Check out subroutine and trace DimPtr/DimPtr1 to see what I mean.
.START PATCH 1.57 REPLACED LOGIC - TEMPORARY PATCH
.                   pack    NORDFLD1,"01R",NCMPMLR
                    pack    NORDFLD1,"01R",COMPOLDMLR
.END PATCH 1.57 REPLACED LOGIC - TEMPORARY PATCH
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
          setprop sheets(SheetIndex).range(range),*NumberFormat="##,####0_);[Red](##,####0)"
          setprop sheets(SheetIndex).range(range),*Value=str25,*HorizontalAlignment=AlignRight
.NET REQUESTED
          pack    range,"L",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="0.00%"
.NET RECEIVED
          pack    range,"M",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="0.00%"
.AVERAGE NET
          pack    range,"N",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="0.00%"
.NET NAMES
          pack    range,"O",str9
.=IF(F18>0,IF(M18>0,F18*M18,IF(L18>0,F18*L18,F18*N18)),IF(M18>0,E18*M18,IF(L18>0,E18*L18,E18*N18)))
          pack      taskname,"=IF(F",str9,">0,IF(M",str9,">0,F",str9,"*M",str9,",IF(L",str9,">0,F",str9,"*L",str9,",F",str9,"*N",str9,")),IF(M",str9,">0,E",str9,"*M",str9,",IF(L",str9,">0,E",str9,"*L",str9,",E",str9,"*N",str9,")))"
          setprop sheets(SheetIndex).range(range),*NumberFormat="##,####0_);(##,####0)"
          setprop sheets(SheetIndex).range(range),*Formula=taskname
.RESPONSE RATE
          pack    range,"P",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="0.00%"
.RR CHANGE
          pack    range2,"Q",str9
.START PATCH 1.4 REPLACED LOGIC
.         pack      str45,"=$Q$13"
          pack      str45,"=$Q$15"
.END PATCH 1.4 REPLACED LOGIC
          setprop sheets(SheetIndex).range(range2),*Formula=str45,*NumberFormat="0.00"
.RR ADJUSTED
          pack    range2,"R",str9
.=Q18*P18
          pack      str45,"=Q",str9,"*P",str9
          setprop sheets(SheetIndex).range(range2),*Formula=str45,*NumberFormat="0.00%"
.RETURNS
          pack    range,"S",str9
.START PATCH 1.52.3 REPLACED LOGIC
..=ROUND(O19*P19,0)
.         pack      str45,"=ROUND(O",str9,"*P",str9,",0)"
..............................................
.=ROUND(O19*R19,0)
          pack      str45,"=ROUND(O",str9,"*R",str9,",0)"
.END PATCH 1.52.3 REPLACED LOGIC
          setprop sheets(SheetIndex).range(range),*NumberFormat="##,####0_);(##,####0)"
          setprop sheets(SheetIndex).range(range),*Formula=str45
.GIFT
          pack    range,"T",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.GIFT CHANGE
          pack    range2,"U",str9
.START PATCH 1.52.2 REPLACED LOGIC
.         call      Trim using NCMPGift
.         move      C0,N32
.         move      NCMPGift,N32
.         if (N32 = 0)
.                   move      "1",NCMPGift
.         endif
.         setprop sheets(SheetIndex).range(range2),*Value=NCMPGift,*NumberFormat="0.00"
          pack      str45,"=$T$15"
          setprop sheets(SheetIndex).range(range2),*Formula=str45,*NumberFormat="0.00"
.END PATCH 1.52.2 REPLACED LOGIC
.ADJUSTED GIFT
          pack    range2,"V",str9
.=T18*U18
          pack      str45,"=T",str9,"*U",str9
          setprop sheets(SheetIndex).range(range2),*Formula=str45,*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
.REVENUE
          pack    range,"W",str9
.START PATCH 1.52.2 REPLACED LOGIC
.HM WANTED TO CHANGE THIS FORMULA
..=S19*T19
.         pack      str45,"=S",str9,"*T",str9
.=S19*V19
          pack      str45,"=S",str9,"*V",str9
.END PATCH 1.52.2 REPLACED LOGIC
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=str45
.PROD COST
.=(O18*$Q$10/1000+Q18*$Q$11*$R$11)      //PackPtr(index,1) = row of Package Total  //PackPtr(index,2) = row of Premium Cost/Percentage
          pack    range,"X",str9
          move      PackPtr(index,1),str5
          move      PackPtr(index,2),str6
          call      Trim using str5
          call      Trim using str6
.=(O18*$Q$10/1000+Q18*$Q$11*$R$11)      //PackPtr(index,1) = row of Package Total  //PackPtr(index,2) = row of Premium Cost/Percentage
          pack      taskname,"=(O",str9,"*$Q$",str5,"/1000+Q",str9,"*$Q$",str6,"*$R$",str6,")"
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=taskname
.LIST COST
          pack    range,"Y",str9
.=IF(O16<>0,O16/1000*I16,"")
          pack      str45,"=IF(O",str9,"<>0,O",str9,"/1000*I",str9,",#"#")"
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=str45
.TOTAL COST
          pack    range,"Z",str9
.=SUM(X18:Y18)
          pack      str45,"=SUM(X",str9,":Y",str9,")"
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=str45
.NET +/-
        pack    range,"AA",str9
.=W17-Z17
          pack      str45,"=W",str9,"-Z",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0_);_($* (##,####0);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=str45
.COST PER MEMBER
          pack    range,"AB",str9
.=IF(S20>0,IF(AA20<>0,AA20/S20,0),0)
          pack      taskname,"=IF(S",str9,">0,IF(AA",str9,"<>0,AA",str9,"/S",str9,",0),0)"
.END PATCH 1.2 REPLACED LOGIC
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=taskname
.INC M
          pack    range,"AC",str9
.=IF(O19>0,IF(W19<>0,W19/O19*1000,0),0)
          pack      taskname,"=IF(O",str9,">0,IF(W",str9,"<>0,W",str9,"/O",str9,"*1000,0),0)"
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=taskname
.COST/M
          pack    range,"AD",str9
.=IF(O20>0,IF(Z20<>0,Z20/O20*1000,0),0)
          pack      taskname,"=IF(O",str9,">0,IF(Z",str9,"<>0,Z",str9,"/O",str9,"*1000,0),0)"
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=taskname
.COST?$
          pack    range,"AE",str9
.=IF(W19>0,IF(Z19<>0,Z19/W19,0),0)
          pack      taskname,"=IF(W",str9,">0,IF(Z",str9,"<>0,Z",str9,"/W",str9,",0),0)"
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=taskname
.
          sub       C1,LastRec(index),N9
          move      N9,str10
          call      Trim using str10
.C Vol
          pack    range,"AF",str9
.=O17+AF16
          pack      str45,"=O",str9,"+AF",str10
          setprop sheets(SheetIndex).range(range),*NumberFormat="##,####0_);(##,####0)"
          setprop sheets(SheetIndex).range(range),*Formula=str45
.C Resp
          pack    range,"AG",str9
.=S17+AG16
          pack      str45,"=S",str9,"+AG",str10
          setprop sheets(SheetIndex).range(range),*NumberFormat="##,####0_);(##,####0)"
          setprop sheets(SheetIndex).range(range),*Formula=str45
.C RR
          pack    range,"AH",str9
.=AG17/AF17
          pack      str45,"=AG",str9,"/AF",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="0.00%"
          setprop sheets(SheetIndex).range(range),*Formula=str45
.C Rev
          pack    range,"AI",str9
.=W17+AI16
          pack      str45,"=W",str9,"+AI",str10
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=str45
.C Gift
          pack    range,"AJ",str9
.=AI17/AG17
          pack      str45,"=AI",str9,"/AG",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=str45
.C Cost
          pack    range,"AK",str9
.=Z17+AK16
          pack      str45,"=Z",str9,"+AK",str10
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=str45
.C Net
          pack    range,"AL",str9
.=AI17-AK17
          pack      str45,"=AI",str9,"-AK",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=str45
.C CPA
          pack    range,"AM",str9
.=(AI17-AK17)/AG17
          pack      str45,"=(AI",str9,"-AK",str9,")/AG",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="_($* ##,####0.00_);_($* (##,####0.00);_($* #"-#"_);_(@_)"
          setprop sheets(SheetIndex).range(range),*Formula=str45
          pack    range2,"AN",str9
          pack      str45,"=AB",str9,"+100"
          setprop sheets(SheetIndex).range(range2),*Formula=str45
.
          pack    range2,"AP",str9
.=AN18/AO18
          pack      str45,"=AN",str9,"/AO",str9
          setprop sheets(SheetIndex).range(range2),*Formula=str45,*NumberFormat="0.00"
.LIST UPDATE
          pack    range,"AQ",str9
.USAGE
          pack    range,"AR",str9
.START PATCH 5/19/05 ASH REMOVED TEMPORARY PATCH
..START PATCH 1.57 REPLACED LOGIC - TEMPORARY PATCH
..        call      CampOrderCalcUsage using NCMPMLR,NLOLLIST,howmany,N7
.         call      CampOrderCalcUsage using COMPOLDMLR,NLOLLIST,howmany,N7
..END PATCH 1.57 REPLACED LOGIC - TEMPORARY PATCH
          call      CampOrderCalcUsage using NCMPMLR,NLOLLIST,howmany,N7
.END PATCH 5/19/05 ASH REMOVED TEMPORARY PATCH
          setprop sheets(SheetIndex).range(range),*NumberFormat="##,####0_);(##,####0)"
          setprop sheets(SheetIndex).range(range),*Value=howmany
          pack    range,"AS",str9
          setprop sheets(SheetIndex).range(range),*NumberFormat="##,####0_);(##,####0)"
          setprop sheets(SheetIndex).range(range),*Value=N7
.NIN LR
          pack    range,"AU",str9
          setprop sheets(SheetIndex).range(range),*Value=OLRN
          if (FrmPtr2 = C0)   .LR/LOL Records without Projections
.RECO QTY
                    pack    range,"E",str9
                    if (NLOLQTY > "0")
                              setprop sheets(SheetIndex).range(range),*Value=NLOLQTY
                    endif
.AVERAGE NET
                    pack    range,"N",str9
                    setprop sheets(SheetIndex).range(range),*NumberFormat="0.00%"
                    move      C0,N35
                    move      NLOLNET,N35
                    mult      ".01",N35
                    setprop sheets(SheetIndex).range(range),*Value=N35
          else                          .Projection Records
.RECO QTY
                    pack    range,"E",str9
                    if (NLOLQTY > "0")
                              setprop sheets(SheetIndex).range(range),*Value=STATRECQTY
                    endif
.RECEIVED QTY
                    pack    range,"F",str9
                    setprop sheets(SheetIndex).range(range),*Value=STATMQTY
.LIST COST
                    pack    range,"H",str9
                    setprop sheets(SheetIndex).range(range),*Value=""
.AVG ?/M
                    pack    range,"I",str9
                    setprop sheets(SheetIndex).range(range),*Value=STATLCPM
.NET REQUESTED
                    pack    range,"L",str9
                    mult      ".01",STATNETREQ,N35
                    setprop sheets(SheetIndex).range(range),*Value=N35
.NET RECEIVED
                    pack    range,"M",str9
                    mult      ".01",STATNETREC,N35
                    setprop sheets(SheetIndex).range(range),*Value=N35
.AVERAGE NET
                    pack    range,"N",str9
                    mult      ".01",STATAVGNET,N35
                    setprop sheets(SheetIndex).range(range),*Value=N35
.RESPONSE RATE
                    pack    range,"P",str9
                    mult      ".01",STATRESP2,N35
                    setprop sheets(SheetIndex).range(range),*Value=N35
.GIFT
                    pack    range,"T",str9
                    setprop sheets(SheetIndex).range(range),*Value=STATGIFT
.COMMENTS
                    move      C1,STATNPATH
                    pack      STATNFLD,STATLR,STATLOL,STATPCKNUM
                    move      "LoadRec-STATNKEY",Location
                    pack      KeyLocation,"Key: ",STATNFLD
                    call      STATNKEY
                    call      Trim using STATNNOTE
                    pack    range,"AT",str9
                    pack      str2,NewLine,B1
                    rep       str2,STATNNOTE
                    setprop sheets(SheetIndex).range(range),*Value=STATNNOTE
          endif
.END PATCH 1.3 REPLACED LOGIC
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
.START PATCH 1.54 REPLACED LOGIC
.        include nmlrIO.inc
.        include nbrkIO.inc
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
.START PATCH 1.53 ADDED LOGIC
          INCLUDE   NSEL2IO.INC
.END PATCH 1.53 ADDED LOGIC
        include comlogic.inc
