PC       EQU       0
         INCLUDE    COMMON.inc
         INCLUDE    CONS.inc
         INCLUDE    NSHPDD.inc
         INCLUDE    HP.inc
           INCLUDE  COMPDD.inc
           INCLUDE  CNTDD.INC
         INCLUDE    NORDDD.inc
         INCLUDE    NCNTDD.inc
         INCLUDE    WINAPI.inc

Release    INIT    "1.02"               DLH      .Excel 2013 *WindowState=xlMinimized
Reldate   Init      "2014 January 22"
.Release  INIT                 "1.01"               DLH excel version
.reldate   Init      "23 APril 2009"
.Release  INIT                 "1.00"               DMB 13 AUG 2006 Temporary Release of shipping prgram to apply frontline shipping records.
//
//In order to use any of the properties/methods associated with all parent objects
//of the Worksheet, I need to create automation objects for each of them.
//
//Look at Excel Object Model to understand heirarchy.  This can be found in hard
//documentation:  Microsoft Office 2000 Object Model Guide (found in MS Office 2000 Developers Edition).
//Software available via PL/B Designer - create a Container object on a form, create an Excel
//Spreadsheet, right click on Container object and Browse object.  This will invoke the PL/B Object
//Browser, which will give you SOME of the components of the Object Model.  To browse the Object
//Model in its entirety, open Excel.  Under Tools menu select Macro, select Visual Basic Editor.
//In the Visual Basic Editor screen, under the View menu, select Object Browser.  There you can 
//view all of the objects/methods/properties in Excel.  Right clicking on an item will give you
//option to locate Help topics to see specifics.
//
//General heirarchy:
// Excel Application
//       Workbooks Collection (all open Workbooks)
//               Single Workbook
//                       Worksheets Collection (all Worksheets in this Workbook)
//                               Single Worksheet
//                                       SortColumn (a Single Column in that Worksheet used for sorting)

.begin 1.01 to find version of excel  DH 04/02/09
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.end 1.01 to find version of excel

books                         AUTOMATION
book                          AUTOMATION
.......................
//bars    automation
//bar     automation
//menubar INTEGER 2,"0x00000006"
.......................
SubFlag form    1
sheets                        AUTOMATION
sheet                         AUTOMATION
sortcol                       AUTOMATION
sortcol1                      AUTOMATION
ex                            AUTOMATION       class="Excel.Application"
exrange                                 AUTOMATION       

HPageBreaks                             AUTOMATION
HPageBreak                              AUTOMATION
VT_BOOL                       EQU                  11
OTRUE                         VARIANT
OFALSE                        VARIANT
VT_I4                         EQU                  3           .4 byte integer
Zoom70                        VARIANT

VT_R8                                   EQU                  5           .Double - 8 byte Real
xlColumnWidth                           VARIANT

xlColumnWidthGross            VARIANT

xlColumnWidthMailer           VARIANT
xlColumnWidthPrice            VARIANT
xlColumnWidthDescription      VARIANT

TopMargin                     VARIANT
BottomMargin                            VARIANT
LeftMargin                              VARIANT
RightMargin                             VARIANT
xlCenter                      VARIANT

xlRowHeight                             VARIANT

//Formatting vars needed
//This constant was found in the Object Browser in Excel under the Help topic for the
//HorizontalAlignment property of the Range object.
AlignLeft                               integer 4,"0xffffefdd"
AlignRight                              integer 4,"0xffffefc8"
AlignCenter                             integer 4,"0xffffeff4"
SheetsDefault                           integer 4,"0x00000000"
xlLandscape                             integer 4,"0x2"                     .2
xlPortrait                              integer 4,"0x1"                     .1
xlMinimized                             integer 4,"0xFFFFEFD4"
xlUnderlineStyleSingle                  integer 4,"0x2"

xlBorderWeightMedium                    VARIANT
xlPageBreakManual             VARIANT
xlPageBreakAutomatic                    VARIANT

SheetIndex                              VARIANT

//Colors
Red Color
RGB form 24
//Flag whether to make row red or not
ColorFlag Dim 1


XLSNAME    DIM  255


CELLPOINT                     FORM      5.2
C15       FORM "15"

Cell         DIM    5
Cell1        DIM    5
CurCellNum FORM 5
CellRange  DIM   255
CellRowCnt          FORM      "46"
CellRowCnt1         FORM      "54"


Input     File      
SAVE      File      
LOGFILE   Ifile     KEYLEN=6,var=498

InputName Dim       254

Count     Form      4
Write     Form      4                                       //Counter for Records Updated
Update    Form      4                                       //Counter for Records Updated
Diff      Form      10.2
Tenper    Form      10
Page      Form      2
Date      Dim       8
Sales     Dim       2
Holdmkey  Dim       7

//Temp Vars for Data Clean
ShippedLR       Dim       6
ShippedCost         Dim       9
ShippedQTY      Dim       9
ShippedInfo     Dim       36
ShippedDate     Dim       8

HoldQuant       Dim       11

QTYVAR          Form      10.2
Var       Dim       1
Faxflag   Dim       1
Newflag   Init      "N"
Okstats   Init      "0"

Variation Form      3.2
RecordCount     Form      10

        create  Red=255:0:0
          getitem   Red,0,RGB

//File Cleanup
         Pack      XLSNAME,"FrontLineShipping.xls"
         Pack      Taskname,"c:\work\","Frontlineshipping.xls"
         Erase     Taskname
//File Prep

.         PACK      InputName,NTWKPATH1,"FrontLine_shp.csv"
         PACK      InputName,"\\nins1\e\STORAGE\IMPORT\000002\SHIPPING\","FrontLine_shp.csv"

         OPEN      INPUT,InputName

         OPEN      LOGFILE,"SHIPfax"         *TURNED On 12/08/94.
//Program Header Information
         MOVE      "NSHP0011" TO PROGRAM
         MOVE      "Frontline Shipping Information" TO STITLE
         MOVE      "Names In The News" TO COMPNME
         
         
         MOVE      C0 TO PAGE
//Todays Date         
         CLOCK     DATE TO DATE
         Clock     timestamp to str18
         Unpack    str18 to str2,str16

         Call          PAINT
         MOVE      C1 TO NORDPATH
//Which Windows Version is This?
         Call      GetWinVer
         
         
//Create the Variant objects for Excel SpreadSheet
//Initialize variables
          create  Zoom70,VarType=VT_I4,VarValue=70
          create    OTRUE,VarType=VT_BOOL,VarValue=1
          create    OFALSE,VarType=VT_BOOL,VarValue=0
          create    xlColumnWidth,VarType=VT_R8,VarValue="0.0"
          create    xlColumnWidthGross,VarType=VT_R8,VarValue="10.5"  
          create    xlColumnWidthMailer,VarType=VT_R8,VarValue="40.5" 
          create    xlColumnWidthPrice,VarType=VT_R8,VarValue="9.0"             
          create    xlColumnWidthDescription,VarType=VT_R8,VarValue="60.0"                          
//"1" increment in Excel interface equals "1.3888" in OLE logic
          create    TopMargin,VarType=VT_R8,VarValue="18"             Roughly equals .25 inches:  18 * 1.388 = 25
          create    BottomMargin,VarType=VT_R8,VarValue="36"          Roughly equals .50 inches:  36 * 1.388 = 50
          create    LeftMargin,VarType=VT_R8,VarValue="14"            Roughly equals .25 inches:  18 * 1.388 = 25       
          create    RightMargin,VarType=VT_R8,VarValue="14"           Roughly equals .25 inches:  18 * 1.388 = 25       
          
          create    xlPageBreakManual,VarType=VT_R8,VarValue="-4135"
          create    xlPageBreakAutomatic,VarType=VT_R8,VarValue="-4105"         
          create    xlCenter,VarType=VT_R8,VarValue="-4108"           
          create    xlBorderWeightMedium,VarType=VT_R8,VarValue="-4138"         
          create    xlRowHeight,VarType=VT_R8,VarValue="15.0"
          create    SheetIndex,VarType=VT_I4      

//         

INPUT        
         READ      INPUT,SEQ;*cdfon,ShippedLR:   
                               ShippedInfo:         
                               ShippedDate:                                      
                                       ShippedCost: 
                                       ShippedQTY 
         GOTO   EOJ IF OVER
         CMATCH     " " TO ShippedLR         
         GOTO       INPUT IF EOS    
         REP    ZFILL IN ShippedLR
         MOVE   ShippedLR TO NSHPFLD         
         CALL   NSHPKEY
         If     Not Over
          Clear HoldQuant         
          Move Squant,str9
                Call FormatNumeric using str9,HOLDQUANT
          Clear SHPVARS                
         Else
          Clear SHPVARS
          Clear HoldQuant
         Endif
         
         Clear Taskname
         Add        C1 to RecordCount
         If (RecordCount = C1)
//Create a new excel worksheet
//.Open Excel application
                    create  ex
.begin 1.01 get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
//File Cleanup
         if        (#ver = c1)
         Pack      XLSNAME,"FrontLineShipping.xlsx"
         Pack      Taskname,"c:\work\","Frontlineshipping.xlsx"
         else
         Pack      XLSNAME,"FrontLineShipping.xls"
         Pack      Taskname,"c:\work\","Frontlineshipping.xls"
         endif
         Erase     Taskname
.end 1.01 get exel version info
.begin patch 1.02
.                  setprop ex,*WindowState=xlMinimized
.end patch 1.02
                  setprop ex,*Visible="True"
                    setprop ex.CommandBars("Standard"),*Visible="True"
                    setprop ex.CommandBars("Formatting"),*Visible="True"
                    setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
//........................
//.       setprop ex,*AltStartupPath="C:\Documents and Settings\aharkin\application data\microsoft\office"
//.       setprop ex,*DisplayFullScreen=OTRUE
//........................
//.Reset Default of Worksheets found in a Workbook
          getprop ex,*SheetsInNewWorkbook=SheetsDefault
          setprop ex,*SheetsInNewWorkbook=C1
//........................
.                   getprop ex,*CommandBars=bars
.                   getprop   bars,*ActiveMenuBar=bar
.                   setprop   bar,*Visible="True"
.                   setprop   bar,*Position=menubar
//........................
//.Create Workbooks collection
          getprop ex,*Workbooks=books
//.Create/Add a single Workbook
          books.add
          books.item giving book using 1
//.Create Worksheets collection
          getprop book,*Sheets=sheets
//.Create a single Worksheet - we did not need to add it as we set the default above to
//.add one new Worksheet each time a Workbook is created.
          sheets.item giving sheet using 1
                    setprop sheet.PageSetup,*Orientation=xlPortrait
                  pack taskname," &D",crlf," &P"
                  setprop sheet.PageSetup,*RightHeader=taskname        
//Sheet Formatting          
          setprop sheet.PageSetup,*Zoom=Zoom70
                  setprop sheet.PageSetup,*TopMargin=TopMargin
                  setprop sheet.PageSetup,*BottomMargin=BottomMargin
                  setprop sheet.PageSetup,*FooterMargin=TopMargin
                  setprop sheet.PageSetup,*LeftMargin=LeftMargin
                  setprop sheet.PageSetup,*RightMargin=RightMargin     
                    getprop sheet,*Hpagebreaks=HpageBreaks
                    pack str5 with "L1",":","P1"
                    move c1 to curcellnum
          CALL      HEADER              
         Endif
         MOVE       NO TO VAR
.         CMATCH " " TO ShippedLR
.         GOTO      INPUT IF EOS
//Ship Description
          Move ShippedInfo,SINFO
         


          move    "," to str1
        call    Removechar using ShippedCost,str1
          move    "$" to str1
        call    Removechar using ShippedCost,str1   
.         move    "." to str1
.        call    Removechar using scost,str1        
        call        trim using ShippedCost
        Type    ShippedCost
        If Not Over
                  Move ShippedCost to n9
                    if (n9 <> C0)
//      Patch 1.1  to account for possible dollar sign in costs.        
                            Scan    ".00",ShippedCost
                            If Equal
                              SDELETE ShippedCost
                              Reset   ShippedCost
                              Call Trim using ShippedCost
                            Endif
                              Count n1,ShippedCost
                              If (n1 = c4)        
                                        move ShippedCost,spost
                                        Unpack ShippedCost,str2,str3
                                        pack ShippedCost,str2,".",str3
                              else
                                        goto badcost                  
                              Endif
                    else
                              move    ShippedCost,spost
                    
                    Endif
          Endif



//Date Check
DateCheck
          Unpack ShippedDate,str4,mm,dd
          type ShippedDate
          goto baddate if not equal     
          unpack str4,cc,yy
          compare   "20" to CC
          goto      baddate if not equal
          move    MM,N2
        if (N2 > "12")
                    goto baddate
          elseif (N2 = "00")
                    goto baddate        
        else
                move    DD,N2
                if (N2 > "31")
                              goto baddate                
                    elseif (N2 = "00")
                              goto baddate                            
                else
                            move    CC,N2
                              if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                        goto baddate                
                        elseif (N2 = "19")
                                move    YY,N2
                                if (N2 < "80")
                                                  goto baddate                
                                endif
                              elseif (N2 = "00")                        
                                        goto baddate                                      
                              endif
                endif
         endif      
         pack       sdate from cc,yy,mm,dd

//Quantity Check
SQTY
           Call      Zfillit Using ShippedQTY
         TYPE      ShippedQTY
         GOTO      BADQTY IF NOT EQUAL
         Move      ShippedQTY,SQUANT
.         REP       ZFILL IN ShippedLR
         Move      ShippedLR,SLRNUM
//Frontline Shipping Data         
         MOVE     "F" TO SCODE
         ADD       C1 TO COUNT
         DISPLAY   *P10:12,"RECORDS IN : ",COUNT
         MOVE      SLRNUM TO NORDFLD
         CALL      NORDKEY
         GOTO      NOORD IF OVER
         RESET     OKSTATS
//Only Live Orders are applied         
         MATCH     OSTAT IN OKSTATS
         GOTO      NOLIVE IF NOT EQUAL
POST
//Check For Variance will notify if 10% or greater
//Find out what is 10% of Order Quantity
         MOVE      C0 TO N10
         MOVE      OQTY TO N10
         DIV       C10 INTO N10
         MOVE      N10 TO TENPER
         MOVE      ShippedQTY TO N10
         MOVE      OQTY TO DIFF
         SUB       N10 FROM DIFF
//Is There no variation/difference?         
         COMPARE   C0 TO DIFF
//If negative make it positive for comparison purposes.    
           IF LESS
                    MULT      SEQ BY DIFF          
           Endif
//Is the variation 10% or more - compare calculated 10% difference menus actual difference.         
         COMPARE   DIFF TO TENPER
         CALL      VAR IF NOT GREATER

         MOVE      OQTY TO QTYVAR         
         Calc      Variation=(DIFF/QTYVAR)
           Move        str18,SRDATE
           Move        "IS",SINITS
         MOVE      SLRNUM TO NSHPFLD
         CALL      NSHPTST
         GOTO      DUPE IF NOT OVER
//Write the record if it passed the test. 
//Turn on when Live
         CALL      NSHPWRT
         ADD       C1 TO WRITE

         DISPLAY   *P10:13,"RECORDS WRITTEN : ",WRITE

         CMATCH    YES TO Var
         IF        EQUAL
          MOVE      NO TO Var
          GOTO      WRITELOG
         ENDIF
           Pack   Taskname with "Record Added"
           Call   Detail       
         GOTO       WRITELOG

DUPE
//Turn on when Live
         Call      nshpupd
         ADD  C1 TO UPDATE
.          Pack Taskname with "Record Updated"
           Append "Record Updated ",Taskname
           Append "Old Ship Qty Was: ",Taskname    
           Append HoldQuant,Taskname
           Call Detail
         Goto writelog
         Goto input
VAR
           Append "Exceeds allowable quantity Variation. Sent to NIN Contact for Verification - ",Taskname
.          Pack Taskname with "Exceeds allowable quantity Variation. Sent to NIN Contact"
.          Call Detail         
         Move Yes to Faxflag
         Move Yes TO Var
//Turn on when Live
         call      sendnews
         RETURN
BADDATE
.          Pack Taskname with "Ship Date not Formatted Properly.  Record Not added."   
           Append "Ship Date not Formatted Properly.  Record Not added.",Taskname
           Move Yes,ColorFlag  
           Call Detail         
         GOTO INPUT
NOORD
.          Pack Taskname with "LR Number Not Found.  Record Not added"
           Append "LR Number Not Found.  Record Not added",Taskname
           Move Yes,ColorFlag  
           Call Detail
         GOTO INPUT
NOlive
           If (OSTAT = "B")
                    Pack Taskname with "Record Has already Been Billed.  This update will not be applied."    
                    Move Yes,ColorFlag                      
                    Call Detail
           Else
                    Pack Taskname with "Record is not Live.  This record will not be applied."      
                    Move Yes,ColorFlag                      
                    Call Detail          
           Endif
         GOTO   INPUT
         

HEADER   
          ADD       C1 TO PAGE
          Move      c0 to CellRowCnt    
          If (CurCellNum = C1)
                    Move C1 to CellRowCnt                   
                    Move CurCellNum to str5
                    Call Trim Using Str5                    
                    Pack Cell  with "S",str5                
                  setprop sheet.range(Cell),*Value=Today,*HorizontalAlignment=AlignRight  
                    Mult CurCellNum,C15,CELLPOINT
                    sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,190,60         
                    Pack Cell  with "G",str5
                    Add C1 to CurCellNum
                    Add C1 to CellRowCnt          
                    Move CurCellNum to str5
                    Call Trim Using Str5          
                    Pack Cell1 with "G",str5      
                    Pack CellRange with Cell,":",Cell1
                    sheet.range(CellRange).Merge  
                  setprop sheet.range(CellRange),*Value="Frontline Shipping Report",*HorizontalAlignment=AlignCenter                   
                  setprop sheet.range(CellRange).Font,*Bold="True"         
                  setprop sheet.range(CellRange).Font,*Name="Arial"        
                  setprop sheet.range(CellRange).Font,*Size="16"
                  setprop sheet.range(CellRange),*HorizontalAlignment=xlCenter      
                  setprop sheet.range(CellRange),*VerticalAlignment=xlCenter 
                    Add c5 to CurCellNum
                    Add C5 to CellRowCnt          
                    Move CurCellNum to str5
                    Call Trim Using Str5          
                    Pack Cell with "A",str5
                  setprop sheet.range(Cell),*Value="Confidential"
                  setprop sheet.range(Cell).Font,*Bold="True"                 
                  sheet.range(CellRange).BorderAround using *LineStyle=1,*Weight=2                  
                  pack    str11,"1:","7"
          setprop sheet.PageSetup,*PrintTitleRows=str11               
                    Add c1 to CurCellNum
                    Add c1 to CellRowCnt                    
          Endif
          Move CurCellNum to str5
          Call Trim Using Str5          
          Pack Cell with "A",str5        
          Move Cell to str8   
        Setprop sheet.range(Cell),*Value="LR ##"                   
          Pack Cell,"B",Str5        
        Setprop sheet.range(Cell),*Value="Ship Quantity",*HorizontalAlignment=AlignCenter                          
          Pack Cell,"C",Str5
        Setprop sheet.range(Cell),*Value="Order Qty",*HorizontalAlignment=AlignCenter                      
          Pack Cell,"D",Str5
        Setprop sheet.range(Cell),*Value="Variance",*HorizontalAlignment=AlignCenter              
          Pack Cell,"E",Str5
        Setprop sheet.range(Cell),*Value="Ship Cost",*HorizontalAlignment=AlignRight,*NumberFormat="0%"                 
          Pack Cell,"F",Str5
        Setprop sheet.range(Cell),*Value="Shipping Method",*HorizontalAlignment=AlignLeft
          Pack Cell,"G",Str5
        Setprop sheet.range(Cell),*Value="Description",*HorizontalAlignment=AlignLeft,*ColumnWidth=xlColumnWidthDescription        
                    
          Move Cell,str9
        Setprop sheet.range(str8,str9).Font,*Name="Arial",*Size="10",*Bold="True" 
        Setprop sheet.range(str8,str9).Borders(8),*LineStyle=1,*Weight=xlBorderWeightMedium        
        Setprop sheet.range(str8,str9).Borders(9),*LineStyle=1,*Weight=xlBorderWeightMedium         

        RETURN
BadQty

.          Pack Taskname with "Shipped Quantity is not formatted Properly.  This record will not be applied."           
           Append "Shipped Quantity is not formatted Properly.  This record will not be applied.",Taskname    
           Move Yes,ColorFlag
           Call Detail        
         GOTO INPUT

BadCost
.          Pack Taskname with "Cost is not formatted properly.  This record will not be applied."             
           Append "Cost is not formatted properly.  This record will not be applied.",Taskname
           Move Yes,ColorFlag            
           Call Detail                 
         GOTO INPUT


WRITELOG
         PACK      MKEY FROM OMLRNUM,OCOBN
         MATCH     MKEY TO HOLDMKEY
         IF        NOT EQUAL
          CALL      NMLRKEY
          MOVE      MKEY TO HOLDMKEY
         ENDIF
         MOVE      C0 TO N2
         PACK      SALES FROM OSALES10,OSALES
         MOVE      SALES TO N2
         COMPARE   C6 TO N2
         goto      wrtalph2 if equal
         COMPARE   "19" TO N2
         goto      wrtalph2 if equal
         GOTO      INPUT
WRTALPH2 
           FILEPI    3;LOGFILE
         READ      LOGFILE,NORDFLD;;
         GOTO      INPUT IF NOT OVER
//Turn on when Live         
         WRITE     LOGFILE,NORDFLD;ORDVARS
         GOTO      INPUT

EOJ      
           CLOSE     INPUT
         COMPARE   C0 TO PAGE
         If Equal
          Call SendtoIS
          Goto Done
         Endif

           Add C1 to CurCellNum
           Add C1 to CellRowCnt
           If (Page = C1)     
                    If (CellRowCnt > 54)
                              Call Header
                              Add C1 to CurCellNum
                              Add C1 to CellRowCnt                    
                                        
                    Endif
           Else
                    If (CellRowCnt > 46)
                              Call Header
                              Add C1 to CurCellNum
                              Add C1 to CellRowCnt                    
                                        
                    Endif     
           Endif
Tets       
         sheet.range("A1",CELL).Columns.Autofit              
         
           Move CurCellNum,Str5
           call trim using str5
           Pack Cell,"A",Str5
           Pack Taskname,"Number of Order Received: ",Count
         setprop sheet.range(Cell),*Value=Taskname                   
           Pack CellRange,"A1",":",Cell
           Setprop sheet.Range(CellRange),*RowHeight=xlRowHeight       
           
         RELEASE
        
.       book.printout using *ActivePrinter="\\nts0\laser2"
          setprop ex,*Visible="True"
        clear   taskname
        append  "c:\work",taskname
        append  "\",taskname
        reset   taskname
        setprop ex,*DefaultFilePath=taskname
        call    Trim using XLSNAME
        pack    taskname,taskname,XLSNAME
............................................
//Would Dislplay File Save as dialog box but for now this is automated.
.        setmode *mcursor=*arrow
.        ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
.        if (taskname <> "0")
.                movelptr taskname,N9
.                reset   taskname,N9
.                append  "xls",taskname
.                reset   taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
        book.saveas giving N9 using *Filename=taskname
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"

.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
          destroy Zoom70
        destroy sortcol
        destroy sortcol1
        destroy sheet
        destroy sheets
        destroy book
        destroy books
        Destroy exrange
          Destroy HPageBreaks
          Destroy HPageBreak
  
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        setprop ex,*DisplayAlerts=OFALSE
        setprop ex,*SheetsInNewWorkbook=SheetsDefault
        ex.quit
        destroy ex
        pack MailAttach,"c:\work\",XLSNAME
        Move "DesktopSupportGroup@nincal.com",Mailto
        Move "DesktopSupportGroup@nincal.com",MailFrom
        Move "File From Frontline shipping Program.",MAILSubjct
        Call  SendMail                 

Rename
         DISPLAY   *P1:24,*R,*EL,"COPING FILE TO .SAV",*B,*W5
           Pack       STR35,NTWKPATH1,"FrontLine_shp.sav"
         Erase      STR35
         Rename     InputName,str35
         
DONE     
        shutdown
        STOP

Detail
//Writes detail record out for shipping record
           Add C1 to CurCellNum
           Add C1 to CellRowCnt
           If (Page = C1)     
                    If (CellRowCnt > 54)
                              Call Header
                              Add C1 to CurCellNum
                              Add C1 to CellRowCnt                    
                                        
                    Endif
           Else
                    If (CellRowCnt > 46)
                              Call Header
                              Add C1 to CurCellNum
                              Add C1 to CellRowCnt                    
                                        
                    Endif     
           Endif
           Move CurCellNum,Str5
           call trim using str5
           Pack Cell,"A",Str5
           Move Cell,str8
         setprop sheet.range(Cell),*Value=SLRNUM,*HorizontalAlignment=AlignLeft                    
           Pack Cell,"B",Str5        
         setprop sheet.range(Cell),*Value=ShippedQTY,*HorizontalAlignment=AlignRight
           Pack Cell,"C",Str5
         setprop sheet.range(Cell),*Value=OQTY,*HorizontalAlignment=AlignRight
           Pack Cell,"D",Str5
         setprop sheet.range(Cell),*Value=Variation,*HorizontalAlignment=AlignRight,*NumberFormat="0%"                  
           Pack Cell,"E",Str5
         setprop sheet.range(Cell),*Value=ShippedCost,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"         
           Pack Cell,"F",Str5
           Call Trim using SINFO
         Setprop sheet.range(Cell),*Value=SINFO,*HorizontalAlignment=AlignLeft            
           Pack Cell,"G",Str5
           Move Cell,str9      
           Reset Taskname
         Setprop sheet.range(Cell),*Value=Taskname,*HorizontalAlignment=AlignLeft,*WrapText=OTRUE  
           If (ColorFlag = Yes)
          Setprop sheet.range(str8,str9).Font,*Color=RGB,*Bold="True",*Italic="True" 
          Move No to ColorFlag
         Endif

           Return

SendNews

//Subroutine sends Email to the Our Contact Person that the shipped qty does not match our order qty. They need to confirm.

        Pack    NCNTFLD,OCOCODE
        Move    "SVCREP-NCNTKEY",Location
        Pack    KeyLocation,"Key: ",NCNTFLD
        Clear   Cntname
        Clear   Str35
        Call    NCNTKEY
        Move    Cntname to str35
        Squeeze str35,str35

        Move    "This is a Informational e-mail from  the Frontline shipping program",MailSubjct
        Append  "This is a Informational e-mail from  the Frontline shipping program",MailBody
        Append  CRLF,MailBody
        
        clear   str25
        Append  "record## " to str25
        Append  olrn to str25
        Append  b1 to str25
        Reset   str25
        Append  str25,MailBody
        Append  CRLF,MailBody
        Append  " Your above LR had a shipping qty variance:",MailBody        
        Append  CRLF,MailBody
        
        Clear   str55
        Append  "Order qty " to str55
        Append  oqty to str55
        Append  ", Shipped qty ",str55
        Append   ShippedQTY,str55
        Reset    str55
        Append  str55,MailBody
        Append  CRLF,MailBody                
        Append  "Please review & correct as necessary.",MailBody                

        Clear    MailFrom  
        Append   str35 to MailFrom
        Append   "@nincal.com",MailFrom
        Reset    MailFrom
        Move     MailFrom,MailTo
          Call     SendMail
          
        winshow
        return
SendtoIs
          Move "InformationServices@nincal.com",Mailto
          Move "InformationServices@nincal.com",MailFrom
          Move "Program NSHP0011 Did not Process Any Records.",MAILSubjct
          Call  SendMail
          
          return

          Include   COMPIO.inc
          Include   CNTIO.INC
        Include NSHPIO.inc
        Include NORDIO.inc
        Include NCNTIO.inc
        Include COMLOGIC.inc

