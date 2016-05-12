..........................................................
. NDAT002A.PLS
. Converts NINDAT to a format used by NextMark
. Andrew Harkins
. July 23, 2003
..........................................................
PC        Equ       0
          include   common.inc
          include   cons.inc
          include   ndatdd.inc
          include   nrefdd.inc
          include   ncatdd.inc
          include   winapi.inc
.START PATCH 1.1 ADDED LOGIC
          include   nseldd.inc
          include   nadddd.inc
          include   nsrcdd.inc
          include   nsltdd.inc
          include   ntxtdd.inc
          include   nmoddd.inc
.END PATCH 1.1 ADDED LOGIC
.Note turned off 2014 March 10 DLH
Release    INIT    "1.87"               DLH      .Replace Reuben with Amy
Reldate   Init      "2015 May 18"
.Release    INIT    "1.86"               DLH      .Excel 2013 *WindowState=xlMinimized, and save file with correct extension for version of excel
.Reldate   Init      "2014 January 22"
.release             init      "1.85"     Remove Jack Add Reuben
.Reldate   Init      "15 September 2011"
.release             init      "1.84"     Remove Kelly add Jack
.Reldate   Init      "25 May 2010"
.release             init      "1.83"     DLH  cleanup
.Reldate   Init      "10 March 2010"
.release             init      "1.82"     DLH  rem Gayle to emails
.Reldate   Init      "2 December 2008"
.release            init      "1.81"     DLH  add Gayle to emails
.Reldate  Init      "12 May 2008"
.release            init      "1.8"     30Nov2007 DLH  Convert to sendmail add Sherene to emails
.Reldate  Init      "30 November 2007"
.release            init      "1.7"     25JAN2006 ASH  Added some format changes
.release            init      "1.6"     07JUN2005 ASH  Filter for Datacards that we do not want sent out
.release            init      "1.5"     07APR2005 ASH  COMMPER Conversion
.release            init      "1.4"     12NOV2004 ASH  Another Small Patch for SK
.release            init      "1.3"     08SEP2004 ASH  Small Patch for SK
.release            init      "1.2"     05AUG2004 ASH  LOGO CONVERSION
.release            init      "1.1"     02FEB2004 ASH  DATACARD CONVERSION
.release            init      "1.0"     23JUL2003 ASH First Release

.begin patch 1.83
ZFILE      FILE                                    
ZData      INIT      0x50,0x4B,0x5,0x6,0x0,0x0,0x0,0x0,0x0,0x0:
                    0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
*
.begin patch 1.86
.to find version of excel  DH 

#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.end patch 1.86

.Objects used       
.         
Auto      Automation          // The window Shell
SFolder   Automation          // The source folder
DFolder Automation  // The destination folder
Items     Automation          // Items within a folder
*
.Folder names
.   
SFolderN INIT       "c:\temp\source"    // Source Directory   
DFolderN init       "c:\work\List0315.zip"  // Destination Zip File
.         
DLresult  form 9
DLndx     form 9
dmFileName          dim 80
.end patch 1.83

.START PATCH 1.7 REPLACED LOGIC
.ListTable          file
ListTbl   file
.END PATCH 1.7 REPLACED LOGIC
ListSel   file
ListOut   file
ListCat   file
.START PATCH 1.1 ADDED LOGIC
ListSrc   file
ListTxt   file
ListSel2  file
.END PATCH 1.1 ADDED LOGIC
output              file
.START PATCH 1.2 REPLACED LOGIC
.NINCA              init      "Names in the News California, Inc."
NINCA               init      "Names in the News"
.END PATCH 1.2 REPLACED LOGIC
NINCAWeb  init      "WWW.NINCAL.COM"
.START PATCH 1.1 REMOVED LOGIC
.TempArray          dim       46(10)
.TempArray2         dim       3(10)
.TempSArray         dim       46(10)
.TempSArray2        dim       7(10)
.TempOArray         dim       46(10)
.TempCArray         dim       77(10)
.TempCArray2        dim       25(10)
.END PATCH 1.1 REMOVED LOGIC
str46               dim       46
b46                 init      "                                              "
startfp             form      4
endfp               form      4
CleanStr  dim       46
NetStr              dim       46
LastUpdate          form      5
.hexeight integer 4,"4294967295"

.ListTable file
books   automation
book    automation
sheets  automation
sheet  automation
ex      automation      class="Excel.Application"
.ListSel file
booksA   automation
bookA    automation
sheetsA  automation
sheetA  automation
exA      automation      class="Excel.Application"
.ListOut file
booksB   automation
bookB    automation
sheetsB  automation
sheetB  automation
exB      automation      class="Excel.Application"
.ListCat file
booksC   automation
bookC    automation
sheetsC  automation
sheetC  automation
exC      automation      class="Excel.Application"
.START PATCH 1.1 ADDED LOGIC
.ListSrc file
booksD   automation
bookD    automation
sheetsD  automation
sheetD  automation
exD      automation      class="Excel.Application"
.ListTxt file
booksE   automation
bookE    automation
sheetsE  automation
sheetE  automation
exE      automation      class="Excel.Application"
.ListSel2 file
booksF   automation
bookF    automation
sheetsF  automation
sheetF  automation
exF      automation      class="Excel.Application"
.END PATCH 1.1 ADDED LOGIC
.
RecCnt    form      9
RecCntA   form      9
RecCntB   form      9
RecCntC   form      9
.START PATCH 1.1 ADDED LOGIC
RecCntD   form      9
RecCntE   form      9
RecCntF   form      9
NewDate1 dim        10
RevDate1 dim        10
.END PATCH 1.1 ADDED LOGIC
.
.AlignLeft integer 4,"0xffffefdd"
.AlignRight integer 4,"0xffffefc8"
.AlignCenter integer 4,"0xffffeff4"
.AutoCalc integer 4,"0xffffeff7"
SheetsDefault integer 4,"0x00000000"
.xlLandscape integer 4,"0x2"                     .2
xlMinimized integer 4,"0xFFFFEFD4"
.xlNormal integer 4,"0xFFFFEFD1"
.xlMaximized integer 4,"0xFFFFEFD7"
xlUnderlineStyleSingle integer 4,"0x2"

.         goto testit
.Testing Logic
.START PATCH 1.7 REPLACED LOGIC
.         erase     "\\nins1\e\data\ListTable.dat"
          erase     "\\nins1\e\data\ListTbl.dat"
.END PATCH 1.7 REPLACED LOGIC
          erase     "\\nins1\e\data\ListSel.dat"
          erase     "\\nins1\e\data\ListOut.dat"
          erase     "\\nins1\e\data\ListCat.dat"
.START PATCH 1.1 ADDED LOGIC
          erase     "\\nins1\e\data\ListSrc.dat"
          erase     "\\nins1\e\data\ListTxt.dat"
          erase     "\\nins1\e\data\ListSel2.dat"
.END PATCH 1.1 ADDED LOGIC
          erase     "c:\work\badname.dat"
.START PATCH 1.7 REPLACED LOGIC
.         prep      ListTable,"\\nins1\e\data\ListTable.dat",EXCLUSIVE
          prep      ListTbl,"\\nins1\e\data\ListTbl.dat",EXCLUSIVE
.END PATCH 1.7 REPLACED LOGIC
          prep      ListSel,"\\nins1\e\data\ListSel.dat",EXCLUSIVE
          prep      ListOut,"\\nins1\e\data\ListOut.dat",EXCLUSIVE
          prep      ListCat,"\\nins1\e\data\ListCat.dat",EXCLUSIVE
.START PATCH 1.1 ADDED LOGIC
          prep      ListSrc,"\\nins1\e\data\ListSrc.dat",EXCLUSIVE
          prep      ListTxt,"\\nins1\e\data\ListTxt.dat",EXCLUSIVE
          prep      ListSel2,"\\nins1\e\data\ListSel2.dat",EXCLUSIVE
.END PATCH 1.1 ADDED LOGIC
          prep      OutPut,"c:\work\badname.dat",EXCLUSIVE
.Open Excel applications
.START PATCH 1.7 REPLACED LOGIC
.         erase     "\\nins1\e\data\ListTable.xls"
          erase     "\\nins1\e\data\ListTbl.xls"
.END PATCH 1.7 REPLACED LOGIC
          erase     "\\nins1\e\data\ListSel.xls"
          erase     "\\nins1\e\data\ListOut.xls"
          erase     "\\nins1\e\data\ListCat.xls"
.START PATCH 1.1 ADDED LOGIC
          erase     "\\nins1\e\data\ListSrc.xls"
          erase     "\\nins1\e\data\ListTxt.xls"
          erase     "\\nins1\e\data\ListSel2.xls"
.END PATCH 1.1 ADDED LOGIC
.
        create  ex
        create  exA
        create  exB
        create  exC
.START PATCH 1.1 ADDED LOGIC
          create  exD
          create  exE
          create  exF
.END PATCH 1.1 ADDED LOGIC
.
.begin patch 1.86
.        setprop ex,*WindowState=xlMinimized
.        setprop exA,*WindowState=xlMinimized
.        setprop exB,*WindowState=xlMinimized
.        setprop exC,*WindowState=xlMinimized
..START PATCH 1.1 ADDED LOGIC
.        setprop exD,*WindowState=xlMinimized
.        setprop exE,*WindowState=xlMinimized
.        setprop exF,*WindowState=xlMinimized
.end patch 1.86
.END PATCH 1.1 ADDED LOGIC
.
        setprop ex,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
        setprop exA,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
        setprop exB,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
        setprop exC,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
.START PATCH 1.1 ADDED LOGIC
        setprop exD,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
        setprop exE,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
        setprop exF,*Visible="True",*IgnoreRemoteRequests="True",*Interactive="False"
.END PATCH 1.1 ADDED LOGIC
.
          setprop ex.CommandBars("Standard"),*Visible="True"
          setprop exA.CommandBars("Standard"),*Visible="True"
          setprop exB.CommandBars("Standard"),*Visible="True"
          setprop exC.CommandBars("Standard"),*Visible="True"
.START PATCH 1.1 ADDED LOGIC
          setprop exD.CommandBars("Standard"),*Visible="True"
          setprop exE.CommandBars("Standard"),*Visible="True"
          setprop exF.CommandBars("Standard"),*Visible="True"
.END PATCH 1.1 ADDED LOGIC
.
          setprop ex.CommandBars("Formatting"),*Visible="True"
          setprop exA.CommandBars("Formatting"),*Visible="True"
          setprop exB.CommandBars("Formatting"),*Visible="True"
          setprop exC.CommandBars("Formatting"),*Visible="True"
.START PATCH 1.1 ADDED LOGIC
          setprop exD.CommandBars("Formatting"),*Visible="True"
          setprop exE.CommandBars("Formatting"),*Visible="True"
          setprop exF.CommandBars("Formatting"),*Visible="True"
.END PATCH 1.1 ADDED LOGIC
.
          setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
          setprop exA.CommandBars("Worksheet Menu Bar"),*Enabled="True"
          setprop exB.CommandBars("Worksheet Menu Bar"),*Enabled="True"
          setprop exC.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.START PATCH 1.1 ADDED LOGIC
          setprop exD.CommandBars("Worksheet Menu Bar"),*Enabled="True"
          setprop exE.CommandBars("Worksheet Menu Bar"),*Enabled="True"
          setprop exF.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.END PATCH 1.1 ADDED LOGIC
.Reset Default of Worksheets found in a Workbook
        getprop ex,*SheetsInNewWorkbook=SheetsDefault
        getprop exA,*SheetsInNewWorkbook=SheetsDefault
        getprop exB,*SheetsInNewWorkbook=SheetsDefault
        getprop exC,*SheetsInNewWorkbook=SheetsDefault
.START PATCH 1.1 ADDED LOGIC
        getprop exD,*SheetsInNewWorkbook=SheetsDefault
        getprop exE,*SheetsInNewWorkbook=SheetsDefault
        getprop exF,*SheetsInNewWorkbook=SheetsDefault
.END PATCH 1.1 ADDED LOGIC
.
        setprop ex,*SheetsInNewWorkbook=C1
        setprop exA,*SheetsInNewWorkbook=C1
        setprop exB,*SheetsInNewWorkbook=C1
        setprop exC,*SheetsInNewWorkbook=C1
.START PATCH 1.1 ADDED LOGIC
        setprop exD,*SheetsInNewWorkbook=C1
        setprop exE,*SheetsInNewWorkbook=C1
        setprop exF,*SheetsInNewWorkbook=C1
.END PATCH 1.1 ADDED LOGIC
.Create Workbooks collection
        getprop ex,*Workbooks=books
        getprop exA,*Workbooks=booksA
        getprop exB,*Workbooks=booksB
        getprop exC,*Workbooks=booksC
.START PATCH 1.1 ADDED LOGIC
        getprop exD,*Workbooks=booksD
        getprop exE,*Workbooks=booksE
        getprop exF,*Workbooks=booksF
.END PATCH 1.1 ADDED LOGIC
.Create/Add a single Workbook
        books.add
        booksA.add
        booksB.add
        booksC.add
.START PATCH 1.1 ADDED LOGIC
        booksD.add
        booksE.add
        booksF.add
.END PATCH 1.1 ADDED LOGIC
.
        books.item giving book using 1
        booksA.item giving bookA using 1
        booksB.item giving bookB using 1
        booksC.item giving bookC using 1
.START PATCH 1.1 ADDED LOGIC
        booksD.item giving bookD using 1
        booksE.item giving bookE using 1
        booksF.item giving bookF using 1
.END PATCH 1.1 ADDED LOGIC
.Create Worksheets collection
        getprop book,*Sheets=sheets
        getprop bookA,*Sheets=sheetsA
        getprop bookB,*Sheets=sheetsB
        getprop bookC,*Sheets=sheetsC
.START PATCH 1.1 ADDED LOGIC
        getprop bookD,*Sheets=sheetsD
        getprop bookE,*Sheets=sheetsE
        getprop bookF,*Sheets=sheetsF
.END PATCH 1.1 ADDED LOGIC
.
          sheets.item giving sheet using 1
          sheetsA.item giving sheetA using 1
          sheetsB.item giving sheetB using 1
          sheetsC.item giving sheetC using 1
.START PATCH 1.1 ADDED LOGIC
          sheetsD.item giving sheetD using 1
          sheetsE.item giving sheetE using 1
          sheetsF.item giving sheetF using 1
.END PATCH 1.1 ADDED LOGIC
.Set up Header Information for individual .XLS files
.START PATCH 1.1 REPLACED LOGIC
.        setprop sheet.range("A1"),*Value="List Number"
.        setprop sheet.range("B1"),*Value="List Name"
.        setprop sheet.range("C1"),*Value="Base Price"
.        setprop sheet.range("D1"),*Value="Universe"
.        setprop sheet.range("E1"),*Value="Revision Date"
.        setprop sheet.range("F1"),*Value="List Manager"
.        setprop sheet.range("G1"),*Value="Text"
.        setprop sheet.range("H1"),*Value="List Manager Website"
.        setprop sheet.range("I1"),*Value="Update Data"
.        setprop sheet.range("J1"),*Value="Minimum"
.        setprop sheet.range("K1"),*Value="Creation Date"
.        setprop sheet.range("L1"),*Value="SOURCE 1"
.        setprop sheet.range("M1"),*Value="SOURCE 1 %"
.        setprop sheet.range("N1"),*Value="SOURCE 2"
.        setprop sheet.range("O1"),*Value="SOURCE 2 %"
.        setprop sheet.range("P1"),*Value="SOURCE 3"
.        setprop sheet.range("Q1"),*Value="SOURCE 3 %"
.        setprop sheet.range("R1"),*Value="SOURCE 4"
.        setprop sheet.range("S1"),*Value="SOURCE 4 %"
.        setprop sheet.range("T1"),*Value="SOURCE 5"
.        setprop sheet.range("U1"),*Value="SOURCE 5 %"
.        setprop sheet.range("V1"),*Value="SOURCE 6"
.        setprop sheet.range("W1"),*Value="SOURCE 6 %"
.        setprop sheet.range("X1"),*Value="SOURCE 7"
.        setprop sheet.range("Y1"),*Value="SOURCE 7 %"
.        setprop sheet.range("Z1"),*Value="Gender"
.        setprop sheet.range("AA1"),*Value="Unit of Sale"
.        setprop sheet.range("AB1"),*Value="Net Name Data"
.        setprop sheet.range("AC1"),*Value="Exchange/Rental Data"
.        setprop sheet.range("AD1"),*Value="Commision"
.        setprop sheet.range("AE1"),*Value="Cancellation Fee"
.START PATCH 1.7 REPLACED LOGIC
.         setprop sheet.range("A1"),*Value="List Number"
.         setprop sheet.range("B1"),*Value="List Name"
.         setprop sheet.range("C1"),*Value="Base Price"
.         setprop sheet.range("D1"),*Value="Universe"
.         setprop sheet.range("E1"),*Value="Revision Date"
.         setprop sheet.range("F1"),*Value="List Manager"
.         setprop sheet.range("G1"),*Value="List Manager Website"
.         setprop sheet.range("H1"),*Value="Update Data"
.         setprop sheet.range("I1"),*Value="Minimum"
.         setprop sheet.range("J1"),*Value="Creation Date"
.         setprop sheet.range("K1"),*Value="Gender"
.         setprop sheet.range("L1"),*Value="Unit of Sale"
.         setprop sheet.range("M1"),*Value="Net Name Data"
.         setprop sheet.range("N1"),*Value="Exchange/Rental Data"
.         setprop sheet.range("O1"),*Value="Commision"
.         setprop sheet.range("P1"),*Value="Cancellation Fee"
..END PATCH 1.1 REPLACED LOGIC
..
.         setprop sheetA.range("A1"),*Value="Selection Description"
.         setprop sheetA.range("B1"),*Value="Selection Code"
.         setprop sheetA.range("C1"),*Value="List Number"
.         setprop sheetA.range("D1"),*Value="Selectable"
.         setprop sheetA.range("E1"),*Value="Price"
.         setprop sheetA.range("F1"),*Value="Currency"
.         setprop sheetA.range("G1"),*Value="Price Units"
..
.         setprop sheetB.range("A1"),*Value="List Number"
.         setprop sheetB.range("B1"),*Value="Output Code"
.         setprop sheetB.range("C1"),*Value="Output Description"
.         setprop sheetB.range("D1"),*Value="Currency"
..
.         setprop sheetC.range("A1"),*Value="List Number"
.         setprop sheetC.range("B1"),*Value="Category Code"
.         setprop sheetC.range("C1"),*Value="Category Description"
.         setprop sheetC.range("D1"),*Value="Category Type"
..START PATCH 1.1 ADDED LOGIC
.         setprop sheetD.range("A1"),*Value="List Number"
.         setprop sheetD.range("B1"),*Value="Source Code"
.         setprop sheetD.range("C1"),*Value="Source Description"
.         setprop sheetD.range("D1"),*Value="Source Percentage"
..
.         setprop sheetE.range("A1"),*Value="List Number"
.         setprop sheetE.range("B1"),*Value="Text Code"
.         setprop sheetE.range("C1"),*Value="Text"
..
.         setprop sheetF.range("A1"),*Value="List Number"
.         setprop sheetF.range("B1"),*Value="Select Number"
.         setprop sheetF.range("C1"),*Value="Select Name"
.         setprop sheetF.range("D1"),*Value="Quantity"
.         setprop sheetF.range("E1"),*Value="Price"
.         setprop sheetF.range("F1"),*Value="Price Units"
.         setprop sheetF.range("G1"),*Value="Base Info"
.         setprop sheetF.range("H1"),*Value="Notes"
.         setprop sheetF.range("I1"),*Value="Availability"
.         setprop sheetF.range("J1"),*Value="Index"
..END PATCH 1.1 ADDED LOGIC
............................................
          setprop sheet.range("A1"),*Value="LIST_ID"
          setprop sheet.range("B1"),*Value="LIST_NAME"
          setprop sheet.range("C1"),*Value="PRICE_PER_NAME"
          setprop sheet.range("D1"),*Value="NUM_RECORDS"
          setprop sheet.range("E1"),*Value="LAST_VERIFIED_DT"
          setprop sheet.range("F1"),*Value="LIST_MGR_NAME"
          setprop sheet.range("G1"),*Value="MGR_WEB_ADDRESS"
          setprop sheet.range("H1"),*Value="UPDATE_FREQUENCY"
          setprop sheet.range("I1"),*Value="MINIMUM_ORDER"
          setprop sheet.range("J1"),*Value="CREATED_DT"
          setprop sheet.range("K1"),*Value="FEMALE_PCT_MALE_PCT"
          setprop sheet.range("L1"),*Value="UNIT_SALE_YES_NO"
          setprop sheet.range("M1"),*Value="NET_NAME_PCT"
          setprop sheet.range("N1"),*Value="EXCHANGE_ALLOWED"
          setprop sheet.range("O1"),*Value="BROKER_COMMISSION"
          setprop sheet.range("P1"),*Value="Cancellation_Fee"
.
          setprop sheetA.range("A1"),*Value="SELECT_DESC"
          setprop sheetA.range("B1"),*Value="ATT_ID"
          setprop sheetA.range("C1"),*Value="LIST_ID"
          setprop sheetA.range("D1"),*Value="SELECTABLE"
          setprop sheetA.range("E1"),*Value="SELECT_FEE"
          setprop sheetA.range("F1"),*Value="SELECT_FEE_CD"
          setprop sheetA.range("G1"),*Value="SELECT_UNITS"
.
          setprop sheetB.range("A1"),*Value="LIST_ID"
          setprop sheetB.range("B1"),*Value="OUTPUT_ID"
          setprop sheetB.range("C1"),*Value="OUTPUT_MEDIA"
          setprop sheetB.range("D1"),*Value="OUTPUT_PRICE"
          setprop sheetB.range("E1"),*Value="OUTPUT_PRICE_MOD"
          setprop sheetB.range("F1"),*Value="OUTPUT_RATE_CD"
.
          setprop sheetC.range("A1"),*Value="List_ID"
          setprop sheetC.range("B1"),*Value="CATEGORY_ID"
          setprop sheetC.range("C1"),*Value="CATEGORY_DESC"
          setprop sheetC.range("D1"),*Value="CATEGORY_TYPE"
.
          setprop sheetD.range("A1"),*Value="LIST_ID"
          setprop sheetD.range("B1"),*Value="SOURCE_CODE"
          setprop sheetD.range("C1"),*Value="SOURCE_DESCRIPTION"
          setprop sheetD.range("D1"),*Value="SOURCE_PERCENTAGE"
.
          setprop sheetE.range("A1"),*Value="LIST_ID"
          setprop sheetE.range("B1"),*Value="Text_Code_seq"
          setprop sheetE.range("C1"),*Value="LIST_TEXT"
.
          setprop sheetF.range("A1"),*Value="LIST_ID"
          setprop sheetF.range("B1"),*Value="SEG_ID"
          setprop sheetF.range("C1"),*Value="SEG_DESC"
          setprop sheetF.range("D1"),*Value="SEG_UNIVERSE"
          setprop sheetF.range("E1"),*Value="SEG_RATE"
          setprop sheetF.range("F1"),*Value="SEG_UNITS"
          setprop sheetF.range("G1"),*Value="SEG_BASE_INFO"
          setprop sheetF.range("H1"),*Value="NOTES"
          setprop sheetF.range("I1"),*Value="AVAILABILITY"
          setprop sheetF.range("J1"),*Value="INDEX_ID"
.END PATCH 1.7 REPLACED LOGIC
.
          move      C1,RecCnt
          move      C1,RecCntA
          move      C1,RecCntB
          move      C1,RecCntC
.START PATCH 1.1 ADDED LOGIC
          move      C1,RecCntD
          move      C1,RecCntE
          move      C1,RecCntF
.END PATCH 1.1 ADDED LOGIC
.Establish Cut-Off for Old Datacards
          clock     timestamp,timestamp
          unpack    timestamp,CC,YY,MM,DD
          move      C0,LastUpdate
          move      C0,JULDAYS
          call      CVTJUL
.
.START PATCH 1.4 REPLACED LOGIC
.         sub       "365",JULDAYS,LastUpdate      .12 Months ago from today
          sub       "547",JULDAYS,LastUpdate      .18 Months ago from today
.END PATCH 1.4 REPLACED LOGIC
.
          move      C1,NDATPATH
          call      Paint
          loop
                    pack      Location,"NDATSEQ"
                    call      NDATSEQ
                    until over
                    add       C1,howmany
                    display    *p10:10,"records ",howmany
                    move      MLSTNAME,str55
                    rep       lowup,str55
.                   scan      "OFFICE USE ONLY",str55
                    scan      "OFFICE USE",str55
                    if not equal
                              scan      "|A",MLSTNAME
                              if equal
                                        reset     MLSTNAME
                                        write     OutPut,SEQ;LSTNUM:
                                                            MLSTNAME
                              else
                                        reset     MLSTNAME
                                        scan      "|T",MLSTNAME
                                        if equal
                                                  reset     MLSTNAME
                                                  write     OutPut,SEQ;LSTNUM:
                                                                      MLSTNAME
                                        endif
                              endif
                              reset     MLSTNAME
.START PATCH 1.3 REPLACED LOGIC
.                             if (ELSTCDE = "C" AND STATUS = " ")     .Exclusive AND Not Withdrawn
.START PATCH 1.6 REPLACED LOGIC
.                             if (ELSTCDE = "C" AND STATUS = " " AND NDATOFF <> "1")      .Exclusive AND Not Withdrawn AND Not Office Use Only
.
.Exclusive AND Not Withdrawn AND Not Office Use Only AND Not marked as "No Web Display"
                              if (ELSTCDE = "C" AND STATUS = " " AND NDATOFF <> "1" AND NDATWEB <> "1")
.END PATCH 1.6 REPLACED LOGIC
.END PATCH 1.3 REPLACED LOGIC
.Prep Values for List Table File

.START PATCH 1.1 REPLACED LOGIC
.                                       unpack    REVDATE,MM,str1,DD,str1,CC,YY
                                        unpack    REVDATE,CC,YY,MM,DD
                                        clear     REVDATE1
.END PATCH 1.1 REPLACED LOGIC
                                        move      C0,JULDAYS
                                        call      CVTJUL
                                        if (JULDAYS <> C0 & JULDAYS > LastUpdate)
.START PATCH 1.1 ADDED LOGIC
                                                  unpack    REVDATE,CC,YY,MM,DD
                                                  pack      REVDATE1,MM,SLASH,DD,SLASH,CC,YY
.END PATCH 1.1 ADDED LOGIC
                                                  clear     str6
                                                  scan      "|A",MLSTNAME
                                                  call      undofixita if equal
                                                  reset     MLSTNAME
                                                  scan      "|T",MLSTNAME
                                                  call      undofixitThe if equal
                                                  reset     MLSTNAME
.
.START PATCH 1.1 REPLACED LOGIC
.                                                 clear     str46
.                                                 movefptr TEXTDATA,startfp
.                                                 parse     TEXTDATA into str46 using " ~09",noskip,blankfill,truncate
.                                                 movefptr TEXTDATA,endfp
.                                                 move      endfp,N5
.                                                 sub       startfp,N5
.                                                 compare   "2256",startfp
..                                                goto      packtext if equal       .we have it all
..                                                if (N5 > 46)               .wordwrap, no new line char
.                                                 if not equal
.                                                           if (N5 > 46)                                      .wordwrap, no new line char
.                                                                     cmatch    B1,str46
.                                                                     if eos
.                                                                               move      B55,str46
.                                                                     endif
.                                                                     movelptr str46,N3
.                                                                     if (N3 = 0)                             .if empty blank fill
.                                                                               move      B55,str46
.                                                                     endif
.                                                           endif
.                                                 endif
..
.                                                 rep       lowup,str46
.                                                 scan      "EXCHANGE ONLY",str46
.                                                 if equal
.                                                           move      "2",str1
.                                                 else
.                                                           reset     str46
.                                                           scan      "RENTAL ONLY",str46
.                                                           if equal
.                                                                     move      "3",str1
.                                                           else
..                                                                    move      "1",str1
..                                                          endif
.                                                                     reset     str46
.                                                                     scan      "EXCH",str46                  .push text away from any Base Select Dollar Selects, and go straight to Price
.                                                                     if not equal
.                                                                               move      "3",str1
.                                                                     else
.                                                                               move      "1",str1
.                                                                     endif
.                                                                     scan      "$",str46
.                                                                     if equal
.                                                                               bump      str46
.                                                                               scan      "$",str46
..                                                                              if equal
.                                                                                         bump      str46
.                                                                                         move      str46,str6
.                                                                                         for result,"6","1",SEQ
.                                                                                                   type      str6
.                                                                                                   if equal
.                                                                                                             break
.                                                                                                   else
.                                                                                                             setlptr   str6,result
.                                                                                                   endif
.                                                                                         repeat
.                                                                               else
.                                                                                         move      str46,str6
.                                                                                         for result,"6","1",SEQ
.                                                                                                   type      str6
.                                                                                                   if equal
.                                                                                                             break
.                                                                                                   else
.                                                                                                             setlptr   str6,result
.                                                                                                   endif
.                                                                                         repeat
.                                                                               endif
.                                                                     endif
.                                                                     call      Trim using str6
.                                                                     if (str6 = "")                .No Pricing Information, clear Exchange/Rent Flag
.                                                                               clear     str1
.                                                                     endif
.                                                           endif
.                                                 endif
.                                                 reset     TEXTDATA
.                                                 clear     TempArray
.                                                 clear     TempArray2
..
.                                                 call      Trim using SCODE1
.                                                 if (SCODE1 = "")
.                                                           pack      TempArray(1),B55
.                                                           pack      TempArray2(1),B55
.                                                 else
.                                                           move      SCODE1,NREFFLD
.                                                           pack      Location,"NREFKEY1"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempArray(1),B55
.                                                                     pack      TempArray2(1),B55
.                                                           else
.                                                                     pack      TempArray(1),NREFDESC
.                                                                     pack      TempArray2(1),SCODEP1
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SCODE2
.                                                 if (SCODE2 = "")
.                                                           pack      TempArray(2),B55
.                                                           pack      TempArray2(2),B55
.                                                 else
.                                                           move      SCODE2,NREFFLD
.                                                           pack      Location,"NREFKEY2"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempArray(2),B55
.                                                                     pack      TempArray2(2),B55
.                                                           else
.                                                                     pack      TempArray(2),NREFDESC
.                                                                     pack      TempArray2(2),SCODEP2
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SCODE3
.                                                 if (SCODE3 = "")
.                                                           pack      TempArray(3),B55
.                                                           pack      TempArray2(3),B55
.                                                 else
.                                                           move      SCODE3,NREFFLD
.                                                           pack      Location,"NREFKEY3"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempArray(3),B55
.                                                                     pack      TempArray2(3),B55
.                                                           else
.                                                                     pack      TempArray(3),NREFDESC
.                                                                     pack      TempArray2(3),SCODEP3
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SCODE4
.                                                 if (SCODE4 = "")
.                                                           pack      TempArray(4),B55
.                                                           pack      TempArray2(4),B55
.                                                 else
.                                                           move      SCODE4,NREFFLD
.                                                           pack      Location,"NREFKEY4"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempArray(4),B55
.                                                                     pack      TempArray2(4),B55
.                                                           else
.                                                                     pack      TempArray(4),NREFDESC
.                                                                     pack      TempArray2(4),SCODEP4
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SCODE5
.                                                 if (SCODE5 = "")
.                                                           pack      TempArray(5),B55
.                                                           pack      TempArray2(5),B55
.                                                 else
.                                                           move      SCODE5,NREFFLD
.                                                           pack      Location,"NREFKEY5"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempArray(5),B55
.                                                                     pack      TempArray2(5),B55
.                                                           else
.                                                                     pack      TempArray(5),NREFDESC
.                                                                     pack      TempArray2(5),SCODEP5
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SCODE6
.                                                 if (SCODE6 = "")
.                                                           pack      TempArray(6),B55
.                                                           pack      TempArray2(6),B55
.                                                 else
.                                                           move      SCODE6,NREFFLD
.                                                           pack      Location,"NREFKEY6"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempArray(6),B55
.                                                                     pack      TempArray2(6),B55
.                                                           else
.                                                                     pack      TempArray(6),NREFDESC
.                                                                     pack      TempArray2(6),SCODEP6
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SCODE7
.                                                 if (SCODE7 = "")
.                                                           pack      TempArray(7),B55
.                                                           pack      TempArray2(7),B55
.                                                 else
.                                                           move      SCODE7,NREFFLD
.                                                           pack      Location,"NREFKEY7"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempArray(7),B55
.                                                                     pack      TempArray2(7),B55
.                                                           else
.                                                                     pack      TempArray(7),NREFDESC
.                                                                     pack      TempArray2(7),SCODEP7
.                                                           endif
.                                                 endif
................................................................................
                                                  clear     str1
                                                  clear     str6
                                                  if (NDATCONV = "1")
.                                                           if (NDATEXCH <> "1")
                                                                      pack      NSELFLD1,"01X",LSTNUM
                                                                      pack      NSELFLD2,"021XBASE"
                                                                      move      "NSELAIM",Location
                                                                      pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                                                                      call      NSELAIM
                                                                      if not over
                                                                                if (NSELEXC = "2")  .1 = Exc/Rent, 2 = Exchange Only, 3 = Rental Only
                                                                                          move      "2",str1
                                                                                else
                                                                                          if (NSELEXC = "3")
                                                                                                    move      "3",str1
                                                                                          else
                                                                                                    move      "1",str1
                                                                                          endif
                                                                                          if (NSELPRICE > 0)
                                                                                                    move      NSELPRICE,str6
                                                                                          else      .No Pricing Information, clear Exchange/Rent Flag
                                                                                                    clear     str1
                                                                                          endif
                                                                                endif
                                                                      else
                                                                                goto DataCheckText
                                                                      endif
.                                                           else
.                                                                     move      "2",str1
.                                                           endif
                                                  else
DataCheckText
                                                            pack      NTXTFLD,LSTNUM,"1"
                                                            move      "D.Load-NTXTKEY",Location
                                                            pack      KeyLocation,"Key: ",NTXTFLD
                                                            call      NTXTKEY
                                                            if not over
                                                                      clear     str46
                                                                      parse     NTXTTEXT into str46 using " ~09",noskip,blankfill,truncate
                                                                      movefptr NTXTTEXT,endfp
                                                                      move      endfp,N5
                                                                      sub       startfp,N5
                                                                      compare   "2256",startfp
                                                                      if not equal
                                                                                if (N5 > 46)                                      .wordwrap, no new line char
                                                                                          cmatch    B1,str46
                                                                                          if eos
                                                                                                    move      B55,str46
                                                                                          endif
                                                                                          movelptr str46,N3
                                                                                          if (N3 = 0)                             .if empty blank fill
                                                                                                    move      B55,str46
                                                                                          endif
                                                                                endif
                                                                      endif
.
                                                                      rep       lowup,str46
                                                                      scan      "EXCHANGE ONLY",str46
                                                                      if equal
                                                                                move      "2",str1
                                                                      else
                                                                                reset     str46
                                                                                scan      "RENTAL ONLY",str46
                                                                                if equal
                                                                                          move      "3",str1
                                                                                else
                                                                                          reset     str46
                                                                                          scan      "EXCH",str46                  .push text away from any Base Select Dollar Selects, and go straight to Price
                                                                                          if not equal
                                                                                                    move      "3",str1
                                                                                          else
                                                                                                    move      "1",str1
                                                                                          endif
                                                                                          scan      "$",str46
                                                                                          if equal
                                                                                                    bump      str46
                                                                                                    scan      "$",str46
                                                                                                    if equal
                                                                                                              bump      str46
                                                                                                              move      str46,str6
                                                                                                              for result,"6","1",SEQ
                                                                                                                        type      str6
                                                                                                                        if equal
                                                                                                                                  break
                                                                                                                        else
                                                                                                                                  setlptr   str6,result
                                                                                                                        endif
                                                                                                              repeat
                                                                                                    else
                                                                                                              move      str46,str6
                                                                                                              for result,"6","1",SEQ
                                                                                                                        type      str6
                                                                                                                        if equal
                                                                                                                                  break
                                                                                                                        else
                                                                                                                                  setlptr   str6,result
                                                                                                                        endif
                                                                                                              repeat
                                                                                                    endif
                                                                                          endif
                                                                                          call      Trim using str6
                                                                                          if (str6 = "")                .No Pricing Information, clear Exchange/Rent Flag
                                                                                                    clear     str1
                                                                                          endif
                                                                                endif
                                                                      endif
                                                            endif
                                                  endif
.END PATCH 1.1 REPLACED LOGIC
.
                                                  call      Trim using CLEANCDE
                                            if (CLEANCDE = "")
                                                            clear     CleanStr
.START PATCH 1.1 REPLACED LOGIC
.                                                 elseif (CLEANCDE = "C02")
                                                  elseif (CLEANCDE = "C002")
.END PATCH 1.1 REPLACED LOGIC
                                                    move    CLNINFO,CleanStr
                                                  else
                                                            move      CLEANCDE,NREFFLD
                                                            pack      Location,"NREFKEY8"
                                                            pack      KeyLocation,"Key: ",NREFFLD
                                                            call      NREFKEY
                                                            if over
                                                                      clear     CleanStr
                                                            else
                                                                      move      NREFDESC,CleanStr
                                                            endif
                                                  endif
.
                                                  call      Trim using NETNAME
                                                  if (NETNAME = "")
                                                            clear     NetStr
.START PATCH 1.1 REPLACED LOGIC
.                                                 elseif (NETNAME = "N02")
                                                  elseif (NETNAME = "N002")
.END PATCH 1.1 REPLACED LOGIC
                                                            move      NETINFO,NetStr
                                                  else
                                                            move      NETNAME,NREFFLD
                                                            pack      Location,"NREFKEY9"
                                                            pack      KeyLocation,"Key: ",NREFFLD
                                                            call      NREFKEY
                                                            if over
                                                                      clear     NetStr
                                                            else
                                                                      move      NREFDESC,NetStr
                                                            endif
                                                  endif
.
                                                  type      NEWDATE
                                                  if not equal
                                                            clear     NEWDATE1
.START PATCH 1.1 ADDED LOGIC
                                                  else
                                                            unpack    NEWDATE,CC,YY,MM,DD
                                                            pack      NEWDATE1,MM,SLASH,DD,SLASH,CC,YY
.END PATCH 1.1 ADDED LOGIC
                                                  endif
.Write to List Table File
.START PATCH 1.1 REPLACED LOGIC
.                                                 write     ListTable,SEQ;LSTNUM:
.                                                                     MLSTNAME:
.                                                                     str6:
.                                                                     UNIVERSE:
.                                                                     REVDATE:
.                                                                     NINCA:
.                                                                     TEXTData:
.                                                                     NINCAWeb:
.                                                                     CleanStr:
.                                                                     MIN:
.                                                                     NEWDATE:
.                                                                     TempArray(1):
.                                                                     TempArray2(1):
.                                                                     TempArray(2):
.                                                                     TempArray2(2):
.                                                                     TempArray(3):
.                                                                     TempArray2(3):
.                                                                     TempArray(4):
.                                                                     TempArray2(4):
.                                                                     TempArray(5):
.                                                                     TempArray2(5):
.                                                                     TempArray(6):
.                                                                     TempArray2(6):
.                                                                     TempArray(7):
.                                                                     TempArray2(7):
.                                                                     SEX:
.                                                                     UNITDATA:
.                                                                     NetStr:
.                                                                     str1:
.                                                                     COMMPER:
.                                                                     "75"
.START PATCH 1.7 REPLACED LOGIC
.                                                 write     ListTable,SEQ;LSTNUM:
.                                                                     MLSTNAME:
.                                                                     str6:
.                                                                     UNIVERSE:
.                                                                     REVDATE1:
.                                                                     NINCA:
.                                                                     NINCAWeb:
.                                                                     CleanStr:
.                                                                     MIN:
.                                                                     NEWDATE1:
.                                                                     SEX:
.                                                                     UNITDATA:
.                                                                     NetStr:
.                                                                     str1:
.                                                                     COMMPER:
.                                                                     "75"
                                                  write     ListTbl,SEQ;LSTNUM:
                                                                      MLSTNAME:
                                                                      str6:
                                                                      UNIVERSE:
                                                                      REVDATE1:
                                                                      NINCA:
                                                                      NINCAWeb:
                                                                      CleanStr:
                                                                      MIN:
                                                                      NEWDATE1:
                                                                      SEX:
                                                                      UNITDATA:
                                                                      NetStr:
                                                                      str1:
                                                                      COMMPER:
                                                                      "75"
.END PATCH 1.7 REPLACED LOGIC
.END PATCH 1.1 REPLACED LOGIC
.Write to Excel File
                                                  add       C1,RecCnt
                                                  move      RecCnt,str9
                                                  call      Trim using str9
.START PATCH 1.1 REPLACED LOGIC
.                                                 pack      str15,"A",str9
.                                                 setprop sheet.range(str15),*Value=LSTNUM
.                                                 pack      str15,"B",str9
.                                                 setprop sheet.range(str15),*Value=MLSTNAME
.                                                 pack      str15,"C",str9
.                                                 setprop sheet.range(str15),*Value=str6
.                                                 pack      str15,"D",str9
.                                                 setprop sheet.range(str15),*Value=UNIVERSE
.                                                 pack      str15,"E",str9
.                                                 setprop sheet.range(str15),*Value=REVDATE
.                                                 pack      str15,"F",str9
.                                                 setprop sheet.range(str15),*Value=NINCA
.                                                 pack      str15,"G",str9
.                                                 setprop sheet.range(str15),*Value=TEXTData
.                                                 pack      str15,"H",str9
.                                                 setprop sheet.range(str15),*Value=NINCAWeb
.                                                 pack      str15,"I",str9
.                                                 setprop sheet.range(str15),*Value=CleanStr
.                                                 pack      str15,"J",str9
.                                                 setprop sheet.range(str15),*Value=MIN
.                                                 pack      str15,"K",str9
.                                                 setprop sheet.range(str15),*Value=NEWDATE
.                                                 pack      str15,"L",str9
.                                                 setprop sheet.range(str15),*Value=TempArray(1)
.                                                 pack      str15,"M",str9
.                                                 setprop sheet.range(str15),*Value=TempArray2(1)
.                                                 pack      str15,"N",str9
.                                                 setprop sheet.range(str15),*Value=TempArray(2)
.                                                 pack      str15,"O",str9
.                                                 setprop sheet.range(str15),*Value=TempArray2(2)
.                                                 pack      str15,"P",str9
.                                                 setprop sheet.range(str15),*Value=TempArray(3)
.                                                 pack      str15,"Q",str9
.                                                 setprop sheet.range(str15),*Value=TempArray2(3)
.                                                 pack      str15,"R",str9
.                                                 setprop sheet.range(str15),*Value=TempArray(4)
.                                                 pack      str15,"S",str9
.                                                 setprop sheet.range(str15),*Value=TempArray2(4)
.                                                 pack      str15,"T",str9
.                                                 setprop sheet.range(str15),*Value=TempArray(5)
.                                                 pack      str15,"U",str9
.                                                 setprop sheet.range(str15),*Value=TempArray2(5)
.                                                 pack      str15,"V",str9
.                                                 setprop sheet.range(str15),*Value=TempArray(6)
.                                                 pack      str15,"W",str9
.                                                 setprop sheet.range(str15),*Value=TempArray2(6)
.                                                 pack      str15,"X",str9
.                                                 setprop sheet.range(str15),*Value=TempArray(7)
.                                                 pack      str15,"Y",str9
.                                                 setprop sheet.range(str15),*Value=TempArray2(7)
.                                                 pack      str15,"Z",str9
.                                                 setprop sheet.range(str15),*Value=SEX
.                                                 pack      str15,"AA",str9
.                                                 setprop sheet.range(str15),*Value=UNITDATA
.                                                 pack      str15,"AB",str9
.                                                 setprop sheet.range(str15),*Value=NetStr
.                                                 pack      str15,"AC",str9
.                                                 setprop sheet.range(str15),*Value=str1
.                                                 pack      str15,"AD",str9
.                                                 setprop sheet.range(str15),*Value=COMMPER
.                                                 pack      str15,"AE",str9
.                                                 setprop sheet.range(str15),*Value="75.00"
.Prep Values for Selection File
.                                                 clear     TempSArray
.                                                 clear     TempSArray2
..
.                                                 call      Trim using SELCDE1
.                                                 if (SELCDE1 = "")
.                                                           pack      TempSArray(1),B55
.                                                           pack      TempSArray2(1),B55
.                                                 else
.                                                           move      SELCDE1,NREFFLD
.                                                           pack      Location,"NREFKEY11"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempSArray(1),B55
.                                                                     pack      TempSArray2(1),B55
.                                                           else
.                                                                     pack      TempSArray(1),NREFDESC
.                                                                     pack      TempSArray2(1),SEL1M
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SELCDE2
.                                                 if (SELCDE2 = "")
.                                                           pack      TempSArray(2),B55
.                                                           pack      TempSArray2(2),B55
.                                                 else
.                                                           move      SELCDE2,NREFFLD
.                                                           pack      Location,"NREFKEY12"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempSArray(2),B55
.                                                                     pack      TempSArray2(2),B55
.                                                           else
.                                                                     pack      TempSArray(2),NREFDESC
.                                                                     pack      TempSArray2(2),SEL2M
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SELCDE3
.                                                 if (SELCDE3 = "")
.                                                           pack      TempSArray(3),B55
.                                                           pack      TempSArray2(3),B55
.                                                 else
.                                                           move      SELCDE3,NREFFLD
.                                                           pack      Location,"NREFKEY13"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempSArray(3),B55
.                                                                     pack      TempSArray2(3),B55
.                                                           else
.                                                                     pack      TempSArray(3),NREFDESC
.                                                                     pack      TempSArray2(3),SEL3M
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SELCDE4
.                                                 if (SELCDE4 = "")
.                                                           pack      TempSArray(4),B55
.                                                           pack      TempSArray2(4),B55
.                                                 else
.                                                           move      SELCDE4,NREFFLD
.                                                           pack      Location,"NREFKEY14"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempSArray(4),B55
.                                                                     pack      TempSArray2(4),B55
.                                                           else
.                                                                     pack      TempSArray(4),NREFDESC
.                                                                     pack      TempSArray2(4),SEL4M
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SELCDE5
.                                                 if (SELCDE5 = "")
.                                                           pack      TempSArray(5),B55
.                                                           pack      TempSArray2(5),B55
.                                                 else
.                                                           move      SELCDE5,NREFFLD
.                                                           pack      Location,"NREFKEY15"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempSArray(5),B55
.                                                                     pack      TempSArray2(5),B55
.                                                           else
.                                                                     pack      TempSArray(5),NREFDESC
.                                                                     pack      TempSArray2(5),SEL5M
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SELCDE6
.                                                 if (SELCDE6 = "")
.                                                           pack      TempSArray(6),B55
.                                                           pack      TempSArray2(6),B55
.                                                 else
.                                                           move      SELCDE6,NREFFLD
.                                                           pack      Location,"NREFKEY16"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempSArray(6),B55
.                                                                     pack      TempSArray2(6),B55
.                                                           else
.                                                                     pack      TempSArray(6),NREFDESC
.                                                                     pack      TempSArray2(6),SEL6M
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SELCDE7
.                                                 if (SELCDE7 = "")
.                                                           pack      TempSArray(7),B55
.                                                           pack      TempSArray2(7),B55
.                                                 else
.                                                           move      SELCDE7,NREFFLD
.                                                           pack      Location,"NREFKEY17"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempSArray(7),B55
.                                                                     pack      TempSArray2(7),B55
.                                                           else
.                                                                     pack      TempSArray(7),NREFDESC
.                                                                     pack      TempSArray2(7),SEL7M
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SELCDE8
.                                                 if (SELCDE8 = "")
.                                                           pack      TempSArray(8),B55
.                                                           pack      TempSArray2(8),B55
.                                                 else
.                                                           move      SELCDE8,NREFFLD
.                                                           pack      Location,"NREFKEY18"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempSArray(8),B55
.                                                                     pack      TempSArray2(8),B55
.                                                           else
.                                                                     pack      TempSArray(8),NREFDESC
.                                                                     pack      TempSArray2(8),SEL8M
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SELCDE9
.                                                 if (SELCDE9 = "")
.                                                           pack      TempSArray(9),B55
.                                                           pack      TempSArray2(9),B55
.                                                 else
.                                                           move      SELCDE9,NREFFLD
.                                                           pack      Location,"NREFKEY19"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempSArray(9),B55
.                                                                     pack      TempSArray2(9),B55
.                                                           else
.                                                                     pack      TempSArray(9),NREFDESC
.                                                                     pack      TempSArray2(9),SEL9M
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using SELCDE10
.                                                 if (SELCDE10 = "")
.                                                           pack      TempSArray(10),B55
.                                                           pack      TempSArray2(10),B55
.                                                 else
.                                                           move      SELCDE10,NREFFLD
.                                                           pack      Location,"NREFKEY10"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempSArray(10),B55
.                                                                     pack      TempSArray2(10),B55
.                                                           else
.                                                                     pack      TempSArray(10),NREFDESC
.                                                                     pack      TempSArray2(10),SEL10M
.                                                           endif
.                                                 endif
..Write to Selection File
.                                                 for N2,"1","10"
.                                                           call      Trim using TempSArray(N2)
.                                                           if (TempSArray(N2) <> "")
.                                                                     move      N2,str2
.                                                                     rep       zfill,str2
.                                                                     write     ListSel,SEQ;TempSArray(N2):
.                                                                                         str2:
.                                                                                         LSTNUM:
.                                                                                         "Y":
.                                                                                         TempSArray2(N2):
.                                                                                         "USD":
.                                                                                         "M"
..Write to .XLS file
.                                                                     add       C1,RecCntA
.                                                                     move      RecCntA,str9
.                                                                     call      Trim using str9
.                                                                     pack      str15,"A",str9
.                                                                     setprop sheetA.range(str15),*Value=TempSArray(N2)
.                                                                     pack      str15,"B",str9
.                                                                     setprop sheetA.range(str15),*Value=str2
.                                                                     pack      str15,"C",str9
.                                                                     setprop sheetA.range(str15),*Value=LSTNUM
.                                                                     pack      str15,"D",str9
.                                                                     setprop sheetA.range(str15),*Value="Y"
.                                                                     pack      str15,"E",str9
.                                                                     setprop sheetA.range(str15),*Value=TempSArray2(N2)
.                                                                     pack      str15,"F",str9
.                                                                     setprop sheetA.range(str15),*Value="USD"
.                                                                     pack      str15,"G",str9
.                                                                     setprop sheetA.range(str15),*Value="M"
.                                                           endif
.                                                 repeat
..Prep Values for OutPut File
.                                                 clear     TempOArray
..
.                                                 call      Trim using ADDCDE1
.                                                 if (ADDCDE1 = "")
.                                                           pack      TempOArray(1),B55
.                                                 else
.                                                           move      ADDCDE1,NREFFLD
.                                                           pack      Location,"NREFKEY21"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempOArray(1),B55
.                                                           else
.                                                                     pack      TempOArray(1),NREFDESC
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using ADDCDE2
.                                                 if (ADDCDE2 = "")
.                                                           pack      TempOArray(2),B55
.                                                 else
.                                                           move      ADDCDE2,NREFFLD
.                                                           pack      Location,"NREFKEY22"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempOArray(2),B55
.                                                           else
.                                                                     pack      TempOArray(2),NREFDESC
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using ADDCDE3
.                                                 if (ADDCDE3 = "")
.                                                           pack      TempOArray(3),B55
.                                                 else
.                                                           move      ADDCDE3,NREFFLD
.                                                           pack      Location,"NREFKEY23"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempOArray(3),B55
.                                                           else
.                                                                     pack      TempOArray(3),NREFDESC
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using ADDCDE4
.                                                 if (ADDCDE4 = "")
.                                                           pack      TempOArray(4),B55
.                                                 else
.                                                           move      ADDCDE4,NREFFLD
.                                                           pack      Location,"NREFKEY24"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempOArray(4),B55
.                                                           else
.                                                                     pack      TempOArray(4),NREFDESC
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using ADDCDE5
.                                                 if (ADDCDE5 = "")
.                                                           pack      TempOArray(5),B55
.                                                 else
.                                                           move      ADDCDE5,NREFFLD
.                                                           pack      Location,"NREFKEY25"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempOArray(5),B55
.                                                           else
.                                                                     pack      TempOArray(5),NREFDESC
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using ADDCDE6
.                                                 if (ADDCDE6 = "")
.                                                           pack      TempOArray(6),B55
.                                                 else
.                                                           move      ADDCDE6,NREFFLD
.                                                           pack      Location,"NREFKEY26"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempOArray(6),B55
.                                                           else
.                                                                     pack      TempOArray(6),NREFDESC
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using ADDCDE7
.                                                 if (ADDCDE7 = "")
.                                                           pack      TempOArray(7),B55
.                                                 else
.                                                           move      ADDCDE7,NREFFLD
.                                                           pack      Location,"NREFKEY27"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempOArray(7),B55
.                                                           else
.                                                                     pack      TempOArray(7),NREFDESC
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using MAGSPEC1
.                                                 if (MAGSPEC1 = "")
.                                                           pack      TempOArray(8),B55
.                                                 elseif (MAGSPEC1 <> "M00")
.                                                           move      MAGSPEC1,NREFFLD
.                                                           pack      Location,"NREFKEY28"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempOArray(8),B55
.                                                           else
.                                                                     pack      TempOArray(8),NREFDESC
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using MAGSPEC2
.                                                 if (MAGSPEC2 = "")
.                                                           pack      TempOArray(9),B55
.                                                 elseif (MAGSPEC2 <> "M00")
.                                                           move      MAGSPEC2,NREFFLD
.                                                           pack      Location,"NREFKEY29"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempOArray(9),B55
.                                                           else
.                                                                     pack      TempOArray(9),NREFDESC
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using MAGSPEC3
.                                                 if (MAGSPEC3 = "")
.                                                           pack      TempOArray(10),B55
.                                                 elseif (MAGSPEC3 <> "M00")
.                                                           move      MAGSPEC3,NREFFLD
.                                                           pack      Location,"NREFKEY30"
.                                                           pack      KeyLocation,"Key: ",NREFFLD
.                                                           call      NREFKEY
.                                                           if over
.                                                                     pack      TempOArray(10),B55
.                                                           else
.                                                                     pack      TempOArray(10),NREFDESC
.                                                           endif
.                                                 endif
..Write to Output File
.                                                 for N2,"1","10"
.                                                           call      Trim using TempOArray(N2)
.                                                           if (TempOArray(N2) <> "")
.                                                                     move      N2,str2
.                                                                     rep       zfill,str2
.                                                                     write     ListOut,SEQ;LSTNUM:
.                                                                                         str2:
.                                                                                         TempOArray(N2):
.                                                                                         "USD"
.                                                                     add       C1,RecCntB
.                                                                     move      RecCntB,str9
.                                                                     call      Trim using str9
.                                                                     pack      str15,"A",str9
.                                                                     setprop sheetB.range(str15),*Value=LSTNUM
.                                                                     pack      str15,"B",str9
.                                                                     setprop sheetB.range(str15),*Value=str2
.                                                                     pack      str15,"C",str9
.                                                                     setprop sheetB.range(str15),*Value=TempOArray(N2)
.                                                                     pack      str15,"D",str9
.                                                                     setprop sheetB.range(str15),*Value="USD"
.                                                           endif
.                                                 repeat
..Prep Values for Category File
.                                                 clear     TempCArray
.                                                 clear     TempCArray2
..
.                                                 call      Trim using CATCDE1
.                                                 if (CATCDE1 = "")
.                                                           pack      TempCArray(1),B55,B55
.                                                           pack      TempCArray2(1),B55
.                                                 else
.                                                           clear     NCATFLD
.                                                           move      CATCDE1,NCATFLD
.                                                           pack      Location,"NCATKEY1"
.                                                           pack      KeyLocation,"Key: ",NCATFLD
.                                                           call      NCATKEY
.                                                           if over
.                                                                     pack      TempCArray(1),B55,B55
.                                                                     pack      TempCArray2(1),B55
.                                                           else
.                                                                     pack      TempCArray(1),NCATDESC
.                                                                     unpack    CATCDE1,str1
.                                                                     if (str1 = "B")
.                                                                               pack      TempCArray2(1),"Business"
.                                                                     elseif (str1 = "C")
.                                                                               pack      TempCArray2(1),"Consumer"
.                                                                     elseif (str1 = "E")
.                                                                               pack      TempCArray2(1),"Enhanced"
.                                                                     else
.                                                                               pack      TempCArray2(1),B55
.                                                                     endif
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using CATCDE2
.                                                 if (CATCDE2 = "")
.                                                           pack      TempCArray(2),B55,B55
.                                                           pack      TempCArray2(2),B55
.                                                 else
.                                                           clear     NCATFLD
.                                                           move      CATCDE2,NCATFLD
.                                                           pack      Location,"NCATKEY2"
.                                                           pack      KeyLocation,"Key: ",NCATFLD
.                                                           call      NCATKEY
.                                                           if over
.                                                                     pack      TempCArray(2),B55,B55
.                                                                     pack      TempCArray2(2),B55
.                                                           else
.                                                                     pack      TempCArray(2),NCATDESC
.                                                                     unpack    CATCDE2,str1
.                                                                     if (str1 = "B")
.                                                                               pack      TempCArray2(2),"Business"
.                                                                     elseif (str1 = "C")
.                                                                               pack      TempCArray2(2),"Consumer"
.                                                                     elseif (str1 = "E")
.                                                                               pack      TempCArray2(2),"Enhanced"
.                                                                     else
.                                                                               pack      TempCArray2(2),B55
.                                                                     endif
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using CATCDE3
.                                                 if (CATCDE3 = "")
.                                                           pack      TempCArray(3),B55,B55
.                                                           pack      TempCArray2(3),B55
.                                                 else
.                                                           clear     NCATFLD
.                                                           move      CATCDE3,NCATFLD
.                                                           pack      Location,"NCATKEY3"
.                                                           pack      KeyLocation,"Key: ",NCATFLD
.                                                           call      NCATKEY
.                                                           if over
.                                                                     pack      TempCArray(3),B55,B55
.                                                                     pack      TempCArray2(3),B55
.                                                           else
.                                                                     pack      TempCArray(3),NCATDESC
.                                                                     unpack    CATCDE3,str1
.                                                                     if (str1 = "B")
.                                                                               pack      TempCArray2(3),"Business"
.                                                                     elseif (str1 = "C")
.                                                                               pack      TempCArray2(3),"Consumer"
.                                                                     elseif (str1 = "E")
.                                                                               pack      TempCArray2(3),"Enhanced"
.                                                                     else
.                                                                               pack      TempCArray2(3),B55
.                                                                     endif
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using CATCDE4
.                                                 if (CATCDE4 = "")
.                                                           pack      TempCArray(4),B55,B55
.                                                           pack      TempCArray2(4),B55
.                                                 else
.                                                           clear     NCATFLD
.                                                           move      CATCDE4,NCATFLD
.                                                           pack      Location,"NCATKEY4"
.                                                           pack      KeyLocation,"Key: ",NCATFLD
.                                                           call      NCATKEY
.                                                           if over
.                                                                     pack      TempCArray(4),B55,B55
.                                                                     pack      TempCArray2(4),B55
.                                                           else
.                                                                     pack      TempCArray(4),NCATDESC
.                                                                     unpack    CATCDE4,str1
.                                                                     if (str1 = "B")
.                                                                               pack      TempCArray2(4),"Business"
.                                                                     elseif (str1 = "C")
.                                                                               pack      TempCArray2(4),"Consumer"
.                                                                     elseif (str1 = "E")
.                                                                               pack      TempCArray2(4),"Enhanced"
.                                                                     else
.                                                                               pack      TempCArray2(4),B55
.                                                                     endif
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using CATCDE5
.                                                 if (CATCDE5 = "")
.                                                           pack      TempCArray(5),B55,B55
.                                                           pack      TempCArray2(5),B55
.                                                 else
.                                                           clear     NCATFLD
.                                                           move      CATCDE5,NCATFLD
.                                                           pack      Location,"NCATKEY5"
.                                                           pack      KeyLocation,"Key: ",NCATFLD
.                                                           call      NCATKEY
.                                                           if over
.                                                                     pack      TempCArray(5),B55,B55
.                                                                     pack      TempCArray2(5),B55
.                                                           else
.                                                                     pack      TempCArray(5),NCATDESC
.                                                                     unpack    CATCDE5,str1
.                                                                     if (str1 = "B")
.                                                                               pack      TempCArray2(5),"Business"
.                                                                     elseif (str1 = "C")
.                                                                               pack      TempCArray2(5),"Consumer"
.                                                                     elseif (str1 = "E")
.                                                                               pack      TempCArray2(5),"Enhanced"
.                                                                     else
.                                                                               pack      TempCArray2(5),B55
.                                                                     endif
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using CATCDE6
.                                                 if (CATCDE6 = "")
.                                                           pack      TempCArray(6),B55,B55
.                                                           pack      TempCArray2(6),B55
.                                                 else
.                                                           clear     NCATFLD
.                                                           move      CATCDE6,NCATFLD
.                                                           pack      Location,"NCATKEY6"
.                                                           pack      KeyLocation,"Key: ",NCATFLD
.                                                           call      NCATKEY
.                                                           if over
.                                                                     pack      TempCArray(6),B55,B55
.                                                                     pack      TempCArray2(6),B55
.                                                           else
.                                                                     pack      TempCArray(6),NCATDESC
.                                                                     unpack    CATCDE6,str1
.                                                                     if (str1 = "B")
.                                                                               pack      TempCArray2(6),"Business"
.                                                                     elseif (str1 = "C")
.                                                                               pack      TempCArray2(6),"Consumer"
.                                                                     elseif (str1 = "E")
.                                                                               pack      TempCArray2(6),"Enhanced"
.                                                                     else
.                                                                               pack      TempCArray2(6),B55
.                                                                     endif
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using CATCDE7
.                                                 if (CATCDE7 = "")
.                                                           pack      TempCArray(7),B55,B55
.                                                           pack      TempCArray2(7),B55
.                                                 else
.                                                           clear     NCATFLD
.                                                           move      CATCDE7,NCATFLD
.                                                           pack      Location,"NCATKEY7"
.                                                           pack      KeyLocation,"Key: ",NCATFLD
.                                                           call      NCATKEY
.                                                           if over
.                                                                     pack      TempCArray(7),B55,B55
.                                                                     pack      TempCArray2(7),B55
.                                                           else
.                                                                     pack      TempCArray(7),NCATDESC
.                                                                     unpack    CATCDE7,str1
.                                                                     if (str1 = "B")
.                                                                               pack      TempCArray2(7),"Business"
.                                                                     elseif (str1 = "C")
.                                                                               pack      TempCArray2(7),"Consumer"
.                                                                     elseif (str1 = "E")
.                                                                               pack      TempCArray2(7),"Enhanced"
.                                                                     else
.                                                                               pack      TempCArray2(7),B55
.                                                                     endif
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using CATCDE8
.                                                 if (CATCDE8 = "")
.                                                           pack      TempCArray(8),B55,B55
.                                                           pack      TempCArray2(8),B55
.                                                 else
.                                                           clear     NCATFLD
.                                                           move      CATCDE8,NCATFLD
.                                                           pack      Location,"NCATKEY8"
.                                                           pack      KeyLocation,"Key: ",NCATFLD
.                                                           call      NCATKEY
.                                                           if over
.                                                                     pack      TempCArray(8),B55,B55
.                                                                     pack      TempCArray2(8),B55
.                                                           else
.                                                                     pack      TempCArray(8),NCATDESC
.                                                                     unpack    CATCDE8,str1
.                                                                     if (str1 = "B")
.                                                                               pack      TempCArray2(8),"Business"
.                                                                     elseif (str1 = "C")
.                                                                               pack      TempCArray2(8),"Consumer"
.                                                                     elseif (str1 = "E")
.                                                                               pack      TempCArray2(8),"Enhanced"
.                                                                     else
.                                                                               pack      TempCArray2(8),B55
.                                                                     endif
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using CATCDE9
.                                                 if (CATCDE9 = "")
.                                                           pack      TempCArray(9),B55,B55
.                                                           pack      TempCArray2(9),B55
.                                                 else
.                                                           clear     NCATFLD
.                                                           move      CATCDE9,NCATFLD
.                                                           pack      Location,"NCATKEY9"
.                                                           pack      KeyLocation,"Key: ",NCATFLD
.                                                           call      NCATKEY
.                                                           if over
.                                                                     pack      TempCArray(9),B55,B55
.                                                                     pack      TempCArray2(9),B55
.                                                           else
.                                                                     pack      TempCArray(9),NCATDESC
.                                                                     unpack    CATCDE9,str1
.                                                                     if (str1 = "B")
.                                                                               pack      TempCArray2(9),"Business"
.                                                                     elseif (str1 = "C")
.                                                                               pack      TempCArray2(9),"Consumer"
.                                                                     elseif (str1 = "E")
.                                                                               pack      TempCArray2(9),"Enhanced"
.                                                                     else
.                                                                               pack      TempCArray2(9),B55
.                                                                     endif
.                                                           endif
.                                                 endif
..
.                                                 call      Trim using CATCDE10
.                                                 if (CATCDE10 = "")
.                                                           pack      TempCArray(10),B55,B55
.                                                           pack      TempCArray2(10),B55
.                                                 else
.                                                           clear     NCATFLD
.                                                           move      CATCDE10,NCATFLD
.                                                           pack      Location,"NCATKEY10"
.                                                           pack      KeyLocation,"Key: ",NCATFLD
.                                                           call      NCATKEY
.                                                           if over
.                                                                     pack      TempCArray(10),B55,B55
.                                                                     pack      TempCArray2(10),B55
.                                                           else
.                                                                     pack      TempCArray(10),NCATDESC
.                                                                     unpack    CATCDE10,str1
.                                                                     if (str1 = "B")
.                                                                               pack      TempCArray2(10),"Business"
.                                                                     elseif (str1 = "C")
.                                                                               pack      TempCArray2(10),"Consumer"
.                                                                     elseif (str1 = "E")
.                                                                               pack      TempCArray2(10),"Enhanced"
.                                                                     else
.                                                                               pack      TempCArray2(10),B55
.                                                                     endif
.                                                           endif
.                                                 endif
..Write to Category File
.                                                 for N2,"1","10"
.                                                           call      Trim using TempCArray(N2)
.                                                           if (TempCArray(N2) <> "")
.                                                                     move      N2,str2
.                                                                     rep       zfill,str2
.                                                                     write     ListCat,SEQ;LSTNUM:
.                                                                                         str2:
.                                                                                         TempCArray(N2):
.                                                                                         TempCArray2(N2)
.                                                                     add       C1,RecCntC
.                                                                     move      RecCntC,str9
.                                                                     call      Trim using str9
.                                                                     pack      str15,"A",str9
.                                                                     setprop sheetC.range(str15),*Value=LSTNUM
.                                                                     pack      str15,"B",str9
.                                                                     setprop sheetC.range(str15),*Value=str2
.                                                                     pack      str15,"C",str9
.                                                                     setprop sheetC.range(str15),*Value=TempCArray(N2)
.                                                                     pack      str15,"D",str9
.                                                                     setprop sheetC.range(str15),*Value=TempCArray2(N2)
.                                                           endif
.
.                                                 repeat
....................................................................................
                                                  pack      str15,"A",str9
                                                  setprop sheet.range(str15),*Value=LSTNUM
                                                  pack      str15,"B",str9
                                                  setprop sheet.range(str15),*Value=MLSTNAME
                                                  pack      str15,"C",str9
                                                  setprop sheet.range(str15),*Value=str6
                                                  pack      str15,"D",str9
                                                  setprop sheet.range(str15),*Value=UNIVERSE
                                                  pack      str15,"E",str9
                                                  setprop sheet.range(str15),*Value=REVDATE1
                                                  pack      str15,"F",str9
                                                  setprop sheet.range(str15),*Value=NINCA
                                                  pack      str15,"G",str9
                                                  setprop sheet.range(str15),*Value=NINCAWeb
                                                  pack      str15,"H",str9
                                                  setprop sheet.range(str15),*Value=CleanStr
                                                  pack      str15,"I",str9
                                                  setprop sheet.range(str15),*Value=MIN
                                                  pack      str15,"J",str9
                                                  setprop sheet.range(str15),*Value=NEWDATE1
                                                  pack      str15,"K",str9
                                                  setprop sheet.range(str15),*Value=SEX
                                                  pack      str15,"L",str9
                                                  setprop sheet.range(str15),*Value=UNITDATA
                                                  pack      str15,"M",str9
                                                  setprop sheet.range(str15),*Value=NetStr
                                                  pack      str15,"N",str9
                                                  setprop sheet.range(str15),*Value=str1
                                                  pack      str15,"O",str9
.START PATCH 1.5 REPLACED LOGIC
.                                                 setprop sheet.range(str15),*Value=COMMPER
                                                  move      COMMPER,str7
                                                  setprop sheet.range(str15),*Value=str7
.END PATCH 1.5 REPLACED LOGIC
                                                  pack      str15,"P",str9
                                                  setprop sheet.range(str15),*Value="75.00"
.Selection File
                                                  pack      NSLTFLD1,"01X",LSTNUM
                                                  move      "NSLTAIM",Location
                                                  pack      KeyLocation,"Key: ",NSLTFLD1
                                                  call      NSLTAIM
                                                  loop
                                                            until over
                                                            pack      NREFFLD,"L",NSLTNUM
                                                            move      "NREFKEY1",Location
                                                            pack      KeyLocation,"Key: ",NREFFLD
                                                            call      NREFKEY
                                                            if not over
                                                                      write     ListSel,SEQ;NREFDESC:
                                                                                          NSLTNUM:
                                                                                          LSTNUM:
                                                                                          "Y":
                                                                                          NSLTPRICE:
                                                                                          "USD":
                                                                                          "M"
.Write to .XLS file
                                                                      add       C1,RecCntA
                                                                      move      RecCntA,str9
                                                                      call      Trim using str9
                                                                      pack      str15,"A",str9
                                                                      setprop sheetA.range(str15),*Value=NREFDESC
                                                                      pack      str15,"B",str9
                                                                      setprop sheetA.range(str15),*Value=NSLTNUM
                                                                      pack      str15,"C",str9
                                                                      setprop sheetA.range(str15),*Value=LSTNUM
                                                                      pack      str15,"D",str9
                                                                      setprop sheetA.range(str15),*Value="Y"
                                                                      pack      str15,"E",str9
                                                                      setprop sheetA.range(str15),*Value=NSLTPRICE
                                                                      pack      str15,"F",str9
                                                                      setprop sheetA.range(str15),*Value="USD"
                                                                      pack      str15,"G",str9
                                                                      setprop sheetA.range(str15),*Value="M"
                                                            endif
                                                            move      "NSLTKG",Location
                                                            pack      KeyLocation,"Key: ",NSLTFLD1
                                                            call      NSLTKG
                                                  repeat
.Addressing File
                                                  pack      NADDFLD1,"01X",LSTNUM
                                                  move      "NADDAIM",Location
                                                  pack      KeyLocation,"Key: ",NADDFLD1
                                                  call      NADDAIM
                                                  loop
                                                            until over
                                                            pack      NREFFLD,"A",NADDNUM
                                                            move      "NREFKEY2",Location
                                                            pack      KeyLocation,"Key: ",NREFFLD
                                                            call      NREFKEY
                                                            if not over
.START PATCH 1.7 REPLACED LOGIC
.                                                                     write     ListOut,SEQ;LSTNUM:
.                                                                                         NADDNUM:
.                                                                                         NREFDESC:
.                                                                                         "USD"
                                                                      if (NADDPRICE = 0)
                                                                                clear     str8
                                                                      else
                                                                                move      NADDPRICE,str8
                                                                      endif
                                                                      if (NADDDESC = "001")
                                                                                move      "/M",str6
                                                                      elseif (NADDDESC = "002")
                                                                                move      "/F",str6
                                                                      else
                                                                                clear     str6
                                                                      endif
                                                                      write     ListOut,SEQ;LSTNUM:
                                                                                          NADDNUM:
                                                                                          NREFDESC:
                                                                                          str8:
                                                                                          str6:
                                                                                          "USD"
.END PATCH 1.7 REPLACED LOGIC
                                                                      add       C1,RecCntB
                                                                      move      RecCntB,str9
                                                                      call      Trim using str9
                                                                      pack      str15,"A",str9
                                                                      setprop sheetB.range(str15),*Value=LSTNUM
                                                                      pack      str15,"B",str9
                                                                      setprop sheetB.range(str15),*Value=NADDNUM
                                                                      pack      str15,"C",str9
                                                                      setprop sheetB.range(str15),*Value=NREFDESC
.START PATCH 1.7 REPLACED LOGIC
.                                                                     pack      str15,"D",str9
.                                                                     setprop sheetB.range(str15),*Value="USD"
                                                                      pack      str15,"D",str9
                                                                      setprop sheetB.range(str15),*Value=str8
                                                                      pack      str15,"E",str9
                                                                      setprop sheetB.range(str15),*Value=str6
                                                                      pack      str15,"F",str9
                                                                      setprop sheetB.range(str15),*Value="USD"
.END PATCH 1.7 REPLACED LOGIC
                                                            endif
                                                            move      "NADDKG",Location
                                                            pack      KeyLocation,"Key: ",NADDFLD1
                                                            call      NADDKG
                                                  repeat
.Category File
                                                  pack      NCATFLD1,"01X",LSTNUM
                                                  move      "NCATAIM",Location
                                                  pack      KeyLocation,"Key: ",NCATFLD1
                                                  call      NCATAIM
                                                  loop
                                                            until over
                                                            pack      NREFFLD,"T",NCATCODE,NCATNUM
                                                            move      "NREFKEY3",Location
                                                            pack      KeyLocation,"Key: ",NREFFLD
                                                            call      NREFKEY
                                                            if not over
                                                                      if (NCATCODE = "B")
                                                                                pack      str25,"Business"
                                                                      elseif (NCATCODE = "C")
                                                                                pack      str25,"Consumer"
                                                                      elseif (NCATCODE = "E")
                                                                                pack      str25,"Enhanced"
                                                                      else
                                                                                pack      str25,B55
                                                                      endif
                                                                      write     ListCat,SEQ;LSTNUM:
                                                                                          NCATNUM:
                                                                                          NREFDESC:
                                                                                          str25
                                                                      add       C1,RecCntC
                                                                      move      RecCntC,str9
                                                                      call      Trim using str9
                                                                      pack      str15,"A",str9
                                                                      setprop sheetC.range(str15),*Value=LSTNUM
                                                                      pack      str15,"B",str9
                                                                      setprop sheetC.range(str15),*Value=NCATNUM
                                                                      pack      str15,"C",str9
                                                                      setprop sheetC.range(str15),*Value=NREFDESC
                                                                      pack      str15,"D",str9
                                                                      setprop sheetC.range(str15),*Value=str25
                                                            endif
                                                            move      "NCATKG",Location
                                                            pack      KeyLocation,"Key: ",NCATFLD1
                                                            call      NCATKG
                                                  repeat

.Source File
                                                  pack      NSRCFLD1,"01X",LSTNUM
                                                  move      "NSRCAIM",Location
                                                  pack      KeyLocation,"Key: ",NSRCFLD1
                                                  call      NSRCAIM
                                                  loop
                                                            until over
                                                            pack      NREFFLD,"S",NSRCNUM
                                                            move      "NREFKEY4",Location
                                                            pack      KeyLocation,"Key: ",NREFFLD
                                                            call      NREFKEY
                                                            if not over
                                                                      write     ListSrc,SEQ;LSTNUM:
                                                                                          NSRCNUM:
                                                                                          NREFDESC:
                                                                                          NSRCPER
                                                                      add       C1,RecCntD
                                                                      move      RecCntD,str9
                                                                      call      Trim using str9
                                                                      pack      str15,"A",str9
                                                                      setprop sheetD.range(str15),*Value=LSTNUM
                                                                      pack      str15,"B",str9
                                                                      setprop sheetD.range(str15),*Value=NSRCNUM
                                                                      pack      str15,"C",str9
                                                                      setprop sheetD.range(str15),*Value=NREFDESC
                                                                      pack      str15,"D",str9
                                                                      setprop sheetD.range(str15),*Value=NSRCPER
                                                            endif
                                                            move      "NSRCKG",Location
                                                            pack      KeyLocation,"Key: ",NSRCFLD1
                                                            call      NSRCKG
                                                  repeat
.Text File
                                                  pack      NTXTFLD1,"01X",LSTNUM
                                                  move      "NTXTAIM",Location
                                                  pack      KeyLocation,"Key: ",NTXTFLD1
                                                  call      NTXTAIM
                                                  loop
                                                            until over
                                                            write     ListTxt,SEQ;LSTNUM:
                                                                                NTXTNUM:
                                                                                NTXTTEXT
                                                            add       C1,RecCntE
                                                            move      RecCntE,str9
                                                            call      Trim using str9
                                                            pack      str15,"A",str9
                                                            setprop sheetE.range(str15),*Value=LSTNUM
                                                            pack      str15,"B",str9
                                                            setprop sheetE.range(str15),*Value=NTXTNUM
                                                            pack      str15,"C",str9
                                                            setprop sheetE.range(str15),*Value=NTXTTEXT
                                                            move      "NTXTKG",Location
                                                            pack      KeyLocation,"Key: ",NTXTFLD1
                                                            call      NTXTKG
                                                  repeat
.Select File
                                                  if (NDATCONV = "1") .File Converted
                                                            pack      NSELFLD1,"01X",LSTNUM
                                                            clear     NSELFLD2
                                                            move      "NSELAIM-2",Location
                                                            pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                                                            call      NSELAIM
                                                            loop
                                                                      until over
                                                                      if (NSELINACTIVE <> "1" & NSELSTATUS <> "1")
                                                                                add       C1,RecCntF
                                                                                move      RecCntF,str9
                                                                                call      Trim using str9
                                                                                pack      str15,"A",str9
                                                                                setprop sheetF.range(str15),*Value=LSTNUM
                                                                                pack      str15,"B",str9
                                                                                setprop sheetF.range(str15),*Value=NSELNUM
                                                                                pack      str15,"C",str9
                                                                                setprop sheetF.range(str15),*Value=NSELSNAME
                                                                                pack      str15,"D",str9
                                                                                setprop sheetF.range(str15),*Value=NSELQTY
                                                                                pack      str15,"E",str9
                                                                                setprop sheetF.range(str15),*Value=NSELPRICE
                                                                                pack      str15,"F",str9
                                                                                pack      NMODFLD,NSELDESC
                                                                                rep       zfill,NMODFLD
                                                                                move      "NMODKEY",Location
                                                                                pack      KeyLocation,"Key: ",NMODFLD
                                                                                call      NMODKEY
                                                                                call      Trim using NMODDESC
                                                                                setprop sheetF.range(str15),*Value=NMODDESC
                                                                                pack      str15,"G",str9
                                                                                setprop sheetF.range(str15),*Value=NSELBASE
                                                                                pack      str15,"H",str9
                                                                                if (NSELNOTES = "1")
                                                                                          move      YES,str1
                                                                                else
                                                                                          clear     str1
                                                                                endif
                                                                                setprop sheetF.range(str15),*Value=str1
                                                                                pack      str15,"I",str9
                                                                                if (NSELEXC = "1")
                                                                                          move      "Exc/Rent",str16
                                                                                elseif (NSELEXC = "2")
                                                                                          move      "Exchange Only",str16
                                                                                elseif (NSELEXC = "3")
                                                                                          move      "Rental Only",str16
                                                                                else
                                                                                          clear     str16
                                                                                endif
                                                                                setprop sheetF.range(str15),*Value=str16
                                                                                pack      str15,"J",str9
                                                                                setprop sheetF.range(str15),*Value=NSELINDEX
                                                                                write     ListSel2,SEQ;LSTNUM:
                                                                                                    NSELNUM:
                                                                                                    NSELSNAME:
                                                                                                    NSELQTY:
                                                                                                    NSELPRICE:
                                                                                                    NMODDESC:
                                                                                                    NSELBASE:
                                                                                                    str1:
                                                                                                    str16:
                                                                                                    NSELINDEX
                                                                      endif
                                                                      move      "NSELKG-2",Location
                                                                      pack      KeyLocation,"Key: ",NSELFLD1
                                                                      call      NSELKG
                                                            repeat
                                                  endif
                                        endif
                              endif
                    endif
          repeat
.START PATCH 1.7 REPLACED LOGIC
.         write     ListTable,SEQ;"DONE"
          write     ListTbl,SEQ;"DONE"
.END PATCH 1.7 REPLACED LOGIC
          write     ListSel,SEQ;"DONE"
          write     ListOut,SEQ;"DONE"
          write     ListCat,SEQ;"DONE"
.START PATCH 1.1 REPLACED LOGIC
          write     ListSrc,SEQ;"DONE"
          write     ListTxt,SEQ;"DONE"
          write     ListSel2,SEQ;"DONE"
.END PATCH 1.1 REPLACED LOGIC
          write     OutPut,SEQ;"DONE"
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
          trap    TrapCampaignObject if Object
.START PATCH 1.7 REPLACED LOGIC
.         book.saveas giving N9 using *Filename="\\nins1\e\data\ListTable.xls"
.begin patch 1.86
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
                              if        (#ver = c1)
                                       book.saveas giving N9 using *Filename="\\nins1\e\data\ListTbl.xlsx"
                              else
                                 book.saveas giving N9 using *Filename="\\nins1\e\data\ListTbl.xls"
                              endif

.          book.saveas giving N9 using *Filename="\\nins1\e\data\ListTbl.xls"
.end patch 1.86
.END PATCH 1.7 REPLACED LOGIC
          trapclr Object
          trap    TrapCampaignObject if Object
          bookA.saveas giving N9 using *Filename="\\nins1\e\data\ListSel.xls"
          trapclr Object
          trap    TrapCampaignObject if Object
          bookB.saveas giving N9 using *Filename="\\nins1\e\data\ListOut.xls"
          trapclr Object
          trap    TrapCampaignObject if Object
          bookC.saveas giving N9 using *Filename="\\nins1\e\data\ListCat.xls"
          trapclr Object
.START PATCH 1.1 REPLACED LOGIC
          trap    TrapCampaignObject if Object
          bookD.saveas giving N9 using *Filename="\\nins1\e\data\ListSrc.xls"
          trapclr Object
          trap    TrapCampaignObject if Object
          bookE.saveas giving N9 using *Filename="\\nins1\e\data\ListTxt.xls"
          trapclr Object
          trap    TrapCampaignObject if Object
          bookF.saveas giving N9 using *Filename="\\nins1\e\data\ListSel2.xls"
          trapclr Object
.END PATCH 1.1 REPLACED LOGIC
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
          setprop exA,*IgnoreRemoteRequests="False",*Interactive="True"
          setprop exB,*IgnoreRemoteRequests="False",*Interactive="True"
          setprop exC,*IgnoreRemoteRequests="False",*Interactive="True"
.START PATCH 1.1 REPLACED LOGIC
          setprop exD,*IgnoreRemoteRequests="False",*Interactive="True"
          setprop exE,*IgnoreRemoteRequests="False",*Interactive="True"
          setprop exF,*IgnoreRemoteRequests="False",*Interactive="True"
.END PATCH 1.1 REPLACED LOGIC
CampaignCleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
          destroy sheet
          destroy sheetA
          destroy sheetB
          destroy sheetC
.START PATCH 1.1 REPLACED LOGIC
          destroy sheetD
          destroy sheetE
          destroy sheetF
.END PATCH 1.1 REPLACED LOGIC
          destroy sheets
          destroy sheetsA
          destroy sheetsB
          destroy sheetsC
.START PATCH 1.1 REPLACED LOGIC
          destroy sheetsD
          destroy sheetsE
          destroy sheetsF
.END PATCH 1.1 REPLACED LOGIC
          destroy book
          destroy bookA
          destroy bookB
          destroy bookC
.START PATCH 1.1 REPLACED LOGIC
          destroy bookD
          destroy bookE
          destroy bookF
.END PATCH 1.1 REPLACED LOGIC
          destroy books
          destroy booksA
          destroy booksB
          destroy booksC
.START PATCH 1.1 REPLACED LOGIC
          destroy booksD
          destroy booksE
          destroy booksF
.END PATCH 1.1 REPLACED LOGIC
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
          setprop ex,*SheetsInNewWorkbook=SheetsDefault
          ex.quit
          destroy ex
          setprop exA,*SheetsInNewWorkbook=SheetsDefault
          exA.quit
          destroy exA
          setprop exB,*SheetsInNewWorkbook=SheetsDefault
          exB.quit
          destroy exB
          setprop exC,*SheetsInNewWorkbook=SheetsDefault
          exC.quit
          destroy exC
.START PATCH 1.1 REPLACED LOGIC
          setprop exD,*SheetsInNewWorkbook=SheetsDefault
          exD.quit
          destroy exD
          setprop exE,*SheetsInNewWorkbook=SheetsDefault
          exE.quit
          destroy exE
          setprop exF,*SheetsInNewWorkbook=SheetsDefault
          exF.quit
          destroy exF
.END PATCH 1.1 REPLACED LOGIC
.Email the documents
.START PATCH 1.7 ADDED LOGIC
Testit
          call      GetWinVer
.begin patch 1.83
.          clear     taskname
.          Path      Exist,"c:\windows"
.          if over                       .nt/2000
.                    append    "!c:\winnt\system32\cmd.exe",taskname
.          elseif (osflag = c6)          .XP
.                    append    "!c:\windows\system32\cmd.exe",taskname
.          else                          .95/98
.                    append    "!c:\command.com",taskname
.          endif
.          append    " /c del c:\work\nin_*.zip",taskname
.          reset     taskname
.          execute taskname
          clear     Mailbody
          FIndDIr   "c:\work\NIN_*.zip",MailBody,Itemcount=n5
          if        (n5 > c0)
          FOr       n4 from c0 to N5
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    bump      DmFileName,c1
                    pack      taskname from "C:\work\",DmFIleName  ."comment :)
                    FINDFILE  Taskname,WRITE=Str25
                    unpack    str25,CC,YY,MM,DD
                    call      cvtjul
                    move      Juldays,Howmany
                    calc      N9=(result-howmany)
                                        if (N9 >= 1)
                                        erase          taskname
                                        endif
                    endif
          repeat
          endif
.end patch 1.83

          clock     timestamp,timestamp
          unpack    timestamp,str2,YY,MM,DD
          pack      DFolderN,"c:\work\List",MM,DD,".zip"
.begin patch 1.83
..START PATCH 1.7 REPLACED LOGIC
..         pack      taskname,"\\nts0\c\apps\tools\pkzip\pkzip.exe ",str35," \\nins1\e\data\ListTable.xls"
.          pack      taskname,"\\nins1\e\apps\tools\pkzip\pkzip.exe ",str35," \\nins1\e\data\ListTbl.xls"
..END PATCH 1.7 REPLACED LOGIC
.          execute   taskname
.          pack      taskname,"\\nins1\e\apps\tools\pkzip\pkzip.exe ",str35," \\nins1\e\data\ListSel.xls"
.          execute   taskname
.          pack      taskname,"\\nins1\e\apps\tools\pkzip\pkzip.exe ",str35," \\nins1\e\data\ListOut.xls"
.          execute   taskname
.          pack      taskname,"\\nins1\e\apps\tools\pkzip\pkzip.exe ",str35," \\nins1\e\data\ListCat.xls"
.          execute   taskname
.          pack      taskname,"\\nins1\e\apps\tools\pkzip\pkzip.exe ",str35," \\nins1\e\data\ListSrc.xls"
.          execute   taskname
.          pack      taskname,"\\nins1\e\apps\tools\pkzip\pkzip.exe ",str35," \\nins1\e\data\ListTxt.xls"
.          execute   taskname
.          pack      taskname,"\\nins1\e\apps\tools\pkzip\pkzip.exe ",str35," \\nins1\e\data\ListSel2.xls"
.          execute   taskname
*.....................................................................
.
.Zip files
.    
.Create an empty zip file
.       
          PREP      ZFile,DFolderN,Exclusive
          WRITE     ZFile,Seq;*ABSON,ZData
          WEOF      ZFile,Seq
          CLOSE     ZFile
*
.Crank up the shell
.
          CREATE    Auto,Class="Shell.Application"
*
.Set the destination
.    
          Auto.NameSpace Giving DFolder USING DFOlderN
*
.Set the source folder (zipping a complete directory (recursive))
.    
.         Auto.NameSpace GIVING SFolder USING "c:\bud\9.3d"
.         DFolder.CopyHere USING SFolder
*
.OR Zip individual files
.         
          DFolder.CopyHere USING "\\nins1\e\data\ListTbl.xls"
          DFolder.CopyHere Using "\\nins1\e\data\ListSel.xls"
          DFolder.CopyHere Using "\\nins1\e\data\ListOut.xls"
          DFolder.CopyHere Using "\\nins1\e\data\ListCat.xls"
          DFolder.CopyHere Using "\\nins1\e\data\ListSrc.xls"
          DFolder.CopyHere Using "\\nins1\e\data\ListTxt.xls"
          DFolder.CopyHere Using "\\nins1\e\data\ListSel2.xls"
*
.Wait for completion
.     
          EXCEPTSET Wait1 IF IO
Wait1     PAUSE     "1"
          OPEN      ZFile,DFolderN,EXCLUSIVE
          CLOSE     ZFile
.end patch 1.83





.END PATCH 1.7 ADDED LOGIC
tester
.begin patch 1.8
          move      "Datacard Files from NIN",Mailsubjct
          clear     MailBody
          Append    "Datacard Files from Names in the News",MailBody
          Append    CRLF,Mailbody       
          append    "these files include information for ",Mailbody
          append    RecCnt,Mailbody
          append    " Lists.",mailbody
          reset     Mailbody
.         move      "Datacard Files from NINCA",SmtpSubject Subject
.         move      "0",SmtpTextIndexLast                                           Index to last entry       in TextMessage array
.         move      "NTS4",SmtpEmailServer                             Address of email serverc
.         move      "InformationServices@nincal.com",SmtpEmailAddress
          move      "InformationServices@nincal.com",MailFrom
.         move      "AndrewHarkins@nincal.com",SmtpEmailAddress
.         move      "Information Services",SmtpUserName                                     User name
.         move      "Information Services",SmtpUserFullName               User Full Name
.         move      "AndrewHarkins@nincal.com",SmtpDestinations(1,1)
.         move      "jswindell@Nextmark.com",SmtpDestinations(1,1)
          move      "jswindell@Nextmark.com",MailTo
.         move      "JoseDuenas@nincal.com",SmtpDestinations(2,1)
.         move      "ComputerRequest@nincal.com",SmtpDestinations(2,1)
.         move      "ComputerRequest@nincal.com,ShereneKelly@nincal.com",MailCC
.         move      "ComputerRequest@nincal.com,ShereneKelly@nincal.com,GailTrenam@nincal.com",MailCC
.          move      "ComputerRequest@nincal.com,JackForder@nincal.com",MailCC
          move      "ComputerRequest@nincal.com,AmyFrey@nincal.com",MailCC
.         move      "2",SmtpDestIndexLast                                           Index to last entry       in Dest   array
.end patch 1.8
.
.START PATCH 1.7 REPLACED LOGIC
.         move      C0,N1
.         pack      APIFileName,"\\nins1\e\data\ListTable.xls",hexzero
.         call      FindFirstFile
.         if (APIResult <> 0 & APIResult <> hexeight)
.                   add       C1,N1
.                   move      "ListTable.xls",SmtpAttachments(N1,1)                                 .Attached file name
.                   move      "\\nins1\e\data",SmtpAttachments(N1,2)                                 .Path to attached file name
.         endif
..
.         pack      APIFileName,"\\nins1\e\data\ListSel.xls",hexzero
.         call      FindFirstFile
.         if (APIResult <> 0 & APIResult <> hexeight)
.                   add       C1,N1
.                   move      "ListSel.xls",SmtpAttachments(N1,1)                                   .Attached file name
.                   move      "\\nins1\e\data",SmtpAttachments(N1,2)                                 .Path to attached file name
.         endif
..
.         pack      APIFileName,"\\nins1\e\data\ListOut.xls",hexzero
.         call      FindFirstFile
.         if (APIResult <> 0 & APIResult <> hexeight)
.                   add       C1,N1
.                   move      "ListOut.xls",SmtpAttachments(N1,1)                                   .Attached file name
.                   move      "\\nins1\e\data",SmtpAttachments(N1,2)                                 .Path to attached file name
.         endif
..
.         pack      APIFileName,"\\nins1\e\data\ListCat.xls",hexzero
.         call      FindFirstFile
.         if (APIResult <> 0 & APIResult <> hexeight)
.                   add       C1,N1
.                   move      "ListCat.xls",SmtpAttachments(N1,1)                                   .Attached file name
.                   move      "\\nins1\e\data",SmtpAttachments(N1,2)                                 .Path to attached file name
.         endif
..START PATCH 1.1 REPLACED LOGIC
.         pack      APIFileName,"\\nins1\e\data\ListSrc.xls",hexzero
.         call      FindFirstFile
.         if (APIResult <> 0 & APIResult <> hexeight)
.                   add       C1,N1
.                   move      "ListSrc.xls",SmtpAttachments(N1,1)                                   .Attached file name
.                   move      "\\nins1\e\data",SmtpAttachments(N1,2)                                 .Path to attached file name
.         endif
..
.         pack      APIFileName,"\\nins1\e\data\ListTxt.xls",hexzero
.         call      FindFirstFile
.         if (APIResult <> 0 & APIResult <> hexeight)
.                   add       C1,N1
.                   move      "ListTxt.xls",SmtpAttachments(N1,1)                                   .Attached file name
.                   move      "\\nins1\e\data",SmtpAttachments(N1,2)                                 .Path to attached file name
.         endif
..
.         pack      APIFileName,"\\nins1\e\data\ListSel2.xls",hexzero
.         call      FindFirstFile
.         if (APIResult <> 0 & APIResult <> hexeight)
.                   add       C1,N1
.                   move      "ListSel2.xls",SmtpAttachments(N1,1)                                  .Attached file name
.                   move      "\\nins1\e\data",SmtpAttachments(N1,2)                                 .Path to attached file name
.         endif
.         if (N1 > 0)
.                   move      N1,str1
.                   rep       zfill,str1
.                   move      N1,SmtpAttIndexLast                                   Index to last entry       - Only 1 entry
.                   clear     SmtpLogFile                                                     'Clear' disables the LogFile
.                   call      SmtpSend   ( 'Send' is in Smtp.Pri which is included in     TestSmtp.Dbs )
.         endif
..END PATCH 1.1 REPLACED LOGIC
.begin patch 1.8
.          pack      MailAttach from "c:\work\",str25          ."
          pack      MailAttach from DFolderN
.         move      str25,SmtpAttachments(1,1)                                  .Attached file name
.         move      "c:\work",SmtpAttachments(1,2)                                        .Path to attached file name
.         move      "1",SmtpAttIndexLast                                            Index to last entry       - Only 1 entry
.         clear     SmtpLogFile                                                     'Clear' disables the LogFile
.         call      SmtpSend   ( 'Send' is in Smtp.Pri which is included in     TestSmtp.Dbs )
          call      SendMail
.end patch 1.8
.END PATCH 1.7 REPLACED LOGIC
          shutdown "cls"

TrapCampaignObject
          return

UndoFixitA
          clear     str55
          bump      MLSTNAME,-1
          lenset    MLSTNAME
          reset     MLSTNAME
          append    "A ",str55
          append    MLSTNAME,str55
          reset     str55
          clear     MLSTNAME
          move      str55,MLSTNAME
          return

UndoFixitThe
          clear     str55
          bump      MLSTNAME,-1
          lenset    MLSTNAME
          reset     MLSTNAME
          append    "The ",str55
          append    MLSTNAME,str55
          reset     str55
          clear     MLSTNAME
          move      str55,MLSTNAME
          return

          include   ndatio.inc
          include   nrefio.inc
          include   ncatio.inc
.START PATCH 1.1 ADDED LOGIC
          include   nselio.inc
          include   naddio.inc
          include   nsrcio.inc
          include   nsltio.inc
          include   ntxtio.inc
          include   nmodio.inc
.END PATCH 1.1 ADDED LOGIC
          include   comlogic.inc