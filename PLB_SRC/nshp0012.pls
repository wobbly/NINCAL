PC       EQU       0
         INCLUDE    COMMON.inc
         INCLUDE    CONS.inc
         INCLUDE    NSHPDD.inc
         INCLUDE    HP.inc
           INCLUDE  COMPDD.inc
           INCLUDE  CNTDD.INC
         INCLUDE    NORDDD.inc
         INCLUDE    NCNTDD.inc
         Include    NFTP2DD.INC
         Include        NOWNDD.INC
         INCLUDE    WINAPI.inc
Release   Init      "1.35"    DLH .for infogroup check NNotfile (order confirm, if we never got that record add it
reldate   Init      "2015 April 07"
.Release   Init      "1.34"    DLH .Replace Dawn with Cheryl
.reldate   Init      "2015 March 31"
.Release   Init      "1.33"    DLH .place a copy of Innovairre file in billing folder for processing
.reldate   Init      "2015 February 5"
.Release   Init      "1.32"    DLH .Infogroup changed format again about 5/7
.reldate   Init      "2014 May 28"
.Release   Init      "1.31"    DLH .excel 2013 Issue
.reldate   Init      "2014 January 22"
.Release   Init      "1.30"    DLH .IDMI new format in Excel
.reldate   Init      "2014 January 14"
.Release   Init      "1.28"    DLH .DMI
.reldate   Init      "12 August 2011"
.Release   Init      "1.27"    DLH .Infogroup changed format
.reldate   Init      "13 July 2011"
.Release   Init      "1.26"    DLH .PDMG
.reldate   Init      "19 January 2010"
.Release   Init      "1.25"    DLH .Excel
.reldate   Init      "23 April 2009"
.Release   Init      "1.24"    DLH .MMI
.reldate   Init      "14 Jan 2009"
.Release  Init      "1.23"    DLH .fixed shipping info and use order qty for target
.reldate  Init      "08 Sep 2008"
.Release  Init      "1.22"    JD updated Target read to CSV for Doctors
.reldate  Init      "04 Sep 2008"
.Release  Init      "1.21"    DLH add wait for email attachment to be ready
.reldate  Init      "07 Aug 2008"
.RElease  INit      "1.2"               DLH Object trap
.Reldate  Init      "18 October 2007"
.Release  INIT                "1.10"               DMB 13 SEP 2006 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.Release  INIT                "1.00"               DMB 13 SEP 2006 Shipping program for all current and future fulfillment Companies
.//
.//In order to use any of the properties/methods associated with all parent objects
.//of the Worksheet, I need to create automation objects for each of them.
.//
.//Look at Excel Object Model to understand heirarchy.  This can be found in hard
.//documentation:  Microsoft Office 2000 Object Model Guide (found in MS Office 2000 Developers Edition).
.//Software available via PL/B Designer - create a Container object on a form, create an Excel
.//Spreadsheet, right click on Container object and Browse object.  This will invoke the PL/B Object
.//Browser, which will give you SOME of the components of the Object Model.  To browse the Object
.//Model in its entirety, open Excel.  Under Tools menu select Macro, select Visual Basic Editor.
.//In the Visual Basic Editor screen, under the View menu, select Object Browser.  There you can 
.//view all of the objects/methods/properties in Excel.  Right clicking on an item will give you
.//option to locate Help topics to see specifics.
.//
.//General heirarchy:
.// Excel Application
.//       Workbooks Collection (all open Workbooks)
.//               Single Workbook
.//                       Worksheets Collection (all Worksheets in this Workbook)
.//                               Single Worksheet
.//                                       SortColumn (a Single Column in that Worksheet used for sorting)

books                         AUTOMATION
book                          AUTOMATION
.......................
.begin 1.25 to find version of excel  DH 04/02/09
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.end 1.25 to find version of excel
.//bars   automation
.//bar    automation
.//menubar INTEGER 2,"0x00000006"
........................
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
Zoom55                                  VARIANT

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
.begin patch 1.30
xlCSV DEFINE 6
ExinputFlag         Dim       1
IDMIFLAG            Dim       1
csvstring           dim       255
.end patch 1.30

.//Formatting vars needed
.//This constant was found in the Object Browser in Excel under the Help topic for the
.//HorizontalAlignment property of the Range object.
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
.//Colors
Red Color
RGB form 24
.//Flag whether to make row red or not
ColorFlag Dim 1

.begin patch 1.21
FileCheck FIle
trapcount form      4
.end patch 1.21


XLSNAME    DIM  255

.begin patch 1.35
NNOTLR    Dim        6
NNOTDATE  Dim       10
NNOTTIME  Dim       10
NOTFILE   IFile     var=80,keylen=6
.end patch 1.35

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

.//Temp Vars for Data Clean
ShippedLR       Dim       6
ShippedCost         Dim       9
ShippedQTY      Dim       9
ShippedInfo     Dim       36
ShippedDate     Dim       10
ShippedTrack    Dim       25
.//Must Keep for non csv file -target
LISTINFO  DIM       36

.//datalist for filelisting
dlFiles       datalist
Taskname4  DIM 510
Taskname5  DIM 510


HoldQuant       Dim       11

QTYVAR          Form      10.2
.Var                Dim       1

Newflag   Init      "N"
Okstats   Init      "0"

Variation Form      3.2
RecordCount     Form      10
CompHold        DIM       6
CompNHold DIM       55
CompParentHold  DIM       6

.//Vars for Email Donnelley About Descrepancies.
SBEmailflag         Dim       1
SBEmailBody     DIM       5000
SBEmailLine     DIM       255

.//For Dir Listings
.FileString DIM 255
fileDir      DIM 255
NDX        FORM 9
.//For Fiile Rename
DateString DIM 16


ValidFiles Form     3
          clock timestamp,timestamp
.         unpack timestamp,str2,yy,mm,dd
.         pack DateString,mm,slash,dd,slash,cc,yy
          Move Timestamp,Datestring
        create  Red=255:0:0
          getitem   Red,0,RGB

.begin patch 1.35
          Open      NOTFILE,"NINNOT|NINS1:502"
.end patch 1.35


         OPEN      LOGFILE,"Shipfax|NINS1:502"         *TURNED On 12/08/94.
.//Program Header Information
         MOVE      "NSHP0012" TO PROGRAM
         MOVE      "SB Shipping Data Import Program" TO STITLE
         MOVE      "Names In The News" TO COMPNME
         
         
         MOVE      C0 TO PAGE
.//Todays Date         
         CLOCK     DATE TO DATE
         Clock     timestamp to str18
         Unpack    str18 to str2,str16

         Call          PAINT
         MOVE      C1 TO NORDPATH
.//Which Windows Version is This?
         Call      GetWinVer
         
         
.//Create the Variant objects for Excel SpreadSheet
.//Initialize variables
          create  Zoom55,VarType=VT_I4,VarValue=55
          create    OTRUE,VarType=VT_BOOL,VarValue=1
          create    OFALSE,VarType=VT_BOOL,VarValue=0
          create    xlColumnWidth,VarType=VT_R8,VarValue="0.0"
          create    xlColumnWidthGross,VarType=VT_R8,VarValue="10.5"  
          create    xlColumnWidthMailer,VarType=VT_R8,VarValue="40.5" 
          create    xlColumnWidthPrice,VarType=VT_R8,VarValue="9.0"             
          create    xlColumnWidthDescription,VarType=VT_R8,VarValue="60.0"                          
.//"1" increment in Excel interface equals "1.3888" in OLE logic
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
          Trap      OBJError giving error if object

INPUT        

           Loop
                    Call   NFTP2KS
           Until Over                   
                    If        (NFTP2INFOTYPE = "S")
                               Move        NFTP2COMP to CompHold 
                               Move      NFTP2COMP,COMPFLD            
                               Call      Compkey
                               Clear     CompNhold
                               Move        CompComp,CompNhold                              
                               Move        CompMain,CompParentHold                         
                               DISPLAY   *P10:15,"Working On Company : ",CompComp,b5,CompHold                            

//Do file Listing of Directory to get file name to open
                              Create    dlFiles=1:10:1:10,visible=0
                              Pack      taskname,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\SHIPPING\","*.*"
                              Pack      fileDir,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\SHIPPING\"                                                                          
                          dlFiles.Dir giving result using *Filespec=Taskname,*flags=0
                              If (result <> -1 )
                                        For NDX from 0 to result
                                                  dlFiles.GetText giving FileString using *Index=NDX  
//File Prep
                                                  Call        Trim Using FileString
//If record has been applied then skip  - Program will slap Applied_ after it has been applied.                                   
                                                  Scan      "Applied" in FileString
                                                  Goto        NextFile If Equal
                                                  reset     filestring
.begin patch dave 26apr2007
                                                  Scan      "applied" in FileString
                                                  Goto        NextFile If Equal
.end patch dave 26apr2007
.begin patch 1.30
                                                  Scan      "Converted" in FileString
                                                  Goto        NextFile If Equal
                                                  Scan      "converted" in FileString
                                                  Goto        NextFile If Equal
.                                                  call      debug

                                                  reset     filestring
                                                  move      filestring,taskname
                                                  scan      ".xls" in taskname
                                                  if        equal 
                                                  MOVEFPTR  taskname,n3
                                                  sub       c1 from n3
                                                  setlptr  taskname,n3
                                                  move      yes,ExinputFlag
                                                  reset     filestring
                                                  Call        Trim Using FileString
                                                  reset     taskname
                                                  pack      csvstring from taskname,".csv"
                                                  else
                                                  move      No,ExinputFlag
                                                  endif
                                                  move      No,IDMIFlag

                                                  scan      "IDMI " in filestring
                                                  if        equal 
                                                  move      yes,IDMIFlag
                                                  endif
                                                  scan      "idmi " in filestring
                                                  if        equal 
                                                  move      yes,IDMIFlag
                                                  endif
                                                  reset     filestring
.end patch 1.30
                                                PACK      InputName,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\SHIPPING\",FileString
                                                Trap      FileOpenFail if IO
//Open the File                                           
                                                  Add         c1 to ValidFiles
.begin patch 1.30
.                                                  OPEN      INPUT,InputName
                                                     if        (ExInputFlag = yes)
                                                            create  ex
.begin patch 1.31
.                                                            setprop ex,*WindowState=xlMinimized
.end patch 1.31
.                                                            setprop ex,*Visible="True",*DisplayAlerts=OFALSE       // Hidden & NO dialogs !!
                                                            setprop ex,*Visible="False",*DisplayAlerts=OFALSE       // Hidden & NO dialogs !!
                                                            GETPROP   ex,*Workbooks=books

.                                                  call      debug
.
                                                            Books.Open GIVING Book USING Inputname           .Open XLS file
.
.                           Find The First Sheet From The Sheets Collection
                                                  call      debug
.
                                                           GETPROP   Book,*SHEETS=Sheets
                                                           Sheets.Item GIVING Sheet USING 1
                                                PACK      InputName,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\SHIPPING\",csvString

                                                           Book.SaveAs USING *FileName=Inputname,*FileFormat=xlCSV     .Save to CSV file (overwrite due to NoAlerts)
                                                           setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
                                                           Book.Close
                                                           Ex.Quit
.
                                                            EXCEPTSET PROC3B30 IF IO
PROC3B30                                                    DISPLAY   *P1:24,*EF,"Waiting: ",csvstring,*W1;
                                                            OPEN      INput,Inputname,EXCLUSIVE
                                                           EXCEPTCLEAR IO                                                  

                                                  Trap      ErrorRename giving str50 if IO
                                                  Clear       Taskname
                                                  clear     Taskname4
                                                  clear     taskname5
                                                  
                                                  Pack      taskname,"Converted_",DateString,"_",FileString
                                                  Pack      taskname4,fileDir,FileString
                                                  Pack      taskname5,fileDir,taskname                                                                                                        
//Rename the file with Converted_ as a prefix                                               
                                                  Trap      ErrorRename giving str50 if IO
                                                            Rename    taskname4,taskname5
                                                  Trapclr IO
                                                            
                                                            
.                                                  Move      csvstring,inputname
                                                  Move      csvstring,Filestring
                                                OPEN      INPUT,InputName
                                                  Trapclr IO
                                                  
                                                  else
                                                OPEN      INPUT,InputName
                                                  endif
                                                  TrapClr   IO
.end patch 1.30
//apply the Records                                         
                                                  DISPLAY   *P10:16,"Working On File : ",FileString
                                                  Call      ApplyShipping

                                                  CLOSE     INPUT                                             
//File Renamed After Application
.GOd help me                                      
                                                  Clear       Taskname
                                                  clear     Taskname4
                                                  clear     taskname5
.GOd help me                                      
                                                  call      debug                                                  
                                                  Pack      taskname,"Applied_",DateString,"_",FileString
                                                  Pack      taskname4,fileDir,FileString
.begin patch 1.33
                                                  if        (IDMIFlag = yes)                   .Place a copy in billing folder for processing
                                                  pack       taskname5,"\\nins1\e\storage\import\009410\billing\",filestring     ."
                                                  copyfile   taskname4,taskname5                                                 
                                                  endif
.end patch 1.33
                                                  Pack      taskname5,fileDir,taskname                                                                                                        
                                                  Trap      ErrorRename giving str50 if IO
//Rename the file with Applied_ as a prefix                                               
.begin patch 1.30
                                                            Rename    taskname4,taskname5
.                                                  if        (IDMIFlag <> yes)                   .Don't know where the csv file went we just processed it
.                                                            Rename    taskname4,taskname5
.                                                  endif
.end patch 1.30
                                                  Trapclr IO
                                                            
NextFile                                          

                                        Repeat
                                        
//Clear out content in Listing
                                        dlFiles.ResetContent
                              Endif
                    Endif

          repeat

//Destroy Object with Listing                                         
          Destroy dlFiles                                   
//
EOJ      
//Check to see if there were any valid files that were processed that contained no valid records.
        COMPARE   C0 TO PAGE
        If Equal
                    If (ValidFiles > 0)        
                    Call SendtoIS
          Endif
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
          setprop ex,*Visible="False"
        clear   taskname
        append  "c:\work",taskname                        
        append  "\",taskname                                                                 ."
        reset   taskname
        setprop ex,*DefaultFilePath=taskname
        call    Trim using XLSNAME
        pack    taskname,taskname,XLSNAME
............................................
.//Would Dislplay File Save as dialog box but for now this is automated.
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
          destroy Zoom55
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
        Move "creques@nincal.com,gspranz@nincal.com",Mailto
.        Move "DesktopSupportGroup@nincal.com",MailFrom
          Move "ComputerRequest@nincal.com",MailFrom
        
        Move "File From The Shipping Program.",MAILSubjct
        Move "",MAILBody
.........................
.begin patch 1.21
          Move      c0,Trapcount
CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,MailAttach,Exclusive          
          Close     FIleCHeck

          call      debug
          Move      Yes,Mailtrace

.end patch 1.21
.begin patch 1.22
        If (SBEmailFlag = YES)
          Call EmailServiceBureau
        Endif
.end patch 1.22


        Call  SendMail                 
.begin patch 1.22
.        If (SBEmailFlag = YES)
.         Call EmailServiceBureau
.        Endif
.end patch 1.22

          Erase MailAttach        

DONE     
        shutdown
        STOP
.begin patch 1.2
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Nmrg0009 - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "dherric@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    mailattach,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    endif
          
                    goto      checkfile

.end patch 1.21
        

.//
ApplyShipping       
.//Ship Codes
                clear           shippedlr         
                    Loop
                                       If (COMPHOLD = "009406") .Donelley

.need to add code to read headers and see if they changed
.current header as of 5/28
."List Name,Order Description,Create Date,Ship Date,Mailer Name,Broker PO #,PO Number,Quantity,Decoy Quantity,Decoy Key,Total Quantity"
.                                                           List Name,Create Date,Ship Date,Mailer Name,Broker PO #,PO Number,Quantity,Decoy Quantity,Total Quantity
.                                                           List Name,Create Date,Ship Date,Mailer Name,Broker PO #,PO Number,Quantity,Decoy Quantity,Total Quantity
.again it changed -argh
.                                                           List Name,lr,Create Date,Ship Date,Mailer Name,Broker PO #,PO Number,Quantity,Decoy Quantity,Decoy Key,Total Quantity
.begin patch 1.32
                                                       if         (Recordcount = c0)
                                                 READ      INPUT,SEQ;str255
                                                       squeeze    str255,str255,b1
                                                       move       "List Name,Order Description,Create Date,Ship Date,Mailer Name,Broker PO ##,PO Number,Quantity,Decoy Quantity,Decoy Key,Total Quantity",taskname
                                                       squeeze    taskname,taskname,b1
                                                 match str255 to taskname
                                                 call  alert if not equal
                                                       READ      INPUT,SEQ;*CDFON,str1,str6,str1,ShippedDate,str1,str1,ShippedLR,str1,str1,str1,ShippedQTY
                                               
                                                 else
                                                       READ      INPUT,SEQ;*CDFON,str1,str6,str1,ShippedDate,str1,str1,ShippedLR,str1,str1,str1,ShippedQTY
                                                       endif
.                                                
.                                                      READ      INPUT,SEQ;*CDFON,str1,str1,ShippedDate,str1,str1,ShippedLR,str1,str1,ShippedQTY
.end patch 1.32
 
.                                                  READ      INPUT,SEQ;*CDFON,str1,str1,str1,str1,str1,ShippedInfo,ShippedLR,ShippedDate,ShippedQTY,strack,ShippedCost                         
                                        Elseif (COMPHOLD = "009387") .PIDI
                                        READ      INPUT,SEQ;*cdfon,ShippedLR,str1,str1,str1,ShippedInfo,ShippedDate,ShippedQTY,ShippedCost,ShippedTrack                             
.begin patch 1.28
                                        Elseif (COMPHOLD = "009412") .DMI
                                        READ      INPUT,SEQ;*cdfon,ShippedLR,ShippedDate,str1,str1,str1,ShippedCost,ShippedQTY,ShippedInfo,ShippedTrack                             

.end patch 1.28
.begin patch 1.26
                                        Elseif (COMPHOLD = "010408") .PDMG
                                        READ      INPUT,SEQ;*cdfon,ShippedLR,str1,str1,str1,ShippedInfo,ShippedDate,ShippedQTY,ShippedCost,ShippedTrack                             
.begin patch 1.26
                                        Elseif (COMPHOLD = "009428") .MMI
                                        call      debug
                                        READ      INPUT,SEQ;*cdfon,str1,ShippedLR,str1,str1,ShippedInfo,ShippedDate,ShippedQTY,ShippedCost                      
                                        Elseif (COMPHOLD = "009411") .Target - Now CSV Format
                                                  call debug          
.start patch 1.22
.                                               READ        INPUT,SEQ;ShippedLR,ListInfo,str8,str13,str6,ShippedTrack                                                                         
.begin patch 1.23
.                                               READ        INPUT,SEQ;*CDFON,ShippedLR,ListInfo,str8,str13,str6,ShippedTrack                                                                            
                                                READ        INPUT,SEQ;*CDFON,ShippedLR,ListInfo,str8,str13,str10,ShippedTrack                                                                           
.end patch 1.23
.end patch 1.22
.                                               Call    Trim Using Str13
.                                               Count   N3,str13
.                                               If      (N3 > c9) 
.                                                 Move ShippedLR,SLRNUM
.                                                 Move Str13,SINFO
.                                                           Pack Taskname with "Please Verify Quantity.  It may be too large for var."      
.                                                           Move Yes,ColorFlag                      
.                                                           Call Detail
.                                                           Goto NextRead                                               
.                                               Else
.                                                 Move str13,ShippedQty
.                                               Endif

.                                                 READ      INPUT,SEQ;ShippedLR,ShippedInfo,TEMPSDATE,STR13,STR6,TEMPSTRACK
                                        Elseif (COMPHOLD = "009410") .Frontline
                                                  if        (IDMIflag = yes)
.list name, lr, date, mailer,blank,shipped qty, rate, rate mod, unit price, total,blank,qty, deliv $, Delv total, total,blank, DSI list code,DSI LEX#
                                                READ        INPUT,SEQ;*cdfon,str55,ShippedLR,ShippedDate,str25,str1,ShippedQTY,str6,str6,str6,str6,str1,str5,ShippedCost
                                                  else
                                                READ        INPUT,SEQ;*cdfon,ShippedLR,ShippedInfo,ShippedDate,ShippedCost,ShippedQTY                           
                                                  endif
                                        Endif
                    Until Over
                    
                    
                    
                              Type      ShippedLR
                              Goto      NextRead If Not Equal
                    Cmatch    " " TO ShippedLR         
                    Goto      NextRead IF EOS                         
                    REP       ZFILL IN ShippedLR
                    Move    ShippedLR,NSHPFLD
                              CALL      NSHPKEY
                              If        Not Over
                                        Clear HoldQuant         
                                        Move Squant,str9
                                        Call FormatNumeric using str9,HOLDQUANT
                                        Clear SHPVARS                
                              Else
                                        Clear SHPVARS
                                        Clear HoldQuant
                              Endif 
                              If (CompHold = "009410")
                              Move      "F" TO SCODE     .Frontline    9410 now Innovairre
                    Elseif (CompHold = "009411")
                              Move      "A" TO SCODE     .Target       9411     
                                      Move        str8,ShippedDate
                                      Call    Trim using str6
.                                     Move        Str6,ShippedInfo                        
                                      Move        ShippedTrack,ShippedInfo                          
                              
                    Elseif (CompHold = "009406")            
                              Move      "T" TO SCODE     .Donnelley    9406
.begin patch 1.24
                    Elseif (CompHold = "009428")            
                              Move      "m" TO SCODE     .MMI
.end patch 1.24
                    Elseif (CompHold = "009387")            
                              Move      "Z" TO SCODE     .PIDI         9387     
.begin patch 1.26
                    Elseif (CompHold = "010408")            
                              Move      "X" TO SCODE     .PDMG         
.end patch 1.26
.begin patch 1.28
                    Elseif (CompHold = "0009412")            
                              Move      "d" TO SCODE     .DMI
.end patch 1.28
                              Endif                         
.//Create Excel Spreadsheet
                    Clear     Taskname
                    Add       C1 to RecordCount
                    If (RecordCount = C1)
.//Create a new excel worksheet
.//.Open Excel application
                                      create  ex
.begin patch 1.31
.                                      setprop ex,*WindowState=xlMinimized
.end patch 1.31
.                                      setprop ex,*Visible="True"
                                      setprop ex,*Visible="False"
                                        setprop ex.CommandBars("Standard"),*Visible="True"
                                        setprop ex.CommandBars("Formatting"),*Visible="True"
                                        setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"

.begin 1.25 get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
.//File Cleanup
          if        (#ver = c1)
                             Pack      XLSNAME,DateString,"_","Shipping.xlsx"
          else
                             Pack      XLSNAME,DateString,"_","Shipping.xls"
          endif
                             Pack      Taskname,"c:\work\",XLSNAME              ."
                             Erase     Taskname
.end 1.25 get exel version info
.//.Reset Default of Worksheets found in a Workbook
                              getprop ex,*SheetsInNewWorkbook=SheetsDefault
                              setprop ex,*SheetsInNewWorkbook=C1
.//.Create Workbooks collection
                              getprop ex,*Workbooks=books
.//.Create/Add a single Workbook
                              books.add
                              books.item giving book using 1
.//.Create Worksheets collection
                              getprop book,*Sheets=sheets

.//.Create a single Worksheet - we did not need to add it as we set the default above to
.//.add one new Worksheet each time a Workbook is created.
                              sheets.item giving sheet using 1
                                        setprop sheet.PageSetup,*Orientation=xlPortrait
                              pack taskname," &D",crlf," &P"
                              setprop sheet.PageSetup,*RightHeader=taskname        
.//Sheet Formatting         
                              setprop sheet.PageSetup,*Zoom=Zoom55
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
.//Variance Flag                        
.                             Move      NO TO VAR



.//Shipping Delivery Information - EMAIL etc.
                              Call GetShipDescription
.//Date Check
                              Call DateClean         
.//Shipping Cost
                              Call ShipCostClean
.begin patch 1.35
                      if (CompHold = "009406")                       
                      call       CheckCOnfirm
                      endif
.end patch 1.35
.//Quantity Check
                              Call QuantityClean
.//Shipping Data         
                    ADD       C1 TO COUNT
                    DISPLAY   *P10:12,"RECORDS IN : ",COUNT
                    Move        ShippedLr to SLRNUM
                    MOVE      SLRNUM TO NORDFLD
.//Check if order exists                          
                    CALL      NORDKEY
                    If Over
                                        Append "LR Number Not Found.  Record Not added",Taskname
                                        Move Yes,ColorFlag   
                                        Call Detail
                                        Goto NextRead
                    Else
.//Only Live Orders are applied                             
                              RESET     OKSTATS
                              MATCH     OSTAT IN OKSTATS                        
                              If Not Equal
                                                  If (OSTAT = "B")
                                                            Pack Taskname with "Record Has already Been Billed.  This update will not be applied."    
                                                            Move Yes,ColorFlag                      
                                                            Call Detail
                                                            Goto NextRead
                                                  Else
                                                            Pack Taskname with "Record is not Live.  This record will not be applied."      
                                                            Move Yes,ColorFlag                      
                                                            Call Detail
                                                            Goto NextRead
                                                  Endif
                              Endif
.//Check to see if the lr matches the fulfillment company
.Start Patch 1.1 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.    
.                             MOVE    OLON TO NOWNFLD
.                                       Call      Zfillit Using NOWNFLD
.                             Call      NOWNKEY
.                                       If (OWNCTN <> "")
.                                                 pack      COMPFLD6,OWNCTN
.                                                 rep       zfill,COMPFLD6
.                                                 move      C1,COMPPATH
.                                                 move      "Nshp0012-COMPKEY6",Location
.                                                 pack      KeyLocation,COMPFLD6
.                                                 call      COMPKEY6
.                                                 If over
.                                                           Clear     COMPFLD6
.                                                 Else
.                                                           If (COMPSVBFLG = "T")
.                                                                     If (COMPNUM <> COMPHOLD)    . Different Service Bureau May be the wrong LR NUM
.                                                                               If (CompMain <> CompParentHold)                                                                                                             
.                                                                                         Pack Taskname with "The Service Bureau on the order differs from the SB sending us info."           
.                                                                                         Move Yes,ColorFlag                      
.                                                                                         Call Detail
.                                                                                         Goto NextRead
.                                                                               Endif
.                                                                     Else
.                                                                               Clear     COMPFLD6
.                                                                     Endif                         
.                                                           Else
.                                                                     Clear     COMPFLD6
.                                                           Endif               
.                                                 Endif
.                                       Endif
.End Patch 1.1 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.      
.Start Patch 1.1 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.    
                                        Call      trim using OFULLFIL
                                        If (OFULLFIL <> "")
                                                  pack      COMPFLD,OFULLFIL
                                                  call    zfillit using compfld
                                                  move      C1,COMPPATH
                                                  move      "Nshp0012-COMPKEY",Location
                                                  pack      KeyLocation,COMPFLD
                                                  call      COMPKEY
                                                  If over
                                                            Clear     COMPFLD
                                                  Else
                                                            If (COMPSVBFLG = "T")
                                                                      If (COMPNUM <> COMPHOLD)    . Different Service Bureau May be the wrong LR NUM
                                                                                If (CompMain <> CompParentHold)                                                                                                             
                                                                                          Pack Taskname with "The Service Bureau on the order differs from the SB sending us info."           
                                                                                          Move Yes,ColorFlag                      
                                                                                          Call Detail
                                                                                          Goto NextRead
                                                                                Endif
                                                                      Else
                                                                                Clear     COMPFLD
                                                                      Endif                         
                                                            Else
                                                                      Clear     COMPFLD
                                                                      call      SendtoIsBadSB                                                                   
                                                            Endif               
                                                  Endif
                                        Endif
.End Patch 1.1 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.      







.//Check For Variance will notify if 10% or greater
.//Find out what is 10% of Order Quantity
                                        MOVE      C0 TO N10
                                        MOVE      OQTY TO N10
                                        DIV       C10 INTO N10
                                        MOVE      N10 TO TENPER
                                        MOVE      ShippedQTY TO N10
                                        MOVE      OQTY TO DIFF
                                        SUB       N10 FROM DIFF
.//Is There no variation/difference?         
                                        COMPARE   C0 TO DIFF
.//If negative make it positive for comparison purposes.    
                                        IF LESS
                                                  MULT      SEQ BY DIFF          
                                        Endif
.//Is the variation 10% or more - compare calculated 10% difference menus actual difference.         
                                        COMPARE   DIFF TO TENPER
                                        IF NOT GREATER
                                                  Append "Exceeds allowable quantity Variation. Sent to NIN Contact for Verification - ",Taskname
                                                  If (CompHold = "009406")   .Donnelley
                                                  Move Yes to SBEmailflag
                                                            Pack SBEmailLine,SLRNUM,b5,"NIN Order Qty: ",OQTY," Donnelley Qty: ",SQUANT 
                                                            Call ServiceBureauEmailBody   
                                                  Endif
                                        call sendnews                                     
                                        Endif
                                        MOVE      OQTY TO QTYVAR         
                                        Calc      Variation=(DIFF/QTYVAR)
                                        Move         str18,SRDATE
                                        Move         "IS",SINITS
                                        MOVE      SLRNUM TO NSHPFLD
                                        CALL      NSHPTST
                                        If Not Over
.//Record Has already been written.  I will update                                        
                                                  Call      nshpupd
                                                  ADD       C1 TO UPDATE
                                                  Append "Record Updated ",Taskname
                                                  Append "Old Ship Qty Was: ",Taskname     
                                                  Append HoldQuant,Taskname
                                                  DISPLAY   *P10:14,"RECORDS Updated : ",Update                                             
                                        Else
.//Write the record if it passed the test. 
                                                  CALL      NSHPWRT
                                                  ADD       C1 TO WRITE
                                                  DISPLAY   *P10:13,"RECORDS WRITTEN : ",WRITE
.                                                 CMATCH    YES TO Var
.                                                 IF        EQUAL
.                                                           MOVE      NO TO Var
.                                                           Call      WRITELOG
.                                                 ENDIF
                                                  Pack   Taskname with "Record Added"                                   
                                        Endif
                                        Call    Detail       
                              Call      WRITELOG
                    Endif
NextRead                      
          Repeat
          Return
//
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
                  setprop sheet.range(CellRange),*Value="Shipping Report",*HorizontalAlignment=AlignCenter                   
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
          Pack Cell,"H",Str5
        Setprop sheet.range(Cell),*Value="Company",*HorizontalAlignment=AlignLeft,*ColumnWidth=xlColumnWidthDescription                     
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
         GOTO NextRead

BadCost
.          Pack Taskname with "Cost is not formatted properly.  This record will not be applied."             
           Append "Cost is not formatted properly.  This record will not be applied.",Taskname
           Move Yes,ColorFlag            
           Call Detail                 
         GOTO NextRead
BADDATE
.          Pack Taskname with "Ship Date not Formatted Properly.  Record Not added."   
           Append "Ship Date not Formatted Properly.  Record Not added.",Taskname
           Move Yes,ColorFlag  
           Call Detail         
         GOTO NextRead

WRITELOG
         PACK       MKEY FROM OMLRNUM,OCOBN
         MATCH      MKEY TO HOLDMKEY
         IF         NOT EQUAL
          CALL      NMLRKEY
          MOVE      MKEY TO HOLDMKEY
         ENDIF
         MOVE       C0 TO N2
         PACK       SALES FROM OSALES10,OSALES
         MOVE       SALES TO N2
         COMPARE C6 TO N2
         If Equal
          Call WRTALPH2       
         Endif
         COMPARE "19" TO N2
         If Equal
          Call WRTALPH2       
         Endif
         return

WRTALPH2 
           FILEPI    3;LOGFILE
         READ      LOGFILE,NORDFLD;;
         GOTO      NextRead IF NOT OVER
         WRITE     LOGFILE,NORDFLD;ORDVARS
         Return

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
.         setprop sheet.range(Cell),*Value=ShippedCost,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"         
         setprop sheet.range(Cell),*Value=ShippedCost,*HorizontalAlignment=AlignRight
           Pack Cell,"F",Str5
           Call Trim using SINFO
         Setprop sheet.range(Cell),*Value=SINFO,*HorizontalAlignment=AlignLeft            
           Pack Cell,"G",Str5
           Reset Taskname
         Setprop sheet.range(Cell),*Value=Taskname,*HorizontalAlignment=AlignLeft,*WrapText=OTRUE  
           Pack Cell,"H",Str5         
           Move Cell,str9                          
         Setprop sheet.range(Cell),*Value=CompNhold,*HorizontalAlignment=AlignLeft         
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

        Move    "This is a Informational e-mail from  the Shipping program",MailSubjct
        Append  "This is a Informational e-mail from  the Shipping program",MailBody
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
        Reset  MailBody

        Clear    MailFrom  
        Append   str35 to MailFrom
        Append   "@nincal.com",MailFrom
        Reset    MailFrom
        Move     MailFrom,MailTo
................................

          Call     SendMail
          
        winshow
        return

DateClean
//Get Dates in the right vars/formats

                    Goto DateClean1 if (COMPHOLD = "009387")    .PIDI
                    Goto DateClean1 if (COMPHOLD = "009406")    .Donnelley                
.begin patch 1.24
                    Goto DateClean1 if (COMPHOLD = "009428")    .MMI
.end patch 1.24
.begin patch 1.26
                    Goto DateClean1 if (COMPHOLD = "010408")    .PDMG
.end patch 1.26
.begin patch 1.28
                    Goto DateClean1 if (COMPHOLD = "009412")    .DMI
.end patch 1.28

                    Goto DateClean2 if (COMPHOLD = "009410" & IDMIflag <> yes)    .Frontline
                    Goto DateClean1 if (COMPHOLD = "009410" & IDMIflag = yes)    .IDMI
                    Goto DateClean2 if (COMPHOLD = "009411")    .Target                   
                    Return              
          
          
DateClean1
//clean format 9/11/2006 - add code for single digit month,day
//Triplex,PIDI
          move ShippedDate to str2
          type str2
          if not equal
                    scan "/" in str2
                    if equal 
                              reset str2                    
                              squeeze str2,str2,"/"
                              call zfillit using str2
                              move str2 to mm
                    endif
          else 
                    move str2 to mm
          endif
          reset ShippedDate
          scan "/" in ShippedDate
          if equal
                    bump ShippedDate
                    move ShippedDate to str2
                    type str2
                    if not equal
                              scan "/" in str2
                              if equal 
                                        reset str2
                                        squeeze str2,str2,"/"
                                        call zfillit using str2
                                        move str2 to dd
                              endif
                    else
                              move str2 to dd
                    endif               
          endif
          scan "/" in ShippedDate
          if equal
                    bump      ShippedDate
                    move      ShippedDate to str4
                    type      str4
                    if not equal
                              noreturn
                              goto      baddate             
                    endif
                    unpack    str4,cc,yy
                    if not equal
                              noreturn
                              goto      baddate             
                    endif
          endif
          Call DateCheck        
          return
DateClean2
//Correct Data Format
//target,FrontLine
          Type ShippedDate
          if not equal
                    noreturn
                    goto      baddate             
          endif
          Unpack ShippedDate,str4,mm,dd
          Unpack str4,cc,yy
          Call DateCheck
          return    
DateCheck
.Check if Date is valid and makes sense 
          move    MM,N2
        if (N2 > "12")
          noreturn
                    goto baddate
          elseif (N2 = "00")
                    noreturn
                    goto baddate        
        else
                move    DD,N2
                if (N2 > "31")
                    noreturn
                              goto baddate                
                    elseif (N2 = "00")
                              noreturn
                              goto baddate                            
                else
                            move    CC,N2
                              if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                        noreturn
                                        goto baddate                
                        elseif (N2 = "19")
                                move    YY,N2
                                if (N2 < "80")
                                                  noreturn                                
                                                  goto baddate                
                                endif
                              elseif (N2 = "00")                        
                                        noreturn                      
                                        goto baddate                                      
                              endif
                endif
        endif
        pack        sdate from cc,yy,mm,dd                 
          return

ShipCostClean       

                    Goto ShipCostClean2 if (COMPHOLD = "009387")    .PIDI
                    Goto ShipCostClean3 if (COMPHOLD = "009406")    .Donnelley            

                    Goto ShipCostClean1 if (COMPHOLD = "009410" & IDMIflag <> yes)    .Frontline
                    Goto ShipCostClean2 if (COMPHOLD = "009410" & IDMIflag = yes)    .Innovaire
                    Goto ShipCostClean3 if (COMPHOLD = "009411")    .Target
.begin patch 1.24
                    Goto ShipCostClean1 if (COMPHOLD = "009428")    .MMI
.end patch 1.24
.begin patch 1.26
                    Goto ShipCostClean1 if (COMPHOLD = "010408")    .pdmg
.end patch 1.26
.begin patch 1.28
                     if (COMPHOLD = "009412")    .DMI
                              Call Trim using ShippedCost
                    Goto ShipCostClean4
                    endif
.end patch 1.28
                    
ShipCostClean1      
.FrontLine
          move    "," to str1
        call    Removechar using ShippedCost,str1
          move    "$" to str1
        call    Removechar using ShippedCost,str1   
        call        trim using ShippedCost
        Type    ShippedCost
        If Not Over
                  Move ShippedCost to n9
                    if (n9 <> C0)
//      Patch 1.1 For Pidi to account for possible dollar sign in costs.        
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
                                        noreturn
                                        goto badcost                  
                              Endif
                    else
                              move    ShippedCost,spost
                    
                    Endif
          Endif
          Return
ShipCostClean2
.PIDI
//      Patch 1.1 For Pidi to account for possible dollar sign in costs.
        call        trim using ShippedCost
          move    "," to str1
        call    Removechar using ShippedCost,str1
          move    "$" to str1
        call    Removechar using ShippedCost,str1   
          move    "." to str1
        call    Removechar using ShippedCost,str1        
        call        trim using ShippedCost
          Count n1,ShippedCost
          If (n1 = c4)        
                    move ShippedCost,spost
          elseif (n1 = c3)    
                    pack ShippedCost,"0",spost
          else
                    Noreturn
                    goto badcost                  
          Endif
          Return
ShipCostClean3
.Donnelley,Target
          move       "." to str1
        call       removechar using ShippedCost,str1
        move       ShippedCost to spost
          Return
ShipCostClean4
.DMI
//      Patch 1.1 For Pidi to account for possible dollar sign in costs.
        call        trim using ShippedCost
          move    "," to str1
        call    Removechar using ShippedCost,str1
          move    "$" to str1
        call    Removechar using ShippedCost,str1   
          move    "." to str1
        call    Removechar using ShippedCost,str1        
        call        trim using ShippedCost
          Count n1,ShippedCost
          If (n1 = c4)        
                    move ShippedCost,spost
          elseif (n1 = c3)    
                    pack ShippedCost,"0",spost
          elseif (n1 = c2)    
                    pack ShippedCost,"00",spost
          else
                    Noreturn
                    goto badcost                  
          Endif
          Return
//No cost for target          
          

QuantityClean

          Goto QuantityClean2 if (COMPHOLD = "009387")    .PIDI
          Goto QuantityClean2 if (COMPHOLD = "009406")    .Donnelley            
.begin patch 1.24
          Goto QuantityClean2 if (COMPHOLD = "009428")    .MMI
.end patch 1.24
.begin patch 1.26
          Goto QuantityClean2 if (COMPHOLD = "010408")    .pdmg
.end patch 1.26
.begin patch 1.28
          Goto QuantityClean2 if (COMPHOLD = "009412")    .DMI
.end patch 1.28

          Goto QuantityClean1 if (COMPHOLD = "009410" & IDMIflag <> yes)    .Frontline
          Goto QuantityClean2 if (COMPHOLD = "009410" & IDMIflag = yes)    .Frontline
          Goto QuantityClean3 if (COMPHOLD = "009411")    .Target

QuantityClean1
.Frontline
           Call      Zfillit Using ShippedQTY
         TYPE      ShippedQTY
         If Not Equal
          noreturn            
          goto badqty
         Endif
         Move      ShippedQTY,SQUANT
         Move      ShippedLR,SLRNUM
          return
QuantityClean2
.//Pidi,Donnelley
          move    "," to str1
        call    removechar using ShippedQTY,str1
          call    trim using ShippedQTY
        count   n2,ShippedQTY
        clear   squant
        Call        zfillit using shippedqty        
        TYPE        ShippedQty
        IF NOT EQUAL
          noreturn
                    GOTO      BADQTY         
          Endif
          Move      Shippedqty to SQUANT          
                    
          return
QuantityClean3
.//Target

          clear     ShippedQty
.begin patch 1.23
.         call      trim using str13
.         count     n9,str13
          call      trim using str10
          count     n9,str10
;field is 10 bytes and squant is only 9 if this occurs do not apply   
          If (n9 > c9)
                    noreturn  
                    goto      badqty    
          Endif
.         move      str13 to ShippedQty
          move      str10 to ShippedQty
.end patch 1.23
          call      zfillit using ShippedQty
        TYPE        ShippedQty
        IF NOT EQUAL
          noreturn
                    GOTO      BADQTY         
          Endif
          Move      Shippedqty to SQUANT          
          return
QuantityClean4
          return    

GetShipDescription  
.//Ship Description
                    Goto GetShipDescription1 if (COMPHOLD = "009387")    .PIDI
                    Goto GetShipDescription1 if (COMPHOLD = "009406")    .Donnelley                 
.begin patch 1.26
                    Goto GetShipDescription1 if (COMPHOLD = "010408")    .pdmg
.end patch 1.26

                    Goto GetShipDescription1 if (COMPHOLD = "009410")    .Frontline
                    Goto GetShipDescription1 if (COMPHOLD = "009411")    .Target
.begin patch 1.28
                    Goto GetShipDescription1 if (COMPHOLD = "009412")    .DMI
.end patch 1.28

GetShipDescription1
.//Donnelley,PIDI,FRONTLINE
          Move ShippedInfo,SINFO
          SCAN "EFT" in SINFO
          If   equal
                    Move      "Email" to SINFO
          Endif     
          return                        
GetShipDescription2
.//Target?
          return
ErrorRename

.         Pack    SmtpTextMessage(1),"Error Was ",str50
          Move "DesktopSupportGroup@nincal.com",Mailto
          Move "DesktopSupportGroup@nincal.com",MailFrom
          Move "ComputerRequest@nincal.com",MailFrom
          
          Pack MAILSubjct,"Could Not Rename File.  Status is unknown ",fileDir," for ",b1,FileString          
          append    "error Was ",mailbody
          append    CRLF,mailbody
          append    str50,mailbody
          append    CRLF,mailbody
          append    taskname,mailbody
          append    crlf,mailbody
          append    "renaming from :",mailbody
          append    crlf,mailbody
          append    taskname4,mailbody
          append    crlf,mailbody
          append    "renaming To :",mailbody
          append    crlf,mailbody
          append    taskname5,mailbody
          reset     mailbody
          Call SendMail       
          Return    
.begin patch 1.32
Alert

.         Pack    SmtpTextMessage(1),"Error Was ",str50
          Move "InformationServices@nincal.com",Mailto
          Move "ComputerRequest@nincal.com",MailFrom
           reset      str255          
          Pack MAILSubjct,"InfoGroup File format may have changed ",fileDir," for ",b1,FileString          
          Clear     MAILBody
          Append    "The Shipping Program (nshp0012)",MAILBody
          Append    CRLF,MAILBody
          append    "Defined Header ",mailbody
          append    CRLF,mailbody
          append    "List Name,Order Description,Create Date,Ship Date,Mailer Name,Broker PO #,PO Number,Quantity,Decoy Quantity,Decoy Key,Total Quantity",mailbody
          append    CRLF,mailbody
          append    CRLF,mailbody
          append    "Header in file",mailbody
          append    CRLF,mailbody
           append     str255,mailbody
          append    CRLF,mailbody
          append    CRLF,mailbody
          append    "Input file name",mailbody
          append    CRLF,mailbody
          append    CRLF,mailbody
          append    Inputname,mailbody
          append    CRLF,mailbody
          reset     mailbody
          Call SendMail       
          Return    
.end patch 1.32

SendtoIs
          Move "ComputerRequest@nincal.com",Mailto
          Move "InformationServices@nincal.com",MailFrom
          Move "The Shipping Program Did not Process Any Records.",MAILSubjct
          Clear     MAILBody
          Append    "The Shipping Program (nshp0012) Did not Process Any Records.",MAILBody
          Append    CRLF,MAILBody
          reset     MAILBody
          Call  SendMail
          Return
          
EmailServiceBureau
.begin patch 1.22   
.         Move "Dawn.Michaelis@infoUSA.com",Mailto
          MOVe  "Cheryl.Achee@infogroup.com",MailCC
.         Move "Brandi.Johansen@infoUSA.com",Mailto
.         Move "ComputerRequest@nincal.com",MailFrom
.end patch 1.22     
          Move "Please review these shipping records for validity. They Show a variation of at least 10 percent",MAILSubjct
          Reset SBEmailBody
          Move SBEmailBody,MailBody
.         Call  SendMail                
.end patch 1.22     
          Return
          
          
ServiceBureauEmailBody        
        Append  SBEmailLine,SBEmailBody
        Append  CRLF,SBEmailBody
          return
.Start Patch 1.1 email IS to have them update with appropriate sb info
SendtoIsBadSB
          Move      "InformationServices@nincal.com",Mailto
          Move      "InformationServices@nincal.com",MailFrom
          Move "ComputerRequest@nincal.com",MailFrom
          
          Move      "Service Bureau is sending us inforamtion but is not longer flagged as a Active SB",MAILSubjct
          Pack      MailBody,"LR is ",OLRN," Company Number is: ",COMPNUM," Company Name is ",COMPCOMP,CRLF,"Please Verify and Update if Necessary."
          Call      SendMail
          Return
.End Patch 1.1 email IS to have them update with appropriate sb info
.begin patch 1.35
CheckCOnfirm
           packkey   NNotlr from Shippedlr
           READ      NOTFILE,NNOTLR;;
           If      Over
           clock      time,str8
           move       str8,NNottime
           unpack      sdate into cc,yy,mm,dd
           Pack      NNOTDATE from cc,yy,dash,mm,dash,dd
           FilePi    1;Notfile
           Write   NOTFILE,NNOTLR;NNOTLR,b1,NNOTDATE,b1,NNOTTIME
           endif

           return
.end patch 1.35


FileOpenFail
         DISPLAY   *P1:24,"File Open Error",error,*W2;
         KEYIN     *P20:26,"ERROR WAS ",*DV,str50,",Q for Quit ? ",STR1         
         CMATCH    "Q" TO str1
         GOTO      IOEXIT1 IF EQUAL

IOEXIT1  TRAPCLR   IO
         SHUTDOWN  "CLS"         
    
          stop
ObjError
          Move      "informationservices@nincal.com",MailFrom
          Move "ComputerRequest@nincal.com",MailFrom
          
          Move      "informationservices@nincal.com",Mailto
          move      "NSHP0012 Object trap",MailSubjct
          clear     MailBody
          append    "error:",Mailbody
          append    CRLF,Mailbody
          append    error,Mailbody
          append    CRLF,Mailbody
          Append    "Working On Company : ",Mailbody
          Append    CompComp,Mailbody
          append    CRLF,Mailbody
          Append    CompHold,Mailbody
          reset     mailBOdy
          call      sendmail
          
          GOto      IOExit1

          Include   COMPIO.inc
          Include   CNTIO.INC
        Include NSHPIO.inc
        Include NORDIO.inc
        Include NCNTIO.inc
        Include     NFTP2IO.INC                 
        Include NOWNIO.INC        
        Include COMLOGIC.inc

