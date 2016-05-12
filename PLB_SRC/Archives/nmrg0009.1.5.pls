PC       EQU       0
         INCLUDE    COMMON.inc
         INCLUDE    CONS.inc
         INCLUDE    NMRGDD.inc
         INCLUDE    HP.inc
           INCLUDE  COMPDD.inc
           INCLUDE  CNTDD.INC
         INCLUDE    NORDDD.inc
         Include    NINVDD.INC
         INCLUDE    NCNTDD.inc
         Include    NFTP2DD.INC
         Include        NOWNDD.INC
         INCLUDE    WINAPI.inc

Release   Init      "1.5"     DLH  MSF/LW Robbins
reldate   Init      "3 Sep 2009"
.Release   Init      "1.4"     DLH excel 2007
.reldate   Init      "24 Apr 2009"
.Release   Init      "1.3"     DLH make SB email part of initial email
.reldate   Init      "14 Aug 2008"
.Release  Init      "1.2"     DLH add wait for email attachment to be ready
.reldate  Init      "07 Aug 2008"
.RElease  INit      "1.11"              JD      added JL to email distribution
.RElease  INit      "1.1"               DLH Object trap
.Reldate  Init      "18 October 2007"
.Release  INIT                "1.00"               DMB 13 SEP 2006 Merge program for all current and future fulfillment Companies
./
./In order to use any of the properties/methods associated with all parent objects
./of the Worksheet, I need to create automation objects for each of them.
./
./Look at Excel Object Model to understand heirarchy.  This can be found in hard
./documentation:  Microsoft Office 2000 Object Model Guide (found in MS Office 2000 Developers Edition).
./Software available via PL/B Designer - create a Container object on a form, create an Excel
./Spreadsheet, right click on Container object and Browse object.  This will invoke the PL/B Object
./Browser, which will give you SOME of the components of the Object Model.  To browse the Object
./Model in its entirety, open Excel.  Under Tools menu select Macro, select Visual Basic Editor.
./In the Visual Basic Editor screen, under the View menu, select Object Browser.  There you can 
./view all of the objects/methods/properties in Excel.  Right clicking on an item will give you
./option to locate Help topics to see specifics.
./
./General heirarchy:
./ Excel Application
./       Workbooks Collection (all open Workbooks)
./               Single Workbook
./                       Worksheets Collection (all Worksheets in this Workbook)
./                               Single Worksheet
./                                       SortColumn (a Single Column in that Worksheet used for sorting)

.to find version of excel  DH 04/02/09

#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER

.to find version of excel
books                         AUTOMATION
book                          AUTOMATION
.......................
./bars    automation
./bar     automation
./menubar INTEGER 2,"0x00000006"
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
Zoom65                                  VARIANT

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

./Formatting vars needed
./This constant was found in the Object Browser in Excel under the Help topic for the
./HorizontalAlignment property of the Range object.
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

./Colors
Red Color
RGB form 24
./Flag whether to make row red or not
ColorFlag Dim 1

.begin patch 1.2
FileCheck FIle
trapcount form      4
.end patch 1.2


XLSNAME    DIM  255


CELLPOINT                     FORM      5.2
C15       FORM "15"

Cell         DIM    5
Cell1        DIM    5
CurCellNum FORM 5
CellRange  DIM   255
CellRowCnt          FORM      "58"
CellRowCnt1         FORM      "54"


Input     File      
SAVE      File      
LOGFILE   Ifile     KEYLEN=6,var=498

InputName Dim       254

Count     Form      4
Write     Form      4                                       //Counter for Records Updated
Update    Form      4                                       //Counter for Records Updated
Diff      Form     10.2
Tenper    Form     10
Page      Form      2
Date      Dim       8
Holdmkey  Dim       7
OrderVar  Form        9                                     //Order Variation - Difference between Order and Input Qty
RIVar               Form        9                                               //Difference Between Received and Input Qty
IRQTY               Dim         1
MailerName      Dim  55

MergeLR   Dim      10
skipcnt   Form      5
fivePER   Form     10
ChkBillFlag     Dim       1

Skipped   File      var=441
.Not Currently being used
.begin patch 1.1
BookCreated         Init      "N"                         .if no do not try to manipulate excel objects as they were not created
.end patch 1.1

TEMPMRGVARS         LIST
./for pidi,frontline
DPVomits dim           8 
iqty     dim       8
convers  dim       8
ncoa     dim       8
dma      dim       8
intra    dim       8
nonpers  dim       8
net      dim       8
elim     dim       8
notused  dim       8
./

./New for frontline
Merror    dim     8
Dead              dim         8
Zip4                dim       8
Prison              dim     8
Conversion          dim     8
Disaster  dim     8                                                             

          LISTEND






HOLDMLR  DIM           6





./datalist for filelisting
dlFiles       datalist
Taskname4  DIM 510
Taskname5  DIM 510


HoldQuant       Dim       11

QTYVAR          Form      10.2

Okstats   Init      "0B"

Variation Form      3.2
RecordCount     Form      10
CompHold        DIM       6
CompNHold DIM       55

./Vars for Email Donnelley About Descrepancies.
SBEmailflag         Dim       1
SBEmailBody     DIM       5000
SBEmailLine     DIM       255

./For Dir Listings
FileString DIM 255
fileDir      DIM 255
NDX        FORM 9

ValidFiles Form     3
./For Fiile Rename
DateString DIM 16
          clock timestamp,timestamp
.         unpack timestamp,str2,yy,mm,dd
.         pack DateString,mm,slash,dd,slash,cc,yy
          Move Timestamp,Datestring
        create  Red=255:0:0
          getitem   Red,0,RGB
          Trap      OBJError giving error if object

         Pack      Str45,NTWKPATH1,"text\mrgeskip.ped"
         Open      skipped,STR45

         OPEN      LOGFILE,"Shipfax"         *TURNED On 12/08/94.
./Program Header Information
         Move      "NMRG0009" TO PROGRAM
         Move      "Merge Data Import Program" TO STITLE
         Move      "Names In The News" TO COMPNME
         
         
         Move      C0 TO PAGE
./Todays Date         
         CLOCK     DATE TO DATE
         Clock     timestamp to str18
         Unpack    str18 to str2,str16

         Call          PAINT
         Move      C1 TO NORDPATH
./Which Windows Version is This?
         Call      GetWinVer
         
         
./Create the Variant objects for Excel SpreadSheet
./Initialize variables
          create  Zoom65,VarType=VT_I4,VarValue=65
          create    OTRUE,VarType=VT_BOOL,VarValue=1
          create    OFALSE,VarType=VT_BOOL,VarValue=0
          create    xlColumnWidth,VarType=VT_R8,VarValue="0.0"
          create    xlColumnWidthGross,VarType=VT_R8,VarValue="10.5"  
          create    xlColumnWidthMailer,VarType=VT_R8,VarValue="40.5" 
          create    xlColumnWidthPrice,VarType=VT_R8,VarValue="9.0"             
          create    xlColumnWidthDescription,VarType=VT_R8,VarValue="60.0"                          
./"1" increment in Excel interface equals "1.3888" in OLE logic
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

./         

INPUT        

           Loop
                    Call   NFTP2KS
           Until Over                   
                    If        (NFTP2INFOTYPE = "M")
                               Move        NFTP2COMP to CompHold 
                               Move      NFTP2COMP,COMPFLD            
                               Call      Compkey
                               Clear     CompNhold
                               Move        CompComp,CompNhold                              
                               DISPLAY   *P10:18,"Working On Company : ",CompComp,b5,CompHold                            
./File Cleanup
.begin patch 1.4
.                             Pack      XLSNAME,DateString,"_","Merge.xls"
.                             Pack      Taskname,"\\nins1\e\data\",XLSNAME
.                             Erase     Taskname
.end patch 1.4

./Do file Listing of Directory to get file name to open
                              Create    dlFiles=1:10:1:10,visible=0
                              Pack      taskname,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\MERGE\","*.*"
                              Pack      fileDir,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\MERGE\"                                                                             
                          dlFiles.Dir giving result using *Filespec=Taskname,*flags=0
                              If (result <> -1 )
                                        For NDX from 0 to result
                                                  dlFiles.GetText giving FileString using *Index=NDX  
./File Prep
                                                  Call        Trim Using FileString
./If record has been applied then skip  - Program will slap Applied_ after it has been applied.                                   
                                                  Scan      "Applied" in FileString
                                                  Goto        NextFile If Equal
.begin dave 26apr2007
                                                  Scan      "applied" in FileString
                                                  Goto        NextFile If Equal
.end dave 26apr2007
.Begin dave 09sep09                                                  
                                                  Reset     Filestring
.end dave 09sep09                                                  
                                                PACK      InputName,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\MERGE\",FileString
                                                Trap      FileOpenFail if IO
./Open the File                                           
                                                  Add         c1 to ValidFiles
                                                OPEN      INPUT,InputName
                                                  TrapClr   IO
./apply the Records                                         
                                                  DISPLAY   *P10:20,"Working On File : ",FileString

                                                  Call      ApplyMerge

                                                  CLOSE     INPUT                                             
./File Renamed After Application
                                                  Clear     taskname
                                                  Clear     taskname4
                                                  Clear     taskname5
                                                  Pack      taskname,"Applied_",DateString,"_",FileString
                                                  Pack      taskname4,fileDir,FileString
                                                  Pack      taskname5,fileDir,taskname                                                                                                        
./Rename the file with Applied_ as a prefix                                               
                                                  Trap      ErrorRename giving str50 if IO
                                                            Rename    taskname4,taskname5
                                                  Trapclr IO
                                                            
NextFile                                          

                                        Repeat
                                        
./Clear out content in Listing
                                        dlFiles.ResetContent
                              Endif
                    Endif

          repeat

./Destroy Object with Listing                                         
          Destroy dlFiles                                   
./
EOJ      
        COMPARE   C0 TO PAGE
        If Equal
                    If (ValidFiles > 0)        
                    Call SendtoIS
          Endif
          Goto Done
        Endif
        RELEASE
        book.printout using *ActivePrinter="\\nts0\laser2"
          setprop ex,*Visible="False"
        clear   taskname
        append  "\\nins1\e\data",taskname
        append  "\",taskname                                    ."
        reset   taskname
        setprop ex,*DefaultFilePath=taskname
        call    Trim using XLSNAME
        pack    taskname,taskname,XLSNAME
............................................
./Would Dislplay File Save as dialog box but for now this is automated.
.        setmode *mcursor=*arrow
.        ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
.        if (taskname <> "0")
.                Movelptr taskname,N9
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
          destroy Zoom65
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
        pack MailAttach,"\\nins1\e\data\",XLSNAME
        Move "DesktopSupportGroup@nincal.com",Mailto
          Move "ComputerRequest@nincal.com",MailFrom
.        Move "DesktopSupportGroup@nincal.com",MailFrom
.begin patch 1.11
        Move "GemmaSpranza@nincal.com",MailCC
.        Move "JohnLacombe@nincal.com,GemmaSpranza@nincal.com,GemmaBarlaan@nincal.com",MailCC
.        Move "GemmaSpranza@nincal.com,GemmaBarlaan@nincal.com",MailCC
        Move "File From The Merge Program.",MAILSubjct
          move      YEs,Mailtrace
.end patch 1.11
.begin patch 1.2
          Move      c0,TrapCount                   .reset

CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,MailAttach,Exclusive          
          Close     FIleCHeck

          call      debug
.begin patch 1.3
        If (SBEmailFlag = YES)
          Call EmailServiceBureau
        Endif
.end patch 1.3

.end patch 1.2

        Call  SendMail                 
.begin patch 1.3
        
.        If (SBEmailFlag = YES)
.         Call EmailServiceBureau
.        Endif
.end patch 1.3
        Erase XLSNAME

DONE     
        shutdown
        STOP

./
ApplyMerge
        Move                  NO TO ChkBillFlag
        KeyIn                 *P10:6,"APPLY ALREADY BILLED RECORDS ?? ",*uc,*rv,*T5,ChkBillFlag
          Uppercase ChkBillFlag
          If        ((ChkBillFlag <> "N") & (ChkBillFlag <> "Y"))
                    Goto ApplyMerge
          Endif
Qty
          If (CompHold = "009406")
                    Move      "I" to IRQTY
          KeyIn     *P10:8,"APPLY (I)nput Qty or (R)eceived Qty ?? ",*uc,*rv,*T5,IRQTY
                    If        ((IRQTY <> "I") & (IRQTY <> "R"))
                              Goto Qty
                    Endif
          Endif



          If (RecordCount > 1)   .>not the first file
          sheets.add using *After=sheet
                    getprop sheets,*Count=sheetindex
          sheets.item giving sheet using sheetindex
          call SheetSetup
          move c1 to curcellnum
          move c0 to CellRowCnt
          Call Header
                    Move C0 to count
          
          Endif

./Ship Codes                  
.                   If (CompHold = "009410")
.                             Move      "F" TO SCODE     .Frontline    9410
.                   Elseif (CompHold = "009411")
.                             Move      "A" TO SCODE     .Target       9411     
.                   Elseif (CompHold = "009406")            
.                             Move      "T" TO SCODE     .Donnelley    9406
.                   Elseif (CompHold = "009387")            
.                             Move      "Z" TO SCODE     .PIDI         9387     
.                   Endif               
          



                    Loop
                                        Clear NMRGVAR
                                        Clear TEMPMRGVARS
                                        If (COMPHOLD = "009406") .Donelley
                                                  READ      INPUT,SEQ;MergeLR,NMRGLNAM,str1,str55,str18,str1,NMRGRQTY,NMRGIQTY,NMRGID,NMRGFam,N8,NMRGELIM,NMRGCS:
                                                                                 NMRGUDUP,NMRGNET,NCOAMNF,NMRGERR,NMRGDISF,NMRGNPER,NMRGDMA,NMRGZ4,NMRGTDMC,NMRGPRIS,NMRGDROP,NMRGCUST:
                                                                                 N8,N8,N8,N8,N8,NMRGHH
./I believe this code if for a file format we no longer Receive - either  way if this code is necessary it must be moved so as not to disrupt the over flag                                                                            
.                                                 call trim using str15
.                                                 Move str15 to NMRGKCOD                                                                               
                                        Elseif (COMPHOLD = "009387") .PIDI
.                                               READ      INPUT,seq;*cdfon,MergeLR,str12,str12,nmrglnam,nmrgiqty,nmrgid,ncoamnf,nmrgnper,DPVomits,nmrgdma,nmrgelim,nmrgudup,nmrgnet                               
                                        READ      INPUT,seq;*cdfon,MergeLr,str12,str12,nmrglnam,IQTY,intra,ncoa,nonpers,DPVomits,dma,elim,notused,net
.                                       Elseif (COMPHOLD = "009411") .Target - Not CSV Format
                                        Elseif (COMPHOLD = "009410") .Frontline                               
                                        READ      INPUT,seq;*cdfon,MergeLr,str1,nmrglnam,str1,IQTY,str1,net,str1,intra,str1,ncoa,str1,Merror,str1,Dead,str1,nonpers,str1,dma,str1,Zip4,str1,Prison,str1,Conversion,str1,Disaster                                                                            
                                        Elseif (COMPHOLD = "009428") .MMI
                                        READ      INPUT,seq;*cdfon,MergeLr,nmrglnam,IQTY,net,intra,ncoa,Merror,Dead,nonpers,dma,Zip4,Prison,Conversion,Disaster                                                                                                               
                                        Elseif (COMPHOLD = "009420") .Consumer Direct - Waiting for Verification
                                        READ      INPUT,seq;*cdfon,str1,MergeLr,nmrglnam,str1,IQTY,Conversion,ncoa,str1,dma,nonpers,intra,str1,str1,str1,str1,net
.begin 1.5
                                        Elseif (COMPHOLD = "000586") .MSF / LW Robbins
                                        call      debug
                                        
                                        READ      INPUT,seq;*cdfon,str1,NMRGLNAM,Iqty,MergeLR,Merror,Zip4,PRison,NCOA,Intra,DMA,net
.end 1.5
                                        Endif
                    Until Over
                              
                    
./For Pidi - DPVOmits

                                                            
.                             move      dpvomits to nmrgdpv
.                             move      c0 to nmrgdpv

                    
                              If (COMPHOLD = "009387") .PIDI
./Common to all no Donnelley Merge
                                        Move      Iqty,nmrgiqty                           
                                        Move      intra,nmrgid
                                        Move      ncoa,ncoamnf
                                        Move      nonpers,nmrgnper
                                        Move      dma,nmrgdma
                                        Move      net,nmrgnet         

                              
                                        Move      elim,nmrgelim
                                        Move      notused,nmrgudup
./rqty does not exist in PIDI Download - we must fake it by putting input qty in received qty field.
                                      Move    nmrgiqty to nmrgrqty                                
.begin patch 1.5
.                              Elseif (COMPHOLD = "009410" or COMPHOLD = "009428" or COMPHOLD = "009420")  .Frontline & MMI & CDI 
                              Elseif (COMPHOLD = "009410" or COMPHOLD = "009428" or COMPHOLD = "009420" or COMPHOLD = "000586")  .Frontline & MMI & CDI & MSF
.end patch 1.5
./Common to all no Donnelley Merge
                                        Move      Iqty,nmrgiqty                           
                                        Move      intra,nmrgid
                                        Move      ncoa,ncoamnf
                                        Move      nonpers,nmrgnper
                                        Move      dma,nmrgdma
                                        Move      net,nmrgnet                                       
                    
                              
                                        Move      Merror,NMRGERR
                                        Move    Dead,NMRGDISF
                                        Move      Zip4,NMRGZ4
                                        Move      Prison,NMRGPRIS
                                        Move      Conversion,nmrgconv
                                        Move      Disaster,nmrgdisa
./rqty does not exist in Frontline/MMI/CDI Download - we must fake it by putting input qty in received qty field.
                                      Move    nmrgiqty to nmrgrqty                                            
                    Endif
                    
                    Call      Trim Using MergeLr
                              Type      MergeLR
                              Goto      NextRead If Not Equal
                    Cmatch    " " TO MergeLR         
                    Goto      NextRead IF EOS 

                    Move    MergeLR,NMRGLR
./Create Excel Spreadsheet
                    Clear     Taskname
                              Clear     RIVAR
                              Clear     OrderVar                                          
                    Add       C1 to RecordCount
                    If (RecordCount = C1)
./Create a new excel worksheet
./.Open Excel application
                                      create  ex
                                      setprop ex,*WindowState=xlMinimized
                                      setprop ex,*Visible="True"
                                        setprop ex.CommandBars("Standard"),*Visible="True"
                                        setprop ex.CommandBars("Formatting"),*Visible="True"
                                        setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.begin patch 1.4
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
                              if        (#ver = c1)
                             Pack      XLSNAME,DateString,"_","Merge.xlsx"
                              else
                             Pack      XLSNAME,DateString,"_","Merge.xls"
                              endif
                             Pack      Taskname,"\\nins1\e\data\",XLSNAME           ."
                             erase          taskname
.end patch 1.4

./.Reset Default of Worksheets found in a Workbook
                              getprop ex,*SheetsInNewWorkbook=SheetsDefault
                              setprop ex,*SheetsInNewWorkbook=C1
./.Create Workbooks collection
                              getprop ex,*Workbooks=books
./.Create/Add a single Workbook
                              books.add
                              books.item giving book using 1
./.Create Worksheets collection
                              getprop book,*Sheets=sheets
./.Create a single Worksheet - we did not need to add it as we set the default above to
./.add one new Worksheet each time a Workbook is created.
                              sheets.item giving sheet using 1
                              Clear Taskname
                                        Call SheetSetup
.                                       pack str5 with "L1",":","P1"
                                        Move c1 to curcellnum
                              CALL      HEADER              
                              move      Yes,BookCreated
                    Endif
./Merge Data         
                    ADD       C1 TO COUNT
                    DISPLAY   *P10:12,"RECORDS IN : ",COUNT
                    Move      NMRGLR TO NORDFLD
./Check if order exists
                              Move      NMRGRQTY,n9
                              Move    NMRGIQTY,n10
                              If (n9 = 0 AND N10 = 0)
                                        Append "Input and Received Qty Are Invalid.  Record Not Added",Taskname
                                        Move      Yes,ColorFlag                                               
                                        Call    Detail
                                        Goto      NextRead            
                              Endif
                              
                    CALL      NORDKEY
                    If Over
                                        Append "LR Number Not Found.  Record Not added",Taskname
                                        Move Yes,ColorFlag   
                                        Call Detail
                                        Goto NextRead                           
                              Else
                                        if (olrn = "631243")
                                                  call debug
                                        endif
                                        Reset   OKSTATS
                                        SCAN      OSTAT IN OKSTATS
                                        If        Not Equal
                                                  Append "NIN Order is not Live or Billed.  Record Not Added.",Taskname
                                                  Move Yes,ColorFlag   
                                                  Call Detail
                                                  Goto NextRead                                                                   
                                        Else
./Check to see if the lr matches the return company - code is incorrect but may be updated after conversion
.                                                 Move    OLON TO NOWNFLD
.                                                 Call      Zfillit Using NOWNFLD
.                                                 Call      NOWNKEY
.                                                 If (OWNCTN <> "")
.                                                           pack      COMPFLD6,OWNCTN
.                                                           rep       zfill,COMPFLD6
.                                                           Move      C1,COMPPATH
.                                                           Move      "Nshp0012-COMPKEY6",Location
.                                                           pack      KeyLocation,COMPFLD6
.                                                           call      COMPKEY6
.                                                           If over
.                                                                     Clear     COMPFLD6
.                                                           Else
.                                                                     If (COMPSVBFLG = "T")
.                                                                               If (COMPNUM <> COMPHOLD)    . Different Service Bureau May be the wrong LR NUM
.                                                                                         Pack Taskname with "The Service Bureau on the order differs from the SB sending us info."           
.                                                                                         Move Yes,ColorFlag                      
.                                                                                         Call Detail
.                                                                                         Goto NextRead
.                                                                               Else
.                                                                                         Clear     COMPFLD6
.                                                                               Endif                         
.                                                                     Else
.                                                                               Clear     COMPFLD6
.                                                                     Endif               
.                                                           Endif                                   
.                                                 Endif
./Get mailer Name
                                        Pack      Compfld3 FROM OMLRNUM
                                        Call      Zfillit using Compfld3
                                        CALL      CompKey3
                                        Move      CompComp to MailerName
./
                                                  If (RecordCount > 1)   .>not the first file
                                                            if (HOLDMLR <> COMPNUM)
                                                                      Call      SheetEnd                                                                                                                          
                                                                      sheets.add using *After=sheet
                                                                      getprop sheets,*Count=sheetindex
                                                                      sheets.item giving sheet using sheetindex
                                                                      call SheetSetup
                                                                      move c1 to curcellnum
                                                                      move c0 to CellRowCnt
                                                                      Call Header
                                                                      Pack str30 with compfld3,"_",RecordCount
                                                                      setprop sheet,*Name=str30
                                                                      Move C0 to count
                                                                      move COMPNUM to HOLDMLR
                                                            Endif
                                                  Else
                                                            Pack str30 with compfld3,"_",RecordCount
                                                            setprop sheet,*Name=str30     
                                                            move COMPNUM to HOLDMLR                                                         
                                                  Endif
                                                  If (COMPHOLD = "009406") .Donelley
                                                            Call      RecInputVar
./Are you using input qty or return qty - Donnelley Only - These appear backward at least to me.                                                      
                                                            If (irqty = "I")
                                                                      Move NMRGRQTY to NMRGIQTY
                                                            Elseif (irqty = "R")
                                                                      Move NMRGIQTY to NMRGRQTY
                                                            Endif                                                       
./.                                               Elseif (COMPHOLD = "009387") .PIDI
./.                                               Move    nmrgiqty,nmrgrqty
./.                                               Elseif (COMPHOLD = "009411") .Target - Not CSV Format
./.                                               Elseif (COMPHOLD = "009410") .Frontline
                                                  Endif
                                                  Move      NMRGLR,NMRGFLD
                                                  Rep     ZFILL,NMRGFLD
                                                  If      (ChkBillFlag = NO)
                                                            Move      NMRGFLD,NINVFLD
                                                  Rep       zfill in NINVFLD
                                                  Call    NINVKEY
                                                  If        Not Over
                                                            Add       c1 to skipcnt
                                                            Call      Skipwrt
                                                                      Append "Order was previously billed.  Program skipped the application of this record",Taskname
                                                                      Move      Yes,ColorFlag                                               
                                                            Call    Detail
                                                            Goto      NextRead                                
                                                  Endif
                                        Endif
                                                  Call      NMRGTST
                                                  If        Over
                                                            CALL      NMRGWRT
                                                            ADD       C1 TO WRITE
                                                            DISPLAY   *P10:14,"RECORDS Written : ",WRITE                                                                                                          
                                                            Pack      Taskname with "New Record Added"                                                                                                            
                                                  Else
                                                            Call      NMRGDEL
                                                            Call      NMRGWRT
                                                            ADD       C1 TO Update
                                                            DISPLAY   *P10:16,"RECORDS Updated : ",Update                                                                                                                   
                                                            Pack      Taskname with "Record Updated"                                                                                                              
                                                  Endif                         
                                                  Call      CheckOrderVar
                                                  Call      Detail

                                        Endif               

                              Endif
NextRead       
          Repeat
                    Call      SheetEnd            
          
          
          Return
./
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
                    Pack Cell  with "E",str5
                    Add C1 to CurCellNum
                    Add C1 to CellRowCnt          
                    Move CurCellNum to str5
                    Call Trim Using Str5          
                    Pack Cell1 with "I",str5      
                    Pack CellRange with Cell,":",Cell1
                    sheet.range(CellRange).Merge
                    
.                   If (CompHold = "009410")
.                             Move      "F" TO SCODE     .Frontline    9410
.                   Elseif (CompHold = "009411")
.                             Move      "A" TO SCODE     .Target       9411     
                    Clear Str55
                    Move CompNHold,Str55
.                   If (CompHold = "009406")                
.                             Move      "Donnelley",str55 
.                   Elseif (CompHold = "009387")            
.                             Move      "PIDI",str55 
.                   Elseif (COMPHOLD = "009410") .Frontline                               
.                             Move    "Frontline",str55
.                   Elseif (COMPHOLD = "009428") .MMI
.                             Move    "MMI",str55           
.                   Elseif (COMPHOLD = "009420") .Consumer Direct - Waiting for Verification                            
.                             Move    "Consumer Direct",str55                                       
.                             
.                   Endif         




                    
                    Call      Trim Using str55
                    Pack      Taskname with Str55," Merge Report"
                  setprop sheet.range(CellRange),*Value=Taskname,*HorizontalAlignment=AlignCenter                   
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
                  pack    str11,"1:","8"
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
        Setprop sheet.range(Cell),*Value="Mailer",*HorizontalAlignment=AlignCenter                          
          Pack Cell,"C",Str5
        Setprop sheet.range(Cell),*Value="MP Number",*HorizontalAlignment=AlignCenter                      
          Pack Cell,"D",Str5
        Setprop sheet.range(Cell),*Value="Order Qty",*HorizontalAlignment=AlignCenter              
          Pack Cell,"E",Str5
        Setprop sheet.range(Cell),*Value="Input Qty",*HorizontalAlignment=AlignRight
          Pack Cell,"F",Str5
        Setprop sheet.range(Cell),*Value="Received Qty",*HorizontalAlignment=AlignLeft
          Pack Cell,"G",Str5
        Setprop sheet.range(Cell),*Value="Variance (IQTY - RQTY)",*HorizontalAlignment=AlignLeft
          Pack Cell,"H",Str5
        Setprop sheet.range(Cell),*Value="Variance (OQTY - IQTY)",*HorizontalAlignment=AlignLeft
          Pack Cell,"I",Str5
        Setprop sheet.range(Cell),*Value="Comments",*HorizontalAlignment=AlignLeft,*ColumnWidth=xlColumnWidthDescription                
          Pack Cell,"J",Str5
        Setprop sheet.range(Cell),*Value="List Description",*HorizontalAlignment=AlignLeft,*ColumnWidth=xlColumnWidthDescription                
          Move Cell,str9
        Setprop sheet.range(str8,str9).Font,*Name="Arial",*Size="10",*Bold="True" 
        Setprop sheet.range(str8,str9).Borders(8),*LineStyle=1,*Weight=xlBorderWeightMedium        
        Setprop sheet.range(str8,str9).Borders(9),*LineStyle=1,*Weight=xlBorderWeightMedium         

        RETURN

Detail
./Writes detail record out for Merge record
           Add C1 to CurCellNum
           Add C1 to CellRowCnt
           If (Page = C1)     
                    If (CellRowCnt > 58)
                              ADD       C1 TO PAGE
                              Move      c0 to CellRowCnt              

.                             Call Header
.                             Add C1 to CurCellNum
.                             Add C1 to CellRowCnt                    
.                                       
                    Endif
           Endif
           Move CurCellNum,Str5
           call trim using str5
           Pack Cell,"A",Str5
           Move Cell,str8
         setprop sheet.range(Cell),*Value=NMRGLR,*HorizontalAlignment=AlignLeft                    
           Pack Cell,"B",Str5        
         setprop sheet.range(Cell),*Value=MailerName,*HorizontalAlignment=AlignLeft
           Pack Cell,"C",Str5
         setprop sheet.range(Cell),*Value=OMLRKY,*HorizontalAlignment=AlignLeft
           Pack Cell,"D",Str5
         setprop sheet.range(Cell),*Value=OQTY,*HorizontalAlignment=AlignRight
           Pack Cell,"E",Str5
         setprop sheet.range(Cell),*Value=NMRGIQTY,*HorizontalAlignment=AlignRight
           Pack Cell,"F",Str5
         Setprop sheet.range(Cell),*Value=NMRGRQTY,*HorizontalAlignment=AlignRight            
           Pack Cell,"G",Str5
         Setprop sheet.range(Cell),*Value=RIVAR,*HorizontalAlignment=AlignLeft 
           Pack Cell,"H",Str5         
         Setprop sheet.range(Cell),*Value=OrderVar,*HorizontalAlignment=AlignLeft          
           Pack Cell,"I",Str5         
           Move Cell,str9                          
           Reset Taskname      
         Setprop sheet.range(Cell),*Value=Taskname,*HorizontalAlignment=AlignLeft,*WrapText=OTRUE             
.begin patch 1.5
           Pack Cell,"J",Str5         
           Move Cell,str9                          
         Setprop sheet.range(Cell),*Value=NmrgLnam,*HorizontalAlignment=AlignLeft,*WrapText=OTRUE             
.end patch 1.5
           If (ColorFlag = Yes)
          Setprop sheet.range(str8,str9).Font,*Color=RGB,*Bold="True",*Italic="True" 
          Move No to ColorFlag
         Endif
.Move this cell back to this position so autofit doesn't affect comment field formatting         
           Pack Cell,"H",Str5         
           Return

ErrorRename

.         Pack    SmtpTextMessage(1),"Error Was ",str50
          Move "DesktopSupportGroup@nincal.com",Mailto
.         Move "DesktopSupportGroup@nincal.com",MailFrom
          Move "ComputerRequest@nincal.com",MailFrom
          Pack MAILSubjct,"Could Not Rename File.  Status is unknown ",fileDir," for ",b1,FileString          
          append    str50,mailbody
          append    crlf,mailbody
          append    taskname,mailbody
          append    crlf,mailbody
          append    "From :",mailbody
          append    crlf,mailbody
          append    taskname4,mailbody
          append    crlf,mailbody
          append    "to :",mailbody
          append    crlf,mailbody
          append    taskname5,mailbody
          append    crlf,mailbody
          
          Call SendMail       
          Return    

SendtoIs
          Move "InformationServices@nincal.com",Mailto
.         Move "InformationServices@nincal.com",MailFrom
          Move "ComputerRequest@nincal.com",MailFrom
          Move "The Merge Program Did not Process Any Records.",MAILSubjct
          Call  SendMail
          Return
          
EmailServiceBureau
.begin patch 1.3
.         Move "ComputerRequest@nincal.com",Mailto
          Move "ComputerRequest@nincal.com",MailCC
.         Move "ComputerRequest@nincal.com",MailFrom
.end patch 1.3
          Move "Please review these Merge records for validity. They Show a variation of at least 10 percent",MAILSubjct
          Reset SBEmailBody
          Move SBEmailBody,MailBody
.         Call  SendMail                
          Return
          
          
ServiceBureauEmailBody        
        Append  SBEmailLine,SBEmailBody
        Append  CRLF,SBEmailBody
          return

FileOpenFail
         DISPLAY   *P1:24,"File Open Error",error,*W2;
         KEYIN     *P20:26,"ERROR WAS ",*DV,str50,",Q for Quit ? ",STR1         
         CMATCH    "Q" TO str1
         GOTO      IOEXIT1 IF EQUAL


SkipWrt
          Write         skipped,SEQ;MergeLR,str1:
                              NMRGLNAM,b2:
                              NMRGKCOD,str1:
                              NMRGRQTY,str1:
                              NMRGIQTY,str1:
                              NMRGTREJ,str1:
                              NMRGID,str1:
                              NMRGNETI,str1:
                              NMRGELIM,str1:
                              NMRGHDRP,str1:
                              NMRGCS,str1:
                              NMRGUDUP,str1:
                              NMRGND,str1:
                              NMRGDUPM,str1:
                              NMRGNET,str1:
                              NMRGZIPV,str1:
                              NMRGZIPC,str1:
                              NMRGZIP4,str1:
                              NCOAMWF,str1:
                              NCOAMNF,str1:
                              NCOATOTM,str1:
                              NIXIEM,str1:
                              NCOAUNM,str1:
                              NCOANFRJ,str1:
                              NCOANIX1,str1:
                              NCOANIX2,str1:
                              NCOANIX3,str1:
                              NMRGERR,str1:
                              NMRGDISF,str1:
                              NMRGNPER,str1:
                              NMRGDMA,str1:
                              NMRGELMX,str1:
                              NMRGZ4,str1:
                              NMRGNIX,str1:
                              NMRGTDMC,str1:
                              NMRGPRIS,str1:
                              NMRGDROP,str1:
                              NCOAREJ,str1:
                              NMRGCUST,str1:
                              NMRGFAM,str1:
                              NMRGHH,str1:
                              str8:               *FAMILY DUPE DROPS/DUP FIELD.
                              str1:
                              nmrgrep:           new field 8/24/95. (DNC)
                              str1:
                              nmrgnnet:            new field 8/24/95. (DNC)
                                                                                                    nmrgdpv:             new field 6/25/04 PIDI
                                                                                                    nmrgfil2         
         
          Return 
CheckOrderVar
         Move                 C0 TO N10
         Move                 OQTY TO N10
         Mult                 ".05" by N10
         Move                 N10 TO fivePER
         Move                 NMRGRQTY TO N10
         Move                 OQTY TO DIFF
         SUB                  N10 FROM DIFF
         COMPARE    C0 TO DIFF
         If Less
                              MULT SEQ BY DIFF         
         Endif
         COMPARE    DIFF TO fivePER
           Clear n9
           Move     NMRGRQTY to n9
           If (DIFF >= 1000 | DIFF <= -1000)
                    Move DIFF,ORDERVAR
                    Move Yes,ColorFlag

           Endif
          return


RecInputVar
          clear   n8
          clear     n9
          clear   n10
          clear   RIVAR
          Move      NMRGRQTY,n9
          Move      NMRGIQTY,n10
          if        (n9 <> n10)
.Difference Between Received and Input Qty
                    clear n8
                    sub n10 from n9,n8
                    if (n8 < c0)
                          MULT          SEQ BY n8
                    endif
                    Move N8,RIVAR                 
          Endif
          Return
./This notification was turned off but may be necessary at a later date.  I will leave in there for now.
SendEmail
        Move    c1 to ncntpath      .set path to read by contact id#
        Move    c3 to ncntlock      .no locks
        Move    ococode to ncntfld
          Clear   cntname
        Clear   str35
          Call      ncntkey
        If          Not Over
          SQUEEZE CNTNAME,CNTNAME
          Endif
          Pack    MailTo,CNTNAME,"@nincal.com"
.         Move      "Accounting@nincal.com",MailFrom
          Move "ComputerRequest@nincal.com",MailFrom
          
          Move      "This is a Informational e-mail from  the Merge Info program",MAILSubjct
          Clear   MailBody
          Append  "This is a Informational e-mail from the Merge Info program",Mailbody
          Append  CRLF,MailBody
          
        Clear   str25
        Append  "record## " to str25
        Append  olrn to str25
        Append  b1 to str25
        Reset   str25                                                 
          Append  str25,Mailbody
          
          Append  CRLF,MailBody                                                 
          Append  " Your above LR had a Merge qty variance:",Mailbody
          Append  CRLF,MailBody         
        
        Clear   str55
        Append  "Order qty " to str55
        Append  oqty to str55
        Append  ", Merge Input qty ",str55
        Move    NMRGRQTY to str10
        Append  str10,str55
        Reset   str55                                                 
          Append  Str55,Mailbody
          
          Append  CRLF,MailBody                                                 
          Append  "Please review & correct as necessary.",Mailbody                                            
          Append  CRLF,MailBody                                                 
          Reset     MailBody

          Call  SendMail      
          Return
SheetSetup
./Sheet Formatting          
          setprop sheet.PageSetup,*Orientation=xlLandscape
          pack taskname," &D",crlf," &P"
          setprop sheet.PageSetup,*RightHeader=taskname 
          setprop sheet.PageSetup,*Zoom=Zoom65
          setprop sheet.PageSetup,*TopMargin=TopMargin
          setprop sheet.PageSetup,*BottomMargin=BottomMargin
          setprop sheet.PageSetup,*FooterMargin=TopMargin
          setprop sheet.PageSetup,*LeftMargin=LeftMargin
          setprop sheet.PageSetup,*RightMargin=RightMargin     
          getprop sheet,*Hpagebreaks=HpageBreaks
          Return

SheetEnd
.begin patch 1.1
          if        (BookCreated = No)                            ..did we create a spreadsheet ??
          return
          endif
.end patch 1.1
          Add C1 to CurCellNum
          Add C1 to CellRowCnt
          If (Page = C1)      
                    If (CellRowCnt > 58)
                              ADD       C1 TO PAGE
                              Move      c0 to CellRowCnt
.                             Call Header
.                             Add C1 to CurCellNum
.                             Add C1 to CellRowCnt                    
                    Endif
          Else
                    If (CellRowCnt > 50)
                              ADD       C1 TO PAGE
                              Move      c0 to CellRowCnt              
.                             Call Header
.                             Add C1 to CurCellNum
.                             Add C1 to CellRowCnt                                                            
                    Endif     
          Endif

./Don't autofit the hardwired length for description - this column (I) has wrap text enabled.
          Move CurCellNum,Str5
          call trim using str5
          Pack Cell,"H",Str5  
        sheet.range("A1",CELL).Columns.Autofit  
./
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
.List Name
        getprop sheet.range("A9"),*Columns(1)=sortcol
        Pack Cell,"I",str5
.Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
        sheet.range("A9",Cell).sort using *Key1=sortcol,*Order1=1
./
        
        
        
./total records
          Pack Cell,"A",Str5
          Pack Taskname,"Number of Order Received: ",Count
        setprop sheet.range(Cell),*Value=Taskname                   
./Set the height of all the cells
          Pack Cell,"H",Str5        
          Pack CellRange,"A1",":",Cell
          Setprop sheet.Range(CellRange),*RowHeight=xlRowHeight       
          Return
IOEXIT1  TRAPCLR   IO
         SHUTDOWN  "CLS"         
    
          stop
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

.end patch 1.2
ObjError
          Move      "informationservices@nincal.com",MailFrom
          Move      "informationservices@nincal.com",Mailto
          Move "ComputerRequest@nincal.com",MailFrom
          move      "Nmrg0009 Object trap",MailSubjct
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
        Include NMRGIO.inc
        Include NORDIO.inc
        Include     NINVIO.INC        
        Include NCNTIO.inc
        Include     NFTP2IO.INC                 
        Include NOWNIO.INC        
        Include COMLOGIC.inc

