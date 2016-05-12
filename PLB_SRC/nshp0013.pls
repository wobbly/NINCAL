PC       EQU       0
         INCLUDE    COMMON.inc
         INCLUDE    CONS.inc
         INCLUDE    NSHP3DD.inc
         INCLUDE    HP.inc
           INCLUDE  COMPDD.inc
           INCLUDE  CNTDD.INC
         INCLUDE    NORDDD.inc
         Include    NINVDD.INC
         INCLUDE    NCNTDD.inc
         Include    NFTP2DD.INC
         Include        NOWNDD.INC
         INCLUDE    WINAPI.inc
         Include    XLS.INC

Release    INIT    "1.02"               DLH      .Excel 2013 *WindowState=xlMinimized
Reldate   Init      "2014 January 22"
.Release  INIT                 "1.01"               DLH Excel
.reldate   Init      "23 April 2009"
.Release  INIT                 "1.00"               DMB 13 SEP 2006 LOL program for all current and future fulfillment Companies
.begin 1.01 to find version of excel  DH 04/02/09
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.end 1.01 to find version of excel

//Flag whether to make row red or not
ColorFlag           Dim 1



CELLPOINT           FORM      5.2
C15                 FORM "15"



Input     File      
SAVE      File      
LOGFILE   Ifile     KEYLEN=6,var=498

InputName Dim       254
OrigNameHold        Dim       254
Count     Form      4
Write     Form      4                                       //Counter for Records Updated
Update    Form      4                                       //Counter for Records Updated
Page      Form      2
Date      Dim       8
Holdmkey  Dim       7
MailerName      Dim  55

skipcnt   Form      5
ChkBillFlag     Dim       1

Skipped   File      var=441
.Not Currently being used

TEMPSHP3VARS        LIST
TEMP3LR                       DIM       6
TEMP3Description    DIM       75
TEMP3Selection                DIM       55
TEMP3OrderQty                 DIM       11
TEMP3RecDate                  DIM       11
TEMP3Declared                 DIM       11
TEMP3Input                    DIM       11
TEMP3Output                   DIM       11
TEMP3Rejects                  DIM       11        
          LISTEND


DimPtr    Dim       ^



HOLDMLR  DIM           6





//datalist for filelisting
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

//Vars for Email Donnelley About Descrepancies.
SBEmailflag         Dim       1
SBEmailBody     DIM       5000
SBEmailLine     DIM       255

//For Dir Listings
.FileString DIM 255
fileDir      DIM 255
NDX        FORM 9

ValidFiles Form     3
//For Fiile Rename
DateString DIM 16

xlColumnWidthDescription      VARIANT

          clock timestamp,timestamp
.         unpack timestamp,str2,yy,mm,dd
.         pack DateString,mm,slash,dd,slash,cc,yy
          Move Timestamp,Datestring
//Program Header Information
         Move      "NSHP0013" TO PROGRAM
         Move      "SB LOL Data Import Program" TO STITLE
         Move      "Names In The News" TO COMPNME
         Move      C0 TO PAGE
//Todays Date         
         CLOCK     DATE TO DATE
         Clock     timestamp to str18
         Unpack    str18 to str2,str16

         Call          PAINT
         Move      C1 TO NORDPATH
//Which Windows Version is This?
         Call      GetWinVer
         
         
//Create the Variant objects for Excel SpreadSheet
//Initialize variables
          create    xlColumnWidth,VarType=VT_R8,VarValue="0.0"
//"1" increment in Excel interface equals "1.3888" in OLE logic
          create    TopMargin,VarType=VT_R8,VarValue="18"             Roughly equals .25 inches:  18 * 1.388 = 25
          create    BottomMargin,VarType=VT_R8,VarValue="36"          Roughly equals .50 inches:  36 * 1.388 = 50
          create    LeftMargin,VarType=VT_R8,VarValue="14"            Roughly equals .25 inches:  18 * 1.388 = 25       
          create    RightMargin,VarType=VT_R8,VarValue="14"           Roughly equals .25 inches:  18 * 1.388 = 25       
          create    xlColumnWidthDescription,VarType=VT_R8,VarValue="60.0"                          
          create    xlRowHeight,VarType=VT_R8,VarValue="15.0"
//         

INPUT        

           Loop
                    Call   NFTP2KS
           Until Over                   
                    If        (NFTP2INFOTYPE = "L")
                               Move        NFTP2COMP to CompHold 
                               Move      NFTP2COMP,COMPFLD            
                               Call      Compkey
                               Clear     CompNhold
                               Move        CompComp,CompNhold                              
                               DISPLAY   *P10:18,"Working On Company : ",CompComp,b5,CompHold                            

//Do file Listing of Directory to get file name to open
                              Create    dlFiles=1:10:1:10,visible=0
                              Pack      taskname,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\LOL\","*.*"
                              Pack      fileDir,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\LOL\"                                                                               
                          dlFiles.Dir giving result using *Filespec=Taskname,*flags=0
                              If (result <> -1 )
                                        For NDX from 0 to result
                                                  dlFiles.GetText giving FileString using *Index=NDX  
//File Prep
                                                  Call        Trim Using FileString
//If record has been applied then skip  - Program will slap Applied_ after it has been applied.                                   
                                                  Scan      "Applied" in FileString
                                                  Goto        NextFile If Equal
                                                  Scan      "applied" in FileString
                                                  Goto        NextFile If Equal
                                                PACK      InputName,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\LOL\",FileString
                                                Trap      FileOpenFail if IO

                                                  Uppercase InputName
                                                  scan        ".XLS",InputName
                                                  If Equal
                                                            reset inputname                                             
//Convert file if necessary
//.Open Excel application
                                                            create  ex
.begin patch 1.02
.                                                  setprop ex,*WindowState=xlMinimized
.end patch 1.02
                                                  setprop ex,*Visible="True"
                                                            setprop ex.CommandBars("Standard"),*Visible="True"
                                                            setprop ex.CommandBars("Formatting"),*Visible="True"
                                                            setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.begin 1.01 get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
.//File Cleanup
          if        (#ver = c1)
                             Pack      XLSNAME,DateString,"_","LOL.xlsx"
          else
                             Pack      XLSNAME,DateString,"_","LOL.xls"
          endif                             
                             Pack      Taskname,"\\nins1\e\data\",XLSNAME                   ."
                             Erase     Taskname
.end 1.01 get exel version info
//.Reset Default of Worksheets found in a Workbook
                                                  getprop ex,*SheetsInNewWorkbook=SheetsDefault
                                                  setprop ex,*SheetsInNewWorkbook=C1
//.Create Workbooks collection          
                                                  getprop ex,*Workbooks=books
                                                  reset InputName
                                                            books.open giving book using *Filename=InputName                                                                                            
                                                            move InputName,OrigNameHold
                                                            scan        ".XLS",InputName
                                                            MOVEFPTR InputName,n9
                                                            Sub c1 from n9
                                                            Setlptr Inputname,n9
                                                            reset InputName                                                       
                                                            pack      Inputname with InputName,".CSV"
                                                  book.saveas giving N9 using *Filename=InputName,*FileFormat=xlCSV                                                                                                                             
                                                  setprop book,*Saved=OTRUE
                                                          destroy book
                                                          destroy books
                                                            setprop ex,*SheetsInNewWorkbook=SheetsDefault
                                                            ex.quit
                                                            destroy ex                                                            
                                                            erase OrigNameHold
                                                            Uppercase filestring
                                                            scan      ".XLS",filestring
                                                            MOVEFPTR filestring,n9
                                                            Sub c1 from n9
                                                            Setlptr filestring,n9
                                                            reset filestring                        
                                                            pack  filestring with filestring,".CSV"                                                   
                                                  Endif
//Open the File                                           
                                                  Add         c1 to ValidFiles
                                                OPEN      INPUT,InputName
                                                  TrapClr   IO
//apply the Records                                         
                                                  DISPLAY   *P10:20,"Working On File : ",FileString

                                                  Call      ApplyLOL

                                                  CLOSE     INPUT                                             
//File Renamed After Application
                                                  Pack      taskname,"Applied_",DateString,"_",FileString
                                                  Pack      taskname4,fileDir,FileString
                                                  Pack      taskname5,fileDir,taskname                                                                                                        
//Rename the file with Applied_ as a prefix                                               
                                                  Trap      ErrorRename giving str50 if IO
                                                            Rename    taskname4,taskname5
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
        COMPARE   C0 TO PAGE
        If Equal
                    If (ValidFiles > 0)        
                    Call SendtoIS
          Endif
          Goto Done
        Endif
        RELEASE
.        book.printout using *ActivePrinter="\\nts0\laser2"
          setprop ex,*Visible="False"
        clear   taskname
        append  "\\nins1\e\data",taskname
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
        Move "DesktopSupportGroup@nincal.com",MailFrom
        Move "File From The LOL Program.",MAILSubjct
.        Call  SendMail                 
.        If (SBEmailFlag = YES)
.         Call EmailServiceBureau
.        Endif
.        Erase XLSNAME

DONE     
        shutdown
        STOP

//
ApplyLOL
        Move                  NO TO ChkBillFlag
        KeyIn                 *P10:6,"APPLY ALREADY BILLED RECORDS ?? ",*uc,*rv,*T5,ChkBillFlag
          Uppercase ChkBillFlag
          If        ((ChkBillFlag <> "N") & (ChkBillFlag <> "Y"))
                    Goto ApplyLOL
          Endif
Qty
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

//Ship Codes                  
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
                              Clear NSHP3VARs
                              Clear TEMPSHP3VARS
                              If (COMPHOLD = "009406") .Donelley
                                        READ      INPUT,SEQ;*cdfon,TEMP3LR,TEMP3OrderQty,TEMP3Output,TEMP3RecDate,NSHP3Comments
.                                       Elseif (COMPHOLD = "009387") .PIDI
.                                       Elseif (COMPHOLD = "009411") .Target - Not CSV Format
.                                       Elseif (COMPHOLD = "009410") .Frontline                               
                              Elseif (COMPHOLD = "009428") .MMI
                                        READ      INPUT,seq;*cdfon,TEMP3LR,TEMP3Description,TEMP3Selection,TEMP3OrderQty,TEMP3RecDate,TEMP3Declared,TEMP3Input,TEMP3Output,TEMP3Rejects
.                                       Elseif (COMPHOLD = "009420") .Consumer Direct - Waiting for Verification
                              Endif
                    Until Over


                    Call      Trim Using TEMP3LR
                              Type      TEMP3LR
                              Goto      NextRead If Not Equal
                    Cmatch    " " TO TEMP3LR         
                    Goto      NextRead IF EOS 
                    Move    TEMP3LR,NSHP3LR
//Create Excel Spreadsheet
                              Move b2,taskname
                    Clear Taskname
                    Add       C1 to RecordCount
                    If (RecordCount = C1)
//Create a new excel worksheet
//.Open Excel application
                                      create  ex
.begin patch 1.02
.                                      setprop ex,*WindowState=xlMinimized
.end patch 1.02
                                      setprop ex,*Visible="True"
                                        setprop ex.CommandBars("Standard"),*Visible="True"
                                        setprop ex.CommandBars("Formatting"),*Visible="True"
                                        setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
//.Reset Default of Worksheets found in a Workbook
                              getprop ex,*SheetsInNewWorkbook=SheetsDefault
                              setprop ex,*SheetsInNewWorkbook=C1
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
                              Clear Taskname
                              
                                        Call SheetSetup
.                                       pack str5 with "L1",":","P1"
                                        Move c1 to curcellnum
                              CALL      HEADER              
                    Endif
//LOL Data         
                    ADD       C1 TO COUNT
                    DISPLAY   *P10:12,"RECORDS IN : ",COUNT
                    Move      NSHP3LR TO NORDFLD
//Check if order exists
                    CALL      NORDKEY
                    If Over
                                        Append "LR Number Not Found.  Record Not added",Taskname
                                        Move Yes,ColorFlag   
                                        Call Detail
                                        Goto NextRead                           
                              Else
//Get mailer Name
                              Pack      Compfld3 FROM OMLRNUM
                              Call      Zfillit using Compfld3
                              CALL      CompKey3
                              Move      CompComp to MailerName
//
                                        If (RecordCount > 1)   .not the first file
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
                                        Move      NSHP3LR,NSHP3FLD
                                        Rep     ZFILL,NSHP3FLD
                                        If      (ChkBillFlag = NO)
                                                            Move      NSHP3FLD,NINVFLD
                                                  Rep       zfill in NINVFLD
                                                  Call    NINVKEY
                                                  If        Not Over
                                                            Add       c1 to skipcnt
                                                                      Append "Order was previously billed.  Program skipped the application of this record",Taskname
                                                                      Move      Yes,ColorFlag                                               
                                                            Call    Detail
                                                            Goto      NextRead                                
                                                  Endif
                              Endif
                                        
                                        If (COMPHOLD = "009428")
                                                  clear str11
                                                  Uppercase Temp3Declared,Str11
                                                  if (str11 = "CANCELLED")
                                                            Move str11,NSHP3Comments
                                                            Clear NSHP3DQTY
                                                            Clear NSHP3RQTY
                                                            Clear NSHP3Date
                                                            Goto WriteOut
                                                  Endif
                                        Elseif (COMPHOLD = "009406")
                                                  move    "," to str1
                                        call    Removechar using TEMP3OrderQty,str1
                                                  Move TEMP3OrderQty,N11
                                                  if (N11 = c0)
                                                            Clear NSHP3DQTY
                                                            Clear NSHP3RQTY
                                                            Clear NSHP3Date
                                                            Goto WriteOut                                               
                                                  endif
                                                  Uppercase NSHP3Comments
                                                  scan "CANCELLED",NSHP3Comments
                                                  if Equal
                                                            reset NSHP3Comments
                                                            Clear NSHP3DQTY
                                                            Clear NSHP3RQTY
                                                            Clear NSHP3Date
                                                            Goto WriteOut                                                                   
                                                  endif
                                        Endif
.Order Qty                                        
                                        CALL      QuantityClean Using TEMP3OrderQty       
.Declared
                                        If (COMPHOLD = "009428")    .MMI
                                                  CALL      QuantityClean Using TEMP3Declared
                                                  Move      TEMP3Declared,NSHP3DQTY                 
                                                  Call    ZFILLIT Using NSHP3DQTY
                                        Else
                                                  Clear   NSHP3DQTY
                                        Endif                                   
.Input
                                        If (COMPHOLD = "009428")    .MMI
                                                  CALL      QuantityClean Using TEMP3Input
                                        Else
                                                  Clear   TEMP3Input                                
                                        Endif
.Output
                                        CALL      QuantityClean Using TEMP3Output         
                                        Move      TEMP3Output,NSHP3RQTY
                                        Call    ZFILLIT Using NSHP3RQTY                                       
.Rejects
                                        If (COMPHOLD = "009428")    .MMI
                                                  CALL      QuantityClean Using TEMP3Rejects
                                        Endif
.RecDate                                                              
                                        Call    DateClean using TEMP3RECDATE
                                        Move      TEMP3RECDATE,NSHP3Date
WriteOut                                
                                        Call      NSHP3TST                                
                                        If        Over                                              
                                                  CALL      NSHP3WRT
                                                  ADD       C1 TO WRITE
                                                  DISPLAY   *P10:14,"RECORDS Written : ",WRITE                                                                                                          
                                                  Pack      Taskname with "New Record Added"                                                                                                            
                                        Else
                                                  Call      NSHP3UPD
                                                  ADD       C1 TO Update
                                                  DISPLAY   *P10:16,"RECORDS Updated : ",Update                                                                                                                   
                                                  Pack      Taskname with "Record Updated"                                                                                                              
                                        Endif                         
                                        Call      Detail
                              Endif      
NextRead       
          Repeat
                    Call      SheetEnd            
          
          
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
                    Pack Cell  with "E",str5
                    Add C1 to CurCellNum
                    Add C1 to CellRowCnt          
                    Move CurCellNum to str5
                    Call Trim Using Str5          
                    Pack Cell1 with "I",str5      
                    Pack CellRange with Cell,":",Cell1
                    sheet.range(CellRange).Merge
                    Clear Str55
                    Move CompNHold,Str55
                    Call      Trim Using str55
                    Pack      Taskname with Str55," LOL Report"
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
        Setprop sheet.range(Cell),*Value="Order Qty",*HorizontalAlignment=AlignCenter                      
          Pack Cell,"D",Str5
        Setprop sheet.range(Cell),*Value="Declared Qty",*HorizontalAlignment=AlignCenter                      
          Pack Cell,"E",Str5
        Setprop sheet.range(Cell),*Value="Received Qty",*HorizontalAlignment=AlignCenter              
          Pack Cell,"F",Str5
        Setprop sheet.range(Cell),*Value="Received Date",*HorizontalAlignment=AlignRight
          Pack Cell,"G",Str5
        Setprop sheet.range(Cell),*Value="Comments",*HorizontalAlignment=AlignLeft,*ColumnWidth=xlColumnWidthDescription                
          Pack Cell,"H",Str5
        Setprop sheet.range(Cell),*Value="",*HorizontalAlignment=AlignLeft,*ColumnWidth=xlColumnWidthDescription                
        
          Move Cell,str9
        Setprop sheet.range(str8,str9).Font,*Name="Arial",*Size="10",*Bold="True" 
        Setprop sheet.range(str8,str9).Borders(8),*LineStyle=1,*Weight=xlBorderWeightMedium        
        Setprop sheet.range(str8,str9).Borders(9),*LineStyle=1,*Weight=xlBorderWeightMedium         

        RETURN

Detail
//Writes detail record out for LOL record
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
         setprop sheet.range(Cell),*Value=NSHP3LR,*HorizontalAlignment=AlignLeft                    
           Pack Cell,"B",Str5        
         setprop sheet.range(Cell),*Value=MailerName,*HorizontalAlignment=AlignLeft
           Pack Cell,"C",Str5
         setprop sheet.range(Cell),*Value=OQTY,*HorizontalAlignment=AlignLeft
           Pack Cell,"D",Str5
         setprop sheet.range(Cell),*Value=TEMP3Declared,*HorizontalAlignment=AlignRight
           Pack Cell,"E",Str5
         setprop sheet.range(Cell),*Value=TEMP3Output,*HorizontalAlignment=AlignRight
           Pack Cell,"F",Str5
         Setprop sheet.range(Cell),*Value=TEMP3RECDATE,*HorizontalAlignment=AlignRight            
           Pack Cell,"G",Str5
         Setprop sheet.range(Cell),*Value=NSHP3COMMENTS,*HorizontalAlignment=AlignLeft 
           Pack Cell,"H",Str5         
           Move Cell,str9                          
           Reset Taskname      
         Setprop sheet.range(Cell),*Value=Taskname,*HorizontalAlignment=AlignLeft,*WrapText=OTRUE             
           If (ColorFlag = Yes)
          Setprop sheet.range(str8,str9).Font,*Color=RGB,*Bold="True",*Italic="True" 
          Move No to ColorFlag
         Endif
.Move this cell back to this position so autofit doesn't affect comment  field formatting         
           Pack Cell,"H",Str5         
           Return

ErrorRename

          Pack    mailbody,"Error Was ",str50
          Move "DesktopSupportGroup@nincal.com",Mailto
          Move "DesktopSupportGroup@nincal.com",MailFrom
          Pack MAILSubjct,"Could Not Rename File.  Status is unknown ",fileDir," for ",b1,FileString          
          Call SendMail       
          Return    

SendtoIs
          Move "InformationServices@nincal.com",Mailto
          Move "InformationServices@nincal.com",MailFrom
          Move "The LOL Program Did not Process Any Records.",MAILSubjct
          Call  SendMail
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


//This notification was turned off but may be necessary at a later date.  I will leave in there for now.
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
          Move      "Accounting@nincal.com",MailFrom
          Move      "This is a Informational e-mail from  the LOL Info program",MAILSubjct
          Clear   MailBody
          Append  "This is a Informational e-mail from the LOL Info program",Mailbody
          Append  CRLF,MailBody
          
        Clear   str25
        Append  "record## " to str25
        Append  olrn to str25
        Append  b1 to str25
        Reset   str25                                                 
          Append  str25,Mailbody
          
          Append  CRLF,MailBody                                                 
          Append  " Your above LR had a LOL qty variance:",Mailbody
          Append  CRLF,MailBody         
        
        Clear   str55
        Append  "Order qty " to str55
        Append  oqty to str55
        Append  ", LOL Input qty ",str55
        Move    NSHP3RQTY to str10
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
//Sheet Formatting          
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

//Don't autofit the hardwired length for description - this column (I) has wrap text enabled.
          Move CurCellNum,Str5
          call trim using str5
          Pack Cell,"H",Str5  
        sheet.range("A1",CELL).Columns.Autofit  
//
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
.List Name
        getprop sheet.range("A9"),*Columns(1)=sortcol
        Pack Cell,"I",str5
.Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
        sheet.range("A9",Cell).sort using *Key1=sortcol,*Order1=1
//
        
        
        
//total records
          Pack Cell,"A",Str5
          Pack Taskname,"Number of Order Received: ",Count
        setprop sheet.range(Cell),*Value=Taskname                   
//Set the height of all the cells
          Pack Cell,"H",Str5        
          Pack CellRange,"A1",":",Cell
          Setprop sheet.Range(CellRange),*RowHeight=xlRowHeight       
          Return


QuantityClean LRoutine DimPtr
.         Goto QuantityClean2 if (COMPHOLD = "009387")    .PIDI
.         Goto QuantityClean2 if (COMPHOLD = "009406")    .Donnelley            
.         Goto QuantityClean1 if (COMPHOLD = "009410")    .Frontline
.         Goto QuantityClean3 if (COMPHOLD = "009411")    .Target

QuantityClean1
//MMI,Donnelley
          move    "," to str1
        call    removechar using DIMPTR,str1
          call    trim using DIMPTR
          count     n9,DIMPTR
          If (n9 > c9)
                    noreturn  
                    goto      badqty    
          Endif     
        TYPE        DIMPTR
        IF NOT EQUAL
          noreturn
                    GOTO      BADQTY         
          Endif
.        Call       zfillit using DIMPTR          
          return



DateClean LRoutine DimPtr
          move DIMPTR to str2
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
          reset DIMPTR
          scan "/" in DIMPTR
          if equal
                    bump DIMPTR
                    move DIMPTR to str2
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
          scan "/" in DIMPTR
          if equal
                    bump      DIMPTR
                    move      DIMPTR to str4
                    type      str4
                    if not equal
                              noreturn
                              goto      baddate             
                    endif
                    Count    n1,str4
                    if (n1 = c2)
                              move str4,yy
                    elseif (n1 = c4)
                              unpack    str4,cc,yy                    
                    else
                              noreturn
                              goto      baddate                                 
                    endif

          endif
          Call      DateCheck 
        pack        Dimptr from cc,yy,mm,dd                 
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
          return    
          
BadQty
           Append "Shipped Quantity is not formatted Properly.  This record will not be applied.",Taskname    
           Move Yes,ColorFlag
           Call Detail        
         GOTO NextRead

BADDATE
           Append "Ship Date not Formatted Properly.  Record Not added.",Taskname
           Move Yes,ColorFlag  
           Call Detail         
         GOTO NextRead
          
          
IOEXIT1  TRAPCLR   IO
         SHUTDOWN  "CLS"         
    
          stop

          Include   COMPIO.inc
          Include   CNTIO.INC
        Include NSHP3IO.inc
        Include NORDIO.inc
        Include     NINVIO.INC        
        Include NCNTIO.inc
        Include     NFTP2IO.INC                 
        Include NOWNIO.INC        
        Include COMLOGIC.inc

