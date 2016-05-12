.//New Program for applying Billing for infogroup & Frontline
pc       Equ       0
         Include   common.inc
         Include   cons.inc                   
         Include   norddd.inc
         Include   compdd.inc
         Include   cntdd.inc
         Include   tinvdd.inc
         Include   consacct.inc
         Include   cvtdd.inc
         Include   nmrgdd.inc
         Include   ndatdd.inc
         Include   nowndd.inc
         Include   nshpdd.inc
         Include   nbildd.inc         
         Include   nacddd.inc
         Include   ninvdd.inc
         INclude   NInvAcddd.inc
         Include   ndat3dd.inc
         Include   nxcgdd.inc
         Include   xls.inc
         Include   nftp2dd.inc


Release    INIT    "2.01"               DLH      .Add Innovaire (rework frontline section)
Reldate    Init    "2015 February 6"
.Release    INIT    "2.00"               DLH      .NEW FORMAT
.Reldate   Init      "2014 July 17"
.Release    INIT    "1.31"               DLH      .Excel 2013 *WindowState=xlMinimized
.Reldate   Init      "2014 January 22"
.RELEASE  Init      "1.3"            DLH Infogroup changed the record layout -- without letting us know -- again
.reldate   Init      "2013 June"
.RELEASE  Init      "1.2"            DLH Infogroup changed the record layout -- without letting us know
.reldate   Init      "2012 December"
.RELEASE  Init      "1.1"            DLH Excel
.reldate   Init      "24 Apr 09"
.RELEASE  Init      "1.0"            DMB  11MAY2005          Added code to account for new triplex billing file
.begin 1.1 to find version of excel  DH 04/02/09
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.end 1.1 to find version of excel
.begin patch 2.0
xlCSV DEFINE 6
ExinputFlag         Dim       1
IDMIFLAG            Dim       1
csvstring           dim       255
UpdateFlag Dim        1
.end patch 2.0


Input                                   file     
Skipcnt                       Form     5
APPLIED                                 Form     5
Selectd                       Form     5
Update                                  Form     5
.//TRIPLEX BILLING VARIABLES.
TDMCAMT                       Form     8.2
runchrg                       Form     9.2        =Running charges
Form122                       Form     12.2
BATCHBR                       Form     1       "0" =NO, "1" = YES.
SALESBR                       Form     2
mrgsw                         Dim      1
shipsw                        Dim      1 

.//PRINT MASK VARIABLES
Mask12                                  Init    "ZZZZZZZZZ.99"                ;formatting vars
Dim12a                                  Dim     12                              ;formatting vars
n92                                     Form      9.2
n102                                     Form      10.2

Page                                    Form    3
Count                         Form    4


.//Excel Vars
C15                                     Form       "15"
CELLPOINT                     Form       5

BillingComments                         DIM       255


.//datalist for filelisting
dlFiles       datalist
Taskname4  DIM 510
Taskname5  DIM 510

.//For Dir Listings
FString      DIM 255
fileDir      DIM 255
NDX        FORM 9

ValidFiles Form     3
.//For Fiile Rename
DateString DIM 16

.//Company Validation and display vars
CompHold        DIM       6
CompNHold DIM       55
CompParentHold  DIM       6
.//FileName
InputName Dim       254

RecordCount     Form      10

SKIPFLAG  DIM         1
           Clock timestamp,timestamp
           Move Timestamp,Datestring

         Move     "Automated Billing Data Import" to STITLE
         Move      "TINV0006" to PROGRAM 
         Move      "NIN" to COMPNME
         Clock     DATE to TODAY
         Call      PAINT
         Pack          STR35,NTWKPATH1,"TDMCBILL.LST"
         splopen   STR35       
         Trap      OOPS if f5
         Move      "Abort" to pf5
         Call      FuncDisp

.//Create the Variant objects
.//Initialize variables

          create  Zoom85,VarType=VT_I4,VarValue=70
          create    OTRUE,VarType=VT_BOOL,VarValue=1
          create    OFALSE,VarType=VT_BOOL,VarValue=0
          create    xlRowHeight,VarType=VT_R8,VarValue="15.0"
.//"1" increment in Excel interface equals "1.3888" in OLE logic
          create    TopMargin,VarType=VT_R8,VarValue="18"             Roughly equals .25 inches:  18 * 1.388 = 25
          create    BottomMargin,VarType=VT_R8,VarValue="36"          Roughly equals .50 inches:  36 * 1.388 = 50
.//Patch 2.53
          create    xlPageBreakManual,VarType=VT_R8,VarValue="-4135"
          create    xlPageBreakAutomatic,VarType=VT_R8,VarValue="-4105"         
.//Patch 2.53
         
         

        Move      C1 TO NMLRPATH       *SET ACCESS TYPE.
        Move      C1 TO NORDPATH       *SET ACCESS TYPE.
        Move      C1 TO NinvPATH       *SET ACCESS TYPE.    
       
           Loop
                    Call   NFTP2KS
           Until Over                   
                    If        (NFTP2INFOTYPE = "B")
                               Move        NFTP2COMP to CompHold 
                               Move      NFTP2COMP,COMPFLD            
                               Call      Compkey
                               Clear     CompNhold
                               Move        CompComp,CompNhold                              
                               Move        CompMain,CompParentHold                         
                               DISPLAY   *P10:15,*ES,"Working On Company : ",CompComp,b5,CompHold                        
.//File Cleanup
.begin patch 1.1
.                             Pack      XLSNAME,DateString,"_","Billing.xls"
.                             Pack      Taskname,"c:\work\",XLSNAME
.                             Erase     Taskname
.end patch 1.1

.//Do file Listing of Directory to get file name to open
                              Create    dlFiles=1:10:1:10,visible=0
                              Pack      taskname,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\Billing\","*.*"
                              Pack      fileDir,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\Billing\"                                                                           
                          dlFiles.Dir giving result using *Filespec=Taskname,*flags=0
                              If (result <> -1 )

                                        For NDX from 0 to result
                                                  dlFiles.GetText giving Fstring using *Index=NDX  
.//File Prep
                                                  Call        Trim Using Fstring
.//If record has been applied then skip  - Program will slap Applied_ after it has been applied.                                   
.                                                      call       debug
                                                  Scan      "Applied" in Fstring
                                                  Goto        NextFile If Equal
                                                  Scan      "applied" in Fstring
                                                  Goto        NextFile If Equal
.begin patch 2.0
                                                  Scan      "Converted" in FString
                                                  Goto        NextFile If Equal
                                                  Scan      "converted" in FString
                                                  Goto        NextFile If Equal
.                                                  call      debug

                                                  reset     fstring
                                                  move      fstring,taskname
                                                  scan      ".xls" in taskname
                                                  if        equal 
                                                  MOVEFPTR  taskname,n3
                                                  sub       c1 from n3
                                                  setlptr  taskname,n3
                                                  move      yes,ExinputFlag
                                                  reset     fstring
                                                  Call        Trim Using FString
                                                  reset     taskname
                                                  pack      csvstring from taskname,".csv"
                                                  else
                                                  move      No,ExinputFlag
                                                  endif

                                                  reset     fstring
.end patch 2.0




                                                PACK      InputName,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\BILLING\",Fstring
                                                Trap      FileOpenFail if IO
.//Open the File                                           
                                                  Add         c1 to ValidFiles
.begin patch 2.0
.                                                OPEN      INPUT,InputName
                                            if (COMPHOLD = "009406" & ExInputFlag = yes ) .Infogroup

                                                            create  ex
                                                            setprop ex,*Visible="True",*DisplayAlerts=OFALSE       // Hidden & NO dialogs !!
                                                            GETPROP   ex,*Workbooks=books

.                                                  call      debug
.
                                                            Books.Open GIVING Book USING Inputname           .Open XLS file
.
.                           Find The First Sheet From The Sheets Collection
.                                                  call      debug
.
                                                           GETPROP   Book,*SHEETS=Sheets
                                                           Sheets.Item GIVING Sheet USING 1
                                                PACK      InputName,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\Billing\",csvString

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
                                                  
                                                  Pack      taskname,"Converted_",DateString,"_",FString
                                                  Pack      taskname4,fileDir,FString
                                                  Pack      taskname5,fileDir,taskname                                                                                                        
//Rename the file with Converted_ as a prefix                                               
                                                  Trap      ErrorRename giving str50 if IO
                                                            Rename    taskname4,taskname5
                                                  Trapclr IO
.>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
.                                                                 goto Done

                                                  Move      csvstring,Fstring
                                                OPEN      INPUT,InputName
                                                  Trapclr IO
                                                  
                                                  else
                                                OPEN      INPUT,InputName
                                                  endif
.end patch 2.0
                                                  TrapClr   IO
.//apply the Records                                         
                                                  DISPLAY   *P10:16,"Working On File : ",Fstring
                                                  Call      ApplyBilling

                                                  CLOSE     INPUT                                             
.//File Renamed After Application
.                                                       call       debug
                                                  Pack      taskname,"Applied_",DateString,"_",Fstring
                                                  Pack      taskname4,fileDir,Fstring
                                                  Pack      taskname5,fileDir,taskname                                                                                                        
.//Rename the file with Applied_ as a prefix                                               
                                                  Trap      ErrorRename giving str50 if IO
                                                            Rename    taskname4,taskname5
                                                  Trapclr IO
NextFile                                          
                                 Repeat
                                        
.//Clear out content in Listing
                                        dlFiles.ResetContent
                              Endif
                    Endif

          repeat

.//Destroy Object with Listing                                         
          Destroy dlFiles                                   
.//
EOJ
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
        Display   *p2:23,"Please wait I'm Printing to Laser 2!!!!!"
          Move CurCellNum,Str9
          call trim using str9
          Pack Cell,"A",Str9
          Pack Taskname,"Number of Order Received: ",rECORDCount
        setprop sheet.range(Cell),*Value=Taskname                   
          Pack CellRange,"A1",":",Cell
          Setprop sheet.Range(CellRange),*RowHeight=xlRowHeight        
.DH testing 2016 MArch 15
          Pack CellRange,"B1",":",Cell
        call debug
        sheet.range(CellRange).Columns.Autofit  
.DH testing 2016 MArch 15
        RELEASE
        book.printout using *ActivePrinter="\\NINS2\laser2"
          setprop ex,*Visible="False"
        clear   taskname
        append  "c:\work",taskname
        append  "\",taskname                   ."
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
          destroy Zoom85
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
        pack MailAttach,"c:\work\",XLSNAME                ."
        Move "GemmaSpranza@nincal.com",Mailto
        Move "ComputerRequest@nincal.com",MailFrom
        Move "File From The Billing Import Program.",MAILSubjct
        Move "There is an attachment with this email",MAILBody
        Call  SendMail                 
          Erase MailAttach        

DONE     
        pause     "5"
        shutdown
        STOP
;............................................................................
ApplyBilling

        Move                  NO TO SKIPFLAG
        KeyIn                 *P10:6,"APPLY SKIPPED RECORDS ?? ",*uc,*rv,*T5,SKIPFLAG
          Uppercase SkipFlag
          If        ((SkipFlag <> "N") & (SkipFlag <> "Y"))
                    Goto ApplyBilling
          Endif




          Loop
                    If (COMPHOLD = "009406") .Donelley
.                      call       debug
                                 MOve    yes,skipflag
.                              Read      Input,seq;*CDFON,TINVLR,TINVDATE,TINVINV,TINVDOLR           
.                              Read      Input,seq;*CDFON,Str55,TINVLR,TINVDATE,TINVINV,TINVDOLR           
.                              Read      Input,seq;*CDFON,Str55,TINVLR,TINVINV,TINVDATE,TINVDOLR           
.begin patch 2.0
.                              Read      Input,seq;*CDFON,Str55,TINVLR,tinvinv,str10,TINVDOLR           
.march 2015 18 ARGH                              Read      Input,seq;*CDFON,Str55,TINVLR,Olstname,mcomp,tinvord,tinvinv,str10,TINVDOLR           
                              Read      Input,seq;*CDFON,str6,TINVLR,Olstname,mcomp,tinvord,tinvinv,str10,TINVDOLR           
.end patch 2.0
.begin patch xxx Tinvdate is defined as ccyymmdd they have started sending mm/dd/ccyy   DLH June 2013
.          call debug
          explode   str10,"/",mm,dd,str4
          call      Trim using mm
          count     n2,mm
          if        (n2 = c1)
          pack      str2 from c0,mm
          move      str2,mm
          rep       zfill,mm
          endif
          call      Trim using dd
          count     n2,dd
          if        (n2 = c1)
          pack      str2 from c0,dd
          move      str2,dd
          rep       zfill,dd
          endif
          pack      tinvdate using str4,mm,dd
.end patch  xxx        

                    Elseif (COMPHOLD = "009387") .PIDI
                    Elseif (COMPHOLD = "009410") .Innovaire (was frontline)                     
.begin patch 2.01
.                              Read      Input,seq;*CDFON,TINVDATE,str1,str1,TINVLR,str1,str1,str1,str1,TINVDOLR      
.list name,LR,shipdate,mailer,qty,rate,$,base (/m),$,subtot,qty,ship$,subtot,Grandtotal,list code, their inv#
.           call debug
                              Read      Input,seq;*CDFON,Olstname,Tinvlr,STR10,Mcomp,str1,str1,str1,str1,str1,str1,str1,str1,str1,str1,str1,TINvDolr,str1,str1,TINVinv      



          explode   str10,"/",mm,dd,str4
          call      Trim using mm
          count     n2,mm
          if        (n2 = c1)
          pack      str2 from c0,mm
          move      str2,mm
          rep       zfill,mm
          endif
          call      Trim using dd
          count     n2,dd
          if        (n2 = c1)
          pack      str2 from c0,dd
          move      str2,dd
          rep       zfill,dd
          endif
          pack      tinvdate using str4,mm,dd
.end patch 2.01
                    Elseif (COMPHOLD = "009428") .MMI
                    Elseif (COMPHOLD = "009420") .Consumer Direct - Waiting for Verification
                    Endif
          Until     Over
                    Add                 C1 to Count         
                  Type                  TINVLR
                  Goto                  NextRead if not equal
                   if (COMPHOLD = "009410") .Innovaire (was frontline)                     
                      packkey  Nordfld,Tinvlr
                      rep        zfill,tinvlr
                      move       c1,nordpath
                      move       c3,nordlock
                      call       Nordkey
                      if         Not over
                                 packkey    NDAT3FLD,olnum
                                 REP       ZFILL IN NDAT3FLD
                                 call       NDAT3key
                                 goto       Nextread if over       .we dont bill
                                 if         (NDATTDMC = b1 or ndattdmc = "")       . WE DON'T BILL
                                 goto       nextread
                                 Elseif     (ndat3cde <> "R")                      .Not Innovaire
                                 goto       nextread
                                 endif
                      else
                      goto       Nextread                     .no valid order
                      endif
                   endif

                    Fill    B1,BillingComments
                    Clear     BillingComments
                    Add       C1 to RecordCount
                    If (RecordCount = C1)                 
.//Create a new excel worksheet
.//.Open Excel application
                            create  ex
.begin patch 1.31
.                            setprop ex,*WindowState=xlMinimized
.end patch 1.31
                            setprop ex,*Visible="True"
                              setprop ex.CommandBars("Standard"),*Visible="True"
                              setprop ex.CommandBars("Formatting"),*Visible="True"
                              setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.begin 1.1 get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
.//File Cleanup
          if        (#ver = c1)
          Pack      XLSNAME,DateString,"_","Billing.xlsx"
          else
          Pack      XLSNAME,DateString,"_","Billing.xls"
          endif
          Pack      Taskname,"c:\work\",XLSNAME                       ."
          Erase     Taskname
.end 1.1 get exel version info
          

.//........................
.//.       setprop ex,*AltStartupPath="C:\Documents and Settings\aharkin\application data\microsoft\office"
.//.       setprop ex,*DisplayFullScreen=OTRUE
.//........................
.//.Reset Default of Worksheets found in a Workbook
                            getprop ex,*SheetsInNewWorkbook=SheetsDefault
                            setprop ex,*SheetsInNewWorkbook=C1
.//........................
.//        getprop ex,*CommandBars=bars
.//        getprop   bars,*ActiveMenuBar=bar
.//        setprop   bar,*Visible="True"
.//        setprop   bar,*Position=menubar
.//........................
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
                            setprop sheet.PageSetup,*Orientation=xlLandscape
                            setprop sheet.PageSetup,*CenterFooter=" Page &P of &N"
.//.START PATCH 1.3 ADDED LINE BACK IN - IT WAS ORIGINALLY COMMENTED OUT
                            setprop sheet.PageSetup,*Zoom=Zoom85
                            setprop sheet.PageSetup,*TopMargin=TopMargin
                            setprop sheet.PageSetup,*BottomMargin=BottomMargin
                            setprop sheet.PageSetup,*FooterMargin=TopMargin
                              getprop sheet,*Hpagebreaks=HpageBreaks  
                              move c1 to curcellnum                             
                    Call      Header            
                  Endif
                  
                  
                  Squeeze     TINVDOLR,TINVDOLR,"$"
                  Call                  Trim using TINVDOLR
                    Move                mask12 to dim12a
                  Move                  tinvdolr to n92
                  Edit        n92 to dim12a         
                  Squeeze     dim12a,dim12a,"."
                  Squeeze     dim12a,dim12a,"$"         
                  Call                  trim using dim12a
                  Move                  dim12a to tinvdolr
                  Call        zfillit using tinvdolr
                  Call        Trim using tinvdolr
                  Add         c1 to n4
                  Display         *p12:10,"Records in :",n4
                  Move                  TINVLR TO TINVFLD
                  Rep                   ZFILL IN tinvfld
                   move          No,Updateflag
                    Call                TINVTST
                    If                  OVER
                                        CALL      TINVWRT
                                        ADD       C1 TO APPLIED
                  Else
                              If (SKIPFLAG <> YES)
                                                  Move      "Already in File!!" to MCOMP
                                                  Add       C1 to SKIPCNT
                                                  Pack      BillingComments using BillingComments,".SKIP - Already Applied. "      
.                                                  Append  "SKIP - Already Applied. ",BillingComments
....New code
                                            Elseif (COMPHOLD = "009406") .Infogroup  replace the record and let Billing know
                                            move       TinvDOlr,TinvdolrH
                                            move       TinvDate,TinvDateH
                                            move       tinvinv,tinvinvH
                                            call       Tinvkey
                                            move       "GemmaSpranza@nincal.com",Mailto
                                            move       "ComputerRequest@nincal.com",Mailfrom
                                            move       "Change in InfoGroup Billing",mailsubjct
                                            Clear      Mailbody
                                            append     "Here is the old bill",mailbody
                                            append     Crlf,Mailbody
                                            append     "LR ",mailbody
                                            append     tinvfld,mailbody
                                            append     Crlf,Mailbody
                                            append     tinvdolr,mailbody
                                            append     Crlf,Mailbody
                                            append     tinvdate,mailbody
                                            append     Crlf,Mailbody
                                            append     tinvinvh,mailbody
                                            append     Crlf,Mailbody
                                            reset      mailbody
                                            call       sendmail
                                            move       TinvDateH,TinvDate
                                            move       tinvinvH,tinvinv
                                            Move      tinvdolrh,tinvdolr
                                                  CALL      TINVUPD
                                                  ADD       C1 TO Update                                       
                                                  move yes,Updateflag
.... end New code
                                        Else
                                                  CALL      TINVUPD
                                                  ADD       C1 TO Update                                                                                                  
                                        Display         *p12:10,"Records Updated :",Update
                                        Endif
                  Endif
                  Move        tinvlr to nordfld
                  Move        "" to mcomp
                  Clear       str2
                  Move        C1 TO NORDPATH       *SET ACCESS TYPE.
                  Call        nordkey
                  If          not over
                    PACK      MKEY FROM OMLRNUM,OCOBN
                            REP         ZFILL IN MKEY
                              CALL      NMLRKEY
                    Else
                              Pack      BillingComments using BillingComments,"No Associated Order. "
.                              Append  "No Associated Order. ",BillingComments             
                    Endif
Det   
                  Move        Tinvlr to ninvfld        
                  Move        Tinvlr to nmrgfld
                  Move        Tinvlr to nshpfld         
                  Call        Ninvkey
                  If Over
                              if (Ostat = "B" or Ostat = "Q")
                              Pack      BillingComments using BillingComments,"Billed LM Exch"
.                              append  "Billed LM Exch",BillingComments            
                              else   
                              Pack      BillingComments using BillingComments,"Not Billed. "
.                              Append  "Not Billed. ",BillingComments            
                              endif
                  Endif
                  Move        Olon to nownfld
                  MOVE        PPM TO CMPT92
                  Divide      HUND INTO CMPT92
                  MOVE        CMPT92 TO FORM32
                  Call    READMLR
                  Move    c1 to nownpath
                  Move    olon to nownfld
                  Call    nownkey
                  Rep     ZFILL IN NMRGFLD
                  Move    c0 to nmrgrqty
                  Move    c0 to nmrgiqty
                  Move    c0 to nmrgnet
                  Move    olrn to nshpfld
                  Move    no to shipsw
                  Call    NshpKEY                    ;on line ship data
                  If      not over
                    Move      yes to shipsw
                  Endif
                  Move      no to mrgsw
                  CALL    NMRGKEY                    ;on line merge data
                  If          Not over
                    Move      yes to mrgsw
                  Endif
                  Call    nownkey
                  PACK    MKEY FROM OMLRNUM,OCOBN
                  REP     ZFILL IN MKEY
                  Call    NMLRKEY
                  Move    c2 to tdmcflag     .force compute to calc tdmc goodies
                  
                  Call    NInvAcdRecClear
                  Clear   NInvAcdfld
                  Packkey NInvAcdFld from Invnum
                  Call    NinvAcdTst
                  Call    NInvAcdRecLoad          
                  
                  Call    COMPUTE
                  PACK        MKEY FROM omlrnum,OCOBN
                  REP         ZFILL IN MKEY
                  MOVE        C0 TO BATCHBR
                  CALL        NMLRKEY

.//Fulfillment Total Charge
                   ADD        C1 TO selectd
                   Move       tinvdolr to form122
                   Mult       ".01" by form122
         Move       c0 to TDMCAMT
                   Add        Form122 to TDMCAMT
.//Billed for running Charges -prog 19 charges
                   Move      c0 to runchrg
                   Move      tinvlr to nxcgfld
                   Rep       zfill in nxcgfld
                   Call      nxcgkey
                   If Not Over
                    Loop
                              Add       nxcgar to runchrg                       
                                        Call      nxcgks                        
                    Until Over                              
                              Until (nxcglr <> nxcgfld)               
                    Repeat
                   
                   Endif
.//Commented out and Moved up above - End
.//tdmcamt = $ from Triplex invoice file
.//Runrar = total receivable $ from triplex related additional charges
.//runrpass = receivables from Run Charge, commisionable selects
.//runrflat = receivables from Flat charges ie Mag tape, Shipping, etc.
.//IncRFLat   = Profit from flat charges (tdmcamt - runrpass = tdmc flat charges 'TDMCFLAT'; 
.//runrflat - tdmcflat = IncRflat)         

                    Call LoadCells            
                    Move       c0 to runrar
                    Move       c0 to runrlr
                    Move       c0 to runrpass
                    Move       c0 to runrflat
                    DISPLAY   *ef,*P10:14,"Records Skipped : ",skipcnt
                    DISPLAY   *ef,*P10:12,"Records Selected: ",selectd
NextRead 
          if        (count - recordcount > 45)   .more than 45 blank records get out
          break
          endif

          Repeat    

          Return

HEADER

          ADD       C1 TO PAGE
          Move      c0 to CellRowCnt    
          If (CurCellNum = C1)
                    Move C1 to CellRowCnt                   
                    Move CurCellNum to str9
                    Call Trim Using Str9
                    Pack Cell  with "L",str9
                    setprop sheet.range(Cell),*Value=Today,*HorizontalAlignment=AlignRight          
                    Mult CurCellNum,C15,CELLPOINT
                    sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,190,60         
                    Pack Cell  with "E",str9
                    Add C1 to CurCellNum
                    Add C1 to CellRowCnt          
                    Move CurCellNum to str9
                    Call Trim Using Str9          
                    Pack Cell1 with "I",str9      
                    Pack CellRange with Cell,":",Cell1
                    sheet.range(CellRange).Merge  
                  setprop sheet.range(CellRange),*Value="Fulfillment Charges Report",*HorizontalAlignment=AlignCenter                   
                  setprop sheet.range(CellRange).Font,*Bold="True"         
                  setprop sheet.range(CellRange).Font,*Name="Arial"        
                  setprop sheet.range(CellRange).Font,*Size="16"
                  setprop sheet.range(CellRange),*HorizontalAlignment=xlCenter      
                  setprop sheet.range(CellRange),*VerticalAlignment=xlCenter 
                    Add c5 to CurCellNum
                    Add C5 to CellRowCnt          
                    Move CurCellNum to str9
                    Call Trim Using Str9
                    Pack Cell with "A",str9
                  setprop sheet.range(Cell),*Value="Confidential"
                  setprop sheet.range(Cell).Font,*Bold="True"                 
                  sheet.range(CellRange).BorderAround using *LineStyle=1,*Weight=2                  
                  pack    str11,"1:","7"
          setprop sheet.PageSetup,*PrintTitleRows=str11               
                    Add c1 to CurCellNum
                    Add c1 to CellRowCnt                    
          Endif
          Move CurCellNum to str9
          Call Trim Using Str9          
          Pack Cell with "A",str9
          Move Cell to str8
        setprop sheet.range(Cell),*Value="Mailer ##"
          Pack Cell,"B",str9
        setprop sheet.range(Cell),*Value="Mailer Name"
          Pack Cell,"C",str9  
        setprop sheet.range(Cell),*Value="Broker Name"
          Pack Cell,"D",str9  
        setprop sheet.range(Cell),*Value="LR Number"
          Pack Cell,"E",str9  
        setprop sheet.range(Cell),*Value="Invoice Number",*HorizontalAlignment=AlignRight   
          Pack Cell,"F",str9  
        setprop sheet.range(Cell),*Value="Invoice Date",*HorizontalAlignment=AlignRight   
          Pack Cell,"G",str9
        setprop sheet.range(Cell),*Value="Charges",*HorizontalAlignment=AlignLeft           
          Pack Cell,"H",str9  
        setprop sheet.range(Cell),*Value="NIN INCOME",*HorizontalAlignment=AlignRight   
          Pack Cell,"I",str9  
        setprop sheet.range(Cell),*Value="Run Charge",*HorizontalAlignment=AlignRight           
          Pack Cell,"J",str9  
        setprop sheet.range(Cell),*Value="We Billed",*HorizontalAlignment=AlignRight           
       
          Pack Cell,"K",str9  
        setprop sheet.range(Cell),*Value="Billing Variation",*HorizontalAlignment=AlignLeft
        
          Pack Cell,"L",str9
        setprop sheet.range(Cell),*Value="Comments",*HorizontalAlignment=AlignLeft
        
          Pack cellrange,"A",str9,":",Cell
          setprop sheet.range(CellRange).Font,*Bold="True"        
        sheet.range(CellRange).Columns.Autofit                        
        RETURN





READMLR  
          Pack      MKEY FROM OMLRNUM,OCOBN
        Call      NMLRKEY
        Move      MCOMP TO BILCOMP
        Clear     Bilname
        Clear     BRCOMP
        Clear     BRaddr
        Clear     BRcity
        Clear     BRstate
        Clear     BRzip
        Clear     NBRKFLD
        Pack      NBRKFLD FROM iBRKNUM,iBRKCNT
        Cmatch    B1 TO NBRKFLD
        return    IF EOS
        Call      Nbrkkey
        If        NOT OVER
                      Move    MComp to BilCOMP
                      Move    BRCOMP to BilNAME
        Endif
        Return



oops     display    *p1:24,*el,*blinkon,"Now you've Done it!!!!!!!",*b,*b,*b,*w10
         stop   


LoadCells
          Add c1 to CurCellNum
          Add C1 to CellRowCnt                              
.          If (CellRowCnt > 46)
          If (CellRowCnt > 54)
                    Call Header
                    Add C1 to CurCellNum
                    Add C1 to CellRowCnt                    
                              
          Endif                                   
          
          Move CurCellNum to str9
          Call Trim Using Str9          
          Pack Cell,"A",str9  
          setprop sheet.range(Cell),*Value=OMLRNUM
          Pack Cell,"B",str9  
          setprop sheet.range(Cell),*Value=BILCOMP
          sheet.range(Cell).Columns.Autofit                                   
          Pack Cell,"C",str9          
          setprop sheet.range(Cell),*Value=BILNAME
          sheet.range(Cell).Columns.Autofit                                   
          Pack Cell,"D",str9          
          setprop sheet.range(Cell),*Value=TINVLR
          Pack Cell,"E",str9          
          setprop sheet.range(Cell),*Value=INVNUM,*HorizontalAlignment=AlignRight   
          Pack Str10,INVDTEM,"/",INVDTED,"/",INVDTEY
          Count n2,str10
          If (n2 = c2)
                    clear str10
          Endif
          Pack Cell,"F",str9          
          setprop sheet.range(Cell),*Value=str10,*HorizontalAlignment=AlignRight            
     
          Pack Cell,"G",str9          
          setprop sheet.range(Cell),*Value=TDMCAMT,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"                        
          
          Pack Cell,"H",str9          
          setprop sheet.range(Cell),*Value=NININC,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"                        

          Pack Cell1,"I",str9         
          setprop sheet.range(Cell1),*Value=runchrg,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"                                               
          
          pack Str25,"=SUM","(",Cell,",",CELL1,")"
          Pack Cell,"J",str9                      
          setprop sheet.range(Cell),*Value=str25,*HorizontalAlignment=AlignRight                   

          Pack Cell,"J",str9                      
          Pack Cell1,"G",str9
          pack Str25,"=","(",Cell,"-",CELL1,")"                       
          Pack Cell,"K",str9                      
          setprop sheet.range(Cell),*Value=str25,*HorizontalAlignment=AlignRight                   
.          Reset BillingComments

          Pack Cell,"L",str9                      
          setprop sheet.range(Cell),*Value=BillingComments,*HorizontalAlignment=AlignRight,*WrapText=OTRUE                     
          
          Clear N92
          Add NININC,runchrg,N92
          If (N92 < TDMCAMT) 
                    Pack cellrange,"A",str9,":",Cell
                    setprop sheet.range(CellRange).Font,*Bold="True",*Color=255
          Endif

.          if         (Updateflag = Yes)
.          Pack Cell,"M",str9
.         setprop sheet.range(Cell),*Value=TINVDOLRH,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"                        
.          endif

          return
ErrorRename

.         Pack    SmtpTextMessage(1),"Error Was ",str50
          Move      "ComputerRequest@nincal.com",MailTo
          Move      "ComputerRequest@nincal.com",MailFrom
          Pack      MAILSubjct,"Could Not Rename File.  Status is unknown ",fileDir," for ",b1,Fstring     
          Clear     MAILBody
          Append    "Could Not Rename File.  Status is unknown ",MAILBody
          append    FileDir,MailBody
          Append    " for  ",Mailbody
          append    Fstring,Mailbody
          Append    CRLF,MAILBody
          Append    "Error was: ",Mailbody
          Append    CRLF,MAILBody
          Append    Str50,Mailbody
          reset     MAILBody
          Call SendMail       
          Return    

SendtoIs
          Move "ComputerRequest@nincal.com",Mailto
          Move      "ComputerRequest@nincal.com",MailFrom
          Move      "The Shipping Program Did not Process Any Records.",MAILSubjct
          Clear     MAILBody
          Append    "The Shipping Program (Tinv0006) Did not Process Any Records.",MAILBody
          Append    CRLF,MAILBody
          reset     MAILBody
          Call  SendMail
          Return    
FileOpenFail
        DISPLAY   *P1:24,"File Open Error",error,*W2;
        KEYIN     *P20:26,"ERROR WAS ",*DV,str50,",Q for Quit ? ",STR1         
        CMATCH    "Q" TO str1
        GOTO      IOEXIT1 IF EQUAL

IOEXIT1  
          TRAPCLR   IO
        SHUTDOWN  "CLS"         
          stop         

        Include  tinvio.inc
        Include  nordio.inc
          Include    compio.inc
          Include    cntio.inc
        Include  nownio.inc
          Include  ninvio.inc
          Include    NinvAcdio.inc
          Include  compute.inc
        Include  nbilio.inc
        Include  nmrgio.inc
        Include  nshpio.inc
        Include  ndat3io.inc
        Include  nxcgio.inc 
        Include  nacdio.inc
        Include  ndatio.inc
        Include  cvt.inc       
        Include  nftp2io.inc        
        Include  comlogic.inc         
