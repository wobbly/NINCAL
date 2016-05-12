.Neom0006 - Month to date list management volume by list vs last year
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         inc       hp.inc
         INCLUDE   NDATDD.inc
          include   Nmdldd.inc

INPUT    FILE    
OUTPUT   FILE      UNCOMP
RElease   Init      "1.0"   DLH New
REldate   Init      "2013 August 08"
AKEY1    INIT      "01R"
AKEY2    INIT      "02R"
ALKEY    DIM       9           LISTKEY
AMKEY    DIM       7
MASK     INIT      "Z,ZZZ,ZZZ,ZZ9"
OQTY1    FORM      9
EQTY     FORM      10
Begyear   form      8
endyear   form      8
Begyear1   form      8
endyear1   form      8
ex        automation      class="Excel.Application"
books     automation
book      automation
sheets    automation
sheet     automation
sheet2              automation          // new sheet
range1              automation
sheetindex          VARIANT
result2   form      8
N15                 form 15
.to find version of excel  DH 04/02/09
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.to find version of excel
.Variant objects used to talk to outside applications
.Booleans
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom85  variant
VT_R8     EQU 5           .Double - 8 byte Real
xlRowHC8         variant
.Formatting vars needed
SheetsDefault       integer 4,"0x00000000"
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlDouble    integer 4, "0x2"
xlAscending integer 4, "0x1"
range     dim       20
range2    dim       20

xlLeft integer 4,"0xffffefDD"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
FileCheck FIle
trapcount form      4
...........................................
. 
.OUTPUT FILE.
..............
.STATUS   DIM       1           1-1
.LSTNUM   DIM       6           2-7
.MLSTNAME DIM       55          8-62
QTY      FORM      5          63-67    .total for current year 
NAMES    FORM      10         68-77       .total for current year 
Enames    Form      10         78-88       .Total Exchange Names
QTY1      form      5          89-93         .total last year
Names1    FOrm      10         94-103          .total last year
Enames1   Form      10        104-113           .total Exchange Names last year
...............................................................................
.PRINT VARIABLES
PAGE     FORM      3
LINES    FORM      2
SIXTY2   FORM      "62"
ANS      DIM       1
MOMATCH  FORM      "03"
ORDMO    FORM      2
PRNTBR   FORM      1
SIX      FORM      "6"
BLNK3    DIM       3
check    form      5
check2   form      5 
orddate  form      5
today1   form      5
COUNT    FORM      5
USED     FORM      5
NOTUSED  FORM      5
.taskname dim      120
.
         MOVE      "Neom0006" TO PROGRAM
         MOVE      "List Usage" TO STITLE
         MOVE      "Names In The News" TO COMPNME
         MOVE      C1 TO NDATPATH  .SET ACCESS TO ISAM
         MOVE      C2 TO NORDPATH
         move      c3 to ndatlock
         move      c3 to NORDLOCK

         UNPACK    today INTO mm,str1,dd,str1,yy           .make sure today is provided by dsprog.dat. should always be last day of month
         CALL      PAINT
         MOVE      "ABORT" TO PF5
         CALL      FUNCDISP
. 
.          goto      output
.Prepare date ranges
          Move      yy to N2
          sub       c1 from N2
          Pack      str8 from cc,N2,mm,dd              .ending date for last year
          move      str8 to Endyear1
          Pack      str8 from cc,N2,"0101"             .start date for last year
          move      str8 to Begyear1
          Pack      str8 from cc,yy,mm,dd                    .ending date for this year
          move      str8 to Endyear
          Pack      str8 from cc,yy,"0101"                   . start date for this year
          move      str8 to Begyear

. OPEN INPUT FILES
OPEN     TRAP      ABORT IF F5
. 
. 
. PREP OUTPUT FILE
. 
         PREPARE   OUTPUT,"\\nins1\e\DATA\ManageVol.dat|NINS1:502",exclusive
         
         move      c4,ndatpath
         move      "000000" to ndatfld
         call       ndattst
. 
. GET NEXT DATA CARD
. 
A100
          move      c4,ndatpath
          move      c3,ndatlock
         CALL      NDATKS
         GOTO      Z900 IF OVER
. 
         DISPLAY   *P1:8,"WORKING ON LIST ",LSTNUM," - ",MLSTNAME
. 
          if        (Elstcde <> "C" and ElstCDE <> "P")           .should be a redundant check as we are only reading exclusives
          goto      A100
          endif

.         cmatch    b1 to ndatoff
.         goto      a100 if not equal

         REP       " 0" IN LSTNUM
         PACK      NORDFLD2,AKEY2,LSTNUM
         CLEAR     NORDFLD1
         CLEAR     NORDFLD3
         CLEAR     NORDFLD4
         move      c3,nordlock
. 
         CALL      NORDAIM
         GOTO      CARDNG IF OVER
         if           (olrn = "813935")
         call         debug
         endif
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      b100a IF EQUAL     YES, skip.
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      b100a IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       LCR order ?
         GOTO      b100a IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       Cancelled LCR order ?
         GOTO      b100a IF EQUAL     YES, skip.
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         goto      b100a if equal
          pack      str2 from osales10,osales
          rep       zfill,str2
.we only want list management orders
.          if        (str2 <> "06" & str2 <> "27" & str2 <> "30")
.          goto      b100a
.          endif

          pack      str8 from oodtec,oodtey,oodtem,oodted
          move      str8,n8
          if        (n8 >= Begyear1 & N8 <= EndYEar)
          goto      B101
          endif


         GOTO      B100A

CARDNG   DISPLAY   *P1:24,*EL,"LIST ",MLSTNAME:
                   *P1:23,*EL,"LIST NUMBER : ",LSTNUM," NOT USED "
         ADD       C1 TO NOTUSED
         DISPLAY   *P12:20,"NUMBER OF LISTS WITH NO USAGE : ",NOTUSED
.         GOTO      C100
         GOTO      A100
. 
. COMPUTE ORDER USAGE
. 
B100
         MOVE      "Z,ZZZ,ZZZ,ZZ9",MASK
         EDIT      NAMES,MASK
. 
         DISPLAY   *P1:9,"ORDERS = ",QTY:
                   *P1:10," NAMES = ",MASK
. 
B100A    CALL      NORDKG
         GOTO      C100 IF OVER
         MOVE      OODTEM TO ORDMO
         if         (Ostat = "p" or Ostat = "x" or Ostat = "l" or Ostat = "z")
         goto       B100a
         endif

         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         goto      b100a if equal
. 
         MATCH     LSTNUM,OLNUM
         GOTO      C100 IF NOT EQUAL
..
.we only want list management orders
          pack      str2 from osales10,osales
          rep       zfill,str2

.          if        (str2 <> "06" & str2 <> "27" & str2 <> "30")
.          goto      b100a
.          endif

          pack      str8 from oodtec,oodtey,oodtem,oodted
          move      str8,n8
          if        (n8 >= Begyear1 & N8 <= EndYEar)
          goto      B101
          endif

. 
         GOTO      B100A
B101     
.check dates add to correct buckets
.CALC EXCHANGE NAMES
          if        (lstnum = "025755")
          call      debug
          endif
          move      c0,Eqty
          move      c0,oqty1
          move      c0,n9  
          RESET     EXCODES
          Move      Oelcode,N1
          SCAN      OELCODE IN EXCODES            EXCHANGE?
          IF        EQUAL                        .exchange
                      move  oexqty,n9
                      if         (n9 = 0)   .100% exchange
                      move       Oqty,Eqty
                      move       Oqty,oqty1
                      else
                      MOVe      Oexqty,Eqty         .split exch portion
                      MOve       Oqty,Oqty1         .split rent portion
                      endif
           Else
          MOVE      OQTY TO OQTY1           .rental
          move      c0,Eqty
          endif 
.          if        (n1 = c1)               .rental
.          MOVE      OQTY TO OQTY1           .entire      
.          move      c0,Eqty
.          Elseif    (n1 = c2)
.          MOVE      OQTY TO OQTY1           .entire      
.          MOVe      Oexqty,Eqty         .split
.          elseif    (n1 = c3)
.          MOVE      OQTY TO OQTY1       .entire
.          MOVe      Oqty,Eqty
.          else
.          MOVE      OQTY TO OQTY1       .entire
.          MOVe      c0,Eqty
.          endif
          
          if        (n8 >= BegYear1 & N8 <= Endyear1)
          ADD       OQTY1,NAMES1
          add       Eqty,ENames1
          ADD       C1,QTY1

          Elseif        (n8 >= BegYear & N8 <= Endyear)
          ADD       OQTY1,NAMES
          add       Eqty,ENames
          ADD       C1,QTY
          ENDIF
. 
         GOTO      B100
. 
. WRITE SUMMARY RECORD IKF USAGE <> 0
. 
C100
. 
          if        (qty > c0 or qty1 > c0)
         WRITE     OUTPUT,SEQ;STATUS,LSTNUM,MLSTNAME,QTY,NAMES,Enames,Qty1,Names1,Enames1
         ADD       C1 TO USED
         DISPLAY   *P12:18,"NUMBER OF LISTS WITH USAGE ",USED
          endif
. 
          MOVE      c0,NAMES
          Move      c0,Enames
          MOVE      c0,QTY
          MOVE      c0,NAMES1
          Move      c0,Enames1
          MOVE      c0,QTY1
. 
. GO DO IT AGAIN
. 
D100
         GOTO      A100
. 
. 
. ABORT - OPERATOR ABORTED JOB. RESULTS NOT VALID
. 
ABORT
         DISPLAY   *P1:24,*EL,*B,*B,"JOB ABORTED, RESULTS NOT VALID",*W5
. 
. CLOSE FILE AND EXIT
. 
Z900
         WEOF      OUTPUT,SEQ
         CLOSE     OUTPUT
PRINT1         clear     taskname
          Pack      Taskname From "\\nins1\e\data\ManageVol.dat,\\nins1\e\DATA\ManageVol.srt,c:\work ":
                    "-8-62"
                    sort      taskname
. 
.............................
.Output
Output
         TRAP      STOP IF F5
         OPEN      OUTPUT,"\\nins1\e\data\ManageVol.srt|NINS1:502",read
.Create the Variant objects
.Booleans
          create  OTRUE,VarType=VT_BOOL,VarValue=1
          create  OFALSE,VarType=VT_BOOL,VarValue=0
          create  Zoom85,VarType=VT_I4,VarValue=1
          create    xlRowHC8,VarType=VT_R8,VarValue="75.0"
.Open Excel application
          create  ex
.not supported in 2013
.          setprop ex,*WindowState=xlMinimized
          setprop ex,*Visible="False"
.get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
.get exel version info
          
//.Reset Default of Worksheets found in a Workbook
        Getprop ex,*SheetsInNewWorkbook=SheetsDefault
        Setprop ex,*SheetsInNewWorkbook=C2
        
.Create Workbooks collection
          getprop ex,*Workbooks=books
.Create/Add a single Workbook
          books.add
          books.item giving book using 1
.Create Sheets collection
          getprop book,*workSheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add C1 new Worksheet each time a Workbook is created.
          sheets.item giving sheet using 1
          sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,190,60
       


         CALL      HEADER

READOUT  READ      OUTPUT,SEQ;STATUS,LSTNUM,MLSTNAME,QTY,NAMES,Enames,Qty1,Names1,Enames1
         GOTO      EOJPRT IF OVER

         ADD       C1 TO COUNT
         DISPLAY   *P12:14,"RECORDS IN ",COUNT

          move N5, str10
          call trim using str10
          pack str15, "B", str10
.
          setprop sheet.range(str15),*Value=lstnum,*HorizontalAlignment=xlLeft
          pack str15, "C", str10
          setprop sheet.range(str15),*Value=Mlstname,*HorizontalAlignment=xlLeft
          pack str15, "D", str10
          setprop sheet.range(str15),*Value=Qty,*NumberFormat="##,####0",*HorizontalAlignment=xlLeft
          pack str15, "E", str10
          setprop sheet.range(str15),*Value=Names,*NumberFormat="##,####0",*HorizontalAlignment=xlLeft
           if         (enames < Names)
          sub       Enames from Names
          else
          move        c0,names
          endif
          pack str15, "F", str10
          setprop sheet.range(str15),*Value=Names,*NumberFormat="##,####0",*HorizontalAlignment=xlLeft
          pack str15, "G", str10
          setprop sheet.range(str15),*Value=ENames,*NumberFormat="##,####0",*HorizontalAlignment=xlLeft


          pack str15, "h", str10
          setprop sheet.range(str15),*Value=Qty1,*NumberFormat="##,####0",*HorizontalAlignment=xlLeft
          pack str15, "I", str10
          setprop sheet.range(str15),*Value=Names1,*NumberFormat="##,####0",*HorizontalAlignment=xlLeft
           if         (enames1 < Names1)
          sub       Enames1 from Names1
          else
          move        c0,names1
          endif
          pack str15, "j", str10
          setprop sheet.range(str15),*Value=Names1,*NumberFormat="##,####0",*HorizontalAlignment=xlLeft
          pack str15, "k", str10
          setprop sheet.range(str15),*Value=ENames1,*NumberFormat="##,####0",*HorizontalAlignment=xlLeft

          pack str15, "M", str10
          pack        taskname from "=D",str10,"-H",str10
          setprop sheet.range(str15),*Formula=taskname,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlLeft
          pack str15, "N", str10
          pack        taskname from "=E",str10,"-I",str10
          setprop sheet.range(str15),*Formula=taskname,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlLeft
          pack str15, "O", str10
          pack        taskname from "=F",str10,"-J",str10
          setprop sheet.range(str15),*Formula=taskname,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlLeft
          pack str15, "P", str10
          pack        taskname from "=G",str10,"-K",str10
          setprop sheet.range(str15),*Formula=taskname,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlLeft


          add       c1,n5
         GOTO      READOUT
HEADER  
.Column Headers
.
          setprop sheet.range("B5"), *Value="Management Volume - Year to Date" ,*HorizontalAlignment=xlAlignCenter
                    sheet.range("B5:E5").Merge
          setprop sheet.range("B8"), *Value="List ##" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("C8"), *Value="List Name" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("D7"), *Value="Current" ,*HorizontalAlignment=xlAlignCenter
                    sheet.range("D7:G7").Merge
          
          setprop sheet.range("D8"), *Value="## Orders" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("E8"), *Value="## Total Names" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("f8"), *Value="## Rental Names" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("G8"), *Value="## Exchange Names" ,*HorizontalAlignment=xlAlignCenter

          setprop sheet.range("H7"), *Value="Last Year" ,*HorizontalAlignment=xlAlignCenter
                    sheet.range("H7:K7").Merge
          
          setprop sheet.range("H8"), *Value="## Orders" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("I8"), *Value="## Total Names" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("J8"), *Value="## Rental Names" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("K8"), *Value="## Exchange Names" ,*HorizontalAlignment=xlAlignCenter

          setprop sheet.range("M8"), *Value="## Orders" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("N8"), *Value="## Total Names" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("O8"), *Value="## Rental Names" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("P8"), *Value="## Exchange Names" ,*HorizontalAlignment=xlAlignCenter

          setprop sheet.range("L8"), *Value="Status" ,*HorizontalAlignment=xlAlignCenter
.
          setprop sheet.range("B5:E5").Font,*Name="Arial", *Size=10
          setprop sheet.range("B5:E5").Font,*Bold="True"
.
.          sheet.range("B8:H8").BorderAround using *LineStyle=1,*WC8=3
          setprop sheet.range("B8:L8").Font,*Name="Arial", *Size=10
          setprop sheet.range("B8:L8").Font,*Bold="True"
          move C10, N5 // for excel insertions, book        
.

         RETURN
EOJPRT

          CLOSE     OUTPUT

STOP     
                              move      n5,str9
                              call      Trim using str9
                              pack    range,"B8"
                              pack    range2,"B",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"C8"
                              pack    range2,"C",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"D8"
                              pack    range2,"D",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"E8"
                              pack    range2,"E",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"F8"
                              pack    range2,"F",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"G8"
                              pack    range2,"G",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"H8"
                              pack    range2,"H",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"I8"
                              pack    range2,"I",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"K8"
                              pack    range2,"K",str9
                              sheet.range(range,range2).Columns.Autofit
                              pack    range,"L8"
                              pack    range2,"P",str9
                              sheet.range(range,range2).Columns.Autofit



FileNameSelect
.                              setprop ex,*Visible="True"
                              clear   taskname
                              if        (#ver = c1)
                              Move  "C:\WORK\ManageVol.xlsx",taskname
                              else
                              move  "C:\WORK\ManageVol.XLS",taskname
                              endif
                              erase     taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                              trap    TrapObject if Object
                              book.saveas giving N9 using *Filename=taskname
                              trapclr Object
.
                              setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"


CleanUp
.Clean up after myself
                              destroy   OTRUE
.I was getting Excel.exe errors before I included following line.
.All automation objects need to be destroyed before you close down spreadsheet!
                              destroy sheet
                              destroy sheets
                              destroy book
                              destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
                              setprop ex,*DisplayAlerts=OFALSE
                              destroy   OFALSE
                              setprop ex,*SheetsInNewWorkbook=SheetsDefault
                              ex.quit
                              destroy ex
.Email new XLS to User
                              move    "Here is Management List history in Excel",MailSubjct
                              pack      MailBOdy,"File:  ListUsage"
                              Pack      Mailto from "DHerric@nincal.com"
                              Pack      Mailcc from "SuzieMcGuire@nincal.com"
                              Pack      MailFrom from "CReques@nincal.com"
                              if        (#ver = c1)
                              Pack      MailAttach from "c:\work\ManageVol.XLSX"
                              else
                              Pack      MailAttach from "c:\work\ManageVol.XLS"
                              endif
                              Move      c0,TrapCount                   .reset

CheckFile

                              trap      WaitForEnd giving error if IO
                              open      FileCheck,MailAttach,Exclusive          
                              Close     FIleCHeck

                              call      SendMail



          SHutdown

TrapObject
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
                pack    taskname,str50," already exists and is open!!"
                alert   caution,taskname,result
.                goto CampaignFileNameSelect
        endif
.Send them back to select another File name and try to Save again.
        goto FileNameSelect
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"NEOm0006 - "
                    Move      "CReques@nincal.com",MailFrom
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
.begin patch 2.3
          include   Nmdlio.inc
.end patch 2.3

         INCLUDE   NORDIO.inc
         INCLUDE   NDATIO.inc
         INCLUDE   COMLOGIC.inc

