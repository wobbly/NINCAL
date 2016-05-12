.  s. CARDCNT - NAMES IN THE NEWS CALIFORNIA VERSION 10/29/85
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         inc       hp.inc
         INCLUDE   NDATDD.inc
.begin patch 2.3
          include   Nmdldd.inc
.end patch 2.3

INPUT    FILE    
OUTPUT   FILE      UNCOMP
Release    INIT    "2.32"               DLH      .Excel 2013 *WindowState=xlMinimized
Reldate   Init      "2014 January 22"
.RElease   Init      "2.31"   DLH Remove Pia
.REldate   Init      "2013 September 9"
.RElease   Init      "2.3"   DLH add caller
.REldate   Init      "07 November 2012"
.RElease   Init      "2.2"   DLH dump to excel
.REldate   Init      "31 August 2012"
.RElease   Init      "2.12"   DLH Internal SOrt
.REldate   Init      "23 April 2008"
.RELEASE  INIT      "2.11"       JD  09apr01    updated unc on open/prep.
.RELEASE  INIT      "2.1"       ASH 30DEC98    NINORD Y2K, File expansion
.RELEASE  INIT      "R2.0"      DLH 12MARH92   ALL NEW INCLUDES ETC.
AKEY1    INIT      "01R"
AKEY2    INIT      "02R"
ALKEY    DIM       9           LISTKEY
AMKEY    DIM       7
NAMES    FORM      10
QTY      FORM      5
MASK     INIT      "Z,ZZZ,ZZZ,ZZ9"
ZERO     FORM      "0"
ONE      FORM      "1"
EIGHT    FORM      "8"
.Start Patch #2.1 - increase var to handle OQTY increase
.OQTY1    FORM      7
OQTY1    FORM      9
.End Patch #2.1 - increase var to handle OQTY increase
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
xlRowHeight         variant
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
.BLNK3    DIM       3           8-10
.MLSTNAME DIM       55         11-65
.QTY      FORM      5          66-70
.NAMES    FORM      10         71-80
...............................................................................
.PRINT VARIABLES
PAGE     FORM      3
LINES    FORM      2
SIXTY2   FORM      "62"
ANS      DIM       1
EIGHTY8  INIT      "88"
EIGHTY9  INIT      "89"
NINETY   INIT      "90"
NINETY1  INIT      "91"
NINETY2  INIT      "92"
NINETY3  INIT      "93"
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
Holdyear  Form      4        .CCYY
.taskname dim      120
.
         MOVE      "NEOY0007" TO PROGRAM
         MOVE      "CARD COUNT E-O-Y" TO STITLE
         MOVE      "Names In The News" TO COMPNME
         MOVE      C1 TO NDATPATH  .SET ACCESS TO ISAM
         MOVE      C2 TO NORDPATH
         move      c3 to ndatlock
         move      c3 to NORDLOCK
         CLOCK     DATE TO today
         UNPACK    today INTO mm,str1,dd,str1,yy
          pack      Str4 from CC,yy
          move      str4,holdyear
         CALL      CVTJUL
         MOVE      juldays TO TODAY1
         CALL      PAINT
         MOVE      "ABORT" TO PF5
         CALL      FUNCDISP
         KEYIN     *P12:12,"(P)rint or (C)alc 'default' ",*T60,ANS
         CMATCH    "P" TO ANS
         GOTO      OPEN IF EOS
         GOTO      print1 IF EQUAL
. 
. OPEN INPUT FILES
OPEN     TRAP      ABORT IF F5
. 
. 
. PREP OUTPUT FILE
. 
         IFNZ      PC
         PREP      OUTPUT,"CARDUSE/CAL:PRINT"
         XIF
         IFZ       PC
         PREPARE   OUTPUT,"\\nins1\e\DATA\CARDUSE.CAL|NINS1:502",exclusive
         Erase      "\\nins1\e\DATA\CARDEOY.lst"
         
         XIF
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
.         CMATCH    "C" TO ELSTCDE                  .exclusive ?
          if        (Elstcde <> "C" and ElstCDE <> "P")
          goto      A100
          endif
.         GOTO      A100 IF NOT EQUAL      .no get next rec.

                              cmatch    b1 to ndatoff
                              goto      a100 if not equal
         REP       " 0" IN LSTNUM
         PACK      NORDFLD2,AKEY2,LSTNUM
         CLEAR     NORDFLD1
         CLEAR     NORDFLD3
         CLEAR     NORDFLD4
         move      c3,nordlock
. 
         CALL      NORDAIM
         GOTO      CARDNG IF OVER
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

.         MOVE      OODTEM TO ORDMO
..         MATCH     NINETY3,OODTEY
.         MOVE      OODTEM TO MM
.         MOVE      OODTED TO DD
.         MOVE      OODTEY TO YY
.         CALL      CVTJUL
.         MOVE      juldays TO ORDDATE
.         MOVE      ORDDATE TO CHECK
.         move      today1 to check2
.         SUB       check FROM CHECK2
..         compare   "180" to check2           usage in last year
.         compare   "365" to check2           usage in last year
..         compare   "430" to check2           usage in last year
.         goto      b101 if LESS
          pack      str4 from oodtec,oodtey
          move      str4,n4
          if        (n4 = holdyear)
          goto      B101
          endif


.         GOTO      B101          .IT COUNTS 
.         GOTO      B101 IF EQUAL
.         MATCH     NINETY2,OODTEY
.         IF        EQUAL
.         COMPARE   MOMATCH TO ORDMO
.         IF        NOT LESS
.         GOTO      B101
.         ENDIF
.         ELSE
.         MATCH     NINETY2,OODTEY
.         IF        EQUAL
.         COMPARE   MOMATCH TO ORDMO
.         IF        NOT GREATER
.         GOTO      B101
.         ENDIF
.         ENDIF
.         ENDIF
         GOTO      B100A
CARDNG   DISPLAY   *P1:24,*EL,"LIST ",MLSTNAME:
                   *P1:23,*EL,"LIST NUMBER : ",LSTNUM," NOT USED "
         ADD       ONE TO NOTUSED
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
.         CMATCH    "p" TO OSTAT       Pending order ?
.         GOTO      b100a IF EQUAL     YES, skip.
.         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
.         GOTO      b100a IF EQUAL     YES, skip.
.         CMATCH    "l" TO OSTAT       LCR order ?
.         GOTO      b100a IF EQUAL     YES, skip.
.         CMATCH    "z" TO OSTAT       Cancelled LCR order ?
.         GOTO      b100a IF EQUAL     YES, skip.

         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         goto      b100a if equal
. 
         MATCH     LSTNUM,OLNUM
         GOTO      C100 IF NOT EQUAL
..
.         MOVE      OODTEM TO MM
.         MOVE      OODTED TO DD
.         MOVE      OODTEY TO YY
.         CALL      CVTJUL
.         MOVE      juldays TO ORDDATE
.         MOVE      ORDDATE TO CHECK
.         move      today1 to check2
.         SUB       check FROM CHECK2
..         compare   "180" to check2           usage in last year
.         compare   "365" to check2           usage in last year (twelve months)?
..         compare   "430" to check2           usage in last year?
.         goto      b101 if less
          pack      str4 from oodtec,oodtey
          move      str4,n4
          if        (n4 = holdyear)
          goto      B101
          endif

. 
.         MATCH     NINETY2,OODTEY
.         IF        EQUAL
.         COMPARE   MOMATCH TO ORDMO
.         IF        NOT LESS
.         GOTO      B101
.         ENDIF
.         ELSE
..         MATCH     NINETY2,OODTEY
.         IF        EQUAL
.         COMPARE   MOMATCH TO ORDMO
.         IF        NOT GREATER
.         GOTO      B101
.         ENDIF
.         ENDIF
.         ENDIF
         GOTO      B100A
B101     MOVE      OQTY TO OQTY1
         ADD       OQTY1,NAMES
         ADD       ONE,QTY
. 
         GOTO      B100
. 
. WRITE SUMMARY RECORD IKF USAGE <> 0
. 
C100
         COMPARE   ZERO,QTY
         GOTO      C101 IF EQUAL
         COMPARE   ZERO TO NAMES
         GOTO      C101 IF EQUAL
. 
         WRITE     OUTPUT,SEQ;STATUS,LSTNUM,"   ",MLSTNAME,QTY,NAMES,Revdate
         ADD       ONE TO USED
         DISPLAY   *P12:18,"NUMBER OF LISTS WITH USAGE ",USED
. 
C101     MOVE      ZERO,NAMES
         MOVE      ZERO,QTY
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
         IFNZ      PC
         FLUSH     OUTPUT
         XIF
         WEOF      OUTPUT,SEQ
         CLOSE     OUTPUT
         IFZ       PC
.          PREPARE   INPUT,"f:\APPS\BATCH\CARDEOY.BAT"
PRINT1         clear     taskname
.begin patch 2.12
.         append    "f:\NETUTILS\SORT32 \\nins1\e\data\CARDUSE.CAL,\\nins1\e\DATA\CARDEOY.DAT" to taskname
..         append    " /S(66,5,N,D,71,10,N,D) W(C:)" to taskname
.         append    " /S(86,5,N,D,91,10,N,D) W(C:)" to taskname
.         reset     taskname
          Pack      Taskname From "\\nins1\e\data\CARDUSE.CAL,\\nins1\e\DATA\CARDEOY.DAT,c:\work ":
                    "-d86-90,d91-100"
                    sort      taskname
.end patch 2.12
.         clear     taskname
.         WRITE     INPUT,SEQ;"SORT g:\DATA\CARDUSE.CAL,g:\DATA\CARDEOY.DAT":
.                  " /S(66,5,N,D,71,10,N,D) W(C:)"
.begin patch 2.12
.         append    "f:\NETUTILS\SORT32 \\nins1\e\DATA\CARDUSE.CAL,\\nins1\e\DATA\CARDEOY1.DAT" to taskname
.         append    " /S(91,10,N,D,86,5,N,D) W(C:)" to taskname
.;         append    " /S(71,10,N,D,66,5,N,D) W(C:)" to taskname
.         reset     taskname
.          Pack      Taskname From "\\nins1\e\data\CARDUSE.CAL,\\nins1\e\DATA\CARDEOY1.DAT,c:\work ":
.                    "-d91-100,d86-90"
.          Sort      Taskname
.         execute   taskname
.end patch 2.12
.         WRITE     INPUT,SEQ;"SORT g:\DATA\CARDUSE.CAL,g:\DATA\CARDEOY1.DAT":
.                  " /S(71,10,N,D,66,5,N,D) W(C:)"
.         WRITE     INPUT,SEQ;"f:\APPS\PCBUS\BINT/R"
.         WEOF      INPUT,SEQ
.         CLOSE     INPUT
.         ROLLOUT  "CARDEOY"
         GOTO      PRINT
         XIF
        STOP
. 
.............................
.PRINT - PASS ONE WILL PRINT FILE SORTED BY NUMBER OF ORDERS (CARDSBYORDER)
.        PASS TWO WILL PRINT FILE SORTED BY NUMBER OF NAMES (CARDSBYNAMES)
PRINT    
         TRAP      STOP IF F5
.         IFNZ      PC
.        OPEN      OUTPUT,"g:\data\CARDEOY",EXCLUSIVE
         OPEN      OUTPUT,"\\nins1\e\data\CARDEOY",read
.         SPLOPEN   "CARDCOUNTEOY/PRT:PRINT","Q",8
.         XIF
.         IFZ       PC
..        OPEN      OUTPUT,"CARDEOY",EXCLUSIVE
.         OPEN      OUTPUT,"CARDEOY"
.         SPLOPEN   "\\nins1\e\DATA\CARDEOY.lst","Q"
.         XIF
         MOVE      ONE TO PRNTBR
.Create the Variant objects
.Booleans
          create  OTRUE,VarType=VT_BOOL,VarValue=1
          create  OFALSE,VarType=VT_BOOL,VarValue=0
          create  Zoom85,VarType=VT_I4,VarValue=1
          create    xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
          create  ex
.begin patch 2.32
.          setprop ex,*WindowState=xlMinimized
.end patch 2.32
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
.add one new Worksheet each time a Workbook is created.
          sheets.item giving sheet using 1
          sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,190,60
       


         CALL      HEADER

READOUT  READ      OUTPUT,SEQ;STATUS,LSTNUM,BLNK3,MLSTNAME,QTY,NAMES,revdate
         GOTO      EOJPRT IF OVER
.begin patch 2.3
          Clear     mdlCALL
          packkey   NMDLFLD   from lstnum,b1
          call      Nmdlkey
.end patch 2.3

         ADD       ONE TO COUNT
         DISPLAY   *P12:14,"RECORDS IN ",COUNT
.         ADD       ONE TO LINES
.         COMPARE   SIXTY2 TO LINES
.         CALL      HEADER IF EQUAL
         MOVE      "Z,ZZZ,ZZZ,ZZ9",MASK
         EDIT      NAMES,MASK
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
          pack str15, "F", str10
          setprop sheet.range(str15),*Value=Status,*HorizontalAlignment=xlLeft
          unpack    REVDATE,CC,YY,MM,DD
          pack      str12,MM,SLASH,DD,SLASH,CC,YY
          pack str15, "G", str10
          setprop sheet.range(str15),*Value=str12,*HorizontalAlignment=xlLeft
.begin patch 2.3
          pack str15, "H", str10
          setprop sheet.range(str15),*Value=mdlcall,*HorizontalAlignment=xlLeft

.end patch 2.3
          add       c1,n5
.         PRINT     *1,LSTNUM,*10,MLSTNAME,*71,QTY,*84,MASK,*99,STATUS
         GOTO      READOUT
HEADER  
.Column Headers
.
          setprop sheet.range("B5"), *Value="DATACARD USAGE - Year to Date" ,*HorizontalAlignment=xlAlignCenter
                    sheet.range("B5:E5").Merge
          setprop sheet.range("B8"), *Value="List ##" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("C8"), *Value="List Name" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("D8"), *Value="## Orders" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("E8"), *Value="## Names" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("F8"), *Value="Status" ,*HorizontalAlignment=xlAlignCenter
          setprop sheet.range("G8"), *Value="Revised" ,*HorizontalAlignment=xlAlignCenter
.begin patch 2.3
          setprop sheet.range("H8"), *Value="Caller" ,*HorizontalAlignment=xlAlignCenter
.end patch 2.3
.
          setprop sheet.range("B5:E5").Font,*Name="Arial", *Size=10
          setprop sheet.range("B5:E5").Font,*Bold="True"
.
.begin patch 2.3
.          sheet.range("B8:G8").BorderAround using *LineStyle=1,*Weight=3
.          setprop sheet.range("B8:G8").Font,*Name="Arial", *Size=10
.          setprop sheet.range("B8:G8").Font,*Bold="True"
          sheet.range("B8:H8").BorderAround using *LineStyle=1,*Weight=3
          setprop sheet.range("B8:H8").Font,*Name="Arial", *Size=10
          setprop sheet.range("B8:H8").Font,*Bold="True"
.end patch 2.3
          move C10, N5 // for excel insertions, book        
.

.          MOVE      EIGHT TO LINES
.         ADD       ONE TO PAGE
.         BRANCH    PRNTBR OF HEADER1,HEADER2
.HEADER1  PRINT     hp17ptch,*F,*L,*34,"DATACARD USAGE BY NUMBER OF ORDERS";
.         PRINT     *98,"PAGE ",PAGE:
.                   *L,*L,*71,"NUMBER",*84,"NUMBER",*99,"LIST":
.                   *L,*1,"LIST",*10,"LIST NAME",*71,"OF ORDERS";
.         PRINT     *84,"OF NAMES",*99,"STATUS":
.                   *L,*1,"------";
.         PRINT     *10,"-------------------------------------------------";
.         PRINT     "-------------":
.                   *71,"---------",*84,"-----------",*99,"------",*L
         RETURN
.HEADER2  PRINT     hp17ptch,*F,*L,*34,"DATACARD USAGE BY NUMBER OF NAMES";
.         PRINT     *98,"PAGE ",PAGE:
.                   *L,*L,*71,"NUMBER",*84,"NUMBER",*99,"LIST":
.                   *L,*1,"LIST",*10,"LIST NAME",*71,"OF ORDERS";
.         PRINT     *84,"OF NAMES",*99,"STATUS":
.                   *L,*1,"------";
.         PRINT     *10,"-------------------------------------------------";
.         PRINT     "-------------":
.                   *71,"---------",*84,"-----------",*99,"------",*L
         RETURN
EOJPRT
         BRANCH    PRNTBR OF EOJPRT1,EOJPRT2
EOJPRT1  ADD       ONE TO PRNTBR
.         CLOSE     OUTPUT
.         OPEN      OUTPUT,"CARDEOY1",EXCLUSIVE
         MOVE      "0" TO LINES
         MOVE      "0" TO PAGE
         MOVE      "0" TO COUNT
.         IFNZ      PC
.         PRINT     *F,*FLUSH
.         XIF
.         IFZ       PC
.         PRINT     *F
.         XIF
.         CALL      HEADER
.         GOTO      READOUT
EOJPRT2  CLOSE     OUTPUT
.         IFNZ      PC
.         PRINT     *FLUSH,*F
.         XIF
.         IFZ       PC
.         PRINT     *F
.         XIFjul
.         SPLCLOSE

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



FileNameSelect
.                              setprop ex,*Visible="True"
                              clear   taskname
                              if        (#ver = c1)
                              Move  "C:\WORK\ListUsage.xlsx",taskname
                              else
                              move  "C:\WORK\ListUsage.XLS",taskname
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
                              Pack      MailCC from "DHerric@nincal.com"
.                              Pack      MailTo from "SusanAnstrand@nincal.com,PiaPayne@nincal.com,SuzieMcGuire@nincal.com"
                              Pack      MailTo from "SusanAnstrand@nincal.com,SuzieMcGuire@nincal.com,DeniseHubbard@nincal.com"
                              Pack      MailCC from "KatherineQuamina@nincal.com,JenniferMagee@nincal.com"
                              Pack      MailFrom from "CReques@nincal.com"
                              if        (#ver = c1)
                              Pack      MailAttach from "c:\work\ListUsage.XLSX"
                              else
                              Pack      MailAttach from "c:\work\ListUsage.XLS"
                              endif
                              Move      c0,TrapCount                   .reset

CheckFile

                              trap      WaitForEnd giving error if IO
                              open      FileCheck,MailAttach,Exclusive          
                              Close     FIleCHeck

                              call      SendMail



          STOP

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
                    Pack       MailSubjct,"NEOY0007 - "
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
