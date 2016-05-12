.
. PURPOSE - PRINT REPORT FOR DAILY ORDERS SHOWING:
.           1)TOTAL ORDERS PRINTED.
.           2)NUMBER OF NEW ORDERS PER TYPIST.
.           3)NUMBER OF REPRINTS PER TYPIST.
.           4)PERCENTAGES OF ABOVE COMPARED TO TOTALS.
. .............................................................................
. FILES.
. ......
.INPUT   FILE      ORDER DAILY PRINT FILE 
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NTYPDD.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NUSEDD.inc
.         INCLUDE   HP.inc
.
INPUT   FILE      FIXED=696,STATIC=12

RElease   Init      "2.5"        DLH add new counters, convert output to excel
Reldate   Init      "2015 April 29"
.RElease   Init            "2.43"        DLH fix calc
.Reldate   Init      "2014 March 31"
.RElease   Init            "2.42"        DLH Remove Suzie from email
.Reldate   Init      "2014 January 13"
.RElease   Init            "2.41"        DLH Do not allow running for data collection - only reporting, no more reprints, now updates
.Reldate   Init      "2013 October 7"
.RElease   Init            "2.40"        DH add pending order updates, eliminate approvals to output file
.Reldate   Init      "2013 October 3"
.RElease   Init            "2.34"        DH add "reported" date to header, allow dsprog to pass desired report month info
.Reldate   Init      "08 march 2012"
.RElease   Init            "2.33"        DH Various fixes - update to PDF and prtpage
.Reldate   Init      "24 June 2009"
.RElease       Init            "2.32"          JD updated input file nprint.tmp
.Reldate   Init      "18 June 2008"
.RElease       Init            "2.31"         DLH Internal index
.Reldate  Init      "23 April 2008"
.RElease       Init            "2.3"          Feb2006 DLH New file stucture new logic etc
. .............................
FILL3    DIM       3
DATE     DIM       8
.
.begin patch 2.33
prfile   pfile
Title1   form    9
Title2   form    9
Title3   form    9
Title4   form    9
PgCnt    form    9

FileCheck FIle
trapcount form      4
.begin patch 2.5
RangeStr  dim       12
RangeStr2 dim       12
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
books   automation
book    automation
sheets  automation
Range1    automation
sheet   automation
ex      automation      class="Excel.Application"
.Variant objects used to talk to outside applications
xlRowHeight   variant
VT_R8         EQU 5           .Double - 8 byte Real
.Booleans
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
Zoom80  variant
.Formatting vars needed
SheetsDefault integer 4,"0x00000000"
xlMinimized integer 4,"0xFFFFEFD4"
xlMaximized integer 4,"0xFFFFEFD7"
xlAlignCenter integer 4,"0xffffeff4"
xlUnderlineStyleSingle integer 4,"0x2"
MedThick integer 4,"0xFFFFEFD6"
AllMargin     variant
xlColWidth    variant
xlLandscape integer 4,"0x2"                     .2
xlAlignRight integer 4,"0xFFFFEFC8"
xlAlignLeft integer 4,"0xFFFFEFDD"
xlPaperLegal integer 4,"0x5"
SReturn       init            0x0a                                                        .soft return/line feed
LOText        dim             100
.
range         dim             20
range2        dim             20
ExRange1      Dim             7
ExRange2      Dim             7
ExRange3      Dim            16
.end patch 2.5


.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
font7   font
font8   font
font9   font
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
        create  font6,"Arial",size=14
        create  font7,"Times New Roman",size=9
        create  font8,"Times New Roman",size=10
        create  font9,"Times New Roman",size=10,italic
.end patch 2.33
. ..............
. OTHER VARIABLES.
NOrdTypRec     REcord         (40)  
TypeREc       Dim             3                 ;typist inits
Nqtyrec       Form            9                 ;order qty
Countrec      Form            4                 ;number of new orders            
RepRec        Form            4                 ;number reprint orders
RePQtyREc     Form            9                 ;reprint qty            
PordRec       form            4                 ;pending order count
PordURec       form            4                 ;pending order Updatecount
LCRRec        Form            4                 ;new lcr count
              RecordEnd

NQTY          FORM      9            NEW ORDER QTY
NCOUNT        FORM      4            NUMBER OF NEW ORDERS
REPCNT        FORM      4            NUMBER OF REPRINT ORDERS
.Pendings will never be in this file (see Nord0008) need to be counted as written to NINprint.dat
Pcount        form            4            pending orders
PUcount        form            4            pending orders Updates
Lcount        form            4              lcr's
SUBRTY        FORM      9                    order reprint qty 
COUNT         FORM      5
BRANCH        FORM      "00"
.R             INIT      "R"
QTY           FORM      9
.END           FORM      2
HUNDRED       FORM      "100"
.begin patch 2.43
.TOTREP        FORM      3
TOTREP        FORM      5
.end patch 2.43
Calc53        Form            5.3
REPCALC       FORM      3.2        *percentage of reprint orders
TOTCALC       FORM      3.2        *percentage of orders
LTCALC        FORM      3.2        *PERCENTAGE OF NEW LCR'S.
LRCALC        FORM      3.2        *PERCENTAGE OF REPRINT LCR'S.
ATCALC        FORM      3.2        *PERCENTAGE OF NEW APPROVALS.
pucalc        FORM      3.2        *PERCENTAGE OF Updated Pending.
ADCALC        FORM      3.2        *PERCENTAGE OF NEW adjustments.
PCalc         Form      3.2        *percentage of Pending orders
INVCALC       FORM      3.2
INVRCALC      FORM      3.2
LstCalc       Form            3.2
LstUCalc      Form            3.2
.begin patch 2.5
DBCalc       Form            3.2
DBUCalc      Form            3.2
.end patch 2.5
LINES         FORM      2
ENDSW         DIM       1
ANS           DIM       1
PRTDATE       DIM       8          *DATE FORMAT MM/DD/YY
IOERROR       FORM      1          *INDEX FOR IO ERROR BRANCH.
PAGE          FORM      "  1"
CANC          FORM      4
LSTOTAL       FORM      5          *LCR TOTALS
LRTOTAL       FORM      5          *LCR TOTALS
OrdSum        Form            5                 ;holding place # of new orders
OrdrSum       Form            5                 ;holding place # of reprint orders
qtySum        Form            5                 ;holding place # of new order names
LcrSum        Form            5                 ;holding place # of new lcrs
LcrRSum       Form            5                 ;holding place # of reprint Lcrs
POrdSum       Form            5                 ;holding place # of new Pending orders
POrdUSum       Form            5                 ;holding place # of updated Pending orders
InvSUm        form            5
InvRsum       Form            5
CorSum        form            5
CancSum       form            5
.appsum        form            5
PndUsum        form            5
adjsum        form            5
lstsum        form            5
lstUsum       form            5
.begin patch 2.5
DBsum        form            5
DBUsum       form            5
.end patch 2.5
Datecheck     dim             8
M01       INIT      "January"
M02       INIT      "February"
M03       INIT      "March"
M04       INIT      "April"
M05       INIT      "May"
M06       INIT      "June"
M07       INIT      "July"
M08       INIT      "August"
M09       INIT      "September"
M010      INIT      "October"
M011      INIT      "November"
M012      INIT      "December"
bigdate   dim       25

. PROGRAM MAIN.
         MOVE      "NIN" TO COMPNME
         MOVE      "ORDER TYPIST REPORTING" TO STITLE
         MOVE      "EXIT" TO PF5
         CALL       PAINT
         CALL       FUNCDISP
         TRAP      DOWN IF F5
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         XIF
         IFZ       PC 
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         XIF
         REP       ZFILL IN YY
         REP       ZFILL IN MM
         REP       ZFILL IN DD
         PACK      PRTDATE FROM MM,SLASH,DD,SLASH,YY

         clock     timestamp,str6
         unpack    str6,str2,yy,mm
              Move            str2 to cc         
         pack      TypDate from str6
         
         rep       Zfill in typDate
.patch 2.31
         pack            Datecheck from cc,yy,mm,dd
.patch 2.31        
.         
.testing

.          move      "20140228",datecheck


.begin patch xxx
          Rep       LowUP,Program
          if         (PROGRAM = "NORD0014")  .chained from dsinit
                    if        (func = "3")           .print
                    move      "P",ans
                    unpack today into mm,str1,dd,str1,yy
                    PACK      PRTDATE FROM MM,SLASH,DD,SLASH,YY
                    pack      TypDate from cc,yy,mm
                    move      MM,N2
                    load      STR9 USING N2 FROM M01,m02,m03,m04,m05,m06:
                              m07,m08,m09,m010,m011,m012
                    move      DD,STR2
                    reset     STR2,1
                    setlptr   STR2,1
                    rep       "0 ",STR2
                    setlptr   STR2
                    clear     bigDATE
                    append    STR9,bigDATE
                    append    B1,bigDATE
                    append    STR2,bigDATE
                    append    B1,bigDATE
                    append    ",",bigDATE
                    append    cc,bigDATE
                    append    YY,bigDATE
                    reset     bigDATE
                    goto      autoprint
                    endif
.begin patch 2.41
.          move      "D" to ans
.          goto       Daily
          endif
.end patch 2.41

.end patch xxx

.begin patch 2.41
CHOOSE   move      "P" to ans
         KEYIN     *P25:14,"(*)Exit, (P)rint report, (K)ill file":
                   " ",*T60,*RV,ANS;
.CHOOSE   move      "D" to ans
.         KEYIN     *P25:14,"(D)aily processing, (P)rint report, (K)ill file":
.                   " ",*T60,*RV,ANS;
.         CMATCH    "D" TO ANS
.         GOTO      DAILY IF EQUAL
         CMATCH    "*" TO ANS
          if        equal
         shutdown   "CLS"
         endif
         CMATCH    "R" TO ANS
         GOTO      Repair IF EQUAL
         CMATCH    "P" TO ANS
.end patch 2.41
         IF EQUAL
              KEYIN           *P10:12,"Report DATE ",*p22:12,*el,*+,*dv,mm,"/",*dv,dd,"/",*dv,cc,*dv,yy:
                              *p22:12,*t30,*ZF,*JR,*rv,MM,*DV,SLASH,*rv,DD:
                              *DV,SLASH,*rv,cc,*rv,YY
                    move      MM,N2
                    load      STR9 USING N2 FROM M01,m02,m03,m04,m05,m06:
                              m07,m08,m09,m010,m011,m012
                    move      DD,STR2
                    reset     STR2,1
                    setlptr   STR2,1
                    rep       "0 ",STR2
                    setlptr   STR2
                    clear     bigDATE
                    append    STR9,bigDATE
                    append    B1,bigDATE
                    append    STR2,bigDATE
                    append    B1,bigDATE
                    append    ",",bigDATE
                    append    cc,bigDATE
                    append    YY,bigDATE
                    reset     bigDATE


Autoprint
              PACK            PRTDATE FROM MM,SLASH,DD,SLASH,YY
              PACK            DATE FROM MM,SLASH,DD,SLASH,YY
              pack            TypDate from cc,yy,mm
              pack            Datecheck from cc,yy,mm,dd
.Begin patch 2.5
CreateSheet
.Create the Variant objects
.Booleans
               create  OTRUE,VarType=VT_BOOL,VarValue=1
               create  OFALSE,VarType=VT_BOOL,VarValue=0
               create  Zoom80,VarType=VT_I4,VarValue=80
."1" increment in Excel interface equals "1.3888" in OLE logic
               create         AllMargin,VarType=VT_R8,VarValue="18"                       .Roughly equals .25 inches:  18 * 1.388 = 25
               create         xlColWidth,VarType=VT_R8,VarValue="0.0"                     .Default
.
               create         xlRowHeight,VarType=VT_R8,VarValue="75.0"
.Open Excel application
               create  ex
.turned off for debug
.        setprop ex,*Visible="False",*IgnoreRemoteRequests="True",*Interactive="False"

              getprop ex,*SheetsInNewWorkbook=SheetsDefault
                      setprop ex,*SheetsInNewWorkbook=C1
.Create Workbooks collection
               getprop ex,*Workbooks=books
.Create/Add a single Workbook
               books.add
               books.item giving book using 1
.Create Worksheets collection
               getprop book,*Sheets=sheets
               sheets.item giving sheet using 1
               setprop sheet.PageSetup,*Orientation=xlLandscape
               setprop sheet.PageSetup,*Zoom=Zoom80
               setprop sheet.PageSetup,*TopMargin=AllMargin
               setprop sheet.PageSetup,*BottomMargin=AllMargin
               setprop sheet.PageSetup,*RightMargin=AllMargin
               setprop sheet.PageSetup,*LeftMargin=AllMargin
.               //Using xlColWidth for dual purposes!!
               setprop sheet.PageSetup,*HeaderMargin=xlColWidth
               setprop sheet.PageSetup,*FooterMargin=xlColWidth
               pack    str11,"1:8"
               setprop sheet.PageSetup,*PrintTitleRows=str11
               setprop sheet.PageSetup,*PaperSize=xlPaperLegal
               setprop        sheet.range("A1:A1").Rows,*RowHeight=xlRowHeight
          Pack      Taskname from "=Hyperlink(#"http://www.namesinthenews.com#",#"Names in the News#")"
          setprop         sheet.Range("A1:A1"),*Formula=taskname
          sheet.range("A1:C1").Merge
.add bigdate, etc
          setprop        sheet.Range("E1"),*Value="Month to date data entry analysis",*HorizontalAlignment=xlAlignCenter
          setprop        sheet.Range("E1").Font,*BOLD="TRUE"
          sheet.range("E1:J1").Merge
          setprop        sheet.Range("E3"),*Value=Bigdate,*HorizontalAlignment=xlAlignCenter
          setprop        sheet.Range("E3").Font,*BOLD="TRUE"
          sheet.range("E3:J3").Merge
          setprop        sheet.Range("A5"),*Value="Name",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("C5"),*Value="Orders",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("D5"),*Value="%",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("E5"),*Value="Pending",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("F5"),*Value="%",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("G5"),*Value="Lcrs",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("H5"),*Value="%",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("I5"),*Value="Invoices",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("J5"),*Value="%",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("k5"),*Value="Adjustments",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("L5"),*Value="%",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("m5"),*Value="lists",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("n5"),*Value="%",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("O5"),*Value="Database",*HorizontalAlignment=xlAlignLeft
          setprop        sheet.Range("P5"),*Value="%",*HorizontalAlignment=xlAlignLeft

          setprop sheet.Range("a5:p5").Font,*Bold="True",*Size=12

          move      c7,Howmany
.end patch 2.5
              
                   
.begin patch 2.33
.         PACK      STR35,NTWKPATH1,"ntypst.lst"
.test test test
.         splopen   STR35
.          Splopen   "","Q"
.test test test
. Logic Addition for PDF Quality Control
.        call       GetPDFPath
.                    pack      str45 from PDFPATH,"\res\pdf995.ini"
. 
.                   call      "GU$INI;WRITE_TO_INI" USING str45:
.                             "Parameters":
.                             "ProcessPDF":
.                             "\\nins1\e\apps\winbatch\Del995flag.exe":
.          result
.          if (result = C0)
..Prepare Flag file
.          pack      str45 from pdfpath,"\flag.dat"
.          prep      tempfile,str45
.          write     tempfile,SEQ;"flag set"
.          close     tempfile
.          endif
. Logic Addition for PDF Quality Control         
.begin patch 2.5
.          PRTOPEN prfile,"PDF:","c:\work\pdf\TypistRep.pdf"
.          prtpage prfile;*UNITS=*HIENGLISH:
.          *ORIENT=*LANDSCAPE;
.end patch 2.5

.         print     hpland,hp14ptch,hplin8,hptop
.end patch 2.33
.begin patch 2.5
           pack       Ntypfld,typdate
           call       ntyptst

         goto      totcalc
         endif
         CMATCH    "K" TO ANS
         GOTO      CHOOSE IF NOT EQUAL
         KEYIN     *P1:24,"THIS WILL DELETE ALL RECORDS IN THE FILE OK??",ANS;
         CMATCH    "Y" TO ANS
         GOTO      DOWN IF NOT EQUAL
.
              PACK            STR55,NTWKPATH1,"text\typout.dat"
              Erase           str55
.begin patch 2.31
.         PACK      TASKNAME,"\\nts0\c\apps\plb\code\sunindex.exe ",NTWKPATH1,"text\typout.dat ",NTWKPATH1,"index\typout.isi,L83 -1-9,e,n"
.begin patch 2.5
.          PACK      TASKNAME,NTWKPATH1,"text\typout.dat ",NTWKPATH1,"index\typout.isi,L83 -1-9,e,n"
          PACK      TASKNAME,NTWKPATH1,"text\typout.dat ",NTWKPATH1,"index\typout.isi,L110 -1-9,e,n"
.end patch 2.5
.         execute   TASKNAME
          INdex   TASKNAME
.end patch 2.31
DOWN
              Shutdown        "CLS"
. ...................
DAILY
.         DISPLAY    *P25:12,"NUMBER OF ORDERS READ ";
.
         TRAP      IO GIVING ERROR IF IO
         MOVE      C1 TO IOERROR

               if             (func <> "1" & Func <> "2")
               move           c1,func
               endif
.begin patch 2.32
          if              (Func = "1")           .hourly run        
          OPEN      INPUT,"NPRINT.tmp",SHARE
         DISPLAY    *P25:12,"NUMBER OF ORDERS READ ";
         Elseif              (Func = "2")           .LCR Nightly run        
          OPEN      INPUT,"LcrPRINT.lcr",SHARE
         DISPLAY    *P25:12,"NUMBER OF LCR's READ ";
          endif
.        OPEN      INPUT,"NPRINT.srt",SHARE
.end patch 2.32

         SUB       IOERROR FROM IOERROR
         TRAPCLR    IO
READ     READ      INPUT,SEQ;ORDVARS
         GOTO      OUTPUT IF OVER
.patch 5    New values for OSTAT  p Lower case = Pending order (awaiting LO/manager Approval)
.                                 x Lower case = Cancellation of above (never approved)
.                                 l Lower case = LCR
.                                 z Lower case = Cancellation of LCR
.add lcr stuff later
               if              (Func = "1")           .hourly run
.patch 2.31
.                              pack      str4 from "xlz"
                              pack      str4 from "xlzp"
.patch 2.31
                              Scan             Ostat in str4
                              goto            read if equal
                              RESET     CANCODES
                              SCAN      OSTAT IN CANCODES
                              GOTO      READ IF EQUAL
.                                      0-Live order
.                                      B-Billed order
.                                      Q-Cancelled/Billed order
.                                      X-Cancelled order
.                                      e-Live Order with Estimated Invoice uses "X" if cancelled         

               ElseIf         (Func = "2")               
                              pack      str4 from "px"            .nightly run include LCr's
                              Scan             Ostat in str4
                              goto            read if equal
.                              
                                             If             (Ostat = "p" or Ostat = "z" or Ostat = "l" or Ostat = "x")    pending or canc pending or lcr or canc lcr
                                             pack           str8 from OODTEC,OODTEy,OODTEm,OODTEd
                                             match          str8 to datecheck
                                             goto           read if not equal
                                             endif
              Else                                                   ;darn func check failed
.                              pack      str4 from "xlz"
.                              Scan             Ostat in str4
.                              goto            read if equal
              endif              
         MATCH     "      " TO OLRN
         GOTO      READ IF EQUAL     *CHECK FOR NULL 1ST RECORD.
         GOTO      READ IF EOS
         ADD       C1 TO COUNT
         DISPLAY   *P48:12,*EL,COUNT;
         GOTO      BREAK
.
BREAK
              MOVE      ODOWJ TO NTypTYPe
              GOTO      ADD
.
PASSONE
.
ADD
              MOVE      OQTY TO QTY
SUBTOT
              FOR           Branch,"1","40"
                                            
                              If             (NTypTYPe = NordTypRec(branch).TypeRec)                            
                                             Move           NordTypREc(Branch).Nqtyrec,nqty                  ;# new Names
                                             Move           NordTypREc(Branch).Repqtyrec,subrty
                                             Move           NordTypREc(Branch).Countrec,Ncount
                                             Move           NordTypREc(Branch).PordRec,Pcount
                                             Move           NordTypREc(Branch).PordURec,PUcount
                                             Move           NordTypREc(Branch).LcrRec,LCount
                                             Move           NordTypREc(Branch).Reprec,RepCnt
                                             if             (Ostat = "B" & (Ntyptype = "ARB" or Ntyptype = "AMB"))
                                             add            qty to nqty                                  ;# new Names
                                             add            c1 to Ncount
                                             elseif         (ostat = "R")          .reprint
                                             add            qty to subrty
                                             add            c1 to repcnt
                                             Elseif         (Ostat = "p")           .pending
                                             Add            C1 to PCount
                                             Elseif         (Ostat = "x")           .pending
                                             Add            C1 to PCount
                                             Elseif         (Ostat = "l")           .LCR
                                             add            c1 to Lcount
                                             Elseif         (Ostat = "z")           .canc LCR
                                             add            c1 to Lcount
                                             Else                                   .must be regular live order
                                             add            qty to nqty
                                             add            c1 to Ncount
                                             endif
                                             Move           Nqty,NordTypREc(Branch).Nqtyrec              ;# new Names
                                             Move           subrty,NordTypREc(Branch).Repqtyrec
                                             Move           NCount,NordTypREc(Branch).Countrec
                                             Move           PCount,NordTypREc(Branch).PordRec
                                             Move           PuCount,NordTypREc(Branch).PordURec
                                             Move           LCount,NordTypREc(Branch).LcrRec
                                             Move           RepCnt,NordTypREc(Branch).Reprec
                             Break                
                             endif
                             if              (Nordtyprec(Branch).Typerec = "")
                                             if             (Ostat = "B" & (Ntyptype = "ARB" or Ntyptype = "AMB"))
                                             add            qty to nqty                ;# new Names
                                             add            c1 to Ncount
                                             ElseIf        (ostat = "R")
                                             add            qty to subrty
                                             add            c1 to repcnt
                                             Elseif         (Ostat = "p")           .pending
                                             Add            C1 to PCount
                                             Elseif         (Ostat = "x")           .pending
                                             Add            C1 to PCount
                                             Elseif         (Ostat = "l")           .LCR
                                             add            c1 to LCount
                                             Elseif         (Ostat = "x")           .canc LCR
                                             add            c1 to LCount
                                             Else                                   .must be regular live order
                                             add            qty to nqty
                                             add            c1 to Ncount
                                             endif
                                             Move           Nqty,NordTypREc(Branch).Nqtyrec              ;# new Names
                                             Move           subrty,NordTypREc(Branch).Repqtyrec
                                             Move           NCount,NordTypREc(Branch).Countrec
                                             Move           RepCnt,NordTypREc(Branch).Reprec
                                             Move           PCount,NordTypREc(Branch).PordRec
                                             Move           PuCount,NordTypREc(Branch).PorduRec
                                             Move           LCount,NordTypREc(Branch).LcrRec
                                             Move           NTypTYPe,NordTypREc(Branch).Typerec
                             break                
                             endif                
           repeat

.
         MOVE      C0 TO NCOUNT
         MOVE      C0 TO LCOUNT
         MOVE      C0 TO PCOUNT
         MOVE      C0 TO PUCOUNT
         MOVE      C0 TO SUBRTY
         MOVE      C0 TO NQTY
         MOVE      C0 TO REPCNT
              goto            read
         RETURN
.
OUTPUT
         DISPLAY   *P1:24,"PREPARING FILE OUTPUT",*B;
         SUB       TOTREP FROM COUNT   *SUBTRACT REPRINTS FROM TOTAL.
OUTPUT1
.      UNLOAD TABLES.
              FOR           Branch,"1","40"
                                            
                                             Move           NordTypRec(branch).TypeRec,NTypTYPe
                                             Move           NordTypREc(Branch).Nqtyrec,nqty     ;# new Names
                                             Move           NordTypREc(Branch).Repqtyrec,subrty    ;order reprint qty
                                             Move           NordTypREc(Branch).Countrec,Ncount     ;new orders 
                                             Move           NordTypREc(Branch).Reprec,RepCnt       ;order reprint count
                                             Move           NordTypREc(Branch).PordRec,Pcount      ;pending
                                             Move           NordTypREc(Branch).PorduRec,Pucount      ;pending
                                             Move           NordTypREc(Branch).LcrRec,LCount       ;lcrs
                             if              (Nordtyprec(Branch).Typerec = "")
                             break  
                             goto            outlast
                             else
         MATCH     "  " TO NTypTYPe
         CALL      NOTYP IF EQUAL
         CALL      NOTYP IF EOS
              Move            c0 to n3
              move            Ntyptype to n3
              if              (n3 = "99")
              goto            outofhere
              endif
.         DISPLAY   *P1:24,*EL,"READ  KEY= ",NTYPFLD;
              MOVE      NTypType TO ODOWJ                            .save it
              MOVE       C1 TO NTYPPATH                         ;detail record
              Move            Str6 to TypDate
              Packkey         Ntypfld from TypDate,NTypType         
.add to totals              
              ADD             NQTY TO QtySum        ;totals         ;# new Names
              ADD             NCOUNT TO OrdSum        ;totals
              ADD             REPCNT TO OrdRSum        ;totals
              add             Pcount to Pordsum        ;totals
              add             PUcount to PordUsum        ;totals
              add             LCount to LcrSum        ;totals
.do we have a detail record for this typist this month?
              CALL            NTYPtst
              GOTO            WRITE IF OVER                      .Nope go make one
              CALL            NTYPKEY                    .yes we do read it and update
               ADD            NQTY TO SUBQTY                      ;Detail         ;# new Names
               ADD            NCOUNT TO SUBCOUNT                  ;Detail     
               ADD            REPCNT TO REPCOUNT                  ;detail
               add            Pcount to Pndcount            ;detail
               add            PUcount to PndUcount            ;detail
               add            LCount to LSUBCNT             ;detail
.              DISPLAY   *P1:24,*EL,"UPDATE";
              CALL            NTYPUPD
              GOTO            OUTPUTX
NOTYP    
              Move            Str6 to TypDate
              Packkey         Ntypfld from TypDate,"???"
              move            "???" to NTypType
              RETURN
WRITE
              MOVE            ODOWJ TO NTypType
              Move            str6 to Typdate
              Packkey         Ntypfld from Typdate,NTypType

               ADD            NQTY TO SUBQTY                      ;Detail         ;# new Names
               ADD            NCOUNT TO SUBCOUNT                  ;Detail     
               ADD            REPCNT TO REPCOUNT                  ;detail
               add            Pcount to Pndcount            ;detail
               add            PUcount to PndUcount            ;detail
               add            LCount to LSUBCNT             ;detail
              DISPLAY         *P1:24,*EL,"WRITE  KEY=",NTYPFLD;
              CALL            NTYPWRT
.
. OUTPUTX - OUTPUT SECTION EXIT.
OUTPUTX
                              endif                
OutOfHere
              repeat
OUTLAST
              DISPLAY   *P1:24,*EL,"FINAL READ";
              Move            str6 to Typdate
              Packkey         Ntypfld from Typdate,"99 "
.              move            NTypfld to Idnum
              Move            "99 " to NTypType
.              MOVE      C2 TO NTYPPATH
              CALL      NTYPtst
              GOTO      NONINE IF OVER
              CALL      NTYPKEY
.              add             OrdSum to STotal
.              add             OrdRsum to RTotal
.              Add             QtySum to QtyTotal
.              add             LcrSum to LCSTot
.              add             LcrRsum to LCRTot
.              add             Pordsum to PndTot
              add             OrdSum to SUbCount
              add             OrdRsum to RepCount
              Add             QtySum to SubQty
              add             LcrSum to LSUBCNT
              add             LcrRsum to LREPCNT
              add             Pordsum to PndCOunt
              add             PordUsum to PndUCOunt
              DISPLAY   *P1:24,*EL,"FINAL WRITE";
               CALL       NTYPUPD
         GOTO      STOP
NONINE   
              Move            str6 to Typdate
              Packkey         Ntypfld from Typdate,"99 "
.              move            nTypfld to Idnum
              Move            "99 " to NTypType
.              MOve            OrdSum to STotal
.              MOve            OrdRsum to RTotal
.              MOve            QtySum to QtyTotal
.              MOve            LcrSum to LCSTot
.              MOve            LcrRsum to LCRTot
.              Move            Pordsum to PndTot
              Move            OrdSum to SUbCount
              Move            OrdRsum to RepCount
              Move            QtySum to SubQty
              Move            LcrSum to LSUBCNT
              Move            LcrRsum to LREPCNT
              Move            Pordsum to PndCOunt
              add             PordUsum to PndUCOunt
              CALL      NTYPWRT
              GOTO      STOP
.
..........................................................................................
. TOTCALC - READ AND PRINT FROM FILE TYPOUT.
TOTCALC
.Set up columns and title positions
        move    "100",column                      
        move    "2600",column1                    .Order
        move    "3100",column2                    .Order
        move    "3950",column3                    .pending
        move    "4450",column4                    .pending
        move    "5100",column5                    .Lcr
        move    "5600",column6                    .Lcr
        move    "6600",column7                    .invoice
        move    "7100",column8                    .invoice
        move    "8100",column9                    .adjustment
        move    "8600",column10                   .adjustment
        move    "9600",column11                  .list
        move    "10100",column12                  .list
.       7860 row position of pg #
        move    "5260",Title1
.        move    "4000",Title1
        move    "9000",Title2
        move    "9500",Title3
        move    "5260",Title4

              CALL      HEADER
.              Packkey         Ntypfld from Typdate,"99 "

.              MOVE      C2 TO NTYPPATH
          Move      DateCheck,str6
.
READKS   MOVE      C1 TO NTYPPATH
         MOVE      C0 TO COUNT
         MOVE      C0 TO TOTREP
         CALL      NTYPKS
         GOTO      EOJ IF OVER
                    If             (str6 <> typDate)
                    goto           readks                   .different month
                    endif
         call       debug
              call            Trim using NTypType
              move            c0 to n3
              Move            NTypType to n3     
              if              (n3 = "99")
                    Move           Typdate to str6
              Move            SUBCOUNT to ordsum
              Move            REPCOUNT to OrdRsum
              Move            SUBQTY   to QtySum
              Move            LSUBCNT  to LCrsum
              Move            LREPCNT  to LcrRSum
              Move            INVCOUNT to Invsum     
              Move            INVRCNT  to InvRsum    
              Move            CORCOUNT To CorSum     
              Move            CANCOUNT to Cancsum    
.              Move            APPCOUNT to AppSum     
              Move            PndUCOUNT to PndUsum
              Move            ADJCount to AdjSum     
              Move            PndCOunt to Pordsum    
              Move            PnduCOunt to Pordusum    
              Move            lstCount to Lstsum     
              move            LStUCOunt to LstUSum
.begin patch 2.5              
              Move            DBCount to DBsum     
              move            DBUCOunt to DBUSum
.end patch 2.5              
              goto            readks
              endif
.
CALCPER
.      CALCULATE PERCENTAGE OF NEW ORDERS.
         MOVE      C0 TO TOTCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO SUBCOUNT
         GOTO      CALCREP IF EQUAL
.         MOVE      STotal TO COUNT
              Move            Ordsum to count
         MOVE      SUBCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            calc53 to totcalc
CALCREP
.      CALCULATE PERCENTAGE OF REPRINTS.
           MOVE      C0 TO REPCALC
           MOve      c0 to Calc53         
           COMPARE   C0 TO REPCOUNT
           GOTO      CALCLCR IF EQUAL
          Move      OrdRsum to totrep
.         MOVE      RTotal TO TOTREP
         MOVE      REPCOUNT TO CALC53
         DIVIDE    TOTREP INTO CALC53
         MULT      HUNDRED BY CALC53
         Move            calc53 to repcalc
.
.CALCLCR - CALC LIST CLEARANCE REQ'S.
CALCLCR
         MOVE      C0 TO LTCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO LSUBCNT
         GOTO      CALCLCR1 IF EQUAL
.         MOVE      LCSTot TO COUNT
         MOVE      LCrsum TO COUNT
         MOVE      LSUBCNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              MOve            calc53 to ltcalc
.
CALCLCR1
         MOVE      C0 TO LRCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO LREPCNT
         GOTO      CALCINV IF EQUAL
.         MOVE      LCRTot TO COUNT
         MOVE      LCRrsum TO COUNT
         MOVE      LREPCNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            Calc53 to LRcalc         
.
.CALCINV - CALC INCVOICES.
CALCINV
         MOVE      C0 TO INVCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO INVCOUNT
         GOTO      CALCINV1 IF EQUAL
.         MOVE      INVTOT TO COUNT
         MOVE      INVsum TO COUNT
         MOVE      INVCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            Calc53 to invcalc
.
CALCINV1
         MOVE      C0 TO INVRCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO INVRCNT
.         GOTO      TYPREAD IF EQUAL
.         GOTO      CALCAPP IF EQUAL
         GOTO      CALCPndU IF EQUAL
.         MOVE      INVRTOT TO COUNT
         MOVE      INVRsum TO COUNT
         MOVE      INVRCNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            Calc53 to invrcalc
.
calcPndU
         MOVE      C0 TO PUCALC
         MOve            c0 to Calc53         
         COMPARE   C0 TO PndUCOUNT
         GOTO      calcadj IF EQUAL
         MOVE      pndUsum TO COUNT
         MOVE      PndUcount TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
         Move      calc53 to PUcalc
.CALCAPP  MOVE      C0 TO ATCALC
.              MOve            c0 to Calc53         
.         COMPARE   C0 TO APPCOUNT
.         GOTO      calcadj IF EQUAL
.         MOVE      APPsum TO COUNT
.         MOVE      APPCOUNT TO CALC53
.         DIVIDE    COUNT INTO CALC53
.         MULT      HUNDRED BY CALC53
.              Move            calc53 to Atcalc
.
CALCAdj  MOVE      C0 TO AdCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO AdjCOUNT
         GOTO      calcpnd IF EQUAL
         MOVE      Adjsum TO COUNT
         MOVE      AdjCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
              Move            Calc53 to adcalc
.
CALCPnd  MOVE      C0 TO pCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO PndCOUNT
         GOTO      calclst IF EQUAL
         MOVE      Pordsum TO COUNT
         MOVE      PndCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
         Move      calc53 to pcalc

CALClst  MOVE      C0 TO lstCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO lstCOUNT
         GOTO      CalcUlst IF EQUAL
         MOVE      lstsum TO COUNT
         MOVE      lstCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
         Move      calc53 to lstcalc
.         
CALCUlst  MOVE      C0 TO lstuCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO lstuCOUNT
         GOTO      TYPREAD IF EQUAL
         MOVE      lstusum TO COUNT
         MOVE      lstuCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
         Move      calc53 to lstucalc
.begin patch 2.5
CALCDB  MOVE      C0 TO DBCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO DBCOUNT
         GOTO      CalcUDB IF EQUAL
         MOVE      DBsum TO COUNT
         MOVE      DBCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
         Move      calc53 to DBcalc
.         
CALCUDB  MOVE      C0 TO DBuCALC
              MOve            c0 to Calc53         
         COMPARE   C0 TO DBuCOUNT
         GOTO      TYPREAD IF EQUAL
         MOVE      DBusum TO COUNT
         MOVE      DBuCOUNT TO CALC53
         DIVIDE    COUNT INTO CALC53
         MULT      HUNDRED BY CALC53
         Move      calc53 to DBucalc
.end patch 2.5

.
TYPREAD
.      READ TYPIST FILE TO GET TYPIST'S NAME.
         CMATCH    " " TO NTypType
         CALL      OOPS IF EQUAL
         CALL      OOPS IF EOS
         MOVE      C2 TO NUSEPATH
.         MOVE      NTYPFLD TO NUSEFLD
         CLEAR     NUSEFLD2
         PACK      NUSEFLD2 FROM NTypType,B1
         CALL      NUSEKEY
         CALL      NOTYPIST IF OVER
..begin patch xxx
           if         (NuseActive = No)
Inact      call       Nuseks
                      if         over                             .only one typist and inactive use anyway
                      CALL      NUSEKEY
                      else
                                 if         (NuseActive = No)
                                 goto       Inact
                                 endif
                                 if         (NtypType <> Nuseinit)
                                 call       NoTypist
                                 endif
                      endif
           endif      
..end patch xxx
.
         DISPLAY   *P1:23,*EL,"WORKING ON ",NTypType,B1,NUSEFLD,B1,NUSEUSER;
         GOTO      DETAIL
.
OOPS     MOVE      "**" TO NTypType
         RETURN
.
NOTYPIST
         MOVE      "TYPIST UNKNOWN " TO NUSEUSER
         RETURN
DETAIL
         MOVE      CANCOUNT,CANC
.begin patch 2.33
.         COMPARE   "45" TO LINES
.         CALL      HEADER IF NOT LESS
.begin patch 2.5
.          if (row >= 7100)        .Position of Largest Possible Last Record
.          call      Header
.          endif
.          prtpage   prfile;*pcolumn:row,*ALIGNMENT=*Left,*font=font8,*boldon,NTypType,B1,nuseuser;
.          add       eightlpi,row
.          prtpage   prfile;*p1500:row,*ALIGNMENT=*Right,"New";
.          prtpage   prfile;*pcolumn1:row,*ALIGNMENT=*Right,Subcount:
.                    *pcolumn2:row,*ALIGNMENT=*Right,Totcalc:
.                    *pcolumn3:row,*ALIGNMENT=*Right,Pndcount:
.                    *pcolumn4:row,*ALIGNMENT=*Right,Pcalc:
.                    *pcolumn5:row,*ALIGNMENT=*Right,LSubcnt:
.                    *pcolumn6:row,*ALIGNMENT=*Right,LTcalc:
.                    *pcolumn7:row,*ALIGNMENT=*Right,INVcount:
.                    *pcolumn8:row,*ALIGNMENT=*Right,Invcalc:
.                    *pcolumn9:row,*ALIGNMENT=*Right,adjcount:
.                    *pcolumn10:row,*ALIGNMENT=*Right,adcalc:
.                    *pcolumn11:row,*ALIGNMENT=*Right,LStcount:
.                    *pcolumn12:row,*ALIGNMENT=*Right,Lstcalc
.          add       eightlpi,row
..          prtpage   prfile;*p1500:row,*ALIGNMENT=*Right,"Reprint/Update";
.          prtpage   prfile;*p1500:row,*ALIGNMENT=*Right,"Updates";
.          prtpage   prfile;*pcolumn1:row,*ALIGNMENT=*Right,Repcount:
.                    *pcolumn2:row,*ALIGNMENT=*Right,REpcalc:
.                    *pcolumn3:row,*ALIGNMENT=*Right,PndUcount:
.                    *pcolumn4:row,*ALIGNMENT=*Right,PUcalc:
.                    *pcolumn5:row,*ALIGNMENT=*Right,Lrepcnt:
.                    *pcolumn6:row,*ALIGNMENT=*Right,Lrcalc:
.                    *pcolumn7:row,*ALIGNMENT=*Right,INVRcnt:
.                    *pcolumn8:row,*ALIGNMENT=*Right,InvRcalc:
.                    *pcolumn11:row,*ALIGNMENT=*Right,LStucount:
.                    *pcolumn12:row,*ALIGNMENT=*Right,Lstucalc:
.                    *boldoff;
.          add       eightlpi,row
.          add       eightlpi,row
              add             C1,howmany
              move            howmany,str9
              call            Trim using str9
              pack            str12,"A",str9
              setprop         sheet.Range(str12),*Value=nuseuser
              setprop         sheet.range(str12).Font,*Bold="True"

              pack            str12,"B",str9
              setprop         sheet.Range(str12),*Value="New"

              pack            str12,"C",str9
              setprop         sheet.Range(str12),*Value=subcount,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"D",str9
              setprop         sheet.Range(str12),*Value=Totcalc,*NumberFormat="##,####0.00_);[Red](##,####0.00)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"E",str9
              setprop         sheet.Range(str12),*Value=PndCount,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"F",str9
              setprop         sheet.Range(str12),*Value=PCalc,*NumberFormat="##,####0.00_);[Red](##,####0.00)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"G",str9
              setprop         sheet.Range(str12),*Value=LSubcnt,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"H",str9
              setprop         sheet.Range(str12),*Value=LTcalc,*NumberFormat="##,####0.00_);[Red](##,####0.00)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"I",str9
              setprop         sheet.Range(str12),*Value=INVcount,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"J",str9
              setprop         sheet.Range(str12),*Value=INVcalc,*NumberFormat="##,####0.00_);[Red](##,####0.00)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"K",str9
              setprop         sheet.Range(str12),*Value=adjcount,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"L",str9
              setprop         sheet.Range(str12),*Value=Adcalc,*NumberFormat="##,####0.00_);[Red](##,####0.00)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"M",str9
              setprop         sheet.Range(str12),*Value=LStcount,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"N",str9
              setprop         sheet.Range(str12),*Value=Lstcalc,*NumberFormat="##,####0.00_);[Red](##,####0.00)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"o",str9
              setprop         sheet.Range(str12),*Value=DBcount,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"P",str9
              setprop         sheet.Range(str12),*Value=DBcalc,*NumberFormat="##,####0.00_);[Red](##,####0.00)",*HorizontalAlignment=xlAlignCenter

              add             C1,howmany
              move            howmany,str9
              call            Trim using str9
              pack            str12,"A",str9
              setprop         sheet.Range(str12),*Value=NTypType
              pack            str12,"B",str9
              setprop         sheet.Range(str12),*Value="Updates"

              pack            str12,"C",str9
              setprop         sheet.Range(str12),*Value=Repcount,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"D",str9
              setprop         sheet.Range(str12),*Value=Repcalc,*NumberFormat="##,####0.00_);[Red](##,####0.00)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"E",str9
              setprop         sheet.Range(str12),*Value=PndUCount,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"F",str9
              setprop         sheet.Range(str12),*Value=PUCalc,*NumberFormat="##,####0.00_);[Red](##,####0.00)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"G",str9
              setprop         sheet.Range(str12),*Value=Lrepcnt,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"H",str9
              setprop         sheet.Range(str12),*Value=Lrcalc,*NumberFormat="##,####0.00_);[Red](##,####0.00)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"I",str9
              setprop         sheet.Range(str12),*Value=INVrcnt,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"J",str9
              setprop         sheet.Range(str12),*Value=INVrcalc,*NumberFormat="##,####0.00_);[Red](##,####0.00)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"M",str9
              setprop         sheet.Range(str12),*Value=LStucount,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"N",str9
              setprop         sheet.Range(str12),*Value=Lstucalc,*NumberFormat="##,####0.00_);[Red](##,####0.00)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"o",str9
              setprop         sheet.Range(str12),*Value=DBucount,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter
              pack            str12,"P",str9
              setprop         sheet.Range(str12),*Value=DBucalc,*NumberFormat="##,####0.00_);[Red](##,####0.00)",*HorizontalAlignment=xlAlignCenter
             Move     Howmany,n9
             sub      c1,n9
             move     n9,str10
             call     trim using str10
             pack     str9 from "A",str10
             call     trim using str9
             sheet.range(str9,str12).BorderAround using *LineStyle=1,*Weight=3
.end patch 2.5






          
.         PRINT     *N,TYPE,B1,NUSEUSER;
.         PRINT     *N,*20,"NEW",*32,SUBCOUNT,*41,TOTCALC,*51,pndcount,*60,Pcalc;
.         PRINT     *70,LSUBCNT,*80,LTCALC,*89,INVCOUNT,*96,INVCALC:
.                   *108,AdjCOUNT,*114,AdCALC,*127,lstcount,*136,lstcalc:
.                   *N,*20,"REPRINT",*32,REPCOUNT,*41,REPCALC;
.         PRINT     *70,LREPCNT,*80,LRCALC,*89,INVRCNT,*96,INVRCALC,*127,lstucount,*136,lstuCalc
.         ADD       "3" TO LINES
.end patch 2.33
         GOTO      READKS
HEADER
.begin patch 2.33
.begin patch 2.5
.        add c1 to Pgcnt
.        if (pgcnt = c1)
.          prtpage prfile;*UNITS=*HIENGLISH:
.                       *ORIENT=*LANDSCAPE;        
.        else
.          prtpage prfile;*NEWPAGE:
.                 *UNITS=*HIENGLISH:
.                       *ORIENT=*LANDSCAPE;
.          endif                       
.        clear   row
.          move    "300",row
.          prtpage prfile;*pcolumn:row,*font=font12,*boldon,"Confidential";
.          prtpage prfile;*pTitle1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,"Names in the News";
.          prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date:",*boldoff;
.          clock timestamp,str8
.          unpack str8,str2,yy,mm,dd
.          clear str10
.          pack  str10,mm,slash,dd,slash,str2,yy
.          prtpage prfile;*pTitle3:row,*font=font12,str10;
.          add     eightlpi,row
.          add     eightlpi,row
.          prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,*ULON,*ll,"MTD Data Entry Analysis",*ULOFF;
..          prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,*ULON,*ll,"Data Entry Analysis",*ULOFF;
.          prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Page:",*boldoff;
.          prtpage prfile;*pTitle3:row,*font=font12,Pgcnt;
.          add     eightlpi,row
.          add     eightlpi,row
.          prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,*ULON,*ll,BigDate,*ULOFF;
.          add     eightlpi,row
.          add     eightlpi,row
.          prtpage prfile;*pcolumn:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Typist",*uloff,*boldoff;
.          prtpage prfile;*pcolumn1:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Orders",*uloff,*boldoff;
.          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"   %   ",*uloff,*boldoff;
.          prtpage prfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Pending",*uloff,*boldoff;
.          prtpage prfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"   %   ",*uloff,*boldoff;
.          prtpage prfile;*pcolumn5:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Lcr's",*uloff,*boldoff;
.          prtpage prfile;*pcolumn6:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"   %   ",*uloff,*boldoff;
.          prtpage prfile;*pcolumn7:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Invoices",*uloff,*boldoff;
.          prtpage prfile;*pcolumn8:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"   %   ",*uloff,*boldoff;
.          prtpage prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"Adjst",*uloff,*boldoff;
.          prtpage prfile;*pcolumn10:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"   %   ",*uloff,*boldoff;
.          prtpage prfile;*pcolumn11:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"List's",*uloff,*boldoff;
.          prtpage prfile;*pcolumn12:row,*ALIGNMENT=*RIGHT,*font=font12,*boldon,*ulon,"   %   ",*uloff,*boldoff;
.          add     eightlpi,row
.          add     eightlpi,row
.          add     eightlpi,row
.end patch 2.5
.          
.         MOVE      "7" TO LINES
.         PRINT     *F,*l,*l,*41,"***   N A M E S   I N   T H E   ";
.         PRINT     *74,"N E W S   ***",*140,PRTDATE:
.                   *L,*140,"PAGE :",PAGE:
.                   *N,*54,HPBON," TYPIST ANALYSIS":
.                   *L,*L,*1,"TYPIST",*32,"ORDERS",*43,"% ",*51,"PENDNG",*62,"%";
.         PRINT     *70,"LCR'S",*81,"% ",*89,"INVOICE",*100,"%",*108,"ADJST",*119,"%":
.                   *127,"Lists",*138,"%":
.                   *L,*1,"------------------------";
.         PRINT     *32,"------",*40,"-------",*51,"------",*59,"------",*70,"------",*80,"------",*89;
.         PRINT     "--------",*97,"-------":
.                   *108,"------",*116,"-------",*127,"------"
.         ADD       C1 TO PAGE
.end patch 2.33

         RETURN
.
EOJ
.begin patch 2.33
.         COMPARE   "45" TO LINES
.         CALL      HEADER IF NOT LESS
.begin patch 2.5
.          if (row >= 7100)        .Position of Largest Possible Last Record
.          call      Header
.          endif
.          add     eightlpi,row
.          add     eightlpi,row
.          add     eightlpi,row
.          add     eightlpi,row
.          prtpage   prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of new orders:":
.                    *pcolumn3:row,*ALIGNMENT=*RIGHT,Ordsum;
..          prtpage   prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of reprints:":
.          prtpage   prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of Updatess:":
.                    *pcolumn10:row,*ALIGNMENT=*RIGHT,OrdRsum;
.          add     eightlpi,row
.          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of pnd orders:":
.                    *pcolumn3:row,*ALIGNMENT=*RIGHT,POrdsum;
.          prtpage   prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of updates:":
.                    *pcolumn10:row,*ALIGNMENT=*RIGHT,PndUsum;
.          add     eightlpi,row
.          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of new lcrs:":
.                    *pcolumn3:row,*ALIGNMENT=*RIGHT,Lcrsum;
..          prtpage   prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of reprints:":
.          prtpage   prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of Updates:":
.                    *pcolumn10:row,*ALIGNMENT=*RIGHT,LcrRsum;
.          add     eightlpi,row
.          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of new invoices:":
.                    *pcolumn3:row,*ALIGNMENT=*RIGHT,Invsum;
.          prtpage   prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of reprints:":
.                    *pcolumn10:row,*ALIGNMENT=*RIGHT,InvRsum;
.          add     eightlpi,row
.          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of new adjustments:":
.                    *pcolumn3:row,*ALIGNMENT=*RIGHT,ADJsum;
.          add     eightlpi,row
.          prtpage prfile;*pcolumn2:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of new datacards:":
.                    *pcolumn3:row,*ALIGNMENT=*RIGHT,Lstsum;
.          prtpage   prfile;*pcolumn9:row,*ALIGNMENT=*RIGHT,*font=font8,*ll,"Total number of updates:":
.                    *pcolumn10:row,*ALIGNMENT=*RIGHT,Lstusum;
.          add     eightlpi,row

           add        c2,Howmany
              move            howmany,str9
              call            Trim using str9
              pack            str12,"A",str9
              setprop         sheet.Range(str12),*Value="Totals"
              setprop         sheet.Range(str12).Font,*Bold="TRUE"
              pack            str12,"B",str9
              setprop         sheet.Range(str12),*Value="New"
              setprop         sheet.Range(str12).Font,*Bold="TRUE"
              pack            str12,"C",str9
              setprop         sheet.Range(str12),*Value=Ordsum,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"E",str9
              setprop         sheet.Range(str12),*Value=POrdsum,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"G",str9
              setprop         sheet.Range(str12),*Value=Lcrsum,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"I",str9
              setprop         sheet.Range(str12),*Value=Invsum,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"K",str9
              setprop         sheet.Range(str12),*Value=ADJsum,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"M",str9
              setprop         sheet.Range(str12),*Value=Lstsum,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"O",str9
              setprop         sheet.Range(str12),*Value=DBsum,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter

    
              add             C1,howmany
              move            howmany,str9
              call            Trim using str9
              pack            str12,"B",str9
              setprop         sheet.Range(str12),*Value="Updates"
              setprop         sheet.Range(str12).Font,*Bold="TRUE"

              pack            str12,"C",str9
              setprop         sheet.Range(str12),*Value=OrdRsum,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"E",str9
              setprop         sheet.Range(str12),*Value=PndUsum,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"G",str9
              setprop         sheet.Range(str12),*Value=LcrRsum,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"I",str9
              setprop         sheet.Range(str12),*Value=InvRsum,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"M",str9
              setprop         sheet.Range(str12),*Value=Lstusum,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"O",str9
              setprop         sheet.Range(str12),*Value=DBusum,*NumberFormat="##,####0_);[Red](##,####0)",*HorizontalAlignment=xlAlignCenter

              pack            str12,"P",str9

              pack            str9,"a5"
              call            Trim using str9
             sheet.range(str9,str12).Columns.Autofit

             Move     Howmany,n9
             sub      c1,n9
             move     n9,str10
             call     trim using str10
             pack     str9 from "A",str10
             call     trim using str9
             sheet.range(str9,str12).BorderAround using *LineStyle=1,*Weight=3


             sheet.range(str9,str12).Columns.Autofit
ExcelFileNameSelect
          clear   taskname
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
                              Append    "C:\WORK\Typistrep",taskname    
                              if        (#ver = c1)
                              append  ".xlsx",taskname
                              else
                              append  ".xls",taskname
                              endif
                              Reset     Taskname                              
                              erase          taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                              trap    TrapExcelObject if Object
                              book.saveas giving N9 using *Filename=taskname
                              trapclr Object
.
                              setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
CleanUp
                    setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.Clean up after myself
                              destroy        OTRUE
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
                              destroy        OFALSE
                              setprop ex,*SheetsInNewWorkbook=SheetsDefault
                              ex.quit
                              destroy ex

.end patch 2.5


.         PRINT     *L,*L;
.         PRINT     *L,*30,"TOTAL NUMBER OF NEW ORDERS     :",Ordsum:
.                   *80,"TOTAL NUMBER OF REPRINTS    :",ordRsum:
.                   *L,*30,"TOTAL NUMBER OF PND ORDERS     :",Pordsum;
.         PRINT     *L,*30,"TOTAL NUMBER OF NEW LCR'S      :",LCrsum;
.         PRINT     *80,"TOTAL NUMBER OF REPRINTS    :",LCRrsum;
.         PRINT     *L,*30,"TOTAL NUMBER OF NEW INVOICES   :",INVsum:
.                   *80,"TOTAL NUMBER OF REPRINTS    :",INVRsum;
..         PRINT     *L,*30,"TOTAL NUMBER OF NEW APPROVALS:",APPTOT
.         PRINT     *L,*30,"TOTAL NUMBER OF NEW DATACARDS   :",LSTsum:
.                   *80,"TOTAL NUMBER OF UPdates    :",LstUsum;
.         PRINT     *L,*30,"TOTAL NUMBER OF NEW ADJUSTMENTS:",ADJsum
.end patch 2.33
.         PRINT     *L,*30,"TOTAL NUMBER OF CORRECTIONS FOR MONTH:",CORsum;
.        PRINT     *L,*30,"TOTAL NUMBER OF CANCELATIONS FOR MONTH:",CANcsum
STOP     DISPLAY   *P1:24,*EL,*B,"JOB DONE, SHUTTING DOWN TO CONTINUE CHAIN":
                   *W2,*B;
         CMATCH    "D" TO ANS
         IF         equal
         Shutdown             "CLS"
         endif
.         STop       IF EQUAL
.begin patch 2.33
.         PRINT     HPPORT,HPRESET
.         SPLCLOSE
          if        (ans = "P")
          PRtclose  PrFile
CheckFile

          pack      str55 from Taskname
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
          Pause     "15"
          MOVE      C0,TrapCount
          Pack       MailSubjct,"Data Entry report  "
          pack      MailAttach from str55

        
.begin patch 2.42
.          move      "DavidHerrick@nincal.com,ReubenHolland@nincal.com,SuzanneMcGuire@nincal.com" to mailto
          move      "DavidHerrick@nincal.com,ReubenHolland@nincal.com" to mailto
          move      "DavidHerrick@nincal.com" to mailto
.end patch 2.42
          move      "creques@nincal.com" to mailfrom
.          move      "DavidHerrick@nincal.com" to mailto

..First check 995 autolaunch settings
.        call       GetPDFPath
.                    pack      str45 from PDFPATH,"\res\pdf995.ini"
. 
.                   call      "GU$INI;WRITE_TO_INI" USING str45:
.                             "Parameters":
.                             "ProcessPDF":
.                             "\\nins1\e\apps\winbatch\Del995flag.exe":
.                              result
.                              if (result = C0)
..Prepare Flag file
.                                        pack      str45 from PDFPATH,"\Flag.dat"
.                                        prep      tempfile,str45
.                                        write     tempfile,SEQ;"flag set"
.                                        close     tempfile
.                              endif
.                    Call      PDF995Auto
                    Move      "30",MailTimer
                    call      SendMail
          endif
.end patch 2.33
         shutdown   "CLS"
         STOP
Repair
              KEYIN           *P10:12,"REPAIR CCYYMM ",*p22:12,*el,*+,*dv,CC,*dv,YY,*dv,MM:
                              *p22:12,*t30,*ZF,*JR,*rv,CC,*rv,YY,*rv,mm
              pack            ntypfld from cc,yy,mm
              move            ntypfld to str6
              call            Ntyptst
RepLoop       call            nTypks
              if              Not over
              scan            "99" in NTypType
              goto            reploop if equal
                              if              (typdate = str6)
                              goto           repair1
                              else
                              goto           RepLoop
                              endif
Repair1       add            SUBCOUNT to ordsum
              add             REPCOUNT to OrdRsum
              add             SUBQTY   to QtySum
              add             LSUBCNT  to LCrsum
              add             LREPCNT  to LcrRSum
              add             INVCOUNT to Invsum     
              add             INVRCNT  to InvRsum    
              add             CORCOUNT To CorSum     
              add             CANCOUNT to Cancsum    
.              add             APPCOUNT to AppSum     
              add             PndUCOUNT to pndUSum     

              add             ADJCount to AdjSum     
              add             PndCOunt to Pordsum    
              add             lstCount to Lstsum     
              add             LStUCOunt to LstUSum
              goto            reploop
              else
              packkey         ntypfld from str6,"99 "
              call            Ntypkey
              Move            OrdSum to SUbCount
              Move            OrdRsum to RepCount
              Move            QtySum to SubQty
              Move            LcrSum to LSUBCNT
              Move            LcrRsum to LREPCNT
              Move            Pordsum to PndCOunt
              Move            Invsum to invcount
              Move            InvRsum to INVRcnt
              Move            Corsum to Corcount
              move            Cancsum to cancount
.              move            appsum to appcount
              move            pndusum,pnducount
              move            adjsum to adjcount
              move            lstsum to lstcount
              move            lstusum to lstUcount
              call            ntypupd
              endif
              shutdown        "cls"
              stop
.              

.begin patch 2.33
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"data Enry report - ",str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
.                   Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    append    CRLF,MailBOdy
                    append    "c:\work\pdf\typistrep.pdf",Mailbody
                    append    CRLF,MailBOdy
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return

                    endif
          
                    goto      checkfile
.begin patch 2.5
TrapExcelObject
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
        goto ExcelFileNameSelect
.end patch 2.5

. IO - I/O ERROR TRAPS
IO
         TRAPCLR   IO
         NORETURN
IOBRANCH
         DISPLAY   *P1:23,*EL,ERROR
         BRANCH    IOERROR OF FILE1
         DISPLAY   *P1:24,*EL,"UNKNOWN RUN TIME IO ERROR",*B;
         GOTO      IOEXIT
FILE1
         DISPLAY   *P1:24,"ORDER PRINT FILE ERROR",*B,*W2;
         GOTO      IOEXIT
IOEXIT
         KEYIN     *P50:24,ANS;
         CMATCH    "Q" TO ANS
         GOTO      IOBRANCH IF NOT EQUAL
.         SHUTDOWN  "ALERT"
         shutdown   "CLS"
         STOP
         INCLUDE   NUSEIO.inc
         INCLUDE   NTYPIO.inc
         INCLUDE   COMLOGIC.inc

