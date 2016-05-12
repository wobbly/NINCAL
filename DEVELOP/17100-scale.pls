*******************************************************************************
. THIS IS THE EXCHANGE STATUS  PRINT PROGRAM    APRIL 1983              *
*******************************************************************************
. Written for Names in the News California By David Herrick                   *
*******************************************************************************
.
* VARIABLES USED BY THE EXCHANGE STATUS PROGRAM.
.
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
                              include compdd.inc
                              include   cntdd.inc
         include   nxrfdd.inc
         include   ndatdd.inc
         include   norddd.inc
         INCLUDE   HP.inc
         INCLUDE   NOFRDD.INC
          INCLUDE   NSEL2DD.INC
          include   NXNGDD.INC
          INCLUDE   NXCHDD.INC
release   init      "4.3"                      DLH       Internal PDf, rewrite to prtpage
reldate   init      "2014 March 24"
.PLease see archives for previous changes
INPUT    FILE
FileCheck FIle
trapcount form      4
PrintFlag Form      1
          include   prtpagedd.inc
P_FLAGS_EMBED_FONTS EQU 2
P_FLAGS_SUB_GENFONTS EQU 4
.end patch 4.2

.............................................................................................
.some excel goodies
.to find version of excel
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
.to find version of excel
sheetno    form      2
NumberofSheets Integer        4,"0x00000000"
RowNumber     Dim             9
books   automation
book    automation
sheets  automation
sheet   automation
Rowcol  automation
ex      automation      class="Excel.Application"
RecordHeader form 9
RecordTop form  9
N34     form    3.4
N92     form    9.2
MailDate dim    4
VT_BOOL EQU 11          .Boolean
OTRUE   variant
OFALSE  variant
xlLeft integer 4,"0xffffefDD"
xlTop integer 4,"0xffffefc0"
XLAlignLeft                               integer 4,"0xffffefdd"
XlAlignRight                              integer 4,"0xffffefc8"
xlAlignCenter integer 4,"0xffffeff4"
xlBottom  integer      4,"0xffffeff5"
XlLineStyleDBl Integer 4,"0xffffefe9"                         .line style double
XLShiftToLeft       Variant                        .range delete shift to left
XLShiftUp Variant                        .range delete shift Up     
xlLandscape integer 4,"0x2"                     .2
xlUnderlineStyleSingle integer 4,"0x2"

xlInsideHorizontal            integer 4,"0x12"                .Borders inside defined range
xlInsideVertical              integer 4,"0x11"                .Borders inside defined range
XlEdgeRight                   integer 4,"0x10"                .Borders right edge of defined range
VT_R8         EQU 5           .Double - 8 byte Real
VT_R8a         EQU 5           .Double - 8 byte Real
xlRowHeight   variant
xlColumnWidth variant
xlColumnWidth2 variant
xlColumnWidth13 variant
TopMargin     variant
BottomMargin  variant
LeftMargin     variant
RightMargin     variant

REportHdr     Dim             250
ExRange1      Dim             6
ExRange2      Dim             6
ExRange3      Dim            14
Row1          form            3
Row2          Form            3
Row3          form            3
Row4          Form            3
DimRow1       Dim             3
DimRow2       Dim             3
DimRow3       Dim             3
DimRow4       Dim             3
.end patch 4.10

..........................................
MLR1     DIM       6              MAILER#1
MLR2     DIM       6              MAILER # 2
MDES1    DIM       45
.
+ .WORK FIELDS
.
DATE     DIM       8
FMESG    DIM       24
WORK04   DIM       4
SENTRY   DIM       5              *TEMP. FIELD FOR ENTRY NUMBER.
NSW      DIM       1              SWITCH FOR NEW RECORD CONTAINS "N" IF
.                                 RECORD IS TO BE CREATED
PRTSW    DIM       1              PRINT SWITCH  'T'=totals
LNAME    DIM       35             LIST DESC.
recsin   form      4
.
+ PRINT VARIABLES
Font1    font
.Arial
        create  Font1,"Calibri",size=10
.        create  Font1,"Times New Roman",size=10
Font1i    font
        create  Font1i,"Calibri",size=10,italic        
.needed for printing to PDF for non native fonts and things like Copyright to appear correctly in the pdf
PDF_FLAGS_WIN_ANSI_ENCODING EQU 128


MLRTAB   FORM      3
. ...............
.
LN       DIM       6
LINES    FORM      2
NOFILE   DIM       8
PMO      DIM       2
PDAY     DIM       2
PYR      DIM       2
SUBTOT1  FORM      10
GRNDTOT1 FORM      10
GRNDTOT2 FORM      10
PAGE     FORM      3
PAGENUM  DIM       3
PAGEMASK INIT      "ZZ9"
TEMPMLR1 DIM       6
PICSW    DIM       1                   HOLDS (1) IF PAYABLE PICK,(2) IF REC.
.                                      (3) IF EVEN, 4 IF ALL
STOPLOOP DIM       1
STATMSG  DIM       11                 HOLDS "*CANCELLED*" IF ORDER CANCELLED.
MLRDES1  DIM       45
MLRDES2  DIM       45                 USED TO SAVE MASTER MAILER NAMES FOR
.                                     DETAILED REPORTS.
holdsw   dim       1
DONESW   DIM       1
QTYMASK  INIT      "ZZZ,ZZZ,ZZ9"
QTYOUT   DIM       11
MASK     INIT      "Z,ZZZ,ZZZ,ZZ9"
MASKA    DIM       13                 USED FOR EDITING OUTPUT.
NUM      FORM      1                  BRANCHING CONSTANT.
TOTAL    FORM      9                  PRINT TOTAL BALANCE ON TOTAL'S REPORT.
TITLE    DIM       40                 FOR KEYED IN TITLE LINE.
DASHLINE DIM       40                 CUSTOM UNDERLINE FOR KEYED IN TITLE LINE.
KEYS     FORM      2                  NUMBERS OF CHARACTERS IN TITLE.
placemnt FORM      3                  STARTING POSITION FOR TITLE PRINT LINE.
LOCAL    INIT      "LOCAL"
univflag form      1                  1=dont print, 2=print
unimask  dim       13
umbrflag form      1                  1=no, 2=umbrella exch org.
DUPFLAG  form      1                  1=simplex, 2=duplex
listflag form      1                  1=no list number 2=list number (V)
TRANflag form      1                  1=no list number 2=list number (V)
detflag  form      1                  1=no, 2=print offer, select, & MLR PO if avail.
DimPtr   dim       4                  Mailer Number
DimPtr1  dim       4                  List Number
USEDFLAG init      "Y"                .Flag to see if mailer ever used
NEVERUFLAG dim     1
LogoFlag  Dim       1
REVVFLAG  DIM      1                  ;flag to see if inserting revision date
REVISIONDATE    DIM     16            DIM to insert REVISION DATE
ExcelFlag Form       1
PDFFLAG   FORM      1
.
. DEFAULT OPTIONS ARE : NONE
.                       laser PRINTER FORMAT
.
. COMMENT CAN MODIFY THE DEFAULTS:
.                                   duplex :  PRINT both sides
.
+******************************************************************************
. PROGRAM
*******************************************************************************
.
PREP     CMATCH    B1 TO PROGRAM
         IF        EOS
         MOVE      "NXCH0003" TO PROGRAM
         MOVE      "EXCHANGE PRINT PRGRAM" TO STITLE
          If        (LogoFLag = "P")
          MOVE      "PL" TO COMPNME
          else
          MOVE      "NIN" TO COMPNME
          endif
         MOVE      "EXSTAT" TO INPNAME
         MOVE      "LOCAL" TO PRTNAME
         ENDIF
         move      c2 to nxrfpath
         MOVE      "EXIT" TO PF5
         CLOCK     DATE TO DATE
         IFNZ      PC
         MOVE      "Z9/99/99" TO TODAY
         MOVE      DATE TO N6
         EDIT      N6 TO TODAY
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         XIF
         CALL      PAINT
         CALL      FUNCDISP
         move      c2 to detflag            .temp
BEGIN    DISPLAY   *P01:05,"Options     :":
                   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
.
         GOTO      OPTION
OPTGET   MOVE      C0 TO DUPFLAG
         RESET     COMMENT
         KEYIN     *P20:10,"DUPLEX  :  Print Both sides":
                   *P20:11,"List    :  Print List ## on (V)":
                   *P20:1,"XLS     :  Load in Excel":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:16,*EL,*P20:17,*EL;
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
OPTION   MOVE      C0 TO DUPFLAG
          MOVE      C1 TO ExcelFLAG
          RESET     COMMENT
         SETLPTR   COMMENT
         DISPLAY   *P15:05,COMMENT
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
         RESET     COMMENT
         SCAN      "DUPLEX" IN COMMENT
         CALL      OPTDUPLX IF EQUAL
         RESET     COMMENT
         SCAN      "LIST" IN COMMENT
         CALL      OPTlist IF EQUAL
         RESET     COMMENT
         RESET     COMMENT
         SCAN      "XLS" IN COMMENT
         CALL      OPTXLS IF EQUAL
         RESET     COMMENT
.begin patch 4.2
         RESET     COMMENT
         SCAN      "PDF" IN COMMENT
         CALL      OPTPDF IF EQUAL
         RESET     COMMENT
.end patch 4.2

         GOTO      INPGET
OPTNG    KEYIN     *P20:10,"Duplex  :  Print both sides":
                   *P20:11,"List    :  Print List ## on (V)":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL,*P20:16,*EL,*P20:17,*EL;
         GOTO      OPTGET
OPTDEFLT MOVE      C1 TO DUPFLAG
         move      c1 to listflag
         MOVE      C1 TO ExcelFLAG
.begin patch 4.2
         MOVE      C1 TO PDFFLAG
.end patch 4.2
         goto      inpget
OPTDUPLX MOVE      C2 TO DUPFLAG
         RETURN
OPTLIsT  MOVE      C2 TO LISTFLAG
         RETURN
OPTXLS    MOVE      C2 TO ExcelFLAG
         RETURN
.begin patch 4.2
OPTPDF    MOVE      C2 TO PDFFLAG
         RETURN
.end patch 4.2
.
.
.
INPGET   
          TRAP      INPNG GIVING ERROR IF IO
         OPEN      INPUT,INPNAME,exclusive
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         READ      INPUT,SEQ;TEMPMLR1,WORK04,PICSW,univflag,TRANFLAG,NEVERUFLAG,REVVFLAG,*tab=153,LogoFlag
          Close     Input
          pack      str12 from inpname,".srt"
          pack      str15 from inpname,".dat"
          if        (work04 = "H   ")
          pack      taskname from "\\nins1\e\data\",str15,",\\nins1\e\data\",str12," -236-260,65-72,21-26"
          else
          pack      taskname from "\\nins1\e\data\",str15,",\\nins1\e\data\",str12," -74,13-17"
          endif
          Sort      Taskname
          open      input,str12,exclusive
          
         GOTO      PRTGET
INPNG    NORETURN
         TRAPCLR   IO
         KEYIN     *P01:24,*EL,"The Input file is not on-line, or Busy. ":
                   *DV,ERROR:
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   
          if        (ExcelFlag <> c2)
          MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MATCH     LOCAL  TO PRTNAME
         GOTO      PRESTART IF EQUAL
.begin patch 4.2
         
.         PACK      PRTFILE WITH pdrive,PRTNAME
.         SPLOPEN   PRTFILE
          if        (func > "" & func <> "  ")
          move      func,printflag
          else
          move      c2,printflag
          endif
         DISPLAY   *P15:07,PRTNAME
.          pack    str45,prtname
          if        (pdfflag = c2)
          pack      str55 from "c:\work\pdf\",prtname,".pdf"
          PRTOPEN   Laser,"PDF:",str55,Flags=PDF_FLAGS_WIN_ANSI_ENCODING
          pack    str45,prtname

          else
	  call      GetWinVer
          
                    if (PrintFlag = C0 | PrintFlag = 1)     .Laser2 
                              if (osflag = c1 | Osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt win2k Xp
                                          PRTOPEN Laser,"\\NINS2\laser2",prtname
                                  elseif (osflag = c3 | OSflag =c4)         .win 95 98
                                          PRTOPEN Laser,"Laser2",prtname
                                  else   .(osflag = c0)         .Don't know prompt for printer
                                          PRTOPEN Laser,"-",prtname
                                  endif
                          elseif (PrintFlag = 2)  .Laser3
                                  if (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt 2k xp
                                          PRTOPEN Laser,"\\NINS2\laser3 Blankstock",prtname
                                  elseif (osflag = c3 | osflag =c4)         .win 95 98
                                          PRTOPEN Laser,"Laser3 Blankstock",prtname
                                  else   .(osflag = c0)         .Don't know prompt for printer
                                          PRTOPEN Laser,"-",prtname
                                  endif
                          elseif (PrintFlag = 5)  .Susan
                                    if (osflag = c2)         .nt
                                            PRTOPEN Laser,"Kyocera FS-1030D",Str45
                                    elseif (osflag = c6 | osflag = c8 | osflag = c9)         .XP
                                            PRTOPEN Laser,"Kyocera FS-1030D",Str45
                                    elseif (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .win 95 98
                                            PRTOPEN Laser,"KYOCERAS",Str45
                                    else   .(osflag = c0)         .Don't know prompt for printer
                                            PRTOPEN Laser,"@",Str45
                                    endif
                          elseif (PrintFlag = 7)  .david
                                    if (osflag = c2)         .nt +
                                            PRTOPEN Laser,"Kyocera FS-C5030N KX",Str45
                                    elseif (osflag = c6 | osflag = c8 | osflag = c9)         .XP
                                            PRTOPEN Laser,"Kyocera FS-C5030N KX",Str45
                                    elseif (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .win 95 98
                                            PRTOPEN Laser,"KYOCERAM",Str45
                                    else   .(osflag = c0)         .Don't know prompt for printer
                                            PRTOPEN Laser,"@",Str45
                                    endif
			Else			.we don't know send it to laser3
                                  if (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt 2k xp
                                          PRTOPEN Laser,"\\NINS2\laser3 Blankstock",prtname
                                  elseif (osflag = c3 | osflag =c4)         .win 95 98
                                          PRTOPEN Laser,"Laser3 Blankstock",prtname
                                  else   .(osflag = c0)         .Don't know prompt for printer
                                          PRTOPEN Laser,"-",prtname
                                  endif
                    endif
          endif

.end patch 4.2
          Else
.Open Excel application
        create  ex
.Reset Default of Worksheets found in a Workbook
        setprop ex,*SheetsInNewWorkbook=C1
.Create Workbooks collection
        getprop ex,*Workbooks=books
.Create/Add a single Workbook
        books.add
        books.item giving book using 1
.Create Worksheets collection
        getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
        sheets.item giving sheet using 1
        create   xlShifttoLeft,VarType=VT_R8,VarValue="-4159"
        create   xlShiftUp,VarType=VT_R8,VarValue="-4162"
        create xlRowHeight,VarType=VT_R8,VarValue="2.75"
        create xlColumnWidth,VarType=VT_R8a,VarValue="0.46"
        create          OTRUE,VarType=VT_BOOL,VarValue=1
        create          OFALSE,VarType=VT_BOOL,VarValue=0
        create          TopMargin,VarType=VT_R8,VarValue="18"                       Roughly equals .25 inches:  18 * 1.388 = 25
        create          BottomMargin,VarType=VT_R8,VarValue="18"
        create          LeftMargin,VarType=VT_R8,VarValue="5"
        create          RightMargin,VarType=VT_R8,VarValue="5"                Roughly equals .0694 inches:  5 * 1.388 = 6.94
        setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
        create    xlColumnWidth2,VarType=VT_R8,VarValue="2"  
        create    xlColumnWidth13,VarType=VT_R8,VarValue="13"  
          
          endif
         GOTO      PRESTART
          
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
PRESTART
         MOVE      B1,STAT              CLEAR FIELD SO CORRECT STATUS WRITTEN
         MOVE      B1 TO DONESW
         READ      INPUT,SEQ;TEMPMLR1,WORK04,PICSW,univflag,TRANFLAG,NEVERUFLAG,REVVFLAG,*tab=153,LogoFlag

PRT0
         MATCH     "H   " TO WORK04
         IF        EQUAL
         MOVE      "T" TO PRTSW

         compare   c2 to dupflag
                    if        equal
.begin patch 4.2
.                              if        (ExcelFlag = c1)
.                              print     FAXPORT,hpreset,hpdupl,HPTOP
.                              endif
.end patch 4.2
                    endif
                    GOTO      PRT1
         ELSE
                    MOVE      "D" TO PRTSW
                    compare   c2 to dupflag
                    if        equal
.begin patch 4.2
.                              if        (ExcelFlag = c1)
.                              print     hpreset,hpland,hptop,hpdupl,hplin8,hp10ptch
.                              endif
.                    else
.                              if        (ExcelFlag = c1)
.                    print     hpreset,hpland,hptop,hplin8,hp12ptch
.                              endif
.end patch 4.2
                    endif
         CLOSE     INPUT
         GOTO      DETAILED
         ENDIF
         
PRT1     branch    univflag to nuni,yuni
nuni     DISPLAY   *P15:05,COMMENT
         goto      prt1a
yuni     DISPLAY   *P15:05,COMMENT,b1,"UNIVERSE"
         goto      prt1a
prt1a    KEYIN     *P35:09,"CHANGE REPORT TITLE ? ",*T5,STR1;
         CMATCH    YES TO STR1
         GOTO      TITLEKEY IF EQUAL
         CLEAR     TITLE
         CLEAR     DASHLINE
          if        (LogoFLag = "P")
          APPEND    "PL EXCHANGE STATUS REPORT" TO TITLE
          APPEND    "    == ======== ====== ======" TO DASHLINE
          Else
          APPEND    "NIN EXCHANGE STATUS REPORT" TO TITLE
          APPEND    "    === ======== ====== ======" TO DASHLINE
          Endif
         CALL      NEWTTL
         GOTO      PRT11
.
NEWTTL   SETLPTR   TITLE
         ENDSET    TITLE
CHKTTL   CMATCH    B1 TO TITLE
         GOTO      SETTTL IF NOT EQUAL
         BUMP      TITLE BY -1
         GOTO      CHKTTL IF NOT EOS
SETTTL   MOVEFPTR  TITLE TO N3
         MOVE      C80 TO placemnt
         SUBTRACT  N3 FROM placemnt
         DIVIDE    C2 INTO placemnt
         RESET     TITLE
         SETLPTR   TITLE
         RETURN
.
TITLEKEY MOVE      C0 TO KEYS
         KEYIN     *P10:09,*EL,"WOULD YOU LIKE TO KEEP PREVIOUS TITLE? ",STR1;
         CMATCH    YES TO STR1
         GOTO      PRT11 IF EQUAL
         KEYIN     *P10:09,*EL,"TITLE: ",TITLE,*P77:09,"OK?",STR1,*P77:09,*EL;
         CMATCH    B1 TO TITLE
         GOTO      PRT1 IF EQUAL
         GOTO      PRT1 IF EOS
         CMATCH    YES TO STR1
         GOTO      TITLEKEY IF NOT EQUAL
         MOVELPTR  TITLE TO KEYS
MAKEDASH CLEAR     DASHLINE
DODASH   CMATCH    B1 TO TITLE
         GOTO      DASHDONE IF EOS
         GOTO      BUMPDASH IF EQUAL
         APPEND    "=" TO DASHLINE
         BUMP      TITLE BY 1
         GOTO      DASHDONE IF EOS
         GOTO      DODASH
DASHDONE RESET     DASHLINE
         RESET     TITLE
.         GOTO      POSTN
         CALL      NEWTTL
         GOTO      PRT11
BUMPDASH APPEND    B1 TO DASHLINE
         BUMP      TITLE BY 1
         GOTO      DODASH
POSTN
         DIVIDE    C2 INTO KEYS
         SUBTRACT  C1 FROM KEYS
         MOVE      "40" TO placemnt
         SUB       KEYS FROM placemnt
         GOTO      PRT11
ENDPRT   DISPLAY   *P01:03,*EL,"PRINTING IS COMPLETED ! ",*B:
                   "----------------------";
         MOVE      C1,PAGE
         MOVE      C0,LINES
          if        (excelFlag = c1)
.begin patch 4.2
.         PRINT     HPPORT,HPLIN6,HPRESET,hpdupoff
.         splclose
          Prtclose  Laser
          pack      mailattach from "c:\work\pdf\",prtname,".pdf"
.end patch 4.2
          ElseIf    (excelFlag = c2)

          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF

          if        (prtSw = "T")
          Move      c10,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          Move      Row1,DimRow2
          call      Trim using DimRow2                 
          pack      Exrange2 from "C",DimRow2
          sheet.range(ExRange1,ExRange2).Columns.Autofit

          Elseif        (prtSw = "D")

          Move      c11,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "E",DimRow1
          Move      Row1,DimRow2
          call      Trim using DimRow2                 
          pack      Exrange2 from "E",DimRow2
          sheet.range(ExRange1,ExRange2).Columns.Autofit
          Move      c11,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "H",DimRow1
          Move      Row1,DimRow2
          call      Trim using DimRow2                 
          pack      Exrange2 from "H",DimRow2
          sheet.range(ExRange1,ExRange2).Columns.Autofit
          Move      c11,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "C",DimRow1
          Move      Row1,DimRow2
          call      Trim using DimRow2                 
          pack      Exrange2 from "C",DimRow2
          sheet.range(ExRange1,ExRange1).Columns.Autofit
          Setprop Sheet.Range("A1"),*ColumnWidth=xlColumnWidth13
          Setprop Sheet.Range("B1"),*ColumnWidth=xlColumnWidth13
          Setprop Sheet.Range("D1"),*ColumnWidth=xlColumnWidth2
          Setprop Sheet.Range("F1"),*ColumnWidth=xlColumnWidth13
          Setprop Sheet.Range("G1"),*ColumnWidth=xlColumnWidth2
          Setprop Sheet.Range("I1"),*ColumnWidth=xlColumnWidth13
          Setprop Sheet.Range("J1"),*ColumnWidth=xlColumnWidth13

          Add       C2,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          pack      str55 from "©1991-2014, Names in the News"
          setprop sheet.range(ExRange1,ExRange1),*Value=Str55
          endif
..Trap in case a workbook with the same name is already open.  In such a case, the saveas will
..not occur
        move    N2,str2
        rep     zfill,str2
        clear   taskname
        append  "C:\work\Exstat",taskname                                ."
          if        (#ver = c1)
          append  ".xlsx",taskname
          else
        append  ".xls",taskname
          endif
        reset   taskname
        setprop ex,*DisplayAlerts=OFALSE
        book.saveas giving N9 using *Filename=taskname
          setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
        destroy rowcol
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
.        setprop ex,*DisplayAlerts=OFALSE
        ex.quit
        destroy ex
.begin patch 4.2
          move      taskname,mailattach
.end patch 4.2
          
          endif
.end patch 4.10
.begin patch 4.2
          if        (pdfFlag = 2 or Excelflag = 2)
CheckFile
	  pause	    c5
          pack      str55 from mailattach
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO


          Move      "Here is your PDF File",MailSubjct
          Clear     MailBody
          append    Mailattach,mailbody
          append    CRLF,Mailbody
          Reset     MailBOdy
          pack      Mailto from User,"@nincal.com"
          pack      MailFrom from User,"@nincal.com"
          call      Sendmail

          endif
.end patch 4.2
         CLOSE     INPUT
	Erase	Mailattach
         release
         shutdown  "CLS"
         STOP
.
...............................................................................
DETAILED
         OPEN      INPUT,INPNAME
         MOVE      C0 TO PAGE
         call      oh
...............................................................................
PRT1D
          READ      INPUT,SEQ;EXKEY,LR2,LR,USAGE1,USAGE2:
                    QTYFILL,QTY,LIST,cc,Pyr,Pmo,Pday,STAT,MLRSW,TYPE,XCHCOMNT,date1,datem,xchfiller,LNAME:
                    COMPCOMP,LogoFlag
         GOTO      EOJ2 IF OVER
. MOVE MAILER NUMBERS TO WORK FIELDS
         add       c1 to recsin
         display   *p15:09,recsin
         call      rotdial
         PACK      DATE FROM PMO,SLASH,PDAY,SLASH,PYR
         UNPACK    EXKEY INTO MLR1,MLR2,SENTRY
         MOVE      SENTRY TO ENTRY
         CMATCH    YES,STOPLOOP
         CALL      LOADLN IF EQUAL
         MATCH     "    ",MLR1
         GOTO      PRT1D IF EQUAL
         MATCH     "    ",MLR2
         GOTO      PRT1D IF EQUAL
. ............................................................................
.begin patch 4.2
.details
Break1
	if        (row >= 8000)
	call	OH
	endif
.BREAK1   COMPARE   "50",LINES
.         CALL      NEWPAGE IF NOT LESS
.         call       oh if not less
.
..         COMPARE   C0 TO ENTRY
..         CALL      newpage IF EQUAL
..         CALL      OH IF EQUAL
.         COMPARE   "50",LINES
..         CALL      NEWPAGE IF NOT LESS
.         CALL      oh IF NOT LESS
.need to figure this one out
.         COMPARE   C0 TO LINES
.         CALL      newpage IF EQUAL
.end patch 4.2
.         CALL      OH IF EQUAL
.
         COMPARE   C0,ENTRY
         CALL     BEGBAL IF EQUAL
         CALL      DETAIL IF NOT EQUAL
.
.begin patch 4.2
.really again?
	if        (row >= 8000)
	call	OH
	endif
.         COMPARE   "50",LINES
..         CALL      NEWPAGE IF NOT LESS
.         call      oh if not less
.end patch 4.2
.         COMPARE   C0 TO LINES
.         CALL      newpage IF EQUAL
.
         GOTO      PRT1D
.
. ............................................................................
* THIS STOPS A BREAK FROM HAPPENING AFTER THE BEGINNING BALANCE IS PRINTED
. NORMAL BREAKS ARE STARTED AFTER THE FIRST DETAIL RECORD.
LOADLN
         MOVE      LIST,LN
         MOVE      "N",STOPLOOP
         CALL      LHEAD
         RETURN
. ............................................................................
* BEGINNING BALANCE
BEGBAL
         MOVE      YES,STOPLOOP
          pack      COMPFLD,MLR1
          move      "BEGBAL-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    call      NOMAILER
          elseif (COMPMLRFLG <> "T")
                    call      NOMAILER
          endif
          move      COMPCOMP,MDES1
          move      MDES1,MLRDES1
.
          pack      COMPFLD,MLR2
          move      "BEGBAL,2-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    call      NOMAILER
          elseif (COMPMLRFLG <> "T")
                    call      NOMAILER
          endif
          move      COMPCOMP,MLRDES2
.
         move      mask to maska
         edit      usage1 to maska
          if        (excelFlag = c1)
.begin patch 4.2
          move      "500",row
          PRTPAGE   Laser;*p=250:row,"BEGINNING BALANCE ## ",MLR1,*p=5000:row,"OPENING":
                    *p=8000:row,"BEGINNING BALANCE ## ",MLR2
          add       sixlpi,row
          PRTPAGE   Laser;*p=250:row,*ULON,MDES1,*p=5000:row," DATE  ":
                    *p=8000:row,COMPCOMP,*ULOFF
          add       sixlpi,row
          PRTPAGE   Laser;*p=250:row,MaskA,"  NAMES",*p=5000:row,DATE
          
.         PRINT     *L,*L,*L,*10,"BEGINNING BALANCE ## ",MLR1,hpt475,"OPENING":
.                  hpt800,"BEGINNING":
.                   " BALANCE ## ",MLR2:
.                   *L,*10,hpunon,MDES1,HPUNOff,hpt475,HPUNON," DATE  ":
.                   HPUNOff,hpt800,HPUNON,COMPCOMP:
.                   HPUNOFF:
.                   *L,*10,maska,"  NAMES",hpt475,DATE;
.end patch 4.2
          Elseif   (excelFlag = c2)
                    Add       C3,Row1
                    Move            Row1,DimRow1
                    call            Trim using DimRow1                 
                    pack            Exrange1 from "B",DimRow1
                    pack      taskname from "BEGINNING BALANCE ## ",MLR1
                    setprop sheet.range(ExRange1,ExRange1),*Value=taskname

                    Move            Row1,DimRow1
                    call            Trim using DimRow1                 
                    pack            Exrange1 from "F",DimRow1
                    setprop sheet.range(ExRange1,ExRange1),*Value="OPENING",*HorizontalAlignment=XLAlignCenter

                    Move            Row1,DimRow1
                    call            Trim using DimRow1                 
                    pack            Exrange1 from "I",DimRow1
                    pack      taskname from "BEGINNING BALANCE ## ",MLR2
                    setprop sheet.range(ExRange1,ExRange1),*Value=taskname

                    Add       C1,Row1
                    Move            Row1,DimRow1
                    call            Trim using DimRow1                 
                    pack            Exrange1 from "B",DimRow1
                    setprop sheet.range(ExRange1,ExRange1),*Value=MDES1
                    setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
                    setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          
                    Move            Row1,DimRow1
                    call            Trim using DimRow1                 
                    pack            Exrange1 from "F",DimRow1
                    setprop sheet.range(ExRange1,ExRange1),*Value="Date",*HorizontalAlignment=XLAlignCenter
                    setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle

                    Move            Row1,DimRow1
                    call            Trim using DimRow1                 
                    pack            Exrange1 from "I",DimRow1
                    setprop sheet.range(ExRange1,ExRange1),*Value=COMPCOMP
                    setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
                    setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle

                    Add       C1,Row1
                    Move            Row1,DimRow1
                    call            Trim using DimRow1                 
                    pack            Exrange1 from "B",DimRow1
                    setprop sheet.range(ExRange1,ExRange1),*Value=MaskA,*HorizontalAlignment=XLAlignCenter

                    Move            Row1,DimRow1
                    call            Trim using DimRow1                 
                    pack            Exrange1 from "F",DimRow1
                    setprop sheet.range(ExRange1,ExRange1),*Value=Date,*HorizontalAlignment=XLAlignCenter

          endif
         move      mask to maska
         edit      usage2 to maska
          if        (excelFlag = c1)
.Begin patch 4.2
          PrtPage   Laser;*p=8000:row,MaskA,"  NAMES"
          add       sixlpi,row
          add       sixlpi,row
          add       sixlpi,row
          add       sixlpi,row
          add       sixlpi,row
.         print     hpt800,maska,"  NAMES":
.                   *L,*L,*L,*L,*L
.end patch 4.2
.
          Elseif    (excelFlag = c2)
                    Move      Row1,DimRow1
                    call      Trim using DimRow1                 
                    pack      Exrange1 from "I",DimRow1
                    setprop sheet.range(ExRange1,ExRange1),*Value=MaskA,*HorizontalAlignment=XLAlignCenter

          endif
         ADD       USAGE1,GRNDTOT1
         ADD       USAGE2,GRNDTOT2
.
.
         move     c1 to holdsw
         NORETURN
         GOTO      PRT1D
. .............................................................................
* LIST TOTALS
LTOT
.Begin patch 4.2
          if        (excelFlag = c1)
         if        (row >= 7000)
         call       Newpage
         endif
         add        Sixlpi,row
         add        Sixlpi,row
         add        Sixlpi,row
         prtpage   Laser;*p=10:row,"LIST *"
         add        Sixlpi,row
         prtpage   Laser;*p=10:row,"Total*"
         add        Sixlpi,row
         prtpage   Laser;*p=10:row,SUBTOT1,"   ","NAMES"
         add        Sixlpi,row
         add        Sixlpi,row
         add        Sixlpi,row
          
.         COMPARE   "50",LINES
.         CALL      NEWPAGE IF NOT LESS
.         PRINT *L,*L,*L,*3,"LIST *":
.               *L,*3,"TOTAL*",*19,"--------":
.                   *L,*16,SUBTOT1,"   ","NAMES"
.
         MOVE      C0 TO SUBTOT1
.         ADD       C3,LINES
.end patch 4.2
          Else
          endif
        RETURN
.
. .............................................................................
* GRAND TOTAL
OTOT
         call      subtprt2
.begin patch 4.2
	if        (row >= 7000)
	call	OH
	endif
.         COMPARE   "50",LINES
..         CALL      NEWPAGE IF NOT LESS
.         call       oh if not less
.end patch 4.2
.
         move      mask to maska
         edit      grndtot1 to maska
          if        (excelFlag = c1)
.begin patch 4.2
         if        (row >= 7000)
         call       Newpage
         endif
         add        Sixlpi,row
         add        Sixlpi,row
         add        Sixlpi,row
         prtpage   Laser;*p=1000:row," MAILER **  ",MLRDES1,*p=6000:row," MAILER **  ",MLRDES2
         add        Sixlpi,row
         prtpage   Laser;*p=1000:row," Total Usage **  ",maska
         move      mask to maska
         edit      grndtot2 to maska
         prtpage   Laser;*p=6000:row," Total Usage ** ",maska
.         PRINT   *L,*L,*L,*10," MAILER **  ",MLRDES1,B1:
.                hpt550," MAILER **  ",MLRDES2:
.                  *L,*10," Total Usage **  ",maska;
..                   hpt800," TOTAL USAGE ** ",":
..                   *L,*28,maska;
.         move      mask to maska
.         edit      grndtot2 to maska
..         print    *76,maska
.         print    hpt550," Total Usage ** ",maska
.end patch 4.2
          Elseif    (excelFlag = c2)

          add       c1,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "B",DimRow1
          pack      taskname from " MAILER **  ",MLRDES1
          setprop sheet.range(ExRange1,ExRange1),*Value=TaskName
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "H",DimRow1
          pack      taskname from " MAILER **  ",MLRDES2
          setprop sheet.range(ExRange1,ExRange1),*Value=TaskName
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"

          add       c1,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "B",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=" Total Usage **  "
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "C",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=MaskA
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"

          move      mask to maska
          edit      grndtot2 to maska

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "H",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=" Total Usage **  "
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "I",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=MaskA
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          endif
   
.begin patch 4.2
.         ADD       C4 TO LINES
.         COMPARE   "50",LINES
.         CALL      NEWPAGE IF NOT LESS
.end patch 4.2

. CALC FOR BAL.
         COMPARE   GRNDTOT1 TO GRNDTOT2
         GOTO      OTOTE IF EQUAL
         GOTO      OTOTL IF LESS
         GOTO      OTOTO
.
OTOTE   
          if        (excelFlag = c1)
         if        (row >= 7500)
         call       Newpage
         endif
         add        Sixlpi,row
         add        Sixlpi,row
         add        Sixlpi,row
         prtpage   Laser;*p=1000:row," MAILER **  ",MLRDES1,*p=4500:row,"AND",*p=6000:row," MAILER **  ",MLRDES2," ARE EVEN."
.          PRINT     *L,*L,*L,*25,*RPTCHAR "_":60:
.                   *L,*40,MLRDES1," AND ",MLRDES2," ARE EVEN"
          Elseif   (excelFlag = c2)
          add       c2,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "D",DimRow1
          pack      taskname from MLRDES1," AND ",MLRDES2," ARE EVEN"
          setprop sheet.range(ExRange1,ExRange1),*Value=Taskname
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          endif          
         add       c5 to lines
         GOTO      OTEXIT
.
OTOTL    SUBTRACT  GRNDTOT2 FROM GRNDTOT1
         MOVE      MASK TO MASKA
         EDIT      GRNDTOT1 TO MASKA

          if        (excelFlag = c1)
         if        (row >= 7500)
         call       Newpage
         endif
         add        Sixlpi,row
         add        Sixlpi,row
         add        Sixlpi,row
         prtpage   Laser;*p=1000:row,MLRDES1,*p=4500:row,"OWES",*p=6000:row,MLRDES2,MASKA," NAMES"
.         PRINT     *L,*L,*L:
.                   *L,*20,MLRDES1,HPBON," OWES ",HPBOFF,MLRDES2,B1,HPBON,MASKA,HPBOFF," NAMES"

          Elseif   (excelFlag = c2)
          add       c2,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "D",DimRow1
          pack      taskname from MLRDES1," OWES ",MLRDES2,B1,MASKA," NAMES"
          setprop sheet.range(ExRange1,ExRange1),*Value=Taskname
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          endif          
         add       c5 to lines
         GOTO      OTEXIT
.
OTOTO    SUBTRACT  GRNDTOT1 FROM GRNDTOT2
         MOVE      MASK TO MASKA
         EDIT      GRNDTOT2 TO MASKA
          if        (excelFlag = c1)
         if        (row >= 7500)
         call       Newpage
         endif
         add        Sixlpi,row
         add        Sixlpi,row
         add        Sixlpi,row
         prtpage   Laser;*p=1000:row,MLRDES2,*p=4500:row,"OWES",*p=6000:row,MLRDES1,MASKA," NAMES"
.         PRINT     *L,*L,*L:
.                   *L,*20,MLRDES2,HPBON," OWES ",HPBOFF,MLRDES1,B1,HPBON,MASKA,HPBOFF," NAMES"
          Elseif   (excelFlag = c2)
          add       c2,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "D",DimRow1
          pack      taskname from MLRDES2," OWES ",MLRDES1,B1,MASKA," NAMES"
          setprop sheet.range(ExRange1,ExRange1),*Value=Taskname
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          endif          
         add       c5 to lines
         GOTO      OTEXIT
.
OTEXIT
         CALL      PAGENUM
          if        (excelFlag = c1)
.begin patch 4.2
.         PRINT     *FLUSH
.end patch 4.2
          endif          
         MOVE      C0,GRNDTOT1
         MOVE      C0,GRNDTOT2
.
         ADD       C4,LINES
.
         RETURN
.
. .............................................................................
* DETAIL HEADING
OH
         COMPARE   C0 TO PAGE
         IF        EQUAL
         ADD       C1 TO PAGE
                   ELSE
         CALL      PAGENUM
         ENDIF
         MOVE      LIST,LN
         compare   c1 to page
         if        equal
                    if        (LogoFlag = "P")
                              if        (excelFlag = c1)
                              MOVe      "20",row
                              prtpage   Laser;*UNITS=*HIENGLISH:
                                        *ORIENT=*LANDSCAPE,*Font=Font1:
                                        *p=1:row,*BOLDON,"CONFIDENTIAL",*p=9500:row,TODAY:
                                        *p=3250:row,"*** PL EXCHANGE STATUS REPORT ***":
                                        *BOLDOFF
                             add        Sixlpi,row
                             add        Sixlpi,row
                             add        Sixlpi,row
.                              PRINT     *L,*1,hp12ptch,"CONFIDENTIAL",hpt950,TODAY:
.                                        hpt325,"*** PL EXCHANGE STATUS REPORT ***":
.                                        hp14ptch
                              Elseif   (excelFlag = c2)
                              add       c1,Row1
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "A",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value="CONFIDENTIAL"
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "J",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value=TODAY
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "D",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value="*** PL EXCHANGE STATUS REPORT ***"
                              endif          
                    Else
                              if        (excelFlag = c1)
                              MOVe      "20",row
                              prtpage   Laser;*UNITS=*HIENGLISH:
                                        *ORIENT=*LANDSCAPE,*Font=Font1:
                                        *p=1:row,*BOLDON,"CONFIDENTIAL",*p=9500:row,TODAY:
                                        *p=4050:row,"*** NIN EXCHANGE STATUS REPORT ***":
                                        *Boldoff
                             add        Sixlpi,row
                             add        Sixlpi,row
                             add        Sixlpi,row
.                              PRINT     *L,*1,hp12ptch,"CONFIDENTIAL",hpt950,TODAY:
.                                        hpt325,"*** NIN EXCHANGE STATUS REPORT ***":
.                                        hp14ptch

                              Elseif   (excelFlag = c2 & Page = c1)
                              add       c1,Row1
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "A",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value="CONFIDENTIAL"
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "J",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value=TODAY
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "D",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value="*** NIN EXCHANGE STATUS REPORT ***"
                              endif          
                    endif
         else
                    if        (LogoFlag = "P")
                              if        (excelFlag = c1)
                              MOVe      "20",row
                              prtpage   Laser;*UNITS=*HIENGLISH:
                                        *ORIENT=*LANDSCAPE,*Font=Font1:
                                        *p=1:row,*BOLDON,"CONFIDENTIAL",*p=9500:row,TODAY:                                        
                                        *p=4050:row,"*** PL EXCHANGE STATUS REPORT ***":
                                        *BOLDOFF
                             add        Sixlpi,row
                             add        Sixlpi,row
                             add        Sixlpi,row
.                              PRINT     *f,hpland,*l,*L,*1,hp10ptch,"CONFIDENTIAL",hpt950,TODAY:
.                                        hpt325,"*** PL EXCHANGE STATUS REPORT ***":
.                                        hp14ptch

                              Elseif   (excelFlag = c2 & Page = c1)
                              add       c1,Row1
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "A",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value="CONFIDENTIAL"
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "J",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value=TODAY
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "D",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value="*** PL EXCHANGE STATUS REPORT ***"
                              endif          
                    Else
                              if        (excelFlag = c1)
                              MOVe      "20",row
                              prtpage   Laser;*UNITS=*HIENGLISH:
                                        *ORIENT=*LANDSCAPE,*Font=Font1:
                                        *p=1:row,*BOLDON,"CONFIDENTIAL",*p=9500:row,TODAY:
                                        *p=4050:row,"*** NIN EXCHANGE STATUS REPORT ***":
                                        *Boldoff
                             add        Sixlpi,row
                             add        Sixlpi,row
                             add        Sixlpi,row
                    
.                              PRINT     *f,hpland,*l,*L,*1,hp10ptch,"CONFIDENTIAL",hpt950,TODAY:
.                                        hpt325,"*** NIN EXCHANGE STATUS REPORT ***":
.                                        hp14ptch
                              Elseif   (excelFlag = c2 & Page = c1)
                              add       c1,Row1
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "A",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value="CONFIDENTIAL"
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "J",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value=TODAY
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "D",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value="*** NIN EXCHANGE STATUS REPORT ***"
                              endif          
                   endif
         endif
         COMPARE   C0 TO ENTRY
         RETURN    IF EQUAL
...............................................................................
* DETAIL HEADING
LHEAD
          if        (excelFlag = c1)
          prtpage   Laser;*p=1:row,*ULON,"LR / PO ",*p=775:row,"  DATE  ":
                    *p=1500:row,"        MAILER           ",*p=3500:row,"QUANTITY ":
                    *p=4250:row," STATUS  ",*p=5400:row,"LIST DESCRIPTION                   ":
                    *p=7750:row,"Maildate",*p=8500:row,"COMMENTS                  ",*UlOff,*BoldOff
.                    *p=4250:row," STATUS  ",*p=5900:row,"LIST DESCRIPTION                   ":
.                    *p=8250:row,"Maildate",*p=9000:row,"COMMENTS         ",*UlOff,*BoldOff
          add       sixlpi,row
          add       sixlpi,row



          Elseif   (excelFlag = c2 & Page = c1)

          add       c4,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="LR / PO "
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "B",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="DATE",*HorizontalAlignment=XLAlignCenter
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "C",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="MAILER",*HorizontalAlignment=XLAlignCenter
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "E",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Quantity",*HorizontalAlignment=XLAlignCenter
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "F",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Status",*HorizontalAlignment=XLAlignCenter
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "H",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="LIST DESCRIPTION"
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "I",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Maildate",*HorizontalAlignment=XLAlignCenter
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "J",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Comments"
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle

          endif          
 
         RETURN
...............................................................................
DETAIL   cmatch    mlrsw to holdsw
         call      subtprt if not equal
         CMATCH    "1",MLRSW
         GOTO      DETAILA IF EQUAL
         CMATCH    "2",MLRSW
         GOTO      DETAILB IF EQUAL
.
DETAILA
         PACK      COMPFLD,MLR1
         CMATCH    "C",STAT                          ORDER IS CANCELLED
         GOTO      PRCANCEL IF EQUAL
         CMATCH    "R",STAT                          ORDER CHANGED TO RENTAL
         GOTO      PRRENT IF EQUAL
         ADD       QTY TO SUBTOT1
         ADD       QTY TO GRNDTOT1
         CLEAR     STATMSG
         CMATCH    "X" TO STAT
         GOTO      PRCANCL1 IF EQUAL                 ORDER CANCELLED
.                                                    AFTER MAILDATE
         GOTO      DETAILC
.
DETAILB
         PACK      COMPFLD,MLR2
         CMATCH    "C",STAT                          ORDER IS CANCELLED
         GOTO      PRCANCEL IF EQUAL
         CMATCH    "R",STAT                          ORDER CHANGED TO RENTAL
         GOTO      PRRENT IF EQUAL
         ADD       QTY TO SUBTOT1
         ADD       QTY TO GRNDTOT2
         CLEAR     STATMSG
         CMATCH    "X" TO STAT
         GOTO      PRCANCL1 IF EQUAL                 ORDER CANCELLED
.                                                    AFTER MAILDATE
         GOTO      DETAILC
.
subtprt  move      mlrsw to holdsw
         move      mask to maska
         edit      grndtot1 to maska
          if        (excelFlag = c1)
         	if        (row >= 8000)
		call	OH
.         	call       Newpage
         	endif
          add       sixlpi,row
          prtpage   Laser;*p=100:row,*ULOn,"Mailer total : ",*p=4500:row,maska,*ULoff
          add       sixlpi,row

          Elseif     (excelFlag = c2)

          add       c1,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "C",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Mailer total : "
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "E",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=MaskA
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          add       c1,Row1

          endif          
          add       c2 to lines
         return
subtprt2
         move      mask to maska
         edit      grndtot2 to maska
          if        (excelFlag = c1)
         if        (row >= 7000)
	 call	OH
.         call       Newpage
         endif
          add       sixlpi,row
          prtpage   Laser;*p=100:row,*ULOn,"Mailer total : ",*p=4500:row,maska,*ULoff
          add       sixlpi,row
          Elseif     (excelFlag = c2)
          add       c1,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "C",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Mailer total : "
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "E",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=MaskA
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          add       c1,Row1

          endif          
         add       c2 to lines
         return
.
PRCANCEL
         MOVE      "*CANCELLED*",STATMSG
         GOTO      DETAILC
PRRENT
         MOVE      "*RENTAL*",STATMSG
         GOTO      DETAILC
PRCANCL1 MOVE      "CANC/NO ADJ",STATMSG
         GOTO      DETAILC
DETAILC
	if	(row >= 7000)
	call	OH
	endif
          move      "DETAILC-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    call      NOMAILER
          elseif (COMPMLRFLG <> "T")
                    call      NOMAILER
          endif
          move      COMPCOMP,MDES1
         move      lr to nordfld
         move      c1 to nordpath
         clear     str10
         unpack    date into mm,str1,dd,str1,yy
         call      cvtjul
         move      juldays to n6
         move      "01" to mm
         move      "01" to dd
         move      "88" to yy
         call      cvtjul
         compare   juldays to n6
         if       not less
         call      nordkey
         if        not over
         pack      str10 from OMDTEM,slash,OMDTEd,slash,OMDTEC,OMDTEy
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
         endif
         endif
         MOVE      QTYMASK TO QTYOUT
         EDIT      QTY TO QTYOUT
          if        (excelFlag = c1)
         if        (row >= 7000)
	call	OH
.         call       Newpage
         endif
          add       sixlpi,row
          prtpage   Laser;*p=1:row,LR,*p=775:row,DATE:
                    *p=1500:row,Mdes1,*p=3750:row,QTYOut:
                    *p=4500:row,StatMsg,*p=5400:row,Lname:
                    *p=7750:row,Str10,*p=8500:row,XCHCOMnt
.                    *p=4500:row,StatMsg,*p=5900:row,Lname:
.                    *p=8250:row,Str10,*p=9000:row,XCHCOMnt
          add       sixlpi,row

          Elseif     (excelFlag = c2)
          add       c1,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=LR,*HorizontalAlignment=XlAlignLeft   

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "B",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=DATE,*HorizontalAlignment=XLAlignCenter

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "C",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=Mdes1

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "E",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=QTYOUT

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "F",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=STATMSG

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "H",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=LNAME

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "I",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=STR10,*HorizontalAlignment=XLAlignCenter

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "J",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=XCHCOMNT

          endif          
.
         ADD       C1,LINES
         compare   c2 to detflag
         if        equal
         clear     oodes
         clear     o2des
         call      nordkey
         if        not over
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
          if        (excelFlag = c1)
	         if        (row >= 7000)
		call	OH
.	         call       Newpage
 	        endif
          prtpage   Laser;*p=10:row,OMlrpon,*p=1500:row,OFDESC:
                    *p=5400:row,NSEL2Name
.                    *p=5900:row,NSEL2Name
          add       sixlpi,row

          Elseif     (excelFlag = c2)
          add       c1,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=omlrpon,*HorizontalAlignment=XLAlignRight

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "C",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=ofdesc

          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "H",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=NSEL2NAME
          endif          

.         ADD       C1,LINES
         endif
         endif
         RETURN
...............................................................................
...............................................................................
NOFILE   DISPLAY   *B,*P01:23,*EL,NOFILE," FILE IS NOT ON LINE NOTIFY YOUR ":
                   "PROGRAMER !!!!!",*W,*B,*W,*B,*W5;
         shutdown  "CLS"
         STOP
...............................................................................
NOMAILER
           MOVE      "NO MAILER FOUND",COMPCOMP
         RETURN
...............................................................................
EOJ2
         CALL      OTOT
         BEEP
         GOTO      ENDPRT
+..............................................................................
. NODETAIL - TOTALS REPORT.
NODETAIL
...............................................................................
PRT11
          pack      COMPFLD,TEMPMLR1
          move      "PRT11-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    call      NOMAILER
          elseif (COMPMLRFLG <> "T")
                    call      NOMAILER
          endif
          move      COMPCOMP,MDES1
         CALL      OH1
PRT12    call      rotdial
          READ    INPUT,SEQ;MLR1,MLR2,ENTRY,LR2,LR,USAGE1,USAGE2:
                   QTYFILL,QTY,LIST,cc,pyr,PMO,PDAY,STAT,MLRSW,TYPE,XCHCOMNT,date1,Datem,xchfiller,LNAME:
                   COMPCOMP,logoflag
         GOTO      EOJ22 IF OVER
         MATCH     "    ",MLR1
         GOTO      PRT12 IF EQUAL
         MATCH     "    ",MLR2
         GOTO      PRT12 IF EQUAL
         MATCH     "       " TO COMPCOMP
         GOTO      PRT12 IF EQUAL        *SUPPRESS BLANK PRINT OF 1ST REC.
         add       c1 to recsin
         display   *p15:09,recsin
         move      c1 to umbrflag
         compare   c2 to univflag        *universe wanted ?
         if        equal                 *yes
         clear     nxrffld2
         clear     universe
         move       c0 to n9
         move       c0 to n10
         MATCH     TEMPMLR1,MLR1
            IF        EQUAL
              move      mlr2 to nxrffld2
            else
              move      mlr1 to nxrffld2
            endif
         rep       zfill in nxrffld2
         move      c1 to umbrflag
         call      nxrfkey
            if        not over
            clear     ndatfld
            clear     universe
            move       c0 to n9
            move      nxrflist to ndatfld
            move      c1 to ndatpath
            rep       zfill in ndatfld
            call      ndatkey
            goto      uniexit if over
            CMATCH     b1  TO status
            goto      moreuni if not equal
            move      c0 to n11
                                        call trim using universe
                                        call zfillit using universe
            move      universe to n11
            add       n11 to n10
moreuni         call      nxrfks
                if        not over                .an umbrella
                  match     nxrffld2 to nxrfmlr
                  goto      uniexit if not equal
                  move      c1 to ndatpath
                  move      nxrflist to ndatfld
                  rep       zfill in ndatfld
                  clear     universe
                  call      ndatkey
                  CMATCH     b1  TO status
                  goto      moreuni if not equal
                  move      c0 to n11
                                                            call trim using universe
                                                            call zfillit using universe
                  move      universe to n11
                  add       n11 to n10
                  goto     moreuni
                endif
            else
            display      *p1:24,*el,*red,*dion,"NO X-REF",*dioff,*white:
                         *b,*p1:24,*el;
            endif
         endif
uniexit
         PACK      DATE FROM PMO,SLASH,PDAY,SLASH,PYR
         GOTO      BREAK11
.
. ............................................................................
BREAK11  
         if        (row >= 10000)
	 call       OH1
	 endif

         CALL      LTOT1
         if        (row >= 10000)
	 call       OH1
	 endif

         GOTO      PRT12
. ............................................................................
* LIST TOTALS
LTOT1
         MATCH     TEMPMLR1,MLR1
         GOTO      LTOT1A IF EQUAL
         MATCH     TEMPMLR1,MLR2
         GOTO      LTOT1B IF EQUAL
         RETURN

.
LTOT1A
         MOVE      PICSW TO NUM
         BRANCH    NUM OF LTOT11P,LTOT11R,LTOT11E,LTOTALL
. LTOT11P-PAYABLES REPORT. LTOT11R-RECIEVABLES REPORT
.
LTOT1B
         MOVE      PICSW TO NUM
         BRANCH    NUM OF LTOT12P,LTOT12R,LTOT11E,LTOTALL
. LTOT12P-PAYABLES REPORT. LTOT12R-RECIEVABLES REPORT
.
LTOT11P  SUBTRACT  USAGE2 FROM USAGE1
         MOVE      MASK TO MASKA
         EDIT      USAGE1 TO MASKA
.
         ADD       USAGE1 TO TOTAL           *FOR FINAL TOTALS.
        RETURN
.
LTOT11R  SUBTRACT  USAGE1 FROM USAGE2
         MOVE      MASK TO MASKA
         EDIT      USAGE2 TO MASKA
         ADD       USAGE2 TO TOTAL           *FOR FINAL TOTALS.
        RETURN
.
LTOT11E
	add       sixlpi,row
	 prtpage   Laser;*p=11:row,compcomp
         RETURN
.
.
LTOT12P  SUBTRACT  USAGE1 FROM USAGE2
         MOVE      MASK TO MASKA
         EDIT      USAGE2 TO MASKA
.
         ADD       USAGE2 TO TOTAL           *FOR FINAL TOTALS.
        RETURN
.
LTOT12R  SUBTRACT  USAGE2 FROM USAGE1
         MOVE      MASK TO MASKA
         EDIT      USAGE1 TO MASKA
         ADD       USAGE1 TO TOTAL           *FOR FINAL TOTALS.
        RETURN
.
LTOTALL
         clear     str7
         compare   c2 to listflag                .lst numbers requested?
         if        equal                         .yes
         clear     nxrffld2
         MATCH     TEMPMLR1,MLR1
            IF        EQUAL
            move      mlr2 to nxrffld2
            else
            move      mlr1 to nxrffld2
            endif
         call      nxrfkey
            if        not over
          compare  c2 to umbrflag
          goto     cont if equal

                clear     str7
                append    "##" to str7
                append    nxrflist to str7
                reset      str7
            endif
         endif
CONT
         cmatch YES,NEVERUFLAG  ;if want never been used option
         if equal     .yes
                    pack      NORDFLD1,"01X",COMPOLDMLR
                  clear NORDFLD3
                  clear NORDFLD4
                  move C2,NXRFPATH
                  clear NXRFFLD
                  MATCH     TEMPMLR1,MLR1
                  IF        EQUAL
                        move      mlr2 to nxrffld2
                  else
                        move      mlr1 to nxrffld2
                  endif
                  call NXRFKEY
                  loop
                       until over
                       until (NXRFFLD2 <> NXRFMLR)
                       pack NORDFLD2,"02X",NXRFLIST
                       call NORDLAST
                       goto overread if over
                       loop
                            if (OSTAT = "0" | OSTAT = "B")
                                move YES to usedflag
                      goto cont2
                            else
                                move NO to usedflag
                            endif
OverRead
                            move NO to usedflag
                            call NORDKGP
                            until over
                            until (OMLRNUM <> COMPOLDMLR)
                       repeat
                       call NXRFKS
                   until over
                   repeat
         endif
CONT2
                  cmatch YES,REVVFLAG ;if want never been used option
                  if equal
               move  C2,NXRFPATH
               clear NXRFFLD
                         MATCH     TEMPMLR1,MLR1
                         IF        EQUAL
                               move      mlr2 to nxrffld2
                         else
                               move      mlr1 to nxrffld2
                         endif
                         call  NXRFKEY
                         if over
                     move "00/00/0000" to str10
                              else
                     call zfillit using NXRFLIST
                               move c1 to ndatpath
                     clear ndatfld
                     move nxrflist to ndatfld
                     call ndatkey
                               if over
                            pack REVISIONDATE with "REV: ","00/00/0000"
                     else
                              unpack    REVDATE,CC,YY,MM,DD
                              pack      str10,MM,SLASH,DD,SLASH,CC,YY
                            pack REVISIONDATE with "REV: ",str10
                     endif
               endif
                  else
               clear REVISIONDATE
                  endif
getout
          ADD       C2 TO LINES

.check for even usage
          COMPARE   USAGE1 TO USAGE2
          IF        EQUAL
                    MOVE      MASK TO MASKA
                    EDIT      C0 TO MASKA
.excel or print?
                    IF        (excelFlag = c1)
         		if        (row >= 10000)
         		call       OH1
         		endif
                    add       sixlpi,row
.                    prtpage   Laser;*p=2:row,Str7,*p=500:row,compcomp,*p=7000:row,"X"
                    prtpage   Laser;*p=2:row,Str7,*p=500:row,compcomp,*p=7750:row,"X"
.                    add       sixlpi,row

                    Elseif    (excelFlag = c2)

                    Add       C1,Row1
                    Move            Row1,DimRow1
                    call            Trim using DimRow1                 
                    pack            Exrange1 from "A",DimRow1
                    setprop sheet.range(ExRange1,ExRange1),*Value=COMPCOMP


                    Move            Row1,DimRow1
                    call            Trim using DimRow1                 
                    pack            Exrange1 from "D",DimRow1
                    setprop sheet.range(ExRange1,ExRange1),*Value="X",*HorizontalAlignment=XLAlignCenter

                    endif
                    goto      ltotallx
          ENDIF

          MATCH     TEMPMLR1,MLR1
          IF        EQUAL
                    SUBTRACT  USAGE1 FROM USAGE2
                    MOVE      MASK TO MASKA
                    EDIT      USAGE2 TO MASKA
                    COMPARE   C0 TO USAGE2
                    IF        LESS
                              IF        (excelFlag = c1)
         			if        (row >= 10000)
         			call       OH1
         			endif
                              add       sixlpi,row
                              prtpage   Laser;*p=2:row,Str7,*p=500:row,compcomp,*alignment=*Right,*p=5500:row,MaskA,*alignment=*Left
.                    	      add       sixlpi,row
                    
                    Elseif    (excelFlag = c2)

                              Add       C1,Row1
                              Move            Row1,DimRow1
                              call            Trim using DimRow1                 
                              pack            Exrange1 from "A",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value=COMPCOMP


                              Move            Row1,DimRow1
                              call            Trim using DimRow1                 
                              pack            Exrange1 from "B",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value=MaskA

                    endif
                              ADD       USAGE2 TO GRNDTOT1
                    ELSE
                              if (usedflag = NO)
                                        if (usage1 > c0)
.if have no usage in exchange file and have nothing in our order file   bold
                                                  IF        (excelFlag = c1)
         if        (row >= 10000)
         call       OH1
         endif
                                                  add       sixlpi,row
                                                  prtpage   Laser;*p=2:row,Str7,*p=500:row,compcomp,*alignment=*Right,*p=6750:row,MaskA,*alignment=*Left
.                                                  add       sixlpi,row
                    
                                        Elseif    (excelFlag = c2)

                                                  Add       C1,Row1
                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "A",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=COMPCOMP


                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "C",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=MaskA

                                                  endif
                                        else
                                                  IF        (excelFlag = c1)
         						if        (row >= 10000)
         						call       OH1
         						endif
                                                  add       sixlpi,row
                                                  prtpage   Laser;*p=2:row,Str7,*p=500:row,compcomp,*alignment=*Right,*p=6750:row,MaskA,*alignment=*Left
                    
                                        Elseif    (excelFlag = c2)

                                                  Add       C1,Row1
                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "A",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=COMPCOMP


                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "C",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=MaskA

                                                  endif
                                        endif
                              else
                                        IF        (excelFlag = c1)
					         if        (row >= 10000)
					         call      OH1
					         endif
                                                 add       sixlpi,row
                                                  prtpage   Laser;*p=2:row,Str7,*p=500:row,compcomp,*alignment=*Right,*p=6750:row,MaskA,*alignment=*Left
                    
                                        Elseif    (excelFlag = c2)

                                                  Add       C1,Row1
                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "A",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=COMPCOMP


                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "C",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=MaskA

                                                  endif
                              endif

                              ADD       USAGE2 TO GRNDTOT2
                    ENDIF
          ENDIF

          MATCH     TEMPMLR1,MLR2
          IF        EQUAL
                    SUB       USAGE2 FROM USAGE1
                    MOVE      MASK TO MASKA
                    EDIT      USAGE1 TO MASKA
                    COMPARE   C0 TO USAGE1
                    IF        LESS
                              IF        (excelFlag = c1)
			         if        (row >= 10000)
			         call       OH1
			         endif
                              add       sixlpi,row
                              prtpage   Laser;*p=2:row,Str7,*p=500:row,compcomp,*alignment=*Right,*p=5500:row,MaskA,*alignment=*Left
                    
                                        Elseif    (excelFlag = c2)

                                                  Add       C1,Row1
                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "A",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=COMPCOMP


                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "B",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=MaskA

                                                  endif
                              ADD       USAGE1 TO GRNDTOT1
                    ELSE
                              if (usedflag = NO)
.if have no usage in exchange file and have nothing in our order file bold
                                        if (usage2 > c0)
                                                  IF        (excelFlag = c1)
						         if        (row >= 10000)
						         call       OH1
						         endif
                                                 add       sixlpi,row
                                                  prtpage   Laser;*p=2:row,Str7,*p=500:row,compcomp,*alignment=*Right,*p=6750:row,MaskA,*alignment=*Left
.                                                  PRINT     *L,*2,str7,*09,hp12ptch,COMPCOMP,hp10ptch,*53,MASKA;
                    
                                        Elseif    (excelFlag = c2)

                                                  Add       C1,Row1
                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "A",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=COMPCOMP


                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "c",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=MaskA

                                                  endif
                                        else
                                                  IF        (excelFlag = c1)
					         if        (row >= 10000)
					         call       OH1
					         endif
                                                 add       sixlpi,row
                                                  prtpage   Laser;*p=2:row,Str7,*p=500:row,compcomp,*alignment=*Right,*p=6750:row,MaskA,*alignment=*Left
.                                                  PRINT     *L,*2,str7,*09,hp12ptch,HPBON,COMPCOMP,HPBOFF,hp10ptch,*53,MASKA;  .Yes
                    
                                        Elseif    (excelFlag = c2)

                                                  Add       C1,Row1
                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "A",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=COMPCOMP


                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "c",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=MaskA

                                                  endif
                                        endif
                              else
                                                  IF        (excelFlag = c1)
						         if        (row >= 10000)
						         call       OH1
						         endif
                                                 add       sixlpi,row
                                                  prtpage   Laser;*p=2:row,Str7,*p=500:row,compcomp,*alignment=*Right,*p=6750:row,MaskA,*alignment=*Left
                    
                                        Elseif    (excelFlag = c2)

                                                  Add       C1,Row1
                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "A",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=COMPCOMP


                                                  Move            Row1,DimRow1
                                                  call            Trim using DimRow1                 
                                                  pack            Exrange1 from "c",DimRow1
                                                  setprop sheet.range(ExRange1,ExRange1),*Value=MaskA

                                                  endif
                              endif
                              ADD       USAGE1 TO GRNDTOT2
                    ENDIF
          ENDIF
.
ltotallx
           clear     str1
         compare   c2 to tranflag
         if        equal
         MATCH     TEMPMLR1,MLR1
            IF        EQUAL
              move      mlr1 to nxrffld2
            else
              move      mlr2 to nxrffld2
            endif
         call      nxrfkey
         match      nxrflist to list
         if         not equal
         move       "*" to str1
         else
         clear      str1
         endif
         endif
         compare    c2 to univflag                .universes requested?
         if         equal                          .yes
         compare    c2 to umbrflag               .umbrella organization?
                    if        equal                         .yes
                              IF        (excelFlag = c1)
		         	if        (row >= 10000)
		         	call       OH1
		         	endif
                              add       sixlpi,row
                              prtpage   Laser;*p=125:row,*Boldon,"Consortium",*Boldoff
                              Elseif    (excelFlag = c2)
                              Move            Row1,DimRow1
                              call            Trim using DimRow1                 
                              pack            Exrange1 from "D",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value="Consortium"
                              endif
                    move       c0 to n10
                    RETURN
                    endif          else
         move     "Z,ZZZ,ZZZ,ZZ9" to unimask
         edit       n10 to unimask

         compare   c2 to TRANflag                .TRANS DATE requested?
         if        not equal
         clear     date
         endif                        .yes
          IF        (excelFlag = c1)
         if        (row >= 10000)
         call       OH1
         endif
          add       sixlpi,row
          prtpage   Laser;*p=25:row,Unimask,*p=2750:row,revisiondate,*p=3750:row,date,str1
          else
.not supported yet
          endif
         else
         compare   c2 to TRANflag
         if        not equal
         clear     date
         endif                        .yes
          IF        (excelFlag = c1)
         if        (row >= 10000)
         call       OH1
         endif
          add       sixlpi,row
          prtpage   Laser;*p=25:row,Unimask,*p=2750:row,revisiondate,*p=3750:row,date
          else
         
.not supported yet
          endif
         endif
         move       c0 to n10
         RETURN
. .............................................................................
* totals report HEADING
OH1
         COMPARE   C0 TO PAGE
         IF        EQUAL
	prtpage   Laser;*UNITS=*HIENGLISH:
                        *ORIENT=*Portrait,*Font=Font1
	ADD       C1 TO PAGE
                   ELSE
         CALL      PAGENUM
         ENDIF
         
         CALL      CNTRMLR
         compare   c1 to page
         if        equal
                    IF        (excelFlag = c1)
                    MOve      "500",row
                    PrtPAge   Laser;*alignment=*center,*p=4250:row,"CONFIDENTIAL",*alignment=*left
                    add       sixlpi,row
                    add       sixlpi,row
                    PrtPAge   Laser;*p=1000:Row,Title          
                    add       sixlpi,row
                    PrtPAge   Laser;*p=1000:Row,"Date: ",TODAY
                    add       sixlpi,row
                    PrtPAge   Laser;*p=1000:Row,"Client: ","(",COMPNUM,") ",MDES1
                    add       sixlpi,row
                    Elseif            (excelFlag = c2)
                              Move      C2,Row1
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "B",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value="CONFIDENTIAL"
                              setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
                              Add       C1,Row1
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "A",DimRow1
                              setprop sheet.range(ExRange1,ExRange1),*Value=TITLE
                              setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
                              Add       C1,Row1
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "A",DimRow1
                              pack      taskname from "Date: ",Today
                              setprop sheet.range(ExRange1,ExRange1),*Value=Taskname
                              setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
                              Add       C1,Row1
                              Move      Row1,DimRow1
                              call      Trim using DimRow1                 
                              pack      Exrange1 from "A",DimRow1
                              pack      taskname from "Client: ","(",COMPNUM,") ",MDES1
                              setprop sheet.range(ExRange1,ExRange1),*Value=taskname
                              setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"

                    endif
          else
                    IF        (excelFlag = c1)
                    MOve      "500",row
                    PrtPAge   Laser;*alignment=*center,*p=4250:row,"CONFIDENTIAL",*alignment=*left
                    add       sixlpi,row
                    add       sixlpi,row
                    PrtPAge   Laser;*p=1000:Row,Title          
                    add       sixlpi,row
                    PrtPAge   Laser;*p=1000:Row,"Date: ",TODAY
                    add       sixlpi,row
                    PrtPAge   Laser;*p=1000:Row,"Client: ","(",COMPNUM,") ",MDES1
                    add       sixlpi,row
                    endif
          endif
.
         MOVE      C7 TO LINES
         ADD       C7 TO LINES
         MOVE      C0 TO N1
         MOVE      PICSW TO N1
         BRANCH    N1 OF OH1PAY,OH1REC,OH1EVEN,OH1ALL
.
OH1PAY
                    IF        (excelFlag = c1)
          add       Sixlpi,Row
          prtpage   Laser;*p=1000:row,"Owes names to:"
          add       Sixlpi,Row
          add       Sixlpi,Row
          prtpage   Laser;*p=1400:row,*ulon,"Mailer",*p=5250:row,"Quantity",*ulOff
          add       Sixlpi,Row
                    endif
         GOTO      OH1EXIT
.
OH1REC
                    IF        (excelFlag = c1)
          add       Sixlpi,Row
          prtpage   Laser;*p=1000:row,"Is owed names from:"
          add       Sixlpi,Row
          add       Sixlpi,Row
          prtpage   Laser;*p=1400:row,*ulon,"Mailer",*p=5250:row,"Quantity",*ulOff
          add       Sixlpi,Row
                    endif
         GOTO      OH1EXIT
.
OH1EVEN
                    IF        (excelFlag = c1)
          add       Sixlpi,Row
          prtpage   Laser;*p=1000:row,"Has exchanged an even number of names with:"
          add       Sixlpi,Row
          add       Sixlpi,Row
          prtpage   Laser;*p=1400:row,*ulon,"Mailer",*uloff
          add       Sixlpi,Row
                    endif
         GOTO      OH1EXIT
.
OH1ALL
           IF        (excelFlag = c1)
          add       Sixlpi,Row
          add       Sixlpi,Row
          prtpage   Laser;*p=1000:row,"Has an exchange history with the organizations listed below."
          add       Sixlpi,Row
          prtpage   Laser;*p=1000:row,"Currently this mailer:"
          add       Sixlpi,Row
          add       Sixlpi,Row
          add       Sixlpi,Row
          prtpage   Laser;*p=4500:row,*P=4500:ROW,mdes1
          add       Sixlpi,Row
.          prtpage   Laser;*p=4500:row,*UlOn,"   Owes",*p=5550:row," Is owed",*p=6750:row,"Is even",*ulOff
          prtpage   Laser;*p=4750:row,*UlOn,"   Owes",*p=6100:row," Is owed",*p=7500:row,"Is even",*ulOff
          add       Sixlpi,Row
          elseif    (excelFlag = c2 & page = c1)
          Add       C1,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Has an exchange history with the organizations listed below."
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          Add       C1,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          pack      taskname from "Currently this mailer: ",MDES1
          setprop sheet.range(ExRange1,ExRange1),*Value=taskname
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          Add       C1,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "B",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Owes"
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "C",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Is owed"
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "D",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Is Even"
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          setprop sheet.range(ExRange1,ExRange1).Font,*Underline=xlUnderlineStyleSingle
          Add       C1,Row1
          endif
OH1EXIT
         ADD       C5 TO LINES
        RETURN
...............................................................................
PAGENUM
.begin patch 4.0
.         cmatch    "T" to prtsw
.         goto      DETCHK if not equal
.         goto      pagemask
..
.detchk   COMPARE   "55" TO LINES
.         IF        NOT EQUAL
.         GOTO      LINE1
.         endif
.end patch 4.2
.
pagemask
         MOVE      PAGEMASK TO PAGENUM
         EDIT      PAGE TO PAGENUM
         If        ("T" = prtsw & tranflag = "2")         
                    IF        (excelFlag = c1)
		    Move	"10500",row	
                    prtpage   laser;*p100:row," ©1991-2014, Names in the News":
                              *alignment=*center,*p=4150:row,Pagenum,*alignment=*left:
			      *p5700:row,"* Indicates report client was last to use list.":	
                              *newpage
                    endif
         else          
         	cmatch    "D" to prtsw
         	if        equal
                    	IF        (excelFlag = c1)
		    	Move	"8000",row	
                    	prtpage   laser;*p100:row," ©1995-2014, Names in the News":
                              	*alignment=*center,*p=5500:row,Pagenum,*alignment=*left:
                              	*newpage
                    	endif
         	else
                    	IF        (excelFlag = c1)
		    	Move	"10500",row	
                    	prtpage   laser;*p100:row," ©1991-2014, Names in the News":
                    	          *alignment=*center,*p=4250:row,Pagenum,*alignment=*left:
                    	          *newpage
                    	endif
         	endif
         endif
         
         ADD       C1 TO PAGE
         RETURN
.
.begin patch 4.0

LINE1    
.          IF        (excelFlag = c1)
.          endif
.         ADD       C1 TO LINES
.         GOTO      PAGENUM
.end patch 4.0
...............................................................................
CNTRMLR  SETLPTR   MDES1
         ENDSET    MDES1
CHKMHEAD CMATCH    B1 TO MDES1
         GOTO      SETMHEAD IF NOT EQUAL
         BUMP      MDES1 BY -1
         GOTO      CHKMHEAD IF NOT EOS
SETMHEAD MOVEFPTR  MDES1 TO N3
         MOVE      C80 TO MLRTAB
         SUBTRACT  N3 FROM MLRTAB
         DIVIDE    C2 INTO MLRTAB
         RESET     MDES1
         SETLPTR   MDES1
         RETURN
...............................................................................
NEWPAGE
          IF        (excelFlag = c1)
          prtpage   laser;*newpage
          move      c10,row
          endif
         MOVE      C0,LINES
         ADD       C1,PAGE
         RETURN
...............................................................................
EOJ22
         if        (row >= 10000)
	 call       OH1
	 endif
         MOVE      MASK TO MASKA
         EDIT      TOTAL TO MASKA
         MOVE      PICSW TO NUM
         BRANCH    NUM OF EOJ22A,EOJ22B,EOJ22C,EOJ22ALL
EOJ22A
          IF        (excelFlag = c1)
                    endif
         GOTO      EOJ22D
.
EOJ22B
          IF        (excelFlag = c1)
                    endif
         GOTO      EOJ22D
.
EOJ22C   
         GOTO      EOJ22D
.
EOJ22ALL 
         if        (row >= 10000)
	 call       OH1
	 endif
         MOVE      MASK TO MASKA
         EDIT      GRNDTOT1 TO MASKA
          IF        (excelFlag = c1)
          add       sixlpi,row
          add       sixlpi,row
          add       sixlpi,row
          PrtPAge   Laser;*p=1000:row,"TOTAL NAMES:",*alignment=*right,*p=5500:row,MASKA
          ElseIF   (excelFlag = c2)
          add       c2,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value="Total Names:"
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "B",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=MaskA
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          endif
          
         MOVE      MASK TO MASKA
         EDIT      GRNDTOT2 TO MASKA
          IF        (excelFlag = c1)
          PrtPAge   Laser;*p=7000:row,MASKA,*alignment=*Left
          ElseIF   (excelFlag = c2)
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "C",DimRow1
          setprop sheet.range(ExRange1,ExRange1),*Value=MaskA
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          Add       C1,Row1
          Move      Row1,DimRow1
          call      Trim using DimRow1                 
          pack      Exrange1 from "A",DimRow1
          pack      str55 from "©1991-2014, Names in the News"
          setprop sheet.range(ExRange1,ExRange1),*Value=Str55
          setprop sheet.range(ExRange1,ExRange1).Font,*Bold="True"
          endif
         GOTO      EOJ22D
EOJ22D
         MOVE      C0 TO TOTAL
          IF        (excelFlag = c1)
          endif
.         ADD       C4 TO LINES
         CALL      PAGENUM
         BEEP
         GOTO      ENDPRT
. .............................................................................
.
EXIT
         CLOSE     INPUT
         shutdown  "CLS"
         STOP
ERROR    DISPLAY   *P03:24,*EL,FMESG,"NOT ON LINE PLEASE INFORM COMPUTER ":
                   "PERSONEL";
         KEYIN     *P30:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT IF EQUAL
         GOTO      ERROR
.
INT
         TRAPCLR   INT
         NORETURN
         TRAP      INT IF INT
         TRAPCLR   F1
INT1
         DISPLAY   *P1:1,*ES,*B,*B,*B,*B,*HON:
                   *P8:4,"**********************************************":
                   *P8:5,"*   INT. ERROR CALL COMPUTER PERSONNEL.":    *
                   *P8:6,"**********************************************",*HOFF
         PAUSE     C1
         BEEP
         PAUSE     C1
         BEEP
         KEYIN     *P30:24,*EOFF,STR1;
         CMATCH    "I",STR1
         GOTO      EXIT IF EQUAL
         GOTO      INT1
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Exchange Report - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
.                   Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return

                    endif
          
                    goto      checkfile
                              include compio.inc
                              include   cntio.inc
         include   nxrfio.inc
         include   ndatio.inc
         include   nordio.inc
         INCLUDE   NOFRIO.INC
          INCLUDE   NSEL2IO.INC
          include   NXNGIO.INC
          INCLUDE   NXCHIO.INC
          INCLUDE   COMLOGIC.inc