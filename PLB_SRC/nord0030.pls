PC       EQU       0
.
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
.patch1.62
                                        include   compdd.inc
                                        include   cntdd.inc
.        INCLUDE   NMLRDD.inc
.patch1.62
         INCLUDE   OSLSPERN.inc
.patch1.62
.        include   NBRKDD.inc
.patch1.62
.next patch add creation of excel file for LM output requested by Susan,
release   init    "1.80"                  DLH      New YEar
Reldate   Init      "2016 January 5"
.release   init    "1.79"                  DLH      New YEar
.Reldate   Init      "2015 January 5"
.release   init    "1.78"                  DLH      New YEar
.Reldate   Init      "2014 January 2"
.release   init    "1.77"                  DLH      New YEar
.Reldate   Init      "03 January 13"
.release   init    "1.76"                  DLH      New YEar
.Reldate   Init      "03 January 12"
.release   init    "1.75"                  DLH      New YEar
.Reldate   Init      "04 January 11"
.release   init    "1.74"                  DLH      EXcel, use all option by default
..                                         also specify sales numbers as null records causes print routine to loop
.Reldate   Init      "21 July 10"
.release   init    "1.73"                  DLH      combine LM-PLI with LM
.Reldate   Init      "17 March 10"
.release   init    "1.72"                  DLH      New Year
.Reldate   Init      "28 JAN 10"
.release   init    "1.71"                  DLH      Use date passed by dsinit
.Reldate   Init      "2 Nov 09"
.release   init    "1.70"                  DLH      New printer, Turn off PRTPLAY
.Reldate   Init      "31 Aug 09"
.release   init    "1.68"                  DLH      New YEar
.Reldate   Init      "31 Jan 09"
.release  init    "1.67"                01Feb2008  DLH      New YEar
.release  init    "1.66"                08Mar2007  DLH      Oslspern.inc expansion
.release  init      "1.65"         JD   23Jan2006 2006 date.
.;release  init      "1.64"         JD  13Jan2005 2005 date.
.;release  init      "1.63"        ASH  05AUG2004 Logo Conversion
.release  init      "1.62"        DMB   26MAY2004 Mailer Conversion
.RELEASE   init      "1.61"             JD 24OCT2002 upd str2 for salesp to new var.
.;RELEASE   init      "1.6"             DMB 06AUG2002 changed printer to use laser 8 (FS-9000 Drivers) (7000 Drv do not work properly with overlay)
.;RELEASE   init      "1.5"             DMB 21FEB2002 ADDED Printing of Sales Year to date report and List management monthly report
.release  init      "1.4"             ASH 02OCT2000 NEW SERVER ADDED
.release  init      "1.3b"            JD  29JAN99   skip pending orders.
.RELEASE  INIT      "1.3"             04JAN99 ASH   NINORD Y2K, File expansion: did not increase QTYOLD & QTYNEW which
.                                     may need to be increased if OQTY ever fills its' new DIM 9 capacity!
.release  init      "1.2"             ASH 21Sep98   NINMLR Y2K File expansion, Ouput file size increased
.RELEASE  INIT      "1.1"             JD 27JAN98    update for 1998.
.RELEASE  INIT      "1.0"             07JUL92 DLH   CHANGE DATABASE TO INCLUDE
.                                                   ORDER MONTH  & YEAR
.RELEASE  INIT      "PRE"             12JUN92 DLH   PULL INFO BY SALES PERSON.
FileCheck FIle
trapcount form      4
***************************************************************************************
OSLS     DIM        25
.Start Patch #1.2 - Increased Output file and key size cause MCOMP was increased
.OUTPUT   IFILE      KEYLEN=54,UNCOMP,fix=89
.File size originally was incorrect!!!!!!
**************************************************
.OUTPUT   IFILE      KEYLEN=74,UNCOMP,fix=106
OUTPUT   IFILE      KEYLEN=74,UNCOMP,fix=150
**************************************************
.End Patch #1.2 - Increased Output file and key size cause MCOMP was increased
.str2                          1-2
.Start Patch #1.2 - Increased Output file and key size cause MCOMP was increased
.OUTKEY   DIM        54         3-56
.OUTKEY   DIM        74         3-76
OUTKEY   DIM        74         3-76      .osls,mcomp,oodtem,oodtey
outstr   dim        2
.End Patch #1.2 - Increased Output file and key size cause MCOMP was increased
NUMNEW   FORM       6         74-82
QTYNEW   FORM       9         83-91
NUMOLD   FORM       6         92-97
QTYOLD   FORM       9         98-106
****************************************************
.brcomp   dim      45         107-150
***************************************************
ANS      DIM        1         107-107
DATE     DIM        8         108-115
DATEMASK DIM        8         116-123
SYSMO    DIM        2         124-125
SYSDY    DIM        2         126-127
SYSYR    DIM        2         128-129
LASTYY   DIM        2         130-131
RUNLISTS INIT       "014477-005051-009766"
..proyears init      "00-01"
*************************************************************************************
.Must be updated Yearly (LST YR - Cur YR)
.begin patch 1.80
proyears init      "15-16"
.end patch 1.80
.Must Change LASTYY VAR located in code
.++++++++++++++++++++++++++
.start patch 1.68
.begin patch 1.72
thisyy    dim       2
.proyears init      "08-09"
.Update
.begin patch 1.75
.proyears init      "09-10"
.begin patch 1.75
.proyears init      "10-11"
.end patch 1.75
.end patch 1.72
.start patch 1.67
.proyears init      "07-08"
.start patch 1.65
.proyears init      "06-07"
.proyears init      "05-06"
.end patch 1.65
.start patch 1.64
.proyears init      "04-05"
.end patch 1.64
.proyears init      "03-04"
.proyears init      "02-03"
.proyears init      "01-02"
.+++++++++++++++++++++++++++
.............................................................................................
.some excel goodies
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
N34       form    3.4
N92       form    9.2
MailDate  dim    4
VT_BOOL   EQU 11          .Boolean
VT_I4     EQU                  3           .4 byte integer
str5a     Dim       5
OTRUE     variant
OFALSE    variant
xlLeft    integer 4,"0xffffefDD"
xlTop     integer 4,"0xffffefc0"
xlAlignCenter integer 4,"0xffffeff4"
xlBottom  integer      4,"0xffffeff5"
XlLineStyleDBl Integer 4,"0xffffefe9"                         .line style double
.XlShiftToLeft  Integer 4,"0xffffefc1"                         .delete shift to left
XLShiftToLeft       Variant                        .range delete shift to left
XLShiftUp Variant                        .range delete shift Up     
xlLandscape integer 4,"0x2"                     .2
SheetsDefault                           integer 4,"0x00000000"
AlignLeft                               integer 4,"0xffffefdd"
AlignRight                              integer 4,"0xffffefc8"

xlInsideHorizontal            integer 4,"0x12"                .Borders inside defined range
xlInsideVertical              integer 4,"0x11"                .Borders inside defined range
XlEdgeRight                   integer 4,"0x10"                .Borders right edge of defined range
VT_R8         EQU 5           .Double - 8 byte Real
VT_R8a         EQU 5           .Double - 8 byte Real
xlRowHeight   variant
xlColumnWidth variant
TopMargin     variant
BottomMargin  variant
LeftMargin     variant
RightMargin     variant
SheetIndex                              variant
Cell         DIM    6

CellRowCnt          FORM      4

REportHdr     Dim             250
ExRange1      Dim             5
ExRange2      Dim             5
ExRange3      Dim            12
Row1          form            2
Row2          Form            2
Row3          form            2
Row4          Form            2
DimRow1       Dim             2
DimRow2       Dim             2
DimRow3       Dim             2
DimRow4       Dim             2
.START PATCH 1.04 REPLACED LOGIC
TASKNAME2 DIM       200
SReturn        init 0x0a                     ;soft return/line feed
HOLDEXCL  dim       1
AlignCenter integer 4,"0xffffeff4"
Label1    Init      "Label"   
Label2    Init      "Label"   
Label3    Init      "Label"   
Label4    Init      "Label"   
Label5    Init      "Label"   
Label6    Init      "Label"   
Label7    Init      "Label"   
Label8    Init      "Label"   
***************************************************************************************.
INFILE   FILE
.sort Parameters=======================================================
INDAT    init  "\\nins1\e\data\salesref.dat"   .File to be sorted
OUTSRT   init  "\\nins1\e\data\salesref.srt"   .Sorted Output file
Salesper dim   2
ClintSrt init  "28-72"                        .Sort by client #
.SORTVAR   INIT     "\\nins1\e\data\salesref.dat,\\nins1\e\data\salesref.srt;28-72,s=1='06'&73='0901'|1='06'&73='0900'"
SORTFLE  dim    70                            .Var to pack file names of sort
.======================================================================
prfile  pfile
sales    dim       25
slsper   dim       26          .VAR USED TO LOAD SALESPERSON COMBO BOX
yr1        dim       2           .GRABS PREVIOUS YEAR
yr2      dim       2           .GRABS CURRENT YEAR FROM TIMESTAMP
MOYR1    dim      4            .USED TO MATCH AGAINST PREVIOUS MO/YR IN FILE
MOYR2    dim      4            .USED TO MATCH AGAINST CURRENT MO/YR  IN FILE
HOLDCOMP dim      45           .HOLDCOMP COMPANY NAME OF PREVIOUS INPUT RECORD
MO1      dim      2
.bosflag   form     1            . 1=win 95,98, 2=NT
.=============================================================
Title1   form     9
Title2   form     9
Title3   form     9
Title4   form     9
SROW     FORM     9            .ROW FOR SALES PERSON NAME
.============================================================
C23      FORM    "23"
Rowcount form    3             .KEEP TRACK OF ROW PER PAGE
PgCnt    form    9             .COUNT OF PAGES
NEWPG    FORM    1             .COUNTER TO SHOW- IF NEW PAGE PUT TOTALS AT TOP OF PAGE
FRSTFLG  form    1             .Flag to show if record is first of two poss records
Newsls   form    1             .Flag to start new page for new salesperson
BothFlg  dim     1
.ALLSLS   FORM    1             .FLAG TO SHOW THAT REPORT IS FOR ALL SALES PEOPLE
RPTFLAG  form    1             .FLag Shows if rep is LM or all sales
OYR1     init    "Orders "     .Must be changed Yearly  -For Titles
QYR1     init    "Qty "        .Must be changed Yearly  -For Titles
.OYR2     init    "Orders 2001".Must be changed Yearly  -For Titles
.QYR2     init    "Qty"        .Must be changed Yearly  -For Titles
TITLEYR1 DIM      4            .TITLE FOR PREVIOUS YEAR
TITLEYR2 DIM      4            .TITLE FOR CURRENT YEAR
Holdsales    dim       25

OTOT1      form     9           .Order total for previous YR
QTOT1      form     9           .QTY total for current YR
OTOT2      form     9           .Order total for current YR
QTOT2      form     9           .QTY total for current YR
TMPOTOT1   form     9          .Temp Order Total
TMPQTOT1   form     9          .Temp Order Total
TMPOTOT2   form     9          .Temp Order Total
TMPQTOT2   form     9          .Temp Order Total
.====================================================================================
REPTFLAG    form     1
Filesize  FOrm      10

.Months of the Year
CurMo     dim   10
Month1    init  "January"
Month2    init  "February"
Month3    init  "March"
Month4    init  "April"
Month5    init  "May"
Month6    init  "June"
Month7    init  "July"
Month8    init  "August"
Month9    init  "September"
Month10   init  "October"
Month11   init  "November"
Month12   init  "December"
.====================================================================================
font15    font
          create  font15,"Times New Roman",size=16,bold
Font9    font
          create  font9,"Times New Roman",size=10,italic
***************************************************************************************
.START PATCH 1.63 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.63 ADDED LOGIC
         IFZ       PC
.Start Patch #1.2 - Increased Output file and key size cause MCOMP was increased
.         PREPARE   OUTPUT,"g:\DATA\SALESREF","g:\data\salesref","54","89"
.START PATCH 1.4 REPLACED LOGIC
.         PREPARE   OUTPUT,"g:\DATA\SALESREF","g:\data\salesref","74","106"
**************************************************
. STARTING LR #.
         KEYIN     *P1:24,*EL,"HEAR WE GO";
.**********.280833 FIRST LR 1997. *************
.         MOVE      "185570" TO NORDFLD            change every jan.
.         MOVE      "234738" TO NORDFLD            change every jan.
.         MOVE      "257867" TO NORDFLD            change every jan.
.         MOVE      "280833" TO NORDFLD            change every jan.
.         MOVE      "305233" TO NORDFLD            change every jan. previous yr.
.         MOVE      "370000" TO NORDFLD            change every jan. previous yr.
.         MOVE      "450000" TO NORDFLD            change every jan. previous yr.
.         MOVE       "500000" TO NORDFLD            change every jan. previous yr.
.begin 12/31/09
.         MOVE       "555000" TO NORDFLD            change every jan. previous yr.         
.**********.643810 FIRST NIN LR OF 2009
.**********.686400 FIRST NIN LR OF 2010
         MOVE       "686400" TO NORDFLD            change every jan. previous yr.         
.end 12/31/09
.begin patch 1.71
.          CLOCK     DATE TO DATE
          if        (program = "NORD0030")
          Unpack    Today into mm,str1,dd,str1,yy
          move     Today to date
          else
          clock     Date to date
          endif
.end patch 1.71
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         MOVE      DATE TO DATEMASK
         MOVE      DATE TO TODAY
         XIF
         TRAP      EOJ IF F5
         MOVE      "ABORT" TO PF5
         MOVE      "NORD0030" TO PROGRAM
         MOVE      "Names In The News"   TO COMPNME
         MOVE      "SALESPERSON X-REF" TO STITLE
         CALL       PAINT
         CALL       FUNCDISP
DATEDIS
         KEYIN     *P10:10,*DV,DATEMASK," OK? ",*t20,ANS
         CMATCH    "N" TO ANS
         GOTO      BEGIN IF NOT EQUAL
         KEYIN     *P10:10,*+,mm,"/",dd,"/",yy
         PACK      DATE FROM mm,SLASH,dd,SLASH,yy
         MOVE      DATE TO DATEMASK
         GOTO      DATEDIS
BEGIN
         MOVE      C0 TO N2
         MOVE      YY TO N2
         SUB       C1 FROM N2
.begin patch 1.68
.begin patch 1.67
.;Need to change this on a yearly basis
.begin patch 1.72
.begin patch 1.75
.         MOVE      "09" TO LASTYY
.begin patch 1.76
.         MOVE      "10" TO LASTYY
.begin patch 1.77
.         MOVE      "11" TO LASTYY
.begin patch 1.78
.         MOVE      "12" TO LASTYY
.begin patch 1.79
.begin patch 1.80
         MOVE      "15" TO LASTYY
.end patch 1.80
.end patch 1.79
.end patch 1.78
          move      lastyy,n2
          add       c1,n2
          move      n2,thisyy
.end patch 1.77
.begin patch 1.75
.end patch 1.72
.         MOVE      "07" TO LASTYY
.         MOVE      "06" TO LASTYY
.end patch 1.67
.         MOVE      "04" TO LASTYY
.         MOVE      "01" TO LASTYY
.         MOVE      N2 TO LASTYY
         MOVE      C1 TO NORDPATH
         MOVE      C1 TO NMLRPATH
..temp*************************************************************************         
.         goto       Start2002
.temp*************************************************************************         
         PACK     STR35,NTWKPATH1,"SALESREF"
         PACK     STR45,NTWKPATH1,"SALESREF"
**************************************************
         PREPARE   OUTPUT,str35,str45,"74","150"
.         PREPARE   OUTPUT,str35,str45,"74","106"

         CALL      NORDKEY
         GOTO      START2002 IF OVER
         ADD       C1 TO N8
         GOTO      DETAIL
LOOP     CALL      NORDKS
         GOTO      START2002 IF OVER
         ADD       C1 TO N8
         reset     proyears
         scan      oodtey in proyears
         goto      loop if not equal
.
DETAIL   DISPLAY   *P10:12,"RECORDS PROCESSED ",N8
.begin patch 1.3b
.         CMATCH    "p" TO OSTAT       Pending order ?
.         GOTO      loop IF EQUAL     YES, skip.
.         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
.         GOTO      loop IF EQUAL     YES, skip.
.         CMATCH    "l" TO OSTAT      lcr order ?
.         GOTO      loop IF EQUAL     YES, skip.
.         CMATCH    "z" TO OSTAT       Cancelled LCR order ?
.         GOTO      loop IF EQUAL     YES, skip.
          if        (Ostat <> "0" and OSTAT <> "B")
          goto      loop
          endif
.note cancodes also updated to skip cancelled pending orders.
.end patch 1.3b
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES         *CANCELLED ?
         GOTO      LOOP IF EQUAL               *YES
         RESET     RUNcodes
         SCAN      OLNUM IN RUNcodes          *RUNNING CHARGE?
         GOTO      LOOP IF EQUAL              *YES
.Start patch #1.3 - increased var
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         COMPARE   C0 TO N7                 *REAL ORDER?
         MOVE      C0 TO N9
         MOVE      OQTY TO N9
         COMPARE   C0 TO N9                 *REAL ORDER?
.end patch #1.3 - increased var
         GOTO      LOOP IF EQUAL            *NO.
         PACK      outstr FROM OSALES10,OSALES
.begin patch 1.73
          if        (outstr = "27")
          move      "06",outstr
          endif
.end patch 1.73

         MOVE      C0 TO N2
         MOVE      outstr TO N2
         PACK      OSLS FROM B10,B10,B5
         MOVE      OSLS0 TO OSLS
         LOAD      OSLS FROM N2 OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5,OSLS6:
                   OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13,OSLS14:
                   OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21,OSLS22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
.
         PACK      STR7 FROM OMLRNUM,Z3
         MATCH     STR7 TO MKEY
         IF        NOT EQUAL
         MOVE      STR7 TO MKEY
         REP       ZFILL IN MKEY
         CALL      NMLRKEY
         ENDIF
         CLEAR     OUTKEY
         PACK      OUTKEY FROM OSLS,MCOMP,OODTEM,OODTEY
         reset     outkey
         READ      OUTPUT,OUTKEY;outstr,OUTKEY,NUMOLD,QTYOLD,NUMNEW,QTYNEW
         GOTO      WRITE IF OVER
          call      debug
.         MATCH     OODTEY TO YY
.         IF        EQUAL
.         ADD       C1 TO NUMNEW
..Start patch #1.3 - increased var
..         ADD       N7 TO QTYNEW
.         ADD       N9 TO QTYNEW
..End patch #1.3 - increased var
.         GOTO      UPDATE
.         ENDIF
          if        (oodtey = Lastyy)
.         MATCH     OODTEY TO LASTYY
.         IF        EQUAL
         ADD       C1 TO NUMOLD
.Start patch #1.3 - increased var
.         ADD       N7 TO QTYOLD
         ADD       N9 TO QTYOLD
.End patch #1.3 - increased var
         GOTO      UPDATE
          elseif    (oodtey = thisyy)
         ADD       C1 TO NUMNEW
         ADD       N9 TO QTYNEW
         GOTO      UPDATE
         ENDIF  
         GOTO      LOOP
UPDATE   FILEPI    1;OUTPUT
**********************************************************************
         UPDATE    OUTPUT;outstr,OUTKEY,NUMOLD,QTYOLD,NUMNEW,QTYNEW
.         UPDATE    OUTPUT;STR2,OUTKEY,NUMOLD,QTYOLD,NUMNEW,QTYNEW
**********************************************************************
         GOTO      LOOP
WRITE    PACK      OUTKEY FROM OSLS,MCOMP,OODTEM,OODTEY       .RESTORE KEY
         reset     outkey
         PACK      outstr FROM OSALES10,OSALES
         MOVE      C0 TO NUMNEW
         MOVE      C0 TO QTYNEW
         MOVE      C0 TO NUMOLD
         MOVE      C0 TO QTYOLD
          if        (oodtey = Lastyy)
          add       c1,numold
         ADD       N9 TO QTYOLD
         GOTO      write1
          elseif    (oodtey = thisyy)
         ADD       C1 TO NUMNEW
         ADD       N9 TO QTYNEW
         GOTO      write1
         ENDIF  
         GOTO      LOOP
WRITE1   FILEPI    1;OUTPUT
********************************************************************
         PACK   NBRKFLD,OBRKNUM,z3
         rep    zfill in nbrkfld
         call   NBRKKEY
         WRITE     OUTPUT,OUTKEY;outstr,OUTKEY,NUMOLD,QTYOLD,NUMNEW,QTYNEW,BRCOMP
.         WRITE     OUTPUT,OUTKEY;STR2,OUTKEY,NUMOLD,QTYOLD,NUMNEW,QTYNEW,OBRKNUM,OBRKCNT
********************************************************************
         GOTO      LOOP
.
START2002
************************************************************************************
                      call     getwinver
.        getinfo  system,str6
.        unpack   str6 into str1,str2
.        unpack   str2 into str1
.        move     c0 to bosflag
..0 = unknown
..1 = Windows NT
..2 = WIN32s Windows 3.1x (obsolete)
..3 = Window 95
..4 = Window 98
..5 = Windows 2000
..6 = Windows XP
..8 = Windows CE
.        if (str1 = "3" or str1 = "4")
.                 move     c1 to bosflag
.        endif
.        if (str1 = "1" or str1 = "5" | str1 = "6")
.        if (str1 = "1" or str1 = "5")
.         move     c2 to bosflag
.        endif
.               if             (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
.                                                  move           c2 to bosflag
.               ELSEIF (OSFLAG = "3" or OSFLAG = "4")                  .95/98
.                                                  move           c1 to bosflag
.               endif
.==================================================================
.Set up columns
        move    "100",column
        move    "3350",column1
        move    "4500",column2
        move    "5700",column3
        move    "6900",column4
.       7860 row position of pg #
        move    "3000",Title1
        move    "6600",Title2
        move    "5600",Title3
        move    "4000",Title4
.        move    "3350",Title4
        MOVE    "520",SROW
.        MOVE    "811",SROW
.==============================================================
.==============================================================
          call paint
.==============================================================
.Fills MO, YR Edit text boxes with Prev and Current Year
.begin patch 1.71
.        clock   timestamp,str6
.        unpack  str6,str4,str2
.        move    str2 to MO1
.        call    zfillit using str2
.        BEEP
          Move      MM,MO1
          pack      str4 from CC,YY
          move      YY,yr1
.end patch 1.71          
MNTH
        move     YES to ANS
        KEYIN   *P10:12,*DV,"Current Month  ",MO1," OK? ",*t20,ANS
        CMATCH  "N" TO ANS
        IF  EQUAL
                  KEYIN   *P10:12,"Current Month  ",MO1
                  GOTO    MNTH
        ENDIF
.        CMATCH  "N" TO ANS
.        GOTO    MNTH IF EQUAL

.        BEEP
CURYR
        move     YES to ANS
        KEYIN   *P10:12,*DV,"Current Year ",*DV,str4," OK? ",*t20,ANS
        CMATCH  "N" TO ANS
        IF  EQUAL
                    KEYIN   *P10:12,"Current Year ",str4
                GOTO    CURYR
        ENDIF
.        CMATCH  "N" TO ANS
.        GOTO     CURYR IF EQUAL
        MOVE     STR4 TO TITLEYR1
        unpack   str4,cc,yr1
        move    str4 to n4
        sub     c1,n4                    .subtract current year buy 1
        clear   str4
        move    n4 to str4
        MOVE     STR4 TO TITLEYR2
        unpack   str4,cc,yr2
.Open Excel application
        create  ex
.Reset Default of Worksheets found in a Workbook
        Getprop ex,*SheetsInNewWorkbook=SheetsDefault
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
          create    SheetIndex,VarType=VT_I4                          
        setprop ex,*Visible="True"




DOALL
.............................
.Code Added to be used in conjuction with eomjob.wbt
        If    (FUNC = "B")
                  move      "A" to str1
          elseIF    (FUNC = "L")
                  move      "L" to str1
          Else
                  move      "A" to str1
        ENDIF
.        KEYIN     *P10:12,*EF,"'L'",*white,"istManagement ",*cyan,"(A)",*white,"ll ",*cyan,"(B)",*white,"oth ",*cyan,*T15,*RV,*UC,STR1
        KEYIN     *P10:12,*EF,"'L'",*white,"istManagement ",*cyan,"(A)",*white,"ll ","'B'",*white,"oth ",*cyan,*T15,*RV,*UC,STR1
        rep       "L1A2B3" in STR1
        clear n1
.if running both reports start with list management
        if       (str1 = "3")
                 move YES to BothFLG
                 move c1 to str1
        endif
        move      str1 to rptflag
Again
        if        (rptflag < c1)
                   goto EOJ
        endif
          move      c0,N2
        branch    rptflag to ListM,Print11
ListM
        move      c6 to n2
        LOAD      str25 FROM N2 OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5,OSLS6:
                   OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13,OSLS14:
                   OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21,OSLS22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
        move     n2 to outstr
        move     outstr to Salesper
        rep      zfill in salesper
        goto Print11

Print11
        clear Rowcount
        clear PgCnt
        clear NEWPG
        Clear FRSTFLG
        Clear OTOT1
        Clear QTOT1
        Clear OTOT2
        Clear QTOT2

        if       (rptflag = c2)
                 goto ALLSALES
        endif

ALLSALES

        if       (rptflag = C2)
           add      c1 to n2
          if        (n2 = "21")
          call      debug
          endif
          if       (n2 = "36")
          goto      end1
          endif
          if        (n2 = "2" | n2 = "3" | n2 = "6" | n2 = "8" | n2 = "9" | n2 = "11" | n2 = "13" | n2 = "15" | n2 = "21" | n2 = "29")
          else
          goto      AllSales
          endif
                   CLEAR    STR25

                       LOAD      str25 FROM N2 OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5,OSLS6:
                         OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13,OSLS14:
                             OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21,OSLS22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
                 call Trim using str25   
                 if (str25 = "                         ")
                        goto ALLSALES
                 endif
                 if (str25 = "")
                        goto ALLSALES
                 endif
                 move     n2 to salesper
                 rep      ZFILL,salesper
                 if       (salesper = "")
                           goto ALLSALES
                 endif
          Else
          Goto      End1
        ENDIF
          close    infile
.======================================================================
.Sort Commands
SORTER
         DISPLAY   *P10:14,"Doing  ",str25
         pack   SortFle,indat,comma,outsrt
.Current Month Year
         pack   MOYR1,MO1,yr1
.Previous Month Year
         pack   MOYR2,MO1,yr2
         if     (rptflag = c1)
          pack   taskname,sortfle,";",clintsrt,comma,"s=1=","'",Salesper,"'","&73=","'",MOYR1,"'","|","1=","'",salesper,"'","&73=","'",MOYR2,"'"
         endif
         if     (rptflag = c2)
          pack   taskname,sortfle,";",clintsrt,comma,"s=1=","'",Salesper,"'","&73<=","'",MOYR1,"'","|","1=","'",salesper,"'","&73<=","'",MOYR2,"'"
         endif
         sort   taskname
         if over
               alert caution,S$ERROR$,result,"No Sort"
.               return
         endif
          FindFIle   outsrt,Filesize=filesize
                    if         (Filesize = c0)
                    goto       Allsales
                    endif
.        pack      taskname from NTWKPATH2,"sort32 ","\\nins1\e\data\salesref.dat ","\\nins1\e\data\salesref.srt ","/s (28,45,alp,a)"," inc(1,2,n,eq,\'06\',and,(73,2,n,eq,\'09\'))"

opener
          OPEN    INFILE,"\\nins1\e\data\salesref.srt"
Print1
**********If printer does NOT have KYOCERA 9000 driver may have printing issues
.Printing-Open
        clear   str40
        pack    str40,"c:\work\",str25,".lst"
        call    trim using str40
.        if (bosflag = c2)
.                if (osflag = c1 | osflag = C5 | osflag = c6)         .nt
.                        PRTOPEN prfile,"\\NINs2\Laser8",str25
.                elseif (osflag = c3 | osflag = c4)         .win 95 98
.                        PRTOPEN prfile,"Laser8",str25
.                else   .(osflag = c0)         .Don't know prompt for printer
.                        PRTOPEN prfile,"-",str25
.                endif
                              if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6" or Osflag = c8 or Osflag = c9)  .NT4,NT5,XP
                                        PRTOPEN   PRFile,"\\NINs2\Laser8",str25
                              elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
                                        PRTOPEN   PRFile,"Laser8",str25
                              elseif (osflag = "9" or osflag = "7")         .win 7  or vista
                                        PRTOPEN   PRFile,"\\NINs2\Laser8",str25
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   PRFile,"-",str25
                              endif



..          PRTOPEN prfile,"\\NINs2\Laser6",str25,noprint,spoolfile=str40
..         PRTOPEN prfile,"\\NINs2\Laser6",str25,noprint,spoolfile=str40
.        else
..          PRTOPEN prfile,"Laser6","List Mgmt Ref",noprint,spoolfile=str40
.                if (osflag = c1 | osflag = C5 | osflag = c6)         .nt
.                        PRTOPEN prfile,"\\NINs2\Laser8","List Mgmt Ref"
.                elseif (osflag = c3 | osflag = c4)         .win 95 98
.                        PRTOPEN prfile,"Laser8","List Mgmt Ref"
.                else   .(osflag = c0)         .Don't know prompt for printer
.                        PRTOPEN prfile,"-","List Mgmt Ref"
.                endif
..         PRTOPEN prfile,"Laser6","List Mgmt Ref",noprint,spoolfile=str40
.        endif
.============================================================================================
NewPage
.Defining Page
        CLEAR     ROWCOUNT
        ADD       C1 TO PGCNT
        prtpage   prfile;*NEWPAGE:
                   *UNITS=*HIENGLISH;
.======================================================================
        clear     row
        move      "300",row
.======================================================================
        prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date: ";
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*ll,*font=font12,str10;
        prtpage prfile;*pTitle4:row,*font=font12,*ALIGNMENT=*CENTER,*boldon,*ll,*ULON,"Salesperson",*ULOFF,*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        add     "20",row
        add     eightlpi,row
.===========================================================================
        move    MO1 to nmm
        Load    curmo,nmm with Month1,Month2,Month3,Month4,Month5,Month6,Month7:
                Month8,Month9,Month10,Month11,Month12
        call    trim using curmo
                    prtpage prfile;*pTitle4:Row,*ALIGNMENT=*CENTER,*font=font15,*ll,curmo;
        if          (rptflag = c2)
                    call trim using curmo
                                        pack str15 with curmo," -YTD"
                    prtpage prfile;*pTitle4:Row,*ALIGNMENT=*CENTER,*font=font15,*ll,str15;
        endif
.==========================================================================
Client
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Client",*ULOFF,*boldoff;
        CLEAR   STR11
        PACK    STR11,OYR1,TITLEYR2
        prtpage prfile;*pColumn1:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,*ll,STR11,*ULOFF,*boldoff;
        CLEAR   STR8
        PACK    STR8,QYR1,TITLEYR2
        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,*ll,STR8,*ULOFF,*boldoff;
        CLEAR   STR11
        PACK    STR11,OYR1,TITLEYR1
        prtpage prfile;*pColumn3:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,*ll,STR11,*ULOFF,*boldoff;
        CLEAR   STR8
        PACK    STR8,QYR1,TITLEYR1
        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,*ll,STR8,*ULOFF,*boldoff;
        add     eightlpi,row
        add     "20",row
        if (FRSTFLG = C1)
                goto pageread
        endif


          loop
        if (newsls <> c1)
          READ  INFILE,SEQ;outstr,OSLS,MCOMP,OODTEM,OODTEY,NUMOLD,QTYOLD,NUMNEW,QTYNEW
        endif
          until over
          call      Trim using Osls
        if (str25 <> osls)
                if (OSLS <> "")
                        move c1 to newsls
                        goto Newpage
                else
                        move c0 to newsls
                endif
.===========================================================================
        endif
        if  (OODTEM <> MO1)
               if (rptflag <> c2)
                    goto warning
               endif
        endif
        if  (OODTEY <> YR2 & OODTEY <> YR1)
                goto warning
        endif

PAGEREAD
          move  osls to sales
          call trim using sales

          if        (Sales <> HOldSales)
          call      SheetSetup                              
          endif          
          
          prtpage prfile;*p4000:SROW,*font=font12,*ALIGNMENT=*CENTER,*ll,sales;
          Match MCOMP to HOLDCOMP
          if not equal
                  Clear TMPOTOT1
          Clear TMPQTOT1
                    Clear TMPOTOT2
                    Clear TMPQTOT2
                    add     eightlpi,row
                    add     "35",row
                prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,MCOMP;

          Move CellRowCnt,Str5
          call trim using str5
          Pack Cell,"A",Str5
          setprop sheet.range(Cell),*Value=Mcomp,*HorizontalAlignment=xlLeft

                clear str9
                  move numold to str9
                    call trim using str9
                prtpage prfile;*pcolumn1:row,*font=font12,*ALIGNMENT=*Left,str9;
          Pack Cell,"B",Str5        
          setprop sheet.range(Cell),*Value=numold,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"
                  clear str9
                    move qtyold to str9
                  call trim using str9
                    prtpage prfile;*pcolumn2:row,*font=font12,*ALIGNMENT=*Left,str9;
          Pack Cell,"C",Str5        
          setprop sheet.range(Cell),*Value=qtyold,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"
                clear str9
                  move numnew to str9
                    call trim using str9
                prtpage prfile;*pcolumn3:row,*font=font12,*ALIGNMENT=*Left,str9;
          Pack Cell,"D",Str5        
          setprop sheet.range(Cell),*Value=numnew,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"
                  clear str9
                    move qtynew to str9
                call trim using str9
                  prtpage prfile;*pcolumn4:row,*font=font12,*ALIGNMENT=*Left,str9;
          add c1 to RowCount
          Pack Cell,"E",Str5        
          setprop sheet.range(Cell),*Value=qtynew,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"
          add c1 to CellRowcnt
.excel


                add NUMOLD to OTOT1
                add QTYOLD to QTOT1
                add NUMNEW to OTOT2
                add QTYNEW to QTOT2

.====================================================================================
                if (rptflag = c2)
                        add numold to TMPOTOT1
                        add qtyold to TMPQTOT1
                        add numnew to TMPOTOT2
                        add qtynew to TMPQTOT2
                endif
.====================================================================================
                move c1 to FRSTFLG
          else
.============================================================================
          call      Debug
                    if (rptflag = c2)
                    if (qtyold <> c0 | numold <> c0)
.                             if (rptflag = c2)
                                        add numold to TMPOTOT1
                                        add qtyold to TMPQTOT1
                                        clear str9
                              move TMPOTOT1 to str9
                                        call trim using str9
                              prtpage prfile;*pcolumn1:row,*font=font12,*ALIGNMENT=*Left,str9;
                              Pack Cell,"B",Str5        
                              setprop sheet.range(Cell),*Value=tmpotot1,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"

                              clear str9
                                        move TMPQTOT1 to str9
                                        call trim using str9
                                      prtpage prfile;*pcolumn2:row,*font=font12,*ALIGNMENT=*Left,str9;
                                        Pack Cell,"C",Str5        
                                        setprop sheet.range(Cell),*Value=tmpqtot1,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"
                                      add NUMold to OTOT1
                                        add QTYold to QTOT1

.                                       endif
                          endif
                          if (qtynew <> c0 |numnew <> c0)
.                           if (rptflag = c2)
                                        add numnew to TMPOTOT2
                                        add qtynew to TMPQTOT2
                                        clear str9
                              move TMPOTOT2 to str9
                              call trim using str9
                                        prtpage prfile;*pcolumn3:row,*font=font12,*ALIGNMENT=*Left,str9;
                                        Pack Cell,"D",Str5        
                                        setprop sheet.range(Cell),*Value=TMPOTOT2,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"
                              clear str9
                              move TMPQTOT2 to str9
                                        call trim using str9
                              prtpage prfile;*pcolumn4:row,*font=font12,*ALIGNMENT=*Left,str9;
                              Pack Cell,"E",Str5        
                              setprop sheet.range(Cell),*Value=TMPQTOT2,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"
                                      add NumNew to OTOT2
                              add QtyNew to QTOT2

.                             endif
                         endif
.
                else
.============================================================================
          call      Debug
                    move C0 to FRSTFLG
                    clear str9
                    move numnew to str9
                    call trim using str9
                    prtpage prfile;*pcolumn3:row,*font=font12,*ALIGNMENT=*Left,b9;
                    prtpage prfile;*pcolumn3:row,*font=font12,*ALIGNMENT=*Left,str9;
                    Pack Cell,"D",Str5        
                    setprop sheet.range(Cell),*Value=numnew,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"
                    clear str9
                          move qtynew to str9
                  call trim using str9
                    prtpage prfile;*pcolumn4:row,*font=font12,*ALIGNMENT=*Left,b9;
                    prtpage prfile;*pcolumn4:row,*font=font12,*ALIGNMENT=*Left,str9;
                    Pack Cell,"E",Str5        
                    setprop sheet.range(Cell),*Value=qtynew,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"
                          add NUMNEW to OTOT2
                  add QTYNEW to QTOT2
                endif
.============================================================================
          endif
                move mcomp to holdcomp
Row2
.       if (ROWCOUNT = "50")
          if (ROWCOUNT = "50" | ROWCOUNT = "51")
.===========================================================
.Play with to see why missing records when running
                     if (FRSTFLG = C1)
                 READ      INFILE,SEQ;outstr,OSLS,MCOMP,OODTEM,OODTEY,NUMOLD,QTYOLD,NUMNEW,QTYNEW
                                   if over
                                             goto ENDLOOP
                                   endif
                           Match MCOMP to HOLDCOMP
                                   if equal
                                                  goto PAGEREAD
                                     endif
                     endif
.===========================================================
                 move "10200",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.63 REPLACED LOGIC
.                prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News/CA";
                    prtpage   prfile;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.63 REPLACED LOGIC
                 goto NewPage
       endif

        repeat
.===================================================================
       if (rowcount = c0)
                goto  print11
.                pause c10
       endif
ENDLOOP
       if (ROWCOUNT < "49")
                 goto totals
       else

.Added to correct page # and page label for next to last page
.==========================================================================================
                 move "10200",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page'#' ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,*ll,PgCnt;
.START PATCH 1.63 REPLACED LOGIC
.                prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News/CA";
                    prtpage   prfile;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.63 REPLACED LOGIC
.===========================================================================================
               move c1 to newpg
               add  c1 to pgcnt
.               prtpage prfile;*NEWPAGE:
.                              *UNITS=*HIENGLISH:
.                                    *ORIENT=*PORTRAIT;
               goto Totals
       endif

TOTALS
       if (newpg = c1)
                 move srow,row
       endif

               add     eightlpi,row
               add     "50" to row
               prtpage prfile;*pcolumn1:row,*pensize=10,*line=7600:row;
               add     "30" to row
               prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Grand Total";
          Move      str5,str5a
          add  c2,CellRowCnt
          Move CellRowCnt,Str5
          call trim using str5
          Pack Cell,"A",Str5
          setprop sheet.range(Cell),*Value="Grand Total"
               clear str9
               move OTOT1 to str9
               call trim using str9
               prtpage prfile;*pcolumn1:row,*font=font12,*ALIGNMENT=*Left,*boldon,str9,*boldoff;
          Pack Cell,"B",Str5
          pack      str25 from "=Sum(B10:B",str5a,")"
          setprop sheet.range(Cell),*Value=STR25,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"
               clear str9
               move QTOT1 to str9
               call trim using str9
               prtpage prfile;*pcolumn2:row,*font=font12,*ALIGNMENT=*Left,*boldon,str9,*boldoff;
          Pack Cell,"C",Str5
          pack      str25 from "=Sum(C10:C",str5a,")"
          setprop sheet.range(Cell),*Value=STR25,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"
               clear str9
               move OTOT2 to str9
               call trim using str9
               prtpage prfile;*pcolumn3:row,*font=font12,*ALIGNMENT=*Left,*boldon,str9,*boldoff;
          Pack Cell,"D",Str5
          pack      str25 from "=Sum(D10:D",str5a,")"
          setprop sheet.range(Cell),*Value=STR25,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"
               clear str9
               move QTOT2 to str9
               call trim using str9
               prtpage prfile;*pcolumn4:row,*font=font12,*ALIGNMENT=*Left,*boldon,str9,*boldoff;
          Pack Cell,"E",Str5
          pack      str25 from "=Sum(E10:E",str5a,")"
          setprop sheet.range(Cell),*Value=STR25,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0"
          sheet.range("A6:Z9999").Columns.Autofit               

          
.=============================================================
.Footer for Last Page
Print2
                 move "10200",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,*ll,PgCnt;
.START PATCH 1.63 REPLACED LOGIC
.                prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News/CA";
                    prtpage   prfile;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.63 REPLACED LOGIC
               PRTCLOSE prfile
.                        if (bosflag = c2)
                                DISPLAY   *P10:14,"Printing ",str25
..                                        PRTPLAY str40,"\\NINs2\Laser6"
..                                       PRTPLAY str40,"\\NINs2\Laser6"

.==============================================================================
.added to erase spoolfile 10/15/01
.                                erase str40
.==============================================================================
.                        else
.                                DISPLAY   *P10:14,"Printing ",str25
..                                        PRTPLAY str40,"Laser6"
..                                       PRTPLAY str40,"Laser6"
..                                erase str40
.                        endif
.test
               if (rptflag = c1)
                              goto print11
               endif
.test

               if (rptflag = c2)
                              goto print11
               endif
               If (Bothflg = YES)
                        DISPLAY   *P10:14,"Doing All Sales - YTD "
                        move c2 to rptflag
                        move NO to BothFLG
                        clear n2
                        goto again
               endif
End1
        erase str40
************************************************************************************
        DISPLAY   *P10:14,"All Done!!!!!!!! "
        pause      c2
        DISPLAY   *P10:14,"Adios Amigo!!!!!!!!!! "
        pause      c2
              clear   taskname
              setprop ex,*DisplayAlerts=OFalse


              setprop ex,*DefaultFilePath=taskname
              bump            timestamp,8
              Clear           Taskname
              pack            Taskname,"c:\work\Nord0030","_",cc,yr1,mo1
./////
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
.                trap    TrapCampaignObject if Object
                book.saveas giving N9 using *Filename=taskname
                trapclr Object

.CleanUp
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.do some housekeeping first

        destroy Rowcol
        destroy sheet
        destroy sheets
        destroy book
        destroy books
        Destroy xlRowHeight
        Destroy xlColumnWidth
        Destroy OTRUE
        Destroy OFALSE
        Destroy TopMargin
        Destroy BottomMargin
        Destroy LeftMargin
        Destroy RightMargin
              setprop ex,*DisplayAlerts=OFALSE
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        ex.quit
        destroy ex

EOJ
          Move      "Creques@nincal.com",mailfrom
          Pack      MailAttach from taskname,".xlsx"        
          move      mailattach,taskname
          move      "dherric@nincal.com",Mailto
CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,taskname,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
          call      SendMail
          
        NORETURN
        shutdown  "cls"
          STOP
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    move      "500",str4      .=5 sec
                    call      Waitin using str4

.                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Nord0030 - ",taskname
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
.                   Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    taskname,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    NORETURN
                    shutdown  "cls"
                    STOP

                    endif
          
                    goto      checkfile

SheetSetup
          call debug
          sheets.add using *After=sheet
          Getprop sheets,*Count=sheetindex
          sheets.item giving sheet using sheetindex
          Setprop sheet,*Name=Sales
          move      Sales,HoldSales

             Sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,144,45
.                    endif
.end patch 1.15
                    setprop sheet.range("d1:z250").Font,*Name="Times New Roman", *Size=11
.END PATCH 1.04 REPLACED LOGIC
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*TopMargin=TopMargin
              setprop sheet.PageSetup,*BottomMargin=BottomMargin
              setprop sheet.PageSetup,*FooterMargin=TopMargin
              setprop sheet.PageSetup,*LeftMargin=LeftMargin
              setprop sheet.PageSetup,*RightMargin=RightMargin

              setprop         sheet.range("a1:a1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("c1:c1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("f1:f1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("k1:k1").Columns,*ColumnWidth=xlColumnWidth
              DESTROY         xlColumnWidth
              create xlColumnWidth,VarType=VT_R8a,VarValue="12.00"
              setprop         sheet.range("d1:d1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("g1:g1").Columns,*ColumnWidth=xlColumnWidth
              DESTROY         xlColumnWidth
              create xlColumnWidth,VarType=VT_R8a,VarValue="14.75"
              setprop         sheet.range("e1:e1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("h1:h1").Columns,*ColumnWidth=xlColumnWidth
              setprop         sheet.range("J1:O1").Columns,*ColumnWidth=xlColumnWidth
              DESTROY         xlColumnWidth
              create xlColumnWidth,VarType=VT_R8a,VarValue="5.13"
              setprop         sheet.range("I1:I1").Columns,*ColumnWidth=xlColumnWidth

              setprop sheet.range("A6","A6"),*Value=Sales,*HorizontalAlignment=xlLeft
              setprop sheet.range("A6:A6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A6:A6").Font,*Bold="True"

          pack str15 with curmo," -YTD"

              setprop sheet.range("B6","B6"),*Value=Str15,*HorizontalAlignment=xlLeft
              setprop sheet.range("B6:B6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("B6:B6").Font,*Bold="True"
          sheet.range("B6:C6").Merge              

              setprop sheet.range("A8","A8"),*Value="Client",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("A8:A8").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("A8:A8").Font,*Bold="True"
          CLEAR   STR11
          PACK    STR11,OYR1,TITLEYR2
              setprop sheet.range("B8","B8"),*Value=str11,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("B8:B8").Font,*Name="Times New Roman", *Size=12
              setprop sheet.range("B8:B8").Font,*Bold="True"

        CLEAR   STR8
        PACK    STR8,QYR1,TITLEYR2
              setprop sheet.range("C8","C8"),*Value=str8,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("C8:C8").Font,*Name="Times New Roman", *Size=12
              setprop sheet.range("C8:C8").Font,*Bold="True"

        CLEAR   STR11
        PACK    STR11,OYR1,TITLEYR1
              setprop sheet.range("D8","D8"),*Value=str11,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("D8:D8").Font,*Name="Times New Roman", *Size=12
              setprop sheet.range("D8:D8").Font,*Bold="True"

        CLEAR   STR8
        PACK    STR8,QYR1,TITLEYR1
              setprop sheet.range("E8","E8"),*Value=str8,*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("E8:E8").Font,*Name="Times New Roman", *Size=12
              setprop sheet.range("E8:E8").Font,*Bold="True"

                   
          Move "10" to CellRowCnt

          Return

         INCLUDE   NORDIO.inc
.patch1.62
                                        include   compio.inc
                                        include   cntio.inc
.        INCLUDE   NMLRIO.inc
.patch1.62
         INCLUDE   COMLOGIC.inc
.patch1.62
.         include   NBRKIO.inc
.patch1.62
Warning
               alert caution,"There are no matching records!",result,"No Record"

