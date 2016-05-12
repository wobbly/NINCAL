.NDAT0023 - PRINT DATACARD TRIPLEX BILLING INFO
.........
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         include   consacct.inc
         INCLUDE   NDAT3DD.inc
         INCLUDE   NDATDD.inc
.begin patch 1.1
           include    nxrfdd.inc
           include norddd.inc
           include    compdd.inc
           include    cntdd.inc
              INCLUDE         NSEL2DD.INC
form52     form       5.2
.end patch 1.1
DATE     DIM       8
.begin patch 1.0
.OUTFILE  IFILE     KEYLEN=56
OUTFILE  IFILE     KEYLEN=6,fixed=500
.end patch 1.0
.............................
.LSTNUM,MLSTNAME,Outcomp,Outmlr,outppm,status,OODTEM,OODTED,OODTEC,OODTEY,ELSTCDE,NDATTDMC,ndat3cde,ndatdolc,ndat3exh,ndat3ex1,ndat3ex2,Ndat3ExRt
.ndat3key  dim           6                   1-6         list number
.mlstname  dim           75                  7-81
.begin patch 1.1   .
Outcomp    Dim        6                      82-87
outmlr     dim        4                      88-91
outppm     form       5.2                    92-99
.STATUS    dim        1                     100-100
.orderdate            8                     101-108
.ELSTCDE              1                     109-109
.end patch 1.1   .
.ndatttdmc  dim         1                   110-110
.ndat3own  dim          1                   111-111
.ndat3cde  dim          1                   112-112
.ndatdolc  dim          1                   113-113
.ndat3exh  dim        1                     114-114  "Y"=We bill charges on Exchange
.ndat3ex1  dim        8                                115-122   date we started billing exch fee mmddccyy
.ndat3ex2  dim        8                                123-130   Date we stopped billing exch fee mmddccyy
.Ndat3ExRt Form       3.2                   131-136          Management Exchange Rate per thousand
.begin patch 1.1   .
Count1     Form       5
Count2     Form       5
Count3     Form       5
.end patch 1.1
.
LINES    FORM      2
BILLINFO INIT      "                   "
release  init      "1.21"          DLH Minimum
Reldate   Init      "2015 June 03"
.release  init      "1.20"          DLH add SB number and name from datacard. Read ndat3 by key after reading managed lists. add logic prog 39 SB name into spreadsheet.
..                                    second loop append lists that are no longer managed but have billing info
..                                    cleanup spreadsheet appearance. delete working files. use data manager. Gui fi 
.Reldate   Init      "2014 October 09"
.release  init      "1.10"          DLH read list mlr xref add mailer # to report, read order file by mailer get last exchange $
.                                  and all files in nindat3
.                                change program to visible, display process counters, and output file location when done
.Reldate   Init      "2014 January 31"
.release  init      "1.01"          DLH Look at all managed files
..                                  and all files in nindat3
.Reldate   Init      "2013 September 13"
.release  init      ".03"          ASH 29OCT2000 NEW SERVER ADDED
.release  init      ".02"          DLH 28feb96
.RELEASE  INIT      ".01"          DLH 09MAR92.
.
OUTSB      Dim        55

.begin patch 1.0
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
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
N34     form    3.4
N92     form    9.2
MailDate dim    4
VT_BOOL EQU 11          .Boolean
OTRUE   variant
OFALSE  variant
xlLeft                        integer 4,"0xffffefDD"
xlTop                         integer 4,"0xffffefc0"
AlignRight                    integer 4,"0xffffefc8"
xlAlignCenter                 integer 4,"0xffffeff4"
xlBottom                      integer      4,"0xffffeff5"
XlLineStyleDBl                Integer 4,"0xffffefe9"                         .line style double
XLShiftToLeft       Variant                        .range delete shift to left
XLShiftUp Variant                        .range delete shift Up     
xlLandscape integer 4,"0x2"                     .2

xlInsideHorizontal            integer 4,"0x12"                .Borders inside defined range
xlInsideVertical              integer 4,"0x11"                .Borders inside defined range
XlEdgeRight                   integer 4,"0x10"                .Borders right edge of defined range
VT_R8         EQU 5           .Double - 8 byte Real
VT_R8a         EQU 5           .Double - 8 byte Real
xlRowHeight   variant
xlColumnWidth variant
xlColumnWidthA variant
xlColumnWidthB variant
xlColumnWidthCats variant
TopMargin     variant
BottomMargin  variant
LeftMargin     variant
RightMargin     variant
.begin patch 4.0
HPageBreaks                             AUTOMATION
HPageBreak                              AUTOMATION
VT_I4                         EQU                  3           .4 byte integer
Zoom70                        VARIANT
ZoomRowMax            form    9      //Max row before a soft page break
ZoomRowMaxPage1   form        9      //Max row before a soft page break on page 1
CellRowCnt          FORM      "46"
CellRowCnt1         FORM      "54"
CurCellNum FORM 5
SheetIndex                              variant
sheetcount          integer   1

Row1          form            5
Row2          Form            5
DimRow1       Dim             5
DimRow2       Dim             5
ExRange1      Dim             7
ExRange2      Dim             7
ExRange3      Dim            16
range1    automation
.end patch 1.0
WithFlag   Init       "N"        .default do not exclude withdrawn files
ManFlag    Init       "N"        .default do not exclude Datacards that are no longer managed
Timer   Timer
.Colors
white   color
grey    color
RED     COLOR
BLACK   COLOR

.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font

.Set Up Menu Bar
mFile    menu
mEdit    menu
mHelp    menu
mOptions menu
.Set Up SubMenu for Options

.Present Data for Menu Bar
FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options"
HData   init    "&Help;&About"


.Define Collections for Object Colors
ColText Collection
ColBack Collection

.Define Colors for Each Object
FTC     color
BGC     color

colornum dim   9(2)
Fred     form    3
Fgreen   form    3
Fblue    form    3
colorflag form   1
.Set Vars used for About Box
          move    "NDAT0023.PLS",Wprognme
          move    "Additional LM Billing Reporting",Wfunction
          move    "David Herrick",Wauthor
          move    Release,Wrelease
          move    Reldate,Wreldate

         MOVE      "NDAT0023" TO PROGRAM
         MOVE      C1 TO NDATPATH
         MOVE      "PRINT Exch BILLING STATUS" TO STITLE
         MOVE      "Names in the News" TO COMPNME
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         XIF
.begin patch 1.1
           winshow
.end patch 1.1
.        CALL      PAINT
.START PATCH .03 REPLACED LOGIC
.         splopen   "g:\data\tdmcrun.lst"
.begin Patch 1.0
.         PACK      TASKNAME,NTWKPATH1,"TDMCRUN.LST"
.         splopen   TASKNAME
..END PATCH .03 REPLACED LOGIC
.         MOVE      "PRT ONLY" TO PF3
.end Patch 1.0
.         MOVE      "ABORT" TO PF5
.         TRAP      ABORT IF F5
.         TRAP      PRTONLY IF F3
.         CALL      FUNCDISP
.begin Patch 1.0
.         KEYIN     *P1:24,*EL,"You have 60 seconds to select Print only":
.                   *B,*T60,STR1,*P1:24,*EL;
.         CLEAR     PF3
.end Patch 1.0
.         CALL      FUNCDISP
.Declare forms, Always declare child forms first
mss1      plform  Error
abt       plform  About
x         plform  Ndat0023
          winhide
.Load Forms, Always load parent form first
          formload x
          formload abt
          formload mss1

          CREATE  TIMER,1200     .2 minutes
          ACTIVATE TIMER,Timeout,RESULT
.Create Menus
          create  NDat0022;mFile,FData
          create  NDat0022;mEdit,EData,mFile
          create  NDat0022;mOptions,OData,mEdit
          create  NDat0022;mHelp,HData,mOptions

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
.Only a SubMenu under these
        activate mOptions
        activate mHelp,HelpGo,result

.Create Colors for EditText Inquiry
          create  white=*white
          create    grey=220:220:220
          create  RED=*RED
          create  black=*black

.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic


.START PATCH .03 REPLACED LOGIC
.         PREPARE   OUTFILE,"g:\data\NDAT23","g:\data\ndat23","56","68"
.begin Patch 1.0
.         PACK      STR35,NTWKPATH1,"NDAT23"
.         PACK      STR45,NTWKPATH1,"NDAT23"
.         PREPARE   OUTFILE,STR35,STR45,"76","100"
.end Patch 1.0
.END PATCH .03 REPLACED LOGIC
        setfocus ndat0023ButtonGo
        loop
                waitevent
                setitem timer,0,1200   .reset to 2 minutes
        repeat


.begin Patch 1.0
RunIt
          move      c4,ndatpath
          move      "000000",ndatfld
         call       ndattst
         PACK      STR35,NTWKPATH1,"NDAT23"
         PACK      STR45,NTWKPATH1,"NDAT23|nins1:502"
.begin patch 1.1
.         PREPARE   OUTFILE,STR35,STR45,"6","110"
         PREPARE   OUTFILE,STR35,STR45,"6","197",exclusive
.end patch 1.1
Loop1     Loop
          call      Ndatks
          until     over
.begin patch 1.1
           add        c1,count1
.          display    *p10:10,"Managed Lists reviewed: ",count1
           move       count1,str5
           setitem    ndat0023StatText2,0,str5
           if         (lstnum = "008603" or lstnum = "002210" or lstnum = "074891")
           call       debug
           endif
           if         (Elstcde = "N")
           goto       loop1
           endif
           packkey    NXRFFLD,lstnum
           call       getxref
           pack      COMPFLD,DATFUL         
           rep       zfill,COMPFLD          
           Move      C1,COMPPATH
.end patch 1.1
           Clear      CompComp
           Clear      OutSB
           move      "Verify-COMPKEY",Location
           pack      KeyLocation,COMPFLD          
           call      COMPKEY
           if         not over
           move       CompComp,OutSB
           endif
.end Patch 1.0
           call       clearflds
           packkey    NDAT3FLD,Lstnum
           call       Ndat3key
           if         (Withflag = Yes)     .take only currently active Files
                      if         (status = "W" or Status = "T")
                      goto       skip
                      endif
           endif      
           
.           WRITE     OUTFILE,LSTNUM;LSTNUM,MLSTNAME,Outcomp,Outmlr,outppm,status,OODTEM,OODTED,OODTEC,OODTEY,ELSTCDE,DATFUL,OutSB,NDATTDMC,ndat3cde,ndatdolc,ndat3exh,ndat3ex1,ndat3ex2,Ndat3ExRt
           WRITE     OUTFILE,LSTNUM;LSTNUM,MLSTNAME,Outcomp,Outmlr,outppm,status,OODTEM,OODTED,OODTEC,OODTEY,ELSTCDE,DATFUL,OutSB,NDATTDMC,ndat3cde,ndatdolc,ndat3exh,ndat3ex1,ndat3ex2,Ndat3ExRt,Ndat3Min
skip
           repeat
           Move       c0,NDAT3FLG                      .force reopen of the file
LOOP      Loop
          CALL      NDAT3SEQ
.begin Patch 1.0
          until     over
.         GOTO      PASSTWO IF OVER
           add        c1,count2
.          display    *p10:12,"List instructions reviewed: ",count2
           move       count2,str5
           setitem    ndat0023StatText3,0,str5

           if         (lstnum = "008603" or lstnum = "002210" or lstnum = "074891")
           call       debug
           endif
          call      trim using ndat3key
          if        (ndat3key <> "")
          packkey   str6 from ndat3key
.          call      debug
          read      outfile,ndat3key;;
                    if        over
.          MOVE      NDAT3KEY TO NDATFLD
                    packkey   ndatfld,ndat3key
                    move      c1,ndatpath
                    CALL      NDATKEY
                              IF        OVER
                              APPEND    "NO LIST FOUND!!!!" TO MLSTNAME
                              APPEND    NDAT3KEY TO MLSTNAME
                              RESET     MLSTNAME
                              move      ndat3key,lstnum          
                              ENDIF
.begin patch 1.1
                                 packkey    NXRFFLD,ndat3key
                                 call       getxref
.end patch 1.1
                      if         (Manflag = Yes)     .take only currently managed files
                                 if         (elstcde <> "C" and elstcde <> "P")
                                 goto       skip1
                                 endif
                      endif      
                      if         (Withflag = Yes)     .take only currently active Files
                                 if         (status = "W" or Status = "T")
                                 goto       skip1
                                 endif
                      endif      
.                    WRITE     OUTFILE,LSTNUM;LSTNUM,MLSTNAME,Outcomp,Outmlr,outppm,status,OODTEM,OODTED,OODTEC,OODTEY,ELSTCDE,DATFUL,OutSB,NDATTDMC,ndat3cde,ndatdolc,ndat3exh,ndat3ex1,ndat3ex2,Ndat3ExRt
                    WRITE     OUTFILE,LSTNUM;LSTNUM,MLSTNAME,Outcomp,Outmlr,outppm,status,OODTEM,OODTED,OODTEC,OODTEY,ELSTCDE,DATFUL,OutSB,NDATTDMC,ndat3cde,ndatdolc,ndat3exh,ndat3ex1,ndat3ex2,Ndat3ExRt,Ndat3Min
                    Else
.                    read      outfile,ndat3key;Ndat3key,MLSTNAME,Outcomp,Outmlr,outppm,status,OODTEM,OODTED,OODTEC,OODTEY,ELSTCDE,DATFUL,OutSB;
.                    Update     OUTFILE;Ndat3key,MLSTNAME,Outcomp,Outmlr,outppm,status,OODTEM,OODTED,OODTEC,OODTEY,ELSTCDE,DATFUL,OutSB,NDATTDMC,ndat3cde,ndatdolc,ndat3exh,ndat3ex1,ndat3ex2,Ndat3ExRt
                    endif 
          endif
skip1
          Repeat
.          ADD       C1 TO N4
.         DISPLAY   *P10:10,*EL,"PASS ONE, RECORD ",N4
.         MOVE      NDAT3KEY TO NDATFLD
.         CALL      NDATKEY
.         IF        OVER
.         APPEND    "NO LIST FOUND!!!!" TO MLSTNAME
.         APPEND    NDAT3KEY TO MLSTNAME
.         RESET     MLSTNAME
.         ENDIF
.         WRITE     OUTFILE,MLSTNAME;NDAT3KEY,MLSTNAME,NDATTDMC,ndat3exh
.         GOTO      LOOP
.end Patch 1.0
PASSTWO  CLOSE     OUTFILE
PASSTWOA OPEN      OUTFILE,"NDAT23|nins1:502",read
.begin Patch 1.0
         CALL      HEADER
         MOVE      C0 TO N4
READKS    Loop
.begin patch 1.1
.          READKS    OUTFILE;NDAT3KEY,MLSTNAME,NDATTDMC,ndat3cde,ndatdolc,ndat3exh,ndat3ex1,ndat3ex2,Ndat3ExRt
.          READKS    OUTFILE;NDAT3KEY,MLSTNAME,Outcomp,Outmlr,outppm,status,OODTEM,OODTED,OODTEC,OODTEY,ELSTCDE,DATFUL,OutSB,NDATTDMC,ndat3cde,ndatdolc,ndat3exh,ndat3ex1,ndat3ex2,Ndat3ExRt
          READKS    OUTFILE;NDAT3KEY,MLSTNAME,Outcomp,Outmlr,outppm,status,OODTEM,OODTED,OODTEC,OODTEY,ELSTCDE,DATFUL,OutSB,NDATTDMC,ndat3cde,ndatdolc,ndat3exh,ndat3ex1,ndat3ex2,Ndat3ExRt,Ndat3Min
.end patch 1.1
.READKS   READKS    OUTFILE;NDAT3KEY,MLSTNAME,NDATTDMC,ndat3exh
          Until over
.         GOTO      DONE IF OVER
           add        c1,count3
.          display    *p10:14,"Compiled data output: ",count3
           move       count3,str5
           setitem    ndat0023StatText4,0,str5
          add       c1,row1
          add       c1,row2

          Move            Row1,DimRow1
                    call            Trim using DimRow1
                    Move            Row2,DimRow2
                    call            Trim using DimRow2
.                   
                    pack            Exrange1 from "B",DimRow1
                    pack            Exrange2 from "B",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
          
                    setprop sheet.range(ExRange1,ExRange1),*Value=NDAT3KEY,*NumberFormat="######0"
                    pack            Exrange1 from "C",DimRow1
                    pack            Exrange2 from "C",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=Mlstname
                    pack            Exrange1 from "D",DimRow1
                    pack            Exrange2 from "D",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    CLEAR     BILLINFO
                      call       trim using NdatTdmc
                    CMATCH    "B" TO NDATTDMC
                    CALL      BOTH IF EQUAL
                    CMATCH    "R" TO NDATTDMC
                    CALL      RENT IF EQUAL
                    CMATCH    "E" TO NDATTDMC
                    CALL      EXCH IF EQUAL
                    setprop sheet.range(ExRange1,ExRange1),*Value=Billinfo
.          setprop sheet.range(ExRange1),*Value="SB Key: ' ' = triplex, 'A'=anacapa, 'F'=Fide, 'J'=Antares, 'M'=MMI, 'P'=Pidi, 'R'= Frontline, 'K'=MKGT"
                    
                    pack            Exrange1 from "E",DimRow1
                    pack            Exrange2 from "E",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
.                    setprop sheet.range(ExRange1,ExRange1),*Value=ndat3cde
                          if     (ndat3cde = "K")                          
                    setprop sheet.range(ExRange1,ExRange1),*Value="MKGT"
                      elseif     (ndat3cde = "R")                          
                    setprop sheet.range(ExRange1,ExRange1),*Value="Frontline"
                      elseif     (ndat3cde = "P")                          
                    setprop sheet.range(ExRange1,ExRange1),*Value="PIDI"
                      elseif     (ndat3cde = "M")
                    setprop sheet.range(ExRange1,ExRange1),*Value="MMI"
                      elseif     (ndat3cde = "J")
                    setprop sheet.range(ExRange1,ExRange1),*Value="Antares"
                      elseif     (ndat3cde = "F")
                    setprop sheet.range(ExRange1,ExRange1),*Value="Fide"
                      elseif     (ndat3cde = "A")
                    setprop sheet.range(ExRange1,ExRange1),*Value="Anacapa"
.                     elseif     (ndat3cde = "" or Ndat3cde = " ")
                      elseif     ((ndat3cde = "" or Ndat3cde = " ") & Ndattdmc <> " " & Ndattdmc <> "")
                    setprop sheet.range(ExRange1,ExRange1),*Value="InfoGrp"
                      endif
                    pack            Exrange1 from "F",DimRow1
                    pack            Exrange2 from "F",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=ndatdolc
                    pack            Exrange1 from "g",DimRow1
                    pack            Exrange2 from "g",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=ndat3exh
.start date
                    pack            Exrange1 from "h",DimRow1
                    pack            Exrange2 from "h",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    unpack    ndat3ex1 into mm,dd,cc,yy
                    pack      str10 from mm,slash,dd,slash,cc,yy
                    setprop sheet.range(ExRange1,ExRange1),*Value=str10,*HorizontalAlignment=AlignRight,*NumberFormat="MMMM DD, YYYY"
                    pack            Exrange1 from "i",DimRow1
                    pack            Exrange2 from "i",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
.End date
                    unpack    ndat3ex2 into mm,dd,cc,yy
                    pack      str10 from mm,slash,dd,slash,cc,yy
                    setprop sheet.range(ExRange1,ExRange1),*Value=str10,*HorizontalAlignment=AlignRight,*NumberFormat="MMMM DD, YYYY"
                    pack            Exrange1 from "j",DimRow1
                    pack            Exrange2 from "j",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=Ndat3ExRt,*NumberFormat="$0.00"
.begin patch 1.1
.begin patch 1.21  Min added all moved over one column
.temp code 
                    if           (Ndat3Min = c0)
                    move         "15.00",ndat3min
                    endif
.temp code 
                    pack            Exrange1 from "k",DimRow1
                    pack            Exrange2 from "k",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=Ndat3Min,*NumberFormat="$0.00"


                    pack            Exrange1 from "l",DimRow1
                    pack            Exrange2 from "l",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=outcomp

                    pack            Exrange1 from "m",DimRow1
                    pack            Exrange2 from "m",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=outmlr

                    pack            Exrange1 from "n",DimRow1
                    pack            Exrange2 from "n",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=outppm,*NumberFormat="$0.00"
         
                  PACK      str10 FROM OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
         

                    pack            Exrange1 from "o",DimRow1
                    pack            Exrange2 from "o",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=str10,*HorizontalAlignment=AlignRight,*NumberFormat="MMMM DD, YYYY"

                    pack            Exrange1 from "p",DimRow1
                    pack            Exrange2 from "p",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=status

                    pack            Exrange1 from "q",DimRow1
                    pack            Exrange2 from "q",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=ELSTCDE

                    pack            Exrange1 from "r",DimRow1
                    pack            Exrange2 from "r",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=DatFul

                    pack            Exrange1 from "s",DimRow1
                    pack            Exrange2 from "s",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=OUTSB
.end patch 1.21
.end patch 1.1


.         ADD       C1 TO N4
.         DISPLAY   *P10:12,*EL,"PASS TWO, RECORD ",N4
.         CLEAR     BILLINFO
.         CMATCH    "B" TO NDATTDMC
.         CALL      BOTH IF EQUAL
.         CMATCH    "R" TO NDATTDMC
.         CALL      RENT IF EQUAL
.         CMATCH    "E" TO NDATTDMC
.         CALL      EXCH IF EQUAL
..
.         COMPARE   "58" TO LINES
.         CALL      HEADER IF NOT LESS
.                             match     "005172" to ndat3exh
.                             goto      Plan if equal
.                             match     yes to ndat3exh
.                             goto      readks if not equal
.                             reset     rateten
.                             scan      ndat3key in rateten
.                             if        equal
.                             move      "$10.00" to billinfo
.                             else
.                             move      "$2.00" to billinfo
.                             endif
..
.         PRINT     NDAT3KEY,B3,MLSTNAME,B1,BILLINFO
.         ADD       C1 TO LINES
.         GOTO      READKS
          repeat
          goto      Done
.end patch 1.0         
BOTH     MOVE      "BOTH" TO BILLINFO
         RETURN
RENT     MOVE      "RENT/SPLIT" TO BILLINFO
         RETURN
EXCH     MOVE      "EXCH/Only" TO BILLINFO
         RETURN
Dolr     append    " $/Date" to billinfo
         return
.begin patch 1.0
Header
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
        create xlColumnWidth,VarType=VT_R8A,VarValue="0.46"
        create xlColumnWidthA,VarType=VT_R8,VarValue="64.00"
        create xlColumnWidthB,VarType=VT_R8,VarValue="12.00"
        create xlColumnWidthCats,VarType=VT_R8a,VarValue="24.00"
        create          OTRUE,VarType=VT_BOOL,VarValue=1
        create          OFALSE,VarType=VT_BOOL,VarValue=0
        create          TopMargin,VarType=VT_R8,VarValue="18"                       Roughly equals .25 inches:  18 * 1.388 = 25
        create          BottomMargin,VarType=VT_R8,VarValue="18"
        create          LeftMargin,VarType=VT_R8,VarValue="5"
        create          RightMargin,VarType=VT_R8,VarValue="5"                Roughly equals .0694 inches:  5 * 1.388 = 6.94
.        setprop ex,*Visible="True"
              sheet.Shapes.AddPicture using "\\nins1\e\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,144,45

              setprop sheet.range("a1:z250").Font,*Name="Times New Roman", *Size=11
              setprop sheet.PageSetup,*Orientation=xlLandscape
              setprop sheet.PageSetup,*TopMargin=TopMargin
              setprop sheet.PageSetup,*BottomMargin=BottomMargin
              setprop sheet.PageSetup,*FooterMargin=TopMargin
              setprop sheet.PageSetup,*LeftMargin=LeftMargin
              setprop sheet.PageSetup,*RightMargin=RightMargin
              setprop sheet.range("b6","B6"),*Value="List ##",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b6:b6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b6:b6").Font,*Bold="True"

              setprop sheet.range("C6","C6"),*Value="Master List Name",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("C6:C6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("C6:C6").Font,*Bold="True"

              setprop sheet.range("d4","d4"),*Value="We Bill SB Fees",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("d4:d4").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("d4:d4").Font,*Bold="True"
              setprop sheet.range("d5","d5"),*Value="Running Charges",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("d5:d5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("d5:d5").Font,*Bold="True"
              setprop sheet.range("d6","d6"),*Value="and Selects",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("d6:d6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("d6:d6").Font,*Bold="True"

              setprop sheet.range("e4","e4"),*Value="Prog.",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("e4:e4").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("e4:e4").Font,*Bold="True"
              setprop sheet.range("e5","e5"),*Value="39",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("e5:e5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("e5:e5").Font,*Bold="True"
              setprop sheet.range("e6","e6"),*Value="S.B.",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("e6:e6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("e6:e6").Font,*Bold="True"

              setprop sheet.range("F6","f6"),*Value="Bill Sel.",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("f6:f6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("f6:f6").Font,*Bold="True"
              setprop sheet.range("g6","g6"),*Value="Y= We Bill LM Exch",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("g6:g6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("g6:g6").Font,*Bold="True"

              setprop sheet.range("h5","h5"),*Value="Start Date for",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("h5:h5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("h5:h5").Font,*Bold="True"
              setprop sheet.range("h6","h6"),*Value="Management Fees",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("h6:h6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("h6:h6").Font,*Bold="True"

              setprop sheet.range("i5","i5"),*Value="End Date for",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("i5:i5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("i5:i5").Font,*Bold="True"
              setprop sheet.range("i6","i6"),*Value="Management Fees",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("i6:i6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("i6:i6").Font,*Bold="True"
.begin patch 1.1
              setprop sheet.range("J4","J4"),*Value="List Management",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("J4:J4").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("J4:J4").Font,*Bold="True"
              setprop sheet.range("J5","J5"),*Value="Exchange Fee",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("J5:J5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("J5:J5").Font,*Bold="True"
              setprop sheet.range("J6","J6"),*Value="to NIN",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("J6:J6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("J6:J6").Font,*Bold="True"
.begin patch 1.21
              setprop sheet.range("K6","K6"),*Value="Minimum",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("K6:K6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("K6:K6").Font,*Bold="True"

              setprop sheet.range("L6","L6"),*Value="Company ##",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("L6:L6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("L6:L6").Font,*Bold="True"

              setprop sheet.range("m6","m6"),*Value="Mailer ##",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("m6:m6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("m6:m6").Font,*Bold="True"

              setprop sheet.range("N4","N4"),*Value="Last Brokerage",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("N4:N4").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("N4:N4").Font,*Bold="True"
              setprop sheet.range("N5","N5"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("N5:N5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("N5:N5").Font,*Bold="True"
              setprop sheet.range("N6","N6"),*Value="order Price",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("N6:N6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("N6:N6").Font,*Bold="True"

              setprop sheet.range("O4","O4"),*Value="Last Brokerage",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("O4:O4").Font,*Oame="Times New Roman", *Size=14
              setprop sheet.range("O4:O4").Font,*Bold="True"
              setprop sheet.range("O5","O5"),*Value="Exchange",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("O5:O5").Font,*Oame="Times New Roman", *Size=14
              setprop sheet.range("O5:O5").Font,*Bold="True"
              setprop sheet.range("O6","O6"),*Value="Order Date",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("O6:O6").Font,*Oame="Times New Roman", *Size=14
              setprop sheet.range("O6:O6").Font,*Bold="True"

              setprop sheet.range("p5","p5"),*Value="List",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("p5:p5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("p5:p5").Font,*Bold="True"

              setprop sheet.range("p6","p6"),*Value="Status",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("p6:p6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("p6:p6").Font,*Bold="True"

              setprop sheet.range("q6","q6"),*Value="Managed",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("q6:q6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("q6:q6").Font,*Bold="True"


              setprop sheet.range("R5","R5"),*Value="Datacard",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("R5:R5").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("R5:R5").Font,*Bold="True"
              setprop sheet.range("q6","R6"),*Value="S.B.##",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("R6:R6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("R6:R6").Font,*Bold="True"

              setprop sheet.range("S5","S5"),*Value="Datacard",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("S5:S5").Font,*Name="Times New Soman", *Size=14
              setprop sheet.range("S5:S5").Font,*Bold="True"
              setprop sheet.range("S6","S6"),*Value="S.B.",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("S6:S6").Font,*Name="Times New Soman", *Size=14
              setprop sheet.range("S6:S6").Font,*Bold="True"
.end patch 1.21
.end patch 1.1

          move      c6,row1
          move      c7,row2


.HEADER   PRINT     *F,*L,*L:
.                   *1,"CONFIDENTIAL",*25,"TRIPLEX BILLING INFORMATION":
.                   *70,TODAY:
.                   *N,*N,*1,"LIST##",*10,"LIST DESCRIPTION",*66,"Price":
.                   *L
.         MOVE      C4 TO LINES
         RETURN
.end patch 1.0

plan         return

PRTONLY  TRAPCLR   F3
         NORETURN
         GOTO      PASSTWOA
ABORT    DISPLAY *P1:24,*EL,*B,"JOB ABORTED",*B,*W3
DONE     
.begin patch 1.0
.          PRINT     *F
.         splclose
          move      "Billing",prtname
          pack            Exrange1 from "B5"
          pack            Exrange2 from "R",DimRow2
          sheet.range(EXrange1,EXrange2).Columns.Autofit
         Setprop Sheet.Range("J1"),*ColumnWidth=xlColumnWidthCats

          add       c1,row1
          add       c1,row2
          Move            Row1,DimRow1
          Move            Row2,DimRow2
          pack            Exrange1 from "E",DimRow1
          pack            Exrange2 from "E",DimRow2
          pack            Exrange3,Exrange1,":",Exrange2
.          setprop sheet.range(ExRange1),*Value="SB Key: ' ' = triplex, 'A'=anacapa, 'F'=Fide, 'J'=Antares, 'M'=MMI, 'P'=Pidi, 'R'= Frontline, 'K'=MKGT"




              clear   taskname
              setprop ex,*DisplayAlerts=OFalse


              setprop ex,*DefaultFilePath=taskname
              bump            timestamp,8
              Clear           Taskname
              pack            Taskname,"\\nins1\e\data\",Prtname
./////
          pack      taskname,"c:\work\"                     ."
          setprop ex,*DefaultFilePath=taskname
          pack      taskname,"c:\work\",pRTNAME                       ."
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF

............................................
SaveAsFileNameSelect
          setmode *mcursor=*arrow
              clear   taskname
                              if        (#ver = c1)
                              pack            Taskname,"c:\work\",Prtname,".xlsx"
                              else
                              pack            Taskname,"c:\work\",Prtname,".xls"
                              endif                  
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
.                trap    TrapSaveAsObject if Object
.                book.saveas giving N9 using *Filename=taskname
.                trapclr Object
                              erase          taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                              trap    TrapSaveAsObject if Object
                              book.saveas giving N9 using *Filename=taskname
                              trapclr Object
.
                              setprop ex,*IgnoreRemoteRequests="False",*Interactive="FALSE"

..............................................................................................................
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
        Destroy xlColumnWidthCats
        Destroy OTRUE
        Destroy OFALSE
        Destroy TopMargin
        Destroy BottomMargin
        Destroy LeftMargin
        Destroy RightMargin
              setprop ex,*DisplayAlerts=OTRUE
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        ex.quit
        destroy ex
        pack    taskname,"Your file is c:\work\billing.xlsx!!"
           Close outfile
         PACK      STR35,NTWKPATH1,"NDAT23.dat"
           Erase      str35      
         PACK      STR35,NTWKPATH1,"NDAT23.isi"
           Erase      str35      
        alert   caution,taskname,result
           shutdown   
        STOP
TrapSaveAsObject
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
.
.
        scan    "Cannot access",str55
        if equal
..Instance 1 - exists and open elsewhere
                pack    taskname,str50," already exists and is open!!",newline,"Select another Filename!!"
                alert   caution,taskname,result
        endif
..Send them back to select another File name and try to Save again.
        goto SaveAsFileNameSelect
.end patch 1.0
.begin patch 1.1
getxref
           clear      outmlr
           clear      outcomp
           Clear      OODTEM
           Clear      OODTED
           Clear      OODTEC
           Clear      OODTEY
           move       c0,Outppm
           move       c1,NXRFPATH
           call       nxrfkey
           if         not over
           move       NXRFMLR,Outcomp
           call       getmlr
           endif
           return
.
getmlr
           packkey    COMPFLD,outcomp
           move       c1,CompPath
           call       compkey
           if         not over
           move       COMPOLDMLR,Outmlr
           call       getPpm
           endif
           return
getppm
           PACK       NORDFLD1 FROM "01L",outmlr
           MOVE       C2 TO NORDPATH
           call    nordlast
           if      not over
           if         (Ostat <> "0" & Ostat <> "B")
           goto       again
           endif
           cmatch     "M",olrn
           goto       again if equal
           cmatch     "B",olrn
           goto       again if equal
           PACK      STR2 FROM OSALES10,OSALES
           REP       ZFILL IN STR2
           MOVE      NO TO LSTMSW
           
           If        (str2 = "02" or Str2 = "06" or str2 = "19" or str2 = "27" or str2 = "28")
                                 goto      again                             .list management order
                      endif
                      
                      RESET     EXCODES
           SCAN      OELCODE IN EXCODES             .EXCHANGE ?
           GOTO      OKEX IF EQUAL                                .YES
           ELSE                                                              
                      RETURN
           ENDIF      

again      CALL       NORDKGP
           if      not over
           if         (Ostat <> "0" & Ostat <> "B")
           goto       again
           endif
           cmatch     "M",olrn
           goto       again if equal
           cmatch     "B",olrn
           goto       again if equal
           PACK      STR2 FROM OSALES10,OSALES
           REP       ZFILL IN STR2
           MOVE      NO TO LSTMSW
           If        (str2 = "02" or Str2 = "06" or str2 = "19" or str2 = "27" or str2 = "28")
                                 goto      again                             .list management order
                      endif

                      RESET     EXCODES
           SCAN      OELCODE IN EXCODES             .EXCHANGE ?
           GOTO      OKEX IF EQUAL                                .YES
                      goto       again
           ELSE
                      RETURN
           ENDIF                 
OKEX       
           MOVE      C0 TO FORM92
           MOVE      OEXQTY TO FORM92
           COMPARE   C0 TO FORM92            PURE EXCHANGE ?
           goto       again IF not EQUAL                 NO.

              packkey         NSEL2FLD,"1",OLRN
              move            "NSEL2KEY",Location
              pack            KeyLocation,"Key: ",NSEL2FLD
              call            NSEL2KEY
              if not over
                      if         (NSEL2DESC <> "001")      .per m rate
                      goto       again
                      endif
                      MOVE            NSEL2PRICE,FORM52
                                 if             (form52 <> 0 & OELCODE = "3")
                                 move       form52,outppm
.                                CALL       debug
                                 return
                      Elseif         (form52 <> 0 & OELCODE = "2")     
                                 move       form52,outppm
.                                CALL       debug
                                 return
                                 endif
              else
               unpack         OPPM,str3,str2
               pack           str6,str3,".",str2
               rep            zfill,str6
               move           str6,NSEL2PRICE
              endif

           goto       again
           return
clearflds  
           Clear      NDAT3KEY
           Clear      NDATTDMC  
           Clear      ndat3own 
           Clear      ndat3cde 
           Clear      ndatdolc 
           Clear      ndat3exh 
           Clear      ndat3ex1 
           Clear      ndat3ex2 
           move       c0,Ndat3ExRt

           return
SetFlags
          Getitem   ndat0023CheckWith,0,result
          if        (result = c1)
          Move      Yes,WithFlag                     .exclude withdrawn
          Else
          Move      No,WithFlag  .Default
          endif
          Getitem   ndat0023CheckMan,0,result
          if        (result = c1)
          Move      Yes,MAnFlag                     .exclude No longer managed files
          Else
          Move      No,ManFlag   .Default
          endif
           goto       Runit      

......................................................
.timeout we are going to run the Job with defaults
Timeout
           goto       RunIt

......................................................
HelpGo
        setprop AboutMssg,visible=1
        return
......................................................

FileGo
          goto      Filego3
          branch result to FileGo1,FileGo2,FileGo2
FileGo1
                    return
FileGo2
                    winshow
FileGo3
                    stop
          return



           include    nxrfio.inc
           include nordio.inc
           include    compio.inc
           include    cntio.inc
           iNCLUDE     NSEL2io.INC
.end patch 1.1
         INCLUDE   NDAT3IO.inc
         INCLUDE   NDATIO.inc
         INCLUDE   COMLOGIC.inc

