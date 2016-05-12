PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NDATDD.inc
          include   nowndd.inc
         include   nmdldd.inc
          include   nusgdd.inc
          include         NQRCDD.inc
         include   nmoddd.inc
          INCLUDE   NTXTDD.INC
          INCLUDE   NSELDD.INC
          Include Compdd.inc
          Include Cntdd.inc   
RELEASE  INIT      "1.1"      DLH Replace Reuben with Amy
reldate   Init      "2015 May 18"
.RELEASE  INIT      "1.0"      DLH New pull rolling report of lists that update "monthly" that have not been updated 
.reldate   Init      "2013 October 7"
....................................................................
OUTPUT   FILE      
....................................................................

............................................................
QTY      FORM      5
MASK     INIT      "Z,ZZZ,ZZZ,ZZ9"
. 
...............................................................................
.misc VARIABLES
FileCheck FIle
trapcount form      4

MASK13    INIT      "Z,ZZZ,ZZZ,ZZZ"
DIM13a    DIM       13
dim9a     dim       13
TOTUNIV  FORM      10
UNIMASK  INIT      "Z,ZZZ,ZZZ,ZZ9"
UNIVPRT  DIM       13
PRICE    DIM       25
TEXT     DIM       46
TEXT1    DIM       47         556-602  FREE TEXT.  **NOTE: EACH LINE OF TEXT
NoteText  Dim       4000
Dim11a    Dim       11
akey2    init       "02R"
sysmo    dim       2           .used to
sysday   dim       2           .hold the
sysyr    dim       2           .system date
DATMO    DIM       2           .used to 
DATDAY   DIM       2           .hold the
DATYR    DIM       2           .datacard revised date.
ORDDATE  FORM      5           .order date in julian
CARDATE  FORM      5           .datacard date in julian
date     DIM       8
today1   FORM      5 
today2   FORM      5 
today3   FORM      5                 .cut off date for datacards
date1    dim       8
CHECK    FORM      5
check2   form      5
BLNK3    DIM       3
COUNT    FORM      5
count1   form      5
count2   form      5
USED     FORM      5
NOTUSED  FORM      5
OPTFLAG  FORM      1

ORDS      FORM      9
Volume    Form      13
mask9      init    "ZZZ,ZZZ,ZZ9"         ;formatting vars
.............................................................................................
.some excel goodies
#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER
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
xlColumnWidthCats variant
TopMargin     variant
BottomMargin  variant
LeftMargin     variant
RightMargin     variant
Row1          form            5
Row2          Form            5
DimRow1       Dim             5
DimRow2       Dim             5
ExRange1      Dim             7
ExRange2      Dim             7
ExRange3      Dim            16
NFULCOMP  DIM       55

....................................................................
         MOVE      "Ndat0012" TO PROGRAM
         MOVE      "Monthly Updates" TO STITLE
         MOVE      "Names In The News" TO COMPNME
         MOVE      C1 TO NDATPATH  .SET ACCESS TO ISAM
         MOVE      C2 TO NORDPATH
         move       c3,nordlock
         move       c3,ndatlock
         clock     date to today
         CALL      PAINT
         MOVE      "ABORT" TO PF5
         CALL      FUNCDISP
         display   *p1:24,*el;
. 
         TRAP      ABORT IF F5
. 
. 
....................................................................
         CLOCK     DATE TO DATE
         UNPACK    DATE INTO SYSMO,str1,SYSDAY,str1,SYSYR
         REP       zfill,SYSDAY
         REP       zfill,SYSMO
         MOVE      SYSMO TO MM
         MOVE      SYSDAY TO DD
         MOVE      SYSYR TO YY
         CALL      CVTJUL
         MOVE      juldays TO TODAY1
         sub        "30" from Juldays           .Approx 1 mos
         move       Juldays to Today2
         sub        "90" FROM jULDAYS           .Approx 3 mos
         move       juldays,Today3
.
          if        (prtname = "")
          Move      "Monthly",prtname
          endif
          if        (func = "")
          move      c2,func
          endif
. 
....................................................................
BEGIN    
.         PREPARE   OUTPUT,"\\nins1\e\data\datawkly.dat|10.10.30.103:502",exclusive
          MOve            "11",Row1
          move            "12",row2
          move      c4,ndatpath
          move      "000000",ndatfld
         call       ndattst
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
              setprop sheet.range("D6","D6"),*Value="Usage Link",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("D6:D6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("D6:D6").Font,*Bold="True"
              setprop sheet.range("E6","E6"),*Value="## Orders",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("E6:E6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("E6:E6").Font,*Bold="True"
              setprop sheet.range("F6","F6"),*Value="Volume",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("F6:F6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("F6:F6").Font,*Bold="True"
              setprop sheet.range("g6","g6"),*Value="Quantity",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("g6:g6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("g6:g6").Font,*Bold="True"
              setprop sheet.range("h6","h6"),*Value="Description",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("h6:h6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("h6:h6").Font,*Bold="True"
              setprop sheet.range("i6","i6"),*Value="Price",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("i6:i6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("i6:i6").Font,*Bold="True"
              setprop sheet.range("j6","j6"),*Value="Updated",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("j6:j6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("j6:j6").Font,*Bold="True"
              setprop sheet.range("k6","k6"),*Value="Revised",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("k6:k6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("k6:k6").Font,*Bold="True"

              setprop sheet.range("l6","l6"),*Value="Cleaned",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("l6:l6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("l6:l6").Font,*Bold="True"

              setprop sheet.range("M6","M6"),*Value="Planner",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("M6:M6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("M6:M6").Font,*Bold="True"

              setprop sheet.range("N6","N6"),*Value="Caller",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("N6:N6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("N6:N6").Font,*Bold="True"

              setprop sheet.range("O6","O6"),*Value="Owner ##",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("O6:O6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("O6:O6").Font,*Bold="True"

              setprop sheet.range("P6","P6"),*Value="Owner ",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("P6:P6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("P6:P6").Font,*Bold="True"

              setprop sheet.range("Q6","Q6"),*Value="Fulfillment",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("Q6:Q6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("Q6:Q6").Font,*Bold="True"

              setprop sheet.range("R6","R6"),*Value="Comm.",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("R6:R6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("R6:R6").Font,*Bold="True"

              setprop sheet.range("S6","S6"),*Value="Status",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("S6:S6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("S6:S6").Font,*Bold="True"

              setprop sheet.range("T6","T6"),*Value="Typist",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("T6:T6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("T6:T6").Font,*Bold="True"

              setprop sheet.range("u6","u6"),*Value="Web Reco",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("u6:u6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("u6:u6").Font,*Bold="True"

              setprop sheet.range("u6","u6"),*Value="Categories",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("u6:u6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("u6:u6").Font,*Bold="True"

         GOTO      A100
....................................................................
. 
. GET NEXT DATA CARD
. 
A100
          move      "Driver-NDATKS",Location
         CALL      NDATKS
         GOTO      Z900 IF OVER
. 
         DISPLAY   *P1:8,"WORKING ON LIST ",LSTNUM," - ",MLSTNAME
         add       c1 to count
        display   *p10:13,"records read ",count
        if          (lstnum = "006650")
        call        Debug
        endif
          
. 
CHKRUN   RESET     RUNCODES
         SCAN      lstnum IN RUNCODES         .running charge only list?
         GOTO      A100 IF EQUAL              .yes skip
.
          if        (elstcde <> "C" & Elstcde <> "P")         .just double checking
          goto      A100
          endif
          
          if        (Status = "W")         .skip Withdrawn
          goto      A100
          endif

.
CHECK   
          unpack    revdate into cc,yy,mm,dd
         CALL      CVTJUL
          if        (juldays < Today2 & cLEANCDE = "C004")
          call      INFo
          endif
          if        (juldays < Today3 & cLEANCDE = "C003")
          call      INFo
          endif
          if        (juldays < Today3 & cLEANCDE = "C023")
          call      INFo
          endif
         GOTO      a100


INFO     add       c1 to count1
         display   *p10:14,"Not updated last 30 days",count1

                    pack      COMPFLD,datFUl
                    rep       zfill,COMPFLD
                    move      C1,COMPPATH
                    move      "Driver-COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
                    if over
                              clear     COMPFLD
                              clear     NFULCOMP
                    else
                              if (COMPSVBFLG <> "T")
                                        clear     COMPFLD
                                        clear     NFULCOMP
                              else
                                        move      COMPCOMP,NFULCOMP
                              endif
                    endif
          Packkey   Nmdlfld from lstnum
          rep       Zfill,Nmdlfld
          move      "Driver-NMDLKEY",Location
          pack      KeyLocation,"Key: ",NMDLFLD
         call      nmdlkey

C100
          clear     ords
          clear check
          clear check2
          clear orddate
          REP       zfill IN LSTNUM
          PACK      NORDFLD2,AKEY2,LSTNUM
          CLEAR     NORDFLD1
          CLEAR     NORDFLD3
          CLEAR     NORDFLD4
.
          CALL      NORDAIM
          GOTO      Output IF OVER            .no usage skip it
          CMATCH    "p" TO OSTAT       Pending order ?
          GOTO      CheckOtherOrds IF EQUAL               . YES, skip.
          CMATCH    "x" TO OSTAT       Cancelled Pending order ?
          GOTO      CheckOtherOrds IF EQUAL     YES, skip.
          CMATCH    "l" TO OSTAT       LCR order ?
          GOTO      CheckOtherOrds IF EQUAL     YES, skip.
          CMATCH    "z" TO OSTAT       Cancelled LCR order ?
          GOTO      CheckOtherOrds IF EQUAL     YES, skip.
          MOVE      OODTEM TO MM
          MOVE      OODTED TO DD
          MOVE      OODTEY TO YY
          CALL      CVTJUL

          if        (juldays >= Today2 & Juldays <= today1)
          add       c1 to ORDS             .ITs GOOD
          move      oqty,n9
          add       n9,volume
          ELSE
          GOTO     CheckOtherOrds           .DATE OUT OF RANGE
          ENDIF
CheckOtherOrds
          loop
          CALL      NORDKG
          until     over
          CMATCH    "p" TO OSTAT       Pending order ?
          GOTO      CheckOtherOrds IF EQUAL               . YES, skip.
          CMATCH    "x" TO OSTAT       Cancelled Pending order ?
          GOTO      CheckOtherOrds IF EQUAL     YES, skip.
          CMATCH    "l" TO OSTAT       LCR order ?
          GOTO      CheckOtherOrds IF EQUAL     YES, skip.
          CMATCH    "z" TO OSTAT       Cancelled LCR order ?
          GOTO      CheckOtherOrds IF EQUAL     YES, skip.
          MOVE      OODTEM TO MM
          MOVE      OODTED TO DD
          MOVE      OODTEY TO YY
          CALL      CVTJUL

          if        (juldays >= Today2 & Juldays <= today1)
          add       c1 to ORDS             .ITs GOOD
          move      oqty,n9
          add       n9,volume
          endif
          REpeat
. 
.
Output
          move      c1,nownpath
          unpack    ownnum into str2,str4
          packkey      Nownfld,str4
          rep       zfill,Nownfld
          move      "Driver-NownKEY",Location
          pack      KeyLocation,"Key: ",NownFLD
          call      Nownkey

          move      COMMPER,str3

          Move            Row1,DimRow1
                    call            Trim using DimRow1
                    Move            Row2,DimRow2
                    call            Trim using DimRow2
.                   
                    pack            Exrange1 from "B",DimRow1
                    pack            Exrange2 from "B",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
          
                    setprop sheet.range(ExRange1,ExRange1),*Value=Lstnum,*NumberFormat="######0"
                    pack            Exrange1 from "C",DimRow1
                    pack            Exrange2 from "C",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2

          clear     TEXT1

          if (NDATCONV = "1")
                              pack      NSELFLD1,"01X",LSTNUM
                              pack      NSELFLD2,"02XBASE"
                              move      "NSELAIM",Location
                              pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                              call      NSELAIM
                              if not over
                                        clear dim13a
                                        clear n10
                                        move      mask13 to dim13a
                                        move      nselqty to n10
                   ADD   N10 TO TOTUNIV
                                        edit      n10 to dim13a
                                        call trim using dim13a
                                        call trim using nselsname
                                        clear str30
                                        packkey str30 with nselsname
                                        uppercase  str30
                                        move nselprice to dim9a
                                        call trim using dim9a
                                        CALL SelectLoadModifier
                                        if (NSELEXC <> "2")
                                                  pack      TEXT1,dim13a,B1,str30,"$",str25
                                        else
                                                  pack      TEXT1,dim13a,B1,str30,"EXCH ONLY"
                                        endif
                              endif
          endif
                    pack      NTXTFLD,LSTNUM,"1"
                    move      "NTXTKEY",Location
                    pack      KeyLocation,"Key: ",NTXTFLD
                    call      NTXTKEY
                    if not over
                              call      PARSITUP using text1,ntxttext,C1
                    endif
          call      trim using mlstname
                              if        ((ELSTCDE = "P" | ELSTCDE = "C") & NDATWEB <> "1")
                              REP             "#"'" in  Mlstname     ."       Hyperlink does not like double quotes
                              Pack      Taskname from "=Hyperlink(#"http://www.ninlists.com/Datacards/Data",lstnum,".htm","#",#"",Mlstname,"#")"
                              setprop sheet.range(ExRange1,ExRange1),*Formula=taskname
                              else
                              setprop sheet.range(ExRange1,ExRange1),*Value=Mlstname
                              endif
.                              setprop sheet.range(ExRange1,ExRange1),*Value=Mlstname
                    pack            Exrange1 from "D",DimRow1
                    pack            Exrange2 from "D",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2


                    pack      NUSGFLD1,"01X",LSTNUM
                    move      "NUSGAIM",Location
                    pack      KeyLocation,"Key: ",NUSGFLD1
                    call      NUSGAIM
                              If Not over
                                        if (NDATLUSAGE <> "F")
                                        Pack      Taskname from "=Hyperlink(#"http://www.ninlists.com/usage/usg",lstnum,".pdf","#",#"","Click to see Usage","#")"
                                        setprop sheet.range(ExRange1,ExRange1),*Formula=taskname
                                        endif
                              endif
                    pack            Exrange1 from "t",DimRow1
                    pack            Exrange2 from "t",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
          move      b55,noteText
          Clear     NoteText
          Clear     NQRCDdesc
          packkey   NQRCFLD   From Lstnum
          rep       zfill in nqrcfld
          Call      NQRCKey
          if        not over  
          packkey   NQRCDFLD from NQRCNum
          rep       zfill in nqrcdfld
          Call      NqrcDkey

          append    NQRCDdesc,NoteText
          Loop
          Call      NQRCKS
                    if        not over
                              if        (NQRCLIST <> Lstnum)
                              Break
                              endif
                    packkey   NQRCDFLD from NQRCNum
                    rep       zfill in nqrcdfld
                    Call      NqrcDkey
          append    NQRCDdesc,NoteText
          else
          Break     
                    Endif
         repeat
         endif
          reset     NoteText
                    setprop   sheet.Range(ExRange1,ExRange1),*Value=NoteText,*WrapText=OTRUE             
                    Clear     notetext
.order data here
                    pack            Exrange1 from "e",DimRow1
                    pack            Exrange2 from "e",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=Ords,*NumberFormat="##,####0"

                    pack            Exrange1 from "f",DimRow1
                    pack            Exrange2 from "f",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=volume,*NumberFormat="##,####0"


                    pack            Exrange1 from "g",DimRow1
                    pack            Exrange2 from "g",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=UNiverse,*NumberFormat="##,####0"
                    pack            Exrange1 from "h",DimRow1
                    pack            Exrange2 from "h",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=text1
                    pack            Exrange1 from "i",DimRow1
                    pack            Exrange2 from "i",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=PRice,*NumberFormat="$$,$$$$0.00"
                    pack            Exrange1 from "j",DimRow1
                    pack            Exrange2 from "j",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    unpack    revdate into cc,yy,mm,dd
                    pack      str10 from mm,slash,dd,slash,cc,yy
.                    setprop sheet.range(ExRange1,ExRange1),*Value=str10,*HorizontalAlignment=AlignRight,*NumberFormat="MMMM DD, YYYY"
                    setprop sheet.range(ExRange1,ExRange1),*Value=str10,*HorizontalAlignment=AlignRight,*NumberFormat="M/D/YYYY"
                    pack            Exrange1 from "k",DimRow1
                    pack            Exrange2 from "k",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    unpack    NDATUPDDATE into cc,yy,mm,dd
                    pack      str10 from mm,slash,dd,slash,cc,yy
.                    setprop sheet.range(ExRange1,ExRange1),*Value=str10,*HorizontalAlignment=AlignRight,*NumberFormat="MMMM DD, YYYY"
                    setprop sheet.range(ExRange1,ExRange1),*Value=str10,*HorizontalAlignment=AlignRight,*NumberFormat="M/D/YYYY"
                    pack            Exrange1 from "l",DimRow1
                    pack            Exrange2 from "l",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    if        (CLEANCDE = "C004")
                    setprop sheet.range(ExRange1,ExRange1),*Value="Monthly"
                    elseif    (CLEANCDE = "C003")
                    setprop sheet.range(ExRange1,ExRange1),*Value="Quarterly"
                    elseif    (CLEANCDE = "C021")
                    setprop sheet.range(ExRange1,ExRange1),*Value="Quarterly"
                    endif



                    pack            Exrange1 from "m",DimRow1
                    pack            Exrange2 from "m",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=MdlPlan
                    pack            Exrange1 from "n",DimRow1
                    pack            Exrange2 from "n",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=Mdlcall
                    pack            Exrange1 from "o",DimRow1
                    pack            Exrange2 from "o",DimRow2
                    setprop sheet.range(ExRange1,ExRange1),*Value=Ownnum
                    pack            Exrange3,Exrange1,":",Exrange2
                    pack            Exrange1 from "p",DimRow1
                    pack            Exrange2 from "p",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=OWNOCPY
                    pack            Exrange1 from "q",DimRow1
                    pack            Exrange2 from "q",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=NFULCOMP
                    pack            Exrange1 from "r",DimRow1
                    pack            Exrange2 from "r",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=Commper
                    pack            Exrange1 from "s",DimRow1
                    pack            Exrange2 from "s",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=status
                    pack            Exrange1 from "t",DimRow1
                    pack            Exrange2 from "t",DimRow2
                    pack            Exrange3,Exrange1,":",Exrange2
                    setprop sheet.range(ExRange1,ExRange1),*Value=Password
          Add       c1 to row1
          Add       c1 to row2

         ADD       c1 TO USED
         DISPLAY   *P12:18,"NUMBER OF LISTS WITH USAGE ",USED
         move      c0 to qty
         move       c0,volume
         clear      status
         clear      password
          goto      a100
          Return
. 
. 
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
          pack            Exrange1 from "B6"
          pack            Exrange2 from "u",DimRow2

          sheet.range(EXrange1,EXrange2).Columns.Autofit
         Setprop Sheet.Range("u1"),*ColumnWidth=xlColumnWidthCats


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
.begin 3.92 get exel version info
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
.end 3.92 get exel version info

............................................
SaveAsFileNameSelect
          setmode *mcursor=*arrow
.          ex.GetSaveAsFilename giving taskname using *InitialFilename=taskname
.                              append  ".xls",taskname
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
.Email new XLS to User
                              move    "Here is your List Report of overdue Monthly update lists",MailSubjct
                              pack    MailBody,"Input File:  ",INPNAME
                              pack      MailBOdy,"Input File:  ",INPNAME
          if        (func = "1")
                              Pack      MailTO from User,"@nincal.com"
          elseif    (func = "2")
                              Pack      MailTO from "AFREY@nincal.com,CarolFrazer@nincal.com"
                              move    "Here is your List Report of overdue Monthly update lists",MailSubjct
                              move      "davidherrick@nincal.com",mailcc
          endif
                              Pack      MailFrom from User,"CReques@nincal.com"
                              Pack      MailAttach from taskname
                              Move      c0,TrapCount                   .reset

CheckFile

                              trap      WaitForEnd giving error if IO
                              open      FileCheck,MailAttach,Exclusive          
                              Close     FIleCHeck
.                              move      "DavidHerrick@nincal.com",mailto

                              call      SendMail

. 
        shutdown   "cls"
.
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
.
SelectLoadModifier
        if          (lstnum = "006650")
        call        Debug
        endif
          pack      NMODFLD,NSELDESC
          rep       zfill,NMODFLD
          move      "D.Load2-NMODKEY",Location
          pack      KeyLocation,"Key: ",NMODFLD
          call      NMODKEY
          call      Trim using NMODDESC
          if        Not Eos
                    if ((NSELBASE = "BASE")|(NSELBASE = "SEC."))
                              pack      str25,dim9a,NMODDESC
                              call trim using str25
          
                    else
                              pack      str25,"+",dim9a,NMODDESC
                              call trim using str25
                    endif
          else
                    clear     str25
                    clear     price
          endif          
          move      str25,price
          return
. 
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Ndat0003 - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
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


          include         NQRCIO.inc
          include   Nownio.inc
         include   nmdlio.inc
          include   nusgio.inc
         INCLUDE   NORDIO.inc
         INCLUDE   NDATIO.inc
         include   nmodio.inc
          INCLUDE   NTXTio.INC
          INCLUDE   NSELio.INC
          Include Compio.inc
          Include Cntio.inc   
         INCLUDE   COMLOGIC.inc

