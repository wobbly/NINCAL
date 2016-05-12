.
.
.  INPUT FILE IS SORTED
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
           INCLUDE   NDATDD.inc
          Include   M2Ndd.inc
         INCLUDE   NOWNDD.inc
          INCLUDE   NREFDD.INC
.begin patch 1.2
         include   nmdldd.inc
.end patch 1.2
          
Release   Init      "1.30"     DLH  convert from csv output to XLS.
Reldate   Init      "2013 September 20"
.Release   Init      "1.20"     DLH  add Planner and Caller
.Reldate   Init      "2013 September 19"
.Release   Init      "1.11"     DLH  Remove Pia
.Reldate   Init      "2013 September 9"
.Release   Init      "1.1"     DLH  Sunbelt PDF
.Reldate   Init      "2013 April 23"
.Release   Init      "1.01"     DLH  Remove SS from email
.Reldate   Init      "15 December 2011"
.Release   Init      "1.0"     DLH  PDF change, auto email, etc
.Reldate   Init      "28 August 2009"
NAME     DIM       19 (FILE NAME)
INPUT    FILE      fix=126 (INPUT  FILE)
LINECT   FORM      2           LINE COUNTER
PAGE     FORM      3           PAGE NUMBER
LR       FORM      "0"         '1'=LAST RECORD PENDING
LRTOT    FORM      7          GRAND TOTAL
DATE     DIM       8
LINE1    DIM       150
LINE2    DIM       150
LINE3    DIM       150
LINE4    DIM       150
LINE5    DIM       150
LINE1A    DIM       5
LINE2A    DIM       5
LINE3A    DIM       5
LINE4A    DIM       5
LINE5A    DIM       5
PRSW     FORM      "0"
PSTATUS   DIM       4
BLANK126 DIM       126
COUNT    FORM      "00000"
EXSW     DIM       1           * "Y" = DO NOT PRINT WITHDRAWN CARDS.
SW1P     INIT      "Y"         * "Y" = 1ST PASS.
Laser     pfile
*............................................................
.
.Begin patch 1.3
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
VT_R8         EQU 5           .Double - 8 byte Real
VT_R8a         EQU 5           .Double - 8 byte Real
.Booleans
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
VT_I4   EQU 3           .4 byte integer
.Zoom85  variant
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
XLShiftToLeft       Variant                        .range delete shift to left
XLShiftUp Variant                        .range delete shift Up     
TopMargin     variant
BottomMargin  variant
LeftMargin     variant
RightMargin     variant
xlRowHeight   variant
xlColumnWidth variant
xlColumnWidthA variant
xlColumnWidthB variant
xlColumnWidthCats variant
SReturn       init            0x0a                                                        .soft return/line feed
LOText        dim             100
range         dim             20
range2        dim             20
.end patch 1.3
Output    FIle                          .for CSV
FileCheck FIle
trapcount form      4
EXCL     DIM       4                    EXCLUSIVE PRINT FIELD
PRTFLAG    FORM      1
OWNFLAG  FORM          1
WITHFLAG FORM      1
LASRFLAG FORM      1
XCLFlag   Form      1
NINPPATH FORM      1
.
dim11a    dim       11
.Column Defs
Header1   form    9
Title1   form    9
Title2   form    9
Title3   form    9
Column4R  form    9
Column5R  form    9
Column5A  form    9
Column6R form    9
Column7R form    9
Column8R form    9
Column9R form    9
Column10R form    9
+............................................................
.
.Fonts
font8     font
          create  font8,"Times New Roman",size=8
font8i    font
          create  font8i,"Times New Roman",size=8,italic        
.Position of Columns
          move    "4250" to Header1
          move    "10" to Title1       
          move    "1500" to Title2
          move    "6750" to Title3       
          move    "200",column
          move    "10",column1
          move    "1250",column2
          move    "3200",column3
          move    "5000",column4
          move    "4100",column5
          move    "4350",column5a
          move    "6000",column6
          move    "5400",column7
          move    "6250",column8
          move    "6800",column9
          move    "7300",column10
          move    "4000",column4R
          move    "4700",column5R
          move    "6100",column7R
          move    "6600",column8R
          move    "7100",column9R
          move    "7700",column10R

. FILE OPENING SEQUENCE
         TRAP      TRAP IF F5
         MOVE      "EXIT" TO PF5
         TRAP      TRAP IF INT
           CMATCH    B1 TO PROGRAM          .CHAINED FROM DSINIT?
           IF        EOS                    .NO
         MOVE      "Names In The News" TO COMPNME
         MOVE      "NDAT0045" TO PROGRAM
           CLEAR      COMMENT
           MOVE       "LOCAL" TO PRTNAME
           ENDIF
         MOVE      "MASTER DATACARD LISTING" TO STITLE
         CLOCK     DATE TO DATE
           UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         REP       ZFILL,DD
         REP       ZFILL,MM
         CLEAR     TODAY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         MOVE      "Options" TO PF4
         TRAP      OPTGET IF F4
         CALL      PAINT
         CALL      FUNCDISP
         TRAP      IO IF IO
.
         TRAPCLR   IO
         MOVE      C0 TO COUNT
         MOVE      NO TO STR1
BEGIN    DISPLAY   *P01:05,"Options     :":
                   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
.
         GOTO      OPTION
OPTGET
         RESET     COMMENT
         KEYIN     *P20:10,"WITHDRN:  INCLUDE WITHDRAWN CARDS":
                       *P20:11,"NOWNER :  EXCLUDE OWNER INFO     ":
                   *P20:12,"LASER  :  PRINT ON LASER":
                   *P20:13,"XCL  :  Create CSV":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL;
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
OPTION
           MOVE        C0 TO WITHFLAG
         RESET     COMMENT
         SETLPTR   COMMENT
         DISPLAY   *P15:05,COMMENT
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
         RESET     COMMENT
           SCAN      "WITHDRN" IN COMMENT
           CALL      OPTWITH IF EQUAL
           RESET       COMMENT
           SCAN        "NOWNER" IN COMMENT
           CALL      OPTOWN IF EQUAL
           SCAN      "LASER" IN COMMENT
           CALL      OPTLASER IF EQUAL
           SCAN      "XLS" IN COMMENT
           CALL      OPTExcel IF EQUAL
         GOTO      INPGET
OPTNG    KEYIN         *P20:10,"WITHDRN:  INCLUDE WITHDRAWN CARDS":
                       *P20:11,"NOWNER :  EXCLUDE OWNER INFO     ":
                   *P20:13,"XCL  :  Create CSV":
                   *P20:12,"LASER  :  PRINT ON LASER":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL;
         GOTO      OPTGET
OPTDEFLT
           MOVE      C1 TO OWNFLAG
           MOVE      C1 TO WITHFLAG
          MOVE      C1 TO LASRFLAG
          MOVE      C1 TO XCLFLAG
          GOTO      INPGET
OPTWITH    MOVE        C2 TO WITHFLAG
           RETURN
OPTOWN     MOVE        C2 TO OWNFLAG
           RETURN
OPTEXCeL     MOVE        C2 TO XCLFLAG
           RETURN
OPTLASER  MOVE      C2 TO LASRFLAG
          RETURN
.
INPGET   TRAP      INPNG GIVING ERROR IF IO
         BRANCH    NINPPATH TO PRTGET
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         CLOSE     TESTFILE
           MOVE        C1 TO NINPPATH
         DISPLAY   *P15:06,INPNAME
         MOVE      INPNAME TO NAME
         OPEN      INPUT,INPNAME,EXCLUSIVE
         GOTO      PRTGET
INPNG    NORETURN
         TRAPCLR   IO
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *DV,ERROR:
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH pdrive,PRTNAME
          pack      str55 from "c:\work\pdf\",prtname,".pdf"
.          PRTOPEN   Laser,"PDF995",PRtname
          PRTOPEN   Laser,"PDF:",str55
         DISPLAY   *P15:07,PRTNAME
         GOTO      START
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
START
         CALL      HEADER
          if        (xclflag = c2)
.begin patch 1.3
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
              setprop sheet.range("b6","B6"),*Value="Status",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("b6:b6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("b6:b6").Font,*Bold="True"
              setprop sheet.range("C6","C6"),*Value="List ##",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("C6:C6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("C6:C6").Font,*Bold="True"
              setprop sheet.range("d6","d6"),*Value="Owner ##",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("d6:d6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("d6:d6").Font,*Bold="True"
              setprop sheet.range("e6","e6"),*Value="New",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("e6:e6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("e6:e6").Font,*Bold="True"
              setprop sheet.range("f6","f6"),*Value="Exclusive",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("f6:f6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("f6:f6").Font,*Bold="True"
              setprop sheet.range("g6","g6"),*Value="Commission",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("g6:g6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("g6:g6").Font,*Bold="True"
              setprop sheet.range("h6","h6"),*Value="Hotline",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("h6:h6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("h6:h6").Font,*Bold="True"

              setprop sheet.range("i6","i6"),*Value="Date New",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("i6:i6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("i6:i6").Font,*Bold="True"

              setprop sheet.range("j6","j6"),*Value="Revised",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("j6:j6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("j6:j6").Font,*Bold="True"

              setprop sheet.range("k6","k6"),*Value="Reviser",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("k6:k6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("k6:k6").Font,*Bold="True"

              setprop sheet.range("l6","l6"),*Value="Master List Name",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("l6:l6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("l6:l6").Font,*Bold="True"

              setprop sheet.range("m6","m6"),*Value="Universe",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("m6:m6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("m6:m6").Font,*Bold="True"

              setprop sheet.range("n6","n6"),*Value="## of Orders",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("n6:n6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("n6:n6").Font,*Bold="True"

              setprop sheet.range("o6","o6"),*Value="Owner",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("o6:o6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("o6:o6").Font,*Bold="True"

              setprop sheet.range("p6","p6"),*Value="Planner",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("p6:p6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("p6:p6").Font,*Bold="True"

              setprop sheet.range("q6","q6"),*Value="Caller",*HorizontalAlignment=xlAlignCenter
              setprop sheet.range("q6:q6").Font,*Name="Times New Roman", *Size=14
              setprop sheet.range("q6:q6").Font,*Bold="True"
               move           C8,howmany


.          prepare   output,"c:\work\dataupd.csv",exclusive
.          Write     Output,seq;*CDFON,"List","List","Owner","New","exclusive","Commission","Hotline","New","revised","revised","Master","Universe","Number","Owner"
..begin patch 1.2
..          Write     Output,seq;*CDFON,"Status","Number","Number","Code","Code","Percent","Hotline","Date","Date","By","Name"," ","of Orders","Name"
.          Write     Output,seq;*CDFON,"Status","Number","Number","Code","Code","Percent","Hotline","Date","Date","By","Name"," ","of Orders","Name","Planner","Caller"
.end patch 1.2
.end patch 1.3

          endif
.
*............................................................
. READ A RECORD FROM THE FILE
.
READ     DISPLAY   *P1:24,*EL,*HON,"READING";
                    READ      INPUT,SEQ;STATUS,LSTNUM,OWNNUM,NLSTCDE,ELSTCDE,str3,HOTLINE,NEWDATE:
                              REVDATE,PASSWORD,MLSTNAME,UNIVERSE,DIM11A
          call      debug
         DISPLAY   *P1:24,*EL;
*............................................................
. TEST FOR END OF FILE
. IF END OF FILE: TURN ON LR AND L1-L9
.
         GOTO      RIDON IF NOT OVER
        CALL       OWNER
         CALL      SETLR
         GOTO      TOTCALC
*............................................................
. TURN ON LEVEL-INDICATOR SWITCHES
.
SETLR    MOVE      C1 TO LR
         RETURN
*............................................................
.
RIDON
         ADD       C1 TO COUNT
         DISPLAY   *P15:12,*EL,"RECORDS READ = ",COUNT;
          reset     EXFEELST
          scan      Lstnum,EXFEELST
          goto      Read if equal
          reset     RUNCODES
          scan      Lstnum,RUNCODES
          goto      Read if equal
          
        CALL       OWNER
.begin patch 1.2
          Packkey   Nmdlfld from lstnum
          rep       Zfill,Nmdlfld
          move      "Driver-NMDLKEY",Location
          pack      KeyLocation,"Key: ",NMDLFLD
         call      nmdlkey
.end patch 1.2

+............................................................
. TOTAL CALCULATIONS
.
TOTCALC  TRAPCLR   PARITY  (NOP)
TOTCLX   TRAPCLR   PARITY  (NOP)
         CALL      TOTOUT
*............................................................
. SEE IF LR INDICATOR IS ON
. IF SO: END JOB
.
TESTLR   BRANCH    LR OF EOJ
         GOTO      MOVEDATA  (NOP)
*............................................................
. MOVE DATA FROM INPUT AREA TO FIELDS
.
MOVEDATA
         MOVE      B4 TO EXCL        CLEAR EXCLUSIVE PRINT FIELD.
*............................................................
. DETAIL CALCULATIONS
.
DETCALC  MOVE      "        ",PSTATUS
         CMATCH    "W",STATUS
         CALL      WITHDRAW IF EQUAL
         CMATCH    "T" TO STATUS
         CALL      TEMPWITH IF EQUAL
         ADD       C1,PRSW
         ADD       C1 TO LRTOT
         GOTO      DETOut
* ...........................................................
WITHDRAW
         BRANCH    WITHFLAG OF SKIP
         MOVE      "WDRN",STATUS
         RETURN
TEMPWITH MOVE      "TMPW" TO STATUS
         RETURN
SKIP
         NORETURN
         GOTO      READ
*............................................................
DETOUT   DISPLAY   *P1:24,*EL,*HON,"PRINTING";
. HEADING AND DETAIL OUTPUT
          COMPARE  "9900" to ROW

          CALL      HEADER IF equal
          CALL      HEADER IF not less

          unpack    REVDATE,CC,YY,MM,DD
          pack      str10,MM,SLASH,DD,SLASH,CC,YY
.
          prtpage Laser;*pcolumn1:row,*ALIGNMENT=*Left,*font=font8,*ll,Lstnum,"/",M2nMin;                               
          prtpage Laser;*pcolumn2:row,*ALIGNMENT=*left,*font=font8,*ll,Mlstname;                                          
          prtpage Laser;*pColumn4:row,*ALIGNMENT=*Left,*font=font8,*ll,Dim11a;                                        
          prtpage Laser;*pColumn5a:row,*ALIGNMENT=*Center,*font=font8,*ll,ELSTCDE;                                        
          prtpage Laser;*pcolumn6:row,*ALIGNMENT=*left,*font=font8,*ll,Ownnum,b1,Ownocpy;                                 
          add     eightlpi,row        
.          add     eightlpi,row  
          prtpage Laser;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,Str10;                                        
          prtpage Laser;*pColumn5a:row,*ALIGNMENT=*Center,*font=font8,*ll,Status;                                        
          add     eightlpi,row        
          if        (xclflag = c2)
.begin patch 1.2
.          Write      Output,SEQ;*CDFON,STATUS,LSTNUM,OWNNUM,NLSTCDE,ELSTCDE,str3,HOTLINE,NEWDATE:
.                              str10,PASSWORD,MLSTNAME,UNIVERSE,DIM11A,Ownocpy
.begin patch 1.3
.          Write      Output,SEQ;*CDFON,STATUS,LSTNUM,OWNNUM,NLSTCDE,ELSTCDE,str3,HOTLINE,NEWDATE:
.                     str10,PASSWORD,MLSTNAME,UNIVERSE,DIM11A,Ownocpy,MdlPlan,Mdlcall
.end patch 1.2
              add             C1,howmany
              move            howmany,str9
              call            Trim using str9
              pack            str12,"B",str9
              setprop         sheet.Range(str12),*Value=Status
              pack            str12,"C",str9
              setprop         sheet.Range(str12),*Value=Lstnum
              pack            str12,"D",str9
              setprop         sheet.Range(str12),*Value=Ownnum
              pack            str12,"E",str9
              setprop         sheet.Range(str12),*Value=Nlstcde
              pack            str12,"f",str9
              setprop         sheet.Range(str12),*Value=elstcde
              pack            str12,"g",str9
              setprop         sheet.Range(str12),*Value=Str3
              pack            str12,"h",str9
              setprop         sheet.Range(str12),*Value=Hotline
              unpack          NewDATE,CC,YY,MM,DD
              pack            str10,MM,SLASH,DD,SLASH,CC,YY
              pack            str12,"i",str9
              setprop         sheet.Range(str12),*Value=STR10

              unpack    REVDATE,CC,YY,MM,DD
              pack      str10,MM,SLASH,DD,SLASH,CC,YY
              pack            str12,"j",str9
              setprop         sheet.Range(str12),*Value=str10
              pack            str12,"k",str9
              setprop         sheet.Range(str12),*Value=Password
              pack            str12,"l",str9
              setprop         sheet.Range(str12),*Value=Mlstname
              pack            str12,"m",str9
              setprop         sheet.Range(str12),*Value=Universe
              pack            str12,"n",str9
              setprop         sheet.Range(str12),*Value=Dim11a
              pack            str12,"o",str9
              setprop         sheet.Range(str12),*Value=ownocpy
              pack            str12,"p",str9
              setprop         sheet.Range(str12),*Value=mdlplan
              pack            str12,"q",str9
              setprop         sheet.Range(str12),*Value=mdlcall
              
              
              
              
              
              
              




.end patch 1.3
          endif
*............................................................
. TURN OFF R.I.D. & ALL L-INDICATORS
.
TURNOFF
         COMPARE   C1 TO LR
         GOTO      RETURN IF EQUAL
         NORETURN
*............................................................
. GO READ ANOTHER RECORD
.
         GOTO      READ
* ...........................................................
. GOTO PRINT TOTAL COUNT
RETURN
         RETURN
*............................................................
. PAGE HEADING ROUTINE
.
PAGE     ADD       C1 TO PAGE

          COMPARE   C1 TO PAGE
          IF        EQUAL
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT:
                    *MarginL=0,*MarginT=0,*Duplex=2
         ENDIF

          if        (page <> c1)
          PrtPage   Laser;*Newpage;
          endif
          move      "200",row
        prtpage Laser;*pColumn:row,*ALIGNMENT=*Left,*font=font8i,"Confidential";
        prtpage Laser;*p7050:row,*ALIGNMENT=*LEFT,*font=font8,"Date:",Today;        
        prtpage Laser;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"*** Names in the News Datacard Listing ***",*boldoff;  
        add     eightlpi,row        
        add     eightlpi,row        
        prtpage Laser;*p7050:row,*ALIGNMENT=*Left,*font=font8,*ll,"Page:",Page;                  
        prtpage Laser;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"*** These list are out of date ***",*boldoff;  
        add     eightlpi,row        
        add     eightlpi,row  
         MOVE      C5 TO LINECT
         CMATCH    YES TO SW1P
         CALL      ZEROLINE IF EQUAL
         RETURN

HEADER    CALL       PAGE
          BRANCH     OWNFLAG OF HD1,HD2
HD1
          prtpage Laser;*pTitle1:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,*ulon,"List##/Min ID";                               
          prtpage Laser;*pTitle2:row,*ALIGNMENT=*LEFT,*font=font8,*ll,"Master List Name";                                          
          prtpage Laser;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,"Revised";                                        
          prtpage Laser;*pColumn4:row,*ALIGNMENT=*Left,*font=font8,*ll,"## Orders",*boldoff,*uloff;                                        
          prtpage Laser;*pColumn5:row,*ALIGNMENT=*Left,*font=font8,*ll,"Excl/Status";                                        
          prtpage Laser;*pColumn6:row,*ALIGNMENT=*Left,*font=font8,*ll,"Owner/Manager";                                 
          add     eightlpi,row        
          add     eightlpi,row  
          RETURN
HD2
          prtpage Laser;*pTitle1:row,*ALIGNMENT=*Left,*font=font8,*boldon,*ll,*ulon,"List##/Min ID";                               
          prtpage Laser;*pTitle2:row,*ALIGNMENT=*Left,*font=font8,*ll,"Master List Name";                                          
          prtpage Laser;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,"Revised";                                        
          prtpage Laser;*pColumn4:row,*ALIGNMENT=*Left,*font=font8,*ll,"## Orders",*boldoff,*uloff;                                        
          prtpage Laser;*pColumn5:row,*ALIGNMENT=*Left,*font=font8,*ll,"Excl/Status";                                        
          add     eightlpi,row        
          add     eightlpi,row  
          RETURN
          
ZEROLINE MOVE      C0 TO LINECT
         MOVE      NO TO SW1P
         RETURN
*............................................................
. TOTAL OUTPUT
.
TOTOUT   TRAPCLR   PARITY  (NOP)
         COMPARE   C1 TO LR
         RETURN    IF NOT EQUAL
.         CALL      DETOUT
          COMPARE  "10000" to ROW

          CALL      HEADER IF equal
          CALL      HEADER IF not less

          add     eightlpi,row        

          prtpage Laser;*pColumn3:row,*ALIGNMENT=*Left,*font=font8,*ll,"NUMBER OF RECORDS FOUND ",LRTOT,"****";                                        
          add     eightlpi,row        
          prtpage Laser;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,"Excl: P = Pacific Lists, C = Names in the News";                                        
          add     eightlpi,row        
          prtpage Laser;*pColumn2:row,*ALIGNMENT=*Left,*font=font8,*ll,"Status: W = Withdrawn, T = Temporarily Withdrawn";                                        
          

         RETURN
*............................................................
. READ OWNER
OWNER    CLEAR     NOWNFLD
           BRANCH    OWNFLAG OF OWN1,OWNX
OWN1
           unpack     OWNNUM,str2,NOWNFLD
           REP       ZFILL IN NOWNFLD
           CMATCH    B1 TO NOWNFLD
           IF        EOS
           MOVE      "NO OWNER NUMBER" TO Ownocpy
           GOTO      OWNX
           ENDIF

          CALL      NOWNKEY
          IF        OVER
          MOVE      "NO OWNER FOUND!!!" TO Ownocpy
          ENDIF
...get min ##
          Move      C2,M2nPath
          packkey   M2NFld2,lstnum
          call      M2nKey
          if        OVer
          Clear     M2NMin
          endif
          
...get min ##

OWNX     RETURN
* ...........................................................
. TRAP - JOB INTERUPTED.
.
TRAP     TRAPCLR   INT
         TRAPCLR   F5
         TRAP      TRAP IF F5
         TRAP      TRAP IF INT
         DISPLAY   *P1:24,*EL,*B,"JOB ABORTED!!!!!",*B,*W2;
         NORETURN
         GOTO      EOJ
*............................................................
. END OF JOB
.
EOJ
          if        (xclflag = c2)
.begin patch 1.3
          pack    range,"B6"
          pack    range2,"Q",str9
          sheet.range(range,range2).Columns.Autofit
.          Weof      Output,seq
.          Close     Output
XlsFileNameSelect
                              clear   taskname
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
                              Append    "c:\work\dataupd",taskname    
                              if        (#ver = c1)
                              append  ".xlsx",taskname
                              else
                              append  ".xls",taskname
                              endif
                              Reset     Taskname                              
                              erase          taskname
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
                    
                              trap    TrapXlsObject if Object
                              book.saveas giving N9 using *Filename=taskname
                              trapclr Object
.
                              setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
XLSCleanUp
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
                              ex.quit
                              destroy ex
.Email new XLS to User
                              Pack      MailAttach from taskname

.end patch 1.3
          endif
          CLOSE     INPUT
          PrtCLose  Laser
          pack      Str55 from "c:\work\pdf\",Prtname,".pdf"
          Move      c0,Trapcount
CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck

.send the file          
          Move      "Here is the Midmonth Managed List Use report",MailSubjct
          Move      "Creques@nincal.com",MailFrom
.          Move      "Davidherrick@nincal.com,IngaBeck@nincal.com,SusanAnstrand@nincal.com,ShirleySchoevaars@nincal.com",MailTo
.          Move      "IngaBeck@nincal.com,SusanAnstrand@nincal.com,JenniferCox@NINCAL.COM,PiaPayne@nincal.com",MailTo
.          Move      "IngaBeck@nincal.com,SusanAnstrand@nincal.com,JenniferCox@NINCAL.COM",MailTo
          Move      "Davidherrick@nincal.com",MailCC
          Move      "SuzieMcGuire@nincal.com",MailTo
.begin patch 1.3
          move      MailAttach,Mailbody
.          Move      Str55,MailBody
          if        (xclFlag = c2)
.          pack      MailAttach from str55,";","c:\work\dataupd.csv"
.end patch 1.3
          Pause     "15"
          else
          Move      Str55,MailBody
          MOve      str55,MailAttach
          endif
          Pause     "5"
          call      SendMail

          shutdown     "cls"
          STOP
*............................................................
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"List History Ndat0045",b1,str55
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

.Begin patch 1.3
TrapXlsObject
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
        goto XlsFileNameSelect
.end patch 1.3
*............................................................
. IO ERROR
.
IO        DISPLAY   *P1:1,*ES,"FILE NOT FOUND ",*B,*B ;
          shutdown  "CLS"
          STOP
          INCLUDE   NOWNIO.inc
          Include   M2Nio.inc
          INCLUDE   NREFIO.INC
.begin patch 1.2
         include   nmdlio.inc
.end patch 1.2
          INCLUDE   COMLOGIC.inc
