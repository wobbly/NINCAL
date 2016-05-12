.NAMES IN THE NEWS CALIF. Epsilon TO NINCAL MERGE UPDATE
.input created from spreasheet sent to accounting. Exported (text no tabs)
.new column on end of sheet used for LR# (keyed in from unbilled report)
.File saved to g:\data\epsimrg.dat.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         include   norddd.inc
.patch1.42
                              include   compdd.inc
                              include   cntdd.inc
.         include   nmlrdd.inc
.patch1.42
         INCLUDE   NMRGDD.inc
         include   ninvdd.inc
Release    INIT    "1.62"               DLH      .add zip+4
Reldate   Init      "2014 November 25"
.Release    INIT    "1.61"               DLH      .Excel 2013 *WindowState=xlMinimized
.Reldate   Init      "2014 January 22"
.Release   Init      "1.6"    DLH .Also being used for MSF merge from AB DATA  added customer suppress and unused multies
.reldate   Init      "22 May 2012"
.Release   Init      "1.52"    DLH various cleanup
.reldate   Init      "16 August 2011"
.Release   Init      "1.51"    DLH output report to excel
.reldate   Init      "26 April 2011"
.Release   Init      "1.50"    DLH verify valid order before update
.reldate   Init      "21 April 2008"
.Release  Init      "1.47"    DLH 19DEc2006  NOn Personal
.release  init      "1.46"        JD14Feb2006 New input directory.
.release  init      "1.45"        JD   27aug2004 added dead,prison dedcuts after lr.
.release  init      "1.42"        DMB   26MAY2004 Mailer Conversion
.release  init      "1.41"        DLH 12Jul2002 use getwinver
.release  init      "1.4"        ASH 02OCT2000 NEW SERVER ADDED
.release  init      "1.32"        JD28Aug00 read csv file format.
.release  init      "1.31"        JD05Aug99 added apply billed orders.
.release  init      "1.3"        JS22Jun99 NT copy.
.release  init      "1.2"        JDnov2898 print for mcomp .
.release  init      "1.1"       JDnov0797 updated report title.
.release  init      "1.0"       JDJAN0797 updated version of nmrg0003.(epsilon)
IN       FORM      5
APPLIED  FORM      5
first    init      "Y"
eop      form      "58"
lines    form      2
page     form      2
TDMCLR   DIM       10
byte2    dim       1
recno    dim       3
lname    dim       8
IQTY     dim       8
convers  dim       8
ncoa     dim       8
dma      dim       8
intra    dim       8
dim23    dim       23
net      dim       8
dim14    dim       14
dim24    dim       24
lrno     dim       6
errorr   dim       8
dead     dim       8
pris     dim       8
NonPer    Dim       8
.begin patch 1.6
Cust      Dim       8
Multies   Dim       8
.end patch 1.6
num      form      1
INPUT    FILE      var=441
skipped  file      var=449
.skipped  file      var=441
skipcnt  form      5
.begin patch 1.62
Zip4       dim        8
.end patch 1.62

.begin patch 1.51
OrderVar  Form        9                                     //Order Variation - Difference between Order and Input Qty
RIVar               Form        9                                               //Difference Between Received and Input Qty
TotIqty   form      10.2
totNetOut Form      10.2
Percent   form      4.4
BookCreated         Init      "N"                         .if no do not try to manipulate excel objects as they were not created
Count     Form      5
.to find version of excel  DH 04/02/09

#VERSION  DIM 10
#VER_F    FORM 4.2
#VER      FORM 1 1=EXCEL 2007 OR LATER

.to find version of excel
books                         AUTOMATION
book                          AUTOMATION
.......................
./bars    automation
./bar     automation
./menubar INTEGER 2,"0x00000006"
.......................
SubFlag form    1
sheets                        AUTOMATION
sheet                         AUTOMATION
sortcol                       AUTOMATION
sortcol1                      AUTOMATION
ex                            AUTOMATION       class="Excel.Application"
exrange                                 AUTOMATION       

HPageBreaks                             AUTOMATION
HPageBreak                              AUTOMATION
VT_BOOL                       EQU                  11
OTRUE                         VARIANT
OFALSE                        VARIANT
VT_I4                         EQU                  3           .4 byte integer
Zoom65                                  VARIANT

VT_R8                                   EQU                  5           .Double - 8 byte Real
xlColumnWidth                           VARIANT

xlColumnWidthGross            VARIANT

xlColumnWidthMailer           VARIANT
xlColumnWidthPrice            VARIANT
xlColumnWidthDescription      VARIANT

TopMargin                     VARIANT
BottomMargin                            VARIANT
LeftMargin                              VARIANT
RightMargin                             VARIANT
xlCenter                      VARIANT

xlRowHeight                             VARIANT

./Formatting vars needed
./This constant was found in the Object Browser in Excel under the Help topic for the
./HorizontalAlignment property of the Range object.
AlignLeft                               integer 4,"0xffffefdd"
AlignRight                              integer 4,"0xffffefc8"
AlignCenter                             integer 4,"0xffffeff4"
SheetsDefault                           integer 4,"0x00000000"
xlLandscape                             integer 4,"0x2"                     .2
xlPortrait                              integer 4,"0x1"                     .1
xlMinimized                             integer 4,"0xFFFFEFD4"
xlUnderlineStyleSingle                  integer 4,"0x2"

xlBorderWeightMedium                    VARIANT
xlPageBreakManual             VARIANT
xlPageBreakAutomatic                    VARIANT

SheetIndex                              VARIANT

./Colors
Red Color
RGB form 24
./Flag whether to make row red or not
ColorFlag Dim 1

.begin patch 1.2
FileCheck FIle
trapcount form      4
.end patch 1.2


XLSNAME    DIM  255


CELLPOINT                     FORM      5.2
C15       FORM "15"

Cell         DIM    5
Cell1        DIM    5
CurCellNum FORM 5
CellRange  DIM   255
CellRowCnt          FORM      "58"
CellRowCnt1         FORM      "54"
.............
DateString DIM 16
          clock timestamp,timestamp
          Move Timestamp,Datestring
          create  Red=255:0:0
          getitem   Red,0,RGB
.end patch 1.51



         MOVE      "Names In The News" TO COMPNME
         MOVE      "NMRG0005" TO PROGRAM
         MOVE      "APPLY EPSILON MERGE INFO" TO STITLE
         CLOCK    DATE TO TODAY
         CALL      PAINT
.START PATCH 1.4 REPLACED LOGIC
.         splopen   "g:\data\nmrgepsi.lst"
.         open      skipped,"g:\data\text\mrgeskip.ped"
         PACK      STR35,NTWKPATH1,"NMRGEPSI.LST"
         PACK      STR45,NTWKPATH1,"text\mrgeskip.ped"
.begin patch 1.51
.         splopen   STR35
.end patch 1.51
         open      skipped,STR45
.END PATCH 1.4 REPLACED LOGIC
         move       c1 to nordpath
         move       c1 to ninvpath
.begin patch 1.51
.         call       header
.end patch 1.51
         MOVE       NO TO STR1
         KEYIN     *P10:12,"APPLY ALREADY BILLED RECORDS ?? ",*uc,*T20,STR1
         REPLACE   "Y1N2" IN STR1
         MOVE      STR1,NUM
.START PATCH 1.4 REPLACED LOGIC
.         OPEN      INPUT,"g:\data\epsimrg.csv"
.         PACK      STR35,NTWKPATH1,"epsimrg.csv"
         PACK      STR35,"\\nins1\d\accounting\epsimrg.csv"
         OPEN      INPUT,STR35
.END PATCH 1.4 REPLACED LOGIC
.begin patch 1.51
./Which Windows Version is This?
         Call      GetWinVer
         
         
./Create the Variant objects for Excel SpreadSheet
./Initialize variables
          create  Zoom65,VarType=VT_I4,VarValue=65
          create    OTRUE,VarType=VT_BOOL,VarValue=1
          create    OFALSE,VarType=VT_BOOL,VarValue=0
          create    xlColumnWidth,VarType=VT_R8,VarValue="0.0"
          create    xlColumnWidthGross,VarType=VT_R8,VarValue="10.5"  
          create    xlColumnWidthMailer,VarType=VT_R8,VarValue="40.5" 
          create    xlColumnWidthPrice,VarType=VT_R8,VarValue="9.0"             
.          create    xlColumnWidthDescription,VarType=VT_R8,VarValue="60.0"                          
          create    xlColumnWidthDescription,VarType=VT_R8,VarValue="40.0"                          
./"1" increment in Excel interface equals "1.3888" in OLE logic
          create    TopMargin,VarType=VT_R8,VarValue="18"             Roughly equals .25 inches:  18 * 1.388 = 25
          create    BottomMargin,VarType=VT_R8,VarValue="36"          Roughly equals .50 inches:  36 * 1.388 = 50
          create    LeftMargin,VarType=VT_R8,VarValue="9"            Roughly equals .25 inches:  18 * 1.388 = 25       
          create    RightMargin,VarType=VT_R8,VarValue="9"           Roughly equals .25 inches:  18 * 1.388 = 25       
          
          create    xlPageBreakManual,VarType=VT_R8,VarValue="-4135"
          create    xlPageBreakAutomatic,VarType=VT_R8,VarValue="-4105"         
          create    xlCenter,VarType=VT_R8,VarValue="-4108"           
          create    xlBorderWeightMedium,VarType=VT_R8,VarValue="-4138"         
          create    xlRowHeight,VarType=VT_R8,VarValue="15.0"
          create    SheetIndex,VarType=VT_I4      

./         
.end patch 1.51


LOOP     READ      INPUT,SEQ;*cdfon:           1-2
                        recno:               3-4         4
                        lname:
                        IQTY:               13-20        8
                        convers:            21-28        8
                        ncoa:               29-36        8
                        dma:                37-44        8
                        intra:              45-52        8
                        str8:              53-76        24
                        str8:
                        str8:
                        net:                77-84        8
                        str8:              85-98        14
                        str8:
                        lrno:                99-104       6             LIst rental number
                        dead:
.begin patch 1.47
                        pris:
                        NonPer:
.begin patch 1.6
.                        errorr
                        errorr:
                        Cust:
.begin patch 1.62
.                        Multies
                        Multies:
                        Zip4
.end patch 1.62
.end patch 1.6
.end patch 1.47

         GOTO      STOP IF OVER
.         call      Trim      using LRno
         ADD       C1 TO IN
.begin patch 1.51          
          if        (in = c1)
./Create Excel Spreadsheet
                    Clear     Taskname
                              Clear     RIVAR
                              Clear     OrderVar                                          
./Create a new excel worksheet
./.Open Excel application
                                      create  ex
.begin patch 1.61
.                                      setprop ex,*WindowState=xlMinimized
.end patch 1.61
                                      setprop ex,*Visible="True"
                                        setprop ex.CommandBars("Standard"),*Visible="True"
                                        setprop ex.CommandBars("Formatting"),*Visible="True"
                                        setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
          GETPROP   ex,*VERSION=#VERSION
          MOVE      #VERSION,#VER_F
          IF        (#VER_F >= 12)
          MOVE      c1,#VER
          ELSE
          MOVE      "0",#VER
          ENDIF
                              if        (#ver = c1)
                             Pack      XLSNAME,DateString,"_","Merge.xlsx"
                              else
                             Pack      XLSNAME,DateString,"_","Merge.xls"
                              endif
                             Pack      Taskname,"\\nins1\e\data\",XLSNAME           ."
                             erase          taskname
         DISPLAY   *P10:18,"Spreadsheet : ",taskname

./.Reset Default of Worksheets found in a Workbook
                              getprop ex,*SheetsInNewWorkbook=SheetsDefault
                              setprop ex,*SheetsInNewWorkbook=C1
./.Create Workbooks collection
                              getprop ex,*Workbooks=books
./.Create/Add a single Workbook
                              books.add
                              books.item giving book using 1
./.Create Worksheets collection
                              getprop book,*Sheets=sheets
./.Create a single Worksheet - we did not need to add it as we set the default above to
./.add one new Worksheet each time a Workbook is created.
                              sheets.item giving sheet using 1
                              Clear Taskname
                                        Call SheetSetup
.                                       pack str5 with "L1",":","P1"
                                        Move c1 to curcellnum
                              CALL      HEADER              
                              move      Yes,BookCreated
./Merge Data         
          ADD       C1 TO COUNT
          endif
.end patch 1.51          
         cmatch    b1 to lrno
         goto      loop if eos
         DISPLAY   *P10:10,"RECORDS READ : ",IN
.         type      lrno
.         goto      loop if not equal
         call      wipemrg
.begin patch 1.52 
          Type      lrno
          goto      Loop if not equal
.end patch 1.52 

         move      lrno to nmrglr
         MATCH     B6 TO NMRGLR
         GOTO      LOOP IF EQUAL
         move      iqty to nmrgrqty
         move      iqty to nmrgiqty
         move      convers to nmrgconv
         move      ncoa to ncoamnf
         move      dma to nmrgdma
         move      intra to nmrgid
         move      pris to nmrgpris
         move      dead   to nmrgdisf
.begin patch 1.47
          MOve      NonPer,NmrgNPer         
          move    errorr,nmrgerr
.end patch 1.47
.begin patch 1.6
          Move      Cust,nmrgcs
          move      multies,nmrgudup
.end patch 1.6
.begin patch 1.62
         Move         zip4,NMRGZ4
.end patch 1.62
         move      net to nmrgnet 
         move      lname to nmrglnam
         MOVE      NMRGLR TO NMRGFLD
         REP       ZFILL IN NMRGFLD
         BRANCH    NUM OF NOCHK,chk
CHK      move      nmrgfld to ninvfld
         rep       zfill in ninvfld
         call      ninvkey
         if        not over
         move      "Previously billed/skip it" to mcomp
         add       c1 to skipcnt
         goto      skipwrt
         endif
nochk
.begin patch 1.50
          clear     omlrky
          Packkey   Nordfld from Nmrglr
          call      NordKey
          if        Not OVer
          CALL      NMRGTST
          IF        OVER
          CALL      NMRGWRT
          ADD       C1 TO APPLIED
                    ELSE
          CALL      NMRGDEL
          CALL      NMRGWRT
          ADD       C1 TO APPLIED
          ENDIF
          PACK      MKEY FROM OMLRNUM,OCOBN
          REP       ZFILL IN MKEY
          CALL      NMLRKEY
.         move      nmrglr to nordfld
          Else
          move      "Order not found!!" to mcomp
.         clear     omlrky
.         call      nordkey
.         if        not over
.         PACK      MKEY FROM OMLRNUM,OCOBN
.         REP       ZFILL IN MKEY
.         CALL      NMLRKEY
.         endif
          Endif
.end patch 1.50
.begin patch 1.51
          call      Detail
.det      PRINT     *1,nmrglr,*08,mcomp,*55,nmrgrqty,*65,omlrky
.         add       c1 to lines
.         compare   eop to lines
.begin patch 1.52 
...         call      header if not less
.end patch 1.52 
.end patch 1.51
         DISPLAY   *P10:14,"RECORDS SKIPPED : ",skipcnt
         DISPLAY   *P10:12,"RECORDS APPLIED : ",APPLIED
         GOTO      LOOP
.begin patch 1.51
.HEADER
.         ADD       C1 TO PAGE
.         PRINT     *F,*N
.         PRINT     *1,today,*21,"* * * NEW EPSILON MERGE INFO * * *";
.         PRINT     *70,"Page: ",PAGE
.         PRINT     *N
.         PRINT     *1,"LR ##",*08,"MAILER",*55,"QTY",*65,"M/P ##"
.         move       c6 to lines
.         RETURN
.begin patch 1.51
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
                    Move "Epsilon",Str55

                    Call      Trim Using str55
                    Pack      Taskname with Str55," Merge Report"
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
          Move      "8",str5
          Call Trim Using Str5          
          Pack Cell with "A",str5        
          Move Cell to str8   
.        Setprop sheet.range(Cell),*Value="LR ##"                   
          Pack Cell,"B",Str5        
.        Setprop sheet.range(Cell),*Value="Mailer",*HorizontalAlignment=AlignCenter                          
          Pack Cell,"C",Str5
        Setprop sheet.range(Cell),*Value="MP ",*HorizontalAlignment=AlignCenter                      
          Pack Cell,"D",Str5
        Setprop sheet.range(Cell),*Value="Order ",*HorizontalAlignment=AlignCenter              
          Pack Cell,"E",Str5
        Setprop sheet.range(Cell),*Value="Input ",*HorizontalAlignment=AlignRight
          Pack Cell,"F",Str5
        Setprop sheet.range(Cell),*Value="Net Out",*HorizontalAlignment=AlignLeft
          Pack Cell,"G",Str5
        Setprop sheet.range(Cell),*Value="Received",*HorizontalAlignment=AlignLeft
          Pack Cell,"H",Str5
        Setprop sheet.range(Cell),*Value="Variance ",*HorizontalAlignment=AlignLeft
          Pack Cell,"I",Str5
        Setprop sheet.range(Cell),*Value="Variance ",*HorizontalAlignment=AlignLeft
          Pack Cell,"J",Str5
.        Setprop sheet.range(Cell),*Value="Comments",*HorizontalAlignment=AlignLeft,*ColumnWidth=xlColumnWidthDescription                
          Pack Cell,"K",Str5
        Setprop sheet.range(Cell),*Value="List ",*HorizontalAlignment=AlignLeft,*ColumnWidth=xlColumnWidthDescription                
.
          Add c1 to CurCellNum
          Add c1 to CellRowCnt                    
.          Move CurCellNum to str5
          Move      "9",str5
          Call Trim Using Str5          
          Pack Cell with "A",str5        
        Setprop sheet.range(Cell),*Value="LR ##"                   
          Pack Cell,"B",Str5        
        Setprop sheet.range(Cell),*Value="Mailer",*HorizontalAlignment=AlignCenter                          
          Pack Cell,"C",Str5
        Setprop sheet.range(Cell),*Value="Number",*HorizontalAlignment=AlignCenter                      
          Pack Cell,"D",Str5
        Setprop sheet.range(Cell),*Value="Qty",*HorizontalAlignment=AlignCenter              
          Pack Cell,"E",Str5
        Setprop sheet.range(Cell),*Value="Qty",*HorizontalAlignment=AlignRight
          Pack Cell,"F",Str5
        Setprop sheet.range(Cell),*Value="Qty",*HorizontalAlignment=AlignLeft
          Pack Cell,"G",Str5
        Setprop sheet.range(Cell),*Value="Qty",*HorizontalAlignment=AlignLeft
          Pack Cell,"H",Str5
        Setprop sheet.range(Cell),*Value="(IQTY - RQTY)",*HorizontalAlignment=AlignLeft
          Pack Cell,"I",Str5
        Setprop sheet.range(Cell),*Value="(OQTY - IQTY)",*HorizontalAlignment=AlignLeft
          Pack Cell,"J",Str5
        Setprop sheet.range(Cell),*Value="Comments",*HorizontalAlignment=AlignLeft,*ColumnWidth=xlColumnWidthDescription                
          Pack Cell,"K",Str5
        Setprop sheet.range(Cell),*Value="Description",*HorizontalAlignment=AlignLeft

          Move Cell,str9
        Setprop sheet.range(str8,str9).Font,*Name="Arial",*Size="10",*Bold="True" 
        Setprop sheet.range(str8,str9).Borders(8),*LineStyle=1,*Weight=xlBorderWeightMedium        
        Setprop sheet.range(str8,str9).Borders(9),*LineStyle=1,*Weight=xlBorderWeightMedium         
          Move "10" to CurCellNum
          Move "10" to CellRowCnt                    

        RETURN

Detail
./Writes detail record out for Merge record
           Add C1 to CurCellNum
           Add C1 to CellRowCnt
           If (Page = C1)     
                    If (CellRowCnt > 58)
                              ADD       C1 TO PAGE
                              Move      c0 to CellRowCnt              

.                                       
                    Endif
           Endif
           Move CurCellNum,Str5
           call trim using str5
           Pack Cell,"A",Str5
           Move Cell,str8


         setprop sheet.range(Cell),*Value=NMRGLR,*HorizontalAlignment=AlignLeft                    
           Pack Cell,"B",Str5        
         setprop sheet.range(Cell),*Value=Mcomp,*HorizontalAlignment=AlignLeft
           Pack Cell,"C",Str5
         setprop sheet.range(Cell),*Value=OMLRKY,*HorizontalAlignment=AlignLeft
           Pack Cell,"D",Str5
         setprop sheet.range(Cell),*Value=OQTY,*HorizontalAlignment=AlignRight
           Pack Cell,"E",Str5
         setprop sheet.range(Cell),*Value=NMRGIQTY,*HorizontalAlignment=AlignRight
           Pack Cell,"F",Str5
         Setprop sheet.range(Cell),*Value=NMRGNET,*HorizontalAlignment=AlignRight            
           Pack Cell,"G",Str5
         Setprop sheet.range(Cell),*Value=NMRGRQTY,*HorizontalAlignment=AlignRight            
           Pack Cell,"H",Str5
         Setprop sheet.range(Cell),*Value=RIVAR,*HorizontalAlignment=AlignLeft 
           Pack Cell,"I",Str5         
         Setprop sheet.range(Cell),*Value=OrderVar,*HorizontalAlignment=AlignLeft          
           Pack Cell,"J",Str5         
           Move Cell,str9                          
           Reset Taskname      
         Setprop sheet.range(Cell),*Value=Taskname,*HorizontalAlignment=AlignLeft,*WrapText=OTRUE             
           Pack Cell,"K",Str5         
           Move Cell,str9                          
         Setprop sheet.range(Cell),*Value=NmrgLnam,*HorizontalAlignment=AlignLeft,*WrapText=OTRUE             
           If (ColorFlag = Yes)
          Setprop sheet.range(str8,str9).Font,*Color=RGB,*Bold="True",*Italic="True" 
          Move No to ColorFlag
         Endif
.Move this cell back to this position so autofit doesn't affect comment field formatting         
           Pack Cell,"I",Str5         
          call      debug
          move      c0,n9
          move      NmrgIqty,N9
          add       N9,TotIqty
          move      c0,n9
          move      NmrgNet,N9
          add       N9,TotNetout

           Return
.end patch 1.51
.
skipwrt  write     skipped,SEQ;TDMCLR,B1:
                        NMRGLNAM,b2:
                        NMRGKCOD,B1:
                        NMRGRQTY,B1:
                        NMRGIQTY,B1:
                        NMRGTREJ,B1:
                        NMRGID,B1:  
                        NMRGNETI,B1:
                        NMRGELIM,B1:
                        NMRGHDRP,B1:
                        NMRGCS,B1:
                        NMRGUDUP,B1:
                        NMRGND,B1:
                        NMRGDUPM,B1:
                        NMRGNET,B1:
                        NMRGZIPV,B1:
                        NMRGZIPC,B1:
                        NMRGZIP4,B1:
                        NCOAMWF,B1:
                        NCOAMNF,B1:
                        NCOATOTM,B1:
                        NIXIEM,B1:
                        NCOAUNM,B1:
                        NCOANFRJ,B1:
                        NCOANIX1,B1:
                        NCOANIX2,B1:
                        NCOANIX3,B1:
                        NMRGERR,B1:
                        NMRGDISF,B1:
                        NMRGNPER,B1:
                        NMRGDMA,B1:
                        NMRGELMX,B1:
                        NMRGZ4,B1:
                        NMRGNIX,B1:
                        NMRGTDMC,B1:
                        NMRGPRIS,B1:
                        NMRGDROP,B1:
                        NCOAREJ,B1:
                        NMRGCUST,B1:
                        NMRGFAM,B1:
                        NMRGHH,B1:
                        str8:               *FAMILY DUPE DROPS/DUP FIELD.
                        b1:
                        nmrgrep:           new field 8/24/95. (DNC)
                        b1:
                        nmrgnnet:            new field 8/24/95. (DNC)
                       nmrgdpv            new field 6/25/04  PIDI
.begin patch 1.51
.         goto       det
          call      Detail
          call      Wipemrg
          goto      loop
.end patch 1.51

wipemrg
         clear      nmrgLR  
         move      c0 to nmrgFILL
         move      c0 to nmrgLNAM
         move      c0 to nmrgKCOD
         move      c0 to nmrgRQTY
         move      c0 to nmrgCONV
         move      c0 to nmrgIQTY
         move      c0 to nmrgTREJ
         move      c0 to nmrgID  
         move      c0 to nmrgNETI
         move      c0 to nmrgELIM
         move      c0 to nmrgHDRP
         move      c0 to nmrgCS  
         move      c0 to nmrgUDUP
         move      c0 to nmrgND  
         move      c0 to nmrgDUPM
         move      c0 to nmrgNET 
         move      c0 to nmrgZIPV
         move      c0 to nmrgZIPC
         move      c0 to nmrgZIP4
         move      c0 to NCOAMWF 
         move      c0 to NCOAMNF 
         move      c0 to NCOATOTM
         move      c0 to NIXIEM  
         move      c0 to NCOAUNM 
         move      c0 to NCOANFRJ
         move      c0 to NCOANIX1
         move      c0 to NCOANIX2
         move      c0 to NCOANIX3
         move      c0 to nmrgERR 
         move      c0 to nmrgDISF
         move      c0 to nmrgNPER
         move      c0 to nmrgDMA 
         move      c0 to nmrgELMX
         move      c0 to nmrgZ4  
         move      c0 to nmrgNIX
         move      c0 to nmrgTDMC
         move      c0 to NCOAREJ 
         move      c0 to nmrgCUST
         move      c0 to nmrgPRIS
         move      c0 to nmrgDROP
         move      c0 to nmrgHH  
         move      c0 to nmrgFAM
         move      c0 to nmrgrep
         move      c0 to nmrgnnet 
         move      c0 to nmrgFIL1
                              move      c0 to nmrgdpv
        return 
STOP 
.begin patcch 1.51

.         compare   eop to lines
.         call      header if not less
.         PRINT     *L,*10,"NUMBER OF RECORDS APPLIED: ",applied
.         splclose
        book.printout using *ActivePrinter="\\NINS2\laser2"
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
          Pack      Taskname,"\\nins1\e\data\",XLSNAME           ."
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
          Move "ComputerRequest@nincal.com",MailFrom
        Move "GemmaSpranza@nincal.com",MailCC
        Move "File From The Epsi Merge Program.",MAILSubjct
          move      YEs,Mailtrace
          Move      c0,TrapCount                   .reset

CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,MailAttach,Exclusive          
          Close     FIleCHeck

          call      debug

        Call  SendMail                 

        Erase XLSNAME

.end patch 1.51
         weof      skipped,seqeof
         close     skipped
.begin patch 1.41
.begin patch 1.3
                    call                GetWinVer
.         path      exist,"c:\windows"
.         if        over
.START PATCH 1.4 REPLACED LOGIC
.         Execute   "c:\winnt\system32\cmd.exe /c copy g:\DATA\nmrgepsi.LST \\NINS2\laser2 "
.         else
.         EXECUTE   "c:\command.com /c copy g:\DATA\nmrgepsi.LST \\NINS2\laser2 "
.begin patch 1.51
.                    If                  (osflag = c1 | osflag = c5)
.         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"nmrgepsi.LST \\NINS2\laser2 "
.         Execute   TASKNAME
..         else
.                    elseif                  (osflag = c3 | osflag = c4)
.         EXECUTE   "c:\command.com /c copy \\nins1\E\DATA\nmrgepsi.LST \\NINS2\laser2 "
..END PATCH 1.4 REPLACED LOGIC
.                    ElseIf                  (osflag = c6)
.         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"nmrgepsi.LST \\NINS2\laser2 "
..end patch 1.41
.         endif
.end patch 1.3
         display   *p2:23,"Please wait I'm PRINTING !!!!!"
.end patch 1.51
         pause     "10"
         stop

SheetSetup
./Sheet Formatting          
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
          if        (BookCreated = No)                            ..did we create a spreadsheet ??
          return
          endif
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

./Don't autofit the hardwired length for description - this column (I) has wrap text enabled.
          Move CurCellNum,Str5
          call trim using str5
          Pack Cell,"H",Str5  
        sheet.range("A1",CELL).Columns.Autofit  
./
.Select a column on which to sort
.This is ugly code.  You need to set the key value of the Sort method to a specific column.
.The Columns property returns a Range object, which is then used by the Sort method.
.Again, all this info found in the Object Browser in Excel.
.List Name
        getprop sheet.range("A9"),*Columns(1)=sortcol
.        Pack Cell,"I",str5
        Pack Cell,"K",str5
.Key1 set to List Name, Order1 set to 1(Ascending) or 2(Descending)
        sheet.range("A10",Cell).sort using *Key1=sortcol,*Order1=1
./
        
        
        
./total records
          Pack Cell,"A",Str5
          Pack Taskname,"Number of Order Received: ",Count
        setprop sheet.range(Cell),*Value=Taskname                   
          move      str5,n5
          sub       c1,n5
          move      n5,str6
          call      trim using str6
          Pack Cell,"E",Str5
          pack      str45 from "=sum(E10:E",str6,")"
         Setprop sheet.range(Cell),*Value=Str45,*HorizontalAlignment=AlignRight            
          Pack Cell,"F",Str5
          pack      str45 from "=sum(F10:F",str6,")"
         Setprop sheet.range(Cell),*Value=Str45,*HorizontalAlignment=AlignRight            
          calc      percent=(totNetOut/TotIqty)
          pack      str45 from "=sum(F",str5,"/E",str5,")"
          Pack Cell,"g",Str5
         Setprop sheet.range(Cell),*Value=str45,*HorizontalAlignment=AlignRight            

./Set the height of all the cells
          Pack Cell,"H",Str5        
          Pack CellRange,"A1",":",Cell
.          Setprop sheet.Range(CellRange),*RowHeight=xlRowHeight       
          Return

WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
                    if        (trapcount > 60)   . 3 min are you kidding me
                    Pack       MailSubjct,"Nmrg0005 - ",str35,b1,str55
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
.end patch 1.51

         INCLUDE   NMRGIO.inc
.patch1.42
         include   compio.inc
         include   cntio.inc
.         include   nmlrio.inc
.patch1.42
         include   nordio.inc
         include   ninvio.inc
         INCLUDE   COMLOGIC.inc

