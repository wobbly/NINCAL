//New Program for Billing for Frontline.


pc       Equ       0
         Include   common.inc
         Include   cons.inc                   
         Include   norddd.inc
           Include   compdd.inc
           Include   cntdd.inc
         Include   tinvdd.inc
         Include   NBILDD.inc
         Include   consacct.inc
         Include   cvtdd.inc
         Include   hp.inc
         Include   nmrgdd.inc
         Include   ndatdd.inc
         Include   nowndd.inc
         Include   nshpdd.inc
         Include   nacddd.inc
         Include   ninvdd.inc
         INclude   NInvAcddd.inc
         Include   ndat3dd.inc
         Include   nxcgdd.inc


RELEASE  Init      "1.0"            DMB  11MAY2005          Added code to account for new triplex billing file

Cell                Dim       5
Cell1               Dim       5
CurCellNum          Form      5
CellRange           Dim       255
BalanceRow          Form      5
CellRowCnt          FORM      "46"

//Patch 2.53
//In order to use any of the properties/methods associated with all parent objects
//of the Worksheet, I need to create automation objects for each of them.
//
//Look at Excel Object Model to understand heirarchy.  This can be found in hard
//documentation:  Microsoft Office 2000 Object Model Guide (found in MS Office 2000 Developers Edition).
//Software available via PL/B Designer - create a Container object on a form, create an Excel
//Spreadsheet, right click on Container object and Browse object.  This will invoke the PL/B Object
//Browser, which will give you SOME of the components of the Object Model.  To browse the Object
//Model in its entirety, open Excel.  Under Tools menu select Macro, select Visual Basic Editor.
//In the Visual Basic Editor screen, under the View menu, select Object Browser.  There you can 
//view all of the objects/methods/properties in Excel.  Right clicking on an item will give you
//option to locate Help topics to see specifics.
//
//General heirarchy:
// Excel Application
//       Workbooks Collection (all open Workbooks)
//               Single Workbook
//                       Worksheets Collection (all Worksheets in this Workbook)
//                               Single Worksheet
//                                       SortColumn (a Single Column in that Worksheet used for sorting)

books                         AUTOMATION
book                          AUTOMATION
.......................
//bars    automation
//bar     automation
//menubar INTEGER 2,"0x00000006"
.......................
SubFlag                       Form    1
sheets                        AUTOMATION
sheet                         AUTOMATION
sortcol                       AUTOMATION
sortcol1                      AUTOMATION
ex                            AUTOMATION       class="Excel.Application"

HPageBreaks                             AUTOMATION
HPageBreak                              AUTOMATION
VT_BOOL                       EQU       11
OTRUE                         VARIANT
OFALSE                        VARIANT
VT_I4                         EQU        3           .4 byte integer
Zoom85                        VARIANT
VT_R8                                   EQU        5           .Double - 8 byte Real
xlRowHeight                             VARIANT
TopMargin                     VARIANT
BottomMargin                            VARIANT
//Formatting vars needed
//This constant was found in the Object Browser in Excel under the Help topic for the
//HorizontalAlignment property of the Range object.
AlignLeft                               integer  4,"0xffffefdd"
AlignRight                              integer  4,"0xffffefc8"
AlignCenter                             integer  4,"0xffffeff4"
SheetsDefault                           integer  4,"0x00000000"
xlLandscape                             integer  4,"0x2"                     .2
xlMinimized                             integer  4,"0xFFFFEFD4"
xlUnderlineStyleSingle                  integer  4,"0x2"


xlPageBreakManual             variant

xlPageBreakAutomatic                    variant

CELLPOINT                     Form       5
C15                                     Form       "15"

Input                                   file     
Lines                                   Form     2
Page                                    Form     2
Eop                                     Form     "58"
Skipcnt                       Form     5
APPLIED                                 Form     5
Selectd                       Form     5
Countin                       Form     5

//TRIPLEX BILLING VARIABLES.
TDMCLIST                      Init    "005051"    LIST NUMBER USED FOR ADDITIONAL R.C. BILLING
LManage                       Init    "018710"    List management exchange fees only
TDMCAMT                       Form     8.2
tottdmc                       Form     9.2
MT$tdmc                       Dim     15
TDMChrg                       Dim      1          (Y) = tdmc related charges
runchrg                       Form     9.2        =Running charges
grunrlr                       Form     9.2        =Total NINCA income on triplex billing
grunrar                       Form     9.2        =Total NINCA adc billing (TDMC)
grunrflat                               Form     9.2        =total ninca rental flat billing
grunrpass                               Form     9.2        =total ninca rental pass through billing  RC, Selects
TDMCFLAT                                Form     9.2        =estimated triplex flat charges shipping, Mag tape....
IncRFlat                                Form     9.2        =Calced NINCA income from flat charges.
PRCflat                                 Form     3          =percent cost associated to flat charges
PRCPASS                                 Form     3          =percent cost associated to /m charges
PRCLRflat                               Form     3          =percent income associated to flat charges
prcLRPASS                               Form     3          =percent income associated to /m charges
.Form7                                  Form     7
Form122                       Form     12.2
innets                        Dim      1
exfeflag                      Dim      1
BATCHBR                       Form     1       "0" =NO, "1" = YES.
RENTSW                        Form     1       "1" = RENTAL, "2" OR "3" = EXCHANGE
SALESBR                       Form     2
mangflag                      Dim      1       " " = brokerage order "*"=list management
mrgsw                         Dim      1
shipsw                        Dim      1 
//PRINT MASK VARIABLES
M$AR                          Dim     15
M$ARp                         Dim     13                              *prepaid 
M$PPM                         Dim      6
M$QTY                         Dim      9
M$AP1                         Dim     15
M$AP2                         Dim     15
M$LRINC                       Dim     15
M$NINC                        Dim     15
M$GROSS                       Dim     15
MT$AR                         Dim     15
MT$ARP                        Dim     15
MT$AP1                        Dim     15
MT$AP2                        Dim     15
MT$STAX                       Dim     15
MT$CTAX                       Dim     10
MT$POST                       Dim      9
MT$LRINC                      Dim     15
MT$NINC                       Dim     15
DATEMASK                      Dim      8
Mask12                                  Init    "ZZZZZZZZZ.99"                ;formatting vars
Dim12a                                  Dim     12                              ;formatting vars
n92                                     Form      9.2

         Move     "Frontline Automated Billing" to STITLE
         Move      "TINV0006" to PROGRAM 
         Move      "NIN" to COMPNME
         Clock     DATE to TODAY
         Call      PAINT
         Pack          STR35,NTWKPATH1,"TDMCBILL.LST"
         splopen   STR35       
         Trap      OOPS if f5
         Move      "Abort" to pf5
         Call      FuncDisp
         Open      Input,"Frontline.tst",exclusive

//Create the Variant objects
//Initialize variables
          move    C0,SubFlag
          create  Zoom85,VarType=VT_I4,VarValue=70
          create    OTRUE,VarType=VT_BOOL,VarValue=1
          create    OFALSE,VarType=VT_BOOL,VarValue=0
          create    xlRowHeight,VarType=VT_R8,VarValue="70.0"
//"1" increment in Excel interface equals "1.3888" in OLE logic
          create    TopMargin,VarType=VT_R8,VarValue="18"             Roughly equals .25 inches:  18 * 1.388 = 25
          create    BottomMargin,VarType=VT_R8,VarValue="36"          Roughly equals .50 inches:  36 * 1.388 = 50
//Patch 2.53
          create    xlPageBreakManual,VarType=VT_R8,VarValue="-4135"
          create    xlPageBreakAutomatic,VarType=VT_R8,VarValue="-4105"         
//Patch 2.53
         
         
//Create a new excel worksheet
//.Open Excel application
        create  ex
.        setprop ex,*WindowState=xlMinimized
        setprop ex,*Visible="True"
          setprop ex.CommandBars("Standard"),*Visible="True"
          setprop ex.CommandBars("Formatting"),*Visible="True"
          setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
//........................
//.       setprop ex,*AltStartupPath="C:\Documents and Settings\aharkin\application data\microsoft\office"
//.       setprop ex,*DisplayFullScreen=OTRUE
//........................
//.Reset Default of Worksheets found in a Workbook
        getprop ex,*SheetsInNewWorkbook=SheetsDefault
        setprop ex,*SheetsInNewWorkbook=C1
//........................
//        getprop ex,*CommandBars=bars
//        getprop   bars,*ActiveMenuBar=bar
//        setprop   bar,*Visible="True"
//        setprop   bar,*Position=menubar
//........................
//.Create Workbooks collection
        getprop ex,*Workbooks=books
//.Create/Add a single Workbook
        books.add
        books.item giving book using 1
//.Create Worksheets collection
        getprop book,*Sheets=sheets
//.Create a single Worksheet - we did not need to add it as we set the default above to
//.add one new Worksheet each time a Workbook is created.
        sheets.item giving sheet using 1
        setprop sheet.PageSetup,*Orientation=xlLandscape
        setprop sheet.PageSetup,*CenterFooter=" Page &P of &N"
//.START PATCH 1.3 ADDED LINE BACK IN - IT WAS ORIGINALLY COMMENTED OUT
        setprop sheet.PageSetup,*Zoom=Zoom85
        setprop sheet.PageSetup,*TopMargin=TopMargin
        setprop sheet.PageSetup,*BottomMargin=BottomMargin
        setprop sheet.PageSetup,*FooterMargin=TopMargin
//
          getprop sheet,*Hpagebreaks=HpageBreaks      
          
        Call      Header
        Move      C1 TO NMLRPATH       *SET ACCESS TYPE.
        Move      C1 TO NORDPATH       *SET ACCESS TYPE.
        Move      C1 TO NinvPATH       *SET ACCESS TYPE.    
          
loop
                    Read                Input,seq;*CDFON,TINVDATE,str1,str1,TINVLR,str1,str1,str1,str1,TINVDOLR      
                    Goto                eoj if over
          Type                TINVLR
          Goto                loop if not equal
          Squeeze   TINVDOLR,TINVDOLR,"$"
          Call                Trim using TINVDOLR
                  Move                  mask12 to dim12a
          Move                tinvdolr to n92
          Edit      n92 to dim12a         
          Squeeze             dim12a,dim12a,"."
          Squeeze             dim12a,dim12a,"$"         
          Call                trim using dim12a
          Move                dim12a to tinvdolr
          Call      zfillit using tinvdolr
          Call      Trim using tinvdolr
   
          Add       c1 to n4
          Display         *p12:10,"Records in :",n4

          Move                TINVLR TO TINVFLD
          Rep                 ZFILL IN tinvfld
.                   Call                TINVTST
.                   If                  OVER
.                             CALL      TINVWRT
                              ADD       C1 TO APPLIED
.                   Else
.                                       Move      "Already in File!!" to MCOMP
.                                       Add       C1 to SKIPCNT
.                   Endif
          Move      tinvlr to nordfld
          Move      "Order not found!!" to mcomp
          Clear     str2
          Clear     mangflag             .management
          Move      C1 TO NORDPATH       *SET ACCESS TYPE.
          call      nordkey
          If        not over
                    Pack      str2 from osales10,osales
                    Move      str2 to n2
                    If        (n2 = 6)
                              Move      "*" to mangflag
                    Endif
                    PACK      MKEY FROM OMLRNUM,OCOBN
                            REP         ZFILL IN MKEY
                              CALL      NMLRKEY
                    Endif
Det   
          Move      tinvlr to ninvfld        
          Move      tinvlr to nmrgfld
          Move      tinvlr to nshpfld         
          Call      ninvkey
          Move      olon to nownfld
.                   MOVE      C0 TO FORM7
.                   MOVE      QTYbild TO FORM7
          MOVE      PPM TO CMPT92
          Divide    HUND INTO CMPT92
          MOVE      CMPT92 TO FORM32
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
          If        Not over
                    Move      yes to mrgsw
          Endif
          Call    nownkey
          PACK    MKEY FROM OMLRNUM,OCOBN
          REP     ZFILL IN MKEY
          Call    NMLRKEY
          Move    c2 to tdmcflag     .force compute to calc tdmc goodies
          Call    COMPUTE
MASKIT   

          MOVE      MASK92 TO M$GROSS
          EDIT      GROSS TO M$GROSS
          
          MOVE      MASK92 TO M$AR
          EDIT      FORMAR TO M$AR
          
          
          MOVE      MASK92 TO M$AP1
                  EDIT      AP TO M$AP1
                  
          
          MOVE      MASK92 TO M$AP2
          EDIT      FORMAP2 TO M$AP2
          

          COMPARE   C0 TO FORMAP2
          IF        EQUAL
                      CLEAR   M$AP2
          Endif
          
          MOVE      MASK92 TO M$LRINC
          EDIT      LRINC TO M$LRINC
          
                  
          MOVE      MASK92 TO M$NINC
          EDIT      NININC TO M$NINC

*****************************************************************************
          MOVE      C0 TO RENTSW
          MOVE      OELCODE TO RENTSW
;...........................................................................
;  BATCH BILLING SECTION. 25MAR93
                    PACK      MKEY FROM omlrnum,OCOBN
                    REP       ZFILL IN MKEY
                    MOVE      C0 TO BATCHBR
                    CALL      NMLRKEY
//List management exchange fee   
                    MATCH   OLNUM TO Lmanage
                    IF        EQUAL
                              Move Yes,ExFeFlag      
                    Else
                              Move No,ExFeFlag
                    ENDIF
                  GOTO        PRINT

*****************************************************************************************
;DET
Print
//Fulfillment Total Charge
         ADD        C1 TO selectd
         Move       tinvdolr to form122
         Mult       ".01" by form122
         Move       c0 to TDMCAMT
         Add        Form122 to TDMCAMT
         
//Billed for running Charges
         Move       c0 to runchrg

         Move      tinvlr to nxcgfld
         Rep       zfill in nxcgfld
         Call      nxcgkey
         Goto      runexit if over
         Move      yes to tdmchrg
         Add       nxcgar to runchrg
runloop  
           Call      nxcgks
         Goto      runexit if over       
         Match     nxcglr to nxcgfld
         Goto      runexit if not equal
         Add       nxcgar to runchrg
         Goto      runloop
runexit         
.         Compare   "56" TO LINES
.         Call      HEADER IF not less
         Move      c0 to n2
         Move      onetper to n2
         Compare   c0 to n2                           .net name order?
         If            equal
          Move    b1 to innets
         Else
          Move      "*" to innets
         Endif
         If        (runchrg > 0)
                    Add       runchrg to runrar             .add exchange charges.
         Endif
.prog 19 charges
         Move      runrar to runrlr
         Sub       tdmcamt from runrlr
         Move      c0 to tdmcflat
         move      tdmcamt to tdmcflat
         Sub       runrpass,tdmcflat                  .what part of tdmc charge is flat charges
         If        (tdmcflat < 0 )   
          Move      c0 to tdmcflat
         Endif
         Calc      prcflat=TDMCFLAT/(runrpass+tdmcflat)*100
         Calc      prcpass=runrpass/(runrpass+tdmcflat)*100
         Calc      prcLRPASS=(RUNRLR/runrar)*100

.
         MOVE      MASK92 TO Mt$AP1
         MOVE      MASK92 TO Mt$AP2
         MOVE      MASK92 TO Mt$AR
         MOVE      MASK92 TO Mt$lrinc
         EDIT      RUNRAR TO Mt$AR
         EDIT      RUNRPASS TO Mt$AP1
         EDIT      RUNRflat TO Mt$AP2
         EDIT      RUNRlr TO Mt$lrinc        
.         
         If        (runrlr < 0 | incRflat < 0)
;LETS ONLY PRINT REJECTS
          COMPARE   C1 TO CMREFLAG
          IF     EQUAL
          
                    MATCH     YES TO EXFEFLAG
                    IF     EQUAL
.                                       Call LoadCells
                              PRINT     HPBON,*2,HPUNON,OMLRNUM,HPUNOFF,*8,innets,*9,LRN,*18,INVNUM,RUNFLAG:
                                        *26,BILNAME," ",GUARPAY,*50,M$AR,*64,M$AP1," ":
                                        *79,M$NINC,*91,M$LRINC:
                                        *L,*1,COBN,"-",BILLTN:
                                        *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
                                        *26,BILCOMP,*64,M$AP2," ",HPBOFF,HPUNOFF
                                        
                                        
                                        
                                        
                    ELSE
.                                       Call LoadCells                          
                              PRINT     HPBON,*2,OMLRNUM,*8,innets,*9,LRN,*18,INVNUM,RUNFLAG:
                                        *26,BILNAME," ",GUARPAY,*50,M$AR,*64,M$AP1," ":
                                        *79,M$NINC,*91,M$LRINC:
                                        *L,*1,COBN,"-",BILLTN:
                                        *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
                                        *26,BILCOMP,*64,M$AP2," ",HPBOFF
                    ENDIF
          ELSE
                    MATCH     YES TO EXFEFLAG
                    IF        EQUAL
.                                       Call LoadCells                          
                              PRINT     *1,mangflag,*2,HPuNoN,OMLRNUM,HPUNOFF,*8,innets,*9,tinvlr,*18,INVNUM,RUNFLAG:
                                        *26,BILNAME," ",GUARPAY,*50,M$AR,*64,M$AP1," ":
                                        *79,M$NINC,*91,M$LRINC:
                                        *L,*1,COBN,"-",BILLTN:
                                        *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
                                        *26,BILCOMP,*64,M$AP2," ",HPUNOFF
                    ELSE
.                                       Call LoadCells                          
                              PRINT     *1,mangflag,*2,OMLRNUM,*8,innets,*9,tinvlr,*18,INVNUM,RUNFLAG:
                                        *26,BILNAME," ",GUARPAY,*50,M$AR,*64,M$AP1," ":
                                        *79,M$NINC,*91,M$LRINC:
                                        *L,*1,COBN,"-",BILLTN:
                                        *17,INVDTEM,"/",INVDTED,"/",INVDTEY:
                                        *26,BILCOMP,*64,M$AP2," "
                    ENDIF
          ENDIF
          ADD       c2 TO LINES
         Endif
//Commented out and Moved up above

.         MOVE      MASK92 TO Mt$AP1
.         MOVE      MASK92 TO Mt$AP2
.         MOVE      MASK92 TO Mt$AR
.         MOVE      MASK92 TO Mt$lrinc
.         EDIT      RUNRAR TO Mt$AR
.         EDIT      RUNRPASS TO Mt$AP1
.         EDIT      RUNRflat TO Mt$AP2
.         EDIT      RUNRlr TO Mt$lrinc
//Commented out and Moved up above - End
//tdmcamt = $ from Triplex invoice file
//Runrar = total receivable $ from triplex related additional charges
//runrpass = receivables from Run Charge, commisionable selects
//runrflat = receivables from Flat charges ie Mag tape, Shipping, etc.
//IncRFLat   = Profit from flat charges (tdmcamt - runrpass = tdmc flat charges 'TDMCFLAT'; 
//runrflat - tdmcflat = IncRflat)         

.let print it all
         If        (prclrflat < 0)
                   print     hpunon,hpbon,*2,"Triplex Billed : ",tdmcamt,"  We Billed",*48,mt$ar,*62,mt$ap1,"/m ":
                             prcpass,"%":
                             *89,mt$lrinc,B1,PRCLRPASS,"%":
                             *l,*64,TDMCFLAT," Flat ",PRCFLAT,"%":
                             *91,INCrFLAT,B2,PRCLRFLAT,"%",hpboff,hpunoff
         Else
                   print     *2,"Triplex Billed : ",tdmcamt,"  We Billed",*48,mt$ar,*62,mt$ap1,"/m ":
                             prcpass,"%":
                             *89,mt$lrinc,B1,PRCLRPASS,"%":
                             *l,*64,TDMCFLAT," Flat ",PRCFLAT,"%":
                             *91,INCrFLAT,B2,PRCLRFLAT,"%"
         Endif
           Call LoadCells            
         ADD        C2 TO LINES
         Clear      tdmchrg      
         Add        runrar to grunrar
         Add        runrpass to grunrpass
         Add        runrflat to grunrflat    
         Add        runrlr to grunrlr
         Move       c0 to runrar
         Move       c0 to runrlr
         Move       c0 to runrpass
         Move       c0 to runrflat
         DISPLAY   *ef,*P10:14,"RECORDS SKIPPED : ",skipcnt
         DISPLAY   *ef,*P10:12,"RECORDS selected: ",selectd
         Goto      loop
;............................................................................
HEADER
         ADD       C1 TO PAGE
         compare    c1 to page
//         if         equal
//                 print      hp17ptch,hpdupl,*f          .compressed, duplex
//         endif
//         PRINT     *f,*29,"***  N E W   T R I P L E X   ":
//                   *60,"B I L L I N G   A N A L Y S I S  ***":
//                   *116,"DATE ",today:
//                   *L,*1,"CONFIDENTIAL",*116,"PAGE ",PAGE:
//                   *L,*L,*1,"MAILER",*11,"LR":
//                   *17,"INVOICE":
//                   *26,"MAILER BILL-TO":
//                   *52,"--------ACCOUNTS--------":
//                   *79,"-------COMMISSIONS------":
//                   *108,"------TAXES----",*128,"OUR":
//                   *L,*1,"NUMBER",*9,"NUMBER":
//                   *18,"NUMBER",*26,"NAME AND THRU":
//                   *53,"RECEIVABLE":
//                   *69,"PAYABLE",*81,"NIN INCOME":
//                   *94,"LR INCOME",*109,"STATE",*119,"CITY":
//                   *126,"POSTAGE",*L
//                 MOVE      c6 TO LINES

          Move C0 to CellRowCnt
          Add C1 to CurCellNum
          Add C1 to CellRowCnt          
          Move CurCellNum to str5
          Call Trim Using Str5
.For Bolding Below  
          Pack Cell1,"A",str5           
          
          Mult CurCellNum,C15,CELLPOINT
          If (Page = C1)
                    sheet.Shapes.AddPicture using "\\SRV2008A\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,0,190,60     
          Else
                    sheet.Shapes.AddPicture using "\\SRV2008A\c\netutils\Logocolornotag.jpg",OTRUE,OTRUE,0,CellPoint,190,60       
          Endif

          Add C6 to CurCellNum
          Add C6 to CellRowCnt          
          Move CurCellNum to str5
          Call Trim Using Str5
          Pack Cell,"K",str5  
        setprop sheet.range(Cell),*Value="Fulfillment",*HorizontalAlignment=AlignLeft           
        sheet.range(Cell).Columns.Autofit         
.         Pack Cell,"N",str5  
.        setprop sheet.range(Cell),*Value="Billing Variation (%)",*HorizontalAlignment=AlignLeft           
.        sheet.range(Cell).Columns.Autofit        
.         Pack Cell,"K",str5  
.        setprop sheet.range(Cell),*Value="Running",*HorizontalAlignment=AlignLeft   
.        sheet.range(Cell).Columns.Autofit 
.         Pack Cell,"L",str5  
.        setprop sheet.range(Cell),*Value="Guaranteed",*HorizontalAlignment=AlignRight               
.         Pack Cell,"M",str5  
.        setprop sheet.range(Cell),*Value="List Management",*HorizontalAlignment=AlignRight                               

          Add C1 to CurCellNum
          Add C1 to CellRowCnt          
          Move CurCellNum,str5
          Call Trim Using Str5          
          Pack Cell,"A",str5  
        setprop sheet.range(Cell),*Value="Mailer ##"
          Pack Cell,"B",str5  
        setprop sheet.range(Cell),*Value="Mailer Name"
          Pack Cell,"C",str5          
        setprop sheet.range(Cell),*Value="Broker Name"
          Pack Cell,"D",str5          
        setprop sheet.range(Cell),*Value="LR Number"
          Pack Cell,"E",str5          
        setprop sheet.range(Cell),*Value="Invoice Number",*HorizontalAlignment=AlignRight   
          Pack Cell,"F",str5          
        setprop sheet.range(Cell),*Value="Invoice Date",*HorizontalAlignment=AlignRight   
          Pack Cell,"G",str5          
        setprop sheet.range(Cell),*Value="Receivables",*HorizontalAlignment=AlignRight               
            

          Pack Cell,"H",str5          
        setprop sheet.range(Cell),*Value="Payables 1",*HorizontalAlignment=AlignRight               

          Pack Cell,"I",str5          
        setprop sheet.range(Cell),*Value="Payables 2",*HorizontalAlignment=AlignRight               

          Pack Cell,"J",str5          
        setprop sheet.range(Cell),*Value="Total Payables ",*HorizontalAlignment=AlignRight               
      
          Pack Cell,"K",str5  
        setprop sheet.range(Cell),*Value="Charges",*HorizontalAlignment=AlignLeft           
        
          Pack Cell,"L",str5          
        setprop sheet.range(Cell),*Value="NIN Income",*HorizontalAlignment=AlignRight   
        
          Pack Cell,"M",str5          
        setprop sheet.range(Cell),*Value="LR Income",*HorizontalAlignment=AlignRight   

          Pack Cell,"N",str5          
        setprop sheet.range(Cell),*Value="Billing Variation",*HorizontalAlignment=AlignLeft

.         Pack Cell,"N",str5  
.        setprop sheet.range(Cell),*Value="Charges",*HorizontalAlignment=AlignLeft   
.        sheet.range(Cell).Columns.Autofit               
.         Pack Cell,"O",str5  
.        setprop sheet.range(Cell),*Value="Payment (GOP)",*HorizontalAlignment=AlignRight               
.         Pack Cell,"P",str5  
.        setprop sheet.range(Cell),*Value="Order",*HorizontalAlignment=AlignRight   
.         Pack Cell,"Q",str5  
.         setprop sheet.range(Cell),*Value="Net Name",*HorizontalAlignment=AlignRight          

          Pack cellrange,"A",str5,":",Cell
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

;end patch 1.2
eoj
         compare   eop to lines
         call      header if not less
         PRINT     *L,*10,"NUMBER OF RECORDS APPLIED: ",applied
.Patch 2.1
         PRINT     *L,*10,"NUMBER OF RECORDS SKIPPED: ",skipcnt         
.Patch 2.1           
         splclose
;begin patch 1.8
                    call                GetWinVer
                    If                  (osflag = c3 | osflag = c4)
         PACK     TASKNAME,"!c:\command.com /c copy ",NTWKPATH1,"tdmcbill.LST \\NINs2\Laser2 "
         execute  TASKNAME
         PACK     TASKNAME,"!c:\command.com /c copy ",NTWKPATH1,"tdmcbill.LST \\NINs2\Laser2 "
         execute  TASKNAME
                    ElseIf             (osflag = c1 | osflag = c5)
;        else
         PACK     TASKNAME,"!c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"tdmcbill.LST \\NINs2\Laser2 "
         execute  TASKNAME
         PACK     TASKNAME,"!c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"tdmcbill.LST \\NINs2\Laser2 "
         execute  TASKNAME
;END PATCH 1.7 REPLACED LOGIC
                    ElseIf             (osflag = c6)
         PACK     TASKNAME,"!c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"tdmcbill.LST \\NINs2\Laser2 "
         execute  TASKNAME
         PACK     TASKNAME,"!c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"tdmcbill.LST \\NINs2\Laser2 "
         execute  TASKNAME
;end patch 1.8
         endif
;         .execute "F:\PUBLIC\NPRINT g:\DATA\tdmcbill.LST Q=LASER2 NT NA=GS_JL f=0 S=SRV2008A_fpnw C=2"
         display   *p2:23,"Please wait I'm PRINTING !!!!!"
         pause     "10"
         stop
oops     display    *p1:24,*el,*blinkon,"Now you've Done it!!!!!!!",*b,*b,*b,*w10
         stop   
LoadCells
                              Add c1 to CurCellNum
                              Add C1 to CellRowCnt                              
                              If (CellRowCnt > 46)
                                        Call Header
                                        Add C1 to CurCellNum
                                        Add C1 to CellRowCnt                    
                                                  
                              Endif                                   
                              
                              Move CurCellNum to str5
                              Call Trim Using Str5          
                              Pack Cell,"A",str5  
                            setprop sheet.range(Cell),*Value=OMLRNUM
                              Pack Cell,"B",str5  
                            setprop sheet.range(Cell),*Value=BILCOMP
                            sheet.range(Cell).Columns.Autofit                                     
                              Pack Cell,"C",str5          
                            setprop sheet.range(Cell),*Value=BILNAME
                            sheet.range(Cell).Columns.Autofit                                     
                              Pack Cell,"D",str5          
                            setprop sheet.range(Cell),*Value=TINVLR
                              Pack Cell,"E",str5          
                            setprop sheet.range(Cell),*Value=INVNUM,*HorizontalAlignment=AlignRight   
                              Pack Str10,INVDTEM,"/",INVDTED,"/",INVDTEY
                              Count n2,str10
                              If (n2 = c2)
                                        clear str10
                              Endif
                              Pack Cell,"F",str5          
                            setprop sheet.range(Cell),*Value=str10,*HorizontalAlignment=AlignRight            
                              Pack Cell,"G",str5          
                            setprop sheet.range(Cell),*Value=RUNRAR,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"                        
      
                              Pack Cell,"H",str5          
                            setprop sheet.range(Cell),*Value=AP1,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"                        

                              Pack Cell1,"I",str5         
                            setprop sheet.range(Cell1),*Value=AP2,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"                        
                              pack Str25,"=SUM","(",Cell,",",CELL1,")"
                              Pack Cell,"J",str5          
                            setprop sheet.range(Cell),*Value=str25,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"                        
                              Pack Cell,"K",str5  
                            setprop sheet.range(Cell),*Value=TDMCAMT,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"                    
                              Pack Cell,"L",str5          
                            setprop sheet.range(Cell),*Value=nininc,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"            
                            
                              Pack Cell,"M",str5          
                            setprop sheet.range(Cell),*Value=lrinc,*HorizontalAlignment=AlignRight,*NumberFormat="##,####0.00"            

                              Pack Cell,"L",str5
                              Pack Cell1,"K",str5
                              pack Str25,"=","(",Cell,"-",CELL1,")"                       

                              Pack Cell,"N",str5                      
                            setprop sheet.range(Cell),*Value=str25,*HorizontalAlignment=AlignRight                   
.                             If (RUNFLAG = "*")
.                                       move YES,STR1
.                             Else 
.                                       move b1,str1
.                             Endif
.                             Pack Cell,"N",str5  
.                           setprop sheet.range(Cell),*Value=str1,*HorizontalAlignment=AlignLeft   
.                           sheet.range(Cell).Columns.Autofit               
.                             If (MANGFLAG = "*")
.                                       move YES,STR1
.                             Else 
.                                       move b1,str1
.                             Endif                       
.                             Pack Cell,"O",str5  
.                           setprop sheet.range(Cell),*Value=str1,*HorizontalAlignment=AlignRight               
.                             If (RUNFLAG = "*")
.                                       move YES,STR1
.                             Else 
.                                       move b1,str1
.                             Endif                       
.                             Pack Cell,"P",str5  
.                           setprop sheet.range(Cell),*Value=str1,*HorizontalAlignment=AlignRight                       
.
.                             If (INNETS = "*")
.                                       move YES,STR1
.                             Else 
.                                       move b1,str1
.                             Endif                       
.                             Pack Cell,"Q",str5  
.                           setprop sheet.range(Cell),*Value=str1,*HorizontalAlignment=AlignRight                                                   
                              If (PRCLRPASS < 0)
                                        Pack cellrange,"A",str5,":",Cell
                              setprop sheet.range(CellRange).Font,*Bold="True",*Color=255
                              Endif
          return
         
;...........................................................................
         include  tinvio.inc
         include  nordio.inc
.START PATCH 1.9 REPLACED LOGIC
.         include   nbrkio.inc
.         include   nmlrio.inc
          include   compio.inc
          include   cntio.inc
.END PATCH 1.9 REPLACED LOGIC
;begin patch 1.2
         include  nownio.inc
;begin patch 2.0         
;         include  ninvio.inc
          include   ninvio.inc
          INclude   NInvAcdio.inc
;         include  compute.inc
          include   compute.inc
;end patch 2.0         
         include  nbilio.inc
         include  nmrgio.inc
         include  nshpio.inc
         include  ndat3io.inc
         include  nxcgio.inc 
         include  nacdio.inc
         include  ndatio.inc
         include  cvt.inc       
;end patch 1.2
         include  comlogic.inc         
