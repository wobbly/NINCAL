PC       EQU       1                                        ;guid
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.begin patch 2.4
         INCLUDE   CONSACCT.inc
.begin patch 3.3
.         INCLUDE   NINVDD.inc
               INCLUDE        ninvdd.inc
               Include        NInvAcddd.inc
.end patch 3.3
         include   ndatdd.inc
         include   nacddd.inc
         include   nshpdd.inc
.end patch 2.4
         INCLUDE   NORDDD.inc
.         INCLUDE   ORDFDINV.inc

.begin patch 2.5
         INCLUDE   NADJDD.inc
.end patch 2.5
.patch3.2
                              include   compdd.inc
                              include   cntdd.inc
.         include   nbrkdd.inc
.         INCLUDE   NMLRDD.inc
.patch3.2

         INCLUDE   NOWNDD.INC
         INCLUDE   NMOADD.inc
         INCLUDE   NDAT3DD.INC
         include   nmrgdd.inc
         INCLUDE   NMOBDD.inc
.START PATCH 3.1 ADDED LOGIC
          INCLUDE   NSEL2DD.INC
          include   nmoddd.inc
.END PATCH 3.1 ADDED LOGIC
.begin patch 3.7  
           include    nmlddd.inc
.end patch 3.7  

release        init           "3.80"        DLH    .added Days past due
Reldate        Init           "2016 April 25"
.release        init           "3.71"        DLH    .some overdue cleanup
.Reldate        Init           "2015 March 24"
.release        init           "3.7"        DLH    .Use original mail date
.Reldate        Init           "2015 January 26"
.release        init           "3.6"        DLH    .Show if money has been received (order payment is in a control but not yet closed)
..                                                 .and having to double click go was fixed          
.Reldate        Init           "17 May 2012"
.release        init           "3.5"        DLH    if Pure LM unbilled exchange reflect ZERO A/R
.Reldate        Init           "09 Jaunary 2009"
.release        init           "3.4"        DLH   Display mailer on details page
.Reldate        Init           "24 September 2008"
.release        init           "3.3"        DLH   8March2005          Invoice Conversion   
.Reldate        Init           "Sept 6, 2005"
.Release  Init      3.21      DLH 25April2005 fixed Dawson discount price
.RelDate  Init      "April 25, 2005"
.release  init      "3.2"        DMB    26MAY2004 Mailer Conversion   
.Release        Init           "3.1"          ASH  DATACARD CONVERSION
.Reldate        Init           "January 30, 2004"
.Release        Init           "3.03"          DLH Fixed lookup by list # form only allowed 4 bytes, etc
.Reldate        Init           "26 August 2003"
.Release        Init           "3.02"          DLH Add chkn1 to listview objects so users can see if order is in a control.
.Reldate        Init           "22 July 2003"
.Release        Init           "3.01"          DLH breakout totals for unbilled and billed
.Reldate        Init           "14 July 2003"

.Release        Init           "3.0"          DLH GUI redesign
.see archives for old code
.Reldate        Init           "24 April 2003"
.release  init      "2.7"             DLH  03April2001 add broker search
.release  init      "2.6"             JD  07jan00 skip pending/lcr orders.
.release  init      "2.5"            DLH 26aug99 NINadj nadjust Y2K, File expansion
.release  init      "2.4"            DLH 5May99 NININV Y2K, File expansion
.release  init      "2.3"            ASH 29Dec98 NINORD Y2K, File expansion
.release  init      "2.22"           Dlh 17Sep96 - force clear/zero of balance
.                                   variables before read. fixes erroneos dislay
.release  init      "2.21"           DLH 16may95 *uc
.Release  init      "2.2"             JD  30mar95 added dawson brk # getprice
.Release  init      "2.1"            DLH 16mar95 charges from o2des & shipping
.                                   on mag tape.
.RELEASE  INIT      "2.0"            DLH '08MAR95' ESTIMATE ADDITILONAL CHARGES
.                                   ON ESTIMATED/UNBILLED ORDERS
.RELEASE  INIT      "1.9"           DLH 15NOV94 NEW COMPUTE ,CONSACCT.INC
.release  init      "1.8"           25may94 fixed prob with mobks check of mlr.
.release  init      "1.7"           13oct93  dlh display prob & exit at
.                                   1st choice
.release  init      "1.6"           21sep93 DLH added mlr key filter & fixed
.                                  M-I-H display problem.
.RELEASE  INIT      "1.5"           20SEP93  DLH ADD MLR PO FILTER.
.
.RELEASE  INIT      "1.4"           17SEP93  DISPLAY money in house for
.                                  manually paid l/o a/r currently in control.
.RELEASE  INIT      "1.3"          17AUG92  NMOBXX INCLUDES.
.
.RELEASE  INIT      "1.2"          DLH 11MAR92 NMLRXX,NORDXX, MLRHELP,
.                                 NADJXX INCLUDES.
.RELEASE  INIT      "1.1"          D.L. HERRICK   13AUG91
.* *****************************************************************************
.* AMOUNTDUE
.* NAMES IN THE NEWS CALIF. A/R DUE INQUIRY PROGRAM      25APR88
.*
.* CREATED FROM  NINUSAGE/TEXT
.*         AND   CALCINCOME
.*  1990  MAY  10   -  ADDED   1) DETAIL DISPLAY OPTION
.*                             2) DISPLAY OF MONEY ON ACCOUNT.
.* *****************************************************************************
.*
.....................................................
.begin patch 3.0
.Following used only in order to load Search.plf
        include ncmpdd.inc
        include nrtndd.inc
.Following key not used by program but required for search.plf
AKey2   init    "01F"
AKey3   init    "02F"
AKey1A  init    "01L"
AKey2A  init    "02L"
.....................................................
DateBranch  form    1       .'1=REcord date, 2=Trans date, 3=Control #
newdate1    dim    10      mm/dd/ccyy
newdate2    dim    10      mm/dd/ccyy
startdate   form      5
Enddate     form      5
MaskAmount  INIT     "$,$$$,$$9.99-"
MaskAmountAR  INIT     "$$$,$$$,$$9.99-"
okformore      init     "Y"
DateFIlter       Dim            1         On if date filter
ControlFlag      dim            1         On if COntrol filter
str10a           dim     10
DateOkFlag dim     1
NInvstopflag   Dim            1
SelectTOtal    form           7.2
SelectCOunt    FOrm           5
HoldMlr        Dim            4
HoldBRK        Dim            4
GRANDBALANCE   fORM           7.2
Summary        Record         (25)
SumKey         dim            8                                             ;holds BRK+MLR,total
SumAmountAR    form           9.2
SumAmountMOA   form           9.2
               recordend
COunter        Form           2
Avail          Form           2
ARTOTAL        FORM      9.2
.end patch 3.0
. .............................................................................
.
. WORK VARIABLES
.
.Begin patch 3.80
Days     dim       5
holdmdate form     5
.end patch 3.80
CLIENT   DIM       25
BILL     DIM       1
DISDATE  DIM       8
MO       DIM       2
DY       DIM       2
DISPBR   FORM      1               BRANCH FOR DISPLAY TYPE DETAIL,TOTAL
keybr    form      1               branch for secondary filter on mlr key.
Kmlrkey  dim       12
.begin patch 3.0
KPO      Dim       12
.end patch 3.0
CHANGE   FORM      7.2
TOTALDOL FORM      7.2
YR       DIM       2
TOTAL    FORM      9.2
TOTALMLR FORM      8.2
LRTOTAL  FORM      8.2
mfp      form      2
mll      form      2
charge   form      3.2
. 
ADJLR    FORM      8.2
ADJAR    FORM      9.2
PROGNAME DIM       8
M1       FORM      "000"
M2       FORM      "031"
M3       FORM      "059"
M4       FORM      "090"
M5       FORM      "120"
M6       FORM      "151"
M7       FORM      "181"
M8       FORM      "212"
M9       FORM      "243"
M10      FORM      "273"
M11      FORM      "304"
M12      FORM      "334"
LODATE   FORM      5
HIDATE   FORM      5
A        FORM      2
B        FORM      5
C        FORM      2
HLDOWN   DIM       4
CHKJUL   FORM      5
UBCNT    FORM      5
BLCNT    FORM      5
PDCNT    FORM      5
UNBILAMT FORM      9.2
.
. NIN SPECIAL INSTRUCTION CODES, NET NAME%, RUNNING CHARGES
. 
NNET1    INIT      "08 .85 10.00"
NNET2    INIT      "61 .85  8.00"
NNET3    INIT      "64 .80  3.00"
NNET4    INIT      "69 .85  3.00"
NNET5    INIT      "70 .85  4.00"
NNET6    INIT      "71 .90  5.00"
NNET7    INIT      "73 .85  5.00"
NNET8    INIT      "83 .75  5.00"
NNET9    INIT      "84 .85  6.00"
NNET10   INIT      "86 .85  0.00"
NNET11   INIT      "87 .75  3.50"
NNET12   INIT      "92 .85  3.50"
NNET13   INIT      "93 .85  4.50"
NNET14   INIT      "95 .90  3.00"
NNET15   INIT      "A1 .80  0.00"
NNET16   INIT      "A2 .85 15.00"
NNET17   INIT      "A3 .85  7.50"
NNET18   INIT      "A4 .85  6.50"
NNETS    FORM      "18"
.
. CMP SPECIAL INSTRUCTION CODES, NET NAME%, RUNNING CHARGES
.
CNET1    INIT      "07 .85  6.00"
CNET2    INIT      "21 .80  5.00"
CNET3    INIT      "40 .85  6.00"
CNET4    INIT      "52 .75  5.00"
CNET5    INIT      "53 .90  5.00"
CNET6    INIT      "54 .85  4.00"
CNET7    INIT      "57 .85  3.00"
CNET8    INIT      "60 .85  3.50"
CNET9    INIT      "81 .85  5.00"
CNET10   INIT      "86 .85  4.50"
CNET11   INIT      "89 .70  5.00"
CNETS    FORM      "11"
. 
NETPERC  FORM      0.2
RUNCHRG  FORM      2.2
. 
PERC     FORM      2.2
REMPERC  FORM      1.2
AR1      FORM      7.2
AR2      FORM      7.2
FORM2    FORM      2
FORM52   FORM      5.2
FORM7    FORM      7
NUM10    FORM      10             NUMERIC WORK FIELD FOR CONVERSION.
NXTMO    DIM       2
OUTKEY   DIM       17                                                     *R002
MLDYYMM  DIM       4
SYSYYMM  DIM       4
INCPERC  FORM      0.2
UNBILINC FORM      9.2
UNBILTOT FORM      7.2
BILLTOT  FORM      7.2
PAIDTOT  FORM      7.2
PDINC    FORM      7.2
COUNT    FORM      5
CO       FORM      "2"         *NINCA
V1       FORM      2
V2       FORM      2
V3       FORM      2
V4       FORM      2
WRIT     FORM      1
STARTLR  DIM       6
. 
ORDERS   FORM      5
ORDYR    DIM       2
KEY      DIM       28
mrgsw    dim       1
shipsw   dim       1
.
. .............................................................................
.
.
. PROGRAM VARIABLES
. .................
.
KMAILER  DIM       4                  KEYIN FIELD FOR MAILER##;
KLIST    DIM       6                  KEYIN FIELD FOR LIST##;
KBRK     dim       4
QUES     INIT      "????"
VTAB     FORM      2
VTAB1    FORM      2
VTAB2    FORM      2
VALKEY   FORM      1
HIT      FORM      1
DATEBR   FORM      1
DATE     DIM       8
CHKMM    FORM      2                  HOLDS MONTH PARAMETER
CHKYR    FORM      2                  HOLDS YEAR PARAMETER
WORK02   DIM       2
DAY      DIM       2                   "      "
YEAR     DIM       2                   "      "
PASS     FORM      3                  NUMBER OF RECORD DISPLAYS;
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
LISTKY1  DIM       9                 LIST FILE AIM KEY, LIST NUMBER.
LISTKY2  DIM       58                 LIST FILE AIM KEY, LIST NAME.
AKEY1    DIM       3                  USED TO BUILD MAILER & LIST FILE AIM KEY.
CHKMLR   DIM       4                  USED TO ELIMINATE DUP MAILER DISP.
COMPMLR  DIM       4
KEYCOUNT FORM      2                  USED TO CHECK AIM KEY LENGTH, FREE FLOAT
TEN      FORM      "10"
VARIN    FORM      2
COUNTO   FORM      5                  NUMBER OF ORDERS READ.
COUNTO1  FORM      5                  NUMBER OF ORDERS CALCULATED.
COUNTI   FORM      5                  NUMBER OF INVOICES READ
COUNTI1  FORM      5                  NUMBER OF INVOICES CALCULATED
TOTALB   FORM      9.2
TOTALU   FORM      9.2
TENDOLL  FORM      "99999"
NINEDOLL FORM      "199999"
SEVDOLL  FORM      "300000"
SPLITSW   DIM       1                  LIST MANAGEMENT INDICATOR.
SYSJDATE FORM      5
MDATE    FORM      5
JUNDATE  FORM      6
QTYCHK   FORM      9
JUNEDAT  INIT      "060191"
.
.begin patch 3.0
colorfile file
white   color
grey    color
RED     COLOR
BLACK   COLOR
CCField        Dim            2
.
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
.
.Set Up Menu Bar
mFile    menu
.mEdit    menu
mHelp    menu
.Set Up SubMenu for Options
mOptions Menu
sColor  submenu
sSearch submenu
.
.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
OData   init    "&Options;&Search-F2;-;&Color;"
HData   init    "&Help;&About"
.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To"
.Present Data for Colors SubMenu
CData   init    ";&Background;&Text"
.
endindex form      9
Timer   Timer
holdsInfo dim      36

.Define Collections for Object Colors
ColText Collection
ColBack Collection
.................................
coll1     collection
specs     form          4(4)
size      form          "1.000"
infostring          dim        590
Tabnum    form          2
SaveTab   form          2
ListViewNum   form          2

.Define Colors for Each Object
FTC     color
BGC     color

colornum dim   9(2)
Fred     form    3
Fgreen   form    3
Fblue    form    3
colorflag form   1
.............................................................................................................
.Set Vars used for About Box
        move    "NINV0010.PLS",Wprognme
        move    "Amount Due",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        move    Reldate to Wreldate
.end patch 3.0
+..............................................................................
. .............................................................................
. MAINLINE
. .............................................................................
SRCH    plform  Search        
mss1    plform  Error
abt     plform  About
NINV10A plform  NINV010A
NINV10B plform  NINV010B
NINV10C plform  NINV010C
x       plform  NINV010
        winhide
.Load Forms, Always load parent form first
        formload x
        formload NINV10c,NINV010
        formload NINV10b,NINV010
        formload NINV10A,NINV010
        formload abt
        formload mss1
        formload SRCH

.Set tab index
        move    C2,TabNum
.
        
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        CREATE  NINV010;MFile,FData
        create  NINV010;mOptions,OData,mFile
        create  NINV010;mHelp,HData,mOptions
        CREATE  NINV010;sCOlor,Cdata,mOptions,1
.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
        activate mOptions
        activate mHelp,HelpGo,result
        activate sColor,ColorGo,result
.Create SubMenu
        create  NINV010;sSearch,SData,mOptions,1
.Activate SubMenus
        activate sSearch,SearchGo,result        
.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic        
.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=*ltgray
        create  RED=*RED
        create  black=*black

           EVENTREG  X, 17, XRESIZE


.
.Open color file
opencolor
        trap    colorerror if io
        open    colorfile,"c:\program files\nincal\Ninv010.col"
        goto    colorerror if over
        clear   n1
        loop
                add     c1,n1
                read    colorfile,seq;colornum(n1)
                until   over
                until   (n1 = 2)
        repeat
        close   colorfile
        trapclr io
        unpack  colornum(1),Fred,Fgreen,Fblue
        create  FTC=Fred:Fgreen:Fblue
        setprop ColText,fgcolor=FTC
        
        unpack  colornum(2),Fred,Fgreen,Fblue
        create  BGC=Fred:Fgreen:Fblue
        setprop ColBack,bgcolor=BGC
aftercolor   
.end patch 3.0
:..................................................................................................................
         TRAP      IO GIVING ERROR NORESET IF IO
         TRAP      RANGE GIVING ERROR NORESET IF RANGE
         TRAP      FORMAT GIVING ERROR NORESET IF FORMAT
         TRAP      PARITY GIVING ERROR NORESET IF PARITY
         MOVE      "                    " TO FERROR
.
.begin patch xxx
         move      c3 to nmldlock           mail date change file        
.end patch xxx

.begin patch 3.0
         move      c1 to nordpath
         CLOCK     DATE TO DATE
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         MOVE      DATE TO TODAY
.Create Ninv010aListView001 Columns
.Column Clicking
        Ninv010aListView001.InsertColumn using "LR",50,1
        Ninv010aListView001.InsertColumn using "Order Date",70,2
        Ninv010aListView001.InsertColumn using "Mail Date",70,3
        Ninv010aListView001.InsertColumn using "List",180,4
        Ninv010aListView001.InsertColumn using "Order Qty",70,5
        Ninv010aListView001.InsertColumn using "Amount",80,6
        Ninv010aListView001.InsertColumn using "Inv Date",70,7
.        Ninv010aListView001.InsertColumn using "Check ##",65,8
        Ninv010aListView001.InsertColumn using "Days",65,8
        Ninv010aListView001.InsertColumn using "Check ##",65,9
        Ninv010aListView001.SetColumnFormat using 5,1              .set column justify right
        Ninv010aListView001.SetColumnFormat using 6,1              .set column justify right
.
.Create Ninv010aListView002 Columns by Invoice date
.Column Clicking
        Ninv010aListView002.InsertColumn using "Inv Date",0,1                    ;unformated so sort works
        Ninv010aListView002.InsertColumn using "Inv Date",75,2
        Ninv010aListView002.InsertColumn using "LR",50,3
        Ninv010aListView002.InsertColumn using "Order Date",75,4
        Ninv010aListView002.InsertColumn using "Mail Date",75,5
        Ninv010aListView002.InsertColumn using "List",180,6
        Ninv010aListView002.InsertColumn using "Order Qty",75,7
        Ninv010aListView002.InsertColumn using "Amount",80,8
.        Ninv010aListView002.InsertColumn using "Check ##",65,9
        Ninv010aListView002.InsertColumn using "Days",65,9
        Ninv010aListView002.InsertColumn using "Check ##",65,10
        Ninv010aListView002.SetColumnFormat using 8,1              .set column justify right
        Ninv010aListView002.SetColumnFormat using 7,1              .set column justify right
.
.Create Ninv010aListView003 Columns by Mail Date
.Column Clicking
        Ninv010aListView003.InsertColumn using "Mail Date",0,1                    ;unformated so sort works
        Ninv010aListView003.InsertColumn using "Mail Date",75,2
        Ninv010aListView003.InsertColumn using "LR",50,3
        Ninv010aListView003.InsertColumn using "Order Date",75,4
        Ninv010aListView003.InsertColumn using "List",180,5
        Ninv010aListView003.InsertColumn using "Order Qty",75,6
        Ninv010aListView003.InsertColumn using "Amount",80,7
        Ninv010aListView003.InsertColumn using "Inv Date",75,8
.        Ninv010aListView003.InsertColumn using "Check ##",65,9
        Ninv010aListView003.InsertColumn using "Days",65,9
        Ninv010aListView003.InsertColumn using "Check ##",65,10
        Ninv010aListView003.SetColumnFormat using 6,1              .set column justify right
        Ninv010aListView003.SetColumnFormat using 7,1              .set column justify right

.
.Create Ninv010aListView004 Columns 
        Ninv010aListView004.InsertColumn using "Order Date",0,1                    ;unformated so sort works
        Ninv010aListView004.InsertColumn using "Order Date",75,2
        Ninv010aListView004.InsertColumn using "LR",50,3
        Ninv010aListView004.InsertColumn using "Mail Date",75,4
        Ninv010aListView004.InsertColumn using "List",180,5
        Ninv010aListView004.InsertColumn using "Order Qty",75,6
        Ninv010aListView004.InsertColumn using "Amount",80,7
        Ninv010aListView004.InsertColumn using "Inv Date",75,8
        Ninv010aListView004.InsertColumn using "Days",65,9
        Ninv010aListView004.InsertColumn using "Check ##",65,10
.        Ninv010aListView004.InsertColumn using "Check ##",65,9
        Ninv010aListView004.SetColumnFormat using 6,1              .set column justify right
        Ninv010aListView004.SetColumnFormat using 7,1              .set column justify right

.

.Create Ninv010bListView001 Columns by Broker
        Ninv010bListView001.InsertColumn using "Broker",200,1
        Ninv010bListView001.InsertColumn using "Mailer",200,2
        Ninv010bListView001.InsertColumn using "Amount Due",100,3
        Ninv010bListView001.InsertColumn using "MOA",75,4
        Ninv010bListView001.SetColumnFormat using 3,1              .set $ column justify right
        Ninv010bListView001.SetColumnFormat using 4,1              .set $ column justify right
.Create Ninv010bListView002 Columns by Broker
        Ninv010bListView002.InsertColumn using "Mailer",200,1
        Ninv010bListView002.InsertColumn using "Broker",200,2
        Ninv010bListView002.InsertColumn using "Amount Due",100,3
        Ninv010bListView002.InsertColumn using "MOA",75,4
        Ninv010bListView002.SetColumnFormat using 3,1              .set $ column justify right
        Ninv010bListView002.SetColumnFormat using 4,1              .set $ column justify right

.Set Error Message Stat Text Boxes
        call    SetNINVErrorMssgDefault
. .................................................................
        move        c1 to n1
          call        NInvTabChange
        setfocus Ninv010EditText001
        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat
        goto    timeout
..............................................................................................................
.end patch 3.0
.
NOGOOD
.begin patch 3.0
               alert   caution,"NOT ENOUGH INFORMATION TO SEARCH ON!!!",result
               setfocus       Ninv010Edittext001
               return
.end patch 3.0
.*......................................................................
.begin patch 3.0
.........................................................................................................
.LoadNInvSearchList - sort out keys being used and go to it
LoadNInvSearchList
               setprop Ninv010aListView001,visible=1
               setprop Ninv010aListView003,visible=0
               setprop Ninv010aListView002,visible=0
               setprop Ninv010aListView004,visible=0
               Ninv010aListView001.EnsureVisible using c1,0
               setfocus Ninv010alistview001
               call           SetNInvDates
               Clear          Mlr
               CLear          KBrk
               Clear          Klist
               Getitem        Ninv010EditText001,0,Mlr
               Getitem        Ninv010EditText002,0,Kbrk
               Getitem        Ninv010EditText005,0,KList
               Getitem        NInv010EditText003,0,KPO
               Getitem        NInv010EditText004,0,KMlrKey
.begin patch 3.03
               call           debug
               call           trim using Klist
               count          N2,Klist
               If             (N2 > 0)
                              if             (N2 = 1)
                              pack           str6 from "00000",klist
                              elseif         (N2 = 2)
                              pack           str6 from "0000",klist
                              elseif         (N2 = 3)
                              pack           str6 from "000",klist
                              elseif         (N2 = 4)
                              pack           str6 from "00",klist
                              elseif         (N2 = 5)
                              pack           str6 from "0",klist
                              elseif         (N2 = 6)
                              pack           str6 from klist
                              endif
               Move           str6 to Klist
               endif
.end patch 3.03
               Move           mlr to holdmlr
               Move           KBrk to holdBrk
               IF             (MLR = " " or Mlr = "") 
               Clear          Nordfld1
               else
               packkey        Nordfld1 from "01R",mlr
               endif
               IF             (KList = " " or KList = "") 
               Clear          Nordfld2
               else
               packkey        Nordfld2 from "02R",Klist
               endif
               IF             (KPO = " " or KPO = "") 
               Clear          Nordfld3
               else
               packkey        Nordfld3 from "03L",KPO
               endif
               IF             (Kbrk = " " or Kbrk = "") 
               Clear          Nordfld4
               else
               packkey        Nordfld4 from "04L",Kbrk
               endif
.
               CALL      KEYEDIT
               COMPARE   "3",VALKEY
               GOTO      NOGOOD IF EQUAL
               MOVE      C2 TO NORDPATH
               CALL      NORDAIM
               If             Not Over
               add       c1 to counto
               move           CountO to str5
               setitem        Ninv010aEditText003,0,str5
               call           CheckFilters
               goto           NInvsearchlistloop
               else
                              IF             (Nmoapath = c4)
                              goto           NINVSearchListLoop
                              endif
               goto           ExitNINvSearchList
               endif
NInvSearchListLoop
               Loop
               call           NOrdKg
               If             Not Over
               add            c1 to counto
               move           CountO to str5
               setitem        Ninv010aEditText003,0,str5
               Move           Yes to OKforMore
                              clear          invdtem
                              clear          invdted
                              clear          invdtey
                              clear          invdteC
                              clear          chkn1
                              clear          invnum

               Call           CheckFilters
               Else
               MOve           No to OKForMore
               endif
               UNtil          (OKForMore = No)
               Repeat
.ok when done with details load up summary  If search by Mailer or brk/mailer we want totals for all accounts the mlr
. may have
.if by broker only all totals for all accounts
NMobSearchListLoop
               Getitem        NINV010EditText001,0,mlr
               Getitem        NINV010EditText002,0,Nmobbrk
               If             (mlr <> "" & Nmobbrk ="")
               move           c1 to nmobpath
               packkey        nmobfld from mlr
               ElseIF         (Nmobbrk <> "" & Nmobbrk <> "0000" & (mlr = "0000" or mlr = ""))
               move           c2 to nmobpath
               packKey        nmoafld4 from nmobbrk,mlr
               REP            ZFILL IN NMOAFLD4
               endif
               move           no to NmobMsgFlag

               call           nmobkey
                              if             not over
                              call           BuildSummaryRec
                              endif
              loop
              call            nmobks
.patch3.01
.the over flag get's cleared when the message box comes up never resulting in an over even if over is true
.             if              (over <> YES)
            if              not over
.patch3.01
                             if              (mlr = Nmobmlr & Nmobpath = c1)
                             move            yes to okformore
                             call            BuildSummaryRec
                             Elseif         (HOldbrk = Nmobbrk & Nmobpath = c2)
                             move            yes to okformore
                             call            BuildSummaryRec
                             endif
                else            
.patch3.01          
                    move NO to OVER
.patch3.01         
                             move            No to OkForMore
                endif
.patch3.01
              Until          (OkForMore = No)
              repeat
              call            LoadSummaryListView
              return
...........................................................................................................
BuildSummaryRec
               Clear          Str8
               pack           str8 from Nmobbrk,Nmobmlr
               rep            zfill in  str8
               MOve           c0 to Avail
          FOR counter,"1","25"
                              IF ( STR8=Summary(counter).SumKey)
                              MOve  Balance to Summary(Counter).SumAmountMOA
                              Break
.                              Goto   ExitBuildSummaryRec
                              endif
                              IF ( Summary(counter).SumKey = "" & avail=0)
                              MOve           Counter to Avail
                              endif
               repeat
               if             (avail <> 0)
               MOVe           Str8 to Summary(Avail).Sumkey
               move           Balance to Summary(Avail).SumAmountMOA
               endif
ExitBuildSummaryRec
               return
...........................................................................................................
loadSummaryListView
               move           c0 to ARTOTAL
          FOR counter,"1","25"
                              Move Summary(counter).SumKey,str8
                              Move Summary(Counter).SumAmountAR,total
                              Move Summary(Counter).SumAmountMoa,Balance
               If             (str8 = "")
               Break
               endif
               unpack         str8 into nmobbrk,nmobmlr
               Clear          Mcomp
               packkey        Mkey from nmobMLR,z3
               call           nmlrkey
               clear          brcomp
               if             (Nmobbrk <> "" & Nmobbrk <> "0000")
               packkey        Nbrkfld from nmobbrk,z3
               call           nbrkkey
               endif
               Ninv010bListView001.InsertItem giving N9 using brcomp
               Ninv010bListView001.SetItemText using N9,mcomp,1
               move           MaskAmountAR to Str15
               edit           total to str15
               add            total to artotal
               Ninv010bListView001.SetItemText using N9,str15,2
               move           MaskAmount to Str13
               edit           Balance to str13
               Ninv010bListView001.SetItemText using N9,str13,3
               Ninv010bListView001.SetColumnFormat using 5,2              .set $ column justify right
               Ninv010bListView001.SetColumnFormat using 5,3              .set $ column justify right
.
               Ninv010bListView002.InsertItem giving N9 using Mcomp
               Ninv010bListView002.SetItemText using N9,brcomp,1
               move           MaskAmountAR to Str15
               edit           total to str15
               Ninv010bListView002.SetItemText using N9,str15,2
               move           MaskAmount to Str13
               edit           Balance to str13
               Ninv010bListView002.SetItemText using N9,str13,3
               Ninv010bListView002.SetColumnFormat using 5,2              .set $ column justify right
               Ninv010bListView002.SetColumnFormat using 5,3              .set $ column justify right
.add grand total goodies
               MULT           "-1" by BALANCE
               ADD            BALANCE TO GRANDBALANCE
               repeat
               move           MaskAmount to Str13
               edit           GrandBalance to str13
               Setitem        Ninv010bEditText001,0,str13
               MOve           MaskAmountAr to str15
               edit           ARTotal to str15
               Setitem        Ninv010bEditText002,0,str15
               return
...........................................................................................................
CheckFilters
               CMATCH         "p" TO OSTAT       Pending order ?
               return         IF EQUAL     YES, skip.
               CMATCH         "x" TO OSTAT       Pending cancelled order ?
               return         IF EQUAL     YES, skip.
               CMATCH         "l" TO OSTAT       LCR order ?
               return         IF EQUAL     YES, skip.
               CMATCH         "z" TO OSTAT       cancelled LCR order ?
               return         IF EQUAL     YES, skip.
               CMATCH         "X" TO OSTAT
               RETURN         IF EQUAL
               if             (olrn = "491962")
               call           debug
               endif
               IF             (KMLrkey <> "" & KMlrKEy <> " ")
               match          Kmlrkey to omlrky
               return         if not equal
               endif
               if             (DateBranch = c2)                            ;order date
                              move           OODtem to mm
                              move           OODted to dd
                              move           OODtey to yy
                              Call           cvtjul
                              if          (startdate > 0 and enddate > 0)
                                              if       (startdate <= juldays and juldays <= enddate)
                                              else
                                              Goto      ExitCheckFilters1     
                                              endif
                              endif
               Elseif        (DateBranch = c3)                             ;Mail Date
                              move           OMdtem to mm
                              move           OMdted to dd
                              move           OMdtey to yy
                              Call           cvtjul
                              if          (startdate > 0 and enddate > 0)
                                              if       (startdate <= juldays and juldays <= enddate)
                                              else
                                              Goto      ExitCheckFilters1     
                                              endif
                              endif
               Elseif        (DateBranch = c5)                             ;return Date
                              move           ORTNDTEm to mm
                              move           ORTNDTED to dd
                              move           ORTNDTEy to yy
                              Call           cvtjul
                              if          (startdate > 0 and enddate > 0)
                                              if       (startdate <= juldays and juldays <= enddate)
                                              else
                                              Goto      ExitCheckFilters1     
                                              endif
                              endif
               endif
.
.
.Begin patch 3.7
              pack            NMLDFLD1,"01X",OLRN
        clear   str8
              pack            str8,"99999999"
              call            NMLDAIM
              loop
               until over
               if (NMLDDATE < str8)
                              move           NMLDDATE,str8
               endif
               call           NMLDKG
              repeat
              if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
         unpack     str8 into omdtec,omdtey,omdtem,omdted
              else
.Use current Mail Date
              endif
.end patch 3.7
               If             (Ostat = "B" or Ostat = "Q")            ;billed
               MOVE           C1 TO NINVPATH
               MOVE           OLRN TO NINVFLD
               call           Ninvkey
                              If             over
                              goto           ExitCheckFIlters1
                              else
                                             if        (DateBranch = c4)                             ;INV Date
                                             move           INVdtem to mm
                                             move           INVdteD to dd
                                             move           INVdtey to yy
                                             Call           cvtjul
                                                            if          (startdate > 0 and enddate > 0)
                                                                if       (startdate <= juldays and juldays <= enddate)
                                                                else
                                                                Goto      ExitCheckFilters1     
                                                                endif
                                                            endif
                                             endif
                              add            c1 to counti
                              unpack         chkn1 into str4
.                                             IF             (StatB <> "P" & str4 <> "CASH")
                                             IF             (StatB = "P" or str4 = "CASH")
                                             goto           ExitCHeckFilters1
                                             else
                                             MOVE      OLON TO NOWNFLD
                                             CALL      NOWNKEY
                                             MOVE     YES TO SUBPPSW
                                             MOVE      NinvFLD to nmrgfld
                                             REP       ZFILL IN NMRGFLD
                                             move      c0 to nmrgrqty
                                             move      c0 to nmrgiqty
                                             move      c0 to nmrgnet
                                             CALL      NMRGKEY
                                                            if             not over
                                                            move           yes to mrgsw
                                                            else
                                                            move           no to mrgsw
                                                            endif
                                             move      lrn to nshpfld
                                             call      nshpkey
                                                            if             not over
                                                            move           yes to shipsw
                                                            else
                                                            move           no to shipsw
                                                            endif
                                             move      olnum to ndatfld
                                             MOVE      C1 TO NDATPATH
                                             call      ndatkey
                                             call      wipecvars
                                                                                                                                call           NInvAcdRecClear
                                           CLEAR          NInvAcdfld
                                               packkey           NInvAcdFld from Invnum
.               call           NInvAcdRecClear
                                            call           NinvAcdTst
                                            Call           NInvAcdRecLoad

                                             CALL      COMPUTE
                                             MOVE      LRN TO NADJFLD
                                             call      Nadjkey
                                                            If             Not over
                                                            add        asrecadj to formar
                                                            endif
                              ADD            FORMAR TO TOTAL
                              ADD            FORMAR TO TOTALB
                              add            c1 to CountI1
                              endif                                           
               endif
               Else                            ;not billed lets estimate
               clear          INvdtec
               clear          INvdtey
               clear          INvdtem
               clear          INvdted
               move           c0 to formar
               SUB       CMPT92 FROM CMPT92
               SUB       FORM52 FROM FORM52
               MOVE      OQTY TO CMPT92
               DIV       THOUS INTO CMPT92
.START PATCH 3.1 REPLACED LOGIC
.               MOVE      OPPM TO FORM52
.               DIV       HUND INTO FORM52
                    packkey   NSEL2FLD,"1",OLRN
                    move      "NSEL2KEY",Location
                    pack      KeyLocation,"Key: ",NSEL2FLD
                    call      NSEL2KEY
                    if over
                              move      O2DES,NSEL2NAME
                              unpack    OPPM,str3,str2
                              pack      str6,str3,".",str2
                              rep       zfill,str6
                              move      str6,NSEL2PRICE
.                             move      "/m",NMODDESC
.                   else
.                             pack      NMODFLD,NSEL2DESC
.                             rep       zfill,NMODFLD
.                             move      "NMODKEY",Location
.                             pack      KeyLocation,"Key: ",NMODFLD
.                             call      NMODKEY
.                             if over
.                                       move      "/m",NMODDESC
.                             endif
                    endif
                    move      NSEL2PRICE,FORM52
.END PATCH 3.1 REPLACED LOGIC
               MULT      FORM52 BY CMPT92
               MOVE      CMPT92 TO UNBILINC
.lets check for known additional charges.
.one media charges.
               MATCH     B2,OFOCODE
               goto      chkspi if equal       .none check special instructions
               MOVE      C0 TO n2
               TYPE      OFOCODE
               GOTO      MED10 IF NOT EQUAL
               MOVE      OFOCODE TO N2
               GOTO      gotmedia
MED10          REP       "A0B1C2D3E4F5G6H7I8J9" IN OFOCODE
               TYPE      OFOCODE
               GOTO      MED20 IF NOT EQUAL
               MOVE      OFOCODE TO N2
               ADD       C10 TO N2
               GOTO      gotmedia
MED20          REP       "K0L1M2N3O4P5Q6R7S8T9" IN OFOCODE
               TYPE      OFOCODE
               GOTO      MED30 IF NOT EQUAL
               MOVE      OFOCODE TO N2
               ADD       "20" TO N2
               GOTO      gotmedia
MED30          REP       "U0V1X2Y3Z4" IN OFOCODE
               MOVE      OFOCODE TO N2
               ADD       "30" TO N2
gotmedia       branch    n2 of nomed$,nomed$,nomed$,nomed$,nomed$,nomed$,nomed$:
                         nomed$,nomed$,med$15,med$20,med$25,med$30:
                         med$15,med$20,med$25,med$30,med$7m
nomed$         goto      chkspi           .no media charges identified.           
.
med$15         add       "15" to unbilinc      .tape fee
               add       "25" to unbilinc      .shipping fee
               goto      chkspi
med$20         add       "20" to unbilinc      .tape fee
               add       "25" to unbilinc      .shipping fee
               goto      chkspi
med$25         add       "25" to unbilinc      .tape fee
               add       "25" to unbilinc      .shipping fee
               goto      chkspi
med$30         add       "30" to unbilinc      .tape fee
               add       "25" to unbilinc      .shipping fee
               goto      chkspi
med$7m
               SUB       CMPT92 FROM CMPT92       .clear 
               MOVE      OQTY TO CMPT92            .move order qty to numeric
               DIV       THOUS INTO CMPT92         
               mult      c7 by CMPT92
               add       CMPT92 to unbilinc
               goto      chkspi
.chkspi lets check for charges buried in spec instructions.
chkspi   
....sigh OSPI is defunct  
               MOVE      "999-999-999-999-999-999-999-999" TO STR35
               EDIT      OSPI TO STR35
               REP       ZFILL IN STR35
               SCAN      "080" IN STR35
               CALL      SEL$10M IF EQUAL
               RESET     STR35      
                        SCAN      "071" IN STR35
                        CALL      SEL$48M IF EQUAL
                        RESET     STR35     
                        SCAN      "078" IN STR35
                        IF        EQUAL
                        CALL      SEL$4M
                        CALL      SEL$5M
                        ENDIF
                        RESET     STR35      
                        SCAN      "039" IN STR35
                        CALL      SEL$12M IF EQUAL
                        RESET     STR35      
                        SCAN      "038" IN STR35
                        CALL      SEL$48M IF EQUAL
                        RESET     STR35      
                        SCAN      "034" IN STR35
                        CALL      SEL$15M IF EQUAL
                        RESET     STR35      
                        SCAN      "035" IN STR35
                        CALL      SEL$20M IF EQUAL
                        RESET     STR35      
                        SCAN      "028" IN STR35
                        CALL      SEL$1_5M IF EQUAL
                        RESET     STR35      
                        SCAN      "024" IN STR35
                        CALL      SEL$1M IF EQUAL
                        RESET     STR35      
                        SCAN      "002" IN STR35
                        CALL      SEL$2_5M IF EQUAL
                        RESET     STR35      
                        SCAN      "004" IN STR35
                        CALL      SEL$2_5M IF EQUAL
                        RESET     STR35      
                        SCAN      "006" IN STR35
                        CALL      SEL$2_5M IF EQUAL
                        RESET     STR35      
                        SCAN      "008" IN STR35
                        CALL      SEL$2_5M IF EQUAL
                        RESET     STR35      
                        move      b1,str1
                        move      c0 to mfp
                        move      c0 to mll
scanslct
.START PATCH 3.1 REPLACED LOGIC
.                        scan       "@$" in o2des              .any charges
.                        goto       scanexch if not equal      .nope
.                        bump       o2des by 2                .found one, get ready 
.                        movefptr   o2des to mfp               .set formpointer
.o1                      bump       o2des                      .forward one more
.                        goto       scanexch if eos            .oops nothing there get out
.                        cmove      o2des to str1              .get character
.                        type       str1                      .numeric ?
.                        goto       o1 if equal               .yes, we will take it
.                        cmatch     "." to str1               .decimal
.                        goto       o1 if equal               .yes, we will take it
.                        movefptr   o2des to mll               .get current position
.                        reset      o2des to mfp              .reset formpointer
.                        sub        c1 from mll          .move lp back to last good character
.                        setlptr    o2des to mll              .set length pointer
.                        move       o2des to str6             .take it
.                        move       str6 to charge
.                        call       sel$                      .calc it
.                        move       mll to mfp                .get ready to look again
.                        setlptr    o2des to 35                .reset length to max
.                        reset      o2des to mfp               .reset formpointer past 1st hit
.                        goto       scanslct                   .try again
........................................................
                              call      Trim using NSEL2NAME
                        scan       "@$" in NSEL2NAME              .any charges
                        goto       scanexch if not equal      .nope
                        bump       NSEL2NAME by 2                .found one, get ready 
                        movefptr   NSEL2NAME to mfp               .set formpointer
o1                      bump       NSEL2NAME                      .forward one more
                        goto       scanexch if eos            .oops nothing there get out
                        cmove      NSEL2NAME to str1              .get character
                        type       str1                      .numeric ?
                        goto       o1 if equal               .yes, we will take it
                        cmatch     "." to str1               .decimal
                        goto       o1 if equal               .yes, we will take it
                        movefptr   NSEL2NAME to mll               .get current position
                        reset      NSEL2NAME to mfp              .reset formpointer
                        sub        c1 from mll          .move lp back to last good character
                        setlptr    NSEL2NAME to mll              .set length pointer
                        move       NSEL2NAME to str6             .take it
                        move       str6 to charge
                        call       sel$                      .calc it
                        move       mll to mfp                .get ready to look again
                        setlptr    NSEL2NAME to 35                .reset length to max
                        reset      NSEL2NAME to mfp               .reset formpointer past 1st hit
                        goto       scanslct                   .try again
.END PATCH 3.1 REPLACED LOGIC
.
SEL$     
         SUB       CMPT92 FROM CMPT92       .clear 
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92         
         mult      charge by CMPT92
         add       CMPT92 to unbilinc
         display   *p1:23,*el,"sel$ ",CMPT92,*w2
         RETURN
.
.
SEL$1M  
         SUB       CMPT92 FROM CMPT92       .clear 
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92         
         mult      c1 by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.
SEL$1_5M
         SUB       CMPT92 FROM CMPT92       .clear 
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92         
         mult      "1.5" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.         
SEL$2_5M 
         SUB       CMPT92 FROM CMPT92       .clear 
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92         
         mult      "2.5" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.         
SEL$4M   
         SUB       CMPT92 FROM CMPT92       .clear 
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92         
         mult      "4" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.         
SEL$48M  
         SUB       CMPT92 FROM CMPT92       .clear 
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92         
         mult      "4.8" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.         
SEL$5M   
         SUB       CMPT92 FROM CMPT92       .clear 
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92         
         mult      "5" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.         
SEL$10M  
         SUB       CMPT92 FROM CMPT92       .clear 
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92         
         mult      c10 by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.         
SEL$12M  
         SUB       CMPT92 FROM CMPT92       .clear 
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92         
         mult      "12" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.         
SEL$15M  
         SUB       CMPT92 FROM CMPT92       .clear 
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92         
         mult      "15" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.
SEL$20M  
         SUB       CMPT92 FROM CMPT92       .clear 
         MOVE      OQTY TO CMPT92            .move order qty to numeric
         DIV       THOUS INTO CMPT92         
         mult      "20" by CMPT92
         add       CMPT92 to unbilinc
         RETURN
.         

.check for exchange
SCANEXCH       RESET     EXCODES
               SCAN      OELCODE IN EXCODES
               GOTO      OKEX IF EQUAL
               GOTO      OK
.OKEX - CHECK FOR SPLIT RENTAL/EXCHANGE.
OKEX
               MOVE      C0 TO CMPT92
               MOVE      OEXQTY TO CMPT92
               COMPARE   C0 TO CMPT92            PURE EXCHANGE ?
               IF        EQUAL                 YES.
               MOVE      C0 TO QTYCHK
               MOVE      NO TO SPLITSW
               MOVE      OQTY TO QTYCHK
               MOVE      QTYCHK TO CMPT92
.test
                    pack      str2 from Osales10,Osales
                    move      c0,n2
                    move      str2,n2
                    if        (N2 = 6 or n2 = 27)    .list management exchange NO ar from mailer
                    move      c0,qtychk
                    move      c0,cmpt92
                    endif
               GOTO      GETPRICE
                             ELSE
               MOVE      YES TO SPLITSW
               MOVE      C0 TO QTYCHK
               MOVE      OEXQTY TO QTYCHK
               MOVE      QTYCHK TO CMPT92
               GOTO      GETPRICE
               ENDIF
.
GETPRICE
                        PACK       MKEY FROM OMLRNUM,OCOBN
                        REP       " 0" IN MKEY
                        CALL       NMLRKEY
                        SCAN       "DAWSON" IN MCCTO
                        IF         EQUAL
.appears to have been incorrect since 95  should be $5
.                        MOVE       C7 TO FORM52
                        MOVE       C5 TO FORM52
                        GOTO       CALCE
                        ENDIF
                        match      "0171" to obrknum
                        IF         EQUAL
.appears to have been incorrect since 95  should be $5
.                        MOVE       C7 TO FORM52
                        MOVE       C5 TO FORM52
                        GOTO       CALCE
                        ENDIF
                         UNPACK    JUNEDAT INTO MM,DD,YY
                        CALL      CVTJUL           *CONVERT JUNE 1ST'S DATE TO JULIAN
                        MOVE      JULDAYS TO JUNDATE    *SAVE RESULT
                        MOVE      OODTEM TO MM
                        MOVE      OODTED TO DD
                        MOVE      OODTEY TO YY
                        CALL      CVTJUL           *CONVERT TODAY'S  DATE TO JULIAN
                        MOVE      JULDAYS TO MDATE    *SAVE RESULT
                        COMPARE   JUNDATE TO MDATE
                        IF        NOT GREATER
                        MOVE      C8 TO FORM52
                        GOTO      CALCE
                        ENDIF
                        COMPARE   QTYCHK TO TENDOLL
                        IF        NOT LESS
                        MOVE      C10 TO FORM52
                        GOTO      CALCE
                        ENDIF
                        COMPARE   QTYCHK TO NINEDOLL
                        IF        NOT LESS
                        MOVE      C9 TO FORM52
                        GOTO      CALCE
                        ENDIF
                        COMPARE   SEVDOLL TO QTYCHK
                        IF        NOT LESS
                        MOVE      C7 TO FORM52
                        ENDIF
                        MOVE      C8 TO FORM52
CALCE
                        DIVIDE    THOUS INTO CMPT92
                        MULTIPLY  FORM52 BY CMPT92
                        MOVE      CMPT92 TO UNBILAMT
                        CMATCH    YES TO SPLITSW
                        IF        EQUAL
                        GOTO      RENTPART
                        ELSE
                        MOVE      CMPT92 TO UNBILINC
                        GOTO      OK
                        ENDIF
.
RENTPART 
                        MOVE      C0 TO CMPT92          SPLIT RENT/EXCHANGE
                        MOVE      C0 TO N9
                        MOVE      OQTY TO CMPT92
                        MOVE      OEXQTY TO N9
                        SUBTRACT  N9 FROM CMPT92           GET RENTAL PORTION
                        MULT      ".001" BY CMPT92
                        MOVE      "65.00" TO FORM52          *ESTIMATED $.   (USE DATACARD?)
                        MULT      FORM52 BY CMPT92
                        ADD       CMPT92 TO UNBILAMT
                        MOVE      UNBILAMT TO UNBILINC
                        GOTO      OK

.
OK       
                        MOVE      UNBILINC TO CMPT92
                        ADD       CMPT92 TO TOTALU
                        ADD       CMPT92 TO TOTAL
                        ADD       C1 TO COUNTO1
                        MOVE      CMPT92 TO FORMAR
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
               endif
               If             (olnum <> Ndatfld)     ;we need datacard info
               packkey        ndatfld from olnum
               move           c1 to ndatpath
               call           ndatkey
               endif

.               call           debug
               move           MaskAmountAR to Str15
               edit           formar to str15
               Ninv010aListView001.InsertItem giving N9 using Olrn
               clear     str10
               pack      str10 from Oodtem,slash,oodted,slash,oodtec,oodtey
               NInv010aListView001.SetItemText using N9,str10,1
               clear     str10
               pack      str10 from Omdtem,slash,omdted,slash,omdtec,omdtey
               Ninv010aListView001.SetItemText using N9,str10,2
               Ninv010aListView001.SetItemText using N9,Olstname,3
               move           oqty to str9
               call           formatnumeric using str9,str11
               Ninv010aListView001.SetItemText using N9,str11,4
               Ninv010aListView001.SetColumnFormat using 4,1              .set $ column justify right
               Ninv010aListView001.SetItemText using N9,str15,5
               clear     str10
               pack      str10 from invdtem,slash,invdted,slash,invdtec,invdtey
               call           trim using str10
               if             ("//" = str10)
               clear          str10
               endif
               Ninv010aListView001.SetItemText using N9,str10,6
.begin patch 3.80
.               Ninv010aListView001.SetItemText using N9,chkn1,7
           call       GetDays
               Ninv010aListView001.SetItemText using N9,days,7
               Ninv010aListView001.SetItemText using N9,chkn1,8
.end patch 3.80
               Ninv010aListView001.SetColumnFormat using 5,1              .set $ column justify right
.
               clear     str10
               pack      str10 from invdtec,invdtey,invdtem,invdted
               Ninv010aListView002.InsertItem giving N9 using str10
               clear     str10
               pack      str10 from invdtem,slash,invdted,slash,invdtec,invdtey
               Ninv010aListView002.SetItemText using N9,str10,1
               Ninv010aListView002.SetItemText using N9,olrn,2
               clear     str10
               pack      str10 from Oodtem,slash,oodted,slash,oodtec,oodtey
               Ninv010aListView002.SetItemText using N9,str10,3
               clear     str10
               pack      str10 from Omdtem,slash,omdted,slash,omdtec,omdtey
               Ninv010aListView002.SetItemText using N9,str10,4
               Ninv010aListView002.SetItemText using N9,Olstname,5
               Ninv010aListView002.SetItemText using N9,str11,6
               Ninv010aListView002.SetItemText using N9,str15,7
.begin patch 3.80
.               Ninv010aListView002.SetItemText using N9,chkn1,8
           call       GetDays
               Ninv010aListView002.SetItemText using N9,days,8
               Ninv010aListView002.SetItemText using N9,chkn1,9
.end patch 3.80
               Ninv010aListView002.SetColumnFormat using 6,1              .set $ column justify right
               Ninv010aListView002.SetColumnFormat using 7,1              .set $ column justify right
.
               clear     str10
               pack           str10 from omdtec,omdtey,omdtem,omdted
               Ninv010aListView003.InsertItem giving N9 using str10
               clear     str10
               pack      str10 from Omdtem,slash,omdted,slash,omdtec,omdtey
               Ninv010aListView003.SetItemText using N9,str10,1
               Ninv010aListView003.SetItemText using N9,olrn,2
               clear     str10
               pack      str10 from Oodtem,slash,oodted,slash,oodtec,oodtey
               Ninv010aListView003.SetItemText using N9,str10,3
               Ninv010aListView003.SetItemText using N9,Olstname,4
               Ninv010aListView003.SetItemText using N9,str11,5
               Ninv010aListView003.SetColumnFormat using 5,1              .set $ column justify right
               Ninv010aListView003.SetItemText using N9,str15,6
               Ninv010aListView003.SetColumnFormat using 6,1              .set $ column justify right
               clear     str10
               pack      str10 from invdtem,slash,invdted,slash,invdtec,invdtey
               Ninv010aListView003.SetItemText using N9,str10,7
.begin patch 3.80
.               Ninv010aListView003.SetItemText using N9,chkn1,8
           call       GetDays
               Ninv010aListView003.SetItemText using N9,days,8
               Ninv010aListView003.SetItemText using N9,chkn1,9
.end patch 3.80

.
               clear     str10
               pack           str10 from oodtec,oodtey,oodtem,oodted
               Ninv010aListView004.InsertItem giving N9 using str10
               clear     str10
               pack      str10 from Oodtem,slash,oodted,slash,oodtec,oodtey
               Ninv010aListView004.SetItemText using N9,str10,1
               Ninv010aListView004.SetItemText using N9,olrn,2
               clear     str10
               pack      str10 from Omdtem,slash,omdted,slash,omdtec,omdtey
               Ninv010aListView004.SetItemText using N9,str10,3
               Ninv010aListView004.SetItemText using N9,Olstname,4
               Ninv010aListView004.SetItemText using N9,str11,5
               Ninv010aListView004.SetColumnFormat using 5,1              .set $ column justify right
               Ninv010aListView004.SetItemText using N9,str15,6
               Ninv010aListView004.SetColumnFormat using 6,1              .set $ column justify right
               clear     str10
               pack      str10 from invdtem,slash,invdted,slash,invdtec,invdtey
               Ninv010aListView004.SetItemText using N9,str10,7
.begin patch 3.80
.               Ninv010aListView004.SetItemText using N9,chkn1,8
           call       GetDays
               Ninv010aListView004.SetItemText using N9,days,8
               Ninv010aListView004.SetItemText using N9,chkn1,9
.end patch 3.80
.........................................................................................................               
ExitCheckFilters
               add            c1 to selectcount
               move           selectcount to str5
               setitem        Ninv010aEditText002,0,str5
               move           MaskAmountAR to Str15
               edit           Total to str15
               setitem        Ninv010aEditText001,0,str15
               move           MaskAmountAR to Str15
               edit           TotalU to str15
               setitem        Ninv010aEditText004,0,str15
               move           MaskAmountAR to Str15
               edit           TotalB to str15
               setitem        Ninv010aEditText005,0,str15
.;Before we return check summary info and add info 
               Clear          Str8
               pack           str8 from obrknum,omlrnum
               rep            zfill in  str8
               MOve           c0 to Avail
          FOR counter,"1","25"
                              IF ( STR8=Summary(counter).SumKey)
                              add  Formar to Summary(Counter).SumAmountAR
                              Break
                              endif
                              IF ( Summary(counter).SumKey = "" & avail=0)
                              MOve           Counter to Avail
                              endif
               repeat
               IF             (Avail <> 0)
               MOVe           Str8 to Summary(Avail).Sumkey
               move           Formar to Summary(Avail).SumAmountAR
               endif
ExitCheckFilters1
               return
.
ExitNInvSearchList
               Return
..............................................................................................................
..NInvSetFocusTab
NInvSetFocusTab
         setitem     NINV010TabControl001,0,n1
         setfocus    NINV010TabControl001,1
         return
.........................................................................................................
DisplayInvDetail
.need to finish this section add some order goodies etc.
               if             (lrn = "495393")
.               call           debug
               endif
              move      "NINVKEY",Location
              pack      KeyLocation,"Key: ",ninvfld
               call           NInvkey
.begin patch 3.71
               if     over       .not billed
               move   str6,lrn
               setitem        Ninv010cEditText005,0,LRn
               goto   NoInv
               endif
.end patch 3.71
               setitem        Ninv010cEditText005,0,LRn
               setitem        Ninv010cEditText004,0,Invnum
               clear          str10
               pack           str10 from invdtem,slash,invdted,slash,invdtec,invdtey
               setitem        Ninv010cEditText007,0,str10
NoInv                              Move      c1 to nordpath
                              move      lrn to nordfld
                              rep       zfill in nordfld
                              move      "Nordkey",Location
                              pack      KeyLocation,"Key: ",Nordfld
                              call      nordkey
.begin patch 3.7
              pack            NMLDFLD1,"01X",LRN
        clear   str8
              pack            str8,"99999999"
              move      "NMLDAIM",Location
              pack      KeyLocation,"Key: ",NMLDFld1

              call            NMLDAIM
              loop
               until over
               if (NMLDDATE < str8)
                              move           NMLDDATE,str8
               endif
              move      "NMLDkg",Location
              pack      KeyLocation,"Key: ",NMLDFld1
               call           NMLDKG
              repeat
              if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
         unpack     str8 into omdtec,omdtey,omdtem,omdted
              else
.Use current Mail Date
              endif
.end patch 3.7
               clear          str10
               pack           str10 from oodtem,slash,oodted,slash,oodtec,oodtey
               setitem        Ninv010cEditText001,0,str10
               clear          str10
               pack           str10 from omdtem,slash,omdted,slash,omdtec,omdtey
               setitem        Ninv010cEditText002,0,str10
               setitem        Ninv010cEditText006,0,olnum
               setitem        Ninv010cEditText009,0,omlrky
               setitem        Ninv010cEditText010,0,OMLRPON
               call           nolist
               MOVE           olnum TO ndatfld
               move           c1 to ndatpath
               rep            zfill in ndatfld
              move      "NDatkey",Location
              pack      KeyLocation,"Key: ",Ndatfld
               CMATCH         B1 TO ndatfld
               CALL           NDATkey IF NOT Eos
               setitem        Ninv010cEditText012,0,Olstname
.begin patch 3.6
               Count          n2,mlrpayd
               if             (n2 = 8)
               Unpack         MLRPAYD into str2,yy,mm,dd      
               clear          str10
               pack           str10 from mm,slash,dd,slash,str2,yy
               setitem        Ninv010cEditTextcash2,0,str10
               else
               setitem        Ninv010cEditTextcash2,0,b1
               endif
               setitem        Ninv010cEditTextcash,0,CHKN1 
.end patch 3.6

.begin patch
          pack      Mkey from Omlrnum,"000"
            move          c1 to nmlrpath
          call      Nmlrkey
          setitem   Ninv010cEditText003,0,Omlrnum
          Setitem   Ninv010cEditText011,0,Mcomp
.end patch

               MOVE      OLON TO NOWNFLD
              move      "Nownkey",Location
              pack      KeyLocation,"Key: ",Nownfld
               CALL      NOWNKEY
               MOVE     YES TO SUBPPSW
               MOVE      NinvFLD to nmrgfld
               REP       ZFILL IN NMRGFLD
               move      c0 to nmrgrqty
               move      c0 to nmrgiqty
               move      c0 to nmrgnet
               CALL      NMRGKEY
               if             not over
               move           yes to mrgsw
                    else
               move           no to mrgsw
               endif
               move      lrn to nshpfld
              move      "Nshpkey",Location
              pack      KeyLocation,"Key: ",NshpFld
               call      nshpkey
               if             not over
               move           yes to shipsw
                     else
               move           no to shipsw
               endif
               call      wipecvars
               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
.               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad

               CALL      COMPUTE
               MOVE      LRN TO NADJFLD
              move      "Nadjkey",Location
              pack      KeyLocation,"Key: ",Nadjfld
               call      Nadjkey
               If             Not over
               add        asrecadj to formar
               endif

               move           MaskAmountAR to Str15
               edit           Formar to str15
               setitem        Ninv010cEditText008,0,str15                  ;amount
               clear          str10
                getprop       Ninv010aListView001,visible=N9
                move          n9 to ListViewNum
                setfocus      Ninv010cEditText001
              return
.........................................................................................................
NInvDetAdd
               Return
GetDetailDatainv
.               call           Invread
               return
.........................................................................................................
SetNInvErrorMssgDefault
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=1
        setprop ErrorMssgStat4,visible=1
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0,"Enter 4 Digit Mailer Number:"
        setitem ErrorMssgStat2,0,""
        setitem ErrorMssgStat3,0,"    Or hit F2 to Search"
        setitem ErrorMssgStat4,0,"      By Company Name"
        setitem ErrorMssgStat5,0,"      That Mailer Does Not Exist!"
        setitem ErrorMssgOK,0,"&OK"
        return
.........................................................................................................
GetOrderInfo
               rep            zfill in nordfld
               MOVE           C1 TO NordPATH
               call           nordkey
               move           olnum to list
               move           Olrn to LRnum
               setitem        Ninv010cEditText006,0,olnum
               packkey        Ndatfld from olnum
               move           c1 to ndatpath
              move      "Ndatkey",Location
              pack      KeyLocation,"Key: ",Ndatfld
               call           Ndatkey
               Setitem        Ninv010cEditText012,0,olstname
               MOve           Olnum to List
               MOVE           C1 TO NINVPATH
               packkey        Ninvfld from olrn
               CALL           NINVKEY
               If             over
               clear          Invoice
               Clear          Invdate
               setitem        Ninv010cEditText007,0,""
               setitem        Ninv010cEditText004,0,""
               else
               PACK           INVDATE FROM INVDTEC,INVDTEY,INVDTEM,INVDTED         
               pack           str13 from invdtem,slash,invdted,slash,invdtec,invdtey
               setitem        Ninv010cEditText007,0,str13
               setitem        Ninv010CEditText004,0,Invnum
               move           Invnum to Invoice
               endif
               setFocus       Ninv010cEditText010
.begin patch
          pack      Mkey from Omlrnum,"000"
            move          c1 to nmlrpath
          call      Nmlrkey
          setitem   Ninv010cEditText003,0,Omlrnum
          Setitem   Ninv010cEditText011,0,Mcomp
.end patch
               return
.........................................................................................................
.
Timeout
        beep
        beep
        beep
Filego  winshow
        stop
..........................................................................................................
getmailer
           getitem   Ninv010EditText001,0,str4
           call      trim using str4

           count     n2 in str4
           if        (n2 = c3)
           move      str4 to str3
           pack      str4 from c0,str3
           goto      buildmkey
           endif
           if        (n2 = c2)
           move      str4 to str2
           pack      str4 from c0,c0,str2
           goto      buildmkey
           endif
           if        (n2 = c1)
           move      str4 to str1
           pack      str4 from c0,c0,c0,str1
           goto      buildmkey
           endif

buildmkey
           setitem   Ninv010EditText001,0,str4
           clear     mkey
           packkey   mkey from str4,z3
           clear     mnum
                move          c1 to nmlrpath
           call      nmlrkey
           if        not over
           setitem   Ninv010Mlrcomp,0,Mcomp
         else
           setitem   Ninv010Mlrcomp,0," "
           endif
         return
..........................................................................................................
GetBroker
           getitem   Ninv010EditText002,0,str4

           call      trim using str4
           count     n2 in str4
           if        (n2 = c3)
           move      str4 to str3
           pack      str4 from c0,str3
           goto      buildbrkfld
           endif
           if        (n2 = c2)
           move      str4 to str2
           pack      str4 from c0,c0,str2
           goto      buildbrkfld
           endif
           if        (n2 = c1)
           move      str4 to str1
           pack      str4 from c0,c0,c0,str1
           goto      buildbrkfld
           endif
           if        (n2 = 0)
           setitem   Ninv010BRkcomp,0," "
           return
           endif

buildbrkfld
           setitem   Ninv010EditText002,0,str4
           clear     nbrkfld
           packkey   nbrkfld from str4,z3
           clear     brknum
           call      nbrkkey
           if        not over
           setitem   Ninv010BRkcomp,0,BRcomp
         else
           setitem   Ninv010BRkcomp,0," "
           endif
         return
..............................................................................................................
.SetNInvDates - check for date goodies
SetNInvDates
.
        move    yes to dateokflag
        clear   mm
        clear   dd
        clear   yy
        clear   newdate1
        clear   newdate2
        getitem Ninv010ComboBox001,0,result
        move    result to datebranch
               If             (DateBranch > 0 & DateBranch < c4)
               MOve           "T" to DateFilter
               endif

        getitem Ninv010EditDate1,0,str10
        clear   mm
        clear   dd
        clear   str2
        clear   yy
        call    TRIM using str10
        count   N2,str10
        if (N2 = 10)
                unpack  str10,MM,str1,DD,str1,STR2,YY
        elseif (N2 = 8)
                unpack  str10,MM,DD,STR2,YY
        elseif (N2 <> 0)
                alert   caution,"Date Must be in MMDDCCYY Format",result
                    setFocus Ninv010EditDate1
                goto BadDate
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
                    setFocus Ninv010EditDate1
                goto BadDate
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
                              setFocus Ninv010EditDate1
                          goto BadDate
                else
                        move    STR2,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
                                        setFocus Ninv010EditDate1
                                    goto BadDate
                        endif
                endif
        endif
        call    TRIM using MM
        count   N2,MM
        if (N2 <> 0 AND MM <> "00")
                pack    newdate1,MM,SLASH,DD,SLASH,STR2,YY
        else
                clear   newdate1
        endif
        setitem Ninv010EditDate1,0,newdate1
.
        clear   mm
        clear   dd
        clear   yy
        clear   str2
        clear   newdate2
        getitem Ninv010EditDate2,0,str10
        call    TRIM using str10
        count   N2,str10
        if (N2 = 10)
                unpack  str10,MM,str1,DD,str1,STR2,YY
        elseif (N2 = 8)
                unpack  str10,MM,DD,STR2,YY
        elseif (N2 <> 0)
                alert   caution,"Date Must be in MMDDCCYY Format",result
                    setFocus Ninv010EditDate2
                goto BadDate
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
                    setFocus Ninv010EditDate2
                goto BadDate
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
                              setFocus Ninv010EditDate2
                        goto BadDate
                else
                        move    STR2,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
                                        setFocus Ninv010EditDate2
                                goto BadDate
                        endif
                endif
        endif
        call    TRIM using MM
        count   N2,MM
        if (N2 <> 0 AND MM <> "00")
                pack    NewDate2,MM,SLASH,DD,SLASH,STR2,YY
        else
                clear   NewDate2
        endif
        setitem Ninv010EditDate2,0,NewDate2
        if      (Newdate1  <= b1 and Newdate2 <= b1)
        move    c0 to datebranch
        endif
        move    c0 to startdate
        move    c0 to enddate
        if      (newdate1 > b1)
        unpack  newdate1 into mm,str1,dd,str1,str2,yy
        call    cvtjul
        move    juldays to startdate
        endif
        if      (newdate2 > b1)
        unpack  newdate2 into mm,str1,dd,str1,str2,yy
        call    cvtjul
        move    juldays to enddate
        endif
        if     ((startdate > 0 or EndDate > 0)& (DateBranch < 1 & DateBranch > 3))     .dates entered no date type
                                alert   caution,"Specify Date type!",result
                                setfocus  Ninv010ComboBox001
                                goto BadDate
        else
        endif
        return

BadDate
               move           No to dateokflag
               setprop        Ninv010GoButton,visible=1
               setprop        Ninv010Stop,visible=0
               Move           "F" to DateFilter
        return
.......................................................................................................................
NInvForceToOne
               setitem NINV010TabControl001,0,1
               move        c1 to n1
          call        NINVTabChange

               return
                
...........................................................................................
NInvTabClick
        IF (N1 = C1)
            getprop Ninv010aListView001,visible=N9
                move    n9 to ListViewNum
                Deactivate NInv10a
        elseif (N1 = C2 )
                Deactivate NInv10b
        setprop Ninv010aListView001,visible=ListViewNum
        call    NInvSortListView
        elseif (N1 = C3 )
                Deactivate NInv10c
        Endif
        return

NInvTabChange
               Deactivate NInv10A
               Deactivate NInv10B
               Deactivate NInv10C
        IF (N1 = C1)
                move    C1,TabNum
                Activate NINV10a
                setfocus Ninv010aListView001
            setprop Ninv010aListView001,visible=ListViewNum
            call    NInvSortListView
                        LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT

        elseif (N1 = C2)
            getprop Ninv010aListView001,visible=N9
                move    n9 to ListViewNum
                move    C2,TabNum
                Activate Ninv10b
                setfocus Ninv010bListView001
.Prevent occurance or accumulated events which may place "hidden" objects on wrong form
.ie, ResetStatus Checkbox. This generally only happens with LostFocus events from Stats2.plf
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT

        elseif (N1 = C3)
                getprop Ninv010aListView001,visible=N9
                move    n9 to ListViewNum
                move    C3,TabNum
                Activate NInv10C
                setfocus Ninv010cEditText001
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT
        Endif
        return
.............................................................................................................
NInvSortListView
.Dynamically sorts Different ListViews.  
.In order to switch between different ListViews we need two pieces of information.
.We need to ascertain which column was clicked AND which ListView we currently
.have visible, as each ListView has its' columns ordered differently.
.Getprops will determine which ListView is currently active, #EventResult passed to result
.prior to calling this subroutine will determine which column was clicked.
        getprop Ninv010aListView001,visible=N9
        if (N9 = C1)    .Ninv010aListView001 is visible      ;by lr
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                If (result <> 1 and result <> 2 and result <> 6)  .no click?
                        setprop Ninv010aListView001,visible=1
                        setprop Ninv010aListView003,visible=0
                        setprop Ninv010aListView002,visible=0
                        setprop Ninv010aListView004,visible=0
                    Ninv010aListView001.EnsureVisible using c1,0
                        setfocus Ninv010alistview001
                endif
.
                if (result = 1)            .clicked on O date
                        setprop Ninv010aListView001,visible=0
                        setprop Ninv010aListView003,visible=0
                        setprop Ninv010aListView004,visible=1
                        setprop Ninv010aListView002,visible=0
                  Ninv010aListView004.EnsureVisible using c1,0
                        setfocus Ninv010alistview004
                Elseif (result = 2)  .clicked on MD
                        setprop Ninv010aListView001,visible=0
                        setprop Ninv010aListView003,visible=1
                        setprop Ninv010aListView004,visible=0
                        setprop Ninv010aListView002,visible=0
                  Ninv010aListView003.EnsureVisible using c1,0
                        setfocus Ninv010alistview003
                Elseif (result = 6)  .clicked on InvD
                        setprop Ninv010aListView001,visible=0
                        setprop Ninv010aListView003,visible=0
                        setprop Ninv010aListView004,visible=0
                        setprop Ninv010aListView002,visible=1
                    Ninv010aListView002.EnsureVisible using c1,0
                        setfocus Ninv010alistview002
                endif
.            endif

                else
                getprop Ninv010aListView002,visible=N9
                if (N9 = C1)    .Ninv010aListView002 is visible
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
.begin patch - note we have a hidden pseudo day column that scews the count
               if (result <> 2 and result <> 3 and result <> 4)
                                setprop Ninv010aListView001,visible=0
                                setprop Ninv010aListView002,visible=1
                                setprop Ninv010aListView003,visible=0
                                setprop Ninv010aListView004,visible=0
                            Ninv010aListView002.EnsureVisible using c1,0
                            setfocus Ninv010alistview002
                        Elseif (result = 2 )
                                setprop Ninv010aListView001,visible=1
                                setprop Ninv010aListView002,visible=0
                                setprop Ninv010aListView003,visible=0
                                setprop Ninv010aListView004,visible=0
                      Ninv010aListView001.EnsureVisible using c1,0
                                  setfocus Ninv010alistview001
                        Elseif (result = 3 )
                                setprop Ninv010aListView001,visible=0
                                setprop Ninv010aListView002,visible=0
                                setprop Ninv010aListView003,visible=0
                                setprop Ninv010aListView004,visible=4
                            Ninv010aListView004.EnsureVisible using c1,0
                                  setfocus Ninv010alistview004
                        Elseif (result = 4 )
                                setprop Ninv010aListView001,visible=0
                                setprop Ninv010aListView002,visible=0
                                setprop Ninv010aListView003,visible=1
                                setprop Ninv010aListView004,visible=0
                            Ninv010aListView003.EnsureVisible using c1,0
                                  setfocus Ninv010alistview003
                          endif
          else
                getprop Ninv010aListView003,visible=N9
                if (N9 = C1)    .Ninv010aListView003 is visible

.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                      if (result <> 2 and result <> 3 and result <> 7)
                                setprop Ninv010aListView001,visible=0
                                setprop Ninv010aListView002,visible=0
                                setprop Ninv010aListView003,visible=1
                                setprop Ninv010aListView004,visible=0
                            Ninv010aListView003.EnsureVisible using c1,0
                            setfocus Ninv010alistview003
                        Elseif (result = 2 )   .want lr order
                                setprop Ninv010aListView001,visible=1
                                setprop Ninv010aListView002,visible=0
                                setprop Ninv010aListView003,visible=0
                                setprop Ninv010aListView004,visible=0
                      Ninv010aListView001.EnsureVisible using c1,0
                           setfocus Ninv010alistview001
                        Elseif (result = 3 )      
                                setprop Ninv010aListView001,visible=0
                                setprop Ninv010aListView002,visible=0
                                setprop Ninv010aListView003,visible=0
                                setprop Ninv010aListView004,visible=1
                      Ninv010aListView004.EnsureVisible using c1,0
                           setfocus Ninv010alistview004
                        Elseif (result = 7 )      
                                setprop Ninv010aListView001,visible=0
                                setprop Ninv010aListView002,visible=1
                                setprop Ninv010aListView003,visible=0
                                setprop Ninv010aListView004,visible=0
                      Ninv010aListView002.EnsureVisible using c1,0
                           setfocus Ninv010alistview002
                          endif
.                endif
               else
                getprop Ninv010aListView004,visible=N9
                if (N9 = C1)    .Ninv010aListView004 is visible
.               call debug
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                      if (result <> 2 and result <> 3 and result <> 7)
                                setprop Ninv010aListView001,visible=0
                                setprop Ninv010aListView002,visible=0
                                setprop Ninv010aListView003,visible=0
                                setprop Ninv010aListView004,visible=1
                            Ninv010aListView004.EnsureVisible using c1,0
                            setfocus Ninv010alistview004
                        Elseif (result = 2 )   .want lr order
                                setprop Ninv010aListView001,visible=1
                                setprop Ninv010aListView002,visible=0
                                setprop Ninv010aListView003,visible=0
                                setprop Ninv010aListView004,visible=0
                      Ninv010aListView001.EnsureVisible using c1,0
                               setfocus Ninv010alistview001
                        Elseif (result = 3 )   
                                setprop Ninv010aListView001,visible=0
                                setprop Ninv010aListView002,visible=0
                                setprop Ninv010aListView003,visible=1
                                setprop Ninv010aListView004,visible=0
                      Ninv010aListView003.EnsureVisible using c1,0
                          setfocus Ninv010alistview003
                        Elseif (result = 7 )   
                                setprop Ninv010aListView001,visible=0
                                setprop Ninv010aListView002,visible=1
                                setprop Ninv010aListView003,visible=0
                                setprop Ninv010aListView004,visible=0
                            Ninv010aListView002.EnsureVisible using c1,0
                                  setfocus Ninv010alistview002
                          endif
                endif
        endif
        endif
        endif
        return
.......................................................................................................
.........................................................................................................................
SearchGo
        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4
SearchGo1
.BROKER
        move    C1,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo2        
.LIST
        move    C2,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo3        
.MAILER
        move    C3,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo4
.SHIP-TO - not an option with this program
        move    C4,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchLoad
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4
SearchLoad1
.BROKER
        unpack  Srchstr,str4,str1,str3,str1,str45
        setitem  Ninv010EditText002,0,str4
        setitem  Ninv010BRkcomp,0,str45
        setfocus Ninv010EditText002
        return
SearchLoad2
.LIST 
        unpack      Srchstr,str6,str1,str35
        setitem NInv010EditText005,0,str6
        setitem Ninv010StatText008,0,str35
        setfocus NInv010EditText005
        return
SearchLoad3
.MAILER
        unpack  Srchstr,str4,str1,str3,str1,str45
        setitem Ninv010EditText001,0,str4
        setitem NInv010StatText002,0,str45
        setfocus Ninv010EditText001
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
.

HelpGo
        setprop AboutMssg,visible=1
        return       
...........................................................................................................
ClearNInvSearchList
.Clear ListView
               Ninv010aListView001.DeleteAllItems giving N9
               Ninv010aListView002.DeleteAllItems giving N9
               Ninv010aListView003.DeleteAllItems giving N9
               Ninv010bListView001.DeleteAllItems giving N9
               Ninv010bListView002.DeleteAllItems giving N9
               mOVE           C0 TO GrandBalance
               mOVE           C0 TO SelectTotal
               mOVE           C0 TO Selectcount
               setitem        Ninv010aEditText001,0,""
               setitem        Ninv010aEditText002,0,""
               setitem        Ninv010aEditText004,0,""
               setitem        Ninv010aEditText005,0,""
               clear        str5
               clear    str14
          FOR counter,"1","25"
                              Clear Summary(counter).SumKey
                              MOve  c0 to Summary(Counter).SumAmountMOA
                              MOve  c0 to Summary(Counter).SumAmountAr
               repeat

               return
...........................................................................................................
NInvDetailClear
               setitem    Ninv010cEditText001,0," "
               setitem    Ninv010cEditText002,0," "
.               setitem    Ninv010cEditText003,0," "
               setitem    Ninv010cEditText004,0," "
               setitem    Ninv010cEditText005,0," "
               setitem    Ninv010cEditText006,0," "
               setitem    Ninv010cEditText007,0," "
               setitem    Ninv010cEditText008,0," "
               setitem    Ninv010cEditText009,0," "
               setitem    Ninv010cEditText010,0," "
.               setitem    Ninv010cEditText011,0," "
               setitem    Ninv010cEditText012,0," "
.               setitem    Ninv010cEditText003,0," "
               setitem    Ninv010cEditText011,0," "
.begin patch 3.6
               setitem        Ninv010cEditTextcash2,0,b1
               setitem        Ninv010cEditTextcash2,0,b1 
.end patch 3.6
               Move           c0 to total
               Move           c0 to totalB
               Move           c0 to totalU

               return
...............................................................................
NOLIST   clear     OLSTNAME
         RETURN
...............................................................................
KEYEDIT
.
         MOVE      "0",VALKEY
         MATCH     "0000",KLIST
         CALL      CUME IF EQUAL
.
         MATCH     "0000",KMAILER
         CALL      CUME IF EQUAL
.
         cmatch    b1 to str12
         call      cume if eos
         RETURN
.
CUME
         ADD       "1",VALKEY
         RETURN
..................................................................................................................................
.Begin patch 3.80
CheckMLdate
              pack            NMLDFLD1,"01X",OLRN
        clear   str8
              pack            str8,"99999999"
              call            NMLDAIM
              loop
               until over
               if (NMLDDATE < str8)
                              move           NMLDDATE,str8
               endif
               call           NMLDKG
              repeat
              if (str8 <> "99999999")
.Valid Hit - Use this Value as Earliest Date
         unpack     str8 into omdtec,omdtey,omdtem,omdted
              else
.Use current Mail Date
              endif
           Return
GetDays
         clear     str10a
         if        (omdtem <> "00" and omdtem <> "  ")
         pack      str10a from omdtem,slash,omdted,slash,omdtec,omdtey
         move      omdtem to mm
         move      omdted to dd
         move      omdtey to yy
         else
         pack      str10a from invdtem,slash,invdted,slash,invdtec,invdtey
         move      invdtem to mm
         move      invdted to dd
         move      invdtey to yy
         endif
         call      cvtjul
         move      juldays to holdmdate
        cmatch     "P" to statb
        if         equal
        unpack     mlrpayd into str2,yy,mm,dd
        else
        clock      date to today
        unpack     today into mm,str1,dd,str1,yy
        endif
        call      cvtjul
.
        sub       holdmdate from juldays
        move      c0 to days
        if          (juldays >= c0)  
        move      juldays to days
          endif
           return
.end patch 3.80
..................................................................................................................................
colorerror
        noreturn
        move    C1,colorflag
        goto aftercolor
ColorGo
        if (result = C1)
                call    BackColor
        elseif (result = C2)
                call    TextColor
        else
                return
        endif
        clear   n1
        prep    colorfile,"c:\program files\nincal\NInv010.col"
                  loop
                add     c1,n1
                write   colorfile,seq;colornum(n1)
                until (n1 =2)
        repeat
        close   colorfile
        return
.Trap for Cancel Entry in Color System Menu
ColorTrap
        noreturn
        return
BackColor
        trap    ColorTrap if object
        create  BGC
        trapclr object
        setprop ColBack,bgcolor=BGC
        getitem BGC,1,Fred
        getitem BGC,2,Fgreen
        getitem BGC,3,Fblue
        pack    colornum(2),Fred,Fgreen,Fblue
        return

TextColor
        trap    ColorTrap if object
        create  FTC
        trapclr object
        setprop ColText,fgcolor=FTC
        getitem FTC,1,Fred
        getitem FTC,2,Fgreen
        getitem FTC,3,Fblue
        pack    colornum(1),Fred,Fgreen,Fblue
        return        
...........................................................................................................
.end patch 3.0
* ***************************************************************************
*  EXIT AND FERROR SUBROUTINES
* ****************************************************************************
.
EXIT 
EXIT1    BEEP
         STOP
IO
         TRAPCLR   IO
         NORETURN
               alert   caution,"I/O ERROR INFORM COMPUTER PERSONNEL !!!",result
               alert   caution,Error,result
         BEEP
         STOP
RANGE
         TRAPCLR   RANGE
         NORETURN
               alert   caution,"Range ERROR INFORM COMPUTER PERSONNEL !!!",result
               alert   caution,Error,result
         BEEP
         STOP
FORMAT
         TRAPCLR   FORMAT
         NORETURN
               alert   caution,"Format ERROR INFORM COMPUTER PERSONNEL !!!",result
               alert   caution,Error,result
         BEEP
         STOP
PARITY
         TRAPCLR   PARITY
         NORETURN
               alert   caution,"PARITY ERROR INFORM COMPUTER PERSONNEL !!!",result
               alert   caution,Error,result
         BEEP
         STOP
XRESIZE
           NINV010.Scale
           RETURN


.begin patch 3.3
.         INCLUDE   COMPUTE.inc
               INCLUDE        compute.inc
.end patch 3.3
         include   ndatio.inc
         include   nacdio.inc
         include   nshpio.inc
.patch3.2
                              include   compio.inc
                              include   cntio.inc
.         INCLUDE   NMLRIO.inc
.patch3.2
         INCLUDE   MLRHELP.inc
         INCLUDE   brkHELP.inc
         INCLUDE   NORDIO.inc
         INCLUDE   NADJIO.inc
.patch3.2
.         include   nbrkio.inc
.patch3.2
.begin patch 3.3
.         INCLUDE   NINVIO.inc
               INCLUDE        ninvio.inc
               include        NInvAcdio.inc
.end patch 3.3
         INCLUDE   NOWNIO.INC
         INCLUDE   NDAT3IO.INC
         INCLUDE   NMOBIO.inc
         inc       nmrgio.inc
         include   ncmpio.inc
         include   nrtnio.inc
         include   searchio.inc      .contains logic for search.plf
         INCLUDE   NMOaIO.inc
.START PATCH 3.1 ADDED LOGIC
          INCLUDE   NSEL2IO.INC
          include   nmodio.inc
.END PATCH 3.1 ADDED LOGIC
.begin patch 3.7  
           include    nmldio.inc
.end patch 3.7  
         INCLUDE   COMLOGIC.inc
