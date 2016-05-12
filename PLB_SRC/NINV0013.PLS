.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.begin patch 2.7
.;begin patch 3.2
.;         INCLUDE   NINVDD.inc
               INCLUDE        ninvdd.inc
.;begin patch 3.2
.end patch 2.7
         include   ndatdd.inc
.;patch3.1
          include   compdd.inc
          include   cntdd.inc
.         INCLUDE   NMLRDD.inc
.;patch3.1
         include   nowndd.inc
         INCLUDE   NORDDD.INC
.;patch3.1
.         INCLUDE   NBRKDD.INC
.;patch3.1
.begin patch 2.8
         include   nadjdd.inc
.end patch 2.8
         INCLUDE   NOFRDD.INC
         include   nshpdd.inc
.START PATCH 3.0 REPLACED LOGIC
.begin patch 3.4
          include   COnsacct.inc
          include   Nacddd.inc
          include   Ninvacddd.inc
         inc       nmrgdd.inc
        include ncmpdd.inc
        include nrtndd.inc
         INCLUDE   NMOADD.inc
         INCLUDE   NMOBDD.inc
.end patch 3.4
          include   Ndat3dd.inc
          INCLUDE   NSELDD.INC
          INCLUDE   NTXTDD.INC
          include   nsel2dd.inc
          include   nmoddd.inc
.END PATCH 3.0 REPLACED LOGIC
.Testing/Pending                            calc estimate on unbilled.
.
release        init           "3.4"        DLH       Cleanup - Gui
Reldate   INit      "05 March 2009"
.release        init           "3.3"        DLH              26Sep2007 PLI Conversion    
.release        init           "3.2"        DLH              8March2005         Invoice Conversion    
.release  init      "3.1"        DMB    26MAY2004 Mailer Conversion    
.release  init      "3.0"       ASH 28JAN2004  DATACARD CONVERSION
.release  init      "2.9"       ASH 21MAR2003 DATACARD CONVERSION
.release  init      "2.8"       DLH 26Aug99 NINadj nadjust Y2K, File expansion
.release  init      "2.7"       DLH 5May99 NININV Y2K, File expansion
.release  init      "2.6"       ASH 29Dec98 NINORD Y2K, File expansion
.        For this program, Form72 has been redefined to FORM 9.2; instead
.        of declaring Form92, as Form92 is already declared and used elsewhere
.        in program.  Otherwise we might be able to knock out these declarations
.        and simply include, CONSACCT.INC.  Unfortunately, we can't do that!!!
.release  init      "2.5"       DLH  7Feb97 new datacard format
.Release  init      "2.4"             JD  30mar95 added dawson brk # getprice
.release  init      "P2.3"     dlh 07sep94 select all/paid/payable (pending)
.RELEASE  INIT      "2.2"     DLH 2FEB94 START ADDING ADJUSTMENTS.
.
.RELEASE  INIT      "2.1"     DLH 6JAN94 Major update : display like nord0010,
.                            if list number included search aim.
.                            if exhange marked billed with no inv - flagged.
.RELEASE  INIT      "2.0"     DLH 10NOV93 REVIVED FROM ARCHIVES.
.
. .............................................................................
. PROGRAM VARIABLES
. .................
.
Kown     DIM       4                  KEYIN FIELD FOR MAILER##;
KLIST    DIM       6                  KEYIN FIELD FOR LIST##;
QUES     INIT      "????"
VTAB     FORM      2
VTAB1    FORM      2
VTAB2    FORM      2
VALKEY   FORM      1
HIT      FORM      1
DATE     DIM       8
CHKMM    FORM      2                  HOLDS MONTH PARAMETER
CHKYR    FORM      2                  HOLDS YEAR PARAMETER
WORK02   DIM       2
MO       DIM       2                  DATE DISPLAY;
DAY      DIM       2                   "      "
YEAR     DIM       2                   "      "
PASS     FORM      3                  NUMBER OF RECORD DISPLAYS;
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
AKEY1    init      "01F"              USED TO BUILD MAILER & LIST FILE AIM KEY.
CHKMLR   DIM       4                  USED TO ELIMINATE DUP MAILER DISP.
KEYCOUNT FORM      2                  USED TO CHECK AIM KEY LENGTH, FREE FLOAT
VARIN    FORM      2
WSW      DIM       1            WITHDRAWN SWITCH DEFAULT NO
BRKCHECK DIM       4            KEYED IN BROKER/CONSULTANT NUMBER.
MCHECK   DIM       6            KEYED IN MAILER CHECK NUMBER.
CHKFLAG  FORM      1
BRKFLAG  FORM      1
repflag  form      1           report type 1=all 2=pAID 3=OPEN
check    dim       6
APPLIED  DIM       13
AMTMASK  INIT      "$$$$$$,$$9.99"
.hund     form      "100"
total    form      9.2
MASK     INIT      "$$$$,$$$.99"
MASK1    INIT      "$$$$,$$$.99"
TOTAL2   FORM      9
TOTAL2A  DIM       11
TOTALA   DIM       11
AP1TOT   FORM      9
AP2TOT   FORM      9
.Start Patch #2.6 - VARS replaced due to OQTY expansion
.form72   form      7.2
.form72   form      9.2
.End Patch #2.6 - VARS replaced due to OQTY expansion
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10             NUMERIC WORK FIELD FOR CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
SPLITSW  DIM       1                  RENT/EXCHANGE SPLIT = 'Y'
.Start Patch #2.6 - vars replaced due to OQTY expansion
.QTYCHK   FORM      6
QTYCHK   FORM      9
.End Patch #2.6 - vars replaced due to OQTY expansion
.THOUS    FORM      "1000"
.LSTMSW   DIM       1                  LIST MANAGEMENT INDICATOR.
MDATE    FORM      5
TENDOLL  FORM      "99999"
NINEDOLL FORM      "199999"
SEVDOLL  FORM      "300000"
JUNEDAT  INIT      "060191"
JUNDATE  FORM      6
.Start Patch #2.6 - VARS replaced due to OQTY expansion
.UNBILAMT FORM      7.2
.UNBILINC FORM      7.2
UNBILAMT FORM      9.2
UNBILINC FORM      9.2
.End Patch #2.6 - VARS replaced due to OQTY expansion
FORM52   FORM      5.2
.form92   form      9.2
ap1adj   form      9.2
.Start Patch #2.6 - VARS replaced due to OQTY expansion
.ap2adj   form      7.2
ap2adj   form      9.2
.End Patch #2.6 - VARS replaced due to OQTY expansion
ninvhit  form      1         .'1' if invoiced.
exchflag form      1         .'1' if dead exchange order.
.START PATCH 3.0 - ADDED LOGIC
TEXT1    DIM       47
.END PATCH 3.0 - ADDED LOGIC
.
.begin patch 3.4
COUNTO   FORM      5                  NUMBER OF ORDERS READ.
COUNTO1  FORM      5                  NUMBER OF ORDERS CALCULATED.
COUNTI   FORM      5                  NUMBER OF INVOICES READ
COUNTI1  FORM      5                  NUMBER OF INVOICES CALCULATED
mfp      form      2
mll      form      2
charge   form      3.2
CHANGE   FORM      7.2
mrgsw    dim       1
shipsw   dim       1
TOTALB   FORM      9.2
TOTALU   FORM      9.2
GRANDBALANCE   fORM           7.2
SelectTOtal    form           7.2
SelectCOunt    FOrm           5
Summary        Record         (25)
SumKey         dim            8                                             ;holds BRK+MLR,total
SumAmountAP    form           9.2
SumAmountMOA   form           9.2
               recordend
COunter        Form           2
Avail          Form           2
ARTOTAL        FORM      9.2
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
        move    "NINV0013.PLS",Wprognme
        move    "Amount Payable",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        move    Reldate to Wreldate
.end patch 3.4
. .............................................................................
. MAINLINE
.Begin patch 3.4
SRCH    plform  Search        
mss1    plform  Error
abt     plform  About
NINV13A plform  NINV0013A
NINV13B plform  NINV0013B
NINV13C plform  NINV0013C
x       plform  NINV0013
        winhide
.Load Forms, Always load parent form first
        formload x
        formload NINV13c,NINV0013
        formload NINV13b,NINV0013
        formload NINV13A,NINV0013
        formload abt
        formload mss1
        formload SRCH

.Set tab index
        move    C2,TabNum
.
        
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Ninv0013,RESULT
.Create Menus
        CREATE  NINV0013;MFile,FData
        create  NINV0013;mOptions,OData,mFile
        create  NINV0013;mHelp,HData,mOptions
        CREATE  NINV0013;sCOlor,Cdata,mOptions,1
.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
        activate mOptions
        activate mHelp,HelpGo,result
        activate sColor,ColorGo,result
.Create SubMenu
        create  NINV0013;sSearch,SData,mOptions,1
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
.
.Open color file
opencolor
        trap    colorerror if io
        open    colorfile,"c:\program files\nincal\Ninv0013.col"
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
         MOVE      C1 TO NINVPATH     .SET ACCESS TO ISI.
         MOVE      C1 TO NORDPATH     .SET ACCESS TO isi.
         MOVE      C3 TO NORDLock     .no locks
.Create Ninv0013aListView001 Columns
.Column Clicking
        Ninv0013aListView001.InsertColumn using "LR",50,1
        Ninv0013aListView001.InsertColumn using "Order Date",75,2
        Ninv0013aListView001.InsertColumn using "Mail Date",75,3
        Ninv0013aListView001.InsertColumn using "List",180,4
        Ninv0013aListView001.InsertColumn using "Order Qty",75,5
        Ninv0013aListView001.InsertColumn using "Amount",80,6
        Ninv0013aListView001.InsertColumn using "Inv Date",75,7
        Ninv0013aListView001.InsertColumn using "Check ##",65,8
        Ninv0013aListView001.SetColumnFormat using 5,1              .set column justify right
        Ninv0013aListView001.SetColumnFormat using 6,1              .set column justify right
.
.Create Ninv0013aListView002 Columns by Invoice date
.Column Clicking
        Ninv0013aListView002.InsertColumn using "Inv Date",0,1                    ;unformated so sort works
        Ninv0013aListView002.InsertColumn using "Inv Date",75,2
        Ninv0013aListView002.InsertColumn using "LR",50,3
        Ninv0013aListView002.InsertColumn using "Order Date",75,4
        Ninv0013aListView002.InsertColumn using "Mail Date",75,5
        Ninv0013aListView002.InsertColumn using "List",180,6
        Ninv0013aListView002.InsertColumn using "Order Qty",75,7
        Ninv0013aListView002.InsertColumn using "Amount",80,8
        Ninv0013aListView002.InsertColumn using "Check ##",65,9
        Ninv0013aListView002.SetColumnFormat using 8,1              .set column justify right
        Ninv0013aListView002.SetColumnFormat using 7,1              .set column justify right
.
.Create Ninv0013aListView003 Columns by Mail Date
.Column Clicking
        Ninv0013aListView003.InsertColumn using "Mail Date",0,1                    ;unformated so sort works
        Ninv0013aListView003.InsertColumn using "Mail Date",75,2
        Ninv0013aListView003.InsertColumn using "LR",50,3
        Ninv0013aListView003.InsertColumn using "Order Date",75,4
        Ninv0013aListView003.InsertColumn using "List",180,5
        Ninv0013aListView003.InsertColumn using "Order Qty",75,6
        Ninv0013aListView003.InsertColumn using "Amount",80,7
        Ninv0013aListView003.InsertColumn using "Inv Date",75,8
        Ninv0013aListView003.InsertColumn using "Check ##",65,9
        Ninv0013aListView003.SetColumnFormat using 6,1              .set column justify right
        Ninv0013aListView003.SetColumnFormat using 7,1              .set column justify right

.
.Create Ninv0013aListView004 Columns 
        Ninv0013aListView004.InsertColumn using "Order Date",0,1                    ;unformated so sort works
        Ninv0013aListView004.InsertColumn using "Order Date",75,2
        Ninv0013aListView004.InsertColumn using "LR",50,3
        Ninv0013aListView004.InsertColumn using "Mail Date",75,4
        Ninv0013aListView004.InsertColumn using "List",180,5
        Ninv0013aListView004.InsertColumn using "Order Qty",75,6
        Ninv0013aListView004.InsertColumn using "Amount",80,7
        Ninv0013aListView004.InsertColumn using "Inv Date",75,8
        Ninv0013aListView004.InsertColumn using "Check ##",65,9
        Ninv0013aListView004.SetColumnFormat using 6,1              .set column justify right
        Ninv0013aListView004.SetColumnFormat using 7,1              .set column justify right

.

.Create Ninv0013bListView001 Columns by Broker
        Ninv0013bListView001.InsertColumn using "Broker",200,1
        Ninv0013bListView001.InsertColumn using "Mailer",200,2
        Ninv0013bListView001.InsertColumn using "Amount Due",100,3
        Ninv0013bListView001.InsertColumn using "MOA",75,4
        Ninv0013bListView001.SetColumnFormat using 3,1              .set $ column justify right
        Ninv0013bListView001.SetColumnFormat using 4,1              .set $ column justify right
.Create Ninv0013bListView002 Columns by Broker
        Ninv0013bListView002.InsertColumn using "Mailer",200,1
        Ninv0013bListView002.InsertColumn using "Broker",200,2
        Ninv0013bListView002.InsertColumn using "Amount Due",100,3
        Ninv0013bListView002.InsertColumn using "MOA",75,4
        Ninv0013bListView002.SetColumnFormat using 3,1              .set $ column justify right
        Ninv0013bListView002.SetColumnFormat using 4,1              .set $ column justify right

.Set Error Message Stat Text Boxes
        call    SetNINVErrorMssgDefault
. .................................................................
        move        c1 to n1
          call        NInvTabChange
        setfocus Ninv0013EditText001
        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat
        goto    Ninv0013
..............................................................................................................
.LoadNInvSearchList - sort out keys being used and go to it
LoadNInvSearchList
               setprop Ninv0013aListView001,visible=1
               setprop Ninv0013aListView003,visible=0
               setprop Ninv0013aListView002,visible=0
               setprop Ninv0013aListView004,visible=0
               Ninv0013aListView001.EnsureVisible using c1,0
               setfocus Ninv0013alistview001
               call           SetNInvDates
               CLear          KOWn
               Clear          Klist
               Getitem        Ninv0013EditText001,0,Kown
               Getitem        Ninv0013EditText002,0,STR1                        .type
               Getitem        Ninv0013EditText005,0,KList
          rep       "A1P2O3" in str1
          move      c1 to repflag                           .Default is all records
          move      str1 to repflag
          if        (repflag = 1)
          Setitem   NINV0013Type,0,"All Records"
          elseif    (repflag = 2)
          Setitem   NINV0013Type,0,"Only Paid Records"
          elseif    (repflag = 3)
          Setitem   NINV0013Type,0,"Only Open Records"
          endif

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
               Move           c2,varin     
               Move           str6 to Klist
               else
               Move           c1,varin     
               endif
.end patch 3.4
. .............................................................................
.         TRAP      int IF F5
.         TRAP      RANGE GIVING ERROR NORESET IF RANGE
.         TRAP      FORMAT GIVING ERROR NORESET IF FORMAT
.         TRAP      PARITY GIVING ERROR NORESET IF PARITY
..         TRAPCLR   IO
.         TRAP      IO GIVING ERROR NORESET IF IO
.         TRAP      F3 IF F3
.         CLOCK     DATE TO DATE
.         MOVE      DATE TO TODAY
.         MOVE      C1 TO NINVPATH     .SET ACCESS TO ISI.
.         MOVE      C1 TO NORDPATH     .SET ACCESS TO isi.
.         MOVE      C3 TO NORDLock     .no locks
.         GOTO      START
.F3       TRAPCLR   F3
.         DISPLAY   *RESETSW
.         TRAP      F3 IF F3
.         GOTO      START
.
START
.         MOVE      "NINV0013" TO PROGRAM
.         MOVE      "Names In The News" TO COMPNME
.         MOVE      "Revenue due to LIST OWNER" TO STITLE
         MOVE      C0 TO TOTAL
         MOVE      C0 TO TOTALA
         MOVE      C0 TO TOTAL2A
         MOVE      C0 TO TOTAL2
         MOVE      C0,AP1TOT
         MOVE      C0,AP2TOT
. 
.         CALL      PAINT
.         DISPLAY   *P29:09,"1) Owner##: ____":
.                   *P29:11,"2) List##: ______":
.                   *p29:13,"3) Type:   _ (",*cyan,"A",*white,")ll, (P)aid, (O)pen/payable":      
.                   *P20:21,*HON,"ENTER (*) TO EXIT, (<) TO BACKUP":
.                   " or (?) FOR HELP":
.                   *P20:22,"(F3) ESCAPE SEARCH",*HOFF
..
.KEYIN0
.         MOVE      C0 TO VARIN              *NUMBER OF VARIABLES IN
.         CLEAR     KOWN
.         KEYIN     *P40:09,*ZF,*JR,*T254,KOWN
.         DISPLAY   *P40:09,KOWN
.         TRAP      F3 IF F3
.         TRAP      EXIT1 IF F5  
         TYPE      KOWN
         IF        EOS
                alert   caution,"Owner is required",result
                    setFocus Ninv0013EditText001
          return
          
.         DISPLAY   *P1:24,*EL,"REQUIRED!!!!",*B,*W,*P1:24,*EL
.         GOTO      KEYIN0
         ENDIF
.KEYIN1
..         MOVE      C1 TO VARIN              *NUMBER OF VARIABLES IN
.         CLEAR     ownnum
.         scan      "*",KOWN
.         GOTO      EXIT IF EQUAL
.         reset     kown
.         scan      "?" in KOWN
.         if        equal
.         CALL      ownHELP 
..START PATCH 3.0 REPLACED LOGIC
..         move      ownnum to kown
.          unpack    ownnum,str2,str4
.          move      str4 to kown
..END PATCH 3.0 REPLACED LOGIC
.         endif
.         reset     kown
.         DISPLAY   *P40:09,KOWN
.disown
.         RESET     KOWN
.         move      c1 to nownpath
.         PACK      nownfld FROM KOWN
.         REP       zfill,nownfld
.         CALL      NownKEY
.         CALL      NOown IF OVER
.disown1
.         KEYIN     *P44:09,*DV,ownocpy,*P75:09,"OK?",*T60,str1;
.         CMATCH    NO,str1
.         GOTO      KEYIN0 IF EQUAL
.         CMATCH    YES,str1
.         GOTO      KEYIN2 IF EQUAL
.         GOTO      disown1
.KEYIN2
.         CLEAR     str6
.         KEYIN     *P40:11,*ZF,*JR,*T60,str6
.         DISPLAY   *P40:11,str6
.         MATCH     "00000<",str6
.         GOTO      KEYIN0 IF EQUAL
.         MATCH     "00000*" TO str6
.         GOTO      EXIT IF EQUAL
.         match     "00000?" to str6
.         if        equal
.         call      listhelp
.         move      lstnum to str6
.         endif
.         DISPLAY   *P40:11,str6
.         TYPE      str6
.         GOTO      KEYIN2x IF EOS
.DISLST   move      str6 to ndatfld
.         move      c1 to ndatpath
.         move      c2 to varin
.         CALL      NdatKEY
.         KEYIN     *P45:11,*DV,mlstname,*P75:11,"OK?",*T60,str1;
.         CMATCH    NO,str1
.         GOTO      KEYIN2 IF EQUAL
.         CMATCH    YES,str1
.         GOTO      KEYIN2x IF EQUAL
.         GOTO      DISLST
.         
..
.KEYIN2X  BRANCH    VARIN OF keyin3,keyin3
..
.KEYIN3         move      "A" to str1
.         keyin     *p41:13,*rv,str1
.         rep       "A1P2O3" in str1
.         move      c0 to repflag
.         move      str1 to repflag
.         branch    repflag to keyin3x,keyin3x,keyin3x
.         GOTO      EXIT
.KEYIN3X  BRANCH    VARIN OF READ1,READ1
.         GOTO      EXIT
..
.READ1
.         DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G",*HOFF;
.         MOVE      "0",HIT
.         MOVE      "01",VTAB
.         MOVE      "02",VTAB1
.         MOVE      "03",VTAB2
..
         MOVE      "01",PASS
         branch    varin of srchks,srchkg         
.
srchks   move      c1 to nordpath
.         call      rotdial
         CALL      NORdseq
          return    if over
.         GOTO      NOMORE IF OVER
.         DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G",b1,kown,b1,olon,*HOFF,b1,olrn,*w5;
..START PATCH 2.7 - NEW LOGIC
..EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
..END PATCH 2.7 - NEW LOGIC
         match     kown to olon
         goto      readks if not equal
         goto     readok1
srchkg        
         CLEAR     NORDFLD1
         clear     nordfld3
         clear     nordfld4
         MOVE      "02R" TO AKEY1
         pack      nordfld2 from akey1,str6
         move      c2 to nordpath
         call      nordaim
          return    if over
.         goto      nomore if over
.         DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G",b1,kown,b1,nordfld2,*HOFF;
..START PATCH 2.7 - NEW LOGIC
..EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
..END PATCH 2.7 - NEW LOGIC
         match     kown to olon
         goto      readkg if not equal
readok1         
         GOTO      CALLMLR
..         
READKs
.         DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G",*HOFF,b1,kown,b1,olon,b1,olrn;
         CALL       NORDseq
          return    if over
.         GOTO      NOMORE2 IF OVER
         match     kown to olon
         goto      readks if not equal
readks1  DISPLAY   *P01:24,*EL;
         goto       callmlr
READKG
.         DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G",*HOFF;
         CALL       NORDKG
          return    if over
.         GOTO      NOMORE2 IF OVER
.         DISPLAY   *P01:24,*EL;
         match     kown to olon
         goto      readkg if not equal
.         
..
CALLMLR  
         PACK      STR2 FROM OSALES10,OSALES
         REP       ZFILL IN STR2
         MOVE      NO TO LSTMSW
.begin patch 3.3
          if        (Str2 = "06" or str2 = "19" or Str2 = "27" or Str2 = "28")
..         MATCH     "06" TO STR2
..         IF        EQUAL
         MOVE      YES TO LSTMSW            *LIST MANAGEMENT.
..         ELSE
..         MATCH     "19" TO STR2
..         IF        EQUAL
..         MOVE      YES TO LSTMSW
         ELSE      
         MATCH     "00" TO STR2
                   IF        EQUAL
                   RESET     RUNCODES
                   SCAN      OLNUM IN RUNCODES
                             IF        NOT EQUAL
                             MOVE      YES TO LSTMSW
                             ENDIF
                   ENDIF
        ENDIF
..end patch 3.3
.         ENDIF
         PACK      MKEY FROM OMLRNUM,OCOBN
         REP       ZFILL,MKEY
         CALL      NMLRKEY
         MOVE      OLRN TO NINVFLD
         REP       ZFILL IN NINVFLD
..        DISPLAY   *P1:24,*EL,"INV KEY ",NINVFLD,*W2
         CALL      NINVKEY
         if        over
         move      c0 to ninvhit
..if not billed branch report based on rep type.
         branch    repflag to estimate,callmlrx,estimate
..         goto      estimate
         else
         branch    repflag to detadjin,detadjin
          endif
callmlrx branch    varin of srchks,srchkg         
detadjin move     c1 to ninvhit
         PACK      NadjFLD FROM ninvfld
         CALL      NadjKEY
         GOTO      doit IF OVER
..begin patch 2.8
..         MOVE      C0 TO CVTFLD
..         move      aspayad1 to cvtfld
..        call       cvt
..        move       C0 to form92
..        move       cvtfld to form92
..        div        hund into form92
..        mult       seq by form92
        move       c0 to ap1adj
..        add        form92 to ap1adj
         add        aspayad1 to ap1adj
..                move       c0 to cvtfld
..        move       aspayad2 to cvtfld
..        call       cvt
..        move       c0 to form92
..        move       cvtfld to form92
..        div        hund into form92
..        mult       seq by form92
..        move       c0 to form72
..        add        form92 to form72
..
..         move      form72 to ap2adj
         move       c0 to ap2adj
         move       aspayad2 to ap2adj
..end patch
        goto      doit
.         endif
estimate clear     ap1
         clear     ap2
         move      c0 to ap1adj
         move      c0 to ap2adj
         move      c0 to exchflag
         reset     excodes
         scan      oelcode in excodes
         if        equal
         move      c1 to exchflag
         goto      doit
         endif
.>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
.>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*add estimate goodies here.>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
.>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
.>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
DETAIL   MOVE      C0 TO FORM72
         MOVE      C0 TO UNBILAMT
         MOVE      C0 TO UNBILINC
         MOVE      C0 TO FORM52
         MOVE      C0 TO AR
         MOVE      OLRN TO NSHPFLD
         CALL      NSHPKEY
           IF        not OVER
.Start Patch #2.6 - increase var for future increase of SQUANT
.I chose to switch to N10 because N9 is used elsewhere in program - ASH
.           MOVE      C0 TO N7
.           MOVE      SQUANT TO N7
.           COMPARE   C0 TO N7
           MOVE      C0 TO N10
           MOVE      SQUANT TO N10
           COMPARE   C0 TO N10
.End Patch #2.6 - increase var for future increase of SQUANT
             IF        NOT EQUAL
.Start Patch #2.6 - remmed and replaced logic due to OQTY expansion
             MOVE      SQUANT TO OQTY
.             PACK       OQTY,"00",SQUANT
.End Patch #2.6 - remmed and replaced logic due to OQTY expansion
             ENDIF
            endif
.
doit
         ADD       C1 TO N5
.         pack      mkey from omlrnum,z3
.         CALL      nmlrkey
.
.DISREC    ADD       "4",VTAB
.         ADD       "4",VTAB1
.         ADD       "4",VTAB2
.         COMPARE   "21",VTAB
.         CALL      NEXTPAGE IF NOT LESS
          call      checkfilters
.         CALL      DISSREC
         branch    varin of readks,readkg
..
DISSREC
         ADD       "1",PASS
ADD      MOVE      C0 TO N9
         MOVE      AP1 TO N9
         ADD       N9,AP1TOT
         MOVE      C0 TO N9
         MOVE      AP2 TO N9
         ADD       N9,AP2TOT
         COMPARE   C0 TO N9
         GOTO      ADD1 IF EQUAL
         GOTO      ADD2
ADD1     MOVE      C0 TO N9
         MOVE      AP1 TO N9
         ADD       N9 TO TOTAL2
         move      chkn1 to check
         GOTO      ENDINV
ADD2     MOVE      C0 TO N9
         MOVE      AP2 TO N9
         ADD       N9 TO TOTAL2
         move      chkn2 to check
         GOTO      ENDINV
ENDINV
.START PATCH 2.7 - REPLACED LOGIC
.         DISPLAY   *P01:VTAB,*EF,"MAILER: ",MCOMP,*P35:VTAB,"OFFER: ":
.                   OODES,*P68:VTAB,*HON,"PO##: ",OMLRPON,*HOFF:
.                   *P01:VTAB1,*EL,"LIST: ",OLNUM,*P14:VTAB1,O1DES:
.                   *P21:23,*EL,ownocpy," TOTAL = ",total2
         DISPLAY   *P01:VTAB,*EF,"MAILER: ",MCOMP,*P35:VTAB,"OFFER: ":
                   OFDESC,*P68:VTAB,*HON,"PO##: ",OMLRPON,*HOFF:
                   *P01:VTAB1,*EL,"LIST: ",OLNUM,*P14:VTAB1,O1DES:
                   *P21:23,*EL,ownocpy," TOTAL = ",total2
.END PATCH 2.7 - REPLACED LOGIC
.
.
         branch   ninvhit to diss1              .incoiced
..............................................................................
.unbilled - estimate.
         branch    exchflag to exchange
         clear    invdtem
         clear    invdted
         clear    invdtey
         move     c0 to n9          .temp until calcs done
         branch    exchflag to exchange
         DISPLAY   *P01:VTAB2,*EL,*HON,"LR##",*HOFF,LRN:
                   *P11:VTAB2,"IDTE: ",INVDTEM,"/",INVDTED,"/",INVDTEY:
                   *P27:VTAB2,*red,"EST: ",n9,*white,B1:
                    *P70:VTAB2,"QTY:",oqty;
         RETURN
..............................................................................
.exchange - dead
exchange
         DISPLAY   *P01:VTAB2,*EL,*HON,"LR##",*HOFF,LRN:
                   *P11:VTAB2,"IDTE: ",INVDTEM,"/",INVDTED,"/",INVDTEY:
                   *P27:VTAB2,*cyan,"Exchange ",*white,B1:
                    *P70:VTAB2,"QTY:",oqty;
         RETURN
..............................................................................
.billed use actual.         
DISS1    UNPACK   MLRPAYD INTO MM,DD,YY
         cmatch   "P" to statb               .paid ?
         goto      diss1p if equal           .yes
         DISPLAY   *P01:VTAB2,*EL,*HON,"LR##",*HOFF,LRN:
                   *P11:VTAB2,"IDTE: ",INVDTEM,"/",INVDTED,"/",INVDTEY:
                   *P27:VTAB2,*hon,"OPEN: ",n9,*hoff,B1:
                    *P70:VTAB2,"QTY:",QTYbild;
         RETURN
.
diss1p   DISPLAY   *P01:VTAB2,*EL,*HON,"LR##",*HOFF,LRN:
                   *P11:VTAB2,"IDTE: ",INVDTEM,"/",INVDTED,"/",INVDTEY:
                   *P27:VTAB2,"PAID: ",n9,B1:
                    "CHK##: ",CHecK,*P70:VTAB2,"QTY:",QTYbild;
         DISPLAY   *P68:VTAB1,"PDTE:",chk1dteM,"/",chk1dteD,"/",chk1dteY;
         RETURN
.
NOown
         MOVE      "OWner DESC. NOT FOUND",ownocpy
         RETURN
.
NEXTPAGE
         KEYIN     *P01:24,*EL,"There are more hit ENTER to see the rest":
                   " (*) to exit ":
                   *B,*T200,str1;
         CMATCH    "*",str1
         GOTO      START IF EQUAL
         MOVE      "01",VTAB
         MOVE      "02",VTAB1
         MOVE      "03",VTAB2
         RETURN
.
NOMORE
         KEYIN     *P01:24,*EL,*B,"NO RECORDS FOUND !!!!!!",*T65,str1;
         GOTO      START
.
NOMORE1
         KEYIN     *P01:24,*EL,*B,"NO RECORDS FOUND MEETING THE CRITERIA !!!!":
                   *T65,str1;
         GOTO      START
NOMORE2
         COMPARE   C0 TO N5
         GOTO      NOMORE1 IF EQUAL
         MOVE      MASK,TOTALA
         EDIT      TOTAL,TOTALA
         MOVE      MASK1,TOTAL2A
         EDIT      TOTAL2,TOTAL2A
         KEYIN     *P01:24,*EL,"That's all I can find, hit ENTER":
                   "  Total : ",*dv,total2a,*T60,str1;
         move      c0 to total
         GOTO      START
.ownhelp  DISPLAY   *SETSWALL  12:24:40:80;
.         KEYIN     *P1:1,*ES:
.                   *P1:12,*EL,"I WILL HELP YOU FIND THE OWNER NUMBER",*R:
.                   *P1:12,*EL,"THAT YOU WANT":
.                   *R,*P1:12
.KEYOWN   KEYIN     *P1:1,*ES,"TYPE IN OWNER NAME : ":
.                   *P1:2,*T60,OWNOCPY
.         KEYIN     *P1:2,*DV,OWNOCPY,*P1:3,"OK ? ",*T60,str1:
.                   *P1:3,*EL;
.         CMATCH    NO TO str1
.         GOTO      KEYOWN IF EQUAL
.         CMATCH    B1 TO OWNOCPY
.         GOTO      NTnofown IF EQUAL
.         GOTO      ntnofown IF EOS
.         PACK      NOWNFLD2 FROM AKEY1,OWNOCPY,QUES
.OWNEADA  MATCH     "01F" TO AKEY1            *FREE FLOAT SEARCH?
.         CALL      CHKKEY IF EQUAL           *YES, CHECK KEY LENGTH
.         MOVE      "*****NO RECORD FOUND*****" TO OWNOCPY
.         CALL      NOWNAIM
.         GOTO      OWNAEXIT IF OVER
.         GOTO      DISOWNA
CHKKEY   MOVELPTR  NOWNFLD2 TO KEYCOUNT
         COMPARE   c10 TO KEYCOUNT
         GOTO      ntnofown IF LESS         *NOT ENOUGH INFO, LET THEM KNOW
         RETURN                             *OK, RETURN
.OWNEADA1 MOVE      "*****NO RECORD FOUND*****" TO OWNOCPY
.         CALL      NOWNKG
.         GOTO      OWNAEXIT IF OVER
.         GOTO      DISOWNA
.OWNAEXIT
.         CALL      NORECORD
.OWNAEXT1 DISPLAY   *P1:1,*ES,*RESETSW:
.                   *P20:21,*HON,"ENTER TO EXIT,":
.                   " or (?) FOR HELP",*HOFF;
.         move      ownlon to kown
.         GOTO      DISOWN
.         GOTO      START
.DISOWNA  DISPLAY   *P1:09,*EL,"##",OWNLON,"  ":
.                   *P1:10,OWNLONM,*P1:11,OWNOCPY
.         KEYIN     *P1:13,"TAP ENTER TO CONTINUE, (*) TO END",*T254,str1;
.         CMATCH    star TO str1
.         GOTO      OWNAEXT1 IF EQUAL
.         GOTO      OWNEADA1
.NORECORD MOVE      "*****NO RECORD FOUND*****" TO OWNOCPY
.         DISPLAY   *P1:14,*EL,OWNOCPY,*W2,*B,*P1:13,*EL
.         RETURN
.
ntnofown NORETURN
.
NOGOOD
         DISPLAY   *P1:24,*HON,*EL,*B,"NOT ENOUGH INFORMATION TO SEARCH ON!!!":
                   *RESETSW:
                   *P20:21,*HON,"ENTER TO EXIT,":
                   " or (?) FOR HELP",*HOFF;
         PAUSE     "3"
         GOTO      START
.CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
.         RETURN    IF EQUAL                      ITS OK.
.FORMERR  MOVE      "0000000000" TO CVTFLD
.         RETURN
.CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
.
GETCARD  MOVE      OLNUM TO NDATFLD
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY
         RETURN    IF OVER
.START PATCH 3.0 REPLACED LOGIC
.         parse     textdata into text1 using " ~09",noskip,blankfill
.         SCAN      "EXCHANGE ONLY" IN TEXT1
.         RETURN    IF EQUAL                 NO USABLE $ RETURN
.         RESET     TEXT1
.         SCAN      "$" IN TEXT1
.         RETURN    IF NOT EQUAL        NO USABLE $ RETURN
.         BUMP      TEXT1 BY 1
.         PACK      STR2 FROM TEXT1
.         MOVE      STR2 TO FORM52
.         SCAN      "$" IN TEXT1        *DO WE HAVE CORRECT PRICE?
.         RETURN    IF NOT EQUAL            *YES.
.         CLEAR     STR2
.         BUMP      TEXT1 BY 1
.         PACK      STR2 FROM TEXT1       *NO, NOW WE DO!
.         MOVE      STR2 TO FORM52
.         RETURN
          if (NDATCONV = "1")
.                   if (NDATEXCH <> "1")
                              pack      NSELFLD1,"01X",LSTNUM
                              pack      NSELFLD2,"021XBASE"
                              move      "NSELAIM",Location
                              pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                              call      NSELAIM
                              if not over
                                        if (NSELEXC <> "2")
                                                  move      C0,FORM52
                                                  move      NSELPRICE,FORM52
                                        endif
                              else
                                        goto DataCheckText
                              endif
.                   endif
          else
DataCheckText
                    pack      NTXTFLD,LSTNUM,"1"
                    move      "D.Load-NTXTKEY",Location
                    pack      KeyLocation,"Key: ",NTXTFLD
                    call      NTXTKEY
                    if not over
                              move      NTXTTEXT,text1
                              SCAN      "EXCHANGE ONLY" IN TEXT1
                              RETURN    IF EQUAL                 NO USABLE $ RETURN
                              RESET     TEXT1
                              SCAN      "$" IN TEXT1
                              RETURN    IF NOT EQUAL        NO USABLE $ RETURN
                              BUMP      TEXT1 BY 1
                              PACK      STR2 FROM TEXT1
                              MOVE      STR2 TO FORM52
                              SCAN      "$" IN TEXT1        *DO WE HAVE CORRECT PRICE?
                              RETURN    IF NOT EQUAL            *YES.
                              CLEAR     STR2
                              BUMP      TEXT1 BY 1
                              PACK      STR2 FROM TEXT1       *NO, NOW WE DO!
                              MOVE      STR2 TO FORM52
                              RETURN
                    else
                              clear     text1
                    endif
          endif
                    Return
.END PATCH 3.0 REPLACED LOGIC
.
* ***************************************************************************
*  EXIT AND FERROR SUBROUTINES
* ****************************************************************************
.
INT      call      paint
         call      funcdisp
         trapclr   f5
         trap      int if f5
         Keyin     *P29:12,*HON,"Search Interupted !!!, Exit ",*cyan,yes:
                   *white,slash,no,*HOFF,str1
         cmatch    no to str1
         goto      start if equal          
EXIT     DISPLAY   *P01:01,*ES,*P29:12,*HON,"B Y E !!!",*HOFF;
         TRAP      EXIT1 IF F5
EXIT1    BEEP
         STOP
IODIS    DISPLAY   *P1:24,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*r,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!",*B;
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      EXIT1 IF EQUAL
         GOTO      IODIS
IO       TRAPCLR   IO
         TRAP      IO GIVING ERROR NORESET IF IO
         NORETURN
IODIS1   DISPLAY   *P1:24,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*r,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!",*B;
         KEYIN     *P70:24,*EOFF,str1,*RESETSW;
         CMATCH    "Q",str1
         GOTO      EXIT IF EQUAL
         GOTO      IODIS1
RANGE
         TRAPCLR   RANGE
         TRAP      RANGE GIVING ERROR NORESET IF RANGE
         NORETURN
RANGED   DISPLAY   *P1:24,*EL,"RANGE ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      EXIT1 IF EQUAL
         GOTO      RANGED
         STOP
FORMAT
         TRAPCLR   FORMAT
         NORETURN
         DISPLAY   *P1:24,*EL,"FORMAT ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      EXIT1 IF EQUAL
         GOTO      FORMAT
         STOP
PARITY
         TRAPCLR   PARITY
         NORETURN
         DISPLAY   *P1:24,*EL,"PARITY ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      EXIT1 IF EQUAL
         GOTO      PARITY
         STOP
..................................................................................................................................
.begin patch 3.4
..........................................................................................................
getOwner
           getitem   NINV0013EditText001,0,str4
           call      trim using str4

           count     n2 in str4
           if        (n2 = c3)
           move      str4 to str3
           pack      str4 from c0,str3
           Elseif        (n2 = c2)
           move      str4 to str2
           pack      str4 from c0,c0,str2
           Elseif        (n2 = c1)
           move      str4 to str1
           pack      str4 from c0,c0,c0,str1
           goto      buildokey
           endif

buildOkey
           setitem   NINV0013EditText001,0,str4
           clear     Kown
           packkey   Kown from str4
          PACK      nownfld FROM KOWN
          REP       zfill,nownfld
          CALL      NownKEY
           if        not over
           setitem   NInv0013Owncomp,0,OWNOCPY
         else
           setitem   NInv0013Owncomp,0," "
           endif
         return
..........................................................................................................
GetList
           getitem   NInv0013EditText005,0,str6

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
               else
               return
               endif

buildlstfld
           setitem   NInv0013EditText005,0,str6
           clear     ndatfld
           packkey   ndatfld from str6
           call      ndatkey
           if        not over
           setitem   NInv0013StatText008,0,Olstname
         else
           setitem   NInv0013StatText008,0," "
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
        getitem Ninv0013ComboBox001,0,result
        move    result to datebranch
               If             (DateBranch > 0 & DateBranch < c4)
               MOve           "T" to DateFilter
               endif

        getitem Ninv0013EditDate1,0,str10
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
                    setFocus Ninv0013EditDate1
                goto BadDate
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
                    setFocus Ninv0013EditDate1
                goto BadDate
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
                              setFocus Ninv0013EditDate1
                          goto BadDate
                else
                        move    STR2,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
                                        setFocus Ninv0013EditDate1
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
        setitem Ninv0013EditDate1,0,newdate1
.
        clear   mm
        clear   dd
        clear   yy
        clear   str2
        clear   newdate2
        getitem Ninv0013EditDate2,0,str10
        call    TRIM using str10
        count   N2,str10
        if (N2 = 10)
                unpack  str10,MM,str1,DD,str1,STR2,YY
        elseif (N2 = 8)
                unpack  str10,MM,DD,STR2,YY
        elseif (N2 <> 0)
                alert   caution,"Date Must be in MMDDCCYY Format",result
                    setFocus Ninv0013EditDate2
                goto BadDate
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
                    setFocus Ninv0013EditDate2
                goto BadDate
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
                              setFocus Ninv0013EditDate2
                        goto BadDate
                else
                        move    STR2,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
                                        setFocus Ninv0013EditDate2
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
        setitem Ninv0013EditDate2,0,NewDate2
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
                                setfocus  Ninv0013ComboBox001
                                goto BadDate
        else
        endif
        return

BadDate
               move           No to dateokflag
               setprop        Ninv0013GoButton,visible=1
               setprop        Ninv0013Stop,visible=0
               Move           "F" to DateFilter
        return
.......................................................................................................................
NInvForceToOne
               setitem Ninv0013TabControl001,0,1
               move        c1 to n1
          call        NINVTabChange

               return
                
...........................................................................................
NInvTabClick
        IF (N1 = C1)
            getprop Ninv0013aListView001,visible=N9
                move    n9 to ListViewNum
                Deactivate NInv13a
        elseif (N1 = C2 )
                Deactivate NInv13b
        setprop Ninv0013aListView001,visible=ListViewNum
        call    NInvSortListView
        elseif (N1 = C3 )
                Deactivate NInv13c
        Endif
        return

NInvTabChange
               Deactivate NInv13A
               Deactivate NInv13B
               Deactivate NInv13C
        IF (N1 = C1)
                move    C1,TabNum
                Activate NINV13a
                setfocus Ninv0013aListView001
            setprop Ninv0013aListView001,visible=ListViewNum
            call    NInvSortListView
                        LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT

        elseif (N1 = C2)
            getprop Ninv0013aListView001,visible=N9
                move    n9 to ListViewNum
                move    C2,TabNum
                Activate Ninv13b
                setfocus Ninv0013bListView001
.Prevent occurance or accumulated events which may place "hidden" objects on wrong form
.ie, ResetStatus Checkbox. This generally only happens with LostFocus events from Stats2.plf
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT

        elseif (N1 = C3)
                getprop Ninv0013aListView001,visible=N9
                move    n9 to ListViewNum
                move    C3,TabNum
                Activate NInv13C
                setfocus Ninv0013cEditText001
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT
        Endif
        return
..NInvSetFocusTab
NInvSetFocusTab
         setitem     NINV0013TabControl001,0,n1
         setfocus    NINV0013TabControl001,1
         return
        
.............................................................................................................
NInvSortListView
.Dynamically sorts Different ListViews.  
.In order to switch between different ListViews we need two pieces of information.
.We need to ascertain which column was clicked AND which ListView we currently
.have visible, as each ListView has its' columns ordered differently.
.Getprops will determine which ListView is currently active, #EventResult passed to result
.prior to calling this subroutine will determine which column was clicked.
        getprop Ninv0013aListView001,visible=N9
        if (N9 = C1)    .Ninv0013aListView001 is visible      ;by lr
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                If (result <> 1 and result <> 2 and result <> 6)  .no click?
                        setprop Ninv0013aListView001,visible=1
                        setprop Ninv0013aListView003,visible=0
                        setprop Ninv0013aListView002,visible=0
                        setprop Ninv0013aListView004,visible=0
                    Ninv0013aListView001.EnsureVisible using c1,0
                        setfocus Ninv0013alistview001
                endif
.
                if (result = 1)            .clicked on O date
                        setprop Ninv0013aListView001,visible=0
                        setprop Ninv0013aListView003,visible=0
                        setprop Ninv0013aListView004,visible=1
                        setprop Ninv0013aListView002,visible=0
                  Ninv0013aListView004.EnsureVisible using c1,0
                        setfocus Ninv0013alistview004
                Elseif (result = 2)  .clicked on MD
                        setprop Ninv0013aListView001,visible=0
                        setprop Ninv0013aListView003,visible=1
                        setprop Ninv0013aListView004,visible=0
                        setprop Ninv0013aListView002,visible=0
                  Ninv0013aListView003.EnsureVisible using c1,0
                        setfocus Ninv0013alistview003
                Elseif (result = 6)  .clicked on InvD
                        setprop Ninv0013aListView001,visible=0
                        setprop Ninv0013aListView003,visible=0
                        setprop Ninv0013aListView004,visible=0
                        setprop Ninv0013aListView002,visible=1
                    Ninv0013aListView002.EnsureVisible using c1,0
                        setfocus Ninv0013alistview002
                endif
.            endif

                else
                getprop Ninv0013aListView002,visible=N9
                if (N9 = C1)    .Ninv0013aListView002 is visible
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
.begin patch - note we have a hidden pseudo day column that scews the count
               if (result <> 2 and result <> 3 and result <> 4)
                                setprop Ninv0013aListView001,visible=0
                                setprop Ninv0013aListView002,visible=1
                                setprop Ninv0013aListView003,visible=0
                                setprop Ninv0013aListView004,visible=0
                            Ninv0013aListView002.EnsureVisible using c1,0
                            setfocus Ninv0013alistview002
                        Elseif (result = 2 )
                                setprop Ninv0013aListView001,visible=1
                                setprop Ninv0013aListView002,visible=0
                                setprop Ninv0013aListView003,visible=0
                                setprop Ninv0013aListView004,visible=0
                      Ninv0013aListView001.EnsureVisible using c1,0
                                  setfocus Ninv0013alistview001
                        Elseif (result = 3 )
                                setprop Ninv0013aListView001,visible=0
                                setprop Ninv0013aListView002,visible=0
                                setprop Ninv0013aListView003,visible=0
                                setprop Ninv0013aListView004,visible=4
                            Ninv0013aListView004.EnsureVisible using c1,0
                                  setfocus Ninv0013alistview004
                        Elseif (result = 4 )
                                setprop Ninv0013aListView001,visible=0
                                setprop Ninv0013aListView002,visible=0
                                setprop Ninv0013aListView003,visible=1
                                setprop Ninv0013aListView004,visible=0
                            Ninv0013aListView003.EnsureVisible using c1,0
                                  setfocus Ninv0013alistview003
                          endif
          else
                getprop Ninv0013aListView003,visible=N9
                if (N9 = C1)    .Ninv0013aListView003 is visible

.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                      if (result <> 2 and result <> 3 and result <> 7)
                                setprop Ninv0013aListView001,visible=0
                                setprop Ninv0013aListView002,visible=0
                                setprop Ninv0013aListView003,visible=1
                                setprop Ninv0013aListView004,visible=0
                            Ninv0013aListView003.EnsureVisible using c1,0
                            setfocus Ninv0013alistview003
                        Elseif (result = 2 )   .want lr order
                                setprop Ninv0013aListView001,visible=1
                                setprop Ninv0013aListView002,visible=0
                                setprop Ninv0013aListView003,visible=0
                                setprop Ninv0013aListView004,visible=0
                      Ninv0013aListView001.EnsureVisible using c1,0
                           setfocus Ninv0013alistview001
                        Elseif (result = 3 )      
                                setprop Ninv0013aListView001,visible=0
                                setprop Ninv0013aListView002,visible=0
                                setprop Ninv0013aListView003,visible=0
                                setprop Ninv0013aListView004,visible=1
                      Ninv0013aListView004.EnsureVisible using c1,0
                           setfocus Ninv0013alistview004
                        Elseif (result = 7 )      
                                setprop Ninv0013aListView001,visible=0
                                setprop Ninv0013aListView002,visible=1
                                setprop Ninv0013aListView003,visible=0
                                setprop Ninv0013aListView004,visible=0
                      Ninv0013aListView002.EnsureVisible using c1,0
                           setfocus Ninv0013alistview002
                          endif
.                endif
               else
                getprop Ninv0013aListView004,visible=N9
                if (N9 = C1)    .Ninv0013aListView004 is visible
.               call debug
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                      if (result <> 2 and result <> 3 and result <> 7)
                                setprop Ninv0013aListView001,visible=0
                                setprop Ninv0013aListView002,visible=0
                                setprop Ninv0013aListView003,visible=0
                                setprop Ninv0013aListView004,visible=1
                            Ninv0013aListView004.EnsureVisible using c1,0
                            setfocus Ninv0013alistview004
                        Elseif (result = 2 )   .want lr order
                                setprop Ninv0013aListView001,visible=1
                                setprop Ninv0013aListView002,visible=0
                                setprop Ninv0013aListView003,visible=0
                                setprop Ninv0013aListView004,visible=0
                      Ninv0013aListView001.EnsureVisible using c1,0
                               setfocus Ninv0013alistview001
                        Elseif (result = 3 )   
                                setprop Ninv0013aListView001,visible=0
                                setprop Ninv0013aListView002,visible=0
                                setprop Ninv0013aListView003,visible=1
                                setprop Ninv0013aListView004,visible=0
                      Ninv0013aListView003.EnsureVisible using c1,0
                          setfocus Ninv0013alistview003
                        Elseif (result = 7 )   
                                setprop Ninv0013aListView001,visible=0
                                setprop Ninv0013aListView002,visible=1
                                setprop Ninv0013aListView003,visible=0
                                setprop Ninv0013aListView004,visible=0
                            Ninv0013aListView002.EnsureVisible using c1,0
                                  setfocus Ninv0013alistview002
                          endif
                endif
        endif
        endif
        endif
        return
.........................................................................................................................
SearchGo
        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4
SearchGo1
.BROKER- not an option with this program
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
.MAILER- not an option with this program
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
SearchGo5
.CAMPAIGN - not an option with this program
          move      C5,SrchFlag
          call      SearchSetTitle
          call      SearchSetVisible
          return

.START PATCH 3.66 ADDED LOGIC
SearchGo6
.OWNER
          move      C6,SrchFlag
          call      SearchSetTitle
          call      SearchSetVisible
          return
SearchLoad
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6
SearchLoad1
.BROKER
        return
SearchLoad2
.LIST 
        unpack      Srchstr,str6,str1,str35
        setitem Ninv0013EditText005,0,str6
        setitem Ninv0013StatText008,0,str35
        setfocus Ninv0013EditText005
        return
SearchLoad3
.MAILER
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
SearchLoad5
.CAMPAIGN - not an option with this program
        return
SearchLoad6
.OWNER - 
          unpack    Srchstr,str4,str1,str3,str1,str45
          setitem   NINV0013EditText001,0,str4
          setitem   NInv0013Owncomp,0,str45
          setfocus  NINV0013EditText001
        return
.

HelpGo
        setprop AboutMssg,visible=1
        return       
...........................................................................................................
ClearNInvSearchList
.Clear ListView
               Ninv0013aListView001.DeleteAllItems giving N9
               Ninv0013aListView002.DeleteAllItems giving N9
               Ninv0013aListView003.DeleteAllItems giving N9
               Ninv0013bListView001.DeleteAllItems giving N9
               Ninv0013bListView002.DeleteAllItems giving N9
               mOVE           C0 TO GrandBalance
               mOVE           C0 TO SelectTotal
               mOVE           C0 TO Selectcount
               setitem        Ninv0013aEditText001,0,""
               setitem        Ninv0013aEditText002,0,""
               setitem        Ninv0013aEditText004,0,""
               setitem        Ninv0013aEditText005,0,""
               clear        str5
               clear    str14
          FOR counter,"1","25"
                              Clear Summary(counter).SumKey
                              MOve  c0 to Summary(Counter).SumAmountMOA
                              MOve  c0 to Summary(Counter).SumAmountAP
               repeat

               return
...........................................................................................................
NInvDetailClear
               setitem    Ninv0013cEditText001,0," "
               setitem    Ninv0013cEditText002,0," "
.               setitem    Ninv0013cEditText003,0," "
               setitem    Ninv0013cEditText004,0," "
               setitem    Ninv0013cEditText005,0," "
               setitem    Ninv0013cEditText006,0," "
               setitem    Ninv0013cEditText007,0," "
               setitem    Ninv0013cEditText008,0," "
               setitem    Ninv0013cEditText009,0," "
               setitem    Ninv0013cEditText010,0," "
.               setitem    Ninv0013cEditText011,0," "
               setitem    Ninv0013cEditText012,0," "
.               setitem    Ninv0013cEditText003,0," "
               setitem    Ninv0013cEditText011,0," "
               Move           c0 to total
               Move           c0 to totalB
               Move           c0 to totalU

               return
.............................................
.........................................................................................................
DisplayInvDetail
.need to finish this section add some order goodies etc.
               if             (lrn = "495393")
.               call           debug
               endif
               call           NInvkey
               setitem        Ninv0013cEditText005,0,LRn
               setitem        Ninv0013cEditText004,0,Invnum
               clear          str10
               pack           str10 from invdtem,slash,invdted,slash,invdtec,invdtey
               setitem        Ninv0013cEditText007,0,str10
                              Move      c1 to nordpath
                              move      lrn to nordfld
                              rep       zfill in nordfld
                              call      nordkey
               clear          str10
               pack           str10 from oodtem,slash,oodted,slash,oodtec,oodtey
               setitem        Ninv0013cEditText001,0,str10
               clear          str10
               pack           str10 from omdtem,slash,omdted,slash,omdtec,omdtey
               setitem        Ninv0013cEditText002,0,str10
               setitem        Ninv0013cEditText006,0,olnum
               setitem        Ninv0013cEditText009,0,omlrky
               setitem        Ninv0013cEditText010,0,OMLRPON
               call           nolist
               MOVE           olnum TO ndatfld
               move           c1 to ndatpath
               rep            zfill in ndatfld
               CMATCH         B1 TO ndatfld
               CALL           NDATkey IF NOT Eos
               setitem        Ninv0013cEditText012,0,Olstname
.begin patch
          pack      Mkey from Omlrnum,"000"
            move          c1 to nmlrpath
          call      Nmlrkey
          setitem   Ninv0013cEditText003,0,Omlrnum
          Setitem   Ninv0013cEditText011,0,Mcomp
.end patch

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
                    add        aspayad1 to ap1
               
               endif

               move           MaskAmountAR to Str15
               edit           Formar to str15
               setitem        Ninv0013cEditText008,0,str15                  ;amount
               clear          str10
                getprop       Ninv0013aListView001,visible=N9
                move          n9 to ListViewNum
               move           MaskAmountAR to Str15
               edit           Ap1 to str15
               setitem        Ninv0013cEditTextAp,0,str15                        
               setitem        Ninv0013cEditTextchk,0,chkn1                 
                setfocus      Ninv0013cEditText001
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
               setitem        Ninv0013cEditText006,0,olnum
               packkey        Ndatfld from olnum
               move           c1 to ndatpath
               call           Ndatkey
               Setitem        Ninv0013cEditText012,0,olstname
               MOve           Olnum to List
               MOVE           C1 TO NINVPATH
               packkey        Ninvfld from olrn
               CALL           NINVKEY
               If             over
               clear          Invoice
               Clear          Invdate
               setitem        Ninv0013cEditText007,0,""
               setitem        Ninv0013cEditText004,0,""
               else
               PACK           INVDATE FROM INVDTEC,INVDTEY,INVDTEM,INVDTED         
               pack           str13 from invdtem,slash,invdted,slash,invdtec,invdtey
               setitem        Ninv0013cEditText007,0,str13
               setitem        Ninv0013CEditText004,0,Invnum
               move           Invnum to Invoice
               endif
               setFocus       Ninv0013cEditText010
.begin patch
          pack      Mkey from Omlrnum,"000"
            move          c1 to nmlrpath
          call      Nmlrkey
          setitem   Ninv0013cEditText003,0,Omlrnum
          Setitem   Ninv0013cEditText011,0,Mcomp
.end patch
               return
.........................................................................................................
.
Ninv0013
        beep
        beep
        beep
Filego  winshow
        stop
..........................................................................................................

...............................................................................
NOLIST   clear     OLSTNAME
         RETURN
...............................................................................
loadSummaryListView
               move           c0 to ARTOTAL
          FOR counter,"1","25"
                              Move Summary(counter).SumKey,str8
                              Move Summary(Counter).SumAmountAP,total
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
               Ninv0013bListView001.InsertItem giving N9 using brcomp
               Ninv0013bListView001.SetItemText using N9,mcomp,1
               move           MaskAmountAR to Str15
               edit           total to str15
               add            total to artotal
               Ninv0013bListView001.SetItemText using N9,str15,2
               move           MaskAmount to Str13
               edit           Balance to str13
               Ninv0013bListView001.SetItemText using N9,str13,3
               Ninv0013bListView001.SetColumnFormat using 5,2              .set $ column justify right
               Ninv0013bListView001.SetColumnFormat using 5,3              .set $ column justify right
.
               Ninv0013bListView002.InsertItem giving N9 using Mcomp
               Ninv0013bListView002.SetItemText using N9,brcomp,1
               move           MaskAmountAR to Str15
               edit           total to str15
               Ninv0013bListView002.SetItemText using N9,str15,2
               move           MaskAmount to Str13
               edit           Balance to str13
               Ninv0013bListView002.SetItemText using N9,str13,3
               Ninv0013bListView002.SetColumnFormat using 5,2              .set $ column justify right
               Ninv0013bListView002.SetColumnFormat using 5,3              .set $ column justify right
.add grand total goodies
               MULT           "-1" by BALANCE
               ADD            BALANCE TO GRANDBALANCE
               repeat
               move           MaskAmount to Str13
               edit           GrandBalance to str13
               Setitem        Ninv0013bEditText001,0,str13
               MOve           MaskAmountAr to str15
               edit           ARTotal to str15
               Setitem        Ninv0013bEditText002,0,str15
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
                                             IF             (StatB = "P" or str4 = "CASH" & repflag = 3)
                                             goto           ExitCHeckFilters1
                                             Elseif       (StatB <> "P" & str4 <> "CASH" & repflag = 2)
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

          MOVE      C0 TO N9
          MOVE      AP1 TO N9
          ADD       N9,AP1TOT
          MOVE      C0 TO N9
          MOVE      AP2 TO N9
          ADD       N9,AP2TOT

          if        (c0 = N9)
          MOVE      C0 TO N9
          MOVE      AP1 TO N9
          ADD       N9 TO TOTAL2
          move      chkn1 to check
          else
          MOVE      C0 TO N9
          MOVE      AP2 TO N9
          ADD       N9 TO TOTAL2
          move      chkn2 to check
          endif

.               call           debug
               move           MaskAmountAR to Str15
               edit           N9 to str15
               Ninv0013aListView001.InsertItem giving N9 using Olrn
               clear     str10
               pack      str10 from Oodtem,slash,oodted,slash,oodtec,oodtey
               Ninv0013aListView001.SetItemText using N9,str10,1
               clear     str10
               pack      str10 from Omdtem,slash,omdted,slash,omdtec,omdtey
               Ninv0013aListView001.SetItemText using N9,str10,2
               Ninv0013aListView001.SetItemText using N9,Olstname,3
               move           oqty to str9
               call           formatnumeric using str9,str11
               Ninv0013aListView001.SetItemText using N9,str11,4
               Ninv0013aListView001.SetColumnFormat using 4,1              .set $ column justify right
               Ninv0013aListView001.SetItemText using N9,str15,5
               clear     str10
               pack      str10 from invdtem,slash,invdted,slash,invdtec,invdtey
               call           trim using str10
               if             ("//" = str10)
               clear          str10
               endif
               Ninv0013aListView001.SetItemText using N9,str10,6
               Ninv0013aListView001.SetItemText using N9,chkn1,7
               Ninv0013aListView001.SetColumnFormat using 5,1              .set $ column justify right
.
               clear     str10
               pack      str10 from invdtec,invdtey,invdtem,invdted
               Ninv0013aListView002.InsertItem giving N9 using str10
               clear     str10
               pack      str10 from invdtem,slash,invdted,slash,invdtec,invdtey
               Ninv0013aListView002.SetItemText using N9,str10,1
               Ninv0013aListView002.SetItemText using N9,olrn,2
               clear     str10
               pack      str10 from Oodtem,slash,oodted,slash,oodtec,oodtey
               Ninv0013aListView002.SetItemText using N9,str10,3
               clear     str10
               pack      str10 from Omdtem,slash,omdted,slash,omdtec,omdtey
               Ninv0013aListView002.SetItemText using N9,str10,4
               Ninv0013aListView002.SetItemText using N9,Olstname,5
               Ninv0013aListView002.SetItemText using N9,str11,6
               Ninv0013aListView002.SetItemText using N9,str15,7
               Ninv0013aListView002.SetItemText using N9,chkn1,8
               Ninv0013aListView002.SetColumnFormat using 6,1              .set $ column justify right
               Ninv0013aListView002.SetColumnFormat using 7,1              .set $ column justify right
.
               clear     str10
               pack           str10 from omdtec,omdtey,omdtem,omdted
               Ninv0013aListView003.InsertItem giving N9 using str10
               clear     str10
               pack      str10 from Omdtem,slash,omdted,slash,omdtec,omdtey
               Ninv0013aListView003.SetItemText using N9,str10,1
               Ninv0013aListView003.SetItemText using N9,olrn,2
               clear     str10
               pack      str10 from Oodtem,slash,oodted,slash,oodtec,oodtey
               Ninv0013aListView003.SetItemText using N9,str10,3
               Ninv0013aListView003.SetItemText using N9,Olstname,4
               Ninv0013aListView003.SetItemText using N9,str11,5
               Ninv0013aListView003.SetColumnFormat using 5,1              .set $ column justify right
               Ninv0013aListView003.SetItemText using N9,str15,6
               Ninv0013aListView003.SetColumnFormat using 6,1              .set $ column justify right
               clear     str10
               pack      str10 from invdtem,slash,invdted,slash,invdtec,invdtey
               Ninv0013aListView003.SetItemText using N9,str10,7
               Ninv0013aListView003.SetItemText using N9,chkn1,8

.
               clear     str10
               pack           str10 from oodtec,oodtey,oodtem,oodted
               Ninv0013aListView004.InsertItem giving N9 using str10
               clear     str10
               pack      str10 from Oodtem,slash,oodted,slash,oodtec,oodtey
               Ninv0013aListView004.SetItemText using N9,str10,1
               Ninv0013aListView004.SetItemText using N9,olrn,2
               clear     str10
               pack      str10 from Omdtem,slash,omdted,slash,omdtec,omdtey
               Ninv0013aListView004.SetItemText using N9,str10,3
               Ninv0013aListView004.SetItemText using N9,Olstname,4
               Ninv0013aListView004.SetItemText using N9,str11,5
               Ninv0013aListView004.SetColumnFormat using 5,1              .set $ column justify right
               Ninv0013aListView004.SetItemText using N9,str15,6
               Ninv0013aListView004.SetColumnFormat using 6,1              .set $ column justify right
               clear     str10
               pack      str10 from invdtem,slash,invdted,slash,invdtec,invdtey
               Ninv0013aListView004.SetItemText using N9,str10,7
               Ninv0013aListView004.SetItemText using N9,chkn1,8
ExitCheckFilters
               add            c1 to selectcount
               move           selectcount to str5
               setitem        Ninv0013aEditText002,0,str5
               move           MaskAmountAR to Str15
               edit           Total to str15
               setitem        Ninv0013aEditText001,0,str15
               move           MaskAmountAR to Str15
               edit           TotalU to str15
               setitem        Ninv0013aEditText004,0,str15
               move           MaskAmountAR to Str15
               edit           TotalB to str15
               setitem        Ninv0013aEditText005,0,str15
.;Before we return check summary info and add info 
               Clear          Str8
               pack           str8 from obrknum,omlrnum
               rep            zfill in  str8
               MOve           c0 to Avail
          FOR counter,"1","25"
                              IF ( STR8=Summary(counter).SumKey)
.                              add  Formar to Summary(Counter).SumAmountAP
                              add  Total2 to Summary(Counter).SumAmountAP
                              Break
                              endif
                              IF ( Summary(counter).SumKey = "" & avail=0)
                              MOve           Counter to Avail
                              endif
               repeat
               IF             (Avail <> 0)
               MOVe           Str8 to Summary(Avail).Sumkey
.               move           Formar to Summary(Avail).SumAmountAP
               move           Total2 to Summary(Avail).SumAmountAP
               endif
ExitCheckFilters1
               return
.
ExitNInvSearchList
               Return
.................................
KEYEDIT
.
         MOVE      "0",VALKEY
         MATCH     "0000",KLIST
         CALL      CUME IF EQUAL
.
         MATCH     "0000",KOwn
         CALL      CUME IF EQUAL
.
         cmatch    b1 to str12
         call      cume if eos
         RETURN
.
CUME
         ADD       "1",VALKEY
         RETURN
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
        prep    colorfile,"c:\program files\nincal\NInv0013.col"
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
.end patch 3.4

.;patch3.1
          include   compio.inc
          include   cntio.inc
.         INCLUDE   NMLRIO.inc
.;patch3.1
.;begin patch 3.2
.;INCLUDE   NINVIO.inc
               Include        ninvio.inc
.;end patch 3.2
         include  ndatio.inc
.;patch3.1
.         INCLUDE   NBRKIO.inc
.;patch3.1
         INCLUDE   NORDIO.INC
         include   nownio.inc
         include   listhelp.inc
.begin patch 2.8
         include   nadjio.inc
         include   nofrio.inc

.end patch 2.8
         include   nshpio.inc
.START PATCH 3.0 REPLACED LOGIC
          INCLUDE   NSELIO.INC
          INCLUDE   NTXTIO.INC
          include   nsel2io.inc
          include   nmodio.inc
.END PATCH 3.0 REPLACED LOGIC
.begin patch 3.4
         INCLUDE   NMOAio.inc
         INCLUDE   NMOBio.inc
          include   searchio.inc      .contains logic for search.plf
          include   ncmpio.inc
          include   nrtnio.inc
          include   Ndat3io.inc
          inc       nmrgio.inc
          include   Ninvacdio.inc
          include   Nacdio.inc
          include  compute.inc
.end patch 3.4
         INCLUDE   COMLOGIC.inc
