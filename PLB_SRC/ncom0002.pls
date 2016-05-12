;New CARE/Domain Group Inc. Commission Report

PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   CONSacct.inc
         include   NORDDD.INC
;begin patch 1.5
;         include   NINVDD.INC
         include   ninvdd.inc
         Include    NinvAcddd.inc
;end patch 1.5
         include   NSHPDD.INC
         include   NMRGDD.INC
         include   NOWNDD.INC
         include   NDATDD.INC
         include   NDAT3DD.INC
         include   NACDDD.INC
         include   NADJDD.INC
         include   NCMPDD.INC

.START PATCH 1.1 REPLACED LOGIC
.        include nmlrdd.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
.END PATCH 1.1 REPLACED LOGIC
         INCLUDE   NPASDD.INC
         INCLUDE   LOGDATA.inc
         include   nusedd.inc
         include   NCOMDD.INC
Release   Init      "1.8"    DLH    Sunbelt PDF
Reldate   Init      "2013 Arpil 23"
.Release   Init      "1.71"    27march2007 DLH    fix
.Reldate   Init      "27 March 2007"
.Release  init     "1.7"         07JUL2006 DMB Revision per DH 7/7/06.  We are not going to use the campaign mail date at all.  Sales is clearing mulitple mail dates 
.//in one campaign which makes the single campaign mail date invalid.  We will only refer to the order's mail date when calculating
.//the due date.  DH no longer wants to use the invoice date since if the invoice date is too much later it means that the clients vendor was not supplying the information needed in 
.//which case we should not be penalized nor should the client be given extra time to pay
.//Release  init   "1.6"          09MAY2005 DMB Fixed bug that allowed the total to represent the total possible if records were paid after due date during the initial load routine.  
.;                               Only if the quarter records were activated in the gui listview would routine correct the balance.  This has been fixed to have routine see if item has been checked (paid before due date)
.;                               if true then add to quarter total.
.;Release  init   "1.5"          09MAY2005 DMB Modified Code to allow printing of total further down on page w/0 making a new page
.Release  init   "1.4"          04OCT2004 DMB added code to print thru pdf995
.Release  init   "1.2"          05AUG2004 ASH LOGO CONVERSION
.Release  init   "1.1"          27MAY2004 ASH MAILER CONVERSION
.Release  init   "1"          03MAR03 DMB Initial Release of Domain Group Commission Report
.needed for printing to PDF for non native fonts and things like Copyright to appear correctly in the pdf
PDF_FLAGS_WIN_ANSI_ENCODING EQU 128
PCOMFILE  PFILE
SEQCHK    FILE
CHECKFILE IFILE    
CAREFILE  FILE
SORTFLE   DIM    70                            .Var to pack file names of sort
;         DIM   10            MISC
COMPO     DIM   12            Mailer PO
COMKEY    DIM   12            MAILER Key
COMMLR    DIM   45            Mailer Name
COMNUM    DIM    4            Mailer Num
COMLR     DIM    6            Mailer LR
COMORD    DIM   10            Mailer Order Date
COMRTN    DIM   10            Mailer Return Date
COMMAIL   DIM   10            Mailer Mail Date
COMINV    DIM   10            Mailer Invoice Date
COMRECD   DIM   10            Mailer Payment Rec'd Date
COMRE     DIM    4            Mailer Rent/Exch
COMNET    DIM    2            Mailer Net
COMRENT   DIM   10            Mailer Rental Qty
COMEXCH   DIM   10            Mailer Exchange Qty
COMOUTP   DIM    9            Mailer Output
COMCAMP   DIM    6            Campaign #
COMCDATE  DIM    8            Campaign Date
COMAR     FORM  10.2
COMCHG    FORM   2            NIN Commission
COMBILD   FORM   9            BILLED Qty from Invoice
COMPPM    FORM   3.2          PPM from Invoice
;Flags
QUARTERFLG    FORM  1
DISQUALIFIED1 DIM   1
DISQUALIFIED2 DIM   1
DISQUALIFIED3 DIM   1
DISQUALIFIED4 DIM   1
;To segment Running total by Quarter
;First
BegQtr1       FORM     5
EndQtr1       FORM     5
;Second Quarter
BegQtr2       FORM     5
EndQtr2       FORM     5
;Third
BegQtr3       FORM     5
EndQtr3       FORM     5
;Fourth Quarter
BegQtr4       FORM     5
EndQtr4       FORM     5
;To Segment Year 
BEGYr        FORM     5
ENDYr        FORM     5
;Segment Previous Year
PrvBegYr     FORM     5
PrvEndYr     FORM     5
;Order Julian Date - used to compute refund amount at end of year
OrdJul       FORM     5
;Start date of agreement
START        FORM     "8217"
;AIM READS
Akey1    init      "01X"
Akey2    init      "02X"
Akey3    init      "03X"
;.Tiered Limits
TIERFLAG     FORM     1
Tier         FORM     2.2
Tier1        FORM    ".05"
Tier2        FORM    ".10"
Tier3        FORM    ".15"
Tier4        FORM    ".20"
Tier1QTY     FORM    "5000000"
Tier2QTY     FORM    "10000000"
Tier3QTY     FORM    "15000000"
Tier4QTY     FORM    "20000000"
;.Vars to help formualte tiering amts
;TIERAMT1     FORM    7.2
;TIERAMT2     FORM    7.2
;TIERAMT3     FORM    7.2
;TIERAMT4     FORM    7.2
;TIERCUMAMT1  FORM    7.2
;TIERCUMAMT2  FORM    7.2
;TIERCUMAMT3  FORM    7.2
;TIERCUMAMT4  FORM    7.2
;TIERCUMQTY1  FORM    10
;TIERCUMQTY2  FORM    10
;TIERCUMQTY3  FORM    10
;TIERCUMQTY4  FORM    10
;Subtotals for Two Year Current and Past
TierYrPrev   FORM    10
TierYrCur    FORM    10
;Total Refund Amt per quarter
QtrRefund1   FORM    9.2
QtrRefund2   FORM    9.2
QtrRefund3   FORM    9.2
QtrRefund4   FORM    9.2
;Possible Refund Amt per quarter
PosQTRRefund1   FORM    9.2
PosQtrRefund2   FORM    9.2
PosQtrRefund3   FORM    9.2
PosQtrRefund4   FORM    9.2
;Total Refund Amt Per Year
YearRefund  FORM    9.2
;test var
curyr        DIM    4
;For Progress Bar
;tempfile    FILE
;CurRec      FORM  5.2
;CurVal      FORM  3
;LastVal     form  3
;For Julian Comparison
MailDate    FORM   5
InvDate     FORM   5
RECDDate    FORM   5
CALCDATE    FORM   5
PMTDUE      FORM   5
COMDUE      DIM   10       .Due date for commision refund     
;Misc Vars
SIXTY       DIM    6
DUMVAR92    FORM   9.2
COMMISH     FORM   9.2            ;commission earned
LRINCOM     FORM   9.2            ;invoice lrincom
shipsw      DIM    1
mrgsw       DIM    1
n12         FORM   12
mask13      init    "Z,ZZZ,ZZZ,ZZ9"         ;formatting vars
Dim13a      dim     13            ;formatting vars
mask15      init    "ZZZ,ZZZ,ZZZ.99"        ;formatting vars
Dim15a      dim     15            ;formatting vars
PAID        DIM     1             ;cc holding var
CCField     Dim     2                       ;hold Century
NewDate1    DIM    10                       ;New Formatted Date
Holddat     DIM     8             ;holdvar for checkdate
HOLDNUM1    DIM     6             ;holdvar for check number
TabNum      form    9
;Print File Creation
.Patch 1.4
PRTITLE   DIM    50
PRTNAME1  DIM    50
PRTDIR    INIT   "c:\work\pdf"
.Patch 1.4
PRTFILE1  DIM    40
QTRHEADER DIM    17
;.Page Count
Rowcount    form    3             .KEEP TRACK OF ROW PER PAGE
PgCnt       form    9             .COUNT OF PAGES
CRECORDS    FORM    9
;Hold Variables
HOLDCLIENT  DIM     45
HOLDFLAG    DIM      1
;report stuff
RptCan      DIM     1
EditTextBoxes   EditText (4)
Buttons         Button  (3)
StatTextBoxes   StatText (7)
objectcoll      Collection
;ListView
;VT_BOOL EQU 11
;OTRUE               variant
;OFALSE              variant

;ColHeads            automation
;ColHead  automation
;ListIts  automation
;ListIt   automation
;SubIt    automation

;NewItem  automation
;FirstFont           font

;$Click              CONST "4294966696"
;$MouseMove          CONST "4294966690"
;$ColumnClick        CONST "3"
;$KeyPress           CONST "4294966693"
;$KeyDown            CONST "4294966694"
;Print Columns
;Columns
Title2   form    9
Title3   form    9
.Column8  form    9
.Column9  form    9
font7    font
        create  font7,"Arial",size=10
font9    font
         create  font9,"Times New Roman",size=10,italic
font8    font
         create  font8,"Times New Roman",size=9,bold,italic
         move    "100",column
         move    "500",column9
         move    "3000",column1
         move    "4000",column2
         move    "5000",column3
         move    "5260",column4
         move    "6520",column5
         move    "8000",column6
         move    "7800",column7
         move    "10000",column8
         move    "9320",Title2      ;.Date Header
         move    "5260",Title3

;=====================================================================
        move    "NCOM0002.PLS",Wprognme
        move    "CARE/Domain Group Commission",Wfunction
        move    "David Baca",Wauthor
        move    release,Wrelease
        move    Reldate,Wreldate
;=======================================================================
;Test for port 1.33
PORTNUM  DIM       3
ID       DIM       1
PORTX    FORM      3
DIM3     DIM       3
ANS      DIM       1
SFILE    sndfile
SoundFLag form   1
NAME     DIM       25
;Test for port 1.33


mauve     color
colornum            form      24
yellow    color
red       color
blue      color
;RowHold  form      9
abt                 plform  About
rpt2                plform  Report2
pss plform          Passwrd
y1 plform ncar001b
x1 plform           ncar001a
x  plform ncar0001
          winhide
          formload x
                    formload x1,ncar0001
                    formload y1,ncar0001
                    formload pss
                    formload rpt2
                    formload abt
                     
;Form Menu
;.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu

FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options"
RData   init    "&Reports"
HData   init    "&Help;&About"
        create  ncar0001;mFile,FData
        create  ncar0001;mEdit,EData,mFile
        create  ncar0001;mOptions,OData,mEdit
        create  ncar0001;mHelp,HData,mOptions
.START PATCH 1.2 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.2 ADDED LOGIC
;.Activate Menus
;.FileGo leads to stop
        activate mFile,FileGo,result
;.Need this when it works
        activate mEdit,EditGo,result
        activate mOptions,OptionsGo,result
;.Only a SubMenu under this one
        activate mHelp,HelpGo,result
;Sort By Order Date
        MOVE   "NINCAR",NCOMNAME
        pack   SortFle,NTWKPATH1,"careaccr.dat",comma,NTWKPATH1,"careaccr.srt"
        pack   taskname,sortfle,";","96-99,90-91,93-94"
        sort   taskname
        create      blue=*blue
        create      red=*red
        create      yellow=255:255:160
        create      mauve=150:60:100
        getitem     red,0,colornum
        call   columns
        call   columns2
        call   columns3
        call   columns4
          call NCAR001bColumns
        PACK     str45 with ntwkpath1,"careaccr.srt"
;        OPEN     tempfile,str45
;        positeof tempfile
;        fposit   tempfile,N10
;        calc     howmany=(N10/207)     ;. 205 = 205(CARE record length) + 2 bytes for CR/LF
;        close    tempfile
        OPEN     CAREFILE,str45
;Patch 1.33
         MOVE       C1 TO NUSEPATH    *ACCESS BY PORTNUMBER
         call findport
         move       "V",Progcode
;Patch 1.33
        setfocus  ncar001aEditText009
          loop
                    waitevent
          repeat
;For Progress Bar
;        CALL    NComInitProgressBar

;        getprop  NCAR0001ListView001,*ListItems=ListIts
;for testing
;        move "2003" to curyr
;        move curyr to n4
RunCommission
        call clearform1
        call clearvars1
        reposit CAREFILE,c0
        getitem ncar001aEditText009,0,str4
        move str4 to n4
        if (n4 < "1988")
          alert caution,"Date Must be for valid Year!(1988 & up)",result,"Invalid Year!"  
          return
        endif
;Check to see if check has been cut for this fical year
        clear NCOMFLD1
        clear NCOMFLD2
        clear NCOMFLD3
        Pack  NCOMFLD3,AKEY3,str4
        call NCOMAIM
        if not over
                   alert caution,"A Check has already been cut for this fiscal year.",result,"Already Ran"
                   return
        endif
        move n4 to curyr        
        sub c1 from n4
        move n4 to str4
        pack str7 with "FY ",str4
        setitem NCAR0001StatText004,0,str7
        move curyr to str4
        pack str7 with "FY ",str4
        setitem NCAR0001StatText003,0,str7
        setprop ncar001alistview004,zorder=1001
;.Branch to get params for YEAR
;curyr
        move c7 to mm
        move c1 to dd
        unpack curyr to cc,yy
        move yy to n2
        subtract c1 from n2
        move n2 to yy 
        call zfillit using mm
        call zfillit using DD
        move yy to str2
        rep zfill,str2
        pack str24 with mm,slash,dd,slash,cc,str2
        call trim using str24
        setitem ncar001aStatText007,0,str24
        call cvtjul                
        move juldays to BegYr
        unpack curyr,cc,yy
        move c6 to mm
        move c30 to dd
        call zfillit using mm
        call zfillit using DD
        move yy to str2
        rep zfill,str2
        pack str24 with mm,slash,dd,slash,cc,str2
        call trim using str24
        setitem ncar001aStatText008,0,str24      
        call cvtjul                
        move juldays to EndYr
        
;Previous Year
        move c7 to mm
        move c1 to dd
        unpack curyr to cc,yy
        move yy to n2
        subtract c2 from n2
        move n2 to yy 
        call zfillit using mm
        call zfillit using DD
        call zfillit using yy
        call cvtjul                      
        move juldays to PrvBegYr
        unpack curyr,cc,yy
        move yy to n2
        subtract c1 from n2
        move n2 to yy
        move c6 to mm
        move c30 to dd
        call zfillit using mm
        call zfillit using DD
        call zfillit using yy
        call cvtjul                     
        move juldays to PrvEndYr
;getquarter
FirstQuarter
        move c7 to mm
        move c1 to dd
        unpack curyr to cc,yy
        move yy to n2
        subtract c1 from n2
        move n2 to yy
        call zfillit using mm
        call zfillit using DD
        call zfillit using yy
        call cvtjul
        move juldays to begqtr1
        move c9 to mm
        move c30 to dd
        call zfillit using mm
        call zfillit using DD
        call zfillit using yy
        call cvtjul
        move juldays to endqtr1
;        goto readfile
SecondQuarter
        move c10 to mm
        move c1 to dd
        unpack curyr to cc,yy
        unpack curyr,cc,yy
        move yy to n2
        subtract c1 from n2
        move n2 to yy
        call zfillit using mm
        call zfillit using DD
        call zfillit using yy
        call cvtjul
        move juldays to begqtr2
        move "12" to mm
        move c31 to dd
        call zfillit using mm
        call zfillit using DD
        call zfillit using yy
        call cvtjul
        move juldays to endqtr2
;        goto readfile
ThirdQuarter
        move c1 to mm
        move c1 to dd
        unpack curyr to cc,yy
        call zfillit using mm
        call zfillit using DD
        call zfillit using yy
        call cvtjul
        move juldays to begqtr3
        move c3 to mm
        move c31 to dd
        call zfillit using mm
        call zfillit using DD
        call zfillit using yy
        call cvtjul
        move juldays to endqtr3
;        goto readfile
FourthQuarter
        move c4 to mm
        move c1 to dd
        unpack curyr to cc,yy
        call zfillit using mm
        call zfillit using DD
        call zfillit using yy
        call cvtjul
        move juldays to begqtr4
        move c6 to mm
        move c30 to dd
        call zfillit using mm
        call zfillit using DD
        call zfillit using yy
        call cvtjul
        move juldays to endqtr4
;        goto readfile
 
readfile
        loop
        READ    CAREFILE,SEQ;b10:
                        COMPO:
                        COMKEY:
                        COMMLR:
                        COMNUM:
                        COMLR:
                        COMORD:
                        COMRTN:
                        COMMAIL:
                        COMINV:
                        COMRECD:
                        COMRE:
                        COMNET:
                        COMRENT:
                        COMEXCH:
                        COMOUTP:
                        b1:
                        COMCAMP:
                        COMCDATE:
                        COMAR:
                        COMCHG:
              COMBILD:
                        COMPPM
        until over

;Orders are subtotalled by order date
        unpack comord,mm,b1,dd,b1,str2,yy
        call cvtjul
        clear ordjul
        move juldays to ordjul
        if ((juldays >= begyr) & (juldays <= endyr))  ;Do orders fall within the current year?
                 Match COMRE,"Exch"
                 IF equal
                    move COMEXCH to N10
                    add  N10 to tieryrcur
                    clear tier
                    move tieryrcur to str10
                    move str10 to n10
                    move mask13 to dim13a
                    edit n10 to dim13a
                    setitem NCAR0001StatText001,0,dim13a
                    goto readfile
                 else
          move COMRENT to N10
          add  N10 to tieryrcur
                 endif
;Don't need because it is calcluated at the end
;                 IF (tieryrcur < TIER2QTY)    ;<10 mil
;         move tier1 to tier
;                    move c1 to tierflag
;                 ElseIF ((tieryrcur >= TIER2QTY)&(tieryrcur < TIER3QTY)) ;10+ mil   < 15mil
;         move tier2 to tier
;                    move c2 to tierflag
;                 ElseIF ((tieryrcur >= TIER3QTY)&(tieryrcur < TIER4QTY)) ;15+ mil   < 20 mil
;                    move tier3 to tier
;                    move c3 to tierflag
;                 ElseIF (tieryrcur >= TIER4QTY) ;20+mil
;                    move tier4 to tier
;                    move c4 to tierflag
;                 Endif
;Masking Total
                 move tieryrcur to str10
                 move str10 to n10
                 move mask13 to dim13a
                 edit n10 to dim13a
                 setitem NCAR0001StatText001,0,dim13a
        endif
        if ((juldays >= prvbegyr) & (juldays <= prvendyr))      ;Or are they from the previous year?
                 Match COMRE,"Exch"
                 IF equal
                    move COMEXCH to N10
                    add  N10 to tieryrprev
                    clear tier 
                    move tieryrprev to str10
                    move str10 to n10
                    move mask13 to dim13a
                    edit n10 to dim13a
                    setitem NCAR0001StatText002,0,dim13a
                    goto readfile
                 else
          move COMRENT to N10
          add  N10 to tieryrprev
                 endif
;Don't need because it is calcluated at the end
;                 IF (tieryrprev < TIER2QTY)    ;<10 mil
;         move tier1 to tier
;                    move c1 to tierflag
;                 ElseIF ((tieryrprev >= TIER2QTY)&(tieryrcur < TIER3QTY)) ;10+ mil, but < 15mil
;         move tier2 to tier
;                    move c2 to tierflag
;                 ElseIF ((tieryrprev >= TIER3QTY)&(tieryrcur < TIER4QTY)) ;15 mil, but < 20 mil
;                    move tier3 to tier
;                    move c3 to tierflag
;                 ElseIF (tieryrprev >= TIER4QTY) ;20mil
;                    move tier4 to tier
;                    move c4 to tierflag
;                 Endif
                 move tieryrprev to str10
                 move str10 to n10
                 move mask13 to dim13a
                 edit n10 to dim13a
                 setitem NCAR0001StatText002,0,dim13a
;                 move tieryrprev to str13
;                 setitem NCAR0001StatText002,0,str13

        endif        
        goto     readfile if (ordjul <= START)             ;if order date is before jul 1st 2002
        goto     readfile if (COMCHG < "20")                         ;Order Must have 20% Commission
        move     c1 to nordpath
        move     comlr to nordfld
        call     zfillit using nordfld
        call     nordkey
        clear    n10
        move     oexqty to n10
        goto     readfile if (n10 > c0)                              ;Order must not be a split
;Check Which is later Campaign Maildate or Invoice Date
;.===================================================================================
;.Convert Campaign Mail Date to Julian Date
;.Convert Campaign Date to Julian Date
//Revision per DH 7/7/06.  We are not going to use the campaign mail date at all.  We are clearing mulitple mail dates 
//in one campaign which makes the single campaign mail date invalid.  We will only refer to the order's mail date when caluclating
//the due date
//DH no longer wants to use the invoice date since if the invoice date is too much later it means that the clients 
//vendor was not supplying the information needed in which case we should not be penalized nor should the client be given extra time to pay
//if not we are using the maildate - we should go back an revisit using the orig maildate when the
//prep file is created.
//        call trim using COMCDATE
//        if (comcdate = "")
          unpack COMMAIL,mm,b1,dd,b1,str2,yy      
//        else
//                  unpack COMCDATE,str2,yy,mm,dd 
//        endif
//Comment Out We update it above to check for no campaign date
//        unpack COMCDATE,str2,yy,mm,dd
//Comment Out
        call zfillit using mm
        call zfillit using DD
        call cvtjul
        move juldays to MAILDATE

//Convert Inv Date to Julian Date
        unpack cominv,mm,b1,dd,b1,str2,yy
        call trim using mm
//If there is no invoice date read next record
        if (mm = "/")
                move c0 to invdate
                goto readfile
        Endif
//Comment Out We are not longer using invoice date per DH
//reason:  Means that their vendor did not give us the info on time or the Gemmas were killed.          
//        call zfillit using mm
//        call zfillit using DD
//        call cvtjul
//        move juldays to INVDATE
//NOINV
//;Compare campaign date and Inv Dates to see which is later
//        compare MailDate to INVDATE

//;Add sixty days to pmt due date and then convert from julian to date
//        IF LESS
//Comment Out We are not longer using invoice date per DH        
                 move Maildate to calcdate
                 ADD  "60" to MAILDATE,PMTDUE
//Comment Out We are not longer using invoice date per DH                         
//        ELSE
//                 move Invdate to calcdate
//                 ADD  "60" to INVDATE,PMTDUE
//        ENDIF
//Comment Out We are not longer using invoice date per DH                
        move PMTDUE to JULDAYS
        call CVTGREG
        pack comdue,mm,slash,dd,slash,cc,yy
;If payment not received
        call trim using comrecd
        IF ((COMRECD = "1")|(COMRECD = "0"))

          MOVE NO to PAID
          MOVE NO to SIXTY
;                    move c0 to commish
;                    move commish to str15
        ELSE
;Payment was recieved
          MOVE YES to PAID
          unpack COMRECD,mm,b1,dd,b1,str2,yy
          call zfillit using mm
          call zfillit using DD
          call cvtjul
          move juldays to RECDDATE
;.subtract billing date(later of maildate or inv date) from pmt rec'd date
          SUB  CALCDATE FROM RECDDATE,N5
          IF (N5 <= 60)
;.less than sixty
;.qulified for commission
                    MOVE YES to SIXTY
                    else
                                        MOVE NO to SIXTY
                    endif
        ENDIF
readinv
        move       c1 to ninvpath
        move       comlr to NINVFLD
        rep        zfill in ninvfld
        call       ninvkey
        call       wipecvars
        MOVE       NO TO SHIPSW
        MOVE       NO TO MRGSW
        MOVE       c0 TO nmrgrqty
        move       c0 TO nmrgiqty
        MOVE       OLRN TO NMRGFLD
        REP        ZFILL IN NMRGFLD
        CALL       NMRGKEY                 ;merge needed for compute
        if         not over
                   move yes to mrgsw 
        endif      
        move       c0 to squant
        MOVE       olrn TO NSHPFLD
        CALL       NSHPKEY                 ;shipping needed for compute
        if         not over
                           move  yes to shipsw
        endif
        clear      LRINC

               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
;               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
        call       compute                 ;call compute
        move       LRINC to LRINCOM
        MOVE       OLRN TO NADJFLD
        CALL       NADJKEY                 ;any adjustments?
        if         not over
                   move ASLRINC to n12
                   add  n12 to LRINCOM
        else      
                   move c0 to n12
        endif
        move       lrincom to str12
;clculate commission
;        clear      dumvar92
;        mult       LRINCOM,tier,commish    ;commish =  lrincom(from compute) x tierpct 
;        move       commish to str15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;       clear n10
;       clear dumvar72
;       move  COMBILD to N10          ;combild billed qty used on invoice
;       div "1000",N10,dumvar72
;       mult COMPPM,dumvar72            ;.mult inv PPM * (qty/1000)
;       mult dumvar72,tier,commish
;       move commish to str10
;       ELSE
;.missed commission deadline
;                   MOVE "Missed" to PAID
;                   MOVE NO to SIXTY
;         ENDIF
;         ENDIF
;         Branch   QuarterLoop with FirstQuarterCalc,SecondQuarterCalc,ThirdQuarterCalc,FourthQuarterCalc
;Insert Information into listview object by quarter
FirstQuarterCalc
         if ((pmtdue >= begqtr1) & (pmtdue <= endqtr1))     ;1st Quarter
                  NCAR001aListView001.InsertItem giving n7 using O1DES
                  NCAR001aListView001.SetItemText  giving N8 using n7,COMLR,1
                  NCAR001aListView001.SetItemText  giving N8 using n7,COMORD,2
                  NCAR001aListView001.SetItemText  giving N8 using n7,COMRECD,3
                  NCAR001aListView001.SetItemText  giving N8 using n7,str12,4
                  NCAR001aListView001.SetItemText  giving N8 using n7,comdue,5
                  NCAR001aListView001.SetItemText  giving N8 using n7,COMMLR,6
                  move ordjul to str5
                  NCAR001aListView001.SetItemText  giving N8 using n7,str5,9
  
                  if (sixty = YES)
                           NCAR001aListView001.SetItemCheck giving n1 using n7,c1
;                           Add commish to qtrrefund1
                  else
                           if (PAID = NO) 
                                         NCAR001aListView001.SetItemText using n7,"0xFF0000",7                            ;sets font red
                                         move YES to disqualified1     
                           endif
                  endif
;                  add commish to PosQTRRefund1                   ;possible refund for quarter

         elseif  ((pmtdue >= begqtr2) & (pmtdue <= endqtr2))     ;2nd Quarter
                  NCAR001aListView002.InsertItem giving n7 using O1DES
                  NCAR001aListView002.SetItemText  giving N8 using n7,COMLR,1
                  NCAR001aListView002.SetItemText  giving N8 using n7,COMORD,2
                  NCAR001aListView002.SetItemText  giving N8 using n7,COMRECD,3
                  NCAR001aListView002.SetItemText  giving N8 using n7,str12,4
                  NCAR001aListView002.SetItemText  giving N8 using n7,comdue,5
                  NCAR001aListView002.SetItemText  giving N8 using n7,COMMLR,6
                  move ordjul to str5
                  NCAR001aListView002.SetItemText  giving N8 using n7,str5,9
                  if (sixty = YES)
                           NCAR001aListView002.SetItemCheck giving n1 using n7,c1
;                           Add commish to qtrrefund2
                  else
                           if (PAID = NO) 
                                         NCAR001aListView002.SetItemText using n7,"0xFF0000",7                            ;sets font red
                                         move YES to disqualified2
                           endif
                  endif
;                  add commish to PosQTRRefund2                   ;possible refund for quarter

         elseif  ((pmtdue >= begqtr3) & (pmtdue <= endqtr3))     ;3rd Quarter
                  NCAR001aListView003.InsertItem giving n7 using O1DES
                  NCAR001aListView003.SetItemText  giving N8 using n7,COMLR,1
                  NCAR001aListView003.SetItemText  giving N8 using n7,COMORD,2
                  NCAR001aListView003.SetItemText  giving N8 using n7,COMRECD,3
                  NCAR001aListView003.SetItemText  giving N8 using n7,str12,4
                  NCAR001aListView003.SetItemText  giving N8 using n7,comdue,5
                  NCAR001aListView003.SetItemText  giving N8 using n7,COMMLR,6
                  move ordjul to str5
                  NCAR001aListView003.SetItemText  giving N8 using n7,str5,9
                  if (sixty = YES)
;                           Add commish to qtrrefund3
                           NCAR001aListView003.SetItemCheck giving n1 using n7,c1
                  else
                           if (PAID = NO) 
                                         NCAR001aListView003.SetItemText using n7,"0xFF0000",7                            ;sets font red
                                         move YES to disqualified3
                           endif
                  endif
;                  add commish to PosQTRRefund3                   ;possible refund for quarter

         elseif  ((pmtdue >= begqtr4) & (pmtdue <= endqtr4))     ;4th Quarter
                  NCAR001aListView004.InsertItem giving n7 using O1DES
                  NCAR001aListView004.SetItemText  giving N8 using n7,COMLR,1
                  NCAR001aListView004.SetItemText  giving N8 using n7,COMORD,2
                  NCAR001aListView004.SetItemText  giving N8 using n7,COMRECD,3
                  NCAR001aListView004.SetItemText  giving N8 using n7,str12,4
                  NCAR001aListView004.SetItemText  giving N8 using n7,comdue,5
                  NCAR001aListView004.SetItemText  giving N8 using n7,COMMLR,6
                  move ordjul to str5
                  NCAR001aListView004.SetItemText  giving N8 using n7,str5,9
                  if (sixty = YES)
                           NCAR001aListView004.SetItemCheck giving n1 using n7,c1
;                           Add commish to qtrrefund4
                  else
                           if (PAID = NO) 
                                         NCAR001aListView004.SetItemText using n7,"0xFF0000",7                            ;sets font red
                                         move YES to disqualified4
                           endif
                  endif
;                  add commish to PosQTRRefund4          ;possible refund for quarter
         else
                  goto readfile
         endif
      repeat
;Sort the listview objects
;      NCAR001aListView001.SortColumn using *Column=6,*Type=1
;      NCAR001aListView002.SortColumn using *Column=6,*Type=1
;      NCAR001aListView003.SortColumn using *Column=6,*Type=1
;      NCAR001aListView004.SortColumn using *Column=6,*Type=1
;Check if a record was not paid at all for each quarter which would disqualify any payment for that quarter
;Refund By Quarter
CommissionQuarter1
        NCAR001aListView001.getitemcount giving n9
        sub c1 from n9
        for n8 from "0" to n9       
;calculation of refund
                    clear ordjul
                    NCAR001aListView001.getitemtext giving str5 using n8,9
                    move str5 to ordjul                                ;julian order date
                    NCAR001aListView001.getitemtext giving str12 using n8,4
                    move str12 to lrincom                    ;lrincom
                    call tiercalculation                                         ;must have vars lrincom and ordjul to calculate properly
                    NCAR001aListView001.SetItemText  using n8,str12,8            ;str12 is commission returned
.Patch 1.6                    
                    NCAR001aListView001.getitemcheck giving n1 using n8
                    if (n1 = c1)                    
                              add commish to qtrrefund1
                    endif
.Patch 1.6                    
                    add commish to posqtrrefund1
;not paid?
                   clear str16
                   move qtrrefund1 to str16
                   if (disqualified1 = YES)                   ;if a record was not paid for the quarter zero quarterrefudn, uncheck all items in listview and type disqualified in edittext box
                              move c0 to qtrrefund1                                  
                              move "disqualified" to str16
                              NCAR001aListView001.setitemcheck giving n1 using n8,c0
                   endif
                   setitem ncar001aEditText002,0,str16   
       repeat
CommissionQuarter2
        NCAR001aListView002.getitemcount giving n9
        sub c1 from n9
        for n8 from "0" to n9       
;calculation of refund
                    clear ordjul
                    NCAR001aListView002.getitemtext giving str5 using n8,9
                    move str5 to ordjul
                    NCAR001aListView002.getitemtext giving str12 using n8,4
                    move str12 to lrincom
                    call tiercalculation                                             ;must have vars lrincom and ordjul to calculate properly
                    NCAR001aListView002.SetItemText using n8,str12,8      ;str12 is commission returned

.Patch 1.6                    
                    NCAR001aListView002.getitemcheck giving n1 using n8
                    if (n1 = c1)                    
                              add commish to qtrrefund2                         
                        endif                   
.Patch 1.6                    

                    add commish to posqtrrefund2
;not paid?
                   clear str16
                   move qtrrefund2 to str16
;if any quarter for the given  year does not have a paid invoice disqualify all subsequent refund
                   if ((disqualified1 = YES)|(disqualified2 = YES))                   ;if a record was not paid for the quarter zero quarterrefudn, uncheck all items in listview and type disqualified in edittext box
                              move c0 to qtrrefund2                                  
                              move "disqualified" to str16
                              NCAR001aListView002.setitemcheck giving n1 using n8,c0
                   endif
                   setitem ncar001aEditText003,0,str16   
       repeat
CommissionQuarter3
        NCAR001aListView003.getitemcount giving n9
        sub c1 from n9
        for n8 from "0" to n9       
;calculation of refund
                    clear ordjul
                    NCAR001aListView003.getitemtext giving str5 using n8,9
                    move str5 to ordjul
                    NCAR001aListView003.getitemtext giving str12 using n8,4
                    move str12 to lrincom
                    call tiercalculation                                             ;must have vars lrincom and ordjul to calculate properly
                    NCAR001aListView003.SetItemText using n8,str12,8                 ;str12 is commission returned
.Patch 1.6                    
                    NCAR001aListView003.getitemcheck giving n1 using n8
                    if (n1 = c1)                    
                              add commish to qtrrefund3                         
                        endif                   
.Patch 1.6                        
;                    add commish to qtrrefund3
                    add commish to posqtrrefund3
;not paid?
                   clear str16
                   move qtrrefund3 to str16
;if any quarter for the given  year does not have a paid invoice disqualify all subsequent refund
                   if ((disqualified1 = YES)|(disqualified2 = YES)|(disqualified3 = YES))                                          ;if a record was not paid for the quarter zero quarterrefudn, uncheck all items in listview and type disqualified in edittext box
                              move c0 to qtrrefund3                                  
                              move "disqualified" to str16
                              NCAR001aListView003.setitemcheck giving n1 using n8,c0
                   endif
                   setitem ncar001aEditText004,0,str16   
       repeat       
CommissionQuarter4
        NCAR001aListView004.getitemcount giving n9
        sub c1 from n9    
        for n8 from "0" to n9
;calculation of refund
                    clear ordjul
                    NCAR001aListView004.getitemtext giving str5 using n8,9
                    move str5 to ordjul
                    NCAR001aListView004.getitemtext giving str12 using n8,4
                    move str12 to lrincom
                    call tiercalculation                                             ;must have vars lrincom and ordjul to calculate properly
                    NCAR001aListView004.SetItemText using n8,str12,8                 ;str12 is commission returned
.Patch 1.6                    
                    NCAR001aListView004.getitemcheck giving n1 using n8
                    if (n1 = c1)                    
                              add commish to qtrrefund4                         
                        endif                   
.Patch 1.6                        
;                    add commish to qtrrefund4
                    add commish to posqtrrefund4
;not paid?
                   clear str16
                   move qtrrefund4 to str16
;if any quarter for the given  year does not have a paid invoice disqualify all subsequent refund
                   if ((disqualified1 = YES)|(disqualified2 = YES)|(disqualified3 = YES)|(disqualified4 = YES))  ;if a record was not paid for the quarter zero quarterrefudn, uncheck all items in listview and type disqualified in edittext box
                              move c0 to qtrrefund4                                  
                              move "disqualified" to str16
                              NCAR001aListView004.setitemcheck giving n1 using n8,c0
                   endif
                   setitem ncar001aEditText005,0,str16   
       repeat

;possible refund By Quarter
      clear str16
      move posqtrrefund1 to str16
      setitem ncar001aEditText001,0,str16             ;1st Quarter
      clear str16
      move posqtrrefund2 to str16
      setitem ncar001aEditText006,0,str16             ;2nd Quarter
      clear str16
      move posqtrrefund3 to str16
      setitem ncar001aEditText007,0,str16             ;3rd Quarter
      clear str16
      move posqtrrefund4 to str16
      setitem ncar001aEditText008,0,str16             ;4th Quarter
      call yeartotal      

 loop
 waitevent 
 repeat

Columns
        NCAR001aListView001.deleteallcontents
        NCAR001aListView001.InsertColumn using "List",18,0
        NCAR001aListView001.InsertColumn using "LR",60,1
        NCAR001aListView001.InsertColumn using "Order Date",70,2
        NCAR001aListView001.InsertColumn using "Payment Rec",70,3
        NCAR001aListView001.InsertColumn using "Lr Income",0,4
        NCAR001aListView001.InsertColumn using "Due Date",70,5
        NCAR001aListView001.InsertColumn using "Mailer Name",0,6
        NCAR001aListView001.InsertColumnFgClr USING *Index=7
        NCAR001aListView001.InsertColumn using "Amt Due",70,8
        NCAR001aListView001.InsertColumn using "Ord Jul Date",0,9
;        NCAR001aListView001.InsertColumn using "Percent",60,7
        return
Columns2
        NCAR001aListView002.deleteallcontents
        NCAR001aListView002.InsertColumn using "List",18,0
        NCAR001aListView002.InsertColumn using "LR",60,1
        NCAR001aListView002.InsertColumn using "Order Date",70,2
        NCAR001aListView002.InsertColumn using "Payment Rec",70,3
        NCAR001aListView002.InsertColumn using "Lr Income",0,4
        NCAR001aListView002.InsertColumn using "Due Date",70,5
        NCAR001aListView002.InsertColumn using "Mailer Name",0,6
        NCAR001aListView002.InsertColumnFgClr  USING *Index=7
        NCAR001aListView002.InsertColumn using "Amt Due",70,8
        NCAR001aListView002.InsertColumn using "Ord Jul Date",0,9
        return
Columns3
        NCAR001aListView003.deleteallcontents
        NCAR001aListView003.InsertColumn using "List",18,0
        NCAR001aListView003.InsertColumn using "LR",60,1
        NCAR001aListView003.InsertColumn using "Order Date",70,2
        NCAR001aListView003.InsertColumn using "Payment Rec",70,3
        NCAR001aListView003.InsertColumn using "Lr Income",0,4
        NCAR001aListView003.InsertColumn using "Due Date",70,5
        NCAR001aListView003.InsertColumn using "Mailer Name",0,6
        NCAR001aListView003.InsertColumnFgClr  USING *Index=7
        NCAR001aListView003.InsertColumn using "Amt Due",70,8
        NCAR001aListView003.InsertColumn using "Ord Jul Date",0,9
        return
Columns4
        NCAR001aListView004.deleteallcontents
        NCAR001aListView004.InsertColumn using "List",18,0
        NCAR001aListView004.InsertColumn using "LR",60,1
        NCAR001aListView004.InsertColumn using "Order Date",70,2
        NCAR001aListView004.InsertColumn using "Payment Rec Due",70,3
        NCAR001aListView004.InsertColumn using "Lr Income",0,4
        NCAR001aListView004.InsertColumn using "Due Date",70,5
        NCAR001aListView004.InsertColumn using "Mailer Name",0,6
        NCAR001aListView004.InsertColumnFgClr  giving n4 uSING *Index=7
        NCAR001aListView004.InsertColumn using "Amt Due",70,8
        NCAR001aListView004.InsertColumn using "Ord Jul Date",0,9
        return
NCAR001bColumns
        NCAR001bListView001.InsertColumn using "LR",100,1
        NCAR001bListView001.InsertColumn using "Amount",100,2
        NCAR001bListView001.InsertColumn using "Check ##",100,3
        NCAR001bListView001.InsertColumn using "Check Date",100,4
        NCAR001bListView001.InsertColumn using "REC",0,5
ListviewRecount1
        NCAR001aListView001.getitemcount giving n9
        clear n8
        sub c1 from n9
        clear qtrrefund1
        clear dumvar92
        clear str16
        for n8 from "0" to n9       
                            NCAR001aListView001.getitemcheck giving n1 using n8
                            if (n1 = c1)
                                 NCAR001aListView001.getitemtext giving str13 using n8,8
                                 move str13 to dumvar92
                                 add dumvar92 to qtrrefund1
                            endif
        repeat
        move    qtrrefund1 to str16
        setitem ncar001aEditText002,0,str16             ;1st Quarter
        call YearTotal
        return
ListviewRecount2
        NCAR001aListView002.getitemcount giving n9
        sub c1 from n9
        clear qtrrefund2
        clear dumvar92
        clear str16
        for n8 from "0" to n9       
                            NCAR001aListView002.getitemcheck giving n1 using n8
                            if (n1 = c1)
                                 NCAR001aListView002.getitemtext giving str13 using n8,8
                                 move str13 to dumvar92
                                 add dumvar92 to qtrrefund2
                            endif
        repeat
        move    qtrrefund2 to str16
        setitem ncar001aEditText003,0,str16             ;2nd Quarter
        call YearTotal
        return
ListviewRecount3
        NCAR001aListView003.getitemcount giving n9
        sub c1 from n9
        clear qtrrefund3
        clear dumvar92
        clear str16
        for n8 from "0" to n9       
                            NCAR001aListView003.getitemcheck giving n1 using n8
                            if (n1 = c1)
                                 NCAR001aListView003.getitemtext giving str13 using n8,8
                                 move str13 to dumvar92
                                 add dumvar92 to qtrrefund3
                            endif
        repeat
        move    qtrrefund3 to str16
        setitem ncar001aEditText004,0,str16             ;3rd Quarter
        call YearTotal
        return
ListviewRecount4
        NCAR001aListView004.getitemcount giving n9
        clear n8
        sub c1 from n9
        clear qtrrefund4
        clear dumvar92
        clear str16
        for n8 from "0" to n9       
                            NCAR001aListView004.getitemcheck giving n1 using n8
                            if (n1 = c1)
                                 NCAR001aListView004.getitemtext giving str13 using n8,8
                                 move str13 to dumvar92
                                 add dumvar92 to qtrrefund4
                            endif
        repeat
        move    qtrrefund4 to str16
        setitem ncar001aEditText005,0,str16             ;4th Quarter
        call YearTotal
        return
YearTotal
      clear YearRefund
      add qtrrefund1 to YearRefund
      add qtrrefund2 to YearRefund
      add qtrrefund3 to YearRefund
      add qtrrefund4 to YearRefund
      move YearRefund to str16
      setitem ncar001aEditText010,0,str16             ;Year
      return

TierCalculation
;Figure out payment on each order
        clear tier
.begin patch 1.71
.DH testing move lowest tier then check - don't know why the clear does not generate an error
          MOve      Tier1,Tier
.DH end   
.end patch 1.71
        if ((ordjul >= begyr) & (ordjul <= endyr))  ;Do orders fall within the current year?
                 IF (tieryrcur < TIER2QTY)    ;<10 mil
          move tier1 to tier
;                    move c1 to tierflag
                 ElseIF ((tieryrcur >= TIER2QTY)&(tieryrcur < TIER3QTY)) ;10+ mil   < 15mil
          move tier2 to tier
;                    move c2 to tierflag
                 ElseIF ((tieryrcur >= TIER3QTY)&(tieryrcur < TIER4QTY)) ;15+ mil   < 20 mil
                    move tier3 to tier
;                    move c3 to tierflag
                 ElseIF (tieryrcur >= TIER4QTY) ;20+mil
                    move tier4 to tier
;                    move c4 to tierflag
                 Endif
        endif
        if ((ordjul >= prvbegyr) & (ordjul <= prvendyr))      ;Or are they from the previous year?
                 IF (tieryrprev < TIER2QTY)    ;<10 mil
          move tier1 to tier
;                    move c1 to tierflag
                 ElseIF ((tieryrprev >= TIER2QTY)&(tieryrcur < TIER3QTY)) ;10+ mil, but < 15mil
          move tier2 to tier
;                    move c2 to tierflag
                 ElseIF ((tieryrprev >= TIER3QTY)&(tieryrcur < TIER4QTY)) ;15 mil, but < 20 mil
                    move tier3 to tier
;                    move c3 to tierflag
                 ElseIF (tieryrprev >= TIER4QTY) ;20mil
                    move tier4 to tier
;                    move c4 to tierflag
                 Endif
        endif
;calculate commission
        clear      dumvar92
        mult       LRINCOM,tier,commish    ;commish =  lrincom(from compute) x tierpct 
        move       commish to str12
 return
;Print Prepare 
PrintReport
        clear Qtrrefund1
        clear Qtrrefund2
        clear Qtrrefund3
        clear Qtrrefund4
        PACK  PRTITLE,"carecom"
.Patch 1.4
        PACK  PRTNAME1,PRTITLE,".LST"
        PACK  PRTFILE1,PRTDIR,PRTNAME1
.Patch 1.4
          pack      str55 from "c:\work\pdf\",PRTITLE,".PDF"
          PrtOPen   PComFIle,"PDF:",str55,Flags=PDF_FLAGS_WIN_ANSI_ENCODING
.        PRTOPEN   PCOMFILE,"\\NINs2\Laser6",PRTITLE,noprint,spoolfile=PRTFILE1
;patch1.1
        CLEAR     ROWCOUNT
        Move       C1 TO PGCNT
        prtpage   PCOMFILE; *ORIENT=*LANDSCAPE:
                  *UNITS=*HIENGLISH;
;patch1.35
;don't remove had to fudge to get text to work properly on first page.
        prtpage pcomfile;*p0:0," ";
        call begnewpage
;patch1.1
        move c0 to quarterflg 
        add  c1 to quarterflg
QuarterLoop
        perform quarterflg of GetCount1,GetCount2,GetCount3,GetCount4
;        NCAR001aListView001.GetItemCount giving CRECORDS
        sub c1 from CRECORDS
        Clear N9
        MOVE c0 to N9
        loop
        until (N9 > CRECORDS)
                     perform quarterflg of getclient1,getclient2,getclient3,getclient4
;                    NCAR001aListView001.GetItemText giving str45 using n9,6
                    if (n9 = c0)
                                 move str45 to HOLDCLIENT
                    else
                                 match HOLDCLIENT to STR45
                                 move  str45 to HOLDCLIENT
                                 if not equal
                                       goto subtotal
                                 endif
                    Endif
Record
                    add c1 to rowcount
                    perform quarterflg of getvars1,getvars2,getvars3,getvars4                    
                    if (rowcount = c1)
                                prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font7,*boldon,*ll,str45;
                                add eightlpi to row
                                prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font8,*ll,*ULON,*boldon,"List Name";
                                add eightlpi to row
                                add "30",row
                                add c2 to rowcount
                    endif
;                    NCAR001aListView001.GetItemText giving str35 using n9,0                            ;ListName
                    prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font13,*ll,str35;
;                    NCAR001aListView001.GetItemText giving str6 using n9,1                             ;LR
                    prtpage PCOMFILE;*pcolumn2:row,*ALIGNMENT=*CENTER,*font=font13,*ll,str6;
;                    NCAR001aListView001.GetItemText giving str10 using n9,5                 ;Payment Due Date
                    prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font13,*ll,str10;          
;Payment Rec'd Date
                    perform quarterflg of getpayrec1,getpayrec2,getpayrec3,getpayrec4
;                    NCAR001aListView001.GetItemText giving str10 using n9,3                            ;Payment Received Date
                    move str10 to str11
                    call trim using str11
                    IF ((str11 = "1")|(str11 = "0"))
                             move "Open" to str10
                    Endif
                    prtpage PCOMFILE;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=font13,*ll,str10;
;Refund Section
                     perform quarterflg of getcheck1,getcheck2,getcheck3,getcheck4   
;                    NCAR001aListView001.getitemcheck giving n1 using n9                                 ;Is record to be refunded
                    if (n1 = c1)
                               perform quarterflg of getrefund1,getrefund2,getrefund3,getrefund4
;                              NCAR001aListView001.getitemtext giving str13 using n9,4                   ;Refund Amt
                              move str13 to dumvar92
                              if (quarterflg = c1)
                                                  add dumvar92 to qtrRefund1
                              elseif (quarterflg = c2)
                                                  add dumvar92 to qtrRefund2                                                  
                              elseif (quarterflg = c3)
                                                  add dumvar92 to qtrRefund3                                                  
                              elseif (quarterflg = c4)
                                                  add dumvar92 to qtrRefund4
                              endif
                    else
                              move "       .00" to str13 
                    endif
;.Moved to be right justified
;                     prtpage PCOMFILE;*p9300:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;
                     prtpage PCOMFILE;*p8100:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;
                     add c1 to n9
                     add     eightlpi,row
                     add     "50",row
                     call Endpage
       repeat
;;DBADD
        if (crecords = seq)  ;no records for quarter
;Hard Coded for first run
                       prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font7,*boldon,*ll,"CARE, Inc.";
                       add eightlpi to row
                       prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font8,*ll,*ULON,*boldon,"List Name";
                       add eightlpi to row
                       add "30",row
                       add c2 to rowcount

                       Load str11 with quarterflg,"1st Quarter  ","2nd Quarter  ","3rd Quarter  ","4th Quarter  "
                       pack str55 with "No Qualifying Records for CARE in the ",str11
                       add eightlpi to row
                       add "30",row
                       prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font9,*boldon,str55;
;                       prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font7,*boldon,"No Qualifying Records for CARE in the ";
;                       prtpage PCOMFILE;*font=font7,*boldon,*ll,str11;
                       add c1 to rowcount
                       goto stillgoing
        endif
;;
       sub     "50",row
;       prtpage PCOMFILE;*p7700:row,*pensize=10,*line=8320:row;
       prtpage PCOMFILE;*p7300:row,*pensize=10,*line=8100:row;
;       prtpage PCOMFILE;*p7600:row,*pensize=10,*line=8200:row;
       add     "50",row
       add c1 to rowcount
       prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font9,*BOLDON,*ll,"Subtotal";
;       move POSQTREFUND to str13
;.Moved to be right justified
;       move qtrRefund1 to str13
       move mask15 to dim15a

       if (quarterflg = c1)
                              edit QTRREFUND1 to dim15a
       elseif (quarterflg = c2)
                              edit QTRREFUND2 to dim15a
       elseif (quarterflg = c3)
                              edit QTRREFUND3 to dim15a
       elseif (quarterflg = c4)
                              edit QTRREFUND4 to dim15a
       endif
;       edit QTRREFUND1 to dim15a
;       call FormatNumeric using str13,str16,comma       
;       prtpage PCOMFILE;*p9300:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,dim15a;
       prtpage PCOMFILE;*p8100:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,dim15a;
StillGoing
       add  eightlpi,row
       add  "50",row
       call ENDPAGE
       add  c1 to quarterflg
       goto close if (quarterflg = c5)
       Load str11 with quarterflg,"1st Quarter  ","2nd Quarter  ","3rd Quarter  ","4th Quarter  "
       prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,str11;
       add eightlpi to row
       add eightlpi to row
       goto quarterloop
Close
       call lastpage
;       call endpage
       PRTCLOSE   PCOMFILE
       goto pdfthis
;       PRTPLAY PRTFILE1,"\\NINs2\Laser6"
       return
SUBTOTAL
;;       sub     "50",row
;       prtpage PCOMFILE;*p7700:row,*pensize=10,*line=8320:row;
;;       prtpage PCOMFILE;*p7300:row,*pensize=10,*line=8100:row;
;;       add     "50",row
;;       add c1 to rowcount
;;       prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font9,*BOLDON,*ll,"Subtotal";
;;       move mask15 to dim15a
;;       edit QTRREFUND1 to dim15a
;;       prtpage PCOMFILE;*p7600:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,dim15a;
;;       prtpage PCOMFILE;*p7600:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,str13;
;;       add     eightlpi,row
;;       add     "50",row
;;       prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font7,*boldon,*ll,str45;
;;       add eightlpi to row
;;       prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font8,*ll,*boldon,*ulon,"List Name";
;;       add     eightlpi,row
;;       add     "30",row
;;       add c2 to rowcount
;;       call ENDPAGE
;;       goto RECORD
EndPage

        if (row > "7040")
;       if (ROWCOUNT = "33")
;.Added to correct page # and page label for next to last page
;.==========================================================================================
                 move "7750",row
                 prtpage PCOMFILE;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage PCOMFILE;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.2 ADDED LOGIC
.                prtpage PCOMFILE;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
                    prtpage   PCOMFILE;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.2 ADDED LOGIC
;.===========================================================================================
                 CALL  newpage
       endif
       return

NEWPAGE
        CLEAR     ROWCOUNT
        ADD       C1 TO PGCNT
        prtpage   PCOMFILE;*NEWPAGE:
                               *ORIENT=*LANDSCAPE:
                   *UNITS=*HIENGLISH;
BegNewPage
        clear     row
        move      "300",row
        prtpage PCOMFILE;*pTitle2:row,*ALIGNMENT=*Left,*ll,*font=font12,"Date: ";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage PCOMFILE;*font=font12,*ll,str10;
        prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,"CARE / The Domain Group Commission Detail";
        add     eightlpi,row
        add     "40",row
;        pack    str17,"Fiscal Year ",curyr
        getitem ncar001aStatText007,0,str24
;        call trim using str24
        getitem ncar001aStatText008,0,str25
;        call trim using str25
        pack taskname with str24,b1,dash,b1,str25

        prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,*ll,taskname;
        add     eightlpi,row
        add     "40",row
        if (quarterflg < c1)
                             move c1 to quarterflg          ;for first Record Only
.Patch 1.5                             
                                       Load    str11 with quarterflg,"1st Quarter  ","2nd Quarter  ","3rd Quarter  ","4th Quarter  ","4th Quarter  "
;                                       Load    str11 with quarterflg,"1st Quarter  ","2nd Quarter  ","3rd Quarter  ","4th Quarter  "
.Patch1.5                                       
                                       prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,str11;
                                       clear quarterflg
        else
                                       Load    str11 with quarterflg,"1st Quarter  ","2nd Quarter  ","3rd Quarter  ","4th Quarter  "
                                       prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,str11;
        endif
;        Load    str11 with quarterflg,"1st Quarter  ","2nd Quarter  ","3rd Quarter  ","4th Quarter  "
;        prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,str11;
        add     eightlpi,row
        add     "60",row
;        prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Billing";
        prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Pymt Due";
        prtpage PCOMFILE;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Payment";
        prtpage PCOMFILE;*pcolumn7:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Refund";
        add     eightlpi,row
        add     "30",row
        prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Client";
        prtpage PCOMFILE;*pcolumn2:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"LR ";
;        prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Date*";
        prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"For Comm.";
        prtpage PCOMFILE;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Rec'd**";
;        prtpage PCOMFILE;*pcolumn6:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Quantity";
        prtpage PCOMFILE;*pcolumn7:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Amount";
        add     eightlpi,row
        add     "50",row
        return

LastPage
;;        sub     "50",row
;;        prtpage PCOMFILE;*p7300:row,*pensize=10,*line=8100:row;
        add     "50",row
;;        add c1 to rowcount
        prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font12,*BOLDON,*ll,"Total";
        move mask15 to dim15a
        edit YEARREFUND to dim15a
        prtpage PCOMFILE;*p8100:row,*ALIGNMENT=*RIGHT,*font=font12,*BOLDON,*ll,dim15a;
        add  eightlpi,row
        add  "50",row
        move tieryrcur to n10
        move mask13 to dim13a
        edit n10 to dim13a
        pack taskname with "Total Orders for Fiscal Year ",curyr,b2,colon,b1,dim13a
.Patch 1.5        
.        if (row < "6855")        
        if (row < "7300")
.Patch 1.5                
                  prtpage PCOMFILE;*pcolumn:row,*font=font9,*ALIGNMENT=*Left,taskname;               
;                 prtpage PCOMFILE;*pcolumn:row,*font=font9,*ALIGNMENT=*Left,"Total Orders for Fiscal Year ";
;                 prtpage PCOMFILE;*font=font9,curyr#:;
;                 prtpage PCOMFILE;*font=font9,"  ";
;                 prtpage PCOMFILE;*font=font9,tieryrcur;

;                call lastpage2
.==========================================================================================
                  move "7750",row
                  prtpage PCOMFILE;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                  prtpage PCOMFILE;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.2 ADDED LOGIC
.                  prtpage PCOMFILE;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
                    prtpage   PCOMFILE;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.2 ADDED LOGIC
.===========================================================================================
        else
.Added to correct page # and page label for next to last page
.==========================================================================================
                  move "7750",row
                  prtpage PCOMFILE;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                  prtpage PCOMFILE;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.2 ADDED LOGIC
.                  prtpage PCOMFILE;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
                    prtpage   PCOMFILE;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.2 ADDED LOGIC
.===========================================================================================
                  CALL  newpage
                      pack taskname with "Total Orders for Fiscal Year ",curyr,b2,colon,b1,dim13a                  
                  prtpage PCOMFILE;*pcolumn:row,*font=font9,*ALIGNMENT=*Left,taskname;               
;               prtpage PCOMFILE;*pcolumn:row,*font=font9,*ALIGNMENT=*Left,"Total Orders for Fiscal Year ";
;               prtpage PCOMFILE;*font=font9,curyr;
;               prtpage PCOMFILE;*font=font9,b2;
;               prtpage PCOMFILE;*font=font9,tieryrcur;
;               call  lastpage2
       endif
       return
GetCount1
        NCAR001aListView001.GetItemCount giving CRECORDS
        return
GetCount2
        NCAR001aListView002.GetItemCount giving CRECORDS
        return
GetCount3
        NCAR001aListView003.GetItemCount giving CRECORDS
        return
GetCount4
        NCAR001aListView004.GetItemCount giving CRECORDS
        return
GetClient1
        NCAR001aListView001.GetItemText giving str45 using n9,6
        return
GetClient2
        NCAR001aListView002.GetItemText giving str45 using n9,6
        return
GetClient3
        NCAR001aListView003.GetItemText giving str45 using n9,6
        return
GetClient4
        NCAR001aListView004.GetItemText giving str45 using n9,6
        return

GetVars1
        NCAR001aListView001.GetItemText giving str35 using n9,0   
        NCAR001aListView001.GetItemText giving str6 using n9,1                   ;LR
        NCAR001aListView001.GetItemText giving str10 using n9,5                  ;Payment Due Date
        return
GetVars2
        NCAR001aListView002.GetItemText giving str35 using n9,0   
        NCAR001aListView002.GetItemText giving str6 using n9,1                   ;LR
        NCAR001aListView002.GetItemText giving str10 using n9,5                  ;Payment Due Date
        return
GetVars3
        NCAR001aListView003.GetItemText giving str35 using n9,0   
        NCAR001aListView003.GetItemText giving str6 using n9,1                   ;LR
        NCAR001aListView003.GetItemText giving str10 using n9,5                  ;Payment Due Date
        return
GetVars4
        NCAR001aListView004.GetItemText giving str35 using n9,0   
        NCAR001aListView004.GetItemText giving str6 using n9,1                   ;LR
        NCAR001aListView004.GetItemText giving str10 using n9,5                  ;Payment Due Date
        return
GetPayRec1
        NCAR001aListView001.GetItemText giving str10 using n9,3                  ;Payment Received Date
        return
GetPayRec2
        NCAR001aListView002.GetItemText giving str10 using n9,3                  ;Payment Received Date
        return
GetPayRec3
        NCAR001aListView003.GetItemText giving str10 using n9,3                  ;Payment Received Date
        return
GetPayRec4
        NCAR001aListView004.GetItemText giving str10 using n9,3                  ;Payment Received Date
        return
GetCheck1
        NCAR001aListView001.getitemcheck giving n1 using n9                      ;Is record to be refunded
        return
GetCheck2
        NCAR001aListView002.getitemcheck giving n1 using n9                      ;Is record to be refunded
        return
GetCheck3
        NCAR001aListView003.getitemcheck giving n1 using n9                      ;Is record to be refunded
        return
GetCheck4
        NCAR001aListView004.getitemcheck giving n1 using n9                      ;Is record to be refunded
        return
GetRefund1
        NCAR001aListView001.getitemtext giving str13 using n9,8                  ;Refund Amt
        return
GetRefund2
        NCAR001aListView002.getitemtext giving str13 using n9,8                  ;Refund Amt
        return
GetRefund3
        NCAR001aListView003.getitemtext giving str13 using n9,8                  ;Refund Amt
        return
GetRefund4
        NCAR001aListView004.getitemtext giving str13 using n9,8                  ;Refund Amt
        return
.......................................................................................................
PayBox
.............MESSAGE BOXES.............
;Following are dynamically created Message Boxes using Report2, which only contains a couple of objects.
;.Each time a new Message box is created:  1)the Objects from the previous incarnation must be destroyed
;. 2)the Form must receive a new Title  3)the Objects must be dumped into the ObjectColl collection in order
;.to facilitate easy destruction.
;.Each Message Box may have several associated sub-routines, triggered by object events, ie Lost_Focus,
;.Click, etc.
;.Keep all dynamic use of Report 2 in this section in order to better monitor its use.  ASH
;.......................................
;.Allows selection of different reports for Exchange Summary Reports
.
        call    Report2DestroyObjects
        setprop Report2,title="Commission Payment Screen"
        create  Report2;StatTextBoxes(1)=30:50:10:110,"Check Date","'>MS Sans Serif'(8)",ToolTip="8-digit field"
        create  Report2;StatTextBoxes(2)=50:70:10:110,"Check Number","'>MS Sans Serif'(8)"
        create  Report2;StatTextBoxes(3)=70:90:10:110,"Check Amt.","'>MS Sans Serif'(8)"
;;        create  Report2;StatTextBoxes(3)=70:90:10:110,"Copies","'>MS Sans Serif'(8)"
        create  Report2;EditTextBoxes(1)=30:50:90:200,MaxChars=8,EditType=2,SelectAll=1,Style=1,Border=1
        create  Report2;EditTextBoxes(2)=50:70:90:200,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
        create  Report2;EditTextBoxes(3)=70:90:90:200,MaxChars=12,EditType=3,SelectAll=1,Style=1,Border=1

        create  Report2;Buttons(1)=205:230:50:100,"&OK"
        move     NO,str1
        activate StatTextBoxes(1)
        activate StatTextBoxes(2)
        activate StatTextBoxes(3)
;        setitem  EditTextBoxes(1),1,"001"          .default copies
        activate EditTextBoxes(1)
        activate EditTextBoxes(2)
        activate EditTextBoxes(3)
        activate Buttons(1),Verify,result
;Patch7.21
;;        listins  ObjectColl,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3),EditTextBoxes(1),EditTextBoxes(2),EditTextBoxes(3)               
;Patch7.21
         setfocus EditTextBoxes(1)
         setprop  report2,visible=1
TESTER

         RETURN
;.if rptcan=no  then verify User has ok'd lets go and do job

Verify
         return If (rptcan = YES)
             
         call     OrderSetMouseBusy
;Check Date
         getitem EditTextBoxes(1),0,str10
         call    RemoveChar using str10,slash
         call    TRIM using str10
         MOVE    STR10 TO STR8
         if      (str10="")
              alert    caution,"Date Cannot be a null value!",result,"Bad Date"
              setfocus EditTextBoxes(1)
;             CLEAREVENT
              call ordersetmousefree
              return
         endif
         CALL       ZFILLIT USING STR8
         if      (str8="00000000")
                     alert   caution,"Date Cannot be a null value!",result,"Bad Date"
                     setfocus EditTextBoxes(1)
;                     CLEAREVENT
                     call ordersetmousefree
                     return
         endif
;=======================================================================
         count   N2,str10
         if (N2 = 0)
                   clear   MM
                   clear   DD
                   clear   CCField
                   clear   YY
                   alert   caution,"Date Must be in MMDDCCYY Format",result
                   setfocus EditTextBoxes(1)
;                   ClearEvent
                   call ordersetmousefree
                   Return
         else
                   if (N2 = 10)
                           unpack  str10,MM,str1,DD,str1,CCField,YY
                   elseif (N2 = 8)
                           unpack  str10,MM,DD,CCField,YY
                   elseif (N2 <> 0)
                           alert   caution,"Date Must be in MMDDCCYY Format",result
                           setfocus EditTextBoxes(1)
;                           ClearEvent
                           call ordersetmousefree
                           Return
                   endif
                   move    MM,N2
                   if (N2 > "12")
                          alert   caution,"Invalid Month!",result
                          setfocus EditTextBoxes(1)
;                          ClearEvent
                          call ordersetmousefree
                          Return
                   else
                           move    DD,N2
                           if (N2 > "31")
                                           alert   caution,"Invalid Day!",result
                                           setfocus EditTextBoxes(1)
;                                           ClearEvent
                                           call ordersetmousefree
;                                           Return
                           else
                                   move    CCField,N2
                                   if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                           alert   caution,"Invalid Year!",result
                                           setfocus EditTextBoxes(1)
;                                           ClearEvent
                                           call ordersetmousefree
                                           Return
                                   elseif (N2 = "19")
                                           move    YY,N2
                                           if (N2 < "80")
                                                   alert   caution,"Invalid Year!",result
                                                   setfocus EditTextBoxes(1)
;                                                   ClearEvent
                                                   call ordersetmousefree
                                                   Return
                                           endif
                                   endif
                           endif
                   endif
         endif
         call    TRIM using MM
         count   N2,MM
         if (N2 <> 0 AND MM <> "00")
                          pack    HOLDDAT,MM,DD,CCField,YY
                          move    holddat to cdat
                          pack    NewDate1,MM,SLASH,DD,SLASH,CCField,YY
         else
;                          clear NewDate1
;                          clear Checkdate
                          alert   caution,"Invalid Month!",result
                          setfocus EditTextBoxes(1)
                          call ordersetmousefree
                          return
         endif
         setitem EditTextBoxes(1),0,NewDate1
;==========================================================
;Check Number
         getitem EditTextBoxes(2),c0,str6
         call zfillit using str6
         if ((str6 = "")|(str6 = "000000"))
                    alert caution,"Not a valid Check Number",result,"Error"
                    setfocus EditTextBoxes(2)
                    call ordersetmousefree
                    return 
         endif
         move str6 to holdnum1
         move holdnum1 to cnum
;Check Amt
         clear dumvar92
         clear str13
         getitem EditTextBoxes(3),c0,str13
         move str13 to dumvar92
         compare c0 to dumvar92
         if equal 
                          alert caution,"Not a valid Amount",result,"Error"
                          setfocus EditTextBoxes(3)
                          call ordersetmousefree
                          return 
         endif
         if less 
                          alert caution,"Not a valid Amount",result,"Error"
                          setfocus EditTextBoxes(3)
                          call ordersetmousefree
                          return 
         endif
;Total Commission
         getitem  ncar001aEditText010,c0,str13
         clear yearrefund
         move str13 to yearrefund
         compare c0 to yearrefund
         if equal 
                           alert caution,"Commission to be paid cannot be zero",result,"Error"
                           call ordersetmousefree
                           return 
         endif
         if less 
                            alert caution,"Commission to be paid cannot be less than zero",result,"Error"
                            call ordersetmousefree
                            return 
         endif
         compare yearrefund to dumvar92 
         if not equal
                             alert caution,"Total Commission Does not Match Check Total",result,"Error"
                             call ordersetmousefree
                             return
         endif 
         setprop  report2,visible=0
         call ListviewPAY1
         call ListviewPAY2
         call ListviewPAY3
         call ListviewPAY4
         call ordersetmousefree
         return         
;   endif
ListviewPAY1
        NCAR001aListView001.getitemcount giving n9
        clear n8
        sub c1 from n9
        clear str6
        clear str12
        for n8 from "0" to n9       
                            NCAR001aListView001.getitemcheck giving n1 using n8
                            if (n1 = c1)
                                 NCAR001aListView001.getitemtext giving str6 using n8,1
                                 rep zfill,str6
                                 move str6 to clr
                                 move clr to ncomfld
                                 NCAR001aListView001.getitemtext giving str12 using n8,8
                                 move str12 to cpd
                                 call ncomwrt
                            endif
        repeat
        return
ListviewPAY2
        NCAR001aListView002.getitemcount giving n9
        clear n8
        sub c1 from n9
        clear str6
        clear str12
        for n8 from "0" to n9       
                            NCAR001aListView002.getitemcheck giving n1 using n8
                            if (n1 = c1)
                                 NCAR001aListView002.getitemtext giving str6 using n8,1
                                 rep zfill,str6
                                 move str6 to clr
                                 move clr to ncomfld
                                 NCAR001aListView002.getitemtext giving str12 using n8,8
                                 move str12 to cpd
                                 call ncomwrt
                            endif
        repeat
        return
ListviewPAY3
        NCAR001aListView003.getitemcount giving n9
        clear n8
        sub c1 from n9
        clear str6
        clear str12
        for n8 from "0" to n9       
                            NCAR001aListView003.getitemcheck giving n1 using n8
                            if (n1 = c1)
                                 NCAR001aListView003.getitemtext giving str6 using n8,1
                                 rep zfill,str6
                                 move str6 to clr
                                 move clr to ncomfld
                                 NCAR001aListView003.getitemtext giving str12 using n8,8
                                 move str12 to cpd
                                 call ncomwrt
                            endif
        repeat
        return
ListviewPAY4
        NCAR001aListView004.getitemcount giving n9
        clear n8
        sub c1 from n9
        clear str6
        clear str12
        for n8 from "0" to n9       
                            NCAR001aListView004.getitemcheck giving n1 using n8
                            if (n1 = c1)
                                 NCAR001aListView004.getitemtext giving str6 using n8,1
                                 rep zfill,str6
                                 move str6 to clr
                                 move clr to ncomfld
                                 NCAR001aListView004.getitemtext giving str12 using n8,8
                                 move str12 to cpd
                                 call ncomwrt
                            endif
        repeat
        return


;Routine to Print Out History




Report2DestroyObjects
        destroy ObjectColl
        destroy report2ok
        return       
OrderSetMouseBusy
        setmode *mcursor=*wait
        return
OrderSetMouseFree
        setmode *mcursor=*arrow
        return
clearvars

FileGo
        call  Close_ncar001a
        return
EditGo
        return
OptionsGo
        return
HelpGo
        setprop AboutMssg,visible=1
        return



pdfthis
.Patch 1.4 Commented Out
.         clear     taskname
.         append    "c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\butil job=O34 INfile=",TASKNAME
.         APPEND    PRTITLE TO TASKNAME
.         APPEND    " B=",TASKNAME
.         APPEND    user TO TASKNAME
.         reset     taskname
.;         alert caution,taskname,result
.         EXECUTE   TASKNAME
.Patch 1.4 Comment Out
.Patch 1.4 Code Added
.          prtplay             PRTFILE1,"PDF:",str55
          pause     c10
          move      c1 to nusepath
          clock     port to str3
          unpack     str3 into str2,str1
          pack       str3 from str1,str2
          MOVE      str3 TO NUSEFLD .removed FOR TESTING only
          REP       ZFILL IN NUSEFLD
          CALL      NUSEKEY
          goto      userng if over
          scan      "INVALID" in nuseuser
          goto      userng if equal
          squeeze    nuseuser,nuseuser
EmailReport
.   pack str55,PRTITLE,".pdf"
   move    "Here is your commission report for CARE.",MailSubjct
          move    str55,Mailbody
          clear   MailTO
          append  nuseuser,MailTO
          append  "@nincal.com",MailTO
          reset   MailTO
          move      MailTO,MailFrom
          Pack      MailAttach from str55    
          call      SendMail

ClearForm1
          ncar001aListView001.DeleteAllItems giving N9
          ncar001aListView002.DeleteAllItems giving N9
          ncar001aListView003.DeleteAllItems giving N9
          ncar001aListView004.DeleteAllItems giving N9
          setitem   ncar001aEditText001,0,""
          setitem   ncar001aEditText002,0,""
          setitem   ncar001aEditText003,0,""
          setitem   ncar001aEditText004,0,""
          setitem   ncar001aEditText005,0,""
          setitem   ncar001aEditText006,0,""
          setitem   ncar001aEditText007,0,""
          setitem   ncar001aEditText008,0,""
          setitem   ncar001aEditText010,0,""
          return
ClearVars1
          clear     BegQtr1
          clear     EndQtr1
          clear     BegQtr2
          clear     EndQtr2
          clear     BegQtr3
          clear     EndQtr3
          clear     BegQtr4
          clear     EndQtr4
          clear     BEGYr   
          clear     ENDYr   
          clear     PrvBegYr
          clear     PrvEndYr
          clear     OrdJul  
          clear     TierYrPrev   
          clear     TierYrCur    
          clear     QtrRefund1   
          clear     QtrRefund2   
          clear     QtrRefund3   
          clear     QtrRefund4   
          clear     PosQTRRefund1
          clear     PosQtrRefund2
          clear     PosQtrRefund3
          clear     PosQtrRefund4
          clear     YearRefund
          clear     curyr        
                    clear               MailDate  
                    clear               InvDate   
                    clear               RECDDate  
                    clear               CALCDATE  
                    clear               PMTDUE    
                    clear               COMDUE    
                    clear               SIXTY     
                    clear               DUMVAR92  
                    clear               COMMISH   
                    clear               LRINCOM   
                    clear               shipsw    
                    clear               mrgsw     
                    clear               n12       
                    clear               PAID      
                    clear               CCField   
                    clear               NewDate1  
                    clear               Holddat   
                    clear               HOLDNUM1  
                    clear               QTRHEADER 
                    clear               Rowcount  
                    clear               PgCnt     
                    clear               CRECORDS  
                    clear               HOLDCLIENT
          setitem   NCAR0001StatText001,0,""
                    setitem             NCAR0001StatText002,0,""
          return

CompTabClick
.Force LostFocus event for fields when switching tabs.
.This is done so that fields found on other forms that require data
.established through LostFocus events will be set.
.Switching to another tab does not affect the focus on that
.particular form!  LostFocus events must be triggered!
        if (N2 = C1)
                Deactivate x1
        elseif (N2 = C2)
                Deactivate y1
        endif
        return
 
CompTabChange
        move    N2,TabNum
.
        if (N2 = C1)
                Activate x1
        elseif (N2 = C2)
          Activate y1
        endif
        return
.patch 1.4 Code Added
userng
          clear     taskname
          append    "I'm sorry I've lost track of who you are,",taskname
          append    NewLine,taskname
          append    "Please leave the program and try again!",taskname
          reset     taskname
          alert     caution,taskname,result
          shutdown
   stop
.patch 1.4 Code Added
        INCLUDE   NORDIO.INC   
;         include  ninvio.inc
          include   ninvio.inc
          INclude   NInvAcdio.inc
;         include  compute.inc
          include   compute.inc
        INCLUDE   NSHPIO.INC   
        include   NMRGIO.INC   
        include   NOWNIO.INC   
        include   NDATIO.INC   
        include   COMLOGIC.INC 
        include   NACDIO.INC   
        include   NDAT3IO.INC  
.START PATCH 1.1 REPLACED LOGIC
.        include nmlrIO.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
.END PATCH 1.1 REPLACED LOGIC
        include   NADJIO.INC   
        include   NCMPIO.INC   
        INCLUDE   NCOMIO.INC   
        INCLUDE   NPASIO.INC   
        INCLUDE   PORTCALC.inc 
        include   nuseio.inc   
