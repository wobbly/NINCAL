PC        EQU       0
         INCLUDE    COMMON.inc
         INCLUDE    CONS.inc
         INCLUDE    NORDDD.inc
         INCLUDE    NPASDD.inc
         INCLUDE    NCOMDD.INC
         INCLUDE    NDATDD.INC
Release init    "1.7"          DMB      02OCT2006 Allow Printing of report when no records are selected.
.Release init    "1.6"         DMB      24JAN2005 Bug Fix.  Register 
.Release init    "1.5"         ASH      05AUG2004 Logo Conversion
.Release init    "1.4"         DMB      05JAN2003 Added Code to omit zero qty's
;Release init    "1.3"         DMB     REWRITE USING LISTVIEW OBJECT 6.0

;*These LR's were using the old method of calculating commissions
;*If this quarter is rerun at any time the commission report will 
;*NOT match with with these records exactly
;*This program cannot reproduce the quarterly statement that ties with these records
;*The 2nd quarter 2002 will be the first time using the new calculation for commission.
;*Any reports run for that 2nd quarter 2002 or any subsequent quarter can be reproduced
;*Note that these records are for the 1st quarter of 2002
;*****1st Quarter 2002*****
;.437133       28.59  024512  04082002
;.411857       29.94  024512  04082002
;.437285       15.00  024512  04082002
;.409039       43.86  024512  04082002
;.437146       22.29  024512  04082002
;.409032       43.92  024512  04082002
;.437147       21.48  024512  04082002
;.409372       33.63  024512  04082002

PCOMFILE  PFILE




SEQCHK    FILE
CHECKFILE IFILE    
ROBBFILE  FILE
SORTFLE   DIM       70        ;Var to pack file names of sort
;MISC     DIM       10        ;MISC
COMPO               DIM                 12        Mailer PO
COMKEY              DIM                 12        MAILER Key
COMMLR              DIM                 45        Mailer Name
COMNUM              DIM                  4        Mailer Num
COMLR               DIM                  6        Mailer LR
COMORD              DIM                 10        Mailer Order Date
COMRTN              DIM                 10        Mailer Return Date
COMMAIL             DIM                 10        Mailer Mail Date
COMINV              DIM                 10        Mailer Invoice Date
COMRECD             DIM                 10        Mailer Payment Rec'd Date
COMRE               DIM                  4        Mailer Rent/Exch
COMNET              DIM                  2        Mailer Net
COMRENT             DIM                 10        Mailer Rental Qty
COMEXCH             DIM                 10        Mailer Exchange Qty
COMOUTP             DIM                  9        Mailer Output
COMCAMP             DIM                  6        Campaign #
COMCDATE            DIM                  8        Campaign Date   
COMAR     FORM      10.2
;AIM READS
Akey1     init      "01X"
Akey2     init      "02X"
AKEY4     init      "04X"
;CalcVARS
PAID                DIM        9
SIXTY               DIM        6
LSTQUAL             DIM       10
;For Julian Comparison
MailDate            FORM       5
InvDate             FORM       5
RECDDate            FORM       5
CALCDATE            FORM       5
PMTDUE              FORM       5
COMDUE              DIM       10        ;Due date for commision refund     
;To get info for qtrs past and present
CurQtr              form       1 
QTRPARAM1           form       2 
QTRPARAM2           form       2 
QTRYEAR             Form       4 
PREVYEAR            FORM       4 
NEXTYEAR  FORM       4 
SAVQTY              DIM                 10        ;Commission based Order qty
CUMREF              FORM                 9.2      ;Cummulative Refund
CUMORD              FORM                10        ;Cummulative Ordered Total
CUMEXCH             FORM                10        ;Cummulative Ordered EXCH Total
CUMRENT             FORM                10        ;Cummulative Ordered Rent Total
CUMQRENT            FORM                10        ;Cummulative Qualified Rent
CUMMQTY             FORM                10        ;Missed Qty that Qualified
POSREF              FORM                 9.2      ;Possible Refund
MISREF              FORM                 9.2      ;Missed Refund
MSQRENT             FORM                10        ;Missed Rental Qty
;Subtotal Vars for Campaign
CAMDUE              FORM                10.2      ;total refund due for campaign                
CAMPOS              FORM                10.2      ;total refund possible for campaign
CAMNUM              DIM                  6        ;campaign number
MAILERDUE           FORM                10.2      ;Subtotal for mailer  
MAILERPOS           FORM                10.2      ;Possible Subtotal for mailer 
CAMARTOTAL          FORM                10.2      ;AR subtotal for campaign                           
ARTOTAL             FORM                10.2      ;AR TOTAL
GRANDAR             FORM                10.2      ;Total for all campaigns
;
MONTHLYFLG          FORM       1
QUARTERFLG          FORM       1
MIFLAG              DIM        1        ;Flag to show which date (Invoice/Mail date)is being used to create 60 day due date
TIERFLAG            FORM       1
;Print Vars
;Print File Creation
PRTITLE             DIM       18
PRTNAME1            DIM       11
PRTDIR    INIT      "C:\WORK\"
PRTFILE1            DIM       19
QTRHEADER           DIM       17
;Columns
.Column8   form       9
.Column9   form       9
;Columns for monthly summary
column3a  form       9
column4a  form       9
column5a  form       9

;Summary Columns
Column21            Form       9
Column21a           Form       9
Column22            Form       9
Column23            Form       9
Column23a           Form       9
Column24            Form       9
Column24a           Form       9
Column25            Form       9
Column26            Form       9
Column26a           Form       9
;Title1   form    9
Title2              form       9
Title3              form       9
Title4              form       9
Title21             form       9
Title22             form       9
font7     font
          create    font7,"Arial",size=10
font9     font
          create    font9,"Times New Roman",size=10,italic
font8     font
          create    font8,"Times New Roman",size=9,bold,italic
                    move       "100",column
                    move       "500",column9
                    move      "3000",column1
                    move      "4000",column2
                    move      "5000",column3                  
                    move      "6000",column4
                    move      "7000",column5
                    move      "8000",column6
                    move      "9000",column7
                    move      "10000",column8
                    move      "5300",column3a
                    move      "6300",column4a
                    move      "7300",column5a
                    move      "2260",column21
                    move      "2560",column21a
                    move      "3260",column22
                    move      "4260",column23
                    move      "4560",column23a
                    move      "6260",column24
                    move      "6560",column24a
                    move      "7260",column25
                    move      "8260",column26
                    move      "8560",column26a
                    move      "4020",Title21
                    move      "6600",Title22
                    move      "9320",Title2      ;.Date Header
                    move      "5260",Title3

;.Page Count
Rowcount  form       3        ;KEEP TRACK OF ROW PER PAGE
PgCnt     form       9        ;COUNT OF PAGES
CRECORDS  FORM       9
;.Hold Variables
HOLDCLIENT          DIM       45
HOLDDAT             DIM        8
HOLDNUM             DIM        6
HOLDCKTOT FORM       9.2

;.Subtotals for Clients
PREFUND   FORM       7.2
PQUANTITY FORM      10
TOTREFUND           FORM                 7.2
;
DUMVAR10  FORM      10
DUMVAR13  FORM      10
DUMVAR102 FORM      10.2
DUMVAR92  FORM       9.2
DUMVAR72  FORM       7.2

;.Tiered Limits
Tier                FORM       2.2
Tier1               FORM      "3.00"
Tier2               FORM      "3.50"
Tier3               FORM      "4.00"         

Tier1QTY            FORM      "3500000"
Tier2QTY            FORM      "4500000"
;.Vars to help formualte tiering amts
TIERAMT1            FORM       7.2
TIERAMT2            FORM       7.2
TIERAMT3            FORM       7.2
TIERCUMAMT1         FORM       7.2
TIERCUMAMT2         FORM       7.2
TIERCUMAMT3         FORM       7.2
TIERCUMQTY1         FORM      10
TIERCUMQTY2         FORM      10
TIERCUMQTY3         FORM      10
;.create subtotals of order qty for year to calc when tier goes into effect
TierYrNext          FORM      10
TierYrPrev          FORM      10
TierYrCur           FORM      10

;.Missed Qty totals
MTIERAMT1 FORM      7.2
MTIERAMT2 FORM      7.2
MTIERAMT3 FORM      7.2
MTIERCUMAMT1        FORM      7.2
MTIERCUMAMT2        FORM      7.2
MTIERCUMAMT3        FORM      7.2
MTIERCUMQTY1        FORM      10
MTIERCUMQTY2        FORM      10
MTIERCUMQTY3        FORM      10

;.Objects for report form
;.report stuff
RptCan    DIM       1
EditTextBoxes       EditText  (4)
Buttons   Button    (2)
Radios    Radio     (4)
ComboBoxes          ComboBox  (2)
StatTextBoxes       StatText  (3)
ObjectColl          Collection
;.Masking Vars
mask9p2             INIT      "$ZZ,ZZZ,ZZZ.99"         
dim14     DIM       14
mask13    INIT      "Z,ZZZ,ZZZ,ZZ9"         
Dim13a    DIM       13                      
MASK11    INIT      "$ZZZ,ZZZ.99"
num11     DIM       11
dum10     form      10
mask15    INIT      "$ZZZ,ZZZ,ZZZ.99"
num15     DIM       15
dum13     FORM      13
;
NextYr              DIM        4
PresYr              DIM        4
PastYr              DIM        4
;
RED       COLOR
BLACK     COLOR
MAUVE     color
colornum form       24
YELLOW    color
BLUE      color
;
CheckDate DIM        8
CheckTotal          FORM       7.2
;.for control tabs
TABLAST             FORM                2          .Keep track of Last tab
TABNUM              FORM                2                                         
;.Menu
;.Set Up Menu Bar
mFile     menu
mEdit     menu
mOptions  menu
mHelp     menu
;.Present Data for Menu Bar
sReports  submenu
;
FData               init                "&File;E&xit"
EData               init                "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData               init                "&Options;&Search;-;&Reports"
RData     init      "&Reports;&Monthly Estimate;&Quarterly Statement"
HData     init      "&Help;&About"

CCField   DIM        2        ;Hold Century
NewDate1  DIM       10        ;New Formatted Date
Quesbox   INTEGER    1,"0x000004"       ;Question box to add xref,beginning balance
DateStr             DIM       10
;
VT_BOOL   EQU       11
OTRUE     variant
OFALSE    variant
IntIndex  INTEGER 3 
ColHeads  automation
ColHead   automation
ListIts   automation
ListIt    automation
SubIt     automation
NewItem   automation
LUCIDAFONT          FONT
;

.Patch 1.7 Var Added
yesno1    integer   1,"0x000004"
.Patch 1.7 Var Add End
RowHold   form      9

          create    OTRUE,VarType=VT_BOOL,VarValue=1
          create    OFALSE,VarType=VT_BOOL,VarValue=0
          create    blue=*blue
          create    red=*red
          create    black=*black
          create    yellow=255:255:160
          create    mauve=150:60:100
          getitem   mauve,0,colornum
          create    LUCIDAfont,">Lucida Console",size=14
.START PATCH 1.5 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.5 ADDED LOGIC
Selstate  FORM       8
tierstr   DIM        5
;
        move    "NCOM0001.PLS",Wprognme
        move    "LW Robbins Commission",Wfunction
        move    "David Baca",Wauthor
        move    release,Wrelease
        move    "June 14, 2002",Wreldate
;
abt       plform    About
rpt2      plform    Report2
pss       plform    Passwrd
y1        plform    NCOM001b
x1        plform    NCOM001a
x         plform    NCOM0001
          winhide
;
          formload x
          formload x1,ncom0001
          formload y1,ncom0001
          formload RPT2
          formload abt
          formload pss
;
          create    ncom0001;mFile,FData
          create    ncom0001;mEdit,EData,mFile
          create    ncom0001;mOptions,OData,mEdit
          create    ncom0001;mHelp,HData,mOptions
          create    ncom0001;sReports,RData,mOptions,3   
;.Activate Menus
;
          activate  mFile,FileGo,result
          activate  mEdit,EditGo,result
          activate  mOptions,OptionsGo,result
          activate  mHelp,HelpGo,result
;SUBMENU
          activate  sReports,ReportsGo,result   
;Sort By Order Date
          pack      SortFle,NTWKPATH5,"lwrobb.dat",comma,NTWKPATH5,"lwrobb.SRT"
          pack      taskname,sortfle,";","96-99,90-91,93-94"
          sort      taskname
;Set some properties for ListView object
          setprop   ListView1,*HideColumnHeaders=OFALSE
          setprop   ListView1,*HideSelection=OFALSE
          setprop   ListView1,*HotTracking=OTRUE
          setprop   ListView1,*FullRowSelect=OTRUE
          setprop   ListView1,*MultiSelect=OTRUE
          setprop   ListView1,*Sorted=OTRUE
          setprop   ListView1,*SortOrder=0
          setprop   ListView1,*AllowColumnReorder=OTRUE
          setprop   ListView1,*View=3
          getprop   ListView1,*ColumnHeaders=ColHeads
;Define Column Headers
          ColHeads.Add using *Index=1,*Key="one",*Text="Mailer Num",*Width=50
          ColHeads.Add using *Index=2,*Key="two",*Text="Mailer Name",*Width=70
          ColHeads.Add using *Index=3,*Key="three",*Text="LR",*Width=70
          ColHeads.Add using *Index=4,*Key="four",*Text="Rent\Exch",*Width=70
          ColHeads.Add using *Index=5,*Key="five",*Text="Quantity",*Width=70
          ColHeads.Add using *Index=6,*Key="six",*Text="Mail Date",*Width=70
          ColHeads.Add using *Index=7,*Key="seven",*Text="Invoice Date",*Width=70
          ColHeads.Add using *Index=8,*Key="eight",*Text="Pmt Rec'd",*Width=70
          ColHeads.Add using *Index=9,*Key="nine",*Text="Paid",*Width=70
          ColHeads.Add using *Index=10,*Key="ten",*Text="(60)",*Width=70
          ColHeads.Add using *Index=11,*Key="eleven",*Text="Pmt Due",*Width=70
          ColHeads.Add using *Index=12,*Key="twelve",*Text="Refund",*Width=70
          ColHeads.Add using *Index=13,*Key="thirteen",*Text="InvMail",*Width=70
          ColHeads.Add using *Index=14,*Key="fourteen",*Text="List Name",*Width=70
          ColHeads.Add using *Index=15,*Key="fifteen",*Text="Net",*Width=70
          ColHeads.Add using *Index=16,*Key="sixteen",*Text="Cam No",*Width=70
          ColHeads.Add using *Index=17,*Key="seventeen",*Text="Cam Date",*Width=70
          ColHeads.Add using *Index=18,*Key="eighteen",*Text="Refund Amt 2",*Width=70
          ColHeads.Add using *Index=19,*Key="nineteen",*Text="AR",*Width=70
          ColHeads.Add using *Index=20,*Key="twenty",*Text="PPM",*Width=70
          getprop ListView1,*ListItems=ListIts
;         eventreg ListView1,$ColumnClick,ColumnClick_ListView giving ColHead

          listins ObjectColl,stattextBoxes(1),stattextBoxes(2),stattextBoxes(3),Radios(1),Radios(2),Radios(3),Radios(4),Buttons(1),ComboBoxes(1),ComboBoxes(2):
                    EditTextBoxes(1),EditTextBoxes(2),EditTextBoxes(3),EditTextBoxes(4)
;Columns001b
          NCOM001BListView001.deleteallcontents
          NCOM001BListView001.InsertColumn using "LR",100,1
          NCOM001BListView001.InsertColumn using "Amount",100,2
          NCOM001BListView001.InsertColumn using "Check ##",100,3
          NCOM001BListView001.InsertColumn using "Check Date",100,4
          NCOM001BListView001.InsertColumn using "REC",0,5






;Password Flag

          move    "V",progcode
          move    "N",PassFlag
Pass
          setprop Passwrd,visible=1
          if        (passflag = "Y")
                    move    NPASUSER,str10  
                    alert   note,"Password Accepted!",result
          else
                    add c1 to n1
                    if (n1 =c4)
                              stop
                    else
                              goto pass
                    endif
                    
          endif

        loop
                  waitevent
        repeat

DatePick
          call      Report2DestroyObjects
          setprop   Report2,title="Pick Quarter to Run report"
                    create    Report2;StatTextBoxes(1)=50:70:10:110,"Year",""
                    create    Report2;radios(1)=100:120:50:150,"1st Quarter",objectid=1,groupid=10
                    create    Report2;radios(2)=120:140:50:150,"2nd Quarter",objectid=2,groupid=10
                    create    Report2;radios(3)=100:120:200:305,"3rd Quarter",objectid=3,groupid=10
                    create    Report2;radios(4)=120:140:200:305,"4th Quarter",objectid=4,groupid=10


                    create  Report2;ComboBoxes(1)=50:71:80:130,"",";).;)."
;;                    create  Report2;ComboBoxes(2)=150:171:50:280,"",";Q)uarterly Payment Report;M)onthly Summary Report"

          activate  StatTextBoxes(1)
          create    Report2;Buttons(1)=205:230:50:100,"O&K",zorder=500,default=1
;.When dynamically creating an EditTextBox, you are only given three default events: GotFocus,LostFocus,LostFocus+Change.
;.Any other events must be registered manually.
;.Below we register a KeyPress event.
          activate  Buttons(1),StartLROK,result
          activate  comboboxes(1)
          activate  radios(1)
          activate  radios(2)
          activate  radios(3)
          activate  radios(4)
          activate  comboboxes(1)
          setitem   radios(1),0,c1
          clock     timestamp,timestamp
          unpack    timestamp,str4,str2
          move      str4 to n4
;.var used to help keep track of total orders by order date from previous year and current yr
          deleteitem          ComboBoxes(1),c0
          Add       c1 to n4
          move      n4 to str4
          insertitem          ComboBoxes(1),1,str4
          sub       c1 from n4
          move      n4 to str4
          insertitem          ComboBoxes(1),2,str4
          sub       c1 from n4,n5
          move      n5 to n4    
          move      n4 to str4
          insertitem          ComboBoxes(1),3,str4
          setitem   Comboboxes(1),n1,c2
          setprop   Report2,visible=1
          return
StartLROK

;         NCOM001aListView001.deleteallitems
          getprop   radios(1),SELGROUPID=quarterflg
          getitem   comboboxes(1),n4,n3
          getitem   comboboxes(1),n3,str4
          move      str4 to QTRYEAR
          setprop   Report2,visible=0
test
          Load      str11 with quarterflg,"1st Quarter  ","2nd Quarter  ","3rd Quarter  ","4th Quarter  "
          pack      QTRHEADER with str11,b2,str4
          clear     NCOMFLD1
          clear     NCOMFLD2
          clear     NCOMFLD3
          move      quarterflg to str2
          call      zfillit using str2
          Pack      NCOMFLD4,AKEY4,str2,str4
          call      NCOMAIM
          if not over
                    alert caution,"A Check has already been cut for this quarter.",result,"Already Ran"
                    return
          endif


          goto      Start
Start

          sub       c1 from qtryear,prevyear
          move      qtryear to str4
          setitem   NCOM0001StatText004,0,str4
          move      prevyear to str4
          setitem   NCOM0001StatText003,0,str4
          clear     str4
          call      OrderSetMouseBusy
          call      clearvars
;        call       columns
;        call       ncom001bcolumns
;OPEN FILE
          PACK      str45 with ntwkpath5,"lwrobb.srt"
          OPEN      ROBBFILE,str45
;.Branch to get params for quarter 
          move      qtryear to str4
          pack      str18,str11,B2,str4
          clear     str4
          SetItem   NCOM001aStatText007,0,str18
          Branch    Quarterflg,FirstQuarter,SecondQuarter,ThirdQuarter,FourthQuarter
FirstQuarter
          move c0 to qtrparam1
          move c4 to qtrparam2
          goto readfile
SecondQuarter
          move c3 to qtrparam1
          move c7 to qtrparam2
          goto readfile
ThirdQuarter
          move c6 to qtrparam1
          move c10 to qtrparam2
          goto readfile
FourthQuarter
          move c9 to qtrparam1
          move c13 to qtrparam2
          goto readfile

ReadFile
          loop
LWROBB
          READ    ROBBFILE,SEQ;b10:
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
                    COMAR
          until over
;
          unpack    comord,mm,b1,dd,b1,str2,yy
          pack      str4,cc,yy
          move      str4 to N4
;get current year and then check which tier it is in
;keep running total(tieryrcur) to keep track of tiering
          compare     n4,qtryear
          if equal
                    match COMRE,"Exch"
                    IF equal
                              move      COMEXCH to N10
                              add       N10 to tieryrcur
                              clear     tier
                    else
                              move      COMRENT to N10
                              add       N10 to tieryrcur
                    endif
                    IF ((tieryrcur < TIER1QTY)|(tieryrcur = TIER1QTY))
                              move      tier1 to tier
                              move      c1 to tierflag
                    Endif
                    IF ((tieryrcur > TIER1QTY)&(tieryrcur < TIER2QTY))
                              move      tier2 to tier
                              move      c2 to tierflag
                    Endif
                    IF ((tieryrcur = TIER2QTY)|(tieryrcur > TIER2QTY))
                              move      tier3 to tier
                              move      c3 to tierflag
                    Endif
          endif
;.tieryrprev
;.get previous year and then check which tier it is in
;.keep running total(tieryrprev) for previous Year
          compare n4,prevyear
          if equal
                    match COMRE,"Exch"
                    IF equal
                              move      COMEXCH to N10
                              add       N10 to tieryrprev
                              clear     tier
                    else
                              move      COMRENT to N10
                              add       N10 to tieryrprev
                    endif
                    IF ((tieryrprev < TIER1QTY)|(tieryrprev = TIER1QTY))
                              move      tier1 to tier
                              move      c1 to tierflag
                    Endif
                    IF ((tieryrprev > TIER1QTY)&(tieryrprev < TIER2QTY))
                                        move      tier2 to tier
                              move      c2 to tierflag
                    Endif
                    IF ((tieryrprev = TIER2QTY)|(tieryrprev > TIER2QTY))
                              move      tier3 to tier
                              move      c3 to tierflag
                    Endif
          endif
;.Convert Inv Date to Julian Date
                    unpack cominv,mm,b1,dd,b1,str2,yy
                    call trim using mm
;Requirement
;If there is no invoice date read next record
                    if (mm = "/")
                                        move                c0 to invdate
                                        goto                LWROBB
                    Endif  
                    call                zfillit             using mm
                    call                zfillit             using DD
                    call                cvtjul
                    move                juldays to INVDATE
;Convert Campaign Date to Julian Date
          unpack    COMCDATE,str2,yy,mm,dd
          call      zfillit using mm
          call      zfillit using DD
          call      cvtjul
          move      juldays to MAILDATE
;.Compare campaign date and Inv Dates to see which is later
          compare   MailDate to INVDATE
;.Add sixty days to pmt due date and then convert from julian to date
          IF LESS
                    move "M" to MIFLAG
                    move Maildate to calcdate
                    ADD  "60" to MAILDATE,PMTDUE
          ELSE
                    move "I" to MIFLAG
                    move Invdate to calcdate
                    ADD  "60" to INVDATE,PMTDUE
          ENDIF
          move      PMTDUE to JULDAYS
          call      CVTGREG
          pack      comdue,mm,slash,dd,slash,cc,yy
          move      mm to n2
          pack      str4,cc,yy
          move      str4 to N4
;Compare to see if commission date falls within quarter for commision
;check year
          compare   n4,qtryear
          goto      LWROBB if not equal
;Patch1.4
          call trim using comrent
          call trim using comexch
          call trim using comoutp
          goto lwrobb if (COMRENT = "0" & COMEXCH = "0" & COMOUTP = "0")
;Patch1.4

;check mo
          if      ((n2 > QTRPARAM1)&(n2 < QTRPARAM2))
          else
                    goto LWROBB
          endif
;.falls within quarter
;.is it exchange order?
          match COMRE,"Exch"
;.if exchange add exchange total to cummulative order total for quarter and exch cumm total
          IF equal
                    move      COMEXCH to N10      
                    add       N10 to CUMORD       
                    add       N10 to CUMEXCH      
                    goto      LWROBB
          else
;.if rental add rental total to cummulative order total for quarter and rent cumm total
                    move      COMRENT to N10
                    add       N10 to CUMRENT
                    add       N10 to CUMORD
          endif
;
;*If there is a net on order use full rental qty, else use output qty if available
        clear n2
        move comnet to n2
        if (N2=c0)
          MOVE COMOUTP to n10
          if (N10=c0)
                              move comrent to savqty
          else
                    move comoutp to savqty
          endif
        else
          move comrent to savqty
        endif
;Get listname(01DES)
        move        c1 to nordpath
        move        comlr to NORDFLD
        call        NORDKEY
SIXTY
        call        trim using comrecd
;if no payment rec'd
          IF ((COMRECD = "1")|(COMRECD = "0"))
                    MOVE      NO to PAID
                    MOVE      NO to SIXTY
          ELSE
;pmt rec'd
                    MOVE      YES to PAID
                    unpack    COMRECD,mm,b1,dd,b1,str2,yy
                    call      zfillit using mm
                    call      zfillit using DD
                    call      cvtjul
                    move      juldays to RECDDATE
;.subtract billing date(later of maildate or inv date) from pmt rec'd date
                    SUB       CALCDATE FROM RECDDATE,N5
                    IF (N5 < 61)
;.less than sixty
;.qulified for commission
                              MOVE YES to SIXTY
                    ELSE
;.missed commission deadline
                              MOVE "Missed" to PAID
                              MOVE NO to SIXTY
                    ENDIF
          ENDIF

          clear n10
          clear dumvar72
; savqty = rent qty(if net or no output qty) or output qty
          move  SAVQTY to N10
          ADD   N10 to CUMMQTY
          div "1000",N10,dumvar72
;mult tiered rate by (qty/1000)
          mult tier,dumvar72
          move dumvar72 to str10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        NCOM001aListView001.SetItemText  giving N8 using n7,str10,17
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        setprop ListIt,*SubItems(17)=dumvar72
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        ListIt.ListSubItems.Add giving SubIt using *Index=10,*Text=COMDUE
          move tier to tierstr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; setprop ListIt,*SubItems(19)=tierstr
;';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        ListIt.ListSubItems.Add giving SubIt using *Index=10,*Text=COMDUE
          add dumvar72 to POSREF
;.qualified refund qty
          match yes,sixty
          if  equal
                    move savqty to N9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                          setprop ListIt,*SubItems(11)=str10
;subtotals for tier1
                    If (tierflag = c1)
                                        ADD DUMVAR72 to TIERAMT1
                              ADD N9 to TIERCUMQTY1
;subtotals for tier2
                              ElseIf (tierflag = c2)
                              ADD DUMVAR72 to TIERAMT2
                              ADD N9 to TIERCUMQTY2
;subtotals for tier3
                              ElseIf (tierflag = c3)
                              ADD DUMVAR72 to TIERAMT3
                              ADD N9 to TIERCUMQTY3
                              Endif
          else
.================================================================================
;.Added to keep track of Missed Refund Qty
                    move savqty to N9
;.subtotals for missed tier1
                    If (tierflag = c1)
                              ADD DUMVAR72 to MTIERAMT1
                              ADD N9 to MTIERCUMQTY1
;.subtotals for missed tier2
                    ElseIf (tierflag = c2)
                              ADD DUMVAR72 to MTIERAMT2
                              ADD N9 to MTIERCUMQTY2
;.subtotals for missed tier3
                    ElseIf (tierflag = c3)
                              ADD DUMVAR72 to MTIERAMT3
                              ADD N9 to MTIERCUMQTY3
                    Endif
.================================================================================
;                             clear dumvar72
;                             add  c0 to dumvar72
;                             move dumvar72 to str10
                    move c0 to str10
          endif
          call loadlistview1

                    repeat
          call initselect
          call refund
          return
.==========================================================================
LoadListview1
;         ListIts.Add giving ListIt using *Text=COMNUM
          ListIts.Add giving ListIt
          setprop   ListIt,*Selected=OFALSE
          ListIt.ListSubItems.Add giving SubIt using *Index=1,*Text=COMMLR
          ListIt.ListSubItems.Add giving SubIt using *Index=2,*Text=COMLR
          ListIt.ListSubItems.Add giving SubIt using *Index=3,*Text=COMRE
          ListIt.ListSubItems.Add giving SubIt using *Index=4,*Text=SAVQTY
          ListIt.ListSubItems.Add giving SubIt using *Index=5,*Text=COMMAIL
          ListIt.ListSubItems.Add giving SubIt using *Index=6,*Text=COMINV
          ListIt.ListSubItems.Add giving SubIt using *Index=7,*Text=COMRECD
          ListIt.ListSubItems.Add giving SubIt using *Index=8,*Text=PAID
          ListIt.ListSubItems.Add giving SubIt using *Index=9,*Text=SIXTY
          ListIt.ListSubItems.Add giving SubIt using *Index=10,*Text=COMDUE
          ListIt.ListSubItems.Add giving SubIt using *Index=11,*Text=str10
          ListIt.ListSubItems.Add giving SubIt using *Index=12,*Text=MIFLAG
          ListIt.ListSubItems.Add giving SubIt using *Index=13,*Text=O1DES
          ListIt.ListSubItems.Add giving SubIt using *Index=14,*Text=COMNET
          ListIt.ListSubItems.Add giving SubIt using *Index=15,*Text=COMCAMP
          clear str10
          unpack comcdate,str4,mm,dd
          pack   str10,mm,slash,dd,slash,str4
          ListIt.ListSubItems.Add giving SubIt using *Index=16,*Text=str10
          ListIt.ListSubItems.Add giving SubIt using *Index=17,*Text=DUMVAR72
          ListIt.ListSubItems.Add giving SubIt using *Index=18,*Text=COMAR
          ListIt.ListSubItems.Add giving SubIt using *Index=19,*Text=TIERSTR
          return
InitSelect
          getprop ListIts,*Count=result
          if (result > 0)
                    for IntIndex,"1",result
                              getprop ListIts,*Item(IntIndex)=ListIt
                              getprop ListIt,*SubItems(9)=str1
                              if (str1 = YES)
;                                       setprop   ListIt,*Selected=OTRUE
                                        getprop   ListIt,*SubItems(2)=comlr
                                        setprop   ListView1,*SelectedItem=ListIt
                              endif
                              getprop ListIts(IntIndex),*SubItems(1)=str45

                              repeat
          endif
          call      OrderSetMouseFree
          return
ReportPick
          getprop ListIts,*Count=result
          if (result > 0)
          
                    call      Report2DestroyObjects
                    destroy   radios(1)
                    destroy   radios(2)
                    destroy   radios(3)
                    destroy   radios(4)
                    setprop   Report2,title="Pick Report to Run"
                    create    Report2;StatTextBoxes(1)=25:40:80:310,"Choose Your Report",""
                              create    Report2;ComboBoxes(1)=50:71:80:250,"",";Q)uarterly Payment Report;M)onthly Summary Report"
                    activate  StatTextBoxes(1)
                    create    Report2;Buttons(1)=205:230:50:100,"O&K",zorder=500,default=1
;When dynamically creating an EditTextBox, you are only given three default events: GotFocus,LostFocus,LostFocus+Change.
;Any other events must be registered manually.
;Below we register a KeyPress event.
                    activate  Buttons(1),StartPrint,result
                    activate  comboboxes(1)
                    setitem   comboboxes(1),0,c1
;                   setprop   radios(1),enabled=1
;                   setprop   radios(1),enabled=1
;                   setprop   radios(1),enabled=1
;                   setprop   radios(1),enabled=1
                    setprop   Report2,visible=1
          endif
          return
StartPrint
          setprop Report2,visible=0
          call      refund
          getitem   ComboBoxes(1),0,n1
          if        (n1 = c1)
                                        setprop             ListView1,*SortKey=13                    ;list name
                                        getprop             ListView1,*SortOrder=c0                           
                    setprop   ListView1,*SortKey=1          ;list name
                    getprop   ListView1,*SortOrder=c0
;                             move "13" to intindex
;                             move "fourteen" to str10
;                   getprop   ListView1,*ColumnHeaders=ColHeads
;                   setprop   colheads giving *index=intindex,*key=str10 giving colhead
;                   setprop   ListView1,*SortOrder=1 using *index=intindex
                    call                DetailPayReport
                    return
          else

                                        setprop             ListView1,*SortKey=15                   
                                        getprop             ListView1,*SortOrder=c0                           
                                        setprop             ListView1,*SortKey=1                   ;list name
                                        getprop             ListView1,*SortOrder=c0                           
                    call      MonthlySummary
                    return
          endif
          return
ClearVars
          clear MailDate
          clear InvDate 
          clear RECDDate
          clear CALCDATE  
          clear PMTDUE  
          clear COMDUE  
          clear SAVQTY   
          clear CUMREF   
          clear CUMORD   
          clear CUMEXCH  
          clear CUMRENT  
          clear CUMQRENT 
          clear CUMMQTY  
          clear POSREF       
          clear MISREF   
          clear MSQRENT  
          clear CAMDUE    
          clear CAMPOS    
          clear CAMNUM    
          clear MAILERDUE 
          clear MAILERPOS     
          clear CAMARTOTAL
          clear ARTOTAL   
          clear GRANDAR   
          clear HOLDCLIENT 
          clear HOLDDAT    
          clear HOLDNUM       
          clear HOLDCKTOT  
          clear PREFUND    
          clear PQUANTITY  
          clear DUMVAR10   
          clear DUMVAR13   
          clear DUMVAR102  
          clear DUMVAR92   
          clear DUMVAR72   
          clear TOTREFUND  
          clear Tier       
          clear TIERAMT1    
          clear TIERAMT2    
          clear TIERAMT3    
          clear TIERCUMAMT1 
          clear TIERCUMAMT2 
          clear TIERCUMAMT3 
          clear TIERCUMQTY1 
          clear TIERCUMQTY2 
          clear TIERCUMQTY3 
;.create subt
          clear TierYrPrev  
          clear TierYrCur   
            
;.Missed Qty 
          clear MTIERAMT1   
          clear MTIERAMT2   
          clear MTIERAMT3   
          clear MTIERCUMAMT1
          clear MTIERCUMAMT2
          clear MTIERCUMAMT3
          clear MTIERCUMQTY1
          clear MTIERCUMQTY2
          clear MTIERCUMQTY3
                    setitem NCOM001aStatText001,0,""
                    setitem NCOM001aStatText002,0,""
                    setitem NCOM001aStatText003,0,""
                    setitem NCOM001aStatText004,0,""
                    setitem NCOM001aStatText005,0,""
                    setitem NCOM001aStatText006,0,""
                    setitem NCOM001aStatText007,0,""
                    setitem NCOM001aStatText011,0,""
          setitem NCOM0001StatText001,0,""
          setitem NCOM0001StatText002,0,""
;         setitem NCOM0001StatText003,0,""
;         setitem NCOM0001StatText004,0,""
          listits.clear
          return

Refund
;
          getprop ListIts,*Count=crecords
          return if (crecords <= 0)
          clear tieramt1
          clear tiercumqty1
          clear tieramt2
          clear tiercumqty2
          clear tieramt3
          clear tiercumqty3
          clear mtieramt1
          clear mtiercumqty1
          clear mtieramt2
          clear mtiercumqty2
          clear mtieramt3
          clear mtiercumqty3
          clear totrefund
          clear cumref
          clear cumqrent
;         clear prefund
          clear totrefund
          clear msqrent
          clear misref

                    for IntIndex,"1",crecords
                    getprop ListIts(IntIndex),*Selected=N5
                    if (N5 <> C0)
                              getprop ListIts(IntIndex),*SubItems(17)=str10     ;Refund Amt
                              move str10 to dumvar72
                              ADD dumvar72 to cumref
;Patch Test
                              ADD dumvar72 to totrefund
;                             ADD dumvar72 to PREFUND      

;Patch Test
;
                              getprop ListIts(IntIndex),*SubItems(4)=str10                ;Refund Qty
                              Move str10 to dumvar13
                              ADD dumvar13 to cumqrent
                              ADD dumvar13 to pquantity

;What tier??
                              getprop ListIts(IntIndex),*SubItems(19)=TIERSTR   ;What Tier?
                                        reset tierstr                    
                                        MOVE TIER1 TO STR4 
                                        scan STR4 in tierstr
                              if equal
                                                  add dumvar72 to tieramt1
                                        add dumvar13 to tiercumqty1
                                        endif
                              reset tierstr
                              MOVE TIER2 TO STR4 
                              scan STR4 in tierstr
                              if equal
                                        add dumvar72 to tieramt2                
                                        add dumvar13 to tiercumqty2
                              endif
                              reset tierstr
                              MOVE TIER3 TO STR4 
                              scan STR4 in tierstr
                              if equal
                                        add dumvar72 to tieramt3
                                        add dumvar13 to tiercumqty3
                              endif        
                    ELSE 
;Missed Section
                              getprop ListIts(IntIndex),*SubItems(17)=str10     ;Missed Refund Amt
                              move str10 to dumvar72
;patch test
                              ADD dumvar72 to misref    
;                             ADD dumvar72 to PREFUND   
                              ADD dumvar72 to TOTREFUND 
;add to zero out balance if not selected                          
                              move "       .00" to str10                    
                              setprop ListIts(IntIndex),*SubItems(11)=str10 
;patch test
                                                          
                              getprop ListIts(IntIndex),*SubItems(4)=str10      ;Missed Refund Qty 
                              Move str10 to dumvar13
                              add dumvar13 to msqrent
;patchtest
                              getprop ListIts(IntIndex),*SubItems(19)=TIERSTR   ;What Tier?
                              reset tierstr       
                              MOVE TIER1 TO STR4 
                              scan STR4 in tierstr
                              if equal
                                                  add dumvar72 to Mtieramt1
                                        add dumvar13 to Mtiercumqty1
                              endif 
                              reset tierstr
                              MOVE TIER2 TO STR4 
                              scan STR4 in tierstr
                              if equal
                                        add dumvar72 to Mtieramt2               
                                        add dumvar13 to Mtiercumqty2
                              endif
                              reset tierstr
                              MOVE TIER3 TO STR4 
                              scan STR4 in tierstr
                              if equal
                                        add dumvar72 to Mtieramt3
                                        add dumvar13 to Mtiercumqty3
                                        endif 
                    ENDIF
          repeat



                    move tieryrcur to str10             
                    move str10 to n10                   
                    move mask13 to dim13a               
                    edit n10 to dim13a                  
                    setitem NCOM0001StatText002,0,dim13a


                    move tieryrprev to str10             
                    move str10 to n10                   
                    move mask13 to dim13a               
                    edit n10 to dim13a                  
                    setitem NCOM0001StatText001,0,dim13a

;Total Qty Refunded 3.00
          move  TIERCUMQty1 to str10
          setitem NCOM001aStatText001,0,str10
;Total Qty Refunded 3.50
          move  TIERCUMQTY2 to str10
          setitem NCOM001aStatText002,0,str10
;Total Qty Refunded 4.00
          move  TIERCUMQTY3 to str10
          setitem NCOM001aStatText003,0,str10
;Total Amt Refunded 3.00
          move TIERAMT1 to str10
           setitem ncom001aStatText004,0,str10
;Total Amt Refunded 3.50
          move  TIERAMT2 to str10
          setitem NCOM001aStatText005,0,str10
;Total Amt Refunded 4.00
          move  TIERAMT3 to str10
          setitem NCOM001aStatText006,0,str10
                    call OrderSetMouseFree

                    move cumref to str12
          move str12 to dumvar102
          move mask15 to num15
          edit dumvar102 to num15
                    setitem NCOM001aStatText011,0,num15
                    RETURN
DetailPayReport
          clear pgcnt
          getprop ListIts,*Count=crecords
          return if (crecords <= 0)

          for IntIndex,"1",crecords
                    getprop ListIts(IntIndex),*Selected=N5
          until (N5 <> C0)
          repeat
          if (N5 = C0)

//.Patch 1.7 Code Modified
                    alert type=yesno1,"No records were selected to be paid do you want to continue?",n1
                    if (n1=7)    . 6 = yes , 7 = no
                              call ordersetmousefree
                              return              
                    endif
//.Patch 1.7 Code Modified End
//.Patch 1.7 Code Replaced Above        
.                   alert caution,"Select one or more records to be paid!",result,"No records selected"
.                   call ordersetmousefree
.                   return
//.Patch 1.7 Code Replaced Above End
          endif
          PACK  PRTITLE,"EOQ Commission"
          PACK  PRTNAME1,"LWROBB.LST"
          PACK PRTFILE1,PRTDIR,PRTNAME1
          PRTOPEN   PCOMFILE,"\\NINs2\Laser2",PRTITLE,noprint,spoolfile=PRTFILE1
          Call newpage
;         clear totrefund
;         clear cumref
;         clear cumqrent
          clear prefund
          clear pquantity 
;
                    for IntIndex,"1",crecords
                                        getprop ListIts(IntIndex),*SubItems(1)=str45               ;Client Name
                    if (intindex = 1)
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
                    if (rowcount = c1)
                    if (row > 7480)
                              call endpage
                    endif
                              prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font7,*boldon,*ll,str45;
                              add eightlpi to row
                                        prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font8,*ll,*ULON,*boldon,"List Name";
                              add eightlpi to row
                              add "30",row
                              add c2 to rowcount
                    endif
                    getprop ListIts(IntIndex),*SubItems(13)=str35     ;List Name
                    prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font13,*ll,str35;
;LR
                              getprop ListIts(IntIndex),*SubItems(2)=str6       ;LR
                    prtpage PCOMFILE;*pcolumn2:row,*ALIGNMENT=*CENTER,*font=font13,*ll,str6;
;get which date came later INV\Mail, move to str1
                              getprop ListIts(IntIndex),*SubItems(12)=str1               ;Mail/Inv
                    if (str1 = "M")
;now using campaign date
                                        getprop ListIts(IntIndex),*SubItems(16)=str10     ;Mail Date
                                        prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font13,*ll,str10;
                    elseif (str1 = "I")
                              getprop ListIts(IntIndex),*SubItems(6)=str10               ;Inv Date
                              prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font13,*ll,str10;
                    endif
          
                    getprop ListIts(IntIndex),*SubItems(10)=str10               ;Payment Due Date
                    prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font13,*ll,str10;
                    getprop ListIts(IntIndex),*SubItems(7)=str10               ;Payment Rec'd Date
                    move str10 to str11
                    call trim using str11
                    IF ((str11 = "1")|(str11 = "0"))
                              move "Open" to str10
                    Endif
;If pmt made by payment due date bold
;Turn Off Until Further Notice
;         getprop ListIts(IntIndex),*SubItems(9)=str1               ;On Time??
;         if (str1 = YES)
;                   prtpage PCOMFILE;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=font13,*boldon,*ll,str10;
;         else
                              prtpage PCOMFILE;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=font13,*ll,str10;
;         endif

                    getprop ListIts(IntIndex),*SubItems(4)=str10               ;Payment Due Date
                    Clear dumvar10
                    move str10 to dumvar10
                    ADD dumvar10 to PQUANTITY
                    call FormatNumeric using str10,str13,comma
;.Moved to be right justified
                    prtpage PCOMFILE;*p8320:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;

                    getprop ListIts(IntIndex),*Selected=N5
                    if (N5 <> C0)
                              getprop ListIts(IntIndex),*SubItems(17)=str10               ;
;                   Clear N10
                              move str10 to dumvar72
                              ADD dumvar72 to PREFUND               ;for subtotal of client
;                   ADD dumvar72 to TOTREFUND
;                   ADD dumvar72 to CUMREF
;                   ADD dumvar10 to cumqrent 
;;if checkwrtflag is set then write to temporary file
;;                           if (checkwrtflag = YES)
;;                                   write checkfile,str6;str6,quarterflg,qtryear
;;               endif
                    else
;                             getprop ListIts(IntIndex),*SubItems(17)=str10
;                             move str10 to dumvar72
;                   ADD dumvar72 to misref
;                   ADD dumvar72 to PREFUND
;                   ADD dumvar72 to TOTREFUND
;add to zero out balance if not selected
                                       move "       .00" to str10                    
;                                       setprop ListIts(IntIndex),*SubItems(11)=str10 
                    move str10 to dumvar72
;                   ADD dumvar10 to msqrent
                    endif
;;subpatch 1.4
;Moved to be right justified
                    prtpage PCOMFILE;*p9300:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str10;
                    add       eightlpi,row
                    add       "50",row
                    call      Endpage
          repeat
;
          sub       "50",row
          prtpage PCOMFILE;*p7700:row,*pensize=10,*line=8320:row;
          prtpage PCOMFILE;*p8700:row,*pensize=10,*line=9300:row;
          add       "50",row
          add       c1 to rowcount
          prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font9,*BOLDON,*ll,"Subtotal";
          move PQUANTITY to str10
          call FormatNumeric using str10,str13,comma
;Moved to be right justified
          prtpage PCOMFILE;*p8320:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,str13;
;patch1.32
;patch1.32
          clear     num11
          move      mask11 to num11
          edit      PREFUND to num11
          prtpage PCOMFILE;*p9300:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,num11;
;         move PREFUND to str10
;.Moved to be right justified
;         prtpage PCOMFILE;*p9300:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,str10;
;patch1.32
          add     eightlpi,row
          add     "50",row
          call LASTPAGE
Close
          PRTCLOSE   PCOMFILE
          PRTPLAY PRTFILE1,"@\\NINs2\Laser2"
          call OrderSetMouseFree
          return
EndPage
          if (row > 7480)
;         if (ROWCOUNT = "33")
;Added to correct page # and page label for next to last page
                    move "7750",row
                    prtpage PCOMFILE;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                    prtpage PCOMFILE;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.5 REPLACED LOGIC
.                   prtpage PCOMFILE;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
.                   prtpage   PCOMFILE;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:1300:0:7600:NINLogo
                    prtpage   PCOMFILE;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.5 REPLACED LOGIC
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
          clear     row
          move      "300",row
          prtpage PCOMFILE;*pTitle2:row,*ALIGNMENT=*Left,*ll,*font=font12,"Date: ";
          clock     timestamp,str8
          unpack    str8,str2,yy,mm,dd
          clear     str10
          pack      str10,mm,slash,dd,slash,str2,yy
          prtpage PCOMFILE;*font=font12,*ll,str10;
          prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,"LW Robbins Commission";
                    add       eightlpi,row
                    add       "30",row
          prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,"Quarterly Report";
                    add       eightlpi,row
                    add       "30",row
          prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,QTRHEADER;
                    add       eightlpi,row
                    add       "60",row
          prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Billing";
          prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Pymt Due";
          prtpage PCOMFILE;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Payment";
          prtpage PCOMFILE;*pcolumn7:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Refund";
          add       eightlpi,row
          add       "30",row
          prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Client";
          prtpage PCOMFILE;*pcolumn2:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"LR ";
          prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Date*";
          prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"For Comm.";
          prtpage PCOMFILE;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Rec'd**";
          prtpage PCOMFILE;*pcolumn6:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Quantity";
          prtpage PCOMFILE;*pcolumn7:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Amount";
          add       eightlpi,row
          add       "50",row
          return              
SUBTOTAL 
          sub       "50",row
          prtpage PCOMFILE;*p7700:row,*pensize=10,*line=8320:row;
          prtpage PCOMFILE;*p8700:row,*pensize=10,*line=9300:row;
          add       "50",row
          add       c1 to rowcount
          prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font9,*BOLDON,*ll,"Subtotal";
          move      PQUANTITY to str10
          call      FormatNumeric using str10,str13,comma
          prtpage PCOMFILE;*p8320:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,str13;
;patch1.32
          clear     num11
          move      mask11 to num11
          edit      PREFUND to num11
;         move      PREFUND to str10
          prtpage PCOMFILE;*p9300:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,num11;
;         prtpage PCOMFILE;*p9300:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,str10;
;patch1.32
          add       eightlpi,row
          add       "50",row
          if (row > 7480)
                    call endpage
          endif
          prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font7,*boldon,*ll,str45;
          add       eightlpi to row
          prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font8,*ll,*boldon,*ulon,"List Name";
          add       eightlpi,row
          add       "30",row
          add       c2 to rowcount
;.       call crossref
          Clear     PREFUND
          Clear     PQuantity
          call      ENDPAGE
          goto      RECORD

LastPage
          if (row < "6051")
;        if (ROWCOUNT < "29")
                    call lastpage2
          else
.Added to correct page # and page label for next to last page
.==========================================================================================
                    move      "7750",row
                    prtpage PCOMFILE;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                    prtpage PCOMFILE;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.5 REPLACED LOGIC
.                   prtpage PCOMFILE;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
                    prtpage   PCOMFILE;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.5 REPLACED LOGIC
;patch1.31
          CLEAR     ROWCOUNT
          ADD       C1 TO PGCNT
          prtpage   PCOMFILE;*NEWPAGE:
                    *ORIENT=*LANDSCAPE:
                    *UNITS=*HIENGLISH;
          clear     row
          move      "300",row
          prtpage PCOMFILE;*pTitle2:row,*ALIGNMENT=*Left,*ll,*font=font12,"Date: ";
          clock     timestamp,str8
          unpack    str8,str2,yy,mm,dd
          clear     str10
          pack      str10,mm,slash,dd,slash,str2,yy
          prtpage PCOMFILE;*font=font12,*ll,str10;
          prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,"LW Robbins Commission";
                    add       eightlpi,row
                    add       "30",row
          prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,"Quarterly Report";
                    add       eightlpi,row
                    add       "30",row
          prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,QTRHEADER;
          add       eightlpi,row
          add       "50",row
;                   CALL  newpage
;patch1.31
                    call  lastpage2
          endif
          return
LastPage2
          prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Commission Summary";
          add       eightlpi,row
          add       "50",row
          add       "1600" to row,n7
;               prtpage PCOMFILE;*pensize=10,*RECT=row:n7:3260:7260;
          add       eightlpi,row
          prtpage PCOMFILE;*pColumn21:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Qualified Qty";
          prtpage PCOMFILE;*pcolumn23:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Qualified Refund";
.Missed Summary
          prtpage PCOMFILE;*pcolumn24:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Missed Qty";
          prtpage PCOMFILE;*pcolumn26:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Missed Refund";
          add       eightlpi,row
          add       "50",row
.Break out at 3.00
.Qualified Breakout
          clear     str10
          move      TIERCUMQty1,str10
          call      FormatNumeric using str10,str13,comma
          prtpage PCOMFILE;*pcolumn21a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;
;patch1.32
          clear     num11
          move      mask11 to num11
          edit      tieramt1 to num11
          prtpage PCOMFILE;*pcolumn23a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,num11;
;         move      c0 to str13
;         move      tieramt1 to str13
;         prtpage PCOMFILE;*pcolumn23a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;

;patch1.32
          prtpage PCOMFILE;*pcolumn22:row,*ALIGNMENT=*CENTER,*font=font13,*ll,"(@3.00/m)";
.Missed Breakout
          move      mtiercumqty1 to str10
          call      FormatNumeric using str10,str13,comma
          prtpage PCOMFILE;*pcolumn24a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;
;patch1.32
          clear     num11
          move      mask11 to num11
          edit      Mtieramt1 to num11
          prtpage PCOMFILE;*pcolumn26a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,num11;
;         move      c0 to str13
;         move      Mtieramt1 to str13
;         prtpage PCOMFILE;*pcolumn26a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;
;patch1.32
          prtpage PCOMFILE;*pcolumn25:row,*ALIGNMENT=*CENTER,*font=font13,*ll,"(@3.00/m)";
                    add       eightlpi,row
                    add       "50",row
;.Break out at 3.50
;.Qualified Breakout
          clear     str10
          move      TIERCUMQty2,str10
          call      FormatNumeric using str10,str13,comma
          prtpage PCOMFILE;*pcolumn21a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;
;patch1.32
          clear     num11
          move      mask11 to num11
          edit      tieramt2 to num11
          prtpage PCOMFILE;*pcolumn23a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,num11;
;         move      c0 to str13
;         move      tieramt2 to str13
;         prtpage PCOMFILE;*pcolumn23a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;
;patch1.32
          prtpage PCOMFILE;*pcolumn22:row,*ALIGNMENT=*CENTER,*font=font13,*ll,"(@3.50/m)";
;.Missed Breakout
          move      mtiercumqty2 to str10
          call      FormatNumeric using str10,str13,comma
          prtpage PCOMFILE;*pcolumn24a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;
;patch1.32
          clear     num11
          move      mask11 to num11
          edit      Mtieramt2 to num11
          prtpage PCOMFILE;*pcolumn26a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,num11;
;         move      c0 to str13
;         move      mtieramt2 to str13
;         prtpage PCOMFILE;*pcolumn26a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;
;patch1.32
          prtpage PCOMFILE;*pcolumn25:row,*ALIGNMENT=*CENTER,*font=font13,*ll,"(@3.50/m)";
          add       eightlpi,row
          add       "50",row
;Break out at 4.00
;Qualified Breakout
          clear     str10
          move      TIERCUMQty3,str10
          call      FormatNumeric using str10,str13,comma
          prtpage PCOMFILE;*pcolumn21a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;
;patch1.32
          clear     num11
          move      mask11 to num11
          edit      tieramt3 to num11
          prtpage PCOMFILE;*pcolumn23a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,num11;
;         move      c0 to str13
;         move      tieramt3 to str13
;         prtpage PCOMFILE;*pcolumn23a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;
;patch1.32
          prtpage PCOMFILE;*pcolumn22:row,*ALIGNMENT=*CENTER,*font=font13,*ll,"(@4.00/m)";
;Missed Breakout
          move      mtiercumqty3 to str10
          call      FormatNumeric using str10,str13,comma
          prtpage PCOMFILE;*pcolumn24a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;
;patch1.32
          clear     num11
          move      mask11 to num11
          edit      Mtieramt3 to num11
          prtpage PCOMFILE;*pcolumn26a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,num11;
;         move      c0 to str13
;         move      mtieramt3 to str13
;         prtpage PCOMFILE;*pcolumn26a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,str13;
;patch1.32
          prtpage PCOMFILE;*pcolumn25:row,*ALIGNMENT=*CENTER,*font=font13,*ll,"(@4.00/m)";
          add       eightlpi,row
          add       "50",row
          sub       "50",row
          prtpage PCOMFILE;*p1940:row,*pensize=10,*line=column21a:row;
          prtpage PCOMFILE;*p3940:row,*pensize=10,*line=column23a:row;
          prtpage PCOMFILE;*p5940:row,*pensize=10,*line=column24a:row;
          prtpage PCOMFILE;*p7940:row,*pensize=10,*line=column26a:row;
          add       "50",row
;Totals
;Qualified Totals
          move      cumqrent to str10        
          call      FormatNumeric using str10,str13,comma
          prtpage PCOMFILE;*pcolumn21a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,*boldon,str13;
;patch1.32
          clear     dim14
          move      mask9p2 to dim14
          edit      CUMREF to dim14
          prtpage PCOMFILE;*pcolumn23a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,*boldon,dim14;
;         move      CUMREF to str13
;         prtpage PCOMFILE;*pcolumn23a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,*boldon,str13;
;patch1.32
          prtpage PCOMFILE;*pcolumn22:row,*ALIGNMENT=*CENTER,*font=font13,*ll,*boldon,"Totals";
;Missed Totals
          move      msqrent to str10
          call      FormatNumeric using str10,str13,comma
          prtpage PCOMFILE;*pcolumn24a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,*boldon,str13;
;patch1.32
          clear     dim14
          move      mask9p2 to dim14
          edit      MISREF to dim14
          prtpage PCOMFILE;*pcolumn26a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,*boldon,dim14;
;         move      misref to str13
;         prtpage PCOMFILE;*pcolumn26a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,*boldon,str13;
;patch1.32
          prtpage PCOMFILE;*pcolumn25:row,*ALIGNMENT=*CENTER,*font=font13,*ll,*boldon,"Totals";
                    add       eightlpi,row
                    add       eightlpi,row
                    add       "50",row
          prtpage PCOMFILE;*pTitle3:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Total Volume";
          add       eightlpi,row
          add       "50",row
          prtpage PCOMFILE;*pcolumn23:row,*ALIGNMENT=*CENTER,*font=font13,*ll,*boldon,"Year-to-Date";
;          prtpage PCOMFILE;*pcolumn23:row,*ALIGNMENT=*CENTER,*font=font13,*ll,*boldon,"This Quarter";
          move      tieryrcur to str10
;          getitem com001StatTotalOrders,0,str10
          call      FormatNumeric using str10,str13,comma
          prtpage PCOMFILE;*ptitle3:row,*ALIGNMENT=*CENTER,*font=font13,*ll,str13;
          move "7750",row
          sub "370" from row
          prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font13,*ll,"*Billing Date reflects the later of Invoice Date or Mail Date.";
          add "185" to row
;         prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font13,*ll,"**Any Payment Received on or before the Commission Deadline is in bold.";
          add "185" to row
          prtpage PCOMFILE;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
          prtpage PCOMFILE;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.5 REPLACED LOGIC
.         prtpage PCOMFILE;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
          prtpage   PCOMFILE;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.5 REPLACED LOGIC
          return
MonthlySummary
          getprop ListIts,*Count=crecords
          return if (crecords <= 0)
          for IntIndex,"1",crecords
                    getprop ListIts(IntIndex),*Selected=N5
          until (N5 <> C0)
          repeat
          if (N5 = C0)
                    alert caution,"Select one or more records to be paid!",result,"No records selected"
                    call ordersetmousefree
                    return
          endif
          clear pgcnt
          clear totrefund
          clear prefund
          PACK  PRTITLE,"Monthly Comm Rep."
          PACK  PRTNAME1,"LWROBB.LST"
          PACK PRTFILE1,PRTDIR,PRTNAME1
          PRTOPEN   PCOMFILE,"\\NINs2\Laser2",PRTITLE,noprint,spoolfile=PRTFILE1
          call newmonthpage
          add c1 to rowcount
          if (rowcount = c1)
                    getprop ListIts(IntIndex),*SubItems(1)=str45               ;Client
                              prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font7,*boldon,*ll,str45;
                              add eightlpi to row
                              prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font8,*ll,*ULON,*boldon,"Campaign Number";
                              add eightlpi to row
                              add "30",row
                              add c2 to rowcount
          endif
                    for IntIndex,"1",crecords
                    getprop ListIts(IntIndex),*SubItems(1)=str45               ;Client
                    getprop ListIts(IntIndex),*SubItems(15)=str6               ;Client
                    if (IntIndex = 1)
                              move str45 to HOLDCLIENT
                              move str6 to camnum
                              goto MailerContinue
                    else
                              match HOLDCLIENT to STR45
                              move  str45 to HOLDCLIENT
                              if not equal
                                        move str6 to camnum
                                        call CAMSUB
                                        goto MLRSUB
                              else
MailerContinue
;.                                                                    if (sumflag = YES)
                                        match str6 to camnum
                                        move str6 to camnum
                                        if not equal
                                                  call camsub
                                                  goto ContinueRec
                                        else
;.Starting point to complete new campaign
ContinueRec
                                                  getprop ListIts(IntIndex),*SubItems(15)=str6               ;Campaign Num
                                                  prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font13,*ll,str6;
                                                  getprop ListIts(IntIndex),*SubItems(16)=str10               ;Campaign Date
                                                  prtpage PCOMFILE;*pcolumn1:row,*ALIGNMENT=*CENTER,*font=font13,*ll,str10;
                                                  getprop ListIts(IntIndex),*SubItems(10)=str10               ;Payment Due Date
                                                  move str10 to datestr
;                                                          prtpage PCOMFILE;*pcolumn2:row,*ALIGNMENT=*CENTER,*font=font13,*ll,str10;

;.Commission Due
                                                  getprop ListIts(IntIndex),*SubItems(11)=str10               ;Commission Due
                                                  move str10 to dumvar72
                                                  if zero
                                                            clear dumvar72
                                                  endif
                                                  add dumvar72 to camdue        ;total refund due for campaign
                                                  ADD dumvar72 to TOTREFUND     ;total refund for all mailers for report
                                                  ADD dumvar72 to MAILERDUE     ;Subtotal for mailer
                                                  getprop ListIts(IntIndex),*SubItems(17)=str10               ;Commission Possible
                                                  move str10 to dumvar72
                                                  add dumvar72 to campos        ;total refund possible for campaign
                                                  ADD dumvar72 to PREFUND       ;totals possible refund for all mailers report
                                                  ADD dumvar72 to MAILERPOS     ;Subtotal for mailer
                                                  getprop ListIts(IntIndex),*SubItems(18)=str13               ;Commission Possible
                                                  clear dumvar102
                                                  move str13 to dumvar102
                                                  if zero
                                                            clear dumvar102
                                                  else
                                                            ADD dumvar102 to CAMARTOTAL   ;AR subtotal for campaign                         
                                                            ADD dumvar102 to ARTOTAL      ;AR subtotal for mailer
                                                            ADD dumvar102 to GRANDAR      ;AR subtotal for ALL
                                                  endif
                                        endif
                              endif
                    Endif
          repeat
          call CAMSUB
          call LASTMLRSUB
          call ROBBTOTAL
          call Endpage2
          PRTCLOSE   PCOMFILE
          PRTPLAY PRTFILE1,"\\NINs2\Laser2"
          call ordersetmousefree
          return
CAMSUB
;patch1.2
          prtpage PCOMFILE;*pcolumn2:row,*ALIGNMENT=*CENTER,*font=font13,*ll,datestr;
;patch1.2

;.AR due on invoices 
          move camARTOTAL to str13
          clear num15
          move  mask15 to num15
          edit  camartotal to num15
          prtpage PCOMFILE;*pcolumn3a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,num15;
;.Amt Due for commission(AKA paid inv that we now owe money on)
          move  camdue to str13
          clear num15
          move  mask15 to num15
          edit  camdue to num15
          prtpage PCOMFILE;*pcolumn4a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,num15;
;.AMT POSSIBLE
          move  campos to str13
          clear num15
          move  mask15 to num15
          edit  campos to num15
          prtpage PCOMFILE;*pcolumn5a:row,*ALIGNMENT=*RIGHT,*font=font13,*ll,num15;
                    Clear CAMPOS
                    Clear CAMDUE
                    clear CAMARTOTAL
;.       add c1 to n9
                    add     eightlpi,row
                    add     "50",row
                    call Endpage1
                    return
MLRSUB
       sub     "50",row
       add     "50",row
       add c1 to rowcount
       prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font9,*BOLDON,*ll,"Subtotal";
;.Total Mailer AR 
       move ARTOTAL to str13
       clear num15
       move  mask15 to num15
       edit  ARTOTAL to num15
       prtpage PCOMFILE;*pcolumn3a:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,num15;
       clear str13
       move MailerDUE to str13
       clear num15
       move mask15 to num15
       edit MailerDue to num15
;.Moved to be right justified
       prtpage PCOMFILE;*pcolumn4a:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,num15;
       clear str13
       move MailerPOS to str13
       clear num15
       move mask15 to num15
       edit MailerPos to num15
;.Moved to be right justified
       prtpage PCOMFILE;*pcolumn5a:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,num15;
       add     eightlpi,row
       add     "50",row
       Clear MailerPOS
       Clear MailerDUE
       Clear ARTOTAL
       call Endpage1
       prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font7,*boldon,*ll,str45;
       add eightlpi to row
       prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font8,*ll,*ULON,*boldon,"Campaign Number";
       add eightlpi to row
       add "30",row
       add c2 to rowcount
       goto MailerContinue
LASTMLRSUB
       sub     "50",row
;.       prtpage PCOMFILE;*p7700:row,*pensize=10,*line=8320:row;
;.       prtpage PCOMFILE;*p8700:row,*pensize=10,*line=9300:row;
       add     "50",row
       add c1 to rowcount
       prtpage PCOMFILE;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font9,*BOLDON,*ll,"Subtotal";
;.Total Mailer AR 
                     move ARTOTAL to str13
                     clear num15
       move mask15 to num15
       edit ARTOTAL to num15
       prtpage PCOMFILE;*pcolumn3a:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,num15;
;.       prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font13,*BOLDON,*ll,str13;
       clear str13 
       move MailerDUE to str13
       clear num15
       move mask15 to num15
       edit MAILERDUE to num15
;.Moved to be right justified
        prtpage PCOMFILE;*pcolumn4a:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,num15;
;.        prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font13,*BOLDON,*ll,str13;
;.       prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font13,*BOLDON,*ll,str13;
       clear str13
       move MailerPOS to str13
       clear num15
       move mask15 to num15
       edit MailerPOS to num15
;.Moved to be right justified
       prtpage PCOMFILE;*pcolumn5a:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,num15;
;.        prtpage PCOMFILE;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=font13,*BOLDON,*ll,str13;
;.       prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font13,*BOLDON,*ll,str13;
       add     eightlpi,row
       add     "50",row
       Clear MailerPOS
       Clear MailerDUE
       call Endpage1
       return

RobbTotal
       prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font7,*boldon,*ll,"Total";
;.GRAND AR TOTAL
       clear str13
       move GRANDAR to str13
       clear num15
       move mask15 to num15
       edit GRANDAR to num15
       prtpage PCOMFILE;*pcolumn3a:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,num15;
;.            prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font13,*BOLDON,*ll,str13;
       clear str13
       move TOTREFUND to str13
       clear num11
       move mask11 to num11
       edit TOTREFUND to num11
       prtpage PCOMFILE;*pcolumn4a:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,num11;
;.       prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font13,*BOLDON,*ll,str13;
;.                   prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font13,*BOLDON,*ll,str13;
       clear str13
       move PREFUND to str13
       clear num11
       move mask11 to num11
       edit PREFUND to num11
       prtpage PCOMFILE;*pcolumn5a:row,*ALIGNMENT=*RIGHT,*font=font13,*BOLDON,*ll,num11;
;.       prtpage PCOMFILE;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=font13,*BOLDON,*ll,str13;
;.       prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font13,*BOLDON,*ll,str13;
       return
NEWMONTHPAGE
        CLEAR     ROWCOUNT
        ADD       C1 TO PGCNT
        prtpage   PCOMFILE;*NEWPAGE:
                           *ORIENT=*Portrait:
                 *UNITS=*HIENGLISH;
        clear     row
        move      "300",row
        prtpage PCOMFILE;*pTitle22:row,*ALIGNMENT=*Left,*ll,*font=font12,"Date: ";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage PCOMFILE;*font=font12,*ll,str10;
        prtpage PCOMFILE;*pTitle21:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,"LW Commission Report";
        add     eightlpi,row
        add     "30",row
        prtpage PCOMFILE;*pTitle21:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,"Monthly Report";
        add     eightlpi,row
        add     "60",row
        prtpage PCOMFILE;*pTitle21:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,QTRHEADER;
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     "60",row
        prtpage PCOMFILE;*pcolumn1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Campaign";
        prtpage PCOMFILE;*pcolumn2:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Pymt Due";
        prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Total Inv";
;.        prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Commission";
;.        prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Total Possible";
        prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Commission";
        prtpage PCOMFILE;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*boldon,"Total Possible";
        add     eightlpi,row
        add     "60",row
        prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Client";
        prtpage PCOMFILE;*pcolumn1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Date";
        prtpage PCOMFILE;*pcolumn2:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"For Comm.";
;.        prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Earned";
;.        prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Commission";
        prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Amt Due";
        prtpage PCOMFILE;*pcolumn4:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Earned";
        prtpage PCOMFILE;*pcolumn5:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Commission";
        add     eightlpi,row
        add     "50",row
        return
EndPage1
          if (ROWCOUNT = "44")
;.Added to correct page # and page label for next to last page
                    move "10300",row
                    prtpage PCOMFILE;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                    prtpage PCOMFILE;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.5 REPLACED LOGIC
.                   prtpage PCOMFILE;*pcolumn5a:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
                    prtpage   PCOMFILE;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.5 REPLACED LOGIC
;         prtpage PCOMFILE;*ptitle23:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
                    CALL  newmonthpage
       endif
       return
EndPage2
          move "10300",row
          prtpage PCOMFILE;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
          prtpage PCOMFILE;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.5 REPLACED LOGIC
.         prtpage PCOMFILE;*pcolumn5a:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
          prtpage   PCOMFILE;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.5 REPLACED LOGIC
;         prtpage PCOMFILE;*ptitle23:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
          return

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
          getprop ListIts,*Count=crecords
          return if (crecords <= 0)
          call    Report2DestroyObjects
          destroy comboboxes(1)
          destroy comboboxes(2)
          destroy radios(1)
          destroy radios(2)
          destroy radios(3)
          destroy radios(4)
          setprop Report2,title="Commission Payment Screen"
          create  Report2;StatTextBoxes(1)=30:50:10:110,"Check Date","'>MS Sans Serif'(8)",ToolTip="8-digit field"
          create  Report2;StatTextBoxes(2)=50:70:10:110,"Check Number","'>MS Sans Serif'(8)"
          create  Report2;StatTextBoxes(3)=70:90:10:110,"Check Amt.","'>MS Sans Serif'(8)"
          create  Report2;EditTextBoxes(1)=30:50:90:200,MaxChars=8,EditType=2,SelectAll=1,Style=1,Border=1
          create  Report2;EditTextBoxes(2)=50:70:90:200,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
          create  Report2;EditTextBoxes(3)=70:90:90:200,MaxChars=12,EditType=3,SelectAll=1,Style=1,Border=1
          create  Report2;Buttons(1)=205:230:50:100,"&OK"
          move     NO,str1
          activate StatTextBoxes(1)
          activate StatTextBoxes(2)
          activate StatTextBoxes(3)
          activate EditTextBoxes(1)
          activate EditTextBoxes(2)
          activate EditTextBoxes(3)
          activate Buttons(1),Verify,result
          setfocus EditTextBoxes(1)
          setprop  report2,visible=1
;         RETURN
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
                    call ordersetmousefree
                    return
          endif
          CALL       ZFILLIT USING STR8
          if      (str8="00000000")
                              alert   caution,"Date Cannot be a null value!",result,"Bad Date"
                    setfocus EditTextBoxes(1)
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
                              call ordersetmousefree
                              Return
                    endif
                    move    MM,N2
                    if (N2 > "12")
                              alert   caution,"Invalid Month!",result
                              setfocus EditTextBoxes(1)
                              call ordersetmousefree
                              Return
                    else
                              move    DD,N2
                              if (N2 > "31")
                                                            alert   caution,"Invalid Day!",result
                                        setfocus EditTextBoxes(1)
                                        call ordersetmousefree
                                        return
                              else
                                        move    CCField,N2
                                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                                  alert   caution,"Invalid Year!",result
                                                  setfocus EditTextBoxes(1)
                                                  call ordersetmousefree
                                                  Return
                                        elseif (N2 = "19")
                                                  move    YY,N2
                                                  if (N2 < "80")
                                                            alert   caution,"Invalid Year!",result
                                                            setfocus EditTextBoxes(1)
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
          move str6 to holdnum
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
          compare c0 to totrefund
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
test1
          compare cumref to dumvar92 
          if not equal
                    alert caution,"Total Commission Does not Match Check Total",result,"Error"
                    call ordersetmousefree
                    return
          endif 
          setprop   report2,visible=0
          move      dumvar92 to holdcktot
          call      ListviewPAY
          call      ordersetmousefree
          return         

ListViewPay
          move holddat to CDAT
          move holdNUM to CNUM
          clear CheckTotal
          call checkdetail
          for IntIndex,"1",crecords
                    getprop ListIts(IntIndex),*Selected=N5
                    if (N5 <> C0)
                              clear     str6
                              clear     str12
                                                  getprop   ListIts(IntIndex),*SubItems(2)=str6               ;LR
                              rep       zfill,str6
                              move      str6 to clr
                              move      clr to ncomfld 
.Patch 1.6

.                                                            getprop  ListIts(IntIndex),*SubItems(11)=str12               ;Refund Amount
                                                            getprop ListIts(IntIndex),*SubItems(17)=str12     ;Refund Amt
.Patch 1.6
                              move      str12 to cpd
                              add       CPD to CheckTotal             ;for total on detail print for check
                              move      quarterflg to str2
                              call      zfillit using str2
                              move      qtryear to str4
                              pack      cqtryr with str2,str4
                              call ncomwrt

;.Printing routine
                              add c1 to rowcount
                              prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font12,*ll,CLR;
                              prtpage PCOMFILE;*pcolumn1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,CPD;
                              prtpage PCOMFILE;*pcolumn2:row,*ALIGNMENT=*CENTER,*font=font12,*ll,CNUM;
                              prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font12,*ll,CDAT;
                              add     eightlpi,row
                              add     "50",row
                              if (ROWCOUNT >= "48")
                                        call      EndDetail 
                              endif
                    endif
          repeat
          call      checktotals
          call      ordersetmousefree
          return
CheckDetail
          PACK  PRTITLE,"LWR Chk Detail"
          PACK  PRTNAME1,"LWRCkDetail.LST"
          PACK PRTFILE1,PRTDIR,PRTNAME1
          PRTOPEN   PCOMFILE,"\\NINs2\Laser2",PRTITLE,noprint,spoolfile=PRTFILE1
DetailPage
          CLEAR     ROWCOUNT
          ADD       C1 TO PGCNT
          prtpage   PCOMFILE;*NEWPAGE:
                              *ORIENT=*Portrait:
                              *UNITS=*HIENGLISH;
          clear     row
          move      "300",row
          prtpage PCOMFILE;*pTitle22:row,*ALIGNMENT=*Left,*ll,*font=font12,"Date: ";
          clock   timestamp,str8
          unpack  str8,str2,yy,mm,dd
          clear   str10
          pack    str10,mm,slash,dd,slash,str2,yy
          prtpage PCOMFILE;*font=font12,*ll,str10;
          prtpage PCOMFILE;*pTitle21:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,"LW Robbins";
          add     eightlpi,row
          add     "30",row
          prtpage PCOMFILE;*pTitle21:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,"Check Detail";
          add     eightlpi,row
          add     "60",row
          prtpage PCOMFILE;*pTitle21:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,QTRHEADER;
          add     eightlpi,row
          add     eightlpi,row
          add     eightlpi,row
          add     "60",row
          prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"LR";
          prtpage PCOMFILE;*pcolumn1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Amount";          
          prtpage PCOMFILE;*pcolumn2:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Check Num";
          prtpage PCOMFILE;*pcolumn3:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Check Date";
          add     eightlpi,row
          add     "50",row
          return                           
EndDetail
       if (ROWCOUNT >= "48")
;Added to correct page # and page label for next to last page
                    move "10300",row
                    prtpage PCOMFILE;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                    prtpage PCOMFILE;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.5 REPLACED LOGIC
.                   prtpage PCOMFILE;*ptitle22:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
                    prtpage   PCOMFILE;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.5 REPLACED LOGIC
                    CALL  detailpage
          endif
          return
CheckTotals
          prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font12,*ll,*ULON,*boldon,"Total";
          prtpage PCOMFILE;*pcolumn1:row,*ALIGNMENT=*CENTER,*font=font12,*ll,checktotal;
          add     eightlpi,row
          add     eightlpi,row
          add     "60",row
          prtpage PCOMFILE;*pcolumn:row,*ALIGNMENT=*CENTER,*font=font9,*ll,*ULON,*boldon,"Check Total";
          prtpage PCOMFILE;*pcolumn1:row,*ALIGNMENT=*CENTER,*font=font9,*ll,*boldon,holdcktot;
          move "10300",row
          prtpage PCOMFILE;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
          prtpage PCOMFILE;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.5 REPLACED LOGIC
.         prtpage PCOMFILE;*ptitle22:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
          prtpage   PCOMFILE;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
.END PATCH 1.5 REPLACED LOGIC
          prtclose PCOMFILE
          PRTPLAY PRTFILE1,"\\NINs2\Laser2"
          call ordersetmousefree
          return

OrderSetMouseBusy
          setmode *mcursor=*wait
          return
OrderSetMouseFree
          setmode *mcursor=*arrow
          return
Report2DestroyObjects
          destroy ObjectColl
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
          listview1.Refresh
        elseif (N2 = C2)
          Activate y1
        endif
        return

FileGo
;.Flag set to "N" if in Modify or New mode
          branch result to FileGo1
FileGo1
          call Close_NCOM001a
          return
Optionsgo
          branch result to OptionsGo1,OptionsGo2,OptionsGo3
OptionsGo1
        return
OptionsGo2
        return
OptionsGo3
        return

Reportsgo     
          branch result to ReportsGo1,ReportsGo2,ReportsGo3
ReportsGo1    
;         call getdates
        return
ReportsGo2    
        return
ReportsGo3    
        return
ViewGo
        return
EditGo
        return
HelpGo
          setprop AboutMssg,visible=1
        return


                      
                      
                      
          INCLUDE   NDATIO.INC
          INCLUDE   NORDIO.inc
          INCLUDE   NPASIO.inc
          INCLUDE   NCOMIO.INC
          INCLUDE   comlogic.inc
