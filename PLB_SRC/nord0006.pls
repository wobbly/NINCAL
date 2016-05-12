PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.patch5.12
                                        include   compdd.inc
                                        include   cntdd.inc
.         INCLUDE   NMLRDD.inc
.patch5.12
         INCLUDE   NORDDD.inc
         INCLUDE   NDATDD.inc
.patch5.12
.         INCLUDE   nbrkdd.inc
.patch5.12
         INCLUDE   ncntdd.inc
         INCLUDE   NOWNDD.inc
         INCLUDE   OSLSPERN.INC
         INCLUDE   NRTNDD.inc
.START PATCH 5.11A ADDED LOGIC
          INCLUDE   NCATDD.INC
.END PATCH 5.11A ADDED LOGIC
..............................................................
.Following used only in order to load Search.plf
        include ncmpdd.inc
...............................................................
Author    INit      "David Herrick"
Release   Init      "5.29"    DLH       .Allow selection of list owner copies pdf when primary select is campaign. User Beware
Reldate   Init      "2016 April 2?"
.Release   Init      "5.28"    DLH       .if excluding cancelled selected, exclude cancelled pending and LCR's as well
.Reldate   Init      "2014 December 1"
.Release   Init      "5.27"    DLH       .set PDF as default printer
.Reldate   Init      "2014 May 2"
.Release   Init      "5.26"    DLH       All Nord0005 reports run locally instead of on batch server.
.Reldate   Init      "7 February 2013"
.Release   Init      "5.25"    DLH       check for leap year
.Reldate   Init      "29 February 2012"
.Release   Init      "5.24"    DLH       add PDF of Owner order copies from Diskin rep37
.Reldate   Init      "26 August 2011"
.Release   Init      "5.23"    DLH       add Fulfillment pick off
.Reldate   Init      "28 April 2011"
.Release   Init      "5.22"    DLH       add PDF of mailer order copies from diskin rep36 , check for 64 bit Windows
.Reldate   Init      "17 February 2011"
.Release   Init      "5.21"    DLH       Use data manager
.Reldate   Init      "26 January 2011"
.Release   Init      "5.20"    DLH       Change location of Batch.exe for Vista & Win7
.Reldate   Init      "07 July 2010"
.Release   Init      "5.19"    DLH       Tweek for PLI allow suppresion of imported data
.Reldate   Init      "08 October 2007"
.Author   INit      "David Herrick"
.Release  Init      "5.18"    DLH       Tweek for PLI lr's with "B" or "M" and may be shorter that 6 non zero filled
.Reldate  Init      "18 September 2007"
.Release  Init      "5.17"    DLH       22May07   PLI company code
.Reldate  Init      "22 May 2007"
.release  init      "5.16"         JD   22NOV2006 Update islist variable.
.release  init      "5.15"        DMB   12APR2006 Update code for report 22 to not enable the copy option
.release  init      "5.14"        ASH   02AUG2004 Replaced Discount portion of XReport with new rental savings version
.release  init      "5.13"        ASH   07JUN2004 Added option to print savings on XReport if all records were rental
.release  init      "5.12"        DMB   26MAY2004 Mailer Conversion
.release           init     "5.11"         JD   26Mar04   Move ARSTATE job to Batch Server A. 
.release           init     "5.11A"         ASH 02FEB2004 DATACARD CONVERSION
.release           init     "5.10"         DMB 07JUL03      Added code for PDF and Printing of Certain. Reports 
.release           init     "5.09"         DLH July 2003 Added Code for Fiscal Year for mlr/list sum and mailplan
.release           init     "5.08"         DMB 21OCT2002 Moved coded that populates report field to primary change section
.release           init     "5.07"        DMB 15OCT2002 Added code to add one year past current year for picks
.release           init     "5.06"        DMB 09SEP2002 Added code to allow return if no records are found instead of chaining new form
.release           init     "5.05"        DMB 08AUG2002 Adding Examples when key press on report
.release           init     "5.04"        DMB 29JUL2002 Allow to make selections on reports before running batch job
.release           init     "5.03"       DMB Allow for selection of records with no maildate if selecting by lr-for dummy billing
.release           init     "5.02"       DMB 26JUN2002 for Report selection LST/MLR SUM 5yrs default is all dates(Report click event)
.release           init     "5.01"       DMB 29MAY2002 Added Dates only option for IS job
.release           init     "5.00"       DMB 12MAR2002 Gui Rewrite of Order Pick Program
RECMST   FILE      VAR=498,COMP         .was 328
wavfile  sndfile
KEY      DIM       6
LR       DIM       6              LR NUMBER
SALES    DIM       2
SALES2   DIM       2
. ..MISCELLANEOUS WORK FIELDS
.BRKFLAG  FORM      1              2=ALPHA BROKER BOOK.
CAREOF   INIT      "C/O"
KEYPAD1  INIT      "01L"
KEYPAD2  INIT      "02L"
KEYPAD4  INIT      "04L"
QUES     INIT      "??????"
AKEY1    DIM       3
KEYCOUNT FORM      2
.CORP     DIM       1              COMPANY (C OR N)
MAJPIC   FORM      2              MAJOR PICK-OFF FIELD (1-10)
MINPIC   FORM      2              MINOR PICK-OFF FIELD (1-5)
DATPIC   FORM      1              DATE PICK-OFF FIELD (1-3)
DTEANS   DIM       1              ALL DATES? (Y OR N)
ALPHANS  DIM       1              ALPHABETIC BOOK? (Y OR N)
H1       FORM      2              HORIZONTAL POSITION
H2       FORM      2              HORIZONTAL POSITION
V1       FORM      2              VERTICAL LINE
V2       FORM      2              VERTICAL LINE
YRPASS   DIM       18              HOLDS lALL REQUESTED YEARS was 6 (MAX OF 8)
MOPASS   DIM       216            HOLDS ALL REQUESTED MONTHS was 72 (MAX OF 96)
HOLDYR   DIM       2              YEAR WORK FIELD
YEAR     DIM       2              YEAR WORK FIELD
MONYEAR  DIM       4              MONTH WORK FIELD
MOYR     DIM       4              MONTH WORK FIELD
CKMO     DIM       2              MONTH CHECK FIELD
CKYR     DIM       2              YEAR CHECK FIELD
PASS     DIM       126            RECORD NUMBER WORK FIELD
MAJPASS1 DIM       126            REQUESTED RECORD NUMBERS (18 ONES/9 RANGES)
MAJPASS2 DIM       126            REQUESTED RECORD NUMBERS (18 ONES/9 RANGES)
MAJPASS3 DIM       126            REQUESTED RECORD NUMBERS (18 ONES/9 RANGES)
MINPASS  DIM       126            MINOR PICK-OFF RECORD NUMBERS (18 ONES)
MAJONRG  DIM       1              MAJOR PICK-OFF IS ONE OR RANGE (O OR R)
HOLDMLR  DIM       7              HOLDS MLR # TO OMIT REUNDANT READS
HOLDBRK  DIM       4              HOLDS BROKER # TO OMIT REDUNDANT READS
BCNAME   DIM       45             BROKER NAME
REC1     DIM       7              WORK FIELD TO HOLD ONES OR LOW RANGE
REC2     DIM       7              WORK FIELD TO HOLD HIGH RANGE
RECCNTR  FORM      2              KEYIN RECORD COUNTER ON SCREEN
SECSRCH  DIM       1              SECONDARY SEARCH/MINOR PICK-OFF (Y OR N)
FILENUM  FORM      2              TO CREATE DISKIN FILENAME
F2       DIM       2              TO CREATE DISKIN FILENAME
NEWNAME  DIM       12             USED TO SEARCH SYSTEM FOR FILE NAME IN USE
RECNAME  DIM       55             NEW FILE NAME CREATED...WRITTEN TO DRIVE 12
HOLD     DIM       7              WORK FIELD USED IN BUMPING RECORD FIELDS
HOLD2    DIM       14             WORK FIELD USED FOR RANGES
FOUND    FORM      6              AMOUNT OF RECORDS FOUND
RECORDS  FORM      9              AMOUNT OF RECORDS READ
MAJBUMP  FORM      1              TO MOVE CORRECT MAJPASS FIELD INTO MAJPASS1
TOTAL    FORM      6              TOTAL RECORDS PROCESSED IN SEQUENTIAL SEARCH
LRNO     DIM       6              LR SEARCH FIELD
LRNLOW   FORM      6              LOW NUMBER FOR LR RANGES
LRNHI    FORM      6              HIGH NUMBER FOR LR NUMBER RANGES
MLRNO    DIM       4              MLR SEARCH FIELD
MLRHI    DIM       4              HIGH NUMBER FOR MLR RANGES
OFRNO    DIM       7              OFFER SERCH FIELD
LSTNO    DIM       6              LIST NUMBER SEARCH FIELD
LSTHI    DIM       6              HIGH NUMBER FOR LIST NUMBER SEARCHES
OWNRNO   DIM       4              OWNER SEARCH FIELD
OWNLO    FORM      4              LOW NUMBER FOR OWNER RANGES
OWNHI    FORM      4              HIGH NUMBER FOR OWNER SEARCHES
BRKNO    DIM       4              BROKER SEARCH FIELD
BRKLO    dim       4              LOW NUMBER FOR BROKER SEARCHES
BRKHI    dim       4              HIGH NUMBER FOR BROKER SEARCHES
RETURN   DIM       4              RETURN-TO SEARCH FIELD
RTNLO    FORM      4              LOW NUMBER FOR RETURN-TO SEARCHES
RTNHI    FORM      4              HIGH NUMBER FOR RETURN-TO SEARCHES
CNTNO    DIM       7              MLR/CONTACT SEARCH FIELD
CONTACT  DIM       3
ALL      DIM       1              COMSELECT SWITCH "Y" FOR ALL DATES
BLANK50  INIT      "                                                 "
BLANK26  INIT      "                          "
CATAGRY  DIM       3
.START PATCH 5.11A REPLACED LOGIC
.CATCODES DIM       30
CATCODES DataList
.END PATCH 5.11A REPLACED LOGIC
REC12    dim       12
WSW      DIM       1            WITHDRAWN SWITCH DEFAULT NO
reptype  FORM       2
LODATE   FORM      5         julian low order date   (yyjjj)
HIDATE   FORM      5         julian high order date  (yyjjj)
daterng  dim       1
.Patch5.16
islist   init      "DHERRIC,CREQUES,BATCH32C"
.islist   init      "DHERRIC,JDUENAS,DMONTOY,AHARKIN,CREQUES,DBACA"
.Patch5.16
.levflag  dim       1
majrep   form      1
LBRAK    INIT      "<"
PDF      init      " PDF=Y"
.Flags for Butil
COPY     DIM       2               number of copies
PDFFLag  form      1               0-No PDF 1-PDF
Xlines   FORM      1               0-No XtraLines 1-Xtra Lines
DUPLEX   form      1               0-No Duplex 1-Duplex
.Flags for Nord0005/Nord0003
Exclude  INIT      "E"               
LCRFLAG  INIT      "L"               
PENDFLAG INIT      "P"               
NETSFLAG INIT      "N"              
.begin patch xxx
PendFlag1 Init      "N"  
.end patch xxx
.begin patch 5.19
PLFlag    INIT   "Y"                .default suppress
.end patch 5.19
OptFlag  DIM        1
.Exclude  FORM      1               0-No Exclude   1-Exclude
.LCRFLAG  FORM      1               0-Default      1-LCR's Only
.PENDFLAG FORM      1               0-Default      1-Pending order Only
.NETSFLAG FORM      1               0-No Nets      1-Nets Only


c12      form      "12"           .used to identify selection of primary search for sec search options
YEAR1    DIM       4              .used as comparison for Years combo box
yesno1   integer  1,"0x000004"
.START PATCH 5.13 ADDED LOGIC
yesno2   integer  1,"0x000104"
.END PATCH 5.13 ADDED LOGIC
HoldSel  form     3               .Holds previous selection of report type
.Stop building of the file
StopFlag init     "N"
ExitFlag init     "Y"
.Collections
ColPrim     collection              .Collection for Primary Action search
ColSec      collection              .Collection for Secondary Action search
ColRadate   collection              .Collection for Radio Button Dates
ColCombDate collection              .Collection for Combo Dates
ColFilter   collection              .Collection for Filter GroupBox
ColPrint    collection              .Collection for Print GroupBox
HOLDPRM  form     3               .Holds selected Primary Search
HOLDSEC  form     3               .Holds selected Secondary Search

PRMPIK   form     "15"            .Number of primary options
.PRMPIK   form     "13"            .Number of primary options
.PRMPIK   form     "12"            .Number of primary options
SECPIK   form     "12"            .Number of secondary options
REPPIK   form     "37"            .Number of Report Options
.Flags
SECFLAG  init     "N"             .Flag to specify where search result will be located (Primary/Secondary)
SECOND   init     "N"

PRIMTRACE form     2              . Tells what the Primary pick is
SECTRACE  form     2              . Tells what the secondary pick is

PRIMLIST  form     2              .How many search params in Primary Pick
SECOLIST  form     2              .How many search params in Secondary Pick
DateGROUP form     1              . 0-ALL Dates 1-Order Dates 2-Mail Dates 3-Return Dates
KEEPDATE1 form     5              .Julian Date of Beg Date
KEEPDATE2 form     5              .Julian Date of End Date
.patch5.11
HOLDCAMP  dim       6
.patch5.11

.report stuff
RptCan  dim     1
EditTextBoxes   EditText (4)
Buttons         Button  (3)
CheckBoxes      CheckBox (6)
ComboBoxes      ComboBox (4)
StatTextBoxes   StatText (7)
ListViews       ListView (2)
ObjectColl      Collection

Timer     Timer
..............................................................
.Menu
.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu
.Set Up SubMenu for Options
sSearch submenu

.Present Data for Menu Bar
.FData   init    "&File;&Print;Pre&view;-;E&xit"
FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search;-;&Preferences"
HData   init    "&Help;&About;Help Topics"

.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To"
..................................................................................


.Read Status
         MOVE      C1 TO NDATPATH
         move      c3 to nmlrlock
         move      c3 to nordlock
         move      c3 to ndatlock
         move      c3 to ncntpath
.Find Printer Aread
         move      portn to ncntfld1
         rep       zfill in ncntfld1
         call      ncntkey
         if        over
                   move      c2 to cntprint
         endif
.Patch5.04
.Reporter plform  nord006b
.SubPatch5.04

abt      plform  About
rpt2     plform  Report2
SRCH     plform  Search
.Load Forms
x       plform  NORD0006
.Collection for Primary Section
.ColPrim collection
        listins ColPrim,Nord0006DataListPrimary,Nord0006EditSearchP,Nord0006PrimaryAdd:
          Nord0006PrimaryRemove
.Collection for Secondary Section
.ColSec collection
        listins ColSec,Nord0006ComboSecondary,Nord0006DataListSecondary,Nord0006EditSearchS,Nord0006SecondaryAdd:
          Nord0006SecondaryRemove
.Collection for Radio Button Dates Section
.ColRadate collection
        listins ColRadate,Nord0006RadioAll,Nord0006RadioOrder,Nord0006RadioMail,Nord0006RadioReturn
.Collection for ComboButton Dates Section
.ColCombDate collection
        listins ColCombDate,Nord0006ComboMo1,Nord0006ComboDay1,Nord0006ComboYear1,Nord0006ComboMo2,Nord0006ComboDay2:
          Nord0006ComboYear2
.Collection for Filter Group Box
.Filter collection
.        listins ColFilter,Nord0006CheckExclude,Nord0006CheckNets,Nord0006CheckLCR,Nord0006ing
        listins ColFilter,Nord0006RadioExclude,Nord0006RadioNets,Nord0006RadioLCR,Nord0006RadioPending
.Collection for Print Group Box
.Printer collection
.        listins ColPrint,Nord006CheckDUP,Nord006CheckX,Nord006CheckPDF,Nord0006ComboCopy
        listins ColPrint,Nord006CheckDUP,Nord006CheckX,Nord0006ComboCopy,Nord0006ComboBox001
     winhide

          Call      Get64OS
          
        move    "NORD0006.PLS",Wprognme
        move    "NIN Order Pick Program",Wfunction
          MOve      Author,Wauthor
.        move    "David Baca",Wauthor
          Move      Release,Wrelease
.        move    "1.1",Wrelease
          Move      Reldate,Wreldate
.        move    "May 2002",Wreldate

  formload x
  formload SRCH
  formload RPT2
  formload ABT
.Timer creation
          CREATE    TIMER,18000         .30 minutes
          ACTIVATE TIMER,Timeout,RESULT

.Patch5.04
.  formload reporter
.subPatch5.04
  create  NORD0006;mFile,FData
  create  NORD0006;mEdit,EData,mFile
  create  NORD0006;mOptions,OData,mEdit
  create  NORD0006;mHelp,HData,mOptions
.Create SubMenu
  create  NORD0006;sSearch,SData,mOptions,1
.Activate Menus
.FileGo leads to stop
  activate mFile,FileGo,result
.Need this when it works
  activate mEdit,EditGo,result
  activate mOptions,OptionsGo,result
.Only a SubMenu under this one
  activate mHelp,HelpGo,result
.Activate SubMenus
  activate sSearch,SearchGo,result
.Load Primary SEARCH Combo Box
.Patch5.04
OrderPickGetCriteria external "NORD006B;OrderPickGetCriteria"
REPNUM    FORM    3
TEAMDWN   FORM    3
SELREP    FORM    3
.Patch 5.09
PDFOPT    DIM     1
FISCMO    FORM    2
DateOPT   FORM      1
.Patch5.09
.EndPatch5.04
.Patch5.05
.OPTIONS   DIM     45
.OrderRunReport external "extNORD0004;YRUNREPORT"
.NAMVAR    DIM     8
.PRTNAM    DIM     8
.EndPatch5.05
. 12 Options for Primary Search
.====================================
. 1  Mailer Number
. 2  List Number
. 3  LR Number
. 4  Offer Number
. 5  Owner Number
. 6  Broker Number
. 7  Return to Number
. 8  Comselect Only
. 9 Salesman Only
. 10 Broker\Contact Number
. 11 Broker Guaranty
. 12 Names in the News Guarantee
. 13 Dates Only
. 14 Campaign Number 
.begin patch 5.23
. 15 Fulfillment Number 
.end patch 5.23

        deleteitem Nord0006ComboPrimary,c0
        move   PRMPIK to n2
        loop
                  Load str55 with n2,"1.   Mailer Number":
                                       "2.   List Number":
                              "3.   LR Number":
                              "4.   Offer Number":
                              "5.   Owner Number":
                              "6.   Broker Number":
                              "7.   Return to Number":
                              "8.   Comselect Only":
.                                   "6.   Broker Number","7.   Return to Number","8.   Dates Only","9.   Comselect Only":
                              "9.   Salesperson Only":
                              "10.  Broker\Contact Number":
                              "11.  Broker Guaranty":
                              "12.  NIN Guaranty":
                              "13.  Dates Only":                     .Patch5.01
.patch5.11
.begin patch 5.23
.                              "14.  Campaign Number"                     
                              "14.  Campaign Number":                     
.patch5.11
                              "15.  Fulfillment Number"
.end patch 5.23
          insertitem Nord0006ComboPrimary,n3,str55
                sub c1 from n2
          until (n2 ="0")
        repeat
.Set Default Option as Mailer
        setitem  Nord0006ComboPrimary,n1,c1
        move     c1 to HOLDPRM

.Load Secondary SEARCH Combo Box
. 12 Options for Secondary Search
.====================================
. 1  Mailer Number
. 2  List Number
. 3  Offer Number
. 4  Owner Number
. 5  Broker Number
. 6  Return-To Number
. 7  Comselect Orders
. 8  Salesman Number
. 9  Category Code
. 10 Exclude Category
. 11 Mailer Key/Merge Purge
. 12 Mailer PO Number
**. 13 Returnable Mag Tape-no longer in use
**. 14 Non-Returnable Mag Tape-no longer in use
.======================================================================================================================
.All Possible Secondary Picks
.                 Load str55 with n2,"Mailer Number","List Number","Offer Number","Owner Number":
.                                   "Broker Number","Return-to Number","Comselect Orders","Salesperson Only":
.                                   "Category Code","Exclude Category Code","Mailer Key/Merge Purge","Mailer PO Number":
.                                    "Returnable Mag Tape","Non-returnable Magtape"
          deleteitem Nord0006ComboSecondary,c0
        clear n2
        move  SECPIK to n2
        loop
                  Load str55 with n2,"1.   **************************":
                                 "2.   List Number":
                                   "3.   **************************":
                                   "4.   Owner Number":
                                   "5.   Broker Number":
                                   "6.   Return-to Number":
                                   "7.   Comselect Orders":
                                   "8.   Salesperson Only":
                                   "9.   Category Code":
                                   "10.  Exclude Category Code":
                                   "11.  Mailer Key/Merge Purge":
                                   "12.  Mailer PO Number"
.                                    "13.  Returnable Mag Tape","14.  Non-returnable Magtape"

          insertitem Nord0006ComboSecondary,n3,str55
                sub    c1 from n2
          until (n2 ="0")
        repeat
.Set Default Secondary Option as null
        setitem  Nord0006ComboSecondary,n1,c0


.Load Secondary SEARCH Combo Box
. 36 Options for Report
.====================================
. 1  History of List Usage    - Alphabetical By Mailer
. 2  History of Mailer Usage  - Alphabetical By List
. 3  History of Broker Orders - Alphabetical By Mailer
. 4
. 5
. 6
. 7
. 8
. 9
. 10
. 11
. 12
. 13
. 14
.======================================================================================================================

          call MailerOfferReps
.         deleteitem Nord0006ComboReportType,c0
.        clear n2
.        move  REPPIK to n2
.        loop
.                 Load str55 with n2,"1.    History of List Usage -  Alphabetical By Mailer","2.    History of Mailer Usage - Alphabetical By List","3.    Alpha by Broker","4.    Alpha by List(No Offer Break)":
.                                   "5.    As is","6.    Numeric/Totals","7.    Prepays by List","8.    Shipping by List":
.                                   "9.    Alpha with RC","10.  Summary Y - All Types","11.  List/Mailer Summaries","12.  Rent\Exch Only":
.                                   "13.  View Totals Only","14.  Mailer Disking Download Summary","15.  Test NIN PAID\PAYABLES\LOINC","15.  NMLRALD","16.  Numeric":
.                                   "17.  TOTALS Only","18.  Mail Date","19.  Num w/ Tax","20.  Alpha by List","21.  Numeric","22.  Open Invoices","23.  Create List Number":
.                                   "25.  NIN Paid/Payables -List Owner Income","26.  Shipping By LO","27.  Invoice Copies","28.  Unbilled","29.  REG X":
..                                   "24.  Alpha/Mailer Duplex","25.  NIN Paid/Payables -List Owner Income","26.  Shipping By LO","27.  Invoice Copies","28.  Unbilled","29.  REG X":
.                                   "30.  Qty X","31.  Dwn X","32.  Arstate","33.  View List\Alphabetic by Client","34.  Qty X-List Cost Analysis- On Net"
..                                   "31.  Qty X","31.  Dwn X","32.  Arstate","33.  View List\Alphabetic by Client","34.  NMLRALD","35.  Numeric Duplex","36.  Qty X-List Cost Analysis- ON Net"
.         insertitem Nord0006ComboReportType,n3,str55
.                sub    c1 from n2
.         until (n2 ="0")
.        repeat
.Set Default Report Option as null
        setitem  Nord0006ComboReportType,n1,c0

.Deleteitem for aesthetics
          deleteitem Nord0006ComboYear1,c0
          deleteitem Nord0006ComboYear2,c0
          clock timestamp to timestamp
          unpack timestamp,str4,str2
.begin patch 5.25
          unpack timestamp,cc,yy,mm,dd
          call      datetest              .check for leap year
.end patch 5.25
.Patch5.07
                    move str4 to n4
          add c1 to n4
          move n4 to str4
.EndPatch5.07
        move str4 to year1
        move "1988" to str4
        clear n4
        move str4 to n4
        clear n3
        loop
          add c1 to n1
          insertitem Nord0006ComboYear1,n3,str4
          insertitem Nord0006ComboYear2,n3,str4
          until (str4 = YEAR1)
          add c1 to n4
                    move n4 to str4
        repeat
        move str2 to n2
          setitem  Nord0006ComboMo1,n1,c1
          setitem  Nord0006ComboMo2,n1,n2
.patch5.07
.move focus down to current year
        setitem  Nord0006ComboYear1,n1,c2
        setitem  Nord0006ComboYear2,n1,c2
.patch5.07
          deleteitem Nord0006ComboDay1,c0
          deleteitem Nord0006ComboDay2,c0
        call DAY1
        call DAY2
        setitem Nord0006ComboCOPY,c0,c1
                      setfocus Nord0006EditSearchP
.START PATCH 5.11A ADDED LOGIC
          create    CATCODES=1:1:1:1
.END PATCH 5.11A ADDED LOGIC
.begin patch 5.27
           setitem Nord0006ComboBox001,n1,c2
.end patch 5.27

        LOOP
                waitevent
                    deactivate TIMER
                    ACTIVATE TIMER,Timeout,RESULT
        Repeat
Timeout
.Add Test to make sure nothing is left open
          beep
          beep
          beep
          shutdown

SecondaryFocus
.For Lost Focus Event of Nord0006ComboPrimary
        clear n2
        clear n5
.Dont Need Users cannot select a blank entry
.         getitem Nord0006ComboPrimary,n4,n3
.         getitem Nord0006ComboPrimary,n3,str55
.        if (str55 = " ")
.                alert caution,"You must select a valid Primary Search to add!",result,"Primary Search Needed"
.         deleteitem Nord0006ComboSecondary,c0
.                deleteitem Nord0006DataListPrimary,c0
.                   setitem Nord0006EditSearchP,0,""
.                   setitem Nord0006StatClient1,0,""
.                return
.        endif
PrimaryChange
.For Change Event of Nord0006ComboPrimary
        clear n2
        clear n5
          getitem Nord0006ComboPrimary,n4,n3
        compare N3 to Holdprm
        if not equal
                deleteitem Nord0006DataListPrimary,c0
                    setitem Nord0006EditSearchP,0,""
                    setitem Nord0006StatClient1,0,""
                deleteitem Nord0006DataListSecondary,c0
                    setitem Nord0006EditSearchS,0,""
                    setitem Nord0006StatClient2,0,""
                move N3 to HoldPRM
        endif
.Delete Certain Selection that do not apply to the primary Search
.        branch  n3,MailerSecondaryCombo,ListSecondaryCombo,LrSecondaryCombo,OfferSecondaryCombo,OwnerSecondaryCombo,BrokerSecondaryCombo,ReturnSecondaryCombo,DatesSecondaryCombo:
               branch         n3,MailerSecondaryCombo,ListSecondaryCombo,LrSecondaryCombo,OfferSecondaryCombo,OwnerSecondaryCombo:
                              BrokerSecondaryCombo,ReturnSecondaryCombo,CommSecondaryCombo,SalesmanSecondaryCombo,BrokerCntSecondaryCombo:
                              BrokerGuarSecondaryCombo,NINGuarSecondaryCombo:
.patch5.11
                        DatesSecondaryCombo,CampaignSecondaryCombo
.patch5.11
.                                                  DatesSecondaryCombo
MailerSecondaryCombo
        if (n3=c1)
.Load Secondary Combo Box
                clear n3
          deleteitem Nord0006ComboSecondary,c0
                  move  SECPIK to n2
          loop
                    Load str55 with n2,"1.    **************************":
                                         "2.    List Number":
                               "3.    **************************":
                               "4.    Owner Number":
                               "5.    Broker Number":
                               "6.    Return-to Number":
                               "7.    Comselect Orders":
                               "8.    Salesperson Only":
                               "9.    Category Code":
                               "10.   Exclude Category Code":
                               "11.   Mailer Key/Merge Purge":
                               "12.   Mailer PO Number"
.                                    "13.  Returnable Mag Tape","14.  Non-returnable Magtape"
                    insertitem Nord0006ComboSecondary,n3,str55
                  sub    c1 from n2
                    until (n2 ="0")
                  repeat
                call MailerOfferReps
        endif
ListSecondaryCombo
        if (n3=c2)
                clear n3
          deleteitem Nord0006ComboSecondary,c0
                  clear n2
          move  SECPIK to n2
                  loop
                            Load str55 with n2,"1.    Mailer Number":
                                           "2.    **************************":
                                 "3.    Offer Number":
                                 "4.    Owner Number":
                                 "5.    Broker Number":
                                 "6.    Return-to Number":
                                 "7.    Comselect Orders":
                                 "8.    Salesperson Only":
                                 "9.    Category Code":
                                 "10.   Exclude Category Code":
                                 "11.   Mailer Key/Merge Purge":
                                 "12.   Mailer PO Number"
.                                    "13.  Returnable Mag Tape","14.  Non-returnable Magtape"
                    insertitem Nord0006ComboSecondary,n3,str55
                          sub    c1 from n2
                    until (n2 ="0")
                  repeat
                call    OwnerListReps
        endif

.If Primary Search is LR
LrSecondaryCombo
        if (n3=c3)
                setprop Nord0006RadioAll,enabled=c0
                  setprop Nord0006RadioOrder,enabled=c0
          setprop Nord0006RadioMail,enabled=c0
                  setprop Nord0006RadioReturn,enabled=c0
                setprop Nord0006ComboMo1,enabled=c0
                setprop Nord0006ComboMo2,enabled=c0
                setprop Nord0006ComboDay1,enabled=c0
                setprop Nord0006ComboDay2,enabled=c0
                setprop Nord0006ComboYear1,enabled=c0
                setprop Nord0006ComboYear2,enabled=c0
                clear n3
          deleteitem Nord0006ComboSecondary,c0
                  move  SECPIK to n2
          loop
                    Load str55 with n2,"1.    **************************":
                                         "2.    **************************":
                               "3.    **************************":
                               "4.    **************************":
                               "5.    **************************":
                               "6.    **************************":
                               "7.    Comselect Orders":
                               "8.    Salesperson Only":
                               "9.    Category Code":
                               "10.   Exclude Category Code":
                               "11.   Mailer Key/Merge Purge":
                               "12.   Mailer PO Number"
.                                    "13.  Returnable Mag Tape","14.  Non-returnable Magtape"
                    insertitem Nord0006ComboSecondary,n3,str55
                  sub    c1 from n2
                    until (n2 ="0")
                  repeat
                call OtherReps
                return
          endif
.Offer Number is Primary Search
OfferSecondaryCombo
        if (n3=c4)
          deleteitem Nord0006ComboSecondary,c0
                clear n3
                  clear n2
          move  SECPIK to n2
                  loop
                            Load str55 with n2,"1.    **************************":
                                           "2.    List Number":
                                 "3.    **************************":
                                 "4.    Owner Number":
                                 "5.    Broker Number":
                                 "6.    Return-to Number":
                                 "7.    Comselect Orders":
                                 "8.    Salesperson Only":
                                 "9.    Category Code":
                                 "10.   Exclude Category Code":
                                 "11.   Mailer Key/Merge Purge":
                                 "12.   Mailer PO Number"
.                                    "13.  Returnable Mag Tape","14.  Non-returnable Magtape"

                    insertitem Nord0006ComboSecondary,n3,str55
                    sub    c1 from n2
                    until (n2 ="0")
                  repeat
                call MailerOfferReps
        endif
.Owner Number is Primary Search
OwnerSecondaryCombo
        if (n3=c5)
          deleteitem Nord0006ComboSecondary,c0
                clear n3
                  clear n2
          move  SECPIK to n2
                  loop
                            Load str55 with n2,"1.    Mailer Number":
                                           "2.    **************************":
                                 "3.    Offer Number":
                                 "4.    **************************":
                                 "5.    Broker Number":
                                 "6.    Return-to Number":
                                 "7.    Comselect Orders":
                                 "8.    Salesperson Only":
                                 "9.    Category Code":
                                 "10.   Exclude Category Code":
                                 "11.   Mailer Key/Merge Purge":
                                 "12.   Mailer PO Number"
.                                    "13.  Returnable Mag Tape","14.  Non-returnable Magtape"

                    insertitem Nord0006ComboSecondary,n3,str55
                    sub    c1 from n2
                    until (n2 ="0")
                  repeat
                call    OwnerListReps
        endif
.Broker Number is Primary Search
BrokerSecondaryCombo
        if (n3=c6)
          deleteitem Nord0006ComboSecondary,c0
                clear n3
                  clear n2
          move  SECPIK to n2
                  loop
                            Load str55 with n2,"1.    Mailer Number":
                                           "2.    List Number":
                                 "3.    Offer Number":
                                 "4.    Owner Number":
                                 "5.    **************************":
                                 "6.    Return-to Number":
                                 "7.    Comselect Orders":
                                 "8.    Salesperson Only":
                                 "9.    Category Code":
                                 "10.   Exclude Category Code":
                                 "11.   Mailer Key/Merge Purge":
                                 "12.   Mailer PO Number"
.                                    "13.  Returnable Mag Tape","14.  Non-returnable Magtape"

                    insertitem Nord0006ComboSecondary,n3,str55
                          sub    c1 from n2
          until (n2 ="0")
                  repeat
                call OtherReps
        endif
.Return Number is Primary Search
ReturnSecondaryCombo
        if (n3=c7)
          deleteitem Nord0006ComboSecondary,c0
                clear n3
                  clear n2
          move  SECPIK to n2
                  loop
                            Load str55 with n2,"1.    Mailer Number":
                                           "2.    List Number":
                                 "3.    Offer Number":
                                 "4.    Owner Number":
                                 "5.    Broker Number":
                                 "6.    **************************":
                                 "7.    Comselect Orders":
                                 "8.    Salesperson Only":
                                 "9.    Category Code":
                                 "10.   Exclude Category Code":
                                 "11.   Mailer Key/Merge Purge":
                                 "12.   Mailer PO Number"
.                                    "13.  Returnable Mag Tape","14.  Non-returnable Magtape"
                    insertitem Nord0006ComboSecondary,n3,str55
                          sub    c1 from n2
          until (n2 ="0")
                  repeat
                call OtherReps
        endif
.Dates Only Number is Primary Search
.DatesSecondaryCombo
.        if (n3=c8)
.         deleteitem Nord0006ComboSecondary,c0
.                call OtherReps
.        endif
.Comselect Number is Primary Search
CommSecondaryCombo
        if (n3=c8)
          deleteitem Nord0006ComboSecondary,c0
                clear n3
                  clear n2
          move  SECPIK to n2
                  loop
                            Load str55 with n2,"1.    Mailer Number":
                                           "2.    List Number":
                                 "3.    Offer Number":
                                 "4.    Owner Number":
                                 "5.    Broker Number":
                                 "6.    Return-to Number":
                                 "7.    **************************":
                                 "8.    Salesperson Only":
                                 "9.    Category Code":
                                 "10.   Exclude Category Code":
                                 "11.   Mailer Key/Merge Purge":
                                 "12.   Mailer PO Number"
.                                    "13.  Returnable Mag Tape","14.  Non-returnable Magtape"

                    insertitem Nord0006ComboSecondary,n3,str55
                          sub    c1 from n2
          until (n2 ="0")
          repeat
                call OtherReps
        endif

.Salesman Number is Primary Search
SalesmanSecondaryCombo
        if (n3=c9)
          deleteitem Nord0006ComboSecondary,c0
                clear n3
                  clear n2
          move  SECPIK to n2
                  loop
                            Load str55 with n2,"1.    Mailer Number":
                                           "2.    List Number":
                                 "3.    Offer Number":
                                 "4.    Owner Number":
                                 "5.    Broker Number":
                                 "6.    Return-to Number":
                                 "7.    Comselect Orders":
                                 "8.    **************************":
                                 "9.    Category Code":
                                 "10.   Exclude Category Code":
                                 "11.   Mailer Key/Merge Purge":
                                 "12.   Mailer PO Number"
.                                    "13.  Returnable Mag Tape","14.  Non-returnable Magtape"

                    insertitem Nord0006ComboSecondary,n3,str55
                          sub    c1 from n2
          until (n2 ="0")
          repeat
                call OtherReps
        endif
.BrokerContact Number is Primary Search
BrokerCntSecondaryCombo
        if (n3=c10)
          deleteitem Nord0006ComboSecondary,c0
                clear n3
                  clear n2
          move  SECPIK to n2
                  loop
                            Load str55 with n2,"1.    Mailer Number":
                                           "2.    List Number":
                                 "3.    Offer Number":
                                 "4.    Owner Number":
                                 "5.    **************************":
                                 "6.    Return-to Number":
                                 "7.    **************************":
                                 "8.    Salesperson Only":
                                 "9.    Category Code":
                                 "10.   Exclude Category Code":
                                 "11.   Mailer Key/Merge Purge":
                                 "12.   Mailer PO Number"
.                                    "13.  Returnable Mag Tape","14.  Non-returnable Magtape"

                    insertitem Nord0006ComboSecondary,n3,str55
                          sub    c1 from n2
          until (n2 ="0")
          repeat
                call OtherReps
        endif
.Broker Number is Primary Search
BrokerGuarSecondaryCombo
        if (n3=c11)
          deleteitem Nord0006ComboSecondary,c0
.                clear n3
.                 clear n2
.         move  SECPIK to n2
.                 loop
.                           Load str55 with n2,"1.    Mailer Number","2.    List Number","3.    Offer Number","4.    Owner Number":
.                                   "5.    Broker Number","6.    Return-to Number","7.    Comselect Orders","8.    Salesperson Only":
.                                   "9.    Category Code","10.  Exclude Category Code","11.  Mailer Key/Merge Purge","12.  Mailer PO Number"
..                                    "13.  Returnable Mag Tape","14.  Non-returnable Magtape"
.                   insertitem Nord0006ComboSecondary,n3,str55
.                         sub    c1 from n2
.         until (n2 ="0")
.         repeat
                call OtherReps
        endif
.NIN Guarantee is Primary Search
NINGuarSecondaryCombo
        if (n3=c12)
          deleteitem Nord0006ComboSecondary,c0
.                clear n3
.                 clear n2
.         move  SECPIK to n2
.                 loop
.                           Load str55 with n2,"1.    Mailer Number","2.    List Number","3.    Offer Number","4.    Owner Number":
.                                   "5.    **************************","6.    Return-to Number","7.    Comselect Orders","8.    Salesperson Only":
.                                   "9.    Category Code","10.  Exclude Category Code","11.  Mailer Key/Merge Purge","12.  Mailer PO Number"
..                                    "13.  Returnable Mag Tape","14.  Non-returnable Magtape"
.                   insertitem Nord0006ComboSecondary,n3,str55
.                         sub    c1 from n2
.         until (n2 ="0")
.         repeat
                call OtherReps
        endif
.Patch5.01
DatesSecondaryCombo
        if (n3=c13)
          deleteitem Nord0006ComboSecondary,c0
         call OtherReps
        endif
.Subpatch5.01
.patch5.11
CampaignSecondaryCombo
          if (n3=14)

                    deleteitem Nord0006ComboSecondary,c0
                    clear n3
                    clear n2
                    move  SECPIK to n2
                    loop
                              Load str55 with n2,"1.    Mailer Number":
                                        "2.    List Number":
                              "3.    Offer Number":
                              "4.    Owner Number":
                                                            "5.    Broker Number":
                              "6.    Return-to Number":
                              "7.    Comselect Orders":
                              "8.    Salesperson Only":
                              "9.    Category Code":
                              "10.   Exclude Category Code":
                              "11.   Mailer Key/Merge Purge":
                              "12.   Mailer PO Number"
."13.  Returnable Mag Tape","14.  Non-returnable Magtape"
                              insertitem Nord0006ComboSecondary,n3,str55
                              sub    c1 from n2
          until (n2 ="0")
          repeat
          call MailerOfferReps
                    setitem ColRadate,n1,c0
                    setitem Nord0006RadioAll,n1,c1  
                    call Click_Nord0006RadioAll
          return
          endif     
.patch5.11
.Set Default Secondary Option as null
        setitem  Nord0006ComboSecondary,n1,c0
        setprop Nord0006RadioAll,enabled=c1
        setprop Nord0006RadioOrder,enabled=c1
        setprop Nord0006RadioMail,enabled=c1
        setprop Nord0006RadioReturn,enabled=c1
        setprop Nord0006ComboMo1,enabled=c1
        setprop Nord0006ComboMo2,enabled=c1
        setprop Nord0006ComboDay1,enabled=c1
        setprop Nord0006ComboDay2,enabled=c1
        setprop Nord0006ComboYear1,enabled=c1
        setprop Nord0006ComboYear2,enabled=c1
          return
AddData
.Adds data in datalist from secondary pick
          getitem Nord0006DataListPrimary,c1,n2
          getitem Nord0006EditSearchP,0,str10
          if (str10="")
          alert caution,"Item added to list cannot be blank. Please try again",result,"Attempted Blank Entry"
                setfocus Nord0006EditSearchP
          return
        endif
          insertitem Nord0006DataListPrimary,9999,str10
          setfocus Nord0006EditSearchP
          return
DelData
.Adds data in datalist from primary pick
          getitem Nord0006DataListPrimary,0,n2
          if (n2=c0)
                    alert type=yesno1," Do you want to delete all of the entries in the datalist?", result,"Clear DataList"
                              if (result=6)    . 6 = yes , 7 = no
                              deleteitem Nord0006DataListPrimary,n2
                              return
                        else
                              alert note,"Select a field in the Data Field, then click Remove or Press Delete.",result,"Select Field"
                                return
                              endif
          endif
          deleteitem Nord0006DataListPrimary,n2
          return
SecAddData
.Adds data in datalist from secondary pick
          getitem Nord0006DataListSecondary,c1,n2
          getitem Nord0006EditSearchS,0,str10
          if (str10="")
          alert caution,"Item added to list cannot be blank. Please try again",result,"Attempted Blank Entry"
                setfocus Nord0006EditSearchS
          return
        endif
          insertitem Nord0006DataListSecondary,9999,str10
          setfocus Nord0006EditSearchS
          return
SecDelData
.Deletes data in datalist from secondary pick
          getitem Nord0006DataListSecondary,0,n2
          if (n2=c0)
                    alert type=yesno1," Do you want to delete all of the entries in the datalist?", result,"Clear DataList"
                              if (result=6)    . 6 = yes , 7 = no
                              deleteitem Nord0006DataListSecondary,n2
                              return
                        else
                              alert note,"Select a field in the Data Field, then click Remove.",result,"Select Field"
                                return
                              endif
          endif
          deleteitem Nord0006DataListSecondary,n2
          return

LostFocusSecondary
.For Lost Focus Event of Nord0006ComboSecondary
        clear n2
        clear n5
          getitem Nord0006ComboSecondary,n4,n3
          getitem Nord0006ComboSecondary,n3,str55
SecondaryChange
.For LostFocus of Nord0006ComboSecondary
        clear n2
        clear n5
          getitem Nord0006ComboSecondary,n4,n3
        compare N3 to HoldSEC
        if not equal
                deleteitem Nord0006DataListSecondary,c0
                    setitem Nord0006EditSearchS,0,""
                    setitem Nord0006StatClient2,0,""
                move N3 to HoldSEC
        endif
        return
MailerOfferReps
.Report Section for Mailer or Mailer/Offer Picks
          deleteitem Nord0006ComboReportType,c0
        clear n2
.        move  REPPIK to n2
        loop
                add  c1 to n2
                  Load str55 with n2,"1.  ---------------------------------------":
                              "2.  History of Mlr Usage -Alpha By List":                 
                              "3.  History of Mlr Usage -Alpha by Broker":               
                              "4.  History of Mlr Usage -Alpha by List(No Offer Break)": 
                              "5.  History of Mlr Usage -Seq. Order by Users selected Criteria":
                              "6.  History of Mlr Usage -Diskin Totals Only":                   
.                              "7.  Prepays by List":
                              "7.  History of Mlr Usage -w/ Shipping Information":                 
                              "8.  History of Mlr Usage -Alpha with Running Charge Orders": 
                              "9.  Mlr Usage by Month for a given year(Y)":                         
                              "10. History of Last 5 Yrs. of Usage by Mailer Or List":
                              "11. ---------------------------------------":
                              "12. Excel Diskin File -(Emailed to User)":
                              "13. History of Mlr Usage -Shipping by Maildate":
                              "14. ---------------------------------------":
                              "15. ---------------------------------------":
                              "16. ---------------------------------------":
                              "17. ---------------------------------------":
                              "18. ---------------------------------------":
                              "19. ---------------------------------------":
                              "20. Open Invoices":                               
                              "21. Create flat file of List Numbers from diskin":
                              "22. ---------------------------------------":
                              "23. Shipping By LO":                                                      
                              "24. Invoice Copies":                                                      
                              "25. Unbilled Report by LR":                                               
                              "26. List Cost Analysis(X Rep) -Print & Excel Spreadsheet":                                            
                              "27. List Cost Analysis(X Rep) -Print Only w/ Order Qty":                               
                              "28. List Cost Analysis(X Rep) -Download Only":                           
                              "29. Accounts Receivable Statement -ARSTATE":
                              "30. ---------------------------------------":                 
                              "31. ---------------------------------------":                                                      
                              "32. History of Mlr Usage -Separate Rental and Exchange Diskin Reports":
                              "33. ---------------------------------------":                                                      
                              "34. ---------------------------------------":                                                      
                              "35. ---------------------------------------":                                                      
                              "36. PDF'd Mailer copies of Orders          ":    
                              "37. PDF'd List Owner copies of Orders      "    
                              
.                              "24.  Alpha/Mailer Duplex","25.  NIN Paid/Payables -List Owner Income","26.  Shipping By LO","27.  Invoice Copies","28.  Unbilled","29.  REG X":
.                              "31.  Qty X","31.  Dwn X","32.  Arstate","33.  View List\Alphabetic by Client","34.  NMLRALD","35.  Numeric Duplex","36.  Qty X-List Cost Analysis- ON Net"
.         insertitem Nord0006ComboReportType,n3,str55
          insertitem Nord0006ComboReportType,9999,str55
.                sub    c1 from n2
          until (n2 =REPPIK)
.         until (n2 ="0")
        repeat
        return

OwnerListReps
.Report Section for Owner Picks and List Picks
          deleteitem Nord0006ComboReportType,c0
        clear n2
.        move  REPPIK to n2
        loop
                add    c1 to n2
                  Load str55 with n2,"1.  History of List Usage -Alpha By Mailer":
                                       "2.  ---------------------------------------":
                              "3.  ---------------------------------------":
                              "4.  ---------------------------------------":
                              "5.  ---------------------------------------":
                              "6.  ---------------------------------------":
.                              "7.  ---------------------------------------":
                              "7.  ---------------------------------------":
                              "8.  ---------------------------------------":
                              "9.  ---------------------------------------":
                              "10. History of Last 5 Yrs. of Usage by Mailer Or List":
                              "11. History of List Usage -View Diskin Totals Only (users directory)": 
                              "12. ---------------------------------------":
                              "13. ---------------------------------------":
                              "14. History of List Usage -By Order Date":                                                            
                              "15. History of List Usage -Diskin Totals Only":                         
                              "16. History of List Usage -by Mail Date":                               
                              "17. History of List Usage -w/ tax information by Order Date":           
                              "18. Alpha by List":                                                      
                              "19. Numeric":                                                            
                              "20. Open Invoices":                                                      
                              "21. Create flat file of List Numbers from diskin":                       
                              "22. NIN Paid/Payables/List Owner Income Reports":                        
                              "23. Shipping By LO":                                                     
                              "24. Invoice Copies":                                                     
                              "25. Unbilled Report by LR":                                              
                              "26. ---------------------------------------":
                              "27. ---------------------------------------":
                              "28. ---------------------------------------":
                              "29. ---------------------------------------":
                              "30. History of List Usage -View Alpha by Mlr":
                              "31. History of List Usage -By Order Date (Duplexed)":
                              "32. ---------------------------------------":
.patch5.12
                              "33.  History of List Usage --> Excel        ":
.patch5.12
                              "34. ---------------------------------------":                                                      
                              "35. ---------------------------------------":                                                      
                              "36. ---------------------------------------":                                                      
                              "37. PDF'd List Owner copies of Orders      "    
.                              "24.  Alpha/Mailer Duplex","25.  NIN Paid/Payables -List Owner Income","26.  Shipping By LO","27.  Invoice Copies","28.  Unbilled","29.  REG X":
.                              "31.  Qty X","31.  Dwn X","32.  Arstate","33.  View List\Alphabetic by Client","34.  NMLRALD","35.  Numeric Duplex","36.  Qty X-List Cost Analysis- ON Net"
.         insertitem Nord0006ComboReportType,n3,str55
          insertitem Nord0006ComboReportType,9999,str55
.                sub    c1 from n2
          until (n2 =REPPIK)
.         until (n2 ="0")
        repeat
        return
OtherReps
.Report Section for Misc
        deleteitem Nord0006ComboReportType,c0
        clear n2
.        move  REPPIK to n2
        loop
                add    c1 from n2
                  Load str55 with n2,"1.  History of List Usage -Alpha By Mlr":
                              "2.  History of Mlr Usage -Alpha By List":
                              "3.  History of Mlr Usage -Alpha by Broker":
                              "4.  History of Mlr Usage -Alpha by List(No Offer Break)":
                              "5.  History of Mlr Usage -Seq. Order by Users selected Criteria":
                              "6.  History of Mlr Usage -Diskin Totals Only":
.                                  "7.  Prepays by List":
                              "7.  History of Mlr Usage -w/ Shipping Information":
                              "8.  History of Mlr Usage -Alpha with Running Charge Orders":
                              "9.  Mailer Usage by Month for a given year(Y)":
                              "10. History of Last 5 Yrs. of Usage by Mailer Or List":
                              "11. History of List Usage -View Diskin Totals Only(users directory)":
                              "12. NMLRALF.XLSX - (C:\work)":
                              "13. History of Mlr Usage -w/ Shipping by Maildate":
                              "14. History of List Usage -By Order Date":
                              "15. History of List Usage -Diskin Totals Only":
                              "16. History of List Usage -by Mail Date":
                              "17. History of List Usage -w/ tax information by Order Date":
                              "18. Alpha by List":
                              "19. Numeric":
                              "20. Open Invoices":
                              "21. Create flat file of List Numbers from diskin":
                              "22. NIN Paid/Payables/List Owner Income Reports":
                              "23. Shipping By LO":
                              "24. Invoice Copies":
                              "25. Unbilled Report by LR":
                              "26. List Cost Analysis(X Rep) -Print & Excel Spreadsheet":                                            
                              "27. List Cost Analysis(X Rep) -Print Only w/ Order Qty":                               
                              "28. List Cost Analysis(X Rep) -Download Only":                           
                              "29. Accounts Receivable Statement- ARSTATE":
                              "30. History of List Usage -View Alpha by Mailer":
                              "31. History of List Usage -By Order Date (Duplexed)":
                              "32. History of Mlr Usage -Separate Rental and Exchange Diskin Reports":
.patch5.12
                              "33.  History of List Usage -By Mailer Flat file In Users Dir":
.patch5.12
                              "34. ---------------------------------------":                                                      
                              "35. ---------------------------------------":                                                      
                              "36. PDF'd Mailer copies of Orders          ":    
                              "37. PDF'd List Owner copies of Orders      "    
.                              "24.  Alpha/Mailer Duplex","25.  NIN Paid/Payables -List Owner Income","26.  Shipping By LO","27.  Invoice Copies","28.  Unbilled","29.  REG X":
.                              "31.  Qty X","31.  Dwn X","32.  Arstate","33.  View List\Alphabetic by Client","34.  NMLRALD","35.  Numeric Duplex","36.  Qty X-List Cost Analysis- ON Net"
          insertitem Nord0006ComboReportType,9999,str55
.                sub    c1 from n2
          until (n2 =REPPIK)
.         until (n2 ="0")
        repeat
        return
Day2
.Used during lost focus event Nord0006ComboMo2 to show correct Dates
.Do not have code in for feb leap year
          deleteitem Nord0006ComboDay2,c0
        clear n3
          getitem  Nord0006ComboMo2,n1,n2
.DLH 02/02/2012 temp fix for feb need to check for leap year
.begin patch 5.25
.        Load     NDD using N2,c31,c28,c31,c30,c31,c30,c31,c31,c30,c31,c30,c31
        Load     NDD using N2,c31,Nfeb,c31,c30,c31,c30,c31,c31,c30,c31,c30,c31
.end patch 5.25
        move     NDD to str2
        move     NDD to N10
        loop
                 call trim using str2
                        insertitem Nord0006ComboDay2,n3,str2
           until (NDD = c1)
                   sub c1 from NDD
            move NDD to str2
        repeat
               setitem  Nord0006ComboDay2,n1,N10
        return
Day1
.Used during lost focus event Nord0006ComboMo1 to show correct Dates
.Do not have code in for feb leap year
          deleteitem Nord0006ComboDay1,c0
        clear n3
          getitem  Nord0006ComboMo1,n1,n2
.begin patch 5.25
.        Load     NDD using N2,c31,c28,c31,c30,c31,c30,c31,c31,c30,c31,c30,c31
        Load     NDD using N2,c31,Nfeb,c31,c30,c31,c30,c31,c31,c30,c31,c30,c31
.end patch 5.25
        move     NDD to str2
        loop
                 call trim using str2
                        insertitem Nord0006ComboDay1,n3,str2
           until (NDD = c1)
                   sub c1 from NDD
            move NDD to str2
        repeat
          setitem  Nord0006ComboDay1,n1,c1
        return



PICKOFF
.begin patch 5.19
          
          getprop Nord0006RadioPL,Value=N1
          if        (n1 <> c1)
          move      NO,PLFlag
          Elseif    (n1 = c1)
          move      Yes,PLFlag
          endif
.end patch 5.19
          
          setprop Nord0006PickOff,enabled=c0
   setprop ColPrim,enabled=c0
          setprop ColSec,enabled=c0
          setprop ColRadate,enabled=c0
          setprop ColCombDate,enabled=c0
          setprop ColFilter,enabled=c0
          setprop ColPrint,enabled=c0
          getitem Nord0006ComboPrimary,n4,n3
.begin patch 5.23
.        if ((n3 = c8)|(n3 = c9)|(n3 = c11)|(n3 = c12)|(n3 = c13))
        if ((n3 = c8)|(n3 = c9)|(n3 = c11)|(n3 = c12)|(n3 = c13)|(n3 = "15"))
.end patch 5.23
                alert   type=Yesno1,"Do you want to start at the beginning of the file?",result,"Start from Beginning"
          if (result = c7)         .NO
                        call FormBeginLR
                                setprop Report2,visible=1
                        IF (RPTCAN = YES)
                                                     call livebut
                                return
                        ENDIF
BEGLRVAL
                        If (LRNO = "")
                                move YES to ALL
                        else
                                move NO to ALL
                        endif
                        goto reporting
                     else
                        move YES to ALL
                        goto reporting
                     endif
        endif
          getitem Nord0006ComboPrimary,n3,str55
        getitem Nord0006DataListPrimary,c1,n2
        if (n2=c0)
                alert          caution,"Please Enter Valid Data for Primary Search!",result,"Data"
                call     livebut
                return
        endif
.Check to see if doing a secondary search
          getitem Nord0006ComboSecondary,n4,n3
        if (n3=c0)
                  move NO to SECOND
        else
                  move n3 to SECTRACE
                         getitem Nord0006ComboSecondary,n3,str55
            scan    "*" in str55
                  If equal
                            move NO to SECOND
           else
                            getitem Nord0006DataListPrimary,c1,n2
                            if (n2=c0)
                              alert      caution,"Please Enter Valid Data for Secondary Search!",result,"Data"
                                   call     livebut
                              return
              endif
              move YES to SECOND              .Yes secondary search is applicable
                  endif
        endif
.Get Report Type
Reporting
          getitem Nord0006ComboReportType,n4,n3
        if (n3 = c0)
                alert          caution,"Please choose a valid Report Type!",result,"Choose Report"
                call     livebut
                setfocus Nord0006ComboReportType
                return
        endif
          getitem Nord0006ComboReportType,n3,str55
        scan "--------" in str55
.        scan    "NOT" in str55
        if equal
                alert          caution,"Please choose a valid Report Type!",result,"Choose Report"
                call     livebut
                setfocus Nord0006ComboReportType
                return
        endif

   getprop Nord0006RadioAll,SELGROUPID=DateGROUP        0-all 1-Order 2-Rental  3-Return
          getitem Nord0006ComboMo1,n4,n3

        if (n3=c0)
                alert          caution,"Please select a valid date!",result,"Select Valid Date"
                call livebut
                setfocus Nord0006ComboMo1
                return
        endif
               getitem Nord0006ComboYear1,n4,n3
               getitem Nord0006ComboYear1,n3,str4
        move    str4 to N6
               getitem Nord0006ComboYear2,n4,n3
               getitem Nord0006ComboYear2,n3,str4
        move    str4 to N7
        if (N6 > N7)
                alert          caution,"To Date is greater than From Date!",result,"Select Valid Date"
                call     livebut
                       setfocus Nord0006ComboYear1
                return
        endif
        if (N6 = N7)
.patch5.05
.changed var from n3 to n4, added n1
                              clear  n1
                              clear n4
.                             getitem Nord0006ComboMo1,n3,n4
                              getitem Nord0006ComboMo1,n1,n4
.                             getitem Nord0006ComboMo1,n3,str2
                              getitem Nord0006ComboMo1,n4,str2
          clear n4
.                move str2 to n3
                move str2 to n4
.patch5.05
                              clear n1
.                             getitem Nord0006ComboMo2,n4,n3
                              getitem Nord0006ComboMo2,n1,n3
.patch5.05
                              getitem Nord0006ComboMo2,n3,str2
                move str2 to n8
.patch5.05
                if (N4 > N8)
.patch5.05
                          alert          caution,"To Date is greater than From Date!",result,"Select Valid Date"
                          call     livebut
                                 setfocus Nord0006ComboMo1
                   return
                endif
.patch5.05
                if (N4 = N8)
.patch5.05
                                             getitem Nord0006ComboDay1,n4,n3
                                             getitem Nord0006ComboDay1,n3,str2
                        move str2 to n6
                                             getitem Nord0006ComboDay2,n4,n3
                                             getitem Nord0006ComboDay2,n3,str2
                        move str2 to n7
                        if (n6 > n7)
                                             alert           caution,"To Date is greater than From Date!",result,"Select Valid Date"
                                             call     livebut
                                                    setfocus Nord0006ComboDay1
                               return
                        endif
                endif
        endif

                      getitem Nord0006ComboCOPY,c0,COPY
                      if (copy = "")
                                                  alert type=yesno1,"You have selected no copies.  Are you Sure?", result,"Zero copies"
                                if (result=7)    . 6 = yes , 7 = no
                                    call     livebut
                                                                setfocus Nord0006ComboCOPY
                                                                return
                                endif
                                move "0" to copy
                      endif
.patch5.04
                    getitem Nord0006ComboReportType,n4,n3

.Yreport
                    if (n3 = c9)        
test2000
                              move c1 to selrep
                    call      OrderPickGetCriteria using SelRep,REPNUM,TEAMDWN,PDFOPT,FISCMO,DateOPT
                              if (selrep = c0)
                                    alert caution,"You did not pick any criteria to run these reports!  Please try Again!",result,""
                                    call     livebut
                                              return
                    endif
          endif
.Mailer Summary

                    if (n3 = c10)
                              move c2 to selrep
                    call      OrderPickGetCriteria using SelRep,REPNUM,TEAMDWN,PDFOPT,FISCMO,DateOPT
                              if (selrep = c0)
                                    alert caution,"You did not pick any criteria to run these reports!  Please try Again!",result,""
                                    call     livebut
                                              return
                    endif
          endif

.Test to see if Both PDF/Print is an option
.patch5.10
test
          if ((n3 = c9)|(n3=c10)|(n3=c11)|(n3="15")|(n3="32")|(n3="33"))
                    getitem Nord0006ComboBox001,n1,pdfflag
                    if (PDFFLAG = C3)
                                              alert caution,"You cannot select BOTH option with this report!",result,""
                    endif
          endif
.patch5.10


.NEED TO ADD CODE TO CHECK IF CANCELLED
.                    if (selrep = c0)
.                         alert caution,"You did not pick any criteria to run these reports!  Please try Again!",result,""
.                         call     livebut
.                                    return
.         endif
.patch5.04


                move NO to exitflag
                call ordersetmousebusy
FileCreate
.Creation of Diskin File
         MOVE      "01",FILENUM
FILENAME CLEAR     NEWNAME
         APPEND    "DISKIN",NEWNAME
         MOVE      FILENUM,F2
         REP       " 0",F2
         APPEND    F2,NEWNAME
         RESET     NEWNAME TO 8
         RESET     NEWNAME
         TRAP      GOODFILE GIVING ERROR IF IO
         OPEN      RECMST,NEWNAME
         CLOSE     RECMST
ADDFILE  ADD       "1",FILENUM
         GOTO      FILENAME
GOODFILE TRAPCLR   IO
         NORETURN
         SCAN      "0030-0031" IN ERROR
         GOTO      ADDFILE IF EQUAL
         RESET     ERROR
         SCAN      "I * Y" IN ERROR
         GOTO      ADDFILE IF EQUAL
         reset     error
         SCAN      "I10" IN ERROR
         GOTO      ADDFILE IF EQUAL
         CLEAR     RECNAME
         IFNZ      PC
         APPEND    NEWNAME,RECNAME
         APPEND    "/TEXT:PRINT",RECNAME
         XIF
         IFZ       PC
         APPEND    "\\nins1\e\data\",RECNAME                                 ."
         APPEND    NEWNAME,RECNAME
         XIF
         RESET     RECNAME
         MOVE      B1 TO ERROR
.begin patch 5.21
.         PREPARE   RECMST,RECNAME,CREATE
          Pack      Taskname from "e:\data\",Newname,"|NINS1:502"                       ."
         PREPARE   RECMST,taskname,Exclusive
          Clear     Taskname
.end patch 5.21
         setitem   Nord0006DiskinDisplay,c0,newname
         setprop   Nord0006Stop,enabled=c1
         clear n2
         GETITEM Nord0006DataListPrimary,c1,n2
         move    n2 to PRIMLIST                  .Number of selections in datalist
         call    GETDATE
         clear   N9
         getitem Nord0006ComboPrimary,n4,n3
        move    n3 to PRIMTRACE                 .What are we catagory are we searching under?




        Branch  PRIMTRACE,NEXTMAILER,NEXTLIST,NEXTLR,NEXTOFFER,NEXTOWNER,NEXTBROKER,NEXTRETURN:
.patch5.11
.         NEXTCOMSELECT,NEXTSLSPERSON,NEXTBRKCNT,NEXTBRKGUAR,NEXTNINGUAR,NEXTDATES
.begin patch 5.23
.          NEXTCOMSELECT,NEXTSLSPERSON,NEXTBRKCNT,NEXTBRKGUAR,NEXTNINGUAR,NEXTDATES,NEXTCAMPAIGN
          NEXTCOMSELECT,NEXTSLSPERSON,NEXTBRKCNT,NEXTBRKGUAR,NEXTNINGUAR,NEXTDATES,NEXTCAMPAIGN,NextFullFil
.end patch 5.23
.patch5.11
NextMailer
        loop
                add c1 to N9
        until (N9 > PRIMLIST)
                        GETITEM Nord0006DataListPrimary,n9,str10
                        Clear NORDFLD1
                        Clear NORDFLD2
                        Clear NORDFLD3
                        Clear NORDFLD4
                        move  str10 to mlrno
              PACK  NORDFLD1 FROM KEYPAD1,MLRNO
              MOVE  C2 TO NORDPATH
                        CALL  NORDAIM
                        goto  NextMailer if over
                        add   c1 to RECORDS
..                                                 call readrecs
..                goto  CompleteMailerSearch
                        goto  CHKDATES
        repeat
        goto   PrinterOpt
..        goto    Ender
..CompleteMailerSearch
..       loop
...               call CHKDATES
...              if (second = YES)
.A call to the label for a secondary pick
...                       Branch SECTRACE,SECMLR,SECLST
..                                        PERFORM SECTRACE,SECMLR,SECLST,SECOFFR,SECOWN,SECBROK:
..                                  SECRTN,SECCOM,SECSLS,SECCAT,SECEXCL,MLRKEY,MLRPO
...              else
...                        goto writeit
...              endif
MailerSearch
        call checkstop
        call NORDKG
..                                         add   c1 to RECORDS
..                                                call readrecs
        goto NextMailer if over
        goto CHKDATES
.               goto CompleteMailerSearch
.       until over
.       repeat
.====================================================================================
NextList
        loop
                add c1 to N9
        until (N9 > PRIMLIST)
                GETITEM Nord0006DataListPrimary,n9,str10
          CLEAR     NORDFLD2
          CLEAR     NORDFLD1
          CLEAR     NORDFLD3
          clear     nordfld4
          move      str10 to lstno
          PACK      NORDFLD1 FROM KEYPAD1,QUES
          PACK      NORDFLD2 FROM KEYPAD2,LSTNO
          MOVE      C2 TO NORDPATH
          CALL      NORDAIM
                    GOTO      NextList if over
.                   add       c1 to RECORDS
.         call      readrecs
                    goto      CHKDATES
        repeat
        goto    PrinterOpt
.       goto    Ender
ListSearch
          call checkstop
          call NORDKG
                    goto NextList if over
..                    add   c1 to RECORDS
..                    call readrecs
                    goto CHKDATES
.====================================================================================
NextLr
.patch5.03
       move c0 to dategroup
.patch5.03
        loop
                add c1 to N9
        until (N9 > PRIMLIST)
                GETITEM Nord0006DataListPrimary,n9,str10
                    CLEAR     NORDFLD2
                    CLEAR     NORDFLD1
                    CLEAR     NORDFLD3
                    clear     nordfld4
                    move      str10 to LRNO
                    MOVE      LRNO TO NORDFLD
                    MOVE      C1 TO NORDPATH
          call checkstop
          CALL      NORDKEY
                    goto      NextLr if over
.         add   c1 to RECORDS
.                             call  readrecs
                    CMATCH    " " TO OEXQTY
                    IF NOT EOS
                        rep zfill in OEXQTY
                    ENDIF
                    goto SECONDSEARCH
         repeat
         goto   PrinterOpt
.        goto    Ender
.=================================================================================
NextOffer
        loop
                add c1 to N9
        until (N9 > PRIMLIST)
                   GETITEM Nord0006DataListPrimary,n9,str10
                   move      str10 to OFRNO
                   MOVE      OFRNO,MLRNO
                   CLEAR     NORDFLD1
                   CLEAR     NORDFLD2
                   CLEAR     NORDFLD3
                   clear     nordfld4
                   PACK      NORDFLD1 FROM KEYPAD1,MLRNO
                   MOVE      C2 TO NORDPATH
                   CALL      NORDAIM
                   goto      NextOffer if over
..          add       c1 to RECORDS
..          call      readrecs
                    MATCH     OMLRNUM TO MLRNO
                    GOTO      OfferSearch IF NOT EQUAL
                    MATCH     OODNUM TO OFRNO
                    GOTO      CHKDATES IF EQUAL
                    GOTO      OFFERSEARCH
        repeat
        goto        PrinterOpt
..        goto    Ender
OfferSearch
        call      checkstop
        call      NORDKG
        goto      NextOffer if over
..  add       c1 to RECORDS
..  call      readrecs
        MATCH     OMLRNUM TO MLRNO
        GOTO      OfferSearch IF NOT EQUAL
        MATCH     OODNUM TO OFRNO
        GOTO      CHKDATES IF EQUAL
        GOTO      OFFERSEARCH
.====================================================================================
NextOwner
.Sequetial Read that checks all entries in pick
        CLEAR     NORDFLD1
        CLEAR     NORDFLD2
        CLEAR     NORDFLD3
        CLEAR     NORDFLD4
        MOVE      C1 TO NORDPATH
        call      checkstop
        CALL      NORDSEQ
        GOTO      PrinterOpt if over
..        add       c1 to RECORDS
..        call      readrecs
..       GOTO      Ender if over
        clear     N9
OwnerSearch
        loop
                    add c1 to N9
        until (N9 > PRIMLIST)
                    GETITEM Nord0006DataListPrimary,n9,str10
                    MOVE      str10 to OWNRNO
                    Match     OLON,OWNRNO
          GOTO      CHKDATES IF EQUAL
        repeat
        goto    NextOwner
.=================================================================================
NextBroker
        loop
                add c1 to N9
        until (N9 > PRIMLIST)
                GETITEM Nord0006DataListPrimary,n9,str10
                    Clear NORDFLD1
                    Clear NORDFLD2
                    Clear NORDFLD3
                    Clear NORDFLD4
                    move  str10 to brkno
          PACK  NORDFLD4 FROM KEYPAD4,BRKNO
          MOVE  C2 TO NORDPATH
          CALL  NORDAIM
          goto  NextBroker if over
..         add   c1 to RECORDS
..         call readrecs
                    goto  CHKDATES
        repeat
        goto    PrinterOpt
.        goto    Ender
BrokerSearch
        call checkstop
        call NORDKG
        goto NextBroker if over
..          add   c1 to RECORDS
..                             call readrecs
        goto CHKDATES
.====================================================================================
NextReturn
.Sequetial Read that checks all entries in pick
        CLEAR     NORDFLD1
        CLEAR     NORDFLD2
        CLEAR     NORDFLD3
        CLEAR     NORDFLD4
        MOVE      C1 TO NORDPATH
        call      checkstop
        CALL      NORDSEQ
        GOTO      PrinterOpt if over
..       add       c1 to RECORDS
..       call      readrecs
.       GOTO      Ender if over
        clear     N9
ReturnSearch
        loop
                add c1 to N9
        until (N9 > PRIMLIST)
                    GETITEM Nord0006DataListPrimary,n9,str10
                    MOVE      str10 to RETURN
                    Match     ORTNNUM,RETURN
          GOTO      CHKDATES IF EQUAL
        repeat
        goto    NextReturn

NextComselect
         CMATCH    YES,ALL
         GOTO      ComselectSearch2 IF EQUAL
         MOVE      LRNO TO NORDFLD
         MOVE      C1 TO NORDPATH
         CALL      NORDKEY
         GOTO      PRINTEROPT IF OVER
..         add       c1 to RECORDS
..         call      readrecs
..         GOTO      ENDER IF OVER
         CMATCH    "C",OCOMSLCT
         GOTO      CHKDATES IF EQUAL
         GOTO      ComselectSearch
ComselectSearch
         CMATCH    YES,ALL
         GOTO      ComselectSearch2 IF EQUAL
         MOVE      C1 TO NORDPATH
         call      checkstop
.         CALL     
         GOTO      PrinterOpt IF OVER
..         add       c1 to RECORDS
..                            call      readrecs
..         GOTO      ENDER IF OVER
         CMATCH    "C",OCOMSLCT
         GOTO      CHKDATES IF EQUAL
         GOTO      ComselectSearch
ComselectSearch2
         MOVE      C1 TO NORDPATH
         call      checkstop
         CALL      NORDSEQ
         GOTO      PrinterOpt IF OVER
..         add       c1 to RECORDS
..         call      readrecs
..         GOTO      ENDER IF OVER
         CMATCH    "C",OCOMSLCT
         GOTO      CHKDATES IF EQUAL
         GOTO      ComselectSearch2
NextSLSperson
         CMATCH    YES,ALL
         GOTO      SLSpersonsearch2 IF EQUAL
         MOVE      C1 TO NORDPATH
         MOVE      LRNO TO NORDFLD
         CALL      NORDTST
         Clear     N9
         MOVE      C1 TO NORDPATH
         MOVE      LRNO TO NORDFLD
         CALL      NORDKEY
         GOTO      PRINTEROPT IF OVER
..         add       c1 to RECORDS
..         call      readrecs
..         GOTO      ENDER IF OVER
slspersonsearch1
         CMATCH    YES,ALL
         GOTO      slspersonsearch2 IF EQUAL
         MOVE      C1 TO NORDPATH
         call      checkstop
         CALL      NORDKS
..         add       c1 to RECORDS
..                     call readrecs
         GOTO      printeropt IF OVER
..         GOTO      ENDER IF OVER
         GOTO      slspersonsearch
slspersonsearch2
         MOVE      C1 TO NORDPATH
         call      checkstop
         CALL      NORDSEQ
         GOTO      printeropt IF OVER
..         add       c1 to RECORDS
..                            call      readrecs
..         GOTO      ENDER IF OVER
         GOTO      slspersonsearch
SlspersonSearch
         Clear     N9
         loop
                add c1 to N9
         until (N9 > PRIMLIST)
          GETITEM Nord0006DataListPrimary,n9,str10
                    move      str10 to sales
          PACK      SALES2 FROM OSALES10,OSALES
          MATCH     SALES2 TO SALES
                    GOTO      CHKDATES IF EQUAL
         repeat
             CMATCH    YES,ALL
             GOTO      slspersonsearch2 IF EQUAL
             GOTO      slspersonsearch1

NEXTBRKCNT
        loop
                add c1 to N9
        until (N9 > PRIMLIST)
                GETITEM Nord0006DataListPrimary,n9,str10
                Unpack str10,BRKNO,CONTACT
                Clear NORDFLD1
                Clear NORDFLD2
                Clear NORDFLD3
                Clear NORDFLD4
                PACK  NORDFLD4 FROM KEYPAD4,BRKNO
                MOVE  C2 TO NORDPATH
                CALL  NORDAIM
                goto  NextBrkcnt if over
..                add   c1 to RECORDS
..                                                 call readrecs
                MATCH OBRKCNT,CONTACT
                goto  CHKDATES if equal
                goto  BRKCNTSearch
        repeat
        goto    printeropt
.       goto    Ender
BRKCNTSearch
        call   checkstop
        call   NORDKG
        goto   NEXTBRKCNT if over
..       add    c1 to RECORDS
..       call   readrecs
        unpack cntno,b4,str3
        MATCH  OBRKCNT,CONTACT
        GOTO   CHKDATES if equal
        GOTO   BRKCNTSearch
NextBrkGuar
         CMATCH    YES,ALL
         GOTO      BrkGuarSearch2 IF EQUAL
         MOVE      LRNO TO NORDFLD
         MOVE      C1 TO NORDPATH
         CALL      NORDKEY
         GOTO      printeropt IF OVER
..         ADD       c1 to RECORDS
..         call      readrecs
..         GOTO      ENDER IF OVER
         CMATCH    b1,obrkguar
         GOTO      CHKDATES IF NOT EQUAL
         GOTO      BrkGuarSearch
BrkGuarSearch
         CMATCH    YES,ALL
         GOTO      BrkGuarSearch2 IF EQUAL
         MOVE      C1 TO NORDPATH
         call      checkstop
         CALL      NORDKS
         GOTO      printeropt IF OVER
..         ADD       c1 to RECORDS
..         call      readrecs
..         GOTO      ENDER IF OVER
         CMATCH    b1,obrkguar
         GOTO      CHKDATES IF NOT EQUAL
         GOTO      BrkGuarSearch
BrkGuarSearch2
         MOVE      C1 TO NORDPATH
         call      checkstop
         CALL      NORDSEQ
         GOTO      printeropt IF OVER
..         ADD       c1 to records
..                            call      readrecs
..         GOTO      ENDER IF OVER
         CMATCH    b1,obrkguar
         GOTO      CHKDATES IF NOT EQUAL
         GOTO      BrkGuarSearch2

NEXTNINGUAR
         CMATCH    YES,ALL
         GOTO      ninguarSearch2 IF EQUAL
         MOVE      LRNO TO NORDFLD
         MOVE      C1 TO NORDPATH
         CALL      NORDKEY
         GOTO      printeropt IF OVER
..         ADD       c1 to records
..         call      readrecs
..         GOTO      ENDER IF OVER
         CMATCH    "C",OCOMSLCT
         GOTO      CHKDATES IF EQUAL
         GOTO      ninguarSearch
NINGUARSearch
         CMATCH    YES,ALL
         GOTO      ninguarSearch2 IF EQUAL
         MOVE      C1 TO NORDPATH
         call      checkstop
         CALL      NORDKS
         GOTO      Printeropt IF OVER
..         GOTO      ENDER IF OVER
..                            ADD       c1 to records
..         call      readrecs
         CMATCH    b1,guarcode
         GOTO      CHKDATES IF NOT EQUAL
         GOTO      ninguarSearch
NINGUARSearch2
         MOVE      C1 TO NORDPATH
         call      checkstop
         CALL      NORDSEQ
         GOTO      printeropt IF OVER
..         GOTO      ENDER IF OVER
..         ADD       c1 to records
..         call      readrecs
         CMATCH    b1,guarcode
         GOTO      CHKDATES IF NOT EQUAL
         GOTO      ninguarSearch2
.Patch5.01
NEXTDATES
         CMATCH    YES,ALL
         GOTO      DATESSearch2 IF EQUAL
         MOVE      LRNO TO NORDFLD
         MOVE      C1 TO NORDPATH
         CALL      NORDKEY
         GOTO      printeropt IF OVER
         GOTO      CHKDATES
         GOTO      DATESSearch
DatesSearch
         CMATCH    YES,ALL
         GOTO      DatesSearch2 IF EQUAL
         MOVE      C1 TO NORDPATH
         call      checkstop
         CALL      NORDKS
         GOTO      Printeropt IF OVER
         GOTO      CHKDATES
         GOTO      DatesSearch
DatesSearch2
         MOVE      C1 TO NORDPATH
         call      checkstop
         CALL      NORDSEQ
         GOTO      printeropt IF OVER
         GOTO      CHKDATES
.EndPatch5.01
.=================================================================================
.patch5.11
NextCampaign
        loop
                add c1 to N9
        until (N9 > PRIMLIST)
                GETITEM Nord0006DataListPrimary,n9,str6
          call      zfillit using str6,c0
                    Clear NORDFLDC
                    move  str6 to nordfldC
          MOVE  C4 TO NORDPATH
          CALL  NORDKEY
          goto  NextCampaign if over
          move NORDFLDC to holdcamp
..         add   c1 to RECORDS
..         call readrecs
                    goto  CHKDATES
        repeat
        goto    PrinterOpt
.        goto    Ender
CampaignSearch
        call checkstop
        call NORDKS
        goto NextCampaign if over
        goto nextcampaign if (holdcamp <> OCAMP)
..          add   c1 to RECORDS
..                             call readrecs
        goto CHKDATES
.patch5.11
.begin patch 5.23
NEXTFULLFIL
         CMATCH    YES,ALL
         GOTO      Fullfilsearch2 IF EQUAL
         MOVE      C1 TO NORDPATH
         MOVE      LRNO TO NORDFLD
         CALL      NORDTST
         Clear     N9
         MOVE      C1 TO NORDPATH
         MOVE      LRNO TO NORDFLD
         CALL      NORDKEY
         GOTO      PRINTEROPT IF OVER
Fullfilsearch1
         CMATCH    YES,ALL
         GOTO      Fullfilsearch2 IF EQUAL
         MOVE      C1 TO NORDPATH
         call      checkstop
         CALL      NORDKS
         GOTO      printeropt IF OVER
         GOTO      Fullfilsearch
Fullfilsearch2
         MOVE      C1 TO NORDPATH
         call      checkstop
         CALL      NORDSEQ
         GOTO      printeropt IF OVER
         GOTO      Fullfilsearch
FullfilSearch
         Clear     N9
         loop
                add c1 to N9
         until (N9 > PRIMLIST)
          GETITEM Nord0006DataListPrimary,n9,str6
          IF        (STR6 = OFullFil)
          GOTO      CHKDATES
          ENDIF
         repeat
             CMATCH    YES,ALL
             GOTO      Fullfilsearch2 IF EQUAL
             GOTO      Fullfilsearch1
.end patch 5.23
.====================================================================================


.=================================================================================
CHKDATES
.begin patch 5.19

.Cheat and add PL suppress here
          IF        (PLflag = Yes)                .suppress PL
                    Type      Olrn
                    if        Not equal
                    goto      ReCycle
                    endif
          Endif     
.end patch 5.19

.if not all dates
          if (DateGroup <> c0)
                     call      OMRDate
                            COMPARE   KEEPDATE1 TO juldays
                            GOTO      RECYCLE IF LESS              NO. TRY NEXT ORDER.
..                          GOTO      MailerSearch IF LESS              NO. TRY NEXT ORDER.
                            COMPARE   juldays TO KEEPDATE2
                            GOTO      RECYCLE IF LESS              NO. TRY NEXT ORDER.
..                          GOTO      MailerSearch IF LESS
          endif
          goto    SECONDSEARCH
..              call CHKDATES
SECONDSEARCH
                 if (second = YES)
.A call to the label for a secondary pick
                       Branch SECTRACE,SECMLR,SECLST,SECOFFER,SECOWNER,SECBROKER:
                              SECRETURNTO,SECCOMM,SECSALES,SECCAT,SECEXCAT,SECMAILERKEY:
                              SECMAILERPO
                 else
                 goto writeit
                 endif
GetDate
.Beginning Date
        getitem Nord0006ComboYear1,n4,n3
        getitem Nord0006ComboYear1,n3,str4
        unpack str4 to CC,YY
        getitem Nord0006ComboMo1,n4,n3
        getitem Nord0006ComboMo1,n3,str2
        move str2 to MM
        call zfillit using mm
        getitem Nord0006ComboDay1,n4,n3
        getitem Nord0006ComboDay1,n3,str2
        move str2 to DD
        call zfillit using DD
        call cvtjul
        move juldays to KEEPDATE1
.Ending Date
        getitem Nord0006ComboYear2,n4,n3
        getitem Nord0006ComboYear2,n3,str4
        unpack str4 to CC,YY
        getitem Nord0006ComboMo2,n4,n3
        getitem Nord0006ComboMo2,n3,str2
        move str2 to MM
        call zfillit using mm
        getitem Nord0006ComboDay2,n4,n3
        getitem Nord0006ComboDay2,n3,str2
        move str2 to DD
        call zfillit using DD
        call cvtjul
        move juldays to KEEPDATE2
        return

OMRDate
.get date from order
         branch  dategroup to ORD,Mail,Rtrn
         return
Ord      MOVE      OODTEM,MM
         MOVE      OODTEY,YY
         MOVE      OODTED,DD
         call      cvtjul
         return
Mail     MOVE      OMDTEM,MM
         MOVE      OMDTEY,YY
         MOVE      OMDTED,DD
         call          cvtjul
         return
Rtrn     MOVE      ORTNDTEM,MM
         MOVE      ORTNDTEY,YY
                MOVE              ORTNDTED,DD
         call          cvtjul
         return
SECMLR
         clear n2
         GETITEM Nord0006DataListSecondary,c1,n2
         for n3,"1" to N2
                   GETITEM Nord0006DataListSecondary,n3,str10
                    move str10 to MLRNO
                    MATCH MLRNO,OMLRNUM
          IF EQUAL
                              goto WRITEIT
                     endif
         repeat
         goto   RECYCLE
         return
SECLST
         clear n2
         GETITEM Nord0006DataListSecondary,c1,n2
         for n3,"1" to N2
          GETITEM Nord0006DataListSecondary,n3,str10
                    move str10 to LSTNO
                    MATCH LSTNO,OLNUM
          IF EQUAL
                    goto WRITEIT
                    endif
         repeat
         goto   RECYCLE
SECOFFER
         clear n2
         GETITEM Nord0006DataListSecondary,c1,n2
         for n3,"1" to N2
          GETITEM Nord0006DataListSecondary,n3,str10
                    move  str10 to OFRNO
                    MATCH OODNUM TO OFRNO
          IF EQUAL
                          goto WRITEIT
                    endif
        repeat
        goto   RECYCLE
SECOWNER
        clear n2
        GETITEM Nord0006DataListSecondary,c1,n2
        for n3,"1" to N2
          GETITEM Nord0006DataListSecondary,n3,str10
                    MOVE      str10 to OWNRNO
                    Match     OLON,OWNRNO
          IF EQUAL
                    goto WRITEIT
                    endif
        repeat
        goto   RECYCLE
SECBROKER
        clear n2
        GETITEM Nord0006DataListSecondary,c1,n2
        for n3,"1" to N2
          GETITEM Nord0006DataListSecondary,n3,str10
          MOVE      str10 to BRKNO
          Match     OBRKNUM,BRKNO
          IF EQUAL
                    goto WRITEIT
                    endif
        repeat
        goto   RECYCLE
SECRETURNTO
        clear n2
        GETITEM Nord0006DataListSecondary,c1,n2
        for n3,"1" to N2
          GETITEM Nord0006DataListSecondary,n3,str10
                    MOVE      str10 to RETURN
                    Match     ORTNNUM,RETURN
                    IF EQUAL
                    goto WRITEIT
                    endif
        repeat
        goto   RECYCLE
SECCOMM
        CMATCH    "C",OCOMSLCT
        GOTO      WRITEIT IF EQUAL
        CMATCH    "L",OCOMSLCT
        GOTO      WRITEIT IF EQUAL
        CMATCH    "I",OCOMSLCT
        GOTO      WRITEIT IF EQUAL
        goto   RECYCLE
SECSALES
               clear n2
               GETITEM Nord0006DataListSecondary,c1,n2
        for n3,"1" to N2
          GETITEM Nord0006DataListSecondary,n3,str10
          move      str10 to sales
          PACK      SALES2 FROM OSALES10,OSALES
          MATCH     SALES2 TO SALES
          IF EQUAL
                    goto WRITEIT
          endif
      repeat
      goto   RECYCLE
SECCAT
.START PATCH 5.11A REPLACED LOGIC
.        MOVE      OLNUM TO NDATFLD
.        CALL      NDATKEY
.        PACK      CATCODES FROM CATCDE1,CATCDE2,CATCDE3,CATCDE4,CATCDE5:
.                  CATCDE6,CATCDE7,CATCDE8,CATCDE9,CATCDE10
          CATCODES.ResetContent         .Clear DataList
          pack      NCATFLD1,"01X",LSTNUM
          move      "NCATAIM",Location
          pack      KeyLocation,"Key: ",NCATFLD1
          call      NCATAIM
          loop
                    until over
                    pack      str3,NCATCODE,NCATNUM
                    CATCODES.AddString using str3
                    move      "NCATKG",Location
                    pack      KeyLocation,"Key: ",NCATFLD1
                    call      NCATKG
          repeat
.END PATCH 5.11A REPLACED LOGIC
        clear n2
        GETITEM Nord0006DataListSecondary,c1,n2
        for n3,"1" to N2
          GETITEM Nord0006DataListSecondary,n3,str10
                    move      str10 to CATAGRY
.START PATCH 5.11A REPLACED LOGIC
.                    SCAN      CATAGRY IN CATCODES
.                    IF EQUAL
.                    CATCODES.FindStringExact giving result using CATAGRY
.DLH Jan 26 2011
                    CATCODES.FindStringExact giving result using CATAGRY,C0
                    if (result <> SEQ)
.END PATCH 5.11A REPLACED LOGIC
                    goto WRITEIT
                    endif
        repeat
        goto   RECYCLE
SECEXCAT
.START PATCH 5.11A REPLACED LOGIC
.        MOVE      OLNUM TO NDATFLD
.        CALL      NDATKEY
.        PACK      CATCODES FROM CATCDE1,CATCDE2,CATCDE3,CATCDE4,CATCDE5:
.                  CATCDE6,CATCDE7,CATCDE8,CATCDE9,CATCDE10
          CATCODES.ResetContent         .Clear DataList
          pack      NCATFLD1,"01X",LSTNUM
          move      "NCATAIM2",Location
          pack      KeyLocation,"Key: ",NCATFLD1
          call      NCATAIM
          loop
                    until over
                    pack      str3,NCATCODE,NCATNUM
                    CATCODES.AddString using str3
                    move      "NCATKG2",Location
                    pack      KeyLocation,"Key: ",NCATFLD1
                    call      NCATKG
          repeat
.END PATCH 5.11A REPLACED LOGIC
        clear n2
        GETITEM Nord0006DataListSecondary,c1,n2
        for n3,"1" to N2
                    GETITEM Nord0006DataListSecondary,n3,str10
                    move      str10 to CATAGRY
.START PATCH 5.11A REPLACED LOGIC
.         SCAN      CATAGRY IN CATCODES
.         IF EQUAL
                    CATCODES.FindStringExact giving result using CATAGRY,C0
                    if (result <> SEQ)
.END PATCH 5.11A REPLACED LOGIC
                    goto RECYCLE
                    endif
        repeat
        goto   RECYCLE

SECMAILERKEY
        clear n2
        GETITEM Nord0006DataListSecondary,c1,n2
        for n3,"1" to N2
          GETITEM Nord0006DataListSecondary,n3,rec12
                    Match     OMLRKY,rec12
                    goto writeit if equal
        repeat
        goto   RECYCLE

SECMAILERPO
        clear n2
        GETITEM Nord0006DataListSecondary,c1,n2
        for n3,"1" to N2
          GETITEM Nord0006DataListSecondary,n3,rec12
                    Match OMLRPON,rec12
                    goto  writeit if equal
        repeat
        goto   RECYCLE
WRITEIT
.begin patch 5.28
.check for cancelled being desired if not skip here
.          getitem Nord0006CheckExclude,0,str1 ---- this check box hidden and not in use? use the radio control
.2014 Dec 1 also drop cancelled pendings and LCRS
.          if        (str1 = "1" & (ostat = "X" or OStat = "Q"))
.2015 December 29 temp fix for cost savings
               getitem Nord0006ComboReportType,n4,reptype

.          if        (Exclude = "E" & (ostat = "X" or OStat = "Q" or OStat = "x" or OStat = "z"))
           if        (reptype = "26" or reptype = "27" or reptype = "28" or reptype = "29")
           
          Elseif        (Exclude = "E" & (ostat = "X" or OStat = "Q" or OStat = "x" or OStat = "z"))
.2015 December 29 temp fix for cost savings
.end patch 5.28
          goto      recycle
          endif
.end july 10 2012 dlh          
         ADD       "1",FOUND
         move      found to str9
         move      found to str9
         setitem   Nord0006StatRecords,0,str9
WRTALPH
         PACK      MKEY FROM OMLRNUM,OCOBN
         MATCH     MKEY,HOLDMLR                 *same mlr?
         GOTO      WRTALPH2 IF EQUAL           *yes, no mlr read needed.
         MOVE      MKEY,HOLDMLR                 *no, get mlr
         CALL      NMLRKEY
         MOVE      MCOMP TO BCNAME      *was remmed for patch #4.3
.patch 31 march 2008 --- mccto is dead
.         PACK      mNAME FROM MCCTO,B6,CAREOF
         clear     nbrkfld
         move      c1 to nbrkpath
         match     obrknum to holdbrk
         goto      wrtalph2 if equal
         move      obrknum to holdbrk
         pack      nbrkfld from obrknum,z3
         cmatch    b1 to nbrkfld
         if        not eos
            call      nbrkkey
            if        not over
                          move      brcomp to mname
            endif 
         endif
         MOVE      MCOMP TO BCNAME      *was remmed for patch #4.3
         GOTO      WRTALPH2
WRTALPH2
         rep       lowup in o2des
         goto      ritereca if (DateGroup = c0)    .all dates

         if        (omdtec <> "00" or omdtec = "00" and olnum = "")
RITERECA
.         RESET     EXFEELST
.         SCAN      OLNUM IN EXFEELST
.                             goto       recycle if equal
         WRITE     RECMST,SEQ;ORDVARS:
                              MNAME:
                              BCNAME
         endif
RECYCLE
        Branch PRIMTRACE,MailerSearch,ListSearch,NextLR,OfferSearch,OwnerSearch,BrokerSearch:
                     ReturnSearch,ComselectSearch,SLSpersonSearch1,BRKCNTSearch,BrkGuarSearch:
.patch5.11
.begin patch 2.23
.                    NINGUARSearch,DatesSearch,CampaignSearch
                    NINGUARSearch,DatesSearch,CampaignSearch,FullFilsearch
.end patch 2.23
.                         NINGUARSearch,DatesSearch            .Patch5.01
.patch5.11
PrinterOpt
   CLOSE   RECMST
   if (found = c0)
.patch5.06
          alert type=yesno1," Do you want to retain previous values?", result,"No Records Found"
          if (result=6)    . 6 = yes , 7 = no
.subpatch5.06
.         alert note,"No Records Matching Criteria....Try Again",result,"No records"
.Patch5.06
                    Move NO to Second
                    if (nordflag = c1)
                              reposit nordfile,c0 
                    endif
                                        move Yes to exitflag
                                        call livebut        
                    return
.subpatch5.06
                    else
                                        winshow             
                    CHAIN     "NORD0006"
                    stop                
.endpatch5.06
          endif



    endif
   if (COPY = "0")
         alert note,NEWNAME,result,"Done"
         move Yes to exitflag
         call livebut
         winshow
         CHAIN     "NORD0006"
         stop

          endif
          getitem Nord0006ComboPrimary,n4,n3
.Tells if pick is by list or mailer
        if (n3 = c2)
                move c2 to majrep     .list
        else
                move c1 to majrep
        endif
        getprop Nord0006RADIOExclude,SELGROUPID=n4
.Begin Patch5.06
        clear optflag 
.subPatch5.06
          call      debug
         load optflag with n4 from exclude,netsflag,pendflag,lcrflag

..        getitem Nord0006RADIOLCR,n4,LCRFlag
..        getitem Nord0006RADIOPENDING,n4,PendFlag
..        getitem Nord0006RADIONets,n4,NetsFlag
..        getitem Nord0006CheckExclude,n4,Exclude
..        getitem Nord0006CheckLCR,n4,LCRFlag
..        getitem Nord0006CheckPENDING,n4,PendFlag
..        getitem Nord0006CheckNets,n4,NetsFlag
         getitem Nord006CheckDUP,0,DUPLEX
         getitem Nord006CheckX,0,XLINES

.        getitem Nord0006ComboCOPY,n5,COPY
               getitem Nord0006ComboReportType,n4,reptype
        BRANCH  reptype of rep1,rep2,rep3,rep4,rep5,rep6,rep7,rep8,rep9,rep10:
                    rep11,rep12,rep13,rep14,rep15,rep16,rep17,rep18,rep19,rep20:
                    rep21,rep22,rep23,rep24,rep25,rep26,rep27,rep28,rep29,rep30:
                    rep31,rep32,rep33,rep34,rep35,rep36,rep37
.Rep1 *   NIN LIST BOOK   (ALPHA BY MLR NAME)              - O24 ALPHAMLR
rep1
        if (DUPLEX = c0)
.                if  (Bit64Flag = No)    
.               append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\butil job=O24 INfile=",TASKNAME
.               else     
               append    "!\\Nins1\Winbatch\butil job=O24 INfile=",TASKNAME
.               endif     
.                append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\BUTIL job=ALPHAMLR INfile=",TASKNAME
        else
.                if  (Bit64Flag = No)    
.               append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O25 INfile=",TASKNAME
.                    else
               append    "!\\Nins1\Winbatch\butil  job=O25 INfile=",TASKNAME
.                    endif                    
.               append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O25 INfile=",TASKNAME
.                append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=ALPHAMLD INfile=",TASKNAME
        endif
        goto      repfini

rep2
        if (DUPLEX = c1)
                if (XLINES = c1)
.XLines/Duplexed
.                              if  (Bit64Flag = No)    
.                             append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\butil job=O10 INfile=",TASKNAME
.                              Else
.                             append    "!\\Nins1\Winbatch\butil job=O10 INfile=",TASKNAME
                             append    "\\Nins1\Winbatch\butil job=O9 INfile=",TASKNAME
.                              endif
.                            append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\BUTIL job=NMLRALDW INfile=",TASKNAME
                else
.Duplexed
.                              if  (Bit64Flag = No)    
.                             append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O9 INfile=",TASKNAME
.                              else
                             append    "!\\Nins1\Winbatch\butil  job=O9 INfile=",TASKNAME
.                              endif
.                            append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O9 INfile=",TASKNAME
.                            append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=nmlrald INfile=",TASKNAME
                endif
        else
                if (XLINES = c1)
.Default/Xtra Lines
.                              if  (Bit64Flag = No)    
.                             append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\butil job=O11 INfile=",TASKNAME
.                              Else
                             append    "!\\Nins1\Winbatch\butil job=O11 INfile=",TASKNAME
.                              endif
.                            append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\BUTIL job=NMLRALW INfile=",TASKNAME
                else
.Default - test run from server ? instead of \\Nins1\Lanbatch\batch32  as vista & windows 7 need a rights change to run locally
.                              if  (Bit64Flag = No)    
.                              append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\butil job=O5 INfile=",TASKNAME
.                              else
                              append    "!\\Nins1\Winbatch\butil job=O5 INfile=",TASKNAME
.                              endif
.                append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\butil job=O5 INfile=",TASKNAME
.append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\BUTIL job=O5 INfile=",TASKNAME
.                 append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\BUTIL job=NMLRAL INfile=",TASKNAME
                endif
         endif
         goto      repfini
rep3
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC  \\Nins1\Winbatch\butil  job=O14 INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=O14 INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1  \\Nins1\Winbatch\butil  job=O14 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1  \\Nins1\Winbatch\butil  job=ALPHABK INfile=",TASKNAME
         goto      repfini

rep4
         if (DUPLEX = c0)
              if (XLINES = c1)
.                              if  (Bit64Flag = No)    
.                             append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil job=O7 INfile=",TASKNAME
.                              else
                             append    "!\\Nins1\Winbatch\butil job=O7 INfile=",TASKNAME
.                              endif
.                            append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil job=O7 INfile=",TASKNAME
.                            append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil job=NMLRAL2W INfile=",TASKNAME
              else
.                              if  (Bit64Flag = No)    
.                             append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\ServerC \\Nins1\Winbatch\butil job=O6 INfile=",TASKNAME
.                              Else
                             append    "!\\Nins1\Winbatch\butil job=O6 INfile=",TASKNAME
.                              endif
.                            append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil job=O6 INfile=",TASKNAME
.                     append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil job=NMLRAL2 INfile=",TASKNAME
              endif
                               else
              if (XLINES = c1)
.                              if  (Bit64Flag = No)    
.                             append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil job=O33 INfile=",TASKNAME
.                              else
                             append    "!\\Nins1\Winbatch\butil job=O33 INfile=",TASKNAME
.                              endif
.                            append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil job=O33 INfile=",TASKNAME
.                            append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil job=NMLRAL2W INfile=",TASKNAME
              else
.                              if  (Bit64Flag = No)    
.                             append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil job=O8 INfile=",TASKNAME
.                              else
                             append    "!\\Nins1\Winbatch\butil job=O8 INfile=",TASKNAME
.                              endif
.                            append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil job=O8 INfile=",TASKNAME
.                     append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil job=NMLRAL2 INfile=",TASKNAME
              endif
                               endif
         goto      repfini
rep5
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC  \\Nins1\Winbatch\butil  job=O1 INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=O1 INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1  \\Nins1\Winbatch\butil  job=O1 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1  \\Nins1\Winbatch\butil  job=NMLRASIS INfile=",TASKNAME
         goto      repfini
rep6
.         append    "abat submit /queue=test /on=creques /interactive=maximized /nooutput \\nts0\c\apps\winbatch\butil.exe /parameter=#(job=O3,INfile=",TASKNAME
..         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1  \\Nins1\Winbatch\butil  job=O3 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1  \\Nins1\Winbatch\butil  job=NMLRGT INfile=",TASKNAME
.               APPEND    NEWNAME TO TASKNAME
.         APPEND    " C=",TASKNAME
.         APPEND    COPY,TASKNAME
.         APPEND    ",B=",TASKNAME
.         APPEND    user TO TASKNAME
.         APPEND    " PRIN=",TASKNAME
.         APPEND    ",PA=",TASKNAME
.         APPEND    cntprint TO TASKNAME
.                             append    "#)" to taskname
.         RESET     TASKNAME
.         BATCH     TASKNAME
.                             stop
         goto      repfini
rep7
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O13 INfile=",TASKNAME
.          else
         append    "\\Nins1\Winbatch\butil  job=O13 INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O13 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=NMLRALU INfile=",TASKNAME
         goto      repfini
rep8
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\ServerC \\Nins1\Winbatch\butil  job=O16 INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=O16 INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O16 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=NMLRALC INfile=",TASKNAME
         goto      repfini
rep9
.Patch5.05
.          move c1 to selrep
.          call     OrderRunReport using NAMVAR,PRTNAM,OPTIONS
.          move      Yes to exitflag
.          call      livebut
.          winshow
.          CHAIN     "NORD0006"
.          stop
.Patch5.05
.         append    "\\Nins1\Winbatch\develop\extmailplan.exe",TASKNAME
.         append    "\\nts0\c\apps\winbatch\mailplan.exe",TASKNAME
         append    "\\Nins1\Winbatch\mailplan.exe",TASKNAME
         goto      repfini3
rep10
          call      Debug
.Patch5.05
.         append    "\\Nins1\Winbatch\develop\extordersum.exe",TASKNAME
.Patch5.05
.         append    "\\nts0\c\apps\winbatch\ordersum.exe",TASKNAME
         append    "\\Nins1\Winbatch\ordersum.exe",TASKNAME
         move      repnum to str3
         call      trim using str3
         goto      repfini2
rep11
         move      newname to comment
         pack      func,cntprint
         chain     "nlistsum0001"
         goto      repfini2
rep12
.04 jan 2013 - testing
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O12 INfile=",TASKNAME
.          Else
         append    "\\Nins1\Winbatch\butil  job=O12 INfile=",TASKNAME
.          endif

.         append    "\\Nins1\Lanbatch\batch32 -X -SC -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O12 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O12 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=nmlralf INfile=",TASKNAME
         goto      repfini
rep13
.          if  (Bit64Flag = No)    
.        append    "\\Nins1\Lanbatch\batch32  /X \\Nins1\Winbatch\butil  job=O18 INfile=",TASKNAME
.          Else
        append    "!\\Nins1\Winbatch\butil  job=O18 INfile=",TASKNAME
.          endif
.        append    "\\Nins1\Lanbatch\batch32  /X \\Nins1\Winbatch\butil  job=NMLRDTE INfile=",TASKNAME
         goto      repfini
rep14
        if (DUPLEX = c1)
.          if  (Bit64Flag = No)    
.              append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O28 INfile=",TASKNAME
.          Else
              append    "!\\Nins1\Winbatch\butil  job=O28 INfile=",TASKNAME
.          endif
.              append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O28 INfile=",TASKNAME
        else
.          if  (Bit64Flag = No)    
.                            append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O19 INfile=",TASKNAME
.          else
                            append    "!\\Nins1\Winbatch\butil  job=O19 INfile=",TASKNAME
.          endif
.                           append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O19 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=NLISTBK INfile=",TASKNAME
                      endif
        goto      repfini
rep15
        call    Report2destroyObjects
        setprop Report2,title="Report Title"
        create  Report2;StatTextBoxes(1)=50:70:10:110,"Title",""
        create  Report2;StatTextBoxes(2)=130:150:10:310,"",""
        create  Report2;StatTextBoxes(3)=150:170:10:310,"",""
        create  Report2;StatTextBoxes(4)=70:90:10:310,"",""
        create  Report2;StatTextBoxes(5)=10:30:10:310,"",""
        create  Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
        create  Report2;Buttons(1)=205:230:50:100,"O&K",zorder=500,default=1
        activate StatTextBoxes(1)
        activate StatTextBoxes(2)
        activate StatTextBoxes(3)
        activate StatTextBoxes(4)
        activate StatTextBoxes(5)
.When dynamically creating an EditTextBox, you are only given three default events: GotFocus,LostFocus,LostFocus+Change.
.Any other events must be registered manually.
.Below we register a KeyPress event.
        activate EditTextBoxes(1)
        activate Buttons(1),StartLROK,result
        listins ObjectColl,stattextBoxes(1),StatTextBoxes(2),StatTextBoxes(3),StatTextBoxes(4):
                StatTextBoxes(5),EditTextBoxes(1),Buttons(1),Buttons(2)
        setfocus EditTextBoxes(1)
        getitem EditTextBoxes(1),0,str55
        setprop Report2,visible=0
         append    "!\\Nins1\Winbatch\butil  job=NLISTSUM INfile=",TASKNAME
.         append    "\\Nins1\Winbatch\NLISTSUM.exe",TASKNAME
.end patch xxx
.Run("\\nins1\e\apps\winbatch\butil.exe", "job=nlistsum infile=%param1% prin=3 b=%b% c=1 levels=%q% pdf=y")

.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=NLISTSUM INfile=",TASKNAME
         goto      repfini15
         goto      repfini2
rep16
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O23 INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=O23 INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O23 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=NLISTMDT INfile=",TASKNAME
         goto      repfini
rep17
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O20 INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=O20 INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O20 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=NLISTC3 INfile=",TASKNAME
         goto      repfini
rep18
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O21 INfile=",TASKNAME
.          ELSE
         append    "!\\Nins1\Winbatch\butil  job=O21 INfile=",TASKNAME
.          ENDIF
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O21 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=NLISTBKA INfile=",TASKNAME
         goto      repfini
rep19
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O22 INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=O22 INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O22 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=NLISTOFR INfile=",TASKNAME
         goto      repfini
rep20
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=A2 INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=A2 INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=A2 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=DISKINVO INfile=",TASKNAME
         goto      repfini
rep21
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O27 INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=O27 INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O27 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=DISKORD INfile=",TASKNAME
         goto      repfini
rep22
          move      newname to comment
          pack      func,cntprint
          chain     "nloinc0001"
rep23
         append    "\\Nins1\Winbatch\butil  job=NSHPK INfile=",TASKNAME
         goto      repfini
rep24
.          if  (Bit64Flag = No)            
.          append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=A3 INfile=",TASKNAME
.          else
          append    "!\\Nins1\Winbatch\butil  job=A3 INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=A3 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=DISKINV INfile=",TASKNAME
         goto      repfini
rep25
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=A1 ",TASKNAME
.         append     " PORT=",taskname
.         append     portn,taskname
.         append     " INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=A1 INfile=",TASKNAME
.         append     " infile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=A1 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=NUNBILL INfile=",TASKNAME
         goto      repfini
rep26
.START PATCH 5.13 REPLACED LOGIC
.          alert type=yesno1," Discount Portion?", result,"Discount"
.          if (result=6)    . 6 = yes , 7 = no
.                   move      C2,str1
.                else
.                         move      C1,str1
.          endif
.START PATCH 5.14 REPLACED LOGIC
.         alert     type=yesno1,"Discount Portion?",result,"Discount"
.         if (result = 6)    . 6 = yes , 7 = no
.                   move      C2,str1
.         else
.                   alert     type=yesno2,"Print Rental Savings Columns?",result,"Rental Savings"
.                   if (result = 6)    . 6 = yes , 7 = no
.                             move      C0,str1
.                   else
.                             move      C1,str1
.                   endif
.         endif
          alert     type=yesno1,"Discount Portion?",result,"Discount"
          if (result = 6)    . 6 = yes , 7 = no
                    move      C2,str1
          else
                    move      C0,str1
          endif
.END PATCH 5.14 REPLACED LOGIC
.END PATCH 5.13 REPLACED LOGIC
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\butil  job=DWNX INfile=",TASKNAME
.          Else
         append    "!\\Nins1\Winbatch\butil  job=DWNX INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=REGX INfile=",TASKNAME
         goto      repfini
rep27
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\butil  job=QTYX INfile=",TASKNAME
.          Else
         append    "!\\Nins1\Winbatch\butil  job=QTYX INfile=",TASKNAME
.          endif
         goto      repfini
rep28
.START PATCH 5.13 REPLACED LOGIC
.          alert type=yesno1," Discount Portion?", result,"Discount"
.          if (result=6)    . 6 = yes , 7 = no
.                   move      C2,str1
.                else
.                         move      C1,str1
.          endif
.START PATCH 5.14 REPLACED LOGIC
.         alert     type=yesno1,"Discount Portion?",result,"Discount"
.         if (result = 6)    . 6 = yes , 7 = no
.                   move      C2,str1
.         else
.                   alert     type=yesno2,"Print Rental Savings Columns?",result,"Rental Savings"
.                   if (result = 6)    . 6 = yes , 7 = no
.                             move      C0,str1
.                   else
.                             move      C1,str1
.                   endif
.         endif
          alert     type=yesno1,"Discount Portion?",result,"Discount"
          if (result = 6)    . 6 = yes , 7 = no
                    move      C2,str1
          else
                    move      C0,str1
          endif
.END PATCH 5.14 REPLACED LOGIC
.END PATCH 5.13 REPLACED LOGIC
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\butil  job=DWNX INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=DWNX INfile=",TASKNAME
.          endif
         goto      repfini
rep29
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\Nins1\ServerA \\Nins1\Winbatch\butil  job=A4 INfile=",TASKNAME
.          else
         append    "\\Nins1\Winbatch\butil  job=A4 INfile=",TASKNAME
.          endif
.patch5.11
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=ARSTATE INfile=",TASKNAME
         goto      repfini
rep30
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O26 INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=O26 INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O26 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=VALPAMLR INfile=",TASKNAME
         goto      repfini
rep31
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O28 INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=O28 INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O28 INfile=",TASKNAME
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=nlistbkd INfile=",TASKNAME
         goto      repfini
rep32
         append    "\\Nins1\Winbatch\rentexch.exe",TASKNAME
         goto      repfini2
rep33
.Patch5.12
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O35 INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=O35 INfile=",TASKNAME
.          endif
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=O35 INfile=",TASKNAME
         goto       repfini
.Patch5.12
rep34
         goto       repfini
rep35
         goto       repfini
.begin 5.22
rep36
.04 jan 2013 - testing
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O36 INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=O36 INfile=",TASKNAME
.          endif
         goto      repfini
.end 5.22
.begin 5.24
rep37
.04 jan 2013 - testing
.          if  (Bit64Flag = No)    
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\Nins1\ServerC \\Nins1\Winbatch\butil  job=O37 INfile=",TASKNAME
.          else
         append    "!\\Nins1\Winbatch\butil  job=O37 INfile=",TASKNAME
.          endif
         goto      repfini
.end 5.24
.rep7
.         append    "\\Nins1\Lanbatch\batch32 -X -S#"<ANY>#" -Q\\nts0\c\lanbat~1 \\Nins1\Winbatch\butil  job=NMLRPRE INfile=",TASKNAME
.         goto      repfini
repfini4
         APPEND    NEWNAME TO TASKNAME
         append     " PORT=",taskname
          clear     str3
          move      portn,str3
          rep       zfill,str3
         append     str3,taskname
         APPEND    " C=",TASKNAME
         APPEND    COPY,TASKNAME
         APPEND    " B=",TASKNAME
         APPEND    user TO TASKNAME
         APPEND    " PA=",TASKNAME
         APPEND    cntprint TO TASKNAME
          Append    " co=",taskname
          Append    Company,Taskname
          getitem Nord0006ComboBox001,n1,pdfflag
         if (pdfflag = c1)    ;regular print
                   APPEND    " BP=Y",TASKNAME
          elseif (pdfflag = c2)             ;pdf
                    append    pdf to taskname
          elseif (PDFFLAG = C3)
                    append    " PDF=B" to taskname          ;both
          endif
         RESET     TASKNAME
          call      Debug
         batch   TASKNAME

                              call      ordersetmousefree
         move      Yes to exitflag
         call      livebut
         winshow
         CHAIN     "NORD0006"
         stop
repfini
                APPEND    NEWNAME TO TASKNAME
         APPEND    " C=",TASKNAME
         APPEND    COPY,TASKNAME
         APPEND    " B=",TASKNAME
         APPEND    user TO TASKNAME
.         APPEND    " PRIN=",TASKNAME
         APPEND    " PA=",TASKNAME
         APPEND    cntprint TO TASKNAME
.begin patch xxx
          Append    " co=",taskname
          Append    Company,Taskname
.end patch xxx
.patch5.10
.         getitem Nord006CheckPDF,0,PDFFlag
.may 2013

          getitem Nord0006ComboBox001,n1,pdfflag
         if (pdfflag = c1)    ;regular print
                   APPEND    " BP=Y",TASKNAME
          elseif (pdfflag = c2)             ;pdf
                    append    pdf to taskname
          elseif (PDFFLAG = C3)
                    append    " PDF=B" to taskname          ;both
          endif
.patch5.10
         if         (reptype = 26)      .XREPORT
                   append       " P=",TASKNAME
                   append       str1,TASKNAME
                              else
                   append  " P=",taskname
                   append  optflag,taskname
         endif                    

         RESET     TASKNAME
          call      Debug
         batch   TASKNAME

                              call      ordersetmousefree
         move      Yes to exitflag
         call      livebut
         winshow
         CHAIN     "NORD0006"
         stop

repfini2
         APPEND    " ",TASKNAME
         APPEND    NEWNAME TO TASKNAME
         APPEND    " ",TASKNAME
         APPEND    COPY,TASKNAME
         APPEND    " ",TASKNAME
         APPEND    MAJREP,TASKNAME
         APPEND    " ",TASKNAME
..         APPEND    " PA=",TASKNAME
.begin patch NOv 13 2007
.         APPEND    " PRIN=",TASKNAME
         APPEND    " PA=",TASKNAME
.end patch NOv 13 2007
         APPEND    cntprint TO TASKNAME
.;;;;
.Patch5.05
.Calendar or Fiscal
         if (selrep = c2)         ;List/Mailer Summary
                   APPEND    " B=",TASKNAME
                   APPEND    str3 TO TASKNAME
.patch5.09
                   APPEND    " X=",TASKNAME
                   APPEND    PDFOPT TO TASKNAME                   
                   APPEND    " Y=",TASKNAME
                   clear     str2
                   move      fiscmo to str2
                   rep       zfill,str2
                   APPEND    str2 TO TASKNAME                   
                   APPEND    " Z=",TASKNAME
                   APPEND    USER TO TASKNAME
                   APPEND    " D=",TASKNAME
                   APPEND    DATEOPT TO TASKNAME
         endif
.patch5.09
.Patch5.05
.;;;
..         if        (pdfflag = 2)
..                 append    pdf to taskname
..         endif
         RESET     TASKNAME
         winhide
         EXECUTE   TASKNAME


          call      ordersetmousefree
         move Yes to exitflag
         winshow
         CHAIN     "NORD0006"
         stop
repfini15
         APPEND    NEWNAME TO TASKNAME
         APPEND    " c=1",TASKNAME
         APPEND    " ",TASKNAME
         APPEND    " PA=",TASKNAME
         APPEND    cntprint TO TASKNAME
.Run("\\nins1\e\apps\winbatch\butil.exe", "job=nlistsum infile=%param1% prin=3 b=%b% c=1 levels=%q% pdf=y")
                   APPEND    " B=",TASKNAME
                   APPEND    str3 TO TASKNAME
                   APPEND    " levels=",TASKNAME
                   APPEND    str55 TO TASKNAME                   
                   APPEND    " pdf=y",TASKNAME
         RESET     TASKNAME
         winhide
         EXECUTE   TASKNAME


          call      ordersetmousefree
         move Yes to exitflag
         winshow
         CHAIN     "NORD0006"
         stop
.end patch
repfini3
          call      debug
         APPEND    " ",TASKNAME
         APPEND    NEWNAME TO TASKNAME
         APPEND    " ",TASKNAME
         APPEND    COPY,TASKNAME
         APPEND    " ",TASKNAME
         APPEND    mlrno,TASKNAME
         APPEND    " ",TASKNAME
.begin patch NOv 13 2007
.         APPEND    " PRIN=",TASKNAME
         APPEND    " PA=",TASKNAME
.end patch NOv 13 2007

         APPEND    cntprint TO TASKNAME
.DH test
         APPEND    " BP=",TASKNAME
         APPEND    user TO TASKNAME
.DH test

.Report Selection
         APPEND    " P=",TASKNAME
         move repnum to str3
         call trim using str3
         APPEND str3 TO TASKNAME
.SalesTeam if applicable
         if (teamdwn <> 0)
                   APPEND    " B=",TASKNAME
                   move teamdwn to str3
                   call trim using str3
                   APPEND     str3 TO TASKNAME
         endif
.Patch5.08
.patch5.10
PDF
.         getitem Nord006CheckPDF,0,PDFFlag
          getitem Nord0006ComboBox001,c1,pdfflag
          if (pdfflag = c2)             ;pdf
                    append    pdf to taskname
                    APPEND    " BP=",TASKNAME
                    APPEND    user TO TASKNAME
          endif


.        getitem Nord006CheckPDF,0,PDFFlag
.        if        (pdfflag = c1)

.         append    pdf to taskname
.User Name if Email PDF
.         APPEND    " BP=",TASKNAME
.                  APPEND    user TO TASKNAME
.         endif
.patch5.10
.Patch5.08
Debugg
         RESET     TASKNAME
         winhide

         execute    taskname
          call      ordersetmousefree
         move Yes to exitflag
         winshow
         CHAIN     "NORD0006"
         stop
Ender
        Alert caution,"Where are we at?",result
        call livebut
            call      ordersetmousefree
        return

FormBeginLR
        call    Report2destroyObjects
        setprop Report2,title="Beginning LR to start Search"
.        create  Report2;mRSearch,"&Search;&List"
        create  Report2;StatTextBoxes(1)=50:70:10:110,"Starting LR",""
        create  Report2;StatTextBoxes(2)=130:150:10:310,"",""
        create  Report2;StatTextBoxes(3)=150:170:10:310,"",""
        create  Report2;StatTextBoxes(4)=70:90:10:310,"",""
        create  Report2;StatTextBoxes(5)=10:30:10:310,"",""
        create  Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
        create  Report2;Buttons(1)=205:230:50:100,"O&K",zorder=500,default=1
.        create  Report2;Buttons(2)=180:205:140:190,"Cancel",enabled=1
.        activate mRSearch,ExRefSearchGo,result
        activate StatTextBoxes(1)
        activate StatTextBoxes(2)
        activate StatTextBoxes(3)
        activate StatTextBoxes(4)
        activate StatTextBoxes(5)
.When dynamically creating an EditTextBox, you are only given three default events: GotFocus,LostFocus,LostFocus+Change.
.Any other events must be registered manually.
.Below we register a KeyPress event.
.        eventreg EditTextBoxes(1),10,ExRefKeyPress,RESULT=N9
        activate EditTextBoxes(1)
.        activate EditTextBoxes(1),ExRefEditChange,result
        activate Buttons(1),StartLROK,result
.       activate Buttons(2),StartLRCancel,result
        listins ObjectColl,stattextBoxes(1),StatTextBoxes(2),StatTextBoxes(3),StatTextBoxes(4):
                StatTextBoxes(5),EditTextBoxes(1),Buttons(1),Buttons(2)
        setfocus EditTextBoxes(1)
        return

StartLROK
        getitem EditTextBoxes(1),0,LRNO
        setprop Report2,visible=0
        goto    BEGLRVAL

.................................................................................
FileGo
.Flag set to "N" if in Modify or New mode
.        branch result to FileGo1,FileGo2,FileGo3,FileGo3
        branch result to FileGo1
FileGo1
          call click_Nord0006Exit
        RETURN
Optionsgo
        return
ViewGo
        return
EditGo
        return
HelpGo
        branch  result to HelpGo1,HelpGo2
HelpGo1
        setprop AboutMssg,visible=1
        return
HelpGo2
        execute   "c:\progra~1\Intern~1\iexplore.exe http://web01\programs\ninca\orderpick\orderpickmenu.html"  

        return
SearchGo
.patch5.11
.        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4
.begin patch 5.23
.        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4,SearchGo5
        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4,SearchGo5,SearchGo6,SearchGo7
.end patch 5.23
.patch5.11
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
.SHIP-TO
        move    C4,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
.patch5.11
SearchGo5
.SHIP-TO
        move    C5,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
.begin patch 5.23
SearchGo6
.OWNER
              move            C6,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchGo7
.Fulfillment
              move            C7,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
.end patch 1.31

SearchLoad
.begin patch 1.31
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6,SearchLoad7

.patch5.11
.Called by SearchDataList_DoubleClick
.patch5.11
.        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4
.        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6,SearchLoad7
.end patch 5.23
.patch5.11
SearchLoad1
.BROKER
        unpack srchstr,str4,b1,str3
        if (SecFlag = NO)
                      getitem Nord0006ComboPrimary,n4,n3
          if (n3 = c10)
                    pack str7 with str4,str3
                          setitem Nord0006EditSearchP,0,str7
                          setfocus Nord0006EditSearchP
                  else
                    pack str7 with str4,z3
                          setitem Nord0006EditSearchP,0,str4
                    setfocus Nord0006EditSearchP
                  endif
        else
                  setitem Nord0006EditSearchS,0,str4
            setfocus Nord0006EditSearchS
        endif
        return
SearchLoad2
.LIST
        unpack srchstr,str6,str1,str35,str1,str10
        if (secflag = NO)
                  setitem Nord0006EditSearchP,0,str6
                  setfocus Nord0006EditSearchP
        else
                  setitem Nord0006EditSearchS,0,str6
                  setfocus Nord0006EditSearchS
        endif
        return
SearchLoad3
.MAILER
        unpack  Srchstr,str4,str1,str3,str1,str45
        if (SECFLAG = NO)
                  if (n3 = c4)
                            pack str7 with str4,str3
                      setitem Nord0006EditSearchP,0,str7
                            setfocus Nord0006EditSearchP
           else
                      setitem Nord0006EditSearchP,0,str4
                      setfocus Nord0006EditSearchP
            endif
        else
              if (n3 = c3)
                              pack str7 with str4,str3
                              setitem Nord0006EditSearchS,0,str7
                              setfocus Nord0006EditSearchS
               else
                              setitem Nord0006EditSearchS,0,str4
                              setfocus Nord0006EditSearchS
               endif
        endif
        return
SearchLoad4
.SHIP-TO - not an option with this program
        unpack  Srchstr,str4
        if (SECFLAG = NO)
             setitem Nord0006EditSearchP,0,str4
                    setfocus Nord0006EditSearchP
        else
             setitem Nord0006EditSearchS,0,str4
                    setfocus Nord0006EditSearchS
             return
        endif
        return
.patch5.11
SearchLoad5
.CAMPAIGN
        unpack  Srchstr,str6
        if (SECFLAG = NO)
             setitem Nord0006EditSearchP,0,str6
                    setfocus Nord0006EditSearchP
        else
.            setitem Nord0006EditSearchS,0,str6
.                   setfocus Nord0006EditSearchS
             return
        endif
        return
.patch5.11
.begin patch 5.23
SearchLoad6
.OWNER - 
        if (SECFLAG = NO)
               unpack         Srchstr,str4,str1,str25
               setitem        Nord0006EditSearchP,0,str4
                    setfocus Nord0006EditSearchP
              endif
        return
SearchLoad7
.Fulfillment - 
        if (SECFLAG = NO)
               unpack         Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
               setitem        Nord0006EditSearchP,0,str6
                    setfocus Nord0006EditSearchP
              endif
        return
.end patch 5.23
LiveBut
          setprop ColPrim,enabled=c1
          setprop ColSec,enabled=c1
          setprop ColRadate,enabled=c1
          setprop ColCombDate,enabled=c1
          setprop ColFilter,enabled=c1
          setprop ColPrint,enabled=c1
          setprop Nord0006PickOff,enabled=c1
          call ordersetmousefree
   return
OrderSetMouseBusy
        setmode *mcursor=*wait
        return
OrderSetMouseFree
        setmode *mcursor=*arrow
        return
.................................................................................................................
Report2DestroyObjects
        destroy ObjectColl
        return
DiskinDel
                                                                      close RECMST
                     clear     recname
                     append    NTWKPATH1 to recname
                     append    newname to recname
                     append    ".DAT" to recname
                     reset     recname
                     erase     recname
                                          setitem   Nord0006DiskinDisplay,c0,""
                                                                      setitem   Nord0006StatRecords,c0,""
                     setitem   Nord0006StatRead,c0,""
                                                                      move      c0 to found
                                                                      move      c0 to records
                                                                      pack      taskname with recname," Deleted!"
                                                                      alert     caution,taskname,result,"Deleted"
                                                                      noreturn
                                                                      call      livebut
                                                                      move      NO to StopFlag
                                                                      move      YES to ExitFlag
                     winshow              
                     CHAIN     "NORD0006" 
                     stop                 
CheckStop
            eventcheck
                      if (stopflag = YES)
              goto DISKINDEL
                      else
                                                  return
                      endif      
.ReadRecs
.                             move records to str9
.         setitem Nord0006StatRead,c0,str9
.                             return
................................................................
.patch5.12
                                        include   compio.inc
                                        include   cntio.inc
.        INCLUDE   NMLRIO.inc
.patch5.12
         INCLUDE   NORDIO.inc
         INCLUDE   NDATIO.inc
         include   listhelp.inc
         include   mlrhelp.inc
.patch5.12
.        include   nbrkio.inc
.patch5.12
         include   ncntio.inc
         INCLUDE   NOWNIO.inc
         INCLUDE   NRTNIO.inc
.ADDED FOR SEARCH.PLF............................................
         include    searchio.inc      .contains logic for search.plf
         include    ncmpio.inc
.START PATCH 5.11A ADDED LOGIC
          INCLUDE   NCATIO.INC
.END PATCH 5.11A ADDED LOGIC
         include    comlogic.inc