. MODINV
.* .............................................................................
. .......
PC             EQU            1
.
               INCLUDE        COMMON.INC
               INCLUDE        CONS.INC
               INC            NONODD.inc
               INC            NCRCDD.inc
               INCLUDE        CONSacct.inc
               INCLUDE        ninvdd.inc
               INC            NOWNDD.inc
               INCLUDE        NBILDD.inc
               INCLUDE        NORDDD.INC
               INCLUDE        NSPEDD.inc
               INCLUDE        NSPIDD.inc
               INCLUDE        NSHPDD.INC
               INCLUDE        NDATDD.inc
               INCLUDE        NRTNDD.inc
               INCLUDE        NPASDD.inc
               INCLUDE        NACDDD.inc
               INCLUDE        NPAYDD.inc
               INCLUDE        NADJDD.inc
               include        tinvdd.inc
               include        nusedd.inc
               INCLUDE        NDAT3DD.inc
               INCLUDE        NMRGDD.inc
               INCLUDE        NCNTDD.inc
               INCLUDE        MEDIA.INC
               INCLUDE        NMOADD.INC
               include        nmobdd.inc
               include        shipping.inc
               include         compdd.inc
               include         cntdd.inc
               INCLUDE        GNXTDD.inc
               INCLUDE        osLSPERN.inc
               INCLUDE        HP.INC
               include        nofrdd.inc
.START PATCH 10.021.1 REMOVED LOGIC
.               include        nfuldd.inc
.END PATCH 10.021.1 REMOVED LOGIC
               include        Nmoddd.inc
               Include        Nsel2dd.inc
               include        winapi.inc
.begin patch 10.5
          INCLUDE   NTYPdd.INC
.enD patch 10.5
.begin patch 10.0
               Include        NInvAcdDD.inc
.End patch 10.0
.begin patch 10.15
FileCheck FIle
trapcount form      4
.end patch 10.15
.begin patch 10.5
.Pointers used for updating objects
FrmPtr    form      ^
.end patch 10.5

..............................................................
.begin patch 10.0
...........................
..EXTERNAL ROUTINES FROM NADJ0001.PLC
LoadAdjustListViews external "NADJ0001;AdjustLoadListViews"
DisableAdjustForm   external "NADJ0001;AdjustDisableForm"
EnableAdjustForm    external "NADJ0001;AdjustEnableForm"
.Following used only in order to load Search.plf
               include        ncmpdd.inc                                   ;campaign
AKey2          init           "01F"
AKey3          init           "02F"
AKey1A         init           "01L"
AKey2A         init           "02L"
filler         init           "0000"
filler2        init           "0000000"
badstat        init           "B*P"
datestring     dim            10
Indexstring    dim            10
str10a         dim            10
Carr           init           0x7f
testff         init           0xff
test55         init           0x55
test00         init           0x00
test01         init           0x01
test1          init           0x33
test2          init           0x08
.hexeight       integer        4,"4294967295"
hextwo         init           0x02
hexfour        init           0x04
ScanBreak      form           "0"
.Colors
white          color
grey           color
RED            COLOR
BLACK          COLOR
Yellow         Color
Green          Color
Blue           Color
Cyan           Color
MAGENTA        COlor
.Define Fonts to be used
font1                  font
Font4                  font
font5                  font
Font08                 font
font09                 font
Font09I                font
Font09B                font
Font09BI               font
Font012                font
Font012B               font
Font012I               font
Font014                font
Font014B               font
Font014BI              font
Font018I               font
Font07                 font
Font07dot5             font
Font07dot5B            font
Font07dot5I            font
Font07dot5BI           font
Font018B               font
Font018BI              font
PRTPG24B               font
PRTPG24I               font
PRTPG10                font
PrFIle         Pfile
NINLogo        PICT
.IntegralStoreDetail           external       "INT001A;IntegralStoreDetail"
.IntegralTestDetail            external       "INT001A;IntegralTestDetail"

Timer          Timer
Tabnum         form           2
SaveTab        form           2
M$AR           DIM            15
M$AP1          DIM            15
M$AP2          DIM            15
M$AP3          DIM            15
M$STAX         DIM            10
M$POST         DIM            6
M$LRINC        DIM            15
M$NINC         DIM            15
M$GROSS        DIM            15
M$GROSSbase    DIM            15      base  cost before net
M$netsavins    DIM            15      $ saved by having net
M$netsavfee    DIM            15      Fee to NIN for negotiating savings
height         form               7.4
width          form               7.4
MaxHeight      form               "610.0000"
MinHeight      form               "438.0000"
MaxWidth       form               "905.0000"
MinWidth       form               "642.0000"
MouseForm      form               10
FarRight       form               4
FarBottom      form               4
userlogn2      dim            7
PRTFLAG        dim             1
.Parameters for Search Screen
TempTop        form            9
TempLeft       form           9
SerTop         form            9
SerLeft        form           9
MaxTHeight     form           "600.0000"
MinTHeight     form           "88.0000"
MaxTWidth      form           "2000.0000"
MinTWidth      form           "360.0000"
SrchWinFlag    form 1
T1             form            4
L1             form            4
STR11a         Dim            11
HoldStatb     Dim             1
.end patch 10.0
release   init    "10.84"       DLH     .html cleanup
Reldate   Init      "2016 February 24"
.release   init    "10.83"       DLH     .rewrite LO memo to be html in the email body.
.Reldate   Init      "2015 May 28"
.release   init    "10.82"       DLH     New fax gateway
.Reldate   Init      "2015 April 14"
.release   init    "10.81"       DLH     Finished  patch for Celco, et al on List Owner Memo;s
.Reldate   Init      "2014 March 24"
.release   init    "10.8"       DLH     temporary patch for Celco on List Owner Memo;s
.Reldate   Init      "2014 March 14"
.release   init    "10.7"       ASH     Correction of TYPOUT logic
.Reldate   Init      "2013 November 12"
.release   init    "10.6"       DLH     User owner email for Invoice request if avail.
.Reldate   Init      "2013 October 14"
.release   init    "10.5"       DLH     TYpist file updates
.Reldate   Init      "2013 October 07"
.release   init    "10.4"       DLH     Sunbelt PDF
.Reldate   Init      "2013 March 23"
.release   init    "10.3"       DLH     Add Entire list check box
.Reldate   Init      "22 March 2013"
.release   init    "10.2"       DLH     check for addlt charge stop date (AMB, ARB)
.Reldate   Init      "8 March 2012"
.release   init    "10.15"       DLH     use pdf995 instead of faxfile
.Reldate   Init      "16 March 2011"
.release   init    "10.14"       DLH     wipe display of brk/consult if not on Invoice
.Reldate   Init      "18 August 2010"
.release   init    "10.13"       DLH     change event fixed for NinvEditTextLR and INV ninv001a.plf
.Reldate   Init      "28 September 2009"
.release   init    "10.12"       DLH     Display collection letter status
.Reldate   Init      "7 October 2008"
.release  init    "10.11"       DLH     Update for PL exclusive lists
.Reldate  Init      "09 October 2008"
.release  init    "10.10"       DLH     Update To flag orders with overlays IE Comselect
.Reldate  Init      "05 May 2008"
.release  init    "10.09"       DLH     Update ndat3 code to match ndat0022 - fixed displays
.Reldate  Init      "07 Feb 2008"
.release  init    "10.08"       DLH     PLI ---- MOA
.Reldate  Init      "08 Nov 2007"
.release  init    "10.07"               17Sep2007  DLH      PLI
.Reldate  Init      "17 Sep 2007"
.release  init    "10.06"               22May2007  DLH      PLI
.Reldate  Init      "22 May 2007"
.release  init    "10.05"               22Mar2007  DLH      Check for LR at Hotinv
.Reldate  Init      "22 March 2007"
.release  init    "10.04"               08Mar2007  DLH      Oslspern.inc expansion
.Reldate  Init      "8 March 2007"
.release  init          "10.03"            DLH    07Dec2006 sendmail
.Reldate  Init      "7 December 2006"
.release                      init          "10.023"           JD    13OCT2006 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.Release         Init               "10.022"          DLH  08September2006 INsure Typist inits do not corrupt
.Reldate  Init      "17 October 2006"
.Reldate  Init      "08 September 2006"
.Release         Init               "10.021.1"        DMS  21June2006 Fulfillment Conversion
.Release         Init               "10.021"      DMS          14June2006 NONODD Conversion
.Release        Init           "10.020"      DLH  05June2006 PLF Ninv001a added checking for disabled buttons
.Release        Init           "10.019"      DLH  16May2006 Label Muvsys - defunct attempt fix bad inv date
.Release        Init           "10.018"      DLH  10Mar2006 Clear more vars
.Release        Init           "10.017"      JD  06Feb2006 Batching Hotinv/Hot PDFs.
.Release        Init           "10.016"     JD  18Nov2005 Allow disaster deducts on Net orders.
.Release        Init           "10.015"     DLH 17Nov2005 test temp code if insufficient funds in MOB.
.Release        Init           "10.014"     DLH 24Oct2005 attempt to cleanup return from change to ninv0008
.Release        Init           "10.013"     DLH 20Oct2005 FOrce invcode to 4 on pure exchange
.Release        Init           "10.012"     DLH 20Oct2005 Add alerts for Notes, Cancelled, adjusted, restored use of variable "PROGRAM"
.Release        Init           "10.011"     DLH 19Oct2005 move scan for latelo inv
.Release        Init           "10.010"     06Oct2005 added new allowable deduct nmrgdisa.
.Release        Init           "10.009"    05oct20005 Updated ninv0001a.plf enlarge max char to 8, ap1 on force ap.
.Release        Init           "10.008"    September 26 2005 DLH Force INquire mode on "P"aid invoice
.                                          & disable modify button in add mode
.                                          reset mode at clear inv vars
.                                          create HoldStatB
.Release        Init           "10.007"    September 22 2005 DLH Duplicate key error in nininvacd file on new invoice
.Release        Init           "10.006"    September 21 2005 DLH
.Release        Init           "10.005"    August 9 2005 DLH Non net input qty fix Pray
.Release        Init           "10.004"    July 12 2005 DLH Non net input qty fix unfix/refix. ?
.Release        Init           "10.003"    June 24 2005 DLH Non net input qty fix
.Release        Init           "10.002"    June  2005 JD TDMC charges getting over
.Release        Init           "10.001"   April 2005 DLH Commission percentage change.
.Release        Init           "10.0"   June 2003 DLH new additional charges structure, revised compute, GUI interface
.                       automation of several processes; search for orders ready to bill, etc
.***********need to add admin mode allowing fix busy etc.****************************
.Reldate        Init           "09 June 2003"
* .............................................................................
.....................
.
.begin patch 10.84
HexCRLF   INIT 0x0D, 0x0A
.end patch 10.84

. FILES DESCRIPTIONS.
. ...................
FORMFILE       FILE           UNCOMP
preffile       file
. .....................................................
TempINv        Ifile          Keyl=6                    *get actual inv # from live file
. .....................................................
. .....................................................
.
. VARIABLES DESCRIPTIONS.
. .......................
addcflag      form      1         1=default, 2=warning - special %(all run chrg in prog 19 none on inv)
addclist      init      "006700,"
form102       form      10.2
form122       form      12.2
TDMCAMT       FORM       8.2
KEY           DIM       6            USED TO PASS LR TO NOTES CODE.
MO            DIM       2
DAY           DIM       2
YR            DIM       2
CINVNO        FORM      6        INVOICE #'S FOR       cinvno actually is now a date field
DINVNO        FORM      6        the DIFFERENT
NINVNO        FORM      6        CORPS
.begin patch 10.0
.min74         form      7.4
.min72         form      7.2
.F2            DIM       2
.F6            DIM       6
.Form102        FORM           10.2
holdar         form           10.2
holdap         form           10.2
holdap2        form           10.2
holdap3        form           10.2
holdlrinc      form           10.2
holdNIninc     form           10.2
ExitFlag       DIM               %1
               MOVE               YES,EXITFLAG
ModAcdFlag     Dim            1                  .= yes if add charges changed
InvSearchFlag  Form           1                  1=lr 2=inv 3=po 4=LO Inv, 5= Check ##
InvSearchType  Form           1                  1=Invoice File 2=Order File
.Str255         dim            255
.End patch 10.0
. .............................................................................
. .INVOICE Data manipulation vars
. .................................
SYSMO    DIM       2             DATE FOR CREATED
SYSDAY   DIM       2             INVOICE
SYSYR    DIM       2             RECORD
SYSCC    DIM       2
.
brkflag        dim        1                    ;"Y" if Brk read is valid
. .............................................................................
CONTACT        DIM            25                   ;NIN Contact
PERCENT        FORM      4.2
DIFF           FORM      9
FPPM           FORM       5.2              PRICE PER THOUSAND
****************************************************************************
. .............
. ........ADJUSTMENT VARS
FARADJ         FORM      6.2
FAP1AJ         FORM      6.2
FAP2AJ         FORM      6.2
FAP3AJ         FORM      6.2
FLRADJ         FORM      6.2
FNINADJ        FORM      6.2
FSTXAJ         FORM      4.2
FCTXAJ         FORM      4.2
FPSTAJ         FORM      3.2
MOADJ          DIM       2
DAYADJ         DIM       2
YRADJ          DIM       2
JDATE          FORM      5
CHKDATE        FORM      5
.BEGIN PATCH
FxNINADJ        FORM      6.2
.END PATCH
. .............................................................................
. ********   W O R K    A R E A S   ************************
. .............................................................................
LONGDIST DIM       1
recname  dim       50
FORMNAME DIM       30
KFPPERM  FORM      4.2          usual price work area
KFQINM   FORM      9
TENDOLL        FORM      "99999"
NINEDOLL       FORM      "199999"
SEVDOLL        FORM      "300000"
ownpay         form      10.2        new field for fax memo to l/o 24mar94.
CHANGE         FORM      7.2         CHANGE TO BE APPLIED TO BALANCE.
Form94a        FORM      9.4
CORP           DIM       1
WORK3          FORM      10.4            ;from .3 to .4
.Begin patch 10.0
Mode           Form           1              0, or 1 = Inquire, 2 = Modify, 3 = add
.end patch 10.0
BRANCH   FORM      1
SAVE    DIM       47
RBRAK    INIT      ">"
LBRAK    INIT      "<"
SYSDAT   DIM       6
TIME     INIT      "HH:MM:SS"
.end patch 10.0
.DISMOD   DIM       1
.SAVCD    DIM       1
.CMPCHECK FORM      6
DATCHECK FORM      6
NINCHECK FORM      6
CHECK    FORM      6
HOLDINVNum     FORM      6
HOLDMO         DIM       2
HOLDDY         DIM       2
HOLDYR         DIM       2
.$F1      DIM       1
OFFLINE  DIM       8
REPRINT  DIM       1     HOLDS 'R' IF INVOICE IS BEING REPRINTED.
DATE     DIM       8
HOLDKEY  DIM       6
SHIPSW   DIM       1    "Y" IF SHIPPING INFO
MRGSW    DIM       1    "Y" IF MERGE INFO
CALCQTY  FORM      7.2
RUNSW    DIM       1
FORM8    FORM      8
INPQTY   FORM      8
BaTCHflag form       1
faxtele  dim       10
faxname  dim       45
latelo   init     "      NONEGS      NONEGB     NOLOINV"
acctypE   Dim      15
faxflag  form     1
userinfo dim       500
userlogn dim       7
userlogw dim       7
oqtyflag dim       1          Y= no mrg/ no ship/ use qty billed as input for Select/m calc.
BEGIN    FORM      2
LAST     FORM      2
febdat   form      5
.begin patch 10.0
mask10         init       "ZZ,ZZZ,ZZ9"
mask11         init       "ZZZ,ZZZ,ZZ9"
MASK52         INIT      "ZZ,ZZZ.ZZ-"
MASK72         INIT       "Z,ZZZ,ZZZ.99-"
MaskBalance    INIT       "$,$$$,$$9.99-"
.START PATCH        10.021              REPLACED LOGIC
.hold3               dim               390               .FOR NOTES
hold3               dim               530               .FOR NOTES
.END PATCH          10.021              REPLACED LOGIC
.START PATCH        10.021              REPLACED LOGIC
ndate2              dim       8
ntime2              dim       4
.END PATCH          10.021              REPLACED LOGIC
Hold           Dim            408
mFile                     menu
mHelp                     menu
mView           Menu
mOptions                  Menu
.Set Up SubMenu for Options
sSearch                 submenu
.Present Data for Menu Bar
FData                  init    "&File;&Print;-;E&xit"
OData                  init    "&Options;&Search-F2;-;&Preferences;"
HData          init    "&Help;&About"
.
EditTextBoxes EditText (4)
Buttons        Button         (5)
CheckBoxes    CheckBox (2)
ComboBoxes    ComboBox (4)
StatTextBoxes StatText (8)
ListViews     ListView (2)
DataLists     DataList (2)
MDateWindow   Window
.Following Collection is used to dynamically destroy objects listed above
.Each time a form is loaded with those objects,             dump them into this collection
.and then destroy whole       collection when               needed
ObjectColl    Collection
.Objects used by OptionsOrd.plf              (a generic form)
.OptionsArrayEx               dim            x(y,z)
.Following Array will allow:  y              rows - representing each Screen
.                                   z        columns        - representing # of preferances              for each Screen
.                                   x        length of each field in       each Screen
.
OptionsArr1 CONST             "10"
OptionsArr2 CONST             "5"
OptionsArrSize CONST          "5"
OptionsArray dim              OptionsArrSize(OptionsArr1,OptionsArr2)
.Screen       1
Options1Coll   Collection
OptionsScreenInit             ComboBox
OptionsBilledNo               CheckBox
OptionsCancNo                CheckBox
OptionsPassInit               CheckBox
.Screen       2
Options2Coll   Collection
.Screen       3
Options3Coll   Collection
.Screen       4
Options4Coll   Collection
.Screen       5
Options5Coll   Collection
OptionsScreen5BrkFilter       CheckBox
OptionsScreen5EMailOption CheckBox
OptionsScreen5FileDefault CheckBox
.Screen       6
Options6Coll   Collection
.OptionsScreen6Init CheckBox
.Screen       7
Options7Coll   Collection
OptionsScreen7Proj CheckBox
.Screen       8
Options8Coll   Collection
.OptionsScreen8OrderSearch CheckBox
.Screen       9
Options9Coll   Collection
OptionsScreen9View CheckBox
.Screen       10
Options10Coll  Collection
OptionsScreen10View CheckBox
OptionsScreen10Usage CheckBox
EmailAddr     dim             256

SRCH           plform  Search
mss1           plform  Error
.EXTERNAL ROUTINES FROM               INFO.PLC
OrderLoadForm      external "INFO;LoadForm"
OrderDisplayMailer external "INFO;DisplayMailer"
OrderDisplayBroker external "INFO;DisplayBroker"
OrderDisplayShipto external "INFO;DisplayShipto"
OrderDisplayOwner  external "INFO;DisplayOwner"
.
NInvB          plform         Ninv0001B
X              plform         Ninv0001
SearchForm     plform         NInv0001C
NInvA          plform         Ninv0001a
abt            plform         About
adjfrm         plform         ninv001M
opt           plform          OptionsOrd
pss            plform         Passwrd
               winhide
.end patch 10.0
. .............................................................................
* PROGRAM MAIN.
. .............
               call               OrderLoadForm
.               Move           "NINACDNEW" to NACDNAME
.begin patch 10.0
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.               Move           "c:\work\NINInvAcd" to Ninvacdname
.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.Load Forms, Always load parent form first
               formload       x
               formload       mss1
               formload       SRCH
               formload       pss
              formload       opt
               formload       NInvB,NInv0001
               formload       NInvA,NInv0001
              formload       SearchForm,Ninv0001
.


               call           LoadAdjustListViews using ninv0001

.Create SubMenu
               CREATE         NInv0001;MFile,FData
               create         NInv0001;mOptions,OData,mFile
               create         NInv0001;mHelp,HData,mOptions
.Activate SubMenus
               activate       mFile,FileGo,result
               activate       mOptions,OptionsGo,Result
               activate       mHelp,HelpGo,result
.Create fonts to be used
..Create fonts to be used
               create               font1,"Helvetica",size=14,bold
               create               Font08,"Helvetica",size=8
               create               font5,"Helvetica",size=11
               create               font09,"Helvetica",size=9
               create               Font09I,"Helvetica",size=9,Italic
               create               Font09B,"Helvetica",size=9,Bold
               create               Font09BI,"Helvetica",size=9,Bold,Italic
               create               Font012,"Helvetica",size=12
               create               Font012I,"Helvetica",size=12,Italic
               create               Font012B,"Helvetica",size=12,Bold
               create               Font014,"Helvetica",size=14
               create               Font014B,"Helvetica",size=14,Bold
               create               Font014BI,"Helvetica",size=14,Bold,Italic
               create               Font018I,"Helvetica",size=18,Italic
               create               Font07,"Helvetica",size=7
               create               Font018B,"Helvetica",size=18,Bold
               create               Font018BI,"Helvetica",size=18,Bold,Italic
               create               PRTpg24B,"Helvetica",size=24,Bold
               create               PRTpg24I,"Helvetica",size=24,Italic
               create               PRTpg10,"Helvetica",size=10
.               create         font4,"Arial",size=14,italic
              create          NINLogo=3:13:30:50:
               "\\nins1\e\netutils\NIN logo black outline.jpg"
.Create Colors
               create         white=*white
               create         grey=*ltgray
               create         RED=*RED
               create         black=*black
               create         Yellow=*yellow
               create         Green=*Green
               create         Blue=*Blue
               create         Cyan=*Cyan
               create         Magenta=*Magenta
               InvListView001.InsertColumn using "Rec",0,1
               InvListView001.InsertColumn using "Code",30,2
               InvListView001.InsertColumn using "Description",165,3
               InvListView001.InsertColumn using "Quantity",100,4
               InvListView001.InsertColumn using "Rate",75,5
               InvListView001.InsertColumn using "Per",30,6
               InvListView001.InsertColumn using "%",50,7
               InvListView001.InsertColumn using "P Code",50,8
               InvListView001.InsertColumn using "Q Code",30,9
               InvListView001.InsertColumn using "Amount",75,10
               activate        InvEditTextLR
               Setfocus        InvEditTextLR
               InvoiceListView.InsertColumn using "LR",50,1
               InvoiceListView.InsertColumn using "Mailer P.O.",80,2
               InvoiceListView.InsertColumn using "Mailer",40,3
               InvoiceListView.InsertColumn using "List",55,4
               InvoiceListView.InsertColumn using "Broker",50,5
               InvoiceListView.InsertColumn using "Mailer Key",80,6
               InvoiceListView.InsertColumn using "Other               Detail",100,7
.Set Following extended               properties for all ListView objects:
.  FullRowSelect
.  DragAndDrop
.  OneClickActivate
hexer               integer               1,"0x0070"
.
.Create/Activate Objects on OptionsOrd.plf
.Screen       1
              create          Options;OptionsBilledNo=60:80:40:255,"'Include Billed in Search' Default = 'No'",zorder=100
              create          Options;OptionsCancNo=80:100:40:300,"'Include Cnc/open in Search' Default = 'No'",zorder=100
              create          Options;OptionsPassInit=100:120:40:305,"Skip Password Check at Program Execution",zorder=100
              listins         Options1Coll,OptionsScreenInit,OptionsBilledNo,OptionsCancNo,OptionsPassInit
              setitem         OptionsBilledNo,0,c0
              setitem         OptionsCancNo,0,c0
               setitem        OptionsPassInit,0,c0
              setprop         Options1Coll,visible=1        .always        start with the first tab visible

.Screen       5
.             create          Options;OptionsScreen5BrkFilter=80:100:40:250,"Filter Broker Report Records",zorder=100
.             create          Options;OptionsScreen5EMailOption=100:120:40:250,"Display EMail            Option Message",zorder=100
.             create          Options;OptionsScreen5FileDefault=120:140:40:250,"Use Default Notes File",zorder=100
              listins         Options5Coll,OptionsScreen5BrkFilter,OptionsScreen5EMailOption,OptionsScreen5FileDefault

.Screen       6
.              create          Options;OptionsScreen6Init=80:100:40:250,"Open             Program        on Campaign Screen",zorder=100
.              listins Options6Coll,OptionsScreen6Init

.Screen       7
.              create          Options;OptionsScreen7Proj=80:100:40:250,"Allow Viewing of Projection Breakdown",zorder=100
.              listins Options7Coll,OptionsScreen7Proj
.Screen       8
.              create          Options;OptionsScreen8OrderSearch=80:100:40:250,"Include Order            File in        Searches",zorder=100
.              listins Options8Coll,OptionsScreen8OrderSearch

.Screen       9
              create          Options;OptionsScreen9View=80:100:40:250,"Disable Viewing of Screen 9",zorder=100
              listins         Options9Coll,OptionsScreen9View

.Screen       10
              create          Options;OptionsScreen10View=80:100:40:250,"Disable Viewing of Screen 10",zorder=100
              create          Options;OptionsScreen10Usage=100:120:40:250,"Automatically Calculate Usage/Orders",zorder=100
              listins         Options10Coll,OptionsScreen10View,OptionsScreen10Usage

               InvoiceListView.SetExtendedStyle giving N9 using 0,hexer
               InvListView001.SetExtendedStyle giving N9 using 0,hexer
               call           openpref
.....................................................................................................................................
               CREATE         TIMER,27000     ..45 minutes
               ACTIVATE       TIMER,Timeout,RESULT
               SETWTITLE      "Names in the News - Billing Inquiry "
               TRAP           IOmssg GIVING ERROR IF IO

               formload       adjfrm
               Call           InvButtonDefault
               call           EnableAcdButtons

.end patch 10.0
.Get default printer
              move            PORTN,NCNTFLD1
              rep             zfill,NCNTFLD1
              move            C3,NCNTPATH
               TRAP           IOmssg GIVING ERROR IF IO
              move            "NCNTKEY",Location
              pack            KeyLocation,"Key: ",NCNTFLD1
              call            NCNTKEY
              if over
               move           C2,CNTPRINT    .Laser 3
              endif
              move            CNTPRINT,PRTFLAG
              call            Trim using CNTNAME
              if (CNTNAME <> "")
               move           CNTNAME,str1
               scan           B1,CNTNAME
               if equal
                              bump           CNTNAME,1
                              call           RemoveChar using CNTNAME,B1
               endif
               pack           str6,CNTNAME
               clear          userlogn2
               pack           userlogn2,str1,str6
              endif
              reset           CNTNAME
              move            C1,NCNTPATH
               Call           GetWinVer
               move           "USER" to userinfo
               clock          env,userinfo
               scan           "LOGIN" in  userinfo
               if             equal
               bump           userinfo by 6
               clear          userlogw
               move           userinfo,userlogw
               MOVEFPTR       userlogw TO BEGIN
               SCAN           "," IN userlogw
                              if             equal
                              MOVEFPTR       userlogw TO LAST
                              SUB            C3 FROM LAST
                              RESET          userlogw
                              SETLPTR        userlogw TO LAST
                              clear          userlogn
                              APPEND         userlogw TO userlogn
                              reset          userlogn
.              goto      mrunsw
.begin patch 10.0
                              GOTO           LognSet
.end patch 10.0
                              endif
               endif
               move           userinfo to userlogn
.begin patch 10.0
LognSet
.               move           c1 to ninvoutflag          .force details on for additional charges
               move           c2 to ninvoutflag          .force details on for additional charges
.Set Error Message Stat Text Boxes
               call    SetNInvErrorMssgDefault
. .................................................................
               CLOCK          DATE TO DATE
               CLOCK          DATE TO today
               UNPACK         DATE INTO SYSMO,STR1,SYSDAY,STR1,SYSYR
               move           CC,SYSCC
               REP            ZFILL,SYSDAY
               REP            ZFILL,SYSMO
               MOVE           SYSMO TO MM
               MOVE           SYSDAY TO DD
               MOVE           SYSYR TO YY
               CALL           CVTJUL
               MOVE           JULDAYS TO JDATE
               MOVE           SYSMO,HOLDMO
               MOVE           SYSDAY,HOLDDY
               MOVE           SYSYR,HOLDYR
.load acd combo
               Move           b1 to str3
               call           trim using str3
               clear          str45
               pack           str55,str3," - ",str45
               insertitem     InvComboBox003,0,str55
               loop
               call           nacdks
                              until          over
               move           nacdkey to str3
               call           trim using str3
               move           nacdtext to str45
               call           trim using str45
               pack           str55,str3," - ",str45
               move           str3 to n3
               insertitem     InvComboBox003,n3,str55
               repeat
              setitem         InvComboBox003,0,c0
               move           c1 to n1
               call           NInvTabChange
               setfocus       InvEditTextLR


               MATCH          "NINV0008" TO PROGRAM       *RETURNED FROM RUN CHARGE?
               IF             EQUAL
               MOVE           YES TO RUNSW
               MOVE           NO TO CORP
               MOVE           B1 TO MODE
.begin patch 10.014
               call           NInvForceToOne
               call           InvButtonDefault
               setfocus       InvEditTextLR
.end patch 10.014
               ENDIF
.Set Vars used for About Box
               move           "NInv0001.PLS",Wprognme
               Move           "NINV0001" to PROGRAM                   .needed to talk to NINV0008
               move           "Invoicing PRogram",Wfunction
               move           "David Herrick",Wauthor
               move           Release,Wrelease
               move           Reldate,Wreldate

               formload       Abt

               Move           No to Corp
               if             (runsw = yes)
               clear          Runsw
               goto           Main
               endif

               move           "I",progcode
              getitem         OptionsPassInit,0,N5
              if             (N5 = 0)
               setprop        Passwrd,visible=1
               endif

Main
               EVENTREG  X, 17, XRESIZE

               loop
               waitevent
               setitem        timer,0,18000   .reset to 30 minutes
               repeat
               goto           timeout

BadDate        alert   caution,"Date is not ok, Program will terminate",result
               stop

Seeadj1
               SUB            FARADJ,FARADJ
               move           ASRECADJ,faradj
               compare        c0 to faradj
               GOTO           MVEP1J IF less
               COMPARE        C0,FARADJ
               GOTO           MVEP1J IF EQUAL
MVEP1J
        SUB FAP1AJ,FAP1AJ
        move      ASPAYAD1,fap1aj
        compare   c0 to fap1aj
        GOTO      MVA2CN IF less
        COMPARE      C0,FAP1AJ
        GOTO      MVA2CN IF EQUAL
MVA2CN
        SUB FAP2AJ,FAP2AJ
         move        aspayad2 to fap2aj
        compare   c0 to fap2aj
        goto      mvlrj if less
        COMPARE   C0,FAP2AJ
        GOTO      MVA3cn IF EQUAL
MVA3CN
        SUB FAP3AJ,FAP3AJ
         move        aspayad3 to fap3aj
        compare   c0 to fap3aj
        goto      mvlrj if less
        COMPARE   C0,FAP3AJ
        GOTO      MVLRJ IF EQUAL
MVLRJ
         move     ASLRINC to flradj
         move     ASNININC to fNINadj
        COMPARE   C0,FLRADJ
        GOTO      MVSTX IF EQUAL
        GOTO      MVSTX IF less
MVSTX
        move       asstax,fstxaj
        COMPARE   C0,FSTXAJ
        GOTO      MVCTX IF EQUAL
        GOTO      MVCTX IF less
MVCTX
         move      ascity to fctxaj
         COMPARE   C0,FCTXAJ
         GOTO      MVPST IF EQUAL
         GOTO      MVPST IF less
MVPST
         move      aspost,fpstaj
         COMPARE   C0,FPSTAJ
.begin patch         
.         GOTO      DISADFIG IF EQUAL
.         GOTO      DISADFIG IF less

MVXNIN    sUB       fxNINadj,fxNINadj         
          moVE      ASXNINC,fxNINadj
.end patch         
. ...............................
DISADFIG
.display the total of adjustments
         clear    str10
         move     faradj to str10
         setitem Ninv001Medittext001,0,str10             .ar
         clear    str10
         move     fap1aj to str10
         setitem Ninv001Medittext002,0,str10              .ap1
         clear    str10
         move     fap2aj to str10
         setitem Ninv001Medittext003,0,str10              .ap2
.
         clear    str10
         move     fap3aj to str10

         setitem Ninv001Medittext004,0,str10              .inter company

.         setitem Ninv001Medittext004,0,"0.00"              .inter company
.
         clear    str10
         move     fxninadj to str10

         setitem Ninv001Medittext020,0,str10              .inter company

         clear    str10
         move     flradj to str10
         setitem Ninv001Medittext005,0,str10              .lr
         clear    str10
         move     fNINadj to str10
         setitem Ninv001Medittext006,0,str10               .nin
         clear    str10
         move     fctxaj to str10
         setitem Ninv001Medittext007,0,str10
         clear    str10
         move     fstxaj to str10
         setitem Ninv001Medittext008,0,str10
         clear    str10
         move     fpstaj to str10
         setitem Ninv001Medittext009,0,str10
         clear   str8
         UNPACK    ASCRDTE INTO str2,yradj,MOADJ,DAYADJ
         pack    str8 from moadj,slash,dayadj,slash,yradj
         setitem Ninv001Medittext010,0,str8
         setitem Ninv001Medittext011,0,asamnum
         clear   str8
         setitem Ninv001Medittext012,0,LRN
.ok lets see what it did to the invoice
         move      c0 to form102
         add       ar to form102
         ADD       FARADJ,form102
         clear     str14
         move      form102 to str14
         setitem Ninv001Medittext013,0,str14          .adjusted ar


         move      c0 to form102
         add       lrinc to form102
         ADD       FLRADJ,form102
         clear     str14
         move      form102 to str14
         setitem Ninv001Medittext017,0,str14          .adjusted LR

         move      c0 to form102
         add       NINinc to form102
         ADD       ASNININC,form102
         clear     str14
         move      form102 to str14
         setitem Ninv001Medittext018,0,str14          .adjusted NIN

         move      c0 to form102
         add       XNINinc to form102
         ADD       ASXNINC,form102
         clear     str14
         move      form102 to str14
         setitem   ninv001mEditText0019,0,str14          .adjusted XNIN

         move      c0 to form102
         add       ap1 to form102
         add       fap1aj to form102
         clear     str14
         move      form102 to str14
         setitem Ninv001Medittext014,0,str14          .adjusted AP1


         move      c0 to form102
         add       ap2 to form102
         add       fap2aj to form102
         clear     str14
         move      form102 to str14
         setitem Ninv001Medittext015,0,str14          .adjusted AP2

         move      c0 to form102
         add       ap3 to form102
         add       fap3aj to form102
         clear     str14
         move      form102 to str14
         setitem Ninv001Medittext016,0,Str14          .adjusted AP3
.         setitem Ninv001Medittext016,0,"0.00"          .adjusted AP3
         setprop NINV001MM,visible=1
.temp DH
          return
.end temp
.; *********   READS SPEC.FILE FOR INV. #,DISPLAYS INV. # and DATE  ********;
MUVBLAD
               MOVE           "NINVDTE" TO GNXTFLD
               CALL           GNXTKEY
               MOVE           GNXTNUM TO CINVNO
               MOVE           "NINVNXT" TO GNXTFLD
               CALL           GNXTKEY
               MOVE           GNXTNUM TO NINVNO
.begin patch 10.00
.               FILEPI         6;PINVOICE,NINVFILE,NINVFIL2
.end patch 10.00
               DIVIDE         "10" INTO CINVNO
               MOVE           CINVNO TO CHKDATE
               COMPARE        CHKDATE TO JDATE
               GOTO           BADDATE IF LESS
NINADD         MOVE           NINVNO,INVNUM
               ADD            "1",NINVNO
. ..
WRTINVNUM
               move           sysmo to invdtem
               move           sysday to invdted
               move           sysyr to invdtey
               move           syscc to invdtec
.               MOVE           YES,WRTORD
               REP            ZFILL,INVNUM
               filepi         2;ninvfil2
               READ           NINVFIL2,INVNUM;;
               GOTO           NINADD IF NOT OVER
               MOVE           "NINVNXT" TO GNXTFLD
               CALL           GNXTKEY
               MOVE           INVNUM TO GNXTNUM
. temp Test
.               OPen           Tempinv,"\\nins1\e\data\index\nininv"
.               Filepi         1;tempinv
.               readtab        Tempinv,lrn;*tab=106,STR6
.               move           str6 to Invnum
.               close          Tempinv
               TRAP           IOmssg GIVING ERROR IF IO
               move           "WrtInvNum - GNxtupd",Location
               pack           KeyLocation,"Key: ",Gnxtkey,b1,gnxtnum
               CALL           GNXTUPD

. temp Test
               call           wipecvars
               move           olrn to nmrgfld
               move           c0 to nmrgrqty
               move           c0 to nmrgiqty
               move           c0 to nmrgnet
               move           no to mrgsw
               move           no to SHIPsw
               call           nmrgkey
               if             not over
               move           yes to mrgsw
               endif
               move           olrn to nshpfld
               call           nshpkey
               if             not over
               move           yes to shipsw
               endif
               move           yes to subppsw
               call           compute
               MOVE           FORMAR TO AR
               move           ap to ap1
               move           "F" to code
               move           c0 to statb
.begin patch 10.00
.               FILEPI         1;PINVOICE,NINVFILE,NINVFIL2
.end patch 10.00
               move           c1 to ninvpath
               TRAP           IOmssg GIVING ERROR IF IO
               move           "WrtInvNum - Ninvwrt",Location
               pack           KeyLocation,"Key: ",NInvFld
               call           ninvwrt
.begin patch 10.5
               call           InvUpdateTypistTable using c1
.end patch 10.5

.               
               Move           c0 to billtn
               move           c0 to paytn          --- need to accomodate
.test if there are infact any
               Move           Invnum to NInvAcdINV
.
               FOR           AcdRecCount,"1","15"
.begin patch 10.007
.                              MOve           NInvAcdRec(AcdRecCount).NinvAcdNumRec TO NinvAcdNum
                              MOve           NInvAcdRec(AcdRecCount).NinvAcdCodeRec TO NinvAcdCode
                              MOve           NInvAcdRec(AcdRecCount).NinvAcdRateRec TO NinvAcdRate
                              MOve           NInvAcdRec(AcdRecCount).NInvAcdPercRec to NInvAcdPerc
                              MOve           NInvAcdRec(AcdRecCount).NINVAcdANINCDRec to NINVAcdANINCD
                              MOve           NInvAcdRec(AcdRecCount).NinvAcdAextcdRec to NinvAcdAextcd
                              Move           NInvAcdRec(AcdRecCount).NinvAcdRateTRec to NINvAcdRateT
                              MOve           NInvAcdRec(AcdRecCount).NINVAcdQtyRec to NINVAcdQty
                              if             (ninvacdCode = "" or NinvacdCode = "   ")
                              break
                              endif
                              Move            AcdRecCount to Ninvacdnum
                              count          n2,ninvacdnum
                              if             (n2 = c1)
                              pack           str3,c0,c0,ninvacdnum
                              elseif         (n2 = c2)
                              pack           str3,c0,ninvacdnum
                              Elseif          (n2 = c3)
                              move           Ninvacdnum to str3
                              endif
                              Move           Str3 to NInvAcdNUm
                              rep            zfill in NInvAcdNUm
               packkey        Ninvacdfld from INvnum,NinvAcdnum
.end patch 10.007
               rep            Zfill in ninvacdfld
               rep            Zfill in ninvacdNum
               TRAP           IOmssg GIVING ERROR IF IO
               move           "WrtInvNum - Ninvacdwrt",Location
               pack           KeyLocation,"Key: ",NInvAcdFld
               Call           NInvAcdWRT
               repeat

.
               MOVE           C0,STATB
               MOVE           "NINVDTE" TO GNXTFLD
               CALL           GNXTKEY
               PACK           GNXTNUM FROM JDATE,C0
               MOVE           "NINVDTE" TO GNXTKEY
. temp Test
               TRAP           IOmssg GIVING ERROR IF IO
               move           "WrtInvNum - Gnxtupdt",Location
               pack           KeyLocation,"Key: ",gnxtfld,b1,gnxtkey
               CALL           GNXTUPD
. temp Test
. ...WRITE  TO  PRINT FILE ......................
WRTPRNT
.begin patch 10.008
              Move            statb to HoldStatB
.end patch 10.008
               MOVE           C0 TO STATB
               CMATCH         "R" TO REPRINT
               GOTO           WRTPRNTA IF NOT EQUAL
               MOVE           REPRINT TO STATB
WRTPRNTA
.begin patch 10.00
.               FILEPI         5;PINVOICE
.end patch 10.00
               move           "F" to CODE
. temp Test
.begin patch 10.00
               TRAP           IOmssg GIVING ERROR IF IO
               move           "WrtPrntA - Pinvwrt",Location
               pack           KeyLocation,"Key: ",NInvFld
               Call           PInvWrt
.               write     pinvoice,ninvfld;invvars,inits
.end patch 10.00
. temp Test
.
               move      c0 to n1
               move      ownnec to n1
               COMPARE   C0 TO n1           .MULTIPLE COPIES WANTED?
               GOTO      WRTPRTX IF EQUAL       .NO.
               SUB       "1" FROM n1
               move      n1 to ownnec
               GOTO      WRTPRNTA
. ...
. ...
WRTPRTX
.begin patch 10.008
              if              (reprint = "R")
              move            HoldStatb to Statb
              else
              MOVE      C0 TO STATB
              endif
.              MOVE      C0 TO STATB
.end patch 10.009

               MOVE      C0 TO REPRINT
. ...
               setitem        InvEditTextInv,0,invnum
               Clear          str10
               pack           str10 from sysmo,slash,sysday,slash,syscc,sysyr
               setitem        InvEditText009,0,str10
.begin patch 10.011
.              reset           latelo
.              scan            loinvn in latelo
.              if              equal
.              call            loinvmem
.              endif
.end patch 10.011
.begin patch 10.0
.        if         (mode = "I" or admod = "2" )   .correct mode & prepay?
.               If             (mode = c0 or mode = c1)     ;verf admod 2 stuff
.test test test DLH 14sep05
.begin patch 10.008
               If             (mode = c0 or mode = c1)     ;Inquire mode - must have called MuvSYS for reprint
               return
               Elseif         (Mode = c2)

.               If             (mode = c0 or mode = c1 or mode = c2)     ;verf admod 2 stuff
.end patch 10.008
.end patch 10.0
        else
         cmatch    yes to ppsw                  .set in compute
         if        equal
         move      z3 to mcnt
           PACK      NMOAFLD FROM mlrn,mcnt
         move      c4 to nmoapath
         MOVE      oLRn TO LRNUM
         PACK      NMOAFLD4 FROM obrknum,mlrn
         REP       ZFILL IN NMOAFLD4
         pack      recdate from syscc,sysyr,sysmo,sysday
         pack      trandate from syscc,sysyr,sysmo,sysday
         move      "18" to reason
         move      mlrn to mlr
         move      mlrn to nmobmlr
         move      mcnt to nmobmcnt
         move      b1 to control
         move      invnum to invoice
         pack      invdate from syscc,sysyr,sysmo,sysday
         MOVE      C0 TO ONAMOUNT
         type      obrknum
         if        equal
         MOVE      oBRKnum TO NMOABRK
         move      oBRKnum to nmobbrk
         else
         MOVE      "0000" TO NMOABRK
         move      "0000" to nmobbrk
         endif
         move      c0 to change
         move      prepay to change                    .from compute
         move      prepay to onamount
         move      olnum to LIST
         CLEAR     CHECKNUM
           MOVE      "NONANXT" TO GNXTFLD
           CALL      GNXTKEY
           MOVE      GNXTNUM TO n7
ADDONEB  ADD       C1 TO n7
         MOVE      n7 TO TRANSNUM
         REP       ZFILL IN TRANSNUM               .dlh 13mar95
         MOVE      TRANSNUM TO NMOAFLD
         MOVE      C2 TO NMOAPATH
         CALL      NMOATST
         GOTO      ADDONEB IF NOT OVER
           MOVE      n7 TO STR7
           BUMP      STR7 BY 1
           MOVE      STR7 TO GNXTNUM
           REP       ZFILL IN GNXTNUM
.temp test
               TRAP           IOmssg GIVING ERROR IF IO
               move           "ADDOneB - GNXTUPD",Location
               pack           KeyLocation,"Key: ",Gnxtfld,b1,gnxtnum

              CALL      GNXTUPD
.NOTE GNXT ONLY ALLOWS 6 BYTE NUMBER. DLH.
.temp test
         MOVE      "0" TO INAMOUNT
         move      olnum to LIST
         MOVE      C2 TO NMOBPATH
.begin 10.015
               TRAP           IOmssg GIVING ERROR IF IO
              move           "AddoneB - NMobUpd",Location
              pack           KeyLocation,"Key: ",NMoafld
              Move            yes to NMobOkFlag
              CALL       NMOBUPD
              IF         (Code96OK = No)
              alert   caution,"This entry would result in a Negative Balance.  Balance NOT applied!",result
              alert   caution,"Please Delete the code 96 !!!!!! Balance NOT applied!",result
              Move            no to NMobOkFlag
              return                    ??????????????????????
.ultimtaely add code to auto remove the add charge
              endif

.;end 10.015
.temp test
         MOVE       INITS TO NMOAINIT
         MOVE       mlrn TO mlr
.temp test
               TRAP           IOmssg GIVING ERROR IF IO
               move           "AddOneB - NMoawrt",Location
               pack           KeyLocation,"Key: ",NMOAFLd
.begin patch 10.08
         Move       OCompID,MoaCOmp
.end patch 10.08
               
               CALL           NMOAWRT
.temp test
         endif
         endif
         MOVE      HOLDMO,SYSMO
         MOVE      HOLDDY,SYSDAY
         MOVE      HOLDYR,SYSYR
               If             (Mode = C3)                  .Add Mode
.begin patch 10.011
              reset           latelo
              scan            loinvn in latelo
              if              equal
              call            loinvmem
              endif
.end patch 10.011
               Goto           UpdOrd
               endif
.         MATCH     "Q",ostat
.         GOTO      UPDORD IF EQUAL
.         MATCH     "0",OSTAT
.         GOTO      PATCH1 IF EQUAL
.         MATCH     "X",OSTAT
.         GOTO      PATCH2 IF EQUAL
.PATCH1   MOVE      "B",OSTAT
.         GOTO      UPDORD
.PATCH2   MOVE      "Q",OSTAT
.         GOTO      UPDORD
. ******  MODIFICATION OF RECORDS  ***********************************
UPDATER
.Need to add the code to check for changes etc

               move           "F" to code
.Need to unload the acd object --- changed ? delete all acd rec's and write out the new ones
               TRAP           IOmssg GIVING ERROR IF IO
.begin 10.006
.               move           "Updater - NinvUpd",Location
.               pack           KeyLocation,"Key: ",NInvFld
.               call           ninvupd
.end 10.006
               if             (ModAcdFlag = yes)

               FOR           n3,"1","15"

               move           N3 to str3
               rep            zfill in str3
               packkey        Ninvacdfld from INvnum,str3
               rep            Zfill in ninvacdfld
               TRAP           IOmssg GIVING ERROR IF IO
              move            "NInvAcdTst",Location
              pack            KeyLocation,"Key: ",Ninvacdfld
               Call           NInvAcdtst
                              if             Not OVer
                              move            "NInvAcdDel",Location
                              pack            KeyLocation,"Key: ",Ninvacdfld
                              Call           NInvAcdDel
                              endif
               repeat
               Move           No to Modacdflag
.
               FOR           AcdRecCount,"1","15"
                              MOve           NInvAcdRec(AcdRecCount).NinvAcdNumRec to NinvAcdNum
                              MOve           NInvAcdRec(AcdRecCount).NinvAcdCodeRec to NinvAcdCode
                              MOve           NInvAcdRec(AcdRecCount).NinvAcdRateRec to NinvAcdRate
                              MOve           NInvAcdRec(AcdRecCount).NInvAcdPercRec to NInvAcdPerc
                              MOve           NInvAcdRec(AcdRecCount).NINVAcdANINCDRec to NINVAcdANINCD
                              MOve           NInvAcdRec(AcdRecCount).NINVAcdQtyRec to NINVAcdQty
                              MOve           NInvAcdRec(AcdRecCount).NinvAcdAextcdRec to NinvAcdAextcd
                              Move           NInvAcdRec(AcdRecCount).NinvAcdRateTRec to NINvAcdRateT
                              call           trim using NInvAcdNum
.begin patch 10.007
                              IF             (Ninvacdcode = "")      ;check to see if goodies
.end patch 10.007
                              break
                              Else
.17Feb2005 DLH
               Move           AcdRecCount to NinvAcdNum         .use count (records may have been delete/added)
                              count          n2,ninvacdnum
                              if             (n2 = c1)
                              pack           str3,c0,c0,ninvacdnum
                              elseif         (n2 = c2)
                              pack           str3,c0,ninvacdnum
                              Elseif          (n2 = c3)
                              move           Ninvacdnum to str3
                              endif
                              Move           Str3 to NInvAcdNUm
                              rep            zfill in NInvAcdNUm
                              packkey        Ninvacdfld from INvnum,NInvAcdNum
                              rep            Zfill in ninvacdfld
                              rep            Zfill in NinvacdCode
                              MOve           Invnum to NinvAcdINV
                              rep            Zfill in NinvacdInv
                              move           "Updater - Ninvacdwrt",Location
                              pack           KeyLocation,"Key: ",NInvAcdFld
                              Call           NInvAcdwrt
                              endif
               repeat

               endif
.begin 10.006
               call           wipecvars
               if             (frcCompFlag = yes)
               move           olrn to nmrgfld
               move           c0 to nmrgrqty
               move           c0 to nmrgiqty
               move           c0 to nmrgnet
               move           no to mrgsw
               move           no to SHIPsw
               call           nmrgkey
                             if             not over
                             move           yes to mrgsw
                             endif
               move           olrn to nshpfld
               call           nshpkey
                             if             not over
                             move           yes to shipsw
                             endif
               endif
               move           yes to subppsw        ??????????????
               call           compute
               MOVE           FORMAR TO AR
               move           ap to ap1
               move           "Updater - NinvUpd",Location
               pack           KeyLocation,"Key: ",NInvFld
               call           ninvupd
.begin patch 10.5
               call           InvUpdateTypistTable using C2
.end patch 10.5
.end 10.006
. .......
.read print file
READPIN        MOVE      NINVFLD TO HOLDKEY
.begin patch 10.0
.               FILEPI    1;PINVOICE
.               READ      PINVOICE,NINVFLD;CODE,REPRINT
               call           PinvTst
               Goto           Doyou if Over
               unpack         str2 into Code,Reprint
.               Clear          HoldINv
.               pack           Holdinv from invvars,inits                  .Save record details
.               Call           PINVKey
.;end patch 10.0
.               If             Over
.               Unpack         Holdinv into invvars,inits                   .restore record
.               Alert          Plain," REPRINT THIS INVOICE ?",Result
.                              IF             (result <> c1)
.                              Move           B1 to Reprint
.                              goto           UpDord
.                              Else
.                              Move           "R" to Reprint
.                              endif
.               endif
.begin patch 10.0
.was not over lets see what to do
.               MOVE      C0 TO STATB
.               CMATCH    "R" TO REPRINT                                  .not using this var anymore
.               CMATCH    "R" TO StatB                                     .All ready in the file and New?
.;               GOTO      READPINA IF NOT EQUAL                            .YES
.               If             Not Equal
.               Unpack         Holdinv into invvars,inits                   .restore record
.               goto           ReadPina
.               endif
.
               MOVE           C0 TO STATB
               CMATCH         "R" TO REPRINT
               GOTO           READPINA IF NOT EQUAL
               MOVE           "R" TO STATB    *THIS IS A REPRINT.

.end patch 10.0
READPINA
               move      "F" to code
.begin patch 10.0
.               FILEPI    6;PINVOICE
.               UPDATE    PINVOICE;invvars,inits
               pack           Holdinv from invvars,inits                  .Save record details
               move           "ReadPina - PinvUpd",Location
               pack           KeyLocation,"Key: ",NInvFld
               Call           PINVUPD
.end patch 10.0
               move           c0 to n1
               move           ownnec to n1
               COMPARE        C0 TO n1
               GOTO           RDPINAX IF EQUAL
.begin patch 10.0
.               READKS    PINVOICE;FILL6,LRN;
               Call           PinvKS
               GOTO           RDPINAX IF OVER
               MATCH          LRN TO HOLDKEY
               IF             Equal
               Unpack         Holdinv from invvars,inits                  .Restore record details
               GOTO           READPinA
.               GOTO      READPINA IF EQUAL
               Endif
RDPINAX
.               MOVE      C0 TO REPRINT
.end patch 10.0
               MOVE           C0 TO STATB
. .....
.begin patch 10.0
UPDORD
               If             (Mode = c3 & Ostat = "0")
               Move           "B" to Ostat
               Endif
               If             (Mode = c3 & Ostat = "X")
               Move           "Q" to Ostat
               endif
.             call            IntegralStoreDetail using OLRN,C1
               move           "UpdOrd - Updatab Nordfile",Location
               pack           KeyLocation,"Key: ",NordFld
               FILEPI    1;NORDFILE
.end patch 10.0
.temp test
               UPDATAB    NORDFILE;*1,"S",OSTAT
.               UPDATAB    NORDFILE;*1,"S"
.              call            IntegralStoreDetail using OLRN,C1
.               call            IntegralTestDetail using OLRN,C1

.temp test
. ..
               setprop       InvSearchQuit,Height=0
               setprop       InvSearchGo,Height=0
               setprop       INvSearch,Height=20
               setprop       INvok,Height=20
               setfocus      InvoiceListView
.begin patch 10.0
               If             (Mode != c3)                     .If not add mode
               return
               Endif
               IF         (Olnum = "018492")
                              alert   caution,"No LO invoice Required per SA 12/04",result
               endif

JDTEST
               IF             (NDatTDmc = B1 or NDatTdmc = "")
                              cmatch         "C" to elstcde
                              if             equal
                              Clear          Str255
                              pack           Str255 from "016130-","019539-","015102-","011507-","003252-","000995-","015503-016724-001106-018847-020565"
                              scan           Olnum in str255
                                             if             equal
                                             alert          caution,"Exclusive List with Special deal on selects",result
                                             ElseIF         (Olnum = "021703" or Olnum = "021716")
                                             alert          caution,"Excl List Split Comm with NIN",result
                                             ElseIF         (Olnum = "012594")
                                             alert          caution,"Excl List No NIN :(",result
                                             ElseIf         (Olnum = "021611")
                                             alert          caution,"Excl List All NIN :)",result
                                             ElseIf         (Olnum = "002479")
                                             alert          caution,"Exclusive List Matrix w/Special $1 deal on $6/m slcts ",result
.begin patch 10.022
                         Elseif         (ndat3cde = B1)     
.                                             Else
                          alert          caution,"Exclusive Bill selects from Donnelly Matrix",result
                         Elseif         (ndat3cde = "M" or ndat3cde = "R" or ndat3cde = "P")        
                         alert          caution,"Exclusive Bill selects from PDI/MMI/Frontline Matrix",result
                                             Else
                                             alert          caution,"Exclusive Bill selects from Matrix",result

.end patch 10.022                                             
                                             endif
.                              Clear          Str255
.                              pack           Str255 from "021703-","021716"
.;                              scan           Olnum in str255
.                                             if             equal
.                                             alert          caution,"Excl List Split Comm with NIN",result
.                                             endif
                              endif
.                              endif

               return
               endif
.
                              RESET          EXCODES
                              SCAN           OELCODE IN EXCODES
                              IF             EQUAL
                              cmatch         "R" to ndattdmc         *Do We do splits?
                                             if         equal                  *Yes
                                             move       c0 to n9               *Is
                                             move       oexqty to n9           *   This
                                             compare    c0 to n9               *        One ?
                                             return         if equal            *Nope get out
                                             endif

                                             If             (Mode = c3)                ;add mode
                                             Move           Yes to FrcCOmpflag
                                             Goto           RunnKey
                                             else
                                             Return
                                             endif
RunnKey
               Alert          Plain,"Bill RUN/Exchange CHARGES NOW?  ?",Result
               IF             (result = c1)
                              CLEAR          COMMENT
                              MOVE           C0 TO STR8
                              MOVE           C0 TO STR9          .ADDED THIS LINE TO INITIALIZE - ASH
                              MOVE           C0 TO N9          .ADDED THIS LINE TO INITIALIZE - ASH
                              move           squant to n9
                              compare        c0 to n9
                                             if         not equal
                                             move       squant to str9
                                           else
                                             move       oqty to str9
                                             endif
.                               PACK       COMMENT FROM NINVFLD,B1,STR9
.12Sep05 test dlh
                               PACK       COMMENT FROM Olrn,B1,STR9
.12Sep05 end test dlh
.                               RESET      COMMENT
                               CHAIN      "NINV0008"
                endif
.               ELSE
.
.begin patch 10.0
.can't Updatab a filelist file
.               FILEPI    1;NINVFILE
.               UPDATAB   NINVFILE;*1,"F"
.end patch 10.0
               ENDIF

.begin patch 10.0   --- below relevant?
               clear     addcd1
               clear     addcd2
               clear     addcd3
               clear     addcd4
               clear     addcd5
               clear     addcd6
               clear     addcd7
               clear     addcd8
               clear     addcd9
               clear     addcd10
               clear     addp1
               clear     addp2
               clear     addp3
               clear     addp4
               clear     addp5
               clear     addp6
               clear     addp7
               clear     addp8
               clear     addp9
               clear     addp10
               clear     addncd1
               clear     addncd2
               clear     addncd3
               clear     addncd4
               clear     addncd5
               clear     addncd6
               clear     addncd7
               clear     addncd8
               clear          addncd9
               clear          addncd10
               return
. ......
. ...**********************************
.think this section is defunct  DLH March 1 2004
GOTPASWD       If             (mode = c0 or mode = c1)
               Goto           RemVBusy
               endif
.               CMATCH         "I",MODE
.               GOTO           REMVBUSY IF EQUAL
GOT2           MOVE           NINVFLD TO HOLDKEY
.begin patch 10.00
.               FILEPI         1;PINVOICE
.               READ           PINVOICE,NINVFLD;;
               Call           PinvTst
.end patch 10.00
               GOTO           DOYOU IF OVER
               GOTO           REMVBUSY
DOYOU          MOVE           HOLDKEY TO NINVFLD
.               Setprop        InvButtonfax,height=20
.               Setprop        InvButtonHPRint,height=20
               Setprop        InvButtonRePRint,height=20
               Alert          Plain," REPRINT THIS INVOICE ?",Result
               IF             (result <> c1)
               MOve           B1 to Reprint
               goto           REMVBUSY
               Else
               Move           "R" to Reprint
               endif

MUVSYS
.begin patch 10.019
.               MOVE           invdtem,SYSMO
.               MOVE           invdted,SYSDAY
.               MOVE           invdtey,SYSYR
.end patch 10.019
.              MOVE           "R" TO REPRINT
               GOTO           WRTPRNT
.think this section is defunct  DLH March 1 2004   --- still in here somewhere

REMVBUSY
               move           "RemvBusy - Updatab - NINORD",Location
               pack           KeyLocation,"Key: ",NordFld
              FILEPI         1;NORDFILE
               UPDATAB        NORDFILE;*1,"S"
.begin patch 10.00
               if             (Mode = C3)
               return
               endif
.               CMATCH         YES,ADREC
.               return         if equal
.               MATCH          "**",COMMPCT
.               return         if equal
.end patch 10.00
.REMINV         CMATCH         "M",MINV
.begin patch 10.0 --- can't updatab a filelist file  --- not sure anyone busies inv file anyway
.               If             (NinvPath = c1)                ;by LR
.               FILEPI         1;NINVFILE
.               UPDATAB        NINVFILE;*1,"F"
.               ElseIF         (NinvPath = c2)                ;by Inv   else lost
.               FILEPI         1;NINVFIL2
.               UPDATAB        NINVFIL2;*1,"F"
.               endif
.               GOTO           REMINV0 IF NOT EQUAL
.               FILEPI         1;NINVFIL2
.               UPDATAB        NINVFIL2;*1,"F"
.               return
.End Patch 10.0
.REMINV0
.               FILEPI         1;NINVFILE
.               UPDATAB        NINVFILE;*1,"F"
.end patch 10.00
               return
*...........................................................
.
MAILNG         MOVE           "MAILTO  ",FILENAME
               GOTO           DISPISI
BROKMISS       MOVE           "BROKER  ",FILENAME
               GOTO           FILEMESG
INVMISS
               Clear          str50
               pack           str50 from "FILE 'INVNUM' IS MISSING !"
               ALERT          cAUTION,str50,result
               GOTO           REMVBUSY
DUPKEY
               Clear          str50
               pack           str50 from "DUPLICATE KEY ATTEMPT!"
               ALERT          cAUTION,str50,result
               GOTO           REMVBUSY
OWNNG          MOVE           "OWNER  ",FILENAME
               GOTO           DISPISI
MAILMISS       MOVE           "MAILER  ",FILENAME
               GOTO           FILEMESG
ORDMISS        MOVE           "ORDER  ",FILENAME
               GOTO           FILEMESG
DISPERR BEEP
               Clear          str50
               pack           str50 from filename,b1,"ERROR ENCOUNTERED-THIS LR CANNOT BE CONTINUED!"
               ALERT          cAUTION,str50,result
KEYRECOG
               GOTO           REMVBUSY
NOFILES        ALERT          cAUTION,"ALL MASTER FILES NOT PRESENT!",result
               ALERT          cAUTION,Offline,result
               ALERT          cAUTION,ERROR,result
.begin patch 10.03
          Clear     Mailbody
          append    "This is a message from       the Invoice Program",Mailbody
          Append    CRLF,MailBody                                               
          Append    "All Master files not present",Mailbody
          Append    CRLF,MailBody                                               
          append    "LR =   ",mailbody
          append    Olrn,Mailbody
          Append    CRLF,MailBody                                               
          append    "Ninvfld =   ",mailbody
          append    ninvfld,Mailbody
          Append    CRLF,MailBody                                               
          Reset     MailBody
                    
          pack      Mailto,userlogn,"@nincal.com"
          pack      Mailfrom,userlogn,"@nincal.com"
          move      "This is an error message",Mailsubjct
          PAUSE     "120"

          call      SendMail
          



.               Move    "This is a Error e-mail from Ninv0001",SmtpSubject Subject
.
..   Set the text message that is send with the attachments
.
.       Move    "This is an error message",SmtpTextMessage(1)   Array <Text message >
.        Move    error,SmtpTextMessage(2)   Array <Text message >
.        Move    "ALL MASTER FILES NOT PRESENT!",SmtpTextMessage(3)   Array <Text message >
.        clear   smtpTextmessage(4)
.        append   olrn to smtpTextmessage(4)
.        append   b2 to smtpTextmessage(4)
.        append   ninvfld to smtpTextmessage(4)
.        reset    smtpTextmessage(4)
.
.        Move    "4",SmtpTextIndexLast                               Index to last entry in TextMessage array
.
.         call      errmesg
.end patch 10.03
        winshow
        STOP
FILEMESG
               Clear          str50
               pack           str50 from "Lr not found in ",filename,"!"
               ALERT          cAUTION,str50,result
               GOTO           REMVBUSY

INVNG          MOVE           "INVOICE",FILENAME
               GOTO           DISPISI
BROKNG         MOVE           "BROKER",FILENAME
               GOTO           DISPISI
ORDNG          MOVE           "ORDER",FILENAME
DISPISI        Clear          str50
               pack           str50 from "CHECK ISI FILE: ",filename,b1,olrn,b1,ninvfld
               ALERT          cAUTION,str50,result
               GOTO           KEYRECOG
RECBUSY
               Clear          str50
               pack           str50 from "LR: ",ninvfld," Is in use!!"
               ALERT          cAUTION,str50,result
               return
. *****      CLEAR   BUSY   SIGN   **********
ORDBUSY
.begin patch 10.00
.               CMATCH    YES,ADREC
.               GOTO      RECBUSY IF EQUAL
               If             (Mode = C3)
               Goto           REcBusy
               endif
.cant updatab a filelist file
.               If             (NinvPath = C1)            ;lr
.               FILEPI    1;NINVFILE
.               UPDATAB   NINVFILE;*1,"F"
.               ElseIf         (NinvPath = c2)               :inv
.               FILEPI    1;NINVFIL2
.               UPDATAB    NINVFIL2;*1,"F"
.               endif
.end patch 10.00
.               CMATCH    "M",MINV
.               GOTO      FINV IF NOT EQUAL
.               FILEPI    1;NINVFIL2
.               UPDATAB    NINVFIL2;*1,"F"
.               GOTO      RECBUSY
.
.FINV
.               FILEPI    1;NINVFILE
.               UPDATAB   NINVFILE;*1,"F"
.end patch 10.00
               GOTO      RECBUSY
.
.*******************************************************************************
.HOTinv - DO REAL TIME PRINT * NOW!
HOTinv         CLEAR          TASKNAME
.TEMP TEMP TEMP
.               MOve           "Dherric" to user
.               Move           "DH" to INits
.               move           "Dherric" to userlogn2
.begin patch 10.05
          IF        (LRN = "" or LRN = "      ")
            ALERT          Plain,"I can't NO LR ##!!!!",result
          return
          endif     
.end patch 10.05
               aLERT          Plain,"Shall I PDF it for you?",result
               If             (result = c1)
.begin patch 10.017
.;   append     "!\\nts0\c\apps\winbatch\butil job=HOTINV",TASKNAME
.              append          "!c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 \\nts0\c\apps\winbatch\butil job=HOTINV",taskname
.dh test 10/20/08
              append          "!\\Nins1\Winbatch\butil job=HOTINV",taskname
.begin patch 10.017
.             append          "!c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\butil job=HOTINVDev ",taskname
.             append          "!c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\butil job=HOTINV",taskname
               move           "P",str2
.               move           "P",prtflag
              else
.             append          "!\\nts0\c\apps\winbatch\butil job=HOTINVdev ",TASKNAME
.begin patch 10.017
.;              append          "!\\nts0\c\apps\winbatch\butil job=HOTINV",TASKNAME
.dh test 10/20/08
.              append          "!c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 \\nts0\c\apps\winbatch\butil job=HOTINV",taskname
              append          "!\\Nins1\Winbatch\butil job=HOTINV",taskname
.begin patch 10.017
              move            PRTFLAG,str2
              endif

              append          " infile=",taskname
              append          lrn,taskname
              append          str2,taskname
              append          inits,taskname
              append          " F=default C=1",TASKNAME
              append          " B=",TASKNAME
.             append          userlogn2,TASKNAME
               APPEND         user TO TASKNAME
               RESET          TASKNAME
               EXECUTE        TASKNAME
erasepdf
.   pause      "10"
               move           "                                        ",APIFileName
              clear         APIFileName
              pack          APIFileName,"C:\WORK\PDF\",LRN,"P",inits,".pdf",hexzero
               call          DeleteFile
              if            (APIResult = 0 | APIResult = hexeight)
              endif
               move           "                                        ",APIFileName
              clear         APIFileName
              pack          APIFileName,"C:\WORK\PDF\",LRN,"P",inits,"LO.pdf",hexzero
               call          DeleteFile
              if            (APIResult = 0 | APIResult = hexeight)
              endif

               RETURN
.*******************************************************************************
.faxinv - Fax MLR COpy
faxinv
               cmatch         "C" to elstcde
               if             equal
               aLERT          Caution,"Exclusive list NOT allowed!!",result
               return
               endif
               RESET          WHITNEY
               SCAN           OWNNUM IN WHITNEY
               if             equal
               aLERT          Caution,"Whitney list NOT allowed!!",result
               return
               endif

               CLEAR          TASKNAME
.               append         "!f:\apps\winbatch\butil job=HOTINV1Dev ",TASKNAME
               append         "!\\Nins1\Winbatch\butil job=HOTINV1",TASKNAME
               APPEND         " infile=",taskname
               append         lrn,taskname
               append         inits to taskname
               append         " F=default C=1",TASKNAME
               APPEND         " B=",TASKNAME
               APPEND         user TO TASKNAME
               RESET          TASKNAME
               EXECUTE        TASKNAME
               RETURN
.*******************************************************************************
.*******************************************************************************
loinvmem
              move            "750",column
              move            "1750",column1
              move            "3000",column2
               move           c1 to faxflag                 .reset flag
               match          "0000000000",ownfax2
               goto           ownfax if equal
               type           ownfax2
               goto           ownfax if not equal
               move           ownfax2 to ownfax
ownfax
               TYPE           OWNFAX                        .VALID PHONE?
.New code DH 16Aug2010
               IF             (ownfax = "0000000000" & Ownfax2 = "0000000000")
               aLERT          Caution,"No fax number for List owner",result
               endif
.New code DH 16Aug2010

               IF             EQUAL
               aLERT          Plain,"Fax to List owner ? ",result
                              If             (Result = c1)
                              move      c2 to faxflag
                              endif
               else
               aLERT          Caution,"No fax number for List owner",result
               return
               endif
               branch         faxflag of nomem,faxmem
nomem          return
faxmem         move           c1 to nusepath
               move           c0 to nusefld
               MOVE           PORTN TO NUSEFLD
               REP            ZFILL IN NUSEFLD
               CALL           NUSEKEY
               goto           userng if over
               scan           "INVALID" in nuseuser
               goto           userng if equal
               reset          nuseuser
               COUNT          N2,OWNFAX
               COMPARE        C10 TO N2
               IF             EQUAL
               MOVE           C1 TO LONGDIST
               UNPACK         OWNFAX INTO STR3,STR7
               match          "510" to str3            . local?
                              IF         EQUAL
                              MOVE       STR7 TO OWNFAX
                              CLEAR      LONGDIST
                              endif
               endif
FILENAM2       CLEAR          formname
               APPEND         "NINVMEM",formNAME
               RESET          formname
               CLEAR          RECNAME
               clear          recname
               append         NTWKPATH1,recname
         append    formname to recname
         append    ".dat" to recname
         reset     recname
.begin patch 10.15
.WHY???????
.         SPLOPEN    recname
.end patch 10.15
         clock     date to date
        compare    c0,ap2
         if        equal
         move     ap1 to ownpay
         else
         call     nownkey
         move      ap2 to ownpay
         endif
         unpack    date into mm,str1,dd,str1,yy
         clock     time to time
         clear     str5
         append    time to str5
         reset     str5
.begin patch 10.15
.Prepare Flag file

.begin patch 10.4
.                                                  call      GetPDfPAth
.          call      PDF995Auto
.          call      SetPDFAuto
.          call      setpdfflag                    
..                                                  pack      str45 from PDFPATH,"\flag.dat"
..                                                  prep      tempfile,STR45
..                                                  write     tempfile,SEQ;"flag set"
..                                                  close     tempfile
..          PRTOPEN   prfile,"PDF995","LoMemo"
.begin patch 10.83
.          pack      str25 from "LoMemo",olrn
.          rep       zfill in str25
.          pack      str55 from "c:\work\pdf\",str25,".pdf"
.          erase     str55
..          PRTOPEN   prfile,"PDF995",Str25
.          PRTOPEN   prfile,"PDF:",Str55
..end patch 10.4
.
..              PRTOPEN prfile,"FAXFILE","FAXFILE.PRN"
..end patch 10.15
.              prtpage         prfile;*UNITS=*HIENGLISH;
.end patch 10.83

.begin patch 10.06
.          IF        (OCompID = "P")
.          call      debug
.          prtpage   prfile;*p=218:25,*font=font018b,"Pacific Lists, Inc.":
.                    *p=625:343,*font=font07,"180 Grand Avenue, Suite 1545":
.                    *p=625:443,"Oakland, CA 94612-3799":
.                    *p=460:543,"415-945-9450 ","·"," Fax 415-945-9451":
.                    *p=460:643,"A Division of Names in the News"
.          Else
.           prtpage         prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.end patch 10.06
.          endif              
.begin patch 10.83
           Clear      Mailbody
.begin patch 10.84
           append     "<!DOCTYPE html PUBLIC #"-//W3C//DTD HTML 4.01 Transitional//EN#"#"http://www.w3.org/TR/html4/loose.dtd#">",Mailbody   
           append     HexCRLF,mailbody
           append     "<html><Head>",Mailbody
           append     "<meta name=#"generator#" content=#"HTML Tidy for Linux (vers 25 March 2009), see www.w3.org#">",mailbody
.           append     "<!DOCTYPE HTML PUBLIC #"-//W3C//DTD HTML 4.01 Transitional//EN#">",Mailbody   
.           append     "<html>",Mailbody
.           append     "<head>",Mailbody
.end patch 10.84
.           append     "<meta http-equiv=#"content-type#" content=#"text/html; charset=UTF-8#">",Mailbody         
.           append     "<meta http-equiv=#"Content-Type: text/html; charset=ISO-8859-1 Content-Transfer-Encoding: base64#">",Mailbody         
           append     "<title>REQUEST FOR INVOICE</title>",Mailbody
           append     "</head>",Mailbody
.begin patch 10.84
           append     HexCRLF,mailbody
.end patch 10.84
           append     "<body>",Mailbody
           append     "<h2 align=#"center#">REQUEST FOR INVOICE</h2>",Mailbody
           append     "<br><br>",mailbody
           append     "Date: ",mailbody
           pack       str10 from mm,"/",dd,"/",CC,yy
           append     str10,mailbody
           append     "<br><br>",mailbody
           append     "TO: ",mailbody
           append     OWNOCPY,mailbody
           append     "<br><br>",mailbody
           append     "FROM: ",mailbody
           append     Nuseuser,mailbody
           append     " - Accounting Department",Mailbody
           append     "<br><br>",mailbody
           append     "<u>RE:  BILLING STATUS/NOTIFICATION</u>",mailbody           
           append     "<br><br>",mailbody
           append     "LR##: ",mailbody
           append     ninvfld,mailbody
           append     "     Maildate: ",mailbody
           clear      str10
           pack       str10 from omdtem,"/",omdted,"/",OMDTEC,omdtey
           append     str10,mailbody
           append     "<br><br>",mailbody
           append     "Mailer: ",mailbody
           append     Mcomp,mailbody
           append     "<br><br>",mailbody
           append     "List: ",mailbody
           append     o1des,mailbody
           append     "<br><br>",mailbody
           append     "To date we have not received an invoice for our above noted mailer. Our calculations reflect the amount due to you to be $ ",mailbody
           append     ownpay,mailbody
           append     ".",mailbody
           append     "<br><br>",mailbody
           append     "In order to verify this amount, please send a copy of your invoice to Names in the News:  ",mailbody
.begin patch 10.84
           append     HexCRLF,mailbody
.end patch 10.84
              move            c3 to ncntpath
              move            portn to ncntfld1
              REP             ZFILL IN ncntfld1
              call            ncntkey
              move            c3 to ncntpath
              squeeze         cntname,cntname
              squeeze         cntphone,cntphone
              pack            EmailAddr with cntname,"@nincal.com"
           append     EmailAddr,mailbody
           append     " or Fax to (415)433-7796. ",mailbody
           append     "<br><br>",mailbody
           append     "If we do not hear from you within (3) working days we will consider this our final billing",mailbody
           append     " and we will be unable to accept any adjustments.  Payment to you will be based on",mailbody
           append     " the amount noted above.",mailbody
.begin patch 10.84
           append     HexCRLF,mailbody
.end patch 10.84
           append     "<br><br>",mailbody
           append     "If you have any questions or there is a problem in invoicing us immediately, please do",mailbody
           append     " not hesitate to contact me at ",mailbody
           append     EmailAddr,mailbody
           append     " or ",mailbody
           append     CNTPHONE,mailbody
           append     ".",mailbody
           append     "</body>",mailbody
           append     "</html>",mailbody
           reset      mailbody

.              prtpage         prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.              move            "1500",row
.              prtpage         prfile;*pcolumn2:row,*font=font012B,"REQUEST FOR INVOICE"
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              prtpage         prfile;*pcolumn2:row,*font=font012,"       VIA FACSIMILE"
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              prtpage         prfile;*pcolumn:row,*font=font012,"Date:"
.              prtpage         prfile;*pcolumn1:row,mm,"/",dd,"/",CC,yy
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              prtpage         prfile;*pcolumn:row,"TO:"
.              prtpage         prfile;*pcolumn1:row,OWNOCPY
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              prtpage         prfile;*pcolumn:row,"FROM:"
.              prtpage         prfile;*pcolumn1:row,NUSEUSER
.              add             sixlpi,row
.              prtpage         prfile;*pcolumn1:row,"Accounting Department"
.              add             eightlpi,row
.              add             eightlpi,row
.              prtpage         prfile;*pcolumn:row,*ULON,"RE:  BILLING STATUS/NOTIFICATION",*ULOff
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              prtpage         prfile;*pcolumn:row,"LR## ",ninvfld
.              prtpage         prfile;*pcolumn1:row,"Mail Date:  ",omdtem,"/",omdted,"/",OMDTEC,omdtey
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              prtpage         prfile;*pcolumn:row,"Mailer:"
.              prtpage         prfile;*pcolumn1:row,mcomp
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              prtpage         prfile;*pcolumn:row,"List:"
.              prtpage         prfile;*pcolumn1:row,o1des
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              prtpage         prfile;*pcolumn:row,"To date we have not received an invoice for our above noted mailer. Our calculations reflect"
.              add             tenlpi,row
.              add             twelvelpi,row
.              prtpage         prfile;*pcolumn:row,"the amount due to you to be $ ",ownpay," ."
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              prtpage         prfile;*pcolumn:row,"In order to verify this amount, please send a copy of your invoice to Names in the News to "
.              add             tenlpi,row
.              add             twelvelpi,row
..Patch 9.73 Code Added
..;
.              move            c3 to ncntpath
.              move            portn to ncntfld1
.              REP             ZFILL IN ncntfld1
.              call            ncntkey
.              move            c3 to ncntpath
..;
.              squeeze         cntname,cntname
.              squeeze         cntphone,cntphone
.              pack            EmailAddr with cntname,"@nincal.com"
..end Patch 9.73 Code Added
..              prtpage         prfile;*pcolumn:row,"attention of our Billing Department. "
.              prtpage         prfile;*pcolumn:row,Emailaddr," or Fax to (415)433-7796. "
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              prtpage         prfile;*pcolumn:row,*font=font012I,"If we do not hear from you within (3) working days we will consider this our final billing"
.              add             tenlpi,row
.              add             twelvelpi,row
.              prtpage         prfile;*pcolumn:row,"and we will be unable to accept any adjustments.  Payment to you will be based on"
.              add             tenlpi,row
.              add             twelvelpi,row
.              prtpage         prfile;*pcolumn:row,"the amount noted above."
.              add             eightlpi,row
.              add             eightlpi,row
.              add             eightlpi,row
.              prtpage         prfile;*pcolumn:row,*font=font012,"If you have any questions or there is a problem in invoicing us immediately, please do"
.              add             tenlpi,row
.              add             twelvelpi,row
..Patch 9.73 Code Added
.              prtpage         prfile;*pcolumn:row,*ll,"not hesitate to contact me at ",EmailAddr," or ",CNTPHONE,"."
..Patch 9.73
.              prtclose prfile
.end patch 10.83

.begin patch 10.4
.          call      PDF995Auto0
.end patch 10.4
.Fax it out
.             display   *p1:24,*el,"I'm sending now!!!!";
.START TESTING!!!!
.             move            "6288313",OWNFAX
.             clear           longdist
.END TESTING!!!!
tester
.begin patch 10.15
.begin patch 10.4
.          call      GetPDfPAth
.          pack      str45 from PDFPATH,"\flag.dat"
.          pack      APIFileName,"!",STR45,hexzero
.          loop
.          call      FindFirstFile
.          until (APIResult = 0 | APIResult = hexeight)
.                    move      "1000",str3
.                    call      Waitin using str3
.          repeat
.          move      "3000",str4                   .30 seconds
.          call      Waitin using str4
.end patch 10.4

.begin patch 10.83
.          pack      MailAttach from "c:\work\pdf\LoMemo",olrn,".pdf"
.          rep       zfill in mailattach
CheckFile
.          Move      MailAttach,Str55
.          trap      WaitForEnd giving error if IO
.          open      FileCheck,STR55,Exclusive     
.          Close     FIleCHeck
.          trap      IOMssg GIVING ERROR IF IO


.          Pack      MailSubjct from "Invoice Request Memo for NIN LR## ",ninvfld
          Pack      MailSubjct from "Invoice Request Memo for NIN LR## ",ninvfld,b1,"Mailer: ",mcomp,"Maildate: ",str10
.          Clear     MailBody
.          append    mailattach,mailbody
.          append    CRLF,Mailbody
.          Reset     MailBOdy
.end patch 10.83

.begin patch 10.8
.this should be updated to user company cross reference and pull the accounting email from there.
           move       compfld,str6         .save key
          packkey   compfld,owncomp
          call      compkey
                    if        not over
                      scan       "@",CompActEmailAR
                                 if         equal
                                 reset      CompActEmailAR
                                 MOVE       CompActEmailAR,OwnEmail
                                 endif
                      endif
.restore company vars
          packkey   compfld,str6
          call      compkey
.
.            match               "1866",OWNLON
.            if               equal
.            move                "denise@carolenters.com",OwnEmail
.            endif
.end patch 10.8
.begin patch 10.6
          scan      "@",OwnEmail
           if        equal
                      reset OwnEmail
                      pack      mailto,OwnEmail
          else                    
.end patch 10.6
                    if        (ownfax = "0000000000")
                    pack      MailTo from "GemmaSpranza@nincal.com"
                    else
.                    pack      MailTo,"IMCEAFACSYS-",longdist,ownfax,"@nincal.com"
                    pack      MailTo,"+",longdist,ownfax,"@fax.nincal.com"
                    endif          
.begin patch 10.6
          endif
.end patch 10.6
          pack      MailFrom from "GemmaSpranza@nincal.com"
          pack      MailCC from "GemmaSpranza@nincal.com"
          pack      MailBCC from "ComputerRequest@nincal.com"
          Call      SendMail



.              SPLOPEN         "c:\work\HDRFILE.PRN"
.              print           "^[D",longdist,ownfax,"^[N",ownocpy:
.               "^[T",today,b1,str5,"^[S",Nuseuser,"^]"
.              SPLCLOSE
.              clear   taskname
.              Path            Exist,"c:\windows"
.              if over                                       .nt/2000
.               append         "!c:\winnt\system32\cmd.exe /c ",taskname
.              elseif (osflag = c6)           .XP
.               append         "!c:\windows\system32\cmd.exe /c ",taskname
.              else                                          .95/98
.               append         "!c:\command.com /c ",taskname
.              endif
.              append  "copy c:\work\hdrfile.prn /b + c:\work\faxfile.prn /b c:\work\inv",taskname
.              append  ninvfld,taskname
.              append  ".prn /b",taskname
.              reset   taskname
..New code DH 16Aug2010
.               match          "0000000000",ownfax
.                    if        Not equal
.              execute taskname
.                    endif
.              execute taskname
..end New code DH 16Aug2010
..
.              clear   taskname
.              Path            Exist,"c:\windows"
.              if over                                       .nt/2000
.               append         "!c:\winnt\system32\cmd.exe /c ",taskname
.              elseif (osflag = c6)           .XP
.               append         "!c:\windows\system32\cmd.exe /c ",taskname
.              else                                          .95/98
.               append         "!c:\command.com /c ",taskname
.              endif
.              append  "copy c:\work\inv",taskname
.              append  ninvfld,taskname
..              append  ".prn \\nts3\fax",taskname
.              append  ".prn \\SRV2008A\fax",taskname
.              reset   taskname
.              execute taskname
..             display   *p1:24,*el,"I'm printing your copy now!!!!";
.              clear   taskname
.              Path            Exist,"c:\windows"
.              if over                                       .nt/2000
.               append         "!c:\winnt\system32\cmd.exe /c ",taskname
.              elseif (osflag = c6)           .XP
.               append         "!c:\windows\system32\cmd.exe /c ",taskname
.              else                                          .95/98
.               append         "!c:\command.com /c ",taskname
.              endif
.;              append  "copy c:\work\faxfile.prn \\nts0\laser2",taskname
.              append  "copy c:\work\faxfile.prn \\SRV2008a\laser2",taskname
.              reset   taskname
.              execute taskname
.             display   *p1:24,*el,"I'm Done!!!!",*w4,*p1:24,*el;
.end patch 10.15

              RETURN
userng
               aLERT          cAUTION,"I'm sorry I've lost track of who you are. Please leave the program and try again, Don't forget to tell I.S.",result
               return
no96
               aLERT          cAUTION,"NO PREPAY CODE ALLOWED !! ",result
               return

.......................................................................................................
InvRetrieveLR
               packkey        nordfld from str6
               move           c1 to nordpath
               call           Nordkey
               if             oVER
               aLERT          cAUTION,"No such order !!!!!!!",result
               setfocus       InvEditTextLR
               return
               endif
               move           "pxlz" to str4
               scan           ostat in str4
               IF             equal
               Alert          Caution,"Lcr or Pending Order -- Not Allowed",result
               return
               endif
               reset          cancodes
               scan           ostat in CANCODES
               IF             equal
               Alert          Caution,"Cancelled Order !! ",result
               endif
.begin patch 10.10
.OCOMSLCT DIM       1      211-211       overlay: COMSELECT='C', lifestyle="L"

          setitem   InvStatTextOverlay,0,""
          setprop   InvStatTextOverlay,visible=0
          if        (OCOMSLCT = "C")
          setitem   InvStatTextOverlay,0,"Comselect Overlay"
          setprop   InvStatTextOverlay,FGColor=red
          setprop   InvStatTextOverlay,visible=1
          Elseif    (OCOMSLCT = "L")
          setitem   InvStatTextOverlay,0,"Lifestyle Overlay"
          setprop   InvStatTextOverlay,FGColor=red
          setprop   InvStatTextOverlay,visible=1
          endif

.end patch 10.10
               call           InvAcdClearRecord
.add some status checking here
               call           LoadOrderINfo
               Move           C1 to Ninvpath
               packkey        Ninvfld from Olrn
               call           wipecvars
               call           Ninvkey

               If             Not OVer
               call           InvLoadRecord
               Call           InvAcdLoadRecord
               Setprop        InvButtonfax,height=20
               Setprop        InvButtonLomemo,height=20
               Setprop        InvButtonHPRint,height=20
               Setprop        InvButtonHPRint,visible=1
               Setprop        InvButtonRePRint,height=20
               call           Pinvtst
                              If             Over
.looks like I will need a flag - this is getting reset before user can see it
                              Setprop        InvButtonRePRint,height=20
                              Else
.begin patch 10.022
                              Move                INits,str3                  .save them
.end patch 10.022
                              call           Pinvkey
.begin patch 10.022
                              MOVe                Str3 to INits       .restore them
.end patch 10.022
                              endif
               ELSE                                                 ;no inv pull order info
               Setprop        InvButtonfax,height=0
               Setprop        InvButtonLomemo,height=0
               Setprop        InvButtonHPRint,height=0
               Setprop        InvButtonHPRint,visible=0
.               If             over
                              IF             (Passflag = "N")
                              move           "I",progcode
                              setprop        Passwrd,visible=1
                                             If             (passflag = "N")
                                             setprop        InvSearchQuit,Height=0
                                             setprop        InvSearchGo,Height=0
                                             setprop        INvSearch,Height=20
                                             setprop        INvok,Height=20
                                             setprop        INVSaveQuit,Height=0
                                             setprop        INVSave,Height=0
                                             Setprop        InvSearchQuit,Enabled=0
                                             Setprop        InvSearchGo,Enabled=0
                                             Setprop        InvOK,Enabled=1
                                             Setprop        InvSearch,Enabled=1
                                             setprop        INVSaveQuit,enabled=0
                                             setprop        INVSave,enabled=0
                                             return
                                             endif
                              endif

                              If             (Brkflag = yes)
                              move      obrknum to ibrknum
                              move      obrkcnt to ibrkcnt
                              endif
.begin patch 10.001
.               reset          commper                    .commission from datacard.
.;               bump           commper,1
.               call           trim using commper
.end patch 10.001
               MOVE           COMMPER TO COMMPCT
               Clear          str2
               PACK           STR2 FROM OSALES10,OSALES
               rep            zfill in str2
.begin patch 10.11               
.               if             (elstcde = "C" & NDATEXCH <> "2")
               if             ((elstcde = "C" | elstcde = "P") & NDATEXCH <> "2")
.end patch 10.11               
.begin patch 10.001
               add            "10" to commpct
               endif
               Move           Mask32 to str6
               edit           commpct to str6
               Setitem        InvEditText013,0,str6
.end patch 10.001
               RESET          EXCODES
               SCAN           OELCODE IN EXCODES
                              IF             EQUAL
                              move           inpqty to qtybildn
.begin patch 10.013
                                             move           Oexqty to n9
                                             if             (n9 = c0)
                                             move           n4 to str1
                                             Setitem        InvstatText012,0,str1
                                             Move           Mask32 to str6
                                             Edit           c0 to str6
                                             Setitem        InvEditText013,0,str6
                                             endif
.end patch 10.013
                              endif
.
               move           c0 to inetrc
               move           c0 to irnetper
               move           onetper to irnetper
               pack           Str3 from Irnetper,"%"
               setitem        InvStatText073,0,str3
               move           onetrc to inetrc
               move           Inetrc to str6
               setitem        InvStatText075,0,str6

               MOVE           oexqty TO irexqty
               MOVE           OQTY TO KFQINM
               MOVE           OQTY TO  qtyin
               MOVE           OQTY TO  inpqty
               MOVE           C0 TO N9
               MOVE           OEXQTY TO N9
               COMPARE        C0 TO N9
                              IF         not EQUAL
                              move      c0 to n2
                              move      onetper to n2
                              compare   c0 to n2                    .net name order?
                                             If             Not equal
                                             aLERT          cAUTION,"NET NAME WITH SPLIT CAN'T DO !!",result
                                             SetFocus       InvEditTextLR
                                             Return
                                             endif
                              endif
.DH 18may 2004 belv did this at lofad ord info
.DH turn back on Feb 05
               MOVE           NO TO SHIPSW
               MOVE           NO TO MRGSW
               packkey        Nmrgfld from olrn
               REP            ZFILL IN NMRGFLD
               CALL           NMRGKEY
                              IF             Not OVER
                              MOVE           YES TO MRGSW
                              endif

               move           c0 to squant
               Packkey        Nshpfld from olrn
               CALL           NSHPKEY
                              IF             Not OVER
                              MOVE           YES TO SHIPSW
                              endif
.End DH 18may 2004 belv did this at load ord info
...................................................................
Buggerthis
.begin 10.0004
              Call            SetNetFlag
.end 10.0004
               MOVE           C0 TO TOTREJ
.begin of long nested IF
               If             (MrgSw = Yes)
               ADD            NMRGID TO TOTREJ
               ADD            NMRGERR TO TOTREJ
               ADD            NMRGdisf TO TOTREJ
               ADD            NMRGDMA TO TOTREJ
               ADD            NMRGNPER TO TOTREJ
               ADD            NMRGZ4 TO TOTREJ
               ADD            NMRGCONV TO TOTREJ         DLH 1/12/95 EPSILON
               ADD            NMRGPRIS TO TOTREJ
               ADD            NCOAmnf TO TOTREJ
.begin 10.0010
               ADD            Nmrgdisa TO TOTREJ
.begin 10.0010
.begin 10.07
               ADD            Nmrgcnr TO TOTREJ
.begin 10.07
               MOVE           NMRGRQTY TO INPQTY
               move           c0 to n2
               move           irnetper to n2
               compare        c0 to n2
                              if        equal
                              SUB       TOTREJ FROM NMRGRQTY
.begin 10.016
                             else
                             sub       nmrgdisa from nmrgrqty
                             sub       nmrgdisa from inpqty
.end 10.016
.begin 10.07
                              sub       nmrgcnr from nmrgrqty
                              sub       nmrgcnr from inpqty
.end 10.07
                              endif
               MOVE           C0 TO N8
               MOVE           C0 TO FORM8
               MOVE           NMRGRQTY TO N8            MOVE MRG FINAL QTY TO FORM FIELD.
               MOVE           TOTREJ TO FORM8             MOVE REJECTS TO FORM FIELD.
               MOVE           N8 TO CALCPER
               MOVE           CALCPER TO DIFF
               MOVE           INPQTY TO FORM8
               MOVE           NMRGRQTY TO CALCPER
               move           c0 to per85                  .DLH may95 use order net
               move           irnetper to per85             .info if avail.
               mult           ".01" by per85               .
               compare        c0 to per85                  .
                              if             equal                        .
                              move           ".85" to per85               .
                              endif                                  .
               MULT           PER85 BY CALCPER
               move           NMRGRQTY to chgrqty
               MOVE           NMRGNET TO CHGQTY1
               MOVE           CALCPER TO CHGQTY2
               COMPARE        CHGQTY1 TO CHGQTY2
                              IF             LESS
                              SUB            chgqty1 from chgrqty
               move           mask11 to str11
               edit           chgrqty to str11
               setitem        InvStatText076,0,str11
                              move           nmrgnet to qtybildn
               ELSE
                              SUB            chgqty2 from chgrqty
               move           mask11 to str11
               edit           chgrqty to str11
               setitem        InvStatText076,0,str11
                              move           calcper to qtybildn
                              ENDIF
               move           irnetper to n2
               compare        c0 to n2
                              if             equal
.begin patch 10.005   .undone
                              move           nmrgrqty to qtybildn
.                              move           nmrgiqty to qtybildn
.                             Move           nmrgiqty to nmrgnet                .no net force inpqty
.end patch 10.005
                              endif
.more goodies
              move            mask10 to str10
              edit            NmrgrQty to str10
              setitem         InvStatText078,0,str10
              move            mask10 to str10
              edit            InpQty to str10
              setitem         InvStatText079,0,str10
              move            mask10 to str10
              edit            nmrgnet to str10

                                             if             (nmrgnet > calcper)
                                             setitem        InvEditText014,0,str10                        .qty billed field
                                             Else
                                             Move           Mask11 to str11
                                             Edit           Calcper to str11
                                             setitem        InvEditText014,0,str11
                                             endif

                              move           mask11 to str11
                              move           Inpqty to n9
                              mult           per85 by n9
                              edit           n9 to str11
                              setitem        InvStatText074,0,str11
                              setitem        InvStatText080,0,str10
                              move           qtybildn to kfqinm
                              MOVE           inpqty TO  qtyin

                              Elseif         (shipsw = YES)

                              MOVE           SQUANT TO KFQINM
                              MOVE           Squant TO  qtyin
                              MOVE           SQuant TO  inpqty
                              MOVE           SQuant TO  calcper
                              move           "ZZZ,ZZZ,Z99",str11
                              edit           qtyin to str11
                              setitem        InvStatText060,0,str11

                              Else

                              MOVE           OQTY TO KFQINM
                              MOVE           OQTY TO  qtyin
                              MOVE           OQTY TO  inpqty
                              move           oqty to calcper
                              move           yes to oqtyflag

                              Endif

                                             if             (mrgsw <> yes)               .if its yes we already calculated
                                             move           c0 to per85                  .DLH may95 use order net
                                             move           irnetper to per85             .info if avail.
                                                            if             (per85 != 0)
                                                            mult           ".01" by per85               .
                                                            MULT           PER85 BY CALCPER
                                                            endif
                                             Endif
                              move           mask10 to str10
                              edit           InpQty to str10
                              setitem        InvStatText079,0,str10
                              move           mask10 to str10
                              edit           nmrgnet to str10
                                             if             (nmrgnet > calcper)
.                                            setitem        InvStatText074,0,str10
                                             setitem        InvEditText014,0,str10
                                             Else
                                             Move           Mask11 to str11
                                             Edit           Calcper to str11
.                                            setitem        InvStatText074,0,str11
                                             setitem        InvEditText014,0,str11
                                             endif
                              move           mask11 to str11
                              move           Inpqty to n9
                              mult           per85 by n9
                              edit           n9 to str11
                              setitem        InvStatText074,0,str11
                              setitem        InvStatText080,0,str10

.begin patch 10.003 DLH
.begin patch 10.005  above patch same number perhaps not nec
.things are messed up in this nested if and wrong qty is getting used ????????/
Heresthebugger
                              IF             (netflag = 1)                     .1 is not a net so reset some things

                                             if             (mrgsw = yes)         .Do we have merge info
                                             RESET          EXCODES                .yes
                                             SCAN           OELCODE IN EXCODES
                                                            IF             EQUAL
                                                            ADD            TOTREJ to NMRGRQTY
                                                            endif
                                             move           Nmrgrqty to qtyin
. jul 13 2005 maybe not this one                                                          move           Nmrgrqty to inpqty
                                             move           Nmrgrqty to qtybild
                                             Move           Mask11 to str11
                                             Edit           qtyin to str11
                                             setitem        InvEditText014,0,str11
                                             ElseIf         (shipsw = yes)               .No merg do we have shipping
                                             move           squant to qtyin                 .yes
                                             move           squant to inpqty
                                             move           squant to qtybild
                                             Move           Mask11 to str11
                                             Edit           qtyin to str11
                                             setitem        InvEditText014,0,str11
                                             Else                                         .No merge no shipping use order info
                                             move           oqty to qtyin
                                             move           oqty to inpqty
                                             move           oqty to qtybild
                                             Move           Mask11 to str11
                                             Edit           qtyin to str11
                                             setitem        InvEditText014,0,str11
                                             endif
.begin patch 10.004 DLH 12July 05
.                             ElseIf         (mrgsw = yes)
.                                            move           qtybildn to kfqinm
.                                            MOVE           inpqty TO  qtyin
.                                            ELSE
.                                            move           c0 to n2
.                                            move           irnetper to n2
.                                                           if        (lstmsw = Yes & n2 != 0)
.                                                           move       qtybildn to kfqinm
.                                                           MOVE       inpqty TO  qtyin
.                                                           ELSE
.                                                           CMATCH         YES TO SHIPSW
.                                                                          IF             EQUAL
.                                                                          MOVE           SQUANT TO KFQINM
.                                                                          MOVE           Squant TO  qtyin
.                                                                          move           "ZZZ,ZZZ,Z99",str11
.                                                                          edit           qtyin to str11
.                                                                          setitem        InvStatText060,0,str11
.                                                                          ELSE
.                                                                          MOVE           OQTY TO KFQINM
.                                                                          MOVE           OQTY TO  qtyin
.                                                                          MOVE           OQTY TO  inpqty
.                                                                          move           yes to oqtyflag
.                                                                          ENDIF
.                                                           ENDIF
.                              ENDIF
.end patch 10.004 DLH 12July 05
.end patch 10.003 DLH
                              endif
..................................................................
Shite
                              goto           skipshite
.end of new try
.             CMATCH         YES TO MRGSW
.                              IF             EQUAL
.                              move           qtybildn to kfqinm
.                              MOVE           inpqty TO  qtyin
.                              ELSE
.                              move           c0 to n2
.                              move           irnetper to n2
.                                             if        (lstmsw = Yes & n2 != 0)
.                                             move       qtybildn to kfqinm
.                                             MOVE       inpqty TO  qtyin

.                                             ELSE
.                                             CMATCH         YES TO SHIPSW
.                                                            IF             EQUAL
.                                                            MOVE           SQUANT TO KFQINM
.                                                            MOVE           Squant TO  qtyin
.                                                            move           "ZZZ,ZZZ,Z99",str11
.                                                            edit           qtyin to str11
.                                                            setitem        InvStatText060,0,str11
.                                                            ELSE
.                                                            MOVE           OQTY TO KFQINM
.                                                            MOVE           OQTY TO  qtyin
.                                                            MOVE           OQTY TO  inpqty
.                                                            move           yes to oqtyflag
.                                                            ENDIF
.                                             ENDIF
.                              ENDIF
skipshite
               move           fppm to kfpperm
               Setitem        InvEditText015,0,Kfpperm
.               clear          str11
.               move           mask11 to str11
.23Feb2005 DLH Cleanup
.;06Oct2004 DH messing
.               edit           Qtyin to str11
.;               edit           qtybildn to str11
.               setitem        InvEditText014,0,str11
.               clear          str11
.end cleanup


               move           mask10 to str10
               edit           Inpqty to str10
               setitem        InvEditText017,0,str10

               move           c0 to inetrc
               move           c0 to irnetper
               move           onetper to irnetper
               move           onetrc to inetrc
               Call           CheckSplit
               setitem        InvEditText018,0,irexqty
               setitem        InvEditText031,0,iexppm
               call           NInvAcdRecClear
               call           InvAcdClearRecord
               Setprop        InvButtonfax,height=0
               Setprop        InvButtonLomemo,height=0
               Setprop        InvButtonHPRint,height=0
               Setprop        InvButtonHPRint,visible=0
               Setprop        InvButtonRePRint,height=0
.               ;;SetProp     InvSave,*NODEFTABID=0
               aLERT          cAUTION,"Let's Bill it !!!!!!!",result
               IF             (Passflag = "N")
               move           "I",progcode
               setprop        Passwrd,visible=1
                             If             (passflag = "N")
                             return
                             MOve            C1 to mode
                             endif
               endif
               IF         (Olnum = "018492")
                              alert   caution,"No LO invoice Required per SA 12/04",result
               endif
.begin patch 10.008
               Setprop        InvModify,Enabled=0
               Setprop        InvModify,Height=0
.end patch 10.008


               IF             (NDatTDmc = B1 or NDatTdmc = "")
                              cmatch         "C" to elstcde
                              if             equal
                              Clear          Str255
                              pack           Str255 from "016130-","019539-","015102-","011507-","003252-","000995-","015503-016724-001106-018847-020565"
                              scan           Olnum in str255
                                             if             equal
                                             alert          caution,"Exclusive List with Special deal on selects",result
                                             ElseIF         (Olnum = "021703" or Olnum = "021716")
                                             alert          caution,"Excl List Split Comm with NIN",result
                                             ElseIF         (Olnum = "012594")
                                             alert          caution,"Excl List No NIN :(",result
                                             ElseIf         (Olnum = "021611")
                                             alert          caution,"Excl List All NIN :)",result
                                             ElseIf         (Olnum = "002479")
                                             alert          caution,"Exclusive List Matrix w/Special $1 deal on $6/m slcts ",result
.begin patch 10.022
                                                       Elseif         (ndat3cde = B1)     
.                                             Else
                                                                 alert          caution,"Exclusive Bill selects from Donnelly Matrix",result
                                                       Elseif         (ndat3cde = "M" or ndat3cde = "R" or ndat3cde = "P")        
                                                                 alert          caution,"Exclusive Bill selects from PDI/MMI/Frontline Matrix",result                                             
                                             Else
                                             alert          caution,"Exclusive Bill selects from Matrix",result
                                             endif
.end patch 10.022                                             
                              endif

               endif

.
.begin patch 10.0
.               MOVE           "1",ADMOD                          .add mode
.               MOVE           YES,ADREC                          .yes to add record
               Move           C3 to Mode
               Move           Yes to FrcCOmpFlag
               Call           EnableAcdButtons
.end patch 10.0
               Move           Qtyin to Qtybild
               Move           C0 to Qtyin                        ;if in add mode this needs to be zero until  after compute
.
               Move           c0,NinvAcdNum
               setprop        InvSearchQuit,Height=0
               setprop        InvSearchGo,Height=0
               setprop        INvSearch,Height=0
               setprop        INvok,Height=0
               setprop        INVSaveQuit,Height=20
               setprop        INVSave,Height=20
.
               Setprop        InvSearchQuit,Enabled=0
               Setprop        InvSearchGo,Enabled=0
               Setprop        InvOK,Enabled=0
               Setprop        InvSearch,Enabled=0
               setprop        INVSaveQuit,enabled=1
               setprop        INVSave,enabled=1
               setfocus       InvEditText010
.*** All of the above is no INV
.*******************************************************************************************************
.               Else                                           ;we have invoice record
.
.               call           InvLoadRecord
.               Call           InvAcdLoadRecord
.               Setprop        InvButtonfax,height=20
.               Setprop        InvButtonHPRint,height=20

               Endif
.end of long nested IF               ?

               return
.......................................................................................................
LoadOrderINfo
              MOVE            NO TO LSTMSW
              PACK            STR2 FROM OSALES10,OSALES
              REP             ZFILL IN STR2
.begin patch 10.07
.               IF             (str2 = "06" or str2 = "19")
               IF             (str2 = "06" or str2 = "19" or str2 = "27" or str2 = "28")
.end patch 10.07
              MOVE            YES TO LSTMSW            *LIST MANAGEMENT.
              endif

              CLEAR          NBRKFLD
               PACK           NBRKFLD FROM OBRKNUM,OBRKCNT
               rep            zfill in nbrkfld
               setitem        InvEditBRk,0,ObrkNum
               setitem        InvEditBcnt,0,Obrkcnt
               CALL           NBRKKEY
               if             over
               move           no to brkflag
                              if             (C2 <> Ninvpath)     ;inquiry by Invoice?
                              clear          ibrknum
                              clear          ibrkcnt
                              clear          brcomp
                              endif
               CLEAR          BRFAX
               else
               move           yes to brkflag
               endif
.
               setitem        InvEditMlr,0,Omlrnum
               setitem        InvEditMCnt,0,z3
               packkey        mkey from Omlrnum,z3
               move           c1 to nmlrpath
               call           nmlrkey
               setitem        InvStatText035,0,Mcomp
               setitem        Ninv0001StatText001,0," "
               setprop        Ninv0001StatText001,FGColor=black
               IF             (Mcode = "B")
               setitem        Ninv0001StatText001,0,"Batch Bill"
               ElseIF         (Mcode = "A")
               setprop        Ninv0001StatText001,FGColor=red
               setitem        Ninv0001StatText001,0,"Batch - Adjust"
               ENDIF
               IF             (Mcopies = "Y")
               setprop        Ninv0001StatText002,FGColor=Blue
               setitem        Ninv0001StatText002,0,"Regional"
               setprop        InvStatText035,FGColor=Red
               Else
               setprop        Ninv0001StatText002,FGColor=Black
               setitem        Ninv0001StatText002,0," "
               setprop        InvStatText035,FGColor=Black
               endif


               packkey        Nownfld from olon
               CALL           NOWNKEY
               if             oVER
               aLERT          cAUTION,"invalid owner, Fix order !!!!!!!",result
               setfocus       InvEditTextLR
               return
               endif
               setprop        InvStatText037,FGColor=Black
               packkey        Npayfld from olon,paytn
               CALL           NPAYTST
                              if        not over
                              CALL      NPAYKEY
                              MOVE      PNAME TO OWNLONM
                              MOVE      PCOMP TO OWNOCPY
                              MOVE      PSTREET TO OWNLOSA
                              MOVE      PCITY TO OWNLOCTY
                              MOVE      PSTATE TO OWNLOS
                              MOVE      PZIP TO OWNLOZC
                              setprop        InvStatText037,FGColor=Red
                              endif
.
               clear          nofrfld
               pack           nofrfld from oodnum
               call           nofrkey
               setitem        InvEditTextLR,0,Olrn
               SetItem        InvEditText001,0,ofdesc
               SetItem        InvEditOwner,0,Olon
               SetItem        InvEditPayTo,0,Paytn
               SetItem        InvStatText037,0,Ownocpy
.
.call               Trim using OWNCTN
.END PATCH 10.021.1 REMOVED LOGIC
.START PATCH 10.021.1 REMOVED LOGIC
.               if (OWNCTN <> "")
.                              pack               NFULFLD,OWNCTN
.                              rep               zfill,NFULFLD
.                              move               C1,NFULPATH
.                              move               "OWNPRNT-NFULKEY",Location
.                              pack               KeyLocation,NFULFLD
.                              call               NFULKEY
.               else
.                              clear               NFULFLD
.                              clear               NFULVARS
.               endif
.Start Patch 10.023 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.      
.                   if (OWNCTN <> "")
.                             pack      COMPFLD6,OWNCTN
.                             rep       zfill,COMPFLD6
.                             move      C1,COMPPATH
.                             move      "OWNPRNT-COMPKEY6",Location
.                             pack      KeyLocation,COMPFLD6
.                             call      COMPKEY6
.                             if over
.                                       clear     COMPFLD6
.                                       clear     COMPCOMP // ONLY VAR USED IN DISPORD.INC
.                             else
.                                       if (COMPSVBFLG <> "T")
.                                                 clear     COMPFLD6
.                                                 clear     COMPCOMP
.                                       endif
.                             endif
.                   else      // OWNCTN = ""
.                             clear     COMPFLD6
.                             clear     COMPCOMP
.                   endif
.END PATCH 10.021.1 REMOVED LOGIC

.         call      Trim using OWNCTN
          call      Trim using OFULLFIL
          if (OFULLFIL <> "")
                    pack      COMPFLD,OFULLFIL
                    call      zfillit using COMPFLD
                    move      C1,COMPPATH
                    move      "DISRTN-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                              if over
                                        clear     COMPFLD
                                        clear     COMPCOMP
                              endif
          else      // OFULLFIL = ""
                    clear     COMPFLD
                    clear     COMPCOMP
          endif

.End Patch 10.023 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.        
               if (GUARCODE = "1")
                              setitem               InvComboBox001,0,2
               elseif (GUARCODE = "2")
                              setitem               InvComboBox001,0,3
               elseif (GUARCODE = "3")
                              setitem               InvComboBox001,0,4
               elseif (GUARCODE = "4")
                              setitem               InvComboBox001,0,5
               elseif (GUARCODE = "5")
                              setitem               InvComboBox001,0,1
               elseif (GUARCODE = "6")
                              setitem               InvComboBox001,0,6
               elseif (GUARCODE = "7")
                              setitem               InvComboBox001,0,7
               elseif (GUARCODE = "8")
                              setitem               InvComboBox001,0,8
               elseif (GUARCODE = "9")
                              setitem               InvComboBox001,0,9
               else
                              setitem               InvComboBox001,0,1
               endif
               Move           c0 to N1
               move           wsjpc to n1                      ;guar letter status
               if (OBRKGUAR = "1")
                              If             (n1 > 0)
                              setitem               InvComboBox002,0,6
                              else
                              setitem               InvComboBox002,0,2
                              endif
               elseif (OBRKGUAR = "2")
                              IF             (n1 > 0)
                              setitem               InvComboBox002,0,7
                              else
                              setitem               InvComboBox002,0,3
                              endif
               elseif (OBRKGUAR = "3")
                              IF             (n1 > 0)
                              setitem               InvComboBox002,0,8
                              else
                              setitem               InvComboBox002,0,4
                              endif
               elseif (OBRKGUAR = "4")
                              IF             (n1 > 0)
                              setitem               InvComboBox002,0,9
                              else
                              setitem               InvComboBox002,0,5
                              endif
               else
                              setitem               InvComboBox002,0,1
               endif

               pack           str2,OSALES10,OSALES
               move           str2 to n2
               move               osls0,str15
               load               str15 from n2 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
                              osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
                              osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
                              osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
               setitem               NInv0001BStatText008,0,str15
               pack           NCNTFLD,OCOCODE
               move           "DISCONTC-NCNTKEY",Location
               pack           KeyLocation,"Key: ",NCNTFLD
               call           NCNTKEY
               if             over
               clear          CNTNAME
               endif
               setitem               NInv0001BStatText007,0,CntName
.
              packkey         NSEL2FLD,"1",OLRN
              move            "NSEL2KEY",Location
              pack            KeyLocation,"Key: ",NSEL2FLD
              call            NSEL2KEY
              if             over
              move            O2DES,NSEL2NAME
              unpack          OPPM,str3,str2
              pack            str6,str3,".",str2
              rep             zfill,str6
              move            str6,NSEL2PRICE
              endif

               Clear          str10
               pack           str10 from Omdtem,slash,omdted,slash,omdtec,omdtey
               setitem        InvEditText006,0,str10
               Clear          str10
               setitem        InvEditText009,0,str10
               setitem        InvEditText010,0,LOINVN
               setitem        InvEditText007,0,OMLRPON
               setitem        InvEditText033,0,Olnum
               setitem        invedittext011,0,Incc
               setitem        invedittext012,0,paycode
               move           c1 to nDATpath
               PACKKEY        nDATFLD from olnum
               call           Ndatkey
               setitem        InvEditText034,0,O1des
               setitem        InvEditText035,0,NSel2name
               setprop        INvEdittext034,Enabled=1
.begin patch 10.3
          if (OELCODE = "1")
                    setitem   Ninv0001ACheckEntire,0,1
          elseif (OELCODE     = "2")
                    setitem   Ninv0001ACheckEntire,0,0
          elseif (OELCODE     = "3")
                    setitem   Ninv0001ACheckEntire,0,1
          else      .OELCODE = B1
                    setitem   Ninv0001ACheckEntire,0,0
          endif
.end patch 10.3
.
.               CMATCH         "C" TO ELSTCDE
          If        (elstCde = "C")     
..if             equal
               setprop        INvEdittext034,FGColor=CYAN
               setprop        INvEdittext034,BGColor=Blue
          Elseif    (ElstCDE ="P")
               setprop        INvEdittext034,FGColor=Green
               setprop        INvEdittext034,BGColor=Yellow

               else
               setprop        INvEdittext034,FGColor=BlACK
               setprop        INvEdittext034,BGColor=White
               ENDIF
.               setprop        INvEdittext034,Enabled=
               setitem        InvEditText034,0,O1des
               setitem        InvEditText035,0,NSel2name
               scan           olnum in addclist
               if             equal
               move           c2 to addcflag
               else
               move           c1 to addcflag
               endif
. temp
HUH
               MOVE          OLNUM TO NDAT3FLD
               clear          str6
               Clear                str8
               clear                str10
               clear                Str45
            call    debug
               CLEAR          NDATTDMC
               rep            zfill in ndat3fld
               CALL           NDAT3KEY
               IF             NOT OVER                  *TDMC BILLING INFO.
.begin patch 10.2  check for stop date
               call           Trim using ndat3ex2
               if             (ndat3ex2 <> "")
               move           cc,str2            .save it       
               call           datetest
               if           (dateflag = c0)           .valid date
                          unpack         ndat3ex2,mm,dd,cc,yy
                          pack           str10 from cc,yy,mm,dd
                          pack           str8 from oodtec,oodtey,oodtem,oodted
                          move           str10,n10
                          move           str8,n8
                               if             (n8 >= N10)  .past stop date?
                               goto            NoAddChrgs    
                               endif
                endif
               move              str2,cc               .restore       
               endif
.end patch 10.2  check for stop date

               CLEAR          STR10
                              IF             (NDatTDMC = "B")
                              MOVE      "BOTH" TO STR10
                              Elseif         (NDatTDMC = "R")
                              MOVE      "RENT/SPLIT" TO STR10
                              ELseIF         (NDatTDMC = "E")
                              MOVE      "EXCH/Only" TO STR10
                              ENDIF
.begin patch 10.09
                              If        (DATFUL = "009406")
                              move      b1,ndat3cde
                              MOVE      "TDMC" TO STR8
          
                              Elseif    (COMPNUM = "009383")
                              move      "A",ndat3cde
                                        MOVE      "Anacapa" TO STR8

                              Elseif    (COMPNUM = "009831")
                              move      "F",ndat3cde
                                        MOVE      "FIDE" TO STR8
          
                              Elseif    (COMPNUM = "009384")
                              move      "J",ndat3cde
                                        MOVE      "Antares" TO STR8
.
                              ElseIf    (COMPNUM = "004907")
                              move      "K",ndat3cde
                                        MOVE      "MKGT" TO STR8

                              Elseif    (COMPNUM = "009428")
                              move      "M",ndat3cde
                                        MOVE      "MMI" TO STR8

                              Elseif    (COMPNUM = "009387" or COMPNUM = "009414" or COMPNUM = "009429")
                              move      "P",ndat3cde
                                        MOVE      "PIDI" TO STR8
.         
                              Elseif    (COMPNUM = "009410")
                              move      "R",ndat3cde
                                        MOVE      "Frontlin" TO STR8
                              endif

.                   
.end patch 10.09
.
                              IF             (ndatdolc = "Y")
                              move       "$/Date" to str6
                              endif

                              clear          str45

                              Append         Str8,str45
                              append         b1,str45
                              append         str10,str45
                              append         b1,str45
                              append         "Chrgs ",str45
                              append         b1,str45
                              append         str6 to str45
                              if             (ndat3exh = yes)
                              append         "+Exch Fee",str45
                              Endif
                              reset          str45
               setitem        InvStatText053,0,str45
                              IF             (addcflag = "1")
                              setprop        InvStatText053,FGColor=RED
                              Elseif         (addcflag = "2")
                              setprop        InvStatText053,FGColor=Green
                              endif
               endif
.begin patch 10.2
NoAddChrgs
.end patch 10.2
               MOVE           NO TO SHIPSW
               MOVE           NO TO MRGSW
               MOVE           olrn TO NMRGFLD
               REP            ZFILL IN NMRGFLD
               CALL           NMRGKEY
               If             not over
               MOVE           YES TO MRGSW
               endif
               move           c0 to squant
               MOVE           Olrn TO NSHPFLD
               CALL           NSHPKEY
               IF             Not OVER
               MOVE           YES TO SHIPSW
               endif

.need to check logic here  ---- which qty to present & take into account R/Ex splits
.for now order qty
               RESET     EXCODES
               SCAN      OELCODE IN EXCODES
               IF        EQUAL
               move      inpqty to qtybildn
               endif
.
               CMATCH     YES TO MRGSW
               IF         EQUAL
               move       qtybildn to kfqinm
               MOVE       inpqty TO  qtyin
               ELSE
               move      c0 to n2
               move      irnetper to n2
                              if        (lstmsw = Yes & n2 != 0)
                              move       qtybildn to kfqinm
                              MOVE       inpqty TO  qtyin
                              ELSE
                              CMATCH     YES TO SHIPSW
                                             IF         EQUAL
                                             MOVE       SQUANT TO KFQINM
                                             MOVE       Squant TO  qtyin
                                             ELSE
                                             MOVE       OQTY TO KFQINM
                                             MOVE       OQTY TO  qtyin
                                             move       yes to oqtyflag
                                             ENDIF
                              ENDIF
               ENDIF
debugger
               MOVE           C0 TO N10
               MOVE           C0 TO N9
               MOVE           SQUANT TO N9          MOVE SHIPPED QTY TO FORM FIELD.
               MOVE           OQTY TO N10             MOVE ORDERED QTY TO FORM FIELD.
               SUB            N10 FROM N9
               move           n9 to diff
               MOVE           diff TO form94
              move            mask11 to str11
              edit            diff to str11
              setitem         InvStatText082,0,str11
.               MOVE           CALCPER TO DIFF
               MOVE           oqty TO N9
               DIVIDE         N9 INTO form94
               MULT           "100" BY form94
               MOVE           C0 TO PERCENT
               ADD            form94 TO PERCENT
               MOve           "ZZZ9.99" to str7
               clear          str11
               move           mask11 to str11
               mOVE           C0 TO N9
               move           oqty to N9
               edit           N9 to str11
               setitem        InvStatText059,0,str11
               clear          str11
               move           mask11 to str11
               mOVE           C0 TO N9
               move           squant to N9
               edit           n9 to str11
               setitem        InvStatText060,0,str11
               clear          str8
               move           "ZZZZ.99-",str8
               edit           percent to str8
               setitem        InvStatText061,0,str8

.
.patch 10.002
               move           lrn to tinvfld       Jd moving olrn, lrn = blank
.patch 10.002
               move           olrn to tinvfld
               call           tinvkey
               if             not over
               move           tinvdolr to form122
               mult           ".01" by form122
               move           c0 to TDMCAMT
               add            form122 to TDMCAMT
               clear          str45
               append         "S.B. Billed ",str45
               append         Tdmcamt,str45
               append         "INV ## ",str45
               append         tinvinv,str45
               reset          str45
               setitem        InvStatText031,0,str45
                              Loop
                              call           tinvks
                              Until          over
                              match          tinvfld to tinvlr
                                             If             Equal
                                             move       c0 to form122
                                             move       tinvdolr to form122
                                             mult       ".01" by form122
                                             add         form122 to TDMCAMT
                                             else
                                             Break
                                             endif
               Repeat
               else
               setitem        InvStatText031,0,"No S.B. billing found"
               endif
               if             (qtybildn != 0)
               clear          str11
               move           mask11 to str11
               edit           qtybildn to str11
               setitem        InvEditText014,0,str11                   ;qty billed - rental
               endif

               clear          str11
               move           mask11 to str11
.               edit           qtyin to str11
               setitem        InvEditText017,0,str11                   ;qtyin
               move           mask32 to str6
.               MOVE           OPPM TO FPPM
.               DIV            "100" INTO FPPM
.               Move           Fppm to ppm
.               edit           ppm to str6
               Move           NSEL2PRICE to str7
               setitem        InvEditText015,0,str7
               clear          str11
               move           mask11 to str11
               move           c0 to n9
               move           OEXQTY to n9
               edit           n9 to str11
               setitem        InvEditText018,0,str11                 ;exchange qty
               move           mask32 to str6
               move           c0 to fppm
               MOVE           OxPPM TO FPPM
               DIV            "100" INTO FPPM
               Move           Fppm to iexppm
               edit           iexppm to str6
               setitem        InvEditText031,0,str6

.
               setitem        InvStatText035,0,Mcomp

          if        (Ocompid ="P")
            setprop        InvStatText035,FGColor=Green
.            setprop        InvStatText035,BGColor=Yellow
            Else
            setprop        InvStatText035,FGColor=Black
.            setprop        InvStatText035,BGColor=grey
            endif
               
               setitem        InvStatText036,0,BRcomp
               setitem        InvStatText037,0,OwnoCPy
               setitem        InvEditText008,0,OMLRKY
.
               move           no to over
               PACKKey        NMOAFLD4 FROM obrknum,omlrnum
               REP            ZFILL IN NMOAFLD4
               move           no to NmobMsgFlag
               move           c2 to nmobpath
.test o ru 11/08/05 DLH
               Call           NMobTst
               If             Over
               SetItem        Ninv0001StatText003,0,"  "
.               SetItem        Ninv0001StatText004,0,str13
               SetItem        Ninv0001StatText004,0," "
               GOTO           CheckEscrow
               Else
               CALL           NMOBKEY      .IS THERE MONEY ON ACCOUNT?
.               cmatch         "Y" to over
.               GOTO           CheckEscrow IF equal
               endif
.               COMPARE        C0 TO BALANCE
.               GOTO           CheckEscrow IF EQUAL
.               GOTO           CheckEscrow IF NOT LESS
               If             (balance <= c0 )
               setprop        Ninv0001StatText003,FGColor=red
               SetItem        Ninv0001StatText003,0,"MOA = "
               move           MaskBalance to Str13
               edit           Balance to str13
               setprop        Ninv0001StatText004,FGColor=red
               SetItem        Ninv0001StatText004,0,str13
               else
               SetItem        Ninv0001StatText003,0,"  "
               SetItem        Ninv0001StatText004,0," "
               endif
...........................
CheckEscrow
               reset     escrmlrs
               scan       Omlrnum in escrmlrs
               if         equal
               setprop        Ninv0001StatText005,FGColor=red
               SetItem        Ninv0001StatText005,0,"ESCROW"
               endif
               reset          epsimlrs
               if             (obrknum <> "0192")
               scan           Omlrnum in epsimlrs
                              if         equal
                              aLERT          cAUTION,"NO 10% ON THIS EPSILON CLIENT",result
                              endif
               endif

               pack               NSPEFLD,OLRN
               rep               zfill,NSPEFLD
               move               C3,NSPELOCK
               move               "LoadScreens-NSPEKEY",Location
               pack               KeyLocation,"Key: ",NSPEFLD
               call               NSPEKEY
.
               call               Trim using DESC002
               setitem               Ninv0001bEditText010,0,DESC002
.
               Call           InvLoadNotes
.
               pack           ncrcfld from nordfld
               CALL           NCRCKEY                  ANY REVISIONS?
               IF             NOT OVER     *YES
REvised        RESET          CANCODES
               SCAN           NCRCCODE IN CANCODES       *CANCELLED?
               IF             EQUAL
               setitem               InvStatText038,0,"Cancelled"
               pack           str12 from NCRCMM,SLASH,NCRCDD,SLASH,NCRCYY,b1,NCRCTYP
               setitem               InvStatText039,0,str12
               aLERT          cAUTION,"Cancelled Order",result
               ELSE
               setitem               InvStatText038,0,"Revised"
               pack           str12 from NCRCMM,SLASH,NCRCDD,SLASH,NCRCYY,b1,NCRCTYP
               setitem               InvStatText039,0,str12
               ENDIF
               CALL           NCRCKS
               goto           RevisedExit IF OVER
               MATCH          NCRCFLD TO NCRCKEY
               goto           RevisedExit IF NOT EQUAL
               Beep
               pause          "1"
               GOTO           REvised
               endif
RevisedExit

               return
.......................................................................................................
InvRetrieveInv
               Move           C2 to Ninvpath
               packkey        Ninvfld from Invnum
               call           Ninvkey
               If             Not over
.dave goes mad 04March2005
               Setprop        InvButtonRePRint,height=20
               packkey        nordfld from lrn
              move            lrn to str6
              GOTO            InvRetrieveLR
.dave goes mad 04March2005
.               move           c1 to nordpath
.               call           Nordkey
.               call           LoadOrderINfo
.               REP            ZFILL,PAYTN
.               packkey        Npayfld from olon,paytn
.               CALL           NPAYTST
.                              if        not over
.                              CALL      NPAYKEY
.                              MOVE      PNAME TO OWNLONM
.                              MOVE      PCOMP TO OWNOCPY
.                              MOVE      PSTREET TO OWNLOSA
.                              MOVE      PCITY TO OWNLOCTY
.                              MOVE      PSTATE TO OWNLOS
.                              MOVE      PZIP TO OWNLOZC
.                              endif
.
.               call           InvLoadRecord
.               Call           InvAcdLoadRecord
.
               else
              Alert          Caution,"Invoice not found!!!",result

               endif
               setitem        InvStatText037,0,Ownocpy
               return
.......................................................................................................
InvLoadRecord

               setitem        InvEditTextLR,0,lrn
               cmatch         "P" to statb
               if             equal
.begin patch 10.008
               Setprop        InvModify,Enabled=0
               Setprop        InvModify,Height=0
               call           DisableAcdButtons
               Move           C1 to Mode                    .force to inquire
.end patch 10.008
               setprop        InvEditTextLR,FGColor=Magenta
               else
               setprop        InvEditTextLR,FGColor=Black
               endif

               setitem        InvEditTextInv,0,Invnum
               clear          str11
               move           mask11 to str11
               edit           qtyin to str11
               setitem        InvEditText017,0,str11
.               SetItem        Ninv0001StatText003,0," "
.               SetItem        Ninv0001StatText004,0," "
.               SetItem        Ninv0001StatText005,0," "
               Call           Trim Using chkn1
               setitem        InvEditText022,0,chkn1
               if             (chkn1 <> "" & chk1dtey <> "")
               pack           str12 from CHK1DTEM,SLASH,CHK1DTEd,SLASH,CHK1DTEc,CHK1DTEy
               SetItem        INvStatText043,0,"Paid"
               setitem        InvStatText045,0,str12
               else
               SetItem        INvStatText043,0,""
               Setitem        InvStatText045,0," "
               endif
               Call           Trim Using chkn2
               setitem        InvEditText023,0,chkn2
               if             (chkn2 <> "" & chk2dtey <> "")
               pack           str12 from CHK2DTEM,SLASH,CHK2DTEd,SLASH,CHK2DTEc,CHK2DTEy
               SetItem        INvStatText044,0,"Paid"
               Setitem        InvStatText046,0,str12
               else
               SetItem        INvStatText044,0,""
               Setitem        InvStatText046,0," "
               endif
               Call           Trim Using chkn3
               setitem        InvEditText040,0,chkn3
               if             (chkn3 <> "" & chk3dtey <> "")
               pack           str12 from CHK3DTEM,SLASH,CHK3DTEd,SLASH,CHK3DTEc,CHK3DTEy
               SetItem        INvStatText068,0,"Paid"
               Setitem        InvStatText086,0,str12
               else
               SetItem        INvStatText068,0,""
               Setitem        InvStatText086,0," "
               endif

               Setitem        InvEditText002,0,IMLRCHK
               setitem        InvEditPayTo,0,paytn
               if             (MLRPAYR <> c0)
               MOVE           MASK102 TO str17
               EDIT           MlrPayr TO str17
               setitem        InvEditText003,0,str17
               else
               setitem        InvEditText003,0," "
               endif
               clear          M$AR
               if             (Mlrpayd <> "")
               unpack         MLrpayd into str2,yy,mm,dd
                              Type           MlrPayd
                              if             equal                            ;numeric is date
                              pack           str12 from mm,SLASH,dd,SLASH,str2,yy
                              setitem        InvStatText042,0,str12
                              else
                              setitem        InvStatText042,0,MlrPayd
                              endif
               else
               setitem        InvStatText042,0," "
               endif
               packkey        str8 from IBRKNUM,"/",IBRKcnt
          call      debug
               setitem        InvEditBrk,0,IBrknum
               setitem        InvEditBcnt,0,IBrkcnt
.patch 10.14 18 aug 2010  do not displlay brk/consult from order if they are not on the invoice
                    call      Trim using IBrknum
                    if        (Ibrknum = "")
                    Setitem   InvStatText036,0," "
                    endif
.end patch 10.14 18 aug 2010
               Clear          str10
               pack           str10 from Omdtem,slash,omdted,slash,omdtec,omdtey
               setitem        InvEditText006,0,str10
               Clear          str10
               pack           str10 from Invdtem,slash,Invdted,slash,Invdtec,Invdtey
               setitem        InvEditText009,0,str10
               setitem        InvEditText010,0,LOINVN
               setitem        InvEditText007,0,OMLRPON
               setitem        invedittext011,0,Incc
               setitem        invedittext012,0,paycode
               clear          str11
               move           mask11 to str11
               edit           qtybild to str11
               setitem        InvEditText014,0,str11
.begin patch 10.12
             if     (let90D = "1")
             setitem          InvStatLEtter,0,"Overdue letter sent"      
             Elseif (let90D = "2")
             setitem          InvStatLEtter,0,"Credit Hold letter sent"      
             Else
             setitem          InvStatLEtter,0,"                   "      
             endif
.end patch 10.12
.begin patch 10.001
.               if             (elstcde = "C" & NDATEXCH <> "2")
.begin patch 10.001
.               add            "10" to commpct
.               endif
               Move           Mask32 to str6
               edit           commpct to str6
               Setitem        InvEditText013,0,str6
.end patch 10.001
.               move           mask32 to str6
.              Edit            Commpct to str6
.;               setitem        InvEditText013,0,COMMPCT
.               setitem        InvEditText013,0,Str6
.end patch 10.001
               move           mask32 to str6
               edit           ppm to str6
               setitem        InvEditText015,0,str6
               clear          str11
               move           mask11 to str11
               move           c0 to n9
               call           trim using irexqty
               move           irexqty to n9
               edit           n9 to str11
               setitem        InvEditText018,0,str11
               move           mask32 to str6
               edit           iexppm to str6
               setitem        InvEditText031,0,str6

               pack           Str3 from Irnetper,"%"
               setitem        InvStatText073,0,str3
               move           onetrc to inetrc
               move           Inetrc to str6
               setitem        InvStatText075,0,str6

               setitem        InvStatText083,0,""
               Cmatch         Star,ADJC
               if             equal
               MOVE           NINVFLD TO NADJFLD
               CALL           NADJKEY
               if             over
               pack               taskname,"No Adjustments for LR ## ",NADJFLD
               alert               caution,taskname,result
               else
               setitem        InvStatText083,0,"Adjustment"
               setprop        InvStatText083,FGColor=red
               aLERT          cAUTION,"Adjusted Invoice",result
.               call           seeadj1
               endif
               endif
               return
.......................................................................................................
InvAcdLoadRecord
               call           InvAcdClearRecord
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
. test test  test               call           wipecvars
               move      olrn to nmrgfld
               move      c0 to nmrgrqty
               move      c0 to nmrgiqty
               move      c0 to nmrgnet
               move      no to mrgsw
               move      no to SHIPsw
               call      nmrgkey
               if       not over
               move     yes to mrgsw
               endif
               move      olrn to nshpfld
               call      nshpkey
               if       not over
               move     yes to shipsw
               endif
InvAcdLoadRecord1
               move           yes to subppsw
               call           compute
               FOR           AcdRecCount,"1","15"
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdNumRec,NinvAcdNum
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdCodeRec,NinvAcdCode
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdRateRec,NinvAcdRate
                              MOve   NInvAcdRec(AcdRecCount).NInvAcdPercRec,NInvAcdPerc
                              MOve   NInvAcdRec(AcdRecCount).NINVAcdANINCDRec,NINVAcdANINCD
                              MOve   NInvAcdRec(AcdRecCount).NINvAcdqtyRec,NinvacdQty
                              MOve   NInvAcdRec(AcdRecCount).NINvAcdTotalRec,str15
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdAextcdRec,NinvAcdAextcd
                              MOve   NInvAcdRec(AcdRecCount).NinvAcdRateTRec,NinvAcdRateT
                              MOve   NInvAcdRec(AcdRecCount).NINvAcdDescRec,nacdtext
                              if             (NinvacdNum = "")
               Break
               endif
.               packkey        Nacdfld with NinvAcdCode
.               CALL           NACDKEY
.               if             over
.               Move           "Invalid charge" to Nacdtext
.               endif
               clear          str10
               move           mask72 to str12
               edit           NinvAcdRate to str12
               move           "ZZZ.9999" to str8
               edit           NInvAcdPerc to str8
               if             (NInvAcdRateT = "m")
               move           mask11 to str11
               edit           ninvacdqty to str11
               else
               clear          str11
               endif
               InvListView001.InsertItem giving N9 using NinvAcdNum
               InvListView001.SetItemText using N9,NinvAcdCode,1
               InvListView001.SetItemText using N9,Nacdtext,2
               InvListView001.SetItemText using N9,str11,3
               InvListView001.SetItemText using N9,str12,4
               InvListView001.SetItemText using N9,NinvAcdRateT,5
               InvListView001.SetItemText using N9,str8,6
               InvListView001.SetItemText using N9,NINVAcdANINCD,7
               InvListView001.SetItemText using N9,NINvAcdAextcd,8
               InvListView001.SetItemText using N9,str15,9           amount
               InvListView001.SetColumnFormat using 3,2              .set $ column justify right
               InvListView001.SetColumnFormat using 4,2              .set $ column justify right
               InvListView001.SetColumnFormat using 6,2              .set $ column justify right
               InvListView001.SetColumnFormat using 10,2              .set $ column justify right
               repeat
              call            InvLoadRecord2
              REturn
.........................................................................................................
InvLoadRecord2
              move            c0 to n9
               Move           Irexqty to n9
              if              (n9 > 0)
               MOVE           MASK92 TO M$GROSS
               EDIT           GROSSBaseSR TO M$GROSS
               setitem        InvEditText016,0,m$gross

               MOVE           MASK92 TO M$GROSS
               EDIT           GROSSBaseSE TO M$GROSS
               setitem        InvEditText036,0,m$gross
              Else
               MOVE           MASK92 TO M$GROSS
               EDIT           GROSS TO M$GROSS
               setitem        InvEditText016,0,m$gross
               setitem        InvEditText036,0,"0.00"
              endif

               MOVE           MASK92 TO M$AR
               EDIT           FORMAR TO M$AR
               Move           formar to holdar
               setitem        InvEditText019,0,m$AR
.
               call           debug
               MOVE           MASK92 TO M$AP1
               EDIT           AP TO M$AP1
               Move           ap to holdap
               setitem        InvEditText004,0,m$AP1
.
               MOVE           MASK92 TO M$AP2
               EDIT           FORMAP2 TO M$AP2
               Move           ap2 to holdap2
               setitem        InvEditText005,0,m$AP2
.

               MOVE           MASK92 TO M$AP3
               EDIT           FORMAP3 TO M$AP3
               Move           ap3 to holdap3
               setitem        InvEditText041,0,m$AP3
. 
             MOVE           MASK92 TO M$LRINC
               EDIT           LRINC TO M$LRINC
               move           lrinc to holdlrinc
               setitem        InvEditText020,0,M$LRINC
.
               MOVE           MASK92 TO M$NINC
               EDIT           NININC TO M$NINC
               move           nininc to holdnininc
               setitem        InvEditText021,0,M$NINC
.
               MOVE           MASK52 TO M$STAX
               EDIT           TAXES TO M$STAX
               setitem        InvEditText024,0,M$sTAX
.
.
               MOVE           MASK32 TO M$POST
               EDIT           POST TO M$POST
               setitem        InvEditText025,0,M$POST

               return
.......................................................................................................
InvAcdUnLoadRecord
.add charge #
               InvListView001.Getitemtext giving Ninvacdnum using n9,c0
.add code
               InvListView001.Getitemtext giving NinvAcdCode using n9,c1
.add description
               InvListView001.Getitemtext giving Nacdtext using n9,c2
.add quantity
.               InvListView001.Getitemtext giving str9 using n9,c3
               InvListView001.Getitemtext giving str11 using n9,c3
.add Rate
               InvListView001.Getitemtext giving str12 using n9,c4
.add flat or Per m
               InvListView001.Getitemtext giving NinvAcdRateT using n9,c5
.add % NIN
               InvListView001.Getitemtext giving str8 using n9,c6
               InvListView001.Getitemtext giving NINVAcdANINCD using n9,c7
               InvListView001.Getitemtext giving NinvAcdAextcd using n9,c8
               Return
.......................................................................................................
InvAcdClearRecord
               InvListView001.DeleteAllItems giving N9
               Setitem        InvEditText026,0," "
               Setitem        InvEditText028,0," "
               Setitem        InvEditText029,0," "
               Setitem        InvEditText030,0," "
              setitem        InvComboBox003,0,c0
               Setitem        InvEditText032,0," "
               Setitem        InvEditText037,0," "
               Setitem        InvEditText038,0," "
               return
.......................................................................................................
.InvAcdDeleteDetail - delete one additional charge Need to verify that one is selected
.                     need to establish if selected record has been written to disk or is only in memory
.                     need to check if delete is allowable: within date parameters
.                                                           Some add charges are not allowed to be deleted
.above still needs to be implemented                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.                     IF all criteria met - Alert box ---- No UNdo for Deletion  blah blah blah
.                     delete all occurance in list view object
.                     delete from file
InvAcdDeleteDetail
.Delete the offending item from the list view object
               InvListView001.GetNextItem giving N9 using C2
               InvListView001.GetItemText giving NinvAcdNum using N9,0
               InvListView001.DeleteItem Using N9
.above seems to work - now cleanup the array
               call           NInvAcdRecClear                         .Clear the array
               For            AcdRecCount,"1","15"                    .Reload the Array from list view object
               Move           AcdRecCount to n9
               sub            c1 from n9
               call           InvAcdUnLoadRecord
                              if             (NinvacdNum = "")
                              Break
                              else
                              MOve           NinvAcdNum,NInvAcdRec(AcdRecCount).NinvAcdNumRec       ;add charge #
                              MOve           NinvAcdCode,NInvAcdRec(AcdRecCount).NinvAcdCodeRec       ;add code
                              MOve           str12,NInvAcdRec(AcdRecCount).NinvAcdRateRec             ;rate
                              MOve           str8,NInvAcdRec(AcdRecCount).NInvAcdPercRec
                              MOve           NINVAcdANINCD,NInvAcdRec(AcdRecCount).NINVAcdANINCDRec
                              call           RemoveChar using str11,comma
                              MOve           str11,NInvAcdRec(AcdRecCount).NINvAcdqtyRec                ;str9
                              MOve           str15,NInvAcdRec(AcdRecCount).NINvAcdTotalRec
                              MOve           NinvAcdAextcd,NInvAcdRec(AcdRecCount).NinvAcdAextcdRec        ;
                              MOve           NinvAcdRateT,NInvAcdRec(AcdRecCount).NinvAcdRateTRec         ;add flat or Per m
                              MOve           nacdtext,NInvAcdRec(AcdRecCount).NINvAcdDescRec             ;add description
                              endif
               repeat
               Move           Yes to ModacdFlag
               Call           ClearAcdEdit
.begin patch 10.006
.mmmmmmmm  should also repull all other variables from rest of invoice as they can change them - sigh
               call           InvUnloadForm
.end patch 10.006
               return
.......................................................................................................
InvEnableButtons
               Setprop        InvSearchQuit,Enabled=0
              setprop        InvSearchQuit,Height=0
               Setprop        InvSearchGo,Enabled=0
              setprop        InvSearchGo,Height=0
               Setprop        InvOK,Enabled=1
              setprop        INvok,Height=20
               Setprop        InvSave,Enabled=1
              setprop        InvSave,Height=20
               Setprop        InvSaveQuit,Enabled=1
              setprop        InvSaveQuit,Height=20
               Setprop        InvSearch,Enabled=1
              setprop        INvSearch,Height=20
               return
.......................................................................................................
InvButtonDefault
               Setprop        InvSearchQuit,Enabled=0
               Setprop        InvSearchQuit,height=0
               Setprop        InvSearchGo,Enabled=0
               Setprop        InvSearchGo,Height=0
               Setprop        InvOK,Enabled=1
               Setprop        InvOK,Height=20
               Setprop        InvSearch,Enabled=1
               Setprop        InvSearch,Height=20
               setprop        INVSave,Height=0
               setprop        INVSave,enabled=0
               Setprop        InvButtonfax,height=0
               Setprop        InvButtonLomemo,height=0
               Setprop        InvButtonHPRint,height=0
               Setprop        InvButtonRePRint,height=0
               Setprop        InvModify,Enabled=0
               Setprop        InvModify,Height=0
               Setprop        InvAcdButtonCalc,height=0
               Setprop        InvButtonAdjSum,height=0
               Setprop        InvAcdButtonCalc,visible=0
               Setprop        InvButtonAdjSum,visible=0

               Return
.......................................................................................................
InvButtonModify
               Setprop        InvSearch,Enabled=0
               Setprop        InvSearch,Height=0
               Setprop        InvModify,Enabled=1
               Setprop        InvModify,Height=20
               Setprop        InvButtonfax,height=20
               Setprop        InvButtonLomemo,height=20
               Setprop        InvButtonHPRint,height=20
               Setprop        InvButtonHPRint,visible=1
               Setprop        InvButtonRePRint,height=20
               Setprop        InvAcdButtonCalc,height=20
               Setprop        InvButtonAdjSum,height=20
               Setprop        InvAcdButtonCalc,visible=1
               Setprop        InvButtonAdjSum,visible=1
               return
.......................................................................................................
InvDisableButtons
               return
.......................................................................................................
.order note goodies
INVLoadNotes
               call               InvDisableOrderNotes
               clear               hold3
               deleteitem    NInv0001BDataList001,0
               move               OLRN,NONOFLD
               move               "O.LoadNotes-NONOKEY",Location
               pack               KeyLocation,"Key: ",NONOFLD
               call               NONOKEY
               if not over
                              setitem        InvStatText084,0,"Order Notes"
                              setprop        InvStatText084,FGColor=Red
                              aLERT          cAUTION,"Order Notes",result
                              unpack               NTIME,str2,str3
                              pack               str5,str2,COLON,str3
                              unpack               NDATE,MM,DD,YY,STR2
.START PATCH        10.021    REPLACED LOGIC
.                              pack               hold3,NOTEKEY,B1,MM,SLASH,DD,SLASH,STR2,YY,B2,str5,B1,NLINE1:
.                                             NLINE2,NLINE3,NLINE4,NLINE5,NLINE6,NINITS
                                    pack               hold3,NOTEKEY,B1,MM,SLASH,DD,SLASH,STR2,YY,B2,str5,B1,NINITS,NDATE,NTIME
.END PATCH          10.021    REPLACED LOGIC
                              insertitem   NInv0001BDataList001,0,hold3
                              loop
                                             move               "LoadNotes-NONOKS",Location
                                             pack               KeyLocation,"Key: ",NONOFLD
                                             call               NONOKS
                                             until over
                                             until (NOTEKEY <> NONOFLD)
                                             unpack               NTIME,str2,str3
                                             pack               str5,str2,COLON,str3
                                             unpack               NDATE,MM,DD,YY,STR2
.START PATCH        10.021    REPLACED LOGIC
.                                             pack               hold3,NOTEKEY,B1,MM,SLASH,DD,SLASH,STR2,YY,B2,str5,B1:
.                                                            NLINE1,NLINE2,NLINE3,NLINE4,NLINE5,NLINE6,NINITS
                                                       pack               hold3,NOTEKEY,B1,MM,SLASH,DD,SLASH,STR2,YY,B2,str5,B1,NINITS,NDATE,NTIME
.END PATCH          10.021    REPLACED LOGIC
                                             insertitem     NInv0001BDataList001,0,hold3
                              repeat
                              setitem               NInv0001BDataList001,1,1
               endif
               call               InvLoadNotes2

               return

INvLoadNotes2
.START PATCH 10.021 ADDED LOGIC
          NInv0001BDataList001.GetCount giving result
          if (result = 0)
                    setitem   NInv0001BEditText002,0,""
                    setitem   NInv0001BEditText001,0,""
                    setitem   NInv0001BEditText003,0,""
                    setitem   NInv0001BEditNotes,0,""
                    return
          endif
.END PATCH 10.021 ADDED LOGIC
               getitem               NInv0001BDataList001,0,result
               getitem               NInv0001BDataList001,result,hold3
.START PATCH        10.021    REPLACED LOGIC
.               unpack               hold3,NOTEKEY,str1,str10,str2,str5,str1,NLINE1,NLINE2,NLINE3:
.                              NLINE4,NLINE5,NLINE6,NINITS
               unpack               hold3,NOTEKEY,str1,str10,str2,str5,str1,NINITS,NDATE,NTIME
.END PATCH          10.021    REPLACED LOGIC
               call               Trim using str10
               count               N2,str10
               if (N2 <> 10)
                              clear               str10
               endif
               setitem               NInv0001BEditText002,0,str10
               setitem               NInv0001BEditText001,0,str5
               setitem               NInv0001BEditText003,0,NINITS
.START PATCH        10.021    REPLACED LOGIC
.               setitem               NInv0001BEditText004,0,NLINE1
.               setitem               NInv0001BEditText005,0,NLINE2
.               setitem               NInv0001BEditText006,0,NLINE3
.               setitem               NInv0001BEditText007,0,NLINE4
.               setitem               NInv0001BEditText008,0,NLINE5
.               setitem               NInv0001BEditText009,0,NLINE6
. now do read on notekey, ndate, and ntime
.         save old vars
          move ndate, ndate2
          move ntime, ntime2
          move      notekey,NONOFLD   // notekey will change - old value still in nonofld
          move      "NInv0001.LoadNotes-NONOKEY",Location
          pack      KeyLocation,"Key: ",NONOFLD
          call      NONOKEY
          if not over
                    if (nonofld=notekey && ndate2=ndate && ntime2=ntime)  // is found record the one I want?
                              call      TRIM using NLINE
                              setitem   NInv0001BEditNotes,0,NLINE
                    else
                              loop
                                        move      "NInv0001.LoadNotes-NONOKS",Location
                                        pack      KeyLocation,"Key: ",NONOFLD
                                        call      NONOKS
                                        until over
                                        until (NOTEKEY <> NONOFLD)
                                        if (nonofld=notekey && ndate2=ndate && ntime2=ntime) // is found record the one I want?
                                                  call      TRIM using NLINE
                                                  setitem   NInv0001BEditNotes,0,NLINE
                                        endif
                              repeat
                    endif
          endif
.END PATCH          10.021    REPLACED LOGCI
               setfocus      NInv0001BDataList001
               return


.......................................................................................................
InvClearUpper
               setitem        InvEditTextLR,0," "
               setitem        InvEditTextInv,0," "
               setitem        InvStatText035,0," "
               setitem        InvStatText036,0," "
               setitem        InvStatText037,0," "
               setitem        InvStatText038,0," "
               setitem        InvStatText039,0," "
.               setitem        InvStatText040,0," "
               setitem        InvStatText041,0," "
               setitem        InvStatText042,0," "
               setitem        InvStatText043,0," "
               setitem        InvStatText044,0," "
               setitem        InvStatText045,0," "
               setitem        InvStatText046,0," "
               setitem        InvStatText047,0," "
.               setitem        InvStatText048,0," "
               setitem        InvStatText049,0," "
.               setitem        InvStatText050,0," "
               setitem        InvStatText051,0," "
               setitem        InvStatText052,0," "
               setitem        InvStatText053,0," "
               setitem        InvStatText059,0," "
               setitem        InvStatText060,0," "
               setitem        InvStatText061,0," "
               setitem        InvStatText062,0," "
.               setitem        InvStatText063,0," "
.               setitem        InvStatText064,0," "
.               setitem        InvStatText065,0," "
.               setitem        InvStatText066,0," "
.               setitem        InvStatText067,0," "
               setitem        InvStatText068,0," "
.               setitem        InvStatText069,0," "
.               setitem        InvStatText070,0," "
.               setitem        InvStatText071,0," "
               setitem        InvStatText073,0," "
               setitem        InvStatText074,0," "
               setitem        InvStatText075,0," "
               setitem        InvStatText076,0," "
               setitem        InvStatText078,0," "
               setitem        InvStatText079,0," "
               setitem        InvStatText080,0," "
               setitem        InvStatText082,0," "
               setitem        InvStatText086,0," "
.
               setitem        InvEditText040,0," "
               setitem        InvEditText035,0," "
               setitem        InvEditText034,0," "
               setitem        InvEditText033,0," "
               setitem        InvEditText025,0," "
               setitem        InvEditText024,0," "
               setitem        InvEditText023,0," "
               setitem        InvEditText022,0," "
               setitem        InvEditText021,0," "
               setitem        InvEditText020,0," "
               setitem        InvEditText019,0," "
               setitem        InvEditText018,0," "
               setitem        InvEditText017,0," "
.               setitem        InvEditText016,0," "
               setitem        InvEditText015,0," "
               setitem        InvEditText014,0," "
               setitem        InvEditText013,0," "
.               setitem        InvEditText012,0," "
               setitem        InvEditText011,0," "
               setitem        InvEditText010,0," "
.               setitem        InvEditText009,0," "
               setitem        InvEditText008,0," "
               setitem        InvEditText007,0," "
               setitem        InvEditText006,0," "
               setitem        InvEditText005,0," "
               setitem        InvEditText004,0," "
               setitem        InvEditText003,0," "
               setitem        InvEditText002,0," "
               setitem        InvEditPayTo,0," "
               setitem        InvEditMlr,0," "
               setitem        InvEditMCnt,0," "
               setitem        InvEditBrk,0," "
               setitem        InvEditBCnt,0," "
               setprop        InvStatText053,FGColor=black
.begin patch 10.3
               setitem        NINV0001ACheckEntire,0,0
.end patch 10.3

               call           InvClearPage2

               return
.......................................................................................................
INvDisableEntry
               Setprop        InvEditTextLR,enabled=0
               Setprop        InvEditTextInv,Enabled=0
               Setprop        InvEditText040,Enabled=0
               Setprop        InvEditText035,Enabled=0
               Setprop        InvEditText034,Enabled=0
               Setprop        InvEditText033,Enabled=0
               Setprop        InvEditText025,Enabled=0
               Setprop        InvEditText024,Enabled=0
               Setprop        InvEditText023,Enabled=0
               Setprop        InvEditText022,Enabled=0
               Setprop        InvEditText021,Enabled=0
               Setprop        InvEditText020,Enabled=0
               Setprop        InvEditText019,Enabled=0
               Setprop        InvStatText037,Enabled=0
               Setprop        InvEditText017,Enabled=0
               Setprop        InvEditText016,Enabled=0
               Setprop        InvEditText015,Enabled=0
               Setprop        InvEditText014,Enabled=0
               Setprop        InvEditText013,Enabled=0
               Setprop        InvEditText012,Enabled=0
               Setprop        InvEditText011,Enabled=0
               Setprop        InvEditText010,Enabled=0
               Setprop        InvEditText009,Enabled=0
               Setprop        InvEditText008,Enabled=0
               Setprop        InvEditText007,Enabled=0
               Setprop        InvEditText006,Enabled=0
               Setprop        InvStatText035,Enabled=0
               Setprop        InvStatText036,Enabled=0
               Setprop        InvEditMlr,Enabled=0
               Setprop        InvEditMCnt,Enabled=0
               Setprop        InvEditBrk,Enabled=0
               Setprop        InvEditBCnt,Enabled=0
               Setprop        InvStatText038,Enabled=0
               Setprop        InvStatText039,Enabled=0
               return
.......................................................................................................
INvEnableOrderFields
               Setprop        InvEditTextLR,enabled=1
               Setprop        InvEditText007,enabled=1
               Setprop        InvEditText008,enabled=1
               Setprop        InvEditBrk,enabled=1
               Setprop        InvEditMlr,enabled=1
               setfocus       InvEditBrk
               return
.......................................................................................................
INvEnableInvFields
               Setprop        InvEditTextInv,enabled=1
               Setprop        InvEditText010,enabled=1
               Setprop        InvEditText007,enabled=1
               Setprop        InvEditText002,enabled=1
               setProp        InvEditPayTo,enabled=1
               Setprop        InvEditText022,enabled=1
               Setprop        InvEditText023,enabled=1
               Setprop        InvEditText040,enabled=1
               setfocus       InvEditText010                ;LO Invoice

               return
.......................................................................................................
InvClearLower
               return
.......................................................................................................
InvSearch
               CALL               NinvAIM
               IF                 NOT OVER
               move               c1 to nordpath
               packkey            Nordfld from lrn
               call               nordkey
               call               INvSearchLoad1
               else
               call               invbuttondefault
               call               InvEnableButtons
               return
               endif
.
               Loop
               MOve           "InvSearch-NORDKG",Location
               call           NInvKG
               until          over
               packkey        Nordfld from lrn
               call           nordkey
               call           INvSearchLoad1
               repeat
.
               setprop       InvSearchQuit,Height=0
               setprop       InvSearchGo,Height=0
               setprop       INvSearch,Height=20
               setprop       INvok,Height=20
               setfocus      InvoiceListView
               return
.......................................................................................................
OrderSearch
               CALL          NordAIM
               IF            NOT OVER
               call          INvSearchLoad
               else
               call               invbuttondefault
               call               InvEnableButtons
               return
               endif
.
               Loop
               MOve           "InvSearch-NORDKG",Location
               call           NORDKG
               until          over
               call           INvSearchLoad
               repeat
.
               setprop       InvSearchQuit,Height=0
               setprop       InvSearchGo,Height=0
               setprop       INvSearch,Height=20
               setprop       INvok,Height=0
               setfocus       InvoiceListView
               return
.......................................................................................................
InvSearchLoad
               Getitem         OptionsBilledNo,0,n5         ;include billed ?
               IF             (N5 = c0)
               move           "QB" to str2
               scan           ostat in str2
               return         if equal
               endif
InvSearchLoad1                                              ;searching invloices skip check above
               move           "pxlz" to str4
               scan           ostat in str4
               IF             equal
               return
               endif
.

               Getitem         OptionsCancNo,0,n5         ;include unbilled cancelled?
               IF             (N5 = c0)
               move           "X" to str2
               scan           ostat in str2
               return         if equal
               endif
.begin patch 10.3
          if (OELCODE = "1")
                    setitem   Ninv0001ACheckEntire,0,1
          elseif (OELCODE     = "2")
                    setitem   Ninv0001ACheckEntire,0,0
          elseif (OELCODE     = "3")
                    setitem   Ninv0001ACheckEntire,0,1
          else      .OELCODE = B1
                    setitem   Ninv0001ACheckEntire,0,0
          endif
.end patch 10.3

               InvoiceListView.InsertItem giving N9 using Olrn
               InvoiceListView.SetItemText using N9,Omlrpon,1
               InvoiceListView.SetItemText using N9,Omlrnum,2
               InvoiceListView.SetItemText using N9,Olnum,3
               InvoiceListView.SetItemText using N9,Obrknum,4
               InvoiceListView.SetItemText using N9,OMLrky,5
               clear          hold
               pack           hold,Ordvars
               InvoiceListView.SetItemText using N9,Hold,6
               return
.......................................................................................................
InvSearchClear
               InvoiceListView.DeleteAllItems giving N9
               setitem        InvStatRec,0,""
               call           InvClearUpper
              Call            InvClearLower
               return
.......................................................................................................
InvClearPage2
               setitem               Ninv0001bEditText001,0," "
               setitem               Ninv0001bEditText002,0," "
               setitem               Ninv0001bEditText003,0," "
               setitem               Ninv0001bStatText007,0," "
               setitem               Ninv0001bStatText008,0," "
               return
.........................................................................................................
InvDisableOrderNotes
.called               by:  INVLoadNotes
               setitem               InvStatText084,0,""
               setprop               NInv0001BCancel,enabled=0
               setprop               NInv0001BSave,enabled=0
               setprop               NInv0001BNew,enabled=1
               setprop               NInv0001BNew,visible=1
.........................................................................................................
.START PATCH        10.021    REPLACED LOGIC
.               setprop               NInv0001BEditText004,enabled=0
.               setprop               NInv0001BEditText005,enabled=0
.               setprop               NInv0001BEditText006,enabled=0
.               setprop               NInv0001BEditText007,enabled=0
.               setprop               NInv0001BEditText008,enabled=0
.               setprop               NInv0001BEditText009,enabled=0
                 setprop                     NInv0001BEditNotes,enabled=1  // so user can use scroll
                 setprop                     NInv0001BEditNotes,readonly=C1 // but can't change!
.END PATCH          10.021    REPLACED LOGIC
               return
.........................................................................................................
INvEnableOrderNotes
.Called               by New_Click
.START PATCH        10.021    REPLACED LOGIC
.               setprop               NInv0001BEditText004,enabled=1
.               setprop               NInv0001BEditText005,enabled=1
.               setprop               NInv0001BEditText006,enabled=1
.               setprop               NInv0001BEditText007,enabled=1
.               setprop               NInv0001BEditText008,enabled=1
.               setprop               NInv0001BEditText009,enabled=1
               setprop               NInv0001BCancel,enabled=1
               setprop               NInv0001BSave,enabled=1
               setprop               NInv0001BNew,enabled=1
               setprop               NInv0001BCancel,visible=1
               setprop               NInv0001BSave,visible=1
               setprop               NInv0001BNew,visible=1
                 setprop                     NInv0001BEditNotes, enabled=1
                 setprop                     NInv0001BEditNotes,readonly=C0 // let user change it!
.END PATCH          10.021    REPLACED LOGIC
               return
.end patch 10.0
.........................................................................................................
.begin patch 10.0
ClearAcdEdit
              setitem InvEditText026,0,""
              setitem InvEditText037,0,""
              setitem InvEditText028,0,""
              setitem InvEditText029,0,""
              setitem InvEditText030,0,""
              setitem InvEditText032,0,""
              setitem InvEditText038,0,""
              Call    LoadNinvAcdCombo003
.LoadNinvAcdCombo003 is used elsewhere and load qtyin to inveditext037 so reset it
              setitem InvEditText037,0,""
               return
.........................................................................................................
DisableAcdButtons
              setprop         InvAcdButtonDel,Enabled=0
              setprop         InvAcdButtonDel,height=0
              setprop         InvAcdButtonSave,Enabled=0
              setprop         InvAcdButtonSave,height=0
              setprop         InvAcdButtonQuit,Enabled=0
              setprop         InvAcdButtonQuit,height=0
               return
EnableAcdButtons
               Call           DisableAcdButtons
               IF             (mode = c0 or Mode = c1)
               return                                         ;inquire mode
               Elseif         (Mode = c2)                     ;Modify Mode
.check date param's
              setprop         InvAcdButtonSave,Enabled=1
              setprop         InvAcdButtonSave,height=20
              setprop         InvAcdButtonDel,Enabled=1
              setprop         InvAcdButtonDel,height=20
              setprop         InvAcdButtonQuit,Enabled=1
              setprop         InvAcdButtonQuit,height=20
               Elseif         (Mode = c3)                     ;add Mode
              setprop         InvAcdButtonSave,Enabled=1
              setprop         InvAcdButtonSave,height=20
              setprop         InvAcdButtonDel,Enabled=1
              setprop         InvAcdButtonDel,height=20
              setprop         InvAcdButtonQuit,Enabled=1
              setprop         InvAcdButtonQuit,height=20
               endif
               return
.........................................................................................................
.begin patch 10.0
.........................................................................................................
SetNInvErrorMssgDefault
               setprop        ErrorMssgStat1,visible=1
               setprop        ErrorMssgStat2,visible=1
               setprop        ErrorMssgStat3,visible=1
               setprop        ErrorMssgStat4,visible=1
               setprop        ErrorMssgStat5,visible=0
               setitem        ErrorMssgStat1,0,"Enter 4 Digit Mailer Number:"
               setitem        ErrorMssgStat2,0,""
               setitem        ErrorMssgStat3,0,"    Or hit F2 to Search"
               setitem        ErrorMssgStat4,0,"      By Company Name"
               setitem        ErrorMssgStat5,0,"      That Mailer Does Not Exist!"
               setitem        ErrorMssgOK,0,"&OK"
               return
.......................................................................................................................
NInvForceToOne
               setitem        NInv001TabControl001,0,1
               move           c1 to n1
               call           NInvTabChange

               return

...........................................................................................
NInvTabClick
               IF             (N1 = C1)
               Deactivate NINVA
               elseif         (N1 = C2 )
               Deactivate NINVB
               elseif         (N1 = C3 )
               call            DisableAdjustForm
               Endif
               return
...........................................................................................
NInvTabChange
               Deactivate NInvA
               Deactivate NINVB
               IF             (N1 = C1)
                              move           C1,TabNum
                              Activate       NINVa
                              setfocus       InvEditTextLR
                              LOOP
                              CLEAREVENT
                              UNTIL OVER
                              REPEAT

               elseif         (N1 = C2)
                              move           C2,TabNum
                              Activate       NINVb
                              Setprop        NInv0001BSave,visible=1
                              Setprop        NInv0001Bnew,visible=1
                              Setprop        NInv0001BCancel,visible=1
                              setfocus       Ninv001TabControl001
.Prevent occurance or accumulated events which may place "hidden" objects on wrong form
                              LOOP
                              CLEAREVENT
                              UNTIL OVER
                              REPEAT

               elseif         (N1 = C3)
                              move           C3,TabNum
                              Move           Olrn to GlobalVar1
                              call           EnableAdjustForm
                              LOOP
                              CLEAREVENT
                              UNTIL OVER
                              REPEAT
               Endif
               return
............................................................................................................................................
CheckSplit
.check for split.............................................................

               move           c0 to irexqty
               move           c0 to n9
               MOVE           oexqty TO irexqty
               move           oexqty to n9
               compare        c0 to n9
               goto           nosplt if equal
.
               SCAN           "DAWSON" IN MCCTO       .dawson deal
               IF             EQUAL
               scan           omlrnum in dawsmlrs
                              if         equal
                              move       c5 to iexppm
                              else
                              MOVE       C7 TO iexppm
                              GOTO       NetExt
                              ENDIF
               endif
.
               match          "0171" to obrknum       .dawson deal
               IF             EQUAL
               scan           omlrnum in dawsmlrs
                              if         equal
                              move       c5 to iexppm
                              else
                              MOVE       C7 TO iexppm
                              GOTO       NetExt
                              ENDIF
               endif
.
               match          "0006" to omlrnum       .splc deal
               if             equal
               move           c7 to iexppm
               goto           NetExt
               endif
.
               CLEAR          MM
               CLEAR          DD
               CLEAR          YY
               move           irexqty to n9
               COMPARE        n9 TO TENDOLL
               IF             NOT LESS
               MOVE           c10 TO iexppm
               goto           NetExt
               ENDIF
.
               COMPARE        n9 TO NINEDOLL
               IF             NOT LESS
               MOVE           c9 TO iexppm
               goto           NetExt
               ENDIF
.
               COMPARE        SEVDOLL TO n9
               IF             NOT LESS
               MOVE           c7 TO iexppm
               goto           NetExt
               ENDIF
.
               MOVE           c8 TO iexppm
.***********************************
NoSplt
.
               if             (netflag = "1")        .not net
               goto           netext                 .goto net exit
               else                              .yes
               move           c2 to netflag
               endif
               compare        c1 to netflag
               goto           netext if equal          .not a net
.lets get net goodies
.1st is there a minimum & do we meet it?
               compare        c0 to onetmin
               if             equal           .no min
                              if             (Mrgsw <> yes & LstmSw <> Yes & OnetFm <> "F")
                              aLERT          cAUTION,"Can't do NET --> No merge data",result
                              endif
.
                              If             (OnetFM = "F" or LstmSw = Yes)
                              move      c3 to netflag
                              goto      flat1
                              endif
               endif
NetExt
           Return
............................................................................................................................................
flat1
.if its marked Flat(volume) & is not from an outside broker, use merge info if
. we have it. DLH 12Jan96
               cmatch         yes to mrgsw            .do we have merge info?
               if             equal                  .yes
               cmatch         yes to lstmsw          .list management ? turned off 1/97.
                              if        not  equal             .nope.
                              move      inpqty  to net94
                              move      irnetper to form72
                              div       hund into form72
                              mult      form72 by net94
                              move      net94 to n10              .force round to whole # of names
                              move      n10 to net94              .ditto
                              div       thous into net94         . "
                              move      net94 to n10
                              goto     flat1a
                              endif
               endif

               cmatch         yes to shipsw            .do we have shipping?
               if             equal
               move           squant  to net94
               move           irnetper to form72
               div            hund into form72
               mult           form72 by net94
               move           net94 to n10              .force round to whole # of names
               move           n10 to net94              .ditto
               div            thous into net94          added 10/13/95
               move           net94 to n10
               else
               move           oqty  to net94
               move           irnetper to form72
               div            hund into form72
               mult           form72 by net94
               move           net94 to n10              .force round to whole # of names
               move           n10 to net94              .ditto
               div            thous into net94        added 10/13/95
               move           net94 to n10
               endif
.
flat1a
               move           irnetper to form94
               MOVE           C0 TO PERCENT
               ADD            FORM94 TO PERCENT
               move           net94 to Form94a
               mult           "1000" by form94a
               move           form94a to n10
               Return
............................................................................................................................................
ADDInvAcdDet
.lets do some checking first codes 115-150 are not currently used but are placeholders
.if they tried to use one - No No NO
               GetItem        InvEditText026,0,str4
               move           str4 to n4
.               if             (N4 >= 115 & n4 <= 150)
.patch jan 2011 new SC codes
.               if             (N4 >= 116 & n4 <= 150)
               if             (N4 >= 118 & n4 <= 150)
.end patch jan 2011 new SC codes
               Alert          caution,"Invalid additional charge code!",result
               setfocus       INvedittext026
               return
               endif
               Getitem       InvEditText032,0,NINvacdRateT                                    ;' ' or 'f'  or 'm'
               If             (NinvAcdRateT = "m")
               getitem        InvEditText037,0,str11                                 ;qty
               call           RemoveChar using str11,comma
               call           trim using str11
               clear          str9
               move           str11,str9
               Move           str9 to NInvAcdQty
               else
               MOve           c0 to ninvacdqty
               endif
.should be handled elsewhere
.mmmmmmmm  should also repull all other variables from rest of invoice as they can change them - sigh
               call           InvUnloadForm
.trying to pull them above  (invloadform)
               Move           str11 to qtybild
.
               Getitem       InvEditText028,0,str12                                   ;Rate
               Getitem       InvEditText029,0,str8                                    ;Percentage
.Cannot exceed 1.00 - check
               move           c0 to work3
               move           str8 to work3
               if             (work3 > 1.00)
               Alert          caution,"Cannot Exceed 1.0 (100%)!",result
               setfocus       INvedittext029
               return
               endif
               Getitem       InvEditText030,0,NINVAcdANINCD
               Getitem       InvEditText038,0,NINvAcdAextcd
......
boogers
               setfocus       InvEditText026
               FOR           AcdRecCount,"1","15"
                              MOve          NInvAcdRec(AcdRecCount).NinvAcdcodeRec to str3
               IF             (str3 = "" or str3 = "   ")
               break
               endif
               repeat
               Move           AcdRecCount to NinvAcdNum
               MOve           NinvAcdNum to NInvAcdRec(AcdRecCount).NinvAcdNumRec
               MOve           NACDKEY to NInvAcdRec(AcdRecCount).NinvAcdCodeRec
               MOve           str12 to NInvAcdRec(AcdRecCount).NinvAcdRateRec
               MOve           str8 to NInvAcdRec(AcdRecCount).NInvAcdPercRec
               MOve           NINVAcdANINCD to NInvAcdRec(AcdRecCount).NINVAcdANINCDRec
               Move           NInvAcdQty to NInvAcdRec(AcdRecCount).NINVAcdQtyRec
               MOve           NinvAcdAextcd to NInvAcdRec(AcdRecCount).NinvAcdAextcdRec
               Move           NINvAcdRateT to NInvAcdRec(AcdRecCount).NinvAcdRateTRec

. test test test
.tester
               Call           InvAcdClearRecord
               If             (Mode = c2)                             .modify mode
               move           yes to Modacdflag                       .set flag to delete all acd records and write out the changes
               Call           InvAcdLoadRecord1
               else
               Call           InvAcdLoadRecord1
               endif
               return

............................................................................................................................................
.value of Invoice code has changed.   Verify code and set other fields
.legitimate codes are ' ' and 0,1,2,3,4,   I find no functionality for '4'
VerifyInvCode
.check to see if in modify or add mode
.check date parameters
               IF             (Passflag = "N")
               move           "I",progcode
               setprop        Passwrd,visible=1
                             If             (passflag = "N")
                             return
                             MOve            C1 to mode
                             endif
               endif
               Getitem        InvEditText012,0,str1
               if             (str1 = " ")
               setprop        InvEditText004,Enabled=0                   ;a/p1
               setprop        InvEditText005,Enabled=0                   ;a/p2
               setprop        InvEditText041,Enabled=0                   ;a/p3
               return
               Elseif         (str1 = "1")
               setprop        InvEditText004,Enabled=0                   ;a/p1
               setprop        InvEditText005,Enabled=0                   ;a/p2
               setprop        InvEditText041,Enabled=0                   ;a/p3
               return
               Elseif         (str1 = "2")
               setprop        InvEditText004,Enabled=0                   ;a/p1
               setprop        InvEditText005,Enabled=0                   ;a/p2
               setprop        InvEditText041,Enabled=0                   ;a/p3
               setfocus       InvEditText004
               return
               Elseif         (str1 = "3")
               setprop        InvEditText004,Enabled=1                   ;a/p1
               setfocus       InvEditText004
               return
               elseif         (str1 = "4")
               setprop        InvEditText004,Enabled=0                   ;a/p1
               setprop        InvEditText005,Enabled=0                   ;a/p2
               setprop        InvEditText041,Enabled=0                   ;a/p3
               return
               endif
               Return
............................................................................................................................................
VerifySave
. ...
.Still need to actually verify
               call           InvUnloadFOrm
               If             (Mode = C3)                      .Add mode
               Call            MuvBlad
               Else
               Call           Updater
               endif
               call           InvButtonDefault

               return
............................................................................................................................................
InvUnloadForm
               MOVE           OMLRNUM,MLRN
               MOVE           OCOBN,COBN
               move           obrknum to ibrknum
               move           obrkcnt to ibrkcnt
               move           olrn to lrn
               getitem        InvEditText010,0,LOINVN
               getitem        Invedittext011,0,Incc
               getitem        InveditPayTo,0,paytn
               getitem        Invedittext012,0,paycode
               If             (paycode = "3")
               getitem        InvEditText004,0,str15
               Move           "," to str1
               call           RemoveChar using str15,str1
               Move           "-" to str1
               call           RemoveChar using str15,str1
               MOve           Str15 to AP1
               MOve           Str15 to AP
.check for code "3" if applicable pull ap1 InvEditText004 info and save in AP field
               endif
               getitem        InvEditText014,0,str11
               call           RemoveChar using str11,comma
               call           trim using str11
               Move           str11 to qtybild
               getitem        InvEditText017,0,str11
               call           RemoveChar using str11,comma
               call           trim using str11
               Move           str11 to qtyin

.begin patch 10.001
               getitem        InvEditText013,0,str6
               Move           c0 to commpct
               move           str6 to commpct

.end patch 10.001

               getitem        InvEditText015,0,str6
               call           trim using str6
               move           str6 to ppm
               getitem        InvEditText018,0,str11
               call           RemoveChar using str11,comma
               call           trim using str11
               Move           str11 to str9
               call           ZFILLIT using str9
               move           str9 to Irexqty
.testing  24 Feb 2010 DH
               move           c0,str6
               move           c0,iexppm
.testing  24 Feb 2010 DH
               getitem        InvEditText031,0,str6
               call           trim using str6
               move           str6 to iexppm
               getitem        InvEditText014,0,str11
               call           RemoveChar using str11,comma
               call           trim using str11
               move           str11 to qtybildn
               return

............................................................................................................................................
InvClearVars
.               move       c0 to achgctr
               clear     addcd1
               clear     addcd2
               clear     addcd3
               clear     addcd4
               clear     addcd5
               clear     addcd6
               clear     addcd7
               clear     addcd8
               clear     addcd9
               clear     addcd10
               clear     addp1
               clear     addp2
               clear     addp3
               clear     addp4
               clear     addp5
               clear     addp6
               clear     addp7
               clear     addp8
               clear     addp9
               clear     addp10
               clear     addncd1
               clear     addncd2
               clear     addncd3
               clear     addncd4
               clear     addncd5
               clear     addncd6
               clear     addncd7
               clear     addncd8
               clear     addncd9
               clear     addncd10
               clear          LOINVN
               clear     CODE
               clear  STATB
               clear     MLRN
               clear   LRN
               clear   BILLTN
               clear   PAYTN
               clear   LOINVN
               sub      ar,AR
               sub      ap1,Ap1
               sub      ap2,Ap2
               sub      ap3,Ap3
.begin patch 10.001
.               clear   COMMPCT
               sub   COMMPCT,commpct
.end patch 10.001
               clear   PAYCODE
               sub      qtyin,QTYIN
               sub      qtybild,QTYBILD
               sub      qtybildn,QTYBILDn
               sub      ppm,PPM
               clear    INVNUM
               clear     INVDTEc
               clear     INVDTEY
               clear     INVDTEM
               clear     INVDTED
.begin patch 10
.               clear     addchg1
.               clear     addchg2
.               clear     addchg3
.               clear     addchg4
.               clear     addchg5
.               clear     addchg6
.               clear     addchg7
.               clear     addchg8
.               clear     addchg9
.               clear     addchg10
.end patch 10
               clear     COBN
               clear     invsales
               clear     INCC
               clear     LON
               clear     lon1
               clear     lon2
               clear     WSJPC
               clear     IBRKNUM
               clear     IBRKCNT
               clear     IRCQTY
               clear     IREXQTY
               sub      iexppm,iexPPM
               clear     irnetper
               sub      inetrc,inetrc
               clear     ADJC
               clear     statpay
               clear     CHKN1
               clear     CHK1DTEc
               clear     CHK1DTEY
               clear     CHK1DTEM
               clear     CHK1DTED
               clear     CHKN2
               clear     CHK2DTEc
               clear     CHK2DTEY
               clear     CHK2DTEM
               clear     CHK2DTED
               clear     CHKN3
               clear     CHK3DTEc
               clear     CHK3DTEY
               clear     CHK3DTEM
               clear     CHK3DTED
               clear     LET90D
               sub      mlrpayr,MLRPAYR
               clear     MLRPAYD
               clear     GUARPAY
               clear     IMLRCHK
               clear     ppsw
.               sub       ppamt,ppamt

               CLEAR       NINVFLD
               CLEAR     KEY
.               SUB      CHGNUM,CHGNUM
.               SUB      CD3PAY1,CD3PAY1
               SUB      POST,POST
.               SUB      DISPTAX,DISPTAX
.               SUB       CTAX,CTAX
.               SUB       STAX,STAX
.               MOVE      B1,COMMLINE
.               MOVE     B1,SAV1
.               MOVE     B3,SAV3
.               MOVE     B4,SAV4
               MOVE      C0,BILLTN

.               move      c0 to dfppmx
               MOVE      C0,PAYTN
.               MOVE     B1,SAVCD
.               move     c0 to achgctr
.begin patch 10.0
.               MOVE      NO,ADREC    .DLH 05NOV92
.               MOVE      C2 TO ADMOD  .DLH 05NOV92
.end patch 10.0
.               MOVE      B1,CHANGES
.               MOVE      B1,MINV
               MOVE      B4,LON
               MOVE      "      ",CHKN2
               MOVE      B1,WSJPC
               MOVE      B1 TO ADJC
               MOVE      "      ",CHKN1
               MOVE      B2,CHK1dtem
               MOVE      B2,CHK1dtey
               MOVE      B2,CHK1dted
               MOVE      B2,CHK1dtec
.               MOVE      B1,I1
               clear     mlrpayd
               MOVE      C0 TO REPRINT
               clear     mrgsw
               clear     irexqty
               move      b5 to iexppm
               Clear          Obrknum
               Clear          Omlrnum
               Clear          Nmoafld
               move      b4 to obrknum
               move       b3 to obrkcnt
               move      b4 to ibrknum
               move       b3 to ibrkcnt
               move      b1 to brkflag
               move      c1 to netflag
               clear     onetper
               clear     nbrkfld
               clear      oexqty
               clear      mbildrct
               clear      ircqty
               move       c0 to inetrc
.begin patch 10.008
              move            c1 to mode                 ;set to inquire
.end patch 10.008
.begin patch 10.018
              Clear           INVVARS
              Clear           Ordvars
              Clear           Moavars
.end patch 10.018
               Return
.......................................................................................................
Chkdate
               MOve              NO to FrcCompFlag             .reset flag
               MOVE      "NINVLAST" TO GNXTFLD
               CALL      GNXTKEY
               MOVE      GNXTNUM TO NINCHECK
               MOVE      NINCHECK,CHECK
               MOVE      INVNUM,HOLDINVNUM
               COMPARE   CHECK TO HOLDINVNUM
               GOTO      OLDINV IF LESS
               GOTO      OLDINV IF EQUAL
CK2
               MOVE      sysmo TO MM
               MOVE      sysday TO DD
               MOVE      sysyr TO YY
               CALL      CVTJUL
               move      juldays to jdate
               MOVE      invdtem TO MM
               MOVE      invdted TO DD
               MOVE      invdtey TO YY
               move      invdtec to cc
               CALL      CVTJUL
.               add       c7 to juldays             changed 5/5/95 per lp.
               add       "31" to juldays             .temporary  DH Jan 27 2011
               COMPARE   jdate TO JULDAYS
               GOTO      OLDINV IF LESS
               MOve      Yes to FrcCompFlag             .reset flag  within the allowed mod period dlh 14Sep05
               call      VerifyInvCode
               Move      c2 to mode
               setprop        INVSave,Height=20
               setprop        INVSave,enabled=1
               setprop        INVSaveQuit,Height=20
               setprop        INVSaveQuit,enabled=1
               setprop        INVOk,Height=0
               setprop        INVOk,enabled=0
               call           EnableAcdButtons
               return
OLDINV
               Beep
               Alert          Caution,"INVOICE FROM A PREVIOUS DAY, NOT ALLOWED! ",result
               Noreturn                      ;pop stack
               return
.......................................................................................................
LoadNinvAcdCombo003
              getitem                        InvEditText026,0,str4           .acd code
              Count                          n2,str4
              clear                          str3

              if                             (N2 = 0)
                             Return
               Elseif                        (N2 = 1)
               pack                          str3 with c0,c0,str4
               elseif                        (n2 = 2)
               pack                          str3 with c0,str4
               elseif                        (n2 = 3)
               pack                          str3 with str4
              endif

              Rep                            Zfill in str3
              packkey                        nacdfld from str3
              call                     nacdkey
              if                       not over
               Move                    nacdfld to n9
               add            c1 to n9
               setitem        InvComboBox003,0,n9
                             clear          str11
                             move           mask11 to str11
.                             if             (mrgsw = Yes | n2 != 0)
                              if             (mrgsw = Yes)                     . if no code should not even be here dlh 24aug05
                                             Move           Inpqty to qtyin     .dlh 24aug05
.argh 08242005 - where is qtyin why not loaded yet  --- argh used 586806 as test DLH
                                             move           qtyin to NINvAcdQty
.take care of minimums/old invoices that have 0 qtyin
                                             if        (qtyin < 5000)
                                             MOVE      QTYbild TO NINvAcdQty
                                             endif
                                             else
                                             move           QTYbild  to NINvAcdQty
                              endif
.
                             if        (nacdkey = "041" | nacdkey = "035")
                             MOVE          QTYbild TO NINvAcdQty
                             endif
               Move           c0 to N9
              Move            irexqty to N9
               IF             (n9 > 0 )
                              If             (nacdkey = "001" | nacdkey = "002" | nacdkey = "003")
                              move           qtybild to NInvAcdQty
                              endif
               endif
               move           mask11 to str11
               Move           c0 to N9
              If              (nacdkey = "152" & mrgsw = "Y" & chgrqty > 0)
              move            chgrqty to n9
              else
               Move           NInvAcdQty to N9
              endif
               edit           N9 to str11
Whatthe
               Setitem        InvEditText037,0,str11
              Setitem        InvEditText032,0,NacdType
              setfocus       InvEditText028
              else
.             Move                            "Invalid code" to str15
              setitem                         InvComboBox003,0,c0
              aLERT                           caution,"Invalid code",result
              setfocus         INvEditText026
              endif
               return
.......................................................................................................
.begin patch 10.15
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    move      "3000",str4
                    call      waitin using str4
.                    pause     "30"
                    noreturn
                   if        (trapcount > 60)   . 5 min are you kidding me. clearly not waiting 5 min
                    Pack       MailSubjct,"LoMemo print - ",str55
.                    Move      "CReques@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    "mailTo = ",mailbody
                    append    mailto,mailbody
                    append    CRLF,MailBOdy
                    append    "maiLFrom = ",mailbody
                    append    maiLFrom,mailbody
                    
                    append    CRLF,MailBOdy
                    append    str45,MailBody
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Clear     Mailto
                    Pack      MailTO,"CReques@nincal.com"
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    
                    endif
          
                    goto      checkfile
.end patch 10.15




.......................................................................................................
HelpGo
               setprop        AboutMssg,visible=1
               return

.......................................................................................................
FileGo
              branch          result to FileGo1,FileGo2,FileGo2
              return
.filego1 - report options for printing
FileGo1
              return
FileGo2
              winshow
.              goto            outofhere
              stop
.......................................................................................................
.timeout -- may need to add code to remove busy

Timeout
               stop
*==============================================================================
Optionsgo
               branch         result to SearchGo,OptionsGo1
OptionsGo1
              setprop         Options,visible=1
              return
OptionsWritePref
.Pull values from OptionsOrd.plf
.Screen       1
              getitem         OptionsBilledNo,0,N5
              move            N5,OptionsArray(1,1)
              getitem         OptionsCancNo,0,N5
              move            N5,OptionsArray(1,2)
              getitem         OptionsPassInit,0,N5
              move            N5,OptionsArray(1,3)
.Screen       5
.             getitem         OptionsScreen5BrkFilter,0,N1
.             move            N1,OptionsArray(5,1)
.Screen       6
.              getitem OptionsScreen6Init,0,N1
.              move            N1,OptionsArray(6,1)
.Screen       7
.             getitem         OptionsScreen7Proj,0,N1
.             move             N1,OptionsArray(7,1)
.Screen       8
.              getitem OptionsScreen8OrderSearch,0,N1
.              move            N1,OptionsArray(8,1)
.Screen9
              getitem         OptionsScreen9View,0,N1
              move            N1,OptionsArray(9,1)
.Screen10
              getitem         OptionsScreen10View,0,N1
              move            N1,OptionsArray(10,1)
              getitem         OptionsScreen10Usage,0,N1
              move            N1,OptionsArray(10,2)

.Write to the file
              pack            APIFileName,"c:\program files\nincal\NInv0001.pre",hexzero
              call            DeleteFile
              prep            preffile,"c:\program files\nincal\NInv0001.pre"
              move            C0,N9
              loop
               add            C1,N9
               move           C0,N8
               loop
                              add            C1,N8
                              write          preffile,seq;OptionsArray(N9,N8)
                              until          over
                              until (N8 = OptionsArr2)
               repeat
               until (N9 = OptionsArr1)
              repeat
              close           preffile
              return
OptionsTabClick
              if (N2 = C1)
               setprop        Options1Coll,visible=0
              elseif (N2 = C2)
.               setprop Options2Coll,visible=0
              elseif (N2 = C3)
.               setprop Options3Coll,visible=0
              elseif (N2 = C4)
.               setprop Options4Coll,visible=0
              elseif (N2 = C5)
               setprop        Options5Coll,visible=0
              elseif (N2 = C6)
.               setprop Options6Coll,visible=0
              elseif (N2 = C7)
               setprop Options7Coll,visible=0
              elseif (N2 = C8)
.               setprop Options8Coll,visible=0
              elseif (N2 = C9)
               setprop        Options9Coll,visible=0
              elseif (N2 = C10)
               setprop        Options10Coll,visible=0
              endif
              return

OptionsTabChange
              if (N2 = C1)
               setprop        Options1Coll,visible=1
              elseif (N2 = C2)
.               setprop Options2Coll,visible=1
              elseif (N2 = C3)
.               setprop Options3Coll,visible=1
              elseif (N2 = C4)
.               setprop Options4Coll,visible=1
              elseif (N2 = C5)
               setprop        Options5Coll,visible=1
              elseif (N2 = C6)
.               setprop Options6Coll,visible=1
              elseif (N2 = C7)
               setprop Options7Coll,visible=1
              elseif (N2 = C8)
.               setprop Options8Coll,visible=1
              elseif (N2 = C9)
               setprop        Options9Coll,visible=1
              elseif (N2 = C10)
               setprop        Options10Coll,visible=1
              endif
              return
...........................................................................................................
.Open Preferences File
openpref
              pack            APIFileName,"c:\program files\nincal\nInv0001.pre",hexzero
              call            FindFirstFile
              if (APIResult <> 0 & APIResult <> hexeight)
.               trap           Preferror if IO
               open           preffile,"c:\program files\nincal\NInv0001.pre"
               move           C0,N9
               loop
                              add            C1,N9
                              move           C0,N8
                              loop
                                             add            C1,N8
                                             read           preffile,seq;OptionsArray(N9,N8)
                                             until          over
                                             until (N8 = OptionsArr2)
                              repeat
                              until (N9 = OptionsArr1)
               repeat
               close          preffile
.               trapclr io
               move           C0,N9
.Screen1
               move           OptionsArray(1,1),str5
               call           Trim using str5
               move           str5,N5
               setitem        OptionsBilledNo,0,N5
.
               move           OptionsArray(1,2),str5
               call           Trim using str5
               move           str5,N5
               setitem        OptionsCancNo,0,N5
.
               move           OptionsArray(1,3),str5
               call           Trim using str5
               move           str5,N5
               setitem        OptionsPassInit,0,N5
.              move           OptionsArray(1,2),str5
.              call           Trim using str5
.              move           str5,N5
.              setitem        OptionsFaxLONo,0,N5
.Screen       5
.              move           OptionsArray(5,1),str5
.              call           Trim using str5
.              move           str5,N5
.              setitem        OptionsScreen5BrkFilter,0,N5
.
.Screen       6
.               move           OptionsArray(6,1),str5
.               call           Trim using str5
.               move           str5,N5
.               setitem        OptionsScreen6Init,0,N5
.START PATCH 3.66 ADDED LOGIC
.Screen       7
.              move            OptionsArray(7,1),str5
.              call            Trim using str5
.              move            str5,N5
.Screen       8
.               move           OptionsArray(8,1),str5
.               call           Trim using str5
.               move           str5,N5
.               setitem        OptionsScreen8OrderSearch,0,N5
.Screen9
.              move           OptionsArray(9,1),str5
.              call           Trim using str5
.              move           str5,N5
.              setitem        OptionsScreen9View,0,N5
.Screen10
.              move           OptionsArray(10,1),str5
.              call           Trim using str5
.              move           str5,N5
.              setitem        OptionsScreen10View,0,N5
.
.              move           OptionsArray(10,2),str5
.              call           Trim using str5
.              move           str5,N5
.              setitem        OptionsScreen10Usage,0,N5
              endif
               return
............................................................................................................
SearchGo
               branch         result to SearchGo1,SearchGo2,SearchGo3,SearchGo4,SearchGo5,SearchGo6
SearchGo1
.BROKER
              move            C1,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
               return
SearchGo2
.LIST
               move           C2,SrchFlag
               call           SearchSetTitle
               call           SearchSetVisible
               return
SearchGo3
.MAILER
              move            C3,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchGo4
.SHIP-TO
. not an option with this program
               return
SearchGo5
.CAMPAIGN
              move            C5,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return

.START PATCH 3.66 ADDED LOGIC
SearchGo6
.OWNER
              move            C6,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchLoad
               branch         SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6
SearchLoad1
.BROKER

               unpack         Srchstr,str4,str1,str3,str1,str45,str55
              setitem       InvEditBrk,0,str4
               setitem        InvStatText036,0,str45
              setfocus      InvEditBRk
               return
SearchLoad2
              unpack          Srchstr,str6
              setitem         InvEditText033,0,str6
              unpack          Srchstr,str6,str1,str35
              setitem         InvEditText034,0,str35
              setfocus       InvEditText034
               return
SearchLoad3
              unpack          Srchstr,str4
              setitem         InvEditMlr,0,str4
               setitem        InvStatText035,0,str45
               Setfocus       InvEditMlr
               return
SearchLoad4
.SHIP-TO - not an option with this program
               return
SearchLoad5
.Campaign - not an option with this program
               return
SearchLoad6
.OWNER
              setitem         InvEditOwner,0,str4
              setitem         InvStatText037,0,str25
              setfocus       InvEditOwner
               return
.end patch 10.0
*******************************************************************************
RestoreSearchButton
              Setprop InvoiceViewSearch,height=20
               Return
*******************************************************************************
InvoiceSetSearchKey
              setitem       InvEditTextLR,0,OLRN
              return
*******************************************************************************
InvoiceSetSearchDefault
.Position Search Form in Default Position
.               getprop        InvoiceSearch,top=T1,left=L1
              add             "52",L1
              setprop         SearchForm,winpos=1
              getprop         NINV0001,top=H,left=V
              add            T1,H,SerTop
              add            "50",SerTop          .Compensate for Menu Bar/Title Bar + some to allow second click to make invisible
              add            L1,V,SerLeft
              setprop         SearchForm,top=SerTop,left=SerLeft                          .Default
              return
XRESIZE
           Ninv0001.Scale
           RETURN
.........................................................................................................................
.START PATCH 10.05 ADDED LOGIC
InvUpdateTypistTable LROUTINE FrmPtr
.Routine Modifies Typist table when records are updated.  Called by Modify button
.FrmPtr  = "1" - New record, "2" - Update
.START PATCH 10.7 ADDED LOGIC
.        pack    Typdate from INVDTEc,INVDTEy,INVDTEM
        clear   NTYPDET
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM
        pack    Typdate,CC,YY,MM
.END PATCH 10.7 ADDED LOGIC
        packkey NTYPFLD from TypDate,Inits
        move    Inits,NtypType
        call    NTYPTST
        if over
                        if (FrmPtr = 1)
                                move    C1,INVCOUNT
                        elseif (FrmPtr = 2)
                                move    C1,INVRCNT
                        else
                                goto INVUpdateTypistTableEnd
                        endif
                call    NTYPWRT
        else
                call    NTYPKEY
                        if (FrmPtr = 1)
                                add    C1,INVCOUNT
                        elseif (FrmPtr = 2)
                                add    C1,INVRCNT
                        else
                                goto INVUpdateTypistTableEnd
                        endif
                call    NTYPUPD
.START PATCH 10.7 ADDED LOGIC
        endif
        clear   NTYPDET
        unpack  timestamp,CC,YY,MM
        pack    Typdate,CC,YY,MM
.END PATCH 10.7 ADDED LOGIC
.Update  Typist Totals value...
        packkey NTYPFLD from Typdate,"99 "
        move    "99 " to Ntyptype
        call    NTYPTST
.START PATCH 10.7 REPLACED LOGIC        
.        GOTO NONINE IF OVER
        if over
                call NONINE using FrmPtr
                goto InvUpdateTypistTableEnd
        endif
.END PATCH 10.7 REPLACED LOGIC
        packkey NTYPFLD from Typdate,"99 "
        move    "99 " to Ntyptype
        call    NTYPKEY
                if (FrmPtr = 1)
                        add    C1,INVCOUNT
                elseif (FrmPtr = 2)
                        add    C1,INVRCNT
                else
                        goto INVUpdateTypistTableEnd
                endif
        call    NTYPUPD
.START PATCH 10.7 REMOVED LOGIC
.        endif
.END PATCH 10.7 REMOVED LOGIC
InvUpdateTypistTableEnd
        return
.START PATCH 10.7 REPLACED LOGIC
.NONINE
.          Move      "Ninv0001 - output",MailSubjct
.          Move      "DavidHerrick@nincal.com",MailFrom
.          Move      "DavidHerrick@nincal.com",MailTo
.          Clear     MailBody
.          Append    " Typist",MailBody
.          Append    CRLF,MailBOdy
.          Append    "No 99 record found !   key =     ",Mailbody
.          Append    NTypFld,Mailbody
.          Append    CRLF,MailBOdy
.          Reset     Mailbody  
.          Call      SendMail              
.              packkey              Ntypfld from Typdate,"99 "
.              Move            "99 " to Ntyptype
.              Move            c1,pndcount      
.              CALL            NTYPWRT            
.               return
.

NONINE LROUTINE FrmPtr
.Routine ONLY creates New '99' records...
.FrmPtr  = "1" - New record, "2" - Update
          Move      "Ninv0001 - output",MailSubjct
          Move      "DavidHerrick@nincal.com",MailFrom
          Move      "DavidHerrick@nincal.com",MailTo
          Clear     MailBody
          Append    " Typist",MailBody
          Append    CRLF,MailBOdy
          Append    "No 99 record found !   key =     ",Mailbody
          Append    NTypFld,Mailbody
          Append    CRLF,MailBOdy
          Reset     Mailbody  
          Call      SendMail              
.
        clear   NTYPDET
        unpack  timestamp,CC,YY,MM
        pack    Typdate,CC,YY,MM
        packkey Ntypfld from Typdate,"99 "
        Move    "99 " to Ntyptype
        if (FrmPtr = 1)
                move    C1,INVCOUNT
        elseif (FrmPtr = 2)
                move    C1,INVRCNT
        else
                goto NONINEEnd
        endif
        CALL    NTYPWRT            
NONINEEnd
        return
.END PATCH 10.7 REPLACED LOGIC
.END PATCH 10.05 ADDED LOGIC

......I/O INCLUDES
               INCLUDE        NONOIO.inc
               include        compute.inc
               include        nofrio.inc
               INCLUDE        NOWNIO.inc
               INCLUDE        NSHPIO.inc
               INCLUDE        NCRCIO.inc
               INCLUDE        NBILIO.inc
               INCLUDE        NORDIO.INC
               INCLUDE        NSPEIO.inc
               INCLUDE        NSPIIO.inc
               INCLUDE        NDATIO.inc
               INCLUDE        NPASIO.inc
               INCLUDE        NRTNIO.inc
               INCLUDE        NACDIO.inc
               INCLUDE        NPAYIO.inc
               INCLUDE        NADJIO.inc
               INCLUDE        NDAT3IO.inc
               INCLUDE        ninvio.inc
               INCLUDE        NMRGIO.inc
               INCLUDE        GNXTIO.INC
               include        nuseio.inc
               include        nmoaio.inc
               include        nmobio.inc
               include        mlrhelp.inc
               include        brkhelp.inc
               include        tinvio.inc
.begin patch 10.5
          INCLUDE   NTYPIO.INC
.enD patch 10.5
.START PATCH        10.021              REMOVED LOGIC
.               INCLUDE        DISPORD.INC
.               include        nonocode.inc
.END PATCH          10.021              REMOVED LOGIC
               include        hpio.inc
              include         compio.inc
               INCLUDE        NCNTIO.inc
               INCLUDE        CNTIO.inc
.START PATCH 10.021.1 REMOVED LOGIC
.               include        nfulio.inc
.END PATCH 10.021.1 REMOVED LOGIC
               include        Nmodio.inc
               Include        Nsel2io.inc

.begin patch 10.0
               include        ncmpio.inc                                   ;campaign
               include        searchio.inc      .contains logic for search.plf
               Include        NInvAcdIO.inc
.end patch 10.0
               INCLUDE        COMLOGIC.inc
