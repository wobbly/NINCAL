PC       EQU       1
              INCLUDE         COMMON.INC
              INCLUDE         CONS.INC
              INCLUDE         NSHPDD.inc
              INCLUDE         NSHP2DD.INC
              INCLUDE         NORDDD.INC
              INCLUDE         NCMPDD.INC
.Patch5.1.4
              INCLUDE         COMPDD.INC
              INCLUDE         CNTDD.INC
.             INCLUDE         NMLRDD.INC
.Patch5.1.4
              INCLUDE         NOWNDD.INC
              INCLUDE         NRTNDD.INC
              INCLUDE         SHIPPING.INC
              INCLUDE         NOFRDD.INC
              include         ncntdd.inc
              include         winapi.inc
.;begin patch 5.2
.;             include         ninvdd.inc
              include         ninvdd.inc
.;end patch 5.2
.START PATCH 5.3 REMOVED LOGIC
.               INCLUDE NFULDD.INC
.END PATCH 5.3 REMOVED LOGIC
                include         nusedd.inc
              include         npasdd.inc
.;Patch5.1.4
.             include         nbrkdd.inc     .Needed to load Search.plf
.;Patch5.1.4
              include         ndatdd.inc     .Needed to load Search.plf
.START PATCH 5.1.3A ADDED LOGIC
              INCLUDE         NSEL2DD.INC
.END PATCH 5.1.3A ADDED LOGIC
.Patch 5.4.4
                    Include       nshp3dd.inc
.Patch 5.4.4
Release  init      "5.4.8"          DLH      Patch for 64 bit windows
Reldate     init        "21 August 2011"
.Release  init      "5.4.7"          DLH      added PDMG and PLF
.Reldate     init        "19 January 2010"
.Release   init      "5.4.6"          DLH      Move batch server
.Reldate     init        "06 April 2009"
.Release  init      "5.4.5"          DLH      added MMI cleaned up missing code for frontline, etc fixed PLF
.Reldate     init        "14 January 2009"
.Release       init            "5.4.4"          DMB           Add nshp3dd/io fields to forms nshp001a/001b - new inventory fields ninshp3.dat
.Reldate       init            "October 30, 2006"
.Release       init            "5.4.3"          DMB           Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.Reldate       init            "October 12, 2006"
.release       init            "5.4.2"          DMB           Added PIDI as possible shipping info provider code "Z"
.reldate       init            "September 12, 2006"
.release       init            "5.4.1"          DMB           Added frontline as possible shipping info provider code "F"
.reldate       init            "August 16, 2006"
.release       init            "5.4"          DLH            SendMail
.reldate       init            "August 14, 2006"
.release       init            "5.3"          DMS            22June2006     Fulfillment Conversion
.release       init            "5.2"          DLH            02March2005    INvoice Conversion
.reldate       init            "Sept 6, 2005"
.release      init            "5.1.7"        DMB            01JUN2005      Added Target Analysis SCODE - A
.release      init            "5.1.6"        DLH            23May2005      Added Dmexchange
.reldate      init            "May 23, 2005"
.release      init            "5.1.5"        DMB            01DEC2004      Mailer Conversion
.release      init            "5.1.4"        DMB            26MAY2004      Mailer Conversion
.release      init            "5.1.3A"       ASH            29JAN2004  DATACARD CONVERSION
.reldate      init            "JANUARY 29, 2004"
.release      init            "5.1.3"        DLH            blocked adding or modifying record for billed order (again)
.;                                                           code is under bottons Nshp001a.plf
.reldate      init            "March 11, 2004"
.release      init            "5.1.2"        ASH            08SEP2003 Small Patch
.reldate      init            "September 8, 2003"
.release      init            "5.1.1"        ASH            28AUG2003 Small Patches
.reldate      init            "August 28, 2003"
.release      init            "5.1"          ASH            15JUL2003 ALLOW FULL STAFF TO VIEW SECOND TAB PAGE
.reldate      init            "July 15, 2003"
.release      init            "5.0"          ASH            24APR2003 REWRITE OF SHIPPING PROGRAM
.reldate      init            "April 24, 2003"

.EXTERNAL ROUTINES FROM       SPELLCHECK.PLC
SpellCheck external "SPELLCHECK;SpellCheck"

prfile        pfile
.tempfile file
DIFF          FORM            10
fivePER       FORM            10
last          form            3
NewFlag       init            "N"
NewFlag2 init "N"             .Used to determine which buttons are enabled.
NewRFlag init "N"             .Used to determine if Record Date needs to be updated.
ReturnFlag init               "N"
ExitFlag init "Y"
ExitFlag2 init "Y"
StopFlag init "N"
Ship2Flag form "0"            .used to determine if Save routine was completed when creating NINSHP en masse on Screen 2
SecFlag       init            "N"
MaskNotes dim 500
MaskNotesB dim 500
.START PATCH 5.1 REPLACED LOGIC
.revtyps      init            "DH JD AH DM DB SM MM KS "
revtyps       init            "DH JD AH DM DB "
.END PATCH 5.1 REPLACED LOGIC
.Vars used for Report Screen
RptCan        dim             1
PrintFlag form "01"           .used to determine which Printer is used, default to Laser 3
RptType       form            "001"
.hexeight integer 4,"4294967295"
ComboBoxes    ComboBox (2)
StatTextBoxes StatText (2)
ObjectColl    Collection

LOGFILE       IFILE           KEYLEN=6,VAR=498,COMP
.BCNAME       DIM             25                            COL 24-48 COMPANY NAME
....MISC. ITEMS....................................
.DATE         DIM             6
ZERO          FORM            "00"
SALES         DIM             2
JDATE         FORM            5
MAILDATE FORM 5
KMAILERP DIM  7
AKEY1         DIM             3

.Patch 5.4.4 Start 
SHPINVHOLD          DIM 336
shp3wrtflg          form 1
.Patch 5.4.4 End
.Colors
white         color
grey          color
RED           COLOR
BLACK         COLOR
blue          color
.Define Fonts to be used
font1         font
font2         font
font3         font
font4         font
font5         font
.Set Up Menu Bar
mFile         menu
mEdit         menu
mOptions menu
mSecurity menu
mHelp         menu
.Set Up       SubMenu         for Options
sSearch       submenu

.Present Data for Menu Bar
FData         init            "&File;&Print;Pre&view;-;E&xit"
EData         init            "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData         init            "&Options;&Search"
SEData        init            "Security;FIXREC"
HData         init            "&Help;&About"
.Present Data for SubMenu
SData         init            ";&Broker;&List;&Mailer;&Ship-To;&Campaign;&Owner"
.
endindex form 9
userlogn dim  7
userlogw dim  7
Timer         Timer
hold          dim             500
holdsInfo dim 36
HoldMdate Form 5
str100        Dim             100
str250        Dim             250
recipient dim 100
EmailMesg dim 2500                  250 * 10
startfp       form            4
endfp         Form            4
DimPtr        dim             ^
FrmPtr        form            ^
TabNum        form   "01"
.
VT_BOOL       EQU 11
OTRUE         variant
OFALSE        variant
OBOOL         variant
IntIndex integer 4
IntIndex2 integer 4
ColHeads automation
ColHead       automation
ListIts       automation
ListIt        automation
SubIt         automation
.
SrchCmp       dim             6              .Valid Search Field
SrchMlr       dim             4              .Valid Search Field
SrchLO        dim             4              .Valid Search Field
SrchMP        dim             12             .Valid Search Field
SrchCnt       dim             2              .Valid Search Field
SrchFrRet dim 8               .Filter Field
SrchFrRet2 dim 8              .Filter Field Flag
SrchToRet dim 8               .Filter Field
SrchFrMl dim  8               .Filter Field
SrchFrMl2 dim 8               .Filter Field Flag
SrchToMl dim  8               .Filter Field
SrchLCR       form            1              .Filter Field
Sapi          Automation                               //Define the ActiveX control
str500        dim 500
.............................................................................................................
              move            "NSHP0001.PLS",Wprognme
              move            "Shipping File Maintenance",Wfunction
              move            "David Herrick",Wauthor
              move            Release,Wrelease
              move            Reldate,Wreldate
.Declare forms, Always declare child forms first
rpt2          plform  Report2
sch           plform          Search
pass          plform          Passwrd
mss1          plform          Error
abt           plform          About
x             plform          NShp0001
Nshp001a plform Nshp001a
Nshp001b plform Nshp001b
              winhide
.Load Forms, Always load parent form first
              formload x
              formload Nshp001a,NShp0001
              formload Nshp001b,NShp0001
              formload abt
              formload pass
              formload mss1
              formload sch
              formload rpt2

              CREATE          TIMER,18000     .30 minutes
              ACTIVATE TIMER,Timeout,RESULT
.Create Menus
              create          NShp0001;MFile,FData
              create          NShp0001;mEdit,EData,mFile
              create          NShp0001;mOptions,OData,mEdit
              reset           revtyps
              scan            INITS,revtyps
              if equal
               create         NShp0001;mSecurity,SEData,mOptions
               create         NShp0001;mHelp,HData,mSecurity
              else
               create         NShp0001;MHelp,HData,mOptions
              endif
.Create       SubMenu
              create          NShp0001;sSearch,SData,mOptions,1
              Create  Sapi,class="Sapi.SpVoice"         //Create the ActiveX control

.Activate Menus
.FileGo leads to stop
              activate mFile,FileGo,result
.Need this when               it works
              activate mEdit,EditGo,result
.Only a       SubMenu         under this one
              activate mOptions,OptionsGo,result
              reset           revtyps
              scan            INITS,revtyps
              if equal
               activate mSecurity,SecurityGo,result
              endif
              activate mHelp,HelpGo,result
..Activate SubMenus
              activate sSearch,SearchGo,result

.Activate SubMenus
              activate sSearch,SearchGo,result

.
.Create fonts to be used
              create          font1,"Arial",size=12,bold
              create          font2,"Arial",size=8
              create          font3,"Helvetica",size=9
              create          font4,"Arial",size=14,italic
.Create Colors for EditText Inquiry
              create          white=*white
              create          grey=220:220:220
              create          RED=*RED
              create          black=*black
              create          blue=*blue
.
.Create ShipListView Columns
.Column Clicking
.Check out notes under Order3ListView_ColumnClick for other options.
              ShipListView.InsertColumn using "LR",50,1
              ShipListView.InsertColumn using "Mailer P.O.",80,2
              ShipListView.InsertColumn using "Mailer",50,3
              if (security = 8)
               ShipListView.InsertColumn using "",0,4
               ShipListView.InsertColumn using "Broker",50,5
               ShipListView.InsertColumn using "Mailer Key",80,6
               ShipListView.InsertColumn using "Other Detail",0,7
.              ShipListView.InsertColumn using "Broker",50,4
.              ShipListView.InsertColumn using "Mailer Key",80,5
.              ShipListView.InsertColumn using "Other Detail",100,6
               setprop        ShipEditSearchPO,Tooltip="Maildate required for PO search MMDDYYYY"
              else
.Load Contact ComboBox
               move           C1,N2
               move           "  ",str45
.Must delete blank items entered to ensure adequate space
               deleteitem Ship2ComboContactSearch,0
               insertitem Ship2ComboContactSearch,N2,str45
               loop
                              move           C1,NCNTPATH
                              move           "Load-NCNTSEQ",Location
                              call           NCNTSEQ
                              until over
                              if (CNTCNT = "1")
                                             pack           str45,CNTNAME,B1,CNTNUM
                                             add            C1,N2
                                             insertitem Ship2ComboContactSearch,N2,str45
                              endif
               repeat
               setitem        Ship2ComboContactSearch,0,1
.
               ShipListView.InsertColumn using "List",55,4
               ShipListView.InsertColumn using "Broker",50,5
               ShipListView.InsertColumn using "Mailer Key",80,6
               ShipListView.InsertColumn using "Other Detail",0,7
.
               getprop        Ship2ListView,*ColumnHeaders=ColHeads
.I hide       the first item as I have not yet figured out if              I can change the ForeColor of that item, since it does not appear to be    a sub-item.
               ColHeads.Add using *Index=1,*Key="key",*Text="",*Width=0
               ColHeads.Add using *Index=2,*Key="one",*Text="LR ##",*Width=50
               ColHeads.Add using *Index=3,*Key="two",*Text="Status",*Width=50
               ColHeads.Add using *Index=4,*Key="three",*Text="Ship Qty",*Width=70,*Alignment=1
               ColHeads.Add using *Index=5,*Key="four",*Text="Ship Date",*Width=70
               ColHeads.Add using *Index=6,*Key="five",*Text="Info",*Width=55
               ColHeads.Add using *Index=7,*Key="six",*Text="Tracking",*Width=55
               ColHeads.Add using *Index=8,*Key="seven",*Text="List ##",*Width=55
               ColHeads.Add using *Index=9,*Key="eight",*Text="Owner",*Width=55
               ColHeads.Add using *Index=10,*Key="nine",*Text="Mailer",*Width=55
               ColHeads.Add using *Index=11,*Key="ten",*Text="Quantity",*Width=70,*Alignment=1
               ColHeads.Add using *Index=12,*Key="eleven",*Text="Mail Date",*Width=70
               ColHeads.Add using *Index=13,*Key="twelve",*Text="Ret. Date",*Width=70
               ColHeads.Add using *Index=14,*Key="thirteen",*Text="Camp.",*Width=55
               ColHeads.Add using *Index=15,*Key="fourteen",*Text="Contact",*Width=70
               ColHeads.Add using *Index=16,*Key="fifteen",*Text="MP",*Width=70
               ColHeads.Add using *Index=17,*Key="sixteen",*Text="Other Detail",*Width=0
               ColHeads.Add using *Index=18,*Key="seventeen",*Text="Other Detail2",*Width=0
.Patch 5.4.4
               ColHeads.Add using *Index=19,*Key="nineteen",*Text="Other Detail3",*Width=0
.Patch 5.4.4
.Set some properties for ListView object
               create         OTRUE,VarType=VT_BOOL,VarValue=1
               create         OFALSE,VarType=VT_BOOL,VarValue=0
               create         OBOOL,VarType=VT_BOOL,VarValue=0
.
               setprop        Ship2ListView,*HideColumnHeaders=OFALSE
               setprop        Ship2ListView,*HideSelection=OFALSE
               setprop        Ship2ListView,*FullRowSelect=OTRUE
               setprop        Ship2ListView,*MultiSelect=OTRUE
               setprop        Ship2ListView,*Sorted=OTRUE
               setprop        Ship2ListView,*SortOrder=1
               setprop        Ship2ListView,*SortKey=1
               setprop        Ship2ListView,*AllowColumnReorder=OTRUE
               setprop        Ship2ListView,*LabelEdit=1
               setprop        Ship2ListView,*View=3
               setprop        Ship2ListView,*Font=font2
               setprop        Ship2ListView.Font,*Bold=OFALSE
.
               getprop        Ship2ListView,*ListItems=ListIts
              endif
.START PATCH 5.1 REMOVED LOGIC
.             reset           revtyps
.             scan            INITS,revtyps
.             if not equal
.              setprop        ShipPrint,height=0
.             endif
.END PATCH 5.1 REMOVED LOGIC
.
.Set Error Message Stat Text Boxes
              call            SetShipErrorMssgDefault
              OPEN            LOGFILE,"SHIPfax"
.
              call            ShipDisableLower
              setprop ShipComboShip,enabled=0,bgcolor=grey
.
              setfocus ShipEditSearchLR
              if (security = 8)
               setprop        ShipTabControl,height=0,enabled=0            .This is very important!!!!
               setitem        ShipStatOwnerComp,0,""
               setitem        ShipStatOwnerCnt,0,""
               setitem        ShipStatCC2,0,""
               setitem        ShipStatMlrComp,0,"Search by PO requires Maildate"
               setitem        ShipStatMlrCnt,0,"in format MMDDYYYY"
               setprop        ShipStatMlrComp,fgcolor=blue
               setprop        ShipStatMlrCnt,fgcolor=blue
               setprop        ShipStatMlrComp,Font="'>MS Sans Serif'(8)"
               setprop        ShipStatMlrCnt,Font="'>MS Sans Serif'(8)"
               setprop        ShipStatMlr,height=0
               setprop        ShipStatMlr2,height=0
               setprop        ShipStatOwner,height=0
               setprop        ShipStatOwner2,height=0
               setprop        ShipstatOwnercnt,height=0
               setprop        ShipstatOwnerComp,height=0
               setprop        ShipStatCC,height=0
               setprop        ShipStatCC2,height=0
               setprop        ShipStatListName,height=0
               setprop        ShipStatListSel2,height=0
               setprop        ShipEditSearchDate,height=1,Enabled=1
               setprop        ShipStatList,height=0
               setprop        ShipStatListSel,height=0
               setprop        ShipStatNotes,height=0
               setprop        ShipEditNotes,height=0
               setprop        ShipEditNotes2,height=0
.
               setprop        ShipDelete,enabled=0,height=0,tabid=0
               setprop        ShipNew,enabled=0,height=0,tabid=0
               setprop        ShipSave,enabled=0,height=0,tabid=0
               setprop        ShipModify,enabled=0,height=0,tabid=0
               setprop        ShipQuit,enabled=0,height=0,tabid=0
               setprop        ShipPrint,enabled=0,height=0,tabid=0
.
               setprop        ShipEmail,visible=1,enabled=1,height=20,tabid=910
               setprop        ShipEmailSend,visible=1,enabled=0,height=20,tabid=940
               setprop        ShipEmailQuit,visible=1,enabled=0,height=20,tabid=950
               setprop        ShipEditFrom,Enabled=1,height=20,tabid=920
               setprop        ShipEditMessage,Enabled=1,height=80,tabid=930
               setprop        ShipStatFrom,height=20
               setprop        ShipStatMessage,height=20
              else
.Hide all those objects related to Emails from Web version
               setprop        ShipEmail,visible=0,enabled=1,height=0,tabid=0
               setprop        ShipEmailSend,visible=0,enabled=0,height=0,tabid=0
               setprop        ShipEmailQuit,visible=0,enabled=0,height=0,tabid=0
               setprop        ShipEditFrom,Enabled=0,height=0,tabid=0
               setprop        ShipEditMessage,Enabled=0,height=0,tabid=0
               setprop        ShipStatFrom,height=0
               setprop        ShipStatMessage,height=0
              endif
.Set up some flags
              move            C1,NORDPATH
              move            C1,NCMPPATH
.Main Loop
.Set tab index
              clock           timestamp,timestamp
              unpack          timestamp,CC,YY,MM,DD
              move            C0,NUSEFLD
              move            C1,NUSEPATH
              move            PORTN,NUSEFLD
              rep             zfill,NUSEFLD
              call            NUSEKEY
.              goto userng if over
              scan            "INVALID",NUSEUSER
.              goto userng if equal
              reset           NUSEUSER
.Find out system information
              call            GetWinVer
              call            Trim using NUSEUSER
              scan            "BILLING",NUSEUSER
              if not equal
               move           NUSEUSER,str1
               loop
                              bump           NUSEUSER,1
                              cmatch         B1,NUSEUSER
                              until equal
                              until eos
               repeat
               if not eos
                              bump           NUSEUSER,1
                              move           NUSEUSER,str6
                              clear          userlogn
                              pack           userlogn,str1,str6
               endif
              endif
              reset           NUSEUSER
.
           EVENTREG  X, 17, XRESIZE


              loop
               waitevent
               setitem        timer,0,18000   .reset to 30 minutes
              repeat
Timeout
              beep
              beep
              beep
              winshow
              if (security = 8)
               chain          "masterweb"
              endif
              stop

FileGo
              if (ExitFlag <> YES & ExitFlag2 <> YES)
               return
              endif
              winshow
              if (security = 8)
               chain          "masterweb"
              endif
              stop
              return
EditGo
              return
OptionsGo
              return
SearchGo
              branch          result to SearchGo1,SearchGo2,SearchGo3,SearchGo4,SearchGo5,SearchGo6
SearchGo1
.BROKER
              move            C1,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchGo2
.LIST
              move            C2,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchGo3
.MAILER
              move            C3,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchGo4
.SHIP-TO
              move            C4,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchGo5
.CAMPAIGN
              move            C5,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchGo6
.OWNER
              move            C6,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchLoad
.Called       by SearchDataList_DoubleClick
.Only load if in modify       mode
              getprop         Ship2OK,enabled=howmany
              branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6
SearchLoad1
.BROKER
              return
SearchLoad2
.LIST
              return
SearchLoad3
.MAILER
              if (TabNum = 2)
               unpack         Srchstr,str4
               setitem        Ship2EditMlrSearch,0,str4
               setfocus Ship2EditMlrSearch
              endif
              return
SearchLoad4
.SHIP-TO
              return
SearchLoad5
.CAMPAIGN
              if (TabNum = C2)
               unpack         Srchstr,str6
               setitem        Ship2EditCampSearch,0,str6
               setfocus Ship2EditCampSearch
              endif
              return
SearchLoad6
.OWNER
              if (TabNum = C2)
               unpack         Srchstr,str4
               setitem        Ship2EditLOSearch,0,str4
               setfocus Ship2EditLOSearch
              endif
              return

HelpGo
              setprop AboutMssg,visible=1
              return

SecurityGo
              branch          result to SecurityGo1
SecurityGo1
              move            "(",progcode
              move            NO,SecFlag
              pack            str55,"                        To Enter FIXREC mode"
              setitem         PasswordStatMssg1,0,str55
              setprop         PasswordStatMssg1,visible=1
              setitem         PasswordEdit,0,""
              setfocus PasswordEdit
              clear           NPASFLD
              setprop         Passwrd,visible=1
              if (NPASFLD <> "(COSMO")
               return
              endif
              alert           note,"FIXREC mode permitted!",result
              move            YES,SecFlag
              call            Click_ShipModify
              call            ShipEnableLowerSecurity
              return

ShipEnableLower
              setprop ShipEditInfo,enabled=1,bgcolor=white
              setprop ShipEditShipQty,enabled=1,bgcolor=white
              setprop ShipEditShipDate,enabled=1,bgcolor=white
              setprop ShipEditTrack,enabled=1,bgcolor=white
              setprop ShipComboCode,enabled=1,bgcolor=white
              setprop ShipEditPrice,enabled=1,bgcolor=white
              setprop ShipEditNotes,readonly=0,bgcolor=white
              setprop ShipEditNotes2,readonly=0,bgcolor=white
              
.Patch 5.4.4              
              setprop ShipEditTextLolRecdQty,enabled=1,bgcolor=white
              setprop ShipEditTextLolRecDate,enabled=1,bgcolor=white    
              setprop ShipEditTextLOLComments,enabled=1,bgcolor=white                 
.Patch 5.4.4              
              return

ShipEnableLowerSecurity
              setprop ShipEditRecDate,enabled=1,bgcolor=white
              setprop ShipEditRecInit,enabled=1,bgcolor=white
              setprop ShipEditPrintDate,enabled=1,bgcolor=white
              setprop ShipEditPrintInit,enabled=1,bgcolor=white
              return

ShipDisableLower
              move            NO,SecFlag
              setprop ShipEditInfo,enabled=0,bgcolor=grey
              setprop ShipEditShipQty,enabled=0,bgcolor=grey
              setprop ShipEditShipDate,enabled=0,bgcolor=grey
              setprop ShipEditTrack,enabled=0,bgcolor=grey
              setprop ShipComboCode,enabled=0,bgcolor=grey
              setprop ShipEditPrice,enabled=0,bgcolor=grey
              setprop ShipEditRecDate,enabled=0,bgcolor=grey
              setprop ShipEditRecInit,enabled=0,bgcolor=grey
              setprop ShipEditPrintDate,enabled=0,bgcolor=grey
              setprop ShipEditPrintInit,enabled=0,bgcolor=grey
              setprop ShipEditNotes,readonly=1,bgcolor=grey
              setprop ShipEditNotes2,readonly=1,bgcolor=grey
.Patch 5.4.4              
              setprop ShipEditTextLolRecdQty,enabled=0,bgcolor=grey
              setprop ShipEditTextLolRecDate,enabled=0,bgcolor=grey     
              setprop ShipEditTextLOLComments,enabled=0,bgcolor=grey                  
.Patch 5.4.4
              
              
              
              return

Ship2EnableLowerAll
              setprop         Ship2Process,enabled=1
              setprop         Ship2Delete,enabled=1
              setprop         Ship2Print,enabled=1
Ship2EnableLower
              setprop         Ship2Stop,enabled=0
              setprop         Ship2OK,enabled=1
              setprop         Ship2ListView,*enabled=1
              move            YES,ExitFlag2
              move            NO,StopFlag
              return

Ship2DisableLowerAll
              setprop         Ship2OK,enabled=0
              setprop         Ship2ListView,*enabled=0
Ship2DisableLower
              move            "N",ExitFlag2
              setprop         Ship2Process,enabled=0
              setprop         Ship2Delete,enabled=0
              setprop         Ship2Print,enabled=0
              return

ShipClearUpper
              setitem         ShipStatLR,0,""
              setitem         ShipStatOrderStat,0,""
              setitem         ShipStatOffer2,0,""
              setitem         ShipStatMlrComp,0,""
              setitem         ShipStatMlrCnt,0,""
              setitem         ShipStatOwnerComp,0,""
              setitem         ShipStatOwnerCnt,0,""
              setitem         ShipStatCC2,0,""
              setitem         ShipStatRtnComp,0,""
              setitem         ShipStatRtnCnt,0,""
              setitem         ShipStatListName,0,""
              setitem         ShipStatListSel2,0,""
              setitem         ShipStatOrderDate2,0,""
              setitem         ShipStatRtnDate2,0,""
              setitem         ShipStatMailDate2,0,""
              setitem         ShipStatOrderQty2,0,""
              setitem         ShipStatPO2,0,""
              setitem         ShipStatKey2,0,""
              setitem         ShipStatRec,0,""
              setitem         ShipComboShip,0,1
              return

ShipClearLower
              setitem ShipEditInfo,0,""
              setitem ShipEditShipQty,0,""
              setitem ShipEditPrice,0,""
              setitem ShipEditShipDate,0,""
              setitem ShipEditTrack,0,""
              setitem ShipEditRecDate,0,""
              setitem ShipEditRecInit,0,""
              setitem ShipEditPrintDate,0,""
              setitem ShipEditPrintInit,0,""
              setitem ShipEditNotes,0,""
              setitem ShipEditNotes2,0,""
              setitem ShipComboCode,0,C0
              return
              
.Patch 5.4.4              
ShipClearLowerInventory
              setitem ShipEditTextLolRecdQty,0,""
              setitem ShipEditTextLolRecDate,0,""      
              setitem ShipEditTextLOLComments,0,""                   
                return
.Patch 5.4.4
Ship2ClearUpper
              setitem Ship2StatLRNumber,0,""
              setitem Ship2StatStatusName,0,""
              setitem Ship2StatCampaignName,0,""
              setitem Ship2StatListName,0,""
              setitem Ship2StatOwnerName,0,""
              setitem Ship2StatMailerName,0,""
              setitem Ship2StatOQtyName,0,""
              setitem Ship2StatMPName,0,""
              setitem Ship2StatMDateName,0,""
              setitem Ship2StatRDateName,0,""
              setitem Ship2StatContactName,0,""
              return

Ship2ClearLower
              setitem Ship2StatShipInfoName,0,""
              setitem Ship2StatQtyName,0,""
              setitem Ship2StatDateName,0,""
              setitem Ship2StatTrackingName,0,""
              setitem Ship2StatRDateName,0,""
              setitem Ship2StatPDateName,0,""
              setitem Ship2StatInits,0,""
.Patch 5.4.4  Begin
              setitem Ship2StatTextLolRecdQty,0,""
              setitem Ship2StatTextLolRecDate,0,""              
              setitem Ship2StatTextLolComments,0,""                            
.Patch 5.4.4  End              
              setitem Ship2EditNotes2,0,""
              setitem Ship2EditNotes2B,0,""
              return

SetShipErrorMssgDefault
.Set Default for Owner File Maintenance
              setprop ErrorMssgStat1,visible=1
              setprop ErrorMssgStat2,visible=1
              setprop ErrorMssgStat3,visible=1
              setprop ErrorMssgStat4,visible=1
              setprop ErrorMssgStat5,visible=0
              setitem ErrorMssgStat1,0,"To Search By LR Number:"
              setitem ErrorMssgStat2,0,"Enter 6 Digit Number"
              setitem ErrorMssgStat3,0,"To Search By Purchase Order:"
              setitem ErrorMssgStat4,0,"Enter Entire PO"
              setitem ErrorMssgStat5,0,"      That Record Does Not Exist!"
              setitem ErrorMssgOK,0,"&OK"
              return

ShipDisableButtons
              move            "N",ExitFlag
              setprop         ShipOK,enabled=0
              setprop         ShipNew,enabled=0
              setprop         ShipModify,enabled=0
              setprop         ShipPrint,enabled=0
              setprop         ShipExit,enabled=0
              return
ShipDisableButtons1
              setprop         ShipQuit,enabled=0
              setprop         ShipSave,enabled=0
              setprop         ShipDelete,enabled=0
              return

ShipEnableButtons
              setprop         ShipOK,enabled=1
              setprop         ShipExit,enabled=1
              move            YES,ExitFlag
              return

ShipEnableButtons2
              setprop         ShipDelete,enabled=1
ShipEnableButtons3
              setprop         ShipQuit,enabled=1
              setprop         ShipSave,enabled=1
              return

Ship2SetReports
.Allows       selection of different reports for Shipping Records
.Called       by:  Ship2Print
              call            Report2DestroyObjects
              setprop         Report2,title="NIN Report"
              create          Report2;StatTextBoxes(1)=50:70:10:110,"Report Type","'>MS Sans Serif'(8)"
              create          Report2;StatTextBoxes(2)=70:90:10:110,"Printer","'>MS Sans Serif'(8)"
              create          Report2;ComboBoxes(1)=50:71:80:310,"",";S)hipping Info Request-Galley Format"
              move            NO,str1
              trap            Ship2ReportsSpoolTrap if Spool
              PRTOPEN         prfile,"FAXFILE","FAXFILE.PRN"
              PRTCLOSE prfile
              if (str1 = NO)
               create         Report2;ComboBoxes(2)=70:91:80:310,"",";L)aser 3;).PDF File;)Laser 2;)Fax"
              else
               create         Report2;ComboBoxes(2)=70:91:80:310,"",";L)aser 3;).PDF File;)Laser 2"
              endif
              activate StatTextBoxes(1)
              activate StatTextBoxes(2)
              activate ComboBoxes(1)
              activate ComboBoxes(2)
              listins         ObjectColl,StatTextBoxes(1),StatTextBoxes(2),ComboBoxes(1),ComboBoxes(2)
              setfocus ComboBoxes(1)
              return
Ship2ReportsSpoolTrap
              move            YES,str1
              return

Report2DestroyObjects
              destroy         ObjectColl
              return

ShipRetrieveLR
              move            C1,NORDPATH
              call            NORDKEY
              if over
               alert          note,"That Order Record Does Not Exist!",result
               setfocus ShipEditSearchLR
               call           ShipClearLower
.Patch 5.4.4               
               call       ShipClearLowerInventory 
.Patch 5.4.4               
               return
              elseif (KMAILERP <> "")
               scan           KMAILERP,OMLRPON
               if not equal
                              alert          note,"That Order Record Does Not Exist!",result
                              setfocus ShipEditSearchLR
                              call           ShipClearLower
.Patch 5.4.4               
                                   call       ShipClearLowerInventory 
.Patch 5.4.4                              
                              return
               endif
               reset          OMLRPON
              endif
              call            ShipLoadListView
              return

ShipRetrievePO
              count           result,KMAILERP
              if (result < 3)
               alert          note,"You must use 3+ Characters!",result
               setfocus ShipEditSearchPO
               return
              else
               unpack         KMAILERP,str3
               scan           B1,str3
               if equal
                              alert          note,"First 3 Characters cannot contain a blank space!",result
                              setfocus ShipEditSearchPO
                              return
               else
                              reset          str3
                              scan           QUESTION,str3
                              if equal
                                             alert          note,"First 3 Characters cannot contain '?'!",result
                                             setfocus ShipEditSearchPO
                                             return
                              endif
               endif
              endif
.2nd check security if outside user date is required for search
              if (security = 8)
               getitem        ShipEditSearchDate,0,str10
               call           Trim using str10
               count          N9,str10
               if (N9 = C10)
                              unpack         str10,MM,str1,DD,str1,CC,YY
                              pack           str10,CC,YY,MM,DD
               elseif (N9 = C8)
                              unpack         str10,MM,DD,CC,YY
                              pack           str10,CC,YY,MM,DD
               elseif (N9 <> C0)
                              alert          note,"Date must be in MMDDYYYY format!!",result
                              setfocus ShipEditSearchDate
                              return
               endif
               if (str10 <> "")
                              type           str10
                              if not equal
                                             alert          note,"Date must be in MMDDYYYY format!!",result
                                             setfocus ShipEditSearchDate
                                             return
                              endif
               else
                              move           C0,str10        .Set Up Default
               endif
               call           DATETEST
               if (dateflag = 1)
                              alert          note,"Date must be in MMDDYYYY format!!",result
                              setfocus ShipEditSearchDate
                              return
               endif
              endif
              call            cvtjul
              move            juldays,HoldMdate
              clear           NORDFLD1
              clear           NORDFLD2
              clear           NORDFLD3
              clear           NORDFLD4
              if (security = 8)
               move           "03X",AKEY1
              else
               move           "03L",AKEY1
              endif
              pack            NORDFLD3,AKEY1,KMAILERP
              move            C2,NORDPATH
              move            "NORDAIM",Location
              pack            KeyLocation,"Key: ",NORDFLD3
              call            NORDAIM
              if over
.Change StatText Boxes For Error Message
               setprop ErrorMssgStat1,visible=0
               setprop ErrorMssgStat2,visible=0
               setprop ErrorMssgStat3,visible=0
               setprop ErrorMssgStat4,visible=0
               setprop ErrorMssgStat5,visible=1
.Display Error Message
               setprop ErrorMssg,visible=1
.Reset StatText Boxes
               setprop ErrorMssgStat5,visible=0
               setprop ErrorMssgStat1,visible=1
               setprop ErrorMssgStat2,visible=1
               setprop ErrorMssgStat3,visible=1
               setprop ErrorMssgStat4,visible=1
               setfocus ShipEditSearchPO
               call           ShipClearLower
.Patch 5.4.4               
               call       ShipClearLowerInventory 
.Patch 5.4.4               
               return
              else
               move           "NORDKG",Location
               loop
                              if (security = 8)
                                             move           OMDTEM,MM          ;move maildate to julian vars
                                             move           OMDTED,DD
                                             move           OMDTEC,CC
                                             move           OMDTEY,YY
                                             call           CVTJUL
                                             if (juldays <> holdmdate)
                                                            goto ShipSearchLoopEnd
                                             endif
                              endif
                              call           ShipLoadListView
ShipSearchLoopEnd
                              call           NORDKG
                              until over
               repeat
        endif
              return

ShipLoadListView
              ShipListView.InsertItem giving N9 using OLRN
              ShipListView.SetItemText using N9,OMLRPON,1
              ShipListView.SetItemText using N9,OMLRNUM,2
              if (security <> 8)
               ShipListView.SetItemText using N9,OLNUM,3
              endif
              ShipListView.SetItemText using N9,OBRKNUM,4
              ShipListView.SetItemText using N9,OMLRKY,5
              pack            hold,ORDVARS
              ShipListView.SetItemText using N9,hold,6
              return

ShipLoadRecord
.Load LR information
              move            YES,NewFlag2   .Default
              setitem         ShipStatLR,0,OLRN
              if (OSTAT = "X")
               setitem        ShipStatOrderStat,0,"Cancelled Order"
              elseif (OSTAT = "Q")
               setitem        ShipStatOrderStat,0,"Cancelled/Billed Order"
              elseif (OSTAT = "z")
               setitem        ShipStatOrderStat,0,"Cancelled LCR"
              elseif (OSTAT = "x")
               setitem        ShipStatOrderStat,0,"Cancelled Pending Order"
              elseif (OSTAT = "p")
               setitem        ShipStatOrderStat,0,"Pending Order"
              elseif (OSTAT = "l")
               setitem        ShipStatOrderStat,0,"LCR"
              elseif (OSTAT = "B")
               setitem        ShipStatOrderStat,0,"Billed Order"
              elseif (OSTAT = "0")
               setitem        ShipStatOrderStat,0,"Live Order"
              else
               setitem        ShipStatOrderStat,0," "
              endif
              bump            OODNUM,4
              pack            NOFRFLD,OMLRNUM,OODNUM
              reset           OODNUM
              move            "ShipLRec-NOFRKEY",Location
              pack            KeyLocation,"Key: ",NOFRFLD
              call            NOFRKEY
              if not over
               setitem        ShipStatOffer2,0,OFDESC
              else
               setitem        ShipStatOffer2,0,"Offer Not Found"
              endif
.
              pack            MKEY,OMLRNUM,OCOBN
              rep             zfill,MKEY
              move            "ShipLRec-NMLRKEY",Location
              pack            KeyLocation,"Key: ",MKEY
              call            NMLRKEY
              if not over
               if (Security = 8)
               else
                              setitem        ShipStatMlrComp,0,Mcomp
                              setitem        ShipStatMlrCnt,0,MNAME
               endif
              else
               setitem        ShipStatMlrComp,0,"Mailer Not found"
              endif
              move            OLON,NOWNFLD
              move            "ShipLRec-NOWNKEY",Location
              pack            KeyLocation,"Key: ",NOWNFLD
              call            NOWNKEY
              if not over
               if (security = 8)
               else
                              setitem        ShipStatOwnerComp,0,ownocpy
                              setitem        ShipStatOwnerCnt,0,ownlonm
.Start Patch 1.34 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                                                    
.                              call           Trim using OWNCTN
                              call           Trim using OFULLFIL
.End Patch 1.34 CODE REPLACEMENT Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                                                      
.START PATCH 5.3 REMOVED LOGIC
.                              if (OWNCTN <> "")
.                                             pack           NFULFLD,OWNCTN
.                                             rep            zfill,NFULFLD
.                                             move           C1,NFULPATH
.                                             move           "ShipLRec-NFULKEY",Location
.                                             pack           KeyLocation,NFULFLD
.                                             call           NFULKEY
.                                             if not over
.                                                            setitem        ShipStatCC2,0,NFULCOMP
.
.Start Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                 
.                              if (OWNCTN <> "")
.                                             pack           COMPFLD6,OWNCTN
.                                             rep            zfill,COMPFLD6
.                                             move           C1,COMPPATH
.                                             move           "ShipLRec-COMPKEY6",Location
.                                             pack           KeyLocation,COMPFLD6
.                                             call           COMPKEY6
.                                             if not over
.                                                            if (COMPSVBFLG = "T")
.                                                                        setitem        ShipStatCC2,0,COMPCOMP
.                                                                         else
.                                                                                  setitem        ShipStatCC2,0,""
.                                                                         endif
.END PATCH 5.3 REMOVED LOGIC..............................................................                                        
.                                                      else
.                                                            setitem        ShipStatCC2,0,""
.                                             endif
.                              else
.                                             setitem        ShipStatCC2,0,""
.                              endif
.End Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                                                 
.Start Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                                               
                                        if (OFULLFIL <> "")
                                                  pack      COMPFLD,OFULLFIL
                                        call      zfillit using COMPFLD
                                        move      C1,COMPPATH
                                        move      "ShipLRec-COMPKEY",Location
                                        pack      KeyLocation,COMPFLD
                                          call    COMPKEY
                                        if not over
                                                            setitem        ShipStatCC2,0,COMPCOMP
                                                  else
                                                            setitem        ShipStatCC2,0,""
                                                  endif
                                        else
                                                  setitem        ShipStatCC2,0,""
                                        endif
                    endif
.Start Patch 1.34 Comment Out Owner/Fulfillment Association is not longer valid - Order now contains the fulfillment number and has a list/fulfillment assoc.                                               
              else
                              setitem        ShipStatOwnerComp,0,"Owner Record not found"
                              setitem        ShipStatOwnerCnt,0,""
                              setitem        ShipStatCC2,0,""
              endif
.Patch 5.4.4
                Move      OLRN,NSHP3FLD
                Call          NSHP3KEY
                If not over
                          call    FormatNumeric using NSHP3RQTY,str11       
                        setitem ShipEditTextLolRecdQty,0,str11
                        call trim using nshp3date
                        if (nshp3date <> "")
                              unpack  nshp3date,cc,yy,mm,dd
                              pack    str10,mm,slash,dd,slash,cc,yy                 
                        else
                              clear str10
                        endif
                        setitem ShipEditTextLolRecDate,0,str10                                
                        setitem ShipEditTextLOLComments,0,NSHP3Comments                                   
                Else
                        setitem ShipEditTextLolRecdQty,0,""
                        setitem ShipEditTextLolRecDate,0,""              
                        setitem ShipEditTextLOLComments,0,""                            
              Endif
.Patch 5.4.4
              
              
              
              move            ORTNNUM,NRTNFLD
              move            "ShipLRec-NRTNKEY",Location
              pack            KeyLocation,"Key: ",NRTNFLD
              call            NRTNKEY
              if not over
               setitem        ShipStatRtnComp,0,RTCOMP
               setitem        ShipStatRtnCnt,0,RTCNTCT
              else
               setitem        ShipStatRtnComp,0,"Ship to Not Found"
              endif
              setitem         ShipStatListName,0,o1des
.START PATCH 5.1.3A ADDED LOGIC
.             setitem         ShipStatListSel2,0,o2des
              packkey         NSEL2FLD,"1",OLRN
              move            "NSEL2KEY",Location
              pack            KeyLocation,"Key: ",NSEL2FLD
              call            NSEL2KEY
              if over
               move           O2DES,NSEL2NAME
              endif
              setitem         ShipStatListSel2,0,NSEL2NAME
.END PATCH 5.1.3A ADDED LOGIC
              move            OQTY,str9
              call            FormatNumeric using str9,str11
              setitem         ShipStatOrderQty2,0,str11
              clear           str10
              pack            str10,oodtem,slash,oodted,slash,oodtec,oodtey
              setitem         ShipStatOrderDate2,0,str10
              clear           str10
              pack            str10,ORTNDTEm,slash,ORTNDTEd,slash,ORTNDTEc,ORTNDTEy
              setitem         ShipStatRtnDate2,0,str10
              clear           str10
              pack            str10,OMDTEm,slash,OMDTEd,slash,OMDTEc,OMDTEy
              setitem         ShipStatMailDate2,0,str10
.
              setitem         ShipStatPO2,0,OMLRPON
              setitem         ShipStatKey2,0,OMLRKY
.
              call            TRIM using OSHP
              if (OSHP = "")
               setitem        ShipComboShip,0,1
              else
               move           OSHP,N2
               add            C2,N2
               setitem        ShipComboShip,0,N2
               getitem        ShipComboShip,n2,holdsinfo
               if (n2 = c2)
                              move           "Email",holdsinfo
               endif
               if (n2 = 12)
.START PATCH 5.3 REPLACED LOGIC
.                              move           NFULCOMP,holdsinfo
                              move           COMPCOMP,holdsinfo
.END PATCH 5.3 REPLACED LOGIC
               endif
              endif
              move            NORDFLD,NSHPFLD
.Load Shipping file information
              move            "ShipLRec-NSHPKEY",Location
              pack            KeyLocation,"Key: ",NSHPFLD
              call            NSHPKEY
              if over
               setitem        ShipGroupBox,0,"No Shipping record for this order."
               call           ShipClearlower
              else
ShipLoadList
               move           NO,NewFlag2
               setitem        ShipGroupBox,0,""
               setitem        ShipEditInfo,0,SINFO
               move           SQUANT,str9
               call           FormatNumeric using str9,str11
               setitem        ShipEditShipQty,0,str11
               if (Security = 8)
                              setitem        ShipEditPrice,0,""
               else
                              move           C0,N32
                              unpack         SPOST,str2,str3
                              call           Trim using str2
                              call           Trim using str3
                              if (str3 <> "")
                                             pack           str7,str2,PERIOD,str3
                                             move           "$$9.99",str6
                                             move           str7,N32
                                             edit           N32,str6
                              else
                                             clear          str6
                              endif
                              setitem        ShipEditPrice,0,str6
               endif
               call           Trim using SDATE
               if (SDATE <> "")
                              unpack         SDATE,str2,yy,mm,dd
                              pack           str10,mm,slash,dd,slash,str2,yy
               else
                              clear          str10
               endif
               setitem        ShipEditShipDate,0,str10
               setitem        ShipEditTrack,0,Strack
.set combo box using scode
               if (SCODE = "C")
                              move           C2,N2
               elseif (SCODE = "I")
                              move           C3,N2
               elseif (SCODE = "M")
                              move           C4,N2
               elseif (SCODE = "P")
                              move           C5,N2
               elseif (SCODE = "S")
                              move           C6,N2
               elseif (SCODE = "T")
                              move           C7,N2
               elseif (SCODE = "E")
                              move           C8,N2
               elseif (SCODE = "D")
                              move           C9,N2
.>Patch 5.1.7 Code Added
               elseif (SCODE = "A")
                              move           C10,N2
.>Patch 5.1.7
.>Patch 5.4.1 Code Added
.Frontline
               elseif (SCODE = "F")
                              move           C11,N2
.>Patch 5.4.1
.>Patch 5.4.2 Code Added
.Pidi
               elseif (SCODE = "Z")
                              move           "12",N2
.>Patch 5.4.2
.begin patch 5.4.5
.MMI
               elseif (SCODE = "m")
                              move           "13",N2
.end patch 5.4.5
.begin patch 5.4.7
.PDMG
               elseif (SCODE = "X")
                              move           "14",N2
.end patch 5.4.7
.DMI
               elseif (SCODE = "d")
                              move           "15",N2
.end patch 5.4.7
               else
                              move           C1,N2
               endif
               setitem ShipComboCode,0,N2
.Security Fields
               call           Trim using SRDATE
               if (SRDATE <> "")
                              unpack         SRDATE,str2,yy,mm,dd
                              pack           str10,mm,slash,dd,slash,str2,yy
               else
                              clear          str10
               endif
               setitem ShipEditRecDate,0,str10
               setitem ShipEditRecInit,0,SINITS
               call           Trim using SPDATE
               if (SPDATE <> "")
                              unpack         SPDATE,str2,yy,mm,dd
                              pack           str10,mm,slash,dd,slash,str2,yy
               else
                              clear          str10
               endif
               setitem ShipEditPrintDate,0,str10
               setitem ShipEditPrintInit,0,SPINITS
              endif
              pack            NSHP2FLD,NORDFLD
              move            "NSHP2KEY",Location
              pack            KeyLocation,"Key: ",NSHP2FLD
              call            NSHP2KEY
              if over
               clear          SNOTES2
               clear          SNOTES2B
              else
               move           NO,NewFlag2
              endif
              setitem ShipEditNotes,0,SNOTES2
              setitem ShipEditNotes2,0,SNOTES2B
.
              if (NewFlag2 = YES)
               setprop        ShipNew,enabled=1
              else
               setprop        ShipModify,enabled=1
              endif
              setprop         ShipPrint,enabled=1
              return

ShipVerifyDataLR
              getitem         ShipStatLR,0,OLRN
              call            Trim using OLRN
              if (OLRN = "")
               alert          note,"You must call up a valid LR first!",result
               move           YES,ReturnFlag
               return
              endif
.Following 2 checks are double checks - New/Modify Buttons should not have allowed creation of a New Shipping record
              packkey         NINVFLD,OLRN
              move            C1,NINVPATH
              move            "ShipV.-NINVKEY",Location
              pack            KeyLocation,"Key: ",NINVFLD
              call            NINVKEY
              if not over
.START PATCH 5.1.2 REMOVED LOGIC
.              alert          note,"Not Allowed to enter info for Billed orders!",RESULT
.              move           YES,ReturnFlag
.              return
.END PATCH 5.1.2 REMOVED LOGIC
              endif
              packkey         NORDFLD,OLRN
              move            C1,NORDPATH
              move            "ShipV.-NORDKEY",Location
              pack            KeyLocation,"Key: ",NORDFLD
              call            NORDKEY
              if over
               alert          note,"Valid LR Required!",RESULT
               move           YES,ReturnFlag
               return
              endif
              return

ShipVerifyData
              if (NewFlag = YES)
               move           OLRN,SLRNUM
              endif
.
              getitem         ShipEditInfo,0,SINFO
.
              getitem         ShipEditShipQty,0,str11
              call            RemoveChar using str11,COMMA
              call            Trim using str11
              move            C0,N9
              move            str11,N9
              move            N9,SQUANT
              type            SQUANT
              if not equal
               alert          note,"Invalid Quantity!!!",RESULT
               move           YES,ReturnFlag
               setfocus ShipEditShipQty
               return
              endif
.
              clear           SPOST
              getitem         ShipEditPrice,0,str6
              call            Trim using str6
              if (str6 <> "")
               move           "$",str1
               call           RemoveChar using str6,str1
               scan           ".",str6
               if not equal
                              reset          str6
                              move           str6,SPOST
               else
                              reset          str6
                              move           C0,N32
                              move           str6,N32
                              if (N32 > 0)
                                             move           N32,str6
                                             rep            zfill,str6
                                             call           RemoveChar using str6,PERIOD
                                             bump           str6
                                             move           str6,SPOST
                              endif
               endif
              endif
.
              getitem         ShipEditShipDate,0,str10
              call            Trim using str10
              if (str10 = "")
               alert          note,"Date must be in MMDDYYYY format!!",RESULT
               move           YES,ReturnFlag
               setfocus ShipEditShipDate
               return
              endif
              count           N9,str10
              if (N9 = 10)
               unpack         str10,MM,str1,DD,str1,str2,YY
               pack           str10,str2,YY,MM,DD
              elseif (N9 = 8)
               unpack         str10,MM,DD,str2,YY
               pack           str10,str2,YY,MM,DD
              else
               alert          note,"Date must be in MMDDYYYY format!!",RESULT
               move           YES,ReturnFlag
               setfocus ShipEditShipDate
               return
              endif
              type            str10
              if not equal
               alert          note,"Date must be in MMDDYYYY format!!",RESULT
               move           YES,ReturnFlag
               setfocus ShipEditShipDate
               return
              endif
              pack            SDATE,str2,yy,mm,dd
              move            str2,cc
              call            DATETEST
              if (dateflag = 1)
               alert          note,"Ship Date is not Valid!",RESULT
               move           YES,ReturnFlag
               setfocus ShipEditShipDate
               return
              endif
.
              move            OODTEM,MM              MOVE ORDER DATE TO JULIAN VAR'S
              move            OODTED,DD                          "       "
              move            OODTEY,YY                          "       "
              call            CVTJUL
              move            JULDAYS,JDATE             SAVE JULIAN ORDER DATE.
              move            ZERO,MM            CLEAR VARIABLES.
              move            ZERO,DD
              move            ZERO,YY
              unpack          SDATE,cc,yy,mm,dd
              call            CVTJUL
              move            JULDAYS,MAILDATE             SAVE JULIAN SHIP DATE.
              if (maildate < Jdate)
               alert          note,"Ship Date must be after Order Date!",RESULT
               move           YES,ReturnFlag
               setfocus ShipEditShipDate
               return
              endif
.
              getitem         ShipEditTrack,0,STRACK
.
              getitem         ShipComboCode,0,N2
              if (N2 = C2)
               move           "C",SCODE
              elseif (N2 = C3)
               move           "I",SCODE
              elseif (N2 = C4)
               move           "M",scode
              elseif (N2 = C5)
               move           "P",SCODE
              elseif (N2 = C6)
               move           "S",SCODE
              elseif (N2 = C7)
               move           "T",SCODE
              elseif (N2 = C8)
               move           "E",SCODE
              elseif (N2 = C9)
               move           "D",SCODE
.>Patch 5.1.7 Code Added
              elseif (N2 = C10)
               move           "A",SCODE
.>Patch 5.1.7 Code Added
.begin patch 5.4.5
              elseif (N2 = C11)
               move           "F",SCODE
              elseif (N2 = 12)
               move           "Z",SCODE
              elseif (N2 = 13)
               move           "m",SCODE
.end patch 5.4.5
.begin patch 5.47
              elseif (N2 = 14)
               move           "X",SCODE
.end patch 5.47
              elseif (N2 = 15)
               move           "d",SCODE
              else
               move           B1,SCODE
              endif
              return


ShipVerifyDataSecurity
.START PATCH 5.1.1 REPLACED LOGIC
.             getitem ShipEditRecDate,0,str10
.             call            Trim using str10
.             if (str10 = "")
.              alert          note,"Date must be in MMDDYYYY format!!",RESULT
.              move           YES,ReturnFlag
.              setfocus ShipEditRecDate
.              return
.             endif
.             count           N9,str10
.             if (N9 = 10)
.              unpack         str10,MM,str1,DD,str1,CC,YY
.              pack           str10,CC,YY,MM,DD
.             elseif (N9 = 8)
.              unpack         str10,MM,DD,CC,YY
.              pack           str10,CC,YY,MM,DD
.             else
.              alert          note,"Date must be in MMDDYYYY format!!",RESULT
.              move           YES,ReturnFlag
.              setfocus ShipEditRecDate
.              return
.             endif
.             type            str10
.             if not equal
.              alert          note,"Date must be in MMDDYYYY format!!",RESULT
.              move           YES,ReturnFlag
.              setfocus ShipEditRecDate
.              return
.             endif
.             pack            SRDATE,CC,yy,mm,dd
.             call            DATETEST
.             if (dateflag = 1)
.              alert          note,"Ship Date is not Valid!",RESULT
.              move           YES,ReturnFlag
.              setfocus ShipEditRecDate
.              return
.             endif
.             getitem ShipEditRecInit,0,SINITS
..
.             getitem ShipEditPrintDate,0,str10
.             call            Trim using str10
.             if (str10 = "")
.              alert          note,"Date must be in MMDDYYYY format!!",RESULT
.              move           YES,ReturnFlag
.              setfocus ShipEditPrintDate
.              return
.             endif
.             count           N9,str10
.             if (N9 = 10)
.              unpack         str10,MM,str1,DD,str1,CC,YY
.              pack           str10,CC,YY,MM,DD
.             elseif (N9 = 8)
.              unpack         str10,MM,DD,CC,YY
.              pack           str10,CC,YY,MM,DD
.             else
.              alert          note,"Date must be in MMDDYYYY format!!",RESULT
.              move           YES,ReturnFlag
.              setfocus ShipEditPrintDate
.              return
.             endif
.             type            str10
.             if not equal
.              alert          note,"Date must be in MMDDYYYY format!!",RESULT
.              move           YES,ReturnFlag
.              setfocus ShipEditPrintDate
.              return
.             endif
.             pack            SPDATE,CC,yy,mm,dd
.             call            DATETEST
.             if (dateflag = 1)
.              alert          note,"Ship Date is not Valid!",RESULT
.              move           YES,ReturnFlag
.              setfocus ShipEditPrintDate
.              return
.             endif
.             getitem ShipEditPrintInit,0,SPINITS
........................................................
              getitem ShipEditRecDate,0,str10
              call            Trim using str10
              call            RemoveChar using str10,SLASH
              if (str10 <> "")
               count          N9,str10
               if (N9 = 10)
                              unpack         str10,MM,str1,DD,str1,CC,YY
                              pack           str10,CC,YY,MM,DD
               elseif (N9 = 8)
                              unpack         str10,MM,DD,CC,YY
                              pack           str10,CC,YY,MM,DD
               else
                              alert          note,"Date must be in MMDDYYYY format!!",RESULT
                              move           YES,ReturnFlag
                              setfocus ShipEditRecDate
                              return
               endif
               type           str10
               if not equal
                              alert          note,"Date must be in MMDDYYYY format!!",RESULT
                              move           YES,ReturnFlag
                              setfocus ShipEditRecDate
                              return
               endif
               pack           SRDATE,CC,yy,mm,dd
               call           DATETEST
               if (dateflag = 1)
                              alert          note,"Ship Date is not Valid!",RESULT
                              move           YES,ReturnFlag
                              setfocus ShipEditRecDate
                              return
               endif
              endif
              getitem ShipEditRecInit,0,SINITS
.
              getitem ShipEditPrintDate,0,str10
              call            Trim using str10
              call            RemoveChar using str10,SLASH
              if (str10 <> "")
               count          N9,str10
               if (N9 = 10)
                              unpack         str10,MM,str1,DD,str1,CC,YY
                              pack           str10,CC,YY,MM,DD
               elseif (N9 = 8)
                              unpack         str10,MM,DD,CC,YY
                              pack           str10,CC,YY,MM,DD
               else
                              alert          note,"Date must be in MMDDYYYY format!!",RESULT
                              move           YES,ReturnFlag
                              setfocus ShipEditPrintDate
                              return
               endif
               type           str10
               if not equal
                              alert          note,"Date must be in MMDDYYYY format!!",RESULT
                              move           YES,ReturnFlag
                              setfocus ShipEditPrintDate
                              return
               endif
               pack           SPDATE,CC,yy,mm,dd
               call           DATETEST
               if (dateflag = 1)
                              alert          note,"Ship Date is not Valid!",RESULT
                              move           YES,ReturnFlag
                              setfocus ShipEditPrintDate
                              return
               endif
              endif
              getitem ShipEditPrintInit,0,SPINITS
.END PATCH 5.1.1 REPLACED LOGIC
              return

ShipVerifyInventory


              getitem         ShipEditTextLolRecdQty,0,str11
              call            RemoveChar using str11,COMMA
              call            Trim using str11
              move            C0,N9
              move            str11,N9
              move            N9,NSHP3RQTY
              type            NSHP3RQTY
              if not equal
               alert          note,"Invalid Quantity!!!",RESULT
               move           YES,ReturnFlag
                 move               c0 to shp3wrtflg               
               setfocus       ShipEditTextLolRecdQty
               return
               else
                              if (n9 <> c0)
                                    move c1 to shp3wrtflg
                              endif
              endif



              getitem ShipEditTextLolRecDate,0,str10
              call            Trim using str10
              call            RemoveChar using str10,SLASH
              if (str10 <> "")
               count          N9,str10
               if (N9 = 10)
                              unpack         str10,MM,str1,DD,str1,CC,YY
                              pack           str10,CC,YY,MM,DD
               elseif (N9 = 8)
                              unpack         str10,MM,DD,CC,YY
                              pack           str10,CC,YY,MM,DD
               else
                              alert          note,"Date must be in MMDDYYYY format!!",RESULT
                              move           YES,ReturnFlag
                                    move                c0 to shp3wrtflg                                             
                              setfocus ShipEditTextLolRecDate
                              return
               endif
               type           str10
               if not equal
                              alert          note,"Date must be in MMDDYYYY format!!",RESULT
                              move           YES,ReturnFlag
                                    move                c0 to shp3wrtflg                                             
                              setfocus ShipEditTextLolRecDate
                              return
               endif
               pack           NSHP3DATE,CC,yy,mm,dd
               call           DATETEST
               if (dateflag = 1)
                              alert    note,"Ship Date is not Valid!",RESULT
                              move     YES,ReturnFlag
                                    move     c0 to shp3wrtflg                                             
                              setfocus ShipEditTextLolRecDate
                              return
               endif
              endif
              
              getitem ShipEditTextLOLComments,0,nshp3comments
              call trim using nshp3comments
              if (nshp3comments <> "")
                    move c1,shp3wrtflg
              endif
              return


ShipPreVerifyData LRoutine FrmPtr
.Used to test if there are any valid entries at all.
.This allows saving of Notes fields independently of Shipping Record.
.Takes into consideration that default values are dropped into ShipEditInfo
.and so does not attempt a read.
.
.Initialize Return Value
              move            C0,FrmPtr
.
              getitem         ShipEditShipQty,0,str11
              call            Trim using str11
              if (str11 <> "")
               move           C1,FrmPtr
               return
              endif
.
              getitem         ShipEditPrice,0,str6
              call            Trim using str6
              if (str6 <> "")
               move           C1,FrmPtr
               return
              endif
.
              getitem         ShipEditShipDate,0,str10
              call            Trim using str10
              if (str10 <> "")
               move           C1,FrmPtr
               return
              endif
              getitem         ShipEditTrack,0,str25
              call            Trim using str25
              if (str25 <> "")
               move           C1,FrmPtr
               return
              endif
.
              getitem         ShipComboCode,0,N2
              if (N2 > 1)
               move           C1,FrmPtr
              endif
              return

ShipVerifyNotes
              getitem ShipEditNotes,0,SNOTES2
              call            Trim using SNOTES2
              getitem ShipEditNotes2,0,SNOTES2B
              call            Trim using SNOTES2B
              return

ShpChkVariance
              move            C0,N10
              move            OQTY,N10
              mult            ".05",N10
              move            N10,fivePER
              move            SQUANT,N10
              move            OQTY,DIFF
              sub             N10,DIFF
              compare         C0,DIFF
              call            NEG IF LESS
              compare         DIFF,fivePER
              If              Not Greater
.              call            sendnews IF NOT GREATER
                              call sendnews 
//Use the Speak Method of this control
                              pack           str500 from "Please double check the shipped quantity, Thank you "
                              Sapi.Speak using Str500
               endif
              return
NEG
              mult            SEQ,DIFF
              return
sendnews
              move            C1,NCNTPATH      .set path to read by contact id#
              move            C3,NCNTLOCK      .no locks
              move            OCOCODE,NCNTFLD
              clear           CNTNAME
              clear           str25
              clear           str35
              pack            KeyLocation,"Key: ",NCNTFLD
              move            "NCNTKEY",Location
              call            NCNTKEY
              if not over
               move           CNTNAME,str35
              endif
              append          str35,str25
              reset           str25
              clear           str1
              append          str25,str1         .1st init
              reset           str1
              scan            B1,str25
              bump            str25 by 1
              clear           str6
              append          str25,str6
              reset           str6
              call            RTrim using str6
              pack            str7,str1,str6
              reset           STR25
              reset           str35
              move            str35,user
.begin patch 5.4
          Move      "This is a Informational e-mail from  the shipping program",MailSubjct
.              move            "This is a Informational e-mail from  the shipping program",SmtpSubject Subject
.   Set the text message that is send with the attachments
          Pack      MailBody from "This is a Informational e-mail from  the shipping program":
                    CRLF:
                    "record## ",Olrn,B1:
                    CRLF:
                    " Your above LR had a shipping qty variance:":
                    CRLF:
                    "Order Qty ",OQty," Shipped Qty ",SQuant:
                    CRLF:
                    "Please review & correct as necessary.",CRLF
          pack      MailTO from str7,"@nincal.com"                    
          pack      MailFrom from str7,"@nincal.com"                  
          call      SendMail
.              move            "This is a Informational e-mail from  the shipping program",SmtpTextMessage(1)   Array <Text message >
.              clear           str25
.              append          "record## ",str25
.              append          OLRN,str25
.              append          B1,str25
.              reset           str25
.              move            str25,SmtpTextMessage(2)   Array <Text message >
.              move            " Your above LR had a shipping qty variance:",SmtpTextMessage(3)   Array <Text message >
.              clear           str55
.              append          "Order qty ",str55
.              append          OQTY,str55
.              append          ", Shipped qty ",str55
.              move            squant,str10
.              append          str10,str55
.              reset           str55
.              move            str55,SmtpTextMessage(4)   Array <Text message >
.              move            "Please review & correct as necessary.",SmtpTextMessage(5)   Array <Text message >
.              move            "5",SmtpTextIndexLast                               Index to last entry in TextMessage array
.              clear           taskname
.              move            "NTS4",SmtpEmailServer                   Address of email serverc
.              clear           smtpemailaddress
.              append          str7,SmtpEmailAddress
.              append          "@nincal.com",SmtpEmailAddress
.              reset           smtpemailaddress
.              move            str7,SmtpUserName                                User name
.              move            STR35,SmtpUserFullName              User Full Name
..   Set the destinations of the email. Max 100 (Mime spec)
.              move            smtpemailaddress,SmtpDestinations(1,1)                  .send a copy to the originator
.              move            user,SmtpDestinations(1,2)                          originators UserName
.              move            "1",SmtpDestIndexLast                               Index to last entry in Dest array
.              move            "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.              clear           SmtpLogFile                                         'Clear' disables the LogFile
.              Call            SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.end patch 5.4
              return

ShpWriteLog
              move            C0,N2
              pack            SALES,OSALES10,OSALES
              move            SALES,N2
              if (N2 = 6 | N2 = 19)
               filepi         3;LOGFILE
               read           LOGFILE,NORDFLD;;
               if over
                              write          LOGFILE,NORDFLD;ORDVARS
                              alert          Note,"Added to daily fax file",RESULT
               endif
              endif
              return
.
ShipEmailerEnabled
              setprop         ShipEMail,enabled=0
              setprop         ShipEMailSend,enabled=1
              setprop         ShipEMailQuit,enabled=1
              setprop         ShipEditFrom,enabled=1
              setprop         ShipEditMessage,enabled=1
              setprop         ShipEditFrom,visible=1
              setprop         ShipEditMessage,visible=1
              setprop         ShipStatFrom,visible=1
              setprop         ShipStatMessage,visible=1
              setfocus ShipEditFrom
              return
.
ShipEmailerDisable
              setprop         ShipEMailSend,enabled=0
              setprop         ShipEMailQuit,enabled=0
              setprop         ShipEditFrom,enabled=0
              setprop         ShipEditMessage,enabled=0
              setfocus ShipEditSearchLR
              setitem         ShipEditFrom,0,""
              setitem         ShipEditMessage,0,""
              setprop         ShipEditFrom,visible=0
              setprop         ShipEditMessage,visible=0
              setprop         ShipStatFrom,visible=0
              setprop         ShipStatMessage,visible=0
              setprop         ShipEMail,enabled=1
              return
.
ShipEmailer
              getitem         ShipEditFrom,0,MailTo
              call            Trim using MailTo
              if (MailTo = "")
.              getitem         ShipEditFrom,0,smtpemailaddress
.              call            Trim using smtpemailaddress
.              if (smtpemailaddress = "")
               alert          note,"Valid Email address is required!",result
               setfocus ShipEditFrom
               return
              endif
              scan            "@",MailTo
              if not equal
               alert          note,"Valid Email address is required!",result
               setfocus ShipEditFrom
               return
              endif
              reset           MailTo
              getitem         ShipEditSearchPO,0,str25
              getitem         ShipEditMessage,0,EmailMesg
              if (str6 = "")
.we don't have lr so will be a general email to List Managenment?
              else
.               pull nin contact info for email
              endif
.begin patch 5.3
              move            "This is a Request for Information from the WEB Shipping Program",MailSubjct
.              move            "This is a Request for Information from the WEB Shipping Program",SmtpSubject Subject
.   Set the text message that is send with the attachments
          Pack      MailBody from "This is a Request for Information from the WEB Shipping Program":
                    CRLF:
                    "- Record## ",str6,b1,str25:
                    CRLF

.              move            "This is a Request for Information from the WEB Shipping Program",SmtpTextMessage(1)   Array <Text message >
.              clear           str25
.              clear           str100
.              append          "- Record## ",str100
.              append          str6,str100
.              append          B1,str100
.              append          str25,str100
.              reset           str100
.              move            str100,SmtpTextMessage(2)   Array <Text message >
              for N2 from "3" to "13"
               movefptr EmailMesg,startfp
               movelptr EmailMesg,endfp
               until (startfp = endfp)
               call           PARSITUP using str250,EmailMesg,C1
.               move           str250,SmtpTextMessage(N2)
               append         str250 to Mailbody
               append         CRLF to MailBody
               reset          MailBody              
              repeat
dagnabit
              add             C1,N2
               append         MailTo to Mailbody
               append         CRLF to MailBody
.              move            SmtpEmailAddress,SmtpTextMessage(N2)
.              move            SmtpEmailAddress,SmtpUserFullName              User Full Name
.              move            N2,SmtpTextIndexLast                               Index to last entry in TextMessage array
              clear           taskname
.              move            "NTS4",SmtpEmailServer                   Address of email serverc
              clear           recipient
.              move            "ListManagementShippingInquiry@nincal.com" to recipient
              Pack            MailTO from "ListManagementShippingInquiry@nincal.com,"
          Move      MailTO,MailFrom
.   Set the destinations of the email. Max 100 (Mime spec)
.              move            smtpemailaddress to SmtpDestinations(2,1)                  .send a copy to the originator
.              move            smtpemailaddress,SmtpDestinations(2,2)                          originators UserName
.              move            recipient to SmtpDestinations(1,1)                  .send a copy to the originator
.              move            recipient,SmtpDestinations(1,2)                          originators UserName
.              move            "2",SmtpDestIndexLast                               Index to last entry in Dest array
.              move            "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
.              clear           SmtpLogFile                                         'Clear' disables the LogFile
.              call            SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
          call      SendMail
          If        (MailOkFlag = Yes)
              alert           Note,"Message sent.",RESULT
                    endif
.end patch 5.3
              call            ShipEmailerDisable
              return

Ship2LoadListView
              pack            hold,ORDVARS
              ListIts.Add giving ListIt using               *Index=1,*Text=""
              setprop         ListIt,*SubItems(1)=OLRN
              if (OSTAT = "0")
               move           "Live Order",str25
              elseif (OSTAT = "B")
               move           "Billed Order",str25
              elseif (OSTAT = "X")
               move           "Cancelled Order",str25
              elseif (OSTAT = "Q")
               move           "Cancelled Billed Order",str25
              elseif (OSTAT = "l")
               move           "LCR",str25
              elseif (OSTAT = "z")
               move           "Cancelled LCR",str25
              elseif (OSTAT = "p")
               move           "Pending Order",str25
              elseif (OSTAT = "x")
               move           "Cancelled Pending Order",str25
              else
               clear          str25
              endif
              setprop         ListIt,*SubItems(2)=str25
.Load Shipping file information
              move            OLRN,NSHPFLD
              move            "Ship2LLV-NSHPKEY",Location
              pack            KeyLocation,"Key: ",NSHPFLD
              call            NSHPKEY
              if over
               clear          taskname
              else
               pack           taskname,SHPVARS
              endif
.Patch 5.4.4              
                    Move OLRN,NSHP3FLD
                    Call NSHP3KEY
                    if over 
                              clear SHPINVHOLD
                    else
                              pack SHPINVHOLD,NSHP3VARS
                    endif
.Patch 5.4.4
              
              move            SQUANT,str9
              call            FormatNumeric using str9,str11
              if (str11 = "")
               move           B1,str11
              endif
              setprop         ListIt,*SubItems(3)=str11
              unpack          SDATE,CC,YY,MM,DD
              call            Trim using MM
              if (MM = "")
               clear          str10
              else
               pack           str10,MM,SLASH,DD,SLASH,CC,YY
              endif
              setprop         ListIt,*SubItems(4)=str10
              call            Trim using SINFO
              setprop         ListIt,*SubItems(5)=SINFO
              call            Trim using STRACK
              setprop         ListIt,*SubItems(6)=STRACK
              setprop         ListIt,*SubItems(7)=OLNUM
              setprop         ListIt,*SubItems(8)=OLON
              setprop         ListIt,*SubItems(9)=OMLRNUM
              move            OQTY,str9
              call            FormatNumeric using str9,str11
              setprop         ListIt,*SubItems(10)=str11
              call            Trim using OMDTEM
              if (OMDTEM = "")
               clear          str10
              else
               pack           str10,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
              endif
              setprop         ListIt,*SubItems(11)=str10
              call            Trim using ORTNDTEM
              if (ORTNDTEM = "")
               clear          str10
              else
               pack           str10,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
              endif
              setprop         ListIt,*SubItems(12)=str10
              setprop         ListIt,*SubItems(13)=OCAMP
              move            C1,NCNTPATH
              move            OCOCODE,NCNTFLD
              rep             zfill,NCNTFLD
              pack            KeyLocation,"Key: ",NCNTFLD
              move            "Ship2LLV-NCNTKEY",Location
              call            NCNTKEY
              call            Trim using CNTNAME
              setprop         ListIt,*SubItems(14)=CNTNAME
              setprop         ListIt,*SubItems(15)=OMLRKY
              setprop         ListIt,*SubItems(16)=hold
              setprop         ListIt,*SubItems(17)=taskname
.Patch 5.4.4
              setprop         ListIt,*SubItems(18)=SHPINVHOLD
.Patch 5.4.4
              return

Ship2LoadScreen
              unpack          hold,ORDVARS
              unpack          taskname,SHPVARS
              setitem Ship2StatLRNumber,0,OLRN
              if (OSTAT = "0")
               move           "Live Order",str25
              elseif (OSTAT = "B")
               move           "Billed Order",str25
              elseif (OSTAT = "X")
               move           "Cancelled Order",str25
              elseif (OSTAT = "Q")
               move           "Cancelled Billed Order",str25
              elseif (OSTAT = "l")
               move           "LCR",str25
              elseif (OSTAT = "z")
               move           "Cancelled LCR",str25
              elseif (OSTAT = "p")
               move           "Pending Order",str25
              elseif (OSTAT = "x")
               move           "Cancelled Pending Order",str25
              else
               clear          str25
              endif
              setitem Ship2StatStatusName,0,str25
              pack            NCMPFLD,OCAMP
              move            C1,NCMPPATH
              move            "Ship2LoadS-NCMPKEY",Location
              pack            KeyLocation,"Key: ",NCMPFLD
              call            NCMPKEY
              setitem Ship2StatCampaignName,0,NCMPCNAME
              setitem Ship2StatListName,0,O1DES
              move            C1,NOWNPATH
              pack            NOWNFLD,OLON
              move            "Ship2LoadS-NOWNKEY",Location
              pack            KeyLocation,"Key: ",NOWNFLD
              call            NOWNKEY
              setitem Ship2StatOwnerName,0,OWNOCPY
              move            C1,NMLRPATH
              pack            MKEY,OMLRNUM,"000"
              move            "Ship2LoadS-NMLRKEY",Location
              pack            KeyLocation,"Key: ",MKEY
              call            NMLRKEY
              setitem Ship2StatMailerName,0,MCOMP
              move            OQTY,str9
              call            FormatNumeric using str9,str11
              setitem Ship2StatOQtyName,0,str11
              setitem Ship2StatMPName,0,OMLRKY
              call            Trim using OMDTEM
              if (OMDTEM = "")
               clear          str10
              else
               pack           str10,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
              endif
              setitem Ship2StatMDateName,0,str10
              call            Trim using ORTNDTEM
              if (ORTNDTEM = "")
               clear          str10
              else
               pack           str10,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
              endif
              setitem Ship2StatRDateName,0,str10
              move            C1,NCNTPATH
              move            OCOCODE,NCNTFLD
              rep             zfill,NCNTFLD
              pack            KeyLocation,"Key: ",NCNTFLD
              move            "Ship2LoadS-NCNTKEY",Location
              call            NCNTKEY
              setitem Ship2StatContactName,0,CNTNAME
              
.Patch 5.4.4  Begin
              call    FormatNumeric using NSHP3RQTY,str11
                setitem Ship2StatTextLolRecdQty,0,str11
                call trim using nshp3date
                if (nshp3date <> "")
                    unpack  nshp3date,cc,yy,mm,dd
                    pack    str10,mm,slash,dd,slash,cc,yy                 
                else
                    clear str10
                endif                       
                setitem Ship2StatTextLolRecDate,0,str10            
                setitem Ship2StatTextLolComments,0,NSHP3Comments                                          
.Patch 5.4.4  End                
              
              setitem Ship2StatShipInfoName,0,SINFO
              move            SQUANT,str9
              call            FormatNumeric using str9,str11
              setitem Ship2StatQtyName,0,str11
              unpack          SDATE,CC,YY,MM,DD
              call            Trim using MM
              if (MM = "")
               clear          str10
              else
               pack           str10,MM,SLASH,DD,SLASH,CC,YY
              endif
              setitem Ship2StatDateName,0,str10
              setitem Ship2StatTrackingName,0,STRACK
              unpack          SRDATE,CC,YY,MM,DD
              call            Trim using MM
              if (MM = "")
               clear          str10
              else
               pack           str10,MM,SLASH,DD,SLASH,CC,YY
              endif
              setitem Ship2StatRDateName,0,str10
              unpack          SPDATE,CC,YY,MM,DD
              call            Trim using MM
              if (MM = "")
               clear          str10
              else
               pack           str10,MM,SLASH,DD,SLASH,CC,YY
              endif
              setitem Ship2StatPDateName,0,str10
              setitem Ship2StatInits,0,SPINITS
              pack            NSHP2FLD,OLRN
              move            "Ship2LoadS-NSHP2KEY",Location
              pack            KeyLocation,"Key: ",NSHP2FLD
              call            NSHP2KEY
              if over
               clear          SNOTES2
               clear          SNOTES2B
              endif
              setitem Ship2EditNotes2,0,SNOTES2
              setitem Ship2EditNotes2B,0,SNOTES2B
              return
...................................................................
.........................GUI HOUSEKEEPING..........................
...................................................................
ShipSwitchTab LRoutine        FrmPtr
.Redundant code, but a safety check!!!
              if (Security = 8)
               return
              endif
.START PATCH 5.1 REMOVED LOGIC
.BETA Test logic
.             reset           revtyps
.             scan            INITS,revtyps
.             if not equal
.              return
.             endif
.BETA Test logic END
.END PATCH 5.1 REMOVED LOGIC
              if (TabNum <> FrmPtr)
               move           TabNum,N2
               call           ShipTabClick
               move           FrmPtr,N2
               call           ShipTabChange
               setitem        ShipTabControl,0,FrmPtr
              endif
              return

ShipTabClick
.Force LostFocus event for fields when switching tabs.
.This is done so that fields found on other forms that require data
.established through LostFocus events will be set.
.Switching to another tab does not affect the focus on that
.particular form!  LostFocus events must be triggered!
.
.Redundant code, but a safety check!!!
              if (Security = 8)
               return
              endif
.START PATCH 5.1 REMOVED LOGIC
.BETA Test logic
.             reset           revtyps
.             scan            INITS,revtyps
.             if not equal
.              return
.             endif
.BETA Test logic END
.END PATCH 5.1 REMOVED LOGIC
              if (N2 = C1)
.              setfocus   ShipEditInfo                      .Benign        yet important OBJECT
               Deactivate NSHP001A
              elseif (N2 = C2)
.              setfocus   Ship2EditCampSearch               .Not terribly important       field
               Deactivate NSHP001B
              elseif (N2 = C3)
              endif
              return

ShipTabChange
.Redundant code, but a safety check!!!
              if (Security = 8)
               return
              endif
.START PATCH 5.1 REMOVED LOGIC
.BETA Test logic
.             reset           revtyps
.             scan            INITS,revtyps
.             if not equal
.              return
.             endif
.BETA Test logic END
.END PATCH 5.1 REMOVED LOGIC
              move            N2,TabNum
              if (N2 = C1)
               Activate NSHP001A
              elseif (N2 = C2)
               Activate NSHP001B
              elseif (N2 = C3)
              endif
              return
.PrintShipReq
PrintShipReq

.         move      C0,#howmany
          move      C0,howmany
          getitem   ShipStatLR,0,str6
          call      Trim using str6
          if (str6 <> "")
                    move      str6,NORDFLD
                    move      "Print-NORDKEY",Location
                    pack      KeyLocation,"Key: ",NORDFLD
                    call      NORDKEY
                    if over
                              alert     note,"This LR no longer exists!!",result
                    else
                              if  (OSTAT <> "0" & OSTAT <> "B")
                                        alert     Note,"Printing only allowed for Live Orders!",RESULT
                              else
                                        call      ShipDisableButtons
                                        call      Ship2SetReports
                                        setitem   ComboBoxes(1),0,RptType
                                        setitem   ComboBoxes(2),0,PrintFlag
                                        setprop Report2,visible=1
                                        getitem   ComboBoxes(1),0,RptType       
                                        getitem   ComboBoxes(2),0,PrintFlag
                                        if (RptCan <> YES)
                                                  if (PrintFlag = 2)
                                                            clock     timestamp,timestamp
                                                            call      Trim using INITS
                                                            pack      str25,timestamp,PERIOD,INITS
                                                            pack      str55,NTWKPATH3,str25
                                                  else
                                                            pack      str55,"c:\work\shipfile.dat"
                                                            move      str55,str25
                                                            erase     str55
                                                  endif
                                                  prepare   tempfile,str55
                                                  if (RptType = 1)    .First Request for Shipping Info
                                                            if (OSTAT = "0" | OSTAT = "B")    .Redundant
                                                                      move      C1,NOWNPATH
                                                                      pack      NOWNFLD,OLON
                                                                      pack      KeyLocation,"Key: ",NOWNFLD
                                                                      move      "Print-NOWNKEY",Location
                                                                      call      NOWNKEY
.Start Patch 5.4.3 Order File Fulfillment Field addition also mod from owner/fulfill assoc to list/fulfill assoc.
.                                                                     call      Trim using OWNCTN
                                                                      call      Trim using OFULLFIL
.                                                                     if (OWNCTN <> "0026")                   .Skip Triplex records!!!
                                                                      if (OFULLFIL <> "009406")               .Skip Triplex records!!!
.End Patch 5.4.3 Order File Fulfillment Field addition also mod from owner/fulfill assoc to list/fulfill assoc.
.Update Print Date field - This has to be done after writing to tempfile!!!  Otherwise all records would be marked as "2nd Request"
                                                                                move      OLRN,NSHPFLD
                                                                                move      "Print-NSHPTST",Location
                                                                                pack      KeyLocation,"Key: ",NSHPFLD
                                                                                call      NSHPKEY
                                                                                if over
.All criteria has been met - write the record
                                                                                          write     tempfile,SEQ;ORDVARS
                                                                                          unpack    timestamp,SPDATE
                                                                                          move      INITS,SPINITS
                                                                                          move      OLRN,SLRNUM
                                                                                          move      "Print-NSHPWRT",Location
                                                                                          call      NSHPWRT
                                                                                else
                                                                                          write     tempfile,SEQ;ORDVARS,SHPVARS
                                                                                          unpack    timestamp,SPDATE
                                                                                          move      INITS,SPINITS
                                                                                          move      "Print-NSHPUPD",Location
                                                                                          call      NSHPUPD
                                                                                endif
                                                                                move      C1,howmany
.                                                                               move      C1,#howmany
                                                                      else
                                                                                alert     Note,"Triplex records not allowed!",RESULT
                                                                      endif
                                                            endif
                                                  elseif (RptType = 2)
                                                  endif
                                        endif
                              endif
                    endif
          endif
          close     tempfile
.         if (#howmany = C1)            .Records were written - print them out
          if (howmany = C1)             .Records were written - print them out
.Test to see if File was actually created
                pack    APIFileName,str55,HexZero
                call    FindFirstFile
                if (APIResult <> 0 & APIResult <> hexeight) 
.Only run Batch job if file exists
                              clear     taskname
                call Get64OS
                              if (RptType = 1)
                                        if (PrintFlag = 2)
.                                                  append    "!\\Nins1\Lanbatch\batch32 -X -SC -Q\\nts0\c\lanbat~1 \\nts0\c\apps\winbatch\butil job=SHPRPT ",taskname
.                                                  IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.                                                  append    "!\\Nins1\Lanbatch\batch32 -X -SC -Q\\Nins1\ServerC \\nins1\winbatch\butil job=SHPRPT ",taskname
.                                                  else
                                                  append    "\\nins1\winbatch\butil job=SHPRPT ",taskname
.                                                  endif
                              else
.                                                 append  "!\\nts0\c\apps\winbatch\butil job=SHPRPT ",TASKNAME
.                                                  IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.                                                  append  "!\\Nins1\Lanbatch\batch32 -X -SC -Q\\Nins1\ServerC \\nins1\winbatch\butil job=SHPRPT ",TASKNAME
.                                                  else
                                                  append    "\\nins1\winbatch\butil job=SHPRPT ",taskname
.                                                  endif
                                        endif
                              elseif (RptType = 2)
                              endif
                              append  " infile=",taskname
.INPNAME is 25 bytes long, so filename for PDF version must be shortened.
.It is chained programs responsibility to add path back
                              append  str25,taskname
                              append  " P=",taskname
                              move      PrintFlag,str2
                              rep       zfill,str2
                              append  str2,taskname
                              append  " B=",taskname
                              move      portn,str3
                              rep       zfill,str3
                              append    str3,taskname
                    reset   TASKNAME
                    batch     TASKNAME
                endif
          endif
          call      ShipEnableButtons
          call      Click_ShipListView
          REturn
.end      
XRESIZE
           Nshp0001.Scale
           RETURN



              INCLUDE         NSHPIO.INC
              INCLUDE         NSHP2IO.INC
.Patch5.1.4
              INCLUDE         COMPIO.INC
              INCLUDE         CNTIO.INC
.             INCLUDE         NMLRIO.INC
.Patch5.1.4
              INCLUDE         NORDIO.INC
              INCLUDE         NCMPIO.INC
              INCLUDE         NOWNIO.INC
              INCLUDE         NRTNIO.INC
              INCLUDE         NOFRIO.INC
              include         ncntio.inc
.begin patch 5.2
.             include         ninvio.inc
              include         ninvio.inc
.end patch 5.2
.START PATCH 5.3 REMOVED LOGIC
.              INCLUDE         NFULIO.INC
.END PATCH 5.3 REMOVED LOGIC
              include         nuseio.inc
              include         npasio.inc
              include         searchio.inc     .contains logic for search.plf
.Patch5.1.4
.             include         nbrkio.inc     .Needed to load Search.plf
.Patch5.1.4
              include         ndatio.inc     .Needed to load Search.plf
.START PATCH 5.1.3A ADDED LOGIC
              INCLUDE         NSEL2IO.INC
.END PATCH 5.1.3A ADDED LOGIC
.Patch 5.4.4
                    Include       nshp3io.inc
.Patch 5.4.4
              INCLUDE         COMLOGIC.INC
