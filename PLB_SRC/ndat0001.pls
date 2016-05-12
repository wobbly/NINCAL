PC            EQU             1
              include         common.inc
              include         cons.inc
              include         ndatdd.inc
              include         nseldd.inc
              include         nowndd.inc
.START PATCH        1.2.6     REPLACED LOGIC
.              include         nfuldd.inc
.END PATCH          1.2.6     REPLACED LOGIC
.START PATCH 1.1.4 REPLACED LOGIC
.             include         nmlrdd.inc
              INCLUDE         COMPDD.inc
              INCLUDE         CNTDD.inc
.END PATCH 1.1.4 REPLACED LOGIC
              include         nxrfdd.inc
              include         nsmpdd.inc
              include         nrefdd.inc
              include         npasdd.inc
              include         npaydd.inc
              include         nmdldd.inc
              include         ntxtdd.inc
              include         nadddd.inc
              include         narrdd.inc
              include         ncatdd.inc
.             include         nmeddd.inc
              include         NSLTdd.inc
              include         nsrcdd.inc
              include         nmoddd.inc
              include         nusedd.inc
              include         gnxtdd.inc
              include         ncntdd.inc
              include         winapi.inc
              include         plbequ.inc
.begin patch 1.2.5
              include         Ntypdd.inc
.end patch 1.2.5
.begin patch 1.6
              include         NQRCDD.inc
.end patch 1.6
.START PATCH 1.1.2 ADDED LOGIC
.For Search Screen
              include         norddd.inc
.START PATCH 1.1.4 REPLACED LOGIC
.             include         nbrkdd.inc
.END PATCH 1.1.4 REPLACED LOGIC
              include         nrtndd.inc
              include         ncmpdd.inc
.END PATCH 1.1.2 ADDED LOGIC
.START PATCH 1.1.5 ADDED LOGIC
              include         nusgdd.inc
.END PATCH 1.1.5 ADDED LOGIC
.begin patch 1.5
          Include   M2Ndd.inc
.end patch 1.5
.begin patch 1.92
          Include   S2Ndd.inc
.end patch 1.92
.begin patch 1.94
          include   ntxt1dd.inc
.end patch 1.94

Release   INit      "2.09"     DLH           .if file no longer managed turn off LRA and order/Booking flags
REldate   Init      "2015 July 7"
.Release   INit      "2.08"     DLH           .fix display issue with datacombonet. it has an item #0 so the existing logic would never display the last item. at this time #16
.REldate   Init      "2015 May 22"
.Release   INit      "2.07"     DLH           .Remove Reuben
.REldate   Init      "2015 May 18"
.Release   INit      "2.06"     DLH           .additional logic flag for order program
.REldate   Init      "2014 August 1"
.Release   INit      "2.05"     DLH           .logic flags for order program
.REldate   Init      "2014 July 30"
.Release   INit      "2.04"     DLH           .logic issue ntxt1text modify/save (under save button ndat0001.plf)
.REldate   Init      "2014 July 29"
.Release   INit      "2.03"     DLH           .allow print to XLS cards
.REldate   Init      "2014 July 23"
.Release   INit      "2.02"     DLH           .set PDF as default Printer
.REldate   Init      "2014 May 2"
.Release   INit      "2.01"     DLH           .add code to display Mailer usage allowed/not on usage page in additionm to list usage
.REldate   Init      "2014 February 12"
.Release   INit      "2.00"     DLH           .add code for managed/nonmanaged change and status changed of managed file
.REldate   Init      "2013 December 20"
.Release   INit      "1.99"     DLH           .replace laser3 with laser4
.REldate   Init      "2013 December 12"
.Release   INit      "1.98"     AH           .Adjusted TYPOUT logic
.REldate   Init      "13 November 2013"
.Release   Init      "1.97"         DLH   .allow more flexibility in release of busy records
.Reldate   Init      "2013 October 18"
.Release   Init      "1.96"         DLH   .allow more flexibility in release of busy records
.Reldate   Init      "2013 September 27"
.Release   Init      "1.95"         DLH   .allow editing of ntxt1text
.Reldate   Init      "2013 August 22"
.Release   Init      "1.94"         DLH   .srds display more data  see ntxt1dd.inc
.Reldate   Init      "03 January 2013"
.Release   Init      "1.93"         DLH   .remove old "dos" delete of htm files
.Reldate   Init      "29 November 2011"
.Release   Init      "1.92"         DLH   .SRDS
.Reldate   Init      "20 jULy 2011"
.Release   Init      "1.91"         DLH   .Change in ntypdd
.Reldate   Init      "22 March 2010"
.Release   Init      "1.9"         DLH   .If available display owner contact from prog 34
.Reldate   Init      "14 January 2009"
.Release  Init      "1.8"         DLH   .Replace .dir logic
.Reldate  Init      "24 November 2008"
.Release  Init      "1.7"         DLH   .Make Logo logic for usage print match datacard
.Reldate  Init      "13 August 2008"
.Release  Init      "1.6"         DLH   .Quick reco for web
.Reldate  Init      "19 June 2008"
.Release  Init      "1.5"         11 March 2008 DLH         .Display MIN #
.Reldate  Init      "11 March 2008"
.Release  Init      "1.4"         18 January 2008 DLH       .change temp file cleanup to use .dir method and datalist
.Reldate  Init      "15 February 2008"
.Release  Init      "1.3.1"       18 January 2008 DLH       .add search to manager and Fulfillment objects
.Reldate  Init      "18 January 2008"
.Release  Init      "1.3.0"       16October2007 DLH         .PL cleanup exclusive - change from radio and message to combobox
.Reldate  Init      "16 October 2007"
.Release  Init      "1.2.9"       30March2007 DLH .PL
.Reldate  Init      "30 March 2007"
.Release  Init      "1.2.8"       24October2006 DLH         .MIn
.Release       Init                         "1.2.7"             29JUNE2006 DMS             Add search, as per 6/12/2006 CTF Meeting
.Release       Init                         "1.2.6"             20JUNE2006 DMS             fulfillment conversion
.Release      Init                          "1.2.5"        28Feb2006      DLH            start adding typist info
.;release      init                           "1.2.4"        30JAN2006                     DMB            Remove old file copy of gif no longer needed for web datacard
.release      init                           "1.2.3"        09AUG2005      ASH            SMALL PATCH TO ERADICATE RANDOM I55 ERRORS
.release      init                           "1.2.2"        18MAY2005      ASH            LISTMLR/NINMDLST Conversion
.release      init                           "1.2.1"        06APRFEB2005   ASH            Increased COMMPER
.release      init                           "1.2.0"        25FEB2005      ASH            PATCHES FOR BUG FIXES
.release      init                           "1.1.9"        02JAN2005      ASH            CHANGED USAGE CRITERIA
.release      init                           "1.1.8"        02NOV2004      ASH            SAMPLE FILE CONVERSION - INCREASED MAILER FIELD TO 6 BYTES
.release      init                           "1.1.7"        14OCT2004      ASH            PATCH ADDED TO ALLOW SALES TO MODIFY RECORDS
.release      init                           "1.1.6"        20SEP2004      ASH            USAGE SCREEN PATCH
.                                                                                         Work Order 536 Added Usage stuff for SK
.release      init                           "1.1.5"        21JUL2004      ASH            ADDED USAGE SCREEN
.                                            28JUL2004      ASH            Added Usage Print Capabilities to append to Datacard Print
.release      init                           "1.1.4"        27MAY2004      ASH            MAILER CONVERSION
.release      init                           "1.1.3"        20MAY2004      ASH            Small fixes
.                                                                          Attempt to thwart I55 errors on NDATUPD by using FilePi on UPDATABs
.release      init                           "1.1.2"        04MAY2004      ASH            Small fixes
.                                                                          Reassign focus to SearchListView when using Up/Down keys
.                                                                          Added update to Datacard Update Date/Name when modifying Reference Prices
.                                                                          Added Search logic - includes access in Print Screen as well as appropriate F2 features
.                                                                          Fine Tuned Printer options with Print button
.                                                                          Added code to protect integrity of NDATFLD!!
.                                                                          Added code to include Owner HTML file deletion
.release      init                           "1.1.1"        23APR2004      ASH            Added Search features
.release      init                           "1.1"          20APR2004      ASH            ADDED UPDATE DATE/NAME
.                                                                                         ADDED BUSY BYTE
.release      init                           "1.0"          12APR2004      ASH            INITIAL RELEASE
.EXTERNAL ROUTINES FROM       LVREORDER.PLC
.begin patch 1.4
............................Vista
.begin patch 1.8
.dlFiles  datalist
.end patch 1.8
DLresult  form 9
DLndx     form 9
dmFileName          dim 80
............................Vista
.end patch 1.4
.begin patch 2.0
statChange          Dim       1          
exclChange          Dim       1
.end patch 2.0


ReOrder external "LVReorder;Reorder"
GBBORDER external "LVReorder;GBBORDER"
.EXTERNAL ROUTINES FROM       NDAT001a.PLC
SelectTestBase external "NDAT001a;SelectTestBase"
SelectTestBase2 external "NDAT001a;SelectTestBase2"
.SelectTestBase3 external "NDAT001aA;SelectTestBase3"
SelectTestBase4 external "NDAT001a;SelectTestBase4"
SelectGetExchange external "NDAT001a;SelectGetExchange"
SelectSetExchange external "NDAT001a;SelectSetExchange"
.SelectDataInitializeListView external "NDAT001aA;DataInitializeListView"
.SelectDataLoadListView external "NDAT001aA;DataLoadListView"
.SelectDataListViewExtract external "NDAT001aA;DataListViewExtract"
SelectGetNextIndex external "NDAT001a;SelectGetNextIndex"
.START PATCH 1.1.7 ADDED LOGIC
GetDatacardExclusive external "NDAT001a;GetDatacardExclusive"
.END PATCH 1.1.7 ADDED LOGIC
.EXTERNAL ROUTINES FROM INFO.PLC
MailerLoadForm external "INFO;LoadForm"
.DataDisplayMailer external "INFO;DisplayMailer"
DataDisplayCompany external "INFO;DisplayCompany"
DataDisplayOwner external "INFO;DisplayOwner"
.begin patch 1.2.8
DataDisplayOCNT external "INFO;DisplayOCnt"
.DataDisplayPrevOwn external "INFO;DisplayPrevOwn"
.end patch 1.2.8
.MailerDisplayBroker external "INFO;DisplayBroker"
.DataDisplayShipto external "INFO;DisplayShipto"
OrderDisplayMessage external "INFO;DisplayMessage"
OrderInfoClose external "INFO;InfoClose"
.EXTERNAL ROUTINES FROM       SPELLCHECK.PLC
SpellCheck external "SPELLCHECK;SpellCheck"
CaseChange external "SPELLCHECK;CaseChange"
.EXTERNAL ROUTINES FROM       NDAT002W.PLC
CreateWebCard external "NDAT002W;CreateWebCard"
CreateDataCard external "NDAT0002;CreateDataCard"
.START PATCH 1.1.5 ADDED LOGIC
PrintUsageToExcel external "NUSG002A;PrintUsageToExcel"
UpdateListUsage external "NUSG0001;UpdateListUsage"
.END PATCH 1.1.5 ADDED LOGIC
.begin patch 2.03
CreateXLSCard         external "NDAT0005;CreateXLSCard"
.end patch 2.03
RunAs     external  "ShellExec;RunAs"

preffile file
.TempFile file
DATVARS2 dim  3002            .Used to create temporary Web Cards prior to conversion of NDAT0002

revtyps       init            "DH REH SM RW SA               "
.revtyps       init            "DH AH DM DB JD               "
NewFlag       init            "N"
NewFlag2 init "N"             .for Select Records
NewFlag3 init "N"             .for Ref Records
ReturnFlag init               "N"
ReturnFlag2 init "N"          .for Select Records
ReturnFlag3 init "N"          .for Ref Records
.START PATCH 1.1.5 ADDED LOGIC
ReturnFlag4 init "N"          .for Usage Records
.END PATCH 1.1.5 ADDED LOGIC
ExitFlag init "Y"
ExitFlag2 init "Y"            .for Select Records
ExitFlag3 init "Y"            .for Ref Records
HoldFlag init "N"
SecFlag       init            "N"            .Allows fixes - needs Password
StopFlag init "N"             .Allows break from Searching via associated Text
SelHoldKey dim 10
.RefHoldKey dim               6
RefLRFlag form 1
BaseFlag dim  1               .Used to determine if Base was changed during Update/Create
BaseFlag2 form "0"            .Used to determine if Base/Sec. Bases were changed during Update/Create - Used to flag refresh of DataComboSelectBase
HoldBase dim  4               .Used in conjunction with above to determine if Base/Sec. Bases were changed
DLV2Flag form "1"             .Used to determine which List View is currently viewed
DLVS1Flag form "1"            .Used to determine which List View is currently viewed
SelectFlag form               "0"            .Used to detemine if Currently Modifying/Creating Select Fields
RefFlag       form            "0"            .Used to detemine if Currently Modifying/Creating Ref Fields
SelViewFlag form "1"          .Used to determine which DataListViewSelect currently has focus
.
hold          dim             600            .length of Datacard record
hold2         dim             7500           .length of largest possible text record ---MIN
.hold2         dim             4500           .length of largest possible text record
taskname2 dim 200
taskname3 dim 200
N52           form            5.2
#result       form            9
.
TabNum        form            "01"
DimPtr        dim             ^
FrmPtr        form            ^
DataLVPointer ListView        ^
height        form            7.4
width         form            7.4
.begin patch 1.2.9
yesno1    integer   1,"0x000024"                yes no buttons, question Icon
.end patch  1.2.9

.Parameters for Parent Screen
.MaxHeight form               "733.0000"
MaxHeight form "804.0000"
MinHeight form "438.0000"
MaxWidth form "905.0000"
MinWidth form "642.0000"
.Parameters for Select Screen
MaxSHeight form               "600.0000"
.MinSHeight form              "160.0000"
MinSHeight form               "55.0000"
MaxSWidth form "800.0000"
MinSWidth form "483.0000"
.Parameters for ListView Object on Select Screen
.MaxLVHeight form "600.0000"
.MinLVHeight form "160.0000"
.MinLVWidth form              "480.0000"
.
NCLEAN        form            9              .Used to hold number of items for DataComboClean
NDELIV        form            9              .Used to hold number of items for DataComboDelivery
NNET          form            9              .Used to hold number of items for DataComboNet
NSAMPLE       form            9              .Used to hold number of items for DataComboSample
.Items used by Sample Screen
Preview form    1
Default form    1
Select  form    1
SMPPath dim     35
SmpPage form    5
SmpPage2 form   5
SmpFile pfile
SmpScale form   3
.START PATCH 1.1 ADDED LOGIC
HoldREVDATE dim               10
.END PATCH 1.1 ADDED LOGIC
.Vars used for Report Screen
RptCan  dim     1
ObjectColl    Collection
StatTextBoxes StatText (2)
EditTextBoxes EditText (1)
ComboBoxes    ComboBox (2)
CheckBoxes    CheckBox (3)
Buttons        Button (3)
ListViews     ListView (1)
.START PATCH 1.1.2 ADDED LOGIC
DataLists     DataList (2)
.END PATCH 1.1.2 ADDED LOGIC
.START PATCH 1.2.7 ADDED LOGIC
ReturnValue         form      1
SearchBreak      init            "N"  // quit button quits search
SearchFlag          init                "N"       // are we in search mode?
searchAlert         init                "Y"
UpdateStartJul      dim       5
UpdateEndJul        dim       5
RevisionStartJul    dim 5
RevisionEndJul      dim       5
NDATFLDBack         dim       6
NDATFLDBack2        dim       6
.END PATCH 1.2.7 ADDED LOGIC
.
filler  init    "0000"
.hexeight integer 4,"4294967295"
MouseForm form 10
T1            form            4
L1            form            4
.
VScrollBar1 VScrollBar
HScrollBar1 HScrollBar
.Vars used for floating Select Screen
SelLeft       form            9
SelTop        form            9
SelTopC       form            9
SelLeftC form 9
TempTop       form            9
TempLeft form 9
.
OptionsArr1 CONST             "10"
OptionsArr2 CONST             "5"
OptionsArrSize CONST          "5"
OptionsArray dim              OptionsArrSize(OptionsArr1,OptionsArr2)
.Screen       1
Options1Coll Collection
OptionsScreenInit ComboBox
OptionsPassInit Checkbox
.START PATCH 1.1 ADDED LOGIC
OptionsReviseInit Checkbox
.END PATCH 1.1 ADDED LOGIC
.START PATCH 1.1.5 ADDED LOGIC
.OptionsFullScreen Checkbox
.END PATCH 1.1.5 ADDED LOGIC
.Screen       2
Options2Coll Collection
OptionsScreen2CreateHTML Checkbox
OptionsScreen2DeleteHTMLStat StatText
OptionsScreen2DeleteHTML ComboBox
.Screen       3
Options3Coll Collection
.Colors
white         color
grey          color
colornum form 24
colordim dim  8
colordim2 dim 8
.begin patch 2.01
REd        Color
Black      color
.end patch 2.01
.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mSecurity menu
mHelp    menu
.Set Up       SubMenu         for Security
sSecurity submenu
.START PATCH 1.1.2 ADDED LOGIC
.Set Up SubMenu for Options
sSearch  submenu
.END PATCH 1.1.2 ADDED LOGIC

.Present Data for Menu Bar
FData   init    "&File;&Print;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
.START PATCH 1.1.2 REPLACED LOGIC
.OData   init    "&Options;&View Samples;&Preferences"
OData   init    "&Options;&View Samples;&Preferences;&Search"
.Present Data for Search SubMenu
.Begin PATCH 1.3.1 REPLACED LOGIC
SData   init    ";&List;&Mailer;&Owner;&Fulfillment"
.SData   init    ";&List;&Mailer;&Owner"
.END PATCH 1.3.1 REPLACED LOGIC
.END PATCH 1.1.2 REPLACED LOGIC
SEData        init            "Security;Security mode"
HData   init    "&Help;&About"
.Present Data for SubMenu
SESData       init            ";&Add;&Modify;&Release"

Timer   Timer
.
VT_BOOL       EQU 11
OTRUE         variant
OFALSE        variant
OBOOL         variant

RepLoop   Form      9
.Set Vars used for About Box
        move    "NDAT0001.PLS",Wprognme
        move    "Datacard File Maintenance",Wfunction
        move    "Andrew Harkins",Wauthor
        move    Release,Wrelease
.begin patch 1.2.9
.        move    "August 20, 2002",Wreldate
          Move      Reldate,Wreldate
.end patch 1.2.9
.
.begin patch xxx
Note    automation
Mes     automation      class="Outlook.Application"
.end patch xxx


.START PATCH 1.1.2 ADDED LOGIC
srch    plform  Search
.END PATCH 1.1.2 ADDED LOGIC
pass          plform          Passwrd
mss1          plform          Error
abt           plform          About
rpt           plform          Report
rpt2          plform          Report2
opt           plform          OptionsOrd

smp           plform          NORD001F
NDAT001A plform               NDAT001A
x             plform          NDAT0001
NDAT001B plform               NDAT001B
NDAT001C plform               NDAT001C
SelectForm plform NDAT001D    .Select Form
        winhide
.Load Forms, Always load parent form first
        formload x
        formload NDAT001A,NDAT0001
        formload NDAT001B,NDAT0001
        formload NDAT001C,NDAT0001
              formload SelectForm,NDAT0001
              formload pass
              formload abt
              formload mss1
              formload opt
              formload rpt2
              formload rpt
              formload smp
.START PATCH 1.1.2 ADDED LOGIC
              formload srch
.END PATCH 1.1.2 ADDED LOGIC

.Set the following property as it gets set to '5' each time form opened!!!
              setprop         DataGroupBox001,left=7

              getinfo         system,taskname
              bump            taskname,12
              move            taskname,str4
.             bump            infostring,4
.             move            infostring,str5
              call            GetSdrive      
.              call  debug
.begin patch 2.01
          create    RED=*RED
          create    black=*black
.end patch 2.01           
              if (str4 = "0640")
.              move           "60.0000",MinSHeight
              endif
              call            DataSetSelectDefault
              setprop         DataEditListNum,bgcolor=$BTNFACE
.START PATCH 1.1 ADDED LOGIC
              setprop         DataEditRevised2,bgcolor=$BTNFACE
.END PATCH 1.1 ADDED LOGIC
.
              getprop         NDAT0001,top=TempTop,left=TempLeft                          .Get Default
.Prep some stuff for the Sample Viewing form
              call            MailerLoadForm
              setmode         *GRAYSCALE=0
              pack            SMPPath,NTWKPATH1,"\samples\" ."
.
.Set up ScrollBar linking.  There is no way to access information from a ScrollBar embedded in a form, so we
.must declare another, link it, then register the Change event, and then determine values from this phantom object.
              getprop         NDAT0001,LINKVSCROLL=VScrollBar1,LINKHSCROLL=HScrollBar1
              eventreg VScrollBar1,C3,VScrollChange
              eventreg HScrollBar1,C3,HScrollChange
              setprop         VScrollBar1,Min=0,Max=20,Shift=10
              setprop         HScrollBar1,Min=0,Max=20,Shift=10
.
        DataListViewSearch.InsertColumn using "Key",0,1
        DataListViewSearch.InsertColumn using "Number",50,2
        DataListViewSearch.InsertColumn using "Name",175,3
.START PATCH 1.1.1 REPLACED LOGIC
.        DataListViewSearch.InsertColumn using "Details",0,4
.START PATCH 1.2.7 REPLACED LOGIC
.         DataListViewSearch.InsertColumn using "Revised",75,4
          DataListViewSearch.InsertColumn using "Updated",75,4
.END PATCH 1.2.7 REPLACED LOGIC
        DataListViewSearch.InsertColumn using "Status",50,5
.begin patch 1.92        .add srds number 
.begin patch 1.5        .add MIN number 
        DataListViewSearch.InsertColumn using "SRDS ##",50,6
        DataListViewSearch.InsertColumn using "Min ##",50,7
.        DataListViewSearch.InsertColumn using "Min ##",50,6
..        DataListViewSearch.InsertColumn using "Details",0,6
        DataListViewSearch.InsertColumn using "Details",0,7
.START PATCH 1.2.7 REPLACED LOGIC
...        DataListViewSearch.InsertColumnFgClr using *Index=6 // really col 7
..         DataListViewSearch.InsertColumn using "Revised",0,7
..         DataListViewSearch.InsertColumn using "Exclusive",0,8
..         DataListViewSearch.InsertColumn using "New",0,9
..         DataListViewSearch.InsertColumn using "Updated Jul",0,10
..         DataListViewSearch.InsertColumn using "Revised Jul",0,11
..         DataListViewSearch.InsertColumnFgClr using *Index=11 // really col 12

          DataListViewSearch.InsertColumn using "Revised",0,9
          DataListViewSearch.InsertColumn using "Exclusive",0,10
          DataListViewSearch.InsertColumn using "New",0,11
          DataListViewSearch.InsertColumn using "Updated Jul",0,12
          DataListViewSearch.InsertColumn using "Revised Jul",0,13
          DataListViewSearch.InsertColumnFgClr using *Index=13 // really col 14

.          DataListViewSearch.InsertColumn using "Revised",0,8
.          DataListViewSearch.InsertColumn using "Exclusive",0,9
.          DataListViewSearch.InsertColumn using "New",0,10
.          DataListViewSearch.InsertColumn using "Updated Jul",0,11
.          DataListViewSearch.InsertColumn using "Revised Jul",0,12
.          DataListViewSearch.InsertColumnFgClr using *Index=12 // really col 13
.end patch 1.5       
.END PATCH 1.2.7 REPLACED LOGIC
.
.START PATCH 1.2.7 REMOVED LOGIC
.        DataListViewSearch2.InsertColumn using "Key",0,1
.        DataListViewSearch2.InsertColumn using "Revised",75,2
.        DataListViewSearch2.InsertColumn using "Name",280,3
.        DataListViewSearch2.InsertColumn using "Number",50,4
.        DataListViewSearch2.InsertColumn using "Status",50,5
.        DataListViewSearch2.InsertColumn using "Details",0,6
.        DataListViewSearch2.InsertColumnFgClr using *Index=6
.END PATCH 1.2.7 REMOVED LOGIC
.END PATCH 1.1.1 REPLACED LOGIC
.
        DataListViewSamples.InsertColumn using "",50,1
        DataListViewSamples.InsertColumn using "",150,2
.
        DataListViewRef.InsertColumn using "",0,1
        DataListViewRef.InsertColumn using "",140,2
        DataListViewRef.InsertColumn using "",95,3
              DataListViewRef.InsertColumn using "",60,4
              DataListViewRef.InsertColumn using "Details",0,5
              DataListViewRef.SetColumnFormat using 2,1
              DataListViewRef.SetColumnFormat using 3,1
              call            LoadDataListViewRefHeaders
.
        DataListView2.InsertColumn using "Code",0,1
              DataListView2.InsertColumn using "Type",70,2
        DataListView2.InsertColumn using "Description",175,3
        DataListView2.InsertColumn using "Sub-Type",65,4
        DataListView2.InsertColumn using "Details",0,5
.
        DataListView2B.InsertColumn using "Description",175,1
              DataListView2B.InsertColumn using "Type",70,2
        DataListView2B.InsertColumn using "Sub-Type",65,3
        DataListView2B.InsertColumn using "Code",0,4
        DataListView2B.InsertColumn using "Details",0,5
.
..Create      Column Headers for HistListView
.             getprop         DataListViewSelect,*ColumnHeaders=ColHeads
..I hide      the first item as I have not yet figured out if              I can change the ForeColor of that item, since it does not appear to be    a sub-item.
.             ColHeads.Add using *Index=1,*Key="key",*Text="",*Width=0
.             ColHeads.Add using *Index=2,*Key="one",*Text="Name",*Width=200
.             ColHeads.Add using *Index=3,*Key="two",*Text="",*Width=30
.             ColHeads.Add using *Index=4,*Key="three",*Text="Qty",*Width=75,*Alignment=1
.             ColHeads.Add using *Index=5,*Key="four",*Text="Price",*Width=75,*Alignment=1
.             ColHeads.Add using *Index=6,*Key="five",*Text="Status",*Width=75
.             ColHeads.Add using *Index=7,*Key="six",*Text="Comm",*Width=50
.             ColHeads.Add using *Index=8,*Key="seven",*Text="Inactive",*Width=70
.             ColHeads.Add using *Index=9,*Key="eight",*Text="Gender",*Width=0
.             ColHeads.Add using *Index=10,*Key="nine",*Text="Other Detail",*Width=0
.             ColHeads.Add using *Index=11,*Key="ten",*Text="Qty Key",*Width=0
.             ColHeads.Add using *Index=12,*Key="eleven",*Text="Price Key",*Width=0
.             ColHeads.Add using *Index=13,*Key="twelve",*Text="Status Key",*Width=0
.             ColHeads.Add using *Index=14,*Key="thirteen",*Text="Name Key",*Width=50
.             ColHeads.Add using *Index=15,*Key="fourteen",*Text="Adj. Price",*Width=75,*Alignment=1
.
              DataSelectRecListView.InsertColumn using "Key",0,0
              DataSelectRecListView.InsertColumn using "Name",200,1
              DataSelectRecListView.InsertColumn using "Qty",75,2
              DataSelectRecListView.InsertColumn using "Price",75,3
              DataSelectRecListView.InsertColumn using "",0,4
              DataSelectRecListView.InsertColumnFgClr using *Index=5
              DataSelectRecListView.InsertColumnBgClr using *Index=6
              DataSelectRecListView.SetColumnFormat using 3,1
              DataSelectRecListView.SetColumnFormat using 4,1
.
              DataListViewSelect.InsertColumn using "Key",0,1                             .Ascending by Index Value
              DataListViewSelect.InsertColumn using "Name",200,2
              DataListViewSelect.InsertColumn using "",30,3
              DataListViewSelect.InsertColumn using "Qty",75,4
              DataListViewSelect.InsertColumn using "Price",75,5
              DataListViewSelect.InsertColumn using "Status",75,6
              DataListViewSelect.InsertColumn using "Comm",50,7
              DataListViewSelect.InsertColumn using "Inactive",70,8
              DataListViewSelect.InsertColumn using "Other Detail",0,9
              DataListViewSelect.InsertColumnFgClr using *Index=9
              DataListViewSelect.InsertColumnBgClr using *Index=10
              DataListViewSelect.SetColumnFormat using 3,1
              DataListViewSelect.SetColumnFormat using 4,1
.
              DataSelectListView.InsertColumn using "##",0,0
              DataSelectListView.InsertColumn using "Name",200,1
.
              call            ReOrder using DataListViewSelect,C9
              call            GBBORDER USING NDAT001D,DataListViewSelect
.
              DataListViewSelectB.InsertColumn using "Key",0,1                            .Ascending by Select Name
              DataListViewSelectB.InsertColumn using "Name",200,2
              DataListViewSelectB.InsertColumn using "",30,3
              DataListViewSelectB.InsertColumn using "Qty",75,4
              DataListViewSelectB.InsertColumn using "Price",75,5
              DataListViewSelectB.InsertColumn using "Status",75,6
              DataListViewSelectB.InsertColumn using "Comm",50,7
              DataListViewSelectB.InsertColumn using "Inactive",70,8
              DataListViewSelectB.InsertColumn using "Other Detail",0,9
              DataListViewSelectB.InsertColumnFgClr using *Index=9
              DataListViewSelectB.InsertColumnBgClr using *Index=10
              DataListViewSelectB.SetColumnFormat using 3,1
              DataListViewSelectB.SetColumnFormat using 4,1
.
.             call            ReOrder using DataListViewSelectB,C9
.             call            GBBORDER USING NDAT001D,DataListViewSelectB
.
              DataListViewSelectC.InsertColumn using "Key",0,1                            .Descending by Quantity
              DataListViewSelectC.InsertColumn using "Name",200,2
              DataListViewSelectC.InsertColumn using "",30,3
              DataListViewSelectC.InsertColumn using "Qty",75,4
              DataListViewSelectC.InsertColumn using "Price",75,5
              DataListViewSelectC.InsertColumn using "Status",75,6
              DataListViewSelectC.InsertColumn using "Comm",50,7
              DataListViewSelectC.InsertColumn using "Inactive",70,8
              DataListViewSelectC.InsertColumn using "Other Detail",0,9
              DataListViewSelectC.InsertColumnFgClr using *Index=9
              DataListViewSelectC.InsertColumnBgClr using *Index=10
              DataListViewSelectC.SetColumnFormat using 3,1
              DataListViewSelectC.SetColumnFormat using 4,1
.
.             call            ReOrder using DataListViewSelectC,C9
.             call            GBBORDER USING NDAT001D,DataListViewSelectC
.
              DataListViewSelectD.InsertColumn using "Key",0,1                            .Ascending by Price
              DataListViewSelectD.InsertColumn using "Name",200,2
              DataListViewSelectD.InsertColumn using "",30,3
              DataListViewSelectD.InsertColumn using "Qty",75,4
              DataListViewSelectD.InsertColumn using "Price",75,5
              DataListViewSelectD.InsertColumn using "Status",75,6
              DataListViewSelectD.InsertColumn using "Comm",50,7
              DataListViewSelectD.InsertColumn using "Inactive",70,8
              DataListViewSelectD.InsertColumn using "Other Detail",0,9
              DataListViewSelectD.InsertColumnFgClr using *Index=9
              DataListViewSelectD.InsertColumnBgClr using *Index=10
              DataListViewSelectD.SetColumnFormat using 3,1
              DataListViewSelectD.SetColumnFormat using 4,1
.
.             call            ReOrder using DataListViewSelectD,C9
.             call            GBBORDER USING NDAT001D,DataListViewSelectD
.
              DataListViewSelectE.InsertColumn using "Key",0,1                            .Ascending by Status
              DataListViewSelectE.InsertColumn using "Name",200,2
              DataListViewSelectE.InsertColumn using "",30,3
              DataListViewSelectE.InsertColumn using "Qty",75,4
              DataListViewSelectE.InsertColumn using "Price",75,5
              DataListViewSelectE.InsertColumn using "Status",75,6
              DataListViewSelectE.InsertColumn using "Comm",50,7
              DataListViewSelectE.InsertColumn using "Inactive",70,8
              DataListViewSelectE.InsertColumn using "Other Detail",0,9
              DataListViewSelectE.InsertColumnFgClr using *Index=9
              DataListViewSelectE.InsertColumnBgClr using *Index=10
              DataListViewSelectE.SetColumnFormat using 3,1
              DataListViewSelectE.SetColumnFormat using 4,1
.
.             call            ReOrder using DataListViewSelectE,C9
.             call            GBBORDER USING NDAT001D,DataListViewSelectE

.Load Reference Code/Descriptions into appropriate Objects
.Prep the ComboBoxes
              deleteitem DataComboClean,0
              deleteitem DataComboDelivery,0
              deleteitem DataComboNet,0
              deleteitem DataComboSample,0
              move            C0,NCLEAN
              move            C0,NDELIV
              move            C0,NNET
              move            C0,NSAMPLE
              insertitem DataComboClean,NCLEAN,B1
              insertitem DataComboDelivery,NDELIV,B1
              insertitem DataComboNet,NNET,B1
              insertitem DataComboSample,NSAMPLE,B1
              loop
               call           NREFSEQ
               until over
               pack           str4,NREFCODE,NREFNUM
               if (NREFCODE <> "C" & NREFCODE <> "D" & NREFCODE <> "N" & NREFCODE <> "P")
                              pack           taskname,NREFVARS
                       DataListView2.InsertItem giving N9 using str4
                              clear          str15
                              if (NREFCODE = "A")           .Addressing
                                             move           "Addressing",str25
                              elseif (NREFCODE = "L")       .Selection
                                             move           "Selection",str25
.                             elseif (NREFCODE = "M")       .Media
.                                            move           "Media",str25
                              elseif (NREFCODE = "R")       .Arrangement
                                             move           "Arrangement",str25
                              elseif (NREFCODE = "S")       .Source
                                             move           "Source",str25
                              elseif (NREFCODE = "T")       .Category
                                             move           "Category",str25
                                             unpack         NREFNUM,str1
                                             if (str1 = "B")
                                                            move           "Business",str15
                                             elseif (str1 = "C")
                                                            move           "Consumer",str15
                                             elseif (str1 = "E")
                                                            move           "Enhanced",str15
                                             endif
                              endif
                              DataListView2.SetItemText using N9,str25,1
                              call           Trim using NREFDESC
                              DataListView2.SetItemText using N9,NREFDESC,2
                              DataListView2.SetItemText using N9,str15,3
                              DataListView2.SetItemText using N9,taskname,4
.
                       DataListView2B.InsertItem giving N9 using NREFDESC
                              DataListView2B.SetItemText using N9,str25,1
                              DataListView2B.SetItemText using N9,str15,2
                              DataListView2B.SetItemText using N9,str4,3
                              DataListView2B.SetItemText using N9,taskname,4
               else
                              call          debug
                              pack           nrefdesc using nrefdesc,b55                  .pad it out   
                              pack           taskname,NREFDESC,str4
                              if (NREFCODE = "C")
                                             add            C1,NCLEAN
                                             insertitem DataComboClean,NCLEAN,taskname
                              elseif (NREFCODE = "D")
                                             add            C1,NDELIV
                                             insertitem DataComboDelivery,NDELIV,taskname
                              elseif (NREFCODE = "N")
                                             add            C1,NNET
                                             insertitem DataComboNet,NNET,taskname
                              elseif (NREFCODE = "P")
                                             add            C1,NSAMPLE
                                             insertitem DataComboSample,NSAMPLE,taskname
                              endif
               endif
              repeat
.begin patch 1.6
            Data3ListViewQrec.InsertColumn using "Key",0,1                            .Ascending by Status
            Data3ListViewQrec.InsertColumn using "Category",200,2
            
            Data3ListView001.InsertColumn using "Key",0,1                            .Ascending by Status
              Data3ListView001.InsertColumn using "Category",200,2
.         call      debug
              loop
               call           NQRCDSEQ
               until over
               Data3ListView001.insertitem giving n9 using NQRCDNum
               Data3ListView001.setitemText using N9,NQRCDdesc,1               
               repeat
.end patch 1.6
              
.Insert a blank item at the end
              DataListView2.InsertItem giving N9 using "ZZZZ"
              DataListView2B.InsertItem giving N9 using ""
.START PATCH 1.1.5 ADDED LOGIC
              Data3ListView.InsertColumn using "Mailer",200,0                             .Ascending by Mailer Name
              Data3ListView.InsertColumn using "",50,1
              Data3ListView.InsertColumn using "Date",75,2
              Data3ListView.InsertColumn using "Type",75,3
              Data3ListView.InsertColumn using "Initials",50,4
              Data3ListView.InsertColumn using "",150,5
              Data3ListView.InsertColumn using "",0,6
              Data3ListView.InsertColumnFgClr using *Index=7
              Data3ListView.InsertColumnBgClr using *Index=8
.END PATCH 1.1.5 ADDED LOGIC
.Create       Colors for EditText Inquiry
              create          white=*white
              create          grey=220:220:220
.Create/Activate Objects on OptionsOrd.plf
.First - reset Tab Items on Options
              setprop OptionsTabControl,TabLabel="1;2;3"
.Screen       1
              create          Options;OptionsScreenInit=80:100:40:255,"",";O)pen on Detail 1;)Open on Datacard;)Open on Usage",zorder=100
              create          Options;OptionsPassInit=60:80:40:305,"Skip Password Check at Program Execution",zorder=100
.START PATCH 1.1 REPLACED LOGIC
.             listins         Options1Coll,OptionsScreenInit,OptionsPassInit
              create          Options;OptionsReviseInit=100:120:40:305,"Prompt Revision Date Update with Select Changes",zorder=100
.START PATCH 1.1.5 REPLACED LOGIC
              listins         Options1Coll,OptionsScreenInit,OptionsPassInit,OptionsReviseInit
.             create          Options;OptionsFullScreen=120:140:40:305,"Open to Full Screen",zorder=100
.             listins         Options1Coll,OptionsScreenInit,OptionsPassInit,OptionsReviseInit,OptionsFullScreen
.END PATCH 1.1.5 REPLACED LOGIC
.END PATCH 1.1 REPLACED LOGIC
              setprop         Options1Coll,visible=1        .always        start with the first tab visible

.Screen       2
              create          Options;OptionsScreen2CreateHTML=60:90:40:250,"Automatic HTML Creation of Non-Exclusive Records",zorder=100
              create          Options;OptionsScreen2DeleteHTMLStat=100:120:40:255,"Delete Temporary HTML Files","'>MS Sans Serif'(8)"
              create          Options;OptionsScreen2DeleteHTML=120:140:40:255,"",";W)eekly;)Daily;)Each time program is opened;)Monthly",zorder=100
              listins         Options2Coll,OptionsScreen2CreateHTML,OptionsScreen2DeleteHTMLStat,OptionsScreen2DeleteHTML

.Screen       3
.              create          Options;OptionsScreen6Init=80:100:40:250,"Open             Program        on Campaign Screen",zorder=100
.              listins Options3Coll,OptionsScreen6Init
.
              create          TIMER,18000     .30 minutes
              activate TIMER,Timeout,RESULT
.Create Menu Items
        create  NDAT0001;mFile,FData
        create  NDAT0001;mEdit,EData,mFile
        create  NDAT0001;mOptions,OData,mEdit
              reset           revtyps
              scan            INITS,revtyps
              if equal
               create         NDAT0001;mSecurity,SEData,mOptions
               create         NDAT0001;mHelp,HData,mSecurity
.Create       SubMenu
               create         NDAT0001;sSecurity,SESData,mSecurity,1
              else
               create         NDAT0001;mHelp,HData,mOptions
              endif
.START PATCH 1.1.2 ADDED LOGIC
.Create SubMenus
        create  NDAT0001;sSearch,SData,mOptions,3
.END PATCH 1.1.2 ADDED LOGIC
.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
        activate mEdit,EditGo,result
        activate mOptions,OptionsGo,result
              reset           revtyps
              scan            INITS,revtyps
              if equal
               activate mSecurity,SecurityGo,result
.Activate SubMenus
               activate sSecurity,SubSecurityGo,#result
              endif
        activate mHelp,HelpGo,result
.START PATCH 1.1.2 ADDED LOGIC
        activate sSearch,SearchGo,result
.END PATCH 1.1.2 ADDED LOGIC
.
              call            DataDisableLower
              call            DataDisableSelectFields
              setprop         DataEditSelectRec,bgcolor=grey
.             call            DataDisableSelectButtons1
              call            DataDisableSelectButtons2
              call            DataDisableRefFields
.START PATCH 1.1.5 ADDED LOGIC
              call            Data3DisableLower
.START PATCH 1.1.9 REPLACED LOGIC
.             pack            hold2,"Usage Determined by Following Criteria:",newline,newline,"  - Live/Billed Order in Last 12 months with either a minimum Quantity of 50K OR with 'Entire' box checked.",newline,"  - 2 or more Live/Billed Orders in Last 12 months.",newline,"  - Live/Billed Order in last 6 months AND Live/Billed Order in Last 18 months.",newline,newline,"**If the most recent Live/Billed Order is a Test, Usage is excluded."
              pack            hold2,"Usage Determined by Following Criteria:",newline,newline,"For List Universes up to 20,000",newline,"  - 1 Live/Billed Order in Last 12 months & 1 Additional Live/Billed Order in Last 18 months.",newline,newline,"For List Universes over 20,000",newline,"  - 1 Live/Billed Order w/Qty >= 5,000 in Last 12 months & 2 Additional Live/Billed Orders w/Qty >= 5,000 in Last 18 months.",newline,"  - 1 Live/Billed Order w/Qty >= 10,000 in Last 12 months & 1 Additional Live/Billed Order w/Qty >= 5,000 in Last 18 months.",newline,"  - 1 Live/Billed Order w/Qty >= 50,000 in Last 12 months & 1 Additional Live/Billed Order w/Qty >= 10,000 in Last 24 months."
.END PATCH 1.1.9 REPLACED LOGIC
              setitem         Data3EditUsageText,0,hold2
              setprop         Data3EditUsageText,bgcolor=$BTNFACE
.END PATCH 1.1.5 ADDED LOGIC
.Open Preferences File
openpref
              pack            APIFileName,"c:\program files\nincal\ndat0001.pre",hexzero
              call            FindFirstFile
              if (APIResult <> 0 & APIResult <> hexeight)
.               trap           Preferror if IO
               open           preffile,"c:\program files\nincal\ndat0001.pre"
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
               setitem        OptionsScreenInit,0,N5
.
               move           OptionsArray(1,2),str5
               call           Trim using str5
               move           str5,N5
               setitem        OptionsPassInit,0,N5
.START PATCH 1.1 ADDED LOGIC
               move           OptionsArray(1,3),str5
               call           Trim using str5
               move           str5,N5
               setitem        OptionsReviseInit,0,N5
.END PATCH 1.1 ADDED LOGIC
.START PATCH 1.1.5 ADDED LOGIC
.              move           OptionsArray(1,4),str5
.              call           Trim using str5
.              move           str5,N5
.              setitem        OptionsFullScreen,0,N5
.END PATCH 1.1.5 ADDED LOGIC
.Screen       2
               move           OptionsArray(2,1),str5
               call           Trim using str5
               move           str5,N5
               setitem        OptionsScreen2CreateHTML,0,N5
.
               move           OptionsArray(2,2),str5
               call           Trim using str5
               move           str5,N5
               setitem        OptionsScreen2DeleteHTML,0,N5
.Screen       3
              endif
.
              move            C1,NDATPATH
              move            C3,NDATLOCK
.
              Data2WebBrowser.Navigate2 USING "about:blank"
              clock           timestamp,timestamp
.Delete all previously created HTML Files, according to Preferences
. 1  = Weekly
. 2  = Daily
. 3  = Each time program is opened
. 4  = Monthly
              getitem         OptionsScreen2DeleteHTML,0,N5
.Find out system information first
              call            GetWinVer
.>Patch 1.2.4 Comment out Code               - gif no longer used
.             clear           taskname
.             Path            Exist,"c:\windows"
.             if over                                       .nt/2000
.              append         "!c:\winnt\system32\cmd.exe /c ",taskname
.             elseif (osflag = c6)           .XP
.              append         "!c:\windows\system32\cmd.exe /c ",taskname
.             else                                          .95/98
.              append         "!c:\command.com /c ",taskname
.             endif
.             append          "xcopy \\nts2\c\http\Datacards\data.gif c:\work\ /y",taskname
.             reset           taskname
.             batch           taskname
.>Patch 1.2.4 Comment out Code               - gif no longer used
              call            GetSdrive                     .get system drive      
              clear           taskname
              pack            Str35 from SysDrive,"\Windows"      
.              Path            Exist,"c:\windows"
              Path            Exist,str35
.              if over                                       .nt/2000
..               append         "!c:\winnt\system32\cmd.exe /c ",taskname
.               Append         "!",taskname
.               append         Sysdrive,Taskname
.               append         "\winnt\system32\cmd.exe /c ",taskname
.              elseif          (osflag = c6)           .XP
.               Append         "!",taskname
.               append         Sysdrive,Taskname
.               append         "\windows\system32\cmd.exe /c ",taskname
.              else                                          .95/98
.               Append         "!",taskname
.               append         Sysdrive,Taskname
.               append         "\command.com /c ",taskname
.              endif
                    call      testclient
.              if (N5 = 3)                .delete all files everythine program loads
.          call      Debug
.                    call      testclient
.                    if        (ClntServFlag = c1)                         .we are using the client
.                    append         "del !c:\work\data*.HTM",taskname
.                    Else
.                    append         "del c:\work\data*.HTM",taskname
.                    Endif
.               reset          taskname
.               batch          taskname
.START PATCH 1.1.2 ADDED LOGIC
.          if        (ClntServFlag = c1)                         .we are using the client
.          append         "!del c:\work\own*.HTM",taskname
.          Else
.          append         "del c:\work\own*.HTM",taskname
.          endif
.          reset          taskname
.          batch          taskname
.END PATCH 1.1.2 ADDED LOGIC
.              else            .Extract the file date from each record
               unpack         timestamp,CC,YY,MM,DD
               call           CVTJUL
               move           JULDAYS,result
..begin patch 1.4

.               append         "dir c:\work\data*.HTM > c:\work\datatemp.dat",taskname
.               reset          taskname
..If osflag = c7 must do api call execute does not have enough rights
.         if        (Osflag <> c7)         
.               execute        taskname
.               else
.api here
.begin patch 1.8
.          call      Debug
          clear     Mailbody
          if        (ClntServFlag = c1)                         .we are using the client
          FIndDIr   "!c:\work\data*.htm",MailBody,Itemcount=n5
          Else
          FIndDIr   "c:\work\data*.htm",MailBody,Itemcount=n5
          endif
          if        (n5 > c0)
          FOr       n4 from c0 to N5
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    bump      DmFileName,c1
                    if        (ClntServFlag = c1)                         .we are using the client
                    pack      taskname from "!C:\work\",DmFIleName  ."comment :)
                    Else
                    pack      taskname from "C:\work\",DmFIleName  ."comment :)
                    endif
                              if (N5 = 3)                .delete all files everythine program loads
                              erase          taskname
                              Else
                              FINDFILE  Taskname,WRITE=Str25
                              unpack    str25,CC,YY,MM,DD
                              call      cvtjul
                              move      Juldays,Howmany
                              calc      N9=(result-howmany)
                                        if (N9 >= 1)
                                        erase          taskname
                                        endif
                              endif
                    endif
          repeat
          endif
.         create dlFiles=1:10:1:10,visible=0
.         dlFiles.Dir giving DLresult using *Filespec="c:\work\data*.htm":
.                               *Flags=0x0000
..        call    runas using taskname,N1
...mmm api not working could also do Dir method with filefind
...temp workaround for testing on VM/Vista
..        Prepare   TEmpfile,"c:\work\datatemp.dat"
..        write     Tempfile,Seq;str1
..        weof      tempfile,seq
..               endif
.
..               close          TempFile
..               open           TempFile,"c:\work\datatemp.dat"
..               loop
.         for DLndx from 0 to DLresult
.         dlFiles.GetText giving dmFileName using *Index=DLndx
.         clear     taskname
.         pack      taskname from "c:\work\",DmFIleName              ."
.         
.         FINDFILE Taskname,WRITE=Str25
.         unpack    str25,CC,YY,MM,DD
.         call      cvtjul
.         move      Juldays,Howmany
.               calc           N9=(result-howmany)
.                              if (N9 >= 1)
.                              erase          taskname
.                              endif
.         repeat
.         Destroy   DLFIles
.end patch 1.8
.                              read           TempFile,seq;MM,str8,str10,str10,str9,str25
.                              until over
.                              type           MM
.                              if equal
.                                             unpack         str8,str1,DD,str1,CC,YY
.                                             call           CVTJUL
.                                             move           JULDAYS,howmany
.                                             calc           N9=(result-howmany)
.. 1  = Weekly
.. 2  = Daily
.. 4  = Monthly
..                                            if ((N5 = 1 & N9 > 7) OR (N5 = 2 & N9 >= 1) OR (N5 = 4 & N9 > 31))
.                                             if (N9 >= 1)
.                                                            call           Trim using str25
.                                                            pack           str35,"c:\work\",str25
.                                                            erase          str35
.                                             endif
.                              endif
.               repeat
.               close          TempFile
.               erase          "c:\work\datatemp.dat"
.START PATCH 1.1.2 ADDED LOGIC
.begin patch 1.8
.          call      Debug
          clear     Mailbody
            if        (ClntServFlag = c1)                         .we are using the client
            FIndDIr   "!c:\work\own*.htm",MailBody,Itemcount=n5
           eLSE
            FIndDIr   "c:\work\own*.htm",MailBody,Itemcount=n5
          ENDIF
          if        (n5 > c0)
          FOr       n4 from c0 to N5
          explode   MailBody,"|",Dmfilename 
          match     "f",Dmfilename 

                    if        equal

                    clear     taskname
                    bump      DmFileName,c1
                    if        (ClntServFlag = c1)                         .we are using the client
                    pack      taskname from "!C:\work\",DmFIleName  ."comment :)
                    ELSE
                    pack      taskname from "C:\work\",DmFIleName  ."comment :)
                    endif
                              if (N5 = 3)                .delete all files everythine program loads
                              erase          taskname
                              else
                              FINDFILE  Taskname,WRITE=Str25
                              unpack    str25,CC,YY,MM,DD
                              call      cvtjul
                              move      Juldays,Howmany
                              calc      N9=(result-howmany)
                                        if (N9 >= 1)
                                        erase          taskname
                                        endif
                              endif
                    endif
          repeat
          endif

.         create dlFiles=1:10:1:10,visible=0
.         dlFiles.Dir giving DLresult using *Filespec="c:\work\own*.htm":
.                               *Flags=0x0000
.         for DLndx from 0 to DLresult
.         dlFiles.GetText giving dmFileName using *Index=DLndx
.         clear     taskname
.         pack      taskname from "c:\work\",DmFIleName              ."
.         
.         FINDFILE Taskname,WRITE=Str25
.         unpack    str25,CC,YY,MM,DD
.         call      cvtjul
.         move      Juldays,Howmany
.               calc           N9=(result-howmany)
.                              if (N9 >= 1)
.                              erase          taskname
.                              endif
.         repeat
.         Destroy   DLFIles
.end patch 1.8
.               append         "dir c:\work\own*.HTM > c:\work\datatemp.dat",taskname
.               reset          taskname
..If osflag = c7 must do api call execute does not have enough rights
.         if        (Osflag <> c7)         
.               execute        taskname
.               else
..api here

.         call    runas using taskname,N1
..mmm api not working could also do Dir method with filefind
..temp workaround for testing on VM/Vista
.         Prepare   TEmpfile,"c:\work\datatemp.dat"
.         write     Tempfile,Seq;str1
.         weof      tempfile,seq
.               endif
..
.               close          TempFile
.               open           TempFile,"c:\work\datatemp.dat"
.               loop
.                              read           TempFile,seq;MM,str8,str10,str10,str9,str25
.                              until over
.                              type           MM
.                              if equal
.                                             unpack         str8,str1,DD,str1,CC,YY
.                                             call           CVTJUL
.                                             move           JULDAYS,howmany
.                                             calc           N9=(result-howmany)
.. 1  = Weekly
.. 2  = Daily
.. 4  = Monthly
..                                            if ((N5 = 1 & N9 > 7) OR (N5 = 2 & N9 >= 1) OR (N5 = 4 & N9 > 31))
.                                             if (N9 >= 1)
.                                                            call           Trim using str25
.                                                            pack           str35,"c:\work\",str25
.                                                            erase          str35
.                                             endif
..                              endif
.               repeat
.               close          TempFile
.               erase          "c:\work\datatemp.dat"
.end patch 1.4
.END PATCH 1.1.2 ADDED LOGIC
.this one
.              endif
.
              move            C0,NUSEFLD
              move            C1,NUSEPATH
              move            PORTN,NUSEFLD
              rep             zfill,NUSEFLD
              call            NUSEKEY
.
              move            "D",progcode
              setprop         SelectForm,visible=1
.
              setitem         DataCheckFree,0,1             .Default is 'On'
              setfocus DataSearchList
              getitem         OptionsScreenInit,0,N5
              if (N5 = 3)
               call           DataSwitchTab using C3
              elseif (N5 = 2)
               call           DataSwitchTab using C2
              elseif (N5 = 1)
               call           DataSwitchTab using C1
              else
               call           DataSwitchTab using C1
              endif
              getitem         OptionsPassInit,0,N5
              if (N5 = 0)
.Verify Password if necessary
               setitem        PasswordEdit,0,""
               setfocus PasswordEdit
               setprop        Passwrd,visible=1
.Test for  Password
               unpack  NPASFLD,str1,NPASKEY
               pack    NPASFLD,progcode,NPASKEY
               reset   NPASFLD
               call    NPASKEY
               if not over
                              move    YES,HoldFlag
                              alert   note,"Password Accepted!",result
               endif
              endif
.START PATCH 1.1.5 ADDED LOGIC
.             getitem         OptionsFullScreen,0,N5
.             if (N5 = 1)
.Initialize to Full Screen
.              setprop        NDAT0001,height=MaxHeight
.              setprop        NDAT0001,width=MaxWidth
.              call           Resize_NDAT001D
.             endif
.END PATCH 1.1.5 ADDED LOGIC
.Test validity of PORTN
              move            C0,N3
              move            PORTN,N3
              compare         C0,N3
              if equal
               clock          PORT,str3
.bytes 1-2 are 0-99
.byte  3   is the hundreds field
               unpack         str3,str2,str1
               pack           str3,str1,str2
               move           str3,PORTN
              endif
.
           EVENTREG  X, 17, XRESIZE

              loop
               waitevent
                setitem timer,0,18000   .reset to 30 minutes
              repeat

Timeout
              if (ExitFlag = NO)
               call           Click_DataQuit
              endif
              if (ExitFlag2 = NO)
               call           Click_DataSelectQuit
              endif
              if (ExitFlag3 = NO)
               call           Click_DataRefQuit
              endif
        beep
        beep
        beep
        winshow
        stop

FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2
FileGo1
              call            DataSetPrintOptions using C0
FileGo1A
              setprop         Report2,visible=1
              if (RptCan = NO)
               pack           APIFileName,"c:\work\dataprnt.lst",hexzero
               call           FindFirstFile
               if (APIResult <> 0 & APIResult <> hexeight)
                              call           SetMouseBusy
                              move           "c:\work\dataprnt.lst",str25
.begin patch 2.03
.                              if (N1 = 3)  .PDF
                              if (N1 = 3 or N1 = 4)  .PDF or excel
.end patch 2.03
.                                            pack           taskname,"PDF is not yet an option.",newline,"Your Datacards will print via Laser 2!"
.                                            alert          note,taskname,result
.                                            move           C1,N1
.                                            call           CreateDataCard using str25,INITS,CNTNAME,N1
                                             move           PORTN,NCNTFLD1
                                             rep            zfill,NCNTFLD1
                                             if (NCNTFLD1 = "000")
                                                            move           ":::",NCNTFLD1
                                             endif
                                             move           C3,NCNTPATH
                                             move           "NCNTKEY",Location
                                             pack           KeyLocation,"Key: ",NCNTFLD1
                                             call           NCNTKEY
                                             if over
                                                            pack           taskname,"I have lost track of who you are.",newline,"Your Datacards will print via Laser 4!"
                                                            alert          note,taskname,result
                                                            move           C1,N1
                                             endif
.begin patch 1.29
.begin patch 2.03
                                 if         (n1 = 3)         .PDF
                                                 call           CreateDataCard using str25,INITS,CNTNAME,N1,N2,N3,company
                                 Elseif     (n1 = 4)         .excel
                                                 call  debug
                                             call           CreateXlsCard using str25,INITS,CNTNAME,N2,N3,company
                                 endif
.end patch 2.03

.START PATCH 1.1.5 REPLACED LOGIC
.                                            call           CreateDataCard using str25,INITS,CNTNAME,N1,N2
.                                             call           CreateDataCard using str25,INITS,CNTNAME,N1,N2,N3
.END PATCH 1.1.5 REPLACED LOGIC

                              else
.START PATCH 1.1.5 REPLACED LOGIC
.                                            call           CreateDataCard using str25,INITS,CNTNAME,N1,N2
.                                             call           CreateDataCard using str25,INITS,CNTNAME,N1,N2,N3
                                             call           CreateDataCard using str25,INITS,CNTNAME,N1,N2,N3,company
.end patch 1.29
.END PATCH 1.1.5 REPLACED LOGIC
.DimPtr  = FileName
.DimPtr1 = Initials
.DimPtr2 = LogIn Name
.FrmPtr  = Printer Choice
                              endif
.                             batch          "\\nins1\e\apps\plb\code\plbwin.exe \\nins1\e\apps\plb\code\nord0045"
                              call           SetMouseFree
               endif
              endif
              return

FileGo2
              if (ExitFlag = "Y" & ExitFlag2 = "Y" & ExitFlag3 = "Y")
               winshow
               stop
              endif
              RETURN
OptionsGo
              branch result to OptionsGo1,OptionsGo2
OptionsGo1
              getitem         DataEditMlrNum,0,str6
.START PATCH 1.1.8 ADDED LOGIC - TEMPORARY PATCH
.START PATCH 1.2.2 REMOVED LOGIC
.             pack            COMPFLD3,str6
.             move            "OptionsGo1-COMPKEY3",Location
.             pack            KeyLocation,"Key: ",COMPFLD3
.             call            COMPKEY3
.             if over
.              clear          COMPNUM
.             endif
.             move            COMPNUM,str6
.END PATCH 1.2.2 REMOVED LOGIC
.END PATCH 1.1.8 ADDED LOGIC - TEMPORARY PATCH
              setitem         SamplesEditMailer,0,str6
.
              DataListViewSamples.GetNextItem giving N9 using C2
              DataListViewSamples.GetItemText giving str3 using N9,0
              setitem         SamplesEditSample,0,str3
.
              Activate NORD001F
              if (str6 <> "" AND str3 <> "")
               goto Click_SamplesOK
              endif
              return
OptionsGo2
              setprop         Options,visible=1
              return
.START PATCH 1.1.2 ADDED LOGIC
SearchGo
.        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4
.begin patch 1.31
.        branch  result to SearchGo2,SearchGo3,SearchGo6
        branch  result to SearchGo2,SearchGo3,SearchGo6,SearchGo7
.end patch 1.31
SearchGo1
.BROKER - not an option with this program
        return
SearchGo2
.LIST
        move    C2,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo3 Routine
.MAILER
              move            C3,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchGo4
.SHIP-TO - not an option with this program
        return
SearchGo5
.CAMPAIGN - not an option with this program
        return
SearchGo6
.OWNER
              move            C6,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
.begin patch 1.31
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
.        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6
.end patch 1.31
SearchLoad1
.BROKER - not an option with this program
        return
SearchLoad2
.LIST
              getprop         Report2,visible=N6
              if (N6 = C1)
               unpack         Srchstr,str6
               setitem        EditTextBoxes(1),0,str6
               return
              endif
.START PATCH 1.1.6 ADDED LOGIC
              getprop         Data3EditListU,enabled=N6
              if (N6 = 1)     .Creating New Usage record
               unpack         Srchstr,str6
               setitem        Data3EditListU,0,str6
               call           LostFocus_Data3EditListU
               return
              endif
.END PATCH 1.1.6 ADDED LOGIC
        unpack  Srchstr,str6,str1,str35
        getprop DataEditListNum,readonly=result
        if (result = 0)
                setitem DataEditListNum,0,str6
                setitem DataEditListName,0,str35
                setfocus DataEditListNum
        endif
        return
SearchLoad3
.MAILER
.START PATCH 1.2.2 REPLACED LOGIC
..START PATCH 1.1.6 ADDED LOGIC
.             getprop         Data3EditMlrU,enabled=N6
.             if (N6 = 1)     .Creating New Usage record
.              unpack         Srchstr,str4
.              setitem        Data3EditMlrU,0,str4
.              call           LostFocus_Data3EditMlrU
.              return
.             endif
..END PATCH 1.1.6 ADDED LOGIC
.             unpack          Srchstr,str4,str1,str3,str1,str45
.        getprop DataEditMlrNum,enabled=result
.        if (result = 1)
.                setitem DataEditMlrNum,0,str4
.                setitem DataStatMlrName,0,str45
.                setfocus DataEditMlrNum
.        endif
              unpack  Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
              getprop         Data3EditMlrU,enabled=N6
              if (N6 = 1)     .Creating New Usage record
               setitem        Data3EditMlrU,0,str6
               call           LostFocus_Data3EditMlrU
               return
              endif
.
        getprop DataEditMlrNum,enabled=result
        if (result = 1)
                setitem DataEditMlrNum,0,str6
                setitem DataStatMlrName,0,str45
                setfocus DataEditMlrNum
        endif
.END PATCH 1.2.2 REPLACED LOGIC
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
SearchLoad5
.CAMPAIGN - not an option with this program
        return
SearchLoad6
.OWNER - 
              getprop         DataEditOwnNum,enabled=result
              if (result = C1)
               unpack         Srchstr,str4,str1,str25
               setitem        DataEditOwnNum,0,str4
               setitem        DataStatOwnName,0,str25
               setfocus DataEditOwnNum
              endif
        return
.END PATCH 1.1.2 ADDED LOGIC
.begin patch 1.31
SearchLoad7
.Fulfillment - 
              getprop         DataEditManagerNum,enabled=result
              if (result = C1)
               unpack         Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
               setitem        DataEditManagerNum,0,str6
               setitem        DataStatManagerName,0,str45
               setfocus DataEditManagerNum
              endif
        return
.end patch 1.31

OptionsWritePref
.Pull values from OptionsOrd.plf
.Screen1
.Screen       1
              getitem         OptionsScreenInit,0,N5
              move            N5,OptionsArray(1,1)
              getitem         OptionsPassInit,0,N5
              move            N5,OptionsArray(1,2)
.START PATCH 1.1 ADDED LOGIC
              getitem         OptionsReviseInit,0,N5
              move            N5,OptionsArray(1,3)
.END PATCH 1.1 ADDED LOGIC
.START PATCH 1.1.5 ADDED LOGIC
.             getitem         OptionsFullScreen,0,N5
.             move            N5,OptionsArray(1,4)
.END PATCH 1.1.5 ADDED LOGIC
.Screen       2
              getitem         OptionsScreen2CreateHTML,0,N5
              move            N5,OptionsArray(2,1)
.
              getitem         OptionsScreen2DeleteHTML,0,N5
              move            N5,OptionsArray(2,2)
.Screen       3
.Write to the file
              pack            APIFileName,"c:\program files\nincal\ndat0001.pre",hexzero
              call            DeleteFile
              prep            preffile,"c:\program files\nincal\ndat0001.pre"
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

EditGo
HelpGo
              setprop AboutMssg,visible=1
              return

SecurityGo
              return
SubSecurityGo
.begin patch 1.96
.if user is DH or RW or SM and they have already entered password allow release with
.Verify Password if necessary
.          if        (holdflag <> yes & (NPASUSER = "RHOLLAND" | NPASUSER = "DAVID H." | NPASUSER ="SMcGuire" | NPASUSER ="RWhiting")& #result = c3)
          if        (holdflag <> yes & (NPASUSER = "DAVID H." | NPASUSER ="SMcGuire" | NPASUSER ="RWhiting")& #result = c3)
               setitem        PasswordEdit,0,""
               setfocus PasswordEdit
               setprop        Passwrd,visible=1
.Test for  Password
               unpack  NPASFLD,str1,NPASKEY
               pack    NPASFLD,progcode,NPASKEY
               reset   NPASFLD
               call    NPASKEY
               if not over
                              move    YES,HoldFlag
                              alert   note,"Password Accepted!",result
                              move            YES,SecFlag
                              move            YES,HoldFlag
                              branch          #result to SubSecurityGo1,SubSecurityGo2,SubSecurityGo3
               endif
.          Elseif      (holdflag = yes & (NPASUSER = "RHOLLAND" | NPASUSER = "DAVID H." | NPASUSER ="SMcGuire" | NPASUSER ="RWhiting" )& #result = c3)
          Elseif      (holdflag = yes & (NPASUSER = "DAVID H." | NPASUSER ="SMcGuire" | NPASUSER ="RWhiting" )& #result = c3)
              move            YES,SecFlag
              move            YES,HoldFlag
              branch          #result to SubSecurityGo1,SubSecurityGo2,SubSecurityGo3
          endif                                        
.end patch 1.96

              move            "(",progcode
              move            NO,SecFlag
              pack            str55,"               To Enter Security mode"
              setitem         PasswordStatMssg1,0,str55
              setprop         PasswordStatMssg1,visible=1
              setitem         PasswordEdit,0,""
              setfocus PasswordEdit
              clear           NPASFLD
              setprop         Passwrd,visible=1
              if (NPASFLD <> "(COSMO")
               return
              endif
              alert           note,"Security mode permitted!",result
              move            YES,SecFlag
              move            YES,HoldFlag
              branch          #result to SubSecurityGo1,SubSecurityGo2,SubSecurityGo3
SubSecurityGo1
              call            Click_DataNew
              call            DataEnableLowerSecurity
              return

SubSecurityGo2
              call            StartModify
              call            DataEnableLowerSecurity
              return

SubSecurityGo3
              move            "Security-NDATRELEASE",Location
              pack            KeyLocation,"Key: ",NDATFLD
              call            NDATRELEASE
              return
.moved to comlogic
.SetMouseBusy
.              setmode         *mcursor=*wait
.              return
.SetMouseFree
.              setmode         *mcursor=*arrow
.              return

Report2DestroyObjects
              destroy         ObjectColl
              return

DataSetPrintOptions Routine FrmPtr
.DimPtr  = Current List Number
.Allows       selection of Datacards for printing
.Called       by File menu & Print Button
              call            Report2DestroyObjects
.START PATCH 1.1.2 ADDED LOGIC
              move    PORTN,NCNTFLD1
              rep     zfill,NCNTFLD1
              move    C3,NCNTPATH
              move    "Print-NCNTKEY",Location
              pack    KeyLocation,"Key: ",NCNTFLD1
              call    NCNTKEY
.END PATCH 1.1.2 ADDED LOGIC
              setprop         Report2,title="NIN Datacard Print Screen"
              move            NO,RptCan
              create          Report2;StatTextBoxes(1)=25:45:5:65,"List Number",""
              create          Report2;EditTextBoxes(1)=25:45:65:135,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
              create          Report2;Buttons(2)=25:45:135:160,"+"
              create          Report2;Buttons(3)=25:45:160:185,"-"
              create          Report2;StatTextBoxes(2)=25:45:190:225,"Printer",""
.begin patch 2.03
.              create  Report2;ComboBoxes(1)=25:45:225:325,"",";L)aser 3;)Laser 4;)PDF"
              create  Report2;ComboBoxes(1)=25:45:225:325,"",";L)aser 3;)Laser 4;)PDF;Excel"
.end patch 2.03
.START PATCH 1.1.5 REPLACED LOGIC
.             create  Report2;CheckBoxes(1)=45:65:190:325,"Include Owner Info"
              create  Report2;CheckBoxes(1)=45:65:5:125,"Include Owner Info"
              create  Report2;CheckBoxes(2)=45:65:135:240,"Append Usage"
              create  Report2;CheckBoxes(3)=45:65:245:325,"Duplex"
.END PATCH 1.1.5 REPLACED LOGIC
              create          Report2;ListViews(1)=65:170:5:320,MultiSelect=1,HideColHdr=1,FullRow=1
              create          Report2;Buttons(1)=205:230:50:100,"O&K",zorder=500,default=1
              activate StatTextBoxes(1)
              activate StatTextBoxes(2)
.START PATCH 1.1.2 ADDED LOGIC
.When dynamically creating an EditTextBox, you are only     given three default events: GotFocus,LostFocus,LostFocus+Change.
.Any other events must be registered manually.
.Below we register a KeyPress event.
              eventreg EditTextBoxes(1),10,DataSetListKeyPress,RESULT=N9
.END PATCH 1.1.2 ADDED LOGIC
              activate EditTextBoxes(1),DataSetListEditChange,result
.Register LostFocus Event on EditTextBoxes so that corresponding "+/-" Buttons will have default
              eventreg EditTextBoxes(1),9,DataSetButton2
              activate Buttons(1),DataSetPrintOK,result
              activate Buttons(2),DataSetPrintAdd,result
              activate Buttons(3),DataSetPrintRemove,result
              activate ComboBoxes(1)
              activate CheckBoxes(1)
.START PATCH 1.1.5 ADDED LOGIC
              activate CheckBoxes(2)
              activate CheckBoxes(3)
.END PATCH 1.1.5 ADDED LOGIC
.START PATCH 1.1.2 ADDED LOGIC
.begin patch 2.02   .default PDF
               setitem        ComboBoxes(1),0,3
.              if (CNTPRINT = 2)              .Laser 3 Default
.               setitem        ComboBoxes(1),0,2
.              endif
.end patch 2.02
.END PATCH 1.1.2 ADDED LOGIC
.Register LostFocus Event on ListViews so that corresponding "+/-" Buttons will have default
              eventreg ListViews(1),9,DataSetButton3
.Register LostFocus Event on ListViews so that OK Button will have default
              eventreg ListViews(1),11,DataSetButton4
.Register Double-Click Event on ListViews
              eventreg ListViews(1),6,DataSetPrintRemove
              activate ListViews(1)
              ListViews(1).InsertColumn using "",50,0
              ListViews(1).InsertColumn using "",250,1
              ListViews(1).InsertColumn using "",0,2
.START PATCH 1.1.5 REPLACED LOGIC
.             listins         ObjectColl,StatTextBoxes(1),StatTextBoxes(2),EditTextBoxes(1),Buttons(1),Buttons(2),Buttons(3):
.              ComboBoxes(1),CheckBoxes(1),ListViews(1)
              listins         ObjectColl,StatTextBoxes(1),StatTextBoxes(2),EditTextBoxes(1),Buttons(1),Buttons(2),Buttons(3):
               ComboBoxes(1),CheckBoxes(1),CheckBoxes(2),CheckBoxes(3),ListViews(1)
.END PATCH 1.1.5 REPLACED LOGIC
              setfocus EditTextBoxes(1)
              if (FrmPtr = 1)
               move           SEQ,result
               move           result,N9
               DataListViewSearch.GetNextItem giving result using C2,N9
               if (result <> SEQ)
                              move           SEQ,N10
                              loop
                                             move           N10,N9
                                             DataListViewSearch.GetNextItem giving N10 using C2,N9
                                             until (N10 = SEQ)
                                             DataListViewSearch.GetItemText giving str6 using N10,1
                                             call           Trim using str6
                                             if (str6 <> "")
                                                            setitem        EditTextBoxes(1),0,str6
                                                            call           DataSetPrintAdd
                                             endif
                              repeat
               endif
              endif
              return
DataSetListEditChange
              if (result = C2)               .Lost Focus + Change
               getitem        EditTextBoxes(1),0,str6
               call           Trim using str6
               move           C0,N6
               move           str6,N6
               move           N6,str6
               rep            zfill,str6
              endif
              return
DataSetButton2
.Got Focus
              setprop         Buttons(2),default=1
              return
DataSetButton3
.Got Focus
              setprop         Buttons(3),default=1
              return
DataSetButton4
.Lost Focus
              setprop         Buttons(1),default=1
              return
DataSetPrintOK
              call            DataSetPrintAdd
.
              getitem         ComboBoxes(1),0,N1
.START PATCH 1.1.2 ADDED LOGIC
.1 = Laser3
.2 = Laser4
.3 = PDF
.begin patch 2.03
.4 = Excel
.end patch 2.03
              if (N1 < 3)
               if (N1 = 1)    .Laser3
                              move           "Laser3",str10
               else                          .Laser4
                              move           "Laser4",str10
               endif
.
               move           C0,result
               destroy        DataLists(1)
               create  DataLists(1)=1:1:1:1
               activate DataLists(1)
               getinfo        printers,Datalists(1)
               move    "0",howmany
               DataLists(1).GetCount giving howmany
               if (howmany > 0)
                              for N10,"1",howmany
                                             getitem DataLists(1),N10,taskname
                                             scan           str10,taskname
                                             if equal
                                                            move           C1,result
                                                            break
                                             endif
                              repeat
               endif
               destroy        DataLists(1)
               if (result = C0)
                              pack           taskname,"Printer '",str10,"'is not available on this machine!",newline,"Select another."
                              alert          note,taskname,result
                              setfocus ComboBoxes(1)
                              return
               endif
              endif
.END PATCH 1.1.2 ADDED LOGIC
              getitem         CheckBoxes(1),0,N2
.START PATCH 1.1.5 ADDED LOGIC
              getitem         CheckBoxes(2),0,N3
              if (N3 = 1)
               getitem        CheckBoxes(3),0,N4
               if (N4 = 1)
                              move           C2,N3
               endif
              endif
.END PATCH 1.1.5 ADDED LOGIC
.
              close           tempfile
              erase           "c:\work\dataprnt.lst"
              prepare         tempfile,"c:\work\dataprnt.lst"
              move            SEQ,result
              loop
               move           result,N9
               ListViews(1).GetNextItem giving result using 0,N9
               until (result = SEQ)
               ListViews(1).GetItemText giving hold using result,2
.              call           Trim using hold                              .Extra Precaution
               unpack         hold,DATVARS
               call           Trim using LSTNUM
               if (LSTNUM <> "")
                              write          tempfile,SEQ;hold
               endif
              repeat
              close           tempfile
.
              setprop         Report2,visible=0
              return
DataSetPrintAdd
              getitem         EditTextBoxes(1),0,str6
              call            Trim using str6
              if (str6 <> "")
               call           ZFillIt using str6
               packkey        NDATFLD,str6
               move           "DSPAdd-NDATKEY",Location
               pack           KeyLocation,"Key: ",NDATFLD
               move           C1,NDATPATH
               call           NDATKEY
               if not over
                              ListViews(1).GetItemCount giving howmany
                              sub            C1,howmany
                              for result,C0,howmany
                                             ListViews(1).GetItemText giving str6 using result
                                             if (str6 = NDATFLD)
                                                            return
                                             endif
                              repeat
                              ListViews(1).InsertItem giving N9 using NDATFLD,9999
                              ListViews(1).SetItemText using N9,OLSTNAME,1
                              pack           hold,DATVARS
                              ListViews(1).SetItemText using N9,hold,2
               endif
              endif
              setitem         EditTextBoxes(1),0,""
              setfocus EditTextBoxes(1)
              return
DataSetPrintRemove
.Test first to make sure there are records highlighted
              move            SEQ,result
              move            result,N9
              ListViews(1).GetNextItem giving result using C2,N9
              if (result = SEQ)
               return
              endif
              move            SEQ,result
              loop
               move           result,N9
               ListViews(1).GetNextItem giving result using C2,N9
               until (result = SEQ)
               ListViews(1).DeleteItem using result
               sub            C1,result
              repeat
              return
.START PATCH 1.1.2 ADDED LOGIC
DataSetListKeyPress
              if (N9 = 113) .F2 Key calls Search Function
.Virtual Key Value
               goto SearchGo2
              elseif (N9 = 120)     .F9 Key closes Search Function
               setprop        Search,visible=0
              endif
              return
.END PATCH 1.1.2 ADDED LOGIC
..............................................................................
DataEnableUpper
              move            NO,NewFlag
              move            NO,SecFlag
              setprop         DataOK,enabled=1
              setprop         DataNew,enabled=1
              setprop         DataListViewSearch,enabled=1
.START PATCH 1.2.7 REMOVED LOGIC
..START PATCH 1.1.1 ADDED LOGIC
.              setprop         DataListViewSearch2,enabled=1
..END PATCH 1.1.1 ADDED LOGIC
.END PATCH 1.2.7 REMOVED LOGIC
              setprop         DataPrint,enabled=1
.START PATCH 1.2.7 REPLACED LOGIC
.             setprop         DataSearch,enabled=1
                    setprop   DataSearch,enabled=1
.END PATCH 1.2.7 REPLACED LOGIC
              if (ExitFlag2 = YES)
               if (ExitFlag3 = YES)
                              setprop        DataExit,enabled=1
               endif
               call           DataEnableSelectButtons
              endif
              if (ExitFlag3 = YES)
               call           DataEnableRefButtons
              endif
              move            YES,ExitFlag
              return

DataEnableUpperButtons
              setprop         DataSave,enabled=1
              setprop         DataQuit,enabled=1
              return

DataDisableUpper
              move            NO,ExitFlag
              setprop         DataExit,enabled=0
              setprop         DataOK,enabled=0
              setprop         DataNew,enabled=0
              setprop         DataModify,enabled=0
              setprop         DataListViewSearch,enabled=0
.START PATCH 1.2.7 REMOVED LOGIC
..START PATCH 1.1.1 ADDED LOGIC
.              setprop         DataListViewSearch2,enabled=0
..END PATCH 1.1.1 ADDED LOGIC
.END PATCH 1.2.7 REMOVED LOGIC
.             setprop         DataPrint,enabled=0
              setprop         DataSearch,enabled=0
DataDisableUpperButtons
              setprop         DataSave,enabled=0
              setprop         DataQuit,enabled=0
              return

DataEnableLower
.begin patch 2.05
               setProp         DataCheckBlocked,enabled=1,bgcolor=white
              setProp         DataCheckCounts,enabled=1,bgcolor=White
              setPRop         DataCheckGender,enabled=1,bgcolor=white
.begin patch 2.06
              setPRop         DataCheckmlrRestrictions,enabled=1,bgcolor=white
.end patch 2.06
.end patch 2.05
              setprop         DataComboStatus,enabled=1,bgcolor=white
.              setprop         DataCheckExclusive,enabled=1
          SetProp   DataExclComboBOx,Enabled=1              
              setprop         DataCheckNew,enabled=1
              setprop         DataEditMListName,enabled=1,bgcolor=white
              setprop         DataEditListName,enabled=1,bgcolor=white
              setprop         DataEditOwnNum,enabled=1,bgcolor=white
              setprop         DataEditManagerNum,enabled=0,bgcolor=white
              setprop         DataEditFulfillNum,enabled=1,bgcolor=white
              setprop         DataEditMlrNum,enabled=1,bgcolor=white
              setprop         DataEditPayNum,enabled=0,bgcolor=white
              setprop         DataEditDate,bgcolor=white
              setprop         DataEditRevised,enabled=1,bgcolor=white
.START PATCH 1.1 ADDED LOGIC
              setprop         DataEditUpdate,enabled=1,bgcolor=white
.END PATCH 1.1 ADDED LOGIC
              setprop         DataEditUniverse,enabled=1,bgcolor=white
          if        (ndatfem > 0 or Ndatmen > 0 or Sex = "")
              setprop         DataEditGender,enabled=0,bgcolor=grey
              setprop         DataEditGender,visible=0
              setprop         DataEditFem,enabled=1,bgcolor=white
              setprop         DataEditFem,visible=1
              setprop         DataEditMen,enabled=1,bgcolor=white
              setprop         DataEditMen,visible=1
              setprop         DataStatTextFem,enabled=1,bgcolor=grey
              setprop         DataStatTextFem,visible=1
              setprop         DataStatTextMen,enabled=1,bgcolor=grey
              setprop         DataStatTextMen,visible=1
          else
              setprop         DataEditFem,enabled=0,bgcolor=white
              setprop         DataEditFem,visible=0
              setprop         DataEditMen,enabled=0,bgcolor=white
              setprop         DataEditMen,visible=0
              setprop         DataStatTextFem,enabled=0,bgcolor=grey
              setprop         DataStatTextFem,visible=0
              setprop         DataStatTextMen,enabled=0,bgcolor=grey
              setprop         DataStatTextMen,visible=0
              setprop         DataEditGender,enabled=1,bgcolor=white
          endif
              
              setprop         DataEditMinimum,enabled=1,bgcolor=white
              setprop         DataEditCommission,enabled=1,bgcolor=white
              setprop         DataCheckHotline,enabled=1
.             setprop         DataComboExcRent,enabled=1,bgcolor=white
              setprop         DataCheckWeb,enabled=1
              setprop         DataCheckOffice,enabled=1
              setprop         DataCheckConverted,enabled=1
.START PATCH 1.1.5 ADDED LOGIC
              setprop         DataCheckUsage,enabled=1
.END PATCH 1.1.5 ADDED LOGIC
.START PATCH 1.9.7 ADDED LOGIC
              setprop         DataCheckLRA,enabled=1
.END PATCH 1.9.78 ADDED LOGIC
              setprop         DataEditText1,readonly=0,bgcolor=white
              setprop         DataEditTextText,readonly=0,bgcolor=white
              setprop         DataEditTextUnitData,readonly=0,bgcolor=white
              setprop         DataComboDelivery,enabled=1,bgcolor=white
              setprop         DataComboSample,enabled=1,bgcolor=white
              setprop         DataComboNet,enabled=1,bgcolor=white
              setprop         DataEditNet,enabled=1,bgcolor=white
              setprop         DataComboClean,enabled=1,bgcolor=white
              setprop         DataEditClean,enabled=1,bgcolor=white
              return
.START PATCH 1.2.7 ADDED LOGIC
SearchEnableLower
                setprop             DataListViewSearch, enabled=0  // new
                setprop         DataCheckWeb,visible=0
                setprop         DataEditRevised2,visible=0
                setprop         DataCheckConverted,visible=0
                setprop         DataCheckUsage,visible=0
.START PATCH 1.9.7 ADDED LOGIC
              setprop         DataCheckLRA,visible=0
                setprop         DataStatUpdate2,visible=0
                setprop         DataStatDate,visible=0
                setprop         DataEditDate,visible=0
                setprop         DataComboExcRent,visible=0
                setprop             DataCheckFree, enabled=0
                setprop             DataCheckNotes,enabled=0
                setprop             DataEditRevised, enabled=1,bgcolor=white  // new
                setprop             DataEditUpdate,enabled=1,bgcolor=white // new
                setprop             DataComboStatus,enabled=1,bgcolor=white    // dave
                setprop         NDAT001aStatUpdatedEnd,visible=1
                setprop         NDAT001aEditUpdatedEnd,visible=1
                setprop         NDAT001aStatRevisedEnd,visible=1
                setprop         NDAT001aEditRevisedEnd,visible=1
.               setprop             DataCheckExclusive,enabled=1
                setprop             DataExclComboBox,enabled=1
                setprop             DataCheckNew,enabled=1
                return
SearchDisableLower
                setprop             DataListViewSearch, enabled=1  // new
                setprop         DataCheckWeb,visible=1
                setprop         DataEditRevised2,visible=1
                setprop         DataCheckConverted,visible=1
                setprop         DataCheckUsage,visible=1
.START PATCH 1.9.7 ADDED LOGIC
              setprop         DataCheckLRA,visible=1
                setprop         DataStatUpdate2,visible=1
                setprop         DataStatDate,visible=1
                setprop         DataEditDate,visible=1
                setprop         DataComboExcRent,visible=1
                setprop             DataCheckFree, enabled=1
                setprop             DataCheckNotes,enabled=1
                setprop             DataEditRevised, enabled=0,bgcolor=grey  // new
                setprop             DataEditUpdate,enabled=0,bgcolor=grey // new
                setprop             DataComboStatus,enabled=0,bgcolor=grey
                setprop         NDAT001aStatUpdatedEnd,visible=0
                setprop         NDAT001aEditUpdatedEnd,visible=0
                setprop         NDAT001aStatRevisedEnd,visible=0
                setprop         NDAT001aEditRevisedEnd,visible=0
.               setprop             DataCheckExclusive,enabled=0
                setprop             DataExclComboBox,enabled=0
                setprop             DataCheckNew,enabled=0
               // setprop               NDAT0001ButtonRetrieve, enabled=0
                return
SearchEnableList
          setprop DataListViewSearch, enabled=1
          return
.
SearchDisableList
          setprop DataListViewSearch, enabled=0
          return
SearchClearRecord
          setitem DataCheckNew,0,c0
.         setitem DataCheckExclusive,0,c0
          setitem DataExclComboBox,0,c0
          setitem DataComboStatus,0,c1
          setitem DataEditUpdate,0,""
          setitem NDAT001aEditUpdatedEnd,0,""
          setitem DataEditRevised,0,""
          setitem NDAT001aEditRevisedEnd,0,""
          clear RevisionStartJul
          clear RevisionEndJul
          clear UpdateStartJul
          clear UpdateEndJul
          return
OnSearchButtons
          setprop DataQuit, enabled=1
          setprop DataSearch, enabled=0
          setprop DataNew, enabled=0
          setprop DataPrint, enabled=0
          setprop DataOk, enabled=0
          setprop DataModify, enabled=0
          setprop DataSearchList, enabled=0, bgcolor=grey
          return
.
OffSearchButtons
          setprop DataSearch, enabled=1
          setprop DataNew, enabled=1
          setprop DataModify, enabled=1
          setprop DataPrint, enabled=1
          setprop DataOk, enabled=1
          setprop DataSearchList, enabled=1, bgcolor=white
          setprop DataQuit, enabled=0
          setprop NDAT0001ButtonRetrieve, enabled=0
          return
.
ClearNDATFLDs
          CLEAR     NDATFLD
          CLEAR     NDATFLD1
          CLEAR     NDATFLD2
          CLEAR     NDATFLD3
          CLEAR     NDATFLD4
          clear     NDATFLD5
          clear     NDATFLD6
          clear     NDATFLD7

          return
.END PATCH 1.2.7 ADDED LOGIC
.DataGetListIts
.             getprop         DataListViewSelect,*ListItems=ListIts
.             return

DataSetLVPointer Routine FrmPtr
              if (FrmPtr = 1)
               moveaddr DataListViewSelect,DataLVPointer
              elseif (FrmPtr = 2)
               moveaddr DataListViewSelectB,DataLVPointer
              elseif (FrmPtr = 3)
               moveaddr DataListViewSelectC,DataLVPointer
              elseif (FrmPtr = 4)
               moveaddr DataListViewSelectD,DataLVPointer
              elseif (FrmPtr = 5)
               moveaddr DataListViewSelectE,DataLVPointer
              endif
              return

DataEnableLowerSecurity
.called       by SecurityGo - used to        enable ALL fields for modification
              setprop         DataEditListNum,readonly=0
              setprop         DataEditDate,enabled=1,bgcolor=white
.START PATCH 1.1 ADDED LOGIC
              setprop         DataEditRevised2,readonly=0
.END PATCH 1.1 ADDED LOGIC
              return
.
DataDisableLower
.begin patch 2.05
               setProp         DataCheckBlocked,enabled=0,bgcolor=grey
              setProp         DataCheckCounts,enabled=0,bgcolor=grey
              setPRop         DataCheckGender,enabled=0,bgcolor=grey
.begin patch 2.06
              setPRop         DataCheckmlrRestrictions,enabled=0,bgcolor=grey
.end patch 2.06

.end patch 2.05
              setprop         DataComboStatus,enabled=0,bgcolor=grey
.              setprop         DataCheckExclusive,enabled=0
              setprop         DataExclComboBox,enabled=0
              setprop         DataCheckNew,enabled=0
              setprop         DataEditListNum,readonly=1
              setprop         DataEditMListName,enabled=0,bgcolor=grey
              setprop         DataEditListName,enabled=0,bgcolor=grey
              setprop         DataEditOwnNum,enabled=0,bgcolor=grey
              setprop         DataEditManagerNum,enabled=0,bgcolor=grey
              setprop         DataEditFulfillNum,enabled=0,bgcolor=grey
              setprop         DataEditMlrNum,enabled=0,bgcolor=grey
              setprop         DataEditPayNum,enabled=0,bgcolor=grey
              setprop         DataEditDate,enabled=0,bgcolor=grey
              setprop         DataEditRevised,enabled=0,bgcolor=grey
.START PATCH 1.1 ADDED LOGIC
              setprop         DataEditUpdate,enabled=0,bgcolor=grey
              setprop         DataEditRevised2,readonly=1
.END PATCH 1.1 ADDED LOGIC
              setprop         DataEditUniverse,enabled=0,bgcolor=grey
              setprop         DataEditGender,enabled=0,bgcolor=grey
              setprop         DataEditFem,enabled=0,bgcolor=grey
              setprop         DataEditFem,visible=0
              setprop         DataEditMen,enabled=0,bgcolor=grey
              setprop         DataEditMen,visible=0
              setprop         DataStatTextFem,enabled=0,bgcolor=grey
              setprop         DataStatTextFem,visible=0
              setprop         DataStatTextMen,enabled=0,bgcolor=grey
              setprop         DataStatTextMen,visible=0
              
              setprop         DataEditMinimum,enabled=0,bgcolor=grey
              setprop         DataEditCommission,enabled=0,bgcolor=grey
              setprop         DataCheckHotline,enabled=0
              setprop         DataComboExcRent,enabled=0,bgcolor=grey
              setprop         DataCheckWeb,enabled=0
              setprop         DataCheckOffice,enabled=0
              setprop         DataCheckConverted,enabled=0
.START PATCH 1.1.5 ADDED LOGIC
              setprop         DataCheckUsage,enabled=0
.END PATCH 1.1.5 ADDED LOGIC
.START PATCH 1.9.7 ADDED LOGIC
              setprop         DataCheckLRA,enabled=0
              setprop         DataEditText1,readonly=1,bgcolor=grey
              setprop         DataEditTextText,readonly=1,bgcolor=grey
              setprop         DataEditTextUnitData,readonly=1,bgcolor=grey
              setprop         DataComboDelivery,enabled=0,bgcolor=grey
              setprop         DataComboSample,enabled=0,bgcolor=grey
              setprop         DataComboNet,enabled=0,bgcolor=grey
              setprop         DataEditNet,enabled=0,bgcolor=grey
              setprop         DataComboClean,enabled=0,bgcolor=grey
              setprop         DataEditClean,enabled=0,bgcolor=grey
              return

DataEnableSelectFields
              setprop         DataSelectArrow,enabled=1
              setprop         DataSelectListView,enabled=1,bgcolor=white
              setprop         DataEditSelectList,enabled=1,bgcolor=white
              setprop         DataEditSelectNum,enabled=1,bgcolor=white
              setprop         DataEditSelectName,enabled=1,bgcolor=white
              setprop         DataEditSelectQty,enabled=1,bgcolor=white
              setprop         DataEditSelectPrice,enabled=1,bgcolor=white
.drewtest
              setprop         DataEditSelectIndex,enabled=1,bgcolor=white
.drewtest
              setprop         DataComboSelectBase,enabled=1,bgcolor=white
              setprop         DataComboSelectModifier,enabled=1,bgcolor=white
              setprop         DataComboSelectStatus,enabled=1,bgcolor=white
              setprop         DataComboSelectExcRent,enabled=1,bgcolor=white
              setprop         DataCheckSelectComm,enabled=1
              setprop         DataCheckSelectInactive,enabled=1
              setprop         DataCheckSelectNotes,enabled=1
              return

DataDisableSelectFields
              setprop         DataSelectArrow,enabled=0
              setprop         DataSelectListView,enabled=0,bgcolor=grey,height=0
              setprop         DataEditSelectList,enabled=0,bgcolor=grey,readonly=1
              setprop         DataEditSelectNum,enabled=0,bgcolor=grey
              setprop         DataEditSelectName,enabled=0,bgcolor=grey
              setprop         DataEditSelectQty,enabled=0,bgcolor=grey
              setprop         DataEditSelectPrice,enabled=0,bgcolor=grey
.drewtest
              setprop         DataEditSelectIndex,enabled=0,bgcolor=grey
.drewtest
              setprop         DataComboSelectBase,enabled=0,bgcolor=grey
              setprop         DataComboSelectModifier,enabled=0,bgcolor=grey
              setprop         DataComboSelectStatus,enabled=0,bgcolor=grey
              setprop         DataComboSelectExcRent,enabled=0,bgcolor=grey
              setprop         DataCheckSelectComm,enabled=0
              setprop         DataCheckSelectInactive,enabled=0
              setprop         DataCheckSelectNotes,enabled=0
              return

DataEnableSelectButtons
.Setup Select Interface
              DataListViewSelect.GetItemCount giving result
              if (result > 0)
               call           DataEnableSelectButtons1
              else
               call           DataEnableSelectButtons1A
              endif
              return

DataEnableSelectButtons1
              setprop         DataSelectModify,enabled=1
DataEnableSelectButtons1A
              setprop         DataSelectNew,enabled=1
              setprop         DataEditSelectRec,enabled=1,bgcolor=white
              setprop         DataSelectRecArrow,enabled=1
              setprop         DataListViewSelect,enabled=1
              setprop         DataListViewSelectB,enabled=1
              setprop         DataListViewSelectC,enabled=1
              setprop         DataListViewSelectD,enabled=1
              setprop         DataListViewSelectE,enabled=1
              setprop         DataIndex,enabled=1
              if (ExitFlag = YES)
               if (ExitFlag3 = YES)
                              setprop        DataExit,enabled=1
               endif
              endif
.START PATCH 1.1.5 REPLACED LOGIC
.             setprop         DataOK,enabled=1
              getprop         DataNew,enabled=result
              if (result = 1)
               setprop        DataOK,enabled=1
               setprop        DataListViewSearch,enabled=1
              endif
.END PATCH 1.1.5 REPLACED LOGIC
              move            YES,ExitFlag2
              return

DataEnableSelectButtons2
              setprop         DataSelectDelete,enabled=1
DataEnableSelectButtons2A
              setprop         DataSelectQuit,default=0
              setprop         DataSelectSave,default=0
              setprop         DataSelectQuit,enabled=1
              setprop         DataSelectSave,enabled=1
              setprop         DataEditSelectRec,enabled=0,bgcolor=grey
              setprop         DataSelectRecArrow,enabled=0
              setprop         DataListViewSelect,enabled=0
              setprop         DataListViewSelectB,enabled=0
              setprop         DataListViewSelectC,enabled=0
              setprop         DataListViewSelectD,enabled=0
              setprop         DataListViewSelectE,enabled=0
              setprop         DataIndex,enabled=0
              return

DataDisableSelectButtons1
.             setprop         DataOK,enabled=0
.START PATCH 1.1.5 ADDED LOGIC
              setprop         DataOK,enabled=0
              setprop         DataListViewSearch,enabled=0
.END PATCH 1.1.5 ADDED LOGIC
              setprop         DataExit,enabled=0
              setprop         DataSelectNew,enabled=0
              setprop         DataSelectModify,enabled=0
              move            C1,SelectFlag
              move            NO,ExitFlag2
              return

DataDisableSelectButtons2
              setprop         DataSelectDelete,enabled=0
              setprop         DataSelectQuit,enabled=0
              setprop         DataSelectSave,enabled=0
              move            C0,SelectFlag
              return

DataEnableRefFields
              if (DLV2Flag = 1)
               call           Click_DataListView2
              else
               call           Click_DataListView2B
              endif
.             setprop         DataComboRefModifier,enabled=1,bgcolor=white
              setprop         DataEditRefList,enabled=1,bgcolor=white
.             setprop         DataEditRefPrice,enabled=1,bgcolor=white
.             setprop         DataEditRefQty,enabled=1,bgcolor=white
.             setprop         DataEditRefPer,enabled=1,bgcolor=white
              return

DataDisableRefFields
              setprop         DataComboRefModifier,enabled=0,bgcolor=grey
              setprop         DataEditRefList,enabled=0,bgcolor=grey,readonly=1
              setprop         DataEditRefPrice,enabled=0,bgcolor=grey
              setprop         DataEditRefQty,enabled=0,bgcolor=grey
              setprop         DataEditRefPer,enabled=0,bgcolor=grey
              return

DataEnableRefButtons
.Setup Ref Interface
              DataListViewRef.GetItemCount giving result
              if (result > 5)
               call           DataEnableRefButtons1
              else
               call           DataEnableRefButtons1A
              endif
              return

DataEnableRefButtons1
              setprop         DataRefModify,enabled=1
DataEnableRefButtons1A
              setprop         DataRefNew,enabled=1
              setprop         DataListViewRef,enabled=1
              if (ExitFlag = YES)
               if (ExitFlag2 = YES)
                              setprop        DataExit,enabled=1
               endif
              endif
.             setprop         DataOK,enabled=1
.START PATCH 1.1.5 ADDED LOGIC
              getprop         DataNew,enabled=result
              if (result = 1)
               setprop        DataOK,enabled=1
               setprop        DataListViewSearch,enabled=1
              endif
.END PATCH 1.1.5 ADDED LOGIC
              move            YES,ExitFlag3
              return

DataEnableRefButtons2
              setprop         DataRefDelete,enabled=1,height=20
              setprop         DataListView2,enabled=0
              setprop         DataListView2B,enabled=0
DataEnableRefButtons2A
              setprop         DataRefQuit,enabled=1
              setprop         DataRefSave,enabled=1
              setprop         DataListViewRef,enabled=0
              move            C1,RefFlag
              return

DataDisableRefButtons1
.             setprop         DataOK,enabled=0
.START PATCH 1.1.5 ADDED LOGIC
              setprop         DataOK,enabled=0
              setprop         DataListViewSearch,enabled=0
.END PATCH 1.1.5 ADDED LOGIC
              setprop         DataExit,enabled=0
              setprop         DataRefNew,enabled=0
              setprop         DataRefModify,enabled=0
              move            NO,ExitFlag3
              return

DataDisableRefButtons2
              setprop         DataRefDelete,enabled=0,height=0
              setprop         DataRefQuit,enabled=0
              setprop         DataRefSave,enabled=0
              setprop         DataListView2,enabled=0
              setprop         DataListView2B,enabled=0
              move            C0,RefFlag
              return

.START PATCH 1.1.5 ADDED LOGIC
Data3DisableLower
              setprop         Data3EditListU,enabled=0,bgcolor=grey
              setprop         Data3EditMlrU,enabled=0,bgcolor=grey
              setprop         Data3EditDateU,enabled=0,bgcolor=grey
              setprop         Data3ComboCodeU,enabled=0
              setprop         Data3Quit,enabled=0
              setprop         Data3Save,enabled=0
              setprop         Data3New,enabled=1
.START PATCH 1.1.5 ADDED LOGIC
              reset           revtyps
              scan            INITS,revtyps
              if equal
               setprop        Data3Calc,enabled=1
              endif
.END PATCH 1.1.5 ADDED LOGIC
              setprop         Data3Delete,enabled=1
              setprop         Data3ListView,enabled=1
              setprop         Data3ListView001,enabled=1
              setprop         Data3ListViewQReC,enabled=1
              getprop         DataNew,enabled=result
              if (result = 1)
               setprop        DataOK,enabled=1
               setprop        DataListViewSearch,enabled=1
              endif
              return

Data3EnableLower
              setprop         DataOK,enabled=0
              setprop         DataListViewSearch,enabled=0
              setprop         Data3ListView,enabled=0
              setprop         Data3ListView001,enabled=1
              setprop         Data3ListViewQReC,enabled=1


              setprop         Data3New,enabled=0
              setprop         Data3Calc,enabled=0
              setprop         Data3Delete,enabled=0
              setprop         Data3EditListU,enabled=1,bgcolor=white
              setprop         Data3EditMlrU,enabled=1,bgcolor=white
              setprop         Data3EditDateU,enabled=1,bgcolor=white
.             setprop         Data3ComboCodeU,enabled=1
              setprop         Data3Quit,enabled=1
              setprop         Data3Save,enabled=1
              return
.END PATCH 1.1.5 ADDED LOGIC

DataClearScreen
.Screen1
              setitem         DataEditListNum,0,""
              setitem         DataComboStatus,0,1
.              setitem         DataCheckExclusive,0,0
              setitem         DataExclComboBOx,0,0
              setitem         DataCheckNew,0,0
              setitem         DataEditMListName,0,""
              setitem         DataEditListName,0,""
              setitem         DataEditRevised,0,""
.START PATCH 1.1 REPLACED LOGIC
.             setitem         DataStatRevised2,0,""
              setitem         DataEditRevised2,0,""
              setitem         DataEditUpdate,0,""
              setitem         DataStatUpdate2,0,""
.END PATCH 1.1 REPLACED LOGIC
              setitem         DataEditPrevOwn,0,""
              setitem         DataEditOwnNum,0,""
              setitem         DataStatOwnName,0,""
              setitem         DataStatOwnAdd1,0,""
              setitem         DataStatOwnAdd2,0,""
              setitem         DataStatOwnAdd3,0,""
              setitem         DataStatOwnCnt,0,""
              setitem         DataStatOwnPhone,0,""
              setitem DataStatAge,0,""
.             setitem         DataStatPayTo2,0,""
              setitem         DataEditManagerNum,0,""
              setitem         DataStatManagerName,0,""
              setitem         DataEditFulfillNum,0,""
              setitem         DataStatFulfillName,0,""
              setitem         DataEditMlrNum,0,""
              setitem         DataStatMlrName,0,""
              DataListViewSamples.DeleteAllItems giving N9
              setitem         DataEditPayNum,0,""
              setitem         DataStatPayName,0,""
              setitem         DataEditDate,0,""
              setitem         DataEditUniverse,0,""
              setitem         DataEditGender,0,""
              setItem         DataEditFem,0,""
              setitem         DataEditMEn,0,""
              
              setitem         DataEditMinimum,0,""
              setitem         DataEditCommission,0,""
              setitem         DataCheckHotline,0,0
              setitem         DataComboExcRent,0,1
              setitem         DataCheckWeb,0,0
              setitem         DataCheckOffice,0,0
              setitem         DataCheckConverted,0,0
.START PATCH 1.1.5 ADDED LOGIC
              setitem         DataCheckUsage,0,0
.END PATCH 1.1.5 ADDED LOGIC
.START PATCH 1.9.7 ADDED LOGIC
              setitem         DataCheckLRA,0,0
              setitem         DataEditText1,0,""
              setitem         DataEditTextText,0,""
              setitem         DataEditTextUnitData,0,""
              setitem         DataEditBooking,0,""
              setitem         DataComboDelivery,0,1
              setitem         DataComboSample,0,1
              setitem         DataComboNet,0,1
              setitem         DataEditNet,0,""
              setitem         DataComboClean,0,1
              setitem         DataEditClean,0,""
.Screen 2
              setitem         Data2StatListNum,0,""
              Data2WebBrowser.Navigate2 USING "about:blank"
.Screen 3
.begin patch 2.01
              setitem         Data3Statmlrusage,0,""
.end patch 2.01
              setitem         Data3StatListNum,0,""
.START PATCH 1.1.5 ADDED LOGIC
              setitem         Data3StatUsageNum,0,""
              setitem         Data3StatUsageDate,0,""
              setitem         Data3StatListName,0,""
              setitem         Data3StatListMessage,0,""
              Data3ListView.DeleteAllItems
.              Data3ListViewQREc.DeleteAllItems
              call            Data3ClearScreen
.END PATCH 1.1.5 ADDED LOGIC
.begin patch 2.05
              setitem         DataCheckCounts,0,0
              setitem         DataCheckBlocked,0,0
              setitem         DataCheckGender,0,0
.begin patch 2.06
              setitem         DataCheckmlrRestrictions,0,0
.end patch 2.06
.end patch 2.05
              return

.START PATCH 1.1.5 ADDED LOGIC
Data3ClearScreen
              setitem         Data3EditListU,0,""
              setitem         Data3StatListNameU,0,""
              setitem         Data3EditMlrU,0,""
              setitem         Data3StatMlrNumU,0,""
              setitem         Data3StatMlrNameU,0,""
              setitem         Data3EditDateU,0,""
              setitem         Data3StatInitsU,0,""
              setitem         Data3ComboCodeU,0,1
              return
.END PATCH 1.1.5 ADDED LOGIC

DataClearSelectLV
              DataSelectRecListView.DeleteAllItems giving N2
              setitem         DataEditSelectRec,0,""
              DataListViewSelect.DeleteAllItems giving N2
              DataListViewSelectB.DeleteAllItems giving N2
              DataListViewSelectC.DeleteAllItems giving N2
              DataListViewSelectD.DeleteAllItems giving N2
              DataListViewSelectE.DeleteAllItems giving N2
DataClearSelectFields
              setitem         DataEditSelectList,0,""
DataClearSelectFieldsA
              setitem         DataEditSelectNum,0,""
              setitem         DataEditSelectName,0,""
              setitem         DataEditSelectQty,0,""
              setitem         DataEditSelectPrice,0,""
.drewtest
              setitem         DataEditSelectIndex,0,""
.drewtest
              setitem         DataComboSelectBase,0,""
              setitem         DataComboSelectModifier,0,1
              setitem         DataComboSelectStatus,0,1
              setitem         DataComboSelectExcRent,0,1
              setitem         DataCheckSelectComm,0,0
              setitem         DataCheckSelectInactive,0,0
              setitem         DataCheckSelectNotes,0,0
              return

DataClearRefFields
              setitem         DataEditRefList,0,""
DataClearRefFieldsA
              setitem         DataEditRefQty,0,""
              setitem         DataEditRefPrice,0,""
              setitem         DataEditRefPer,0,""
              setitem         DataComboRefModifier,0,1
              return

DataLoadListView
              pack            hold,DATVARS
.START PATCH 1.2.7 ADDED LOGIC
          call TRIM using MLSTNAME
.END PATCH 1.2.7 ADDED LOGIC

.START PATCH 1.1.1 REPLACED LOGIC
.             DataListViewSearch.InsertItem giving N9 using MLSTNAME
.             DataListViewSearch.SetItemText using N9,LSTNUM,1
.             DataListViewSearch.SetItemText using N9,MLSTNAME,2
.             DataListViewSearch.SetItemText using N9,hold,3
..............................................................
              DataListViewSearch.InsertItem giving N9 using MLSTNAME
              DataListViewSearch.SetItemText using N9,LSTNUM,1
              DataListViewSearch.SetItemText using N9,MLSTNAME,2
              unpack          REVDATE,CC,YY,MM,DD
              call            Trim using MM
              if (MM <> "")
               pack           str10,MM,SLASH,DD,SLASH,CC,YY
              else
               clear          str10
              endif
              DataListViewSearch.SetItemText using N9,str10,3
.START PATCH 1.2.7 REPLACED LOGIC
.                   DataListViewSearch.SetItemText using N9,STATUS,4
                    if (STATUS = "W")
                              pack str15,"Withdrawn"
                              call trim using str15
                    elseif (STATUS = "T")
                              pack str15,"Temp. Withdrawn"
                              call trim using str15
                    else
                              clear str15
                    endif
                    DataListViewSearch.SetItemText using N9,STR15,4
.begin patch 1.92
.SRDS #
                    MOve      c2,S2NPATH
                    packkey   S2nFLd2 from Lstnum
                    call      S2nkey
                    if        Not over
                    DataListViewSearch.SetItemText using N9,S2NSRDS,5
                    endif
.begin patch 1.5
.MIN #
                    MOve      c2,M2NPATH
                    packkey   M2nFLd2 from Lstnum
                    call      M2nkey
                    if        Not over
.                    DataListViewSearch.SetItemText using N9,M2NMin,5
                    DataListViewSearch.SetItemText using N9,M2NMin,6
                    endif
                    
                    
.END PATCH 1.2.7 REPLACED LOGIC
..              DataListViewSearch.SetItemText using N9,hold,5
.              DataListViewSearch.SetItemText using N9,hold,6
              DataListViewSearch.SetItemText using N9,hold,7
.end patch 1.5
.START PATCH 1.2.7 REPLACED LOGIC
.              if (STATUS = "T" | STATUS = "W")
.               move           "0xFF0000",colordim                          .Red
.              else
.               move           "0x000000",colordim                          .Black
.              endif
.             DataListViewSearch.SetItemText using N9,colordim,6
.begin patch 1.5
..                   DataListViewSearch.SetItemText using N9,NDATUPDDATE,6
..                   DataListViewSearch.SetItemText using N9,ELSTCDE,7
..                   DataListViewSearch.SetItemText using N9,NLSTCDE,8

.                    DataListViewSearch.SetItemText using N9,NDATUPDDATE,7
.                    DataListViewSearch.SetItemText using N9,ELSTCDE,8
.                    DataListViewSearch.SetItemText using N9,NLSTCDE,9

                    DataListViewSearch.SetItemText using N9,NDATUPDDATE,8
                    DataListViewSearch.SetItemText using N9,ELSTCDE,9
                    DataListViewSearch.SetItemText using N9,NLSTCDE,10
.end patch 1.5
                    UNPACK REVDATE,CC,YY,MM,DD
                    CALL CVTJUL
                    MOVE JULDAYS,STR5
.begin patch 1.5
..                   DataListViewSearch.SetItemText using N9,str5,9
.                    DataListViewSearch.SetItemText using N9,str5,10
                    DataListViewSearch.SetItemText using N9,str5,11
.end patch 1.5
                    unpack          NDATUPDDATE,CC,YY,MM,DD
                call CVTJUL
                    move JULDAYS, str5
.begin patch 1.5
..                   DataListViewSearch.SetItemText using N9,str5,10
.                    DataListViewSearch.SetItemText using N9,str5,11
                    DataListViewSearch.SetItemText using N9,str5,12
.end patch 1.5
.begin patch xxx
                              move           "0x000000",colordim                          .Black

.                    if (STATUS = "T" | STATUS = "W")
                    if           (lstnum = "074891")
                    call         debug
                    endif
                    if (STATUS = "T" | STATUS = "W")
                                 if (elstcde <> "C" & Elstcde <> "P")
                                     move           "0xFF0000",colordim                          .Red
                                 else           
                                     move           "0xFF9900",colordim                         .Orange
                                 endif
                    Else             
                                 if (elstcde <> "C" & Elstcde <> "P")
                                            move           "0x000000",colordim                          .Black
                                 else           
                                            move           "0x0000FF",colordim                          .Blue
                                 endif           
.                    else
.                              move           "0x000000",colordim                          .Black
                    endif
.end patch xxx
.begin patch 1.5
..                   DataListViewSearch.SetItemText using N9,colordim,11
.                    DataListViewSearch.SetItemText using N9,colordim,12
                    DataListViewSearch.SetItemText using N9,colordim,13
.end patch 1.5

.END PATCH 1.2.7 REPLACED LOGIC
.
.START PATCH 1.2.7 REMOVED LOGIC
.              call            CVTJUL
.              move            JULDAYS,str5
.              rep             zfill,str5
.              DataListViewSearch2.InsertItem giving N9 using str5
.              DataListViewSearch2.SetItemText using N9,str10,1
.              DataListViewSearch2.SetItemText using N9,MLSTNAME,2
.              DataListViewSearch2.SetItemText using N9,LSTNUM,3
.              DataListViewSearch2.SetItemText using N9,STATUS,4
.              DataListViewSearch2.SetItemText using N9,hold,5
.              DataListViewSearch2.SetItemText using N9,colordim,6
.END PATCH 1.2.7 REMOVED LOGIC
.END PATCH 1.1.1 REPLACED LOGIC
              return

.START PATCH 1.2.7 REPLACED LOGIC
..START PATCH 1.1.1 ADDED LOGIC
.DataSetListViewSearch Routine FrmPtr
..FrmPtr = 1 - calling from DataListViewSearch2, so set DataListViewSearch
..FrmPtr = 2 - calling from DataListViewSearch, so set DataListViewSearch2
.              DataListViewSearch.GetItemCount giving howmany
.              sub             C1,howmany
.              for result,C0,howmany
.               if (FrmPtr = 1)
.                              DataListViewSearch.GetItemText giving str7 using result,5
.               elseif (FrmPtr = 2)
.                             DataListViewSearch2.GetItemText giving str7 using result,5
.               endif
.               unpack         str7,str1,str6
.               if (str6 = LSTNUM)
.                              if (FrmPtr = 1)
.                                             DataListViewSearch.SetItemState giving N9 using result,2,2
.                                             DataListViewSearch.EnsureVisible using result,0
.                              elseif (FrmPtr = 2)
.                                             DataListViewSearch2.SetItemState giving N9 using result,2,2
.                                             DataListViewSearch2.EnsureVisible using result,0
.                              endif
.                              break
.               endif
.              repeat
.              return
..END PATCH 1.1.1 ADDED LOGIC
DataSetListViewSearch
              DataListViewSearch.GetItemCount giving howmany
              sub             C1,howmany
              for result,C0,howmany
                              DataListViewSearch.GetItemText giving str7 using result,5
               unpack         str7,str1,str6
               if (str6 = LSTNUM)
                    DataListViewSearch.SetItemState giving N9 using result,2,2
                    DataListViewSearch.EnsureVisible using result,0
                        break
               endif
               repeat
               return
.END PATCH 1.2.7 REPLACED LOGIC
DataLoadScreen
.Screen 1
.LIST NUMBER
              setitem         DataEditListNum,0,LSTNUM
              pack            NDATFLD,LSTNUM
.STATUS
              if (STATUS = "W")
               setitem        DataComboStatus,0,2
              elseif (STATUS = "T")
               setitem        DataComboStatus,0,3
              else
               setitem        DataComboStatus,0,1
              endif
.EXCLUSIVE CODE
.Begin patch  1.2.9
              move            C0,N1
          if (Elstcde = "C")
                    move      C2,N1
          elseif (Elstcde = "P")
                    move      C3,N1
          else
                    move      C0,N1
          endif
          setitem   DataExclComboBox,0,N1

.              setprop         DataCheckExclusive,Title="Exclusive"
.              if (ELSTCDE = "C")
.              setprop         DataCheckExclusive,Title="NIN Excl"
.              move           C1,N1
.              Elseif          (ELSTCDE = "P")
..             elseif (ELSTCDE = "N")
..              move           C3,N1
.              setprop         DataCheckExclusive,Title="PL Excl"
.              move           C1,N1
.              endif
.end patch  1.2.9
.             setitem         DataCheckExclusive,0,N1
.NEW CODE
              move            C0,N1
              if (NLSTCDE = YES)
               move           C1,N1
              endif
              setitem         DataCheckNew,0,N1
.MASTER LIST NAME
              setitem         DataEditMListName,0,MLSTNAME
.ORDER LIST NAME
              setitem         DataEditListName,0,OLSTNAME
.REVISION INFO
              unpack          REVDATE,CC,YY,MM,DD
              call            Trim using MM
              if (MM <> "")
               pack           str10,MM,SLASH,DD,SLASH,CC,YY
              else
               clear          str10
              endif
              call            Trim using PASSWORD
              if (PASSWORD <> "")
.START PATCH 1.1 REPLACED LOGIC
.              pack           str15," by ",PASSWORD
               pack           str15,PASSWORD
.END PATCH 1.1 REPLACED LOGIC
              else
               clear          str15
              endif
              setitem         DataEditRevised,0,str10
.START PATCH 1.1 REPLACED LOGIC
.             setitem         DataStatRevised2,0,str15
              setitem         DataEditRevised2,0,str15
.END PATCH 1.1 REPLACED LOGIC
              call            CVTJUL
              move            JULDAYS,N5
              unpack          timestamp,CC,YY,MM,DD
              call            CVTJUL
              sub             N5,JULDAYS
              if (JULDAYS > 1095)
               setitem DataStatAge,0,"Over 36 Months Old!"
              else
               setitem DataStatAge,0,""
              endif
.START PATCH 1.1 ADDED LOGIC
.RECORD UPDATE INFO
              unpack          NDATUPDDATE,CC,YY,MM,DD
              call            Trim using MM
              if (MM <> "")
               pack           str10,MM,SLASH,DD,SLASH,CC,YY
              else
               clear          str10
              endif
              setitem         DataEditUpdate,0,str10
              setitem         DataStatUpdate2,0,NDATUPDINIT
.END PATCH 1.1 ADDED LOGIC
.OWNER INFO
.Temporary measure until Client file is created
              unpack          OWNNUM,str2,str4
              move            str4,OWNNUM
.................................
.min patch
            clear         str4
            unpack        NDatoldOwn,str2,str4
            setitem       DataEditPrevOwn,0,Str4
.end min patch        
......................................................
              setitem         DataEditOwnNum,0,OWNNUM
              pack            NOWNFLD,OWNNUM
              move            "D.Load-NOWNKEY",Location
              pack            KeyLocation,"Key: ",NOWNFLD
              call            NOWNKEY
.begin patch 1.9
.begin patch 1.9
              call            DataOwnerLostFocus
.PAY-TO INFO
              clear           NPAYFLD
              pack            NPAYFLD,OWNLON,C0
              move            "D.Load-NPAYKEY",Location
              pack            KeyLocation,"Key: ",NPAYFLD
              call            NPAYKEY
              if over
               clear          taskname
              else
               pack           taskname,POWNER,DASH,PAYNUM,B1,PCOMP
              endif
.             setitem         DataStatPayTo2,0,taskname
              setitem         DataStatPayName,0,taskname
.Will need to read Client File for Manager info
              setitem         DataEditManagerNum,0,DATMAN
              setitem         DataStatManagerName,0,""
.Will need to read Client File for Fulfillment info
              setitem         DataEditFulfillNum,0,DATFUL
.START PATCH 1.2.6 REPLACED LOGIC
.              pack            NFULFLD,DATFUL
.              rep             zfill,NFULFLD
.              move            C1,NFULPATH
.              move            "D.Load-NFULKEY",Location
.              pack            KeyLocation,NFULFLD
.              call            NFULKEY
.              if over
.               clear          NFULCOMP
.              endif
.              setitem         DataStatFulfillName,0,NFULCOMP
                    call                TRIM using DATFUL
                    if (DATFUL <> "")
                                        pack      COMPFLD,DATFUL
                                        rep       zfill,COMPFLD
                                        move            C1,COMPPATH
                                        move                "D.Load-COMPKEY",Location
                                        pack                KeyLocation,COMPFLD
                                        call                COMPKEY
                                        if OVER
                                                  clear     COMPCOMP
                                        else
                                                  if (COMPSVBFLG <> "T")
                                                            clear COMPCOMP
                                                  endif
                                        endif
                    else      // DATFUL = ""
                                        clear COMPCOMP
                    endif
                    setitem             DataStatFulfillName,0,COMPCOMP
.END PATCH 1.2.6 REPLACED LOGIC
.MAILER INFO
.Locate       Mailer of List being used
              DataListViewSamples.DeleteAllItems giving N9
              move            C1,NXRFPATH
              clear           NXRFFLD2
              pack            NXRFFLD,LSTNUM
              move            "D.Load-NXRFKEY",Location
              pack            KeyLocation,"Key: ",NXRFFLD
              call            NXRFKEY
              if not over
.START PATCH 1.2.2 REPLACED LOGIC
.              move           C1,NMLRPATH
.              pack           MKEY,NXRFMLR,"000"
.              move           "D.Load-NMLRKEY",Location
.              pack           KeyLocation,"Key: ",MKEY
.              call           NMLRKEY
.              setitem        DataEditMlrNum,0,NXRFMLR
.              setitem        DataStatMlrName,0,MCOMP
.................................
               pack           COMPFLD,NXRFMLR
               move           "D.Load-COMPKEY",Location
               pack           KeyLocation,"Key: ",COMPFLD
               call           COMPKEY
               setitem        DataEditMlrNum,0,NXRFMLR
               setitem        DataStatMlrName,0,COMPCOMP
.END PATCH 1.2.2 REPLACED LOGIC
.START PATCH 1.1.8 REPLACED LOGIC - TEMPORARY PATCH
.              call           DataLoadSamples using MNUM
               call           DataLoadSamples using COMPNUM
.END PATCH 1.1.8 REPLACED LOGIC - TEMPORARY PATCH
              else
               setitem        DataEditMlrNum,0,""
               setitem        DataStatMlrName,0,""
              endif
.Will need to read Client File for Return-To info
              setitem         DataEditPayNum,0,DATPAY
.             setitem         DataStatPayName,0,""
.RECORD DATE
              unpack          NEWDATE,CC,YY,MM,DD
              call            Trim using MM
              if (MM <> "")
               pack           str10,MM,SLASH,DD,SLASH,CC,YY
              else
               clear          str10
              endif
              setitem         DataEditDate,0,str10
.UNIVERSE
              move            UNIVERSE,str10
              call            FormatNumeric using str10,str13
              setitem         DataEditUniverse,0,str13
.begin patch 2.05
           if         (Ndatflag1 = yes)                
           setitem         DataCheckBlocked,0,1
           else
           setitem         DataCheckBlocked,0,0
           endif
           if         (Ndatflag3 = yes)                
              setitem         DataCheckCounts,0,1
              else
              setitem         DataCheckCounts,0,0
           endif
           if         (Ndatflag2 = yes)                
              setitem         DataCheckGender,0,1
           else                  
              setitem         DataCheckGender,0,0
           endif
.begin patch 2.06
           if         (Ndatflag4 = yes)                
              setitem         DataCheckmlrRestrictions,0,1
           else                  
              setitem         DataCheckmlrRestrictions,0,0
           endif
.end patch 2.06
.end patch 2.05



.GENDER INFO
.begin patch 1.2.8
.         call      debug
                    Clear     Str25
          if        (NdatFem > 0)
                    move                NDatFem,N3
                    MOVe                N3,str3
.                   append    "Women ",str25
                    append    Ndatfem,str25
                    append    "%",Str25
                    reset     str25
.                   append    " ",Str25
          Setitem   DataEditFem,0,str25
              setprop         DataEditGender,enabled=0,bgcolor=grey
              setprop         DataEditGender,visible=0
              setprop         DataEditFem,enabled=1,bgcolor=grey
              setprop         DataEditFem,visible=1
              setprop         DataStatTextFem,enabled=1,bgcolor=grey
              setprop         DataStatTextFem,visible=1
          
             endif
          if        (NdatMen > 0)
                    move                NDatMen,N3
                    MOVe                N3,str3
.                   append    "Men ",str25
                    append    Ndatmen,str25
                    append    "%",Str25
                    reset     str25
           Setitem  DataEditMEn,0,str25
              setprop         DataEditGender,enabled=0,bgcolor=grey
              setprop         DataEditGender,visible=0
              setprop         DataEditMen,enabled=1,bgcolor=grey
              setprop         DataEditMen,visible=1
              setprop         DataStatTextMen,enabled=1,bgcolor=grey
              setprop         DataStatTextMen,visible=1
          endif
          reset     str25
          if        (NdatFem > 0 or NdatMen > 0)
.                   Setitem   DataEditGender,0,Str25
                    Else
                              setitem         DataEditGender,0,SEX
                              endif
.end patch 1.2.8
.MINIMUM INFO
              setitem         DataEditMinimum,0,MIN
.COMMISSION
.START PATCH 1.2.1 REPLACED LOGIC
.             setitem         DataEditCommission,0,COMMPER
              move            COMMPER,str6
              call            Trim using str6
              setitem         DataEditCommission,0,str6
.END PATCH 1.2.1 REPLACED LOGIC
.HOTLINE
              if (HOTLINE = YES)
               move           C1,N1
              else
               move           C0,N1
              endif
              setitem         DataCheckHotline,0,N1
..EXCHANGE ONLY
.             move            C0,N1
.             move            NDATEXCH,N1
.             setitem         DataComboExcRent,0,N1
.WEBSITE ALLOWED TO BE GIVEN OUT
              move            C0,N1
              move            NDATWEB,N1
              setitem         DataCheckWeb,0,N1
.OFFICE USE ONLY
              move            C0,N1
              move            NDATOFF,N1
              setitem         DataCheckOffice,0,N1
.CONVERTED BYTE - EVENTUALLY WILL BE OBSOLETE!!!!!!!!!
              move            C0,N1
              move            NDATCONV,N1
              setitem         DataCheckConverted,0,N1
.START PATCH 1.1.5 ADDED LOGIC
.USAGE
              if (NDATLUSAGE = "F")
               move           C0,N1
              else
               move           C1,N1
              endif
              setitem         DataCheckUsage,0,N1
.END PATCH 1.1.5 ADDED LOGIC
.START PATCH 1.9.7 ADDED LOGIC
              if (NDATLRA = "F")
               move           C0,N1
              elseif (NDATLRA = "T")
               move           C1,N1
               else
               move           C0,N1
              endif
.              call    debug
              setitem         DataCheckLRA,0,n1
               MOve              c0,n2                 
               move              NDatLRADte,n2
               setitem           Data3ComboBoxLRARenewal,0,n2

.UNIT DATA
              setitem         DataEditTextUnitData,0,UNITDATA
.TEXT
              setitem         DataEditText1,0,""         .Initialize object
              setitem         DataEditTextText,0,""         .Initialize object
              clear           NTXTTEXT
              clear           hold2
.begin patch 1.94
patchtst
          move      c2,Ntxt1path
          for N2,C1,"5"
                    pack      NTXT1FLD1,LSTNUM,N2
                    rep       zfill,ntxt1fld1
                    move      "D.Load-NTXT1KEY",Location
                    pack      KeyLocation,"Key: ",NTXT1FLD1
.                    call      debug
                    call      NTXT1KEY
                    until     over
                    if        (lstnum <> NTXT1LIST)
                    Break    
                    endif
                    call      Padit
          repeat                  
          goto      endtxt1

PADIT
          MOVELPTR  NTXT1TEXT,n3
          IF        (N3 < 50)
          PACK      nTXT1TEXT FROM NTXT1TEXT,B1
          GOTO PADIT
          ENDIF
          append         ntxt1text,hold2

.          repeat
          return
endtxt1                 
          if (hold2 <> "")
          APPEND    B55,HOLD2
          reset     hold2
          ENDIF
          setitem         DataEditText1,0,hold2
          
          clear     Hold2
.end patch 1.94


.              for N1,C1,"9"
              for N2,C1,"15"
.               pack           NTXTFLD,LSTNUM,N1
               pack           NTXTFLD,LSTNUM,N2
               rep            Zfill,NTxtFLd
               move           "D.Load-NTXTKEY",Location
               pack           KeyLocation,"Key: ",NTXTFLD
               call           NTXTKEY
               until over
               append         NTXTTEXT,hold2
              repeat
              if (hold2 <> "")
               reset          hold2
               call           Trim using hold2
              endif
              setitem         DataEditTextText,0,hold2
.SELECTS
              if (SelectFlag =0)
               call           DataLoadSelectSelect
              endif
.............................
              if (RefFlag = 0)
.ADDRESSING, ARRANGEMENT, CATEGORY, SELECT, SOURCE
               DataListViewRef.DeleteAllItems giving N9     .Initialize object
               call           LoadDataListViewRefHeaders
.START PATCH 1.1.3 ADDED LOGIC
               call           Trim using LSTNUM
               if (LSTNUM <> "")
.END PATCH 1.1.3 ADDED LOGIC
.ADDRESSING
                              pack           NADDFLD1,"01X",LSTNUM
                              move           "D.Load-NADDAIM",Location
                              pack           KeyLocation,"Key: ",NADDFLD1
                              call           NADDAIM
                              loop
                                             until over
                                             call           DataLoadRefAddressing
                                             move           "D.Load-NADDKG",Location
                                             pack           KeyLocation,"Key: ",NADDFLD1
                                             call           NADDKG
                              repeat
.SELECT
                              pack           NSLTFLD1,"01X",LSTNUM
                              move           "D.Load-NSLTAIM",Location
                              pack           KeyLocation,"Key: ",NSLTFLD1
                              call           NSLTAIM
                              loop
                                             until over
                                             call           DataLoadRefSelection
                                             move           "D.Load-NSLTKG",Location
                                             pack           KeyLocation,"Key: ",NSLTFLD1
                                             call           NSLTKG
                              repeat
.ARRANGEMENT
                              pack           NARRFLD1,"01X",LSTNUM
                              move           "D.Load-NARRAIM",Location
                              pack           KeyLocation,"Key: ",NARRFLD1
                              call           NARRAIM
                              loop
                                             until over
                                             call           DataLoadRefArrangement
                                             move           "D.Load-NARRKG",Location
                                             pack           KeyLocation,"Key: ",NARRFLD1
                                             call           NARRKG
                              repeat
.SOURCE
                              pack           NSRCFLD1,"01X",LSTNUM
                              move           "D.Load-NSRCAIM",Location
                              pack           KeyLocation,"Key: ",NSRCFLD1
                              call           NSRCAIM
                              loop
                                             until over
                                             call           DataLoadRefSource
                                             move           "D.Load-NSRCKG",Location
                                             pack           KeyLocation,"Key: ",NSRCFLD1
                                             call           NSRCKG
                              repeat
.CATEGORY
                              pack           NCATFLD1,"01X",LSTNUM
                              move           "D.Load-NCATAIM",Location
                              pack           KeyLocation,"Key: ",NCATFLD1
                              call           NCATAIM
                              loop
                                             until over
                                             call           DataLoadRefCategory
                                             move           "D.Load-NCATKG",Location
                                             pack           KeyLocation,"Key: ",NCATFLD1
                                             call           NCATKG
                              repeat
.START PATCH 1.1.3 ADDED LOGIC
               endif
.END PATCH 1.1.3 ADDED LOGIC
.Highlight top item
               DataListViewRef.GetItemCount giving result
               sub            C1,result
               for howmany,C0,result
                              DataListViewRef.GetItemText giving str4 using howmany
                              call           Trim using str4
                              count          N1,str4
                              if (N1 > 3)
                                             break
                              endif
               repeat
               if (howmany > result)
                              move           C0,howmany
               endif
               DataListViewRef.SetItemState giving N9 using howmany,2,2
               DataListViewRef.EnsureVisible using howmany,0
               call           Click_DataListViewRef
              endif
.BOOKING INSTRUCTIONS
              setitem         DataEditBooking,0,""                         .Initialize object
              clear           MDLCODE
              clear           MDLLCRCD
              clear           MDLPLAN
              clear           MDLCALL
.START PATCH 1.2.2 REPLACED LOGIC
.             move            LSTNUM,NMDLFLD
.             rep             zfill,NMDLFLD
              pack            NMDLFLD,LSTNUM,B1
.END PATCH 1.2.2 REPLACED LOGIC
              move            "D.Load-NMDLKEY",Location
              pack            KeyLocation,"Key: ",NMDLFLD
              call            NMDLKEY
              if not over
.Add first section of verbage - Booking
               if (MDLCODE = NO)
                              setitem        DataEditBooking,0,"No Booking Allowed"
               else
                              setitem        DataEditBooking,0,"Booking Allowed"
               endif
.Append second section of verbage - LCR Info
               getitem        DataEditBooking,0,result                     .Store position of last character currently in TextBox
               setitem        DataEditBooking,1,result                     .Set beginning selected cursor to that position
               setitem        DataEditBooking,2,result                     .Set ending selected cursor to that position
               setitem        DataEditBooking,1,newline                    .Place a carriage return character there
               getitem        DataEditBooking,0,result                     .Store position of last character currently in TextBox
               add  C1,result                                    .Since this character will not be read by getitem, add 1
               setitem        DataEditBooking,1,result                     .Reset the beginning selected cursor
               setitem        DataEditBooking,2,result                     .Reset the ending selected cursor
               if (MDLLCRCD = NO)                                                  .Following section will append text to appointed cursor location.  Note the value '1' as second argument!
                              setitem        DataEditBooking,1,"No LCRs"
               else
                              setitem        DataEditBooking,1,"LCRs OK"
               endif
.Append third section of verbage - Planner
               getitem        DataEditBooking,0,result
               setitem        DataEditBooking,1,result
               setitem        DataEditBooking,2,result
               setitem        DataEditBooking,1,newline
               getitem        DataEditBooking,0,result
               add            C1,result
               setitem        DataEditBooking,1,result
               setitem        DataEditBooking,2,result
               pack           str55,"Planner: ",MDLPLAN
.Append fourth section of verbage - Caller
               setitem        DataEditBooking,1,str55
               getitem        DataEditBooking,0,result
               setitem        DataEditBooking,1,result
               setitem        DataEditBooking,2,result
               setitem        DataEditBooking,1,newline
               getitem        DataEditBooking,0,result
               add            C1,result
               setitem        DataEditBooking,1,result
               setitem        DataEditBooking,2,result
               pack           str55,"Caller: ",MDLCALL
               setitem        DataEditBooking,1,str55
.Append final section of verbage - Text
               getitem        DataEditBooking,0,result
               setitem        DataEditBooking,1,result
               setitem        DataEditBooking,2,result
               setitem        DataEditBooking,1,newline
               getitem        DataEditBooking,0,result
               add            C1,result
               setitem        DataEditBooking,1,result
               setitem        DataEditBooking,2,result
               setitem        DataEditBooking,1,MDLtext
.Reset the cursor to first byte
               setitem        DataEditBooking,1,1
               setitem        DataEditBooking,2,1
              endif
.DELIVERY CODE
              move            C1,result
              for             N9,C1,NDELIV
               getitem        DataComboDelivery,N9,taskname
               unpack         taskname,NREFDESC,str4
               if (str4 = DELCODE)
                              move           N9,result
               endif
              repeat
              setitem         DataComboDelivery,0,result
.SAMPLE CODE
              move            C1,result
              for             N9,C1,NSAMPLE
               getitem        DataComboSample,N9,taskname
               unpack         taskname,NREFDESC,str4
               if (str4 = SAMPLE)
                              move           N9,result
               endif
              repeat
              setitem         DataComboSample,0,result
.NET CODE
              call debug
              move            C1,result
.begin patch 2.08
              calc            N10=nnet+1
.              for             N9,C1,NNET
.end patch 2.08
              for             N9,C1,N10
               getitem        DataComboNet,N9,taskname
               unpack         taskname,NREFDESC,str4
               if (str4 = NETNAME)
                              move           N9,result
               endif
              repeat
              setitem         DataComboNet,0,result
              setitem         DataEditNet,0,NETINFO
.CLEAN CODE
              move            C1,result
              for             N9,C1,NCLEAN
               getitem        DataComboClean,N9,taskname
               unpack         taskname,NREFDESC,str4
               if (str4 = CLEANCDE)
                              move           N9,result
               endif
              repeat
              setitem         DataComboClean,0,result
              setitem         DataEditClean,0,CLNINFO
.Screen 2
              setitem         Data2StatListNum,0,LSTNUM
.Test Logic
.          call      debug
              call            CreateWebCard using LSTNUM
              pack            taskname,"c:\work\data",LSTNUM,".HTM"
              pack            APIFileName,str55,str55,str55,str55,str55
              pack            APIFileName,taskname,hexzero
              call            FindFirstFile
              if (APIResult <> 0 & APIResult <> hexeight)
               Data2WebBrowser.Navigate2 USING taskname
              else
               Data2WebBrowser.Navigate2 USING "about:blank"
              endif
.Screen 3
              setitem         Data3StatListNum,0,LSTNUM
.START PATCH 1.1.5 ADDED LOGIC
DataLoadScreen3
              setitem         Data3StatListName,0,MLSTNAME
              if (NDATLUSAGE = "F")
               setitem        Data3StatListMessage,0,"Do Not Share ANY List Usage Information for this Datacard!!!"
               move           "0xFF0000",colordim                          .Red
              else
               setitem        Data3StatListMessage,0,""
               move           "0x000000",colordim                          .Black
              endif
              clear           str45
              Data3ListView.DeleteAllItems
.              Data3ListViewQREc.DeleteAllItems
              call Trim using LSTNUM
              if (LSTNUM <> "")
               pack           NUSGFLD1,"01X",LSTNUM
               move           "D.Load-NUSGAIM",Location
               pack           KeyLocation,"Key: ",NUSGFLD1
               call           NUSGAIM
               loop
                              until over
                              call           Data3LoadListView
                              move           "D.Load-NUSGKG",Location
                              call           NUSGKG
               repeat
              endif
              Data3ListView.GetItemCount giving howmany
              if (howmany > 0)
               Data3ListView.SetItemState giving N9 using 0,2,2
               Data3ListView.EnsureVisible using 0,0
               call           Click_Data3ListView
              else
               call           Data3ClearScreen
              endif
.begin patch 2.01
                      if         (COMPMUSAGE = "F")
                      pack       str35,"Mailer Does not Share Usage"
                      setprop         Data3StatMlrUsage,fgcolor=red
                      else
                      pack       str35,"Mailer Shares Usage"
                      setprop         Data3StatMlrUsage,fgcolor=Black
                      endif
                      setitem         Data3StatMlrUsage,0,str35
           
.end patch 2.01
              
              setitem         Data3StatUsageDate,0,str45
              move            howmany,str9
              call            Trim using str9
              call            FormatNumeric using str9,str11
              pack            str45,str11," Usage Records Found"
              setitem         Data3StatUsageNum,0,str45
.END PATCH 1.1.5 ADDED LOGIC
.begin patch 1.6
..... get Quick recok goodies
              Data3ListViewQREc.DeleteAllItems
          packkey   NQRCFLD   From Lstnum
          rep       zfill in nqrcfld
          Call      NQRCKey
          if        not over  
          packkey   NQRCDFLD from NQRCNum
          rep       zfill in nqrcdfld
          Call      NqrcDkey
          Data3ListViewQReC.insertitem giving n9 using NQRCNum
          Data3ListViewQReC.setitemText using N9,NQRCDdesc,1   
          Loop
          Call      NQRCKS
                    if        not over
                              if        (NQRCLIST <> Lstnum)
                              Break
                              endif
                    packkey   NQRCDFLD from NQRCNum
                    rep       zfill in nqrcdfld
                    Call      NqrcDkey
                    Data3ListViewQReC.insertitem giving n9 using NQRCNum
                    Data3ListViewQReC.setitemText using N9,NQRCDdesc,1   
          else
          Break     
                    endif
          repeat
               Data3ListViewQrec.SetItemState giving N9 using 0,2,2
               Data3ListViewQrec.EnsureVisible using 0,0
          endif
.end patch 1.6



              return

.START PATCH 1.1.5 ADDED LOGIC
Data3LoadListView
              move            "D.Load-COMPKEY",Location
              pack            COMPFLD,NUSGMLR
              pack            KeyLocation,"Key: ",COMPFLD
              call            COMPKEY
.
              Data3ListView.InsertItem giving result using COMPCOMP
              pack            str11,COMPOLDMLR,SLASH,COMPNUM
              Data3ListView.SetItemText using result,str11,1
              unpack          NUSGDATE,str4,MM,DD
              pack            str10,MM,SLASH,DD,SLASH,str4
              Data3ListView.SetItemText using result,str10,2
              if (NUSGCODE = "2")
               move           "hand entered",str25
              else
               if (str45 = "")
                              pack           str45,"Usage Last Updated: ",str10
               endif
               clear          str25
              endif
              Data3ListView.SetItemText using result,str25,3
              Data3ListView.SetItemText using result,NUSGINITS,4
              if (COMPMUSAGE = "F")
               move           "Do Not Share!",str25
               move           "0xFFFF00",colordim2                         .Yellow
              else
               clear          str25
               move           "0xFFFFFF",colordim2                         .White
              endif
              Data3ListView.SetItemText using result,str25,5
              pack            taskname,NUSGVARS,COMPOLDMLR,COMPCOMP
              Data3ListView.SetItemText using result,taskname,6
              Data3ListView.SetItemText using result,colordim,7
              Data3ListView.SetItemText using result,colordim2,8
              return

Data3LoadScreen
              setitem         Data3EditListU,0,NUSGLIST
              setitem         Data3StatListNameU,0,MLSTNAME
.START PATCH 1.2.2 REPLACED LOGIC
.             setitem         Data3EditMlrU,0,COMPOLDMLR
              setitem         Data3EditMlrU,0,COMPNUM
.END PATCH 1.2.2 REPLACED LOGIC
              setitem         Data3StatMlrNumU,0,NUSGMLR
              setitem         Data3StatMlrNameU,0,COMPCOMP
.START PATCH 1.1.6 REPLACED LOGIC
.             setitem         Data3EditDateU,0,str10
              call            Trim using NUSGDATE
              if (NUSGDATE <> "")
               unpack         NUSGDATE,CC,YY,MM,DD
               pack           str10,MM,SLASH,DD,SLASH,CC,YY
              else
               clear          str10
              endif
              setitem         Data3EditDateU,0,str10
.END PATCH 1.1.6 REPLACED LOGIC
              setitem         Data3StatInitsU,0,NUSGINITS
              move            C1,N1
              move            NUSGCODE,N1
              setitem         Data3ComboCodeU,0,N1
              return
.END PATCH 1.1.5 ADDED LOGIC

DataLoadRefAddressing
              pack            taskname,NADDVARS
              pack            str6,"AAA",NADDNUM
              DataListViewRef.InsertItem giving N10 using str6
              pack            NREFFLD,"A",NADDNUM
              move            "D.Load1-NREFKEY",Location
              pack            KeyLocation,"Key: ",NREFFLD
              call            NREFKEY
              call            Trim using NREFDESC
              DataListViewRef.SetItemText using N10,NREFDESC,1
              if (NADDPRICE = C0)
               clear          str9
              else
               unpack         NADDPRICE,str5,str3
               call           FormatNumeric using str5,str6
               pack           str9,str6,str3
              endif
              pack            NMODFLD,NADDDESC
              rep             zfill,NMODFLD
              move            "D.Load2-NMODKEY",Location
              pack            KeyLocation,"Key: ",NMODFLD
              call            NMODKEY
              call            Trim using NMODDESC
              pack            str25,str9,NMODDESC
              DataListViewRef.SetItemText using N10,str25,2
              DataListViewRef.SetItemText using N10,taskname,4
              return

DataLoadRefSelection
              pack            taskname,NSLTVARS
              pack            str6,"LLL",NSLTNUM
              DataListViewRef.InsertItem giving N10 using str6
              pack            NREFFLD,"L",NSLTNUM
              move            "D.Load2-NREFKEY",Location
              pack            KeyLocation,"Key: ",NREFFLD
              call            NREFKEY
              call            Trim using NREFDESC
              DataListViewRef.SetItemText using N10,NREFDESC,1
              if (NSLTPRICE = C0)
               clear          str9
              else
               unpack         NSLTPRICE,str5,str3
               call           FormatNumeric using str5,str6
               pack           str9,str6,str3
              endif
              pack            NMODFLD,NSLTDESC
              rep             zfill,NMODFLD
              move            "D.Load3-NMODKEY",Location
              pack            KeyLocation,"Key: ",NMODFLD
              call            NMODKEY
              call            Trim using NMODDESC
              pack            str25,str9,NMODDESC
              DataListViewRef.SetItemText using N10,str25,2
              call            FormatNumeric using NSLTQTY,str11
              DataListViewRef.SetItemText using N10,str11,3
              DataListViewRef.SetItemText using N10,taskname,4
              return

DataLoadRefArrangement
              pack            taskname,NARRVARS
              pack            str6,"RRR",NARRNUM
              DataListViewRef.InsertItem giving N10 using str6
              pack            NREFFLD,"R",NARRNUM
              move            "D.Load4-NREFKEY",Location
              pack            KeyLocation,"Key: ",NREFFLD
              call            NREFKEY
              call            Trim using NREFDESC
              DataListViewRef.SetItemText using N10,NREFDESC,1
              DataListViewRef.SetItemText using N10,taskname,4
              return

DataLoadRefSource
              pack            taskname,NSRCVARS
              pack            str6,"SSS",NSRCNUM
              DataListViewRef.InsertItem giving N10 using str6
              pack            NREFFLD,"S",NSRCNUM
              move            "D.Load5-NREFKEY",Location
              pack            KeyLocation,"Key: ",NREFFLD
              call            NREFKEY
              call            Trim using NREFDESC
              DataListViewRef.SetItemText using N10,NREFDESC,1
              move            C0,N3
              move            NSRCPER,N3
              if (N3 > C0)
               move           N3,NSRCPER
               call           Trim using NSRCPER
               pack           str4,NSRCPER,PRC
               DataListViewRef.SetItemText using N10,str4,2
              endif
              DataListViewRef.SetItemText using N10,taskname,4
              return

DataLoadRefCategory
              pack            taskname,NCATVARS
              pack            str6,"TTT",NCATCODE,NCATNUM
              DataListViewRef.InsertItem giving N10 using str6
              pack            NREFFLD,"T",NCATCODE,NCATNUM
              move            "D.Load6-NREFKEY",Location
              pack            KeyLocation,"Key: ",NREFFLD
              call            NREFKEY
              call            Trim using NREFDESC
              DataListViewRef.SetItemText using N10,NREFDESC,1
              if (NCATCODE = "B")
               move           "Business",str8
              elseif (NCATCODE = "C")
               move           "Consumer",str8
              elseif (NCATCODE = "E")
               move           "Enhanced",str8
              endif
              DataListViewRef.SetItemText using N10,str8,2
              DataListViewRef.SetItemText using N10,taskname,4
              return

DataSelectSetListView LRoutine FrmPtr
              DataListViewSelect.GetItemCount giving result
              if (result > 0)
               if (SelViewFlag = 1)
                              if (FrmPtr <> C1)
                                             DataListViewSelect.SetItemState giving N9 using 0,2,2
                                             DataListViewSelect.EnsureVisible using 0,0
                              endif
                              call           Click_DataListViewSelect
                              setfocus DataListViewSelect
               elseif (SelViewFlag = 2)
                              if (FrmPtr <> C1)
                                             DataListViewSelectB.SetItemState giving N9 using 0,2,2
                                             DataListViewSelectB.EnsureVisible using 0,0
                              endif
                              call           Click_DataListViewSelectB
                              setfocus DataListViewSelectB
               elseif (SelViewFlag = 3)
                              if (FrmPtr <> C1)
                                             DataListViewSelectC.SetItemState giving N9 using 0,2,2
                                             DataListViewSelectC.EnsureVisible using 0,0
                              endif
                              call           Click_DataListViewSelectC
                              setfocus DataListViewSelectC
               elseif (SelViewFlag = 4)
                              if (FrmPtr <> C1)
                                             DataListViewSelectD.SetItemState giving N9 using 0,2,2
                                             DataListViewSelectD.EnsureVisible using 0,0
                              endif
                              call           Click_DataListViewSelectD
                              setfocus DataListViewSelectD
               elseif (SelViewFlag = 5)
                              if (FrmPtr <> C1)
                                             DataListViewSelectE.SetItemState giving N9 using 0,2,2
                                             DataListViewSelectE.EnsureVisible using 0,0
                              endif
                              call           Click_DataListViewSelectE
                              setfocus DataListViewSelectE
               endif
              else                           .Safety        measure
               call           DataClearSelectFields
              endif
              return

DataLoadSelectSelect
              DataSelectRecListView.DeleteAllItems giving N2
              setitem         DataEditSelectRec,0,""
              DataListViewSelect.DeleteAllItems giving N2
              DataListViewSelectB.DeleteAllItems giving N2
              DataListViewSelectC.DeleteAllItems giving N2
              DataListViewSelectD.DeleteAllItems giving N2
              DataListViewSelectE.DeleteAllItems giving N2
              call            DataLoadSelectComboBase using C1
              call            DataSortSelectListViews
              call            DataSelectSetListView using C0
              return

DataSortSelectListViews
              DataListViewSelect.SortColumn using *Column=0,*Type=3                       .Numeric Ascending
              DataListViewSelectB.SortColumn using *Column=0,*Type=1                      .Alpha Ascending
              DataListViewSelectC.SortColumn using *Column=0,*Type=4                      .Numeric Descending
              DataListViewSelectD.SortColumn using *Column=0,*Type=3                      .Numeric Ascending
              DataListViewSelectE.SortColumn using *Column=0,*Type=3                      .Numeric Ascending
              return

DataLoadSelectComboBase Routine FrmPtr
.Refresh Associated ComboBox
              deleteitem DataComboSelectBase,0
              insertitem DataComboSelectBase,C0,B1
              insertitem DataComboSelectBase,C1,"Base"
              insertitem DataComboSelectBase,C2,"Secondary Base"
.
              pack            NSELFLD1,"01X",LSTNUM
              clear           NSELFLD2
              move            "D.Load-NSELAIM",Location
              pack            KeyLocation,"Key: ",NSELFLD1
              call            NSELAIM
              loop
               until over
               if (FrmPtr = C1)
                              call           DataLoadSelectRecord
               endif
.Refresh Associated ComboBox
.                    call debug
               if (NSELBASE = "BASE")
                              pack           taskname,NSELNUM,"-",NSELSNAME
                              insertitem DataComboSelectBase,C3,taskname
               elseif (NSELBASE = "SEC.")
                              pack           taskname,NSELNUM,"-",NSELSNAME
                              insertitem DataComboSelectBase,C4,taskname
               endif
               move           "D.Load-NSELKG",Location
               pack           KeyLocation,"Key: ",NSELFLD1
               call           NSELKG
              repeat
              return

DataLoadSelectSelect2
              DataSelectRecListView.DeleteAllItems giving N2
              setitem         DataEditSelectRec,0,""
              DataListViewSelect.DeleteAllItems giving N2
              DataListViewSelectB.DeleteAllItems giving N2
              DataListViewSelectC.DeleteAllItems giving N2
              DataListViewSelectD.DeleteAllItems giving N2
              DataListViewSelectE.DeleteAllItems giving N2
              if (SelViewFlag = 1)
               call           Click_DataListViewSelect
              elseif (SelViewFlag = 2)
               call           Click_DataListViewSelectB
              elseif (SelViewFlag = 3)
               call           Click_DataListViewSelectC
              elseif (SelViewFlag = 4)
               call           Click_DataListViewSelectD
              elseif (SelViewFlag = 5)
               call           Click_DataListViewSelectE
              endif
              return

DataLoadSelectRecord
.Establish Foreground Color
              if (NSELINACTIVE = "1")
               move           "0xFF0000",colordim                          .Red
              elseif (NSELSTATUS = "1")
               move           "0x0000FF",colordim                          .Blue
              else
               move           "0x000000",colordim                          .Black
              endif
.Establish Background Color
              if (NSELBASE = "BASE")
               move           "0xFFFF00",colordim2                         .Yellow
              elseif (NSELBASE = "SEC.")
               move           "0xFFFFC0",colordim2                         .Pale Yellow
              else
               move           "0xFFFFFF",colordim2                         .White
              endif
.This ListView needs to initialized each time
              move            NSELSNAME,taskname
              if (NSELNOTES = "1")
               pack           taskname2,"**",taskname
              else
               pack           taskname2,taskname
              endif
              call            Trim using NSELINDEX
              move            C0,N4
              move            NSELINDEX,N4
              move            N4,NSELINDEX
              rep             zfill,NSELINDEX
              DataListViewSelect.InsertItem giving result using NSELINDEX
              DataListViewSelect.SetItemText using result,taskname2,1
              DataListViewSelect.SetItemText using result,NSELNUM,2
              move            NSELQTY,str10
              call            FormatNumeric using str10,str13
              DataListViewSelect.SetItemText using result,STR13,3
              if (NSELEXC = "2" & (NSELBASE = "BASE" | NSELBASE = "SEC."))
               pack           str25,"Exc. Only"
              else
               unpack         NSELPRICE,str5,str3
               call           FormatNumeric using str5,str6
               pack           str9,str6,str3
               pack           NMODFLD,NSELDESC
               rep            zfill,NMODFLD
               move           "D.Load1-NMODKEY",Location
               pack           KeyLocation,"Key: ",NMODFLD
               call           NMODKEY
               call           Trim using NMODDESC
               if (NSELBASE = "BASE" | NSELBASE = "SEC." | NSELBASE = "    ")
                              clear          str1
               else                                         .if (NSELPRICE > 0)
                              move           "+",str1
.              else
.                             clear          str1
               endif
               pack           str25,str1,str9,NMODDESC
              endif
              DataListViewSelect.SetItemText using result,str25,4
              clear           str15
              if (NSELBASE = "BASE")
               append         "Base",str15
              elseif (NSELBASE = "SEC.")
               append         "Sec. Base",str15
              endif
              if (NSELSTATUS = "1")
               if (str15 <> "")
                              append         "/Spec.",str15
               else
                              move           "Special",str15
               endif
              endif
              if (str15 <> "")
               reset          str15
              endif
              DataListViewSelect.SetItemText using result,str15,5
              if (NSELPCOMM = "1")
               move           YES,str1
              else
               clear          str1
              endif
              DataListViewSelect.SetItemText using result,str1,6
.             setprop SubIt,*ForeColor=colornum,*Bold=OBOOL
              if (NSELINACTIVE = "1")
               move           YES,str2
              else
               clear          str2
              endif
              DataListViewSelect.SetItemText using result,str2,7
.Following needs to be done in order to pad out with blank spaces
              packkey         NSELSNAME,NSELSNAME
              pack            taskname3,NSELVARS
              DataListViewSelect.SetItemText using result,taskname3,8
              DataListViewSelect.SetItemText using result,colordim,9
              DataListViewSelect.SetItemText using result,colordim2,10
.
              DataListViewSelectB.InsertItem giving result using taskname
              DataListViewSelectB.SetItemText using result,taskname2,1
              DataListViewSelectB.SetItemText using result,NSELNUM,2
              DataListViewSelectB.SetItemText using result,STR13,3
              DataListViewSelectB.SetItemText using result,str25,4
              DataListViewSelectB.SetItemText using result,str15,5
              DataListViewSelectB.SetItemText using result,str1,6
              DataListViewSelectB.SetItemText using result,str2,7
              DataListViewSelectB.SetItemText using result,taskname3,8
              DataListViewSelectB.SetItemText using result,colordim,9
              DataListViewSelectB.SetItemText using result,colordim2,10
.
              rep             zfill,NSELQTY
.  0 = NO GENDER SELECT
.  1 = MEN
.  2 = WOMEN
.  3 = OMIT MEN
.  4 = OMIT WOMEN
.  5 = MEN/OTHER
.  6 = WOMEN/OTHER
.              if (NSELGENDER = "0" | NSELGENDER = " ")
.                             move           "99",str2
.              elseif (NSELGENDER = "1")
.                             move           "98",str2
.              elseif (NSELGENDER = "2")
.                             move           "97",str2
.              elseif (NSELGENDER = "3")
.                             move           "96",str2
.              elseif (NSELGENDER = "4")
.                             move           "95",str2
.              elseif (NSELGENDER = "5")
.                             move           "94",str2
.              elseif (NSELGENDER = "6")
.                             move           "93",str2
.              endif
              if (NSELBASE = "BASE")
               move           "9",str3
              else
               move           "0",str3
              endif
              pack            str16,str3,NSELQTY
              DataListViewSelectC.InsertItem giving result using str16     .Qty Sort Key
              DataListViewSelectC.SetItemText using result,taskname2,1
              DataListViewSelectC.SetItemText using result,NSELNUM,2
              DataListViewSelectC.SetItemText using result,STR13,3
              DataListViewSelectC.SetItemText using result,str25,4
              DataListViewSelectC.SetItemText using result,str15,5
              DataListViewSelectC.SetItemText using result,str1,6
              DataListViewSelectC.SetItemText using result,str2,7
              DataListViewSelectC.SetItemText using result,taskname3,8
              DataListViewSelectC.SetItemText using result,colordim,9
              DataListViewSelectC.SetItemText using result,colordim2,10
.
.             if (NSELEXC = "2")
              if (NSELEXC = "2" & (NSELBASE = "BASE" | NSELBASE = "SEC."))
               move           B1,str8
               move           C0,N52
              else
               call           SelectTestBase4 using NSELLIST,NSELBASE,N52
               add            NSELPRICE,N52
               move           N52,str8
               rep            zfill,str8
              endif
              DataListViewSelectD.InsertItem giving result using str8                     .Price Sort Key
              DataListViewSelectD.SetItemText using result,taskname2,1
              DataListViewSelectD.SetItemText using result,NSELNUM,2
              DataListViewSelectD.SetItemText using result,STR13,3
              DataListViewSelectD.SetItemText using result,str25,4
              DataListViewSelectD.SetItemText using result,str15,5
              DataListViewSelectD.SetItemText using result,str1,6
              DataListViewSelectD.SetItemText using result,str2,7
              DataListViewSelectD.SetItemText using result,taskname3,8
              DataListViewSelectD.SetItemText using result,colordim,9
              DataListViewSelectD.SetItemText using result,colordim2,10
.
              call            Trim using NSELSTATUS
              if (NSELSTATUS = "")
               move           "A",str3
              else
               move           NSELSTATUS,str3
              endif
              DataListViewSelectE.InsertItem giving result using str3                     .Status Sort Key
              DataListViewSelectE.SetItemText using result,taskname2,1
              DataListViewSelectE.SetItemText using result,NSELNUM,2
              DataListViewSelectE.SetItemText using result,STR13,3
              DataListViewSelectE.SetItemText using result,str25,4
              DataListViewSelectE.SetItemText using result,str15,5
              DataListViewSelectE.SetItemText using result,str1,6
              DataListViewSelectE.SetItemText using result,str2,7
              DataListViewSelectE.SetItemText using result,taskname3,8
              DataListViewSelectE.SetItemText using result,colordim,9
              DataListViewSelectE.SetItemText using result,colordim2,10
.
DataLoadSelectRecRecord
              DataSelectRecListView.InsertItem giving result using NSELINDEX
              DataSelectRecListView.SetItemText using result,NSELSNAME,1
              DataSelectRecListView.SetItemText using result,str13,2
              DataSelectRecListView.SetItemText using result,str25,3
              pack            str10,NSELLIST,NSELNUM
              DataSelectRecListView.SetItemText using result,str10,4
              DataSelectRecListView.SetItemText using result,colordim,5
              DataSelectRecListView.SetItemText using result,colordim2,6
              return

DataSetListViewItems LRoutine DimPtr
.DimPtr  = Key Value for Select Record
.Set all Other ListViews to point to same record
              unpack          DimPtr,NSELLIST,NSELNUM
              call            DataSetLVPointer using SelViewFlag
              DataLVPointer.GetItemCount giving result
              if (result > 0)
               sub            C1,result
               for N4,"0",result
                              DataLVPointer.GetItemText giving str10 using N4,8
                              unpack         str10,str6,str4
                              if (NSELLIST = str6 & NSELNUM = str4)
                                             DataLVPointer.SetItemState giving N9 using N4,2,2
                                             DataLVPointer.EnsureVisible using N4,0
                                             break
                              endif
               repeat
              endif
              call            DataSelectSetListView using C1
              return

DataSetListViewRecItems LRoutine DimPtr
.DimPtr  = Key Value for corresponding record for ListView objects in floating Select Window
              DataSelectRecListView.GetItemCount giving result
              if (result > 0)
               sub            C1,result
               for N4,"0",result
                              DataSelectRecListView.GetItemText giving str10 using N4,4
                              if (str10 = DimPtr)
                                             DataSelectRecListView.GetItemText giving NSELSNAME using N4,1
                                             setitem        DataEditSelectRec,0,NSELSNAME
.                                            DataSelectRecListView.SetItemState giving N9 using N4,2,2
.                                            DataSelectRecListView.EnsureVisible using N4,0
                                             break
                              endif
               repeat
              endif
              return

DataLoadSelectRecRecordA
.Establish Foreground Color
              if (NSELINACTIVE = "1")
               move           "0xFF0000",colordim                          .Red
              elseif (NSELSTATUS = "1")
               move           "0x0000FF",colordim                          .Blue
              else
               move           "0x000000",colordim                          .Black
              endif
.Establish Background Color
              if (NSELBASE = "BASE")
               move           "0xFFFF00",colordim2                         .Yellow
              elseif (NSELBASE = "SEC.")
               move           "0xFFFFC0",colordim2                         .Pale Yellow
              else
               move           "0xFFFFFF",colordim2                         .White
              endif
.
              call            Trim using NSELINDEX
              move            C0,N4
              move            NSELINDEX,N4
              move            N4,NSELINDEX
              rep             zfill,NSELINDEX
.
              move            NSELQTY,str10
              call            FormatNumeric using str10,str13
.
              if (NSELEXC = "2" & (NSELBASE = "BASE" | NSELBASE = "SEC."))
               pack           str25,"Exc. Only"
              else
               unpack         NSELPRICE,str5,str3
               call           FormatNumeric using str5,str6
               pack           str9,str6,str3
               pack           NMODFLD,NSELDESC
               rep            zfill,NMODFLD
               move           "D.Load2-NMODKEY",Location
               pack           KeyLocation,"Key: ",NMODFLD
               call           NMODKEY
               call           Trim using NMODDESC
               if (NSELBASE = "BASE" | NSELBASE = "SEC." | NSELBASE = "    ")
                              clear          str1
               else                                         .if (NSELPRICE > 0)
                              move           "+",str1
.              else
.                             clear          str1
               endif
               pack           str25,str1,str9,NMODDESC
              endif
              DataListViewSelect.SetItemText using result,str25,4
.
              call            DataLoadSelectRecRecord
              return

DataOwnerLostFocus
              setitem         DataStatOwnName,0,OWNOCPY
              setitem         DataStatOwnAdd1,0,OWNLOSA
              setitem         DataStatOwnAdd2,0,""
              clear           taskname
              call Trim using OWNLOCTY
              if (OWNLOCTY <> "")
               append         OWNLOCTY,taskname
               append         COMMA,taskname
               append         B1,taskname
              endif
              append          OWNLOS,taskname
              append          B1,taskname
              append          OWNLOZC,taskname
              reset           taskname
              setitem         DataStatOwnAdd3,0,taskname
              setitem         DataStatOwnCnt,0,OWNLONM
              call            Trim using OWNTELE
              if (OWNTELE <> "")
               count          howmany,OWNTELE
               if (howmany = 10)
                              unpack         OWNTELE,str3,str2,str1,str4
                              pack           str15,"(",str3,") ",str2,str1,DASH,str4
               elseif (howmany = 7)
                              unpack         OWNTELE,str3,str4
                              pack           str15,str3,DASH,str4
               else
                              move           OWNTELE,str15
               endif
              else
               clear          str15
              endif
              setitem         DataStatOwnPhone,0,str15
.begin patch 1.9
          sETITEM   DataEditText001,0,""
          if        (owncomp <> "000000" & Owncomp <> "")
          packkey   compfld,owncomp
          call      compkey
                    if        not over
                    Packkey   CNCTFLD,OWncomp,owncont
                    call      CNCTKEY
                              if        not over
                              setitem   DataStatOwnCnt,0,CNCTFNAME
                              sETITEM   DataEditText001,0,Owncont
                              endif
                    endif
          endif     
                    
.end patch 1.9
              return

DataLoadSamples LRoutine DimPtr
.Called       by:  DataLoadScreen
.This is used to dynamically change the      Samples        when the Mailer               is changed!!!
.Load Samples ComboBox
              call            Trim using DimPtr
              if (DimPtr <> "")
               clear          NSMPFLD
               clear          str3
               move           C0,N3
               loop
                              add            C1,N3
                              move           N3,str3
                              rep            zfill,str3
                              pack           NSMPFLD,DimPtr,str3
                              move           "D.LoadSamples-NSMPKEY",Location
                              pack           KeyLocation,"Key: ",NSMPFLD
                              call           NSMPKEY
                              if not over
                                      DataListViewSamples.InsertItem giving N9 using str3
                                      DataListViewSamples.SetItemText using N9,NSMPDES1,1
                              endif
                              until (N3 > 50)               .THIS WILL NEED               TO BE INCREASED               IF MORE        THAN 50        SAMPLES        EXIST!!
               repeat
              endif
              return

DataLoadSelectFields
              setitem         DataEditSelectList,0,NSELLIST
              setitem         DataEditSelectNum,0,NSELNUM
              call            FormatNumeric using NSELQTY,str13
              setitem         DataEditSelectQty,0,str13
              move            C0,N1
              move            NSELEXC,N1
              setitem         DataComboSelectExcRent,0,N1
.             if (NSELEXC = "2")                            .Exchange Only
              if (NSELEXC = "2" & (NSELBASE = "BASE" | NSELBASE = "SEC."))                .Exchange Only & a Base
               setitem        DataEditSelectPrice,0,""
               setitem        DataComboSelectModifier,0,1
              else
               unpack         NSELPRICE,str5,str3
               call           FormatNumeric using str5,str6
               call           Trim using NSELBASE
               if (NSELBASE = "BASE" | NSELBASE = "SEC." | NSELBASE = "    ")
                              clear          str1
               else                          .if (NSELPRICE > 0)
                              move           "+",str1
.              else
.                             clear          str1
               endif
               pack           str10,str1,str6,str3
               setitem        DataEditSelectPrice,0,str10
               call           Trim using NSELDESC
               move           C0,N3
               move           NSELDESC,N3
               add            C1,N3
               setitem        DataComboSelectModifier,0,N3
              endif
.drewtest
              call            DataLoadComboSelectBaseItem using NSELBASE
.
              setitem         DataEditSelectIndex,0,NSELINDEX
.
              move            C0,N2
              move            NSELSTATUS,N2
              add             C1,N2
              setitem         DataComboSelectStatus,0,N2
              if (NSELPCOMM = "1")
               move           C1,N1
              else
               move           C0,N1
              endif
              setitem         DataCheckSelectComm,0,N1
              if (NSELINACTIVE = "1")
               move           C1,N1
              else
               move           C0,N1
              endif
              setitem         DataCheckSelectInactive,0,N1
              if (NSELNOTES = "1")
               move           C1,N1
              else
               move           C0,N1
              endif
              setitem         DataCheckSelectNotes,0,N1
.
              setitem         DataEditSelectName,0,NSELSNAME
              return

DataLoadComboSelectBaseItem Routine DimPtr
              call            Trim using DimPtr
              if (DimPtr = "")
               move           C1,result
              else
               for result,"2","9999"                        .Skip first record, which is always blank
                              getitem        DataComboSelectBase,result,str4
                              rep            lowup,str4
                              if ((str4 = DimPtr) | (DimPtr = "SEC." & str4 = "SECO"))
                                             break
                              else
.Will only come here if NSELBASE has a value that is no longer a valid Base/Secondary Base
                                             call           Trim using str4
                                             if (str4 = "")
                                                            move           C1,result
                                                            break
                                             endif
                              endif
               repeat
              endif
              setitem         DataComboSelectBase,0,result
              return

LoadDataListViewRefHeaders
              for N9 from "1" to "5"
               if (N9 = 1)
                              move           "AAA",str3
                              move           "*****Addressing*****",str45
               elseif (N9 = 2)
                              move           "LLL",str3
                              move           "*****Selection******",str45
               elseif (N9 = 3)
                              move           "RRR",str3
                              move           "*****Arrangement*****",str45
               elseif (N9 = 4)
                              move           "SSS",str3
                              move           "********Source*******",str45
               elseif (N9 = 5)
                              move           "TTT",str3
                              move           "*******Category******",str45
               endif
                      DataListViewRef.InsertItem giving N10 using str3
               DataListViewRef.SetItemText using N10,str45,1
              repeat
              return

DataLoadRefFields LRoutine FrmPtr
              if (FrmPtr = C1)               .Addressing
.NADDVARS
               pack           NREFFLD,"A",NADDNUM
               call           DataSetListView2 using NREFFLD,N1
               if (N1 = C1)   .No Record
                              setitem        DataEditRefList,0,""
                              setitem        DataEditRefPrice,0,""
                              setitem        DataComboRefModifier,0,1
               else
                              setitem        DataEditRefList,0,NADDLIST
                              unpack         NADDPRICE,str5,str3
                              call           FormatNumeric using str5,str6
                              pack           str9,str6,str3
                              setitem        DataEditRefPrice,0,str9
                              call           Trim using NADDDESC
                              move           C0,N2
                              move           NADDDESC,N2
                              add            C1,N2
                              setitem        DataComboRefModifier,0,N2
               endif
               setitem        DataEditRefQty,0,""
               setitem        DataEditRefPer,0,""
              elseif (FrmPtr = C2)    .Selection
.NSLTVARS
               pack           NREFFLD,"L",NSLTNUM
               call           DataSetListView2 using NREFFLD,N1
               if (N1 = C1)   .No Record
                              setitem        DataEditRefList,0,""
                              setitem        DataEditRefPrice,0,""
                              setitem        DataComboRefModifier,0,1
                              setitem        DataEditRefQty,0,""
               else
                              setitem        DataEditRefList,0,NSLTLIST
                              unpack         NSLTPRICE,str5,str3
                              call           FormatNumeric using str5,str6
                              pack           str9,str6,str3
                              setitem        DataEditRefPrice,0,str9
                              call           Trim using NSLTDESC
                              move           C0,N2
                              move           NSLTDESC,N2
                              add            C1,N2
                              setitem        DataComboRefModifier,0,N2
                              move           NSLTQTY,str9
                              call           FormatNumeric using str9,str11
                              setitem        DataEditRefQty,0,str11
               endif
               setitem        DataEditRefPer,0,""
.             elseif (FrmPtr = C3)    .Media
..NMEDVARS
.              pack           NREFFLD,"M",NMEDNUM
.              call           DataSetListView2 using NREFFLD,N1
.              setitem        DataEditRefPrice,0,""
.              setitem        DataComboRefModifier,0,1
.              setitem        DataEditRefQty,0,""
.              setitem        DataEditRefPer,0,""
              elseif (FrmPtr = C4)    .Arrangement
.NARRVARS
               pack           NREFFLD,"R",NARRNUM
               call           DataSetListView2 using NREFFLD,N1
               setitem        DataEditRefList,0,NARRLIST
               setitem        DataEditRefPrice,0,""
               setitem        DataComboRefModifier,0,1
               setitem        DataEditRefQty,0,""
               setitem        DataEditRefPer,0,""
              elseif (FrmPtr = C5)    .Source
.NSRCVARS
               pack           NREFFLD,"S",NSRCNUM
               call           DataSetListView2 using NREFFLD,N1
               setitem        DataEditRefPrice,0,""
               setitem        DataComboRefModifier,0,1
               setitem        DataEditRefQty,0,""
               if (N1 = C1)   .No Record
                              setitem        DataEditRefList,0,""
                              setitem        DataEditRefPer,0,""
               else
                              setitem        DataEditRefList,0,NSRCLIST
                              call           Trim using NSRCPER
                              move           C0,N3
                              move           NSRCPER,N3
                              move           N3,str3
                              call           Trim using str3
                              pack           str4,str3,PRC
                              setitem        DataEditRefPer,0,str4
               endif
              elseif (FrmPtr = C6)    .Category
.NCATVARS
               pack           NREFFLD,"T",NCATCODE,NCATNUM
               call           DataSetListView2 using NREFFLD,N1
               setitem        DataEditRefList,0,NCATLIST
               setitem        DataEditRefPrice,0,""
               setitem        DataComboRefModifier,0,1
               setitem        DataEditRefQty,0,""
               setitem        DataEditRefPer,0,""
              endif
              return

DataSetListView2 LRoutine DimPtr,FrmPtr
              call            DataSetListView2B using DimPtr
              DataListView2.GetItemCount giving result
              sub             C1,result
              for howmany,C0,result
               DataListView2.GetItemText giving str4 using howmany
               if (str4 = DimPtr)
                              break
               endif
              repeat
              if (howmany > result)          .Did not locate, choose blank item
               move           result,howmany
               move           C1,FrmPtr
              else
               move           C0,FrmPtr
              endif
              DataListView2.SetItemState giving N9 using howmany,2,2
              DataListView2.EnsureVisible using howmany,0
              return

DataSetListView2B LRoutine DimPtr
              DataListView2B.GetItemCount giving result
              sub             C1,result
              for howmany,C0,result
               DataListView2B.GetItemText giving str4 using howmany,3
               if (str4 = DimPtr)
                              break
               endif
              repeat
              if (howmany > result)          .Did not locate, choose blank item
               move           C1,howmany
              endif
              DataListView2B.SetItemState giving N9 using howmany,2,2
              DataListView2B.EnsureVisible using howmany,0
              return

DataVerifyData
.Screen 1
.begin patch 2.05
           Getitem         DataCheckBlocked,0,N1
           if         (n1 = c1)
           move       yes,Ndatflag1
           else
           move       b1,ndatflag1
           endif

           Getitem         DataCheckCounts,0,N1
           if         (n1 = c1)
           move       yes,Ndatflag3
           else
           move       b1,ndatflag3
           endif

           Getitem         DataCheckGender,0,n1
           if         (n1 = c1)
           move       yes,Ndatflag2
           else
           move       b1,ndatflag2
           endif
.end patch 2.05

.begin patch 2.06
         Getitem         DataCheckmlrRestrictions,0,n1
           if         (n1 = c1)
           move       yes,Ndatflag4
           else
           move       b1,ndatflag4
           endif
.end patch 2.06

.LIST NUMBER
              if (SecFlag = YES)
               getitem        DataEditListNum,0,LSTNUM
               call           ZFillIt using LSTNUM,C0
.START PATCH 1.1.2 ADDED LOGIC
              elseif (NewFlag <> YES)
               getitem        DataEditListNum,0,NDATFLD
.END PATCH 1.1.2 ADDED LOGIC
              endif
.STATUS
.begin patch 2.0          
          move      status,str1
.end patch 2.0          
              getitem         DataComboStatus,0,N1
              if (N1 = 2)
               move           "W",STATUS
              elseif (N1 = 3)
               move           "T",STATUS
              else
               move           b1,STATUS
              endif
.begin patch 2.0          
          clear     statChange
          if        (Status <> str1)
          move      yes,StatChange
          endif
.end patch 2.0          
.EXCLUSIVE CODE
.begin patch 1.2.9
          clear n1
          clear n2
.begin patch 2.0          
          move      elstcde,str1
.end patch 2.0          
          
          getitem   DataExclComboBox,N1,N2                 ;Exclusive?
          clear  str35
          getitem   DataExclComboBox,N2,str35
          if (str35 <> "")
                    Cmatch    "N",str35
                    if equal
                              move "C" to Elstcde
                    endif
                    reset str35
                    CMatch "P",str35
                    if equal
                              move "P" to Elstcde
                    endif
                    reset str35
          else
                    Move      b1,Elstcde
          endif
.begin patch 2.0          
          clear     exclChange
          if        (Elstcde <> str1)
          move      yes,exclChange
          endif
          if        (statChange = yes & (Elstcde = "C" | Elstcde = "P") & LSTNUM <> "")
          Move      "Ndat0001 - Datacards",MailSubjct
          Move      "Creques@nincal.com",MailFrom
          Move      "Creques@nincal.com",Mailreply
          Move      "GemmaSpranza@nincal.com,Creques@nincal.com",MailTo
          Clear     MailBody
            Append         "List: ",Mailbody
            Append         lstnum,Mailbody
            Append         b1,Mailbody   
            Append         Mlstname,Mailbody         
          Append    CRLF,MailBOdy
            Append         "Withdrawn status has changed. ",Mailbody
            append    "<BR>",mailbody
            append    "status byte is now : ",mailbody
            append    status,mailbody
            append    "<BR>",mailbody
            append    "Typist: ",mailbody
            append    Inits,mailbody
            
           reset    Mailbody
           clear    mailattach
           call     sendmail          
           endif

          if        (str1 <> ELstcde & LSTNUM <> "")           .status changed
          Move      "Ndat0001 - Datacards",MailSubjct
          Move      "Creques@nincal.com",MailFrom
          Move      "GemmaSpranza@nincal.com,Creques@nincal.com",MailTo
          Clear     MailBody
            Append         "List: ",Mailbody
            Append         lstnum,Mailbody
            Append         b1,Mailbody   
            Append         Mlstname,Mailbody         
          Append    CRLF,MailBOdy
            Append         "Exclusive management status has changed ",Mailbody
            append    "<BR>",mailbody
            append    "status byte is now : ",mailbody
            if        (Elstcde = "C")
            append    "NIN Managed",mailbody
            elseif    (Elstcde = "P")
            append    "PL Managed",mailbody
            Else          
            append    "NOT Managed",mailbody
            endif
            append    "<BR>",mailbody
            append    "Typist: ",mailbody
            append    Inits,mailbody

           reset    Mailbody
           clear    mailattach
           call     sendmail
          endif
.end patch 2.0 
.begin patch 2.09
           if         (str1 <> ELstcde & LSTNUM <> "")           .status changed
                      if         (Elstcde <> "C" & Elstcde <> "P")         .no longer managed file
                      alert type=yesno1," No Longer NIN Managed? (if yes, I will delete LRA & Order reminders)",n1
                      if         (n1=6)    . 6 = yes , 7 = no          
                      Elseif    (n1=7)    . 6 = yes , 7 = no          
                      return                                               .they are not sure                                                                               
                      endif
                      MOVe       "F",NDATLRA                               .turn of any LRA requirement  
                      move       C0,N1
                      setitem    DataCheckLRA,0,n1
                      Clear      NdatLRADte 
                      move       b1,ndatflag1                              .turn off booking/order flags  
                      move       b1,ndatflag2
                      move       b1,ndatflag3
                      move       b1,ndatflag4
                      setitem    DataCheckBlocked,0,0
                      setitem    DataCheckCounts,0,0
                      setitem    DataCheckGender,0,0
                      setitem    DataCheckmlrRestrictions,0,0
                    endif                  
          endif
.end patch 2.09

.              getitem         DataCheckExclusive,0,N1
.              if (N1 = 1)
.           alert type=yesno1," Names in the News Excusive? (if No, I will mark as Pacific Lists)",n1
.                   if (n1=6)    . 6 = yes , 7 = no          
.                   move           "C",ELSTCDE
.                   Elseif (n1=7)    . 6 = yes , 7 = no          
.                   move           "P",ELSTCDE
.                   endif
.            else
.         clear          ELSTCDE
                    
.end patch 1.2.9
.              endif
.NEW CODE
              getitem         DataCheckNew,0,N1
              if (N1 = 1)
               move           YES,NLSTCDE
              else
               clear          NLSTCDE
              endif
.MASTER LIST NAME
              getitem         DataEditMListName,0,MLSTNAME
              call            Trim using MLSTNAME
              if (MLSTNAME = "")
               alert          caution,"Valid Master Name Required!",result
               setfocus DataEditMListName
               move           YES,ReturnFlag
               return
              endif
.ORDER LIST NAME
              getitem         DataEditListName,0,OLSTNAME
              call            Trim using OLSTNAME
              if (OLSTNAME = "")
               move           MLSTNAME,OLSTNAME
               setitem        DataEditListName,0,OLSTNAME
              endif
.REVISION INFO
              getitem         DataEditRevised,0,str10
              call            Trim using str10
              if (str10 <> "")
               call           RemoveChar using str10,SLASH
               unpack         str10,MM,DD,CC,YY
               if ((MM = "" | MM > "12" | MM < "01") | (DD = "" | DD > "31" | DD < "01") | (CC = 0 | CC > 25 | CC < 19) | (YY = ""))
                              alert          caution,"Valid Revision Date Required!",result
                              setfocus DataEditRevised
                              move           YES,ReturnFlag
                              return
               endif
               pack           REVDATE,CC,YY,MM,DD
              else
               clear          REVDATE
              endif
.START PATCH 1.1 REPLACED LOGIC
.             call            Trim using NUSEUSER
.             scan            B1,NUSEUSER
.             if equal
.              bump           NUSEUSER
.              movefptr NUSEUSER,N9
.              setlptr NUSEUSER,N9
.             endif
.             reset           NUSEUSER
.             if (NUSEUSER <> "")
.              pack           PASSWORD,NUSEUSER,PERIOD
.             else
.              clear          NUSEUSER
.             endif
              if (SecFlag = YES)
               getitem        DataEditRevised2,0,PASSWORD
              elseif (REVDATE <> HoldREVDATE | NewFlag = YES)
               reset          NUSEUSER
               if (NUSEUSER <> "")
                              pack           PASSWORD,NUSEUSER,PERIOD
               else
                              move           "Unknown",PASSWORD
               endif
              endif
.END PATCH 1.1 REPLACED LOGIC
.START PATCH 1.1 ADDED LOGIC
.VERIFICATION INFO
              getitem         DataEditUpdate,0,str10
              call            Trim using str10
              if (str10 <> "")
               call           RemoveChar using str10,SLASH
               unpack         str10,MM,DD,CC,YY
               if ((MM = "" | MM > "12" | MM < "01") | (DD = "" | DD > "31" | DD < "01") | (CC = 0 | CC > 25 | CC < 19) | (YY = ""))
                              alert          caution,"Valid Verification Date Required!",result
                              setfocus DataEditUpdate
                              move           YES,ReturnFlag
                              return
               endif
               pack           NDATUPDDATE,CC,YY,MM,DD
              else
               clear          NDATUPDDATE
              endif
.
              call            Trim using NUSEUSER
              scan            B1,NUSEUSER
              if equal
               bump           NUSEUSER
               movefptr NUSEUSER,N9
               setlptr NUSEUSER,N9
              endif
              reset           NUSEUSER
              if (NUSEUSER <> "")
               pack           NDATUPDINIT,NUSEUSER,PERIOD
              else
               clear          NDATUPDINIT
              endif
.END PATCH 1.1 ADDED LOGIC
.OWNER INFO
              getitem         DataEditOwnNum,0,OWNNUM
              call            Trim using OWNNUM
              if (OWNNUM = "")
               alert          caution,"Valid Owner Number Required!",result
               setfocus DataEditOwnNum
               move           YES,ReturnFlag
               return
              endif
              pack            NOWNFLD,OWNNUM
              move            "Verify-NOWNTST",Location
              pack            KeyLocation,"Key: ",NOWNFLD
              call            NOWNTST
              if over
               alert          caution,"Valid Owner Number Required!",result
               setfocus DataEditOwnNum
               move           YES,ReturnFlag
               return
              endif
.Temporary measure until Client file is created
              call            ZFillIt using OWNNUM,C0
.PAY-TO INFO
.             getitem         DataEditPayNum,0,DATPAY
.             call            ZFillIt using DATPAY
.             pack            NPAYFLD,DATPAY
.             move            "Verify-NPAYTST",Location
.             pack            KeyLocation,"Key: ",NPAYFLD
.             call            NPAYTST
.             if over
.              alert          caution,"Valid Pay-To Number Required!",result
.              setfocus DataEditPayNum
.              move           YES,ReturnFlag
.              return
.             endif
..MANAGER INFO
..Will need to read Client File for Manager info
.             getitem         DataEditManagerNum,0,DATMAN
.             call            ZFillIt using DATMAN
.FULFILLMENT INFO
.Will need to read Client File for Fulfillment info
              getitem         DataEditFulfillNum,0,DATFUL
              call            Trim using DATFUL
.START PATCH 1.2.6 REPLACED LOGIC
.                if (DATFUL <> "")
..                  call           ZFillIt using NDATFUL
.                   pack           NFULFLD,DATFUL
.                   move           C1,NFULPATH
.                   move           "Verify-NFULTST",Location
.                   pack           KeyLocation,NFULFLD
.                   call           NFULTST
.                   if over
.                              alert          caution,"Valid Fulfillment Number Required!",result
.                              setfocus DataEditFulfillNum
.                              move           YES,ReturnFlag
.                              return
.                   endif
.              else
.                   clear          NFULCOMP
.              endif
.              setitem         DataStatFulfillName,0,NFULCOMP
                    if (DATFUL <> "")
                              pack      COMPFLD, DATFUL
                              rep       zfill, COMPFLD
                              move           C1,COMPPATH
                              move      "Verify-COMPKEY",Location
                              pack      KeyLocation,COMPFLD
                              call      COMPKEY
                              if over
                                        alert     caution,"Valid Fulfillment Number Required!", result
                                        setfocus  DataEditFulfillNum
                                        move      YES,ReturnFlag
                                        return
                              else
                                        if (COMPSVBFLG <> "T")
                                                  alert     caution,"Valid Fulfillment Number Required!", result
                                                  setfocus  DataEditFulfillNum
                                                  move      YES,ReturnFlag
                                                  return
                                        endif
                              endif
                    else      // datful = ""
                              clear     COMPCOMP
                    endif
                    setitem   DataStatFulfillName,0,COMPCOMP
.END PATCH  1.2.6 REPLACED LOGIC
.MAILER INFO
              DataListViewSamples.DeleteAllItems giving N9
.START PATCH 1.2.2 REPLACED LOGIC
.             getitem         DataEditMlrNum,0,str4
.             call            Trim using str4
.             if (str4 <> "")
.              move           C1,NMLRPATH
.              pack           MKEY,str4,"000"
.              move           "Verify-NMLRTST",Location
.              pack           KeyLocation,"Key: ",MKEY
.              call           NMLRKEY
.              if over
.                             alert          caution,"Valid Mailer Number Required!",result
.                             setfocus DataEditMlrNum
.                             move           YES,ReturnFlag
.                             return
.              endif
.              if (NewFlag <> YES)           .Modifying existing record - Creation of new XREF Mailer will happen for New Datacards under Save button
.                             move           C1,NXRFPATH
.                             clear          NXRFFLD2
.                             pack           NXRFFLD,LSTNUM
.                             move           "Verify-NXRFKEY",Location
.                             pack           KeyLocation,"Key: ",NXRFFLD
.                             call           NXRFKEY
.                             if not over
.                                            if (str4 <> NXRFMLR)
.                                                           move           str4,NXRFMLR
.                                                           move           "Verify-NXRFUPD",Location
.                                                           call           NXRFUPD
.                                            endif
.                             else
.                                            move           str4,NXRFMLR
.                                            move           LSTNUM,NXRFLIST
.                                            move           "Verify-NXRFWRT",Location
.                                            call           NXRFWRT
.                                            setitem        DataEditMlrNum,0,str4
.                                            setitem        DataStatMlrName,0,MCOMP
.                             endif
.              endif
.................................
              getitem         DataEditMlrNum,0,str6
              call            Trim using str6
              if (str6 <> "")
               pack           COMPFLD,str6
               move           "Verify-COMPKEY",Location
               pack           KeyLocation,"Key: ",COMPFLD
               call           COMPKEY
               if over
                              alert          caution,"Valid Mailer Number Required!",result
                              setfocus DataEditMlrNum
                              move           YES,ReturnFlag
                              return
               elseif (COMPMLRFLG <> "T")
                              alert          caution,"Valid Mailer Number Required!",result
                              setfocus DataEditMlrNum
                              move           YES,ReturnFlag
                              return
               endif
               if (NewFlag <> YES)           .Modifying existing record - Creation of new XREF Mailer will happen for New Datacards under Save button
                              move           C1,NXRFPATH
                              clear          NXRFFLD2
                              pack           NXRFFLD,LSTNUM
                              move           "Verify-NXRFKEY",Location
                              pack           KeyLocation,"Key: ",NXRFFLD
                              call           NXRFKEY
                              if not over
                                             if (str6 <> NXRFMLR)
                                                            move           str6,NXRFMLR
                                                            move           "Verify-NXRFUPD",Location
                                                            call           NXRFUPD
                                             endif
                              else
                                             move           str6,NXRFMLR
                                             move           LSTNUM,NXRFLIST
                                             move           "Verify-NXRFWRT",Location
                                             call           NXRFWRT
                                             setitem        DataEditMlrNum,0,str6
                                             setitem        DataStatMlrName,0,COMPCOMP
                              endif
               endif
.END PATCH 1.2.2 REPLACED LOGIC
              elseif (NewFlag <> YES)
               move           C1,NXRFPATH
               clear          NXRFFLD2
               pack           NXRFFLD,LSTNUM
               move           "Verify2-NXRFKEY",Location
               pack           KeyLocation,"Key: ",NXRFFLD
               call           NXRFKEY
               if not over
                              pack           taskname,"There is a previously associated Mailer!",newline,"Are you sure you want to delete the association?"
                              alert          plain,taskname,result
                              if (result = 1)
                                             move           "Verify2-NXRFDEL",Location
                                             call           NXRFDEL
                                             setitem        DataEditMlrNum,0,""
                                             setitem        DataStatMlrName,0,""
                              else
                                             setitem        DataEditMlrNum,0,NXRFMLR
                                             setfocus DataEditMlrNum
                                             move           YES,ReturnFlag
                                             return
                              endif
               else
                              setitem        DataEditMlrNum,0,""
                              setitem        DataStatMlrName,0,""
               endif
              else
               setitem        DataStatMlrName,0,""
              endif
.UNIVERSE
              getitem         DataEditUniverse,0,str13
              call            Trim using str13
              call            RemoveChar using str13,COMMA
              move            C0,UNIVERSE
              move            str13,UNIVERSE
.DATE
              if (SecFlag = YES)
               getitem        DataEditDate,0,str10
               call           Trim using str10
               call           RemoveChar using str10,SLASH
               count          result,str10
               if (result <> 8)
                              alert          caution,"Date not formatted as MMDDYYY.  I'm not gonna save it!",result
               else
                              unpack         str10,MM,DD,CC,YY
                              pack           NEWDATE,CC,YY,MM,DD
               endif
              endif
.GENDER INFO
.         call      debug

          Getitem   DataEditFem,0,str25
          if        (str25 <> "")
          move      "%",str1
          call      removechar using str1,str25
          call      trim using str25
          move      str25,NdatFem
          endif
          Getitem   DataEditMEn,0,str25
          if        (str25 <> "")
          move      "%",str1
          call      removechar using str1,str25
          call      trim using str25
          move      str25,NdatMen
          endif
          
          if        (Ndatfem > 0 or Ndatmen > 0)       we are using the new vars
          else
              getitem         DataEditGender,0,SEX

          endif
.              setprop         DataEditGender,enabled=0,bgcolor=grey
.              setprop         DataEditGender,visible=0
.              setprop         DataEditFem,enabled=1,bgcolor=white
.              setprop         DataEditFem,visible=1
.              setprop         DataStatTextFem,enabled=1,bgcolor=white
.              setprop         DataStatTextFem,visible=1

.              getitem         DataEditGender,0,SEX
.              call       Debug
.MINIMUM INFO
              getitem         DataEditMinimum,0,MIN
.COMMISSION
.START PATCH 1.2.1 REPLACED LOGIC
.             getitem         DataEditCommission,0,COMMPER
              move            C0,N32
              getitem         DataEditCommission,0,str6
              call            Trim using str6
              if (str6 <> "")
               move           str6,N32
               move           N32,str6
               call           Trim using str6
               setitem        DataEditCommission,0,str6
              endif
              move            N32,COMMPER
.END PATCH 1.2.1 REPLACED LOGIC
.HOTLINE
              getitem         DataCheckHotline,0,N1
              if (N1 = C1)
               move           YES,HOTLINE
              else
               clear          HOTLINE
              endif
..EXCHANGE ONLY
.             getitem         DataComboExcRent,0,N1
.             move            N1,NDATEXCH
.             if (NDATEXCH = "2" | NDATEXCH = "3")
.              move           C0,result
.              pack           NSELFLD1,"01X",LSTNUM
.              clear          NSELFLD2
.              move           "D.Verify-NSELAIM",Location
.              pack           KeyLocation,"Key: ",NSELFLD1
.              call           NSELAIM
.              loop
.                             until over
.                             if (NSELEXC <> NDATEXCH)
.                                            if (result = C0)
.                                                           pack           taskname,"There are Select records with different Exchange Status values!",newline,"Do you wish to update all the Select records?"
.                                                           alert          plain,taskname,result
.                                            endif
.                                            if (result = 1)
.                                                           move           NDATEXCH,NSELEXC
.                                                           call           NSELUPD
.                                            elseif (result = 2)
.                                                           break
.                                            elseif (result = 3)
.                                                           setfocus DataComboExcRent
.                                                           move           YES,ReturnFlag
.                                                           return
.                                            endif
.                             endif
.                             move           "D.Verify-NSELKG",Location
.                             pack           KeyLocation,"Key: ",NSELFLD1
.                             call           NSELKG
.              repeat
.             endif
.WEBSITE ALLOWED TO BE GIVEN OUT
              getitem         DataCheckWeb,0,N1
              if (N1 = 1)
               move           N1,NDATWEB
              else
               clear          NDATWEB
              endif
.OFFICE USE ONLY
              getitem         DataCheckOffice,0,N1
              if (N1 = 1)
               move           N1,NDATOFF
              else
               clear          NDATOFF
              endif
.CONVERTED BYTE - EVENTUALLY WILL BE OBSOLETE!!!!!!!!!
              getitem         DataCheckConverted,0,N1
              move            N1,NDATCONV
.START PATCH 1.1.5 ADDED LOGIC
.USAGE
              getitem         DataCheckUsage,0,N1
              if (N1 = C0)
               move           "F",NDATLUSAGE
              else
               clear          NDATLUSAGE
              endif
.END PATCH 1.1.5 ADDED LOGIC
.begin patch 1.97
              getitem         DataCheckLRA,0,N1
              if (N1 = C0)
               move           "F",NDATLra
                Clear NdatLRADte 
              else
               move           "T",NDATLra
.               call debug
               getitem           Data3ComboBoxLRARenewal,0,n2
               move              n2,NDatLRADte,n2
              endif

.end patch 1.97
.UNIT DATA
              getitem         DataEditTextUnitData,0,UNITDATA
.DELIVERY CODE
              getitem         DataComboDelivery,0,result
              getitem         DataComboDelivery,result,taskname
              unpack          taskname,NREFDESC,DELCODE
.SAMPLE CODE
              getitem         DataComboSample,0,result
              getitem         DataComboSample,result,taskname
              unpack          taskname,NREFDESC,SAMPLE
.NET CODE
              getitem         DataComboNet,0,result
              getitem         DataComboNet,result,taskname
              unpack          taskname,NREFDESC,NETNAME
              if (NETNAME = "N002")
               getitem        DataEditNet,0,NETINFO
              else
               clear          NETINFO
              endif
.CLEAN CODE
              getitem         DataComboClean,0,result
              getitem         DataComboClean,result,taskname
              unpack          taskname,NREFDESC,CLEANCDE
              if (CLEANCDE = "C002")
               getitem        DataEditClean,0,CLNINFO
              else
               clear          CLNINFO
              endif
              return

.START PATCH 1.1.5 ADDED LOGIC
Data3VerifyData
              getitem         Data3EditListU,0,NUSGLIST
.START PATCH 1.1.6 ADDED LOGIC
              call            Trim using NUSGLIST
              if (NUSGLIST = "")
               alert          caution,"Valid List Required!",result
               move           YES,ReturnFlag4
               setfocus Data3EditListU
               return
              endif
.END PATCH 1.1.6 ADDED LOGIC
              call            ZFillIt using NUSGLIST,C0
              packkey         NDATFLD,NUSGLIST
              move            "Data3Ver-NDATTST",Location
              pack            KeyLocation,"Key: ",NDATFLD
              move            C1,NDATPATH
              call            NDATTST
              if over
               alert          caution,"Valid List Required!",result
               move           YES,ReturnFlag4
               setfocus Data3EditListU
               return
              endif
.
.START PATCH 1.2.2 REPLACED LOGIC
.             getitem         Data3EditMlrU,0,str4
..START PATCH 1.1.6 ADDED LOGIC
.             call            Trim using str4
.             if (str4 = "")
.              alert          caution,"Valid Mailer Required!",result
.              move           YES,ReturnFlag4
.              setfocus Data3EditMlrU
.              return
.             endif
..END PATCH 1.1.6 ADDED LOGIC
.             call            ZFillIt using str4,C0
.             move            "Data3Ver-COMPKEY3",Location
.             pack            COMPFLD3,str4
.             pack            KeyLocation,"Key: ",COMPFLD3
.             call            COMPKEY3
.             if over
.              alert          caution,"Valid Mailer Required!",result
.              move           YES,ReturnFlag4
.              setfocus Data3EditMlrU
.              return
.             endif
.................................
              getitem         Data3EditMlrU,0,str6
              call            Trim using str6
              if (str6 = "")
               alert          caution,"Valid Mailer Required!",result
               move           YES,ReturnFlag4
               setfocus Data3EditMlrU
               return
              endif
              call            ZFillIt using str6,C0
              move            "Data3Ver-COMPKEY",Location
              pack            COMPFLD,str6
              pack            KeyLocation,"Key: ",COMPFLD
              call            COMPKEY
              if over
               alert          caution,"Valid Mailer Required!",result
               move           YES,ReturnFlag4
               setfocus Data3EditMlrU
               return
              elseif (COMPMLRFLG <> "T")
               alert          caution,"Valid Mailer Required!",result
               move           YES,ReturnFlag4
               setfocus Data3EditMlrU
               return
              endif
.END PATCH 1.2.2 REPLACED LOGIC
              move            COMPNUM,NUSGMLR
.Test for dupes
              pack            NUSGFLD,NUSGLIST,NUSGMLR
              move            "Data3Ver-NUSGTST",Location
              pack            KeyLocation,NUSGFLD
              call            NUSGTST
              if not over
               alert          caution,"This List/Mailer combination already exists!!",result
               move           YES,ReturnFlag4
               setfocus Data3EditMlrU
               return
              endif
.
.START PATCH 1.1.6 REPLACED LOGIC
.             move            timestamp,NUSGDATE
              getitem         Data3EditDateU,0,str10
              call            Trim using str10
              if (str10 = "")
               move           timestamp,NUSGDATE
              else
               call           RemoveChar using str10,SLASH
               type           str10
               if equal
                              unpack         str10,MM,DD,CC,YY
                              pack           NUSGDATE,CC,YY,MM,DD
                              rep            zfill,NUSGDATE
               else
                              move           timestamp,NUSGDATE
               endif
              endif
.END PATCH 1.1.6 REPLACED LOGIC
              move            INITS,NUSGINITS
.             getitem         Data3ComboCodeU,0,N1
              move            C2,NUSGCODE
.END PATCH 1.1.5 ADDED LOGIC

DataVerifySelect
              getitem         DataEditSelectName,0,NSELSNAME
              call            Trim using NSELSNAME
              if (NSELSNAME = "")
               alert          caution,"Valid Select Name Required!",result
               move           YES,ReturnFlag2
               setfocus DataEditSelectName
               return
              endif
              getitem         DataEditSelectList,0,NSELLIST
.START PATCH 1.2.0 ADDED LOGIC
              call             Trim using NSELLIST
              if (NSELLIST = "")
               alert          caution,"Valid List Number Required!",result
               move           YES,ReturnFlag2
               setfocus DataEditSelectList
               return
              endif
.END PATCH 1.2.0 ADDED LOGIC
.Test New records
              if (NewFlag2 = YES)
               call           ZFillIt using NSELLIST
               getitem        DataEditListNum,0,str6
               if (NSELLIST <> str6)
                              pack           NDATFLD,NSELLIST
                              move           "Sel.Verify-NDATTST",Location
                              pack           KeyLocation,"Key: ",NDATFLD
                              call           NDATTST
                              if over
                                             alert          caution,"Valid List Number Required!",result
                                             move           YES,ReturnFlag2
                                             setfocus DataEditSelectList
                                             return
.START PATCH 1.1.7 ADDED LOGIC
                              elseif (HoldFlag <> YES)
                                             call           GetDatacardExclusive using NDATFLD,str1
                                             if (str1 = "C")
                                                            alert          note,"Password Needed to create/modify Exclusive Selects!",result
                                                            setitem        PasswordEdit,0,""
                                                            setfocus PasswordEdit
                                                            setprop        Passwrd,visible=1
.Test for Credit Password
                                                            unpack  NPASFLD,str1,NPASKEY
                                                            pack    NPASFLD,progcode,NPASKEY
                                                            reset   NPASFLD
                                                     call    NPASKEY
                                              if not over
                                                                    move    YES,HoldFlag
                                                                           alert   note,"Password Accepted!",result
                                                            else
                                                                           move           YES,ReturnFlag2
                                                                           return
                                                     endif
                                                            move           YES,HoldFlag
                                             elseif (str1 = STAR)
.Should never happen!!!
                                                            alert          caution,"Valid List Number Required!",result
                                                            move           YES,ReturnFlag2
                                                            setfocus DataEditSelectList
                                                            return
                                             endif
.END PATCH 1.1.7 ADDED LOGIC
                              endif
               endif
              endif
.             endif
              getitem         DataEditSelectNum,0,NSELNUM
              getitem         DataEditSelectQty,0,str13
              call            Trim using str13
              if (str13 = "")
               pack           NSELQTY,"00000000000"
              else
               call           RemoveChar using str13,COMMA
               move           str13,NSELQTY
               call           ZFillIt using NSELQTY
              endif
.
              getitem         DataEditSelectIndex,0,NSELINDEX
              clear           BaseFlag
              move            C0,BaseFlag2
              getitem         DataComboSelectBase,0,result
              getitem         DataComboSelectBase,result,NSELBASE
              call            Trim using NSELBASE
              if (NSELBASE <> "")
               type           NSELBASE
               if not equal
                              rep            lowup,NSELBASE
                              if (NSELBASE = "BASE")        Base Select - do not allow more than one Base Select per Datacard!
                                             call           SelectTestBase using NSELLIST,NSELNUM,BaseFlag
                                             if (BaseFlag = NO)            .There is another Base Select that the user did not want to overwrite
                                                            clear          NSELBASE
                                             elseif (BaseFlag = YES)       .Base was updated, I need to refresh all Select Records for this Datacard under Save event
                                                            move           "BASE",NSELBASE
                                             endif
                              elseif (NSELBASE = "SECO")
                                             move           "SEC.",NSELBASE
                              endif
               else
.Being extra cautious here.
                              move           C0,N4
                              move           NSELBASE,N4
                              if (N4 > C0)
                                             move           N4,NSELBASE
                                             rep            zfill,NSELBASE
                              else
                                             clear          NSELBASE
                              endif
                              if (NSELBASE <> "")
.                                            if (NewFlag2 <> YES)          .Modified record
                                             if (NSELBASE = NSELNUM)
                                                            alert          note,"Associated Base Number must differ from Select Number!",result
                                                            move           YES,ReturnFlag2
                                                            setfocus DataComboSelectBase
                                                            return
                                             endif
.                                            endif
.Verify Base actually exists!!
                                             getitem        DataEditSelectName,0,taskname
                                             call           SelectTestBase2 using NSELLIST,NSELBASE,str1,taskname
                                             if (str1 <> YES)
.Called routine will attempt to locate a logical choice
                                                            if (taskname <> "")
                                                                           setitem        DataComboSelectBase,0,taskname
                                                                           alert          note,"Valid Base Select Required!  I have supplied a suggestion.",result
                                                            else
                                                                           alert          note,"Valid Base Select Required!",result
                                                            endif
                                                            move           YES,ReturnFlag2
                                                            setfocus DataComboSelectBase
                                                            return
                                             endif
                              endif
               endif
              else
               pack           taskname,"This Select is neither a Base nor has an associated Base",newline,"Is this what you intend?"
               alert          plain,taskname,result
               if (result <> 1)
                              move           YES,ReturnFlag2
                              setfocus DataComboSelectBase
                              return
               endif
              endif
.
              getitem         DataComboSelectExcRent,0,N1
              move            N1,NSELEXC
              if (NSELBASE <> "BASE" & NSELBASE <> "SEC." & NSELBASE <> "")
               call           SelectGetExchange using NSELLIST,NSELBASE,str1
.              if (str1 = "2" | str1 = "3")
                              if (NSELEXC <> str1)
                                             pack           taskname,"All Selects must match the Base/Sec. Base Exchange Status!",newline,"I will change the status for this Select."
                                             alert          note,taskname,result
                                             move           str1,NSELEXC
                                             move           C0,N1
                                             move           str1,N1
                                             setitem        DataComboSelectExcRent,0,N1
                              endif
.              endif
              elseif (NSELBASE = "BASE" | NSELBASE = "SEC.")                              .Set all other Selects off of this Base
               call           SelectSetExchange using NSELLIST,NSELNUM,NSELEXC
              endif
              if (NSELEXC = "2" & (NSELBASE = "BASE" | NSELBASE = "SEC."))
               clear          NSELPRICE
               setitem        DataEditSelectPrice,0,""
               setitem        DataComboSelectModifier,0,1
               move           "   ",NSELDESC
              else
               getitem        DataEditSelectPrice,0,str10
               call           Trim using str10
               call           RemoveChar using str10,COMMA
               move           "+",str1
               call           RemoveChar using str10,str1
               move           C0,NSELPRICE
               move           str10,NSELPRICE
               getitem        DataComboSelectModifier,0,N3
               if (N3 > 1)
                              sub            C1,N3
                              move           N3,NSELDESC
                              rep            zfill,NSELDESC
               else
                              move           "   ",NSELDESC
               endif
              endif
              getitem         DataComboSelectStatus,0,N1
              if (N1 > 1)
               sub            C1,N1
               move           N1,NSELSTATUS
               rep            zfill,NSELSTATUS
              else
               move           B1,NSELSTATUS
              endif
.
              getitem         DataCheckSelectComm,0,N1
              if (N1 = 1)
               move           N1,NSELPCOMM
              else
               move           B1,NSELPCOMM
              endif
              getitem         DataCheckSelectInactive,0,N1
              if (N1 = 1)
               move           N1,NSELINACTIVE
              else
               move           B1,NSELINACTIVE
              endif
              getitem         DataCheckSelectNotes,0,N1
              if (N1 = 1)
               move           N1,NSELNOTES
              else
               move           B1,NSELNOTES
              endif
.START PATCH 1.2.0 ADDED LOGIC
              unpack          timestamp,NSELDATE
.
              move            INITS,NSELINIT
.END PATCH 1.2.0 ADDED LOGIC
              return

DataVerifyRef
.First, make sure List is valid
              getitem         DataEditRefList,0,str6
.Test New records
              move            C0,RefLRFlag
              if (NewFlag3 = YES)
               call           ZFillIt using str6
               getitem        DataEditListNum,0,str7
               if (str6 <> str7)
                              pack           NDATFLD,str6
                              move           "RefVerify-NDATTST",Location
                              pack           KeyLocation,"Key: ",NDATFLD
                              call           NDATTST
                              if over
                                             alert          caution,"Valid List Number Required!",result
                                             move           YES,ReturnFlag3
                                             setfocus DataEditRefList
                                             return
.START PATCH 1.1.7 ADDED LOGIC
                              elseif (HoldFlag <> YES)
                                             call           GetDatacardExclusive using NDATFLD,str1
                                             if (str1 = "C")
                                                            alert          note,"Password Needed to create/modify Exclusive Ref. Prices!",result
                                                            setitem        PasswordEdit,0,""
                                                            setfocus PasswordEdit
                                                            setprop        Passwrd,visible=1
.Test for Credit Password
                                                            unpack  NPASFLD,str1,NPASKEY
                                                            pack    NPASFLD,progcode,NPASKEY
                                                            reset   NPASFLD
                                                     call    NPASKEY
                                              if not over
                                                                    move    YES,HoldFlag
                                                                           alert   note,"Password Accepted!",result
                                                            else
                                                                           move           YES,ReturnFlag3
                                                                           return
                                                     endif
                                                            move           YES,HoldFlag
                                             elseif (str1 = STAR)
.Should never happen!!!
                                                            alert          caution,"Valid List Number Required!",result
                                                            move           YES,ReturnFlag2
                                                            setfocus DataEditSelectList
                                                            return
                                             endif
.END PATCH 1.1.7 ADDED LOGIC
                              endif
                              move           C1,RefLRFlag
               endif
              endif
.
              if (DLV2Flag = 1)
               move           SEQ,result
               move           result,N9
               DataListView2.GetNextItem giving result using C2,N9
               move           result,N9
               DataListView2.GetNextItem giving result using C2,N9
               if (result <> SEQ)
                              return
               endif
               DataListView2.GetNextItem giving N9 using C2
               DataListView2.GetItemText giving str4 using N9,0
              else
               move           SEQ,result
               move           result,N9
               DataListView2B.GetNextItem giving result using C2,N9
               move           result,N9
               DataListView2B.GetNextItem giving result using C2,N9
               if (result <> SEQ)
                              return
               endif
               DataListView2B.GetNextItem giving N9 using C2
               DataListView2B.GetItemText giving str4 using N9,3
              endif
              unpack          str4,NREFCODE,NREFNUM
              if (NREFCODE = "A")            .Addressing
               move           str6,NADDLIST
               move           NREFNUM,NADDNUM
               getitem        DataEditRefPrice,0,str9
               call           Trim using str9
               call           RemoveChar using str9,COMMA
               move           C0,NADDPRICE
               move           str9,NADDPRICE
.              if (NADDPRICE = 0)
.                             alert          caution,"Valid Price Required!",result
.                             move           YES,ReturnFlag3
.                             setfocus DataEditRefPrice
.                             return
.              endif
               getitem        DataComboRefModifier,0,N3
               sub            C1,N3
               move           N3,NADDDESC
               rep            zfill,NADDDESC
               if (NADDPRICE = 0 & (N3 > 0 & N3 < 5))                      .Some Price Modifiers require a valid Price
                              alert          caution,"Valid Price Required with a Modifier!",result
                              move           YES,ReturnFlag3
                              setfocus DataEditRefPrice
                              return
               endif

              elseif (NREFCODE = "L")        .Selection
               move           str6,NSLTLIST
               move           NREFNUM,NSLTNUM
               getitem        DataEditRefQty,0,str11
               call           Trim using str11
               call           RemoveChar using str11,COMMA
.              if (str11 = "")
.                             alert          caution,"Valid Quantity Required!",result
.                             move           YES,ReturnFlag3
.                             setfocus DataEditRefQty
.                             return
.              endif
               move           str11,NSLTQTY
               getitem        DataEditRefPrice,0,str9
               call           Trim using str9
               call           RemoveChar using str9,COMMA
               move           C0,NSLTPRICE
               move           str9,NSLTPRICE
.              if (NSLTPRICE = 0)
.                             alert          caution,"Valid Price Required!",result
.                             move           YES,ReturnFlag3
.                             setfocus DataEditRefPrice
.                             return
.              endif
               getitem        DataComboRefModifier,0,N3
               sub            C1,N3
               move           N3,NSLTDESC
               rep            zfill,NSLTDESC
               if (NSLTPRICE = 0 & (N3 > 0 & N3 < 5))
                              alert          caution,"Valid Price Required with a Modifier!",result
                              move           YES,ReturnFlag3
                              setfocus DataEditRefPrice
                              return
               endif

              elseif (NREFCODE = "R")        .Arrangement
               move           str6,NARRLIST
               move           NREFNUM,NARRNUM
              elseif (NREFCODE = "S")        .Source
               move           str6,NSRCLIST
               move           NREFNUM,NSRCNUM
               getitem        DataEditRefPer,0,str4
               call           Trim using str4
               if (str4 = "")
.                             alert          caution,"Valid Percentage Required!",result
.                             move           YES,ReturnFlag3
.                             setfocus DataEditRefPer
.                             return
                              clear          NSRCPER
               else
                              call           RemoveChar using str4,PRC
                              type           str4
                              if not equal
                                             alert          caution,"Valid Percentage Required!",result
                                             move           YES,ReturnFlag3
                                             setfocus DataEditRefPer
                                             return
                              endif
                              move           str4,NSRCPER
                              call           ZFillIt using NSRCPER,C0
               endif
              elseif (NREFCODE = "T")        .Category
               move           str6,NCATLIST
               unpack         NREFNUM,NCATCODE,NCATNUM
              endif
              return

DataSwitchTab LRoutine FrmPtr
        if (TabNum <> FrmPtr)
                move    TabNum,N2
                call    DataTabClick
                move    FrmPtr,N2
                call    DataTabChange
                setitem DataTabControl001,0,FrmPtr
        endif
        return
DataTabClick
.Force LostFocus event for fields when switching tabs.
.This is done so that fields found on other forms that require data
.established through LostFocus events will be set.
.Switching to another tab does not affect the focus on that
.particular form!  LostFocus events must be triggered!
        if (N2 = C1)
                Deactivate NDAT001A
               setprop        NDAT001D,visible=0
        elseif (N2 = C2)
                Deactivate NDAT001b
        elseif (N2 = C3)
                Deactivate NDAT001c
        elseif (N2 = C4)
        elseif (N2 = C5)
        elseif (N2 = C6)
        elseif (N2 = C7)
        else    N2 = C8
        endif
        return

DataTabChange
        move    N2,TabNum
.
        if (N2 = C1)
                Activate NDAT001A
               setprop        NDAT001D,visible=1
        elseif (N2 = C2)
                Activate NDAT001b
               Data2WebBrowser.Refresh
.Strange work-around - zorder for this object is somehow lost when DEACTIVATE/ACTIVATE is used on Child Form!!!
               getprop        Data2WebBrowser,zorder=result
               setprop        Data2WebBrowser,zorder=result
        elseif (N2 = C3)
                Activate NDAT001c
        elseif (N2 = C4)
        elseif (N2 = C5)
        elseif (N2 = C6)
        elseif (N2 = C7)
        else   .N2 = C8
        endif
        return

OptionsTabClick
              if (N2 = C1)
               setprop        Options1Coll,visible=0
              elseif (N2 = C2)
               setprop Options2Coll,visible=0
              elseif (N2 = C3)
.              setprop Options3Coll,visible=0
              endif
              return

OptionsTabChange
              if (N2 = C1)
               setprop        Options1Coll,visible=1
              elseif (N2 = C2)
               setprop Options2Coll,visible=1
              elseif (N2 = C3)
.              setprop Options3Coll,visible=1
              endif
              return

VScrollChange
              getitem         VScrollBar1,0,N3                                            .Find where the ScrollBar currently sits.
              getprop         NDAT0001,height=result                                      .Find the current Height
.Calculation:  ((MaxHeight - Current Height) / MaxIncrements of ScrollBar * Current ScrollBar Index)
              calc            howmany=((MaxHeight-result)/20*N3)
              setprop         NDAT0001,WINOFFSETV=howmany
.Reposition the Select Screen
              sub             howmany,SelTopC,T1
              setprop         SelectForm,top=T1
.Force a Refresh of objects on Select Screen - otherwise they get funky
              call            Resize_NDAT001D
              return

HScrollChange
              getitem         HScrollBar1,0,N3                                            .Find where the ScrollBar currently sits.
              getprop         NDAT0001,width=result                                       .Find the current Width
.Calculation:  ((MaxWidth - Current Width) / MaxIncrements of ScrollBar * Current ScrollBar Index)
              calc            howmany=((MaxWidth-result)/20*N3)
              setprop         NDAT0001,WINOFFSETH=howmany
.Reposition the Select Screen
              sub             howmany,SelLeftC,L1
              setprop         SelectForm,left=L1
.Force a Refresh of objects on Select Screen - otherwise they get funky
              call            Resize_NDAT001D
              return

DataSetSelectDefault
.Position Select Form in Default Position
              getprop         DataStatNet,top=T1,left=L1
              add             "20",T1                       .height of this object
              setprop SelectForm,winpos=1
              getprop NDAT0001,top=H,left=V
              add     T1,H,SelTop
              add     "50",SelTop          .Compensate for Menu Bar/Title Bar + some to allow second click to make invisible
              add     L1,V,SelLeft
              setprop SelectForm,top=SelTop,left=SelLeft                   .Default
              move            SelTop,SelTopC
              move            SelLeft,SelLeftC
              return

DataMasterMove
              getprop         NDAT0001,top=T1,left=L1
              if (T1 <> TempTop)
               move           C0,N8
               if (TempTop > T1)
                              calc           N8=(TempTop-T1)
                              sub            N8,SelTop
               elseif (T1 > TempTop)
                              calc           N8=(T1-TempTop)
                              add            N8,SelTop
               endif
.             add     "54",SelTop          .Compensate for Menu Bar/Title Bar + some to allow second click to make invisible
               setprop NDAT001D,top=SelTop
              endif
              if (L1 <> TempLeft)
               move           C0,N8
               if (TempLeft > L1)
                              calc           N8=(TempLeft-L1)
                              sub            N8,SelLeft
               elseif (L1 > TempLeft)
                              calc           N8=(L1-TempLeft)
                              add            N8,SelLeft
               endif
               setprop NDAT001D,Left=SelLeft
              endif
              move            T1,TempTop
              move            L1,TempLeft
              return

DataStatMlrName_Click
.START PATCH 1.2.2 REMOVED LOGIC - TEMPORARILY, UNTIL INFO.PLS IS CONVERTED to use company ##
             call            DATADisplayCompany using NDAT0001,DataEditMlrNum,DataEditMlrCnt,N4,MouseForm,T1,L1
.END PATCH 1.2.2 REMOVED LOGIC - TEMPORARILY, UNTIL INFO.PLS IS CONVERTED
              return

DataStatPayName_Click
.             getitem         DataEditMlrNum,0,str6
.REPYN contains nonsensical verbage for this routine, forcing an over on the Offer file read.
.             call            DataDisplayPayTo using NDAT0001,DataEditPayNum,C0,str6,REPYN,N4,MouseForm,T1,L1
              return

DataStatOwnName_Click
              call            DataDisplayOwner using NDAT0001,DataEditOwnNum,N4,MouseForm,T1,L1
              return
.begin patch 1.2.8              
DataStatOwnCnt_Click              
               call            DataDisplayOCNT using NDAT0001,DataEditListNum,N4,MouseForm,T1,L1
          REturn
DataStatPrevOwnCnt_Click
               type Ndatoldown
               return         if not equal   no #
               call            DataDisplayOwner using NDAT0001,DataEditPrevOwn,N4,MouseForm,T1,L1
          REturn
.end patch 1.2.8              
.START PATCH 1.2.7 ADDED LOGIC
getJulDates
          getitem DataEditUpdate,0,str10
          call      RemoveChar,str10,SLASH
          call      TRIM using str10
          call      VerifyDate giving returnValue using DataEditUpdate
          if (ReturnValue = C2) // field was empty
                    move      "00000",UpdateStartJul
          else
                    unpack    str10, mm,dd,CC,yy
                    call      CVTJUL
                    move      JULDAYS, UpdateStartJul
                    rep       zfill, UpdateStartJul
          endif
.
          getitem   NDAT001aEditUpdatedEnd,0,str10
          call      RemoveChar,str10,SLASH
          call      TRIM using str10
          call      VerifyDate giving returnValue using NDAT001aEditUpdatedEnd
          if (ReturnValue = C2) // field was empty
                    move      "99999", UpdateEndJul
          else
                    unpack    str10,mm,dd,CC,yy
                    call      CVTJUL
                    move      JULDAYS,UpdateEndJul
                    rep       zfill,UpdateEndJul
          endif
.
          getitem   DataEditRevised,0,str10
          call      RemoveChar,str10,SLASH
          call      TRIM using str10
          call      VerifyDate giving returnValue using DataEditRevised
          if (ReturnValue = C2) // field was empty
                    move      "00000",RevisionStartJul
          else
                    unpack    str10,mm,dd,cc,yy
                    call      CVTJUL
                    move      JULDAYS,RevisionStartJul
                    rep       zfill,RevisionStartJul
          endif
.
          getitem   NDAT001aEditRevisedEnd,0,str10
          call      RemoveChar,str10,SLASH
          call      TRIM using str10
          call      VerifyDate giving returnValue using NDAT001aEditRevisedEnd
          if (ReturnValue = C2) // field was empty
                    move      "99999",RevisionEndJul
          else
                    unpack    str10,mm,dd,cc,yy
                    call      CVTJUL
                    move      JULDAYS,RevisionEndJul
                    rep       zfill,RevisionEndJul
          endif
          return
.
SearchDataLoadListView
          pack      hold,DATVARS
          unpack    REVDATE,CC,YY,MM,DD
          call      CVTJUL
          move      JULDAYS,str5
          rep       zfill,str5
          if (str5 >= RevisionStartJul && str5 <= RevisionEndJul)  // between revision dates!
                    unpack    ndatupddate,CC,YY,MM,DD
                    call      CVTJUL
                    move      JULDAYS,str5
                    rep       zfill,str5
                    if (str5 >= UpdateStartJul && str5 <= UpdateEndJul)
                              call      TRIM using MLSTNAME
                              DataListViewSearch.InsertItem giving N9 using MLSTNAME
                              DataListViewSearch.SetItemText using N9,LSTNUM,1
                              DataListViewSearch.SetItemText using N9,MLSTNAME,2
                              unpack    REVDATE, cc,yy,mm,dd
                              call      TRIM using MM
                              if (MM <> "")
                                        pack      str10,mm,SLASH,dd,SLASH,cc,yy
                              else
                                        clear     str10
                              endif
                              DataListViewSearch.SetItemText using N9,str10,3
                              call      CVTJUL
                              move      JULDAYS, str5
                              if (STATUS = "W")
                                        pack      str15,"Withdrawn"
                                        call      trim using str15
                              elseif (STATUS = "T")
                                        pack      str15,"Temp. Withdrawn"
                                        call      trim using str15
                              else
                                        clear     str15
                              endif
                              DataListViewSearch.SetItemText using N9,str15,4
                              DataListViewSearch.SetItemText using N9,hold,5
                              unpack    NDATUPDDATE, cc,yy,mm,dd
                              pack      str10,mm,SLASH,dd,SLASH,cc,yy
                              call      CVTJUL
                              DataListViewSearch.SetItemText using N9, str10,6
                              if (ELSTCDE = "C")
                                        pack      str3,"Yes"
                              else
                                        clear     str3
                              endif
                              DataListViewSearch.SetItemText using N9, str3,7
                              if (NLSTCDE = "Y")
                                        pack      str3,"Yes"
                              else
                                        clear     str3
                              endif
                              DataListViewSearch.SetItemText using N9, str3,8
                              DataListViewSearch.SetItemText using N9, str5,9
                              move      JULDAYS, str5
                              DataListViewSearch.SetItemText using N9, str5,10
                              if (STATUS = "T" | STATUS = "W")
                                        move      "0xFF0000",colordim                          .Red
                              else
                                        move      "0x000000",colordim                          .Black
                              endif
                              DataListViewSearch.SetItemText using N9,colordim,11
                    endif
          endif
          return
XRESIZE
           NDat0001.Scale
           RETURN

.END PATCH 1.2.7 ADDED LOGIC
              include         ndatio.inc
              include         nselio.inc
              include         nownio.inc
.START PATCH 1.2.6  REPLACED LOGIC
.              include         nfulio.inc
.START PATCH 1.2.6  REPLACED LOGIC
              include         nsmpio.inc
              include         nxrfio.inc
.START PATCH 1.1.4 REPLACED LOGIC
.             include         nmlrIO.inc
              INCLUDE         COMPIO.inc
              INCLUDE         CNTIO.inc
.END PATCH 1.1.4 REPLACED LOGIC
              include         nrefio.inc
              include         npasio.inc
              include         npayio.inc
              include         nmdlio.inc
              include         ntxtio.inc
              include         naddio.inc
              include         narrio.inc
              include         ncatio.inc
.             include         nmedio.inc
              include         NSLTio.inc
              include         nuseio.inc
              include         nsrcio.inc
              include         nmodio.inc
              include         gnxtio.inc
              include         ncntio.inc
.begin patch 1.6
              include         NQRCIO.inc
.end patch 1.6
.begin patch 1.2.5
              include         Ntypio.inc
.end patch 1.2.5
.begin patch 1.92
          Include   S2Nio.inc
.end patch 1.92
.begin patch 1.5
          Include   M2Nio.inc
.end patch 1.5
.START PATCH 1.1.2 ADDED LOGIC
.For Search Screen
        include searchio.inc      .contains logic for search.plf
.START PATCH 1.1.4 REPLACED LOGIC
.             include         nbrkio.inc
.END PATCH 1.1.4 REPLACED LOGIC
              include         nrtnio.inc
              include         ncmpio.inc
.END PATCH 1.1.2 ADDED LOGIC
.START PATCH 1.1.5 ADDED LOGIC
              include         nusgio.inc
.END PATCH 1.1.5 ADDED LOGIC
.begin patch 1.94
          include   ntxt1io.inc
.end patch 1.94
              include         comlogic.inc