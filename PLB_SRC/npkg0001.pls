          include   COMMON.INC
          include   CONS.INC
          include   npkgdd.inc
          include   nprcdd.inc
          include   nprc2dd.inc
;patch1.8
          include   compdd.inc
          include   cntdd.inc
.         include   nmlrdd.inc
;Patch1.8
          include   gnxtdd.inc
.START PATCH 1.2 ADDED LOGIC
          include   npkgdd2.inc
.END PATCH 1.2 ADDED LOGIC
;PATCH1.7
          INCLUDE   TNCBAKDD.INC
;PATCH1.7

PC      EQU     1

Release   init                "2.0"     13DEC2004 ASH       Fixed bug with Search window for Comp0001.pls and stat0007.pls
.Release  init                "1.9"     14OCT2004 ASH       CONVERTED MAILER PACKAGE TO 6 BYTES
.Release  init                "1.8.2"   01OCT2004 ASH       ADDED PATCH TO FIX CRASHES/FLICKERS WHEN LOADING PACKAGES IN COMPANY PROGRAM (34)
.Release  init                "1.8.1"   17SEP2004 ASH       ADDED PATCH TO FIX CRASHES/FLICKERS WHEN LOADING PACKAGES FROM COMP0001
.                                                                     FLICKER IS SOLVED BY GETTING RID OF MS LISTVIEW 6.0 - NOT YET IMPLEMENTED
.Release  init      "1.8"     DMB       26MAY2004 Mailer Conversion
;release  init      "1.7"     DMB       14AUG2003 ADDED LOGIC TO INCLUDE BACKEND FIELD FOR NEW TNCBACK FILE
;release  init      "1.6"     ASH       14JUL2003 ADDED LOGIC TO PREVENT DUPLICATE PRICES
.release  init      "1.5"     ASH       13DEC2002 ADDED LOGIC TO REQUIRE PACKAGE NAME
.release  init      "1.4"     ASH       18OCT2002 ADDED LOGIC TO DISPLAY ON COMP0001.PLC
.release  init      "1.3"     ASH       18JUN2002 ADDED ENHANCEMENTS
.release  init      "1.2"     ASH       25MAR2002 ADDED LOGIC TO SPEED UP CREATION OF NEW PACKAGES
.                                                           INCREASED FLEXIBILITY OF MASTER PACKAGES AS PER BETA TESTING WITH SK
.release  init      "1.1"     ASH       25FEB2002 ADDED MASTER PACKAGES LOGIC
.                                                           REPLACED LISTVIEW OBJECTS WITH MS LISTVIEW 6.0
.release  init      "1.0"
.EXTERNAL ROUTINES FROM nordtest.PLC
SearchGo3 external "nordtest;SearchGo3"
.START PATCH 2.0 ADDED LOGIC
SearchGo3A external "stat0007;SearchGo3"
.Following is not yet available
.SearchGo3B external "company;SearchGo3"
CallWindow form     1
.END PATCH 2.0 ADDED LOGIC
.START PATCH 1.2 ADDED LOGIC
PackageTestforDupe external "NPKG001A;PackageTestforDupe"
.END PATCH 1.2 ADDED LOGIC
.START PATCH 1.3 ADDED LOGIC
PackagePrintExcel external "NPKG0002;PackagePrintExcel"
RptCan    dim       1
EditTextBoxes       EditText (2)
Buttons             Button    (2)
CheckBoxes          CheckBox (2)
StatTextBoxes       StatText (2)
.END PATCH 1.3 ADDED LOGIC
.
.START PATCH 1.1 ADDED VAR
taskname2 dim       255       .Length of Alert box text maximum
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant
OBOOL     variant
IntIndex integer 4
IntIndex2 integer 4
IntIndex3 integer 4
ColHeads automation
ColHead   automation
ListIts   automation
ListIt    automation
SubIt     automation
font2   font
.END PATCH 1.1 ADDED VAR
ListView1 ListView
WindPtr   Window    ^
DimPtr    Dim       ^
DimPtr1   Dim       ^
DimPtr2   Dim       ^
DimPtr3   Dim       ^
.START PATCH 1.1 ADDED VAR
DimPtr4   Dim       ^
.END PATCH 1.1 ADDED VAR
FrmPtr    Form      ^
FrmPtrA   Form      ^
FrmPtr1   Form      ^
EditPtr EditText ^
ExitFlag4 dim       %%        .Initialized by calling program
NewFlag   init      "N"
NewFlag2 init       "N"
ReturnFlag init "N"
PriceFlag init      "N"
PriceFlag1 init     "N"
QuitFlag init       "N"
.START PATCH 1.1 REMOVED LOGIC
.ViewFlag form "1"  .Default is to sort/view by Package Name
.END PATCH 1.1 REMOVED LOGIC
;PATCH1.7
Backendflag         INIT      "N"
;PATCH1.7
.START PATCH 1.9 REPLACED LOGIC
.holdkey dim        10
holdkey dim         12
.str20    dim       20
str6a     dim       6
.END PATCH 1.9 REPLACED LOGIC
hold      dim       793       .Used for NPKGVARS
hold1     dim       355       .Used for NPRCVARS
hold2     dim       105       .Used for NPRC2VARS
hold3     dim       355       .Used for both NPRCVARS/NPRC2VARS

Temp122 form    12.2
.Variables used for TreeView Object
TVI_ROOT integer    4,"0xFFFF0000"
TVI_1ST   integer             4,"0xFFFF0001"
TVI_LAST integer    4,"0xFFFF0002"
TVI_SORT integer    4,"0xFFFF0003"
.
ROOT      integer   1,"0x00"  TVGN_ROOT first child item of the root item.
NEXT      integer   1,"0x01"  TVGN_NEXT next sibling item.
PREVIOUS integer 1,"0x02"     TVGN_PREVIOUS       previous sibling item.
PARENT    integer   1,"0x03"  TVGN_PARENT         parent of the item
CHILD     integer   1,"0x04"  TVGN_CHILD          first child item.
.0x05     TVGN_FIRSTVISIBLE   first visible item.
.0x06     TVGN_NEXTVISIBLE    next visible item.
.0x07     TVGN_PREVIOUSVISIBLE          first visible item that precedes the given item.
.0x08     TVGN_DROPHILITE     item that is the target of a drag-and-drop operation.
.0x09     TVGN_CARET          currently selected item.
.
MyRoot    integer   4
Level2    integer   4
.
Handle          Integer         4,"0"
Handle2         Integer         4,"0"
.End of variables used for TreeView Object
.START PATCH 1.3 ADDED LOGIC
PackHelpWin         Window
PackEditTextBox     EditText
PackHelpText        dim       1000
.END PATCH 1.3 ADDED LOGIC

.Colors
white   color
grey    color
testcolor color
.START PATCH 1.1 ADDED VAR
RED     color
colornum form       24
.END PATCH 1.1 ADDED VAR

          SHUTDOWN
          
PackageLoadListViews Routine WindPtr,FrmPtr
..EXTERNAL ROUTINES FROM NORDTEST.PLC
.OrderLoadNotes2 external "NORDTEST;OrderLoadNotes2"
.OrderLoadNotes external "NORDTEST;OrderLoadNotes"
.OrderEnableOrder4 external "NORDTEST;OrderEnableOrder4"
.OrderDisableOrder4 external "NORDTEST;OrderDisableOrder4"
.
.START PATCH 2.0 ADDED LOGIC
          move      FrmPtr,CallWindow
.END PATCH 2.0 ADDED LOGIC
        create  white=*white
        create  grey=220:220:220
          create    testcolor=*white
.START PATCH 1.1 ADDED LOGIC
        create  RED=*RED
        create  font2,"Arial",size=8
.END PATCH 1.1 ADDED LOGIC
err     plform  Error
NPKG0001 plform     NPKG0001
NPKGMSK1 plform     NPKGMSK1

.START PATCH 1.3 ADDED LOGIC
rpt2      plform    Report2
.
          create    PackHelpWin=10:160:10:360:
                    title="Package Search Help":
                    wintype=1:
                    winpos=3
          eventreg PackHelpWin,5,PackClosePackHelpWin
          create    PackHelpWin;PackEditTextBox=1:149:1:349,Style=1,Border=1,ReadOnly=1,WordWrap=1
          clear     PackHelpText
          append    "Entering a Mailer Numb. and a Package Num, without the Master Check Box checked, will clear all other Search fields and locate that single Mlr/Package record.",PackHelpText
          append    newline,PackHelpText
          append    newline,PackHelpText
          append    "Entering a Mailer Num., ID, Name will locate records which have matching fields.",PackHelpText
          append    newline,PackHelpText
          append    newline,PackHelpText
          append    "Entering a Mailer Num. and checking the Master Check Box will locate all the Master Packages for that Mailer.",PackHelpText
          append    newline,PackHelpText
          append    newline,PackHelpText
          append    "Entering a Package Num., and, optionally, the Mailer Num., and checking the Master Check Box will locate all records that have the same Master Package Association as the Package Num./Mlr Num.",PackHelpText
          reset     PackHelpText
          setitem   PackEditTextBox,0,PackHelpText
          activate PackEditTextBox
          formload rpt2
.END PATCH 1.3 ADDED LOGIC

sizediff form       "40"
sizediff2 form      "50"
.START PATCH 1.4 ADDED LOGIC
sizediff3 form      "60"
.END PATCH 1.4 ADDED LOGIC

.NPKGMSK2 plform    NPKGMSK2
          formload NPKG0001,WindPtr
          formload NPKGMSK1,WindPtr
          if (FrmPtr = 3)
.Resize objects for NSTAT0007.PLC
                    setprop   PackageTreeView,height=0
..
.                   getprop   NPKG0001,top=result
.                   add       sizediff,result
.                   setprop   NPKG0001,top=result
..
.                   getprop   NPKGMSK1,top=result
.                   add       sizediff,result
.                   setprop   NPKGMSK1,top=result
.
.         elseif (FrmPtr = 4)
.
                    getprop   PackageDelete,top=result
                    add       sizediff,result
                    setprop   PackageDelete,top=result
.
                    getprop   PackageEditSearch,top=result
                    add       sizediff2,result
                    setprop   PackageEditSearch,top=result
.
                    getprop   PackageListView,top=result
                    add       sizediff2,result
                    setprop   PackageListView,top=result
.START PATCH 1.1 REMOVED LOGIC
.                   getprop   PackageListView2,top=result
.                   add       sizediff2,result
.                   setprop   PackageListView2,top=result
..
.                   getprop   PackageListView3,top=result
.                   add       sizediff2,result
.                   setprop   PackageListView3,top=result
.END PATCH 1.1 REMOVED LOGIC
                    getprop   PackageModify,top=result
                    add       sizediff,result
                    setprop   PackageModify,top=result
.
                    getprop   PackageNew,top=result
                    add       sizediff2,result
                    setprop   PackageNew,top=result
.
.START PATCH 1.3 ADDED LOGIC
                    getprop   PackageHelp,top=result
                    add       sizediff2,result
                    setprop   PackageHelp,top=result
.
                    getprop   PackagePrint,top=result
                    add       sizediff2,result
                    setprop   PackagePrint,top=result
.END PATCH 1.3 ADDED LOGIC
.
                    getprop   PackageOK,top=result
                    add       sizediff2,result
                    setprop   PackageOK,top=result
.
                    getprop   PackageQuit,top=result
                    add       sizediff,result
                    setprop   PackageQuit,top=result
.
.                   getprop   PackageRetrieve,top=result
.                   add       sizediff2,result
.                   setprop   PackageRetrieve,top=result
.
                    getprop   PackageSave,top=result
                    add       sizediff,result
                    setprop   PackageSave,top=result
.
.                   getprop   PackageSearch,top=result
.                   add       sizediff2,result
.                   setprop   PackageSearch,top=result
.Special placing for following StatText Box
                    add       "20",result
                    setprop   PackageStatRecords,top=result
                    getprop   PackageStatRecords,width=howmany
                    add       C5,howmany
                    getprop   PackageListView,left=result
                    sub       howmany,result
                    setprop   PackageStatRecords,left=result
.
                    getprop   PackageStatSearch,top=result
                    add       sizediff2,result
                    setprop   PackageStatSearch,top=result
.START PATCH 1.1 ADDED LOGIC
                    getprop   PackageCheckSearchMaster,top=result
                    add       sizediff2,result
                    setprop   PackageCheckSearchMaster,top=result
.
                    getprop   PackageEditSearchPackage,top=result
                    add       sizediff2,result
                    setprop   PackageEditSearchPackage,top=result
.
                    getprop   PackageStatSearchPackage,top=result
                    add       sizediff2,result
                    setprop   PackageStatSearchPackage,top=result
.
                    getprop   PackageEditSearchName,top=result
                    add       sizediff2,result
                    setprop   PackageEditSearchName,top=result
.
                    getprop   PackageStatSearchName,top=result
                    add       sizediff2,result
                    setprop   PackageStatSearchName,top=result
.
                    getprop   PackageEditSearchID,top=result
                    add       sizediff2,result
                    setprop   PackageEditSearchID,top=result
.
                    getprop   PackageStatSearchID,top=result
                    add       sizediff2,result
                    setprop   PackageStatSearchID,top=result
.END PATCH 1.1 ADDED LOGIC
.
.
.
                    getprop   PackageCalcTotal,top=result
                    add       sizediff,result
                    setprop   PackageCalcTotal,top=result
.
                    getprop   PackageEditDate,top=result
                    add       sizediff,result
                    setprop   PackageEditDate,top=result
.
;patch1.7
.
                    getprop   PackageEditBackendCost,top=result
                    add       sizediff,result
                    setprop   PackageEditBackendCost,top=result
.
;patch1.7
                    getprop   PackageEditID,top=result
                    add       sizediff,result
                    setprop   PackageEditID,top=result
.
                    getprop   PackageEditMlr,top=result
                    add       sizediff,result
                    setprop   PackageEditMlr,top=result
.
                    getprop   PackageEditName,top=result
                    add       sizediff,result
                    setprop   PackageEditName,top=result
.
                    getprop   PackageEditNotes,top=result
                    add       sizediff,result
                    setprop   PackageEditNotes,top=result
.
                    getprop   PackageEditNumber,top=result
                    add       sizediff,result
                    setprop   PackageEditNumber,top=result
.
                    getprop   PackageEditOtherDate,top=result
                    add       sizediff,result
                    setprop   PackageEditOtherDate,top=result
.
                    getprop   PackageEditOtherName,top=result
                    add       sizediff,result
                    setprop   PackageEditOtherName,top=result
.
                    getprop   PackageEditOtherNum,top=result
                    add       sizediff,result
                    setprop   PackageEditOtherNum,top=result
.
                    getprop   PackageEditOtherPrice,top=result
                    add       sizediff,result
                    setprop   PackageEditOtherPrice,top=result
.
                    getprop   PackageEditPostage,top=result
                    add       sizediff,result
                    setprop   PackageEditPostage,top=result
.
                    getprop   PackageEditPremium,top=result
                    add       sizediff,result
                    setprop   PackageEditPremium,top=result
.
                    getprop   PackageEditPriceDate,top=result
                    add       sizediff,result
                    setprop   PackageEditPriceDate,top=result
.
                    getprop   PackageEditPriceNotes,top=result
                    add       sizediff,result
                    setprop   PackageEditPriceNotes,top=result
.
                    getprop   PackageEditPrinting,top=result
                    add       sizediff,result
                    setprop   PackageEditPrinting,top=result
.
                    getprop   PackageEditTotal,top=result
                    add       sizediff,result
                    setprop   PackageEditTotal,top=result
.
                    getprop   PackageGroupBox001,top=result
                    add       sizediff,result
                    setprop   PackageGroupBox001,top=result
.
                    getprop   PackageGroupBox002,top=result
                    add       sizediff,result
                    setprop   PackageGroupBox002,top=result
.
                    getprop   PackageListViewOther,top=result
                    add       sizediff,result
                    setprop   PackageListViewOther,top=result
.
                    getprop   PackageListViewPrice,top=result
                    add       sizediff,result
                    setprop   PackageListViewPrice,top=result
.
                    getprop   PackageOtherAdd,top=result
                    add       sizediff,result
                    setprop   PackageOtherAdd,top=result
.
                    getprop   PackageOtherRemove,top=result
                    add       sizediff,result
                    setprop   PackageOtherRemove,top=result
.
                    getprop   PackagePriceAdd,top=result
                    add       sizediff,result
                    setprop   PackagePriceAdd,top=result
.
                    getprop   PackagePriceRemove,top=result
                    add       sizediff,result
                    setprop   PackagePriceRemove,top=result
.
;patch1.7
.
                    getprop   PackageStatBackendCost,top=result
                    add       sizediff,result
                    setprop   PackageStatBackendCost,top=result
.
;patch1.7
                    getprop   PackageStatDate,top=result
                    add       sizediff,result
                    setprop   PackageStatDate,top=result
.
                    getprop   PackageStatID,top=result
                    add       sizediff,result
                    setprop   PackageStatID,top=result
.
                    getprop   PackageStatMailer,top=result
                    add       sizediff,result
                    setprop   PackageStatMailer,top=result
.
                    getprop   PackageStatMlrName,top=result
                    add       sizediff,result
                    setprop   PackageStatMlrName,top=result
.
                    getprop   PackageStatName,top=result
                    add       sizediff,result
                    setprop   PackageStatName,top=result
.
                    getprop   PackageStatNotes,top=result
                    add       sizediff,result
                    setprop   PackageStatNotes,top=result
.
                    getprop   PackageStatNumber,top=result
                    add       sizediff,result
                    setprop   PackageStatNumber,top=result
.
                    getprop   PackageStatOther,top=result
                    add       sizediff,result
                    setprop   PackageStatOther,top=result
.
                    getprop   PackageStatOtherDate,top=result
                    add       sizediff,result
                    setprop   PackageStatOtherDate,top=result
.
                    getprop   PackageStatOtherName,top=result
                    add       sizediff,result
                    setprop   PackageStatOtherName,top=result
.
                    getprop   PackageStatOtherPrice,top=result
                    add       sizediff,result
                    setprop   PackageStatOtherPrice,top=result
.
                    getprop   PackageStatPostage,top=result
                    add       sizediff,result
                    setprop   PackageStatPostage,top=result
.
                    getprop   PackageStatPremium,top=result
                    add       sizediff,result
                    setprop   PackageStatPremium,top=result
.
                    getprop   PackageStatPriceDate,top=result
                    add       sizediff,result
                    setprop   PackageStatPriceDate,top=result
.
                    getprop   PackageStatPriceNotes,top=result
                    add       sizediff,result
                    setprop   PackageStatPriceNotes,top=result
.
                    getprop   PackageStatPrinting,top=result
                    add       sizediff,result
                    setprop   PackageStatPrinting,top=result
.
                    getprop   PackageStatTotal,top=result
                    add       sizediff,result
                    setprop   PackageStatTotal,top=result
.
.START PATCH 1.1 ADDED LOGIC
                    getprop   PackageCheckMaster,top=result
                    add       sizediff,result
                    setprop   PackageCheckMaster,top=result
.
                    getprop   PackageEditMaster,top=result
                    add       sizediff,result
                    setprop   PackageEditMaster,top=result
.
                    getprop   PackageStatMaster,top=result
                    add       sizediff,result
                    setprop   PackageStatMaster,top=result
.END PATCH 1.1 ADDED LOGIC
          elseif (FrmPtr = 2)
.Resize objects for NMLR0001.PLC
....Hide several objects
                    setprop   PackageStatSearch,height=0,width=0
.START PATCH 1.3 ADDED LOGIC
                    setprop   PackageHelp,height=0,width=0
                    setprop   PackagePrint,top=88,width=50,left=260
.END PATCH 1.3 ADDED LOGIC
.START PATCH 1.1 REPLACED LOGIC
.                   setprop   PackageEditSearch,height=0,width=0
.                   setprop   PackageOK,height=0,width=0
                    setprop   PackageEditSearch,height=0,width=0,tabid=0
                    setprop   PackageOK,height=0,width=0,tabid=0
.END PATCH 1.1 REPLACED LOGIC
                    setprop   PackageStatMlrName,height=0,width=0
.START PATCH 1.1 ADDED LOGIC
                    setprop   PackageCheckSearchMaster,height=0,width=0,tabid=0
                    setprop   PackageEditSearchPackage,height=0,width=0,tabid=0
                    setprop   PackageStatSearchPackage,height=0,width=0
                    setprop   PackageEditSearchName,height=0,width=0,tabid=0
                    setprop   PackageStatSearchName,height=0,width=0
                    setprop   PackageEditSearchID,height=0,width=0,tabid=0
                    setprop   PackageStatSearchID,height=0,width=0
.END PATCH 1.1 ADDED LOGIC
....Change Others.............
                    setprop   PackageNew,top=85,height=23,width=50,left=10
                    setprop   PackageModify,top=85,height=23,width=50,left=60
                    setprop   PackageQuit,top=85,height=23,width=50,left=110
                    setprop   PackageSave,top=85,height=23,width=50,left=160
.                   setprop   PackageSearch,top=85,height=23,width=50,left=210
.                   setprop   PackageRetrieve,top=85,height=23,width=50,left=260
                    setprop   PackageDelete,top=108,height=21,width=50,left=260
                    setprop   PackageListView,top=85,height=45,width=320,left=310
.START PATCH 1.1 REMOVED LOGIC
.                   setprop   PackageListView2,top=85,height=45,width=320,left=310
.                   setprop   PackageListView3,top=85,height=45,width=320,left=310
.END PATCH 1.1 REMOVED LOGIC
                    setprop   PackageTreeView,top=110,height=0,width=190,left=70
                    setprop   PackageStatRecords,top=110,left=10
.START PATCH 1.4 ADDED LOGIC
          elseif (FrmPtr = 5)
.Resize objects for COMP0001.PLC
....Hide several objects
                    setprop   PackageStatSearch,height=0,width=0
                    setprop   PackageHelp,height=0,width=0
                    setprop   PackageEditSearch,height=0,width=0,tabid=0
.START PATCH 1.8.2 REPLACED LOGIC
.                   setprop   PackageOK,height=0,width=0,tabid=0
                    setprop   PackageOK,top=128,height=20,width=50,left=10
.END PATCH 1.8.2 REPLACED LOGIC
                    setprop   PackageStatMlrName,height=0,width=0
                    setprop   PackageCheckSearchMaster,height=0,width=0,tabid=0
                    setprop   PackageEditSearchPackage,height=0,width=0,tabid=0
                    setprop   PackageStatSearchPackage,height=0,width=0
                    setprop   PackageEditSearchName,height=0,width=0,tabid=0
                    setprop   PackageStatSearchName,height=0,width=0
                    setprop   PackageEditSearchID,height=0,width=0,tabid=0
                    setprop   PackageStatSearchID,height=0,width=0
....Change Others.............
.                   setprop   PackagePrint,top=108,width=50,left=260
                    setprop   PackagePrint,top=108,width=50,left=210
                    setprop   PackageNew,top=108,height=20,width=50,left=10
                    setprop   PackageModify,top=108,height=20,width=50,left=60
                    setprop   PackageQuit,top=108,height=20,width=50,left=110
                    setprop   PackageSave,top=108,height=20,width=50,left=160
.                   setprop   PackageDelete,top=128,height=20,width=50,left=260
.                   setprop   PackageListView,top=108,height=80,width=320,left=310
                    setprop   PackageDelete,top=128,height=20,width=50,left=210
                    setprop   PackageListView,top=108,height=80,width=370,left=260
.START PATCH 1.8.2 REPLACED LOGIC
.                   setprop   PackageStatRecords,top=130,left=10
                    setprop   PackageStatRecords,top=150,left=10
.END PATCH 1.8.2 REPLACED LOGIC
.
                    setprop   PackageTreeView,height=0
.
                    getprop   PackageCalcTotal,top=result
                    add       sizediff3,result
                    setprop   PackageCalcTotal,top=result
.
;patch1.7
.
                    getprop   PackageEditBackendCost,top=result
                    add       sizediff3,result
                    setprop   PackageEditBackendCost,top=result
.
;patch1.7

                    getprop   PackageEditDate,top=result
                    add       sizediff3,result
                    setprop   PackageEditDate,top=result
.
                    getprop   PackageEditID,top=result
                    add       sizediff3,result
                    setprop   PackageEditID,top=result
.
                    getprop   PackageEditMlr,top=result
                    add       sizediff3,result
                    setprop   PackageEditMlr,top=result
.
                    getprop   PackageEditName,top=result
                    add       sizediff3,result
                    setprop   PackageEditName,top=result
.
                    getprop   PackageEditNotes,top=result
                    add       sizediff3,result
                    setprop   PackageEditNotes,top=result
.
                    getprop   PackageEditNumber,top=result
                    add       sizediff3,result
                    setprop   PackageEditNumber,top=result
.
                    getprop   PackageEditOtherDate,top=result
                    add       sizediff3,result
                    setprop   PackageEditOtherDate,top=result
.
                    getprop   PackageEditOtherName,top=result
                    add       sizediff3,result
                    setprop   PackageEditOtherName,top=result
.
                    getprop   PackageEditOtherNum,top=result
                    add       sizediff3,result
                    setprop   PackageEditOtherNum,top=result
.
                    getprop   PackageEditOtherPrice,top=result
                    add       sizediff3,result
                    setprop   PackageEditOtherPrice,top=result
.
                    getprop   PackageEditPostage,top=result
                    add       sizediff3,result
                    setprop   PackageEditPostage,top=result
.
                    getprop   PackageEditPremium,top=result
                    add       sizediff3,result
                    setprop   PackageEditPremium,top=result
.

                    getprop   PackageEditPriceDate,top=result
                    add       sizediff3,result
                    setprop   PackageEditPriceDate,top=result
.
                    getprop   PackageEditPriceNotes,top=result
                    add       sizediff3,result
                    setprop   PackageEditPriceNotes,top=result
.
                    getprop   PackageEditPrinting,top=result
                    add       sizediff3,result
                    setprop   PackageEditPrinting,top=result
.
                    getprop   PackageEditTotal,top=result
                    add       sizediff3,result
                    setprop   PackageEditTotal,top=result
.
                    getprop   PackageGroupBox001,top=result
                    add       sizediff3,result
                    setprop   PackageGroupBox001,top=result
.
                    getprop   PackageGroupBox002,top=result
                    add       sizediff3,result
                    setprop   PackageGroupBox002,top=result
.
                    getprop   PackageListViewOther,top=result
                    add       sizediff3,result
                    setprop   PackageListViewOther,top=result
.
                    getprop   PackageListViewPrice,top=result
                    add       sizediff3,result
                    setprop   PackageListViewPrice,top=result
.
                    getprop   PackageOtherAdd,top=result
                    add       sizediff3,result
                    setprop   PackageOtherAdd,top=result
.
                    getprop   PackageOtherRemove,top=result
                    add       sizediff3,result
                    setprop   PackageOtherRemove,top=result
.
                    getprop   PackagePriceAdd,top=result
                    add       sizediff3,result
                    setprop   PackagePriceAdd,top=result
.
                    getprop   PackagePriceRemove,top=result
                    add       sizediff3,result
                    setprop   PackagePriceRemove,top=result
.
;patch1.7
.
                    getprop   PackageStatBackendCost,top=result
                    add       sizediff3,result
                    setprop   PackageStatBackendCost,top=result
.
;patch1.7
                    getprop   PackageStatDate,top=result
                    add       sizediff3,result
                    setprop   PackageStatDate,top=result
.
                    getprop   PackageStatID,top=result
                    add       sizediff3,result
                    setprop   PackageStatID,top=result
.
                    getprop   PackageStatMailer,top=result
                    add       sizediff3,result
                    setprop   PackageStatMailer,top=result
.
                    getprop   PackageStatMlrName,top=result
                    add       sizediff3,result
                    setprop   PackageStatMlrName,top=result
.
                    getprop   PackageStatName,top=result
                    add       sizediff3,result
                    setprop   PackageStatName,top=result
.
                    getprop   PackageStatNotes,top=result
                    add       sizediff3,result
                    setprop   PackageStatNotes,top=result
.
                    getprop   PackageStatNumber,top=result
                    add       sizediff3,result
                    setprop   PackageStatNumber,top=result
.
                    getprop   PackageStatOther,top=result
                    add       sizediff3,result
                    setprop   PackageStatOther,top=result
.
                    getprop   PackageStatOtherDate,top=result
                    add       sizediff3,result
                    setprop   PackageStatOtherDate,top=result
.
                    getprop   PackageStatOtherName,top=result
                    add       sizediff3,result
                    setprop   PackageStatOtherName,top=result
.
                    getprop   PackageStatOtherPrice,top=result
                    add       sizediff3,result
                    setprop   PackageStatOtherPrice,top=result
.
                    getprop   PackageStatPostage,top=result
                    add       sizediff3,result
                    setprop   PackageStatPostage,top=result
.
                    getprop   PackageStatPremium,top=result
                    add       sizediff3,result
                    setprop   PackageStatPremium,top=result
.
                    getprop   PackageStatPriceDate,top=result
                    add       sizediff3,result
                    setprop   PackageStatPriceDate,top=result
.
                    getprop   PackageStatPriceNotes,top=result
                    add       sizediff3,result
                    setprop   PackageStatPriceNotes,top=result
.
                    getprop   PackageStatPrinting,top=result
                    add       sizediff3,result
                    setprop   PackageStatPrinting,top=result
.
                    getprop   PackageStatTotal,top=result
                    add       sizediff3,result
                    setprop   PackageStatTotal,top=result
.
                    getprop   PackageCheckMaster,top=result
                    add       sizediff3,result
                    setprop   PackageCheckMaster,top=result
.
                    getprop   PackageEditMaster,top=result
                    add       sizediff3,result
                    setprop   PackageEditMaster,top=result
.
                    getprop   PackageStatMaster,top=result
                    add       sizediff3,result
                    setprop   PackageStatMaster,top=result
.END PATCH 1.4 ADDED LOGIC
          else
                    setprop   PackageTreeView,height=0
          endif
          formload err
.
.START PATCH 1.1 REPLACED LOGIC
..        PackageListView.InsertColumn using "##",40,1
.         if (FrmPtr = 2)
.                   PackageListView.InsertColumn using "Name",125,1
.         else
.                   PackageListView.InsertColumn using "Name",165,1
.         endif
.        PackageListView.InsertColumn using "ID",70,2
.        PackageListView.InsertColumn using "Mailer",45,3
.        PackageListView.InsertColumn using "Date",60,4
.        PackageListView.InsertColumn using "Other Detail",0,5
..
.        PackageListView2.InsertColumn using "ID",70,1
.         if (FrmPtr = 2)
.                   PackageListView2.InsertColumn using "Name",125,2
.         else
.                   PackageListView2.InsertColumn using "Name",165,2
.         endif
.        PackageListView2.InsertColumn using "Mailer",45,3
.        PackageListView2.InsertColumn using "Date",60,4
.        PackageListView2.InsertColumn using "Other Detail",0,5
.
.        PackageListView3.InsertColumn using "",0,1
.        PackageListView3.InsertColumn using "Date",60,2
.         if (FrmPtr = 2)
.                   PackageListView3.InsertColumn using "Name",125,3
.         else
.                   PackageListView3.InsertColumn using "Name",165,3
.         endif
.        PackageListView3.InsertColumn using "ID",70,4
.        PackageListView3.InsertColumn using "Mailer",45,5
.        PackageListView3.InsertColumn using "Other Detail",0,6
...............................
.Create Column Headers for PackageListView
        getprop PackageListView,*ColumnHeaders=ColHeads
.I hide the first item as I have not yet figured out if I can change the ForeColor of that item, since it does not appear to be a sub-item.
          ColHeads.Add using *Index=1,*Key="key",*Text="",*Width=0
          if (FrmPtr = 2)
                    ColHeads.Add using *Index=2,*Key="one",*Text="Name",*Width=125
          else
                    ColHeads.Add using *Index=2,*Key="one",*Text="Name",*Width=165
          endif
          ColHeads.Add using *Index=3,*Key="two",*Text="ID",*Width=70
          ColHeads.Add using *Index=4,*Key="three",*Text="Mailer",*Width=45
          ColHeads.Add using *Index=5,*Key="four",*Text="Date",*Width=60
          ColHeads.Add using *Index=6,*Key="five",*Text="Julian Date",*Width=0
          ColHeads.Add using *Index=7,*Key="six",*Text="Other Detail",*Width=0
.Set some properties for ListView object
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  OBOOL,VarType=VT_BOOL,VarValue=0
.
          setprop   PackageListView,*HideColumnHeaders=OFALSE
          setprop   PackageListView,*HideSelection=OFALSE
.         setprop   PackageListView,*HotTracking=OTRUE
          setprop   PackageListView,*FullRowSelect=OTRUE
.         setprop   PackageListView,*MultiSelect=OTRUE
          setprop   PackageListView,*Sorted=OTRUE
          setprop   PackageListView,*SortOrder=0
.         setprop   PackageListView,*AllowColumnReorder=OTRUE
          setprop   PackageListView,*LabelEdit=1
          setprop   PackageListView,*View=3
          setprop   PackageListView,*Font=font2
.
        getprop PackageListView,*ListItems=ListIts
.END PATCH 1.1 REPLACED LOGIC
.
          PackageListViewPrice.InsertColumn using "",0,1
          PackageListViewPrice.InsertColumn using "Date",65,2
        PackageListViewPrice.InsertColumn using "Total",65,3
        PackageListViewPrice.InsertColumn using "Print",65,4
        PackageListViewPrice.InsertColumn using "Postage",65,5
        PackageListViewPrice.InsertColumn using "Premium",65,6
          PackageListViewPrice.InsertColumn using "Other Detail",0,7
        PackageListViewPrice.SetColumnFormat using 2,1
          PackageListViewPrice.SetColumnFormat using 3,1
          PackageListViewPrice.SetColumnFormat using 4,1
          PackageListViewPrice.SetColumnFormat using 5,1
.
.        PackageListViewOther.InsertColumn using "##",60,1
.        PackageListViewOther.InsertColumn using "Name",160,2
.        PackageListViewOther.InsertColumn using "Price",65,3
.        PackageListViewOther.InsertColumn using "Other Detail",0,4
.        PackageListViewOther.SetColumnFormat using 2,1
        PackageListViewOther.InsertColumn using "Name",220,1
        PackageListViewOther.InsertColumn using "Price",65,2
        PackageListViewOther.InsertColumn using "Other Detail",0,3
        PackageListViewOther.SetColumnFormat using 2,1
.
          call      PackageDisableLower
          return

.START PATCH 1.3 ADDED LOGIC
PackageSetPrint LRoutine DimPtr
          destroy   StatTextBoxes(1)
          destroy   EditTextBoxes(1)
          destroy   CheckBoxes(1)
          destroy   Buttons(1)
          setprop   Report2,title="Package Printing"
          move      NO,RptCan
          create    Report2;StatTextBoxes(1)=50:70:10:310,"Group",""
.START PATCH 1.9 REPLACED LOGIC
.         create    Report2;EditTextBoxes(1)=50:70:80:120,MaxChars=4,EditType=2,SelectAll=1,Style=1,Border=1
          create    Report2;EditTextBoxes(1)=50:70:80:120,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
.END PATCH 1.9 REPLACED LOGIC
          create    Report2;CheckBoxes(1)=70:90:10:310,"Master Packages Only"
          create    Report2;Buttons(1)=205:230:50:100,"O&K",zorder=500,default=1
          activate StatTextBoxes(1)
          activate EditTextBoxes(1)
          activate CheckBoxes(1)
          activate Buttons(1),PackageSetPrintOK,result
          setfocus EditTextBoxes(1)
          setitem   EditTextBoxes(1),0,DimPtr
          setprop   Report2,visible=1
          return
PackageSetPrintOK
.START PATCH 1.9 REPLACED LOGIC
.         getitem   EditTextBoxes(1),0,str4
.         call      Trim using str4
.         if (str4 <> "")
.                   call      ZFillIt using str4,C0
.                   setitem   EditTextBoxes(1),0,str4
.                   pack      MKEY,str4,"000"
.                   move      "Print-NMLRTST",Location
.                   pack      KeyLocation,"Key: ",MKEY
.                   call      NMLRTST
.                   if over
.                             alert     caution,"Valid Mailer Required!",result
.                             setfocus EditTextBoxes(1)
.                             return
.                   endif
.         else
.                   alert     caution,"Valid Mailer Required!",result
.                   setfocus EditTextBoxes(1)
.                   return
.         endif
...............................
          getitem   EditTextBoxes(1),0,str6
          call      Trim using str6
          if (str6 <> "")
                    call      ZFillIt using str6,C0
                    setitem   EditTextBoxes(1),0,str6
                    move      str6,COMPFLD
                    rep       zfill,COMPFLD
                    move      "Print-COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
                    if over
                              alert     caution,"Valid Mailer Required!",result
                              setfocus EditTextBoxes(1)
                              return
                    elseif (COMPMLRFLG <> "T")
                              alert     caution,"Valid Mailer Required!",result
                              setfocus EditTextBoxes(1)
                              return
                    endif
          else
                    alert     caution,"Valid Mailer Required!",result
                    setfocus EditTextBoxes(1)
                    return
          endif
.END PATCH 1.9 REPLACED LOGIC
.
          getitem   CheckBoxes(1),0,howmany
          setprop   Report2,visible=0
          return
.END PATCH 1.3 ADDED LOGIC

PackageDisableLower
;PATCH1.7
          setprop   PackageEditBackendCost,bgcolor=grey,enabled=0
;PATCH1.7

          setprop   PackageEditDate,bgcolor=grey,enabled=0
          setprop   PackageEditID,bgcolor=grey,enabled=0
          setprop   PackageEditMlr,bgcolor=grey,enabled=0
          setprop   PackageEditName,bgcolor=grey,enabled=0
          setprop   PackageEditNotes,bgcolor=grey,enabled=0
          setprop   PackageEditNumber,bgcolor=grey,enabled=0
          setprop   PackageEditOtherDate,bgcolor=grey,enabled=0
          setprop   PackageEditOtherName,bgcolor=grey,enabled=0
          setprop   PackageEditOtherNum,bgcolor=grey,enabled=0
          setprop   PackageEditOtherPrice,bgcolor=grey,enabled=0
          setprop   PackageEditPostage,bgcolor=grey,enabled=0
          setprop   PackageEditPremium,bgcolor=grey,enabled=0
          setprop   PackageEditPriceDate,bgcolor=grey,enabled=0
          setprop   PackageEditPriceNotes,bgcolor=grey,enabled=0
          setprop   PackageEditPrinting,bgcolor=grey,enabled=0
          setprop   PackageEditTotal,bgcolor=grey,enabled=0
          setprop   PackageCalcTotal,height=0
.
          setprop   PackageListView,*Enabled=1
.START PATCH 1.1 REPLACED LOGIC
.         setprop   PackageListView2,bgcolor=white,enabled=1
.         setprop   PackageListView3,bgcolor=white,enabled=1
.END PATCH 1.1 REPLACED LOGIC
          setprop   PackageListViewOther,bgcolor=white,enabled=1
          setprop   PackageListViewPrice,bgcolor=white,enabled=1
.START PATCH 1.1 ADDED LOGIC
          setprop   PackageCheckMaster,enabled=0
          setprop   PackageEditMaster,bgcolor=grey,enabled=0
          move      NO,NewFlag
          move      NO,NewFlag2
.END PATCH 1.1 ADDED LOGIC
          return

PackageEnableLower
;PATCH1.7
          setprop   PackageEditBackendCost,bgcolor=white,enabled=1
;PATCH1.7

          setprop   PackageEditDate,bgcolor=white,enabled=1
          setprop   PackageEditID,bgcolor=white,enabled=1
.         setprop   PackageEditMlr,bgcolor=white,enabled=1
          setprop   PackageEditName,bgcolor=white,enabled=1
          setprop   PackageEditNotes,bgcolor=white,enabled=1
.         setprop   PackageEditNumber,bgcolor=white,enabled=1
          setprop   PackageEditOtherDate,bgcolor=white,enabled=1
          setprop   PackageEditOtherName,bgcolor=white,enabled=1
.         setprop   PackageEditOtherNum,bgcolor=white,enabled=1
          setprop   PackageEditOtherPrice,bgcolor=white,enabled=1
          setprop   PackageEditPostage,bgcolor=white,enabled=1
          setprop   PackageEditPremium,bgcolor=white,enabled=1
          setprop   PackageEditPriceDate,bgcolor=white,enabled=1
          setprop   PackageEditPriceNotes,bgcolor=white,enabled=1
          setprop   PackageEditPrinting,bgcolor=white,enabled=1
          setprop   PackageEditTotal,bgcolor=white,enabled=1
          setprop   PackageCalcTotal,height=20
.
          setprop   PackageListView,*Enabled=0
.START PATCH 1.1 REPLACED LOGIC
.         setprop   PackageListView2,bgcolor=grey,enabled=0
.         setprop   PackageListView3,bgcolor=grey,enabled=0
.END PATCH 1.1 REPLACED LOGIC
          setprop   PackageListViewOther,bgcolor=grey
          setprop   PackageListViewPrice,bgcolor=grey
          move      "N",PriceFlag
          move      "N",PriceFlag1
.START PATCH 1.1 ADDED LOGIC
          setprop   PackageCheckMaster,enabled=1
          setprop   PackageEditMaster,bgcolor=white,enabled=1
.END PATCH 1.1 ADDED LOGIC
          return

PackageEnableButtonsUpper1A
          setprop   PackageOK,enabled=1
PackageEnableButtonsUpper1
          setprop   PackageNew,enabled=1
.         setprop   PackageModify,enabled=1
.         setprop   PackageSearch,enabled=1
          move    "Y",ExitFlag4
          return

PackageEnableButtons2
          setprop   PackageSave,enabled=1
          setprop   PackageQuit,enabled=1
          setprop   PackagePriceAdd,enabled=1
          setprop   PackagePriceRemove,enabled=1
          setprop   PackageOtherAdd,enabled=1
          setprop   PackageOtherRemove,enabled=1
          return

PackageDisableButtonsUpper1A
          setprop   PackageOK,enabled=0
PackageDisableButtonsUpper1
          setprop   PackageNew,enabled=0
          setprop   PackageModify,enabled=0
.         setprop   PackageSearch,enabled=0
.         setprop   PackageRetrieve,enabled=0
          move    "N",ExitFlag4
          return

PackageDisableButtons2
          setprop   PackageSave,enabled=0
          setprop   PackageQuit,enabled=0
          setprop   PackageDelete,enabled=0
          setprop   PackagePriceAdd,enabled=0
          setprop   PackagePriceRemove,enabled=0
          setprop   PackageOtherAdd,enabled=0
          setprop   PackageOtherRemove,enabled=0
          return

PackageClearScreen
          setitem   PackageEditDate,0,""
          setitem   PackageEditID,0,""
          setitem   PackageEditMlr,0,""
          setitem   PackageEditName,0,""
          setitem   PackageEditNotes,0,""
          setitem   PackageEditNumber,0,""
          setitem   PackageStatMlrName,0,""
          setitem   PackageStatRecords,0,""
.START PATCH 1.1 ADDED LOGIC
          setitem   PackageEditMaster,0,""
          setitem   PackageCheckMaster,0,0
.END PATCH 1.1 ADDED LOGIC
PackageClearScreen1
;PATCH1.7
          setitem   PackageEditBackendCost,0,""
;PATCH1.7

          setitem   PackageEditPostage,0,""
          setitem   PackageEditPremium,0,""
          setitem   PackageEditPriceDate,0,""
          setitem   PackageEditPriceNotes,0,""
          setitem   PackageEditPrinting,0,""
          setitem   PackageEditTotal,0,""
PackageClearScreen2
          setitem   PackageEditOtherDate,0,""
          setitem   PackageEditOtherName,0,""
          setitem   PackageEditOtherNum,0,""
          setitem   PackageEditOtherPrice,0,""
          return

PackageLoadAimRecords Routine DimPtr,DimPtr1,DimPtr2,DimPtr3,DimPtr4,FrmPtr
          move      STAR,QuitFlag
          setprop   PackageQuit,enabled=1
          move    C1,NPKGPATH
          move      "P.LoadRec.-NPKGAIM",Location
          pack      KeyLocation,"Key: ",DimPtr,DimPtr1,DimPtr2,DimPtr3,DimPtr4
          call      NPKGAIM
          loop
                    until over
                    if (FrmPtr = C0 | (FrmPtr = C1 & NPKGMaster = "1"))
                              call      PackageLoadListView
                    endif
                    eventcheck
                    until (QuitFlag = YES)
                    move      "P.LoadRec.-NPKGKG",Location
                    pack      KeyLocation,"Key: ",DimPtr,DimPtr1,DimPtr2,DimPtr3,DimPtr4
                    call      NPKGKG
          repeat
          move      NO,QuitFlag
          setprop   PackageQuit,enabled=0
.START PATCH 1.1 REPLACED LOGIC
.        PackageListView.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
.        PackageListView.EnsureVisible using 0,0
.        PackageListView2.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
.        PackageListView2.EnsureVisible using 0,0
.        PackageListView3.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
.        PackageListView3.EnsureVisible using 0,0
.         if (ViewFlag = 1)
.                 setfocus PackageListView
.                 call        Click_PackageListView
.         elseif (ViewFlag = 2)
.                 setfocus PackageListView2
.                 call        Click_PackageListView2
.         elseif (ViewFlag = 3)
.                 setfocus PackageListView3
.                 call        Click_PackageListView3
.         endif
          getprop   ListIts,*Count=result
          if (result > C0)
                    getprop ListIts,*Item(1)=ListIt
                    setprop   PackageListView,*SelectedItem=ListIt
                    call      Click_PackageListView
          endif
.END PATCH 1.1 REPLACED LOGIC
          return

.START PATCH 1.1 ADDED LOGIC
PackageLoadIsamRecords Routine DimPtr
          move    C1,NPKGPATH
          move      "P.LoadRec.-NPKGKEY",Location
          pack      KeyLocation,"Key: ",DimPtr
          call      NPKGKEY
          if not over
                    call      PackageLoadListView
          endif
          getprop   ListIts,*Count=result
          if (result > C0)
                    call      Click_PackageListView
          endif
          return
.END PATCH 1.1 ADDED LOGIC

PackageLoadPrice Routine DimPtr,DimPtr1
          PackageListViewPrice.DeleteAllItems giving N9
          if (DimPtr = "" and DimPtr1 = "")
                    PackageListViewOther.DeleteAllItems giving N9
                    call      PackageClearScreen1
                    return
          endif
          move    C1,NPRCPATH
          move      "P.LoadRec.-NPRCAIM",Location
          pack      KeyLocation,"Key: ",DimPtr,DimPtr1
          call      NPRCAIM
          loop
                    until over
                    call      PackageLoadListViewPrice
                    move      "P.LoadRec.-NPRCKG",Location
                    pack      KeyLocation,"Key: ",DimPtr,DimPtr1
                    call      NPRCKG
          repeat
        PackageListViewPrice.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
        PackageListViewPrice.EnsureVisible using 0,0
        call        Click_PackageListViewPrice
          return

PackageLoadOther Routine DimPtr,DimPtr1,DimPtr2
          PackageListViewOther.DeleteAllItems giving N9
          if (DimPtr = "" and DimPtr1 = "" and DimPtr2 = "")
                    call      PackageClearScreen2
                    return
          endif
          move    C1,NPRC2PATH
          move      "P.LoadRec.-NPRC2AIM",Location
          pack      KeyLocation,"Key: ",DimPtr,DimPtr1,DimPtr2
          call      NPRC2AIM
          loop
                    until over
                    call      PackageLoadListViewOther
                    move      "P.LoadRec.-NPRC2KG",Location
                    pack      KeyLocation,"Key: ",DimPtr,DimPtr1,DimPtr2
                    call      NPRC2KG
          repeat
        PackageListViewOther.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
        PackageListViewOther.EnsureVisible using 0,0
        call        Click_PackageListViewOther
          return

PackageLoadListView
          pack      hold,NPKGVARS
          move      C0,JULDAYS
.START PATCH 1.1 REPLACED LOGIC
..        PackageListView.InsertItem giving N9 using NPKGNUM
.        PackageListView.InsertItem giving N9 using NPKGPNAME
.        PackageListView.SetItemText using N9,NPKGID,1
.        PackageListView.SetItemText using N9,NPKGMLR,2
.         call      Trim using NPKGDATE
.         if (NPKGDATE = "")
.                   clear     str10
.         else
.                   unpack    NPKGDATE,CC,YY,MM,DD
.                   pack      str10,MM,SLASH,DD,SLASH,CC,YY
.                   call      CVTJUL
.         endif
.        PackageListView.SetItemText using N9,str10,3
.        PackageListView.SetItemText using N9,hold,4
..
.        PackageListView2.InsertItem giving N9 using NPKGID
.        PackageListView2.SetItemText using N9,NPKGPNAME,1
.        PackageListView2.SetItemText using N9,NPKGMLR,2
.        PackageListView2.SetItemText using N9,str10,3
.        PackageListView2.SetItemText using N9,hold,4
..
.         move      JULDAYS,str5
.        PackageListView3.InsertItem giving N9 using str5
.        PackageListView3.SetItemText using N9,str10,1
.        PackageListView3.SetItemText using N9,NPKGPNAME,2
.        PackageListView3.SetItemText using N9,NPKGID,3
.        PackageListView3.SetItemText using N9,NPKGMLR,4
.        PackageListView3.SetItemText using N9,hold,5
.......................................................
.Following Line is what causes the flicker
          ListIts.Add giving ListIt using *Index=1,*Text=NPKGPNAME
          setprop ListIt,*SubItems(1)=NPKGPNAME
          setprop ListIt,*SubItems(2)=NPKGID
          setprop ListIt,*SubItems(3)=NPKGMLR
          move      C0,N5
          call      Trim using NPKGDATE
          if (NPKGDATE = "")
                    clear     str10
          else
                    unpack    NPKGDATE,CC,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
                    call      CVTJUL
                    move      JULDAYS,N5
          endif
          setprop ListIt,*SubItems(4)=str10
          move      N5,str5
          setprop ListIt,*SubItems(5)=str5
          setprop ListIt,*SubItems(6)=hold
          if (NPKGMaster = "1")
                    getitem   red,0,colornum
                    setprop ListIt.ListSubItems(1),*ForeColor=colornum
                    setprop ListIt.ListSubItems(2),*ForeColor=colornum
                    setprop ListIt.ListSubItems(3),*ForeColor=colornum
                    setprop ListIt.ListSubItems(4),*ForeColor=colornum
                    setprop ListIt.ListSubItems(5),*ForeColor=colornum
                    setprop ListIt.ListSubItems(6),*ForeColor=colornum
          endif
.END PATCH 1.1 REPLACED LOGIC
          return

PackageLoadListViewPrice
          pack      hold1,NPRCVARS
        move    C0,JULDAYS
          call      Trim using NPRCDATE
        if (NPRCDATE <> "")
                unpack  NPRCDATE,CC,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,CC,YY
                call    cvtjul
          else
                    clear     str10
        endif
        move    JULDAYS,str5
        PackageListViewPrice.InsertItem giving N9 using str5
        PackageListViewPrice.SetItemText using N9,str10,1
          move      NPRCTOTAL,str11
          unpack    str11,str8,str3
          if (NPRCTOTAL < C0)
                    call      RemoveChar using str8,DASH
          endif
          call      FormatNumeric using str8,str10
          if (NPRCTOTAL < C0)
                    pack      str15,DASH,str10,str3
          else
                    pack      str15,str10,str3
          endif
        PackageListViewPrice.SetItemText using N9,str15,2
          move      NPRCPRINT,str9
          unpack    str9,str6,str3
          if (NPRCPRINT < C0)
                    call      RemoveChar using str6,DASH
          endif
          call      FormatNumeric using str6,str7
          if (NPRCPRINT < C0)
                    pack      str11,DASH,str7,str3
          else
                    pack      str11,str7,str3
          endif
        PackageListViewPrice.SetItemText using N9,str11,3
          move      NPRCPOST,str9
          unpack    str9,str6,str3
          if (NPRCPOST < C0)
                    call      RemoveChar using str6,DASH
          endif
          call      FormatNumeric using str6,str7
          if (NPRCPOST < C0)
                    pack      str11,DASH,str7,str3
          else
                    pack      str11,str7,str3
          endif
        PackageListViewPrice.SetItemText using N9,str11,4
          move      NPRCPREMIUM,str9
          unpack    str9,str6,str3
          if (NPRCPREMIUM < C0)
                    call      RemoveChar using str6,DASH
          endif
          call      FormatNumeric using str6,str7
          if (NPRCPREMIUM < C0)
                    pack      str11,DASH,str7,str3
          else
                    pack      str11,str7,str3
          endif
        PackageListViewPrice.SetItemText using N9,str11,5
        PackageListViewPrice.SetItemText using N9,hold1,6
          return

PackageLoadListViewOther
          pack      hold2,NPRC2VARS
.        PackageListViewOther.InsertItem giving N9 using NPRC2CODE
.        PackageListViewOther.SetItemText using N9,NPRC2OTHERNAME,1
.         move      NPRC2OTHER,str9
.         unpack    str9,str6,str3
.         if (NPRC2OTHER < C0)
.                   call      RemoveChar using str6,DASH
.         endif
.         call      FormatNumeric using str6,str7
.         if (NPRC2OTHER < C0)
.                   pack      str11,DASH,str7,str3
.         else
.                   pack      str11,str7,str3
.         endif
.        PackageListViewOther.SetItemText using N9,str11,2
.        PackageListViewOther.SetItemText using N9,hold2,3
.
        PackageListViewOther.InsertItem giving N9 using NPRC2OTHERNAME
          move      NPRC2OTHER,str9
          unpack    str9,str6,str3
          if (NPRC2OTHER < C0)
                    call      RemoveChar using str6,DASH
          endif
          call      FormatNumeric using str6,str7
          if (NPRC2OTHER < C0)
                    pack      str11,DASH,str7,str3
          else
                    pack      str11,str7,str3
          endif
        PackageListViewOther.SetItemText using N9,str11,1
        PackageListViewOther.SetItemText using N9,hold2,2
          return

PackageLoadScreen
          call      Trim using NPKGDATE
          if (NPKGDATE <> "")
                    unpack    NPKGDATE,str2,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,str2,YY
          else
                    clear     str10
          endif
          setitem   PackageEditDate,0,str10
.
          setitem   PackageEditID,0,NPKGID
.
          setitem   PackageEditMlr,0,NPKGMLR
.START PATCH 1.9 REPLACED LOGIC
.         move      C1,NMLRPATH
.         pack      MKEY,NPKGMLR,"000"
.         move      "P.LoadScreen-NMLRKEY",Location
.         pack      KeyLocation,"Key: ",MKEY
.         call      NMLRKEY
.         if over
.                   clear     MCOMP
.         endif
.         setitem   PackageStatMlrName,0,MCOMP
..........................................
          move      NPKGMLR,COMPFLD
          rep       zfill,COMPFLD
          move      "P.LoadScreen-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          if over
                    clear     COMPCOMP
          endif
          setitem   PackageStatMlrName,0,COMPCOMP
.END PATCH 1.9 REPLACED LOGIC
.
          setitem   PackageEditName,0,NPKGPNAME
.
          setitem   PackageEditNotes,0,NPKGNOTES
.
          setitem   PackageEditNumber,0,NPKGNUM
.START PATCH 1.1 ADDED LOGIC
          move      C0,N1
          move      NPKGMaster,N1
          setitem   PackageCheckMaster,0,N1
.
          setitem   PackageEditMaster,0,NPKGMastNum
.END PATCH 1.1 ADDED LOGIC
          return

PackageLoadScreenPrice
          move      NPRCPOST,str9
          unpack    str9,str6,str3
          if (NPRCPOST < C0)
                    call      RemoveChar using str6,DASH
          endif
          call      FormatNumeric using str6,str7
          if (NPRCPOST < C0)
                    pack      str11,DASH,str7,str3
          else
                    pack      str11,str7,str3
          endif
          setitem   PackageEditPostage,0,str11
.
          move      NPRCPREMIUM,str9
          unpack    str9,str6,str3
          if (NPRCPREMIUM < C0)
                    call      RemoveChar using str6,DASH
          endif
          call      FormatNumeric using str6,str7
          if (NPRCPREMIUM < C0)
                    pack      str11,DASH,str7,str3
          else
                    pack      str11,str7,str3
          endif
          setitem   PackageEditPremium,0,str11
.
          call      Trim using NPRCDATE
          if (NPRCDATE <> "")
                    unpack    NPRCDATE,str2,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,str2,YY
          else
                    clear     str10
          endif
          setitem   PackageEditPriceDate,0,str10
;patch1.7
          move      NPKGID to TNCBAKFLD
          call      trim      using     TNCBAKFLD
          if        (TNCBAKFLD <>       "")
                    call      TNCBAKKEY
                    if not over
                              move TNCBAKCOST to str9
                              setitem PackageEditBACKENDCOST,0,str9
                              clear     str9
                    else
                              setitem PackageEditBACKENDCOST,0,""
                    endif
          endif
;patch1.7
.
          setitem   PackageEditPriceNotes,0,NPRCNOTES
.
          move      NPRCPRINT,str9
          unpack    str9,str6,str3
          if (NPRCPRINT < C0)
                    call      RemoveChar using str6,DASH
          endif
          call      FormatNumeric using str6,str7
          if (NPRCPRINT < C0)
                    pack      str11,DASH,str7,str3
          else
                    pack      str11,str7,str3
          endif
          setitem   PackageEditPrinting,0,str11
.
          move      NPRCTOTAL,str11
          unpack    str11,str8,str3
          if (NPRCTOTAL < C0)
                    call      RemoveChar using str8,DASH
          endif
          call      FormatNumeric using str8,str10
          if (NPRCTOTAL < C0)
                    pack      str15,DASH,str10,str3
          else
                    pack      str15,str10,str3
          endif
          setitem   PackageEditTotal,0,str15
          return

PackageLoadScreenOther
          setitem   PackageEditOtherName,0,NPRC2OTHERNAME
.
          setitem   PackageEditOtherNum,0,NPRC2CODE
.
          move      NPRC2OTHER,str9
          unpack    str9,str6,str3
          if (NPRC2OTHER < C0)
                    call      RemoveChar using str6,DASH
          endif
          call      FormatNumeric using str6,str7
          if (NPRC2OTHER < C0)
                    pack      str11,DASH,str7,str3
          else
                    pack      str11,str7,str3
          endif
          setitem   PackageEditOtherPrice,0,str11
.
          call      Trim using NPRC2DATE
          if (NPRC2DATE <> "")
                    unpack    NPRC2DATE,str2,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,str2,YY
          else
                    clear     str10
          endif
          setitem   PackageEditOtherDate,0,str10
          return

PackageCalcTotalCost
          getitem   PackageEditPriceDate,0,str10
          call      Trim using str10
          if (str10 = "")
                    alert     caution,"Valid Price Date required in order to make Calculation!",result
                    return
          endif
          count     N2,str10
          if (N2 = 10)
                    unpack    str10,MM,str1,DD,str1,str4
          elseif (N2 = 8)
                    unpack    str10,MM,DD,str4
          else
                    alert     caution,"Price Date must be in MMDDCCYY Format in order to make Calculation!",result
                    return
          endif
          pack      NPRCDATE,str4,MM,DD
.
          call      PackageVerifyPrice2
.
          move      C0,NPRCTOTAL
          calc      NPRCTOTAL=(NPRCPOST + NPRCPREMIUM + NPRCPRINT)
          PackageListViewOther.GetItemCount giving howmany
          sub       C1,howmany
          for       result,"0",howmany
                    PackageListViewOther.GetItemText giving hold2 using result,2
                    unpack    hold2,NPRC2VARS
                    if (NPRC2Date = NPRCDATE)
                              add       NPRC2OTHER,NPRCTOTAL
                    endif
          repeat
.
          move      NPRCTOTAL,str11
          unpack    str11,str8,str3
          if (NPRCTOTAL < C0)
                    call      RemoveChar using str8,DASH
          endif
          call      FormatNumeric using str8,str10
          if (NPRCTOTAL < C0)
                    pack      str15,DASH,str10,str3
          else
                    pack      str15,str10,str3
          endif
          setitem   PackageEditTotal,0,str15
          return

PackageVerifyData
          move      "N",ReturnFlag
.
          getitem   PackageEditDate,0,str10
          call      Trim using str10
          if (str10 = "")
                    alert     caution,"Package Date required!",result
                    move      YES,ReturnFlag
                    setfocus PackageEditDate
                    return
          endif
          count     N2,str10
          if (N2 = 10)
                    unpack    str10,MM,str1,DD,str1,str4
          elseif (N2 = 8)
                    unpack    str10,MM,DD,str4
          else
                    alert     caution,"Package Date must be in MMDDCCYY Format!",result
                    move      YES,ReturnFlag
                    setfocus PackageEditDate
                    return
          endif
          pack      NPKGDATE,str4,MM,DD
.
          getitem   PackageEditID,0,NPKGID
          call      Trim using NPKGID
.
          call      PackageVerifyMlr
          if (ReturnFlag = YES)
                    return
          endif
.START PATCH 1.9 REPLACED LOGIC
.         move      str4,NPKGMLR
          move      str6,NPKGMLR
.END PATCH 1.9 REPLACED LOGIC
.START PATCH 1.2 ADDED LOGIC
          if (NPKGID <> "")
                    call      PackageTestforDupe using NPKGMLR,NPKGNUM,NPKGID,NewFlag,N1
                    if (N1 = C1)
                              alert     caution,"Duplicate Package ID found!",result
                              move      YES,ReturnFlag
                              setfocus PackageEditID
                              return
                    endif
          endif
.END PATCH 1.2 ADDED LOGIC
.
          getitem   PackageEditName,0,NPKGPNAME
          call      Trim using NPKGPNAME
.START PATCH 1.5 REPLACED LOGIC
..START PATCH 1.1 ADDED LOGIC
.         if (NPKGPNAME <> "")
.                   count     N9,NPKGPNAME
.                   if (N9 < 3)
.                             alert     caution,"Package Name must be clear, OR include 3 or more characters!",result
.                             move      YES,ReturnFlag
.                             setfocus PackageEditName
.                             return
.                   endif
..Not gonna work!
..                  move      NPKGPNAME,str3
..                  scan      STAR,str3
..                  if equal
..                            alert     caution,"First 3 Characters cannot contain '*'!",result
..                            move      YES,ReturnFlag
..                            setfocus PackageEditName
..                            return
..                  endif
..                  scan      QUESTION,str3
..                  if equal
..                            alert     caution,"First 3 Characters cannot contain '?'!",result
..                            move      YES,ReturnFlag
..                            setfocus PackageEditName
..                            return
..                  endif
..                  scan      B1,str3
..                  if equal
..                            alert     caution,"First 3 Characters cannot contain ' '!",result
..                            move      YES,ReturnFlag
..                            setfocus PackageEditName
..                            return
..                  endif
.         endif
...................
          if (NPKGPNAME <> "")
                    count     N9,NPKGPNAME
                    if (N9 < 3)
                              alert     caution,"Package Name must include 3 or more characters!",result
                              move      YES,ReturnFlag
                              setfocus PackageEditName
                              return
                    endif
          else
                    alert     caution,"Package Name required!",result
                    move      YES,ReturnFlag
                    setfocus PackageEditName
                    return
          endif
.END PATCH 1.1 ADDED LOGIC
.END PATCH 1.5 REPLACED LOGIC
.
          getitem   PackageEditNotes,0,NPKGNOTES
          call      Trim using NPKGNOTES
.Following is currently non-modifiable
.         getitem   PackageEditNumber,0,NPKGNUM
.
.START PATCH 1.1 ADDED LOGIC
          getitem   PackageCheckMaster,0,N1
          if (N1 = C1)
                    clear     taskname2
                    append    "Master Packages are used to group other Packages",taskname2
                    append    newline,taskname2
                    append    "while calculating Statistical History.",taskname2
                    append    newline,taskname2
                    append    "They cannot be associated with LR, LOL or Projection Record.",taskname2
                    append    newline,taskname2
.START PATCH 1.2 REPLACED LOGIC
.                   append    "Prices/IDs cannot be associated with them.",taskname2
                    append    "IDs cannot be associated with them.",taskname2
.END PATCH 1.2 REPLACED LOGIC
                    append    newline,taskname2
                    append    "Are you sure you want to continue?",taskname2
                    reset     taskname2
                    alert     plain,taskname2,result
                    if (result <> 1)
                              move      YES,ReturnFlag
                              setfocus PackageCheckMaster
                              return
                    endif
                    move      C1,NPKGMaster
.A Master Package cannot have a Master Package Number associated with it!!
                    setitem   PackageEditMaster,0,""
                    clear     NPKGMastNum
.A Master Package cannot have an ID associated with it!!
                    setitem   PackageEditID,0,""
                    clear     NPKGID
                    if (NPKGPNAME = "")
                              alert     caution,"Valid Package Name Required!",result
                              move      YES,ReturnFlag
                              setfocus PackageEditName
                              return
                    endif
          else
                    clear     NPKGMaster
                    getitem   PackageEditMaster,0,NPKGMastNum
                    call      Trim using NPKGMastNum
                    if (NPKGMastNum <> "")
.Save Key Value of current record prior to testing
.START PATCH 1.9 REPLACED LOGIC
.                             move      NPKGFLD,str10
                              move      NPKGFLD,str12
.END PATCH 1.9 REPLACED LOGIC
.
                              call      ZFillIt using NPKGMastNum,C0
                              pack      NPKGFLD,NPKGMlr,NPKGMastNum
                              rep       zfill,NPKGFLD
                              setitem   PackageEditMaster,0,NPKGMastNum
                              move      C1,NPKGPATH
                              move      "P.VerifyData-NPKGTSTA",Location
                              pack      KeyLocation,"Key: ",NPKGFLD
                              call      NPKGTSTA
                              if over
.Reset File Pointer to last record
                                        call      PackageResetFilePointer
.
                                        alert     caution,"Master Record does not exist!",result
                                        move      YES,ReturnFlag
                                        setfocus PackageEditMaster
                                        return
                              else
                                        if (str1 <> "1")
.Reset File Pointer to last record
                                                  call      PackageResetFilePointer
.
                                                  alert     caution,"Valid Master Record required!",result
                                                  move      YES,ReturnFlag
                                                  setfocus PackageEditMaster
                                                  return
                                        endif
.Reset File Pointer to last record
                                        call      PackageResetFilePointer
                              endif
                    endif
          endif
.END PATCH 1.1 ADDED LOGIC
          return

.START PATCH 1.1 ADDED LOGIC
PackageResetFilePointer
.Reset File Pointer to last record
.START PATCH 1.9 REPLACED LOGIC
.         move      str10,NPKGFLD
          move      str12,NPKGFLD
.END PATCH 1.9 REPLACED LOGIC
          rep       zfill,NPKGFLD
          move      "P.Reset-NPKGTST",Location
          pack      KeyLocation,"Key: ",NPKGFLD
          call      NPKGTST
          return
.END PATCH 1.1 ADDED LOGIC

PackageVerifyMlr
.START PATCH 1.9 REPLACED LOGIC
.         getitem   PackageEditMlr,0,str4
.         call      Trim using str4
.         if (str4 = "")
.                   alert     caution,"Valid Mailer required!",result
.                   move      YES,ReturnFlag
.                   setfocus PackageEditMlr
.                   return
.         endif
.         pack      MKEY,str4,"000"
.         move      "P.VerifyData-NMLRTST",Location
.         pack      KeyLocation,"Key: ",MKEY
.         call      NMLRTST
.         if over
.                   alert     caution,"Valid Mailer required!",result
.                   move      YES,ReturnFlag
.                   setfocus PackageEditMlr
.                   return
.         endif
...............................
          getitem   PackageEditMlr,0,str6
          call      Trim using str6
          if (str6 = "")
                    alert     caution,"Valid Mailer required!",result
                    move      YES,ReturnFlag
                    setfocus PackageEditMlr
                    return
          endif
          move      str6,COMPFLD
          rep       zfill,COMPFLD
          move      "P.VerifyData-COMPTST",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPTST
          if over
                    alert     caution,"Valid Mailer required!",result
                    move      YES,ReturnFlag
                    setfocus PackageEditMlr
                    return
          elseif (COMPMLRFLG <> "T")
                    pack      taskname,"This Company is not listed as a Mailer.",newline,"Valid Mailer required!"
                    alert     caution,taskname,result
                    move      YES,ReturnFlag
                    setfocus PackageEditMlr
                    return
          endif
.END PATCH 1.9 REPLACED LOGIC
          return

PackageVerifyPrice
          move      "N",ReturnFlag
.
          getitem   PackageEditPriceDate,0,str10
          call      Trim using str10
          if (str10 = "")
                    alert     caution,"Valid Price Date required!",result
                    move      YES,ReturnFlag
                    setfocus PackageEditPriceDate
                    return
          endif
          count     N2,str10
          if (N2 = 10)
                    unpack    str10,MM,str1,DD,str1,str4
          elseif (N2 = 8)
                    unpack    str10,MM,DD,str4
          else
                    alert     caution,"Price Date must be in MMDDCCYY Format!",result
                    move      YES,ReturnFlag
                    setfocus PackageEditPriceDate
                    return
          endif
          pack      NPRCDATE,str4,MM,DD
.
          getitem   PackageEditPriceNotes,0,NPRCNOTES
          call      Trim using NPRCNOTES
          call      BFillIt using NPRCNOTES
.
          getitem   PackageEditTotal,0,str15
          call      RemoveChar using str15,COMMA
          call      Trim using str15
          clear     NPRCTOTAL
          move      str15,NPRCTOTAL
          if (NPRCTOTAL <= C0)
                    alert     caution,"Valid Total required!",result
                    move      YES,ReturnFlag
                    setfocus PackageEditTotal
                    return
          endif
PackageVerifyPrice2
          getitem   PackageEditPostage,0,str11
          call      RemoveChar using str11,COMMA
          call      Trim using str11
          clear     NPRCPOST
          move      str11,NPRCPOST
.
          getitem   PackageEditPremium,0,str11
          call      RemoveChar using str11,COMMA
          call      Trim using str11
          clear     NPRCPREMIUM
          move      str11,NPRCPREMIUM
.
          getitem   PackageEditPrinting,0,str11
          call      RemoveChar using str11,COMMA
          call      Trim using str11
          clear     NPRCPRINT
          move      str11,NPRCPRINT
          return

PackageVerifyOtherPrice
          move      "N",ReturnFlag
          getitem   PackageEditOtherName,0,NPRC2OTHERNAME
          call      Trim using NPRC2OTHERNAME
          if (NPRC2OTHERNAME = "")
                    alert     caution,"Valid Other Price Name required!",result
                    move      YES,ReturnFlag
                    setfocus PackageEditOtherName
                    return
          endif
          call      BFillIt using NPRC2OTHERNAME
.
.         getitem   PackageEditOtherNum,0,NPRC2NUM
.
          getitem   PackageEditOtherPrice,0,str11
          call      RemoveChar using str11,COMMA
          call      Trim using str11
          clear     NPRC2OTHER
          move      str11,NPRC2OTHER
          if (NPRC2OTHER <= C0)
                    alert     caution,"Valid Other Price required!",result
                    move      YES,ReturnFlag
                    setfocus PackageEditOtherPrice
                    return
          endif
.
          getitem   PackageEditOtherDate,0,str10
          call      Trim using str10
          if (str10 = "")
                    alert     caution,"Valid Other Date required!",result
                    move      YES,ReturnFlag
                    setfocus PackageEditOtherDate
                    return
          endif
          count     N2,str10
          if (N2 = 10)
                    unpack    str10,MM,str1,DD,str1,str4
          elseif (N2 = 8)
                    unpack    str10,MM,DD,str4
          else
                    alert     caution,"Other Date must be in MMDDCCYY Format!",result
                    move      YES,ReturnFlag
                    setfocus PackageEditOtherDate
                    return
          endif
          pack      NPRC2DATE,str4,MM,DD
          return

PackageLoadTreeView Routine DimPtr
          PackageTreeView.DeleteAll giving result
          move    C1,NPKGPATH
          move      DimPtr,NPKGFLD
          move      "P.LoadTree.-NPKGKEY",Location
          pack      KeyLocation,"Key: ",NPKGFLD
          call      NPKGKEY
          if not over
                    pack      hold,NPKGVARS
                    PackageTreeView.InsertItem Giving MyRoot Using hold, TVI_ROOT,TVI_1ST
                    move    C1,NPRCPATH
.START PATCH 1.9 REPLACED LOGIC
.                   unpack    NPKGFLD,str4,str6
.                   pack      NPRCFLD1,"01X",str4
                    unpack    NPKGFLD,str6A,str6
                    pack      NPRCFLD1,"01X",str6A
.END PATCH 1.9 REPLACED LOGIC
                    pack      NPRCFLD2,"02X",str6
                    move      "P.LoadTree.-NPRCAIM",Location
                    pack      KeyLocation,"Key: ",NPRCFLD1,NPRCFLD2
                    call      NPRCAIM
                    if not over
                              move    C1,NPRC2PATH
.START PATCH 1.9 REPLACED LOGIC
.                             unpack    NPKGFLD,str4,str6
.                             pack      NPRC2FLD1,"01X",str4
                              unpack    NPKGFLD,str6A,str6
                              pack      NPRC2FLD1,"01X",str6A
.END PATCH 1.9 REPLACED LOGIC
                              pack      NPRC2FLD2,"02X",str6
                              loop
                                        pack      hold1,NPRCVARS
                                        PackageTreeView.InsertItem giving level2 Using hold1,MyRoot,TVI_SORT
                                        pack      NPRC2FLD3,"03X",NPRCDATE
                                        move      "P.LoadTree.-NPRC2AIM",Location
                                        pack      KeyLocation,"Key: ",NPRC2FLD1,NPRC2FLD2,NPRC2FLD3
                                        call      NPRC2AIM
                                        loop
                                                  until over
                                                  pack      hold2,NPRC2VARS
                                                  PackageTreeView.InsertItem Using hold2,level2,TVI_SORT
                                                  move      "P.LoadTree.-NPRC2KG",Location
                                                  pack      KeyLocation,"Key: ",NPRC2FLD1,NPRC2FLD2,NPRC2FLD3
                                                  call      NPRC2KG
                                        repeat
                                        move      "P.LoadTree.-NPRCKG",Location
                                        pack      KeyLocation,"Key: ",NPRCFLD1,NPRCFLD2
                                        call      NPRCKG
                                        until over
                              repeat
                    endif
          endif
          return

PackageLoadNewTreeView
          PackageTreeView.DeleteAll giving result
          move      NO,ReturnFlag
          call      PackageVerifyData
          if (ReturnFlag <> YES)
                    pack      hold,NPKGVARS
                    PackageTreeView.InsertItem Giving MyRoot Using hold, TVI_ROOT,TVI_1ST
          endif
          move      "N",NewFlag2
          return

PackageSearchTreeView Routine FrmPtr,FrmPtr1,DimPtr
.FrmPtr = Record Type:  1 = Price, 2 = Other Price
.FrmPtr1 = Operation:   1 = Add, 2 = Remove, 3 = Load into ListView, 4 = Save
.DimPtr = Record
          move      root,handle
          move      root,handle2
          PackageTreeView.GetNextItem GIVING handle USING *Item=handle2,*Code=root
          loop
                    until (handle = root)
                    PackageTreeView.GetItemText Giving hold Using handle
                    move      handle,handle2
                    PackageTreeView.GetNextItem GIVING handle USING *Item=handle2,*Code=child
                    loop
.                             until (handle=root)
.                             PackageTreeView.GetItemText Giving hold1 Using handle
.                             if (FrmPtr1 = 1)
..Add New Records
.                                       if (FrmPtr = 1)
..Add a New Price Record
.                                                 PackageTreeView.InsertItem Using DimPtr,MyRoot,TVI_SORT
.                                                 goto PackageSearchTreeViewEnd
.                                       elseif (FrmPtr = 2)
..Adding a New Other Price Record - Get the correct Parent Record
.                                                 unpack    DimPtr,NPRC2MLR,NPRC2NUM,NPRC2DATE
.                                                 unpack    hold1,NPRCMLR,NPRCNUM,NPRCDATE
.                                                 if (NPRC2MLR = NPRCMLR & NPRC2NUM = NPRCNUM & NPRC2DATE = NPRCDATE)
.                                                           PackageTreeView.InsertItem Using DimPtr,handle,TVI_SORT
.                                                           goto PackageSearchTreeViewEnd
.                                                 endif
.                                       endif
.                             endif
                              if (FrmPtr1 = 1)
                                        if (FrmPtr = 1)
.Add a New Price Record
                                                  PackageTreeView.InsertItem Using DimPtr,MyRoot,TVI_SORT
                                                  goto PackageSearchTreeViewEnd
                                        endif
                              endif
                              until (handle=root)
                              PackageTreeView.GetItemText Giving hold1 Using handle
                              if (FrmPtr1 = 1)
                                        if (FrmPtr = 2)
.Adding a New Other Price Record - Get the correct Parent Record
                                                  unpack    DimPtr,NPRC2MLR,NPRC2NUM,NPRC2DATE
                                                  unpack    hold1,NPRCMLR,NPRCNUM,NPRCDATE
                                                  if (NPRC2MLR = NPRCMLR & NPRC2NUM = NPRCNUM & NPRC2DATE = NPRCDATE)
                                                            PackageTreeView.InsertItem Using DimPtr,handle,TVI_SORT
                                                            goto PackageSearchTreeViewEnd
                                                  endif
                                        endif
                              endif
                              move      handle,handle2
                              PackageTreeView.GetNextItem GIVING handle USING *Item=handle2,*Code=child
                              loop
                                        if (FrmPtr1 = 2 & FrmPtr = 1)
.Remove Price Record
                                                  unpack    hold1,NPRCFLD
.START PATCH 1.9 REPLACED LOGIC
.                                                 unpack    DimPtr,str18
.                                                 if (NPRCFLD = str18)
                                                  unpack    DimPtr,str20
                                                  if (NPRCFLD = str20)
.END PATCH 1.9 REPLACED LOGIC
                                                            if (handle = root)  .Okay to go forward
                                                                      PackageTreeView.DeleteItem USING handle2
                                                            else
                                                                      clear     taskname
                                                                      append    "There are Additional Price Records Under this Price Record!",taskname
                                                                      append    NewLine,taskname
                                                                      append    "You must Delete those first!",taskname
                                                                      reset     taskname
                                                                      alert     caution,taskname,result
                                                                      move      "*",DimPtr
                                                            endif
                                                            goto PackageSearchTreeViewEnd
                                                  endif
                                        endif
                                        until (handle = root)
                                        PackageTreeView.GetItemText Giving hold2 Using handle
                                        if (FrmPtr1 = 2 & FrmPtr = 2)
.START PATCH 1.9 REPLACED LOGIC
.                                                 unpack    DimPtr,str18
.                                                 unpack    str18,str24
.                                                 unpack    hold2,str18
.                                                 if (str24 = str18)
...................
                                                  unpack    DimPtr,str20
                                                  unpack    str20,str24
                                                  unpack    hold2,str20
                                                  if (str24 = str20)
.END PATCH 1.9 REPLACED LOGIC
.Remove Additional Price Record
                                                            PackageTreeView.DeleteItem USING handle
                                                            goto PackageSearchTreeViewEnd
                                                  endif
                                        elseif (FrmPtr1 = 3 & FrmPtr = 2)
.START PATCH 1.9 REPLACED LOGIC
.                                                 unpack    hold2,str18
.                                                 if (str18 = DimPtr)
                                                  unpack    hold2,str20
                                                  if (str20 = DimPtr)
.END PATCH 1.9 REPLACED LOGIC
                                                            unpack    hold2,NPRC2VARS
                                                            call      PackageLoadListViewOther
                                                  endif
                                        elseif (FrmPtr1 = 4 & FrmPtr = 2)       .Save Records
                                                  unpack    hold2,NPRC2VARS
                                                  move      DimPtr,NPRC2NUM
.Get next available Other Price Number
                                        move    C0,N3
                                                  loop
                                                            add       C1,N3
                                                  move    N3,NPRC2CODE
                                                          rep     zfill,NPRC2CODE
                                                            pack      NPRC2FLD,NPRC2MLR,NPRC2NUM,NPRC2DATE,NPRC2CODE
                                                            move      C1,NPRC2PATH
                                                            move      "Save-NPRC2TST",Location
                                                            pack    KeyLocation,"Key: ",NPRC2FLD
                                                            call      NPRC2TST
                                                            until over
                                                            if (N3 >= 998)
                                                                      clear     taskname
                                                                      append    "You have exceeded the number of Additional Prices allowed for a Price entry!",taskname
                                                                      append    NewLine,taskname
                                                                      append    "Current record will not be retained!",taskname
                                                                      reset     taskname
                                                                      alert     caution,taskname,result
                                                                      goto PackageSearchSaveEnd
                                                            endif
                                                  repeat
                                                  pack      NPRC2FLD,NPRC2MLR,NPRC2NUM,NPRC2DATE,NPRC2CODE
                                                  move      "Save-NPRC2WRT",Location
                                                  pack      KeyLocation,"Key: ",NPRC2FLD
                                                  call      NPRC2WRT
                                        endif
PackageSearchSaveEnd
                                        move      handle,handle2
                                        PackageTreeView.GetNextItem GIVING handle USING *Item=handle2,*Code=next
                                        if (handle = root)
                                                  PackageTreeView.GetNextItem GIVING handle USING *Item=handle2,*Code=parent
                                                  move      handle,handle2
                                                  break
                                        endif
                              repeat
                              PackageTreeView.GetNextItem GIVING handle USING *Item=handle2,*Code=next
                    repeat
                    PackageTreeView.GetNextItem GIVING handle USING *Item=handle2,*Code=next
          repeat
PackageSearchTreeViewEnd
          return

.AdjustMarkAdjBusy
.         move      "A.MarkFree-NADJUPD",Location
.        pack    KeyLocation,"Key: ",NADJFLD
.         move      STAR,ASCODE
.         call      NADJUPD
.         move      YES,AdjBFlag
.         return

.AdjustMarkAdjFree
.         move      "A.MarkFree-NADJUPD",Location
.        pack    KeyLocation,"Key: ",NADJFLD
.         move      "J",ASCODE
.         call      NADJUPD
.         move      NO,AdjBFlag
.         return

.AdjustSetISPassword
.        call    Report2DestroyObjects
.        setprop Report2,title="NINCA Administrator Mode Password"
.        move    NO,RptCan
.        create  Report2;StatTextBoxes(1)=50:70:10:110,"Password",""
.        create  Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=10,EditType=5,SelectAll=1,Style=1,Border=1
.        create  Report2;Buttons(1)=205:230:50:100,"O&K",zorder=500,default=1
.        activate StatTextBoxes(1)
.        activate EditTextBoxes(1)
.        activate Buttons(1),AdjustAdminOK,result
.        listins ObjectColl,StatTextBoxes(1),EditTextBoxes(1),Buttons(1)
.        setfocus EditTextBoxes(1)
.        return

.AdjustAdminOK
.        getitem EditTextBoxes(1),0,str10
.        call    Trim using str10
.        if (str10 = "")
.                setitem StatTextBoxes(2),0,"Not a valid Password!!"
.                setfocus EditTextBoxes(1)
.        else
.                   if (str10 = "COSMO")
.                             move      C1,SecFlag
.                   endif
.                setprop Report2,visible=0
.        endif
.        return

..Routine which destroys all objects created from above routines
Report2DestroyObjects
.        destroy ObjectColl
        return

.PackageFormatNumeric LRoutine EditPtr,FrmPtr
.         if (FrmPtr = C1)
.                   getitem   EditPtr,0,str15
.                   call      RemoveChar using str15,COMMA
.                   scan      DASH,str15
.                   if equal
.                             move      DASH,str1
.                             reset     str15
.                   else
.                             clear     str1
.                   endif
.                   call      RemoveChar using str15,DASH
.                   move      C0,N92
.                   move      str15,N92
.                   move      N92,str15
.                   unpack    str15,str9,str3
.                   call      FormatNumeric using str9,str11
.                   pack      str15,str1,str11,str3
.                   setitem   EditPtr,0,str15
.         elseif (FrmPtr = C2)
.                   getitem   EditPtr,0,str10
.                   call      RemoveChar using str10,COMMA
.                   scan      DASH,str10
.                   if equal
.                             move      DASH,str1
.                             reset     str10
.                   else
.                             clear     str1
.                   endif
.                   call      RemoveChar using str10,DASH
.                   move      C0,N52
.                   move      str10,N52
.                   move      N52,str10
.                   unpack    str10,str5,str3
.                   call      FormatNumeric using str5,str6
.                   pack      str10,str1,str6,str3
.                   setitem   EditPtr,0,str10
.         endif
.         return

.AdjustDisableSave
.        setitem AdjustSave,0,"Save"
.        return
.AdjustEnableSave
.        setitem AdjustSave,0,"Sa&ve"
.        return

PackageFormatDate Routine EditPtr
          getitem   EditPtr,0,str10
          call      Trim using str10
          count     N9,str10
          if (N9 = 10 | N9 = 8)
                    if (N9 = 10)
                              unpack    str10,MM,str1,DD,str1,str4
                    elseif (N9 = 8)
                              unpack    str10,MM,DD,str4
                    endif
                    pack      str10,MM,SLASH,DD,SLASH,str4
          else
                    clear     str10
          endif
          setitem   EditPtr,0,str10
          return

PackageFormatPrice Routine EditPtr
          getitem   EditPtr,0,str15
        call    RemoveChar using str15,COMMA
        move        C0,Temp122
          move      str15,Temp122
          move      Temp122,str15
        unpack  str15,str12,str3
        call    Trim using str12
        call    FormatNumeric using str12,str15
        pack    str18,str15,str3
          setitem   EditPtr,0,str18
        return

PackageLoadOK Routine DimPtr
          setitem   PackageEditSearch,0,DimPtr
          call      Click_PackageOK
          return

PackageSetListView Routine DimPtr
          move      C0,howmany
.START PATCH 1.1 REPLACED LOGIC
.         if (ViewFlag = 1)
.                 PackageListView.GetItemCount giving result
.         elseif (ViewFlag = 2)
.                 PackageListView2.GetItemCount giving result
.         elseif (ViewFlag = 3)
.                 PackageListView3.GetItemCount giving result
.         endif
.         sub       C1,result
.         for N9 from "0" to result
.                   if (ViewFlag = 1)
.                             PackageListView.GetItemText giving hold using N9,C4
.                   elseif (ViewFlag = 2)
.                             PackageListView2.GetItemText giving hold using N9,C4
.                   elseif (ViewFlag = 3)
.                             PackageListView3.GetItemText giving hold using N9,C5
.                   endif
.                   unpack    hold,str4,str6
.                   if (DimPtr = str6)
.                             move      N9,howmany
.                             break
.                   endif
.         repeat
.         if (ViewFlag = 1)
.                 PackageListView.SetItemState GIVING N9 USING *Index=howmany,*State=3,*Statemask=3
.         PackageListView.EnsureVisible using howmany,0
.                 call        Click_PackageListView
.         elseif (ViewFlag = 2)
.                 PackageListView2.SetItemState GIVING N9 USING *Index=howmany,*State=3,*Statemask=3
.         PackageListView2.EnsureVisible using howmany,0
.                 call        Click_PackageListView2
.         elseif (ViewFlag = 3)
.                 PackageListView3.SetItemState GIVING N9 USING *Index=howmany,*State=3,*Statemask=3
.         PackageListView3.EnsureVisible using howmany,0
.                 call        Click_PackageListView3
.         endif
.........................
        getprop PackageListView,*ListItems=ListIts
          getprop   ListIts,*Count=result
          if (result > C0)
                    move      C0,IntIndex3
                    move      C6,IntIndex2
                    sub       C1,result
                    for IntIndex,"0",result
                              getprop   ListIts(IntIndex),*SubItems(IntIndex2)=hold
.START PATCH 1.9 REPLACED LOGIC
.                             unpack    hold,str4,str5,str1
.                             pack      str7,str5,str1
.                             if (DimPtr = str7)
                              unpack    hold,str6a,str6a
                              if (DimPtr = str6a)
.END PATCH 1.9 REPLACED LOGIC
                                        move      IntIndex,IntIndex3
                                        break
                              endif
                    repeat
                    getprop ListIts,*Item(IntIndex3)=ListIt
                    setprop   PackageListView,*SelectedItem=ListIt
                  call        Click_PackageListView
          endif
.END PATCH 1.1 REPLACED LOGIC
          return

.PackageDisableForm Routine FrmPtr
PackageDisableForm Routine
          deactivate NPKG0001
          deactivate NPKGMSK1
          return
.PackageEnableForm Routine FrmPtr
PackageEnableForm Routine
          activate NPKG0001
          activate NPKGMSK1
.START PATCH 1.1 REPLACED LOGIC
.         if (ViewFlag = 1)
.                   setprop   PackageListView2,visible=0
.                   setprop   PackageListView3,visible=0
.                   setprop   PackageListView,visible=1
.         elseif (ViewFlag = 2)
.                   setprop   PackageListView,visible=0
.                   setprop   PackageListView3,visible=0
.                   setprop   PackageListView2,visible=1
.         elseif (ViewFlag = 3)
.                   setprop   PackageListView,visible=0
.                   setprop   PackageListView2,visible=0
.                   setprop   PackageListView3,visible=1
.         endif
          setprop   PackageListView,visible=1
.END PATCH 1.1 REPLACED LOGIC
          return

          include   npkgio.inc
          include   nprcio.inc
          include   nprc2io.inc
;patch1.8
          include   compio.inc
          include   cntio.inc
;         include   nmlrio.inc
;Patch1.8
          include   gnxtio.inc
.START PATCH 1.2 ADDED LOGIC
          include   npkgio2.inc
.END PATCH 1.2 ADDED LOGIC
;PATCH1.7
          INCLUDE   TNCBAKIO.INC
;PATCH1.7

          include   comlogic.inc