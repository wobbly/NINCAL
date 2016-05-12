........................................
. Program:      NSMP0001.PLS
. Function:     Sample File Maintenance
. Author:       Andrew Harkins
. Orig. Date:   December 9,1999
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
.Patch1.8
                              include compdd.inc
                              include cntdd.inc
.        include nmlrdd.inc
.Patch1.8
        include npasdd.inc
        include nsmpdd.inc
        include nusedd.inc
        include winapi.inc
.Following used only in order to load Search.plf
.Patch1.8
.        include nbrkdd.inc
.patch1.8
        include ndatdd.inc
        include nrtndd.inc
.START PATCH 1.2 ADDED INCLUDE
        include ncmpdd.inc
        include norddd.inc
.END PATCH 1.2 ADDED INCLUDE
          include   nowndd.inc
release   init      "2.2.3"   DMS       10MARCH2006         Add excel dump button for records in sample file
reldate	init	"2006 March 10"
.release  init      "2.2.2" DMB 23JAN2006 UPdate file folder location
.release  init      "2.2.1" ASH 20MAY2005 Bug Fix:  Check for validity of NSMPFLD prior to updating
.        move    "MAY 20, 2005",Wreldate
.release  init      "2.2" ASH 15DEC2004 Bug Fix:  Hitting 'Cancel' when FileSelect window popped up was causing I40 error
.        move    "December 15, 2003",Wreldate
.release  init      "2.1" ASH 08DEC2004 Added option to allow users to scan their own Samples from their desktops
.release  init      "1.9" ASH 29OCT2004 Mailer Conversion - Increased Mailer field to 6 bytes
.release  init      "1.8" DMB 26MAY2004 Mailer Conversion
.release init    "1.7"   ASH  21Aug2003     Moved Samples to TIFF (DCX logic maintained in case of emergency)
.                                                           REmoved excessive calls of GetWinVer
.                                                           Remodeled the interface
.        move    "August 21, 2003",Wreldate
.release init    "1.6"   AH  09Sep2002     Added Inactive Code
.release init    "1.52"   AH  24Jul2002     New machine up front.  Removed pause
.release init    "1.51"   AH  23Jul2002     New machine up front.  Decreased pause
.release init    "1.5"   DLH 12Jul2002     Use GetWinVer
.release init    "1.4"   ASH 19MAR01     NINORD MOVED TO FILE MANAGER
.                                       NORD001H NOW HAS ITS OWN .PLC
.release init    "1.3"   DLH/ASH/JD 08Nov2000 timestamp/loop logic to wait for files to be acknowledged.
.release init    "1.2"   ASH 21JUN2000 ADDED CAMPAIGN INCLUDE FILES
.release init    "1.1"   ASH 04APR2000 ADDED LOGIC TO SET FILE ATTRIBUTES
.                                     ADDED ROUTINE TO CLOSE DOWN ORDERINFO WINDOW
.                                     ADDED LOGIC TO FIND PORT NUMBER IF NOT RUN THROUGHT MASTER.PLC
.RELEASE INIT    "1.0"   ASH 06dec99 ORIGINAL RELEASE

.START PATCH 1.4 ADDED LOGIC
.EXTERNAL ROUTINES FROM INFO.PLC
SampleLoadForm external "INFO;LoadForm"
.test 11 aug 2010
.SampleDisplayMailer external "INFO;DisplayMailer"
.end test 11 aug 2010
.Need to call these routines with Order prefix as they are called via NORD001F.PLF
OrderDisplayMessage external "INFO;DisplayMessage"
OrderInfoClose external "INFO;InfoClose"
.END PATCH 1.4 ADDED LOGIC

.TESTVARS
TESTINT1        INTEGER 4
TESTINT2        INTEGER 4
str4b   dim     4

Timer   Timer
.Files to open
prfile  pfile
.START PATCH 1.7 ADDED LOGIC
preffile file
.END PATCH 1.7 ADDED LOGIC
.pcxfile file
.Used to keep track of tabs during Updating and Saving
TabNum   form   1
ExitFlag init   "Y"
ReturnFlag init "N"
UpdateFlag init "N"
NewFlag init    "N"
MlrFlag init    "N"
HoldFlag form   "1"
AKey1   init    "01X"
.Following key not used by program but required for search.plf
AKey2   init    "01F"
AKey3   init    "02F"
AKey1A  init    "01L"
AKey2A  init    "02L"
filler  init    "0000"
filler2 init    "0000000"
badstat init    "B*P"
newdate1 dim     10
olddate dim     10
Carr    init    0x7f
testff  init    0xff
test55  init    0x55
test00  init    0x00
test01  init    0x01
test1   init    0x33
test2   init    0x08
.hexeight integer 4,"4294967295"
hextwo  init    0x02
hexfour init    0x04
ScanBreak form  "0"
.START PATCH 1.7 ADDED LOGIC
TIFFBreak form      "0"
.END PATCH 1.7 ADDED LOGIC

.Length of record plus space for B1 and space for "/"
hold    dim     257
key     dim     45
holdkey dim     7
.START PATCH 1.9 REPLACED LOGIC
.holdmlr dim     4
holdmlr dim     6
.END PATCH 1.9 REPLACED LOGIC
holdcnt dim     3
holdstat dim    1

.Vars used for Report Screen
RptCan  dim     1
FromNo  dim     4
ToNo    dim     4
FromDate dim    8
ToDate  dim     8
List    form    1
Inactive form   1
Preview form    1
Default form    1
Select  form    1

.Vars used for printing
mlrprnt dim     4
linenumber form 1
maxline form    "7"
page    form    9

.Vars used by Nord001f - OrderSamples
.dcxpath init    "\\nts0\d\data\samples\"
dcxpath init    "\\nins1\e\data\samples\"                              .\
.dcxpath init    "g:\data\samples\"
bmppath init    "c:\work\"                              ."
SmpPage form    5
SmpPage2 form   5
SmpFile pfile
SmpScale form   3

.Objects used by Options.plf (a generic form)
OptionsGroupBox GroupBox
OptionsScannerCheckBox CheckBox
OptionsStatRes  StatText
OptionsStatX    StatText
OptionsEditX    EditText
OptionsStatY    StatText
OptionsEditY    EditText
OptionsStatType StatText
OptionsTypeComboBox ComboBox
OptionsStatDim  StatText
OptionsStatW    StatText
OptionsEditW    EditText
OptionsStatH    StatText
OptionsEditH    EditText
OptionsStatUnits StatText
OptionsUnitsComboBox ComboBox
OptionsStatScale StatText
OptionsEditScale EditText
OptionsStatThresh StatText
OptionsEditThresh EditText
OptionsCollection Collection
.START PATCH 1.7 ADDED LOGIC
.Objects used by OptionsOrd.plf         (a generic form)
.OptionsArrayEx     dim       x(y,z)
.Following Array will allow:  y         rows - representing each Screen
.                                   z   columns   - representing # of preferances         for each Screen
.                                   x   length of each field in       each Screen
.
OptionsArr1 CONST   "2"       .Current Number of items used in Preference Screen
OptionsArr2 CONST   "5"
OptionsArrSize CONST          "5"
OptionsArray dim    OptionsArrSize(OptionsArr1,OptionsArr2)
.
OptionsDCXCheckBox CheckBox
OptionsDefaultPathStat StatText
OptionsDefaultPath EditText
.
colordim dim        8
.END PATCH 1.7 ADDED LOGIC

.Variables used for WinAPI calls for Twain device
TwainDisplay    Integer 4
TwainUnits      Integer 4
TwainXDPI       Integer 4
TwainYDPI       Integer 4
TwainFormat     Integer 4
TwainSize       Integer 4
TwainDepth      Integer 4
TwainThreshold  Integer 4
TwainRecDim     Dim     32
TwainRecord     RECORD          . Structure
Left            Integer 4,"0"   . Left
Top             Integer 4,"0"   . Top
Right           Integer 4,"0"   . Right
Bottom          Integer 4,"0"   . Bottom
                RECORDEND       . STRUCTure size =   16 Mem Align=4 byte
APIFileNameA    dim     29
APIFileNameA2   dim     29
.Colors
white   color
grey    color
RED     COLOR
BLACK   COLOR

.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font

.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu
.Set Up SubMenu for Options
sSearch submenu

.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search;-;&Preferences"
HData   init    "&Help;&About"

.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To"
.timestamp1 dim  16
timestamp2 dim  16
.time1   form    16
.time2   form    16
.time3   form    16
.START PATCH 1.7 ADDED LOGIC
HoldDir   dim       200
HoldFName dim       45
FileName1 dim       250
FileName2 dim       250
.END PATCH 1.7 ADDED LOGIC
.START PATCH 2.0 ADDED LOGIC
.I am not going to use this new logic right now.  Try having everyone use the settings for the front desk
.userlogn dim       7
.END PATCH 2.0 ADDED LOGIC
.................................
.Start Patch #1.0 - added logic for dynamic form sizing
.coll1   collection
.specs   form          4(4)
.size    form          "1.000"
.infostring dim        590
.
.Getinfo - NOT YET IMPLEMENTED!!!!!!!
.        getinfo system,infostring
.        bump    infostring,12
.        move    infostring,str4
.        bump    infostring,4
.        move    infostring,str5
.End Patch #1.0 - added logic for dynamic form sizing
.............................

.Set Vars used for About Box
        move    "NSMP0001.PLS",Wprognme
        move    "Sample File Maintenance",Wfunction
        move    "Andrew Harkins",Wauthor
        move    release,Wrelease
        move    Reldate,Wreldate

.Declare forms, Always declare child forms first
srch    plform  Search
opt     plform  Options
rpt     plform  Report
mss1    plform  Error
pss     plform  Passwrd
abt     plform  About
.START PATCH 1.4 REMOVED LOGIC
.INFO    plform  NORD001H
.END PATCH 1.4 REMOVED LOGIC
x       plform  NSMP0001
        winhide

.Load Forms, Always load parent form first
        formload x
.START PATCH 1.4 REMOVED LOGIC
.        formload INFO
.END PATCH 1.4 REMOVED LOGIC
        formload abt
        formload pss
        formload mss1
        formload rpt
        formload opt
        formload srch

        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT

.Create Menus
        create  NSMP0001;mFile,FData
        create  NSMP0001;mEdit,EData,mFile
        create  NSMP0001;mOptions,OData,mEdit
        create  NSMP0001;mHelp,HData,mOptions

.Create SubMenu
        create  NSMP0001;sSearch,SData,mOptions,1

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

.Create Colors for EditText Inquiry
        create  white=*white
.START PATCH 1.7 REPLACED LOGIC
.        create  grey=*ltgray
          create    grey=220:220:220
.END PATCH 1.7 REPLACED LOGIC
        create  RED=*RED
        create  black=*black

.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic

.Create/Activate Objects on Options.plf(a generic form)
.        create  Options;OptionsGroupBox=10:180:20:310,title="Scanner Settings"
.        create  Options;OptionsScannerCheckBox=30:50:30:190,"Display Scanner Interface",tabid=10
.        create  Options;OptionsStatRes=50:70:30:90,"Resolution:","'>MS Sans Serif'(8)"
.        create  Options;OptionsStatX=50:70:100:140,"X DPI","'>MS Sans Serif'(8)"
.        create  Options;OptionsEditX=50:70:140:180,SelectAll=1,Style=1,Border=1,MaxChars=4,EditType=2,tabid=20
.        create  Options;OptionsStatY=50:70:190:230,"Y DPI","'>MS Sans Serif'(8)"
.        create  Options;OptionsEditY=50:70:230:270,SelectAll=1,Style=1,Border=1,MaxChars=4,EditType=2,tabid=30
.        create  Options;OptionsStatType=70:90:30:90,"Type:","'>MS Sans Serif'(8)"
.        create  Options;OptionsTypeComboBox=70:90:100:270,"":
.        ";)Black & White Bitmap;)Gray 16 Bit;)Gray 256 Bit;)Color 16 Bit;)Color 256 Bit;)True Color",tabid=40
.        create  Options;OptionsStatDim=90:110:30:90,"Dimensions","'>MS Sans Serif'(8)"
.        create  Options;OptionsStatW=90:110:100:140,"Width","'>MS Sans Serif'(8)"
.        create  Options;OptionsEditW=90:110:140:180,SelectAll=1,Style=1,Border=1,MaxChars=5,EditType=3,tabid=50
.        create  Options;OptionsStatH=90:110:190:230,"Height","'>MS Sans Serif'(8)"
.        create  Options;OptionsEditH=90:110:230:270,SelectAll=1,Style=1,Border=1,MaxChars=5,EditType=3,tabid=60
.        create  Options;OptionsStatUnits=110:130:30:90,"Units:","'>MS Sans Serif'(8)"
.        create  Options;OptionsUnitsComboBox=110:130:100:270,"":
.        ";)Inches;)Centimeters;)Picas;)Points;)Twips;)Pixels",tabid=70
.        create  Options;OptionsStatScale=130:150:30:90,"Scale:","'>MS Sans Serif'(8)"
.        create  Options;OptionsEditScale=130:150:100:140,SelectAll=1,Style=1,Border=1,MaxChars=3,EditType=2,tabid=80
.        create  Options;OptionsStatThresh=150:170:30:90,"Threshold","'>MS Sans Serif'(8)"
.        create  Options;OptionsEditThresh=150:170:100:140,SelectAll=1,Style=1,Border=1,MaxChars=3,EditType=2,tabid=90
.        activate OptionsGroupBox
.        activate OptionsScannerCheckBox,SamplesSetScanner,result
.        listins OptionsCollection,OptionsStatRes,OptionsStatX,OptionsEditX,OptionsStatY:
.                OptionsEditY,OptionsStatType,OptionsTypeComboBox,OptionsStatDim,OptionsStatW:
.                OptionsEditW,OptionsStatH,OptionsEditH,OptionsStatUnits,OptionsUnitsComboBox:
.                OptionsStatScale,OptionsEditScale,OptionsStatThresh,OptionsEditThresh
.        activate OptionsCollection
.START PATCH 1.7 ADDED LOGIC
.         create    Options;OptionsDCXCheckBox=30:50:80:320,"Save Files in DCX format",tabid=10
          create    Options;OptionsDCXCheckBox=30:50:80:320,"Use Scanner instead of Copier",tabid=10
.         create    Options;OptionsDefaultPathStat=50:70:10:70,"Default Path:","'>MS Sans Serif'(8)"
.         create    Options;OptionsDefaultPath=50:70:80:320,SelectAll=1,Style=1,Border=1,tabid=20
          activate OptionsDCXCheckBox
.         activate OptionsDefaultPathStat
.         activate OptionsDefaultPath
.Open Preferences File
openpref
          pack      APIFileName,"c:\program files\nincal\nsmp0001.pre",hexzero
          call      FindFirstFile
          if (APIResult <> 0 & APIResult <> hexeight)
.                    trap      Preferror if IO
                    open      preffile,"c:\program files\nincal\nsmp0001.pre"
                    for N9,"1","2"
                              if (N9 = 1)
                                        read      preffile,seq;str1
                                        if over
                                                  break
                                        endif
                                        move      C0,N1
                                        move      str1,N1
                                        setitem   OptionsDCXCheckBox,0,N1
.                             elseif (N9 = 2)
.                                       read      preffile,seq;taskname
.                                       if over
.                                                 break
.                                       endif
.                                       setitem   OptionsDefaultPath,0,taskname
                              endif
                    repeat
                    close     preffile
.                   trapclr io
          endif
.Set up SampleListView columns
          SamplesListView.InsertColumn using "Number",50,0
          SamplesListView.InsertColumn using "Sample Name",280,1
          SamplesListView.InsertColumn using "Other Detail",0,2
          SamplesListView.InsertColumnFgClr using *Index=3
.Refresh Lower Screen to lighten up the grey color
          call      SampleDisableLower
.END PATCH 1.7 ADDED LOGIC
.START PATCH 1.4 ADDED LOGIC
.Load OrderInfo
        call    SampleLoadForm
.END PATCH 1.4 ADDED LOGIC

.Main Loop
        move    "M",progcode
        move    "N",PassFlag
.START PATCH 1.7 ADDED LOGIC
          call      GetWinVer
.END PATCH 1.7 ADDED LOGIC
.
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM,DD
.Get User Info
.START PATCH 1.1 - ADDED LOGIC
        move    C0,N3
        move    PORTN,N3
        if (N3 <  C1)
                clock   PORT,str3
                unpack  str3,str2,str1
                pack    str3,str1,str2
                move    str3,PORTN
        endif
.END PATCH 1.1 - ADDED LOGIC
        move    C0,NUSEFLD
        move    C1,NUSEPATH
        move    PORTN,NUSEFLD
        rep     zfill,NUSEFLD
        call    NUSEKEY
.START PATCH 2.0 ADDED LOGIC
.I am not going to use this new logic right now.  Try having everyone use the settings for the front desk
.         call      Trim using NUSEUSER
.         scan      "BILLING",NUSEUSER
.         if not equal
.                   move      NUSEUSER,str1
.                   loop
.                             bump      NUSEUSER,1
.                             cmatch    B1,NUSEUSER
.                             until equal
.                             until eos
.                   repeat
.                   if not eos
.                             bump      NUSEUSER,1
.                             move      NUSEUSER,str6
.                             clear     userlogn
.                             pack      userlogn,str1,str6
.                   endif
.         endif
.END PATCH 2.0 ADDED LOGIC
.Load Outside Library Files
.        pack    APILibFileName,"l:\library\patches\scanner\eztw32.dll",hexzero
.        pack    APILibFileName,"g:\data\samples\eztw32.dll",hexzero
        pack    APILibFileName,"\\nins1\e\data\samples\eztw32.dll",hexzero
.        pack    APILibFileName,"c:\work\eztw32.dll",hexzero
        call    LoadLibrary
        if (APIResult = C0 OR APIResult = hexeight)
                alert   caution,"EZTW32.DLL has not been loaded!!!",result
        endif
.        pack    APILibFileName,"l:\library\patches\scanner\acnc.dll",hexzero
.        pack    APILibFileName,"g:\data\samples\acnc.dll",hexzero
        pack    APILibFileName,"\\nins1\e\data\samples\acnc.dll",hexzero
.        pack    APILibFileName,"c:\work\acnc.dll",hexzero
.drew test - Get rid of call as we are not using this .DLL
.        call    LoadLibrary
.        if (APIResult = C0 OR APIResult = hexeight)
.                alert   caution,"acnc.dll has not been loaded!!!",result
.        endif
.Set Error Message Stat Text Boxes
        call    SetSampleErrorMssgDefault

.START PATCH 1.9 REPLACED LOGIC
.;Set Flags to Open NINMLR.DAT
.        move    C0,NMLRFLAG
.        move    C0,NMLRFLG2
.Set Flags to Open COMPANY.DAT
        move    C0,COMPFLAG
.END PATCH 1.9 REPLACED LOGIC
        setfocus SamplesEditMailer
          setmode   *GRAYSCALE=0

           EVENTREG  X, 17, XRESIZE

        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat

...............................................................................
.   Twain - Acquire image to File name
...............................................................................
.   Notes    :
.
.              PLB Variable      PLB type Var.  Notes.
.              ===============   ============== ===============================
.   On entry : APIWindowHandle    - Int    4       Window handle of window to receive dialog box
.              APIFileName        - Dim    260     Path and file name to receive image(Zero term)
.              APIResult          - Int    4       Don't care
.   On exit  : APIWindowHandle    - Int    4       Unchanged
.              APIFileName        - Dim    260     Unchanged
.              APIResult          - Int    4       1 = Image acquired to File
.                                               0 = Image load failed

TwainAcquireToDCXProfile Profile        acnc.dll:               Dll library
                                        AcquireDCX:             Entry Point
                                        NONE:                   VOID
                                        Dim                     File name


TwainAcquireDCX  WinApi TwainAcquireToDCXProfile Using APIFileName

.                        Call            GetLastError              Load the last error value
                        RETURN
............................
TwainAcquireToDCXExProfile Profile      acnc.dll:               Dll library
                                        AcquireDCXExF:           Entry Point
                                        NONE:                   VOID
                                        Dim:                    File name
                                        Int4:                   Display Interface?
                                        Int4:                   Units
                                        Int4:                   X DPI
                                        Int4:                   Y DPI
                                        Int4:                   Format
                                        Dim                     Record

TwainAcquireDCXEx WinApi TwainAcquireToDCXExProfile Using APIFileName,TwainDisplay,TwainUnits,TwainXDPI,TwainYDPI,TwainFormat,TwainRecDim

.                                        TwainXDPI,TwainYDPI,TwainFormat,TwainSize,TwainDepth,TwainThreshold
.                                        TwainXDPI,TwainYDPI,TwainFormat,TwainThreshold


.                        Call            GetLastError              Load the last error value
                        RETURN
....................................
TwainBmpToDcxProfile Profile            acnc.dll:               Dll library
                                        BmpToDcx:               Entry Point
                                        NONE:                   VOID
                                        Dim:                    BMP File name
                                        Dim                     DCX File name

TwainBmpToDcx WinApi TwainBmpToDcxProfile Using APIFileNameA,APIFileNameA2
.                        Call            GetLastError              Load the last error value
                        RETURN

Timeout
        beep
        beep
        beep
        stop

SamplesSetScanner
        if (result = 1) .Box is checked
                setitem OptionsScannerCheckBox,0,0
                move    C0,result
        else
                setitem OptionsScannerCheckBox,0,1
                move    C1,result
        endif
SamplesSetScanner2
        if (result = 1) .Box is checked
                setprop OptionsCollection,enabled=0
        else
                setprop OptionsCollection,enabled=1
        endif
        return

SetSampleErrorMssgDefault
.Set Default for Sample File Maintenance
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

SetSampleReportScreen
.Set Default for Report Screen
        setitem ReportStatText1,0,"From Mailer :"
        setitem ReportStatText2,0,"To Mailer   :"
        setprop ReportStatText3,width=200
        setitem ReportStatText3,0,"Enter Dates As MMDDYYYY"
        setitem ReportStatText4,0,"From Date   :"
        setitem ReportStatText5,0,"To Date     :"
        setprop ReportEditText1,edittype=3,maxchars=4
        setitem ReportEditText1,0,""
        setprop ReportEditText2,edittype=3,maxchars=4
        setitem ReportEditText2,0,""
        setprop ReportEditText3,visible=0
        setprop ReportEditText4,edittype=3,maxchars=8
        setitem ReportEditText4,0,""
        setprop ReportEditText5,edittype=3,maxchars=8
        setitem ReportEditText5,0,""
        setitem ReportCheck1,0,"Long Listing"
        setitem ReportCheck1,0,0
        setitem ReportCheck2,0,"Exclude Inactives"
        setitem ReportCheck2,0,1
        setprop ReportCheck3,visible=0
        setprop ReportCheck4,visible=0
        setfocus ReportEditText1
        move    NO,RptCan
        return
SampleClearRec
.Clear all Text Fields
.START PATCH 1.7 REPLACED LOGIC
.        deleteitem SamplesComboSamples,0
          SamplesListView.DeleteAllItems giving result
.END PATCH 1.7 REPLACED LOGIC
        setitem SamplesEditEnterDate,0,""
        setitem SamplesEditMailer2,0,""
        setitem SamplesEditSampleDate,0,""
        setitem SamplesStatMlrName,0,""
        setitem SamplesEditUser,0,""
.START PATCH 1.6 ADDED LOGIC
        setitem SamplesCheckInactive,0,0
.END PATCH 1.6 ADDED LOGIC
SampleClearRec2
        setitem SamplesEditSampleNum,0,""
        setitem SamplesEditSampleDesc,0,""
        setitem SamplesEditSampleDesc2,0,""
        setitem SamplesEditPages,0,""
        setitem SamplesStatNum,0,""
        return

FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo3,FileGo3
FileGo1
        RETURN
.        PRTOPEN prfile,"-",WPrognme
.        CLOCK   DATE,DATE
.        unpack  date,mm,str1,dd,str1,yy
.        goto    StartPrint
FileGo2
        RETURN
.        PRTOPEN prfile,"@",WPrognme
.        CLOCK   DATE,DATE
.        unpack  date,mm,str1,dd,str1,yy
.        goto    StartPrint
FileGo3
        if (ExitFlag = "Y")
                winshow
                stop
        endif
        return
Optionsgo
        branch  result to Viewgo
ViewGo
        setprop Options,visible=1
        return
OptionsWritePref
.Pull values from Options.plf
.Screen   1
          getitem   OptionsDCXCheckBox,0,N1
          move      N1,str1
.         getitem   OptionsDefaultPath,0,taskname
.Logic below will work when/if more options are allowed
          prep      preffile,"c:\program files\nincal\nsmp0001.pre"
          write     preffile,seq;str1
.         write     preffile,seq;taskname
.         move      C0,N9
.         loop
.                   add       C1,N9
.                   move      C0,N8
.                   loop
.                             add       C1,N8
.                             write     preffile,seq;OptionsArray(N9,N8)
.                             until     over
.                             until (N8 = OptionsArr2)
.                   repeat
.                   until (N9 = OptionsArr1)
.         repeat
          close     preffile
          return

EditGo
HelpGo
        setprop AboutMssg,visible=1
        return

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
.SHIP-TO
        move    C4,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchLoad
.Called by SearchDataList_DoubleClick
.Only load if not in Inquiry mode
        getprop SamplesEditMailer,enabled=N9
        if (N9 <> C1)
                return
        endif
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4
SearchLoad1
.BROKER - not an option with this program
        return
SearchLoad2
.LIST - not an option with this program
        return
SearchLoad3
.MAILER
.START PATCH 1.9 REPLACED LOGIC
.        unpack  Srchstr,str4,str1,str3,str1,str45
.        setitem SamplesEditMailer,0,str4
        unpack  Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
        setitem SamplesEditMailer,0,str6
.END PATCH 1.9 REPLACED LOGIC
        setitem SamplesStatMlrName,0,str45
        setfocus SamplesEditMailer
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
.
.Disable Upper Screen
SampleDisableUpper
        setprop SamplesEditMailer,enabled=0
        setprop SamplesOK,enabled=0
        setprop SamplesExit,enabled=0
        setprop SamplesNew,enabled=0
        setprop SamplesEditMailer,enabled=0,bgcolor=grey
          setprop SamplesListView,enabled=0,bgcolor=grey
        return

.Enable Upper Screen
SampleEnableUpper
.Allow Exit
        move    "Y",ExitFlag
        setprop SamplesEditMailer,enabled=1
        setprop SamplesOK,enabled=1
        setprop SamplesExit,enabled=1
        setprop SamplesNew,enabled=1
        setprop SamplesEditMailer,enabled=1,bgcolor=white
          setprop SamplesListView,enabled=1,bgcolor=white
        return

.Disable Lower Screen
SampleDisableLower
.START PATCH 1.7 REPLACED LOGIC
.        setprop SamplesComboSamples,enabled=0,bgcolor=grey
.END PATCH 1.7 REPLACED LOGIC
        setprop SamplesEditEnterDate,enabled=0,bgcolor=grey
        setprop SamplesEditMailer2,enabled=0,bgcolor=grey
        setprop SamplesEditSampleDate,enabled=0,bgcolor=grey
        setprop SamplesEditSampleDesc,enabled=0,bgcolor=grey
        setprop SamplesEditSampleDesc2,enabled=0,bgcolor=grey
        setprop SamplesEditSampleNum,enabled=0,bgcolor=grey
        setprop SamplesEditUser,enabled=0,bgcolor=grey
        setprop SamplesEditPages,enabled=0,bgcolor=grey
.START PATCH 1.6 ADDED LOGIC
          setprop SamplesCheckInactive,enabled=0
.END PATCH 1.6 ADDED LOGIC
        setprop SamplesQuit,enabled=0
        setprop SamplesSave,enabled=0
        setprop SamplesView,enabled=0
        setprop SamplesDelete,visible=0
        return

.Enable Lower Screen
SampleEnableLower
        move    "N",UpdateFlag
        move    "N",ExitFlag
.START PATCH 1.7 REPLACED LOGIC
.        setprop SamplesComboSamples,enabled=1,bgcolor=white
.END PATCH 1.7 REPLACED LOGIC
        setprop SamplesEditEnterDate,bgcolor=white
        setprop SamplesEditMailer2,bgcolor=white
        setprop SamplesEditSampleDate,enabled=1,bgcolor=white
        setprop SamplesEditSampleDesc,enabled=1,bgcolor=white
        setprop SamplesEditSampleDesc2,enabled=1,bgcolor=white
        setprop SamplesEditSampleNum,bgcolor=white
        setprop SamplesEditUser,enabled=1,bgcolor=white
        setprop SamplesEditPages,enabled=1,bgcolor=white
.START PATCH 1.6 ADDED LOGIC
          setprop SamplesCheckInactive,enabled=1
.END PATCH 1.6 ADDED LOGIC
        setprop SamplesSave,enabled=1
        setprop SamplesQuit,enabled=1
        return

.Disable Upper Screen
SampleDisableView
        setprop SamplesView,enabled=0
        setprop SamplesPrint,enabled=0
        setprop SamplesBack,enabled=0
        setprop SamplesNext,enabled=0
        setprop SamplesZoomIn,enabled=0
        setprop SamplesZoomOut,enabled=0
        return

SampleLoadList
.Enable Modify button
        setprop SamplesModify,enabled=1
.START PATCH 1.9 REPLACED LOGIC
.        setitem SamplesStatMlrName,0,MCOMP
        setitem SamplesStatMlrName,0,COMPCOMP
.END PATCH 1.9 REPLACED LOGIC
.START PATCH 1.7 REPLACED LOGIC
.        deleteitem SamplesComboSamples,0  .Prepare to refresh it
.        clear   NSMPFLD
.        clear   str3
.        move    C0,N3
.        loop
.                add     C1,N3
.                move    N3,str3
.                rep     zfill,str3
.                pack    NSMPFLD,str4,str3
.                move    "S.LoadSamples-NSMPKEY",Location
.                pack    KeyLocation,"Key: ",NSMPFLD
.                call    NSMPKEY
.                if not over
.                        pack    str35,str3,B1,NSMPDES1
.                        insertitem SamplesComboSamples,N3,str35
.                endif
.                until (N3 > 100) .THIS WILL NEED TO BE INCREASED IF MORE THAN 100 SAMPLES EXIST!!
.        repeat
.        setitem SamplesComboSamples,0,1
.        getitem SamplesComboSamples,1,str3
.        if (str3 <> "." AND str3 <> "")
.                setprop SamplesComboSamples,enabled=1,bgcolor=white
.        else
.                setprop SamplesComboSamples,enabled=0,bgcolor=grey
.        endif
..................................................................................................
          SamplesListView.DeleteAllItems giving result      .Prepare to refresh it
.START PATCH 1.9 REPLACED LOGIC
.         pack      NSMPFLD1,"01X",str4
          pack      NSMPFLD1,"01X",str6
.END PATCH 1.9 REPLACED LOGIC
          move    "S.LoadSamples-NSMPAIM",Location
          pack    KeyLocation,"Key: ",NSMPFLD1
          call    NSMPAIM
        loop
                until over
                    pack      taskname,NSMPVARS
                    SamplesListView.InsertItem giving N9 using NSMPNUM
                    SamplesListView.SetItemText using N9,NSMPDES1,1
                    SamplesListView.SetItemText using N9,taskname,2
                    if (NSMPINACTIVE = "1")
                              move      "0xFF0000",colordim           .Red
                    else
                              move      "0x000000",colordim           .Black
                    endif
                    SamplesListView.SetItemText using N9,colordim,3
                    move    "S.LoadSamples-NSMPKG",Location
                    call    NSMPKG
        repeat
          SamplesListView.EnsureVisible using 0,0
          SamplesListView.SetItemState giving N9 using 0,2,2
          call      Click_SamplesListView
.END PATCH 1.7 REPLACED LOGIC
SampleLoadList2
.Enable View button
        setprop SamplesView,enabled=1
.
.START PATCH 1.7 REMOVED LOGIC
.        pack    NSMPFLD,MNUM,str3
.        move    "S.LoadSamples-NSMPKEY",Location
.        pack    KeyLocation,"Key: ",NSMPFLD
.        call    NSMPKEY
.        if over .This should never happen
.        endif
.END PATCH 1.7 REMOVED LOGIC
        clear   newdate1
        count   N2,NSMPDATE
        if (N2 = "8")
                unpack  NSMPDATE,CC,YY,MM,DD
                pack    newdate1,MM,slash,DD,slash,CC,YY
        endif
        setitem SamplesEditEnterDate,0,newdate1
        setitem SamplesEditMailer2,0,NSMPMLR
        clear   newdate1
        count   N2,NSMPDTE
        if (N2 = "8")
                unpack  NSMPDTE,CC,YY,MM,DD
                pack    newdate1,MM,slash,DD,slash,CC,YY
        endif
        setitem SamplesEditSampleDate,0,newdate1
        setitem SamplesEditSampleDesc,0,NSMPDES1
        setitem SamplesEditSampleDesc2,0,NSMPDES2
        setitem SamplesEditSampleNum,0,NSMPNUM
        setitem SamplesEditUser,0,NSMPUSER
.START PATCH 1.6 ADDED LOGIC
          move      C0,N1
          move      NSMPINACTIVE,N1
        setitem SamplesCheckInactive,0,N1
.END PATCH 1.6 ADDED LOGIC
        setitem SamplesEditPages,0,""
        setitem SamplesStatNum,0,""
        return

.Verify Data Entry
SampleVerifyData
        getitem SamplesEditMailer2,0,NSMPMLR    .ZFILL'ed at LostFocus_Event
        call    Trim Using NSMPMLR
.START PATCH 1.9 REPLACED LOGIC
.        pack    MKEY,NSMPMLR,"000"
.        move    "S.VerifyData-NMLRTST",Location
.        pack    KeyLocation,"Key: ",MKEY
.        move    C1,NMLRPATH
.        call    NMLRTST
.        if over
.                alert   caution,"Valid Mailer Required!",result
.                setfocus SamplesEditMailer2
.                move    YES,ReturnFlag
.                return
.        endif
......................................
        pack    COMPFLD,NSMPMLR
        move    "S.VerifyData-COMPKEY",Location
        pack    KeyLocation,"Key: ",COMPFLD
        move    C1,COMPPATH
        call    COMPKEY
        if over
                alert   caution,"Valid Mailer Required!",result
                setfocus SamplesEditMailer2
                move    YES,ReturnFlag
                return
          elseif (COMPMLRFLG <> "T")
                alert   caution,"Valid Mailer Required!",result
                setfocus SamplesEditMailer2
                move    YES,ReturnFlag
                return
        endif
.END PATCH 1.9 REPLACED LOGIC
        getitem SamplesEditSampleNum,0,NSMPNUM  .ZFILL'ed at LostFocus_Event
        call    Trim using NSMPNUM
        if (NSMPNUM = "")
                alert   caution,"Valid Sample Number Required!",result
                setfocus SamplesEditSampleNum
                move    YES,ReturnFlag
                return
        endif
        pack    NSMPFLD,NSMPMLR,NSMPNUM
        move    "S.VerifyData-NSMPTST",Location
        pack    KeyLocation,"Key: ",NSMPFLD
        call    NSMPTST
        if over
                if (NewFlag = "M")       .This should not happen!!
                        clear   taskname
                        append  "Modification Not Allowed!!",taskname
                        append  carr,taskname
                        append  "Record does not Exist!!",taskname
                        reset   taskname
                        alert   caution,taskname,result
                        setfocus SamplesEditMailer2
                        move    YES,ReturnFlag
                        return
                endif
        else
                if (NewFlag = YES)
                        clear   taskname
                        append  "Record Already Exists!!",taskname
                        append  carr,taskname
                        append  "Mailer: ",taskname
                        append  NSMPMLR,taskname
                        append  "  Sample: ",taskname
                        append  NSMPNUM,taskname
                        append  carr,taskname
                        append  "If you wish to re-use this KEY,",taskname
                        append  carr,taskname
                        append  "Delete existing record first.",taskname
                        reset   taskname
                        alert   caution,taskname,result
                        setfocus SamplesEditMailer2
                        move    YES,ReturnFlag
                        return
                endif
        endif
        getitem SamplesEditSampleDesc,0,NSMPDES1
        call    Trim using NSMPDES1
        if (NSMPDES1 = "")
                alert   caution,"Valid Description 1 Required!",result
                setfocus SamplesEditSampleDesc
                move    YES,ReturnFlag
                return
        endif
        getitem SamplesEditSampleDesc2,0,NSMPDES2
        call    Trim using NSMPDES2
        getitem SamplesEditEnterDate,0,str10
        call    TRIM using str10
        count   N2,str10
        if (N2 = 0)     .This should never happen!!
                alert   caution,"Valid Enter Date Required!",result
                setfocus SamplesEditEnterDate
                move    YES,ReturnFlag
                return
        else
                if (N2 = 10)
                        unpack  str10,MM,str1,DD,str1,CC,YY
                elseif (N2 = 8)
                        unpack  str10,MM,DD,CC,YY
                elseif (N2 <> 0)
                        alert   caution,"Enter Date Must be in MMDDCCYY Format!",result
                        setfocus SamplesEditEnterDate
                        move    YES,ReturnFlag
                        return
                endif
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
                setfocus SamplesEditEnterDate
                move    YES,ReturnFlag
                return
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
                        setfocus SamplesEditEnterDate
                        move    YES,ReturnFlag
                        return
                else
                        move    CC,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
                                setfocus SamplesEditEnterDate
                                move    YES,ReturnFlag
                                return
                        endif
                endif
        endif
        pack    NSMPDATE,CC,YY,MM,DD
        pack    newdate1,MM,str1,DD,str1,CC,YY
        setitem SamplesEditEnterDate,0,newdate1
.
        getitem SamplesEditSampleDate,0,str10
        call    TRIM using str10
        count   N2,str10
        if (N2 = 0)     .This should never happen!!
                alert   caution,"Valid Sample Date Required!",result
                setfocus SamplesEditSampleDate
                move    YES,ReturnFlag
                return
        else
                if (N2 = 10)
                        unpack  str10,MM,str1,DD,str1,CC,YY
                elseif (N2 = 8)
                        unpack  str10,MM,DD,CC,YY
                elseif (N2 <> 0)
                        alert   caution,"Enter Date Must be in MMDDCCYY Format!",result
                        setfocus SamplesEditSampleDate
                        move    YES,ReturnFlag
                        return
                endif
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
                setfocus SamplesEditSampleDate
                move    YES,ReturnFlag
                return
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
                        setfocus SamplesEditSampleDate
                        move    YES,ReturnFlag
                        return
                else
                        move    CC,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
                                setfocus SamplesEditSampleDate
                                move    YES,ReturnFlag
                                return
                        endif
                endif
        endif
        pack    NSMPDTE,CC,YY,MM,DD
        pack    newdate1,MM,str1,DD,str1,CC,YY
        setitem SamplesEditSampleDate,0,newdate1
.
        getitem SamplesEditUser,0,NSMPUSER
        call    Trim using NSMPUSER
        if (NSMPUSER = "")
                alert   caution,"Valid User Name Required!",result
                setfocus SamplesEditUser
                move    YES,ReturnFlag
                return
        endif
.
.START PATCH 1.6 ADDED LOGIC
          getitem SamplesCheckInactive,0,N1
          if (N1 = C1)
                    move      C1,NSMPINACTIVE
          else
                    clear     NSMPINACTIVE
          endif
.END PATCH 1.6 ADDED LOGIC
        return

SampleSearchForMailer
.RefreshSampleIndexSearchList
.Disable Modify button
        setprop SamplesModify,enabled=0
.START PATCH 1.9 REPLACED LOGIC
.        getitem SamplesEditMailer,0,str4
.        count   HowMany,str4
.        if (HowMany > "4")
.                setprop ErrorMssg,visible=1
.                setprop SamplesModify,enabled=0
.                setfocus SamplesEditMailer
.                call    SampleClearRec
.                return
.        elseif (HowMany < "4")
.                sub     HowMany from "4" giving N1
.                setlptr filler2, N1
.                pack    MKEY,filler2,str4
.                move    MKEY,str4
.        endif
.        pack    MKEY,str4,"000"
.        move    C3,NMLRLOCK
.        move    "S.Search4Mlr-1rst NMLRKEY",Location
.        pack    KeyLocation,"Key: ",MKEY
.        call    NMLRKEY
.        if Over
.;Change StatText Boxes For Error Message
.                setprop ErrorMssgStat1,visible=0
.                setprop ErrorMssgStat2,visible=0
.                setprop ErrorMssgStat3,visible=0
.                setprop ErrorMssgStat4,visible=0
.                setprop ErrorMssgStat5,visible=1
.;Display Error Message
.                setprop ErrorMssg,visible=1
.;Reset StatText Boxes
.                call    SetSampleErrorMssgDefault
.                setfocus SamplesEditMailer
.                call    SampleClearRec
.        else
.                call    SampleLoadList
.        endif
...............................................
        getitem SamplesEditMailer,0,str6
        count   HowMany,str6
        if (HowMany > "6")
                setprop ErrorMssg,visible=1
                setprop SamplesModify,enabled=0
                setfocus SamplesEditMailer
                call    SampleClearRec
                return
        elseif (HowMany < "6")
                sub     HowMany from "6" giving N1
                setlptr filler2, N1
                pack    COMPFLD,filler2,str6
                move    COMPFLD,str6
        endif
        pack    COMPFLD,str6
        move    C3,COMPLOCK
        move    "S.Search4Mlr-1rst COMPKEY",Location
        pack    KeyLocation,"Key: ",COMPFLD
        call    COMPKEY
        if Over
.Change StatText Boxes For Error Message
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
.Display Error Message
                setprop ErrorMssg,visible=1
.Reset StatText Boxes
                call    SetSampleErrorMssgDefault
                setfocus SamplesEditMailer
                call    SampleClearRec
        elseif (COMPMLRFLG <> "T")
.Change StatText Boxes For Error Message
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
.Display Error Message
                setprop ErrorMssg,visible=1
.Reset StatText Boxes
                call    SetSampleErrorMssgDefault
                setfocus SamplesEditMailer
                call    SampleClearRec
          else
                call    SampleLoadList
        endif
.END PATCH 1.9 REPLACED LOGIC
        return

SampleScanSample
.START PATCH 1.7 ADDED LOGIC
          getitem   OptionsDCXCheckBox,0,result
          if (result = 0)               .Scanned in TIFF format using Copier/Scanner
SampleAcquireTiff
.                   getitem   OptionsDefaultPath,0,taskname
.                   move      "c:\work\Copier Scans\",taskname
.START PATCH 2.1 REPLACED LOGIC
.                   move      "\\nins1\d\Users\SCANNE\SCANNER",taskname
.I am not going to use this new logic right now.  Try having everyone use the settings for the front desk
.>Code Modified 2.2.2
.                   move      "\\nins1\d\Users\SCANNE\SCANNER",taskname
                    move      "\\nins1\d\Everyone\SCANNER",taskname
.                    move      "I:\Everyone\SCANNER",taskname

.>Code Modified 2.2.2
.                   if (PORTN = 241)
.                             move      "\\nins1\d\Users\SCANNE\SCANNER",taskname
.                   else
.                             pack      taskname,"\\nins1\d\Users\",userlogn,"\SCANNER"
.                   endif
.END PATCH 2.1 REPLACED LOGIC
.begin patch
                    call      debug
                    move      "",str45
                    getfname open,"Open TIFF File",str45,taskname,"tif"
.                    getfname open,"Open TIFF File",str45,"tif"
                    pack      FileName1,taskname,"\teachers.tif"
                    pack      FileName1,taskname,str45
.START PATCH 2.2 ADDED LOGIC
                    if (str45 = "")     .User hit 'Cancel'
.Simulate a Quit
                              noreturn
                              goto Click_SamplesQuit
                    endif
.END PATCH 2.2 ADDED LOGIC
......................................................................................
.Force a view of the TIFF file to ensure this is the one they want
......................................................................................
.This is all a version of the code under SamplesView button!!!!!
.Disable buttons
                    clear     SmpPage             .Holds number of pages
                    clear     SmpPage2  .Holds value of currently displayed page
                    setprop   SamplesView,enabled=0
                    setprop SamplesBack,enabled=0 .will remain disabled until next page hit or you break out of this
                    setprop SamplesNext,enabled=0
                    setprop SamplesZoomIn,enabled=0
                    setprop SamplesZoomOut,enabled=0
.begin patch
          call      debug
.                    FindFIle  FileName1
.                              if        Not Zero                       .its Not there

                  pack    apifilename,FileName1,HexZero
                  call    FindFirstFile
                    if (APIResult = 0 | APIResult = hexeight)
.end patch
                              pack      taskname from "Scanned Sample Not Found! ",filename1
.                              alert     caution,"Scanned Sample Not Found!",result
                              alert     caution,taskname,result
                              move      C0,TIFFBreak
                              setprop   SamplesProceed,enabled=0,height=0
                              goto Click_SamplesQuit
                    endif
.Load Actual Sample
.Next and Previous buttons use taskname, so move over FileName1 so that they work
                    move      FileName1,taskname
.
                  clear   N9
                    move      "35",SmpScale                           .Initialize value
                    trap      SamplesFinishOK2 giving error if object
                  CREATE  Nsmp0001;SamplesPicture=200:475:1:641:
                          taskname,BORDER,SCROLLBAR=1,AUTOZOOM=0,SCALE=SmpScale
                    Activate SamplesPicture
                    setprop SamplesPicture,visible=1        .Really only needed for first time
                    SamplesPicture.GetPageCount GIVING SmpPage
                    move      C1,SmpPage2
.Allow SamplesNext, SamplesZoomIn, SamplesZoomOut only if there is more than one page
.SamplesBack disabled until you hit SamplesNext
                    if (SmpPage > C1)
                              setprop SamplesNext,enabled=1
                    endif
                    setprop SamplesZoomIn,enabled=1
                    setprop SamplesZoomOut,enabled=1
                    trapclr   object
                    call      SampleLoadPage
                    setprop   SamplesProceed,enabled=1,height=20
                    move      C1,TIFFBreak
                    loop
                              checkevent
                              until (TIFFBreak > 1)
                    repeat
.TIFFBreak Values:
. 0 = Cleared
. 1 = Waiting to confirm TIFF image for sample
. 2 = Proceed button hit - TIFF image confirmed
. 3 = Quit button hit - TIFF image denied
.
                    if (TIFFBreak = 3)  .Quit button hit
                              alert     plain,"Did you want to select a different TIFF image?",result
                              if (result = 1)
                                        goto SampleAcquireTiff
                              endif
                              move      C0,TIFFBreak
                              setprop   SamplesProceed,enabled=0,height=0
.START PATCH 2.2 ADDED LOGIC
                              noreturn
.END PATCH 2.2 ADDED LOGIC
                              goto Click_SamplesQuit
                    endif
                    move      C0,TIFFBreak
                    setprop   SamplesProceed,enabled=0,height=0
                    pack      FileName2,dcxpath,"s",NSMPMLR,NSMPNUM,".tif"
                    copyfile FileName1,FileName2
..Offer optional deletion of copied record
.                   pack      taskname,"Do you wish to delete the parent file?:",newline,FileName1
.                   alert     plain,taskname,result
.                   if (result = 1)
                              erase     FileName1
.                   endif
.Set Attributes
                    clear     taskname
.                   call      GetWinVer
                    if (osflag = c1 | osflag = c5)  NT or 2000
                              append    "!c:\winnt\system32\cmd.exe /c ",taskname
                    elseif              (osflag = c3 | osflag = c4)   9x
                              append    "!c:\command.com /c ",taskname
                    elseif              (osflag = c6) Xp
                              append    "!c:\windows\system32\cmd.exe /c ",taskname
                    endif
                    append    "attrib ",taskname
                    append    FileName2,taskname
                    append    " +r",taskname
                    reset     taskname
                    execute   taskname
.
                    goto SAMPLESSCANSAMPLEEND
SamplesFinishOK2
                    trapclr   IO
                    noreturn
                    pack      taskname from "Scanned Sample object error! ",crlf,error
.                    alert     caution,"Scanned Sample Not Found!",result
                    alert     caution,taskname,result
                    move      C0,TIFFBreak
                    setprop   SamplesProceed,enabled=0,height=0
                    goto Click_SamplesQuit
          endif
.END PATCH 1.7 ADDED LOGIC
        getitem SamplesEditPages,0,str9
        move    str9,N9
        if (N9 = C0)
                alert   caution,"How many Pages are to be Scanned??",result
                move    YES,ReturnFlag
                setfocus SamplesEditPages
                return
        endif
        call    TwainIsAvailable
        if (APIResult = 0 | APIResult = hexeight)
                alert   caution,"Scanner is not Ready!!",result
                move    YES,ReturnFlag
                setfocus SamplesEditMailer2
                return
        endif
        getprop NSMP0001,HWND=APIWindowHandle
.        call    TwainSelectImageSource
.        call    TwainAcquireToClipboard
        pack    APIFileName,"                                                        "
        GOTO SAMPLESSCANSAMPLEBEGIN
.........................
        clear   APIFileName
        pack    APIFileName,dcxpath,"S",NSMPMLR,NSMPNUM,".DCX",hexzero
        move    C0,TwainDisplay
        getitem OptionsEditX,0,str10
        move    str10,TwainXDPI
        getitem OptionsEditY,0,str10
        move    str10,TwainYDPI
        getitem OptionsTypeComboBox,0,N9
        sub     C1,N9
        move    N9,TwainFormat
        getitem OptionsEditW,0,str10
        move    str10,TwainRecord.Right
        getitem OptionsEditH,0,str10
        move    str10,TwainRecord.Bottom
        getitem OptionsUnitsComboBox,0,N9
        sub     C1,N9
        move    N9,TwainUnits
.        move    C5,TwainUnits
.        move    "300",TwainXDPI
.        move    "300",TwainYDPI
.        move    C0,TwainFormat
.        move    C3,TwainSize
.        move    C0,TwainDepth
.        move    "255",TwainThreshold
        move    "0",TwainRecord.Top
        move    "0",TwainRecord.Left
        move    "1055",TwainRecord.Right
        move    "1055",TwainRecord.Bottom
        move    TwainRecDim,TwainRecord
.        pack    TwainRecDim,"0.00;0.00;8.50;11.00",hexzero
        call    TwainAcquireDCXEx
.        call    TwainAcquireDCX
        GOTO SAMPLESSCANSAMPLEEND
..................
SAMPLESSCANSAMPLEBEGIN
        call    FinalCleanUp
        move    C0,N10
        loop
LoopBegin
                add     C1,N10
                until (N10 > N9)
                eventcheck
                until (ScanBreak = C2)
                move    N10,str10
                call    Trim using str10
                setitem SamplesStatNum,0,str10
                clear   APIFileName
.                append  dcxpath,APIFileName
                append  bmppath,APIFileName
                append  "bmp",APIFileName
                append  str10,APIFileName
                append  ".bmp",APIFileName
                append  hexzero,APIFileName
                reset   APIFileName
                call    TwainAcquireToFilename
.                if (APIResult = 0 | APIResult = hexeight)
.                        goto SamplesScanError
.                endif
.Test to see if File was actually created or Scan was Cancelled
.                pack    APIFileName,dcxpath,"bmp",str10,".bmp",HexZero
                pack    APIFileName,bmppath,"bmp",str10,".bmp",HexZero
                clock   timestamp,timestamp1
                move    timestamp1,time1
                loop
                        call    FindFirstFile
                        if (APIResult = 0 | APIResult = hexeight)
.If Bitmap file does not exist then do not convert to PCX!!!
                                clock   timestamp,timestamp2
                                move    timestamp2,time2
                                sub     time1,time2,time3
                                if (time3 > 2000) ..20 Seconds Maximum
.                                if (time3 > 1500) ..15 Seconds Maximum
.                                        goto    loopbegin
                                move    "TwainAcquireToFilename",taskname
                                        goto SamplesScanError
                                endif
                        endif
                        until   (apiresult <> 0 & APIRESULT <> Hexeight)
                repeat
.START PATCH 1.7 REPLACED LOGIC
.;Convert to PCX
.                clear   taskname
.;begin patch 1.5
..                    call                GetWinVer
.                    If                  (osflag = c1 | osflag = c5)  NT or 2000
.;                Path    Exist,"c:\windows"
.;                if over
.                        append  "!c:\winnt\system32\cmd.exe /c ",taskname
.;                else
.                    Elseif              (osflag = c3 | osflag = c4)   9x
.                        append  "!c:\command.com /c ",taskname
.                    Elseif              (osflag = c6) Xp
.                        append  "!c:\windows\system32\cmd.exe /c ",taskname
.;end patch 1.5
.                endif
.;START PATCH 1.2 REPLACED LOGIC
.;                append  "\\nts\c\netutils\irview\i_view32.exe \\nins1\e\data\samples\bmp",taskname
.                append  "\\nts\c\netutils\irview\i_view32.exe c:\work\bmp",taskname
.;                append  "f:\netutils\irview\i_view32.exe g:\data\samples\bmp",taskname
.;END PATCH 1.2 REPLACED LOGIC
.                append  str10,taskname
.                append  ".bmp",taskname
.                append  " /convert=",taskname
.;                append  dcxpath,taskname
.                append  bmppath,taskname
.                append  "bmp",taskname
.                append  str10,taskname
.                append  ".pcx",taskname
.                reset   taskname
.                execute taskname
.;Test to see if File was actually converted to PCX
.;                pack    APIFileName,dcxpath,"bmp",str10,".pcx",HexZero
.                pack    APIFileName,bmppath,"bmp",str10,".pcx",HexZero
.                clock   timestamp,timestamp1
.                move    timestamp1,time1
.                loop
.                        call    FindFirstFile
.                        if (APIResult = 0 | APIResult = hexeight)
.;If Bitmap file does not exist then do not convert to DCX!!!
.                                clock   timestamp,timestamp2
.                                move    timestamp2,time2
.                                sub     time1,time2,time3
.;                                if (time3 > 1500) ..15 Seconds Maximum
.                                if (time3 > 3000) ..30 Seconds Maximum
.;                                        goto    loopbegin
.                                        goto SamplesScanError
.                                endif
.                        endif
.                        until   (apiresult <> 0 & APIRESULT <> Hexeight)
.
.                repeat
.;START PATCH 1.52 ADDED LOGIC.
.         call      Trim using NUSEINIT
.         if (NUSEINIT <> "FD")
..END PATCH 1.52 ADDED LOGIC
.;The .PCX file is out there, but the OS on the front desk has not really released it, so put in a short pause
.;                call    FindFirstFile
.;                if (APIResult = 0 | APIResult = hexeight)
.;.If Bitmap file does not exist then do not convert to DCX!!!
.;                        goto LoopBegin
.;                endif
.;                clear   str1
.;                pack    taskname,bmppath,"bmp",str10,".pcx"
.                clock   timestamp,timestamp1
.                move    timestamp1,time1
.                loop
.;                        trap    PCXIO if IO
.;                        open    pcxfile,taskname,EXCLUSIVE
.;                        close   pcxfile
.;                        trapclr IO
.;                        move    "1",str1
.;PCXLoop
.;                        if (str1 = "")
.                                clock   timestamp,timestamp2
.                                move    timestamp2,time2
.                                sub     time1,time2,time3
.;START PATCH 1.51 REPLACED LOGIC
.                                until (time3 > 1500)     .Wait for 15 seconds
.;                                until (time3 > 500)     .Wait for 5 seconds
.;END PATCH 1.51 REPLACED LOGIC
.;                                if (time3 > 1000) ..10 Seconds Maximum
.;                                if (time3 > 1500) ..15 Seconds Maximum
.;                                        goto SamplesScanError
.;                                endif
.;                        endif
.;                        until   (str1 = "1")
.                repeat
.;START PATCH 1.52 ADDED LOGIC
.                   endif
.;END PATCH 1.52 ADDED LOGIC
.;Convert to DCX
.                clear   taskname
.;begin patch 1.5
..                    call                GetWinVer
.                    If                  (osflag = c1 | osflag = c5)  NT or 2000
.;                Path    Exist,"c:\windows"
.;                if over
.                        append  "!c:\winnt\system32\cmd.exe /c ",taskname
.;                else
.                    Elseif              (osflag = c3 | osflag = c4)   9x
.                        append  "!c:\command.com /c ",taskname
.                    Elseif              (osflag = c6) Xp
.                        append  "!c:\windows\system32\cmd.exe /c ",taskname
.;end patch 1.5
.                endif
.                append  "\\nts\c\netutils\makedcx \\nins1\e\data\samples\s",taskname
.;                append  "f:\netutils\makedcx g:\data\samples\s",taskname
.                append  NSMPMLR,taskname
.                append  NSMPNUM,taskname
.                append  ".dcx ",taskname
.;                append  dcxpath,taskname
.                append  bmppath,taskname
.                append  "bmp",taskname
.                append  str10,taskname
.                append  ".pcx -V",taskname
.                reset   taskname
.                execute taskname
..................................................................................................
.Convert to TIF
                    clear   taskname
                    if (osflag = c1 | osflag = c5)  NT or 2000
                              append    "!c:\winnt\system32\cmd.exe /c ",taskname
                    elseif (osflag = c3 | osflag = c4)   9x
                              append    "!c:\command.com /c ",taskname
                    elseif (osflag = c6) Xp
                              append    "!c:\windows\system32\cmd.exe /c ",taskname
                    endif
                    append    "c:\informatikreformat\2000refx.exe conv in=c:\work\bmp",taskname
                    append    str10,taskname
                    append    ".bmp; out=\\nins1\e\data\samples\s",taskname
                append  NSMPMLR,taskname
                append  NSMPNUM,taskname
                append  ".tif; format=29; save=1",taskname
                reset   taskname
                execute taskname
.END PATCH 1.7 REPLACED LOGIC
        repeat
.START PATCH 1.1 ADDED LOGIC
.Set Attributes
        clear   taskname
.begin patch 1.5
.                    call                GetWinVer
                    If                  (osflag = c1 | osflag = c5)  NT or 2000
.                Path    Exist,"c:\windows"
.                if over
                        append  "!c:\winnt\system32\cmd.exe /c ",taskname
.                else
                    Elseif              (osflag = c3 | osflag = c4)   9x
                        append  "!c:\command.com /c ",taskname
                    Elseif              (osflag = c6) Xp
                        append  "!c:\windows\system32\cmd.exe /c ",taskname
.end patch 1.5
        endif
        append  "attrib \\nins1\e\data\samples\s",taskname
.        append  "attrib g:\data\samples\s",taskname
        append  NSMPMLR,taskname
        append  NSMPNUM,taskname
        append  ".dcx +r",taskname
        reset   taskname
        execute taskname
. END PATCH 1.1 ADDED LOGIC

FinalCleanUp
.Clean up after myself
        clear   taskname
.begin patch 1.5
.                    call                GetWinVer
                    If                  (osflag = c1 | osflag = c5)  NT or 2000
.                Path    Exist,"c:\windows"
.                if over
                        append  "!c:\winnt\system32\cmd.exe /c ",taskname
.                else
                    Elseif              (osflag = c3 | osflag = c4)   9x
                        append  "!c:\command.com /c ",taskname
                    Elseif              (osflag = c6) Xp
                        append  "!c:\windows\system32\cmd.exe /c ",taskname
.end patch 1.5
        endif
        append  "del ",taskname
.        append  dcxpath,taskname
        append  bmppath,taskname
        append  "bmp*.*",taskname
        reset   taskname
        execute taskname
.
..............
SAMPLESSCANSAMPLEEND
        return

.START PATCH 1.7 ADDED LOGIC
SampleLoadPage
          clear     str45
          move      SmpPage2,str10
          call      Trim using str10
          append    str10,str45
          append    " of ",str45
          move      SmpPage,str10
          call      Trim using str10
          append    str10,str45
          reset     str45
          setitem SamplesStatPage,0,str45
          return
.END PATCH 1.7 ADDED LOGIC

SamplesScanError
          alert     note,taskname,result
        clear   taskname
        append  "Error in Scanning!",taskname
        append  carr,taskname
        append  "Please delete Sample, if necessary, and re-scan.",taskname
        reset   taskname
        alert   caution,taskname,result
        goto FinalCleanUp
.START PATCH 2.2.3  ADDED LOGIC
NSMPExcelDump
          batch "\\nins1\e\apps\plb\code\plbwin.exe \\nins1\e\apps\plb\code\nsmp002a.plc"
          return
.END PATCH 2.2.3    ADDED LOGIC
XRESIZE
           Nsmp0001.Scale
           RETURN


.PCXIO
.        noreturn
.        goto PCXLoop
.START PATCH 1.1 ADDED LOGIC
.START PATCH 1.4 REMOVED LOGIC
.OrderInfoClose
.        setprop OrderInfo,visible=0
.        setprop OrderInfo,winpos=3
.        return
.END PATCH 1.4 REMOVED LOGIC
.END PATCH 1.1 ADDED LOGIC

.Include IO file
.Patch1.8
                              include compio.inc
                              include cntio.inc
.        include nmlrio.inc
.Patch1.8
        include npasio.inc
        include nsmpio.inc
        include nuseio.inc
.Following used only in order to load Search.plf
.Patch1.8
.        include nbrkio.inc
.Patch1.8
        include ndatio.inc
        include nrtnio.inc
        include searchio.inc      .contains logic for search.plf
.START PATCH 1.2 ADDED INCLUDE
        include ncmpio.inc
.END PATCH 1.2 ADDED INCLUDE
          include   nownio.inc
        include comlogic.inc
