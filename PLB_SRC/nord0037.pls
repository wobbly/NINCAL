........................................
. Program:      nord0037.PLS
. Function:     Order Report Program (development)
. Author:       Andrew Harkins
. Orig. Date:   December 2,1998
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include norddd.inc
.patch1.85
                                        include   compdd.inc
                                        include   cntdd.inc
.        include nmlrdd.inc
.        include nbrkdd.inc
.patch1.85
        include nshpdd.inc
        include npnddd.inc
        include nord4dd.inc
        include oslspern.inc

.2014 February revied for new pending status
release   init    "1.90"                DLH    misc
Reldate   Init      "2011 May 18"
.release   init    "1.89"                08Mar2007  DLH      Oslspern.inc expansion
.Reldate   Init      "2007 March 08"
.release  init      "1.88"         DMB  18JUN2005 FM IP CHG
.release  init      "1.87"        ASH   10AUG2004 Work Order 496 - Added logic
.release  init      "1.86"        ASH   09AUG2004 Logo Conversion
.release  init      "1.85"        DMB   26MAY2004 Mailer Conversion
.release init    "1.84"   ASH 17SEP2003 BROKER FILE CONVERSION
.release init    "1.83"   ASH 15JAN2003 ADD OPTION TO ALLOW BREAK ON CONTACT NUMBER FOR BROKER REPORT
.                                                 ADDED MUTUAL EXCLUSIVITY TO CHECKBOXES 2 & 3 ON REPORT.PLF
.                                                 LIGHTENED BACKGROUND COLOR ON ALL DISABLED EDITTEXT BOXES
.release init    "1.82"   DLH 12Jul2002 Use GetWinVer
.release init    "1.81"   ASH 2FEB2001 FIX FOR NINPRINT
.release init    "1.8"   ASH 02OCT2000 NEW SERVER ADDED
.release init    "1.7"   ASH 24mar2000  re-worked logic for HEADER/FOOTER
.release init    "1.6"   ASH 24Nov99  FIX: Force "Pending List Owner Approval" when stat should say "2nd Request","Revised Request"
.release init    "1.5"   ASH 30JUL99  FIX: Force Pending Desc. until Cancellation Desc. decided
.RELEASE INIT    "1.4"   ASH 17JUN99  NINSHP Y2K, File expansion
.RELEASE INIT    "1.3"   ASH 15JUN99  1) ADDED LOGIC FOR Cancelled Pending Report
.                                     2) Added century for Input4
.                                     3) Consolidated code for all reports
.RELEASE INIT    "1.2"   ASH 08JUN99  LOGIC TO TEST FOR OS FOR EXECUTES
.                       REPLACED ALL INSTANCES OF HOLD4 WITH HOLD5 AS HOLD4
.                       IS NOW FOUND IN CONS.INC
.RELEASE INIT    "1.1"   ASH 14JAN99  NINORD Y2K, File expansion
.release init    "1.0"   ASH 02DEC98  DEVELOPMENT RELEASE

.Files to open
prfile  pfile
input   file
.Start Patch #1.1 - increased file size
.input2  afile   fixed=582
.>patch 1.88
.input2  afile   fixed=696,Name="NINPRINT.aam|20.20.30.103:502"
input2  afile   fixed=696,Name="NINPRINT.aam|10.10.30.103:502"
.>patch 1.88
.End Patch #1.1 - increased file size
input3  file
input4  file
holdnum form    9
height  form    9
width   form    9
.Used to keep track of tabs during Updating and Saving
TabNum  form   1
ExitFlag init   "Y"
PrtFlag init   "N"
Fileflag dim    1
RtnFlag init    "N"
AKey1   init    "01X"
AKey2   init    "01F"
filler  init    "0000"
GoodStat init   "xp"
.START PATCH 1.83 ADDED LOGIC
CONTHOLD DIM    45
FrmPtr    form      ^
RecFlag   form      1
RecFlag2 form       1
.END PATCH 1.83 ADDED LOGIC
newdate1 dim    10
newnum  dim     13
area    dim     3
.START PATCH 1.3 - ADDED VAR
RptTitle dim    100
RptTitle2 dim   100
.END PATCH 1.3 - ADDED VAR
.Length of record plus space for B1 and space for "/"
hold    dim     235     .FOR BROKER RECORD - AAM SEARCH
.Start Patch #1.1 - increased var
.NOTE - ORIGINAL SIZE WAS 8 BYTES SHORT - I KEEP THINKING OMLRPON IS ONLY 4 BYTES LONG (ACTUALLY 12)!!!!!
.hold2   dim     201     .FOR PENDING RECORD
hold2   dim     215     .FOR PENDING RECORD  (8 bytes for real OMLRPON size + 4 bytes{2 centuries} + 2 bytes for extra qty)
.Start Patch #1.1 - increased var
.START PATCH 1.4 - REPLACED LOGIC
.hold3   dim     211     .FOR SHIPPED RECORD
hold3   dim     213     .FOR SHIPPED RECORD
.END PATCH 1.4 - REPLACED LOGIC
.START PATCH 1.2 - REPLACED LOGIC
.HOLD4   dim     50      .FOR BROKER SEARCHES
HOLD5   dim     50      .FOR BROKER SEARCHES
.START PATCH 1.2 - REPLACED LOGIC
HoldBrk dim     4
.key holds longest possible Aam
key     dim     45
holdkey dim     4
inkey   dim     4
outkey  dim     4
.Vars used for printing
page    form    9
.
.Start patch #1.1 - Increased var
.EditMask init   "Z,ZZZ,ZZZ"
.EditQuan dim    9
.Quan    form    7
EditMask init   "ZZZ,ZZZ,ZZZ"
EditQuan dim    11
Quan    form    9
.End patch #1.1 - Increased var
.
.START PATCH 1.7 REMOVED VAR
..Vars for header
.pict1   pict
.pict2   pict
.END PATCH 1.7 REMOVED VAR1
.Vars used for Report Screen
RptCan  dim     1
FromBrk dim     4
ToBrk   dim     4
Preview form    1
Default form    1
Select  form    1
FromDate dim    8
ToDate  dim     8
.
date    dim     8
.Vars used to determine MailerStatSalesMssg
sales   dim     15

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
font5   font

.Set Up Menu Bar
mFile   menu
mEdit   menu
mOptions menu
mReports menu
mHelp   menu

.Set Up SubMenu for Options
sView   submenu

.Present Data for Menu Bar
FData   init    "&File;&Print;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&View"
.START PATCH 1.3 - REPLACED LOGIC
.RData   init    "&Reports;&Daily Broker;Daily &Pending"
RData   init    "&Reports;&Daily Broker;Daily &Pending;&Cancelled Pending"
.END PATCH 1.3 - REPLACED LOGIC
HData   init    "&Help;&About"

.Present Data for Colors SubMenu
VData   init    ";Pendin&g Brokers;&Shipping Brokers"

.Set Vars used for About Box
        move    "NORD0037.PLS",Wprognme
        move    "Order Report Program",Wfunction
        move    "Andrew Harkins",Wauthor
        move    release,Wrelease
        move    "September 17, 2003",Wreldate
          move      reldate,WRelDate
formstuff
.Declare forms, Always declare child forms first
brk     plform  Nord037d        .OrderBrokers
rpt     plform  Report
mss1    plform  Error
abt     plform  About
Nord037c plform  Nord037c
Nord037b plform  Nord037b
Nord037a plform  Nord037a
x       plform  Nord0037
        winhide

.Load Forms, Always load parent form first
        formload x
        formload Nord037a,Nord0037
        formload Nord037b,Nord0037
        formload Nord037c,Nord0037
        formload abt
        formload mss1
        formload rpt
        formload brk
        
.Create Menus
        create  Nord0037;mFile,FData
        create  Nord0037;mEdit,EData,mFile
        create  Nord0037;mOptions,OData,mEdit
        create  Nord0037;mReports,RData,mOptions
        create  Nord0037;mHelp,HData,mReports
.Create SubMenu
        create  Nord0037;sView,VData,mOptions,1

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
.Only a SubMenu under this one
        activate mOptions
        activate mReports,ReportGo,result
        activate mHelp,HelpGo,result
        
.Activate SubMenu
        activate sView,ViewGo,result        

.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=*ltgray
        create  RED=*RED
        create  black=*black
        
.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8 
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic
        create  font5,"Arial",size=10

.START PATCH 1.86 ADDED LOGIC
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 1.86 ADDED LOGIC

.Main Loop
.Set Error Message Stat Text Boxes
        call    SetOrderErrorMssgDefault
.Set tab index
        move    C2,TabNum
.Open Files
        trap    IOMssg giving Error if IO
        move    "Driver-NINPRINT,Open",Location
.START PATCH 1.8 REPLACED LOGIC
.        open    input2,"G:\DATA\index\NINPRINT.aam"        
.START PATCH 1.81 REPLACED LOGIC
.        PACK     STR35,NTWKPATH1,"index\NINPRINT.aam"
.        open    input2,str35
.>patch 1.88
.        PACK    taskname,NTWKPATH1,"index\NINPRINT.aam|20.20.30.103:502"
        PACK    taskname,NTWKPATH1,"index\NINPRINT.aam|10.10.30.103:502"
.>patch 1.88        
        open    input2,taskname
.END PATCH 1.81 REPLACED LOGIC
.END PATCH 1.8 REPLACED LOGIC
        trapclr IO
        trap    IOMssg giving Error if IO
        move    "Driver-NINORD4,Open",Location
        open    NORD4FILE,NORD4NAME 
        trapclr IO
.Load ToDate Field
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    str10,MM,SLASH,DD,SLASH,CC,YY
        setitem OrderEditToDate,0,str10
.START PATCH 1.7 REMOVED VAR
..Create Header Logo
.. note update winrev to make sure users have directory and files
.        CREATE  PICT1=1:15:1:10:
.                "c:\program files\nincal\HEADER.BMP",AUTOZOOM
.        CREATE  PICT2=20:35:20:30:
.                "c:\program files\nincal\FOOTER.BMP",AUTOZOOM
.END PATCH 1.7 REMOVED VAR
.START PATCH 1.83 ADDED LOGIC
          eventreg ReportCheck2,4,ClickReportCheck2,RESULT=N9
          eventreg ReportCheck3,4,ClickReportCheck3,RESULT=N9
.END PATCH 1.83 ADDED LOGIC
        setfocus OrderSearchKey
        loop
                waitevent
        repeat
        
SetOrderErrorMssgDefault
.Set Default for OrderFile Maintenance
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=0
        setprop ErrorMssgStat4,visible=0
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0,"To Search By Broker Number:"
        setitem ErrorMssgStat2,0,"Enter 4 Digit Number"
        setitem ErrorMssgStat5,0,"      That Record Does Not Exist!"
        setitem ErrorMssgOK,0,"&OK"
        return

SetOrderDailyBrokerReportScreen
..Set Default for Report Screen
.Visibility changed while testing REports
        setprop Report,title="Daily Broker Report"
        setitem ReportStatText1,0,"Broker ##   :"
        setprop ReportStatText1,visible=1
        setprop ReportStatText2,visible=0
        setprop ReportStatText3,width=200
        setitem ReportStatText3,0,"Enter Dates as MMDDYYYY"     .not used
        setprop ReportStatText3,visible=0
        setitem ReportStatText4,0,"From Date   :"               .not used
        setprop ReportStatText4,visible=0
        setitem ReportStatText5,0,"To Date     :"               .not used
        setprop ReportStatText5,visible=0
        setprop ReportEditText1,edittype=3,maxchars=4,selectall=1
        getitem OrderSearchKey,0,key
        type    key
        if equal
                setitem ReportEditText1,0,key
        else 
                setitem ReportEditText1,0,""
        endif
        setprop ReportEditText2,edittype=3,maxchars=4,visible=0
        setitem ReportEditText2,0,""
        setprop ReportEditText3,visible=0
        setprop ReportEditText4,edittype=3,maxchars=8,visible=0
        setitem ReportEditText4,0,""
        setprop ReportEditText5,edittype=3,maxchars=8,visible=0
        setitem ReportEditText5,0,""
        setitem ReportCheck1,0,"Print Preview"
        setitem ReportCheck1,0,1
        setprop ReportCheck1,visible=1
        setitem ReportCheck2,0,"Default Printer"
        setitem ReportCheck2,0,1
        setprop ReportCheck2,visible=1
        setitem ReportCheck3,0,"Select Printer"
        setitem ReportCheck3,0,0
        setprop ReportCheck3,visible=1
.START PATCH 1.83 REPLACED LOGIC
.        setprop ReportCheck4,visible=0
          setitem ReportCheck4,0,"Break on Contact"
          setitem ReportCheck4,0,0
          setprop ReportCheck4,visible=1
.END PATCH 1.83 REPLACED LOGIC
        setfocus ReportEditText1
        move    NO,RptCan
        return

.START PATCH 1.83 ADDED LOGIC
ClickReportCheck2
          call      ExclusiveCHECKBOX using ReportCheck2,ReportCheck3,ReportCheck3
          return
ClickReportCheck3
          call      ExclusiveCHECKBOX using ReportCheck3,ReportCheck2,ReportCheck2
          return
.END PATCH 1.83 ADDED LOGIC

SetOrderDailyPendingReportScreen
..Set Default for Report Screen
.Visibility changed while testing REports
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    str10,MM,SLASH,DD,SLASH,CC,YY
.START PATCH 1.3 - CODE ROUTINE NOW CALLED BY TWO DIFFERENT ROUTINES
.        setprop Report,title="Daily Pending Report"
        setitem ReportStatText1,0,"From Broker ##   :"
        setprop ReportStatText1,visible=1
        setitem ReportStatText2,0,"To Broker ##     :"
        setprop ReportStatText2,visible=1
        setitem ReportStatText3,0,"Enter Dates as MMDDYYYY"
        setprop ReportStatText3,width=200,visible=1
        setitem ReportStatText4,0,"From Date   :"               
        setprop ReportStatText4,visible=1
        setitem ReportStatText5,0,"To Date     :"
        setprop ReportStatText5,visible=1
        setprop ReportEditText1,edittype=3,maxchars=4,selectall=1
        setitem ReportEditText1,0,""
        setprop ReportEditText2,edittype=3,maxchars=4,selectall=1,visible=1
        setitem ReportEditText2,0,""
        setprop ReportEditText3,visible=0
        setprop ReportEditText4,maxchars=10,visible=1
        setitem ReportEditText4,0,str10
        setprop ReportEditText5,maxchars=10,visible=1
        setitem ReportEditText5,0,str10
        setitem ReportCheck1,0,"Print Preview"
        setitem ReportCheck1,0,1
        setprop ReportCheck1,visible=1
        setitem ReportCheck2,0,"Default Printer"
        setitem ReportCheck2,0,1
        setprop ReportCheck2,visible=1
        setitem ReportCheck3,0,"Select Printer"
        setitem ReportCheck3,0,0
        setprop ReportCheck3,visible=1
        setprop ReportCheck4,visible=0
        setfocus ReportEditText1
        move    NO,RptCan
        return

FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo2
FileGo1
        if (PrtFlag = Yes)
                goto    OrderSingleBrokerReport
        else
                return
        endif
FileGo2        
        if (ExitFlag = "Y")
                winshow
                stop
        endif
        return
EditGo
HelpGo
        setprop AboutMssg,visible=1
        return       

ViewGo
        branch result to PendBrokers,ShipBrokers
        return
PendBrokers
        setitem OrderBrokersStatText,0,"Current Pending Brokers"
.START PATCH 1.8 REPLACED LOGIC
.        open    input3,"g:\data\ninprint.dat",read
.START PATCH 1.81 REPLACED LOGIC
.        PACK     STR35,NTWKPATH1,"NINPRINT.dat"
.        open    input3,str35,read
.>patch 1.88        
.        PACK    taskname,NTWKPATH1,"NINPRINT.dat|20.20.30.103:502"
        PACK    taskname,NTWKPATH1,"NINPRINT.dat|10.10.30.103:502"
.>patch 1.88                
        open    input3,taskname,read
.END PATCH 1.81 REPLACED LOGIC
.END PATCH 1.8 REPLACED LOGIC
        move    "P",FileFlag
        call    BrokerSearch
        return
ShipBrokers
        setitem OrderBrokersStatText,0,"Current Shipped Brokers"
.START PATCH 1.8 REPLACED LOGIC
.        open    input3,"g:\data\shipfax.dat",read
        PACK     STR35,NTWKPATH1,"shipfax.dat"
        open    input3,str35,read
.END PATCH 1.8 REPLACED LOGIC
        move    "S",FileFlag
        call    BrokerSearch
        return
BrokerSearch
        deleteitem OrderBrokersDataList,0
        loop
.START PATCH #1.1 - INCREASED FILE SIZE
.                read    input3,seq;str1,OSTAT,str55,str55,str55,str55,str45,str1,str4
                read    input3,seq;str1,OSTAT,str55,str55,str55:
                str55,str55,str25,str4
.END PATCH #1.1 - INCREASED FILE SIZE
                until over
                if (FileFlag = "P")
.Following scans for records that match with a pending or cancelled status.  To see
.all records in file, simply remove following decision statement
                        scan    OSTAT,GoodStat
                        if equal
                                reset   GoodStat
                                call    OrderLoadBrokers
                        else
                                reset   GoodStat
                        endif
                else    .FileFlag = "S"
                        call    OrderLoadBrokers
                endif
        repeat
        close   input3
........BUBBLE SORT THE DATALIST...................
        getitem OrderBrokersDataList,1,HowMany
        clear   result        
        loop
                add     C1,result
                until (result > HowMany)
                getitem OrderBrokersDataList,result,outkey
                move    result,N9
                move    outkey,holdkey
                move    result,holdnum
                loop
                        add     C1,N9
                        until (N9 > HowMany)
                        getitem OrderBrokersDataList,N9,inkey
                        if (inkey < holdkey)
                                move    inkey,holdkey
                                move    N9,holdnum
                        endif
                repeat
                if (result <> holdnum)
                        getitem OrderBrokersDataList,result,str55
.START PATCH 1.2 - REPLACED LOGIC
.                        getitem OrderBrokersDataList,holdnum,HOLD4
.                        setitem OrderBrokersDataList,result,HOLD4
                        getitem OrderBrokersDataList,holdnum,HOLD5
                        setitem OrderBrokersDataList,result,HOLD5
.END PATCH 1.2 - REPLACED LOGIC
                        setitem OrderBrokersDataList,holdnum,str55
                endif                   
        repeat
..........................
        setitem OrderBrokersDataList,0,1
        setfocus OrderBrokersDataList
        setprop Nord037d,visible=1      .OrderBrokers
        return
OrderLoadBrokers
        call    TRIM using str4
        count   N2,str4
        if (N2 <> 0)
.Following block loops through datalist and checks each entry, testing for the
.possibility of a duplicate.  If no duplicate is found (str1 = NO) then a new item
.is added to the datalist.  This logic is used in OrderPrint_Click Event as well.
                clear   result
                move    YES,str1
                getitem OrderBrokersDataList,1,HowMany
                loop
                        add     C1,result
                        until (result > HowMany)
                        getitem OrderBrokersDataList,result,str45
                        scan    str4,str45
                        if equal
                                move    NO,str1
                        endif                    
                repeat
                if (str1 = YES)        
                        rep     zfill,str4
                        pack    NBRKFLD,str4,"000"
                        move    C3,NBRKLOCK
                        move    "Driver-NBRKKEY,BrokerSearch",Location
                        call    NBRKKEY
                        if over
                                alert   note,"No Broker Record Found!",result
                        else
.START PATCH 1.2 - REPLACED LOGIC
.                                pack    HOLD4,BRKNUM,B1,BRCOMP
.                                insertitem OrderBrokersDataList,9999,HOLD4
                                pack    HOLD5,BRKNUM,B1,BRCOMP
                                insertitem OrderBrokersDataList,9999,HOLD5
.END PATCH 1.2 - REPLACED LOGIC
                        endif
                endif
        endif
        return
ReportGo
.START PATCH 1.3 - REPLACED LOGIC
.        branch  result to ReportGo1,ReportGo2
        branch  result to ReportGo1,ReportGo2,ReportGo3
.END PATCH 1.3 - REPLACED LOGIC
ReportGo1        
        if (PrtFlag = Yes)
                goto    OrderSingleBrokerReport
        else
                return
        endif        
ReportGo2
        goto    OrderDailyListingReport
.START PATCH 1.3 - ADDED LOGIC
ReportGo3
        goto    OrderCancelledPendingReport
.END PATCH 1.3 - ADDED LOGIC

OrderClear
          deleteitem Order2DataList,0
.START PATCH 1.83 ADDED LOGIC
          deleteitem Order2TestDataList,0
.END PATCH 1.83 ADDED LOGIC
        setitem Order2EditBrokerPO,0,""
        setitem Order2EditContact,0,""
        setitem Order2EditDate,0,""
        setitem Order2EditLR,0,""
        setitem Order2EditList,0,""
        setitem Order2EditMailer,0,""
        setitem Order2EditQuantity,0,""
        setitem Order2EditStatus,0,""
        setitem Order2StatRecords,0,""
        deleteitem Order3DataList,0
        setitem Order3EditBrokerPO,0,""
        setitem Order3EditContact,0,""
        setitem Order3EditDate,0,""
        setitem Order3EditLR,0,""
        setitem Order3EditList,0,""
        setitem Order3EditMailer,0,""
        setitem Order3EditQuantity,0,""
        setitem Order3EditMethod,0,""
        setitem Order3EditTracker,0,""
        setitem Order3StatRecords,0,""
        return
        
OrderLoadScreen2
        setitem Order2EditBrokerPO,0,OMLRPON
        setitem Order2EditContact,0,BRCNTCT
        call    TRIM using ORTNDTEM
        count   N2,ORTNDTEM
        if (N2 <> 0 AND ORTNDTEM <> "00")
.Start Patch #1.1 - added century
.                pack    newdate1,ORTNDTEM,SLASH,ORTNDTED,SLASH,CC,ORTNDTEY
                pack    newdate1,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
.END Patch #1.1 - added century
        else
                clear   newdate1
        endif
        setitem Order2EditDate,0,newdate1
        count   N2,OODTEM
        if (N2 <> 0 AND OODTEM <> "00")
.Start Patch #1.1 - added century
.                pack    newdate1,OODTEM,SLASH,OODTED,SLASH,CC,OODTEY
                pack    newdate1,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
.End Patch #1.1 - added century
        else
                clear   newdate1
        endif
        setitem Order2EditOrderDate,0,newdate1
        setitem Order2EditLR,0,OLRN
        setitem Order2EditList,0,O1DES
        setitem Order2EditMailer,0,MCOMP
        move    EditMask,EditQuan
        move    C0,Quan
        move    OQTY,Quan
        edit    Quan,EditQuan
        setitem Order2EditQuantity,0,EditQuan
        setitem Order2EditStatus,0,NPNDDESC
        clear   str25
        pack    str25,N7,b1,"Record(s)"
        setitem Order2StatRecords,0,str25
        setfocus Order2DataList
        return
OrderLoadScreen3        
        setitem Order3EditBrokerPO,0,OMLRPON
        setitem Order3EditContact,0,BRCNTCT
        call    TRIM using SDATE
        count   N2,SDATE
        if (N2 <> 0)
                scan    "0000",SDATE
                if equal
                        clear   newdate1
                else
                        unpack  SDATE,STR2,YY,MM,DD
                        pack    newdate1,MM,SLASH,DD,SLASH,STR2,YY
                endif
        else
                clear   newdate1
        endif
        setitem Order3EditDate,0,newdate1
        setitem Order3EditLR,0,OLRN
        setitem Order3EditList,0,O1DES
        setitem Order3EditMailer,0,MCOMP
        move    EditMask,EditQuan
        move    C0,Quan
        move    SQUANT,Quan
        edit    Quan,EditQuan
        setitem Order3EditQuantity,0,EditQuan
        setitem Order3EditMethod,0,SINFO
        setitem Order3EditTracker,0,STRACK
        clear   str25
        pack    str25,N8,b1,"Record(s)"
        setitem Order3StatRecords,0,str25
        setfocus Order3DataList
        return
OrderLoadScreen4
        if (BRCREDIT = "*")
                setitem Order4ComboCredit,0,2
        elseif (BRCREDIT = "N")
                setitem Order4ComboCredit,0,3
        elseif (BRCREDIT = "I")
                setitem Order4ComboCredit,0,4
        elseif (BRCREDIT = "B")
                setitem Order4ComboCredit,0,5
        else
                setitem Order4ComboCredit,0,1
        endif
        setitem Order4EditAddress,0,BRADDR
        setitem Order4EditAddress2,0,BR2ADDR
        setitem Order4EditCity,0,BRCITY
        setitem Order4EditCode,0,BRKCNT
        setitem Order4EditCompany,0,BRCOMP
        setitem Order4EditContact,0,BRCNTCT
        setitem Order4EditCountry,0,BRCOUN
        call    TRIM using BRREVDAT
        count   N2,BRREVDAT
        if (N2 <> 0)
                unpack  BRREVDAT,STR2,YY,MM,DD
                pack    newdate1,MM,SLASH,DD,SLASH,STR2,YY
        else
                clear   newdate1
        endif
        setitem Order4EditDate,0,newdate1
        count   N2,BRFAX
        if (N2 = 10)
                unpack  BRFAX,area,str3,str4
                pack    Newnum,"(",area,")",str3,Dash,str4
        else
                clear   Newnum
        endif
        setitem Order4EditFax,0,Newnum
        setitem Order4EditNumber,0,BRKNUM
        setitem Order4EditSales,0,BRSALES
        clear   HowMany
        move    BRSALES,HowMany
        move    osls0,sales
        load    sales from HowMany of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
                osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
                osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
              osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
        setitem Order4StatSalesMssg,0,sales
        setitem Order4EditState,0,BRSTATE
        count   N2,BRTELE
        if (N2 = 10)
                unpack  BRTELE,area,str3,str4
                pack    Newnum,"(",area,")",str3,Dash,str4
        else
                clear   Newnum
        endif
        setitem Order4EditTelephone,0,newnum
        setitem Order4EditUser,0,BRNAME
        setitem Order4EditZip,0,BRZIP
        return        

RefreshOrderAamdexSearchList
.Set up for Printing
        move    YES,PrtFlag
.Prep Screen2
        clear   N7
.Hide DataList
        setprop OrderSearchList,visible=0
.Delete Prior Entries
        deleteitem Order2DataList,0
.START PATCH 1.83 ADDED LOGIC
          deleteitem Order2TestDataList,0
.END PATCH 1.83 ADDED LOGIC
        count   HowMany,key
        if (HowMany > "4")
                setprop ErrorMssg,visible=1
                setfocus OrderSearchKey
                return          
        endif
.Pack number with preceding zeroes
        if (HowMany < "4")
                sub HowMany from "4" giving N1
                setlptr filler, N1
                pack NORDFLD4,filler,key
                move NORDFLD4,key
        endif
        setitem OrderSearchKey,0,key
        pack    NORDFLD4,AKey1,key      .FILE ONLY AAMDEXED WITH LAST KEY OF NINORD
        TRAP    IOMssg giving Error if IO
        move    "Driver-NINPRINT,Read",Location
        read    input2,NORDFLD4;ORDVARS
        trapclr IO
        if Over
                goto THIRDSCREEN   
        else
                scan    OSTAT,GoodStat
                if equal
.Filter Date Range
.Start Patch #1.1 - added century
.                        pack    str8,CC,OODTEY,OODTEM,OODTED
                        pack    str8,OODTEC,OODTEY,OODTEM,OODTED
.eND Patch #1.1 - added century
                        if ((str8 >= FromDate) AND (str8 <= ToDate))
                                add     C1,N7
                                call    OrderReadOtherFiles
                                call    OrderReadPendFiles
.Start Patch #1.1 - increased var
.                                pack    hold2,OMLRPON,SLASH,OLRN,B1,BRCNTCT,MCOMP,O1DES,OQTY,ORTNDTEM:
.                                        ORTNDTED,ORTNDTEY,OODTEM,OODTED,OODTEY,NPNDDESC
                                pack    hold2,OMLRPON,SLASH,OLRN,B1,BRCNTCT,MCOMP,O1DES,OQTY,ORTNDTEC:
                                        ORTNDTEY,ORTNDTEM,ORTNDTED,OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC
.End Patch #1.1 - increased var
                                insertitem Order2DataList,9999,hold2
.START PATCH 1.83 ADDED LOGIC
                                        pack      hold2,BRCNTCT,"1",OMLRPON,OLRN,MCOMP,O1DES,OQTY,ORTNDTEC:
                                                  ORTNDTEY,ORTNDTEM,ORTNDTED,OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC
                                        insertitem Order2TestDataList,9999,hold2
.END PATCH 1.83 ADDED LOGIC
.Broker Info will have been pulled with above call to OrderReadOtherFiles, Now Load It!
                        endif
                endif
                reset   GoodStat
        endif
        move    "Driver-NINPRINT,KG",Location
        loop
                trap    IOMssg giving Error if IO
                readkg  input2;ORDVARS
                trapclr IO
                until over
                scan    OSTAT,GoodStat
                if equal
.Filter Date Range
.Start Patch #1.1 - added century
.                        pack    str8,CC,OODTEY,OODTEM,OODTED
                        pack    str8,OODTEC,OODTEY,OODTEM,OODTED
.END Patch #1.1 - added century
                        if ((str8 >= FromDate) AND (str8 <= ToDate))
                                add     C1,N7
                                call    OrderReadOtherFiles
                                call    OrderReadPendFiles
.Start Patch #1.1 - increased var
.                                pack    hold2,OMLRPON,SLASH,OLRN,B1,BRCNTCT,MCOMP,O1DES,OQTY,ORTNDTEM:
.                                        ORTNDTED,ORTNDTEY,OODTEM,OODTED,OODTEY,NPNDDESC
                                pack    hold2,OMLRPON,SLASH,OLRN,B1,BRCNTCT,MCOMP,O1DES,OQTY,ORTNDTEC:
                                        ORTNDTEY,ORTNDTEM,ORTNDTED,OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC
.End Patch #1.1 - increased var
                                insertitem Order2DataList,9999,hold2
.START PATCH 1.83 ADDED LOGIC
                                        pack      hold2,BRCNTCT,"1",OMLRPON,OLRN,MCOMP,O1DES,OQTY,ORTNDTEC:
                                                  ORTNDTEY,ORTNDTEM,ORTNDTED,OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC
                                        insertitem Order2TestDataList,9999,hold2
.END PATCH 1.83 ADDED LOGIC
                              endif
                endif
                reset   GoodStat
        repeat        
THIRDSCREEN
.Prep Screen3
.Delete Prior Entries
        deleteitem Order3DataList,0
        clear   N8
        clear   OLRN
.START PATCH 1.8 REPLACED LOGIC
.        open    input,"g:\data\shipfax.dat",read
        PACK     STR35,NTWKPATH1,"shipfax.dat"
        open    input,str35,read
.END PATCH 1.8 REPLACED LOGIC
        loop
Read            read    input,seq;ordvars
                until over
                if (OBRKNUM = KEY)
                        add     C1,N8
                        match   "      " to olrn
                        goto    read if equal
                        match   "      " to olrn
                        goto    read if eos
                        type    olrn
                        goto    read if not equal
                        CALL    OrderReadShippingFile
                        pack    hold3,OMLRPON,SLASH,OLRN,B1,BRCNTCT,MCOMP,O1DES,SQUANT,SDATE:
                                SINFO,STRACK
                        insertitem Order3DataList,9999,hold3
.START PATCH 1.83 ADDED LOGIC
                              pack    hold3,BRCNTCT,"2",OMLRPON,OLRN,MCOMP,O1DES,SQUANT,SDATE:
                                        SINFO,STRACK
                              insertitem Order2TestDataList,9999,hold3
.END PATCH 1.83 ADDED LOGIC
                endif
        repeat
        close   input       
.Load the Inquiry Screens immediately
        setitem Order2DataList,0,1
        call OrderData2
        setitem Order3DataList,0,1
        call OrderData3
.Fill Screen4
        call    OrderLoadScreen4        .Dependent upon one of above calls to OrderReadOtherFiles!
        return        

OrderReadShippingFile   
.includes OrderReadOtherFiles
.Shipping File
         rep     zfill,OLRN
         pack    NSHPFLD,OLRN
         move    C3,NSHPLOCK
         move    "Driver-NSHPKEY,1rst",Location
         call    NSHPKEY
         if over
                 alert   caution,"Shipping Info Not Found!",result
         endif
OrderReadOtherFiles
.Open other files to retrieve appropriate information            
.Mailer File
         rep     zfill,OMLRNUM
         pack    MKEY,OMLRNUM,"000"      .Master Record
         move    C3,NMLRLOCK
         move    "Driver-NMLRKEY,1rst",Location
         call    NMLRKEY
         if over
                 pack   MCOMP,"UNKNOWN",B55
         endif  
.SAVE THIS INFORMATION IN CASE USERS WANT THE MASTER LIST NAME
.ALL INSTANCES OF O1DES WOULD THEN NEED TO REPLACED WITH MLSTNAME
..DataCard File
.         rep     zfill,OLNUM
.         pack    NDATFLD,OLNUM
.         move    C3,NDATLOCK
.         move    C1,NDATPATH
.         move    "Driver-NDATKEY,1rst",Location
.         call    NDATKEY
.         if over
.                 alert   caution,"DataCard Name Not Found!",result
.         endif 
OrderReadBrokerFile
.Broker File
         rep     zfill,OBRKNUM                
         rep     zfill,OBRKCNT
         pack    NBRKFLD,OBRKNUM,OBRKCNT
         move    C3,NBRKLOCK
         move    "Driver-NBRKKEY,1rst",Location
         call    NBRKKEY
         if over
.                 move   "000",OBRKCNT
.                 pack   NBRKFLD,OBRKNUM,OBRKCNT
.                 call   NBRKKEY     .CALL USING HEAD RECORD TO EXTRACT BRCOMP
.                 if over
                         pack   BRCNTCT,"UNKNOWN",B55
.                 endif
         endif    
         return

OrderReadPendFiles
.NINORD4 File
        move    OLRN,NORD4FLD
        rep     zfill,NORD4FLD
.was using nord4tst ????? need the nord4stat data  .dlh 18dec98
        call    NORD4key
        if over
                alert   caution,"No NINORD4 Record Found!",result
                move    "No Status Found!",NPNDDESC
        else
.NINPND File
.START PATCH 1.6 - ADDED LOGIC
                if (NORD4STAT = "11" | NORD4STAT = "12")
                        move    "00",NORD4STAT
                endif
.END PATCH 1.6 - ADDED LOGIC
                cmatch  "0" to ostat
                if       equal
                        move    "p" to str1
                        pack    NPNDFLD,str1,NORD4STAT
                else
.START PATCH 1.5 - ADDED LOGIC
                        if (OSTAT = "x")
                                move    "p",OSTAT
                        endif
.END PATCH 1.5 - ADDED LOGIC
                        pack    NPNDFLD,OSTAT,NORD4STAT
                endif
                rep     zfill,NPNDFLD
                move    C3,NPNDLOCK
                call    NPNDKEY
                if over
                        alert   caution,"No NINPND Record Found! ",result
                        clear   npnddesc
                        append    "No Status Found!",NPNDDESC
                        append    npndfld,npnddesc
                        reset     npnddesc
                endif
        endif
        return
RefreshOrderSearchList
.Set up for Printing
        move    NO,PrtFlag
        setprop OrderPrint,enabled=0
.Display DataList
        setprop OrderSearchList,visible=1
.Clear other screens
        call    OrderClear        
.First Test for Minimum Entry
        count    result,key
        if (result < 3)
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
                setitem ErrorMssgStat5,0,"  Name Must Have 3+ Characters"
                setprop ErrorMssg,visible=1
                call    SetOrderErrorMssgDefault
                setfocus OrderSearchKey
                setprop OrderOK,enabled=1
                return
        endif
.START PATCH 1.84 REPLACED LOGIC
.        move    C3,NBRKLOCK
.        pack    NBRKFLD2,AKey2,key
.        move    "Driver-NBRKAIM",Location
.        call    NBRKAIM
.        if Over
..Change StatText Boxes For Error Message
.                setprop ErrorMssgStat1,visible=0
.                setprop ErrorMssgStat2,visible=0
.                setprop ErrorMssgStat3,visible=0
.                setprop ErrorMssgStat4,visible=0
.                setprop ErrorMssgStat5,visible=1
..Display Error Message
.                setprop ErrorMssg,visible=1
..Reset StatText Boxes
.                setprop ErrorMssgStat5,visible=0
.                setprop ErrorMssgStat1,visible=1
.                setprop ErrorMssgStat2,visible=1
.                setprop ErrorMssgStat3,visible=1
.                setprop ErrorMssgStat4,visible=1
.
.                setprop OrderOK,enabled=1
.                setfocus OrderSearchKey
.                return
.        else
..                move "Y",AamFlag
.                
.                pack hold,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRADDR,BR2ADDR,BRCITY:
.                     BRSTATE,BRZIP,BRCOUN,BRNAME,BRREVDAT,BRCREDIT,BRBLANK,BRTELE,BRFAX,BRSALES
.                insertitem OrderSearchList,9999,hold
.
.        endif
.        move    "Driver-NBRKKG",Location
.        loop
.                call NBRKKG
.                if not over
.                        pack hold,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRADDR,BR2ADDR,BRCITY:
.                             BRSTATE,BRZIP,BRCOUN,BRNAME,BRREVDAT,BRCREDIT,BRBLANK,BRTELE,BRFAX,BRSALES
.                        insertitem OrderSearchList,9999,hold
.                endif
.                until over
.        repeat
.......................................................
          move      C0,N2
          clear     CNCTFLD2
          if (N1 = C1)
                  pack    CNCTFLD3,"02F",Key
                    pack      COMPFLD2,"01F",Key
          else
                    pack    CNCTFLD3,"02L",Key
                    pack      COMPFLD2,"01L",Key
          endif
          move    "CNCTAIM",Location
        call    CNCTAIM
          if not over
                    call      LoadBrokerInfoA
                    move      "CNCTKG",Location
                  loop
                  call CNCTKG
                              until over
                              call      LoadBrokerInfoA
          repeat
          else
                    move      C1,N2
          endif
.
          move    "COMPAIM",Location
        call    COMPAIM
          if over
                    if (N2 = 1)
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
                              setprop OrderOK,enabled=1
                              setfocus OrderSearchKey
                              return
                    endif
        else
                    clear     CNCTVARS
                    if (COMPBRKFLG = "T" | COMPCLRFLG = "T")
                              pack      CNCTCNT,COMPOLDBRK,"000"
                              pack      cnctfname,B55
                              call      LoadBrokerInfo
                    endif
                    move      "COMPKG",Location
          loop
                          call COMPKG
                  until over
                              if (COMPBRKFLG = "T" | COMPCLRFLG = "T")
                                        pack      CNCTCNT,COMPOLDBRK,"000"
                                        pack      cnctfname,B55
                                        call      LoadBrokerInfo
                              endif
                  repeat
          endif
.END PATCH 1.84 REPLACED LOGIC
.Put focus on first item in DataList
        setitem OrderSearchList,0,1
.Load the Inquiry Screen immediately
        call    OrderData4
        call    OrderSwitchToFour
        return

.START PATCH 1.84 ADDED LOGIC
LoadBrokerInfoA
          pack      COMPFLD,CNCTCODE
          move    "2-COMPKEY",Location
        call    COMPKEY
LoadBrokerInfo
          clear     NBRKVARS
          call      MVBRVARS
          move      B1,BRBLANK
          pack hold,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRADDR,BR2ADDR,BRCITY:
                    BRSTATE,BRZIP,BRCOUN,BRNAME,BRREVDAT,BRCREDIT,BRBLANK,BRTELE,BRFAX,BRSALES
          insertitem OrderSearchList,9999,hold
          return
.END PATCH 1.84 ADDED LOGIC

OrderData2      
        getitem Order2DataList,0,result
        getitem Order2DataList,result,hold2
.Start Patch #1.1 - increased var
.        unpack  hold2,OMLRPON,str1,OLRN,str1,BRCNTCT,MCOMP,O1DES,OQTY,ORTNDTEM:
.                ORTNDTED,ORTNDTEY,OODTEM,OODTED,OODTEY,NPNDDESC
        unpack  hold2,OMLRPON,str1,OLRN,str1,BRCNTCT,MCOMP,O1DES,OQTY,ORTNDTEC:
                ORTNDTEY,ORTNDTEM,ORTNDTED,OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC
.End Patch #1.1 - increased var
        call    OrderLoadScreen2
        return
.
OrderData3
        getitem Order3DataList,0,result
        getitem Order3DataList,result,hold3
        unpack  hold3,OMLRPON,str1,OLRN,str1,BRCNTCT,MCOMP,O1DES,SQUANT,SDATE:
                SINFO,STRACK
        call    OrderLoadScreen3
        return

OrderData4      
        getitem OrderSearchList,0,result
        getitem OrderSearchList,result,hold
        unpack  hold,BRKNUM,str1,BRKCNT,str1,BRCOMP,BRCNTCT,BRADDR,BR2ADDR,BRCITY:
                BRSTATE,BRZIP,BRCOUN,BRNAME,BRREVDAT,BRCREDIT,BRBLANK,BRTELE,BRFAX,BRSALES
        call    OrderLoadScreen4
        setfocus OrderSearchList
        return

OrderSwitchToTwo
        if (TabNum <> C2)
                if (TabNum = C3)
                        move    C2,N1
                elseif (TabNum = C4)
                        move    C3,N1
                endif
                call    OrderTabClick
                move    C1,N1
                call    OrderTabChange
                setitem OrderTabControl001,0,1
        endif
        return
        
OrderSwitchToThree
        if (TabNum <> C3)
                if (TabNum = C2)
                        move    C1,N1
                elseif (TabNum = C4)
                        move    C3,N1
                endif
                call    OrderTabClick
                move    C2,N1
                call    OrderTabChange
                setitem OrderTabControl001,0,2
        endif
        return 
               
OrderSwitchToFour
        if (TabNum <> C4)
                if (TabNum = C2)
                        move    C1,N1
                elseif (TabNum = C3)
                        move    C2,N1
                endif
                call    OrderTabClick
                move    C3,N1
                call    OrderTabChange
                setitem OrderTabControl001,0,3
        endif
        return 

OrderTabClick
        IF (N1 = C1)
                Deactivate Nord037a
        elseif (N1 = C2 )
                Deactivate Nord037b
        else    .N1 = C3
                Deactivate Nord037c
        Endif
        return

OrderTabChange
        IF (N1 = C1)
                move    C2,TabNum
                Activate Nord037a
                setfocus Order2DataList
        elseif (N1 = C2)
                move    C3,TabNum
                Activate Nord037b
                setfocus Order3DataList
        else
                move    C4,TabNum
                Activate Nord037c
        Endif
        return

.....................................................................
.....................REPORTS AND PRINTING ROUTINES...................
.....................................................................
OrderSingleBrokerReport
.Used for DAILY BROKER REPORT
.Disable Appropriate Buttons
        setprop OrderPrint,enabled=0
        setprop OrderExit,enabled=0
        setprop OrderOK,enabled=0
.Select Printer
        call    SetOrderDailyBrokerReportScreen
        setprop Report,visible=1
        if (RptCan = YES)
                goto OrderEndPrint
        endif   
.Format Report Screen Vars
        getitem ReportEditText1,0,FromBrk
.If you are choosing a different Broker then one displayed go
.back and display that Broker before printing
        if (FromBrk <> key)
                setitem OrderSearchKey,0,FromBrk
                call    Reload          .GO BACK TO ORDEROK BUTTON
        endif
        count   HowMany,FromBrk
        if (HowMany <> "0")
                if (HowMany < 4)
.Pack number with preceding zeroes
                        sub     HowMany from "4" giving N1
                        setlptr filler, N1
                        pack    str4,filler,FromBrk
                        move    str4,FromBrk                      
                        reset   FromBrk
                endif
        else
.Else force Broker entry
                alert   note,"Select Broker Number!",result
                call    SetOrderDailyBrokerReportScreen
                setprop Report,visible=1
                if (RptCan = YES)
                        goto OrderEndPrint
                endif
        endif
        getitem ReportCheck1,0,Preview
        getitem ReportCheck2,0,Default
        getitem ReportCheck3,0,Select
.Default and Select should be mutually exclusive
        if (Default = 1 AND Select = 1)
                alert   note,"Choose Default OR Select!",result
                goto OrderSingleBrokerReport
        elseif (Default = 0 AND Select = 0)
                move    C1,Default      .REESTABLISH DEFAULT IF NO CHOICE WAS MADE
        endif
.Preview it?
        if (Preview = 0)
                if (Default = 1)
                        PRTOPEN prfile,"-",WPrognme
                else    .Select
                        PRTOPEN prfile,"",WPrognme
                endif
        else    .PREVIEW
                if (Default = 1)
                        PRTOPEN prfile,"@",WPrognme
                else    .Select
                        PRTOPEN prfile,"@?",WPrognme
                endif
        endif
.Set up columns
        move    "500",column
        move    "1500",column2  .used to be 2000
        move    "3700",column3  .used to be 4000
        move    "4500",column4  .used to be 4800
        move    "5600",column5
        count   HowMany,key
.Clear page for each printing instance
        clear   page
.Always Print Header
.START PATCH 1.83 REPLACED LOGIC
.        call    OrderPrintHeader
        getitem ReportCheck4,0,result
          if (result = 1)
                    call      OrderPrintHeader using C1
          else
                    call      OrderPrintHeader using C0
          endif
.END PATCH 1.83 REPLACED LOGIC
.Start Pending Print
.START PATCH 1.3 - ADDED LOGIC
        move    "STATUS OF ORDERS RECEIVED",RptTitle2 
.END PATCH 1.3 - ADDED LOGIC
        call    OrderPendHeader
.START PATCH 1.83 ADDED LOGIC
        getitem ReportCheck4,0,result
          if (result = 1)
                    call      OrderSingleBrokerReportB
          else
                    call      OrderSingleBrokerReportA
          endif
          return

OrderSingleBrokerReportA
.END PATCH 1.83 ADDED LOGIC
        clear   result
        clear   HowMany
        getitem Order2DataList,1,HowMany
        if (HowMany = 0)
                add     eightlpi,row
                prtpage prfile;*p3100:row,*font=font2,"--- NO PENDING ORDERS ---";
                add     eightlpi,row
                add     eightlpi,row
                add     eightlpi,row
        else
.MAINTAIN REFERENCES TO N3 FOR TESTING!!!!!  SEE DOCUMENTATION BELOW.
.               CLEAR   N3
                loop
.                       ADD     C1,N3
                        add     C1,result
                        until (result > HowMany)
                        getitem Order2DataList,result,hold2
.Start Patch #1.1 - increased var
.                        unpack  hold2,OMLRPON,str1,OLRN,str1,BRCNTCT,MCOMP,O1DES,OQTY,ORTNDTEM:
.                                ORTNDTED,ORTNDTEY,OODTEM,OODTED,OODTEY,NPNDDESC

                              unpack    hold2,OMLRPON,str1,OLRN,str1,BRCNTCT,MCOMP,O1DES,OQTY,ORTNDTEC:
                                        ORTNDTEY,ORTNDTEM,ORTNDTED,OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC
.End Patch #1.1 - increased var
.TEST FOR ENOUGH ROOM ON PAGE
                        if (row >= 9576)        .POSITION OF 13th RECORD
                                prtpage prfile;*NEWPAGE;
.START PATCH 1.83 REPLACED LOGIC
.                                       call    OrderPrintHeader
                                        call    OrderPrintHeader using C0
.END PATCH 1.83 REPLACED LOGIC

                                call    OrderPendHeader
                        endif                   
                        call    OrderPrintPendRecord
.FOLLOWING SECTION IS FOR FORMAT TESTING.  CURRENTLY A PAGE BREAK OCCURS AFTER THE 13TH RECORD
.(EITHER PENDING OR SHIPPED)  THERE IS, HOWEVER, A PAGE BREAK IF THE SHIPPED HEADER FALLS AFTER
.ANYTHING GREATER THAN THE 11TH RECORD.  THIS INSURES THAT THE SHIPPED HEADER WILL ALWAYS HAVE
.AT LEAST ONE RECORD UNDER IT BEFORE A NEW PAGE BREAK; WHILE KEEPING THE FOOTER FROM BEING CROWDED.
.ALL VERBAGE (CURRENTLY REMMED) REFERRING TO "N3" IS USED TO REPEAT RECORDS SO THAT EXACT LOCATION
.OF PAGE BREAKS MIGHT BE TESTED.
.                       IF (N3 < 10)
.                               CLEAR   RESULT
.                       ENDIF
                repeat
                add     eightlpi,row    .Extra space after last record
        endif
.Start Shipping Header
        if (row > 8496)                         .POSITION OF 11th PENDING RECORD
                prtpage prfile;*NEWPAGE;
.START PATCH 1.83 REPLACED LOGIC
.                   call    OrderPrintHeader
                    call    OrderPrintHeader using C0
.END PATCH 1.83 REPLACED LOGIC

        endif
        call    OrderShipHeader
        clear   result
        clear   HowMany
        getitem Order3DataList,1,HowMany
        if (HowMany = 0)
.FOLLOWING SECTION USED FOR TEST PURPOSES.  SEE ABOVE DOCUMENTATION.
.               CLEAR   N3
.               unpack  hold2,OMLRPON,str1,OLRN,str1,BRCNTCT,MCOMP,O1DES,OQTY,ORTNDTEC:
.                       ORTNDTEY,ORTNDTEM,ORTNDTED,OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC
.               LOOP
.               ADD     C1,N3
.               UNTIL (N3 > 14)
.               if (row >= 9576)        .POSITION OF 13th RECORD
.                               prtpage prfile;*NEWPAGE;
.                               call    OrderPrintHeader
.                               call    OrderShipHeader
.                       endif   
.               call    OrderPrintPendRecord
.               REPEAT
                add     eightlpi,row
                prtpage prfile;*p3100:row,*font=font2,"--- NO SHIPPED ORDERS ---";
                add     eightlpi,row
        else
                loop
                        add     C1,result
                        until (result > HowMany)
                        getitem Order3DataList,result,hold3
                        unpack  hold3,OMLRPON,str1,OLRN,str1,BRCNTCT,MCOMP,O1DES,SQUANT,SDATE:
                                SINFO,STRACK
.TEST FOR ENOUGH ROOM ON PAGE
                        if (row >= 9576)        .POSITION OF 13th RECORD
                                prtpage prfile;*NEWPAGE;
.START PATCH 1.83 REPLACED LOGIC
.                                       call    OrderPrintHeader
                                        call    OrderPrintHeader using C0
.END PATCH 1.83 REPLACED LOGIC
                                call    OrderShipHeader
                        endif                   
                        call    OrderPrintShipRecord
                repeat
        endif
OrderEndPrint
        prtclose PRfile
.Enable Appropriate Buttons
        setprop OrderPrint,enabled=1
        setprop OrderExit,enabled=1
        setprop OrderOK,enabled=1
        return

.START PATCH 1.83 ADDED LOGIC
OrderSingleBrokerReportB
          move      "^^^FIRST&&&",CONTHOLD
        clear   result
        clear   HowMany
        getitem Order2TestDataList,1,HowMany
        if (HowMany = 0)
                add     eightlpi,row
                prtpage prfile;*p3100:row,*font=font2,"--- NO PENDING ORDERS ---";
                add     eightlpi,row
                add     eightlpi,row
                add     eightlpi,row
                add     eightlpi,row
                prtpage prfile;*p3100:row,*font=font2,"--- NO SHIPPED ORDERS ---";
                add     eightlpi,row
        else
.MAINTAIN REFERENCES TO N3 FOR TESTING!!!!!  SEE DOCUMENTATION BELOW.
.               CLEAR   N3
                    move      C0,RecFlag2
                    loop
.                       ADD     C1,N3
                              add     C1,result
                              until (result > HowMany)
                              getitem Order2TestDataList,result,hold2
                              unpack    hold2,BRCNTCT,RecFlag
                              if (RecFlag = C1)                       .Pending Record
                                        unpack    hold2,BRCNTCT,RecFlag,OMLRPON,OLRN,MCOMP,O1DES,OQTY,ORTNDTEC:
                                                  ORTNDTEY,ORTNDTEM,ORTNDTED,OODTEC,OODTEY,OODTEM,OODTED,NPNDDESC
                              else                                    .Shipping Record
                                        unpack    hold2,BRCNTCT,RecFlag,OMLRPON,OLRN,MCOMP,O1DES,SQUANT,SDATE:
                                                  SINFO,STRACK
                              endif
                              if (BRCNTCT <> CONTHOLD)
                                        if (CONTHOLD = "^^^FIRST&&&")
.I have manually calculated the row.  I do not use the variable as I want to have it remain stable
.eightlpi = 135
.I pull the row from OrderPrintHeader, so if that routine changes, this will need to as well.
.                                               move    "300",row
.                                               add     eightlpi,row
.                                               add     eightlpi,row
.                                               add     eightlpi,row
.                                               add     "60",row
.                                               add     eightlpi,row
.                                               add     eightlpi,row 
.                                               add     eightlpi,row
.                                               add     eightlpi,row
                                                prtpage prfile;*pcolumn:1305,*font=font13,*boldoff,"TO:  ";
                                                prtpage prfile;BRCNTCT;
                                                  if (RecFlag = C2)
                                                            add     eightlpi,row
                                                            prtpage prfile;*p3100:row,*font=font2,"--- NO PENDING ORDERS ---";
                                                            add     eightlpi,row
                                                            add     eightlpi,row
                                                            add     eightlpi,row
                                                  endif
                                        else                          .Go back and Add Contact Name
                                                  prtpage prfile;*NEWPAGE;
                                            call    OrderPrintHeader using C1
                                                  if (RecFlag = C1)
                                            call    OrderPendHeader
                                                  else
                                            call    OrderShipHeader
                                                            move      RecFlag,RecFlag2
                                                  endif
                                        endif
                                        move      BRCNTCT,CONTHOLD
                              endif
.END PATCH 1.83 ADDED LOGIC
.End Patch #1.1 - increased var
.TEST FOR ENOUGH ROOM ON PAGE
                        if (row >= 9576)        .POSITION OF 13th RECORD
                                prtpage prfile;*NEWPAGE;
                                call    OrderPrintHeader using C1
                                        if (RecFlag = C1)
                                          call    OrderPendHeader
                                        else
                                  call    OrderShipHeader
                                                  move      RecFlag,RecFlag2
                                        endif
                        endif                   
                              if (RecFlag = C1)
                                  call    OrderPrintPendRecord
                              else
                                        if (RecFlag <> RecFlag2)
                                  call    OrderShipHeader
                                        endif
                                  call    OrderPrintShipRecord
                              endif
                              move      RecFlag,RecFlag2
.FOLLOWING SECTION IS FOR FORMAT TESTING.  CURRENTLY A PAGE BREAK OCCURS AFTER THE 13TH RECORD
.(EITHER PENDING OR SHIPPED)  THERE IS, HOWEVER, A PAGE BREAK IF THE SHIPPED HEADER FALLS AFTER
.ANYTHING GREATER THAN THE 11TH RECORD.  THIS INSURES THAT THE SHIPPED HEADER WILL ALWAYS HAVE
.AT LEAST ONE RECORD UNDER IT BEFORE A NEW PAGE BREAK; WHILE KEEPING THE FOOTER FROM BEING CROWDED.
.ALL VERBAGE (CURRENTLY REMMED) REFERRING TO "N3" IS USED TO REPEAT RECORDS SO THAT EXACT LOCATION
.OF PAGE BREAKS MIGHT BE TESTED.
.                       IF (N3 < 10)
.                               CLEAR   RESULT
.                       ENDIF
                repeat
        endif
        goto OrderEndPrint
.END PATCH 1.83 ADDED LOGIC


OrderDailyListingReport
.Used for DAILY PENDING REPORT
        setprop OrderPrint,enabled=0
        setprop OrderExit,enabled=0
        setprop OrderOK,enabled=0
.START PATCH 1.3 - ADDED LOGIC
        setprop Report,title="Daily Pending Report"
.END PATCH 1.3 - ADDED LOGIC
        call    SetOrderDailyPendingReportScreen
OrderDailyListingReport2
.Label above used in case incorrect field input occured
.START PATCH 1.3 - REPLACED LOGIC
.        setprop Report,visible=1
.        if (RptCan = YES)
.                goto OrderEndPrint
.        endif       
..Format Report Screen Vars
.        getitem ReportEditText1,0,FromBrk
.        getitem ReportEditText2,0,ToBrk
.        count   HowMany,FromBrk
.        if (HowMany <> "0")
.                if (HowMany < 4)
..Pack number with preceding zeroes
.                        sub     HowMany from "4" giving N1
.                        setlptr filler, N1
.                        pack    str4,filler,FromBrk
.                        move    str4,FromBrk                      
.                        reset   FromBrk
.                endif
.        else
.                pack    FromBrk,"0000"
.        endif
.        count   HowMany,ToBrk
.        if (HowMany <> "0")
.                if (HowMany < 4)
..Pack number with preceding zeroes
.                        sub     HowMany from "4" giving N1
.                        setlptr filler, N1
.                        pack    str4,filler,ToBrk
.                        move    str4,ToBrk                      
.                        reset   ToBrk
.                endif
.        else
.                pack    ToBrk,"9999"
.        endif
..Dates
..Logic permits FROMDATE & TODATE entry format as follows:
..               MMDDYY
..               MMDDCCYY
..               MM/DD/YY
..               MM/DD/CCYY
..               MM-DD-YY
..               MM-DD-CCYY
..Any other entry format will force re-entry of data!
.        getitem ReportEditText4,0,str10
.        call    TRIM using str10
.        count   N2,str10
.        if (N2 = 0)
.                pack    FromDate,"00000000"
.        elseif ((N2 <> 6) AND (N2 <> 8) AND (N2 <> 10))
.                alert   caution,"Bad From-Date Format!",result
.                setfocus ReportEditText4
.                goto    OrderDailyListingReport2
.        else    .Valid Date
.                reset   str10
.                scan    SLASH,str10
.                if equal
.                        reset   str10
.                        if (N2 = 6)
.                                alert   caution,"Bad From-Date Format!",result
.                                setfocus ReportEditText4
.                                goto    OrderDailyListingReport2
.                        elseif (N2 =8)
.                                unpack  str10,MM,str1,DD,str1,YY
.                                pack    FromDate,CC,YY,MM,DD
.                        else    .N2 = 10
.                                unpack  str10,MM,str1,DD,str1,str2,YY
.                                pack    FromDate,str2,YY,MM,DD
.                        endif
.                else
.                        reset   str10
.                        scan    DASH,str10
.                        if equal
.                                reset   str10
.                                if (N2 = 6)
.                                        alert   caution,"Bad From-Date Format!",result
.                                        setfocus ReportEditText4
.                                        goto    OrderDailyListingReport2
.                                elseif (N2 =8)
.                                        unpack  str10,MM,str1,DD,str1,YY
.                                        pack    FromDate,CC,YY,MM,DD
.                                else    .N2 = 10
.                                        unpack  str10,MM,str1,DD,str1,str2,YY
.                                        pack    FromDate,str2,YY,MM,DD
.                                endif                
.                        else
.                                reset   str10
.                                if (N2 = 6)
.                                        unpack  str10,MM,DD,YY
.                                        pack    FromDate,CC,YY,MM,DD
.                                elseif (N2 =8)
.                                        unpack  str10,MM,DD,str2,YY
.                                        pack    FromDate,str2,YY,MM,DD
.                                else    .N2 = 10
.                                        alert   caution,"Bad From-Date Format!",result
.                                        setfocus ReportEditText4
.                                        goto    OrderDailyListingReport2
.                                endif
.                        endif
.                endif
.                type    FromDate
.                if not equal
.                        alert   caution,"Bad From-Date Format!",result
.                        setfocus ReportEditText4
.                        goto    OrderDailyListingReport2
.                endif
.        endif
.        getitem ReportEditText5,0,str10
.        call    TRIM using str10
.        count   N2,str10
.        if (N2 = 0)
.                pack    ToDate,"99999999"
.        elseif ((N2 <> 6) AND (N2 <> 8) AND (N2 <> 10))
.                goto    OrderDailyListingReport2
.        else    .Valid Date
.                reset   str10
.                scan    SLASH,str10
.                if equal
.                        reset   str10
.                        if (N2 = 6)
.                                alert   caution,"Bad To-Date Format!",result
.                                setfocus ReportEditText5
.                                goto    OrderDailyListingReport2
.                        elseif (N2 =8)
.                                unpack  str10,MM,str1,DD,str1,YY
.                                pack    ToDate,CC,YY,MM,DD
.                        else    .N2 = 10
.                                unpack  str10,MM,str1,DD,str1,str2,YY
.                                pack    ToDate,str2,YY,MM,DD
.                        endif
.                else
.                        reset   str10
.                        scan    DASH,str10
.                        if equal
.                                reset   str10
.                                if (N2 = 6)
.                                        alert   caution,"Bad To-Date Format!",result
.                                        setfocus ReportEditText5
.                                        goto OrderDailyListingReport2
.                                elseif (N2 =8)
.                                        unpack  str10,MM,str1,DD,str1,YY
.                                        pack    ToDate,CC,YY,MM,DD
.                                else    .N2 = 10
.                                        unpack  str10,MM,str1,DD,str1,str2,YY
.                                        pack    ToDate,str2,YY,MM,DD
.                                endif                        
.                        else
.                                reset   str10
.                                if (N2 = 6)
.                                        unpack  str10,MM,DD,YY
.                                        pack    ToDate,CC,YY,MM,DD
.                                elseif (N2 =8)
.                                        unpack  str10,MM,DD,str2,YY
.                                        pack    ToDate,str2,YY,MM,DD
.                                else    .N2 = 10
.                                        alert   caution,"Bad To-Date Format!",result
.                                        setfocus ReportEditText5
.                                        goto OrderDailyListingReport2
.                                endif
.                        endif
.                endif
.                type    ToDate
.                if not equal
.                        alert   caution,"Bad To-Date Format!",result
.                        setfocus ReportEditText5
.                        goto OrderDailyListingReport2
.                endif
.        endif        
..Print Options        
.        getitem ReportCheck1,0,Preview
.        getitem ReportCheck2,0,Default
.        getitem ReportCheck3,0,Select
..Default and Select should be mutually exclusive
.        if (Default = 1 AND Select = 1)
.                alert   note,"Choose Default OR Select!",result
.                setfocus ReportCheck2
.                goto OrderDailyListingReport2
.        elseif (Default = 0 AND Select = 0)
.                move    C1,Default      .REESTABLISH DEFAULT IF NO CHOICE WAS MADE
.        endif
..Preview it?
.        if (Preview = 0)
.                if (Default = 1)
.                        PRTOPEN prfile,"-",WPrognme
.                else    .Select
.                        PRTOPEN prfile,"",WPrognme
.                endif
.        else    .PREVIEW
.                if (Default = 1)
.                        PRTOPEN prfile,"@",WPrognme
.                else    .Select
.                        PRTOPEN prfile,"@?",WPrognme
.                endif
.        endif
..Set up columns
.        move    "500",column
.        move    "1500",column2  .used to be 2000
.        move    "3700",column3  .used to be 4000
.        move    "4500",column4  .used to be 4800
.        move    "5600",column5
.        count   HowMany,key
        move    NO,RtnFlag
        call    OrderCollectReportParams
        if (RtnFlag = YES)
                goto OrderDailyListingReport2
        elseif (RtnFlag = "E")
                goto OrderEndPrint
        endif
.END PATCH 1.3 - REPLACED LOGIC
.Clear page for each printing instance
        clear   page
.START PATCH 1.3 - ADDED LOGIC
.Establish Report title
. these two vars need to be established before Headers are printed
        move    "  DAILY PENDING REPORT",RptTitle     .used by OrderPrintHeader2
        move    "STATUS OF ORDERS RECEIVED",RptTitle2   .used by OrderPendHeader
.END PATCH 1.3 - ADDED LOGIC
        call    OrderPrintHeader2
        call    OrderPendHeader
.*****************************
.Open Files
.START PATCH 1.8 REPLACED LOGIC
.        open    input3,"G:\DATA\NINPRINT.dat"
.START PATCH 1.81 REPLACED LOGIC
.        PACK     STR35,NTWKPATH1,"NINPRINT.dat"
.        open    input3,str35
.>patch 1.88
.        PACK    taskname,NTWKPATH1,"NINPRINT.dat|20.20.30.103:502"
        PACK    taskname,NTWKPATH1,"NINPRINT.dat|10.10.30.103:502"
.>patch 1.88        
        open    input3,taskname
.END PATCH 1.81 REPLACED LOGIC
.END PATCH 1.8 REPLACED LOGIC
        prepare input4,"c:\work\ORDFILE.DAT"
PendLoop
        loop
                read    input3,seq;ordvars
                until over
                scan    OSTAT,GoodStat
                if equal
.Start Patch #1.1 - added century
.                        pack    str8,CC,OODTEY,OODTEM,OODTED
                        pack    str8,OODTEC,OODTEY,OODTEM,OODTED
.END Patch #1.1 - added century
                        if ((str8 >= FromDate) AND (str8 <= ToDate))
                                if ((OBRKNUM >= FromBrk) AND (OBRKNUM <= ToBrk))
                                        call    OrderReadOtherFiles
                                        call    OrderReadPendFiles
                                        filepi  1;input4
.START PATCH 1.3 - ADDED CENTURY
.                                        write   input4,seq;OBRKNUM,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP:
.                                                O1DES,OQTY,ORTNDTEM,ORTNDTED,ORTNDTEY,NPNDDESC
                                        write   input4,seq;OBRKNUM,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP:
                                                O1DES,OQTY,ORTNDTEM,ORTNDTED,ORTNDTEC,ORTNDTEY,NPNDDESC
.END PATCH 1.3 - ADDED CENTURY
                                endif
                        endif
                endif
                reset   GoodStat                
         repeat
.START PATCH 1.3 - REPLACED LOGIC
.SortFile
.        clear   taskname
.        move    "c:\work\ORDFILE.DAT,C:\work\ORDFILE.SRT,C:\work;1-4,113-157",taskname
.        reset   taskname
.        sort    taskname
.        if over
.                move    s$error$,error
.                move    "Sort did not work!",Location
.                call    IOMssg
.                stop
.        endif
.        clear   taskname   
..Begin Printing
..Initialize HLDBRK
.        clear   HOLDBRK        
..close files
.        close   input3
.        close   input4
..open newly sorted file
.        open    input4,"C:\work\ORDFILE.SRT"
.        loop
.                read    input4,seq;OBRKNUM,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES:
.                        OQTY,ORTNDTEM,ORTNDTED,ORTNDTEY,NPNDDESC
.                until over
.                if ((OBRKNUM <> HOLDBRK) AND (row < 9440))
.                        add     eightlpi,row
.                        prtpage prfile;*pcolumn:row,*font=font5,*boldon,OBRKNUM,B2,BRCOMP
.                        add     eightlpi,row
.                        add     eightlpi,row
.                endif
.                move    OBRKNUM,HOLDBRK
.                if (row > 9440)
.                        prtpage prfile;*NEWPAGE;
.                        call    OrderPrintHeader2
.                        call    OrderPendHeader
.                        add     eightlpi,row
.                        prtpage prfile;*pcolumn:row,*font=font5,*boldon,OBRKNUM,B2,BRCOMP
.                        add     eightlpi,row
.                        add     eightlpi,row
.                endif
.                call    OrderPrintPendRecord
.        repeat
.        close   input4
.        clear   taskname
..START PATCH 1.2 - REPLACED LOGIC
..        append  "c:\command.com /c del C:\work\ordfile.dat",taskname
.        Path    Exist,"c:\windows"
.        if      over
.                append  "c:\winnt\system32\cmd.exe",taskname
.        else    
.                append  "!c:\command.com",taskname
.        endif
.        append  " /c del C:\work\ordfile.dat",taskname
..END PATCH 1.2 - REPLACED LOGIC 
.        reset   taskname
.        execute taskname
.        clear   taskname
..START PATCH 1.2 - REPLACED LOGIC
..        append  "c:\command.com /c del C:\work\ordfile.srt",taskname
.        Path    Exist,"c:\windows"
.        if      over
.                append  "c:\winnt\system32\cmd.exe",taskname
.        else    
.                append  "!c:\command.com",taskname
.        endif
.        append  " /c del C:\work\ordfile.srt",taskname
..END PATCH 1.2 - REPLACED LOGIC
.        reset   taskname
.        execute taskname
..*****************************
        call    SortFile
.END PATCH 1.3 - REPLACED LOGIC
        goto    OrderEndPrint

.START PATCH 1.3 - ADDED LOGIC
OrderCancelledPendingReport
.Used for Cancelled Pending Report
        setprop OrderPrint,enabled=0
        setprop OrderExit,enabled=0
        setprop OrderOK,enabled=0
        setprop Report,title="Cancelled Pending Report"
        call    SetOrderDailyPendingReportScreen
OrderCancelledPendingReport2
.Label above used in case incorrect field input occured
        move    NO,RtnFlag
        call    OrderCollectReportParams
        if (RtnFlag = YES)
                goto OrderCancelledPendingReport2
        elseif (RtnFlag = "E")
                goto OrderEndPrint
        endif
.Clear page for each printing instance
        clear   page
.Establish Report title
. these two vars need to be established before Headers are printed
        move    "CANCELLED PENDING REPORT",RptTitle     .used by OrderPrintHeader2
        move    "",RptTitle2                            .used by OrderPendHeader
        call    OrderPrintHeader2
        call    OrderPendHeader
.Open Files
        prepare input4,"c:\work\ORDFILE.DAT"
CancelLoop
        loop
                call    nord4SEQ
                until over
                if (NORD4STAT = "07")
                        if ((NORDPDTE >= FromDate) AND (NORDPDTE <= ToDate))
                                move    C1,NORDPATH
                                move    NORD4LR,NORDFLD
                                move    "CancelLoop-NORDKEY",Location
                                call    NORDKEY
                                if ((OBRKNUM >= FromBrk) AND (OBRKNUM <= ToBrk))
                                        call    OrderReadOtherFiles
                                        move    "Cancelled",npnddesc
                                        unpack  NORDPDTE,ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED
                                        filepi  1;input4
                                        write   input4,seq;OBRKNUM,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP:
                                                O1DES,OQTY,ORTNDTEM,ORTNDTED,ORTNDTEC,ORTNDTEY,NPNDDESC
                                endif
                        endif
                endif
        repeat
        call    SortFile
        goto    OrderEndPrint
.END PATCH 1.3 - ADDED LOGIC

.START PATCH 1.3 - CODE NOW USED BY TWO ROUTINES
OrderCollectReportParams
        setprop Report,visible=1
        if (RptCan = YES)
                move    "E",RtnFlag
                return
        endif       
.Format Report Screen Vars
        getitem ReportEditText1,0,FromBrk
        getitem ReportEditText2,0,ToBrk
        count   HowMany,FromBrk
        if (HowMany <> "0")
                if (HowMany < 4)
.Pack number with preceding zeroes
                        sub     HowMany from "4" giving N1
                        setlptr filler, N1
                        pack    str4,filler,FromBrk
                        move    str4,FromBrk                      
                        reset   FromBrk
                endif
        else
                pack    FromBrk,"0000"
        endif
        count   HowMany,ToBrk
        if (HowMany <> "0")
                if (HowMany < 4)
.Pack number with preceding zeroes
                        sub     HowMany from "4" giving N1
                        setlptr filler, N1
                        pack    str4,filler,ToBrk
                        move    str4,ToBrk                      
                        reset   ToBrk
                endif
        else
                pack    ToBrk,"9999"
        endif
.Dates
.Logic permits FROMDATE & TODATE entry format as follows:
.               MMDDYY
.               MMDDCCYY
.               MM/DD/YY
.               MM/DD/CCYY
.               MM-DD-YY
.               MM-DD-CCYY
.Any other entry format will force re-entry of data!
        getitem ReportEditText4,0,str10
        call    TRIM using str10
        count   N2,str10
        if (N2 = 0)
                pack    FromDate,"00000000"
        elseif ((N2 <> 6) AND (N2 <> 8) AND (N2 <> 10))
                alert   caution,"Bad From-Date Format!",result
                setfocus ReportEditText4
                move    YES,RtnFlag
                return
        else    .Valid Date
                reset   str10
                scan    SLASH,str10
                if equal
                        reset   str10
                        if (N2 = 6)
                                alert   caution,"Bad From-Date Format!",result
                                setfocus ReportEditText4
                                move    YES,RtnFlag
                                return
                        elseif (N2 =8)
                                unpack  str10,MM,str1,DD,str1,YY
                                pack    FromDate,CC,YY,MM,DD
                        else    .N2 = 10
                                unpack  str10,MM,str1,DD,str1,str2,YY
                                pack    FromDate,str2,YY,MM,DD
                        endif
                else
                        reset   str10
                        scan    DASH,str10
                        if equal
                                reset   str10
                                if (N2 = 6)
                                        alert   caution,"Bad From-Date Format!",result
                                        setfocus ReportEditText4
                                        move    YES,RtnFlag
                                        return
                                elseif (N2 =8)
                                        unpack  str10,MM,str1,DD,str1,YY
                                        pack    FromDate,CC,YY,MM,DD
                                else    .N2 = 10
                                        unpack  str10,MM,str1,DD,str1,str2,YY
                                        pack    FromDate,str2,YY,MM,DD
                                endif                
                        else
                                reset   str10
                                if (N2 = 6)
                                        unpack  str10,MM,DD,YY
                                        pack    FromDate,CC,YY,MM,DD
                                elseif (N2 =8)
                                        unpack  str10,MM,DD,str2,YY
                                        pack    FromDate,str2,YY,MM,DD
                                else    .N2 = 10
                                        alert   caution,"Bad From-Date Format!",result
                                        setfocus ReportEditText4
                                        move    YES,RtnFlag
                                        return
                                endif
                        endif
                endif
                type    FromDate
                if not equal
                        alert   caution,"Bad From-Date Format!",result
                        setfocus ReportEditText4
                        move    YES,RtnFlag
                        return
                endif
        endif
        getitem ReportEditText5,0,str10
        call    TRIM using str10
        count   N2,str10
        if (N2 = 0)
                pack    ToDate,"99999999"
        elseif ((N2 <> 6) AND (N2 <> 8) AND (N2 <> 10))
                move    YES,RtnFlag
                return
        else    .Valid Date
                reset   str10
                scan    SLASH,str10
                if equal
                        reset   str10
                        if (N2 = 6)
                                alert   caution,"Bad To-Date Format!",result
                                setfocus ReportEditText5
                                move    YES,RtnFlag
                                return
                        elseif (N2 =8)
                                unpack  str10,MM,str1,DD,str1,YY
                                pack    ToDate,CC,YY,MM,DD
                        else    .N2 = 10
                                unpack  str10,MM,str1,DD,str1,str2,YY
                                pack    ToDate,str2,YY,MM,DD
                        endif
                else
                        reset   str10
                        scan    DASH,str10
                        if equal
                                reset   str10
                                if (N2 = 6)
                                        alert   caution,"Bad To-Date Format!",result
                                        setfocus ReportEditText5
                                        move    YES,RtnFlag
                                        return
                                elseif (N2 =8)
                                        unpack  str10,MM,str1,DD,str1,YY
                                        pack    ToDate,CC,YY,MM,DD
                                else    .N2 = 10
                                        unpack  str10,MM,str1,DD,str1,str2,YY
                                        pack    ToDate,str2,YY,MM,DD
                                endif                        
                        else
                                reset   str10
                                if (N2 = 6)
                                        unpack  str10,MM,DD,YY
                                        pack    ToDate,CC,YY,MM,DD
                                elseif (N2 =8)
                                        unpack  str10,MM,DD,str2,YY
                                        pack    ToDate,str2,YY,MM,DD
                                else    .N2 = 10
                                        alert   caution,"Bad To-Date Format!",result
                                        setfocus ReportEditText5
                                        move    YES,RtnFlag
                                        return
                                endif
                        endif
                endif
                type    ToDate
                if not equal
                        alert   caution,"Bad To-Date Format!",result
                        setfocus ReportEditText5
                        move    YES,RtnFlag
                        return
                endif
        endif        
.Print Options        
        getitem ReportCheck1,0,Preview
        getitem ReportCheck2,0,Default
        getitem ReportCheck3,0,Select
.Default and Select should be mutually exclusive
        if (Default = 1 AND Select = 1)
                alert   note,"Choose Default OR Select!",result
                setfocus ReportCheck2
                move    YES,RtnFlag
                return
        elseif (Default = 0 AND Select = 0)
                move    C1,Default      .REESTABLISH DEFAULT IF NO CHOICE WAS MADE
        endif
.Preview it?
        if (Preview = 0)
                if (Default = 1)
                        PRTOPEN prfile,"-",WPrognme
                else    .Select
                        PRTOPEN prfile,"",WPrognme
                endif
        else    .PREVIEW
                if (Default = 1)
                        PRTOPEN prfile,"@",WPrognme
                else    .Select
                        PRTOPEN prfile,"@?",WPrognme
                endif
        endif
.Set up columns
        move    "500",column
        move    "1500",column2  .used to be 2000
        move    "3700",column3  .used to be 4000
        move    "4500",column4  .used to be 4800
        move    "5600",column5
        count   HowMany,key
        return

SortFile
        clear   taskname
        move    "c:\work\ORDFILE.DAT,C:\work\ORDFILE.SRT,C:\work;1-4,113-157",taskname
        reset   taskname
        sort    taskname
        if over
                move    s$error$,error
                move    "Sort did not work!",Location
                call    IOMssg
                stop
        endif
        clear   taskname   
.Begin Printing
.Initialize HLDBRK
        clear   HOLDBRK        
.close files
        close   input3
        close   input4
.open newly sorted file
        open    input4,"C:\work\ORDFILE.SRT"
        loop
.START PATCH 1.3 - ADDED CENTURY
.                read    input4,seq;OBRKNUM,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES:
.                        OQTY,ORTNDTEM,ORTNDTED,ORTNDTEY,NPNDDESC
                read    input4,seq;OBRKNUM,OMLRPON,OLRN,BRCNTCT,BRCOMP,MCOMP,O1DES:
                        OQTY,ORTNDTEM,ORTNDTED,ORTNDTEC,ORTNDTEY,NPNDDESC
.END PATCH 1.3 - ADDED CENTURY
                until over
                if ((OBRKNUM <> HOLDBRK) AND (row < 9440))
                        add     eightlpi,row
                        prtpage prfile;*pcolumn:row,*font=font5,*boldon,OBRKNUM,B2,BRCOMP
                        add     eightlpi,row
                        add     eightlpi,row
                endif
                move    OBRKNUM,HOLDBRK
                if (row > 9440)
                        prtpage prfile;*NEWPAGE;
                        call    OrderPrintHeader2
                        call    OrderPendHeader
                        add     eightlpi,row
                        prtpage prfile;*pcolumn:row,*font=font5,*boldon,OBRKNUM,B2,BRCOMP
                        add     eightlpi,row
                        add     eightlpi,row
                endif
                call    OrderPrintPendRecord
        repeat
        close   input4
        clear   taskname
.START PATCH 1.2 - REPLACED LOGIC
.        append  "c:\command.com /c del C:\work\ordfile.dat",taskname
;begin patch 1.82
                    call                GetWinVer
.begin patch 1.90
.;        Path    Exist,"c:\windows"
.;        if      over
.                    If                  (osflag = c1 | osflag = C5)
.                append  "c:\winnt\system32\cmd.exe",taskname
.;        else
.                ElseIf                  (osflag = c3 | osflag = C4)
.                append  "!c:\command.com",taskname
.                ElseIf                  (osflag = c6)
.                append  "c:\windows\system32\cmd.exe",taskname
.;end patch 1.82
.        endif
.        append  " /c del C:\work\ordfile.dat",taskname
..END PATCH 1.2 - REPLACED LOGIC 
.        reset   taskname
.        execute taskname
          Erase     "c:\work\ordfile.dat"
.end patch 1.90
        clear   taskname
.START PATCH 1.2 - REPLACED LOGIC
.        append  "c:\command.com /c del C:\work\ordfile.srt",taskname
;begin patch 1.82
                    call                GetWinVer
.begin patch 1.90
.;        Path    Exist,"c:\windows"
.;        if      over
.                    If                  (osflag = c1 | osflag = C5)
.                append  "c:\winnt\system32\cmd.exe",taskname
.;        else
.                ElseIf                  (osflag = c3 | osflag = C4)
.                append  "!c:\command.com",taskname
.                ElseIf                  (osflag = c6)
.                append  "c:\windows\system32\cmd.exe",taskname
.;end patch 1.82
.        endif
.        append  " /c del C:\work\ordfile.srt",taskname
..END PATCH 1.2 - REPLACED LOGIC
.        reset   taskname
.        execute taskname
          Erase     "c:\work\ordfile.srt"
.end patch 1.90
        return
.END PATCH 1.3 - CODE NOW USED BY TWO ROUNTINES
...................................................................
......................ACTUAL PRINTING BEGINS.......................
...................................................................
.Print Headings
OrderPrintHeader LRoutine FrmPtr
.Used for DAILY BROKER REPORT
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate1,MM,SLASH,DD,SLASH,CC,YY
        add     C1,page
        prtpage prfile;*UNITS=*HIENGLISH;
.START PATCH 1.7 - REPLACED LOGIC
.        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=1:15000:1:8750:PICT1
.        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=9700:14700:100:8750:PICT2
.        move    "300",row
..................................................... 
...FOLLOWING SECTION USED TO DETERMINE PLACEMENT OF COLUMNS - GOOD FOR FUTURE REFERNECE, KEEP IT!!!!
..        clear   str1
..        clear   n1
..        move    row,column2
..        add     sixlpi,column2       
..        prtpage prfile;*pcolumn:row,*font=font2,"0"
..        loop
..                add     c1,n1
..                add   "100",column
..                prtpage prfile;*pcolumn:row,*font=font2,"1"
..                add    "100",column
..                prtpage prfile;*pcolumn:row,*font=font2,"2"
..                add    "100",column
..                prtpage prfile;*pcolumn:row,*font=font2,"3"
..                add    "100",column
..                prtpage prfile;*pcolumn:row,*font=font2,"4"
..                add    "100",column
..                prtpage prfile;*pcolumn:row,*font=font2,"5"
..                add    "100",column
..                prtpage prfile;*pcolumn:row,*font=font2,"6"
..                add    "100",column
..                prtpage prfile;*pcolumn:row,*font=font2,"7"
..                add    "100",column
..                prtpage prfile;*pcolumn:row,*font=font2,"8"
..                add    "100",column
..                prtpage prfile;*pcolumn:row,*font=font2,"9"
..                add    "100",column
..                move    n1,str1
..                prtpage prfile;*pcolumn:row,*font=font2,str1
..                prtpage prfile;*pcolumn:column2,"0"
..                until (column >= "9000")
..        repeat
..        move    "100",column
..        move    "200",row                
..        loop
..                prtpage prfile;*pcolumn:row,*font=font2,row
..                add     "200",row
..                until (row >= "15000")
..        repeat
..        return
................................................
.        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page
.        add     sixlpi,row
.        add     sixlpi,row
.        add     eightlpi,row 
        move    "300",row
        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page;
.START PATCH 1.86 REPLACED LOGIC
.        prtpage prfile;*p2700:row,*font=font10,"Names";
.        prtpage prfile;*font=font11,"  in the News";
.        add     eightlpi,row
.        add     eightlpi,row
.        add     eightlpi,row
.        prtpage prfile;*p1000:row,*pensize=10,*line=7100:row;
.        add     "60",row
.        prtpage prfile;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.        prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
..............................
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     "60",row
        add     eightlpi,row
.END PATCH 1.86 REPLACED LOGIC
.END PATCH 1.7 - REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row 
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"TO:  ";
.START PATCH 1.83 REPLACED LOGIC
.        prtpage prfile;BRCOMP;
          if (FrmPtr = C0)
                    prtpage prfile;BRCOMP;
          else
                    prtpage prfile;BRCNTCT;
          endif
.END PATCH 1.83 REPLACED LOGIC
        prtpage prfile;*p6000:row,"FROM:  List Management";
        add     eightlpi,row 
.START PATCH 1.83 ADDED LOGIC
          if (FrmPtr = C1)
                  prtpage prfile;*pcolumn:row,"        ",BRCOMP;
          endif
.END PATCH 1.83 ADDED LOGIC
        prtpage prfile;*p6000:row,"DATE:  ",newdate1;
        add     eightlpi,row
.START PATCH 1.83 ADDED LOGIC
          if (FrmPtr = C1)
                    add     eightlpi,row
          endif
.END PATCH 1.83 ADDED LOGIC
        prtpage prfile;*pcolumn:row,"Below is the status information on your order(s).  ";
        prtpage prfile;"Please distribute to the listed Contact name(s).  Thank you.";
        add     eightlpi,row 
.START PATCH 1.87 REPLACED LOGIC 
        prtpage prfile;*pcolumn:row,"**All pricing subject to rate verification at time of order. Please verify pricing at www.namesinthenews.com";
.END PATCH 1.87 REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row 
.START PATCH 1.83 REPLACED LOGIC
.        add     eightlpi,row
          if (FrmPtr = C0)
                    add     eightlpi,row
          endif
.END PATCH 1.83 REPLACED LOGIC
        return
OrderPrintHeader2
.Used for DAILY PENDING REPORT
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
        add     C1,page
        prtpage prfile;*UNITS=*HIENGLISH;
.START PATCH 1.7 - REPLACED LOGIC
.        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=1:15000:1:8750:PICT1
.        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=9700:14700:100:8750:PICT2
.        move    "300",row
.        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page
.        add     sixlpi,row
.        add     sixlpi,row
.        add     eightlpi,row
        move    "300",row
        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page;
.START PATCH 1.86 REPLACED LOGIC
.        prtpage prfile;*p2700:row,*font=font10,"Names";
.        prtpage prfile;*font=font11,"  in the News";
.        add     eightlpi,row
.        add     eightlpi,row
.        add     eightlpi,row
.        prtpage prfile;*p1000:row,*pensize=10,*line=7100:row;
.        add     "60",row
.        prtpage prfile;*p2700:row,*font=font12,"C  A  L  I  F  O  R  N  I  A        I  N  C .";
..Go ahead and print the last line now
..Bullets produced using:  Alt+0149
.        prtpage prfile;*p1500:9950,*font=font13,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 • 415-989-3350 • Fax 415-433-7796";
..............................
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     "60",row
        add     eightlpi,row
.END PATCH 1.86 REPLACED LOGIC
.END PATCH 1.7 - REPLACED LOGIC
        add     eightlpi,row
        add     eightlpi,row 
        add     eightlpi,row
        add     eightlpi,row
.START PATCH 1.3 - REPLACED LOGIC
.        prtpage prfile;*p3100:row,*font=font5,*boldon,"DAILY PENDING REPORT";
        prtpage prfile;*p2900:row,*font=font5,*boldon,RptTitle;
.END PATCH 1.3 - REPLACED LOGIC
        if (FromDate = ToDate)
                prtpage prfile;*pcolumn5:row,"DATE:  ";                  .PRINT SINGLE DATE
                unpack  ToDate,STR2,YY,MM,DD
                pack    newdate1,MM,SLASH,DD,SLASH,STR2,YY
                prtpage prfile;newdate1;
        elseif ((FromDate <> "00000000") AND (ToDate <> "99999999"))    .PRINT BOTH DATES
                prtpage prfile;*pcolumn5:row,"DATE:  ";
                unpack  FromDate,STR2,YY,MM,DD
                pack    newdate1,MM,SLASH,DD,SLASH,STR2,YY
                prtpage prfile;newdate1," - ";
                unpack  ToDate,STR2,YY,MM,DD
                pack    newdate1,MM,SLASH,DD,SLASH,STR2,YY
                prtpage prfile;newdate1;
        elseif (FromDate <> "00000000")                                 .PRINT FROMDATE ONLY
                prtpage prfile;*pcolumn5:row,"FROM DATE:  ";
                unpack  FromDate,STR2,YY,MM,DD
                pack    newdate1,MM,SLASH,DD,SLASH,STR2,YY
                prtpage prfile;newdate1;
        else                                                            .PRINT TODATE ONLY
                prtpage prfile;*pcolumn5:row,"TO DATE:  ";
                unpack  ToDate,STR2,YY,MM,DD
                pack    newdate1,MM,SLASH,DD,SLASH,STR2,YY
                prtpage prfile;newdate1;
        endif
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        return        
OrderPendHeader
.........Pending Header
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"Broker PO";
        prtpage prfile;*pcolumn2:row,"Contact";
        prtpage prfile;*pcolumn3:row,"Quantity";
        prtpage prfile;*pcolumn4:row,"Request Date";
        prtpage prfile;*pcolumn5:row,"Status";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"NIN ##";
        prtpage prfile;*pcolumn2:row,"Mailer";
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,"List";
        add     eightlpi,row
.START PATCH 1.3 - REPLACED LOGIC
.        prtpage prfile;*p3000:row,"STATUS OF ORDERS RECEIVED";
        prtpage prfile;*p3000:row,RptTitle2;
.END PATCH 1.3 - REPLACED LOGIC
        add     sixlpi,row
.FOLLOWING CODE USEFUL IN ILLUSTRATING DIFFERENT PENSIZES - KEEP IT FOR FUTURE REFERENCE!!!!
.        prtpage prfile;*pcolumn:row,*line=7300:row;
.        add     sixlpi,row
.        prtpage prfile;*pcolumn:row,*pensize=10,*line=7300:row;
.        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*pensize=20,*line=7500:row;
.        add     sixlpi,row
.        prtpage prfile;*pcolumn:row,*pensize=30,*line=7300:row;
.        add     sixlpi,row
.        prtpage prfile;*pcolumn:row,*pensize=40,*line=7300:row;
.        add     sixlpi,row
.        prtpage prfile;*pcolumn:row,*pensize=50,*line=7300:row;
        add     eightlpi,row
        return
OrderShipHeader
.........Shipping Header
        prtpage prfile;*pcolumn:row,*font=font5,*boldon,"Broker PO";
        prtpage prfile;*pcolumn2:row,"Contact";
        prtpage prfile;*pcolumn3:row,"Quantity";
        prtpage prfile;*pcolumn4:row,"Ship Date";
        prtpage prfile;*pcolumn5:row,"Ship Method";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"NIN ##";
        prtpage prfile;*pcolumn2:row,"Mailer";
        prtpage prfile;*pcolumn5:row,"Tracker ##";
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,"List";
        add     eightlpi,row
        prtpage prfile;*p3100:row,"SHIPPING CONFIRMATION";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*line=7500:row;
        add     eightlpi,row    
        return

OrderPrintPendRecord
        prtpage prfile;*pcolumn:row,*font=font2,*boldoff,OMLRPON;
        prtpage prfile;*pcolumn2:row,*boldon,BRCNTCT,*boldoff;
        move    EditMask,EditQuan
        move    C0,Quan
        move    OQTY,Quan
        edit    Quan,EditQuan
        prtpage prfile;*pcolumn3:row,EditQuan;
        call    TRIM using ORTNDTEM
        count   N2,ORTNDTEM
        if (N2 <> 0 AND ORTNDTEM <> "00")
.START PATCH 1.3 - ADDED CENTURY
.                prtpage prfile;*pcolumn4:row,ORTNDTEM,SLASH,ORTNDTED,SLASH,CC,ORTNDTEY;
                prtpage prfile;*pcolumn4:row,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY;
.END PATCH 1.3 - ADDED CENTURY
        endif
        prtpage prfile;*pcolumn5:row,NPNDDESC;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,OLRN;
        prtpage prfile;*pcolumn2:row,MCOMP;
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,O1DES;
        add     eightlpi,row
        add     eightlpi,row
        return

OrderPrintShipRecord
        prtpage prfile;*pcolumn:row,*font=font2,*boldoff,OMLRPON;
        prtpage prfile;*pcolumn2:row,*boldon,BRCNTCT,*boldoff;
        move    EditMask,EditQuan
        move    C0,Quan
        move    SQUANT,Quan
        edit    Quan,EditQuan
        prtpage prfile;*pcolumn3:row,EditQuan;
        call    TRIM using SDATE
        count   N2,SDATE
        if (N2 <> 0)
                unpack  SDATE,str2,YY,MM,DD
                call    TRIM using MM
                count   N2,MM
                if (N2 <> 0 AND MM <> "00")
                        prtpage prfile;*pcolumn4:row,MM,SLASH,DD,SLASH,str2,YY;
                endif
        endif
        prtpage prfile;*pcolumn5:row,SINFO;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,OLRN;
        prtpage prfile;*pcolumn2:row,MCOMP;
        prtpage prfile;*pcolumn5:row,STRACK;
        add     eightlpi,row
        prtpage prfile;*pcolumn2:row,O1DES;
        add     eightlpi,row
        add     eightlpi,row
        return

.Include IO file
        include comlogic.inc
        include nordio.inc
.patch1.85
                                        include   compio.inc
                                        include   cntio.inc
.        include nmlrio.inc
.        include nbrkio.inc
.patch1.85
        include nshpio.inc
        include npndio.inc
        include nord4io.inc
