........................................
. Program:      NPAY0001.PLS
. Function:     Pay-to File Maintenance
. Author:       Andrew Harkins
. Orig. Date:   August 28,1998
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include npaydd.inc
        include npasdd.inc
        include nowndd.inc
        include stabbdd.inc

release         init   "1.6.1"    ASH 18NOV2003 CHANGED EDITTEXT BACKGROUND COLOR
.release         init   "1.6"    ASH 20MAR200 REPLACED AAM SEARCH LOGIC
.release         init   "1.5"    ASH 19MAR99 Moved: 1) Vars associated with passwrd.plf not found in NPASDD.INC
.release         init   "1.4"    ASH 19JAN99 added: 1) Added timeout logic
.release         init   "1.3"    ASH 03Dec98 fixes: 1) Added logic to allow blank chars. in first 3 of search
.                                             criteria, provided string is longer than 3
.release        init    "1.2"    ASH 23OCT98 Added logic to prevent '?' as search criteria for first 4 chars
.release        init    "1.1"    ASH 01OCT98 1. Password.plf ->Passwrd.plf
.                                           2. NINPAY.DAT expansion
.                                           3. Logic for future dynamic sizing
.RELEASE        init    "1.0"   ASH 28SEP98 Original Release
.Start Patch #1.4 - Timer logic
Timer   Timer
.End Patch #1.4 - Timer logic
.START PATCH #1.5 - VARS NOW DEFINED IN NPASDD.INC
.Vars used for Password Box
.progcode init   "P"
.END PATCH #1.5 - VARS NOW DEFINED IN NPASDD.INC
.START PATCH #1.5 - VARS NOW DEFINED IN NPASDD.INC
.PassFlag init   "N"
.END PATCH #1.5 - VARS NOW DEFINED IN NPASDD.INC
ExitFlag init   "Y"
ReturnFlag init "N"
UpdateFlag init "N"
AamFlag init    "N"
NewFlag init    "N"
CompFlag init   "N"
AKey1   init    "01F"
filler  init    "0000"
filler2 init    "00000"
area    dim     3
newnum  dim     13
newdate dim     10
olddate dim     10
.Set to length of record plus space for B1 and space for "-"
hold    dim     229
.Set to maximum length of Aam
key     dim     90
holdkey dim     5
date    dim     8
tstpayvars  list
tstPAYRCODE DIM       1           1-1     RECORD CODE = '6'
tstPOWNER   DIM       4           2-5     OWNER NUMBER
tstPAYNUM   DIM       1           6-6     PAY-TO NUMBER FOR MULT PAY-TO'S
tstPNAME    DIM       45          7-51    CONTACT NAME
tstPCOMP    DIM       45         52-96    COMPANY NAME
tstPSTREET  DIM       25         97-121   STREET ADDRESS
tstP2STREET DIM       25        122-146   STREET ADDRESS2
tstPCITY    DIM       15        147-161   CITY
tstPSTATE   DIM       2         162-163   STATE
tstPZIP     DIM       10        164-173   ZIPCODE
tstPCOUN    DIM       15        174-188   COUNTRY CODE
tstPPASS    DIM       10        189-198   PASSWORD ID
tstPDATE    DIM       8         199-206   REVISED DATE
tstPBLANK   DIM       1         207-207   NOT USED.
tstpayTELE  DIM       10        208-217   TELEPHONE NUMBER
tstpayFAX   DIM       10        218-227    FAX NUMBER.
         listend

.Colors
white   color
grey    color

.Set Up Menu Bar
mFile    menu
mEdit    menu
mHelp    menu

.Present Data for Menu Bar
FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
HData   init    "&Help;&About"
.........................
.Start Patch #1.1 - added logic for dynamic form sizing
font5   font
coll1   collection
specs   form          4(4)
size    form          "1.000"
infostring dim        590

.Getinfo - NOT YET IMPLEMENTED!!!!!!!
.        getinfo system,infostring
.        bump    infostring,12
.        move    infostring,str4
.        bump    infostring,4
.        move    infostring,str5     
.End Patch #1.1 - added logic for dynamic form sizing
.............................
.Set Vars used for About Box
        move    "NPAY0001.PLS",Wprognme
        move    "Pay-To File Maintenance",Wfunction
        move    "Andrew Harkins",Wauthor
        move    "1.6",Wrelease
        move    "March 20, 2000",Wreldate

.Declare forms, Always declare child forms first
mss1    plform  Error
pss     plform  Passwrd
abt     plform  About
x       plform  \\nins1\e\library\plb_Src\Npay0001
        winhide
.Load Forms, Always load parent form first
        formload x
        formload abt
        formload pss
        formload mss1

.Start Patch #1.4 - timer logic
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.End Patch #1.4 - timer logic
.Create Menus
        create  Npay0001;mFile,FData
        create  Npay0001;mEdit,EData,mFile
        create  Npay0001;mHelp,HData,mEdit

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
.Only a SubMenu under this one
        activate mHelp,HelpGo,result

.Create Colors for EditText Inquiry
        create  white=*white
.START PATCH 1.6.1 REPLACED LOGIC
.        create  grey=*ltgray
        create  grey=220:220:220
          call      PayToDisableLower
.END PATCH 1.6.1 REPLACED LOGIC

.Start Patch #1.1 - added logic for dynamic form sizing
.COLLECTION NOT YET USED!!!
.        listins Coll1,Payto,PaytoCancel,PaytoDelete,PaytoEditAddress,PaytoEditAddress2:
.        PaytoEditCity,PaytoEditCode,PaytoEditCompany,PaytoEditContact,PaytoEditCountry:
.        PaytoEditDate,PaytoEditFax,PaytoEditNumber,PaytoEditState,PaytoEditTelephone:
.        PaytoEditUser,PaytoEditZip,PaytoExit,PaytoInquiryGroup,PaytoModify,PaytoNew,PaytoOK:
.        PaytoPrint,PaytoSave,PaytoSearchKey,PaytoSearchList,PaytoStatAddress,PaytoStatAddress2:
.        PaytoStatBy,PaytoStatCity,PaytoStatCode,PaytoStatCompany,PaytoStatContact:
.        PaytoStatCountry,PaytoStatFax,PaytoStatLastUpdate,PaytoStatNumMssg,PaytoStatOwner:
.        PaytoStatSearch,PaytoStatSearch2,PaytoStatState,PaytoStatState2,PaytoStatTelephone:
.        PaytoStatZip,PaytoUpdate
.        
.        if (str4 = "1024")
.                create  font5,"Arial",size=12
.                setprop coll1,font=font5
.                move    "1.25",size
.                getprop Payto,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Payto,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoCancel,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoCancel,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoDelete,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoDelete,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop PaytoEditAddress,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditAddress,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoEditAddress2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditAddress2,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoEditCity,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditCity,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoEditCode,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditCode,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoEditCompany,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditCompany,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoEditContact,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditContact,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop PaytoEditCountry,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditCountry,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoEditDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoEditFax,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditFax,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoEditNumber,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditNumber,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5       
..
.                getprop PaytoEditState,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditState,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoEditTelephone,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditTelephone,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoEditUser,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditUser,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoEditZip,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoEditZip,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoExit,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoExit,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop PaytoInquiryGroup,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoInquiryGroup,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop PaytoModify,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoModify,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoNew,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoNew,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop PaytoOK,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoOK,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop PaytoPrint,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoPrint,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoSave,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoSave,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop PaytoSearchKey,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoSearchKey,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop PaytoSearchList,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoSearchList,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop PaytoStatAddress,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatAddress,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatAddress2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatAddress2,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatBy,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatBy,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatCity,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatCity,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatCode,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatCode,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatCompany,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatCompany,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatContact,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatContact,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop PaytoStatCountry,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatCountry,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatFax,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatFax,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatLastUpdate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatLastUpdate,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatNumMssg,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatNumMssg,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatOwner,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatOwner,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5       
..
.                getprop PaytoStatSearch,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatSearch,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatSearch2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatSearch2,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatState,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatState,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatState2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatState2,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatTelephone,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatTelephone,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoStatZip,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoStatZip,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop PaytoUpdate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop PaytoUpdate,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.        endif
.End Patch #1.1 - added logic for dynamic form sizing       

.START PATCH #1.5 - VARS NOW DEFINED IN NPASDD.INC
        move    "P",progcode
        move    "N",PassFlag
.END PATCH #1.5 - VARS NOW DEFINED IN NPASDD.INC
.Main Loop
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate,MM,SLASH,DD,SLASH,CC,YY
.Set Flags to Open NINPAY.DAT
        move    C0,NPAYFLAG 
        move    C0,NPAYFLG2
.Set Error Message Stat Text Boxes
        call    SetErrorMssgDefault
.Open Files
        trap    StateFileError if IO
        open    NSTFILE,NSTNAME
        trapclr IO        
.Display Password Form
        setprop Passwrd,visible=1
        setfocus PayToSearchKey
        loop
                waitevent
.Start Patch #1.4 - timer logic                
                setitem timer,0,18000   .reset to 30 minutes
.End Patch #1.4 - timer logic
        repeat
        
.Start Patch #1.4 - timer logic
Timeout
        beep
        beep
        beep
        stop
.End Patch #1.4 - timer logic
        
SetErrorMssgDefault
.Set Default for Pay-To File Maintenance
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=1
        setprop ErrorMssgStat4,visible=1
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0,"To Search By Owner/Pay-To Code:"
        setitem ErrorMssgStat2,0,"Enter 5 Digit Number"
        setitem ErrorMssgStat3,0,"To Search By Company Name:"
        setitem ErrorMssgStat4,0,"Enter Search Name"
        setitem ErrorMssgStat5,0,"      That Record Does Not Exist!"
        return

PayToClearRec
.Clear all Text Fields
        setitem PayToEditNumber,0,""
        setitem PayToEditCode,0,""
        setitem PayToEditContact,0,""
        setitem PayToEditCompany,0,""
        setitem PayToEditAddress,0,""
        setitem PayToEditAddress2,0,""
        setitem PayToEditCity,0,""
        setitem PayToEditState,0,""
        setitem PayToEditZip,0,""
        setitem PayToEditCountry,0,""
        setitem PayToEditUser,0,NPASuser
        setitem PayToEditDate,0,newdate
        setitem PayToEditTelephone,0,""
        setitem PayToEditFax,0,""
        setitem PayToStatState2,0,""
        setfocus PayToEditNumber
        return

FileGo
        if (ExitFlag = "Y")
                winshow
                stop
        endif
        return
EditGo
HelpGo
        setprop AboutMssg,visible=1
        return
PayToLoadList
        setitem PayToEditNumber,0,POwner
        setitem PayToEditCode,0,PayNum
        setitem PayToEditContact,0,PName
        setitem PayToEditCompany,0,PComp
        setitem PayToEditAddress,0,PStreet
        setitem PayToEditAddress2,0,P2Street
        setitem PayToEditCity,0,PCity
        setitem PayToEditState,0,PState
        call    Trim Using PState
        count   N2,PState
        if (N2 = "0")
                move    "",STNAME
        else
                pack    NSTFLD,PState
                trap    StateFileError if IO
                read    NSTFILE,NSTFLD;STVARS
                trapclr IO
                if over
                        move    "Unknown State",STNAME
                endif
        endif
        setitem PayToStatState2,0,STNAME
        setitem PayToEditzip,0,PZip
        setitem PayToEditCountry,0,PCoun
        setitem PayToEditUser,0,PPass
.Format Date
        clear   olddate
        count   N2,PDate
        if (N2 = "8")
                unpack  PDate,CC,YY,MM,DD
                pack    olddate,MM,slash,DD,slash,CC,YY
        endif
        setitem PayToEditDate,0,olddate
        clear   Newnum
        count   N2,PayTele
        if (N2 = "10")
                unpack  PayTele,area,str3,str4
                pack    Newnum,"(",area,")",str3,Dash,str4
                setprop PayToEditTelephone,maxchars=13
        else
                setprop PayToEditTelephone,maxchars=10
        endif
        setitem PayToEditTelephone,0,Newnum
        clear   Newnum
        count   N2,PayFax
        if (N2 = "10")
                unpack  PayFax,area,str3,str4
                pack    Newnum,"(",area,")",str3,Dash,str4
                setprop PayToEditFax,maxchars=13
        else
                setprop PayToEditFax,maxchars=10
        endif
        setitem PayToEditFax,0,Newnum
        setprop PayToModify,default=1
        packkey NPAYFLD,POWNER,PAYNUM
        packkey NPAYFLD2,"01X",PNAME,PCOMP
        return

.Disable Upper Screen
PayToDisableUpper
        setprop PayToSearchKey,enabled=0
        setprop PayToOK,enabled=0
        setprop PayToExit,enabled=0
.        setprop PayToPrint,enabled=0
        setprop PayToNew,enabled=0
        setprop PayToSearchList,enabled=0,bgcolor=grey
        return

.Enable Upper Screen
PayToEnableUpper
.Allow Exit
        move    "Y",ExitFlag
        setprop PayToSearchKey,enabled=1
        setprop PayToOK,enabled=1
        setprop PayToExit,enabled=1
.        setprop PayToPrint,enabled=1
        setprop PayToNew,enabled=1
        setprop PayToSearchList,enabled=1,bgcolor=white
        return

.Disable Lower Screen
PayToDisableLower
        setprop PayToEditAddress,enabled=0,bgcolor=grey
        setprop PayToEditAddress2,enabled=0,bgcolor=grey
        setprop PayToEditCity,enabled=0,bgcolor=grey
        setprop PayToEditCode,enabled=0,bgcolor=grey
        setprop PayToEditContact,enabled=0,bgcolor=grey
        setprop PayToEditCompany,enabled=0,bgcolor=grey
        setprop PayToEditDate,enabled=0,bgcolor=grey
        setprop PayToEditFax,enabled=0,bgcolor=grey
        setprop PayToEditNumber,enabled=0,bgcolor=grey
        setprop PayToEditState,enabled=0,bgcolor=grey
        setprop PayToEditTelephone,enabled=0,bgcolor=grey
        setprop PayToEditUser,enabled=0,bgcolor=grey
        setprop PayToEditZip,enabled=0,bgcolor=grey
        setprop PayToEditCountry,enabled=0,bgcolor=grey
        setprop PayToStatNumMssg,visible=0
        setprop PayToModify,enabled=1
        setprop PayToCancel,visible=0
        setprop PayToUpdate,visible=0
        return

.Enable Lower Screen
PayToEnableLower
        move    "N",UpdateFlag
        move    "N",ExitFlag
        setprop PayToEditAddress,enabled=1,bgcolor=white
        setprop PayToEditAddress2,enabled=1,bgcolor=white
        setprop PayToEditCity,enabled=1,bgcolor=white
        setprop PayToEditCode,enabled=1,bgcolor=white
        setprop PayToEditContact,enabled=1,bgcolor=white
        setprop PayToEditCompany,enabled=1,bgcolor=white
        setprop PayToEditDate,enabled=1,bgcolor=white
        setprop PayToEditFax,enabled=1,bgcolor=white
        setprop PayToEditNumber,enabled=1,bgcolor=white
        setprop PayToEditState,enabled=1,bgcolor=white
        setprop PayToEditTelephone,enabled=1,bgcolor=white
        setprop PayToEditUser,enabled=1,bgcolor=white
        setprop PayToEditZip,enabled=1,bgcolor=white
        setprop PayToEditCountry,enabled=1,bgcolor=white
        setprop PayToStatNumMssg,visible=1
        setprop PayToModify,enabled=0
        setprop PayToCancel,visible=1
        setprop PayToUpdate,visible=1
        return

.Verify Data Entry
PayToVerifyData
        move    C6,PayRCode
.
        getitem PayToEditNumber,0,POwner
        call    TRIM using POwner
        count   HowMany,POwner
        if (HowMany < 4)
                alert caution,"4 Digit Owner Number Required!",result
                setfocus PayToEditNumber
                move "Y",ReturnFlag
                return
        endif
        setitem PayToEditNumber,0,POwner
.        
        getitem PayToEditCode,0,PayNum
        call    TRIM using PayNum
        count   HowMany,PayNum
        if (HowMany = 0)
                alert caution,"Pay-To Number Required!",result
                setfocus PayToEditCode
                move "Y",ReturnFlag
                return
        endif
        setitem PayToEditCode,0,PayNum
.        
        getitem PayToEditContact,0,PName
        call    TRIM using PName
        setitem PayToEditContact,0,PName
.
        getitem PayToEditCompany,0,PComp
        call    TRIM using PComp
        count   HowMany,PComp
        if (HowMany = 0)
                alert caution,"Company Name Required!",result
                setfocus PayToEditCompany
                move "Y",ReturnFlag
                return
        endif
        setitem PayToEditCompany,0,PComp
.        
        getitem PayToEditAddress,0,PStreet
        call    TRIM using PStreet
        setitem PayToEditAddress,0,PStreet
.        
        getitem PayToEditAddress2,0,P2Street
        call    TRIM using P2Street
        setitem PayToEditAddress2,0,P2Street
.        
        getitem PayToEditCity,0,PCity
        call    TRIM using PCity
        setitem PayToEditCity,0,PCity        
.        
        getitem PayToEditState,0,PState
        call    TRIM using PState
        setitem PayToEditState,0,PState
.        
        getitem PayToEditZip,0,PZip
        call    TRIM using PZip
        setitem PayToEditZip,0,PZip
.        
        getitem PayToEditCountry,0,PCoun
        call    TRIM using PCoun
        setitem PayToEditCountry,0,PCoun
.        
        move    NPASuser,PPass
        setitem PayToEditUser,0,PPass
.        
        getitem PayToEditDate,0,HowMany
        if (UpdateFlag = "Y")
                unpack newdate,MM,STR1,DD,STR1,CC,YY
        else
                unpack olddate,MM,STR1,DD,STR1,CC,YY
        endif
        pack    PDate,CC,YY,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,CC,YY
        setitem PayToEditDate,0,str10
.        
        clear   Newnum
        getitem PayToEditTelephone,0,HowMany
        getitem PayToEditTelephone,0,Newnum
        if (HowMany <> 0)
                if (HowMany = 13)
                        unpack  Newnum,str1,area,str1,str3,str1,str4
                        pack    Newnum,area,str3,str4
                elseif (HowMany <> 10)
                        alert   caution,"10 Digit Number Only!",result
                        setfocus PayToEditTelephone
                        move    "Y",ReturnFlag
                        return
                endif
        endif
        move    Newnum,PayTele
        count   N9,Newnum
        if (Newnum > "0")
                unpack  Newnum,area,str3,str4
                pack    Newnum,"(",area,")",str3,dash,str4
        endif
        setitem PayToEditTelephone,0,Newnum
.
        clear   Newnum
        getitem PayToEditFax,0,HowMany
        getitem PayToEditFax,0,Newnum
        if (HowMany <> 0)
                if (HowMany = 13)
                        unpack  Newnum,str1,area,str1,str3,str1,str4
                        pack    Newnum,area,str3,str4
                elseif (HowMany <> 10)
                        alert caution,"10 Digit Number Only!",result
                        setfocus PayToEditFax
                        move "Y",ReturnFlag
                        return
                endif
        endif
        move    Newnum,PayFax
        count   N9,Newnum
        if (Newnum > "0")
                unpack  Newnum,area,str3,str4
                pack    Newnum,"(",area,")",str3,dash,str4
        endif
        setitem PayToEditFax,0,Newnum        
.        
        packkey NPAYFLD,POWNER,PAYNUM
.        packkey        NPAYFLD2,pname,pcomp
        return

RefreshPayToSearchList
.Clear DataList
        deleteitem PayToSearchList,0
.First Test for Blank Entry After "?"
        count    result,key
        if (result = "1")
                setprop ErrorMssg,visible=1
                setfocus PayToSearchKey
                setprop PayToOK,enabled=1
                return
        elseif (result < 3)
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
                setitem ErrorMssgStat5,0,"  Name Must Have 3+ Characters"
                setprop ErrorMssg,visible=1
                call    SetErrorMssgDefault
                setfocus PayToSearchKey
                setprop PaytoOK,enabled=1
                return
        endif
        setprop PayToSearchList,enabled=1,bgcolor=white
.        bump    key
        move    C3,NPAYLOCK
        pack    NPAYFLD2,AKey1,key
        move    "Driver-NPAYAIM",Location
        call    NPAYAIM
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
                setprop ErrorMssgStat5,visible=0
                setprop ErrorMssgStat1,visible=1
                setprop ErrorMssgStat2,visible=1
                setprop ErrorMssgStat3,visible=1
                setprop ErrorMssgStat4,visible=1

                setprop PayToOK,enabled=1
                setfocus PayToSearchKey
                return
        else
                move "Y",AamFlag
                pack hold,POWNER,DASH,PAYNUM,B1,PCOMP,PAYRCODE,PNAME,PSTREET,P2STREET:
                PCITY,PSTATE,PZIP,PCOUN,PPASS,PDATE,PBLANK,PAYTELE,PAYFAX
                insertitem PayToSearchList,9999,hold

        endif
        move    "Driver-NPAYKG",Location
        loop
                call NPAYKG
                if not over
                        pack hold,POWNER,DASH,PAYNUM,B1,PCOMP,PAYRCODE,PNAME,PSTREET,P2STREET:
                        PCITY,PSTATE,PZIP,PCOUN,PPASS,PDATE,PBLANK,PAYTELE,PAYFAX
                        insertitem PayToSearchList,9999,hold
                endif
                until over
        repeat
.Put focus on first item in DataList
        setitem PayToSearchList,1,1
.Load the Inquiry Screen immediately
        call PayToData
        return
.Include IO file
        include \\nins1\e\library\include\comlogic.inc
        include \\nins1\e\library\include\npayio.inc
        include \\nins1\e\library\include\npasio.inc
        include \\nins1\e\library\include\nownio.inc
