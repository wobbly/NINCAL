........................................
. Program:      NOwn0001.PLS
. Function:     Owner File Maintenance
. Author:       David Herrick
. Orig. Date:   October ,2000
. Release:      1.0
........................................

PC      EQU     1                .gui yes
.Include Files
        include common.inc
        include cons.inc
        include nOwndd.inc
.         include   f:\library\develop\backups\nowndd.inc
        include npasdd.inc
        include gnxtdd.inc
.begin release 1.1
        include npaydd.inc
.end release 1.1
.START PATCH 1.2.3 REPLACED LOGIC
..START PATCH 1.2 ADDED LOGIC
          include compdd.inc
          include cntdd.inc
          include Compnotesdd.inc       
.         include   nfuldd.inc          
           include    ntypdd.inc
..END PATCH 1.2 ADDED LOGIC
.END PATCH 1.2.3 REPLACED LOGIC
Release   init      "1.29"              DLH      .Typist
Reldate   Init      "2015 April 27"
.Release   init      "1.28"              DLH      .new field to suppress Shipping requests to LO
.Reldate   Init      "2015 February 12"
.Release   init      "1.27"              DLH      .outlook
.Reldate   Init      "2013 April 30"
.release   init      "1.2.6"     DLH     add room for company and contact keys
.Redate    Init      "14Jan2009"
.release init       "1.2.5"     DLH     24Jan8 add email
.release init       "1.2.4"     DMB     13OCT006 Disabled cc to object.  relationship is now established in ndat0001 - datacard/fulfillment
.release init       "1.2.3"     DMS     22JUN2006 FULFILLMENT CONVERSION
.release init    "1.2.2"   ASH          10JUN2005 ADDED CODE TO ALLOW OWNERS TO PREVENT SAMPLES BEING SENT
.release init    "1.2.1"   ASH          18NOV2003 CHANGED EDITTEXT BACKGROUND COLOR
.release init    "1.2"   ASH  30APR2002 NINFUL CONVERSION
.release init    "1.1"   DLH 03Apr2001 forgound color of red for owner with payto code 0
.release init    "1.0"   DLH Oct2000 Initial release
.Begin PATCH 1.27
Note    automation
Mes     automation      class="Outlook.Application"
.end PATCH 1.27

Timer   Timer
.Files to open
file2   file    uncomp
prfile  pfile
colorfile file
newOwn   form    4
ExitFlag init   "Y"
ReturnFlag init "N"
UpdateFlag init "N"
AamFlag init    "N"
NewFlag init    "N"
.Used to test if Aamdex needs to be updated
.START PATCH 1.2.3 REPLACED LOGIC  // b/c compflag was already in compdd that this patch added...
.CompFlag init   "N"
.CompFlag2 init   "N"
.END PATCH 1.2.3 REPLACED LOGIC
CompChg  init   "N"
AKey1   init    "01F"
filler  init    "0000"
filler2 init    "0000000"
area    dim     3
newnum  dim     13
newdate dim     10
olddate dim     10
.Length of record plus space for B1 and space for "/"
.hold    dim     192
.begin patch 1.2.6
.hold    dim     243
.hold    dim     252
hold    dim     312
.end patch 1.2.6
.key holds longest possible Aam
key     dim     50
holdkey dim     4
holdOwn dim     4
HoldFlag dim    1
.Vars used for printing
Ownprnt dim     4
linenumber form 1
maxline form    "7"
page    form    9
.Vars used for Report Screen
RptCan  dim     1
FromNo  dim     4
ToNo    dim     4
FromDate dim    8
ToDate  dim     8
List    form    1
Inactive form   1
.
date    dim     8
.begin patch 1.1
payto0  form    1
.end patch 1.1

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
mFile    menu
mEdit    menu
mHelp    menu
mOptions menu
.Set Up SubMenu for Options
sColor  submenu

.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Color"
HData   init    "&Help;&About"

.Present Data for Colors SubMenu
CData   init    ";&Background;&Text"

.Define Collections for Object Colors
ColText Collection
ColBack Collection

.Define Colors for Each Object
FTC     color
BGC     color

colornum dim   9(2)
Fred     form    3
Fgreen   form    3
Fblue    form    3
colorflag form   1
.................................
coll1   collection
specs   form          4(4)
size    form          "1.000"
infostring dim        590
.START PATCH 1.2 ADDED LOGIC
ComboNum form       9
.END PATCH 1.2 ADDED LOGIC

.............................
.Set Vars used for About Box
        move    "Nown0001.PLS",Wprognme
        move    "Owner File Maintenance",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        move    "October 20, 2000",Wreldate
          move      reldate,Wreldate
          
.Declare forms, Always declare child forms first
.rpt     plform  Report
mss1    plform  Error
pss     plform  Passwrd
abt     plform  About
x       plform  NOwn0001
        winhide
.Load Forms, Always load parent form first
        formload x
        formload abt
        formload pss
        formload mss1
.        formload rpt

        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        create  NOwn0001;mFile,FData
        create  NOwn0001;mEdit,EData,mFile
        create  NOwn0001;mOptions,OData,mEdit
        create  NOwn0001;mHelp,HData,mOptions
.Create SubMenus
.        create  NOwn0001;sColor,CData,mOptions,1

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
.Only a SubMenu under these
        activate mOptions
        activate mHelp,HelpGo,result
.Activate SubMenus
.        activate sColor,ColorGo,result

.Create Colors for EditText Inquiry
        create  white=*white
.START PATCH 1.2.1 REPLACED LOGIC
.        create  grey=*ltgray
          create    grey=220:220:220
.END PATCH 1.2.1 REPLACED LOGIC
        create  RED=*RED
        create  black=*black

.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic
        move    "O",progcode
        move    "N",PassFlag
.Main Loop
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate,MM,SLASH,DD,SLASH,CC,YY
.Set Error Message Stat Text Boxes
        call    SetOwnerErrorMssgDefault
.Set Flags to Open NINOwn.DAT
        move    C0,NOwnFLG1
        move    C0,NOwnFLG2
.START PATCH 1.2 ADDED LOGIC
.Must delete blank items entered to ensure adequate space
          deleteitem OwnerComboCC,0
.START PATCH 1.2.3 REPLACED LOGIC
.         move      C1,NFULPATH
.         move      "Driver-NFULSEQ",Location
.         pack      KeyLocation,"Key: Sequential"
.         loop
..First one will always be blank
.                   pack      taskname,NFULCOMP,NFULNUM
.                   insertitem OwnerComboCC,ComboNum,taskname
.                   add       C1,ComboNum
.                   call      NFULSEQ
.                   until over
.         repeat
          move      C1,COMPPATH
          move      "Driver-COMPSEQ",Location
          pack      KeyLocation,"Key: Sequential"
.First one will always be blank
          pack      taskname,""
          insertitem OwnerComboCC,ComboNum,taskname
          add       C1,ComboNum
          OPEN    COMPFILE2,COMPNAME
          pack    str4,"05X","T"
          READ    COMPFILE2,str4;COMPVARS
          if not over
                    loop
.                   call COMPKS6
                              if (COMPSVBFLG = "T")
                                        pack      taskname,COMPCOMP,COMPOLDSVB
                                        insertitem OwnerComboCC,ComboNum,taskname
                                        add       C1,ComboNum
                              endif
                              READKG COMPFILE2;COMPVARS                         
                    until over                    
                    repeat
          endif
.END PATCH 1.2.3 REPLACED LOGIC
          setitem   OwnerComboCC,0,1
.END PATCH 1.2 ADDED LOGIC
.START PATCH 1.2.1 REPLACED LOGIC
          call      OwnerDisableLower
.END PATCH 1.2.1 REPLACED LOGIC
.begin patch 1.27  
        setprop OwnerEmail,Enabled=1
        setprop OwnerEmail,Visible=1
.end patch 1.27

.Open color file
opencolor
.        trap    colorerror if io
.        open    colorfile,"c:\progra~1\nincal\nOwn0001.col"
.        goto    colorerror if over
.        clear   n1
.        loop
.                add     c1,n1
.                read    colorfile,seq;colornum(n1)
.                until   over
.                until   (n1 = 2)
.        repeat
.        close   colorfile
.        trapclr io
.        unpack  colornum(1),Fred,Fgreen,Fblue
.        create  FTC=Fred:Fgreen:Fblue
.        setprop ColText,fgcolor=FTC
.
.        unpack  colornum(2),Fred,Fgreen,Fblue
.        create  BGC=Fred:Fgreen:Fblue
.        setprop ColBack,bgcolor=BGC
aftercolor

           EVENTREG  X, 17, XRESIZE

        setfocus OwnerSearchKey
        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat

Timeout
        beep
        beep
        beep
        stop
SetOwnerErrorMssgDefault
.Set Default for Owner File Maintenance
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=1
        setprop ErrorMssgStat4,visible=1
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0,"To Search By Owner Number:"
        setitem ErrorMssgStat2,0,"Enter 4 Digit Number"
        setitem ErrorMssgStat3,0,"To Search By Company Name:"
        setitem ErrorMssgStat4,0,"Enter Search Name"
        setitem ErrorMssgStat5,0,"      That Record Does Not Exist!"
        setitem ErrorMssgOK,0,"&OK"
        return

OwnerClearRec
.Clear all Text Fields
        setitem OwnerEditNumber,0,""
        setitem OwnerEditContact,0,""
        setitem OwnerEditCompany,0,""
        setitem OwnerEditAddress,0,""
        setitem OwnerEditCity,0,""
        setitem OwnerEditState,0,""
        setitem OwnerEditZip,0,""
        setitem OwnerEditCountry,0,""
        setitem OwnerEditUser,0,NPASuser
        setitem OwnerEditDate,0,newdate
        setitem OwnerEditTelephone,0,""
        setitem OwnerEditFax,0,""
        setitem OwnerEditFax2,0,""
        setitem OwnercheckGalley,0,""
.START PATCH 1.2.2 ADDED LOGIC
        setitem OwnercheckSamples,0,0
.END PATCH 1.2.2 ADDED LOGIC
        setitem OwnercheckCopy,0,0
        setitem OwnercheckStatus,0,""
.START PATCH 1.2 ADDED LOGIC
.         SetItem OwnerEditCarbonCopyTo,0,""
          setitem   OwnerComboCC,0,1
          setitem   OwnerComboVia,0,1
        setitem OwnerEditTextEmail,0,""
.begin patch 1.2.6        
          setitem   OwnerEditText001,0,""
          setitem   OwnerEditText002,0,""
          setitem   OwnerEditText003,0,""
          setitem   OwnerEditText004,0,""

.end patch 1.2.6        
          
.END PATCH 1.2 ADDED LOGIC
.begin patch 1.28
           setItem    OwnerCheckNoShp,0,0
.end patch 1.28

        setfocus OwnerEditCOntact
        return

FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo3,FileGo3
FileGo1
        PRTOPEN prfile,"-",WPrognme
        CLOCK   DATE,DATE
        unpack  date,mm,str1,dd,str1,yy
.        goto    StartPrint
FileGo2
        PRTOPEN prfile,"@",WPrognme
        CLOCK   DATE,DATE
        unpack  date,mm,str1,dd,str1,yy
.        goto    StartPrint
        return
FileGo3
        if (ExitFlag = "Y")
                winshow
                stop
        endif
        return
EditGo

colorerror
.        noreturn
.        move    C1,colorflag
.        clear   n1
.        prep    colorfile,"c:\progra~1\nincal\nOwn0001.col"
.        loop
.                add     c1,n1
.                write   colorfile,seq;colornum(n1)
.                until (n1 =2)
.        repeat
.        close   colorfile
        goto aftercolor
ColorGo
        if (result = C1)
                call    BackColor
        elseif (result = C2)
                call    TextColor
        else
                return
        endif
        clear   n1
.        prep    colorfile,"c:\progra~1\nincal\nOwn0001.col"
.        loop
.                add     c1,n1
.                write   colorfile,seq;colornum(n1)
.                until (n1 =2)
.        repeat
.        close   colorfile
        return
.Trap for Cancel Entry in Color System Menu
ColorTrap
        noreturn
        return
BackColor
        trap    ColorTrap if object
        create  BGC
        trapclr object
        setprop ColBack,bgcolor=BGC
        getitem BGC,1,Fred
        getitem BGC,2,Fgreen
        getitem BGC,3,Fblue
        pack    colornum(2),Fred,Fgreen,Fblue
        return

TextColor
        trap    ColorTrap if object
        create  FTC
        trapclr object
        setprop ColText,fgcolor=FTC
        getitem FTC,1,Fred
        getitem FTC,2,Fgreen
        getitem FTC,3,Fblue
        pack    colornum(1),Fred,Fgreen,Fblue
        return
.End Patch #2.0 - added logic

HelpGo
        setprop AboutMssg,visible=1
        return

OwnerLoadList
.begin patch 1.1
        packkey npayfld from ownlon,c0
        call    npaykey
        if      over
        move    c0 to payto0
        else
        move    c1 to payto0
        endif
        setitem OwnerEditNumber,0,ownlon
        setitem OwnerEditContact,0,OWNLONM
        setitem OwnerEditCompany,0,OWNOCPY
        if      (payto0 = c1)
        setprop OwnerEditCompany,fgcolor=red
        else
        setprop OwnerEditCompany,fgcolor=black
        endif
.end patch 1.1
        setitem OwnerEditAddress,0,OWNLOSA
        setitem OwnerEditCity,0,OWNLOCTY
        setitem OwnerEditState,0,OWNLOS
        setitem OwnerEditzip,0,OWNLOZC
        setitem OwnerEditUser,0,OWNPASS
.START PATCH 1.2 ADDED LOGIC
.         SetItem OwnerEditCarbonCopyTo,0,OwnCtn
          call      Trim using OWNCTN
          move      C1,result
.START PATCH 1.2.3 ADDED LOGIC
          if (OWNCTN <>"")
                    pack COMPFLD6,OWNCTN
                    rep zfill,COMPFLD6
                    move C1,COMPPATH
                    move "OwnerLoadList-COMPKEY6",Location
                    pack KeyLocation,COMPFLD6
dave                call COMPKEY6
                    if over
.                             clear COMPNUM
                              clear COMPOLDSVB                        
                    else
                              if (COMPSVBFLG <> "T")
.                                       clear COMPNUM
                                        clear COMPOLDSVB
                              endif
                    endif
          else // OWNCTN = ""
.                   clear COMPNUM
                    clear COMPOLDSVB
          endif
.         move COMPNUM,STR6
          Move COMPOLDSVB,str4
.END PATCH 1.2.3 ADDED LOGIC
          for N9,C1,ComboNum
                    getitem   OwnerComboCC,N9,taskname
.START PATCH 1.2.3 REPLACED LOGIC
.                   unpack    taskname,NFULCOMP,NFULNUM
.                   if (NFULNUM = OWNCTN)
.                   unpack    taskname,COMPCOMP,COMPNUM
                    unpack    taskname,COMPCOMP,COMPOLDSVB
                    if (COMPOLDSVB = str4)
.END PATCH 1.2.3 REPLACED LOGIC
                              move      N9,result
                              break
                    endif
          repeat
          setitem   OwnerComboCC,0,result
.END PATCH 1.2 ADDED LOGIC
.Format Date
        clear   olddate
        count   N2,OwnRdte
        if (N2 = "8")
                unpack  OwnRdte,MM,DD,CC,YY
                pack    olddate,MM,slash,DD,slash,CC,YY
        endif
        setitem OwnerEditDate,0,olddate
        clear   Newnum
        count   N2,OwnTELE
        if (N2 = "10")
                unpack  OwnTELE,area,str3,str4
                pack    Newnum,"(",area,")",str3,Dash,str4
                setprop OwnerEditTelephone,maxchars=13
        else
                setprop OwnerEditTelephone,maxchars=10
        endif
        setitem OwnerEditTelephone,0,Newnum
.
        clear   Newnum
        count   N2,OwnFAX
        if (N2 = "10")
                unpack  OwnFAX,area,str3,str4
                pack    Newnum,"(",area,")",str3,Dash,str4
                setprop OwnerEditFax,maxchars=13
        else
                setprop OwnerEditFax,maxchars=10
        endif
        setitem OwnerEditFax,0,Newnum

        clear   Newnum
        count   N2,OwnFAX2
        if (N2 = "10")
                unpack  OwnFAX2,area,str3,str4
                pack    Newnum,"(",area,")",str3,Dash,str4
                setprop OwnerEditFax2,maxchars=13
        else
                setprop OwnerEditFax2,maxchars=10
        endif
        setitem OwnerEditFax2,0,Newnum
.
         cmatch    "I" to ownstat
         if        equal
         Setitem   OwnerCheckStatus,0,1
         else
         Setitem   OwnerCheckStatus,0,0
         endif

         cmatch    true to owngally
         if        equal
         Setitem   OwnerCheckGalley,0,1
         else
         Setitem   OwnerCheckGalley,0,0
         endif
.START PATCH 1.2.2 ADDED LOGIC
          move      C0,N1
          move      OWNBLK,N1
          setitem OwnerCheckSamples,0,N1
.END PATCH 1.2.2 ADDED LOGIC
          call      debug
          move      C0,N1
          if        (OwnCopy = "" or Owncopy = " " or owncopy = "Y")
          move      c1,N1
          Elseif    (owncopy = "N")
          move      C0,N1
          endif
          setitem OwnerCheckCopy,0,N1

.begin patch 1.28
           call       debug
           if         (OwnNoShp = "Y")
           setItem    OwnerCheckNoShp,0,1
           else
           setItem    OwnerCheckNoShp,0,0
           endif
.end patch 1.28

          SetItem OwnerEditTextEmail,0,OwnEmail
          move      c0,result
          MOve      OwnTranFlag,result
          add       c1,result
          setitem   OwnerComboVia,0,result
          
        setprop OwnerModify,default=1
.begin patch 1.27  
        setprop OwnerEmail,default=1
.end patch 1.27
        packkey NOwnFLD,ownlon
        packkey NOwnFLD2,"01X",ownlonm,OWNOCPY
.begin patch 1.2.6        
                    setitem   OwnerEditText001,0,OwnCOmp
                    setitem   OwnerEditText002,0,Owncont
                    setitem   OwnerEditText003,0,""
                    setitem   OwnerEditText004,0,""
          if        (owncomp <> "000000" & Owncomp <> "")
          packkey   compfld,owncomp
          call      compkey
                    if        not over
                    setitem   OwnerEditText003,0,compcomp
                    endif
          Packkey   CNCTFLD,OWncomp,owncont
          call      CNCTKEY
                    if        not over
                    setitem   OwnerEditText004,0,CNCTFNAME
                    endif
          endif     
.end patch 1.2.6        
        return

.Disable Upper Screen
OwnerDisableUpper
        setprop OwnerSearchKey,enabled=0
        setprop OwnerOK,enabled=0
        setprop OwnerExit,enabled=0
.        setprop OwnerPrint,enabled=0
        setprop OwnerNew,enabled=0
        setprop OwnerSearchList,enabled=0,bgcolor=grey
        return

.Enable Upper Screen
OwnerEnableUpper
.Allow Exit
        move    "Y",ExitFlag
        setprop OwnerSearchKey,enabled=1
        setprop OwnerOK,enabled=1
        setprop OwnerExit,enabled=1
.begin patch 1.27  
        setprop OwnerEmail,Enabled=1
        setprop OwnerEmail,Visible=1
.end patch 1.27
.        setprop OwnerPrint,enabled=1
        setprop OwnerNew,enabled=1
        setprop OwnerSearchList,enabled=1,bgcolor=white
        return

.Disable Lower Screen
OwnerDisableLower
        setprop OwnerEditAddress,enabled=0,bgcolor=grey
        setprop OwnerEditCity,enabled=0,bgcolor=grey
        setprop OwnerEditContact,enabled=0,bgcolor=grey
        setprop OwnerEditCompany,enabled=1,bgcolor=grey
        if      (payto0 = c1)
        setprop OwnerEditCompany,fgcolor=red
        else
        setprop OwnerEditCompany,fgcolor=black
        endif
        setprop OwnerEditDate,enabled=0,bgcolor=grey
        setprop OwnerEditFax,enabled=0,bgcolor=grey
        setprop OwnerEditFax2,enabled=0,bgcolor=grey
        setprop OwnerEditNumber,enabled=0,bgcolor=grey
        setprop OwnerEditState,enabled=0,bgcolor=grey
        setprop OwnerEditTelephone,enabled=0,bgcolor=grey
        setprop OwnerEditZip,enabled=0,bgcolor=grey
        setprop OwnerEditCountry,enabled=0,bgcolor=grey
        setprop OwnerCheckGalley,enabled=0,bgcolor=grey
        setprop OwnerComboVia,enabled=0,bgcolor=grey
        setprop OwnerEditTextEmail,enabled=0,bgcolor=grey
.begin patch 1.2.6
          Setprop   OwnerEditText001,enabled=0,bgcolor=grey
          Setprop   OwnerEditText002,enabled=0,bgcolor=grey
.end patch 1.2.6
        
.START PATCH 1.2.2 ADDED LOGIC
        setprop OwnercheckSamples,enabled=0
.END PATCH 1.2.2 ADDED LOGIC
        setprop OwnercheckCopy,enabled=0
        setprop OwnerCheckStatus,enabled=0,bgcolor=grey
        setprop OwnerEditTaxid,enabled=0,bgcolor=grey
.START PATCH 1.2 ADDED LOGIC
.        setprop OwnerEditCarbonCopyTo,enabled=0,bgcolor=grey
         setprop OwnerComboCC,enabled=0,bgcolor=grey
.END PATCH 1.2 ADDED LOGIC
        setprop OwnerEditUser,enabled=0,bgcolor=grey
        setprop OwnerEditDate,enabled=0,bgcolor=grey
        setprop OwnerEditNumber,enabled=0,bgcolor=grey
        setprop OwnerModify,enabled=1
        setprop OwnerCancel,visible=0
.        setprop OwnerUpdate,visible=0
.begin PATCH 1.28
        setprop OwnerCheckNoShp,enabled=0
.end PATCH 1.28
        return

.Enable Lower Screen
OwnerEnableLower
        move    "N",UpdateFlag
        move    "N",ExitFlag
        setprop OwnerEditAddress,enabled=1,bgcolor=white
        setprop OwnerEditCity,enabled=1,bgcolor=white
        setprop OwnerEditContact,enabled=1,bgcolor=white
        setprop OwnerEditCompany,enabled=1,bgcolor=white
        setprop OwnerEditFax,enabled=1,bgcolor=white
        setprop OwnerEditFax2,enabled=1,bgcolor=white
        setprop OwnerEditState,enabled=1,bgcolor=white
        setprop OwnerEditTelephone,enabled=1,bgcolor=white
        setprop OwnerEditZip,enabled=1,bgcolor=white
        setprop OwnerEditCountry,enabled=1,bgcolor=white
        setprop OwnerCheckGalley,enabled=1,bgcolor=white
        setprop OwnerComboVia,enabled=1,bgcolor=White
        setprop OwnerEditTextEmail,enabled=1,bgcolor=White
.begin patch 1.2.6
          Setprop   OwnerEditText001,enabled=1,bgcolor=White
          Setprop   OwnerEditText002,enabled=1,bgcolor=White
.end patch 1.2.6
         setprop OwnerComboCC,enabled=1,bgcolor=White
         
.START PATCH 1.2.2 ADDED LOGIC
        setprop OwnercheckSamples,enabled=1
.END PATCH 1.2.2 ADDED LOGIC
        setprop OwnercheckCopy,enabled=1
        setprop OwnerCheckStatus,enabled=1,bgcolor=white
        setprop OwnerEditTaxid,enabled=1,bgcolor=white
.START PATCH 1.2 ADDED LOGIC
.Start Patch 1.2.4 Disable Object
.        setprop OwnerEditCarbonCopyTo,enabled=1,bgcolor=white
.        setprop OwnerComboCC,enabled=1,bgcolor=white
.End Patch 1.2.4        
.END PATCH 1.2 ADDED LOGIC
        setprop OwnerModify,enabled=0
        setprop OwnerCancel,visible=1
.        setprop OwnerUpdate,visible=1
        setprop OwnerEditUser,enabled=0,bgcolor=grey
        setprop OwnerEditDate,enabled=0,bgcolor=grey
        setprop OwnerEditNumber,enabled=0,bgcolor=grey
.begin PATCH 1.28
        setprop OwnerCheckNoShp,enabled=1
.end PATCH 1.28
        return

OwnerDisableUpdateAlt
.        setitem OwnerUpdate,0,"Update"
        return
OwnerEnableUpdateAlt
.        setitem OwnerUpdate,0,"&Update"
        return

.Verify Data Entry
OwnerVerifyData
        getitem OwnerEditNumber,0,ownlon
        call    TRIM using ownlon
        count   HowMany,ownlon
        if (HowMany < 4 & OWNLON <> "NEW")
                alert caution,"4 Digit Owner Number Required!",result
                setfocus OwnerEditNumber
                move "Y",ReturnFlag
                return
        endif
        setitem OwnerEditNumber,0,ownlon
.
.begin patch 1.2.6
          setitem   OwnerEditText003,0,""
          setitem   OwnerEditText004,0,""
          getitem   OwnerEditText001,0,str6
          call      TRIM using str6
          count     n2,str6
          if        (n2 = 6)
          move      str6,Owncomp
          Elseif    (n2 = 5)
          pack      owncomp from "0",str6
          Elseif    (n2 = 4)
          pack      owncomp from "00",str6
          Elseif    (n2 = 3)
          pack      owncomp from "000",str6
          Elseif    (n2 = 2)
          pack      owncomp from "0000",str6
          Elseif    (n2 = 1)
          pack      owncomp from "00000",str6
          Else
          move      "000000",owncomp
          endif
          getitem   OwnerEditText002,0,str3
          call      TRIM using str3
          count     n2,str3
          if        (n2 = 3)
          move      str3,Owncont
          Elseif    (n2 = 2)
          pack      owncont from "0",str3
          Elseif    (n2 = 1)
          pack      owncont from "00",str3
          else
          move      "000",owncont
          endif
          setitem   OwnerEditText001,0,owncomp
          setitem   OwnerEditText002,0,owncont
          if        (owncomp <> "000000")
          packkey   compfld,owncomp
          call      compkey
                    if        not over
                    setitem   OwnerEditText003,0,compcomp
                    endif
          Packkey   CNCTFLD,OWncomp,owncont
          call      CNCTKEY
                    if        not over
                    setitem   OwnerEditText004,0,CNCTFNAME
                    endif
          endif     
.end patch 1.2.6
        getitem OwnerEditContact,0,OWNLONM
        call    TRIM using OWNLONM
        match   "ATTN ",OWNLONM
        call    OwnerClean if equal
        reset   OWNLONM
        match   "Attn ",OWNLONM
        call    OwnerClean if equal
        reset   OWNLONM
        match   "ATTN: ",OWNLONM
        call    OwnerClean if equal
        reset   OWNLONM
        match   "Attn: ",OWNLONM
        call    OwnerClean if equal
        reset   OWNLONM
        call    TRIM using OWNLONM
        setitem OwnerEditContact,0,OWNLONM
.
        getitem OwnerEditCompany,0,OWNOCPY
        call    TRIM using OWNOCPY
        count   HowMany,OWNOCPY
        if (HowMany = 0)
                alert caution,"Company Name Required!",result
                setfocus OwnerEditCompany
                move "Y",ReturnFlag
                return
        endif
        setitem OwnerEditCompany,0,OWNOCPY
.
        getitem OwnerEditAddress,0,OWNLOSA
        call    TRIM using OWNLOSA
        setitem OwnerEditAddress,0,OWNLOSA
.
        getitem OwnerEditCity,0,OWNLOCTY
        call    TRIM using OWNLOCTY
        setitem OwnerEditCity,0,OWNLOCTY
.
        getitem OwnerEditState,0,OWNLOS
        call    TRIM using OWNLOS
        setitem OwnerEditState,0,OWNLOS
.
        getitem OwnerEditzip,0,OWNLOZC
        call    TRIM using OWNLOZC
        setitem OwnerEditzip,0,OWNLOZC
.
        getitem OwnerEditTaxid,0,OWNtaxid
        call    TRIM using OWNtaxid
        setitem OwnerEdittaxid,0,OWNtaxid
        
          Getitem   OwnerComboVia,0,n2
          if        (n2 = 1)
          Clear     OwnTranFlag
          elseif    (n2 = 2)
          move      c1,OwnTranFlag
          elseif    (n2 = 3)
          move      c2,OwnTranFlag
          endif
.
          Getitem   OwnerEditTextEmail,0,OwnEmail
          call      TRIM using OWNEmail
          squeeze     OwnEmail,OwnEmail
          rep         ";,",OwnEmail
          Setitem   OwnerEditTextEmail,0,OwnEmail

.
.START PATCH 1.2 ADDED LOGIC
.        getitem OwnerEditCarboncopyto,0,OWNctn
.        call    TRIM using OWNCtn
.        setitem OwnerEditCarboncopyto,0,OWNctn
          getitem   OwnerComboCC,0,N5
          if (N5 = 1)
                    clear     OWNCTN
          else
                    getitem   OwnerComboCC,N5,taskname
.START PATCH 1.2.3 REPLACED LOGIC
.                   unpack    taskname,NFULCOMP,NFULNUM
.                   move      NFULNUM,OWNCTN
.         endif
.                   unpack    taskname,COMPCOMP,COMPNUM
                    unpack    taskname,COMPCOMP,OWNCTN
.                   move      COMPNUM,OWNCTN
          endif
.END PATCH 1.2.3 REPLACED LOGIC
.END PATCH 1.2 ADDED LOGIC
.
        move    NPASuser,OWNPASS
        setitem OwnerEditUser,0,OWNPASS
.
        if (UpdateFlag = "Y")
                unpack newdate,MM,STR1,DD,STR1,CC,YY
        else
                unpack olddate,MM,STR1,DD,STR1,CC,YY
        endif
        pack    OwnRdte,mm,dd,CC,YY
        pack    str10,MM,SLASH,DD,SLASH,CC,YY
        setitem OwnerEditDate,0,str10
.
        clear   Newnum
        getitem OwnerEditTelephone,0,HowMany
        getitem OwnerEditTelephone,0,Newnum
        if (HowMany <> 0)
                if (HowMany = 13)
                        unpack  Newnum,str1,area,str1,str3,str1,str4
                        pack    Newnum,area,str3,str4
                elseif (HowMany <> 10)
                        alert   caution,"10 Digit Number Only!",result
                        setfocus OwnerEditTelephone
                        move    "Y",ReturnFlag
                        return
                else
                        match "0000000000",Newnum
                        if equal
                                clear Newnum
                        endif
                endif
        endif
        move    Newnum,ownTELE
        count   N9,Newnum
        if (N9 > "0")
                unpack  Newnum,area,str3,str4
                pack    Newnum,"(",area,")",str3,dash,str4
        endif
        setitem OwnerEditTelephone,0,Newnum
.
        clear   Newnum
        getitem OwnerEditFax,0,HowMany
        getitem OwnerEditFax,0,Newnum
        if (HowMany = 0)
                alert caution,"Fax Number Must Be Entered!",result
                setfocus OwnerEditFax
                move "Y",ReturnFlag
                return
        else
                if (HowMany = 13)
                        unpack  Newnum,str1,area,str1,str3,str1,str4
                        pack    Newnum,area,str3,str4
                elseif (HowMany <> 10)
                        alert caution,"10 Digit Number Only!",result
                        setfocus OwnerEditFax
                        move "Y",ReturnFlag
                        return
                endif
        endif
        move    Newnum,ownFAX
        count   N9,Newnum
        if (N9 > "0")
                unpack  Newnum,area,str3,str4
                pack    Newnum,"(",area,")",str3,dash,str4
        endif
        setitem OwnerEditFax,0,Newnum
.
        clear   Newnum
        getitem OwnerEditFax2,0,HowMany
        getitem OwnerEditFax2,0,Newnum
                if (HowMany = 13)
                        unpack  Newnum,str1,area,str1,str3,str1,str4
                        pack    Newnum,area,str3,str4
                elseif (HowMany <> 10 & HowMany <> 0)
                        alert caution,"10 Digit Number Only!",result
                        setfocus OwnerEditFax2
                        move "Y",ReturnFlag
                        return
                endif
        move    Newnum,ownFAX2
        count   N9,Newnum
        if (N9 > "0")
                unpack  Newnum,area,str3,str4
                pack    Newnum,"(",area,")",str3,dash,str4
        endif
        setitem OwnerEditFax2,0,Newnum
.
        getitem   OwnerCheckGalley,0,n1
        if        (n1 = c1)
        move      "T" to OwnGally
        else
        clear     owngally
        endif
.
        getitem   OwnerCheckStatus,0,n1
        if        (n1 = c1)
        move      "I" to OwnStat
        else
        clear     ownStat
        endif
        move      "B" to Ownloc
.START PATCH 1.2.2 REPLACED LOGIC
.        move      c0 to ownblk
          getitem OwnercheckSamples,0,n1
          move      N1,ownblk
.END PATCH 1.2.2 REPLACED LOGIC
          call      debug
          getitem OwnercheckCopy,0,n1
          if        (n1 = c1)
          move      Yes,ownCopy
          else
          move      No,Owncopy
          endif
.begin patch 1.28
           GetItem    OwnerCheckNoShp,0,N1
           if         (N1 = c1)
           move       "Y",OwnNoShp
           else
           move       b1,OwnNoShp
           endif
.end patch 1.28
       return
.
RefreshOwnerSearchList
.Clear DataList
        deleteitem OwnerSearchList,0
.First Test for Blank Entry After "?"
                  count    result,key
        if (result = "1")
                setprop ErrorMssg,visible=1
                setfocus OwnerSearchKey
                setprop OwnerOK,enabled=1
                return
        elseif (result < 3)
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
                setitem ErrorMssgStat5,0,"  Name Must Have 3+ Characters"
                setprop ErrorMssg,visible=1
                call    SetOwnerErrorMssgDefault
                setfocus OwnerSearchKey
                setprop OwnerOK,enabled=1
                return
        endif
        setprop OwnerSearchList,enabled=1,bgcolor=white
        pack    NOwnFLD2,AKey1,key
        move    "Driver-NOwnAIM",Location
        call    NOwnAIM
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

                setprop OwnerOK,enabled=1
                setfocus OwnerSearchKey
                return
        else
                move "Y",AamFlag
                pack hold,OWNLOC,OWNBLK,OWNLON,OWNLONM,OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC:
                                OWNNEC,OWNCTN,OWNTELE,OWNPASS,OWNRDTE,owngally,OWNTAXID,OWNFAX,ownfax2:
.begin patch 1.2.6
.                               OWNstat,OwnEmail,OwnTranFlag
.begin patch 1.28
.                                OWNstat,OwnEmail,OwnTranFlag,Owncomp,Owncont,OWncopy
                                OWNstat,OwnEmail,OwnTranFlag,Owncomp,Owncont,OWncopy,OwnNoSHP
.end patch 1.28
.end patch 1.2.6
                insertitem OwnerSearchList,9999,hold


        endif
        move    "Driver-NOwnKG",Location
        loop
                call NOwnKG
                if not over
                pack hold,OWNLOC,OWNBLK,OWNLON,OWNLONM,OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC:
                                OWNNEC,OWNCTN,OWNTELE,OWNPASS,OWNRDTE,owngally,OWNTAXID,OWNFAX,ownfax2:
.begin patch 1.2.6
.                               OWNstat,OwnEmail,OwnTranFlag
.begin patch 1.28
.                                OWNstat,OwnEmail,OwnTranFlag,Owncomp,Owncont,OWncopy
                                OWNstat,OwnEmail,OwnTranFlag,Owncomp,Owncont,OWncopy,OwnNoSHP
.end patch 1.28
.end patch 1.2.6
                        insertitem OwnerSearchList,9999,hold
                endif
                until over
        repeat
.Put focus on first item in DataList
        setitem OwnerSearchList,1,1
.Load the Inquiry Screen immediately
        call OwnerData
        return
RefreshOwnerIndexSearchList
.Clear DataList
        deleteitem OwnerSearchList,0
        count   HowMany,key
        if (HowMany > "7")
                setprop ErrorMssg,visible=1
                setprop OwnerOK,enabled=1
                setprop OwnerExit,enabled=1
                setprop OwnerNew,enabled=1
                setprop OwnerModify,enabled=0
                setfocus OwnerSearchKey
                return
        endif
.Pack number with preceding zeroes
        if (HowMany < "4")
                sub HowMany from "4" giving N1
                setlptr filler2, N1
                pack NOwnFLD,filler2,key
                move NOwnFLD,key
        endif
        setprop OwnerSearchList,enabled=0,bgcolor=grey
        setitem OwnerSearchKey,0,key
        pack    NOwnFLD,key
        move    "OK-1rst NOwnKEY",Location
        call    NOwnKEY
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
                call    SetOwnerErrorMssgDefault
                setfocus OwnerSearchKey
        else
                pack    hold,ownvars
                clear   result
                clear   HowMany
                unpack  key,holdOwn
                pack    NOwnFLD,holdOwn
                move    "OK-2nd NOwnKEY",Location
                call    NOwnKEY
                if Over
                        unpack  hold,ownvars
                        call    OwnerLoadList
                        setprop OwnerModify,enabled=1
                        setfocus OwnerModify
                else
                        add C1,result
                        add C1,HowMany
                        setprop OwnerSearchList,enabled=1,bgcolor=white
                        deleteitem OwnerSearchList,0
                          pack hold,OWNLOC,OWNBLK,OWNLON,OWNLONM,OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC:
                                OWNNEC,OWNCTN,OWNTELE,OWNPASS,OWNRDTE,owngally,OWNTAXID,OWNFAX,ownfax2:
.begin patch 1.2.6
.                               OWNstat,OwnEmail,OwnTranFlag
.begin patch 1.28
.                                OWNstat,OwnEmail,OwnTranFlag,Owncomp,Owncont,OWncopy
                                OWNstat,OwnEmail,OwnTranFlag,Owncomp,Owncont,OWncopy,OwnNoSHP
.end patch 1.28
.end patch 1.2.6
                        insertitem OwnerSearchList,9999,hold
                        move    "OK-NOwnKS",Location
                        loop
                                call NOwnKS
                                until (holdOwn <> ownlon)
                                if not over
                                       add C1,HowMany
                                    pack hold,OWNLOC,OWNBLK,OWNLON,OWNLONM,OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC:
                                          OWNNEC,OWNCTN,OWNTELE,OWNPASS,OWNRDTE,owngally,OWNTAXID,OWNFAX,ownfax2:
.begin patch 1.2.6
.                               OWNstat,OwnEmail,OwnTranFlag
.begin patch 1.28
.                                OWNstat,OwnEmail,OwnTranFlag,Owncomp,Owncont,OWncopy
                                OWNstat,OwnEmail,OwnTranFlag,Owncomp,Owncont,OWncopy,OwnNoSHP
.end patch 1.28
.end patch 1.2.6
                                  insertitem OwnerSearchList,9999,hold
                                endif
                                until over
                        repeat
.Put focus on SEARCH item in DataList if more than 1 record was found
                        setitem OwnerSearchList,1,result
                        call OwnerData
                endif
        endif
        return
OwnerClean
.Extracts "ATTN" and "Attn" from string
        clear   str55
        clear   N9
        reset   OWNLONM
        loop
                add "1",N9
                reset OWNLONM,N9
                cmatch B1,OWNLONM
                until equal
                until eos
        repeat
        move    OWNLONM,str55
        clear   OWNLONM
        reset   str55
        move    str55,OWNLONM
        return
OwnerData
          getitem   OwnerSearchList,0,result
          getitem   OwnerSearchList,result,hold
          if (AamFlag = "Y")
                Unpack hold,OWNLOC,OWNBLK,OWNLON,OWNLONM,OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC:
                                OWNNEC,OWNCTN,OWNTELE,OWNPASS,OWNRDTE,owngally,OWNTAXID,OWNFAX,ownfax2:
.begin patch 1.2.6
.                               OWNstat,OwnEmail,OwnTranFlag
.begin patch 1.28
.                                OWNstat,OwnEmail,OwnTranFlag,Owncomp,Owncont,OWncopy
                                OWNstat,OwnEmail,OwnTranFlag,Owncomp,Owncont,OWncopy,OwnNoSHP
.end patch 1.28
.end patch 1.2.6
          else
                Unpack hold,OWNLOC,OWNBLK,OWNLON,OWNLONM,OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC:
                                OWNNEC,OWNCTN,OWNTELE,OWNPASS,OWNRDTE,owngally,OWNTAXID,OWNFAX,ownfax2:
.begin patch 1.2.6
.                               OWNstat,OwnEmail,OwnTranFlag
.begin patch 1.28
.                                OWNstat,OwnEmail,OwnTranFlag,Owncomp,Owncont,OWncopy
                                OWNstat,OwnEmail,OwnTranFlag,Owncomp,Owncont,OWncopy,OwnNoSHP
.end patch 1.28
.end patch 1.2.6
          endif
          call      OwnerLoadList
          setfocus OwnerSearchList
          setprop OwnerModify,enabled=1
        return
.
OwnerGetNext
.Locate and Display next logical Owner Number
        move    "NOWNNXT",GNXTFLD
        call    GNXTKEY
        if over
.Change StatText Boxes For Error Message
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
                setitem ErrorMssgStat5,0,"  Error Getting Owner ID Number!"
                setitem ErrorMssgOK,0,"&Stop"
.Display Error Message
                setprop ErrorMssg,visible=1
.Reset StatText Boxes
                call    SetOwnerErrorMssgDefault
.Terminate Program
                stop
        endif
        bump    GNXTNUM,2
        move    GNXTNUM,newOwn
        loop
                add     C1,newOwn
                move    newOwn,ownlon
                rep     zfill,ownlon
                pack    NOwnFLD,ownlon,filler
                rep     zfill,NOwnFLD
                call    NOwnTST
                until over
        repeat
.Following code used for testing purposes- leave here!!!!!
.       clear   GNXTNUM
.       move    "0545",NEWOwn
.       append  B2,GNXTNUM
.       append  NEWOwn,GNXTNUM
.       reset   GNXTNUM
.       rep     zfill,GNXTNUM
.       call    GNXTUPD
        return

XRESIZE
           Nown0001.Scale
           RETURN

.Include IO files
.begin release 1.1
        include npayio.inc
.end release 1.1
        include comlogic.inc
        include nOwnio.inc
        include npasio.inc
        include gnxtio.inc
.START PATCH 1.2.3 REPLACED LOGIC
..START PATCH 1.2 ADDED LOGIC
.         include   nfulio.inc
          include compio.inc
          include cntio.inc
           include    ntypio.inc
          include Compnotesio.inc                 
          
..END PATCH 1.2 ADDED LOGIC
.END PATCH 1.2.3 REPLACED LOGIC
