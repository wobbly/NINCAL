PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         include   npasdd.inc
         INCLUDE   NMRGDD.inc
           INCLUDE   NORDDD.INC
.Patch5.88
                              include   compdd.inc
                              include   cntdd.inc
.           include   nmlrdd.inc
.Patch5.88
         INCLUDE   HP.INC
         INCLUDE   NOWNDD.INC
         include   nusedd.inc
           INCLUDE   NRTNDD.INC
         INCLUDE   NSPEDD.INC
         INCLUDE   NSPIDD.INC
         INCLUDE   MEDIA.INC
.         INCLUDE   NBRKDD.INC
         INCLUDE   OSLSPERN.INC
         INCLUDE   SHIPPING.INC
.START PATCH 5.83 REPLACED LOGIC
.         INCLUDE   CONTACT1.INC
         INCLUDE   NCNTDD.INC
.END PATCH 5.83 REPLACED LOGIC
.begin patch 1.3
.         include   ninvdd.inc
          include             ninvdd.inc
.end patch 1.3
.START PATCH 1.61 REMOVED LOGIC
.        include NFULDD.inc
.START PATCH 1.61 REMOVED LOGIC
         include   winapi.inc
.
RELEASE   INIT      "2.03"          DLH display House Hits
Reldate   Init      "2013 November 13"
.RELEASE   INIT      "2.02"          DLH Fix Nmrg002c.plf display issues
..                                   required rebuilding nmrg0001.plf from scratch  close_nmrg0001 was missing and would not add.
.Reldate   Init      "2013 August 22"
.RELEASE   INIT      "2.01"          DLH Fix timer, add exit to red x button on plf
..                                   required rebuilding nmrg0001.plf from scratch  close_nmrg0001 was missing and would not add.
.Reldate   Init      "2013 July 29"
.RELEASE   INIT      "2.00"          DLH Sunbelt PDF
.Reldate   Init      "2013 April 23"
.RELEASE   INIT      "1.92"          DLH call Nmrg0002 directly, eliminate Butil
.Reldate   Init      "15 Jan 2013"
.RELEASE   INIT      "1.91"          DLH run butil locally
.Reldate   Init      "08 Jan 2013"
.RELEASE   INIT      "1.9"          DLH allow modify/entry of customer suppress and unused multies
.Reldate   Init      "22 May 2012"
.RELEASE   INIT      "1.8"          DLH cleanup of fax code to use pdf & email
.Reldate   Init      "05 May 2011"
.RELEASE   INIT                "1.7"          JD new merge var CNR
.Reldate       Init           "17 Sep 2007"
.RELEASE            INIT                "1.62"        DLH PLI
.RELEASE            INIT                "1.61"        DMS 22JUN2006 Fulfillment Conversion
.RELEASE            INIT                "1.5"        Total rejects calced for running total (add mode)
.RELEASE            INIT                "1.4"        JD  15OCT2005  Timer.
.RELEASE            INIT                "1.3"        DLH 10March2005  Invoice Conversion
.Reldate       Init           "10 March 2005"
.RELEASE  INIT      "1.2"        JD Dec2004  Turned off DPV deduct.
.RELEASE  INIT      "1.1"       JD Sep2004  Only enter password 1 in add mode.
.RELEASE  INIT      "1.0"      JD Sep2004
.Reldate        Init           "September 2004"
.begin patch 1.8
FileCheck FIle
trapcount form      4
.end patch 1.8

PrintHotcv   external "NMRG0002;PrintCV"


editTextBox         editText
F94        FORM      9.4
F32        FORM      3.2
V1       FORM      2
V2       FORM      2
SAVE     DIM       47
CORP     DIM       1
SPCL1    DIM      2
SPCL2    DIM      2
SPCL3    DIM      2
SPCL4    DIM      2
SPCL5    DIM      2
SPCL6    DIM      2
SPCL7    DIM      2
SPCL8    DIM      2
SPCL9    DIM      2
CONTACT  DIM      25
PERCENT  FORM      4.2
CALCPER  FORM      7.4
DATE     DIM       8
RTNTAB   FORM      3
TOTREJ   FORM      8
TOTBILL  FORM      8
recname  dim       50
LONGDIST DIM       1
FORMFILE FILE      UNCOMP
FAXBAT   FILE
FORMNAME DIM       30
TOTNCOA  FORM      8
.Start Patch 1.5 added
mergerej form      8
mergetmp form      8
.End patch 1.5 added
.Start Patch #5.5 - remmed and replaced var - duplicate in cons.inc
.THIS IS ONLY PATCH NOTIFICATION - LOOK FOR NEW VAR FOR ALL OCCURANCES!!!
.location form      2
location2 form      2
.End Patch #5.5 - remmed and replaced var - duplicate in cons.inc
SYSMO    DIM       2             current
SYSDAY   DIM       2
SYSYR    DIM       2
TIME     INIT      "HH:MM:SS"
userinfo dim       500
userlogn dim       7
userlogw dim       7
febdat  form      5
feb      dim       1
scrn1    dim       15000         dlh- sunbelt 05mar96
newdate dim     10
HOTFLAG  FORM       1                 "1=daily print, 2=hot print"
HoldFlag init   "N"
HoldFlg2 init   "N"
tabnum   form      1
badfaxflag dim      1
ExitFlag init   "Y"
ExitFlag4 dim       %1
          move      "Y",ExitFlag4
hold      dim       500
Timer   Timer
ReturnFlag init "N"
UpdateFlag init "N"
NewFlag2 init       "N"       .Used to determine which buttons are enabled.
AamFlag init    "N"
NewFlag init    "N"
Modflag init    "N"
.Used to test if Aamdex needs to be updated
.CompFlag init   "N"
CompChg  init   "N"
.Set Up Menu Bar
mFile    menu
mEdit    menu
mHelp    menu
mOptions menu
.Set Up SubMenu for Options
sColor  submenu


.Present Data for Colors SubMenu
CData   init    ";&Background;&Text"



.Define Colors for Each Object
FTC     color
BGC     color
white     color
grey  color

colornum dim   9(2)
Fred     form    3
Fgreen   form    3
Fblue    form    3
colorflag form   1
prfile  pfile
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
font1   font
font2   font
font3   font
font4   font
font5   font
.....
font6   font
.begin patch 1.62
FontO7    font
FontO18B  font

          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold
.end patch 1.62

        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
.END PATCH 1.83 REPLACED LOGIC
......
        create  font6,"Arial",size=14
REP      DIM       1
odate   dim        10
.hexeight integer 4,"4294967295"
.................................
.coll1   collection
...............................................................................

.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Color"
HData   init    "&Help;&About"


.Create Colors for EditText Inquiry
          create    white=*white
.................................
.Set Vars used for About Box
        move    "Nmrg0001.PLS",Wprognme
        move    "Merge File Maintenance",Wfunction
        move    "Jose Dueñas",Wauthor
        move    Release,Wrelease
        Move    Reldate to Wreldate

                      move    c1 to ninvpath
.Declare forms, Always declare child forms first
.rpt     plform  Report
mss1    plform  Error
pss     plform  Passwrd
abt     plform  About
Merge2  plform  NMRG001a
Merge3  plform  NMRG001c
x       plform  NMRG0001
        winhide
.Load Forms, Always load parent form first
        formload x
        formload Merge3,NMRG0001
        formload Merge2,NMRG0001
        formload abt
        formload pss
        formload mss1
.        formload rpt

        cREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        create  NMRG0001;mFile,FData
        create  NMRG0001;mEdit,EData,mFile
        create  NMRG0001;mOptions,OData,mEdit
        create  NMRG0001;mHelp,HData,mOptions
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
.        create  white=*white
        create  grey=*ltgray
.        create  RED=*RED
.        create  black=*black

        move    "N",PassFlag
.Main Loop
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate,MM,SLASH,DD,SLASH,CC,YY
        OPEN      NMRGFLE1,NMRGNME1
.Set Error Message Stat Text Boxes
.        call    SetOwnerErrorMssgDefault
.Set Flags to Open NINOwn.DAT
        move    C0,NOwnFLG1
        move    C0,NOwnFLG2
.Set tab index
        move    C2,TabNum
.START PATCH 1.2 ADDED LOGIC
        setfocus Mergelr
.Create MergeSearchListView Columns
.Column Clicking
.Check out notes under Order3ListView_ColumnClick for other options.
          MergeSearchListView.InsertColumn using "LR ##",50,1
          MergeSearchlistView.InsertColumn using "Mailer",100,2
          MergeSearchlistView.InsertColumn using "vars",0,3
.
.Create MergeListView Columns
.Column Clicking
.Check out notes under Order3ListView_ColumnClick for other options.
          MergeListView.InsertColumn using "Name",80,1
          MergelistView.InsertColumn using "Merge Deducts",100,2
          call Get64OS
.
Mergetab
        if (n1 = c1)
.        move  c2 to tabnum
         activate    merge3
         deactivate  merge2
.        setfocus stattext014
         elseif (TabNum = c2)
         activate    merge2
         deactivate  merge3
.         setfocus Mergelr
                endif
.
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT
.        setfocus stattext014
           EVENTREG  X, 17, XRESIZE

    loop
                waitevent
.Start Patch #1.4 - timer logic
                deactivate TIMER
                ACTIVATE TIMER,Timeout,RESULT
.End Patch #1.4 - timer logic
        repeat
MergeLoad
.Load LR information
.         call     MergeSearchLoadlist
.         getitem   MergelR,0,nmrglr
          pack   nmrgfld from olrn
          call     nmrgkey
.         return   if over
          if       over
           call     mergeclearlower
           call     mergecleardeducts
           call     mergedisablebuttons
          alert     note,"That Merge Record Does Not Exist!",result
          setprop   MergeADD,enabled=1
.         setprop   MergeMod,enabled=0
.   alert plain,"Would you like to add?",result
.         cmatch    yes to str1
.         if (result <> 1)
.                   setfocus mergeLR
.                   else
.                   call     add
.                   setfocus  mergesearchloadlist
.                   endif
    return
          endif
         MOVE        ORTNNUM TO NRTNFLD
           CALL      NRTNKEY
           pack      mkey from omlrnum,ocobn
           call      nmlrkey
         move      nmrgfld to ninvfld
         rep       zfill in ninvfld
         call      ninvkey
         MOVE      invdtem TO MM
         MOVE      invdted TO DD
         MOVE      invdtey TO YY
         CALL      CVTJUL
         compare   juldays to febdat
         goto      calcstf if equal
         if        not less
         move      yes to feb
         endif
calcstf   call      calcs
          move      YES,NewFlag2        .Default
          setitem   MergeLR,0,OLRN
          setitem   Mergelistinfo,0,nmrglnam
.         setitem   MergeOrderdate,0,str10
          setitem   MergeOrderdate,0,odate
          setitem   Mergekey,0,nmrgkcod
          setitem   Mergeshipto,0,rtcomp
          setitem   Mergedatt2,0,rtcomp
          setitem   Mergemlredit,0,mcomp
          setitem   Mergestat50,0,mcomp
          setitem   Mergemasterlist,0,o1des
          setitem   Mergemp,0,omlrky
          move     nmrgrqty to str10
          setitem   Mergeinputqty,0,str10
          move     nmrgnet to str10
          setitem   Mergenetoutput,0,str10
          move     totbill to str10
          setitem   Mergetotbill,0,str10
          move     percent,str10
          setitem   Mergenet,0,str10
          move     totrej,str10
          setitem   Mergetotrej,0,str10
.begin patch 2.02
          setitem   mergerejtot,0,str10
.end patch 2.02
          call     MergeLoadlistView
          move     nmrgcs,str10
          setitem   Mergecusts,0,str10
.         move     nmrgcust,str10
.         setitem   Mergecustr,0,str10
          move     nmrgelmx,str10
          setitem   MergeCNR,0,str10
          move     nmrgdisf,str10
          setitem   Mergedead,0,str10
          move     nmrgdma,str10
          setitem   Mergedma,0,str10
          move     nmrgelim,str10
          setitem   Mergeelim,0,str10
          move     nmrgerr,str10
          setitem   Mergeerr,0,str10
          move     nmrgfam,str10
          setitem   Mergefamily,0,str10
          move     nmrghdrp,str10
          setitem   Mergehd,0,str10
.
          move     NMRGHH,str10
          setitem   MergehH,0,str10
.

          move     nmrgid,str10
          setitem   Mergeintra,0,str10
          move     nmrgdrop,str10
          setitem   Mergedrop,0,str10
          move     nmrgdupm,str10
          setitem   Mergedupm,0,str10
          move     totncoa,str10
          setitem   Mergencoa,0,str10
          move     ncoamnf,str10
          setitem   Mergemnf,0,str10
          move     ncoamwf,str10
          setitem   Mergemwf,0,str10
          move     ncoanfrj,str10
          setitem   Mergenfrj,0,str10
          move     ncoanix1,str10
          setitem   Mergenix1,0,str10
          move     ncoanix2,str10
          setitem   Mergenix2,0,str10
          move     ncoanix3,str10
          setitem   Mergenix3,0,str10
          move     ncoatotm,str10
          setitem   Mergencoatot,0,str10
          move     ncoaunm,str10
          setitem   Mergencoaunm,0,str10
          move     nmrgnix,str10
          setitem   Mergenix,0,str10
          move     nixiem,str10
          setitem   Mergenixem,0,str10
          move     nmrgnd,str10
          setitem   Mergend,0,str10
          move     nmrgnper,str10
          setitem   Mergenonp,0,str10
          move     nmrgpris,str10
          setitem   Mergeprison,0,str10
          move     nmrgrep,str10
          setitem   Mergerep,0,str10
          move     nmrgtdmc,str10
          setitem   Mergetdmc,0,str10
          move     nmrgzip4,str10
          setitem   Mergez4crt,0,str10
          move     nmrgz4,str10
          setitem   Mergez4rej,0,str10
          move     nmrgzipc,str10
          setitem   Mergezipcor,0,str10
          move     nmrgzipv,str10
          setitem   Mergezipver,0,str10
          move     nmrgdpv,str10
          setitem   MergeDPV,0,str10
          move     nmrgconv,str10
          setitem   Mergeconv,0,str10
          move     nmrgudup,str10
          setitem   Mergeunused,0,str10
          move     nmrgdisa,str10
          setitem   Mergedisa,0,str10
          move     nmrgcnr,str10
          setitem   MergeCRN,0,str10

 return
MergeretrieveLR
.Load LR information
          move      C1,NORDPATH
          call      NORDKEY
          if over
           call     mergeclearlower
           call     mergecleardeducts
          alert     note,"That Order Record Does Not Exist!",result
                    setfocus mergeLR
                    return
                    else
                    call   mergesearchloadlist
                    return
  endif
.
MergeLoadListView
.                   move       c1 to mergen
.                   call       checkrej
          COMPARE   C0 TO nmrgid
          if not equal
                    MergeListView.InsertItem giving N9 using "Intra Dupes"
                    move       nmrgid to str10
                    MergeListView.SetItemText using N9,str10,1
          endif
          COMPARE   C0 TO nmrgerr
          if not equal
                    MergeListView.InsertItem giving N9 using "Errors"
                    move      nmrgerr to str10
                    MergeListView.SetItemText using N9,str10,1
          endif
          COMPARE   C0 TO nmrgdisf
          if not equal
                    MergeListView.InsertItem giving N9 using "Deceased"
                    move      nmrgdisf to str10
                    MergeListView.SetItemText using N9,str10,1
          endif
          COMPARE   C0 TO nmrgnper
          if not equal
                    MergeListView.SetItemText using N9,str10,1
                    MergeListView.InsertItem giving N9 using "Non Person"
                    move      nmrgnper to str10
                    MergeListView.SetItemText using N9,str10,1
          endif
          COMPARE   C0 TO nmrgdma
          if not equal
                    MergeListView.InsertItem giving N9 using "DMA"
                    move      nmrgdma to str10
                    MergeListView.SetItemText using N9,str10,1
          endif
          COMPARE   C0 TO nmrgz4
          if not equal
                    MergeListView.InsertItem giving N9 using "Zip +4"
                    move      nmrgz4 to str10
                    MergeListView.SetItemText using N9,str10,1
          endif
          COMPARE   C0 TO nmrgpris
          if not equal
                    move      nmrgpris to str10
                    MergeListView.InsertItem giving N9 using "Prison Rej"
.                             move      nmrgpris to str10
   MergeListView.SetItemText using N9,str10,1
                              endif
         COMPARE    C0 TO ncoamnf
                              if         not equal
   MergeListView.SetItemText using N9,str10,1
                    MergeListView.InsertItem giving N9 using "NCOA Mtch NON FWD"
                              move      ncoaMNF to str10
   MergeListView.SetItemText using N9,str10,1
                endif
.         COMPARE    C0 TO nmrgcust
.                             if         not equal
.                   MergeListView.InsertItem giving N9 using "Customer"
.                             move      nmrgcust to str10
.   MergeListView.SetItemText using N9,str10,1
.               endif
         COMPARE    C0 TO nmrgdpv
                              if         not equal
                    MergeListView.InsertItem giving N9 using "DPV Drops"
                              move      nmrgdpv to str10
   MergeListView.SetItemText using N9,str10,1
                endif
         COMPARE    C0 TO nmrgconv
                              if         not equal
                    MergeListView.InsertItem giving N9 using "Conversion Drops"
                              move      nmrgconv to str10
   MergeListView.SetItemText using N9,str10,1
                endif
         COMPARE    C0 TO nmrgdisa
                              if         not equal
                    MergeListView.InsertItem giving N9 using "Disaster Drops"
                              move      nmrgdisa to str10
   MergeListView.SetItemText using N9,str10,1
                endif
         COMPARE    C0 TO nmrgcnr
                              if         not equal
                    MergeListView.InsertItem giving N9 using "CNR Matches"
                              move      nmrgcnr to str10
   MergeListView.SetItemText using N9,str10,1
                endif
.         MergeListView.InsertItem giving N9 using mergen1
.   MergeListView.SetItemText using N9,str10,1
.         MergeListView.InsertItem giving N9 using mergen1
.   MergeListView.SetItemText using N9,str10,1
       return
MergeSearchLoadList
                    MergeSearchListView.InsertItem giving N9 using Olrn
             MergeSearchListView.SetItemText using N9,OMLRnum,1
                    pack     hold,ordvars
             MergeSearchListView.SetItemText using N9,hold,2
  return
.
MergeEnableLower
.         setprop Mergemlredit,enabled=1,bgcolor=white
          setprop Mergemlredit,enabled=1
          return

MergeEnableAddvars
          setprop Mergeinputqty,enabled=1,bgcolor=white,tabid=800,text="0"
          setprop Mergenetoutput,enabled=1,bgcolor=white,tabid=805,text="0"
          setprop Mergedma,enabled=1,bgcolor=white,tabid=820,text="0"
          setprop Mergemnf,enabled=1,bgcolor=white,tabid=840,text="0"
          setprop Mergez4rej,enabled=1,bgcolor=white,tabid=855,text="0"
          setprop Mergeintra,enabled=1,bgcolor=white,tabid=835,text="0"
          setprop Mergeconv,enabled=1,bgcolor=white,tabid=860,text="0"
          setprop Mergeerr,enabled=1,bgcolor=white,tabid=830,text="0"
          setprop Mergedead,enabled=1,bgcolor=white,tabid=815,text="0"
.         setprop Mergecustr,enabled=1,bgcolor=white,tabid=810,text="0"
          setprop Mergenonp,enabled=1,bgcolor=white,tabid=845,text="0"
          setprop Mergeprison,enabled=1,bgcolor=white,tabid=850,text="0"
          setprop Mergedpv,enabled=1,bgcolor=white,tabid=825,text="0"
          setprop Mergedisa,enabled=1,bgcolor=white,tabid=865,text="0"
          setprop MergeCRN,enabled=1,bgcolor=white,tabid=875,text="0"
          setprop Mergerejtot,enabled=1,bgcolor=white,text="0"
.begin patch 1.9
          setprop MergeCusts,bgcolor=white,text="0"
          setprop MergeUnused,bgcolor=white,text="0"
.end patch 1.9

.Start patch 1.5 added
          setprop Mergestatrej,enabled=1,text="Total Rejects:"
.End patch 1.5 added
          return

MergeEnableModvars
.         setprop MergeDisa,enabled=1,bgcolor=white,tabid=830,text="0"
          setprop MergeDisa,enabled=1,bgcolor=white,tabid=830
          setprop MergeCRN,enabled=1,bgcolor=white,tabid=840
.begin patch 1.9
.          setprop MergeCusts,enabled=1,bgcolor=white,visible=1
.          setprop MergeUnused,enabled=1,bgcolor=white,visible=1
.end patch 1.9
          return
MergeEnableLowerSecurity
          return

MergeClearUpper
    return
MergeCLearLower
          setitem mergeshipto,0,""
          setitem mergemlredit,0,""
          setitem   mergemasterlist,0,""
          setitem mergeinputqty,0,""
          setitem mergenetoutput,0,""
          setitem mergetotbill,0,""
          setitem mergeorderdate,0,""
          setitem mergemp,0,""
          setitem mergenet,0,""
          setitem mergetotrej,0,""
          setitem mergelistinfo,0,""
          setitem mergekey,0,""
          return

Mergecleardeducts
          setitem   Mergeinputqty,0,""
          setitem   Mergenetoutput,0,""
          setitem   Mergetotbill,0,""
          setitem   Mergenet,0,""
          setitem   Mergetotrej,0,""
          setitem   Mergecusts,0,""
          setitem   Mergecustr,0,""
          setitem   MergeCNR,0,""
          setitem   Mergedead,0,""
          setitem   Mergedma,0,""
          setitem   Mergeelim,0,""
          setitem   Mergeerr,0,""
          setitem   Mergefamily,0,""
          setitem   Mergehd,0,""
.
          setitem   Mergehh,0,""
.
          setitem   Mergeintra,0,""
          setitem   Mergedrop,0,""
          setitem   Mergedupm,0,""
          setitem   Mergencoa,0,""
          setitem   Mergemnf,0,""
          setitem   Mergemwf,0,""
          setitem   Mergenfrj,0,""
          setitem   Mergenix1,0,""
          setitem   Mergenix2,0,""
          setitem   Mergenix3,0,""
          setitem   Mergencoatot,0,""
          setitem   Mergencoaunm,0,""
          setitem   Mergenix,0,""
          setitem   Mergenixem,0,""
          setitem   Mergend,0,""
          setitem   Mergenonp,0,""
          setitem   Mergeprison,0,""
          setitem   Mergerep,0,""
          setitem   Mergetdmc,0,""
          setitem   Mergez4crt,0,""
          setitem   Mergez4rej,0,""
          setitem   Mergezipcor,0,""
          setitem   Mergezipver,0,""
          setitem   MergeDPV,0,""
          setitem   Mergeconv,0,""
          setitem   Mergedisa,0,""
          setitem   MergeCRN,0,""
.Start patch 1.5 added
          setprop Mergestatrej,enabled=0,text=""
.End patch 1.5 added
          return
.
MergeDisableLower
.         move      NO,SecFlag
          return

MergeEnableButtons
          setprop   MergeOK,enabled=1
          setprop   MergeExit,enabled=1
          setprop   MergeHot,enabled=1
          setprop   MergeDELETE,enabled=1
          setprop   MergeFax,enabled=1
          setprop   MergeDaily,enabled=1
          setprop   MergeMod,enabled=1
.         setprop   MergeADD,enabled=1
          move      YES,ExitFlag
          return
MergeEnableButtons2
          setprop   MergeQuit,enabled=1
          setprop   MergeSave,enabled=1
          setprop   MergeADD,enabled=0
          setprop   MergeMod,enabled=0
          setprop   MergeFax,enabled=0
          setprop   MergeHot,enabled=0
          setprop   MergeDELETE,enabled=0
          setprop   MergeExit,enabled=0
          setprop   MergeDaily,enabled=0
          return
MergeDisableButtons2
          setprop   MergeSAVE,enabled=0
          setprop   MergeQUIT,enabled=0
          return
MergeDisableButtons
          move      "N",ExitFlag
          setprop   MergeOK,enabled=0
          setprop   MergeFax,enabled=0
          setprop   MergeHot,enabled=0
          setprop   MergeDELETE,enabled=0
          setprop   MergeExit,enabled=0
          setprop   MergeDaily,enabled=0
          setprop   MergeAdd,enabled=0
.         setprop   MergeMod,enabled=0
          return

    loop
                waitevent
.Start Patch #1.4 - timer logic
                    deactivate TIMER
                    ACTIVATE TIMER,Timeout,RESULT
.End Patch #1.4 - timer logic
        repeat


.Start Patch #1.4 - timer logic
Timeout
        beep
        beep
        beep
        winshow
        stop

SCREEN1
SCREEN2
screen3
..............................................................................
.add. - add info manually from outside merge - or correct uploaded info.
Add
.Display Password Form
.begin patch 1.1
        cmatch  yes,holdflag
                      goto    goodpass if equal
.end patch 1.1
        setprop Passwrd,visible=1
        move    NPASUSER,str10
.Test for Credit Password
mergetest
.        unpack  NPASFLD,str1,NPASKEY
        unpack  NPASFLD,NPASKEY
        pack    NPASFLD,"I",NPASKEY
        reset   NPASFLD
        call    NPASKEY
        if not over
                move    YES,HoldFlag
                alert   note,"Password Accepted!",result
        else
                move    str10,NPASUSER
                      return
        endif
goodpass
        setfocus Mergelr
                      getitem  mergelr,0,nmrglr
          call     nmrgkey
          if       not over
          alert     note,"That Merge Already Exists!",result
          setfocus   mergelr
          else
.          call     mergeclearlower
           call     mergecleardeducts
           call     mergedisablebuttons
.         alert     note,"That Merge Record Does Not Exist!",result
   alert  plain,"Would you like to add?",result
         cmatch    yes to str1
          if (result <> 1)
             return
                    setfocus mergeLR
                    else
                    goto    enterit
                    endif

           setfocus   mergelr
           endif
enterit
.           call      wipemrg
           MOVE      NMRGFLD TO NORDFLD
           MOVE      NMRGFLD TO Nmrglr
                                        move      C1,NORDPATH
           CALL        NORDKEY
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
                              goto      exchok if equal
                    if (OSTAT = "B" or OSTAT = "Q")
                              alert     Note,"Order Already Billed!!",RESULT
.                             alert     Note,"New Merge record not allowed for Billed orders!",RESULT
.         call      MERGEDISABLEButtons2
.         call      MERGEDisableLower
.         call      MERGEclearDEDUCTS
.         call      mERGEEnableButtons
.         move      NO,NewFlag
.         return
.                             setfocus mergelr
                    elseif  (OSTAT = "X")
                              alert     Note,"New Merge record not allowed for Cancelled orders!",RESULT
          call      MERGEDISABLEButtons2
          call      MERGEDisableLower
          call      MERGEclearDEDUCTS
          call      mERGEEnableButtons
          move      NO,NewFlag
                              setfocus mergelr
                    Elseif  (OSTAT = "p" or OSTAT = "x" or OSTAT = "z" or OSTAT = "l")
                              alert     Note,"New Merge record not allowed for Pending orders or LCR's!",RESULT
          call      MERGEDISABLEButtons2
          call      MERGEDisableLower
          call      MERGEclearDEDUCTS
          call      mERGEEnableButtons
          move      NO,NewFlag
.                             setfocus mergeLR
                              return
                              endif
.                             endif
.
exchok

         move      o1des to NMRGLNAM
         move      omlrky to nmrgkcod
           MOVE        ORTNNUM TO NRTNFLD
           CALL      NRTNKEY
.           if        over
.           GOTO      KEY IF OVER
           pack      mkey from omlrnum,ocobn
           call      nmlrkey
adddisp
          setprop   Mergelistview,enabled=0
          setprop   Mergetotbill,enabled=0,bgcolor=grey
          setprop   Mergeorderdate,enabled=0,bgcolor=grey
          setprop   Mergemp,enabled=0,bgcolor=grey
          setprop   Mergenet,enabled=0,bgcolor=grey
          setprop   Mergetotrej,enabled=0,bgcolor=grey
          setitem   Mergelistinfo,0,o1des
          setitem   MergeOrderdate,0,str10
          setitem   Mergekey,0,nmrgkcod
          setitem   Mergeshipto,0,rtcomp
          setitem   Mergedatt2,0,rtcomp
          setitem   Mergemlredit,0,mcomp
          setitem   Mergestat50,0,mcomp
          setitem   Mergemasterlist,0,o1des
          setitem   Mergemp,0,omlrky
          setfocus Mergeinputqty
          move      YES,NewFlag
          call   mergeenableaddvars
          call      mergeEnableButtons2

    return
write
.         move    olrn to nmrgfld
.         move    olrn to nmrglr
.         call    nmrgwrt
.         goto     key

Modify
.Display Password Form
.begin patch 1.1
        cmatch  yes,holdflg2
                      goto    goodpass2 if equal
.end patch 1.1
        setprop Passwrd,visible=1
        move    NPASUSER,str10
.Test for Credit Password
mergetestJD
.        unpack  NPASFLD,str1,NPASKEY
        unpack  NPASFLD,NPASKEY
        pack    NPASFLD,"I",NPASKEY
        reset   NPASFLD
        call    NPASKEY
        if not over
                move    YES,HoldFlg2
                alert   note,"Password Accepted!",result
        else
                move    str10,NPASUSER
                      return
        endif
goodpass2
           call     mergedisablebuttons
          setitem   Mergedisa,0,nmrgdisa
          setitem   MergeCRN,0,nmrgdisa
          setfocus Mergedisa
          move      YES,modFlag
          call   mergeenablemodvars
          call      mergeEnableButtons2
          return

..............................................................................
PRINT
.           NORETURN
           CMATCH    B1 TO NMRGLR
           return    IF EOS
           MOVE        NMRGLR TO NMRGFLD
           REP       ZFILL IN NMRGFLD
           READ      NMRGFLE1,NMRGFLD;;
           return     IF NOT OVER
           WRITE     NMRGFLE1,NMRGFLD;NMRGFLD,TYPINIT,B1
           alert    note,"Added to Daily Print !",result
                                return
hotcv
         CLEAR     TASKNAME
   alert  plain,"PDF ??",result
         cmatch    yes to str1
          if (result <> 1)
.           append    "\\Nins1\Lanbatch\batch32 -X f:\apps\winbatch\butil job=HOTCV ",TASKNAME
.           append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\nts0\c\lanbat~2 f:\apps\winbatch\butil job=HOTCV ",TASKNAME
.                    IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.                    append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\nins1\ServerA \\Nins1\Winbatch\butil job=HOTCV ",TASKNAME
.                    else
                    append    "\\Nins1\Winbatch\butil job=HOTCV ",TASKNAME
.                    endif
           APPEND    " infile=",taskname
           append    nmrgfld,taskname
         append    typinit to taskname
           append    " F=default C=1",TASKNAME
           APPEND    " B=",TASKNAME
           APPEND    user TO TASKNAME
         RESET     TASKNAME
.           EXECUTE   TASKNAME
           alert    note,"SUBMITTING PRINT NOW !",result
          call      PrintHotCV using nmrgfld,INITS,user,C1,C0

                                else
.           append    "!f:\apps\winbatch\butil job=HOTCVPDF ",TASKNAME
.                    IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.                    append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\nins1\ServerA \\Nins1\Winbatch\butil job=HOTCVPDF ",TASKNAME
.                    else
                    append    "\\Nins1\Winbatch\butil job=HOTCVPDF ",TASKNAME
.                    endif
.           append    "!\\Nins1\Winbatch\butil job=HOTCVPDF ",TASKNAME
           APPEND    " infile=",taskname
           append    nmrgfld,taskname
         append    typinit to taskname
           append    " F=default C=1",TASKNAME
           APPEND    " B=",TASKNAME
           APPEND    user TO TASKNAME
           append   " co=",taskname
           append   company,taskname
         RESET     TASKNAME
.           EXECUTE   TASKNAME
          call      PrintHotCV using nmrgfld,INITS,user,C1,C1

                                endif
           return
.GOTO      KEY
FAX
                               call       getwinver
.NORETURN
.           TRAPCLR   F3
.           TRAP      FAX IF F3
                               call      mergedisablebuttons
          CMATCH    B1 TO NMRGLR
         return     IF EOS
         CALL      CNTRTN
         move      OLON to nownfld
         rep       zfill in nownfld
         call      nownkey
FAX2
.begin patch 1.8
.testing
.          move      "4154337796",ownfax
.          move      "4154337796",ownfax2
.          move      "dherric",nuseuser
.end test          
          scan      "@",OwnEmail
          if        equal
          reset     OwnEmail
          pack      Str55 from "use - ",OwnEmail," ?"
          alert      plain,str55,result
                    if (result = 1)
                    pack      Mailto from OwnEmail
                    goto      attnto
                    endif
          endif
.end patch 1.8
         type      ownfax2
         goto      ownfax if not equal
         move      yes to str1
.begin patch 5.86
.         keyin     *p1:24,*el,*b,"Use the Accounting fax ##? ",*rv,*uc,*t120,str1;
         alert      plain,"Use the Accounting fax ##?",result
          if (result <> 1)
.end patch 5.86
.         cmatch    yes to str1
         goto      ownfax
                              endif
         move      ownfax2 to ownfax
          count     N2,OwnFax
          compare   C10,N2
          if equal
                    move      C1,LONGDIST
                    unpack    OwnFax,str3,str7
                    match     "510",str3                    .LOCAL ?
                    if equal
                              move      str7,OwnFax
                              clear     LONGDIST
                    else
                              match     B3,str3             .LOCAL ?
                              if equal
                                        move      str7,OwnFax
                                        clear     LONGDIST
                              endif
                    endif
          endif

        pack      MailTo,"IMCEAFACSYS-",longdist,ownfax,"@nincal.com"

ownfax
         TYPE      OWNFAX
         IF        NOT EQUAL
.         DISPLAY   *P1:24,*EL,*red,"FAX NUMBER IS REQUIRED!!! ",ownfax,*white,*B,*W4;
          alert     note,"FAX NUMBER IS REQUIRED!!",result
        setfocus Mergelr
         ELSE
.begin patch 5.86
attnto
.         keyin     *p1:24,*el,"attn: ",*t120,ownlonm
.         DISPLAY    *P7:24,OWNLONM
          setprop     mergeownattn,Visible=1,Enabled=1,height=20
                               setitem    Mergeownattn,0,ownlonm
.                              setfocus  mergeownattn
.          KEYIN     *P76:24,"OK?",*t60,STR1;
         alert      plain,"Name OK?",result
          if (result = 3)
   mergeListView.DeleteAllItems
          call      MERGEclearLower
          clear     ownlonm
          setprop     mergeownattn,Visible=0,Enabled=0,height=20
          call      MERGEclearDEDUCTS
          call      mERGEEnableButtons
          move      NO,NewFlag
                return
                              endif
.end patch 5.86
.         cmatch    yes to str1
.         if        not equal
          if (result <> 1)
.begin patch 5.86
.         keyin     *p1:24,*el,"attn: ",*t120,ownlonm;
                               clear     ownlonm
                               setfocus  mergeownattn
          getitem     mergeownattn,0,ownlonm
.end patch 5.86
         goto      attnto
                              else
         endif
.            COUNT     N2,OWNFAX
.            COMPARE   C10 TO N2
.            IF        EQUAL
.            MOVE      C1 TO LONGDIST
.            UNPACK    OWNFAX INTO STR3,STR7
..START PATCH 5.84 REPLACED LOGIC
..            MATCH     "415" TO STR3           .LOCAL ?
.            MATCH     "510" TO STR3           .LOCAL ?
..END PATCH 5.84 REPLACED LOGIC
..            IF         EQUAL
.            MOVE       STR7 TO OWNFAX
.            CLEAR      LONGDIST
.            else
.            MATCH      B3 TO STR3           .LOCAL ?
.            IF         EQUAL
.            MOVE       STR7 TO OWNFAX
.            CLEAR      LONGDIST
.            endif
. .           ENDIF
.            ENDIF
.         ENDIF
.         move      c1 to n3
.FILENAME CLEAR     formname
.         APPEND    "NCV",formNAME
.         MOVE      n3,str3
.         REP       zfill,str3
.         APPEND    str3,formname
.         RESET     formname TO 6
.         RESET     formname
.         TRAP      GOODFILE GIVING ERROR IF IO
.         OPEN      formfile,formname
.         CLOSE     formfile
.ADDFILE  ADD       c1,n3
.         GOTO      FILENAME
.GOODFILE TRAPCLR   IO
.         NORETURN
.         reset     error
.         SCAN      "I * Y" IN ERROR
.         GOTO      ADDFILE IF EQUAL
.         CLEAR     RECNAME
..START PATCH 5.85 REPLACED LOGIC
.           APPEND    "g:\data\",RECNAME
.           APPEND    NTWKPATH1,RECNAME
.END PATCH 5.85 REPLACED LOGIC
.           APPEND    formname,RECNAME
. .        RESET     RECNAME
.         MOVE      B1 TO ERROR
.         PREPARE   formfile,RECNAME,CREATE
.         DISPLAY   *P1:24,*EL,"The output file name for this fax is: ":
.                   formname;
.         clear     recname
.START PATCH 5.85 REPLACED LOGIC
.         append    "g:\data\",recname
.         append    NTWKPATH1,recname
.END PATCH 5.85 REPLACED LOGIC
.         append    formname to recname
.         append    ".dat" to recname
.         reset     recname
.         .SPLOPEN    recname
.          clear     badfaxflag
.          trap      faxspool if spool
.          Splopen   "Facsys2","A"
.          cmatch    yes to badfaxflag
.          if        equal
.          SPLOPEN    recname
.          endif
.......         .splopen   "Laser3 Blankstock","A"
.         clock     time to time
.         clear     str5
.         append    time to str5
.         reset     str5
.begin patch 5.82
.         print     "^[D",longdist,ownfax,"^[N",ownocpy:
.                   "^[T",today,b1,str5,"^]":
.                   *n,032,hpreset:
.                   hpttray:
.                   hpport:
.                   033,"&l66P":               page length
.                   033,"&l65F":
.                   033,"&l1E",033,"&a0c0R":     top margin * print position
.                   hpltrhd
.         print     "^[D",longdist,ownfax,"^[N",ownocpy:
.                   "^[T",today,b1,str5,"^]":
.                   *n,032,hpreset:
.                   hpttray:
.                   hpport:
.                   033,"&l66P":               page length
.                   033,"&l65F";
.        call       PortraitLTRHEAD
...............................................................................
.          PRINT     *N,*N,*31,hp14pt,"COMPUTER VERIFICATION"
OpenFile
.begin patch 1.8
.                              pack      taskname,"c:\work\hdrfile.prn"
        clear   badfaxflag
.FOR TESTING PURPOSES
.        move    YES,badfaxflag
.        trap    faxspool if spool
.        SPLOPEN taskname
.patch1.81
          pack      MailSubjct from "CV for ",ownocpy,"-",cntname
          pack      MailFrom from Nuseuser,"@nincal.com"
.        print   "^[D",longdist,ownfax,"^[N",ownocpy:
.                "^[T",today,b1,str5,"^[S",cntname,"^]"
..        print   "^[D",longdist,ownfax,"^[N",ownocpy:
..                "^[T",today,b1,str5,"^[S",NUSEUSER,"^]"
.patch1.81
.        SPLCLOSE
.FOR TESTING PURPOSES
.        move    "4154337796",ownfax
        call    Trim using LONGDIST
.begin patch 2.0
.          call      PDF995Auto
.end patch 2.0
       if (badfaxflag = YES)
.                if (PRTNAME = "1" | PRTNAME = "3")      .Laser2
.                        PRTOPEN prfile,"\\SRV2008a\laser2","FAXFILE.PRN"
.                else                                    .Laser3 = Default
.                        PRTOPEN prfile,"\\SRV2008a\laser3 Blankstock","FAXFILE.PRN"
.                endif

          pack      MailTo from Nuseuser,"@nincal.com"
          
.begin patch 2.0
.                PRTOPEN prfile,"pdf995","CV"
                PRTOPEN prfile,"pdf:","c:\work\pdf\CV.pdf"
        else
.                PRTOPEN prfile,"FAXFILE","FAXFILE.PRN"
.                PRTOPEN prfile,"pdf995","CV"
                PRTOPEN prfile,"pdf:","c:\work\pdf\CV.pdf"
.end patch 2.0
        endif
                      endif     jD check.
PRINTCV
        prtpage prfile;*UNITS=*HIENGLISH;
.begin patch 1.62
.          IF        (company = c2)
.          prtpage   PrFile;*p=1:25,*font=fontO18b,"Pacific Lists, Inc.":
.                    *p=451:343,*font=fontO7,"1300 Clay St. 11th Floor":
.                    *p=451:443,"Oakland, CA 94612-1492":
.                    *p=317:543,"415-945-9450 ","·"," Fax 415-945-9451":
.                    *p=317:643,"A Division of Names in the News"
.          else
          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.          endif
.end patch 1.8
.end patch 1.62
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     "60",row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p3000:row,*font=font5,*boldon,"COMPUTER VERIFICATION",*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Attention: ",ownlonm;
.         PRINT     HPTMSRMN,hpfixed,*n,*n,*n,*n,*n,*n,*n,*n,*n:
.                  *L,*L,*31,"COMPUTER VERIFICATION"
.end patch
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Mailer:";
        prtpage prfile;*p900:row,mcomp;
        prtpage prfile;*p5200:row,"Order Date:";
        prtpage prfile;*p6500:row,odate;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"List:";
        prtpage prfile;*p900:row,o1des;
        prtpage prfile;*p5200:row,"LR##";
        prtpage prfile;*p5700:row,olrn;
        add     sixlpi,row
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Input Qty:";
        prtpage prfile;*p900:row,nmrgrqty;
        prtpage prfile;*p5200:row,"M/P ##";
        prtpage prfile;*p5700:row,omlrky;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Net Output:";
        prtpage prfile;*p900:row,nmrgnet;
        prtpage prfile;*p5100:row,percent;
        prtpage prfile;*p5700:row,"% Names Mailed"
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Total Billable Names:",totbill;
.                     goto        stop
.         COMPARE    C0 TO NMRGID
.         IF         NOT EQUAL
.         PRINT      *L,*L,*5,"INTRA DUPES: ",*25,NMRGID;
        add     sixlpi,row
        add     sixlpi,row
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Intra Dupes:";
        prtpage prfile;*p1500:row,nmrgid;
..                    *40,"ELIMINIX HITS: ",*65,NMRGELIM;
.        ENDIF
         COMPARE    C0 TO NMRGERR
         IF         NOT EQUAL
.         PRINT      *L,*5,"ERROR REJECTS: ",*25,NMRGERR;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Error Rejects:";
        prtpage prfile;*p1500:row,nmrgerr;
.                    *40,"TDMC REJECTS: ",*65,NMRGTDMC;
         ENDIF
         COMPARE    C0 TO NMRGdisf
         IF         NOT EQUAL
.         PRINT      *L,*5,"DECEASED REJECTS: ",*25,NMRGDISF;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Deceased Rejects:";
        prtpage prfile;*p1500:row,nmrgdisf;
.                    *40,"EXIT REJECTS: ",*65,NMRGEXIT;
         ENDIF
         COMPARE    C0 TO NMRGNPER
        IF         NOT EQUAL
.        PRINT      *L,*5,"NONPERSONAL REJECTS: ",*25,NMRGNPER;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Nonpersonal Rejects:";
        prtpage prfile;*p1500:row,nmrgnper;
.                    *40,"MAILDROP REJECTS: ",*65,NMRGDROP;
        ENDIF
         COMPARE    C0 TO NMRGDMA
         IF         NOT EQUAL
.        PRINT      *L,*5,"DMA REJECTS: ",*25,NMRGDMA;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"DMA Rejects:";
        prtpage prfile;*p1500:row,nmrgdma;
.                    *40,"UNUSED DUPES: ",*65,NMRGUDUP;
         ENDIF
.         COMPARE    C0 TO NMRGELMX
.         IF         NOT EQUAL
.         PRINT      *L,*5,"ELIMINIX REJECTS: ",*30,NMRGELMX;
.         ENDIF
         COMPARE    C0 TO NMRGZ4
         IF         NOT EQUAL
.         PRINT      *L,*5,"ZIP+4 CRT REJECTS: ",*25,NMRGZ4;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Zip+4 Rejects:";
        prtpage prfile;*p1500:row,nmrgz4;
.                    *40,"NIXIE REJECTS: ",*65,NMRGNIX;
         ENDIF
. .        COMPARE    C0 TO NMRGTDMC
.         IF         NOT EQUAL
.         PRINT      *L,*5,"TDMC REJECTS: ",*30,NMRGTDMC;
.         ENDIF
         COMPARE    C0 TO NMRGPRIS
         IF         NOT EQUAL
.         PRINT      *L,*5,"PRISON REJECTS: ",*25,NMRGPRIS;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Prison Rejects:";
        prtpage prfile;*p1500:row,nmrgpris;
         ENDIF
         COMPARE    C0 TO NCOAMNF
         IF         NOT EQUAL
.         PRINT      *L,*5,"NCOA MTCH NON FWD: ",*25,NCOAMNF;
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"NCOA Mtch Non Fwd:";
        prtpage prfile;*p1500:row,ncoamnf;
         ENDIF
         cmatch     yes to feb
         if         equal
         COMPARE    C0 TO NMRGCUST
         IF         NOT EQUAL
.         PRINT      *L,*5,"CUSTOMER REJECTS: ",*25,NMRGCUST;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Customer Rejects:";
        prtpage prfile;*p1500:row,nmrgcust;
         ENDIF
         endif
         COMPARE    C0 TO NMRGconv
         IF         NOT EQUAL
.         PRINT      *L,*5,"CONVERSION REJECTS: ",*25,NMRGCONV;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Conversion Rejects:";
        prtpage prfile;*p1500:row,nmrgconv;
         ENDIF
.start Patch #2.0 - new var.
         COMPARE    C0 TO NMRGDPV
         IF         NOT EQUAL
.         PRINT      *L,*5,"DPV DROPS: ",*25,NMRGDPV;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"DPV Drops:";
        prtpage prfile;*p1500:row,nmrgdpv;
         ENDIF
         COMPARE    C0 TO NMRGdisa
         IF         NOT EQUAL
.         PRINT      *L,*5,"DISASTER DROPS: ",*25,NMRGDPV;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"DISASTER Drops:";
        prtpage prfile;*p1500:row,nmrgdisa;
         ENDIF
.Start patch 1.7 added

         COMPARE    C0 TO NMRGCNR
         IF         NOT EQUAL
.         PRINT      *L,*5,"CNR MATCHES: ",*25,NMRGCNR;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"CNR Matches:";
        prtpage prfile;*p1500:row,nmrgCNR;
         ENDIF
.End patch 1.7 added
.end Patch #2.0 - new var.
.         move       c0 to n2
.         move       onetper to n2
.         compare    c0 to n2
.         if         not equal
.         COMPARE    C0 TO NMRGrep
.         IF         NOT EQUAL
.         PRINT      *L,*5,"REPUBLICAN REJECTS: ",*25,NMRGrep;
.         ENDIF
.         endif
.         COMPARE    C0 TO NMRGDROP
.         IF         NOT EQUAL
.         PRINT      *L,*5,"MAILDROP REJECTS: ",*30,NMRGDROP;
.         ENDIF
.         PRINT      *L,*25,"____________";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*pensize=10,*line=3000:row;
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Total Rejects:";
        prtpage prfile;*p1500:row,totrej;
.        PRINT      *L,*5,"TOTAL REJECTS: ",*25,TOTREJ;
        add     sixlpi,row
        add     sixlpi,row
        prtpage prfile;*p3000:row,"Eliminator Hits:";
        prtpage prfile;*p5500:row,nmrgelim;
.          PRINT      *l,*l,*40,"ELIMINATOR HITS: ",*65,NMRGELIM;
.         PRINT      *l,*40,"TDMC REJECTS: ",*65,NMRGTDMC;
        add     sixlpi,row
        prtpage prfile;*p3000:row,"TDMC Rejects:";
        prtpage prfile;*p5500:row,nmrgtdmc;
.         PRINT      *l,*40,"NCOA REJECTS: ",*65,totncoa;
        add     sixlpi,row
        prtpage prfile;*p3000:row,"NCOA Rejects:";
        prtpage prfile;*p5500:row,totncoa;
.         PRINT      *l,*40,"MAILDROP REJECTS: ",*65,NMRGDROP;
        add     sixlpi,row
        prtpage prfile;*p3000:row,"Maildrop Rejects:";
        prtpage prfile;*p5500:row,nmrgdrop;
.        PRINT      *l,*40,"UNUSED DUPES: ",*65,NMRGUDUP;
        add     sixlpi,row
        prtpage prfile;*p3000:row,"Unused Dupes:";
        prtpage prfile;*p5500:row,nmrgudup;
.         PRINT      *l, *40,"FAMILY REJECTS: ",*65,NMRGFAM;
        add     sixlpi,row
        prtpage prfile;*p3000:row,"Family Rejects:";
        prtpage prfile;*p5500:row,nmrgfam;
.         PRINT      *l, *40,"NIXIE REJECTS: ",*65,NMRGNIX;
        add     sixlpi,row
        prtpage prfile;*p3000:row,"Nixie Rejects:";
        prtpage prfile;*p5500:row,nmrgnix;
.         PRINT      *l, *40,"HOUSE HITS: ",*65,NMRGHH;
        add     sixlpi,row
        prtpage prfile;*p3000:row,"House Hits:";
        prtpage prfile;*p5500:row,nmrghh;
.         PRINT      *l, *40,"CUSTOMER SUPPRESS: ",*65,NMRGCS;
        add     sixlpi,row
        prtpage prfile;*p3000:row,"Customer Suppress:";
        prtpage prfile;*p5500:row,nmrgcs;
         cmatch     yes to feb
         if         not equal
.         PRINT      *l, *40,"CUSTOMER REJECTS: ",*65,NMRGCust;
        add     sixlpi,row
        prtpage prfile;*p3000:row,"Customer Rejects:";
        prtpage prfile;*p5500:row,nmrgcust;
         endif
         COMPARE    C0 TO NMRGrep
         IF         NOT EQUAL
         move       yes to rep
.         PRINT      *L,*40,"INDEPENDENT REJECTS: ",*65,NMRGrep;
        add     sixlpi,row
        prtpage prfile;*p3000:row,"Independent Rejects:";
        prtpage prfile;*p5500:row,nmrgrep;
         endif
         COMPARE    C0 TO NMRGelmx
         if         not equal
         goto       printrep
         else
         cmatch     yes to rep
          if         equal
printrep
.         PRINT      *L,*40,"CNR REJECTS: ",*65,NMRGelmx;
        add     sixlpi,row
        prtpage prfile;*p3000:row,"CNR Rejects:";
        prtpage prfile;*p5500:row,nmrgelmx;
.         PRINT      *N,*N,*5,"NOTE: INPUT QTY MINUS TOTAL REJECTS,";
.         PRINT      *l,*5,"ELIMINATOR, TDMC, NCOA TOTAL , MAILDROP,";
.         PRINT      *l,*5,"UNUSED DUPES, NIXIE, FAMILY, HOUSE HITS,";
.         cmatch     yes to feb
.         if         not equal
.         PRINT      *l,*5,"CUSTOMER SUPPRESS, CUSTOMER REJECTS, CNR REJECTS";
.         else
.         PRINT      *l,*5,"CUSTOMER SUPPRESS, CNR REJECTS,";
.         endif
.         PRINT      *l,*5,"INDEPENDENT REJECTS = NET OUTPUT"
.         PRINT       *N,*N,*N,*2,STR3
.         goto       zerotots
.         ENDIF
.         endif
        add     sixlpi,row
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Note: Input qty minus Total rejects,";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Eliminator, TDMC, Ncoa Total, Maildrop,";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Unused Dupes, Nixie, Family, House Hits,";
         cmatch     yes to feb
         if         not equal
.         PRINT      *l,*5,"CUSTOMER SUPPRESS, CUSTOMER REJECTS = NET OUTPUT";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Customer Suppress, Customer Rejects, CNR Rejects,";
         else
        prtpage prfile;*pcolumn:row,"Customer Suppress, CNR Rejects,";
                      endif
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Independent Rejects = Net Output";
        add     sixlpi,row
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,STR3
         goto       zerotots
         ENDIF
         endif
PRINTREG
        add     sixlpi,row
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Note: Input qty minus Total rejects,";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Eliminator, TDMC, Ncoa Total, Maildrop,";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Unused Dupes, Nixie, Family, House Hits,";
         cmatch     yes to feb
         if         not equal
.         PRINT      *l,*5,"CUSTOMER SUPPRESS, CUSTOMER REJECTS = NET OUTPUT";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Customer Suppress, Customer Rejects = Net Output";
        add     sixlpi,row
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,STR3
         else
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Customer Suppress = Net Output";
        add     sixlpi,row
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,STR3
         endif
zerotots MOVE        C0 TO TOTREJ
         MOVE        C0 TO TOTNCOA
         move        no to feb
         move        no to rep
.
         clear     taskname
        PRTCLOSE prfile
   alert  plain,"OK to Send Now??",result
         cmatch    yes to str1
          if (result <> 1)
                              alert     Note,"Job Cancelled!!",RESULT
   mergeListView.DeleteAllItems
          call      MERGEclearLower
          clear     ownlonm
          setprop     mergeownattn,Visible=0,Enabled=0,height=20
          call      MERGEclearDEDUCTS
          call      mERGEEnableButtons
             return
                    setfocus mergeLR
                    else
.begin patch 1.8
.                              alert     Note,"Sending now, to Stop job use Facsys Program!!",RESULT
                              alert     Note,"Sending now",RESULT
.end patch 1.8
                    endif

.
.begin patch 1.8
.begin patch 2.0
.          call      PDF995Auto0
.end patch 2.0
                              
          Pack      MailAttach from "c:\work\pdf\CV.pdf"
          Move      c0,TrapCount                   .reset

CheckFile

          trap      WaitForEnd giving error if IO
          open      FileCheck,MailAttach,Exclusive          
          Close     FIleCHeck
          call      SendMail

.        if (badfaxflag <> YES)
.                clear   taskname
..begin patch 1.6
..                Path    Exist,"c:\windows"
..                if      over
.                    If                  (osflag = c1 | osflag = c5)
.                        append  "!c:\winnt\system32\cmd.exe",taskname
..                else
.                 ElseIf                  (osflag = c3 | osflag = c4)
.                        append  "!c:\command.com",taskname
.                 ElseIf                  (osflag = c6)
.                        append  "!c:\windows\system32\cmd.exe",taskname
..end patch 1.6
.                endif
.                append  " /c copy ",taskname
..                append  NTWKPATH4,taskname
..                append  "fax\hdrfile.prn /b + ",taskname
.                append  "c:\work\hdrfile.prn /b +",taskname
..                append  NTWKPATH4,taskname
..                append  "fax\faxfile.prn /b ",taskname
.                append  "c:\work\faxfile.prn /b ",taskname
.                append  NTWKPATH4,taskname
.                append  "fax\",taskname
.                clock   timestamp,timestamp
.                append  timestamp,taskname
.                append  ".prn",taskname
.              reset   taskname
.                execute taskname
.                clear   taskname
..begin patch 1.6
..                Path    Exist,"c:\windows"
..                if      over
.                    If                  (osflag = c1 | osflag = c5)
.                        append  "!c:\winnt\system32\cmd.exe",taskname
..                else
.                 ElseIf                  (osflag = c3 | osflag = c4)
.                        append  "!c:\command.com",taskname
.                 ElseIf                  (osflag = c6)
.                        append  "!c:\windows\system32\cmd.exe",taskname
..end patch 1.6
.                endif
.                append  " /c copy ",taskname
.                append  NTWKPATH4,taskname
.                append  "fax\",taskname
.                append  timestamp,taskname
..                append  ".prn \\nts2\fax",taskname
.                append  ".prn \\SRV2008a\laser2",taskname
.                reset   taskname
..                execute taskname
.
.                move    "                                        ",APIFileName
.                clear   APIFileName
.                pack    APIFileName,NTWKPATH4,"fax\",timestamp,".prn",hexzero
.                call    DeleteFile
.                if (APIResult = 0 | APIResult = hexeight)
.                endif
.                      return
.                      endif
..       shutdown
.
..faxspool - Facsys printer not available/defined do it a different way
.
.faxspool
..          trapclr  spool
..          move     yes to badfaxflag
..          return
..         goto      faxend
..  display   *p1:24,*b,*el,"Job cancelled by user!!!",*w5,*p1:24,*el;
..faxend   erase     recname
..         return
..         open      formfile,formname,exclusive
..         close     formfile,delete
..         GOTO        KEY
...............................................................................
..now the fun begins
.         reset     ownocpy to 25
.blankc   cmatch   b1 to ownocpy
.         if       equal
.         bump     ownocpy,-1
.         goto     blankc
.         else
.         lenset   ownocpy
.         reset    ownocpy,1
.         endif
.         MOVE      c1 to n2
.         WRITE     FORMFILE,seq;"SMF-70"
.         WRITE     FORMFILE,seq;"o-nw-formtype: n,w, #"Computer Verification#""
.         WRITE     FORMFILE,seq;"TO: Fax @ MHSNTFAX (",*ll,ownocpy,") {FAX: ":
.                   LONGDIST,B1,*LL,OWNFAX;
.         WRITE     FORMFILE,seq;"; TO: ",*ll,OWNOCPY,"}"
.         WRITE     FORMFILE,seq;"From: ",nuseuser
.         WRITE     FORMFILE,seq;"Subject: Computer Verification"
.         WRITE     FORMFILE,seq;"ATTACHMENT: ",*LL,formNAME,".dat"
.         WRITE     FORMFILE,seq;"ATTACHMENT-NAME: ",*LL,formname,".dat"
.         cmatch    b1 to ownlonm
.         goto      wrteof if eos
.         goto      wrteof if equal
.         write     formfile,seq;b1
.         write     formfile,seq;"Attention: ",ownlonm
.         write     formfile,seq;"-------------------------------------"
.wrteof   WEOF      FORMFILE,SEQ
.         CLOSE     FORMFIle,chop
.         clear     taskname
..           append    "\PUBLIC\BATCH -X -N -PC:\WORK BB BUTIL job=NcvSEND INFILE=",TASKNAME
.
.           append    "\\Nins1\Lanbatch\batch32 -X -SA -Q\\nins1\ServerA \\Nins1\Winbatch\BUTIL job=NcvSEND INFILE=",TASKNAME
.           APPEND    formname TO TASKNAME
.           APPEND    " F=default C=",TASKNAME
.           APPEND    C1,TASKNAME
.           APPEND    " B=",TASKNAME
.           APPEND    user TO TASKNAME
.         RESET     TASKNAME
..          EXECUTE   TASKNAME
..         display   *p1:24,*el,"I'm sending now!!!!",*w4,*p1:24,*el;
..         GOTO        KEY
..
         call      mergeEnablebuttons

CNTRTN   SETLPTR   RTCOMP
         ENDSET    RTCOMP
CHKRHEAD CMATCH    B1 TO RTCOMP
         GOTO      SETRHEAD IF NOT EQUAL
         BUMP      RTCOMP BY -1
         GOTO      CHKRHEAD IF NOT EOS
SETRHEAD MOVEFPTR  RTCOMP TO N3
         MOVE      "80" TO RTNTAB
         SUBTRACT  N3 FROM RTNTAB
         DIVIDE    C2 INTO RTNTAB
         RESET     RTCOMP
         SETLPTR   RTCOMP
         RETURN
DISSORD
.         TRAPCLR   F2
.         move      c1 to nownpath
.         move      olon to nownfld
.         call      nownkey
.         scrnsave  scrn1
.         if        eos
.         display   *p1:24,*el,"Screen not saved successfully!!!",*b,*w5
.         endif
.         TRAP       DISSORD IF F2
. ...
.           CALL      DISSCRNB
.         KEYIN     *P35:24,"Enter to return ",*t200,STR1,*P1:1,*ES;
.         scrnrest  scrn1
         RETURN
.end patch 1.8
.
............................................................................
calcs    move      c0 to totrej
         move      c0 to totncoa
         move      c0 to n7
         move      c0 to calcper
.Start Patch #5.6 - increased var to hold century
.         PACK      STR8 FROM OODTEM,SLASH,OODTED,SLASH,OODTEY
         PACK      odate FROM OODTEM,SLASH,OODTED,SLASH,OODTEY
.End Patch #5.6 - increased var to hold century
         MOVE      NMRGNET TO CALCPER
         MOVE      NMRGIQTY TO N7
         DIVIDE    N7 INTO CALCPER
         MULT      "100" BY CALCPER
         MOVE      C0 TO PERCENT
         ADD       CALCPER TO PERCENT
         ADD        NMRGID TO TOTREJ
         ADD        NMRGERR TO TOTREJ
         ADD        NMRGDISF TO TOTREJ
         ADD        NMRGNPER TO TOTREJ
         ADD        NMRGDMA TO TOTREJ
         ADD        NMRGZ4 TO TOTREJ
         ADD        NMRGPRIS TO TOTREJ
         ADD        NCOAMNF TO TOTREJ
         cmatch     yes to feb
         if         equal
         ADD        NMRGCUST TO TOTREJ       .add customer rejects
         endif
         ADD        NMRGCONV TO TOTREJ       .CONVERSION DROPS - EPSLILON 1/12/95
         ADD        NMRGdpv TO TOTREJ        .DPV DROPS - PIDI 6/25/04
         ADD        NMRGdisa TO TOTREJ       .Disaster DROPS - added 10/5/05
.Start patch 1.7 added
         ADD        NMRGCNR TO TOTREJ        .CNR - added 09/07
.End patch 1.7 added

.         add        nmrgrep to totrej        add if ???? 8/95
.         add        nmrgelmx to totrej
         move       c0 to totbill
         MOVE       NMRGRQTY TO TOTBILL
         SUB        TOTREJ FROM TOTBILL
         add        ncoanix1 to totncoa
         add        ncoanix2 to totncoa
         add        ncoanix3 to totncoa
         return
............................................................................
wipemrg
         clear      nmrgLR
         move      c0 to nmrgFILL
         move      c0 to nmrgLNAM
         move      c0 to nmrgKCOD
         move      c0 to nmrgRQTY
         move      c0 to nmrgCONV
         move      c0 to nmrgIQTY
         move      c0 to nmrgTREJ
         move      c0 to nmrgID
         move      c0 to nmrgNETI
         move      c0 to nmrgELIM
         move      c0 to nmrgHDRP
         move      c0 to nmrgCS
         move      c0 to nmrgUDUP
         move      c0 to nmrgND
         move      c0 to nmrgDUPM
         move      c0 to nmrgNET
         move      c0 to nmrgZIPV
         move      c0 to nmrgZIPC
         move      c0 to nmrgZIP4
         move      c0 to NCOAMWF
         move      c0 to NCOAMNF
         move      c0 to NCOATOTM
         move      c0 to NIXIEM
         move      c0 to NCOAUNM
         move      c0 to NCOANFRJ
         move      c0 to NCOANIX1
         move      c0 to NCOANIX2
         move      c0 to NCOANIX3
         move      c0 to nmrgERR
         move      c0 to nmrgDISF
         move      c0 to nmrgNPER
         move      c0 to nmrgDMA
         move      c0 to nmrgELMX
         move      c0 to nmrgZ4
         move      c0 to nmrgNIX
         move      c0 to nmrgTDMC
         move      c0 to NCOAREJ
         move      c0 to nmrgCUST
         move      no to feb
         move      c0 to nmrgPRIS
         move      c0 to nmrgDROP
         move      c0 to nmrgHH
         move      c0 to nmrgFAM
         move      c0 to nmrgrep
         move      c0 to nmrgnnet
         move      c0 to nmrgFIL1
                              move      c0 to nmrgdpv
                              move      c0 to nmrgdisa
.Start patch 1.7 added
                              move      c0 to nmrgcnr
.Start patch 1.7 added
                              move      c0 to nmrgfil2
.                             move      c0,mergerej
         return

MergeVerifyData2
          if (ModFlag = YES)
                    move      OLRN,nmrglr
.                   endif
          clear    str8
          getitem   mergedisa,0,str8
          move      str8,nmrgdisa
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergedisa
                    return
          endif
.         
.Start patch 1.7 added
          clear    str8
          getitem   mergeCRN,0,str8
          move      str8,nmrgcnr
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergeCRN
                    return
          endif
.Begin patch 1.9

          clear    str8
          getitem   mergecusts,0,str8
          move      str8,nmrgcs
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergecusts
                    return
          endif
          clear    str8
          getitem   mergeunused,0,str8
          move      str8,nmrgudup
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergeunused
                    return
          endif
.end patch 1.9

.Start patch 1.7 added

.Begin patch sep 29 2011 
          getitem   mergenetoutput,0,str8
          move  str8,nmrgnet
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergenetoutput
                    return
          endif
.          FILEPI    1;nmrgfile
.   UPDATE    nmrgfile;nmrgvar
          CALL      nMRGUPD
.end patch sep 29 2011 
.          call     mergeEnablebuttons2
.         call      MERGEclearDEDUCTS
          call      mERGEEnableButtons
          setfocus mergeLR
          return
          endif
.
MergeVerifyData
.
          if (NewFlag = YES)
                    move      OLRN,nmrglr
          endif
.
.
          clear    str8
   getitem          mergeinputqty,0,str8
          move      str8,nmrgrqty
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergeinputqty
                    return
          endif
          clear    str8
          getitem   mergenetoutput,0,str8
          move  str8,nmrgnet
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergenetoutput
                    return
          endif

.         clear    str8
.         getitem   mergecustr,0,str8
.         move      str8,nmrgcust
.         type      str8
.         if not equal
.                   alert     note,"Invalid Quantity!!!",RESULT
.                   move      YES,ReturnFlag
.                   setfocus mergecustr
.                   return
.         endif
.
          clear    str8
          getitem   mergedead,0,str8
          move      str8,nmrgdisf
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergedead
                    return
          endif
          clear    str8
          getitem   mergedma,0,str8
          move      str8,nmrgdma
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergedma
                    return
          endif
.         clear    str8
.         getitem   mergedpv,0,str8
.         move      str8,nmrgdpv
.         type      str8
.         if not equal
.                   alert     note,"Invalid Quantity!!!",RESULT
.                   move      YES,ReturnFlag
.                   setfocus mergedpv
.                   return
.         endif
          clear    str8
          getitem   mergeerr,0,str8
          move      str8,nmrgerr
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergeerr
                    return
          endif
          clear    str8
          getitem   mergeintra,0,str8
          move      str8,nmrgid
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergeintra
                    return
          endif
          clear    str8
          getitem   mergemnf,0,str8
          move      str8,ncoamnf
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergemnf
                    return
          endif
          clear    str8
          getitem   mergenonp,0,str8
          move      str8,nmrgnper
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergenonp
                    return
          endif
          clear    str8
          getitem   mergeprison,0,str8
          move      str8,nmrgpris
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergeprison
                    return
          endif
          clear    str8
          getitem   mergez4rej,0,str8
          move      str8,nmrgz4
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergez4rej
                    return
          endif
          clear    str8
          getitem   mergeconv,0,str8
          move      str8,nmrgconv
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergez4rej
                    return
          endif
          clear    str8
          getitem   mergedisa,0,str8
          move      str8,nmrgdisa
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergedisa
                    return
          endif
.Begin patch 1.9

          clear    str8
          getitem   mergecusts,0,str8
          move      str8,nmrgcs
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergecusts
                    return
          endif
          clear    str8
          getitem   mergeunused,0,str8
          move      str8,nmrgudup
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergeunused
                    return
          endif
.end patch 1.9
.Start patch 1.7 added

          clear    str8
          getitem   mergeCRN,0,str8
          move      str8,nmrgcnr
          type      str8
          if not equal
                    alert     note,"Invalid Quantity!!!",RESULT
                    move      YES,ReturnFlag
                    setfocus mergeCRN
                    return
          endif
.Start patch 1.7 added
    move nmrgrqty,nmrgiqty
    call   nmrgwrt
          setprop   Mergelistview,enabled=1
   call     mergeclearlower
           call     mergecleardeducts
           call     mergedisablebuttons
                    setfocus mergeLR
          return

.Start patch 1.5
totrejects
                              move c0,mergerej
                    getitem   mergedead,0,str8
                              move c0,mergetmp
                              move str8,mergetmp
                              add mergetmp,mergerej
                              move mergerej,str8
                              setitem   Mergerejtot,0,str8
.
                    getitem   mergedma,0,str8
                              move c0,mergetmp
                              move str8,mergetmp
                              add mergetmp,mergerej
                              move mergerej,str8
                              setitem   Mergerejtot,0,str8
.
                    getitem   mergeerr,0,str8
                              move c0,mergetmp
                              move str8,mergetmp
                              add mergetmp,mergerej
                              move mergerej,str8
                              setitem   Mergerejtot,0,str8

                    getitem   mergeintra,0,str8
                              move c0,mergetmp
                              move str8,mergetmp
                              add mergetmp,mergerej
                              move mergerej,str8
                              setitem   Mergerejtot,0,str8

                    getitem   mergemnf,0,str8
                              move c0,mergetmp
                              move str8,mergetmp
                              add mergetmp,mergerej
                              move mergerej,str8
                              setitem   Mergerejtot,0,str8

                    getitem   mergenonp,0,str8
                              move c0,mergetmp
                              move str8,mergetmp
                              add mergetmp,mergerej
                              move mergerej,str8
                              setitem   Mergerejtot,0,str8


                    getitem   mergeprison,0,str8
                              move c0,mergetmp
                              move str8,mergetmp
                              add mergetmp,mergerej
                              move mergerej,str8
                              setitem   Mergerejtot,0,str8

                    getitem   mergez4rej,0,str8
                              move c0,mergetmp
                              move str8,mergetmp
                              add mergetmp,mergerej
                              move mergerej,str8
                              setitem   Mergerejtot,0,str8


                    getitem   mergeconv,0,str8
                              move c0,mergetmp
                              move str8,mergetmp
                              add mergetmp,mergerej
                              move mergerej,str8
                              setitem   Mergerejtot,0,str8

                    getitem   mergedisa,0,str8
                              move c0,mergetmp
                              move str8,mergetmp
                              add mergetmp,mergerej
                              move mergerej,str8
                              setitem   Mergerejtot,0,str8

                    getitem   mergeCRN,0,str8
                              move c0,mergetmp
                              move str8,mergetmp
                              add mergetmp,mergerej
                              move mergerej,str8
                              setitem   Mergerejtot,0,str8
                              return
.End patch 1.5 added

.begin patch 5.86

DELREC
        setprop Passwrd,visible=1
        move    NPASUSER,str10
.Test for Credit Password
mergedel
.        unpack  NPASFLD,str1,NPASKEY
        unpack  NPASFLD,NPASKEY
        pack    NPASFLD,"J",NPASKEY
        reset   NPASFLD
        call    NPASKEY
        if not over
                move    YES,HoldFlag
                alert   note,"Password Accepted!",result
        else
                move    str10,NPASUSER
                      return
        endif
   alert  plain,"ARE YOU SURE YOU WANT TO DELETE THIS RECORD?",result
         cmatch    yes to str1
          if (result <> 1)
.                   setfocus mergeLR
                       return
                     endif
.end patch 5.86
         move      nmrgfld to ninvfld
         rep       zfill in ninvfld
         call      ninvkey
          if        not over
   alert  plain,"PREVIOUSLY BILLED!!!/SKIP IT?",result
         cmatch    yes to str1
          if (result = 1)
         RETURN
                              ENDIF
                              ENDIF
        call      nmrgdel
                              alert     note,"RECORD DELETED!",result
.          call     mergeclearlower
.          call     mergecleardeducts
.         mergeListView.DeleteAllItems giving N9
.          call     mergeenablebuttons
.         setfocus MergeSearchListView
.         call      Click_MergeSearchListView
          call      MERGEDISABLEButtons2
          call      MERGEDisableLower
          call      MERGEclearDEDUCTS
          call      mERGEEnableButtons
          move      NO,NewFlag
         return
.
.                             ENDIF
.         GOTO      KEY
io       display   *p1:24,*el,"Error opening the daily file",error,*w5
         stop
.faxspool - Facsys printer not available/defined do it the old way.

.axspool  trapclr  spool
.         move     yes to badfaxflag
.         return

EditGo
HelpGo
        setprop AboutMssg,visible=1
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
.START PATCH 2.51 REPLACED LOGIC
.        if (ExitFlag = "Y")
        if (ExitFlag = "Y" & ExitFlag4 = "Y")
.END PATCH 2.51 REPLACED LOGIC
                winshow
                stop
        endif
MergeTabChange
        IF (N1 = C1)
                move    C2,TabNum
                Activate Merge2
.These setprops reset visibility of original form
                else
                DEActivate Merge3
                endif
                                                   return
.
STOP     STOP
.begin patch 1.8
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"NMgr0001 - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "dherric@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    mailattach,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    endif
          
                    goto      checkfile

.end patch 1.8
XRESIZE
           Nmrg0001.Scale
           RETURN

         INCLUDE   nmrgIO.INC
           INCLUDE   NRTNIO.INC
           INCLUDE   NORDIO.INC
.           include   nmlrio.inc
.Patch5.88
                              include   compio.inc
                              include   cntio.inc
.           include   nmlrio.inc
.Patch5.88
         INCLUDE   NOWNIO.INC
         INCLUDE   NUSEIO.INC
.         INCLUDE   DISPORD.INC
         INCLUDE   NSPEIO.INC
         INCLUDE   NSPIIO.INC
.         INCLUDE   NBRKIO.INC
         include   npasio.inc
.begin patch 1.3
.         include   ninvIO.inc
          include             ninvio.inc
.end patch 1.3
         include   hpio.inc
.START PATCH 5.83 REPLACED LOGIC
         INCLUDE   NCNTIO.INC
.END PATCH 5.83 REPLACED LOGIC
         INCLUDE   COMLOGIC.INC