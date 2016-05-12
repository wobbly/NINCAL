WARNING:
this program needs to have patches applied that are found in nordtest.pls
these include making sure the sorts perform correctly when refreshing material

PC        EQU       1
.......................................
. Program:      NSCR0005.PLS
. Function:     SCREEN 5 LITE Program
. Author:       David Strahan
. Orig. Date:   May 17, 2006
. Release:      1.0
.......................................
.Include Files
          include common.inc
          include cons.inc  //srchflag
          include   norddd.inc
          include   compdd.inc
          include   cntdd.inc
          include   ndatdd.inc
          include   nowndd.inc
          include   ncmpdd.inc
          include   nrtndd.inc
          include nordpdd.inc
          include nord4dd.inc
          include nspedd.inc
          include nord5dd.inc
          include nusedd.inc
          include nspe2dd.inc
          include nofrdd.inc
          include   ncntdd.inc
          include   winapi.inc
          include npnddd.inc
Release             Init      "1.0"
.
.Set Vars used for About Box
          move      "NSCR0005.PLS",Wprognme
          move      "Order Program - Screen 5",Wfunction
          move      "Andrew   Harkins/David Strahan",Wauthor
          move      release,Wrelease
          move      "April 25, 2006",Wreldate
          liston
IntegralStoreDetail external "INT001A;IntegralStoreDetail"
IntegralTestDetail external "INT001A;IntegralTestDetail"
.
.Files
ORDPRINT ifile      keylen=6,fixed=696
ORDPRNTA afile      fixed=696
.
T1        form      4  // nordtest.pls
L1        form      4  // nordtest.pls
hold2     dim       752       .FOR SPECIAL INSTRUCTIONS , 674         of which is modifiable,       78 of which is for XSTAT
Carr      init      0x7f  //nordtest.pls
DimPtr    Dim       ^
DimPtr1   Dim       ^
DimPtr2   Dim       ^
DimPtr3   Dim       ^
FrmPtr    form      ^
Hold5From dim       8         .For Screen 5 From Date
Hold5To   dim       8         .For Screen 5 To Date
LCRMod    form      "00"
RptCan    dim       1  // req for report2.plf
STYLE     INTEGER   1,"0x000104"   // nordtest
Hold5Date form      "0"       .For Screen 5 Date Selection - Record Date versus Clearance Date
StopFlag init       "N"       .used to break out of a       Query on Screen     5
RptType   form      "001"
userlogn dim        7
ClickFlag init      "N"    // used to see if columns have been reordered or not
NotesFlag init      "N"  // see if any notes were selected.  If not, do default
MouseForm form      10
EditPtr   EditText  ^
mFile     menu
mEdit     menu
mOptions menu
mHelp     menu
sSearch   submenu
.Present Data for Menu Bar
FData     init      "&File;E&xit"
EData     init      "&Edit;<1&Undo;-;<2&Cut       Ctrl+X;<3&Copy Ctrl+C;<4&Paste Ctrl+V;<5&Delete;-;<6&Select All"
OData     init      "&Options;&Search-F2;"
HData     init      "&Help;&About"
SData     init      ";&List;&Mailer"
.Screen   5
newdate1 dim        10        mm/dd/ccyy
prfile    pfile
ObjectColl          Collection
ComboBoxes          ComboBox (4)
Buttons             Button    (5)
StatTextBoxes       StatText (8)
MDateWindow         Window
CheckBoxes          CheckBox (3)

.
abt       plform    About
mss1      plform    Error
y         plform    Search
rpt2      plform    Report2
z         plform    NSCR0005
x         plform    NSCR005D
          winhide
          formload abt
          formload z
          formload mss1
          formload y
          formload rpt2
          formload x, NSCR0005
.
          open      ORDPRINT,"NINPRINT"
.         open      ORDPRINT,"NINPRINT.isi|10.10.30.103:502"
          open      ordprnta,"ninprint"
.         open      ordprnta,"ninprint.aam|10.10.30.103:502"
.
          Order5ListView.InsertColumn using "Mailer Name",80,1
          Order5ListView.InsertColumn using "List Name",80,2
          Order5ListView.InsertColumn using "LR",60,3
          Order5ListView.InsertColumn using "Quantity",70,4
          Order5ListView.InsertColumn using "Exc./Rent",60,5
          Order5ListView.InsertColumn using "Mail Date",70,6
          Order5ListView.InsertColumn using "LCR Date",70,7
          Order5ListView.InsertColumn using "Clear Stat",60,8
          Order5ListView.InsertColumn using "Clear Inits",60,9
          Order5ListView.InsertColumn using "Clr./Den./Fax Date",70,10
          Order5ListView.InsertColumn using "Contact",100,11
          Order5ListView.InsertColumn using "Caller",100,12
          Order5ListView.InsertColumn using "Sample",50,13
          Order5ListView.InsertColumn using "Status",80,14
          Order5ListView.InsertColumn using "Owner Name",80,15
          Order5ListView.InsertColumn using "Offer",80,16
          Order5ListView.InsertColumn using "Xstat",80,17
          Order5ListView.InsertColumn using "List/Owner",80,18
          Order5ListView.InsertColumn using "Mailer",45,19
          Order5ListView.InsertColumn using "",0,20   // hold yyyymmdd for mail date sort
          Order5ListView.InsertColumn using "",0,21   // yyyymmdd for lcr date
          Order5ListView.InsertColumn using "",0,22   // yyyymmdd for clr/den/fax date
.Set Following extended       properties for ListView:
.  FullRowSelect
.  DragAndDrop
.  OneClickActivate
hexer     integer   1,"0x0070"
          Order5ListView.SetExtendedStyle         giving N9 using     0,hexer
.Load Contact &     Caller ComboBox
          move      C1,N2
          move      "  ",str45
.Must delete blank items entered to ensure adequate space
          deleteitem Order5ComboCallerSearch,0
          deleteitem Order5ComboContactSearch,0
          insertitem Order5ComboCallerSearch,N2,str45
          insertitem Order5ComboContactSearch,N2,str45
          loop
                    move      C1,NCNTPATH
                    move      "Load-NCNTSEQ",Location
                    call      NCNTSEQ
                    until over
                    if (CNTCNT = "1")
                              pack      str45,CNTNAME,B1,CNTNUM
                              add       C1,N2
                              insertitem Order5ComboCallerSearch,N2,str45
                              insertitem Order5ComboContactSearch,N2,str45
                    endif
          repeat
          setitem   Order5ComboCallerSearch,0,1
          setitem   Order5ComboContactSearch,0,1
.
.Create   Menus
          create    NSCR0005;mFile,FData
          create    NSCR0005;mEdit,EData,mFile
          create    NSCR0005;mOptions,OData,mEdit
          create    NSCR0005;mHelp,HData,mOptions

.Create   SubMenu
          create    NSCR0005;sSearch,SData,mOptions,1
          // the above - create sSearch menu with SDATA, after mOptions, beginning at 1
.Activate Menus
.FileGo   leads to stop
          activate mFile,FileGo,result
.Need this when     it works
          activate mEdit,EditGo,result
.Only a   SubMenu   under this one
          activate mOptions,OptionsGo,result
          activate mHelp,HelpGo,result
.Activate SubMenus
          activate sSearch,SearchGo,result
          // upon activation, call SearchGo with result - here, result is 1 and 2
.
          clock     timestamp,timestamp
          loop
                    eventwait
          repeat
.
FileGo
          shutdown
          return
.
EditGo
          return
HelpGo
          setprop   AboutMssg,visible=1
          return
.
OptionsGo
          setmode *mcursor=*wait
          call "nordtest;OptionsGo"
          setmode *mcursor=*arrow
          return
.
SearchGo routine
          branch    result to SearchGo2,SearchGo3
SearchGo2 routine
.LIST
          move      C2,SrchFlag
          call      SearchSetTitle
          call      SearchSetVisible
          return
SearchGo3 Routine
.MAILER
.Needs to be a Routine as NPKG0001 will call it!
          move      C3,SrchFlag
          call      SearchSetTitle
          call      SearchSetVisible
          return
SearchLoad
.Called   by SearchDataList_DoubleClick
.Only load if in modify       mode
          move c0, howmany
          move c0, result
          if (SrchFlag = C2)
.LIST
                    getprop   Report2,visible=N6
                    if (N6 = C1)
                              unpack    Srchstr,str6
.                             setitem   EditTextBoxes(2),0,str6
                              return
                    endif
.Only Load if OK button       is activated
                    getprop   Order5OK,enabled=Howmany
                    if (Howmany = C1)
                              unpack    Srchstr,str6
                              setitem   Order5EditListSearch,0,str6
                              setfocus Order5EditListSearch
                    endif
                    return
          elseif (SrchFlag=C3)
.MAILER
          getprop   Report2,visible=N6
          if (N6 = C1)
                    unpack    Srchstr,str4
.                   setitem   EditTextBoxes(1),0,str4
                    return
          endif
                    getprop   Order5OK,enabled=Howmany
                    if (Howmany = C1)
                              unpack    Srchstr,str4
                              setitem   Order5EditMlrSearch,0,str4
                              setfocus Order5EditMlrSearch
                    endif
          return
          endif
.
Report2DestroyObjects
          destroy   ObjectColl
          return
.
OrderDisableScreen5All routine
          setprop   Order5OK,enabled=0
          setprop   Order5ListView,enabled=0
OrderDisableScreen5
          setprop   Order5ClearExchange,enabled=0
          setprop   Order5ClearRent,enabled=0
          setprop   Order5ClearExcRent,enabled=0
          setprop   Order5Email,enabled=0
          setprop   Order5Fax,enabled=0
          setprop   Order5Deny,enabled=0
          setprop   Order5EditSpecial1,enabled=0
          setitem   Order5EditSpecial1,0,""
          return
.
Order5SelectRecipient
          call      Report2DestroyObjects
          setprop   Report2,title="NIN Select Recipient"
          create    Report2;StatTextBoxes(1)=50:70:100:310,"Select Email Recipient(s)",""
          create    Report2;Buttons(1)=205:230:50:110,"&Contact(s)",zorder=500,default=1
          create    Report2;Buttons(2)=180:205:140:200,"C&aller(s)"
          activate StatTextBoxes(1)
          activate Buttons(1),Order5SelectContact,result
          activate Buttons(2),Order5SelectCaller,result
          listins   ObjectColl,StatTextBoxes(1),Buttons(1),Buttons(2)
          setfocus Buttons(1)
          setprop   Report2,visible=1
          return
.
Order5SelectContact
          move      C1,howmany
          setprop   Report2,visible=0
          return
.
Order5SelectCaller
          move      C2,howmany
          setprop   Report2,visible=0
          return
.
Order5SelectInstructions
          move      C0,howmany
          call      Report2DestroyObjects
          setprop   Report2,title="NIN File Select"
          create    Report2;StatTextBoxes(1)=50:70:50:325,"Select one or more categories in which to save notes:",""
          create    Report2;CheckBoxes(1)=120:150:25:100,"Internal Notes", zorder=500
          create    Report2;CheckBoxes(2)=120:150:125:200,"Spec. Instr.", zorder=500
          setitem CheckBoxes(2),0,C1  // default
          create    Report2;CheckBoxes(3)=120:150:225:300,"Mailer Notes", zorder=500
          activate StatTextBoxes(1)
          activate CheckBoxes(1)
          activate CheckBoxes(2)
          activate CheckBoxes(3)
          setfocus Report2OK
          setprop Report2OK, default=1
          listins   ObjectColl,StatTextBoxes(1),CheckBoxes(1),CheckBoxes(2),CheckBoxes(3)
          setprop   Report2,visible=1
          return
.
Order5TestBusy LRoutine       DimPtr
.str1 is used as NORDTST uses this variable to determine if Busy Byte is active
.Following Note     is obsolete as this routine was         pulled from NORD01D.PLF       in order
.to use   it in different     forms.
..NOTE:    #result CANNOT     be used   in this   routine   as its'   value needs to remain integral in calling routine!!!
          move      C1,NORDPATH
          move      DimPtr,NORDFLD
          move      "O.5TestBusy-NORDTST",Location
          pack      KeyLocation,"Key: ",NORDFLD
          call      NORDTST
          if over
                    clear     taskname
                    append    NORDFLD,taskname
                    append    " is not in Master Order File!!!",taskname
                    reset     taskname
                    alert     caution,taskname,N6
                    move      STAR,str1
          elseif (str1 = STAR)
                    clear     taskname
                    append    NORDFLD,taskname
                    append    " is currently in use.        Try again later!",taskname
                    reset     taskname
                    alert     note,taskname,N6
          endif
          return
.
OrderEnableScreen5
          setprop   Order5Fax,enabled=1
          setprop   Order5ClearRent,enabled=1
          setprop   Order5ClearExchange,enabled=1
          setprop   Order5ClearExcRent,enabled=1
          setprop   Order5Deny,enabled=1
          setprop   Order5EditSpecial1,enabled=1
.START PATCH        3.78.2    ADDED LOGIC
          setprop Order5ListView,enabled=1
.END PATCH          3.78.2    ADDED LOGIC
OrderEnableScreen5A
          setprop   Order5ListView,enabled=1
OrderEnableScreen5B
          setprop   Order5Email,enabled=1
          setprop Order5EditSpecial1, enabled=1
OrderEnableScreen5C
          setprop   Order5OK,enabled=1
          setprop   Order5Stop,enabled=0
          move      NO,StopFlag
          return
.
OrderLoadScreen5
          clear     str8
          append    OLRN,str8
          if (OSTAT = "p")
                    append    "p",str8
          endif
          if (LCRMod = 4)
.          if (LCRMod = 3)
                    TRAP      IOMssg Giving Error if IO
                    move      "O.LoadScr.5-NINPRINT",Location
                    pack      KeyLocation,"Key: ",OLRN
                    if (OSTAT = "l")
                              read      ordprint,OLRN;;
                              if not over
                                        append    STAR,str8
                              endif
                    else      .OSTAT = "p"
                              read      ordprint,OLRN;str55,str55,str55,str10,str1;
                              if not over
                                        if (str1 = "*")
                                                  append    STAR,str8
                                        endif
                              endif
                    endif
                    TRAPCLR   IO
          endif
.ListView #1
          reset     str8  // from above
          clear     MKEY
                    pack      MKEY,OMLRNUM,"000"
                    rep       zfill,MKEY
                    move      "O.Load5-NMLRKEY",Location
                    pack      KeyLocation,"Key: ",MKEY
                    call      NMLRKEY
                    if over
                              clear     MCOMP
                    endif
          Order5ListView.InsertItem giving N9 using MCOMP
          Order5ListView.SetItemText using N9,str8,2
          pack      str15,OLNUM,SLASH,OLON
          Order5ListView.SetItemText using N9,str15,17
          Order5ListView.SetItemText using N9,OMLRNUM,18
          Order5ListView.SetItemText using N9,OQTY,3
          if (OELCODE = "2" | OELCODE = "3")
                    if (ORENT = "1")  .LCR Rental
                              move      "Exc/Rent",str10
                    else
                              move      "Exchange",str10
                    endif
          else
                    move      "Rental",str10
          endif
          Order5ListView.SetItemText using N9,str10,4
          call      TRIM using OMDTEM
          count     N2,OMDTEM
          if (N2 <> 0 AND     OMDTEM <> "00")
                    pack newdate1, OMDTEC,OMDTEY,OMDTEM,OMDTED,"00000000"
                    Order5ListView.SetItemText using N9,newdate1,19
                    clear newdate1
                    pack      newdate1,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
          else
                    clear     newdate1
          endif
          Order5ListView.SetItemText using N9,newdate1,5
          call      TRIM using OODTEM
          count     N2,OODTEM
          if (N2 <> 0 AND     OODTEM <> "00")
                    pack newdate1, OODTEC,OODTEY,OODTEM,OODTED,"00000000"
                    Order5ListView.SetItemText using N9,newdate1,20
                    clear newdate1
                    pack      str11,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
          else
                    clear     str11
          endif
          Order5ListView.SetItemText using N9,str11,6
          pack      NCNTFLD,OCOCODE
          move      C1,NCNTPATH
          move      "O.Load5-NCNTKEY",Location
          pack      KeyLocation,"Key: ",NCNTFLD
          call      NCNTKEY
          if over
                    clear     str35
          else
                    move      CNTNAME,str35
          endif
          clear     str12
          if (OCLRSTAT = "1")
                    move      "Exchange",str12
          elseif (OCLRSTAT = "2")
                    move      "Rental",str12
          elseif (OCLRSTAT = "3")
                    move      "Exc/Rent",str12
          elseif (OCLRSTAT = "4")
                    move      "Denied",str12
          endif
          Order5ListView.SetItemText using N9,str12,7
          Order5ListView.SetItemText using N9,OCLRINIT,8
          clear     str16
          call      TRIM using OCLRDTEM
          count     N2,OCLRDTEM
          if (N2 <> 0 AND     OCLRDTEM <> "00")
                    pack newdate1, OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,"00000000"
                    Order5ListView.SetItemText using N9,newdate1,21
                    clear newdate1
                    append    OCLRDTEM,str16
                    append    SLASH,str16
                    append    OCLRDTED,str16
                    append    SLASH,str16
                    append    OCLRDTEC,str16
                    append    OCLRDTEY,str16
                    if (OBRKRPT = "1")
                              append    STAR,str16
                    endif
                    reset     str16
          elseif (OBRKRPT     = "1")
                    append    STAR,str16
                    reset     str16
          endif
          Order5ListView.SetItemText using N9,str16,9
          Order5ListView.SetItemText using N9,str35,10
          pack      NCNTFLD,OCO2CODE
          move      C1,NCNTPATH
          move      "O.Load5-2-NCNTKEY",Location
          pack      KeyLocation,"Key: ",NCNTFLD
          call      NCNTKEY
          if over
                    clear     CNTNAME
          endif
          Order5ListView.SetItemText using N9,CNTNAME,11
          Order5ListView.SetItemText using N9,OSAMCDE,12
          clear     NPNDDESC
          if (OSTAT = "l"     | OSTAT   = "z")
                    move      OLRN,NORD5FLD
                    if (NORD5FLD <>     "")
                              rep       zfill in NORD5FLD
                              clear     NORD5STAT
                              move      "O.Load5-NORD5KEY",Location
                              pack      KeyLocation,"Key: ",NORD5FLD
                              call      NORD5KEY            .get LCR info
                              if not over                                 .File is out of whack!
                                        move      "l",str1
                                        pack      NPNDFLD   from str1,NORD5STAT
                                        rep       zfill in NPNDFLD
                                        move      "O.PendingStatus-NPNDKEY",Location
                                        pack      KeyLocation,"Key: ",NPNDFLD
                                        call      NPNDKEY
                                        if over
                                                  clear     NPNDDESC
                                        endif
                              endif
                    endif
          else
                    move      OLRN,NORD4FLD
                    if (NORD4FLD <>     "")
                              rep       zfill in NORD4FLD
                              clear     NORD4STAT
                              move      "O.Load5-NORD4KEY",Location
                              pack      KeyLocation,"Key: ",NORD4FLD
                              call      NORD4KEY            .get LCR info
                              if not over                                 .File is out of whack!
                                        if (OSTAT = "p"     | OSTAT   = "x")
                                                  move      "p",str1
                                        endif
                                        pack      NPNDFLD   from str1,NORD4STAT
                                        rep       zfill in NPNDFLD
                                        move      "O.PendingStatus-NPNDKEY",Location
                                        pack      KeyLocation,"Key: ",NPNDFLD
                                        call      NPNDKEY
                                        if over
                                                  clear     NPNDDESC
                                        endif
                              endif
                    endif
          endif
          Order5ListView.SetItemText using N9,NPNDDESC,13
          Order5ListView.SetItemText using N9,O1DES,1
          move      OLON,NOWNFLD
          rep       ZFILL,NOWNFLD
          move      "O.Load5-NOWNKEY",Location
          pack      KeyLocation,"Key: ",NOWNFLD
          call      NOWNKEY
          if over
                    clear     OWNOCPY
          endif
          Order5ListView.SetItemText using N9,OWNOCPY,14

          BUMP      OODNUM BY 4
          pack      NOFRFLD,OMLRNUM,OODNUM
          reset     OODNUM
          rep       zfill in NOFRFLD
          move      "O.Load5-NOFRKEY",Location
          pack      KeyLocation,"Key: ",NOFRFLD
          call      NOFRKEY
          if over
                    clear     OFDESC
          endif
          Order5ListView.SetItemText using N9,OFDESC,15
          clear     DESC001
          move      C3,NSPELOCK
          pack      NSPEFLD,OLRN
          rep       zfill,NSPEFLD
          move      "O.Load5-NSPEKEY",Location
          pack      KeyLocation,"Key: ",NSPEFLD
          call      NSPEKEY
          if over
                    clear     DESC001
          endif
          Order5ListView.SetItemText using N9,DESC001,16
          return

OrderLoadStatScreen5
          setitem   Order5StatListName,0,O1DES
          setitem   Order5StatOwnerName,0,OWNOCPY
          setitem   Order5StatMailerName,0,MCOMP
          setitem   Order5StatOfferName,0,OFDESC
          setitem   Order5StatSpecial1,0,DESC001
          pack      NSPEFLD,OLRN
          rep       zfill,NSPEFLD
          clear     DESC002
          if (NSPEFLD <> "")
                    move      C3,NSPELOCK
                    move      "O.Load5,2-NSPEKEY",Location
                    pack      KeyLocation,"Key: ",NSPEFLD
                    call      NSPEKEY
          endif
          pack      NSPE2FLD,OLRN
          rep       zfill,NSPE2FLD
          clear     DESC003
          clear     DESC004
          if (NSPE2FLD <>     "")
                    move      C3,NSPE2LOCK
                    move      "O.Load5,2-NSPE2KEY",Location
                    pack      KeyLocation,"Key: ",NSPE2FLD
                    call      NSPE2KEY
          endif
GetRadioButtons
          getitem Order5RadioSpecInstr,0,N1
          if (N1=1)
                    setitem Order5EditTextNotes,0,Desc002
                    return
          endif
          getitem Order5RadioIntNotes,0,N1
          if (N1=1)
                    setitem Order5EditTextNotes,0,Desc003
                    return
          endif
          getitem Order5RadioMlrNotes,0,N1
                    if (N1=1)
                    setitem Order5EditTextNotes,0,Desc004  // mailer notes
                    return
          endif
          return
.
          include nofrio.inc
          include npndio.inc
          include nspe2io.inc
          include nuseio.inc
          include nord5io.inc
          include nspeio.inc
          include nord4io.inc
          include nordpio.inc
          include   searchio.inc
          include   nordio.inc
          include   compio.inc
          include   cntio.inc
          include   ndatio.inc
          include   nownio.inc
          include   ncmpio.inc
          include nrtnio.inc
          include   ncntio.inc
          include comlogic.inc