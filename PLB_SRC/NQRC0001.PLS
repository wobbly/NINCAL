          include   common.inc
          include   cons.inc
          include   nQRCdd.inc

PC      EQU     1

release   INIT      "1.0"     DLH 
Reldate   Init      "1 June 2000"

NewFlag init        "N"
ReturnFlag init     "N"
hold      dim       81        .Length of NQrcVARS
holdkey   dim       4
.
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
.
white     color
grey      color

;Set Up Menu Bar
mFile    menu
mHelp    menu
;Set Up SubMenu for Options
mOptions Menu
sColor  submenu
sSearch submenu
;
;Present Data for Menu Bar
FData   init    "&File;E&xit"
HData   init    "&Help;&About"

        create  font2,"Arial",size=8,bold=0
        create  white=*white
        create  grey=220:220:220

err     plform  Error
abt            plform         About
x         plform    NQrc0001
        move    "Nqrc0001.PLS",Wprognme
        move    "Datacard Quick Reco",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        move    Reldate to Wreldate




          winhide
          formload x
          formload err
                    formload       abt

        CREATE  NQrc0001;MFile,FData
        create  NQrc0001;mHelp,HData,MFIle

;FileGo leads to stop
        activate mFile,FileGo,result
        activate mHelp,HelpGo,result


.Create Column Headers for PackageListView
        getprop NQrcListView,*ColumnHeaders=ColHeads
.I hide the first item as I have not yet figured out if I can change the ForeColor of that item, since it does not appear to be a sub-item.
          ColHeads.Add using *Index=1,*Key="one",*Text="Num",*Width=40
          ColHeads.Add using *Index=2,*Key="two",*Text="Description",*Width=175
.Set some properties for ListView object
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
        create  OBOOL,VarType=VT_BOOL,VarValue=0
.
          setprop   NQrcListView,*HideColumnHeaders=OFALSE
          setprop   NQrcListView,*HideSelection=OFALSE
.         setprop   NQrcListView,*HotTracking=OTRUE
          setprop   NQrcListView,*FullRowSelect=OTRUE
.         setprop   NQrcListView,*MultiSelect=OTRUE
          setprop   NQrcListView,*Sorted=OTRUE
          setprop   NQrcListView,*SortOrder=0
.         setprop   NQrcListView,*AllowColumnReorder=OTRUE
          setprop   NQrcListView,*LabelEdit=1
          setprop   NQrcListView,*View=3
          setprop   NQrcListView,*Font=font2
.
        getprop NQrcListView,*ListItems=ListIts

.Main Loop
          call      NqrcDisableLower
          loop
                    eventwait
          repeat

NQrcClearScreen
          setitem   NQrcComboCode,0,1
          setitem   NQrcEditDescription,0,""
          setitem   NQrcEditNumber,0,""
          setitem   NQrcEditText001,0,""
          return

NQrcEnableLower
          setprop   NQrcComboCode,bgcolor=white
          setprop   NQrcEditDescription,enabled=1,bgcolor=white
          setprop   NQrcEditNumber,bgcolor=white
          setprop   NQrcEditText001,enabled=1,bgcolor=white
          return

NQrcdisableLower
          setprop   NQrcComboCode,enabled=0,bgcolor=grey
          setprop   NQrcEditDescription,enabled=0,bgcolor=grey
          setprop   NQrcEditNumber,enabled=0,bgcolor=grey
          setprop   NQrcEditTExt001,enabled=0,bgcolor=grey
          move      NO,NewFlag
          return

NQrcdisableUpper
.         setprop   NQrcListView,enabled=0,bgcolor=grey
          return

NQrcEnableUpper
.         setprop   NQrcListView,enabled=1,bgcolor=white
          return

NQrcdisableUpperButtons
          setprop   NQrcOk,enabled=0
          setprop   NQrcNew,enabled=0
          setprop   NQrcModify,enabled=0
NQrcdisableUpperButtons2
          setprop   NQrcQuit,enabled=0
          setprop   NQrcSave,enabled=0
          setprop   NQrcDelete,enabled=0
          return

NQrcEnableUpperButtons
          setprop   NQrcOk,enabled=1
          setprop   NQrcNew,enabled=1
          return

NQrcEnableUpperButtons2
          setprop   NQrcQuit,enabled=1
          setprop   NQrcSave,enabled=1
          return

NQrcLoadListView
          pack      hold,NQrcdVARS
          ListIts.Add giving ListIt using *Index=1,*Text=NQRCDNum
          setprop ListIt,*SubItems(1)=NQrcdDESC
          return

NQrcLoadScreen
          move      C1,N2
          for N3,C1,"50"
                    getitem   NQrcComboCode,N3,str1
                    if (str1 = NQrcdNUM)
                              move      N3,N2
                              break
                    endif
          repeat
          setitem   NQrcComboCode,0,N2
.
          move      C1,N2
                    move      NQrcdNUM,str4
          setitem   NQrcEditDescription,0,NQrcdDESC
          setitem   NQrcEditNumber,0,str4
          
          return

NQrcVerifyData
          getitem   NQrcComboCode,0,result
          if (result = 1)
                    alert     caution,"Code is Required!",result
                    setfocus NQrcComboCode
                    move      YES,ReturnFlag
                    return
          endif
          getitem   NQrcComboCode,result,NQrcdNUM
.
          
.
          getitem   NQrcEditDescription,0,NQrcdDESC
          return

NQrccheckNumber
          return
;......................................................................................................
FileGo
                winshow
                stop
...........................................................................................................................
HelpGo
        setprop AboutMssg,visible=1
        return
..
          include   nqrcio.inc
          include   comlogic.inc
