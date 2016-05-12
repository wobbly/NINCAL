          include        common.inc
            include        cons.inc
          include   winapi.inc
            include        ncntdd.inc
               include        MrktLstdd.inc
               include        gnxtdd.inc
               INCLUDE       PRTPAGEDD.INC
Release   Init      "1.23"    DLH .excel 2013 Issue
reldate   Init      "2014 January 22"
.release        Init            "1.22"          DLH added new fields
.Reldate    INit         "Jan 05 2010"
.release        Init            "1.21"          DLH added Broker Lists changed report output to c:\work
.Reldate   INit         "May 22 2008"
.release        Init            "1.20"          DLH fix select and sorts
.Reldate   INit         "April 12 2008"
.release        Init            "1.18"          23JAN2005 DMB Change dir due to file folder restructure
.Reldate   INit         "October 4 2006"
.release        Init            "1.17"          23JAN2005 DMB Change dir due to file folder restructure
.Reldate   INit         "September 20 2006"
.release        Init           "1.16"          23DEC2004 ASH Work Order 591 - Increase robustness of Patch 1.14
.release        Init           "1.15"          14DEC2004 ASH Moved PS Label print columns over 150 units (HIENGLISH = 1/8 inch)
.release        Init           "1.14"          05OCT2004 DMB Cleaned up code and added option to dump client file to xls
.release        Init           "1.13"          11DEC2003 DMB Added code to setfocus on listview in maintenance screen when a item is selected
.release        Init           "1.12"          11Jul2002 DLH  add print counter
.select and/or operator
.release        Init           "1.11"          21May2002 DLH  Clean up Findit 1 & 2
.release        Init           "1.1"          13May2002 DLH  add masking on add
.                                            fix records with 5 byte id num
.release        Init           "1.0"

.Set Vars used for About Box
               move    "Mrkt0001.PLS",Wprognme
               move    "Marketing List Maint.",Wfunction
               move    "David Herrick",Wauthor
               move    Release,Wrelease
.               move    "May 21 2002",Wreldate
          Move      Reldate,WReldate
.NewNum   Dim       13
formstuff
.ErrorMssgEditTextBox that is created and destroyed dynamically
.Note that this EditText fills spot for ErrorMssgStat4!!
ErrorMssgEdit1  EditText

.Colors
white   color
grey    color
RED     COLOR
BLACK   COLOR
Yellow  Color

.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
fontB   font

.Pointers for accessing ComboBoxes
ComboPtr ComboBox       ^

.Set Up Menu Bar
mFile   menu
mEdit   menu
mHelp   menu

.Present Data for Menu Bar
FData   init    "&File;&Print;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
HData   init    "&Help;&About"
.some goodies for on moving Icon
ICONID   FORM      4
CURICON  FORM      4
FRAMES   FORM      4
.ICON$ANIM    ICON
FORM32   FORM      32
FORM32A  FORM      32
FORM32B  FORM      32
Dim20          dim            20
Area           Dim            3                .Hold Area code
.
endindex form      9
.tempfile file
tempfile2 file
CurRec    form    5.2
CurVal    form      3
LastVal   form      3
RecVal    form      9

DimPtr    dim       ^
DimPtr1   dim       ^
FrmPtr    form      ^
FrmPtr1   form      ^
ExitFlag init       "Y"
ReturnFlag dim      1
TIMER     timer
.hexeight integer 4,"4294967295"
userlogn dim    7
DefTest   form      "1"
DefDate   dim       10
DefDate2 form       5
newNum         Dim            13
Mode           dim            1           A=add M=modify
MaskFlag       dim            1            . Y = yes mask current record during add.
AlertStyle     integer        1,"0x000004"
SlctInfo       Dim            200          .select info for sort
SortInfo       Dim            200             .sort info for sort
SortVar        Dim            500
Tabnum         form          2
SaveTab        form          2
ListSlctCount  Form           2
DatacardSw     Dim            1
PromoSw        Dim            1
HolidaySw       Dim            1
NewLtrSw       Dim            1
.begin patch 1.21
BrkrSw    Dim       1
.end patch 1.21
.begin patch 1.22
ActiveSw    Dim       1
VendSw    Dim       1
ProsSw    Dim       1
.end patch 1.22
PartySw        Dim            1
CodeSw         Dim            1
BarCodeSw      Dim            1
Page           Form             4
ReportTitle    Dim            50
ReportSw       form           1           .report type detail,summary,labels
SortFlag       Form           1           1=primary sort by Comp, 2=Primary sort by Contact
ZipMask        dim            10
BarCode        dim            12
LabelCount     FOrm           3
ColumnCount    Form           1
ColumnB        Form           5
RowCount       form           2
Row1           Form           "475"
Row2           Form           "1475"
Row3           Form           "2475"
Row4           Form           "3475"
Row5           Form           "4475"
Row6           Form           "5475"
Row7           Form           "6475"
Row8           Form           "7475"
Row9           Form           "8475"
Row10          Form           "9475"
.Begin patch 1.12
PrintCount          Form      5
SelectOp  dim       1                 .selection operator   | = or  & = and
.end patch 1.12
STRYes    Init      "'Y'"
.
.Patch 1.14
XlsFile   file
.Patch 1.14
.START PATCH 1.16 ADDED LOGIC
books   automation
book    automation
sheets  automation
sheet   automation
sortcol automation
sortcol1 automation
ex      automation      class="Excel.Application"
VT_BOOL EQU 11
OFALSE  variant
xlMinimized integer 4,"0xFFFFEFD4"
.END PATCH 1.16 ADDED LOGIC
.ICON PLFORM    ANIMATE  * CONTAINS ALL THE ICONS
mss1             plform  Error
abt              plform  About
Report           plform  Mrkt0003
Maint            Plform  Mrkt0002
x                plform  Mrkt0001
                 winhide
.
.Load Forms, Always load parent form first
        formload              x
        Formload              Maint,Mrkt0001
        Formload              Report,Mrkt0001
        formload              abt
        formload              mss1
.        FORMLOAD              ANIMICON
.Create Menus
        create  Mrkt0001;mFile,FData
        create  Mrkt0001;mEdit,EData,mFile
        create  Mrkt0001;mHelp,HData,mEdit

.        activate Default_Resource
.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
        activate mEdit,EditGo,result
        activate mHelp,HelpGo,result


.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=*ltgray
        create  RED=*RED
        create  Yellow=*Yellow
        create  black=*black

.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic
        create  font5,"Arial",size=10
        Trap    fontErr if object
        create  fontB,"3 of 9 Barcode",size=12
        trapclr Object
..........................................................
                move    C2,n1
                call    Mrkt001TABCLICK
                move    C1,n1
                call    Mrkt001TabCHANGE
...........................................
          call      MrktInitProgressBar
.         setitem   MrktStatProgress,0,""
.              setitem        MrktStatProgress,0,"Loading Instructions"
              move          "\\nins1\e\data\text\MrktList.dat" to str45
.              move          "c:\data\FaxPromo.dat" to str45
              open          tempfile,str45
          positeof      tempfile
          fposit    tempfile,N10
          calc      howmany=(N10/288)   .'288 = 286(NIMrktST record length) + 2 bytes for CR/LF
              close         tempfile

        Mrkt002ListView001.InsertColumn using "Company",160,1
        Mrkt002ListView001.InsertColumn using "Contact",140,2
        Mrkt002ListView001.InsertColumn using "ID Num",40,3

        Mrkt002ListView002.InsertColumn using "Contact",160,1
        Mrkt002ListView002.InsertColumn using "Company",140,2
        Mrkt002ListView002.InsertColumn using "ID num",40,3

        move    "Driver Prep-MrktSEQ",Location
.hide all until table is loaded
        setprop Mrkt002ListView001,visible=0
        setprop Mrkt002ListView002,visible=0


. test               open           MrktfList
.
Looper
.Dynamically reset Animate as same size as Main window
anim1
        getprop Mrkt0001,height=H
.        setprop Animate,height=H
        getprop Mrkt0001,width=V
.        setprop Animate,width=V
.Must clear Resize Event which was tiggered when Animate was resized,
.as this is where the AnimateIt subroutine sits.
        clearevent
.        moveaddr Mrkt0001,AnimateWindow
.        move    C1,AnimateCurIcon
.        move    C4,AnimateFrames
.        move    C0,AnimateIconID
.        move    "400",H
.        move    "540",V
        loop
.                CALL   ANIMATEIT
.                read           Mrktfile,seq;Mrktvars
               call           mrktks
               until over
.Update Progress Bar
            call    MrktUpdateProgressBar
.load contact combo
               clear          str45
               move           b25 to str45
               insertitem     Mrkt002ComboBox001,N2,str45
        loop
                move          C1,NCNTPATH
                move          "Load-NCNTSEQ",Location
                call          NCNTSEQ
                until         over
                if            (CNTCNT = "1")
                        pack    str45,CNTNAME,B1,CNTNUM
                        add     C1,N2
                        insertitem Mrkt002ComboBox001,N2,str45
                endif
        repeat
        setitem Mrkt002ComboBox001,0,1


................................
                call          trim using MrktCoName
                If            (MrktCoName <= " ")   .No company name
                Mrkt002ListView001.InsertItem giving N9 using MrktCntName
                else
                Mrkt002ListView001.InsertItem giving N9 using MrktCoName
                endif
                Mrkt002ListView001.SetItemText using N9,MrktCntName,1
                Mrkt002ListView001.SetItemText using N9,MrktIdnum,2
                Mrkt002ListView001.EnsureVisible using N9,0

                Mrkt002ListView002.InsertItem giving N9 using MrktCntName
                Mrkt002ListView002.SetItemText using N9,MrktCoName,1
                Mrkt002ListView002.SetItemText using N9,MrktIdnum,2
                Mrkt002ListView002.EnsureVisible using N9,0
               repeat
.START PATCH 1.16 ADDED LOGIC
          move      "100",CurVal
          setitem   Mrkt002ProgressBar,0,CurVal
.END PATCH 1.16 ADDED LOGIC

.cleanup
.               destroy AnimateIcon
               setprop Mrkt002ListView001,visible=1
          Mrkt002ListView001.EnsureVisible using c1,0
          Mrkt002ListView001.GetItemText giving MrktFld using c1,c2
          Mrkt002ListView001.SetItemState giving N9 using 1,2,2
          setfocus      Mrkt002ListView001
               setProp        Mrkt002Add,visible=1
               setProp        Mrkt002Remove,visible=1
               setProp        Mrkt002Quit,visible=0

.START PATCH 1.16 ADDED LOGIC
          create    OFALSE,VarType=VT_BOOL,VarValue=0
.END PATCH 1.16 ADDED LOGIC

.main loop
.Set Error Message Stat Text Boxes
.        call    SetMrktErrorMssgDefault
         loop
         waitevent
         repeat



               stop
.................................................................................
DisplayDetail
               packkey        mrktfld from str6
               rep            zfill in mrktfld
               call           MrktKey
               If             Over
               alert          caution,"I am lost, & exiting program. Location DisplayDetail !",result
               stop
               endif
               If             (Mrktfld <> MRktidnum)
               alert          caution,"Isam is blown -- exiting program. Location DisplayDetail !",result
               stop
               endif
               setitem        Mrkt002EditText001,0,MrktCoName
               setitem        Mrkt002EditText002,0,MrktCntName

               setitem        Mrkt002EditText003,0,MrktAddr1
               setitem        Mrkt002EditText004,0,MrktAddr2
               setitem        Mrkt002EditText005,0,MrktCity
               setitem        Mrkt002EditText006,0,MrktState
               clear          str5
               clear          str4
               unpack         MrktZip into str5,str4
               setitem        Mrkt002EditText007,0,str5
               setitem        Mrkt002EditText008,0,str4
               setitem        Mrkt002EditText009,0,MrktCode
               Setitem              Mrkt002EditText010,0,MrktEmail
.begin patch 1.22
               setitem        Mrkt002EditText013,0,MrktComp
               Setitem        Mrkt002EditText014,0,MrktCnt

.end patch 1.22
.begin patch
        count   N2,MrktPhone
        if (N2 = "10")
                unpack  MrktPhone,area,str3,str4
                pack    Newnum,"(",area,")",str3,Dash,str4
                setprop Mrkt002EditText011,maxchars=13
        else
                setprop Mrkt002EditText011,maxchars=10
        endif
        setitem Mrkt002EditText011,0,Newnum
.
        clear   Newnum
        count   N2,MrktFax
        if (N2 = "10")
                unpack  MrktFax,area,str3,str4
                pack    Newnum,"(",area,")",str3,Dash,str4
                setprop Mrkt002EditText012,maxchars=13
        else
                setprop Mrkt002EditText012,maxchars=10
        endif
        setitem Mrkt002EditText012,0,Newnum
.End patch

               call           LoadCombo Using Mrkt002ComboBox001,MrktSls

               if             (MrktDtacrd = yes)          .datacard list
               setitem        Mrkt002Radio001,0,1
               else
               setitem        Mrkt002Radio001,0,0
               endif
               if             (MrktPromo = yes)
               setitem        Mrkt002Radio002,0,1
               else
               setitem        Mrkt002Radio002,0,0
               endif
               if             (MrktHlydy = yes)
               setitem        Mrkt002Radio003,0,1
               else
               setitem        Mrkt002Radio003,0,0
               endif
               if             (MrktNews = yes)
               setitem        Mrkt002Radio004,0,1
               else
               setitem        Mrkt002Radio004,0,0
               endif
               if             (MrktParty = yes)
               setitem        Mrkt002Radio005,0,1
               else
               setitem        Mrkt002Radio005,0,0
               endif
.begin patch 1.21

               if             (Mrktbrkr = yes)
               setitem        Mrkt002Radio006,0,1
               else
               setitem        Mrkt002Radio006,0,0
               endif
.end patch 1.21
.begin patch 1.22
               if             (MrktActive = yes)
               setitem        Mrkt002RadioAct,0,1
               else
               setitem        Mrkt002RadioAct,0,0
               endif
               if             (MrktPrsct = yes)
               setitem        Mrkt002RadioPros,0,1
               else
               setitem        Mrkt002RadioPros,0,0
               endif
               if             (MrktVndr = yes)
               setitem        Mrkt002RadioVndr,0,1
               else
               setitem        Mrkt002RadioVndr,0,0
               endif
.end patch 1.22

               return
LoadCombo       Routine ComboPtr,DimPtr
               move    C2,N2           .Must start with first legitimate entry!!
               clear   str2
               loop
                getitem ComboPtr,N2,str45
                unpack  str45,str35,str1,str2
                if (str2 = "" | str2 = " " | str2 = "  ")
                        move    C1,N2
                        break
                endif
                until (str2 = DimPtr)
                add     C1,N2
.Extra protection
                if (N2 >= 98)
                        move    C1,N2
                        break
                endif
        repeat
        setitem ComboPtr,0,N2
        return
.................................................................................
DataVerf
.the only thing we are really going to verify is that we have a good fax number and a contact or company
.after verify passes we will update the record - unless in add mode
               getitem        Mrkt002EditText001,0,MrktCoName
               getitem        Mrkt002EditText002,0,MrktCntName
               getitem        Mrkt002EditText003,0,MrktAddr1
               getitem        Mrkt002EditText004,0,MrktAddr2
               getitem        Mrkt002EditText005,0,MrktCity
               getitem        Mrkt002EditText006,0,MrktState
               getitem        Mrkt002EditText007,0,str5
               getitem        Mrkt002EditText008,0,str4
               pack           MrktZip from str5,str4
               getitem        Mrkt002EditText009,0,MrktCode
               Getitem              Mrkt002EditText010,0,MrktEmail
               Getitem              Mrkt002EditText011,0,MrktPhone
               Getitem              Mrkt002EditText012,0,MrktFax
.begin patch
        count   N2,MrktPhone
        if (N2 = "10")
                unpack  MrktPhone,area,str3,str4
                pack    Newnum,"(",area,")",str3,Dash,str4
                setprop Mrkt002EditText011,maxchars=13
        else
                setprop Mrkt002EditText011,maxchars=10
        endif
        setitem Mrkt002EditText011,0,Newnum
.
        clear   Newnum
        count   N2,MrktFax
        if (N2 = "10")
                unpack  MrktFax,area,str3,str4
                pack    Newnum,"(",area,")",str3,Dash,str4
                setprop Mrkt002EditText012,maxchars=13
        else
                setprop Mrkt002EditText012,maxchars=10
        endif
        setitem Mrkt002EditText012,0,Newnum
.End patch

              getitem         Mrkt002ComboBox001,0,N2
                       if (N2 > 1)
                getitem       Mrkt002ComboBox001,N2,str45
                       else
                getitem       Mrkt002ComboBox001,0,N2
                getitem       Mrkt002ComboBox001,N2,str45
                       endif
               unpack         str45,CNTNAME,str1,cntnum
               move           CntNum to MrktSls


               if             (MrktCoName = "" & MrktCntName = "")
               alert          caution,"A company or contact name required!",result
               setfocus       Mrkt002EditText001
               return
               endif

               getitem        Mrkt002Radio001,0,n1
               If             (n1 = c1)
               move           Yes to MrktDtacrd
               else
               clear          MrktDtacrd
               endif

               getitem        Mrkt002Radio002,0,n1
               If             (n1 = c1)
               move           Yes to MrktPromo
               else
               clear          MrktPromo
               endif

               getitem        Mrkt002Radio003,0,n1
               If             (n1 = c1)
               move           Yes to MrktHlydy
               else
               clear          MrktHlydy
               endif

               getitem        Mrkt002Radio004,0,n1
               If             (n1 = c1)
               move           Yes to MrktNews
               else
               clear          MrktNews
               endif

               getitem        Mrkt002Radio005,0,n1
               If             (n1 = c1)
               move           Yes to MrktParty
               else
               clear          MrktParty
               endif
.begin patch 1.21
               getitem        Mrkt002Radio006,0,n1
               If             (n1 = c1)
               move           Yes to MrktBrkr
               else
               clear          MrktBrkr
               endif
.end patch 1.21
.begin patch 1.22
          Getitem        Mrkt002RadioAct,0,N1
          If             (n1 = c1)
          Move      Yes,MrktActive
          else
          Clear     MrktActive
          endif

          Getitem        Mrkt002RadioPros,0,N1
          If             (n1 = c1)
          Move      Yes,MrktPrsct
          else
          Clear     MrktPrsct
          endif

          Getitem        Mrkt002RadioVndr,0,N1
          If             (n1 = c1)
          Move      Yes,MrktVndr
          else
          Clear     MrktVndr
          endif
          Getitem              Mrkt002EditText013,0,MrktComp
          Getitem              Mrkt002EditText014,0,MrktCnt

.end patch 1.22

..............................................
               If             (Mode = "A")
               move           "MRKTNEXT" to gnxtfld
               call           Gnxtkey
               move           GNXTNUM,Mrktfld
.
testnum
                rep            zfill in Mrktfld
                call          MrktTST
......................
                               If            over
                               move          Mrktfld to  MrktIdNum
                               call          Mrktwrt
                               move          gnxtnum to n6
                               add           C1,n6
                               move          n6 to gnxtnum
                               call          Gnxtupd
                               else
                               add           c1 to n6
                               move          n6 to Gnxtnum
                               move          GNXTNUM,Mrktfld
                               goto          testnum
                               endif
.......................
                Else
                call           Mrktupd
                call           Findit
                endif
.........................................
                Move          "M" to mode
                call          UpdateListView
                setProp       Mrkt002Add,visible=1
                setProp       Mrkt002Remove,visible=1
                setProp       Mrkt002Quit,visible=0
                return
.................................................................................
UpdateListView
.insert the updated item into all listviews
                Mrkt002ListView001.InsertItem giving N9 using MrktCoName
                Mrkt002ListView001.SetItemText using N9,MrktCntName,1
                Mrkt002ListView001.SetItemText using N9,MrktIdnum,2
                Mrkt002ListView001.EnsureVisible using N9,0

                Mrkt002ListView002.InsertItem giving N9 using MrktCntName
                Mrkt002ListView002.SetItemText using N9,MrktCoName,1
                Mrkt002ListView002.SetItemText using N9,MrktIdnum,2
                Mrkt002ListView002.EnsureVisible using N9,0
                return
*..............................................................................
MrktSortListView
.Dynamically sorts Different ListViews.
.In order to switch between different ListViews we need two pieces of information.
.We need to ascertain which column was clicked AND which ListView we currently
.have visible, as each ListView has its' columns ordered differently.
.Getprops will determine which ListView is currently active, #EventResult passed to result
.prior to calling this subroutine will determine which column was clicked.
        getprop Mrkt002ListView001,visible=N9
        if (N9 = C1)    .MrktListView001 is visible
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                if (result = 1)
                        setprop Mrkt002ListView001,visible=0
                        setprop Mrkt002ListView002,visible=1
                            Mrkt002ListView002.EnsureVisible using c1,0
                            Mrkt002ListView002.GetItemText giving Mrktfld using c1,c2
                            Mrkt002ListView002.SetItemState giving N9 using 1,2,2
                        setfocus Mrkt002ListView002
                endif
        else
                getprop Mrkt002ListView002,visible=N9
                if (N9 = C1)    .MrktListView002 is visible
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                        if (result = 1)
                                setprop Mrkt002ListView001,visible=1
                                setprop Mrkt002ListView002,visible=0
                      Mrkt002ListView001.EnsureVisible using c1,0
                      Mrkt002ListView001.GetItemText giving MrktFld using c1,c2
                      Mrkt002ListView001.SetItemState giving N9 using 1,2,2
                           setfocus Mrkt002ListView001
                            endif
               endif
               endif
*..............................................................................
RestoreFocus
.what view are we using?
               getprop        Mrkt002ListView001,visible=N8
               if             (n8 = c1)                    .was visible
               Mrkt002ListView001.EnsureVisible using c1,0
          Mrkt002ListView001.GetItemText giving MrktFld using c1,c2
          Mrkt002ListView001.SetItemState giving N9 using 1,2,2
          setfocus Mrkt002ListView001
               else
               Mrkt002ListView002.EnsureVisible using c1,0
          Mrkt002ListView002.GetItemText giving MrktFld using c1,c2
          Mrkt002ListView002.SetItemState giving N9 using 1,2,2
          setfocus Mrkt002ListView002
               endif
        return
..................................................................................
Findit
         call       findit1
         call       findit2
         return
...............................................................................
.findit 1 find the modified item in listview
findit1
               Mrkt002ListView001.getitemcount  giving endindex
               add     c1 to endindex
               move    c0 to n9
.begin patch 1.11
.findit1Lookup       Mrkt002ListView001.GetItemText giving str5 using N9,2
.         Match     MrktIDNum to str5
findit1Lookup       Mrkt002ListView001.GetItemText giving str6 using N9,2
          Match     MrktIDNum to str6
.end patch 1.11
               if        equal
               Mrkt002ListView001.DeleteItem Using N9
               else
               add     c1 to n9
                       if      (n9 <  endindex)
                       goto    findit1Lookup
                       return
                       endif
               endif
               return

.findit 2 find the modified item in listview2
findit2
        Mrkt002listview002.getitemcount  giving endindex
        add     c1 to endindex
        move    c0 to n9
.begin patch 1.11
.findit2Lookup       Mrkt002ListView002.GetItemText giving str5 using N9,2
.         Match     MrktIdnum to str5
findit2Lookup       Mrkt002ListView002.GetItemText giving str6 using N9,2
          Match     MrktIdnum to str6
.end patch 1.11
       if        equal
        Mrkt002ListView002.DeleteItem Using N9
        else
        add     c1 to n9
        if      (n9 <  endindex)
        goto    findit2Lookup
        return
        endif
        endif
        return

*..............................................................................

MrktUpdateProgressBar
          calc      CurRec=(CurRec+1)
          calc      CurVal=((CurRec/howmany)*100)
          if (CurVal <> LastVal)
                    setitem   Mrkt002ProgressBar,0,CurVal
                    move      CurVal,LastVal
          endif
          return
MrktInitProgressBar
          move      C0,CurRec
          move      C0,CurVal
          move      C0,LastVal
          return
.................................................................................
PrintPrep
.get and verify request
.sort file
.Invoke printer dialogue
.print
.First what records
.get report type
               move           c0 to reportsw
               Move           C0 to page
               getitem        Mrkt003Radio011,0,result        .detail list
               if             (result = c1)
               move           c1,reportsw
               MOVe           "800" to Row1
               MOVe           "1800" to Row2
               MOVe           "2800" to Row3
               MOVe           "3800" to Row4
               MOVe           "4800" to Row5
               MOVe           "5800" to Row6
               MOVe           "6800" to Row7
               MOVe           "7800" to Row8
               MOVe           "8800" to Row9
               Move           c0 to column
               endif
               getitem        Mrkt003Radio012,0,result        .summary listing
               if             (result = c1)
               move           c2,reportsw
               endif
               getitem        Mrkt003Radio013,0,result         .avery labels
               if             (result = c1)
               move           c3,reportsw
               MOVe           "475" to Row1
               MOVe           "1475" to Row2
               MOVe           "2475" to Row3
               MOVe           "3475" to Row4
               MOVe           "4475" to Row5
               MOVe           "5475" to Row6
               MOVe           "6475" to Row7
               MOVe           "7475" to Row8
               MOVe           "8475" to Row9
               MOVe           "9475" to Row10
.Patch 1.14
.               getitem        Mrkt003Radio014,0,result         .Barcodes ?
               getitem        Mrkt0003Check001,0,result         .Barcodes ?

.Patch 1.14
                              If             (Result = C1)
                              move           yes to BarCodeSw
                              else
                              move           No TO BarCodeSw
                              endif
               endif
.Patch 1.14 Code Added
               getitem        Mrkt003RadioXLS,0,result         .XLS
               if             (result = c1)
                         move           c4,reportsw
.>Patch 1.17 Modified Code                   
.                             Prepare   XLSfile,"\\nins1\d\users\promo\prmlisting.csv"
.                             Prepare   XLSfile,"\\nins1\d\promo\prmlisting.csv"
                              Prepare   XLSfile,"c:\work\prmlisting.csv"
.>Patch 1.17 Modified Code End                              
                 endif
.Patch 1.14 COde Added
               getitem        Mrkt003Radio015,0,result

               IF             (result = c1)
               move           "&" to Selectop
               else
               Move           "|" to Selectop
               endif

               If             (reportsw < c1)
               alert          caution,"You must select output type!",result
               setfocus       Mrkt003Radio011
               endif
               move           c0 to ListSlctCount
               getitem        Mrkt003Radio001,0,result
               IF             (result = c1)
               move           yes to DatacardSw           .datacard list is selected
               add            c1 to ListSlctCount       .track number of lists selected must be at least 1
               else
               Move           No to DatacardSw
               endif
.
               getitem        Mrkt003Radio002,0,result
               IF             (result = c1)
               move           yes to PromoSw           .Promo list is selected
               add            c1 to ListSlctCount       .track number of lists selected must be at least 1
               else
               Move           No to PromoSw
               endif
.
               getitem        Mrkt003Radio003,0,result
               IF             (result = c1)
               move           yes to HolidaySw           .Holiday list is selected
               add            c1 to ListSlctCount       .track number of lists selected must be at least 1
               else
               Move           No to HolidaySw
               endif
.
               getitem        Mrkt003Radio004,0,result
               IF             (result = c1)
               move           yes to NewltrSw           .NewsLetter list is selected
               add            c1 to ListSlctCount       .track number of lists selected must be at least 1
               else
               Move           No to NewLtrSw
               endif
.
.begin patch 1.21
               getitem        Mrkt003Radio014,0,result
               IF             (result = c1)
               move           yes to BrkrSw           .Broker list is selected
               add            c1 to ListSlctCount       .track number of lists selected must be at least 1
               else
               Move           No to BrkrSw
               endif
.end patch 1.21
.
.begin patch 1.22
               getitem        Mrkt003RadioAct,0,result
               IF             (result = c1)
               move           yes to ActiveSw           .Prospect list is selected
               add            c1 to ListSlctCount       .track number of lists selected must be at least 1
               else
               Move           No to ActiveSw
               endif

               getitem        Mrkt003RadioPrs,0,result
               IF             (result = c1)
               move           yes to ProsSw           .Prospect list is selected
               add            c1 to ListSlctCount       .track number of lists selected must be at least 1
               else
               Move           No to ProsSw
               endif

               getitem        Mrkt003RadioVndr,0,result
               IF             (result = c1)
               move           yes to VendSw           .Prospect list is selected
               add            c1 to ListSlctCount       .track number of lists selected must be at least 1
               else
               Move           No to VendSw
               endif
.end patch 1.22
               getitem        Mrkt003Radio009,0,result
               IF             (result = c1)
               move           yes to PartySw           .party list is selected
               add            c1 to ListSlctCount       .track number of lists selected must be at least 1
               else
               Move           No to PartySw
               endif
.
               clear          str1
               getitem        Mrkt003EditText001,0,str1
               IF             (str1 = "A" | str1 = "C" | str1 = "D" | str1 = "B" | str1 = "P" | str1 = "N")
               move           yes to CodeSw           .party list is selected
               add            c1 to ListSlctCount       .track number of lists selected must be at least 1
               else
               Move           No to CodeSw
               endif
.               endif
.
               if             (ListSlctCount < c1)
               alert          caution,"You must select at least one list to print!",result
               setfocus       Mrkt003Radio001
               return
               endif
.
.               if             (ListSlctCount > c1)
.              alert          caution,"You can't select more than one list to print!",result
.               setfocus       Mrkt003Radio001
.               return
.               endif
.
.build select for sort
               clear          SlctInfo
.Begin DH tries to cleanup 12 April 2008
          If        (ListSlctCount > c1)                              .multiple sources
.lets find the 1st list starting with datacards
                    If        (datacardsw = YES)
                    append    "S=#"268=",slctinfo                     ."
                    append    STRYES to Slctinfo
                    Elseif    (Promosw = YES)
                    append    "S=#"269=",slctinfo                     ."
                    append    STRYES to Slctinfo
                    Elseif    (Holidaysw = YES)
                    append    "S=#"270=",slctinfo                     ."
                    append    STRYES to Slctinfo
                    Elseif    (NewLtrsw = YES)
                    append    "S=#"271=",slctinfo                     ."
                    append    STRYES to Slctinfo
                    Elseif    (Partysw = YES)
                    append    "S=#"272=",slctinfo                     ."
                    append    STRYES to Slctinfo
.begin patch 1.21
                    Elseif    (Brkrsw = YES)
                    append    "S=#"382=",slctinfo                     ."
                    append    STRYES to Slctinfo
.end patch 1.21
.begin patch 1.22
                    Elseif    (Activesw = YES)
                    append    "S=#"276=",slctinfo                     ."
                    append    STRYES to Slctinfo
                    Elseif    (Prossw = YES)
                    append    "S=#"277=",slctinfo                     ."
                    append    STRYES to Slctinfo
                    Elseif    (Vendsw = YES)
                    append    "S=#"278=",slctinfo                     ."
                    append    STRYES to Slctinfo
.end patch 1.22
                    Elseif    (Codesw = YES)                         .can't be the first one if multiple but if we add more we woll need
                    append    "S=#"273=",slctinfo                     ."
                    append    STRYES to Slctinfo
                    endif
.so we have the first select lets get the rest              
                    If        (Promosw = YES)
                    append      Selectop to Slctinfo
                    append    "269=",slctinfo                         ."
                    append    STRYES to Slctinfo
                    endif

                    If        (Holidaysw = YES)
                    append      Selectop to Slctinfo
                    append    "270=",slctinfo                         ."
                    append    STRYES to Slctinfo
                    endif

                    If        (NewLtrsw = YES)
                    append      Selectop to Slctinfo
                    append    "271=",slctinfo                         ."
                    append    STRYES to Slctinfo
                    endif

                    If        (Partysw = YES)
                    append      Selectop to Slctinfo
                    append    "272=",slctinfo                         ."
                    append    STRYES to Slctinfo
                    endif
.begin patch 1.21

                    If        (Brkrsw = YES)
                    append      Selectop to Slctinfo
                    append    "382=",slctinfo                         ."
                    append    STRYES to Slctinfo
                    endif
.end patch 1.21
.begin patch 1.22
                    Elseif    (Activesw = YES)
                    append    "276=",slctinfo                     ."
                    append    STRYES to Slctinfo
                    Elseif    (Prossw = YES)
                    append    "277=",slctinfo                     ."
                    append    STRYES to Slctinfo
                    Elseif    (Vendsw = YES)
                    append    "278=",slctinfo                     ."
                    append    STRYES to Slctinfo
.end patch 1.22

                    If        (Codesw = YES)                         .can't be the first one if multiple but if we add more we woll need
                    append      Selectop to Slctinfo
                    append    "273=",slctinfo                         ."
                    append    STRYES to Slctinfo
                    endif
                    append    "#"" to SlctInfo                        ."
          Else                                              .single source
.lets find the list starting with datacards
                    If        (datacardsw = YES)
                    append      "S268=Y" to Slctinfo
                    Elseif    (Promosw = YES)
                    append      "S269=Y" to Slctinfo
                    Elseif    (Holidaysw = YES)
                    append      "S270=Y" to Slctinfo
                    Elseif    (NewLtrsw = YES)
                    append      "S271=Y" to Slctinfo
                    Elseif    (Partysw = YES)
                    append      "S272=Y" to Slctinfo
.begin patch 1.21
                    Elseif    (Brkrsw = YES)
                    append      "S382=Y" to Slctinfo
.end patch 1.21
.begin patch 1.22
                    Elseif    (Activesw = YES)
                    append    "S276=Y",slctinfo                     ."
                    append    STRYES to Slctinfo
                    Elseif    (Prossw = YES)
                    append    "S277=Y",slctinfo                     ."
                    append    STRYES to Slctinfo
                    Elseif    (Vendsw = YES)
                    append    "S278=Y",slctinfo                     ."
                    append    STRYES to Slctinfo
.end patch 1.22
                    Elseif    (Codesw = YES)
                    append      "S273=Y" to Slctinfo
                    endif
          endif
          reset          SlctInfo

.              if             (DatacardSw = Yes & ListSlctCount = C1)
.              append         "S268=Y" to Slctinfo
.              elseif         (DatacardSw = Yes )                     .more than one list selected
.              append         "S=#"268=",slctinfo                     ."
.              append               "'Y'" to Slctinfo
.              endif
.
.              if             (DatacardSw = Yes & PromoSw = Yes)
.              append         Selectop to Slctinfo
.              append         "269='Y'" to Slctinfo
.              elseif         (PromoSw = Yes & ListSlctCount = C1)
.               append         "S269=Y" to Slctinfo
.               Elseif         (PromoSw = Yes)
.               append         "S=#"269='Y'" to Slctinfo                        ."
.               endif
.
.               if             ((DatacardSw = Yes | PromoSw = Yes) & HolidaySw = Yes)
.               append         Selectop to Slctinfo
.               append         "270='Y'" to Slctinfo
.               elseif         (HolidaySw = Yes & ListSlctCount = C1)
.               append         "S270=Y" to Slctinfo
.               elseif         (HolidaySw = Yes)
.               append         "S=#"270=Y" to Slctinfo                          ."
.               endif
.
.               if             ((DatacardSw = Yes | PromoSw = Yes | HolidaySw = Yes) & NewLtrSw = Yes)
.               append         Selectop to Slctinfo
.               append         "271='Y'" to Slctinfo
.               elseif         (NewLtrSw = Yes & ListSlctCount = C1)
.               append         "S271=Y" to Slctinfo
.               elseif         (NewLtrSw = Yes)
.               append         "S=#"271=Y" to Slctinfo                          ."
.               endif
.               
.               if             ((DatacardSw = Yes | PromoSw = Yes | HolidaySw = Yes | NewLtrSw = Yes) & Partysw = Yes)
.               append         Selectop to Slctinfo
.               append         "272='Y'" to Slctinfo
.               elseif         (PartySw = Yes & ListSlctCount = C1)
.               append         "S272=Y" to Slctinfo
.               elseif         (PartySw = Yes)
.               append         "S=#"272=Y" to Slctinfo                          ."
.               endif
.               
.               clear          str1
.               getitem        Mrkt003EditText001,0,str1
.               if             ((DatacardSw = Yes | PromoSw = Yes | HolidaySw = Yes | NewLtrSw = Yes | Partysw = Yes) & CodeSw = yes)
.               append         Selectop to Slctinfo
.               append         "273='" to Slctinfo
.               append         str1 to Slctinfo
.               append         "'" to Slctinfo
.               elseif         (CodeSw = Yes)
.               append         "S273=" to Slctinfo
.               append         str1 to Slctinfo
.               endif
.               If              (ListSlctCount > c1)
.               append           "#"" to SlctInfo                     ."
.               endif
.               reset          SlctInfo
.End DH tries to cleanup 12 April 2008
.build sort info for sort
               clear          Sortinfo
               getitem        Mrkt003Radio005,0,result
               If             (result = c1)           .primary sort by Company name
               Move           c1 to SortFlag
               append         ",1-35" to sortinfo
.               append         ",1-50" to sortinfo
               endif
               getitem        Mrkt003Radio006,0,result
               If             (result = c1)           .primary sort by Contact name
               Move           c2 to SortFlag
               append         ",51-86" to sortinfo
.               append         ",51-100" to sortinfo
               endif
               getitem        Mrkt003Radio010,0,result
               If             (result = c1)           .primary sort by Zip
               Move           c3 to SortFlag
               append         ",253-261" to sortinfo
               endif
               getitem        Mrkt003Radio007,0,result
               If             (result = c1)           .2nd sort by Contact name
.               append         ",51-100" to sortinfo
               append         ",51-86" to sortinfo
               endif
               getitem        Mrkt003Radio008,0,result
               If             (result = c1)           .2nd sort by Company name
.               append         ",1-50" to sortinfo
               append         ",1-35" to sortinfo
               endif
               reset          sortinfo
               clear          SortVar
               append         "\\nins1\e\data\text\MrktList.dat,c:\work\MrktList.srt;" to sortvar
               append         SlctInfo to SortVar
               append         SortInfo to SortVar
               reset          SortVar
          Setitem   Mrkt003StatText005,0,"Selecting and Sorting"
               Sort           SortVar
               If             over
               Alert          Caution,Sortvar,result
               alert          caution,S$ERROR$,result
               return
               endif
...................
.ok lets print
.starting with a single list report and adding as we go.
.Patch 1.14  Code Added
          setitem   Mrkt003StatText005,0,"Creating / Printing report"
          if (reportsw <> c4)
                trap            PrintErr if SPOOL
                    pack    str55,"c:\work\pdf\barcode.pdf"
                    PRTOPEN Laser,"PDF:",str55
                prtpage         Laser;*UNITS=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT;
          endif
.Patch 1.14
                CLOCK           DATE,Today
                unpack          Today,mm,str1,dd,str1,yy
                open            Tempfile,"c:\work\MrktList.srt",exclusive
.                prtpage         Laser;*UNITS=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT;
                if             (ListSlctCount = c1)
                       if             (DatacardSw = Yes)
                       MOVE           "Marketing List - Weekly Datacard" to ReportTitle
                       Elseif         (PromoSw = Yes)
                       MOVE           "Marketing List - Promotional" to ReportTitle
                       elseif         (HolidaySw = Yes)
                       MOVE           "Marketing List - Holiday" to ReportTitle
                       elseif         (NewLtrSw = Yes)
                       MOVE           "Marketing List - Newsletter" to ReportTitle
                       elseif         (PartySw = Yes)
                       MOVE           "Marketing List - Party" to ReportTitle
.begin patch 1.21
                       elseif         (BrkrSw = Yes)
                       MOVE           "Marketing List - Broker" to ReportTitle
.end patch 1.21
.begin patch 1.22
                       elseif         (ActiveSw = Yes)
                       MOVE           "Active Client List" to ReportTitle
                       elseif         (VendSw = Yes)
                       MOVE           "Vendor List" to ReportTitle
                       elseif         (ProsSw = Yes)
                       MOVE           "Prospective Client List" to ReportTitle
.end patch 1.22
                       elseif         (CodeSw = Yes)
                       MOVE           "Marketing List - Marketing Code" to ReportTitle
                       endif
                Else
                Move            "Marketing Lists Report - Multilist select" to ReportTitle
                endif
.Patch 1.14
          if (reportsw <> c4)
                Call            Header
          endif
.Patch 1.14
PrintLoop       read            Tempfile,seq;Mrktvars
                Goto            PrintExit If over
                add             c1 to PrintCount

.
.this section is for Listings.
               If               (reportSw = C1)         .Detail listing
.
.
                              If             (Row > 9500 & Column > 4000)
.                              Prtpage         Laser;*p=1:10400,"Page ",page,*p=2500:10400,"Key ='D'=Datacards, 'P'=Promotional, ":
.                                              "'H'=Holiday, 'N'=Newsletter ,'Y'=Party, Broker Rank 'A-D'"
                              Prtpage         Laser;*p=1:10400,"Page ",page,*p=2000:10400,"Key ='D'=Datacards, 'P'=Promotional, ":
                                              "'H'=Holiday, 'N'=Newsletter ,'Y'=Party, 'V'=Vendor, 'T'=Prospect, 'a'=Active, Broker Rank 'A-D'"
                               prtpage         Laser;*NEWPAGE;
                               call           Header
                              endif
.
                              call           trim using MrktCntName
                              call           trim using MrktCoName
                              call           trim using MrktAddr1
                              call           trim using MrktAddr2
                              call           trim using MrktCity
                              count          n2 in MrktZip
                              if             (n2 = C9)
                              unpack         MrktZip into str5,str4
                              pack           ZipMask from str5,"-",str4
                              else
                              Move           MrktZip to Zipmask
                              endif
                              prtpage        Laser;*p=Column:row,*LL,*font=Font2,MrktCntName
                                             If             (MrktCoName <> "")
                                             add            "150" to Row
                                             prtpage        Laser;*p=Column:row,*LL,*font=Font2,MrktCoName
                                             endif
                              add            "150" to Row
                              prtpage        Laser;*p=Column:row,*LL,MrktAddr1
                                             If             (MrktAddr2 <> "")
                                             add            "150" to Row
                                             prtpage        Laser;*p=Column:row,*LL,*font=Font2,MrktAddr2
                                             endif
                              add            "150" to Row
                              prtpage        Laser;*p=Column:row,*LL,*font=Font2,MrktCity,", ",MrktState," ",ZipMask
.                              endif
...........
                                      Move           Column to ColumnB
                                      add            "2000" to columnB
                                      if             (MrktDtacrd = Yes)
                                      PrtPage        Laser;*p=columnB:row,"D"
                                      endif
.
                                      If            (MrktPromo = Yes)
                                      add            "100" to columnB
                                      PrtPage        Laser;*p=columnB:row,"P"
                                      endif
.
                                      If            (MrktHlydy = Yes)
                                      add            "100" to columnB
                                      PrtPage        Laser;*p=columnB:row,"H"
                                      endif
.
                                      If             (MrktNews = Yes)
                                      add            "100" to columnB
                                      PrtPage        Laser;*p=columnB:row,"N"
                                      endif
.
                                      If             (Mrktparty = Yes)
                                      add            "100" to columnB
                                      PrtPage        Laser;*p=columnB:row,"Y"
                                      endif
.
.begin patch 1.22

                                      If             (MrktActive = Yes)
                                      add            "100" to columnB
                                      PrtPage        Laser;*p=columnB:row,"a"
                                      endif
.
                                      If             (MrktPrsct = Yes)
                                      add            "100" to columnB
                                      PrtPage        Laser;*p=columnB:row,"T"
                                      endif
.
                                      If             (MrktVndr = Yes)
                                      add            "100" to columnB
                                      PrtPage        Laser;*p=columnB:row,"V"
                                      endif
.
 
.end patch 1.22
                                      If             (Mrktcode <> "")
                                      add            "100" to columnB
                                      PrtPage        Laser;*p=columnB:row,MrktCode
                                      endif
.
                              add             "200" to row
                              add            c1 to ColumnCount

                              IF             (ColumnCount <= 2)
                              add            "4000" to column
                              else
                              move            "0" TO Column
                              move           c1 to columnCount
                              add            c1 to RowCount
                              endif
.
               if             (rowcount > "9")        .lets double check
.               Prtpage         Laser;*p=1:10400,"Page ",page,*p=1500:10400,"Key ='D'=Datacards, 'P'=Promotional, ":
.                               "'H'=Holiday, 'N'=Newsletter ,'Y'=Party, Broker Rank 'A-D'"
          Prtpage         Laser;*p=1:10400,"Page ",page,*p=2000:10400,"Key ='D'=Datacards, 'P'=Promotional, ":
                    "'H'=Holiday, 'N'=Newsletter ,'Y'=Party, 'V'=Vendor, 'T'=Prospect, 'a'=Active, Broker Rank 'A-D'"

               prtpage         Laser;*NEWPAGE;
               call           header
               endif
               Load           Row from RowCount of Row1,Row2,Row3,Row4,Row5,Row6,Row7,Row8,row9
.

                endif              .end of reportsw = 1 "IF"
.......................................................................
               If               (ReportSw = C2)            .summary listing
.
                If              (row >= 10000)
                If              (ListSlctCount = c1)
                Prtpage         Laser;*p=1:10400,"Page ",page
                else
.                Prtpage         Laser;*p=1:10400,"Page ",page,*p=2500:10400,"'D'=Datacards, 'P'=Promotional, ":
.                                "'H'=Holiday, 'N'=Newsletter ,'Y'=Party"
          Prtpage         Laser;*p=1:10400,"Page ",page,*p=2000:10400,"Key ='D'=Datacards, 'P'=Promotional, ":
                    "'H'=Holiday, 'N'=Newsletter ,'Y'=Party, 'V'=Vendor, 'T'=Prospect, 'a'=Active, Broker Rank 'A-D'"
                endif
                prtpage         Laser;*NEWPAGE;
                call            header
                endif
.
                               If              (ListSlctCount = C1)
.
                                       If              (SortFlag = c1)
                                       PrtPage         Laser;*p=1:Row,*font=PRTpg10,MrktCoName,*p=4500:row,MrktCntName
                                       ElseIf          (SortFlag = c2)
                                       PrtPage         Laser;*p=1:Row,*font=PRTpg10,MrktCntName,*p=4500:row,MrktCoName
                                       ElseIf          (SortFlag = c3)
                                       PrtPage         Laser;*p=1:Row,*font=PRTpg10,MrktCoName,*p=4500:row,MrktCntName
                                       endif
                               add             "200" to row
.
                               else
.
                                       If              (SortFlag = c1)
                                       PrtPage         Laser;*p=1:Row,*font=PRTpg10,MrktCoName,*p=3000:row,MrktCntName
                                       ElseIf          (SortFlag = c2)
                                       PrtPage         Laser;*p=1:Row,*font=PRTpg10,MrktCntName,*p=3000:row,MrktCoName
                                       ElseIf          (SortFlag = c3)
                                       PrtPage         Laser;*p=1:Row,*font=PRTpg10,MrktCoName,*p=3000:row,MrktCntName
                                       endif
.
                                      if             (MrktDtacrd = Yes)
                                      PrtPage        Laser;*p=7000:row,"D"
                                      endif
.
                                      If            (MrktPromo = Yes)
                                      PrtPage        Laser;*p=7150:row,"P"
                                      endif
.
                                      If            (MrktHlydy = Yes)
                                      PrtPage        Laser;*p=7250:row,"H"
                                      endif
.
                                      If             (MrktNews = Yes)
                                      PrtPage        Laser;*p=7350:row,"N"
                                      endif
.
                                      If             (Mrktparty = Yes)
                                      PrtPage        Laser;*p=7450:row,"Y"
                                      endif
.begin patch 1.22

                                      If             (MrktActive = Yes)
                                      PrtPage        Laser;*p=7550:row,"Y"
                                      endif
.
                                      If             (MrktPrsct = Yes)
                                      PrtPage        Laser;*p=7650:row,"Y"
                                      endif
.
                                      If             (MrktVndr = Yes)
                                      PrtPage        Laser;*p=7750:row,"Y"
                                      endif
.
 
.end patch 1.22
.
                               add             "200" to row
                                endif
                endif
.end if listing section
.this section is for Labels
               If               (reportSw = C3)
                              If             (Row > 10500 & Column > 6500)
                              call           Header
                              endif
.
                              call           trim using MrktCntName
                              call           trim using MrktCoName
                              call           trim using MrktAddr1
                              call           trim using MrktAddr2
                              call           trim using MrktCity
                              count          n2 in MrktZip
                              if             (n2 = C9)
                              unpack         MrktZip into str5,str4
                              pack           ZipMask from str5,"-",str4
                              else
                              Move           MrktZip to Zipmask
                              endif
.                              If             (labelCount = C1)          .first label of page
                              prtpage        Laser;*p=Column:row,*LL,*font=Font2,MrktCntName
                                             If             (MrktCoName <> "")
                                             add            "150" to Row
                                             prtpage        Laser;*p=Column:row,*LL,*font=Font2,MrktCoName
                                             endif
                              add            "150" to Row
                              prtpage        Laser;*p=Column:row,*LL,MrktAddr1
                                             If             (MrktAddr2 <> "")
                                             add            "150" to Row
                                             prtpage        Laser;*p=Column:row,*LL,*font=Font2,MrktAddr2
                                             endif
                              add            "150" to Row
                              prtpage        Laser;*p=Column:row,*LL,*font=Font2,MrktCity,", ",MrktState," ",ZipMask
.
                              If             (BarCodeSw = Yes)
                              add            "150" to Row
                              clear          BarCode
                              pack           barcode from star,zipmask,star
                              prtpage        Laser;*p=Column:row,*LL,*font=FontB,ZipMask,*font=Font2
                              endif
.
               add            c1 to LabelCount
               add            c1 to ColumnCount
                              IF             (ColumnCount <= 3)
                              add            "2750" to column
                              else
.START PATCH 1.15 REPLACED LOGIC
.                              move            "0" TO Column
                              move            "150" TO Column
.END PATCH 1.15 REPLACED LOGIC
                              move           c1 to columnCount
                              add            c1 to RowCount
                              endif
               if             (rowcount > "10")        .lets double check
               call           header
               endif
               Load           Row from RowCount of Row1,Row2,Row3,Row4,Row5,Row6,Row7,Row8,row9,row10
               endif
.end section for Labels
.Patch 1.14
.XLS Section
                    If (reportSw = C4)         .Detail listing
                              call      trim using MrktCntName
                              call      trim using MrktCoName
                              call      trim using MrktAddr1
                              call      trim using MrktAddr2
                              call      trim using MrktCity
                              call      Trim using MrktEmail
                              call      Trim using MrktPhone
                              call      Trim using MrktFax
                              count     n2 in MrktZip
                              if (n2 = C9)
                                        unpack    MrktZip into str5,str4
                                        pack      ZipMask from str5,"-",str4
                              else
                                        Move      MrktZip to Zipmask
                              endif
                              write     xlsfile,seq;*cdfon,MRKTCntName,MRKTCoName,MrktAddr1,MrktAddr2,MrktCity,MrktState,ZipMask,MrktEmail,MrktPhone,MrktFax
                    endif
.Patch 1.14
                goto            PrintLoop

PrintExit
.Patch 1.14
                               If  (reportSw = C4)         .XLS
                                                  close     XLSfile
.START PATCH 1.16 REPLACED LOGIC
.                                                 alert note,"Please check \\nins1\d\users\promo for your file!",result,"Excel CSV File"
                                                  close     tempfile
.Open Excel application
                                                create  ex
.begin patch 1.23                                                
.                                                setprop ex,*WindowState=xlMinimized
.end patch 1.23                                                
                                                setprop ex,*Visible="False"
                                                  setprop ex.CommandBars("Standard"),*Visible="True"
                                                  setprop ex.CommandBars("Formatting"),*Visible="True"
                                                  setprop ex.CommandBars("Worksheet Menu Bar"),*Enabled="True"
.Reset Default of Worksheets found in a Workbook
                                                getprop ex,*SheetsInNewWorkbook=HOWMANY
                                                setprop ex,*SheetsInNewWorkbook=C1
.Create Workbooks collection
                                                getprop ex,*Workbooks=books
.Create/Add a single Workbook
.>Patch 1.17 Modified Code       
.                                               books.open using "\\nins1\d\users\promo\prmlisting.csv"
.                                               books.open using "\\nins1\d\promo\prmlisting.csv"
                                                books.open using "c:\work\prmlisting.csv"
.>Patch 1.17 Modified Code                                                    
                                                books.item giving book using 1
                                                getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
                                                sheets.item giving sheet using 1
.....Do some formatting
.Trim everything.  Writing to a file automatically pads out the field, so we have to get rid of this excess
                                                  for N6,C1,PrintCount
                                                            move      N6,str6
                                                            call      Trim using str6
.                                                           for N1,C1,C7
                                                            for N1,C1,C9
                                                                      switch    N1
                                                                                case "1"
                                                                                          move      "A",str1
                                                                                case "2"
                                                                                          move      "B",str1
                                                                                case "3"
                                                                                          move      "C",str1
                                                                                case "4"
                                                                                          move      "D",str1
                                                                                case "5"
                                                                                          move      "E",str1
                                                                                case "6"
                                                                                          move      "F",str1
                                                                                case "7"
                                                                                          move      "G",str1
                                                                                case "8"
                                                                                          move      "H",str1
                                                                                case "9"
                                                                                          move      "I",str1
                                                                                default
                                                                                          move      "J",str1
                                                                      endswitch
                                                                      pack    str7,str1,str6
                                                                      getprop sheet.range(str7),*Value=str55
                                                                      call      Trim using str55
                                                                      if (str1 = "G")
.First set Zip code column as text
                                                                                setprop sheet.range(str7),*NumberFormat="@"
.
                                                                                type      str55
                                                                                if equal
                                                                                          count     N2,str55
                                                                                          if (N2 < "5")
                                                                                                    move      str55,N5
                                                                                                    move      N5,str5
                                                                                                    rep       zfill,str5
                                                                                                    move      str5,str55
                                                                                          endif
                                                                                endif
                                                                      endif
                                                                      if (str1 = "I" or Str1 = "J")
                                                                      setprop sheet.range(str7),*NumberFormat="(###) ###-####"
                                                                      Endif
                                                                      setprop sheet.range(str7),*Value=str55
                                                                      
                                                            repeat
                                                  repeat
.
                                                  move      PrintCount,str5
                                                  call      Trim using str5
                                                pack    str4,"A1"
                                                pack    str6,"A",str5
                                                sheet.range(str4,str6).Columns.Autofit
                                                pack    str4,"B1"
                                                pack    str6,"B",str5
                                                sheet.range(str4,str6).Columns.Autofit
                                                pack    str4,"C1"
                                                pack    str6,"C",str5
                                                sheet.range(str4,str6).Columns.Autofit
                                                pack    str4,"D1"
                                                pack    str6,"D",str5
                                                sheet.range(str4,str6).Columns.Autofit
                                                pack    str4,"E1"
                                                pack    str6,"E",str5
                                                sheet.range(str4,str6).Columns.Autofit
                                                pack    str4,"F1"
                                                pack    str6,"F",str5
                                                sheet.range(str4,str6).Columns.Autofit
                                                pack    str4,"G1"
                                                pack    str6,"G",str5
                                                sheet.range(str4,str6).Columns.Autofit
.
                                                pack    str4,"H1"
                                                pack    str6,"H",str5
                                                sheet.range(str4,str6).Columns.Autofit

                                                pack    str4,"I1"
                                                pack    str6,"I",str5
                                                sheet.range(str4,str6).Columns.Autofit

                                                pack    str4,"J1"
                                                pack    str6,"J",str5
                                                sheet.range(str4,str6).Columns.Autofit

                                                destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
                                                setprop ex,*DisplayAlerts=OFALSE
                                                setprop ex,*SheetsInNewWorkbook=HOWMANY
                                                setprop ex,*Visible="True"
                                                destroy ex
.>Patch 1.17 Modified Code                                                              
.                                                 erase     "\\nins1\d\users\promo\prmlisting.csv"
.                                                 erase     "\\nins1\d\promo\prmlisting.csv"
                                                  erase     "c:\work\prmlisting.csv"
.>Patch 1.17 Modified Code End                                                  
.END PATCH 1.16 REPLACED LOGIC
                setitem       Mrkt003StatText005,0," "
                                                  return
                                         endif
.Patch 1.14
.begin patch 1.12
                add            "750" to Row
.                add            "150" to Row
                PrtPage         Laser;*p1:row,"Records printed : ",Printcount
.end patch 1.12
                If              (ListSlctCount = c1)
                Prtpage         Laser;*p=1:10400,"Page ",page
                else
                Prtpage         Laser;*p=1:10400,"Page ",page,*p=2500:10400,"Key ='D'=Datacards, 'P'=Promotional, ":
                                "'H'=Holiday, 'N'=Newsletter,'Y'=Party"
                endif
                prtClose        Laser
                Close           Tempfile
                move            c0 to page
                return
                setitem       Mrkt003StatText005,0," "

Header
.header section for reports
                If             (reportSw = C1)
                PRTPAGE         Laser;*ROWSpaceE=0,*COLSPACE=0,*Overlayon:
                                *p=25:102,*font=PRTpg10B,"Names  ":
                                *font=PRTpg10i," in the News":
                                *p=5:240,*Line=1260:240:
                                *p=10:250,*font=PRTpg10,"C a l i f o r n i a    I n c .":
                                *p=3500:250,ReportTitle,*p=7200:250,Today
                Move            "800" to row
                add             c1 to page
                move            "0" TO Column
                Move            c1 to ColumnCount
                Move            c1 to RowCount
                endif
.
                If             (reportSw = C2)
                PRTPAGE               Laser;*ROWSpaceE=0,*COLSPACE=0,*Overlayon:
                                *p=25:102,*font=PRTpg10B,"Names  ":
                                *font=PRTpg10i," in the News":
                                *p=5:240,*Line=1260:240:
                                *p=10:250,*font=PRTpg10,"C a l i f o r n i a    I n c .":
                                *p=3500:250,ReportTitle,*p=7200:250,Today
                If              (ListSlctCount = C1)
                        If              (SortFlag = c1)
                        PrtPage         Laser;*p=50:525,*font=PRTpg10B,"Company",*p=4550:525,"Contact"
                        ElseIf          (SortFlag = c2)
                        PrtPage         Laser;*p=50:525,*font=PRTpg10B,"Contact",*p=4550:525,"Company"
                        ElseIf          (SortFlag = c3)
                        PrtPage         Laser;*p=50:525,*font=PRTpg10B,"Company",*p=4550:525,"Contact"
                        endif
                else
                        If              (SortFlag = c1)
                        PrtPage         Laser;*p=50:525,*font=PRTpg10B,"Company",*p=3050:525,"Contact"
                        ElseIf          (SortFlag = c2)
                        PrtPage         Laser;*p=50:525,*font=PRTpg10B,"Contact",*p=3050:525,"Company"
                        ElseIf          (SortFlag = c3)
                        PrtPage         Laser;*p=50:525,*font=PRTpg10B,"Company",*p=4550:525,"Contact"
                        endif
                endif
                Move            "800" to row
                add             c1 to page
                endif
.End Header section for reports
.
.header section for labels
                If              (reportSw = C3)
.reset to first column and row
.START PATCH 1.15 REPLACED LOGIC
.                move            "0" TO Column
                move            "150" TO Column
.END PATCH 1.15 REPLACED LOGIC
                move            "500" to Row
                prtpage         Laser;*NEWPAGE;
                move            c1 to LabelCount              .reset count to indicate 1st label of page
                Move            c1 to ColumnCount
                Move            c1 to RowCount
                endif
.End header section for labels
                Return
...........................................................................................
.tab click deactivate the tab page we were on.
Mrkt001TabClick
        IF (N1 = C1)
                               Deactivate Maint
        else (N1 = C2 )
                          Deactivate Report
        Endif
        return

Mrkt001TabChange
        IF (N1 = C1)

                  move    C1,TabNum
                  Deactivate Report
                  Activate Maint
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT

        else (N1 = C2)
                move    C2,TabNum
                Deactivate    Maint
                Activate      Report
                setitem        Mrkt003Radio005,0,1
                setitem        Mrkt003Radio007,0,1

.Prevent occurance or accumulated events which may place "hidden" objects on wrong form
.ie, ResetStatus Checkbox. This generally only happens with LostFocus events from Stats2.plf
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT
        Endif
        return
.............................................................................................................
.SetFocusTab
Mrkt001SetFocusTab
         setitem     Mrkt001TabControl001,0,c2
         setfocus    Mrkt001TabControl001,1
         return
.............................................................................................................
.FontErr     ----   barcode font missing
Fonterr
               Trapclr        Object
               alert          Stop,"Barcode Font missing inform I.S.!",result
               return
.............................................................................................................
.PrintErr     ----   Printing cancelled or error
Printerr
               Trapclr        Spool
               alert          Caution,"Printing Cancelled or Error!",result
               Noreturn
               Return
.................................................................................
FileGo
.ExitFlag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo2

FileGo1
          return

FileGo2
          if (ExitFlag = NO)
                    return
          endif
          winshow
          stop
EditGo
          return
OptionsGo
SearchGo
        branch  result to SearchGo2,SearchGo3
SearchGo2
.LIST
.        move    C2,SrchFlag
.        call    SearchSetTitle
.        call    SearchSetVisible
        return
SearchGo3
.MAILER
.        move    C3,SrchFlag
.        call    SearchSetTitle
.        call    SearchSetVisible
        return
SearchLoad
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5
SearchLoad1
.BROKER - not an option with this program
        return
SearchLoad2
..LIST
.         unpack  Srchstr,str6,str1,str35
.         if (ClientFlag = 0)
.                   setitem Proj2EditClient,0,str6
.                   setitem   Proj2ComboSource,0,3
.                   setitem   Proj2ComboType,0,1
.                setfocus Proj2EditClient
.        else
.                setitem Proj2Edit2Client,0,str6
.                   setitem   Proj2Stat2ClientName,0,str35
.                   setitem   Proj2Combo2Source,0,3
.                setfocus Proj2Edit2Client
.        endif
.        return
SearchLoad3
.MAILER
.        unpack  Srchstr,str4,str1,str3,str1,str45
.         if (ClientFlag = 0)
.                   setitem Proj2EditClient,0,str4
.                   setitem   Proj2ComboSource,0,2
.                setfocus Proj2EditClient
.        else
.                setitem Proj2Edit2Client,0,str4
.                   setitem   Proj2Stat2ClientName,0,str45
.                   setitem   Proj2Combo2Source,0,2
.                setfocus Proj2Edit2Client
.        endif
.        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
SearchLoad5
.CAMPAIGN - not an option with this program
        return
ReportGo
          return
HelpGo
        branch  result to HelpGo1
HelpGo1
        setprop AboutMssg,visible=1
        return

               include        Mrktlstio.inc
               include        ncntio.inc
               include        gnxtio.inc
               include        comlogic.inc
