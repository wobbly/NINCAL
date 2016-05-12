........................................
. Program:      ROLO.PLS
. Function:     In-House Phone Directory
. Author:       Andrew Harkins
. Date:         November 25,1998
. Release:      2.0
. Notes:        Check Archives for history prior to 2.0
........................................


PC      EQU     0
.Include Files
        include common.inc
        include cons.inc
        include npasdd.inc
        INCLUDE       PRTPAGEDD.INC
release         init    "2.50"   DLH     add area code and code to support it
Reldate        Init     "2015 March 11"
.release         init    "2.42"   DS     13DEC2005 Replaced Datalist with ListView, Reformat UI
.Reldate        Init           "13 December 2005"
.release         init    "2.41"   DLH   15Sep2004 Moved Logo on printout
.Reldate        Init           "15 September 2004"
.release         init    "2.4"   ASH    09AUG2004 Logo Conversion
.release         init    "2.3"   DLH 24Oct01 Played with header
.release         init    "2.2"   ASH 19MAR99 MOVED: 1) VArs associated with Passwrd.plf now found in NPASDD.INC
.release         init    "2.1"   ASH 19Jan99 added: 1)timeout logic
.release         init    "2.0"   ASH 25Nov98 New release

.Start Patch #2.1 - Timer logic
Timer   Timer
.End Patch #2.1 - Timer logic
.START PATCH #2.2 - VARS NOW DECLARED IN NPASDD.INC
.Vars used for Password Box
.progcode init   "Y"
.END PATCH #2.2 - VARS NOW DECLARED IN NPASDD.INC
.List for File
RoloVars list
RoloName dim    25             1-25
.RoloNum  dim    7             26-32
RoloNum  dim    10             26-35
RoloStat dim    1             36-36
RoloNote dim    24            37-60
        listend

hold    dim     120
key     dim     25
holdkey dim     25
.Newnum  dim     8
Newnum  dim     13
ReturnFlag init "N"
.begin patch 2.50
Area    Dim           3   
.end patch 2.50
.START PATCH #2.2 - VARS NOW DECLARED IN NPASDD.INC
.PassFlag init   "N"
.START PATCH #2.2 - VARS NOW DECLARED IN NPASDD.INC
ExitFlag init   "Y"
dot45   init    "............................................."

.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu

.Set Up SubMenu for Options
sColors submenu

.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Color"
HData   init    "&Help;&About"

.Present Data for Colors SubMenu
CData   init    ";Foreground E&dit;Foreground &Text;Foreground &List;&Background List"

.Define Collections for Object Colors
ColTxt  Collection
ColStat Collection

.Define Colors for Each Object
FTC     color
FSC     color
FDC     color
BDC     color

.Define colors EditText Inquiries
white   color
grey    color

.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
FontO10               font
FontO18I              font
FontO18B        font
FontO18BI       font
.Start Patch #2.0 - not yet implemented
.font5   font
.End Patch #2.0 - not yet implemented

.Define files to be used
.file1   ifile   keylen=25,fixed=58
file1   ifile   keylen=25,fixed=61
file2   file    uncomp
prfile  pfile
.............................
.Start Patch #2.0 - logic for dynamic form sizing not yet implemented
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
.End Patch #2.0 - logic for dynamic form sizing not yet implemented
.............................
.Open Index file
        open    file1,"DIRECT.ISI|nins1:502"

.Set vars for About Box
        move    "ROLO.PLS",Wprognme
        move    "In-House Directory",Wfunction
        move    "Andrew Harkins",Wauthor
.        move    "2.0",Wrelease
.        move    "November 25,1998",Wreldate
.        move    "2.3",Wrelease
.        move    "Octobere 24,2001",Wreldate
               MOve           Release to Wrelease
               Move           Reldate to Wreldate

.Declare forms, Always declare child forms first
pss     plform  passwrd
abt     plform  about
x       plform  rolo
        winhide
.Load Forms, Always declare parent form first
        formload x
        formload abt
        formload pss

.Start Patch #2.1 - timer logic
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.End Patch #2.1 - timer logic
.Create Menus
        create  rolo;mFile,FData
        create  rolo;mEdit,EData,mFile
        create  rolo;mOptions,OData,mEdit
        create  rolo;mHelp,HData,mOptions

.Create SubMenu
        create  rolo;sColors,CData,mOptions,1

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
.Only a SubMenu under this one
        activate mOptions
        activate mHelp,HelpGo,result

.Activate SubMenu
        activate sColors,ColorGo,result

.Set Properties in Collections
        listins ColTxt,RoloSearchKey,RoloEditName,RoloEditNumber,RoloEditNotes
        listins ColStat,RoloSearchName,RoloStatName,RoloStatNumber,RoloStatNotes

.START PATCH 2.4 ADDED LOGIC
.NINLogo  PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 2.4 ADDED LOGIC

.Start Patch #2.0 - logic for dynamic form sizing not yet implemented
.COLLECTION NOT YET USED!!!
.        listins Coll1,Rolo,RoloCancel,RoloCheckBox,RoloDelete,RoloEditName,RoloEditnotes,roloeditnumber,roloexit:
.        roloinquirygroup,rolomodify,rolonew,rolonummssg,rolook,roloprint,rolosave,rolosearchkey:
.        rolosearchlist,rolosearchname,rolostatname,rolostatnotes,rolostatnumber,roloupdate
.
.        if (str4 = "1024")
.                create  font5,"Arial",size=12
.                setprop coll1,font=font5
.                move    "1.25",size
.                getprop Rolo,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Rolo,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop RoloCancel,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop RoloCancel,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop RoloCheckBox,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop RoloCheckBox,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop RoloDelete,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop RoloDelete,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop RoloEditName,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop RoloEditName,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop RoloEditnotes,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop RoloEditnotes,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Roloeditnumber,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Roloeditnumber,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Roloexit,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Roloexit,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Roloinquirygroup,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Roloinquirygroup,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Rolomodify,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Rolomodify,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Rolonew,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Rolonew,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Rolonummssg,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Rolonummssg,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5,VISIBLE=0
.
.                getprop RoloOK,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop RoloOK,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Roloprint,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Roloprint,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Rolosave,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Rolosave,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Rolosearchkey,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Rolosearchkey,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Rolosearchlist,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Rolosearchlist,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Rolosearchname,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Rolosearchname,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Rolostatname,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Rolostatname,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Rolostatnotes,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Rolostatnotes,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Rolostatnumber,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Rolostatnumber,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop Roloupdate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Roloupdate,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
.        endif
.End Patch #2.0 - logic for dynamic form sizing not yet implemented

.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=*ltgray
.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=12,italic
        create  fontO10,"Times New Roman",size=10
        create  fontO18I,"Times New Roman",size=18,Italic
        create  fontO18B,"Times New Roman",size=18,Bold
        create  fontO18BI,"Times New Roman",size=18,Bold,Italic
.START PATCH #2.2 - VARS NOW DECLARED IN NPASDD.INC
        move    "Y",progcode
        move    "N",PassFlag
.END PATCH #2.2 - VARS NOW DECLARED IN NPASDD.INC
.START PATCH 2.42 ADDED LOGIC
          RoloListView.InsertColumn using "Name",150,0
          RoloListView.InsertColumn using "Phone",90,1
          RoloListView.InsertColumn using "Employee",70, 2
          RoloListView.InsertColumn using "Notes",175,3
          RoloListView.InsertColumn using "SortKey",0,4
          RoloListView.InsertColumn using "SortKey2",0,5
          setprop   RoloListview,hidesel=0
.END PATCH 2.42 ADDED LOGIC
.bad dh bad
          setprop             RoloPrint,visible=1
          call                RefreshList
          setprop   RoloModify,enabled=1

.Main Loop
         EVENTREG  X, 17, XRESIZE

        setfocus RoloSearchKey
        loop
                waitevent
.Start Patch #2.1 - timer logic
                setitem timer,0,18000   .reset to 30 minutes
.End Patch #2.1 - timer logic
        repeat

.Start Patch #2.1 - timer logic
Timeout
        beep
        beep
        beep
        stop
.End Patch #2.1 - timer logic

FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo3,FileGo3
FileGo1
.        PRTOPEN prfile,"-",WPrognme
        PRTOPEN prfile,"",WPrognme
        CLOCK   DATE,DATE
        unpack  date,mm,str1,dd,str1,yy
        goto    StartPrint
FileGo2
.        PRTOPEN prfile,"@",WPrognme
        PRTOPEN prfile,"",WPrognme
        CLOCK   DATE,DATE
        unpack  date,mm,str1,dd,str1,yy
        goto    StartPrint
FileGo3
        if (ExitFlag = "Y")
                winshow
                stop
        endif
        return
EditGo
        return
HelpGo
        setprop AboutMssg,visible=1
        return
ColorGo
        branch result to edit1,text1,list1,list2
        return
edit1
        trap    ColorTrap if object
        create  FTC
        trapclr object
        setprop ColTxt,fgcolor=FTC
        return
text1
        trap    ColorTrap if object
        create  FSC
        trapclr object
        setprop ColStat,fgcolor=FSC
        return
list1
        trap    ColorTrap if object
        create  FDC
        trapclr object
.START PATCH 2.42 REPLACED LOGIC
.        setprop RoloSearchList,fgcolor=FDC
        setprop RoloListView,fgcolor=FDC
.END PATCH 2.42 REPLACED LOGIC
        return
list2
        trap    ColorTrap if object
        create  BDC
        trapclr object
.START PATCH 2.42 REPLACED LOGIC
.        setprop RoloSearchList,fgcolor=BDC
        setprop RoloListView,fgcolor=BDC
.END PATCH 2.42 REPLACED LOGIC
        return

.Trap for Cancel Entry in Color System Menu
ColorTrap
        noreturn
        return

ClearRec
.Clear all Text Fields
        setitem RoloEditName,0,""
        setitem RoloEditNumber,0,""
        setitem RoloEditNotes,0,""
        return
.
.START PATCH 2.42 REPLACED LOGIC
.RefreshList
..Clear DataList
.        deleteitem      RoloSearchList,0
.        getitem         RoloSearchKey,0,HowMany
..Move a blank to key so that it starts at beginning of file
.        if              (HowMany = 0)
.                        move    B1, key
.        else
.                        getitem RoloSearchKey,0,key
.        endif
..Set place in file to start reading
.        read            file1,key;;
.        if not over
.                        pack hold,RoloName,RoloNum,RoloStat,RoloNote
.                        insertitem RoloSearchList,9999,hold
.        endif
.        loop
.                        readks file1;RoloVars
.                        until over
.                        pack hold,RoloName,RoloNum,RoloStat,RoloNote
.                        insertitem RoloSearchList,9999,hold
.        repeat
..Put focus on DataList
.        setitem         RoloSearchList,1,1
..Load the Inquiry Screen immediately
.        call            LoadData
.        return
................................
RefreshList
.Clear DataList
        RoloListView.DeleteAllItems giving result
        getitem         RoloSearchKey,0,HowMany
.Move a blank to key so that it starts at beginning of file
        if              (HowMany = 0)
                        move    B1, key
        else
                        getitem RoloSearchKey,0,key
        endif
.Set place in file to start reading
        read            file1,key;;
        if not over
                        call  RoloLoadListView
        endif
        loop
                        readks file1;RoloVars
                        until over
                        call  RoloLoadListView
        repeat
.Put focus on DataList
        RoloListView.SetItemState giving result using 0,2,2
        RoloListView.EnsureVisible using 0,0
.Load the Inquiry Screen immediately
        call        Click_RoloListView
        return
.END PATCH 2.42 REPLACED LOGIC

.START PATCH 2.42 ADDED LOGIC
RoloLoadListView
.begin patch 2.50
        unpack          RoloNum,Area,str3,str4
.Allow for 3 Digit Number, ie:  911
          count           result,str4
          if (result <> 0)
                    pack    Newnum,"(",Area,")",str3,Dash,str4
.end patch 2.50
          else
                    pack    Newnum,str3
          endif
          RoloListView.InsertItem giving result using RoloName
          RoloListView.SetItemText using result,Newnum,1
                    if (RoloStat="*")
                              move " ", RoloStat
                    else
                              move "Y", RoloStat
                    endif
          RoloListView.SetItemText using result,RoloStat,2
          RoloListView.SetItemText using result,RoloNote,3
          call      Trim using RoloNum
          RoloListView.SetItemText using result,RoloNum,4
          call      Trim using RoloNote
          if (RoloNote = "")
                    pack      taskname,"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
          else
                    move      RoloNote,taskname
          endif
          RoloListView.SetItemText using result,taskname,5
          return
.END PATCH 2.42 ADDED LOGIC

LoadList
        setitem         RoloEditName,0,RoloName
.STARTPATCH 2.42 REPLACED LOGIC
.      unpack          RoloNum,str3, str4
..Allow for 3 Digit Number, ie:  911
.        count           result,str4
.       if (result <> 0)
.                      pack    Newnum,str3,Dash,str4
.         else
.                             pack    Newnum,str3
.         endif
.ENDPATCH 2.42 REPLACED LOGIC
          setitem         RoloEditNumber,0,Newnum

        setitem         RoloEditNotes,0,RoloNote
        if (RoloStat = "*")
                        setitem RoloCheckBox,0,0
        else
                        setitem RoloCheckBox,0,1
        endif
.START PATCH 2.42 REPLACED LOGIC
.         setfocus        RoloSearchList
        setfocus        RoloListView
.END PATCH 2.42 REPLACED LOGIC
        setprop         RoloModify,default=1
        return

DisableUpper
        setprop         RoloSearchKey,enabled=0
.START PATCH 2.42 REPLACED LOGIC
.        setprop         RoloSearchList,enabled=0,bgcolor=grey
        setprop         RoloListView,enabled=0,bgcolor=grey
.END PATCH 2.42 REPLACED LOGIC
        setprop         RoloOK,enabled=0
        setprop         RoloExit,enabled=0
        setprop         RoloNew,enabled=0
        setprop         RoloPrint,enabled=0
        return

EnableUpper
        move            yes,Exitflag
        setprop         RoloSearchKey,enabled=1
.START PATCH 2.42 REPLACED LOGIC
.        setprop         RoloSearchList,enabled=1,bgcolor=white
        setprop         RoloListView,enabled=1,bgcolor=white
.END PATCH 2.42 REPLACED LOGIC
        setprop         RoloOK,enabled=1
        setprop         RoloExit,enabled=1
        setprop         RoloNew,enabled=1
        setprop         RoloPrint,enabled=1
        return

DisableLower
        setprop         RoloEditName,enabled=0,bgcolor=grey
        setprop         RoloEditNumber,enabled=0,bgcolor=grey
        setprop         RoloEditNotes,enabled=0,bgcolor=grey
        setprop         RoloNumMssg,visible=0
        setprop         RoloUpdate,visible=0
        setprop         RoloCancel,visible=0
        setprop         RoloDelete,visible=0
        setprop         RoloModify,enabled=1
        setprop         RoloCheckBox,visible=0
        return

EnableLower
        move            no,Exitflag
        setprop         RoloEditName,enabled=1,bgcolor=white
        setprop         RoloEditNumber,enabled=1,bgcolor=white
        setprop         RoloEditNotes,enabled=1,bgcolor=white
        setprop         RoloUpdate,visible=1
        setprop         RoloCancel,visible=1
        setprop         RoloDelete,visible=1
        setprop         RoloModify,enabled=0
        setprop         RoloCheckBox,visible=1
        return
VerifyData
        getitem RoloEditName,0,RoloName
        call    TRIM using RoloName
        count   HowMany,RoloName
        if (howmany = 0)
                alert caution,"Name Required!",result
                setfocus RoloEditName
                setprop  RoloSave,enabled=1
                return
        endif
        setitem RoloEditName,0,RoloName
.Verify Phone Number
.begin patch 2.50
        getitem RoloEditNumber,0,str13
        call    TRIM using str13
        count   HowMany,str13
.        getitem RoloEditNumber,0,str8
.        call    TRIM using str8
.        count   HowMany,str8
.end patch 2.50
        if (howmany < 7)
                if (HowMany <> 3 AND HowMany <> 1)
                        alert   caution,"Phone Number Required!",result
                        setfocus RoloEditNumber
                        move    Yes,ReturnFlag
                        return
                endif
        endif
.Procure and Extract appropriate phone number
.begin patch 2.50
        if (howmany = 13)
                unpack  str13,str1,Area,str1,str3,str1,str4
        Elseif   (howmany = 10)
                unpack  str13,Area,str3,str4       
        Elseif   (howmany = 8)
.        if (howmany = 8)
.                unpack  str8,str3,str1,str4
                unpack  str13,str3,str1,str4
        elseif (HowMany = 7)
                unpack  str13,str3,str4
                pack    str13,str3,dash,str4
        else
                unpack  str13,str3
                clear   str4
                call    TRIM using str3
                pack    str13,str3
        endif
        pack    RoloNum,area,str3,str4
        setitem RoloEditNumber,0,str13
.end patch 2.50

.Load Notes
        getitem RoloEditNotes,0,RoloNote
        call    TRIM using RoloNote
        setitem RoloEditNotes,0,RoloNote
.Employee Status
        getitem RoloCheckBox,0,result
        if (result = 1)
                move    B1,RoloStat
        else

                move    "*",RoloStat

        endif
        return

RoloPrintHeading
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
.;        add     "300",row
.        MOve     "0",row
        MOve     "50",row
        prtpage prfile;*UNITS=*HIENGLISH;
        prtpage prfile;*ORIENT=*LANDSCAPE,*ROWSPACE=0,*COLSPACE=0;
.        move    "2000",column
.        move    "7500",column3
.        move    "1950",column
.        move    "7450",column3
.        move    "1850",column
.        move    "7350",column3

        move    "1600",column
        move    "7100",column3
.        add     sixlpi,row
.START PATCH 2.4 REPLACED LOGIC
.        PrtPage    PRFile;*pcolumn:325,*font=fontO18b,"Names  ":
.                             *font=FontO18I,"in the News":
.                             *PENSIZE=10,*p=1700:625,*Line=4100:625:
.                             *pcolumn3:325,*font=fontO18b,"Names  ":
.                             *font=FontO18I,"in the News":
.                             *PENSIZE=10,*p=7250:625,*Line=9650:625
.        add     sixlpi,row
.
..        prtpage prfile;*pcolumn3:row,*font=font1,*ulon,"Names In The News";
.
.        add     sixlpi,row
..        add     "200",column
..        add     "200",column3
..        prtpage prfile;*pcolumn:row,*uloff,*font=font4,"California Inc.";
..        prtpage prfile;*pcolumn3:row,*uloff,*font=font4,"California Inc.";
.        prtpage    prfile;*pcolumn:659,*font=fontO10,"C  A  L  I  F  O  R  N  I  A     I  N  C .":
.                             *pcolumn3:659,*font=fontO10,"C  A  L  I  F  O  R  N  I  A     I  N  C ."
...............................
          prtpage   prfile;*Pictrect=*off,*PICT=6075:6875:2800:7800:NINLogo
          prtpage   prfile;*Pictrect=*off,*PICT=6075:6875:8250:12000:NINLogo
.         prtpage   prfile;*Pictrect=*off,*PICT=7075:7875:2800:7800:NINLogo
.         prtpage   prfile;*Pictrect=*off,*PICT=7075:7875:8250:12000:NINLogo
.;        add     sixlpi,row
.;        add     sixlpi,row
.END PATCH 2.4 REPLACED LOGIC
        add     "300",column
        add     "300",column3
        add     "180",column
        add     "180",column3
        prtpage prfile;*pcolumn:row,*font=font2,"Extension List  ",*font=font2,mm,slash,dd,slash,cc,yy,*font=font3;
        prtpage prfile;*pcolumn3:row,*font=font2,"Extension List  ",*font=font2,mm,slash,dd,slash,cc,yy,*font=font3;
.        add     sixlpi,row
.        add     "70",column
.        add     "70",column3
.        prtpage prfile;*pcolumn:row,*font=font2,mm,slash,dd,slash,cc,yy,*font=font3;
.        prtpage prfile;*pcolumn3:row,*font=font2,mm,slash,dd,slash,cc,yy,*font=font3;
        add     eightlpi,row
        return
.Print Line
RoloPrintLine
.Format Number
.begin patch 2.50
        unpack  RoloNum,area,str3,str4
.end patch 2.50
.Trim Variable
        call    RTrim using RoloName
        if              (RoloName = "Stephen King")
        call        debug
        endif
pad
.Pad var with formatting junk
        move    RoloName,dot45
        setlptr dot45
        if (blank = "Y")
                move "N",blank
                add eightlpi,row
        endif
        add      eightlpi,row
        add      C1,result
        if (result > N3)
.HowMany used to deternime border height- DO NOT REMOVE!!!
                move row,HowMany
.                move "3000",column
.                move "8500",column3
                move "2950",column
                move "8450",column3

.Size of row determined by title and border!!!!!!!
.                move "1385",row
.dh testing
.               Move "270" to row
               Move "320" to row

.               Move "615" to row
                move C1,result
        endif
.Print Name {with Non-Proportional Font!!}
        prtpage prfile;*pcolumn:row,dot45;
        prtpage prfile;*pcolumn3:row,dot45;
.Position Number to compensate for Non-Proportional Font
        add     "1500",column giving column2
        add     "1500",column3 giving column4
        call    TRIM using str3
        if (str3 = "0")
.begin patch 2.50
.                prtpage prfile;*pcolumn2:row,".............0";
.                prtpage prfile;*pcolumn4:row,".............0";
                prtpage prfile;*pcolumn2:row,"......................0";
                prtpage prfile;*pcolumn4:row,"......................0";
        elseif (str4 = "    ")
.                prtpage prfile;*pcolumn2:row,".........",str3;
.                prtpage prfile;*pcolumn4:row,".........",str3;
                prtpage prfile;*pcolumn2:row,"..................",str3;
                prtpage prfile;*pcolumn4:row,"..................",str3;
        else
.                prtpage prfile;*pcolumn2:row,str3,dash,str4,"      ";
.                prtpage prfile;*pcolumn4:row,str3,dash,str4,"      ";
                prtpage prfile;*pcolumn2:row,"(",Area,")",str3,dash,str4,"      ";
                prtpage prfile;*pcolumn4:row,"(",Area,")",str3,dash,str4,"      ";
.end patch 2.50
        endif
        return
XRESIZE
           Rolo.Scale
           RETURN
.Include IO
              INCLUDE       PRTPAGEIO.INC
        include comlogic.inc
        include npasio.inc