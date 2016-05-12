........................................
. Program:      NCNT0001.PLS
. Function:     CONTACT File Maintenance
. Author:       Andrew Harkins
. Orig. Date:   September 20,1998
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include ncntdd.inc
        include npasdd.inc
.begin patch 1.41
        include nusedd.inc   
.end patch 1.41

Release   Init      "1.41"    DLH pull inits from ninuser if not present
Reldate   Init      "2016 May 10"
.Release   Init      "1.4"    DLH Update plf change search datalist to list view, allow sorting
.Reldate   Init      "2014 August 18"
.Release   Init      "1.3"    DLH use to update ninuser file as well. they should just be combined and Oslspern.inc
.Reldate   Init      "June 19, 2012"
.Release   Init      "1.2"    DLH Cleanup CNTComboUsage logic and added info to plf
.Reldate   Init      "April 17, 2012"
.Release   Init      "1.1"    DLH 07Jan2008 Cleanup update errors convert to filelist updated plf
.Reldate   Init      "January 7, 2008"
.RELEASE        init    "1.02"   ASH 15JUN2005 Cleaning up errors during Deletes
.RELEASE        init    "1.01"   DMB 09OCT03 Added Code to not allow dupes
.RELEASE        init    "1.0"   ASH 20SEP98 Original Release

P_DL     DATALIST ^
P_TO     DATALIST ^
P_FROM   DATALIST ^

File1   file            .used to write documentation to NCNTDD.INC
MODNAME dim     35      .used to determine who is running this instance of program
Timer   Timer
.Temp holding vars if updating AamKey
TCNTNUM         DIM     2
TCNTNAME        DIM     35
TCNTPHONE       DIM     25
TCNTPORT        DIM     3
TCNTTEAM        DIM     2
TCNTRIGHTS      DIM     1
TCNTRIGHTS2     DIM     40
TCNTPRINT       FORM    1
TCNTCNT         DIM     1
TCNtCOmp            Dim       1
TCntSales           Dim       2
TcntInActive        Dim       1
TcntInActiv1        dim       1
.Flags
ExitFlag init   "Y"
ReturnFlag init "N"
UpdateFlag init "N"
AamFlag init    "N"
NewFlag init    "N"
RightsFlag form 1
LoadFlag init   "N"
.
AKey1   init    "01F"
.Set to length of record plus space for B1 and space for "-"
hold    dim     135
.Set to maximum length of Aam
key     dim     38
holdkey dim     2
Carr    init    0x7f
HoldTeam dim    2
HoldName dim    35
HoldPort dim    3
.begin patch 1.3
HoldInits dim    3
HoldIact  Dim       1
HoldSales Dim       2
.end patch 1.3


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

font5   font
.begin patch 1.41
CurIndex   form       5
.end patch 1.41
str37     dim       37
.Set Vars used for About Box
        move    "NCNT0001.PLS",Wprognme
        move    "Contact File Maintenance",Wfunction
        move    "Andrew Harkins",Wauthor
        move    Release,Wrelease
        move    Reldate,Wreldate

.Declare forms, Always declare child forms first
mss1    plform  Error
pss     plform  Passwrd
abt     plform  About
x       plform  NCNT0001
        winhide
.Load Forms, Always load parent form first
        formload x
        formload abt
        formload pss
        formload mss1

        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        create  NCNT0001;mFile,FData
        create  NCNT0001;mEdit,EData,mFile
        create  NCNT0001;mHelp,HData,mEdit

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
.Only a SubMenu under this one
        activate mHelp,HelpGo,result

.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=220:220:220

.Set up ListView columns
          CNTListViewSearch.InsertColumn using "CNT ## ",50,0
          CNTListViewSearch.InsertColumn using "Name",230,1
          CNTListViewSearch.InsertColumn using "Port ## ",75,2
          CNTListViewSearch.InsertColumn using "Sales ## ",75,3
          CNTListViewSearch.InsertColumn using "CNTVarS ",0,4
.Prep screens
        call    ContactLoadListViewSearch
        call    ContactLoadComboTeam
.        call    ContactLoadDataLists

        setitem CNTDataListRights,1,1
        call    ContactData
        move    CNTNUM,NCNTFLD
        call    ContactDisableLower
.Main Loop
        clock   timestamp,timestamp
        clock   port,str3
        unpack  str3,str2,str1
        pack    NCNTFLD1,str1,str2
        rep     zfill,NCNTFLD1
        move    "Main-NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD1
        move    C3,NCNTPATH
        call    NCNTKEY
        move    CNTNAME,MODNAME
        move    "*",progcode
.Set Flags to Open NINCNT.DAT
        move    C0,NCNTFLAG
.Force UpperCase for Password
        setprop PasswordEdit,edittype=C5
.Set Error Message Stat Text Boxes
        call    ContactSetErrorMssgDefault
        setfocus CNTEditSearch
        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat

Timeout
        beep
        beep
        beep
        stop

ContactSetErrorMssgDefault
.Set Default for Contact File Maintenance
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=1
        setprop ErrorMssgStat4,visible=1
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0,"To Search Contact Code:"
        setitem ErrorMssgStat2,0,"Enter 2 Digit Number"
        setitem ErrorMssgStat3,0,"To Search By Contact Name:"
        setitem ErrorMssgStat4,0,"Enter Search Name"
        setitem ErrorMssgStat5,0,"      That Record Does Not Exist!"
        return

ContactClearRec
.Clear all Text Fields
        setitem CNTComboRights,0,1
.        setitem CNTDataListTeam,0,1
        setitem CNTComboTeam,0,1
        deleteitem CNTDataListAddRights,0
        setitem CNTEditNum,0,""
        setitem CNTEditName,0,""
        setitem CNTEditPhone,0,""
        setitem CNTEditPort,0,""
        setitem CNTComboPrint,0,1
        setitem CNTComboUsage,0,1
        setitem CNTComboBox001,0,1
        setitem CNTComboPrint,0,1
          setitem CNTEditSales,0,""
          Clear     CntSales
        clear   CNTNum
        clear   CNTName
        clear   CNTTeam
        clear   CNTRights
        clear   CNTRights2
          Clear     CntCOMp
        clear   HoldTeam
.begin patch 1.3
        clear   CntInactive     
        clear   CntInactiv1
        clear   Cntcomp         
        clear   CNTINITS   
.end patch 1.3
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

ContactLoadScreen
        packkey hold,CNTVARS
        packkey holdkey,CNTNUM
        packkey NCNTFLD2,"01X",CNTNAME
        setitem CNTEditNum,0,CNTNUM
        setitem CNTEditName,0,CNTNAME
        setitem CNTEditPhone,0,CNTPHONE
        setitem CNTEditPort,0,CNTPORT
        setitem CNTComboPrint,0,CNTPRINT
          move      c0,n2
        setitem CNTEditSales,0,CNTsales





        move    C0,N1
          call      debug
        move    CNTCNT,N1
        add     C1,N1
.        setitem CNTComboUsage,0,N1
        setitem CNTComboUsage,0,cNTcNT
        move        c0,n1
        move        CntCOmp,n1
          if        (n1 = c0)            .old record
          move      c1,n1
          move      c1,CntCOmp
          endif
.        add                  c1,n1         ????? why default NIN
        setitem     CNTComboBox001,0,n1
.
        move    C2,N3
        loop
                getitem CNTComboTeam,N3,str2
                until (str2 = CNTTEAM)
                if (str2 = "")
                        move    C1,N3
                        break
                endif
                add     C1,N3
        repeat
        setitem CNTComboTeam,0,N3
        move    CNTTEAM,HoldTeam
.RETAIN ORIGINAL VALUES, IN CASE YOU MAKE A MODIFICATION
        pack    HoldName,CNTNAME
        call    Trim using HoldName
        pack    HoldPort,CNTPORT
        call    Trim using HoldPort
        pack    HoldInits,CNTInits
        call    Trim using HoldInits
        pack    HoldIact,CNTInactive
..............
.        move    C0,N3
.        getitem CNTDataListTeam,1,N3
.        if (N3 > C0)    .There are entries
.                move    C1,N2
.                loop
.                        getitem CNTDataListTeam,N2,str2
.                        if (str2 = CNTTEAM)
.                                move    N2,N3
.                        endif
.                        add     C1,N2
.                        until (N2 > N3)
.                repeat
.        endif
.        setitem CNTDataListTeam,0,N3
...........
.
        move    CNTRIGHTS,N1
        move    CNTRIGHTS,TCNTRIGHTS
        setitem CNTComboRights,0,N1
.
.        deleteitem CNTDataListAddRights,0
.        move    CNTRIGHTS2,TCNTRIGHTS2
.        loop
.                clear   str2
.                move    TCNTRIGHTS2,str2
.                bump    TCNTRIGHTS2,C2
.                call    Trim using str2
.                until (str2 = "")
.                pack    NCNTFLD,str2
.                move    C1,NCNTPATH
.                move    "LoadScreens-NCNTKEY",Location
.                call    NCNTKEY
.                until over
.                pack    str55,CNTNUM,B1,CNTNAME
.                insertitem CNTDataListAddRights,99,str55
.        repeat
.
.begin patch 1.3
          call      debug
        SETitem     CNTEditTextInits,0,CNTINITS  
          IF        (CntInactive = yES)
          move      c1,result
          SETiTEM   CNTCheckBoxActive,0,result
          ELSE
          move      c0,result
          SETITEM   CNTCheckBoxActive,0,result
          ENDIF

          IF        (CntInactiv1 = yES)
          move      c1,result
          SETiTEM   CNTCheckBoxActiveCnt,0,result
          ELSE
          move      c0,result
          SETITEM   CNTCheckBoxActiveCnt,0,result
          ENDIF

          

.enD patch 1.3
        call    ContactLoadDataListRights
        return

.Disable Upper Screen
ContactDisableUpper
        setprop CNTEditSearch,enabled=0
        setprop CNTOK,enabled=0
        setprop CNTExit,enabled=0
        setprop CNTNew,enabled=0
        setprop CNTListViewSearch,enabled=0,bgcolor=grey
        return

.Enable Upper Screen
ContactEnableUpper
.Allow Exit
        move    "Y",ExitFlag
        setprop CNTEditSearch,enabled=1
        setprop CNTOK,enabled=1
        setprop CNTExit,enabled=1
        setprop CNTNew,enabled=1
        setprop CNTListViewSearch,enabled=1,bgcolor=white
        return

.Disable Lower Screen
ContactDisableLower
        setprop CNTComboUsage,enabled=0,bgcolor=grey
        setprop CNTEDITSales,enabled=0,bgcolor=grey
        setprop CNTComboPrint,enabled=0,bgcolor=grey
        setprop CNTComboRights,enabled=0,bgcolor=grey
.        setprop CNTDataListTeam,enabled=0,bgcolor=grey
        setprop CNTComboTeam,enabled=0,bgcolor=grey
        setprop CNTDataListAddRights,bgcolor=grey
        setprop CNTEditNum,enabled=0,bgcolor=grey
        setprop CNTEditName,enabled=0,bgcolor=grey
        setprop CNTEditPhone,enabled=0,bgcolor=grey
        setprop CNTEditPort,enabled=0,bgcolor=grey
        setprop CNTDataListRights,visible=0
        setprop CNTTo,visible=0
        setprop CNTFrom,visible=0
        setprop CNTModify,enabled=1
        setprop CNTQuit,visible=0
        setprop CNTDelete,visible=0
        setprop CNTSave,visible=0
        setprop CNTAddTeam,visible=0
        setprop CNTSaveTeam,visible=0
        setprop CNTStatTeamInstruct,visible=0
        setprop CNTStatTeamName,visible=0
        setprop CNTEditTeam,visible=0
.begin patch 1.3
          SETprop     CNTEditTextInits,enabled=0,bgcolor=grey
          SETProp   CNTCheckBoxActive,enabled=0
          SETProp   CNTCheckBoxActiveCnt,enabled=0
.end patch 1.3
        return

.Enable Lower Screen
ContactEnableLower
        move    "N",UpdateFlag
        move    "N",ExitFlag
        setprop CNTComboUsage,enabled=1,bgcolor=white
        setprop CNTEditSales,enabled=1,bgcolor=white
        setprop CNTComboPrint,enabled=1,bgcolor=white
        setprop CNTComboRights,enabled=1,bgcolor=white
.        setprop CNTDataListTeam,enabled=1,bgcolor=white
        setprop CNTComboTeam,enabled=1,bgcolor=white
        setprop CNTDataListAddRights,bgcolor=white
        setprop CNTEditNum,bgcolor=white
        setprop CNTEditName,enabled=1,bgcolor=white
        setprop CNTEditPhone,enabled=1,bgcolor=white
        setprop CNTEditPort,enabled=1,bgcolor=white
        setprop CNTDataListRights,visible=1
        setprop CNTTo,visible=1
        setprop CNTFrom,visible=1
        setprop CNTModify,enabled=0
        setprop CNTQuit,visible=1
        setprop CNTDelete,visible=1
        setprop CNTSave,visible=1
.begin patch 1.3
          SETprop     CNTEditTextInits,enabled=1,bgcolor=white
          SETProp   CNTCheckBoxActive,enabled=1
          SETProp   CNTCheckBoxActiveCnt,enabled=1
.end patch 1.3
        return

.Verify Data Entry
ContactVerifyData
        getitem CNTEditNum,0,CNTNUM
        call    TRIM using CNTNUM
        count   HowMany,CNTNUM
        if (HowMany < 2)
                alert caution,"2 Digit Contact Number Required!",result
                setfocus CNTEditNum
                move "Y",ReturnFlag
                return
        endif
        if (NewFlag = YES)
                move    C1,NCNTPATH
.begin patch xxx
.                pack    NCNTFLD,CNTNUM
                packkey    NCNTFLD,CNTNUM
                    move      c4,ncntpath                    
.end patch xxx
                move    "Verify-NCNTTST",Location
                call    NCNTTST
                if not over
                        alert caution,"Contact Number Already In Use!",result
                        setfocus CNTEditNum
                        move "Y",ReturnFlag
                        return
                endif
        endif
        setitem CNTEditNum,0,CNTNUM
.
        getitem CNTEditName,0,CNTNAME
        call    TRIM using CNTNAME
        if (CNTNAME = "")
                alert caution,"Contact Name Required!",result
                setfocus CNTEditName
                move "Y",ReturnFlag
                return
        endif
.        if (str35 <> "" AND CNTNAME <> str35)
.                clear   taskname
.                append  "Do you want to overwrite",taskname
.                append  carr,taskname
.                append  str35,taskname
.                append  carr,taskname
.                append  "with",taskname
.                append  carr,taskname
.                append  CNTNAME,taskname
.                append  "?",taskname
.                reset   taskname
.                alert   plain,taskname,result
.                if (result <> 1)
.                        setfocus CNTEditName
.                        move "Y",ReturnFlag
.                        return
.                endif
.        endif
        setitem CNTEditName,0,CNTNAME
.
        getitem CNTEditPhone,0,CNTPHONE
        call    Trim using CNTPHONE
        setitem CNTEditPhone,0,CNTPHONE
.
        getitem CNTEditPort,0,CNTPORT
        call    Trim using CNTPORT
        count   howmany,CNTPORT
        if (howmany < 3)
                alert   caution,"3 Digit Port Number Required!",result
                setfocus CNTEditPort
                move    "Y",ReturnFlag
                return
          else
.PATCH1.1
.begin patch xxx
.                    if (NEWFLAG = YES)
.                            move    C3,NCNTPATH
.                              pack    NCNTFLD1,CNTPORT
.                            move    "Verify-NCNTTST",Location
.                    call    NCNTTST
.                            if not over
.                              alert caution,"PORT Number Already In Use!",result
.                                      setfocus CNTEditPORT
.                                      move "Y",ReturnFlag
.                              return
.                              endif
.
.                    elseif (HoldPort <> CNTPORT)
.
.                                      move    C3,NCNTPATH
.                                        pack    NCNTFLD1,CNTPORT
.                                      move    "Verify-NCNTTST",Location
.                              call    NCNTTST
.                                      if not over
.                                                  if (cntport <> "999")
.                                                  alert caution,"PORT Number Already In Use!",result
.                                                setfocus CNTEditPORT
.                                                move "Y",ReturnFlag
.                                        return
.                                                  endif
.                              endif
.                    endif
.begin patch xxx
          endif
.PATCH1.1

        setitem CNTEditPort,0,CNTPORT
.
.        getitem CNTDataListTeam,0,N2
        getitem CNTComboTeam,0,N2
        if (N2 = 1)
                alert   caution,"You Should choose a Team!!",result
                setfocus CNTComboTeam
.                move    "Y",ReturnFlag
.                return
                getitem CNTComboTeam,N2,CNTTEAM
        else
                getitem CNTComboTeam,N2,CNTTEAM
        endif
.
        getitem CNTComboPrint,0,CNTPRINT
..begin patch 1.3
          Getitem     CNTEditTextInits,0,CntInits
          GETItem   CNTCheckBoxActive,0,result
          if        (result = 1)
          move      Yes,CNTInactive
          else
          clear     CntInactive
          endif
          GETItem   CNTCheckBoxActiveCnt,0,result
          if        (result = 1)
          move      Yes,CNTInactiv1
          else
          clear     CntInactiv1
          endif
.end patch 1.3

.
        getitem CNTEditSales,0,CNTSales
        call    Trim using CNTSales
        count   howmany,CNTSales
        if (howmany < 2)
                alert   caution,"2 Digit Sales Number Required!",result
                setfocus CNTEditsales
                move    "Y",ReturnFlag
                return
          Elseif    (cntInactive = yes)
.No more checking required its an inactive sales record
          else
                    call      debug
                    packkey   ncntfld4,cntsales
                    READ      NCNTFIL4,NCNTFLD4;*tab=1,str2,*tab=114,str1
                    goto      endslscheck if over
                    rep       zfill,str2
                    if        (str1 = b1 & cntnum <> str2)
                alert   caution,"This Sales Number is already active!",result
                setfocus CNTEditsales
                move    "Y",ReturnFlag
                return
                    endif
                    loop
                    readks    ncntfil4;*tab=112,str2,str1
                    until     over
                    if        (str2 = cntsales & str1 = b1)
                alert   caution,"This Sales Number is already active!",result
                setfocus CNTEditsales
                move    "Y",ReturnFlag
                return
                    endif

                    repeat
          endif
endslscheck
.
.        getitem CNTComboUsage,0,N1
        getitem CNTComboUsage,0,cntcnt
.        sub     C1,N1
.        if (N1 <> C0)
.                move    N1,CNTCNT
.        else
.                move    B1,CNTCNT
.        endif
       getitem CNTComboBox001,0,N1
               move    N1,CNTComp
.
        clear   TCNTRIGHTS2
        getitem CNTDataListAddRights,1,N3
        if (N3 > C0)    .There are entries
                move    C1,N2
                move    CNTNUM,TCNTNUM
                move    CNTNAME,TCNTNAME
                move    CNTPHONE,TCNTPHONE
                move    CNTPORT,TCNTPORT
                move    CNTTEAM,TCNTTEAM
                move    CNTPRINT,TCNTPRINT
                move    CNTCNT,TCNTCNT
                move    CNTComp,TCNTComp
                move    CNTSales,TCNTSales
                move    CntInactive,TCntInactive
                move    CntInactiv1,TCntInactiv1
                loop
                        getitem CNTDataListAddRights,N2,str2
                        until (str2 = "  ")
                        move    C1,NCNTPATH
                        move    "Verify-NCNTKEY",Location
                        rep     zfill,str2
                        packkey NCNTFLD,str2
                        clear   CNTTEAM
                        call    NCNTKEY
                        if not over
                                if (CNTTEAM <> TCNTTEAM)
                                        append  str2,TCNTRIGHTS2
                                endif
                        endif
                        add     C1,N2
                        until (N2 > N3)
                repeat
                move    TCNTNUM,CNTNUM
                move    TCNTNAME,CNTNAME
                move    TCNTPHONE,CNTPHONE
                move    TCNTPORT,CNTPORT
                move    TCNTTEAM,CNTTEAM
                move    TCNTPRINT,CNTPRINT
                move    TCNTCNT,CNTCNT
                move    TCNTComp,CNTComp
                move    TCNTSales,CNTSales
                rep zfill,cntsales
                move    TCntInactive,CntInactive
                move    TCntInactiv1,CntInactiv1
                packkey NCNTFLD,TCNTNUM
        endif
        reset   TCNTRIGHTS2
        move    TCNTRIGHTS2,CNTRIGHTS2
.
        getitem CNTComboRights,0,N1
        move    N1,CNTRIGHTS
        if (CNTRIGHTS = "2" AND TCNTRIGHTS2 <> "2")
                alert   plain,"You want to assign this Contact as Team Captain?",result
                if (result <> 1)
                        setfocus CNTComboRights
                        move    "Y",ReturnFlag
                        return
                endif
                move    C0,N1           .Flag to indicate a hit
                move    CNTNUM,TCNTNUM
                move    CNTNAME,TCNTNAME
                move    CNTPHONE,TCNTPHONE
                move    CNTPORT,TCNTPORT
                move    CNTTEAM,TCNTTEAM
                move    CNTRIGHTS,TCNTRIGHTS
                move    CNTRIGHTS2,TCNTRIGHTS2
                move    CNTPRINT,TCNTPRINT
                move    CNTCNT,TCNTCNT
                move    CNTComp,TCNTComp
                move    CNTSales,TCNTSales
                move    CntInactive,TCntInactive
                move    CntInactiv1,TCntInactiv1
                move    "Verify-NCNTSEQ0",Location
                move    C1,NCNTPATH
                call    NCNTSEQ0
                if not over
                        if (CNTTEAM = TCNTTEAM AND CNTRIGHTS = "2")
                                move    C1,N1
                        else
                                loop
                                        move    "Verify-NCNTSEQ",Location
                                        call    NCNTSEQ
                                        until over
                                        if (CNTTEAM = TCNTTEAM AND CNTRIGHTS = "2")
                                                move    C1,N1
                                                break
                                        endif
                                repeat
                        endif
                endif
                if (N1 = C1)
                        call    Trim using CNTNAME
                        clear   taskname
                        append  CNTNAME,taskname
                        append  carr,taskname
                        append  "will now be a Team member!",taskname
                        reset   taskname
                        alert   note,taskname,result
                        packkey NCNTFLD,CNTNUM
                        move    C1,NCNTPATH
                        move    "New Capt.-NCNTKEY",Location
                        call    NCNTKEY
                        if over
                                alert   caution,"Unable to update!!!",result
                        else
                                move    "3",CNTRIGHTS
                                move    "New Capt.-NCNTUPD",Location
                                call    NCNTUPD
                        endif
                endif
                move    YES,LoadFlag
.Position back to original record
                packkey NCNTFLD,TCNTNUM
                move    TCNTNUM,CNTNUM
                move    TCNTNAME,CNTNAME
                move    TCNTPHONE,CNTPHONE
                move    TCNTPORT,CNTPORT
                move    TCNTTEAM,CNTTEAM
                move    TCNTRIGHTS,CNTRIGHTS
                move    TCNTRIGHTS2,CNTRIGHTS2
                move    TCNTPRINT,CNTPRINT
                move    TCNTCNT,CNTCNT
                move    TCNTComp,CNTComp
                move    TCNTSales,CNTSales
                rep zfill,cntsales
                move    TCntInactive,CntInactive
                move    TCntInactiv1,CntInactiv1
        endif
        return

RefreshContactSearchList
.First Test for Blank Entry After "?"
        count    result,key
        if (result = "1")
                setprop ErrorMssg,visible=1
                setfocus CNTEditSearch
                setprop CNTOK,enabled=1
                return
        elseif (result < 3)
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
                setitem ErrorMssgStat5,0,"  Name Must Have 3+ Characters"
                setprop ErrorMssg,visible=1
                call    ContactSetErrorMssgDefault
                setfocus CNTEditSearch
                setprop CNTOK,enabled=1
                return
        endif
        setprop CNTListViewSearch,enabled=1,bgcolor=white
        move    C3,NCNTLOCK
        move    C2,NCNTPATH
        pack    NCNTFLD2,AKey1,key
        move    "Driver-NCNTAIM",Location
        call    NCNTAIM
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

                setprop CNTOK,enabled=1
                setfocus CNTEditSearch
                return
        else
.if kept needs to be rewritten
.                getitem CNTListViewSearch,1,N4
.                if (N4 > C0)    .There are entries
.                        move    C1,N3
.                        loop
.                                getitem CNTListViewSearch,N3,str2
.                                until (N3 > N4)
.                                until (str2 = CNTNUM)
.                                add     C1,N3
.                        repeat
.                endif
        endif
.        setitem CNTListViewSearch,1,N3
.end of it
.Load the Inquiry Screen immediately
        call    ContactData
        return

.ContactLoadDataLists
.        deleteitem CNTComboTeam,0
.        insertitem CNTComboTeam,0,""
.        loop
.                move    "PreMainLoop-NCNTSEQ",Location
.                call    NCNTSEQ
.                move    CNTPORT,N3
.                if (PORTN = N3)
.                        move    CNTTEAM,HoldTeam
.                endif
.                until over
.                pack    hold,CNTNUM,B1,CNTNAME,CNTPHONE,CNTPORT,CNTTEAM:
.                        CNTRIGHTS,CNTRIGHTS2
.                insertitem CNTListViewSearch,999,hold
.                insertitem CNTDataListRights,999,hold
.                if (CNTRIGHTS = "2")    .Only 1 Captain per team allowed
.                        call    Trim using CNTNAME
.                        pack    str55,CNTTEAM,B1,DASH,B1,CNTNAME,"'s Team"
.                        insertitem CNTComboTeam,99,str55
.                endif
.        repeat
.        return
ContactLoadListViewSearch
          CNTListViewSearch.DeleteAllItems giving result      .Prepare to refresh it
        move    "0-NCNTSEQ",Location
        move    C1,NCNTPATH

           loop
        call    NCNTSEQ
           until over
                move    CNTPORT,N3
                if (PORTN = N3)
                        move    CNTTEAM,HoldTeam
                endif
.begin patch 1.41
           call       trim using cntinits
           if         (cntinits = "" & CntInactiv1 <> "Y")
           packkey    nusefld from cntport
           call       nusekey
                      if         not over
                      move       Nuseinit,cntinits
                      endif
           endif
.end patch 1.41
                
                pack    hold,CNTNUM,B1,CNTNAME,CNTPHONE,CNTPORT,CNTTEAM:
                        CNTRIGHTS,CNTRIGHTS2,CNTPRINT,CNTCNT,cntcomp,CntSales,CNTINITS,CNTINACTIVE,CNTINACTIV1

                      Rep        Zfill,CntSales
.                    CNTListViewSearch.InsertItem giving N9 using CNTnum
.                    CNTListViewSearch.SetItemText using N9,CntName,1
.                    CNTListViewSearch.SetItemText using N9,CNTPORT,2
.                    CNTListViewSearch.SetItemText using N9,CntSales,3
.                    CNTListViewSearch.SetItemText using N9,Hold,4
.new way
             MOVE     "9999",CurIndex
             CNTListViewSearch.InsertItemEX Using cntnum,curIndex:
                                 *SubItem1=cntname:
                                 *SubItem2=cntport:
                                 *SubItem3=cntsales:
                                 *SubItem4=Hold

        repeat

          CNTListViewSearch.EnsureVisible using 0,0
          CNTListViewSearch.SetItemState giving N9 using 0,2,2
          call      Click_CntListViewSearch




.        deleteitem CNTListViewSearch,0
.        move    "0-NCNTSEQ0",Location
.        move    C1,NCNTPATH
.        call    NCNTSEQ0
.        if not over
.                move    CNTPORT,N3
.                if (PORTN = N3)
.                        move    CNTTEAM,HoldTeam
.                endif
.                pack    hold,CNTNUM,B1,CNTNAME,CNTPHONE,CNTPORT,CNTTEAM:
.                        CNTRIGHTS,CNTRIGHTS2,CNTPRINT,CNTCNT,cntcomp,CntSales,CNTINITS,CNTINACTIVE,CNTINACTIV1
.                insertitem CNTListViewSearch,999,hold
.                loop
.                        move    "0-NCNTSEQ",Location
.                        call    NCNTSEQ
.                        until over
.                        move    CNTPORT,N3
..                        if (PORTN = N3)
.                                move    CNTTEAM,HoldTeam
.                        endif
.                        pack    hold,CNTNUM,B1,CNTNAME,CNTPHONE,CNTPORT,CNTTEAM:
.                                CNTRIGHTS,CNTRIGHTS2,CNTPRINT,CNTCNT,cntcomp,CntSales,CNTINITS,CNTINACTIVE,CNTINACTIV1
.                        insertitem CNTListViewSearch,999,hold
.                repeat
.        endif
        return
ContactLoadComboTeam
        deleteitem CNTComboTeam,0
        insertitem CNTComboTeam,0,""
        move    "1-NCNTSEQ0",Location
        move    C1,NCNTPATH
        call    NCNTSEQ0
        if not over
                if (CNTRIGHTS = "2")    .Only 1 Captain per team allowed
                        call    Trim using CNTNAME
                        pack    str55,CNTTEAM,B1,DASH,B1,CNTNAME,"'s Team"
                        insertitem CNTComboTeam,99,str55
                endif
                loop
                        move    "1-NCNTSEQ",Location
                        call    NCNTSEQ
                        until over
                        if (CNTRIGHTS = "2")    .Only 1 Captain per team allowed
                                call    Trim using CNTNAME
                                pack    str55,CNTTEAM,B1,DASH,B1,CNTNAME,"'s Team"
                                insertitem CNTComboTeam,99,str55
                        endif
                repeat
        endif
        return

ContactLoadDataListRights
.        deleteitem CNTDataListRights,0
.        move    "2-NCNTSEQ0",Location
.        call    NCNTSEQ0
.        if not over
.                if (CNTTEAM <> HoldTeam)
.                        pack    hold,CNTNUM,B1,CNTNAME,CNTPHONE,CNTPORT,CNTTEAM:
.                                CNTRIGHTS,CNTRIGHTS2,CNTPRINT,CNTCNT
.                        insertitem CNTDataListRights,999,hold
.                endif
.                loop
.                        move    "2-NCNTSEQ",Location
.                        call    NCNTSEQ
.                        until over
.                        if (CNTTEAM <> HoldTeam)
.                                pack    hold,CNTNUM,B1,CNTNAME,CNTPHONE,CNTPORT,CNTTEAM:
.                                        CNTRIGHTS,CNTRIGHTS2,CNTPRINT,CNTCNT
.                                insertitem CNTDataListRights,999,hold
.                        endif
.                repeat
.        endif
.        return
        deleteitem CNTDataListRights,0
        deleteitem CNTDataListAddRights,0
        move    CNTRIGHTS2,TCNTRIGHTS2
        move    "2-NCNTSEQ0",Location
        move    C1,NCNTPATH
        call    NCNTSEQ0
        if not over
                move    C0,N1
                loop
                        clear   str2
                        move    TCNTRIGHTS2,str2
                        until (str2 = "  ")
                        until (str2 = " ")
                        until (str2 = "")
                        if (str2 = CNTNUM)
                                move    C1,N1
                                break
                        endif
                        bump    TCNTRIGHTS2,C2
                        until over
                repeat
                if (CNTTEAM <> HoldTeam)
                        pack    str55,CNTNUM,B1,CNTNAME
                        if (N1 = C1)
                                insertitem CNTDataListAddRights,999,str55
                        else
                                insertitem CNTDataListRights,999,str55
                        endif
                endif
                loop
                        move    "2-NCNTSEQ",Location
                        call    NCNTSEQ
                        until over
                        move    C0,N1
                        reset   TCNTRIGHTS2
                        loop
                                clear   str2
                                move    TCNTRIGHTS2,str2
                                until (str2 = "  ")
                                until (str2 = " ")
                                until (str2 = "")
                                if (str2 = CNTNUM)
                                        move    C1,N1
                                        break
                                endif
                                bump    TCNTRIGHTS2,C2
                                until over
                        repeat
                        if (CNTTEAM <> HoldTeam)
                                pack    str55,CNTNUM,B1,CNTNAME
                                if (N1 = C1)
                                        insertitem CNTDataListAddRights,999,str55
                                else
                                        insertitem CNTDataListRights,999,str55
                                endif
                        endif
                repeat
        endif
        return
COPY_TO_B
        call    COPY_SELECTION Using CNTDataListAddRights,CNTDataListRights
        return

COPY_FROM_B
        call    COPY_SELECTION USING CNTDataListRights,CNTDataListAddRights
        return

COPY_SELECTION    LROUTINE P_TO,P_FROM
. Find the first selected item.
        P_FROM.GetFirstSel Giving result
        return if (result < 0)  // Nothing was selected, just return.
. Now copy the first item
        P_FROM.GetText Giving str45 using result
        move    str45,str3
        call    InnerLoop
. Find and copy other selected items.
        loop
                P_FROM.GetNextSel GIVING result
                until (result < 0)
                P_FROM.GetText Giving str45 Using result
                move    str45,str3
                call    InnerLoop
        repeat
.Delete entries that were moved over
.There is a problem with putting DeleteString line in InnerLoop.
.Successive entries are not moved over as GetNextSel does not readjust when
.items are dynamically deleted.
        P_TO.GetCount Giving N9
        if (N9 > 0)
                move    C0,howmany
                loop
                        P_TO.GetText Giving str3 using howmany
                        P_FROM.GetCount Giving N8
                        if (N8 > 0)
                                move    C0,result
                                loop
                                        P_FROM.GetText Giving str2 using result
                                        reset   str3
                                        scan    str2,str3
                                        if equal
                                                P_FROM.DeleteString Giving N7 Using *Index=result
                                                break
                                        endif
                                        add     C1,result
                                        until (result >= N8)
                                repeat
                        endif
                        add     C1,howmany
                        until (howmany >= N9)
                repeat
        endif
. Items were transferred, now reset selection.
        call    RESET_SELECTION Using P_FROM
        return

InnerLoop
        move    C0,N7                           Flag to indicate if duplicate found
        P_TO.GetCount Giving N9
        if (N9 > 0)
                move    C0,howmany
                loop
                        P_TO.GetText Giving str2 using howmany
                        reset   str3
                        scan    str2,str3
                        if equal
                                move    C1,N7   .Duplicate in P_TO
                                break
                        endif
                        add     C1,howmany
                        until (howmany >= N9)
                repeat
        endif
        if (N7 = C0)                            .No duplicate in P_TO
                P_TO.AddString Using str45
        endif
        return

RESET_SELECTION   LROUTINE P_DL
        loop
                P_DL.GetFirstSel Giving result
                until (result < 0)
                P_DL.SetSel Using 0,result
        repeat
        return

ContactClearFieldFlags
        move    NO,LoadFlag
        return
.Include IO file
        include ncntio.inc
        include npasio.inc
.begin patch 1.41
        include nuseio.inc   
.end patch 1.41
        include comlogic.inc
