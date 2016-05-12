****************************************************
. Sunfm Log Analyzer :o)
.
.    by Matthew Lake
.          Sunbelt Computer Systems, Inc.
.
.
.
.Start with the log file structure...
.
PC         EQu        0
               include        common.inc
               Include        Cons.inc
Release        Init           "1.0"
Reldate        init           "05 March 2003"
Log_entry  LIST
GMT        DIM        8          ; 1-8  unix time stamp
dumb1      DIM        2          ; 9-10 padding
conNum     DIM        8          ;11-18 Hex connection number
dumb2      DIM        1          ;19-19 padding
msg        DIM        17         ;20-36 log message
dumb3      DIM        1          ;37-37 padding
mdata      DIM        40         ;48-87 message data
           LISTEND

logfname   DIM        250
logf       FILE
fpos       INTEGER    4

Path        Dim         250
PathFname   Dim         500
;begin patch 1.0
;V           Form        2
;seq       FORM       "-1"
Tabnum     form          2
STR100         DIM            100
CurRec     form    5.2
CurVal     form       3
LastVal    form       3
.Tempfile       File            
UserByName     Dim            55
str150         dim            150
str62         dim            62
;Set Up Menu Bar

mFile    menu
mHelp    menu

;Set Up SubMenu for Options
mOptions Menu
sColor  submenu
sSearch submenu
;
;Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
OData   init    "&Options;&Search-F2;-;&Color;"
HData   init    "&Help;&About"
endindex form      9
;.....Search  form       1
Timer   Timer
Refresh   Timer
holdsInfo dim      36
;............................................................................................................
;Set Vars used for About Box
        move    "FMMon.PLS",Wprognme
        move    "File Manager Info",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        move    Reldate to Wreldate
mss1    plform  Error
abt     plform  About
FmMona  plform  FMMon0001A
FMMonb  plform  FMMon0001B
x       plform  FMMon0001
        winhide
;Load Forms, Always load parent form first
        formload x
        formload FMMONb,FMMon0001
        formload FMMona,FMMon0001
        formload abt
        formload mss1
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
        CREATE  Refresh,1200     .2 minutes
        ACTIVATE Refresh,RefreshNow,RESULT
;Create Menus
        CREATE  FMMon0001;MFile,FData
        create  FMMon0001;mOptions,OData,mFile
        create  FMMon0001;mHelp,HData,mOptions
;Activate Menus
;FileGo leads to stop
        activate mFile,FileGo,result
        activate mOptions
        activate mHelp,HelpGo,result
        FMMON0001aListView001.InsertColumn using "Connection Number",100,1
        FMMON0001aListView001.InsertColumn using "TCP/IP Address",100,2
        FMMON0001aListView001.InsertColumn using "User Name",150,3
        FMMON0001aListView002.InsertColumn using "TCP/IP Address",100,1
        FMMON0001aListView002.InsertColumn using "Connection Number",100,2
        FMMON0001aListView002.InsertColumn using "User Name",150,3
        FMMON0001BListView001.InsertColumn using "General File Manager Information",500,1
;end patch 1.0

            move       "*.log",logfname
            Clear       PathFName
            move              "\\nins1\c$\Windows\logs\sunbelt\" to pathfname
            GETFNAME        OPEN,"Open Test", LogFNAME,PATH,"PLS"
        IF      NOT OVER
;begin patch 1.0
        Pack        PathFname From Path,Logfname
        Setitem     FMMON001StatText001,0,Pathfname        
;        DISPLAY *HD,*R,"File name returned: ",logFName,*R,*p1:1,"     Path: ",PATH,"  ",*w3;
        ELSE
        Setitem     FMMON001StatText001,0,"Dialog Cancelled"
.       KEYIN   *HD,*R,"Dialog cancelled...",str1;
;begin patch 1.0
        stop
        ENDIF
.begin patch 1.0
           call       FMMInitProgressBar
              pack            str55 from path,logfname
              open          tempfile,str55
           positeof      tempfile
           fposit     tempfile,N10
           calc       howmany=(N10/89)      .'89 = 87( record length) + 2 bytes for CR/LF
              close         tempfile
.end patch 1.

;          MOVE       "c:\sunbelt\sunfm.86\test\sunfm.log",logfname
; quickly get caught up
            Pack        PathFname From Path,Logfname
           OPEN       logf,PathFName,sharenf
           LOOP
                      READ       logf,seq;log_entry
                      BREAK      IF OVER
;begin patch 1.0
                 call FMMUpdateProgressBar
;end patch 1.0
                      CALL       process_entry
           REPEAT
;begin patch 1.0
           FPOSIT     logf,fpos
                      CALL       displayConns
;end patch 1.0
        loop
                waitevent
                Setitem refresh,0,1200   .reset to 2 min
                setitem timer,0,18000   .reset to 30 minutes
        repeat
        goto    timeout
; now just go into monitor mode :o)
;          FPOSIT     logf,fpos
;          CALL       displayConns
;          LOOP
;                     READ       logf,seq;log_entry
;                     IF OVER
;                                PAUSE      "2"
;                                REPOSIT    logf,fpos
;                                CONTINUE
;                     ENDIF
;                     CALL       process_entry
;                     FPOSIT     logf,fpos
;          REPEAT
;                     CALL       displayConns
;          
;          STOP

conns      RECORD     (512)
conNum     DIM        8          ;Hex connection number
msg        DIM        17         ;log message
mdata      DIM        40         ;message data
Muser          Dim            55             ;name
               RECORDEND

counter    FORM       3
avail      FORM       3
. the process entry will manager the connection status
. array and displaying them on the screen
process_entry
;            scan        "PLBSERVE" in Logfname
;            If          not equal
;lets add info here instead of returning - display the goodies
            If               (ConNum="00000000" or ConNum = "00000001")
;RETURN IF (ConNum="00000000")
;begin patch 1.0
;            call              DisInfo
               call           Load_info
;end patch 1.0
            return
            endif
                      CLEAR      avail
           FOR counter,"1","512"
                      IF ( ConNum=conns(counter).conNum )
                                 SCAN       "Term",msg
                                 IF EQUAL
                                            CLEAR conns(counter)
                                            BREAK
                                 ENDIF
                      ENDIF
                      IF ( conns(counter).conNum="" & avail=0 )
                                 MOVE counter,avail
                      ENDIF
           REPEAT
           SCAN       "Start",msg
           IF EQUAL
                call          GetUserName
                      RESET      msg
                      MOVE       conNum,conns(avail).conNum
                      MOVE       msg,conns(avail).msg
                      MOVE       mdata,conns(avail).mdata
                      ENDIF
 RETURN

conip      DIM        16
horz       FORM       2
displayConns
;          DISPLAY    *HU," connNum  Ip Address     | connNum  Ip Address     | connNum  Ip Address     |",*N,*EF;
;          DISPLAY    *V=1
           SET        horz
           FOR counter,"1","512"
                      IF ( conns(counter).conNum!="")
                      PARSE      conns(counter).mdata,conip,"09.."
                              call           Load_detail
;                     DISPLAY *H=horz,conns(counter).conNum," ",conip;
                      ADD "26",horz
                      IF (horz >"75")
                                 MOVE "1",horz
;                                DISPLAY    *N;
                      ENDIF
                      RESET      conns(counter).mdata
                      ENDIF
           REPEAT
 RETURN
DisInfo
            If          (v = 0)
;            DISPLAY  *p1:1,*ef;
            endif
;            Display     *p1:v,Log_Entry,*w2
            add         "1" to V
            return
;begin patch 1.0
;..........................................................................................
lOAD_Detail
                if             (conns(counter).Muser="")
                call           GetUserName
                move           UserbyName,COnns(counter).MUser
                else
                Move          conns(counter).Muser to Userbyname
                endif
                Clear str100
                Move conns(counter).conNum to str100
                FMMon0001aListView001.InsertItem giving N9 using Str100
            FMMon0001aListView001.SetItemText using N9,Conip,1
            FMMon0001aListView001.SetItemText using N9,UserByName,2
                FMMon0001aListView001.EnsureVisible GIVING n10 using N9,0
;
                FMMon0001aListView002.InsertItem giving N9 using Conip
            FMMon0001aListView002.SetItemText using N9,str100,1
            FMMon0001aListView002.SetItemText using N9,UserByName,2
                FMMon0001aListView002.EnsureVisible GIVING n10 using N9,0
                return

;..........................................................................................
ClearListView
               FMMon0001aListView001.DeleteAllItems giving N9
               FMMon0001aListView002.DeleteAllItems giving N9
;               FMMon0001bListView001.DeleteAllItems giving N9
               return
;..........................................................................................
lOAD_INFO      Clear          Str100

               PACK           Str100 with GMT,dumb1,conNum,dumb2,msg,dumb3,mdata

                FMMon0001bListView001.InsertItem giving N9 using Str100
                FMMon0001bListView001.EnsureVisible GIVING n10 using N9,0
                return

;..........................................................................................
FMMonTabClick
        IF (N1 = C1)
             getprop FMMon0001aListView001,visible=N9
                Deactivate FMMona
        setprop FMMon0001bListView001,visible=1
        elseif (N1 = C2 )
                Deactivate FMMonb
        setprop FMMon0001aListView001,visible=1
        Endif
        return

FMMonTabChange
        IF (N1 = C1)
                move    C1,TabNum
                Activate FMMona
                setfocus FMMon0001aListView001
             setprop FMMon0001aListView001,visible=1
                        LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT

        else (N1 = C2)
             getprop FMMon0001aListView001,visible=N9
                move    C2,TabNum
                Activate FMMonb
                setfocus FMMon0001bListView001
                setprop FMMon0001bListView001,visible=1
;Prevent occurance or accumulated events which may place "hidden" objects on wrong form
;ie, ResetStatus Checkbox. This generally only happens with LostFocus events from Stats2.plf
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT
        Endif
        return
; .........................................................................
;
Timeout
        beep
        beep
        beep
        winshow
        stop
;............................................................................................................
Refreshnow     call           ClearListView
;          call       FMMInitProgressBar
;               close          Logf
;               open          tempfile,str55
;          positeof      tempfile
;          fposit     tempfile,N10
;          calc       howmany=(N10/89)      .'89 = 87( record length) + 2 bytes for CR/LF
;              close         tempfile
;          OPEN       logf,PathFName,sharenf
;          LOOP
;                     READ       logf,seq;log_entry
;                     BREAK      IF OVER
;
;                              call         FMMUpdateProgressBar
;                     CALL       process_entry
;                     REPEAT
;                     CALL       displayConns
;;;;;;;;test dos
;          FPOSIT     logf,fpos
;          CALL       displayConns
           REPOSIT    logf,fpos
           LOOP
                      READ       logf,seq;log_entry
                      IF OVER
;                                PAUSE      "2"
;                                REPOSIT    logf,fpos
                                 CALL       displayConns
                                             return
                      ENDIF
                              call          FMMUpdateProgressBar
                      CALL       process_entry
                      FPOSIT     logf,fpos
           REPEAT
           


               return               
;......................................................................................................
FMMonSortListView
;Dynamically sorts Different ListViews.  
;In order to switch between different ListViews we need two pieces of information.
;We need to ascertain which column was clicked AND which ListView we currently
;have visible, as each ListView has its' columns ordered differently.
;Getprops will determine which ListView is currently active, #EventResult passed to result
;prior to calling this subroutine will determine which column was clicked.
        getprop FMMon0001aListView001,visible=N9
        if (N9 = C1)    .Fmmon0001aListView001 is visible
;if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                If (result = 1)           ;clicked on TCP/IP
                        setprop FMMon0001aListView001,visible=0
                        setprop FMMon0001aListView002,visible=1
                   FMMon0001aListView002.EnsureVisible using c1,0
                        setfocus FMMon0001alistview002
                endif
        else
                getprop FMMon0001aListView002,visible=N9
        if (N9 = C1)    .FMMon0001aListView002 is visible
                If (result = 1)           ;clicked on Connection number
                        setprop FMMon0001aListView001,visible=1
                        setprop FMMon0001aListView002,visible=0
                   FMMon0001aListView001.EnsureVisible using c1,0
                        setfocus FMMon0001alistview001
                endif
        endif
        endif
                return
;............................................................................................................
FileGo
                winshow
                stop
;......................................................................................................
HelpGo
        setprop AboutMssg,visible=1
        return       
;............................................................................................................
;............................................................................................................
FMMUpdateProgressBar
           calc       CurRec=(CurRec+1)
           calc       CurVal=((CurRec/howmany)*100)
           if (CurVal <> LastVal)
                      setitem    FMMProgressBar,0,CurVal
                      move       CurVal,LastVal
           endif
           return
FMMInitProgressBar
           move       C0,CurRec
           move       C0,CurVal
           move       C0,LastVal
           return
;.................................................................................
GetUserName
               Move           "Unknown User" to UserbyName
               pack           str55 from "c:\work\",conip,".tmp"
               pack           str45 from "c:\work\",conip,".srt"
               pack           str150 from "!c:\windows\system32\cmd.exe /c"," Ping -a ",conip," > c:\work\",conip,".tmp"
               execute        str150
               pack           taskname with "c:\work\",conip,".tmp,","c:\work\",conip,".srt;S1=P,1-10"
               sort           taskname
               copyfile        str45 to str55 Mode=0x100

               open           tempfile,str55,exclusive
               loop
               read           tempfile,seq;str62
               if             not over
               scan           "Pinging " in str62
                              if equal
                              movefptr str62,n3
                              add c8 to n3
                              reset  str62,n3
                              scan b1 in str62
                              lenset str62
                              reset  str62,n3
                              move           str62 to UserByName
                              endif
               else
                              goto getusernameexit
                              endif
               repeat
getusernameexit
               close          tempfile,delete 
               return
;.................................................................................
               include           comlogic.inc