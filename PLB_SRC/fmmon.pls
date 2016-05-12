****************************************************
. Sunbelt Application Server Monitor
.
.    by Matthew Lake
.         Sunbelt Computer Systems, Inc.
.
.
.
.Start with the log file structure...
.
PC        Equ       0
               include        common.inc
               Include        Cons.inc
Release        Init           "1.1"          ;quick and dirty update due to log format change
Reldate        init           "23 June 2003"
.Release        Init           "1.0"
.Reldate        init           "05 March 2003"
.    - The format of the File Manager has been changed as follows:
. 
.             1. Each record has a fixed length of 79 characters.  Log
.                records longer than 79 characters will wrap to the next
.                log record.  Records larger than 158 characters are truncated.
. 
.             2. Each record has a field format as follows:
. 
.                  YYYY-MM-DD HH:MM:SS NNNN L DATA
. 
.                  Where:
. 
.                        YYYY-MM-DD - Current date
.                        HH:MM:SS   - Current time
.                        NNNN       - Admin slot identification
.                        L          - File Manager log level value
.                        DATA       - Log data
.2015-10-06 13:36:13 0000 0 Plbserve Version: '9.8   01 Oct 2015' is Started    
.2015-10-06 13:36:13 0000 0 Ported   Machine: 'WIN32'                           
.2015-10-06 13:36:13 0000 0 REGISTRY Auth Number: C882-12AB-GJ16-0018-1JC4-0F98 
.2015-10-06 13:36:13 0000 0 SERVER Serial Number: N005246                       
.2015-10-06 13:36:13 0000 0 Maximum connections : 5                             
.2015-10-06 13:36:13 0000 0 Processor Architecture: '0'                         
.2015-10-06 13:36:13 0000 0 Number of Processors  : '4'                         
.2015-10-06 13:36:13 0000 0 Active Processor Mask : '0xf'                       
.2015-10-06 13:36:13 0000 0 Processor Type        : '586'                       
.2015-10-06 13:36:13 0000 0 Processor Speed       : '1995MHz'                   
.2015-10-06 13:36:13 0000 0 Logon UserName: 'wadmin'                            
.2015-10-06 13:36:13 0000 0 PORTNUM being used is: (3933)                       
.2015-10-06 13:36:13 0000 0 HOSTNAME being used is: (10.10.30.1)                
.2015-10-06 13:36:13 0000 0 Unable to bind socket (10048:Unknown error)         
.2015-10-06 13:36:14 0000 0 Plbserve Version: '9.8   01 Oct 2015' is Terminated.
.2015-10-06 13:36:14 0000 0 ************************END*************************
 
.begin patch 1.1
Log_entry LIST
.GMT      DIM       8         ; 1-8  unix time stamp
FMDate         Dim            10             ; 1-10 ;                        YYYY-MM-DD - Current date
dumb           dim            1              ; 11-11 
FMTIme         DIm            8              ; 12-19
dumb1     DIM       1              ; 20-20 padding
ConNum         Dim            4              ; 21-24      admin slot ID
dumb2     DIM       3         ; 25-27 padding
msg       DIM       17        ; 28-44 log message
.conNum   DIM       8         ; 45-52 Hex connection number
.msg      DIM       17        ;20-36 log message
.dumb3    DIM       1         ;37-37 padding
mdata     DIM       40        ;48-87 message data
          LISTEND
.end patch 1.1
.2016-01-15 05:38:43 0000 0 Data Manager Version: '9.8Aa 09 Oct 2015' is Started
.2016-01-15 05:38:43 0000 0 Ported Machine......: 'WIN32'                       
.2016-01-15 05:38:43 0000 0 REGISTRY Auth Number: C080-122D-LU1N-0220-4G84-0F99 
.2016-01-15 05:38:43 0000 0 SUNDM Serial Number : N003152                       
.2016-01-15 05:38:43 0000 0 Maximum connections : 60                            
.2016-01-15 05:38:43 0000 0 Processor Architecture: '0'                         
.2016-01-15 05:38:43 0000 0 Number of Processors  : '8'                         
.2016-01-15 05:38:43 0000 0 Active Processor Mask : '0xff'                      
.2016-01-15 05:38:43 0000 0 Processor Type        : '586'                       
.2016-01-15 05:38:43 0000 0 Processor Speed       : '2533MHz'                   
.2016-01-15 05:38:43 0000 0 Logon UserName: 'wadmin'                            
.2016-01-15 05:38:43 0000 0 Public Encryption USED without LOGON required!      
.2016-01-15 05:38:43 0000 0 IP Filtering is NOT USED!                           
.2016-01-15 05:38:43 0000 0 PORTNUM being used is: (502)                        
.2016-01-15 05:38:43 0000 0 HOSTNAME being used is: (10.10.30.103)              
.2016-01-15 05:38:43 0000 0 Logon socket is being created.                      
.2016-01-15 05:38:43 0000 0 Runtime PLBENV_ & PLBVOL_ substitution allowed!     
.2016-01-15 05:38:43 0000 0 Logon Timeout set to: 250ms                         
.2016-01-15 05:38:43 0000 0 Windows OS idle time defaults to 1 ms!              
.2016-01-15 05:38:43 0001 0 Admin PORTNUM being used is: (2103)                 
.2016-01-15 05:38:43 0000 0 Open once processing was turned OFF!                
.2016-01-15 05:38:43 0001 0 Admin Logon: HOSTNAME being used is: (10.10.30.103) 
.2016-01-15 05:38:43 0001 0 Admin Logon: Unable to bind socket (0:No error)     
.2016-01-15 05:38:43 0000 0 Replication is NOT USED!                            
.2016-01-15 05:38:48 0000 0 WATCHTIME being used is: (0)                        
.2016-01-15 05:41:58 0148 0 Child Start.....: 10.10.30.206,    0, (1754), 9801  
.                           Child Start...... Run Type     : '3'                
.                           Child Start...... Computer Name: 'VM0001'           
.                           Child Start...... User Name    : 'batch32c'         
.



logfname  DIM       250
logf      FILE
fpos      INTEGER   4

Path        Dim         250
PathFname   Dim         500
.begin patch 1.0
.V           Form        2
.seq      FORM      "-1"
.begin patch 1.1
.ConNumDec      Form            8
ConNumDec      Form            4
.end patch 1.1
Tabnum    form          2
STR100         DIM            100
STR100a         DIM            100
CurRec    form    5.2
CurVal    form      3
LastVal   form      3
.Tempfile       File            
UserByName     Dim            55
str150         dim            150
str62         dim            62
resultinDec    Dim            20               
.TimeStamp1      dim            20             mm/dd/ccyy hh:mm:ss        --- time child started            
min            dim            2
.Set Up Menu Bar

mFile    menu
mHelp    menu

.Set Up SubMenu for Options
mOptions Menu
sColor  submenu
sSearch submenu
.
.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
OData   init    "&Options;&Search-F2;-;&Color;"
HData   init    "&Help;&About"
endindex form      9
......Search  form       1
Timer   Timer
Refresh   Timer
holdsInfo dim      36
.............................................................................................................
.Set Vars used for About Box
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
.Load Forms, Always load parent form first
        formload x
        formload FMMONb,FMMon0001
        formload FMMona,FMMon0001
        formload abt
        formload mss1
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
        CREATE  Refresh,1200     .2 minutes
        ACTIVATE Refresh,RefreshNow,RESULT
.Create Menus
        CREATE  FMMon0001;MFile,FData
        create  FMMon0001;mOptions,OData,mFile
        create  FMMon0001;mHelp,HData,mOptions
.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
        activate mOptions
        activate mHelp,HelpGo,result
        FMMON0001aListView001.InsertColumn using "Connection Number",100,1
        FMMON0001aListView001.InsertColumn using "TCP/IP Address",100,2
        FMMON0001aListView001.InsertColumn using "User Name",170,3
        FMMON0001aListView001.InsertColumn using "Start Time",170,4
.
        FMMON0001aListView002.InsertColumn using "TCP/IP Address",100,1
        FMMON0001aListView002.InsertColumn using "Connection Number",100,2
        FMMON0001aListView002.InsertColumn using "User Name",170,3
        FMMON0001aListView002.InsertColumn using "Start Time",170,4
.
        FMMON0001BListView001.InsertColumn using "General File Manager Information",500,1
.end patch 1.0

            move       "*.log",logfname
            Clear       PathFName
.            move              "\\nins1\c\program files\sunbelt\sunfm86\" to pathfname
            move              "\\nins1\c\Windows\Logs\sunbelt\" to path

            GETFNAME        OPEN,"Open Test", LogFNAME,PATH,"PLS"
        IF      NOT OVER
.begin patch 1.0
        Pack        PathFname From Path,Logfname
        Setitem     FMMON001StatText001,0,Pathfname        
.        DISPLAY *HD,*R,"File name returned: ",logFName,*R,*p1:1,"     Path: ",PATH,"  ",*w3;
        ELSE
        Setitem     FMMON001StatText001,0,"Dialog Cancelled"
.       KEYIN   *HD,*R,"Dialog cancelled...",str1;
.end patch 1.0
        stop
        ENDIF
.begin patch 1.0
          call      FMMInitProgressBar
              pack            str55 from path,logfname
              open          tempfile,str55
          positeof      tempfile
          fposit    tempfile,N10
          calc      howmany=(N10/89)    .'89 = 87( record length) + 2 bytes for CR/LF
              close         tempfile
.end patch 1.

.         MOVE      "c:\sunbelt\sunfm.86\test\sunfm.log",logfname
. quickly get caught up
           call       debug
            Pack        PathFname From Path,Logfname
          OPEN      logf,PathFName,sharenf
          LOOP
                    READ      logf,seq;log_entry
                    BREAK     IF OVER
.begin patch 1.0
                 call         FMMUpdateProgressBar
.end patch 1.0
                    CALL      process_entry
          REPEAT
.begin patch 1.0
          FPOSIT    logf,fpos
                    CALL      displayConns

        loop
                waitevent
                Setitem refresh,0,1200   .reset to 2 min
                setitem timer,0,18000   .reset to 30 minutes
        repeat
        goto    timeout
.end patch 1.0
. now just go into monitor mode :o)
.         FPOSIT    logf,fpos
.         CALL      displayConns
.         LOOP
.                   READ      logf,seq;log_entry
.                   IF OVER
.                             PAUSE     "2"
.                             REPOSIT   logf,fpos
.                             CONTINUE
.                   ENDIF
.                   CALL      process_entry
.                   FPOSIT    logf,fpos
.         REPEAT
.                   CALL      displayConns
.         
.         STOP

conns     RECORD    (512)
conNum    DIM       4         ;Hex connection number --- converted to dec
msg       DIM       17        ;log message
mdata     DIM       40        ;message data
Muser          Dim            55             ;name
Fmdate         dim            10
FmTime         dim            8
.GMT            dim            8              ;unix time stamp        --- time child started            
               RECORDEND

counter   FORM      3
avail     FORM      3
. the process entry will manager the connection status
. array and displaying them on the screen
process_entry
.            scan        "PLBSERVE" in Logfname
.            If          not equal
.lets add info here instead of returning - display the goodies
.begin patch 1.1
.            If               (ConNum="00000000" or ConNum = "00000001")
            If               (ConNum="0000" or ConNum = "0001")
.end patch 1.1
.RETURN IF (ConNum="00000000")
.begin patch 1.0
.            call              DisInfo
               call           Load_info
.end patch 1.0
            return
            endif
               call           HexToDec using ConNum,ConNumDec
               move           ConNumDec to ConNum
                    CLEAR     avail
          FOR counter,"1","512"
                    IF ( ConNum=conns(counter).conNum )
                              SCAN      "Term",msg
                              IF EQUAL
                                        CLEAR conns(counter)
                                        BREAK
                              ENDIF
                    ENDIF
                    IF ( conns(counter).conNum="" & avail=0 )
                              MOVE counter,avail
                    ENDIF
          REPEAT
          SCAN      "Start",msg
          IF EQUAL
.                call          GetUserName
                    RESET     msg
                    MOVE      conNum,conns(avail).conNum
                    MOVE      msg,conns(avail).msg
                    MOVE      mdata,conns(avail).mdata
.begin patch 1.1
.                   MOVE      GMT,conns(avail).GMT
                    MOVE      FmDate,conns(avail).FmDate
                    MOVE      FmTime,conns(avail).FmTime
.end patch 1.1
          ENDIF
 RETURN

conip     DIM       16
horz      FORM      2
displayConns
.         DISPLAY   *HU," connNum  Ip Address     | connNum  Ip Address     | connNum  Ip Address     |",*N,*EF;
.         DISPLAY   *V=1
          SET       horz
          FOR counter,"1","512"
                    IF ( conns(counter).conNum!="")
                    PARSE     conns(counter).mdata,conip,"09.."
                              call           Load_detail
.                   DISPLAY *H=horz,conns(counter).conNum," ",conip;
                    ADD "26",horz
                    IF (horz >"75")
                              MOVE "1",horz
.                             DISPLAY   *N;
                    ENDIF
                    RESET     conns(counter).mdata
                    ENDIF
          REPEAT
 RETURN
DisInfo
            If          (v = 0)
.            DISPLAY          *p1:1,*ef;
            endif
.            Display     *p1:v,Log_Entry,*w2
            add         "1" to V
            return
.begin patch 1.0
...........................................................................................
lOAD_Detail
.begin patch 1.1
.                move           COnns(counter).GMT to GMT
.                call           UnixDateToCalcDate using Gmt,ResultinDec
.                              call           debug
                              clear          timestamp1
.                              unpack         resultindec to str2,YY,MM,DD,HH,min,SS
.                              pack           timestamp1 from mm,"/",dd,"/",str2,yy,b1,hh,":",min,":",ss
                              pack           timestamp1 from COnns(counter).Fmdate,b1,COnns(counter).FmTime
.end patch 1.1
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
           FMMon0001aListView001.SetItemText using N9,timestamp1,3
                FMMon0001aListView001.EnsureVisible GIVING n10 using N9,0
.
                FMMon0001aListView002.InsertItem giving N9 using Conip
           FMMon0001aListView002.SetItemText using N9,str100,1
           FMMon0001aListView002.SetItemText using N9,UserByName,2
           FMMon0001aListView002.SetItemText using N9,timestamp1,3
                FMMon0001aListView002.EnsureVisible GIVING n10 using N9,0
                return

...........................................................................................
ClearListView
               FMMon0001aListView001.DeleteAllItems giving N9
               FMMon0001aListView002.DeleteAllItems giving N9
.               FMMon0001bListView001.DeleteAllItems giving N9
               return
...........................................................................................
lOAD_INFO      Clear          Str100
               call           HexToDec using ConNum,ConNumDec
               move           ConNumDec to ConNum
.               call           debug
.begin patch 1.1
.               call           UnixDateToCalcDate using Gmt,ResultinDec
.                              clear          timestamp1
.                              unpack         resultindec to str2,YY,MM,DD,HH,min,SS
.                              pack           timestamp1 from mm,"/",dd,"/",str2,yy,b1,hh,":",min,":",ss
                              pack           timestamp1 from Fmdate,b1,fmtime
               PACK           Str100 with Timestamp1,b1,dumb1,conNum,dumb2,msg,mdata
                parse         str100 into str100a using " ~09",noskip,blankfill
                FMMon0001bListView001.InsertItem giving N9 using Str100a
                FMMon0001bListView001.EnsureVisible GIVING n10 using N9,0
                return

...........................................................................................
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
.Prevent occurance or accumulated events which may place "hidden" objects on wrong form
.ie, ResetStatus Checkbox. This generally only happens with LostFocus events from Stats2.plf
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT
        Endif
        return
. .........................................................................
.
Timeout
        beep
        beep
        beep
        winshow
        stop
.............................................................................................................
Refreshnow     call           ClearListView
.         call      FMMInitProgressBar
.               close          Logf
.               open          tempfile,str55
.         positeof      tempfile
.         fposit    tempfile,N10
.         calc      howmany=(N10/89)    .'89 = 87( record length) + 2 bytes for CR/LF
.              close         tempfile
.         OPEN      logf,PathFName,sharenf
.         LOOP
.                   READ      logf,seq;log_entry
.                   BREAK     IF OVER
.
.                              call     FMMUpdateProgressBar
.                   CALL      process_entry
.                   REPEAT
.                   CALL      displayConns
.;;;;;;;test dos
.         FPOSIT    logf,fpos
.         CALL      displayConns
          REPOSIT   logf,fpos
          LOOP
                    READ      logf,seq;log_entry
                    IF OVER
.                             PAUSE     "2"
.                             REPOSIT   logf,fpos
                              CALL      displayConns
                                             return
                    ENDIF
                              call      FMMUpdateProgressBar
                    CALL      process_entry
                    FPOSIT    logf,fpos
          REPEAT
          


               return               
.......................................................................................................
FMMonSortListView
.Dynamically sorts Different ListViews.  
.In order to switch between different ListViews we need two pieces of information.
.We need to ascertain which column was clicked AND which ListView we currently
.have visible, as each ListView has its' columns ordered differently.
.Getprops will determine which ListView is currently active, #EventResult passed to result
.prior to calling this subroutine will determine which column was clicked.
        getprop FMMon0001aListView001,visible=N9
        if (N9 = C1)    .Fmmon0001aListView001 is visible
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
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
.............................................................................................................
FileGo
                winshow
                stop
.......................................................................................................
HelpGo
        setprop AboutMssg,visible=1
        return       
.............................................................................................................
.debug   return
.............................................................................................................
FMMUpdateProgressBar
          calc      CurRec=(CurRec+1)
          calc      CurVal=((CurRec/howmany)*100)
          if (CurVal <> LastVal)
                    setitem   FMMProgressBar,0,CurVal
                    move      CurVal,LastVal
          endif
          return
FMMInitProgressBar
          move      C0,CurRec
          move      C0,CurVal
          move      C0,LastVal
          return
..................................................................................
GetUserName
               Getitem     FMMon001Check001,0,n1
               if          (n1 = 0)              .name lookup not selected
               clear        UserByName
               return
               endif
               CLEAR       STR100
               PACK        STR100 FROM "Name lookup for :",conip
               Setitem     FMMON001StatText002,0,str100
               setfocus    FMMON001StatText002
               Move           "Unknown User" to UserbyName
               pack           str55 from "c:\work\",conip,".tmp"
               pack           str45 from "c:\work\",conip,".srt"
               call           getwinver
               if ((osflag = c1)|(osflag = c5))
                              pack           str150 from "!c:\winnt\system32\cmd.exe /c"," Ping -a ",conip," > c:\work\",conip,".tmp"
               else
                              pack           str150 from "!c:\windows\system32\cmd.exe /c"," Ping -a ",conip," > c:\work\",conip,".tmp"
               endif
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
               erase          str45
               Setitem     FMMON001StatText002,0,"                     "
               return
..................................................................................
               include           comlogic.inc