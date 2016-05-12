.-----------------------------------------------------------------------------------------
. Misc. PROCESS lookup's                                                   ©2006.11.24/OMS
.-----------------------------------------------------------------------------------------
. Revisions:
.
.      2006.11.24/OMS  Now using GetTopWindow to ensure proper output
.      2001.11.20/OMS  Now including a Process Monitor
.
.-----------------------------------------------------------------------------------------
. Look up program:    Check active windows titles for a match.
.
. CALL syntax:
.
.        CALL     FindProc USING title, wTitle
.
. Parameter:      title    (I)   DIM max 250  Full or partial WindowTitle to look up
.                 wTitle   (O)   DIM max 250  WindowTitle+handle found (Optional)
.                                             Format: <wtitle><0x09><handle>
.
. Exit:           EQUAL    title matched a windowTitle, wTitle assigned
.              NOT EQUAL   no hit                       wTitle cleared
.                 LESS     no title given as input
.
. A "SCAN  title IN windowTitle" is performed. EQUAL / NOT EQUAL reflects the SCAN.
. All windows are searched. Search is repeated up to three times in 0.5 sec intervals, so
. a complete search may take up to 1.5 seconds.
.
.-----------------------------------------------------------------------------------------
. Look up programs:    Get title and wHandle for all active processes
.
. CALL syntax:
.
.        CALL     FindJobs USING dataList
.
. Parameter:      dataList (I/O) DATALIST     MUST be created on entry
.
. Exit:           EQUAL    dataList assigned
.              NOT EQUAL   no hit/error
.
. dataList will hold WinTitle and -handle for every process found (including CALLER)
. Each element contains <winTitle><0x09><winHandle>, where
.                       <winTitle>   DIM up to 250 chars
.                       <winHandle>  FORM 10
.
.----------------------------------------------------------------------------------------------
. Get Window-Title:
.
. CALL syntax:
.
.        CALL     GetWtitle USING wHdl, wTitle
.
. Parameter:      wHdl     (I)   INTEGER   4  Handle of window to get Title from
.                 wTitle   (O)   DIM max 250  WindowTitle found
.                                             Is terminated with a 0x00 !!!
.
. Exit:           EQUAL    wTitle assigned
.              NOT EQUAL   WinApi failed / no title found      wTitle cleared
.----------------------------------------------------------------------------------------------
. Send a Win-message to a WindowsProcess
.
. CALL syntax:
.
.        CALL     SendWmess USING wHdl, wMsg
.
. Parameter:      wHdl     (I)   INTEGER   4  Handle of window to get Title from
.                 wMsg     (O)   INTEGER   2  Window-Message to Post to the process (handle)
.                                             Default is 16 (=CLOSE)
.
. Exit:           EQUAL    wMsg posted
.              NOT EQUAL   wHdl invalid, no message posted
.
.----------------------------------------------------------------------------------------------
.
ProgDate INIT      "24.Nov 2006 ©OMS"
ProgHead INIT      "Process Monitor v1.1"
         INCLUDE   PLBEQU.INC
         INCLUDE   PLBMETH.INC
..         INCLUDE   FUNCVAR.DEF
*------------------------------------------------------------------------------*
* General purpose constants etc.   PL/B                         1999.11.12/OMS *
*------------------------------------------------------------------------------*
 CIFNDEF   funcDefs
funcDefs EQU      1                 // Prevent multiple inclusions
.
quit     DEFINE   "ESC"             // F-key synonyms
help     DEFINE   "F9"
F27      DEFINE   "ESC"             // RMS-fix
.
. Define color values (for DISPLAY only)
.
$BLACK   DEFINE   "$0"
$BLUE    DEFINE   "$1"
$RED     DEFINE   "$4"
$MAGENTA DEFINE   "$5"
$GREEN   DEFINE   "$2"
$CYAN    DEFINE   "$3"
$YELLOW  DEFINE   "$6"
$WHITE   DEFINE   "$7"
.
.
* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. FIXED DEFINITIONS
.
$0       CONST    "0"
$1       CONST    "1"
$2       CONST    "2"
$3       CONST    "3"
$4       CONST    "4"
$5       CONST    "5"
$6       CONST    "6"
$7       CONST    "7"
$8       CONST    "8"
$9       CONST    "9"

SEQ1     CONST    "-1"     // seq.acc forward from current filecursor
SEQ2     CONST    "-2"     // seq.acc forward from current filecursor
SEQ3     CONST    "-3"     // seq.acc forward from end-of-file
SEQ4     CONST    "-4"     // seq.acc backward from current filecursor

SPACE    INIT     0x20
$tab     INIT     0x09     // Tab character in several object-texts      (e.g. DATALIST)
$nl      INIT     0x7F     // new line indicator in several object-texts (e.g. ALERT)
.
$j       INIT      "J"
$n       INIT      "N"
* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. Junk vars.
.
$a1      DIM       1       // Junk dim 1
$a2      DIM       2       // Junk dim 2
$a3      DIM       3       // Junk dim 3
$a4      DIM       4       // Junk dim 4

$n1      FORM      1       // Junk form 1
$n2      FORM      2       // Junk form 2
$n3      FORM      3       // Junk form 3
$n4      FORM      4       // Junk form 4

$int1    INTEGER   1       // Junk integer 1 (0 - 255)
$int2    INTEGER   2       // Junk integer 2 (0 - 65,535)
$int3    INTEGER   3       // Junk integer 3 (0 - 16,777,215)
$int4    INTEGER   4       // Junk integer 4 (0 - 4,294,967,295)

fSpec    DIM       260     // General purpose (e.g.: a path and/or fileSpec)
 CENDIF
..         INCLUDE   PLBFUNC.FNC
titel    DIM       250    // Search for winTitle with this contents
jobs     DATALIST
mW       MAINWINDOW

GetTopWin PROFILE user32:     // DLL navn
         GetTopWindow:        // API navn
         INT4:                // Result
         INT4                 // Handle

GetWin PROFILE user32:     // DLL navn
         GetWindow:        // API navn
         INT4:             // Result
         INT4:             // Handle
         INT2              // ToDo


GetWTxt PROFILE user32:      // DLL name
         GetWindowTextA:     // API name
         INT4:               // Result
         INT4:               // Handle
         DIM:                // Window Title
         INT2                // Max.size

GetWTxtL PROFILE user32:       // DLL name
         GetWindowTextLengthA: // API name
         INT4:                 // Result
         INT2                  // Current size of Window Title

PostMess PROFILE user32.dll:   // DLL nam,e
         PostMessageA:         // API name
         INT4:                 // Result, True or False
         INT4:                 // Handle of destination
         INT2:                 // Message, e.g. WM_CLOSE
         INT4:                 // Param 1, 0
         INT4                  // Param 2, 0

.DestroyWin PROFILE user32.dll: // DLL name
.         DestroyWindow:        // API navn
.         INT4:                 // Result
.         INT4                  // Handle


Result    INTEGER   4
MSG       INTEGER   2
HDL       INTEGER   4
PARAM1    INTEGER   4,"0"
PARAM2    INTEGER   4,"0"
CLASS     DIM       50
HEX0      INIT      0x00

GW_HNEXT FORM     "2"
GWCMD    INTEGER  2
CURRWIN  INTEGER  4
thisWind INTEGER  4
buf      DIM      600
hWND     FORM     10
titlSize INTEGER  2
answ     DIM      1
hits     FORM     3

dList    DATALIST ^
n9       FORM     9
n8       FORM     8
         GOTO     ProcMon

*-----------------------------------------------------------------------------------------
SENDMESS PROCEDURE WITH thisWind, MSG
         GOTO      ExitNotEqual IF ( thisWind = 0 )
         IF ( NOT MSG )
           MOVE      "16",MSG               // WM_CLOSE
         ENDIF
         WINAPI    PostMess GIVING Result USING thisWind,MSG,PARAM1,PARAM2
         FLAGREST  "0100"
         RETURN

*-----------------------------------------------------------------------------------------
GETWTITL PROCEDURE WITH thisWind, buf
         GOTO      ExitNotEqual IF ( thisWind = 0 )
         CLEAR     hits, $int1
         LOOP
           WINAPI    GetWTxtL GIVING Result USING thisWind
           MOVE      (Result +1), titlSize
           IF ( titlSize > 600 )
             MOVE      "600", titlSize
           ENDIF
           FILL      " ", buf
           WINAPI    GetWTxt GIVING Result USING thisWind,buf,titlSize
           IF ( Result <> 0 )
             SET       hits
             MOVE      $4, $int1
           ELSE
             INCR      $int1
             CALL      Sleeper
           ENDIF
         REPEAT UNTIL ( $int1 > 3 )
         GOTO      ExitProc

.-----------------------------------------------------------------------------------------
FINDJOBS ROUTINE   dList
         CLEAR     thisWind
         WINAPI    GetTopWin GIVING Result USING thisWind
         IF ( NOT Result )
           GETPROP   mW,  HWND=thisWind
         ELSE
           MOVE      Result, thisWind
         ENDIF

         PACK      $a2, HEX0,SPACE
         DELETEITEM dList,0
         CLEAR     $int1                        // Loop counter
         LOOP
           MOVE      thisWind, CURRWIN
           CLEAR     hits
           LOOP
             WINAPI    GetWTxtL GIVING Result USING CURRWIN
             MOVE      (Result +1), titlSize
             IF ( titlSize > 600 )
               MOVE      "600", titlSize
             ENDIF
             FILL      " ", buf
             WINAPI    GetWTxt GIVING Result USING CURRWIN,buf,titlSize
             IF ( Result <> 0 )                                     // A Title found
               INCR      hits
               SETLPTR   buf, titlSize
               REPLACE   $a2, buf
               CHOP      buf, buf
               MOVE      CURRWIN, hWND
               PACK      buf, buf,$tab,hWND
               INSERTITEM dList,9999,buf
             ENDIF
             MOVE      GW_HNEXT, GWCMD
             WINAPI    GetWin GIVING Result USING CURRWIN,GWCMD
             MOVE      Result, CURRWIN
           REPEAT WHILE ( CURRWIN )
           IF ( hits = 0 )
             INCR      $int1                 // Check out windows 3 times without hits
             CALL      Sleeper
           ENDIF
         REPEAT WHILE ( hits = 0 AND $int1 < 3 )
         IF ( hits = 0 )
           FLAGREST  "0000"
         ELSE
           FLAGREST  "0100"
         ENDIF
         RETURN

.-----------------------------------------------------------------------------------------
FINDPROC PROCEDURE WITH titel, buf
         TYPE      titel
         GOTO      errExit IF EOS
         CLEAR     thisWind
         WINAPI    GetTopWin GIVING Result USING thisWind
         IF ( NOT Result )
           GETPROP   mW,  HWND=thisWind
         ELSE
           MOVE      Result, thisWind
         ENDIF

         CLEAR     $int1                        // Loop counter
         LOOP
           MOVE      thisWind, CURRWIN
           CLEAR     hits
           LOOP
             MOVE      GW_HNEXT, GWCMD
             WINAPI    GetWin GIVING Result USING CURRWIN,GWCMD
             MOVE      Result, CURRWIN
            WHILE (CURRWIN <> 0)
             WINAPI    GetWTxtL GIVING Result USING CURRWIN
             MOVE      (Result +1), titlSize
             IF ( titlSize > 600 )
               MOVE      "600", titlSize
             ENDIF
             FILL      " ", buf
             WINAPI    GetWTxt GIVING Result USING CURRWIN,buf,titlSize
             IF ( Result <> 0 )                                     // A Title found
               SCAN      titel, buf
               IF EQUAL                                             // It's a hit
                 SET       hits
                 MOVE      CURRWIN, hWND
                 SCAN      HEX0, buf
                 CMOVE     $tab, buf
                 APPEND    hWND, buf
                 APPEND    HEX0, buf
               ENDIF
             ENDIF
           REPEAT WHILE ( hits = 0 )
           IF ( hits = 0 )
             INCR      $int1                 // Check out windows 3 times without hits
             CALL      Sleeper
           ENDIF
         REPEAT WHILE ( hits = 0 AND $int1 < 3 )
ExitProc
         IF ( hits <> 0 )
           SCAN        HEX0, buf
           BUMP        buf, -1               // Remove 0x00
           LENSET      buf
           RESET       buf
           FLAGREST    "0100"
         ELSE
ExitNotEqual
           CLEAR       buf
           FLAGREST    "0000"                   //  Exit Not Equal
         ENDIF
         RETURN

errExit  FLAGREST   "0010"
         RETURN

.-----------------------------------------------------------------------------------------
title    DIM       80
main     PLFORM    FindProc.PLF
done     FORM      1

ProcMon
         WINHIDE
         CALL      $ProcMon
         STOP
.-----------------------------------------------------------------------------------------
.
$ProcMon PROCEDURE
         CLEAR     done
         FORMLOAD  main
         LOOP
           EVENTWAIT
         REPEAT UNTIL ( done )

ProcMonExit
         DESTROY   ProcMonMain
         SET       done
         RETURN

ProcMonLoad
..         CALL      SysMark USING ProcMonMain, ProgHead,ProgDate
         ProcMonLV.InsertColumn USING "Process Title",   600,0
         ProcMonLV.InsertColumn USING "Handle",           90,1

ProcMonRefresh
         ProcMonLV.DeleteAllItems
         CREATE    jobs=01:01:01:01,SORTED
         CALL      FINDJOBS USING jobs
         jobs.GetCount GIVING n9
         CLEAR     n8
         LOOP
           GETITEM   jobs, (n8 +1),buf
           SCAN      $tab, buf
           BUMP      buf
           MOVE      buf,  hWND
           BUMP      buf, -2
           LENSET    buf
           RESET     buf
           ProcMonLV.InsertItem  USING *Text=buf,*Index=n8
           MOVE      hWND, buf
           ProcMonLV.SetItemText USING *Index=n8,*Text=buf, *SubItem=1
           INCR      n8
         REPEAT UNTIL ( n8 = n9 )
         DESTROY   jobs

         ProcMonLV.GetItemCount GIVING n9
         MOVE      n9, buf
         SETITEM   STitems, 0, buf
         RETURN

ProcMonKill
         MOVE      SEQ1,n8
         ProcMonLV.GetNextItem GIVING n8 USING *Flags=LVNI_SELECTED,*Start=n8
         IF ( n8 >= 0 )
           ProcMonLV.GetItemText GIVING buf USING *Index=n8,*Subitem=1   // hWND
           MOVE      buf, hWND
           CALL      SENDMESS USING hWND
         ENDIF
         GOTO      ProcMonRefresh

.-----------------------------------------------------------------------------------------
. Sorting listView on columnheader clicked.
. SortOrder is determined by sortorder last done.
. If current is NONE or ASCENDING, DESCENDING is used
. If current is DESCENDING, ASCENDING is used
.
curSort  INTEGER  1
#EventResult FORM 11

ProcMonSortLV PROCEDURE WITH #EventResult
         ProcMonLV.GetItemCount GIVING n9
         IF ( n9 )
           SELECT USING curSort
            WHEN   $1
             MOVE    $2, curSort            // Descending
            WHEN   $2
             MOVE    $1, curSort            // Ascending
            DEFAULT
             MOVE    $2, curSort
           ENDSELECT
           ProcMonLV.SortColumn USING *Column=#EventResult, *Type=curSort
         ENDIF
         RETURN

.-----------------------------------------------------------------------------------------
#EventMod    FORM   4
ProcMonTest PROCEDURE WITH #EventMod
         RETURN IF ( #EventMod <> 16 )        // Right click to activate
         WINSHOW
         LOOP
           KEYIN      *HD,*R,"Search for title: ", *RV,title;
          UNTIL ESC
           CALL       FINDPROC USING title, buf
           IF EQUAL
             DISPLAY    " => FOUND":
                        *R,*H=1,*LL,buf;
           ELSEIF LESS
             DISPLAY    " => NO INPUT";
           ELSE
             DISPLAY    " => NOT FOUND";
           ENDIF
         REPEAT
         LOOP
           KEYIN      *HD,*R,"Get our own title: ", *RV,title;
          UNTIL ESC
           GETINFO   SYSTEM, buf
           RESET     buf, 578
           MOVE      buf, thisWind           // Our Windows Handle
           CALL      GETWTITL USING thisWind, buf
           IF EQUAL
             DISPLAY   *R,"Title: ",*LL,buf;
           ELSE
             DISPLAY   "  Error !!";
           ENDIF
         REPEAT
         LOOP
           KEYIN      *HD,*R,"Blow ourselves away ?", *RV,title;
          UNTIL ESC
           GETINFO   SYSTEM, buf
           RESET     buf, 578
           MOVE      buf, thisWind           // Our Windows Handle
           CALL      SENDMESS USING thisWind
         REPEAT
         DISPLAY   *HD,*EL,"Still running !!";
         WINHIDE
         RETURN

.-----------------------------------------------------------------------------------------
SleepPf   PROFILE  kernel32.Dll:              . DLL libary
                   SleepEx:                   . EntryPoint
                   Int4:                      . Result
                   Int4:                      . SleepTime
                   Int4

IntZero  Integer   4,"0"
SleepTime INTEGER  4,"500"                    . Wait timer in Milli Seconds
Sleep    FORM      9
saveFlags DIM      4

Sleeper  ROUTINE   Sleep
         FLAGSAVE  saveFlags
         IF ( Sleep <= 0 )
           MOVE      "500",Sleep
         ENDIF
         MOVE      Sleep, SleepTime
         WINAPI    SleepPf USING SleepTime,IntZero
         FLAGREST  saveFlags
         RETURN

