* FINDPTH5.PLS  A LOADMOD TO LOCATE A PATH
. COPYRIGHT 2000,2001,2003 BY JAMES SEALE, ALL RIGHTS RESERVED.
. 03-19-01 Only expand selected folder
. 4-17-03 USE PLF FOR ICONS RATHER THAN SHELL32.
.
. This code may be modified, used as is, or thrown away. It may be given away.
. It may not be sold.
.
.
.....................................................................................
. Loadmod to locate a path. If a path part is added to the edittext,findpathET1,
. it is appended to the selected path, providing an option for specifying new paths.
. The edittext is off by default or may be turned on or off by the 3rd parameter
. passed in.
.
.....................................................................................
.
GETDRIVES PROFILE   kernel32, GetLogicalDriveStringsA:
                    INT4:     Return, length of returned strings
                    INT4:     Length of destination string
                    DIM       // String to receive results
.         WINAPI GETDRIVES GIVING RET USING I40,DATA
GETDRVTYPE PROFILE  kernel32,GetDriveTypeA, INT4,DIM
.         WINAPI GETDRVTYPE GIVING RET USING DATA
.Find First File
.
. The 'FindFirstFile' function finds the first occurance of a file specified
. by the Filename parameter.  The file properities are placed into the
. second parameter which is a FileData Structure.
.
FindFirst PROFILE  kernel32:           ;DLL name
                   FindFirstFileA:     ;API name ( ASCIIZ string format )
                   INT4:               ;File Handle return value
                   DIM:                ;Search File Name ( ASCIIZ required )
                   DIM                 ;FileData structure
*.............................................................................
.
.Find Next File
.
. The 'FindNextFile' function finds the next occurance of a file after a
. 'FindFirstFile' has succeeded.  The return value will be non-zero when
. 'FindNextFile' locates another file.  The return value will be zero
. when no more files are found.  When a file is found, the second parameter
. will contain the FileData structure information for the file found.
. The first parameter of the function is the File Handle value returned
. from a successful 'FindFirstFile' operation.
.
FindNext  PROFILE         kernel32, FindNextFileA, INT1, INT4, DIM
*..............................................................................
.
.Find Close
.
. The 'FindClose' function closes the File Handle returned from a
. successful 'FindFirstFile' function.
.
FindClose PROFILE         kernel32, FindClose, INT1, INT4
.
*...........................................................................
*
RET       INTEGER   4
I40       INTEGER   4
I41       INTEGER   4
HEX0      INIT      0x00
.
SEQ       FORM      "-1"
SQ3       FORM      "-3"
DATA      DIM       255
P_ETFLAG  FORM      ^
ETFLAG    FORM      1                   // 1 TO USE ET
PATH1     DIM       80
FILENAME  DIM       50
FileData  DIM       400                     ;Name is at offset 44 + 260
FileAttr  INTEGER   4                       ;File Attributes
Extra     DIM       40                      ;
LongName  DIM       260                     ;
ShortName DIM       14                      ;
FILECOUNT FORM      5
ERRCT     FORM      5
CTR       FORM      5
NDX       FORM      5
LVNDX     FORM      6
BASEPATH  DIM       200
.
DRIVE     DIM       20
Name      DIM       270
Ans       DIM       1
SmName    DIM       80
Term      INIT      0x0
$Click    CONST     "4"
$LostFocus CONST    "11"
$GotFocus CONST     "9"
.
FHANDLE   INTEGER   4                         ;File Handle
RESULT1   INTEGER   1                         ;Boolean result
RESULT    FORM      12
NOTE      DIM       255
LINE      FORM      "  3"
N10       FORM      10
MinusOne  form      "4294967295"        ; =0xffffffff
.
TVI_ROOT  INTEGER   4,"0xFFFF0000"
TVI_1ST   INTEGER   4,"0xFFFF0001"
TVI_LAST  INTEGER   4,"0xFFFF0002"
TVI_SORT  INTEGER   4,"0xFFFF0003"
TVE_TOGGLE          INTEGER   2,"0x0003"
TVE_COLLAPSE        INTEGER   2,"0x0001"
TVE_EXPAND          INTEGER   2,"0x0002"
TVI_CHILD INTEGER   1,"4"
TOGGL               INTEGER   2

MyRoot    INTEGER   4
Images    ImageList
NEWROOT   INTEGER   4(1000)
CURROOT   INTEGER   4(1000)
THISROOT  INTEGER   4

CurIndex  FORM      5
Data1     DIM       10
RESNAME   DIM       50
.
LOADED    FORM      1
INITRET   FORM      5
RETCOUNT  FORM      5
p_DATAOUT DIM       ^
P_TITLE   DIM       ^
FRM1      PLFORM    FINDPTH3.PLF
ICONS     PLFORM    FINDICONS
DRIVE_SW  FORM      1
SAVTITLE  DIM       50
WINTYPE   FORM      3
.....................................................................
FINDPATH  ROUTINE   P_DATAOUT, P_TITLE,P_ETFLAG
          RETCOUNT  INITRET
          CALL      CREATE1 IF ( NOT LOADED )
          IF        OVER
            CLEAR   P_DATAOUT
            SETMODE *MCURSOR=*ARROW
            RETURN
          ENDIF
.
          SETMODE   *MCURSOR=*ARROW
          CLEAR       ETFLAG    // SET DEFAULT
          MOVEPTR   P_TITLE,P_TITLE
          IF        NOT OVER
            TYPE    P_TITLE
            IF      EQUAL
              SETPROP findpathW,TITLE=SAVTITLE
              MOVE  P_TITLE,ETFLAG
            ELSE
              SETPROP findpathW,TITLE=P_TITLE
            ENDIF
          ELSE
            SETPROP findpathW,TITLE=SAVTITLE
          ENDIF
          MOVEPTR   P_ETFLAG,P_ETFLAG
          IF NOT OVER
            MOVE P_ETFLAG,ETFLAG
          ENDIF
          IF        ( ETFLAG )
            SETPROP findpathET1,VISIBLE=1
            SETPROP findpathST1,VISIBLE=1
          ELSE
            SETPROP findpathET1,VISIBLE=0
            SETPROP findpathST1,VISIBLE=0
          ENDIF
.
          MOVE      P_DATAOUT,SMNAME
          GETPROP   findpathW,WINTYPE=WINTYPE
.
. Starting directory is required.
.
  DEBUG
          COUNT     RESULT,SMNAME
          GOTO      DRIVE_OK IF ( RESULT )
          MOVE      "COMPUTER",SMNAME
          SET       DRIVE_SW
          CALL      GET_DRIVES
          EVENTREG  findpathTV,$Click,DRIVE_CLICKED
          SETPROP   findpathW,VISIBLE=1
          SETMODE   *MCURSOR=*ARROW
          GOTO      EXIT_NOW IF ( WINTYPE=1 )     // MODAL
          LOOP
            WAITEVENT
          REPEAT
.
.
........................................................................
.
DRIVE_OK
          CLEAR     DRIVE_SW
          CALL      BEGIN_LOCATION
          SETPROP   findpathW,VISIBLE=1
          SETMODE *MCURSOR=*ARROW
          GOTO    EXIT_NOW IF ( WINTYPE=1 )     // MODAL
          LOOP
            WAITEVENT
          REPEAT
..
DRIVE_CLICKED
          EVENTREG  findpathTV,$Click
          findpathTV.GetNextItem GIVING THISROOT USING 0,9
          CLEAR DATA
          findpathTV.GetItemText GIVING DATA USING THISROOT
          MOVE      DATA,SMNAME
          GOTO      DRIVE_OK
..
ACCEPT_FINDPATH
          CALL      GET_findpathTV_SEL
          RETURN    IF OVER
          ENDSET    DATA
          MATCH     "\",DATA
          IF NOT EQUAL
            APPEND  "\",DATA
          ENDIF
          GETITEM   findpathET1,0,NOTE
          COUNT     RESULT,NOTE
          IF        ( RESULT )
            APPEND  NOTE,DATA
          ENDIF
          RESET     DATA
          MOVE      DATA,p_DATAOUT
          SETPROP   findpathW,VISIBLE=0
          GOTO      EXIT_NOW
EXIT_FINDPATH
          CLEAR     p_DATAOUT
          SETPROP   findpathW,VISIBLE=0
EXIT_NOW
          EVENTREG  findpathTV,$Click
.
. Standard exit procedure for EXTERNally called program
.
          LOOP
            RETCOUNT RETCOUNT
          BREAK IF ( RETCOUNT <= INITRET )
            NORETURN
          REPEAT
          SETMODE *MCURSOR=*ARROW
          RETURN

.
*
. Since the function 'FindFirstFileA' defines that string parameters
. must be terminated with a binary zero, we are appending a binary
. zero to the name keyed by the user.
.
BEGIN_LOCATION
          EVENTREG  findpathTV,$Click
          findpathTV.DeleteAll
          findpathLV.DeleteAllItems
          ENDSET    SMNAME
          LOOP
            MATCH   "\",SMNAME
            IF      EQUAL
              LENSET SMNAME
              RESET SMNAME
              MOVE SMNAME,PATH1
              CLEAR FILENAME
              ENDSET SMNAME
              BUMP  SMNAME
              BREAK IF EOS
              SETLPTR SMNAME
              MOVE  SMNAME,FILENAME
              BREAK
            ENDIF
            SETFLAG OVER
            BREAK
          REPEAT
          IF        OVER
            ALERT NOTE,"FILE SPEC ERROR, MUST SPECIFY A PATH...",RESULT
            CLEAR   p_DATAOUT
            CALL    EXIT_FINDPATH
            RETURN
          ENDIF
          COUNT     RESULT,FILENAME
          IF        ZERO
            PACK    SMNAME WITH PATH1,"*.*"
          ELSE
            PACK    SMNAME WITH PATH1,FILENAME
          ENDIF
          SCAN      "?:\",PATH1,"?"
          IF        EQUAL
            RESET   PATH1
            COUNT   RESULT,PATH1
            IF      ( RESULT = 3 )
              CLEAR RESULT
            ELSE
              MOVE  "1",RESULT
            ENDIF
          ELSE
            MOVE    "1",RESULT
          ENDIF
          CLEAR     LVNDX
          findpathTV.InsertItem Giving MyRoot Using PATH1, TVI_ROOT:
                                  TVI_1ST, *Image=RESULT:
                                  *SelImage=RESULT, *param=LVNDX
          findpathLV.InsertItem USING PATH1,LVNDX,*PARAM=MYROOT
          MOVE      PATH1,BASEPATH
          MOVE      MYROOT,THISROOT
          CLEAR     CTR
          MOVE      SEQ,NEWROOT
          CLEAR     NDX
          CALL      FIND_PATHS
          CALL      TV_EXPAND
          SETPROP   findpathTV,VISIBLE=0,VISIBLE=1
          EVENTREG  findpathTV,$Click,GET_findpathTV_SEL
          RETURN
..
GET_findpathTV_SEL
          findpathTV.GetNextItem GIVING THISROOT USING 0,9
          IF        ( THISROOT < 1 )
            ALERT NOTE,"Select a Folder...",RESULT,"Path Selection"
            setflag over
            RETURN
          ENDIF
          IF        ( DRIVE_SW )
            findpathTV.GetItemText GIVING DATA USING THISROOT
            RETURN
          ENDIF
          findpathTV.GetItemParam GIVING RESULT USING THISROOT
          findpathLV.GetItemText GIVING DATA USING RESULT,0
          MOVE  DATA,BASEPATH
          PACK  SMNAME,DATA,"\*.*"
.
. See if already been here, ie, it has a child item.
.
          findpathTV.GetNextItem GIVING RET USING THISROOT, TVI_CHILD
          RETURN    IF ( RET ) // CHILD FOUND, ALREADY BEEN HERE
          CALL  FIND_PATHS
          CALL      TV_EXPAND
          SETPROP   findpathTV,VISIBLE=0,VISIBLE=1
          RETURN
..
TV_EXPAND
          findpathTV.Expand GIVING RESULT USING *Item=THISROOT,*Code=TVE_EXPAND
          RETURN
..
*
FIND_PATHS
. Execute the Windows Api function to find the first occurance of
. the file specification supplied by the user.
.
          PACK      Name USING SmName,Term
          WINAPI    FindFirst GIVING FHANDLE USING NAME,FileData
*
. If the return value identified as FHANDLE is zero or -1, then the file
. was not found.  In this case, this program simply exits.
.
          MOVE      FHANDLE,N10
          IF        ( FHANDLE = MinusOne )
.            PACK      NOTE WITH "Name not found...",N10
.            ALERT     STOP,NOTE,RESULT,"No File..."
            SETFLAG OVER
            RETURN
          ENDIF
.
*
. If we  get to this point, a file was found and the FHANDLE variable
. contains a file handle to be used in the 'FindNextFile' call later.
. The variable 'FileData' was modified to contain the names and properties
. of the file which was found. Note that we are using an UNPACK to
. get the individual members of the 'FileData' structure.
.
          UNPACK    FileData INTO FileAttr:     ;File attributes
                                     Extra:     ;Not used here
                                  LongName:     ;Long file name found
                                  ShortName     ;Short file name

*
. The FileAttr value found in the FileData structure identifies the
. type of file found.
.
          CALL      ShowName               ;Identify file type
*
. Now we want to loop and find all subsequent instances of the file name using
. the FindNext WINAPI function.
.
          LOOP
            WINAPI      FindNext GIVING RESULT1 USING FHANDLE,FileData
          UNTIL       (RESULT1 = 0)
            UNPACK      FileData INTO FileAttr,Extra,LongName,ShortName
            CALL        ShowName                ;Identify File Type
          REPEAT
*
. Execute the function 'FindClose' to close the file handle provided
. by the 'FindFirstFile' function.
.
          WINAPI    FindClose GIVING RESULT1 USING FHANDLE
.  WEOF FPLOG,SEQ
          RETURN
.          STOP
..
*.........................................................................
. This subroutine determines the file type based on the value found
. in the FileAttr variable.
.
SHIFTMASK INTEGER   4,"0xFF"
ShowName
.
. The LongName is an ASCIIZ terminated string.
. We want to find the end of the LongName and set the length pointer
. to the character before the binary zero termination character.
.
          SCAN      TERM,LongName
          BUMP      LongName, -1
          LENSET    LongName
          RESET     LongName
*
. Use the FileData structure FileAttr member to determine the type
. of file being used.
.
. FileAttr Values:
.    128 = Normal File
.     32 = Normal File with Archive Bit Set
.     16 = Directory
.
  DEBUG
          AND       SHIFTMASK,FILEATTR
          IF        (FileAttr < 32 ) // 16)
.  DEBUG
            MATCH ".",LONGNAME
            RETURN IF EQUAL
            INCR    NDX
            INCR    LVNDX
            findpathTV.InsertItem GIVING NEWROOT(NDX) Using  LONGNAME:
                    THISROOT, TVI_SORT, *Image=1, *SelImage=2:
                    *Param=LVNDX
            PACK    DATA WITH BASEPATH,LONGNAME,"\"
            findpathLV.InsertItem USING DATA, LVNDX, *Param=NEWROOT(NDX)
.  WRITE FPLOG,SQ3;*LL,DATA
          ENDIF
.
          RETURN
..
GET_DRIVES
          findpathTV.DeleteAll
          MOVEPLEN  DATA,I40
          WINAPI GETDRIVES GIVING RET USING I40,DATA
          RETURN    IF ( RET < 1)
          SETLPTR   DATA,RET
          CLEAR     LVNDX
          MOVE      "COMPUTER",PATH1
          findpathTV.InsertItem Giving MyRoot Using PATH1, TVI_ROOT:
                                  TVI_1ST, *Image=3:
                                  *SelImage=3
          CLEAR NDX
          MOVE      MyRoot,THISROOT
          LOOP
            MOVEFPTR DATA,RESULT
            SCAN    HEX0,DATA
            BREAK   IF NOT EQUAL
            BUMP    DATA, -1
            LENSET  DATA
            RESET   DATA,RESULT
            PACK    DRIVE WITH DATA,HEX0
            WINAPI  GETDRVTYPE GIVING I40 USING DRIVE
            SWITCH  I40
            CASE    "3"       // ICON 0, DISK
            INCR    NDX
            findpathTV.InsertItem GIVING NEWROOT(NDX) Using  DATA:
                    THISROOT, TVI_SORT, *Image=0, *SelImage=0
            CASE    "4"       // ICON 0, DISK
            INCR    NDX
            findpathTV.InsertItem GIVING NEWROOT(NDX) Using  DATA:
                    THISROOT, TVI_SORT, *Image=0, *SelImage=0
            CASE    "5"       // ICON 5, CD
            INCR    NDX
            findpathTV.InsertItem GIVING NEWROOT(NDX) Using  DATA:
                    THISROOT, TVI_SORT, *Image=5, *SelImage=5
            ENDSWITCH
            ENDSET  DATA
            SETLPTR DATA,RET
            BUMP    DATA,2
            BREAK   IF EOS
          REPEAT
          findpathTV.Expand GIVING RESULT USING *Item=THISROOT:
                              *Code=TVE_EXPAND
          RETURN
..
GETSYSDIR PROFILE   kernel32,GetSystemDirectoryA,INT4,DIM,INT4
CREATE1
          FORMLOAD  FRM1
          GETPROP   findpathW,TITLE=SAVTITLE
          CREATE    Images,ImageSizeH=16,ImageSizeV=16
          FORMLOAD  ICONS
.
          Images.AddIcon Using 1  //8 // Image 0,DISK DRIVE
          Images.AddIcon Using 2 //4 // Image 1, CLOSED FOLDER
          Images.AddIcon Using 3 //5 // Image 2, OPEN FOLDER
          Images.AddIcon Using 4 //16 // Image 3, COMPUTER
          Images.AddIcon Using 5 //6 // Image 4, FLOPPY
          Images.AddIcon Using 6 //12 // Image 5, CD

.
. Attach the ImageList to the TreeView
.
          findpathTV.SetImageList Using Images
          SET       LOADED
          RETURN
..
NORESLIB
          PACK      DATA WITH "MISSING ",RESNAME
          ALERT STOP,DATA,RESULT
          CALL    EXIT_FINDPATH
          RETURN
..............................................