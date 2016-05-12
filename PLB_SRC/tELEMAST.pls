...............................................................................
.
.          NAMES IN THE NEWS   TELECOMMUNICATION FILE MAINT MASTER PROGRAM
.
...............................................................................
.
. COMMON AREA.   THIS AREA GETS OVERWRITTEN WITH AN ELEVEN-BYTE CHARACTER
.                STRING VARIABLE WHEN AN ERROR OCCURS.
.                THE COMMON VARIABLE 'ERROR' USES THE SAME NUMBER OF BYTES
.                OF UDA AS THE COMMON VARIABLES 'PORTN' AND 'TODAY'
.
. >>>>>>>>>>>>>>>>>>>>>>THIS AREA NEEDS TO BE CORRECTED.
         INCLUDE   COMMON.inc
TOTLIN   FORM      2
MODE     DIM       1
CONT     DIM       1
WORKNAME DIM       8
CORP     DIM       1
HOLDTE   DIM       8
HOLDPASS DIM       10
.
.
.CLOCK    FILE
DIRECT   FILE
DNAME    DIM       25
DNUM     DIM       2
H        FORM      2
V        FORM      2
SEQ2     FORM      "-2"
PROGNAME DIM       8
CWK2     DIM       2
CWK11    INIT      "           "
ANS      DIM       1
INDEX    FORM      2
PASS     DIM       4
TIME     INIT      "HH:MM:SS"
HOUR     FORM      2
PER      DIM       3
TIMEDISP DIM       8
DAY      DIM       9
MO       DIM       2
DY       DIM       2
YR       DIM       2
STR2     DIM       2
LOGTIME  DIM       18
DATE     DIM       6
MM       DIM       2
DD       DIM       2
YY       DIM       2
HR       DIM       2
MIN      DIM       2
SEC      DIM       2
WKDAY    FORM      1
SLASH    INIT      "/"
COLON    INIT      ":"
ONSW     DIM       1
.         INC       INCLUDE/LIB.LOGDATA
.
...............................................................................
.
. SEE IF THERE ARE ANY UNTRAPPED DATASHARE ERRORS.
. IF NO ERROR OCCURED, THE VARIABLES 'PORTN' AND 'TODAY' WILL STILL BE IN
. THE FIRST TWELVE BYTES OF UDA.  IN THIS CASE, THE 9TH CHARACTER OF CWK11
. WILL BE A BLANK.
.
. IF AN ERROR OCCURED, THE ELEVEN BYTE ERROR MESSAGE WILL BE MOVED INTO CWK11
. IN THIS CASE THE 9TH CHARACTER OF CWK11 WILL BE A '*'.
.
.         INC       INCLUDE/LIB.LOGIO
PAGE1    DISPLAY   024:
                   *ES,*HON,"|--------------------------------------------":
                       "--------------------------------------------|":
                   *P1:2,"|",*P80:2,"|",*P1:3,"|",*P80:3,"|",*P1:4,"|":
                   *P80:4,"|":
                  *P1:5,"|-------------------------------------------":
                        "-------------------------------------------|":
                   *P13:2,*HON,"N A M E S   I N   T H E   N E W S    ":
                          "C A L I F O R N I A":
                   *P20:4,*HOFF,"M A S T E R   T E L E C O M M   M E N U":
                   *P01:07," 1...DIVOKY T/C MAINT.(not active)":
                   *P01:08," 2...CRAVER (EAST) T/C MAINT(not active ":
                   *P01:09," 3...TRIPLEX TELECOMM MAINT.":
                   *P01:10," 4...EPSILON MERGE ADDITION.":
                   *P01:11," 5...":
                   *P62:21,"Time on: ",TIMEDISP:
                   *P01:17,"(Enter for MASTER MENU)",*HOFF:
                   *P42:17,"(F5 for Time update)":
                   *P62:22,"Port   : ",CWK2:
                   *P62:23,"Day    : ",DAY;
         GOTO      GETINDEX
.
*................................................................
. GET THE PROGRAM'S INDEX
.
GETINDEX MOVE      "*" TO ONSW        *ALL READY LOGGED ON
         KEYIN     *P62:20,*EL:
                   *P1:24,*EL,"Please enter your selection by number: __":
                   *P62:24,"Date   : ",*DV,TODAY:
                   *P40:24,INDEX;
         COMPARE   "1" TO INDEX
         STOP      IF LESS
INDEX
         COMPARE   "05" TO INDEX
         GOTO      GETINDEX IF NOT LESS
         GOTO      GETPROG
NONAME   NORETURN
         DISPLAY   *P60:24,*HON,"*** NO SUCH PROGRAM ***",*W,*P60:24,*EL;
         GOTO      GETINDEX
.................................................................
. BRANCH TO THE ROUTINE INDICATED BY THE INDEX
.
GETPROG  TRAP      NOTFOUND IF CFAIL
         BRANCH    INDEX OF NDIV0001:     # 1
                            CMSRVIEW:     # 2
                            NORD0023:      # 3
                            nmrg0005       #4
         GOTO      GETINDEX
*................................................................
CMSRVIEW
.         CALL      LOGWRITE
         CHAIN     "CMSRVIEW"
.         CALL      LOGWRITE
NDIV0001 CHAIN     "NDIV0001"
.         CALL      LOGWRITE
NORD0023  CHAIN    "NORD0023"
.         CALL      LOGWRITE
nmrg0005  CHAIN     "Nmrg0005"
.
NOTFOUND NORETURN
         DISPLAY   *B,*P45:24,*HON:
                   *B,*P45:24,"Sorry, this program is not on-line.",*W2,*HOFF;
.         CALL      LOGWRITE
         GOTO      GETINDEX

