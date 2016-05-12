PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NDATDD.inc
release  init      "2.5"        ASH 30JAN2004 DATACARD CONVERSION
.release  init      "2.4"        ASH 02OCT2000 NEW SERVER ADDED
.release  init      "2.3"        JD  16mar97 unpack revdate fixed.
.release  init      "2.2"       dlh 17mar94 suppress (office use only) cards.
.RELEASE  INIT      "2.1"       DLH 16MAR94    OPTIONS
.RELEASE  INIT      "R2.0"      DLH 12MARH92   ALL NEW INCLUDES ETC.
....................................................................
OUTPUT   FILE      
....................................................................
ELIMLIST IFILE     KEYLEN=6
. HOLDS LIST NUMBER AND DESCRIPTION
.....................................................................
ELIMOWN  IFILE     KEYLEN=4
.ELIMOWN FILE.
..............
EOWNKEY   DIM       4    1-4      KEY.
EUPE1    DIM       1     5-5
ELIMDES  DIM       30    6-35    DESCRIPTION
....................................................................
DUPEOWN  IFILE     KEYLEN=4
DOWNKEY   DIM       4    1-4      KEY.
DUPE1    DIM       1     5-5
NEWOLON  DIM       4     6-9  OWNER NUMBER TO BE USED FROM DUPEOWN FILE.
DUPE2    DIM       1    10-10
DUPEDES  DIM       30   11-40    DESCRIPTION
............................................................
QTY      FORM      5
MASK     INIT      "Z,ZZZ,ZZZ,ZZ9"
. 
...............................................................................
.misc VARIABLES
akey2    init       "02R"
sysmo    dim       2           .used to
sysday   dim       2           .hold the
sysyr    dim       2           .system date
DATMO    DIM       2           .used to 
DATDAY   DIM       2           .hold the
DATYR    DIM       2           .datacard revised date.
ORDDATE  FORM      5           .order date in julian
CARDATE  FORM      5           .datacard date in julian
date     DIM       8
today1   FORM      5 
date1    dim       8
CHECK    FORM      5
check2   form      5
BLNK3    DIM       3
COUNT    FORM      5
count1   form      5
count2   form      5
USED     FORM      5
NOTUSED  FORM      5
OPTFLAG  FORM      1
....................................................................
         MOVE      "Ndat00??" TO PROGRAM
         MOVE      "DATACARDS NEEDING UPDATES" TO STITLE
         MOVE      "Names In The News Ca." TO COMPNME
         MOVE      C1 TO NDATPATH  .SET ACCESS TO ISAM
         MOVE      C2 TO NORDPATH
         clock     date to today
         CALL      PAINT
         MOVE      "ABORT" TO PF5
         CALL      FUNCDISP
         display   *p1:24,*el,"opening dupe own";
.START PATCH 2.4 REPLACED LOGIC
.         OPEN      DUPEOWN,"g:\data\index\DUPEOWN",READ
.         display   *p1:24,*el,"opening elim own";
.         OPEN      ELIMOWN,"g:\data\index\ELIMOWN",READ
.         display   *p1:24,*el,"opening elim list";
.         OPEN      ELIMLIST,"g:\data\index\ELIMLIST",READ
.
         PACK      STR35,NTWKPATH1,"INDEX\DUPEOWN"
         PACK      STR45,NTWKPATH1,"INDEX\ELIMOWN"
         PACK      STR50,NTWKPATH1,"INDEX\ELIMLIST"
         OPEN      DUPEOWN,STR35,READ
         display   *p1:24,*el,"opening elim own";
         OPEN      ELIMOWN,STR45,READ
         display   *p1:24,*el,"opening elim list";
         OPEN      ELIMLIST,STR50,READ
.END PATCH 2.4 REPLACED LOGIC
         display   *p1:24,*el;
. 
         TRAP      ABORT IF F5
. 
. 
....................................................................
         CLOCK     DATE TO DATE
         UNPACK    DATE INTO SYSMO,str1,SYSDAY,str1,SYSYR
         REP       zfill,SYSDAY
         REP       zfill,SYSMO
         MOVE      SYSMO TO MM
         MOVE      SYSDAY TO DD
         MOVE      SYSYR TO YY
         CALL      CVTJUL
         MOVE      juldays TO TODAY1
. 
....................................................................
WHAT     MOVE      "D" TO STR1
         KEYIN     *P10:12,*EL,"(D)efault OR (R)eclean ?",*T60,*RV,STR1
         REP       "D1R2" IN STR1
         MOVE      C0 TO OPTFLAG
         MOVE      STR1 TO OPTFLAG
         BRANCH    OPTFLAG TO BEGIN,RECLEAN
         GOTO      WHAT
....................................................................
BEGIN    IFZ       PC
.START PATCH 2.4 REPLACED LOGIC
.         PREPARE   OUTPUT,"g:\DATA\CARDexcl.CAL"
         PACK      STR35,NTWKPATH1,"CARDexcl.CAL"
         PREPARE   OUTPUT,STR35
.END PATCH 2.4 REPLACED LOGIC
         xif
         call       ndattst
         GOTO      A100
RECLEAN  display   *savesw 1
         goto       recdat
         display   *p1:24,"copying cardupd.cal to cardupd.dat"
.         EXECUTE   "c:\command.com /c copy g:\DATA\cardupd.cal g:\data\cardupd.dat"
.         EXECUTE   "ncopy g:\DATA\CARDUPD.CAL g:\data\CARDUPD.DAT"
         IF        OVER
         DISPLAY   *P1:24,*EL,*RED,"copy FAILED - JOB ABORTED",*B,*B,*B,*W4
         STOP
         ENDIF
         display   *restsw 1
         display   *p1:24,"copy OK, let's index the file!"
.START PATCH 2.4 REPLACED LOGIC
.         EXECUTE   "INDEX g:\DATA\CARDUPD.DAT g:\DATA\CARDUPD /2-7,S,C"
         PACK      TASKNAME,"INDEX ",NTWKPATH1,"CARDUPD.DAT ",NTWKPATH1,"CARDUPD /2-7,S,C"
         EXECUTE   TASKNAME
.END PATCH 2.4 REPLACED LOGIC
         IF        OVER
         DISPLAY   *P1:24,*EL,*RED,"INDEX FAILED - JOB ABORTED",*B,*B,*B,*W4
         STOP
         ENDIF
         display   *p1:24,"index ok lets begin",*w
         display   *restsw 1
RECDAT   MOVE      "CARDUPD" TO NDATNAME
         OPEN      NDATFILE,NDATNAME
         MOVE      C1 TO NDATFLAG
.START PATCH 2.4 REPLACED LOGIC
.         PREPARE   OUTPUT,"g:\DATA\CARDUPD.CAL"
         PACK      STR35,NTWKPATH1,"CARDUPD.CAL"
         PREPARE   OUTPUT,STR35
.END PATCH 2.4 REPLACED LOGIC
         GOTO      A100
....................................................................
. 
. GET NEXT DATA CARD
. 
A100
         CALL      NDATKS
         GOTO      Z900 IF OVER
. 
         DISPLAY   *P1:8,"WORKING ON LIST ",LSTNUM," - ",MLSTNAME
         add       c1 to count
        display   *p10:13,"records read ",count
. 
CHKRUN   RESET     RUNCODES
         SCAN      lstnum IN RUNCODES         .running charge only list?
         GOTO      A100 IF EQUAL              .yes skip
.
         CMATCH    "W"  TO status        .withdrawn ?
         GOTO      A100 IF EQUAL         .yes skip
         cmatch    "T" to status
         goto      a100 if equal         .temp with. skip per smm 12/3
         cmatch    "C" to elstcde        .exclusive ?
         goto      a100 if not equal         .yes, skip
         scan      "OFFICE USE" in mlstname
         goto      a100 if equal
         reset     mlstname
         scan      "Office use" in mlstname
         goto      a100 if equal
         reset     mlstname
         scan      "Office Use" in mlstname
         goto      a100 if equal
.START PATCH 2.5 REPLACED LOGIC
.         MOVE      OWNNUM TO EOWNKEY
	UNPACK	OWNNUM,STR2,EOWNKEY
.END PATCH 2.5 REPLACED LOGIC
         rep       zfill in eownkey
.         READ      ELIMOWN,EOWNKEY;;         .ELIMINATE OWNER?
.         GOTO      A100 IF NOT OVER          .YES
         READ      ELIMLIST,LSTNUM;;         .ELIMINATE LIST?
         GOTO      A100 IF NOT OVER          .YES
.
CHECK   
.         MOVE      REVDATE TO DATE1
.START PATCH 2.5 REPLACED LOGIC
.         UNPACK    revdate INTO DATMO,SLASH,DATDAY,SLASH,cc,DATYR
         UNPACK    revdate,cc,DATYR,DATMO,DATDAY
.END PATCH 2.5 REPLACED LOGIC
         REP       zfill,DATDAY
         REP       zfill,DATMO
         MOVE      DATMO TO MM
         MOVE      DATDAY TO DD
         MOVE      DATYR TO YY
         CALL      CVTJUL
         MOVE      juldays TO CARDATE
         move      today1 to check
         SUB       cardate FROM CHECK
         compare   "90" to check
         GOTO      INFO IF not LESS
         GOTO      a100

INFO     add       c1 to count1
         display   *p10:14,"not updated last 3 mos",count1
.         BRANCH   OPTFLAG TO ORDINFO,OWNPREP
.ORDINFO  REP       zfill IN LSTNUM
.         PACK      NORDFLD2,AKEY2,LSTNUM
.         CLEAR     NORDFLD1
.         CLEAR     NORDFLD3
.         CLEAR     NORDFLD4
. 
.         display   *p1:24,*el,*cyan,"S E A R C H I N G ",*white
.         CALL      NORDAIM
.         GOTO      CARDNG IF OVER
.         MOVE      OODTEM TO MM
.         MOVE      OODTED TO DD
.         MOVE      OODTEY TO YY
.         CALL      CVTJUL
.         MOVE      juldays TO ORDDATE
.         MOVE      ORDDATE TO CHECK
.         move      today1 to check2
.         SUB       check FROM CHECK2
.         compare   "731" to check2           usage in last 2 yrs?
.         IF        LESS
.         GOTO      B101          .IT COUNTS
.         ELSE
.         GOTO      B100         .BIG DEAL, TRY AGAIN.
.         ENDIF
.CARDNG   DISPLAY   *P1:24,*EL,"LIST ",MLSTNAME:
.                   *P1:23,*EL,"LIST NUMBER : ",LSTNUM," NOT USED "
.         ADD       c1 TO NOTUSED
.         DISPLAY   *P12:20,"NUMBER OF LISTS WITH NO USAGE : ",NOTUSED
.         GOTO      A100
. 
. COMPUTE ORDER USAGE
. 
.B100
.        display   *p1:24,*el,*cyan,"S E A R C H I N G ",*white
.         CALL      NORDKG
.         GOTO      C100 IF OVER
.         MOVE      OODTEM TO MM
. ..        MOVE      OODTED TO DD
.         MOVE      OODTEY TO YY
.         CALL      CVTJUL
.         MOVE      juldays TO ORDDATE
.         MOVE      ORDDATE TO CHECK
.         move      today1 to check2
.         SUB       check FROM CHECK2
.         compare   "731" to check2
.         IF        LESS
.         GOTO      B101             .ITs GOOD
.         ELSE
.         GOTO      B100            .DATE OUT OF RANGE
.         ENDIF
.B101     ADD       C1,QTY
.         display   *p10:19,"b101 uses = ",qty
.         COMPARE   C4 TO QTY
.         GOTO      C100 IF EQUAL
.         GOTO      B100
. 
. 
. WRITE SUMMARY RECORD IKF USAGE <> 0
.  
C100
.         COMPARE   C4,QTY
.         GOTO      C101 IF less
. 
.
OWNPREP
.START PATCH 2.5 REPLACED LOGIC
.         MOVE      OWNNUM TO DOWNKEY
	UNPACK	OWNNUM,STR2,DOWNKEY
.START PATCH 2.5 REPLACED LOGIC
         REP       zfill IN DOWNKEY
         READ      DUPEOWN,DOWNKEY;DOWNKEY,DUPE1,NEWOLON
         IF        NOT OVER
.START PATCH 2.5 REPLACED LOGIC
.         MOVE      NEWOLON TO OWNNUM
	PACK	OWNNUM,"00",NEWOLON 
.END PATCH 2.5 REPLACED LOGIC
         ENDIF
.
         write     output,seq;*+,datvars 
         ADD       c1 TO USED
         DISPLAY   *P12:18,"NUMBER OF LISTS WITH USAGE ",USED
         move      c0 to qty
         goto       a100
. 
C101
.        add       c1 to count2
.        display   *p10:15,"used but not 4 times",count2
.         MOVE      c0,QTY
. 
. GO DO IT AGAIN
. 
D100
         GOTO      A100
. 
. 
. ABORT - OPERATOR ABORTED JOB. RESULTS NOT VALID
. 
ABORT
         DISPLAY   *P1:24,*EL,*B,*B,"JOB ABORTED, RESULTS NOT VALID",*W5
. 
. CLOSE FILE AND EXIT
. 
Z900
         IFNZ      PC
         FLUSH     OUTPUT
         XIF
         WEOF      OUTPUT,SEQ
         CLOSE     OUTPUT
. 
        shutdown   "cls"
        STOP
. 
         INCLUDE   NORDIO.inc
         INCLUDE   NDATIO.inc
         INCLUDE   COMLOGIC.inc

