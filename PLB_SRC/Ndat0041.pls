PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NDATDD.inc
release  init      "2.81"        DLH .removed old references to ElimList & ElimOwner, added exfee list exclusion
.eliminate "reclean" code not longer used, speed up by reading only managed files, and use data manager
Reldate   Init      "2014 April 15"
.release  init      "2.8"        DLH cleaned up added to saturday batch job - 
.Reldate   Init      "26 May 2010"
.release  init      "2.7"        DLH 14Feb2005  After discussion with SMM changed logic from
.                     revdate to NDATUPDDATE & at least temp turned back to 90 days
.
.release  init      "2.6"        DM 04NOV2004  CHANGED DAYS TO SEARCH FROM 90 TO 180
.release  init      "2.5"        ASH 30JAN2004  DATACARD CONVERSION
.release  init      "2.4"        ASH 02OCT2000 NEW SERVER ADDED
.release  init      "2.3"        JD  16mar97 unpack revdate fixed.
.release  init      "2.2"       dlh 17mar94 suppress (office use only) cards.
.RELEASE  INIT      "2.1"       DLH 16MAR94    OPTIONS
.RELEASE  INIT      "R2.0"      DLH 12MARH92   ALL NEW INCLUDES ETC.
.Find Managed files that are not withdrawn, have not been updated in the last 90 days, and have at least one live order in the last 365 days, not marked office use only,\
....................................................................
OUTPUT   FILE      
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
Dim11a    Dim       11
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

ORDS      FORM      9
mask9      init    "ZZZ,ZZZ,ZZ9"         ;formatting vars

....................................................................
         MOVE      "Ndat0041" TO PROGRAM
         MOVE      "DATACARDS NEEDING UPDATES" TO STITLE
         MOVE      "Names In The News" TO COMPNME
         MOVE      C1 TO NDATPATH  .SET ACCESS TO ISAM
         MOVE      C2 TO NORDPATH
         move       c3,nordlock
         move       c3,ndatlock
         clock     date to today
         CALL      PAINT
         MOVE      "ABORT" TO PF5
         CALL      FUNCDISP
         display   *p1:24,*el,"opening dupe own";
         PACK      STR35,NTWKPATH1,"INDEX\DUPEOWN"
         OPEN      DUPEOWN,STR35,READ
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
....................................................................
BEGIN    IFZ       PC
.START PATCH 2.4 REPLACED LOGIC
.         PREPARE   OUTPUT,"g:\DATA\CARDexcl.CAL"
.begin patch 2.81
	pack	str55,ntwkpath1,"cardExcl.cal|NINS1:502"
.         PACK      STR35,"c:\work\CARDexcl.CAL"
.         PREPARE   OUTPUT,STR35,exclusive
         PREPARE   OUTPUT,STR55,exclusive
.end patch 2.81
.END PATCH 2.4 REPLACED LOGIC
         xif
.begin patch 2.81
	move	c4,ndatpath
.end patch 2.81	
         call       ndattst
         GOTO      A100
...............................
..........................................................................................
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
        if          (lstnum = "011507")
        call        Debug
        endif
. 
CHKRUN   RESET     RUNCODES
         SCAN      lstnum IN RUNCODES         .running charge only list?
         GOTO      A100 IF EQUAL              .yes skip
.
	Reset	EXFEELST
         SCAN      lstnum IN EXFEELST         .LM Exchange charge only list?
         GOTO      A100 IF EQUAL              .yes skip
.
         CMATCH    "W"  TO status        .withdrawn ?
         GOTO      A100 IF EQUAL         .yes skip
         cmatch    "T" to status
         goto      a100 if equal         .temp with. skip per smm 12/3
         
          if        (elstcde <> "C" & Elstcde <> "P")                 .should no longer be nec.
          goto      A100
          endif

         scan      "OFFICE USE" in mlstname
         goto      a100 if equal
         reset     mlstname
         scan      "Office use" in mlstname
         goto      a100 if equal
         reset     mlstname
         scan      "Office Use" in mlstname
         goto      a100 if equal
.
CHECK   
         UNPACK    revdate,cc,DATYR,DATMO,DATDAY
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
         display   *p10:14,"not updated last 90 days",count1
C100
          clear     ords
          clear check
          clear check2
          clear orddate
          REP       zfill IN LSTNUM
          PACK      NORDFLD2,AKEY2,LSTNUM
          CLEAR     NORDFLD1
          CLEAR     NORDFLD3
          CLEAR     NORDFLD4
.
          CALL      NORDAIM
          GOTO      A100 IF OVER            .no usage skip it
          CMATCH    "p" TO OSTAT       Pending order ?
          GOTO      CheckOtherOrds IF EQUAL               . YES, skip.
          CMATCH    "x" TO OSTAT       Cancelled Pending order ?
          GOTO      CheckOtherOrds IF EQUAL     YES, skip.
          CMATCH    "l" TO OSTAT       LCR order ?
          GOTO      CheckOtherOrds IF EQUAL     YES, skip.
          CMATCH    "z" TO OSTAT       Cancelled LCR order ?
          GOTO      CheckOtherOrds IF EQUAL     YES, skip.
          MOVE      OODTEM TO MM
          MOVE      OODTED TO DD
          MOVE      OODTEY TO YY
          CALL      CVTJUL
          MOVE      juldays TO ORDDATE
          MOVE      ORDDATE TO CHECK
          move      today1 to check2
          SUB       check FROM CHECK2
          compare   "366" to check2
          IF        LESS
          add       c1 to ORDS             .ITs GOOD
          ELSE
          GOTO     CheckOtherOrds           .DATE OUT OF RANGE
          ENDIF
CheckOtherOrds
          loop
          CALL      NORDKG
          until     over
          CMATCH    "p" TO OSTAT       Pending order ?
          GOTO      CheckOtherOrds IF EQUAL               . YES, skip.
          CMATCH    "x" TO OSTAT       Cancelled Pending order ?
          GOTO      CheckOtherOrds IF EQUAL     YES, skip.
          CMATCH    "l" TO OSTAT       LCR order ?
          GOTO      CheckOtherOrds IF EQUAL     YES, skip.
          CMATCH    "z" TO OSTAT       Cancelled LCR order ?
          GOTO      CheckOtherOrds IF EQUAL     YES, skip.
          MOVE      OODTEM TO MM
          MOVE      OODTED TO DD
          MOVE      OODTEY TO YY
          CALL      CVTJUL
          MOVE      juldays TO ORDDATE
          MOVE      ORDDATE TO CHECK
          move      today1 to check2
          SUB       check FROM CHECK2
          compare   "366" to check2
          IF        LESS
          add       c1 to ORDS             .ITs GOOD
          endif
          REpeat
NOORDS
          clear     str9
          CLEAR     DIM11A
          move      mask9 to dim11A
          edit      ORDS to dim11A
. 
.
OWNPREP
          UNPACK    OWNNUM,STR2,DOWNKEY
         REP       zfill IN DOWNKEY
         READ      DUPEOWN,DOWNKEY;DOWNKEY,DUPE1,NEWOLON
         IF        NOT OVER
          PACK      OWNNUM,"00",NEWOLON
         ENDIF
.
          move      COMMPER,str3
          write     output,seq;b1,lstnum,ownnum,nlstcde,elstcde,str3,hotline,newdate,revdate,password,mlstname,universe,dim11a
         ADD       c1 TO USED
         DISPLAY   *P12:18,"NUMBER OF LISTS WITH USAGE ",USED
         move      c0 to qty
         goto       a100
. 
C101
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
.Debug
.         return    
. 
         INCLUDE   NORDIO.inc
         INCLUDE   NDATIO.inc
         INCLUDE   COMLOGIC.inc

