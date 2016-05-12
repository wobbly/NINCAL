.NDAT0030.DBS
.
PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         inc       hp.inc
         include   nowndd.inc
.START PATCH 2.4 ADDED LOGIC
          INCLUDE   NSELDD.INC
          INCLUDE   NTXTDD.INC
.END PATCH 2.4 ADDED LOGIC
.from ndat0005.dbs

RELEASE  INIT      "2.6"        07APR2005 ASH  COMMPER CONVERSION
.RELEASE  INIT      "2.5"        05AUG2004 ASH  LOGO CONVERSION
.RELEASE  INIT      "2.4"        30JAN2004 ASH  DATACARD CONVERSION
.RELEASE  INIT      "2.3"        02OCT2000 ASH NEW SERVER ADDED
.RELEASE  INIT      "2.2"        20NOV97 JD changed to nprint.
.RELEASE  INIT      "2.1"       22APR94 DLH FIX REFORMAT OF .FRM FILES.
.RELEASE  INIT      "2.0"       19NOV93  DLH ADD FAX OWNER UPDATE REQUEST.
.THIS OPTION - read input file sorted by lo/alpha by list
. at owner break create fax cover sheet, individualy SPOOLED RH for that
. owner & submit to fax with message requesting update.
.RELEASE  INIT      "1.7"       15SEP93  DLH ADD LOTUS FLAT FILE.
.
.RELEASE  INIT      "1.6"
.RELEASE  INIT     "1.5"       02/21/86 -- INCREASE EFFICIENCY OF READS ....
.RELEASE  INIT     "1.4"       01/16/86 -- CHANGE CATAGORY CODES TO 3 BYTES.
.RELEASE  INIT     "1.3"       05/07/85 -- ADD EXCLUDE WITHDRAWN OPTION.
.RELEASE  INIT      "1.2"       08/18/83  DLH   ADDED KILL INPUT FILE &
.                                              REPRINT OPTIONS.
.RELEASE  INIT      "1.1"          05/19/83   DLH INCREASE SPEED VIA ADDING
.                                            PRINT BUFFER, ALSO ADDED PRINT
.                                            OF WITHDRAWN.
.RELEASE  INIT      "1.0"           07/08/83   DLH & DSGEN
NAME     DIM       19 (FILE NAME)
INPUT    FILE      VAR=181 (INPUT  FILE)
OUTPUT   FILE
FORMFILE FILE      UNCOMP

FAXBAT   FILE
FORMNAME DIM       30
data     init      "DATA"
frm      init      ".frm"
.TASKNAME DIM      100
HOLDOWN  DIM       4
LONGDIST DIM       1               HOLDS 1 FOR LONG DISTANCE DIALING
faxuflag  FORM      1
REPLY    DIM       1
LINECT   FORM      2           LINE COUNTER
PAGE     FORM      3           PAGE NUMBER
LR       FORM      "0"         '1'=LAST RECORD PENDING
LRTOT    FORM      7          GRAND TOTAL
ANS      DIM       1
MO       DIM       2       MONTH
DAY      DIM       2
YR       DIM       2
DATE     DIM       8
.START PATCH 2.4 REPLACED LOGIC
.LINE1    DIM       132
.LINE1B   DIM       8
.....................
LINE1    DIM       150
LINE1B   DIM       10
TEXT1    DIM       47         556-602  FREE TEXT.  **NOTE: EACH LINE OF TEXT
.END PATCH 2.4 REPLACED LOGIC
LINE2    DIM       127
LINE2A   DIM       5
LINE2B   DIM       8
LINE3    DIM       127
LINE3A   DIM       5
LINE3B   DIM       8
LINE4    DIM       127
LINE4A   DIM       5
LINE4B   DIM       8
LINE5    DIM       127
LINE5A   DIM       5
LINE5B   DIM       8
PRSW     FORM      "0"
PSTATUS  DIM       6
BLANK127 DIM       127
ONE      FORM      "1"
COUNT    FORM      "00000"
EXSW     DIM       1           * "Y" = DO NOT PRINT WITHDRAWN CARDS.
SW1P     INIT      "Y"         * "Y" = 1ST PASS.
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
UPDFLAG    FORM        1
LOTSFLAG FORM      1
WITHFLAG FORM      1
NINPPATH FORM          1
NUMBER   DIM       10
PRICE    DIM       8
LDESC    DIM       40
BEGIN    FORM      2
LAST     FORM      2
FAXFLAG   FORM     1
spoolfle DIM       30                   .order  SPOOL FILEs
numflag   form     1
*............................................................
           INCLUDE   NDATDD.inc
.
EXCL     DIM       4                    EXCLUSIVE PRINT FIELD
TOTUNIV  FORM      10
UNIMASK  INIT      "Z,ZZZ,ZZZ,ZZ9"
UNIVPRT  DIM       13
+.........................................................................
.
. DEFAULT OPTIONS ARE : NO update info.
.                             NO WITHDRAWN CARDS
.
. COMMENT CAN MODIFY THE DEFAULTS:  UPDATE :  print updated date
.                                         WITHDRN:  PRINT WITHDRAWN CARDS.
.                                   LOTUS  :  OUTPUT TO FLAT FILE
.
. INPNAME WILL CONTAIN THE INPUT FILENAME: ??????/TXT OR ??????
. PRTNAME WILL CONTAIN THE PRINT FILE NAME or 'LOCAL'
.
. IF ANY OF THE ABOVE INFO IS MISSING OR INVALID, REQUEST IT.
.........................................................................

.
+............................................................
.
. FILE OPENING SEQUENCE
         TRAP      TRAP IF F5
         TRAP      TRAP IF INT
         CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        EOS                 .NO
         MOVE      "NDAT0030" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
           MOVE        "LOCAL" TO PRTNAME
           CLEAR     COMMENT
           ENDIF
         MOVE      "RH STYLE DATACARD PRINT" TO STITLE
         MOVE      "EXIT" TO PF5
         MOVE      "Options" TO PF4
         TRAP      OPTGET IF F4
           MOVE        C0 TO UPDFLAG
         CALL      PAINT
         CALL      FUNCDISP
BEGIN    DISPLAY   *P01:05,"Options     :":
                   *P01:06,"Input File  :":
                   *P01:07,"Print File  :":
                   *P01:09,"Record Count:"
.
         GOTO      OPTION
OPTGET   MOVE      C0 TO UPDFLAG
         MOVE      C0 TO LOTSFLAG
         RESET     COMMENT
         KEYIN     *P20:10,"UPDATE :  INCLUDE UPDATE DATE":
                       *P20:11,"WITHDRN:  INCLUDE WITHDRAWN CARDS":
                   *P20:12,"LOTUS  :  PRODUCE FLAT FILE":
                   *P20:13,"FAXUPD :  FAX UPDATE REQUEST":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL;
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
OPTION   MOVE      C0 TO UPDFLAG
           MOVE        C0 TO WITHFLAG
         MOVE      C1 TO LOTSFLAG
         MOVE      C0 TO FAXUFLAG
         RESET     COMMENT
         SETLPTR   COMMENT
         DISPLAY   *P15:05,COMMENT
         MATCH     "                         " TO COMMENT
         GOTO      OPTDEFLT IF EQUAL
         RESET     COMMENT
         SCAN      "FAXUPD" IN COMMENT
         CALL      OPTFAXU IF EQUAL
         RESET     COMMENT
         SCAN      "UPDATE" IN COMMENT
         CALL      OPTUPD IF EQUAL
         RESET     COMMENT
           SCAN      "WITHDRN" IN COMMENT
           CALL      OPTWITH IF EQUAL
         RESET     COMMENT
         SCAN      "LOTUS" IN COMMENT
         CALL      OPTLOTUS IF EQUAL
         GOTO      INPGET
OPTNG    KEYIN     *P20:10,"UPDATE :  INCLUDE UPDATE DATE":
                       *P20:11,"WITHDRN:  INCLUDE WITHDRAWN CARDS":
                   *P20:12,"LOTUS  :  PRODUCE FLAT FILE":
                   *P20:13,"FAXUPD :  FAX UPDATE REQUEST":
                   *P01:24,*EL,"The Option setting is invalid.":
                   *P15:05,COMMENT:
                   *P20:10,*EL,*P20:11,*EL,*P20:12,*EL,*P20:13,*EL:
                   *P20:14,*EL,*P20:15,*EL;
         GOTO      OPTGET
OPTDEFLT MOVE      C1 TO UPDFLAG
           MOVE      C1 TO WITHFLAG
         MOVE      C1 TO LOTSFLAG
         MOVE      C1 TO FAXUFLAG
         GOTO      INPGET
OPTUPD   MOVE      C2 TO UPDFLAG
         move      c1 to lotsflag
         RETURN
OPTWITH    MOVE        C2 TO WITHFLAG
           RETURN
OPTLOTUS MOVE      C2 TO LOTSFLAG
         RETURN
OPTFAXU  MOVE      C2 TO faxuflag
         RETURN
.
INPGET   TRAP      INPNG GIVING ERROR IF IO
         BRANCH    NINPPATH TO PRTGET
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         CLOSE     TESTFILE
           MOVE        C1 TO NINPPATH
         DISPLAY   *P15:06,INPNAME
         MOVE      INPNAME TO NAME
         GOTO      PRTGET
INPNG    NORETURN
         TRAPCLR   IO
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *DV,ERROR:
                   *P15:06,INPNAME
         GOTO      INPGET
.
prtget   COMPARE   C2 TO faxuflag
         IF        EQUAL
         MOVE      "DATA800" TO PRTNAME
         MOVE      "800" TO N3
         ENDIF
         MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL  TO PRTNAME
         GOTO      PRESTART IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH PRTNAME
         BRANCH    LOTSFLAG TO PRTOK,OUTPUT
PRTOK    SPLOPEN   PRTFILE
         DISPLAY   *P15:07,PRTNAME
         GOTO      PRESTART
OUTPUT   SPLOPEN   PRTFILE
         DISPLAY   *P15:7,PRTNAME
.START PATCH 2.3 REPLACED LOGIC
.           APPEND    "g:\DATA\" TO OUTNAME
           APPEND    NTWKPATH1 TO OUTNAME
.END PATCH 2.3 REPLACED LOGIC
         APPEND    PRTNAME TO OUTNAME
           APPEND    ".TMP" TO OUTNAME
           RESET     OUTNAME
           PREPARE   OUTPUT,OUTNAME
           DISPLAY   *P01:07,"Output File :":
                     *P15:07,PRTNAME
         GOTO      PRESTART
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
PRESTART MOVE      C0 TO TOTUNIV
           KEYIN       *P1:24,*T5,STR1;
           CLEAR       PF4
           TRAPCLR   F4
           CALL      FUNCDISP
         COMPARE   C2 TO faxuflag             .FAX MODE ?
         IF        EQUAL
.START PATCH 2.3 REPLACED LOGIC
.         PREPARE   FAXBAT,"g:\data\FAXRHUPD.BAT"
.          WRITE     FAXBAT,SEQ;"g:"
         PACK      TASKNAME,NTWKPATH1,"FAXRHUPD.BAT"
         PREPARE   FAXBAT,TASKNAME
          WRITE     FAXBAT,SEQ;NTWKPATH1
.END PATCH 2.3 REPLACED LOGIC
          WRITE     FAXBAT,SEQ;"CHDIR \data"
         ENDIF
.
         TRAP      IO IF IO
         OPEN      INPUT,inpname
         TRAPCLR   IO
         MOVE      "00000" TO COUNT
         CLOCK     DATE TO DATE
           IFNZ        PC
         UNPACK    DATE INTO MO,DAY,YR
           XIF
           IFZ         PC
         UNPACK    DATE INTO MO,STR1,DAY,STR1,YR
           XIF
         REP       " 0",DAY
         REP       " 0",MO
         CLEAR     TODAY
         PACK      TODAY FROM MO,SLASH,DAY,SLASH,YR
START
.         COMPARE   C2 TO FAXUFLAG
.         IF        EQUAL
.         PRINT     hpreset,hpLAND,hplin8,hp17ptch;
.         ENDIF
.         CALL      HEADER
.
*............................................................
. READ A RECORD FROM THE FILE
.
READ     DISPLAY   *P1:24,*EL,*HON,"READING";
.START PATCH 2.4 REPLACED LOGIC
.          READ      INPUT,SEQ;STATUS,LSTNUM,OWNNUM,CATCDE1,CATCDE2:
.                   CATCDE3,CATCDE4,CATCDE5,CATCDE6,CATCDE7,CATCDE8,CATCDE9:
.                   CATCDE10,NLSTCDE,ELSTCDE,COMMPER,HOTLINE,NEWDATE:
.                   REVDATE,PASSWORD,MLSTNAME,UNIVERSE,TEXT1
.START PATCH 2.6 REPLACED LOGIC
.         READ      INPUT,SEQ;STATUS,LSTNUM,OWNNUM,NLSTCDE,ELSTCDE,COMMPER,HOTLINE,NEWDATE:
.                   REVDATE,PASSWORD,MLSTNAME,UNIVERSE
          READ      INPUT,SEQ;STATUS,LSTNUM,OWNNUM,NLSTCDE,ELSTCDE,str3,HOTLINE,NEWDATE:
                    REVDATE,PASSWORD,MLSTNAME,UNIVERSE
.END PATCH 2.6 REPLACED LOGIC
          clear     TEXT1
          if (NDATCONV = "1")
                    move      C1,NDATPATH
                    pack      NDATFLD,LSTNUM
                    move      "NDATKEY",Location
                    pack      KeyLocation,NDATFLD
                    call      NDATKEY
.                   if (NDATEXCH <> "1")
                              pack      NSELFLD1,"01X",LSTNUM
                              pack      NSELFLD2,"021XBASE"
                              move      "NSELAIM",Location
                              pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                              call      NSELAIM
                              if not over
                                        if (NSELEXC <> "2")
                                                  pack      TEXT1,NSELQTY,B1,NSELSNAME,B1,"$",NSELPRICE
                                        else
                                                  pack      TEXT1,NSELQTY,B1,NSELSNAME,B1,"EXCHANGE ONLY"
                                        endif
                              else
                                        goto DataCheckText
                              endif
.                   endif
          else
DataCheckText
                    pack      NTXTFLD,LSTNUM,"1"
                    move      "NTXTKEY",Location
                    pack      KeyLocation,"Key: ",NTXTFLD
                    call      NTXTKEY
                    if not over
                              move      NTXTTEXT,text1
                    endif
          endif
.END PATCH 2.4 REPLACED LOGIC

*............................................................
. TEST FOR END OF FILE
. IF END OF FILE: TURN ON LR AND L1-L9
.
         GOTO      eoj IF OVER
         DISPLAY   *P1:24,*EL;
*............................................................
.
RIDON
         ADD       ONE TO COUNT
         DISPLAY   *P15:12,*EL,"RECORDS READ = ",COUNT;
         COMPARE   C2 TO faxuflag               .FAX OPTION ?
         IF        EQUAL                       .YES
         call      breakchk
         endif
+............................................................
. TOTAL CALCULATIONS
.
TOTCALC  TRAPCLR   PARITY  (NOP)
TOTCLX   TRAPCLR   PARITY  (NOP)
         call      movedata
         goto      read
*............................................................
. MOVE DATA FROM INPUT AREA TO FIELDS
.
MOVEDATA
         MOVE      B4 TO EXCL        CLEAR EXCLUSIVE PRINT FIELD.
         CMATCH    "C" TO ELSTCDE        EXCLUSIVE?
         CALL      EXCL IF EQUAL         YES
*............................................................
. DETAIL CALCULATIONS
.
DETCALC  MOVE      "        ",PSTATUS
         CMATCH    "W",STATUS
         CALL      WITHDRAW IF EQUAL
         CMATCH    "T" TO STATUS
         CALL      TEMPWITH IF EQUAL
         ADD       ONE,PRSW
         ADD       ONE TO LRTOT
.START PATCH 2.4 REPLACED LOGIC
.         MOVE      C0 TO N9
.         MOVE      UNIVERSE TO N9
.         ADD       N9 TO TOTUNIV
         MOVE      C0 TO N10
         MOVE      UNIVERSE TO N10
         ADD       N10 TO TOTUNIV
.END PATCH 2.4 REPLACED LOGIC
         GOTO      DETLOAD
* ...........................................................
WITHDRAW
           BRANCH    WITHFLAG OF SKIP
         MOVE      "WTHDRN",PSTATUS
         RETURN
TEMPWITH MOVE      "TMPWDN" TO PSTATUS
         RETURN
EXCL     MOVE      "EXCL" TO EXCL
         RETURN
SKIP
         NORETURN
         GOTO      READ
*............................................................
DETLOAD
. LOAD PRINT LINE
         BRANCH    LOTSFLAG OF LOAD,LTSOUT
LTSOUT
.START PATCH 2.4 REPLACED LOGIC
.         UNPACK    REVDATE INTO MM,STR1,DD,STR1,CC,YY
         UNPACK    REVDATE,CC,YY,MM,DD
.END PATCH 2.4 REPLACED LOGIC
         CALL      CVTJULTS
         CLEAR     NUMBER
         clear     price
         clear     ldesc
CMOVE    CMOVE     TEXT1 TO STR1
         TYPE      STR1
         IF        EQUAL
         APPEND    STR1 TO NUMBER
         ELSE
         CMATCH    "," TO STR1
         GOTO      BUMPIT IF EQUAL
         CMATCH    B1 TO STR1
         GOTO      NUMDONE IF EQUAL
         GOTO      NUMDONE IF EOS
         GOTO      BUMPIT
         ENDIF
BUMPIT   BUMP      TEXT1 BY 1
         GOTO      CMOVE
NUMDONE  RESET     NUMBER
         MOVEFPTR  TEXT1 TO BEGIN
         SCAN      "$" IN TEXT1
         IF        NOT EQUAL                   .NO PRICE
         CLEAR     PRICE
         RESET     TEXT1
         SETLPTR   TEXT1 TO BEGIN
         APPEND    TEXT1 TO LDESC
         RESET     TEXT1
         ELSE
         MOVEFPTR  TEXT1 TO LAST
         APPEND    TEXT1 TO PRICE
         SUB       C1 FROM LAST
         RESET     TEXT1
         SETLPTR   TEXT1 TO LAST
         BUMP      TEXT1 BY BEGIN
         APPEND    TEXT1 TO LDESC
         ENDIF
         WRITE     OUTPUT,SEQ;LSTNUM,",#"",MLSTNAME,"#",",NUMBER,",#"",LDESC:
                   "#",",PRICE,",#"",PSTATUS,"#",",JULDAYS
load
.LOAD     BRANCH    PRSW OF LINE1,LINE2,LINE3,LINE4,LINE5
LINE1
         PACK      LINE1 FROM LSTNUM,B3,MLSTNAME,B5:
                   TEXT1,B1,PSTATUS,b1,excl
.START PATCH 2.4 REPLACED LOGIC
.         MOVE      REVDATE TO LINE1B
          UNPACK    REVDATE,CC,YY,MM,DD
          PACK      STR10,MM,SLASH,DD,SLASH,CC,YY
          MOVE      STR10,LINE1B
.END PATCH 2.4 REPLACED LOGIC
         GOTO      LOADEXIT
.
LOADEXIT
         MOVE      B4 TO EXCL
         DISPLAY   *P1:24,*EL;
         ADD       c2 TO LINECT
* ...........................................................
. HEADING AND DETAIL OUTPUT
.
DETOUT   DISPLAY   *P1:24,*EL,*HON,"PRINTING";
         COMPARE   "54" TO LINECT
         CALL      HEADER IF NOT LESS
         MOVE      C0,PRSW
           BRANCH    UPDFLAG OF DT1,DT2
DT1      PRINT     *L,*1,LINE1
.         PRINT     *FLUSH;
           GOTO      BLANK
DT2      PRINT     *L,*1,LINE1,*134,LINE1B
.         PRINT     *FLUSH;
           GOTO      BLANK
.
BLANK      MOVE      BLANK127,LINE1
           MOVE      B1 TO LINE1B
         DISPLAY   *P1:24,*EL;
         RETURN
* ...........................................................
. GOTO PRINT TOTAL COUNT
RETURN
         RETURN
*............................................................
. PAGE HEADING ROUTINE
.
PAGE     ADD       ONE TO PAGE
           BRANCH    UPDFLAG OF PAGE1,PAGE2
PAGE1      branch    faxuflag to page1a,page1b
page1A   PRINT     *F,*C,*L:
                   *1,"CONFIDENTIAL":
                   *50,"*** NAMES IN THE NEWS MASTER LISTING ***":
                   *118,"PAGE ",PAGE:
                   *L,*118,"DATE: ",TODAY,*FLUSH,*L,*L,*C
         MOVE      "5" TO LINECT
         CMATCH    YES TO SW1P
         CALL      ZEROLINE IF EQUAL
         RETURN
page1B   compare   c1 to page
         if        not equal
           PRINT     *F:
                   *118,"PAGE ",PAGE:
                   *L,*118,"DATE: ",TODAY,*FLUSH,*L,*L,*C
         else
           PRINT    *118,"PAGE ",PAGE:
                    *L,*118,"DATE: ",TODAY,*FLUSH,*L,*L,*C
         endif
         MOVE      "5" TO LINECT
         CMATCH    YES TO SW1P
         CALL      ZEROLINE IF EQUAL
         RETURN
PAGE2      BRANCH     FAXUFLAG OF PAGE2A,PAGE2B
PAGE2A   PRINT     *F,033,"M",*C,*L:
                   *1,"CONFIDENTIAL":
                   *50,"*** NAMES IN THE NEWS MASTER LISTING ***":
                   *118,"PAGE ",PAGE:
                   *L,*118,"DATE: ",TODAY,*FLUSH,*L,*L,*C
         RETURN
PAGE2B   compare   c1 to page
         if        not equal
         PRINT     *F,033,"M",*C,*L:
                   *118,"PAGE ",PAGE:
                   *L,*118,"DATE: ",TODAY,*FLUSH,*L,*L,*C
         else
         PRINT     033,"M",*C,*L:
                   *118,"PAGE ",PAGE:
                   *L,*118,"DATE: ",TODAY,*FLUSH,*L,*L,*C
         endif
         RETURN
..............................................................................
HEADER
           CALL       PAGE
           BRANCH    UPDFLAG OF HD1,HD2
HD1      PRINT     *2,"LIST##":
                   *14,"MASTER LIST NAME":
                   *68,"QUANTITY":
                   *84,"DESCRIPTION":
                   *107,"PRICE";
         PRINT     *FLUSH;
         PRINT     *2,"_____":
                   *14,"________________":
                   *68,"________":
                   *84,"___________":
                   *107,"_____":
                    *C,*L
         branch    lotsflag of hdexit,hd3
HD2      PRINT     *2,"LIST##":
                   *14,"MASTER LIST NAME":
                   *68,"QUANTITY":
                   *84,"DESCRIPTION":
                   *107,"PRICE",*134,"REVISED";
         PRINT     *FLUSH;
         PRINT     *2,"_____":
                   *14,"________________":
                   *68,"________":
                   *84,"___________":
                   *107,"_____":
                       *134,"_______":
                    *C,*L
         branch    lotsflag of hdexit,hd3
hdexit   return
.......
HD3      COMPARE   C1 TO PAGE
           GOTO      HDEXIT IF NOT EQUAL
.START PATCH 2.5 REPLACED LOGIC
.           WRITE     OUTPUT,SEQ;"#"CONFIDENTIAL#",":
.                      "#"NAMES IN THE NEWS CA. INC.#",":
.                      "#"",TODAY,"#""
           WRITE     OUTPUT,SEQ;"#"CONFIDENTIAL#",":
                      "#"NAMES IN THE NEWS#",":
                      "#"",TODAY,"#""
.END PATCH 2.5 REPLACED LOGIC
         WRITE      OUTPUT,SEQ;B2,"#"LIST###",":
                   "#"MASTER LIST NAME#",":
                   B6,B6,B6,"#"QUANTITY#",":
                   B2,"#"DESCRIPTION#",":
                   B6,"#"PRICE#",":
                   B6,"#"REVISED#","
        GOTO       HDEXIT
ZEROLINE MOVE      C0 TO LINECT
         MOVE      NO TO SW1P
         RETURN
*............................................................
. TOTAL OUTPUT
.
TOTOUT   TRAPCLR   PARITY  (NOP)
         COMPARE   "52" TO LINECT
         CALL      PAGE IF NOT LESS
         MOVE      UNIMASK TO UNIVPRT
         EDIT      TOTUNIV TO UNIVPRT
         COMPARE    C2 TO FAXUFLAG
         IF        NOT EQUAL
         PRINT     *L,*35,"NUMBER OF RECORDS: ",LRTOT,"****"
         PRINT     *L,*35,"TOTAL UNIVERSE",*61,UNIVPRT
         ENDIF
         move      c0 to lrtot
         move      c0 to totuniv
         move      c0 to page
.         compare   c1 to count
.         return    if equal
         SPLCLOSE
         release
         branch     numflag of totout1,totout2
totout1  clear      taskname
.START PATCH 2.3 REPLACED LOGIC
.         append     "c:\winnt\system32\cmd.exe /c copy g:\data\",taskname
         append     "c:\winnt\system32\cmd.exe /c copy ",taskname
         append     NTWKPATH1,taskname
.END PATCH 2.3 REPLACED LOGIC
         append     prtname to taskname
         APPEND    ".LST" TO TASKNAME
         append     " \\NINs2\Laser2 ",taskname
         reset      taskname
         execute    taskname
         return
totout2
          clear     taskname
.START PATCH 2.3 REPLACED LOGIC
.         append     "c:\winnt\system32\cmd.exe /c copy g:\data\",taskname
         append     "c:\winnt\system32\cmd.exe /c copy ",taskname
         append     NTWKPATH1,taskname
.END PATCH 2.3 REPLACED LOGIC
         append     prtname to taskname
         APPEND    ".LST" TO TASKNAME
.         append     " \\nts2\fax",taskname
         append     " \\NINs2\Laser8",taskname
         reset      taskname
         execute    taskname
         pause     "10"
.        return
.         append     "f:\public\nprint g:\data\",taskname
.         append     prtname to taskname
.          APPEND    ".LST" TO TASKNAME
.         append     " q=facsys s=NTS0_FPNW nt nb f=0 /d",taskname
.         reset      taskname
.         execute    taskname
.         if         over
.         display    *p1:24,*el,"your fax job may have failed",*b,*b,*b,*w10
.         endif
         write       faxbat,seq;taskname
         return
* ...........................................................
. TRAP - JOB INTERUPTED.
.
TRAP     TRAPCLR   INT
         TRAPCLR   F5
         TRAP      TRAP IF F5
         TRAP      TRAP IF INT
         DISPLAY   *P1:24,*EL,*B,"JOB ABORTED!!!!!",*B,*W2;
         NORETURN
         GOTO      EOJ
*............................................................
. END OF JOB
EOJ
NOUPD      PRINT     *F
         GOTO      CLOSE
.         CLOSE     INPUT
UPD       PRINT     *F,033,"@"                  .RESET TO DEFAULTS
CLOSE     SPLCLOSE
         RELEASE
WEOF

EOJ1
END1       COMPARE   C2 TO faxuflag
         IF        EQUAL
.START PATCH 2.4 REPLACED LOGIC
.         move      ownnum to nownfld
          UNPACK    OWNNUM,str2,str4
         match     str4 to nownfld
.END PATCH 2.4 REPLACED LOGIC
         call      nownkey
         call      totout2
.         call      prepfax
         write       faxbat,seq;taskname
         WEOF      FAXBAT,SEQ
         CLOSE     FAXBAT
         endif
         shutdown  "cls"
         STOP
..............................................................................
BREAKCHK compare   c1 to count
          if        equal
.START PATCH 2.4 REPLACED LOGIC
.          move      ownnum to holdown
.          move      ownnum to nownfld
          UNPACK    OWNNUM,str2,str4
          move      str4 to holdown
          move      str4 to nownfld
.END PATCH 2.4 REPLACED LOGIC
          call      nownkey
          call      prepfax
         PRINT     hpreset,hpLAND,hplin8,hp17ptch;
          call      header
          return
          endif
.START PATCH 2.4 REPLACED LOGIC
.         match    ownnum to holdown
.         return   if equal
.         move     ownnum to nownfld
.         call     nownkey
.         move     ownnum to holdown
          UNPACK    OWNNUM,STR2,STR4
         match    STR4 to holdown
         return   if equal
         move     STR4 to nownfld
         call     nownkey
         move     STR4 to holdown
.END PATCH 2.4 REPLACED LOGIC
         call     totout
.        print    *F
.        splclose
.        release
.         call     prepfax
         ADD       C1 TO N3
         MOVE      N3 TO STR3
         PACK      PRTNAME FROM data,STR3
         PACK      PRTFILE FROM PRTNAME
         SPLOPEN   PRTFILE
         display   *P15:07,PRTNAME
          call      prepfax
         PRINT      hpreset,faxland
         call      header
.         call      movedata
         return
+..............................................................................
prepfax
         cmatch    b1 to ownlonm
          if        eos
          clear     ownlonm
          endif
         reset     ownocpy to 25
blankc   cmatch   b1 to ownocpy
          if       equal
          bump     ownocpy,-1
          goto     blankc
          else
          lenset   ownocpy
          reset    ownocpy,1
          endif
         match     "0000000000" to ownfax
         if        equal
         goto      nofaxnum
         endif
.
         type      ownfax                      .valid phone?
         IF        EQUAL                       .yes
              move       c2 to numflag
            COUNT     N2,OWNFAX
            COMPARE   C10 TO N2
             IF        EQUAL
             MOVE      C1 TO LONGDIST
             UNPACK    OWNFAX INTO STR3,STR7
             match     "415" to str3            . local?
              IF         EQUAL
              MOVE       STR7 TO OWNFAX
              CLEAR      LONGDIST
              endif
             endif
         unpack    date into mm,str1,dd,str1,yy
.         clock     time to time
.         clear     str5
.         append    time to str5
.         reset     str5
         print     "^[D",longdist,ownfax,"^[N",ownocpy:
                   "^]":
                   *n,032,hpreset:
                   hpttray:
                   hpport:
                   033,"&l66P":               page length
                   033,"&l65F"
.                  033,"&l1E",033,"&a0c0R":     top margin * print position
.                  hpltrhd:
         call       PortraitLTRHEAD
         print     *n,*n,*n,*n,*n:
                   *n,*n,*n,hpt225,hp10pt,hpbon,"REQUEST FOR UPDATED INFORMATION":
                   *L,hpboff:
                   *N,hpt350,"VIA FACSIMILE":
                   *N,*N,*n,*n,*n,hpt050,hp10pt,"Date: ",mo,"/",day,"/",yr:
                   *N,*N,hpt050,"TO: ",hpt150,OWNOCPY:
                   *N,*N,hpt050,"FROM:",hpt150,"Carol Frazer":
                   *N,*N,hpt050,"We are eager to continue promoting your lists":
                   " to our clients.":
                   *N,*n,hpt050,"The attached reflects your managed list(s) that need updated":
                   *N,hpt050,"information for our data card library. Please fax your ":
                   *N,hpt050,"datacard to my attention, so we can append our records.":
                   *N,*n,hpt050,"Because most of our clients are non-profits, we are interested ":
                   *N,hpt050,"in any fundraising rates or special discounts your list owner may offer.":
                   *N,hpt050,"Please make sure this information is included.":
                   *N,*n,hpt050,"Thank you for your prompt attention. If you ":
                   "have any other questions,":
                   *N,hpt050,"please give me a call.":
                   hpuprght,hp11pt:
                   *N,*n,hpt050,"Regards,":
                   *N,hpt050:
                   *N,hpt050,"Carol Frazer":
                   *N,hpt050,"Data Card Librarian"
.          compare   c1 to count
.          return    if equal
.         SPLCLOSE
          return
          endif
.printing  not faxing - no or invalid fax number
nofaxnum
          display   *p1:24,*el,*b,"no fax number"
          move       c1 to numflag
         return
*...........................................................................
. KILLFLE KILL PRINT FILE
KILLFLE
.         PREPARE   INPUT,NAME
         CLOSE     INPUT,DELETE
         DISPLAY   *P10:24,*EL,NAME," Has been deleted",*B;
.         GOTO      DOMORE
         shutdown   "cls"
           STOP
*............................................................
. IO ERROR
.
IO       DISPLAY   *P1:1,*ES,"FILE NOT FOUND ",*B,*B ;
         GOTO      INPNG
         include   nownio.inc
         include   hpio.inc
.START PATCH 2.4 REPLACED LOGIC
          INCLUDE   NDATIO.INC
          INCLUDE   NSELIO.INC
          INCLUDE   NTXTIO.INC
.END PATCH 2.4 REPLACED LOGIC
         INCLUDE   COMLOGIC.inc
