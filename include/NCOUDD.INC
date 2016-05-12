;    Last change:  JLO   1 Jul 96    8:35 am
* *****************************************************************************
* NAMES IN THE NEWS MASTER COUNTS FILE.
* *****************************************************************************
.
.    FILE:      TRIPLEXCOUNT
.  LENGTH:      62
.COMPRESS:      NONE
.    TYPE:      ISAM
.     KEY:      1-14 CTLIST,CTRTYPE,CTSTYPE
...............................................................................
NCOUFILE IFILE  KEYL=14
NCOUNAME INIT   "TDMCOUNT"
NCOUFLD  DIM    14
NCOUFLAG FORM    1
NCOUPATH FORM    1
*******************************************************************************
*COUNTSVAR -  VARIABLE INCLUSION FOR COUNTS SYSTEM 19-MAY-87.
*******************************************************************************
* THERE ARE FOUR RECORD TYPES FOR EVERY LIST*
* 1. LIST NAME RECORD 1-RECORD OF THIS TYPE PER LIST.
* 2. SEX RECORD 1-RECORD OF THIS TYPE PER LIST.
* 3. SCF RECORD X-RECORDS IF THIS TYPE PER LIST (ONE FOR EVERY SCF THAT HAS
*    NAMES.
* 4. DTT RECORD 1-RECORD OF THIS TYPE PER LIST.
* ******** LIST NAME TYPE *****************************************************
CTKEY    DIM       14            REDEFINES CTLIST,CTRTYPE,CTSTYPE.
.
CTLIST   DIM       8             1-8     KEY LIST NUMBER=00+NINCA LIST #.
CTRTYPE  DIM       3             9-11    KEY RECORD TYPE='LID'
CTSTYPE  DIM       3            12-14    KEY RECORD SUB TYPE='000'
CTLISTN  DIM       48           15-62    LIST NAME 
.
* ******** SEX RECORD TYPE ****************************************************
.CTLIST   DIM       8            1-8     KEY LIST NUMBER=00+NINCA LIST #.
.CTRTYPE  DIM       3            9-11    KEY RECORD TYPE='SEX'
.CTSTYPE  DIM       3           12-14    KEY RECORD SUB TYPE='000'
CTMALE   FORM      8            15-22    MALE COUNT
CTFEMALE FORM      8            23-30    FEMALE COUNT
CTDUAL   FORM      8            31-38    DUAL COUNT
CTCOMP   FORM      8            39-46    COMPANY COUNT
CTUNKN   FORM      8            47-54    UNKNOWN COUNT
CTSEED   FORM      8            55-62    SEED COUNT
* ******** SCF RECORD TYPE ****************************************************
.CTLIST   DIM       8           1-8      KEY LIST NUMBER=00+NINCA LIST #.
.CTRTYPE  DIM       3           9-11     KEY RECORD TYPE='SCF'
.CTSTYPE  DIM       3          12-14     KEY RECORD SUB TYPE="001 TO 999'
.CTUNKN   FORM      8          15-22    UNKNOWN COUNT
.CTFEMALE FORM      8          23-30    FEMALE COUNT
.CTMALE   FORM      8          31-38    MALE COUNT
.CTDUAL   FORM      8          39-46    DUAL COUNT
.CTCOMP   FORM      8          47-54    COMPANY COUNT
CTTOTAL  FORM      8           55-62     RECORD TOTAL
* ******** DTT RECORD TYPE ****************************************************
.CTLIST   DIM       8           1-8      KEY LIST NUMBER=00+NINCA LIST #.
.CTRTYPE  DIM       3           9-11     KEY RECORD TYPE='DTT'
.CTSTYPE  DIM       3          12-14     KEY RECORD SUB TYPE='000'
CTDATE   DIM       8           15-22     DATE COUNTS UPDATED
.PROGRAM ACCESS.
.NAME           TYPE          ACCESS
.misc required vars.
LST      INIT      "LID"
SCF      INIT      "SCF"
SEX1     INIT      "SEX"
ZIP      INIT      "ZIP"
DTT      INIT      "DTT"


