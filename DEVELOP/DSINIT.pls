..............................................................................
.
. PROGRAM    : DSINIT
. DATE       : 07/03/87
. DESCRIPTION: DATASHARE ANSWER PROGRAM FROM CHAINS OR STANDALONE
..............................................................................
.common uda variables do not change unless common.inc changes to match
.*****************************************************************************
PC       EQU       0
.
ERROR    DIM       35                  DATASHARE ERROR MESSAGE
TODAY    DIM       8                   DATE IN mm/dd/yy FORMAT
SECURITY FORM      1
FUNC     DIM       2
TYPINIT  DIM       2                  TYPIST INITIALS
PORTN    FORM      3                   SOFT PORT NUMBER
AGENDAID DIM       6
JULIAN   FORM      5                   JULIAN DAY (FROM 1-1-80)
USER    DIM       10                  USER ID
USERNME DIM       10                  USER NAME (FIRST INIT, LAST)
PRIO    FORM      3                   OVERALL PRIORITY LEVEL
LEVELS  DIM       36                  SECURITY LEVELS
COMM    DIM       1                   COMMUNICATION ALLOWED
.
COMPANY FORM      1                   COMPANY CODE (1-NIN,2-PL,3-,4-)
COMPNME DIM       24                  COMPANY NAME TEXT
MULTCOS FORM      1                   MULTIPLE COMPANIES ALLOWED
CURSYS  FORM      1                   CURRENT SYSTEM
CURLEVL FORM      1                   CURRENT LEVEL WITHIN SYSTEM
MULTSYS FORM      1                   MULTIPLE SYSTEMS ALLOWED
PROGRAM DIM       8                   LAST PROGRAM NAME
COMMENT DIM       30
INITS   DIM       3                   USER'S INITIALS
EXIT    FORM      2                   EXIT FLAG
INPNAME DIM       25                  INPUT FILE NAME
OUTNAME DIM       25                   OUTPUT FILE NAME (/OUT ASSUMED)
PRTNAME DIM       25                   PRINT FILE NAME  (/PRT ASSUMED)
Filepath DIM      45                   use Path name ie \\nins1\e\data\text
Subject  dim      30
.
..............................................................................
.above is common uda do not change - except to match common.inc
.
.
Release  init       "1.6.2"   09SEP00 DLH add subject and path variables
.RELEASE  INIT      "1.6.1"   DLH  06Jan98 Increase size of VERSION VAR
.RELEASE  INIT      "1.6"     D.L. HERRICK 02Jul96 plb/dos/argh
.RELEASE  INIT      "1.5"     D.L. HERRICK 05Jun95 use c drive
.RELEASE  INIT      "1.4"     D.L. HERRICK 22APR92
.                            CONVERTED FOR NINCAL
.RELEASE  INIT      "1.3"     E.W. LAKE  07/14/88
.                            PCBUS CONVERSION
.
.RELEASE INIT      "1.2"     E.W. LAKE     03/10/88
.                            NEW COMMON VARIABLES (EXIT,PRTFILE,KTGNAME)
.
.RELEASE INIT      "1.1"     E.W. LAKE     02/09/88
.                            ADD INITS.
.                            COMPUTE JULIAN.
.
.RELEASE INIT      "1.0"     E.W. LAKE     07/03/87
.                            INITIAL RELEASE
..............................................................................
.
. COMMON VARIABLE NAMES
.
CM1      INIT       "$ERROR"
CM2      INIT       "$PORTN"
CM3      INIT       "$TODAY"
CM4      INIT       "$JULIAN"
CM5      INIT       "$USER"
CM6      INIT       "$USERNME"
CM7      INIT       "$PRIO"
CM8      INIT       "$LEVELS"
CM9      INIT       "$COMM"
CM10     INIT       "$COMPANY"
CM11     INIT       "$COMPNME"
CM12     INIT       "$MULTCOS"
CM13     INIT       "$CURSYS"
CM14     INIT       "$CURLEVL"
CM15     INIT       "$MULTSYS"
CM16     INIT       "$PROGRAM"
CM17     INIT       "$FUNC"
CM18     INIT       "$INITS"
CM19     INIT       "$EXIT"
CM20     INIT       "$INPNAME"
CM21     INIT       "$OUTNAME"
CM22     INIT       "$PRTNAME"
CM23     INIT       "$COMMENT"
.begin patch 1.1
CM24     INIT       "$PATH"
CM25     INIT       "$SUBJECT"
.end patch 1.1
.
DSPROG   FILE
.STR40    DIM       40
.
VERSION  DIM       14                 .changed 06Jan98 DLH
.VERSION  DIM       11
TIME     DIM       8
INTNUM   FORM      2
NEWPASS  DIM       10
.
FLDNAME  DIM       127
VALUE    DIM       127
LL       FORM      2
.C23      FORM      "23"
C25      FORM      "25"
CAPS     INIT      "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"
.
NHEAD    INIT      "Names In The News        "
CHEAD    INIT      "Pacific Lists, Inc.      "
DHEAD    INIT      "NIN Direct Response, Inc."
THEAD    INIT      "     NIN Data, Inc.      "
+
         INCLUDE   CONS.INC
envir    dim       255
env1     dim       80
env2     dim       80
env3     dim       80
env4     dim       80
env5     dim       80
davename dim       50        dh argh hope it works
sdbstuff init      "\\nins1\e\apps\plb\code\"                          ."
+......................................................................
.
STARTOK  MOVE      "DSINIT  " TO PROGRAM
         NORETURN
         SCAN      "A0" IN S$error$
         IF         EQUAL
         DISPLAY   *P10:12,*EL,"ERROR ",S$ERROR$,*W30
         SHUTDOWN  "CLS" 
         ENDIF
         TRAP      END IF F3
         TRAP      STARTOK NORESET IF INT
         TRAP      CHAINERR GIVING ERROR IF IO
         TRAP      CHAINERR GIVING ERROR IF PARITY
         TRAP      CHAINERR GIVING ERROR IF FORMAT
         TRAP      CHAINERR GIVING ERROR IF RANGE
.
         CLOCK     PORT TO STR3
         MOVE      STR3 TO PORTN
.
         clock     env to envir
         unpack    envir into env1,env2,env3,env4,env5
         CLOCK     DATE TO TODAY
         IFZ       PC
         UNPACK    TODAY TO MM,STR1,DD,STR1,YY
         XIF
         IFNZ      PC
         UNPACK    TODAY INTO MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         XIF
         MOVE      C0 TO JULIAN
         CALL      CVTJUL
         MOVE      JULDAYS TO JULIAN
         PACK      ERROR WITH B10,B10
.
         MOVE      C0 TO COMPANY           INITIALIZE COMPANY FIELDS
         MOVE      C0 TO MULTCOS
         PACK      COMPNME WITH B9,B9,B9
         PACK      LEVELS WITH B9,B9,B9,B9 INITIALIZE SECURITY LEVELS
         MOVE      C0 TO CURSYS
         MOVE      C0 TO CURLEVL
         MOVE      C0 TO MULTSYS
         MOVE      C0 TO FUNC
         MOVE      C0 TO COMM
         MOVE      "ZZZ" TO INITS
         MOVE      C0 TO EXIT
         PACK      INPNAME WITH B10,B2
         PACK      OUTNAME WITH B10,B2
         MOVE      B8 TO PRTNAME
         PACK      COMMENT WITH B10,B10,B5
         MOVE      "900" TO PRIO
.
. READ THE 'DSPROG' FILE.
. THIS FILE CAN CONTAIN ANY COMMON VARIABLES NOTED BY THEIR ACTUAL FIELD NAME.
. THE ONLY REQUIRED ENTRY WOULD BE 'PROGRAM'.
.
         TRAP      trya IF IO
         OPEN      DSPROG,"C:\WORK\DSPROG.dat"
         trapclr   io
         goto      dsread
trya     trap      proggone if io
         OPEN      DSPROG,"a:\DSPROG.dat"
         TRAPCLR   IO
DSREAD   READ      DSPROG,SEQ;STR40
         GOTO      EXEC IF OVER
         RESET     CAPS
         REPLACE   CAPS IN STR40
         ENDSET    STR40
CHKEND   CMATCH    B1 TO STR40
         GOTO      LLSET IF NOT EQUAL
         BUMP      STR40 BY -1
         GOTO      CHKEND IF NOT EOS
LLSET    RESET     STR40
         MOVELPTR  STR40 TO LL
         SCAN      EQUAL IN STR40
         GOTO      DSREAD IF NOT EQUAL
         BUMP      STR40 BY -1
         MOVEFPTR  STR40 TO N2              THE LENGTH OF 1ST VARIABLE
         SFORMAT   FLDNAME TO N2            WHICH IS THE COMMON FIELD NAME
         ADD       C1 TO N2                 GET BACK TO FP OF '='.
         SUBTRACT  N2 FROM LL               MINUS FULL LENGTH IS LENGTH OF 2ND 
         SFORMAT   VALUE TO LL              WHICH IS THE VALUE FIELD
         RESET     STR40
         UNPACK    STR40 TO FLDNAME,STR1,VALUE
.
         SEARCH    FLDNAME FROM CM1 USING C25 INTO N2
         GOTO      DSREAD IF OVER
.begin patch 1.1
.         BRANCH    N2 TO CM1,CM2,CM3,CM4,CM5,CM6,CM7,CM8,CM9,CM10:
.                         CM11,CM12,CM13,CM14,CM15,CM16,CM17,CM18,CM19:
.                         CM20,CM21,CM22,CM23
         BRANCH    N2 TO CM1,CM2,CM3,CM4,CM5,CM6,CM7,CM8,CM9,CM10:
                         CM11,CM12,CM13,CM14,CM15,CM16,CM17,CM18,CM19:
                         CM20,CM21,CM22,CM23,cm24,cm25
.end patch 1.1
         GOTO      DSREAD
CM1      MOVE      VALUE TO ERROR
         GOTO      DSREAD
CM2      MOVE      VALUE TO PORTN
         GOTO      DSREAD
CM3      MOVE      VALUE TO TODAY
         GOTO      DSREAD
CM4      MOVE      VALUE TO JULIAN
         GOTO      DSREAD
CM5      MOVE      VALUE TO USER
         GOTO      DSREAD
CM6      MOVE      VALUE TO USERNME
         GOTO      DSREAD
CM7      MOVE      VALUE TO PRIO
         GOTO      DSREAD
CM8      MOVE      VALUE TO LEVELS
         GOTO      DSREAD
CM9      MOVE      VALUE TO COMM
         GOTO      DSREAD
CM10     MOVE      VALUE TO COMPANY
         GOTO      DSREAD
CM11     MOVE      VALUE TO COMPNME
         GOTO      DSREAD
CM12     MOVE      VALUE TO MULTCOS
         GOTO      DSREAD
CM13     MOVE      VALUE TO CURSYS
         GOTO      DSREAD
CM14     MOVE      VALUE TO CURLEVL
         GOTO      DSREAD
CM15     MOVE      VALUE TO MULTSYS
         GOTO      DSREAD
CM16     MOVE      VALUE TO PROGRAM
         GOTO      DSREAD
CM17     MOVE      VALUE TO FUNC
         GOTO      DSREAD
CM18     MOVE      VALUE TO INITS
         GOTO      DSREAD
CM19     MOVE      VALUE TO EXIT
         GOTO      DSREAD
CM20     MOVE      VALUE TO INPNAME
         GOTO      DSREAD
CM21     MOVE      VALUE TO OUTNAME
         GOTO      DSREAD
CM22     MOVE      VALUE TO PRTNAME
         GOTO      DSREAD
CM23     MOVE      VALUE TO COMMENT
         GOTO      DSREAD
.begin patch 1.1
CM24     MOVE      VALUE TO FILEPATH
         GOTO      DSREAD
CM25     MOVE      VALUE TO SUBJECT
         GOTO      DSREAD
.end patch 1.1
.
EXEC     MATCH     B10 TO COMPNME
         GOTO      EXEC2 IF NOT EQUAL
         LOAD      COMPNME USING COMPANY FROM NHEAD,CHEAD,DHEAD,THEAD
EXEC2    TRAP      CHAINERR GIVING ERROR IF CFAIL
.         display   *p01:24,*el,env1,*r:
.                   *p01:24,env2,*r:
.                   *p01:24,env3,*r:
.                   *p01:24,env4,*r:
.                   *p01:24,env5,*w5
         trap      bad giving error if cfail
         clock     version to version
.         display   *p1:24,*el,version,*w5;
          if        (inpname = "NLOINC0007")
          chain     INpname
          endif
          if        (inpname = "CREDIT0003" | inpname = "CREDIT0002")
          chain     INpname
          endif
          if        (inpname = "NEOM0004N" | inpname = "NEOM0004P")
          chain     INpname
          endif
          if        (inpname = "NINC0004A" | inpname = "NINC0004B")
          chain     INpname
          endif
          if        (inpname = "NEOM0023A" | inpname = "NEOM0024A")
          chain     INpname
          endif
          if        (inpname = "NONA0012ADHOC")
          chain     INpname
          endif
          if        (inpname = "NEOM0010ADHOC" | inpname = "NEOM0004ADHOC")
          chain     INpname
          endif
         scan      "SDBWIN" in version
         if        equal
         CHAIN     program
         endif
         reset     version
         scan      "PLBWIN" in version
         if        equal
         CHAIN     program
         endif
         reset     version
         scan      "PLBSERVE" in version
         if        equal
         CHAIN     program
         endif
         reset     version
         scan      "PLBCLIENT" in version
         if        equal
         CHAIN     program
         endif
         pack      davename with sdbstuff,program
         CHAIN     davename
         trapclr   cfail
         shutdown  "cls"
         stop
PROGGONE DISPLAY   *P01:24,"DSPROG File is missing. ",*W
         GOTO      END
.
CHAINERR DISPLAY   *P1:23,*EL,*HON,"PROGRAM NOT FOUND ",PROGRAM,*B,*W,*B
         GOTO       END
bad      DISPLAY   *P1:23,*EL,*HON,"cfail error trapped ",PROGRAM,*B,*W,*B:
                   *p1:24,*el,s$cmdlin,*r,*w,*w,*w,*b:
                   *p1:24,*el,error,*w4,*b
         shutdown  "cls"
         stop
.......................................................................

         INCLUDE   COMLOGIC.INC
         
