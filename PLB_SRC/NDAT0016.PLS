PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INC       NDATDD.inc

RELEASE  INIT      "1.8"       ASH 28JAN2004 DATACARD CONVERSION
.RELEASE  INIT      "1.7"       ASH 29OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.6"       DLH 03MAY94 BUG FIX
.RELEASE  INIT      "1.5"       DLH 14FEB92
INFILE   FILE      VAR=3002,STATIC=12
COUNT    FILE      FIXED=15
SYSMO    DIM       2             DATE FOR CREATED
SYSDAY   DIM       2             INVOICE
SYSYR    DIM       2             RECORD
DATMO    DIM       2
DATDAY   DIM       2
DATYR    DIM       2
DATE     DIM       8
TODAY1    FORM      5
CARDATE  FORM      5
CHECK    FORM      5
JDATE    FORM      5
CHKDATE  FORM      5
STOP     FORM      "7000"
RECRED   FORM      5
RECWRIT  FORM      5
         OPEN      INFILE,"NINDAT",SHARE
.START PATCH 1.7 REPLACED LOGIC
.         PREP      COUNT,"g:\data\text\notnews"
         PACK      TASKNAME,NTWKPATH1,"TEXT\NOTNEWS"
         PREP      COUNT,TASKNAME
.END PATCH 1.7 REPLACED LOGIC
         MOVE      c0 TO RECWRIT
         MOVE      c0 TO RECRED
         MOVE      C1 TO NDATPATH       .SET ACCESS TO ISI BY LIST#.
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO SYSMO,SYSDAY,SYSYR
         PACK      TODAY FROM SYSMO,SLASH,SYSDAY,SLASH,SYSYR
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         UNPACK    DATE INTO SYSMO,STR1,SYSDAY,STR1,SYSYR
         XIF
         REP       zfill,SYSDAY
         REP       zfill,SYSMO
         MOVE      SYSMO TO MM
         MOVE      SYSDAY TO DD
         MOVE      SYSYR TO YY
         CALL      CVTJUL
         MOVE      JULDAYS TO check
.         MOVE      TODAY1 TO CHECK
.         SUB       "365" FROM CHECK
.         SUB       "182" FROM CHECK
         SUB       "90" FROM CHECK
BEGIN    MOVE      "NDAT0016" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "MARK LIST OVER 3 MOS NOT NEW" TO STITLE
         CALL      PAINT
         MOVE      "ABORT" TO PF5
         CALL      FUNCDISP
READ     read      infile,seq;datvars
         GOTO      EOJ IF OVER
         ADD       c1,RECRED
.         COMPARE   RECRED TO STOP
.         GOTO      EOJ IF EQUAL
         DISPLAY   *P12:14,"NUMBER OF RECORDS READ = ",RECRED
         MATCH     NO,NLSTCDE
         GOTO      READ IF EQUAL
         CMATCH    b1,NEWDATE
         GOTO      WRITE IF EQUAL
         GOTO      WRITE IF EOS
         GOTO      NEWDATE
.
NEWDATE  clear     datmo
         clear     datday
         clear     datyr
.START PATCH 1.8 REPLACED LOGIC
.         UNPACK    newDATE INTO DATMO,DATDAY,cc,DATYR
         UNPACK    newDATE INTO CC,DATYR,DATMO,DATDAY
.END PATCH 1.8 REPLACED LOGIC
         REP       zfill,DATDAY
         REP       zfill,DATMO
         move      c0 to mm
         move      c0 to dd
         move      c0 to yy
         MOVE      DATMO TO MM
         MOVE      DATDAY TO DD
         MOVE      DATYR TO YY
         CALL      CVTJUL
         move     c0 to cardate
         MOVE      JULDAYS TO CARDATE
         DISPLAY   *P1:23,*EF,"CHECKING DATE: CHECK = ",CHECK:
                   "CARDATE = ",CARDATE
         COMPARE   CARDATE,CHECK
         GOTO      WRITE IF EQUAL
         GOTO      WRITE IF not LESS
         GOTO      READ
WRITE
         MOVE      LSTNUM TO NDATFLD
         move     mlstname to ndatfld3
         CALL      NDATTST
         CALL      NDATDEL
         MOVE      NO,NLSTCDE
         CALL      NDATWRT
         ADD       c1 TO RECWRIT
         DISPLAY   *P12:15,"NUMBER OF RECORDS WRITTEN = ",RECWRIT
         COMPARE   RECWRIT TO STOP
         GOTO      EOJ IF EQUAL
         GOTO      READ
EOJ
         WRITE     COUNT,SEQ;RECRED,RECWRIT
         WEOF      COUNT,SEQ
         CLOSE     COUNT
         DISPLAY   *ES,*P1:1,*P12:12,"ALL DONE !!",*W2
         CLOSE     INFILE
         STOP
         INCLUDE   NDATIO.inc
         INCLUDE   COMLOGIC.inc

