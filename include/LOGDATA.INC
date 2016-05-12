* .............................................................................
.       THE LOG ENTRYS TYPE:
.                  LOG-ON .........USER SIGN ON
.                  LOG-OFF.........USER SIGN OFF
.                  BAD ID..........INVALID ATTEMPT TO SIGN ON
.                  ERROR...........DATASHARE ERROR
.                  PROGRAM.........SUCCESSFUL CHAIN TO PROGRAM
.                  NO PROGRAM .....UNSUCCESSFUL CHAIN TO PROGRAM
* .............................................................................
.logfile.dat
LOGFILE  FILE      Fixed=140
LOGTYPE  DIM       10
LOGINFO  DIM       20
LogFlag  form              1
* .............................................................................
.SEQ      FORM      "-1"    USE VARS FROM CONS.INC
.SEQ3     FORM      "-3"
LOGPORT  DIM       24  1-24
LOGRN    FORM      4  25-28
ZERO     FORM      "0"           29-29
SUBDIRCT DIM       6  30-35
LOCKSW   DIM       1  36-36
PROC     DIM       2  37-38
PORT     DIM       2  39-40
PORTTYPE DIM       2  41-42
PORTSCRN DIM       2  43-44
PORTUDA  DIM       5  45-49
FILL     DIM       1             50-50      *USED FOR UNPACK OF PORT.
PORTINFO DIM       40            51-90      *USED FOR CLOCK OF PORT.
TASK     DIM       12            91-102     *USED FOR UNPACK OF PORTINFO, = TASK NAME
NODE     DIM       12            103-114    * "    "    "    "    "     , = NODE NAME
;           IFZ          PC
ATTRIB   DIM       16            115-130    * "    "    "    "    "     , = ATTRIBUTES OF PORT.
;user    dim              10     131-140
;           XIF
;           IFNZ         PC
;ATTRIB   DIM       11      * "    "    "    "    "     , = ATTRIBUTES OF PORT.
;           XIF
HEADER   INIT      "*     NODE        TASKNAME        LOG TYPE    DATE "
HEADER1  INIT      "     TIME      OTHER INFO"
HEADER2  INIT      "*     ====        ========        ========    ==== "
HEADER3  INIT      "     ====      =========="
* END OF INCLUDE.
. ................
