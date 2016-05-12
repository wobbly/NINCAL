....................................................
.  PROGRAM:  In-House Directory Listing
.  AUTHOR:   Andrew Harkins
.  DATE:     June 23, 1998
....................................................
         INC       COMMON.inc
         INCLUDE   CONS.inc
         INC       HP.inc
RELEASE  INIT      "1.2"       DLH     area code added
Reldate    Init       "2015 March 11"
.RELEASE  INIT      "1.1"       ASH   04OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.0"       ASH   23JUN98   YEAR 2000 COMPLIANT

COUNT    FORM      5
.
ROLOVARS  LIST
NAME      DIM       25
NUMBER    DIM       10
NOTES     DIM       25
          LISTEND
.
DRCTFILE  FILE
.START PATCH 1.1 REPLACED LOGIC
.         OPEN     DRCTFILE,"G:\DATA\ROLODEX.TXT"
         PACK     STR35,NTWKPATH1,"ROLODEX.TXT"
         OPEN     DRCTFILE,STR35
.END PATCH 1.1 REPLACED LOGIC
.
         MOVE     "NINCAL" TO PROGRAM
         MOVE     "MASTER MAILER PRINT" TO STITLE       
.START PATCH 1.1 REPLACED LOGIC
.         SPLOPEN  "G:\DATA\ROLODEX.LST"
         SPLOPEN  STR35
.END PATCH 1.1 REPLACED LOGIC
         print     hpdupl,hp17ptch,*f
         CALL     PAINT
.
         CALL      HEADER
.
LOOP     READ      DRCTFILE,SEQ;ROLOVARS
. 
         GOTO      EOJ IF OVER
         ADD       "1" TO COUNT
         DISPLAY   *P10:12,*EL,"RECORDS PROCESSED : ",COUNT
         PRINT     *1,NAME,*33,NUMBER,*45,NOTES
.DISPLAY   *P1:23,*BLINKON,*HON,"OOOPS",*B
         GOTO      LOOP


HEADER
         PRINT     *F,*1,"CONFIDENTIAL":
                   *26,"* * *   N I N   M A S T E R   ":
                   "M A I L E R, B I L L - D I R E C T  F I L E   * * *"

         RETURN
EOJ      PRINT     hpport,hpdupoff,hpreset,*FLUSH
         shutdown  "cls"
         STOP
         INCLUDE    COMLOGIC.inc

