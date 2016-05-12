..............................................................................
.
. NSCHDD INCLUSION
. NIN BOOKING SCHEDULE FILE DEFINITION
.
. FILE NAME : MAILDATE
. REC LENGTH: 26 FIXED
. AIM KEY  : 1-6 (6 POSITION LIST#)
.	   : 7-8  YEAR
.	   : 9-10 MONTH 
..............................................................................
         IFNZ      PC
NSCHFILE AFILE     FIX=26
         XIF
         IFZ       PC
.NSCHFILE AFILE     19,3,,,FIX=26
NSCHFILE AFILE     FIX=26
         XIF
.
NSCHFLAG FORM      1
NSCHNAME INIT	   "MAILDATE"
NSCHFLD	 DIM	   9      LIST
NSCHFLD2 DIM	   5      YEAR
NSCHFLD3 DIM	   5     MONTH
.
. NSCHFILE FILE            RECORD SIZE 26 FIXED
. .............
.
SCHKEY   DIM       6       1-6   LIST NUMBER
SCHYEAR  DIM       2       7-8   YEAR SCEDULED (ASSUMED TO BE CURRENT YEAR IF
.                          NOT SPECIFIED).
SCHMONTH DIM       2       9-10   MONTH SCEDULED
SCHDAY   DIM       2      11-12   DAY SCEDULED (ALWAYS A MONDAY).
SCHMLR   DIM       7      13-19   MAILER DESC KEYED IN
SCHQUANT DIM       7      20-26   QUANTITY SCHEDULED KEYED IN.
.

