;*        NXNGDD/INC.
;* *****************************************************************************
;* NAMES IN THE NEWS MASTER EXCHANGE ACCOUNT FILE.
;* *****************************************************************************
;.
;.    FILE:      NINXNUM
;.  LENGTH:      30
;.COMPRESS:      NONE
;.    TYPE:      AIM
;.     KEY:      ACCKEY     MLR1,MLR2 1-6,7-12
;...............................................................................
.Need to add 100 byte note field
;. revision:
...08 February 2010        DLH  NXNGLock
;..13Jul98        DLH  ADD flag AND DATE FOR purpose of allowing ACCOUNT Inactivation.
. Patch 1.1  31JAN05  ASH  Increased Mailer fields
;.
;...............................................................................
;.
.START PATCH 1.1 REPLACED LOGIC
.;NXNGNAME INIT        "NINXNUM  "
.NXNGNAME INIT         "NINXNUM.AAM|20.20.30.103:502"
.NXNGFLD1 DIM          7
.NXNGFLD2 DIM       7
.NXNGFILE AFILE     FIXED=22
.NXNGFLAG  FORM     1
.ACKEY    DIM       8              HOLDS ACCKEY FOR CHECK AFTER READ.
.. .............................................................................
..
.. ..ACCOUNT MASTER
..
.ACCKEY   DIM       8       1-8       MAILER #1/MAILER #2.
.;.BLANK08  DIM       8       9-16      NOT USED.
.nxngdate  DIM      8       9-16      DATE inactivated ccyymmdd
.ENTRY    FORM      5      17-21      ENTRY NUMBER.           detail entry  number for most current detail record
.Flag     DIM       1      22-22      " "=normal, "I" = inactive
.;.FLAG    DIM       1      22-22      NOT USED.
.................................................
NXNGNAME  INIT      "NINXNUM.AAM|NINS1:502"
NXNGFLD1  DIM       9
NXNGFLD2  DIM       9
NXNGFILE  AFILE     FIXED=30
NXNGFLAG  FORM      1
NXNGLock  FORM      1
ACKEY               DIM       12              HOLDS ACCKEY FOR CHECK AFTER READ.
. .............................................................................
.
. ..ACCOUNT MASTER
.
ACCKEY              DIM       12      1-12      MAILER #1/MAILER #2.
nxngdate  DIM       8      13-20      DATE inactivated ccyymmdd
ENTRY               FORM      5      21-25      ENTRY NUMBER.           detail entry  number for most current detail record
Flag                DIM       1      26-26      " "=normal, "I" = inactive
xngfiller dim       4      27-30
.END PATCH 1.1 REPLACED LOGIC
