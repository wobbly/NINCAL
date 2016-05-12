...............................................................................
.
. njrndd inclusion
. NINCA JOURNAL FILE DEFINITION
.
. FILE NAME  :  JOURNAL1
. REC LENGTH :  72 (FIXED)
. AIMDEX KEY :  INVOICE NUMBER 1-6
.patch 2.0  NINVOICE Y2k
.
...............................................................................
.
         IFNZ      PC
.NJRNFILE AFILE     FIX=66
.NJRNFILE AFILE     FIX=68             .plb
NJRNFILE AFILE     FIX=72
         XIF
         IFZ       PC
.NJRNFILE AFILE     1,6,,FIX=66             .pcbus
.NJRNFILE AFILE     FIX=66             .plb
.patch 2.0
.NJRNFILE AFILE     FIX=68             .plb
NJRNFILE AFILE     FIX=72             .plb
.end patch 2.0
         XIF
.NJRNFLE2 FILE      FIX=66
.NJRNFLE2 FILE      FIX=68
.patch 2.0
NJRNFLE2 FILE      FIX=72
.end patch 2.0
NJRNNAME INIT      "JOURNAL1"
NJRNFLAG FORM      1
NJRNPATH FORM      1
NJRNFLD  DIM       9
NJRNLOCK FORM      1    0 or 1=File locks, 2=Record locks, 3=no locks
.
. ................................
.
jtnvars  list
JINVN    DIM       6           *INVOICE NUMBER     POSITION 1-6. KEY
JTRANS   DIM       1           *NOT USED           POSITION 7.
JLRNUM   DIM       6           *LIST RENTAL NUMBER POSITION 8-13.
JMLRN    DIM       4           *MAILER NUMBER      POSITION 14-17
JCOBN    DIM       3           *MAILER CONTACT NUM POSITION 18-20.
JREASON  DIM       1           *REASON CODE        POSITION 21-21.
.begin patch 2.0
.JAMOUNT  FORM      9           *AMOUNT OF TRANS.   POSITION 22-30.
JAMOUNT  FORM     10.2           *AMOUNT OF TRANS.   POSITION 22-30.
.end patch 2.0
JINVN2   DIM       6           *IF CONTRA 2ND INV  POSITION 31-36.
NJRNCMNT DIM       24          *ADDITIONAL COMMENT POSITION 37-60.
JDATE    DIM       8           *TRANSACTION DATE   POSITION 61-68.
         listend
.

