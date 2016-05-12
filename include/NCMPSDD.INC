.NCMPSDD.INC       samples attached to orders/lcrs
......................................................................
NCMPSfile  ifile      keylen=8,FIXED=20,Name="NCMPSAMPL.ISI|NINS1:502"

NCMPSname  init       "NCMPSAMPL"
NCMPSFLD              DIM        8             .Campaign # + Record #

NCMPSFLAG             FORM       1
NCMPSLOCK             FORM       1    0 or 1=File locks, 2=Record locks, 3=No locks
.
NCMPSVARS  LIST
NCMPSLR    DIM        6       1-6   Campaign NUMBER
NCMPSREC   DIM        2       7-8   SAMPLE Record NUMBER
NCMPSNUM   DIM        3      10-11   SAMPLE NUMBER
NCMPSFILL  DIM        9      12-20  FILLER
         LISTEND
