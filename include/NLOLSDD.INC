.NLOLSDD.INC       samples attached to orders/lcrs
......................................................................
NLOLSfile  ifile      keylen=8,FIXED=20,Name="NLOLSAMPL.ISI|NINS1:502"

NLOLSname  init       "NLOLSAMPL"
NLOLSFLD              DIM        8             .LOL # + Record #

NLOLSFLAG             FORM       1
NLOLSLOCK             FORM       1    0 or 1=File locks, 2=Record locks, 3=No locks
.
NLOLSVARS  LIST
NLOLSLOL    DIM        6       1-6   LOL NUMBER
NLOLSREC   DIM        2       7-8   SAMPLE Record NUMBER
NLOLSNUM   DIM        3      10-11   SAMPLE NUMBER
NLOLSFILL  DIM        9      12-20  FILLER
         LISTEND
