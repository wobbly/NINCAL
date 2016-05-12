.NORDS.INC       samples attached to orders/lcrs
......................................................................
NORDSfile  ifile      keylen=8,FIXED=30,Name="NORDSAMPL.ISI|NINS1:502"

NORDSname  init       "NORDSAMPL"
NORDSFLD              DIM        8             .ORder # + Record #

NORDSFLAG             FORM       1
NORDSLOCK             FORM       1    0 or 1=File locks, 2=Record locks, 3=No locks
.
NORDSVARS  LIST
NORDSLR    DIM        6       1-6   LR NUMBER
NORDSREC   DIM        2       7-8   SAMPLE Record NUMBER
NORDSNUM   DIM        3      10-11   SAMPLE NUMBER
NORDSCODE  DIM       1      12-12  SAMPLE CODE, 1=sample enclosed,2=sample to follow, OR 3=sample prev approved
NordScode1 dim       1      13-13   *=sample Denied   
NORDSFILLER           DIM        17     14-30  FILLER
         LISTEND
