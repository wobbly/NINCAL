.****************************************************************************
.*  DATACARD - Service B. CHARGES BILLING FILE DESCRIPTOR.
.*  changed to additional billing instructions jan98
.*  encompasses more than just triplex charges
. ....................................................
.
.      FILE:      NINDAT3
.    LENGTH:      12
.  COMPRESS:      NONE
.      TYPE:      ISAM
. INDEX KEY:      1-6   LSTNUM
.
.DLH 2015 June 2  add Min and space
.DLH 03Jan2008  PLI new rates needed
. 5nov96 DLH added code E
.last change: DLH 09Aug95 added owner number and code fields.
...............................................................................
.NDAT3FLE IFILE     KEYLEN=6,FIXED=36,Name="NINDAT3.ISI|NINS1:502"            .03Jan2008
NDAT3FLE IFILE     KEYLEN=6,FIXED=80,Name="NINDAT3.ISI|NINS1:502"            .03Jan2008
.NDAT3FLE IFILE     KEYLEN=6,FIXED=30            .26Jan98
.NDAT3FLE IFILE     KEYLEN=6,FIXED=13
NDAT3NME INIT      "NINDAT3|NINS1:502                           "
NDAT3FLD DIM       6
NDAT3FLG FORM      1
.
.
Ndat3VAR  LIST
NDAT3KEY  DIM                 6           1-6  ZERO FILLED KEY (LIST NUMBER).
NDATTDMC  DIM                 1           7-7  TDMC RUNNING CHARGE CODES:
.                                   ' ' = WE DON'T BILL, "B" = WE BILL ALL,
.                                   'R' = WE BILL RENTALS & RENTALS/SPLITS
.                                   'E' = We bill only on exchanges 5nov96
ndat3own  dim                 4           8-11 ' ' not nec
ndat3cde  dim                 1          12-12  " " = triplex, "A"=anacapa, "F"=Fide, "J"=Antares, "M"=MMI, "P"=Pidi, "R"= Frontline,,"K"=MKGT
ndatdolc  dim                 1          13-13  "y"=we need to bill $/date selects
ndat3exh  dim                 1          14-14   "Y"=We bill charges on Exchange
ndat3ex1  dim                 8          15-22   date we started billing exch fee mmddccyy
ndat3ex2  dim                 8          23-30   Date we stopped billing exch fee mmddccyy
Ndat3ExRt Form                3.2        31-36   Management Exchange Rate per thousand
nDat3Min  form                5.2        37-42   .Min to be billed 
Ndat3Fill  Dim                38         43-80
           Listend
...............................................................................
.  END INCLUDE.

