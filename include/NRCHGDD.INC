.
.    Last change:       DMB  14 Aug 2001   8:45  pm     Turning Live
.                       DMB  17 Jul 2001   10:53 pm
..............................................................................
.
. NRCHGDD INCLUSION
.
. FILE NAME : EXCHARGE.DAT
. REC LENGTH: 66
. INDEX KEY: 1-6 LR
..............................................................................
.
NRCHGFLE FILE
EXCHARGE IFILE     KEYLEN=6,FIXED=66,DUP,"Name=Excharge|NINS1:502"
EXPRINT  IFILE     KEYLEN=20,STATIC=5
.NRCHGNAME  INIT    "DBTEST"
NRCHGNAME  INIT    "EXCHARGE|NINS1:502"
NRCHGFLD   DIM      6
NRCHGFLAG  FORM      1
NRCHGFLG2  FORM      1
NRCHGlock  form      1             0 or 1 - filepi, 2 =record locking, = no locks
..............................................................................
NRCHGVARS  list
NRCHGLR       DIM       6          (INDEX KEY)= 1-6
NRCHGMLR      DIM       4           7-10                       MALIER NAME
NRCHGMLRCNT   DIM       3          11-13 MAILER/CONTACT NUMBER
NRCHGLN       DIM       6          14-19 LIST NUMBER
NRCHGOWNER    DIM       4           20-23                      OWNER NUMBER
NRCHGTPI      DIM       6          24-29 TRIPLEX INVOICE #
NRCHGQTY      FORM      9           30-38            QUANTITY
NRCHGAR       FORM      8.2         39-49            A/R
NRCHGAP       FORM      8.2         50-60            A/P
NRCHGCE       DIM       2          61-62
NRCHGYR       DIM       2          65-66
NRCHGMO       DIM       2          65-66
  listend




          

