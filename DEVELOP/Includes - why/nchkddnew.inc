..............................................................................
.
. NCHKDD INCLUSION
.
. FILE NAME : NINCHK.DAT
. REC LENGTH: 129
. INDEX KEY : 1-36  CONTROL NUMBER + CONTROL DATE + CHECK NUMBER
. AAMDEX KEY: 1-11  CONTROL NUMBER/CONTROL DATE
.            12-36  CHECK NUMBER
.            45-59  AMOUNT
.            60-104 PAYOR 
..............................................................................
.
NCHKLIST  Filelist
.NCHKFILE  IFILE     KEYLEN=36,FIXED=129,NODUPLICATES
.NCHKFLE2  AFILE     FIXED=129
NCHKFILE  IFILE     NAME="NINCHK.ISI"
NCHKFLE2  AFILE     NAME="NINCHK.AAM"
          Filelistend
NCHKNAME  INIT      "NINCHK"
NCHKFLD   DIM       36
NCHKFLD1  DIM       14
NCHKFLD2  DIM       28
NCHKFLD3  DIM       18
NCHKFLD4  DIM       48
NCHKFLAG  FORM      1
NCHKlock  form      1             0 or 1 - filepi, 2 =record locking, = no locks
..............................................................................

NCHKVARS  list
NCHKCONT  DIM       3        1-3        CONTROL NUMBER (KEY) (AAMKEY)
NCHKCONTD DIM       8	     4-11	CONTROL DATE (AAMKEY)
NCHKNUM   DIM       25      12-36       CHECK NUMBER (KEY) (AAMKEY)
NCHKDATE  DIM       8       37-44       CHECK DATE - CCYYMMDD
NCHKAMT   FORM      12.2    45-59       CHECK AMOUNT
NCHKPAYOR DIM       45      60-104      CHECK PAYOR
NCHKFILL  DIM	    25	   105-129	FILL
          listend

