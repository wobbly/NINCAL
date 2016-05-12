..............................................................................
.
. NCHKDD INCLUSION
.
. FILE NAME : NINCHK.DAT
. REC LENGTH: 110
. INDEX KEY : 1-23 CONTROL NUMBER + CONTROL DATE + CHECK NUMBER
. AAMDEX KEY: 1-11 CONTROL NUMBER/CONTROL DATE
.            12-23 CHECK NUMBER
.            32-46 AMOUNT
.            47-91 PAYOR 
..............................................................................
. Patch 1.1	01/10/2005 ASH  Added filler for eventual increase of check number
.
NCHKLIST  Filelist
.NCHKFILE  IFILE     KEYLEN=23,FIXED=110,NODUPLICATES
.NCHKFLE2  AFILE     FIXED=110
NCHKFILE  IFILE     NAME="NINCHK.ISI"
NCHKFLE2  AFILE     NAME="NINCHK.AAM"
          Filelistend
NCHKNAME  INIT      "NINCHK"
.START PATCH 1.1 REPLACED LOGIC
.NCHKFLD   DIM       17
NCHKFLD   DIM       23
.END PATCH 1.1 REPLACED LOGIC
NCHKFLD1  DIM       14
.START PATCH 1.1 REPLACED LOGIC
.NCHKFLD2  DIM       9
NCHKFLD2  DIM       15
.END PATCH 1.1 REPLACED LOGIC
NCHKFLD3  DIM       18
NCHKFLD4  DIM       48
NCHKFLAG  FORM      1
NCHKlock  form      1             0 or 1 - filepi, 2 =record locking, = no locks
..............................................................................

NCHKVARS  list
NCHKCONT  DIM       3        1-3        CONTROL NUMBER (KEY) (AAMKEY)
NCHKCONTD DIM       8	     4-11	CONTROL DATE (AAMKEY)
.START PATCH 1.1 REPLACED LOGIC
.NCHKNUM   DIM       6       12-17       CHECK NUMBER (KEY) (AAMKEY)
NCHKNUM   DIM       12      12-23       CHECK NUMBER (KEY) (AAMKEY)
.END PATCH 1.1 REPLACED LOGIC
NCHKDATE  DIM       8       24-31       CHECK DATE - CCYYMMDD
NCHKAMT   FORM      12.2    32-46       CHECK AMOUNT
NCHKPAYOR DIM       45      47-91       CHECK PAYOR
NCHKFILL  DIM	    19	    92-110	FILL
          listend
