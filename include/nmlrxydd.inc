..............................................................................
.
. NMLRXYDD INCLUSION
. NIN MAILER TO MAILER FILE DEFINITION
.
. FILE NAME : NINMLRXY
. REC LENGTH: 1068 FIXED
. INDEX KEY : (1)  1-12       (MAILER1/MAILER2)
.AIMDEX KEYS: (1)  1-12       (MAILER1/MAILER2)
.
..............................................................................
NMLRXYFLIST	FILELIST
nmlrxyfile ifile     keylen=12,FIXED=1068,Name="NINMLRXY.ISI"
nmlrxyfle2 Afile     FIXED=1068,Name="NINmlrxy.AAM"
		FILELISTEND
.
nmlrxyname init      "NINMLRXY"
NMLRXYFLD  DIM       12             .MAILER NUMBER 1 + MAILER NUMBER 2
NMLRXYFLD1 DIM       15             .MAILER NUMBER AIMKEY
NMLRXYFLAG FORM      1
NMLRXYLOCK FORM      1    0 or 1=File locks, 2=Record locks, 3=No locks
.
NMLRXYVARS LIST
NMLRXYMLR1  DIM       6         1-6     MAILER1 NUMBER
NMLRXYMLR2  DIM       6         7-12    MAILER2 NUMBER 
NMLRXYNOTE  DIM       750      13-762   NOTES
NMLRXYNOTE2 DIM       250     763-1012  NOTES
NMLRXYDATE  DIM       8      1013-1020  RECORD DATE
NMLRXYUDATE DIM       8      1021-1028  UPDATE DATE
NMLRXYINITS DIM       10     1029-1038  PERSON WHO LAST MODIFIED
NMLRXYFILL  DIM       30     1039-1068  FILLER
         LISTEND
