******************************************************
*  NTXTDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  DATACARD TEXT FILE
* ****************************************************
* NAMES IN THE NEWS MASTER MEDIA CODE FILE.
* ****************************************************
.
.      FILE:      NINTXT
.    LENGTH:      508
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-10           NTXTLIST + NTXTNUM
.AIMDEX KEY:      1-6 NTXTLISTi
.                       8-507 NTXTTXT
.patch 1.1   DLH   16Oct2006   MIn conversion
......................................................
NTXTNAME   INIT       "NINTXT"
.
NTXTFLIST  Filelist
NTXTFILE   IFILE      KEYLEN=8,FIXED=508,Name="NINTXT.isi|NINS1:502"
NTXTFLE2   AFILE      FIXED=508,Name="NINTXT.aam|NINS1:502"
.NTXTFILE  IFILE      KEYLEN=9,FIXED=507,Name="NINTXT.isi"
.NTXTFLE2  AFILE      FIXED=507,Name="NINTXT.aam"
.NTXTFILE  IFILE      KEYLEN=9,FIXED=507,Name="NINTXT.isi|NINS1:502"
.NTXTFLE2  AFILE      FIXED=507,Name="NINTXT.aam|NINS1:502"
                      filelistend
.
NTXTLOCK   FORM       1          0 OR 1=filepi,2=recordlock, 3=no lock
NTXTFLD               DIM        8
NTXTFLD1   DIM        9
NTXTFLD2   DIM        503
NTXTFLAG   FORM       1
NTXTPATH   FORM       1
.
.
NTXTVARS   LIST
NTXTLIST   DIM        6          1-6        LIST NUMBER
NTXTNUM    DIM        2          7-8        TEXT NUMBER
.NTXTNUM              DIM        1          7-7        TEXT NUMBER
.NTXTTEXT  DIM        500     8-507   TEXT
NTXTTEXT   DIM        500     9-508   TEXT
                      LISTEND