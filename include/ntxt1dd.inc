******************************************************
*  NTXT1DD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  DATACARD TEXT FILE
* ****************************************************
* NAMES IN THE NEWS 
* ****************************************************
.
.      FILE:      NINTXT1
.    LENGTH:      256
.      TYPE:      ISAM,MANAGED
. INDEX KEY:      1-6        NTXT1LIST
.                 1-8        
......................................................
NTXT1NAME  INIT      "NINTXT1"
.
NTXT1FLIST Filelist
NTXT1FILE  IFILE     KEYLEN=6,FIXED=58,Name="NINTXT1.isi|NINS1:502"
NTXT1FLE1  IFILE     KEYLEN=8,FIXED=58,Name="NINTXT1a.isi|NINS1:502"
                    filelistend
.
NTXT1LOCK  FORM      1         0 OR 1=filepi,2=recordlock, 3=no lock
NTXT1FLD  DIM        6
NTXT1FLD1  DIM       8
NTXT1FLAG  FORM      1
NTXT1PATH  FORM      1
.
.
NTXT1VARS  LIST
NTXT1LIST  DIM       6         1-6       LIST NUMBER
NTXT1num   DIM       2         7-8       NUMBER
NTXT1TEXT  DIM       50        9-58     TEXT
                    LISTEND
