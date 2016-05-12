******************************************************
*  NSLTDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  SELECTION CODE FILE DESCRIPTOR.
* ****************************************************
* NAMES IN THE NEWS MASTER SELECTION CODE FILE.
* ****************************************************
.patch 1.001        DLH MIn Conversion   September 2006
.
.      FILE:      NINSLT
.    LENGTH:      29
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-9         NSLTLIST + NSLTNUM
.AIMDEX KEY:      1-6         NSLTLIST
......................................................
NSLTNAME  INIT      "NINSLT"
.
NSLTFLIST Filelist
.NSLTFILE IFILE     KEYLEN=9,FIXED=29,Name="NINSLT.isi"
.NSLTFLE2 AFILE     FIXED=29,Name="NINSLT.aam"
.NSLTFILE IFILE     KEYLEN=9,FIXED=29,Name="NINSLT.isi|20.20.30.103:502"
.NSLTFLE2 AFILE     FIXED=29,Name="NINSLT.aam|20.20.30.103:502"
NSLTFILE  IFILE     KEYLEN=9,FIXED=29,Name="NINSLT.isi|NINS1:502"
NSLTFLE2  AFILE     FIXED=29,Name="NINSLT.aam|NINS1:502"
.begin patch 1.001
.NSLTFILE IFILE     KEYLEN=9,FIXED=37,Name="NINSLT.isi|NINS1:502"
.NSLTFLE2 AFILE     FIXED=37,Name="NINSLT.aam|NINS1:502"
.end patch 1.001

         filelistend
.
NSLTLOCK  FORM      1         0 OR 1=filepi,2=recordlock, 3=no lock
NSLTFLD             DIM       9
NSLTFLD1  DIM       9
NSLTFLAG  FORM      1
NSLTPATH  FORM      1
.
. 
NSLTVARS  LIST
NSLTLIST  DIM       6         1-6       LIST NUMBER
NSLTNUM   DIM       3         7-9       SELECTION CODE NUMBER
NSLTPRICE FORM      5.2    10-17        SELECTION CODE PRICE
NSLTDESC  Dim       3      18-20        SELECTION CODE PRICE MODIFIER
.         "   " OR "000"      NO DESCRIPTION/CALCULATION
.         "001"               /M
.         "002"               /FLAT
.         "003"               /EACH
.         "004"               /MIN
.         "005"               N/A
.         "006"               N/C
.         "007"               SEEBASE
.         "008"               SEEDESC
.         "009"               INQUIRE
NSLTQTY   DIM       9      21-29        SELECTION QUANTITY
.begin patch 1.001
.NSLTDATE DIM       8         30-37       date of count maybe yyyymmdd or yyyymm          
.end patch 1.001
                    LISTEND
