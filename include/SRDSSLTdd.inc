******************************************************
*  SRDSSLTDD INCLUSION
*  SUBSIDIARY FILE FOR DATACARD
*  ADDRESSING FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS SRDS FILE.
* ****************************************************
.
.      FILE:      NISRDSSLT
.    LENGTH:      51
.      TYPE:      ISAM,MANAGED
. INDEX KEY:      1-6         SRDSSLTLIST
......................................................
SRDSSLTNAME  INIT      "SRDS_SLT.ISI|NINS1:502"


SRDSSLTFLIST FILELIST


SRDSSLTFILE  IFILE     KEYLEN=6,FIXED=51,Name="SRDS_SLT.isi|NINS1:502"
                    FILELISTEND
.>Patch 1.1 End               
.
SRDSSLTLOCK  FORM      1         0 OR 1=filepi,2=recordlock, 3=no lock
SRDSSLTFLD   DIM       6
SRDSSLTFLD1  DIM       9
SRDSSLTFLAG  FORM      1
SRDSSLTPATH  FORM      1
. 
SRDSSLTVARS         LIST
SRDSSLTLIST         DIM       6         1-6       LIST NUMBER
SRDSSLTTYPE         DIM       1         7-7       if Known record type ADDRESS/Select/Arrangement/sourse
SRDSSLTNUM          DIM       3         9-10       NIN ADDRESS/Select/Arrangement/sourse CODE NUMBER
SRDSSLTPRICE        FORM      5.2      11-18        SELECTION CODE PRICE
SRDSSLTDESC         DIM       3        19-21        SELECTION CODE DESCRIPTION
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
SRDSSLTDES          Dim       30       22-51 .description from SRDS XML FIle
                    LISTEND
