******************************************************
*  NMODDD INCLUSION
*  PRICE MODIFIER FILE FOR DATACARD SUBSIDIARY FILES
*  PRICE MODIFIER FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS MASTER PRICE MODIFIER FILE.
* ****************************************************
.
.      FILE:      NINMOD
.    LENGTH:      43
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-3         NMODNUM
.AIMDEX KEY:      4-23        NMODDESC
......................................................
NMODNAME  INIT      "NINMOD"
NMODFLIST FILELIST
.NMODFILE IFILE     KEYLEN=9,FIXED=43,Name="NINMOD.isi"
.NMODFLE2 AFILE     FIXED=43,Name="NINMOD.aam"
. dlh ??????????? NMODFILE  IFILE     KEYLEN=9,FIXED=43,Name="NINMOD.isi|NINS1:502"
NMODFILE  IFILE     KEYLEN=3,FIXED=43,Name="NINMOD.isi|NINS1:502"
NMODFLE2  AFILE     FIXED=43,Name="NINMOD.aam|NINS1:502"
                    FILELISTEND
.
NMODLOCK  FORM      1         0 OR 1=filepi,2=recordlock, 3=no lock
NMODFLD   DIM       3
NMODFLD1  DIM       23
NMODFLAG  FORM      1
NMODPATH  FORM      1
.
NMODVARS  LIST
NMODNUM             DIM       3         1-3       PRICE MODIFIER NUMBER
NMODDESC  DIM       20      4-23        PRICE MODIFIER DESCRIPTION
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
NMODCALC  DIM       20     24-43        PRICE MODIFIER CALCULATION
.         "   " OR "000"      NO DESCRIPTION/CALCULATION
.         "001"               /1000
.         "002"
.         "003"               *1
.         "004"
.         "005"
.         "006"               *0
.         "007"
.         "008"
.         "009"
                    LISTEND
