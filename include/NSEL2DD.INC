******************************************************
*  NSEL2DD INCLUSION
*  SUBSIDIARY FILE FOR NINORD
*  SELECT NAME/PRICE/UNIVERSE STORAGE FILE.
* ****************************************************
* NAMES IN THE NEWS SECONDARY LIST SELECT FILE.
* ****************************************************
.
.      FILE:      NINSEL2
.    LENGTH:      176
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:       1-7        NSEL2CODE,NSEL2LR
.AIMDEX KEY:      12-86 NSEL2NAME
.
......................................................
NSEL2FNAME          INIT      "NINSEL2"
.
NSEL2FLIST          Filelist
.NSEL2FILE          IFILE     KEYLEN=7,FIXED=176,Name="NINSEL2.isi"
.NSEL2FLE2          AFILE     FIXED=176,Name="NINSEL2.aam"
NSEL2FILE IFILE     KEYLEN=7,FIXED=176,Name="NINSEL2.isi|NINS1:502"
.NSEL2FILE          IFILE     KEYLEN=7,FIXED=176,Name="NINSEL2.isi|20.20.30.103:502"
NSEL2FLE2 AFILE     FIXED=176,Name="NINSEL2.aam|NINS1:502"
.NSEL2FLE2          AFILE     FIXED=176,Name="NINSEL2.aam|20.20.30.103:502"
                    filelistend
.
NSEL2LOCK FORM      1         0 OR 1=filepi,2=recordlock, 3=no lock
NSEL2FLD  DIM       7
NSEL2FLD1 DIM       78
NSEL2FLAG FORM      1
NSEL2PATH FORM      1
.
. 
NSEL2VARS LIST
NSEL2CODE           DIM       1         1-1       INDICATES IF LR OR LOL RECORD - '1'=LR, '2'=LOL
NSEL2LR             DIM       6         2-7       LR NUMBER (KEY)
NSEL2NUM            DIM       4         8-11      SELECT NUMBER
NSEL2NAME           DIM       75     12-86        SELECT NAME
NSEL2PRICE          FORM      5.2    87-94        ACTUAL BASE PRICE WHEN ORDERED
NSEL2PRICE2         FORM      5.2    95-102       DEFAULT BASE PRICE WHEN ORDERED
NSEL2SPRICE         FORM      5.2    103-110      ACTUAL SELECT PRICE WHEN ORDERED
NSEL2SPRICE2        FORM      5.2    111-118      DEFAULT SELECT PRICE WHEN ORDERED
NSEL2APRICE         FORM      5.2    119-126      ADJUSTMENT MADE TO TOTAL OF NSEL3DD ITEMS - OBSOLETE AS OF MARCH 19, 2004
NSEL2QTY            FORM      10     127-136      ACTUAL UNIVERSE WHEN ORDERED
NSEL2QTY2           FORM      10     137-146      DEFAULT UNIVERSE WHEN ORDERED
NSEL2DATE           DIM       8      147-154      CREATION DATE
NSEL2PRCD           DIM       1      155-155      PRICE CODE (IF APPLICABLE)
NSEL2DESC           DIM       3      156-158      ACTUAL PRICE MODIFIER
NSEL2DESC2          DIM       3      159-161      DEFAULT PRICE MODIFIER
NSEL2FILLER         DIM       15     162-176      FILLER
                    LISTEND
