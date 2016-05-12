******************************************************
*  NSEL3DD INCLUSION
*  SUBSIDIARY FILE FOR NINORD
*  ORDER PRICING FILE.
* ****************************************************
* NAMES IN THE NEWS SECONDARY ORDER PRICING FILE.
* ****************************************************
.
.      FILE:      NINSEL3
.    LENGTH:      47
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-11        NSEL2LRCODE + NSEL3LR + NSEL3CODE + NSEL3NUM
.AIMDEX KEY:      1-7   NSEL2LRCODE + NSEL3LR
.
.
.	Patch 1.0 ASH	Added vars to allow tracking of Price Mod vars - thus separating this file from NADDDD.INC & NSLTDD.INC
......................................................
NSEL3FNAME          INIT      "NINSEL3"
.
NSEL3FLIST          Filelist
.NSEL3FILE          IFILE     KEYLEN=11,FIXED=47,Name="NINSEL3.isi"
.NSEL3FLE2          AFILE     FIXED=47,Name="NINSEL3.aam"
.NSEL3FILE          IFILE     KEYLEN=11,FIXED=47,Name="NINSEL3.isi|20.20.30.103:502"
.START PATCH 1.0 REPLACED LOGIC
.NSEL3FILE IFILE     KEYLEN=11,FIXED=47,Name="NINSEL3.isi|NINS1:502"
NSEL3FILE IFILE     KEYLEN=11,FIXED=60,Name="NINSEL3.isi|NINS1:502"
.END PATCH 1.0 REPLACED LOGIC
.NSEL3FLE2          AFILE     FIXED=47,Name="NINSEL3.aam|20.20.30.103:502"
.START PATCH 1.0 REPLACED LOGIC
.NSEL3FLE2 AFILE     FIXED=47,Name="NINSEL3.aam|NINS1:502"
NSEL3FLE2 AFILE     FIXED=60,Name="NINSEL3.aam|NINS1:502"
.END PATCH 1.0 REPLACED LOGIC
                    filelistend
.
NSEL3LOCK FORM      1         0 OR 1=filepi,2=recordlock, 3=no lock
NSEL3FLD  DIM       11
NSEL3FLD1 DIM       10
NSEL3FLAG FORM      1
NSEL3PATH FORM      1
.
. 
NSEL3VARS LIST
NSEL3LRCODE         DIM       1         1-1       INDICATES IF LR OR LOL RECORD - '1'=LR, '2'=LOL
NSEL3LR             DIM       6         2-7       LR NUMBER (KEY)
NSEL3CODE           DIM       1         8-8       PRICING CODE = 'A' or 'L'
NSEL3NUM            DIM       3         9-11      PRICING NUMBER = 3 digit Number
NSEL3PRICE          FORM      5.2    12-19        PRICE AT RECORD CREATION
NSEL3DATE           DIM       8      20-27        CREATION DATE

.START PATCH 1.0 REPLACED LOGIC
.NSEL3FILLER         DIM       20     28-47        FILLER
NSEL3MODNUM         DIM       3      28-30        PRICE MODIFIER NUMBER
NSEL3MODCALC        DIM       20     31-50        PRICE MODIFIER CALCULATION
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
NSEL3FILLER         DIM       10     51-60        FILLER
.END PATCH 1.0 REPLACED LOGIC
                    LISTEND