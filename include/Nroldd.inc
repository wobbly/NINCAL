* *****************************************************************************
* NAMES IN THE NEWS rolodex  FILE.
* *****************************************************************************
.
.    FILE:      ROLODEX
.  LENGTH:      244
.COMPRESS:      COMPRESSED
.    TYPE:      AIM
.    KEY1:      NROLNAME & addr1 7-66
.    KEY2:      NROLNOTE 157-186
...............................................................................
.NROLFILE AFILE    63,2,,COMP,VAR=242
.NROLFILE AFILE     COMP,VAR=242
NROLFILE AFILE     COMP,VAR=244
NROLNAME INIT      "ROLODEX  "
NROLFLD  DIM       63
NROLFLD2 DIM       33
NROLFLAG FORM      1
* 
NROLVARS LIST
.BEGINNING OF LIST.
NROLUSER  DIM       6      1-6    USER NUMBER. NOT CURRENTLY USED
NROLNAM   DIM       30     7-36   NAME          --  KEY1.
NROLADR1  DIM       30    37-66   ADDRESS LINE 1--/
NROLADR2  DIM       30    67-96   ADDRESS LINE 2
NROLADR3  DIM       30    97-126  CITY/STATE
NROLZIP   DIM       10   127-136  BROKER CITY.
NROLPHON  DIM       20   137-156  PHONE
NROLNOTE  DIM       30   157-186  NOTES KEY2.
NROLFAX   DIM       20   187-206  FAX NUMBER.
.nroldate  dim        6   207-212  last update date
nroldate  dim        8   207-214  last update date
NROLADR4  DIM       30   215-244  email address
          LISTEND
.
