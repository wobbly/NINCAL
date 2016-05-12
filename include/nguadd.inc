..............................................................................
.
. NGUADD INCLUSION
. NIN GUAER FILE DEFINITION
.
. FILE NAME : NINGUAR
. REC LENGTH: 14 FIXED
. INDEX KEY : 1-6 (List order number)
..09sep2000 New
..............................................................................
.
NGUAFILE IFILE     KEYLEN=6
NGUANAME INIT      "NINGUAR"
NGUAFLD   DIM       6
NGUAFLG1 FORM      1
.
GUAvars  list
GUALR    DIM       6         001-006   LIST rental ORDER number
GUADate  DIM       8         007-014   Date CCYYMMDD
         listend
..............................................................................*

