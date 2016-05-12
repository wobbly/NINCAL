..............................................................................
.
. NlobDD INCLUSION
. NIN list owner balance FILE DEFINITION
.
. FILE NAME : NINlob
. REC LENGTH: 28 FIXED
. INDEX KEY : 1-16
..............................................................................
.
NlobFILE IFILE     KEYLEN=16,FIXED=28
NlobNAME INIT      "NINlob  "
NlobFLD  DIM       16
NlobFLAG FORM      1
NlobPATH FORM      1
.
lobLON   DIM       4         001-004   owner NUMBER
loblist  dim       6         005-010   list  number
lobCC    DIM       2         011-012   century
lobYY    DIM       2         013-014   year
lobMM    DIM       2         015-016   month
lobbal   form      9.2       017-028   balance
..............................................................................*
         LISTOFF
.PROGRAM ACCESS.
.NAME           TYPE          ACCESS
         LISTON

