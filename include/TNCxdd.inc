..............................................................................
.
. TNCxDD INCLUSION
. TNC list cross reference Stats FILE DEFINITION
.
. FILE NAME : TNCxref
.release   1.2            4Apr 2000    add market code
.RElease   1.1            13Jan 2000  change tnc key to 4 bytes
.
. REC LENGTH: 76 FIXED
. INDEX KEY : 
..............................................................................
.
.begin patch 1.1
.TNCxFILE  IFILE     KEYLEN=3
TNCxFILE  IFILE     KEYLEN=4
.end patch 1.1
TNCxFLE1  IFILE     KEYLEN=30
TNCxFLE2  IFILE     KEYLEN=6
TNCxNAME  INIT      "TNCXref"             7-10
TNCxNME1  INIT      "TNCXref1"
TNCxNME2  INIT      "TNCXref2"            1-6
.begin patch 1.1
.TNCxFLD   dim       3
TNCxFLD   dim       4
.end patch 1.1
TNCxFLD1  dim       30
tncxfld2  DIM       6
TNCxFLAG  FORM      1
TNCxFLG1  FORM      1
TNCxFLG2  FORM      1
tncxpath  FORM      1
.
TNCxvars  list
TNCxlist  dim       6      1-6          nin LIST #    key 3
.begin patch 1.1
.TNCxLcode DIM       3      7-9
.TNCXFill  DIM       1      10-10
.TNCdesc  dim       35      11-45        nin desc
.TNCdesc1 dim       30      46-75         TNC DESC - Key 2
TNCxLcode DIM       4      7-10                             key 1
TNCXFill  DIM       1      11-11
TNCdesc  dim       35      12-46        nin desc
TNCdesc1 dim       30      47-76         TNC DESC - Key 2
.begin patch 1.2
tncxmrkt DIM       2       77-78        market code
.end patch 1.2
.end patch 1.1
         listend
.....
.TNC pacxackage file
TNCPfile  IFILE     keylen=3,FIXED=159
TNCPname  INIT      "TNCpackage"
tncPfld   DIM        3
tncpflag  FORM       1

tncpvars  list
tncpcode  dim        3          1-3
tncpdesc  dim        150        4-153
tncpcost  FORM       3.2       154-159
          listend
.tncpdesc1  dim        30         4-33
.END OF INCLUDE
.
..............................................................................

