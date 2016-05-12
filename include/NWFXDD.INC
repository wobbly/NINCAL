..............................................................................
.
. NWFxDD INCLUSION
. NWF list cross reference Stats FILE DEFINITION
.
. FILE NAME : NWFxref
. REC LENGTH: xxx FIXED
. INDEX KEY : 
..............................................................................
.
NWFxFILE IFILE     KEYLEN=30
NWFxNAME INIT      "NWFXref"
NWFxFLD   dim       30
NWFxFLAG FORM      1
.
NWFxvars list
nwfxlist dim       6      1-6
mwfxstat dim       1      7-7        mm,dd,cc,yy   maildate 
nwfdesc  dim       35     8-42        nin desc
nwfdesc1 dim       30    43-72         NWF DESC - Key
         listend
.END OF INCLUDE
.
..............................................................................

