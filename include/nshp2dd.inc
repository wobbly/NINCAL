..............................................................................
.
. NSHP2DD INCLUSION
. NIN SUPPLEMENTAL SHIPMENT FILE DEFINITION
.
. FILE NAME : NINSHP2
. REC LENGTH: 1006 FIXED
. INDEX KEY : 1-6 (LR NUMBER)
.
.05/1/2003 ASH Created
..............................................................................
.
NSHP2FILE IFILE     KEYLEN=6
.NSHPFILE IFILE     KEYLEN=6,FIXED=1006
NSHP2NAME INIT      "NINSHP2  "
NSHP2FLD  DIM       6
NSHP2FLAG FORM      1
nshp2lock form      1         0 OR 1=filepi,2=recordlock, 3=no lock.
.
shp2vars	list
SLRNUM2		DIM       6         001-006   LR NUMBER
SNOTES2		DIM       500       007-506   INTERNAL SHIPPING NOTES
SNOTES2B	DIM       500       507-1006  EXTERNAL SHIPPING NOTES
		listend
.
..............................................................................
