...............................................................................
.
. NSPE3DD INCLUSION
. PLI ORDER SPECIAL INSTRUCTIONS FILE DEFINITION
.
. NOTES:  THIS FILE STORES TEXT, SELECTS, MEDIA, SHIP VIA
.         FOR PLI LEGACY RECORDS - CREATED IN STATWARE SYSTEM
.
. FILE NAME : NINSPEC3
. REC LENGTH: 3006
. INDEX KEY : 1-6 
..............................................................................
.
NSPE3FILE IFILE     KEYLEN=6,fixed=3006
NSPE3NAME INIT      "NINSPEC3.ISI|NINS1:502"
.NSPE3NAME INIT      "NINSPEC3.ISI"
NSPE3FLD  DIM       6
NSPE3FLAG FORM      1
nspe3lock form      1             0 or 1 - filepi, 2 =record locking, = no locks
..............................................................................

NSPE3VARS	list
NSPELR3		dim	6	1-6       LR NUMBER: TYPE + PLI PO_NUM
DESC005		DIM	3000	7-3006    NON-MODIFIABLE INSTRUCTIONS
		listend