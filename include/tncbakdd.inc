..............................................................................
.
. TNCBAKDD INCLUSION
. NIN BROKER NOTES FILE DEFINITION
.
. FILE NAME : TNCBACK
. REC LENGTH: 103
. INDEX KEY : 1 - 25
.
.File Created 8/15/03
..............................................................................
.
TNCBAKFILE IFILE    KEYLEN=25
TNCBAKNAME INIT      "TNCBACK.ISI"
TNCBAKFLD  DIM       25
TNCBAKFLAG FORM      1
TNCBAKlock form      1             0 or 1 - filepi, 2 =record locking, = no locks
..............................................................................

TNCBAKVARS list
TNCBAKSOURCEKEY	dim	25
TNCBAKPKGDESC	dim	60
;TNCBAKCONST	INIT	"     0.70"	
TNCBAKCONST	form	6.2	
TNCBAKCOST	form	6.2
         listend

tncbakCOSTCONST	INIT	"0.70"
