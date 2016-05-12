******************************************************
*  NINTDD INCLUSION
*  INTEGRAL CLIENT RELATIONSHIP FILE
. ....................................................
* ****************************************************
* NAMES IN THE NEWS MASTER INTEGRAL/CLIENT FILE.
* ****************************************************
.
.      FILE:      NININT
.    LENGTH:      6
.      TYPE:      ISAM,MANAGED
. INDEX KEY:      1-6	NINTCLIENT
......................................................
NINTNAME	INIT	"NININT"
NINTFLIST	FILELIST
NINTFILE	IFILE	KEYLEN=6,FIXED=31,Name="NININT.isi|NINS1:502"
		FILELISTEND
.
NINTLOCK	FORM	1	0 OR 1=filepi,2=recordlock, 3=no lock
NINTFLD		DIM	6
NINTFLAG	FORM	1
NINTPATH	FORM	1
.
NINTVARS	LIST
NINTCLIENT	DIM	6	1-6	LIST NUMBER
NINTFILLER	DIM	25	6-31	FILLER
		LISTEND
