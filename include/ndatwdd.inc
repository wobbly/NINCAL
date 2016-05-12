******************************************************
*  DATACARD WEB FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS DATACARD WEB FILE.
* KEEPS TRACK OF FILES CURRENTLY LOADED ON OUR WEBSITE
* DYNAMICALLY REFRESHED WEEKLY BY NDAT002W.PLS
* ****************************************************
.
.      FILE:      NINDATW
.    LENGTH:      6
.  COMPRESS:      SPACE
.      TYPE:      ISAM,MANAGED
. INDEX KEY:      1-6
......................................................
......................................................
NDATWNAME	DIM	8
NDATWNME1	INIT	"NINDATW "
.
NDATWFLIST	FILELIST
NDATWFILE	IFILE	KEYLEN=6,FIXED=20,Name="NINDATW.ISI|NINS1:502"
	FILELISTEND
NDATWLOCK	FORM	1		0 OR 1=filepi,2=recordlock, 3=no lock
NDATWFLD	DIM	6
NDATWFLAG	FORM	1
NDATWPATH	FORM	1
.
NDATWVARS	LIST
NDATWLST	DIM	6       1-6    ZERO FILLED KEY.
NDATWFILL	DIM	14      7-20   FILLER.
		LISTEND
