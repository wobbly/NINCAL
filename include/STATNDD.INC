..............................................................................
.
. STATNDD INCLUSION
. STATS NOTES FILE DEFINITION
.
. FILE NAME :	NSTATNOTE
. REC LENGTH:	FIXED
. INDEX KEY : 
..............................................................................
.
STATNFLST	Filelist
STATNFILE	IFILE	KEYLEN=6,FIXED=212,UNCOMP,Name="NSTATNOTE.ISI"
		Filelistend
STATNNAME	INIT	"NSTATNOTE"

STATNFLD	dim	13	1-13 STATNLR + STATNLOL + STATNPACK
STATNFLAG	form	1
STATNPATH	form	1
STATNLOCK	form	1	0 & 1=FILEPI, 2=REcord lock, 3=no lock
.
STATNVARS	list
STATNLR		dim	6	 1-6    LR/LOL number
STATNLOL	dim	1	 7-7    0=LR Record, 1=LOL Record
STATNPACK	dim	6	 8-13   Package Number
STATNNOTE	dim	200	13-212  Record Note
         listend
.END OF INCLUDE
.
..............................................................................
