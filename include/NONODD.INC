..............................................................................
.
. NONODD INCLUSION
. NIN ORDER NOTES FILE DEFINITION
.
. FILE NAME : ORDNOTES
. REC LENGTH: 521 VARIABLE
. INDEX KEY : 1- (6 POSITION LIST ORDER #, DUPLICATES ALLOWED)
.
..............................................................................
.
.		PATCH 1.2 DMS 13JUN2006  Replaced NLINEX vars with single NLINE VAR.
.			  Associated with nordtest patch "3.78.3"
.		          Also converted ordnotes file.
;		PATCH 1.1 DMB 18JUN2005  Changed IP of File Manager
.START PATCH 1.2	REPLACE LOGIC
.NONOFILE IFILE     KEYLEN=6,VAR=381
NONOFILE IFILE     KEYLEN=6,VAR=521
.END PATCH 1.2	REPLACE LOGIC
.START PATCH 1/8/2003 ASH - ADDED TO FILE MANAGER
.NONONAME INIT      "ORDNOTES "
.>Patch 1.1 Begin
.NONONAME INIT      "ORDNOTES.isi|20.20.30.103:502"
NONONAME INIT      "ORDNOTES.isi|NINS1:502"
.>Patch 1.1 End
.END PATCH 1/8/2003 ASH - ADDED TO FILE MANAGER
.
NONOFLD  DIM       6
NONOFLAG FORM      1
******************************************************************************
.
.START PATCH 1.2	ADDED LOGIC
ORDNOTESVARS	LIST
.END PATCH 1.2	ADDED LOGIC
NOTEKEY  DIM       6      1-6           *LR NUMBER KEY.
NDATE    DIM       8      7-14          *DATE OF NOTE. MMDDYYCC
NTIME    DIM       4     15-18       *TIME OF NOTE  HHMM
.START PATCH 1.2	REPLACED LOGIC
.NLINE1   DIM       60    19-78
.NLINE2   DIM       60    79-138
.NLINE3   DIM       60   139-198
.NLINE4   DIM       60   199-258
.NLINE5   DIM       60   259-318
.NLINE6   DIM       60   319-378
.NINITS   DIM       3    379-381
NLINE	 DIM	   500	 19-518
NINITS   DIM       3     519-521
.END PATCH 1.2	REPLACED LOGIC
.START PATCH 1.2	ADDED LOGIC
	LISTEND
.END PATCH 1.2	ADDED LOGIC
.

