;    Last change:  AH   19 Aug 1998   11:25 am
..............................................................................
.
. STABBDD INCLUSION
. STATE ABBREVIATION FILE DEFINITION
.
. FILE NAME : STABBREV
. REC LENGTH: 20 FIXED. INDEX KEY : 1-2 (2 POSITION STATE ABBREVIATION)
.
. 
..............................................................................
.
NSTFILE  IFILE     KEYLEN=2,FIXED=20,UNCOMP
NSTNAME  INIT      "STABBREV"
.NSTFLE2 AFILE
.NSTFLE3 FILE      FIXED=20
NSTFLD  DIM       2
.NSTFLD2 DIM       48
.NSTFLAG FORM      1
.NSTFLG2 FORM      1
.NSTFLG3 FORM      1
.NSTPATH FORM      1
.NSTLOCK FORM      1    0 or 1=File Locks, 3=Record Locks, 3=No Locks
.
...............................................................................
* 
stvars  list
STABB   DIM       2      1-2    STATE ABBREVIATION. **KEY**
STNAME  DIM       18     3-20   STATE NAME
         listend
.
        LISTOFF
.PROGRAM ACCESS.
.F:\LIBRARY\PLB_SRC
.NAME           TYPE            ACCESS

.F:\LIBRARY\DEVELOP
.NAME           TYPE            ACCESS


.F:\APPS\PLB\CODE
.NAME           TYPE            ACCESS

        LISTON        
