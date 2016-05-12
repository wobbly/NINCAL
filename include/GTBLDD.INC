..............................................................................
.
. GTBLDD INCLUSION
. TABLE FILE DEFINITION
.
. FILE NAME : TABLEFLE
. REC LENGTH:  79 FIXED
. INDEX KEY : 1-8 (TABLE CODE, TABLE DESIGNATION)
.
..............................................................................
.
GTBLFILE IFILE     KEYLEN=8
GTBLNAME INIT      "TABLEFLE"
GTBLFLD  DIM       8
GTBLFLAG FORM      1
.
TBLCODE  DIM       8         001-008   TABLE CODE    - key
TBLTEXT  DIM       71        009-079   TEXT
.
..............................................................................

