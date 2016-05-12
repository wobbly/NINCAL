*******************************************************************************
*  NmLGdd/INC  - VARIABLE INCLUDE FOR NINCAT FILE.                          *
*******************************************************************************
*  CREATED -  03/18/92                                                        *
*******************************************************************************
* NINMCAT.TXT
.
. RECORD LENGTH - VAR 50.
.
. INDEXED       - ISAM BYTES 1-3 - CATEGORY NUMBER.
.
...............................................................................
.FILE DEFINITION.
.................
NMLGFILE  IFILE     KEYLEN=3,VAR=50,COMP
.
NMLGNAME  INIT	   "NINMCAT     "
NMLGFLD   DIM       3
NMLGFLAG  FORM      1
...............................................................................
.RECORD DESCRIPTION.
....................
NMLGKEY  DIM       3  1-3   KEY1 - CATEGORY CODE.
. byte 1 is always a record identifier A,B,C.
. bytes 2-3 are numeric
NMLGFIL  DIM       2  4-5
NMLGCAT  DIM	   45 6-50  DESCRIPTION.
.
