..............................................................................
.
. get next  INCLUSION
. gnxt FILE DEFINITION
.
. FILE NAME : GNXT
. REC LENGTH: 14 FIXED
. INDEX KEY : 1-8 (1-1 CO DESIGNATION, 2-4   'FILE DESCRIPTOR' 5-7 'NXT')
.
..............................................................................
.
GNXTFILE IFILE     KEYLEN=8,FIXED=14,UNCOMP,Name="Gnxt.ISI|NINS1:502"
GNXTNAME INIT      "GNXT.ISI|NINS1:502"
GNXTFLD  DIM       8
GNXTFLAG FORM      1
* ....................................................
.
GNXTKEY  DIM       8         1-8     1-1 COMPANY 'N'
.			             2-4 FILE IE 'ORD'
.			             5-6 NXT
.                                    EXCEPT   NINPI   DATACARD LOCK.
GNXTNUM  DIM	   6         9-14    NEXT NUMBER TO BE USED.
...............................................................................
