..............................................................................
.
. NSHPDD INCLUSION
. NIN SHIPMENT FILE DEFINITION
.
. FILE NAME : NINSHP
. REC LENGTH: 136 FIXED
. INDEX KEY : 1-6 (LR NUMBER)
.

.PATCH 1.4 18JUN2005 DMB Changed IP address of File Manager
.PATCH 1.3 09May2003 ASH Added new fields
.PATCH 1.2 increased STrack 09Apr01 DLH
.PATCH 1.1 increased SQTY 10Jun99 ASH
.tracking number added 07oct98 DLh
.date converted 15Jul98 DLH
..............................................................................
.
NSHPFILE IFILE     KEYLEN=6
.NSHPFILE IFILE     KEYLEN=6,FIXED=136
.>Patch 1.4 Begin
.NSHPNAME INIT      "NINSHP.ISI|20.20.30.103:502"
NSHPNAME INIT      "NINSHP.ISI|NINS1:502"
.>Patch 1.4 End
NSHPFLD  DIM       6
NSHPFLAG FORM      1
nshplock form      1         0 OR 1=filepi,2=recordlock, 3=no lock.
.
shpvars  list
SLRNUM   DIM       6         001-006   LR NUMBER
SINFO    DIM       36        007-042   SHIPPING TEXT EXPLANATION
SCODE    DIM       1         043-043   HOW SHIP INFO WAS RECEIVED (C,P,S,I,R,T,A,F,Z,X)
.SDATE    DIM       6         044-049   SHIPMENT DATE     mmddyy
SDATE    DIM       8         044-051   SHIPMENT DATE     ccyymmdd
SPOST    DIM       4         052-055   SHIPPING COST
.START PATCH 1.1 - REPLACED LOGIC
.SQUANT   DIM       7         056-062   SHIP QUANTITY
SQUANT   DIM       9         056-064   SHIP QUANTITY
.END PATCH 1.1 - REPLACED LOGIC
.begin patch 1.2
.Strack   DIM       15        065-079   Tracking number
Strack   DIM       25        065-089   Tracking number
.end patch 1.2
SINITS     DIM       3         090-092   Initials of person who created record
SRDATE     DIM       8         093-100   Date record was created
SPINITS    DIM       3         101-103   Initials of person who Printed record
SPDATE     DIM       8         104-111   Date record was Printed
SFILLER    DIM       25        112-136   Filler
         listend
.
..............................................................................
