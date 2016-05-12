..............................................................................
.
. NCKIDD INCLUSION
. NIN MANUAL CHECK FILE DEFINITION
.
.CREATED 12JUL93
.
. FILE NAME : CHKINFO
. REC LENGTH: 44 FIXED
.
. AIM KEYS   : 01-06 INVOICE NUMBER
.begin patch 1.1
..              08-13 OLD CHECK NUMBER         (old values)
..              20-25 NEW CHECK NUMBER         (old values)
.              08-13 OLD CHECK NUMBER         (new values)
.              22-27 NEW CHECK NUMBER         (new values)
.end patch 1.1
.patch 1.1  27 DLH NININV Y2k
..............................................................................
.
.begin patch 1.1
.NCKIFILE FILE     FIXED=45
.NCKIFLE1 AFILE     FIXED=45
NCKIFILE FILE     FIXED=75
NCKIFLE1 AFILE     FIXED=74
.end patch 1.1
NCKINAME INIT      "CHKINFO  "
.
NCKIFLD   DIM       9
NCKIFLD2  DIM       9
NCKIFLD3  DIM       9
NCKIFLAG FORM      1
NCKIFLG1 FORM      1
NCKIPATH FORM      1
.
NCKIVARS LIST
NCKIINV  DIM       6         001-006    INVOICE NUMBER REQUIRED, KEY 1.
NCKISTAT FORM      1         007-007   4="MANUAL ISSUED", 3="VOID".
.                                      2=VOID/CONTRA, 1=VOID/REISSUE.
.                                      5="MANUAL ISSUED ON OPEN INV"
NCKIONUM DIM       6         008-013   CHECK NUMBER VOIDED,   KEY 2.
.begin patch 1.1
.NCKIDTE  DIM       6         014-019   DATE OF OLD CHECK.
NCKIDTE  DIM       8         014-021   DATE OF OLD CHECK. ccyymmdd
.end patch 1.1
NCKINNUM DIM       6         022-027   NEW CHECK NUMBER, KEY 3.
.NCKIDATE DIM       6         026-031        DATE ENTERED
.begin patch 1.1
NCKIDATE DIM       8         028-035        DATE ENTERED
.NCKIAP1  DIM       7         037-042    A/P1
.NCKIAP2  DIM       7         043-049    A/P2
NCKIAP1  form       10.2         037-048    A/P1
NCKIAP2  form       10.2         048-061    A/P2
NCKIAP3  form       10.2         062-074    A/P2
.end patch 1.1
	 LISTEND
..............................................................................*
         LISTOFF
.PROGRAM ACCESS.
.NAME           TYPE          ACCESS
         LISTON

