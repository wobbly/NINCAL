..............................................................................
.
. NBILDD INCLUSION
. NIN BILL-TO FILE DEFINITION
.
. FILE NAME : NINBIL
. REC LENGTH: 130 FIXED
. INDEX KEY : 2-9 (4 POSITION MLR#, 3 POSITION CONTACT#, 1 POSITION BILL#)
.
..............................................................................
.
NBILFILE IFILE     KEYLEN=8,FIX=130
	 IFNZ      PC
NBILFIL2 AFILE	   FIX=130
	 XIF
	 IFZ       PC
.NBILFIL2 AFILE	   51,1,,FIX=130
NBILFIL2 AFILE	   FIX=130
	 XIF
NBILFLE3 FILE      FIXED=130
NBILNAME INIT      "NINBIL      "
NBILFLD  DIM       8
NBILFLAG FORM      1
NBILFLD1 DIM       53
NBILFLG2 FORM      1
NBILFLG3 FORM      1
NBILPATH FORM	   1
.
BILFIL1  DIM       1         001-001   FILLER
BILMLR   DIM       4         002-005   MAILER COMPANY NUMBER
BILCNT   DIM       3         006-008   MAILER CONTACT NUMBER
BILCODE  DIM       1         009-009   BILL-TO NUMBER
BILNAME  DIM       25        010-034   CONTACT NAME
BILCOMP  DIM       25        035-059   COMPANY NAME
BILADDR  DIM       25        060-084   ADDRESS
BILCITY  DIM       15        085-099   CITY
BILSTATE DIM       2         100-101   STATE
BILZIP   DIM       10        102-111   ZIP CODE
BILUSER  DIM       10        112-121   LAST USER TO MODIFY
BILRVDTE DIM       6         122-127   REVISED DATE
BILFIL3  DIM       3         128-130   FILLER
.
..............................................................................

