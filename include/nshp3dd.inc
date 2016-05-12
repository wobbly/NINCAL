..............................................................................
.
. NSHP3DD INCLUSION
. NIN Inventory FILE DEFINITION
.
. FILE NAME : NINSHP3
. REC LENGTH: 336 FIXED
. INDEX KEY : 1-6 (LR NUMBER)
.
..............................................................................
.
NSHP3FLIST       Filelist
NSHP3FILE IFILE     KEYLEN=6,FIXED=336,Name="NINSHP3.ISI|NINS1:502 "                    
	FILELISTEND

NSHP3NAME INIT      "NINSHP3.ISI|NINS1:502"

NSHP3FLD  DIM       6
NSHP3FLAG FORM      1
NSHP3lock form      1         0 OR 1=filepi,2=recordlock, 3=no lock.
.
NSHP3VARS  list
NSHP3LR   	DIM       6	001-006   LR Number
NSHP3RQTY 	DIM       9	007-015   Received Qty
NSHP3Date 	DIM       8	016-023   List Received Date
NSHP3DQTY 	DIM       9	024-032   Declared Qty (The quantity the list owner stated was available)
NSHP3Comments	DIM	255	032-286   Comments
NSHP3FILLER	DIM      50     287-336   Filler
         listend
..............................................................................
