..............................................................................
.
. BrptDD INCLUSION
. Bankruptcy FILE DEFINITION
.
. FILE NAME : BankRuptcy
. REC LENGTH: 12 FIXED
. INDEX KEY : 1-6,7-12 (LR# ,INVOICE#)
.
.Patch 1.0  NEW    July 2008

BrptLIST  	Filelist
BrptFILE 	IFILE     KEYLEN=6,FIX=12,UNCOMP,Name="Bankruptcy.ISI|NINS1:502   "
BrptFLE2 	IFILE     KEYLEN=6,FIX=12,UNCOMP,Name="Bankruptcy2.ISI|NINS1:502   "
	Filelistend
BrptFLSQ 	FILE
BrptNAME 	INIT      "Bankruptcy.ISI|NINS1:502   "
BrptNAMESEQ INIT      "Bankruptcy.Dat|NINS1:502  "
BrptFLD  	DIM       6
BrptFLD1 	DIM       6
BrptFLAG 	FORM      1
BrptPath 	FORM      1                  "1" by lr , "2" by inv inv not implemented yet
.
Brptvars  	list
BrptLR	DIM       6         001-006   LR #
BrptINv	DIM       6         007-012   INV #
         	listend

