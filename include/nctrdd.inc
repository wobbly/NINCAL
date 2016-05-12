..............................................................................
.
. NCTRDD INCLUSION
.
. FILE NAME : CONTROLS.DAT
. REC LENGTH: 43
. INDEX KEY :  1-11 
. AAMDEX KEY : 1-3
.
.PATCH 1.2 DLH INCREASED Record added compid for PLI
.PATCH 1.1 ASH INCREASED ISAM, ADDED AAM
..............................................................................
.
.NCTRFILE  IFILE     KEYLEN=3,FIXED=42,NODUPLICATES
NCTRLIST  Filelist
NCTRFILE IFILE  NAME="CONTROLS.ISI|NINS1:502"
NCTRFLE2 AFILE  NAME="CONTROLS.AAM|NINS1:502"
          Filelistend
NCTRNAME  INIT      "CONTROLS"
NCTRFLD   DIM       11
NCTRFLD1  DIM       6
NCTRFLAG  FORM      1
nCTRlock  form      1             0 or 1 - filepi, 2 =record locking, = no locks
..............................................................................

NCTRVARS  	list
NCTRNUM   	dim       3       	 1-3        .CONTROL NUMBER (KEY/AAMKEY)
NCTRDATE  	DIM       8       	 4-11       .CONTROL DATE - CCYYMMDD (KEY)
NCTRAMT   	FORM      12.2    	12-26       .CONTROL AMOUNT
NCTRCODE  	FORM      1       	27-27       .CONTROL CODE - "1" Edit was run - limited modification
.                                               .       "2" Checks were run - no modification at all!!!
NCTRAMT2  	FORM      12.2    	28-42	.SECONDARY CONTROL AMOUNT
.begin patch 1.2
NCTRCOMP	Dim	1 	43-43	.COmpany ID  "N" or " " = NIN "P" = PLI		                
.end patch 1.2

         	listend
