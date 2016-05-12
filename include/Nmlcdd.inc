;*******************************************************************************
;*  NmLCdd/INC  - VARIABLE INCLUDE FOR MLRCATS FILE.                          *
;*******************************************************************************
;;release     1.1       DLH 29July2005   Mailer conversion
;*  CREATED -  03/18/92     DLH.                                                        *
;*******************************************************************************
;* MLRCATS.TXT
.
. RECORD LENGTH - FIXED 26
.
. INDEXED       - ISAM BYTES 1-6 - Company (MAILER) NUMBER.
.
...............................................................................
.FILE DEFINITION.
.................
;begin patch 1.1
;NMLCFILE IFILE     KEYLEN=6,FIX=26
NMLCFILE IFILE     KEYLEN=4,FIX=24
;end patch 1.1
.
NMLCNAME INIT	   "MLRCATS     "
NMLCFLAG FORM      1
;begin patch 1.1
;NMLCFLD  DIM       4
NMLCFLD  DIM       6
;begin patch 1.1
...............................................................................
.RECORD DESCRIPTION.
....................
;begin patch 1.1
NMLCNUM    DIM       6  1-6   KEY1 - MAILER NUMBER FROM NINMLR FILE.
;NMLCNUM    DIM      46  1-4   KEY1 - MAILER NUMBER FROM NINMLR FILE.
;end patch 1.1
NMLCFIL1   DIM       1  7-7   FILLER USED BETWEEN VARIABLES TO ALLOW FOR SCAN OF
.                           CAT CODES.
NMLCCAT1   DIM       3 9-10   CATEGORY CODE ONE.
NMLCFIL2   DIM       1 11-11
NMLCCAT2   DIM       3 12-14  CATEGORY CODE TWO.
NMLCFIL3   DIM       1 15-15
NMLCCAT3   DIM       3 16-18  CATEGORY CODE THREE.
NMLCFIL4   DIM       1 19-19
NMLCCAT4   DIM       3 20-22  CATEGORY CODE four.
NMLCFIL5   DIM       1 23-23
NMLCCAT5   DIM       3 24-26  CATEGORY CODE five.
.
NMLCCAT    VARLIST   NMLCFIL1:
		     NMLCCAT1:
		     NMLCFIL2:
		     NMLCCAT2:
		     NMLCFIL3:
	             NMLCCAT3:
		     NMLCFIL4:
	             NMLCCAT4:
		     NMLCFIL5:
	             NMLCCAT5

.
*******************************************************************************
