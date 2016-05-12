******************************************************************************
*VARIABLE INCLUSION FOR EXCHANGE LIST/MAILER REF FILE.
******************************************************************************
*              FILE: LISTMLR
*       RECORD SIZE: 12
*          COMPRESS: NONE
*              TYPE: ISAM
*               KEY: 1 LIST NUMBER  1-6
*	        KEY: 2 MAILER NUMBER 7-12 - DUPLICATES ALLOWED
******************************************************************************
. Patch 1.1  ASH  15Mar2005  Mailer field conversion
.
NXRFFLst Filelist
.START PATCH 1.1 REPLACED LOGIC
.NXRFFILE  IFILE     KEYLEN=6,FIX=10,Name="LISTMLR"
.NXRFFIL2  IFILE     KEYLEN=4,FIX=10,DUP,Name="LISTMLR1"
NXRFFILE  IFILE     KEYLEN=6,FIX=12,Name="LISTMLR.ISI|NINS1:502"
NXRFFIL2  IFILE     KEYLEN=6,FIX=12,DUP,Name="LISTMLR1.ISI|NINS1:502"
.END PATCH 1.1 REPLACED LOGIC
         Filelistend
.
NXRFFLAG  FORM      1
NXRFFLG2  FORM     1
NXRFPATH FORM      1
NXRFNAME DIM       8
NXRFNAM1  INIT      "LISTMLR  "
NXRFNAM2  INIT     "LISTMLR1 "
.
NXRFFLD   DIM       6
NXRFFLD2 DIM       6
.
NXRFLIST  DIM       6     1-6       LIST NUMBER KEY
NXRFMLR   DIM       6     7-12      MAILER NUMBER
..............................................................................
