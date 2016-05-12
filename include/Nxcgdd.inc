;    Last change:  JS    9 Nov 1999   12:59 pm
******************************************************************************
*VARIABLE INCLUSION FOR EXCHANGE charges FILE.
******************************************************************************
*              FILE: excharge
*       RECORD SIZE: 66
*          COMPRESS: NONE
*              TYPE: ISAM
*               KEY: 1 lr  1-6
******************************************************************************
.release   1.0      09nov99 jd updated qty,cc
EXCHARGE  IFILE     KEYLEN=6,FIX=66,dupe
.
NXCGNAME  INIT      "EXCHARGE"
NXCGFLAG  FORM      1
.
NxcgFLD   DIM       6
.
nXCGLR       DIM       6        1-6  KEY= LR Position 1-6
NXCGMLR      DIM       4        7-10  MAILER
NXCGMCT      DIM       3       11-13         MAILER/CONTACT NUMBER.
NXCGLN       DIM       6       14-19   LIST NUMBER
NXCGOWN      DIM       4       20-23   OWNER
NXCGTPI      DIM       6       24-29
NXCGQTY      FORM      9       30-38   QUANTITY
NXCGAR       FORM      8.2     39-49   AR
NXCGAP       FORM      8.2     50-60   AP
nxcgcc       DIM       2       61-62
NXCGYR       DIM       2       63-64   YEAR
NXCGMO       DIM       2       65-66   MONTH
..............................................................................

