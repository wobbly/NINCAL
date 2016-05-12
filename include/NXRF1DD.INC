******************************************************************************
*VARIABLE INCLUSION FOR DIAL IN BROKER/MAILER REF FILE.
******************************************************************************
* REVISION:
*           16JUN98 DLH ADDED ID CODE  FOR GREATER SECURITY ON BBS.
*
*              FILE: brk_MLR
*       RECORD SIZE:
*          COMPRESS: NONE
*              TYPE: ISAM
*               KEY: 1 ID & BROKER NUMBER  1-7
*               KEY: 2 MAILER NUMBER  8-13 -
******************************************************************************
. Patch 1.1  ASH  15MAR2005  Mailer/Broker field conversion
. Note that this file is no longer used.  Conversion is merely for protocol
.
.START PATCH 1.1 REPLACED LOGIC
.NXRF1FLE  IFILE     KEYLEN=8
..
.NXRF1FLG  FORM      1
.NXRF1PTH  FORM      1
.NXRF1NME  DIM       8
.NXRF1NAM  INIT      "BRK_MLR  "
..
.NXRF1FLD   DIM       9
..
.NXRF1ID    DIM       1     1-1       id B=BRK.CONSULT  M=MAILER
.NXRF1BRK   DIM       4     2-5       BROKER/CONSULTANT  OR MAILER
.NXRF1MLR   DIM       4     6-9      MAILER NUMBER
.NXRF1DTE   DIM       8    11-17     DATE ADDED
.NXRF1USR   DIM      10    21-30     WHOM
................................
NXRF1FLE  IFILE     KEYLEN=12
.
NXRF1FLG  FORM      1
NXRF1PTH  FORM      1
NXRF1NME  DIM       8
NXRF1NAM  INIT      "BRK_MLR|NINS1:502"
.
NXRF1FLD   DIM       13
.
NXRF1ID    DIM       1     1-1       id B=BRK.CONSULT  M=MAILER
NXRF1BRK   DIM       6     2-7       BROKER/CONSULTANT  OR MAILER
NXRF1MLR   DIM       6     8-13      MAILER NUMBER
NXRF1DTE   DIM       8    14-21      DATE ADDED
NXRF1USR   DIM      10    22-31     WHOM
.END PATCH 1.1 REPLACED LOGIC
..............................................................................
