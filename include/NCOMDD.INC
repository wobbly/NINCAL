;    Last change:  DMB  06/07/02
;..............................................................................
;.
;.NCOMDD INCLUSION
;.LOCATED IN \\NinS1\E\DATA\TEXT
;.FILE NAME : NINCOMM.DAT
;.REC LENGTH: 32
;.INDEX KEY: 1-6 LR
;..............................................................................
;.
NCOMFLE   FILE
NCOMIFILE IFILE     KEYLEN=6,FIXED=48
NCOMFLE2  AFILE     FIXED=48
NCOMNAME  INIT      "NINCOMM"
NCOMFLD   DIM       6      1-6      LR
NCOMFLD1  DIM       9      NCOMAFILE AIM KEY 1 LR
NCOMFLD2  DIM       9      NCOMAFILE AIM KEY 2 Check #
NCOMFLD3  DIM       7      NCOMAFILE AIM KEY 3 YEAR
NCOMFLD4  DIM       9      NCOMAFILE AIM KEY 4 QTR/YEAR
NCOMFLAG  FORM      1
NCOMFLG2  FORM      1
NCOMlock  FORM      1      0 or 1 - filepi, 2 =record locking, = no locks
;.===================================================================================
;.Revenue file
;.Index of revenue file -see above
NCOMVARS    LIST
CLR    DIM   6          1-6                 LR
CPD    FORM  9.2          7-18                     Commission Paid
CNUM   DIM   6          19-24            Check Number
CDAT   DIM   8          25-32            Check Date
CQTRYR DIM   6                             33-38      Quarter Which Paid For
FILLER DIM  10          39-48     BLANK
          listend
;.===================================================================================
