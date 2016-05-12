..............................................................................
.
. NMLR2DD INCLUSION
. NIN MAILER NOTES FILE DEFINITION
.
. FILE NAME : NINMLR2
. REC LENGTH: 754
. INDEX KEY : 1-4 
.
.ASH 29DEC2000 - FILE CREATED
..............................................................................
.
NMLR2FILE IFILE    KEYLEN=4,VAR=754,NODUPLICATES
.NMLR2NAME INIT      "NINMLR2.ISI|20.20.30.103:502"
NMLR2NAME INIT      "NINMLR2.ISI"
NMLR2FLD  DIM       4
NMLR2FLAG FORM      1
nMLR2lock form      1             0 or 1 - filepi, 2 =record locking, = no locks
..............................................................................

NMLR2VARS list
MLR2MLR         dim        4        1-4       MLR NUMBER
MLR2NOTES       DIM       750       5-754     MLR NOTES
         listend
