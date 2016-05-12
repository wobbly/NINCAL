..............................................................................
.
. NBRK2DD INCLUSION
. NIN BROKER NOTES FILE DEFINITION
.
. FILE NAME : NINBRK2
. REC LENGTH: 754
. INDEX KEY : 1-4 
.
.ASH 29DEC2000 - FILE CREATED
..............................................................................
.
NBRK2FILE IFILE    KEYLEN=4,VAR=754,NODUPLICATES
.NBRK2NAME INIT      "NINBRK2.ISI|20.20.30.103:502"
NBRK2NAME INIT      "NINBRK2.ISI"
NBRK2FLD  DIM       4
NBRK2FLAG FORM      1
nBRK2lock form      1             0 or 1 - filepi, 2 =record locking, = no locks
..............................................................................

NBRK2VARS list
BRK2BRK         dim        4        1-4       BRK NUMBER
BRK2NOTES       DIM       750       5-754     BRK NOTES
         listend
