..............................................................................
.
. NSPE2DD INCLUSION
.
. FILE NAME : NINSPEC2
. REC LENGTH: 1006
. INDEX KEY : 1-6 
..............................................................................
.
NSPE2FILE IFILE     KEYLEN=6,VAR=1006,COMP,NODUPLICATES
.NSPE2NAME INIT      "NINSPEC2"
.NSPE2NAME INIT      "NINSPEC2.ISI|20.20.30.103:502"
NSPE2NAME INIT      "NINSPEC2.ISI|NINS1:502"
NSPE2FLD  DIM       6
NSPE2FLAG FORM      1
nspe2lock form      1             0 or 1 - filepi, 2 =record locking, = no locks
..............................................................................

NSPE2VARS list
NSPE2LR   dim        6        1-6        LR NUMBER
DESC003   DIM      500        7-506      INTERNAL NOTES
DESC004   DIM      500      507-1006     MAILER NOTES
         listend
.
