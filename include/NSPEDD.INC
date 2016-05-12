...............................................................................
.
. NSPEDD INCLUSION
. NIN ORDER 00/99/98 FILE DEFINITION
.
. FILE NAME : NINSPEC
. REC LENGTH: 758       used to be 288 COMP
. INDEX KEY : 1-6 
.PATCH #1.2 - DMB 18JUN05 Changed IP Address of File Manager
. Last update add nspelock DLH 10Jun98
.PATCH #1.1 - ASH 04May99 - File conversion/increase
..............................................................................
.
NSPEFILE IFILE     KEYLEN=6,VAR=758,COMP,NODUPLICATES
.NSPENAME INIT      "NINSPEC "
.>Patch 1.2 Begin
.NSPENAME INIT      "NINSPEC.ISI|20.20.30.103:502"
NSPENAME INIT      "NINSPEC.ISI|NINS1:502"
.>Patch 1.2 End
NSPEFLD  DIM       6
NSPEFLAG FORM      1
nspelock form      1             0 or 1 - filepi, 2 =record locking, = no locks
..............................................................................

NSPEVARS list
NSPELR   dim        6        1-6       LR NUMBER
DESC001  DIM       78       7-84       NON-MODIFIABLE XSTAT   
DESC002  DIM      674      85-758      MODIFIABLE INSTRUCTIONS
         listend
.
.desc002 gets parsed into 14 55 byte lines ???

