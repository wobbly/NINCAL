.**************************************************************************************************************************************************************
.* NCRCDD.INC - VARIABLE INCLUDE FOR NINORD3 - CORRECTION/CANCELATION FILE.
.*******************************************************************************
.* LENGTH:    18
.*
.* ACCESS:    ISI
.* KEY    :   1-6     NCRCKEY
.* key 2      7-14    date
.*revisions
.*07Apr98 DLH    add second key by date,CHANGE LAYOUT, ADD LOCK OPTIONS
.*               year 2000 compliance.
.. Patch 1.2 Changed IP address of File Manager DMB 18JUN04
.. Patch 1.1 Added file to File Manager  ASH 07AUG01
.***********************
.
NCRCFILE IFILE    KEYLEN=6,FIXED=18                 *DUPLICATES ALLOWED.
ncrcfle2 Ifile    keylen=8,fixed=18
.NCRCFILE IFILE    KEYLEN=12,FIXED=16                 *DUPLICATES ALLOWED.
NCRCFLAG FORM     1
.START PATCH 1.1 REPLACED LOGIC
.NCRCNAME INIT      "NINORD3 "
.NCRCNME2 INIT      "NINORD3A"
.Patch 1.2 Begin
.NCRCNAME INIT      "NINORD3.ISI|20.20.30.103:502 "
NCRCNAME INIT      "NINORD3.ISI|NINS1:502 "
.NCRCNME2 INIT      "NINORD3A.ISI|20.20.30.103:502"
NCRCNME2 INIT      "NINORD3A.ISI|NINS1:502"
.Patch 1.2 End
.END PATCH 1.1 REPLACED LOGIC

NCRCFLG2 FORM     1
.NCRCFLD  DIM      12
NCRCFLD  DIM      6
ncrcfld2 dim      8
ncrcpath form     1
NCRCLOCK FORM     1           0,1 = FILEPI, 2=RECORD LOCK, 3=NO LOCK


ncrcvars list
NCRCKEY  DIM      6      1-6       ORDER LR NUMBER    key1
ncrccc   dim      2      7-8       Century of revision   \
NCRCYY   DIM      2      9-10      DATE OF REVISION       \
NCRCMM   DIM      2     11-12      DATE OF REVISION        >key 2
NCRCDD   DIM      2     13-14      DATE OF REVISION       /
NCRCCODE DIM      1     15-15      STATUS 'R'=REVISED 'C'=CANCELLED.
NCRCTYP  DIM      3     16-18      WHO REVISED.
         listend
.
.pre 07apr98
............
.NCRCKEY  DIM      6      1-6       ORDER LR NUMBER.
.NCRCCODE DIM      1      7-7       STATUS 'R'=REVISED 'C'=CANCELLED.
.NCRCMM   DIM      2      8-9       DATE OF REVISION.
.NCRCDD   DIM      2     10-11      DATE OF REVISION.
.NCRCYY   DIM      2     12-13      DATE OF REVISION.
.NCRCTYP  DIM      3     14-16      WHO REVISED.
. 
