..............................................................................
.
. NOWNDD INCLUSION
. NIN OWNER FILE DEFINITION
.
. FILE NAME : NINOWN
. REC LENGTH: 190 FIXED
. INDEX KEY : 3-6 (OWNLON)
. AIM KEY   : 7-56
. 09Dec2000 Change IP address of File manager DMB
. 09Dec2000 use filelist
..10sep96 inactive code, batch code for lcr's, century - dlh
..16may95 add 2nd fax number for accounting.
..............................................................................
.
.NOWNNAME INIT      "NINOWN.dat|20.20.30.104:502"
NOWNNAME INIT      "NINOWN.dat|10.10.30.104:502"
NOWNFLD   DIM       4
OwnFList Filelist
.NOWNFLE1 IFILE     KEYLEN=4,FIXED=190,Name="NINOWN.isi|20.20.30.104:502"
NOWNFLE1 IFILE     KEYLEN=4,FIXED=190,Name="NINOWN.isi|10.10.30.104:502"
.NOWNFLE2 AFILE     FIXED=190,Name="NINOWN.AAM|20.20.30.104:502"
NOWNFLE2 AFILE     FIXED=190,Name="NINOWN.AAM|10.10.30.104:502"

.NOWNFLE1 IFILE     KEYLEN=4,FIXED=190,Name="NINOWN.isi"
.NOWNFLE2 AFILE     FIXED=190,Name="NINOWN.AAM"
         filelistend
NOWNFLE3 FILE      FIXED=190
NOWNFLD2  DIM       53
NOWNFLG1 FORM      1
NOWNFLG2 FORM      1
NOWNFLG3 FORM      1
NOWNPATH FORM      1
.
ownvars  list
OWNLOC   DIM       1         001-002   'B'
OWNBLK   DIM       1         002-002    BLANK      '0' 6/10/05 Using this byte to hold Sample Info ASH
OWNLON   DIM       4         003-006   OWNER NUMBER
OWNLONM  DIM       25        007-031   CONTACT NAME
OWNOCPY  DIM       25        032-056   COMPANY NAME
OWNLOSA  DIM       25        057-081   ADDRESS
OWNLOCTY DIM       15        082-096   CITY
OWNLOS   DIM       2         097-098   STATE
OWNLOZC  DIM       10        099-108   ZIP CODE
OWNNEC   DIM       1         109-109   NUMBER OF CARBON COPIES
OWNCTN   DIM       16        110-125   CARBON COPY TO
OWNTELE  DIM       10        126-135   TELEPHONE NUMBER
OWNPASS  DIM       10        136-144   PERSON LAST UPDATED BY
OWNRDTE  DIM       8         145-153   REVISED DATE mm,dd,cc,yy
owngally dim       1         154-154   'T'rue = lcr's get combined request
OWNTAXID DIM       15        155-169   TEFRA TAX ID CODE.
OWNFAX   DIM       10        170-179    FAX NUMBER.
ownfax2  dim       10        178-189    2nd fax ie acct.
OWNstat  DIM       1         190-190   BLANK, "I"nactive
         listend
..............................................................................*
         LISTOFF
.PROGRAM ACCESS.
.NAME           TYPE          ACCESS
         LISTON

