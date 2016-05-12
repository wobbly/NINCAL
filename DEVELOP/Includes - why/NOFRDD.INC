..............................................................................
.
. NOFRDD INCLUSION
. NIN OFFER FILE DEFINITION
.
. FILE NAME : NINOFR
. REC LENGTH: 66 FIXED
. INDEX KEY : 2-8 (4 POSITION MLR#, 3 POSITION OFFER#)
.
. RElease   1.2	24Apr07	DLH added file manager 
. RELEASE	1.1	17OCT02	ASH ADDED AIM FILE FUNCTIONALITY
..............................................................................
.
.START PATCH 1.1 REPLACED LOGIC
.NOFRFILE IFILE     KEYLEN=7,FIXED=64,UNCOMP
NOFRFLIST	FILELIST
NOFRFILE IFILE     KEYLEN=9,FIXED=68,UNCOMP,Name="NINOFR.ISI|NINS1:502"
NOFRFLE2 AFILE     FIXED=68,UNCOMP,Name="NINOFR.AAM|NINS1:502"
		FILELISTEND
.END PATCH 1.1 REPLACED LOGIC
NOFRNAME INIT      "NINOFR|NINS1:502  "
NOFRFLD  DIM       9
.START PATCH 1.1 REPLACED LOGIC
NOFRFLD1 DIM       9
.END PATCH 1.1 REPLACED LOGIC
NOFRFLAG FORM      1
NOFRLOCK FORM      1    0 or 1=File locks, 2=Record locks, 3=no locks
* ....................................................
.
ofrvars  list
OFCODE   DIM       1         1-1     'R'
OFMLR    DIM       6         2-7     MAILER NUMBER      KEY.
OFNUM    DIM       3         8-10     OFFER NUMBER       KEY.
OFDESC   DIM       40        11-50    OFFER DESCRIPTION.
OFNAME   DIM       10       51-60    LAST UPDATED BY.
.OFDATE   DIM       6        59-64    DATE LAST UPDATED.
OFDATE   DIM       8        61-68    DATE LAST UPDATED.
         listend
...............................................................................
                LISTOFF
.PROGRAM ACCESS.
.NAME           TYPE          ACCESS
.NLCR0011       DATABUS
.NDAT0010       DATABUS
.NDAT0012       DATABUS
.NDIV0001       DATABUS
.NDIV0002       DATABUS
.EPSSPOOL       DATABUS
.NLCR0009       DATABUS
.NORD0009       DATABUS
.NAPR0001       DATABUS
.NLCR0001       DATABUS
.NORD0001       DATABUS
.NOFR0001       DATABUS
.PAYORDERPRT    DATABUS
.SPOOLLCRS      DATABUS
.NORD0023       DATABUS
.NORD0024       DATABUS
.WARSPOOL       DATABUS
.WARVIEW        DATABUS
.NDAT0004       DATABUS
.NAPR0002       DATABUS
                LISTON

