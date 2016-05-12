..............................................................................
.
. NSPIDD INCLUSION
. NIN SPECIAL INSTRUCTION FILE DEFINITION
.
. FILE NAME : NINSPI
. REC LENGTH: 97 FIXED
. INDEX KEY : 1-3
.patch 1.0  change key field to 3   oct 10 2006
..............................................................................
.
.begin patch 1.0
NSPIFILE IFILE     KEYLEN=3,FIXED=96,UNCOMP
.NSPIFILE IFILE     KEYLEN=2,FIXED=96,UNCOMP
NSPINAME INIT      "NINSPI.isi|NINS1:502  "
NSPIFLD  DIM       3
.NSPIFLD  DIM       2
NSPIFLAG FORM      1
.
SPINO    dim       3       1-3      SPECIAL INTRUCTION NUMBER
.SPINO    dim       2       1-2      SPECIAL INTRUCTION NUMBER
.end patch 1.0
INST1    DIM       47      4-50     SPECIAL INSTRUCTION LINE #1
INST2    DIM       47     51-97     SPECIAL INSTRUCTION LINE #2
.
                LISTOFF
.PROGRAM ACCESS.
.NAME           TYPE          ACCESS
.CLEARPRT       DATABUS
.DIVSPOOL       DATABUS
.EPSSPOOL       DATABUS
.FIXLCR         DATABUS
.FIXORD         DATABUS
.NINCLEAR       DATABUS
.NINMAC         DATABUS
.NINP11D        DATABUS
.PAYORDERPRT    DATABUS
.SPIMOD         DATABUS
.SPOOLLCRS      DATABUS
.TDMCSPOL       DATABUS
.WARSPOOL       DATABUS
                LISTON


