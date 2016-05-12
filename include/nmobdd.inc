.NMOBDD.INC     CREATED 06MAY92
.17mar94 added brknum
.12may94 added new file by brk/mlr
* *****************************************************************************
.
.    FILE:      NINMOB      - BALANCE RECORD FILE.
.  LENGTH:      21
.COMPRESS:      NONE
.    TYPE:      ISAM
.    KEYS:      1) MLR,
.    KEYS:      2) broker/mlr number
...............................................................................
NMOBFILE IFILE    KEYLEN=4,FIXED=21,dup
NMOBFLE2 IFILE    KEYLEN=8,FIXED=21,NoDup
NMOBNAME INIT    "NINMOB                      "
.START PATCH 01/20/2005 ASH REPLACED LOGIC
.NMOBNME1 INIT    "\\NINS1\e\data\index\NINMOB"
.NMOBNME2 INIT    "\\NINS1\e\data\index\NINMOB2"
NMOBNME1 INIT    "NINMOB|NINS1:502    "
NMOBNME2 INIT    "NINMOB2|NINS1:502   "
.END PATCH 01/20/2005 ASH REPLACED LOGIC
NMOBFLAG FORM    1
NMObFLG2 FORM      1
NMObpath FORM      1
nmobfld  dim       4
NmobMsgFlag    Dim            1              if "N" suppress io messages  See Nmobio.inc
NmobOkFlag    Dim            1              Ninv0001 can set to yes so if No $ and trying code 96 - we return gracefully
Code96Ok      Dim             1             see above
...............................................................................
.BALANCE RECORD
...............
mobvars  list
nmobMLR  DIM       4        1-4    CLIENT NUMBER "KEY".
nmobmCNT DIM       3        5-7    MAILER CONTACT NUMBER "KEY".
.MBILLTO  DIM       1        8-8    MAILER BILL-TO NUMBER.
BALANCE  FORM       7.2      9-17   AMOUNT ON ACCOUNT
nmobbrk   dim       4       18-21   broker/consultant. "KEY"
          Listend
...............................................................................

