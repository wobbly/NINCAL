* *****************************************************************************
* NmoaDD.INC CREATED 30APR92 DLH
* *****************************************************************************
.
.14 Oct 2010 add reason code 23
.move comments to notes so room for COmpany id
.22Dec2003 Expanded notes into seperate file     DLH
.See MoaNotesdd.inc 
. 28dec95 on write use actual variables and *zf on brk number. DLh
.10mar95 increase control number to 3 bytes
.17mar94 added brknum
.12may94 added new file by brk/mlr
.
.    FILE:      NINMOA      - DETAIL RECORD FILE.
.  LENGTH:      128
.COMPRESS:      NONE
.    TYPE:      ISAM
.    KEYS:      1)MLR,ENTRY   ? mlr,cnt   1-7
.               2)TRANSNUM              115-121
.               3)brk,mlr               122-125,1-4
.               4)LRNUM                  30-35
.
.NMOAFILE IFILE     KEYLEN=7,FIXED=119              .mlr/cnt
.NMOAFLE2 IFILE     KEYLEN=7,FIXED=119              .transnum
.NMOAFLE3 FILE      FIXED=119                       .seq
.nmoafle4 ifile     keylen=8,fixed=119              .brk/mlr
.nmoafle5 ifile     keylen=6,fixed=119              .lrn
NMOAFILE IFILE     KEYLEN=7,FIXED=128              .mlr/cnt
NMOAFLE2 IFILE     KEYLEN=7,FIXED=128              .transnum
NMOAFLE3 FILE      FIXED=128                       .seq
nmoafle4 ifile     keylen=8,fixed=128              .brk/mlr
nmoafle5 ifile     keylen=6,fixed=128              .lrn
NMOANAME INIT      "NINMOA                         "
.START PATCH 01/20/2005 ASH REPLACED LOGIC
.NMOANME1 INIT      "\\NINS1\e\data\index\NINMOA  "
.NMOANME2 INIT      "\\NINS1\e\data\index\NINMOA2 "
.NMOANME3 INIT      "\\NINS1\e\data\index\NINMOA  "
.NMOANME4 INIT      "\\NINS1\e\data\index\NINMOA3 "
.NMOANME5 INIT      "\\NINS1\e\data\index\NINMOA4 "
NMOANME1 INIT      "NINMOA  "
NMOANME2 INIT      "NINMOA2 "
NMOANME3 INIT      "NINMOA  "
NMOANME4 INIT      "NINMOA3 "
NMOANME5 INIT      "NINMOA4 "
.END PATCH 01/20/2005 ASH REPLACED LOGIC
NMOAPATH FORM      1                   1=mlr/cnt,2=transnum,3=seq,4=brk/mlr
.                                      5=lrn
NMOAFLAG FORM      1
NMOAFLG2 FORM      1
NMOAFLG3 FORM      1
NMOAFLG4 FORM      1
NMOAFLG5 FORM      1
NMOAFLD  DIM       7
NMOAFLD4 DIM       8
NMOAFLD5 DIM       6
NMOALOCK FORM      1  0 or 1=File locks, 2= Record locks, 3=no locks
.
.DETAIL RECORD.
...............
moavars  list
MLR      DIM       4        1-4    CLIENT NUMBER "KEY".
MCNT     DIM       3        5-7    MAILER CONTACT NUMBER.
ENTRY    FORM      4        8-11   ENTRY NUMBER "KEY".
MBILLTO  DIM       1       12-12   MAILER BILL-TO NUMBER.
.TRANDATE DIM       6       13-18   DATE APPLIED/RECEIVED.
TRANDATE DIM       8       13-20   DATE APPLIED/RECEIVED.   CCYYMMDD
CONTROL  DIM       3       21-23   CONTROL NUMBER.
INVOICE  DIM       6       24-29   INVOICE NUMBER.
LRNUM    DIM       6       30-35   LIST RENTAL NUMBER.
.INVDATE  DIM       6       34-39   INVOICE DATE.
INVDATE  DIM       8       36-43   INVOICE DATE.  ccyymmdd
ONAMOUNT FORM      7.2     44-53   AMOUNT OUT
.RECDATE  DIM       6       50-55   DATE RECORD ENTERED.
RECDATE  DIM       8       54-61   DATE RECORD ENTERED. ccyymmdd
INAMOUNT FORM      5.2     62-69   AMOUNT IN
MoaCOmp   Dim       1         70-70                'P' if Pacific Lists
MoaFIll   Dim       29        71-99
.ONACOM  DIM        30      70-99   COMMENTS
REASON   FORM      2      100-101  REASON CODE
LIST     DIM       6      102-107  LIST NUMBER TO BE APPLIED TO.
CHECKNUM DIM       7      108-114  CHECK NUMBER
TRANSNUM DIM       7      115-121  RECORD ID NUMBER.   "KEY".
NMOABRK  dim       4      122-125  consultant/broker #. 3/17/94 dlh "key"
NMOAINIT DIM       3      126-128  TYPIST INITIALS      .New field
         listend
.
.MOA REASON TABLE.
.............
REAS1    INIT      "OVERPAYMENT"
REAS2    INIT      "UNIDENT. PAYMNT"
REAS3    INIT      "INV. CANCELLED"
REAS4    INIT      "MISSING"
REAS5    INIT      "REFUND"
REAS6    INIT      "UNUSED CREDIT"
REAS7    INIT      "USING CREDIT"
REAS8    INIT      "TRF ENTRY"
REAS9    INIT      "ADV PAY POLITIC"
REAS10   INIT      "ADV PAY NEW MLR"
REAS11   INIT      "PAYMENT ON A/C"
REAS12   INIT      "FROM BOOKS"
REAS13   INIT      "DUP. PAYMENT"
REAS14   INIT      "WRITEOFF"
REAS15   INIT      "Short pay"
REAS16   INIT      "Canc Check"
REAS17   INIT      "Transfer"
REAS18   INIT      "Prepayment"
REAS19   INIT      "Escrow"
REAS20   INIT      "M O A"
REAS21   INIT      "Contra"
.begin patch 3.41 Nona0001
REAS22   INIT      "LO Income check"
REAS23   INIT      "NIN-Discnt/Fees"
.end patch 3.41 Nona0001
REAS99   INIT      "ENTRY CORRECTN"
RDESC    DIM       15

.

