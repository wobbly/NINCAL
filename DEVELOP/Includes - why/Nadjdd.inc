..............................................................................
.
. NADJDD INCLUSION
. NIN ADJUSTMENT FILE DEFINITION
.
. FILE NAME : NINADJ
. REC LENGTH: 151 FIXED
. INDEX KEY : 7-12 (LR#)
.
.patch 1.3   2007 September 14	DLH - PLI
.patch 1.2   2005 June 18	DMB - Changed IP of File Manager
.patch 1.1   y2k elim MP   var list etc
..............................................................................
.
.begin patch 1.1
.NADJFILE IFILE     KEYLEN=6,FIXED=90
.begin patch 1.3
.NADJFILE IFILE     KEYLEN=6,FIXED=151
NADJFILE IFILE     KEYLEN=6,FIXED=173
.begin patch 1.3
.end patch 1.1
.NADJNAME INIT      "NINADJ   "
.>Patch 1.2 Begin
.NADJNAME INIT      "NINADJ.ISI|20.20.30.103:502   "
NADJNAME INIT      "NINADJ.ISI|NINS1:502   "
.>Patch 1.2 End
NADJFLD  DIM       6
NADJFLAG FORM      1
.
adjvars   list
ASCODE   DIM       1        1-1            'J'
ASTATUS  DIM       1        2-2            '0' OR 'P'
ASMLRNUM DIM       4        3-6            MAILER NUMBER FROM INVOICE RECORD.
ASLRNUM  DIM       6        7-12           LR NUMBER - KEY.
ASRECADJ form      10.2    13-25           ACCOUNTS RECEIVABLE ADJUSTMENT.
ASPAYAD1 form      10.2    26-38           ACCOUNTS PAYABLE 1 ADJUSTMENT.
ASPAYAD2 form      10.2    39-51           ACCOUNTS PAYABLE 2 ADJUSTMENT.
ASPAYAD3 form      10.2    42-64           ACCOUNTS PAYABLE 3 ADJUSTMENT. inter company
ASLRINC  form      10.2    55-77           LIST RENTAL INCOME ADJUSTMENT.
ASNININC form      10.2    68-90           Non comm INCOME ADJUSTMENT.
ASREUSE  DIM       1       91-91           RE-USE/RUN CHARGE CODE (1-4).
ASCRDB   DIM       1       92-92           CREDIT OR DEBIT CODE (C 0R D).
ASADD    DIM       1       93-93           CORRECTION OR ADDITIONAL BILL CODE (1 OR2).
ASSTAX   form      5.2     94-101          STATE TAX ADJUSTMENT.
ASPOST   form      5.2    102-109          POSTAGE ADJUSTMENT.
ASCITY   form      5.2    110-117          CITY TAX ADJUSTMENT.
ASCNTCT  DIM       3      118-120          CONTACT/BROKER CODE.
ASINVNO  DIM       6      121-126          INVOICE NUMBER.
ASCRDTE  DIM       8      127-134          CREDIT DATE CCYYMMDD
ASINDTE  DIM       8      135-142          FROM INVOICE RECORD CCYYMMDD
ASAMNUM  DIM       2      143-144          INVOICE AMENDMENT NUMBER.
ASTAT    DIM       1      145-145          INVOICE STATUS FROM INVOICE RECORD.
.begin patch 1.3
.ASBLANK6 DIM       6      146-151          NOT USED
ASXNINC  form      10.2   146-158           Non comm INCOME ADJUSTMENT. inter company
ASBLANK DIM       15      159-173          NOT USED
.end patch 1.3

         listend
.ASCODE   DIM       1      1-1          'J'
.ASTATUS  DIM       1      2-2          '0' OR 'P'
..begin patch 1.1
.ASMLRNUM DIM       4      3-6          MAILER NUMBER FROM INVOICE RECORD.
.ASLRNUM  DIM       6      7-12          LR NUMBER - KEY.
..ASBILLTO DIM       1     13-13           BILL-TO CODE (0-9).
..ASPAYTO  DIM       1     14-14           PAY-TO CODE (0-9).
..ASRECADJ DIM       7     15-21           ACCOUNTS RECEIVABLE ADJUSTMENT.
..ASPAYAD1 DIM       7     22-28           ACCOUNTS PAYABLE 1 ADJUSTMENT.
..ASPAYAD2 DIM       7     29-35           ACCOUNTS PAYABLE 2 ADJUSTMENT.
..ASBLANK1 DIM       1     36-36           NOT USED.
..ASLRINC  DIM       6     37-42           LIST RENTAL INCOME ADJUSTMENT.
.ASRECADJ form      10.2    13-25           ACCOUNTS RECEIVABLE ADJUSTMENT.
.ASPAYAD1 form      10.2    26-38           ACCOUNTS PAYABLE 1 ADJUSTMENT.
.ASPAYAD2 form      10.2    39-41           ACCOUNTS PAYABLE 2 ADJUSTMENT.
.ASPAYAD3 form      10.2    42-54           ACCOUNTS PAYABLE 3 ADJUSTMENT.
.ASLRINC  form      10.2    55-67           LIST RENTAL INCOME ADJUSTMENT.
.ASNININC form      10.2    68-80           LIST RENTAL INCOME ADJUSTMENT.
..end patch 1.1
.ASREUSE  DIM       1       81-81           RE-USE/RUN CHARGE CODE (1-4).
.ASCRDB   DIM       1       82-82           CREDIT OR DEBIT CODE (C 0R D).
.ASADD    DIM       1       83-83           CORRECTION OR ADDITIONAL BILL CODE (1 OR2).
..begin patch 1.1
..ASSTAX   DIM       5     46-50           STATE TAX ADJUSTMENT.
..ASPOST   DIM       4     51-54           POSTAGE ADJUSTMENT.
..ASCITY   DIM       5     55-59           CITY TAX ADJUSTMENT.
.ASSTAX   form       5.2   84-91           STATE TAX ADJUSTMENT.
.ASPOST   form       5.2   92-99           POSTAGE ADJUSTMENT.
.ASCITY   form       5.2  100-117           CITY TAX ADJUSTMENT.
..ASREASON DIM       2     60-61           ADJUSTMENT REASON CODE (1-23).
..end patch 1.1
.ASCNTCT  DIM       3     118-120           CONTACT/BROKER CODE.
.ASINVNO  DIM       6     121-126           INVOICE NUMBER.
..begin patch 1.1
..ASCRDTE  DIM       6     71-76           CREDIT DATE MMDDYY.
..ASINDTE  DIM       6     77-82           FROM INVOICE RECORD MMDDYY.
..ASAMNUM  DIM       1     83-83           INVOICE AMENDMENT NUMBER.
.ASCRDTE  DIM       8     127-134           CREDIT DATE CCYYMMDD
.ASINDTE  DIM       8     135-142           FROM INVOICE RECORD CCYYMMDD
.ASAMNUM  DIM       2     143-144           INVOICE AMENDMENT NUMBER.
..end patch 1.1
.ASTAT    DIM       1     145-145           INVOICE STATUS FROM INVOICE RECORD.
.ASBLANK6 DIM       6     146-151    . 
.PROGRAM ACCESS.
.NAME           TYPE          ACCESS
