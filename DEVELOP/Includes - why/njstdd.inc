..............................................................................
.
. NJSTDD INCLUSION
. NIN DETAIL ADJUSTMENT FILE DEFINITION
.
. FILE NAME : NADJUST
. REC LENGTH: 177 FIXED
. INDEX KEY : 119-124,140-141 (INVOICE#, SUB#)
.
.Patch 1.6  PLI
.PATCH 1.5 IP address of File Manager Changed
.PATCH 1.3 ADDED AAMFILE AND FILELIST
.begin patch 1.2 10Mar99 DLH
. qty & reason field added 06jun95. DLH
.
..............................................................................
.
.begin patch 1.2
.NJSTFILE IFILE     KEYLEN=7,FIX=122,UNCOMP
.START PATCH 1.3 REPLACED LOGIC
.NJSTFILE IFILE     KEYLEN=8,FIX=177,UNCOMP
.>Patch 1.5 Begin
NJSTLIST  Filelist
.NJSTFILE IFILE     KEYLEN=8,FIX=177,UNCOMP,Name="NADJUST.ISI|20.20.30.103:502   "
.begin patch 1.6
.NJSTFILE IFILE     KEYLEN=8,FIX=177,UNCOMP,Name="NADJUST.ISI|NINS1:502   "
NJSTFILE IFILE     KEYLEN=8,FIX=188,UNCOMP,Name="NADJUST.ISI|NINS1:502   "
.NJSTFLE2 AFILE     FIX=177,Name="NADJUST.AAM|20.20.30.103:502   "
.NJSTFLE2 AFILE     FIX=177,Name="NADJUST.AAM|NINS1:502   "
NJSTFLE2 AFILE     FIX=188,Name="NADJUST.AAM|NINS1:502   "
.end patch 1.6
.NJSTFLE2 AFILE     FIX=177,Name="NADJUST.AAM"
	Filelistend
.END PATCH 1.3 REPLACED LOGIC
.end patch 1.2
NJSTFLSQ FILE
;NJSTNAME INIT      "NADJUST       "
.NJSTNAME INIT      "NADJUST.ISI|20.20.30.103:502   "
NJSTNAME INIT      "NADJUST.ISI|NINS1:502   "
.>Patch 1.5 End
.>Patch 1.4
NJSTNAMESEQ INIT      "NADJUST                      "
.>Patch 1.4
.begin patch 1.2
.NJSTFLD  DIM       7
NJSTFLD  DIM       8
NJSTFLD1 DIM       9
.begin patch 1.2
NJSTFLAG FORM      1
NJSTFLG2 FORM      1
.
jstvars  list
JSTBUSY  DIM       1         001-001   BUSY BYTE   and code 'J'
JSTSTAT  DIM       1         002-002   STATUS    '0 ORD P'
JSTMLR   DIM       4         003-006   MAILER CODE
JSTLR    DIM       6         007-012   LR#
JSTBILTO DIM       1         013-013   BILL-TO CODE
JSTPAYTO DIM       1         014-014   PAY-TO CODE
.begin patch 1.2 
..JSTAR    DIM       7         015-021   A/R  ADJUSTMENT
.JSTAP1   DIM       7         022-028   A/P1 ADJUSTMENT
.JSTAP2   DIM       7         029-035   A/P2 ADJUSTMENT
.JSTFIL1  DIM       1         036-036
.JSTLRINC DIM       6         037-042   LR INCOME ADJUSTMENT
JSTAR    form       9.2      015-026   A/R  ADJUSTMENT
JSTAP1   form       9.2      027-038   A/P1 ADJUSTMENT
JSTAP2   form       9.2      039-050   A/P2 ADJUSTMENT
jstap3   form       9.2      051-062   A/P2 ADJUSTMENT
JSTLRINC form       9.2      063-074   LR INCOME ADJUSTMENT
JSTNININC form       9.2     075-086   NIN NON comm INCOME ADJUSTMENT
.end patch 1.2 
JSTREUSE DIM       1         087-087   RE-USE/RUN CHARGE CODE
JSTCD    DIM       1         088-088   CREDIT/DEBIT CODE
JSTCRCT  DIM       1         089-089   CORRECT/ADDITIONAL BILL CODE
.begin patch 1.2 
.JSTSTAX  DIM       5         046-050   STATE TAX ADJUSTMENT
.JSTPOST  DIM       4         051-054   POSTAGE ADJUSTMENT
.JSTCTAX  DIM       5         055-059   CITY ADJUSTMENT
JSTSTAX  form       5.2         090-097   STATE TAX ADJUSTMENT
JSTPOST  form       5.2         098-105   POSTAGE ADJUSTMENT
JSTCTAX  form       5.2         106-113  CITY ADJUSTMENT
.end patch 1.2 
JSTREASN DIM       2         114-115   REASON CODE
JSTCNT   DIM       3         116-118   CONTACT CODE
.begin patch 1.2 
.JSTDATE  DIM       6         071-076   ADJUSTMENT DATE
JSTINVNO DIM       6         119-124   INVOICE NUMBER
JSTDATE  DIM       8         125-132   ADJUSTMENT DATE ccyymmdd
.JSTINVDT DIM       6         077-082   INVOICE DATE
.JSTSUBNO DIM       1         083-083   ADJUSTMENT AMENDMENT NUMBER
JSTINVDT DIM       8         133-140   INVOICE DATE ccyymmdd
JSTSUBNO DIM       2         141-142   ADJUSTMENT AMENDMENT NUMBER
.end patch 1.2 
JSTISTAT DIM       1         143-143   INVOICE STATUS
.begin patch 1.2 
.JSTFIL6  DIM       6         085-090
.jstqty   dim       7         119-7   qty adjusted to ....
jstqty   dim       9         144-152   qty adjusted to ....
.end patch 1.2 
jstqrsn  dim       25        153-177   reason for qty adjustment.
.begin patch 1.6
JSTXNINC form       9.2      178-188   NIN NON comm INCOME ADJUSTMENT
.end patch 1.6
         listend

