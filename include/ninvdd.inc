.. ninvdd.inc
.. ............................................................................
..  NAMES IN THE NEWS INCLUDE LIBRARY
.. ............................................................................
.. 
..     FILE:       NININV
..   LENGTH:       300
.. COMPRESS:       NONE
..     TYPE:       ISAM, aim
.. isi  KEY:       12-17     nininv.isi LR Keyl = 6
..                 124-129   Nininv.isi INVOICE NUMBERS,KEYL=6.
.begin patch 10.03
.. isi  KEY:       12-17     nininv.isi LR Keyl = 6   & Byte 2 <> P
..                 124-129   Nininv.isi INVOICE NUMBERS,KEYL=6.   & Byte 2 <> P

.end patch 10.03

.sunidxnt \\NINS1\e\data\text\nininv.dat,\\NINS1\e\data\index\nininvdev,l300,c:\ -n,e,12-17
.sunidxnt \\NINS1\e\data\text\nininv.dat,\\NINS1\e\data\index\nininvdev2,l300,c:\ -n,e,124-129
.Print File:
.sunidxnt \\NINS1\e\data\ninvoice.dat,\\NINS1\e\data\index\ninvoice,l303,c:\ -12-17
..aim  keys:       
.sunadxnt \\NINS1\E\DATA\TEXT\NININV.DAT \\NINS1\E\DATA\INDEX\NININV.AAM L300 -u,209-214,223-228,237-242,20-31,274-288
.begin patch 10.03
.sunidxnt \\NINS1\e\data\text\nininv.dat,\\NINS1\e\data\index\nininv4,l300,c:\ -n,e,P=\"2!='P'\",12-17
.sunidxnt \\NINS1\e\data\text\nininv.dat,\\NINS1\e\data\index\nininv5,l300,c:\ -n,e,P=\"2!='P'\",124-129
.end patch 10.03
.
.                             CHKN1    DIM       6     209-214  1ST CHECK NUMBER, AFTER CHECK RELEASED
.                             CHKN2     DIM      6     223-228  2ND CHECK NUMBER,  ADDED AFTER CHECK RELEASED
.                             CHKN3     DIM      6     237-242  3rd CHECK NUMBER,  should be escrow IE TDMC
.                             LOINVN   DIM       12     20-31  LIST OWNER INVOICE NUMBER aim key nininv4
.                             IMLRCHK  DIM       15    274-288   CHECK NUMBER MLR PAID WITH.ADDED AFTER CSH.
.*******************************************************************************
.patch 10.04 DLH  allow no locks on read (only)
.Patch 10.03 --- New INdices
.DLH  July 08 2008
. patch 10.02 PLI
.Ninv0001 - Patch 10.00   Total rewrite and new file structure
.*patch 2.2   29Jan2004 DLH  Broker Guar letter
.NINV0001 Patch 10.0 DLH June 2003
. includes change to filelist and combining of the two aimdex files.
.and increase in size of Mailer's check number from 6 to 15
.*patch 2.1   MANAGE  October 2001
.*Patch 2.0   y2k etc March 1999
.*10oct94 DLH prep for nets run charge & percentage new len 302.
.*05OCT94 DLH ADD SPLIT EXCHANGE QTY & ppm new length  294
.*23feb94 DLH increase list owner invoice 6 to 12 add running charge var.
.*23APR93 DLH PREP FOR BROKER/CONSULTANT FIELDS AND MAILER CHECK NUMBER FIELD
. NEW LENGTH WILL BE :  269.
.*;ADDITIONAL NOTES: aim keys added 10/5/92
.*******************************************************************************
.
...............................................................................
.INCOME OR
.PAYCODE  " "= 1 A/P A/R LR, 'STANDARD BILLING'
.         "1"= NOT USED
.         "2"= 2 A/P'S
.         "3"= KEYIN A/P1
.         "4"= GROSS A/R = LRINC.
.
...............................................................................
.ADDITIONAL CHARGE SUB-FIELDS ARE DEFINED AS FOLLOWS:
.
.BYTES      DESCRIPTION.
.begin patch 2.0
..................................................................................
.add chrg key expanded to 3 see below.
.    .  1-2       ADD. CHG. CODE USED TO ACCESS NINACD/TEXT.
.    .  3-9       AMOUNT IF FLAT FEE, OR RATE PER M. BASED ON ADD. CHG. CODE.
.    . 10-10      NOT USED
.    . 11-13      PERCENTAGE OF ADD. CHG. TO LR INCOME, REMAINDER TO A/P.
.    . 14-14      '1'= DON'T PRINT CHARGE ON INVOICE ie: RUNNING CHARGES
..................................................................................
.  1-3        ADD. CHG. CODE USED TO ACCESS NINACD/TEXT.
.  4-10       AMOUNT IF FLAT FEE, OR RATE PER M. BASED ON ADD. CHG. CODE.
. 11-11      NOT USED
. 12-14      PERCENTAGE OF ADD. CHG. TO LR INCOME, REMAINDER TO A/P.
. 15-15      '1'= DON'T PRINT CHARGE ON INVOICE ie: RUNNING CHARGES
.end patch 2.0
.            ADD TO A/R AND A/P1 *
.           ' '= STRAIGHT BILLING
.                INTO A/R AND A/P1 *
.               UNLESS 2 A/P'S THEN TO A/R & A/P2.
.        OR     INCOME/PAYCODE = '3' THEN TO A/R & LR.
.           '2'= INTO A/R AND A/P1 *
.
.         * ALSO PERCENTAGE MAY BE COMBINED WITH CODES 1 & 2 TO SPLIT BETWEEN
.           LRINC & A/P.
...............................................................................
.
.begin patch 2.0
.begin patch 10.0

PINVFILE        IFILE          KEYLEN=6,FIX=303                        .Print file Duplicates master + 3 bytes typist info
.PINVFILE        IFILE          KEYLEN=6,FIX=412                        .Print file Duplicates master + 3 bytes typist info
PINVNAME       INIT           "NINVOICE.ISI|NINS1:502"
.PINVNAME       INIT           "NINVOICE.ISI|20.20.30.103:502"
PINVFlag       Form           1
NINVLOCK FORM      1         0 & 1=FILEPI (default), 2=REcord lock, 3=no lock
.
NInvFLst       Filelist
.NINVFILE IFILE     KEYLEN=6,FIXED=302
.NINVFIL2 IFILE     KEYLEN=6,FIXED=302,NODUPLICATES
.NINVFIL3 AFILE     FIXED=302         .plb
.NINVFIL4 AFILE     FIXED=302        .plb
.NINVFILE IFILE     KEYLEN=6,FIXED=400,"NININV.ISI|20.20.30.103:502 "                    
.NINVFIL2 IFILE     KEYLEN=6,FIXED=400,NODUPLICATES,"NININV2.ISI|20.20.30.103:502 "      
.NINVFIL3 AFILE     FIXED=400,"NININV.AAM|20.20.30.103:502 "        
NINVFILE       IFILE          FIXED=300,KEYLEN=6,Name="NININV.ISI|NINS1:502 "                    
NINVFIL2       IFILE          FIXED=300,KEYLEN=6,NODUPLICATES,Name="NININV2.ISI|NINS1:502 "      
NINVFIL3       AFILE          FIXED=300,Name="NININV.AAM|NINS1:502 "        
.begin patch 10.03
NINVFIL4       IFILE          FIXED=300,KEYLEN=6,NODUPLICATES,Name="NININV4.ISI|NINS1:502 "      
NINVFIL5       IFILE          FIXED=300,KEYLEN=6,NODUPLICATES,Name="NININV5.ISI|NINS1:502 "      
.end patch 10.03
.;;.sunadxnt          -330-335,344-349,358-363,15-26,395-409
.;;                             CHKN1    DIM       6     330-335  1ST CHECK NUMBER, AFTER CHECK RELEASED
.;;                             CHKN2     DIM      6     344-349  2ND CHECK NUMBER,  ADDED AFTER CHECK RELEASED
.;;                             CHKN3     DIM      6     358-363  3rd CHECK NUMBER,  should be escrow IE TDMC
.;;                             LOINVN   DIM       12     15-26  LIST OWNER INVOICE NUMBER aim key nininv4
.;;                             IMLRCHK  DIM       6     395-400   CHECK NUMBER MLR PAID WITH.ADDED AFTER CSH.

..sunadxnt         L285 -u,190-195,204-209,218-223,17-28,255-259

.                             CHKN1    DIM       6     190-195  1ST CHECK NUMBER, AFTER CHECK RELEASED
.                             CHKN2     DIM      6     204-209  2ND CHECK NUMBER,  ADDED AFTER CHECK RELEASED
.                             CHKN3     DIM      6     218-223  3rd CHECK NUMBER,  should be escrow IE TDMC
.                             LOINVN   DIM       12     20-31  LIST OWNER INVOICE NUMBER aim key nininv4
.                             IMLRCHK  DIM       15    255-259   CHECK NUMBER MLR PAID WITH.ADDED AFTER CSH.

.NINVFIL4 AFILE     FIXED=400        .by List owner invoice #          15-26
               Filelistend
.end patch 10.0
.end patch 2.0
.begin patch 2.1
NINVNAME       INIT           "NININV.ISI|NINS1:502 "
NINVNME1       INIT           "NININV.ISI|NINS1:502 "
NINVNME2       INIT           "NININV2.ISI|NINS1:502"
NINVNME3       INIT           "NININV.AAM|NINS1:502"
.NINVNME3 INIT      "NININV3.AAM|NINS1:502"
.NINVNME4 INIT      "NININV4.AAM|NINS1:502"
.NINVNAME INIT      "NININV  "
.NINVNME1 INIT      "NININV"
.NINVNME2 INIT      "NININV2"
.end patch 2.1
NINVFLD        DIM            6    lr or inv number.
.begin patch 2.0
ninvfld1       DIM            9    CHECK no 1 .\
.end patch 2.0                       \
NINVFLD2       DIM            9    CHECK NO 2   .\
NINVFLD3       DIM            9    CHECK NO 3   .-> AIM KEYS.
NINVFLD4       DIM            15    LO INV NO   ./
NINVFLD5       DIM            9    Mlr's check ./
NINVFLAG       FORM           1
.begin patch 10.0
.NINVFLG2 FORM      1
.NINVFLG3 FORM      1
.NINVFLG4 FORM      1
.NINVFLG5 FORM      1
HoldINv        Dim            412            ;hold invoice record (big enough to hold print record)
.end patch 10.0
NINVPATH       FORM           1           1= by lr, 2= by invoice number
InvVarsHold         Dim       300       .holds copy of vars --- see Nchk002l
.
.begin patch 2.0
INVVARS        LIST
CODE           DIM            1       1-1  CODE, "F"
STATB          DIM            1       2-2  STATUS BYTE, "0" open OR "P" we have been paid, Or "e" Estimated (not booked)
.begin patch 10.0
IFiLL1          dim           2       3-4     For Expanded Mlr/Comp #
.end patch 10.0
MLRN           DIM            4       5-8  MAILER NUMBER
IFiLL2         dim            3       9-11     For Expanded lr #
LRN            DIM            6      12-17  LR NUMBER, ---KEY---
BILLTN         DIM            1      18-18  BILL TO NUMBER
PAYTN          DIM            1      19-19  PAY-TO NUMBER
LOINVN         DIM            12     20-31  LIST OWNER INVOICE NUMBER aim key nininv4
AR             form           10.2   32-44   ACCOUNTS RECEIVABLE (A/R),  X,XXX,XXX.XX
AP1            Form           10.2   46-57  LIST OWNER AMOUNT (A/P1), XX,XXX.XX
AP2            form           10.2   58-70   2ND ACCOUNTS PAYABLE (A/P2), X,XXX,XXX.XX
.begin patch 10.02
.AP3            Form           10.2   71-83  LIST OWNER AMOUNT (A/P1), XX,XXX.XX
AP3        Form           10.2   71-83  Inter company COmmission
.end patch 10.02
.begin patch 10.001
.COMMPCT        DIM            2      84-85  COMMISSION PERCENT,  .XX
COMMPCT        Form            3.2   84-89  COMMISSION PERCENT,  .XX
.begin patch 10.001
PAYCODE        DIM            1      90-90  PAYABLE CODE, BLANK,1,2,3 OR 4
IFiLL3         form           3      91-93   For Expanded QTYIn
QTYIN          form           9      94-102   Quantity In (ORdered/shipped/into merge)
IFiLL4         form           3     103-105  For Expanded QTyBild
QTYBILD        form           9     106-114  QUANTITY Billed (3/30/99 DLH),  X,XXX,XXX
PPM            form           3.2   115-120   PRICE PER M,  XXX.XX
IFiLL5         dim            3     121-123  For Expanded INV#
INVNUM         DIM            6     124-129  INVOICE NUMBER   ---KEY---
INVDTEc        DIM            2     130-131  INVOICE DATE (YEAR)
INVDTEY        DIM            2     132-133  INVOICE DATE (YEAR)
INVDTEM        DIM            2     134-135  INVOICE DATE (MONTH)
INVDTED        DIM            2     136-137  INVOICE DATE (DAY)
.begin patch 10.0
.ADDCHG1  DIM       15    122-136  ADDITIONAL CHARGE #1
.ADDCHG2  DIM       15    137-151  ADDITIONAL CHARGE #2
.ADDCHG3  DIM       15    152-166  ADDITIONAL CHARGE #3
.ADDCHG4  DIM       15    167-181  ADDITIONAL CHARGE #4
.ADDCHG5  DIM       15    182-196  ADDITIONAL CHARGE #5
.ADDCHG6  DIM       15    197-211  ADDITIONAL CHARGE #6
.ADDCHG7  DIM       15    212-226  ADDITIONAL CHARGE #7
.ADDCHG8  DIM       15    227-241  ADDITIONAL CHARGE #8
.ADDCHG9  DIM       15    242-254  ADDITIONAL CHARGE #9
.ADDCHG10 DIM       15    254-269  ADDITIONAL CHARGE #10
.end patch 10.0
COBN           DIM            3     138-140  CONTACT # (NIN) OR BROKER # (CMP)
invsales       DIM            2     141-142  Salesman code
INCC           DIM            1     143-143  INCOME CODE, "1"   NOT USED
.begin patch 10.0
IFiLL6         dim            2     144-145     For Expanded lon/Comp #
.end patch 10.0
LON            DIM            4     146-149  LIST OWNER NUMBER, ADDED AFTER CASH RECEIPTS
.begin patch 10.0
lon1           DIM            6     150-155  LIST OWNER NUMBER, for a/p2
lon2           DIM            6     156-161  LIST OWNER NUMBER, for a/p3
.end patch 10.0
.begin patch 2.2
.WSJPC    DIM       1     288-288  W.S.J. PAYMENT CODE, "1"
WSJPC          DIM            1     162-162  BRK Guar Letter "1" if first letter sent, "2" if second letter sent
.end patch 2.2
.begin patch 10.0
IFiLL7         dim            2     163-164     For Expanded Broker/Comp #
.end patch 10.0
IBRKNUM        DIM            4     165-168   BROKER/CONSULTANT NUMBER.
IBRKCNT        DIM            3     169-171   BROKER/CONSULTANT CONTACT NUMBER.
IRCQTY         DIM            9     172-180   quantity of r/c names on net order.
IFiLL8         Form           3     181-183   For Expanded  exchange qty
IREXQTY        DIM            9     184-192   bILLABLE EXCHANGE QTY ON A SPLIT ORDER. 10/5
iexPPM         Form           3.2   193-198   split exchange PRICE PER M,  XXX.XX
irnetper       dim            2     199-200   net name percentage
inetrc         form           3.2   201-206   net name running charge
ADJC           DIM            1     207-207   ADJUSTMENT CODE,  "*" IF ADJUSTED
statpay        DIM            1     208-208   "0" OPEN "P" we have paid    .????nec? OR just CHECK FOR a/p AMOUNT AND a CHECK number???
CHKN1          DIM            6     209-214  1ST CHECK NUMBER, AFTER CHECK RELEASED   CSHxx while in process
CHK1DTEc       DIM            2     215-216  CHECK DATE (CC)   FOR A/P 1
CHK1DTEY       DIM            2     217-218  CHECK DATE (YEAR)
CHK1DTEM       DIM            2     219-220  CHECK DATE (MONTH)
CHK1DTED       DIM            2     221-222  CHECK DATE (DAY)
CHKN2          DIM            6     223-228  2ND CHECK NUMBER,  ADDED AFTER CHECK RELEASED
CHK2DTEc       DIM            2     229-230 CHECK DATE (YEAR)   FOR A/P 2
CHK2DTEY       DIM            2     231-232  CHECK DATE (YEAR)
CHK2DTEM       DIM            2     233-234  CHECK DATE (MONTH)
CHK2DTED       DIM            2     235-236  CHECK DATE (DAY)
CHKN3          DIM            6     237-242  3rd CHECK NUMBER,  INter company
CHK3DTEc       DIM            2     243-244  CHECK DATE (YEAR)   FOR A/P 3
CHK3DTEY       DIM            2     245-246  CHECK DATE (YEAR)
CHK3DTEM       DIM            2     247-248  CHECK DATE (MONTH)
CHK3DTED       DIM            2     249-250  CHECK DATE (DAY)
LET90D         DIM            1     251-251  "1" if warning letter 60days for LM "2" If credit hold letter printed
.LET90D         DIM            1     251-251  OVER 90-DAY LETTER CODE, "1" IF OVER 90 DAY
MLRPAYR        form           10.2  252-264  MAILER $ PAYMENT RECEIVED, ADDED AFTER CASH RCT CCYYMMDD
MLRPAYD        DIM            8     265-272 DATE OF MAILERS PAYMENT, ADDED AFTER CASH RT CCYYMMDD
GUARPAY        DIM            1     273-273  GUARANTEED PAYMENT CODE, "*"=we guar
.IMLRCHK  DIM       6     255-400   CHECK NUMBER MLR PAID WITH.ADDED AFTER CSH.
IMLRCHK        DIM            15    274-288   CHECK NUMBER MLR PAID WITH.ADDED AFTER CSH.         typo somewhere this is correct location
.begin patch 10.02
XNINC          Form           9.2   289-300  Non COmm income           INter company
.IFill9         Dim            12    289-300 
.end patch 10.02
         LISTend
.end patch 2.0
.. ............................................................................

