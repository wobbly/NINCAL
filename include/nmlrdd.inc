.----------------------------------------------------------------
.
.   nmlrdd.inc - 
.
.   Copyright(c) Names in the News Ca., Inc. 2000
.
.   Author: David L Herrick
.   Created: 3/5/2002 12:28:00 PM
.   Last change: DLH 6/12/2002 6:06:28 AM
.----------------------------------------------------------------

..............................................................................
.
. NMLRDD INCLUSION
. NIN MAILER FILE DEFINITION
.
. FILE NAME : NINMLR
. REC LENGTH: 255 FIXED
. INDEX KEY : 3-9 (4 POSITION MLR#, 3 POSITION CONTACT#)
. AIM KEY   : 55-99

.********Note if record changes Ndat0004.dbs requires some manual updating.!!!!!!
. 22July DLH   Patch #101  - add one new field: MFaxOFlag, Modify Mccto
. 10sep98 ASH - PATCH #100 - added two new fields: m2addr,mcoun and increased mname and mcomp
.
. add century and change date format from mmddyy to ccyymmdd 27Mar98
. and add current br k& brk contact field size from 166 to 172
. Bill direct code added 21jul94
. Batch bill code added 27Jan93.
. PHONE & FAX - ADDED 05JAN93.
..............................................................................
.Start Patch #100 - changed file length
.NMLRFILE IFILE     KEYLEN=7,FIXED=175,UNCOMP
NMLRFILE IFILE     KEYLEN=7,FIXED=255,UNCOMP
.End Patch #100 - changed file length

NMLRNAME INIT      "NINMLR      "
         IFNZ      PC
NMLRFLE2 AFILE
         XIF

         IFZ       PC
.Start Patch #100 - changed file length
.NMLRFLE2 AFILE     FIXED=175,UNCOMP
NMLRFLE2 AFILE     FIXED=255,UNCOMP
.End Patch #100 - changed file length
.NMLRFLE2 AFILE     28,1,,FIXED=166,UNCOMP
         XIF

.Start Patch #100 - changed file length
.NMLRFLE3 FILE      FIXED=175
NMLRFLE3 FILE      FIXED=255
.End Patch #100 - changed file length

MKEY     DIM       7
.Start Patch #100 - increased var
.MLRAKEY  DIM       28
MLRAKEY  DIM       48
.End Patch #100 - increased var
NMLRFLAG FORM      1
NMLRFLG2 FORM      1
NMLRFLG3 FORM      3
NMLRPATH FORM      1
NMLRLOCK FORM      1         0 or 1=File locks. 2=record lock, 3=no locks
.
mlrvars  list
MCODE    DIM       1         001-001   Holds 'B' if batch bill.
.                                            'A' if batch with adj
.                                      else  'M'
MRCODE   DIM       1         002-002   Holds 'R' if rental only
MNUM     DIM       4         003-006   MAILER COMPANY NUMBER
MCONTCT  DIM       3         007-009   MAILER CONTACT NUMBER
.Start Patch #100 - Increased field lengths
.MNAME    DIM       25        010-034   CONTACT NAME
MNAME    DIM       45        010-054   CONTACT NAME
.MCOMP    DIM       25        035-059   COMPANY NAME  AIM KEY
MCOMP    DIM       45        055-099   COMPANY NAME  AIM KEY
.End Patch #100 - Increased field lengths
.MBCODE   DIM       3        **57-*59** 'C/O' IF BROKER, NOT USED BY CAL.
.***NOTE MBCODE IS LOCATED IN THE LAST THREE BYTES OF MCOMP.*************
MADDR    DIM       25        100-124   ADDRESS
.Start Patch #100 - Added new field
M2ADDR   DIM       25        125-149   ADDRESS2
.End Patch #100 - Added new field
MCITY    DIM       15        150-164   CITY
MSTATE   DIM       2         165-166   STATE
MZIP     DIM       10        167-176   ZIP CODE
.Start Patch #100 - Added new field
MCOUN    DIM       15        177-191   COUNTRY CODE
.End Patch #100 - Added new field
MCOPIES  DIM       1         192-192   NUMBER OF CARBON COPIES
.                                      used for regional Y/N/' '
.Begin patch #101
.MCCTO    DIM       16        193-208   CARBON COPY TO
Mccto          Dim            15             193-207
MFaxOFlag      Dim            1              208-208                 "T" = Fax Mailer order confirm
.end patch #101
MPASS    DIM       8         209-216   LAST USER TO MODIFY
MSLSPER  DIM       2         217-218   ORDER SALES PERSON
.MREVDATE DIM       6        219-224   REVISED DATE  mmddyy
MREVDATE DIM       8         219-226   REVISED DATE   ccyymmdd .27mar98 DLH
MSTAT    DIM       1         227-227   CREDIT STATUS ' '=OK,
. '                                     "*" = ON HOLD.
. '                                     "I" = INACTIVE,
. '                                     "B" = CREDIT RISK.
. '                                     "N" =   NEW MAILER.
. '                                     "P" = POLITICAL MAILER.
..'21Dec2000                            "W" = Warning - read note
. '05Mar2002                            "M" = Must Prepay
. '05Mar2002                            "9" = On hold until over 90s paid
. '05Mar2002                            "G" = Guarantees are always required
. "P" STATUS RESET NIGHTLY, "B" RESET NIGHTLY IF REQUESTED.
.
MTELE    DIM       10        228-237   MAILER'S TELEPHONE.
MFAX     DIM       10        238-247   MAILERS FAX #.
mbildrct dim       1         248-248   Bill directly not thru brk/consultant=y
MBRKNUM  dim       7         249-255   Current default BRk/brkcnt.Consultant.
.                            169-172   brk num 173-175 brk cnt num.
         listend
MBCODE   DIM       3        **57-*59** 'C/O' IF BROKER, NOT USED BY CAL.
***NOTE MBCODE IS LOCATED IN THE LAST THREE BYTES OF MCOMP.*************

...............................................................................
.END OF INCLUDE
.
..............................................................................

