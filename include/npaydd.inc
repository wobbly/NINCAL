..............................................................................
.
. NPAYDD INCLUSION
. NIN PAY-TO FILE DEFINITION
.
. FILE NAME : NINPAY
. REC LENGTH: 227  FIXED
. INDEX KEY : 2-6 (POWNER,PAYNUM)
. AIM KEY   : 7-96
. added tele & fax 16may95.????
.
. ASH 31AUG98  CREATED TWO NEW FIELDS: P2STREET,PCOUN   
.              Record length changed to 227 fixed
. Patch:       Increased length of PNAME and PCOMP to 45 each
.              Record length changed to 187 FIXED
. Patch Author:  Andrew Harkins
. Patch Date:  July 9,1998
.
. Patch #100 - Created as test for Year 2000 conversion
.              Record length changed to 147 FIXED
.              This takes into consideration two new fields
.              added on 16may95 (see above), as well as 
.              new date format
. Patch Author:  Andrew Harkins
. Patch Date:  May 21,1998
..............................................................................

.
.NPAYFILE IFILE     KEYLEN=5,FIXED=125
.NPAYFILE IFILE     KEYLEN=5,FIXED=147
.NPAYFILE IFILE     KEYLEN=5,FIXED=187
.NPAYFIL AFILE     50,1,,FIXED=125
.NPAYFIL2 AFILE     FIXED=125
.NPAYFIL2 AFILE     FIXED=147
.NPAYFIL2 AFILE     FIXED=187
.NPAYFLE3 FILE      FIXED=125
.NPAYFLE3 FILE      FIXED=147
.NPAYFLE3 FILE      FIXED=187

PayFList Filelist
NPAYFILE IFILE     KEYLEN=5,FIXED=227,Name="NINPAY.isi|NINS1:502"

NPAYFIL2 AFILE     FIXED=227,Name="NINPAY.AAM|NINS1:502"
          FileListEnd

NPAYFLE3 FILE      FIXED=227
          
NPAYNAME INIT      "NINPAY  "
NPAYFLD  DIM       5
.NPAYFLD2 DIM       53
NPAYFLD2 DIM       93
NPAYPATH FORM      1
NPAYFLAG FORM      1
NPAYFLG2 FORM      1
NPAYFLG3 FORM      1
NPAYLOCK FORM      1    0 or 1=File locks, 2=Record locks, 3=no locks
.
.Defined PAYNUM codes:
.                     0= automatically applied substitute for list owner info.
.                     1= Consumer Direct
.                     2= List Processing Service
.                     3= Epsilon
.                     4= CMS
.                     5= not assigned
.                     6= not assigned
.                     7= Lifestyle Selector
.                     8= misc fulfilment Corp's.
.                     9= Generally used to override a default 0 entry. and
.                        force it to revert to owner file info.
...............................................................................
payvars  list
PAYRCODE DIM       1           1-1     RECORD CODE = '6'
POWNER   DIM       4           2-5     OWNER NUMBER
PAYNUM   DIM       1           6-6     PAY-TO NUMBER FOR MULT PAY-TO'S
PNAME    DIM       45          7-51    CONTACT NAME
PCOMP    DIM       45         52-96    COMPANY NAME
PSTREET  DIM       25         97-121   STREET ADDRESS
P2STREET DIM       25        122-146   STREET ADDRESS2
PCITY    DIM       15        147-161   CITY
PSTATE   DIM       2         162-163   STATE
PZIP     DIM       10        164-173   ZIPCODE
PCOUN    DIM       15        174-188   COUNTRY CODE
PPASS    DIM       10        189-198   PASSWORD ID
PDATE    DIM       8         199-206   REVISED DATE
PBLANK   DIM       1         207-207   NOT USED.
payTELE  DIM       10        208-217   TELEPHONE NUMBER
payFAX   DIM       10        128-227   FAX NUMBER.
         listend
.PNAME    DIM       25          7-51    CONTACT NAME
.PCOMP    DIM       25         52-96    COMPANY NAME
                LISTOFF
.PROGRAM ACCESS.
.NAME           TYPE          ACCESS
.CHECKWRITER    DATABUS
.FIXINV         DATABUS
.MODINV         DATABUS
.NINP29         DATABUS
.NINP29EDIT     DATABUS
.ERPAYLET       DATABUS
.PAYMOD         DATABUS
                LISTON
