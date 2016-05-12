
..............................................................................
.
. NOWNDD INCLUSION
. NIN OWNER FILE DEFINITION
.
. FILE NAME : NINOWN
. REC LENGTH: 190 FIXED
. INDEX KEY : 3-6 (OWNLON)
. AIM KEY   : 7-56
.2014 March  DLH  add room to email
.2011 August 23: DLH add more room & suppress LO print copies, and for future galley monthly report
.2010 June 04 : DLH add secondary file maint for DupeOwn
.2009Jan14 : comp codes DLH
.20080124  Email    DLH
. 09Dec2000 Change IP address of File manager DMB
. 09Dec2000 use filelist
..10sep96 inactive code, batch code for lcr's, century - dlh
..16may95 add 2nd fax number for accounting.
..............................................................................
.
.NOWNNAME INIT      "NINOWN.dat|20.20.30.103:502"
DOWNNAME  INIT      "DUPEOWN.dat|NINS1:502"  
DOWNFLE1  IFILE     KEYLEN=4,FIXED=120,Name="DupeOWN.isi|NINS1:502"
DOWNKEY   DIM       4     1-4    *DUPE OWNER FILE.
Down1     DIM       1     5-5    Fill
Downkey2  DIM       4     6-9   OWNER NUMBER TO BE USED FROM DUPEOWN FILE.
Down2     DIM       1    10-10   FIll
DOWNDES   DIM       50   11-60    DESCRIPTION
Down3     Dim       60            Fill
Downflg1   Form      1
Downfld   Dim       4
.................................................................................

NOWNNAME  INIT      "NINOWN.dat|NINS1:502"
NOWNFLD   DIM       4
OwnFList  Filelist
.NOWNFLE1 IFILE     KEYLEN=4,FIXED=190,Name="NINOWN.isi|20.20.30.103:502"
.NOWNFLE1           IFILE     KEYLEN=4,FIXED=190,Name="NINOWN.isi|NINS1:502"
.NOWNFLE1           IFILE     KEYLEN=4,FIXED=241,Name="NINOWN.isi|NINS1:502"
.NOWNFLE1  IFILE     KEYLEN=4,FIXED=260,Name="NINOWN.isi|NINS1:502"
NOWNFLE1  IFILE     KEYLEN=4,FIXED=310,Name="NINOWN.isi|NINS1:502"
.NOWNFLE2 AFILE     FIXED=190,Name="NINOWN.AAM|20.20.30.103:502"
.NOWNFLE2           AFILE     FIXED=190,Name="NINOWN.AAM|NINS1:502"
.NOWNFLE2           AFILE     FIXED=241,Name="NINOWN.AAM|NINS1:502"
.NOWNFLE2  AFILE     FIXED=260,Name="NINOWN.AAM|NINS1:502"
NOWNFLE2  AFILE     FIXED=310,Name="NINOWN.AAM|NINS1:502"

.NOWNFLE1 IFILE     KEYLEN=4,FIXED=190,Name="NINOWN.isi"
.NOWNFLE2 AFILE     FIXED=190,Name="NINOWN.AAM"
          filelistend
NOWNFLE3  FILE      FIXED=310
.NOWNFLE3  FILE      FIXED=260
.NOWNFLE3           FILE      FIXED=190
NOWNFLD2   DIM       53
NOWNFLG1  FORM      1
NOWNFLG2  FORM      1
NOWNFLG3  FORM      1
NOWNPATH  FORM      1
.
ownvars   list
OWNLOC    DIM       1         001-002   'B'... "P" for PLI COnversion 'M' from MIN
OWNBLK    DIM       1         002-002    BLANK      '0   LIst owners that don't allow samples = 1
OWNLON    DIM       4         003-006   OWNER NUMBER
OWNLONM   DIM       25        007-031   CONTACT NAME
OWNOCPY   DIM       25        032-056   COMPANY NAME
OWNLOSA   DIM       25        057-081   ADDRESS
OWNLOCTY  DIM       15        082-096   CITY
OWNLOS    DIM       2         097-098   STATE
OWNLOZC   DIM       10        099-108   ZIP CODE
OWNNEC    DIM       1         109-109   NUMBER OF CARBON COPIES
OWNCTN    DIM       16        110-125   CARBON COPY TO
OWNTELE   DIM       10        126-135   TELEPHONE NUMBER
OWNPASS   DIM       10        136-144   PERSON LAST UPDATED BY
OWNRDTE   DIM       8         145-153   REVISED DATE mm,dd,cc,yy
owngally  dim       1         154-154   'T'rue = lcr's get combined galley request
OWNTAXID  DIM       15        155-169   TEFRA TAX ID CODE.
OWNFAX    DIM       10        170-179    FAX NUMBER.
ownfax2   dim       10        178-189    2nd fax ie acct.
OWNstat   DIM       1         190-190   "I"nactive
OwnTranFlag         Dim       1         191-191    -Send Fulfilment info via: ' '=fax or print based on  presence of fax#
.                                              2=FTP, 1= Email.......
.begin patch
OwnEmail  Dim       100       192-291     corporate email
OwnCOMp   Dim       6         292-297     company code
OwnCont   Dim       3         298-300   Contact code
OwnCopy   Dim       1         301-301   Y or " "-print LO copies of orders (default) or N-no print
OwnFIll   Dim       9         302-310

.OwnEmail  Dim       50        192-241     corporate email
.OwnCOMp   Dim       6         242-247     company code
.OwnCont   Dim       3         248-250   Contact code
.OwnCopy   Dim       1         251-251   Y or " "-print LO copies of orders (default) or N-no print
.OwnFIll   Dim       9         252-260
.end patch
+++-+---+-++---       -+

         listend
..............................................................................*
         LISTOFF
.PROGRAM ACCESS.
.NAME           TYPE          ACCESS
         LISTON
Paige.Kelbe@epsilon.com,JHonadel@epsilon.com
