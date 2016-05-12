.    Last change: DLH 6/13/2002 11:01:20 AM
..............................................................................
.
. NRTNDD INCLUSION
. NIN RETURN-TO FILE DEFINITION
.
. FILE NAME : NINRTN
. REC LENGTH: 227 FIXED. INDEX KEY : 1-4 (4 POSITION RTN#)
. AIM KEY   : 50-94
.
. RElease   1.4     05Aug11   DLH added room for company & contact numbers preping for merge into company file
. RElease   1.3     24Apr07   DLH added file manager 
. RElease   1.2     12April2007 DLH  inactive byte
. RElease   1.1     19AUG98 ASH Y2K CONVERSION
..............................................................................
.
NRTNFILE IFILE     KEYLEN=4,FIXED=236,UNCOMP,Name="NINRTN.isi|NINS1:502"
.NRTNFILE IFILE     KEYLEN=4,FIXED=227,UNCOMP,Name="NINRTN.isi|NINS1:502"
NRTNNAME INIT      "NINRTN|NINS1:502  "
NRTNFLE2 AFILE
NRTNFLE3 FILE      FIXED=236
NRTNFLD  DIM       7
NRTNFLD2 DIM       48
NRTNFLAG FORM      1
NRTNFLG2 FORM      1
NRTNFLG3 FORM      1
NRTNPATH FORM      1
NRTNLOCK FORM      1    0 or 1=File Locks, 3=Record Locks, 3=No Locks
.
...............................................................................
* 
rtnvars  list
RTNUM    DIM       4      1-4    RETURN-TO NUMBER. **KEY**
.RTCNTCT  DIM       25     5-29   RETURN-TO CONTACT NAME.
RTCNTCT  DIM       45     5-49   RETURN-TO CONTACT NAME.
.RTCOMP   DIM       25    30-54   RETURN-TO COMPANY NAME.
RTCOMP   DIM       45    50-94   RETURN-TO COMPANY NAME.
RTADDR   DIM       25    95-119  RETURN-TO ADDRESS.
RT2ADDR  DIM       25   120-144  RETURN-TO ADDRESS2.
RTCITY   DIM       15   145-159  RETURN-TO CITY.
RTSTATE  DIM       2    160-161  RETURN-TO STATE.
RTZIP    DIM       10   162-171  RETURN-TO ZIP.
RTCOUN   DIM       15   172-186  RETURN-T0 COUNTRY CODE
RTNAME   DIM       10   187-196  PASSWORD NAME
.RTREVDAT DIM       6    117-122  REVISED DATE.
RTREVDAT DIM       8    197-204  REVISED DATE.
.RTBLANK  DIM       3    205-207  NOT-USED.
RTBLANK  DIM       2    205-206  NOT-USED.
RTActive Dim       1    207-207  ' ','Y'=active 'N' = do not use
RTTELE   DIM       10   208-217  TELEPHONE
RTFAX    DIM       10   218-227  FAX NUMBER.
RTCOMPN	 Dim	   6	228-233  Company number
RTCNTN	 Dim	   3	233-236  Contact Number 
	 listend
.
