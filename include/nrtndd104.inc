;    Last change: DLH 6/13/2002 11:01:20 AM
;.............................................................................
;
; NRTNDD INCLUSION
; NIN RETURN-TO FILE DEFINITION
;
; FILE NAME : NINRTN
; REC LENGTH: 227 FIXED. INDEX KEY : 1-4 (4 POSITION RTN#)
; AIM KEY   : 50-94
;
. RElease   1.3	24Apr07	DLH added file manager 
. RElease   1.2 	12April2007 DLH  inactive byte
. RElease   1.1 	19AUG98 ASH Y2K CONVERSION
;.............................................................................
;
NRTNFILE IFILE     KEYLEN=4,FIXED=227,UNCOMP,Name="NINRTN.isi|10.10.30.104:502"
NRTNNAME INIT      "NINRTN|10.10.30.104:502  "
NRTNFLE2 AFILE
NRTNFLE3 FILE      FIXED=227
NRTNFLD  DIM       7
NRTNFLD2 DIM       48
NRTNFLAG FORM      1
NRTNFLG2 FORM      1
NRTNFLG3 FORM      1
NRTNPATH FORM      1
NRTNLOCK FORM      1    0 or 1=File Locks, 3=Record Locks, 3=No Locks
;
;..............................................................................
* 
rtnvars  list
RTNUM    DIM       4      1-4    RETURN-TO NUMBER. **KEY**
;RTCNTCT  DIM       25     5-29   RETURN-TO CONTACT NAME.
RTCNTCT  DIM       45     5-49   RETURN-TO CONTACT NAME.
;RTCOMP   DIM       25    30-54   RETURN-TO COMPANY NAME.
RTCOMP   DIM       45    50-94   RETURN-TO COMPANY NAME.
RTADDR   DIM       25    95-119  RETURN-TO ADDRESS.
RT2ADDR  DIM       25   120-144  RETURN-TO ADDRESS2.
RTCITY   DIM       15   145-159  RETURN-TO CITY.
RTSTATE  DIM       2    160-161  RETURN-TO STATE.
RTZIP    DIM       10   162-171  RETURN-TO ZIP.
RTCOUN   DIM       15   172-186  RETURN-T0 COUNTRY CODE
RTNAME   DIM       10   187-196  PASSWORD NAME
;RTREVDAT DIM       6    117-122  REVISED DATE.
RTREVDAT DIM       8    197-204  REVISED DATE.
.RTBLANK  DIM       3    205-207  NOT-USED.
RTBLANK  DIM       2    205-206  NOT-USED.
RTActive Dim       1    207-207  ' ','Y'=active 'N' = do not use
RTTELE   DIM       10   208-217  TELEPHONE
RTFAX    DIM       10   218-227  FAX NUMBER.
         listend
;
        LISTOFF
;PROGRAM ACCESS.
;F:\LIBRARY\PLB_SRC
;NAME           TYPE            ACCESS
;NCHK002L       PLS             READ
;NINC0007       PLS             READ
;NINV0001       PLS             READ
;NINV0003       PLS             READ
;NINV002L       PLS             READ
;NINV0099       PLS             READ
;NMRG0001       PLS             READ
;NMRG0002       PLS             READ
;NORD0001       PLS             READ
;NORD0009       PLS             READ
;NORD0023       PLS             READ
;NORD0024       PLS             READ
;NORD002L       PLS             READ
;NORD013B       PLS             READ
;NORD013C       PLS             READ
;NORD013D       PLS             READ
;NORD013E       PLS             READ
;NORD013Z       PLS             READ
;NRTN0001       PLS             READ,WRITE,UPDATE
;NRTN0002       PLS             READ
;NRTN7777       PLS             READ
;NSHP0001       PLS             READ
;NXCH0001       PLS             READ
;NXCH0099       PLS             READ

;F:\LIBRARY\DEVELOP
;NAME           TYPE            ACCESS
;NMPL0001       PLS             READ
;NMPL0002       PLS             READ

;F:\APPS\PLB\CODE
;NAME           TYPE            ACCESS
;EOM1ST         WBT
;MISC           WBT
;PERGE          WBT
;FRIDAY         BAK
;FRIDAY         WBT
        LISTON        
