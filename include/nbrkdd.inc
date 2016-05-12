.* *****************************************************************************
.* NAMES IN THE NEWS MASTER BROKER  FILE.
.* *****************************************************************************
..
..    FILE:      NINBRK
..  LENGTH:      233
..COMPRESS:      NONE
..    TYPE:      ISAM
..     KEY:      BRKUM,AIM=BKCOMP
. for variable defs only data is in company file
.DLH last change 28 sep 2008 add email
. DLH 23July2002   ---- Inactive Flag
. DLH 22July2002   --- add OFax Flag
.. ASH 17JUL98 Y2K, INCREASED BRKCNTCT-45,BRCOMP-45, ADDED NBRKLOCK FOR IO, ADDED BR2ADDR,BRCOUN
.. DLH 03AUG93 ADDED SALESPERSON
.. DLH 5JAN93 ADDED PHONE & FAX #'S.
.. DLH 20JAN93 CONTACT OPTION ADDED RECORD EXPANDED BY 28.
................................................................................
.	include	\\nts0\c\library\INCLUDE\compdd.inc
.	include	\\nts0\c\library\INCLUDE\cntdd.inc
.
NBRKFILE IFILE     KEYLEN=7,FIXED=233
. 
NBRKFLE2 AFILE     FIXED=233 
.            
NBRKFLE3 FILE      FIXED=233
NBRKNAME INIT      "NINBRK  "
NBRKFLD  DIM       7
NBRKFLD2 DIM       93
NBRKFLAG FORM      1
NBRKFLG2 FORM      1
NBRKFLG3 FORM      1
NBRKPATH FORM      1
NBRKLOCK FORM      1    0 or 1=File locks, 2=Record locks, 3=No locks
.* 
..BRKNUM   DIM       3      1-3    BROKER NUMBER. **KEY**
NBRKVARS LIST
BRKNUM   DIM        4      1-4    BROKER NUMBER KEY PART A... 20JAN93.
BRKCNT   DIM        3       5-7    BROKER CONTACT. KEY PART B...20JAN93.
.BRCNTCT  DIM       25      8-32   BROKER CONTACT NAME
BRCNTCT  DIM       45       8-52   BROKER CONTACT NAME.
.BRCOMP   DIM       25     33-57   BROKER COMPANY NAME
BRCOMP   DIM       45     53-97   BROKER COMPANY NAME.
BRADDR   DIM       25     98-122   BROKER ADDRESS
BR2ADDR  DIM       25    123-147  BROKER ADDRESS2
BRCITY   DIM       15    148-162  BROKER CITY.
BRSTATE  DIM       2     163-164  BROKER STATE.
BRZIP    DIM       10    165-174  BROKER ZIP. LEFT JUST.
BRCOUN   DIM       15    175-189  BROKER COUNTRY CODE
BRNAME   DIM       10    190-199  PASSWORD NAME
..BRREVDAT DIM       6     120-125  REVISED DATE.
BRREVDAT DIM       8     200-207  REVISED DATE.
BRCREDIT DIM       1     208-208  CREDIT STATUS.
..BRBLANK  DIM       3     209-211  NOT-USED.
.BRBLANK  DIM       2     209-210  NOT-USED.
BRBlank       Dim            1               209-209      ;not used  
BRInactive    Dim            1               210-210      ;"T" = Inactive 
BrFaxOFlag    Dim            1               211-211      ;"T" = Fax order confirmations
BRTELE   DIM       10    212-221  TELEPHONE
BRFAX    DIM       10    222-231  FAX.
BRSALES  DIM       2     232-233  SALES PERSON NUMBER
BREMAIL	Dim	50
         LISTEND
.. 
        LISTOFF
..PROGRAM ACCESS.
..NAME           TYPE          ACCESS
..FILES FOUND IN F:\LIBRARY\DEVELOP
..BROKERTEMP                   R,W,U
..NEOM0011.pls                 R
..NLCR0002.DLH                 R
..NMPL0002.pls                 R
..bonnie.pls                   R
..tinv0005.pls                 R

..FILES FOUND IN F:\LIBRARY\PLB_SRC
..NADJ002L.PLS                 R
..NADJ0099.PLS                 R
.NBRK0001.PLS                 R,W,U
.NBRK0002.PLS                 R
.NBRK0003.PLS                 R
.NBRK0006.PLS                 R,U
.NBRK317.PLS                  R
.NBRK7777.PLS                 R,W,U
.NCSH0003.PLS                 R
.NEOM0011.PLS                 R
.NEOM0016.PLS                 R,U
.NEOM0017.PLS                 R,U
.NEOM001A.PLS                 R
.NEOM0020.PLS                 R
.NEOM0027.PLS                 R
.NEOM0098.PLS                 R
.NEOM0099.PLS                 R
.NEOM9999.PLS                 R
.NINC0010.PLS                 R
.NINV0003.PLS                 R
.NINV0006.598                 R
.NINV0016.PLS                 R
.NINV0017.PLS                 R
.NINV0020.PLS                 R
.NINV0021.PLS                 R
.NINV002L.PLS                 R
.NINV0099.PLS                 R
.NLCR0002.PLS                 R
.NMLR0001.PLS                 R
.NONA0001.PLS                 R
.NONA0002.PLS                 R
.NONA0002.old                 R
.NONA0003.PLS                 R
.NONA0008.PLS                 R
.NONA0099                     R
.NORD0006.598                 R
.NORD0007.PLS                 R
.NORD0009.PLS                 R
.NORD0029.PLS                 R
.NORD013Z.PLS                 R
.Ndat0004.pls                 R,U
.Ndat0099.PLS                 R,U
.Neom0001.PLS                 R
.Neom0004.PLS                 R
.Newb0001.PLS                 R
.Ninc0007.jul98               R
.Ninv0001.PLS                 R
.Ninv0006.PLS                 R
.Nlcr0001.pls                 R
.Nord0001.PLS                 R
.Nord002l.PLS                 R
.Nord0040.PLS                 R
.Nord013b.pls                 R
.Nord013c.PLS                 R
.Nord013d.PLS                 R
.Nord013e.pls                 R
.Nshp0004.pls                 R
.Neom0004.698                 R
.Ninc0007.pls                 R
.Nord0006.pls                 R
.inv0004.pls                 R

..FILES FOUND IN F:\APPS\WINBATCH\LIVE
..EOM1ST.WBT
..MISC.WBT
..PERGE.WBT
..FRIDAY.WBT
..FRIDAY.BAK
        LISTON

