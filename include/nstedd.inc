..............................................................................
.
. NSTEDD INCLUSION
. NIN STATEMENT FILE
.
. FILE NAME : NINSTE
. REC LENGTH: 176
. INDEX KEY :
.
.patch    07Dec07 DLH 1.22 - Create Pl FIle
.patch    18mar04 Jd 1.21 - turned on steadjsw.
.patch    27apr99 DLH 1.2   - nininv y2k increased check date year
.revised  20Jan99 Patch #1.1 - NINORD Y2K, increased Mail Date Year
.revised  10mar95 add adjustment switch
.REVISED   27dec94 expanded client name field from 5 to 25.
.REVISED  28JUN94 ADD OWNER NUMBER & check date.
.revised  03FEB94 TO INCLUDE ORDER SALESPERSON #.
.revised  27apr93 to include broker/contact number
.revised  19apr93 filler was 4 now 3, outside guar code added,
.         pulled from order.
..............................................................................
.
.Start Patch #1.1 - increased var
.NSTEFILE FILE      FIX=155
.NSTEFILE FILE      FIX=160
.End Patch #1.1 - increased var
.begin patch 1.2
NSTEFILE FILE      FIX=176
.end patch 1.2
.begin patch 1.22
NSTEPFILE FILE      FIX=176
.end patch 1.22
NSTENAME INIT      "NINSTE|NINS1:502           "
NSTEFLAG FORM      1
.
stevars  list
STECODE  DIM       2         001-002   FILLER  'F0'
STEMLR   DIM       4         003-006   MAILER NUMBER
STELR    DIM       6         007-012   LR NUMBER
STEBILTO DIM       1         013-013   BILL-TO CODE.
STEINVNO DIM       6         014-019   INVOICE NUMBER
.Start Patch #1.1 - increased var
.STEINVDT DIM       6         020-025   INVOICE DATE (MMDDYY)
.STEAR    DIM       8         026-033   A/R (NO DECIMAL)
STEINVDT DIM       8         020-027   INVOICE DATE (CCYYMMDD)
.begin patch 1.2
.STEAR    DIM       9         028-036   A/R (NO DECIMAL)
STEAR    form      10.2       028-040   A/R
.end patch 1.2
.End Patch #1.1 - increased var
STECNT   DIM       3         041-043   MAILER CONTACT#
STEMLRPO DIM       12        044-055   MAILER PO
.Start Patch #1.1 - increased var
.STEAP1   DIM       7         049-055   A/P1 (NO DECIMAL)
.STEAP2   DIM       9         056-064   A/P2 (NO DECIMAL)
.STEMLDDT DIM       6         065-070   MAIL DATE (MMDDYY)
.begin patch 1.2
.STEAP1   DIM       9         052-060   A/P1 (NO DECIMAL)
.STEAP2   DIM       9         061-069   A/P2 (NO DECIMAL)
STEAP1   form      10.2       056-068   A/P1
STEAP2   form      10.2       069-081   A/P2
.end patch 1.2
STEMLDDT DIM       8         082-089   MAIL DATE (CCYYMMDD)
.End Patch #1.1 - increased var
STEGRNTE DIM       1         090-090   GUARANTEED PAYMENT CODE (*)
STELNAME DIM       35        091-125   LIST NAME
STEGUAR  DIM       1         126-126   OUTSIDE GUARANTY.    19APR93 DLH
STEFIL3  DIM       3         127-129   filler               19apr93
STECNAME DIM       25        130-154   CLIENT NAME (FOR ALPHA SORTING)
stebrk   dim       4         155-158   broker/consultant nnumber
stebrkct dim       3         159-161      "       "      contact number
STESLS10 DIM       1         162-162   TENS DIGIT SALESPERSON NUMBER
STESALES DIM       1         163-163   ONES DIGIT SALESPERSON NUMBER
STEOWN   DIM       4         164-167   LIST OWNER NUMBER.
.steckdte dim       6         156-161   inv check paid date
steckdte dim       8         168-175   inv check paid date ccyymmdd
steadjsw dim       1         176-176   1=adjusted 2=adjusted shortpay 18mar04 jd
         listend

