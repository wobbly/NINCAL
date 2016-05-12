.;    Last change:  BC   27 Jan 2003    7:24 am
.* *****************************************************************************
.* NAMES IN THE NEWS MASTER EOM SALES  FILE.
.* *****************************************************************************
.
.    FILE:      NINSLS
.  LENGTH:      163
.COMPRESS:      
.    TYPE:      SEQ
.     KEY:      NONE
.revised      24Sep07   PLI    Patch 1.3
.revised      27APr99   DLH NININV Y2k patch 1.2
. REVISED     20JAN99   ASH NINORD Y2K, File expansion Patch #1.1
.revised      10mar95   dlh added adjust flag
. REVISED     17FEB95   ADDED CHECK DATE/MCOMP .
. REVISED     19APR93   ADDED OUTSIDE GUAR CODE BYTE 15, TAKEN FROM ORDER
.             02nov93   added  a/r for new list manag statement
...............................................................................
NSLSFILE FILE
NSLSFLE2 IFILE     KEYLEN=4
NSLSNAME   INIT    "NINSLS|NINS1:502           "
nslsfld    dim     4
NSLSFLAG   FORM    1
NSLSPATH   FORM    1
.
SLSvars   list
SLSMLR    FORM      4       1-4     MAILER NUMBER.            NIN
.begin patch 1.3 PLI COnversion
.SLSLR     FORM      6       5-10    LIST RENTAL NUMBER.       NIN
SLSLR     Dim        6       5-10    LIST RENTAL NUMBER.       NIN
.end patch 1.3 PLI COnversion
SLSOWN    FORM      4      11-14    LIST OWNER NUMBER.        NIN
SLSGUAR1  DIM       1      15-15    OUTSIDE GUARANTY          NIN
SLSCNT    DIM      3      16-18    BROKER NUMBER.            NIN
.SLSCNT    FORM      3      16-18    BROKER NUMBER.            NIN
.SLSLIST   FORM      6      19-24    LIST NUMBER.              NIN
SLSLIST   FORM      6      19-24    LIST NUMBER.              NIN
.Start Patch #1.1 - increased var
.SLSMDTE   FORM      6      25-30    MAIL DATE.                NIN
.SLSAP1    FORM      7      33-39    ACCOUNT PAYABLE ONE.      NIN
SLSMDTE   FORM      8      25-32    MAIL DATE (CCYYMMDD)      NIN
.start patch 1.2
.SLSAP1    FORM      9      33-41    ACCOUNT PAYABLE ONE.      NIN
SLSAP1    FORM      10.2    33-45    ACCOUNT PAYABLE ONE.      NIN
.end patch 1.2
.End Patch #1.1 - increased var
SLSDJCD   DIM       1      46-46    DOW JONES CODE.           NIN
SLSADJCD  DIM       1      47-47    ADJUSTMENT CODE.          NIN
.Start Patch #1.1 - increased var
.SLSIDTE   FORM      6      44-48    INVOICE DATE.             NIN
SLSIDTE   FORM      8      48-55    INVOICE DATE (CCYYMMDD)   NIN
.END Patch #1.1 - increased var
.begin patch 1.2
.SLSAP2    FORM      9      52-60    ACCOUNTS PAYABLE TWO.     NIN
SLSAP2    FORM      10.2      56-68    ACCOUNTS PAYABLE TWO.     NIN
.end patch 1.2
SLSLST1   DIM       35     69-103    LIST DESCRIPTION ONE.     NIN
SLSCODE   DIM       1     104-104    CREDIT/DEBIT CODE,'C or D'NIN
SLSGUAR   DIM       1     105-105    GUARANTY CODE.            NIN
.Start Patch #1.1 - increased var
.slsAR     DIM       8      94-101   A/R (NO DECIMAL)
.begin patch 1.2
.slsAR     DIM       9      98-106   A/R (NO DECIMAL)
slsAR     form    10.2    106-118   A/R (NO DECIMAL)
.end patch 1.2
.End Patch #1.1 - increased var
.begin patch 1.2
.SLSCHKDTE DIM       6      107-112  INV CHECK DATE
SLSCHKDTE DIM       8      119-126  INV CHECK DATE ccyymmdd
.end patch 1.2
SLSCNAME  DIM      25      127-151  CLIENT NAME
slsadjsw  form      2      152-153  adjustment switch 2-adjusted
slsxchrg  form      7.2    154-163
          listend
. .............................................................................
. 
        LISTOFF
.PROGRAM ACCESS.
.NAME           TYPE          ACCESS
.NEOM0013
        LISTON

