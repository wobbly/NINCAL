.............................................................................
. NCSHDD INCLUSION
. CASH FILE DESCRIPTION.
.
. FILE NAMES: DAT25Nx & DAT25Px      patch 1.5
. REC LENGTH: 132
. INDEX     : (1)   9-14  LR #
. AIM KEY   : (1)   1-2   DUPE ID
.             (2)  25-37  AMOUNT.
.             (3)  40-42  CONTROL #.
.             (4)  43-54  CHECK #
.             (5) 110-117 CONTROL DATE
.Patch 1.6  DLH   09/12/2007  PLI add External code I - INter company transaction
.Patch 1.5  DLH   06/12/2007  PLI
.patch 1.4  ASH   01/10/2005 Increased Mailer/Broker/Check fields.
.patch 1.3  ASH   Increased ISAM for Control, so added Control date.  Added filler.
.patch 1.2   Added FileList, Added Check as AamKey
.patch 1.1   aug99 DLH get rid of EBCDIC
..............................................................................
NCSHFILE FILE
.START PATCH 1.2 REPLACED LOGIC
.         IFNZ       PC
.NCSHFIL2 AFILE
.         XIF
.         IFZ        PC
..NCSHFIL2 AFILE      15,2,,FIXED=71,uncomp
..NCSHFIL2 AFILE      FIXED=71,uncomp
..begin patch 1.1
..NCSHFIL2 AFILE      FIXED=93,uncomp                 .-23-29,32-35
.NCSHFIL2 AFILE      FIXED=99,uncomp                  .-23-35,38-40,41-46
.         XIF
..NcshFiL3 IFILE     KEYLEN=6,FIXED=71,DUP,uncomp
..NcshFiL3 IFILE     KEYLEN=6,FIXED=93,DUP,uncomp
.NcshFiL3 IFILE     KEYLEN=6,FIXED=99,DUP,uncomp           .-7-12
..end patch 1.1
NCSHLIST  Filelist
NCSHFIL2 AFILE  NAME="DAT25N.AAM"
NcshFiL3 IFILE  NAME="DAT25N.ISI"
          Filelistend

.END PATCH 1.2 REPLACED LOGIC
NCSHNAME INIT       "DAT25N  "
NCSHFLD DIM        16
NCSHFLD2 DIM        6
NCSHFLD3 DIM        6
.START ADDED FOR NEW GUI PROGRAM
.START PATCH 1.4 REPLACED LOGIC
.NCSHFLD4 DIM        9
NCSHFLD4 DIM        15
.END PATCH 1.4 REPLACED LOGIC
NCSHFLD1 DIM        5
.END ADDED FOR NEW GUI PROGRAM
.START PATCH 1.3 ADDED LOGIC
NCSHFLD5 DIM        11
.END PATCH 1.3 ADDED LOGIC
NCSHFLAG FORM       1
NCSHFLG2 FORM       2
NCSHFLG3 FORM       2
NCSHPATH FORM       1       1=SEQ,2=AIM
NCSHLOCK FORM       1       0 or 1=File Locks, 2=Record Locks, 3=No Locks
...............................................................................
cashvars list
CID      DIM        2     POSITION    1-2      RECORD ID.
.START PATCH 1.4 REPLACED LOGIC
.CMLR     DIM        4     POSITION    3-6      MAILER NUMBER.
CMLR     DIM        6     POSITION    3-8      MAILER NUMBER.
.END PATCH 1.4 REPLACED LOGIC
.DURING CONVERSION ON 1/25/2005 I forgot to add 3 blank spaces here for future increase in LR number.  ASH
CLR      DIM        6     POSITION    9-14     LIST RENTAL NUMBER.
CFILL1   DIM        2     POSITION   15-16     NOT USED.
CCE      DIM        2     POSITION   17-18     CENTURY RECORD ENTERED.
CYR      DIM        2     POSITION   19-20     YEAR RECORD ENTERED.
CMO      DIM        2     POSITION   21-22     MONTH RECORD ENTERED.
CDY      DIM        2     POSITION   23-24     DAY RECORD ENTERED.
.CMO      DIM        2     POSITION   15-16     MONTH RECORD ENTERED.
.CDY      DIM        2     POSITION   17-18     DAY RECORD ENTERED.
.CYR      DIM        2     POSITION   19-20     YEAR RECORD ENTERED.
..ASH Added following field 06Nov98 for Y2K compliance, increased position for other fields
.CCE      DIM        2     POSITION   21-22     CENTURY RECORD ENTERED.
.begin patch 1.1
.CAMOUNT  DIM        7     POSITION   23-29     AMOUNT OF CASH IN.  minus overpunch
CAMOUNT  FORM      10.2     POSITION   25-37     AMOUNT OF CASH IN.
.end patch 1.1
CEXTCD   DIM        1     POSITION   38-38     EXTERNAL CODE.
./////////////////////////////////////////////////////////////////////////////////////////////////////////
.// A = Do not close invoice. Mlr still owes, but cut check to LO. & flag/adjust A/P
.//       not yet implemented.
.// D = Apply Money to MOA requires MLR/BRK/CHK Number LR optional
.// d = Apply Money to MOA code to Duplicate Payment
.// I = Intercompany Transfer
.// M = Manual - discouraged.
.// N = Apply Money to MOA code to Advance Payment
.// O = Apply funds to A/R from Money on Account.
.// P = Owner was previously paid do not cut check or overwrite manual check info.
.// Q = TYPE P + TYPE O.
./////////////////////////////////////////////////////////////////////////////////////////////////////////

CFILL2   DIM        1     POSITION   39-39     NOT USED.
CNUM     DIM        3     POSITION   40-42     CONTROL NUMBER.
.START PATCH 1.4 REPLACED LOGIC
.NCSHCHK  DIM        6                43-48     Mailers check number.
NCSHCHK  DIM        12               43-54     Mailers check number.
.END PATCH 1.4 REPLACED LOGIC
nckdtec  dim        2                55-56     Mailer check date century.
nckdtey  dim        2                57-58     Mailer check date year.
nckdtem  dim        2                59-60     Mailer check date month.
nckdted  dim        2                61-62     Mailer check date day.
.nckdtey  dim        2                51-52     Mailer check date year.
.nckdtec  dim        2                53-54     Mailer check date century.
.ASH Increased following field 06Nov98 to reflect new size of field in NINMLR/NINBRK
.npayor   dim       25                49-73     mailer/broker who paid us
.START PATCH 1.2 REPLACED LOGIC
.npayor   dim       45                55-99     mailer/broker who paid us
npayor   dim       41                63-103     mailer/broker who paid us
.START PATCH 1.4 REPLACED LOGIC
.NCSHBRK  DIM       4                 96-99     Broker Number
NCSHBRK  DIM       6                 104-109     Broker Number
.END PATCH 1.4 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
.START PATCH 1.3 ADDED LOGIC
CNUMDATE  DIM       8            110-117    Control Date
.begin patch 1.5
CCOMPID   Dim       1         118-118     COmpany ID  "N" or " " = NIN "P" = PLI                                    
.end patch 1.5
CFiller    dim        14                    119-132    Filler 
.CFiller   dim        15                    118-132    Filler 
.end patch 1.5
.END PATCH 1.3 ADDED LOGIC
         listend
..
        LISTOFF
.PROGRAM
.
.NCHK0001
.NCHK0002
.NCHK0003
.       
        LISTON
