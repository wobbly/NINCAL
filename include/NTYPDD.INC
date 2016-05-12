*        NTYPDD/INC.
* *****************************************************************************
* NAMES IN THE NEWS MASTER TYPIST ANALISYS FILE.
* *****************************************************************************
.
.    FILE:      TYPOUT
.  LENGTH:      83
.COMPRESS:      NONE
.    TYPE:      ISAM
.     KEY:      9 bytes date ccyymm & TYPist
...............................................................................
.RElease  2.41       Add counters for prog34, prog 9 (combined) record bumped to 110 bytes
.RElease  2.4       changed type to NtypType      DLH 22 March 2010
.Release      2.0          March 2006           combine to single records types, add new counters
NTYPNAME  INIT    "TYPOUT  "
.begin patch 2.0
.NTYPFILE  IFILE     KEYLEN=3,FIX=55,UNCOMP
.NTYPFILE  IFILE     KEYLEN=9,FIX=83,UNCOMP
NTYPFILE  IFILE     KEYLEN=9,FIX=110,UNCOMP
.NTYPFLD   DIM    3
NTYPFLD   DIM    9
.end patch 2.0
NTYPPATH  FORM     1
NTYPFLAG  FORM    1
.
.begin patch 2.0
.
.**      RECORD               TYPE                1 TOTAL     **
.         
.NtypTot       List
.;IDNUM         DIM             5               1-5           ALWAYS 99
.IDNUM         DIM             9               1-9           ALWAYS 'ccyymm99 '
.STotal        FORM            5              10-14          # OF NEW ORDERS
.RTotal        FORM            5              15-19          # REPRINT ORDERS
.QtyTotal      Form            9              20-28          Total qty
.LCSTot        FORM            5              29-33          # OF NEW LCRS
.LCRTot        FORM            5              34-38          # REPRINT LCRS
.INVTOT        FORM            5              39-43          #NEW INVOICES
.INVRTOT       FORM            5              44-48          #REPRINT INVOICES
.CORTOTAL      FORM            5              49-53          #NEW CORRECTIONS
.CANTOTAL      FORM            5              54-58          #NEW CANCELATIONS
.APPTOT        FORM            5              59-63          #NEW APPROVALS
.ADJTOT        Form            5              64-68          #of new Adjustments
.PndTot        Form            5              69-73          # pending prders
.lstTot        FOrm            5              74-78          # of new datacards
.LStUTot       Form            5              79-83          # updated datacards              
.;SCOUNT        FORM            5               6-10          # OF NEW ORDERS
.;RCOUNT        FORM            5              11-15          # REPRINT ORDERS
.;LCSCOUNT      FORM            5              16-20          # OF NEW LCRS
.;LCRCOUNT      FORM            5              21-25          # REPRINT LCRS
.;INVTOT        FORM            5              26-30          #NEW INVOICES
.;INVRTOT       FORM            5              31-35          #REPRINT INVOICES
.;CORTOTAL      FORM            5              36-40          #NEW CORRECTIONS
.;CANTOTAL      FORM            5              41-45          #NEW CANCELATIONS
.;APPTOT        FORM            5              46-50          #NEW APPROVALS
.;ADJTOT        Form            5              51-55          #of new Adjustments
.              ListEnd
.
.**       RECORD             TYPE                 2 DETAIL     **
.
NtypDet       List
TypDate       Dim             6              1-6           CCYYMM
NtypTYPE      DIM             3              7-9           TYPISTS INITIALS
.TYPE          DIM             5              1-5           TYPISTS INITIALS
SUBCOUNT      FORM            5              10-14           # NEW ORDERS
REPCOUNT      FORM            5              15-19          # REPRINT ORDERS
SUBQTY        FORM            9              20-28          # OF NAMES
LSUBCNT       FORM            5              29-33          # NEW LCRS
LREPCNT       FORM            5              34-38          # REPRINT LCRS
INVCOUNT      FORM            5              39-43          # NEW INVOICES
INVRCNT       FORM            5              44-48          # REPRINT INVOICES
CORCOUNT      FORM            5              49-53          # NEW CORRECTIONS
CANCOUNT      FORM            5              54-58          # NEW CANCELLATIONS
.APPCOUNT      FORM            5              59-63          # NEW APPROVALS
PndUCount     FOrm            5              59-63          #of PEnding Updates           2013 October 3
ADJCount      Form            5              64-68          #of new Adjustments
PndCOunt      Form            5              69-73          # of New pending
lstCount      FOrm            5              74-78          # of new datacards
LStUCOunt     Form            5              79-83          # updated datacards          
. adds
DBCount       Form            5              84-88          # new database records from prog 34 and prog 9 and 15?
DBUcount      Form            5              89-93          # Updated database records from prog 34 and prog 9 and 15?
NtypFill      dim             17             94-110         .fill
...
.SUBCOUNT      FORM            4              6-9           # NEW ORDERS
.REPCOUNT      FORM            4              10-13          # REPRINT ORDERS
.SUBQTY        FORM            9              14-22          # OF NAMES
.LSUBCNT       FORM            4              23-26          # NEW LCRS
.LREPCNT       FORM            4              27-30          # REPRINT LCRS
.INVCOUNT      FORM            4              31-34          # NEW INVOICES
.INVRCNT       FORM            4              35-38          # REPRINT INVOICES
.CORCOUNT      FORM            4              39-42          # NEW CORRECTIONS
.CANCOUNT      FORM            4              43-46          # NEW CANCELLATIONS
.APPCOUNT      FORM            4              47-50          # NEW APPROVALS
.ADJCount      Form            5              51-55          #of new Adjustments
              Listend
.
.end patch 2.0
