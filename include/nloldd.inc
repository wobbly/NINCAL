...................................................
.
. NLOLDD INCLUSION
. NIN supplimental (List of List) ORDER FILE DEFINITION
.
. FILE NAME : NINLOL.DAT
. REC LENGTH: 516 FIXED
. INDEX KEY :   (1)     003-008 (LOL Number- Hidden value!!!)
.               (2)     009-014 (Campaign Number)
.
. AAM KEYS  :   (1)     009-020 (Campaign Number + List Number)
.               (2)     021-026 (LR/LCR Number)
.               (3)
.               (4)
.               (5)
.
.;                  PATCH 1.1 DMB 18JUN2005  Changed IP of File Manager
NLOLNAME        DIM 28
.>Patch 1.1 Begin
.NLOLNME         INIT    "NINLOL.ISI|20.20.30.103:502 "
NLOLNME         INIT    "NINLOL.ISI|NINS1:502 "
.NLOLNME1        INIT    "NINLOL1.ISI|20.20.30.103:502"
NLOLNME1        INIT    "NINLOL1.ISI|NINS1:502"
.NLOLNME2        INIT    "NINLOL.AAM|20.20.30.103:502"
NLOLNME2        INIT    "NINLOL.AAM|NINS1:502"
.>Patch 1.1 End
.NLOLNME         INIT    "NINLOL.ISI "
.NLOLNME1        INIT    "NINLOL1.ISI"
.NLOLNME2        INIT    "NINLOL.AAM "
NLOLFILE        IFILE   KEYLEN=6,FIXED=516
NLOLFLE1        IFILE   KEYLEN=6,FIXED=516
NLOLFLE2        AFILE   FIXED=516
NLOLFLD         DIM     6       LOL Number(ISAM) Hidden value!!!
NLOLFLD1        DIM     6       Campaign Number(ISAM)
NLOLFLD2        DIM     15      Campaign Number + List Number(AAM)
NLOLFLD3        DIM     9       .OBSOLETE. LR Number(AAM)
NLOLFLD4        DIM     9
.NLOLFLD4        DIM     15     P.O. Number(AAM)
.NLOLFLD5        DIM     5      Contact(AAM)
NLOLFLAG        FORM    1
NLOLFLG1        FORM    1
NLOLFLG2        FORM    1
NLOLPATH        FORM    1
NLOLLOCK        FORM    1       0/1=FILEPI, 2=RECORD LOCK, 3=NO LOCK
.
NLOL2           DIM     2       Used to Extract first 2 bytes
NLOL24          DIM     24      Used to Extract next 24 Bytes(ALL Keys!!) w/o destroying preserved values
NLOLVARS        LIST
NLOLCode        DIM     1       001-001 Code for Busy byte
NLOLStat        DIM     1       002-002 LOL Status
NLOLLOL         DIM     6       003-008 LOL Number - Hidden value!!!!!
NLOLCNum        DIM     6       009-014 Campaign Num(Key + AAMKey 1)   
NLOLList        DIM     6       015-020 List Number(Key + AAMKey 1)
NLOLLCR         DIM     6       021-026 Associated LR Number(AAMKey 2) - created from this record
NLOLOwner       DIM     4       027-030 Owner Number
NLOLSelect      DIM     35      031-065 Select
.NLOLUniverse    DIM     9       066-074 List Universe
NLOLUniverse    DIM     10       066-075 List Universe - OBSOLETE AFTER DATACARD CONVERSION

NLOLDate        DIM     8       076-083 Record Date
NLOLModDate     DIM     8       084-091 Modification Date
NLOLInits       DIM     3       092-094 Modification Initials
NLOLQty         DIM     9       095-103 Quantity
NLOLNetQty      DIM     9       104-112 Net Quantity
NLOLTest        DIM     1       113-113 Test/Continuation ' '/0=Cont 1=Test, 2=Retest
NLOLRent        DIM     1       114-114 Exchange/Rent 1=Exchange, 2=Rent, 3=Exchange/Rent
.NLOLPrice       DIM     6       114-119 Average Price per Thousand
NLOLPrice       DIM     7       115-121 Average Price per Thousand
NLOLNet         DIM     6       122-127 Average Net %
NLOLRate        DIM     6       128-133 Response Rate Change %
NLOLGift        DIM     6       134-139 Gift Change %
NLOLNetReq      DIM     6       140-145 Net Requested %
NLOLNetApp      DIM     6       146-151 Net Approved %
NLOLRegional    DIM     1       152-152 Regional
.----------VARIANCE FIELDS----------- CAMPAIGN WILL CONTAIN MASTER
NLOLOffer       DIM     3       153-155 Offer
NLOLSample      DIM     3       156-158 Sample
NLOLPackage     DIM     2       159-160 Package
NLOLMDate       DIM     8       161-168 Mail Date
.
NLOLComment     DIM     150     169-318 Internal Special Instructions - copies over to NINSPEC1.DAT when Order record created
NLOLComment1    DIM     150     319-468 External Special Instructions
NLOLLR          DIM     6       469-474 LR Master record - used to create this record
.LOLFiller      DIM     44      473-516 Filler
NLOLFiller      DIM     42      475-516 Filler
                listend
