* *****************************************************************************
* NAMES IN THE NEWS MASTER MERGE  FILE.
* *****************************************************************************
.
.    FILE:      NINMERGE
.  LENGTH:      465
.COMPRESS:      YES
.    TYPE:      ISAM
.     KEY:      NMRGFLD
. JD  17Sep2007 added new var nmrgcnr
. DLH 8Feb2007  FIle manager 
. JD 05DOct5 Disaster deduct created.
. JD 09Dec04 DPV no longer valid deduct.
. JD 24JUn04 record expanded.
. JD 24aug95 added new variable.
. DLH 6JAN93 CREATED
...............................................................................


.19April 08 DH remove compression add no dupe
.MinXFILE	IFILE	KeyLen=6,FIXED=20,Name="Min2NIN.ISI|NINS1:502"

.NMRGFILE IFILE     KEYLEN=6,COMP,VAR=401
.NMRGFILE IFILE     KEYLEN=6,COMP,VAR=465,Name="NINMERGE.ISI|NINS1:502"
NMRGFILE IFILE     KEYLEN=6,NODUPE,VAR=465,Name="NINMERGE.ISI|NINS1:502"
NMRGFLE1 IFILE     KEYLEN=6,Name="NINMRGE.ISI|NINS1:502"
.........NOTE .DAILY PRINT FILE LEN 9

.NMRGFILE IFILE     KEYLEN=6,COMP,VAR=465
.NMRGFLE1 IFILE     KEYLEN=6       .DAILY PRINT FILE LEN 9
NMRGNAME INIT      "NINMERGE.ISI|NINS1:502"
NMRGNME1 INIT      "NINMRGE.ISI|NINS1:502"
NMRGFLD  DIM       6
NMRGFLAG FORM      1
NMRGPATH FORM      1
*
NMRGVAR  LIST
.
. BEGINNING OF VARIABLE LIST.
.
NMRGLR   DIM       6      001-006  LR NUMBER. **KEY**
NMRGFILL DIM       4      007-010   FILL.
NMRGLNAM DIM       49     011-059  LIST NAME.
NMRGKCOD DIM       10     060-069  KEY CODE.
NMRGRQTY FORM      8      070-077  QTY REC'D
NMRGIQTY FORM      8      078-085  QTY INPUT
NMRGTREJ FORM      8      086-093  TOTAL REJECTS
NMRGID   FORM      8      094-101  INTRA DUPES.
NMRGNETI FORM      8      102-109  NET INPUT.
NMRGELIM FORM      8      110-117  ELIMINATOR HITS
NMRGHDRP FORM      8      118-125 HOUSE DROPS             *NEW
NMRGCS   FORM      8      126-133 CUSTOMER SUPPRESS       *THEse 2 fields were
NMRGUDUP FORM      8      134-141 UNUSED DUPLICATES       *swapped, early 94?.
NMRGND   FORM      8      142-149 NON DUPLICATES
NMRGDUPM FORM      8      150-157 DUPLICATES (MULTIES)
NMRGNET  FORM      8      158-165 NET OUTPUT
NMRGZIPV FORM      8      166-173 ZIPS VERIFIED
NMRGZIPC FORM      8      174-181 ZIPS CORRECTED
NMRGZIP4 FORM      8      182-189 ZIP+4 CRRT CODED
NCOAMWF  FORM      8      190-197 NCOA MTCH W/FWD
NCOAMNF  FORM      8      198-205 NCOA MTCH NO/FWD
NCOATOTM FORM      8      206-213 TOT NCOA MTCH
NIXIEM   FORM      8      214-221 NIXIE OPT MTCH
NCOAUNM  FORM      8      222-229 NCOA UNMTCHED
NCOANFRJ FORM      8      230-237 NCOA  NO/FWD REJECTS       *NEW
NCOANIX1 FORM      8      238-245 NCOA NIXIE 1 REJECTS      *NEW
NCOANIX2 FORM      8      246-253 NCOA NIXIE 2 REJECTS      *NEW
NCOANIX3 FORM      8      254-261 NCOA NIXIE 3 REJECTS      *NEW
NMRGERR  FORM      8      262-269 ERROR REJECTS.
NMRGDISF FORM      8      270-277 DEAD REJECTS.
NMRGNPER FORM      8      278-285 NONPERSONAL REJECTS
NMRGDMA  FORM      8      286-293 DMA REJECTS
NMRGELMX FORM      8      294-301 ELIMINIX REJECTS
NMRGZ4   FORM      8      302-309 ZIP+4 CRRT REJECTS
NMRGNIX  FORM      8      310-317 NIXIE REJECTS
NMRGTDMC FORM      8      318-325 TDMC REJECTS.
.NMRGEXIT FORM     8      326-333 EXIT REJECTS       *no longer used
NCOAREJ  FORM      8      326-333 NCOA REJECTS       *new field NIXIE OPT.
NMRGCUST FORM      8      334-341 CUSTOMER REJECTS
NMRGPRIS FORM      8      342-349 PRISON REJECTS
NMRGDROP FORM      8      350-357 MAILDROP REJECTS
NMRGHH   FORM      8      358-365 DROPED BY HOUSE HITS         *NEW
NMRGFAM  FORM      8      366-373 FAMILY DUPE DROPS            *NEW
nmrgconv form      8      374-381 Epsilon conversion drops - includes
.                                 military, bad address,
NMRGFIL1 DIM       4      382-385 NOT USED.
nmrgrep  form      8      386-393 republican hits (DNC)
nmrgnnet form      8      394-401 new netout after repub hits
nmrgdpv  form      8      402-409 DPV drops
nmrgdisa form      8      410-417 Disaster drops
nmrgcnr  form      8      418-425 CNRS
nmrgfil2 dim       40     426-465 not used.
.nmrgfil2 dim       48     418-465 not used.
.
.END OF LIST
.
         LISTEND
.
