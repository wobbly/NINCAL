* NMTXDD/INC.
* *****************************************************************************
* NAMES IN THE NEWS MASTER MAILER TAX FILE.
* *****************************************************************************
.
.    FILE:      NINMTAX
.  LENGTH:      34
.COMPRESS:      NONE
.    TYPE:      ISAM,FIXED
.     KEY:      NMTXFLD
...............................................................................
.
NMTXFILE IFILE     KEYLEN=6,FIX=36
NMTXNAME INIT      "NINMTAX|NINS1:502 "
NMTXFLD   DIM       6
NMTXFLAG FORM      1
.
mtxvars  list
MTXNUM   DIM       6      1-6          MAILER NUMBER ---KEY--.
MTXCD    DIM       5      7-11          TAX AREA CODE
MTXPERC  DIM       2     12-13         TAX PERCENTAGE
MTXCODE  DIM       1     14-14         CODE TO INDICATE WHICH EXEMPTION
MTXEXMPT DIM       20    15-34         EXEMPTION NUMBER
MTXC501  DIM       1     35-35         C501 STATUS MUST BE '3', '4', ' ',
.                                      OR '5'.
mtxproft dim       1     36-36         "Y" = For Profit company. 9/20/dlh
         listend
.                LISTOFF
.PROGRAM ACCESS.
.NAME           TYPE          ACCESS
.CHECKWRITER    DATABUS
.LISTNIN        DATABUS
.LISTNIN1       DATABUS
.MLRMOD         DATABUS
.NINP21         DATABUS
.                LISTON
