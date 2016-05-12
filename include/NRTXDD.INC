* NRTXDD/INC.
* *****************************************************************************
* NAMES IN THE NEWS MASTER RETURN TO TAX FILE.
* *****************************************************************************
.
.    FILE:      NINRTAX
.  LENGTH:      34
.COMPRESS:      NONE
.    TYPE:      ISAM,FIXED
.     KEY:      NRTXFLD
...............................................................................
.
NRTXFILE IFILE     KEYLEN=4,FIX=34
NRTXNAME INIT      "NINRTAX "
NRTXFLD   DIM       4
NRTXFLAG FORM      1
.
RTXNUM   DIM       4      1-4          RETURN-TO NUMBER ---KEY--.
RTXCD    DIM       5      5-9          TAX AREA CODE
RTXPERC  DIM       4     10-13         TAX PERCENTAGE
RTXCODE  DIM       1     42-14         CODE TO INDICATE WHICH EXEMPTION
RTXEXMPT DIM       20    15-34         EXEMPTION NUMBER
.
.
.          T - TAX EXEMPT CODE #
.          D - DIRECT PAYMENT #
.          R - RESALE CERT. #
.
                LISTOFF
.PROGRAM ACCESS.
.NAME           TYPE          ACCESS
.NINP21         DATABUS
.RTNMOD         DATABUS
                LISTON
