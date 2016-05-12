...............................................................................
.'Nord0012' READS BOTH ORDER FILES STARTING AT THE FIRST LR NUMBER OF 19XX. IT
.THEN PRINTS THE TOTAL NUMBER OF ORDERS AND THEIR QUANTITIES BY MONTH AND YEAR.
.ON A SERIAL PRINTER.                                                         .
...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         include   hp.inc
         INC       OSLSPERN.inc
release 	init    "4.95"		08Mar2007  DLH	Oslspern.inc expansion
.RELEASE  init      "4.94"           JD 13Jan05 2005 run.
.RELEASE  init      "4.93"           DMB 18JUN05 File Manager IP change
.RELEASE  init      "4.92"           JD 13Jan05 2005 run.
.RELEASE  init      "4.91"             JD  04May04 changed font.
.release   init      "4.9"         JD  30JAN02 for 2004.
.release   init      "4.8"         JD  11JAN02 for 2002.
.release   init      "4.7"        ASH 19MAR01 NINORD MOVED TO FILE MANAGER
.release   init      "4.6"        ASH 02OCT2000 NEW SERVER ADDED
.release   init      "4.5"        JD 31Jan00 for 2000.
.release   init      "4.4"       ASH 29DEC98 NINORD Y2K, File expansion
.release   init      "4.31"       JD 29Jan99 skip pending orders.
.release   init      "4.3"       JD 25Jan99 for 99.
.release   init      "4.2"       JD 23Jan98 for 98.
.release  init      "4.11"      DLH 06jan97 for 97
.RELEASE  INIT      "4.1"       DLH 2FEB93     LANDSCAPE LASER.
.RELEASE  INIT      "4.0"      DLH 18MAR92       NORDXX, CONS, COMLOGIC.
.Release  init      "1.0"      DLH 02Nov87
.
. .FILES DECLARATIONS
OUTPUT   FILE      FIXED=984    PHYSICAL OUTPUT FILE 'SALESMSTDET'
.984 IS ROOM FOR 24 SALES PERSON'S.
.
. ....
RN       FORM      "013"      RECORD NUMBER
.
. .ORDER FILE LAYOUT
.
LR       DIM       6         POS 7-12   KEY
.Start patch #4.4 - increase var to reflect OQTY increase
.QUANT    FORM      7         POS 33-39  ORDER QUANTITY
QUANT    FORM      9         POS 33-39  ORDER QUANTITY
.End patch #4.4- increase var to reflect OQTY increase
.
. .ARCCLOCK LAYOUT
.
DATE     DIM       8
TIME     DIM       8
.START PATCH #4.4 - VAR NOW FOUND IN CONS.INC
.F1       DIM       1         POS 1-1    FILLER
.END PATCH #4.4 - VAR NOW FOUND IN CONS.INC
YR       FORM      2         POS 2-3    YEAR
. F1     DIM       1         POS 4-4    FILLER
MO       FORM      2         POS 5-6    MONTH
. F1     DIM       1         POS 7-7    FILLER
DA       FORM      2         POS 8-9    DAY
F4       DIM       4         POS 10-13  FILLER
HR       DIM       2         POS 14-15  HOUR
. F1     DIM       1         POS 16-16  FILLER
MIN      DIM       2         POS 17-18  MINUTE
. F1     DIM       1         POS 19-19  FILLER
SEC      DIM       2         POS 20-21  SECOND
MO1      DIM       2
DA1      DIM       2
YR1      DIM       2
.
. .WORK SPACE
.
MO3      FORM      3         TABLE VARIABLE MONTH + CALCULATED FIGURE FROM
.                            TABLE 
FINAL    DIM       1         CHECKS END OF PROGRAM
COUNT1   FORM      5         COUNTS ORDER RECORDS
COUNT2   FORM      5         OUTPUT MONTH COUNT
COUNT3   FORM      5         OUTPUT YEAR COUNT
ANS      DIM       1
WORK     FORM      9.4       WORK SPACE
TOTORD   FORM      5         TOTAL ORDERS
TOTQTY   FORM      9         TOTAL NAMES
TOTORD1  FORM      5         TOTAL ORDERS
TOTQTY1  FORM      9         TOTAL NAMES
TOTORD2  FORM      5         TOTAL ORDERS
TOTQTY2  FORM      9         TOTAL NAMES
TOTORD3  FORM      5         TOTAL ORDERS
TOTQTY3  FORM      9         TOTAL NAMES
TOTORD4  FORM      5         TOTAL ORDERS
TOTQTY4  FORM      9         TOTAL NAMES
TOTORD5  FORM      5         TOTAL ORDERS
TOTQTY5  FORM      9         TOTAL NAMES
TOTORD6  FORM      5         TOTAL ORDERS
TOTQTY6  FORM      9         TOTAL NAMES
TOTORD7  FORM      5         TOTAL ORDERS
TOTQTY7  FORM      9         TOTAL NAMES
TOTORD8  FORM      5         TOTAL ORDERS
TOTQTY8  FORM      9         TOTAL NAMES
TOTORD9  FORM      5         TOTAL ORDERS
TOTQTY9  FORM      9         TOTAL NAMES
TOTORD10 FORM       5         TOTAL ORDERS
TOTQTY10 FORM      9         TOTAL NAMES
TOTORD11 FORM      5         TOTAL ORDERS
TOTQTY11 FORM      9         TOTAL NAMES
TOTORD12 FORM      5         TOTAL ORDERS
TOTQTY12 FORM      9         TOTAL NAMES
TOTORD13 FORM      5         TOTAL ORDERS
TOTQTY13 FORM      9         TOTAL NAMES
TOTORD14 FORM      5         TOTAL ORDERS
TOTQTY14 FORM      9         TOTAL NAMES
TOTORD15 FORM      5         TOTAL ORDERS
TOTQTY15 FORM      9         TOTAL NAMES
TOTORD16 FORM      5         TOTAL ORDERS
TOTQTY16 FORM      9         TOTAL NAMES
TOTORD17 FORM      5         TOTAL ORDERS
TOTQTY17 FORM      9         TOTAL NAMES
TOTORD18 FORM      5         TOTAL ORDERS
TOTQTY18 FORM      9         TOTAL NAMES
TOTORD19 FORM      5         TOTAL ORDERS
TOTQTY19 FORM      9         TOTAL NAMES
TOTORD20 FORM      5         TOTAL ORDERS
TOTQTY20 FORM      9         TOTAL NAMES
TOTORD21 FORM      5         TOTAL ORDERS
TOTQTY21 FORM      9         TOTAL NAMES
TOTORD22 FORM      5         TOTAL ORDERS
TOTQTY22 FORM      9         TOTAL NAMES
TOTORD23 FORM      5         TOTAL ORDERS
TOTQTY23 FORM      9         TOTAL NAMES
TOTORD24 FORM      5         TOTAL ORDERS
TOTQTY24 FORM      9         TOTAL NAMES
TOTORD25 FORM      5         TOTAL ORDERS
TOTQTY25 FORM      9         TOTAL NAMES
FINORD   FORM      5         TOTAL ORDERS PLACED IN YEAR
EOYQTY   FORM      9
FINORD1  FORM      5         TOTAL ORDERS
EOYQTY1  FORM      9         TOTAL NAMES
FINORD2  FORM      5         TOTAL ORDERS
EOYQTY2  FORM      9         TOTAL NAMES
FINORD3  FORM      5         TOTAL ORDERS
EOYQTY3  FORM      9         TOTAL NAMES
FINORD4  FORM      5         TOTAL ORDERS
EOYQTY4  FORM      9         TOTAL NAMES
FINORD5  FORM      5         TOTAL ORDERS
EOYQTY5  FORM      9         TOTAL NAMES
FINORD6  FORM      5         TOTAL ORDERS
EOYQTY6  FORM      9         TOTAL NAMES
FINORD7  FORM      5         TOTAL ORDERS
EOYQTY7  FORM      9         TOTAL NAMES
FINORD8  FORM      5         TOTAL ORDERS
EOYQTY8  FORM      9         TOTAL NAMES
FINORD9  FORM      5         TOTAL ORDERS
EOYQTY9  FORM      9         TOTAL NAMES
FINORD10 FORM      5         TOTAL ORDERS
EOYQTY10 FORM      9         TOTAL NAMES
FINORD11 FORM      5         TOTAL ORDERS
EOYQTY11 FORM      9         TOTAL NAMES
FINORD12 FORM      5         TOTAL ORDERS
EOYQTY12 FORM      9         TOTAL NAMES
FINORD13 FORM      5         TOTAL ORDERS
EOYQTY13 FORM      9         TOTAL NAMES
FINORD14 FORM      5         TOTAL ORDERS
EOYQTY14 FORM      9         TOTAL NAMES
FINORD15 FORM      5         TOTAL ORDERS
EOYQTY15 FORM      9         TOTAL NAMES
FINORD16 FORM      5         TOTAL ORDERS
EOYQTY16 FORM      9         TOTAL NAMES
FINORD17 FORM      5         TOTAL ORDERS
EOYQTY17 FORM      9         TOTAL NAMES
FINORD18 FORM      5         TOTAL ORDERS
EOYQTY18 FORM      9         TOTAL NAMES
FINORD19 FORM      5         TOTAL ORDERS
EOYQTY19 FORM      9         TOTAL NAMES
FINORD20 FORM      5         TOTAL ORDERS
EOYQTY20 FORM      9         TOTAL NAMES
FINORD21 FORM      5         TOTAL ORDERS
EOYQTY21 FORM      9         TOTAL NAMES
FINORD22 FORM      5         TOTAL ORDERS
EOYQTY22 FORM      9         TOTAL NAMES
FINORD23 FORM      5         TOTAL ORDERS
EOYQTY23 FORM      9         TOTAL NAMES
FINORD24 FORM      5         TOTAL ORDERS
EOYQTY24 FORM      9         TOTAL NAMES
FINORD25 FORM      5         TOTAL ORDERS
EOYQTY25 FORM      9         TOTAL NAMES
.
. *******************CHANGE THE YEARS IN THE FOLLOWING LINES***********
.
FINORDA  DIM       5         TOTAL ORDERS PLACED DURING 1987
FINORDB  DIM       5         TOTAL ORDERS PLACED DURING 1986
ORDS     FORM      5         WORK FIELD FOR TOTAL LOAD
FINORDC  DIM       5         TOTAL ORDERS PLACED DURING 1988
FINQTY   FORM      9.2       TOTAL NAMES ORDERED DURING YEAR
ENDQTY   FORM      9.2       PERCENTAGE WORK SPACE
. *******************CHANGE THE YEARS IN THE FOLLOWING LINES***********
FINQTYA  DIM       9         TOTAL NAMES ORDERED DURING 1986
FINQTYB  DIM       9         TOTAL NAMES ORDERED DURING 1987
FINQTYC  DIM       9         TOTAL NAMES ORDERED DURING 1988
FINQTYB1 DIM       9         USED FOR PARTIAL MONTH % CHANGE CALC.
PCTB     DIM       4         % CHANGE IN 1987
PCT      DIM       3         PRINTS PERCENTAGE
NUMPCT   FORM      3         PERCENTAGE WORK SPACE
PCTC     DIM       4         % CHANGE IN 1988
PCTMB    DIM       4         % CHANGE FROM PREVIOUS MONTH (COLUMN 2)
PCTMC    DIM       4         % CHANGE FROM PREVIOUS MONTH (COLUMN 3)
AVERORDA FORM      9         AVERAGE NAMES PER ORDER (COLUMN 1)
AVERORDB FORM      9         AVERAGE NAMES PER ORDER (COLUMN 2)
AVERORDC FORM      9         AVERAGE NAMES PER ORDER (COLUMN 3)
PREQTYB  DIM       9         TOTAL NAMES ORDERED DURING PREV MONTH
PREQTYC  DIM       9         TOTAL NAMES ORDERED DURING PREV MONTH
TITLE    INIT      "COMPUNAME        "        HEADING (NIN OR CMP)
TITLE2   INIT      "NO SALESPERSON   "
NUMHR    FORM      2         DATE WORK SPACE
INDEX    FORM      "001"
ZERO     FORM      "0"       ZERO FILLS MONTH FIELDS
MONTH    DIM       9         MONTH DESCRIPTION ON REPORT
HOLDYR   FORM      2         HOLDS YEAR
NUMMO    FORM      2         INDEX FOR BRANCH
KEY      DIM       6
TAB      FORM      "001"      TAB POSITIONS
F2       DIM       2         FILLER
.V        FORM      "01"      SPLIT SCREEN POSITION
V1       FORM      "15"      SPLIT SCREEN POSITION
V2       FORM      "26"      SPLIT SCREEN POSITION
PASS     FORM      2
ONE      FORM      "1"
. ***************CHANGE FIRST' ' NUMBERS IN JAN EVERY YEAR********************
**********.045801 FIRST LR OCT 1987.**********
**********.051937 FIRST LR 1988    ***********
**********.078953 FIRST LR 1989 **************
**********.106639 FIRST LR 1990 **************
**********.135371 FIRST LR 1991 **************
**********.158020 FIRST LR 1992 **************
**********.185570 FIRST LR 1993 **************
**********.211248 FIRST LR 1994 **************
**********.234738 FIRST LR 1995. *************
**********.257868 FIRST LR 1996. *************
**********.280833 FIRST LR 1997. *************
**********.305233 FIRST LR 1998. *************
**********.330753 FIRST NIN LR OF 1999
**********.362964 FIRST NIN LR OF 2000
.**********.404000 FIRST NIN LR OF 2001
.**********.430000 FIRST NIN LR OF 2002
* *****************************************************************************
FIRSTN   INIT      "500000"  FIRST NIN LR OF 2003 may
FIRSTC   INIT      "020693"  FIRST CMP LR OF 1982
PLUS     FORM      3
TWELVE   FORM      "12"
TWENTY4  FORM      "24"
THIRTY6  FORM      "36"
FORTY8   FORM      "48"
SIXTY    FORM      "60"
SEVENTY2 FORM      "72"
NUM84    FORM      "84"
NUM96    FORM      "96"
NUM108   FORM      "108"
NUM120   FORM      "120"
NUM132   FORM      "132"
NUM144   FORM      "144"
NUM156   FORM      "156"
NUM168   FORM      "168" 
NUM180   FORM      "180"
NUM192   FORM      "192"
NUM204   FORM      "204"
NUM216   FORM      "216"
NUM228   FORM      "228"
NUM240   FORM      "240"
NUM252   FORM      "252"
NUM264   FORM      "264"
NUM276   FORM      "276"
NUM288   FORM      "288"
NUM300   FORM      "300"
NUM312   FORM      "312"
YR2      form       2         HOLDS YEAR
yrchk    form      2
DA2      DIM       2         HOLDS DAY
MO2      DIM       2         HOLDS MONTH
HRSTRT   DIM       2         HOLDS STARTING HOUR
MINSTRT  DIM       2         HOLDS STARTING MIN
SECSTRT  DIM       2         HOLDS STARTING SEC
COUNT    FORM      2
MNTH    FORM      "01"
JANX     FORM      5
CHECK    DIM       1
FEBX     FORM      5
MARX     FORM      5
APRX     FORM      5
MAYX     FORM      5
JUNX     FORM      5
JULX     FORM      5
AUGX     FORM      5
SEPX     FORM      5
OCTX     FORM      5
NOVX     FORM      5
DECX     FORM      5
JAN      FORM      9
FEB      FORM      9
MAR      FORM      9
APR      FORM      9
MAY      FORM      9
JUN      FORM      9
JUL      FORM      9
AUG      FORM      9
SEP      FORM      9
OCT      FORM      9
NOV      FORM      9
DEC      FORM      9
JANX1    FORM      9
FEBX1    FORM      9
MARX1    FORM      9
APRX1    FORM      9
MAYX1    FORM      9
JUNX1    FORM      9
JULX1    FORM      9
AUGX1    FORM      9
SEPX1    FORM      9
OCTX1    FORM      9
NOVX1    FORM      9
DECX1    FORM      9
JANX2    FORM      9
FEBX2    FORM      9
MARX2    FORM      9
APRX2    FORM      9
MAYX2    FORM      9
JUNX2    FORM      9
JULX2    FORM      9
AUGX2    FORM      9
SEPX2    FORM      9
OCTX2    FORM      9
NOVX2    FORM      9
DECX2    FORM      9
JANX3    FORM      9
FEBX3    FORM      9
MARX3    FORM      9
APRX3    FORM      9
MAYX3    FORM      9
JUNX3    FORM      9
JULX3    FORM      9
AUGX3    FORM      9
SEPX3    FORM      9
OCTX3    FORM      9
NOVX3    fORM      9
DECX3    FORM      9
JANX4    FORM      9
FEBX4    FORM      9
MARX4    FORM      9
APRX4    FORM      9
MAYX4    FORM      9
JUNX4    FORM      9
JULX4    FORM      9
AUGX4    FORM      9
SEPX4    FORM      9
OCTX4    FORM      9
NOVX4    FORM      9
DECX4    FORM      9
JANX5    FORM      9
FEBX5    FORM      9
MARX5    FORM      9
APRX5    FORM      9
MAYX5    FORM      9
JUNX5    FORM      9
JULX5    FORM      9
AUGX5    FORM      9
SEPX5    FORM      9
OCTX5    FORM      9
NOVX5    FORM      9
DECX5    FORM      9
JANX6    FORM      9
FEBX6    FORM      9
MARX6    FORM      9
APRX6    FORM      9
MAYX6    FORM      9
JUNX6    FORM      9
JULX6    FORM      9
AUGX6    FORM      9
SEPX6    FORM      9
OCTX6    FORM      9
NOVX6    FORM      9
DECX6    FORM      9
JANX7    FORM      9
FEBX7    FORM      9
MARX7    FORM      9
APRX7    FORM      9
MAYX7    FORM      9
JUNX7    FORM      9
JULX7    FORM      9
AUGX7    FORM      9
SEPX7    FORM      9
OCTX7    FORM      9
NOVX7    FORM      9
DECX7    FORM      9
JANX8    FORM      9
FEBX8    FORM      9
MARX8    FORM      9
APRX8    FORM      9
MAYX8    FORM      9
JUNX8    FORM      9
JULX8    FORM      9
AUGX8    FORM      9
SEPX8    FORM      9
OCTX8    FORM      9
NOVX8    FORM      9
DECX8    FORM      9
JANX9    FORM      9
FEBX9    FORM      9
MARX9    FORM      9
APRX9    FORM      9
MAYX9    FORM      9
JUNX9    FORM      9
JULX9    FORM      9
AUGX9    FORM      9
SEPX9    FORM      9
OCTX9    FORM      9
NOVX9    FORM      9
DECX9    FORM      9
JANX10   FORM      9
FEBX10   FORM      9
MARX10   FORM      9
APRX10   FORM      9
MAYX10   FORM      9
JUNX10   FORM      9
JULX10   FORM      9
AUGX10   FORM      9
SEPX10   FORM      9
OCTX10   FORM      9
NOVX10   FORM      9
DECX10   FORM      9
JANX11   FORM      9
FEBX11   FORM      9
MARX11   FORM      9
APRX11   FORM      9
MAYX11   FORM      9
JUNX11   FORM      9
JULX11   FORM      9
AUGX11   FORM      9
SEPX11   FORM      9
OCTX11   FORM      9
NOVX11   FORM      9
DECX11   FORM      9
JANX12   FORM      9
FEBX12   FORM      9
MARX12   FORM      9
APRX12   FORM      9
MAYX12   FORM      9
JUNX12   FORM      9
JULX12   FORM      9
AUGX12   FORM      9
SEPX12   FORM      9
OCTX12   FORM      9
NOVX12   FORM      9
DECX12   FORM      9
JANX13   FORM      9
FEBX13   FORM      9
MARX13   FORM      9
APRX13   FORM      9
MAYX13   FORM      9
JUNX13   FORM      9
JULX13   FORM      9
AUGX13   FORM      9
SEPX13   FORM      9
OCTX13   FORM      9
NOVX13   FORM      9
DECX13   FORM      9
JANX14   FORM      9
FEBX14   FORM      9
MARX14   FORM      9
APRX14   FORM      9
MAYX14   FORM      9
JUNX14   FORM      9
JULX14   FORM      9
AUGX14   FORM      9
SEPX14   FORM      9
OCTX14   FORM      9
NOVX14   FORM      9
DECX14   FORM      9
JANX15   FORM      9
FEBX15   FORM      9
MARX15   FORM      9
APRX15   FORM      9
MAYX15   FORM      9
JUNX15   FORM      9
JULX15   FORM      9
AUGX15   FORM      9
SEPX15   FORM      9
OCTX15   FORM      9
NOVX15   FORM      9
DECX15   FORM      9
JANX16   FORM      9
FEBX16   FORM      9
MARX16   FORM      9
APRX16   FORM      9
MAYX16   FORM      9
JUNX16   FORM      9
JULX16   FORM      9
AUGX16   FORM      9
SEPX16   FORM      9
OCTX16   FORM      9
NOVX16   FORM      9
DECX16   FORM      9
JANX17   FORM      9
FEBX17   FORM      9
MARX17   FORM      9
APRX17   FORM      9
MAYX17   FORM      9
JUNX17   FORM      9
JULX17   FORM      9
AUGX17   FORM      9
SEPX17   FORM      9
OCTX17   FORM      9
NOVX17   FORM      9
DECX17   FORM      9
JANX18   FORM      9
FEBX18   FORM      9
MARX18   FORM      9
APRX18   FORM      9
MAYX18   FORM      9
JUNX18   FORM      9
JULX18   FORM      9
AUGX18   FORM      9
SEPX18   FORM      9
OCTX18   FORM      9
NOVX18   FORM      9
DECX18   FORM      9
JANX19   FORM      9
FEBX19   FORM      9
MARX19   FORM      9
APRX19   FORM      9
MAYX19   FORM      9
JUNX19   FORM      9
JULX19   FORM      9
AUGX19   FORM      9
SEPX19   FORM      9
OCTX19   FORM      9
NOVX19   FORM      9
DECX19   FORM      9
JANX20   FORM      9
FEBX20   FORM      9
MARX20   FORM      9
APRX20   FORM      9
MAYX20   FORM      9
JUNX20   FORM      9
JULX20   FORM      9
AUGX20   FORM      9
SEPX20   FORM      9
OCTX20   FORM      9
NOVX20   FORM      9
DECX20   FORM      9
JANX21   FORM      9
FEBX21   FORM      9
MARX21   FORM      9
APRX21   FORM      9
MAYX21   FORM      9
JUNX21   FORM      9
JULX21   FORM      9
AUGX21   FORM      9
SEPX21   FORM      9
OCTX21   FORM      9
NOVX21   FORM      9
DECX21   FORM      9
JANX22   FORM      9
FEBX22   FORM      9
MARX22   FORM      9
APRX22   FORM      9
MAYX22   FORM      9
JUNX22   FORM      9
JULX22   FORM      9
AUGX22   FORM      9
SEPX22   FORM      9
OCTX22   FORM      9
NOVX22   FORM      9
DECX22   FORM      9
JANX23   FORM      9
FEBX23   FORM      9
MARX23   FORM      9
APRX23   FORM      9
MAYX23   FORM      9
JUNX23   FORM      9
JULX23   FORM      9
AUGX23   FORM      9
SEPX23   FORM      9
OCTX23   FORM      9
NOVX23   FORM      9
DECX23   FORM      9
JANX24   FORM      9
FEBX24   FORM      9
MARX24   FORM      9
APRX24   FORM      9
MAYX24   FORM      9
JUNX24   FORM      9
JULX24   FORM      9
AUGX24   FORM      9
SEPX24   FORM      9
OCTX24   FORM      9
NOVX24   FORM      9
DECX24   FORM      9
JANX25   FORM      9
FEBX25   FORM      9
MARX25   FORM      9
APRX25   FORM      9
MAYX25   FORM      9
JUNX25   FORM      9
JULX25   FORM      9
AUGX25   FORM      9
SEPX25   FORM      9
OCTX25   FORM      9
NOVX25   FORM      9
DECX25   FORM      9
JAN1     FORM      9
FEB1     FORM      9
MAR1     FORM      9
APR1     FORM      9
MAY1     FORM      9
JUN1     FORM      9
JUL1     FORM      9
AUG1     FORM      9
SEP1     FORM      9
OCT1     FORM      9
NOV1     FORM      9
DEC1     FORM      9
JAN2     FORM      9
FEB2     FORM      9
MAR2     FORM      9
APR2     FORM      9
MAY2     FORM      9
JUN2     FORM      9
JUL2     FORM      9
AUG2     FORM      9
SEP2     FORM      9
OCT2     FORM      9
NOV2     FORM      9
DEC2     FORM      9
JAN3     FORM      9
FEB3     FORM      9
MAR3     FORM      9
APR3     FORM      9
MAY3     FORM      9
JUN3     FORM      9
JUL3     FORM      9
AUG3     FORM      9
SEP3     FORM      9
OCT3     FORM      9
NOV3     FORM      9
DEC3     FORM      9
JAN4     FORM      9
FEB4     FORM      9
MAR4     FORM      9
APR4     FORM      9
MAY4     FORM      9
JUN4     FORM      9
JUL4     FORM      9
AUG4     FORM      9
SEP4     FORM      9
OCT4     FORM      9
NOV4     FORM      9
DEC4     FORM      9
JAN5     FORM      9
FEB5     FORM      9
MAR5     FORM      9
APR5     FORM      9
MAY5     FORM      9
JUN5     FORM      9
JUL5     FORM      9
AUG5     FORM      9
SEP5     FORM      9
OCT5     FORM      9
NOV5     FORM      9
DEC5     FORM      9
JAN6     FORM      9
FEB6     FORM      9
MAR6     FORM      9
APR6     FORM      9
MAY6     FORM      9
JUN6     FORM      9
JUL6     FORM      9
AUG6     FORM      9
SEP6     FORM      9
OCT6     FORM      9
NOV6     FORM      9
DEC6     FORM      9
JAN7     FORM      9
FEB7     FORM      9
MAR7     FORM      9
APR7     FORM      9
MAY7     FORM      9
JUN7     FORM      9
JUL7     FORM      9
AUG7     FORM      9
SEP7     FORM      9
OCT7     FORM      9
NOV7     FORM      9
DEC7     FORM      9
JAN8     FORM      9
FEB8     FORM      9
MAR8     FORM      9
APR8     FORM      9
MAY8     FORM      9
JUN8     FORM      9
JUL8     FORM      9
AUG8     FORM      9
SEP8     FORM      9
OCT8     FORM      9
NOV8     FORM      9
DEC8     FORM      9
JAN9     FORM      9
FEB9     FORM      9
MAR9     FORM      9
APR9     FORM      9
MAY9     FORM      9
JUN9     FORM      9
JUL9     FORM      9
AUG9     FORM      9
SEP9     FORM      9
OCT9     FORM      9
NOV9     FORM      9
DEC9     FORM      9
JAN10    FORM      9
FEB10    FORM      9
MAR10    FORM      9
APR10    FORM      9
MAY10    FORM      9
JUN10    FORM      9
JUL10    FORM      9
AUG10    FORM      9
SEP10    FORM      9
OCT10    FORM      9
NOV10    FORM      9
DEC10    FORM      9
JAN11    FORM      9
FEB11    FORM      9
MAR11    FORM      9
APR11    FORM      9
MAY11    FORM      9
JUN11    FORM      9
JUL11    FORM      9
AUG11    FORM      9
SEP11    FORM      9
OCT11    FORM      9
NOV11    FORM      9
DEC11    FORM      9
JAN12    FORM      9
FEB12    FORM      9
MAR12    FORM      9
APR12    FORM      9
MAY12    FORM      9
JUN12    FORM      9
JUL12    FORM      9
AUG12    FORM      9
SEP12    FORM      9
OCT12    FORM      9
NOV12    FORM      9
DEC12    FORM      9
JAN13    FORM      9
FEB13    FORM      9
MAR13    FORM      9
APR13    FORM      9
MAY13    FORM      9
JUN13    FORM      9
JUL13    FORM      9
AUG13    FORM      9
SEP13    FORM      9
OCT13    FORM      9
NOV13    FORM      9
DEC13    FORM      9
JAN14    FORM      9
FEB14    FORM      9
MAR14    FORM      9
APR14    FORM      9
MAY14    FORM      9
JUN14    FORM      9
JUL14    FORM      9
AUG14    FORM      9
SEP14    FORM      9
OCT14    FORM      9
NOV14    FORM      9
DEC14    FORM      9
JAN15    FORM      9
FEB15    FORM      9
MAR15    FORM      9
APR15    FORM      9
MAY15    FORM      9
JUN15    FORM      9
JUL15    FORM      9
AUG15    FORM      9
SEP15    FORM      9
OCT15    FORM      9
NOV15    FORM      9
DEC15    FORM      9
JAN16    FORM      9
FEB16    FORM      9
MAR16    FORM      9
APR16    FORM      9
MAY16    FORM      9
JUN16    FORM      9
JUL16    FORM      9
AUG16    FORM      9
SEP16    FORM      9
OCT16    FORM      9
NOV16    FORM      9
DEC16    FORM      9
JAN17    FORM      9
FEB17    FORM      9
MAR17    FORM      9
APR17    FORM      9
MAY17    FORM      9
JUN17    FORM      9
JUL17    FORM      9
AUG17    FORM      9
SEP17    FORM      9
OCT17    FORM      9
NOV17    FORM      9
DEC17    FORM      9
JAN18    FORM      9
FEB18    FORM      9
MAR18    FORM      9
APR18    FORM      9
MAY18    FORM      9
JUN18    FORM      9
JUL18    FORM      9
AUG18    FORM      9
SEP18    FORM      9
OCT18    FORM      9
NOV18    FORM      9
DEC18    FORM      9
JAN19    FORM      9
FEB19    FORM      9
MAR19    FORM      9
APR19    FORM      9
MAY19    FORM      9
JUN19    FORM      9
JUL19    FORM      9
AUG19    FORM      9
SEP19    FORM      9
OCT19    FORM      9
NOV19    FORM      9
DEC19    FORM      9
JAN20    FORM      9
FEB20    FORM      9
MAR20    FORM      9
APR20    FORM      9
MAY20    FORM      9
JUN20    FORM      9
JUL20    FORM      9
AUG20    FORM      9
SEP20    FORM      9
OCT20    FORM      9
NOV20    FORM      9
DEC20    FORM      9
JAN21    FORM      9
FEB21    FORM      9
MAR21    FORM      9
APR21    FORM      9
MAY21    FORM      9
JUN21    FORM      9
JUL21    FORM      9
AUG21    FORM      9
SEP21    FORM      9
OCT21    FORM      9
NOV21    FORM      9
DEC21    FORM      9
JAN22    FORM      9
FEB22    FORM      9
MAR22    FORM      9
APR22    FORM      9
MAY22    FORM      9
JUN22    FORM      9
JUL22    FORM      9
AUG22    FORM      9
SEP22    FORM      9
OCT22    FORM      9
NOV22    FORM      9
DEC22    FORM      9
JAN23    FORM      9
FEB23    FORM      9
MAR23    FORM      9
APR23    FORM      9
MAY23    FORM      9
JUN23    FORM      9
JUL23    FORM      9
AUG23    FORM      9
SEP23    FORM      9
OCT23    FORM      9
NOV23    FORM      9
DEC23    FORM      9
JAN24    FORM      9
FEB24    FORM      9
MAR24    FORM      9
APR24    FORM      9
MAY24    FORM      9
JUN24    FORM      9
JUL24    FORM      9
AUG24    FORM      9
SEP24    FORM      9
OCT24    FORM      9
NOV24    FORM      9
DEC24    FORM      9
JAN25    FORM      9
FEB25    FORM      9
MAR25    FORM      9
APR25    FORM      9
MAY25    FORM      9
JUN25    FORM      9
JUL25    FORM      9
AUG25    FORM      9
SEP25    FORM      9
OCT25    FORM      9
NOV25    FORM      9
DEC25    FORM      9
QTY      FORM      10
OK       DIM       1
FINALSAV FORM      9
SAVE     FORM      9
NUMDA    FORM      2
SPOOL    DIM       1
LASTMOSW DIM       1            HOLDS 'Y' IF LAST MONTH OF CURRENT YEAR.
SALESBR  FORM      2            HOLDS SALESMAN NUMBER FOR BRANCH
.
.
         move       c1 to v
         KEYIN     *P1:1,*ES,*P15:10,"DO YOU WANT THE OUTPUT SPOOLED ? ":
                   *T20,SPOOL;
         CMATCH    "N",SPOOL
         GOTO      BEGIN IF EQUAL
         IFNZ      PC
         SPLOPEN   "SALESDETPRT/PRT:PRINT"
         XIF
         IFZ      PC
.START PATCH 4.6 REPLACED LOGIC
.         SPLOPEN   "g:\DATA\SLSPRTD.LST"
         PACK      STR35,NTWKPATH1,"SLSPRTD.LST"
         SPLOPEN   STR35
.END PATCH 4.6 REPLACED LOGIC
         XIF
.....INITIAL TIME ANS SCREEN DISPLAY;
.
BEGIN
         MOVE      C1 TO NORDPATH        .SET ACCESS TO ISAM.
         CALL      DISPLAY
         CALL      TIME
         MOVE      HR,HRSTRT
         REP       " 0",HRSTRT
         MOVE      MIN,MINSTRT
         REP       " 0",MINSTRT
         MOVE      SEC,SECSTRT
         MOVE      DA,DA2
         REP       " 0",DA2
         MOVE      YR,YR2
         MOVE      MO,MO2
         REP       " 0",MO2
         TRAP      START giving error IF IO
.
         KEYIN     *P1:24,*EF,*P15:24,"PRINT ONLY ?",*T3,ANS;
         CMATCH    "Y" TO ANS
         GOTO      PRINTOLD IF EQUAL
.
. .....CHECKS TO SEE IF 'SALESMSTDET' EXISTS
.
         IFNZ      PC
         OPEN      OUTPUT,"SALESMSTDET/TEXT",EXCLUSIVE
         XIF
         IFZ      PC
         OPEN      OUTPUT,"SALESDET",EXCLUSIVE
         XIF
         TRAPCLR   IO
         TRAP      FORMAT GIVING ERROR IF FORMAT
         MOVE      "Q",CHECK
         MOVE      FIRSTN TO NORDFLD
         CALL      NORDKEY
         MOVE      "NAMES IN THE NEWS",TITLE
         MOVE      OQTY TO QUANT
. /////////////////////////////////////////////////////////////////////////////
.         MOVE      "169",RN     <---ADD '13' IN JAN.
.         MOVE      "221",RN     <---ADD '13' IN JAN.
.         MOVE      "234",RN     <---ADD '13' IN JAN.
         MOVE      "247",RN     <---ADD '13' IN JAN.
. /////////////////////////////////////////////////////////////////////////////
         MOVE      OODTEY,YR
         MOVE      YR,HOLDYR
         MOVE      OODTEM TO MO
         GOTO      READ2
CHECK    CLOSE     NORDFILE
         MOVE      C0 TO NORDFLAG
         CMATCH    "Q",CHECK
         GOTO      CMP IF NOT EQUAL
         MOVE      "CMPORD" TO NORDNAME
         MOVE      FIRSTC TO NORDFLD
         CALL      NORDTST
.         OPEN      NINORD,"CMPORD",SHARE
.         READ      NINORD,FIRSTC;ORCODE,OSTAT
         MOVE      "COMPUNAME",TITLE
. /////////////////////////////////////////////////////////////////////////////
.         MOVE      "169",RN     <---ADD '13' IN JAN.
         MOVE      "247",RN     <---ADD '13' IN JAN.
. /////////////////////////////////////////////////////////////////////////////
         MOVE      "X",FINAL
         ADD       "42",V
         ADD       "42",V1
         ADD       "42",V2
         MOVE      "01",TAB
         CALL      ZERO
         IFNZ      PC
         OPEN      OUTPUT,"SALESMSTDET/TEXT",EXCLUSIVE
         XIF
         IFZ      PC
         OPEN      OUTPUT,"SALESDET/TEXT",EXCLUSIVE
         XIF
         MOVE      YR,HOLDYR
         GOTO      READ2
.
......OPENS FILES
.
START    TRAPCLR   IO
         NORETURN
         move       c1 to v
.         OPEN      NINORD,"NINORD",SHARE
.START PATCH 4.7 REPLACED LOGIC
.         MOVE      "NINORD" TO NORDNAME
.>Patch 4.93 Begin
.         MOVE      "NINORD.ISI|20.20.30.103:502" TO NORDNAME
         MOVE      "NINORD.ISI|10.10.30.103:502" TO NORDNAME
.>Patch 4.93 End         
.END PATCH 4.7 REPLACED LOGIC
         MOVE      "NAMES IN THE NEWS",TITLE
         IFNZ      PC
         PREPARE   OUTPUT,"SALESMSTDET/TEXT:TEXT"
         XIF
         IFZ      PC
.START PATCH 4.6 REPLACED LOGIC
.         PREPARE   OUTPUT,"g:\DATA\TEXT\SALESDET"
         PACK      STR35,NTWKPATH1,"TEXT,SALESDET"
         PREPARE   OUTPUT,STR35
.END PATCH 4.6 REPLACED LOGIC
         XIF
         GOTO      SCRN1
.
.CMP      OPEN      NINORD,"CMPORD",SHARE
CMP      MOVE      "CMPORD" TO NORDNAME
         MOVE      "COMPUNAME",TITLE
         MOVE      "X",FINAL
         IFNZ      PC
         OPEN      OUTPUT,"SALESMSTDET/TEXT",EXCLUSIVE
         XIF
         IFZ      PC
         OPEN      OUTPUT,"SALESDET",EXCLUSIVE
         XIF
.         MOVE      "19",TAB
         MOVE      "01",TAB
         CALL      ZERO
SCRN1    CMATCH    "X",FINAL
         GOTO      READ1 IF NOT EQUAL
         ADD       "42",V
         ADD       "42",V1
         ADD       "42",V2
.
......READS ORDER FILE AND CREATES 'SALESMSTDET'
.
READ1    CALL      NORDKS
.
         GOTO      NEWYEAR IF OVER
         MOVE      OODTEM TO MO
         MOVE      C0 TO QUANT
         MOVE      OQTY TO QUANT
         MOVE      OODTEY TO YR
         MOVE      OODTEY,HOLDYR
         ADD       "1",COUNT1
.Start patch #4.4- ADDED var
.         DISPLAY   *PV1:6,COUNT1,*P24:9,OODTEM,"/",OODTED,"/",OODTEY:
.                   " ",OLRN
         DISPLAY   *PV1:6,COUNT1,*P24:9,OODTEM,"/",OODTED,"/",OODTEC,OODTEY:
                   " ",OLRN
.End patch #4.4- ADDED var
         COMPARE   "1",COUNT1
         GOTO      READ2 IF NOT EQUAL
         MOVE      OODTEY,HOLDYR
READ2    RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      READ1 IF EQUAL
.begin patch 4.31
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      read1 IF EQUAL     YES, skip.
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      read1 IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       LCR order ?
         GOTO      read1 IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       Cancelled LCR order ?
         GOTO      read1 IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
.end patch 4.31
         COMPARE   ZERO TO QUANT
         GOTO      READ1 IF EQUAL    ELIMINATE ORDERS WITH 0 QTY.
         COMPARE   YRchk TO HOLDYR
.         COMPARE   YR TO HOLDYR
.         GOTO      NEWYEAR IF NOT EQUAL
         GOTO      READ3 IF EQUAL
.         GOTO      NEWYEAR IF LESS
         GOTO      READ1
READ3    MOVE      ZERO TO SALESBR
         PACK      B2 FROM OSALES10,OSALES
.         MOVE      OSALES TO SALESBR
         MOVE      B2 TO SALESBR
...........SALESPERSON EXCEPTIONS HERE.... IE OLD PERSONS DATA VALID FOR
...........TEAM BUT NOT FOR INDIVIDUAL "SALES NUMBER REUSED WITHIN CURRENT
...........YEAR".
         COMPARE   "5" TO SALESBR        *NANCY P.
         GOTO      READ3A IF NOT EQUAL
         COMPARE   "90" TO HOLDYR     *1990?
         GOTO      READ3A IF NOT EQUAL       NO.
         COMPARE   "4" TO MO     JAN,FEB,MAR???
         GOTO      READ1 IF LESS       YES
READ3A   MOVE      "  " TO B2
         MOVE      "0" TO PLUS
         LOAD      PLUS FROM SALESBR OF TWELVE,TWENTY4,THIRTY6,FORTY8,SIXTY:
                   SEVENTY2,NUM84,NUM96,NUM108,NUM120,NUM132,NUM144,NUM156:
                   NUM168,NUM180,NUM192,NUM204,NUM216,NUM228,NUM240,NUM252:
                   NUM264,NUM276,NUM288,NUM300,NUM312
         MOVE      MO TO MO3
         ADD       PLUS TO MO3
         LOAD      ORDS FROM MO3 OF JANX,FEBX,MARX,APRX,MAYX,JUNX,JULX,AUGX:
                   SEPX,OCTX,NOVX,DECX:
                   JANX1,FEBX1,MARX1,APRX1,MAYX1,JUNX1,JULX1,AUGX1:
                   SEPX1,OCTX1,NOVX1,DECX1:
                   JANX2,FEBX2,MARX2,APRX2,MAYX2,JUNX2,JULX2,AUGX2:
                   SEPX2,OCTX2,NOVX2,DECX2:
                   JANX3,FEBX3,MARX3,APRX3,MAYX3,JUNX3,JULX3,AUGX3:
                   SEPX3,OCTX3,NOVX3,DECX3:
                   JANX4,FEBX4,MARX4,APRX4,MAYX4,JUNX4,JULX4,AUGX4:
                   SEPX4,OCTX4,NOVX4,DECX4:
                   JANX5,FEBX5,MARX5,APRX5,MAYX5,JUNX5,JULX5,AUGX5:
                   SEPX5,OCTX5,NOVX5,DECX5:
                   JANX6,FEBX6,MARX6,APRX6,MAYX6,JUNX6,JULX6,AUGX6:
                   SEPX6,OCTX6,NOVX6,DECX6:
                   JANX7,FEBX7,MARX7,APRX7,MAYX7,JUNX7,JULX7,AUGX7:
                   SEPX7,OCTX7,NOVX7,DECX7:
                   JANX8,FEBX8,MARX8,APRX8,MAYX8,JUNX8,JULX8,AUGX8:
                   SEPX8,OCTX8,NOVX8,DECX8:
                   JANX9,FEBX9,MARX9,APRX9,MAYX9,JUNX9,JULX9,AUGX9:
                   SEPX9,OCTX9,NOVX9,DECX9:
                   JANX10,FEBX10,MARX10,APRX10,MAYX10,JUNX10,JULX10,AUGX10:
                   SEPX10,OCTX10,NOVX10,DECX10:
                   JANX11,FEBX11,MARX11,APRX11,MAYX11,JUNX11,JULX11,AUGX11:
                   SEPX11,OCTX11,NOVX11,DECX11:
                   JANX12,FEBX12,MARX12,APRX12,MAYX12,JUNX12,JULX12,AUGX12:
                   SEPX12,OCTX12,NOVX12,DECX12:
                   JANX13,FEBX13,MARX13,APRX13,MAYX13,JUNX13,JULX13,AUGX13:
                   SEPX13,OCTX13,NOVX13,DECX13:
                   JANX14,FEBX14,MARX14,APRX14,MAYX14,JUNX14,JULX14,AUGX14:
                   SEPX14,OCTX14,NOVX14,DECX14:
                   JANX15,FEBX15,MARX15,APRX15,MAYX15,JUNX15,JULX15,AUGX15:
                   SEPX15,OCTX15,NOVX15,DECX15:
                   JANX16,FEBX16,MARX16,APRX16,MAYX16,JUNX16,JULX16,AUGX16:
                   SEPX16,OCTX16,NOVX16,DECX16:
                   JANX17,FEBX17,MARX17,APRX17,MAYX17,JUNX17,JULX17,AUGX17:
                   SEPX17,OCTX17,NOVX17,DECX17:
                   JANX18,FEBX18,MARX18,APRX18,MAYX18,JUNX18,JULX18,AUGX18:
                   SEPX18,OCTX18,NOVX18,DECX18:
                   JANX19,FEBX19,MARX19,APRX19,MAYX19,JUNX19,JULX19,AUGX19:
                   SEPX19,OCTX19,NOVX19,DECX19:
                   JANX20,FEBX20,MARX20,APRX20,MAYX20,JUNX20,JULX20,AUGX20:
                   SEPX20,OCTX20,NOVX20,DECX20:
                   JANX21,FEBX21,MARX21,APRX21,MAYX21,JUNX21,JULX21,AUGX21:
                   SEPX21,OCTX21,NOVX21,DECX21:
                   JANX22,FEBX22,MARX22,APRX22,MAYX22,JUNX22,JULX22,AUGX22:
                   SEPX22,OCTX22,NOVX22,DECX22:
                   JANX23,FEBX23,MARX23,APRX23,MAYX23,JUNX23,JULX23,AUGX23:
                   SEPX23,OCTX23,NOVX23,DECX23:
                   JANX24,FEBX24,MARX24,APRX24,MAYX24,JUNX24,JULX24,AUGX24:
                   SEPX24,OCTX24,NOVX24,DECX24:
                   JANX25,FEBX25,MARX25,APRX25,MAYX25,JUNX25,JULX25,AUGX25:
                   SEPX25,OCTX25,NOVX25,DECX25
         ADD       "1",ORDS
         STORE     ORDS INTO MO3 OF JANX,FEBX,MARX,APRX,MAYX,JUNX,JULX,AUGX:
                   SEPX,OCTX,NOVX,DECX:
                   JANX1,FEBX1,MARX1,APRX1,MAYX1,JUNX1,JULX1,AUGX1:
                   SEPX1,OCTX1,NOVX1,DECX1:
                   JANX2,FEBX2,MARX2,APRX2,MAYX2,JUNX2,JULX2,AUGX2:
                   SEPX2,OCTX2,NOVX2,DECX2:
                   JANX3,FEBX3,MARX3,APRX3,MAYX3,JUNX3,JULX3,AUGX3:
                   SEPX3,OCTX3,NOVX3,DECX3:
                   JANX4,FEBX4,MARX4,APRX4,MAYX4,JUNX4,JULX4,AUGX4:
                   SEPX4,OCTX4,NOVX4,DECX4:
                   JANX5,FEBX5,MARX5,APRX5,MAYX5,JUNX5,JULX5,AUGX5:
                   SEPX5,OCTX5,NOVX5,DECX5:
                   JANX6,FEBX6,MARX6,APRX6,MAYX6,JUNX6,JULX6,AUGX6:
                   SEPX6,OCTX6,NOVX6,DECX6:
                   JANX7,FEBX7,MARX7,APRX7,MAYX7,JUNX7,JULX7,AUGX7:
                   SEPX7,OCTX7,NOVX7,DECX7:
                   JANX8,FEBX8,MARX8,APRX8,MAYX8,JUNX8,JULX8,AUGX8:
                   SEPX8,OCTX8,NOVX8,DECX8:
                   JANX9,FEBX9,MARX9,APRX9,MAYX9,JUNX9,JULX9,AUGX9:
                   SEPX9,OCTX9,NOVX9,DECX9:
                   JANX10,FEBX10,MARX10,APRX10,MAYX10,JUNX10,JULX10,AUGX10:
                   SEPX10,OCTX10,NOVX10,DECX10:
                   JANX11,FEBX11,MARX11,APRX11,MAYX11,JUNX11,JULX11,AUGX11:
                   SEPX11,OCTX11,NOVX11,DECX11:
                   JANX12,FEBX12,MARX12,APRX12,MAYX12,JUNX12,JULX12,AUGX12:
                   SEPX12,OCTX12,NOVX12,DECX12:
                   JANX13,FEBX13,MARX13,APRX13,MAYX13,JUNX13,JULX13,AUGX13:
                   SEPX13,OCTX13,NOVX13,DECX13:
                   JANX14,FEBX14,MARX14,APRX14,MAYX14,JUNX14,JULX14,AUGX14:
                   SEPX14,OCTX14,NOVX14,DECX14:
                   JANX15,FEBX15,MARX15,APRX15,MAYX15,JUNX15,JULX15,AUGX15:
                   SEPX15,OCTX15,NOVX15,DECX15:
                   JANX16,FEBX16,MARX16,APRX16,MAYX16,JUNX16,JULX16,AUGX16:
                   SEPX16,OCTX16,NOVX16,DECX16:
                   JANX17,FEBX17,MARX17,APRX17,MAYX17,JUNX17,JULX17,AUGX17:
                   SEPX17,OCTX17,NOVX17,DECX17:
                   JANX18,FEBX18,MARX18,APRX18,MAYX18,JUNX18,JULX18,AUGX18:
                   SEPX18,OCTX18,NOVX18,DECX18:
                   JANX19,FEBX19,MARX19,APRX19,MAYX19,JUNX19,JULX19,AUGX19:
                   SEPX19,OCTX19,NOVX19,DECX19:
                   JANX20,FEBX20,MARX20,APRX20,MAYX20,JUNX20,JULX20,AUGX20:
                   SEPX20,OCTX20,NOVX20,DECX20:
                   JANX21,FEBX21,MARX21,APRX21,MAYX21,JUNX21,JULX21,AUGX21:
                   SEPX21,OCTX21,NOVX21,DECX21:
                   JANX22,FEBX22,MARX22,APRX22,MAYX22,JUNX22,JULX22,AUGX22:
                   SEPX22,OCTX22,NOVX22,DECX22:
                   JANX23,FEBX23,MARX23,APRX23,MAYX23,JUNX23,JULX23,AUGX23:
                   SEPX23,OCTX23,NOVX23,DECX23:
                   JANX24,FEBX24,MARX24,APRX24,MAYX24,JUNX24,JULX24,AUGX24:
                   SEPX24,OCTX24,NOVX24,DECX24:
                   JANX25,FEBX25,MARX25,APRX25,MAYX25,JUNX25,JULX25,AUGX25:
                   SEPX25,OCTX25,NOVX25,DECX25
         LOAD      QTY FROM MO3 OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV:
                   DEC:
                   JAN1,FEB1,MAR1,APR1,MAY1,JUN1,JUL1,AUG1:
                   SEP1,OCT1,NOV1,DEC1:
                   JAN2,FEB2,MAR2,APR2,MAY2,JUN2,JUL2,AUG2:
                   SEP2,OCT2,NOV2,DEC2:
                   JAN3,FEB3,MAR3,APR3,MAY3,JUN3,JUL3,AUG3:
                   SEP3,OCT3,NOV3,DEC3:
                   JAN4,FEB4,MAR4,APR4,MAY4,JUN4,JUL4,AUG4:
                   SEP4,OCT4,NOV4,DEC4:
                   JAN5,FEB5,MAR5,APR5,MAY5,JUN5,JUL5,AUG5:
                   SEP5,OCT5,NOV5,DEC5:
                   JAN6,FEB6,MAR6,APR6,MAY6,JUN6,JUL6,AUG6:
                   SEP6,OCT6,NOV6,DEC6:
                   JAN7,FEB7,MAR7,APR7,MAY7,JUN7,JUL7,AUG7:
                   SEP7,OCT7,NOV7,DEC7:
                   JAN8,FEB8,MAR8,APR8,MAY8,JUN8,JUL8,AUG8:
                   SEP8,OCT8,NOV8,DEC8:
                   JAN9,FEB9,MAR9,APR9,MAY9,JUN9,JUL9,AUG9:
                   SEP9,OCT9,NOV9,DEC9:
                   JAN10,FEB10,MAR10,APR10,MAY10,JUN10,JUL10,AUG10:
                   SEP10,OCT10,NOV10,DEC10:
                   JAN11,FEB11,MAR11,APR11,MAY11,JUN11,JUL11,AUG11:
                   SEP11,OCT11,NOV11,DEC11:
                   JAN12,FEB12,MAR12,APR12,MAY12,JUN12,JUL12,AUG12:
                   SEP12,OCT12,NOV12,DEC12:
                   JAN13,FEB13,MAR13,APR13,MAY13,JUN13,JUL13,AUG13:
                   SEP13,OCT13,NOV13,DEC13:
                   JAN14,FEB14,MAR14,APR14,MAY14,JUN14,JUL14,AUG14:
                   SEP14,OCT14,NOV14,DEC14:
                   JAN15,FEB15,MAR15,APR15,MAY15,JUN15,JUL15,AUG15:
                   SEP15,OCT15,NOV15,DEC15:
                   JAN16,FEB16,MAR16,APR16,MAY16,JUN16,JUL16,AUG16:
                   SEP16,OCT16,NOV16,DEC16:
                   JAN17,FEB17,MAR17,APR17,MAY17,JUN17,JUL17,AUG17:
                   SEP17,OCT17,NOV17,DEC17:
                   JAN18,FEB18,MAR18,APR18,MAY18,JUN18,JUL18,AUG18:
                   SEP18,OCT18,NOV18,DEC18:
                   JAN19,FEB19,MAR19,APR19,MAY19,JUN19,JUL19,AUG19:
                   SEP19,OCT19,NOV19,DEC19:
                   JAN20,FEB20,MAR20,APR20,MAY20,JUN20,JUL20,AUG20:
                   SEP20,OCT20,NOV20,DEC20:
                   JAN21,FEB21,MAR21,APR21,MAY21,JUN21,JUL21,AUG21:
                   SEP21,OCT21,NOV21,DEC21:
                   JAN22,FEB22,MAR22,APR22,MAY22,JUN22,JUL22,AUG22:
                   SEP22,OCT22,NOV22,DEC22:
                   JAN23,FEB23,MAR23,APR23,MAY23,JUN23,JUL23,AUG23:
                   SEP23,OCT23,NOV23,DEC23:
                   JAN24,FEB24,MAR24,APR24,MAY24,JUN24,JUL24,AUG24:
                   SEP24,OCT24,NOV24,DEC24:
                   JAN25,FEB25,MAR25,APR25,MAY25,JUN25,JUL25,AUG25:
                   SEP25,OCT25,NOV25,DEC25
         ADD       QUANT,QTY
         STORE     QTY INTO MO3 OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV:
                   DEC:
                   JAN1,FEB1,MAR1,APR1,MAY1,JUN1,JUL1,AUG1:
                   SEP1,OCT1,NOV1,DEC1:
                   JAN2,FEB2,MAR2,APR2,MAY2,JUN2,JUL2,AUG2:
                   SEP2,OCT2,NOV2,DEC2:
                   JAN3,FEB3,MAR3,APR3,MAY3,JUN3,JUL3,AUG3:
                   SEP3,OCT3,NOV3,DEC3:
                   JAN4,FEB4,MAR4,APR4,MAY4,JUN4,JUL4,AUG4:
                   SEP4,OCT4,NOV4,DEC4:
                   JAN5,FEB5,MAR5,APR5,MAY5,JUN5,JUL5,AUG5:
                   SEP5,OCT5,NOV5,DEC5:
                   JAN6,FEB6,MAR6,APR6,MAY6,JUN6,JUL6,AUG6:
                   SEP6,OCT6,NOV6,DEC6:
                   JAN7,FEB7,MAR7,APR7,MAY7,JUN7,JUL7,AUG7:
                   SEP7,OCT7,NOV7,DEC7:
                   JAN8,FEB8,MAR8,APR8,MAY8,JUN8,JUL8,AUG8:
                   SEP8,OCT8,NOV8,DEC8:
                   JAN9,FEB9,MAR9,APR9,MAY9,JUN9,JUL9,AUG9:
                   SEP9,OCT9,NOV9,DEC9:
                   JAN10,FEB10,MAR10,APR10,MAY10,JUN10,JUL10,AUG10:
                   SEP10,OCT10,NOV10,DEC10:
                   JAN11,FEB11,MAR11,APR11,MAY11,JUN11,JUL11,AUG11:
                   SEP11,OCT11,NOV11,DEC11:
                   JAN12,FEB12,MAR12,APR12,MAY12,JUN12,JUL12,AUG12:
                   SEP12,OCT12,NOV12,DEC12:
                   JAN13,FEB13,MAR13,APR13,MAY13,JUN13,JUL13,AUG13:
                   SEP13,OCT13,NOV13,DEC13:
                   JAN14,FEB14,MAR14,APR14,MAY14,JUN14,JUL14,AUG14:
                   SEP14,OCT14,NOV14,DEC14:
                   JAN15,FEB15,MAR15,APR15,MAY15,JUN15,JUL15,AUG15:
                   SEP15,OCT15,NOV15,DEC15:
                   JAN16,FEB16,MAR16,APR16,MAY16,JUN16,JUL16,AUG16:
                   SEP16,OCT16,NOV16,DEC16:
                   JAN17,FEB17,MAR17,APR17,MAY17,JUN17,JUL17,AUG17:
                   SEP17,OCT17,NOV17,DEC17:
                   JAN18,FEB18,MAR18,APR18,MAY18,JUN18,JUL18,AUG18:
                   SEP18,OCT18,NOV18,DEC18:
                   JAN19,FEB19,MAR19,APR19,MAY19,JUN19,JUL19,AUG19:
                   SEP19,OCT19,NOV19,DEC19:
                   JAN20,FEB20,MAR20,APR20,MAY20,JUN20,JUL20,AUG20:
                   SEP20,OCT20,NOV20,DEC20:
                   JAN21,FEB21,MAR21,APR21,MAY21,JUN21,JUL21,AUG21:
                   SEP21,OCT21,NOV21,DEC21:
                   JAN22,FEB22,MAR22,APR22,MAY22,JUN22,JUL22,AUG22:
                   SEP22,OCT22,NOV22,DEC22:
                   JAN23,FEB23,MAR23,APR23,MAY23,JUN23,JUL23,AUG23:
                   SEP23,OCT23,NOV23,DEC23:
                   JAN24,FEB24,MAR24,APR24,MAY24,JUN24,JUL24,AUG24:
                   SEP24,OCT24,NOV24,DEC24:
                   JAN25,FEB25,MAR25,APR25,MAY25,JUN25,JUL25,AUG25:
                   SEP25,OCT25,NOV25,DEC25
         GOTO      READ1
.
. .....WRITES DATA TO 'SALESMANQTY'
.
.**************************************************************
NEWYEAR  LOAD      TOTORD FROM MNTH OF JANX:
                   FEBX,MARX,APRX,MAYX,JUNX,JULX,AUGX:
                   SEP,OCTX,NOVX,DECX
         LOAD      TOTORD1 FROM MNTH OF JANX1:
                   FEBX1,MARX1,APRX1,MAYX1,JUNX1:
                   JULX1,AUGX1,SEPX1,OCTX1,NOVX1,DECX1
        LOAD       TOTORD2 FROM MNTH OF JANX2,FEBX2,MARX2,APRX2,MAYX2,JUNX2:
                   JULX2,AUGX2,SEPX2,OCTX2,NOVX2,DECX2
        LOAD       TOTORD3 FROM MNTH OF JANX3,FEBX3,MARX3,APRX3,MAYX3,JUNX3:
                   JULX3,AUGX3,SEPX3,OCTX3,NOVX3,DECX3
         LOAD      TOTORD4 FROM MNTH OF JANX4,FEBX4,MARX4,APRX4,MAYX4,JUNX4:
                   JULX4,AUGX4,SEPX4,OCTX4,NOVX4,DECX4
         LOAD      TOTORD5 FROM MNTH OF JANX5,FEBX5,MARX5,APRX5,MAYX5,JUNX5:
                   JULX5,AUGX5,SEPX5,OCTX5,NOVX5,DECX5
         LOAD      TOTORD6 FROM MNTH OF JANX6,FEBX6,MARX6,APRX6,MAYX6,JUNX6:
                   JULX6,AUGX6,SEPX6,OCTX6,NOVX6,DECX6
         LOAD      TOTORD7 FROM MNTH OF JANX7,FEBX7,MARX7,APRX7,MAYX7,JUNX7:
                   JULX7,AUGX7,SEPX7,OCTX7,NOVX7,DECX7
         LOAD      TOTORD8 FROM MNTH OF JANX8,FEBX8,MARX8,APRX8,MAYX8,JUNX8:
                   JULX8,AUGX8,SEPX8,OCTX8,NOVX8,DECX8
         LOAD      TOTORD9 FROM MNTH OF JANX9,FEBX9,MARX9,APRX9,MAYX9,JUNX9:
                   JULX9,AUGX9,SEPX9,OCTX9,NOVX9,DECX9
         LOAD      TOTORD10 FROM MNTH OF JANX10:
                   FEBX10,MARX10,APRX10,MAYX10,JUNX10:
                   JULX10,AUGX10,SEPX10,OCTX10,NOVX10,DECX10
         LOAD      TOTORD11 FROM MNTH OF JANX11,FEBX11,MARX11,APRX11,MAYX11:
                   JUNX11,JULX11,AUGX11,SEPX11,OCTX11,NOVX11,DECX11
         LOAD      TOTORD12 FROM MNTH OF JANX12,FEBX12,MARX12,APRX12,MAYX12:
                   JUNX12,JULX12,AUGX12,SEPX12,OCTX12,NOVX12,DECX12
         LOAD      TOTORD13 FROM MNTH OF JANX13,FEBX13,MARX13,APRX13,MAYX13:
                   JUNX13,JULX13,AUGX13,SEPX13,OCTX13,NOVX13,DECX13
         LOAD      TOTORD14 FROM MNTH OF JANX14,FEBX14,MARX14,APRX14,MAYX14:
                   JUNX14,JULX14,AUGX14,SEPX14,OCTX14,NOVX14,DECX14
         LOAD      TOTORD15 FROM MNTH OF JANX15,FEBX15,MARX15,APRX15,MAYX15:
                   JUNX15,JULX15,AUGX15,SEPX15,OCTX15,NOVX15,DECX15
         LOAD      TOTORD16 FROM MNTH OF JANX16,FEBX16,MARX16,APRX16,MAYX16:
                   JUNX16,JULX16,AUGX16,SEPX16,OCTX16,NOVX16,DECX16
         LOAD      TOTORD17 FROM MNTH OF JANX17,FEBX17,MARX17,APRX17,MAYX17:
                   JUNX17,JULX17,AUGX17,SEPX17,OCTX17,NOVX17,DECX17
         LOAD      TOTORD18 FROM MNTH OF JANX18,FEBX18,MARX18,APRX18,MAYX18:
                   JUNX18,JULX18,AUGX18,SEPX18,OCTX18,NOVX18,DECX18
         LOAD      TOTORD19 FROM MNTH OF JANX19,FEBX19,MARX19,APRX19,MAYX19:
                   JUNX19,JULX19,AUGX19,SEPX19,OCTX19,NOVX19,DECX19
         LOAD      TOTORD20 FROM MNTH OF JANX20,FEBX20,MARX20,APRX20,MAYX20:
                   JUNX20,JULX20,AUGX20,SEPX20,OCTX20,NOVX20,DECX20
         LOAD      TOTORD21 FROM MNTH OF JANX21,FEBX21,MARX21,APRX21,MAYX21:
                   JUNX21,JULX21,AUGX21,SEPX21,OCTX21,NOVX21,DECX21
         LOAD      TOTORD22 FROM MNTH OF JANX22,FEBX22,MARX22,APRX22,MAYX22:
                   JUNX22,JULX22,AUGX22,SEPX22,OCTX22,NOVX22,DECX22
         LOAD      TOTORD23 FROM MNTH OF JANX23,FEBX23,MARX23,APRX23,MAYX23:
                   JUNX23,JULX23,AUGX23,SEPX23,OCTX23,NOVX23,DECX23
         LOAD      TOTORD24 FROM MNTH OF JANX24,FEBX24,MARX24,APRX24,MAYX24:
                   JUNX24,JULX24,AUGX24,SEPX24,OCTX24,NOVX24,DECX24
         LOAD      TOTORD25 FROM MNTH OF JANX25,FEBX25,MARX25,APRX25,MAYX25:
                   JUNX25,JULX25,AUGX25,SEPX25,OCTX25,NOVX25,DECX25
........................
         LOAD      TOTQTY FROM MNTH OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT:
                   NOV,DEC
         LOAD      TOTQTY1 FROM MNTH OF JAN1,FEB1,MAR1,APR1,MAY1,JUN1,JUL1:
                   AUG1,SEP1,OCT1,NOV1,DEC1
         LOAD      TOTQTY2 FROM MNTH OF JAN2,FEB2,MAR2,APR2,MAY2,JUN2,JUL2:
                   AUG2,SEP2,OCT2,NOV2,DEC2
         LOAD      TOTQTY3 FROM MNTH OF JAN3,FEB3,MAR3,APR3,MAY3,JUN3,JUL3:
                   AUG3,SEP3,OCT3,NOV3,DEC3
         LOAD      TOTQTY4 FROM MNTH OF JAN4,FEB4,MAR4,APR4,MAY4,JUN4,JUL4:
                   AUG4,SEP4,OCT4,NOV4,DEC4
         LOAD      TOTQTY5 FROM MNTH OF JAN5,FEB5,MAR5,APR5,MAY5,JUN5,JUL5:
                   AUG5,SEP5,OCT5,NOV5,DEC5
         LOAD      TOTQTY6 FROM MNTH OF JAN6,FEB6,MAR6,APR6,MAY6,JUN6,JUL6:
                   AUG6,SEP6,OCT6,NOV6,DEC6
         LOAD      TOTQTY7 FROM MNTH OF JAN7,FEB7,MAR7,APR7,MAY7,JUN7:
                   JUL7,AUG7,SEP7,OCT7,NOV7,DEC7
         LOAD      TOTQTY8 FROM MNTH OF JAN8,FEB8,MAR8,APR8,MAY8,JUN8:
                   JUL8,AUG8,SEP8,OCT8,NOV8,DEC8
         LOAD      TOTQTY9 FROM MNTH OF JAN9,FEB9,MAR9,APR9,MAY9,JUN9:
                   JUL9,AUG9,SEP9,OCT9,NOV9,DEC9
         LOAD      TOTQTY10 FROM MNTH OF JAN10:
                   FEB10,MAR10,APR10,MAY10,JUN10:
                   JUL10,AUG10,SEP10,OCT10,NOV10,DEC10
         LOAD      TOTQTY11 FROM MNTH OF JAN11,FEB11,MAR11,APR11,MAY11:
                   JUN11,JUL11,AUG11,SEP11,OCT11,NOV11,DEC11
         LOAD      TOTQTY12 FROM MNTH OF JAN12,FEB12,MAR12,APR12,MAY12:
                   JUN12,JUL12,AUG12,SEP12,OCT12,NOV12,DEC12
         LOAD      TOTQTY13 FROM MNTH OF JAN13,FEB13,MAR13,APR13,MAY13:
                   JUN13,JUL13,AUG13,SEP13,OCT13,NOV13,DEC13
         LOAD      TOTQTY14 FROM MNTH OF JAN14,FEB14,MAR14,APR14,MAY14:
                   JUN14,JUL14,AUG14,SEP14,OCT14,NOV14,DEC14
         LOAD      TOTQTY15 FROM MNTH OF JAN15,FEB15,MAR15,APR15,MAY15:
                   JUN15,JUL15,AUG15,SEP15,OCT15,NOV15,DEC15
         LOAD      TOTQTY16 FROM MNTH OF JAN16,FEB16,MAR16,APR16,MAY16:
                   JUN16,JUL16,AUG16,SEP16,OCT16,NOV16,DEC16
         LOAD      TOTQTY17 FROM MNTH OF JAN17,FEB17,MAR17,APR17,MAY17:
                   JUN17,JUL17,AUG17,SEP17,OCT17,NOV17,DEC17
         LOAD      TOTQTY18 FROM MNTH OF JAN18,FEB18,MAR18,APR18,MAY18:
                   JUN18,JUL18,AUG18,SEP18,OCT18,NOV18,DEC18
         LOAD      TOTQTY19 FROM MNTH OF JAN19,FEB19,MAR19,APR19,MAY19:
                   JUN19,JUL19,AUG19,SEP19,OCT19,NOV19,DEC19
         LOAD      TOTQTY20 FROM MNTH OF JAN20,FEB20,MAR20,APR20,MAY20:
                   JUN20,JUL20,AUG20,SEP20,OCT20,NOV20,DEC20
         LOAD      TOTQTY21 FROM MNTH OF JAN21,FEB21,MAR21,APR21,MAY21:
                   JUN21,JUL21,AUG21,SEP21,OCT21,NOV21,DEC21
         LOAD      TOTQTY22 FROM MNTH OF JAN22,FEB22,MAR22,APR22,MAY22:
                   JUN22,JUL22,AUG22,SEP22,OCT22,NOV22,DEC22
         LOAD      TOTQTY23 FROM MNTH OF JAN23,FEB23,MAR23,APR23,MAY23:
                   JUN23,JUL23,AUG23,SEP23,OCT23,NOV23,DEC23
         LOAD      TOTQTY24 FROM MNTH OF JAN24,FEB24,MAR24,APR24,MAY24:
                   JUN24,JUL24,AUG24,SEP24,OCT24,NOV24,DEC24
         LOAD      TOTQTY25 FROM MNTH OF JAN25,FEB25,MAR25,APR25,MAY25:
                   JUN25,JUL25,AUG25,SEP25,OCT25,NOV25,DEC25
         TRAP      ERR giving error IF RANGE
         MOVE      "01" TO TAB
         WRITAB    OUTPUT,RN;*TAB,MNTH,HOLDYR,TOTORD,TOTQTY:
                   MNTH,HOLDYR,TOTORD1,TOTQTY1,MNTH,HOLDYR,TOTORD2,TOTQTY2:
                   MNTH,HOLDYR,TOTORD3,TOTQTY3,MNTH,HOLDYR,TOTORD4,TOTQTY4:
                   MNTH,HOLDYR,TOTORD5,TOTQTY5,MNTH,HOLDYR,TOTORD6,TOTQTY6:
                   MNTH,HOLDYR,TOTORD7,TOTQTY7,MNTH,HOLDYR,TOTORD8,TOTQTY8:
                   MNTH,HOLDYR,TOTORD9,TOTQTY9,MNTH,HOLDYR,TOTORD10,TOTQTY10:
                   MNTH,HOLDYR,TOTORD11,TOTQTY11,MNTH,HOLDYR,TOTORD12,TOTQTY12:
                   MNTH,HOLDYR,TOTORD13,TOTQTY13,MNTH,HOLDYR,TOTORD14,TOTQTY14:
                   MNTH,HOLDYR,TOTORD15,TOTQTY15,MNTH,HOLDYR,TOTORD16,TOTQTY16:
                   MNTH,HOLDYR,TOTORD17,TOTQTY17,MNTH,HOLDYR,TOTORD18,TOTQTY18:
                   MNTH,HOLDYR,TOTORD19,TOTQTY19,MNTH,HOLDYR,TOTORD20,TOTQTY20:
                   MNTH,HOLDYR,TOTORD21,TOTQTY21,MNTH,HOLDYR,TOTORD22,TOTQTY22:
                   MNTH,HOLDYR,TOTORD23,TOTQTY23,MNTH,HOLDYR,TOTORD24,TOTQTY24:
                   MNTH,HOLDYR,TOTORD25,TOTQTY25
         ADD       "1",RN
         ADD       "1",MNTH
         COMPARE   "13",MNTH
         GOTO      NEWYEAR IF NOT EQUAL
.
         ADD       JANX TO FINORD
         ADD       FEBX TO FINORD
         ADD       MARX TO FINORD
         ADD       APRX TO FINORD
         ADD       MAYX TO FINORD
         ADD       JUNX TO FINORD
         ADD       JULX TO FINORD
         ADD       AUGX TO FINORD
         ADD       SEPX TO FINORD
         ADD       OCTX TO FINORD
         ADD       NOVX TO FINORD
         ADD       DECX TO FINORD
.
         ADD       JANX1 TO FINORD1
         ADD       FEBX1 TO FINORD1
         ADD       MARX1 TO FINORD1
         ADD       APRX1 TO FINORD1
         ADD       MAYX1 TO FINORD1
         ADD       JUNX1 TO FINORD1
         ADD       JULX1 TO FINORD1
         ADD       AUGX1 TO FINORD1
         ADD       SEPX1 TO FINORD1
         ADD       OCTX1 TO FINORD1
         ADD       NOVX1 TO FINORD1
         ADD       DECX1 TO FINORD1
.
         ADD       JANX2 TO FINORD2
         ADD       FEBX2 TO FINORD2
         ADD       MARX2 TO FINORD2
         ADD       APRX2 TO FINORD2
         ADD       MAYX2 TO FINORD2
         ADD       JUNX2 TO FINORD2
         ADD       JULX2 TO FINORD2
         ADD       AUGX2 TO FINORD2
         ADD       SEPX2 TO FINORD2
         ADD       OCTX2 TO FINORD2
         ADD       NOVX2 TO FINORD2
         ADD       DECX2 TO FINORD2
.
         ADD       JANX3 TO FINORD3
         ADD       FEBX3 TO FINORD3
         ADD       MARX3 TO FINORD3
         ADD       APRX3 TO FINORD3
         ADD       MAYX3 TO FINORD3
         ADD       JUNX3 TO FINORD3
         ADD       JULX3 TO FINORD3
         ADD       AUGX3 TO FINORD3
         ADD       SEPX3 TO FINORD3
         ADD       OCTX3 TO FINORD3
         ADD       NOVX3 TO FINORD3
         ADD       DECX3 TO FINORD3
.
         ADD       JANX4 TO FINORD4
         ADD       FEBX4 TO FINORD4
         ADD       MARX4 TO FINORD4
         ADD       APRX4 TO FINORD4
         ADD       MAYX4 TO FINORD4
         ADD       JUNX4 TO FINORD4
         ADD       JULX4 TO FINORD4
         ADD       AUGX4 TO FINORD4
         ADD       SEPX4 TO FINORD4
         ADD       OCTX4 TO FINORD4
         ADD       NOVX4 TO FINORD4
         ADD       DECX4 TO FINORD4
.
         ADD       JANX5 TO FINORD5
         ADD       FEBX5 TO FINORD5
         ADD       MARX5 TO FINORD5
         ADD       APRX5 TO FINORD5
         ADD       MAYX5 TO FINORD5
         ADD       JUNX5 TO FINORD5
         ADD       JULX5 TO FINORD5
         ADD       AUGX5 TO FINORD5
         ADD       SEPX5 TO FINORD5
         ADD       OCTX5 TO FINORD5
         ADD       NOVX5 TO FINORD5
         ADD       DECX5 TO FINORD5
.
         ADD       JANX6 TO FINORD6
         ADD       FEBX6 TO FINORD6
         ADD       MARX6 TO FINORD6
         ADD       APRX6 TO FINORD6
         ADD       MAYX6 TO FINORD6
         ADD       JUNX6 TO FINORD6
         ADD       JULX6 TO FINORD6
         ADD       AUGX6 TO FINORD6
         ADD       SEPX6 TO FINORD6
         ADD       OCTX6 TO FINORD6
         ADD       NOVX6 TO FINORD6
         ADD       DECX6 TO FINORD6
.
         ADD       JANX7 TO FINORD7
         ADD       FEBX7 TO FINORD7
         ADD       MARX7 TO FINORD7
         ADD       APRX7 TO FINORD7
         ADD       MAYX7 TO FINORD7
         ADD       JUNX7 TO FINORD7
         ADD       JULX7 TO FINORD7
         ADD       AUGX7 TO FINORD7
         ADD       SEPX7 TO FINORD7
         ADD       OCTX7 TO FINORD7
         ADD       NOVX7 TO FINORD7
         ADD       DECX7 TO FINORD7
.
         ADD       JANX8 TO FINORD8
         ADD       FEBX8 TO FINORD8
         ADD       MARX8 TO FINORD8
         ADD       APRX8 TO FINORD8
         ADD       MAYX8 TO FINORD8
         ADD       JUNX8 TO FINORD8
         ADD       JULX8 TO FINORD8
         ADD       AUGX8 TO FINORD8
         ADD       SEPX8 TO FINORD8
         ADD       OCTX8 TO FINORD8
         ADD       NOVX8 TO FINORD8
         ADD       DECX8 TO FINORD8
.
         ADD       JANX9 TO FINORD9
         ADD       FEBX9 TO FINORD9
         ADD       MARX9 TO FINORD9
         ADD       APRX9 TO FINORD9
         ADD       MAYX9 TO FINORD9
         ADD       JUNX9 TO FINORD9
         ADD       JULX9 TO FINORD9
         ADD       AUGX9 TO FINORD9
         ADD       SEPX9 TO FINORD9
         ADD       OCTX9 TO FINORD9
         ADD       NOVX9 TO FINORD9
         ADD       DECX9 TO FINORD9
.
         ADD       JANX10 TO FINORD10
         ADD       FEBX10 TO FINORD10
         ADD       MARX10 TO FINORD10
         ADD       APRX10 TO FINORD10
         ADD       MAYX10 TO FINORD10
         ADD       JUNX10 TO FINORD10
         ADD       JULX10 TO FINORD10
         ADD       AUGX10 TO FINORD10
         ADD       SEPX10 TO FINORD10
         ADD       OCTX10 TO FINORD10
         ADD       NOVX10 TO FINORD10
         ADD       DECX10 TO FINORD10
.
         ADD       JANX11 TO FINORD11
         ADD       FEBX11 TO FINORD11
         ADD       MARX11 TO FINORD11
         ADD       APRX11 TO FINORD11
         ADD       MAYX11 TO FINORD11
         ADD       JUNX11 TO FINORD11
         ADD       JULX11 TO FINORD11
         ADD       AUGX11 TO FINORD11
         ADD       SEPX11 TO FINORD11
         ADD       OCTX11 TO FINORD11
         ADD       NOVX11 TO FINORD11
         ADD       DECX11 TO FINORD11
.
         ADD       JANX12 TO FINORD12
         ADD       FEBX12 TO FINORD12
         ADD       MARX12 TO FINORD12
         ADD       APRX12 TO FINORD12
         ADD       MAYX12 TO FINORD12
         ADD       JUNX12 TO FINORD12
         ADD       JULX12 TO FINORD12
         ADD       AUGX12 TO FINORD12
         ADD       SEPX12 TO FINORD12
         ADD       OCTX12 TO FINORD12
         ADD       NOVX12 TO FINORD12
         ADD       DECX12 TO FINORD12
.
         ADD       JANX13 TO FINORD13
         ADD       FEBX13 TO FINORD13
         ADD       MARX13 TO FINORD13
         ADD       APRX13 TO FINORD13
         ADD       MAYX13 TO FINORD13
         ADD       JUNX13 TO FINORD13
         ADD       JULX13 TO FINORD13
         ADD       AUGX13 TO FINORD13
         ADD       SEPX13 TO FINORD13
         ADD       OCTX13 TO FINORD13
         ADD       NOVX13 TO FINORD13
         ADD       DECX13 TO FINORD13
.
         ADD       JANX14 TO FINORD14
         ADD       FEBX14 TO FINORD14
         ADD       MARX14 TO FINORD14
         ADD       APRX14 TO FINORD14
         ADD       MAYX14 TO FINORD14
         ADD       JUNX14 TO FINORD14
         ADD       JULX14 TO FINORD14
         ADD       AUGX14 TO FINORD14
         ADD       SEPX14 TO FINORD14
         ADD       OCTX14 TO FINORD14
         ADD       NOVX14 TO FINORD14
         ADD       DECX14 TO FINORD14
.
         ADD       JANX15 TO FINORD15
         ADD       FEBX15 TO FINORD15
         ADD       MARX15 TO FINORD15
         ADD       APRX15 TO FINORD15
         ADD       MAYX15 TO FINORD15
         ADD       JUNX15 TO FINORD15
         ADD       JULX15 TO FINORD15
         ADD       AUGX15 TO FINORD15
         ADD       SEPX15 TO FINORD15
         ADD       OCTX15 TO FINORD15
         ADD       NOVX15 TO FINORD15
         ADD       DECX15 TO FINORD15
.
         ADD       JANX16 TO FINORD16
         ADD       FEBX16 TO FINORD16
         ADD       MARX16 TO FINORD16
         ADD       APRX16 TO FINORD16
         ADD       MAYX16 TO FINORD16
         ADD       JUNX16 TO FINORD16
         ADD       JULX16 TO FINORD16
         ADD       AUGX16 TO FINORD16
         ADD       SEPX16 TO FINORD16
         ADD       OCTX16 TO FINORD16
         ADD       NOVX16 TO FINORD16
         ADD       DECX16 TO FINORD16
.
         ADD       JANX17 TO FINORD17
         ADD       FEBX17 TO FINORD17
         ADD       MARX17 TO FINORD17
         ADD       APRX17 TO FINORD17
         ADD       MAYX17 TO FINORD17
         ADD       JUNX17 TO FINORD17
         ADD       JULX17 TO FINORD17
         ADD       AUGX17 TO FINORD17
         ADD       SEPX17 TO FINORD17
         ADD       OCTX17 TO FINORD17
         ADD       NOVX17 TO FINORD17
         ADD       DECX17 TO FINORD17
.
         ADD       JANX18 TO FINORD18
         ADD       FEBX18 TO FINORD18
         ADD       MARX18 TO FINORD18
         ADD       APRX18 TO FINORD18
         ADD       MAYX18 TO FINORD18
         ADD       JUNX18 TO FINORD18
         ADD       JULX18 TO FINORD18
         ADD       AUGX18 TO FINORD18
         ADD       SEPX18 TO FINORD18
         ADD       OCTX18 TO FINORD18
         ADD       NOVX18 TO FINORD18
         ADD       DECX18 TO FINORD18
.
         ADD       JANX19 TO FINORD19
         ADD       FEBX19 TO FINORD19
         ADD       MARX19 TO FINORD19
         ADD       APRX19 TO FINORD19
         ADD       MAYX19 TO FINORD19
         ADD       JUNX19 TO FINORD19
         ADD       JULX19 TO FINORD19
         ADD       AUGX19 TO FINORD19
         ADD       SEPX19 TO FINORD19
         ADD       OCTX19 TO FINORD19
         ADD       NOVX19 TO FINORD19
         ADD       DECX19 TO FINORD19
.
         ADD       JANX20 TO FINORD20
         ADD       FEBX20 TO FINORD20
         ADD       MARX20 TO FINORD20
         ADD       APRX20 TO FINORD20
         ADD       MAYX20 TO FINORD20
         ADD       JUNX20 TO FINORD20
         ADD       JULX20 TO FINORD20
         ADD       AUGX20 TO FINORD20
         ADD       SEPX20 TO FINORD20
         ADD       OCTX20 TO FINORD20
         ADD       NOVX20 TO FINORD20
         ADD       DECX20 TO FINORD20
.
         ADD       JANX21 TO FINORD21
         ADD       FEBX21 TO FINORD21
         ADD       MARX21 TO FINORD21
         ADD       APRX21 TO FINORD21
         ADD       MAYX21 TO FINORD21
         ADD       JUNX21 TO FINORD21
         ADD       JULX21 TO FINORD21
         ADD       AUGX21 TO FINORD21
         ADD       SEPX21 TO FINORD21
         ADD       OCTX21 TO FINORD21
         ADD       NOVX21 TO FINORD21
         ADD       DECX21 TO FINORD21
.
         ADD       JANX22 TO FINORD22
         ADD       FEBX22 TO FINORD22
         ADD       MARX22 TO FINORD22
         ADD       APRX22 TO FINORD22
         ADD       MAYX22 TO FINORD22
         ADD       JUNX22 TO FINORD22
         ADD       JULX22 TO FINORD22
         ADD       AUGX22 TO FINORD22
         ADD       SEPX22 TO FINORD22
         ADD       OCTX22 TO FINORD22
         ADD       NOVX22 TO FINORD22
         ADD       DECX22 TO FINORD22
.
         ADD       JANX23 TO FINORD23
         ADD       FEBX23 TO FINORD23
         ADD       MARX23 TO FINORD23
         ADD       APRX23 TO FINORD23
         ADD       MAYX23 TO FINORD23
         ADD       JUNX23 TO FINORD23
         ADD       JULX23 TO FINORD23
         ADD       AUGX23 TO FINORD23
         ADD       SEPX23 TO FINORD23
         ADD       OCTX23 TO FINORD23
         ADD       NOVX23 TO FINORD23
         ADD       DECX23 TO FINORD23
.
         ADD       JANX24 TO FINORD24
         ADD       FEBX24 TO FINORD24
         ADD       MARX24 TO FINORD24
         ADD       APRX24 TO FINORD24
         ADD       MAYX24 TO FINORD24
         ADD       JUNX24 TO FINORD24
         ADD       JULX24 TO FINORD24
         ADD       AUGX24 TO FINORD24
         ADD       SEPX24 TO FINORD24
         ADD       OCTX24 TO FINORD24
         ADD       NOVX24 TO FINORD24
         ADD       DECX24 TO FINORD24
.
         ADD       JANX25 TO FINORD25
         ADD       FEBX25 TO FINORD25
         ADD       MARX25 TO FINORD25
         ADD       APRX25 TO FINORD25
         ADD       MAYX25 TO FINORD25
         ADD       JUNX25 TO FINORD25
         ADD       JULX25 TO FINORD25
         ADD       AUGX25 TO FINORD25
         ADD       SEPX25 TO FINORD25
         ADD       OCTX25 TO FINORD25
         ADD       NOVX25 TO FINORD25
         ADD       DECX25 TO FINORD25
.
         ADD       JAN TO EOYQTY
         ADD       FEB TO EOYQTY
         ADD       MAR TO EOYQTY
         ADD       APR TO EOYQTY
         ADD       MAY TO EOYQTY
         ADD       JUN TO EOYQTY
         ADD       JUL TO EOYQTY
         ADD       AUG TO EOYQTY
         ADD       SEP TO EOYQTY
         ADD       OCT TO EOYQTY
         ADD       NOV TO EOYQTY
         ADD       DEC TO EOYQTY
.
         ADD       JAN1 TO EOYQTY1
         ADD       FEB1 TO EOYQTY1
         ADD       MAR1 TO EOYQTY1
         ADD       APR1 TO EOYQTY1
         ADD       MAY1 TO EOYQTY1
         ADD       JUN1 TO EOYQTY1
         ADD       JUL1 TO EOYQTY1
         ADD       AUG1 TO EOYQTY1
         ADD       SEP1 TO EOYQTY1
         ADD       OCT1 TO EOYQTY1
         ADD       NOV1 TO EOYQTY1
         ADD       DEC1 TO EOYQTY1
.
         ADD       JAN2 TO EOYQTY2
         ADD       FEB2 TO EOYQTY2
         ADD       MAR2 TO EOYQTY2
         ADD       APR2 TO EOYQTY2
         ADD       MAY2 TO EOYQTY2
         ADD       JUN2 TO EOYQTY2
         ADD       JUL2 TO EOYQTY2
         ADD       AUG2 TO EOYQTY2
         ADD       SEP2 TO EOYQTY2
         ADD       OCT2 TO EOYQTY2
         ADD       NOV2 TO EOYQTY2
         ADD       DEC2 TO EOYQTY2
.
         ADD       JAN3 TO EOYQTY3
         ADD       FEB3 TO EOYQTY3
         ADD       MAR3 TO EOYQTY3
         ADD       APR3 TO EOYQTY3
         ADD       MAY3 TO EOYQTY3
         ADD       JUN3 TO EOYQTY3
         ADD       JUL3 TO EOYQTY3
         ADD       AUG3 TO EOYQTY3
         ADD       SEP3 TO EOYQTY3
         ADD       OCT3 TO EOYQTY3
         ADD       NOV3 TO EOYQTY3
         ADD       DEC3 TO EOYQTY3
.
         ADD       JAN4 TO EOYQTY4
         ADD       FEB4 TO EOYQTY4
         ADD       MAR4 TO EOYQTY4
         ADD       APR4 TO EOYQTY4
         ADD       MAY4 TO EOYQTY4
         ADD       JUN4 TO EOYQTY4
         ADD       JUL4 TO EOYQTY4
         ADD       AUG4 TO EOYQTY4
         ADD       SEP4 TO EOYQTY4
         ADD       OCT4 TO EOYQTY4
         ADD       NOV4 TO EOYQTY4
         ADD       DEC4 TO EOYQTY4
.
         ADD       JAN5 TO EOYQTY5
         ADD       FEB5 TO EOYQTY5
         ADD       MAR5 TO EOYQTY5
         ADD       APR5 TO EOYQTY5
         ADD       MAY5 TO EOYQTY5
         ADD       JUN5 TO EOYQTY5
         ADD       JUL5 TO EOYQTY5
         ADD       AUG5 TO EOYQTY5
         ADD       SEP5 TO EOYQTY5
         ADD       OCT5 TO EOYQTY5
         ADD       NOV5 TO EOYQTY5
         ADD       DEC5 TO EOYQTY5
.
         ADD       JAN6 TO EOYQTY6
         ADD       FEB6 TO EOYQTY6
         ADD       MAR6 TO EOYQTY6
         ADD       APR6 TO EOYQTY6
         ADD       MAY6 TO EOYQTY6
         ADD       JUN6 TO EOYQTY6
         ADD       JUL6 TO EOYQTY6
         ADD       AUG6 TO EOYQTY6
         ADD       SEP6 TO EOYQTY6
         ADD       OCT6 TO EOYQTY6
         ADD       NOV6 TO EOYQTY6
         ADD       DEC6 TO EOYQTY6
.
         ADD       JAN7 TO EOYQTY7
         ADD       FEB7 TO EOYQTY7
         ADD       MAR7 TO EOYQTY7
         ADD       APR7 TO EOYQTY7
         ADD       MAY7 TO EOYQTY7
         ADD       JUN7 TO EOYQTY7
         ADD       JUL7 TO EOYQTY7
         ADD       AUG7 TO EOYQTY7
         ADD       SEP7 TO EOYQTY7
         ADD       OCT7 TO EOYQTY7
         ADD       NOV7 TO EOYQTY7
         ADD       DEC7 TO EOYQTY7
.
         ADD       JAN8 TO EOYQTY8
         ADD       FEB8 TO EOYQTY8
         ADD       MAR8 TO EOYQTY8
         ADD       APR8 TO EOYQTY8
         ADD       MAY8 TO EOYQTY8
         ADD       JUN8 TO EOYQTY8
         ADD       JUL8 TO EOYQTY8
         ADD       AUG8 TO EOYQTY8
         ADD       SEP8 TO EOYQTY8
         ADD       OCT8 TO EOYQTY8
         ADD       NOV8 TO EOYQTY8
         ADD       DEC8 TO EOYQTY8
.
         ADD       JAN9 TO EOYQTY9
         ADD       FEB9 TO EOYQTY9
         ADD       MAR9 TO EOYQTY9
         ADD       APR9 TO EOYQTY9
         ADD       MAY9 TO EOYQTY9
         ADD       JUN9 TO EOYQTY9
         ADD       JUL9 TO EOYQTY9
         ADD       AUG9 TO EOYQTY9
         ADD       SEP9 TO EOYQTY9
         ADD       OCT9 TO EOYQTY9
         ADD       NOV9 TO EOYQTY9
         ADD       DEC9 TO EOYQTY9
.
         ADD       JAN10 TO EOYQTY10
         ADD       FEB10 TO EOYQTY10
         ADD       MAR10 TO EOYQTY10
         ADD       APR10 TO EOYQTY10
         ADD       MAY10 TO EOYQTY10
         ADD       JUN10 TO EOYQTY10
         ADD       JUL10 TO EOYQTY10
         ADD       AUG10 TO EOYQTY10
         ADD       SEP10 TO EOYQTY10
         ADD       OCT10 TO EOYQTY10
         ADD       NOV10 TO EOYQTY10
         ADD       DEC10 TO EOYQTY10
.
         ADD       JAN11 TO EOYQTY11
         ADD       FEB11 TO EOYQTY11
         ADD       MAR11 TO EOYQTY11
         ADD       APR11 TO EOYQTY11
         ADD       MAY11 TO EOYQTY11
         ADD       JUN11 TO EOYQTY11
         ADD       JUL11 TO EOYQTY11
         ADD       AUG11 TO EOYQTY11
         ADD       SEP11 TO EOYQTY11
         ADD       OCT11 TO EOYQTY11
         ADD       NOV11 TO EOYQTY11
         ADD       DEC11 TO EOYQTY11
.
         ADD       JAN12 TO EOYQTY12
         ADD       FEB12 TO EOYQTY12
         ADD       MAR12 TO EOYQTY12
         ADD       APR12 TO EOYQTY12
         ADD       MAY12 TO EOYQTY12
         ADD       JUN12 TO EOYQTY12
         ADD       JUL12 TO EOYQTY12
         ADD       AUG12 TO EOYQTY12
         ADD       SEP12 TO EOYQTY12
         ADD       OCT12 TO EOYQTY12
         ADD       NOV12 TO EOYQTY12
         ADD       DEC12 TO EOYQTY12
.
         ADD       JAN13 TO EOYQTY13
         ADD       FEB13 TO EOYQTY13
         ADD       MAR13 TO EOYQTY13
         ADD       APR13 TO EOYQTY13
         ADD       MAY13 TO EOYQTY13
         ADD       JUN13 TO EOYQTY13
         ADD       JUL13 TO EOYQTY13
         ADD       AUG13 TO EOYQTY13
         ADD       SEP13 TO EOYQTY13
         ADD       OCT13 TO EOYQTY13
         ADD       NOV13 TO EOYQTY13
         ADD       DEC13 TO EOYQTY13
.
         ADD       JAN14 TO EOYQTY14
         ADD       FEB14 TO EOYQTY14
         ADD       MAR14 TO EOYQTY14
         ADD       APR14 TO EOYQTY14
         ADD       MAY14 TO EOYQTY14
         ADD       JUN14 TO EOYQTY14
         ADD       JUL14 TO EOYQTY14
         ADD       AUG14 TO EOYQTY14
         ADD       SEP14 TO EOYQTY14
         ADD       OCT14 TO EOYQTY14
         ADD       NOV14 TO EOYQTY14
         ADD       DEC14 TO EOYQTY14
.
         ADD       JAN15 TO EOYQTY15
         ADD       FEB15 TO EOYQTY15
         ADD       MAR15 TO EOYQTY15
         ADD       APR15 TO EOYQTY15
         ADD       MAY15 TO EOYQTY15
         ADD       JUN15 TO EOYQTY15
         ADD       JUL15 TO EOYQTY15
         ADD       AUG15 TO EOYQTY15
         ADD       SEP15 TO EOYQTY15
         ADD       OCT15 TO EOYQTY15
         ADD       NOV15 TO EOYQTY15
         ADD       DEC15 TO EOYQTY15
.
         ADD       JAN16 TO EOYQTY16
         ADD       FEB16 TO EOYQTY16
         ADD       MAR16 TO EOYQTY16
         ADD       APR16 TO EOYQTY16
         ADD       MAY16 TO EOYQTY16
         ADD       JUN16 TO EOYQTY16
         ADD       JUL16 TO EOYQTY16
         ADD       AUG16 TO EOYQTY16
         ADD       SEP16 TO EOYQTY16
         ADD       OCT16 TO EOYQTY16
         ADD       NOV16 TO EOYQTY16
         ADD       DEC16 TO EOYQTY16
.
         ADD       JAN17 TO EOYQTY17
         ADD       FEB17 TO EOYQTY17
         ADD       MAR17 TO EOYQTY17
         ADD       APR17 TO EOYQTY17
         ADD       MAY17 TO EOYQTY17
         ADD       JUN17 TO EOYQTY17
         ADD       JUL17 TO EOYQTY17
         ADD       AUG17 TO EOYQTY17
         ADD       SEP17 TO EOYQTY17
         ADD       OCT17 TO EOYQTY17
         ADD       NOV17 TO EOYQTY17
         ADD       DEC17 TO EOYQTY17
.
         ADD       JAN18 TO EOYQTY18
         ADD       FEB18 TO EOYQTY18
         ADD       MAR18 TO EOYQTY18
         ADD       APR18 TO EOYQTY18
         ADD       MAY18 TO EOYQTY18
         ADD       JUN18 TO EOYQTY18
         ADD       JUL18 TO EOYQTY18
         ADD       AUG18 TO EOYQTY18
         ADD       SEP18 TO EOYQTY18
         ADD       OCT18 TO EOYQTY18
         ADD       NOV18 TO EOYQTY18
         ADD       DEC18 TO EOYQTY18
.
         ADD       JAN19 TO EOYQTY19
         ADD       FEB19 TO EOYQTY19
         ADD       MAR19 TO EOYQTY19
         ADD       APR19 TO EOYQTY19
         ADD       MAY19 TO EOYQTY19
         ADD       JUN19 TO EOYQTY19
         ADD       JUL19 TO EOYQTY19
         ADD       AUG19 TO EOYQTY19
         ADD       SEP19 TO EOYQTY19
         ADD       OCT19 TO EOYQTY19
         ADD       NOV19 TO EOYQTY19
         ADD       DEC19 TO EOYQTY19
.
         ADD       JAN20 TO EOYQTY20
         ADD       FEB20 TO EOYQTY20
         ADD       MAR20 TO EOYQTY20
         ADD       APR20 TO EOYQTY20
         ADD       MAY20 TO EOYQTY20
         ADD       JUN20 TO EOYQTY20
         ADD       JUL20 TO EOYQTY20
         ADD       AUG20 TO EOYQTY20
         ADD       SEP20 TO EOYQTY20
         ADD       OCT20 TO EOYQTY20
         ADD       NOV20 TO EOYQTY20
         ADD       DEC20 TO EOYQTY20
.
         ADD       JAN21 TO EOYQTY21
         ADD       FEB21 TO EOYQTY21
         ADD       MAR21 TO EOYQTY21
         ADD       APR21 TO EOYQTY21
         ADD       MAY21 TO EOYQTY21
         ADD       JUN21 TO EOYQTY21
         ADD       JUL21 TO EOYQTY21
         ADD       AUG21 TO EOYQTY21
         ADD       SEP21 TO EOYQTY21
         ADD       OCT21 TO EOYQTY21
         ADD       NOV21 TO EOYQTY21
         ADD       DEC21 TO EOYQTY21
.
         ADD       JAN22 TO EOYQTY22
         ADD       FEB22 TO EOYQTY22
         ADD       MAR22 TO EOYQTY22
         ADD       APR22 TO EOYQTY22
         ADD       MAY22 TO EOYQTY22
         ADD       JUN22 TO EOYQTY22
         ADD       JUL22 TO EOYQTY22
         ADD       AUG22 TO EOYQTY22
         ADD       SEP22 TO EOYQTY22
         ADD       OCT22 TO EOYQTY22
         ADD       NOV22 TO EOYQTY22
         ADD       DEC22 TO EOYQTY22
.
         ADD       JAN23 TO EOYQTY23
         ADD       FEB23 TO EOYQTY23
         ADD       MAR23 TO EOYQTY23
         ADD       APR23 TO EOYQTY23
         ADD       MAY23 TO EOYQTY23
         ADD       JUN23 TO EOYQTY23
         ADD       JUL23 TO EOYQTY23
         ADD       AUG23 TO EOYQTY23
         ADD       SEP23 TO EOYQTY23
         ADD       OCT23 TO EOYQTY23
         ADD       NOV23 TO EOYQTY23
         ADD       DEC23 TO EOYQTY23
.
         ADD       JAN24 TO EOYQTY24
         ADD       FEB24 TO EOYQTY24
         ADD       MAR24 TO EOYQTY24
         ADD       APR24 TO EOYQTY24
         ADD       MAY24 TO EOYQTY24
         ADD       JUN24 TO EOYQTY24
         ADD       JUL24 TO EOYQTY24
         ADD       AUG24 TO EOYQTY24
         ADD       SEP24 TO EOYQTY24
         ADD       OCT24 TO EOYQTY24
         ADD       NOV24 TO EOYQTY24
         ADD       DEC24 TO EOYQTY24
.
         ADD       JAN25 TO EOYQTY25
         ADD       FEB25 TO EOYQTY25
         ADD       MAR25 TO EOYQTY25
         ADD       APR25 TO EOYQTY25
         ADD       MAY25 TO EOYQTY25
         ADD       JUN25 TO EOYQTY25
         ADD       JUL25 TO EOYQTY25
         ADD       AUG25 TO EOYQTY25
         ADD       SEP25 TO EOYQTY25
         ADD       OCT25 TO EOYQTY25
         ADD       NOV25 TO EOYQTY25
         ADD       DEC25 TO EOYQTY25
.
         WRITAB    OUTPUT,RN;*TAB,"19",HOLDYR,FINORD,EOYQTY:
                   "19",HOLDYR,FINORD1,EOYQTY1,"19",HOLDYR,FINORD2,EOYQTY2:
                   "19",HOLDYR,FINORD3,EOYQTY3,"19",HOLDYR,FINORD4,EOYQTY4:
                   "19",HOLDYR,FINORD5,EOYQTY5,"19",HOLDYR,FINORD6,EOYQTY6:
                   "19",HOLDYR,FINORD7,EOYQTY7,"19",HOLDYR,FINORD8,EOYQTY8:
                   "19",HOLDYR,FINORD9,EOYQTY9,"19",HOLDYR,FINORD10,EOYQTY10:
                   "19",HOLDYR,FINORD11,EOYQTY11,"19",HOLDYR,FINORD12,EOYQTY12:
                   "19",HOLDYR,FINORD13,EOYQTY13,"19",HOLDYR,FINORD14,EOYQTY14:
                   "19",HOLDYR,FINORD15,EOYQTY15,"19",HOLDYR,FINORD16,EOYQTY16:
                   "19",HOLDYR,FINORD17,EOYQTY17,"19",HOLDYR,FINORD18,EOYQTY18:
                   "19",HOLDYR,FINORD19,EOYQTY19,"19",HOLDYR,FINORD20,EOYQTY20:
                   "19",HOLDYR,FINORD21,EOYQTY21,"19",HOLDYR,FINORD22,EOYQTY22:
                   "19",HOLDYR,FINORD23,EOYQTY23,"19",HOLDYR,FINORD24,EOYQTY24:
                   "19",HOLDYR,FINORD25,EOYQTY25
         TRAPCLR   RANGE
         ADD       "1",RN
         MOVE      YR,HOLDYR
ZEROFILL STORE     ZERO INTO INDEX OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT:
                   NOV,DEC:
                   JAN1,FEB1,MAR1,APR1,MAY1,JUN1,JUL1,AUG1:
                   SEP1,OCT1,NOV1,DEC1:
                   JAN2,FEB2,MAR2,APR2,MAY2,JUN2,JUL2,AUG2:
                   SEP2,OCT2,NOV2,DEC2:
                   JAN3,FEB3,MAR3,APR3,MAY3,JUN3,JUL3,AUG3:
                   SEP3,OCT3,NOV3,DEC3:
                   JAN4,FEB4,MAR4,APR4,MAY4,JUN4,JUL4,AUG4:
                   SEP4,OCT4,NOV4,DEC4:
                   JAN5,FEB5,MAR5,APR5,MAY5,JUN5,JUL5,AUG5:
                   SEP5,OCT5,NOV5,DEC5:
                   JAN6,FEB6,MAR6,APR6,MAY6,JUN6,JUL6,AUG6:
                   SEP6,OCT6,NOV6,DEC6:
                   JAN7,FEB7,MAR7,APR7,MAY7,JUN7,JUL7,AUG7,SEP7,OCT7,NOV7,DEC7:
                   JAN8,FEB8,MAR8,APR8,MAY8,JUN8,JUL8,AUG8,SEP8,OCT8,NOV8,DEC8:
                   JAN9,FEB9,MAR9,APR9,MAY9,JUN9,JUL9,AUG9,SEP9,OCT9,NOV9,DEC9:
                   JAN10,FEB10,MAR10,APR10,MAY10,JUN10,JUL10,AUG10:
                   SEP10,OCT10,NOV10,DEC10:
                   JAN11,FEB11,MAR11,APR11,MAY11,JUN11,JUL11,AUG11:
                   SEP11,OCT11,NOV11,DEC11:
                   JAN12,FEB12,MAR12,APR12,MAY12,JUN12,JUL12,AUG12:
                   SEP12,OCT12,NOV12,DEC12:
                   JAN13,FEB13,MAR13,APR13,MAY13,JUN13,JUL13,AUG13:
                   SEP13,OCT13,NOV13,DEC13:
                   JAN14,FEB14,MAR14,APR14,MAY14,JUN14,JUL14,AUG14:
                   SEP14,OCT14,NOV14,DEC14:
                   JAN15,FEB15,MAR15,APR15,MAY15,JUN15,JUL15,AUG15:
                   SEP15,OCT15,NOV15,DEC15:
                   JAN16,FEB16,MAR16,APR16,MAY16,JUN16,JUL16,AUG16:
                   SEP16,OCT16,NOV16,DEC16:
                   JAN18,FEB18,MAR18,APR18,MAY18,JUN18,JUL18,AUG18:
                   SEP18,OCT18,NOV18,DEC18:
                   JAN19,FEB19,MAR19,APR19,MAY19,JUN19,JUL19,AUG19:
                   SEP19,OCT19,NOV19,DEC19:
                   JAN20,FEB20,MAR20,APR20,MAY20,JUN20,JUL20,AUG20:
                   SEP20,OCT20,NOV20,DEC20:
                   JAN21,FEB21,MAR21,APR21,MAY21,JUN21,JUL21,AUG21:
                   SEP21,OCT21,NOV21,DEC21:
                   JAN22,FEB22,MAR22,APR22,MAY22,JUN22,JUL22,AUG22:
                   SEP22,OCT22,NOV22,DEC22:
                   JAN23,FEB23,MAR23,APR23,MAY23,JUN23,JUL23,AUG23:
                   SEP23,OCT23,NOV23,DEC23:
                   JAN24,FEB24,MAR24,APR24,MAY24,JUN24,JUL24,AUG24:
                   SEP24,OCT24,NOV24,DEC24:
                   JAN25,FEB25,MAR25,APR25,MAY25,JUN25,JUL25,AUG25:
                   SEP25,OCT25,NOV25,DEC25
...........
         STORE     ZERO INTO INDEX OF JANX,FEBX,MARX,APRX,MAYX,JUNX,JULX,AUGX:
                   SEPX,OCTX,NOVX,DECX:
                   JANX1,FEBX1,MARX1,APRX1,MAYX1,JUNX1,JULX1,AUGX1:
                   SEPX1,OCTX1,NOVX1,DECX1:
                   JANX2,FEBX2,MARX2,APRX2,MAYX2,JUNX2,JULX2,AUGX2:
                   SEPX2,OCTX2,NOVX2,DECX2:
                   JANX3,FEBX3,MARX3,APRX3,MAYX3,JUNX3,JULX3,AUGX3:
                   SEPX3,OCTX3,NOVX3,DECX3:
                   JANX4,FEBX4,MARX4,APRX4,MAYX4,JUNX4,JULX4,AUGX4:
                   SEPX4,OCTX4,NOVX4,DECX4:
                   JANX5,FEBX5,MARX5,APRX5,MAYX5,JUNX5,JULX5,AUGX5:
                   SEPX5,OCTX5,NOVX5,DECX5:
                   JANX6,FEBX6,MARX6,APRX6,MAYX6,JUNX6,JULX6,AUGX6:
                   SEPX6,OCTX6,NOVX6,DECX6:
                   JANX7,FEBX7,MARX7,APRX7,MAYX7,JUNX7,JULX7,AUGX7:
                   SEPX7,OCTX7,NOVX7,DECX7:
                   JANX8,FEBX8,MARX8,APRX8,MAYX8,JUNX8,JULX8,AUGX8:
                   SEPX8,OCTX8,NOVX8,DECX8:
                   JANX9,FEBX9,MARX9,APRX9,MAYX9,JUNX9,JULX9,AUGX9:
                   SEPX9,OCTX9,NOVX9,DECX9:
                   JANX10,FEBX10,MARX10,APRX10,MAYX10,JUNX10,JULX10,AUGX10:
                   SEPX10,OCTX10,NOVX10,DECX10:
                   JANX11,FEBX11,MARX11,APRX11,MAYX11,JUNX11,JULX11,AUGX11:
                   SEPX11,OCTX11,NOVX11,DECX11:
                   JANX12,FEBX12,MARX12,APRX12,MAYX12,JUNX12,JULX12,AUGX12:
                   SEPX12,OCTX12,NOVX12,DECX12:
                   JANX13,FEBX13,MARX13,APRX13,MAYX13,JUNX13,JULX13,AUGX13:
                   SEPX13,OCTX13,NOVX13,DECX13:
                   JANX14,FEBX14,MARX14,APRX14,MAYX14,JUNX14,JULX14,AUGX14:
                   SEPX14,OCTX14,NOVX14,DECX14:
                   JANX15,FEBX15,MARX15,APRX15,MAYX15,JUNX15,JULX15,AUGX15:
                   SEPX15,OCTX15,NOVX15,DECX15:
                   JANX16,FEBX16,MARX16,APRX16,MAYX16,JUNX16,JULX16,AUGX16:
                   SEPX16,OCTX16,NOVX16,DECX16:
                   JANX17,FEBX17,MARX17,APRX17,MAYX17,JUNX17,JULX17,AUGX17:
                   SEPX17,OCTX17,NOVX17,DECX17:
                   JANX18,FEBX18,MARX18,APRX18,MAYX18,JUNX18,JULX18,AUGX18:
                   SEPX18,OCTX18,NOVX18,DECX18:
                   JANX19,FEBX19,MARX19,APRX19,MAYX19,JUNX19,JULX19,AUGX19:
                   SEPX19,OCTX19,NOVX19,DECX19:
                   JANX20,FEBX20,MARX20,APRX20,MAYX20,JUNX20,JULX20,AUGX20:
                   SEPX20,OCTX20,NOVX20,DECX20:
                   JANX21,FEBX21,MARX21,APRX21,MAYX21,JUNX21,JULX21,AUGX21:
                   SEPX21,OCTX21,NOVX21,DECX21:
                   JANX22,FEBX22,MARX22,APRX22,MAYX22,JUNX22,JULX22,AUGX22:
                   SEPX22,OCTX22,NOVX22,DECX22:
                   JANX23,FEBX23,MARX23,APRX23,MAYX23,JUNX23,JULX23,AUGX23:
                   SEPX23,OCTX23,NOVX23,DECX23:
                   JANX24,FEBX24,MARX24,APRX24,MAYX24,JUNX24,JULX24,AUGX24:
                   SEPX24,OCTX24,NOVX24,DECX24:
                   JANX25,FEBX25,MARX25,APRX25,MAYX25,JUNX25,JULX25,AUGX25:
                   SEPX25,OCTX25,NOVX25,DECX25
         ADD       "1",INDEX
         COMPARE   "324",INDEX
         GOTO      ZEROFILL IF NOT EQUAL
         MOVE      "1",INDEX
         MOVE      "0",FINORD
         MOVE      "0",FINQTY
         MOVE      "0",FINORDA
         MOVE      "0",FINQTYA
         MOVE      "0",FINORDB
         MOVE      "0",FINQTYB
         MOVE      "0",FINORDC
         MOVE      "0",FINQTYC
         MOVE      "01",MNTH
. /////////////////////////////////////////////////////////////////////////////
.         COMPARE   "182",RN     <---ADD '13' IN JAN.
.         COMPARE   "234",RN     <---ADD '13' IN JAN.
.         COMPARE   "247",RN     <---ADD '13' IN JAN.
         COMPARE   "260",RN     <---ADD '13' IN JAN.
. /////////////////////////////////////////////////////////////////////////////
         GOTO      READ1 IF NOT EQUAL
.
.....CLOSES OUT 'SALESMSTDET'
.
         WEOF      OUTPUT,RN
         CLOSE     OUTPUT
. /////////////////////////////////////////////////////////////////////////////
.         MOVE      "143" TO RN    <----ADD '13' IN JAN.
.         MOVE      "195" TO RN    <----ADD '13' IN JAN.
.         MOVE      "208" TO RN    <----ADD '13' IN JAN.
         MOVE      "221" TO RN    <----ADD '13' IN JAN.
. .............................................................................
.RN = YEAR TOTAL RECORD FOR YEAR PRIOR TO THE THREE YEARS IN REPORT.
. .............................................................................
. /////////////////////////////////////////////////////////////////////////////
PRINTOLD CALL      ZERO
         IFNZ      PC
         OPEN      OUTPUT,"SALESMSTDET/TEXT",EXCLUSIVE
         XIF
         IFZ      PC
         OPEN      OUTPUT,"SALESDET",EXCLUSIVE
         XIF
.         PRINT     hp17ptch,hpland,hptop,*F                .compressed
.START PATCH 4.91 REPLACED LOGIC
         PRINT     HPtmsr17,hpland,hptop,*F                .compressed
.                   033,"&l66P":               page length
.                   033,"&l65F":               number lines
.                   *f
.end PATCH 4.91 REPLACED LOGIC
.         PRINT     033,033,"H,L,FR5;",033,046,"l6C",*F
.
......DISPLAYS TIME, DATE, AND HEADINGS;
         MOVE     "01" TO TAB
         MOVE      "00" TO PASS
.
DISP
.begin patch 4.94   Cut and paste 13 records from beginning of the file to end of file \\nins1\e\data\salesmst.dat 
PRT1
         PRINT     *F,*L,*1,"Confidential",*44,TITLE,*91,"Date  ";
         PRINT     MO2,"/",DA,"/",YR2:
                   *N,*50,TITLE2,*91,"Time  ",HRSTRT,":",MINSTRT,":",SECSTRT:
                   *N,*N,*15,"---------","2004","-------------";
         PRINT     *45,"----------------","2005";
         PRINT     "--------------------";
         PRINT     *92,"-----------------","2006";
         PRINT     "---------------------":
                   *N,*2,"MONTH",*15,"ORDERS",*24,"NAMES",*33,"AVER";
         PRINT     *45,"ORDERS",*53,"NAMES",*62,"CHNG",*71,"AVER",*81,"CHNG";
         PRINT     *92,"ORDERS",*100,"NAMES",*109,"CHNG",*118,"AVER";
         PRINT     *130,"CHNG":
                   *N,*62,"YEAR",*82,"MON",*109,"YEAR",*131,"MON",*N
.
. .....PRINTS DATA BY MONTH AND YEAR
.
END2     READTAB   OUTPUT,RN;*TAB,MO,YR,FINORDA,FINQTYA
         MOVE      MO,NUMMO
         BRANCH    NUMMO OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC
.
JAN      MOVE      "January",MONTH
         GOTO      END3
FEB      MOVE      "February",MONTH
         GOTO      END3
MAR      MOVE      "March",MONTH
         GOTO      END3
APR      MOVE      "April",MONTH
         GOTO      END3
MAY      MOVE      "May",MONTH
         GOTO      END3
JUN      MOVE      "June",MONTH
         GOTO      END3
JUL      MOVE      "July",MONTH
         GOTO      END3
AUG      MOVE      "August",MONTH
         GOTO      END3
SEP      MOVE      "September",MONTH
         GOTO      END3
OCT      MOVE      "October",MONTH
         GOTO      END3
NOV      MOVE      "November",MONTH
         GOTO      END3
DEC      MOVE      "December",MONTH
.
END3     ADD       "13",RN
         MOVE      " " TO LASTMOSW
         READTAB   OUTPUT,RN;*TAB,MO,YR,FINORDB,FINQTYB
.         MOVE      TOTQTY TO FINQTYB
         MOVE      MO2,NUMMO
         COMPARE   NUMMO,MO
         GOTO      END3B IF EQUAL
         GOTO      END4 IF NOT LESS
         MOVE      FINQTYB,SAVE
         ADD       SAVE,FINALSAV
         GOTO      END4
END3B    MOVE      FINQTYB,SAVE
         DIV       "31",SAVE
         MOVE      DA2,NUMDA
         MULT      NUMDA,SAVE
         ADD       SAVE,FINALSAV
* ***************************************
         MOVE      SAVE TO FINQTYB1
         MOVE      "Y" TO LASTMOSW
* *****************
*PRINTS  MONTHLY   TOTALS.
*****************************
END4     ADD       "13",RN
         READTAB   OUTPUT,RN;*TAB,MO,YR,FINORDC,FINQTYC
         CALL      PERCENT
        PRINT     *N,*1,MONTH,*15,FINORDA,*22,FINQTYA,*33,AVERORDA,*45,FINORDB;
         PRINT     *51,FINQTYB,*62,PCTB,"%",*71,AVERORDB," ",PCTMB,"%";
         PRINT     *92,FINORDC;
         PRINT     *98,FINQTYC,*108,PCTC,"%",*118,AVERORDC," ",PCTMC,"%"
END4A
         SUB       "25",RN     .****DO NOT CHANGE.<<<<<<<<<<<<<<<<<<<<<<<<<<<<
. /////////////////////////////////////////////////////////////////////////////
.         COMPARE   "155",RN     .*ADD '13' EVERY JANUARY
.         COMPARE   "207",RN     .*ADD '13' EVERY JANUARY
.         COMPARE   "220",RN     .*ADD '13' EVERY JANUARY
         COMPARE   "233",RN     .*ADD '13' EVERY JANUARY
. /////////////////////////////////////////////////////////////////////////////
         GOTO      END2 IF NOT EQUAL
.
. .....PRINTS YEARLY TOTALS
.
         READTAB   OUTPUT,RN;*TAB,F2,YR,FINORDA,FINQTYA
         ADD       "13",RN
         READTAB   OUTPUT,RN;*TAB,F2,YR,FINORDB,FINQTYB
         ADD       "13",RN
         MOVE     "A",OK
         READTAB   OUTPUT,RN;*TAB,F2,YR,FINORDC,FINQTYC
         CALL      PERCENT
         CALL      TIME
         PRINT     *N,*15,"_____",*22,"_________",*33,"_________";
         PRINT     *45,"_____",*51,"_________",*62,"_____",*71,"_________";
         PRINT     *92,"_____",*98,"_________",*108,"_____",*118,"_________"
         MOVE      "TOTALS" TO MONTH
        PRINT     *N,*1,MONTH,*15,FINORDA,*22,FINQTYA,*33,AVERORDA,*45,FINORDB;
         PRINT     *51,FINQTYB,*62,PCTB,"%",*71,AVERORDB,*92,FINORDC;
         PRINT     *98,FINQTYC,*108,PCTC,"%",*118,AVERORDC
         PRINT     *L,*L,*L,*L
         PRINT     "*** THIS REPORT DOES NOT INCLUDE CANCELLED ORDERS ***";
         PRINT       B4,B3,B7,"End Time",B1,HR,":",MIN," ",SEC
.         DISPLAY   014;
. .." " IS MOVED TO FINAL AND LOADED TABLES ARE PRINTED OUT DURING NEXT CYCLE
OUTPUT
.         MOVE      "01",TAB
         MOVE      "NAMES IN THE NEWS",TITLE
         ADD       "1" TO PASS
         LOAD      TITLE2 FROM PASS OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5:
                   OSLS6,OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13:
                   OSLS14,OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21:
                   OSLS22,osls23,osls24,osls25:
		osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
.                   OSLS14,OSLS15,OSLS16,OSLS17,OSLS19,OSLS20,OSLS21,OSLS22:
.                   OSLS23,OSLS24,OSLS25
         display   *p10:20,title2
         ADD       "18" TO TAB
         CLOSE     OUTPUT
         COMPARE   "23" TO PASS
.         CMATCH    " ",FINAL
         GOTO      STOP IF EQUAL
         IFNZ      PC
         OPEN      OUTPUT,"SALESMSTDET/TEXT",EXCLUSIVE
         XIF
         IFZ      PC
         OPEN      OUTPUT,"SALESDET",EXCLUSIVE
         XIF
. //////////////////////////////////////////////////////**********.211248 FIRST LR 1994 **************
.         MOVE      "143",RN      .ADD '13' EVERY JANUARY.
.         MOVE      "195",RN      .ADD '13' EVERY JANUARY.
.         MOVE      "208",RN      .ADD '13' EVERY JANUARY.
         MOVE      "221",RN      .ADD '13' EVERY JANUARY.
. //////////////////////////////////////////////////////
         MOVE      " ",FINAL
         MOVE      "0",FINALSAV
         MOVE      "0",SAVE
         GOTO      DISP
.
. .....ZERO FILLS TOTAL FIELDS
.
ZERO     MOVE      ZERO TO TOTORD
         MOVE      ZERO TO COUNT1
         MOVE      ZERO TO COUNT2
         MOVE      ZERO TO COUNT3
         MOVE      ZERO TO TOTQTY
         MOVE      ZERO TO FINORD
         MOVE      ZERO TO FINQTY
         MOVE      ZERO TO ENDQTY
         MOVE      ZERO TO FINORDA
         MOVE      ZERO TO FINORDB
         MOVE      ZERO TO FINORDC
         MOVE      ZERO TO FINQTYA
         MOVE      ZERO TO FINQTYB
         MOVE      ZERO TO FINQTYC
         MOVE      ZERO TO TOTORD1
         MOVE      ZERO TO TOTQTY1
         MOVE      ZERO TO TOTORD2
         MOVE      ZERO TO TOTORD3
         MOVE      ZERO TO TOTORD4
         MOVE      ZERO TO TOTORD5
         MOVE      ZERO TO TOTORD6
         MOVE      ZERO TO TOTORD7
         MOVE      ZERO TO TOTORD8
         MOVE      ZERO TO TOTORD9
         MOVE      ZERO TO TOTORD10
         MOVE      ZERO TO TOTORD11
         MOVE      ZERO TO TOTORD12
         MOVE      ZERO TO TOTORD13
         MOVE      ZERO TO TOTORD14
         MOVE      ZERO TO TOTORD15
         MOVE      ZERO TO TOTORD16
         MOVE      ZERO TO TOTORD17
         MOVE      ZERO TO TOTORD18
         MOVE      ZERO TO TOTORD19
         MOVE      ZERO TO TOTORD20
         MOVE      ZERO TO TOTORD21
         MOVE      ZERO TO TOTORD22
         MOVE      ZERO TO TOTORD23
         MOVE      ZERO TO TOTORD24
         MOVE      ZERO TO TOTORD25
..
         MOVE      ZERO TO TOTQTY2
         MOVE      ZERO,TOTQTY3
         MOVE      ZERO,TOTQTY4
         MOVE      ZERO TO SAVE
         MOVE      ZERO,TOTQTY5
         MOVE      ZERO,TOTQTY6
         MOVE      ZERO TO TOTQTY7
         MOVE      ZERO TO TOTQTY8
         MOVE      ZERO TO TOTQTY9
         MOVE      ZERO TO TOTQTY10
         MOVE      ZERO TO TOTQTY11
         MOVE      ZERO TO TOTQTY12
         MOVE      ZERO TO TOTQTY13
         MOVE      ZERO TO TOTQTY14
         MOVE      ZERO TO TOTQTY15
         MOVE      ZERO TO TOTQTY16
         MOVE      ZERO TO TOTQTY17
         MOVE      ZERO TO TOTQTY18
         MOVE      ZERO TO TOTQTY19
         MOVE      ZERO TO TOTQTY20
         MOVE      ZERO TO TOTQTY21
         MOVE      ZERO TO TOTQTY22
         MOVE      ZERO TO TOTQTY23
         MOVE      ZERO TO TOTQTY24
         MOVE      ZERO TO TOTQTY25
         RETURN
.
. .....READS TIME AND DATE FROM 'ARCCLOCK'
.
TIME     CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MO1,DA1,YR1
         XIF
         IFZ      PC
         UNPACK    DATE INTO MO1,STR1,DA1,STR1,YR1
         XIF
         MOVE      MO1 TO MO
         MOVE      DA1 TO DA
         MOVE      YR1 TO YR
         move      yr1 to yr2
         move      yr2 to yrchk
         CLOCK     TIME TO TIME
         UNPACK    TIME INTO HR,F1,MIN,F1,SEC
         MOVE      HR,NUMHR
         COMPARE   "12",NUMHR
         GOTO      TIME4 IF EQUAL
         GOTO      TIME3 IF LESS
         MOVE      "PM",SEC
         SUB       "12",NUMHR
         MOVE      NUMHR,HR
         RETURN
TIME3
         MOVE      "AM",SEC
         RETURN
TIME4
         MOVE      "PM",SEC
         RETURN
.
. .....FIGURES PERCENTAGE CHANGE
.
PERCENT  MOVE      ZERO TO FINQTY
         MOVE      ZERO TO ENDQTY
         MOVE      FINQTYA,FINQTY
         MOVE      FINQTYB,ENDQTY
         COMPARE   FINQTY,ENDQTY
         GOTO      PERCENT2 IF NOT LESS
         MOVE      ENDQTY,WORK
         DIV       FINQTY,WORK
         GOTO      ZEROB IF OVER
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MULT      "-1",NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTB
         APPEND    "-",PCTB
         APPEND    PCT,PCTB
         RESET     PCTB
         GOTO      PERCENT3
PERCENT2 MOVE      ENDQTY TO WORK
         DIV       FINQTY INTO WORK
         GOTO      ZEROB IF OVER
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTB
         APPEND    "+",PCTB
         APPEND    PCT,PCTB
         RESET     PCTB
         GOTO      PERCENT3
ZEROB    MOVE      "   0%" TO PCTB
         GOTO      PERCENT3
PERCENT3 CMATCH    "A",OK
         GOTO      PERCENT4 IF NOT EQUAL
         MOVE      FINALSAV,FINQTY
         MOVE      " ",OK
         GOTO      PERCENT5
PERCENT4
         CMATCH    "Y" TO LASTMOSW
         GOTO      PERCENTX IF NOT EQUAL
         MOVE      FINQTYB1 TO FINQTY
         MOVE      " " TO LASTMOSW
         GOTO      PERCENTY
PERCENTX
         MOVE      FINQTYB,FINQTY
PERCENTY
         MATCH     "        0",FINQTYC
         GOTO      PERCENT7 IF EQUAL
PERCENT5 MOVE      "0" TO ENDQTY
         MOVE      FINQTYC,ENDQTY
         MATCH     "0",FINQTYC
         GOTO      PERCENT7 IF EQUAL
         COMPARE   FINQTY,ENDQTY
         GOTO      PERCENT6 IF NOT LESS
         MOVE      ENDQTY,WORK
         DIV       FINQTY,WORK
         GOTO      ZEROC IF OVER
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MULT      "-1",NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTC
         APPEND    "-",PCTC
         APPEND    PCT,PCTC
         RESET     PCTC
.         RETURN
         GOTO      CALCAVER
PERCENT6
         MOVE      ENDQTY TO WORK
         DIV       FINQTY INTO WORK
         GOTO      ZEROC IF OVER
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTC
         APPEND    "+",PCTC
         APPEND    PCT,PCTC
         RESET     PCTC
.         RETURN
         GOTO      CALCAVER
ZEROC    MOVE      "   0%" TO PCTC
         GOTO      CALCAVER
PERCENT7 CLEAR     PCTC
         MOVE      "   " TO PCTC
         REP       "0 ",FINQTYC
         REP       "0 ",FINORDC
.         RETURN
CALCAVER MOVE      "0" TO AVERORDA
         MOVE      "0" TO AVERORDB
         MOVE      "0" TO AVERORDC
         MOVE      FINQTYA TO AVERORDA
         MOVE      FINQTYB TO AVERORDB
         MOVE      FINQTYC TO AVERORDC
         MOVE      FINORDA TO ORDS
         DIV       ORDS INTO AVERORDA
         CALL      CLEARA  IF OVER
         MOVE      FINORDB TO ORDS
         DIV       ORDS INTO AVERORDB
         CALL      CLEARB IF OVER
         MOVE      FINORDC TO ORDS
         DIV       ORDS INTO AVERORDC
         CALL      CLEARC IF OVER
         CALL      PERMON         *CALC CHANGE MONTH TO MONTH
         RETURN
CLEARA   MOVE      "0" TO AVERORDA
         RETURN
CLEARB   MOVE      "0" TO AVERORDB
         RETURN
CLEARC   MOVE      "0" TO AVERORDC
         RETURN
.
PERMON   MOVE      MO TO NUMMO
         COMPARE   "01" TO NUMMO     *JAN??
         CALL      GETDEC IF EQUAL      *YES, GO GET DEC FIGURES
         MOVE      ZERO TO FINQTY
         MOVE      ZERO TO ENDQTY
         MOVE      PREQTYB,FINQTY
         MOVE      FINQTYB,ENDQTY
.         DISPLAY   *P1:22,*EL,PREQTYB," ",FINQTY,"  ",FINQTYB," ",ENDQTY
         COMPARE   FINQTY,ENDQTY
         GOTO      PERMON2 IF NOT LESS
         MOVE      ENDQTY,WORK
         DIV       FINQTY,WORK
         GOTO      ZERB IF OVER
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MULT      "-1",NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTMB
         APPEND    "-",PCTMB
         APPEND    PCT,PCTMB
         RESET     PCTMB
         GOTO      PERMON3
PERMON2  MOVE      ENDQTY TO WORK
         DIV       FINQTY INTO WORK
         GOTO      ZERB IF OVER
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTMB
         APPEND    "+",PCTMB
         APPEND    PCT,PCTMB
         RESET     PCTMB
         GOTO      PERMON3
ZERB     MOVE      "   0%" TO PCTMB
         GOTO      PERMON3
PERMON3  MATCH     "        0" TO FINQTYC
         GOTO      PERMON7 IF EQUAL
         SUB       FINQTY FROM FINQTY
         MOVE      PREQTYC TO FINQTY
         MOVE      "0" TO ENDQTY
         MOVE      FINQTYC,ENDQTY
.         KEYIN     *P1:23,*EL,*DV,PREQTYC," ",*DV,FINQTY,"  ",*DV,FINQTYC:
.                   " ",*DV,ENDQTY,ANS
         MATCH     "0",FINQTYC
         GOTO      PERMON7 IF EQUAL
         COMPARE   FINQTY,ENDQTY
         GOTO      PERMON6 IF NOT LESS
         MOVE      ENDQTY,WORK
         DIV       FINQTY,WORK
         GOTO      ZERC IF OVER
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MULT      "-1",NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTMC
         APPEND    "-",PCTMC
         APPEND    PCT,PCTMC
         RESET     PCTMC
         GOTO      PERMONX
PERMON6
         MOVE      ENDQTY TO WORK
         DIV       FINQTY INTO WORK
         GOTO      ZERC IF OVER
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTMC
         APPEND    "+",PCTMC
         APPEND    PCT,PCTMC
         RESET     PCTMC
         GOTO      PERMONX
ZERC     MOVE      "   0%" TO PCTMC
         GOTO      PERMONX
PERMON7  CLEAR     PCTMC
         MOVE      "   " TO PCTMC
PERMONX  MOVE      "0" TO PREQTYB
         MOVE      "0" TO PREQTYC
         MOVE      FINQTYB TO PREQTYB
         MOVE      FINQTYC TO PREQTYC
         RETURN
GETDEC
         SUB       "2" FROM RN
         ADD       "9" TO TAB
         READTAB   OUTPUT,RN;*TAB,PREQTYC
         SUB       "13" FROM RN
         READTAB   OUTPUT,RN;*TAB,PREQTYB
         ADD       "15" TO RN
         SUB       "9" FROM TAB
         RETURN
. .....SCREEN DISPLAY;
.
DISPLAY  DISPLAY   *ES,*P31:24,"***** SALESPRT DETAIL  *****",*R,*R:
               *P12:24,"NAMES IN THE NEWS",*P40:24,"|";
DISPLAY2 DISPLAY   *R,*P40:24,"|";
         ADD       "1",COUNT
         COMPARE   "21",COUNT
         GOTO      DISPLAY2 IF NOT EQUAL
DISPLAY3 DISPLAY   *PV:6,"RECORDS READ: ",*PV:9,"CURRENT DATE BEING READ: ";
         COMPARE   "42",V
         RETURN    IF EQUAL
         ADD       "41",V
         GOTO      DISPLAY3
FORMAT   KEYIN     *P1:24,*EL,",FORMAT ERROR ",rn,b1,ERROR," TRY TO CONTINUE":
                   ANS
         CMATCH    "N" TO ANS
         GOTO      STOP IF EQUAL
         TRAPCLR   FORMAT
         TRAP      FORMAT GIVING ERROR IF FORMAT
         RETURN
STOP     PRINT     *F,033,033,"H,P,FR1;",033,046,"l8C",*FLUSH
         SPLCLOSE
         shutdown
         STOP
ERR      DISPLAY   *P1:1,*ES,"RANGE ERROR";
         KEYIN     *P22:1,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      STOP IF EQUAL
         GOTO      ERR
         INCLUDE   NORDIO.inc
         INCLUDE   COMLOGIC.inc

