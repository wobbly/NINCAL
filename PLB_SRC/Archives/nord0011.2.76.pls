...............................................................................
.'Nord0011' READS BOTH ORDER FILES STARTING AT THE FIRST LR NUMBER OF 19XX. IT.
.THEN PRINTS THE TOTAL NUMBER OF ORDERS AND THEIR QUANTITIES BY MONTH AND YEAR.
.by teamON A SERIAL PRINTER.                                                         .
...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         include   hp.inc
         INC       OSLSTEAM.inc
RELEASE  init      "2.76"              JDJan162006 2007 changes
.RELEASE  init      "2.75"              JDJan092006 2006 changes.
.RELEASE  init      "2.74"              JD Oct04 2005 CB#15 now AL team.
.RELEASE  init      "2.73"              JD AUG31 2005 MM now AL team, PF now DC team.
.RELEASE  init      "2.72"              JD 13Jan05 2005 run.
.RELEASE  init      "2.71"             JD 30Jan04 2004 run.
.RELEASE  init      "2.71"             JD 30Jan04 2004 run.
.RELEASE  init      "2.71"             JD 30Jan04 2004 run.
.RELEASE  init      "2.72"             JD  04May04 changed font.
.RELEASE  init      "2.71"             JD 30Jan04 2004 run.
.RELEASE  init      "2.70"             JD 30Jan03 for 2003.
.RELEASE  init      "2.61"            DB 081701 Added Ann lovi(Sales 4 to JC)to sales
.
.RELEASE  init      "2.6"            ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  init      "2.51"            JD11Sep00 IF sls "13" and 8/2000 forward add to SMM team.
.RELEASE  init      "2.5"            DLH  8Sep00 check for only ostat of "O" or "B"
.release   init      "2.4"           JD 31Jan00 for 2000.
.release   init      "2.3"         ASH 29Dec98 NINORD Y2K, File expansion
.release   init      "2.2"            JD 29Jan99 skip pending orders.
.release   init      "2.1"           JD 25Jan99 for 99.
.release   init      "2.0"          JD 23Jan98 for 98.
.release  init      "1.42"         29apr97 JD chnged #12 to NP's team. (CD)
.release  init      "1.41"        06jan97 DLH for 97
.RELEASE  INIT      "1.4"         21APR92 DLH EXPANDED TO 10 TEAMS, CHANGED 
.                                OUTPUT FILE NAME.
.
.RELEASE  INIT      "1.3"        DLH  SEP91 ADDED CONS,COMLOGIC,CHANGED SALES
.                               BRANCH
.RELEASE  INIT      "1.2"        DLH 11SEP89     EXPANDED TO HANDLE EIGHT TEAMS
.
.RELEASE  INIT      "1.0"       DLH 08AUG88    CREATED.
.
. .FILES DECLARATIONS
.OUTPUT   FILE      FIXED=348    PHYSICAL OUTPUT FILE 'SALESMANMST'
.OUTPUT   FILE      FIXED=380    PHYSICAL OUTPUT FILE 'SALESMST'
OUTPUT   FILE      FIXED=198    PHYSICAL OUTPUT FILE 'SALESMST'
. ....
RN       FORM      "013"      RECORD NUMBER
.
. .ORDER FILE LAYOUT
.
LR       DIM       6         POS 7-12   KEY
.Start patch #2.3 - increase var to reflect OQTY expansion
.QUANT    FORM      7         POS 33-39  ORDER QUANTITY
QUANT    FORM      9         POS 33-39  ORDER QUANTITY
.End patch #2.3 - increase var to reflect OQTY expansion
.
DATE     DIM       8
TIME     DIM       8
.START PATCH #2.3 - VAR NOW FOUND IN CONS.INC
.F1       DIM       1         POS 1-1    FILLER
.STARTEND PATCH #2.3 - VAR NOW FOUND IN CONS.INC
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
BRMO     FORM      3         BRANCH/INDEX VARIABLE FROM MO +.
FINAL    DIM       1         CHECKS END OF PROGRAM
COUNT1   FORM      5         COUNTS ORDER RECORDS
COUNT2   FORM      5         OUTPUT MONTH COUNT
COUNT3   FORM      5         OUTPUT YEAR COUNT
ANS      DIM       1
WORK     FORM      9.4       WORK SPACE
KEY      DIM       6         FOR FIRST ORDER READ
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
TOTORD10 FORM      5         TOTAL ORDERS
TOTQTY10 FORM      9         TOTAL NAMES

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
TITLE    INIT      "COMPUNAME        "        HEADING (NIN OR CMP)
TITLE2   INIT      "NO SALESPERSON   "
NUMHR    FORM      2         DATE WORK SPACE
INDEX    FORM      "001"
MONTH    DIM       9         MONTH DESCRIPTION ON REPORT
HOLDYR   FORM      2         HOLDS YEAR
NUMMO    FORM      2         INDEX FOR BRANCH
TAB      FORM      "001"      TAB POSITIONS
F2       DIM       2         FILLER
.V        FORM      "01"      SPLIT SCREEN POSITION
V1       FORM      "15"      SPLIT SCREEN POSITION
V2       FORM      "26"      SPLIT SCREEN POSITION
PASS     FORM      2
. ***************CHANGE FIRST' ' NUMBERS ever jan EVERY YEAR********************
**********.045801 FIRST LR OCT 1987.**********
**********.051937 FIRST LR 1988    ***********
**********.078953 FIRST LR 1989. *************
**********.106639 FIRST LR 1990. *************
**********.135371 FIRST LR 1991. *************
**********.158020 FIRST LR 1992. *************
**********.185570 FIRST LR 1993. *************
**********.211248 FIRST LR 1994. *************
**********.234738 FIRST LR 1995. *************
**********.257868 FIRST LR 1996. *************
**********.280833 FIRST LR 1997. *************
**********.305233 FIRST LR 1998. *************
**********.330753 FIRST NIN LR OF 1999
**********.362964 FIRST NIN LR OF 2000
.**********.350000 FIRST NIN LR OF 2001
* *****************************************************************************
.FIRSTN   INIT      "470000"  FIRST NIN LR OF 2002
FIRSTN   INIT      "500000"  FIRST NIN LR OF 2002
FIRSTC   INIT      "020693"  FIRST CMP LR OF 1982
PLUS     FORM      3
TWELVE   FORM      "12"
TWENTY4  FORM      "24"
THIRTY6  FORM      "36"
FORTY8   FORM      "48"
SIXTY    FORM      "60"
SEVENTY2 FORM      "72"
EIGHTY4  FORM      "84"
NINETY6  FORM      "96"
HUNDRD8  FORM      "108"
HUNDRD20 FORM      "120"
YR2      form      2         HOLDS YEAR
DA2      DIM       2         HOLDS DAY
MO2      DIM       2         HOLDS MONTH
yrchk    form      2
HRSTRT   DIM       2         HOLDS STARTING HOUR
MINSTRT  DIM       2         HOLDS STARTING MIN
SECSTRT  DIM       2         HOLDS STARTING SEC
COUNT    FORM      2
MNTH    FORM      "01"
CHECK    DIM       1
JANX     FORM      9
FEBX     FORM      9
MARX     FORM      9
APRX     FORM      9
MAYX     FORM      9
JUNX     FORM      9
JULX     FORM      9
AUGX     FORM      9
SEPX     FORM      9
OCTX     FORM      9
NOVX     FORM      9
DECX     FORM      9
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
NOVX3    FORM      9
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
QTY      FORM      10
OK       DIM       1
FINALSAV FORM      9
SAVE     FORM      9
NUMDA    FORM      2
SPOOL    DIM       1
LASTMOSW DIM       1            HOLDS 'Y' IF LAST MONTH OF CURRENT YEAR.
SALESBR  FORM      2            HOLDS SALESMAN NUMBER FOR BRANCH
SALESNUM DIM       2            USED TO BUILD SALESMAN NUMBER
TEAM1    INIT      "01"         "JEANETTE"
TEAM2    INIT      "02"         "DELYNNE "
TEAM3    INIT      "03"         "SUSAN "
TEAM4    INIT      "04"         "  "
TEAM5    INIT      "05"         "  "
TEAM6    INIT      "06"         "LIST MANAGEMENT"
.TEAM7    INIT      "07"          SUZIE
TEAM7    INIT      "07"          ANN L.
TEAM8    INIT      "08"           COLD CALLS
TEAM9    INIT      "09"          
TEAM10   INIT      "10"          
.
.
.
         MOVE      "NORD0011" TO PROGRAM
         MOVE      "ORDERS BY SALES TEAM" TO STITLE
         MOVE      "NINCAL" TO COMPNME
         move      c1 to v
         CALL      PAINT
         KEYIN     *P15:10,"DO YOU WANT THE OUTPUT SPOOLED ? ":
                   *T20,SPOOL;
         CMATCH    NO,SPOOL
         GOTO      BEGIN IF EQUAL
         IFNZ      PC
         SPLOPEN   "SALESPRT/PRT:PRINT"
         XIF
         IFZ       PC
.START PATCH 2.6 REPLACED LOGIC
.         SPLOPEN   "g:\DATA\SALESPRT.LST"
         PACK      STR35,NTWKPATH1,"SALESPRT.LST"
         SPLOPEN   STR35
.END PATCH 2.6 REPLACED LOGIC
         XIF
.....INITIAL TIME ANS SCREEN DISPLAY;
.
BEGIN
.         PRINT     *F,033,033,"H,P,FR3;",033,046,"l8C",*FLUSH
.         print     hp17ptch,hptop,*f
.START PATCH 2.72 REPLACED LOGIC
         PRINT     HPtmsr17,hptop:                .compressed
                   033,"&l66P":               page length
                   033,"&l65F":               number lines
                   *f
.ed PATCH 2.72 REPLACED LOGIC
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
         TRAP      START IF IO
         CALL      ZEROFILL
         MOVE      "NAMES IN THE NEWS",TITLE
         KEYIN     *P1:24,*EF,*P15:24,"PRINT ONLY ?",*T5,ANS;
         CMATCH    YES TO ANS
         GOTO      PRINTOLD IF EQUAL
.
. .....CHECKS TO SEE IF 'SALESMANMST' EXISTS
.
.        OPEN      OUTPUT,"SALESMANMST/TEXT",EXCLUSIVE
         OPEN      OUTPUT,"SALESMST",EXCLUSIVE
         TRAPCLR   IO
         TRAP      FORMAT GIVING ERROR IF FORMAT
.         OPEN      NINORD,"NINORD",SHARE
         MOVE      "Q",CHECK
         MOVE      C1 TO NORDPATH
         MOVE      FIRSTN TO NORDFLD
         REP       " 0" IN NORDFLD
         CALL      NORDKEY
         MOVE      "NAMES IN THE NEWS",TITLE
         MOVE      C0 TO QUANT
         MOVE      OQTY TO QUANT
. /////////////////////////////////////////////////////////////////////////////
.         MOVE      "169",RN     <---ADD '13' ever jan.
         MOVE      "221",RN     <---ADD '13' ever jan. 2000
. /////////////////////////////////////////////////////////////////////////////
         MOVE      OODTEY,YR
         MOVE      YR,HOLDYR
         MOVE      OODTEM TO MO
         GOTO      READ2
CHECK    CLOSE     NORDFILE
         MOVE      C0 TO NORDFLAG
         CMATCH    "Q",CHECK
         GOTO      CMP IF NOT EQUAL
         OPEN      NORDFILE,"CMPORD",SHARE
         MOVE      C1 TO NORDFLAG
         READ      NORDFILE,FIRSTC;ORCODE,OSTAT
         MOVE      "COMPUNAME",TITLE
. /////////////////////////////////////////////////////////////////////////////
.         MOVE      "169",RN     <---ADD '13' ever jan.
         MOVE      "221",RN     <---ADD '13' ever jan.
. /////////////////////////////////////////////////////////////////////////////
         MOVE      "X",FINAL
         ADD       "42",V
         ADD       "42",V1
         ADD       "42",V2
         MOVE      "01",TAB
         CALL      ZERO
.
         OPEN      OUTPUT,"SALESMST",EXCLUSIVE
         MOVE      YR,HOLDYR
         GOTO      READ2
.
......OPENS FILES
.
START    TRAPCLR   IO
         NORETURN
.         OPEN      NINORD,"NINORD",SHARE
         MOVE      "NAMES IN THE NEWS",TITLE
         IFNZ      PC
         PREPARE   OUTPUT,"SALESMST:TEXT"
         XIF
         IFZ       PC
.START PATCH 2.6 REPLACED LOGIC
.         PREPARE   OUTPUT,"g:\DATA\TEXT\SALESMST"
         PACK      STR35,NTWKPATH1,"TEXT\SALESMST"
         PREPARE   OUTPUT,STR35
.END PATCH 2.6 REPLACED LOGIC
         XIF
         GOTO      SCRN1
.
CMP      OPEN      NORDFILE,"CMPORD",SHARE
         MOVE      C1 TO NORDFLAG
         MOVE      "COMPUNAME",TITLE
         MOVE      "X",FINAL
         OPEN      OUTPUT,"SALESMST",EXCLUSIVE
.         MOVE      "19",TAB
         MOVE      "01",TAB
         CALL      ZERO
SCRN1    CMATCH    "X",FINAL
         GOTO      READ1 IF NOT EQUAL
         ADD       "42",V
         ADD       "42",V1
         ADD       "42",V2
.
......READS ORDER FILE AND CREATES 'SALESMANMST'
.
READ1    CALL      NORDKS
         GOTO      NEWYEAR IF OVER
         MOVE      OODTEM TO MO
         MOVE      C0 TO QUANT
         MOVE      OQTY TO QUANT
         MOVE      OODTEY TO YR
         MOVE      OODTEY,HOLDYR
         ADD       C1,COUNT1
.Start patch #2.3 - remmed and replaced logic
.         DISPLAY   *PV1:6,COUNT1,*P24:9,OODTEM,"/",OODTED,"/",OODTEY:
.                   B1,OLRN
         DISPLAY   *PV1:6,COUNT1,*P24:9,OODTEM,"/",OODTED,"/",OODTEC,OODTEY:
                   B1,OLRN
.End patch #2.3 - remmed and replaced logic
         COMPARE   C1,COUNT1
         GOTO      READ2 IF NOT EQUAL
         MOVE      OODTEY,HOLDYR
.begin patch 2.5
READ2
          if        (OSTAT <> "0" and OSTAT <> "B")
          goto      read1
          endif
.READ2    CMATCH    "X",OSTAT
.         GOTO      READ1 IF EQUAL        .SKIP CANCELLED ORDERS.
.         CMATCH    "Q",OSTAT
.         GOTO      READ1 IF EQUAL        .SKIP CANCELLED ORDERS.
..begin patch 2.2
.         CMATCH    "p" TO OSTAT       Pending order ?
.         GOTO      read1 IF EQUAL     YES, skip.
.         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
.         GOTO      read1 IF EQUAL     YES, skip.
..note cancodes also updated to skip cancelled pending orders.
..end patch 2.2
..begin patch 2.3
.         CMATCH    "l" TO OSTAT       lcr order ?
.         GOTO      read1 IF EQUAL     YES, skip.
.         CMATCH    "z" TO OSTAT       Cancelled lcr order ?
.         GOTO      read1 IF EQUAL     YES, skip.
..note cancodes also updated to skip cancelled pending orders.
..end patch 2.3
.end patch 2.5
         COMPARE   C0 TO QUANT
         GOTO      READ1 IF EQUAL        .SKIP DUMMY ORDERS.
         COMPARE   YRchk TO HOLDYR
.         GOTO      NEWYEAR IF NOT EQUAL
         GOTO      READ3 IF EQUAL
.         GOTO      NEWYEAR IF LESS
         GOTO      READ1
READ3    MOVE      C0 TO SALESBR
         PACK      SALESNUM FROM OSALES10,OSALES
         MOVE      SALESNUM TO SALESBR
...............................................................................
.NOTE THIS TABLE NEEDS TO BE ADJUSTED WHEN EVER SALES PERSONNEL CHANGES.
...............................................................................
.CONVERT SALESPERSONS TO SALES TEAMS.
.         COMPARE   C13 TO SALESBR
.         GOTO      READ1 IF EQUAL
.         COMPARE   C0 TO SALESBR
.         GOTO      PLUS IF EQUAL
.        COMPARE    "04" TO  SALESBR         turned on 11/26/03.
.         GOTO      LOADOK IF NOT EQUAL
.         MOVE      C0 TO N2
.         MOVE      OODTEM TO N2
.         COMPARE   N2 TO c10
.         IF        LESS
.         MOVE      C10 TO SALESBR
.         MOVE      C10 TO SALESNUM
.         goto      PLUS
.         else
.         MOVE      C4 TO SALESBR
.         MOVE      C4 TO SALESNUM
.         GOTO      PLUS
.         ENDIF
.
.
.
.                                       3   5  3  7  2  6    3  1  8
.   LOAD      SALESNUM FROM SALESBR OF SAD,BO,SA,AL,PF,LSTM,SK,JC,COLD
.                                       1    2  3  4  5  6   7   8  9
.
.         TEAM# -   7  1  2  7  7   7   2   4   1  6    5  3  3
.                   MM,BS,DC,BC,SMM,CY,KS, MF,??,LSTM,??,??,MG
.         SALES# -  10 11 12 13 14  15  16  17 18 19   20 21 22
.TEAM1 JEANETTE
.TEAM2 DELYNNE
.TEAM3 SUSAN A
.TEAM4 
.TEAM5 BONNIE
.TEAM6 LIST MAN.
.TEAM7 ANN L
.TEAM8 COLD CALLS
.TEAM9 
.TEAM10 
.
LOADOK   LOAD      SALESNUM FROM SALESBR OF TEAM3:     1 SAD-SA
                                            TEAM5:     2 BLO-BLO
                                            TEAM3:     3 SA-SA
                                            TEAM7:     4 AL-SM
                                            TEAM2:     5 PF-PF
                                            TEAM6:     6 LIST/MAN
                                            TEAM3:     7 SK-SA
                                            TEAM1:     8 JC-JC
                                            TEAM8:     9 COLD CALLS
                                            TEAM7:    10 MM-SMM
                                            TEAM1:    11 BS-JC
                                            TEAM2:    12 Delynne
                                            TEAM7:    13 BC-BC
                                            TEAM7:    14 SMM-SMM
                                            TEAM7:    15 CP-JC
                                            TEAM3:    16 KS-SA
                                            TEAM7:    17 mf-SMM
                                            TEAM1:    18 BLANK-JC
                                            TEAM6:    19 BLANK-LIST MAN
                                            TEAM5:    20 BLANK-BLO
                                            TEAM3:    21 BLANK-SA
                                            TEAM3    .22 MG-SA
         MOVE      SALESNUM TO SALESBR
...............................................................................
PLUS     MOVE      "0" TO PLUS
         LOAD      PLUS FROM SALESBR OF TWELVE,TWENTY4,THIRTY6,FORTY8,SIXTY:
                   SEVENTY2,EIGHTY4,NINETY6,HUNDRD8,HUNDRD20
         MOVE      MO TO BRMO
         ADD       PLUS TO BRMO
         MOVE      C0 TO ORDS
         LOAD      ORDS FROM BRMO OF JANX,FEBX,MARX,APRX,MAYX,JUNX,JULX,AUGX:
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
                   SEPX10,OCTX10,NOVX10,DECX10
         ADD       C1,ORDS
         STORE     ORDS INTO BRMO OF JANX,FEBX,MARX,APRX,MAYX,JUNX,JULX,AUGX:
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
                   SEPX10,OCTX10,NOVX10,DECX10
         MOVE      C0 TO QTY
        LOAD      QTY FROM BRMO OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV:
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
                   SEP10,OCT10,NOV10,DEC10
         COMPARE   C0 TO QUANT
         GOTO      READ1 IF EQUAL
         ADD       QUANT,QTY
       STORE     QTY INTO BRMO OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV:
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
                   SEP10,OCT10,NOV10,DEC10
         GOTO      READ1
.
. .....WRITES DATA TO 'SALESMANQTY'
.
.**************************************************************
NEWYEAR  MOVE      C0 TO TOTORD
         MOVE      C0 TO TOTQTY
         MOVE      C0 TO TOTQTY1
         MOVE      C0 TO TOTQTY2
         MOVE      C0 TO TOTQTY3
         MOVE      C0 TO TOTQTY4
         MOVE      C0 TO TOTQTY5
         MOVE      C0 TO TOTQTY6
         MOVE      C0 TO TOTQTY7
         MOVE      C0 TO TOTQTY8
         MOVE      C0 TO TOTQTY9
         MOVE      C0 TO TOTQTY10
         MOVE      C0 TO TOTORD1
         MOVE      C0 TO TOTORD2
         MOVE      C0 TO TOTORD3
         MOVE      C0 TO TOTORD4
         MOVE      C0 TO TOTORD5
         MOVE      C0 TO TOTORD6
         MOVE      C0 TO TOTORD7
         MOVE      C0 TO TOTORD8
         MOVE      C0 TO TOTORD9
         MOVE      C0 TO TOTORD10
         LOAD      TOTORD FROM MNTH OF JANX,FEBX,MARX,APRX,MAYX,JUNX,JULX,AUGX:
                   SEPX,OCTX,NOVX,DECX
         LOAD      TOTORD1 FROM MNTH OF JANX1,FEBX1,MARX1,APRX1,MAYX1,JUNX1:
                   JULX1,AUGX1,SEPX1,OCTX1,NOVX1,DECX1
         LOAD      TOTORD2 FROM MNTH OF JANX2,FEBX2,MARX2,APRX2,MAYX2,JUNX2:
                   JULX2,AUGX2,SEPX2,OCTX2,NOVX2,DECX2
         LOAD      TOTORD3 FROM MNTH OF JANX3,FEBX3,MARX3,APRX3,MAYX3,JUNX3:
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
         LOAD      TOTORD10 FROM MNTH OF JANX10,FEBX10,MARX10,APRX10,MAYX10:
                   JUNX10,JULX10,AUGX10,SEPX10,OCTX10,NOVX10,DECX10
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
         LOAD      TOTQTY7 FROM MNTH OF JAN7,FEB7,MAR7,APR7,MAY7,JUN7,JUL7:
                   AUG7,SEP7,OCT7,NOV7,DEC7
         LOAD      TOTQTY8 FROM MNTH OF JAN8,FEB8,MAR8,APR8,MAY8,JUN8,JUL8:
                   AUG8,SEP8,OCT8,NOV8,DEC8
         LOAD      TOTQTY9 FROM MNTH OF JAN9,FEB9,MAR9,APR9,MAY9,JUN9,JUL9:
                   AUG9,SEP9,OCT9,NOV9,DEC9
         LOAD      TOTQTY10 FROM MNTH OF JAN10,FEB10,MAR10,APR10,MAY10,JUN10:
                   JUL10,AUG10,SEP10,OCT10,NOV10,DEC10
         TRAP      ERR IF RANGE
         MOVE      "01" TO TAB
         WRITAB    OUTPUT,RN;*TAB,MNTH,HOLDYR,TOTORD,TOTQTY:
                   MNTH,HOLDYR,TOTORD1,TOTQTY1,MNTH,HOLDYR,TOTORD2,TOTQTY2:
                   MNTH,HOLDYR,TOTORD3,TOTQTY3,MNTH,HOLDYR,TOTORD4,TOTQTY4:
                   MNTH,HOLDYR,TOTORD5,TOTQTY5,MNTH,HOLDYR,TOTORD6,TOTQTY6:
                   MNTH,HOLDYR,TOTORD7,TOTQTY7,MNTH,HOLDYR,TOTORD8,TOTQTY8:
                   MNTH,HOLDYR,TOTORD9,TOTQTY9,MNTH,HOLDYR,TOTORD10,TOTQTY10
         ADD       C1,RN
         ADD       C1,MNTH
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
         WRITAB    OUTPUT,RN;*TAB,cc,HOLDYR,FINORD,EOYQTY:
                   cc,HOLDYR,FINORD1,EOYQTY1,cc,HOLDYR,FINORD2,EOYQTY2:
                   cc,HOLDYR,FINORD3,EOYQTY3,cc,HOLDYR,FINORD4,EOYQTY4:
                   cc,HOLDYR,FINORD5,EOYQTY5,cc,HOLDYR,FINORD6,EOYQTY6:
                   cc,HOLDYR,FINORD7,EOYQTY7,cc,HOLDYR,FINORD8,EOYQTY8:
                   cc,HOLDYR,FINORD9,EOYQTY9,cc,HOLDYR,FINORD10,EOYQTY10
         TRAPCLR   RANGE
         ADD       C1,RN
         MOVE      YR,HOLDYR
         MOVE      C1 TO INDEX
         CALL      ZEROFILL
         GOTO      FILLDONE
ZEROFILL STORE     C0 INTO INDEX OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT:
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
                   JAN7,FEB7,MAR7,APR7,MAY7,JUN7,JUL7,AUG7:
                   SEP7,OCT7,NOV7,DEC7:
                   JAN8,FEB8,MAR8,APR8,MAY8,JUN8,JUL8,AUG8:
                   SEP8,OCT8,NOV8,DEC8:
                   JAN9,FEB9,MAR9,APR9,MAY9,JUN9,JUL9,AUG9:
                   SEP9,OCT9,NOV9,DEC9:
                   JAN10,FEB10,MAR10,APR10,MAY10,JUN10,JUL10,AUG10:
                   SEP10,OCT10,NOV10,DEC10
         STORE     C0 INTO INDEX OF JANX,FEBX,MARX,APRX,MAYX,JUNX,JULX,AUGX:
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
                   SEPX10,OCTX10,NOVX10,DECX10
         ADD       C1,INDEX
         COMPARE   "132",INDEX
         GOTO      ZEROFILL IF NOT EQUAL
         MOVE      C1,INDEX
         MOVE      C0 TO FINORD
         MOVE      C0 TO FINQTY
         MOVE      C0 TO FINORDA
         MOVE      C0 TO FINQTYA
         MOVE      C0 TO FINORDB
         MOVE      C0 TO FINQTYB
         MOVE      C0 TO FINORDC
         MOVE      C0 TO FINQTYC
         MOVE      C0 TO EOYQTY
         MOVE      C0 TO EOYQTY1
         MOVE      C0 TO EOYQTY2
         MOVE      C0 TO EOYQTY3
         MOVE      C0 TO EOYQTY4
         MOVE      C0 TO EOYQTY5
         MOVE      C0 TO EOYQTY6
         MOVE      C0 TO EOYQTY7
         MOVE      C0 TO EOYQTY8
         MOVE      C0 TO EOYQTY9
         MOVE      C0 TO EOYQTY10
         MOVE      C0 TO TOTQTY
         MOVE      C0 TO TOTQTY1
         MOVE      C0 TO TOTQTY2
         MOVE      C0 TO TOTQTY3
         MOVE      C0 TO TOTQTY4
         MOVE      C0 TO TOTQTY5
         MOVE      C0 TO TOTQTY6
         MOVE      C0 TO TOTQTY7
         MOVE      C0 TO TOTQTY8
         MOVE      C0 TO TOTQTY9
         MOVE      C0 TO TOTQTY10
         RETURN
FILLDONE MOVE      "01",MNTH
. /////////////////////////////////////////////////////////////////////////////
.         COMPARE   "182",RN     <---ADD '13' ever jan.
         COMPARE   "234",RN     <---ADD '13' ever jan.
. /////////////////////////////////////////////////////////////////////////////
         GOTO      READ1 IF NOT EQUAL
.
.....CLOSES OUT 'SALESMANMST'
.
         WEOF      OUTPUT,RN
         CLOSE     OUTPUT
. ////////////////////////////////////////////////////////////////////////////
.         MOVE      "143" TO RN    <----ADD '13' ever jan.
         MOVE      "195" TO RN    <----ADD '13' ever jan.
. .............................................................................
.RN = YEAR TOTAL RECORD FOR YEAR PRIOR TO THE THREE YEARS IN REPORT.
. .............................................................................
. /////////////////////////////////////////////////////////////////////////////
PRINTOLD CALL      ZERO
         OPEN      OUTPUT,"SALESMST",EXCLUSIVE
.
......DISPLAYS TIME, DATE, AND HEADINGS;
         MOVE     "01" TO TAB
         MOVE      "00" TO PASS
.
DISP
PRT1
         PRINT     *f,*L,*N,"Confidential",B7,B7,B7,TITLE,B5,B5,B5,B5,"Date";
         PRINT     B2,MO2,"/",DA2,"/",YR2;
       PRINT     *L,*C,B7,B7,B7,B7,B7,TITLE2,"    ",B7,B7,"Time",B2,HRSTRT,":";
         PRINT     MINSTRT,B1,SECSTRT
.begin patch 2.75   Delete first 13 records from \\nins1\e\data\salesmst.dat 
.         PRINT   *L,B7,B7,"------2003------",B4,"---------2004----------";
.         PRINT        B4,"---------2005-----------"
.         PRINT   *L,B7,B7,"------2004------",B4,"---------2005----------";
.         PRINT        B4,"---------2006-----------"
.begin patch 2.75
.begin patch 2.76   Delete first 13 records from \\nins1\e\data\salesmst.dat 
.         PRINT   *L,B7,B7,"------2003------",B4,"---------2004----------";
.         PRINT        B4,"---------2005-----------"
         PRINT   *L,B7,B7,"------2005------",B4,"---------2006----------";
         PRINT        B4,"---------2007-----------"
.begin patch 2.76
         PRINT      B1,"Month",B5,B5,"Ord",B4,"Names",B5,B3,"Ord",B4;
         PRINT       "Names",B4,"Chng",B4,B2,"Ord",B5,"Names",B4,"Chng"
.
. .....PRINTS DATA BY MONTH AND YEAR
.
END2     READTAB   OUTPUT,RN;*TAB,MO,YR,FINORDA,FINQTYA
.
END3     ADD       "13",RN
         MOVE      B1 TO LASTMOSW
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
         MOVE      YES TO LASTMOSW
* *****************
END4     ADD       "13",RN
         READTAB   OUTPUT,RN;*TAB,MO,YR,FINORDC,FINQTYC
         CALL      PERCENT
.
         MOVE      MO,NUMMO
         BRANCH    NUMMO OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC
.
JAN      MOVE      "January",MONTH
         GOTO      END4B
FEB      MOVE      "February",MONTH
         GOTO      END4B
MAR      MOVE      "March",MONTH
         GOTO      END4B
APR      MOVE      "April",MONTH
         GOTO      END4B
MAY      MOVE      "May",MONTH
         GOTO      END4B
JUN      MOVE      "June",MONTH
         GOTO      END4B
JUL      MOVE      "July",MONTH
         GOTO      END4B
AUG      MOVE      "August",MONTH
         GOTO      END4B
SEP      MOVE      "September",MONTH
         GOTO      END4B
OCT      MOVE      "October",MONTH
         GOTO      END4B
NOV      MOVE      "November",MONTH
         GOTO      END4B
DEC      MOVE      "December",MONTH
.
END4B    PRINT     *L,MONTH,B5,FINORDA,B2,FINQTYA,B4,FINORDB,B2,FINQTYB,B2;
         PRINT       PCTB,"%",B4,FINORDC,B2,FINQTYC,B2,PCTC,"%"
END4A
         SUB       "25",RN     .****DO NOT CHANGE.<<<<<<<<<<<<<<<<<<<<<<<<<<<<
. /////////////////////////////////////////////////////////////////////////////
.         COMPARE   "155",RN     .*ADD '13' EVERY JANUARY
         COMPARE   "207",RN     .*ADD '13' EVERY JANUARY
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
         PRINT     *L,B1,"             _____  _________    _____  ______";
         PRINT     "___  _____    _____  _________  _____"
         PRINT     *L,B2,B6,B6,FINORDA,B2,FINQTYA,B4,FINORDB,B2,FINQTYB,B2;
         PRINT       PCTB,"%",B4,FINORDC,B2,FINQTYC,B2,PCTC,"%"
         PRINT     *l,*l,*l,*l
         PRINT     *L,*L
         PRINT     "*** THIS REPORT DOES NOT INCLUDE CANCELLED ORDERS ***";
         PRINT       B4,B3,B7,"End Time",B1,HR,":",MIN,B1,SEC,*FLUSH
.         DISPLAY   014;
. .." " IS MOVED TO FINAL AND LOADED TABLES ARE PRINTED OUT DURING NEXT CYCLE
OUTPUT
.         MOVE      "01",TAB
         MOVE      "NAMES IN THE NEWS",TITLE
         ADD       C1 TO PASS
         LOAD      TITLE2 FROM PASS OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5:
                   OSLS6,OSLS7,OSLS8,OSLS9,OSLS10
         ADD       "18" TO TAB
         CLOSE     OUTPUT
         DISPLAY   *P1:24,*EL,*P15:24,"PRINTING PASS: ",PASS;
         COMPARE   "11" TO PASS
.         CMATCH    B1,FINAL
         GOTO      STOP IF EQUAL
         OPEN      OUTPUT,"SALESMST",EXCLUSIVE
. //////////////////////////////////////////////////////
.         MOVE      "143",RN      .ADD '13' EVERY JANUARY.
         MOVE      "195",RN      .ADD '13' EVERY JANUARY.
. //////////////////////////////////////////////////////
         MOVE      B1,FINAL
         MOVE      "0",FINALSAV
         MOVE      "0",SAVE
         GOTO      DISP
.
. .....ZERO FILLS TOTAL FIELDS
.
ZERO     MOVE      C0,TOTORD
         MOVE      "0",COUNT1
         MOVE      "0",COUNT2
         MOVE      "0",COUNT3
         MOVE      "0",TOTQTY
         MOVE      "0",FINORD
         MOVE      "0",FINQTY
         MOVE      "0" TO ENDQTY
         MOVE      "0",FINORDA
         MOVE      "0",FINORDB
         MOVE      "0",FINORDC
         MOVE      "0",FINQTYA
         MOVE      "0",FINQTYB
         MOVE      "0",FINQTYC
         MOVE      "0",TOTORD1
         MOVE      "0",TOTQTY1
         MOVE      "0",TOTORD2
         MOVE      "0",TOTORD3
         MOVE      "0",TOTORD4
         MOVE      "0",TOTORD5
         MOVE      "0",TOTORD6
         MOVE      "0",TOTORD7
         MOVE      "0",TOTORD8
         MOVE      "0",TOTORD9
         MOVE      "0",TOTORD10
         MOVE      "0",TOTQTY2
         MOVE      "0",TOTQTY3
         MOVE      "0",TOTQTY4
         MOVE      "0" TO SAVE
         MOVE      "0",TOTQTY5
         MOVE      "0",TOTQTY6
         MOVE      "0",TOTQTY7
         MOVE      "0",TOTQTY8
         MOVE      "0",TOTQTY9
         MOVE      "0",TOTQTY10
         RETURN
.
. .....GET TIME AND DATE
.
TIME     CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MO1,DA1,YR1
         XIF
         IFZ       PC
         UNPACK    DATE INTO MO1,STR1,DA1,STR1,YR1
         XIF
         MOVE      MO1 TO MO
         MOVE      DA1 TO DA
         MOVE      YR1 TO YR2
.         move      yr1 to yr2
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
PERCENT  MOVE      C0 TO FINQTY
         MOVE      C0 TO ENDQTY
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
ZEROB    MOVE      "  0" TO PCTB
         GOTO      PERCENT3
PERCENT2 MOVE      ENDQTY TO WORK
         DIV       FINQTY INTO WORK
         GOTO      ZEROB  IF OVER
         MULT      "100",WORK
         SUB       "100" FROM WORK
         MOVE      WORK,NUMPCT
         MOVE      NUMPCT,PCT
         RESET     PCT
         CLEAR     PCTB
         APPEND    "+",PCTB
         APPEND    PCT,PCTB
         RESET     PCTB
PERCENT3 CMATCH    "A",OK
         GOTO      PERCENT4 IF NOT EQUAL
         MOVE      FINALSAV,FINQTY
         MOVE      B1,OK
         GOTO      PERCENT5
PERCENT4
         CMATCH    YES TO LASTMOSW
         GOTO      PERCENTX IF NOT EQUAL
         MOVE      FINQTYB1 TO FINQTY
         MOVE      B1 TO LASTMOSW
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
         RETURN
ZEROC    MOVE      "  0" TO PCTC
         RETURN
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
         RETURN
PERCENT7 CLEAR     PCTC
         MOVE      B3 TO PCTC
         REP       "0 ",FINQTYC
         REP       "0 ",FINORDC
         RETURN
.
. .....SCREEN DISPLAY;
.
DISPLAY  DISPLAY   *ES,*P31:24,"***** SALESPRT *****",*R,*R:
               *P12:24,"NAMES IN THE NEWS",*P40:24,"|";
DISPLAY2 DISPLAY   *R,*P40:24,"|";
         ADD       C1,COUNT
         COMPARE   "21",COUNT
         GOTO      DISPLAY2 IF NOT EQUAL
         CALL      PAINT
DISPLAY3 DISPLAY   *PV:6,"RECORDS READ: ",*PV:9,"CURRENT DATE BEING READ: ";
         COMPARE   "42",V
         RETURN    IF EQUAL
         ADD       "41",V
         GOTO      DISPLAY3
FORMAT   KEYIN     *P1:24,*EL,",FORMAT ERROR ",ERROR," TRY TO CONTINUE ? ":
                   ANS
         CMATCH    NO TO ANS
         GOTO      STOP IF EQUAL
         TRAPCLR   FORMAT
         TRAP      FORMAT GIVING ERROR IF FORMAT
         RETURN
STOP
.        PRINT     *F,033,033,"H,P,FR1;",033,046,"l8C",*FLUSH
         SPLCLOSE
         release
.         CHAIN     "NORD0012"
         STOP
ERR      DISPLAY   *P1:1,*ES,"RANGE ERROR";
         KEYIN     *P22:1,*EOFF,ANS;
         CMATCH    "Q",ANS
         GOTO      STOP IF EQUAL
         GOTO      ERR
         INCLUDE   NORDIO.inc
         INCLUDE   COMLOGIC.inc


