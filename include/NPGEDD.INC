*******************************************************************************
*INDEXVAR.INC - VARIABLE INCLUDE FOR EOM INDEXES.
*******************************************************************************
* LENGTH     66
*
* ACCESS    SEQUENTIAL.
***********************
.  RELEASE 1.1 ASH 06DEC2004 - INCREASED COMPANY FIELD AND ADDED NEW PAGE NUMBER FIELD
.
.START PATCH 1.1 REPLACED LOGIC
.NPGEFILE FILE     FIXED=60
.NPGEFLAG FORM     1
..NPGENAME INIT      "PAGEXXXX "
..NPGENAME dim      6
.NPGENAME Init      "\\NINS1\e\data\page.srt"
..
.INDNUM   FORM     2       INDEX TYPE.   
..                         1 = STATEMENT EDIT.
..                         2 = PAYABLES REPORT.
..                         3 = MONEY ON ACCOUNT.
..                         4 = INCOME BY MAILER.
..                         5 = INCOME BY LIST.
..                         6 = OVER 90 DAY STATEMENT.
..                         7 = STATEMENT EDIT BY INVOICE DATE.
..                          8 = BAD DEBT STATEMENT EDIT.
..                         9 = open with prepay to lo.
.INDMNUM  DIM      4       MAILER/OWNER/ETC NUMBER.
.INDNAME  DIM      25      CONTACT NAME.
.INDCOMP  DIM      25      COMPANY NAME.
.INDPAGE  DIM      4       PAGE NUMBER
...................................................
NPGEFILE FILE       FIXED=66
NPGEFLAG FORM       1
NPGEFLG2 FORM       1
.START PATCH 01/20/2005 ASH REPLACED LOGIC
.NPGENAME Init      "\\NINS1\e\data\page.srt                 "
NPGENAME Init       "page.srt                 "
.END PATCH 01/20/2005 ASH REPLACED LOGIC
.
NPGEFLE2  IFILE     KEYLEN=6,FIXED=66,Name="page.isi"
NPGEFLD   dim       6
.
NPGEVARS  LIST
INDNUM    FORM      2          1-2      INDEX TYPE.   
.                         1 = STATEMENT EDIT.
.                         2 = PAYABLES REPORT.
.                         3 = MONEY ON ACCOUNT.
.                         4 = INCOME BY MAILER.
.                         5 = INCOME BY LIST.
.                         6 = OVER 90 DAY STATEMENT.
.                         7 = STATEMENT EDIT BY INVOICE DATE.
.                           8 = BAD DEBT STATEMENT EDIT.
.                         9 = open with prepay to lo.
INDMNUM   DIM       6          3-8      MAILER/OWNER/ETC NUMBER
INDNAME   DIM       25         9-33     CONTACT NAME.
INDCOMP   DIM       25        34-58     COMPANY NAME.
INDPAGE   DIM       4         59-62     PAGE NUMBER
INDPAGE2 DIM        4         63-66     PAGE NUMBER
          LISTEND
.END PATCH 1.1 REPLACED LOGIC
