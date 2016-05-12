;    Last change:  JD   24 Sep 1999    1:25 pm
..............................................................................
.
. NJSTCLDD INCLUSION
. NIN DETAIL ADJUSTMENT COMPUTATION DATA DEFINITIONS
.
.patch 1.1   DLH 18Sep2007  PLI
..............................................................................
.
ARJ      FORM      10.2            ADJUSTED A/R
AP1J     FORM      10.2            ADJUSTED A/P1
AP2J     FORM      10.2            ADJUSTED A/P2
.begin patch 1.1
AP3J     FORM      10.2            ADJUSTED A/P3
.end patch 1.1
APJ      FORM      10.2            ADJUSTED TOTAL AP
LRINCJ   FORM      10.2            ADJUSTED LRINC
CTAXJ    FORM      10.2            ADJUSTED CITY TAX
STAXJ    FORM      10.2            ADJUSTED STATE TAX
TAXESJ   FORM      10.2            ADJUSTED TOTAL TAXES
.
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10             NUMERIC WORK FIELD FOR CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
NUM102   FORM      10.2           RETURNED NUMERIC FIELD W/DOL.CENT CONVERTED.

.
FAR       FORM      10.2       A/R
FAP1      FORM      10.2       A/P1
FAP2      FORM      10.2       A/P2
.begin patch 1.1
FAP3     FORM      10.2            ADJUSTED A/P3
.end patch 1.1

