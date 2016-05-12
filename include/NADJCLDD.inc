;    Last change:  JD   28 Aug 1999    9:53 am
..............................................................................
.
. NADJCLDD INCLUSION
. NIN ADJUSTMENT COMPUTATION DATA DEFINITIONS
.
..............................................................................
.
ARJ      FORM      10.2            ADJUSTED A/R
AP1J     FORM      10.2            ADJUSTED A/P1
AP2J     FORM      10.2            ADJUSTED A/P2
APJ      FORM      10.2            ADJUSTED TOTAL AP
LRINCJ   FORM      10.2            ADJUSTED LRINC
CTAXJ    FORM      7.2            ADJUSTED CITY TAX
STAXJ    FORM      7.2            ADJUSTED STATE TAX
TAXESJ   FORM      7.2            ADJUSTED TOTAL TAXES
.
CVTFLD   DIM       13             WORK FIELD USED FOR MP CONVERSION.
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10             NUMERIC WORK FIELD FOR CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
NUM102   FORM      10.2           RETURNED NUMERIC FIELD W/DOL.CENT CONVERTED.

