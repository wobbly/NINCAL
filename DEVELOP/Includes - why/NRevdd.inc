..............................................................................
.
.NREVDD INCLUSION
.LOCATED IN \\NTS1\E\DATA\DBASE
.FILE NAME : REVENUE.DAT
.REC LENGTH: 380
.INDEX KEY: 1-12 TYPE,SRC,CLIENT#,CCYY
. Jan 2011 Expanded for AR- AP DLH
. SEP 2010 New vars DLH
..............................................................................
.
NREVFLE   FILE
.NREVIFILE IFILE     KEYLEN=12,FIXED=380
NREVIFILE IFILE     KEYLEN=12,FIXED=721
NREVNAME  INIT      "REVENUE"
NREVFLD   DIM       12      1-12       TYPE,SRC,CLIENT#,CCYY
NREVFLAG  FORM      1
NREVFLG2  FORM      1
NREVlock  FORM      1             0 or 1 - filepi, 2 =record locking, = no locks
.===================================================================================
.Revenue file
.Index of revenue file -see above
NREVVARS    LIST
TYPE       DIM      1      1-1            'R'ent or 'E'xchange
SRC        DIM      1      2-2            'B'rokerage or 'M'anagement
CID        DIM      6      3-8             Mailer or List number
YR0        DIM      4      9-12            CCYY
CLIENT     DIM     45     13-57           Client\List Name  
.=================================================================================
.
JANlr     FORM    10.2    59-71     HOLDS ADJUSTED LR INC FOR MONTH
FEBlr     FORM    10.2    72-84
MARlr     FORM    10.2    85-97
APRlr     FORM    10.2    98-110
MAYlr     FORM    10.2   111-123
JUNlr     FORM    10.2   124-136
JULlr     FORM    10.2   137-149
AUGlr     FORM    10.2   150-162
SEPlr     FORM    10.2   163-175
OCTlr     FORM    10.2   176-188
NOVlr     FORM    10.2   189-201
DEClr     FORM    10.2   202-214
.
JANnin    FORM    10.2   215-227      HOLDS ADJUSTED NIN INCOME
FEBnin    FORM    10.2   228-240
MARnin    FORM    10.2   241-253
APRnin    FORM    10.2   254-266
MAYnin    FORM    10.2   267-279
JUNnin    FORM    10.2   280-292
JULnin    FORM    10.2   293-305
AUGnin    FORM    10.2   306-318
SEPnin    FORM    10.2   319-331
OCTnin    FORM    10.2   332-344
NOVnin    FORM    10.2   345-357
DECnin    FORM    10.2   358-370
.
JANAR     FORM    10.2   371-384     HOLDS ADJUSTED AR INC FOR MONTH
FEBAR     FORM    10.2   385-397
MARAR     FORM    10.2   398-410
APRAR     FORM    10.2   411-423
MAYAR     FORM    10.2   424-436
JUNAR     FORM    10.2   437-449
JULAR     FORM    10.2   450-462
AUGAR     FORM    10.2   463-475
SEPAR     FORM    10.2   476-488
OCTAR     FORM    10.2   489-501
NOVAR     FORM    10.2   502-514
DECAR     FORM    10.2   514-527
.
JANAP     FORM    10.2   528-540      HOLDS ADJUSTED AP INCOME
FEBAP     FORM    10.2   541-553
MARAP     FORM    10.2   554-566
APRAP     FORM    10.2   567-579
MAYAP     FORM    10.2   580-592
JUNAP     FORM    10.2   593-605
JULAP     FORM    10.2   606-618
AUGAP     FORM    10.2   619-631
SEPAP     FORM    10.2   632-644
OCTAP     FORM    10.2   645-657
NOVAP     FORM    10.2   658-670
DECAP     FORM    10.2   671-683
REVunbld  FORM    8.2    684-694      Current Unbilled
.
REvAR     Form    10.2   695-707       AR for year
REVAP     Form    10.2   707-720
REVCO     Dim     1      721-721      ' ','N' = NIN, 'P'= pacificlists 
.
        listend
.===================================================================================

