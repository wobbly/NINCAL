. NInvAcddd.inc
. ............................................................................
..  NAMES IN THE NEWS INCLUDE LIBRARY
.. ............................................................................
.. 
..     FILE:       NININVACD.dat
..   LENGTH:       40
.. COMPRESS:       NONE
..     TYPE:       ISAM
.. isi  KEY:       01-06   Invoice #
.                 07-09   Charge #               
.*******************************************************************************
. Patch 1.2   11Spe2007 PLI conversion
. patch 1.1   Changed IP of File Manager
. patch 1.0   Created & Info pulled out of invoice record 03June2002 DLH
.*******************************************************************************
..
................................................................................
..ADDITIONAL CHARGE SUB-FIELDS ARE DEFINED AS FOLLOWS:
..
..BYTES      DESCRIPTION.
...................................................................................
.
NINVAcdFILE    IFILE     KEYLEN=9,FIXED=49,NoDuplicates
.NINVACDNAME       INIT      "NININVACD.ISI|20.20.30.103:502 "
NINVACDNAME       INIT      "NININVACD.ISI|NINS1:502 "
NINVACDFLD        DIM       9    Inv number + Charge record number.
NINVACDFLAG       FORM      1
.
INVACDVARS     LIST
NInvAcdINV     Dim            6              01-06              Invoice #
NinvAcdNum     Dim            3              07-09              Additional charge record #
NinvAcdCode    Dim            3              10-12              Additional charge Code #
NinvAcdRate    FOrm           7.2            13-22              $ rate
NInvAcdPerc    Form           3.4            23-30              Percentage to LR/NIN income Balance goes to AP
NINVAcdANINCD  Dim            3              31-33              '0' - ADD. CHG CODE = ' ', ADD. CHG. TO AR.
.                                                               '1'= DON'T PRINT CHARGE ON INVOICE and add chg to AP ie: RUNNING CHARGES
.                                                               '2' = add to ar           
.                                                               '3' = add to ar   Hidden charge        
.                                                               '4' = (hidden transfer from NINinc to AP)
.                                                               need to define '2'& '3'as they seem to be used
.begin patch 1.2
.					    '5' = Add to AP3
.					    '6' = Add to XNinc
.end patch 1.2

NinvAcdAextcd  dim            1              34-34              1 = use original qty when/if qty adj occurs
NINvAcdQty     Form           9              35-43              qty for charge calculating this charge
NinvAcdRateT   Dim            1              44-44              m = per thousand f=flat  ' ' = special
NINacdFill     Dim            5              45-49
               LISTend
................................................................................
AcdRecCount    form           3
NInvAcdRec     REcord         (15)                                   ;artificial cap
NinvAcdNumRec     Dim            3              01-03              Additional charge record #
NinvAcdCodeRec    Dim            3              04-06              Additional charge Code
NinvAcdRateRec    FOrm           7.2            07-16              $ rate
NInvAcdPercRec    Form           3.4            17-24              Percetage to LR/NIN income Balance goes to AP
NINVAcdANINCDRec   Dim            3              25-28              '1'= DON'T PRINT CHARGE ON INVOICE ie: RUNNING CHARGES
NinvAcdAextcdRec    dim            1            29-29              '1' = use original qty for charge after qty adjustment
NINvAcdQtyRec     Form           9              30-38              qty for charge used in display and print, populated by compute
NINvAcdTotalRec   Dim            15             39-53              Total for charge used in display and print, populated by compute
NINvAcdDescRec    Dim            35             54-88
NinvAcdRateTRec   Dim            1              89-89              m = per thousand f=flat  ' ' = special
               recordend

