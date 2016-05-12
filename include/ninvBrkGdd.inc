;. ninvBrkGdd.inc
;. ............................................................................
;.  NAMES IN THE NEWS INCLUDE LIBRARY
;. ............................................................................
;. 
;.     FILE:       NINVBRKGuar
;.   LENGTH:       23
;. COMPRESS:       NONE
;.     TYPE:       ISAM
;. isi  KEY:       01-06  INvoice #           Duplicates allowed
;*******************************************************************************
;*patch 1.1   18JUN2005 DMB  Changed IP of File Manager
;*patch 1.0   09Feb2004 DLH  Broker Guar letter 1st release
;*******************************************************************************
.
.
NINVBRKGFILE   IFILE          KEYLEN=6,fixed=33
.NINVBrkGNAME   INIT           "NINVBRKGuar.ISI|20.20.30.103:502 "
NINVBrkGNAME   INIT           "NINVBRKGuar.ISI|NINS1:502 "
NINVBRKGFLD    DIM            6    inv number.
NINVBrkGFLAG FORM      1
.
NinvBRKGINV    Dim            6              01-6   Invoice #
NinvBRKGCC     Dim            2              07-8   Century
NinvBRKGyy     Dim            2              09-10  year
NinvBRKGMM     Dim            2              11-12  Month
NinvBRKGDD     Dim            2              13-14  Day
NinvBRKGTime   Dim            8              15-22  Time stamp when letter generated
NinvBRKGLTR    Dim            1              23-23  Letter type
NinvBRKGFill   Dim            10             24-33  

;...............................................................................

