. .............................................................................
. VARIABLE DATA DEFINITIONS FOR NINACD/TXT.
. .........................................
.patch 2.01    DLH 05Nov2003
.patch 2.0     DLH Apr99  3 byte key/code
NACDFILE       IFILE          KEYLEN=3,FIXED=65
NACDNAME       INIT           "NINACD|NINS1:502    "
NACDFLAG       FORM           1
NACDFLD        DIM            3
.
NacdVars     List
NACDKEY        DIM            3       1-3         000-999.
NACDTEXT       DIM            35      4-38        FREE FORM TEXT.
;begin patch 2.01
NacdFILL1      Dim            5      39-43        Not used (reserved to expand text) 
NacdType       Dim            1      44-44        ' ' = special, 'm' = per thousand 'f' = flat
NacdFill2      Dim            21     45-65        
;NACDFILL DIM       27     39-65        NOT USED.
             Listend
;end patch 2.01
.
..............................................................................
