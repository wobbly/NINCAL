..............................................................................
.
. LSTIDD 
. NIN Income PRojection data Descriptors
.
. FILE NAME : ListIproj.dat
. REC LENGTH:  FIXED
. INDEX KEY : (1)  1-       (List #, project year, proj #)
.
.INcome by List data descriptor
.used for List management income reporting to Owners
.
.
.
LSTINAME INIT      "LSTIProj.ISI|NINS1:502"
LSTIFILE IFILE     KEYLEN=13,FIXED=138,NODUPLICATES
LSTIFLD  DIM       13
LSTIFLAG FORM      1

LSTIVARS       LIST
LSTIList       DIM             6                 1-6                        \
LSTIYear      DIM             4                 7-10   projection year       } Key
LSTIproj      dim             3                11-13  projection #         /
LstIM1        FORM            9                14-22  projection for month 1 of year (may be fiscal see INCLdd)
LstIM2        FORM            9                23-31  projection for month 1 of year (may be fiscal see INCLdd)
LstIM3        FORM            9                32-40  projection for month 1 of year (may be fiscal see INCLdd)
LstIM4        FORM            9                41-49  projection for month 1 of year (may be fiscal see INCLdd)
LstIM5        FORM            9                50-58  projection for month 1 of year (may be fiscal see INCLdd)
LstIM6        FORM            9                59-67  projection for month 1 of year (may be fiscal see INCLdd)
LstIM7        FORM            9                68-76  projection for month 1 of year (may be fiscal see INCLdd)
LstIM8        FORM            9                77-85  projection for month 1 of year (may be fiscal see INCLdd)
LstIM9        FORM            9                86-94  projection for month 1 of year (may be fiscal see INCLdd)
LstIM10       FORM            9                97-103  projection for month 1 of year (may be fiscal see INCLdd)
LstIM11       FORM            9               104-112  projection for month 1 of year (may be fiscal see INCLdd)
LstIM12       FORM            9               113-121  projection for month 1 of year (may be fiscal see INCLdd)
LSTIDate      Dim            17               122-138  date time projected -         yyyymmddhhmmsssss
              listend
                       
