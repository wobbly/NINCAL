;----------------------------------------------------------------
;
;   SlctClnDD.inc -
;
;   Copyright(c) Names in the News Ca., Inc. 2002
;
;   Author: David L Herrick
;   Created: 2/7/2002 5:54:10 AM
;   Last change: DLH 2/7/2002 6:04:53 AM
;----------------------------------------------------------------

. .............................................................................
. VARIABLE DATA DEFINITIONS FOR SelectClean.dat.
. .........................................
SlctClnFILE IFILE     KEYLEN=35,var=70
SlctClnNAME INIT      "SelectClean"
SlctClnFLAG FORM      1
SlctClnFLD  DIM       35
.
SlctClnVars List
SlctClnKEY  DIM       35       1-35        .variations of Selects from order
SlctClnTEXT DIM       35      36-70        .correct form of select
            Listend
.
..............................................................................
