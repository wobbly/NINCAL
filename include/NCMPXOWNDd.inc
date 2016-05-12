******************************************************************************
*VARIABLE INCLUSION FOR Company to Owner  XREF FILE.
******************************************************************************
*              FILE: CompXOwn
*       RECORD SIZE: 
*          COMPRESS: NONE
*              TYPE: ISAM
*               KEY: 1 Company NUMBER  1-6 - DUPLICATES ALLOWED
*	        KEY: 2 Owner  NUMBER   7-10
******************************************************************************
.
NCXOFLst Filelist
NCXOFILE  IFILE     KEYLEN=6,FIX=35,DUP,Name="CompXOwn.ISI|NINS1:502"
NCXOFIL2  IFILE     KEYLEN=4,FIX=35,Name="CompXOwn1.ISI|NINS1:502"
         Filelistend
.
NCXOFLAG  FORM      1
NCXOFLG2  FORM     1
NCXOPATH FORM      1
NCXONAME DIM       12
NCXONAM1  INIT      "CompXOwn  "
NCXONAM2  INIT     "CompXOwn1 "
.
NCXOFLD   DIM       6
NCXOFLD2  DIM       4
.
NCXOVARS      LIST
NCXOComp  DIM       6     1-6       Company NUMBER KEY
NCXOOwn   DIM       4     7-10      Old Owner NUMBER
NCXOFill  DIM       25    11-35      
	ListEnd
..............................................................................
