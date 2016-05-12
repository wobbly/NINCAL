;----------------------------------------------------------------
;
;   SXRFDD.inc - 
;
;   Copyright(c) Names in the News Ca., Inc. 2000
;
;   Author: David L Herrick
;   Created: 4/8/2002 7:45:34 AM
;   Last change: DLH 4/8/2002 7:45:37 AM
;----------------------------------------------------------------

..............................................................................
.
. Statistical Xref INCLUSION
. List cross reference Stats FILE DEFINITION
.
. FILE NAME : Statxref
.Patch 06/18/2005 - DMB Change IP address of files manager
.Patch 01/25/2005 - ASH Converted Mailer/Broker fields
.RElease   1.0            09April 2001
.
. REC LENGTH: 52 FIXED
. INDEX KEY : 
..............................................................................
.
StatXFLst  FILELIST
.Start Patch 01/25/2005 - Replaced logic
.StatxFILE  IFILE     KEYLEN=12,UNCOMP,Name="STATXREF.ISI|20.20.30.103:502"        ...Mailer & clients LIST code
.StatxFLE1  IFILE     KEYLEN=30,UNCOMP,Name="STATXREF1.ISI|20.20.30.103:502"
.StatxFLE2  IFILE     KEYLEN=6,UNCOMP,Name="STATXREF2.ISI|20.20.30.103:502"
..................................................

.StatxFILE  IFILE     KEYLEN=14,UNCOMP,Name="STATXREF.ISI|20.20.30.103:502"        ...Mailer & clients LIST code
StatxFILE  IFILE     KEYLEN=14,UNCOMP,Name="STATXREF.ISI|NINS1:502"        ...Mailer & clients LIST code
.StatxFLE1  IFILE     KEYLEN=36,UNCOMP,Name="STATXREF1.ISI|20.20.30.103:502"
StatxFLE1  IFILE     KEYLEN=36,UNCOMP,Name="STATXREF1.ISI|NINS1:502"
.StatxFLE2  IFILE     KEYLEN=6,UNCOMP,Name="STATXREF2.ISI|20.20.30.103:502"
StatxFLE2  IFILE     KEYLEN=6,UNCOMP,Name="STATXREF2.ISI|NINS1:502"
.End Patch 01/25/2005 - Replaced logic
           Filelistend
StatxNAME  INIT      "StatXref"             1-14
StatxNME1  INIT      "StatXref1"           1-6,19-48
.StatxNME1  INIT      "StatXref1"           19-48
StatxNME2  INIT      "StatXref2"           15-20
.
.Start Patch 01/25/2005 - Replaced logic
.StatXfld   dim       12
..
.StatxFLD1  dim       34
......................
StatXfld   dim       14
.
StatxFLD1  dim       36
.End Patch 01/25/2005 - Replaced logic
.StatxFLD1  dim       30
Statxfld2  DIM       6
StatxFLAG  FORM      1
StatxFLG1  FORM      1
StatxFLG2  FORM      1
Statxpath  FORM      1
.
Statxvars  list
.Start Patch 01/25/2005 - Replaced logic
.StatXMLR   DIM       4      1-4          NIN Mlr number KEY 1
StatXMLR   DIM       6      1-6          NIN Mlr number KEY 1
.End Patch 01/25/2005 - Replaced logic
Statxcode  DIM       8      7-14                        Key 1  Clients code "left justified"
Statxlist  dim       6     15-20          nin LIST #    key 3  Our code RIGHT justified zero filled
Statxdesc1  dim      30     21-50         Stat DESC - + mlr # = Key 2     Clients description
Statxmrkt  DIM       2     51-52          market code
           listend
.....
.END OF INCLUDE
.
..............................................................................
