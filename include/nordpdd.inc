......................................
.nordPDD INCLUSION
.NINPRINT file for In-House LCR's 
.
.FILE NAME:  NINPRINTL
.REC LENGTH: 408 FIXED
.INDEX KEY:  (1) 1-6  (LR#)
.
.last change 18JUN2005 CHANGED IP ADDRESS OF FILE MANAGER
.last change 03May2001 - DLH change to filelist
.
.nordPNAME       INIT    "NINPRINTL"
.nordPNME2       INIT    "NINPRINTL"
.>Patch Begin
.nordPNAME       INIT    "NINPRINTL.ISI|20.20.30.103:502"
nordPNAME       INIT    "NINPRINTL.ISI|NINS1:502"
.nordPNME2       INIT    "NINPRINTL.AAM|20.20.30.103:502"
nordPNME2       INIT    "NINPRINTL.AAM|NINS1:502"
.............Used for backup database.............
.nordPNAME       INIT    "NINPRINTL.ISI|30.2:502"
.nordPNME2       INIT    "NINPRINTL.AAM|30.2:502"
..................................................
.PendFList       Filelist
.nordPFILE       IFILE   KEYLEN=6,FIXED=408,Name="NINPRINTL.ISI|20.20.30.103:502"
nordPFILE       IFILE   KEYLEN=6,FIXED=408,Name="NINPRINTL.ISI|NINS1:502"
.nordPFLE        AFILE   FIXED=408,Name="NINPRINTL.AAM|20.20.30.103:502"
nordPFLE        AFILE   FIXED=408,Name="NINPRINTL.AAM|NINS1:502"
.                FileListEnd
.>Patch End
nordPFLD        DIM     6
nordPFLD1       DIM     5       .Caller
nordPFLD2       DIM     9       .List
nordPFLD3       DIM     5       .Contact
nordPFLD4       DIM     4       .OHIST - indicates type of Search:  Caller,Contact,Faxed to Owner
nordPFLAG       FORM    1
nordPFLG2       FORM    1
nordPPATH       FORM    1       .1=ISAM, 2=AAM
nordPLOCK       FORM    1
.
.USES ORDVARS!!!!!!!!!!!
