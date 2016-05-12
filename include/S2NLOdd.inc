.*  SrDS 2 NIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* MIN DATACARD FILES.
.* ****************************************************
.
.      FILE:      SRDS2NINLO
.    LENGTH:      Fixed
.  COMPRESS:      NONE
.      TYPE:      Isam fixed
......................................................
......................................................
S2NLONAME INit      "SRDS2NinLO       "
S2NLOFlist          Filelist
S2NLOFILE IFILE     Name="SRDS2ninLo.Isi|NINS1:502"
S2NLOFILE2          IFILE     Name="SRDS2ninLo2.Isi|NINS1:502"
          FileListEnd
S2NLOFLAG FORM      1
S2NLOPATH FORM      1
S2NLOFld  Dim       6
S2NLOFld2 Dim       6
.
S2NLOVARS           LIST                   .
S2NLOSRDS  Dim       6          1-6    
S2NLOFill1          dim       1          7-7    Not USed
S2NLONIN  DIM       6          8-13   NIN Owner #
          ListEnd
          