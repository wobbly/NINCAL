.*  SRDS 2 NIN List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* SRDS DATACARD FILES.
.* ****************************************************
.
.      FILE:      SRDS2NIN
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      Isam fixed
......................................................
. LAST MODIFIED
. patch 1.0           11 May 2011 DLH New
......................................................
S2NNAME   INit      "SRDS2Nin"
S2NFlist  Filelist
S2NFILE   IFILE     Name="SRDS2nin.Isi|NINS1:502"
S2NFILE2  IFILE     Name="SRDS2nin2.Isi|NINS1:502"
          FileListEnd
S2NFLAG   FORM      1
S2NPATH   FORM      1
S2NFld    Dim       6
S2NFld2   Dim       6
.
S2NVARS             LIST                   .
S2NSRDS    Dim       6          1-6    6 byte SRDS LIst code
S2NStatus dim       1          7-7    "*"  = Special do not update
. special pricing -- need to print SRDS list for counts etc - argh
S2NNIN    DIM       6          8-13   NIN List #
          ListEnd
          