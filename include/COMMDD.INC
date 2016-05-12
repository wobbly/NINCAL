..............................................................................
.
.COMMDD INCLUSION
.LOCATED IN \\NinS1\E\DATA\TEXT
.FILE NAME : COMMission.DAT
.REC LENGTH: 32
.INDEX KEY: 1-6 LR
..............................................................................
.
COMMFiLE   FILE     fixed=82
COMMIFILE IFILE     KEYLEN=9,FIXED=82,Name="COMMissions.isi|NINS1:502"

COMmNAME  INIT      "COMMissions.dat|NINS1:502"
COMmINAME  INIT      "COMMissions.isi|NINS1:502"
COMmFLD   DIM       9      1-9      Salesperson & company code
COMmFLAG  FORM      1
COMmFLG2  FORM      1
COMmlock  FORM      1      0 or 1 - filepi, 2 =record locking, = no locks
.===================================================================================
COMMVARS  LIST
COmmSLs   DIM       3          1-3           salesperson ID
COMMCPY   DIm       6          4-9           company code (mailer)
COMMDate  Dim       8         10-17          Date commission started  ccyymmdd
COMMCOMP  Dim       55        18-72          Mailers name (compcomp)     
CommFILL  DIM       10        73-82          BLANK
          listend
.===================================================================================
