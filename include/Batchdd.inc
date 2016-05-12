.----------------------------------------------------------------
.;
.;   BatchDD.INC - 
.;
.;   Copyright(c) Names in the News Ca., Inc. 2000
.;
.;   Author: David L Herrick
.;   Release         1.0                2010 April
.first pass just dump request to file to be submited to Lanbatch 
.second pass replace lanbatch
.;----------------------------------------------------------------
BatchFLIST Filelist
BatchFILE  IFILE     keylen=6,fix=2006,uncomp,Name="Batch.ISI|NINS1:502"
BatchFILE1 IFILE     KEYLEN=6,fix=2006,uncomp,Name="BatchLog.ISI|NINS1:502"
                    FileListEnd
BatchFILE4 FILE     fix=2006

BatchNAME  INIT      "Batch   "
BatchNME2  INIT      "BatchLog"
BatchFLAG  FORM      1
BatchFLD   DIM       6
BatchLOCK  FORM      1
.;
BatchVARS  LIST
BatchCODE  DIM       6        1-6    .JOB NUMBER - KEY
BatchJob   DIM       2000     7-2005 .string to be passed to lanbatch
          Listend
