//******************************************************
// NFTPLOGDD INCLUSION
// Wildcard Subsidiary Ftp Detail File - NINFTPLOG
// ADDRESSING FILE DESCRIPTOR.
//****************************************************
// NAMES IN THE NEWS Subsidiary Ftp detail.
//****************************************************]
//      FILE:      NINFTPLOG
//    LENGTH:      349,ISAM,AAM
//      TYPE:      SEQ
......................................................
NFTPLOGNAME	    INIT                  "NINFTPLOG"
//NFTPLOGFLD	    DIM	6                 .ISI Key Company Number
NFTPLOGFLD1	    DIM	12                 .AAM Key FileName                                 .Company Number
NFTPLOGFLD2	    DIM	103                .AAM Key FileName                                 .Filename

NFTPLOGLOCK	    FORM 1	          .0 OR 1=filepi,2=recordlock, 3=no lock
NFTPLOGFLAG	    FORM 1
NFTPLOGPATH	    FORM 1

NFTPLOGFILE         File                                                                      .FTP Log File


NFTPLOGFLIST  	    FILELIST
.//FTP Log File - NINFTPLOG
.//NFTPLOGFILE	    IFile                 FIXED=100,Name="NINFTPLOG.isi|NINS1:502"     .FTP Log File - BY Company Number
NFTPLOGFLE2	    AFile                 Name="NINFTPLOG.aam|NINS1:502"               .FTP Log File - By Company File Name
		    FILELISTEND







NFTPLOGCOMP	      DIM 6                 .Company Number                                    .1-6
NFTPLOGCOMPID	      DIM 3                 .Counter for mulitple sites                        .7-9
NFTPLOGAction         DIM 1                 .Upload/Download                                   .10
NFTPLOGFileName       DIM 100               .File Name                                         .11-110
NFTPLOGDESC           DIM 100               .Company Description                               .111-210
NFTPLOGDATE           DIM 19                .Receipt Date                                      .211-229
NFTPLOGSIZE           DIM 20                .File Size                                         .230-249
NFTPLOGDDATE          DIM 10                .Transaction Date                                  .250-259
NFTPLOGFiller         DIM 91                .Filler                                            .260-350

//Varlist NFTP3




NFTPLOGVARS VARLIST NFTPLOGCOMP:
	NFTPLOGCOMPID:
	NFTPLOGACTION:	
	NFTPLOGFileName:	
	NFTPLOGDESC: 
	NFTPLOGDATE:
	NFTPLOGSIZE:
	NFTPLOGDDATE:		
	NFTPLOGFiller