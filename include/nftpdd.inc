.//******************************************************
.// NFTPDD INCLUSION
.// SUBSIDIARY FILE FOR DATACARD
.// ADDRESSING FILE DESCRIPTOR.
.//****************************************************
.// NAMES IN THE NEWS MASTER ADDRESSING FILE.
.//****************************************************]
.//      FILE:      NINFTP
.//    LENGTH:      1024
.//      TYPE:      SEQ
......................................................
NFTPNAME      INIT  "NINFTP"
NFTPFLD                 DIM 9
NFTPFLD1      DIM 9

NFTPLOCK      FORM 1                    .0 OR 1=filepi,2=recordlock, 3=no lock
NFTPFLAG      FORM 1
NFTPPATH      FORM 1



NFTPFILE      FILE  
NFTPFLIST     FILELIST
NFTPFILE1     IFILE KEYLEN=9,Name="NINFTP.isi|NINS1:502"
NFTPFLE2      AFILE Name="NINFTP.aam|NINS1:502"
                        FILELISTEND

.Note description should have no spaces so Winbatch FTP jobs process records correctly when using (Parse)
.//FTP Information File - NINFTP
NFTPCOMP      DIM   6                     .Eventually Fulfillment Company Number     .1-6
NFTPCOMPID              DIM   3           .Counter for mulitple sites                .7-9
NFTPDESC      DIM   100                   .Company Description                       .10-109
NFTPPROTOCOL        DIM 50                .FTP/FTPS                                  .110-159
NFTPADDRESS         DIM 255               .Ftp Address                               .160-414
NFTPUSERNAME        DIM 255               .User Name                                 .415-669
NFTPPASSWORD        DIM 255               .Password                                  .670-924
.begin patch
NFTPIP             Dim       15           .IP address xxx.xxx.xxx.xxx
NFTPFILLER          Dim       85
.NFTPFiller          DIM 100               .Filler                                    .925-1024
.end patch


.//Varlist NFTP
NFTPVARS VARLIST NFTPCOMP:
          NFTPCOMPID:
          NFTPDESC:
          NFTPPROTOCOL:
          NFTPADDRESS:
          NFTPUSERNAME:       
          NFTPPASSWORD:
          NFTPIP:
          NFTPFiller

