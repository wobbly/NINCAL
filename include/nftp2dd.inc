.//******************************************************
.// NFTP2DD INCLUSION
.// Wildcard Subsidiary Ftp Detail File - NINFTP2
.// ADDRESSING FILE DESCRIPTOR.
.//****************************************************
.// NAMES IN THE NEWS Subsidiary Ftp detail.
.//****************************************************]
.//      FILE:      NINFTP2
.//    LENGTH:      416
.//      TYPE:      multiple keys
.RElease   2014 February 28 add XML info type
......................................................
NFTP2FLD      DIM   12                .ISI Key
NFTP2FLD1     DIM   12                .AAM Key

NFTP2LOCK     FORM 1                    .0 OR 1=filepi,2=recordlock, 3=no lock
NFTP2FLAG     FORM 1
NFTP2PATH     FORM 1



NFTP2NAME     INIT                  "NINFTP2"

NFTP2FILE     FILE  
NFTP2FLIST              FILELIST
          
NFTP2FILE1              IFILE             Name="NINFTP2.ISI|NINS1:502"
NFTP2FLE2     AFILE           Name="NINFTP2.aam|NINS1:502"
                        FILELISTEND





//Wildcard Subsidiary File - NINFTP2
NFTP2COMP     DIM   9                 .1-9             .NFTPCOMP+NFTPCOMID                                                          
NFTP2COMPID         DIM 3             .10-12           .Counter for muliple file types/prefixes/suffixes                      
NFTP2ACTION         DIM 1             .13-13           ."U"pload, "D"ownload, "X" inactive upload, "Z", inactive download, "A"pply only no up or down                                                          
NFTP2InfoType       DIM 1             .14-14           .S=Shipping/M=Merge/N=Order Confirmation/C=Statement/T=Stats/O=Order/G=Billing Summary/B=Billing/L=Lol
NFTP2RemoteDir      DIM 100           .15-114          .Remote Directory                                                        
NFTP2WILDCARD       DIM 100           .115-214         .Search Criteria                                                         
NFTP2LocalDir       DIM 100           .215-314  .Local Directory                                                         
NFTP2DELETE         DIM 1             .315-315  .Delete File After Download/Upload                                   
NFTP2DAILY          DIM 1             .316-316  .This flag is to check whether we received Info for the previous day 
NFTP2Notification   DIM 50            .317-366  .Email Address of person to be notified                                    
NFTP2Attach         DIM 1             .367-367  .Include Attachment                                                         
NFTP2Filler         DIM 49            .368-416  .Filler                                                                    


//Varlist NFTP2
NFTP2VARS VARLIST NFTP2COMP:
          NFTP2COMPID:
          NFTP2ACTION: 
          NFTP2InfoType:
          NFTP2RemoteDir:
          NFTP2WILDCARD:
          NFTP2LocalDir:
          NFTP2DELETE:
          NFTP2DAILY:
          NFTP2Notification:
          NFTP2Attach:
          NFTP2Filler
