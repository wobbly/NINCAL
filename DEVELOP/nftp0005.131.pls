.//Program information
.//Program Name:NFTP0001.PLS
.//Program Description:This job will post or download files from an ftp site from one of our partners/clients etc.  And Place them in a specific folder.
.//Associated Winbatch Job:  FTP.WBT
.//Programmer Name:David Baca
.//Original Release Date:June 12, 2006

.//I have added a startdate to prevent the downloading of stale files that have already been applied prior to this programs release.
.//Program will dump file into \\nins1\e\data\storage [ship/merge] location depending on the var NFTP2InfoType i have created individual folders for each company

.//Set CuteFTP to use smart overwrite - overwrite when it hits a dupe.  You must do through the interface.


          Include Common.inc
          Include Cons.inc
          Include nftpdd.inc
          Include nftp2dd.inc
          Include nftplogdd.inc


PC EQU 0
Release   Init      "1.31"    DLH	cleanup logfile read and date/time check and prog id to error message
reldate   Init      "2014 March 06"
.Release   Init      "1.30"    RVW  trap OLE Exceptions so program runs and doesn't crash when using case sensitive sites
.reldate   Init      "2014 February 13"
.Release   Init      "1.29"    DLH  set transfer type of downloads to Binary
.reldate   Init      "2014 January 13"
.Release   Init      "1.28"    DLH  more attempts to bolster 1.26
.reldate   Init      "2014 January 10"
.Release   Init      "1.27"    DLH  add username, ftp address, etc to error message
.reldate   Init      "2014 January 6"
.Release   Init      "1.26"    DLH  change object trap to send message and continue instead of stopping
.reldate   Init      "16 Aug 2011"
.Release   Init      "1.25"    DLH  add computername to error messages to try and track down some issues
.reldate   Init      "10 Aug 2011"
.Release   Init      "1.24"    DLH  Increase default timer on sendmail
.reldate   Init      "29 Jan 2010"
.Release   Init      "1.23"    DLH Use data manager like every other instance accessing this file
.reldate   Init      "19 Nov 2008"
.Release  Init      "1.22"    DLH add wait for email attachment to be ready
.reldate  Init      "07 Aug 2008"
.Release  Init "1.2.1"        DLH       additional error infor at Object trap
.REldate  INit      "18 October 2007"
.Release Init "1.2"  30OCT2006          DMB       Fixed bug that did not take into account dupes.  Date check will now check all instances of that filename for date/time
.Release Init "1.1"  26OCT2006          DMB       Added File Type L=LOL
.Release Init "1.0" 12JUN2006 DMB       Program Initial Release

objerror  dim       490
OLEERROR  dim       490
.begin patch 1.22
FileCheck FIle
trapcount form      4
.end patch 1.22
.begin patch 1.29
TRANSFERTYPE    INIT    "BINARY"
.end patch 1.29


FtpDirList          File                  .Temporary Directory Listing

Taskname1  DIM 150
Taskname2  DIM 150
Taskname3  DIM 100
Taskname4  DIM 510
Taskname5  DIM 510
Taskname6  DIM 100
Taskname7  DIM 100

StartDate  FORM 5
FirstCall  FORM 1

dlFiles       datalist
fileDir      DIM 255
NDX        FORM 9
DateString DIM 10
          clock timestamp,timestamp
          unpack timestamp,str2,yy,mm,dd
          pack DateString,mm,slash,dd,slash,cc,yy

.start dh 15 Oct 2010
.          Move      "09",MM
.          Move      "16",DD   
.          Move      "06",YY
          Move      "01",MM
          Move      "01",DD   
          Move      "10",YY
.end dh 15 Oct 2010
          Call      cvtjul    
          Move      juldays  to StartDate
          Call      Paint
.begin patch 1.23
.         Open      NFTPFILE,NFTPNAME
          Open      NFTPFILE,"NINFTP|10.10.30.103:502"
.end patch 1.23

.//Create the Automation Object for Cute Ftp - Current Version is 6.0
MYSite     automation      class="CuteFTPPro.TEConnection"
          create  MySite
.         pause   "5"

          Loop
                    Read NFTPFILE,seq;NFTPVARS
          Until Over
                    SQUEEZE NFTPPROTOCOL,NFTPPROTOCOL       
.temp fix for Donnelly
.                    Goto NextSiteRead IF (NFTPCOMP = "009406")      
                    call      debug if (NFTPCOMP = "009411")      
......
                    Goto NextSiteRead IF (NFTPPROTOCOL = "SMTP")      
                    move c1 to FirstCall                    . Reset the ReadFlag          
.//This section logs onto the ftp site
                    Setprop MySite,*Protocol=NFTPPROTOCOL
                    SQUEEZE NFTPADDRESS,NFTPADDRESS                   
                    Setprop MySite,*Host=NFTPADDRESS
                    SQUEEZE NFTPUSERNAME,NFTPUSERNAME                           
                    Setprop MySite,*Login=NFTPUSERNAME
                    SQUEEZE NFTPPASSWORD,NFTPPASSWORD                                     
                    Setprop MySite,*Password=NFTPPASSWORD
.begin patch 1.29
    		    if	(NFTPCOMP = "009406")
    		    	Setprop MySite,*TransferType=TRANSFERTYPE
 		    endif
.end patch 1.29
                	Trap    TrapConnectFailure giving error if Object
                              MySite.Connect giving N9
                	Trapclr Object                                        
.begin patch 1.21
                Trap    TrapCOMMFailure giving error if Object
.end patch 1.21
                    If (N9 = c0)
.                             MySite.Status giving taskname 
                              Pack    MAILSubjct,"Could Not Connect to ",NFTPADDRESS
                              //   Set the text message that is send with the attachments
                              Clear     Mailbody
.begin patch 1.31
				Append "Nftp0005:",Mailbody
                              append    error,Mailbody
.end patch 1.31
                              append    "error was ",Mailbody
                              append    error,Mailbody
.begin patch 1.27
                              append    CrLF,Mailbody
                              append    NFTPusername,Mailbody                                         
                              append    CrLF,Mailbody
                              append    NFTPDESC,Mailbody                                         
                              append    CrLF,Mailbody
                              append    NFTPAddress,Mailbody                                         
                              append    CrLF,Mailbody
                              append    NFTPCOMP,Mailbody                                         
.end patch 1.27
                              append    CrLF,Mailbody
                              append    taskname,Mailbody
                              reset     Mailbody
.                             Pack    MailBody,"Error Was ",taskname
                              Call      EmailSupport
                              Display *P10:8,"Working On : ",NFTPDESC                                         
                              Display *P10:10,"Connecting to : ",NFTPADDRESS,b5,NFTPCOMP                                                    
                    Else
.//We must check for mutiple file type for this site                  
                              Pack NFTP2FLD1,"01X",NFTPCOMP,NFTPCOMPID
                              Loop 
                                        If (FirstCall = c1)                     
                                                  add c1 to FirstCall
                                                  Call NFTP2AIM
                                                  
                                        Else
                                                  Call NFTP2KG
                                        Endif
                              Until Over
                                        Call CleanWildCardVars
.//This section will do one of two options Based upon NFTPAction either Upload or Download
.************************************************************************************************************
.//Upload is pretty straight forward - it check both the local an remote directories to make sure they exist
.//If they both exist then it posts the file
.************************************************************************************************************
.//Download is a little more complex
.//Download grabs a remote directory listing of the ftp site.
.//It sequentially reads through this file to see if it's gets a match on the ftplog file.  If not it downloads the file.
.//If it does get a match it then does a compare to see if the date/or times do not match.  If they do not it downloads the file else do nothing
.//Lastly the download portion has the option of deleting the file off the ftp site.
************************************************************************************************************
                                        If (NFTP2ACTION = "U")          .Upload
.//Designates where the downloaded file will go
.                                                 If (NFTP2InfoType = "O")                                                                                                                              
.                                                           If (NFTP2LocalDir = "")
.                                                                     Pack NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","ORDERS"
.                                                           Endif
.                                                 Endif
.                                                 If (NFTP2LocalDir = "")
.                                                           Pack    SmtpSubject,"No Local Directory Specified for this Upload",NFTP2RemoteDir," on ",NFTPADDRESS
.                                                           Pack    SmtpTextMessage(1),"Please Verify for Company: ",NFTPCOMP,b1,NFTP2WILDCARD                                                          
.                                                           Call      EmailSupport                                                                    
.                                                 Else
                                                  If (NFTP2LocalDir = "")
                                                            If (NFTP2InfoType = "O")                                                                                                                              
                                                                      Pack      taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","ORDERS\",NFTP2WILDCARD
                                                                      Pack      fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","ORDERS\"
                                                                      Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","ORDERS"                                                                    
                                                            ElseIf (NFTP2InfoType = "S")                                                                                                                          
                                                                      Pack      taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","SHIPPING\",NFTP2WILDCARD
                                                                      Pack      fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","SHIPPING\"                                                                       
                                                                      Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","SHIPPING"                                                                  
                                                            ElseIf (NFTP2InfoType = "M")                                                                                                                          
                                                                      Pack      taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","MERGE\",NFTP2WILDCARD
                                                                      Pack      fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","MERGE\"                                                                
                                                                      Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","MERGE"                                                                     
                                                            ElseIf (NFTP2InfoType = "N")                                                                                                                          
                                                                      Pack      taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","OCONFIRM\",NFTP2WILDCARD
                                                                      Pack      fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","OCONFIRM\"                                                             
                                                                      Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","OCONFIRM"                                                                  
                                                            ElseIf (NFTP2InfoType = "C")                                                                                                                          
                                                                      Pack      taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","STATEMENT\",NFTP2WILDCARD
                                                                      Pack      fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","STATEMENT\"                                                                      
                                                                      Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","STATEMENT"                                                                 
                                                            ElseIf (NFTP2InfoType = "T")                                                                                                                          
                                                                      Pack      taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","STATS\",NFTP2WILDCARD
                                                                      Pack      fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","STATS\"                                                                                                                                                                              
                                                                      Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","STATS"                                                                     
                                                            ElseIf (NFTP2InfoType = "B")                                                                                                                          
                                                                      Pack      taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","BILLING\",NFTP2WILDCARD
                                                                      Pack      fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","BILLING\"                                                                                                                                                                            
                                                                      Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","BILLING"                                                                   
                                                            ElseIf (NFTP2InfoType = "G")                                                                                                                          
                                                                      Pack      taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","MergeSum\",NFTP2WILDCARD
                                                                      Pack      fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","MergeSum\"                                                                                                                                                                           
                                                                      Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","MergeSum"                                                                  
                                                            ElseIf (NFTP2InfoType = "Z")                                                                                                                          
                                                                      Pack      taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","OTHER\",NFTP2WILDCARD
                                                                      Pack      fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","OTHER\"                                                                                                                                                                              
                                                                      Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","OTHER"                                                                     
.//Begin Patch 1.1 Code Added
                                                            ElseIf (NFTP2InfoType = "L")                                                                                                                          
                                                                      Pack      taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","LOL\",NFTP2WILDCARD
                                                                      Pack      fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","LOL\"                                                                                                                                                                                
                                                                      Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","LOL"                                                             
.//End Patch 1.1 Code Added
                                                            Endif
                                                  Endif     
                                                  
                                        
                                                  
                                                            Path Exist,NFTP2LocalDir
                                                            If Over
                                                                      Pack    MAILSubjct,"Local Directory Does not exist for ",NFTP2LocalDir," NFTPCOMP"
.//   Set the text message that is send with the attachments
                                                                      Pack    MailBody," ",taskname
                                                                      Call      EmailSupport                            
                                                            Else
                                                                      Setprop MySite,*LocalFolder=NFTP2LocalDir
.//This sections lists the files on the ftpsite             
                              Display *P10:8,"Working On : ",NFTPDESC                                         
                              Display *P10:10,"Connecting to : ",NFTPADDRESS,b5,NFTPCOMP                                                    

                                                                      Display *P10:12,"Uploading Files to : ",NFTP2RemoteDir      
                                                                      If (NFTP2RemoteDir <> "/")
.//Begin Patch 1.30          													 
	       							      Trap    TrapCaseError giving error if Object
.//End Patch 1.30          													 
                                                                                MySite.RemoteExists giving n9 using NFTP2RemoteDir                                                            
                                                                                If (N9 = c0)                                    
.                                                                                         MySite.Status giving taskname 
                                                                                          Pack    MAILSubjct,"Could Not Find to Remote Directory ",NFTP2RemoteDir," on ",NFTPADDRESS
.//   Set the text message that is send with the attachments
                                                                                          Clear     Mailbody
.begin patch 1.31
				Append "Nftp0005:",Mailbody
                              append    error,Mailbody
.end patch 1.31
                                                                                          append    "error was ",Mailbody
                                                                                          append    error,Mailbody
                                                                                          append    CrLF,Mailbody
                                                                                          append    taskname,Mailbody
                                                                                                                                  move      "COMPUTERNAME",str50
                                                                                                                                  clock     env,str50
                                                                                                                                  append    CrLF,Mailbody
                                                                                                                                  append    str50,Mailbody
                                                                                                                                  append    CrLF,Mailbody                                                                                                                                
                                                                                          reset     Mailbody
.                                                           .                             Pack    MailBody,"Error Was ",taskname
                                                                                          Call      EmailSupport
                                                                                Endif
                                                                      Endif
                                                                      If (N9 <> 0 or NFTP2REMOTEDIR = "/")
                                                                                Setprop MySite,*RemoteFolder=NFTP2RemoteDir                                                                   
                                                                                If (N9 = c0)                                    
.                                                                                         MySite.Status giving taskname 
                                                                                          Pack    MAILSubjct,"Could Not Relocate to Remote Directory ",NFTP2RemoteDir," on ",NFTPADDRESS
.//   Set the text message that is send with the attachments
                                                                                          Clear     Mailbody
.begin patch 1.31
				Append "Nftp0005:",Mailbody
                              append    error,Mailbody
.end patch 1.31
                                                                                          append    "error was ",Mailbody
                                                                                          append    error,Mailbody
                                                                                          append    CrLF,Mailbody
                                                                                          append    taskname,Mailbody
                                                                                                                                  move      "COMPUTERNAME",str50
                                                                                                                                  clock     env,str50
                                                                                                                                  append    CrLF,Mailbody
                                                                                                                                  append    str50,Mailbody
                                                                                                                                  append    CrLF,Mailbody                                                                                                                                
                                                                                          reset     Mailbody
.                                                           .                             Pack    MailBody,"Error Was ",taskname
                                                                                          Call      EmailSupport
                                                                                Else      
                                                                                          Create    dlFiles=1:10:1:10,visible=0
                                                                                          Pack      Taskname,NFTP2LocalDir,"\",NFTP2WILDCARD
                                                                                          Pack      fileDir,NFTP2LocalDir,"\"                                                                           
                                                                                      dlFiles.Dir giving result using *Filespec=Taskname,*flags=0
                                                                                          If ( result <> -1 )
                                                                                                    For NDX from 0 to result
                                                                                                              dlFiles.GetText giving FileString using *Index=NDX
.//File Prep
                                                                                                              Call        Trim Using FileString
                                                                                                              Display *P10:14,"Uploading File : ",FileString                                                                                                        
.//If record has been applied then skip - Program will slap SENT_ after it has been applied.                                      
                                                                                                              Scan      "SENT" in FileString
                                                                                                              Goto        NextFile If Equal                                                                                                     
                                                                                                              Scan      "sent" in FileString
                                                                                                              Goto        NextFile If Equal                                                                                                     
                                                                                                              
                                                                                                              MySite.Upload giving n9 using FileString
                                                                                                              If (N9 = c0)                                    
.                                                                                                                       MySite.Status giving taskname           
                                                                                                                        Pack    MAILSubjct,"Could Not Upload Files.  Status is unknown ",NFTP2RemoteDir," on ",NFTPADDRESS,b1,FileString
.//   Set the text message that is send with the attachments
                                                                                                                        Clear     Mailbody
.begin patch 1.31
				Append "Nftp0005:",Mailbody
                              append    error,Mailbody
.end patch 1.31
                                                                                                                        append    "error was ",Mailbody
                                                                                                                        append    error,Mailbody
                                                                                                                        append    CrLF,Mailbody
                                                                                                                        append    taskname,Mailbody
                                                                                                                                  move      "COMPUTERNAME",str50
                                                                                                                                  clock     env,str50
                                                                                                                                  append    CrLF,Mailbody
                                                                                                                                  append    str50,Mailbody
                                                                                                                                  append    CrLF,Mailbody                                                                                                                                
                                                                                                                        reset     Mailbody
.                                                           .                                                           Pack    MailBody,"Error Was ",taskname
                                                                                                                        Call      EmailSupport
                                                                                                                        Display *P10:16,"Upload of ",Filestring," Failed"                                                                                                               
                                                                                                              Else
                                                                                                                        Display *P10:16,"Upload of ",Filestring," Successful"                                                                                                           
                                                                                                                        Pack taskname,"SENT_",FileString
                                                                                                                        Pack taskname4,fileDir,FileString
                                                                                                                        Pack taskname5,fileDir,taskname                                                                                                             
                                                                                                                        Trap ErrorRename giving str50 if IO
                                                                                                                                  Rename taskname4,taskname5
                                                                                                                        Trapclr IO
                                                                                                                        Move NFTPCOMP,NFTPLOGCOMP                 .Company Number                                    .1-6
                                                                                                                        Move NFTP2Action,NFTPLOGAction            .Upload/Download                                   .7
                                                                                                                        Move FileString,NFTPLOGFileName           .File Name                                         .8-107
                                                                                                                        Move NFTPDESC,NFTPLOGDESC                 .Company Description                               .108-207
                                                                                                                      Move DateString,NFTPLOGDDATE              .Transaction Date 
                                                                                                                      Move "",NFTPLOGSIZE                                                                                             
                                                                                                                        CALL NFTPLOGWRT                                                                                                                   
                                                                                                              Endif
NextFile                                                                                                      
                                                                                                    repeat
                                                                                          Endif
                                                                                          Destroy dlFiles
                                                                                Endif
                                                                      Endif
                                                            Endif
.                                                 Endif
                                        Elseif (NFTP2ACTION = "D")       .Download
                              
.//Designates where the downloaded file will go
.                                                 MySite.LocalFolder giving n9 using NFTP2LocalDir                                
                                                  If (NFTP2LocalDir = "")                                               
                                                            If (NFTP2InfoType = "S")                                                        
.//Shipping - S                                                                                                                             
                                                                      Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","SHIPPING"
.//Merge - M                                                                    
                                                            Elseif (NFTP2InfoType = "M")                                                    
                                                                      Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","MERGE"                                                      
.//Order Confirmation - N                                                                                                                   
                                                            Elseif (NFTP2InfoType = "N")                                                    
                                                                      Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","OCONFIRM"                                                   
.//Statement - C
                                                            Elseif (NFTP2InfoType = "C")                                                    
                                                                      Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","STATEMENT"                                                            
.//Stats - T                                                          
                                                            Elseif (NFTP2InfoType = "T")                                                                                                                
                                                                      Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","STATS"                                                      
.//Live Order - O                                                     
                                                            Elseif (NFTP2InfoType = "O")                                                    
                                                                      Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","ORDERS"                                                     
.//Billing - B                                                        
                                                            Elseif (NFTP2InfoType = "B")                                                    
                                                                      Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","BILLING"                                                    
.//MergeCut - G                                                       
                                                            Elseif (NFTP2InfoType = "M")                                                    
                                                                      Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","MERGECUT"                                                                                                                         
.//Other - Z                                                          
                                                            Elseif (NFTP2InfoType = "Z")                                                    
                                                                      Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","OTHER"                                                      
.//Begin Patch 1.1 Code Added 
.//LOL - L                                                            
                                                            Elseif (NFTP2InfoType = "L")                                                    
                                                                      Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","LOL"                                                        
.//Begin Patch 1.1 Code Added
                                                                      
                                                            Else
.//Should Not Get Here                                                          
                                                                      Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT"                                                        
                                                            Endif
                                                  Endif
                                                  Path Exist,NFTP2LocalDir
                                                  If over
.//Check to see if the Parent Company Dir exists since the child folder does not                                                            
                                                            Pack taskname,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP			."
                                                            Path Exist,TASKNAME
                                                            If Over
                                                                      Path Create,TASKNAME
                                                                      If Over                                                                         
.//If over put files in import directory.  I know that exists.                                                                              
                                                                                Move "\\nins1\E\STORAGE\IMPORT" to NFTP2LocalDir
                                                                                Pack MAILSubjct,"Could Not Create Directory to dump files."," for ",NFTPCOMP
.//   Set the text message that is send with the attachments
                                                                                Pack    MailBody,"These files will be dumped in the parent IMPORT Directory \\nins1\E\STORAGE\IMPORT ",taskname
                                                                                Call      EmailSupport
.//   You couldn't create the parent directory - put it all in the import directory                                                                             
                                                                                Pack      NFTP2LocalDir,"\\nins1\E\STORAGE\IMPORT"                                                                                                                                                                 
                                                                                
                                                                      Else
.//Create Secondary Directory - if you succesfully make the parent directory                                                      
                                                                                Path Create,NFTP2LocalDir
                                                                                If Over
                                                                                          Move "\\nins1\E\STORAGE\IMPORT" to NFTP2LocalDir
                                                                                          Pack MAILSubjct,"Could Not Create Directory to dump files."," for ",NFTPCOMP
.//   Set the text message that is sent with the attachments
                                                                                          Pack    MailBody,"These files will be dumped in the parent IMPORT Directory \\nins1\E\STORAGE\IMPORT ",taskname
                                                                                          Call      EmailSupport
.//   You couldn't create the secondary directory - put it all in the import directory                                                                          
                                                                                          Pack      NFTP2LocalDir,"\\nins1\E\STORAGE\IMPORT"                                                                                           
                                                                                          
                                                                                Endif



                                                                      Endif
                                                            Else
.//You already check the parent if statement of the secondary directory existed - it doesn't >>Create Secondary Directory                                       
                                                                      Path Create,NFTP2LocalDir
                                                                      If Over
                                                                                Move "\\nins1\E\STORAGE\IMPORT" to NFTP2LocalDir
                                                                                Pack MAILSubjct,"Could Not Create Directory to dump files."," for ",NFTPCOMP
.//   Set the text message that is send with the attachments
                                                                                Pack    MailBody,"These files will be dumped in the parent IMPORT Directory \\nins1\E\STORAGE\IMPORT ",taskname
                                                                                Call      EmailSupport
.//   You couldn't create the secondary directory - put it all in the import directory                                                                           
                                                                                Pack      NFTP2LocalDir,"\\nins1\E\STORAGE\IMPORT"                                                                       
                                                                      Endif
                                                            Endif
.//Have to recheck to see if the above ifs were successful if not I'm putting it all in the import directory
                                                            Path Exist,NFTP2LocalDir
                                                            If over                                                     
                                                                      Pack  MAILSubjct,"Could Not Find Directory to dump files."," for ",NFTPCOMP," in Directory ",NFTP2LocalDir
.//   Set the text message that is send with the attachments
                                                                      Pack    MailBody,"These files will be dumped in the parent IMPORT Directory \\nins1\E\STORAGE\IMPORT",taskname
                                                                      Call      EmailSupport                                                                                        
                                                                      Pack      NFTP2LocalDir,"\\nins1\E\STORAGE\IMPORT"                                                                                                                                                                 
                                                            Endif                                                       
                                                  Endif



                                                  Setprop MySite,*LocalFolder=NFTP2LocalDir
.//This sections lists the files on the ftpsite
                                                  If (NFTP2RemoteDir <> "/")
.//Begin Patch 1.30          													 
	       							      Trap    TrapCaseError giving error if Object
.//End Patch 1.30          													                                                   
                                                            MySite.RemoteExists giving n9 using NFTP2RemoteDir                              
                                                            If (N9 = c0)                                    
.                                                           MySite.Status giving taskname           
                                                                      Pack    MAILSubjct,"Cannot find Remote Directory ",NFTP2RemoteDir," on ",NFTPADDRESS
.//   Set the text message that is send with the attachments
                                                                      Pack    MailBody,""
                                                                      clock     env,str50
                                                                      append    CrLF,Mailbody
                                                                      append    str50,Mailbody
                                                                      append    CrLF,Mailbody                                                                                                                                

                                                                      Call      EmailSupport
                                                            Endif
                                                  Endif
                                                  
                                                  If (N9 <> 0 or NFTP2REMOTEDIR = "/")                                            
                                                  Display *P10:8,"Working On : ",NFTPDESC                                         
                                                  Display *P10:10,"Connecting to : ",NFTPADDRESS,b5,NFTPCOMP                                                    
                                                            Display *P10:12,"Downloading Files from : ",NFTP2RemoteDir
                                                            Display *P10:14,"Destination Directory : ",NFTP2LocalDir                                                                
                                                            
                                                            Setprop MySite,*RemoteFolder=NFTP2RemoteDir         
.begin patch 1.29
                                                            Setprop MySite,*TransferType=TRANSFERTYPE
                                                            
.end patch 1.29		
							    
                                                            Move NFTP2RemoteDir,taskname1
                                                            Move "c:\work\temp_list.txt",taskname2
                                                            Move "#%NAME,#%SIZE,#%DATE",taskname3
.begin patch 1.21
                Trap    TrapCoMMFailure giving error if Object
.end patch 1.21
                                                          MySite.GetList giving n9 using taskname1,taskname2,taskname3
                                                            If (N9 = c0)                                    
.                                                                     MySite.Status giving taskname           
                                                                      Pack    MAILSubjct,"Cannot get a remote ftp listing "," on ",NFTPADDRESS
.//   Set the text message that is send with the attachments
                                                                      Pack    MailBody,""
                                                                      clock     env,str50
                                                                      append    CrLF,Mailbody
                                                                      append    str50,Mailbody
                                                                      append    CrLF,Mailbody                                                                                                                                
                                                                      Call      EmailSupport
                                                            Else
                                                                      Open FtpDirList,"c:\work\temp_list.txt"
//                                                                    Open NFTPLOGFLE3,NFTPLOGNAME,Exclusive                                          
                                                                      Loop      
                                                                                          Read FtpDirList,seq;*CDFON,taskname1,taskname2,taskname3                                                                          
                                                                      Until Over
//Test for case sensitivity                                                               
                                                                                Uppercase NFTP2WILDCARD,TASKNAME6
                                                                                Uppercase TASKNAME1,TASKNAME7
                                                                                Scan TASKNAME6 in TASKNAME7
.                                                                               Scan NFTP2WILDCARD in Taskname1
                                                                                if equal
                                                                                          Move taskname3,str10
                                                                                          Unpack str10,mm,slash,dd,slash,cc,yy
                                                                                          call cvtjul
                                                                                          if (Juldays > StartDate)
.begin patch 1.31
.                                                                                                    Pack NFTPLOGFLD2,"02X",taskname1,b55,b55
.                                                                                                    call trim using taskname1
                                                                                                    call trim using taskname1
                                                                                                    Packkey NFTPLOGFLD2,"02X",taskname1

.end patch 1.31
.                                                                                                   SQUEEZE taskname1,taskname1
                                                                                                    CALL NFTPLOGAIM
                                                                                                    If over
          											    call debug
.//Begin Patch 1.30          													 
          													 Trap    TrapCoMMFailure giving error if Object
.//End Patch 1.30          													 
                                                                                                                 	MySite.Download giving n9 using taskname1
          														If (N9 = c0)                                    
.                                                                                                                       MySite.Status giving taskname           
                                                                                                                        Pack    MAILSubjct,"Cannot download file ",taskname1," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
                                                                                                                        Clear     Mailbody
.begin patch 1.31
				Append "Nftp0005:",Mailbody
                              append    error,Mailbody
.end patch 1.31
                                                                                                                        append    "error was ",Mailbody
                                                                                                                        append    error,Mailbody
                                                                                                                        append    CrLF,Mailbody
                                                                                                                        append    taskname,Mailbody
                                                                                                                                  move      "COMPUTERNAME",str50
                                                                                                                                  clock     env,str50
                                                                                                                                  append    CrLF,Mailbody
                                                                                                                                  append    str50,Mailbody
                                                                                                                                  append    CrLF,Mailbody                                                                                                                                
                                                                                                                        reset     Mailbody
.                                                           .                                                           Pack    MailBody,"Error Was ",taskname
                                                                                                                        Call      EmailSupport
                                                                                                                        Display *P10:16,"File Download of : ",Taskname1," Failed"                                                                                                                 
                                                                                                                        
                                                                                                              Else
                                                                                                                        Display *P10:16,"File Download of : ",Taskname1," Successful"                                                                                                                       
                                                                                                                        Call Trim Using NFTP2Notification
                                                                                                                        If (NFTP2Notification <> "")
                                                                                                                                  Call      EmailNotification
                                                                                                                        Endif                                                                                                                   
                                                                                                                        Move NFTPCOMP,NFTPLOGCOMP                .Company Number                                    .1-6
                                                                                                                        Move NFTPCOMPID,NFTPLOGCOMPID                .Company Number                                    .1-6                                                                                                                        
                                                                                                                        Move NFTP2Action,NFTPLOGAction           .Upload/Download                                   .7
                                                                                                                        Move taskname1,NFTPLOGFileName           .File Name                                         .8-107
                                                                                                                        Move NFTPDESC,NFTPLOGDESC                .Company Description                               .108-207
                                                                                                                      Move taskname3,NFTPLOGDATE               .File Date                                      .208-226
                                                                                                                      Move DateString,NFTPLOGDDATE              .Transaction Date                                  .251-258
                                                                                                                      Move taskname2,NFTPLOGSIZE                                                                                      
                                                                                                                        CALL NFTPLOGWRT     
.begin patch 1.28
                Trap    TrapCoMMFailure giving error if Object
.end patch 1.28

                                                                                                                        If (NFTP2DELETE = YES)
.//Begin Patch 1.30          													 
		    													   Trap    TrapCoMMFailure giving error if Object
.//End Patch 1.30          													 
                                                                                                                           MySite.RemoteRemove giving n9 using taskname1  
                                                                                                                           If (N9 = c0)                                 
.                                                                                                                                 MySite.Status giving taskname
                                                                                                                                  Pack    MAILSubjct,"Error while deleting file off of the ftp site ",taskname1," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
                                                                                                                                  Clear     Mailbody
.begin patch 1.31
				Append "Nftp0005:",Mailbody
                              append    error,Mailbody
.end patch 1.31
                                                                                                                                  append    "error was ",Mailbody
                                                                                                                                  append    error,Mailbody
                                                                                                                                  append    CrLF,Mailbody
                                                                                                                                  append    taskname,Mailbody   
                                                                                                                                  reset     Mailbody
.                                                           .                                                                     Pack    MailBody,"Error Was ",taskname
                                                                                                                                  Call      EmailSupport
                                                                                                                           Endif
                                                                                                                        Endif                                                                                               
                                                                                                              Endif
                                                                                                    else
                                                                                                    

.Patch 1.2 Start Modification of Code
                                                                                                              Loop
                                                                                                                        CALL Trim using NFTPLOGDATE
.begin patch 1.31 trim is leaving taskname3 at position 20 should be 19                                                                                                                        
                                                                                                                        Move taskname3,str19
                                                                                                                        move	str19,taskname3
.end patch 1.31
			                                                                                                MATCH taskname3,NFTPLOGDATE                                                                                                       
                                                                                                              until equal                                                                                                                                                                                                                                               
                                                                                                                        CALL NFTPLOGKG
                                                                                                              until over
                                                                                                              repeat
                                                                                                              If (taskname3 <> NFTPLOGDATE)
.Patch 1.2 End Modification of Code                                                                                                                                                                                                                                                                                                                                               
.Patch 1.2 Start Comment Out of Code 
.                                                                                                             CALL Trim using NFTPLOGDATE
.                                                                                                             MATCH taskname3,NFTPLOGDATE
.                                                                                                             If not equal                            .download                               
.Patch 1.2 End Comment Out of Code                                                                                                                                                                                                                        
.begin patch 1.28
                Trap    TrapCoMMFailure giving error if Object
.end patch 1.28
.//Begin Patch 1.30          													 
          													 Trap    TrapCaseError giving error if Object
.//End Patch 1.30          													 
                                                                                                                 	MySite.Download giving n9 using taskname1
                                                                                                                        If (N9 = c0)                                    
.                                                                                                                                 MySite.Status giving taskname           
                                                                                                                                  Pack    MAILSubjct,"Cannot download file ",taskname1," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
                                                                                                                                  Clear     Mailbody
.begin patch 1.31
				Append "Nftp0005:",Mailbody
                              append    error,Mailbody
.end patch 1.31
                                                                                                                                  append    "error was ",Mailbody
                                                                                                                                  append    error,Mailbody
                                                                                                                                  append    CrLF,Mailbody
                                                                                                                                  append    taskname,Mailbody
                                                                                                                                  move      "COMPUTERNAME",str50
                                                                                                                                  clock     env,str50
                                                                                                                                  append    CrLF,Mailbody
                                                                                                                                  append    str50,Mailbody
                                                                                                                                  append    CrLF,Mailbody                                                                                                                                
                                                                                                                                  reset     Mailbody
.                                                           .                                                                     Pack    MailBody,"Error Was ",taskname
                                                                                                                                  Call      EmailSupport
                                                                                                                                  Display *P10:16,"File Download of : ",Taskname1," Failed"                                                                                                                                                                                                                                                   
                                                                                                                        Else
                                                                                                                                  Display *P10:16,"File Download of : ",Taskname1," Successful"                                                                                                                                                                                                                                               
                                                                                                                                  Call Trim Using NFTP2Notification
                                                                                                                                  If (NFTP2Notification <> "")
                                                                                                                                            Call      EmailNotification
                                                                                                                                  Endif
                                                                                                                                  Move NFTPCOMP,NFTPLOGCOMP                .Company Number                                    .1-6
                                                                                                                                  Move NFTP2Action,NFTPLOGAction           .Upload/Download                                   .7
                                                                                                                                  Move taskname1,NFTPLOGFileName           .File Name                                         .8-107
                                                                                                                                  Move NFTPDESC,NFTPLOGDESC                .Company Description                               .108-207
                                                                                                                        Move taskname3,NFTPLOGDATE               .Receipt Date                                      .208-226
                                                                                                                        Move taskname2,NFTPLOGSIZE                                                                                                                                                                                        
                                                                                                                                  CALL NFTPLOGWRT
                                                                                                                                  If (NFTP2DELETE = YES)
.//Begin Patch 1.30          													 
          													 		  Trap    TrapCaseError giving error if Object
.//End Patch 1.30          													                                                                                                                                   
                                                                                                                                     MySite.RemoteRemove giving n9 using taskname1  
                                                                                                                                     If (N9 = c0)                                 
.                                                                                                                                           MySite.Status giving taskname           
                                                                                                                                            Pack    MAILSubjct,"Error while deleting file off of the ftp site ",taskname1," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
                                                                                                                                            Clear     Mailbody
.begin patch 1.31
				Append "Nftp0005:",Mailbody
                              append    error,Mailbody
.end patch 1.31
                                                                                                                                            append    "error was ",Mailbody
                                                                                                                                            append    error,Mailbody
                                                                                                                                            append    CrLF,Mailbody
                                                                                                                                            append    taskname,Mailbody
                                                                                                                                  move      "COMPUTERNAME",str50
                                                                                                                                  clock     env,str50
                                                                                                                                  append    CrLF,Mailbody
                                                                                                                                  append    str50,Mailbody
                                                                                                                                  append    CrLF,Mailbody                                                                                                                                
                                                                                                                                            reset     Mailbody
.                                                           .                                                                               Pack    MailBody,"Error Was ",taskname
                                                                                                                                            Call      EmailSupport
                                                                                                                                     Endif
                                                                                                                                  Endif
                                                                                                                        Endif
                                                                                                              Endif
                                                                                                    Endif
                                                                                          Endif
                                                                                Endif
                                                                      Repeat
                                                                      Close FtpDirList
                                                            Endif
                                                  Endif
                                        Endif
                              Repeat
                    Endif
          MySite.Disconnect
NextSiteRead        
          Repeat
.         MySite.Status giving taskname
          MySite.Close                        
          Stop
          
EmailSupport
          Move "ComputerRequest@nincal.com",Mailto
          Move "ComputerRequest@nincal.com",MailFrom
          move      c10,MailTimer
          Call  SendMail                
          Return






          
EmailNotification
          Move NFTP2Notification,Mailto
          Move "ComputerRequest@nincal.com",MailFrom
          If (NFTP2Attach = YES)
                    If (NFTP2ACTION = "D")                  
                              Move "Your File has been downloaded by the NIN Data Transfer Program",MAILSubjct                                        
                              Pack MailBody with "Your File has been Downloaded to this location: ",NFTP2LocalDir,"\",Taskname1
                              Pack MailAttach,NFTP2LocalDir,"\",Taskname1
.........................
.begin patch 1.22
                              Move      c0,TrapCount                   .reset
CheckFile

                              trap      WaitForEnd giving error if IO
                              open      FileCheck,MailAttach,Exclusive          
                              Close     FIleCHeck


.end patch 1.22

                              move      c10,MailTimer

                              Call  SendMail                          
                    Endif     
          Else

                    If (NFTP2ACTION = "D")                  
                              Move "Your File has been downloaded by the NIN Data Transfer Program",MAILSubjct                                        
                              Pack MailBody with "Your File has been Downloaded to this location: ",NFTP2LocalDir,"\",Taskname1   ."
                              move      c10,MailTimer
                              Call  SendMail                          
                    Endif
          Endif

          Return
          
CleanWildCardVars
          SQUEEZE   NFTP2COMP,NFTP2COMP 
          SQUEEZE   NFTP2COMPID,NFTP2COMPID     
          SQUEEZE   NFTP2ACTION,NFTP2ACTION     
          SQUEEZE   NFTP2InfoType,NFTP2InfoType   
          SQUEEZE   NFTP2RemoteDir,NFTP2RemoteDir  
          SQUEEZE   NFTP2WILDCARD,NFTP2WILDCARD   
          SQUEEZE   NFTP2LocalDir,NFTP2LocalDir   
          SQUEEZE   NFTP2DELETE,NFTP2DELETE     
          SQUEEZE   NFTP2DAILY,NFTP2DAILY      
          SQUEEZE   NFTP2Filler,NFTP2Filler     
          Return
          
ErrorRename
          Pack    MAILSubjct,"NFTP0005 - rename error"
          Clear     MailBody
          Append    "Could Not Rename File.  Status is unknown ",Mailbody
          Append    fileDir,Mailbody
          Append    " for ",mailbody
          Append    b1,mailbody
          Append    FileString,MailBody
          Append    CRLF,Mailbody
          Append              "Error Was ",MailBody
          Append    str50,MailBody
          move      "COMPUTERNAME",str50
          clock     env,str50
          append    CrLF,Mailbody
          append    str50,Mailbody
          append    CrLF,Mailbody                                                                                                                                
          reset     MailBody
          Call      EmailSupport        

          Return
          
TrapCaseError
	 GETINFO   EXCEPTION,OLEERROR
         return
         
TrapConnectFailure
          Move C0 to N9
          return
.begin patch 1.21
.11/12/07 added more details to message
TrapCoMMFailure
.dh test
                              GETINFO   EXCEPTION,objerror
 
.dh test
                              Pack    MAILSubjct,"NFtp005 Error "
                              //   Set the text message that is send with the attachments
                              Clear     Mailbody
.begin patch 1.31
				Append "Nftp0005:",Mailbody
                              append    error,Mailbody
.end patch 1.31
                              Append    "CuteFTP Error Connect to ",MailBody
                              Append    NFTPADDRESS,MailBody
                              append    CrLF,Mailbody
                              append    "error was ",Mailbody
                              append    error,Mailbody
                              append    CrLF,Mailbody
                              append    taskname,Mailbody
                              append    CrLF,Mailbody
                              Append    "Downloading Files from : ",MailBody
                              Append    NFTP2RemoteDir,MailBody
                              Append    CRLF,MailBody
                              Append    "Destination Directory : ",MailBody
                              Append    NFTP2LocalDir,MailBody                                                        
                              Append    CRLF,MailBody
                              Append    "Taskname1 : ",MailBody
                              Append    CRLF,MailBody
                              append    taskname1,Mailbody
                              append    CrLF,Mailbody
                              Append    "Taskname2 : ",MailBody
                              Append    CRLF,MailBody
                              append    taskname2,Mailbody
                              append    CrLF,Mailbody
                              Append    "Taskname3 : ",MailBody
                              Append    CRLF,MailBody
                              append    taskname3,Mailbody
                              append    CrLF,Mailbody
                              Append    "Job Aborted",Mailbody
                              append    CrLF,Mailbody
                              move      "COMPUTERNAME",str50
                              clock     env,str50
                              append    CrLF,Mailbody
                              append    str50,Mailbody
                              append    CrLF,Mailbody    
                              append    objerror,mailbody
                              append    CrLF,Mailbody    
                              reset     Mailbody
.                             Pack    MailBody,"Error Was ",taskname
.//                              Call      EmailSupport
.dh goes bad
                              return
.end dh
          Stop
.end patch 1.21
.begin patch 1.22
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Nftp0005 - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "dherric@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
                    Move      "dherric@nincal.com",MailTO
.begin patch 1.31
				Append "Nftp0005:",Mailbody
                              append    error,Mailbody
.end patch 1.31
                    append    CRLF,MailBOdy
                    append    mailattach,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    move      c10,MailTimer
                    call      SendMail
                    return
                    endif
          
                    goto      checkfile

.end patch 1.22


          Include nftpio.inc
          Include nftp2io.inc
          Include nftplogio.inc
          Include Comlogic.inc