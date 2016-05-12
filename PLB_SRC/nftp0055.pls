//Program information
//Program Name:NFTP0001.PLS
//Program Description:This job will post or download files from an ftp site from one of our partners/clients etc.  And Place them in a specific folder.
//Associated Winbatch Job:  FTP.WBT
//Programmer Name:David Baca
//Original Release Date:June 12, 2006

//I have added a startdate to prevent the downloading of stale files that have already been applied prior to this programs release.
//Program will dump file into \\nins1\e\data\storage [ship/merge] location depending on the var NFTP2InfoType i have created individual folders for each company

//Set CuteFTP to use smart overwrite - overwrite when it hits a dupe.  You must do through the interface.


	Include Common.inc
	Include Cons.inc
	Include nftpdd.inc
	Include nftp2dd.inc
	Include nftplogdd.inc


PC EQU 0
Release 	Init "1.2.1"  18OCT2007	DLH	additional error infor at Object trap
REldate	INit	"18 October 2007"
.Release Init "1.2"  30OCT2006	DMB	Fixed bug that did not take into account dupes.  Date check will now check all instances of that filename for date/time
.Release Init "1.1"  26OCT2006	DMB	Added File Type L=LOL
.Release Init "1.0" 12JUN2006	DMB	Program Initial Release

FtpDirList          File                  .Temporary Directory Listing

Taskname1  DIM 100
Taskname2  DIM 100
Taskname3  DIM 100
Taskname4  DIM 510
Taskname5  DIM 510
Taskname6  DIM 100
Taskname7  DIM 100

StartDate  FORM 5
FirstCall  FORM 1

dlFiles       datalist
FileString DIM 255
fileDir	   DIM 255
NDX        FORM 9
DateString DIM 10
	clock timestamp,timestamp
	unpack timestamp,str2,yy,mm,dd
	pack DateString,mm,slash,dd,slash,cc,yy

	Move 	"09",MM
	Move 	"16",DD	
	Move 	"06",YY
	Call    cvtjul	
	Move    juldays  to StartDate
	Call	Paint
	Open NFTPFILE,NFTPNAME

//Create the Automation Object for Cute Ftp - Current Version is 6.0
MYSite     automation      class="CuteFTPPro.TEConnection"
	create  MySite

	Loop
		Read NFTPFILE,seq;NFTPVARS
	Until Over
		SQUEEZE NFTPPROTOCOL,NFTPPROTOCOL	
		Goto NextSiteRead IF (NFTPPROTOCOL = "SMTP")	
		move c1 to FirstCall		. Reset the ReadFlag	
//This section logs onto the ftp site
		Setprop MySite,*Protocol=NFTPPROTOCOL
		SQUEEZE NFTPADDRESS,NFTPADDRESS		
		Setprop MySite,*Host=NFTPADDRESS
		SQUEEZE NFTPUSERNAME,NFTPUSERNAME			
		Setprop MySite,*Login=NFTPUSERNAME
		SQUEEZE NFTPPASSWORD,NFTPPASSWORD				
		Setprop MySite,*Password=NFTPPASSWORD
         pause     "5"

                Trap    TrapConnectFailure giving error if Object
			MySite.Connect giving N9
                Trapclr Object				
.begin patch 1.21
                Trap    TrapCoMMFailure giving error if Object
.end patch 1.21
		If (N9 = c0)
.			MySite.Status giving taskname	
			Pack    MAILSubjct,"Could Not Connect to ",NFTPADDRESS
			//   Set the text message that is send with the attachments
			Clear	Mailbody
			append	"error was ",Mailbody
			append 	error,Mailbody
			append	CrLF,Mailbody
			append	taskname,Mailbody
			reset	Mailbody
.			Pack    MailBody,"Error Was ",taskname
			Call	EmailSupport
			Display *P10:8,"Working On : ",NFTPDESC					
			Display *P10:10,"Connecting to : ",NFTPADDRESS,b5,NFTPCOMP			 			
		Else
//We must check for mutiple file type for this site		
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
//This section will do one of two options Based upon NFTPAction either Upload or Download
************************************************************************************************************
//Upload is pretty straight forward - it check both the local an remote directories to make sure they exist
//If they both exist then it posts the file
************************************************************************************************************
//Download is a little more complex
//Download grabs a remote directory listing of the ftp site.
//It sequentially reads through this file to see if it's gets a match on the ftplog file.  If not it downloads the file.
//If it does get a match it then does a compare to see if the date/or times do not match.  If they do not it downloads the file else do nothing
//Lastly the download portion has the option of deleting the file off the ftp site.
************************************************************************************************************
				If (NFTP2ACTION = "U")          .Upload
//Designates where the downloaded file will go
.					If (NFTP2InfoType = "O")													
.						If (NFTP2LocalDir = "")
.							Pack NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","ORDERS"
.						Endif
.					Endif
.					If (NFTP2LocalDir = "")
.						Pack    SmtpSubject,"No Local Directory Specified for this Upload",NFTP2RemoteDir," on ",NFTPADDRESS
.						Pack    SmtpTextMessage(1),"Please Verify for Company: ",NFTPCOMP,b1,NFTP2WILDCARD						
.						Call	EmailSupport							
.					Else
					If (NFTP2LocalDir = "")
						If (NFTP2InfoType = "O")													
							Pack	taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","ORDERS\",NFTP2WILDCARD
							Pack	fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","ORDERS\"
							Pack 	NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","ORDERS"							
						ElseIf (NFTP2InfoType = "S")													
							Pack	taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","SHIPPING\",NFTP2WILDCARD
							Pack	fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","SHIPPING\"								
							Pack 	NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","SHIPPING"							
						ElseIf (NFTP2InfoType = "M")													
							Pack	taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","MERGE\",NFTP2WILDCARD
							Pack	fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","MERGE\"							
							Pack 	NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","MERGE"							
						ElseIf (NFTP2InfoType = "N")													
							Pack	taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","OCONFIRM\",NFTP2WILDCARD
							Pack	fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","OCONFIRM\"							
							Pack 	NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","OCONFIRM"							
						ElseIf (NFTP2InfoType = "C")													
							Pack	taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","STATEMENT\",NFTP2WILDCARD
							Pack	fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","STATEMENT\"							
							Pack 	NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","STATEMENT"							
						ElseIf (NFTP2InfoType = "T")													
							Pack	taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","STATS\",NFTP2WILDCARD
							Pack	fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","STATS\"																		
							Pack 	NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","STATS"							
						ElseIf (NFTP2InfoType = "B")													
							Pack	taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","BILLING\",NFTP2WILDCARD
							Pack	fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","BILLING\"																		
							Pack 	NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","BILLING"							
						ElseIf (NFTP2InfoType = "G")													
							Pack	taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","MergeSum\",NFTP2WILDCARD
							Pack	fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","MergeSum\"																		
							Pack 	NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","MergeSum"							
						ElseIf (NFTP2InfoType = "Z")													
							Pack	taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","OTHER\",NFTP2WILDCARD
							Pack	fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","OTHER\"																		
							Pack 	NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","OTHER"							
//Begin Patch 1.1 Code Added
						ElseIf (NFTP2InfoType = "L")													
							Pack	taskname,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","LOL\",NFTP2WILDCARD
							Pack	fileDir,"\\nins1\E\STORAGE\EXPORT\",NFTPCOMP,"\","LOL\"																		
							Pack 	NFTP2LocalDir,"\\nins1\e\STORAGE\EXPORT\",NFTPCOMP,"\","LOL"							
//End Patch 1.1 Code Added
						Endif
					Endif	
					
				
					
						Path Exist,NFTP2LocalDir
						If Over
							Pack    MAILSubjct,"Local Directory Does not exist for ",NFTP2LocalDir," NFTPCOMP"
//   Set the text message that is send with the attachments
							Pack    MailBody," ",taskname
							Call	EmailSupport			
						Else
							Setprop MySite,*LocalFolder=NFTP2LocalDir
//This sections lists the files on the ftpsite		
							Display *P10:12,"Uploading Files to : ",NFTP2RemoteDir	
							If (NFTP2RemoteDir <> "/")
								MySite.RemoteExists giving n9 using NFTP2RemoteDir						
								If (N9 = c0)			        
.									MySite.Status giving taskname	
									Pack    MAILSubjct,"Could Not Find to Remote Directory ",NFTP2RemoteDir," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
									Clear	Mailbody
									append	"error was ",Mailbody
									append 	error,Mailbody
									append	CrLF,Mailbody
									append	taskname,Mailbody
									reset	Mailbody
.						.			Pack    MailBody,"Error Was ",taskname
									Call	EmailSupport
								Endif
							Endif
							If (N9 <> 0 or NFTP2REMOTEDIR = "/")
								Setprop MySite,*RemoteFolder=NFTP2RemoteDir							
								If (N9 = c0)			        
.									MySite.Status giving taskname	
									Pack    MAILSubjct,"Could Not Relocate to Remote Directory ",NFTP2RemoteDir," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
									Clear	Mailbody
									append	"error was ",Mailbody
									append 	error,Mailbody
									append	CrLF,Mailbody
									append	taskname,Mailbody
									reset	Mailbody
.						.			Pack    MailBody,"Error Was ",taskname
									Call	EmailSupport
								Else	
									Create	dlFiles=1:10:1:10,visible=0
									Pack	Taskname,NFTP2LocalDir,"\",NFTP2WILDCARD
									Pack	fileDir,NFTP2LocalDir,"\"								
							                dlFiles.Dir giving result using *Filespec=Taskname,*flags=0
	              							If ( result <> -1 )
										For NDX from 0 to result
											dlFiles.GetText giving FileString using *Index=NDX
//File Prep
											Call 	  Trim Using FileString
											Display *P10:14,"Uploading File : ",FileString											
//If record has been applied then skip	- Program will slap SENT_ after it has been applied.				
											Scan      "SENT" in FileString
											Goto 	  NextFile If Equal											
											
											MySite.Upload giving n9 using FileString
											If (N9 = c0)			        
.												MySite.Status giving taskname		
												Pack    MAILSubjct,"Could Not Upload Files.  Status is unknown ",NFTP2RemoteDir," on ",NFTPADDRESS,b1,FileString
//   Set the text message that is send with the attachments
												Clear	Mailbody
												append	"error was ",Mailbody
												append 	error,Mailbody
												append	CrLF,Mailbody
												append	taskname,Mailbody
												reset	Mailbody
.						.						Pack    MailBody,"Error Was ",taskname
												Call	EmailSupport
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
.					Endif
				Elseif (NFTP2ACTION = "D")       .Download
			
//Designates where the downloaded file will go
.					MySite.LocalFolder giving n9 using NFTP2LocalDir				
					If (NFTP2LocalDir = "")					
						If (NFTP2InfoType = "S")						
//Shipping - S													
							Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","SHIPPING"
//Merge - M							
						Elseif (NFTP2InfoType = "M")						
							Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","MERGE"						
//Order Confirmation - N												
						Elseif (NFTP2InfoType = "N")						
							Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","OCONFIRM"						
//Statement - C
						Elseif (NFTP2InfoType = "C")						
							Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","STATEMENT"						
//Stats - T						
						Elseif (NFTP2InfoType = "T")												
							Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","STATS"						
//Live Order - O						
						Elseif (NFTP2InfoType = "O")						
							Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","ORDERS"						
//Billing - B						
						Elseif (NFTP2InfoType = "B")						
							Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","BILLING"						
//MergeCut - G						
						Elseif (NFTP2InfoType = "M")						
							Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","MERGECUT"													
//Other - Z						
						Elseif (NFTP2InfoType = "Z")						
							Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","OTHER"						
//Begin Patch 1.1 Code Added 
//LOL - L						
						Elseif (NFTP2InfoType = "L")						
							Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP,"\","LOL"						
//Begin Patch 1.1 Code Added
							
						Else
//Should Not Get Here						
							Pack NFTP2LocalDir,"\\nins1\e\STORAGE\IMPORT"						
						Endif
					Endif
					Path Exist,NFTP2LocalDir
					If over
//Check to see if the Parent Company Dir exists since the child folder does not						
						Pack taskname,"\\nins1\e\STORAGE\IMPORT\",NFTPCOMP
						Path Exist,TASKNAME
						If Over
							Path Create,TASKNAME
							If Over								
//If over put files in import directory.  I know that exists.								
								Move "\\nins1\E\STORAGE\IMPORT" to NFTP2LocalDir
								Pack MAILSubjct,"Could Not Create Directory to dump files."," for ",NFTPCOMP
//   Set the text message that is send with the attachments
								Pack    MailBody,"These files will be dumped in the parent IMPORT Directory \\nins1\E\STORAGE\IMPORT ",taskname
								Call	EmailSupport
//   You couldn't create the parent directory - put it all in the import directory								
								Pack 	NFTP2LocalDir,"\\nins1\E\STORAGE\IMPORT"																	
								
							Else
//Create Secondary Directory - if you succesfully make the parent directory						
								Path Create,NFTP2LocalDir
								If Over
									Move "\\nins1\E\STORAGE\IMPORT" to NFTP2LocalDir
									Pack MAILSubjct,"Could Not Create Directory to dump files."," for ",NFTPCOMP
//   Set the text message that is sent with the attachments
									Pack    MailBody,"These files will be dumped in the parent IMPORT Directory \\nins1\E\STORAGE\IMPORT ",taskname
									Call	EmailSupport
//   You couldn't create the secondary directory - put it all in the import directory								
									Pack 	NFTP2LocalDir,"\\nins1\E\STORAGE\IMPORT"										
									
								Endif



							Endif
						Else
//You already check the parent if statement of the secondary directory existed - it doesn't >>Create Secondary Directory				
							Path Create,NFTP2LocalDir
							If Over
								Move "\\nins1\E\STORAGE\IMPORT" to NFTP2LocalDir
								Pack MAILSubjct,"Could Not Create Directory to dump files."," for ",NFTPCOMP
//   Set the text message that is send with the attachments
								Pack    MailBody,"These files will be dumped in the parent IMPORT Directory \\nins1\E\STORAGE\IMPORT ",taskname
								Call	EmailSupport
//   You couldn't create the secondary directory - put it all in the import directory								
								Pack 	NFTP2LocalDir,"\\nins1\E\STORAGE\IMPORT"								
							Endif
						Endif
//Have to recheck to see if the above ifs were successful if not I'm putting it all in the import directory
						Path Exist,NFTP2LocalDir
						If over						
							Pack  MAILSubjct,"Could Not Find Directory to dump files."," for ",NFTPCOMP," in Directory ",NFTP2LocalDir
//   Set the text message that is send with the attachments
							Pack    MailBody,"These files will be dumped in the parent IMPORT Directory \\nins1\E\STORAGE\IMPORT",taskname
							Call	EmailSupport									
							Pack 	NFTP2LocalDir,"\\nins1\E\STORAGE\IMPORT"																	
						Endif						
					Endif



					Setprop MySite,*LocalFolder=NFTP2LocalDir
//This sections lists the files on the ftpsite
					If (NFTP2RemoteDir <> "/")
						MySite.RemoteExists giving n9 using NFTP2RemoteDir			
						If (N9 = c0)			        
.						MySite.Status giving taskname		
							Pack    MAILSubjct,"Cannot find Remote Directory ",NFTP2RemoteDir," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
							Pack    MailBody,""
							Call	EmailSupport
						Endif
					Endif
					
					If (N9 <> 0 or NFTP2REMOTEDIR = "/")					
						Display *P10:12,"Downloading Files from : ",NFTP2RemoteDir
						Display *P10:14,"Destination Directory : ",NFTP2LocalDir							
						
						Setprop MySite,*RemoteFolder=NFTP2RemoteDir					
						Move NFTP2RemoteDir,taskname1
						Move "c:\work\temp_list.txt",taskname2
						Move "#%NAME,#%SIZE,#%DATE",taskname3
					        MySite.GetList giving n9 using taskname1,taskname2,taskname3
						If (N9 = c0)			        
.							MySite.Status giving taskname		
							Pack    MAILSubjct,"Cannot get a remote ftp listing "," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
							Pack    MailBody,""
							Call	EmailSupport
						Else
							Open FtpDirList,"c:\work\temp_list.txt"
//							Open NFTPLOGFLE3,NFTPLOGNAME,Exclusive					
							Loop 	
									Read FtpDirList,seq;*CDFON,taskname1,taskname2,taskname3								
							Until Over
//Test for case sensitivity							
								Uppercase NFTP2WILDCARD,TASKNAME6
								Uppercase TASKNAME1,TASKNAME7
								Scan TASKNAME6 in TASKNAME7
.								Scan NFTP2WILDCARD in Taskname1
								if equal
									Move taskname3,str10
									Unpack str10,mm,slash,dd,slash,cc,yy
									call cvtjul
									if (Juldays > StartDate)
										Pack NFTPLOGFLD2,"02X",taskname1,b55,b55
										call trim using taskname1
.										SQUEEZE taskname1,taskname1
										CALL NFTPLOGAIM
										If over
									    		MySite.Download giving n9 using taskname1
											If (N9 = c0)			        
.												MySite.Status giving taskname		
												Pack    MAILSubjct,"Cannot download file ",taskname1," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
												Clear	Mailbody
												append	"error was ",Mailbody
												append 	error,Mailbody
												append	CrLF,Mailbody
												append	taskname,Mailbody
												reset	Mailbody
.						.						Pack    MailBody,"Error Was ",taskname
												Call	EmailSupport
												Display *P10:16,"File Download of : ",Taskname1," Failed"												
												
											Else
												Display *P10:16,"File Download of : ",Taskname1," Successful"												
												Call Trim Using NFTP2Notification
												If (NFTP2Notification <> "")
													Call	EmailNotification
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
												If (NFTP2DELETE = YES)
												   MySite.RemoteRemove giving n9 using taskname1	
											   	   If (N9 = c0)			        
.													MySite.Status giving taskname
													Pack    MAILSubjct,"Error while deleting file off of the ftp site ",taskname1," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
													Clear	Mailbody
													append	"error was ",Mailbody
													append 	error,Mailbody
													append	CrLF,Mailbody
													append	taskname,Mailbody	
													reset	Mailbody
.						.							Pack    MailBody,"Error Was ",taskname
													Call	EmailSupport
												   Endif
												Endif										
											Endif
								    		else
								    		

.Patch 1.2 Start Modification of Code
											Loop
												CALL Trim using NFTPLOGDATE
												MATCH taskname3,NFTPLOGDATE											
											until equal																								
												CALL NFTPLOGKG
											until over
											repeat
											If (taskname3 <> NFTPLOGDATE)
.Patch 1.2 End Modification of Code								    																										
.Patch 1.2 Start Comment Out of Code 
.											CALL Trim using NFTPLOGDATE
.											MATCH taskname3,NFTPLOGDATE
.											If not equal			.download		    		
.Patch 1.2 End Comment Out of Code																						
								    				MySite.Download giving n9 using taskname1	
												If (N9 = c0)			        
.													MySite.Status giving taskname		
													Pack    MAILSubjct,"Cannot download file ",taskname1," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
													Clear	Mailbody
													append	"error was ",Mailbody
													append 	error,Mailbody
													append	CrLF,Mailbody
													append	taskname,Mailbody
													reset	Mailbody
.						.							Pack    MailBody,"Error Was ",taskname
													Call	EmailSupport
													Display *P10:16,"File Download of : ",Taskname1," Failed"																									
												Else
													Display *P10:16,"File Download of : ",Taskname1," Successful"																								
													Call Trim Using NFTP2Notification
													If (NFTP2Notification <> "")
														Call	EmailNotification
													Endif
													Move NFTPCOMP,NFTPLOGCOMP                .Company Number                                    .1-6
													Move NFTP2Action,NFTPLOGAction           .Upload/Download                                   .7
													Move taskname1,NFTPLOGFileName           .File Name                                         .8-107
													Move NFTPDESC,NFTPLOGDESC                .Company Description                               .108-207
											        	Move taskname3,NFTPLOGDATE               .Receipt Date                                      .208-226
											        	Move taskname2,NFTPLOGSIZE   																			
													CALL NFTPLOGWRT
													If (NFTP2DELETE = YES)
													   MySite.RemoteRemove giving n9 using taskname1	
												   	   If (N9 = c0)			        
.														MySite.Status giving taskname		
														Pack    MAILSubjct,"Error while deleting file off of the ftp site ",taskname1," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
														Clear	Mailbody
														append	"error was ",Mailbody
														append 	error,Mailbody
														append	CrLF,Mailbody
														append	taskname,Mailbody
														reset	Mailbody
.						.								Pack    MailBody,"Error Was ",taskname
														Call	EmailSupport
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
.	MySite.Status giving taskname
	MySite.Close
	Stop
	
EmailSupport
	Move "ComputerRequest@nincal.com",Mailto
	Move "ComputerRequest@nincal.com",MailFrom
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
			Call  SendMail			
		Endif	
	Else

		If (NFTP2ACTION = "D")		
			Move "Your File has been downloaded by the NIN Data Transfer Program",MAILSubjct				
			Pack MailBody with "Your File has been Downloaded to this location: ",NFTP2LocalDir,"\",Taskname1   ."
			Call  SendMail			
		Endif
	Endif

	Return
	
CleanWildCardVars
	SQUEEZE	NFTP2COMP,NFTP2COMP	
	SQUEEZE	NFTP2COMPID,NFTP2COMPID     
	SQUEEZE	NFTP2ACTION,NFTP2ACTION     
	SQUEEZE	NFTP2InfoType,NFTP2InfoType   
	SQUEEZE	NFTP2RemoteDir,NFTP2RemoteDir  
	SQUEEZE	NFTP2WILDCARD,NFTP2WILDCARD   
	SQUEEZE	NFTP2LocalDir,NFTP2LocalDir   
	SQUEEZE	NFTP2DELETE,NFTP2DELETE     
	SQUEEZE	NFTP2DAILY,NFTP2DAILY      
	SQUEEZE	NFTP2Filler,NFTP2Filler     
	Return
	
ErrorRename
	Pack    MAILSubjct,"NFTP0005 - rename error"
	Clear	MailBody
	Append	"Could Not Rename File.  Status is unknown ",Mailbody
	Append	fileDir,Mailbody
	Append	" for ",mailbody
	Append	b1,mailbody
	Append	FileString,MailBody
	Append	CRLF,Mailbody
	Append    	"Error Was ",MailBody
	Append	str50,MailBody
	reset	MailBody
	Call	EmailSupport	

	Return
	
TrapConnectFailure
	Move C0 to N9
	return
.begin patch 1.21
.11/12/07 added more details to message
TrapCoMMFailure

			Pack    MAILSubjct,"NFtp005 Error "
			//   Set the text message that is send with the attachments
			Clear	Mailbody
			Append	"CuteFTP Error Connect to ",MailBody
			Append	NFTPADDRESS,MailBody
			append	CrLF,Mailbody
			append	"error was ",Mailbody
			append 	error,Mailbody
			append	CrLF,Mailbody
			append	taskname,Mailbody
			append	CrLF,Mailbody
			Append	"Downloading Files from : ",MailBody
			Append	NFTP2RemoteDir,MailBody
			Append	CRLF,MailBody
			Append	"Destination Directory : ",MailBody
			Append	NFTP2LocalDir,MailBody                                                        
			Append	CRLF,MailBody
			Append	"Taskname1 : ",MailBody
			Append	CRLF,MailBody
			append	taskname1,Mailbody
			append	CrLF,Mailbody
			Append	"Taskname2 : ",MailBody
			Append	CRLF,MailBody
			append	taskname2,Mailbody
			append	CrLF,Mailbody
			Append	"Taskname3 : ",MailBody
			Append	CRLF,MailBody
			append	taskname3,Mailbody
			append	CrLF,Mailbody
			Append	"Job Aborted",Mailbody
			append	CrLF,Mailbody
			reset	Mailbody
.			Pack    MailBody,"Error Was ",taskname
			Call	EmailSupport
	Stop
.end patch 1.21

	Include nftpio.inc
	Include nftp2io.inc
	Include nftplogio.inc
	Include Comlogic.inc