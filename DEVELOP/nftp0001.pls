//Program information
//Program Name:NFTP0001.PLS
//Program Description:This job will post or download files from an ftp site from one of our partners/clients etc.  And Place them in a specific folder.
//Associated Winbatch Job:  FTP.WBT
//Programmer Name:David Baca
//Original Release Date:June 12, 2006

//I have added a startdate to prevent the downloading of stale files that have already been applied prior to this programs release.
//Program will dump file into \\nts1\e\data\storage [ship/merge] location depending on the var NFTP2InfoType i have created individual folders for each company

//Set CuteFTP to use smart overwrite - overwrite when it hits a dupe.  You must do through the interface.


	Include Common.inc
	Include Cons.inc
	Include nftpdd.inc
	Include nftp2dd.inc
	Include nftplogdd.inc


PC EQU 0

Release Init "1.0" 12JUN2006	DMB	Program Initial Release

FtpDirList          File                  .Temporary Directory Listing

Taskname1  DIM 100
Taskname2  DIM 100
Taskname3  DIM 100
Taskname4  DIM 510
Taskname5  DIM 510

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

	Move 	"06",MM
	Move 	"19",DD	
	Move 	"06",YY
	Call   cvtjul	
	Move   juldays  to StartDate

	Open NFTPFILE,NFTPNAME

//Create the Automation Object for Cute Ftp - Current Version is 6.0
MYSite     automation      class="CuteFTPPro.TEConnection"
	create  MySite

	Loop
		Read NFTPFILE,seq;NFTPVARS
	Until Over
	Until (NFTPCOMP = "")
		move c1 to FirstCall		. Reset the ReadFlag	
//This section logs onto the ftp site
		SQUEEZE NFTPPROTOCOL,NFTPPROTOCOL
		Setprop MySite,*Protocol=NFTPPROTOCOL
		SQUEEZE NFTPADDRESS,NFTPADDRESS		
		Setprop MySite,*Host=NFTPADDRESS
		SQUEEZE NFTPUSERNAME,NFTPUSERNAME			
		Setprop MySite,*Login=NFTPUSERNAME
		SQUEEZE NFTPPASSWORD,NFTPPASSWORD				
		Setprop MySite,*Password=NFTPPASSWORD
		MySite.Connect giving N9
		If (N9 = c0)
			MySite.Status giving taskname		
			Pack    SmtpSubject,"Could Not Connect to ",NFTPADDRESS
			//   Set the text message that is send with the attachments
			Pack    SmtpTextMessage(1),"Error Was ",taskname
			Call	EmailSupport
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
					If (NFTP2InfoType = "O")													
						If (NFTP2LocalDir = "")
							Pack NFTP2LocalDir,"\\nts1\e\STORAGE\EXPORT\",NFTPCOMP,"\","ORDERS"
						Endif
					Endif
					If (NFTP2LocalDir = "")
						Pack    SmtpSubject,"No Local Directory Specified for this Upload",NFTP2RemoteDir," on ",NFTPADDRESS
						Pack    SmtpTextMessage(1),"Please Verify for Company: ",NFTPCOMP,b1,NFTP2WILDCARD						
						Call	EmailSupport							
					Else
						Path Exist,NFTP2LocalDir
						If Over
							Pack    SmtpSubject,"Local Directory Does not exist for ",NFTP2LocalDir," NFTPCOMP"
//   Set the text message that is send with the attachments
							Pack    SmtpTextMessage(1)," ",taskname
							Call	EmailSupport			
						Else
							Setprop MySite,*LocalFolder=NFTP2LocalDir
//This sections lists the files on the ftpsite			
							MySite.RemoteExists giving n9 using NFTP2RemoteDir						
							If (N9 = c0)			        
								MySite.Status giving taskname		
								Pack    SmtpSubject,"Could Not Find to Remote Directory ",NFTP2RemoteDir," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
								Pack    SmtpTextMessage(1),"Error Was ",taskname
								Call	EmailSupport
							Else					
								Setprop MySite,*RemoteFolder=NFTP2RemoteDir							
								If (N9 = c0)			        
									MySite.Status giving taskname		
									Pack    SmtpSubject,"Could Not Relocate to Remote Directory ",NFTP2RemoteDir," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
									Pack    SmtpTextMessage(1),"Error Was ",taskname
									Call	EmailSupport
								Else	
									Create	dlFiles=1:10:1:10,visible=0
									If (NFTP2LocalDir = "")
										If (NFTP2InfoType = "O")													
											Pack	taskname,"\\Nts1\E\STORAGE\EXPORT\",NFTPCOMP,"\","ORDERS\",NFTP2WILDCARD
											Pack	fileDir,"\\Nts1\E\STORAGE\EXPORT\",NFTPCOMP,"\","ORDERS\"								
										Else
											Pack    SmtpSubject,"No Local Directory Specified for this Upload",NFTP2RemoteDir," on ",NFTPADDRESS
											Pack    SmtpTextMessage(1),"Please Verify for Company: ",NFTPCOMP
											Call	EmailSupport
										Endif									
									Else
											Pack	Taskname,NFTP2LocalDir,"\",NFTP2WILDCARD
											Pack	fileDir,NFTP2LocalDir,"\"								
									Endif
							                dlFiles.Dir giving result using *Filespec=Taskname,*flags=0
	              							If ( result <> -1 )
										For NDX from 0 to result
											dlFiles.GetText giving FileString using *Index=NDX
											MySite.Upload giving n9 using FileString
											If (N9 = c0)			        
												MySite.Status giving taskname		
												Pack    SmtpSubject,"Could Not Upload Files.  Status is unknown ",NFTP2RemoteDir," on ",NFTPADDRESS,b1,FileString
//   Set the text message that is send with the attachments
												Pack    SmtpTextMessage(1),"Error Was ",taskname
												Call	EmailSupport
											Else
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
										repeat
									Endif
									Destroy dlFiles
								Endif
							Endif
						Endif
					Endif
				Elseif (NFTP2ACTION = "D")       .Download
			
//Designates where the downloaded file will go
.					MySite.LocalFolder giving n9 using NFTP2LocalDir				
					If (NFTP2InfoType = "S")	
						If (NFTP2LocalDir = "")					
					
							Pack NFTP2LocalDir,"\\nts1\e\STORAGE\IMPORT\",NFTPCOMP,"\","SHIPPING"
							Path Exist,NFTP2LocalDir
							If over
//Create Parent Company Dir						
								Pack taskname,"\\nts1\e\STORAGE\IMPORT\",NFTPCOMP
								Path Exist,TASKNAME
								If Over
									Path Create,TASKNAME
									If Over								
//If over put files in import directory.  I know that exists.								
										Move "\\nts1\E\STORAGE\IMPORT" to NFTP2LocalDir
										Pack SmtpSubject,"Could Not Create Directory to dump files."," for ",NFTPCOMP
//   Set the text message that is send with the attachments
										Pack    SmtpTextMessage(1),"These files will be dumped in the parent IMPORT Directory \\nts1\E\STORAGE\IMPORT",taskname
										Call	EmailSupport																									
									Endif
								Else
//Create Shipping DIrectory						
									Path Create,NFTP2LocalDir
									If Over
										Move "\\nts1\E\STORAGE\IMPORT" to NFTP2LocalDir
										Pack SmtpSubject,"Could Not Create Directory to dump files."," for ",NFTPCOMP
//   Set the text message that is send with the attachments
										Pack    SmtpTextMessage(1),"These files will be dumped in the parent IMPORT Directory \\nts1\E\STORAGE\IMPORT",taskname
										Call	EmailSupport																
									Endif
								Endif		
							Else
								Pack	Taskname,NFTP2LocalDir
								Path Exist,TASKNAME
								If Over
									Path Create,TASKNAME
									If Over								
//If over put files in import directory.  I know that exists.															
										Move "\\nts1\E\STORAGE\IMPORT" to NFTP2LocalDir
										Pack SmtpSubject,"Could Not Create Directory to dump files."," for ",NFTPCOMP
//   Set the text message that is send with the attachments
										Pack    SmtpTextMessage(1),"These files will be dumped in the parent IMPORT Directory \\nts1\E\STORAGE\IMPORT",taskname
										Call	EmailSupport																									
									Endif														
								Endif								
							Endif
						Endif
					ElseIf (NFTP2InfoType = "M")
						If (NFTP2LocalDir = "")										
							Pack NFTP2LocalDir,"\\nts1\e\STORAGE\IMPORT\",NFTPCOMP,"\","MERGE"					
							Path Exist,NFTP2LocalDir
							If over
//Create Parent Company Dir						
								Pack Taskname,"\\nts1\e\STORAGE\IMPORT\",NFTPCOMP
								Path Exist,TASKNAME
								If Over
									Path Create,TASKNAME
									If Over								
//If over put files in import directory.  I know that exists.															
										Move "\\nts1\E\STORAGE\IMPORT" to NFTP2LocalDir
										Pack SmtpSubject,"Could Not Create Directory to dump files."," for ",NFTPCOMP
//   Set the text message that is send with the attachments
										Pack    SmtpTextMessage(1),"These files will be dumped in the parent IMPORT Directory \\nts1\E\STORAGE\IMPORT",taskname
										Call	EmailSupport																									
									Endif						
								Else
									Path Create,NFTP2LocalDir
									If Over
										Move  "\\nts1\E\STORAGE\IMPORT" to NFTP2LocalDir
										Pack  SmtpSubject,"Could Not Create Directory to dump files."," for ",NFTPCOMP
//   Set the text message that is send with the attachments
										Pack    SmtpTextMessage(1),"These files will be dumped in the parent IMPORT Directory \\nts1\E\STORAGE\IMPORT",taskname
										Call	EmailSupport								
									Endif
								Endif								
							Else
								Pack	Taskname,NFTP2LocalDir	
								Path Exist,TASKNAME
								If Over
									Path Create,TASKNAME
									If Over								
//If over put files in import directory.  I know that exists.															
										Move "\\nts1\E\STORAGE\IMPORT" to NFTP2LocalDir
										Pack SmtpSubject,"Could Not Create Directory to dump files."," for ",NFTPCOMP
//   Set the text message that is send with the attachments
										Pack    SmtpTextMessage(1),"These files will be dumped in the parent IMPORT Directory \\nts1\E\STORAGE\IMPORT",taskname
										Call	EmailSupport																									
									Endif														
								Endif
							Endif						
						Endif
					Else
						If (NFTP2LocalDir = "")
							Pack NFTP2LocalDir,"\\nts1\E\DATA"	
						Else
							Path Exist,NFTP2LocalDir
							If over						
								Pack  SmtpSubject,"Could Not Find Directory to dump files."," for ",NFTPCOMP," in Directory ",NFTP2LocalDir
//   Set the text message that is send with the attachments
								Pack    SmtpTextMessage(1),"These files will be dumped in the parent IMPORT Directory \\nts1\E\STORAGE\IMPORT",taskname
								Call	EmailSupport									
								Pack NFTP2LocalDir,"\\nts1\E\STORAGE\IMPORT"																	
							Endif
						Endif
					Endif
					Setprop MySite,*LocalFolder=NFTP2LocalDir
//This sections lists the files on the ftpsite
					MySite.RemoteExists giving n9 using NFTP2RemoteDir			
					If (N9 = c0)			        
						MySite.Status giving taskname		
						Pack    SmtpSubject,"Cannot find Remote Directory ",NFTP2RemoteDir," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
						Pack    SmtpTextMessage(1),"Error Was ",taskname
						Call	EmailSupport
					Else
						Setprop MySite,*RemoteFolder=NFTP2RemoteDir					
						Move NFTP2RemoteDir,taskname1
						Move "c:\work\temp_list.txt",taskname2
						Move "#%NAME,#%SIZE,#%DATE",taskname3
					        MySite.GetList giving n9 using taskname1,taskname2,taskname3
						If (N9 = c0)			        
							MySite.Status giving taskname		
							Pack    SmtpSubject,"Cannot get a remote ftp listing "," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
							Pack    SmtpTextMessage(1),"Error Was ",taskname
							Call	EmailSupport
						Else
							Open FtpDirList,"c:\work\temp_list.txt"
//							Open NFTPLOGFLE3,NFTPLOGNAME,Exclusive					
							Loop 	
								Read FtpDirList,seq;*CDFON,taskname1,taskname2,taskname3
							Until Over
								Scan NFTP2WILDCARD in Taskname1
								if equal
									Move taskname3,str10
									Unpack str10,mm,slash,dd,slash,cc,yy
									call cvtjul
									if (Juldays > StartDate)
										Pack NFTPLOGFLD2,"02X",taskname1,b55,b55
										SQUEEZE taskname1,taskname1
										CALL NFTPLOGAIM
										If over
									    		MySite.Download giving n9 using taskname1
											If (N9 = c0)			        
												MySite.Status giving taskname		
												Pack    SmtpSubject,"Cannot download file ",taskname1," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
												Pack    SmtpTextMessage(1),"Error Was ",taskname
												Call	EmailSupport
											Else
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
													MySite.Status giving taskname		
													Pack    SmtpSubject,"Error while deleting file off of the ftp site ",taskname1," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
													Pack    SmtpTextMessage(1),"Error Was ",taskname
													Call	EmailSupport
												   Endif
												Endif										
											Endif
								    		else
											CALL Trim using NFTPLOGDATE
											MATCH taskname3,NFTPLOGDATE
											If not equal			.download		    		
								    				MySite.Download giving n9 using taskname1	
												If (N9 = c0)			        
													MySite.Status giving taskname		
													Pack    SmtpSubject,"Cannot download file ",taskname1," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
													Pack    SmtpTextMessage(1),"Error Was ",taskname
													Call	EmailSupport
												Else
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
														MySite.Status giving taskname		
														Pack    SmtpSubject,"Error while deleting file off of the ftp site ",taskname1," on ",NFTPADDRESS
//   Set the text message that is send with the attachments
														Pack    SmtpTextMessage(1),"Error Was ",taskname
														Call	EmailSupport
													   Endif
													Endif
												Endif
									    		Endif
										Endif
									Endif
								Endif
							Repeat							
						Endif
					Endif
				Endif
			Repeat
		Endif
	MySite.Disconnect
	Repeat
.	MySite.Status giving taskname
	MySite.Close
	Stop
	
EmailSupport
        Move    "1",SmtpTextIndexLast                               //Index to last entry in TextMessage array
        Move    "NTS4",SmtpEmailServer                              //Address of email serverc
        Clear   smtpemailaddress
        Append  "DesktopSupportGroup",SmtpEmailAddress
        Append  "@nincal.com",SmtpEmailAddress
        Reset   smtpemailaddress
        Move    "DesktopSupportGroup",SmtpUserName                  //User name
//   Set the destinations of the email. Max 100 (Mime spec)
        Move    smtpemailaddress,SmtpDestinations(1,1)
        Move    "DesktopSupportGroup",SmtpDestinations(1,2)
        Move    "1",SmtpDestIndexLast                               //originators UserName
        Move    "0",SmtpAttIndexLast                                //Index to last entry - Only 1 entry
        Clear   SmtpLogFile                                         //'Clear' disables the LogFile
        Move    "1",SmtpProgress                                    //Enable progress bars
        Call    SmtpSend                                            //( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
        If not equal
                Pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
                        "Status Code ",SmtpStatus," - ",SmtpStatusText
                Move    "Error Sending Message",SmtpSubject Subject
                Move    "0",SmtpAttIndexLast                        //Index to last entry - Only 1 entry
                Call    SmtpSend                                    //( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
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
	Pack    SmtpSubject,"Could Not Rename File.  Status is unknown ",fileDir," for ",b1,FileString
	Pack    SmtpTextMessage(1),"Error Was ",str50
	Call	EmailSupport	

	Return

	Include nftpio.inc
	Include nftp2io.inc
	Include nftplogio.inc
	Include Comlogic.inc