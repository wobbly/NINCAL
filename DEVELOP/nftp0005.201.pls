


.//Write simple program that connects to each FTP server and then uploads and downloads the necessary files
.//Original Release Date: February 28, 2014
.//I stole a bunch of this code from the original nftp0005 by David Baca and David Herrick

.// include files
	Include Common.inc
	Include Cons.inc
	Include nftpdd.inc
	Include nftp2dd.inc
	Include nftplogdd.inc
          
PC EQU 0
C EQU 0
Release   Init      "2.01"    RVW	Must change transfertype to Auto before executing RemoteExists method.  Then switch back to binary if appropriate
reldate   Init      "2014 March 21"
.Release   Init      "2.00"    RVW	rewrote entire program so is legible.  added verify function which checks if the file exists before writing to log file.
.					output to user now explains better what is going on.
.reldate   Init      "2014 March 17"
.Release   Init      "1.31"    DLH	cleanup logfile read and date/time check and prog id to error message
.reldate   Init      "2014 March 06"
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

DateString DIM 10
	clock timestamp,timestamp
	unpack timestamp,str2,yy,mm,dd
	pack DateString,mm,slash,dd,slash,cc,yy


.// initialize some variables


TRANSFERTYPE	DIM	6		.//stores BINARY or AUTO to change transfer protocol
COUNTDRAC	INIT	1
check		FORM	1
ErrorReturn	DIM	255		.//error string
FirstCall  	FORM 	1		.//counter
fileDir		DIM 	255
NDX		FORM	9
impex  		DIM 	10
Current		File		


TransferFiles       datalist		.//list of files to transfer
FtpDirList          File

Taskname1  DIM 150
Taskname2  DIM 150
Taskname3  DIM 100
Taskname4  DIM 510
Taskname5  DIM 510
Taskname6  DIM 100
Taskname7  DIM 100

CuteLocalString   	DIM	150
RemoteCheck	   	DIM	150

	CALL Paint			.//Print the release date and change the background to blue

.//Cute FTP ActiveX control					
MySite     automation      class="CuteFTPPro.TEConnection"
          create  MySite
    
.//Open the database that stores the information for the sites 
.//This database is maintained and updated via Program 34 (comp0001.pls)
	Open      NFTPFILE,"NINFTP|10.10.30.103:502"   

	
.//Main Program Loop begins here	
Main
	loop
		Display *ES,""					.//reset the screen
		CALL Paint					.//make it blue again
		Move c1,Firstcall				.//reset our counter
		Read NFTPFILE,seq;NFTPVARS
			Until Over
.//		If the data we pull from the database if for an SMTP server or is targetfiles which is SSL_FTPS
.//		Start over.  Skip those.
		SQUEEZE NFTPPROTOCOL,NFTPPROTOCOL
		GOTO Main IF (NFTPPROTOCOL = "SMTP")		.//Skip e-mail protocol
.//		GOTO Main IF (NFTPPROTOCOL = "FTPS_IMPLICIT")	.//Skip targetfiles ftp, I think we don't use them anymore - we do DLH 2014 March 20
.//		GOTO Main IF (NFTPCOMP = "009428")	        .// MMI is still live??????- we do DLH 2014 March 20
		CALL InitMySiteVars
		CALL OpenSite
		CALL Transfer
		CALL DisconnectSite
	repeat
	MySite.Close						.//Close the Activation object so script doesn't fail to run next at next scheduled time
	Stop
.//Main Program Loop ends here


	

InitMySiteVars
.//Load the variables into the MySite class
	CALL CleanVars
	
	If (NFTPCOMP == "009406")				.//If it's infogroup then we need to use BINARY file transfer only
			MOVE "BINARY",TRANSFERTYPE 
		Else
			MOVE "AUTO",TRANSFERTYPE 
	Endif

	SQUEEZE TRANSFERTYPE,TRANSFERTYPE
	Setprop MySite,*TransferType=TRANSFERTYPE
	
	Setprop MySite,*Protocol=NFTPPROTOCOL
	
	Display *P10:10,"Connecting to: ",NFTPADDRESS
	SQUEEZE NFTPADDRESS,NFTPADDRESS                   
	Setprop MySite,*Host=NFTPADDRESS
	
	Display *P10:12,"Username: ",NFTPUSERNAME
	SQUEEZE NFTPUSERNAME,NFTPUSERNAME                           
	Setprop MySite,*Login=NFTPUSERNAME
	
	SQUEEZE NFTPPASSWORD,NFTPPASSWORD                                     
	Setprop MySite,*Password=NFTPPASSWORD
	
	return
	
OpenSite
	Display *P10:8,"Working On : ",NFTPDESC
	Trap TrapConnectFailure giving error if Object
		MySite.Connect giving N9
	If (N9 == c0)
		Display *P10:20,"Connection Unsuccessful!                      "
		Pause "1"
		GOTO Main
	Else	
		Display *P10:20,"Connection Successful!                        "
		Pause "1"
	Endif
	return
	
Transfer
	loop
		clear taskname
		Pack NFTP2FLD1,"01X",NFTPCOMP,NFTPCOMPID
		If (FirstCall == c1)				.//If this is the first search
			add c1,Firstcall			.//Update out counter
			CALL NFTP2AIM				.//Use an AIM search (start at beginning of database) an stop and return the record that contains this key
		Else						.//If this isn't the first search
			CALL NFTP2KG				.//Start from where you are in the file and return the record that contains this key
		Endif		
			Until Over				.//Stop when you reach the end of the file
		CALL 	CleanVars				.//Remove spaces in variables
		Setprop MySite,*Remotefolder=NFTP2RemoteDir
		Setprop MySite,*LocalFolder=NFTP2LocalDir		.//Set the local folder in our MySite object
		If (NFTP2ACTION == "U")				
			CALL Upload
		Else	
			CALL Download
		Endif						.//We need to do this multiple times per Company+ID because they may have multiple files to handle
	repeat
	return

Upload
	CALL SetLocalDir
	CALL CheckLocalDir
	CALL CheckRemoteDir
	Create    TransferFiles=1:10:1:10,visible=0		.//Let's make a list of files!!!  Yay!
	Pack      Taskname,NFTP2LocalDir,"\",NFTP2WILDCARD	.//"Fix color coding in Textedit
	Pack      fileDir,NFTP2LocalDir,"\"    			.//"Fix color coding in Textedit
	TransferFiles.Dir giving result using *Filespec=Taskname,*flags=0
	If (result <> -1)
		For NDX from 0 to result
			TransferFiles.GetText giving taskname1 using *Index=NDX
			Call        Trim Using taskname1

			Scan      "SENT" in taskname1
			Goto        NextFile If Equal                                                                                                     
			Scan      "sent" in taskname1
			Goto        NextFile If Equal                
			CALL PostFile
NextFile       
		repeat
	Endif                                                                                                
	return
	
Download
	CALL SetLocalDir
	CALL CheckLocalDir
	CALL CheckRemoteDir
	Move NFTP2RemoteDir,taskname1
	Move "c:\work\temp_list.txt",taskname2
	Move "#%NAME,#%SIZE,#%DATE",taskname3
	Trap TrapOLEError giving error if Object
	MySite.GetList giving n9 using taskname1,taskname2,taskname3
        If (N9 = c0)
        	Pack    MAILSubjct,"Cannot get a remote ftp listing "," on ",NFTPADDRESS
		Pack    MailBody,""
		CALL	ErrorEmail
	Else
		Open FtpDirList,"c:\work\temp_list.txt"
		Loop      
			Read FtpDirList,seq;*CDFON,taskname1,taskname2,taskname3                                                                          
                        		Until Over
                        scan NFTP2Wildcard in taskname1
                        reset taskname1
                        CALL CheckLog If equal
		repeat
	Endif
	return	

Verify
.\\Let's see if the file is where it should be now.
	If (NFTP2Action == "D")
		reset CuteLocalString
		Trap FileNoThere If IO
			Open	Current,CuteLocalString
			If (check == c1)
				Display *P10:19,"Download of ",taskname1
				Display *P10:20,"Unsuccessful                            "  
				Pause "2"
				
				Display *P10:15,"                                                                         "
				Display *P10:17,"                                                                         "
				Display *P10:19,"                                                                         "
				Display *P10:20,"                                                                         "
				Display *P10:21,"                                                                         "
				
				Pack	MAILSubjct,"Could Not Download Files.  Status is unknown ",NFTP2RemoteDir," on ",NFTPADDRESS,b1,taskname1
				Clear	Mailbody
				CALL 	ErrorEmail          		
			Else
				Display *P10:19,"Download of ",taskname1
				Display *P10:20,"Successful                              "  			
				Pause "2"
				
				Display *P10:15,"                                                                         "
				Display *P10:17,"                                                                         "
				Display *P10:19,"                                                                         "
				Display *P10:20,"                                                                         "
				Display *P10:21,"                                                                         "
				
				
.\\If it is and we're supposed to delete it let's try				
				If (NFTP2DELETE == "Y")
					CALL DeleteFile
				Endif
.\\If everything's peachy then we'll write it in the log so we don't try and get that file again.					
				CALL LogWrite			
			Endif
	Else
		Pack RemoteCheck,NFTP2RemoteDir,"/",taskname1
		MySite.RemoteExists giving N9 using RemoteCheck
		If (N9 = c0)                                    
			Display *P10:19,"Upload of ",taskname1
			Display *P10:20,"Unsuccessful           "  
			Pause "1"
			Pack	MAILSubjct,"Could Not Upload Files.  Status is unknown ",NFTP2RemoteDir," on ",NFTPADDRESS,b1,taskname1
			Clear	Mailbody
			CALL 	ErrorEmail
		Else
			Display *P10:19,"Upload of ",taskname1
			Display *P10:20,"Successful             "  
			Pause "1"
			CALL RenameFile
			CALL LogWrite
		Endif 
	Endif
	return	


DeleteFile		
	Trap    TrapOLEError giving error if Object
		MySite.RemoteRemove giving n9 using taskname1  
		If (N9 = c0)                                 
			Pack    MAILSubjct,"Error while deleting file off of the ftp site ",taskname1," on ",NFTPADDRESS
			Clear     Mailbody
			Call ErrorEmail
			Display *P10:19,"Removal off FTP of: ",taskname1
			Display *P10:20,"Unsuccessful           "  
		Else
			Display *P10:19,"Removal off FTP of: ",taskname1
			Display *P10:20,"Successful             "  
		Endif
		

LogWrite
	Move NFTPCOMP,NFTPLOGCOMP            	.Company Number                                    .1-6
	Move NFTPCOMPID,NFTPLOGCOMPID         	.Company Number                                    .1-6                                                                                                                        
	Move NFTP2Action,NFTPLOGAction       	.Upload/Download                                   .7
	Move taskname1,NFTPLOGFileName       	.File Name                                         .8-107
	Move NFTPDESC,NFTPLOGDESC            	.Company Description                               .108-207
	Move taskname3,NFTPLOGDATE          	.File Date          	                           .208-226
	Move DateString,NFTPLOGDDATE         	.Transaction Date                                  .251-258
	Move taskname2,NFTPLOGSIZE                                                                                      
	CALL NFTPLOGWRT  
	return



SetLocalDir
	If (NFTP2ACTION = "D")
		PACK impex,"IMPORT"
	Else
		PACK impex,"EXPORT"
	Endif
	If (NFTP2InfoType = "O")                                                                                                                              
		Pack      taskname,"\\nins1\E\STORAGE\",impex,"\",NFTPCOMP,"\","ORDERS\",NFTP2WILDCARD
		Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\",impex,"\",NFTPCOMP,"\","ORDERS"                                                                    
	ElseIf (NFTP2InfoType = "S")                                                                                                                          
		Pack      taskname,"\\nins1\E\STORAGE\",impex,"\",NFTPCOMP,"\","SHIPPING\",NFTP2WILDCARD
		Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\",impex,"\",NFTPCOMP,"\","SHIPPING"                                                                  
	ElseIf (NFTP2InfoType = "M")                                                                                                                          
		Pack      taskname,"\\nins1\E\STORAGE\",impex,"\",NFTPCOMP,"\","MERGE\",NFTP2WILDCARD
		Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\",impex,"\",NFTPCOMP,"\","MERGE"                                                                     
	ElseIf (NFTP2InfoType = "N")                                                                                                                          
		Pack      taskname,"\\nins1\E\STORAGE\",impex,"\",NFTPCOMP,"\","OCONFIRM\",NFTP2WILDCARD
		Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\",impex,"\",NFTPCOMP,"\","OCONFIRM"                                                                  
	ElseIf (NFTP2InfoType = "C")                                                                                                                          
		Pack      taskname,"\\nins1\E\STORAGE\",impex,"\",NFTPCOMP,"\","STATEMENT\",NFTP2WILDCARD
		Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\",impex,"\",NFTPCOMP,"\","STATEMENT"                                                                 
 	ElseIf (NFTP2InfoType = "T")                                                                                                                          
		Pack      taskname,"\\nins1\E\STORAGE\",impex,"\",NFTPCOMP,"\","STATS\",NFTP2WILDCARD
		Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\",impex,"\",NFTPCOMP,"\","STATS"                                                                     
	ElseIf (NFTP2InfoType = "B")                                                                                                                          
		Pack      taskname,"\\nins1\E\STORAGE\",impex,"\",NFTPCOMP,"\","BILLING\",NFTP2WILDCARD
		Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\",impex,"\",NFTPCOMP,"\","BILLING"                                                                   
	ElseIf (NFTP2InfoType = "G")                                                                                                                          
		Pack      taskname,"\\nins1\E\STORAGE\",impex,"\",NFTPCOMP,"\","MergeSum\",NFTP2WILDCARD
		Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\",impex,"\",NFTPCOMP,"\","MergeSum"                                                                  
	ElseIf (NFTP2InfoType = "Z")                                                                                                                          
		Pack      taskname,"\\nins1\E\STORAGE\",impex,"\",NFTPCOMP,"\","OTHER\",NFTP2WILDCARD
		Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\",impex,"\",NFTPCOMP,"\","OTHER"                                                                     
	ElseIf (NFTP2InfoType = "L")                                                                                                                          
		Pack      taskname,"\\nins1\E\STORAGE\",impex,"\",NFTPCOMP,"\","LOL\",NFTP2WILDCARD
		Pack      NFTP2LocalDir,"\\nins1\e\STORAGE\",impex,"\",NFTPCOMP,"\","LOL"                                                             	
	Endif
	return

CheckLocalDir
	Path Exist,NFTP2LocalDir						.//Check if the local path exists
		If over								.//If not 
			Path Create,NFTP2LocalDir	 			.//Create the directory
			If over											.//If that doesn't work
				Move "\\nins1\E\STORAGE\IMPORT",NFTP2LocalDir					.//Set the directory to this and send an e-mail
				Pack    MAILSubjct,"Could Not Create Directory to dump files."," for ",NFTPCOMP
				Pack    MailBody,"These files will be dumped in the parent IMPORT Directory \\nins1\E\STORAGE\IMPORT ",taskname
				CALL	EmailSupport
			Else							.//Else it worked.  
				GOTO CheckLocalDir				.//Go check if the path exists again
			Endif
		Endif
	return

CheckRemoteDir
	If (NFTP2RemoteDir <> "/")
.//Begin patch 2.01
		MOVE "AUTO",TRANSFERTYPE 
		Setprop MySite,*Localfolder=NFTP2LocalDir
		Trap    TrapOLEError giving error if Object
			MySite.RemoteExists giving N9 using NFTP2RemoteDir         
		If (NFTPCOMP == "009406")				.//If it's infogroup then we need to use BINARY file transfer only
			MOVE "BINARY",TRANSFERTYPE 
		Else
			MOVE "AUTO",TRANSFERTYPE 
		Endif
		SQUEEZE TRANSFERTYPE,TRANSFERTYPE
		Setprop MySite,*TransferType=TRANSFERTYPE
.//End patch 2.01		
		If (N9 = c0)                                    
			Pack MAILSubjct,"Could Not Find to Remote Directory ",NFTP2RemoteDir," on ",NFTPADDRESS
			Clear 	Mailbody
			CALL	ErrorEmail
		Endif
	Endif
	return

RenameFile
	Pack taskname,"SENT_",taskname1
	Pack taskname4,fileDir,taskname1
	Pack taskname5,fileDir,taskname                                                                                                             
	Trap ErrorRename giving str50 if IO
		Rename taskname4,taskname5
	Trapclr IO
	return

PostFile
	Trap    TrapOLEError giving error if Object
		If (NFTP2RemoteDir = "/")
			Pack NFTP2RemoteDir,"/" .//"Fix color coding in Textedit
		Else
			Pack NFTP2RemoteDir,NFTP2RemoteDir .//"Fix color coding in Textedit
		Endif
			Pack CuteLocalString,NFTP2LocalDir,"\",taskname1  .//"Fix color coding in Textedit
		
		Display *P10:13,"Filename: ",taskname1
		Display *P10:14,"Local File Path:                                                         "
		Display *P10:15,NFTP2LocalDir
		Display *P10:16,"Remote File Path:                                                        "
		Display *P10:17,NFTP2RemoteDir
		
		Setprop MySite,*Remotefolder=NFTP2RemoteDir
		Setprop MySite,*Localfolder=NFTP2LocalDir
		MySite.Upload giving n9 using taskname1
		
		CALL Verify
	return
	
GetFile
		Pack CuteLocalString,NFTP2LocalDir,"\",taskname1  .//"Fix color coding in Textedit
		Display *P10:13,"Filename: ",taskname1
		Display *P10:14,"Local File Path:                                                         "
		Display *P10:15,NFTP2LocalDir
		Display *P10:16,"Remote File Path:                                                        "
		Display *P10:17,NFTP2RemoteDir
		Trap    TrapOLEError giving error if Object
		MySite.Download giving n9 using taskname1
			
		CALL Verify
	return
	
CheckLog
	Clear NFTPLOGFLD1
	Pack NFTPLOGFLD2,"02X",taskname1	
	Move c1,Firstcall
	loop
		If (FirstCall == c1)				.//If this is the first search
			add c1,Firstcall			.//Update our counter
			CALL NFTPLOGAIM				.//Use an AIM search (start at beginning of database) an stop and return the record that contains this key
		Else						.//If this isn't the first search
			CALL NFTPLOGKG				.//Start from where you are in the log and look for the company number
		Endif		
		If over
			GOTO Getfile			.//If you didn't find it and reached the end of the database go download the file
		Else
			If (NFTPLOGDATE == taskname3)	.//See if the date in NFTPLOGDATE is part of the date in taskname 3
				return			  .//if so return back to Download and move on to the next file
			Endif
		Endif
	repeat							.//If the file isn't over keep reading
	DISPLAY *ES,"Something went wrong..."			.//Shouldn't get to here
	STOP						

DisconnectSite
	MySite.Disconnect 
	return
 
ErrorEmail
	append    "error was ",Mailbody
	append    error,Mailbody
	append    CrLF,Mailbody
	append    NFTPusername,Mailbody                                         
	append    CrLF,Mailbody
	append    NFTPDESC,Mailbody                                         
	append    CrLF,Mailbody
	append    NFTPAddress,Mailbody                                         
	append    CrLF,Mailbody
	append    NFTPCOMP,Mailbody                                         
	append    CrLF,Mailbody
	append    taskname,Mailbody
	reset     Mailbody
	CALL      EmailSupport
	return

CleanVars
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
	SQUEEZE   NFTPCOMP,NFTPCOMP
	SQUEEZE   NFTPCOMPID,NFTPCOMPID
	SQUEEZE   NFTPPROTOCOL,NFTPPROTOCOL
	SQUEEZE   NFTPADDRESS,NFTPADDRESS
	SQUEEZE   NFTPUSERNAME,NFTPUSERNAME
	SQUEEZE   NFTPPASSWORD,NFTPPASSWORD
	return

EmailSupport
	move "ComputerRequest@nincal.com",Mailto
	move "ComputerRequest@nincal.com",MailFrom
	move      c10,MailTimer
	CALL	SendMail                
	return
	
TrapConnectFailure
	GETINFO   EXCEPTION,ErrorReturn
	Pack      MAILSubjct,"Could Not Connect to ",NFTPADDRESS
	Clear     Mailbody
	CALL ErrorEmail
	return
	
TrapOLEError
	GETINFO   EXCEPTION,ErrorReturn
	Pack      MAILSubjct,"OLE exception",NFTPADDRESS
	Display *P10:21, "The program wanted to ask something, it's automated so it could not."
.//	Display *P10:22, "Program will now halt."
	Pause "1"
	Clear     Mailbody
	CALL ErrorEmail
.//	STOP
	return
	
ErrorRename
	Pack    MAILSubjct,"NFTP0005 - rename error"
	Clear     MailBody
	Append    "Could Not Rename File.  Status is unknown ",Mailbody
	Display *P10:20,"Rename UnSuccessful                 "
	Pause "2"
	CALL ErrorEmail
	return
	
	Include nftpio.inc
	Include nftp2io.inc
	Include nftplogio.inc
	Include Comlogic.inc

FileNoThere
	move c1,check
	return