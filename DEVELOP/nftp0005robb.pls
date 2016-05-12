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
Release   Init      "1.0"    
reldate   Init      "2014 February 25"

.// initialize some variables

TRANSFERTYPE	DIM	6
COUNTDRAC	INIT	1

	Open  	NFTPFILE,"NINFTP|NINS1:502"
MySite     automation      class="CuteFTPPro.TEConnection"		
	create MySite          
	
.//Main Program Loop starts here
		CALL InitSite
	STOP
		CALL OpenSite
		CALL InitPaths
		CALL ConnectSite
		CALL DownloadFiles
		CALL UploadFiles
		CALL CloseSite
	STOP
.//Main Program Loop ends here


InitSite	
.//This reads from the file //e/DATA/ninftp.batch, which is updated by program 34.  
.//It pulls each set of information, skipping the set if it's protocol is SMTP

	Loop
		Read NFTPFILE,seq;NFTPVARS
Until Over
        		
	SQUEEZE NFTPROTOCOL,NFTPPROTOCOL
	Display *10:8,"Protocol is: " ,NFTPPROTOCOL
	Setprop MySite,*Protocol=NFTPPROTOCOL
	
	SQUEEZE NFTPADDRESS,NFTPADDRESS
	Display *10:10,"Address is: " ,NFTPADDRESS
        Setprop MySite,*Host=NFTPADDRESS
        
	SQUEEZE NFTPUSERNAME,NFTPUSERNAME
	Display *10:12,"Username is: " ,NFTPUSERNAME
        Setprop MySite,*Login=NFTPUSERNAME
        
	SQUEEZE NFTPPASSWORD,NFTPPASSWORD
	Display *10:14,"Password is: " ,NFTPPASSWORD
	Setprop MySite,*Password=NFTPPASSWORD
	
	SQUEEZE TRANSFERTYPE,TRANSFERTYPE
	Display *10:14,"Transfer Type is: " ,TRANSFERTYPE
	Setprop MySite,*TransferType=TRANSFERTYPE

OpenSite
	return
		
InitPaths
	MOVE	"c:\Tester",NFTP2LocalDir
	Setprop MySite,*LocalFolder=NFTP2LocalDir
	MOVE	"\",NFTP2RemoteDir
.//Fix the color coding error on Textedit so it's less confusing to the eye" 
	Setprop MySite,*RemoteFolder=NFTP2RemoteDir
	return
	
.//

ConnectSite		
	Trap    TrapConnectFailure giving error if Object
		MySite.Connect giving N9
	Trapclr Object         	
		
DownloadFiles
.//FILES	DIM	255	
.//	MOVE	"*.*",FILES
.//	Trap    TrapDownloadFailure giving error if Object
.//			MySite.download giving N9 using FILES
.//			Trapclr Object         
	return

UplaodFiles
	return
	
CloseSite
.//	MySite.Disconnect
.//	MySite.Close
	return

TrapConnectFailure
	  pause 5
          Display *ES,"Error! Crash!  Returns: ",N9
          return
          
TrapDownloadFailure
FOO 	DIM	 400 
	GETINFO	EXCEPTION,FOO
        Display *ES,"Error! Crash!  Returns: ",FOO
          return

