PC       EQU       0
         INCLUDE	COMMON.inc
         INCLUDE	CONS.inc
         INCLUDE	NSHPDD.inc
	 INCLUDE	COMPDD.inc
	 INCLUDE	CNTDD.INC
         INCLUDE	NORDDD.inc
         INCLUDE	NCNTDD.inc
         Include	NFTP2DD.INC
         Include        NOWNDD.INC
         INCLUDE	WINAPI.inc


RELEASE  INIT      "1.1"     DLH        various cleanup
reldate	Init	"18 Nov 2008"
.RELEASE  INIT      "1.0"      jd192003



IN         	FORM	 5
APPLIED    	FORM	 5
NUM      	FORM	 1


NNOTLR   	Dim	 6
NNOTDATE	Dim	10
NNOTTIME	Dim	10

InputName	Dim       254
INPUT    	FILE      var=80
NOTFILE  	IFile     var=80,keylen=6


skipcnt  form      	 5

CompHold        DIM       6
CompNHold	DIM       55

//datalist for filelisting
dlFiles       datalist
Taskname4  DIM 510
Taskname5  DIM 510

//For Dir Listings
FileString DIM 255
fileDir	   DIM 255
NDX        FORM 9
//For Fiile Rename
DateString DIM 16


	clock timestamp,timestamp
.	unpack timestamp,str2,yy,mm,dd
.	pack DateString,mm,slash,dd,slash,cc,yy
	Move Timestamp,Datestring

	Move	"Names In The News" TO COMPNME
	Move	"NNOT0001" TO PROGRAM
	Move	"Apply Order Notification " TO STITLE
	Clock	DATE TO TODAY
	Call	PAINT

	Open	NOTFILE,"NINNOT|10.10.30.103:502"



INPUT	   
	 Loop
	 	Clear  Applied
	 	Clear  SkipCnt
	 	
            		move    "KesSeq-NFTP2KS",Location
            		pack    KeyLocation,"Key: Key Seq "
	 	Call   NFTP2KS
	 Until Over	 	
 		If 	(NFTP2INFOTYPE = "N")
			 Move		NFTP2COMP to CompHold 
			 Move      	NFTP2COMP,COMPFLD 		
 			 Call      	Compkey
 			 Clear     	CompNhold
			 Move 	   	CompComp,CompNhold 			 
			 DISPLAY   	*P10:15,"Working On Company : ",CompComp,b5,CompHold			 
.//File Cleanup - Not used Currently
.		         Pack      XLSNAME,DateString,"_","Shipping.xls"
.		         Pack      Taskname,"c:\work\",XLSNAME
.		         Erase     Taskname

.//Do file Listing of Directory to get file name to open
			Create	dlFiles=1:10:1:10,visible=0
			Pack	taskname,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\OCONFIRM\","*.*"
			Pack	fileDir,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\OCONFIRM\"								
	                dlFiles.Dir giving result using *Filespec=Taskname,*flags=0
			If (result <> -1 )
				For NDX from 0 to result
					dlFiles.GetText giving FileString using *Index=NDX  
.//File Prep
					Call 	  Trim Using FileString
.//If record has been applied then skip	- Program will slap Applied_ after it has been applied.				
					Scan      "Applied" in FileString
					Goto 	  NextFile If Equal
..dave 25Apr2007
					Scan      "applied" in FileString
					Goto 	  NextFile If Equal
					reset	FileString
..dave 25Apr2007
		call	debug
				        PACK      InputName,"\\nins1\e\STORAGE\IMPORT\",COMPHOLD,"\OCONFIRM\",FileString
				        Trap      FileOpenFail Giving error if IO
.//Open the File				        
.				        OPEN      INPUT,InputName
				        OPEN      INPUT,InputName,REad
					TrapClr   IO
.//apply the Records					
			 		DISPLAY   *P10:16,"Working On File : ",FileString
					Call      ApplyConfirmation

					CLOSE	INPUT					
.//File Renamed After Application
..dave 25Apr2007
				            Clear 	Taskname
				            Clear 	Taskname4
				            Clear 	Taskname5
..dave 25Apr2007
					Pack	taskname,"Applied_",DateString,"_",FileString
..dave 25Apr2007
					call	Trim using Taskname
..dave 25Apr2007
					Pack	taskname4,fileDir,FileString
					Pack	taskname5,fileDir,taskname											
..dave 25Apr2007
					call	Trim using Taskname4
					call	Trim using Taskname5
..dave 25Apr2007
//Rename the file with Applied_ as a prefix					
					Trap	ErrorRename giving str50 if IO
						Rename	taskname4,taskname5
					Trapclr IO
						
NextFile					

				Repeat
//Clear out content in Listing
				dlFiles.ResetContent
			Endif
		Endif

	repeat

//Destroy Object with Listing					
	Destroy dlFiles				
	Shutdown
	Stop



ApplyConfirmation
	Loop
			If (COMPHOLD = "009406") .Donelley
				Read	Input,SEQ;*cdfon,str1,str1,NNOTLR,NNOTDATE,NNOTTIME
			Elseif (COMPHOLD = "009410") .Frontline
				Read	Input,SEQ;NNOTLR,str2,NNOTDATE,NNOTTIME
			Endif

	Until Over
			Type   	NNOTLR
			Goto   	NextRead If Not Equal
         		Cmatch 	" " TO NNOTLR        
         		Goto  	NextRead IF EOS 			
         		REP  	ZFILL IN NNOTLR
			READ	NOTFILE,NNOTLR;;
			If      Over
				If (COMPHOLD = "009406") .Donelley						
					Unpack    NNOTDATE into cc,yy,mm,dd
					Pack      NNOTDATE from cc,yy,dash,mm,dash,dd
//Remove Unnecessary Formatting
					Squeeze NNOTTIME,NNOTTIME,"PM"
					Squeeze NNOTTIME,NNOTTIME,"AM"	   
				Elseif (COMPHOLD = "009410") .Frontline
					Call	Trim using NNOTTIME				
// Do nothing	   
				Endif				
			FilePi	1;Notfile
           			Write   NOTFILE,NNOTLR;NNOTLR,b1,NNOTDATE,b1,NNOTTIME
           			Add	C1 to Applied
           			DISPLAY  *P10:10,"Records Added : ",Applied							
           		Else
           			DISPLAY  *P10:12,"Records Skipped : ",SkipCnt							           		
			Endif 

NextRead	
	Repeat
	Return
	
	
FileOpenFail
        Display   *P1:20,"File Open Error",error,*W2;
        MOve	"Q",str1
        Keyin     *P1:21,"ERROR WAS ",*DV,str50:
        		*p1:22,",Q for Quit. C for Continue. ",*t180,*rv,STR1         
	Uppercase str1         
        CMATCH    "C" TO str1
        If Equal
		Pack MailBody,"Could Not Open File ",InputName,".  File was not Applied"
		Move "DesktopSupportGroup@nincal.com",Mailto
		Move "DesktopSupportGroup@nincal.com",MailFrom
		pack mailsubjct,"NNOT0001 Could Not Open File"
		Pack MAILbody,"Could Not Open File.  Status is unknown ",fileDir," for ",b1,FileString	
		append	crlf,mailbody
		append	"input name: ",mailbody
		append	crlf,mailbody
		append	inputname,mailbody		
		append	crlf,mailbody
		reset 	mailbody
.		pack mailbody,"input name: ",inputname,crlf
		Call SendMail	         
        	Goto NextRead
        Else	
		TRAPCLR   IO
		Pack MailBody,"Could Not Open File ",InputName,".  File was not Applied"
		Move "DesktopSupportGroup@nincal.com",Mailto
		Move "DesktopSupportGroup@nincal.com",MailFrom
		pack mailsubjct,"NNOT0001 Could Not Open File"
		Pack MAILbody,"Could Not Open File.  Status is unknown ",fileDir," for ",b1,FileString	
		append	crlf,mailbody
		append	"input name: ",mailbody
		append	crlf,mailbody
		append	inputname,mailbody		
		append	crlf,mailbody
		reset 	mailbody
		Call SendMail	         
		SHUTDOWN  
		Stop
	Endif
ErrorRename
	Pack MailBody,"Error Was ",str50
	Move "DesktopSupportGroup@nincal.com",Mailto
	Move "DesktopSupportGroup@nincal.com",MailFrom
	Pack MAILSubjct,"Could Not Rename File.  Status is unknown ",fileDir," for ",b1,FileString	
	append	Taskname,mailbody
	append	crlf,mailbody
	append	"From:",mailbody
	append	crlf,mailbody
	append	Taskname4,mailbody
	append	crlf,mailbody
	append	"To:",mailbody
	append	crlf,mailbody
	append	Taskname5,mailbody
	reset	MailBody
	
	Call SendMail	
	Return	
	

	Include	COMPIO.inc
	Include	CNTIO.INC
        Include NSHPIO.inc
        Include NORDIO.inc
        Include NCNTIO.inc
        Include	NFTP2IO.INC                 
        Include NOWNIO.INC        
        Include COMLOGIC.inc
         

