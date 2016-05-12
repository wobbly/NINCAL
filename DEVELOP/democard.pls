.Step 1: Get a list of datacards currently listed on the website
.Step 2: Store that list in a database
.Step 3: Go through the datacard file
.          a.  check last used.  if was last is less than or equal to 2 years
.                     1.  check if file is on website
.                                a.  if it is, then check to see if the use date is the same as the datacard on the website
.                                           1.  if it is skip and move on
.                                b.  if it isn't then create a new datacard and upload it to the website
.                     2.  if it isn't pm tje website create a new data card and upload it to the website                      
.          b.  if last used is greater than 2 years

           Include Common.inc 
           Include Cons.inc
           Include nftpdd.inc
           Include nftp2dd.inc
           Include nftplogdd.inc
          
PC EQU 0

Release   Init      "1.00"    RVW          .//This program is new.  It shall do what webcards used to but more efficiently. See description above
reldate   Init      "2015 March 4"


TRANSFERTYPE          DIM        6                     .//stores BINARY or AUTO to change transfer protocol
COUNTDRAC  INIT       1
check                 FORM       1
ErrorReturn           DIM        255                   .//error string
FirstCall             FORM       1                     .//counter
fileDir               DIM        255
NDX                   FORM       9
impex                 DIM        10
Current               File                  

ListPath              DIM        100
ListData              DIM        100

FTPDataList           Datalist
Checkfile             File
DataListToCheck       Datalist

FTPSite               automation class="CuteFTPPro.TEConnection"
                      create     FTPSite           

RemoteDir             DIM        10

Main
                      Call       Paint          
                      destroy    FTPSite                          .//Make sure site variable is empty in case necessary to connect and reconnect
                      Pause      "2"
                      create     FTPSite                          .//Create a new object for each connection, close at the end or the thing has issues
                      call       InitFTPParameters
                      Call       ConnectToSite                    .//Set the remote directory to the Datacards directory                      
                      Call       SetRemoteDir
                      Call       GetFileList
                      destroy    FTPSite                          .//Destroy the FTP object after getting the list of file names
           Stop
           
InitFTPParameters
                      Squeeze    "AUTO",TRANSFERTYPE
                      Setprop    FTPSite,*TransferType=TRANSFERTYPE
                      
                      Squeeze    "FTP",NFTPPROTOCOL
                      Setprop    FTPSite,*Protocol=NFTPPROTOCOL
                      
                      Squeeze    "demo.namesinthenews.com",NFTPADDRESS                   
                      Display    *P10:10,"Connecting to: ",NFTPADDRESS
                      Setprop    FTPSite,*Host=NFTPADDRESS

           
                      Squeeze    "webcards",NFTPUSERNAME    
                      Display    *P10:12,"Username: ",NFTPUSERNAME
                      Setprop    FTPSite,*Login=NFTPUSERNAME
           
                      Squeeze    "Sp!DerD3C|<",NFTPPASSWORD                                     
                      Setprop    FTPSite,*Password=NFTPPASSWORD
           return       
           
ConnectToSite
                      Trap TrapConnectFailure giving error if Object
                                 FTPSite.Connect giving N9
                                 If (N9 == c0)
                                            Display *P10:14,"Connection Unsuccessful!                      "
                                            Pause "1"
                                            GOTO Main
                                 Else       
                                            Display *P10:14,"Connection Successful!                        "
                                            Pause "1"
                                 Endif
           return       

TrapConnectFailure
                      GetInfo    EXCEPTION,ErrorReturn
                      Pack       MAILSubjct,"Could Not Connect to ",NFTPADDRESS
                      Clear      Mailbody
                      Call       ErrorEmail
           return

GetFileList
                      Move "c:\work\temp_list.txt",ListPath
                      Move "#%NAME",ListData
                      
.//The following ActiveX control takes 3 variables (Which Directory to get a list from, Where to make the list, and Which data to pull)
.//We are pulling the name of the files

.                      FTPSite.GetList giving n9 using RemoteDir,ListPath,ListData
.                      If (N9 = c0)
.                                 Pack       MAILSubjct,"Cannot get a remote ftp listing for ",RemoteDir," on ",NFTPADDRESS
.                                 Display    *P10:19,"Cannot get a remote ftp listing for ",RemoteDir," on ",NFTPADDRESS
.                                 Pack       MailBody,""
.                                 Call       ErrorEmail
.                      Else
.                                 Display    *P10:19,"Getting List of Datacard Files..."
.                                 Open       FtpFileList,"c:\work\temp_list.txt"
.                                 Loop      
.                                            Read FTPFileList,seq;*CDFON,RemoteDir,ListPath,ListData                                                                          
.                                            Until Over
.                                 repeat
.                      Endif
                      
                      
                      
.//The following ActiveX control takes 3 variables (Which Directory to get a list from, Where to make the list, and Which data to pull)
.//We are pulling the name of the files

                      FTPSite.GetList giving n9 using RemoteDir,FTPDataList,ListData
                      If (N9 = c0)
                                 Display    *P10:19,"Cannot get a remote ftp listing for ",RemoteDir," on ",NFTPADDRESS
                      Else
                                 Display    *P10:19,"Getting List of Datacard Files..."
                                 Call       CheckDataList using FTPDataList
                      Endif      
           return  

CheckDataList routine 
                      Open       Checkfile,"c:\work\temp_list.txt"
                      Loop
                                 FTPDataList.GetText giving Filename using *Index=NDX
                                 Write Checkfile,NDX //some other junk.  David help?
                                 until over 
                      Repeat
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
                      Call      EmailSupport
           return

EmailSupport
                      move       "ComputerRequest@nincal.com",Mailto
                      move       "ComputerRequest@nincal.com",MailFrom
                      move       c10,MailTimer
                      Call       SendMail                
           return           
           
SetRemoteDir
.Check to make sure the directory exists.
                      move       "/Datacards/",RemoteDir
                      Trap       TrapOLEError giving error if Object
                      FTPSite.RemoteExists giving N9 using RemoteDir
                      If (N9 = c0)                                    
.                                 Pack       MAILSubjct,"Could Not Find Remote Directory, ",RemoteDir,", on ",NFTPADDRESS
                                 Display    *P10:16,"Could Not Find Remote Directory"
                                 Display    *P10:17,RemoteDir,", on ",NFTPADDRESS
                                 Clear      Mailbody
.                                 Call       ErrorEmail
                      Endif 
                      Trap       TrapOLEError giving error if Object
                      setprop    FTPSite,*RemoteFolder=RemoteDir
                      If (N9 = c0)                                    
.                                 Pack       MAILSubjct,"Could Not Change to Remote Director, ",RemoteDir,", on ",NFTPADDRESS
                                 Display    *P10:16,"Could Not Change to Remote Directory"
                                 Display    *P10:17,RemoteDir,", on ",NFTPADDRESS
                                 Clear      Mailbody
.                                 Call       ErrorEmail
                      Endif
                      Display    *P10:16,"Successfully changed"
                      Display    *P10:17,"to Remote Directory, ",RemoteDir,", on ",NFTPADDRESS
                      Pause "5"
           return

TrapOLEError
           GETINFO   EXCEPTION,ErrorReturn
           Pack      MAILSubjct,"OLE exception",NFTPADDRESS
           Display *P10:21,ErrorReturn
           Pause "2"
           Clear     Mailbody
.           Call      ErrorEmail
           return


           Include Comlogic.inc