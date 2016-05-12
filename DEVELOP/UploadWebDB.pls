.Upload the file \\nins1\e\data\ninweb.txt to demo.namesinthenews.com\data

           Include Common.inc 
           Include Cons.inc


PC EQU 0

Release   Init      "1.00"    RVW Initial release
reldate   Init      "2015 July 22"

           Call Paint                            

.initialize some variables


WebFile               File     
ErrorMessage          Dim        150


MySite     automation      class="CuteFTPPro.TEConnection"
           
Main
           create  MySite                     
           Call SetUpVars
           Call OpenFTPConnection
           Pause "1"
           Display *ES,""                                                    .//reset the screen
           Call Paint
           Call UploadFile
           
           destroy MySite
           Pause "1"
           Stop

SetUpVars
           Setprop MySite,*TransferType="AUTO"
           Setprop MySite,*Protocol="FTP"

           Display *P10:10,"Connecting to: demo.namesinthenews.com"
           Setprop MySite,*Host="demo.namesinthenews.com"
                      
           Display *P10:12,"Username: webdbuser"
           Setprop MySite,*Login="webdbuser"
                      
           Setprop MySite,*Password="!Webdb"
                      
           return

OpenFTPConnection
           Pack ErrorMessage,"Could Not Connect to the FTP Server.  Exiting"
           MySite.Connect giving N9                                       //N9 will be 0 if Connection Unsuccessful
           If (n9 == c0)
                      Display *P10:20,"Connection Unsuccessful!                      "
                      Pause "1"
                      
                      Destroy MySite
                      Pause "1"           
                      Stop
           Else       
                      Display *P10:20,"Connection Successful!                        "
                      Pause "1"
           Endif
           
           return

UploadFile
           Call VerifyLocalFile                                      //Let's make sure the local file is there
CheckAgain           
           Call CheckRemoteFile                                     //Find out if you have to remove the file on the server
           If (n9 == 0)                                              //n9 will be 0 if file is not there
                      Display *P10:10,"Checking Remote File...File is not there so we'll put it up."
                      Call PostFile           
                      Call VerifyUploadSuccess
           Else
                      Display *P10:10,"Checking Remote File...File is there so we'll remove it."               
                      Call RemoveOldFile
                      Goto CheckAgain
           Endif                      
                      
           return

VerifyLocalFile
           Display *P10:10,"Verifying Local File..."
           Trap FileMissing if io
           Move "Couldn't open the web file" to ErrorMessage
           Open Webfile, "\\nins1\e\data\webfile.txt", Exclusive
           Pause "1"
           Display *P10:10,"Verifying Local File!...It is there!"           
           Close Webfile
           
           return

CheckRemoteFile
           Display *P10:10,"Checking Remote File...                                                  "
           MySite.RemoteExists giving n9 using "/data/dbfile/webfile.txt"
           
           return

VerifyUploadSuccess
           If (n9 == c0)                                             //n9 will be 0 if file is not there
                      Display *P10:19,"Upload of ",filename
                      Display *P10:20,"Unsuccessful...Exiting            "  
           Else
                      Display *P10:19,"Upload of ",filename
                      Display *P10:20,"Successful...Exiting            "  
                      Pause "1"
           Endif             
           
           return

PostFile
           Setprop MySite,*LocalFolder="\\nins1\e\data"              //Set the local folder in our MySite object
           move "webfile.txt" to Filename
           Display *P10:12,"Filename: ",Filename
           Display *P10:14,"Local File Path: \\nins1\e\data\webfile.txt                              "
           Display *P10:16,"Remote File Path: /data/dbfile                                           "
           Setprop MySite,*Localfolder="\\nins1\e\data"
           Setprop MySite,*Remotefolder="/data/dbfile"
           MySite.Upload giving n9 using Filename           

           return
           
FileMissing
           Display *P10:21,ErrorMessage                                                                       
           Display *P10:22,"Exiting"

           destroy MySite
           Pause "1"
           Stop
           
           return
           
RemoveOldFile
                      MySite.RemoteRemove giving n9 using "/data/dbfile/webfile.txt"       //n9 will be 0 if an error occurs 
                      If (n9 == c0)                                 
                                 Display *P10:19,"An error occureed removing the old file"
                                 Display *P10:20,"Quitting "  
                                                                  
                                 destroy MySite
                                 Pause "1"
                                 Stop
                      Else
                                 Display *P10:19,"Succeeded in removing the file."
                                 Display *P10:20,"Trying again to upload"  
                                 
                                 Pause "1"
                                 Display *ES,""                                                    .//reset the screen
                                 Call Paint
                      Endif
                      
           return

ConnectFailure
           trapclr   io
           Display *P10:21,ErrorMessage

           return
           
OLEError 
           Display *P10:21,ErrorMessage

           return
           Include Comlogic.inc