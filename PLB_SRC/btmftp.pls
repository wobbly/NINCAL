.................................................. ............................ 
. 
. Program Name: BTMFTP 
. 
. Created by: BTM Consulting GmbH, Triberg 
. T. Buchholz 
. 
. Generated: 05.11 
. 
. Created for: GROMAS 
. 
.................................................. .............................
 . 
. Last edited on: 02:05:11; module created 
. It can now be downloaded via FTP data k up or down; 1.0.0: Version on? 
. Last edited by: T. Buchholz; additional additionally can create a file list. 
. 
. Last edited on: 06:06:11, FTP was set File refurbished and truly cleaned?. 
. to Version: 1.0.1 
. Last edited by: T. Buchholz 
. 
. Last edited on: 02:08:11; event controlling it improves the EventQueue is now processed befor the break instruct
 . to Version: 1.0.2 
. Last edited by: T. Buchholz 
. 
.................................................. .............................

          include   common
          INCLUDE   plbmeth.inc
.
$Progvs   INIT      "BTMFTP   1.0.2  08.11"
$BEREICH  INIT      "File Transfer Protocol"
$PROGFKT  INIT      "FTP processing"
. 

          Include   incdfbtm
.
localport FORM      " 1025"                       ;the data port we will listen on...
lport     DIM       5                             ;the user defined or modificatet data port we will listen on...
.
transfer    FILE

.
wdConnectString     DIM       400                 ;connection string to create the connections COMFIL 
wdFTPWork DIM       1024                ;transportation tag must not gr he be? 
wdError   DIM       400
seq       FORM      "-1"                ;Sequential Write forw rts? 
.. Port variables to calculate 
.
int1      INTEGER   2
p1          FORM      3
p2          FORM      3
D1          DIM       3
D2          DIM       3
dtoc        INIT      ".,"
ctod        INIT      ",."
ftpportP    DIM       5
ftpportT    FORM      5
.
tmout     FORM      "60"                ;Timeout f?r die Vebindungen                      
.
stat      DIM       21                            ;Comstat Ergebniss Variable
.
.COMSTAT information
MSGSTAT   RECORD
ConnectF  FORM      1
SendF     FORM      1
RecvF     FORM      1
ErrorF    FORM      1
ErrorC    FORM      8
ErrorN    FORM      8
Pending   FORM      1                             ;new for 8.5A
          RECORDEND
.
CR          INTEGER   2,"0x0a0D"
.
ftpresp     LIST
code        DIM       1
misc        DIM       1023
            LISTEND
.
* .............................................................................
.. Version info call 
.

          call      Test
          Stop

plf_mversinf plform mversinf
          formload  plf_mversinf
          Stop
          
* .............................................................................
.* .............................................................................
. Common description to FTP
.
. There are two types of ftp connections:
.
. 1. Control connection 
. 
.    is used to send control commands to the ftp server. The default port for a FTP connection is port 21.
.    
. control command:

. MLSD = filelist
. STOR = File upload
. RETR = File download
.
. TYPE [n] = type of tranfert: i = binary, a = ASCII
.
. LIST = old file list (whatever ...)
.
. CWD = Change Current Directory
.       attention, you have to use '\' as path seperator if the FTP Server is a Window-Server and '/' if 
.       FTP server is a Linux server. 
. 
. CDUP = change directory to the next (higher) level.
.
. User = Username
. Pass = User Pasword
. 
. DELE = deletes a file on the FTP-Server
. 
. PORT x = Tells the FTP-Server the local telnet socket (x for ecxample 127,0,0,1,3,1)
.
. PASV = FTP server tgive us the port of the data line for connectikon
.
. 2. Data Connection
.
. There are two different types of data connections:
.
. Activ data connection: Here the client open a local port for the server (socket)
. Problem: The firewall will supress the try of a connection of the server.
.
. Passiv: The server opens a second socket for a data line.
.         
* .............................................................................
          
* .............................................................................
. Test and examples:
.
Test

wdTime1   DIM       30
wdTime2   DIM       30

..        clock     TimeStamp,wdTime1
..        Pause     "12"
..        clock     TimeStamp,wdTime2
..        
..        alert     note,wdtime1,$result
..        alert     note,wdtime2,$result

          

..        Call      FtpSyncDir          using "btm-ub.de","C:\$$BTMWRK\FTP","/Ftp/test","btm-ub.de","telikan4001"
..        stop

..        debug
..        Call      FtpSyncDir          using "127.0.0.1","C:\$$BTMWRK\FTP","\TestFtp","torsten","torsten"
..        stop
          
          
          
..        call      FTPGetList          using "127.0.0.1","C:\$$BTMWRK\test1.txt","torsten","torsten"
          call      FTPSetFile          using "127.0.0.1","C:\$$BTMWRK\test1.txt","test.txt","torsten","torsten"
..        call      FTPGetFile          using "127.0.0.1","C:\$$BTMWRK\test2.txt","test.txt","torsten","torsten"
.
          Stop
* .............................................................................
. 
. A file will be downloaded via FTP 
. Download a file with FTP
.
. call    FtpGetFile using uFtpIp,uLocalFile,uFtpFile,uFtpName,uFtpPass [,uFtpMode,uLocalIp,uFtpPort = zus?tzliche ?bergabe f?r den Aktiv Modus]
.
. uFtpIp = IP of the FTP server Adressse 
. uLocalFile = path and file name for the local file (C: \ BTMWRK \ test.txt) 
. uFtpFile = filename on the FtpServer 
.
FtpGetFile          Function
uFtpIp    DIM       100       ;Ip Adresse des FtpServers
uLocalFile          DIM       300       ;Pfad und Dateiname des LokalenFiles / Path and filename of the local file
uFtpFile  DIM       300       ;Pfad und Dateiname des FtpFiles / Path and Filenam on FTP Server
uFtpUser  DIM       100       ;User Name 
uFtpPass  DIM       100       ;User Password
uFtpPort  DIM       100       ;Ftp Port f?r den Aktiv Modus / FTP port for activ mode
uFtpMode  FORM      1         ;1 = Aktiv Modus, Default 0 = Passiv Modus / 1=acitve 0=passive mode (default=0)
uLocalIp  DIM       100       ;Lokale Ip Adresse f?r den Aktiv Modus / local IP adrress if active mode is used
          Entry
myErrorKz FORM      1
ControllS Comfile
DataS     Comfile
.. Connection port pr affected? Possibly put on standard port 21 
. check connection port (default 21) 
.
          $LL       uFtpPort
          If        eos
            Pack      uFtpPort,"21"
          Endif
.. Establishing a connection 
.. establish connection 
.
          Call      SetUpConnection using ControllS,uFtpIp,uFtpPort,uFtpUser,uFtpPass
          If        (uFtpMode = wf0)              ;Passiv Modus
.. Determine the data port of FtpServer 
.. get data port of the FTP server 
.
            Call      GetPort giving lport using ControllS
.. Connect the data cable 
.. Establish data line 
.
            Call      Connect using DataS,uFtpIp,lport
          Else      ;Aktiv Modus
.. Socket F? R the opening dates? 
.. open socket for data line 
.
            MOVE      localport to lport
            Call      SOCKETCREATE usiNg DataS,uLocalIp,lport
.. Port Set 
.. set port 
.
            Call      SetPort using ControllS,uLocalIp,LocalPort
          Endif
.. Put another specification released 
.. The? Bertragungsart mountain level? 
.
          PACK      wdFTPWork,"TYPE i",CR
          CALL      send USING ControllS,wdFTPWork,tmout
          CALL      recv USING ControllS,wdFTPWork,tmout    
.. File Local vorbereiten
.. prepare local file
.
          Call      FilePrep giving wstatus using transfer,uLocalFile
          if        (wStatus)
          Return
          Endif
          
.. Das File anfordern
.. ask for file...
.
          PACK      wdFTPWork,"RETR ",uFtpFile,cr
          CALL      send USING ControllS,wdFTPWork,tmout
          CALL      recv USING ControllS,wdFTPWork,tmout
          UNPACK    wdFTPWork,code
          If        (code = "1")
            COMSTAT   DataS,stat
            UNPACK    stat,msgstat
            IF        (msgstat.ConnectF=1)
              LOOP
                IF              (msgstat.recvf=1)       ;incomming data on dataport
                  CALL            recv USING DataS,wdFTPWork,tmout
                  BREAK IF OVER

                  WRITE           transfer,seq;*LL,*ABSON,wdFTPWork;
                ENDIF
                COMSTAT         DataS,stat
                UNPACK          stat,msgstat
                BREAK                     IF (MSGSTAT.CONNECTF=0)
              REPEAT
              Weof            transfer,seq
              CLOSE           transfer
          Call      ClearEvents
              Pause "2"
              
            ENDIF
.. Nach der ?bertragung den Status des Transfers abfragen
.. check status after transfer
.
.. Works only with big en? Transmissions 
..
..          CALL      recv USING ControllS,wdFTPWork,tmout
..          UNPACK    wdFTPWork,code
..          If        (code <> "2")
..            Set             myErrorKz
..          Endif   
          Else
            Set       myErrorKz                               
          Endif
.. Fehlermeldung aufl?sen und ausgeben
.. check error and alert (if error occurs...)
.         
          If        (myErrorKz)
            Call      ErrorTable giving wdError using wdFTPWork 
            $LngAlert           $Stop,wdError,$Progvs,"SSTRID0001","Error","SSTRID0002"
          Endif

          FunctionEnd
* .............................................................................
.
. Eine Datei wird mittels Ftp hochgeladen
.
. call    FtpSetFile using uFtpIp,uLocalFile,uFtpFile,uFtpName,uFtpPass [,uFtpMode,uLocalIp,uFtpPort = zus?tzliche ?bergabe f?r den Aktiv Modus]
.
. uFtpIp      = IP-Adressse des Ftp Servers
. uLocalFile  = Pfad und Datei Name zur Lokalen Datei (C:\BTMWRK\Test.txt) 
. uFtpFile    = Datei Name auf dem FtpServer
.         
FtpSetFile          Function
uFtpIp    DIM       100       ;Ip Adresse des FtpServers
uLocalFile          DIM       300       ;Pfad und Dateiname des LokalenFiles
uFtpFile  DIM       300       ;Pfad und Dateiname des FtpFiles
uFtpUser  DIM       100       ;User Name
uFtpPass  DIM       100       ;User Password
uFtpPort  DIM       100       ;Ftp Port f?r den Aktiv Modus
uFtpMode  FORM      1         ;1 = Aktiv Modus, Default 0 = Passiv Modus
uLocalIp  DIM       100       ;Lokale Ip Adresse f?r den Aktiv Modus
pProgressB          progress  ^         ;Progressbar
          Entry
myErrorKz FORM      1
ControllS Comfile
DataS     Comfile
isNoProg  FORM      1
myFileSize          FORM      16

myPgVal1  FORM      16.8
myPgVal2  FORM      16.8



.. Teste die Progressbar
          Moveptr   pProgressB,pProgressB
          If        over
.. wenn nicht ?bergeben
            Set       isNoProg
          Endif
.. Verbindungs Port pr?fen ggf. auf Standardport 21 setzten
.
          $LL       uFtpPort
          If        eos
            Pack      uFtpPort,"21"
          Endif
.. Verbindung aufbauen
.
          Call      SetUpConnection giving wStatus using ControllS,uFtpIp,uFtpPort,uFtpUser,uFtpPass
          If        (wStatus)
            Return using wf1
          Endif     

.. Progress auf 15% setzen
          If        (isNoProg = wf0)
            Setitem   pProgressB,0,15
          Endif

          If        (uFtpMode = wf0)              ;Passiv Modus
.. Ermittle den Daten Port des FtpServers         
.
            Call      GetPort giving lport using ControllS
.. Verbinde die Datenleitung
.
            Call      Connect                       using DataS,uFtpIp,lport
          Else      ;Aktiv Modus
.. Socket F?r die Daten ?ffnen
.
            MOVE      localport to lport
            Call      SOCKETCREATE usiNg DataS,uLocalIp,lport
.. Port Setzen
.
            Call      SetPort using ControllS,uLocalIp,LocalPort
          Endif

.. Porgress auf 40% setzen    
          If        (isNoProg = wf0)
            Setitem   pProgressB,0,40
            MOVE      "40" to myPgVal2
          Endif
          
.. weitere Sezifikationen Setzen
.. Die ?bertragungsart ?bergeben
.
          PACK      wdFTPWork,"TYPE i",CR
          CALL      send USING ControllS,wdFTPWork,tmout
          CALL      recv USING ControllS,wdFTPWork,tmout
          
.. Test des Localen files und progress berechnen
.         
          If        (isNoProg = wf0)
            Call      FileSize giving mypgval1 using uLocalFile
            Calc      myPgval1 =                    (1024 / mypgval1)
            Calc      mypgval1 = mypgval1 * 60
          Endif
          
.. File Local vorbereiten
.
          Call      FileOpen giving wStatus using Transfer,uLocalFile
          If        (wStatus)
            Return using wf1
          Endif
          
.. Das File anfordern
.
          PACK      wdFTPWork,"STOR ",uFtpFile,cr
          CALL      send USING ControllS,wdFTPWork,tmout
          CALL      recv USING ControllS,wdFTPWork,tmout
          UNPACK    wdFTPWork,code
          If        (code = "1")
            COMSTAT   DataS,stat
            UNPACK    stat,msgstat
            IF        (msgstat.ConnectF=1)
              LOOP
                IF              (msgstat.sendf=1)       ;incomming data on dataport
                  Read            transfer,seq;*Abson,wdFTPWork;
                  BREAK IF OVER
                  CALL            send USING DataS,wdFTPWork,tmout
.. Progressbar setzen                 
                  If (isNoProg = wf0)
                    Calc            myPgVal2 = myPgVal2 + myPgVal1
                    Setitem         pProgressB,0,myPgVal2
                  Endif           
                ENDIF
                COMSTAT         DataS,stat
                UNPACK          stat,msgstat
                BREAK                     IF (MSGSTAT.CONNECTF=0)
              REPEAT
              CALL            send USING DataS,wdFTPWork,tmout
          Call      ClearEvents
              PAUSE           "2"                     ; muss laut Mathew hier stehen
.
              CLOSE           transfer
              comclr                    Datas
              comclose        Datas
          Call      ClearEvents
              PAUSE           "1"
            ENDIF
.. Nach der ?bertragung den Status des Transfers abfragen
.         
            CALL      recv USING ControllS,wdFTPWork,tmout
            UNPACK    wdFTPWork,code
            If        (code <> "2")
              Set             myErrorKz
            Else
..            PAUSE "2"
..            COMCLOSE DataS,wf1  ;Vebindung bzw. ?bertragung abgeschlossen.        
            Endif     
          Else
            Set       myErrorKz
          Endif

.. Denn Progress noch auf 100 setzen
.
          
          If        (isNoProg = wf0)
            Setitem   pProgressB,0,100
          Endif     
          
.. Fehlermeldung aufl?sen und ausgeben
.         
          If        (myErrorKz)
            Call      ErrorTable giving wdError using wdFTPWork 
            $LngAlert           $Stop,wdError,$Progvs,"SSTRID0001","Error","SSTRID0002"
            Return using wf1
          Endif
          FunctionEnd using wf0
          
* .............................................................................
. Gr?sse des Files ermitteln
.
FileSize  Function
uFileName DIM       300
          entry
myFileSize          FORM      9
          exceptset ioerror if io
          FindFile  uFilename,FileSize=myFileSize
          
          exceptclear         io

          Return using myfilesize
ioerror

          FunctionEnd using wf0 
          
* .............................................................................
.
. Auf dem Ftp Server wird nach den Dateien gefragt
.
. call    FtpGetList using uFtpIp,uLocalFile,uFtpName,uFtpPass [,uFtpMode,uLocalIp,uFtpPort = zus?tzliche ?bergabe f?r den Aktiv Modus]
.
. uFtpIp      = IP-Adressse des Ftp Servers
. uLocalFile  = Pfad und Datei Name zur Lokalen Datei (C:\BTMWRK\Test.txt) 
. uFtpFile    = Datei Name auf dem FtpServer
.
FtpGetList          Function
uFtpIp    DIM       100       ;Ip Adresse des FtpServers
uLocalFile          DIM       300       ;Pfad und Dateiname des LokalenFiles
uFtpUser  DIM       100       ;User Name
uFtpPass  DIM       100       ;User Password
uFtpDir   DIM       300       ;Pfad zum ermitteln der files
uFtpPort  DIM       100       ;Ftp Port f?r den Aktiv Modus
uFtpMode  FORM      1         ;1 = Aktiv Modus, Default 0 = Passiv Modus
uLocalIp  DIM       100       ;Lokale Ip Adresse f?r den Aktiv Modus
          Entry
myWork1   DIM       200
myWork2   DIM       200
myWork3   DIM       200

myErrorKz FORM      1
wdtest    DIM       20
ControllS Comfile
DataS     Comfile
.. Verbindungs Port pr?fen ggf. auf Standardport 21 setzten
.
          $LL       uFtpPort
          If        eos
            Pack      uFtpPort,"21"
          Endif
.. Verbindung aufbauen
.
          Call      SetUpConnection using ControllS,uFtpIp,uFtpPort,uFtpUser,uFtpPass
          If        (uFtpMode = wf0)              ;Passiv Modus
.. Ermittle den Daten Port des FtpServers         
.
            Call      GetPort giving lport using ControllS
.. Verbinde die Datenleitung
.
            Call      Connect                       using DataS,uFtpIp,lport
          Else      ;Aktiv Modus
.. Socket F?r die Daten ?ffnen
.
            MOVE      localport to lport
            Call      SOCKETCREATE usiNg DataS,uLocalIp,lport
.. Port Setzen
.
            Call      SetPort                       using ControllS,uLocalIp,LocalPort
          Endif
.. weitere Sezifikationen Setzen
.. Die ?bertragungsart ?bergeben
.
          PACK      wdFTPWork,"TYPE i",CR
          CALL      send USING ControllS,wdFTPWork,tmout
          CALL      recv USING ControllS,wdFTPWork,tmout
                    
.. File Local vorbereiten
.
          Prep      transfer,uLocalFile,Exclusive
.. Die File-Liste anfordern
.
          $LL       uFtpDir
          If        not eos
            PACK      wdFTPWork,"MLSD ",uFtpDir,cr
          Else
            PACK      wdFTPWork,"MLSD",cr
          Endif
          CALL      send USING ControllS,wdFTPWork,tmout
          CALL      recv USING ControllS,wdFTPWork,tmout
                    
          UNPACK    wdFTPWork,code
          If        (code = "1" | code = "2")
            COMSTAT   DataS,stat
            UNPACK    stat,msgstat
            IF        (msgstat.ConnectF=1)
              LOOP
                IF              (msgstat.recvf=1)       ;incomming data on dataport
                  CALL            recv USING DataS,wdFTPWork,tmout
                  BREAK IF OVER

                  WRITE           transfer,seq;*LL,*ABSON,wdFTPWork;
                ENDIF
                COMSTAT         DataS,stat
                UNPACK          stat,msgstat
                BREAK                     IF (MSGSTAT.CONNECTF=0)
              REPEAT
              Weof            transfer,seq
              CLOSE           transfer
            ENDIF
            
            COMCLOSE          DataS,wf1
.. Nach der ?bertragung den Status des Transfers abfragen
.
..          CALL      recv USING ControllS,wdFTPWork,tmout
..          UNPACK    wdFTPWork,code
..          If        (code <> "2")
..            Set             myErrorKz
..          Endif   
          Else
            Set       myErrorKz                               
          Endif
.. Fehlermeldung aufl?sen und ausgeben
.         
          If        (myErrorKz)
            Call      ErrorTable giving wdError using wdFTPWork 
            $LngAlert           $Stop,wdError,$Progvs,"SSTRID0001","Error","SSTRID0002"
          Endif
          FunctionEnd
          
* .............................................................................
.
.
.
.
.
.
.
FtpSyncDir          Function
uFtpIp    DIM       100       ;Ip Adresse des FtpServers
uLocalDir DIM       300       ;Pfad des lokalen Ordners
uFtpDir   DIM       300       ;Pfad des Ftp Ordners
uFtpUser  DIM       100       ;User Name
uFtpPass  DIM       100       ;User Password
uFtpPort  DIM       100       ;Ftp Port f?r den Aktiv Modus
uFtpMode  FORM      1         ;1 = Aktiv Modus, Default 0 = Passiv Modus
uLocalIp  DIM       100       ;Lokale Ip Adresse f?r den Aktiv Modus
          Entry
myErrorKz FORM      1
ControllS Comfile
DataS     Comfile
myLocalFiles        DIM       32680
myLocalFile                   DIM       200
myItems   FORM      9
myLocalFileAdressed DIM       300
myFtpFileAdressed   DIM       300
mylastwriteL        DIM       14
mylastwriteS        DIM       14
myDate1   FORM      14
myDate2   FORM      14
myDateResult        FORM      14
myItemC   FORM      9
myFtpList datalist

.. Verbindungs Port pr?fen ggf. auf Standardport 21 setzten
.
          $LL       uFtpPort
          If        eos
            Pack      uFtpPort,"21"
          Endif
.. Files auf dem Sever Ermitteln
.
          Call      FTPGetList using uFtpIp,"C:\$$BTMWRK\FileList.txt",uFtpUser,uFtpPass,"\TestFtp"
          Call      Ftpdatalist using myFtpList
.. Directory local vorbereiten
.
          Pack      wdString,uLocalDir,"\"
          Finddir   wdString,myLocalfiles,Mode=wf2,Itemcount=myItems
          For       myItemC from wf1 to myitems 
            Explode   myLocalFiles,"|",myLocalFile
            Bump      myLocalFile
            Pack      myLocalFileAdressed,uLocalDir,"\",myLocalFile
            $LL       uFtpDir
            If        eos
              Pack            myFtpFileAdressed,myLocalFile
            Else
              cmatch          "/",uFtpDir
              if    equal
              Pack            myFtpFileAdressed,uFtpDir,"/",myLocalFile
              else    
              Pack            myFtpFileAdressed,uFtpDir,"\",myLocalFile
              endif
            Endif
            $FITEMDL            myftpList,myLocalFile
            If        ($result = wf0)               ;File nicht in Filelist
              Call            FTPSetFile                        using uFtpIp,myLocalFileAdressed,myFtpFileAdressed,uFtpUser,uFtpPass
            Else      ;File auf dem server vorhanden
              Getitem                   myftpList,$result,wdString
              Explode                   wdString,"|",wdim1,mylastwriteS
              MOVE            mylastwriteS to mydate1
              Findfile        myLocalFileAdressed,Write=myLastwritel
              MOVE            mylastwritel to mydate2
              Calc            myDateResult = mydate2 - mydate1
              If              (myDateResult >= wf0)
                Call                      FTPSetFile              using uFtpIp,myLocalFileAdressed,myFtpFileAdressed,uFtpUser,uFtpPass
              Endif           
            Endif
          Repeat
          
          FunctionEnd
* .............................................................................
.
. ?ffnet/erstellt das File in einer Exception
.
.
FileOpen  Function
uFile     FILE      ^
upath     DIM       ^
          Entry
          
          exceptset ioerror if io
          open      uFile,uPath
          exceptclear io
          Return using wf0
ioerror
          Functionend using wf1
.         
FilePrep  Function
uFile     FILE      ^
upath     DIM       ^
          Entry
          
          exceptset ioerror if io
          prepare   uFile,uPath,Exclusive
          exceptclear io
          Return using wf0
ioerror
          Functionend using wf1

* .............................................................................
.
.
.         
.         
FtpDatalist         Function
myDL      datalist  ^
          Entry     
myFile    File

myDlString          DIM       1024

myFileString        DIM       2048
myFileNameNumber    FORM      4
myFileModify        FORM      4
myFileType                    FORM      4
myFileDetails       DIM       100(20)
isFirstRead                   FORM      "1"
myLP                FORM      9
myDummyModify       DIM       7
myDummyType                   DIM       5
myType              DIM       4
myModifyDate        DIM       14
myFileList                    DIM       200

          Create    myDl=1:1:1:1
          
          Pack      myFileList,"C:\$$BTMWRK\FileList.txt"
          
          Call      FileOpen giving wStatus using myFile,myFileList
          if        (wStatus)
          setflag   over
          Return
          endif
.
          Loop
            Read      myFile,seq;*ll,myFileString
            Break if over
            If        (isFirstRead)
              Clear           isFirstRead
              Occurs                    ";",myFileString,myFileNameNumber
              Incr            myFileNameNumber
              Movelptr        myFileString,myLP
              Scan            "modify=",myFileString
              If              equal
                Lenset          myFileString
                Reset                     myFileString,1
                Occurs          ";",myFileString,myFileModify
                Incr                      myFileModify
                Setlptr         myFileString,myLP
              Endif
              Scan            "type=",myFileString
              If              equal
                Lenset          myFileString
                Reset                     myFileString,1
                Occurs          ";",myFileString,myFileType
                Incr                      myFileType
                Setlptr         myFileString,myLP
              Endif           
            Endif
            Explode   myFileString,";",myFileDetails
            UNPACK    myFileDetails(myFileType),myDummyType,myType
            Continue            if (myType <> "file")
            UNPACK    myFileDetails(myFileModify),myDummyModify,myModifyDate
            Loop
              Cmatch                    " ",myFileDetails(myFileNameNumber)
              If              equal
                Bump                      myFileDetails(myFileNameNumber)
              Else
                Break
              Endif

            Repeat
            Implode   myDlString,"|",myFileDetails(myFileNameNumber),myModifyDate
            Insertitem  myDl,999999,myDlString
          Repeat
.
          Functionend using myDl
* .............................................................................
. Erstellt die Verbindung bis nach dem Login
. 
SetUpConnection     Function
pSocket   Comfile   ^
pFtpIp    DIM       ^
pFtpPort  DIM       ^
pUser     DIM       ^
pPass     DIM       ^
          Entry
myNachricht         DIM       300       
.
.. Die Verbindung zum Ftp Server aufbauen
.
          Call      Connect giving wStatus using pSocket,pFtpIp,pFtpPort
          if        (wStatus)
          Return using wf1
          endif
          
          Call      Recv using pSocket,myNachricht,tmout
          UNPACK    myNachricht,ftpresp
          If        (code = "2")
.. in den Server Einloggen
.. Name des Users ?bergeben
.
            PACK      myNachricht,"USER ",pUser,cr
            CALL      send USING pSocket,myNachricht,tmout
            CALL      recv USING pSocket,myNachricht,tmout
            UNPACK    myNachricht,ftpresp
            If        (code = "3")
.. Password des Users ?bergeben
.
              PACK            myNachricht,"PASS ",pPass,cr
              CALL            send USING pSocket,myNachricht,tmout
              CALL            recv USING pSocket,myNachricht,tmout
.. Restlichen Buffer der Verbindung auslessen
.
              UNPACK                    myNachricht,ftpresp
              If              (code = "5")
                Return using wf1
              Endif           
.
              Loop
                CALL                      Recv USING pSocket,myNachricht,wf1
                Until Zero      
              Repeat
            Endif
          Endif
          FunctionEnd using wf0
.
* .............................................................................
.
. Meldet beim Server den Passiv Modus an und erwartet die Port Antwort zur?ck
.
. Aus der portantwort wird der ge?ffnete Port errechnet und diesen dann zur?ck gegeben
.         
GetPort   Function
pSocket   Comfile   ^         
          Entry
myNachricht         DIM       400
.. Passiv Modus senden
.
          PACK      myNachricht,"PASV",cr
          CALL      send USING pSocket,myNachricht,"60"
          CALL      recv USING pSocket,myNachricht,"60"
          If        OVER
            Return
          Endif
          UNPACK    myNachricht,ftpresp
          SCAN      "(",misc
          BUMP      misc
          PARSE     misc,d1,"09"
          BUMP      misc
          PARSE     misc,d1,"09"
          BUMP      misc
          PARSE     misc,d1,"09"
          BUMP      misc
          PARSE     misc,d1,"09"
          BUMP      misc

          PARSE     misc,d1,"09"
          BUMP      misc
          PARSE     misc,d2,"09"
          MOVE      d1,p1
          MOVE      d2,p2

          MOVE      (p1*256),int1
          ADD       p2,int1
          MOVE      int1,ftpportT
          MOVE      ftpportT,ftpportP

          FunctionEnd         using ftpportP
          
* .............................................................................
.
. Verbindung zu einem ge?ffneten Port aufbauen
.
. pSocket = zu ?ffnendes Comfile
. pHost = die Ip Adresse der Vebindung
. pPort = der Port auf dem verbunden wird
. uMode = ?bergabe ob die Daten rein sind oder schon aufbereitet
.
Connect   Function
pSocket   COMFILE   ^
pHost     DIM       ^
pPort     DIM       ^
uMode     DIM       1
          Entry
MyConnectString     DIM       300
.
          $LL       uMode
          If        eos
            Pack      uMode,"R"
          Endif
.
          PACK      MyConnectString,"S,O,",pHost,",",pPort,",",uMode
          exceptset opfail if io
          COMOPEN   pSocket,MyConnectString
          exceptclear         io
          CALL      CLRFLAG
          Return    using wf0
.         
Opfail    
          FunctionEnd using wf1
* .............................................................................
.
. Liest die Nachricht eines Comfiles aus
.
. pSocket = Comfile
. pMsg = die variable in dem der Inhalt ?bertragen wird
. uTimeout = die Zeit wie Lange auf der vebindung geh?rt wird bevor es abbricht
Recv      Function  
pSocket   COMFILE   ^
pMSG      DIM       ^
uTimeout  FORM      9
          Entry
wfTmout   FORM      9
.
          If        (uTimeout = wf0)
            MOVE      "15" to uTimeout
          Endif
.
          CLEAR     wfTmout
          LOOP
            COMSTAT   pSocket,STAT
            UNPACK    STAT,msgstat
            IF        (msgstat.errorF = 1 )
              SETFLAG                   OVER
              SETFLAG                   ZERO
              SETFLAG                   EOS
              SETFLAG                   LESS
              RETURN
            ELSE IF   (msgstat.ConnectF = 0 )
              CALL            CLRFLAG
              SETFLAG                   OVER
              SETFLAG                   ZERO
              RETURN
            ENDIF
            UNTIL     (msgstat.RecvF = 1 )
.
          Call      ClearEvents
            PAUSE     "1"                           ;one second delay
            IF        (uTimeout!=0)
              INCR            wfTmout
              IF              (wfTMOUT = uTimeout)
                CALL                      CLRFLAG
                SETFLAG         ZERO
                RETURN
              ENDIF
            ENDIF
          REPEAT
.
          COMREAD   pSocket;pMSG;
          CALL      CLRFLAG
.
          FunctionEnd         
* .............................................................................
.
. Sendet ?ber das Comfile die Daten
.
. pSocket = verwendetes Comfile
. pMsg = Variable f?r den Inhalt der gesendet wird
. uTimeout = zeit bis die Verbindung als unterbrochen gilt
.
.
send      Function
pSocket   COMFILE   ^
pMSG      DIM       ^
uTimeout  FORM      9
          Entry
          
          MOVE      "0",TMOUT
          LOOP
            COMSTAT   pSocket,STAT
            UNPACK    STAT,msgstat
            IF        (msgstat.errorF = 1 )
              SETFLAG                   OVER
              SETFLAG                   ZERO
              SETFLAG                   EOS
              SETFLAG                   LESS
              RETURN
            ELSE IF   (msgstat.ConnectF = 0 )
              CALL            CLRFLAG
              SETFLAG                   OVER
              SETFLAG                   ZERO
              RETURN
            ENDIF
            UNTIL     (msgstat.SendF = 1 )
            
            
            Call    ClearEvents
            PAUSE     "1"                           ;one second delay
            IF        (uTimeout)
              INCR            tmout
              IF              (TMOUT=uTimeout)
                CALL                      CLRFLAG
                SETFLAG         ZERO
                RETURN
              ENDIF
            ENDIF
          REPEAT
.
          COMWRITE  pSocket;*ll,pMSG
          CALL      CLRFLAG
          FunctionEnd

* .............................................................................
.
. f?hrt die anliegenden events aus und l?scht falls unerwartet neue events kamen diese noch
          
ClearEvents         Function

          Entry

          Loop
          eventcount          $result
          break if  ZERO
          eventcheck          
          repeat

          Loop
          clearevent
          repeat until over

          FunctionEnd

          
* .............................................................................
.
. erstellt die Socket Verbindung auf der geh?rt wird ob eine Verbindung rein kommt.
.
. Wird nur im Aktiv Modus verwendet da dort der Server die Verbindung zum Client aufbaut
.
.
SOCKETCREATE        Function
pSocket   COMFILE   ^
pHost     DIM       ^
pPort     DIM       ^
uMode     DIM       1
          Entry
MyConnectString     DIM       300

          $LL       uMode
          if        EOS
          Pack      uMode,"R"
          endif
          
          PACK      MyConnectString,"S,C,",pHost,",",pPort,",",uMode
          Exceptset opfail if io
          COMOPEN   pSocket,MyConnectString
          Exceptclear         io

          CALL      CLRFLAG
          Return    using wf0
opfail
          FunctionEnd         using wf1
          
CLRFLAG   SETFLAG   NOT ZERO
          SETFLAG   NOT OVER
          SETFLAG   NOT EOS
          SETFLAG   NOT LESS
          RETURN    
          
* .............................................................................
.
. ?bermittelt den Port der Verbindung an den ftp Server um dar?ber zu kommunizieren
.
. wird nur im akktiv Modus verwendet da dort der Server wissen muss auf welchem Port
. er sich Verbinden muss um eine datei zu senden
.
SetPort   Function
pSocket   COMFILE   ^
pLocalip  DIM       ^
uLocalPort          form      10
          Entry
myint1    INTEGER   2
          MOVE      ulocalport,myint1
          MOVE      (myint1/256),p1               ;should be high byte
          MOVE      (myint1-(p1*256)),p2                    ;should be low byte
          MOVE      P1,D1
          MOVE      P2,D2
          LOOP
            CMATCH    D1," "
            BREAK IF NOT EQUAL
            IF EQUAL
              BUMP            D1
            ENDIF
          REPEAT
          LOOP
            CMATCH    D2," "
            BREAK IF NOT EQUAL
            IF EQUAL
              BUMP            D2
            ENDIF
          REPEAT
.         
          REPLACE   dtoc,pLocalip
          PACK      wdFTPWork,"PORT ",pLocalip,",",d1,",",d2,cr       ;,lf
          REPLACE   ctod,pLocalip
.
          CALL      send USING pSocket,wdFTPWork,tmout
          CALL      recv USING pSocket,wdFTPWork,tmout
          UNPACK    wdFTPWork,ftpresp
.
          FunctionEnd         
* .............................................................................
.
. Eine Aufl?sung der Antworten des Ftp Servers
.
. Sind momentan noch in Englisch
.
.
.
.
ErrorTable          Function
uError    DIM       3
          Entry
          
myErrorMsg          DIM       400

          switch    uError
          
          case      "110"
          move      "Restart marker reply." to myErrorMsg
          case      "120"
          move      "Service ready in (n) minutes." to myErrorMsg
          case      "125"
          move      "Data connection already open, transfer starting." to myErrorMsg
          case      "150"
          move      "File status okay, about to open data connection." to myErrorMsg
          case      "200"
          move      "Command okay." to myErrorMsg
          case      "202"
          move      "Command not implemented" to myErrorMsg
          case      "211"
          move      "System status, or system help reply." to myErrorMsg
          case      "212"
          move      "Directory status." to myErrorMsg
          case      "213"
          move      "File status." to myErrorMsg
          case      "214"
          move      "Help message." to myErrorMsg
          case      "215"
          move      "NAME system type. (NAME is an official system name from the list in the Assigned Numbers document.)" to myErrorMsg
          case      "220"
          move      "Service ready for new user." to myErrorMsg
          case      "221"
          move      "Service closing control connection. (Logged out if appropriate.)" to myErrorMsg
          case      "225"
          move      "Data connection open, no transfer in progress." to myErrorMsg
          case      "226"
          move      "Closing data connection. Requested file action successful (file transfer, abort, etc.)." to myErrorMsg
          case      "227"
          move      "Entering Passive Mode" to myErrorMsg
          case      "230"
          move      "User logged in, proceed." to myErrorMsg
          case      "250"
          move      "Requested file action okay, completed." to myErrorMsg
          case      "257"
          move      "PATHNAME created." to myErrorMsg
          case      "300"     
          move      "The command has been accepted, but the requested action is being held pending receipt of further information." to myErrorMsg
          case      "331"
          move      "User name okay, need password." to myErrorMsg
          case      "332"
          move      "Need account for login." to myErrorMsg
          case      "350"
          move      "Requested file action pending further information." to myErrorMsg
          case      "400"
          move      "The command was not accepted and the requested action did not take place. The error condition is temporary, however, and the action may be requested again." to myErrorMsg
          case      "421"
          move      "Service not available, closing control connection. (May be a reply to any command if the service knows it must shut down.)" to myErrorMsg
          case      "425"
          move      "Can't open data connection." to myErrorMsg
          case      "426"
          move      "Connection closed, transfer aborted." to myErrorMsg
          case      "450"
          move      "Requested file action not taken. File unavailable (e.g., file busy)." to myErrorMsg
          case      "451"
          move      "Requested action aborted, local error in processing." to myErrorMsg
          case      "452"
          move      "Requested action not taken. Insufficient storage space in system." to myErrorMsg
          case      "500"
..        move      "The command was not accepted and the requested action did not take place." to myErrorMsg
..        case      "500"
          move      "Syntax error, command unrecognized. This may include errors such as command line too long." to myErrorMsg
          case      "501"
          move      "Syntax error in parameters or arguments." to myErrorMsg
          case      "502"
          move      "Command not implemented." to myErrorMsg
          case      "503"
          move      "Bad sequence of commands." to myErrorMsg
          case      "504"
          move      "Command not implemented for that parameter." to myErrorMsg
          case      "530"
          move      "User not logged in." to myErrorMsg
          case      "532"
          move      "Need account for storing files." to myErrorMsg
          case      "550"
          move      "Requested action not taken. File unavailable (e.g., file not found, no access)." to myErrorMsg
          case      "552"
          move      "Requested file action aborted, storage allocation exceeded" to myErrorMsg
          case      "553"     
          move      "Requested action not taken. Illegal file name." to myErrorMsg

          default
          
          move      "Ein Unbekannter Fehler ist aufgetreten." to myErrorMsg

          endswitch
                    
          FunctionEnd using myErrorMsg
          
          
          
          Stop