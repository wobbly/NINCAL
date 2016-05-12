PC       EQU       0
 	include common.inc
	include cons.inc

ASocket          Comfile
ASockType        Init            "S"                    ; S=Windows Socket
AHostConnection  Dim             26                     ; Work strorage
AConnected       Form            1                      ; 0=Not Connected,1=Connected
AStatus          Dim             20                     ; Socket status
AWriting         Form            1                      ; 0=Wrt Disabled,1=Wrt Enabled
AReading         Form            1                      ; 0=Read Disabled,1=Read Enabled
ASocketError     Form            1                      ; 0=NO error,1=Error
AGenericError    Dim             8                      ; Coms generic error code
ANativeError     Dim             8                      ; Coms Native error code
ALastRead        Form            "0"                    ; Final Read buffer status 0=Bad,1=Good
ASocketOpen      Form            "0"                    ; 0=Socket Closed,1=Socket Open
ADropped         Form            "0"                    ; Connection was dropped since prev read
AComma           Init            ","                    ; Just what it is
AReadStatus      Dim             10                     ; Read status for the Logfile
AWriteStatus     Dim             10                     ; Write status for the Logfile

ALastWrite        Form           "0"                    ; Final Read buffer status 0=Bad,1=Good
ANull             Init           0
AF1               Form           1
AF2               Form           2
AD1               Dim            1
AD2               Dim            2
AD4               Dim            4
APortD            Dim            4
AReadRetry        Form           6
AWriteRetry       Form           6
ALog              Form           1
ADelCont          Init           0x0D,"*",0x0A,"*"
ABlanks           Init           "                                        ":
                                "                                        "

.   SLEEP wait timer Api call
ATRUE             Integer         4,"1"                 ; TRUE
AResult           Integer         4,"0"
ASleepTime        Integer         4,"100"               ; Our wait timer * Milli seconds
ASleepProfile     Profile         kernel32.Dll,SleepEx,Int4,Int4,Int4

release     init "13.0"
tester
  call paint
  pack Ahostconnection with "S,0,NTS2,25,R"
  comopen Asocket,Ahostconnection
  
  
                Move         "0",AConnected                   ;Assume NOT connected
                Move         "0",AWriting                     ;Assume Writing NOT allowed
                Move         "0",AReading                     ;Assume Reading NOT allowed
                Move         "1",ASocketError                 ;Assume we have a socket error
                Move         "        ",AGenericError         ;Clear any prev error codes
                Move         "        ",ANativeError          ;Clear any prev error codes


                Move        "1",ADropped                     ;Assume that the socket dropped
                 Trap        SocketNOTopen if IO             ;Incase the socket dropped
                 Comstat     ASocket,AStatus                   ;Get the Socket status
                 Trapclr     IO                              ;Remove trap
                 Move        "0",ADropped                     ;Line did not drop, set status
                 Unpack      AStatus,AConnected:               ;Decode receive socket status
                                    AWriting:
                                    AReading:
                                    ASocketError:
                                    AGenericError:
                                    ANativeError
  DISPLAY   *P10:12,AWriting
  DISPLAY   *P10:24,AREading  
  DISPLAY   *P10:36,Asocketerror  
  DISPLAY   *P10:48,Agenericerror
  DISPLAY   *P10:60,Anativeerror
sOCKETNOTOPEN
  DISPLAY   *P10:72,"SOCKET NOT open"
  pause     "55"

 	include comlogic.inc
