*******************************************************************************
*                                                                             *     
*     Program Name         : Socket.RtS Version 1.1                           *     
*                                                                             *     
*     Type of program      : Windows socket driver                            *     
*                                                                             *     
*     Compilation          : Compile standalone as a ROUTINE                  *     
*                                                                             *     
*     Author               : Festus Redelinghuys - plbsa@icon.co.za           *
*                                                                             *     
*     Date                 : 01 November 1998                                 *     
*                                                                             *     
*     Copyright            : All rights reserved                              *     
*                                                                             *     
*     Upgrade - Version    : 1.1 11/14/98                                     *     
*                            Changed error codes,Added error codes text list  *     
*                                                                             *     
*******************************************************************************

.   Error codes returned from winsock
.   Values returned in SocketStatus

ErrorList Init "00 - Error Unknown                                                              ":
               "01 - Successfull, NO errors                                                     ":
               "02 - Reserved - For use by other application ( Eq. Smtp )                       ":
               "03 - Reserved                                                                   ":
               "04 - Host IP is invalid. ( LP=0)                                                ":
               "05 - Server 'Listen' port number invalid ( port = 0)                            ":
               "06 - Server 'Listen' port number invalid ( port = EOS)                          ":
               "07 - Invalid port open method. Must be 'O' or 'C'                               ":
               "08 - Invalid data packet type. Must be 'R' or 'C'                               ":
               "09 - Invalid buffer sizes. Buffer sizes must be equal or less than 4096         ":
               "10 - Invalid communication port parameters                                      ":
               "11 - Connection error during connection - Socket error                          ":
               "12 - Socket not open when it should have been                                   ":
               "13 - Socket not open when it should have been                                   ":
               "14 - Not connected when connection should have been                             ":
               "15 - Socket error during last read                                              ":
               "16 - Socket error during last read                                              ":
               "17 - Socket/line was dropped during last reception                              ":
               "30 - Socket not open during write                                               ":
               "31 - Connection lost during transmission                                        ":
               "32 - Socket error during write                                                  ":
               "33 - Error during last write                                                    ":
               "34 - Socket/line was dropped during last transmission                           ":
               "35 - WinSock.Rtn variable pointers NOT initialized,Call SocketConnect first.    "
ScanCode       Dim              5                      ; Error list scan key

.   Windows socket handler

Socket          Comfile
SockType        Init            "S"                    ; S=Windows Socket
HostConnection  Dim             26                     ; Work strorage
Connected       Form            1                      ; 0=Not Connected,1=Connected
Status          Dim             20                     ; Socket status
Writing         Form            1                      ; 0=Wrt Disabled,1=Wrt Enabled
Reading         Form            1                      ; 0=Read Disabled,1=Read Enabled
SocketError     Form            1                      ; 0=NO error,1=Error
GenericError    Dim             8                      ; Coms generic error code
NativeError     Dim             8                      ; Coms Native error code
LastRead        Form            "0"                    ; Final Read buffer status 0=Bad,1=Good
SocketOpen      Form            "0"                    ; 0=Socket Closed,1=Socket Open
Dropped         Form            "0"                    ; Connection was dropped since prev read
Comma           Init            ","                    ; Just what it is
ReadStatus      Dim             10                     ; Read status for the Logfile
WriteStatus     Dim             10                     ; Write status for the Logfile

LastWrite        Form           "0"                    ; Final Read buffer status 0=Bad,1=Good
Null             Init           0
F1               Form           1
F2               Form           2
D1               Dim            1
D2               Dim            2
D4               Dim            4
PortD            Dim            4
ReadRetry        Form           6
WriteRetry       Form           6
Log              Form           1
DelCont          Init           0x0D,"*",0x0A,"*"
Blanks           Init           "                                        ":
                                "                                        "

.   SLEEP wait timer Api call
TRUE             Integer         4,"1"                 ; TRUE
Result           Integer         4,"0"
SleepTime        Integer         4,"100"               ; Our wait timer * Milli seconds
SleepProfile     Profile         kernel32.Dll,SleepEx,Int4,Int4,Int4

.==============================================================================
.   This routine does the TCP/IP connection to the server

SocketStatus     Form            ^                      ; LROUTINE return status
StatusText       Dim             ^                      ; Human readable text for above code
HostIP           Dim             ^                      ; Host IP number
Port             Form            ^                      ; 'LISTEN' Port number
Method           Dim             ^                      ; O=Open,C=Create
PacketType       Dim             ^                      ; R=Raw,C=Cooked
SocketReadRetry  Form            ^                      ; Loop counter
SocketWriteRetry Form            ^                      ; Loop counter
RxBuffer         Dim             ^                      ; Receive buffer
TxBuffer         Dim             ^                      ; Transmit buffer
szRxBuffer       Form            ^                      ; Rx buffer size(less one for Zero Term)
szTxBuffer       Form            ^                      ; Tx buffer size(less one for Zero Term)
LogFile          Dim             ^                      ; Log all IO to file specified
DispMode         Form            ^                      ; Display Logfile entries as they are made
ETX              Dim             ^                      ; Read End of TeX characters
Filter           Dim             ^                      ; Read filter out characters
Time             Form            ^                      ; Read timeout between characters(3)

Connect          ROUTINE        SocketStatus:          ; Status returned 
                                StatusText:            ; Human readable text for above code
                                HostIP:                ; Host IP number
                                Port:                  ; 'LISTEN' Port number
                                Method:                ; O=Open,C=Create
                                PacketType:            ; R=Raw,C=Cooked
                                SocketReadRetry:       ; Loop counter
                                SocketWriteRetry:      ; Loop counter
                                RxBuffer:              ; Receive buffer
                                TxBuffer:              ; Transmit buffer
                                szRxBuffer:            ; RxBuffer size
                                szTxBuffer:            ; TxBuffer size
                                LogFile:               ; Log all IO to file specified
                                DispMode:              ; Verify only,disable actual IO
                                ETX:                   ; Read End of TeX characters
                                Filter:                ; Read filter out characters
                                Time                   ; Read timeout between characters(3)

.   Validate info passed to LROUTINE
                MoveLPtr        HostIP,F2              ; Have we got anything in there
                If              ( F2 < 1 )
                 Move           "04",SocketStatus      ; Invalid parameter error
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error message
                Endif

.   Properly format and validate the server port

                If              ( Port < 0 )
                 Move           "05",SocketStatus      ; Invalid parameter error
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error message
                Else
                 Move           Port,PortD
                 Loop
                  Cmatch        " ",PortD
                 While          Equal
                  Bump          PortD,1
                  If            EOS
                   Move         "06",SocketStatus      ; Invalid parameter error
                   Call         ErrorText              ; Convert to human readable text
                   RETURN
                  Endif
                 REPEAT
                 Move           PortD,D4
                 Pack           PortD,D4
                Endif

.   Validate local port open or create

                Cmatch          "O",Method
                If              Not equal
                 Cmatch         "C",Method
                 If             Not equal
                  Move          "07",SocketStatus       ; Invalid parameter error
                  Call          ErrorText               ; Convert to human readable text
                  RETURN                                ; With error message
                 Endif
                Endif

.   Validate Data packet types

                Cmatch          "R",PacketType
                If              Not equal
                 Cmatch         "C",PacketType
                 If             Not equal
                  Move          "08",SocketStatus       ; Invalid parameter error
                  Call          ErrorText               ; Convert to human readable text
                  RETURN                                ; With error message
                 Endif
                Endif

.   Validate buffer request size

                If              ( szRxBuffer > 4096 | szTxBuffer > 4096)
                 Move           "09",SocketStatus      ; Buffer size to big error message
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error message
                Else
..                 Sformat        RxBuffer,szRxBuffer    ;Format buffer size
..                 Sformat        TxBuffer,szTxBuffer    ;Format buffer size
                Endif

.   Validate if we must log IO

                MoveLPtr         LogFile,F2            ;If path/filename then
                If               ( F2 > 1 )            ;we log all io subject
                 Move            "1",Log               ;to a valid path and
                Else                                   ;file name.We don't 
                 Move            "0",Log               ;test if the file exist.
                Endif                                  ;We just prepare it.

.   Initialize the Rx and Tx buffers.

                Move            " ",RxBuffer
                Move            " ",TxBuffer

.   Initialize the Windows socket and return the status

                Call            OpenSocket             ; Open coms port

.   Respond to any errors

                If              ( Connected = 0 )      ; Connect error
                 Move           "10",SocketStatus      ; Invalid server/com port parameters
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error message
                Endif
                If              ( SocketError = 1 )    ; Connect error
                 Move           "11",SocketStatus      ; Invalid server/com port parameters
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error message
                Endif
                If              ( SocketOpen = 0 )     ; Connect error
                 Move           "12",SocketStatus      ; Invalid server/com port parameters
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error message
                Endif

                Move            "1",SocketStatus       ; SUCCESSFULL
                Call           ErrorText              ; Convert to human readable text
                RETURN

...............................................................................
.   Read the windows socket
Read            ROUTINE                                                            
                IF              ( SocketOpen = 0 )
                 Move           "13",SocketStatus      ; Socket not initialized
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error code
                Else If         ( Connected = 0 )
                 Move           "14",SocketStatus      ; Connection lost
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error code
                Endif

                Call            ReadSocket             ; Read windows socket

                If              ( SocketError = 1 )
                 Move           "15",SocketStatus      ; Error during socket read
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error code
                Else If         ( LastRead != 1 ) 
                 Move           "16",SocketStatus      ; Last read was bad
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error code
                Else If         ( Dropped = 1 ) 
                 Move           "17",SocketStatus      ; Error getting last status
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error code
                Endif 

                Move            "01",SocketStatus      ; SUCCESSFULL
                Call            ErrorText              ; Convert to human readable text
                RETURN

...............................................................................
.   Write to the windows socket
Write           ROUTINE                                                           

                IF              ( SocketOpen = 0 )
                 Move           "30",SocketStatus      ; Socket not initialized
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error code
                Else If         ( Connected = 0 )
                 Move           "31",SocketStatus      ; Connection lost
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error code
                Endif

                Call            WriteSocket            ; Read windows socket

                If              ( SocketError = 1 )
                 Move           "32",SocketStatus      ; Error during socket read
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error code
                Else If         ( LastWrite != 1 ) 
                 Move           "33",SocketStatus      ; Last read was bad
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error code
                Else If         ( Dropped = 1 ) 
                 Move           "34",SocketStatus      ; Error getting last status
                 Call           ErrorText              ; Convert to human readable text
                 RETURN                                ; With error code
                Endif 

                Move            "01",SocketStatus      ; SUCCESSFULL
                Call            ErrorText              ; Convert to human readable text
                RETURN
...............................................................................
.   Close the socket - Will always return TRUE
Close           ROUTINE

                Call            CloseSocket            ; Close the socket
                Call            LogC If ( Log = 1 )    ; Close logfile if open 
                SetFlag         Equal               
                RETURN

.==============================================================================
.                               SUB ROUTINES
.==============================================================================

.   Open the Windows Socket.
.   When we call GetSocketStatus we also test to see if the connection is still
.   valid ( SocketOpen). If the socket is not 'Connected' or 'SocketOpen' the
.   we do a re-connect.

OPENSOCKET

                 Call           GetSocketStatus                 ;Get the latest status
                 If             ( SocketOpen = 0 )              ;Is the socket open
                  Pack          HostConnection,SockType:        ; S=Windows Socket
                                               Comma:           ; Seperator
                                               Method:          ;
                                               Comma:           ; Seperator
                                               HostIP:          ;
                                               Comma:           ; Seperator
                                               PortD:           ;
                                               Comma:           ; Seperator
                                               PacketType
                  Call          LogO If ( Log = 1 )             ;Log all 
                  Trap          InvalidSocket if IO             ;Respond to ERROR
                  Comopen       Socket,HostConnection           ;Open the socket and connect to the host
                  TrapClr       IO                              ;NO IO error in socket

                  Move          "1",SocketOpen                  ;Socket OPENed
                  Call          GetSocketStatus                 ;Get the latest status
                  If            ( Connected = 0 | SocketError = 1 | Dropped = 1) Error during open
                   Move         "0",Connected                   ;Error connecting to Host
                  Endif
                 Endif
                 RETURN

.   Either the Windows Socket (HostConnection) parameters are incorrect or
.   a connection with the host server on the specified port could not be made.
.   This means that you MUST have a server program running on the host with
.   the same PORT number used. This port number is the port on which the
.   server LISTEN's for new connections before the host hands a connection over
.   to an "internal client on the servers Winsock2". Obviously the server must
.   have a STATIC IP address. Watchout for DHCP on a WinNT system, the IP addr is
.   then dynamic.

InvalidSocket

                Move            "0",Connected                   ;We could not connect
                Move            "0",SocketOpen                  ;Socket is NOT open
                RETURN                                          ;TO ERROR+NEXT LINE

...............................................................................

.   Read the Windows socket.
.   After every read you MUST test ALL the status flags.
.   Wait for READ status to be good before reading
.   the socket. After the read has been completed test the port status again
.   to make sure that it has not changed. If it did then the data received
.   would be in question.
READSOCKET

                 RETURN IF      ( SocketOpen = 0 | Connected = 0 | SocketError = 1 ) ;Open error?,Return with codes

                 Move           "0",LastRead                    ;Assume READ Failure
                 Move           "0",ReadRetry                   ;Avoid indeffinite loop

                 Loop
                  Call          GetSocketStatus                 ;Get the latest status
                  IF            ( SocketError = 1 | Connected = 0 | Dropped = 1) ;Have we got a socket error?
                   If           ( Connected = 1 )               ;Connection dropped?
                    Move        "0",LastRead                    ;Last read is BAD
                    Move        "0",SocketOpen                  ;Remember socket is closed
                    Move        "0",Connected                   ;Set socket close flag
                    Comclose    Socket                          ;Yes,Then close socket
                    SetFlag     NOT Equal                       ;Set exit flag BAD
                    BREAK                                       ;Abort Com Read
                   Endif                  

                  Else If       ( Reading = 1 )                 ;Socket ready to be read?
.Requires 8.2K     Comread      Socket,ETX,Filter;*T=Time,RxBuffer ;Yes, All conditions safe
.for temp bug fix remove *T=Time. dlh 26jul99
                   Comread      Socket,ETX;RxBuffer     ;Yes, All conditions safe
.                   Comread      Socket,ETX;*T=Time,RxBuffer     ;Yes, All conditions safe
                   If           Over                            ;Time out occured?
                    Move        "Timeout  ",ReadStatus          ;Set status for logfile
                    Move        "0",LastRead                    ;Last read is BAD
                   Else IF      EOS                             ;End Of Text char recv'd?
                    Move        "EOT Recvd",ReadStatus          ;Set status for logfile
                    Move        "1",LastRead                    ;Last read is GOOD
                   Else IF      Equal                           ;Normal good read?
                    Move        "Ok Recvd ",ReadStatus          ;Set status for logfile
                    Move        "1",LastRead                    ;Last read is GOOD
                   Else IF      NOT Equal                       ;Error during read?
                    Move        "Error    ",ReadStatus          ;Set status for logfile
                    Move        "0",LastRead                    ;Last read is BAD
                   Endif
                   Call         LogR If ( Log = 1 )             ;Log all
                   Call         GetSocketStatus                 ;See if there are errors during socket read
                   If           ( Connected = 0 | SocketError = 1 | Dropped = 1) ;No errors?
                    Move        "0",LastRead                    ;Something went wrong after read
                   Endif
                   IF           ( Connected = 0 )               ;Must we close the socket
                    Comclose    Socket                          ;Yes,Then close socket
                    Move        "0",LastRead                    ;Last read is BAD
                    Move        "0",SocketOpen                  ;Remember socket is closed
                    Move        "0",Connected                   ;Set socket close flag
                   Endif 
                   SetFlag      Equal                           ;Set exit flag Ok
                   BREAK                                        ;Exit LOOP OK
                  Endif

                  Add           "1",ReadRetry                  ;Socket NOT ready to be read
                 Until          ( ReadRetry > SocketReadRetry) ;Wait some more
                  WinApi        SleepProfile Giving Result Using SleepTime,TRUE Sleep for 100 ms
                 REPEAT                                        ;Go try again
                 Call           LogTr If ( Log = 1 & ReadRetry > SocketReadRetry)
                 RETURN                                        ;To caller with
.                                                               status flags
...............................................................................

.   The Write is basically the same as a read, with the same SocketStatus
.   checking
WRITESOCKET

                 RETURN IF      ( SocketOpen = 0 | Connected = 0 | SocketError = 1 ) ;Open error?,Return with codes

                 Move           "0",LastWrite                   ;Assume Write Failure
                 Move           "0",WriteRetry                  ;Avoid indeffinite loop
                 Loop
                  Call          GetSocketStatus                 ;Get the latest status
                  IF            ( SocketError = 1 | Connected = 0 | Dropped = 1) 
                   If           ( Connected = 1 )               ;Connection dropped?
                    Comclose    Socket                          ;Yes,Then close socket
                    Move        "0",SocketOpen                  ;Remember socket is closed
                    Move        "0",Connected                   ;Set socket close flag
                    SetFlag     NOT Equal                       ;Set exit flag BAD
                    BREAK                                       ;Abort Com write
                   Endif                  
                  Else If       ( Writing = 1 )                 ;Are we ready to write?                   
.                   ComWrite     Socket;*T=Time,*LL,TxBuffer     ;Yes, All conditions safe
                   ComWrite     Socket;*LL,TxBuffer     ;Yes, All conditions safe
                   If           Over                            ;Time out occured?
                    Move        "Timeout  ",WriteStatus         ;Set status for logfile
                    Move        "0",LastWrite                   ;Last write is BAD
                   Else IF      Equal                           ;Normal good read?
                    Move        "Ok Send  ",WriteStatus         ;Set status for logfile
                    Move        "1",LastWrite                   ;Last write is GOOD
                   Else IF      NOT Equal                       ;Error during write?
                    Move        "Error    ",WriteStatus         ;Set status for logfile
                    Move        "0",LastWrite                   ;Last write is BAD
                   Endif
                   Call         LogW If ( Log = 1 )             ;Log all
                   Call         GetSocketStatus                 ;See if there are errors during socket read
                   If           ( Connected = 0 | SocketError = 1 | Dropped = 1) ;No errors?
                    Move        "0",LastWrite                   ;Something went wrong after write
                   Endif
                   IF           ( Connected = 0 )               ;Must we close the socket
                    Comclose    Socket                          ;Yes,Then close socket
                    Move        "0",SocketOpen                  ;Remember socket is closed
                    Move        "0",Connected                   ;Set socket close flag
                   Endif
                   SetFlag      Equal                           ;Set exit flag Ok
                   BREAK
                  Endif
                  Add           "1",WriteRetry                   ;Add another try
                 Until          ( WriteRetry > SocketWriteRetry) ;Something is wrong
                  WinApi        SleepProfile Giving Result Using SleepTime,TRUE
                 REPEAT
                 Call           LogTw If ( Log = 1 & WriteRetry > SocketWriteRetry)

                 RETURN                                          ;To caller with
.                                                                 status flags
...............................................................................

.   This routine reads the status of the Windows Socket

GetSocketStatus

                Move         "0",Connected                   ;Assume NOT connected
                Move         "0",Writing                     ;Assume Writing NOT allowed
                Move         "0",Reading                     ;Assume Reading NOT allowed
                Move         "1",SocketError                 ;Assume we have a socket error
                Move         "        ",GenericError         ;Clear any prev error codes
                Move         "        ",NativeError          ;Clear any prev error codes
                If           ( SocketOpen = 1 )              ;ONLY test stat if socket is open
                 Move        "1",Dropped                     ;Assume that the socket dropped
                 Trap        SocketNOTopen if IO             ;Incase the socket dropped

                 Comstat     Socket,Status                   ;Get the Socket status



                 Trapclr     IO                              ;Remove trap
                 Move        "0",Dropped                     ;Line did not drop, set status
                 Unpack      Status,Connected:               ;Decode receive socket status
                                    Writing:
                                    Reading:
                                    SocketError:
                                    GenericError:
                                    NativeError
                 Call        LogS If ( Log = 1 )             ;Log all
                Endif
                RETURN

.   The socket was closed by the host or some local windows program without our
.   SocketOpen status being updated. This caused an IO error. Correct the problem
.   This is a critical condition insofar as syncronised download from the server.
.   If the line was dropped the the server would also have detected it and the
.   download syncronization between the server and client(us) be lost.
SocketNOTopen
                Move         "1",Dropped                     ;Line was dropped from prev read
                Move         "0",SocketOpen                  ;Socket is NOT open
                Trapclr      IO                              ;Remove trap
                Call         LogE If ( Log = 1 )             ;Log all 
                Noreturn                                     ;Default status already set
                RETURN
...............................................................................
.   Close the socket
CLOSESOCKET

                Move         "0",Connected                   ; NOT connected
                Move         "0",Writing                     ; Writing NOT allowed
                Move         "0",Reading                     ; Reading NOT allowed
                Move         "0",SocketError                 ; No socket error
                Call         LogC If ( Log = 1 )             ; Log all 
                RETURN IF    ( SocketOpen = 0 )              ; Socket should be open
                Comclose     Socket                          ; Yes,Then close socket
                Move         "0",SocketOpen                  ; Socket now closed
                Setflag      Equal                          
                RETURN

...............................................................................
.===============================================================================
.   Log all IO. If 'Log' is 1 then all IO will be logged by these routines.
.   The LogFile will automatically be created for a session

LogRecorder     File
LogEntry        Dim             1000
LogOpen         Form            "0"
Seq             Form            "-1"

.   Prepare the LOG file. The file would it be overwritten if it already exist

PrepLog

                Trap            NoLogFile if IO
                Prepare         LogRecorder,LogFile          ;Create a new log file
                Move            "1",LogOpen                  ;Remember log file is open
                RETURN

.   Any errors during the creation of the log file will result in the logging facility
.   being disabled. 'Log'=1 could be tested after return for successfull logging
.   or not.

NoLogFile
                NORETURN
                Move            "0",LogOpen                  ;Log file NOT ready
                Move            "0",Log                      ;Disable logging
                RETURN                                       ;Abort logging

.   Log all socket read actions

LogR

                Call            PrepLog if ( LogOpen != 1 )  ;Open log file
                Pack            LogEntry,"Read Socket    -": ;Action
                                         ReadStatus:         ;Status returned from read
                                         "-":                ;Seperator
                                         RxBuffer            ;Buffer Data
                Replace         DelCont,LogEntry             ;Convert control characters
                Call            DisplayEntry IF ( DispMode != 0 ) ;Display entries
                Write           LogRecorder,Seq;*LL,LogEntry ;Put in log
                RETURN

.   Log all socket write actions

LogW

                Call            PrepLog if ( LogOpen != 1 )  ;Open log file
                Pack            LogEntry,"Write Socket   -": ;Action
                                         WriteStatus:        ;Status returned from read
                                         "-":                ;Seperator
                                         TxBuffer            ;Buffer Data
                Replace         DelCont,LogEntry             ;Convert control characters
                Call            DisplayEntry IF ( DispMode != 0 ) ;Display entries
                Write           LogRecorder,Seq;*LL,LogEntry ;Put in log
                RETURN

.   Log socket open actions

LogO
                Call            PrepLog if ( LogOpen != 1 )  ;Open log file
                Pack            LogEntry,"Open Socket    -": ;Action
                                         " SockType=",SockType: ; S=Windows Socket
                                         " Meth=",Method:       ;
                                         " Host=",HostIP:       ;
                                         " Port=",PortD:        ;
                                         " Pack=",PacketType
                Replace         DelCont,LogEntry             ;Convert control characters
                Call            DisplayEntry IF ( DispMode != 0 ) ;Display entries
                Write           LogRecorder,Seq;*LL,LogEntry    ;Put in log
                RETURN

.   Log all socket close actions
.   The Logfile session is as long as the socket session
LogC

                If              ( LogOpen = 0 )              ;Is log open
                 Setflag        Equal
                 RETURN
                Else
                 Pack           LogEntry,"Close Socket   -"  ;Action
                 Replace        DelCont,LogEntry             ;Convert control characters
                 Call           DisplayEntry IF ( DispMode != 0 ) ;Display entries
                 Write          LogRecorder,Seq;*LL,LogEntry ;Put in log
                 WEOF           LogRecorder,Seq
                 Close          LogRecorder
                 Move           "0",LogOpen                  ;Remember it is closed
                 Setflag         Equal
                 RETURN
                Endif

.   Log all socket status reads in it's native form
LogS

                Call            PrepLog if ( LogOpen != 1 )  ;Open log file
                Pack            LogEntry,"Socket Status  -": ;Action
                                         " Cnt=",Connected:
                                         " Wr Rdy=",Writing:
                                         " Rd Rdy=",Reading:
                                         " Soc Err=",SocketError:
                                         " Gen Err=",GenericError:
                                         " Nat Err=",NativeError
                Replace         DelCont,LogEntry             ;Convert control characters
                Call            DisplayEntry IF ( DispMode != 0 ) ;Display entries
                Write           LogRecorder,Seq;*LL,LogEntry ;Put in log
                RETURN

.   Log all Socket not open errors. This is normally the result of an
.   invalid connection/IP address

LogE

                Call            PrepLog if ( LogOpen != 1 )  ;Open log file
                Pack            LogEntry,"Socket not Open-": ;Action
                                         HostConnection      ;Buffer Data
                Replace         DelCont,LogEntry             ;Convert control characters
                Call            DisplayEntry IF ( DispMode != 0 ) ;Display entries
                Write           LogRecorder,Seq;*LL,LogEntry ;Put in log
                RETURN

.   Timeout-Read. A timeout has occured by virtue of the SleepEx
.   loop counter being completed without the expected action being completed.
.
LogTr

                Call            PrepLog if ( LogOpen != 1 )  ;Open log file
                Pack            LogEntry,"Timeout - Read -": ;Action
                                         SocketReadRetry     ;Buffer Data
                Replace         DelCont,LogEntry             ;Convert control characters
                Call            DisplayEntry IF ( DispMode != 0 ) ;Display entries
                Write           LogRecorder,Seq;*LL,LogEntry ;Put in log
                RETURN

.   Timeout-Write. A timeout has occured by virtue of the SleepEx
.   loop counter being completed without the expected action being completed.
.
LogTw

                Call            PrepLog if ( LogOpen != 1 )  ;Open log file
                Pack            LogEntry,"Timeout - Write-": ;Action
                                         SocketWriteRetry    ;Buffer Data
                Replace         DelCont,LogEntry             ;Convert control characters
                Call            DisplayEntry IF ( DispMode != 0 ) ;Display entries
                Write           LogRecorder,Seq;*LL,LogEntry ;Put in log
                RETURN

.   If display selected display Logfile entry on screen as entries are made
.   We don't know what application would use this Winsock.Rtn and therefore
.   make sure that cursor possitioning is done under control.

InitCursor      Form            "0"                          ;Init cursor possition

DisplayEntry

                If              ( InitCursor = 0 )           ;Cursor init done?
                 Display        *P1:1," ",*C                 ;Possition cursor
                 Move           "1",InitCursor               ;Remember it is done
                Endif

                Display         *N,LogEntry                  ;Display entry

                If              ( DispMode = 2 )             ;Hold on display requested
                 Keyin          D1                           ;Hold program
                Endif

                RETURN
...............................................................................
.   This routine converts the Socket error code to a human readable text string
.   and returns it to the caller

ErrorText 

        Move    Blanks,StatusText                       Clear out garbage
        Move    SocketStatus,D2                         Get the code
        Replace " 0",D2                                 Make sure in right format
        Pack    ScanCode,D2," - "                       Build a unique scan code
        SetLPtr ErrorList                               .Make sure we use full code list
        Reset   ErrorList                               .From the beginning
        Scan    ScanCode,ErrorList                      Find the error code in out list
        If      Equal                                   We found it!
         Bump   ErrorList,5                             Point to start of text of code
         Move   ErrorList,StatusText                    Get the text message
         SetLPtr StatusText,75                          Cut of extra garbage
        Else                                            We did NOT find it
         Pack   StatusText,"Unknown Error Code = ",ScanCode   We don't have a text for this code
        Endif
        RETURN                                          With a text message

...............................................................................
