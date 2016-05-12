*******************************************************************************
*                                                                             *     
*     Program Name         : Socket.RtP Version 1.0                           *     
*                                                                             *     
*     For ROUTINE .PLC     : Socket.PLC ( Source is Socket.RtS )              *     
*                                                                             *     
*     Type of program      : Windows socket driver for Plb                    *
*                                                                             *     
*     Use                  : To interface to Socket.Plc                       *     
*                                                                             *     
*     Author               : Festus Redelinghuys - plbsa@icon.co.za           *
*                                                                             *     
*     Date                 : 01 November 1998                                 *     
*                                                                             *     
*     Copyright            : Freeware                                         *     
*                                                                             *     
*******************************************************************************
                           Goto JumpWinSockP

.   Variable types and sizes used by CALLS
.   All variables initialized with values("xx") are the defaults for that variable
.   If the default for the variable is acceptable then you don't have to change it
.   Values that are not quoted (" ") need not to be changed at all exept where
.   indicated by an "*" which MUST be changed/set.

SocketReturnStatus     Form           "00"       LROUTINE return status
SocketHostIP           Dim            60        *Host IP number OR Name
SocketPort             Form           4         *'LISTEN' Port number
SocketMethod           Init           "O"        O=Open,C=Create
SocketPacketType       Init           "R"        R=Raw,C=Cooked
SocketReadRetry        Form           "000300"   300 Loops * 100 Millisecond delays = 30 sec
SocketWriteRetry       Form           "000300"   300 Loops * 100 Millisecond delays = 30 sec
SocketszRxBuffer       Form           "1024"     Rx buffer size(Change size here)
SocketszTxBuffer       Form           "1024"     Tx buffer size(Change size here)
SocketRxBuffer         Dim            4096       Receive buffer(do not change size)
SocketTxBuffer         Dim            4096       Transmit buffer(do not change size)
SocketLogFile          Dim            260        Valid path/filename of log
SocketDispMode         Form           "0"        Display Logfile entries as they are made
SocketETX              Init           0x0A,0x0D  Read End of TeX characters - CRLF
SocketFilter           Init           0x0A,0x0D  Read filter out characters - CRLF
SocketTime             Form           "9"        Read timeout between characters(3)

.   Notes:
.   1. If the comport is not ready or is busy read/writes can be put in a loop
.      to wait for the port to become ready. A delay timer of 100 Milliseconds
.      has been build in. The delay component is a WinApi routine which uses
.      the SleepEx routine from kernel32.Dll. The properties of SleepEx is
.      very low system overhead achieved by SleepEx giving up multi tasking
.      time slicing for the duration of the delay( 100 Milliseconds)
.      Comport activity would activate SleepEx whether the 100
.      millisecond timeout has elapsed or not.This method is far more effective
.      than using any loops to create a delay.
.   2. Port - Ports 0-4999 is reserved by Microsoft
.           - Use port number 5000+ for custom servers
.           - Use port 25 for e-mail servers
.   3. RxBuffer and TxBuffer. Keep the variable declararion of these two
.      variables at 4096 (Max buff size) and change szRxBuffer and szTxBuffer
.      if you need smaller buffer sizes.
.   LROUTINE routines in Socket.Plc

...............................................................................
.   Connect - Connect to the TCP/IP server
.
.   On Entry - Set all variables in CALLS ( Excl ReturnStatus)
.   On Exit  - All variables unchanged    ( Excl ReturnStatus)
.            - ReturnStatus =
.                             "00" - Invalid return code
.                             "01" - SUCCESSFULL
.                             "02" - Buffer size to big 
.                             "03" - Invalid parameters error
SocketConnect
.   Format to our required buffer size

                Sformat         SocketRxBuffer,SocketszRxBuffer    ;Format buffer size
                Sformat         SocketTxBuffer,SocketszTxBuffer    ;Format buffer size

.   Connect the Winsock

                CALLS           "Winsock2.PLC;Connect" Using SocketReturnStatus:
                                SocketHostIP:          Host IP number
                                SocketPort:            'LISTEN' Port number
                                SocketMethod:          O=Open,C=Create
                                SocketPacketType:      R=Raw,C=Cooked
                                SocketReadRetry:       Loop counter
                                SocketWriteRetry:      Loop counter
                                SocketRxBuffer:        Receive buffer
                                SocketTxBuffer:        Transmit buffer
                                SocketszRxBuffer:      RxBuffer size
                                SocketszTxBuffer:      TxBuffer size
                                SocketLogFile:         Log all IO
                                SocketDispMode:        Display Logfile entries as they are made
                                SocketETX:             Read End of TeX characters
                                SocketFilter:          Read filter out characters
                                SocketTime             Read timeout between characters(3)

                If              ( SocketReturnStatus != "1" )
                 SetFlag        Not Equal
                 RETURN                                Return with flag
                Else
                 SetFlag        Equal
                 RETURN                                Return with flag
                Endif
...............................................................................
.   Read a buffer of data from the server
.
.   On Entry - Don't care
.   On Exit  - RxBuffer     =      - Contains data read from server
.            - ReturnStatus =
.                             "00" - Invalid return code
.                             "01" - SUCCESSFULL
.                             "02" - Socket not initialized
.                             "03" - Connection lost
.                             "04" - Error during socket read
.                             "05" - Last read was bad
.                             "06" - Error getting last status
SocketRead
                CALLS           "Winsock2.PLC;Read" Using SocketReturnStatus:
                                SocketRxBuffer         Buffer data read
                If              ( SocketReturnStatus != "1" )
                 SetFlag        Not Equal
                 RETURN                                Return with flag
                Else
                 SetFlag        Equal
                 RETURN                                Return with flag
                Endif
...............................................................................
.   Write a buffer of data to the server
.
.   On Entry - TxBuffer must contain data
.   On Exit  - TxBuffer     =      - Unchanged
.            - RxBuffer     =      - Invalid contents(Don't care)
.            - ReturnStatus =
.                             "00" - Invalid return code
.                             "01" - SUCCESSFULL
.                             "02" - Socket not initialized
.                             "03" - Connection lost
.                             "04" - Error during socket write
.                             "05" - Last write was bad
.                             "06" - Error getting last status
SocketWrite
                CALLS           "Winsock2.plc;Write" Using SocketReturnStatus: 
                                SocketTxBuffer         Buffer data write
                If              ( SocketReturnStatus != "1" )
                 SetFlag        Not Equal
                 RETURN                                Return with flag
                Else
                 SetFlag        Equal
                 RETURN                                Return with flag
                Endif
...............................................................................
.   Write a buffer of data to the server
.
.   On Entry - Don't care
.   On Exit  -              =      - All unchanged
.            - ReturnStatus =
.                             "00" - Invalid return code
.                             "01" - SUCCESSFULL       Always 1
SocketClose
                CALLS           "Winsock2.plc;Close" Using SocketReturnStatus 
                If              ( SocketReturnStatus != "1" )
                 SetFlag        Not Equal
                 RETURN                                Return with flag
                Else
                 SetFlag        Equal
                 RETURN                                Return with flag
                Endif
...............................................................................
JumpWinSockP
.                            End of WinSockP.Dbs
...............................................................................

