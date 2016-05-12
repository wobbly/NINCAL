* GETIPNBR.PLS  JSS 3-9-00
.
. VB examples of getting the IP Address.
.   http://www.mvps.org/vbnet/code/network/ipaddress.htm
.   http://www.mvps.org/vbnet/code/network/macaddress.htm
.
. 3-10-00 SQUEEZE BLANKS OUT OF IP#
. 3-22-00 CLEAR INPUT STRING IF FAILURE, CHECK FOR AT LEAST 15 CHAR
. 9-12-00 Gerhard Weiss Replaced external routine "GSTRVAL" with MoveMemory API
.
LoadLibrary profile Kernel32,LoadLibraryA,INT4,DIM
.
.
.BOOL FreeLibrary(
.    HMODULE  hLibModule      // handle of loaded library module  
.   );    
.Parameters
.hLibModule          Identifies the loaded library module. The LoadLibrary or
.                    GetModuleHandle function returns this handle. 
.Return Value:       If the function succeeds, the return value is TRUE.

FreeLibrary PROFILE Kernel32,FreeLibrary,INT4,INT4


LIBRET    INTEGER   4
RET       INTEGER   4
DATA      DIM       255
HEX0      INIT      0x00
WSADATA   DIM       4096
I11       INTEGER   1
I12       INTEGER   1
I13       INTEGER   1
I14       INTEGER   1
F11       FORM      3
F12       FORM      3
F13       FORM      3
F14       FORM      3
DIM4      DIM       4
.int WSAStartup( 
.    WORD wVersionRequested,  
.    LPWSADATA lpWSAData      
.   );
.int WSAGetLastError(void);
.int WSACleanup(void);

I41       INTEGER   4


WSAGetLastError PROFILE Wsock32.dll,WSAGetLastError, INT4

GetLastError PROFILE kernel32,GetLastError, INT4

WSAVER    INTEGER   2,"0x0101"

WSAStartup PROFILE Wsock32.dll,WSAStartup,INT1,INT2,DIM

WSACleanup PROFILE Wsock32.dll,WSACleanup, INT4

gethostname PROFILE Wsock32.dll,gethostname, INT1,DIM,INT1

.struct hostent FAR * gethostbyname ( 
.    const char FAR * name    
.   );

gethostbyname PROFILE Wsock32.dll,gethostbyname,INT4, DIM
.
.
I21       INTEGER   2
I22       INTEGER   2
SZDESC    DIM       256
SZSYSSTAT DIM       256
HOSTNAME  DIM       200
HNAME2    DIM       255

I42       INTEGER   4
I43       INTEGER   4
I44       INTEGER   4
..DIM4      DIM       4
...
P_STRG    DIM       ^
RESULT    FORM      9
LOADED    FORM      1
....
ANS       DIM       1
..

         
         DISPLAY   *ES,"Get IP Address",*N
.
         CALL      GETIPNBR USING DATA
         IF        OVER
         
           KEYIN   "OVER FLAG ",*DV,*LL,DATA:
                   *N,"Tap any key...",*+,ANS

         ELSE

           KEYIN   "The IP Number Is: ",*DV,*LL,DATA:
                   *N,"Tap any key...",*+,ANS

         ENDIF
         WINAPI    FreeLibrary GIVING RET USING LIBRET
         STOP

GETIPNBR ROUTINE   P_STRG
         MOVEPLEN  P_STRG,RESULT
         IF        ( RESULT < 15 )
           CLEAR   P_STRG
           SETFLAG OVER
           RETURN
         ENDIF  
         PACK      DATA WITH "Wsock32.dll",HEX0
         WINAPI    LoadLibrary giving LIBRET using DATA
         WINAPI    WSAStartup GIVING I11 USING WSAVER,WSADATA
         UNPACK    WSADATA TO I21,I22
.         DISPLAY   "I21:",I21,*N,"I22:",I22
         RESET     WSADATA, 5
         SCAN      HEX0,WSADATA
         IF        EQUAL
           BUMP    WSADATA, -1
           LENSET  WSADATA
           RESET   WSADATA,5
           MOVE    WSADATA,SZDESC
           ENDSET  WSADATA
           SETLPTR WSADATA
           BUMP    WSADATA, 2
           MOVEFPTR WSADATA,RESULT         
         ENDIF  
         SCAN      HEX0,WSADATA
         IF        EQUAL
           BUMP    WSADATA, -1
           LENSET  WSADATA
           RESET   WSADATA,RESULT
           PARSE   WSADATA,SZSYSSTAT," ~"
           ENDSET  WSADATA
           SETLPTR WSADATA            
           BUMP    WSADATA, 2
         ENDIF  
.         DISPLAY   "WSA DESC:",SZDESC:
.                   *N,"WSA SYS STAT:",SZSYSSTAT
         MOVE      "200",I12
         WINAPI    gethostname GIVING I11 USING HOSTNAME,I12
         IF        ( I11 != 0 )
           WINAPI  WSAGetLastError GIVING I41
           MOVE    I41,RESULT
.           PACK    DATA WITH "gethostname error: ",RESULT
.           ALERT   NOTE,DATA,RESULT,"Get IP##"
           CALL    WSACLOSE
           CLEAR   P_STRG
           SETFLAG OVER
           RETURN
         ENDIF  
.         DISPLAY   "I11:",I11:
.                   *N,"I12:",I41:
.                   *N,"HOSTNAME:",HOSTNAME
.         KEYIN     "...",ANS
         CHOP      HOSTNAME,DATA
         PACK      HOSTNAME WITH DATA,HEX0
         WINAPI    gethostbyname GIVING I42 USING HOSTNAME
         IF        ( I42 = 0 )
           CALL    WSAGET_LAST_ERROR
           MOVE    RET,RESULT
.           PACK    DATA WITH "GetHostByName error: ",RESULT
.           ALERT   NOTE,DATA,RESULT,"Get IP##"
           CALL    WSACLOSE
           CLEAR   P_STRG
           SETFLAG OVER
           RETURN
         ENDIF  
         MOVE      "2",I11
         MOVE      I42,I43   // begginning addr of HOSTENT structure
         ADD       "12",I43     // skip to last item in structure
..         CALLS     "GSTRVAL" USING I43,I42,I11
..         $GETVAL   I43,I42,I11  // I42 is addr of array of addrs of IPs
         CALL      MoveMemory USING DIM4,I43,(4)
         MOVE      DIM4,I42
.
         MOVE      I42,I43
..         CALLS     "GSTRVAL" USING I43,I42,I11          
..         $GETVAL   I43,I42,I11  // I42 is 1st addr from array
         CALL      MoveMemory USING DIM4,I43,(4)
         MOVE      DIM4,I42
.
         MOVE      I42,I43
..         CALLS     "GSTRVAL" USING I43,I42,I11
..         $GETVAL   I43,I42,I11  // I42 is contents of this IP address
         CALL      MoveMemory USING DIM4,I43,(4)
         MOVE      DIM4,I42
.
         MOVE      I42,DIM4
         UNPACK    DIM4,I11,I12,I13,I14
         MOVE      I11,F11
         MOVE      I12,F12
         MOVE      I13,F13
         MOVE      I14,F14
         PACK      P_STRG, F11,".",F12,".",F13,".",F14
         SQUEEZE   P_STRG,P_STRG
         CALL      WSACLOSE
         SETFLAG   NOT OVER
         RETURN
.
WSACLOSE
         WINAPI    WSACleanup GIVING RET
         RETURN
.          
WSAGET_LAST_ERROR
         WINAPI    WSAGetLastError GIVING RET
         RETURN
.          
GET_LAST_ERROR
         WINAPI    GetLastError GIVING RET
         RETURN
................................................................................
. MoveMemory - The MoveMemory function moves/copies a block of memory from one
. location to another. 
.
. VOID MoveMemory (
.   PVOID  Destination,       // address of move destination 
.   CONST  VOID *  Source,    // address of block to move 
.   DWORD  Length             // size, in bytes, of block to move  
.   );    
.
apiMoveMemory PROFILE Kernel32,RtlMoveMemory,None,Dim,Int4,Int4
.
#strMoveMemoryDest DIM ^
#lngMoveMemoryLpt INTEGER ^
#lngMoveMemoryLen INTEGER 4
.
MoveMemory LROUTINE #strMoveMemoryDest,#lngMoveMemoryLpt:
                     #lngMoveMemoryLen
         WINAPI    apiMoveMemory USING #strMoveMemoryDest:
                     #lngMoveMemoryLpt,#lngMoveMemoryLen
         RETURN
........................................