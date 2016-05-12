. ftp client in plb version 1.2
.
. Acording to RFC 959, FTP command are sent in a telnet string
.  [cmd][space][param1]...[CR][LF]
.
. note: this ftp client is written mostly in compliance to RFC 959, and 765
. with the acception of the USER DEFAULT DATA PORT in which currently
. sunbelt's plb provides no means of getting the required info out of
. a comfile in order to implament it and where otherwise noted in the code
. Therefore, this ftp client will only connect to 100% RFC 959, and 765 
. compliant servers. (unfortunately this means you cannot connect to SOME
. microsoft WINDOWS ftp server implamentations...)
.



dwnl        form    1
upld        form    1
disconnect	form    "1"
transfer    file
seq         form    "-1"


ip          dim      15
port        dim      4
req         dim      30
dtoc        init     ".,"
ctod        init     ",."
TOUPPER     INIT     "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"
localip     init     "208.209.255.181"    ;change this to YOUR local ip (not 127.*)
localport   form     " 1025"              ;the data port we will listen on...
lport       dim      5
int1        integer  2

WCOUNT      FORM     2

c           comfile     ;control connection
d           comfile     ;data connection

p1          form     2
p2          form     2
D1          DIM      2
D2          DIM      2

data        dim      1024
resbuff     dim      1024
message     dim      1024
TMOUT       FORM     1

ftpresp     list
code        dim      1
misc        dim      1023
            listend

USRCMD      DIM      128
      
MSGSTAT     RECORD
ConnectF    FORM      1
SendF       Form      1
RecvF       Form      1
ErrorF      Form      1
ErrorC      Form      8
ErrorN      Form      8
            RECORDEND

CR          INTEGER   2,"0x0a0D"
NULL        init      0x00
junk        dim       1
username    dim       80
password    dim       80

waitres	init	"1?? "
okres	init	"2?? "
moreres init	"3?? "
errres	init	"4?? "
erres	init	"5?? "

hash	form	1

	clear 	hash
	CLEAR   WCOUNT
.      DISPLAY  "Enter VALID Local IP Addredd"
.      DISPLAY  "This address is the IP address of your INTERNET IP"
.      DISPLAY  "NOT your LAN IP"
.      KEYIN    "Local IP  : ",localip

      KEYIN    "Enter Servers IP Address:",ip
      MOVE     "21",PORT      ;standard ftp port

      PACK     req,"S,O,",ip,",",port,",R"  ; Socket open must be in raw mode
      
      TRAP     openfail IF IO
      COMOPEN  c,req
      TRAPCLR  ALL

      CALL     recvc     ;in ftp...there *should* always be a welcome as soon as you connect

      UNPACK   data,ftpresp ;increase the size of code to 3 to analize detail responces
      IF       (code = "2")
        CALL   login
      ELSE
        DISPLAY "Unable to get OK responce from server!"
      ENDIF    ;initial connection
      
end
      KEYIN "Program terminating",junk  
      STOP

openfail
      KEYIN "Open socket failed or lost connection",junk
      STOP
      
dataopenfail
      KEYIN     "cannot initialize data socket",junk
      COMCLOSE  c
      NORETURN
      GOTO      end

sendc
        LOOP
                COMSTAT C, Message
                UNPACK  message,msgstat
                BREAK   IF (msgstat.sendF = 1)
                CALL    openfail IF (msgstat.ConnectF = 0 )
                CALL    openfail IF (msgstat.errorF = 1)
        REPEAT
.        DISPLAY   data
        COMWRITE  C;*ll,DATA ;
       RETURN        

recvc
      MOVE      "0",TMOUT
        LOOP
                COMSTAT C, Message
                UNPACK  message,msgstat
                BREAK   IF (msgstat.RecvF = 1 )
                CALL    openfail IF (msgstat.errorF = 1 )
                CALL    openfail IF (msgstat.ConnectF = 0 )
                KEYIN   *T1,junk;  ;one second delay
                INCR    tmout
                BREAK   IF (zero)
        REPEAT
      COMREAD   C;data
      DISPLAY   data
      RETURN

loggedin
      CALL      PORTCMD   ;SETUP DATAPORT FOR NEXT COMMAND
      KEYIN     "FTP ## ",USRCMD
      CALL      translCMD
      GOTO      loggedin

recvd
      COMREAD   d;data
      RETURN

setupdata ; this routeen will have to be called for virtually every ftp command
      RETURN
      
PORTCMD
     KEYIN   *T1,junk;  ;one second delay
     LOOP
      COMSTAT   C,message
      UNPACK    message,msgstat
      CALL      recvc IF (msgstat.recvf=1)   ;BASICALLY THIS FLUSHES THE RECV BUFFER
      BREAK IF (MSGSTAT.RECVF=0)          ;BEFORE ISSUEING THE PORT CMD TO PREVENT
     REPEAT                               ;CONFUSION

     MOVE       localport,int1
     MOVE       (int1/256),p1 ;should be high byte
     MOVE       (int1-(p1*256)),p2  ;should be low byte
     MOVE       P1,D1
     MOVE       P2,D2
     LOOP
       CMATCH    D1," "
       BREAK IF NOT EQUAL
       IF EQUAL
        BUMP     D1
       ENDIF
     REPEAT
     LOOP
       CMATCH    D2," "
       BREAK IF NOT EQUAL
       IF EQUAL
        BUMP     D2
       ENDIF
     REPEAT

     REPLACE    dtoc,localip
     PACK       data,"PORT ",localip,",",d1,",",d2,cr     ;,lf
     REPLACE    ctod,localip
     CALL       sendc
     KEYIN      *T1,junk;  ;one second delay
     CALL       recvc
     UNPACK     data,ftpresp
     IF (code = "2")
        RETURN
     ELSE
        DISPLAY "PORT COMMAND FAILED"
        DISPLAY " THIS VERSION CANNOT USE THE DEFAUT DATAPORT "
        DISPLAY " SO THE PORT COMMAND MUST BE SUPPORTED ON THE SERVER "
        NORETURN
        GOTO END
     ENDIF

WAITRESPONCE
        CALL    recvc
	call	StripToBuffer
	bump	resbuff
BuferedResponce
        UNPACK  resbuff,ftpresp
        IF (code = "2") 
          RETURN
        ENDIF
        IF (CODE = "1" )
          IF (upld)
		CALL  PUTDATA
          ELSE
            CALL  GETDATA
          ENDIF
	  bump resbuff
          if (EOS)
            goto waitresponce
          endif
          GOTO BuferedResponce 
        ENDIF
        RETURN

StripToBuffer
	move 	" ",resbuff
	scan	waitres,data,"?"
        if (zero)
		unpack	data,ftpresp
		append	code,resbuff
	endif
.
	scan	Okres,data,"?"
	if (zero)
		unpack	data,ftpresp
		append	code,resbuff
	endif
	reset	resbuff
 return
        
GETDATA
. I believe the RFC's for ftp states that as long at there is a connection here
. you should be expection data as a closed connection on the dataport marks the 
. end of the current transfer
      COMSTAT   d,message
      UNPACK    message,msgstat
      IF        (msgstat.ConnectF=1)
        LOOP
           IF   (msgstat.recvf=1)    ;incomming data on dataport
              CALL   recvd
              IF     (dwnl = "1")
                WRITE   transfer,seq;*ll,*abson,data;
                if (hash)
                   IF (WCOUNT > "79")
                     DISPLAY " "
                     MOVE    "1",WCOUNT
                   ENDIF
                   display   *H=WCOUNT,"##";
                   INCR      WCOUNT
                endif
              ELSE
                DISPLAY data;
              endif
           ENDIF
           COMSTAT   d,message
           UNPACK    message,msgstat
           BREAK     IF (MSGSTAT.CONNECTF=0)
        REPEAT
        IF (dwnl = "1")
          CLOSE      transfer
          CLEAR      dwnl
        ENDIF
      ENDIF
 RETURN

PUTDATA
. I believe the RFC's for ftp states that as long at there is a connection here
. you should be expection data as a closed connection on the dataport marks the 
. end of the current transfer
      LOOP
         COMSTAT   d,message
         UNPACK    message,msgstat
         IF        (msgstat.ConnectF=1)
            IF        (msgstat.sendF=1)
               READ   transfer,seq;*abson,data;
               BREAK  if OVER
               comwrite d;*ll,data
               if (UPLD)
                  if (hash)
                      IF (WCOUNT > "79")
                        DISPLAY " "
                        MOVE    "1",WCOUNT
                      ENDIF
                      display   *H=WCOUNT,"##";
                      INCR      WCOUNT
                  endif
               endif
            endif
         ENDIF
      REPEAT
      comwrite d;*ll,data
      comclose d,disconnect
      close    transfer
      CLEAR    UPLD
 RETURN

SETUPDATAPORT
      PACK      REQ,"S,C,",LOCALIP,",",LOCALPORT,",R"
      TRAP      bumpdport IF io,noreset
      COMOPEN   D,REQ
      TRAPCLR   ALL
 RETURN

BUMPDPORT
      INCR      localport
      NORETURN
      GOTO      setupdataport

login
       KEYIN  "Username: ",MISC
       PACK   data,"USER ",MISC,cr   ;,lf
       CALL   sendc
       CALL   recvc
       UNPACK data,ftpresp
       IF (code = "3")
          KEYIN  "PASSWORD: ",*eson,MISC,*esoff
          PACK   data,"PASS ",MISC,cr     ;,lf,null
          CALL   sendc
          CALL   recvc
          UNPACK data,ftpresp
          IF (code = "2")
            CALL SETUPDATAPORT
werein      CALL LOGGEDIN
          ELSE   ;pass
            DISPLAY "invalid password"
            GOTO    login
          endif
       ELSE IF (code = "2")
          GOTO werein
       ELSE   ;user
           DISPLAY "ftp server Returned unexpected responce"
       endIF  ;user
 RETURN
 
translCMD
     MOVE      USRCMD,MESSAGE
     REPLACE   toupper,message
     SCAN      " ",MESSAGE
     IF ZERO      ;MULTI PART COMMAND
        LENSET    message
        RESET     message
        SCAN      " ",USRCMD
        BUMP      USRCMD
        SWITCH    MESSAGE
GET        CASE      "GET "
             PACK    DATA,"TYPE i",CR
             CALL    sendc
             KEYIN   *T1,junk;  ;one second delay to compensate for windows async behavior
             CALL    WAITRESPONCE
             PREP    transfer,USRCMD,EXCLUSIVE
             PACK    message,"RETR ",USRCMD
             set     dwnl
PUT        CASE      "PUT "
             PACK    DATA,"TYPE i",CR
             CALL    sendc
             KEYIN   *T1,junk;  ;one second delay to compensate for windows async behavior
             CALL    WAITRESPONCE
             set     upld
             PACK    message,"STOR ",USRCMD
             open    transfer,USRCMD,exclusive
CD         CASE      "CD "
             PACK    MESSAGE,"CWD ",USRCMD
DIR        CASE   "DIR "
             PACK    DATA,"TYPE a",CR
             CALL    sendc
             KEYIN   *T1,junk;  ;one second delay to compensate for windows async behavior
             CALL    WAITRESPONCE
             PACK    MESSAGE,"LIST ",USRCMD
LS         CASE   "LS "
             PACK    DATA,"TYPE a",CR
             CALL    sendc
             KEYIN   *T1,junk;  ;one second delay to compensate for windows async behavior
             CALL    WAITRESPONCE
             PACK    MESSAGE,"LIST ",USRCMD
TYPE       CASE   "TYPE "
             DISPLAY "Type command masked of, all file transfers are binary"
             RETURN
        ENDSWITCH
        move message,USRCMD
     ENDIF
     IF (MESSAGE = "DIR")
          PACK    DATA,"TYPE a",CR
          CALL    sendc
          KEYIN   *T1,junk;  ;one second delay to compensate for windows async behavior
          CALL    WAITRESPONCE
          MOVE    "LIST",USRCMD
     ENDIF
     IF (MESSAGE = "LS")
          PACK    DATA,"TYPE a",CR
          CALL    sendc
          KEYIN   *T1,junk;  ;one second delay to compensate for windows async behavior
          CALL    WAITRESPONCE
          MOVE    "LIST",USRCMD
     ENDIF
     IF (MESSAGE = "HASH")
HASH      SET HASH		; hash is a client side command...not a server command
          return
     ENDIF
      
     PACK      data,USRCMD,cr
     CALL      sendc
     KEYIN     *T1,junk;  ;one second delay to compensate for windows async behavior
     CALL      WAITRESPONCE
     CLEAR     DATA

 RETURN

.-------------------------------------------------------------------
.  NOW WE HAVE A HALF FUNCTIONAL FTP INTERACTIVE CLIENT
.  LETS PUT A LOADMOD WRAPPER AROUND IT :O)
.-------------------------------------------------------------------
FTPNAME    DIM   ^
FTPPASS    DIM   ^
FTPSERVER  DIM   ^
FTPFILE    DIM   ^
FILE       DIM   260
PATH       DIM   260
pathed     form  1
get        form  1
put        form  1

FTPGET ROUTINE FTPSERVER,FTPFILE,FTPNAME,FTPPASS
      SET      get
      CLEAR    put
      PACK     req,"S,O,",FTPSERVER,",21,R"  ; Socket open must be in raw mode
      TRAP     openfail IF IO
      COMOPEN  c,req
      TRAPCLR  ALL
      CALL     recvc     ;in ftp...there *should* always be a welcome as soon as you connect
      UNPACK   data,ftpresp ;increase the size of code to 3 to GET detail responces
      IF       (code = "2")
        CALL   FTPlogin
      ELSE
        DISPLAY "Unable to OK responce from server!"
      ENDIF    ;initial connection
  RETURN

FTPPUT ROUTINE FTPSERVER,FTPFILE,FTPNAME,FTPPASS
      SET      put
      CLEAR    get
      PACK     req,"S,O,",FTPSERVER,",21,R"  ; Socket open must be in raw mode
      TRAP     openfail IF IO
      COMOPEN  c,req
      TRAPCLR  ALL
      CALL     recvc     ;in ftp...there *should* always be a welcome as soon as you connect
      UNPACK   data,ftpresp ;increase the size of code to 3 to GET detail responces
      IF       (code = "2")
        CALL   FTPlogin
      ELSE
        DISPLAY "Unable to OK responce from server!"
      ENDIF    ;initial connection
  RETURN



FTPlogin
       PACK   data,"USER ",FTPNAME,cr   ;,lf
       CALL   sendc
       CALL   recvc
       UNPACK data,ftpresp
       IF (code = "3")
          PACK   data,"PASS ",FTPPASS,cr     ;,lf,null
          CALL   sendc
          CALL   recvc
          UNPACK data,ftpresp
          IF (code = "2")
            CALL SETUPDATAPORT
Fwerein     CALL FTPIN
          ELSE   ;pass
            DISPLAY "invalid password"
            GOTO    login
          endif
       ELSE IF (code = "2")
          GOTO Fwerein
       ELSE   ;user
           DISPLAY "ftp server Returned unexpected responce"
       endIF  ;user
 RETURN

FTPIN
      CALL        PORTCMD   ;SETUP DATAPORT FOR NEXT COMMAND
      ENDSET      FTPFILE
      LOOP
            BUMP   FTPFILE,SEQ
            BREAK  IF EOS
            CMATCH FTPFILE,"/"
            IF ZERO
              set pathed
              BREAK
            endif
      REPEAT
      IF (pathed)
        BUMP        FTPFILE
        MOVE        FTPFILE,FILE
        BUMP        FTPFILE,SEQ
        LENSET      FTPFILE
        RESET       FTPFILE
        MOVE        FTPFILE,PATH
        PACK        USRCMD,"CD ",PATH
        CALL        translCMD
        CALL        PORTCMD   ;SETUP DATAPORT FOR NEXT COMMAND
      ELSE
        MOVE        FTPFILE,FILE
      ENDIF
      IF (get)
         PACK       USRCMD,"GET ",FILE
      ELSE
         PACK       USRCMD,"PUT ",FILE
      ENDIF
      CALL        TRANSLCMD
      PACK        USRCMD,"QUIT"
      CALL        TRANSLCMD
 RETURN
