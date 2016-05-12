*........................................................................
. Example Program: ANSWER.PLS
.
.  This	program	is a simple answer program.  Typically,	ANSWER is the first
.  program executed in a PLB system.  It's purpose is to initalize any common
.  variables and CHAIN to the Master Program.  Most ANSWER programs also
.  provide security by requiring the user to log-in with a user	name and
.  a password.	Only after correctly entering the information will the 
.  program CHAIN to Master.  This simple ANSWER	program	does not provide
.  security.
.
. Copyright @ 1997-1999	Sunbelt	Computer Systems, Inc.
. All Rights Reserved.
.............................................................................
.
..........................................................................
.
PC       EQU       1
. COMMON AREA.   THIS AREA GETS OVERWRITTEN WITH AN ELEVEN-BYTE CHARACTER
.                STRING VARIABLE WHEN AN ERROR OCCURS.
.                THE COMMON VARIABLE 'ERROR' USES THE SAME NUMBER OF BYTES
.                OF UDA AS THE COMMON VARIABLES 'PORTN' AND 'TODAY'
.
ERROR    DIM       35
TODAY    DIM       8
SECURITY FORM      1
FUNC     DIM       2
TYPINIT  DIM       2
PORTN    FORM      3     *NEEDS TO BE THIS.
AGENDAID DIM       6     *NEED THIS.
.............................................................................
.
JULIAN  FORM      5                  TODAY IN yyjjj FORMAT
.
USER    DIM       10                 USER ID
USERNME DIM       10                 USER NAME (FIRST INIT, LAST)
PRIO    FORM      3                  OVERALL PRIORITY LEVEL
LEVELS  DIM       36                 SECURITY LEVELS
COMM    DIM       1                  COMMUNICATION ALLOWED
;
COMPANY FORM      1                  COMPANY CODE (1-NIN,2-CMP,3-DECK,4-DATA)
COMPNME DIM       24                 COMPANY NAME TEXT
MULTCOS FORM      1                  MULTIPLE COMPANIES ALLOWED
CURSYS  FORM      1                  CURRENT SYSTEM
CURLEVL FORM      1                  CURRENT LEVEL WITHIN SYSTEM
MULTSYS FORM      1                  MULTIPLE SYSTEMS ALLOWED
PROGRAM DIM       8                  LAST PROGRAM NAME
COMMENT DIM       30
INITS    DIM       3                 USER INITIALS
EXIT     FORM      2                 EXIT ALLOWED
INPNAME  DIM       25                INPUT FILE NAME
OUTNAME  DIM       25                 OUTFILE NAME
PRTNAME  DIM       25                 PRINT FILE NAME (/PRT ASSUMED)
TIMEOUT  FORM      2                  USED TO DROP BACK close IF INACTIVE
;
PORT	  DIM	    3
;PORT	  DIM	    2
;TODAY	  DIM	    8
TIME	  DIM	    8
.
               Include        Npasdd.inc
               include        cons.inc

DIM1	  DIM	    1
AST	  INIT	    "*"
 
PICT2	  PICT
PICT1	  Pict
.
SYSINFO	  DIM	    600
SCRWIDE	  FORM	    4
SCRHIGH	  FORM	    4
HIGHF	  FORM	    4.2
WIDEF	  FORM	    4.2
 
W1	  WINDOW
ST1	  STATTEXT
NOTE	  DIM	    80
;RESULT	  FORM	    9
BUTton1	  BUTTON
BUTton2	  BUTTON
ST2	  STATTEXT
ST3	  STATTEXT
ST4	  STATTEXT
mFile          menu
FData          init    "&File;E&xit"
pss              plform       Passwrd
mss1    plform  Error
X                plform       AnswerWEb001
Release        Init           "NOt"
Timer   Timer
*.........................................................................
.
.Do	Allow the Escape Key to	Terminate This Program
.
                 trap    GetPAss if f10
                TRAP	    NOEXIT IF INTERRUPT
*
;               if             (Program = "Master1" or PRogram = "MASTER")
               if             (program <> "" or program <> " ")
               goto           NOEXIT
               endif
.Initialize Common Variables
.
	  CLOCK	    PORT,PORT
	  REPLACE          " 0" IN PORT
	  CLOCK	    DATE,TODAY
	  CLOCK	    TIME,TIME
*
.
	  
	  WINHIDE
	  SETMODE   *SCREEN=OFF
                 FOrmload     x
               formload    pss
               formload    Mss1
               move    "v",progcode
               move    "N",PassFlag
               create  AnswerWeb001;mFile,FData
               activate mFile,ByeBye,result
               move               c8 to security

*
. Create Picture
.
                    eventreg x,10,AnswerKeyPress,RESULT=N9
	  GETINFO   SYSTEM,SYSINFO
                   If         Over 
	    MOVE    "600",SCRWIDE
	    MOVE    "480",SCRHIGH
	    ALERT   NOTE,"OVER ON SYSINFO",RESULT
;	    DEACTIVATE W1
	  ELSE
 
	    RESET   SYSINFO,13
	    SETLPTR SYSINFO,16
	    MOVE    SYSINFO,SCRWIDE
	    RESET   SYSINFO,17
	    SETLPTR SYSINFO,20
	    MOVE    SYSINFO,SCRHIGH
	    PACK    NOTE WITH "WIDE=",SCRWIDE,"	HIGH=",SCRHIGH
	    DIVIDE  "600" INTO SCRWIDE GIVING WIDEF
	    DIV	    "480",SCRHIGH,HIGHF
	  ENDIF
                 CREATE	AnswerWeb001;PICT1=100:500:1:400:
                             "\\nts0\c\netutils\NIN.jpg",BORDER
                 Activate     Pict1
                 CREATE	AnswerWeb001;PICT2=300:800:200:400:
                             "\\nts0\c\netutils\DMA.jpg",BORDER
                 Activate     Pict2
;	  CREATE    W1=0:(SCRHIGH-20):0:SCRWIDE,TITLE="ANSWER PROGRAM"
;                 eventreg w1,10,AnswerKeyPress,RESULT=N9
 
;	  SETMODE   *PIXEL=ON
;	  CREATE    W1;PICTA=1:(SCRHIGH-20):1:SCRWIDE,PICT1
;	  CREATE    W1;PICTA=1:(SCRHIGH-20):1:SCRWIDE,"\\nts0\c\netutils\ninca.gif"
;                 create    LOGO=04:18:12:85,"\\nts0\c\netutils\ninca.gif",border
;	  ACTIVATE  PICTA
;	  SETMODE   *PIXEL=OFF
 
;	  CREATE    W1;ST1=(400*HIGHF):(400*HIGHF+20):10:300,"":
;		    "FIXED(10,BOLD)"
;	  ACTIVATE  ST1
;	  SETITEM   ST1,0,NOTE
*
. Standard ANSWER program display
 
;	  CREATE    W1;ST2=(170*HIGHF):(170*HIGHF+40):240:450:
;		    "ANSWER Program","HELVETICA(12,BOLD)"
;	  CREATE    W1;ST3=(220*HIGHF):(220*HIGHF+20):175:450,"":
;	  CREATE    W1;ST3=(220*HIGHF):(220*HIGHF+20):175:450,"":
;			  "HELVETICA(10,BOLD)"
; 
;	  CREATE    W1;ST4=(250*HIGHF):(250*HIGHF+20):245:450,"":
;			  "HELVETICA(10,BOLD)"
 
;	  ACTIVATE  ST2
;	  ACTIVATE  ST3
;	  ACTIVATE  ST4
 
	  SETITEM   DateText,0,Today
                 Setitem   TimeText,0,Time
	  SETITEM   PortText,0,port
 
*
. Button to exit to MASTER program
 
;	  CREATE    W1;Button1=430:480:460:555,"-> MENU <-"
;	  CREATE    W1;Button2=530:580:460:555,"-> EXIT <-"
;	  ACTIVATE  Button1,EXIT,RESULT	 
;	  ACTIVATE  Button2,BYeBYE,RESULT	 
;	  ACTIVATE  W1
 
        CREATE  TIMER,30     ..3 seconds
        ACTIVATE TIMER,Exit,RESULT
	  LOOP
	    WAITEVENT
	  REPEAT
*
.Execute the MASTER Program
.
EXIT
               If             (Passflag = "Y")
               move               c0 to security
               CHAIN	    "MASTER1"
               else
               move               c8 to security
               CHAIN	    "MASTER1"
               endif
.
*
.Interrupt sequence
.
NOEXIT         
ByeBye         Shutdown
               Stop
             
               RETURN
.
AnswerKeypress
          if (N9 = 121)	.F10 Key calls password routine
          call Getpass
          endif
          return
GetPAss          trapclr F10
                 trap    GetPAss if f10
                 setitem PasswordEdit,0,""
                 setfocus PasswordEdit
                 setprop Passwrd,visible=1
                 return
                 Include      comlogic.inc
                 include      npasio.inc

...............................................................................
