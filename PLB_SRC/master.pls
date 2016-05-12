...............................................................................
.
.              NAMES IN THE NEWS          MASTER MENU PROGRAM
.
...............................................................................
PC       EQU       0
.
. COMMON AREA.   THIS AREA GETS OVERWRITTEN WITH AN ELEVEN-BYTE CHARACTER
.                STRING VARIABLE WHEN AN ERROR OCCURS.
.                THE COMMON VARIABLE 'ERROR' USES THE SAME NUMBER OF BYTES
.                OF UDA AS THE COMMON VARIABLES 'PORTN' AND 'TODAY'
.
.note the *blinkxx display & keyin statements do  not work under windows (9/28/98)
ERROR    DIM       *35
TODAY    DIM       *8
SECURITY FORM      *1
FUNC     DIM       *2
TYPINIT  DIM       *2
.PORTN    FORM      *2
PORTN    FORM      *3     *NEEDS TO BE THIS.
AGENDAID DIM       *6     *NEED THIS.
.............................................................................
.
.26JUL2002  PROGRAM 3 now news fulfillment mod program.
..31Oct2000 DLH double check for win95 & win98 network files
..10Jan00  DLH new findow.inc for 2000
.BEGINNING OF NEW COMMON VARIABLES 23APR92. DLH
.
JULIAN  FORM      *5                  TODAY IN yyjjj FORMAT
.
USER    DIM       *10                 USER ID
USERNME DIM       *10                 USER NAME (FIRST INIT, LAST)
PRIO    FORM      *3                  OVERALL PRIORITY LEVEL
LEVELS  DIM       *36                 SECURITY LEVELS
COMM    DIM       *1                  COMMUNICATION ALLOWED
.
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
.                                     FOR 30 MINUTES.
.
....................
.porttest     dim             %128
....................
..............................................................................
Vnetsup  dim       1
VRedir   dim       1
.............................................................................
.VARIABLES USED AS COMMON IN DATAMOD.... DO NOT CHANGE.
.                            POSITION OR VALUES.
TOTLIN   FORM      2
MODE     DIM       1
CONT     DIM       1
WORKNAME DIM       9
CORP     DIM       1
HOLDTE   DIM       8
HOLDPASS DIM       10
.............................................................................
.
.
Release   Init      "2.98"    DLH Add calc.exe
Reldate   Init      "11 November 2010"
.Release   Init      "2.97"    DLH Add Escrow maint
.Reldate   Init      "11 June 2009"
.Release   Init      "2.96"    DLH 13March2007  add Pacific Lists
.Reldate   Init      "13 March 2007"
.Release  Init      "2.95"    DLH 16Jan2007  add client server checking
.Reldate  Init      "16January2007"
.release    init      "2.94"           ASH 09DEC2004 Added NSMP0001
.Reldate       Init            "01/25/2006"
.release    init      "2.93"           ASH 07DEC2004 Added Mailer/Mailer Program
.release    init      "2.92"           DLH 13Sep2004 add New logo, get rid of words California & Databus
.release    init      "2.91"           DLH 22March2001 get rid of FINDOW
.release    init      "2.9"           ASH 14SEP00 ADDED SUPPRESSION REPORT
.release    init      "2.8"           JD  31May00 replaced old fixinv prog ninv0003 with ninv0022.
.release    init      "2.7"          DLH 10Feb00 replace naugthy trap on INTerurpt with goout
.release   init      "2.61"           JD 14JUL99 replaced nord0001,nordtest(time savior)
.release   init      "2.6"           DLH 22Feb99 Add smtp mail calls
.                                   remove old email template and netware
.                                   send
.Release   init      "2.5"           DLH 22Oct98 use new directory prog
.                                   Nrol.....
.Release   init      "2.4"           DLH 25Sep98 use new comlogic Errmess subroutine
.release  init       "2.3"          DLH 6Jul98 Delete gif and signin sound except for DH
.                                  see section LOGO
.Release  init       "2.2"          DLH 19May98 Misc cleanup display gif add sound files.
.Release  INIT      "2.1"          DLH 06JAN93 ADDED INACTIVE TIMER.
.RELEASE  INIT      "R002"
         INCLUDE   NUSEDD.inc
         INCLUDE   LOGDATA.inc
         INCLUDE   CONS.inc
.
.begin patch 2.5
PifFile   IFILE         keylen=1,fixed=12     .user preference file  --- 12/19/2001 DLH
DIRECT   FILE
.DNAME    DIM       25
.DNUM     DIM       4
.end patch 2.5
PROGNAME DIM       17
CWK11    INIT      "           "
ANS      DIM       1
INDEX    FORM      2
PASS     DIM       4
PASSMAST DIM       4
TIME     INIT      "HH:MM:SS"
HOUR     FORM      2
PER      DIM       3
TIMEDISP DIM       8
DAY      DIM       9
.MO       DIM       2
.DY       DIM       2
.YR       DIM       2
.STR2     DIM       2
LOGTIME  DIM       18
.MM       DIM       2
.DD       DIM       2
.YY       DIM       2
.NMM      FORM      2          FOR MESSAGE BRANCH
.NDD      FORM      2          FOR MESSAGE BRANCH
DASH80   INIT     "========================================":
                  "========================================"
STAR80   INIT     "****************************************":
                  "****************************************"
COL24    INIT      "                        "
HR       DIM       2
MIN      DIM       2
SEC      DIM       2
DOW      FORM      1
LINE     INIT      007           UNDER PCBUS = SMALL DIAMOND
BAR      INIT      006             "     "   = SPADE
CTL      INIT      002             "     "   = INVERSE FACE
CTR      INIT      003             "     "   = HEART
CBL      INIT      004             "     "   = lARGE DIAMOND
CBR      INIT      005             "     "   = CLUB
.COLON    INIT      ":"
ONSW     DIM       1
NAME     DIM       25
PORTNUM  DIM       3
ID       DIM       1
PORTX    FORM      3
.DAY OF WEEK VAR'S
ONE      FORM      "1"
TWO      FORM      "2"
FOUR     FORM      "4"
SEVEN    FORM      "7"
EIGHTY   FORM      "80"
nineteenEIGHTY   FORM      "1980"
DIM2     DIM       2
DIM3     DIM       3
NWORK1   FORM      5
NWORK2   FORM      5
YEARWORK FORM      4
YEAR     FORM      4
JDAYWORK FORM      3
AGENDAKY DIM       18
DIM6     DIM       6
FILL4    DIM       4
AGENDAM  IFILE     KEYLEN=18,VAR=387
AGENDASW FORM      "0"           1=FILE OPEN 0=FILE CLOSED
Logo     pict
SFILE    sndfile
SoundFLag form   1
Sapi          Automation                               //Define the ActiveX control
Str500        Dim             500
HelloFlag     Dim             1
FName         dim             30
PLAYSOUND PROFILE winmm, PlaySound,INT4,DIM,INT4,INT4
SND_FILENAME INTEGER 4,"0x00020000"
SND_ASYNC INTEGER 4,"0x0001"
SND_NODEFAULT INTEGER 4,"0x0002"
NULL INIT 0X00
I4A INTEGER 4
I4B INTEGER 4
.begin patch 4.2
CompanyFlag         Dim       1
.end patch 4.2
.**.............................................................................
main
.           EVENTREG  X, 17, XRESIZE

         CLOCK     DATE TO TODAY
         clock     timestamp to timestamp
         unpack    timestamp into cc           .get century

          Call        testclient
          
         CALL      PAINT
         call      logo
.
         Create    Sapi,class="Sapi.SpVoice"         //Create the ActiveX control
         move      c0 to timeout
         keyin     *cl,*t0,str1
         MOVE       C1 TO NUSEPATH    *ACCESS BY PORTNUMBER
         UNPACK    TODAY INTO MM,STR1,DD,STR1,YY
.DLH 10Feb00   .no reason to do anymore
.         trap      naughty if int
         TRAP      GOOUT IF INT
              call            Openpif

FINDIT   MOVE      MM TO NMM
         MOVE      DD TO NDD
.         if        (portn <= 0)
         if        (portn > 0)
              move            no to helloflag                    ;been there done that
.         call      hello
         endif
         CLEAR     FUNC
         CLEAR     TYPINIT
         CALL      FINDPORT
               call           Trim using NUSEUSER
               if (NUSEUSER <> "" & HelloFlag <> "N" & Soundflag = 1)
                              scan           B1,NUSEUSER
                              if equal
                                             movefptr NUSEUSER,N9
                                             setlptr NUSEUSER,N9
                                             reset  Nuseuser
                    CLEAR I4A
                    MOVE SND_FILENAME,I4B
                    ADD SND_ASYNC,I4B
                    ADD SND_NODEFAULT,I4B
                    pack      Taskname,"\\nins1\e\Netutils\Media\tada.wav",null
                    WINAPI PLAYSOUND USING Taskname,I4A,I4B 
                                             
                              Move           Nuseuser to FName                                             
                              pack           str500 from "Hello ",Fname," It's good to see you"
//Use the Speak Method of this control
                    if        (soundflag = 1 & ClntServFlag = c0)
                              Sapi.Speak using str500 
                        endif
                              Move           No to Helloflag                
                              endif
                              endif
                              reset          NUSEUSER
.         CALL      HELLO
.turned off no more 95 machines DLH 17May2001
.         call      win95
         GOTO      TRAPCHK
.NOFIND
.
...............................................................................
.
. SEE IF THERE ARE ANY UNTRAPPED DATASHARE ERRORS.
. IF NO ERROR OCCURED, THE VARIABLES 'PORTN' AND 'TODAY' WILL STILL BE IN
. THE FIRST TWELVE BYTES OF UDA.  IN THIS CASE, THE 9TH CHARACTER OF CWK11
. WILL BE A BLANK.
.
. IF AN ERROR OCCURED, THE ELEVEN BYTE ERROR MESSAGE WILL BE MOVED INTO CWK11
. IN THIS CASE THE 9TH CHARACTER OF CWK11 WILL BE A '*'.
.
TRAPCHK  CALL      DAY                  *GET DAY OF WEEK IF SAT OR SUN
.                                       DO NOT OPEN AGENDA FILE(s)
.         MATCH     "tc" TO  TYPINIT
.         GOTO      TCPORT IF EQUAL
         TRAP      IO GIVING ERROR IF IO
         CALL      DAY
         IFNZ      PC                    .datapoint agenda
         CALL      OPENAGEN
         XIF                                  
         TRAP      XMAS IF F8
         trap      gamemenu if F6
         trap      logo1 IF F10
         INCLUDE   LOGIO.inc
         TRAP      IO GIVING ERROR IF IO
         TRAP      GOOUT IF F3
         MOVE      ERROR TO CWK11
.         RESET     CWK11,9            .dlh 22jul93
         scan       "*",CWK11
         GOTO      MASMENU IF NOT EQUAL
         GOTO      FREEZE 
XMAS     TRAPCLR   F8
         CHAIN     "XMASTREE"
IO       TRAPCLR   IO
         NORETURN
         TRAP      IO GIVING ERROR IF IO
         trap      naughty if f3
         trap      naughty if f8
         trap      naughty if f6
.         trap      naughty if STACK       .pcbus
         trap      naughty if inS
         trap      naughty if DEL
         trap      naughty if RANGE
         trap      naughty if PARITY
         trap      naughty if FORMAT
.         trap      naughty if PRTOFL        .pcbus
         trap      naughty if CFAIL
.DLH 10Feb00
.         trap      naughty if int
         TRAP      GOOUT IF INT
         GOTO      FREEZE
.
. THIS FREEZES THE ERROR ON THE SCEEEN SO THAT A BETTER DETERMINATION CAN
. BE MADE CONCERNING THE CAUSE OF THE PROBLEM.
.
FREEZE   
         trap      naughty if f3
         trap      naughty if f8
         trap      naughty if f6
.         trap      naughty if STACK       .pcbus
         trap      naughty if inS
         trap      naughty if DEL
         trap      naughty if RANGE
         trap      naughty if PARITY
         trap      naughty if FORMAT
.         trap      naughty if PRTOFL       .pcbus
         trap      naughty if CFAIL
.DLH 10Feb00
.         trap      naughty if int
         TRAP      GOOUT IF INT


.      Move    "This is a Error e-mail from master",SmtpSubject Subject
      Move    "This is a Error e-mail from master",MailSubjct

.   Set the text message that is send with the attachments
          Clear     MailBody
          append    "This is an error message",MailBody
          Append    CRLF,MailBody
          Append    "This is from subroutine FREEZE",MailBody         
          Append    CRLF,MailBody
          Append    "Incorrect termination NIN Master program",MailBody         
          Append    CRLF,MailBody
          reset     MailBody
          pack      MailFrom from User,"@nincal.com"
          move      "InformationServices@nincal.com",MailTO
          call      SendMail
         DISPLAY   *B,*ES,*blinkon,*red:
                   *P15:10,"00000  00000  00000  00000  00000":
                   *P15:11,"0      0   0  0   0  0   0  0   0":
                   *P15:12,"0000   00000  00000  0   0  00000":
                   *P15:13,"0      0  0   0  0   0   0  0  0 ":
                   *P15:14,"00000  0   0  0   0  00000  0   0" 
         KEYIN     *P80:24,ANS;
         MOVE      "ERROR",LOGTYPE
         MOVE      ERROR TO LOGINFO
         CMATCH    "Q",ANS
         GOTO      FREEZE IF EOS
         GOTO      FREEZE IF NOT EQUAL
         DISPLAY   *B,*ES,*blinkoff,*white:
                   *P15:10,"00000  00000  00000  00000  00000":
                   *P15:11,"0      0   0  0   0  0   0  0   0":
                   *P15:12,"0000   00000  00000  0   0  00000":
                   *P15:13,"0      0  0   0  0   0   0  0  0 ":
                   *P15:14,"00000  0   0  0   0  00000  0   0" 
ERROR    KEYIN     *P15:16,"ERROR MESSAGE: ",*DV,CWK11:
                   *P15:17,*EL,*DV,ERROR,*P47:16,*eoff,ANS,*eon

         CALL      LOGWRITE
         CMATCH    "e",ANS
         GOTO      ERROR IF EOS
         GOTO      ERROR IF NOT EQUAL
         move      b1 to error
         trapCLR  INS   
         TRAPCLR   F6
         TRAPCLR   F3
         TRAPCLR   F8
.DLH 10Feb00
.         trap      naughty if int
         TRAP      GOOUT IF INT
         trap      gamemenu if f6
         TRAP      GOOUT IF F3
         TRAP      XMAS IF F8
         move      b1,CWK11
         move      b1 to onsw                   .reset so we get full menu display not just time. 07May98 DLH
         GOTO      MASMENU
naughty 
         trapCLR  F3   
         trapCLR  F8   
         trapCLR  F6   
.         trapCLR  STACK   
         trapCLR  INS   
         trapCLR  DEL   
         trapCLR  RANGE   
         trapCLR  PARITY   
         trapCLR  FORMAT   
.         trapCLR  PRTOFL   
         trapCLR  CFAIL   
         trapCLR  INT   
         trap      naughty if f3
         trap      naughty if f8
         trap      naughty if f6
.         trap      naughty if STACK
         trap      naughty if inS
         trap      naughty if DEL
         trap      naughty if RANGE
         trap      naughty if PARITY
         trap      naughty if FORMAT
.         trap      naughty if PRTOFL
         trap      naughty if CFAIL
.DLH 10Feb00
.         trap      naughty if int
         TRAP      GOOUT IF INT
         move      "Naughty" to error
      Move    "This is a Error e-mail from master",MailSubjct

.   Set the text message that is send with the attachments
          Clear     MailBody
          append    "This is an error message",MailBody
          Append    CRLF,MailBody
          Append    "This is from subroutine Naughty",MailBody        
          Append    CRLF,MailBody
          Append    "Naughty!!!!",MailBody        
          Append    CRLF,MailBody
          Append    "Incorrect termination NIN Master program",MailBody         
          Append    CRLF,MailBody
          reset     MailBody
          pack      MailFrom from User,"@nincal.com"
          move      "InformationServices@nincal.com",MailTO
          call      SendMail

         DISPLAY   *B,*ES,*blinkon:
                   *P15:10,"00  0    000   0   0  00000  0   0  000000 0    0 ":
                   *P15:11,"0 0 0   0   0  0   0  0      0   0    00    0  0":
                   *P15:12,"0 0 0   00000  0   0  0  00  00000    00     00":   
                   *P15:13,"0  00   0   0  0   0  0   0  0   0    00     00 ":
                   *P15:14,"0   0   0   0  00000  00000  0   0    00     00    ": 
                   *b,*b,*b,*b,*b,*b,*b,*b,*r:
                   *r:
                   *r:
                   *r:
                   *r:
                   *r,*b,*b,*b,*b,*b,*b,*b,*b:
                   *P15:10,"00  0    000   0   0  00000  0   0  000000 0    0 ":
                   *P15:11,"0 0 0   0   0  0   0  0      0   0    00    0  0":
                   *P15:12,"0 0 0   00000  0   0  0  00  00000    00     00":   
                   *P15:13,"0  00   0   0  0   0  0   0  0   0    00     00 ":
                   *P15:14,"0   0   0   0  00000  00000  0   0    00     00    ":
                   *blinkoff 
              if              (soundflag = 1)
          CLEAR I4A
          MOVE SND_FILENAME,I4B
          ADD SND_ASYNC,I4B
          ADD SND_NODEFAULT,I4B
          pack      Taskname,"\\nins1\e\Netutils\media\thunder.WAV",null
          WINAPI PLAYSOUND USING Taskname,I4A,I4B 
        endif
.          execute  "\\nins1\e\public\send #"I was Naughty#" to info_services >>nul"
          Clock     Version,str35
          scan      "cliwin",str35
          if        Not equal           .not running PLbclient
                    execute   "c:\progra~1\plus!\micros~1\iexplore.exe http://WEb01/"
                              if         over                .Failed
                              execute   "c:\progra~1\Intern~1\iexplore.exe http://Web01/"          
                              endif
          else
.          alert     Note,"Not available logged in this way - Sorry",result
                    execute   "!c:\progra~1\plus!\micros~1\iexplore.exe http://WEb01/"
                              if         over                .Failed
                              execute   "!c:\progra~1\Intern~1\iexplore.exe http://Web01/"          
                              endif
          endif 
              if              (soundflag = 1)
          CLEAR I4A
          MOVE SND_FILENAME,I4B
          ADD SND_ASYNC,I4B
          ADD SND_NODEFAULT,I4B
          pack      Taskname,"\\nins1\e\Netutils\media\thunder.WAV",null
          WINAPI PLAYSOUND USING Taskname,I4A,I4B 
         endif
          goto     freeze
.
*
.MASMENU  CLOCK     PORT TO CWK2
.         CONSOLE   "MASTER",CWK2,"                 "
MASMENU
.
         ADD       C1 TO TIMEOUT
         COMPARE   "15" TO TIMEOUT
         goto      goout IF EQUAL
         TRAP      DIRECT IF F4
         TRAP      Intranet IF F6
         TRAP      NINWEb IF F7
         CLOCK     DATE TO TODAY

         IFNZ      PC
         UNPACK    TODAY INTO MM,DD,YY
         XIF

         IFZ       PC
         UNPACK    TODAY INTO MM,STR1,DD,STR1,YY
         XIF

         MOVE      MM TO NMM
         MOVE      DD TO NDD
         PACK      PASSMAST FROM DD,MM
         REP       ZFILL IN PASSMAST
. 
         CLOCK     TIME TO TIME
         UNPACK    TIME INTO STR2,ANS,MIN,ANS,SEC
         MOVE      TIME,STR2
.
         MOVE      STR2,HOUR
.         MOVE      HR TO HOUR
         COMPARE   "12",HOUR
         GOTO      PM2 IF EQUAL
         GOTO      PM IF NOT LESS
         MOVE      " AM",PER
         GOTO      CNVTIME
PM       SUB       "12",HOUR
PM2      MOVE      " PM",PER

CNVTIME  CLEAR     TIMEDISP
         PACK      TIMEDISP FROM HOUR,COLON,MIN,COLON,SEC,PER
         BUMP      TIMEDISP BY 1
         REP       ZFILL IN TIMEDISP
         RESET     TIMEDISP

APPDAY   CLEAR     TODAY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         REP       ZFILL IN TODAY
.         GOTO      DAY
         CALL      DAY
         GOTO      MENU2
.
CLOCKERR NORETURN
         GOTO      APPDAY
.
DAY      CLOCK     YEAR TO DIM2
.dave's patch?
         pack      str4 from cc,dim2
.         MOVE      dim2 TO YEARWORK
.         MOVE      dim2 TO YEAR

         MOVE      str4 TO YEARWORK
         MOVE      str4 TO YEAR
.end dave's patch?
         CLOCK     DAY TO DIM3
         MOVE      DIM3 TO JDAYWORK
.go find day of week.
.begin patch 2.91
.         CALL      FINDOW
.         MOVE      NWORK2 TO DOW
         clock      weekday to str2
         move       str2 to DOW
.end patch 2.91
         BRANCH    DOW OF SUN,MON,TUE,WED,THU,FRI,SAT
         COMPARE   "0",DOW
         GOTO      CNVTIME IF NOT EQUAL
         MOVE      "??????" TO DAY
.         MOVE      "Saturday ",DAY
.         GOTO      MENU2
DAYEXIT  RETURN
SUN      MOVE      "Sunday   ",DAY
         GOTO      DAYEXIT
MON      MOVE      "Monday   ",DAY
         GOTO      DAYEXIT
TUE      MOVE      "Tuesday  ",DAY
         GOTO      DAYEXIT
WED      MOVE      "Wednesday",DAY
         GOTO      DAYEXIT
THU      MOVE      "Thursday ",DAY
         GOTO      DAYEXIT
FRI      MOVE      "Friday   ",DAY
         GOTO      DAYEXIT
SAT      MOVE      "Saturday ",DAY
         GOTO      DAYEXIT
.
MENU2
.         IFNZ      PC
.         BRANCH    DOW OF FILESTAT,READOK,READOK,READQUES,READTHUR,READQUES:
.                   FILESTAT
.         XIF
         GOTO      NOAGENDA
READQUES
         CLOCK     TIME TO TIME
         UNPACK    TIME INTO STR2,ANS,MIN,ANS,SEC
         MOVE      STR2,HOUR
         COMPARE   "20" TO HOUR
         GOTO      READOK IF LESS
         GOTO      FILESTAT
READTHUR
         CLOCK     TIME TO TIME
         UNPACK    TIME INTO STR2,ANS,MIN,ANS,SEC
         MOVE      STR2,HOUR
         COMPARE   "8" TO HOUR
         GOTO      READOK IF NOT LESS
FILESTAT BRANCH    AGENDASW OF CLOSEIT
         GOTO      NOAGENDA
CLOSEIT  CLOSE     AGENDAM
         MOVE      "0" TO AGENDASW
         GOTO      NOAGENDA
.READOK   MOVE      "    " TO FILL4
.         PACK      DIM6 FROM FILL4,FUNC
.         PACK      AGENDAKY FROM ONE,DIM6
READOK   PACK      AGENDAKY FROM ONE,AGENDAID
.*****03AUG88 ADDED CHECK FOR EOS AND SKIP READ IF SET.
.         CMATCH    B1 TO FUNC
         CMATCH   B1 TO AGENDAID
         GOTO      MENU2B IF EOS
.         OPEN      AGENDAM,"AGENDAM",SHARE
.         OPEN      AGENDAM,"AGENDAM/MFD",SHARE
.         DISPLAY   *P1:24,AGENDAKY,*W;
         FILEPI    2;AGENDAM
         READ      AGENDAM,AGENDAKY;AGENDAKY
         READKS    AGENDAM;AGENDAKY
         GOTO      MENU2B IF OVER
.         SETLPTR   AGENDAKY TO 7
.         RESET     AGENDAKY TO 2
.         DISPLAY   *P1:24,AGENDAKY,B1,DIM6,*W;
.         MATCH     AGENDAKY TO DIM6
         BUMP      AGENDAKY BY 1
         MATCH     AGENDAKY TO AGENDAID
         GOTO      MENU2B IF NOT EQUAL
         DISPLAY   *P1:24,*HON,"You have messages",*B,*W:
                   *P1:24,*HOFF,"                 ";
         GOTO      MENU2B
.MENU2A   CLOSE     AGENDAM
         IFNZ      PC
NOAGENDA DISPLAY   *P1:24,*EL,*HON,*B,"NO MESSAGE SERVICES ON-LINE",*W4,*HOFF;
         XIF
         IFZ      PC
NOAGENDA 
.        DISPLAY   *P1:24,*EL,*HON,*B,"NO MESSAGE SERVICES ON-LINE",*HOFF;
         XIF
MENU2B   CMATCH    "*" TO ONSW
         GOTO      TIME IF EQUAL        *IF JUST REQUEST FOR TIME UPDATE
.                                           *DON'T WRITE TO LOGFILE
         MOVE      "PROGRAM",LOGTYPE
         MOVE      "MASTER",LOGINFO
         CALL      LOGWRITE
         GOTO      PAGE1
.TIME     SCAN      "2UBE" IN ATTRIB
.         GOTO      TIME1 IF NOT EQUAL
TIME     IFNZ      PC
         DISPLAY   *P3:3,*HON,TIMEDISP,*HOFF
         XIF
         IFZ       PC
         DISPLAY   *P3:3,*DION,*YELLOW,TIMEDISP,*DIOFF,*WHITE
         XIF
         TRAPCLR   F5
         NORETURN
         GOTO      GETINDEX 
TIME1    DISPLAY   *P3:3,*HON,TIMEDISP,*HOFF
         TRAPCLR   F5
         NORETURN
         GOTO      GETINDEX
PAGE1
         TRAPCLR   F1
         TRAP      PAGE2 IF F1
         IFNZ      PC
.Begin Patch 2.96   
          IF        (NuseComp = "P")
          MOve      c2,company
         DISPLAY   024:
                   *P1:1,*ES,*HON,CTL,*RPTCHAR LINE:78,CTR:
                   *P1:2,BAR,*P80:2,BAR,*P1:3,BAR,*P80:3,BAR,*P1:4,BAR:
                   *P80:4,BAR:
                   *P1:5,CBL,*RPTCHAR LINE:78,CBR
                  *P23:2,*HON,"P  A  C  I  F  I  C     L  I  S  T  S   ":
                   *P70:3,"PAGE ONE":
                   *P27:4,*HOFF,"M A S T E R   M E N U"
          Else
          MOve      c1,company
         DISPLAY   024:
                   *P1:1,*ES,*HON,CTL,*RPTCHAR LINE:78,CTR:
                   *P1:2,BAR,*P80:2,BAR,*P1:3,BAR,*P80:3,BAR,*P1:4,BAR:
                   *P80:4,BAR:
                   *P1:5,CBL,*RPTCHAR LINE:78,CBR
.         DISPLAY   *P13:2,*HON,"N A M E S   I N   T H E   N E W S    ":
         DISPLAY   *P23:2,*HON,"N A M E S   I N   T H E   N E W S    ":
.                          "C A L I F O R N I A":
                   *P70:3,"PAGE ONE":
                   *P27:4,*HOFF,"M A S T E R   M E N U"
          Endif
.End Patch 2.96     
         XIF
         IFZ       PC
.Begin Patch 2.96   
          IF        (NuseComp = "P")
          move      c2,company
          DISPLAY   024:
                   *P1:1,*ES,*RED,*ulcdd,*RPTCHAR *hlndd:78,*urcdd:
                   *P1:2,*vlndd,*P80:2,*vlndd,*P1:3,*vlndd,*P80:3,*vlndd:
                   *P1:4,*vlndd:
                   *P80:4,*vlndd:
                   *P1:5,*llcdd,*RPTCHAR *hlndd:78,*lrcdd:
                  *P23:2,*DION,*YELLOW,"P  A  C  I  F  I  C     L  I  S  T  S   ":
.;                          "C A L I F O R N I A":
                   *P70:3,"PAGE ONE":
                   *P27:4,*HOFF,"M A S T E R   M E N U":
                   *WHITE,*DIOFF
          ELse
          move      c1,company
          DISPLAY   024:
                   *P1:1,*ES,*RED,*ulcdd,*RPTCHAR *hlndd:78,*urcdd:
                   *P1:2,*vlndd,*P80:2,*vlndd,*P1:3,*vlndd,*P80:3,*vlndd:
                   *P1:4,*vlndd:
                   *P80:4,*vlndd:
                   *P1:5,*llcdd,*RPTCHAR *hlndd:78,*lrcdd:
                  *P23:2,*DION,*YELLOW,"N A M E S   I N   T H E   N E W S    ":
.;                          "C A L I F O R N I A":
                   *P70:3,"PAGE ONE":
                   *P27:4,*HOFF,"M A S T E R   M E N U":
                   *WHITE,*DIOFF
          Endif
.End Patch 2.96     
         XIF
         DISPLAY   *P01:06," 1...ORDER ENTRY & INQUIRY":
                   *P01:07," 2...INVOICE ENTRY & INQUIRY ":
                   *P01:08," 3...FULFILLMENT ENTRY & INQUIRY ":
                   *P01:09," 4...DATA CARD ENTRY & INQUIRY":
                   *P01:10," 5...SHIPMENT ENTRY & INQUIRY":
                   *P01:11," 6...CASH RECEIPT/ADJUSTMENT PROGRAM":
                   *P01:12," 7...Escrow FILE MAINTENANCE":
                   *P01:13," 8...SRDS Datacard Search":
                   *P01:14," 9...LIST OWNER FILE MAINTENANCE":
                   *P01:15,"10...Booking Instructions":
                   *P01:16,"11...DATA CARD (LIST) REQUESTS":
                   *P01:17,"12...ORDER MERGE INQUIRY":
                   *P01:18,"13...BILL-TO FILE MAINTENANCE":
                   *P42:6,"14...PAY-TO FILE MAINTENANCE":
                   *P42:7,"15...RETURN-TO FILE MAINTENANCE":
                   *P42:08,"16...ORDER HISTORY PIC PROGRAM":
                   *P42:09,"17...CALCULATOR PROGRAM":
                   *P42:10,"18...LIST APPROVAL PROGRAM":
                   *P42:11,"19...RUNNING EXCHANGE CHARGES PROGRAM":
                   *P42:12,"20...EXCHANGE STATUS INQUIRY":
                   *P42:13,"21...MIN Datacard Search":
                   *P42:14,"22...invoice l/o # mod program  ":
                   *P42:15,"23...LIST Rental Agreement PROGRAM ":
                   *P42:16,"24...Marketing List Maint":
                   *P42:17,"25...CORRECTION/CANCELLATION PROGRAM":
                   *P42:18,"26...MAILDATE SCHEDULE PROGRAM ":
                   *P01:19,*HON,"(F1 for Page 2)",*HOFF:
                   *P01:20,"(F6 NIN Intranet)":
                   *P01:21,"(F7 NIN Website)":
                   *P42:19,"(F3 to Exit Menu)":
                   *P42:20,"(F4 for ext. directory)":
                   *P62:22,"Port   : ",PORTN,B1,INITs:
                   *P62:23,"Day    : ",DAY
         MOVE      "*" TO ONSW
         CALL      TIME
         GOTO      GETINDEX
PAGE2
         TRAPCLR   F1
         TRAP      PAGE1 IF F1
         NORETURN
.Begin Patch 2.96   
          IF        (NuseComp = "P")
         DISPLAY   024:
                   *P1:1,*ES,*HON,CTL,*RPTCHAR LINE:78,CTR:
                   *P1:2,BAR,*P80:2,BAR,*P1:3,BAR,*P80:3,BAR,*P1:4,BAR:
                   *P80:4,BAR:
                   *P1:5,CBL,*RPTCHAR LINE:78,CBR
         Display           *P23:2,*HON,"P  A  C  I  F  I  C     L  I  S  T  S   ":
                   *P70:3,"PAGE TWO":
                   *P27:4,*HOFF,"M A S T E R   M E N U"
          Else
         DISPLAY   024:
                   *P1:1,*ES,*HON,CTL,*RPTCHAR LINE:78,CTR:
                   *P1:2,BAR,*P80:2,BAR,*P1:3,BAR,*P80:3,BAR,*P1:4,BAR:
                   *P80:4,BAR:
                   *P1:5,CBL,*RPTCHAR LINE:78,CBR
.         DISPLAY   *P13:2,*HON,"N A M E S   I N   T H E   N E W S    ":
         DISPLAY   *P23:2,*HON,"N A M E S   I N   T H E   N E W S    ":
.                          "C A L I F O R N I A":
                   *P70:3,"PAGE TWO":
                   *P27:4,*HOFF,"M A S T E R   M E N U"
          Endif
.End Patch 2.96     
         XIF
         IFZ       PC
.Begin Patch 2.96   
          IF        (NuseComp = "P")
          DISPLAY   024:
                   *P1:1,*ES,*RED,*ulcdd,*RPTCHAR *hlndd:78,*urcdd:
                   *P1:2,*vlndd,*P80:2,*vlndd,*P1:3,*vlndd,*P80:3,*vlndd:
                   *P1:4,*vlndd:
                   *P80:4,*vlndd:
                   *P1:5,*llcdd,*RPTCHAR *hlndd:78,*lrcdd:
                  *P23:2,*DION,*YELLOW,"P  A  C  I  F  I  C     L  I  S  T  S   ":
.;                          "C A L I F O R N I A":
                   *P70:3,"PAGE TWO":
                   *P27:4,*HOFF,"M A S T E R   M E N U":
                   *WHITE,*DIOFF
          ELse
          DISPLAY   024:
                   *P1:1,*ES,*RED,*ulcdd,*RPTCHAR *hlndd:78,*urcdd:
                   *P1:2,*vlndd,*P80:2,*vlndd,*P1:3,*vlndd,*P80:3,*vlndd:
                   *P1:4,*vlndd:
                   *P80:4,*vlndd:
                   *P1:5,*llcdd,*RPTCHAR *hlndd:78,*lrcdd:
                  *P23:2,*DION,*YELLOW,"N A M E S   I N   T H E   N E W S    ":
.;                          "C A L I F O R N I A":
                   *P70:3,"PAGE TWO":
                   *P27:4,*HOFF,"M A S T E R   M E N U":
                   *WHITE,*DIOFF
          Endif
.End Patch 2.96     
          XIF
.START PATCH 2.9 REPLACED LOGIC
.         DISPLAY   *P01:07,"22...invoice l/o # mod program  ":
.                   *P01:08,"23...LIST USAGE INQUIRY PROGRAM ":
.                   *P01:09,"24...FAX Promo List Maint":
.                   *P01:10,"25...LIST APPROVAL REQUEST USAGEa PROG":
.                   *P01:11,"26...MAILDATE SCHEDULE PROGRAM ":
.                   *P01:12,"27...MANUAL ACCOUNTING JOURNAL ":
.                   *P01:13,"28...CORRECTION/CANCELLATION PROGRAM ":
.                   *P01:14,"29...APPROVAL SUMMARY PROGRAM  ":
.                   *P01:15,"30...FAX PROMOTIONS ":
.                   *P01:16,"31...LIST COUNTS INQUIRY":
.                   *P01:17,"32...MARK ORDERS BILLED":
.                   *P01:18,"33...Mailer Category Maint.":
.                   *P42:07,"34...Broker/Consultant Inq/Maint.":
.                   *P42:08,"35...CHECK INFORMATION MAINT.":
.                   *P42:09,"36...List Owner Payable Account":
.                   *P42:10,"37...Mailer payment inquiry":
.                   *P42:11,"38...TELECOMMUNICATION MAINT. MENU":
.                   *P42:12,"39...TDMC BILLING PROGRAM":
.                   *P42:13,"40...FIXCARD PROGRAM":
.                   *P42:14,"41...MAIL PLANNER PROGRAM":
.                   *P42:15,"42...Money on account":
.                   *P42:16,"43...Amount Due":
.                   *P42:17,"44...Request For Counts":
.                   *P42:18,"45...Rolodex Inquiry":
.                   *P01:20,*HON,"(F1 for Page 1)",*HOFF:
.                   *P42:20,"(F3 to Exit Menu)",*P42:20,*EL:
.                   *P42:21,"(F4 for ext. directory)":
.                   *P62:23,"Port   : ",PORTN,B1,INITs:
.                   *P62:24,"Day    : ",DAY
          DISPLAY   *P01:06,"27...BROKER CALL PROGRAM ":
                   *P01:07,"28...Special Pricing Notes Program":
                   *P01:08,"29...APPROVAL SUMMARY PROGRAM  ":
                   *P01:09,"30...List Management Denied LCRS ":
                   *P01:10,"31...Promotional Fax":
                   *P01:11,"32...MARK ORDERS BILLED":
                   *P01:12,"33...Client Credit History":
                   *P01:13,"34...Broker/Consultant/Mailer/Offer":
                   *P01:14,"35...CHECK INFORMATION MAINT.":
                   *P01:15,"36...List Owner Payable Account":
                   *P01:16,"37...Bankruptcy inquiry":
                   *P01:17,"38...TELECOMMUNICATION MAINT. MENU":
                   *P01:18,"39...Exchange/Run Charge Billing":
                   *P42:06,"40...List Management Order Reports":
                   *P42:07,"41...MAIL PLANNER PROGRAM":
                   *P42:08,"42...Money on account":
                   *P42:09,"43...Amount Due":
                   *P42:10,"44...Request For Counts":
                   *P42:11,"45...Suppression Report":
                   *P42:12,"46...List Income and Order Variance":
                   *P42:13,"47...Sample Scanning Program":
                   *P42:14,"48...New Order Inquiry Program":
                   *P42:15,"49...Website Tracking Program":
                   *P42:16,"50...List Owner Income Reports":
                   *P42:17,"51...New MLR/Brkr Usage Summary Report":
                   *P42:18,"52...Variance Report"
.END PATCH 2.94 ADDED LOGIC
.Begin Patch 2.96   
          IF        (NuseComp = "P")
            Display       *P01:19,*HON,"(F1 for Page 1)",*HOFF:
                   *P01:20,"(F6 NIN Intranet)":
                   *P01:21,"(F7 NIN Website)":
                   *P42:19,"(F3 to Exit Menu)":
                   *P42:20,"(F4 for ext. directory)":
                   *P62:22,"Port   : ",PORTN,B1,INITs:
                   *P62:23,"Day    : ",DAY
           Else
           Display        *P01:19,*HON,"(F1 for Page 1)",*HOFF:
                   *P01:20,"(F6 PL Intranet)":
                   *P01:21,"(F7 PL Website)":
                   *P42:19,"(F3 to Exit Menu)":
                   *P42:20,"(F4 for ext. directory)":
                   *P62:22,"Port   : ",PORTN,B1,INITs:
                   *P62:23,"Day    : ",DAY
           Endif
                              
.End Patch 2.96                   
.END PATCH 2.9 REPLACED LOGIC
         CALL      TIME
         TRAP      GOPAGE1 IF F1
         move      c0 to index
         DISPLAY   *P62:20,*EL:
                   *P1:24,*EL,"Please enter your selection by number ":
                   "& ",0033,": __":
                   *P62:24,"Date   : ",mm,slash,dd,slash,cc,yy;
         KEYIN     *P44:24,*T254,INDEX;
         goto      gopage1 if f1
         goto      goout if f3
         goto      direct if f4
         Call      Intranet if F6
         Call      NINWEB if F7
         COMPARE   "0" TO INDEX
         GOTO      MASMENU IF EQUAL
.         GOTO      PAGE1 IF LESS
         GOTO      INDEX
GOPAGE1  TRAPCLR   F1
         NORETURN
         GOTO      PAGE1
PAGE3
.Begin Patch 2.96   
          IF        (NuseComp = "P")
         DISPLAY   024:
                   *P1:1,*ES,*HON,CTL,*RPTCHAR LINE:78,CTR:
                   *P1:2,BAR,*P80:2,BAR,*P1:3,BAR,*P80:3,BAR,*P1:4,BAR:
                   *P80:4,BAR:
                   *P1:5,CBL,*RPTCHAR LINE:78,CBR
        Display          *P23:2,*HON,"P  A  C  I  F  I  C     L  I  S  T  S   ":
                   *P70:3,"PAGE THREE":
                   *P27:4,*HOFF,"M A S T E R   M E N U"
          Else
         DISPLAY   024:
                   *P1:1,*ES,*HON,CTL,*RPTCHAR LINE:78,CTR:
                   *P1:2,BAR,*P80:2,BAR,*P1:3,BAR,*P80:3,BAR,*P1:4,BAR:
                   *P80:4,BAR:
                   *P1:5,CBL,*RPTCHAR LINE:78,CBR
.         DISPLAY   *P13:2,*HON,"N A M E S   I N   T H E   N E W S    ":
         DISPLAY   *P23:2,*HON,"N A M E S   I N   T H E   N E W S    ":
.                          "C A L I F O R N I A":
                   *P70:3,"PAGE THREE":
                   *P27:4,*HOFF,"M A S T E R   M E N U"
          Endif
.End Patch 2.96     
         XIF
         IFZ       PC
.Begin Patch 2.96   
          IF        (NuseComp = "P")
          DISPLAY   024:
                   *P1:1,*ES,*RED,*ulcdd,*RPTCHAR *hlndd:78,*urcdd:
                   *P1:2,*vlndd,*P80:2,*vlndd,*P1:3,*vlndd,*P80:3,*vlndd:
                   *P1:4,*vlndd:
                   *P80:4,*vlndd:
                   *P1:5,*llcdd,*RPTCHAR *hlndd:78,*lrcdd:
                  *P23:2,*DION,*YELLOW,"P  A  C  I  F  I  C     L  I  S  T  S   ":
.;                          "C A L I F O R N I A":
                   *P70:3,"PAGE THREE":
                   *P27:4,*HOFF,"M A S T E R   M E N U":
                   *WHITE,*DIOFF
          ELse
          DISPLAY   024:
                   *P1:1,*ES,*RED,*ulcdd,*RPTCHAR *hlndd:78,*urcdd:
                   *P1:2,*vlndd,*P80:2,*vlndd,*P1:3,*vlndd,*P80:3,*vlndd:
                   *P1:4,*vlndd:
                   *P80:4,*vlndd:
                   *P1:5,*llcdd,*RPTCHAR *hlndd:78,*lrcdd:
                  *P23:2,*DION,*YELLOW,"N A M E S   I N   T H E   N E W S    ":
.;                          "C A L I F O R N I A":
                   *P70:3,"PAGE THREE":
                   *P27:4,*HOFF,"M A S T E R   M E N U":
                   *WHITE,*DIOFF
          Endif
.End Patch 2.96     
          XIF
         DISPLAY   *P01:07,"70...CALPIC -OWNER PICK FOR TRANSMISSION":
                   *P01:08,"71...DATAMRG - DATAPICK MERGE. ":
                   *P01:09,"72...DATAPEXT - MULT. COPY DATACARD.":
                   *P01:10,"73...DATAPSYS - DATACARD PRINT":
                   *P01:11,"74...DATPRINT - RH STYLE DATACARD":
                   *P01:12,"75...DELREC - RECORD DELETION PROGRAM":
                   *P01:13,"76...DSLIST1 - FILE DUMP PROGRAM ":
                   *P01:14,"77...DSTEXT - DATASHARE UTILITIES ":
                   *P01:15,"78...FIXBUSY -ORDERS/INVOICES/ADJUSTMENTS ":
                   *P01:16,"79...FIXCARD - REINSTATE DATACARDS":
                   *P01:17,"80...FIXCHNG - REINSTATE EXCHANGES":
                   *P01:18,"81...FIXINV - REPAIR INVOICE PROG.":
                   *P01:19,"82...FIXLCR1 - CLEAR BUSY ON LCR'S.  ":
                   *P01:20,"83...FIXORD - REPAIR ORDER PROG.":
                   *P42:07,"84...FIXXNUM - EXCHANGE MAINT. PROGRAM ":
                   *P42:08,"85...LCRMAINT - LCR MAINTENANCE ":
                   *P42:09,"86...LISTNIN - MASTER MAILER PROGRAM ":
                   *P42:10,"87...LISTNIN1 - MINI MAILER PROGRAM ":
                   *P42:11,"88...MARK - ORDERS/INVOICES....":
                   *P42:12,"89...ORDERPRT - ORDER STATISTICS PROGRAM ":
                   *P42:13,"90...OWNNEW - DATACARD OWNER INFO. ":
                   *P42:14,"91...PASSMOD - PASSWORD MODIFICATION":
                   *P42:15,"92...SPIMOD - SPECIAL INSTRUCTION MOD.":
                   *P42:16,"93...STARTER - START JOBS ":
                   *P42:17,"94...SUPERPIC - SUPER DATACARD PICK",*HON:
                   *P01:21,"(tap Enter for  Unlisted programs)",*HOFF:
                   *P3:3,*HON,TIMEDISP,*HOFF:
                   *P62:22,"Port   : ",PORTN,B1,INITs:
                   *P62:23,"Day    : ",DAY,*HOFF:
                   *P62:24,"Date   : ",mm,slash,dd,slash,cc,yy;
         KEYIN     *P62:20,*EL:
                   *P1:24,*EL,"Please enter your selection by number ":
                   "& ",0033,": __":
                   *P44:24,*T254,INDEX,*HOFF;
         COMPARE   "1" TO INDEX
         GOTO      PROG IF LESS
         GOTO      GETPROG
. .............................................................................
. DIRECT - DISPLAY IN HOUSE EXTENSION DIRECTORY
DIRECT
         TRAPCLR   F4
         NORETURN
         TRAP      DIRECT IF F4
.begin patch 2.5
.         DISPLAY   *P1:6,*EF
.         IFNZ      PC
.         OPEN      DIRECT,"DIRECT/ORY"
.         XIF
.         IFZ       PC
.         OPEN      DIRECT,"DIRECT.ORY"
.         XIF
.         MOVE      "07" TO V
.         MOVE      "01" TO H
.DREAD    READ      DIRECT,SEQ;ANS,ANS,ANS,DNAME,DNUM
.         GOTO      DIRDONE IF OVER
.         DISPLAY   *PH:V,"...",DNAME,DNUM
.         ADD       "1" TO V
. ..        COMPARE   "24" TO V
.         GOTO      DIRESET IF EQUAL
.         GOTO      DREAD
.DIRESET  COMPARE   "42" TO H
.         GOTO      DIRESET1 IF EQUAL
.         MOVE      "42" TO H
.         MOVE      "07" TO V
.         GOTO      DREAD
.DIRESET1 KEYIN     *P1:24,*EL,"THERE IS MORE HIT <ENTER> WHEN READY ":
.                   *T30,ANS,*P1:07,*EF;
.         MOVE      "01" TO H
.         MOVE      "07" TO V
.         GOTO      DREAD
.DIRDONE  KEYIN     *P1:24,*EL,"THAT'S ALL",ANS;
.         CLOSE     DIRECT
         execute   "\\nins1\e\apps\plb\code\plbwin.exe \\nins1\e\apps\plb\code\rolo.plc"
         GOTO      PAGE1
.
Intranet      
            Trapclr         F6
            Trap            Intranet if F6
          Clock     Version,str35
          scan      "cliwin",str35
          if        Not equal           .not running PLbclient
          execute   "c:\progra~1\Intern~1\iexplore.exe http://Web01/"          
                    else
                    alert     Note,"Not available logged in this way - Sorry",result
                    endif 
            return
NINWEB      
            Trapclr         F6
            Trap            Intranet if F6
          Clock     Version,str35
          scan      "cliwin",str35
          if        Not equal           .not running PLbclient
.END PATCH 2.94 ADDED LOGIC
.Begin Patch 2.96   
                    IF        (NuseComp = "P")
                      execute   "c:\progra~1\Intern~1\iexplore.exe http://WWW.PacificLists.com/"
                    Else
                      execute   "c:\progra~1\Intern~1\iexplore.exe http://WWW.NINcal.com/"
                              Endif
          
.End Patch 2.96                   

                    else
          pack      Taskname,"Not available logged in this way - Sorry",Crlf:
                    "www.namesinthenews.com or www.pacificlists.com"
                    alert     Note,taskname,result
                    endif 
            return
              
*................................................................
. GET THE PROGRAM'S INDEX
.
GETINDEX MOVE      "*" TO ONSW        *ALL READY LOGGED ON
         TRAP      MASMENU IF F5     *TIME REQUEST
         DISPLAY   *P62:20,*EL:
                   *P1:24,*EL,"Please enter your selection by number ":
                   "& ",0033,": __":
                   *P62:24,"Date   : ",mm,slash,dd,slash,cc,yy;
         KEYIN     *P44:24,*T254,INDEX;
         goto      page2 if f1
         goto      goout if f3
         COMPARE   "0" TO INDEX
         GOTO      MASMENU IF EQUAL
.         COMPARE   "1" TO INDEX
.         GOTO      PAGE2 IF LESS
INDEX    
         COMPARE   "99" TO INDEX
         GOTO      OPER IF EQUAL
.START PATCH 2.94 REPLACED LOGIC
.         COMPARE   "47" TO INDEX
         COMPARE   "55" TO INDEX
.END PATCH 2.94 REPLACED LOGIC
         GOTO      GETINDEX IF NOT  LESS
         GOTO      GETPROG
OPER     KEYIN     *P1:24,*HON,*EL,"ENTER PASSWORD: ",*ESON,PASS,*ESOFF,*HOFF;
         RESET     PASS TO 4
         RESET     PASS
         MATCH     PASSMAST,PASS
         GOTO      PAGE3 IF EQUAL
         MOVE      "BAD-ID",LOGTYPE
         CALL      LOGWRITE
         GOTO      GETINDEX
PROG     KEYIN     *P1:24,*EL,"PROGRAM NAME: ",PROGNAME;
         TRAP      NONAME GIVING ERROR IF CFAIL
         MOVE      PROGNAME,LOGINFO
         MOVE      PASS,LOGTYPE
         CALL      LOGWRITE
         CHAIN     PROGNAME
NONAME   NORETURN
         MOVE      "NO SUCH PROGRAM",LOGINFO
         DISPLAY   *P60:23,*HON,"*** NO SUCH PROGRAM ***",*W,*P60:23,*EL,*HOFF;
         DISPLAY   *P60:24,*HON,ERROR,*W,*P60:24,*EL,*HOFF;
         GOTO      PROG
.................................................................
. BRANCH TO THE ROUTINE INDICATED BY THE INDEX
.
GETPROG  TRAP      NOTFOUND GIVING ERROR IF CFAIL
         MOVE      "PROGRAM",LOGTYPE
         BRANCH    AGENDASW OF CLOSEAG
         GOTO      PRGBRNCH
CLOSEAG  CLOSE     AGENDAM
.START PATCH 2.9 REPLACED LOGIC
.PRGBRNCH BRANCH    INDEX OF NINMAC:     # 1
.                            MODINV:     # 2
.                            ADDADJ:     # 3
.                            DATAMOD:    # 4
.                            SHPMOD:     # 5
.                            CASH:       # 6
.                            MLRMOD:     # 7
.                            OFRMOD:     # 8
.                            OWNMOD:     # 9
.                            nmdl0001:     #10
.                            DATAPIC:    #11
.                            MERGE:     #12
.                            BILMOD:     #13
.                            PAYMOD:     #14
.                            RTNMOD:     #15
.                            ORDERPIC:   #16
.                            CALCPROG:   #17
.                            NINCLEAR:   #18
.                            RUNCAL:     #19
.                            EXCHANGE:   #20
.                            TESTODAY:    #21
.                            FIXINV:    #22
.                            NINUSAGE:    #23
.                            FaxPromo:   #24
.                            LCRUSAGE:    #25
.                            SCHEDULE:   #26
.                            JOURNAL:    #27
.                            NCRC0001:   #28
.                            NAPR0001:   #29
.                            LISTHIST:   #30
.                            NINCOUNT:   #31
.                            NINOBILL:   #32
.                            Nmlr0007:   #33
.                            NBRK0001:   #34
.                            CHCKINFO:   #35
.                            ninv0013:       #36
.                            ninv17:     #37
.                            TELEMAST:   #38
.                            ndat0022:    #39
.                            Nord0037:    #40
.                            MAILPLAN:   #41
.                            ONACOUNT:  #42
.                            AMOUNTD:  #43
.                            nrco0001:   #44
.                            Rolodex:   #45
.                   GETINDEX,GETINDEX,GETINDEX,GETINDEX,GETINDEX:
.                   GETINDEX,GETINDEX,GETINDEX,GETINDEX,GETINDEX,GETINDEX:
.                   GETINDEX,GETINDEX,GETINDEX,GETINDEX,GETINDEX,GETINDEX:
.                   GETINDEX,GETINDEX,GETINDEX,GETINDEX,GETINDEX,GETINDEX:
.                   GETINDEX:
.                            CALPIC:     #70
.                            DATAMRG:    #71
.                            DATAPEXT:   #72
.                            DATAPSYS:   #73
.                            DATPRINT:   #74
.                            DELREC:     #75
.                            DSLIST1:    #76
.                            DSTEXT:     #77
.                            FIXBUSY:    #78
.                             FIXCARD:    #79
.                            FIXCHNG:    #80
.                            FIXINV:     #81
.                            FIXLCR1:     #82
.                            FIXORD:     #83
.                            FIXXNUM:    #84
.                            LCRMAINT:   #85
.                            LISTNIN:    #86
.                            LISTNIN1:   #87
.                            MARK:       #88
.                            ORDERPRT:   #89
.                            OWNNEW:     #90
.                            PASSMOD:    #91
.                            SPIMOD:     #92
.                            STARTER:     #93
.                            SUPERPIC    #94
PRGBRNCH BRANCH    INDEX OF NINMAC:     # 1
                            MODINV:     # 2
                            NFUL0001:     # 3
                            DATAMOD:    # 4
                            SHPMOD:     # 5
                            CASH:       # 6
                            Escrow:     # 7
                            S2N0003:  # 8                             .SRDS
                            OWNMOD:     # 9
                            nmdl0001:     #10
                            DATAPIC:    #11
                            MERGE:     #12
                            BILMOD:     #13
                            PAYMOD:     #14
                            RTNMOD:     #15
                            ORDERPIC:   #16
                            CALCPROG:   #17
                            NINCLEAR:   #18
                            RUNCAL:     #19
                            EXCHANGE:   #20
                            M2n0003:    #21                                     .MIN
                            FIXINV:    #22
                            NINLRA:    #23
                            MrktList:   #24
                            CORRECTION:    #25
.                            LCRUSAGE:    #25
                            SCHEDULE:   #26
                            NCAL0001:    #27
.START PATCH 2.93 REPLACED LOGIC
.                            NCRC0001:   #28
                            NMLRXY0001:   #28
.END PATCH 2.93 REPLACED LOGIC
                            NAPR0001:   #29
                            Nord0045:   #30
                            NPRM0001:   #31
                            NINOBILL:   #32
                            Credit0001:   #33
                            comp0001:   #34
                            CHCKINFO:   #35
                            ninv0013:       #36
                            ninv0024:     #37
                            TELEMAST:   #38
                            ndat0022:    #39
                            NORD0037:    #40
                            MAILPLAN:   #41
                            ONACOUNT:  #42
                            AMOUNTD:  #43
                            nrco0001:   #44
                            SUPPRESS:   #45
                            Nvar0001:    #46
.START PATCH 2.94 REPLACED LOGIC
.                   GETINDEX,GETINDEX,GETINDEX,GETINDEX:
                            NSMP0001:    #47
                            Nord8001:    #48
                            NWeb0001:    #49
                            Nloinc0007:    #50
                            Nrep0001:      #51
.END PATCH 2.94 REPLACED LOGIC
                        NVAR0001:       #52
                   GETINDEX,TESTODAY,GETINDEX,GETINDEX:
                   GETINDEX,GETINDEX,GETINDEX,GETINDEX,GETINDEX,GETINDEX:
                   GETINDEX,GETINDEX,GETINDEX,GETINDEX,GETINDEX,GETINDEX:
                   GETINDEX:
                            CALPIC:     #70
                            DATAMRG:    #71
                            DATAPEXT:   #72
                            DATAPSYS:   #73
                            DATPRINT:   #74
                            DELREC:     #75
                            DSLIST1:    #76
                            DSTEXT:     #77
                            FIXBUSY:    #78
                             FIXCARD:    #79
                            FIXCHNG:    #80
                            FIXINV:     #81
                            FIXLCR1:     #82
                            FIXORD:     #83
                            FIXXNUM:    #84
                            LCRMAINT:   #85
                            LISTNIN:    #86
                            LISTNIN1:   #87
                            MARK:       #88
                            ORDERPRT:   #89
                            OWNNEW:     #90
                            PASSMOD:    #91
                            SPIMOD:     #92
                            STARTER:     #93
                            SUPERPIC    #94
.END PATCH 2.9 REPLACED LOGIC
         GOTO      GETINDEX
*................................................................
NINMAC
.         if        (portn = 31 or portn = 112 OR PORTN = 188 OR PORTN =119 OR PORTN = 19 OR PORTN = 56 OR PORTN = 57 OR PORTN = 222 OR PORTN = 24 OR PORTN = 230 OR PORTN = 250)
.         MOVE      "Drewtest",LOGINFO
.         CALL      LOGWRITE
.         CHAIN     "Drewtest"
.         endif
         MOVE      "NORD0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NORDTEST"
MODINV   MOVE      "NINV0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NINV0001"
BetaINV  MOVE      "NINV0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "master"
NFUL0001 MOVE      "NFUL0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NFUL0001"
DATAMOD  MOVE      "NDAT0001",LOGINFO
         CALL      LOGWRITE
         COMPARE   "101" to portn         *wildcat client dial in ????
         IF        EQUAL
         setmode   *fullscr=MAX
         chain     "ndat0037"
         else
         CHAIN     "NDAT0001"
         endif
NDAT0001a  MOVE      "NDAT0001a",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NDAT0001a"
SHPMOD   MOVE      "NSHP0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NSHP0001"
CASH
         MOVE      "NCSH0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NCSH0001"
Escrow
         MOVE      "Nesc0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "Nesc0001"
MLRMOD   MOVE      "NMLR0001",LOGINFO
         CALL      LOGWRITE
.         CHAIN     "NMLR0001"
         CHAIN     "comp0001"
OFRMOD   MOVE      "NMLR0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "comp0001"
Nord0037 MOVE      "NORD0037",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NORD0037"
NWEB0001 MOVE      "NWEB0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NWEB0001"
NREp0001 MOVE      "NREp0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NREp0001"
NLOINC0007 MOVE      "NLOINC0007",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NLOINC0007"
OWNMOD   MOVE      "NOWN0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NOWN0001"
nmdl0001 MOVE      "Nmdl0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "Nmdl0001"
.START PATCH 2.9 REPLACED LOGIC
.ROLODEX  MOVE      "NROL0001",LOGINFO
.         CALL      LOGWRITE
.         CHAIN     "NROL0001"
SUPPRESS MOVE      "NSPR0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NSPR0001"
.END PATCH 2.9 REPLACED LOGIC
DATAPIC  MOVE      "NDAT0004",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NDAT0004"
MERGE    MOVE      "NMRG0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NMRG0001"
BILMOD   MOVE      "NBIL0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NBIL0001"
PAYMOD   MOVE      "NPAY0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NPAY0001"
RTNMOD   MOVE      "NRTN0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NRTN0001"
ORDERPIC MOVE      "NORD0006",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NORD0006"
CALCPROG MOVE      "Calc,exe",LOGINFO
         CALL      LOGWRITE
         Batch   "c:\Windows\system32\calc.exe"      
.         CHAIN     "NCLC0001"
          Noreturn
          Goto      Getindex
NINCLEAR MOVE      "NLCR0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NLCR0001"
RUNCAL   MOVE      "NINV0008",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NINV0008"

NVAR0001 MOVE      "NVAR0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NVAR0001"
TESTODAY MOVE      "TestoDay",LOGINFO
         CALL      LOGWRITE
         CHAIN     "Testoday"
CHCKINFO MOVE      "NCHK0005" TO LOGINFO
         CALL      LOGWRITE
         CHAIN     "NCHK0005"
EXCHANGE MOVE      "NXCH0001",LOGINFO
         move      "NXCH0001",program
         CALL      LOGWRITE
         CHAIN     "NXCH0001"
NINV0024 MOVE      "NINV0024",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NINV0024"
NINLRA
         MOVE      "NLRA0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NLRA0001"
MrktList       MOVe           "Mrkt0001",loginfo
               Chain          "Mrkt0001"
FaxPromo       MOVe           "nprm0001",loginfo
               Chain          "NPrm0001"
.               winshow
.EXCHPRNT MOVE      "NXCH0002",LOGINFO
.         CALL      LOGWRITE
.         CHAIN     "NXCH0002"
.LCRUSAGE MOVE      "NLCR0006",LOGINFO
CORRECTION MOVE      "NCORR0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NCORR0001"
.         CHAIN     "NLCR0006"
SCHEDULE MOVE      "NSCH0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NSCH0001"
NCAL0001 MOVE      "NCAL0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NCAL0001"
.START PATCH 2.93 REPLACED LOGIC
.NCRC0001 MOVE      "CORRECTION",LOGINFO
.         CALL      LOGWRITE
.         CHAIN     "NCRC0001"
NMLRXY0001
         MOVE      "NMLRXY0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NMLRXY0001"
.END PATCH 2.93 REPLACED LOGIC
NAPR0001 MOVE      "APPROVAL",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NAPR0001"
Nord0045 MOVE      "Nord0045",LOGINFO
         chain     "Nord0045"
         winshow

NPRM0001 MOVE      "NPRM0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NPRM0001"
.         winshow
NINCOUNT MOVE      "NCOU0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NCOU0001"
NINOBILL MOVE      "NINV0007",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NINV0007"
Credit0001 MOVE    "credit0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "Credit0001"
comp0001 MOVE      "comp0001" TO LOGINFO
         CALL      LOGWRITE
         CHAIN     "comp0001"
NDAT0022 MOVE      "NDAT0022",LOGINFO
         CHAIN     "NDAT0022"
FIXCARD  MOVE      "NDAT0009",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NDAT0009"
MAILPLAN MOVE      "MAILPLAN" TO LOGINFO
         CALL      LOGWRITE
         CHAIN     "NMPL0001"
nrco0001 MOVE      "nrco0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "nrco0001"
nacr0001 MOVE      "nacr0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "nacr0001"
GAMEMENU MOVE      "GAMEMENU",LOGINFO
         CALL      LOGWRITE
         CHAIN     "GAMEMENU"
ninv0013 MOVE      "Ninv0013" TO LOGINFO
         CALL      LOGWRITE
         CHAIN     "Ninv0013"
ninv17   move      "ninv0024" to loginfo
         CHAIN     "ninv0024"
ONACOUNT MOVE      "NONA0001" TO LOGINFO
         CALL      LOGWRITE
         CHAIN     "NONA0001"
AMOUNTD  MOVE      "NINV0010" TO LOGINFO
         CALL      LOGWRITE
         CHAIN     "NINV0010"
DATAPSYS MOVE      "NDAT0002",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NDAT0002"
DATPRINT MOVE      "NDAT0005",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NDAT0005"
DATAPEXT MOVE      "NDAT0007",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NDAT0007"
DATAMRG  MOVE     "NDAT0006 ",LOGINFO
         CALL      LOGWRITE
         CHAIN    "NDAT0006"
DSLIST1  MOVE      "DSLIST1 ",LOGINFO
         CALL      LOGWRITE
         CHAIN    "DSLIST1"
DSTEXT   MOVE      "DSTEXT",LOGINFO
         CALL      LOGWRITE
         CHAIN    "DSTEXT"
DELREC   MOVE      "DELREC",LOGINFO
         CALL      LOGWRITE
         CHAIN    "DELREC"
FIXXNUM  MOVE      "NXCH0005",LOGINFO
         CALL      LOGWRITE
         CHAIN    "NXCH0005"
LISTNIN  MOVE      "NMLR0002",LOGINFO
         CALL      LOGWRITE
         CHAIN    "NMLR0002"
LISTNIN1 MOVE      "LISTNIN1",LOGINFO
         CALL      LOGWRITE
         CHAIN    "LISTNIN1"
ORDERPRT MOVE      "NORD0010",LOGINFO
         CALL      LOGWRITE
         CHAIN    "NORD0010"
OWNNEW   MOVE      "OWNNEW",LOGINFO
         CALL      LOGWRITE
         CHAIN    "OWNNEW"
FIXBUSY  MOVE      "NORD0021",LOGINFO
         CALL      LOGWRITE
         CHAIN    "NORD0021"
FIXLCR1  MOVE      "NLCR0010",LOGINFO
         CALL      LOGWRITE
         CHAIN    "NLCR0009"
LCRMAINT MOVE      "NLCR0007",LOGINFO
         CALL      LOGWRITE
         CHAIN    "NLCR0007"
FIXORD   MOVE      "NORD0009",LOGINFO
         CALL      LOGWRITE
         CHAIN    "NORD0009"
FIXINV   MOVE      "NINV0023",LOGINFO
         CALL      LOGWRITE
         CHAIN    "NINV0023"
FIXCHNG  MOVE      "NXCH0004",LOGINFO
         CALL      LOGWRITE
         CHAIN    "NXCH0004"
.FIXCARD  MOVE      "FIXCARD",LOGINFO
         CALL      LOGWRITE
.         CHAIN    "FIXCARD"
MARK     MOVE      "MARK",LOGINFO
         CALL      LOGWRITE
         CHAIN     "MARK"
SPIMOD   MOVE      "NSPI0001",LOGINFO
         CALL      LOGWRITE
         CHAIN    "NSPI0001"
STARTER  MOVE      "STARTER",LOGINFO
         CALL      LOGWRITE
         CHAIN    "STARTER"
SUPERPIC MOVE      "NDAT0017",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NDAT0017"
PASSMOD  MOVE      "NPAS0001",LOGINFO
         CALL      LOGWRITE
         CHAIN    "NPAS0001"
CALPIC   MOVE      "NOWN0003",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NOWN0003"
TELEMAST MOVE      "TELEMAST" TO LOGINFO
         CALL      LOGWRITE
         CHAIN     "TELEMAST"
.START PATCH 2.94 ADDED LOGIC
NSMP0001
         MOVE      "NSMP0001" TO LOGINFO
         CALL      LOGWRITE
         CHAIN     "NSMP0001"
.END PATCH 2.94 ADDED LOGIC
NORD8001
         MOVE      "NORD8001" TO LOGINFO
         CALL      LOGWRITE
         CHAIN     "NORD8001"
S2n0003  MOVE      "S2N0003" TO LOGINFO
         CALL      LOGWRITE
         CHAIN     "S2n0003"
M2n0003  MOVE      "M2N0003" TO LOGINFO
         CALL      LOGWRITE
         CHAIN     "M2n0003"

.
NOTFOUND NORETURN
         DISPLAY   *B,*P45:24,*HON:
                   *B,*P4:23,"error = ",error,*W2,*HOFF:
                   *B,*P45:24,"Sorry, this program is not on-line.",*W2,*HOFF;
         MOVE      "CHAIN ATTEMPT FAILURE",LOGINFO
         CALL      LOGWRITE
         IFNZ      PC
         CALL      OPENAGEN
         XIF
         GOTO      GETINDEX
NOTPC    DISPLAY   *P1:24,*EL,*B,*HON,"THIS IS NOT THE PC!!!!!",*B,*W3,*HOFF;
         GOTO      GETINDEX
GOOUT    MOVE      "SHUTDOWN TO OS",LOGINFO
         CALL      LOGWRITE
.         DISPLAY   *BORDER BLACK
//Use the Speak Method of this control
              if              (soundflag = 1 & ClntServFlag = c0)
               pack           str500 from "Good Bye ",Fname
               Sapi.Speak using Str500
              endif               

.         SNDOPEN   sfile,"\\nins1\e\netutils\Media\bye.wav"
.         SNDPLAY   sfile
.        SNDCLOSE   sfile

         STOP
OPENAGEN
         BRANCH    DOW OF NOPEN,OPENOK,OPENOK,OPENQUES,OPENOK,OPENQUES,NOPEN
         RETURN
OPENOK   OPEN      AGENDAM,"AGENDAM",SHARE
         MOVE      "1" TO AGENDASW
         RETURN
.OPENQUES - IF WED OR FRIDAY AND AFTER 20HNDRD HRS DO NOT OPEN FILE.
OPENQUES
         CLOCK     TIME TO TIME
         UNPACK    TIME INTO STR2,ANS,MIN,ANS,SEC
         MOVE      TIME,STR2
         MOVE      STR2,HOUR
         COMPARE   "20" TO HOUR
         RETURN    IF NOT LESS
         GOTO      OPENOK
NOPEN    RETURN
logo     if        (portn <= 0)
logo1    trapclr   F10
         trap      logo1 if F10
.         create    LOGO=20:24:65:85,"\\nins1\e\netutils\dma.gif"
.         activate  Logo
.         pause     "2"
.         deactivate  Logo
.         create    LOGO=04:18:12:85,"\\nins1\e\netutils\ninca.gif",border
         create    LOGO=04:12:01:60,"\\nins1\e\netutils\Logo color box only.jpg",border
         activate  Logo
              if              (soundflag = 1)
          CLEAR I4A
          MOVE SND_FILENAME,I4B
          ADD SND_ASYNC,I4B
          ADD SND_NODEFAULT,I4B
          pack      Taskname,"\\nins1\e\Netutils\Media\tada.wav",null
          WINAPI PLAYSOUND USING Taskname,I4A,I4B 
.         SNDOPEN   sfile,"\\nins1\e\Netutils\Media\tada.wav"
.         SNDPLAY   sfile
.         SNDCLOSE   sfile
              endif
         pause     "2"
         deactivate  Logo
         destroy    logo
         endif
         return
.Open Pif FIle
OPenPif
.begin patch 4.2
          Clear     CompanyFlag
        trap    Piferror if io
        open    Piffile,"c:\program files\nincal\Master1"
        goto    PifError if over
        clear   n1
        loop
                add     c1,n1
                read    Piffile,seq;str1,n1
                until   over
                If      (str1 = "S")
                move    n1 to soundflag
                endif
                If      (str1 = "C")
                move    n1 to CompanyFlag
                endif
        repeat
        close   Piffile
        trapclr io
afterPif
          if        (CompanyFlag = "1")
          move      C1,Company
          move      "N",Nusecomp
          elseif    (CompanyFlag = "2")
          move      C2,Company
          move      "P",Nusecomp
          endif
.end patch 4.2
.begin patch 4.12
          if        (NuseComp = "P")
          move      c2,company
          Else
          move      c1,company
            Endif
          return
.end patch 4.12
................................................................................
Piferror
        noreturn
        move    C0,Soundflag               .change default to NO  12/08/09 DH
        erase     "c:\program files\nincal\master1.isi"
        erase     "c:\program files\nincal\master1.pre"
          
        Prep    PifFile,"c:\program files\nincal\master1.isi","c:\program files\nincal\master1.Pre","1","12"
        goto   AfterPif
.................................................................................
.XRESIZE
.           Form001.Scale
.           RETURN
             
         INCLUDE   PORTCALC.inc
.begin patch 2.91
.         INCLUDE   FINDOW.inc
.end patch 2.91
         INCLUDE   MESSAGE.inc
         INCLUDE   NUSEIO.inc
.         include   ckpatch.inc
         INCLUDE   COMLOGIC.inc
