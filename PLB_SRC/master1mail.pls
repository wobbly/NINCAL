;..............................................................................
;
;              NAMES IN THE NEWS          MASTER MENU PROGRAM
;
;..............................................................................
PC       EQU       1
;
; COMMON AREA.   THIS AREA GETS OVERWRITTEN WITH AN ELEVEN-BYTE CHARACTER
;                STRING VARIABLE WHEN AN ERROR OCCURS.
;                THE COMMON VARIABLE 'ERROR' USES THE SAME NUMBER OF BYTES
;                OF UDA AS THE COMMON VARIABLES 'PORTN' AND 'TODAY'
;
ERROR    DIM       *35
TODAY    DIM       *8
SECURITY FORM      *1
FUNC     DIM       *2
TYPINIT  DIM       *2
;PORTN    FORM      *2
PORTN    FORM      *3     *NEEDS TO BE THIS.
AGENDAID DIM       *6     *NEED THIS.
;............................................................................
;
;BEGINNING OF NEW COMMON VARIABLES 23APR92. DLH
;
JULIAN  FORM      *5                  TODAY IN yyjjj FORMAT
;
USER    DIM       *10                 USER ID
USERNME DIM       *10                 USER NAME (FIRST INIT, LAST)
PRIO    FORM      *3                  OVERALL PRIORITY LEVEL
LEVELS  DIM       *36                 SECURITY LEVELS
COMM    DIM       *1                  COMMUNICATION ALLOWED
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
;                                     FOR 30 MINUTES.
;
;.............................................................................
Vnetsup  dim       1
VRedir   dim       1
;............................................................................
;VARIABLES USED AS COMMON IN DATAMOD.... DO NOT CHANGE.
;                            POSITION OR VALUES.
TOTLIN   FORM      2
MODE     DIM       1
CONT     DIM       1
WORKNAME DIM       9
CORP     DIM       1
HOLDTE   DIM       8
HOLDPASS DIM       10
;............................................................................
;
;
release    init      "4.1"       DLH Added Mailsend
RelDate        Init           "11 August 2006"
.release    init      "4.0"       DLH Added Listviews dumped datalists got rid of tedious branch
.                                            various cleanup items
.RelDate        Init           "06 July 2006"
.release    init      "3.8"       ASH Added NWEB0001
.RelDate        Init           "22 February 2006"
.release    init      "3.7"       DMB Added NVAR0001
.RelDate        Init           "15 October 2005"
.release    init      "3.6"           ASH  Added NORD8001
.RelDate        Init           "9 August 2005"
.release    init      "3.5"           ASH  Added NSMP0001
.RelDate        Init           "9 December 2004".
.release    init      "3.4"           ASH  Added Mailer/Mailer Notes Program
.RelDate        Init           "7 December 2004"
.release    init      "3.3"           DLH  17June2003 Web Goodies IS
.RelDate        Init           "17 June 2003"
;release    init      "3.2"           DLH  16July2002 add keyin progname for IS
;release    init      "3.1"           DLH  22March2001 get rid of FINdow.inc
;release    init      "3.0"           DLH  DEC GUI
;release   init      "2.91"          DLH 31Oct2000  double check for win95 & win98 network files
;release    init      "2.9"           ASH 14SEP00 ADDED SUPPRESSION REPORT
;release    init      "2.8"           JD  31May00 replaced old fixinv prog ninv0003 with ninv0022.
;release    init      "2.7"          DLH 10Feb00 replace naugthy trap on INTerurpt with goout
;release    init     "2.62"          .10Jan00  DLH new findow.inc for 2000
;release   init      "2.61"           JD 14JUL99 replaced nord0001,nordtest(time savior)
;release   init      "2.6"           DLH 22Feb99 Add smtp mail calls
;                                   remove old email template and netware
;                                   send
;Release   init      "2.5"           DLH 22Oct98 use new directory prog
;                                   Nrol.....
;Release   init      "2.4"           DLH 25Sep98 use new comlogic Errmess subroutine
;release  init       "2.3"          DLH 6Jul98 Delete gif and signin sound except for DH
;                                  see section LOGO
;Release  init       "2.2"          DLH 19May98 Misc cleanup display gif add sound files.
;Release  INIT      "2.1"          DLH 06JAN93 ADDED INACTIVE TIMER.
;RELEASE  INIT      "R002"
         INCLUDE   NUSEDD.inc
         INCLUDE   LOGDATA.inc
         include   npasdd.inc
         INCLUDE   CONS.inc
;
;begin patch 3.0
Timer   Timer
GetTime   Timer
;Colors
white   color
grey    color
RED     COLOR
BLACK   COLOR
date    dim     8
colorfile file
PifFile   FILE              .user preference file  --- 12/19/2001 DLH
;Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font

;Set Up Menu Bar
mFile    menu
mHelp    menu
;Set Up SubMenu for Options
mOptions Menu
sColor  submenu
sSound  submenu
;Present Data for Menu Bar
FData   init    "&File;E&xit"
;EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Color;&Sound"
HData   init    "&Help;&About"

;Present Data for Colors SubMenu
CData   init    ";&Background;&Text"
Sdata   init    ";&Sound on;Sound o&ff"
;Define Collections for Object Colors
ColText Collection
ColBack Collection


;Define Colors for Each Object
FTC     color
BGC     color

colornum dim   9(2)
Fred     form    3
Fgreen   form    3
Fblue    form    3
colorflag form   1
SoundFLag form   1

;................................
coll1   collection
specs   form          4(4)
size    form          "1.000"
infostring dim        590
Tabnum  form          2
SaveTab form          2
;............................
;end patch 3.0
;
DIRECT   FILE                               .test using with plbserve
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
;MO       DIM       2
;DY       DIM       2
;YR       DIM       2
;STR2     DIM       2
LOGTIME  DIM       18
;MM       DIM       2
;DD       DIM       2
;YY       DIM       2
;NMM      FORM      2          FOR MESSAGE BRANCH
;NDD      FORM      2          FOR MESSAGE BRANCH
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
;COLON    INIT      ":"
ONSW     DIM       1
NAME     DIM       25
PORTNUM  DIM       3
ID       DIM       1
PORTX    FORM      3
;DAY OF WEEK VAR'S
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
Sapi          Automation                               //Define the ActiveX control
ProgramName    Dim            250
Str500        Dim             500
HelloFlag     Dim             1
FName         dim             30
GuiFlag       Dim             1                         .has 'T' if text based
;.............................................................................
main
;............................
;Set Vars used for About Box
        move    "Master1.PLS",Wprognme
        move    "Master PLB Menu",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        move    RelDate,Wreldate
;begin patch 3.3
               Move           "MASTER1" to Program
;end patch 3.3


;Declare forms, Always declare child forms first
;rpt     plform  Report
mss1    plform  Error
pss     plform  Passwrd
abt     plform  About
PRog    plform  Master001a
x       plform  Master001
;        winhide
;Load Forms, Always load parent form first
        formload x
        FormLoad              PRog
        formload abt
        formload pss
        formload mss1
;Create Menus
        create  Master001;mFile,FData
        create  Master001;mOptions,OData,mFile
        create  Master001;mHelp,HData,mOptions
        CREATE  Master001;sCOlor,Cdata,mOptions,1
        CREATE  Master001;sSound,Sdata,mOptions,2
        Create  Sapi,class="Sapi.SpVoice"         //Create the ActiveX control

;Activate Menus
;FileGo leads to stop
        activate mFile,FileGo,result
        activate mOptions
        activate mHelp,HelpGo,result
        activate sColor,ColorGo,result
        activate sSound,SoundGo,result
;set properties in collection
        listins ColText,Master001,Master001ListView002,DateText,DayofWeek,DirectirText,DirectirText:
                Master001ListView003,MasterButton001,MasterEditText001,MasterTabControl,PortText:
                PortText001,StaffButton,TimeText,UserText,Master001ListView001
.        listins ColText,Master001,AcctList001,DateText,DayofWeek,DirectirText,DirectirText:
.                ISList001,MasterButton001,MasterEditText001,MasterTabControl,PortText:
.                PortText001,SalesList001,StaffButton,TimeText,UserText,Master001ListView001
        listins Colback,Master001,Master001ListView002,DateText,DayofWeek,DirectirText,DirectirText:
                Master001ListView003,MasterButton001,MasterEditText001,MasterTabControl,PortText:
                PortText001,StaffButton,TimeText,UserText,Master001ListView001
.        listins Colback,Master001,AcctList001,DateText,DayofWeek,DirectirText,DirectirText:
.                ISList001,MasterButton001,MasterEditText001,MasterTabControl,PortText:
.                PortText001,SalesList001,StaffButton,TimeText,UserText

;Create Colors for EditText Inquiry
        create  white=*white
        create  grey=220:220:220
        create  RED=*RED
        create  black=*black

;Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic
        create  font5,"Arial",size=10
;
        move    "S",progcode
        move    "N",PassFlag
                    eventreg x,10,MasterKeyPress,RESULT=N9



        CREATE  TIMER,18000     ..30 minutes
        ACTIVATE TIMER,Timeout,RESULT
;
        CREATE  GetTIME,600     ..1 minute
        ACTIVATE GetTIME,Newtime,RESULT
;
               Activate      Master001Picture001
               setprop       Master001Picture001,visible=1
               CLOCK     DATE TO TODAY
         clock     timestamp to timestamp
         unpack    timestamp into cc           .get century
;         CALL      PAINT
;         keyin     *cl,*t0,str1
;Open Pif FIle
OPenPif
        trap    Piferror if io
        open    Piffile,"c:\progra~1\nincal\Master1.pif"
        goto    PifError if over
        clear   n1
        loop
                add     c1,n1
                read    Piffile,seq;str1,n1
                until   over
                If      (str1 = "S")
                move    n1 to soundflag
                endif
        repeat
        close   Piffile
        trapclr io
afterPif
         call      logo
         move      c0 to timeout
;Open color file
opencolor
        trap    colorerror if io
        open    colorfile,"c:\progra~1\nincal\Master1.col"
        goto    colorerror if over
        clear   n1
        loop
                add     c1,n1
                read    colorfile,seq;colornum(n1)
                until   over
                until   (n1 = 2)
        repeat
        close   colorfile
        trapclr io
        unpack  colornum(1),Fred,Fgreen,Fblue
        create  FTC=Fred:Fgreen:Fblue
        setprop ColText,fgcolor=FTC

        unpack  colornum(2),Fred,Fgreen,Fblue
        create  BGC=Fred:Fgreen:Fblue
        setprop ColBack,bgcolor=BGC
aftercolor

         MOVE       C1 TO NUSEPATH    *ACCESS BY PORTNUMBER
         UNPACK    TODAY INTO MM,STR1,DD,STR1,YY
;DLH 10Feb00   .no reason to do anymore
;         trap      naughty if int
         TRAP      GOOUT IF INT

FINDIT   MOVE      MM TO NMM
         MOVE      DD TO NDD
         CLEAR     FUNC
         CLEAR     TYPINIT
;first time thru? if so say hello
;         if        (portn <= 0)
         if        (portn > 0)                 ;been here done that
              Move            No to Helloflag
;         call      hello
         endif
;later add user preference to suppress
         CALL      FINDPORT
         setitem   UserText,0,nuseUser
               call           Trim using NUSEUSER
               if (NUSEUSER <> "" & HelloFlag <> "N" & Soundflag = 1)
                              scan           B1,NUSEUSER
                              if equal
                                             movefptr NUSEUSER,N9
                                             setlptr NUSEUSER,N9
                                             reset  Nuseuser
                              Move           Nuseuser to FName
                              pack           str500 from "Hello ",Fname," It's good to see you"
//Use the Speak Method of this control
                              Sapi.Speak using str500
                              Move           No to Helloflag
                              endif
                              endif
                              reset          NUSEUSER
         setitem   Porttext,0,Nusefld
;         call      win95
         GOTO      TRAPCHK
;NOFIND
;
;..............................................................................
;
; SEE IF THERE ARE ANY UNTRAPPED DATASHARE ERRORS.
; IF NO ERROR OCCURED, THE VARIABLES 'PORTN' AND 'TODAY' WILL STILL BE IN
; THE FIRST TWELVE BYTES OF UDA.  IN THIS CASE, THE 9TH CHARACTER OF CWK11
; WILL BE A BLANK.
;
; IF AN ERROR OCCURED, THE ELEVEN BYTE ERROR MESSAGE WILL BE MOVED INTO CWK11
; IN THIS CASE THE 9TH CHARACTER OF CWK11 WILL BE A '*'.
;
TRAPCHK  CALL      DAY                  *GET DAY OF WEEK IF SAT OR SUN
;                                       DO NOT OPEN AGENDA FILE(s)
;         MATCH     "tc" TO  TYPINIT
;         GOTO      TCPORT IF EQUAL
         TRAP      IO GIVING ERROR IF IO
         CALL      DAY
         trap      logo1 IF F10
         INCLUDE   LOGIO.inc
         TRAP      IO GIVING ERROR IF IO
         TRAP      GOOUT IF F3
         MOVE      ERROR TO CWK11
;         RESET     CWK11,9            .dlh 22jul93
         scan       "*",CWK11
         GOTO      MASMENU IF NOT EQUAL
         GOTO      FREEZE
IO       TRAPCLR   IO
         NORETURN
         TRAP      IO GIVING ERROR IF IO
;         trap      naughty if STACK       .pcbus
         trap      naughty if DEL
         trap      naughty if RANGE
         trap      naughty if PARITY
         trap      naughty if FORMAT
;         trap      naughty if PRTOFL        .pcbus
         trap      naughty if CFAIL
;DLH 10Feb00
;         trap      naughty if int
         TRAP      GOOUT IF INT
         GOTO      FREEZE
;
; THIS FREEZES THE ERROR ON THE SCEEEN SO THAT A BETTER DETERMINATION CAN
; BE MADE CONCERNING THE CAUSE OF THE PROBLEM.
;
FREEZE
;         trap      naughty if STACK       .pcbus
         trap      naughty if DEL
         trap      naughty if RANGE
         trap      naughty if PARITY
         trap      naughty if FORMAT
;         trap      naughty if PRTOFL       .pcbus
         trap      naughty if CFAIL
;DLH 10Feb00
;         trap      naughty if int
         TRAP      GOOUT IF INT


.begin patch 4.1
.		
.MAILSEND	Out,To,From,Subject,Body
.	
SendMail
	 Clear	 SmtpEmailAddress
          append  user to SmtpEmailAddress
         append  "@nincal.com",SmtpEmailAddress
         reset    smtpemailaddress
	 clear	taskname
	 append	"This is an error message",taskname
	 append	CRLF,taskname
	 append	"This is from subroutine FREEZE",taskname
	 append	CRLF,taskname
	 append	"Incorrect termination this the NIN Master program",taskname
	 append	CRLF,taskname
	 reset  taskname

 	MAILSEND	"NTS4","DavidHerrick@NINCAL.com,DavidBaca@nincal.com",SMtpEmailAddress,"This is a Error e-mail from master":
 		         taskname,*ATTACHMENT="\\nins1\e\data\text\ninorddrew.dat",*ERROR=S$CMDLIN,*TIMEOUT=60

.      Move    "This is a Error e-mail from master",SmtpSubject Subject

.;   Set the text message that is send with the attachments
.
.       Move    "This is an error message",SmtpTextMessage(1)   Array <Text message >
.        Move    error,SmtpTextMessage(2)   Array <Text message >
.        Move    "This is from subroutine FREEZE",SmtpTextMessage(3)   Array <Text message >
.        Move    "Incorrect termination this the NIN Master program",SmtpTextMessage(4)   Array <Text message >
.;        Move    "This is the text message line 4",SmtpTextMessage(4)   Array <Text message >
.;        Move    "This is the text message line 5",SmtpTextMessage(5)   Array <Text message >
.
.        Move    "4",SmtpTextIndexLast                               Index to last entry in TextMessage array
.
.         call      errmesg
.end patch 4.1
;....the darn error dialog box         use
;         DISPLAY   *B,*ES,*blinkon,*red:
;                   *P15:10,"00000  00000  00000  00000  00000":
;                   *P15:11,"0      0   0  0   0  0   0  0   0":
;                   *P15:12,"0000   00000  00000  0   0  00000":
;                   *P15:13,"0      0  0   0  0   0   0  0  0 ":
;                   *P15:14,"00000  0   0  0   0  00000  0   0"
ERROR
;    KEYIN     *P15:16,"ERROR MESSAGE: ",*DV,CWK11:
;                   *P15:17,*EL,*DV,ERROR,*P47:16,*eoff,ANS,*eon
;.         CALL      LOGWRITE
;         CMATCH    "e",ANS
;         GOTO      ERROR IF EOS
;         GOTO      ERROR IF NOT EQUAL
;         move      b1 to error
         trapCLR  INS
         TRAPCLR   F6
         TRAPCLR   F3
         TRAPCLR   F8
;DLH 10Feb00
;         trap      naughty if int
         TRAP      GOOUT IF INT
         move      b1,CWK11
         move      b1 to onsw                   .reset so we get full menu display not just time. 07May98 DLH
         GOTO      MASMENU
naughty
         trapCLR  DEL
         trapCLR  RANGE
         trapCLR  PARITY
         trapCLR  FORMAT
         trapCLR  CFAIL
         trapCLR  INT
         trap      naughty if DEL
         trap      naughty if RANGE
         trap      naughty if PARITY
         trap      naughty if FORMAT
         trap      naughty if CFAIL
;DLH 10Feb00
;         trap      naughty if int
         TRAP      GOOUT IF INT
         move      "Naughty" to error
.begin patch 4.1
	 Clear	 SmtpEmailAddress
         append  user to SmtpEmailAddress
         append  "@nincal.com",SmtpEmailAddress
         reset    smtpemailaddress
	 clear	taskname
	 append	"This is an error message",taskname
	 append	CRLF,taskname
	 append	"Naughty ",taskname
	 append	CRLF,taskname
	 append	"Naughty ",taskname
	 append	CRLF,taskname
	 append	"Incorrect termination this the NIN Master program",taskname
	 append	CRLF,taskname
	 reset  taskname

 	MAILSEND	"NTS4","InformationServices@NINCAL.com",SMtpEmailAddress,"This is a Error e-mail from master":
 		         taskname,*ERROR=S$CMDLIN,*TIMEOUT=5




.      Move    "This is a Error e-mail from master",SmtpSubject Subject
.
.;   Set the text message that is send with the attachments
.
.       Move    "This is an error message",SmtpTextMessage(1)   Array <Text message >
.        Move    "Naughty ",SmtpTextMessage(2)   Array <Text message >
.        Move    "Naughty ",SmtpTextMessage(3)   Array <Text message >
.        Move    "Incorrect termination this the NIN Master program",SmtpTextMessage(4)   Array <Text message >
.;        Move    "This is the text message line 4",SmtpTextMessage(4)   Array <Text message >
.;        Move    "This is the text message line 5",SmtpTextMessage(5)   Array <Text message >
.
.        Move    "4",SmtpTextIndexLast                               Index to last entry in TextMessage array
.         call      errmesg
.end patch 4.1
;ditto use error dialogue
         if        (soundflag = 1)
         SNDOPEN   sfile,"\\nts0\c\Netutils\media\thunder.WAV"
         SNDPLAY   sfile
        SNDCLOSE   sfile
        endif
;          execute  "\\nts0\c\public\send #"I was Naughty#" to info_services >>nul"
;          execute   "c:\progra~1\plus!\micros~1\iexplore.exe http://nts2"
          execute   "c:\progra~1\plus!\micros~1\iexplore.exe http://Web01/"
          if         over                .Failed
;          execute   "c:\progra~1\Intern~1\iexplore.exe http://nts2"
          execute   "c:\progra~1\Intern~1\iexplore.exe http://Web01/"
          endif
         if        (soundflag = 1)
         SNDOPEN   sfile,"\\nts0\c\Netutils\media\thunder.WAV"
         SNDPLAY   sfile
         SNDCLOSE   sfile
         endif
          goto     freeze
;
*
;MASMENU  CLOCK     PORT TO CWK2
;         CONSOLE   "MASTER",CWK2,"                 "
MASMENU
;
         ADD       C1 TO TIMEOUT
         COMPARE   "15" TO TIMEOUT
         goto      goout IF EQUAL
         CLOCK     DATE TO TODAY


         UNPACK    TODAY INTO MM,STR1,DD,STR1,YY
         setitem   Datetext,0,today
         MOVE      MM TO NMM
         MOVE      DD TO NDD
         PACK      PASSMAST FROM DD,MM
         REP       ZFILL IN PASSMAST
;
         call      newtime


APPDAY   CLEAR     TODAY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         REP       ZFILL IN TODAY
;         GOTO      DAY
         CALL      DAY
         GOTO      MENU2
;
CLOCKERR NORETURN
         GOTO      APPDAY
;
DAY      CLOCK     YEAR TO DIM2
         pack      str4 from cc,dim2

         MOVE      str4 TO YEARWORK
         MOVE      str4 TO YEAR
         CLOCK     DAY TO DIM3
         MOVE      DIM3 TO JDAYWORK
;begin patch 3.1
         clock     weekday to str2
         move      str2 to DOW
;         MOVE      NWORK2 TO DOW
;         CALL      FINDOW
;end patch 3.1
;
         BRANCH    DOW OF SUN,MON,TUE,WED,THU,FRI,SAT
         COMPARE   "0",DOW
         GOTO      CNVTIME IF NOT EQUAL
         MOVE      "??????" TO DAY
DAYEXIT  setitem   DayOfWeek,0,day
         RETURN

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
;
MENU2
;                                           *DON'T WRITE TO LOGFILE
         MOVE      "PROGRAM",LOGTYPE
         MOVE      "MASTER",LOGINFO
         CALL      LOGWRITE
         GOTO      PAGE1
PAGE1
;SALESLIST001
              Master001ListView001.InsertColumn using " ##",30,1
              Master001ListView001.InsertColumn using "Program Description",250,2
              Master001ListView001.InsertColumn using "Program Name",0,3
              Master001ListView001.InsertColumn using "Text/Gui",0,4         .has a 'T' if text
               Master001ListView001.SetColumnFormat using 0,c1              .set column justify right
              Setprop Master001ListView001,Sortorder=1
         If    (security = 8)
         Master001ListView001.InsertItem giving IN9 using "1"      
         Master001ListView001.SetItemText using IN9,"...Shipping Status",1
         Master001ListView001.SetItemText using IN9,"NSHP0001",2
         Master001ListView001.InsertItem giving IN9 using "2"
         Master001ListView001.SetItemText using IN9,"...Datacard (List)Inquiry",1
         Master001ListView001.SetItemText using IN9,"NDAT0001",2

.         insertitem SalesList001,9999," 1...Shipping Status"
.         insertitem SalesList001,9999," 2...Datacard (List)Inquiry"
         else
         Master001ListView001.InsertItem giving IN9 using "01"
         Master001ListView001.SetItemText using IN9,"...Order ENTRY & INQUIRY",1      
         Master001ListView001.SetItemText using IN9,"NORDTEST",2
         Master001ListView001.InsertItem giving IN9 using "03"
         Master001ListView001.SetItemText using IN9,"...Fulfillment Maintenance Program",1      
         Master001ListView001.SetItemText using IN9,"NFUL0001",2
         Master001ListView001.InsertItem giving IN9 using "04"
         Master001ListView001.SetItemText using IN9,"...Datacard ENTRY & INQUIRY",1      
         Master001ListView001.SetItemText using IN9,"NDAT0001",2
         Master001ListView001.InsertItem giving IN9 using "05"
         Master001ListView001.SetItemText using IN9,"...Shipping ENTRY & INQUIRY",1
         Master001ListView001.SetItemText using IN9,"NSHP0001",2
         Master001ListView001.InsertItem giving IN9 using "07"
         Master001ListView001.SetItemText using IN9,"...Mailer/Offer Entry/Inquiry",1
         Master001ListView001.SetItemText using IN9,"COMP0001",2
         Master001ListView001.InsertItem giving IN9 using "09"
         Master001ListView001.SetItemText using IN9,"...Owner ENTRY & INQUIRY",1
         Master001ListView001.SetItemText using IN9,"NOwn0001",2
         Master001ListView001.InsertItem giving IN9 using "10"
         Master001ListView001.SetItemText using IN9,"...Booking Instructions",1
         Master001ListView001.SetItemText using IN9,"NMDL0001",2
         Master001ListView001.InsertItem giving IN9 using "11"
         Master001ListView001.SetItemText using IN9,"...List (Datacard) Reports",1
         Master001ListView001.SetItemText using IN9,"NDAT0004",2
         Master001ListView001.SetItemText using IN9,"T",3             .not GUI
         Master001ListView001.InsertItem giving IN9 using "12"
         Master001ListView001.SetItemText using IN9,"...Order Merge Inquiry",1
         Master001ListView001.SetItemText using IN9,"NMRG0001",2
         Master001ListView001.InsertItem giving IN9 using "13"
         Master001ListView001.SetItemText using IN9,"...Bill-to Maintenance",1
         Master001ListView001.SetItemText using IN9,"NBil0001",2
         Master001ListView001.InsertItem giving IN9 using "14"
         Master001ListView001.SetItemText using IN9,"...Pay-to Maintenance",1
         Master001ListView001.SetItemText using IN9,"NPay0001",2
         Master001ListView001.InsertItem giving IN9 using "15"
         Master001ListView001.SetItemText using IN9,"...Return-to Maintenance",1
         Master001ListView001.SetItemText using IN9,"NRtn0001",2
         Master001ListView001.InsertItem giving IN9 using "16"
         Master001ListView001.SetItemText using IN9,"...ORDER HISTORY Reports",1
         Master001ListView001.SetItemText using IN9,"NOrd0006",2
         Master001ListView001.SetItemText using IN9,"T",3             .not GUI
         Master001ListView001.InsertItem giving IN9 using "20"
         Master001ListView001.SetItemText using IN9,"...Exchange History",1
         Master001ListView001.SetItemText using IN9,"Nxch0001",2
.
         Master001ListView001.InsertItem giving IN9 using "21"
         Master001ListView001.SetItemText using IN9,"...Test O the Day",1
         Master001ListView001.SetItemText using IN9,"testoday",2
.
         Master001ListView001.InsertItem giving IN9 using "24"
         Master001ListView001.SetItemText using IN9,"...Marketing List Maintenance",1
         Master001ListView001.SetItemText using IN9,"Mrkt0001",2

         Master001ListView001.InsertItem giving IN9 using "25"
         Master001ListView001.SetItemText using IN9,"...Corrections & Cancellations",1
         Master001ListView001.SetItemText using IN9,"NCORR0001",2
         Master001ListView001.SetItemText using IN9,"T",3
         Master001ListView001.InsertItem giving IN9 using "26"
         Master001ListView001.SetItemText using IN9,"...Maildate schedules",1
         Master001ListView001.SetItemText using IN9,"T",3
         Master001ListView001.SetItemText using IN9,"NSCH0001",2
         Master001ListView001.InsertItem giving IN9 using "27"
         Master001ListView001.SetItemText using IN9,"...Broker Call Program",1
         Master001ListView001.SetItemText using IN9,"Ncal0001",2
         Master001ListView001.InsertItem giving IN9 using "28"
         Master001ListView001.SetItemText using IN9,"...Mailer/Mailer Notes Program",1
         Master001ListView001.SetItemText using IN9,"NMLRXY0001",2
         Master001ListView001.SetItemText using IN9,"T",3
         Master001ListView001.InsertItem giving IN9 using "30"
         Master001ListView001.SetItemText using IN9,"...List History Analysis",1
         Master001ListView001.SetItemText using IN9,"Listhist",2
         Master001ListView001.SetItemText using IN9,"T",3
         Master001ListView001.InsertItem giving IN9 using "31"
         Master001ListView001.SetItemText using IN9,"...Fax Promotion",1
         Master001ListView001.SetItemText using IN9,"NPRM0001",2
         Master001ListView001.InsertItem giving IN9 using "33"
         Master001ListView001.SetItemText using IN9,"...Client Credit History",1
         Master001ListView001.SetItemText using IN9,"credit0001",2
         Master001ListView001.InsertItem giving IN9 using "34"
         Master001ListView001.SetItemText using IN9,"...Broker/Consultant/Mailer/Offer",1
         Master001ListView001.SetItemText using IN9,"Comp0001",2
         Master001ListView001.InsertItem giving IN9 using "44"
         Master001ListView001.SetItemText using IN9,"...Request For Counts",1
         Master001ListView001.SetItemText using IN9,"NRco0001",2
         Master001ListView001.InsertItem giving IN9 using "45"
         Master001ListView001.SetItemText using IN9,"...Suppression reports",1
         Master001ListView001.SetItemText using IN9,"NSPR0001",2
         Master001ListView001.InsertItem giving IN9 using "38"
         Master001ListView001.SetItemText using IN9,"...Telecomm maint",1
         Master001ListView001.SetItemText using IN9,"Telemast",2
         Master001ListView001.InsertItem giving IN9 using "40"
         Master001ListView001.SetItemText using IN9,"...List Management Order reports",1
         Master001ListView001.SetItemText using IN9,"Nord0037",2
         Master001ListView001.InsertItem giving IN9 using "47"
         Master001ListView001.SetItemText using IN9,"...Sample Scanner Program",1
         Master001ListView001.SetItemText using IN9,"Nsmp0001",2
         Master001ListView001.InsertItem giving IN9 using "48"
         Master001ListView001.SetItemText using IN9,"...New Order Inquiry Program",1
         Master001ListView001.SetItemText using IN9,"Nord8001",2
         Master001ListView001.InsertItem giving IN9 using "49"
         Master001ListView001.SetItemText using IN9,"...Website Tracking Program",1
         Master001ListView001.SetItemText using IN9,"NWeb0001",2
         Master001ListView001.InsertItem giving IN9 using "50"
         Master001ListView001.SetItemText using IN9,"...List Owner Income Reports",1      
         Master001ListView001.SetItemText using IN9,"NLoinc0007",2
.Accounting Menu
              Master001ListView002.InsertColumn using " ##",30,1
              Master001ListView002.InsertColumn using "Program Description",250,2
              Master001ListView002.InsertColumn using "Program Name",0,3
              Master001ListView002.InsertColumn using "Text/Gui",0,4         .has a 'T' if text
               Master001ListView002.SetColumnFormat using 0,1              .set column justify right
              Setprop Master001ListView002,Sortorder=1
         Master001ListView002.InsertItem giving IN9 using "02"
         Master001ListView002.SetItemText using IN9,"...Invoice Entry & Inquiry",1
         Master001ListView002.SetItemText using IN9,"NInv0001",2
         Master001ListView002.InsertItem giving IN9 using "46"
         Master001ListView002.SetItemText using IN9,"...List Income and Order Variance",1
         Master001ListView002.SetItemText using IN9,"NVar0001",2
.
         Master001ListView002.InsertItem giving IN9 using "21"
         Master001ListView002.SetItemText using IN9,"...Test O the Day",1
         Master001ListView002.SetItemText using IN9,"testoday",2
.
         Master001ListView002.InsertItem giving IN9 using "03"
         Master001ListView002.SetItemText using IN9,"...Fulfillment Maintenance Program",1      
         Master001ListView002.SetItemText using IN9,"NFUL0001",2
         Master001ListView002.InsertItem giving IN9 using "06"
         Master001ListView002.SetItemText using IN9,"...Cash Receipts & Invoice Adjustments",1      
         Master001ListView002.SetItemText using IN9,"Ncsh0001",2
         Master001ListView002.InsertItem giving IN9 using "19"
         Master001ListView002.SetItemText using IN9,"...Running Charges",1      
         Master001ListView002.SetItemText using IN9,"NINV0008",2
         Master001ListView002.SetItemText using IN9,"T",3
         Master001ListView002.InsertItem giving IN9 using "22"
         Master001ListView002.SetItemText using IN9,"...Invoice LO # Mod",1      
         Master001ListView002.SetItemText using IN9,"NINV0023",2
         Master001ListView002.SetItemText using IN9,"T",3
         Master001ListView002.InsertItem giving IN9 using "23"
         Master001ListView002.SetItemText using IN9,"...Mark Order Billed",1      
         Master001ListView002.SetItemText using IN9,"NINV0007",2
         Master001ListView002.InsertItem giving IN9 using "35"
         Master001ListView002.SetItemText using IN9,"...Check Information Maint",1      
         Master001ListView002.SetItemText using IN9,"NCHK0005",2
         Master001ListView002.SetItemText using IN9,"T",3
         Master001ListView002.InsertItem giving IN9 using "36"
         Master001ListView002.SetItemText using IN9,"...List Owner Payables",1      
         Master001ListView002.SetItemText using IN9,"Ninv0013",2
         Master001ListView002.SetItemText using IN9,"T",3
         Master001ListView002.InsertItem giving IN9 using "37"
         Master001ListView002.SetItemText using IN9,"...Mailer Payment Inquiry",1      
         Master001ListView002.SetItemText using IN9,"Ninv0017",2
         Master001ListView002.SetItemText using IN9,"T",3
         Master001ListView002.InsertItem giving IN9 using "39"
         Master001ListView002.SetItemText using IN9,"...Donnelly (Triplex) Billing",1      
         Master001ListView002.SetItemText using IN9,"Ndat0022",2
         Master001ListView002.SetItemText using IN9,"T",3
         Master001ListView002.InsertItem giving IN9 using "42"
         Master001ListView002.SetItemText using IN9,"...Money on Account",1      
         Master001ListView002.SetItemText using IN9,"Nona0001",2
         Master001ListView002.InsertItem giving IN9 using "43"
         Master001ListView002.SetItemText using IN9,"...Amount Due",1      
         Master001ListView002.SetItemText using IN9,"Ninv0010",2
         Master001ListView002.InsertItem giving IN9 using "33"
         Master001ListView002.SetItemText using IN9,"...Credit History",1      
         Master001ListView002.SetItemText using IN9,"credit0001",2
         Master001ListView002.InsertItem giving IN9 using "50"
         Master001ListView002.SetItemText using IN9,"...List Owner Income Reports",1      
         Master001ListView002.SetItemText using IN9,"NLoinc0007",2
;IS MEnu
              Master001ListView003.InsertColumn using " ##",30,1
              Master001ListView003.InsertColumn using "Program Description",250,2
              Master001ListView003.InsertColumn using "Program Name",0,3
              Master001ListView003.InsertColumn using "Text/Gui",0,4         .has a 'T' if text
               Master001ListView003.SetColumnFormat using 0,1              .set column justify right
              Setprop Master001ListView003,Sortorder=1
.
         Master001ListView003.InsertItem giving IN9 using "21"
         Master001ListView003.SetItemText using IN9,"...Test O the Day",1
         Master001ListView003.SetItemText using IN9,"testoday",2
.
         Master001ListView003.InsertItem giving IN9 using "70"
         Master001ListView003.SetItemText using IN9,"...OWNER PICK FOR TRANSMISSION",1
         Master001ListView003.SetItemText using IN9,"Nown0003",2
         Master001ListView003.InsertItem giving IN9 using "71"
         Master001ListView003.SetItemText using IN9,"...DataPICK Merge",1
         Master001ListView003.SetItemText using IN9,"NDAT0006",2
         Master001ListView003.InsertItem giving IN9 using "73"
         Master001ListView003.SetItemText using IN9,"...DataCard Print",1
         Master001ListView003.SetItemText using IN9,"NDAT0002",2
         Master001ListView003.InsertItem giving IN9 using "74"
         Master001ListView003.SetItemText using IN9,"...DataCard Listing (RH)",1
         Master001ListView003.SetItemText using IN9,"NDAT0005",2
         Master001ListView003.InsertItem giving IN9 using "75"
         Master001ListView003.SetItemText using IN9,"...RECORD DELETION PROGRAM",1
         Master001ListView003.SetItemText using IN9,"DELREC",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "76"
         Master001ListView003.SetItemText using IN9,"...PLB File Dump PRogram",1
         Master001ListView003.SetItemText using IN9,"DSlist1",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "78"
         Master001ListView003.SetItemText using IN9,"...FIXBUSY -ORDERS/INVOICES/ADJUSTMENTS",1
         Master001ListView003.SetItemText using IN9,"Nord0021",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "79"
         Master001ListView003.SetItemText using IN9,"...FIXCARD - REINSTATE DATACARDS",1
         Master001ListView003.SetItemText using IN9,"Ndat0009",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "80"
         Master001ListView003.SetItemText using IN9,"...FIXCHNG - REINSTATE EXCHANGES",1
         Master001ListView003.SetItemText using IN9,"Nxch0004",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "81"
         Master001ListView003.SetItemText using IN9,"...FIXINV - REPAIR INVOICE PROG.",1
         Master001ListView003.SetItemText using IN9,"Ninv0023",2
         Master001ListView003.InsertItem giving IN9 using "84"
         Master001ListView003.SetItemText using IN9,"...FIXXNUM - EXCHANGE MAINT. PROGRAM",1
         Master001ListView003.SetItemText using IN9,"Nxch0005",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "86"
         Master001ListView003.SetItemText using IN9,"...MASTER MAILER PRint",1
         Master001ListView003.SetItemText using IN9,"Nmlr0002",2
         Master001ListView003.InsertItem giving IN9 using "87"
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.SetItemText using IN9,"...MINI MAILER PRint",1
         Master001ListView003.SetItemText using IN9,"Listnin1",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "88"
         Master001ListView003.SetItemText using IN9,"...MARK - ORDERS/INVOICES",1
         Master001ListView003.SetItemText using IN9,"Mark",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "89"
         Master001ListView003.SetItemText using IN9,"...ORDERPRT - ORDER STATISTICS PROGRAM",1
         Master001ListView003.SetItemText using IN9,"Nord0010",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "90"
         Master001ListView003.SetItemText using IN9,"...OWNNEW - DATACARD OWNER INFO.",1
         Master001ListView003.SetItemText using IN9,"OwnNew",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "91"
         Master001ListView003.SetItemText using IN9,"...PASSMOD - PASSWORD MODIFICATION",1
         Master001ListView003.SetItemText using IN9,"Npas0001",2
         Master001ListView003.InsertItem giving IN9 using "92"
         Master001ListView003.SetItemText using IN9,"...SPIMOD - SPECIAL INSTRUCTION MOD.",1
         Master001ListView003.SetItemText using IN9,"Nspi0001",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "93"
         Master001ListView003.SetItemText using IN9,"...STARTER - START JOBS",1
         Master001ListView003.SetItemText using IN9,"starter",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "94"
         Master001ListView003.SetItemText using IN9,"...SUPERPIC - SUPER DATACARD PICK",1
         Master001ListView003.SetItemText using IN9,"Ndat0017",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "95"
         Master001ListView003.SetItemText using IN9,"...Program - Database",1
         Master001ListView003.SetItemText using IN9,"NPRG0001",2
         Master001ListView003.SetItemText using IN9,"T",3
         Master001ListView003.InsertItem giving IN9 using "99"
         Master001ListView003.SetItemText using IN9,"...Name your Program",1
         Master001ListView003.SetItemText using IN9,"",2
         endif

.         setprop SalesList001,enabled=1,bgcolor=white
.         setprop ISList001,enabled=1,bgcolor=white
;
.         setprop acctList001,enabled=1,bgcolor=white
.         setprop   Saleslist001,visible=0
              SETPROP Master001ListView001,ENABLED=1,BGCOLOR=WHITE
              SETPROP MASTER001LISTVIEW001,VISIBLE=1
              SETPROP Master001ListView002,ENABLED=1,BGCOLOR=WHITE
              SETPROP MASTER001LISTVIEW002,VISIBLE=0
              SETPROP Master001ListView003,ENABLED=1,BGCOLOR=WHITE
              SETPROP MASTER001LISTVIEW003,VISIBLE=0
.              SETFOCUS MASTER001LISTView001                       
.         setprop   acctlist001,visible=0
.         setprop   ISlist001,visible=0
.         setfocus  SalesList001,1
         setfocus  Masteredittext001,1

MainLoop
        loop
                winhide
                waitevent
;                setitem timer,0,18000   .reset to 30 minutes
        repeat

        goto    mainloop
Timeout
        beep
        beep
        beep
        stop
Newtime
         CLOCK     TIME TO TIME
         UNPACK    TIME INTO STR2,ANS,MIN,ANS,SEC
         MOVE      TIME,STR2
;
         MOVE      STR2,HOUR
;         MOVE      HR TO HOUR
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
         setitem   TimeText,0,timedisp
         return
*................................................................
; GET THE PROGRAM'S INDEX
;
;...............................................................
; ' TO THE ROUTINE INDICATED BY THE INDEX
;
GETPROG        TRAP      NOTFOUND IF CFAIL
               MOVE      "PROGRAM",LOGTYPE

               If             (passflag = "Y" & INdex = 99)
               Setprop        MasterProgramSelect,visible=1
               Setprop        Master001aEdit,visible=1
               setfocus       Master001aEdit
               Setprop        Master001aEdit,visible=1
               getitem        Master001aEdit,0,programname
               call           Trim using ProgramName
               MOVE           ProgramName,LOGINFO
               CALL           LOGWRITE
               GetItem        Master001aRadio001,0,result
                              if             (result = c0)           .not Gui
                              winshow
                              Endif
               CHAIN          ProgramName
               endif
.
.;         getitem   Masteredittext001,0,index
;
               if             (programname = "")
               call           getdetails
               endif
               call           Trim using ProgramName
               MOVE           ProgramName,LOGINFO
               CALL           LOGWRITE
                              IF             (Guiflag = "T")
                              winshow
                              endif
               Chain           PRogramname
               winshow
               return              
*................................................................
;
NOTFOUND NORETURN
;         DISPLAY   *B,*P45:24,*HON:
;                   *B,*P45:24,"Sorry, this program is not on-line.",*W2,*HOFF;
         MOVE      "CHAIN ATTEMPT FAILURE",LOGINFO
         CALL      LOGWRITE
//Use the Speak Method of this control
         if        (soundflag = 1)
               pack           str500 from "Sorry, ",Fname," That program is not on-line"
               Sapi.Speak using Str500
         endif
         alert   caution,"Sorry, That program is not on-line.",result
         setfocus  Masteredittext001,1
         goto    mainloop

;NOTPC    DISPLAY   *P1:24,*EL,*B,*HON,"THIS IS NOT THE PC!!!!!",*B,*W3,*HOFF;
;         GOTO      NoSuch
GOOUT    MOVE      "SHUTDOWN TO OS",LOGINFO
         CALL      LOGWRITE
;         DISPLAY   *BORDER BLACK
         if        (soundflag = 1)
;         SNDOPEN   sfile,"\\nts0\c\Netutils\media\bye.wav"
;         SNDPLAY   sfile
;        SNDCLOSE   sfile
//Use the Speak Method of this control
               pack           str500 from "Good Bye ",Fname
               Sapi.Speak using Str500
         endif
;begin patch 3.3
;               IF             (SECURITY = 8)
;               SHUTDOWN
;               ENDIF
;enD patch 3.3
              STOP
MasterTabchange
         move      N2,TabNum            .from form TAB Change event
         if        (TABNUM = 1)
         setprop   Master001ListView001,visible=1
         setfocus  Master001ListView001,1
.         setprop   Saleslist001,visible=1
.         setfocus  SalesList001,1
         endif
         if        (TABNUM = 2)
         setprop   Master001ListView002,visible=1
         setfocus  Master001ListView002,1
.         setprop   acctlist001,visible=1
.         setfocus  AcctList001,1
         endif
         if        (TABNUM = 3)
;Verify Password if necessary
                if (PassFlag = "N")
                        setitem PasswordEdit,0,""
                        setfocus PasswordEdit
                        setprop Passwrd,visible=1
                endif
                if (PassFlag = "N")
;//Use the Speak Method of this control
                        pack           str500 from "Invalid Password ",Fname
               if        (soundflag = 1)                        
                                      Sapi.Speak using Str500
                        endif
                        move       savetab to tabnum
                        move       savetab to n2
                        setitem    MasterTabControl,0,n2
                        goto       MasterTabChange
                endif
         Setprop Master001ListView003,visible=1
.         setprop   ISlist001,visible=1
         setfocus  master001Listview003,1
.         setfocus  ISList001,1
         endif
         return
MasterTabClick
         move      N2,TabNum            .from form TAB  Click event
         move      tabnum to savetab
         if        (TABNUM = 1)
              Setprop Master001ListView001,visible=0
.         setprop   Saleslist001,visible=0
         endif
         if        (TABNUM = 2)
              Setprop Master001ListView002,visible=0
.         setprop   acctlist001,visible=0
         endif
         if        (TABNUM = 3)
         Setprop Master001ListView003,visible=0
.         setprop   ISlist001,visible=0
;if leaving IS menu and not an IS machine turn off password ok
                 if        (Portnum = "015" or portnum = "112" or portnum = "119" or portnum = "250")
;               .leave status alone
                 else
                 move      No to PassFlag
                 endif
         endif
         return
NOsuch
         alert   caution,"Failed to load program",result
//Use the Speak Method of this control
               pack           str500 from "Sorry, ",Fname," Could not load that program"
               if        (soundflag = 1)
                             Sapi.Speak using Str500
               endif
              setfocus  Masteredittext001,1               
               goto    mainloop
;..................................................
getdetails
              Master001ListView001.getitemcount giving n3
              move            c0 to result
              Loop
              Master001ListView001.GetItemText giving str2 using result,0
              Move            str2 to N2
                              if              (N2 = index)   .hit
                              Master001ListView001.GetItemText giving programname using result,2
                              Master001ListView001.GetItemText giving GuiFlag using result,3
                              Break
                              return
                              endif
              add             c1 to result                              
              until (result > n3)
              repeat
              if             (programname = "")       .shoot did not find it yet
              Master001ListView002.getitemcount giving n3
              move            c0 to result
              Loop
              Master001ListView002.GetItemText giving str2 using result,0
              Move            str2 to N2
                              if              (N2 = index)   .hit
                              Master001ListView002.GetItemText giving programname using result,2
                              Master001ListView002.GetItemText giving GuiFlag using result,3
                              Break
                              return
                              endif
              add             c1 to result                              
              until (result > n3)
              repeat
              else
              return
              endif
              Master001ListView003.getitemcount giving n3
              move            c0 to result
              Loop
              Master001ListView003.GetItemText giving str2 using result,0
              Move            str2 to N2
                              if              (N2 = index)   .hit
                              Master001ListView003.GetItemText giving programname using result,2
                              Master001ListView003.GetItemText giving GuiFlag using result,3
                              Break
                              return
                              endif
              add             c1 to result                              
              until (result > n3)
              repeat
              if             (programname = "")       .shoot did not find it yet
              call            notfound
              endif
              return
;..................................................
logo     if        (portn <= 0)
logo1    trapclr   F10
         trap      logo1 if F10
;         create    LOGO=20:24:65:85,"\\nts0\c\netutils\dma.gif"
;         activate  Logo
;         pause     "2"
;         deactivate  Logo
         create    LOGO=04:18:12:85,"\\nts0\c\netutils\ninca.gif",border
         activate  Logo
         If         (SoundFlag = 1)
         SNDOPEN   sfile,"\\nts0\c\Netutils\media\tada.wav"
         SNDPLAY   sfile
         SNDCLOSE   sfile
         endif
         deactivate  Logo
         destroy    logo
;             ACTIVATE          LogoPict
         endif
         return
;......................................................................................................
FileGo
               GOTO           gOoUT
               winshow
                stop
        return

Piferror
        noreturn
        move    C1,Soundflag
        goto   AfterPif
;................................................................................
colorerror
        noreturn
        move    C1,colorflag
        goto aftercolor
SoundGo
        if (result = C1)           .default sound on
              move          c1 to soundflag
        elseif (result = C2)
              move          c2 to soundflag
        else
                return
        endif
        clear   n1
        trap    PifTrap if io
        open    Piffile,"c:\progra~1\nincal\master1.Pif"
        write   Piffile,seq;"S",soundflag
        weof    PifFile,seqeof
        close   Piffile
        return
PifTrap
        NoReturn
        trapclr io
        Prep    PifFile,"c:\progra~1\nincal\master1.Pif"
        write   PifFile,seq;"S",SoundFlag
        weof    PifFile,seqeof
        close   PifFile
        return
;........................................................
ColorGo
        if (result = C1)
                call    BackColor
        elseif (result = C2)
                call    TextColor
        else
                return
        endif
        clear   n1
        prep    colorfile,"c:\progra~1\nincal\master1.col"
        loop
                add     c1,n1
                write   colorfile,seq;colornum(n1)
                until (n1 =2)
        repeat
        close   colorfile
        return
;Trap for Cancel Entry in Color System Menu
ColorTrap
        noreturn
        return
BackColor
        trap    ColorTrap if object
        create  BGC
        trapclr object
        setprop ColBack,bgcolor=BGC
        getitem BGC,1,Fred
        getitem BGC,2,Fgreen
        getitem BGC,3,Fblue
        pack    colornum(2),Fred,Fgreen,Fblue
        return

TextColor
        trap    ColorTrap if object
        create  FTC
        trapclr object
        setprop ColText,fgcolor=FTC
        getitem FTC,1,Fred
        getitem FTC,2,Fgreen
        getitem FTC,3,Fblue
        pack    colornum(1),Fred,Fgreen,Fblue
        return
HelpGo
        setprop AboutMssg,visible=1
        return
;begin patch 3.3
MasterKeypress
          if (N9 = 121)       .F10 Key calls password routine
          call Getpass        
          Elseif (N9 = 114)       .F3 Exit
	Goto Sendmail
          goto	goout
          Elseif (N9 = 115)       .F4 Extension listing
          call Click_StaffButton
          Elseif (N9 = 112)       .F1 Tab change
              setfocus        MasterTabControl,1
              call          Change_MasterTabControl
          Elseif (N9 = 119)       .F8 xmas truue
          winshow
          chain               "xmastree"
          Elseif (N9 = 116)       .F5 xmas truue
          winshow
          chain               "gamemenu"
              Elseif (N9 = 117)       .F6 web
              call            Click_Website
         
              Elseif (N9 = 118)       .F7 intranet  
              call            Click_Intranet
              endif
          return
GetPAss          
                        setitem PasswordEdit,0,""
                        setfocus PasswordEdit
                        setprop Passwrd,visible=1
                if (PassFlag = "N")
;//Use the Speak Method of this control
                        pack           str500 from "Invalid Password ",Fname
               if        (soundflag = 1)                        
                                      Sapi.Speak using Str500
                        endif
                        move       savetab to tabnum
                        move       savetab to n2
                        setitem    MasterTabControl,0,n2
                        goto       MasterTabChange
                endif
                return
.
.                 move    "v",progcode
.                 move    "N",PassFlag
.                 setitem PasswordEdit,0,""
.                 setfocus PasswordEdit
.                 setprop Passwrd,visible=1
.               If             (Passflag = "Y")
.               move               c0 to security
.               else
.               move               c8 to security
.;//Use the Speak Method of this control
.               pack           str500 from "Invalid Password ",Fname
.                       if        (soundflag = 1)                           
.                                             Sapi.Speak using Str500
.                 endif
.               endif
.               return
;end patch 3.3

         INCLUDE   PORTCALC.inc
;begin patch
;         INCLUDE   FINDOW.inc
;end patch
         INCLUDE   MESSAGE.inc
         INCLUDE   NUSEIO.inc
;         include   ckpatch.inc
         include   npasio.inc
         INCLUDE   COMLOGIC.inc

