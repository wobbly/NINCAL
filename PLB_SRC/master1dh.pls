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
;note the *blinkxx display & keyin statements do  not work under windows (9/28/98)
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
;
Sapi   Automation                               //Define the ActiveX control
;Voice   Automation                               //Define the ActiveX control
;Voices   Automation                               //Define the ActiveX control
;............................................................................
;
;
release    init      "3.7"       DMB Added NVAR0001
RelDate        Init           "15 October 2005"
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
;LogoPict       Pict
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
ProgramName    Dim            250
str500        Dim             500
Fname         Dim             30
HelloFlag     dim             1
;.............................................................................
main
;............................
;Set Vars used for About Box
        move    "Master1.PLS",Wprognme
        move    "Master PLB Men",Wfunction
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
        listins ColText,Master001,AcctList001,DateText,DayofWeek,DirectirText,DirectirText:
                ISList001,MasterButton001,MasterEditText001,MasterTabControl,PortText:
                PortText001,SalesList001,StaffButton,TimeText,UserText
        listins Colback,Master001,AcctList001,DateText,DayofWeek,DirectirText,DirectirText:
                ISList001,MasterButton001,MasterEditText001,MasterTabControl,PortText:
                PortText001,SalesList001,StaffButton,TimeText,UserText

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

;         create    LOGO=04:18:12:85,"\\nts0\c\netutils\ninca.gif",border
;               CREATE          LogoPict=70:90:390:430,"\\nts0\c\netutils\Logo color with fax and tag.jpg",border
;             ACTIVATE          LogoPict


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
         if        (portn <= 0)
;         call      hello
         endif
;later add user preference to suppress
         CALL      FINDPORT
         setitem   UserText,0,nuseUser
               call           Trim using NUSEUSER
               if (NUSEUSER <> "" & Helloflag <> "N")
                              scan           B1,NUSEUSER
                              if equal
                                             movefptr NUSEUSER,N9
                                             setlptr NUSEUSER,N9
                                             reset  Nuseuser
                              Move           Nuseuser to Fname                                             
                              pack           str500 from "Hello ",Fname," It's good to see you"
//Use the Speak Method of this control
                              Sapi.Speak using str500 
                              Move           no to Helloflag                
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
;         TRAP      XMAS IF F8
         trap      gamemenu if F6
         trap      logo1 IF F10
         INCLUDE   LOGIO.inc
         TRAP      IO GIVING ERROR IF IO
         TRAP      GOOUT IF F3
         MOVE      ERROR TO CWK11
;         RESET     CWK11,9            .dlh 22jul93
         scan       "*",CWK11
         GOTO      MASMENU IF NOT EQUAL
         GOTO      FREEZE
XMAS     TRAPCLR   F8
         CHAIN     "XMASTREE"
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


      Move    "This is a Error e-mail from master",SmtpSubject Subject

;   Set the text message that is send with the attachments

       Move    "This is an error message",SmtpTextMessage(1)   Array <Text message >
        Move    error,SmtpTextMessage(2)   Array <Text message >
        Move    "This is from subroutine FREEZE",SmtpTextMessage(3)   Array <Text message >
        Move    "Incorrect termination this the NIN Master program",SmtpTextMessage(4)   Array <Text message >
;        Move    "This is the text message line 4",SmtpTextMessage(4)   Array <Text message >
;        Move    "This is the text message line 5",SmtpTextMessage(5)   Array <Text message >

        Move    "4",SmtpTextIndexLast                               Index to last entry in TextMessage array

         call      errmesg
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
         trap      gamemenu if f6
         TRAP      GOOUT IF F3
;         TRAP      XMAS IF F8
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
      Move    "This is a Error e-mail from master",SmtpSubject Subject

;   Set the text message that is send with the attachments

       Move    "This is an error message",SmtpTextMessage(1)   Array <Text message >
        Move    "Naughty ",SmtpTextMessage(2)   Array <Text message >
        Move    "Naughty ",SmtpTextMessage(3)   Array <Text message >
        Move    "Incorrect termination this the NIN Master program",SmtpTextMessage(4)   Array <Text message >
;        Move    "This is the text message line 4",SmtpTextMessage(4)   Array <Text message >
;        Move    "This is the text message line 5",SmtpTextMessage(5)   Array <Text message >

        Move    "4",SmtpTextIndexLast                               Index to last entry in TextMessage array
         call      errmesg
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
         If    (security = 8)
         insertitem SalesList001,9999," 1...Shipping Status"
         insertitem SalesList001,9999," 2...Datacard (List)Inquiry"
         else
         insertitem SalesList001,9999," 1...Order ENTRY & INQUIRY"
         insertitem SalesList001,9999," 3...Fulfillment Maintenance Program"
         insertitem SalesList001,9999," 4...Datacard ENTRY & INQUIRY"
         insertitem SalesList001,9999," 5...Shipping ENTRY & INQUIRY"
         insertitem SalesList001,9999," 7...Mailer/Offer Entry/Inquiry"
         insertitem SalesList001,9999," 8...Datacard Inquiry (NEW)"
         insertitem SalesList001,9999," 9...Owner ENTRY & INQUIRY"
         insertitem SalesList001,9999,"10...Booking Instructions"
         insertitem SalesList001,9999,"11...Datacard (List) Requests"
         insertitem SalesList001,9999,"12...Order Merge Inquiry"
         insertitem SalesList001,9999,"13...Bill-to Maintenance"
         insertitem SalesList001,9999,"14...Pay-to Maintenance"
         insertitem SalesList001,9999,"15...Return-to Maintenance"
         insertitem SalesList001,9999,"16...Order History Reports"
         insertitem SalesList001,9999,"20...Exchange History Review"
         insertitem SalesList001,9999,"21...Test of the Day"
         insertitem SalesList001,9999,"23...Order History Review"
;         insertitem SalesList001,9999,"24...Exchange History Reports"
         insertitem SalesList001,9999,"24...Marketing List Maint."
         insertitem SalesList001,9999,"25...Correction\Cancellation Program "
         insertitem SalesList001,9999,"26...Maildate schedules"
         insertitem SalesList001,9999,"27...Broker Call Program"
.START PATCH 3.4 REPLACED LOGIC
.         insertitem SalesList001,9999,"28...                    "
         insertitem SalesList001,9999,"28...Mailer/Mailer Notes Program"
.END PATCH 3.4 REPLACED LOGIC
         insertitem SalesList001,9999,"30...List History Analysis"
         insertitem SalesList001,9999,"31...Fax Promotion"
         insertitem SalesList001,9999,"33...Client Credit History"
         insertitem SalesList001,9999,"34...Broker/Consultant/Mailer/Offer"
         insertitem SalesList001,9999,"44...Request For Counts"
         insertitem SalesList001,9999,"45...Suppression reports"
         insertitem SalesList001,9999,"38...Telecomm maint"
         insertitem SalesList001,9999,"40...List Management Order reports"
.START PATCH 3.5 ADDED LOGIC
         insertitem SalesList001,9999,"47...Sample Scanner Program"
.END PATCH 3.5 ADDED LOGIC
.START PATCH 3.6 ADDED LOGIC
         insertitem SalesList001,9999,"48...New Order Inquiry Program"
.END PATCH 3.6 ADDED LOGIC
;
         insertitem AcctList001,9999," 2...Invoice Entry & Inquiry"
.>Patch 3.7         
         insertitem AcctList001,9999,"46...List Income and Order Variance"
.>Patch 3.7         
         insertitem AcctList001,9999," 3...Fulfillment Maintenance Program"
         insertitem AcctList001,9999," 6...Cash Receipts & Invoice Adjustments"
         insertitem AcctList001,9999,"19...Running Charges "
         insertitem ACCTList001,9999,"21...Test of the Day"
         insertitem AcctList001,9999,"22...Invoice LO # Mod "
         insertitem AcctList001,9999,"32...Mark Order Billed "
         insertitem AcctList001,9999,"35...Check Information Maint"
         insertitem AcctList001,9999,"36...List Owner Payables"
         insertitem AcctList001,9999,"37...Mailer payment Inquiry"
         insertitem AcctList001,9999,"39...Triplex Billing"
         insertitem AcctList001,9999,"42...Money on Account"
         insertitem AcctList001,9999,"43...Amount Due"
         insertitem AcctList001,9999,"33...Credit History"
;

         insertitem ISList001,9999,"21...Test of the Day"
         insertitem ISList001,9999,"70...CALPIC -OWNER PICK FOR TRANSMISSION"
         insertitem ISList001,9999,"71...DATAMRG - DATAPICK MERGE."
         insertitem ISList001,9999,"73...DATAPSYS - DATACARD PRINT"
         insertitem ISList001,9999,"74...DATPRINT - RH STYLE DATACARD"
         insertitem ISList001,9999,"75...DELREC - RECORD DELETION PROGRAM"
         insertitem ISList001,9999,"76...DSLIST1 - FILE DUMP PROGRAM"
         insertitem ISList001,9999,"77...DSTEXT - DATASHARE UTILITIES"
         insertitem ISList001,9999,"78...FIXBUSY -ORDERS/INVOICES/ADJUSTMENTS"
         insertitem ISList001,9999,"79...FIXCARD - REINSTATE DATACARDS"
         insertitem ISList001,9999,"80...FIXCHNG - REINSTATE EXCHANGES"
         insertitem ISList001,9999,"81...FIXINV - REPAIR INVOICE PROG."
         insertitem ISList001,9999,"84...FIXXNUM - EXCHANGE MAINT. PROGRAM"
         insertitem ISList001,9999,"86...LISTNIN - MASTER MAILER PROGRAM"
         insertitem ISList001,9999,"87...LISTNIN1 - MINI MAILER PROGRAM"
         insertitem ISList001,9999,"88...MARK - ORDERS/INVOICES"
         insertitem ISList001,9999,"89...ORDERPRT - ORDER STATISTICS PROGRAM"
         insertitem ISList001,9999,"90...OWNNEW - DATACARD OWNER INFO."
         insertitem ISList001,9999,"91...PASSMOD - PASSWORD MODIFICATION"
         insertitem ISList001,9999,"92...SPIMOD - SPECIAL INSTRUCTION MOD."
         insertitem ISList001,9999,"93...STARTER - START JOBS"
         insertitem ISList001,9999,"94...SUPERPIC - SUPER DATACARD PICK"
         insertitem ISList001,9999,"99...Name your Program"
         endif

         setprop SalesList001,enabled=1,bgcolor=white
         setprop ISList001,enabled=1,bgcolor=white
;
         setprop acctList001,enabled=1,bgcolor=white
         setprop   Saleslist001,visible=1
         setprop   acctlist001,visible=0
         setprop   ISlist001,visible=0
         setfocus  SalesList001,1
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
GETPROG  TRAP      NOTFOUND IF CFAIL
         MOVE      "PROGRAM",LOGTYPE
               If             (passflag = "Y" & INdex = 99)
               setfocus       Master001aEdit
               Setprop        MasterProgramSelect,visible=1
               call           Trim using ProgramName
               MOVE           ProgramName,LOGINFO
               CALL           LOGWRITE
               CHAIN          ProgramName
               endif
;         getitem   Masteredittext001,0,index
;
PRGBRNCH                If    (security = 8)
               BRANCH    INDEX OF SHPMOD:        :#1
                              Ndat1
               ELSE
               BRANCH    INDEX OF NINMAC:     # 1
                            MODINV:     # 2
                            FULFIL:     # 3
                            DATAMOD:    # 4
                            SHPMOD:     # 5
                            CASH:       # 6
                            MLRMOD:     # 7
                            NDAT0001A:     # 8
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
                            TESTODAY:    #21
                            FIXINV:    #22
                            NINUSAGE:    #23
                            MrktList:   #24
                            NCORR0001:    #25
                            SCHEDULE:   #26
                            NCAL0001:    #27
.START PATCH 3.4 REPLACED LOGIC
.                            NoSuch:   #28
                            NMLRXY0001:   #28
.END PATCH 3.4 REPLACED LOGIC
                            NAPR0001:   #29
                            LISTHIST:   #30
                            Promosend:   #31
                            NINOBILL:   #32
                            Credit0001:   #33
                            comp0001:   #34
                            CHCKINFO:   #35
                            ninv0013:       #36
                            ninv17:     #37
                            TELEMAST:   #38
                            ndat0022:    #39
                            NORD0037:    #40
                            MAILPLAN:   #41
                            ONACOUNT:  #42
                            AMOUNTD:  #43
                            nrco0001:   #44
                            SUPPRESS:   #45
.>Patch 3.7                            
                            VARIANCE:   #46
.>Patch 3.7                            
.START PATCH 3.5 REPLACED LOGIC
.                   NoSuch,NoSuch,NoSuch,NoSuch:
                            NSMP0001:   #47
.START PATCH 3.6 REPLACED LOGIC
.                   NoSuch,NoSuch,NoSuch:
                   NORD8001:   #48
                            NoSuch,NoSuch:
.END PATCH 3.6 REPLACED LOGIC
.END PATCH 3.5 REPLACED LOGIC
                   NoSuch,NoSuch,NoSuch,NoSuch,NoSuch,NoSuch:
                   NoSuch,NoSuch,NoSuch,NoSuch,NoSuch,NoSuch:
                   NoSuch,NoSuch,NoSuch,NoSuch,NoSuch,NoSuch:
                   NoSuch:
                            CALPIC:     #70
                            DATAMRG:    #71
                            Nosuch:     #72
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
                            Nosuch:      #83
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
               ENDIF
;END PATCH 2.9 REPLACED LOGIC
         GOTO      NoSuch
*................................................................
NINMAC
;         if        (portn = 31 or portn = 112 OR PORTN = 188 OR PORTN =119 OR PORTN = 19 OR PORTN = 56 OR PORTN = 57 OR PORTN = 222 OR PORTN = 24 OR PORTN = 230 OR PORTN = 250)
;         MOVE      "Drewtest",LOGINFO
;         CALL      LOGWRITE
;         CHAIN     "Drewtest"
;         endif
         MOVE      "NORDtest",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NORDtest"
MODINV   MOVE      "NINV0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NINV0001"
BetaINV  MOVE      "NINV0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "master1"
FULFIL   MOVE      "NFUL0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NFUL0001"

DATAMOD  MOVE      "NDAT0001",LOGINFO
         CALL      LOGWRITE
;         COMPARE   "101" to portn         *wildcat client dial in ????
;         IF        EQUAL
;         setmode   *fullscr=MAX
;         chain     "ndat0037"
;         else
         winshow
         CHAIN     "NDAT0001"
;         endif
;temp until datacard progranm Totally rewritten  DLH 04Feb02
NDAT0001a  MOVE      "NDAT0001a",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NDAT0001a"
;         endif
SHPMOD   MOVE      "NSHP0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NSHP0001"
CASH
         MOVE      "NCSH0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NCSH0001"
MLRMOD   MOVE      "NMLR0001",LOGINFO
         CALL      LOGWRITE
.         CHAIN     "NMLR0001"
         CHAIN     "comp0001"
OWNMOD   MOVE      "NOWN0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NOWN0001"
nmdl0001 MOVE      "Nmdl0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "Nmdl0001"
;START PATCH 2.9 REPLACED LOGIC
;ROLODEX  MOVE      "NROL0001",LOGINFO
;         CALL      LOGWRITE
;         CHAIN     "NROL0001"
SUPPRESS MOVE      "NSPR0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NSPR0001"
;END PATCH 2.9 REPLACED LOGIC
DATAPIC  MOVE      "NDAT0004",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NDAT0004"
MERGE    MOVE      "NMRG0001",LOGINFO
         CALL      LOGWRITE
         winshow
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
         winshow
         CHAIN     "NORD0006"
CALCPROG MOVE      "NCLC0001",LOGINFO
         CALL      LOGWRITE
;         winshow
;         CHAIN     "NCLC0001"
         execute   "calc.exe"
NINCLEAR MOVE      "NLCR0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NLCR0001"
RUNCAL   MOVE      "NINV0008",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NINV0008"
TESTODAY MOVE      "TESTODAY",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "TESTODAY"
CHCKINFO MOVE      "NCHK0005" TO LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NCHK0005"
EXCHANGE MOVE      "NXCH0001",LOGINFO
         move      "NXCH0001",program
         CALL      LOGWRITE
;         winshow
         CHAIN     "NXCH0001"
NINV0017 MOVE      "NINV0017",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NINV0017"
NINUSAGE
         MOVE      "NORD0007",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NORD0007"

NCORR0001 MOVE      "NCORR0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NCORR0001"

MrktList       Chain          "Mrkt0001"
;EXCHPRNT MOVE      "NXCH0002",LOGINFO
;         CALL      LOGWRITE
;         winshow
;         CHAIN     "NXCH0002"
LCRUSAGE MOVE      "NLCR0006",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NLCR0006"
SCHEDULE MOVE      "NSCH0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NSCH0001"
NCAL0001 MOVE      "Ncal0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NCAL0001"
.START PATCH 3.4 ADDED LOGIC
NMLRXY0001
         MOVE      "NMLRXY0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NMLRXY0001"
.END PATCH 3.4 ADDED LOGIC
JOURNAL  MOVE      "NJRN0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NJRN0001"

PromoSend MOVE      "NPRM0001",LOGINFO
;          CALL      LOGWRITE
         CHAIN     "NPRM0001"

NCRC0001 MOVE      "CORRECTION",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NCRC0001"
NAPR0001 MOVE      "APPROVAL",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NAPR0001"
LISTHIST MOVE      "LISTHIST",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "LISTHIST"
NINCOUNT MOVE      "NCOU0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NCOU0001"
NINOBILL MOVE      "NINV0007",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NINV0007"
CREDIT0001 move     "credit1" to loginfo
         CALL      LOGWRITE
;Nmlr0007 MOVE      "Nmlr0007",LOGINFO
;         CALL      LOGWRITE
         winshow
;         CHAIN     "Nmlr0007"
         chain     "credit0001"
comp0001 MOVE      "comp0001" TO LOGINFO
         CALL      LOGWRITE
         CHAIN     "comp0001"
NDAT0022 MOVE      "NDAT0022",LOGINFO
         winshow
         CHAIN     "NDAT0022"
FIXCARD  MOVE      "NDAT0009",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NDAT0009"
Nord0037 MOVE      "NORD0037",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NORD0037"
MAILPLAN MOVE      "MAILPLAN" TO LOGINFO
         CALL      LOGWRITE
         CHAIN     "NMPL0001"
nrco0001 MOVE      "nrco0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "nrco0001"
nacr0001 MOVE      "nacr0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "nacr0001"
GAMEMENU MOVE      "GAMEMENU",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "GAMEMENU"
ninv0013 MOVE      "Ninv0013" TO LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "Ninv0013"
ninv17   move      "ninv0017" to loginfo
         winshow
         CHAIN     "ninv0017"
ONACOUNT MOVE      "NONA0001" TO LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NONA0001"
AMOUNTD  MOVE      "NINV0010" TO LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NINV0010"
DATAPSYS MOVE      "NDAT0002",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NDAT0002"
DATPRINT MOVE      "NDAT0005",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NDAT0005"
DATAPEXT MOVE      "NDAT0007",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NDAT0007"
DATAMRG  MOVE     "NDAT0006 ",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "NDAT0006"
DSLIST1  MOVE      "DSLIST1 ",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "DSLIST1"
DSTEXT   MOVE      "DSTEXT",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "DSTEXT"
DELREC   MOVE      "DELREC",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "DELREC"
FIXXNUM  MOVE      "NXCH0005",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "NXCH0005"
LISTNIN  MOVE      "NMLR0002",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "NMLR0002"
LISTNIN1 MOVE      "LISTNIN1",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "LISTNIN1"
ORDERPRT MOVE      "NORD0010",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "NORD0010"
OWNNEW   MOVE      "OWNNEW",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "OWNNEW"
FIXBUSY  MOVE      "NORD0021",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "NORD0021"
FIXLCR1  MOVE      "NLCR0010",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "NLCR0009"
LCRMAINT MOVE      "NLCR0007",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "NLCR0007"
FIXORD   MOVE      "NORD0009",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "NORD0009"
FIXINV   MOVE      "NINV0023",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "NINV0023"
FIXCHNG  MOVE      "NXCH0004",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "NXCH0004"
;FIXCARD  MOVE      "FIXCARD",LOGINFO
         CALL      LOGWRITE
;         CHAIN    "FIXCARD"
MARK     MOVE      "MARK",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "MARK"
SPIMOD   MOVE      "NSPI0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "NSPI0001"
STARTER  MOVE      "STARTER",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "STARTER"
SUPERPIC MOVE      "NDAT0017",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NDAT0017"
PASSMOD  MOVE      "NPAS0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN    "NPAS0001"
CALPIC   MOVE      "NOWN0003",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NOWN0003"
TELEMAST MOVE      "TELEMAST" TO LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "TELEMAST"
.>Patch 3.7         
VARIANCE MOVE      "NVAR0001",LOGINFO
         CALL      LOGWRITE
         winshow
         CHAIN     "NVAR0001"         
.>Patch 3.7         
Ndat1    MOVE      "Ndat1111",LOGINFO
         CALL      LOGWRITE
         CHAIN     "Ndat1111"
.START PATCH 3.5 ADDED LOGIC
NSMP0001
         MOVE      "NSMP0001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NSMP0001"
.END PATCH 3.5 ADDED LOGIC
.START PATCH 3.6 ADDED LOGIC
NORD8001
         MOVE      "NORD8001",LOGINFO
         CALL      LOGWRITE
         CHAIN     "NORD8001"
.END PATCH 3.6 ADDED LOGIC
;
NOTFOUND NORETURN
;         DISPLAY   *B,*P45:24,*HON:
;                   *B,*P45:24,"Sorry, this program is not on-line.",*W2,*HOFF;
         MOVE      "CHAIN ATTEMPT FAILURE",LOGINFO
         CALL      LOGWRITE
//Use the Speak Method of this control
               pack           str500 from "Sorry, ",Fname," That program is not on-line"
               Sapi.Speak using Str500
         alert   caution,"Sorry, That program is not on-line.",result
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
         setprop   Saleslist001,visible=1
         setfocus  SalesList001,1
         endif
         if        (TABNUM = 2)
         setprop   acctlist001,visible=1
         setfocus  AcctList001,1
         endif
         if        (TABNUM = 3)
;Verify Password if necessary
                if (PassFlag = "N")
                        setitem PasswordEdit,0,""
                        setfocus PasswordEdit
                        setprop Passwrd,visible=1
                endif
                if (PassFlag = "N")
                        move       savetab to tabnum
                        move       savetab to n2
                        setitem    MasterTabControl,0,n2
                        goto       MasterTabChange
                endif
         setprop   ISlist001,visible=1
         setfocus  ISList001,1
         endif
         return
MasterTabClick
         move      N2,TabNum            .from form TAB  Click event
         move      tabnum to savetab
         if        (TABNUM = 1)
         setprop   Saleslist001,visible=0
         endif
         if        (TABNUM = 2)
         setprop   acctlist001,visible=0
         endif
         if        (TABNUM = 3)
         setprop   ISlist001,visible=0
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
               Sapi.Speak using Str500         
         goto    mainloop
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
          endif
          return
GetPAss          trapclr F10
                 trap    GetPAss if f10
                 move    "v",progcode
                 move    "N",PassFlag
                 setitem PasswordEdit,0,""
                 setfocus PasswordEdit
                 setprop Passwrd,visible=1
               If             (Passflag = "Y")
               move               c0 to security
               else
               move               c8 to security
               endif
               call           MasterClearLists
                Noreturn
                goto          Page1
MasterClearLists
               DeleteItem     SalesList001,0
               DeleteItem     AcctList001,0
               DeleteItem     ISList001,0
;               Saleslist001.DeleteAllItems giving N9
;               Acctlist001.DeleteAllItems giving N9
;               ISlist001.DeleteAllItems giving N9
               return
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

