;..............................................................................
;
;              NAMES IN THE NEWS          MASTER MENU FOR OUTSIDE INTERNET CLients (NOT STAFF)
;
;..............................................................................
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
;............................................................................
;
;
PC       EQU       1
release        init      "1.0"           DLH  
REldate        Init           "March 20 2003"
;
               INCLUDE   CONS.inc
;
               INCLUDE   NUSEDD.inc
;
               INCLUDE   LOGDATA.inc
;
               Include   npasdd.inc
;
Timer   Timer
GetTime   Timer
;
;Colors
white   color
grey    color
RED     COLOR
BLACK   COLOR
date    dim     8


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
LOGTIME  DIM       18
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
;.............................................................................
main
;............................
;Set Vars used for About Box
        move    "Masterweb.PLS",Wprognme
        move    "Master PLB Menu",Wfunction
        move    "David Herrick",Wauthor
        move    "MasterWB" to program                  .hopefully force shutdown if we go back to answer
        move    Release,Wrelease
        move    Reldate,Wreldate

mss1    plform  Error
pss     plform  Passwrd
abt     plform  About
PRog    plform  Master001a
x       plform  MasterWEb001
;        winhide
;Load Forms, Always load parent form first
        formload x
        FormLoad              PRog
        formload abt
        formload pss
        formload mss1
;Create Menus
        create  Master001;mFile,FData
;        create  Master001;mOptions,OData,mFile
        create  Master001;mHelp,HData,mOptions
;        CREATE  Master001;sCOlor,Cdata,mOptions,1
;        CREATE  Master001;sSound,Sdata,mOptions,2

;Activate Menus
;FileGo leads to stop
        activate mFile,FileGo,result
;        activate mOptions
        activate mHelp,HelpGo,result
;        activate sColor,ColorGo,result
;        activate sSound,SoundGo,result
;set properties in collection
        listins ColText,Master001,AcctList001,DateText,DayofWeek:
                ISList001,MasterButton001,MasterEditText001,MasterTabControl,PortText:
                PortText001,SalesList001,TimeText,UserText
        listins Colback,Master001,AcctList001,DateText,DayofWeek:
                ISList001,MasterButton001,MasterEditText001,MasterTabControl,PortText:
                PortText001,SalesList001,TimeText,UserText

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
;        move    "S",progcode
        move    "N",PassFlag

        CREATE  TIMER,18000     ..30 minutes
        ACTIVATE TIMER,Timeout,RESULT
;
        CREATE  GetTIME,600     ..1 minute
        ACTIVATE GetTIME,Newtime,RESULT
;
         CLOCK     DATE TO TODAY
         clock     timestamp to timestamp
         unpack    timestamp into cc           .get century
         move      c0 to timeout

         UNPACK    TODAY INTO MM,STR1,DD,STR1,YY
         TRAP      GOOUT IF INT

FINDIT   MOVE      MM TO NMM
         MOVE      DD TO NDD
         CLEAR     FUNC
         CLEAR     TYPINIT
;first time thru? if so say hello
         if        (portn <= 0)
         call      hello
         endif
;later add user preference to suppress
;         CALL      FINDPORT
         setitem   UserText,0,nuseUser
         setitem   Porttext,0,Nusefld
;         call      win95
         GOTO      TRAPCHK
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
         TRAP      IO GIVING ERROR IF IO
         CALL      DAY
         trap      logo1 IF F10
         INCLUDE   LOGIO.inc
         TRAP      IO GIVING ERROR IF IO
         TRAP      GOOUT IF F3
         MOVE      ERROR TO CWK11
         scan       "*",CWK11
         GOTO      MASMENU IF NOT EQUAL
         GOTO      FREEZE 
IO       TRAPCLR   IO
         NORETURN
         TRAP      IO GIVING ERROR IF IO
         trap      naughty if DEL
         trap      naughty if RANGE
         trap      naughty if PARITY
         trap      naughty if FORMAT
         trap      naughty if CFAIL
         TRAP      GOOUT IF INT
         GOTO      FREEZE
;
; THIS FREEZES THE ERROR ON THE SCEEEN SO THAT A BETTER DETERMINATION CAN
; BE MADE CONCERNING THE CAUSE OF THE PROBLEM.
;
FREEZE   
         trap      naughty if DEL
         trap      naughty if RANGE
         trap      naughty if PARITY
         trap      naughty if FORMAT
         trap      naughty if CFAIL
         TRAP      GOOUT IF INT


      Move    "This is a Error e-mail from master",SmtpSubject Subject

;   Set the text message that is send with the attachments

       Move    "This is an error message",SmtpTextMessage(1)   Array <Text message >
        Move    error,SmtpTextMessage(2)   Array <Text message >
        Move    "This is from subroutine FREEZE",SmtpTextMessage(3)   Array <Text message >
        Move    "Incorrect termination this the NINCAL Master WEB program",SmtpTextMessage(4)   Array <Text message >

        Move    "4",SmtpTextIndexLast                               Index to last entry in TextMessage array

         call      errmesg             
         trapCLR  INS   
         TRAPCLR   F6
         TRAPCLR   F3
         TRAPCLR   F8
         TRAP      GOOUT IF INT
         TRAP      GOOUT IF F3
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
         TRAP      GOOUT IF INT
         move      "Naughty" to error
      Move    "This is a Error e-mail from masterweb",SmtpSubject Subject

;   Set the text message that is send with the attachments

       Move    "This is an error message",SmtpTextMessage(1)   Array <Text message >
        Move    "Naughty ",SmtpTextMessage(2)   Array <Text message >
        Move    "Naughty ",SmtpTextMessage(3)   Array <Text message >
        Move    "Incorrect termination this the NINCAL Master WEB program",SmtpTextMessage(4)   Array <Text message >

        Move    "4",SmtpTextIndexLast                               Index to last entry in TextMessage array
         call      errmesg
;ditto use error dialogue
         SNDOPEN   sfile,"\\nts0\c\Netutils\media\thunder.WAV"
         SNDPLAY   sfile
         SNDPLAY   sfile
         SNDCLOSE   sfile
          goto     freeze
;
*
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
         clock     weekday to str2
         move      str2 to DOW
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
         MOVE      "MASTERWEB",LOGINFO
;         CALL      LOGWRITE
         GOTO      PAGE1
PAGE1
;SALESLIST001
         insertitem SalesList001,9999," 1...Shipping Status"
         insertitem SalesList001,9999," 2...Datacard (List)Inquiry"
         setprop SalesList001,enabled=1,bgcolor=white
;
;         insertitem AcctList001,9999," 2...Future Programs"
         setprop acctList001,enabled=0,bgcolor=white
;

;         insertitem ISList001,9999,"21...Test of the Day"
         setprop ISList001,enabled=0,bgcolor=white
;
         setprop   Saleslist001,visible=1
         setprop   acctlist001,visible=0
         setprop   ISlist001,visible=0
         setfocus  SalesList001,1
         setfocus  Masteredittext001,1

MainLoop
        loop
                winhide
                waitevent
        repeat
        
        goto    mainloop
Timeout
        beep
        beep
        beep
        shutdown
        stop
Newtime
         CLOCK     TIME TO TIME
         UNPACK    TIME INTO STR2,ANS,MIN,ANS,SEC
         MOVE      TIME,STR2
;
         MOVE      STR2,HOUR
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
;
PRGBRNCH BRANCH    INDEX OF SHipping:        :#1
                   Ndat1 
                                                      GOTO      NoSuch
*................................................................
SHipPing   MOVE      "NSHP0001",LOGINFO
;         CALL      LOGWRITE
         CHAIN     "NSHP0001"
Ndat1    MOVE      "Ndat1111",LOGINFO
;         CALL      LOGWRITE
         CHAIN     "Ndat1111"
;
NOTFOUND NORETURN
         MOVE      "CHAIN ATTEMPT FAILURE",LOGINFO
;         CALL      LOGWRITE
         alert   caution,"Sorry, That program is not on-line.",result
         goto    mainloop

GOOUT    MOVE      "SHUTDOWN TO OS",LOGINFO
;         CALL      LOGWRITE
         SNDOPEN   sfile,"\\nts0\c\Netutils\media\bye.wav"
         SNDPLAY   sfile
        SNDCLOSE   sfile
        shutdown
        STOP
MasterTabchange
         move      N2,TabNum            .from form TAB Change event
         if        (TABNUM = 1)
         setprop   Saleslist001,visible=1
         setfocus  SalesList001,1
         endif
         if        (TABNUM = 2)
                if (PassFlag = "N")
                        move       savetab to tabnum
                        move       savetab to n2
                        setitem    MasterTabControl,0,n2
                        goto       MasterTabChange
                endif
         setprop   acctlist001,visible=1
         setfocus  AcctList001,1
         endif
         if        (TABNUM = 3)
;Verify Password if necessary
;                if (PassFlag = "N")
;                        setitem PasswordEdit,0,""
;                        setfocus PasswordEdit
;                        setprop Passwrd,visible=1
;                endif
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
                 move      No to PassFlag
         endif
         return
NOsuch
         alert   caution,"Failed to load program",result
         goto    mainloop
;..................................................
logo     if        (portn <= 0)
logo1    trapclr   F10
         trap      logo1 if F10
         create    LOGO=04:18:12:85,"\\nts0\c\netutils\ninca.gif",border
         activate  Logo
         SNDOPEN   sfile,"\\nts0\c\Netutils\media\tada.wav"
         SNDPLAY   sfile
         SNDCLOSE   sfile
         deactivate  Logo
         destroy    logo
         endif
         return
;......................................................................................................
FileGo
                winshow
                shutdown
                stop
        return

;................................................................................
HelpGo
        setprop AboutMssg,visible=1
        return       
         
         INCLUDE   PORTCALC.inc
         INCLUDE   MESSAGE.inc
         INCLUDE   NUSEIO.inc
         include   npasio.inc
         INCLUDE   COMLOGIC.inc

