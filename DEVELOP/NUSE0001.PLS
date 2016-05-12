...............................................................................
.MODUSERS - PROGRAM TO MODIFY 'NINUSERS' FILE                                 .
...............................................................................
.CREATED 13 JUNE 90
...................
PC       EQU       0 

         INC       COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NUSEDD.INC
Release	Init	"1.4"	DLH rewrite
RelDate	Init	"19 March 2008"
.Release	Init	"1.3"	DLH 13March 2007  Pacfic Lists
.release  init      "1.2"          DLH 24Feb98 file structure change
.RELEASE  INIT      "1.1"
.
FUNCBR   FORM      1
.Begin  patch 1.4
	INclude	Npasdd.inc
.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu

.Set Up SubMenu for Options
sColors submenu

.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Color"
HData   init    "&Help;&About"

.Present Data for Colors SubMenu
CData   init    ";Foreground E&dit;Foreground &Text;Foreground &List;&Background List"

.Define Collections for Object Colors
ColTxt  Collection
ColStat Collection

.Define Colors for Each Object
FTC     color
FSC     color
FDC     color
BDC     color

.Define colors EditText Inquiries
white   	color
grey    	color

.Define Fonts to be used
font1   	font
font2   	font
font3   	font
font4   	font
FontO10   	  font
FontO18I   	  font
FontO18B        font
FontO18BI       font
Timer   	Timer
prfile  	pfile
        	move    "Nuse0001.PLS",Wprognme
        	move    "PLB User Port & Inits",Wfunction
        	move    "David Herrick",Wauthor
            MOve           Release to Wrelease
            Move           RelDate to WrelDate

.Declare forms, Always declare child forms first
pss     plform  passwrd
abt     plform  about
x       plform  Nuse0001
        winhide
.Load Forms, Always declare parent form first
        formload x
        formload abt
        formload pss

        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        	create  Nuse0001;mFile,FData
        	create  Nuse0001;mEdit,EData,mFile
        	create  Nuse0001;mOptions,OData,mEdit
        	create  Nuse0001;mHelp,HData,mOptions

.Create SubMenu
        	create  Nuse0001;sColors,CData,mOptions,1

.Activate Menus
.FileGo leads to stop
        	activate mFile,FileGo,result
.Need this when it works
        	activate mEdit,EditGo,result
.Only a SubMenu under this one
        	activate mOptions
        	activate mHelp,HelpGo,result

.Activate SubMenu
        	activate sColors,ColorGo,result

        	CLOCK   TimeStamp,TimeStamp
	unpack         timestamp,CC,YY,MM,DD

	call	ClearRec
.Create Colors for EditText Inquiry
        	create  white=*white
        	create  grey=*ltgray
.Create fonts to be used
        	create  font1,"Arial",size=12,bold
        	create  font2,"Arial",size=8
        	create  font3,"Helvetica",size=9
        	create  font4,"Arial",size=12,italic
        	create  fontO10,"Times New Roman",size=10
        	create  fontO18I,"Times New Roman",size=18,Italic
        	create  fontO18B,"Times New Roman",size=18,Bold
        	create  fontO18BI,"Times New Roman",size=18,Bold,Italic
	Nuse0001ListView001.InsertColumn using "Port",70,0
	Nuse0001ListView001.InsertColumn using "Employee",200,1
	Nuse0001ListView001.InsertColumn using "Initials",70, 2
	Nuse0001ListView001.InsertColumn using "Inactive",70,3
	Nuse0001ListView001.InsertColumn using "Division",70,4
	setprop	Nuse0001ListView001,hidesel=0
.show list of current PLB USers
.add code to read booking instruction inits caller/planner	
	Loop
	call	NUseKS
	Until 	over
	Nuse0001ListView001.InsertItem giving result using NUSENUM
	Nuse0001ListView001.SetItemText using result,NUSEUSER,1 // col2
	Nuse0001ListView001.SetItemText using result,NUSEINIT,2 // col3
	Nuse0001ListView001.SetItemText using result,nuseActive,3 // col4
	Nuse0001ListView001.SetItemText using result,NuseComp,4   // col5
	repeat
            setprop	Nuse0001ButtonMod,enabled=0
            setprop	Nuse0001ButtonMod,Visible=0
            setprop	Nuse0001ButtonNew,enabled=1
            setprop	Nuse0001ButtonNew,Visible=1
            setprop	Nuse0001ButtonQuit,enabled=0
            setprop	Nuse0001ButtonQuit,Visible=0
            setprop	Nuse0001ButtonSave,enabled=0
            setprop	Nuse0001ButtonSave,Visible=0
.end patch 1.4
.
.
.Main Loop

        setfocus Nuse0001ListView001
        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat

.         MOVE      "NUSE0001" TO PROGRAM
.         MOVE      "PORT/USER MAINTENANCE" TO STITLE
.         MOVE      "Names in the News Ca" TO COMPNME
.         CALL      PAINT
.         move      "Exit" to pf5
.         call      funcdisp
.         trap      stop if f5
         MOVE      C1 TO NUSEPATH
.
LoadUserDetail
.Load all Text Fields
	Nuse0001ListView001.GetItemText giving str3 using result,0
	packkey	Nusefld,str3
	call	Nusekey
	setitem Nuse0001EditText001,0,Nusenum
        	setitem Nuse0001EditText002,0,NuseInit
        	setitem Nuse0001EditText003,0,NuseUser
        	setitem Nuse0001EditText004,0,NuseActive
	if	(NUseCOmp = "N")
        	setitem Nuse0001ComboBox001,0,2
	Elseif	(NUseCOmp = "P")
        	setitem Nuse0001ComboBox001,0,3
	endif
            setprop	Nuse0001ButtonMod,enabled=1
            setprop	Nuse0001ButtonMod,Visible=1
	Return




START    KEYIN     *P10:10,*El,"PORT NUMBER : ",*JR,*ZF,NUSENUM
         SCAN      STAR IN NUSENUM
         GOTO      STOP IF EQUAL
         MOVE      NUSENUM TO HOLDNUM
         MOVE      NUSENUM TO NUSEFLD
         MOVE      C1 TO FUNCBR
         CALL      NUSEKEY
         GOTO      ADD IF OVER
         DISPLAY   *P24:10,NUSENUM,"     IP: ",nusernid,"-000-000-",nusenum:
                   *P10:12,"NAME         : ",NUSEUSER:
                   *P10:14,"INITIALS     : ",NUSEINIT:
                   *P10:16,"IP Ring ID   :",NUSERNID:
                   *P10:18,"Lan Card Man :",NUSEADPT:
                   *P10:20,"Company Code :",NuseComp

         KEYIN     *P24:23,*EL,"MODIFY THIS RECORD ? ",STR1
         match     "D" to str1
         goto      delete if equal
         CMATCH    yes TO STR1
         GOTO      START IF NOT EQUAL
KEY
         KEYIN     *P24:10,*RV,*ZF,*JR,NUSENUM
         SCAN      STAR IN NUSENUM
         GOTO      START IF EQUAL
         MOVE      NUSENUM TO NUSEFLD
         TYPE      NUSENUM
         GOTO      KEY1 IF EQUAL
         DISPLAY   *P10:23,*EL,*HON,"MUST BE NUMERIC!!!!!!",*B:
                   *W,*B,*W,*B,*W,*HOFF,*P10:23,*EL
         MOVE      HOLDNUM TO NUSEFLD
         GOTO      KEY
KEY1     KEYIN     *P24:12,*JL,*RV,NUSEUSER
         SCAN      STAR IN NUSEUSER
         GOTO      START IF EQUAL
         SCAN      "<" IN NUSEUSER
         GOTO      KEY IF EQUAL
KEY2     KEYIN     *P24:14,*RV,*JL,NUSEINIT
         SCAN      STAR IN NUSEINIT
         GOTO      START  IF EQUAL
         SCAN      "<" IN NUSEINIT
         GOTO      KEY1 IF EQUAL
KEY3     KEYIN     *P24:16,*RV,*JR,NUSERNID
         SCAN      STAR IN NUSERNID
         GOTO      START IF EQUAL
         SCAN      "<" IN NUSERNID
         GOTO      KEY2 IF EQUAL
KEY4     display   *p35:18,"j2577a(EISA),J2573a(ISA),J2585A(pci),J2585B "
         KEYIN     *P24:18,*RV,*JR,NUSEadpt
         SCAN      STAR IN NUSEadpt
         GOTO      START IF EQUAL
         SCAN      "<" IN NUSEadpt
         GOTO      KEY3 IF EQUAL
KEY5     KEYIN     *P24:20,*RV,*JR,NuseComp
         SCAN      STAR IN NuseComp
         GOTO      START IF EQUAL
         SCAN      "<" IN NuseComp
         GOTO      KEY4 IF EQUAL
KEYDONE  KEYIN     *P24:23,*EL,"EVERYTHING OK ? ",STR1
         CMATCH    "N" TO STR1
         GOTO      KEY IF EQUAL
         BRANCH    FUNCBR OF UPD,WRITE
         GOTO      START
UPD      CALL      NUSEUPD
         GOTO      START
WRITE    CALL      NUSEWRT
         GOTO      START
ADD      DISPLAY   *P24:10,NUSENUM:
                   *P10:12,"NAME        : ",NUSEUSER:
                   *P10:14,"INITIALS    : ",NUSEINIT:
                   *P10:16,"IP Ring ID  : ",NUSERNId:
                   *P10:18,"Type (HP)   : ",NUSEADpt:
                   *P10:20,"Company Code :",NuseComp
         MOVE      NUSEFLD TO NUSENUM
         KEYIN     *P24:10,*DV,NUSENUM:
                   *P24:23,*EF,"ADD THIS RECORD ? ",STR1
         CMATCH    yes TO STR1
         GOTO      START IF NOT EQUAL
         MOVE      C2 TO FUNCBR
         GOTO      KEY1
delete
         KEYIN     *P24:10,*DV,NUSENUM:
                   *P24:23,*EF,"Delete THIS RECORD ? ",STR1
         CMATCH    yes TO STR1
         GOTO      START IF NOT EQUAL
         call      nusedel
         goto      start
STOP     STOP
................................................................................................
ClearRec
.Clear all Text Fields
        	setitem Nuse0001EditText001,0,""
        	setitem Nuse0001EditText002,0,""
        	setitem Nuse0001EditText003,0,""
        	setitem Nuse0001EditText004,0,""
        	setitem Nuse0001ComboBox001,0,0
        return
................................................................................................
Timeout
        beep
        beep
        beep
        stop

FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo3,FileGo3
FileGo1
.        PRTOPEN prfile,"",WPrognme
        	CLOCK   TimeStamp,TimeStamp
	unpack         timestamp,CC,YY,MM,DD

.        goto    StartPrint
FileGo2
        PRTOPEN prfile,"",WPrognme
        CLOCK   TimeStamp,TimeStamp
	unpack         timestamp,CC,YY,MM,DD
.        goto    StartPrint
FileGo3
                winshow
                stop
        return
EditGo
        return
HelpGo
        setprop AboutMssg,visible=1
        return
ColorGo
        branch result to edit1,text1,list1,list2
        return
edit1
        trap    ColorTrap if object
        create  FTC
        trapclr object
        setprop ColTxt,fgcolor=FTC
        return
text1
        trap    ColorTrap if object
        create  FSC
        trapclr object
        setprop ColStat,fgcolor=FSC
        return
list1
        trap    ColorTrap if object
        create  FDC
        trapclr object
        setprop Nuse0001ListView001,fgcolor=FDC
        return
list2
        trap    ColorTrap if object
        create  BDC
        trapclr object
        setprop Nuse0001ListView001,fgcolor=BDC
        return

.Trap for Cancel Entry in Color System Menu
ColorTrap
        noreturn
        return
.end patch 1.4
	INclude	Npasio.inc
         	INCLUDE   NUSEIO.INC
         	INCLUDE   COMLOGIC.INC