PC       EQU       1                         ;=gui
         
         INC       COMMON.inc
         
         	INC       CONS.inc
         
           Include        MOANotesDD.inc

         	INC       NMOADD.INC
         	INCLUDE   NMOBDD.inc      	
         	INCLUDE   NPASDD.inc
         
	include	compdd.inc
	include	cntdd.inc
         
         INCLUDE   NDATDD.inc
               Include        ninvdd.inc
         
         include  norddd.inc
         
         INCLUDE   GNXTDD.inc
         liston
.Following used only in order to load Search.plf
        include ncmpdd.inc
        include nrtndd.inc
        include nowndd.inc
.Following key not used by program but required for search.plf
AKey2   init    "01F"
AKey3   init    "02F"
AKey1A  init    "01L"
AKey2A  init    "02L"
HoldMlr        Dim            4
HoldBRK        Dim            4
SelectTOtal    form           7.2
SelectCOunt    FOrm           5
.....................................................
release        init           "1.00"                  DLH	MOve comments to notes
Reldate        Init           "8 November 2007"

ACCOUNTD FILE      FIXED=128    jd turned off 7/3/99.
BR       FORM      2
CHANGE   FORM      7.2         CHANGE TO BE APPLIED TO BALANCE.
FORM7    FORM      7           NUMERIC WORK VARIABLE USED FOR ENTRY NUMBER
DATE     DIM       8
FMM      DIM       2           FILTER MONTH
FYY      DIM       2           FILTER YEAR
FCNUM    DIM       2           FILTER CONTROL NUMBER
MMFLAG   FORM       1           
YYFLAG   FORM       1
CNFLAG   FORM       1
.
.
. PASSWORD VARIABLES
.
PCODE    INIT      "c"
....MISC. ITEMS....................................
DateBranch  form    1       .'1=REcord date, 2=Trans date, 3=Control #
newdate1    dim    10      mm/dd/ccyy
newdate2    dim    10      mm/dd/ccyy
startdate   form      5
Enddate     form      5
MaskAmount  INIT     "$,$$$,$$9.99-"
okformore init     "Y"
DateFIlter       Dim            1         On if date filter
ControlFlag    dim            1         On if COntrol filter
str10a      dim     10
DateOkFlag dim     1
Nmoastopflag   Dim            1
colorfile file
white   color
grey    color
RED     COLOR
BLACK   COLOR
CCField        Dim            2
Str500	Dim	500
.
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
.
.Set Up Menu Bar
mFile    menu
.mEdit    menu
mHelp    menu
.Set Up SubMenu for Options
mOptions Menu
sColor  submenu
sSearch submenu
.
.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
.EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search-F2;-;&Color;"
HData   init    "&Help;&About"
.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To"
.Present Data for Colors SubMenu
CData   init    ";&Background;&Text"
.
endindex form      9
......Search  form       1
Timer   Timer
holdsInfo dim      36

.Define Collections for Object Colors
ColText Collection
ColBack Collection
.................................
coll1   	collection
specs   	form          4(4)
size    	form          "1.000"
infostring 	dim        590
Tabnum  	form          2
SaveTab 	form          2
ListViewNum   form          2

.Define Colors for Each Object
FTC     color
BGC     color

colornum dim   9(2)
Fred     form    3
Fgreen   form    3
Fblue    form    3
colorflag form   1
GRANDBALANCE   fORM           7.2
RunBAL         fORM           7.2
NMoaPassOk     Init           "N"
.............................................................................................................
.Set Vars used for About Box
        move    "NOna0001.PLS",Wprognme
        move    "Money on Account",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        move    Reldate to Wreldate
+..............................................................................
.MAIN
.Declare forms, Always declare child forms first
SRCH    plform  Search        
mss1    plform  Error
abt     plform  About
pss     plform  Passwrd
NMoaA plform  NMoa001A
NMoaB plform  NMoa001B
NMoaC plform  NMoa001C
x       plform  NMoa001
        winhide
.Load Forms, Always load parent form first
        formload x
        formload NMoac,NMoa0001
        formload NMoab,NMoa0001
        formload NMoaA,NMoa0001
        formload abt
        formload mss1
        formload SRCH
        formload pss

.Set tab index
        move    C2,TabNum
.
        
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        CREATE  NMoa0001;MFile,FData
        create  NMoa0001;mOptions,OData,mFile
        create  NMoa0001;mHelp,HData,mOptions
        CREATE  NMoa0001;sCOlor,Cdata,mOptions,1
.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
        activate mOptions
        activate mHelp,HelpGo,result
        activate sColor,ColorGo,result
.Create SubMenu
        create  NMoa0001;sSearch,SData,mOptions,1
.Activate SubMenus
        activate sSearch,SearchGo,result        
.set properties in collection
        listins ColText,NMoa0001,NMoa001EditText001,NMoa001EditText002:
                NMOA001cButtonAdd:
                NMoa001Mlrcomp:
                NMoa001bListView001:
                NMoa001brkcomp

        listins Colback,NMoa0001,NMoa001EditText001,NMoa001EditText002:
                NMOA001cButtonAdd: 
                NMoa001Mlrcomp:
                NMoa001bListView001:
                NMoa001brkcomp
.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic        
.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=*ltgray
        create  RED=*RED
        create  black=*black
.
.Open color file
opencolor
        trap    colorerror if io
        open    colorfile,"c:\progra~1\nincal\NMoa001.col"
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

         move      c1 to nordpath
         CLOCK     DATE TO DATE
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         MOVE      DATE TO TODAY
         OPEN      ACCOUNTD,"NINMOAD.DAT"
               setprop        NMOA001cButtonSave,Height=0
.Create NMoa001aListView002 Columns  by LR
.Column Clicking
        NMoa001aListView002.InsertColumn using "LR",50,1
        NMoa001aListView002.InsertColumn using "Record date",0,2
        NMoa001aListView002.InsertColumn using "Trans. Date",70,3
        NMoa001aListView002.InsertColumn using "Control",50,4
        NMoa001aListView002.InsertColumn using "Invoice",50,5
        NMoa001aListView002.InsertColumn using "Amount",75,6
        NMoa001aListView002.InsertColumn using "Check",70,7
        NMoa001aListView002.InsertColumn using "Reason",180,8
        NMoa001aListView002.InsertColumn using "Rec ##",45,9
        NMoa001aListView002.SetColumnFormat using 3,1              .set DAYS column justify right
        NMoa001aListView002.SetColumnFormat using 6,1              .set DAYS column justify right
.
.Create NMoa001aListView001 Columns by days
.Column Clicking
        NMoa001aListView001.InsertColumn using "Pseudo Days",0,1
        NMoa001aListView001.InsertColumn using "Record Date",0,2
        NMoa001aListView001.InsertColumn using "LR",50,3
        NMoa001aListView001.InsertColumn using "Trans. Date",70,4
        NMoa001aListView001.InsertColumn using "Control",50,5
        NMoa001aListView001.InsertColumn using "Invoice",50,6
        NMoa001aListView001.InsertColumn using "Amount",75,7
        NMoa001aListView001.InsertColumn using "Check",70,8
        NMoa001aListView001.InsertColumn using "Reason",180,9
        NMoa001aListView001.InsertColumn using "Balance",75,10
        NMoa001aListView001.InsertColumn using "Rec ##",45,11
        NMoa001aListView001.SetColumnFormat using 1,1              .set DAYS column justify right
        NMoa001aListView001.SetColumnFormat using 2,1              .set DAYS column justify right
        NMoa001aListView001.SetColumnFormat using 7,1              .set column justify right
.
.Create NMoa001aListView003 Columns by control
.Column Clicking
        NMoa001aListView003.InsertColumn using "Control",50,1
        NMoa001aListView003.InsertColumn using "LR",50,2
        NMoa001aListView003.InsertColumn using "Record Date",0,3
        NMoa001aListView003.InsertColumn using "Trans. Date",70,4
        NMoa001aListView003.InsertColumn using "Invoice",50,5
        NMoa001aListView003.InsertColumn using "Amount",75,6
        NMoa001aListView003.InsertColumn using "Check",70,7
        NMoa001aListView003.InsertColumn using "Reason",180,8
        NMoa001aListView003.InsertColumn using "Rec ##",45,9

        NMoa001aListView003.SetColumnFormat using 6,1              .set $ column justify right
.
.Create NMoa001aListView004 Columns by check
        NMoa001aListView004.InsertColumn using "Check",70,1
        NMoa001aListView004.InsertColumn using "LR",50,2
        NMoa001aListView004.InsertColumn using "Record Date",0,3
        NMoa001aListView004.InsertColumn using "Trans. Date",70,4
        NMoa001aListView004.InsertColumn using "Invoice",50,5
        NMoa001aListView004.InsertColumn using "Amount",75,6
        NMoa001aListView004.InsertColumn using "Control",50,7
        NMoa001aListView004.InsertColumn using "Reason",190,8
        NMoa001aListView004.InsertColumn using "Rec ##",45,9

        NMoa001aListView003.SetColumnFormat using 6,1              .set $ column justify right
.
        setitem NMoa001ComboBox001,0,c1

.Create NMoa001bListView001 Columns by Broker
        NMoa001bListView001.InsertColumn using "Broker",200,1
        NMoa001bListView001.InsertColumn using "Mailer",200,2
        NMoa001bListView001.InsertColumn using "Amount",75,3
        NMoa001bListView001.SetColumnFormat using 3,1              .set $ column justify right
.Create NMoa001bListView002 Columns by Broker
        NMoa001bListView002.InsertColumn using "Mailer",200,1
        NMoa001bListView002.InsertColumn using "Broker",200,2
        NMoa001bListView002.InsertColumn using "Amount",75,3
        NMoa001bListView002.SetColumnFormat using 3,1              .set $ column justify right

.Set Error Message Stat Text Boxes
        call    SetNMoaErrorMssgDefault
. .................................................................
Process	Loop
	call	Nmoaseq
	stop	if over
	call 	trim using ONACOM 
	if	(ONACOM <> "")
               PackKey        MOANotesFld,transnum
               call           MOANoteskey
               	if             Not over
	   	Clear		Str500
	   	call	Trim using Moanotes
	   	pack	str500,Onacom,NewLine,moanotes
	   	move	Str500,Moanotes
	   	call	MoaNotesUpd
               	Setitem        NMoa001cEditText013,0,MOANotes
               	else
	   	MOVe	Onacom,Moanotes
               	PackKey        MOANotesFld,transnum
	            PackKey        MOANotesKey,transnum
	   	call	MoaNotesWrt
               	Setitem        NMoa001cEditText013,0,MOANotes
               	endif
	Else
               	Setitem        NMoa001cEditText013,0," "
             endif  	
	repeat	




        move        c1 to n1
	call        NMoaTabChange
        setfocus NMoa001EditText001
        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat
        goto    timeout
..............................................................................................................
..........................................................................................................................
GETREAS        CLEAR          RDESC
               IF             (reason = 99)
               move           reas99 to rdesc
               else
               LOAD           RDESC FROM REASON OF REAS1,REAS2,REAS3,REAS4,REAS5:
                              REAS6,REAS7,REAS8,REAS9,REAS10,REAS11,REAS12,REAS13,REAS14:
.begin patch 3.41
.                              REAS15,REAS16,REAS17,REAS18,REAS19,REAS20,REAS21
                              REAS15,REAS16,REAS17,REAS18,REAS19,REAS20,REAS21,REAS22
.end patch 3.41
               endif
               RETURN
................................................................................
NOLIST   clear     OLSTNAME
         RETURN
................................................................................
INVREAD  MOVE      C2 TO NINVPATH
         MOVE      INVOICE TO NINVFLD
         MOVE      NO TO OVER
         CALL      NINVKEY
         GOTO      NOINV IF OVER
                   clear     invdate
         PACK      INVDATE FROM INVDTEC,INVDTEY,INVDTEM,INVDTED         
         MOVE      LRN TO LRNUM
         move      lrn to nordfld
         rep       zfill in nordfld
         call      nordkey
         move      olnum to list
         pack      str13 from invdtem,slash,invdted,slash,invdtec,invdtey
         setitem   NMoa001cEditText007,0,str13
         setitem   NMoa001cEditText006,0,olnum
         Setitem   NMoa001cEditText012,0,olstname
         RETURN
...............................................................................
NOINV    MOVE      YES TO OVER
         CLEAR     INVDATE
         CLEAR     LRNUM
         RETURN
...............................................................................
NOREC    DISPLAY   *P1:24,*EL,"NO RECORD FOUND",*B,*B,*B;
         RETURN
...............................................................................
NMoaForceToOne
            setitem NMoa001TabControl001,0,1
            move        c1 to n1
	call        NMoaTabChange

            return
                
...........................................................................................
NMoaTabClick
        IF (N1 = C1)
	  getprop NMoa001aListView001,visible=N9
                move    n9 to ListViewNum
                Deactivate NMoaa
        elseif (N1 = C2 )
                Deactivate NMoab
        setprop NMoa001aListView001,visible=ListViewNum
        call    NMoaSortListView
        elseif (N1 = C3 )
                Deactivate NMoac
        Endif
        return

NMoaTabChange
               Deactivate NMoaA
               Deactivate NMoaB
               Deactivate NMoaC
        IF (N1 = C1)
                move    C1,TabNum
                Activate NMoaa
                setfocus NMoa001aListView001
	  setprop NMoa001aListView001,visible=ListViewNum
	  call    NMoaSortListView
                        LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT

        elseif (N1 = C2)
	  getprop NMoa001aListView001,visible=N9
                move    n9 to ListViewNum
                move    C2,TabNum
                Activate NMoab
                setfocus NMoa001bListView001
.Prevent occurance or accumulated events which may place "hidden" objects on wrong form
.ie, ResetStatus Checkbox. This generally only happens with LostFocus events from Stats2.plf
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT

        elseif (N1 = C3)
                getprop NMoa001aListView001,visible=N9
                move    n9 to ListViewNum
                move    C3,TabNum
                Activate NMoaC
                setfocus NMoa001cEditText001
                LOOP
                        CLEAREVENT
                        UNTIL OVER
                REPEAT
        Endif
        return
.............................................................................................................
NMoaSortListView
.Dynamically sorts Different ListViews.  
.In order to switch between different ListViews we need two pieces of information.
.We need to ascertain which column was clicked AND which ListView we currently
.have visible, as each ListView has its' columns ordered differently.
.Getprops will determine which ListView is currently active, #EventResult passed to result
.prior to calling this subroutine will determine which column was clicked.
        getprop NMoa001aListView001,visible=N9
        if (N9 = C1)    .NMOA001aListView001 is visible
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                If (result <> 2 and result <> 4 and result <> 7)  .no click?
                        setprop NMoa001aListView001,visible=1
                        setprop NMoa001aListView003,visible=0
                        setprop NMoa001aListView002,visible=0
                        setprop NMoa001aListView004,visible=0
 	          NMoa001aListView001.EnsureVisible using c1,0
                        setfocus NMoa001alistview001
.                endif
.
.                if (result = 2)         LR
                Elseif (result = 2)         LR
                        setprop NMoa001aListView001,visible=0
                        setprop NMoa001aListView003,visible=0
                        setprop NMoa001aListView004,visible=0
                        setprop NMoa001aListView002,visible=1
	        NMoa001aListView002.EnsureVisible using c1,0
                        setfocus NMoa001alistview002
                Elseif (result = 4)  .clicked on Control
                        setprop NMoa001aListView001,visible=0
                        setprop NMoa001aListView003,visible=1
                        setprop NMoa001aListView004,visible=0
                        setprop NMoa001aListView002,visible=0
	        NMoa001aListView003.EnsureVisible using c1,0
                        setfocus NMoa001alistview003
                Elseif (result = 7)  .clicked on Check
                        setprop NMoa001aListView001,visible=0
                        setprop NMoa001aListView003,visible=0
                        setprop NMoa001aListView004,visible=1
                        setprop NMoa001aListView002,visible=0
	          NMoa001aListView004.EnsureVisible using c1,0
                        setfocus NMoa001alistview004
                endif
.            endif

                else
                getprop NMoa001aListView002,visible=N9
                if (N9 = C1)    .NMoa001aListView002 is visible
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
.begin patch - note we have a hidden pseudo day column that scews the count
               if (result <> 3 and result <> 2 and result <> 6)
                                setprop NMoa001aListView001,visible=0
                                setprop NMoa001aListView002,visible=1
                                setprop NMoa001aListView003,visible=0
                                setprop NMoa001aListView004,visible=0
	                  NMoa001aListView002.EnsureVisible using c1,0
	                  setfocus NMoa001alistview002
                        Elseif (result = 3 )
                                setprop NMoa001aListView001,visible=0
                                setprop NMoa001aListView002,visible=0
                                setprop NMoa001aListView003,visible=1
                                setprop NMoa001aListView004,visible=0
		  NMoa001aListView001.EnsureVisible using c1,0
	                        setfocus NMoa001alistview001
                        Elseif (result = 2 )
                                setprop NMoa001aListView001,visible=1
                                setprop NMoa001aListView002,visible=0
                                setprop NMoa001aListView003,visible=0
                                setprop NMoa001aListView004,visible=0
		        NMoa001aListView003.EnsureVisible using c1,0
	                        setfocus NMoa001alistview003
                        Elseif (result = 6 )
                                setprop NMoa001aListView001,visible=0
                                setprop NMoa001aListView002,visible=0
                                setprop NMoa001aListView003,visible=0
                                setprop NMoa001aListView004,visible=1
		        NMoa001aListView004.EnsureVisible using c1,0
	                        setfocus NMoa001alistview004
	                endif
          else
                getprop NMoa001aListView003,visible=N9
                if (N9 = C1)    .NMoa001aListView003 is visible

.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                      if (result <> 1 and result <> 3 and Result <> 6)
                                setprop NMoa001aListView001,visible=0
                                setprop NMoa001aListView002,visible=0
                                setprop NMoa001aListView003,visible=1
                                setprop NMoa001aListView004,visible=0
  	                  NMoa001aListView003.EnsureVisible using c1,0
	                  setfocus NMoa001alistview003
                        Elseif (result = 1 )   .want LR
                                setprop NMoa001aListView001,visible=0
                                setprop NMoa001aListView002,visible=1
                                setprop NMoa001aListView003,visible=0
                                setprop NMoa001aListView004,visible=0
		  NMoa001aListView002.EnsureVisible using c1,0
	                 setfocus NMoa001alistview002
                        Elseif (result = 3 )      
                                setprop NMoa001aListView001,visible=1
                                setprop NMoa001aListView002,visible=0
                                setprop NMoa001aListView003,visible=0
                                setprop NMoa001aListView004,visible=0
		  NMoa001aListView001.EnsureVisible using c1,0
	                 setfocus NMoa001alistview001
                        Elseif (result = 6 )      
                                setprop NMoa001aListView001,visible=0
                                setprop NMoa001aListView002,visible=0
                                setprop NMoa001aListView003,visible=0
                                setprop NMoa001aListView004,visible=1
		  NMoa001aListView004.EnsureVisible using c1,0
	                 setfocus NMoa001alistview004
	                endif
.                endif
               else
                getprop NMoa001aListView004,visible=N9
                if (N9 = C1)    .NMoa001aListView004 is visible
.               call debug
.if (result = 0) then user clicked first column, which is equal to currently viewed ListView, so do nothing!
                      if (result <> 1 and result <> 3 and result <> 6)
                                setprop NMoa001aListView001,visible=0
                                setprop NMoa001aListView002,visible=0
                                setprop NMoa001aListView003,visible=0
                                setprop NMoa001aListView004,visible=1
  	                  NMoa001aListView004.EnsureVisible using c1,0
	                  setfocus NMoa001alistview004
                        Elseif (result = 1 )   .want lr order
                                setprop NMoa001aListView001,visible=0
                                setprop NMoa001aListView002,visible=1
                                setprop NMoa001aListView003,visible=0
                                setprop NMoa001aListView004,visible=0
		  NMoa001aListView002.EnsureVisible using c1,0
                               setfocus NMoa001alistview002
                        Elseif (result = 3 )   
                                setprop NMoa001aListView001,visible=1
                                setprop NMoa001aListView002,visible=0
                                setprop NMoa001aListView003,visible=0
                                setprop NMoa001aListView004,visible=0
		  NMoa001aListView001.EnsureVisible using c1,0
	                setfocus NMoa001alistview001
                        Elseif (result = 6 )   
                                setprop NMoa001aListView001,visible=0
                                setprop NMoa001aListView002,visible=0
                                setprop NMoa001aListView003,visible=1
                                setprop NMoa001aListView004,visible=0
		        NMoa001aListView003.EnsureVisible using c1,0
	                        setfocus NMoa001alistview003
	                endif
                endif
        endif
        endif
        endif
        return
.......................................................................................................
VerifyMoaData
              SetProp         NMoa001CButtonSave,height=0
              SetProp         NMoa001ExitButton,height=0
              Getitem         NMoa001EditText001,0,str10
              call            TRIM using str10
              MOVE            STR10 TO STR4
              if              (str10="")
                        alert   caution,"Mailer Cannot be a null value!",result,"Bad MLR"
                        setfocus    NMoa001EditText001
                        CLEAREVENT
                        goto       VerifMoaFailed
              endif
               packkey        Mkey from str4,z3
               call           nmlrkey
               IF             over              
                        alert   caution,"Mailer Is Required!",result,"Bad MLR"
                        setfocus    NMoa001EditText001
                        CLEAREVENT
               GOto           VerifMoaFailed
              endif
              Move            str4 to Mlr
              move            z3 to Mcnt
.end mailer
              Getitem         NMoa001EditText002,0,str10
              call            TRIM using str10
              MOVE            STR10 TO STR4
              clear           Nmoabrk
              move            str4 to Nmoabrk
              rep             Zfill in Nmoabrk
              move            Nmoabrk to nmobbrk
.end Broker
.check number
               Getitem         NMoa001cEditText010,0,checknum
.amount
               Getitem         NMoa001cEditText008,0,str13
               move            str13 to Onamount
               if              (onamount = c0)
               alert           caution,"Amount is Required!",result,"Amount"
               setfocus        NMoa001cEditText008
               CLEAREVENT
               GOto           VerifMoaFailed
               endif
               move            Onamount to change
               move           MaskAmount to Str13
               edit           onamount to str13
               setitem         NMoa001cEditText008,0,str13
.comments
               Getitem        NMoa001cEditText009,0,ONACOM
.reason
               getitem        NMoa001cComboBox001,0,n2
               Sub            c1 from n2                               ;offset
               move           n2 to reason
               if             (n2 = 23)
               move           "99" to reason
               endif
.begin patch 3.41
.               if      (Reason < 1 or Reason > 21 and reason <> 99)
               if      (Reason < 1 or Reason > 22 and reason <> 99)
.end patch 3.41
                        alert   caution,"Reason Code Required!",result,"Bad Code"
                        setfocus    NMoa001cComboBox001
                        CLEAREVENT
                         GOto           VerifMoaFailed
              endif
.trans date

               getitem NMoa001CEditText001,0,str10
              call    RemoveChar using str10,slash
              call    TRIM using str10
              MOVE    STR10 TO STR8
              if      (str10="")
                        alert   caution,"Date Cannot be a null value!",result,"Bad Date"
                        setfocus    NMoa001CEditText001
                        CLEAREVENT
                          GOto           VerifMoaFailed
              endif
              CALL    ZFILLIT USING STR8
              if      (str8="00000000")
                        alert   caution,"Date Cannot be a null value!",result,"Bad Date"
                        setfocus    NMoa001CEditText001
                        CLEAREVENT
                        GOto           VerifMoaFailed
              endif
              count   N2,str10
              if (N2 = 0)
                        clear   MM
                        clear   DD
                        clear   CCField
                        clear   YY
                        alert   caution,"Date Must be in MMDDCCYY Format",result
                        setfocus NMoa001CEditText001
                        ClearEvent
                         GOto           VerifMoaFailed
                else
                        if (N2 = 10)
                                unpack  str10,MM,str1,DD,str1,CCField,YY
                        elseif (N2 = 8)
                                unpack  str10,MM,DD,CCField,YY
                        elseif (N2 <> 0)
                                alert   caution,"Date Must be in MMDDCCYY Format",result
                                setfocus NMoa001CEditText001
                                ClearEvent
                                  GOto           VerifMoaFailed
                endif
                move    MM,N2
                if (N2 > "12")
                                alert   caution,"Invalid Month!",result
                                setfocus NMoa001CEditText001
                                ClearEvent
               GOto           VerifMoaFailed
                else
                                Load    NDD using N2,c31,c28,c31,c30,c31,c30,c31,c31,c30,c31,c30,c31
                                move    DD,N2
                                if (N2 > NDD)
                                                alert   caution,"Invalid Day!",result
                                                setfocus NMoa001CEditText001
                                                ClearEvent
                                                 GOto           VerifMoaFailed

                                else
                                                move    CCField,N2
                                if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                                alert   caution,"Invalid Year!",result
                                                setfocus NMoa001CEditText001
                                                ClearEvent
                                                 GOto           VerifMoaFailed

                                 elseif (N2 = "19")
                                                 move    YY,N2
                                                 if (N2 < "80")
                                                        alert   caution,"Invalid Year!",result
                                                        setfocus NMoa001CEditText001
                                                        ClearEvent
                                                        GOto           VerifMoaFailed
                                                endif
                                        endif
                                endif
                        endif
                endif
                call    TRIM using MM
                count   N2,MM
                if (N2 <> 0 AND MM <> "00")
                        pack    NewDate1,MM,SLASH,DD,SLASH,CCField,YY
                else
                        clear   Newdate1
                endif
                setitem NMoa001CEditText001,0,NewDate1
                PACK      TRANDATE FROM CCField,YY,MM,DD
.end transaction date
               unpack    today into mm,str1,dd,str1,yy
               PACK      recDATE FROM CC,YY,MM,DD
               pack     NewDate1,MM,SLASH,DD,SLASH,CC,YY
               setitem  NMoa001CEditText002,0,NewDate1
.end date record entered
.get control
               clear          str3
               Getitem        NMoa001cEditText003,0,str3
               clear          Control
               type           str3
               if             equal
		getitem		NMoa001cEditText003,0,str3
		call		Trim using str3
		if (str3 <> "")
				call	ZFillIt using str3,C0
		endif
               move           str3 to control
               elseIF         (str3 <> "" & str3 <> "   ")
               alert          caution,"Control must be numeric!",result
               setfocus       NMoa001CEditText003
               ClearEvent
               GOto           VerifMoaFailed
               endif
.get LR
               clear          str6
               Getitem        NMoa001cEditText005,0,str6
               clear          LRnum
               type           str6
               if             equal
               move           str6 to Lrnum
               elseIF         (str6 <> "" & str6 <> "      ")
               alert          caution,"LR must be numeric!",result
               setfocus       NMoa001CEditText005
               ClearEvent
               GOto           VerifMoaFailed
               endif
.get Invoice
               clear          str6
               Getitem        NMoa001cEditText004,0,str6
               clear          invoice
               type           str6
               if             equal
               move           str6 to invoice
               elseIf         (str6 <> "" & str6 <> "      ")
               alert          caution,"Invoice must be numeric!",result
               setfocus       NMoa001CEditText006
               ClearEvent
               GOto           VerifMoaFailed
               endif
               MOVE           INITS TO NMOAINIT

............................................................................
.all verified lets write data.
.
DAVE
               PACK      NMObFLD FROM MLR
               pack      nmoafld from mlr,mcnt
               if        (Nmobbrk = "" or Nmobbrk = "   ")
               move           "0000" to NmobBrk
               move           "0000" to NmoaBrk
               endif
               packkey      nmoafld4 from nmobbrk,mlr
               move      c2 to nmobpath
               CALL      NMOBUPD
               UNPACK    DATE INTO MM,STR1,DD,STR1,YY
               PACK      RECDATE FROM CC,YY,MM,DD
               MOVE      "NONANXT" TO GNXTFLD
               CALL      GNXTKEY
               MOVE      GNXTNUM TO FORM7
ADDONEMOA      ADD       C1 TO FORM7
               MOVE      FORM7 TO TRANSNUM
               REP       ZFILL IN TRANSNUM
               MOVE      TRANSNUM TO NMOAFLD
               MOVE      C2 TO NMOAPATH
               CALL      NMOATST
               GOTO      ADDONEMoa IF NOT OVER
               MOVE      FORM7 TO STR7
               BUMP      STR7 BY 1
               MOVE      STR7 TO GNXTNUM
               REP       ZFILL IN GNXTNUM
               CALL      GNXTUPD
.NOTE GNXT ONLY ALLOWS 6 BYTE NUMBER. DLH.
               PACK      NMObFLD FROM MLR
               pack      nmoafld from mlr,mcnt
               PACK      NMOAFLD4 FROM nmoabrk,MLR
               MOVE      C1 TO NMOAPATH
               MOVE      INITS TO NMOAINIT
               if             (lrnum = "")
               move           "      " to lrnum
               endif
               CALL      NMOAWRT
               FILEPI    2;ACCOUNTD
               WRITE     ACCOUNTD,SEQEOF;MLR,MCNT,ENTRY,MBILLTO,TRANDATE,CONTROL:
                              INVOICE,LRNUM,INVDATE,ONAMOUNT,RECDATE,INAMOUNT:
                              ONACOM,REASON,LIST,CHECKNUM,TRANSNUM,nmoabrk
               weof      accountd,seqeof

               setprop        NMOA001cButtonAdd,Height=20
               setprop        NMOA001cButtonSave,Height=0
               SetProp        NMoa001ExitButton,Height=20
               alert          Note,"Record added !",result

               return
.......................................................................................................
VerifMoaFailed
               setprop        NMOA001cButtonSave,Height=20
               SetProp        NMoa001ExitButton,Height=20
               Return
.......................................................................................................
FileGo
                winshow
                stop

NMoaXRefKeyPress
        if (N9 = 113) .F2 Key calls Search Function
.Virtual Key Value
                goto SearchGo3
        elseif (N9 = 120)     .F9 Key closes Search Function
                setprop Search,visible=0
        endif
        return
SearchGo
        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4
SearchGo1
.BROKER
        move    C1,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo2        
.LIST
        move    C2,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo3        
.MAILER
        move    C3,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo4
.SHIP-TO - not an option with this program
        move    C4,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchLoad
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4
SearchLoad1
.BROKER
        unpack  Srchstr,str4,str1,str3,str1,str45
        setitem  NMoa001EditText002,0,str4
        setitem  NMoa001BRkcomp,0,str45
        setfocus NMoa001EditText002
        return
SearchLoad2
.LIST - not an option with this program
        return
SearchLoad3
.MAILER
        unpack  Srchstr,str4,str1,str3,str1,str45
        setitem NMoa001EditText001,0,str4
        setitem NMoa001Mlrcomp,0,str45
        setfocus NMoa001EditText001
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
.

HelpGo
        setprop AboutMssg,visible=1
        return       
.debug   return
...........................................................................................................
ClearNMoaSearchList
.Clear ListView
               Nmoa001aListView001.DeleteAllItems giving N9
               Nmoa001aListView002.DeleteAllItems giving N9
               NMoa001aListView003.DeleteAllItems giving N9
               Nmoa001bListView001.DeleteAllItems giving N9
               Nmoa001bListView002.DeleteAllItems giving N9
               mOVE           C0 TO GrandBalance
               mOVE           C0 TO SelectTotal
               mOVE           C0 TO Selectcount
               mOVE           C0 TO Runbal
               setitem        NMOA001aEditText001,0,""
               setitem        NMOA001aEditText002,0,""
               setprop        NMOA001cButtonAdd,Height=20
               setprop        NMOA001cButtonSave,Height=0

.
.
.
        clear        str5
               clear    str14
               return
...........................................................................................................
MoaDetailClear
               setitem    Nmoa001cEditText001,0," "
               setitem    Nmoa001cEditText002,0," "
               setitem    Nmoa001cEditText003,0," "
               setitem    Nmoa001cEditText004,0," "
               setitem    Nmoa001cEditText005,0," "
               setitem    Nmoa001cEditText006,0," "
               setitem    Nmoa001cEditText007,0," "
               setitem    Nmoa001cEditText008,0," "
               setitem    Nmoa001cEditText009,0," "
               setitem    Nmoa001cEditText010,0," "
               setitem    Nmoa001cEditText011,0," "
               setitem    Nmoa001cEditText012,0," "
               Setitem        NMoa001cEditText013,0,""
               return
..........................................................................................................
getmailer
	 getitem   Nmoa001EditText001,0,str4
	 call      trim using str4

	 count     n2 in str4
	 if        (n2 = c3)
	 move      str4 to str3
	 pack      str4 from c0,str3
	 goto      buildmkey
	 endif
	 if        (n2 = c2)
	 move      str4 to str2
	 pack      str4 from c0,c0,str2
	 goto      buildmkey
	 endif
	 if        (n2 = c1)
	 move      str4 to str1
	 pack      str4 from c0,c0,c0,str1
	 goto      buildmkey
	 endif

buildmkey
	 setitem   Nmoa001EditText001,0,str4
	 clear     mkey
	 packkey   mkey from str4,z3
	 clear     mnum
	 call      nmlrkey
	 if        not over
	 setitem   Nmoa001Mlrcomp,0,Mcomp
         else
	 setitem   Nmoa001Mlrcomp,0," "
	 endif
         return
..........................................................................................................
GetBroker
	 getitem   NMoa001EditText002,0,str4

	 call      trim using str4
	 count     n2 in str4
	 if        (n2 = c3)
	 move      str4 to str3
	 pack      str4 from c0,str3
	 goto      buildbrkfld
	 endif
	 if        (n2 = c2)
	 move      str4 to str2
	 pack      str4 from c0,c0,str2
	 goto      buildbrkfld
	 endif
	 if        (n2 = c1)
	 move      str4 to str1
	 pack      str4 from c0,c0,c0,str1
	 goto      buildbrkfld
	 endif

buildbrkfld
	 setitem   NMoa001EditText002,0,str4
	 clear     nbrkfld
	 packkey   nbrkfld from str4,z3
	 clear     brknum
	 call      nbrkkey
	 if        not over
	 setitem   Nmoa001BRkcomp,0,BRcomp
         else
	 setitem   Nmoa001BRkcomp,0," "
	 endif
         return
..............................................................................................................
.SetNMoaDates - check for date goodies
SetNMoaDates
.
        move    yes to dateokflag
        clear   mm
        clear   dd
        clear   yy
        clear   newdate1
        clear   newdate2
        getitem NMoa001ComboBox001,0,result
        move    result to datebranch
               If             (DateBranch = c4)
               goto                  SetNMoaControl
               endif
               If             (DateBranch > 0 & DateBranch < c4)
               MOve           "T" to DateFilter
               endif

        getitem NMoa001EditDate1,0,str10
        clear   mm
        clear   dd
        clear   str2
        clear   yy
        call    TRIM using str10
        count   N2,str10
        if (N2 = 10)
                unpack  str10,MM,str1,DD,str1,STR2,YY
        elseif (N2 = 8)
                unpack  str10,MM,DD,STR2,YY
        elseif (N2 <> 0)
                alert   caution,"Date Must be in MMDDCCYY Format",result
		setFocus NMoa001EditDate1
                goto BadDate
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
		setFocus NMoa001EditDate1
                goto BadDate
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
			setFocus NMoa001EditDate1
	                goto BadDate
                else
                        move    STR2,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
				setFocus NMoa001EditDate1
		                goto BadDate
                        endif
                endif
        endif
        call    TRIM using MM
        count   N2,MM
        if (N2 <> 0 AND MM <> "00")
                pack    newdate1,MM,SLASH,DD,SLASH,STR2,YY
        else
                clear   newdate1
        endif
        setitem NMoa001EditDate1,0,newdate1
.
        clear   mm
        clear   dd
        clear   yy
        clear   str2
        clear   newdate2
        getitem NMoa001EditDate2,0,str10
        call    TRIM using str10
        count   N2,str10
        if (N2 = 10)
                unpack  str10,MM,str1,DD,str1,STR2,YY
        elseif (N2 = 8)
                unpack  str10,MM,DD,STR2,YY
        elseif (N2 <> 0)
                alert   caution,"Date Must be in MMDDCCYY Format",result
		setFocus NMoa001EditDate2
                goto BadDate
        endif
        move    MM,N2
        if (N2 > "12")
                alert   caution,"Invalid Month!",result
		setFocus NMoa001EditDate2
                goto BadDate
        else
                move    DD,N2
                if (N2 > "31")
                        alert   caution,"Invalid Day!",result
	 		setFocus NMoa001EditDate2
                        goto BadDate
                else
                        move    STR2,N2
                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                alert   caution,"Invalid Year!",result
				setFocus NMoa001EditDate2
                                goto BadDate
                        endif
                endif
        endif
        call    TRIM using MM
        count   N2,MM
        if (N2 <> 0 AND MM <> "00")
                pack    NewDate2,MM,SLASH,DD,SLASH,STR2,YY
        else
                clear   NewDate2
        endif
        setitem NMoa001EditDate2,0,NewDate2
        if      (Newdate1  <= b1 and Newdate2 <= b1)
        move    c0 to datebranch
        endif
        move    c0 to startdate
        move    c0 to enddate
        if      (newdate1 > b1)
        unpack  newdate1 into mm,str1,dd,str1,str2,yy
        call    cvtjul
        move    juldays to startdate
        endif
        if      (newdate2 > b1)
        unpack  newdate2 into mm,str1,dd,str1,str2,yy
        call    cvtjul
        move    juldays to enddate
        endif
        if     ((startdate > 0 or EndDate > 0)& (DateBranch < 1 & DateBranch > 3))     .dates entered no date type
                                alert   caution,"Specify Date type!",result
                                setfocus  NMoa001ComboBox001
                                goto BadDate
        else
        endif
        return

BadDate
               move           No to dateokflag
.               setprop        NMoa001GoButton,visible=1
.               setprop        NMoa001Stop,visible=0
               Move           "F" to DateFilter
        return
SetNMoaControl
               getitem NMoa001EditDate1,0,str10
               call    TRIM using str10
               count   N2,str10
               IF             ((N2 > 3)or (N2 <> 0))
               alert   caution,"Control Must be in 999 Format",result
	setFocus NMoa001EditDate1
               goto BadControl
               Else           
               Move           Str10 to Fcnum
               endif
               getitem NMoa001EditDate2,0,str10
               call    TRIM using str10
               count   N2,str10
               IF             ((N2 > 3)or (N2 <> 0))
               alert   caution,"Control Must be in 999 Format",result
	setFocus NMoa001EditDate2
               goto BadControl
               Else           
               Move           Str10 to Fcnum
               endif
               If             (FCNUm = "0")
               alert   caution,"Control Must be > ZERO",result
	setFocus NMoa001EditDate1
               goto BadControl
               endif
               Move           "T" to ControlFlag
               return
BadControl
               move           No to dateokflag
               Move           "F" to ControlFlag
               return
.........................................................................................................
.LoadNmoaSearchList - sort out keys being used and go to it
LoadNMoaSearchList
               setprop NMoa001aListView001,visible=1
               setprop NMoa001aListView003,visible=0
               setprop NMoa001aListView002,visible=0
               setprop NMoa001aListView004,visible=0
               NMoa001aListView001.EnsureVisible using c1,0
               setfocus NMoa001alistview001
               call           SetNMoaDates
               Clear          Mlr
               CLear          NMoaBrk
               Getitem        NMoa001EditText001,0,Mlr
               Getitem        NMoa001EditText002,0,NMoabrk
               Move           mlr to holdmlr
               move           Nmoabrk to holdbrk
               if             (Nmoabrk = "" or Nmoabrk = "    ")
               move           c1 to nmoapath
               else
               packKey        nmoafld4 from nmoabrk,mlr
               REP            ZFILL IN NMOAFLD4
               MOVE           C4 TO NMOAPATH
               endif
               PACKKey        NMObFLD FROM MLR
.               PACKKey        NMOAFLD FROM MLR,"000"
               PACK          NMOAFLD FROM MLR
.               REP            ZFILL IN NMOAFLD
               if             (Nmoapath = c4)
               call           Nmoakey 
               else
               CALL           NMOAtst
               endif
               If             Not Over
               call           CheckFilters
               goto           NMoasearchlistloop
               else
                              IF             (Nmoapath = c4)
                              goto           NmoaSearchListLoop
                              endif
.               goto           ExitNMoaSearchList
               endif
NMoaSearchListLoop
               Loop
               call           NMoaKS
               If             Not Over
               Move           Yes to OKforMore
               Call           CheckFilters
               Else
               MOve           No to OKForMore
               endif
               UNtil          (OKForMore = No)
               Repeat
.ok when done with details load up summary  If search by Mailer or brk/mailer we want totals for all accounts the mlr
. may have
.if by broker only all totals for all accounts
NMobSearchListLoop
               unpack         nmoafld into mlr,str3
               if             (nmoapath = c1)    ;mailer only
               move           c1 to nmobpath
               packkey        nmobfld from mlr
               Elseif          (nmoapath = c4 & mlr = "0000")   ;broker only
               move           c2 to nmobpath
               Else                                         ;broker only nmoafld4 should be good from above
               move           c1 to nmobpath
               packkey        nmobfld from mlr
               endif

               move           no to NmobMsgFlag

               call           nmobkey
                              if             not over
                              call           LoadSummaryListView
                              endif
              loop
              call            nmobks
                             if              not over
                             if              (mlr = Nmobmlr & Nmobpath = c1)
                             move            yes to okformore
                             call            loadSummaryListView
                             Elseif         (HOldbrk = Nmobbrk & Nmobpath = c2)
                             move            yes to okformore
                             call            loadSummaryListView
                             endif
                             move            No to OkForMore
                             else            
                             move            No to OkForMore
                             endif
              Until          (OkForMore = No)
              repeat
               move           MaskAmount to Str13
               edit           GrandBalance to str13
               Setitem        NMoa001bEditText001,0,str13
              return
...........................................................................................................
loadSummaryListView
               Clear          Mcomp
               packkey        Mkey from nmobMLR,z3
               call           nmlrkey
               clear          brcomp
               if             (Nmobbrk <> "" & Nmobbrk <> "0000")
               packkey        Nbrkfld from nmobbrk,z3
               call           nbrkkey
               endif
               move           MaskAmount to Str13
               edit           Balance to str13
               NMoa001bListView001.InsertItem giving N9 using brcomp
               NMoa001bListView001.SetItemText using N9,mcomp,1
               NMoa001bListView001.SetItemText using N9,str13,2
               NMoa001bListView001.SetColumnFormat using 5,2              .set $ column justify right
.
               NMoa001bListView002.InsertItem giving N9 using Mcomp
               NMoa001bListView002.SetItemText using N9,brcomp,1
               NMoa001bListView002.SetItemText using N9,str13,2
               NMoa001bListView002.SetColumnFormat using 5,2              .set $ column justify right
.add grand total goodies
               MULT           BALANCE BY seq
               ADD            BALANCE TO GRANDBALANCE
               return
...........................................................................................................
CheckFilters
               if             (lrnum = "493279")
               call           debug
               endif
               if             (Nmoapath = c1)
               match          "0000" to Nmoabrk
               return         if not equal
               match          mlr to holdmlr
               return         if not equal
               endif
               if             (Nmoapath = c4)
                              IF             (holdmlr <> "" and mlr <> Holdmlr)
                              return         
                              endif
.                              IF             (holdbrk <> "" or  Nmoabrk <> holdbrk)
                              IF             (Nmoabrk <> holdbrk)
                              return
                              endif
               endif
               if             (DateBranch = c2)
                              UNPACK         REcDATE INTO STR2,YY,MM,DD
                              Call           cvtjul
                              if          (startdate > 0 and enddate > 0)
                                              if       (startdate <= juldays and juldays <= enddate)
                                              else
                                              Goto      ExitCheckFilters     
                                              endif
                              endif
               Elseif        (DateBranch = c3)
                              UNPACK         TRANDATE INTO STR2,YY,MM,DD
                              Call           cvtjul
                              if          (startdate > 0 and enddate > 0)
                                              if       (startdate <= juldays and juldays <= enddate)
                                              else
                                              Goto      ExitCheckFilters     
                                              endif
                              endif
               elseif         (DateBranch = c4 & Fcnum <> control)
                              goto           ExitCheckFilters
               endif
.ok passed filters or were none load up objects
               move           MaskAmount to Str13
               edit           onamount to str13
               add            onamount to Runbal
               Call           Getreas
               NMoa001aListView002.InsertItem giving N9 using Lrnum
               clear     str10
               UNpack    REcdate into str2,yy,mm,dd
               pack      str10 from mm,slash,dd,slash,str2,yy
               NMoa001aListView002.SetItemText using N9,str10,1
               clear     str10
               UNpack    Trandate into str2,yy,mm,dd
               pack      str10 from mm,slash,dd,slash,str2,yy
               NMoa001aListView002.SetItemText using N9,str10,2
               NMoa001aListView002.SetItemText using N9,Control,3
               NMoa001aListView002.SetItemText using N9,Invoice,4
               NMoa001aListView002.SetItemText using N9,str13,5
               NMoa001aListView002.SetItemText using N9,Checknum,6
               NMoa001aListView002.SetItemText using N9,Rdesc,7
               NMoa001aListView002.SetItemText using N9,TRANSNUM,8
               NMoa001aListView002.SetColumnFormat using 5,1              .set $ column justify right
.
               clear     str10
               UNpack    REcdate into str2,yy,mm,dd
               pack      str10 from mm,slash,dd,slash,str2,yy
.               NMoa001aListView001.InsertItem giving N9 using REcdate
               NMoa001aListView001.InsertItem giving N9 using transnum
               NMoa001aListView001.SetItemText using N9,str10,1
               NMoa001aListView001.SetItemText using N9,LRnum,2
               clear     str10
               UNpack    Trandate into str2,yy,mm,dd
               pack      str10 from mm,slash,dd,slash,str2,yy
               NMoa001aListView001.SetItemText using N9,str10,3
               NMoa001aListView001.SetItemText using N9,Control,4
               NMoa001aListView001.SetItemText using N9,Invoice,5
               move           MaskAmount to Str13
               edit           onamount to str13
               NMoa001aListView001.SetItemText using N9,str13,6
               NMoa001aListView001.SetItemText using N9,Checknum,7
               NMoa001aListView001.SetItemText using N9,Rdesc,8
               move           MaskAmount to Str13
               edit           runbal to str13
               NMoa001aListView001.SetItemText using N9,str13,9
               NMoa001aListView001.SetItemText using N9,TRANSNUM,10
               NMoa001aListView001.SetColumnFormat using 6,1              .set $ column justify right
.
               NMoa001aListView003.InsertItem giving N9 using Control
               NMoa001aListView003.SetItemText using N9,Lrnum,1
               clear     str10
               UNpack    REcdate into str2,yy,mm,dd
               pack      str10 from mm,slash,dd,slash,str2,yy
               NMoa001aListView003.SetItemText using N9,str10,2
               clear     str10
               UNpack    Trandate into str2,yy,mm,dd
               pack      str10 from mm,slash,dd,slash,str2,yy
               NMoa001aListView003.SetItemText using N9,str10,3
               NMoa001aListView003.SetItemText using N9,Invoice,4
               move           MaskAmount to Str13
               edit           onamount to str13
               NMoa001aListView003.SetItemText using N9,str13,5
               NMoa001aListView003.SetItemText using N9,Checknum,6
               NMoa001aListView003.SetItemText using N9,Rdesc,7
               NMoa001aListView003.SetItemText using N9,TRANSNUM,8
               NMoa001aListView003.SetColumnFormat using 5,1              .set $ column justify right
.
               NMoa001aListView004.InsertItem giving N9 using Checknum
               NMoa001aListView004.SetItemText using N9,Lrnum,1
               clear     str10
               UNpack    REcdate into str2,yy,mm,dd
               pack      str10 from mm,slash,dd,slash,str2,yy
               NMoa001aListView004.SetItemText using N9,str10,2
               clear     str10
               UNpack    Trandate into str2,yy,mm,dd
               pack      str10 from mm,slash,dd,slash,str2,yy
               NMoa001aListView004.SetItemText using N9,str10,3
               NMoa001aListView004.SetItemText using N9,Invoice,4
               move           MaskAmount to Str13
               edit           onamount to str13
               NMoa001aListView004.SetItemText using N9,str13,5
               NMoa001aListView004.SetItemText using N9,Control,6
               NMoa001aListView004.SetItemText using N9,Rdesc,7
               NMoa001aListView004.SetItemText using N9,TRANSNUM,8
               NMoa001aListView004.SetColumnFormat using 5,1              .set $ column justify right
.
               add            c1 to selectcount
               move           selectcount to str5
               setitem        NMOA001aEditText002,0,str5
               add            Onamount to SelectTotal
               move           MaskAmount to Str13
               edit           SelectTotal to str13
               setitem        NMOA001aEditText001,0,str13
ExitCheckFilters
               return
.
ExitNMoaSearchList
               setprop        NMOA001cButtonAdd,Height=20
               setprop        NMOA001cButtonSave,Height=0
               Return
..............................................................................................................
..NMoaSetFocusTab
NMoaSetFocusTab
         setitem     NMoa001TabControl001,0,n1
         setfocus    NMoa001TabControl001,1
         return
.........................................................................................................
DisplayMoaDetail
               call           Nmoakey
               UNpack         Trandate into str2,yy,mm,dd
               clear          str10
               pack           str10 from mm,slash,dd,slash,str2,yy
               setitem        NMoa001cEditText001,0,str10
               setitem        NMoa001cEditText003,0,control
               setitem        NMoa001cEditText005,0,LRnum
               setitem        NMoa001cEditText004,0,Invoice
               Setitem        NMoa001cEditText009,0,ONACOM
               TYPE           INVOICE
               if             equal
               move           c2 to ninvpath
               CALL           INVREAD               
               clear          str10
               pack           str10 from invdtem,slash,invdted,slash,invdtec,invdtey
               setitem        NMoa001cEditText007,0,str10
               Else
               setitem        NMoa001cEditText007,0," "
               setitem        NMoa001cEditText004,0," "
               Type           Lrnum
               if             Equal
                              move      lrnum to nordfld
                              rep       zfill in nordfld
                              call      nordkey
                              endif
               endif
               setitem        NMoa001cEditText006,0,List
               MOVE           LIST TO ndatfld
               move           c1 to ndatpath
               rep            zfill in ndatfld
               CMATCH         B1 TO ndatfld
               call           nolist
               CALL           NDATkey IF NOT Eos
               setitem        NMoa001cEditText012,0,Olstname
               move           MaskAmount to Str13
               edit           onamount to str13
               setitem        NMoa001cEditText010,0,checknum               ;check
               setitem        NMoa001cEditText008,0,str13                  ;amount
               move           reason to n2
               add            c1 to n2                               ;offset
               if             (reason = "99")
.begin patch 3.41
.               move           "22" to n2
               move           "23" to n2
.end patch 3.41
               endif
               setitem        NMoa001cComboBox001,0,n2
               setitem        NMoa001cEditText011,0,transnum
               clear          str10
               UNpack         REcdate into str2,yy,mm,dd
               pack           str10 from mm,slash,dd,slash,str2,yy
               setitem        NMoa001cEditText002,0,str10
                getprop       NMoa001aListView001,visible=N9
                move          n9 to ListViewNum
                setfocus      NMoa001cEditText001
               PackKey        MOANotesFld,transnum
               call           MOANoteskey
               if             Not over
               Setitem        NMoa001cEditText013,0,MOANotes
               else
               Setitem        NMoa001cEditText013,0,""
               endif
              return
.........................................................................................................
NMoaDetAdd     UNpack         Today into mm,str1,dd,str1,yy
               pack     str10 from MM,SLASH,DD,SLASH,CC,YY
               setitem  NMoa001CEditText001,0,str10

               Return
GetDetailDatainv
               call           Invread
               return
.........................................................................................................
SetNMoaErrorMssgDefault
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=1
        setprop ErrorMssgStat4,visible=1
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0,"Enter 4 Digit Mailer Number:"
        setitem ErrorMssgStat2,0,""
        setitem ErrorMssgStat3,0,"    Or hit F2 to Search"
        setitem ErrorMssgStat4,0,"      By Company Name"
        setitem ErrorMssgStat5,0,"      That Mailer Does Not Exist!"
        setitem ErrorMssgOK,0,"&OK"
        return
.........................................................................................................
GetOrderInfo
               rep            zfill in nordfld
               MOVE           C1 TO NordPATH
               call           nordkey
               move           olnum to list
               move           Olrn to LRnum
               setitem        NMoa001cEditText006,0,olnum
               packkey        Ndatfld from olnum
               move           c1 to ndatpath
               call           Ndatkey
               Setitem        NMoa001cEditText012,0,olstname
               MOve           Olnum to List
               MOVE           C1 TO NINVPATH
               packkey        Ninvfld from olrn
               CALL           NINVKEY
               If             over
               clear          Invoice
               Clear          Invdate
               setitem        NMoa001cEditText007,0,""
               setitem        NMoa001cEditText004,0,""
               else
               PACK           INVDATE FROM INVDTEC,INVDTEY,INVDTEM,INVDTED         
               pack           str13 from invdtem,slash,invdted,slash,invdtec,invdtey
               setitem        NMoa001cEditText007,0,str13
               setitem        Nmoa001CEditText004,0,Invnum
               move           Invnum to Invoice
               endif
               setFocus       NMoa001cEditText010
               return
.........................................................................................................
MOANotesPost
               Getitem        NMoa001cEditText013,0,MOANotes
               call           Trim using MOAnotes
               IF             (Moanotes ="" or transnum = "")
               alert          Caution,"Note will not be written. (no text or no ID ##) !",result
               return
               endif
               PackKey        MOANotesFld,transnum
               call           MOANotesTsT
               if             Not over
               alert          Plain,"This will overwrite the current note !",result
                              IF             (result = c1)
                              call           MOANotesUpd
                              else
                              call           MOANotesKey
                              Setitem        NMoa001cEditText013,0,MOANotes
                              return
                              endif
               else
               PackKey        MOANotesKey,transnum
               call           MOANoteswrt
               endif
               return
.........................................................................................................
.
Timeout
        beep
        beep
        beep
        winshow
        stop
.........................................................................................................
colorerror
        noreturn
        move    C1,colorflag
        goto aftercolor
ColorGo
        if (result = C1)
                call    BackColor
        elseif (result = C2)
                call    TextColor
        else
                return
        endif
        clear   n1
        prep    colorfile,"c:\progra~1\nincal\NInv010.col"
	        loop
                add     c1,n1
                write   colorfile,seq;colornum(n1)
                until (n1 =2)
        repeat
        close   colorfile
        return
.Trap for Cancel Entry in Color System Menu
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
. .........................................................................
STOP     
         STOP
         INCLUDE   NMOAIO.INC
         INCLUDE   NMOBIO.inc
         
	include	compio.inc
	include	cntio.inc
         
         INCLUDE   NPASIO.inc
         
         INCLUDE   NDATIO.inc
               Include ninvio.inc        
         
         INCLUDE   GNXTIO.INC
         
         
         include   nordio.inc
         
         include   mlrhelp.inc
         
         include   brkhelp.inc
               Include        MOANotesIO.inc
.Following used only in order to load Search.plf
         include   ncmpio.inc
         include   nrtnio.inc
         include   nownio.inc
         include   searchio.inc      .contains logic for search.plf
         INCLUDE   COMLOGIC.inc

