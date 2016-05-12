PC       EQU       0
          INCLUDE             COMMON.INC
          INCLUDE             CONS.INC
          INCLUDE             NSHPDD.INC
          INCLUDE             NORDDD.INC
.Patch1.1
                              include   compdd.inc
                              include   cntdd.inc
.                   INCLUDE             NMLRDD.INC
.Patch1.1
          INCLUDE             NOWNDD.INC
.Patch1.1
.                   INCLUDE             NbrkDD.INC
.Patch1.1
          include             winapi.inc
.begin patch 1.3
.                   include             ninvdd.inc
               Include        NInvAcddd.inc
                    include             ninvdd.inc
.end patch 1.3
          include   ndatdd.inc
              include       nadjdd.inc
              include       consacct.inc
              include       nmrgdd.inc
              include       nacddd.inc
              include       ndat3dd.inc
               include        ncmpdd.inc
               include        nrtndd.inc
               include        njstdd.inc
               include        nstedd.inc
               Include        DTPDD.inc
........................................................................................................
.
RelDate        Init           "march 2012"
release        init           "1.4"        DLH    .perf increase
.RelDate        Init           "September 30, 2008"
.release        init           "1.3"        DLH    No locks on Order,datacard, ..... file
.RelDate        Init           "March 8, 2005"
.release        init           "1.3"        DLH   08March2005         Invoice Conversion
.RelDate  Init      "December 29, 2004"
.release  init      "1.2"        ASH    29DEC2004 DaysToPay Conversion
.release  init      "1.1"        DMB    26MAY2004 Mailer Conversion
.Release  init      "1.0"             DLH 22January04 NEW
.............................................................................
.START PATCH 1.2 ADDED **TEMPORARY** LOGIC
.This logic is temporary until NINSTE has had it's Mailer and Broker fields converted.
GetNewMlrBrk external "NEOM011A;GetNewMlrBrk"
GetNewMlr external "NEOM011A;GetNewMlr"
.END PATCH 1.2 ADDED **TEMPORARY** LOGIC
Flag12Mos      Dim            1
DIFF     FORM      10
fivePER  FORM      10
last     form      3
Days     dim       5
NDays    form      5
Akey1    init      "01X"
Akey4    init      "04X"
.Following key not used by program but required for search.plf
AKey2   init    "01F"
AKey3   init    "02F"
AKey1A  init    "01L"
AKey2A  init    "02L"
filler  init    "0000"
filler2 init    "0000000"
badstat init    "B*P"
olddate dim     10
Carr    init    0x7f
testff  init    0xff
test55  init    0x55
test00  init    0x00
test01  init    0x01
test1   init    0x33
test2   init    0x08
.hexeight integer 4,"4294967295"
hextwo  init    0x02
hexfour init    0x04
ScanBreak form  "0"
ManFeeflag dim      1
.....................................................
holdmdate form     5
DateOkFlag dim     1
okformore init     "Y"
longest    form     5
longestO   form     5
shortest   form     5
shortestO  form     5
PaidDays   form     7
OpenDays   form     7
Paidcount  form     5
opencount  form     5
prepayflag dim      1
prepaycount form    5
prepaycounto form   5
Guarcount   form    5         guaranteed and paid
GuarcountO  form    5         guaranteed and open
AdvLoPay    form    5         Advanced payment to List Owner
AdvLoPayO   form    5         Advanced payment to List Owner  on open invoice
.begin patch 1.3
ARTotal     form    9.2
.end patch 1.3
.....counters for Non List Management
longestN    form     5
longestNO   form     5
shortestN   form     5
shortestNO  form     5
PaidDaysN   form     7
OpenDaysN   form     7
PaidcountN  form     5
opencountN  form     5
prepayflagN dim      1
prepaycountN form    5
prepaycountNo form   5
GuarcountN   form    5         guaranteed and paid
GuarcountNO  form    5         guaranteed and open
AdvLoPayN    form    5         Advanced payment to List Owner
AdvLoPayNO   form    5         Advanced payment to List Owner  on open invoice
.begin patch 1.3
ARNonLO      form    9.2
.end patch 1.3
........................
DateBranch  form    1       .'1=no date, 2=order date, 3=maildate, 4=returndate, 5= invoice date
newdate1    dim    10      mm/dd/ccyy
newdate2    dim    10      mm/dd/ccyy
startdate   form      5
Enddate     form      5
str10a      dim     10
CreditStopFlag dim   1         .No if break search loop
Mask14      INIT     "$$$,$$$,$$9.99"
CalcARFlag  dim      1
shipsw      dim      1
mrgsw       dim      1
JSTN     DIM       2
EomDate        Form           5
.
.
....MISC. ITEMS....................................
.Colors
white   color
grey    color
RED     COLOR
BLACK   COLOR
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
sSearch submenu
.
.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
.EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search-F2;-;"
HData   init    "&Help;&About"
.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To"
.
endindex form      9
......Search  form       1
Timer   Timer
holdsInfo dim      36



.Define Colors for Each Object
FTC     color
BGC     color

colornum dim   9(2)
Fred     form    3
Fgreen   form    3
Fblue    form    3
colorflag form   1

.................................
coll1     collection
specs     form          4(4)
size      form          "1.000"
infostring          dim        590
Tabnum    form          2
SaveTab   form          2
ListViewNum   form          2
.............................
.Constants for Animate Icon
.These coordinates will place Icon directly
AniH    init    "20"
.AniV    init    "350"
AniV    init    "550"
+
.............................................................................................................
.Set Vars used for About Box
        move    "Neom0036.PLS",Wprognme
        move    "EOM Credit Information",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        move    Reldate to Wreldate
.
.Declare forms, Always declare child forms first
animicon plform Animate         .CONTAINS ALL THE ICONS
SRCH    plform  Search
mss1    plform  Error
abt     plform  About
x       plform  Generic
        winhide
.Load Forms, Always load parent form first
        formload x
        formload abt
        formload mss1
        formload SRCH
        formload animicon
.Set tab index
        move    C2,TabNum
.Dynamically reset Animate as same size as Main window
.

        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        CREATE  Generic001;MFile,FData
        create  Generic001;mOptions,OData,mFile
        create  Generic001;mHelp,HData,mOptions
.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
        activate mOptions
        activate mHelp,HelpGo,result
.Create SubMenu
        create  Generic001;sSearch,SData,mOptions,1
.Activate SubMenus
        activate sSearch,SearchGo,result
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

.Set Error Message Stat Text Boxes
               call           SetCreditErrorMssgDefault
               UNPACK         TODAY INTO mm,STR1,dd,STR1,yy                       ;passed from DSPROG
.               Move           "12" to mm
.               Move           "31" to dd
.               Move           "03" to yy
               call           cvtJul
               move           Juldays to EomDate
. .................................................................
.START PATCH 1.2 REPLACED LOGIC
.               Prepare        DTPFIle,"\\nins1\e\data\DaystoPay.dat","\\nins1\e\data\daystopay.isi","8","40",exclusive
.               Prepare        DTPFIle,"\\nins1\e\data\DaystoPay.dat","\\nins1\e\data\daystopay.isi","12","44",exclusive
               Prepare        DTPFIle,"\\nins1\e\data\DaystoPay.dat","\\nins1\e\data\daystopay.isi|10.10.30.103:502","12","44",exclusive
.END PATCH 1.2 REPLACED LOGIC
               MOVE      C2 TO NORDPATH
               MOVE      C1 TO NinvPATH
.begin patch 1.4
.begin patch 1.4
          Move      C3,NORDLOCK                .no locks required
          Move      C3,NDatLOCK                .no locks required
          Move      C3,CompLOCK                .no locks required
          Move      C3,CNCTLOCK                .no locks required
          
          move      mm,n2
          sub       c1,n2
          move      n2,str2
          move      yy,n2
          sub       c1,n2
          pack      str6 from cc,n2,str2
          rep       zfill in str6
          pack      taskname from "\\nins1\e\data\text\ninord.dat, \\nins1\e\data\norddays.dat /p=#"91>'",str6,"'#""
          sort      taskname,SUNDM="10.10.30.103:502"
          aamdex    "\\nins1\e\data\norddays.dat,\\nins1\e\data\norddays.aam -3-6,16-21,26-37,303-306,22-25,52-63,198-199",SUNDM="10.10.30.103:502"
          Open      NORDFLE2,"\\nins1\e\data\norddays|10.10.30.103:502"
         MOVE      C1 TO NORDFLG2
.end patch 1.4
          move    "ninsteb" to nstename
Main

               call           nsteseq
               Goto           passtwo if over
               Replace        Zfill,stebrk
               Replace        Zfill,stemlr
.START PATCH 1.2 REPLACED LOGIC
.               packkey        DTPFld from SteBrk,SteMlr
.Temporary patch, above logic will be restored when NSTEBRK/NSTEMLR are using NEW Company numbers!!
                    call      GetNewMlrBrk using STEMLR,STEBRK,DTPFLD
.END PATCH 1.2 REPLACED LOGIC
               read           DTPFIle,DTPFld;;
               goto           Main if not over
               clear     nordfld1
               clear     nordfld2
               clear     nordfld3
               clear     nordfld4
               clear     nordfld5
               if        (stemlr > " " and stemlr <> "")
               packkey   nordfld1 from akey1,stemlr
               endif

               if        (stebrk > " " and stebrk <> "" and stebrk <> "0000")
               packkey   nordfld4 from akey4,stebrk
               endif

               call           nordaim
               If             Not over
               Replace        Zfill,stebrk
               Rep            Zfill in Obrknum
               move        oodtem to mm
               move        oodted to dd
               move        oodtey to yy
               call        cvtjul
.;;;set flag for 3 year average and one year of one year only
               Move           No to Flag12Mos
                              If             (OBRKNUM = SteBrk)
                                             if             (Juldays >= (EomDate-365) & Juldays <= Eomdate)
                                             move           yes to Flag12Mos
                                             call           checkinv
                                             goto           lrloop
                                             endif
                                             If             (juldays < (EomDate-1095))      .to old
                                             goto           Lrloop
                                             Else
                                             call           checkinv
                                             endif
                               elseIf        (SteBrk = "0000")
                               goto           LRLoop
                               Else
                               Goto          Main
                               endif
               endif
               goto           main
LRloop
               loop

               call          NordKG
               if            not over
.                            call           Debug
               move          yes to okformore
               Replace        Zfill,stebrk
               Rep            Zfill in Obrknum
                              If             (OBRKNUM <> SteBrk)
                              goto           Skipper
                              endif
               move           oodtem to mm
               move           oodted to dd
               move           oodtey to yy
               call           cvtjul
               Move           No to Flag12Mos
                              if             (Juldays >= (EomDate-365) & Juldays <= Eomdate)
                              Move           Yes to Flag12mos
                              call           checkinv
                              goto           Skipper
                              endif
                              if         (juldays >= (EomDate-1095))      .to old
                              call           Checkinv
                              endif
               else
               move          no to okformore
               endif
Skipper
               until          (okformore = NO)
               repeat
               goto           main
..............................................................................................................
checkinv
        if (OSTAT = "X" OR OSTAT = "z" OR OSTAT = "x" or Ostat = "l")
        return
        endif
.
.
               packkey        ninvfld from olrn
               call           ninvkey
               If             Over
                              Return
               Else
                        if        (omdtem <> "00" and omdtem <> "  ")
                        move      omdtem to mm
                        move      omdted to dd
                        move      omdtey to yy
                        else
                        pack      str10a from invdtem,slash,invdted,slash,invdtec,invdtey
                        move      invdtem to mm
                        move      invdted to dd
                        move      invdtey to yy
                        endif
               endif
               call      cvtjul
               move      juldays to holdmdate
               cmatch     "P" to statb
                              if         equal
                              unpack     mlrpayd into str2,yy,mm,dd
                              call      cvtjul
                              else
                              Move           eomdate to juldays
                              endif
.
               sub            holdmdate from juldays
               move           c0 to days
               move           juldays to days
               move           days to Ndays
               if             (Ndays < 0)            ;must be prepay
               return
               endif
               Replace        Zfill,stebrk
               Replace        Zfill,steMLR
.START PATCH 1.2 REPLACED LOGIC
.               packkey        DTPFld from SteBrk,SteMlr
.Temporary patch, above logic will be restored when NSTEBRK/NSTEMLR are using NEW Company numbers!!
                    call      GetNewMlrBrk using STEMLR,STEBRK,DTPFLD
.END PATCH 1.2 REPLACED LOGIC
               read           DTPFIle,DTPFld;DTPBRoker,DTPMailer,DTP3YrCount,DTP3Yrdays,DTP1YrCount,DTP1Yrdays
               goto           writeDTPFIle if over

                              if        (Flag12Mos = yes)
                              add        c1 to DTP1YrCount
                              add       ndays to DTP1Yrdays
                              endif
               Add            c1 to DTP3YrCount
               add            ndays to DTP3Yrdays

               Update         DTPFIle;DTPBRoker,DTPMailer,DTP3YrCount,DTP3Yrdays,DTP1YrCount,DTP1Yrdays

               return

WriteDTPFIle
.START PATCH 1.2 REPLACED LOGIC
.               packkey        DTPFld from SteBrk,SteMlr
.               Replace        Zfill,stebrk
.               Move           Stebrk to DTPBRoker
.               Move           Stemlr to DTPMailer
..................................................
.DTPFld is already loaded!!
                    unpack    DTPFLD,DTPBRoker,DTPMailer
.END PATCH 1.2 REPLACED LOGIC
                              if        (Flag12Mos = yes)
                              Move        c1 to DTP1YrCount
                              Move        ndays to DTP1Yrdays
                              endif
               Move           c1 to DTP3YrCount
               Move           ndays to DTP3Yrdays
               Write          DTPFIle,DTPFld;DTPBRoker,DTPMailer,DTP3YrCount,DTP3Yrdays,DTP1YrCount,DTP1Yrdays
               return
. .........................................................................
.write record
.return
PassTwo
               Close          DTPFIle
.read thru DTPFIle calc and
               shutdown
.
Timeout
        beep
        beep
        beep
        winshow
        stop

ClearCreditSearchList
.Clear ListView

.
        move         c0 to longest
        move         "900" to shortest
        move         c0 to longesto
        move         "900" to shortesto
        move         c0 to opencount
        move         c0 to paidcount
        move         c0 to opendays
        move         c0 to paiddays
        move         c0 to ndays
        move         c0 to prepaycount
        move         c0 to prepaycounto
        move         c0 to Guarcount
        move         c0 to Guarcounto
        move         c0 to AdvLoPay
        move         c0 to AdvLoPayO
.begin patch 1.3
          move      c0 to ARTotal
          move          c0 to ArNonLO
.end patch 1.3
.
        move         c0 to longestN
        move         "900" to shortestN
        move         c0 to longestNo
        move         "900" to shortestNo
        move         c0 to opencountN
        move         c0 to paidcountN
        move         c0 to opendaysN
        move         c0 to paiddaysN
        move         c0 to prepaycountN
        move         c0 to prepaycountNo
        move         c0 to GuarcountN
        move         c0 to GuarcountNo
        move         c0 to AdvLoPayN
        move         c0 to AdvLoPayNO
.
        clear        str5
        return
..........................................................................................................

STOP
.         CLOSE     LOGFILE
                winshow
         STOP
.
SetCreditErrorMssgDefault
.        setprop ErrorMssgStat1,visible=1
.        setprop ErrorMssgStat2,visible=1
.        setprop ErrorMssgStat3,visible=1
.        setprop ErrorMssgStat4,visible=1
.        setprop ErrorMssgStat5,visible=0
.        setitem ErrorMssgStat1,0,"Enter 4 Digit Mailer Number:"
.        setitem ErrorMssgStat2,0,""
.        setitem ErrorMssgStat3,0,"    Or hit F2 to Search"
.        setitem ErrorMssgStat4,0,"      By Company Name"
.        setitem ErrorMssgStat5,0,"      That Mailer Does Not Exist!"
.        setitem ErrorMssgOK,0,"&OK"
        return
.......................................................................................................................
CreditSwitchToTwo
        if (TabNum <> C1)
                if (TabNum = C2)
                        move    C1,N1
                endif
.                call    CreditTabClick
                move    C1,N1
.                call    CreditTabChange
.                setitem Credit001TabControl001,0,2
        endif
        return
.......................................................................................................................
.begin patch 1.43
CreditForceToOne
.               setitem Credit001TabControl001,0,1
.               Deactivate Credita
.               Deactivate Creditb
               move        c1 to n1
.         call        CreditTabChange

               return
.end patch 1.43

...........................................................................................
.......................................................................................................
FileGo
                winshow
                stop

CreditXRefKeyPress
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
.SHIP-TO
        move    C4,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchLoad
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4
SearchLoad1
.BROKER
        unpack  Srchstr,str4,str1,str3,str1,str45
        return
SearchLoad2
.LIST - not an option with this program
        return
SearchLoad3
.MAILER
        unpack  Srchstr,str4,str1,str3,str1,str45
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
.

HelpGo
        setprop AboutMssg,visible=1
        return
.----------------------------------------------------------------------------------------------------------------------
.begin patch 1.43
getbrk
           clear     nbrkfld
           packkey   nbrkfld from obrknum,z3
           clear     brknum
                clear     brcomp
           call      nbrkkey
                return

          INCLUDE             NSHPIO.INC
.Patch1.1
                              include   compio.inc
                              include   cntio.inc
.                   INCLUDE             NMLRIO.INC
.patch1.1
          INCLUDE             NORDIO.INC
          INCLUDE             NOWNIO.INC
.Patch1.1
.                   INCLUDE             NbrkIO.INC
.Patch1.1
.begin patch 1.3
.         include             ninvio.inc
.              include       compute.inc
               Include        ninvio.inc
               Include        NInvAcdio.inc
               Include        compute.inc
.end patch 1.3

              include       Nadjio.inc
              include       nmrgio.inc
              include       nacdio.inc
              include       ndat3io.inc
         include ncmpio.inc
         include   ndatio.inc
         include   nrtnio.inc
         include   searchio.inc      .contains logic for search.plf
                Include        DTPio.inc
              Include        Nsteio.inc
               include   njstio.inc

         INCLUDE   COMLOGIC.INC

