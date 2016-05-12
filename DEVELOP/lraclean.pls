.......................................
. Program:      LRAClean.PLS
. Function:     LRA Maintenance
. Author:       David Herrick
. Orig. Date:   
. Release:      1.0
.temp cleanup some staff boo boos
........................................

PC      EQU     0
.Include Files
        include common.inc
        include cons.inc
        include NLRAdd.inc
        include nusedd.inc

release   init      "1.00"   DLH display PDF in PLB Object Browser
reldate    init       "2015 October 12"

.TESTVARS
TESTINT1        INTEGER 4
TESTINT2        INTEGER 4
str4b   dim     4
yesno1    integer   1,"0x000004"

INFile       File
Holdlra    dim        60
holdLRAkey    dim        12
Timer   Timer
.Files to open
prfile  pfile
preffile file
TabNum   form   1
ExitFlag init   "Y"
ReturnFlag init "N"
UpdateFlag init "N"
NewFlag init    "N"
MlrFlag init    "N"
HoldFlag form   "1"
AKey1   init    "01X"
.Following key not used by program but required for search.plf
AKey2   init    "01F"
AKey3   init    "02F"
AKey1A  init    "01L"
AKey2A  init    "02L"
filler  init    "0000"
filler2 init    "0000000"
badstat init    "B*P"
newdate1 dim     10
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

.Length of record plus space for B1 and space for "/"
hold    dim     62
key     dim     45
holdkey dim     7
holdList dim     6
str6a      dim        6

holdcnt dim     3
holdstat dim    1

.Vars used for Report Screen
RptCan  dim     1
FromNo  dim     4
ToNo    dim     4
FromDate dim    8
ToDate  dim     8
List    form    1
Inactive form   1
Preview form    1
Default form    1
Select  form    1

LRA2Mode   dim        1          .M=mod,N=New
LRApath init    "\\nins1\e\data\LRAS\"                              ."



.
.
colordim dim        8

.Colors
timestamp2 dim  16
HoldDir   dim       200
HoldFName dim       45
FileName1 dim       250
FileName2 dim       250

.Set Vars used for About Box
        move    "NLRA0001.PLS",Wprognme
        move    "LRA File Maintenance",Wfunction
        move    "David Herrick",Wauthor
        move    release,Wrelease
        move    Reldate,Wreldate



.Main Loop
.
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM,DD
.Get User Info
        move    C0,N3
        move    PORTN,N3
        if (N3 <  C1)
                clock   PORT,str3
                unpack  str3,str2,str1
                pack    str3,str1,str2
                move    str3,PORTN
        endif
        move    C0,NUSEFLD
        move    C1,NUSEPATH
        move    PORTN,NUSEFLD
        rep     zfill,NUSEFLD
        call    NUSEKEY

.        setfocus LRAEditList
.          setmode   *GRAYSCALE=0

           OPen       INFile,"c:\work\lraclean.dat"
Looper     loop
           read       INfile,seq;NLRAVARS
           until      over
           packkey       HoldLRA using NLRAVARS
           Packkey       HoldLRAkey from Nlralist,Nlramailer
           display    *p10:10,Holdlra
           packkey    Nlrafld from Nlralist,Nlramailer
           call       Nlrakey
           if         not over
           pack       str12 from Nlralist,Nlramailer
                      if         (holdLRAkey = str12)
                      call       Isit
                      endif
           endif
looper2
           call       Nlraks
           if         Not over
           pack       str12 from Nlralist,Nlramailer
                      if         (holdLRAkey = str12)
                      call       Isit
                      else       goto looper2
                      endif
           endif
           repeat
           shutdown
Isit       Display    *p10:12,nlravars    
           keyin      *p10:14,"is this the one you want?",str1
           rep        "yY",str1
           if         (str1 = "Y")
           call       Nlradel
           move       Holdlra,Nlravars
           call       Nlrawrt
           goto       looper
           endif
           return
           
           

.Include IO files
           include NLRAio.inc
           include nuseio.inc
.Following used only in order to load Search.plf
           include comlogic.inc
