.........................................................................
.
.  NAMES IN THE NEWS & COMPUNAME, INC. DATA CARD RETRIEVAL PROGRAM
.......................................................................
.
. DATAPIC  RETRIEVES DATA CARD RECORDS FROM THE 'DATMST' FILES BASED
.          ON USER SELECTION CRITERIA.
.......................................................................
.
PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
           INCLUDE   NORDDD.INC
           INCLUDE   NOWNDD.INC
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
           INCLUDE   NDATDD.inc
           INCLUDE   NOFRDD.INC
           INCLUDE   NCATDD.INC
          INCLUDE   NREFDD.INC
         include   nusedd.inc
         inc       logdata.inc
         include   ncntdd.inc
.begin patch 8.1
           include    Ntxtdd.inc
.end patch 8.1
.............................................................
.Following used only in order to load Search.plf
        include ncmpdd.inc
        include nrtndd.inc
          
...............................................................
release   init    "8.1"            DLH add text search
reldate   Init      "2015 May 29"  
.release   init    "8.0"            DLH Gui interface, major rewrite, see version 7.42 for earlier code
.reldate   Init      "2014 August 6"  
Author    Init      "David Herrick"
.release   init    "7.42"            DLH cleanup menu
.reldate   Init      "2013 May 16"
.
.......................................................................
.
NEWMST   IFILE     keylen=6,var=3002,comp,dup  
FORMFILE FILE      UNCOMP
. ............................................................................
.
NEWNAME  DIM       35        file name of output file
isiname  dim       35        "     "   "   "     isi file
SAVENAME DIM       35
recname  dim       55
A        INIT      "A"
R        INIT      "R"
CO       DIM       1         company N/C
PICK     DIM       1         keyed in pick-off choice
NUMPICK  FORM      2         numeric version of above
searchstr dim       25        free form search
WITHDRAW FORM      1         withdrawn flag, 0=don't include them
SORT     FORM      1         SORT BRANCH FLAG 0=DEFAULT ALPHA CARD STYLE,
.                            1=NO SORT, 2=RH style only, 3=RH & Card style.
.                                  4=BLANK STOCK SORTED, 5=BLANK NO SORT, 9-NO PRINT
ID       DIM       1
PORTNUM  DIM       3
CARD     INIT      ",DATA"
IN       INIT      ",IN="
KILL     INIT      ",KILL=Y"
RH       INIT      ",DATARH"
PDF      init      ",PDF=Y"
copy     dim       3        number of copies
FRLSNO   DIM       6         keyed in low list number range
TOLSNO   DIM       6         keyed in high list number range
CHECK1   FORM      6         numeric version of list number low range
CHECK2   FORM      6         numeric version of list number high number
FOUND    FORM      9          records that matched criteria
TOTAL    FORM      9         total records read for a pick-off
ONAME    DIM       25        owner name from the owner file
OCOMP    DIM       25        owner company from the owner file
LISTOWN  DIM       70        chosen list owners seperated by a blank
LISTSB   DIM       90        chosen Service B's seperated by a blank
LISTBRCH FORM      1         used for coordinate and string changes
LIST     DIM       96        current list range string being worked on
LIST1ST  DIM       96        list number ranges
LIST2ND  DIM       96               " "
LIST3RD  DIM       96               " "
LIST4TH  DIM       96               " "
ANDOR    DIM       1         keyed in A or O for category pick-off
ANDORTXT DIM       3         for category pick-off contains 'and'/'or'
CATPICK  DIM       30        chosen categories
CATOMIT  DIM       15        chosen category omissions
OMITBRCH FORM      1         if category omits were requested,0=yes,1=no
CATMASK  INIT      "XXX-XXX-XXX-XXX-XXX-XXX-XXX-XXX-XXX-XXX"
.
CATS     DIM       39        above mask breaks up 'category' field for scan
DESC20   DIM       20        category description retrieved from catmst
EXCLCODE DIM       1         exclusive type requested P/C/B

REVPICK  DIM       112       chosen dates
REVPICK2 DIM       112       chosen dates only when mm/dd/yy is used
REVCOUNT FORM      2         screen line counter
YEAR     DIM       2         keyed in year and year from datmst
MO       DIM       2         keyed in month and month from datmst
MOYR     DIM       4         month & year from datmst
DAY      DIM       2         keyed in day and day from datmst
DATE     DIM       8         mm/dd/yy appended to chosen string
REVBRCH  FORM      1         used to determine if/how dates should
.                             be checked. 0=no,1=yy,2=mmyy,3=mmddyy
MLRBRCH  FORM      1         check mlr/offer omits,0=no,1=no,2=mlr,3=ofr
MLR      DIM       4         keyed in mailer number
MLROMIT  DIM       35        chosen mlr/offer omissions seperated by ' '
.                            maximum of seven mailers.
MLRPAGE  FORM      1         used during mlr omit, if more than 5 mlrs.
YR       DIM       2
LODATE   FORM      5         julian low order date   (yyjjj)
HIDATE   FORM      5         julian high order date  (yyjjj)
ORDJUL   FORM      5         julian order date from ordmst
.
.
DATA     INIT      "DATA"
time      init      "HH:MM:SS"
.
.begin patch 8.0
CreateDataCard        external "NDAT0002;CreateDataCard"
CreateXLSCard         external "NDAT0005;CreateXLSCard"
CreateXLS             external "NDAT0005;CreateXLS"
CreateRHLIST          external "NDAT0005;CreateRHLIST"
CreateRHLISTR         external "NDAT0005;CreateRHLISTR"
CATCODES DataList
Timer     Timer
ColHeads automation
ColHead    automation
OTRUE   variant
OFALSE  variant
ListIts    automation
ListIt     automation
VT_BOOL EQU 11
OBOOL      variant
.end patch 8.0

DR      DIM     45
TXT      INIT      ".DAT"
           XIF
DELNAME  DIM       20
SHORTNME DIM       6
OKEY1    INIT      "01R"
OKEY2    INIT      "02R"
WITHCODE DIM       1
NEWMFLAG FORM      1               0=OUTPUT FILE CLOSED 1=OUTPUT FILE OPEN
repflag  form      2               report type
rhflag   form      1               1=no rh 2=yes rh as secondary rep.
PDFFLag  form      1               0,1 = default, 2= produce a pdf file
repstyle dim       25
blincnt form       1              blank line counter allow several in a row.
autoflag form      1             
prfile    pfile
font1     font
font2     font
NINLogo   PICT
..............................................................
StopFlag init     "N"
ExitFlag init     "Y"
.Collections
ColPrim     collection              .Collection for Primary Action search
ColSec      collection              .Collection for Secondary Action search
ColRadate   collection              .Collection for Radio Button Dates
ColCombDate collection              .Collection for Combo Dates
ColFilter   collection              .Collection for Filter GroupBox
ColPrint    collection              .Collection for Print GroupBox
ColSort     collection              .Collection for Sort Radio Buttons
HOLDPRM  form     3               .Holds selected Primary Search
HOLDSEC  form     3               .Holds selected Secondary Search

PRMPIK   form     "15"            .Number of primary options
SECPIK   form     "12"            .Number of secondary options
REPPIK   form     "37"            .Number of Report Options
.Flags
SECFLAG  init     "N"             .Flag to specify where search result will be located (Primary/Secondary)
SECOND   init     "N"

PRIMTRACE form     3              . Tells what the Primary pick is
SECTRACE  form     2              . Tells what the secondary pick is
PRimCat   dim         1           .if yes searching catetories - PRimary

PRIMLIST  form     2              .How many search params in Primary Pick
SECOLIST  form     2              .How many search params in Secondary Pick
DateGROUP form     1              . 0-ALL Dates 1-Order Dates 2-Mail Dates 3-Return Dates
SortGroup  Form     1              . 0=alpha by name, 1 = asis, 2 = numeric, 3 = by owner then alpha
KEEPDATE1 form     5              .Julian Date of Beg Date
KEEPDATE2 form     5              .Julian Date of End Date

RptCan    Dim       1             .report cancellation flag
Year1     Dim       4
yesno1    integer   1,"0x000024"                yes no buttons, question Icon

.Menu
.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu
.Set Up SubMenu for Options
sSearch submenu

.Present Data for Menu Bar
FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search;-;&Preferences"
HData   init    "&Help;&About;Help Topics"

.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To"
..................................................................................

abt        plform     About
rpt2       plform     Report2
SRCH       plform     Search
.Load Forms
Cat        PlForm     Ndat0004s  
x          plform     NDat0004
.Collection for Primary Section
.ColPrim collection
        listins ColPrim,NDat0004DataListPrimary,NDat0004EditSearchP,NDat0004PrimaryAdd:
          NDat0004ButtonPrimaryDel
.Collection for Secondary Section
.ColSec collection
        listins ColSec,NDat0004SelectSecondary,NDat0004DataListSecondary,NDat0004EditSearchS,NDat0004SecondaryAdd:
          NDat0004ButtonSecondaryDel
.Collection for Radio Button Dates Section
.ColRadate collection
        listins ColRadate,NDat0004RadioAll,NDat0004RadioRevdate,NDat0004RadioPutup
.Collection for ComboButton Dates Section
.ColCombDate collection
        listins ColCombDate,NDat0004ComboMo1,NDat0004ComboDay1,NDat0004ComboYear1,NDat0004ComboMo2,NDat0004ComboDay2:
          NDat0004ComboYear2
.Collection for Filter Group Box
.Filter collection
.        listins ColFilter,NDat0004CheckExclude,NDat0004CheckNets,NDat0004CheckLCR,NDat0004CheckPending
        listins ColFilter,NDat0004CheckBoxWith
.Collection for Print Group Box
.Printer collection
.        listins ColPrint,Nord006CheckDUP,Nord006CheckX,Nord006CheckPDF,NDat0004ComboCopy
        listins ColPrint,NDat0004CheckBoxUsage,NDat0004ComboCopy,NDat0004ComboBoxPrint
.Collection for Sort Group Box
.Sort collection
        listins ColSort,NDat0004RadioSortAlpha,NDat0004RadioSortAsIS,NDat0004RadioSortNumeric,NDat0004RadioSortOwner
.load      
           winhide
          
        move    "NDat0004.PLS",Wprognme
        move    "Datacard Pick Program",Wfunction
          MOve      Author,Wauthor
          Move      Release,Wrelease
          Move      Reldate,Wreldate

           formload x
           formload SRCH
           formload RPT2
           formload ABT
           FormLoad   Cat        


          move      "250",column
          move      "750",column1
          move      "2000",column2
          move      "2250",column3
          create    font1,"Helvetica",size=12,bold
          create    font2,"Arial",size=12 
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
          call      GetWinVer

                    call Get64OS
.Timer creation
          CREATE    TIMER,18000         .30 minutes
          ACTIVATE TIMER,Timeout,RESULT


  create  NDat0004;mFile,FData
  create  NDat0004;mEdit,EData,mFile
  create  NDat0004;mOptions,OData,mEdit
  create  NDat0004;mHelp,HData,mOptions
.Create SubMenu
  create  NDat0004;sSearch,SData,mOptions,1
.Activate Menus
.FileGo leads to stop
  activate mFile,FileGo,result
.Need this when it works
  activate mEdit,EditGo,result
  activate mOptions,OptionsGo,result
.Only a SubMenu under this one
  activate mHelp,HelpGo,result
.Activate SubMenus
  activate sSearch,SearchGo,result
.Load Primary SEARCH Combo Box

.Set Default Report Option as null
        setitem  NDat0004ComboReportType,n1,c0
           setitem    NDat0004ComboBoxPrint,0,C3                  .set default to PDF

.Deleteitem for aesthetics
          deleteitem NDat0004ComboYear1,c0
          deleteitem NDat0004ComboYear2,c0
          clock timestamp to timestamp
          unpack timestamp,str4,str2
          unpack timestamp,cc,yy,mm,dd
          call      datetest              .check for leap year
                    move str4 to n4
          add c1 to n4
          move n4 to str4
        move str4 to year1
        move "1988" to str4
        clear n4
        move str4 to n4
        clear n3
        loop
          add c1 to n1
          insertitem NDat0004ComboYear1,n3,str4
          insertitem NDat0004ComboYear2,n3,str4
          until (str4 = YEAR1)
          add c1 to n4
                    move n4 to str4
        repeat
        move str2 to n2
          setitem  NDat0004ComboMo1,n1,c1
          setitem  NDat0004ComboMo2,n1,n2
.move focus down to current year
        setitem  NDat0004ComboYear1,n1,c2
        setitem  NDat0004ComboYear2,n1,c2
          deleteitem NDat0004ComboDay1,c0
          deleteitem NDat0004ComboDay2,c0
        call DAY1
        call DAY2
        setitem NDat0004ComboCOPY,c0,c1
          setfocus NDat0004EditSearchP
           SETPROP  NDat0004CheckBoxFreefloat,ENABLED=c0
           SETPROP     NDat0004CheckBoxFreefloat,VISIBLE=c0
.           SETPROP    Ndat0004StatTextRecs,VISIBLE=C0
.           SETPROP    Ndat0004StatRead,VISIBLE=C0

.load cats
.
           Ndat0004SListView1.InsertColumn using "Category",90,1
           Ndat0004SListView1.InsertColumn using "Sub Code",90,2
           Ndat0004SListView1.InsertColumn using "Code",45,3

           PACK       NREFFLD1,"01X","T"
           MOVE       "RefOK-NREFAIM",Location
           PACK       KeyLocation,"Key: ",NREFFLD1
           CALL       NREFAIM
           LOOP
                      UNTIL OVER
                      CALL       CATLoadListView
                      MOVE       "RefOK-NREFKG",Location
                      CALL       NREFKG
           REPEAT



          create    CATCODES=1:1:1:1


      if       (program = "NDAT0004")
           move     c1 to autoflag
           if        (Company = c2)
           MOVE         "PLI" TO COMPNME
           else
                      MOVE         "NIN" TO COMPNME
           Endif
           move      "099" to portn
           move       c0,Withdraw
           clear      copy
           move NO to SECOND
           SetItem ndat0004EditSearchP,0,"B"
           SetItem ndat0004StatClient1,0,"NIN or PLI managed"
          Setitem NDat0004SelectPrimary,n4,C3
          insertitem NDat0004DataListPrimary,9999,"B"
           GETITEM Ndat0004DataListPrimary,c1,n2
           move    n2 to PRIMLIST                  .Number of selections in datalist
           move       c4,PRIMTRACE           .Exclusives          
           goto       start
           else
           MOVE       "NDAT0004" TO PROGRAM
           endif
           MOVE       "DATACARD RETRIEVAL" TO STITLE
           MOVE       C1 TO NDATPATH
         move       c0 to newmflag
         move      c1 to nusepath
         move      c1 to pdfflag
         move      c0 to nusefld
         MOVE      PORTN TO NUSEFLD
         REP       ZFILL IN NUSEFLD
         CALL      NUSEKEY
         goto      userng if over
         scan      "INVALID" in nuseuser
         goto      userng if equal
         reset     nuseuser         
         move      c3 to ncntpath
         move      portn to ncntfld1
         rep       zfill in ncntfld1
         call      ncntkey
         if        over
         move      c2 to cntprint
         endif
.

        LOOP
                waitevent
                    deactivate TIMER
                    ACTIVATE TIMER,Timeout,RESULT
        Repeat

Timeout
.Add Test to make sure nothing is left open
          beep
          beep
          beep
          shutdown



.
+
.

.  CREATE OUTPUT FILE NAME, APPENDS "01" TO 'DATA', ADDS 1
.    UNTIL IT FINDS A NON-EXISTENT FILE NAME.
.
VALIDNUM 
         move       c0 to newmflag
         TRAP      GOODA GIVING ERROR IF IO
         MOVE      PORTN TO N3            .11jul96 use port number (again)
         move      c0 to n2
         add       n3 to n2
NEXTFILE MOVE      N2   TO STR2
         REPLACE   ZFILL IN STR2
         TRAP      GOODA GIVING ERROR IF IO
         IF        (AUTOFLAG < C1)
         PACK      NEWNAME WITH ntwkpath1,DATA,STR2
         PACK      SHORTNME FROM DATA,STR2
         ELSE  
        pack    NEWNAME,NTWKPATH1,"DATAEXCL.DAT"
         MOVE      "DATAEXCL" TO SHORTNME
         ENDIF
         MOVE      NEWNAME TO SAVENAME
         OPEN      NEWMST,NEWNAME,exclusive
         CLOSE     NEWMST

ADDFILE  ADD      C1 TO N2
         GOTO      NEXTFILE
GOODA    TRAPCLR   IO
         NORETURN
         SCAN      "0030-0031" IN ERROR        .datapoint rms
         GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I * Y" IN ERROR            .pcbus oops somebody elses file.
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I03" IN ERROR           .plb
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I10" IN ERROR           .plb
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I25" IN ERROR           .plb
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           TRAP      GOODR GIVING ERROR IF IO
           IFNZ        PC
         PACK      NEWNAME WITH DATA,STR2,A,TXT,DR
           XIF
           IFZ         PC
         PACK      NEWNAME WITH DR,DATA,STR2,A
           XIF
         OPEN      NEWMST,NEWNAME,exclusive
         CLOSE     NEWMST
         GOTO      ADDFILE
GOODR    TRAPCLR   IO
         NORETURN
         SCAN      "0030-0031" IN ERROR
         GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I * Y" IN ERROR
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I03" IN ERROR           .plb
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
         TRAP      GOODFILE GIVING ERROR IF IO
           IFNZ        PC
         PACK      NEWNAME WITH DATA,STR2,R,TXT,DR
           XIF
           IFZ         PC
         IF        (AUTOFLAG < C1)
         PACK      NEWNAME WITH DR,DATA,STR2
         PACK      SHORTNME FROM DATA,STR2
         ELSE  
        PACK    NEWNAME,NTWKPATH1,"DATAEXCL.DAT"
         MOVE      "DATAEXCL" TO SHORTNME
         ENDIF
           XIF

         OPEN      NEWMST,NEWNAME,exclusive
         CLOSE     NEWMST
         GOTO      ADDFILE
GOODFILE TRAPCLR   IO
         NORETURN
         TRAP      GOODFILE GIVING ERROR IF IO
         SCAN      "0030-0031" IN ERROR
         GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I * Y" IN ERROR
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
           SCAN      "I03" IN ERROR           .plb
           GOTO      ADDFILE IF EQUAL
           RESET     ERROR
         MOVE      SAVENAME TO NEWNAME
         MOVE      B1 TO ERROR

         if        (autoflag < c1)
         PREPARE   NEWMST,NEWNAME,newname,"6","3002",EXCLUSIVE
         else
         move      "EXCL" to STR4
        pack    NEWNAME,NTWKPATH1,"DATAEXCL"
         PREPARE   NEWMST,NEWNAME,newname,"6","3002",EXCLUSIVE
         endif
           MOVE      C1 TO NEWMFLAG              *FILE OPEN
         TRAP      DELFILE IF F5
         TRAP      DELFILE IF INT
          Return
.
*******************************************************************************
+.......................................................................
.
.FINISH - PICK-OFF IS COMPLETE...
.
FINISH   
         flush     newmst
         close     newmst
           MOVE      C0 TO NEWMFLAG                *FILE CLOSED
           if     (found <= c0)
.add something here? Like an Alert?
                      alert caution,"No records found with this criteria!",result,""
                      erase      str25
                      pack       str25 from NTWKPATH1,shortnme,".dat"
                      erase      str25
                      pack       str25 from NTWKPATH1,shortnme,".isi"
                      erase      str25

                      if         (autoflag = c1)
                      shutdown "cls"
                      else
                      Stop                  
           endif      
          endif
FINISH3  MOVE      YES TO STR1          *IF TIME OUT DEFAULT STR1 = 'Y'.
           MOVE      C0 TO FOUND              28JUN93 DLH   CLEAR COUNTER
.         branch    repflag of card,rh,bstock,lcard,cardview,attcard,nother,RHFLAT,RHREV
         branch    repflag of card,card,RH,Excel,RHREV,RHFLAT
         GOTO      CARD
NOTHER
         IF         (AUTOFLAG < C1)
         move      Yes to exitflag
         call      livebut
         winshow
           erase      str25
           pack       str25 from NTWKPATH1,shortnme,".dat"
           erase      str25
           pack       str25 from NTWKPATH1,shortnme,".isi"
           erase      str25
         CHAIN     "NDAT0004"
         stop
           ELSE
         SHUTDOWN
         ENDIF
..............................................................................................................
. CARD - pretty much all  options.
CARD     
           Call       DataSort
           getitem    NDat0004CheckBoxOwner,0,N2
           getitem    NDat0004CheckBoxUsage,0,n3
           getitem    NDat0004ComboBoxPrint,0,n1
.DimPtr  = FileName
.DimPtr1 = Initials
.DimPtr2 = LogIn Name
.FrmPtr  = Printer Choice  1=laser3,2=laser4,3=pdf
.FrmPtr1 = Owner Info Choice
.FrmPtr2 = Usage Choice - '0'=No Usage Print, '1'=Usage Print, '2'=Usage Print w/o Duplexing
.FrmPtr3 = Company

           call      CreateDataCard using str25,INITS,User,N1,N2,N3,company
           call           SetMouseFree


         goto      nother

..............................................................................................................
.RH - RH style report listing
RH 
           Call       DataSort
           getitem    NDat0004CheckBoxOwner,0,N2
           getitem    NDat0004CheckBoxUsage,0,n3
           getitem    NDat0004ComboBoxPrint,0,n1
.DimPtr  = FileName
.DimPtr1 = Initials
.DimPtr2 = LogIn Name
.FrmPtr  = Printer Choice
.FrmPtr1 = Owner Info Choice
.FrmPtr2 = Usage Choice - '0'=No Usage Print, '1'=Usage Print, '2'=Usage Print w/o Duplexing
.FrmPtr3 = Company

           call      CreateRHLIST using str25,INITS,User,N1,N2,N3,company
           call           SetMouseFree


         goto      nother
..............................................................................................................
.RHREV - RH style report listing with revdate
RHREV
           Call       DataSort
           getitem    NDat0004CheckBoxOwner,0,N2
           getitem    NDat0004CheckBoxUsage,0,n3
           getitem    NDat0004ComboBoxPrint,0,n1
.DimPtr  = FileName
.DimPtr1 = Initials
.DimPtr2 = LogIn Name
.FrmPtr  = Printer Choice
.FrmPtr1 = Owner Info Choice
.FrmPtr2 = Usage Choice - '0'=No Usage Print, '1'=Usage Print, '2'=Usage Print w/o Duplexing
.FrmPtr3 = Company

           call      CreateRHLISTR using str25,INITS,User,N1,N2,N3,company
           call           SetMouseFree

         goto      nother
..............................................................................................................
.Excel - with cards
Excel 
           Call       DataSort

.DimPtr  = FileName
.DimPtr1 = Initials
.DimPtr2 = Used Elsewhere
.DimPtr3 = LogIn Name
.FrmPtr  = Owner Info Choice
.FrmPtr1 = Usage Choice - '0'=No Usage, '1'=Usage 
.FrmPtr2 = Company
           getitem    NDat0004CheckBoxOwner,0,N2
           getitem    NDat0004CheckBoxUsage,0,n3

           
           call           CreateXlsCard using str25,INITS,User,N2,N3,company
           call           SetMouseFree

         goto      nother
..............................................................................................................
.Excel - without cards
RHFLAT
           Call       DataSort

.DimPtr  = FileName
.DimPtr1 = Initials
.DimPtr2 = Used Elsewhere
.DimPtr3 = LogIn Name
.FrmPtr  = 
.FrmPtr1 = 
.FrmPtr2 = Company
.          getitem    NDat0004CheckBoxOwner,0,N2
.          getitem    NDat0004CheckBoxUsage,0,n3
           move       c0,n2
           move       c0,n3
           
           call           CreateXls using str25,INITS,User,N2,N3,company
           call           SetMouseFree

         goto      nother
..............................................................................................................
.

DELFILE  TRAP      DELFILE2 GIVING ERROR IF IO
           TRAPCLR   F5
           TRAP      DELFILE IF F5
.add alert
.           KEYIN     *P1:24,*EL,*B,"THIS OPTION WILL DELETE YOUR DATACARD ":
.                       "FILE OK ? ",*uc,STR1,*lc,*P1:24,*EL;
           CMATCH    YES TO STR1
         if        equal
         noreturn
         goto      delfile2
         endif
         cmatch    no to str1
         return    if equal
           GOTO      DELFILE 

         stop
           BRANCH    NEWMFLAG TO DELFILE2
           GOTO      DELFILE3
DELFILE2 
          match    b4 to newname
          goto     close if eos
          CLOSE     NEWMST
           CLEAR     DELNAME
          PACK      DELNAME FROM DR,SHORTNME,".DAT"
         erase      DELNAME
          CLEAR     DELNAME
          PACK      DELNAME FROM DR,SHORTNME,".ISI"
         erase      DELNAME
           GOTO      CLOSE
DELFILE3 TRAP      NOFILE IF IO
         MOVE      NEWNAME TO FILENAME
          CLOSE     NEWMST
           CLEAR     DELNAME
          PACK      DELNAME FROM DR,SHORTNME,".DAT"
         erase      DELNAME
          CLEAR     DELNAME
          PACK      DELNAME FROM DR,SHORTNME,".ISI"
         erase      DELNAME

CLOSE    STOP
.****************************************************************************************

NOFILE   
           Clear      Taskname
           append     "The file: ",taskname
           append     Filename,Taskname
           append     " is not on-line...",taskname
           reset      taskname
                alert          caution,Taskname,result
           
         SCAN      "DATA" IN FILENAME
         GOTO      DELFILE IF NOT EQUAL
         STOP
userng   
           Clear      Taskname
           append     "I'm sorry I've lost track of who you are ",taskname
           append     Nuseuser,Taskname
           append     CRLF,taskname
           append     "Please leave the program and try again, ",taskname
           append     CRLF,taskname
           append     "Don't forget to tell I.S.",taskname
           reset      Taskname
                alert          caution,Taskname,result
        clock      port to str3           .plb note
        unpack     str3 into str2,str1
        pack       str3 from str1,str2
        move       str3 to portN         
           stop

..........................................................................................................
.list name begins with 'A '
FixitA
          return    
        bump      Mlstname by 2
          clear     str75
          append    mlstname to str75
          append    "|A" to str75
          reset     str75
          clear     Mlstname
          move      str75 to mlstname
        return
.
.list name begins with 'The '
FixitThe
          return
        bump      Mlstname by 4
          clear     str75
          append    mlstname to str75
          append    "|The" to str75
          reset     str75
          clear     Mlstname
          move      str75 to mlstname
        return
..........................................................................................................

Day1
.Used during lost focus event NDat0004ComboMo1 to show correct Dates
.Do not have code in for feb leap year
          deleteitem NDat0004ComboDay1,c0
        clear n3
          getitem  NDat0004ComboMo1,n1,n2
        Load     NDD using N2,c31,Nfeb,c31,c30,c31,c30,c31,c31,c30,c31,c30,c31
        move     NDD to str2
        loop
                 call trim using str2
                        insertitem NDat0004ComboDay1,n3,str2
           until (NDD = c1)
                   sub c1 from NDD
            move NDD to str2
        repeat
          setitem  NDat0004ComboDay1,n1,c1
        return
..........................................................................................................
Day2
.Used during lost focus event NDat0004ComboMo2 to show correct Dates
          deleteitem NDat0004ComboDay2,c0
        clear n3
          getitem  NDat0004ComboMo2,n1,n2
        Load     NDD using N2,c31,Nfeb,c31,c30,c31,c30,c31,c31,c30,c31,c30,c31
        move     NDD to str2
        move     NDD to N10
        loop
                 call trim using str2
                        insertitem NDat0004ComboDay2,n3,str2
           until (NDD = c1)
                   sub c1 from NDD
            move NDD to str2
        repeat
               setitem  NDat0004ComboDay2,n1,N10
        return
.................................................................................
PICKOFF
          
          
          setprop NDat0004PickOff,enabled=c0
          getitem NDat0004SelectPrimary,n4,n3
          getitem NDat0004SelectPrimary,n3,str55
        getitem NDat0004DataListPrimary,c1,n2
        if (n2=c0)
                alert          caution,"Please Enter Valid Data for Primary Search!",result,"Data"
                call     livebut
                return
        endif
        getitem NDat0004DataListSecondary,c1,n2
        if (n2=c0)
                      move NO to SECOND
        Else
                      Move yes to second
        endif              
.Get Report Type
Reporting
          getitem NDat0004ComboReportType,n4,n3
        if (n3 = c0)
                alert          caution,"Please choose a valid Report Type!",result,"Choose Report"
                call     livebut
                setfocus NDat0004ComboReportType
                return
        endif
          getitem NDat0004ComboReportType,n3,str55
        scan "--------" in str55
        if equal
                alert          caution,"Please choose a valid Report Type!",result,"Choose Report"
                call     livebut
                setfocus NDat0004ComboReportType
                return
        endif

   getprop NDat0004RadioSortAlpha,SELGROUPID=SortGROUP        0-alpha 1-asis  2-numeric  3-LO, alpha

   getprop NDat0004RadioAll,SELGROUPID=DateGROUP        0-all 1-Order 2-Rental  3-Return
          getitem NDat0004ComboMo1,n4,n3

        if (n3=c0)
                alert          caution,"Please select a valid date!",result,"Select Valid Date"
                call livebut
                setfocus NDat0004ComboMo1
                return
        endif
               getitem NDat0004ComboYear1,n4,n3
               getitem NDat0004ComboYear1,n3,str4
        move    str4 to N6
               getitem NDat0004ComboYear2,n4,n3
               getitem NDat0004ComboYear2,n3,str4
        move    str4 to N7
        if (N6 > N7)
                alert          caution,"To Date is greater than From Date!",result,"Select Valid Date"
                call     livebut
                       setfocus NDat0004ComboYear1
                return
        endif

        if (N6 = N7)
                              clear  n1
                              clear n4
                              getitem NDat0004ComboMo1,n1,n4
                              getitem NDat0004ComboMo1,n4,str2
          clear n4
                move str2 to n4
                              clear n1
                              getitem NDat0004ComboMo2,n1,n3
                              getitem NDat0004ComboMo2,n3,str2
                move str2 to n8
                if (N4 > N8)
                          alert          caution,"To Date is greater than From Date!",result,"Select Valid Date"
                          call     livebut
                                 setfocus NDat0004ComboMo1
                   return
                endif
                if (N4 = N8)
                                             getitem NDat0004ComboDay1,n4,n3
                                             getitem NDat0004ComboDay1,n3,str2
                        move str2 to n6
                                             getitem NDat0004ComboDay2,n4,n3
                                             getitem NDat0004ComboDay2,n3,str2
                        move str2 to n7
                        if (n6 > n7)
                                             alert           caution,"To Date is greater than From Date!",result,"Select Valid Date"
                                             call     livebut
                                                    setfocus NDat0004ComboDay1
                               return
                        endif
                endif
        endif

                      getitem NDat0004ComboCOPY,c0,COPY
                      if (copy = "")
                                                  alert type=yesno1,"You have selected no copies.  Are you Sure?", result,"Zero copies"
                                if (result=7)    . 6 = yes , 7 = no
                                    call     livebut
                                                                setfocus NDat0004ComboCOPY
                                                                return
                                endif
                                move "0" to copy
                      endif
                    getitem NDat0004ComboReportType,n4,n3
                    move         n3,repFlag


.Test to see if Both PDF/Print is an option
test
          if ((n3 = c9)|(n3=c10)|(n3=c11)|(n3="15")|(n3="32")|(n3="33"))
                    getitem NDat0004ComboBoxPrint,n1,pdfflag
                    if (PDFFLAG = C3)
                                              alert caution,"You cannot select BOTH option with this report!",result,""
                    endif
          endif
                move NO to exitflag
                call Datasetmousebusy
.get primary select
          getitem NDat0004SelectPrimary,n4,PRIMTRACE
         setprop   Ndat0004Stop,enabled=c1
         clear n2
         GETITEM Ndat0004DataListPrimary,c1,n2
         move    n2 to PRIMLIST                  .Number of selections in datalist

         call    GETDATE

         clear   N9

         getitem NDat0004SelectPrimary,n4,PRIMTRACE

.get secondary select
          getitem NDat0004SelectSecondary,n4,SecTRACE
         clear n2
         GETITEM Ndat0004DataListSecondary,c1,n2
         move    n2 to SecoLIST                  .Number of selections in datalist
         getitem NDat0004SelectSecondary,n4,SecTRACE
....................................................................................
           getitem    NDat0004CheckBoxWith,0,n1
           if         (n1 = c1)             .include Withdrawn
           move       c1,Withdraw
           else                             .exclude Withdrawn - default
           move       c0,Withdraw
           endif
Start
          Call      VALIDNUM 
          setitem   Ndat0004DataDisplay,0,Newname
           if         (autoflag = c1)
           move       c4,PRIMTRACE
           move       c0,n9
           endif
.          call        debug
...................................................................................................................................
          if        (PRIMTRACE = c1)            .list number
NextList
        loop
                add c1 to N9
        until (N9 > PRIMLIST)
                GETITEM Ndat0004DataListPrimary,n9,str10
          move      c1,ndatpath
          move      c3,ndatlock
          move      str10 to ndatfld
           REP       ZFILL IN NDATFLD
           CALL      NDATKEY
                    GOTO      NextList if over
                    Call      CHKDATES                            .secondary search checked at chkdates
        repeat

...................................................................................................................................
           elseif    (PRIMTRACE = c2)           .Owner number
NextOwner
        loop
                add c1 to N9
        until (N9 > PRIMLIST)
                GETITEM Ndat0004DataListPrimary,n9,str6
          move      c2,ndatpath
          move      c3,ndatlock
           Clear      NDATFLD1
           Clear      NDATFLD2
           Clear      NDATFLD3
           Clear      NDATFLD4
           Clear      NDATFLD5
           Clear      NDATFLD6
           Clear      NDATFLD7
           packkey    Ndatfld7 from "07X",str6
           CALL      NDATAim
                    GOTO      NextOwner if over
                    Call      CHKDATES                            .secondary search checked at chkdates
                      Loop
                      call       Ndatkg
                    GOTO      NextOwner if over
                      if         (str6 <> OWNNUM)
                      goto       NextOWner
                      endif
                    Call      CHKDATES                            .secondary search checked at chkdates
                      repeat
        repeat

...................................................................................................................................
          elseif    (PRIMTRACE = c3)           .category codes
NextCat
           SETPROP    Ndat0004StatTextRecs,VISIBLE=C1
           SETPROP    Ndat0004StatRead,VISIBLE=C1
           move       c3,NCATLOCK
           move       c3,NDATLOCK
           loop
                add c1 to N9
        until (N9 > PRIMLIST)
          GETITEM Ndat0004DataListPrimary,n9,str6
           call       Trim using str6
           Clear      Ncatfld1
          pack      NCATFLD2,"02X",str6
          move      "NCATAIM",Location
          pack      KeyLocation,"Key: ",NCATFLD2
          call      NCATAIM
           if         Not over
           packkey  ndatfld using NCATLIST
          move      "NDATKEY",Location
          pack      KeyLocation,"Key: ",NDatFLD
           move       c1,ndatpath
           rep        zfill,ndatfld
           call       Ndatkey
                      if         Not over                   .should not happen
                      ADD        C1,TOTAL
                      move       total,str9
                      SETitem   Ndat0004StatRead,0,str9 
                      call       chkdates
                      endif
                      loop
                               move      "NCATKG",Location
                               pack      KeyLocation,"Key: ",NCATFLD2
                               call      NCATKG
                               until    over
                                 packkey  ndatfld using NCATLIST
                                 move      "NDATKEY",Location
                                 pack      KeyLocation,"Key: ",NDatFLD
                                 move       c1,ndatpath
                                 rep        zfill,ndatfld
                                 call       Ndatkey
                                 if         not over
                                 ADD        C1,TOTAL
                                 move       total,str9
                                 SETitem   Ndat0004StatRead,0,str9 
                                 call       chkdates
                                 endif           
.           call       debug          
                      repeat     
.           call       debug          
           endif
          repeat
...................................................................................................................................
          elseif    (PRIMTRACE = c4)           .Exclusives
NextExcl  loop
                add c1 to N9
        until (N9 > PRIMLIST)
                GETITEM Ndat0004DataListPrimary,n9,str6
           call       Trim using str6
           move       str6,exclcode
          move      c4,ndatpath         .managed only
          move      c3,ndatlock
           packkey    Ndatfld using "000000"
           CALL      NDATtst
                      Loop
                      call       NdatKS
                      if         over
                      move       c0,Ndatflag                      .set file open flag to closed
                    GOTO      NextEXCL
                      endif
           ADD        C1,TOTAL
           move       total,str9
           SETitem   Ndat0004StatRead,0,str9 
           if        (exclcode = "C" & Elstcde = "C")
                    Call      CHKDATES                            .secondary search checked at chkdates
           Elseif        (exclcode = "P" & Elstcde = "P")
                    Call      CHKDATES                            .secondary search checked at chkdates
           Elseif        (exclcode = "B" & (Elstcde = "P" or Elstcde = "C"))
                    Call      CHKDATES                            .secondary search checked at chkdates
           endif
                      repeat
        repeat
...................................................................................................................................
          elseif    (PRIMTRACE = c5)           .New
           move       c1,ndatpath
           move       c3,ndatlock
           move       c0,ndatflag           .mark file closed so we start at the begining

           SETPROP    Ndat0004StatTextRecs,VISIBLE=C1
           SETPROP    Ndat0004StatRead,VISIBLE=C1
           MOVE       C0,TOTAL
NextNew    loop
           MOVE    "SearchOK-NDATSEQ",Location
           CALL      NDATSEQ
           Break      if over
           ADD        C1,TOTAL
           move       total,str9
           SETitem   Ndat0004StatRead,0,str9 
           CMATCH    YES,NLSTCDE                       NEW LIST RECORD?
                      if         equal
                      call       CHKDates
                      endif
           repeat
...................................................................................................................................
          elseif    (PRIMTRACE = c6)           .Rev Date
           move       c1,ndatpath
           move       c3,ndatlock
           move       c0,ndatflag           .mark file closed so we start at the begining

           SETPROP    Ndat0004StatTextRecs,VISIBLE=C1
           SETPROP    Ndat0004StatRead,VISIBLE=C1
           MOVE       C0,TOTAL
           loop
           MOVE    "SearchOK-NDATSEQ",Location
           CALL      NDATSEQ
           Break      if over
           ADD        C1,TOTAL
           move       total,str9
           SETitem   Ndat0004StatRead,0,str9 
           call       CHKDates
           Repeat
...................................................................................................................................
          elseif    (PRIMTRACE = c7)           .withdrawn
           move       c1,ndatpath
           move       c3,ndatlock
           move       c0,ndatflag           .mark file closed so we start at the begining

           SETPROP    Ndat0004StatTextRecs,VISIBLE=C1
           SETPROP    Ndat0004StatRead,VISIBLE=C1
           MOVE       C0,TOTAL
           loop
           MOVE    "SearchOK-NDATSEQ",Location
           CALL      NDATSEQ
           Break      if over
           ADD        C1,TOTAL
           move       total,str9
           SETitem   Ndat0004StatRead,0,str9 
           call       CHKDates
           Repeat

...................................................................................................................................
          elseif    (PRIMTRACE = c8)           .Put up date
           move       c1,ndatpath
           move       c3,ndatlock
           move       c0,ndatflag           .mark file closed so we start at the begining

           SETPROP    Ndat0004StatTextRecs,VISIBLE=C1
           SETPROP    Ndat0004StatRead,VISIBLE=C1
           MOVE       C0,TOTAL
           loop
           MOVE    "SearchOK-NDATSEQ",Location
           CALL      NDATSEQ
           Break      if over
           ADD        C1,TOTAL
           move       total,str9
           SETitem   Ndat0004StatRead,0,str9 
           call       CHKDates
           Repeat
...................................................................................................................................
          elseif    (PRIMTRACE = c9)           .Service B
.?
...................................................................................................................................
          elseif    (PRIMTRACE = c10)           .List Name


           getitem    NDat0004CheckBoxFreefloat,0,n1
           SETPROP    Ndat0004StatTextRecs,VISIBLE=C1
           SETPROP    Ndat0004StatRead,VISIBLE=C1
           MOVE       C0,TOTAL

NextName  loop
                add c1 to N9
        until (N9 > PRIMLIST)
                GETITEM Ndat0004DataListPrimary,n9,str35
           call       Trim using str35
           Clear      NDATFLD1
           Clear      NDATFLD2
           Clear      NDATFLD3
           Clear      NDATFLD4
           Clear      NDATFLD5
           Clear      NDATFLD6
           Clear      NDATFLD7
           Clear      NDATFLD8
           move       c1,Ndatpath
          move      c3,ndatlock
           if         (n1 = c1)
           pack       Ndatfld2 using "02F",str35
           else
           pack       Ndatfld2 using "02L",str35
           endif
           MOVE    "SearchOK-NDATAIM",Location
           CALL      NDATaim
           goto       nextname if over
           ADD        C1,TOTAL
           move       total,str9
           SETitem   Ndat0004StatRead,0,str9 
           call       chkdates

                      Loop
                      MOVE    "SearchOK-NDATKG",Location
                      call       NdatKG
                      
                      GOTO      NextName if over
                      ADD        C1,TOTAL
                      move       total,str9
                      SETitem   Ndat0004StatRead,0,str9 

                      call       chkdates
                      repeat
        repeat

          elseif    (PRIMTRACE = "11")           .List text
           getitem    NDat0004CheckBoxFreefloat,0,n1
           SETPROP    Ndat0004StatTextRecs,VISIBLE=C1
           SETPROP    Ndat0004StatRead,VISIBLE=C1
           MOVE       C0,TOTAL
           move       c1,Ndatpath
           move      c3,ndatlock

NextText  loop
                add c1 to N9
        until (N9 > PRIMLIST)
                GETITEM Ndat0004DataListPrimary,n9,str35
           call       Trim using str35

           Clear      NTXTFld1
           pack       NTXTFLD2,"02F",str35
           move       "OK-NINTextAIM",Location
           pack       KeyLocation,"Key: ",NtxtFld2
           call       NTXTAIM
           goto       nextTEXT if over
           packkey    NdatFld,NTXTLIST
           call       NdatKey                  
           ADD        C1,TOTAL
           move       total,str9
           SETitem   Ndat0004StatRead,0,str9 
           call       chkdates

                      loop
                                 until over
                                 call       NTXTKG
                                 GOTO      NextTExt if over
                                 scan       str35,ntxttext
                                if          equal
                                 packkey    NdatFld,NTXTLIST
                                 call       NdatKey                  
                                 ADD        C1,TOTAL
                                 move       total,str9
                                 SETitem   Ndat0004StatRead,0,str9 
           
                                 call       chkdates
                                 endif
                      repeat
           Repeat
          endif

           GOTO     finish
.............................................................................................................................
.following is old code


READNEXT1 loop
          CALL      NdatKG
          until over

         repeat          
          goto      Finish

READNEXT 
         CALL          NDATSEQ
         GOTO      FINISH IF OVER
.
RETBRNCH REPLACE   ZFILL IN REVDATE               SET UP DATE FOR LATER.
          UNPACK    REVDATE,CC,YEAR,MO,DAY
.
.         BRANCH    NUMPICK TO SETOWN,SETOWN,SETCAT,SETEXCL:
.                              SETNEW,SETDATE,SETWITH,SETDAT1,SetSB,setsrch
setsrch
.         BRANCH    REVBRCH TO SETYR,SETMO,SETDY  MATCHED. DATE CHECK?
         GOTO      MLROMITS                               NO.

*
*
. CATEGORIES RETRIEVAL...
.
SETCAT   RESET     CATPICK                       CHOSEN CATEGORIES
         MOVE      CATMASK TO CATS               SET UP THE SCAN STRING
........................................................................................
.NOTE:  THIS IS A CHEAT.  EACH DATACARD IS ALLOWED HUNDREDS OF CATEGORIES, BUT I WILL ONLY
.       ALLOW 10, UNTIL WE REWRITE THIS PROGRAM.
........................................................................................
          packkey   taskname,taskname
          clear     taskname
          pack      NCATFLD1,"01X",LSTNUM
          move      "NCATAIM",Location
          pack      KeyLocation,"Key: ",NCATFLD1
          call      NCATAIM
          for result,"1","10"
                    if over
                              break
                    endif
                    if (result <> 1)
                              append    DASH,taskname
                    endif
                    append    NCATCODE,taskname
                    append    NCATNUM,taskname
                    move      "NCATKG",Location
                    call      NCATKG
          repeat
          reset     taskname
          call      Trim using taskname
          pack      CATS,taskname
         BRANCH    OMITBRCH TO CHKANDOR          CATEGORY OMISSIONS?
*
. CATEGORY OMISSIONS...
.
         RESET     CATOMIT                       CHOSEN CATEGORY OMITS
NEXTOMIT
         MOVE      CATOMIT TO str3                MOVE TO 3 POS
         SCAN      str3 IN CATS                   IS IT IN THE RECORD?
         GOTO      READNEXT IF EQUAL             YES.
         BUMP      CATOMIT BY 3                  NO. MORE TO CHECK?
         GOTO      NEXTOMIT IF NOT EOS               YES.
.
CHKANDOR CMATCH    "A",ANDOR                         NO.
         GOTO      NEXTAND IF EQUAL
*
. 'OR'
.
NEXTOR
         MOVE      CATPICK,str3                   MOVE TO 3 POS
         SCAN      str3 IN CATS                   IS IT IN THE RECORD?
         GOTO      ENDCAT IF EQUAL               YES.
         BUMP      CATPICK BY 3                  NO. MORE TO CHECK?
         GOTO      READNEXT IF EOS                   NO.
         GOTO      NEXTOR                            YES.
*
.'AND'
.
NEXTAND
         MOVE      CATPICK,str3                   MOVE TO 3 POS
         SCAN      str3 IN CATS                   IS IT IN THE RECORD?
         GOTO      READNEXT IF NOT EQUAL         NO.
         RESET     CATS
         BUMP      CATPICK BY 3                  YES. MORE TO CHECK?
         GOTO      ENDCAT IF EOS                      NO.
         GOTO      NEXTAND                            YES.
.
ENDCAT   
.          BRANCH    REVBRCH TO SETYR,SETMO,SETDY  DATE CHECK?
         GOTO      MLROMITS                      NO.
*
*         
. OMIT RECORD BASED ON MLR/OFFER OMISSION...
.
MLROMITS BRANCH    MLRBRCH TO WRITEREC           ANY MLR OMISSIONS?
.
CHKORDRS CLEAR     NORDFLD1
         MOVE      MLROMIT TO MLR
         PACK      NORDFLD1 FROM OKEY1,MLR
         REP       ZFILL IN LSTNUM
         PACK      NORDFLD2 FROM OKEY2,LSTNUM
           CLEAR     NORDFLD3
         CLEAR     NORDFLD4
         DISPLAY   *P1:22,*EL,*HON,"FIRST ORDER READ FOR ##: ",LSTNUM:
                   "  KEYS= ",NORDFLD1,"  ",NORDFLD2,*W
           MOVE    C2 TO NORDPATH
           CALL    NORDAIM
         GOTO      NOHIT IF OVER                 NO MATCH.
         GOTO      CKORDPAR                      MATCH CHECK OTHER PAR.

NEXTORDR DISPLAY   *P1:22,*EL,*HON,"READ ORDER FILE GENERIC FOR ##: ",LSTNUM;
           CALL      NORDKG
         GOTO      NOHIT IF OVER                 NO MATCH.
         GOTO      CKORDPAR                       MATCH, CHECK OTHER PAR
CKORDPAR  BRANCH    MLRBRCH TO WRITEREC,CKORDRNG,CHKBYOFR
CHKBYOFR DISPLAY   *P1:22,*EL,*HON,"CHECKING OFFER";
         SCAN      OODNUM IN MLROMIT             IS OFR ONE OF OMITS?
         GOTO      CKORDRNG IF EQUAL             YES. CHECK ORDER DATE.
         GOTO      NEXTORDR                      NO. TRY NEXT ORDER.
CKORDRNG DISPLAY   *P1:22,*EL,*HON,"CHECKING ORDER DATE";
           MOVE      OODTEM TO MM
           MOVE      OODTED TO DD
           MOVE      OODTEY TO YY
         CALL      CVTJUL                        CONVERT TO JULIAN
         MOVE      JULDAYS TO ORDJUL
         COMPARE   LODATE TO ORDJUL              CHECK IF IN DTE RNG.
         GOTO      NEXTORDR IF LESS              NO. TRY NEXT ORDER.
         COMPARE   ORDJUL TO HIDATE
         GOTO      NEXTORDR IF LESS              NO. TRY NEXT ORDER.
         GOTO      OMITREC                       YES. OMIT THE RECORD.
. NOHIT
NOHIT    DISPLAY   *P1:22,*EL;
         BRANCH    MLRBRCH TO NOHIT1,NOHIT1,NOHIT2
. NOHIT1 OMIT BY MLR.
NOHIT1   BUMP      MLROMIT BY 5                  GET NEXT MLR TO CHECK.
         GOTO      NOHITW IF EOS                 NO MLR, WRITE RECORD.
         GOTO      NOHITW IF OVER                NO MLR, WRITE RECORD.
         GOTO      CHKORDRS                       MORE RECORDS TO CHECK.
. NOHIT2 OMIT BY MLR/OFFER.
NOHIT2
         BUMP      MLROMIT BY 8                  GET NEXT MLR/OFR TO CHK.
         GOTO      NOHITW IF EOS                 NO MLR, WRITE RECORD.
         GOTO      NOHITW IF OVER                NO MLR, WRITE RECORD.
         MOVE      MLROMIT TO NOFRFLD                PREP FOR OFFER MATCH.
         GOTO      CHKORDRS                       MORE RECORDS TO CHECK.
NOHITW   RESET     MLROMIT                       RESET FP FOR NEXT TIME.
         GOTO      WRITEREC                      GO WRITE REC.
OMITREC  RESET     MLROMIT                       RESET FP FOR NEXT TIME.
         DISPLAY   *P1:22,*EL;
          if        (numpick = c10)
         GOTO      READNEXT1                      NO.
          else
         GOTO      READNEXT                      YES. OMIT THE RECORD.
          endif
.....................................................................................................................
.the following is still in use
*
. WRITE THE OUTPUT RECORD...
.
WRITEREC
         READ      NEWMST,LSTNUM;;          .SKIP DUPES
           Return     if Not over
.begin patch 7.1
.dave goes amuck for the sake of alphabetizing
         match     "A " to mlstname
         call      fixitA if equal

         match     "The " to mlstname
         call      fixitThe if equal
.end patch 7.1
         WRITE     NEWMST,lstnum;DATVARS
         FLUSH     NEWMST
         ADD      C1 TO FOUND
         move         found,str9
         setItem      Ndat0004StatRecords,0,str9
           Return
.................................................................................
.CATdbLCLICK
CATdBLcLICK
           nDAT0004slISTvIEW1.GetNextItem GIVING N9 USING C2
           nDAT0004slISTvIEW1.GetItemText GIVING STR10 USING N9,2
           if         (primCat = yes)
        SetItem NDat0004EditSearchP,0,str10
           CALL       Click_NDat0004PrimaryAdd
           else
        SetItem NDat0004EditSearchS,0,str10
           CALL       Click_NDat0004SecondaryAdd
           endif
           return
.................................................................................
FileGo
.Flag set to "N" if in Modify or New mode
.        branch result to FileGo1,FileGo2,FileGo3,FileGo3
        branch result to FileGo1
FileGo1
          call click_NDat0004Exit
        RETURN
Optionsgo
        return
ViewGo
        return
EditGo
        return
HelpGo
        branch  result to HelpGo1,HelpGo2
HelpGo1
        setprop AboutMssg,visible=1
        return
HelpGo2
        execute   "c:\progra~1\Intern~1\iexplore.exe http://web01/?page_id=268"  

        return
SearchGo
        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4,SearchGo5,SearchGo6,SearchGo7,SearchGo8
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
SearchGo5
.SHIP-TO
        move    C5,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo6
.OWNER
              move            C6,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchGo7
.Fulfillment RTN - 
              move            C7,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchGo8
.Service B
              move            C8,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return

SearchLoad
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6,SearchLoad7,SearchLoad8
SearchLoad1
.BROKER- not an option with this program
        return
SearchLoad2
.LIST
        unpack srchstr,str6,str1,str35,str1,str10
        if (secflag = NO)
                  setitem NDat0004EditSearchP,0,str6
                  setfocus NDat0004EditSearchP
        else
                  setitem NDat0004EditSearchS,0,str6
                  setfocus NDat0004EditSearchS
        endif
        return
SearchLoad3
.MAILER - not an option with this program
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
.patch5.11
SearchLoad5
.CAMPAIGN - not an option with this program
        return
SearchLoad6
.OWNER - 
               unpack         Srchstr,str4,str1,str25
        if (SECFLAG = NO)
               setitem        NDat0004EditSearchP,0,str4
                    setfocus NDat0004EditSearchP
           Else
               setitem        NDat0004EditSearchS,0,str4
                    setfocus NDat0004EditSearchS
              endif
        return
SearchLoad7
.Fulfillment RTN - 
               unpack         Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
        if (SECFLAG = NO)
               setitem        NDat0004EditSearchP,0,str6
                    setfocus NDat0004EditSearchP
           Else
               setitem        NDat0004EditSearchS,0,str6
                    setfocus NDat0004EditSearchS
              endif
        return
SearchLoad8
.Service B 
               unpack         Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
        if (SECFLAG = NO)
               setitem        NDat0004EditSearchP,0,str6
                    setfocus NDat0004EditSearchP
           Else
               setitem        NDat0004EditSearchS,0,str6
                    setfocus NDat0004EditSearchS
              endif
        return
LiveBut
          setprop ColPrim,enabled=c1
          setprop ColSec,enabled=c1
          setprop ColRadate,enabled=c1
          setprop ColCombDate,enabled=c1
          setprop ColFilter,enabled=c1
          setprop ColPrint,enabled=c1
          setprop ColSort,enabled=c1
          setprop NDat0004PickOff,enabled=c1
          call dATAsetmousefree
   return
dATASetMouseBusy
        setmode *mcursor=*wait
        return
DataSetMouseFree
        setmode *mcursor=*arrow
        return
AddData
.Adds data in datalist from Primary pick
          getitem NDat0004DataListPrimary,c1,n2
          getitem NDat0004EditSearchP,0,str10
          if (str10="")
          alert caution,"Item added to list cannot be blank. Please try again",result,"Attempted Blank Entry"
                setfocus NDat0004EditSearchP
          return
        endif
          insertitem NDat0004DataListPrimary,9999,str10
          setfocus NDat0004EditSearchP
          return
DelData
.delets data in datalist from primary pick
          getitem NDat0004DataListPrimary,0,n2
          if (n2=c0)
                    alert type=yesno1," Do you want to delete all of the entries in the datalist?", result,"Clear DataList"
                              if (result=6)    . 6 = yes , 7 = no
                              deleteitem NDat0004DataListPrimary,n2
                              return
                        else
                              alert note,"Select a field in the Data Field, then click Remove or Press Delete.",result,"Select Field"
                                return
                              endif
          endif
          deleteitem NDat0004DataListPrimary,n2
          return
SecAddData
.Adds data in datalist from Secondary pick
          getitem NDat0004DataListSecondary,c1,n2
          getitem NDat0004EditSearchS,0,str10
          if (str10="")
          alert caution,"Item added to list cannot be blank. Please try again",result,"Attempted Blank Entry"
                setfocus NDat0004EditSearchS
          return
        endif
          insertitem NDat0004DataListSecondary,9999,str10
          setfocus NDat0004EditSearchS
          return
SecDelData
.deletes data in datalist from Secondary pick
          getitem NDat0004DataListSecondary,0,n2
          if (n2=c0)
                    alert type=yesno1," Do you want to delete all of the entries in the datalist?", result,"Clear DataList"
                              if (result=6)    . 6 = yes , 7 = no
                              deleteitem NDat0004DataListSecondary,n2
                              return
                        else
                              alert note,"Select a field in the Data Field, then click Remove or Press Delete.",result,"Select Field"
                                return
                              endif
          endif
          deleteitem NDat0004DataListSecondary,n2
          return
GetDate
.Beginning Date
        getitem Ndat0004ComboYear1,n4,n3
        getitem Ndat0004ComboYear1,n3,str4
        unpack str4 to CC,YY
        getitem Ndat0004ComboMo1,n4,n3
        getitem Ndat0004ComboMo1,n3,str2
        move str2 to MM
        call zfillit using mm
        getitem Ndat0004ComboDay1,n4,n3
        getitem Ndat0004ComboDay1,n3,str2
        move str2 to DD
        call zfillit using DD
        call cvtjul
        move juldays to KEEPDATE1
.Ending Date
        getitem Ndat0004ComboYear2,n4,n3
        getitem Ndat0004ComboYear2,n3,str4
        unpack str4 to CC,YY
        getitem Ndat0004ComboMo2,n4,n3
        getitem Ndat0004ComboMo2,n3,str2
        move str2 to MM
        call zfillit using mm
        getitem Ndat0004ComboDay2,n4,n3
        getitem Ndat0004ComboDay2,n3,str2
        move str2 to DD
        call zfillit using DD
        call cvtjul
        move juldays to KEEPDATE2
        return
.=================================================================================
CHKDATES
.also check withdrawn
           if         (withdraw = c0)       .default omit withdrawn
           CMATCH    "W" TO STATUS                  THIS ONE WITHDRWN?
           Return     IF EQUAL                 YES.
           CMATCH    "T" TO STATUS
           Return     IF EQUAL                 YES.
           endif
.if not all dates
          if (DateGroup <> c0)
                     call      OMRDate
                             COMPARE   KEEPDATE1 TO juldays
                            Return IF LESS              NO. TRY NEXT.
                            COMPARE   juldays TO KEEPDATE2
                            return IF LESS              NO. TRY NEXT.
          endif

SECONDSEARCH
           if (second = YES)
                      move       c0,n9               .reset
                      if        (Sectrace = c2)      .owner   1=blank
NextOwner2
           loop
                add c1 to N9
           until (N9 > SecoLIST)
                   if      (lstnum = "019647")   
                  call     debug    
                  endif
                GETITEM Ndat0004DataListSecondary,n9,str6
                    IF        (str6 <> ownnum)
                    goto       NextOwner2
                    Else         
                    Goto         WriteREc
                    endif
           repeat
           return   
                      elseif    (Sectrace = c3)      .cat
                      elseif    (Sectrace = c4)      .New
                                 if         (NLSTCDE = YES)
                                 goto       WriteREc
                                 endif                                 
                      elseif    (Sectrace = c5)      .S.B.
.?                      
                      elseif    (Sectrace = c6)      .Mailer Omit
NextMlr
                      loop
                      add c1 to N9
                      until (N9 > SecoLIST)
                      GETITEM Ndat0004DataListSecondary,n9,MLR
                      PACK      NORDFLD1 FROM OKEY1,MLR
                      REP       ZFILL IN LSTNUM
                      PACK      NORDFLD2 FROM OKEY2,LSTNUM
                      CLEAR     NORDFLD3
                      CLEAR     NORDFLD4
                      MOVE    C2 TO NORDPATH
                      CALL    NORDAIM
                      if         OVer
                      goto    NextMlr
                      else
                      return
                      endif
                      repeat
                      goto       Writerec
                      endif
           else
           goto       writerec

           endif                      
           Return
        
OMRDate
.get date from datacard
         branch  dategroup to REv,Putup,Upd
         return
REv      unpack revdate into str2,yy,mm,dd
         call      cvtjul
         return
Putup    unpack NEWDATE into str2,yy,mm,dd
         call          cvtjul
         return
UPd     unpack NDATUPDDATE into str2,yy,mm,dd
         call          cvtjul
         return
..........................................................................................................
DataSort
.SortGROUP        0-alpha 1-asis  2-numeric  3-LO, alpha
.sort it
           if         (sortGroup = c0)     
           pack       taskname from NTWKPATH1,shortnme,".dat,",NTWKPATH1,shortnme,".srt -64-138"
           sort       Taskname,SUNDM="NINS1:502"
           pack       str25 from NTWKPATH1,shortnme,".srt"
           elseif     (sortGroup = c1)     
           pack       Str55 from "e:\data\",shortnme,".dat|nins1:502"
           pack       taskname from "e:\data\",shortnme,".srt|nins1:502"
           CopyFile   str55,Taskname
           pack       str25 from NTWKPATH1,shortnme,".srt"
           elseif     (sortGroup = c2)     
           pack       taskname from NTWKPATH1,shortnme,".dat,",NTWKPATH1,shortnme,".srt -2-7"
           sort       Taskname,SUNDM="NINS1:502"
           pack       str25 from NTWKPATH1,shortnme,".srt"
           else
           pack       taskname from NTWKPATH1,shortnme,".dat,",NTWKPATH1,shortnme,".srt -8-13,64-138"
           sort       Taskname,SUNDM="NINS1:502"
           pack       str25 from NTWKPATH1,shortnme,".srt"
           endif
           return
..........................................................................................................
CATLoadListView

           Ndat0004SListView1.InsertItem giving N9 using Nrefdesc
           Ndat0004SListView1.SetItemText using N9,Nrefnum,2
 
           cmatch     "C",NREFNUM
           if         equal
           Ndat0004SListView1.SetItemText using N9,"Consumer",1
           endif
           cmatch     "B",NREFNUM
           if         equal
           Ndat0004SListView1.SetItemText using N9,"Business",1
           endif
           cmatch     "E",NREFNUM
           if         equal
           Ndat0004SListView1.SetItemText using N9,"Enhanced",1
           endif

           return
..........................................................................................................
           INCLUDE   NORDIO.INC
           INCLUDE   NOWNIO.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
           INCLUDE   NDATIO.inc
           INCLUDE   NOFRIO.INC
           INCLUDE   NCATIO.INC
          INCLUDE   NREFIO.INC
         include   nuseio.inc
.begin patch 8.1
           include    Ntxtio.inc
.end patch 8.1
..............................................................
.Following used only in order to load Search.plf
        include    searchio.inc      .contains logic for search.plf
        include       ncmpio.inc
        include       nrtnio.inc        
...............................................................
         include   ncntio.inc

           INCLUDE   COMLOGIC.INC
