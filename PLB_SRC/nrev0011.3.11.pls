.Brokerage Rental\Exchange & List Management Print Program
.Actual vs. Actual

PC         EQU       0
           include common.inc
           include cons.inc
.patch1.7
          include   compdd.inc
          include   cntdd.inc
         INCLUDE   NXRFDD.INC
.           include nmlrdd.inc
.Pastch1.7
           include nprjdd.inc
           include ndatdd.inc
           include nrevdd.inc
.==============================================================================
Release   Init             "3.11"    DLH .new Year
reldate   Init      "2016 January 5"
.Release   Init             "3.1"    DLH .new Year
.reldate   Init      "2015 January 5"
.Release   INit      "3.0"   DLH   print to PDF & Email, reformat report output
.Reldate   Init      "2014 September"
.Release   Init             "2.17"    DLH .new Year
.reldate   Init      "2014 January 2"
.Release   Init             "2.16"    DLH Correct key fld for company read on cold new biz brokerage
.reldate   Init      "2012 July 20"
.Release   Init             "2.15"    DLH Use AR instead of LR, use dsinit to get date from batch job and run without intervention, Yearly
..                                                 Update to use new company number
.reldate   Init      "2011 Feb 02"
.Release    Init             "2.14"    DLH yearly
.reldate   Init      "Dec 1 2010"

.Release    Init             "2.13"    DLH yearly
.reldate   Init      "Oct 01 2009"
.Release    Init             "2.12"    DLH 03/28/2007       Send Mail
.Release    Init             "2.11"    JD 01/20/2006        Yearly Update
.Release    Init             "2.1"    JD 11/30/2005         Added close of input file before next sort.
.Release    Init             "2.01"    JD 08/01/2005        Fixed nxrfmlr read.
.Release    Init             "2.00"    JD 05/31/2005        Check company file for new business date
.Release    Init             "1.91"   DMB 01/13/2005        Yearly Update
.Release    Init             "1.9"   ASH 11/18/2004         Mailer conversion - increase to 6 bytes
.Release    Init             "1.8"   ASH 08/09/2004         Logo Conversion
.Release    Init             "1.7"   DMB 05/26/04 Mailer Conversion
.Release    Init             "1.6"   DB 04/13/02  added code to automate job
.Release    Init             "1.5"   DB 09/30/02  Added Getwinver subroutine to for XP boxes
.Release    Init             "1.4"  DB 01/14/01  Added sonic and progress bar for aesthetics
..Release    Init             "1.3"  02Jan2002  DLH  email totals from cold new biz reps
..RELEASE   INIT              "1.2"  DB 12/28/01   Code added to reflect change in location of revenue file
..RELEASE   INIT              "1.1"  DB 12/07/01   Code for Listmgnt LIST Report added (loopflg=4)
..RELEASE   INIT              "1.0"  DB 10/30/01  Actuals& Colds(list mgnt,rental,exch) Print program released
.ListView1  ListView
INFILE     FILE
.begin patch 2.15
Auto      Dim       1
str2a     Dim       2
.end patch 2.15
..REDFILE    IFILE
.PROFILE    IFILE
.===========================================================
.OUTPUT     IFILE     KEYLEN=6,FIX=380     .KEY=Client #
TOTYRS     FORM     10.2
.===========================================================
.Printfile
prfile     PFILE
.===================================================================================
.Projected Totals
..PROFILE    IFILE
.projdolr   iFILE     keylen=8,FIX=274
..projvars  list
..projfld   dim      8
.TYPE     DIM      1       1-1   ---\
.SOURCE   DIM       1      2-2------->  KEY=nrevfld
.CLIENTID DIM    6          3-8------/
..CLIENT1   DIM       45     9-53
..projdol  form      11          -64
..proj93   form      11          -75
..proj94   form      11          -86
..proj95   form      11          -97
..proj95a  form      11          -108
..proj96   form      11          -119
..proj97   form      11          -130
..proj97a  form      11          -141
..proj98   form      11          -152
..proj98a  form      11          -164
..proj99   form      11          -175
..proj99old   form      11          -186
..proj2000   form      11          -197
..nin2000    form      11           208
..proj2001   form      11          -219
..nin2001    form      11           230
..proj2001a   form      11          -219
..nin2001a    form      11           230
..proj2002   form      11          -141
..nin2002    form      11           274
..         listend
.=====================================================================================
MONTHX     FORM    10.2
.************************************************************************************
.change yearly----
.Need to change proj20** as well
.Patch 1.91
.LastYr        INIT      "2001"
.LastYr        INIT      "2004"
.begin  patch 2.13
.LastYr        INIT      "2006"
.ThisYr        INIT   "2007"
.begin  patch 2.14
.begin  patch 2.17
.LastYr        INIT   "2012"
.ThisYr        INIT   "2013"
.begin patch 3.1 increment yearly
LastYr        INIT   "2015"                 .previous year
ThisYr        INIT   "2016"                 .current year   
.end patch 3.1 increment yearly
.end  patch 2.17
.LastYr        INIT      "2008"
.ThisYr        INIT   "2009"
.end  patch 2.14
.end  patch 2.13
.Patch 2.11
.LastYr        INIT      "2005"
.LastYr        INIT      "2006"
.ThisYr        INIT   "2002"
.ThisYr        INIT   "2005"
.ThisYr        INIT   "2006"
.ThisYr        INIT   "2007"
.Patch 2.11
.Patch 1.91
.*********************************************************************************
.=====================================================================================
LMLIST     INIT   "018710"
LMLIST2    INIT   "024593"
SORTHLP    form   10.2        .add to ThisYr total in hidden coulumn to be used as sorted column
SORTSTR    DIM     13
HIDYR      FORM    10.2
TMPVAR     FORM    10.2
TMPVAR1    FORM    10.2
TMPVAR2    FORM    10.2
LastYrTOT     FORM    10.2
ThisYrTOT     FORM    10.2
DIFTOT     FORM    10.2
OLDTOT     FORM    10.2
.====================================================================================
.Months of the Year
c12       form   "12"
CurMo     dim    10
Month1    init   "January"
Month2    init   "February"
Month3    init   "March"
Month4    init   "April"
Month5    init   "May"
Month6    init   "June"
Month7    init   "July"
Month8    init   "August"
Month9    init   "September"
Month10   init   "October"
Month11   init   "November"
Month12   init   "December"
TitleYR   dim     27
TitleLastYr  dim     12
.====================================================================================
.FLags and Counters
mtchflg  form    1             .flag if client\list a match or not
.osflag   form    1             . 1=win 95,98, 2=NT
Rowcount form    3             .KEEP TRACK OF ROW PER PAGE
PgCnt    form    9             .COUNT OF PAGES
NEWPG    FORM    1             .COUNTER TO SHOW- IF NEW PAGE PUT TOTALS AT TOP OF PAGE
TOTREC   form    9             .Keeps track of total records on report
LoopFlg  form    1             .flag to keep track of which report is running 1-rental 2-exch 3 -list mgnt
ChkAll   form    1             .Check to see if All reports was selected
ChkOne   form    1             .Check to see if Indiv report was selected
COPY     form    3
one     form    "1"
.=============================================================
Title1   form    9
Title2   form    9
.Title3   form    9
Title4   form    9
font9    font
        create  font9,"Times New Roman",size=10,italic
*******************************************************************************
.some goodies for on moving Icon
ICONID   FORM      4
CURICON  FORM      4
FRAMES   FORM      4
FORM32   FORM      32
FORM32A  FORM      32
FORM32B  FORM      32
.
CurRec    form    5.2
CurVal    form      3
LastVal   form      3
.
.begin   release 1.3
.tempfile      file
.end   release 1.3
.end icon goodies
*******************************************************************************
.==============================================================================
.Sort Parameters=======================================================
INDAT    init  "revenue.dat"   .File to be sorted
.INDAT    init  "\\nins1\e\data\dbase\revenue.dat"   .File to be sorted
OUTSRT   init  "revenue.srt"   .Sorted Output file
.OUTSRT   init  "\\nins1\e\data\dbase\revenue.srt"   .Sorted Output file
.ClintSrt init  "3-8"                        .Sort by client #
ClintSrt init  "3-12"                        .Sort by client # & Year
.SORTVAR   INIT     "\\nins1\e\data\salesref.dat,\\nins1\e\data\salesref.srt;28-72,s=1='06'&73='0901'|1='06'&73='0900'"
SORTFLE   dim    250                           .Var to pack file names of sort
PRTITLE   DIM    18
PRTNAME1  DIM    11
PRTDIR    INIT    "C:\WORK\"                                      ."
PRTFILE1  DIM    19
holdmlr   dim    6
.======================================================================
PDF_FLAGS_WIN_ANSI_ENCODING EQU 128
....................................................................
.hold vars for projection
projnew   form      11          hold exchange portion  LR
projlast  form      11          hold exchange portion  LR  for Previous Yearr
newbiz    init      "N"
check    form      5
check2   form      5 
today1   form      5
revdat   form      5
mmrep    dim       2
yyrep    dim       2
ccrep    dim       2
nmmrep   dim       2
.......................................................................
..............................................................
.Menu
.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu
.Present Data for Menu Bar
.FData   init    "&File;&Print;Pre&view;-;E&xit"
FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search;-;&Preferences"
HData   init    "&Help;&About"
.=====================================================================
        move    "NREV0011.PLS",Wprognme
        move    "Broker/List Mgmnt Actuals",Wfunction
        move    "David Baca",Wauthor
        move    release,Wrelease
        move    Reldate,Wreldate
.=======================================================================
abt     plform  About
x         plform  nrev0011
.x        plform  nerep0001
.==========================================================================
.added 11/19/01
        winhide
.=====================================
        formload x
        formload abt
********************************************************************************
.==========================================================================
  create  NREV0011;mFile,FData
  create  NREV0011;mEdit,EData,mFile
  create  NREV0011;mOptions,OData,mEdit
  create  NREV0011;mHelp,HData,mOptions
.Activate Menus
.FileGo leads to stop
  activate mFile,FileGo,result
.Need this when it works
  activate mEdit,EditGo,result
  activate mOptions,OptionsGo,result
.Only a SubMenu under this one
  activate mHelp,HelpGo,result
.==============================================================================
.        create  ListView1=20:20:20:20,APPEARANCE=1:
.         BORDER=1,BGcolor=color1,VIEWSTYLE=3:
.        sortheader=1,sortorder=2
.        activate ListView1
        ListView1.InsertColumn using "",1,1
.        ListView1.InsertColumn using "2005",100,2
.        ListView1.InsertColumn using "2004",100,3
.begin patch 2.13
.        ListView1.InsertColumn using "2006",100,2
.        ListView1.InsertColumn using "2005",100,3
        ListView1.InsertColumn using ThisYr,100,2
        ListView1.InsertColumn using LastYr,100,3
.end patch 2.13
        ListView1.InsertColumn using "Mailer Name",100,4
        ListView1.InsertColumn using "Mailer ",30,5
        ListView1.InsertColumn using "Difference",100,6
        Listview1.setcolumnformat giving result using 0,0
        Listview1.setcolumnformat giving result using 1,1
        Listview1.setcolumnformat giving result using 2,1
        move    "100",column
.begin patch 3.0
        move    "4000",column1
        move    "5500",column2
        move    "7950",column3
.        move    "3350",column1
.        move    "4900",column2
.        move    "6900",column3
.end patch 3.0
        move    "3000",Title1
        move    "6600",Title2
        move    "3800",Title4
.==============================================================================
.
          If        (program = "NREV0011")           .chained from dsinit
          Unpack    Today into mm,str1,dd,str1,yy
          move      "20",cc
          MOve      Yes,Auto
          else
         CLOCK   TIMESTAMP to str8
         unpack  str8,cc,yy,mm,dd
          endif
.Patch2.0
                      move      cc to ccrep
                                move    mm to mmrep
                      move    yy to yyrep
        CALL      CVTJUL
         MOVE      juldays TO TODAY1
        move    mm to nmm
                      move    nmm to nmmrep
.Patch2.0
.===========================================================
        move    nmm to str2
        rep     " 0",str2
        setitem NerepEditMo,0,str2
        setitem NerepEditCOPY,0,"1"
        getinfo  system,str6
        unpack   str6 into str1,str2
        unpack   str2 into str1
        move     c0 to osflag
.Patch1.5
        call getwinver
.subpatch1.5
        move c1 to ndatpath
.Patch1.6
.begin patch 2.15
          If        (auto = Yes)
          call      Entry
          goto      Click_NerepExit
          else
        loop
                 waitevent
        repeat
          endif
.end patch 2.15
.Patch1.6
Entry
.==================================================
        clear   chkall
        getprop NerepRadioAll,SELGROUPID=loopflg         0-all 1-rental 2-exch 3-list mgnt
.                                                                    4-Cold Brokerage 5- Cold List Management
.==================================================
.checker
        call OrderSetMouseBusy
        getitem NerepEditMo,0,str2              .get month for report
        move str2 to nmm
        if ((nmm > c12) | (nmm < c0) | (nmm = c0))
               alert caution,"Not a vaild Month!",result,"Invalid"
               call OrderSetMouseFree
               return
        endif
        if (loopflg = C0)
                move c1 to chkall
        endif
Checker
.========================================================================================
.Loops to produce the reports
        if (chkall = c1)
                add c1 to loopflg
        endif
        Listview1.deleteallitems
          clear mtchflg
          clear Rowcount
          clear PgCnt
          clear NEWPG
          clear TOTREC
          clear SORTHLP
          clear SORTSTR
          clear HIDYR
          clear TMPVAR
          clear TMPVAR1
          clear TMPVAR2
          clear LastYrTOT
          clear ThisYrTOT
          clear DIFTOT
        clear totyrs
.====================================================================
***********************************************************************************

.============================================================================================
Looper
        Branch LOOPFLG to Sorter,Sorter2,Sorter3,Sorter4,Sorter5,Sorter6
.        Branch LOOPFLG to Sorter,Sorter2,Sorter3,Sorter4
.Rental
.==========================================================================================
SORTER
         PACK  PRTITLE,"Brokerage Rental"
         PACK  PRTNAME1,"BRKRENT.LST"
.begin patch 1.2
         pack   SortFle,ntwkpath6,indat,comma,ntwkpath1,outsrt
           pack   taskname,sortfle,";",clintsrt,comma,"s=1=","'","RB","'","&9=","'",LastYr,"'","|","1=","'","RB","'","&9=","'",ThisYr,"'"
         sort   taskname,sundm="NINS1:502"
         if over
               alert caution,S$ERROR$,result,"No Sort"
               call OrderSetMouseFree
               return
         endif
         goto OpenIt
.=================================================================
.Exchange
.==========================================================================================
SORTER2
         PACK  PRTITLE,"Brokerage Exchange"
         PACK  PRTNAME1,"BRKEXCH.LST"

         pack   SortFle,ntwkpath6,indat,comma,ntwkpath1,outsrt
.         pack   SortFle,indat,comma,outsrt
           pack   taskname,sortfle,";",clintsrt,comma,"s=1=","'","EB","'","&9=","'",LastYr,"'","|","1=","'","EB","'","&9=","'",ThisYr,"'"
         sort   taskname,sundm="NINS1:502"
         if over
               alert caution,S$ERROR$,result,"No Sort"
               call OrderSetMouseFree
               return
         endif
         goto OpenIt
.=================================================================
.LISTMANAGEMENT
.==========================================================================================
SORTER3
         PACK  PRTITLE,"List Management"
         PACK  PRTNAME1,"LSTMGNT.LST"

         pack   SortFle,ntwkpath6,indat,comma,ntwkpath1,outsrt
.         pack   SortFle,indat,comma,outsrt
           pack   taskname,sortfle,";",clintsrt,comma,"s=1=","'"," M","'","&9=","'",LastYr,"'","|","1=","'"," M","'","&9=","'",ThisYr,"'"
         sort   taskname,sundm="NINS1:502"
         if over
               alert caution,S$ERROR$,result,"No Sort"
               call OrderSetMouseFree
               return
         endif
         goto OpenIt
.=================================================================
.List Management - List
.==========================================================================================
SORTER4
         PACK  PRTITLE,"List Mngmnt-List"
         PACK  PRTNAME1,"LMLIST.LST"
         goto OpenIt
.=================================================================
.COLDNEW Brokerage
.==========================================================================================
SORTER5
         PACK  PRTITLE,"Cold Brokerage"
         PACK  PRTNAME1,"CLDBRK.LST"

         pack   SortFle,ntwkpath6,indat,comma,ntwkpath1,outsrt
.         pack   SortFle,indat,comma,outsrt
           pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","B","'","&9=","'",LastYr,"'","|","2=","'","B","'","&9=","'",ThisYr,"'"
         sort   taskname,sundm="NINS1:502"
         if over
               alert caution,S$ERROR$,result,"No Sort"
               call OrderSetMouseFree
               return
         endif
         goto OpenIt
.====================================================================================
.COLDNEW List Management
.==========================================================================================
SORTER6
         PACK  PRTITLE,"Cold List Mgnt"
         PACK  PRTNAME1,"CLDLMT.LST"

         pack   SortFle,ntwkpath6,indat,comma,ntwkpath1,outsrt
.         pack   SortFle,indat,comma,outsrt
           pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","M","'","&9=","'",LastYr,"'","|","2=","'","M","'","&9=","'",ThisYr,"'"
         sort   taskname,sundm="NINS1:502"
         if over
               alert caution,S$ERROR$,result,"No Sort"
               call OrderSetMouseFree
               return
         endif
.====================================================================================
OpenIt
.         OPEN    REDFILE,"\\nins1\e\data\dbase\revenue"
..        OPEN    PROFILE,"c:\work\Projdolr"
.         OPEN    PROFILE,"\\nins1\e\data\Projdolr"
*********************************************************************
          call      NREVInitProgressBar
.         setitem   NMDLStatProgress,0,""
.              setitem        NMDLStatProgress,0,"Loading Instructions"
.              pack str45 with ntwkpath1,outsrt
              pack str45 with outsrt,"|NINS1:502"
.              move          "\\nins1\e\data\text\NINMDLST.dat" to str45
              open          tempfile,str45
          positeof      tempfile
          fposit    tempfile,N10
.begin patch 3.0
          calc      howmany=(N10/723)   .'723 = 721(Revenue record length) + 2 bytes for CR/LF
.end patch 3.0
              close         tempfile
*********************************************************************

.          OPEN    INFILE,outsrt
          OPEN    INFILE,str45
.         OPEN    INFILE,"revenue.srt"
.         OPEN    INFILE,"\\nins1\e\data\dbase\revenue.srt"
.EndPatch 1.2
          move    "1000000000.00" to sorthlp
        setitem NerepStatTitle,0,prtitle
        if (loopflg = c4)
                goto reader4
        endif
READER
        loop
....................................................................................
        clear   monthx
...................................................................................
        READ    INFILE,SEQ;NREVVARS
        goto     Print if over
**************************************
          CALL   NREVUpdateProgressBar
**************************************
.1-Brk rental 2-Brk exch 3-list mgnt 4-Cold Brokerage 5- Cold List Management
        BRANCH loopflg to Reader1,Reader2,Reader3,Reader4,Reader5,Reader6
.        BRANCH loopflg to Reader1,Reader2,Reader3,Reader4
.Reader 1 Brokerage Rental
READER1
.Begin patch 2.15
          REset     RUNCODES
          scan      Cid,runcodes
          goto      reader if equal
.end patch 2.15

                              call      newbus
         cmatch  TYPE to "R"                    .Rental
        goto    READER if not equal
        cmatch  SRC to "B"
        goto    READER if not equal
        if      (YR0 = LastYr | YR0 = ThisYr)
.begin patch 2.15
.                LOAD MONTHX using NMM from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
.                SEPLR,OCTLR,NOVLR,DECLR
                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                SEPAR,OCTAR,NOVAR,DECAR
.end patch 2.15
        endif
        goto listview
.Reader 2 Brokerage Exchange
READER2
                              call    newbus
                              cmatch  TYPE to "E"                 .Exchange
        goto    READER if not equal
        cmatch  SRC to "B"
        goto    READER if not equal
        if      (YR0 = LastYr | YR0 = ThisYr)
.begin patch 2.15
.                LOAD MONTHX using NMM from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
.                SEPLR,OCTLR,NOVLR,DECLR
                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                SEPAR,OCTAR,NOVAR,DECAR
.end patch 2.15
        endif
        goto listview
.======================================================================================
.Reader 3 List Management
READER3
.Begin patch 2.15
          REset     RUNCODES
          scan      Cid,runcodes
          goto      reader if equal
.end patch 2.15
         move      cid to nxrffld
         CALL      NXRFkey
.Patch2.01
.         move     NXRFMLR to omlrnum
          move      NXRFMLR,COMPFLD
          move      "fulfxit-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
.       PACK      MKEY FROM omlrnum,"000"
.Patch2.01
.        unpack    nxrfmlr,str2,str4
.                 PACK      MKEY FROM nxrfmlr,"000"
.                 PACK      MKEY FROM str4,"000"
.Patch2.01
.                                                                   CALL      NMLRKEY
.Patch2.0
       unpack   COMPCNTDATE INTO cc,yy,mm,dd
                    move     "31" to dd
         type       yy
         if        equal
         CALL      CVTJUL
         MOVE      juldays TO revdat
                              move      revdat to check
         move      today1 to check2
         SUB       check FROM CHECK2
         compare   "365" to check2           .usage in last year
         if LESS
                              move       yes to newbiz
                              move     nmmrep to nmm
                              move     yyrep to yy
                              move     ccrep to cc
                              goto       reader
                              endif
                              endif
                              move     nmmrep to nmm
                              move     yyrep to yy
                              move     ccrep to cc
.Patch2.0

        cmatch  TYPE to " "                 .List Management
        goto    READER if not equal
        cmatch  SRC to "M"
        goto    READER if not equal
        if      (YR0 = LastYr | YR0 = ThisYr)
.begin patch 2.15
.                LOAD MONTHX using NMM from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
.                SEPLR,OCTLR,NOVLR,DECLR
                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                SEPAR,OCTAR,NOVAR,DECAR
.end patch 2.15
        endif
        goto listview
.========================================================================================
.reader 4 cold new brokerage
Reader4
.List Management List
        clear   mtchflg
        clear   tmpvar1
        clear   tmpvar2
        clear   monthx
.Begin patch 2.15
          REset     RUNCODES
          scan      Cid,runcodes
          goto      reader if not equal
.end patch 2.15
.Begin patch 2.15
          REset     EXFEELST
          scan      Cid,EXFEELST
          goto      reader if not equal
.end patch 2.15
.Begin patch 2.15
        if      (YR0 = LastYr | YR0 = ThisYr)
                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                SEPAR,OCTAR,NOVAR,DECAR
        endif
          goto      Listview


.        pack    nrevfld,b1,"M",LMLIST,LastYr
.
.        call    nrevkey
.        if over
.                goto Reader4a
.        endif
.        move     c1 to mtchflg
.        if      (YR0 = LastYr)
..                FOR n9,"1",result
..begin patch 2.15
..                LOAD MONTHX using NMM from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
..                SEPLR,OCTLR,NOVLR,DECLR
.                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
.                SEPAR,OCTAR,NOVAR,DECAR
..end patch 2.15
.                    ADD MONTHX to tmpvar1
..                repeat
.        endif
.                    move tmpvar1 to str13
.                Listview1.InsertItem giving N7 using ""
.          Listview1.SetItemText giving N8 using n7,str13,2
.Reader4a
.        clear   tmpvar2
.        pack    nrevfld,b1,"M",LMLIST,ThisYr
.        call    nrevkey
.        if over
..                goto Print
..                 goto CAL4
.                    goto      Reader4b
.        endif
.        if      (YR0 = ThisYr)
..                FOR n9,"1",result
..begin patch 2.15
..                LOAD MONTHX using NMM from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
..                SEPLR,OCTLR,NOVLR,DECLR
.                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
.                SEPAR,OCTAR,NOVAR,DECAR
..end patch 2.15
.                    ADD MONTHX to tmpvar2
..                repeat
.        endif
.        move    tmpvar2 to str13
.        if (mtchflg = c1)
.          Listview1.SetItemText giving N8 using n7,str13,1
.        else
.          Listview1.InsertItem giving n7 using str13
.          Listview1.SetItemText giving N8 using n7,"0",2
.          Listview1.SetItemText giving N8 using n7,str13,1
.        endif
.Reader4b
.        clear   tmpvar1
.        pack    nrevfld,b1,"M",LMLIST2,LastYr
.        call    nrevkey
.        if over
.                    goto      Reader4c
.        endif
.        if      (YR0 = LastYr)
..begin patch 2.15
..                LOAD MONTHX using NMM from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
..                SEPLR,OCTLR,NOVLR,DECLR
.                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
.                SEPAR,OCTAR,NOVAR,DECAR
..end patch 2.15
.                    ADD MONTHX to tmpvar1
.        endif
.        move    tmpvar1 to str13
.        if (mtchflg = c1)
.          Listview1.SetItemText giving N8 using n7,str13,1
.        else
.          Listview1.InsertItem giving n7 using str13
.          Listview1.SetItemText giving N8 using n7,"0",2
.          Listview1.SetItemText giving N8 using n7,str13,1
.        endif
.Reader4c
.        clear   tmpvar2
.        pack    nrevfld,b1,"M",LMLIST2,ThisYr
.        call    nrevkey
.        if over
.                    goto      Reader4c
.        endif
.        if      (YR0 = ThisYr)
..begin patch 2.15
..                LOAD MONTHX using NMM from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
..                SEPLR,OCTLR,NOVLR,DECLR
.                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
.                SEPAR,OCTAR,NOVAR,DECAR
..end patch 2.15
.                    ADD MONTHX to tmpvar2
.        endif
.        move    tmpvar2 to str13
.        if (mtchflg = c1)
.          Listview1.SetItemText giving N8 using n7,str13,1
.        else
.          Listview1.InsertItem giving n7 using str13
.          Listview1.SetItemText giving N8 using n7,"0",2
.          Listview1.SetItemText giving N8 using n7,str13,1
.        endif
.        clear  mtchflg
..=======================================================================
.Cal4
.          sub   TMPVAR1,TMPVAR2
.          move  TMPVAR2 to str13
.          Listview1.SetItemText giving N8 using n7,str13,5
.        goto   print
.end patch 2.15

.        goto listview
.======================================================================================
.reader 4 cold new Management
READER5

.Begin patch 2.15
.          REset     RUNCODES
.          scan      Cid,runcodes
.          goto      reader if equal
.end patch 2.15
       match      "010735" to cid
                      call       debugger if equal
          packkey    Compfld,Cid
          call      Compkey
.        unpack    cid,str2,str4
.        PACK      MKEY FROM str4,"000"
.                                                                    CALL      NMLRKEY
.Patch2.0
                      move      no to newbiz
       unpack   COMPCNTDATE INTO cc,yy,mm,dd
                    move     "31" to dd
         type       yy
         if        equal
         CALL      CVTJUL
         MOVE      juldays TO revdat
                              move      revdat to check
         move      today1 to check2
         SUB       check FROM CHECK2
         compare   "365" to check2           usage in last year
         if LESS
                              move       yes to newbiz
                              move     nmmrep to nmm
                              move     yyrep to yy
                              move     ccrep to cc
                              goto      coldone
                              else
                              move     nmmrep to nmm
                              move     yyrep to yy
                              move     ccrep to cc
                              goto     reader
                              endif
                              endif
coldone
.Patch2.0
         if      (CID = "000001")              .Cold New Brokerage uses Exch&Rent
                goto reader
        endif
        clear   mtchflg
        clear   tmpvar
        clear   tmpvar1
        clear   tmpvar2
        clear   OLDTOT
        if (CID = holdmlr)                      .check to see if mlr\lst already processed
                goto reader
        endif
        move    CID to holdmlr
        cmatch  SRC to "B"
        goto    READER if not equal
        if      (YR0 = ThisYr)
                pack nrevfld,type,src,cid,yr0
                    goto ThisYrStart
        endif
        if      (YR0 = LastYr)
                 pack  nrevfld,type,src,cid,yr0
ADDOLD
.begin patch 2.15
.                add JANLR,OLDTOT
.                add FEBLR,OLDTOT
.                add MARLR,OLDTOT
.                add APRLR,OLDTOT
.                add MAYLR,OLDTOT
.                add JUNLR,OLDTOT
.                add JULLR,OLDTOT
.                add AUGLR,OLDTOT
.                add SEPLR,OLDTOT
.                add OCTLR,OLDTOT
.                add NOVLR,OLDTOT
.                add DECLR,OLDTOT
                add JANAR,OLDTOT
                add FEBAR,OLDTOT
                add MARAR,OLDTOT
                add APRAR,OLDTOT
                add MAYAR,OLDTOT
                add JUNAR,OLDTOT
                add JULAR,OLDTOT
                add AUGAR,OLDTOT
                add SEPAR,OLDTOT
                add OCTAR,OLDTOT
                add NOVAR,OLDTOT
                add DECAR,OLDTOT
.end patch 2.15
.Patch2.0
                    if (newbiz = yes)
                                                            goto checker2
                                                            else
                    if (OLDTOT <> C0) 
                              goto reader
                  endif
                                                            endif
.===================================================================================
checker2
.Patch2.0
                  if (mtchflg = c1)
                        move c0 to mtchflg
.                   if (OLDTOT <> C0 | newbiz = NO)          .if total for prev year is not zero read next rec
.                                                           move     no to newbiz
.                   goto reader
.Patch2.0
                    if (newbiz = yes)
                    goto checker3
                    else
                              if (OLDTOT <> C0) 
                              goto reader
                              endif
                    endif
.                        else
checker3
.                   move     no to newbiz
                                    unpack  nrevfld,type,src,cid,yr0
                              move      ThisYr to yr0
                                    pack    nrevfld,type,src,cid,yr0
                                    clear   tmpvar1
                              goto nextread
.                        endif
                endif
.Patch2.0
                unpack  nrevfld,type,src,cid,yr0
                if (type = "R")
                        move "E" to type
                else
                        move "R" to type
                endif
                pack    nrevfld,type,src,cid,yr0
                call    NREVKEY
.                 READ    REDFILE,NREVFLD;REVVARS
                if not over
                        move c1 to mtchflg
                        goto ADDOLD
                else
                        move c0 to mtchflg
.Patch2.0
                    if (newbiz = yes)
                                                            goto checker4
                                                            else
                    if (OLDTOT <> C0) 
                              goto reader
                  endif
                                                            endif
.                   if (OLDTOT <> C0 )          .if total for prev year is not zero read next rec
.                                                           move     no to newbiz
.                   goto reader
.                        else
.                                                                     move  no to newbiz
checker4
                                                        unpack  nrevfld,type,src,cid,yr0
                              move      ThisYr to yr0
                                    pack    nrevfld,type,src,cid,yr0
                                    clear   tmpvar1
                              goto nextread
                        endif
.                endif
.Patch2.0

NextRead
                call    NREVKEY
.                 READ    REDFILE,NREVFLD;REVVARS

.=============================================================================
                if over
                        if     (mtchflg = c1)
                                move c0 to mtchflg
                              goto Projread
                        endif
                else
ThisYrStart
.begin patch 2.15
.                LOAD MONTHX using NMM from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
.                SEPLR,OCTLR,NOVLR,DECLR
                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                SEPAR,OCTAR,NOVAR,DECAR
.end patch 2.15
                        add   monthx to tmpvar1
                        move    tmpvar1 to monthx
                        if    (mtchflg = c1)
                                move c0 to mtchflg
                                  goto Projread
                        endif
                endif
                          unpack  nrevfld,type,src,cid,yr0
                    if (type = "R")
                              move "E" to type
                              else
                              move "R" to type
                    endif
                          pack    nrevfld,type,src,cid,yr0
                        move  c1 to mtchflg
                        goto    nextread
Projread
                if (MONTHX < c1)
                        goto reader
                endif
                  unpack  nrevfld,type,src,cid,yr0
.                pack    Projfld with TYPE,SRC,CID
Projread1
***************************************************************************************
*Code for new projection file
          move      c0,projlast
.         move      c0,ninlast
          move      c0,projnew
.         move      c0,ninnew
          if (src = "B")
.START PATCH 1.9 ADDED LOGIC
.begin patch 2.16  20July12
                    Packkey  Compfld,Cid
                    move      "COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call     Compkey

.                    unpack    CID,str2,str4
.                    pack      COMPFLD3,str4
.                    move      "COMPKEY3",Location
.                    pack      KeyLocation,"Key: ",COMPFLD3
.                    call      COMPKEY3
.end patch 2.16  20July12
.END PATCH 1.9 ADDED LOGIC
                              move      "EB",str2
                    move      C0,N2
                    loop
                              add       C1,N2
                              move      N2,str2a
                              rep       zfill,str2a
.START PATCH 1.9 ADDED LOGIC
.                             pack      NPRJFLD,str2,cid,YR0,str3
.                              pack      NPRJFLD,str2,COMPCOMP,YR0,str3
                              pack      NPRJFLD,str2,CID,YR0,str2a
.END PATCH 1.9 ADDED LOGIC
..                            pack      NPRJFLD,str2,str6,PrevYear,str3
                              rep       zfill,NPRJFLD
..                            move      "detout-NPRJKEY",Location
..                            pack      KeyLocation,"Key: ",NPRJFLD
                              call      NPRJKEY
                              until over
.begin patch 2.15
.                            move        ProjLR,projnew
                            move        PrjAR,projnew
..                          move        ProjNIN,ninnew
.                              until (ProjMast = YES)
                              until (PrjMast = YES)
.end patch 2.15
                    repeat
                    move      "RB",str2
                    move      C0,N2
                    loop
                              add       C1,N2
                              move      N2,str2a
                              rep       zfill,str2a
.START PATCH 1.9 ADDED LOGIC
.                             pack      NPRJFLD,str2,cid,YR0,str3
.                              pack      NPRJFLD,str2,COMPCOMP,YR0,str3
                              pack      NPRJFLD,str2,CID,YR0,str2a
.END PATCH 1.9 ADDED LOGIC
..                            pack      NPRJFLD,str2,str6,PrevYear,str3
                              rep       zfill,NPRJFLD
..                            move      "detout,2-NPRJKEY",Location
..                            pack      KeyLocation,"Key: ",NPRJFLD
                              call      NPRJKEY
                              until over
.begin patch 2.15
.                            move        ProjLR,projlast
                            move        PrjAR,projlast
..                          move        ProjNIN,ninlast
.                              until (ProjMast = YES)
                              until (PrjMast = YES)
.end patch 2.15
                    repeat
          endif
                    add     projnew,projlast,tmpvar2
**************************************************************************************
.Projected for current year
Project
.Patch2.0
                    if (newbiz = yes | tmpvar2 = c0)
.                  move      mcomp to client
                    Move      CompCOmp,Client
                                                            move   no to newbiz
                  goto coldlistview
                                                            else
                                                            move  no to newbiz
                                                            goto reader
                                                            endif
.                        if (tmpvar2 = c0)
                                                            
.=============================================================================
.write out
.                                move      mcomp to client
.                                 goto coldlistview
.                        else
.                                goto reader
.                        endif
           endif
.Patch2.0

.===========================================================================
.======================================================================================
.Cold New List Management
READER6
.        if      ((CID = "010125")|(CID = "013398")|(CID = "018710")|(CID = "003591")|:              .Cold New Brokerage uses Exch&Rent
.                (CID = "011722")|(CID = "011060")|(CID = "007107")|(CID = "020411")|:
.                (CID = "005086"))
.Begin patch 2.15
          REset     RUNCODES
          scan      Cid,runcodes
          goto      reader if equal
.end patch 2.15
.Begin patch 2.15
          REset     EXFEELST
          scan      Cid,EXFEELST
          goto      reader if equal
.                goto reader
.        endif
.end patch 2.15

         move      cid to nxrffld
         CALL      NXRFkey
.Patch2.01
.         move     NXRFMLR to omlrnum
          move      NXRFMLR,COMPFLD
          move      "fulfxit-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
.       PACK      MKEY FROM omlrnum,"000"
.Patch2.01
.                                                                   CALL      NMLRKEY
.Patch2.0
          if        (Cid = "002700")
          call      Debug
          endif
                              move      no to newbiz
       unpack   COMPCNTDATE INTO cc,yy,mm,dd
                    move     "31" to dd
         type       yy
         if        equal
         CALL      CVTJUL
         MOVE      juldays TO revdat
                              move      revdat to check
         move      today1 to check2
         SUB       check FROM CHECK2
         compare   "365" to check2           usage in last year
         if LESS
                              move       yes to newbiz
                              move     nmmrep to nmm
                              move     yyrep to yy
                              move     ccrep to cc
                              goto      coldone2
                              else
                              move     nmmrep to nmm
                              move     yyrep to yy
                              move     ccrep to cc
                              goto     reader
                              endif
                              endif
coldone2
.Patch2.0
        clear   mtchflg
        clear   tmpvar
        clear   tmpvar1
        clear   tmpvar2
        clear   OLDTOT
        if (CID = holdmlr)                      .check to see if mlr\lst already processed
                goto reader
        endif
        move    CID to holdmlr
        cmatch  SRC to "M"
        goto    READER if not equal
        if      (YR0 = ThisYr)                           .ThisYr=current year
                pack nrevfld,type,src,cid,yr0
                    goto ThisYrStart1
        endif
        if      (YR0 = LastYr)                           .LastYr=Last year
                 pack  nrevfld,type,src,cid,yr0
ADDOLD1
.begin patch 2.15
.                add JANLR,OLDTOT
.                add FEBLR,OLDTOT
.                add MARLR,OLDTOT
.                add APRLR,OLDTOT
.                add MAYLR,OLDTOT
.                add JUNLR,OLDTOT
.                add JULLR,OLDTOT
.                add AUGLR,OLDTOT
.                add SEPLR,OLDTOT
.                add OCTLR,OLDTOT
.                add NOVLR,OLDTOT
.                add DECLR,OLDTOT
                add JANAR,OLDTOT
                add FEBAR,OLDTOT
                add MARAR,OLDTOT
                add APRAR,OLDTOT
                add MAYAR,OLDTOT
                add JUNAR,OLDTOT
                add JULAR,OLDTOT
                add AUGAR,OLDTOT
                add SEPAR,OLDTOT
                add OCTAR,OLDTOT
                add NOVAR,OLDTOT
                add DECAR,OLDTOT
.end patch 2.15
.                   if (OLDTOT <> C0)
.                             goto reader
.                endif
.Patch2.0
                    if (newbiz = yes)
                                                            goto checker5
                                                            else
                    if (OLDTOT <> C0) 
                              goto reader
                  endif
                                                            endif
checker5
.Patch2.0
.==================================================================================
.                if (mtchflg = c1)
.                        move c0 to mtchflg
.                   if (OLDTOT <> C0)          .if total for prev year is not zero read next rec
.                             goto reader
.                        else
.                                   unpack  nrevfld,type,src,cid,yr0
.                             move      ThisYr to yr0
.                                   pack    nrevfld,type,src,cid,yr0
.                                   clear   tmpvar1
.                             goto nextread1
.                        endif
.                endif
.                unpack  nrevfld,type,src,cid,yr0
.                if (type = "R")
.                        move "E" to type
.                else
.                        move "R" to type
.                endif
.                pack    nrevfld,type,src,cid,yr0
.                 READ    REDFILE,NREVFLD;REVVARS
.                if not over
.                        move c1 to mtchflg
.                        goto ADDOLD1
.                else
.                        move c0 to mtchflg
.                   if (OLDTOT <> C0)          .if total for prev year is not zero read next rec
.                             goto reader
.                        else
                                    unpack  nrevfld,type,src,cid,yr0
                              move      ThisYr to yr0
                                    pack    nrevfld,type,src,cid,yr0
                                    clear   tmpvar1
                              goto nextread1
.                        endif
.                endif

NextRead1
                call    NREVKEY
.                 READ    REDFILE,NREVFLD;REVVARS

.=============================================================================
                if over
.                        if     (mtchflg = c1)
.                                move c0 to mtchflg
                                clear monthx
                              goto Projread2
.                        endif
                else
ThisYrStart1
.begin patch 2.15
.                          LOAD MONTHX using NMM from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
.                  SEPLR,OCTLR,NOVLR,DECLR
                          LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                  SEPAR,OCTAR,NOVAR,DECAR
.end patch 2.15
                        add   monthx to tmpvar1
                        move    tmpvar1 to monthx
.                        if   (mtchflg = c1)
.                                move c0 to mtchflg
                                  goto Projread2
.                        endif
                endif
.                         unpack  nrevfld,type,src,cid,yr0
.                   if (type = "R")
.                             move "E" to type
.                             else
.                             move "R" to type
.                   endif
.                         pack    nrevfld,type,src,cid,yr0
.                        move           c1 to mtchflg
.                        goto    nextread1
Projread2
                if (MONTHX < c1)
                        goto reader
                endif
                  unpack  nrevfld,type,src,cid,yr0
.                pack    Projfld with TYPE,SRC,CID
Projread3
.                 READ    PROFILE,PROJFLD;PROJVARS
**********************************************************************************
*Code for new projection file
          move      c0,projlast
          if (src = "M")
                    move      " M",str2
                    move      C0,N2
                    loop
                              add       C1,N2
                              move      N2,str2a
                              rep       zfill,str2a
                              pack      NPRJFLD,str2,CID,YR0,str2a
..                            pack      NPRJFLD,str2,str6,PrevYear,str3
..                            rep       zfill,NPRJFLD
..                            move      "detout,3-NPRJKEY",Location
..                            pack      KeyLocation,"Key: ",NPRJFLD
.                             if nprjfld = (" M203100200301")
.tester
.                                       reset nprjfld                                                            
.                             endif
                              call      NPRJKEY
                              until over
.begin patch 2.15
.                            move        ProjLR,projlast
                            move        PrjAR,projlast
.                           move        ProjNIN,ninlast
.                              until (ProjMast = YES)
                              until (PrjMast = YES)
.end patch 2.15
                    repeat
                move    projlast,tmpvar2
          endif
*********************************************************************************
.                if over
.                        if (mtchflg = c1)
.                                 move c0 to mtchflg
                          goto  project1
.                         endif
.                else
.Change hard coded var-needs to be changed yearly
.=============================================================================
.                        add proj2001 to tmpvar2
.=============================================================================
.                        if (mtchflg = c1)
.                                move c0 to mtchflg
.                             goto project1
.                        endif
.                endif
.                         unpack  projfld,type,src,cid
.                   if (type = "R")
.                             move "E" to type
.                             else
.                             move "R" to type
.                   endif
.                         pack    nrevfld,type,src,cid
.                        move           c1 to mtchflg
.                        goto    projread3

.Projected for current year
Project1
.Patch2.0
                    if (newbiz = yes | tmpvar2 = c0)
                               move holdmlr to NDATFLD
                               call NDATKEY
                               move holdmlr to cid
                               move OLSTNAME to client
                                                            move   no to newbiz
                  goto coldlistview
                                                            else
                                                            move  no to newbiz
                                                            goto reader
                                                            endif
.                       if (tmpvar2 = c0)
.=============================================================================
.write out
.                               move holdmlr to NDATFLD
.                               call NDATKEY
.                               move holdmlr to cid
.                               move OLSTNAME to client
.===================================================================
.                        goto coldlistview
.                       else
.                               goto reader
.                       endif
        endif

.Patch2.0
.===========================================================================


ColdListview
.=============================================================================
        clear hidyr
        add  sorthlp,monthx,hidyr
        move hidyr to str13
        Listview1.InsertItem giving N7 using str13
        move monthx to str14
        Listview1.SetItemText using n7,str14,1
        move holdmlr to cid
        goto defaulter
.=====================================================================================
.======================================================================================
Listview

        Listview1.GetItemCount giving result
        sub c1 from result
        for n9,"0",result
        Listview1.GetItemText giving str6 using n9,4
        if (CID = str6)                      .check to see if mlr\lst already in listview
                MOVE C1 TO MTCHFLG
                if (YR0 = LastYr)
.==========================================================================
.get item and reinsert for proper sorting

        Listview1.GetItemText giving str15 using n9,0
        Listview1.GetItemText giving str13 using n9,1
        Listview1.GetItemText giving str45 using n9,3
        Listview1.GetItemText giving str6 using n9,4
        Listview1.deleteitem giving n1 using n9
        Listview1.InsertItem giving n7 using str15
        Listview1.SetItemText giving N8 using n7,str13,1
        Listview1.SetItemText giving N8 using n7,str45,3
        Listview1.SetItemText giving N8 using n7,str6,4
.======================================================================================
.calculate difference of two years
        Listview1.GetItemText giving str13 using n9,1
                       move  str13 to TMPVAR
                       sub   monthx,TMPVAR,TOTYRS
                       move  TOTYRS to str13
        Listview1.SetItemText giving N8 using n9,str13,5
.======================================================================================
                        move monthx to str14
                        Listview1.SetItemText giving N8 using n9,str14,2
                endif
                if (YR0 = ThisYr)
.delete and insert new item for current year
.==========================================================================

        Listview1.GetItemText giving str15 using n9,2
        Listview1.GetItemText giving str45 using n9,3
        Listview1.GetItemText giving str6 using n9,4
        Listview1.deleteitem giving n1 using n9
.==================================
                        clear hidyr
                        add  sorthlp,monthx,hidyr
                        move hidyr to str13
.==================================
        Listview1.InsertItem giving n7 using str13
        Listview1.SetItemText giving N8 using n7,str15,2
        Listview1.SetItemText giving N8 using n7,str45,3
        Listview1.SetItemText giving N8 using n7,str6,4
.======================================================================================
                        move monthx to str14
                        Listview1.SetItemText giving N8 using n7,str14,1
.======================================================================================
        Listview1.GetItemText giving str13 using n7,2
                       move  str13 to TMPVAR
                       sub   TMPVAR,monthx,TOTYRS
                       move  TOTYRS to str13
        Listview1.SetItemText giving N8 using n7,str13,5
.======================================================================================
                endif
        endif
        repeat
                if (mtchflg = c1)        .already rewritten to data list
                        move c0 to mtchflg     .yes
                        goto Reader
                endif
                if (YR0 = LastYr)         .write new rec for last year
                        move sorthlp to sortstr
                        Listview1.InsertItem giving N7 using sortstr
                              Listview1.SetitemText using n7,".00",1
                        move monthx to str14
                              Listview1.SetitemText using n7,str14,2
.formatting
                        if (str14 = "")
                                        Listview1.SetitemText using n7,".00",5
.==================================================================
                        else
.Calculate diff
                              move  C0 to TMPVAR
                                        sub   monthx,TMPVAR,TOTYRS
                                        move  TOTYRS to str13
                                        Listview1.SetItemText giving N8 using n7,str13,5
.=====================================================================
                        endif
                endif
                if (YR0 = ThisYr)
                        clear hidyr
                        add  sorthlp,monthx,hidyr
                        move hidyr to str13
                        Listview1.InsertItem giving N7 using str13
                        move monthx to str14
                        Listview1.SetItemText using n7,str14,1
                              Listview1.SetitemText using n7,".00",2
.-==============================================================================
                        clear str13
                        move  monthx to str13
                        Listview1.SetItemText giving N8 using n7,str13,5
                endif
                        if (loopflg = c3)     .if listmgmt listname from datacard file
                                move CID to NDATFLD
                                call NDATKEY
                                if not over
                                        Listview1.SetItemText using N7,OLSTNAME,3
                                        goto ID
                                else
                                        goto defaulter
                                endif
                        else                            ..if rental\exch brokerage
.                              unpack    CID,str2,str4
.                                      PACK      MKEY FROM str4,"000"
.                                      CALL      NMLRKEY
                               Packkey  Compfld,Cid
                               call     Compkey
checkc
                            if not over
.                                        Listview1.SetItemText using N7,MCOMP,3
                                        Listview1.SetItemText using N7,COMPComp,3
                                        goto ID
                                else
                                        goto defaulter
                              endif
                        endif
Defaulter
                              Listview1.SetItemText using N7,CLIENT,3

ID                      Listview1.SetItemText using N7,CID,4
.                if (YR0 = ThisYr)
.                        move  monthx to str13
.                        Listview1.SetItemText giving N8 using n7,str13,5
.                else
.                       move  str13 to TMPVAR
.                       sub   monthx,TMPVAR,TOTYRS
.                       move  TOTYRS to str13
.                          Listview1.SetItemText giving N8 using n7,str13,5
.                endif

        repeat

.        loop
.        waitevent
.        repeat


.==================================================================
Print
.patch2.1
                              close   infile
.patch2.1
         clear n9
                PACK PRTFILE1,PRTDIR,PRTNAME1
.patch1.5

          if (osflag >= c6) 
                    PRTOPEN   prfile,"\\nins2\laser8",PRTFILE,noprint,spoolfile=PRTFILE1
.          elseif (osflag = c3 | osflag =c4)         .win 95 98
.                    PRTOPEN   prfile,"Laser8",PRTFILE,noprint,spoolfile=PRTFILE1
.          elseif (osflag = "9" )         .win 7
.                    PRTOPEN   prfile,"\\NINs2\Laser8",PRTFILE,noprint,spoolfile=PRTFILE1
          else   .(osflag = c0)         .Don't know prompt for printer
                    PRTOPEN   prfile,"",PRTFILE,noprint,spoolfile=PRTFILE1
          endif

.=================================================================
.Headers
.Defining Header and Titles
Page
        CLEAR     ROWCOUNT
        ADD       C1 TO PGCNT
.begin patch 3.0                 
           if         (pgcnt = c1)
           prtpage   prfile;*UNITS=*HIENGLISH;
           else
           prtpage   prfile;*NEWPAGE:
                   *UNITS=*HIENGLISH;
           endif
.        prtpage   prfile;*NEWPAGE:
.                   *UNITS=*HIENGLISH;
.end patch 3.0                   
.======================================================================
        clear     row
        move      "300",row
.======================================================================
        prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date:";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*font=font12,str10;
.        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,"Revenue 2001";
        pack titleLastYr,"Receivables ",ThisYr
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,titleLastYr;
        add     eightlpi,row
        add     "30",row
        branch  loopflg to Rent1,Exch1,ListM,LISTMLIST,Cldbr,CldLM
Rent1
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,"Brokerage Rental";
        goto        next
Exch1
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,"Brokerage Exchange";
        goto        next
ListM
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,"List Management";
        goto        next
Listmlist
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,"List Management-List";
        goto        next
.============================================================================
Cldbr
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,"Brokerage";
        goto    next
.============================================================================
CldLM
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,"List Management";
.============================================================================
        goto    next
Next
        add     eightlpi,row
        add     "50",row
.        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,"Actual 2000 vs. Actual 2001";
        if (loopflg > C4)
                  prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,"Cold New Business";
        else
          pack    titleyr,"Actual ",LastYr," vs. ","Actual ",ThisYr
                  prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,titleyr;
        endif
        add     eightlpi,row
        add     "30",row
        Load    curmo,nmm with Month1,Month2,Month3,Month4,Month5,Month6,Month7:
                Month8,Month9,Month10,Month11,Month12
        call    trim using curmo
          prtpage prfile;*pTitle4:Row,*ALIGNMENT=*CENTER,*font=font12,*boldon,curmo,*boldoff;
.        prtpage prfile;*pTitle4:row,*ALIGNMENT=*Center,*font=font12,curmo;
        add     eightlpi,row
        add     "30",row

.============================================================================================
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*OVERLAYOff,*boldon,*ULON,"Client",*ULOFF,*boldoff;
        prtpage prfile;*pColumn1:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,ThisYr,*ULOFF,*boldoff;
        if (loopflg > c4)
               goto print1a
        endif
        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,LastYr,*ULOFF,*boldoff;
        prtpage prfile;*pColumn3:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"Difference",*ULOFF,*boldoff;
Print1a
        add     "20",row
.================================================================================================
        Listview1.GetItemCount giving result
        sub c1 from result
Print1
        loop
          until (N9 > result)
                CLEAR STR13
                CLEAR SORTSTR
.==============================================================================
        if (loopflg = c3)
                    Listview1.GetItemText giving STR6 using n9,4
.temp
.          if (str6 = "018710")     .list management exch fee list #
.                              add c1 to n9
.                    goto Print1
.                    endif
        endif
.================================================================================
.        FOR n9,"0",result
                  Listview1.GetItemText giving str13 using n9,1
                clear tmpvar1
                move str13 to tmpvar1
                add tmpvar1 to ThisYrTOT
                CALL TRIM USING STR13
                if (loopflg > c4)
                        goto Cont
                endif
          Listview1.GetItemText giving SORTSTR using n9,2
                clear tmpvar2
                  move sortstr to tmpvar2
                    add tmpvar2 to LastYrTOT
                CALL TRIM USING SORTSTR
                  IF (STR13 = SORTSTR)
.============================================================================
                        move str13 to n10
                            IF (n10 = c0)
                                        add c1 to n9
                                        goto Print1
                        else
                                goto cont
                        endif
                else
.=========================================================================

Cont                    add c1 to rowcount
                        add c1 to totrec
                            Listview1.GetItemText giving STR45 using n9,3
                    Listview1.GetItemText giving STR6 using n9,4
.=========================================================================
                    if (loopflg > c4)
                              goto Record
                    endif
                    Listview1.GetItemText giving STR15 using n9,5
                        clear tmpvar1
                    move str15 to tmpvar1
                    add tmpvar1 to DIFTOT

.=====================================================================================
Record
                              add     eightlpi,row
                              add     "35",row
.Client
                  prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,str45;
. Current year
                    call trim using str13
.begin patch 3.0                    
                      move       str13,str11           
                      move       str11 to tmpvar1
                      if         (tmpvar1 < c0)
                      edit       tmpvar1,str14,mask="-ZZ,ZZZ,ZZZ.99",Align=2
                      elseif     (tmpvar1 = c0)
                      move       c0,str14
                      else
                      edit       tmpvar1,str14,mask="ZZZ,ZZZ,ZZZ.99",Align=2
                      endif
.                     prtpage    prfile;*pcolumn1:row,*OVERLAYON,*font=font12,*ALIGNMENT=*right,str13;
                      prtpage    prfile;*pcolumn1:row,*OVERLAYON,*font=font12,*ALIGNMENT=*right,str14;
.end patch 3.0                    
                        if (loopflg > c4)
                              goto Rower
                    endif
. PRevious year
                  call trim using SORTSTR
.begin patch 3.0                    
                      move       sortstr,str11         
                      move       str11 to tmpvar1
                      if         (tmpvar1 < c0)
                      edit       tmpvar1,str14,mask="-ZZ,ZZZ,ZZZ.99",Align=2
                      elseif     (tmpvar1 = c0)
                      move       c0,str14
                      else
                      edit       tmpvar1,str14,mask="ZZZ,ZZZ,ZZZ.99",Align=2
                      endif
.                    prtpage prfile;*pcolumn2:row,*font=font12,*ALIGNMENT=*right,sortstr;
                      prtpage    prfile;*pcolumn2:row,*OVERLAYON,*font=font12,*ALIGNMENT=*right,str14;
.end patch 3.0                    
.difference
                          call trim using str15
.begin patch 3.0                    
                      move       str15,str11           
                      move       str11 to tmpvar1
                      if         (tmpvar1 < c0)
                      edit       tmpvar1,str14,mask="-ZZ,ZZZ,ZZZ.99",Align=2
                      elseif     (tmpvar1 = c0)
                      move       c0,str14
                      else
                      edit       tmpvar1,str14,mask="ZZZ,ZZZ,ZZZ.99",Align=2
                      endif
.                  prtpage prfile;*pcolumn3:row,*font=font12,*ALIGNMENT=*right,str15;
                      prtpage    prfile;*pcolumn3:row,*OVERLAYON,*font=font12,*ALIGNMENT=*right,str14;
.end patch 3.0                    
.=========================================================================================
Rower
            if (ROWCOUNT = "49")
                              move "10200",row
                           prtpage prfile;*pcolumn:row,*OVERLAYOff,*font=font12,*ALIGNMENT=*Left,"Page# ";
                           prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.8 REPLACED LOGIC
.                          prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News/CA";
                           prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News";
.END PATCH 1.8 REPLACED LOGIC
.======================================================================================
                       add c1 to n9
                       goto page
.======================================================================================
                    endif


.=======================================================================================
                                  add c1 to n9

                    endif
        repeat

.=======================================================================================
       if (rowcount = c0)
.                goto  print11
.                pause c10
       endif
ENDLOOP
       if (ROWCOUNT < "46")
                 goto totals
       else

.Added to correct page # and page label for next to last page
.==========================================================================================
                 move "10200",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.8 REPLACED LOGIC
.                prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News/CA";
                 prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News";
.END PATCH 1.8 REPLACED LOGIC
.===========================================================================================
               move c1 to newpg
               add  c1 to pgcnt
               prtpage prfile;*NEWPAGE:
                              *UNITS=*HIENGLISH:
                                    *ORIENT=*PORTRAIT;
               goto Totals
       endif

TOTALS
       if (newpg = c1)
                 move "520",row
       endif
               add     eightlpi,row
               add     "50" to row
.begin patch 3.0
.               prtpage prfile;*p3350:row,*pensize=10,*line=7600:row;
               prtpage prfile;*p3350:row,*pensize=10,*line=8100:row;
.end patch 3.0
               add     "30" to row
               prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Grand Totals",*boldoff,*ULOFF;
               clear str13
               move ThisYrTOT to str13
               call trim using str13
               prtpage prfile;*pcolumn1:row,*OVERLAYON,*font=font12,*ALIGNMENT=*Right,*boldon,str13,*boldoff;

.begin patch 3.0
.              If            (loopflg = 5)          .cold new biz brokerage rental
.          Move      "NRev0011 - output",MailSubjct
.          Move      "DavidHerrick@nincal.com",MailFrom
.          Move      "DavidHerrick@nincal.com",MailTo
.          Clear     MailBody
.          Append    "Grand Totals ",MailBody
.          Append    CRLF,Mailbody
..              move          "Nrev0011 - output" to SmtpSubject Subject
..         append         "Grand Totals ",SmtpTextMessage(1)   Array <Text message >
..              reset          smtpTextMessage(1)
.          append    "Cold Brokerage Rental: ",Mailbody 
.          Append    CRLF,Mailbody
.          Append    Str13,Mailbody
.          Append    CRLF,Mailbody
.          Reset     Mailbody  
.
..         Move       "Cold Brokerage Rental: ",SmtpTextMessage(2)   Array <Text message >
..         Move       str13,SmtpTextMessage(3)   Array <Text message >
..
..         Move       "3",SmtpTextIndexLast                               Index to last entry in TextMessage array
..         move       "DHerric" to str45
..         move       "David Herrick" to str55
..         call       Mailmesg
.          call      SendMail
.              endif
.              If            (loopflg = 6)          .cold new biz List Management
..              move          "Nrev0011 - output" to SmtpSubject Subject
..         append         "Grand Totals ",SmtpTextMessage(1)   Array <Text message >
..              reset          smtpTextMessage(1)
.          Move      "NRev0011 - output",MailSubjct
.          Move      "DavidHerrick@nincal.com",MailFrom
.          Move      "DavidHerrick@nincal.com",MailTo
.          Clear     MailBody
.          Append    "Grand Totals ",MailBody
.          Append    CRLF,Mailbody
.          Append    "Cold List Management: ",Mailbody
.          append    CRLF,Mailbody
.          Append    Str13,Mailbody
.          Append    CRLF,Mailbody
.          Reset     Mailbody  
..         Move       "Cold List Management: ",SmtpTextMessage(2)   Array <Text message >
..         Move       str13,SmtpTextMessage(3)   Array <Text message >
.
..         Move       "3",SmtpTextIndexLast                               Index to last entry in TextMessage array
..         move       "DHerric" to str45
..         move       "David Herrick" to str55
..         call       Mailmesg
.          call      SendMail
.              endif
.end patch 3.0
               if (loopflg > c4)
                       goto Print2
               endif
               clear str13
               move LastYrTOT to str13
               call trim using str13
               prtpage prfile;*pcolumn2:row,*Overlayon,*font=font12,*ALIGNMENT=*right,*boldon,str13,*boldoff;
               clear str13
               move DIFTOT to str13
               call trim using str13
               prtpage prfile;*pcolumn3:row,*Overlayon,*font=font12,*ALIGNMENT=*Right,*boldon,str13,*boldoff;
               add     eightlpi,row
               add     eightlpi,row
                 prtpage prfile;*pcolumn:row,*Overlayoff,*font=font12,*ALIGNMENT=*Left,"Total Records:  ";
               clear str9
               move TOTREC to str9
               call trim using str9
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,STR9;
.=============================================================
.Footer for Last Page
Print2
                 move "10200",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.8 REPLACED LOGIC
.                prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News/CA"
                 prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News"
.END PATCH 1.8 REPLACED LOGIC
.=================================================================================================

End1
        PRTCLOSE prfile
        getitem NerepEditCOPY,0,str3
        move str3 to copy
        if (copy = c0)
          goto recall
        endif
        clear n3
.begin patch 3.00 use pdf regardless of os
.        if (osflag = "1" or osflag = "5" or osflag = "6")
.begin patch 3.00  turn off copies
.                    loop
.                    until (N3 = COPY)
.                              PRTPLAY PRTFILE1,"\\nins2\laser8"
.============================================================================

                                 if         (Loopflg = c1)
                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\BrokerageRental.pdf"
                                Pause     "10"
                      Move      "Brokerage Rental PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\BrokerageRental.pdf",MailAttach
                                 
                                 Elseif     (Loopflg = c2)                       
                                 
                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\BrokerageExchange.pdf"
                                Pause     "10"
                      Move      "Brokerage Exchange PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\BrokerageExchange.pdf",MailAttach

                                 Elseif     (Loopflg = c3)                       
                                 
                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\Management.pdf"
                                Pause     "10"
                      Move      "Management  PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\Management.pdf",MailAttach

                                 Elseif     (Loopflg = c4)                       
                                 
                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\ManagementList.pdf"
                                Pause     "10"
                      Move      "Management Exchange PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\ManagementList.pdf",MailAttach

                                 Elseif     (Loopflg = c5)                       
                                 
                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\ColdBrokerage.pdf"
                                Pause     "10"
                      Move      "Cold Brokerage PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      "SusanAnstrand@nincal.com,SuzieMcGuire@nincal.com",MailCC
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\ColdBrokerage.pdf",MailAttach

                                 Elseif     (Loopflg = c6)                       
                                 
                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\ColdManagement.pdf"
                                Pause     "10"
                      Move      "Cold Management PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      "SusanAnstrand@nincal.com,SuzieMcGuire@nincal.com",MailCC
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\ColdManagement.pdf",MailAttach
                                 endif
                      Pause     "10"
                      call       sendmail
                      Pause     "5"
                      Erase      Mailattach
.                    add c1 to n3
.                    repeat
.                erase PRTFILE1
.          elseif (osflag = c3 | osflag =c4)         .win 95 98
.
.                  loop
.                    until (N3 = COPY)
.                              PRTPLAY PRTFILE1,"laser8"
.                          add c1 to n3
.                    repeat
.                erase PRTFILE1
.          elseif (osflag = "9" )         .win 7
.                    loop
.                    until (N3 = COPY)
.                              PRTPLAY PRTFILE1,"\\NINs2\laser8"
.                    add c1 to n3
.                    repeat
.                erase PRTFILE1
.          Else
.                    loop
.                    until (N3 = COPY)
.                              PRTPLAY PRTFILE1,""
.                    add c1 to n3
.                    repeat
.end patch 3.00  turn off copies
                erase PRTFILE1
.begin patch 3.00 
.        endif
.end patch 3.00  

Recall
        if (chkall = c1)
                if (loopflg = c6)
.                if (loopflg = c3)
                        goto ender
                else
                        goto checker
                    endif
.                 repeat
        endif

        call        OrderSetMouseFree
.Patch1.6
.                              stop
.Patch1.6
        return
.==========================================================================
FileGo
.Flag set to "N" if in Modify or New mode
.        branch result to FileGo1,FileGo2,FileGo3,FileGo3
        branch result to FileGo1
FileGo1

        call click_NEREPExit
        RETURN
Optionsgo
        return
ViewGo
        return
EditGo
        return
HelpGo
        setprop AboutMssg,visible=1
        return
.==========================================================================
NREVUpdateProgressBar
          calc      CurRec=(CurRec+1)
          calc      CurVal=((CurRec/howmany)*100)
          if (CurVal <> LastVal)
                    setitem   NREV0011ProgressBar,0,CurVal
                    move      CurVal,LastVal
          endif
          return
NREVInitProgressBar
          move      C0,CurRec
          move      C0,CurVal
          move      C0,LastVal
          return
OrderSetMouseBusy
        setmode *mcursor=*wait
        return
OrderSetMouseFree
        setmode *mcursor=*arrow
        return

Ender
        CALL OrderSetMouseFree
                      goto  exit1
.Patch2.0
newbus
.                                unpack    cid,str2,str4
.                                      PACK      MKEY FROM str4,"000"
.                                                                    CALL      NMLRKEY
                               Packkey  Compfld,Cid
                               call     Compkey
       unpack   COMPCNTDATE INTO cc,yy,mm,dd
                    move     "31" to dd
         type       yy
         if        equal
         CALL      CVTJUL
         MOVE      juldays TO revdat
                              move      revdat to check
         move      today1 to check2
         SUB       check FROM CHECK2
         compare   "365" to check2           usage in last year
         if LESS
                              move       yes to newbiz
                              move     nmmrep to nmm
                              move     yyrep to yy
                              move     ccrep to cc
                              goto       reader
                              endif
                              endif
                              move     nmmrep to nmm
                              move     yyrep to yy
                              move     ccrep to cc
                              return

debugger
               return
.Patch2.0
.                               .Patch1.6
exit1
.                              stop
.Patch1.6
        return
        include nrevio.inc
        include ndatio.inc
        include nprjio.inc
.patch1.7
                              include   compio.inc
                              include   cntio.inc
         INCLUDE   NXRFio.INC
.        include nmlrio.inc
.Patch1.7
        include comlogic.inc
