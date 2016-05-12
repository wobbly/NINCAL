.Brokerage Rental\Exchange & List Management Print Program
.Actual vs. Actual
PC         EQU       0
           include common.inc
           include cons.inc
          include   compdd.inc
          include   cntdd.inc
         INCLUDE   NXRFDD.INC
           include nprjdd.inc
           include ndatdd.inc
           include nrevdd.inc
.==============================================================================
Release   Init             "3.20"    DLH .get rid of Jose patch 2.0. make new biz calcs match other logic
.                                           Also printing straight to PDF instead of a print file and printplay, eliminated a lot of Unnecessary code
.                                           Email reports to destination which varies by report type.
reldate   Init      "2016 March 1"          .See rev 3.11 for previous code
.Release   Init             "3.11"    DLH .new Year
.reldate   Init      "2016 January 5"
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
Auto      Dim       1
str2a     Dim       2
.===========================================================
TOTYRS     FORM     10.2
.===========================================================
.Printfile
prfile     PFILE
.===================================================================================
MONTHX     FORM    10.2
.************************************************************************************
.change yearly----
LastYr        INIT   "2015"                 .previous year
ThisYr        INIT   "2016"                 .current year   
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
*******************************************************************************
.==============================================================================
.Sort Parameters=======================================================
INDAT    init  "revenue.dat"   .File to be sorted
OUTSRT   init  "revenue.srt"   .Sorted Output file
ClintSrt init  "3-12"                        .Sort by client # & Year
SORTFLE   dim    250                           .Var to pack file names of sort
PRTITLE   DIM    18
PRTNAME1  DIM    11
PRTDIR    INIT    "C:\WORK\"                                      ."
PRTFILE1  DIM    19
holdmlr   dim    6
.======================================================================
.needed for printing to PDF for non native fonts and things like Copyright to appear correctly in the pdf
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
.==========================================================================
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
        ListView1.InsertColumn using "",1,1
        ListView1.InsertColumn using ThisYr,100,2
        ListView1.InsertColumn using LastYr,100,3
        ListView1.InsertColumn using "Mailer Name",100,4
        ListView1.InsertColumn using "Mailer ",30,5
        ListView1.InsertColumn using "Difference",100,6
        Listview1.setcolumnformat giving result using 0,0
        Listview1.setcolumnformat giving result using 1,1
        Listview1.setcolumnformat giving result using 2,1
        move    "100",column
        move    "4000",column1
        move    "5500",column2
        move    "7950",column3
        move    "3000",Title1
        move    "6600",Title2
        move    "3800",Title4
.==============================================================================
.
.TESTING
.           MOVE       "NREV0011",program
.           move       "02-29-16",today
.TESTING           

          If        (program = "NREV0011")           .chained from dsinit
          Unpack    Today into mm,str1,dd,str1,yy
          move      "20",cc
          MOve      Yes,Auto
          else
         CLOCK   TIMESTAMP to str8
         unpack  str8,cc,yy,mm,dd
          endif
           move      cc to ccrep
           move    mm to mmrep
           move    yy to yyrep
        CALL      CVTJUL
         MOVE      juldays TO TODAY1
        move    mm to nmm
                      move    nmm to nmmrep
.===========================================================
        move    nmm to str2
        rep     " 0",str2
        setitem NerepEditMo,0,str2
        setitem NerepEditCOPY,0,"1"
        getinfo  system,str6
        unpack   str6 into str1,str2
        unpack   str2 into str1
        move     c0 to osflag
        call getwinver
        move c1 to ndatpath
.begin patch 3.20
           move       c3,ndatlock
.end patch 3.20
          If        (auto = Yes)
          call      Entry
          goto      Click_NerepExit
          else
        loop
                 waitevent
        repeat
          endif
Entry
.==================================================
        clear   chkall
        getprop NerepRadioAll,SELGROUPID=loopflg         0-all 1-rental 2-exch 3-list mgnt 4-lm Exchange 
.                                                                    5-Cold Brokerage 6- Cold List Management
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
.Rental
.==========================================================================================
.Brokerage Rental
SORTER
         PACK  PRTITLE,"Brokerage Rental"
         PACK  PRTNAME1,"BRKRENT.LST"
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
.Brokerage exchange
SORTER2
         PACK  PRTITLE,"Brokerage Exchange"
         PACK  PRTNAME1,"BRKEXCH.LST"

         pack   SortFle,ntwkpath6,indat,comma,ntwkpath1,outsrt
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
.List Management - List exchange
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
           pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","M","'","&9=","'",LastYr,"'","|","2=","'","M","'","&9=","'",ThisYr,"'"
         sort   taskname,sundm="NINS1:502"
         if over
               alert caution,S$ERROR$,result,"No Sort"
               call OrderSetMouseFree
               return
         endif
.====================================================================================
OpenIt
*********************************************************************
          call      NREVInitProgressBar
              pack str45 with outsrt,"|NINS1:502"
              open          tempfile,str45
          positeof      tempfile
          fposit    tempfile,N10
          calc      howmany=(N10/723)   .'723 = 721(Revenue record length) + 2 bytes for CR/LF
              close         tempfile
*********************************************************************

          OPEN    INFILE,str45
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
.1-Brk rental 2-Brk exch 3-list mgnt 3 LM list 5-Cold Brokerage 6- Cold List Management
        BRANCH loopflg to Reader1,Reader2,Reader3,Reader4,Reader5,Reader6
.        BRANCH loopflg to Reader1,Reader2,Reader3,Reader4
.Reader 1 Brokerage Rental
READER1
          REset     RUNCODES
          scan      Cid,runcodes
          goto      reader if equal
.begin patch 3.20
.                              call      newbus
.end patch 3.20
         cmatch  TYPE to "R"                    .Rental
        goto    READER if not equal
        cmatch  SRC to "B"
        goto    READER if not equal
        if      (YR0 = LastYr | YR0 = ThisYr)
                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                SEPAR,OCTAR,NOVAR,DECAR
        endif
        goto listview
.Reader 2 Brokerage Exchange
READER2
.begin patch 3.20
.                              call      newbus
.end patch 3.20
                              cmatch  TYPE to "E"                 .Exchange
        goto    READER if not equal
        cmatch  SRC to "B"
        goto    READER if not equal
        if      (YR0 = LastYr | YR0 = ThisYr)
                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                SEPAR,OCTAR,NOVAR,DECAR
        endif
        goto listview
.======================================================================================
.Reader 3 List Management
READER3
          REset     RUNCODES
          scan      Cid,runcodes
          goto      reader if equal
         move      cid to nxrffld
         CALL      NXRFkey
          move      NXRFMLR,COMPFLD
          move      "fulfxit-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
.begin patch 3.20
.       unpack   COMPCNTDATE INTO cc,yy,mm,dd
.                    move     "31" to dd
.         type       yy
.         if        equal
.         CALL      CVTJUL
.         MOVE      juldays TO revdat
.                              move      revdat to check
.         move      today1 to check2
.         SUB       check FROM CHECK2
.         compare   "365" to check2           .usage in last year
.         if LESS
.                              move       yes to newbiz
.                              move     nmmrep to nmm
.                              move     yyrep to yy
.                              move     ccrep to cc
.                              goto       reader
.                              endif
.                              endif
.                              move     nmmrep to nmm
.                              move     yyrep to yy
.                              move     ccrep to cc
.end patch 3.20

        cmatch  TYPE to " "                 .List Management
        goto    READER if not equal
        cmatch  SRC to "M"
        goto    READER if not equal
        if      (YR0 = LastYr | YR0 = ThisYr)
                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                SEPAR,OCTAR,NOVAR,DECAR
        endif
        goto listview
.========================================================================================
.reader 4 LM List
Reader4
.List Management List
        clear   mtchflg
        clear   tmpvar1
        clear   tmpvar2
        clear   monthx
          REset     RUNCODES
          scan      Cid,runcodes
          goto      reader if not equal
          REset     EXFEELST
          scan      Cid,EXFEELST
          goto      reader if not equal
        if      (YR0 = LastYr | YR0 = ThisYr)
                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                SEPAR,OCTAR,NOVAR,DECAR
        endif
          goto      Listview


.======================================================================================
.reader 4 cold new Brokerage
READER5

          packkey    Compfld,Cid
          call      Compkey
.begin patch 3.20
.                      move      no to newbiz
.       unpack   COMPCNTDATE INTO cc,yy,mm,dd
.                    move     "31" to dd
.         type       yy
.         if        equal
.         CALL      CVTJUL
.         MOVE      juldays TO revdat
.                              move      revdat to check
.         move      today1 to check2
.         SUB       check FROM CHECK2
.         compare   "365" to check2           usage in last year
.         if LESS
.                              move       yes to newbiz
.                              move     nmmrep to nmm
.                              move     yyrep to yy
.                              move     ccrep to cc
.                              goto      coldone
.                              else
.                              move     nmmrep to nmm
.                              move     yyrep to yy
.                              move     ccrep to cc
.                              goto     reader
.                              endif
.                              endif
.end patch 3.20
coldone
         if      (CID = "000001")              .
                goto reader
        endif
        clear   mtchflg
        clear   tmpvar
        clear   tmpvar1
        clear   tmpvar2
        clear   OLDTOT
        if (CID = holdmlr)                      .check to see if mlr already processed
                goto reader
        endif
        move    CID to holdmlr
        cmatch  SRC to "B"
        goto    READER if not equal
.begin patch 3.20
.        if      (YR0 = ThisYr)
.                pack nrevfld,type,src,cid,yr0
.end patch 3.20

.begin patch 3.20
.add code to correctly check for new biz
                      call       CheckNewPrj
                If    (NewBiz <> yes)
                goto  Reader
                endif
.                    goto ThisYrStart
.        endif
.end patch 3.20
.begin patch 3.20 - why?
.        if      (YR0 = LastYr)
.                 pack  nrevfld,type,src,cid,yr0
.begin patch 3.20 - why?
.ADDOLD
.                add JANAR,OLDTOT
.                add FEBAR,OLDTOT
.                add MARAR,OLDTOT
.                add APRAR,OLDTOT
.                add MAYAR,OLDTOT
.                add JUNAR,OLDTOT
.                add JULAR,OLDTOT
.                add AUGAR,OLDTOT
.                add SEPAR,OLDTOT
.                add OCTAR,OLDTOT
.                add NOVAR,OLDTOT
.                add DECAR,OLDTOT
.end patch 3.20 - why?
.begin patch 3.20

.                call  CheckNewPrj
.                    if (newbiz = yes)
.                                                            goto checker2
.                                                            else
.                    if (OLDTOT <> C0) 
.                              goto reader
.                  endif
.             endif         ???????
.end patch 3.20

.===================================================================================
.begin patch 3.20
.checker2
.                  if (mtchflg = c1)
.                        move c0 to mtchflg
.                    if (newbiz = yes)
.                    goto checker3
.                    else
.                              if (OLDTOT <> C0) 
.                              goto reader
.                              endif
.                    endif
.                        else
.checker3
.                                    unpack  nrevfld,type,src,cid,yr0
.                              move      ThisYr to yr0
.                                    pack    nrevfld,type,src,cid,yr0
.                                    clear   tmpvar1
.                              goto nextread
.                endif
.                unpack  nrevfld,type,src,cid,yr0
.                if (type = "E")
.                        move "R" to type
.                endif
.                pack    nrevfld,type,src,cid,yr0
.                call    NREVKEY
.                if not over
.                        move c1 to mtchflg
.                        goto ADDOLD
.                else
.                        move c0 to mtchflg
.                    if (newbiz = yes)
.                                                            goto checker4
.                                                            else
.                    if (OLDTOT <> C0) 
.                              goto reader
.                  endif
.               endif
.checker4
.                                                        unpack  nrevfld,type,src,cid,yr0
.                              move      ThisYr to yr0
.                                    pack    nrevfld,type,src,cid,yr0
.                                    clear   tmpvar1
.                              goto nextread
.                        endif
.
.NextRead
.                call    NREVKEY
.
.=============================================================================
.                if over
.                        if     (mtchflg = c1)
.                                move c0 to mtchflg
.                              goto Projread
.                        endif
.                else
ThisYrStart
           unpack     Nprjfld into type,src,cid
           pack       nrevfld,type,src,cid,Thisyr
           Call       Nrevkey
                LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                SEPAR,OCTAR,NOVAR,DECAR
                        add   monthx to tmpvar1
                        move    tmpvar1 to monthx
.                        if    (mtchflg = c1)
.                                move c0 to mtchflg
.                                  goto Projread
.                        endif
.                endif
.                          unpack  nrevfld,type,src,cid,yr0
.                    if (type = "E")
.                              move "R" to type
.                    endif
.                          pack    nrevfld,type,src,cid,yr0
.                        move  c1 to mtchflg
.                        goto    nextread
Projread
                if (MONTHX < c1)
                        goto reader
                endif
.                  unpack  nrevfld,type,src,cid,yr0
.end Patch 3.2
Projread1
***************************************************************************************
.*Code for new projection file
.          move      c0,projlast
.          move      c0,projnew
.          if (src = "B")
                    Packkey  Compfld,Cid
                    move      "COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call     Compkey

.begin patch 3.20
.                              move      "EB",str2
.                    move      C0,N2
.                    loop
.                              add       C1,N2
.                              move      N2,str2a
.                              rep       zfill,str2a
.                              pack      NPRJFLD,str2,CID,YR0,str2a
.                              rep       zfill,NPRJFLD
.                              call      NPRJKEY
.                              until over
.                            move        PrjAR,projnew
.                              until (PrjMast = YES)
.                    repeat
.                    move      "RB",str2
.                    move      C0,N2
.                    loop
.                              add       C1,N2
.                              move      N2,str2a
.                              rep       zfill,str2a
.                              pack      NPRJFLD,str2,CID,YR0,str2a
.                              rep       zfill,NPRJFLD
.                              call      NPRJKEY
.                              until over
.                            move        PrjAR,projlast
.                              until (PrjMast = YES)
.                    repeat
.          endif
.                    add     projnew,projlast,tmpvar2
**************************************************************************************
.Projected for current year
Project
.                    if (newbiz = yes | tmpvar2 = c0)
           Move      CompCOmp,Client
.                                                            move   no to newbiz
           goto       coldlistview
.                                                            else
           move       no to newbiz
           goto       reader
.                                                            endif
.           endif
.end Patch 3.2
.======================================================================================
.Cold New List Management
READER6
          REset     RUNCODES
          scan      Cid,runcodes
          goto      reader if equal
          REset     EXFEELST
          scan      Cid,EXFEELST
          goto      reader if equal

         move      cid to nxrffld
         CALL      NXRFkey
          move      NXRFMLR,COMPFLD
          move      "fulfxit-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
.          if        (Cid = "002700")
.          call      Debug
.          endif
.begin patch 3.20
.                              move      no to newbiz
.       unpack   COMPCNTDATE INTO cc,yy,mm,dd
.                    move     "31" to dd
.         type       yy
.         if        equal
.         CALL      CVTJUL
.         MOVE      juldays TO revdat
.                              move      revdat to check
.         move      today1 to check2
.         SUB       check FROM CHECK2
.         compare   "365" to check2           usage in last year
.         if LESS
.                              move       yes to newbiz
.                              move     nmmrep to nmm
.                              move     yyrep to yy
.                              move     ccrep to cc
.                              goto      coldone2
.                              else
.                              move     nmmrep to nmm
.                              move     yyrep to yy
.                              move     ccrep to cc
.                              goto     reader
.                              endif
.                              endif
.end patch 3.20

coldone2

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
.begin patch 3.20
.        if      (YR0 = ThisYr)                           .ThisYr=current year
.                pack nrevfld,type,src,cid,yr0
.add code to correctly check for new biz
           call       CheckNewPrj
           If         (NewBiz <> yes)
           goto       Reader
           endif
.                    goto ThisYrStart1
.        endif
.        if      (YR0 = LastYr)                           .LastYr=Last year
.                 pack  nrevfld,type,src,cid,yr0
.ADDOLD1
.                add JANAR,OLDTOT
.                add FEBAR,OLDTOT
.                add MARAR,OLDTOT
.                add APRAR,OLDTOT
.                add MAYAR,OLDTOT
.                add JUNAR,OLDTOT
.                add JULAR,OLDTOT
.                add AUGAR,OLDTOT
.                add SEPAR,OLDTOT
.                add OCTAR,OLDTOT
.                add NOVAR,OLDTOT
.                add DECAR,OLDTOT

.end patch 3.20
.begin patch 3.20
.                    if (newbiz = yes)
.                                                            goto checker5
.                                                            else
.                    if (OLDTOT <> C0) 
.                              goto reader
.                  endif
.                                                            endif
.checker5
..==================================================================================
.            unpack  nrevfld,type,src,cid,yr0
.            move      ThisYr to yr0
.            pack    nrevfld,type,src,cid,yr0
.            clear   tmpvar1
.            goto nextread1
.
.NextRead1
.                call    NREVKEY
.
..=============================================================================
.                if over
.                                clear monthx
.                              goto Projread2
.                else
ThisYrStart1
           unpack     Nprjfld into type,src,cid
           pack       nrevfld,type,src,cid,Thisyr
           Call       Nrevkey
                          LOAD MONTHX using NMM from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                  SEPAR,OCTAR,NOVAR,DECAR
                        add   monthx to tmpvar1
                        move    tmpvar1 to monthx
.                        goto Projread2
.                endif
Projread2
                if (MONTHX < c1)
                        goto reader
                endif
.                  unpack  nrevfld,type,src,cid,yr0
.end patch 3.20
Projread3
.**********************************************************************************
.begin patch 3.20   should already have this info
.*Code for new projection file
.          move      c0,projlast
.          if (src = "M")
.                    move      " M",str2
.                    move      C0,N2
.                    loop
.                              add       C1,N2
.                              move      N2,str2a
.                              rep       zfill,str2a
.                              pack      NPRJFLD,str2,CID,YR0,str2a
.                              call      NPRJKEY
.                              until over
.                            move        PrjAR,projlast
.                              until (PrjMast = YES)
.                    repeat
.                move    projlast,tmpvar2
.          endif
.end patch 3.20
.=============================================================================
.Projected for current year
Project1
.begin patch 3.20
.                    if (newbiz = yes | tmpvar2 = c0)
                               move holdmlr to NDATFLD
                               call NDATKEY
                               move holdmlr to cid
                               move OLSTNAME to client
.                               move   no to newbiz
                  goto coldlistview
.                                                            else
                                                            move  no to newbiz
                                                            goto reader
.                                                            endif
.=============================================================================
.write out
.        endif

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

        repeat



.==================================================================
Print
           close   infile
         clear n9
                PACK PRTFILE1,PRTDIR,PRTNAME1
.begin patch 3.20
                                 if         (Loopflg = c1)
                      PRTOPEN   prfile,"PDF:","c:\work\pdf\BrokerageRental.pdf",Flags=PDF_FLAGS_WIN_ANSI_ENCODING
                                 
                                 Elseif     (Loopflg = c2)                       
                                 
                      PRTOPEN   prfile,"PDF:","c:\work\pdf\BrokerageExchange.pdf",Flags=PDF_FLAGS_WIN_ANSI_ENCODING

                                 Elseif     (Loopflg = c3)                       
                                 
                      PRTOPEN   prfile,"PDF:","c:\work\pdf\Management.pdf",Flags=PDF_FLAGS_WIN_ANSI_ENCODING

                                 Elseif     (Loopflg = c4)                       
                                 
                      PRTOPEN   prfile,"PDF:","c:\work\pdf\ManagementList.pdf",Flags=PDF_FLAGS_WIN_ANSI_ENCODING

                                 Elseif     (Loopflg = c5)                       
                                 
                      PRTOPEN   prfile,"PDF:","c:\work\pdf\ColdBrokerage.pdf",Flags=PDF_FLAGS_WIN_ANSI_ENCODING

                                 Elseif     (Loopflg = c6)                       

                      PRTOPEN   prfile,"PDF:","c:\work\pdf\ColdManagement.pdf",Flags=PDF_FLAGS_WIN_ANSI_ENCODING
                      
                                 endif
.          if (osflag >= c6) 
.                    PRTOPEN   prfile,"\\nins2\laser8",PRTFILE,noprint,spoolfile=PRTFILE1
.          else   .(osflag = c0)         .Don't know prompt for printer
.                    PRTOPEN   prfile,"",PRTFILE,noprint,spoolfile=PRTFILE1
.          endif
.end patch 3.20




.=================================================================
.Headers
.Defining Header and Titles
Page
        CLEAR     ROWCOUNT
        ADD       C1 TO PGCNT
           if         (pgcnt = c1)
           prtpage   prfile;*UNITS=*HIENGLISH;
           else
           prtpage   prfile;*NEWPAGE:
                   *UNITS=*HIENGLISH;
           endif
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
                      move       str13,str11           
                      move       str11 to tmpvar1
                      if         (tmpvar1 < c0)
                      edit       tmpvar1,str14,mask="-ZZ,ZZZ,ZZZ.99",Align=2
                      elseif     (tmpvar1 = c0)
                      move       c0,str14
                      else
                      edit       tmpvar1,str14,mask="ZZZ,ZZZ,ZZZ.99",Align=2
                      endif
                      prtpage    prfile;*pcolumn1:row,*OVERLAYON,*font=font12,*ALIGNMENT=*right,str14;
                        if (loopflg > c4)
                              goto Rower
                    endif
. PRevious year
                  call trim using SORTSTR
                      move       sortstr,str11         
                      move       str11 to tmpvar1
                      if         (tmpvar1 < c0)
                      edit       tmpvar1,str14,mask="-ZZ,ZZZ,ZZZ.99",Align=2
                      elseif     (tmpvar1 = c0)
                      move       c0,str14
                      else
                      edit       tmpvar1,str14,mask="ZZZ,ZZZ,ZZZ.99",Align=2
                      endif
                      prtpage    prfile;*pcolumn2:row,*OVERLAYON,*font=font12,*ALIGNMENT=*right,str14;
.difference
                          call trim using str15
                      move       str15,str11           
                      move       str11 to tmpvar1
                      if         (tmpvar1 < c0)
                      edit       tmpvar1,str14,mask="-ZZ,ZZZ,ZZZ.99",Align=2
                      elseif     (tmpvar1 = c0)
                      move       c0,str14
                      else
                      edit       tmpvar1,str14,mask="ZZZ,ZZZ,ZZZ.99",Align=2
                      endif
                      prtpage    prfile;*pcolumn3:row,*OVERLAYON,*font=font12,*ALIGNMENT=*right,str14;
.=========================================================================================
Rower
            if (ROWCOUNT = "49")
                              move "10200",row
                           prtpage prfile;*pcolumn:row,*OVERLAYOff,*font=font12,*ALIGNMENT=*Left,"Page# ";
                           prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
                           prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News";
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
ENDLOOP
       if (ROWCOUNT < "46")
                 goto totals
       else

.Added to correct page # and page label for next to last page
.==========================================================================================
                 move "10200",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
                 prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News";
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
               prtpage prfile;*p3350:row,*pensize=10,*line=8100:row;
               add     "30" to row
               prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Grand Totals",*boldoff,*ULOFF;
               clear str13
               move ThisYrTOT to str13
               call trim using str13
               prtpage prfile;*pcolumn1:row,*OVERLAYON,*font=font12,*ALIGNMENT=*Right,*boldon,str13,*boldoff;


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
                 prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News"
.=================================================================================================

End1
        PRTCLOSE prfile
        getitem NerepEditCOPY,0,str3
        move str3 to copy
        if (copy = c0)
          goto recall
        endif
        clear n3
.============================================================================

                                 if         (Loopflg = c1)
.begin patch 3.20
.                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\BrokerageRental.pdf"
.                                Pause     "10"
.end patch 3.20
                      Move      "Brokerage Rental PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\BrokerageRental.pdf",MailAttach
                                 
                                 Elseif     (Loopflg = c2)                       
                                 
.begin patch 3.20
.                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\BrokerageExchange.pdf"
.                                Pause     "10"
.end patch 3.20
                      Move      "Brokerage Exchange PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\BrokerageExchange.pdf",MailAttach

                                 Elseif     (Loopflg = c3)                       
                                 
.begin patch 3.20
.                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\Management.pdf"
.                                Pause     "10"
.en patch 3.20
                      Move      "Management  PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\Management.pdf",MailAttach

                                 Elseif     (Loopflg = c4)                       
                                 
.begin patch 3.20
.                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\ManagementList.pdf"
.                                Pause     "10"
.end patch 3.20
                      Move      "Management Exchange PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\ManagementList.pdf",MailAttach

                                 Elseif     (Loopflg = c5)                       
                                 
.begin patch 3.20
.                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\ColdBrokerage.pdf"
.                                Pause     "10"
.end patch 3.20
                      Move      "Cold Brokerage PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      "SusanAnstrand@nincal.com,SuzieMcGuire@nincal.com",MailCC
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\ColdBrokerage.pdf",MailAttach

                                 Elseif     (Loopflg = c6)                       
                                 
.begin patch 3.20
.                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\ColdManagement.pdf"
.                                Pause     "10"
.end patch 3.20
                      Move      "Cold Management PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      "SusanAnstrand@nincal.com,SuzieMcGuire@nincal.com",MailCC
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\ColdManagement.pdf",MailAttach
                                 endif
.                      Pause     "10"
.begin patch 3.20  only send brokerage rent, List Management, COld br, cold LM
                      if         (loopflg = c1 | loopflg = c3 | loopflg = c5 | loopflg = c6) 
                      call       sendmail
                      endif
.end patch 3.20  only send brokerage rent, List Management, COld br, cold LM
.                      Pause     "5"
.end patch 3.20
                      Erase      Mailattach
                erase PRTFILE1

Recall
        if (chkall = c1)
                if (loopflg = c6)
                        goto ender
                else
                        goto checker
                    endif
        endif

        call        OrderSetMouseFree
        return
.==========================================================================
FileGo
.Flag set to "N" if in Modify or New mode
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
.begin patch 3.20
.newbus
..                                unpack    cid,str2,str4
..                                      PACK      MKEY FROM str4,"000"
..                                                                    CALL      NMLRKEY
.                               Packkey  Compfld,Cid
.                               call     Compkey
.       unpack   COMPCNTDATE INTO cc,yy,mm,dd
.                    move     "31" to dd
.         type       yy
.         if        equal
.         CALL      CVTJUL
.         MOVE      juldays TO revdat
.                              move      revdat to check
.         move      today1 to check2
.         SUB       check FROM CHECK2
.         compare   "365" to check2           usage in last year
.         if LESS
.                              move       yes to newbiz
.                              move     nmmrep to nmm
.                              move     yyrep to yy
.                              move     ccrep to cc
.                              goto       reader
.                              endif
.                              endif
.                              move     nmmrep to nmm
.                              move     yyrep to yy
.                              move     ccrep to cc
.                              return
.
.debugger
CheckNewprj
           if         (cid = "010896" | cid = "010897" | cid = "008390" | Cid = "011107")
           call       debug
           endif
           move      C1,N2
           move       c0,Prjar
           move      N2,str3
           rep       zfill,str3
           pack      NPRJFLD with Type,Src,CID,ThisYr,str3
           call      NPRJKEY
           if         over
           move       Yes to NewBiz
           unpack     NPRJFLD with Type,Src,CID
           call       checkrev
           return
           endif
           move       No to NewBiz
           
           loop
           add       C1,N2
           move      N2,str3
           rep       zfill,str3
           pack      NPRJFLD with Type,Src,CID,ThisYr,str3
           call      NPRJKEY
           until      over
           until     (PrjMast = YES)
           repeat

           Move       PrjAR,projlast
           If        (PrjAR > 0)                    
           MOve      No,Newbiz
           Elseif   (PrjAR = 0)                    
           move       Yes to NewBiz
           endif
           call       CheckRev
.end patch 3.20
               return
.begin patch 3.20
CheckRev
           pack       nrevfld with type,src,cid,LastYr          ......<<<<<<<<Last year revenue?
           call       nrevkey
           if         Not over     
                      Calc      OldTot=(JanAR+FebAR+MarAR+AprAR+MayAR+JunAR+JulAR+AugAR+SepAR+OctAR+NovAR+DecAR)
                      if         (oldtot <> c0)
                      move       No,NewBiz
                      endif
           endif
           return
.end patch 3.20

exit1
        return
        include nrevio.inc
        include ndatio.inc
        include nprjio.inc
        include   compio.inc
           include   cntio.inc
         INCLUDE   NXRFio.INC
        include comlogic.inc
