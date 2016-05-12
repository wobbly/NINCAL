.Brokerage Rental\Exchange & List Management Print Program
.Actual vs. Actual YTD

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
.New biz section needs to be cleaned up - see nrev0011.pls  DLH 2016 March 03
Release   Init             "1.10"    DLH update, pdf , etc
reldate   Init      "2014 December 18"
.Release   Init             "1.00"    DLH 
.reldate   Init      "2012 December 05"
.Nrev0013ListView1  ListView
INFILE     FILE
Auto      Dim       1
str2a     Dim       2
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
.YR1        INIT      "2001"
.YR1        INIT      "2004"
.begin  patch 2.13
.YR1        INIT      "2006"
.YR2        INIT   "2007"
.begin  patch 2.14
YR1        INIT   "2013"
YR2        INIT   "2014"
.YR1        INIT      "2008"
.YR2        INIT   "2009"
.end  patch 2.14
.end  patch 2.13
.Patch 2.11
.YR1        INIT      "2005"
.YR1        INIT      "2006"
.YR2        INIT   "2002"
.YR2        INIT   "2005"
.YR2        INIT   "2006"
.YR2        INIT   "2007"
.Patch 2.11
.Patch 1.91
.*********************************************************************************
.=====================================================================================
LMLIST     INIT   "018710"
LMLIST2    INIT   "024593"
SORTHLP    form   10.2        .add to yr2 total in hidden coulumn to be used as sorted column
SORTSTR    DIM     13
HIDYR      FORM    10.2
TMPVAR     FORM    10.2
TMPVAR1    FORM    10.2
TMPVAR2    FORM    10.2
YR1TOT     FORM    10.2
YR2TOT     FORM    10.2
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
TitleYR1  dim     18
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
PRTDIR    INIT    "C:\WORK\"                           ."
PRTFILE1  DIM    19
holdmlr   dim    6
.======================================================================
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
        move    "NREV0013.PLS",Wprognme
        move    "Broker/List Mgmnt Actuals",Wfunction
        move    "David Herrick",Wauthor
        move    release,Wrelease
        move    Reldate,Wreldate
.=======================================================================
abt     plform  About
x         plform  Nrev0013
.x        plform  Nrev00130001
.==========================================================================
.added 11/19/01
        winhide
.=====================================
        formload x
        formload abt
********************************************************************************
.==========================================================================
  create  Nrev0013;mFile,FData
  create  Nrev0013;mEdit,EData,mFile
  create  Nrev0013;mOptions,OData,mEdit
  create  Nrev0013;mHelp,HData,mOptions
.Activate Menus
.FileGo leads to stop
  activate mFile,FileGo,result
.Need this when it works
  activate mEdit,EditGo,result
  activate mOptions,OptionsGo,result
.Only a SubMenu under this one
  activate mHelp,HelpGo,result
.==============================================================================
        Nrev0013ListView1.InsertColumn using "",1,1
        Nrev0013ListView1.InsertColumn using yr2,100,2
        Nrev0013ListView1.InsertColumn using yr1,100,3
        Nrev0013ListView1.InsertColumn using "Mailer Name",100,4
        Nrev0013ListView1.InsertColumn using "Mailer ",30,5
        Nrev0013ListView1.InsertColumn using "Difference",100,6
        Nrev0013ListView1.setcolumnformat giving result using 0,0
        Nrev0013ListView1.setcolumnformat giving result using 1,1
        Nrev0013ListView1.setcolumnformat giving result using 2,1
        move    "100",column
        move    "5000",column1
        move    "6500",column2
        move    "7950",column3
.        move    "3350",column1
.        move    "4900",column2
.        move    "6900",column3
        move    "3000",Title1
        move    "6600",Title2
        move    "3800",Title4
.==============================================================================
.
          If        (program = "NREV0013")           .chained from dsinit
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
        setitem Nrev0013EditMo,0,str2
        setitem Nrev0013EditCOPY,0,"1"
        getinfo  system,str6
        unpack   str6 into str1,str2
        unpack   str2 into str1
        move     c0 to osflag
        call getwinver
        move c1 to ndatpath
          If        (auto = Yes)
          call      Entry
          goto      Click_Nrev0013Exit
          else
        loop
                 waitevent
        repeat
          endif
Entry
.==================================================
        clear   chkall
        getprop Nrev0013RadioAll,SELGROUPID=loopflg         0-all 1-rental 2-exch 3-list mgnt
.                                                                    4-Cold Brokerage 5- Cold List Management
.==================================================
.checker
        call OrderSetMouseBusy
        getitem Nrev0013EditMo,0,str2              .get month for report
        move str2 to nmm
        if ((nmm > c12) | (nmm < c0) | (nmm = c0))
               alert caution,"Not a valid Month!",result,"Invalid"
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
        Nrev0013ListView1.deleteallitems
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
          clear YR1TOT
          clear YR2TOT
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
           pack   taskname,sortfle,";",clintsrt,comma,"s=1=","'","RB","'","&9=","'",YR1,"'","|","1=","'","RB","'","&9=","'",YR2,"'"
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
           pack   taskname,sortfle,";",clintsrt,comma,"s=1=","'","EB","'","&9=","'",YR1,"'","|","1=","'","EB","'","&9=","'",YR2,"'"
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
           pack   taskname,sortfle,";",clintsrt,comma,"s=1=","'"," M","'","&9=","'",YR1,"'","|","1=","'"," M","'","&9=","'",YR2,"'"
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
           pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","B","'","&9=","'",YR1,"'","|","2=","'","B","'","&9=","'",YR2,"'"
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
           pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","M","'","&9=","'",YR1,"'","|","2=","'","M","'","&9=","'",YR2,"'"
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
        setitem Nrev0013StatTitle,0,prtitle
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

        BRANCH loopflg to Reader1,Reader2,Reader3,Reader4,Reader5,Reader6
READER1
          REset     RUNCODES
          scan      Cid,runcodes
          goto      reader if equal

                              call      newbus
         cmatch  TYPE to "R"                    .Rental
        goto    READER if not equal
        cmatch  SRC to "B"
        goto    READER if not equal
        if      (YR0 = YR1 | YR0 = YR2)
                          Calc MONTHX = (JANLR+FEBLR+MARLR+APRLR+MAYLR+JUNLR+JULLR+AUGLR+SEPLR+OCTLR+NOVLR+DECLR)
        endif
        goto listview
READER2
                              call    newbus
                              cmatch  TYPE to "E"                 .Exchange
        goto    READER if not equal
        cmatch  SRC to "B"
        goto    READER if not equal
        if      (YR0 = YR1 | YR0 = YR2)
                          Calc MONTHX = (JANLR+FEBLR+MARLR+APRLR+MAYLR+JUNLR+JULLR+AUGLR+SEPLR+OCTLR+NOVLR+DECLR)
        endif
        goto listview
.======================================================================================
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

        cmatch  TYPE to " "                 .List Management
        goto    READER if not equal
        cmatch  SRC to "M"
        goto    READER if not equal
        if      (YR0 = YR1 | YR0 = YR2)
                          Calc MONTHX = (JANLR+FEBLR+MARLR+APRLR+MAYLR+JUNLR+JULLR+AUGLR+SEPLR+OCTLR+NOVLR+DECLR)
        endif
        goto listview
.========================================================================================
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
        if      (YR0 = YR1 | YR0 = YR2)
                          Calc MONTHX = (JANLR+FEBLR+MARLR+APRLR+MAYLR+JUNLR+JULLR+AUGLR+SEPLR+OCTLR+NOVLR+DECLR)
        endif
          goto      Listview


.======================================================================================
READER5
.Cold Brokerage
       match      "010735" to cid
                      call       debugger if equal
          packkey    Compfld,Cid
          call      Compkey
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
        if      (YR0 = YR2)
                pack nrevfld,type,src,cid,yr0
                    goto Yr2Start
        endif
        if      (YR0 = YR1)
                 pack  nrevfld,type,src,cid,yr0
ADDOLD
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
                    if (newbiz = yes)
                                                            goto checker2
                                                            else
                    if (OLDTOT <> C0) 
                              goto reader
                  endif
                                                            endif
.===================================================================================
checker2
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
                              move      yr2 to yr0
                                    pack    nrevfld,type,src,cid,yr0
                                    clear   tmpvar1
                              goto nextread
.                        endif
                endif
                unpack  nrevfld,type,src,cid,yr0
                if (type = "R")
                        move "E" to type
                else
                        move "R" to type
                endif
                pack    nrevfld,type,src,cid,yr0
                call    NREVKEY
                if not over
                        move c1 to mtchflg
                        goto ADDOLD
                else
                        move c0 to mtchflg
                    if (newbiz = yes)
                                                            goto checker4
                                                            else
                    if (OLDTOT <> C0) 
                              goto reader
                  endif
                                                            endif
checker4
                                                        unpack  nrevfld,type,src,cid,yr0
                              move      yr2 to yr0
                                    pack    nrevfld,type,src,cid,yr0
                                    clear   tmpvar1
                              goto nextread
                        endif

NextRead
                call    NREVKEY

.=============================================================================
                if over
                        if     (mtchflg = c1)
                                move c0 to mtchflg
                              goto Projread
                        endif
                else
Yr2Start
                          Calc MONTHX = (JANLR+FEBLR+MARLR+APRLR+MAYLR+JUNLR+JULLR+AUGLR+SEPLR+OCTLR+NOVLR+DECLR)
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
Projread1
***************************************************************************************
*Code for new projection file
          move      c0,projlast
          move      c0,projnew
          if (src = "B")
                    Packkey  Compfld,Cid
                    move      "COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call     Compkey

                              move      "EB",str2
                    move      C0,N2
                    loop
                              add       C1,N2
                              move      N2,str2a
                              rep       zfill,str2a
                              pack      NPRJFLD,str2,CID,YR0,str2a
                              rep       zfill,NPRJFLD
                              call      NPRJKEY
                              until over
                            move        PrjAR,projnew
                              until (PrjMast = YES)
                    repeat
                    move      "RB",str2
                    move      C0,N2
                    loop
                              add       C1,N2
                              move      N2,str2a
                              rep       zfill,str2a
                              pack      NPRJFLD,str2,CID,YR0,str2a
                              rep       zfill,NPRJFLD
                              call      NPRJKEY
                              until over
                            move        PrjAR,projlast
                              until (PrjMast = YES)
                    repeat
          endif
                    add     projnew,projlast,tmpvar2
**************************************************************************************
.Projected for current year
Project
                    if (newbiz = yes | tmpvar2 = c0)
                    Move      CompCOmp,Client
                                                            move   no to newbiz
                  goto coldlistview
                                                            else
                                                            move  no to newbiz
                                                            goto reader
                                                            endif
           endif

.===========================================================================
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
        if      (YR0 = YR2)
                pack nrevfld,type,src,cid,yr0
                    goto Yr2Start1
        endif
        if      (YR0 = YR1)
                 pack  nrevfld,type,src,cid,yr0
ADDOLD1
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
                    if (newbiz = yes)
                                                            goto checker5
                                                            else
                    if (OLDTOT <> C0) 
                              goto reader
                  endif
                                                            endif
checker5
                                    unpack  nrevfld,type,src,cid,yr0
                              move      yr2 to yr0
                                    pack    nrevfld,type,src,cid,yr0
                                    clear   tmpvar1
                              goto nextread1

NextRead1
                call    NREVKEY
.=============================================================================
                if over
                                clear monthx
                              goto Projread2
                else
Yr2Start1
                          Calc MONTHX = (JANLR+FEBLR+MARLR+APRLR+MAYLR+JUNLR+JULLR+AUGLR+SEPLR+OCTLR+NOVLR+DECLR)
                        add   monthx to tmpvar1
                        move    tmpvar1 to monthx
                                  goto Projread2
                endif
Projread2
                if (MONTHX < c1)
                        goto reader
                endif
                  unpack  nrevfld,type,src,cid,yr0
Projread3
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
                              call      NPRJKEY
                              until over
                            move        PrjAR,projlast
                              until (PrjMast = YES)
                    repeat
                move    projlast,tmpvar2
          endif
*********************************************************************************
                          goto  project1
.Projected for current year
Project1
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
        Nrev0013ListView1.InsertItem giving N7 using str13
        move monthx to str14
        Nrev0013ListView1.SetItemText using n7,str14,1
        move holdmlr to cid
        goto defaulter
.=====================================================================================
.======================================================================================
Listview

        Nrev0013ListView1.GetItemCount giving result
        sub c1 from result
        for n9,"0",result
        Nrev0013ListView1.GetItemText giving str6 using n9,4
        if (CID = str6)                      .check to see if mlr\lst already in listview
                MOVE C1 TO MTCHFLG
                if (YR0 = YR1)
.==========================================================================
.get item and reinsert for proper sorting

        Nrev0013ListView1.GetItemText giving str15 using n9,0
        Nrev0013ListView1.GetItemText giving str13 using n9,1
        Nrev0013ListView1.GetItemText giving str45 using n9,3
        Nrev0013ListView1.GetItemText giving str6 using n9,4
        Nrev0013ListView1.deleteitem giving n1 using n9
        Nrev0013ListView1.InsertItem giving n7 using str15
        Nrev0013ListView1.SetItemText giving N8 using n7,str13,1
        Nrev0013ListView1.SetItemText giving N8 using n7,str45,3
        Nrev0013ListView1.SetItemText giving N8 using n7,str6,4
.======================================================================================
.calculate difference of two years
        Nrev0013ListView1.GetItemText giving str13 using n9,1
                       move  str13 to TMPVAR
                       sub   monthx,TMPVAR,TOTYRS
                       move  TOTYRS to str13
        Nrev0013ListView1.SetItemText giving N8 using n9,str13,5
.======================================================================================
                        move monthx to str14
                        Nrev0013ListView1.SetItemText giving N8 using n9,str14,2
                endif
                if (YR0 = YR2)
.delete and insert new item for current year
.==========================================================================

        Nrev0013ListView1.GetItemText giving str15 using n9,2
        Nrev0013ListView1.GetItemText giving str45 using n9,3
        Nrev0013ListView1.GetItemText giving str6 using n9,4
        Nrev0013ListView1.deleteitem giving n1 using n9
.==================================
                        clear hidyr
                        add  sorthlp,monthx,hidyr
                        move hidyr to str13
.==================================
        Nrev0013ListView1.InsertItem giving n7 using str13
        Nrev0013ListView1.SetItemText giving N8 using n7,str15,2
        Nrev0013ListView1.SetItemText giving N8 using n7,str45,3
        Nrev0013ListView1.SetItemText giving N8 using n7,str6,4
.======================================================================================
                        move monthx to str14
                        Nrev0013ListView1.SetItemText giving N8 using n7,str14,1
.======================================================================================
        Nrev0013ListView1.GetItemText giving str13 using n7,2
                       move  str13 to TMPVAR
                       sub   TMPVAR,monthx,TOTYRS
                       move  TOTYRS to str13
        Nrev0013ListView1.SetItemText giving N8 using n7,str13,5
.======================================================================================
                endif
        endif
        repeat
                if (mtchflg = c1)        .already rewritten to data list
                        move c0 to mtchflg     .yes
                        goto Reader
                endif
                if (YR0 = YR1)         .write new rec for last year
                        move sorthlp to sortstr
                        Nrev0013ListView1.InsertItem giving N7 using sortstr
                              Nrev0013ListView1.SetitemText using n7,".00",1
                        move monthx to str14
                              Nrev0013ListView1.SetitemText using n7,str14,2
.formatting
                        if (str14 = "")
                                        Nrev0013ListView1.SetitemText using n7,".00",5
.==================================================================
                        else
.Calculate diff
                              move  C0 to TMPVAR
                                        sub   monthx,TMPVAR,TOTYRS
                                        move  TOTYRS to str13
                                        Nrev0013ListView1.SetItemText giving N8 using n7,str13,5
.=====================================================================
                        endif
                endif
                if (YR0 = YR2)
                        clear hidyr
                        add  sorthlp,monthx,hidyr
                        move hidyr to str13
                        Nrev0013ListView1.InsertItem giving N7 using str13
                        move monthx to str14
                        Nrev0013ListView1.SetItemText using n7,str14,1
                              Nrev0013ListView1.SetitemText using n7,".00",2
.-==============================================================================
                        clear str13
                        move  monthx to str13
                        Nrev0013ListView1.SetItemText giving N8 using n7,str13,5
                endif
                        if (loopflg = c3)     .if listmgmt listname from datacard file
                                move CID to NDATFLD
                                call NDATKEY
                                if not over
                                        Nrev0013ListView1.SetItemText using N7,OLSTNAME,3
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
.                                        Nrev0013ListView1.SetItemText using N7,MCOMP,3
                                        Nrev0013ListView1.SetItemText using N7,COMPComp,3
                                        goto ID
                                else
                                        goto defaulter
                              endif
                        endif
Defaulter
                              Nrev0013ListView1.SetItemText using N7,CLIENT,3

ID                      Nrev0013ListView1.SetItemText using N7,CID,4
.                if (YR0 = YR2)
.                        move  monthx to str13
.                        Nrev0013ListView1.SetItemText giving N8 using n7,str13,5
.                else
.                       move  str13 to TMPVAR
.                       sub   monthx,TMPVAR,TOTYRS
.                       move  TOTYRS to str13
.                          Nrev0013ListView1.SetItemText giving N8 using n7,str13,5
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
          if (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt 2k xp
                    PRTOPEN   prfile,"\\NINs2\laser8",PRTFILE,noprint,spoolfile=PRTFILE1
          elseif (osflag = c3 | osflag =c4)         .win 95 98
                    PRTOPEN   prfile,"Laser8",PRTFILE,noprint,spoolfile=PRTFILE1
          elseif (osflag = "9" )         .win 7
                    PRTOPEN   prfile,"\\NINs2\Laser8",PRTFILE,noprint,spoolfile=PRTFILE1
          else   .(osflag = c0)         .Don't know prompt for printer
                    PRTOPEN   prfile,"",PRTFILE,noprint,spoolfile=PRTFILE1
          endif

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
.        prtpage   prfile;*NEWPAGE:
.                   *UNITS=*HIENGLISH;
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
        pack titleyr1,"Comm. Only ",YR2
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,titleyr1;
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
          pack    titleyr,"Actual ",yr1," vs. ","Actual ",YR2
                  prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,titleyr;
        endif
        add     eightlpi,row
        add     "30",row
          prtpage prfile;*pTitle4:Row,*ALIGNMENT=*CENTER,*font=font12,*boldon,"Last MTD",*boldoff;
.        prtpage prfile;*pTitle4:row,*ALIGNMENT=*Center,*font=font12,curmo;
        add     eightlpi,row
        add     "30",row

.============================================================================================
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*OVERLAYOff,*boldon,*ULON,"Client",*ULOFF,*boldoff;
        prtpage prfile;*pColumn1:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,YR2,*ULOFF,*boldoff;
        if (loopflg > c4)
               goto print1a
        endif
        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,YR1,*ULOFF,*boldoff;
.        prtpage prfile;*pColumn3:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"Difference",*ULOFF,*boldoff;
Print1a
        add     "20",row
.================================================================================================
        Nrev0013ListView1.GetItemCount giving result
        sub c1 from result
Print1
        loop
          until (N9 > result)
                CLEAR STR13
                CLEAR SORTSTR
.==============================================================================
        if (loopflg = c3)
                    Nrev0013ListView1.GetItemText giving STR6 using n9,4
.temp
.          if (str6 = "018710")     .list management exch fee list #
.                              add c1 to n9
.                    goto Print1
.                    endif
        endif
.================================================================================
.        FOR n9,"0",result
                  Nrev0013ListView1.GetItemText giving str13 using n9,1
                clear tmpvar1
                move str13 to tmpvar1
                add tmpvar1 to YR2TOT
                CALL TRIM USING STR13
                if (loopflg > c4)
                        goto Cont
                endif
          Nrev0013ListView1.GetItemText giving SORTSTR using n9,2
                clear tmpvar2
                  move sortstr to tmpvar2
                    add tmpvar2 to YR1TOT
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
                            Nrev0013ListView1.GetItemText giving STR45 using n9,3
                    Nrev0013ListView1.GetItemText giving STR6 using n9,4
.=========================================================================
                    if (loopflg > c4)
                              goto Record
                    endif
                    Nrev0013ListView1.GetItemText giving STR15 using n9,5
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
.                    prtpage prfile;*pcolumn2:row,*font=font12,*ALIGNMENT=*right,sortstr;
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
.                  prtpage prfile;*pcolumn3:row,*font=font12,*ALIGNMENT=*right,str15;
.                      prtpage    prfile;*pcolumn3:row,*OVERLAYON,*font=font12,*ALIGNMENT=*right,str14;
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
.               prtpage prfile;*p3350:row,*pensize=10,*line=7600:row;
               prtpage prfile;*p3350:row,*pensize=10,*line=8100:row;
               add     "30" to row
               prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Grand Totals",*boldoff,*ULOFF;
               clear str13
               move YR2TOT to str13
               call trim using str13
               prtpage prfile;*pcolumn1:row,*OVERLAYON,*font=font12,*ALIGNMENT=*Right,*boldon,str13,*boldoff;

               if (loopflg > c4)
                       goto Print2
               endif
               clear str13
               move YR1TOT to str13
               call trim using str13
               prtpage prfile;*pcolumn2:row,*Overlayon,*font=font12,*ALIGNMENT=*right,*boldon,str13,*boldoff;
               clear str13
               move DIFTOT to str13
               call trim using str13
.               prtpage prfile;*pcolumn3:row,*Overlayon,*font=font12,*ALIGNMENT=*Right,*boldon,str13,*boldoff;
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
        getitem Nrev0013EditCOPY,0,str3
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
                      Move      "Dherric@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\BrokerageRental.pdf",MailAttach
                                 
                                 Elseif     (Loopflg = c2)                       
                                 
                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\BrokerageExchange.pdf"
                                Pause     "10"
                      Move      "Brokerage Exchange PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "Dherric@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\BrokerageExchange.pdf",MailAttach

                                 Elseif     (Loopflg = c3)                       
                                 
                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\Management.pdf"
                                Pause     "10"
                      Move      "Management  PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "Dherric@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\Management.pdf",MailAttach

                                 Elseif     (Loopflg = c4)                       
                                 
                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\ManagementList.pdf"
                                Pause     "10"
                      Move      "Management Exchange PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "Dherric@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\ManagementList.pdf",MailAttach

                                 Elseif     (Loopflg = c5)                       
                                 
                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\ColdBrokerage.pdf"
                                Pause     "10"
                      Move      "Cold Brokerage PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "Dherric@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\ColdBrokerage.pdf",MailAttach

                                 Elseif     (Loopflg = c6)                       
                                 
                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\ColdManagement.pdf"
                                Pause     "10"
                      Move      "Cold Management PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "Dherric@nincal.com",MailTo
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\ColdManagement.pdf",MailAttach
                                 endif    
                      Pause     "10"
                      call       sendmail
                      Pause     "5"
                      Erase      Mailattach

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

        call click_Nrev0013Exit
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
                    setitem   Nrev0013ProgressBar,0,CurVal
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
