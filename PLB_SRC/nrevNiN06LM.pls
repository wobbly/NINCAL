PC         EQU       0
           include common.inc
           include cons.inc
;Patch1.6
                                        include   compdd.inc
                                        include   cntdd.inc
;           include nmlrdd.inc
;Patch1.6
           include ndatdd.inc
           include nrevdd.inc
           include nprjdd.inc

*Sorted Revenue file
INFILE     FILE
newfile    file
.Print file
PRFILE     PFILE

.Projected Totals
..PROFILE    IFILE
.projdolr   iFILE     keylen=8,FIX=274
..projvars   LIST
..projfld    DIM      8
.TYPE     DIM      1       1-1   ---\
.SOURCE   DIM       1      2-2------->  KEY=nrevfld
.CLIENTID DIM    6          3-8------/
..CLIENT1    DIM       45     9-53
..projdol    FORM      11          -64
..proj93     FORM      11          -75
..proj94     FORM      11          -86
..proj95     FORM      11          -97
..proj95a    FORM      11          -108
..proj96     FORM      11          -119
..proj97     FORM      11          -130
..proj97a    FORM      11          -141
..proj98     FORM      11          -152
..proj98a    FORM      11          -164
..proj99     FORM      11          -175
..proj99old  FORM      11          -186
..proj2000   FORM      11          -197
..nin2000    FORM      11           208
..proj2001   FORM      11          -219
..nin2001    FORM      11           230
..proj2001a  FORM      11          -219
..nin2001a   FORM      11           230
..proj2002   FORM      11          -141
..nin2002    FORM      11           274
.         LISTEND
.=====================================================================================
.*********************************************************************************
.************************************************************************************
.Do a search on yearly to find place to change projection value
.change yearly----
YR1        INIT      "2010"
YR2        INIT   "2011"
.*********************************************************************************
.*********************************************************************************
TMPVAR1    FORM     13
TMPVAR2    DIM     14
UNBILL     FORM   10
INCDUE     FORM   10
HIDYR      FORM   10.2
CAL        FORM   11
LOADV      FORM   10
LOADV2      FORM   10
TOTREC     FORM   9
.Sort Parameters=======================================================
INDAT      INIT  "revenue.dat"   .File to be sorted
.INDAT      INIT  "\\nins1\e\data\dbase\revenue.dat"   .File to be sorted
OUTSRT     INIT  "revenue.srt"   .Sorted Output file
.OUTSRT     INIT  "\\nins1\e\data\dbase\revenue.srt"   .Sorted Output file
ClintSrt   INIT  "3-8"                        .Sort by client #
SORTFLE    DIM    70                          .Var to pack file names of sort
PRTITLE    DIM    18                          .Title of Printjob
PRTNAME1   DIM    11                          .Name of printfile
PRTDIR     INIT    "C:\WORK\"                 ."Printfile directory
PRTFILE1   DIM    19                          .full printfile string
.============================================================================
.NINCA List Management
LMLIST       init   "018710"

holdmlr    DIM    6        .held mlr value to check if already in listview object
SORTHLP    FORM   10.2     .add to yr2 total in hidden coulumn to be used as sorted column
PROJVAR    FORM   10       .Variable for this year to date projection
PROJTOT    FORM   10       .Total Projected For Year
OLDTOT     FORM   10       .Var for cumulative total for previous year
OLDTOT2     FORM   10       .Var for cumulative total for previous year
NEWTOT     FORM   10       .Var for cumulative total for this year
NEWTOT2     FORM   10       .Var for cumulative total for this year  NIN
*Flags
MTCHFLG    FORM   1        .flag if client\list a match or not
WRTFLG     FORM   1        .flag to see if it passes one of the conditions
.                                        .if yr2 YTD <> 0
.                                        .Unbilled   <> 0
.                                        .Projytd    <> 0
.                                        .CID        =  0000000
;osflag     form    1       . 1=win 95,98, 2=NT
CHKALL     form    1       .To see if doing both reports
LoopFlg    form    1       .Used for doing multiple passes on reports
.================================================================================================================================================================
.Vars for insertion into listview
Yr1Act     DIM    10             .2003 Year Actuals
Yr1NIN     DIM    10             .2003 Year NINcomm
YTDPRO     DIM    10             .Year to date projected
YR2ACT     DIM    10             .2004 year actuals
Yr2NIN     DIM    10             .2004 Year nincomm
Varbud     DIM    10             .Variance Budget
Vsyr2      DIM    10             .VARBUD vs. yr 2000
UNBILL1    DIM    10             .Unbilled Total
YrEndPro   DIM    10             .Year End Projected
ENDPRO     FORM   10             .Year End Projected Calc Var
TOTDUE     DIM    13             .Total Currently due for year
.====================================================================================
....................................................................
.hold vars for projection
projnew   form      11          hold exchange portion  LR
projlast  form      11          hold exchange portion  LR  for Previous Yearr
.......................................................................
.TOTALS
.Previous Year
NYR1TOT     FORM    10
NYR1TOT2    FORM    10
.YR1TOT     FORM   10.2
NPROYTD     FORM    10
.Current Year
NYR2TOT     FORM    10
.YR2TOT     FORM   10.2
.Unbilled
NUNBILL     FORM    10
.Yr End Projection
NENDPTOT    FORM    10
.OVERALL DUE YTD
NTOTDUE     FORM    10
CALC1       FORM    10.2    use to perform calculation using calc
.List Management  YTD DIFF Tot
NDIFTOT       form    10
NPROTOT     form    10
YTDDIFF     FORM    10
.=====================================================================================
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
TitleDT   dim     18
TitleYR1  dim     12
.====================================================================================
.Print Vars
Rowcount form    3             .KEEP TRACK OF ROW PER PAGE
PgCnt    form    9             .COUNT OF PAGES
NEWPG    FORM    1             .COUNTER TO SHOW- IF NEW PAGE PUT TOTALS AT TOP OF PAGE
COPY     FORM    3             .Number of Copies
.=============================================================
.Defining Columns for Titles
Title1   form    9
Title2   form    9
Title3   form    9
Title4   form    9
Column1a form    9
.Column8  form    9
.Column9  form    9
.=========================
*******************************************************************************
.some goodies for on moving Icon
ICONID   FORM      4
CURICON  FORM      4
FRAMES   FORM      4
ICON$ANIM    ICON
FORM32   FORM      32
FORM32A  FORM      32
FORM32B  FORM      32
.
CurRec    form    5.2
CurVal    form      3
LastVal   form      3
.
lr12mos  form  10
lr12mosn form  10
lr12mosg form  10
yr1acth  form  10
yr1ninh  form  10
.
ANIMICON PLFORM    ANIMATE  * CONTAINS ALL THE ICONS
.begin   release 1.3
.tempfile      file
.end   release 1.3
.end icon goodies
*******************************************************************************
font8    font
        create  font8,"Times New Roman",size=10
font9    font
        create  font9,"Times New Roman",size=10,italic
.==============================================================================
.Position of Columns
        move    "100",column
        move    "700",column1a
        move    "3500",column1
        move    "4500",column2
        move    "5500",column3
        move    "6500",column4
        move    "7500",column5
        move    "8500",column6
        move    "9500",column7
        move    "10500",column8
        move    "11500",column9
        move    "3000",Title1
        move    "8900",Title2
        move    "5600",Title3
        move    "5250",Title4
.        move    "4900",Title4
Release    Init             "1.8"   ASH 11/18/2004          Mailer conversion - increase to 6 bytes
.Release    Init             "1.7"   ASH 08/09/2004         Logo conversion
.Release    Init             "1.6"   DMB 05/13/04 Mailer Conversion
.Release    Init             "1.5"   DB 04/13/02  added code to automate job
;RELEASE   INIT      "1.4"            DB 09/30/02  Added code for subroutine getwinver for XP Printing
;RELEASE   INIT      "1.3"           DB 01/14/02  added code for sonic and progress bar
;.RELEASE   INIT      "1.2"          DB 01/10/02  added code for new format of projection file
;.RELEASE   INIT      "1.1"          DB 12/28/01  added code to reflect new location of revenue file
;.RELEASE   INIT      "1.0"          DB 11/14/01  Actuals vs. Projected Brokerage Report for EOM
.===========================================================================
.======================================================================
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
        move    "NREV0101.PLS",Wprognme
        move    "Actuals vs. Projected",Wfunction
        move    "David Baca",Wauthor
        move    "1.0",Wrelease
        move    "Dec 2001",Wreldate
.=======================================================================
proc    plform  Processing
abt     plform  About
x         PLFORM  NREV0012
.==========================================================================
        winhide
        FORMLOAD x
        formload abt
        formload proc
********************************************************************************
        FORMLOAD ANIMICON
********************************************************************************
.==========================================================================
.==========================================================================
  create  NREV0012;mFile,FData
  create  NREV0012;mEdit,EData,mFile
  create  NREV0012;mOptions,OData,mEdit
  create  NREV0012;mHelp,HData,mOptions
.Activate Menus
.FileGo leads to stop
  activate mFile,FileGo,result
.Need this when it works
  activate mEdit,EditGo,result
  activate mOptions,OptionsGo,result
.Only a SubMenu under this one
  activate mHelp,HelpGo,result
.==============================================================================
.==========================================================================
        move c1 to ndatpath
        getinfo  system,str6
        unpack   str6 into str1,str2
        unpack   str2 into str1
        move     c0 to osflag
..0 = unknown
..1 = Windows NT
..2 = WIN32s Windows 3.1x (obsolete)
..3 = Window 95
..4 = Window 98
..5 = Windows 2000
..6 = XP
..8 = Windows CE
         call getwinver
;        if (str1 = "3" or str1 = "4")
;                 move     c1 to osflag
;        endif
;        if (str1 = "1" or str1 = "5")
;         move     c2 to osflag
;        endif
.===============================================================================
        NProListView.InsertColumn using "",1,1
        NProListView.InsertColumn using "Total Due",100,2
        NProListView.InsertColumn using "Name",100,3
        NProListView.InsertColumn using "2000 Actual",100,4
        NProListView.InsertColumn using "YTD Projected",100,5
        NProListView.InsertColumn using "YTD Actual",100,6
        NProListView.InsertColumn using "Variance Budget",100,7
        NProListView.InsertColumn using "Var vs 2000",100,8
        NProListView.InsertColumn using "Unbilled",100,9
        NProListView.InsertColumn using "Year End Proj",100,10
        NProListview.setcolumnFORMat giving result using 0,0
        NProListview.setcolumnFORMat giving result using 1,1
        NProListview.setcolumnFORMat giving result using 2,1

OpenIt
..        OPEN    REDFILE,"\\nins1\e\data\dbase\revenue"
   prepare    NEWFILE,"c:\work\revenuel.1011"
*.==========================================================================
.use this when turning live
.         OPEN    PROFILE,"\\nins1\e\data\Projdolr"
*=========================================================================
.         OPEN    PROFILE,"c:\work\Projdolr"
.         OPEN    INFILE,"revenue.srt"
.         OPEN    INFILE,"\\nins1\e\data\dbase\revenue.srt"
          MOVE    "1000000000.00" to sorthlp
.        setitem NerepStatTitle,0,prtitle
.==============================================================================
        CLOCK   TIMESTAMP to str8
        UNPACK  str8,cc,yy,mm,dd
.get current month
.        MOVE    mm to nmm                   
        MOVE    c12 to nmm                   
.============================================================
.set current month on form
        move nmm to str2
        rep     " 0",str2
        setitem NprojEditMonth,0,str2
        setitem NprojEditCopy,0,"1"


***********************************************************
;patch1.5
;         loop
;                   waitevent
;         repeat
;patch1.5
************************************************************


Start
        call    OrderSetMouseBusy
***********************************************************************************
.Dynamically reset Animate as same size as VAR0001
anim1
        getprop NREV0012,height=H
        setprop Animate,height=H
        getprop NREV0012,width=V
        setprop Animate,width=V
.Must clear Resize Event which was tiggered when Animate was resized,
.as this is where the AnimateIt subroutine sits.
        clearevent
        moveaddr NREV0012,AnimateWindow
        move    C1,AnimateCurIcon
        move    C4,AnimateFrames
        move    C0,AnimateIconID
        move    "360",H
        move    "575",V
.        CALL   ANIMATEIT

***********************************************************************************
        clear   chkall
        getitem NprojEditMonth,0,str2
        move    str2 to nmm
        getitem NprojEditCopy,0,str3
        move    str3 to copy
.=========================================================================
        getprop NprojRadioAll,SELGROUPID=loopflg
.       0-All 1-Brokerage 2-list mgnt
.=========================================================================
.        branch loopflg to Brok,ListM
         branch loopflg to Brok,ender
         move   c1 to chkall
         clear  loopflg
Restart
.====================================
.Clear var 1/10/02
           move c0,PROJVAR
           move c0,PROJTOT
           move c0,OLDTOT
           move c0,OLDTOT2
           move c0,NEWTOT
           move c0,NEWTOT2
.===================================
         move c0,pgcnt
         move c0,NYR1TOT
         move c0,NYR1TOT2
         move c0,NPROYTD
         move c0,NYR2TOT
         move c0,NUNBILL
         move c0,NENDPTOT
         move c0,NTOTDUE
         move c0,CALC1
         move c0,NDIFTOT
         move c0,NPROTOT
         move c0,YTDDIFF
.====================================
           add    c1 to loopflg
         if     (loopflg > c2)
                call OrderSetMouseFree
                return
         endif
         branch loopflg to Brok,ender
.========================================================================================
Brok
.Brokerage
         PACK  PRTITLE,"Brokerage"
         PACK  PRTNAME1,"BrokBud.LST"
         pack   SortFle,ntwkpath6,indat,comma,ntwkpath1,outsrt
           pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","M","'","&9=","'",YR1,"'","|","2=","'","M","'","&9=","'",YR2,"'"
.          pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","B","'","&9=","'",YR1,"'","|","2=","'","B","'","&9=","'",YR2,"'"
         sort   taskname
         if over
               alert caution,S$ERROR$,result,"No Sort"
               call OrderSetMouseFree
               return
         endif
*********************************************************************

          call      NREVInitProgressBar
.         setitem   NMDLStatProgress,0,""
.              setitem        NMDLStatProgress,0,"Loading Instructions"
              pack str45 with ntwkpath1,outsrt
.              move          "\\nins1\e\data\text\NINMDLST.dat" to str45
              open          tempfile,str45
          positeof      tempfile
          fposit    tempfile,N10
          calc      howmany=(N10/560)   .'560 = 558(NINMDLST record length) + 2 bytes for CR/LF
              close         tempfile
*********************************************************************
          OPEN    INFILE,outsrt
        reposit infile,c0
        NPROListview.deleteallitems
          NProListView.SetcolumnText using 1,"Total Due"
        NProListView.SetcolumnText using 2,"Name"
        NProListView.SetcolumnText using 3,"2003 Actual"
        NProListView.SetcolumnText using 4,"2003 NIN"
;        NProListView.SetcolumnText using 4,"YTD Projected"
        NProListView.SetcolumnText using 5,"YTD Actual"
        NProListView.SetcolumnText using 6,"2004 NIN"
;        NProListView.SetcolumnText using 6,"Variance Budget"
;        NProListView.SetcolumnText using 7,"Budget Vs. 2003"
        NProListView.SetcolumnText using 7,"2002 actual"
;        NProListView.SetcolumnText using 8,"Unbilled"
        NProListView.SetcolumnText using 8,"2002 NIN"
        NProListView.SetcolumnText using 9,"Year End Pro"
         goto Reader
.====================================================================================
.====================================================================================
ListM
.List Management
         PACK  PRTITLE,"List Mgmnt"
         PACK  PRTNAME1,"Listbud.LST"
         pack   SortFle,ntwkpath6,indat,comma,ntwkpath1,outsrt
.         pack   SortFle,indat,comma,outsrt
.          pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","M","'","&9=","'",YR2,"'"
           pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","M","'","&9=","'",YR1,"'","|","2=","'","M","'","&9=","'",YR2,"'"
         sort   taskname
         if over
               alert caution,S$ERROR$,result,"No Sort"
               call OrderSetMouseFree
               return
         endif
*********************************************************************
          call      NREVInitProgressBar
              pack str45 with ntwkpath1,outsrt
              open          tempfile,str45
          positeof      tempfile
          fposit    tempfile,N10
          calc      howmany=(N10/560)   .'560 = 558(NINMDLST record length) + 2 bytes for CR/LF
              close         tempfile
*********************************************************************
          OPEN    INFILE,outsrt
        reposit infile,c0

        NPROListview.deleteallitems
          NProListView.SetcolumnText using 1," 2003 Acutals"
        NProListView.SetcolumnText using 2,"Projection"
        NProListView.SetcolumnText using 3,"YTD Difference"
        NProListView.SetcolumnText using 4,"2003 Projected"
        NProListView.SetcolumnText using 5,"Unbilled"
        NProListView.SetcolumnText using 6,"Client"
        NProListView.SetcolumnText using 7,"Client ID"
        NProListView.SetcolumnText using 8,""
        NProListView.SetcolumnText using 9,""
        NProListView.SetcolumnText using 10,""
        goto Reader
.====================================================================================
READER
**************************************
        loop
**************************************
          clear    wrtflg
        READ     INFILE,SEQ;NREVVARS
        goto     Print if over
**************************************
;        CALL   ANIMATEIT
          CALL   NREVUpdateProgressBar
**************************************
.=======================================================================================
.Clear Vars for next year
        clear   incdue
        clear   totdue
        clear   str13
        clear   yr1act
        clear   ytdpro
        clear   yr2nin
        clear   yr2act
        clear   mtchflg
        clear   Projvar
        clear   OLDTOT
        clear   OLDTOT2
        clear   NEWTOT
        clear   NEWTOT2
        clear   projtot
.        clear   projvar
        clear   unbill
        clear   loadv
        clear   loadv2
.========================================================================================
        if (loopflg = c1)
          if (CID = holdmlr)                      .check to see if mlr\lst already processed
                    goto reader
          endif
        endif
.========================================================================================
        move    CID to holdmlr
.Check to see if Clients meet criteria for Report
        branch  loopflg to Brkchk,ender
Brkchk
        cmatch  SRC to "M"
        goto    READER if not equal
        if      (YR0 = YR2)
                 move    ".00" to oldtot
                     move    OLDTOT to YR1ACT
                 pack nrevfld,type,src,cid,yr0
                     goto Yr2Start
        endif
        if      (YR0 = YR1)
                 pack  nrevfld,type,src,cid,yr0
                 goto addold
.=================================================
LMCHK
        cmatch  SRC to "M"
        goto    READER if not equal
        if      (CID = LMLIST)
                goto reader
        endif
        if      (YR0 = YR2)
                 pack nrevfld,type,src,cid,yr0
                     goto Yr2Start
        endif
        if      (YR0 = YR1)
.===================================================================================
.Check to see if this should include all records for last year or just ones matching
.The current years clients
                for n2,"1",nmm
                LOAD LOADV using n2 from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
                SEPLR,OCTLR,NOVLR,DECLR
                add LOADV to NYR1TOT
                LOAD LOADV2 using n2 from JANnin,FEBnin,MARnin,APRnin,MAYnin,JUNnin,JULnin,AUGnin:
                SEPnin,OCTnin,NOVnin,DECnin
                add LOADV2 to NYR1TOT2
                repeat
        endif
JD4     goto reader
.==================================================================================
ADDOLD
                add revunbld to unbill
.===================================================================================
                for n2,"1",nmm  updated jd 3/27/06
.                for n2,"1",c4
                LOAD LOADV using n2 from c0,c0,Marlr,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
                SEPLR,OCTLR,NOVLR,DECLR
.                LOAD LOADV using n2 from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
.                SEPLR,OCTLR,NOVLR,DECLR
                add LOADV to OLDTOT
                repeat
.===================================================================================
.===================================================================================
                for n2,"1",nmm  updated jd 3/27/06
.                for n2,"1",c4
.                LOAD LOADV using n2 from JANnin,FEBnin,MARnin,APRnin,MAYnin,JUNnin,JULnin,AUGnin:
.                SEPnin,OCTnin,NOVnin,DECnin
                LOAD LOADV using n2 from c0,c0,marnin,APRnin,MAYnin,JUNnin,JULnin,AUGnin:
                SEPnin,OCTnin,NOVnin,DECnin
                add LOADV to OLDTOT2
                repeat
.===================================================================================
.if second pass of previous year
                if (mtchflg = c1)
                                move c0 to mtchflg
                    if (OLDTOT = C0)
                        else
                                move    c1 to wrtflg    .if total for prev year is zero set flag to be written out
                        endif
                        move    oldtot to yr1act
                        move    oldtot2 to yr1nin
                          unpack  nrevfld,type,src,cid,yr0
                              move      yr2 to yr0       .Moving to next year
                          pack    nrevfld,type,src,cid,yr0
                        clear   newtot
                        clear   newtot2
                              goto nextread
                endif
.Prepare for second pass
.                unpack  nrevfld,type,src,cid,yr0
.                if (type = "R")
.                        move "E" to type
.                else
.                        move "R" to type
.                endif
.                pack    nrevfld,type,src,cid,yr0
.Read Revenue file
.                call    NREVKEY
.                if not over
.                        move c1 to mtchflg
.                        goto ADDOLD
.                else
.                        move c0 to mtchflg
.                   if (OLDTOT = C0)
.                        else
.                                move c1 to wrtflg       .if total for prev year is not zero set write flag
.                        endif
                                move    oldtot to yr1act
                        move    oldtot2 to yr1nin
                                    unpack  nrevfld,type,src,cid,yr0
                              move      yr2 to yr0
                                    pack    nrevfld,type,src,cid,yr0
                              goto    nextread
.                endif

NextRead
.READ of revenue file by index
                call    NREVKEY
.=============================================================================
                if over
                        if     (mtchflg = c1)
                                if (NEWTOT <> C0)
                                        move c1 to wrtflg
                                endif
                                move  newtot to yr2act
                                move c0 to mtchflg
                              goto Projread
                        endif
                else
Yr2Start
.================================================================================
                add revunbld to unbill
                for n2,"1",nmm
                          LOAD LOADV using n2 from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
                  SEPLR,OCTLR,NOVLR,DECLR
                    add LOADV to NEWTOT
                repeat
                move newtot to yr2act
act2004nin
                for n2,"1",nmm
                LOAD LOADV using n2 from JANnin,FEBnin,MARnin,APRnin,MAYnin,JUNnin,JULnin,AUGnin:
                SEPnin,OCTnin,NOVnin,DECnin
                add LOADV to newtot2
                repeat
                move newtot2 to yr2nin

.===================================================================================
                if (loopflg = c2)
                    if (NEWTOT > C0)
                            unpack  nrevfld,type,src,cid,yr0
.                 pack    Projfld with TYPE,SRC,CID
.                           READ    PROFILE,PROJFLD;PROJVARS
.                        add     proj2001 to PROJTOT
**********************************************************************************
*Code for new projection file
                    move      c0,projlast
                    if (src = "M")
tester55
                              move      " M",str2
                              move      C0,N2
                              loop
                                        add       C1,N2
                              move      N2,str3
                              rep       zfill,str3
                              pack      NPRJFLD,str2,CID,YR0,str3
.                             pack      NPRJFLD,str2,str6,PrevYear,str3
*                             rep       zfill,NPRJFLD
.                             move      "detout,3-NPRJKEY",Location
.                             pack      KeyLocation,"Key: ",NPRJFLD
                              call      NPRJKEY
                              until over
                            move        Prjar,projlast
.                           move        ProjNIN,ninlast
                              until (PrjMast = YES)
                    repeat
                add     projlast,PROJTOT
          endif
*********************************************************************************
                        move    projtot to calc1
                    CALC      cal = (calc1/12)*nmm
                              move    cal to Projvar
                        move    Projvar to YTDPRO

.YTD Actuals
                            clear hidyr
.Creating "hidden" total for sorting purposes
                    add  sorthlp,newtot,hidyr
                            move hidyr to str13
                            NProListView.InsertItem giving N7 using str13
                            NProListView.SetItemText giving N8 using n7,yr2act,1
.YTD Projection
;                           NProListView.SetItemText giving N8 using n7,YTDPRO,2
                            NProListView.SetItemText giving N8 using n7,yr2nin,2
.YTD Differential
                            clear str10
                            sub  PROJVAR,NEWTOT,YTDDIFF
                            move YTDDIFF to str10
                            NProListView.SetItemText giving N8 using n7,str10,3
.Total Projection for Year
                            clear str10
                            move projtot to str10
                            NProListView.SetItemText giving N8 using n7,str10,4
.Unbilled
                            move unbill to unbill1
                            NProListView.SetItemText giving N8 using n7,unbill1,5
.Client
                            move holdmlr to ndatfld
                        call zfillit using ndatfld
                            CALL NDATKEY
                            if not over
                              move OLSTNAME to client
                            endif
                    NProListView.SetItemText giving N8 using n7,client,6
                        move holdmlr to str6
                        NProListView.SetItemText giving N8 using n7,str6,7
                        endif
                        goto reader
                endif

.=====================================================================================
NCHECK
                        if    (mtchflg = c1)
                                if (NEWTOT <> C0)
                                        move c1 to wrtflg
                                endif
                                move newtot to yr2act
                                move c0 to mtchflg
                                  goto Projread
                        endif
                endif
.Prepare for second pass
                          unpack  nrevfld,type,src,cid,yr0
                    if (type = "R")
                              move "E" to type
                              else
                              move "R" to type
                    endif
                          pack    nrevfld,type,src,cid,yr0
                        move  c1 to mtchflg
                        goto    nextread
PROJREAD
                  unpack  nrevfld,type,src,cid,yr0
.                pack    Projfld with TYPE,SRC,CID
SECPROJREAD
.                 READ    PROFILE,PROJFLD;PROJVARS
***************************************************************************************
*Code for new projection file
          move      c0,projlast
.         move      c0,ninlast
          move      c0,projnew
.         move      c0,ninnew
          if (src = "B")
.START PATCH 1.8 ADDED LOGIC
                    unpack    CID,str2,str4
                    pack      COMPFLD3,str4
                    move      "COMPKEY3",Location
                    pack      KeyLocation,"Key: ",COMPFLD3
                    call      COMPKEY3
.END PATCH 1.8 ADDED LOGIC
                    move      "EB",str2
                    move      C0,N2
                    loop
                              add       C1,N2
                              move      N2,str3
                              rep       zfill,str3
.START PATCH 1.8 ADDED LOGIC
.                             pack      NPRJFLD,str2,cid,YR0,str3
                              pack      NPRJFLD,str2,COMPCOMP,YR0,str3
.END PATCH 1.8 ADDED LOGIC
.                             pack      NPRJFLD,str2,str6,PrevYear,str3
                              rep       zfill,NPRJFLD
.                             move      "detout-NPRJKEY",Location
.                             pack      KeyLocation,"Key: ",NPRJFLD
                              call      NPRJKEY
                              until over
                            move        prjar,projnew
.                           move        ProjNIN,ninnew
                              until (PrjMast = YES)
                    repeat
                    move      "RB",str2
                    move      C0,N2
                    loop
                              add       C1,N2
                              move      N2,str3
                              rep       zfill,str3
.START PATCH 1.8 ADDED LOGIC
.                             pack      NPRJFLD,str2,cid,YR0,str3
                              pack      NPRJFLD,str2,COMPCOMP,YR0,str3
.END PATCH 1.8 ADDED LOGIC
.                             pack      NPRJFLD,str2,str6,PrevYear,str3
                              rep       zfill,NPRJFLD
.                             move      "detout,2-NPRJKEY",Location
.                             pack      KeyLocation,"Key: ",NPRJFLD
                              call      NPRJKEY
                              until over
                            move        prjar,projlast
.                           move        ProjNIN,ninlast
                              until (PrjMast = YES)
                    repeat
          endif
        move    Projvar to YTDPRO
        add     projnew,projlast,PROJTOT
**************************************************************************************
.                if over
.                        if (mtchflg = c1)
.                                 move c0 to mtchflg
.                                move Projvar to YTDPRO
.                         goto  project
.                         endif
.                else
.Yearly
.Change hard coded var-needs to be changed yearly
.=============================================================================
.                        add proj2001 to PROJTOT
.=============================================================================
.                        if (mtchflg = c1)
.                                move c0 to mtchflg
.                             goto project
.                        endif
.                endif
.                         unpack  projfld,type,src,cid
.                   if (type = "R")
.                             move "E" to type
.                             else
.                             move "R" to type
.                   endif
.                         pack    projfld,type,src,cid
.                        move           c1 to mtchflg
.                        goto    SECPROJREAD

.Projected for current year
Project
                        move      projtot to calc1
                    CALC        cal = (calc1/12)*nmm
                              move      cal to Projvar
                        move      Projvar to YTDPRO
.=============================================================================
.Final Check of Criteria for brokerage
                        if (Projvar <> c0)
                                move c1 to wrtflg
                        endif
                        if (unbill <> c0)
                                move c1 to wrtflg
                        endif
                        if (wrtflg = c1)
                                goto listview
                        else
                                goto reader
                        endif
        endif
.======================================================================================
Listview

        move unbill to unbill1
.Total income due with YTD Revenue and unbilled
        add unbill to newtot,INCDUE
        clear hidyr
.Creating "hidden" total for sorting purposes
        add  sorthlp,incdue,hidyr
        move hidyr to str13
        NProListView.InsertItem giving N7 using str13
        move incdue to totdue
        NProListView.SetItemText giving N8 using n7,totdue,1
.        unpack    holdmlr,str2,str4
.        PACK      MKEY FROM str4,"000"
.        CALL      NMLRKEY
.        if not over
.                   move      mcomp to client
.        endif
                            move holdmlr to ndatfld
                        call zfillit using ndatfld
                            CALL NDATKEY
                            if not over
                              move OLSTNAME to client
                            endif
        NProListView.SetItemText giving N8 using n7,client,2
        NProListView.SetItemText giving N8 using n7,yr1act,3
        NProListView.SetItemText giving N8 using n7,yr1nin,4    -----
        NProListView.SetItemText giving N8 using n7,YR2ACT,5
        NProListView.SetItemText giving N8 using n7,YR2nin,6
                      move          yr2act to LR12mos
                      move          yr1act to yr1acth
                      add           yr1acth to lr12mos
                      move          yr2nin to lr12mosn
                      move          yr1nin to yr1ninh
                      add           yr1ninh to lr12mosn
                      add           lr12mos to lr12mosg
                      add           lr12mosn to lr12mosg
                      move          c0 to n2
                      move          mslsper to n2
.                   load      str25 from n2 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
.                             osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
.                             osls17,osls18,osls19,osls20,osls21,osls22
                              write     newfile,seq;src,cid,client,lr12mos,lr12mosn,lr12mosg,b1,compaddr,compzip,compstate,str25
                                        move      c0 to lr12mos
                                        move      c0 to lr12mosn
                                        move      c0 to yr1ninh
                                        move      c0 to yr1acth
                                        move      c0 to lr12mosn
                                        move      c0 to lr12mosg

.                             write     newfile,seq;cid,client,yr2act,yr2nin,yr1act,yr1nin
.=====================================================================================
;        if        ((NEWTOT = ".00") & (Projvar = ".00"))
;                  move "0" to varbud
;        else
;                move     newtot to CALC1
;                 CALC          n9 =   (CALC1/Projvar)*100
;         move      n9 to varbud
;                 if       (varbud = "")
;                 move "0" to varbud
;                 endif
;        endif
;        call trim using varbud
;        NProListView.SetItemText giving N8 using n7,Varbud,6
;REPLACED WITH NIN FOR 2004 JDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
;
        if        ((NEWTOT = ".00") & (OLDTOT = ".00"))
                  move "0" to vsyr2
        else
                move     newtot to CALC1
          CALC        n9 =   (CALC1/OLDTOT)*100
                  move      n9 to vsyr2
          if       (vsyr2 = "")
                    move "0" to vsyr2
                  endif
        endif
        call trim using VSYR2
        NProListView.SetItemText giving N8 using n7,VSYR2,7
        NProListView.SetItemText giving N8 using n7,Unbill1,8
        sub newtot from ProjTOT
        move PROJTOT to YRENDPRO
        NProListView.SetItemText giving N8 using n7,YrEndPro,9
        repeat
.==================================================================
Print
        clear n9
                PACK PRTFILE1,PRTDIR,PRTNAME1
;patch1.4
        if (osflag = "1" or osflag = "5" or osflag = "6")
          PRTOPEN prfile,"\\srv2008a\Laser8",PRTITLE,noprint,spoolfile=PRTFILE1
        elseif (osflag = c3 | OSflag =c4)         .win 95 98
          PRTOPEN prfile,"Laser8",PRTITLE,noprint,spoolfile=PRTFILE1
        Else  
          PRTOPEN prfile,"-",PRTITLE,noprint,spoolfile=PRTFILE1
        endif

.=================================================================
.Headers
.Defining Header and Titles
Page
        CLEAR     ROWCOUNT
        ADD       C1 TO PGCNT
        prtpage   prfile;*NEWPAGE:
                               *ORIENT=*LANDSCAPE:
                   *UNITS=*HIENGLISH;
        clear     row
        move      "300",row
        prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date:";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*font=font12,str10;
        pack titleyr1,"Revenue ",YR2
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,titleyr1;
        add     eightlpi,row
        add     "30",row
        if (loopflg = c2)
                  prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,"List Management";
        else
          prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,"Brokerage";
        endif
        add     eightlpi,row
        add     "55",row
          pack    titleyr,"Projected"," vs. ","Actual"
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,titleyr;
        add     eightlpi,row
        add     "30",row
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,"Year to Date Ending ";
        Load    curmo,nmm with Month1,Month2,Month3,Month4,Month5,Month6,Month7:
                Month8,Month9,Month10,Month11,Month12
        call    trim using curmo
        pack    TITLEDT,curmo,b1,dd,comma,yr2
          prtpage prfile;*ALIGNMENT=*CENTER,*font=font12,*boldon,TITLEDT,*boldoff;
        add     eightlpi,row
        add     "30",row
        add     eightlpi,row
        if (loopflg = c2)
.===========================================================================================
        PACK str11 with YR2,b1,"YTD"
          prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*right,*boldon,*ll,str11,*boldoff;
        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*right,*boldon,"YTD",*boldoff;
        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*ll,*boldon,*ULON,yr2,*ULOFF,*boldoff;
        add     eightlpi,row
        add     "50",row
        prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"ID",*ULOFF,*boldoff;
        prtpage prfile;*pColumn1a:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Client",*ULOFF,*boldoff;
        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"Projected",*ULOFF,*boldoff;
        PACK str11 with YR2,b1,"Actual"
        prtpage prfile;*pColumn3:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,str11,*ULOFF,*boldoff;
        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"Difference",*ULOFF,*boldoff;
        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*ll,*boldon,*ULON,"Projection",*ULOFF,*boldoff;
        prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Unbilled",*ULOFF,*boldoff;
.=================
        else
        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,"YTD",*boldoff;
        prtpage prfile;*pColumn3:row,*font=font12,*ALIGNMENT=*right,*boldon,"YTD",*boldoff;
        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*right,*boldon,"YTD",*boldoff;
        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*boldon,"YTD",*boldoff;
        prtpage prfile;*pColumn7:row,*font=font12,*ALIGNMENT=*right,*boldon,"YTD",*boldoff;
        prtpage prfile;*pColumn8:row,*font=font12,*ALIGNMENT=*right,*boldon,"YTD",*boldoff;
        add     eightlpi,row
        add     "50",row
        prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Client",*ULOFF,*boldoff;
        PACK str11 with YR2,b1,"LRINC"
        prtpage prfile;*pColumn1:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,str11,*ULOFF,*boldoff;
;        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"Projected",*ULOFF,*boldoff;
        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"2004 NIN",*ULOFF,*boldoff;
        PACK str11 with YR1,b1,"LRinc"
        prtpage prfile;*pColumn3:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,str11,*ULOFF,*boldoff;
;        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Budget %",*ULOFF,*boldoff;
        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"2003 NIN",*ULOFF,*boldoff;
        pack    str11,"2002 Lrinc"
        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*ll,*boldon,*ULON,str11,*ULOFF,*boldoff;
        prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"2002 NIN",*ULOFF,*boldoff;
        prtpage prfile;*pColumn7:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"2001 LRinc",*ULOFF,*boldoff;
        prtpage prfile;*pColumn8:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"2001 NIN",*ULOFF,*boldoff;
        endif
        add     eightlpi,row
        add     eightlpi,row
Print1
        NProListView.GetItemCount giving result
        sub c1 from result
        loop
          until (N9 > result)
                    add c1 to rowcount
                  CLEAR STR13
          CLEAR STR16
.Client Name
        if (loopflg = c2)
          NProListView.GetItemText giving Client using n9,6
          NProListView.GetItemText giving str6 using n9,7
                  prtpage prfile;*pColumn:row,*font=font8,*ALIGNMENT=*Left,*ll,str6;
                  prtpage prfile;*pColumn1a:row,*font=font8,*ALIGNMENT=*Left,CLIENT;
                goto Pro1
          else
                    NProListView.GetItemText giving Client using n9,2
                                      prtpage prfile;*pColumn:row,*font=font8,*ALIGNMENT=*Left,CLIENT;
        endif
.Previous Year Actuals
ACT12003
                  NProListView.GetItemText giving str10 using n9,3
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NYR1TOT
                if (tmpvar1 = c0)
                        move  c0 to str14
                            prtpage prfile;*pColumn3:row,*font=font8,*ALIGNMENT=*right,*ll,str14;

                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
                                      prtpage prfile;*pColumn3:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                              else

                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2,str14
                              prtpage prfile;*pColumn3:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif
ACT1nin2003
                  NProListView.GetItemText giving str10 using n9,4
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NYR1TOT2
                if (tmpvar1 = c0)
                        move  c0 to str14
                            prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str14;

                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
                                      prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                              else

                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2,str14
                              prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif

.Year to Date Projected
PRO1
.LM Year to date Projected
        if (loopflg = c2)
                    NProListView.GetItemText giving str10 using n9,2
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NPROYTD
                if (tmpvar1 = c0)
                        move tmpvar1 to str14
                        move c0 to str14
                            prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
                                      prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        else
                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2,str14
                                      prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif
        else
.Brk
;                   NProListView.GetItemText giving str10 using n9,4
;                clear tmpvar1
;                move str10 to tmpvar1
;                add tmpvar1 to NPROYTD
;                if (tmpvar1 = c0)
;                        move tmpvar1 to str14
;                        move c0 to str14
;                           prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
;                else
;                        if (tmpvar1 < c0)
;                             call removechar using str10,dash
;                           call FormatNumeric using str10,str13,comma
;                                pack str14,dash,str13
;                                     prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
;                        else
;                             call FormatNumeric using str10,tmpvar2,comma
;                                move tmpvar2,str14
;                                     prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
;                        endif
;                endif
        endif
.=========================================================================
.Current Yr Actuals
.LM
        if (LOOPFLG = c2)
          NProListView.GetItemText giving STR10 using n9,1
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NYR2TOT
                if (tmpvar1 = c0)
                        move  c0 to str14
                            prtpage prfile;*pColumn3:row,*font=font8,*ALIGNMENT=*center,*ll,str14;
                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
                                      prtpage prfile;*pColumn3:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        else
                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2 to str14
                                      prtpage prfile;*pColumn3:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif
                goto  Ubill
        else
.BRK 2004 actuals
          NProListView.GetItemText giving STR10 using n9,5
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NYR2TOT
                if (tmpvar1 = c0)
                        move  c0 to str14
                            prtpage prfile;*pColumn1:row,*font=font8,*ALIGNMENT=*center,*ll,str14;
                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
                                      prtpage prfile;*pColumn1:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        else
                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2 to str14
                                      prtpage prfile;*pColumn1:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif
        endif
.Variance Budget/changed to 2004 NIN jd.

                clear str9
                  NProListView.GetItemText giving STR9 using n9,6
                  prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,str9;
.Variance vs 2000

;                clear str9
;                 NProListView.GetItemText giving str9 using n9,7
;                 prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,str9;


.Unbilled
UBill

        if (loopflg = c2)
                  NProListView.GetItemText giving str10 using n9,5
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NUNBILL
                if (tmpvar1 = c0)
                        move tmpvar1 to str14
                        move c0 to str14
                            prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,str14;

                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
                                      prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        else
                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2,str14
                              prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif
        else
                  NProListView.GetItemText giving str10 using n9,8
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NUNBILL
                if (tmpvar1 = c0)
                        move tmpvar1 to str14
                        move c0 to str14
                            prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,str14;

                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
                                      prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        else
                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2,str14
                              prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif
        endif
.Year End Projection
                if (loopflg = c2)
                NProListView.GetItemText giving str10 using n9,4
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NENDPTOT
                if (tmpvar1 = c0)
                        move tmpvar1 to str14
                        move c0 to str14
                            prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
                                prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        else
                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2 to str14
                                      prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif
                        goto diff
                else

                  NProListView.GetItemText giving str10 using n9,9
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NENDPTOT
                if (tmpvar1 = c0)
                        move tmpvar1 to str14
                        move c0 to str14
                            prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
                                prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        else
                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2 to str14
                                      prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif
                endif
.=====================================================================================
.Amount Billed\Owed

                  NProListView.GetItemText giving str10 using n9,1
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NTOTDUE
                if (tmpvar1 = c0)
                        move tmpvar1 to str14
                        move c0 to str14
                            prtpage prfile;*pColumn8:row,*font=font8,*ALIGNMENT=*right,*ll,tmpvar1;
                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
                                      prtpage prfile;*pColumn8:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        else
                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2 to str14
                              prtpage prfile;*pColumn8:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif
                goto record
**********************************************************************************************
Diff
.YTD Difference
                  NProListView.GetItemText giving str10 using n9,3
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NDIFTOT
                if (tmpvar1 = c0)
                        move tmpvar1 to str14
                        move c0 to str14
                            prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
                                prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        else
                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2 to str14
                                      prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif

.================================================================================
Record
                              add     eightlpi,row
                              add     "35",row
Rower
            if (ROWCOUNT = "36")
                           move "7750",row
                           prtpage prfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,"Page# ";
                           prtpage prfile;*font=font8,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.7 REPLACED LOGIC
.                          prtpage prfile;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
                           prtpage prfile;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News";
.END PATCH 1.7 REPLACED LOGIC
                        add c1 to n9
                        goto page
                  else
                    add c1 to n9
                  endif
        repeat

.=======================================================================================
ENDLOOP
       if (ROWCOUNT < "32")
                 goto totals
       else
.Added to correct page # and page label for next to last page
.==========================================================================================
                 move "7750",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.7 REPLACED LOGIC
.                prtpage prfile;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA";
                 prtpage prfile;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News";
.END PATCH 1.7 REPLACED LOGIC
.===========================================================================================
               move c1 to newpg
               add  c1 to pgcnt
               prtpage prfile;*NEWPAGE:
                              *UNITS=*HIENGLISH:
                                    *ORIENT=*LANDSCAPE;
               goto Totals
       endif

TOTALS
       if (newpg = c1)
                 move "520",row
          move c0 to newpg
       endif
               add     eightlpi,row
               add     "50" to row
               prtpage prfile;*p2700:row,*pensize=10,*line=11000:row;
               add     "30" to row
               prtpage prfile;*pColumn:row,*font=font8,*ALIGNMENT=*Left,*boldon,*ULON,"Grand Totals",*boldoff,*ULOFF;
               clear str13
               clear str14
               clear str10
               move NYR1TOT to str10
               call removechar using str10,dash
               call FormatNumeric using str10,str13,comma
               if (NYR1TOT < c0)
                       pack str14,dash,str13
               else
                   move str13 to str14
               endif
               if (loopflg = c1)
                              prtpage prfile;*pcolumn1:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
               endif
               clear str14
               clear str10
               clear str13
               move NPROYTD to str10
               call removechar using str10,dash
               call FormatNumeric using str10,str13,comma
               if (NPROYTD < c0)
                       pack str14,dash,str13
               else
                   move str13 to str14
               endif
               if (loopflg = c2)
               prtpage prfile;*pcolumn2:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
               else
               prtpage prfile;*pcolumn2:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
               endif
               clear str14
               clear str10
               clear str13
               move NYR2TOT to str10
               call removechar using str10,dash
               call FormatNumeric using str10,str13,comma
               if (NYR2TOT < c0)
                       pack str14,dash,str13
               else
                   move str13 to str14
               endif
               if (loopflg = c2)
               prtpage prfile;*pcolumn3:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
               goto Punbil
               else
               prtpage prfile;*pcolumn3:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
               endif
.==================================================================================================
                clear     n9
                move      NPROYTD to CALC1
                  CALC          n9 =   ((NYR2TOT-NPROYTD)/CALC1)*100
                clear     str9
          move      n9 to str9
                  if       (str3 = "")
                   move "0" to str3
                  endif
               prtpage prfile;*pcolumn4:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str9,*boldoff;
.==================================================================================================
                clear     n9
                move      NYR1TOT to CALC1
                  CALC          n9 =   ((NYR2TOT-NYR1TOT)/CALC1)*100
                clear     str9
          move      n9 to str9
                  if        (str9 = "")
                   move "0" to str9
               endif
               prtpage prfile;*pcolumn5:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str9,*boldoff;

.==================================================================================================
PUNBIL
               clear str14
               clear str10
               clear str13
               move NUNBILL to str10
.===================================================================================
               call removechar using str10,dash
               call FormatNumeric using str10,str13,comma
               if (NUNBILL < c0)
                       pack str14,dash,str13
               else
                   move str13 to str14
.===================================================================================
               endif
               if (loopflg = c2)
               prtpage prfile;*pcolumn6:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
               else
               prtpage prfile;*pcolumn6:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
               endif
               clear str14
               clear str10
               clear str13
               move NENDPTOT to str10
.===================================================================================
               call removechar using str10,dash
               call FormatNumeric using str10,str13,comma
               if (NENDPTOT < c0)
                       pack str14,dash,str13
               else
                   move str13 to str14
               endif
               if (loopflg = c2)
               prtpage prfile;*pcolumn5:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
               goto PYTDIFF
               else
               prtpage prfile;*pcolumn7:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
               endif
               clear str14
               clear str10
               clear str13
               move NTOTDUE to str10
               call removechar using str10,dash
               call FormatNumeric using str10,str13,comma
               if (NTOTDUE < c0)
                       pack str14,dash,str13
               else
                   move str13 to str14
               endif
                 prtpage prfile;*pcolumn8:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
               goto LastLine
PYTDIFF
*****************************************************
               clear str14
               clear str10
               clear str13
               move NDIFTOT to str10
.               move YTDDIFF to str10
               call removechar using str10,dash
               call FormatNumeric using str10,str13,comma
               if (NDIFTOT < c0)
                       pack str14,dash,str13
               else
                   move str13 to str14
               endif
               prtpage prfile;*pcolumn4:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
****************************************************
.Projected for Year Total
*****************************************************
LastLine
               add     eightlpi,row
               add     eightlpi,row
                 prtpage prfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,"Total Records: ";
               clear str9
               move RESULT to str9
                 prtpage prfile;*font=font8,*ALIGNMENT=*Left,*ll,STR9;
               add     eightlpi,row
               add     "30" to row
               if (loopflg = c1)
                 prtpage prfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,*ll,"Note: Does not include records with '0' YTD dollars":
               " & which projection is '0', or Cold New Brokerage.  Unbilled is YTD for maildate within report Year Only.";
               endif
               if (loopflg = c2)
                 prtpage prfile;*pcolumn1:row,*font=font8,*ALIGNMENT=*Left,*ll,yr1;
                 prtpage prfile;*font=font8,*ALIGNMENT=*Left,*ll," YTD Actuals: ";
               move NYR1TOT to str10
                 call FormatNumeric using str10,tmpvar2,comma
                 prtpage prfile;*pcolumn3:row,*font=font9,*ALIGNMENT=*Right,*boldon,*ll,tmpvar2;
               add     eightlpi,row
               add     "30" to row
                 prtpage prfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,*ll,"Note: Ranked by 2003 actual booked income.";
;                prtpage prfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,*ll,"Note: Ranked by " 
;                prtpage prfile;YR2;
;                prtpage prfile;" actual booked income.";
               endif
.=============================================================
.Footer for Last Page
Print2
                 move "7750",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,*ll,PgCnt;
.START PATCH 1.7 REPLACED LOGIC
.                prtpage prfile;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News/CA"
                 prtpage prfile;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News"
.END PATCH 1.7 REPLACED LOGIC
.=================================================================================================

End1
        PRTCLOSE prfile
        if (copy = c0)
                if (chkall = c1)
                        if (loopflg > c2)
                              goto ender
                        endif
                        goto restart
                  endif
                goto  Ender
        endif
        clear n3
;patch1.4

        if (osflag = "1" or osflag = "5" or osflag = "6")
;        if (osflag = c2)
;subpatch1.4
                    loop
                    until (N3 = COPY)
                              PRTPLAY PRTFILE1,"\\srv2008a\Laser8"
                    add c1 to n3
                    repeat
                erase PRTFILE1
                if (chkall = c1)
                        if (loopflg > c2)
                              goto ender
                        endif
                        goto restart
                  endif
                goto  Ender
       elseif (osflag = c3 | OSflag =c4)

                  loop
                    until (N3 = COPY)
                              PRTPLAY PRTFILE1,"Laser8"
                          add c1 to n3
                    repeat
                erase PRTFILE1
                if (chkall = c1)
                        if (loopflg > c2)
                              goto ender
                        endif
                        goto restart
                  endif
                goto  Ender
        else                  

                  loop
                    until (N3 = COPY)
                              PRTPLAY PRTFILE1,"-"
                          add c1 to n3
                    repeat
                erase PRTFILE1
                if (chkall = c1)
                        if (loopflg > c2)
                              goto ender
                        endif
                        goto restart
                  endif
                goto  Ender
       endif
.==========================================================================
FileGo
.Flag set to "N" if in Modify or New mode
.        branch result to FileGo1,FileGo2,FileGo3,FileGo3
        branch result to FileGo1
FileGo1
        call click_NREV0012Exit
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
.................................................................................
.begin patch 1.3
NREVUpdateProgressBar
          calc      CurRec=(CurRec+1)
          calc      CurVal=((CurRec/howmany)*100)
          if (CurVal <> LastVal)
                    setitem   NREVProgressBar,0,CurVal
                    move      CurVal,LastVal
          endif
          return
NREVInitProgressBar
          move      C0,CurRec
          move      C0,CurVal
          move      C0,LastVal
          return
.end patch 1.3
.................................................................................





OrderSetMouseBusy
        setmode *mcursor=*wait
        return
OrderSetMouseFree
        setmode *mcursor=*arrow
;Patch1.5
                              stop
;patch1.5
        return
Ender
;        setitem PROCStatComment,0,"        Done Printing on laser8!!"
;        setprop Process,visible=c1
;        pause   c5
;        setprop Process,visible=c0
          call  OrderSetMouseFree
        return

        include nrevio.inc
        include ndatio.inc
;Patch1.6
                                        include   compio.inc
                                        include   cntio.inc
.        include nmlrio.inc
;Patch1.6
        include nprjio.inc
        include comlogic.inc
