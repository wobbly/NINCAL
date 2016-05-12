PC         EQU       0
                    include   common.inc
                    include   cons.inc
          include   compdd.inc
          include   cntdd.inc
                    include ndatdd.inc
                    include nrevdd.inc
                    include nprjdd.inc
          INCLUDE   OSLSPERN.INC
Release   INit      "1.1"                 .change from PRoJLR  (lrinc) to PRJAR A/R
Reldate   init      "12 Dec 12"
.Release    Init             "1.0"   DLH 15Aug2007 New
.Reldate   Init      "15Aug2007"
....Lost Biz   rifle thru revenue and find accounts that have no current income or projection that have had in
.previous years  IE this is 2007 if we had activey 2002-06 produce
*Sorted Revenue file
INFILE     FILE
newfile    file
.TmpFIle key = source and ID company or list
WorkFile  Ifile     keylen=7,fixed=167
TmpFIle   File      .used to calc % done
.Source   Dim       1             1-1
.CID      DIm       6           2-7
ZYear     Dim       4           8-11    year run ie 2007
YCRproj   FOrm      10.2         12-24    rental projection         .current year proj
YCEproj   FOrm      10.2       25-37   exch proj
YCrent    FOrm      10.2       37-50   rent ytd
YCExch    FOrm      10.2       51-63   exch ytd
Y1rent    FOrm      10.2       64-76   rent ytd
Y1Exch    FOrm      10.2       77-89   exch ytd
Y2rent    FOrm      10.2       90-102   rent ytd
Y2Exch    FOrm      10.2      103-115   exch ytd
Y3Rent    FOrm      10.2      116-128   rent ytd
Y3Exch    FOrm      10.2      129-141   exch ytd
Y4rent    FOrm      10.2      142-154  rent ytd
Y4Exch    FOrm      10.2      155-167   exch ytd

BIZKEY    DIM       7



.Print file
PRFILE     PFILE

.=====================================================================================
.*********************************************************************************
.************************************************************************************
.Do a search on yearly to find place to change projection value
.change yearly----
YR1       INit      "2011"
YR2       INit      "2012"
.YR1        INIT   "2005"
.YR2        INIT   "2006"
.YR3        INIT   "2007"
.*********************************************************************************
YR0N      Form      4
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
PRTDIR     INIT    "C:\WORK\"                 .Printfile directory ."
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
lr12mos  form  10
lr12mosn form  10
lr12mosg form  10
yr1acth  form  10
yr1ninh  form  10
.
ANIMICON PLFORM    ANIMATE  * CONTAINS ALL THE ICONS
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
        move    "Lost Biz",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        move    Reldate,Wreldate
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
         call getwinver
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
          prepare    WorkFile,"c:\work\LostBIZ.dat","c:\work\Lostbiz.isi","7","167",exclusive
          Prepare   Newfile,"c:\work\lostBiz1.dat",exclusive
*.==========================================================================
          MOVE    "1000000000.00" to sorthlp
.==============================================================================
        CLOCK   TIMESTAMP to str8
        UNPACK  str8,cc,yy,mm,dd
.get current month
.        MOVE    mm to nmm                   
        MOVE    c12 to nmm                  
        Pack      ZYEAR from CC,YY
.============================================================
.set current month on form
        move nmm to str2
        rep     " 0",str2
        setitem NprojEditMonth,0,str2
        setitem NprojEditCopy,0,"1"


***********************************************************
PassONe
          Loop
          call      NREVSeq
          until     over
          MOve      YR0,YR0N
          move      ZYEAR,n4
          calc      N4=(N4-5)        
          if        (YR0N < N4)
          goto      passOne
          endif
          if        (src = "B")
          packkey   compfld,Cid
                    move      "COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
        if not over
                    move      compcomp to client
        endif
          else
          move Cid to ndatfld
          call zfillit using ndatfld
          CALL NDATKEY
                    if not over
                    move OLSTNAME to client
                    endif
          endif
.          move      c0,Holdrent
.          MOve      c0,HOldExch
.          IF        (type = "E")
          
          Packkey   BizKey from Src,Cid
          read      WOrkFIle,Bizkey;str7,str4,Client:
                    YCRproj:  
                    YCEproj:  
                    YCrent:   
                    YCExch:   
                    Y1rent:   
                    Y1Exch:   
                    Y2rent:   
                    Y2Exch:   
                    Y3Rent:   
                    Y3Exch:   
                    Y4rent:   
                    Y4Exch    
          if        over
          Packkey   BizKey from Src,Cid
                    MOve      Zyear,N4
                    if        (YR0N = N4)
                    add       REvAR,YCRent
                    Elseif   (YR0N = N4-1)
                    add       REvAR,Y1Rent
                    Elseif   (YR0N = N4-2)
                    add       REvAR,Y2Rent
                    Elseif   (YR0N = N4-3)
                    add       REvAR,Y3Rent
                    Elseif   (YR0N = N4-4)
                    add       REvAR,Y4Rent
                    endif
          Packkey   BizKey from Src,Cid
          if        (src = "B")
                    move CompComp to client
          else
                    move OLSTNAME to client
          endif
          Write     WOrkFIle,Bizkey;Bizkey,ZYear,Client:
                    YCRproj:  
                    YCEproj:  
                    YCrent:   
                    YCExch:   
                    Y1rent:   
                    Y1Exch:   
                    Y2rent:   
                    Y2Exch:   
                    Y3Rent:   
                    Y3Exch:   
                    Y4rent:   
                    Y4Exch    
          else
                    MOve      Zyear,N4
                    if        (YR0N = N4)
                    add       REvAR,YCRent
                    Elseif   (YR0N = N4-1)
                    add       REvAR,Y1Rent
                    Elseif   (YR0N = N4-2)
                    add       REvAR,Y2Rent
                    Elseif   (YR0N = N4-3)
                    add       REvAR,Y3Rent
                    Elseif   (YR0N = N4-4)
                    add       REvAR,Y4Rent
                    endif
          Update    WOrkFIle;str7,str4,client:
                    YCRproj:  
                    YCEproj:  
                    YCrent:   
                    YCExch:   
                    Y1rent:   
                    Y1Exch:   
                    Y2rent:   
                    Y2Exch:   
                    Y3Rent:   
                    Y3Exch:   
                    Y4rent:   
                    Y4Exch    
          endif



         repeat
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
         branch loopflg to Brok,ListM
.========================================================================================



.At this point it should re-open the work file and use that for input - the current code is left over from revenue reporting





Brok

.Brokerage
         PACK  PRTITLE,"Brokerage"
         PACK  PRTNAME1,"BrokBud.LST"
.         pack   SortFle,ntwkpath6,indat,comma,ntwkpath1,outsrt
         pack   SortFle,"c:\work\LostBIZ.dat,c:\work\LostBIZ.srt"
         pack   taskname,sortfle,";",clintsrt,comma,"s=1=","'","B","2-7"
         sort   taskname
         if over
               alert caution,S$ERROR$,result,"No Sort"
               call OrderSetMouseFree
               return
         endif
*********************************************************************

          call      NREVInitProgressBar
              pack str45 with "c:\work\LostBIZ.srt"
              open          TmpFIle,str45
          positeof      TmpFIle
          fposit    TmpFIle,N10
          calc      howmany=(N10/169)   .'169 = 167(LostBIz record length) + 2 bytes for CR/LF
              close         TmpFIle
*********************************************************************
          OPEN    INFILE,outsrt
        reposit infile,c0
        NPROListview.deleteallitems
          NProListView.SetcolumnText using 1,"2012 AR"
        NProListView.SetcolumnText using 2,"Name"
        NProListView.SetcolumnText using 3,"2011 AR"
        NProListView.SetcolumnText using 4,"2010 AR"
        NProListView.SetcolumnText using 5,"2009 AR"
.        NProListView.SetcolumnText using 5,"YTD Actual"
.        NProListView.SetcolumnText using 6,"2006 NIN"
.        NProListView.SetcolumnText using 7,"2002 actual"
.        NProListView.SetcolumnText using 8,"2002 NIN"
.        NProListView.SetcolumnText using 9,"Year End Pro"
         goto Reader
.====================================================================================
.====================================================================================
ListM
.List Management
                              close  infile
         PACK  PRTITLE,"List Mgmnt"
         PACK  PRTNAME1,"Listbud.LST"
         pack   SortFle,"c:\work\LostBIZ.dat,c:\work\LostBIZ.srt"
         pack   taskname,sortfle,";",clintsrt,comma,"s=1=","'","M","2-7"
         sort   taskname
         if over
               alert caution,S$ERROR$,result,"No Sort"
               call OrderSetMouseFree
               return
         endif
*********************************************************************
          call      NREVInitProgressBar
              pack str45 with "c:\work\LostBIZ.srt"
              open          TmpFIle,str45
          positeof      TmpFIle
          fposit    TmpFIle,N10
          calc      howmany=(N10/169)   .'169 = 167(LostBIz record length) + 2 bytes for CR/LF
              close         TmpFIle
*********************************************************************
          OPEN    INFILE,outsrt
        reposit infile,c0

        NPROListview.deleteallitems
          NProListView.SetcolumnText using 1,"2012 AR"
        NProListView.SetcolumnText using 2,"Name"
        NProListView.SetcolumnText using 3,"2011 AR"
        NProListView.SetcolumnText using 4,"2010 AR"
        NProListView.SetcolumnText using 5,"2009 AR"
.        NProListView.SetcolumnText using 5,"YTD Actual"
.        NProListView.SetcolumnText using 6,"2006 NIN"
.        NProListView.SetcolumnText using 7,"2002 actual"
.        NProListView.SetcolumnText using 8,"2002 NIN"
.        NProListView.SetcolumnText using 9,"Year End Pro"
        goto Reader
.====================================================================================
READER
**************************************
        loop
**************************************
          move     c1 to wrtflg
.        READ     INFILE,SEQ;NREVVARS

          read      Infile;src,Cid,yr0,Client:
                    YCRproj:  
                    YCEproj:  
                    YCrent:   
                    YCExch:   
                    Y1rent:   
                    Y1Exch:   
                    Y2rent:   
                    Y2Exch:   
                    Y3Rent:   
                    Y3Exch:   
                    Y4rent:   
                    Y4Exch    

        goto     Print if over
**************************************
.        CALL   ANIMATEIT
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

NextRead
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
                            move        PRJAR,projlast
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
.                           NProListView.SetItemText giving N8 using n7,YTDPRO,2
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
.                                                            goto  writeflat
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
                            move        PRJAR,projnew
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
                            move        PRJAR,projlast
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
          packkey   compfld,holdmlr
                    move      "COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
        if not over
                    move      compcomp to client
.                    move      mcomp to client
        endif
writeflat
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
                    load      str25 from n2 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
                              osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
                              osls17,osls18,osls19,osls20,osls21,osls22
                              write     newfile,seq;src,cid,compnum,client,lr12mos,lr12mosn,lr12mosg,b1,unbill1,b1,compaddr,compzip,compstate,str25
                                        move      c0 to lr12mos
                                        move      c0 to lr12mosn
                                        move      c0 to yr1ninh
                                        move      c0 to yr1acth
                                        move      c0 to lr12mosn
                                        move      c0 to lr12mosg
.=====================================================================================
.        if        ((NEWTOT = ".00") & (Projvar = ".00"))
.                  move "0" to varbud
.        else
.                move     newtot to CALC1
.                 CALC          n9 =   (CALC1/Projvar)*100
.         move      n9 to varbud
.                 if       (varbud = "")
.                 move "0" to varbud
.                 endif
.        endif
.        call trim using varbud
.        NProListView.SetItemText giving N8 using n7,Varbud,6
.REPLACED WITH NIN FOR 2006 JDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
.
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
          if (osflag = c1 | osflag = C5 | osflag = c6)         .nt 2k xp
                    PRTOPEN   prfile,"\\nins2\laser8",PRTFILE,noprint,spoolfile=PRTFILE1
          elseif (osflag = c3 | osflag =c4)         .win 95 98
                    PRTOPEN   prfile,"Laser8",PRTFILE,noprint,spoolfile=PRTFILE1
          elseif (osflag = "9" )         .win 7
                    PRTOPEN   prfile,"\\NINS2\Laser8",PRTFILE,noprint,spoolfile=PRTFILE1
          else   .(osflag = c0)         .Don't know prompt for printer
                    PRTOPEN   prfile,"",PRTFILE,noprint,spoolfile=PRTFILE1
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
.        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"Projected",*ULOFF,*boldoff;
        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"2006 NIN",*ULOFF,*boldoff;
        PACK str11 with YR1,b1,"LRinc"
        prtpage prfile;*pColumn3:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,str11,*ULOFF,*boldoff;
.        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Budget %",*ULOFF,*boldoff;
        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"2005 NIN",*ULOFF,*boldoff;
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
ACT12005
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
ACT1nin2005
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
.                   NProListView.GetItemText giving str10 using n9,4
.                clear tmpvar1
.                move str10 to tmpvar1
.                add tmpvar1 to NPROYTD
.                if (tmpvar1 = c0)
.                        move tmpvar1 to str14
.                        move c0 to str14
.                           prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.                else
.                        if (tmpvar1 < c0)
.                             call removechar using str10,dash
.                           call FormatNumeric using str10,str13,comma
.                                pack str14,dash,str13
.                                     prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.                        else
.                             call FormatNumeric using str10,tmpvar2,comma
.                                move tmpvar2,str14
.                                     prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.                        endif
.                endif
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
.BRK 2006 actuals
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
.Variance Budget/changed to 2006 NIN jd.

                clear str9
                  NProListView.GetItemText giving STR9 using n9,6
                  prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,str9;
.Variance vs 2000

.                clear str9
.                 NProListView.GetItemText giving str9 using n9,7
.                 prtpage prfile;*pColumn2:row,*font=font8,*ALIGNMENT=*right,*ll,str9;


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
                 prtpage prfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,*ll,"Note: Ranked by 2005 actual booked income.";
.                prtpage prfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,*ll,"Note: Ranked by " 
.                prtpage prfile;YR2;
.                prtpage prfile;" actual booked income.";
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
        if (osflag = "1" or osflag = "5" or osflag = "6")
                    loop
                    until (N3 = COPY)
                              PRTPLAY PRTFILE1,"\\nins2\laser8"
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
          elseif (osflag = c3 | osflag =c4)         .win 95 98

                  loop
                    until (N3 = COPY)
                              PRTPLAY PRTFILE1,"laser8"
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
          elseif (osflag = "9" )         .win 7
                    loop
                    until (N3 = COPY)
                              PRTPLAY PRTFILE1,"\\nins2\laser8"
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
          Else
                    loop
                    until (N3 = COPY)
                              PRTPLAY PRTFILE1,""
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
.;Patch1.5
                              stop
.;patch1.5
Ender
.;        setitem PROCStatComment,0,"        Done Printing on laser8!!"
.;        setprop Process,visible=c1
.;        pause   c5
.;        setprop Process,visible=c0
          call  OrderSetMouseFree
        return

        include nrevio.inc
        include ndatio.inc
.Patch1.6
                                        include   compio.inc
                                        include   cntio.inc
.        include nmlrio.inc
.Patch1.6
        include nprjio.inc
        include comlogic.inc
