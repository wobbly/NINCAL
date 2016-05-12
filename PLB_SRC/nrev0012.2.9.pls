PC         EQU       0
            include common.inc
           include cons.inc
.Patch1.6
          include   compdd.inc
          include   cntdd.inc
.           include nmlrdd.inc
.Patch1.6
           include ndatdd.inc
           include nrevdd.inc
           include nprjdd.inc

Release   INit      "3.0"   DLH  .user logic matching other reports, print to PDF & Email
Reldate   Init      "2014 August 14"
.Release   INit      "2.9"   DLH  .New Year
.Reldate   Init      "2014 January 2"
.Release   INit      "2.8"   DLH  show Management exchange fees
.Reldate   Init      "2013 July 16"
.Release    Init             "2.7"     DLH check for passed date variable
.REldate   Init      "04 May 2011"
.Release    Init             "2.6"     DLH Use AR instead of LR,Yearly changes, Use new company number
.REldate   Init      "25 Jan 2011"

.Release    Init             "2.5"     JD 06/23/2006         LM pass output match Brk pass.
.Release    Init             "2.4"     JD 06/06/2006        Switch columns Brk pass.
.Release    Init             "2.3"     JD 05/22/2006        Switch columns/add new column Brk pass.
.Release    Init             "2.2"    JD 05/17/2006         Calc/print 2005 actuals LM.
.Release    Init             "2.11"    JD 01/20/2006        Yearly Update
.Release    Init             "2.1"    JD 11/30/2005         Added close of input file before next sort.
.Release    Init             "2.0"   DMB 02/04/2005         Added Code to use percentages in projection file.
.Release    Init             "1.9"   DMB 01/13/2005         Yearly Update
.Release    Init             "1.8"   ASH 11/18/2004         Mailer conversion - increase to 6 bytes
.Release    Init             "1.7"   ASH 08/09/2004         Logo conversion
.Release    Init             "1.6"   DMB 05/13/04 Mailer Conversion
.Release    Init             "1.5"   DB 04/13/02  added code to automate job
.RELEASE   INIT      "1.4"            DB 09/30/02  Added code for subroutine getwinver for XP Printing
.RELEASE   INIT      "1.3"           DB 01/14/02  added code for sonic and progress bar
..RELEASE   INIT      "1.2"          DB 01/10/02  added code for new format of projection file
..RELEASE   INIT      "1.1"          DB 12/28/01  added code to reflect new location of revenue file
..RELEASE   INIT      "1.0"          DB 11/14/01  Actuals vs. Projected Brokerage Report for EOM
.===========================================================================
*Sorted Revenue file
INFILE     FILE
.Print file
PRFILE     PFILE
.begin patch 2.6
str2a     Dim       2
.end patch 2.6
.=====================================================================================
.*********************************************************************************
.************************************************************************************
.Do a search on yearly to find place to change projection value
.change yearly----
YR1        INIT      "2013"
YR2        INIT   "2014"
.*********************************************************************************
.*********************************************************************************
TMPVAR1    FORM     13
TMPVAR2    DIM     14
UNBILL     FORM   10
.START PATCH 2.4
YTDACT     FORM   10
.END PATCH 2.4
INCDUE     FORM   10
HIDYR      FORM   10.2
.Patch 2.0 add form vars for calclulation
CAL        FORM   10.2
.CAL        FORM   11
.Patch 2.0
LOADV      FORM   10
TOTREC     FORM   9
.Sort Parameters=======================================================
INDAT      INIT  "revenue.dat"   .File to be sorted
OUTSRT     INIT  "revenue.srt"   .Sorted Output file
.begin patch 2.6
.ClintSrt   INIT  "3-8"                        .Sort by client #
ClintSrt   INIT  "3-12"                        .Sort by client # & Date
.end patch 2.6
SORTFLE    DIM    250                         .Var to pack file names of sort
PRTITLE    DIM    18                          .Title of Printjob
PRTNAME1   DIM    11                          .Name of printfile
PRTDIR     INIT    "C:\WORK\"                 .Printfile directory   ."
PRTFILE1   DIM    19                          .full printfile string
.============================================================================
.NINCA List Management
.begin patch 2.6
.LMLIST       init   "018710"
.end patch 2.6

holdmlr    DIM    6        .held mlr value to check if already in listview object
SORTHLP    FORM   10.2     .add to yr2 total in hidden coulumn to be used as sorted column
.Patch 2.0 add form vars for calclulation
.PROJVAR    FORM   10       .Variable for this year to date projection
.Patch 2.0
PROJVAR     FORM   10.2       .Variable for this year to date projection
.START PATCH 2.3
PROJVAR2    FORM   10.2       .Variable for this EOY projection Broker pass
.END PATCH 2.3
PROJTOT    FORM   10       .Total Projected For Year
OLDTOT     FORM   10       .Var for cumulative total for previous year
NEWTOT     FORM   10       .Var for cumulative total for this year
.Patch 2.2
OLDTOTLM   FORM   10       .Var for cumulative total for previous year LM
.Patch 2.2
.*Flags
MTCHFLG    FORM   1        .flag if client\list a match or not
WRTFLG     FORM   1        .flag to see if it passes one of the conditions
.                                        .if yr2 YTD <> 0
.                                        .Unbilled   <> 0
.                                        .Projytd    <> 0
.                                        .CID        =  0000000
.osflag     form    1       . 1=win 95,98, 2=NT
CHKALL     form    1       .To see if doing both reports
LoopFlg    form    1       .Used for doing multiple passes on reports
.================================================================================================================================================================
.Vars for insertion into listview
Yr1Act     DIM    10             .Previous Year Actuals
YTDPRO     DIM    10             .Year to date projected
.START PATCH 2.3
EOYPRO     DIM    10             .EOY projected
.END PATCH 2.3
YR2ACT     DIM    10             .Year to date actuals
Varbud     DIM    10             .Variance Budget
Vsyr2      DIM    10             .VARBUD vs. yr 2005
UNBILL1    DIM    10             .Unbilled Total
YrEndPro   DIM    10             .Year End Projected
ENDPRO     FORM   10             .Year End Projected Calc Var
TOTDUE     DIM    13             .Total Currently due for year
.====================================================================================
.Patch 2.0
.begin patch 2.6
.projlrnum form 10.2
.ProjlrmoR FORM      13
.ProjlrmoE FORM      13
.Projper             DIM       3
.Projpernum          form      3
.ProjLRJan1     form  10.2
.ProjLRFEB1     form  10.2
.ProjLRmar1     form  10.2
.ProjLRapr1     form  10.2
.ProjLRmay1     form  10.2
.ProjLRjun1     form  10.2
.ProjLRjul1     form  10.2
.ProjLRaug1     form  10.2
.ProjLRsep1     form  10.2
.ProjLRoct1     form  10.2
.ProjLRnov1     form  10.2
.ProjLRdec1     form  10.2
.ProjLrTot1                    form 10.2
.ProjLRJan2    form  10.2
.ProjLRFEB2    form  10.2
.ProjLRmar2    form  10.2
.ProjLRAPR2    form  10.2
.ProjLRMAY2    form  10.2
.ProjLRJUN2    form  10.2
.ProjLRJUL2    form  10.2
.ProjLRAUG2    form  10.2
.ProjLRSEP2    form  10.2
.ProjLROCT2    form  10.2
.ProjLRNOV2    form  10.2
.ProjLRDEC2    form  10.2
.
.ProjLRJanTot    form  10.2
.ProjLRFEBTot    form  10.2
.ProjLRmarTot    form  10.2
.ProjLRAPRTot    form  10.2
.ProjLRMAYTot    form  10.2
.ProjLRJUNTot    form  10.2
.ProjLRJULTot    form  10.2
.ProjLRAUGTot    form  10.2
.ProjLRSEPTot    form  10.2
.ProjLROCTTot    form  10.2
.ProjLRNOVTot    form  10.2
.ProjLRDECTot    form  10.2
..
PrjARnum           form 10.2
PrjARmoR           FORM      13
PrjARmoE           FORM      13
Projper             DIM       3
Projpernum          form      3
PrjARJan1          form  10.2
PrjARFEB1          form  10.2
PrjARmar1          form  10.2
PrjARapr1          form  10.2
PrjARmay1          form  10.2
PrjARjun1          form  10.2
PrjARjul1          form  10.2
PrjARaug1          form  10.2
PrjARsep1          form  10.2
PrjARoct1          form  10.2
PrjARnov1          form  10.2
PrjARdec1          form  10.2
PrjARTot1          form 10.2
PrjARJan2          form  10.2
PrjARFEB2          form  10.2
PrjARmar2          form  10.2
PrjARAPR2          form  10.2
PrjARMAY2          form  10.2
PrjARJUN2          form  10.2
PrjARJUL2          form  10.2
PrjARAUG2          form  10.2
PrjARSEP2          form  10.2
PrjAROCT2          form  10.2
PrjARNOV2          form  10.2
PrjARDEC2          form  10.2
          
PrjARJanTot        form  10.2
PrjARFEBTot        form  10.2
PrjARmarTot        form  10.2
PrjARAPRTot        form  10.2
PrjARMAYTot        form  10.2
PrjARJUNTot        form  10.2
PrjARJULTot        form  10.2
PrjARAUGTot        form  10.2
PrjARSEPTot        form  10.2
PrjAROCTTot        form  10.2
PrjARNOVTot        form  10.2
PrjARDECTot        form  10.2
          
.end patch 2.6      
.Patch 2.0
....................................................................
.hold vars for projection
projnew   form      11          hold current projection
projlast  form      11          hold Previous Year projection
.......................................................................
.TOTALS
.Previous Year
NYR1TOT     FORM    10
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
.START PATCH 2.3
NEOYPROJ    FORM    10
.END PATCH 2.3
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
        move    "NREV0012.PLS",Wprognme
        move    "Actuals vs. Projected",Wfunction
        move    "David Baca",Wauthor
.begin patch 2.6
.        move    "1.0",Wrelease
.        move    "Dec 2001",Wreldate
        move    Release,Wrelease
        move    Reldate,Wreldate
.end patch 2.6
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
          if        (program = "NREV0012")
          uNPACK    Today into mm,str1,dd,str1,yy
          move      "20",cc
          PACK      str8,cc,yy,mm,dd
          else
          CLOCK   TIMESTAMP to str8
          UNPACK  str8,cc,yy,mm,dd
          endif
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
.        if (str1 = "3" or str1 = "4")
.                 move     c1 to osflag
.        endif
.        if (str1 = "1" or str1 = "5")
.         move     c2 to osflag
.        endif
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
.get current month
        MOVE    mm to nmm
.============================================================
.set current month on form
        move nmm to str2
        rep     " 0",str2
        setitem NprojEditMonth,0,str2
        setitem NprojEditCopy,0,"1"


***********************************************************
.patch1.5
.         loop
.                   waitevent
.         repeat
.patch1.5
************************************************************


Start
        call    OrderSetMouseBusy
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
         branch loopflg to Brok,ListM
         move   c1 to chkall
         clear  loopflg
Restart
.====================================
.Clear var 1/10/02
           move c0,PROJVAR
.START PATCH 2.3
           move c0,PROJVAR2
.END PATCH 2.3       
           move c0,PROJTOT
           move c0,OLDTOT
.START PATCH 2.2
           move c0,OLDTOTLM
.END PATCH 2.2
           move c0,NEWTOT
.===================================
         move c0,pgcnt
         move c0,NYR1TOT
         move c0,NPROYTD
         move c0,NYR2TOT
         move c0,NUNBILL
         move c0,NENDPTOT
         move c0,NTOTDUE
         move c0,CALC1
         move c0,NDIFTOT
         move c0,NPROTOT
         move c0,YTDDIFF
         move c0,NEOYPROJ
.====================================
           add    c1 to loopflg
         if     (loopflg > c2)
                call OrderSetMouseFree
                return
         endif
         branch loopflg to Brok,ListM
.========================================================================================
Brok
.Brokerage
.          return
          PACK  PRTITLE,"Brokerage"
         PACK  PRTNAME1,"BrokBud.LST"
         pack   SortFle,ntwkpath6,indat,comma,ntwkpath1,outsrt
           pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","B","'","&9=","'",YR1,"'","|","2=","'","B","'","&9=","'",YR2,"'"
         sort   taskname,sundm="NINS1:502"
         if over
               alert caution,S$ERROR$,result,"No Sort"
               call OrderSetMouseFree
               return
         endif
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
          calc      howmany=(N10/560)   .'560 = 558(NINMDLST record length) + 2 bytes for CR/LF
              close         tempfile
*********************************************************************
.          OPEN    INFILE,outsrt
          OPEN    INFILE,str45
        reposit infile,c0
        NPROListview.deleteallitems
          NProListView.SetcolumnText using 1,"Total Due"
        NProListView.SetcolumnText using 2,"Name"
.        NProListView.SetcolumnText using 3,"2004 Actual"
        NProListView.SetcolumnText using 3,"2012 Actual"
        NProListView.SetcolumnText using 4,"YTD Projected"
        NProListView.SetcolumnText using 5,"YTD Actual"
        NProListView.SetcolumnText using 6,"Variance Budget"
.        NProListView.SetcolumnText using 7,"Budget Vs. 2005"
        NProListView.SetcolumnText using 7,"Budget Vs. 2013"
        NProListView.SetcolumnText using 8,"Unbilled"
        NProListView.SetcolumnText using 9,"Year End Pro"
         goto Reader
.====================================================================================
.====================================================================================
ListM
.List Management
.patch2.1
                              close infile
.patch2.1
         PACK  PRTITLE,"List Mgmnt"
         PACK  PRTNAME1,"Listbud.LST"
         pack   SortFle,ntwkpath6,indat,comma,ntwkpath1,outsrt
.         pack   SortFle,indat,comma,outsrt
.          pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","M","'","&9=","'",YR2,"'"
                      clear   taskname
        pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","M","'","&9=","'",YR1,"'","|","2=","'","M","'","&9=","'",YR2,"'"
         sort   taskname,sundm="NINS1:502"
         if over
               alert caution,S$ERROR$,result,"No Sort"
               call OrderSetMouseFree
               return
         endif
*********************************************************************
          call      NREVInitProgressBar
              pack str45 with outsrt,"|NINS1:502"
              open          tempfile,str45
          positeof      tempfile
          fposit    tempfile,N10
          calc      howmany=(N10/560)   .'560 = 558(NINMDLST record length) + 2 bytes for CR/LF
              close         tempfile
*********************************************************************
.          OPEN    INFILE,outsrt
          OPEN    INFILE,str45
        reposit infile,c0

        NPROListview.deleteallitems
.         NProListView.SetcolumnText using 1," 2004 Actuals"
.begin patch 2.9
.          NProListView.SetcolumnText using 1," 2012 Acutals"
          NProListView.SetcolumnText using 1," Current Actuals"
.end patch 2.9
        NProListView.SetcolumnText using 2,"YTD Projection"
        NProListView.SetcolumnText using 3,"YTD Difference"
.        NProListView.SetcolumnText using 4,"2004 Projected"
.begin patch 2.9
.        NProListView.SetcolumnText using 4,"2013 Projected"
        NProListView.SetcolumnText using 4,"Total Projected"
.end patch 2.9
        NProListView.SetcolumnText using 5,"Unbilled"
        NProListView.SetcolumnText using 6,"Client"
        NProListView.SetcolumnText using 7,"Client ID"
.        NProListView.SetcolumnText using 8,""
.Patch 2.11
.begin patch 2.9
.        NProListView.SetcolumnText using 8,"2013 actuals"
        NProListView.SetcolumnText using 8,"Past actuals"
.end patch 2.9
.Patch 2.11
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
	if	over
.	call	debug
	goto	PRint
	endif
.        goto     Print if over
**************************************
          CALL   NREVUpdateProgressBar
....debug
          If        (cid = "009221" or Cid = "015407" or Cid = "013422")
.          call      debug
          endif

**************************************
.=======================================================================================
.Clear Vars for next year
        clear   incdue
        clear   ytdact
        clear   totdue
        clear   str13
        clear   yr1act
        clear   ytdpro
.START PATCH 2.3
        clear   EOYpro
.END PATCH 2.3
        clear   yr2act
        clear   mtchflg
        clear   Projvar
.START PATCH 2.3
        clear   projvar2
.END PATCH  2.3
        clear   OLDTOT
        clear   NEWTOT
        clear   projtot
.        clear   projvar
        clear   unbill
        clear   loadv
.Patch 2.0
.Begin Patch 2.6
.clear projLRJan1
.clear projLRJan2
.clear ProjLRJanTot
.clear projLRFeb1
.clear projLRFeb2
.clear ProjLRFebTot
.clear projLRMar1
.clear projLRMar2
.clear ProjLRMarTot
.clear projLRApr1
.clear projLRApr2
.clear ProjLRAprTot
.clear projLRMay1
.clear projLRMay2
.clear ProjLRMayTot
.clear projLRJun1
.clear projLRJun2
.clear ProjLRJunTot
.clear projLRJul1
.clear projLRJul2
.clear ProjLRJulTot
.clear projLRAug1
.clear projLRAug2
.clear ProjLRAugTot
.clear projLRSep1
.clear projLRSep2
.clear ProjLRSepTot
.clear projLROct1
.clear projLROct2
.clear ProjLROctTot
.clear projLRNov1
.clear projLRNov2
.clear ProjLRNovTot
.clear projLRDec1
.clear projLRDec2
.clear ProjLRDecTot
.Clear ProjlrmoR
.clear ProjlrmoE
.clear Projper
.
          clear    PrjARJan1
          clear    PrjARJan2
          clear    PrjARJanTot
          clear    PrjARFeb1
          clear    PrjARFeb2
          clear    PrjARFebTot
          clear    PrjARMar1
          clear    PrjARMar2
          clear    PrjARMarTot
          clear    PrjARApr1
          clear    PrjARApr2
          clear    PrjARAprTot
          clear    PrjARMay1
          clear    PrjARMay2
          clear    PrjARMayTot
          clear    PrjARJun1
          clear    PrjARJun2
          clear    PrjARJunTot
          clear    PrjARJul1
          clear    PrjARJul2
          clear    PrjARJulTot
          clear    PrjARAug1
          clear    PrjARAug2
          clear    PrjARAugTot
          clear    PrjARSep1
          clear    PrjARSep2
          clear    PrjARSepTot
          clear    PrjAROct1
          clear    PrjAROct2
          clear    PrjAROctTot
          clear    PrjARNov1
          clear    PrjARNov2
          clear    PrjARNovTot
          clear    PrjARDec1
          clear    PrjARDec2
          clear    PrjARDecTot
          Clear    PrjARmoR
          clear    PrjARmoE
          clear    Projper

.end Patch 2.6

.Patch 2.0
.========================================================================================
        if (loopflg = c1)			.brokerage pass?
          if (CID = holdmlr)                      . yes - check to see if mlr\lst already processed
                    goto reader
          endif
        endif
.========================================================================================
        move    CID to holdmlr
.Check to see if Clients meet criteria for Report
        branch  loopflg to Brkchk,LMCHK
.Brokerage pass
Brkchk
        cmatch  SRC to "B"
        goto    READER if not equal
        Call      ClearPrjVars
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
.Management pass
LMCHK
        cmatch  SRC to "M"
        goto    READER if not equal
.begin patch 2.6
          REset     RUNCODES
          scan      Cid,runcodes
          goto      reader if equal
.go ahead and print LM exchange fee
.          REset     EXFEELST
.          scan      Cid,EXFEELST
.          goto      reader if equal
.go ahead and print LM exchange fee
          Call      ClearPrjVars
temporary
.        if      (CID = LMLIST)		.why different on LM pass?								
.                goto reader
.        endif
.end patch 2.6
        if      (YR0 = YR2)
                 pack nrevfld,type,src,cid,yr0
                     goto Yr2Start
        endif

        if      (YR0 = YR1)
.===================================================================================
.Check to see if this should include all records for last year or just ones matching.. >>>2014 DH what does this mean should include both years
.The current years clients
.Patch 2.2
                  if  (SRC = "M")
                move c0,oldtotlm
                for n2,"1",nmm
.begin patch 2.6
.                LOAD LOADV using n2 from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
.                SEPLR,OCTLR,NOVLR,DECLR
                    LOAD      LOADV using n2 from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                              SEPAR,OCTAR,NOVAR,DECAR
.end patch 2.6
.                add LOADV to nyr1tot
                add LOADV to oldtotlm
                repeat
                goto LMPREV
                 endif
             goto   reader
.Patch 2.2
        endif
        goto reader
.==================================================================================
ADDOLD
                add REVunbld to unbill
.===================================================================================
                for n2,"1",nmm
.begin patch 2.6
.                LOAD LOADV using n2 from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
.                SEPLR,OCTLR,NOVLR,DECLR
                    LOAD      LOADV using n2 from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                              SEPAR,OCTAR,NOVAR,DECAR
.end patch 2.6
                add LOADV to OLDTOT
                repeat
.===================================================================================
.if second pass of previous year
                if (mtchflg = c1)
                        move c0 to mtchflg
                    if (OLDTOT = C0)
                       
                        else
                                move    c1 to wrtflg    .if total for prev year is not zero set flag to be written out
                        endif
                        move    oldtot to yr1act
                          unpack  nrevfld,type,src,cid,yr0
                              move      yr2 to yr0       .Moving to next year
                          pack    nrevfld,type,src,cid,yr0
                        clear   newtot
                              goto nextread
                endif
.Prepare for second pass
                unpack  nrevfld,type,src,cid,yr0
                if (type = "R")
                        move "E" to type
                else
                        move "R" to type
                endif
                pack    nrevfld,type,src,cid,yr0
.Read Revenue file
                call    NREVKEY
                if not over
                        move c1 to mtchflg
                        goto ADDOLD
                else
                        move c0 to mtchflg
                    if (OLDTOT = C0)
                        else
                                move c1 to wrtflg       .if total for prev year is not zero set write flag
                        endif
                                move    oldtot to yr1act
                                    unpack  nrevfld,type,src,cid,yr0
                              move      yr2 to yr0
                                    pack    nrevfld,type,src,cid,yr0
                              goto    nextread
                endif

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
                add REVunbld to unbill
                for n2,"1",nmm
.begin patch 2.6
.                LOAD LOADV using n2 from JANLR,FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR:
.                SEPLR,OCTLR,NOVLR,DECLR
                    LOAD      LOADV using n2 from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                              SEPAR,OCTAR,NOVAR,DECAR
.end patch 2.6
                    add LOADV to NEWTOT
                repeat
                move newtot to yr2act
.===================================================================================
.Patch 2.2
LMPREV
.Patch 2.2
                if (loopflg = c2)
.Begin patch 3.0                    if (NEWTOT > C0)
                    if (NEWTOT <> C0)                      .if this is not here we get double records for entries with projections but no current actuals
.end patch 3.0
                            unpack  nrevfld,type,src,cid,yr0
.                 pack    Projfld with TYPE,SRC,CID
.                           READ    PROFILE,PROJFLD;PROJVARS
.                        add     proj2001 to PROJTOT
**********************************************************************************
*Code for new projection file
                    move      c0,projlast
                    if (src = "M")
....debug
          If        (cid = "009221" or Cid = "015407" or Cid = "013422")
          call      debug
          endif

                              move      " M",str2
                              move      C0,N2
.search for the most current projection for the desired year
                              loop
                                        add       C1,N2
                              move      N2,str2a
                              rep       zfill,str2a
                              pack      NPRJFLD,str2,CID,YR0,str2a
.                             pack      NPRJFLD,str2,str6,PrevYear,str3
.                             move      "detout,3-NPRJKEY",Location
.                             pack      KeyLocation,"Key: ",NPRJFLD
.begin patch 2.6  this logic seems to depend on variable NOT being cleared on read
.                              call      NPRJKEY
                              call      NPRJtst
                              until over
                              call      NPRJKEY
.end patch 2.6  this logic seems to depend on variable NOT being cleared on read
.begin patch 2.6
.                            move        ProjLR,projlast
                            move        PrjAR,projlast
.                           move        ProjNIN,ninlast
                              until (PrjMast = YES)
.end patch 2.6
                    repeat
                add     projlast,PROJTOT
              
          endif
*********************************************************************************
.Patch 2.0 Comment Out
.                        move    projtot to calc1
.                   CALC      cal = (calc1/12)*nmm
.                             move    cal to Projvar
.                        move    Projvar to YTDPRO
.Patch 2.0 Comment Out
.Patch 2.0
.begin patch 2.6
.         if (Projlr <> 0)
         if (PrjAR <> 0)
.          call      debug
.end patch 2.6
            clear projpernum
            for n4,"1","12"
.begin patch 2.6
.                  load projper using n4,projLRJan,projLRFeb,projLRMar,projLRApr,projLRMay,projLRJun,projLRJul,projLRAug,projLRSep,projLROct,ProjLrNov,ProjLrDec
                  load projper using n4,PrjARJan,PrjARFeb,PrjARMar,PrjARApr,PrjARMay,PrjARJun,PrjARJul,PrjARAug,PrjARSep,PrjAROct,PrjARNov,PrjARDec
                    call trim using projper
.end patch 2.6
                  move projper to n3
                  add n3 to projpernum
            repeat
            if (projpernum = c0)
.begin patch 2.6
.               move projlr to calc1
               move prjAR to calc1
               CALC    cal = (calc1/12.00)
               for n4,"1",nmm
.                 store  cal   using n4,projLRJan1,projLRFeb1,projLRMar1,projLRApr1,projLRMay1,projLRJun1,projLRJul1,projLRAug1,projLRSep1,projLROct1,ProjLrNov1,ProjLrDec1
                 store  cal   using n4,PrjARJan1,PrjARFeb1,PrjARMar1,PrjARApr1,PrjARMay1,PrjARJun1,PrjARJul1,PrjARAug1,PrjARSep1,PrjAROct1,PrjARNov1,PrjARDec1
.end patch 2.6
               repeat
            else
               for n4,"1",nmm
.begin patch 2.6
.                  load projper using n4,projLRJan,projLRFeb,projLRMar,projLRApr,projLRMay,projLRJun,projLRJul,projLRAug,projLRSep,projLROct,ProjLrNov,ProjLrDec
                  load projper using n4,PrjARJan,PrjARFeb,PrjARMar,PrjARApr,PrjARMay,PrjARJun,PrjARJul,PrjARAug,PrjARSep,PrjAROct,PrjARNov,PrjARDec
                    call trim using projper
.                     move projper to n32
.                     move projlr to projlrnum
.                     CALC    cal = (Projlrnum*(n32/100.00))
.                     add cal to ProjlrmoR
                     move projper to n32
                     move prjAR to PrjARnum
                     CALC    cal = (PrjARnum*(n32/100.00))
                     add cal to PrjARmoR
.                 store  cal   using n4,projLRJan1,projLRFeb1,projLRMar1,projLRApr1,projLRMay1,projLRJun1,projLRJul1,projLRAug1,projLRSep1,projLROct1,ProjLrNov1,ProjLrDec1
                 store  cal   using n4,PrjARJan1,PrjARFeb1,PrjARMar1,PrjARApr1,PrjARMay1,PrjARJun1,PrjARJul1,PrjARAug1,PrjARSep1,PrjAROct1,PrjARNov1,PrjARDec1
.end patch 2.6
               repeat
            endif
       endif
.begin patch 2.6
.       add projLRJan1,ProjVar
.       add projLRFeb1,ProjVar
.       add projLRMar1,ProjVar
.       add projLRApr1,ProjVar
.       add projLRMay1,ProjVar
.       add projLRJun1,ProjVar
.       add projLRJul1,ProjVar
.       add projLRAug1,ProjVar
.       add projLRSep1,ProjVar
.       add projLROct1,ProjVar
.       add projLRNov1,ProjVar
.       add projLRDec1,ProjVar
          Calc      ProjVar = (PrjARJan1+PrjARFeb1+PrjARMar1+PrjARApr1+PrjARMay1+PrjARJun1+PrjARJul1+PrjARAug1+PrjARSep1+PrjAROct1+PrjARNov1+PrjARDec1)
.end patch 2.6
       move projvar to ytdpro
.Patch 2.0

.YTD Actuals - List management pass.
.START PATCH 2.5
        add unbill to newtot,INCDUE
.begin patch 2.6
.          add projlr,Projvar2
          add prjAR,Projvar2
.end patch 2.6

                            clear hidyr
.Creating "hidden" total for sorting purposes
                    add  sorthlp,newtot,hidyr
                            move hidyr to str13
                            NProListView.InsertItem giving N7 using str13
                            NProListView.SetItemText giving N8 using n7,yr2act,1
.YTD Projection
                            NProListView.SetItemText giving N8 using n7,YTDPRO,2
        if        ((NEWTOT = ".00") & (Projvar = ".00"))
                  move "0" to varbud
        else
                move     newtot to CALC1
                  CALC          n9 =   (CALC1/Projvar)*100
          move      n9 to varbud
                  if       (varbud = "")
                  move "0" to varbud
                  endif
        endif
        call trim using varbud
        NProListView.SetItemText giving N8 using n7,Varbud,3

        if        ((NEWTOT = ".00") & (OLDTOTLM = ".00"))
                  move "0" to vsyr2
        else
                move     newtot to CALC1
          CALC        n9 =   (CALC1/OLDTOTLM)*100
                  move      n9 to vsyr2
          if       (vsyr2 = "")
                    move "0" to vsyr2
                  endif
        endif
        call trim using VSYR2
        NProListView.SetItemText giving N8 using n7,VSYR2,7
.YTD Differential
.                           .clear str10
.                           sub  PROJVAR,NEWTOT,YTDDIFF
.                           move YTDDIFF to str10
.                           NProListView.SetItemText giving N8 using n7,str10,3
.End PATCH 2.5
.Total Projection for Year
                            clear str10
                            move projtot to str10
                            NProListView.SetItemText giving N8 using n7,str10,4
.REVunbld
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
.                        move holdmlr to str6
.                        NProListView.SetItemText giving N8 using n7,str6,7
.Patch 2.2
                        clear str10
                        move oldtotlm,str10
                            NProListView.SetItemText giving N8 using n7,str10,8
.                           move c0,oldtotlm
.Patch 2.2
.START PATCH 2.5
       sub incdue from projvar2
        move PROJvar2 to YRENDPRO
        NProListView.SetItemText giving N8 using n7,YrEndPro,9
.End PATCH 2.5
.Begin patch 3.0
			endif
.end patch 3.0
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
.*Code for new projection file
          move      c0,projlast
.         move      c0,ninlast
          move      c0,projnew
.         move      c0,ninnew
          if (src = "B")
.START PATCH 1.8 ADDED LOGIC
.begin patch 2.6
.                    unpack    CID,str2,str4
.                    pack      COMPFLD3,str4
                    Packkey   Compfld,Cid
.                    move      "COMPKEY3",Location
.                    pack      KeyLocation,"Key: ",COMPFLD3
.                    call      COMPKEY3
                    move      "COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
.end patch 2.6
.END PATCH 1.8 ADDED LOGIC
                    move      "EB",str2
                    move      C0,N2
                    loop
                              add       C1,N2
                              move      N2,str2a
                              rep       zfill,str2a
.START PATCH 1.8 ADDED LOGIC
.                             pack      NPRJFLD,str2,cid,YR0,str3
                              pack      NPRJFLD,str2,COMPNUM,YR0,str2a
.END PATCH 1.8 ADDED LOGIC
.                             pack      NPRJFLD,str2,str6,PrevYear,str3
                              rep       zfill,NPRJFLD
.                             move      "detout-NPRJKEY",Location
.                             pack      KeyLocation,"Key: ",NPRJFLD
.begin patch 2.6  this logic seems to depend on variable NOT being cleared on read
.                              call      NPRJKEY
                              call      NPRJtst
                              until over
                              call      NPRJKEY
.end patch 2.6  this logic seems to depend on variable NOT being cleared on read
.begin patch 2.6
.                            move        ProjLR,projnew
                            move        PrjAR,projnew
.                           move        ProjNIN,ninnew
                              until (PrjMast = YES)
.end patch 2.6
                    repeat
.Patch 2.0
.begin patch 2.6
.                                        if (Projlr <> 0)
                                        if (PrjAr <> 0)
                                                  clear projpernum
                                                  for n4,"1","12"
.                                                                      load projper using n4,projLRJan,projLRFeb,projLRMar,projLRApr,projLRMay,projLRJun,projLRJul,projLRAug,projLRSep,projLROct,ProjLrNov,ProjLrDec
                                                                      load projper using n4,PrjARJan,PrjARFeb,PrjARMar,PrjARApr,PrjARMay,PrjARJun,PrjARJul,PrjARAug,PrjARSep,PrjAROct,PrjARNov,PrjARDec
                                                                      call trim using projper
.end patch 2.6
                                                                      move projper to n3
                                                                      add n3 to projpernum
                                                  repeat
                                                  if (projpernum = c0)
.                                                            move projlr to calc1
                                                            move prjAR to calc1
                                                            CALC    cal = (calc1/12.00)
                                                            for n4,"1",nmm
.                                                              store   cal       using n4,projLRJan1,projLRFeb1,projLRMar1,projLRApr1,projLRMay1,projLRJun1,projLRJul1,projLRAug1,projLRSep1,projLROct1,ProjLrNov1,ProjLrDec1
                                                              store   cal       using n4,PrjARJan1,PrjARFeb1,PrjARMar1,PrjARApr1,PrjARMay1,PrjARJun1,PrjARJul1,PrjARAug1,PrjARSep1,PrjAROct1,PrjARNov1,PrjARDec1
                                                            repeat
                                                  else
                                                            for n4,"1",nmm
.begin patch 2.6
.                                                                                load projper using n4,projLRJan,projLRFeb,projLRMar,projLRApr,projLRMay,projLRJun,projLRJul,projLRAug,projLRSep,projLROct,ProjLrNov,ProjLrDec
                                                                                load projper using n4,PrjARJan,PrjARFeb,PrjARMar,PrjARApr,PrjARMay,PrjARJun,PrjARJul,PrjARAug,PrjARSep,PrjAROct,PrjARNov,PrjARDec
                                                                                call trim using projper
.end patch 2.6
                                                                                move projper to n32
.begin patch 2.6
.                                                                                move projlr to projlrnum
.                                                                                CALC    cal = (Projlrnum*(n32/100.00))
.                                                                                add cal to ProjlrmoE    .exchange
.                                                                                store cal using n4,projLRJan1,projLRFeb1,projLRMar1,projLRApr1,projLRMay1,projLRJun1,projLRJul1,projLRAug1,projLRSep1,projLROct1,ProjLrNov1,ProjLrDec1
                                                                                move prjAR to PrjARnum
                                                                                CALC    cal = (PrjARnum*(n32/100.00))
                                                                                add cal to PrjARmoE    .exchange
                                                                                store cal using n4,PrjARJan1,PrjARFeb1,PrjARMar1,PrjARApr1,PrjARMay1,PrjARJun1,PrjARJul1,PrjARAug1,PrjARSep1,PrjAROct1,PrjARNov1,PrjARDec1
.end patch 2.6
                                                            repeat
                                                  endif
                                        endif
.Patch 2.0
.START PATCH 2.3
.begin patch 2.6
.                                                            add  projlr,projvar2
                                                            add  prjAr,projvar2
.end patch 2.6
.END PATCH  2.3
                    move      "RB",str2
                    move      C0,N2
                    loop
                              add       C1,N2
                              move      N2,str2a
                              rep       zfill,str2a
.START PATCH 1.8 ADDED LOGIC
.                             pack      NPRJFLD,str2,cid,YR0,str3
                              pack      NPRJFLD,str2,COMPNUM,YR0,str2a
.END PATCH 1.8 ADDED LOGIC
.                             pack      NPRJFLD,str2,str6,PrevYear,str3
                              rep       zfill,NPRJFLD
.                             move      "detout,2-NPRJKEY",Location
.                             pack      KeyLocation,"Key: ",NPRJFLD
.begin patch 2.6  this logic seems to depend on variable NOT being cleared on read
.                              call      NPRJKEY
                              call      NPRJtst
                              until over
                              call      NPRJKEY
.end patch 2.6  this logic seems to depend on variable NOT being cleared on read
.begin patch 2.6
.                            move        ProjLR,projlast
                            move        PrjAR,projlast
.end patch 2.6
.                           move        ProjNIN,ninlast
                              until (PrjMast = YES)
                    repeat
.Patch 2.0
.begin patch 2.6
.                                        if (Projlr <> 0)
                                        if (PrjAR <> 0)
.end patch 2.6
                                                  clear projpernum
                                                  for n4,"1","12"
.begin patch 2.6
.                                                                      load projper using n4,projLRJan,projLRFeb,projLRMar,projLRApr,projLRMay,projLRJun,projLRJul,projLRAug,projLRSep,projLROct,ProjLrNov,ProjLrDec
                                                                      load projper using n4,PrjARJan,PrjARFeb,PrjARMar,PrjARApr,PrjARMay,PrjARJun,PrjARJul,PrjARAug,PrjARSep,PrjAROct,PrjARNov,PrjARDec
                                                                      call trim using projper
.end patch 2.6
                                                                      move projper to n3
                                                                      add n3 to projpernum
                                                  repeat
                                                  if (projpernum = c0)
.begin patch 2.6
.                                                            move projlr to calc1
                                                            move prjAr to calc1
                                                            CALC    cal = (calc1/12.00)
                                                            for n4,"1",nmm
.                                                              store   cal       using n4,projLRJan2,projLRFeb2,projLRMar2,projLRApr2,projLRMay2,projLRJun2,projLRJul2,projLRAug2,projLRSep2,projLROct2,ProjLrNov2,ProjLrDec2
                                                              store   cal       using n4,PrjARJan2,PrjARFeb2,PrjARMar2,PrjARApr2,PrjARMay2,PrjARJun2,PrjARJul2,PrjARAug2,PrjARSep2,PrjAROct2,PrjARNov2,PrjARDec2
                                                            repeat
                                                  else
                                                            for n4,"1",nmm
.                                                                                load projper using n4,projLRJan,projLRFeb,projLRMar,projLRApr,projLRMay,projLRJun,projLRJul,projLRAug,projLRSep,projLROct,ProjLrNov,ProjLrDec
                                                                                load projper using n4,PrjARJan,PrjARFeb,PrjARMar,PrjARApr,PrjARMay,PrjARJun,PrjARJul,PrjARAug,PrjARSep,PrjAROct,PrjARNov,PrjARDec
                                                                                call trim using projper
                                                                                move projper to n32
.                                                                                move projlr to projlrnum
.                                                                                CALC    cal = (Projlrnum*(n32/100.00))
.                                                                                add cal to ProjlrmoR    .rent
.                                                                                store cal using n4,projLRJan2,projLRFeb2,projLRMar2,projLRApr2,projLRMay2,projLRJun2,projLRJul2,projLRAug2,projLRSep2,projLROct2,ProjLrNov2,ProjLrDec2
                                                                                move prjAR to PrjARnum
                                                                                CALC    cal = (PrjARnum*(n32/100.00))
                                                                                add cal to PrjARmoR    .rent
                                                                                store cal using n4,PrjARJan2,PrjARFeb2,PrjARMar2,PrjARApr2,PrjARMay2,PrjARJun2,PrjARJul2,PrjARAug2,PrjARSep2,PrjAROct2,PrjARNov2,PrjARDec2
.end patch 2.6
                                                            repeat
                                                  endif
.;;;
.begin patch 2.6
.                                                  add projLRJan1,projLRJan2,ProjLRJanTot
.                                                  add projLRFeb1,projLRFeb2,ProjLRFebTot
.                                                  add projLRMar1,projLRMar2,ProjLRMarTot
.                                                  add projLRApr1,projLRApr2,ProjLRAprTot
.                                                  add projLRMay1,projLRMay2,ProjLRMayTot
.                                                  add projLRJun1,projLRJun2,ProjLRJunTot
.                                                  add projLRJul1,projLRJul2,ProjLRJulTot
.                                                  add projLRAug1,projLRAug2,ProjLRAugTot
.                                                  add projLRSep1,projLRSep2,ProjLRSepTot
.                                                  add projLROct1,projLROct2,ProjLROctTot
.                                                  add projLRNov1,projLRNov2,ProjLRNovTot
.                                                  add projLRDec1,projLRDec2,ProjLRDecTot
.                                                  add ProjLRJanTot,Projvar
.                                                  add ProjLRFebTot,Projvar
.                                                  add ProjLRMarTot,Projvar
.                                                  add ProjLRAprTot,Projvar
.                                                  add ProjLRMayTot,Projvar
.                                                  add ProjLRJunTot,Projvar
.                                              add ProjLRJulTot,Projvar
.                                              add ProjLRAugTot,Projvar
.                                                add ProjLRSepTot,Projvar
.                                                add ProjLROctTot,Projvar
.                                              add ProjLRNovTot,Projvar
.                                              add ProjLRDecTot,Projvar
..                                                 move projvar to ytdpro
..START PATCH 2.3
.                                                  add projlr,Projvar2

                                                  add PrjARJan1,PrjARJan2,PrjARJanTot
                                                  add PrjARFeb1,PrjARFeb2,PrjARFebTot
                                                  add PrjARMar1,PrjARMar2,PrjARMarTot
                                                  add PrjARApr1,PrjARApr2,PrjARAprTot
                                                  add PrjARMay1,PrjARMay2,PrjARMayTot
                                                  add PrjARJun1,PrjARJun2,PrjARJunTot
                                                  add PrjARJul1,PrjARJul2,PrjARJulTot
                                                  add PrjARAug1,PrjARAug2,PrjARAugTot
                                                  add PrjARSep1,PrjARSep2,PrjARSepTot
                                                  add PrjAROct1,PrjAROct2,PrjAROctTot
                                                  add PrjARNov1,PrjARNov2,PrjARNovTot
                                                  add PrjARDec1,PrjARDec2,PrjARDecTot
                                                  Calc      PRojvar= (PrjARJanTot+PrjARFebTot+PrjARMarTot+PrjARAprTot+PrjARMayTot+PrjARJunTot+PrjARJulTot+PrjARAugTot+PrjARSepTot+PrjAROctTot+PrjARNovTot+PrjARDecTot)
.                                                  add PrjARJanTot,Projvar
.                                                  add PrjARFebTot,Projvar
.                                                  add PrjARMarTot,Projvar
.                                                  add PrjARAprTot,Projvar
.                                                  add PrjARMayTot,Projvar
.                                                  add PrjARJunTot,Projvar
.                                              add PrjARJulTot,Projvar
.                                              add PrjARAugTot,Projvar
.                                                add PrjARSepTot,Projvar
.                                                add PrjAROctTot,Projvar
.                                              add PrjARNovTot,Projvar
.                                              add PrjARDecTot,Projvar
.                                                 move projvar to ytdpro
.START PATCH 2.3
                                                  add prjAR,Projvar2
.end patch 2.6
.END PATCH 2.3
                                         endif
.Patch 2.0
.START PATCH 1.8 ADDED LOGIC
.Refresh vars because revenue file has not yet been converted!!
TESTER
                    unpack  nrevfld,type,src,cid,yr0
.END PATCH 1.8 ADDED LOGIC
          endif
.START PATCH  2.3
        move    Projvar2,EOYPRO
.END PATCH    2.3
        move    Projvar  to YTDPRO
        add     projnew,projlast,PROJTOT
.**************************************************************************************

.Projected for current year
Project
.Patch 2.0 Comment Out
.                        move      projtot to calc1
.                   CALC        cal = (calc1/12)*nmm
.                             move      cal to Projvar
.                        move      Projvar to YTDPRO
.Patch 2.0 Comment Out
.=============================================================================
.Final Check of Criteria 
          If        (cid = "009221") . never got here with this list???????
          call      debug
          endif
.begin patch 3.0
.....maybe if we have a record we should just fing output it?
		move	c1,wrtflg
.....maybe if we have a record we should just fing output it?
.end patch 3.0

                        if (Projvar <> c0)
                                move c1 to wrtflg
                        endif
                        if (unbill <> c0)
                                move c1 to wrtflg
                        endif
.........................................	
                        if (wrtflg = c1)
                                goto listview
                        else
                                goto reader
                        endif
        endif
.======================================================================================
Listview

.Start Patch 2.4
        move yr2act to ytdact
        move unbill to unbill1
.Total income due with YTD Revenue and REVunbld
        add unbill to newtot,INCDUE
        clear hidyr
.Creating "hidden" total for sorting purposes
.        add  sorthlp,incdue,hidyr
        add  sorthlp,ytdact,hidyr
        move hidyr to str13
        NProListView.InsertItem giving N7 using str13
.        move incdue to totdue
.        NProListView.SetItemText giving N8 using n7,totdue,1
        NProListView.SetItemText giving N8 using n7,eoypro,1
.End Patch 2.4
.        unpack    holdmlr,str2,str4
          PackKey   Compfld,holdmlr
          call      compkey
.        PACK      MKEY FROM str4,"000"
.        CALL      NMLRKEY
        if not over
.                    move      mcomp to client
                    move      compcomp to client
        endif
        NProListView.SetItemText giving N8 using n7,client,2
        NProListView.SetItemText giving N8 using n7,yr1act,3
        NProListView.SetItemText giving N8 using n7,YTDPRO,4
        NProListView.SetItemText giving N8 using n7,YR2ACT,5
.=====================================================================================
        if        ((NEWTOT = ".00") & (Projvar = ".00"))
                  move "0" to varbud
        else
                move     newtot to CALC1
                  CALC          n9 =   (CALC1/Projvar)*100
          move      n9 to varbud
                  if       (varbud = "")
                  move "0" to varbud
                  endif
        endif
        call trim using varbud
        NProListView.SetItemText giving N8 using n7,Varbud,6

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
.START PATCH  2.3
.        call trim using VSYR2
.        NProListView.SetItemText giving N8 using n7,VSYR2,7
.        NProListView.SetItemText giving N8 using n7,EOYPRO,7
.END PATCH  2.3
.START PATCH  2.4
        call trim using VSYR2
        NProListView.SetItemText giving N8 using n7,VSYR2,7
.        NProListView.SetItemText giving N8 using n7,EOYPRO,7
        NProListView.SetItemText giving N8 using n7,Unbill1,8
.        sub newtot from ProjTOT
        sub incdue from projvar2
.        move PROJTOT to YRENDPRO
        move PROJvar2 to YRENDPRO
.END PATCH  2.4
        NProListView.SetItemText giving N8 using n7,YrEndPro,9
        repeat
.==================================================================
Print
        clear n9
                PACK PRTFILE1,PRTDIR,PRTNAME1
.patch1.4
.        if (osflag = "1" or osflag = "5" or osflag = "6")
..        if (osflag = c2)
..subpatch1.4
.          PRTOPEN prfile,"\\SRV2008a\laser8",PRTITLE,noprint,spoolfile=PRTFILE1
.        else
.         PRTOPEN prfile,"laser8",PRTITLE,noprint,spoolfile=PRTFILE1
.        endif
          if (osflag = c1 | osflag = C5 | osflag = c6 | osflag = c8 | osflag = c9)         .nt 2k xp
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
.begin patch 3.00
.        prtpage   prfile;*NEWPAGE:
.                               *ORIENT=*LANDSCAPE:
.                   *UNITS=*HIENGLISH;
	if	(pgcnt > c1)
        prtpage   prfile;*NEWPAGE:
                               *ORIENT=*LANDSCAPE:
                   *UNITS=*HIENGLISH;
	else
        prtpage   prfile;*ORIENT=*LANDSCAPE:
                   *UNITS=*HIENGLISH;
	endif
.end patch 3.00
        clear     row
        move      "300",row
        prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date:";
.        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*font=font12,str10;
        pack titleyr1,"Receivables ",YR2
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
.START PATCH 2.5
        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,"YTD",*boldoff;
        prtpage prfile;*pColumn3:row,*font=font12,*ALIGNMENT=*right,*boldon,"YTD",*boldoff;
        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*right,*boldon,"Variance",*boldoff;
.START PATCH 2.3
        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*boldon,"Variance",*boldoff;
        prtpage prfile;*pColumn7:row,*font=font12,*ALIGNMENT=*right,*boldon,"To Make",*boldoff;
.END PATCH 2.3
.START PATCH 2.3
.        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*boldon,"Variance",*boldoff;
.        prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*right,*boldon,"To Make",*boldoff;
.END PATCH 2.3
.        prtpage prfile;*pColumn7:row,*font=font12,*ALIGNMENT=*right,*boldon,"YTD",*boldoff;
.START PATCH 2.3
.        prtpage prfile;*pColumn8:row,*font=font12,*ALIGNMENT=*right,*boldon,"YTD",*boldoff;
        prtpage prfile;*pColumn8:row,*font=font12,*ALIGNMENT=*right,*boldon,"Total EOY",*boldoff;
.END PATCH 2.3
        add     eightlpi,row
        add     "50",row
        prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Client",*ULOFF,*boldoff;
        PACK str11 with YR1,b1,"Actual"
        prtpage prfile;*pColumn1:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,str11,*ULOFF,*boldoff;
        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"Projected",*ULOFF,*boldoff;
        PACK str11 with YR2,b1,"Actual"
        prtpage prfile;*pColumn3:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,str11,*ULOFF,*boldoff;
        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Budget %",*ULOFF,*boldoff;
        pack    str11,"Vs. ",YR1," %"
.START PATCH 2.4
        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*right,*ll,*boldon,*ULON,str11,*ULOFF,*boldoff;
.        prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Unbilled",*ULOFF,*boldoff;
        prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Unbilled",*ULOFF,*boldoff;
        prtpage prfile;*pColumn7:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Projection",*ULOFF,*boldoff;
.END PATCH 2.4
.START PATCH 2.3
.        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*ll,*boldon,*ULON,str11,*ULOFF,*boldoff;
.        prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Unbilled",*ULOFF,*boldoff;
.        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Unbilled",*ULOFF,*boldoff;
.        prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Projection",*ULOFF,*boldoff;
.END PATCH 2.3
.        prtpage prfile;*pColumn7:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Total Due",*ULOFF,*boldoff;
.START PATCH patch2.3
.        prtpage prfile;*pColumn8:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Total Due",*ULOFF,*boldoff;
        prtpage prfile;*pColumn8:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Projection",*ULOFF,*boldoff;
.END PATCH  2.3
.===========================================================================================
.       PACK str11 with YR2,b1,"YTD"
..        PACK str11 with YR1,b1,"Actual"
..        prtpage prfile;*pColumn1:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,str11,*ULOFF,*boldoff;
..        PACK str11 with YR2,b1,"YTD"
..        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*right,*boldon,*ll,str11,*boldoff;
..        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*right,*boldon,"YTD",*boldoff;
..        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*ll,*boldon,*ULON,yr2,*ULOFF,*boldoff;
..        add     eightlpi,row
..        add     "50",row
..        prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"ID",*ULOFF,*boldoff;
..        prtpage prfile;*pColumn1a:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Client",*ULOFF,*boldoff;
..        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"Projected",*ULOFF,*boldoff;
..        PACK str11 with YR2,b1,"Actual"
..        prtpage prfile;*pColumn3:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,str11,*ULOFF,*boldoff;
..        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"Difference",*ULOFF,*boldoff;
...        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*ll,*boldon,*ULON,"Projection",*ULOFF,*boldoff;
..        prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Unbilled",*ULOFF,*boldoff;
.=================
.End PATCH 2.5
        else
        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,"YTD",*boldoff;
        prtpage prfile;*pColumn3:row,*font=font12,*ALIGNMENT=*right,*boldon,"YTD",*boldoff;
        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*right,*boldon,"Variance",*boldoff;
.START PATCH 2.3
        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*boldon,"Variance",*boldoff;
        prtpage prfile;*pColumn7:row,*font=font12,*ALIGNMENT=*right,*boldon,"To Make",*boldoff;
.END PATCH 2.3
.START PATCH 2.3
.        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*boldon,"Variance",*boldoff;
.        prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*right,*boldon,"To Make",*boldoff;
.END PATCH 2.3
.        prtpage prfile;*pColumn7:row,*font=font12,*ALIGNMENT=*right,*boldon,"YTD",*boldoff;
.START PATCH 2.3
.        prtpage prfile;*pColumn8:row,*font=font12,*ALIGNMENT=*right,*boldon,"YTD",*boldoff;
        prtpage prfile;*pColumn8:row,*font=font12,*ALIGNMENT=*right,*boldon,"Total EOY",*boldoff;
.END PATCH 2.3
        add     eightlpi,row
        add     "50",row
        prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Client",*ULOFF,*boldoff;
        PACK str11 with YR1,b1,"Actual"
        prtpage prfile;*pColumn1:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,str11,*ULOFF,*boldoff;
        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"Projected",*ULOFF,*boldoff;
        PACK str11 with YR2,b1,"Actual"
        prtpage prfile;*pColumn3:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,str11,*ULOFF,*boldoff;
        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Budget %",*ULOFF,*boldoff;
        pack    str11,"Vs. ",YR1," %"
.START PATCH 2.4
        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*right,*ll,*boldon,*ULON,str11,*ULOFF,*boldoff;
.        prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Unbilled",*ULOFF,*boldoff;
        prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Unbilled",*ULOFF,*boldoff;
        prtpage prfile;*pColumn7:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Projection",*ULOFF,*boldoff;
.END PATCH 2.4
.START PATCH 2.3
.        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*ll,*boldon,*ULON,str11,*ULOFF,*boldoff;
.        prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Unbilled",*ULOFF,*boldoff;
.        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Unbilled",*ULOFF,*boldoff;
.        prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Projection",*ULOFF,*boldoff;
.END PATCH 2.3
.        prtpage prfile;*pColumn7:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Total Due",*ULOFF,*boldoff;
.START PATCH patch2.3
.        prtpage prfile;*pColumn8:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Total Due",*ULOFF,*boldoff;
        prtpage prfile;*pColumn8:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Projection",*ULOFF,*boldoff;
.END PATCH  2.3
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
.START PATCH 2.5
                    NProListView.GetItemText giving Client using n9,6
                  prtpage prfile;*pColumn:row,*font=font8,*ALIGNMENT=*Left,CLIENT;
..                  NProListView.GetItemText giving Client using n9,6
..                  NProListView.GetItemText giving str6 using n9,7
..                prtpage prfile;*pColumn:row,*font=font8,*ALIGNMENT=*Left,*ll,str6;
..                prtpage prfile;*pColumn1a:row,*font=font8,*ALIGNMENT=*Left,CLIENT;
.End PATCH 2.5
                goto Pro1
          else
                    NProListView.GetItemText giving Client using n9,2
                  prtpage prfile;*pColumn:row,*font=font8,*ALIGNMENT=*Left,CLIENT;
        endif
.Previous Year Actuals
ACT1
                  NProListView.GetItemText giving str10 using n9,3
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NYR1TOT
                if (tmpvar1 = c0)
                        move  c0 to str14
                            prtpage prfile;*pColumn1:row,*font=font8,*ALIGNMENT=*right,*ll,str14;

                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
                                      prtpage prfile;*pColumn1:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                              else

                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2,str14
                              prtpage prfile;*pColumn1:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif
.Year to Date Projected
PRO1
.LM Year to date Projected
        if (loopflg = c2)
        
.Patch 2.2
                  NProListView.GetItemText giving str10 using n9,8
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NYR1TOT
                if (tmpvar1 = c0)
                        move  c0 to str14
                            prtpage prfile;*pColumn1:row,*font=font8,*ALIGNMENT=*right,*ll,str14;

                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
                                      prtpage prfile;*pColumn1:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                              else

                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2,str14
                              prtpage prfile;*pColumn1:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif
.Patch 2.2
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
                    NProListView.GetItemText giving str10 using n9,4
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
.START PATCH 2.5 LM
.Variance Budget
                clear str9
                  NProListView.GetItemText giving STR9 using n9,3
.                 prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str9;
                  prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str9;
.Variance vs Previous year
                clear str9
                  NProListView.GetItemText giving str9 using n9,7
.                 prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str9;
                  prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str9;
.End PATCH 2.5 LM
                goto  Ubill
        else
.BRK
          NProListView.GetItemText giving STR10 using n9,5
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
        endif
.Variance Budget

                clear str9
                  NProListView.GetItemText giving STR9 using n9,6
.                 prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str9;
                  prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str9;
.Variance vs 2000

.START PATCH 2.4
                clear str9
                  NProListView.GetItemText giving str9 using n9,7
.                 prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str9;
                  prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str9;
.END PATCH 2.4
.START PATCH 2.3
.                clear str9
.                 NProListView.GetItemText giving str9 using n9,7
.                 prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str9;
.END PATCH 2.3


.REVunbld
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
.START PATCH 2.4
                            prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH 2.4
.START PATCH 2.3
.                           prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.                           prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH 2.3

                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
.START PATCH 2.3
.START PATCH 2.4
                                      prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH 2.4
.                                     prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH  2.3
                        else
                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2,str14
.START PATCH 2.3
.Start PATCH 2.4
                              prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH 2.4
.                             prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH  2.3
                        endif
                endif
        endif
.Year End Projection
                if (loopflg = c2)
..                NProListView.GetItemText giving str10 using n9,4
                NProListView.GetItemText giving str10 using n9,9
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NENDPTOT
                    if (tmpvar1 = c0)
                            move tmpvar1 to str14
                            move c0 to str14
.                                     prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                                      prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                    else
                            if (tmpvar1 < c0)
                              call removechar using str10,dash
                                      call FormatNumeric using str10,str13,comma
                                    pack str14,dash,str13
                                    prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
..                                    prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                            else
                                        call FormatNumeric using str10,tmpvar2,comma
                                    move tmpvar2 to str14
..                                    prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                                                prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
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
.START PATCH 2.3
.START PATCH 2.4
                            prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH 2.4
.                           prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH 2.3
                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
.START PATCH 2.3
.START PATCH 2.4
                                prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH 2.4
.                                prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH  2.3
                        else
                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2 to str14
.START PATCH 2.3
.START PATCH 2.4
                                prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH 2.4
.                                prtpage prfile;*pColumn6:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH  2.3
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
.START PATCH 2.3
.                           prtpage prfile;*pColumn8:row,*font=font8,*ALIGNMENT=*right,*ll,tmpvar1;
.START PATCH 2.4
.                           prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,tmpvar1;
.END PATCH 2.4
.END PATCH 2.3
                else
                        if (tmpvar1 < c0)
                              call removechar using str10,dash
                            call FormatNumeric using str10,str13,comma
                                pack str14,dash,str13
.START PATCH 2.3
.                                     prtpage prfile;*pColumn8:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.START PATCH 2.4
.                                     prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH 2.4
.END PATCH  2.3
                        else
                              call FormatNumeric using str10,tmpvar2,comma
                                move tmpvar2 to str14
.START PATCH 2.3
.                                     prtpage prfile;*pColumn8:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.START PATCH 2.4
.                                     prtpage prfile;*pColumn7:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
.END PATCH 2.4
.END PATCH 2.3
                        endif
                endif
.START PATCH 2.3
.......................................................................................................
.                goto record
.EOY projection total

.                 NProListView.GetItemText giving str10 using n9,7
                  NProListView.GetItemText giving str10 using n9,1
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NEOYPROJ
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
.END PATCH 2.3
**********************************************************************************************
Diff
.Start patch 2.5
.EOY projection total LM
                  NProListView.GetItemText giving str10 using n9,4
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NEOYPROJ
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
.                goto record
.YTD Difference
..                NProListView.GetItemText giving str10 using n9,3
..                clear tmpvar1
..                move str10 to tmpvar1
..                add tmpvar1 to NDIFTOT
..                if (tmpvar1 = c0)
..                        move tmpvar1 to str14
..                        move c0 to str14
..                          prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
..                else
..                        if (tmpvar1 < c0)
..                            call removechar using str10,dash
..                          call FormatNumeric using str10,str13,comma
...                                pack str14,dash,str13
..                                prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
..                        else
..                            call FormatNumeric using str10,tmpvar2,comma
..                                move tmpvar2 to str14
..                                    prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
..                        endif
..                endif

.End patch 2.5
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
.               if (loopflg = c1 )
.Patch 2.2
               if (loopflg = c1 | loopflg = c2)
.Patch 2.2
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
.Start patch 2.5
.               if (loopflg = c2)
               prtpage prfile;*pcolumn3:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.               goto Punbil
.               else
.               prtpage prfile;*pcolumn3:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.               endif
.==================================================================================================
                clear     n9
                move      Nyr2tot to CALC1
.                move      NPROYTD to CALC1
.                 CALC          n9 =   ((NYR2TOT-NPROYTD)/CALC1)*100
                  CALC          n9 =   (calc1/nproytd)*100
                clear     str9
          move      n9 to str9
                  if       (str3 = "")
                   move "0" to str3
                  endif
.               prtpage prfile;*pcolumn4:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str9,*boldoff;
               prtpage prfile;*pcolumn5:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str9,*boldoff;
.==================================================================================================
.START PATCH 2.4
                clear     n9
                move      NYR2TOT to CALC1
.                move      NYR1TOT to CALC1
.                 CALC          n9 =   ((NYR2TOT-NYR1TOT)/CALC1)*100
                  CALC          n9 =   (calc1/nyr1tot)*100
                clear     str9
          move      n9 to str9
                  if        (str9 = "")
                   move "0" to str9
               endif
.               prtpage prfile;*pcolumn5:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str9,*boldoff;
               prtpage prfile;*pcolumn4:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str9,*boldoff;

.END PATCH  2.4
.START PATCH 2.3
.                clear     n9
.                .move      NYR1TOT to CALC1
.                 CALC          n9 =   ((NYR2TOT-NYR1TOT)/CALC1)*100
.                clear     str9
.         move      n9 to str9
.                 if        (str9 = "")
.                  move "0" to str9
.               endif
.               prtpage prfile;*pcolumn5:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str9,*boldoff;

.END PATCH  2.3
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
.Start patch 2.5
.               if (loopflg = c2)
               prtpage prfile;*pcolumn6:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.               else
.START PATCH 2.3
.START PATCH 2.4
.               prtpage prfile;*pcolumn6:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.               prtpage prfile;*pcolumn5:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.END PATCH 2.4
.               prtpage prfile;*pcolumn6:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.               prtpage prfile;*pcolumn5:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.END PATCH 2.3
.               endif
.End patch 2.5
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
.Start patch 2.5
.               if (loopflg = c2)
.               prtpage prfile;*pcolumn5:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.               goto PYTDIFF
.               else
.START PATCH 2.4
               prtpage prfile;*pcolumn7:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.               prtpage prfile;*pcolumn6:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.END PATCH   2.4
.START PATCH 2.3
.               prtpage prfile;*pcolumn7:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.               prtpage prfile;*pcolumn6:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.END PATCH   2.3
.               endif
.End patch 2.5
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
.START PATCH 2.3
.                prtpage prfile;*pcolumn8:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.START PATCH 2.4
.                prtpage prfile;*pcolumn7:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.END PATCH 2.4
.               goto LastLine
......................................................................................................
.END PATCH 2.3
.
.START PATCH 2.3
               clear str14
               clear str10
               clear str13
               move NEOYPROJ to str10
               call removechar using str10,dash
               call FormatNumeric using str10,str13,comma
               if (NEOYPROJ < c0)
                       pack str14,dash,str13
               else
                   move str13 to str14
               endif
.                prtpage prfile;*pcolumn7:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
                 prtpage prfile;*pcolumn8:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
               goto LastLine
.END PATCH 2.3
PYTDIFF
*****************************************************
.Start patch 2.5
.               clear str14
.               clear str10
.               clear str13
.               move NDIFTOT to str10
..               move YTDDIFF to str10
.               call removechar using str10,dash
.               call FormatNumeric using str10,str13,comma
.               if (NDIFTOT < c0)
.                      pack str14,dash,str13
.               else
.                   move str13 to str14
.               endif
.               prtpage prfile;*pcolumn4:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.End patch 2.5
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
.START PATCH 2.2
               if (loopflg = c2)
.                prtpage prfile;*pcolumn1:row,*font=font8,*ALIGNMENT=*Left,*ll,yr1;
.                prtpage prfile;*font=font8,*ALIGNMENT=*Left,*ll," YTD Actuals: ";
.               move NYR1TOT to str10
.                call FormatNumeric using str10,tmpvar2,comma
.                prtpage prfile;*pcolumn3:row,*font=font9,*ALIGNMENT=*Right,*boldon,*ll,tmpvar2;
.END PATCH 2.2
               add     eightlpi,row
               add     "30" to row
                 prtpage prfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,*ll,"Note: Ranked by YTD actual booked income.";
.                prtpage prfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,*ll,"Note: Ranked by 2005 actual booked income.";
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
.begin patch 3.00 use pdf regardless of os
.patch1.4
.        if (osflag = "1" or osflag = "5" or osflag = "6")
.        if (osflag = c2)
.subpatch1.4
.begin patch 3.00  turn off copies
.                    loop
.                    until (N3 = COPY)
.                              PRTPLAY PRTFILE1,"\\nins2\laser8"

			if	(Loopflg = c1)
                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\Brokerage.pdf"
		          Pause     "10"
          		Move      "Brokerage Receivables PDF File",MailSubjct
          		Move      "Creques@nincal.com",MailFrom
          		Move      "Dherric@nincal.com",MailTo
          		Move      Str55,MailBody
          		MOve      "c:\work\pdf\Brokerage.pdf",MailAttach
			
			Else                           
			
                              PRTPLAY PRTFILE1,"PDF:",PDFNAME="c:\work\pdf\Management.pdf"
		          Pause     "10"
          		Move      "Management Receivables PDF File",MailSubjct
          		Move      "Creques@nincal.com",MailFrom
          		Move      "Dherric@nincal.com",MailTo
          		Move      Str55,MailBody
          		MOve      "c:\work\pdf\Management.pdf",MailAttach
			endif
		Pause     "10"
		call	sendmail
		Pause     "5"
		Erase	Mailattach
.                    add c1 to n3
.                    repeat
.end patch 3.00  turn off copies
                erase PRTFILE1
                if (chkall = c1)
                        if (loopflg > c2)
                              goto ender
                        endif
                        goto restart
                  endif
                goto  Ender
.          elseif (osflag = c3 | osflag =c4)         .win 95 98
.
.                  loop
.                    until (N3 = COPY)
.                              PRTPLAY PRTFILE1,"laser8"
.                          add c1 to n3
.                    repeat
.                erase PRTFILE1
.                if (chkall = c1)
.                        if (loopflg > c2)
.                              goto ender
.                        endif
.                        goto restart
.                  endif
.                goto  Ender
.          elseif (osflag = "9" )         .win 7
.                    loop
.                    until (N3 = COPY)
.                              PRTPLAY PRTFILE1,"\\nins2\laser8"
.                    add c1 to n3
.                    repeat
.                erase PRTFILE1
.                if (chkall = c1)
.                        if (loopflg > c2)
.                              goto ender
.                        endif
.                        goto restart
.                  endif
.                goto  Ender
.          Else
.                    loop
.                    until (N3 = COPY)
.                              PRTPLAY PRTFILE1,""
.                    add c1 to n3
.                    repeat
.                erase PRTFILE1
.                if (chkall = c1)
.                        if (loopflg > c2)
.                              goto ender
.                        endif
.                        goto restart
.                  endif
.                goto  Ender
.        endif
.end patch 3.00 use pdf regardless of os

.begin patch 2.6
.==========================================================================
ClearPrjVars
          move      C0,PrjLR   
          move      C0,PrjNin  
          Clear     prjLRJan   
          Clear     prjLRFeb   
          Clear     prjLRMar   
          Clear     prjLRApr   
          Clear     prjLRMay   
          Clear     prjLRJun   
          Clear     prjLRJul   
          Clear     prjLRAug   
          Clear     prjLRSep   
          Clear     prjLROct   
          Clear     prjLRNov   
          Clear     prjLRDec   
          Clear     prjNINJan  
          Clear     prjNINFeb  
          Clear     prjNINMar  
          Clear     prjNINApr  
          Clear     prjNINMay  
          Clear     prjNINJun  
          Clear     prjNINJul  
          Clear     prjNINAug  
          Clear     prjNINSep  
          Clear     prjNINOct  
          Clear     prjNINNov  
          Clear     prjNINDec  
          move      C0,PRJAR   
          move      C0,PRJAP   
          Clear     PRJARJan   
          Clear     PRJARFeb   
          Clear     PRJARMar   
          Clear     PRJARApr   
          Clear     PRJARMay   
          Clear     PRJARJun   
          Clear     PRJARJul   
          Clear     PRJARAug   
          Clear     PRJARSep   
          Clear     PRJAROct   
          Clear     PRJARNov   
          Clear     PRJARDec   
          Clear     PRJAPJan   
          Clear     PRJAPFeb   
          Clear     PRJAPMar   
          Clear     PRJAPApr   
          Clear     PRJAPMay   
          Clear     PRJAPJun   
          Clear     PRJAPJul   
          Clear     PRJAPAug   
          Clear     PRJAPSep   
          Clear     PRJAPOct   
          Clear     PRJAPNov   
          Clear     PRJAPDec   

          return
.==========================================================================
.end patch 2.6
FileGo
.Flag set to "N" if in Modify or New mode
.        branch result to FileGo1,FileGo2,FileGo3,FileGo3
        branch result to FileGo1
FileGo1
          SHUTDOWN  "CLS"
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
.Patch1.5
                    Shutdown
                    stop
.patch1.5
.        return
Ender
.        setitem PROCStatComment,0,"        Done Printing on laser8!!"
.        setprop Process,visible=c1
.        pause   c5
.        setprop Process,visible=c0
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
