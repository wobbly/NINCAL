...............................................................................
. INCOMEBYMON - CALC'S CLIENT BILLING & ADJUSTMENT FOR CURRENT MONTH.
...............................................................................

PC       EQU       0
         INC       COMMON.inc
.
         INC       CONS.inc
.begin patch 1.8
         INCLUDE   CONSacct.inc
         INCLUDE   NINVDD.inc
         include   nacddd.inc
         include   nshpdd.inc
.end patch 1.8.
				include	compdd.inc
				include	cntdd.inc
;           include nmlrdd.inc
.
         INC       NBILDD.inc
         INCLUDE   NOWNDD.INC
         INCLUDE   NDATDD.inc
         INCLUDE   NORDDD.INC
         INCLUDE   NDAT3DD.INC
.
         INC       GNXTDD.inc
         include   nmrgdd.inc
         include   nprjdd.inc
ProjReadFile	File
Proj9	File
	prepare proj9,"c:\work\projteamfocus.dat"
	OPEN	ProjReadFile,"c:\work\projteamfocus.dat"
			
			
			
release	init "1.0"
White     color
Black     color
.Listviewobject for Sorting of Teams
Team1   init    "03-05-07-01"         .SA TEAM
;Team1   init    "03-05-07-04-22"         .SA TEAM  11/26/03 move 04 to JC.
;Team1   init    "03-05-07-22"         .SA TEAM
Team2   init    "08-11"         .JC TEAM
;Team2   init    "01-04-08-11"         .JC TEAM
Team3   init    "04-13-14-15-10"            .SM TEAM
Team4   init    "02-06-19"            .LM
T1      init    "1"						  .SA
T2      init    "2"						  .JC
T3      init    "3"						  .SM
NAME1   init    "SUSAN'S TEAM"
NAME2   init    "JEANETTE'S TEAM"
NAME3   init    "SUZIE'S TEAM"
NAME4   init    "LIST MANAGEMENT"       
seller  dim      2
team    dim      1
Stamp   dim      10
holdtm dim       1
SLSARTOT    FORM    13
SLSFINLRTOT form    13
n13         form    13
n13a         form    13
saleskey    dim	  46
.vrs for listview
LVARTOT  	form    9.2
LVADJLRTOT  form    9.2
LVFINLRTOT  form    9.2
LVLRTOT     form    9.2
LVLRSTR     DIM     13
LVARSTR     DIM     13
LVADJLRSTR  DIM     13
LVFINLRSTR  DIM     13
str46       dim     46
.subpatch2.6
			
			
			
			
















.===========================================================================
*Sorted Revenue file
INFILE     FILE
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
;YR1        INIT	  "2001"
YR1        INIT      "2004"
;YR2        INIT   "2002"
YR2        INIT   "2005"
.*********************************************************************************
.*********************************************************************************
TMPVAR1    FORM     13
TMPVAR2    DIM     14
UNBILL     FORM   10
INCDUE     FORM   10
HIDYR      FORM   10.2
CAL        FORM   11
LOADV      FORM   10
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
PRTDIR     INIT    "C:\WORK\"                 .Printfile directory
PRTFILE1   DIM    19                          .full printfile string
.============================================================================
.NINCA List Management
LMLIST       init   "018710"

holdmlr    DIM    6        .held mlr value to check if already in listview object
SORTHLP    FORM   10.2     .add to yr2 total in hidden coulumn to be used as sorted column
PROJVAR    FORM   10       .Variable for this year to date projection
PROJTOT    FORM   10       .Total Projected For Year
OLDTOT     FORM   10       .Var for cumulative total for previous year
NEWTOT     FORM   10       .Var for cumulative total for this year
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
Yr1Act     DIM    10             .Previous Year Actuals
YTDPRO     DIM    10             .Year to date projected
YR2ACT     DIM    10             .Year to date actuals
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
NDIFTOT	    form    10
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
Column8  form    9
Column9  form    9
.=========================
*******************************************************************************
.some goodies for on moving Icon
ICONID   FORM      4
CURICON  FORM      4
FRAMES   FORM      4
ICON$ANIM    ICON
FORM32A  FORM      32
FORM32B  FORM      32
.
CurRec	form    5.2
CurVal	form	3
LastVal	form	3
.
ANIMICON PLFORM    ANIMATE  * CONTAINS ALL THE ICONS
.begin   release 1.3
;ile      file
.end   release 1.3
.end icon goodies
*******************************************************************************
font8    font
        create  font8,"Times New Roman",size=10
font9    font
        create  font9,"Times New Roman",size=10,italic
.==============================================================================

cHECKNUM	DIM	6



















        loop
AGAIN
	 				move c0 to wrtflg
        		call nprjseq
           until over
           if ((ProjYr = "2005")&(ProjSrc = "B"))
				reposit projreadfile,c0
				loop
							read	projreadfile,seq;str1,CHECKNUM
					until over
							if (projclient = "004261")
test2
							reset projclient

							endif

							IF (CHECKNUM = pROJCLIENT)
								GOTO AGAIN
							ENDIF
							

					repeat

SECPROJREAD
***************************************************************************************
*Code for new projection file
              move  c0,projlast
              move  c0,projnew
				move projclient to str6
             bump str6,c2
				if (str6 = "7951")
test
				reset seller
				endif
             PACK      MKEY FROM str6,z3
             CALL      NMLRKEY
				  call zfillit using mslsper
					rep zfill,mslsper
             Move  MSLSPER,seller

      if (ProjSrc = "B")
						move "E" to ProjType
						move "B" to ProjSrc
                 move  C0,N2
                 loop
                    add   C1,N2
                    move  N2,str3
                    rep   zfill,str3
                    pack  NPRJFLD,ProjType,ProjSrc,ProjClient,ProjYr,str3
                    rep   zfill,NPRJFLD
							move nprjfld to str14
                    call  NPRJKEY
                 until over
               		move   ProjLR,projnew
                 until (ProjMast = YES)
                 repeat
                 unpack  str14,ProjType,ProjSrc,ProjClient,ProjYr
 						move "R" to ProjType
						move "B" to ProjSrc
                 move  C0,N2
                 loop
                    add   C1,N2
                    move  N2,str3
                    rep   zfill,str3
                    pack  NPRJFLD,ProjType,ProjSrc,ProjClient,ProjYr,str3
                    rep   zfill,NPRJFLD
                    call  NPRJKEY
                 until over
                   move   ProjLR,projlast
                 until (ProjMast = YES)
                 repeat
              endif
              move    Projvar to YTDPRO
              add     projnew,projlast,PROJTOT
Project
              move      projtot to calc1
              CALC    cal = (calc1/12)
              move      cal to Projvar
              move      Projvar to YTDPRO
.=============================================================================
.Final Check of Criteria for brokerage
               if (Projvar <> c0)
                   move c1 to wrtflg
               endif
               if (wrtflg = c1)
                 reset team1
                 reset team2
                 reset team3
                 scan  seller in team1
                 if equal 
                    move t1 to team
                    reset seller
                    goto LV
                 endif
                 scan  seller in team2
                 if equal 
                    move t2 to team
                    reset seller
                    goto LV
                 endif
                 scan  seller in team3
                 if equal 
                    move t3 to team
                    reset seller
                    goto LV
                  endif
                 move c4 to team
LV
                    Write Proj9,seq;Team,str6,MCOMP,ProjVar                        
                endif
		          endif


        
        repeat


.			weof     proj9,seq




         STOP
.
			include	compio.inc
			include	cntio.inc
.         INCLUDE   NMLRIO.inc
         include  nprjio.inc
         INCLUDE  COMLOGIC.inc























































