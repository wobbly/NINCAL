.NREV0022   DLH  - actual vs projected for current Month

PC         EQU       0
           include common.inc
           include cons.inc
           include   compdd.inc
           include   cntdd.inc
           include ndatdd.inc
           include nrevdd.inc
           include nprjdd.inc

.===========================================================================
Release    Init       "1.0"   DLH rewrite of Nrev0012 - actual vs projected for current Month
Reldate    Init       "2016 January 20"
.======================================================================
.*Sorted Revenue file
INFILE     FILE
output     file
.Print file
PRFILE     PFILE
.Projected Totals
.=====================================================================================
.*********************************************************************************
.************************************************************************************
.Do a search on yearly to find place to change projection value
.change yearly----
LastYr        INIT   "2015"
ThisYr        INIT   "2016"
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
OUTSRT     INIT  "revenue.srt"   .Sorted Output file
ClintSrt   INIT  "3-8"                        .Sort by client #
SORTFLE    DIM    70                          .Var to pack file names of sort
PRTITLE    DIM    18                          .Title of Printjob
.============================================================================

holdmlr    DIM    6        .held mlr value to check if already in listview object
SORTHLP    FORM   10.2     .add to ThisYr total in hidden coulumn to be used as sorted column
PROJVAR    FORM   10       .Variable for this year to date projection
PROJTOT    FORM   10       .Total Projected For Year
OLDTOT     FORM   10       .Var for cumulative total for previous year
NEWTOT     FORM   10       .Var for cumulative total for this year
.*Flags
MTCHFLG    FORM   1        .flag if client\list a match or not
WRTFLG     FORM   1        .flag to see if it passes one of the conditions
.                                        .if ThisYr YTD <> 0
.                                        .Unbilled   <> 0
.                                        .Projytd    <> 0
.                                        .CID        =  0000000
CHKALL     form    1       .To see if doing both reports
LoopFlg    form    1       .Used for doing multiple passes on reports
.================================================================================================================================================================
.Vars for insertion into listview
LastYrAct     DIM    10             .Previous Year Actuals
YTDPRO     DIM    10             .Year to date projected
ThisYrACT     DIM    10             .Year to date actuals
UNBILL1    DIM    10             .Unbilled Total
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
NLastYrTOT     FORM    10
NPROYTD     FORM    10
.Current Year
NThisYrTOT     FORM    10
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
TitleLastYr  dim     12
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
.====================================================================================
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
Perc       Form   3.3    
.
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
.needed for printing to PDF for non native fonts and things like Copyright to appear correctly in the pdf
PDF_FLAGS_WIN_ANSI_ENCODING EQU 128
..............................................................
.Menu
.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu
.Present Data for Menu Bar
FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search;-;&Preferences"
HData   init    "&Help;&About"
.=====================================================================
        move    "NREV0022.PLS",Wprognme
        move    "Actuals vs. Projected",Wfunction
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
.********************************************************************************
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
        call getwinver
.===============================================================================
        NProListView.InsertColumn using "",1,1
        NProListView.InsertColumn using "Total Due",100,2
        NProListView.InsertColumn using "Name",100,3
        NProListView.InsertColumn using "Prev Actual",100,4
        NProListView.InsertColumn using "YTD Projected",100,5
        NProListView.InsertColumn using "YTD Actual",100,6
        NProListView.InsertColumn using "Variance Budget",100,7
        NProListView.InsertColumn using "Var vs Prev",100,8
        NProListView.InsertColumn using "Unbilled",100,9
        NProListView.InsertColumn using "Year End Proj",100,10
        NProListview.setcolumnFORMat giving result using 0,0
        NProListview.setcolumnFORMat giving result using 1,1
        NProListview.setcolumnFORMat giving result using 2,1

OpenIt
*.==========================================================================
*=========================================================================
          MOVE    "1000000000.00" to sorthlp
.==============================================================================
          if        (program = "NREV0022")
          uNPACK    Today into mm,str1,dd,str1,yy
          move      "20",cc
          PACK      str8,cc,yy,mm,dd
          else
          CLOCK   TIMESTAMP to str8
          UNPACK  str8,cc,yy,mm,dd
          endif
.add code here to auto generate thisyr and lastyr variables
.testing
.        MOVE          "20151231",str8   
.        UNPACK  str8,cc,yy,mm,dd
.get current month
        MOVE    mm to nmm                   
.============================================================
.set current month on form
        move nmm to str2
        rep     " 0",str2
        setitem NprojEditMonth,0,str2
        setitem NprojEditCopy,0,"1"


.***********************************************************


Start
        call    OrderSetMouseBusy
.***********************************************************************************
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
.Clear vars
           move c0,PROJVAR
           move c0,PROJTOT
           move c0,NThisYrTOT
           move c0,OLDTOT
           move c0,NEWTOT
.===================================
         move c0,pgcnt
         move c0,NLastYrTOT
         move c0,NPROYTD
         move c0,NThisYrTOT
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
Brok
.Brokerage
         PACK  PRTITLE,"Brokerage"
         pack   SortFle,ntwkpath6,indat,comma,ntwkpath1,outsrt
.we only care about the current year and brokerage for this one
           pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","B","'","&9=","'",ThisYr,"'"
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
          calc      howmany=(N10/723)   .723 = 721(Revenue record length) + 2 bytes for CR/LF
              close         tempfile
*********************************************************************
          OPEN    INFILE,outsrt
        reposit infile,c0
.NOte do not need all these columns - just have not cleaned it up        
        NPROListview.deleteallitems
        NProListView.SetcolumnText using 1,"Total Due"
        NProListView.SetcolumnText using 2,"Name"
        NProListView.SetcolumnText using 3,"Actual"
        NProListView.SetcolumnText using 4,"Projected"
        NProListView.SetcolumnText using 5,"Actual"
        NProListView.SetcolumnText using 6,"Variance Budget"
        NProListView.SetcolumnText using 7,"Budget Vs. "
        NProListView.SetcolumnText using 8,"Unbilled"
        NProListView.SetcolumnText using 9,"Year End Pro"
         goto Reader
.====================================================================================
.====================================================================================
ListM
.List Management
          Close    INFILE

         PACK  PRTITLE,"List Mgmnt"
         pack   SortFle,ntwkpath6,indat,comma,ntwkpath1,outsrt
           pack   taskname,sortfle,";",clintsrt,comma,"s=2=","'","M","'","&9=","'",ThisYr,"'"
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
          calc      howmany=(N10/723)   .'723 = 721(Revenue record length) + 2 bytes for CR/LF
              close         tempfile
*********************************************************************
          OPEN    INFILE,outsrt
        reposit infile,c0

        NPROListview.deleteallitems
        NProListView.SetcolumnText using 1," Actuals"
        NProListView.SetcolumnText using 2,"Projection"
        NProListView.SetcolumnText using 3,"YTD Difference"
        NProListView.SetcolumnText using 4,"2015 Projected"
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
          CALL   NREVUpdateProgressBar
**************************************
.=======================================================================================
.Clear Vars for next year
        clear   incdue
        clear   totdue
        clear   str13
        clear   LastYract
        clear   ytdpro
        clear   mtchflg
        clear   Projvar
        clear   OLDTOT
        clear   NEWTOT
        clear   unbill
        clear   loadv
.========================================================================================
        move    CID to holdmlr
.Check to see if Clients meet criteria for Report
        branch  loopflg to Brkchk,LMCHK
Brkchk
        cmatch  SRC to "B"
        goto    READER if not equal
.        if      (YR0 = ThisYr)
.                 move    ".00" to oldtot
.                     move    OLDTOT to LastYrACT
                 pack nrevfld,type,src,cid,ThisYr
.***************************************************************************************
.*Code for projection file
          move      c0,projlast
          move      c0,projnew
                    if           (cid = "003045")
                    call         debug
                    endif
                    move      "RB",str2
                    move      C0,N2
                    loop
                              add       C1,N2
                              move      N2,str3
                              rep       zfill,str3
                              pack      NPRJFLD,str2,CID,ThisYr,str3
                              rep       zfill,NPRJFLD
                             move      "SECPROJREAD,2-NPRJKEY",Location
                             pack      KeyLocation,"Key: ",NPRJFLD
                              call      NPRJKEY
                              until over
                              until (PrjMast = YES)
                    repeat
                    MOve         MM,n2
                    Load         str3 from N2 of PRJARJan:
                                 PRJARFeb:
                                 PRJARMar:
                                 PRJARApr:
                                 PRJARMay:
                                 PRJARJun:
                                 PRJARJul:
                                 PRJARAug:
                                 PRJARSep:
                                 PRJAROct:
                                 PRJARNov:
                                 PRJARDec
                    call         trim using str3
                    move         str3,perc
TESTER
                    unpack  nrevfld,type,src,cid,yr0

**************************************************************************************

.Projected for current year
Project
                        move      PRJAR to calc1
                      
                        Calc     PErc = (Perc/100)
                    CALC      cal = (calc1)*Perc
                              move      cal to Projvar
                        move      Projvar to YTDPRO
                   add          cal,Projtot
.=============================================================================
.Final Check of Criteria for brokerage
.force it for now
                                goto listview
           
.=================================================
LMCHK
        cmatch  SRC to "M"
        goto    READER if not equal
        reset         RUNCODES
        scan          Cid,Runcodes
        goto          Reader if equal
**********************************************************************************
*Code for projection file
                    move      c0,projlast
                              move      " M",str2
                              move      C0,N2
                              loop
                                        add       C1,N2
                              move      N2,str3
                              rep       zfill,str3
                              pack      NPRJFLD,str2,CID,ThisYr,str3
                              move      "ThisYrstart,-NPRJKEY",Location
                             pack      KeyLocation,"Key: ",NPRJFLD
                              call      NPRJKEY
                              until over
                              until (PrjMast = YES)
                    repeat
                    MOve         MM,n2
                    Load         str3 from N2 of PRJARJan:
                                 PRJARFeb:
                                 PRJARMar:
                                 PRJARApr:
                                 PRJARMay:
                                 PRJARJun:
                                 PRJARJul:
                                 PRJARAug:
                                 PRJARSep:
                                 PRJAROct:
                                 PRJARNov:
                                 PRJARDec
                     call        trim using str3 
                     move        str3,perc 
*********************************************************************************
                        move    PRJAR to calc1
                        Calc     PErc = (Perc/100)
                    CALC      cal = (calc1)*Perc
                        move    cal to Projvar
                        move    Projvar to YTDPRO
                        add      projvar,projtot

                    
                        NProListView.SetItemText giving N8 using n7,CID,7
        
.======================================================================================
Listview
                add REVunbld to unbill
                Move   mm,N2
               LOAD LOADV using n2 from JANAR,FEBAR,MARAR,APRAR,MAYAR,JUNAR,JULAR,AUGAR:
                  SEPAR,OCTAR,NOVAR,DECAR
           if         (cal = c0 & loadv = c0 & unbill = c0)
           goto       reader
           endif

                move LOADV to ThisYract
           add        loadv to NThisYrTOT
           if         (loopflg = c1)
           PackKey   Compfld,holdmlr
          call      compkey
                      if not over
                    move      compcomp to client
                      endif
           else

          PackKey   Ndatfld,holdmlr
           call zfillit using ndatfld
           CALL NDATKEY
                      if not over
                      move OLSTNAME to client
                      ELSE
                      move      "something wrong no list",client
                      endif
           endif

        move unbill to unbill1
.Total income due with YTD Revenue and unbilled
        add unbill to newtot,INCDUE
        clear hidyr
.Creating "hidden" total for sorting purposes
        add  sorthlp,Loadv,hidyr
        move hidyr to str13
        NProListView.InsertItem giving N7 using str13
        move          loadv,str10
        NProListView.SetItemText giving N8 using n7,str10,1         
        NProListView.SetItemText giving N8 using n7,client,2
        NProListView.SetItemText giving N8 using n7,YTDPRO,3
.MTD Differential
                            clear str10
                            sub  projvar,loadv,YTDDIFF
                            move YTDDIFF to str10
                            NProListView.SetItemText giving N8 using n7,str10,4
.Unbilled
                            move unbill to unbill1
                            NProListView.SetItemText giving N8 using n7,unbill1,5
                    
                        NProListView.SetItemText giving N8 using n7,CID,6
        

        repeat
.==================================================================
Print
        clear n9
           if         (loopflg = c1)

           PRTOPEN    prfile,"PDF:","c:\work\pdf\Brokerage1.pdf",Flags=PDF_FLAGS_WIN_ANSI_ENCODING

           Elseif     (loopflg = c2)

           PRTOPEN    prfile,"PDF:","c:\work\pdf\Management1.pdf",Flags=PDF_FLAGS_WIN_ANSI_ENCODING

           endif

.=================================================================
.Headers
.Defining Header and Titles
Page
        CLEAR     ROWCOUNT
        ADD       C1 TO PGCNT
        if            (pgcnt > c1)   
        prtpage   prfile;*NEWPAGE:
                   *UNITS=*HIENGLISH;
        Else           
        prtpage   prfile;*UNITS=*HIENGLISH;
        endif   
        clear     row
        move      "300",row
        prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date:";
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*font=font12,str10;
        pack titleLastYr,"Revenue ",ThisYr
        prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,titleLastYr;
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

        Load    curmo,nmm with Month1,Month2,Month3,Month4,Month5,Month6,Month7:
                Month8,Month9,Month10,Month11,Month12
        call    trim using curmo
        pack    TITLEDT,curmo,b1,dd,comma,ThisYr
          prtpage prfile;*pTitle4:row,*ALIGNMENT=*CENTER,*font=font12,*boldon,TITLEDT,*boldoff;
        add     eightlpi,row
        add     "30",row
        add     eightlpi,row
.===========================================================================================
.LM & Brokerage
        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*right,*boldon,*boldoff;
        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*ll,*boldon,*ULON,ThisYr,*ULOFF,*boldoff;
        add     eightlpi,row
        add     "50",row
        prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"ID",*ULOFF,*boldoff;
        prtpage prfile;*pColumn1a:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Client",*ULOFF,*boldoff;
        prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"Actual",*ULOFF,*boldoff;
        prtpage prfile;*pColumn3:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"Projected",*ULOFF,*boldoff;
        prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*Right,*boldon,*ULON,"Difference",*ULOFF,*boldoff;
        prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*right,*boldon,*ULON,"Unbilled",*ULOFF,*boldoff;
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
          NProListView.GetItemText giving Client using n9,2
          NProListView.GetItemText giving str6 using n9,6
                  prtpage prfile;*pColumn:row,*font=font8,*ALIGNMENT=*Left,*ll,str6;
                  prtpage prfile;*pColumn1a:row,*font=font8,*ALIGNMENT=*Left,CLIENT;
. Actuals
ACT1
                  NProListView.GetItemText giving str10 using n9,1
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NLastYrTOT
                if (tmpvar1 = c0)
                        move  c0 to str14
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
. Projected
PRO1
                    NProListView.GetItemText giving str10 using n9,3
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NPROYTD
                if (tmpvar1 = c0)
                        move tmpvar1 to str14
                        move c0 to str14
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
.=========================================================================
.Variance Budget

                clear str10
                  NProListView.GetItemText giving STR10 using n9,4
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NPROYTD
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
                                move tmpvar2,str14
                                      prtpage prfile;*pColumn4:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif


.Unbilled
UBill
                  NProListView.GetItemText giving str10 using n9,5
                clear tmpvar1
                move str10 to tmpvar1
                add tmpvar1 to NUNBILL
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
                                move tmpvar2,str14
                              prtpage prfile;*pColumn5:row,*font=font8,*ALIGNMENT=*right,*ll,str14;
                        endif
                endif
.=====================================================================================
Record
                              add     eightlpi,row
                              add     "35",row
Rower
            if (ROWCOUNT = "46")
                           move "9800",row
                           prtpage prfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,"Page# ";
                           prtpage prfile;*font=font8,*ALIGNMENT=*Left,PgCnt;
                           prtpage prfile;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News";
                        add c1 to n9
                        goto page
                  else
                    add c1 to n9
                  endif
        repeat

.=======================================================================================
ENDLOOP
       if (ROWCOUNT < "44")
                 goto totals
       else
.Added to correct page # and page label for next to last page
.==========================================================================================
                 move "9800",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
                 prtpage prfile;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News";
.===========================================================================================
               move c1 to newpg
               add  c1 to pgcnt
               if     (pgcnt > c1)          
               prtpage prfile;*NEWPAGE:
                              *UNITS=*HIENGLISH
               else
               prtpage prfile;*UNITS=*HIENGLISH
               endif
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
.==================================================================================================
.Actuals
               clear str14
               clear str10
               clear str13
               move NThisYrTOT to str10
               call removechar using str10,dash
               call FormatNumeric using str10,str13,comma
               if (NThisYrTOT < c0)
                       pack str14,dash,str13
               else
                   move str13 to str14
               endif
               prtpage prfile;*pcolumn2:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.==================================================================================================
.Projection
               clear str14
               clear str10
               clear str13
               move ProjTOT to str10
               call removechar using str10,dash
               call FormatNumeric using str10,str13,comma
               if (ProjTOT < c0)
                       pack str14,dash,str13
               else
                   move str13 to str14
               endif
               prtpage prfile;*pcolumn3:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.==================================================================================================
.Difference
               clear str14
               clear str10
               clear str13
               Calc   PROJVAR=(NThisYrTOT-Projtot)                 
               move Projvar to str10
               call removechar using str10,dash
               call FormatNumeric using str10,str13,comma
               if (Projvar < c0)
                       pack str14,dash,str13
               else
                   move str13 to str14
               endif
           
               prtpage prfile;*pcolumn4:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.==================================================================================================
PUNBIL
               clear str14
               clear str10
               clear str13
               move NUNBILL to str10
               call removechar using str10,dash
               call FormatNumeric using str10,str13,comma
               if (NUNBILL < c0)
                       pack str14,dash,str13
               else
                   move str13 to str14
               endif
               prtpage prfile;*pcolumn5:row,*font=font8,*ALIGNMENT=*Right,*boldon,*ll,str14,*boldoff;
.===================================================================================
LastLine
               add     eightlpi,row
               add     eightlpi,row
                 prtpage prfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,"Total Records: ";
               clear str9
               move RESULT to str9
                 prtpage prfile;*font=font8,*ALIGNMENT=*Left,*ll,STR9;
               add     eightlpi,row
               add     "30" to row
                 prtpage prfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,*ll,"Note: Ranked by actual booked income.";
.=============================================================
.Footer for Last Page
Print2
                 move "9800",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,*ll,PgCnt;
                 prtpage prfile;*pcolumn8:row,*font=font9,*ALIGNMENT=*Right,"Names in the News"
.=================================================================================================

End1
        PRTCLOSE prfile
                        if (loopflg > c2)
                              goto ender
                        endif
                      if         (loopflg = c1)
                      Move      "Brokerage MTD Actual VS Projected PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      "SusanAnstrand@nincal.com,SuzieMcGuire@nincal.com",MailCC
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\Brokerage1.pdf",MailAttach
                      endif

                      if         (loopflg = c2)
                      Move      "Management MTD Actual VS Projected PDF File",MailSubjct
                      Move      "Creques@nincal.com",MailFrom
                      Move      "DavidHerrick@nincal.com",MailTo
                      Move      "SusanAnstrand@nincal.com,SuzieMcGuire@nincal.com",MailCC
                      Move      Str55,MailBody
                      MOve      "c:\work\pdf\Management1.pdf",MailAttach
                      endif

                      call       sendmail


                if (chkall = c1)
                        if (loopflg > c2)
                              goto ender
                        endif
                        goto restart
                  endif
                goto  Ender
.==========================================================================
FileGo
.Flag set to "N" if in Modify or New mode
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
.................................................................................





OrderSetMouseBusy
        setmode *mcursor=*wait
        return
OrderSetMouseFree
        setmode *mcursor=*arrow




        Shutdown      "CLS"
        stop
        return
Ender
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
