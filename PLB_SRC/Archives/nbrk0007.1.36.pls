PC      EQU       0
        include common.inc
        include cons.inc
        INCLUDE   NORDDD.inc
.START PATCH 1.2 REPLACED LOGIC
.        INCLUDE   NMLRDD.inc
.        include   nbrkdd.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
.END PATCH 1.2 REPLACED LOGIC
        include oslspern.inc
          include   Ncntdd.inc
*************************************************************************
Release    INIT    "1.36"               DLH      New Year
Reldate   Init      "03 January 2013"
.Release    INIT    "1.35"               DLH      New Year
.Reldate   Init      "03 January 2012"
.Release    INIT    "1.34"               DLH      Update Printers
.Reldate   Init      "07 July 2011"
.Release    INIT    "1.33"               DLH      Update
.Reldate   Init      "05 Oct 2009"
.Release    INIT    "1.32"               DLH        8Mar07 Oslspern expansion
.Release    INIT    "1.31"                       28APR06 2006 run
.Release    INIT    "1.3"                       10AUG04 ASH LOGO CONVERSION
.Release    INIT    "1.2"                       27MAY04 ASH MAILER CONVERSION
.Release    INIT    "1.1"                       ;Added Broker column to reports 1 and three(also added this release-sorted by client)
.RELEASE   INIT    "1.0"                       ;New Brokerage report which breaks out by client or Broker/Client
.
.Must be update Yearly along with nord0030 which created the salesref.dat
*************************************************************************
.begin patch 1.34
PRTFLAG   Dim       1
.end patch 1.34

.Previous Yr
.START PATCH 3.11
.START PATCH 1.35
.YEAR1    INIT   "10"
.begin patch 1.36
.YEAR1    INIT   "11"
YEAR1    INIT   "12"
.YEAR1    INIT   "04"
.Current Yr
.YEAR2    INIT   "11"
YEAR2    INIT   "13"
.end patch 1.36
.end PATCH 1.35
.YEAR2    INIT   "05"
.END PATCH 3.11
*************************************************************************
.Lets program no which year user select and which vars to use
YEARFLAG FORM    1
.Dummy Vars to use for both Years
NUMVAR   FORM    6
QTYVAR   FORM    9
.Key word for changed "Yearly"
INFILE   FILE
.Sort Parameters=======================================================
INDAT    init  "\\nins1\e\data\salesref.dat"   .File to be sorted
OUTSRT   init  "\\nins1\e\data\salesref.srt"   .Sorted Output file
Salesper dim   2

BrkSrt   init  "107-150,28-72"                .Broker,Client Sort
ClintSrt init  "28-72"                        .Sort by client #
.SORTVAR   INIT     "\\nins1\e\data\salesref.dat,\\nins1\e\data\salesref.srt;28-72,s=1='06'&73='0901'|1='06'&73='0900'"
SORTFLE  dim    70                            .Var to pack file names of sort
.temp
CSVfile   File          
.temp
.======================================================================
prfile  pfile
osls     dim       25
sales    dim       25
slsper   dim       26          .VAR USED TO LOAD SALESPERSON COMBO BOX
yr1                 dim       2           .GRABS PREVIOUS YEAR
yr2      dim       2           .GRABS CURRENT YEAR FROM TIMESTAMP
MOYR1    dim      4            .USED TO MATCH AGAINST PREVIOUS MO/YR IN FILE
MOYR2    dim      4            .USED TO MATCH AGAINST CURRENT MO/YR  IN FILE
HOLDCOMP dim      45           .HOLDCOMP COMPANY NAME OF PREVIOUS INPUT RECORD
HOLDCOMP2 dim     45          .HOLDCOMP COMPANY NAME OF PREVIOUS INPUT RECORD
HOLDBRK  dim      45
MO1      dim      2
.osflag   form     1            . 1=win 95,98, 2=NT
.=====================================================


OUTKEY   DIM      74     03-76
NUMNEW   FORM     6      74-82
QTYNEW   FORM     9      83-91
NUMOLD   FORM     6      92-97
QTYOLD   FORM     9      98-106
****************************************************
.brcomp   dim      45    107-150
***************************************************
ANS      DIM      1
DATE     DIM      8
DATEMASK DIM      8
SYSMO    DIM      2
SYSDY    DIM      2
SYSYR    DIM      2
LASTYY   DIM      2
.=============================================================
Title1   form     9
Title2   form     9
Title3   form     9
Title4   form     9
SROW     FORM     9            .ROW FOR SALES PERSON NAME
.============================================================
C23      FORM    "23"
Rowcount form    3             .KEEP TRACK OF ROW PER PAGE
PgCnt    form    9             .COUNT OF PAGES
NEWPG    FORM    1             .COUNTER TO SHOW- IF NEW PAGE PUT TOTALS AT TOP OF PAGE
FRSTFLG  form    1             .Flag to show if record is first of two poss records
Newsls   form    1             .Flag to start new page for new salesperson
ALLSLS   FORM    1             .FLAG TO SHOW THAT REPORT IS FOR ALL SALES PEOPLE
RPTFLAG  form    1
Font9    font
OYR1     init    "Orders "     .Must be changed Yearly  -For Titles
QYR1     init    "Qty "        .Must be changed Yearly  -For Titles
TITLEYR1 DIM      4            .TITLE FOR PREVIOUS YEAR
TITLEYR2 DIM      4            .TITLE FOR CURRENT YEAR
OTOT1     form     9           .Order total for previous YR
QTOT1     form     9           .QTY total for current YR
OTOT2     form     9           .Order total for current YR
QTOT2     form     9           .QTY total for current YR
TMPOTOT1   form     9          .Temp Order Total
TMPQTOT1   form     9          .Temp Order Total
TMPOTOT2   form     9          .Temp Order Total
TMPQTOT2   form     9          .Temp Order Total
.====================================================================================
.Lisview Object
BrokerListView Listview
White     color
Black     color
.Months of the Year
CurMo     dim   10
Month1    init  "January"
Month2    init  "February"
Month3    init  "March"
Month4    init  "April"
Month5    init  "May"
Month6    init  "June"
Month7    init  "July"
Month8    init  "August"
Month9    init  "September"
Month10   init  "October"
Month11   init  "November"
Month12   init  "December"
.====================================================================================
font15    font
          create  font15,"Times New Roman",size=16,bold
        create  font9,"Times New Roman",size=10,italic
START
************************************************************************************
.begin patch xxx
          call      GetWinVer

          clock               port to str3           .plb note
          unpack              str3 into str2,str1
          pack                str3 from str1,str2
          move                str3 to portN
          move      PORTN,NCNTFLD1
          rep       zfill,NCNTFLD1
          move      C3,NCNTPATH
          move      "NCNTKEY",Location
          pack      KeyLocation,"Key: ",NCNTFLD1
          call      NCNTKEY
          if        over
                    move      C2,CNTPRINT    .Laser 3
          endif
          move      CNTPRINT,PRTFLAG
.end patch xxx

.==================================================================
.Set up columns
        move    "100",column
        move    "3350",column1
        move    "4500",column2
        move    "5700",column3

        move    "2500",column4
        move    "5900",column5
        move    "6900",column6
        move    "9700",column7
.        move    "6900",column4
.       7860 row position of pg #
        move    "3000",Title1
        move    "6600",Title2
        move    "5600",Title3
        move    "4000",Title4
        MOVE    "520",SROW
          call paint

          CREATE White=*WHITE
          CREATE BLACK=*BLACK
        TRAP      EOJ IF F5
.==============================================================
.Create and Activate the ListView Object
ListV
.         CREATE BROKERLISTVIEW=1:1:1:1,SORTHEADER=1,SORTORDER=2:
          CREATE BROKERLISTVIEW=4:5:5:60,SORTHEADER=1,SORTORDER=2:
.         CREATE BROKERLISTVIEW=4:5:5:60,SORTHEADER=1:
               APPEARANCE=1:
                 BORDER=1,BGCOLOR=white:
                 TABID=5:
               FONT=FONT9,FGCOLOR=black:
               visible=1,ENABLED=1

          BROKERLISTVIEW.INSERTCOLUMN  USING "Quantity",120,0
          BROKERLISTVIEW.INSERTCOLUMN  USING "Orders",100,1
          BROKERLISTVIEW.INSERTCOLUMN  USING "Client",140,2
        BROKERLISTVIEW.INSERTCOLUMN  USING "Broker",100,3
.        BROKERLISTVIEW.INSERTCOLUMN  USING "",5,4
        ACTIVATE BROKERLISTVIEW
        clock   timestamp,str6
        unpack  str6,str4,str2
        move    str2 to MO1
        call    zfillit using str2
.Let them choose a year
CURYR
          Prepare   csvfile,"c:\work\LMVolume.dat",exclusive
        BEEP
        KEYIN   *P10:12,*DV,"Year for Report ",*cyan,"(1) ",*white,"'",*DV,YEAR1," OR ",*cyan," (2) ",*white,"'",*DV,YEAR2,"  ",*t20,str1
        move    str1 to YEARFLAG
        KEYIN   *P10:12,*DV,"Year for Report ",*cyan,"(1) ",*white,"'",*DV,YEAR1," OR ",*cyan," (2) ",*white,"'",*DV,YEAR2," ",*cyan,*DV,YEARFLAG,*white," OK ? ",*t20,ANS

        CMATCH  "N" TO ANS
        IF  EQUAL
                goto Curyr
.                   KEYIN   *P10:12,"Year for Report ",str4
        ENDIF
.        CMATCH  "N" TO ANS
        if ((YEARFLAG <> c1)&(YEARFLAG <> c2))
                  goto curyr
        endif
.        GOTO     CURYR IF EQUAL
.        MOVE     STR4 TO TITLEYR1
        LOAD     yr1 using yearflag,YEAR1,YEAR2
        pack      Titleyr1 with cc,yr1
.        unpack   str4,cc,yr1
DOALL
        move      "B" to str1
        KEYIN     *P10:12,*EF,*cyan,"(1)",*white," Broker Rpt By Volume ",*cyan,"(2)",*white,"Broker Rpt By Broker ",*cyan,"(3)",*white,"Brk Rpt By Clnt ",*T15,*RV,STR1
.        KEYIN     *P10:12,*EF,*cyan,"(1)",*white," Broker Rpt By Client ",*cyan,"(2)",*white,"Broker Rpt By Broker ",*T15,*RV,STR1
.        rep       "B1" in STR1
        move      str1 to rptflag
        if        ((rptflag < c1)|(rptflag > c3))
.        if        ((rptflag < c1)|(rptflag > c2))
                   goto EOJ
        endif
.        branch    rptflag to Broker1,Broker1
Broker1
        move      c6 to n2
        LOAD      str25 FROM N2 OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5,OSLS6:
                   OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13,OSLS14:
                   OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21,OSLS22,osls23,osls24,osls25:
                    osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
        move     n2 to str2
        move     str2 to Salesper
        rep      zfill in salesper
        goto Print11
Print11
        setprop brokerlistview,height=20
.        setprop brokerlistview,height=15
        clear Rowcount
        clear PgCnt
        clear NEWPG
        Clear OTOT1
        Clear QTOT1
        Clear OTOT2
        Clear QTOT2
SORTER
        pack   SortFle,indat,comma,outsrt
        branch    rptflag to Sorter1,Sorter2,sorter1
        DISPLAY   *P10:14,"Sorting..........                        "
SORTER1
.sort by Volume
                    pack   taskname,sortfle,";",CLINTSRT,comma,"s=1=","'",Salesper,"'","&75=","'",YR1,"'"
                goto   dosort
SORTER2
.sort by broker/client
                    pack   taskname,sortfle,";",BRKSRT,comma,"s=1=","'",Salesper,"'","&75=","'",YR1,"'"
                goto   dosort
DOSORT
         sort   taskname
         if over
               alert caution,S$ERROR$,result,"No Sort"
               goto  EOJ
         endif
.        pack      taskname from NTWKPATH2,"sort32 ","\\nins1\e\data\salesref.dat ","\\nins1\e\data\salesref.srt ","/s (28,45,alp,a)"," inc(1,2,n,eq,\'06\',and,(73,2,n,eq,\'09\'))"

         OPEN    INFILE,"\\nins1\e\data\salesref.srt"
         branch    rptflag to Looper1,Looper2,looper1

Looper1
.Read records and insertthem in the listview
         loop
                    READ  INFILE,SEQ;STR2,OSLS,MCOMP,OODTEM,OODTEY,NUMOLD,QTYOLD,NUMNEW,QTYNEW,brcomp
          until over
          DISPLAY   *P10:14,"Processing Records..........                        "
                LOAD  NUMVAR using yearflag,NUMOLD,NUMNEW
                LOAD  QTYVAR using yearflag,QTYOLD,QTYNEW
          Match MCOMP to HOLDCOMP
                  if not equal
                            Clear TMPOTOT1
                    Clear TMPQTOT1
                            add QTYVAR to TMPQTOT1
.                           add qtynew to TMPQTOT1
                    move  tmpqtot1 to str9
.                        rep   zfill in str9
                            BrokerListView.InsertItem giving  N7 using str9
                    add NUMVAR to TMPOTOT1
.                   add numnew to TMPOTOT1
                    move  tmpotot1 to str9
.                        rep   zfill in str9
.                        call trim using str9
                    brokerListView.SetItemText giving N8 using N7,str9,1
                            brokerListView.SetItemText giving N8 using N7,MCOMP,2
                            brokerListView.SetItemText giving N8 using N7,brCOMP,3
          else
.If a client has more than one record,delete entry and update to allow sorting to work correctly
                            brokerListView.DeleteItem using N7
                    add QTYVAR to TMPQTOT1
.                   add qtynew to TMPQTOT1
                    move  tmpqtot1 to str9
.                        rep   zfill in str9
                            BrokerListView.InsertItem giving  N7 using str9
.                           brokerListView.SetItemText giving N8 using N7,str9,0
                    add NUMVAR to TMPOTOT1
.                   add numnew to TMPOTOT1
                            move  tmpotot1 to str9
.                        rep   zfill in str9
.                        call trim using str9
                    brokerListView.SetItemText giving N8 using N7,str9,1
                            brokerListView.SetItemText giving N8 using N7,MCOMP,2
                            brokerListView.SetItemText giving N8 using N7,brCOMP,3
                  endif
                move mcomp to holdcomp
        repeat
        if (rptflag =c3)
.print setup is same as by volume only different sort
                brokerListView.SortColumn using *Column=2,*Type=1
                move c1 to rptflag
        endif
test
        goto    Print1

Looper2

.Printing all on one document
        clear   str40
        pack    str40,"c:\work\",str25,".lst"
        call    trim using str40
                    cALL      debug
                    if (PRTFLAG = "2")  .Sales
                              if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
                                        PRTOPEN   PRFile,"\\NINs2\Laser3 Blankstock",str25
                              elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
                                        PRTOPEN   PRFile,"Laser3 Blankstock",str25
                              elseif (osflag = "9" or osflag = "7")         .win 7  or vista
                                        PRTOPEN   PRFile,"\\NINs2\Laser3 Blankstock",str25
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   PRFile,"-",str25
                              endif
                    elseif (PRTFLAG = "3")                            .All others
                              if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
                                        PRTOPEN   PRFile,"\\NINs2\Laser2",str25
                              elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
                                        PRTOPEN   PRFile,"Laser2",str45
                              elseif (osflag = "9" or osflag = "7")         .win 7  or vista
                                        PRTOPEN   PRFile,"\\NINs2\Laser2",str25
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   PRFile,"-",str45
                              endif
                    Elseif     (PRTFLAg = "5" )      .Susan
                                        PRTOPEN   PRFile,"\\NIN0010\KYOCERAS",str25
                    Elseif (PRTFlag = "7")      .DH
                                        PRTOPEN prfile,"",str25
                    else
                              if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
                                        PRTOPEN   PRFile,"\\NINs2\Laser8",str25
                              elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
                                        PRTOPEN   PRFile,"Laser8",str45
                              elseif (osflag = "9" or osflag = "7")         .win 7  or vista
                                        PRTOPEN   PRFile,"\\NINs2\Laser8",str25
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   PRFile,"-",str25
                              endif
                    endif



        call Page
        move   "Brokerage" to holdbrk
        loop
                    READ  INFILE,SEQ;STR2,OSLS,MCOMP,OODTEM,OODTEY,NUMOLD,QTYOLD,NUMNEW,QTYNEW,BRCOMP
          until over
          DISPLAY   *P10:14,"Processing Records..........                        "
.file if built where old represents last year and new represents this year
.have to load vars according to which year it is
                LOAD  NUMVAR using yearflag,NUMOLD,NUMNEW
                LOAD  QTYVAR using yearflag,QTYOLD,QTYNEW
NextBRK
          Match BRCOMP to HOLDBRK
.                   Match str4 to HOLDBRK
                    goto PRINTLV2 if not equal
          Match MCOMP to holdcomp2
                  if not equal
.add to listview if new client
                            Clear TMPOTOT1
                    Clear TMPQTOT1
                            add QTYVAR to TMPQTOT1
.                           add qtynew to TMPQTOT1
                    move  tmpqtot1 to str9
.                        rep   zfill in str9
                            BrokerListView.InsertItem giving  N7 using str9
                    add NUMVAR to TMPOTOT1
.                   add numnew to TMPOTOT1
                    move  tmpotot1 to str9
.                        rep   zfill in str9
.                        call trim using str9
                    brokerListView.SetItemText giving N8 using N7,str9,1
                            brokerListView.SetItemText giving N8 using N7,MCOMP,2
          else
.If  a client has more than one record,delete entry and update to allow sorting to work correctly
                            brokerListView.DeleteItem using N7
                            add QTYVAR to TMPQTOT1
.                   add qtynew to TMPQTOT1
                    move  tmpqtot1 to str9
.                        rep   zfill in str9
                            BrokerListView.InsertItem giving  N7 using str9
.                           brokerListView.SetItemText giving N8 using N7,str9,0
                    add NUMVAR to TMPOTOT1
.                   add numnew to TMPOTOT1
                            move  tmpotot1 to str9
.                        rep   zfill in str9
.                        call trim using str9
                    brokerListView.SetItemText giving N8 using N7,str9,1
                            brokerListView.SetItemText giving N8 using N7,MCOMP,2
                  endif
                move mcomp to holdcomp2
        repeat
        goto printlv2



Print1
.Printing-Open
        clear   str40
        pack    str40,"c:\work\",str25,".lst"
        call    trim using str40
          call      debug
                    if (PRTFLAG = "2")  .Sales
                              if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
                                        PRTOPEN   PRFile,"\\NINs2\Laser3 Blankstock",str25
                              elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
                                        PRTOPEN   PRFile,"Laser3 Blankstock",str25
                              elseif (osflag = "9" or osflag = "7")         .win 7  or vista
                                        PRTOPEN   PRFile,"\\NINs2\Laser3 Blankstock",str25
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   PRFile,"-",str25
                              endif
                    elseif (PRTFLAG = "3")                            .All others
                              if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
                                        PRTOPEN   PRFile,"\\NINs2\Laser2",str25
                              elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
                                        PRTOPEN   PRFile,"Laser2",str45
                              elseif (osflag = "9" or osflag = "7")         .win 7  or vista
                                        PRTOPEN   PRFile,"\\NINs2\Laser2",str25
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   PRFile,"-",str45
                              endif
                    Elseif     (PRTFLAg = "5" )      .Susan
                                        PRTOPEN   PRFile,"\\NIN0010\KYOCERAS",str25
                    Elseif (PRTFlag = "7")      .
                                        PRTOPEN prfile,"",str25
.                                        PRTOPEN prfile,"\\NIN0100\Kyoceram",str25
                    else
                              if (OSFLAG = "1" or OSFLAG = "5" or OSFLAG = "6")  .NT4,NT5,XP
                                        PRTOPEN   PRFile,"\\NINs2\Laser8",str25
                              elseif (OSFLAG = "3" or OSFLAG = "4")             .95/98
                                        PRTOPEN   PRFile,"Laser8",str45
                              elseif (osflag = "9" or osflag = "7")         .win 7  or vista
                                        PRTOPEN   PRFile,"\\NINs2\Laser8",str25
                              else   .(osflag = c0)         .Don't know prompt for printer
                                        PRTOPEN   PRFile,"-",str25
                              endif
                    endif

        call Page
        goto PrintLV1
.============================================================================================
Page
.Defining Page
        CLEAR     ROWCOUNT
        ADD       C1 TO PGCNT
        prtpage   prfile;*NEWPAGE:
                   *UNITS=*HIENGLISH;
.======================================================================
        clear     row
        move      "300",row
.======================================================================
        prtpage prfile;*pTitle2:row,*ALIGNMENT=*Left,*font=font12,"Date: ";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*ll,*font=font12,str10;
        prtpage prfile;*pTitle4:row,*font=font12,*ALIGNMENT=*CENTER,*boldon,*ll,*ULON,"List Management Broker Report",*ULOFF,*boldoff;
        add     eightlpi,row
        add     "60",row
        if      (rptflag = c1)
          prtpage prfile;*pTitle4:row,*font=font12,*ALIGNMENT=*CENTER,*boldon,*ll,*ULON,"By Client",*ULOFF,*boldoff;
        endif
        if      (rptflag = c2)
                  prtpage prfile;*pTitle4:row,*font=font12,*ALIGNMENT=*CENTER,*boldon,*ll,*ULON,"By Broker",*ULOFF,*boldoff;
        endif
.        if      (rptflag = c3)
.         prtpage prfile;*pTitle4:row,*font=font12,*ALIGNMENT=*CENTER,*boldon,*ll,*ULON,"By Client",*ULOFF,*boldoff;
.                move c1 to rptflag
.        endif
        add     eightlpi,row
        add     eightlpi,row
        if          (rptflag = c1)
                  prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Broker",*ULOFF,*boldoff;
.                 prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Client",*ULOFF,*boldoff;
        endif
        if          (rptflag = c1)
                  prtpage prfile;*pColumn4:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Client",*ULOFF,*boldoff;
        endif

        CLEAR   STR11
        PACK    STR11,OYR1,TITLEYR1
        if          (rptflag = c1)
                  prtpage prfile;*pColumn5:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,*ll,STR11,*ULOFF,*boldoff;
        else
                  prtpage prfile;*pColumn1:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,*ll,STR11,*ULOFF,*boldoff;
        endif
        CLEAR   STR8
        PACK    STR8,QYR1,TITLEYR1
        if          (rptflag = c1)
                  prtpage prfile;*pColumn6:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,*ll,STR11,*ULOFF,*boldoff;
        else
                  prtpage prfile;*pColumn2:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,*ll,STR8,*ULOFF,*boldoff;
        endif
        add     eightlpi,row
        add     "20",row
.        move  osls to sales
.        call trim using sales
.        prtpage prfile;*p4000:SROW,*font=font12,*ALIGNMENT=*CENTER,*ll,sales;
        return


PrintLV1
        DISPLAY   *P10:14,"Printing Records from Listview Obj..........                        "
        BrokerListView.GetItemCount giving result
        sub c1 from result
        for n9,"0",result
          BrokerListView.GetItemText giving str45 using n9,2
.         BrokerListView.GetItemText giving mcomp using n9,2
        Match str45 to HOLDCOMP
.        Match MCOMP to HOLDCOMP
        if not equal
                    add     eightlpi,row
                    add     "35",row
.Broker
                clear brcomp
                    BrokerListView.GetItemText giving brcomp using n9,3
                prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,brcomp;
.Client
                move str45 to str35
                prtpage prfile;*pcolumn4:row,*font=font12,*ALIGNMENT=*Left,str35;
.                prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,str45;
.                prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,MCOMP;
                clear str9
                    BrokerListView.GetItemText giving str9 using n9,1
                  move str9 to numnew
                    call trim using str9
                prtpage prfile;*pcolumn5:row,*font=font12,*ALIGNMENT=*Left,str9;
.                prtpage prfile;*pcolumn1:row,*font=font12,*ALIGNMENT=*Left,str9;
                clear str9
                    BrokerListView.GetItemText giving str9 using n9,0
                  move str9 to qtynew
                  prtpage prfile;*pcolumn6:row,*font=font12,*ALIGNMENT=*Left,str9;
.                 prtpage prfile;*pcolumn2:row,*font=font12,*ALIGNMENT=*Left,str9;
          add c1 to RowCount
                add NUMNEW to OTOT1
                add QTYNEW to QTOT1
.temp
          WRite     Csvfile,seq;Brcomp,str35,NumNew,QtyNew
.temp
        endif
                move str45 to holdcomp
.                move mcomp to holdcomp
        if (ROWCOUNT = "48" | ROWCOUNT = "49")
                 move "10200",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.3 REPLACED LOGIC
.                prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News/CA";
                 prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News";
.END PATCH 1.3 REPLACED LOGIC
                 call Page
        endif
.                goto row2
        repeat
        goto    EndLoop
PrintLV2
.extract items from listview for printing and create running totals for broker
        DISPLAY   *P10:14,"Printing Records from Listview Obj..........                        "
        if (holdbrk = "Brokerage")
                move BRCOMP to holdbrk
                goto NextBrk
        endif
        clear   OTOT1
        clear   QTOT1
.Just grab first Record and Use its broker
        BrokerListView.GetItemCount giving result
..        if (result = c0)
. .               goto End1
.        endif
..        BrokerListView.GetItemText giving str45 using c0,3
.        PACK   NBRKFLD,str4,z3
.        rep    zfill in nbrkfld
.        call   NBRKKEY
.       get brcomp
.         add     eightlpi,row
.         add     "35",row
        add         c1 to rowcount
        prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,*ll,*ULON,*boldon,HoldBRK,*ULOFF;
        BrokerListView.GetItemCount giving result
        sub c1 from result
        for n9,"0",result
          BrokerListView.GetItemText giving str45 using n9,2
.         BrokerListView.GetItemText giving mcomp using n9,2
        Match str45 to HOLDCOMP
.        Match MCOMP to HOLDCOMP
        if not equal
                    add     eightlpi,row
                    add     "50",row
                prtpage prfile;*pcolumn:row,*font=font12,*ll,*ALIGNMENT=*Left,str45;
.                prtpage prfile;*pcolumn:row,*font=font12,*ll,*ALIGNMENT=*Left,MCOMP;
                clear str9
                    BrokerListView.GetItemText giving str9 using n9,1
                  move str9 to numnew
                    call trim using str9
                prtpage prfile;*pcolumn1:row,*font=font12,*ll,*ALIGNMENT=*Left,str9;
                clear str9
                    BrokerListView.GetItemText giving str9 using n9,0
                  move str9 to qtynew
                  prtpage prfile;*pcolumn2:row,*font=font12,*ll,*ALIGNMENT=*Left,str9;
          add c1 to RowCount
                add NUMNEW to OTOT1
                add QTYNEW to QTOT1
        endif
                move str45 to holdcomp
.                move mcomp to holdcomp
.        goto    EndLoop

Row2
       if (ROWCOUNT > "47")
                 move "10200",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
.START PATCH 1.3 REPLACED LOGIC
.                prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News/CA";
                 prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News";
.END PATCH 1.3 REPLACED LOGIC
                 call Page
       endif
       repeat

Subtot2
       if (ROWCOUNT < "47")
                goto Totals2
       else
.Added to correct page # and page label for next to last page
.==========================================================================================
                 move "10200",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,*ll,PgCnt;
.START PATCH 1.3 REPLACED LOGIC
.                prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News/CA";
                 prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News";
.END PATCH 1.3 REPLACED LOGIC
.===========================================================================================
               move c1 to newpg
               call   Page
.               add  c1 to pgcnt
.               prtpage prfile;*NEWPAGE:
.                              *UNITS=*HIENGLISH:
.                                   *ORIENT=*PORTRAIT;
               goto Totals2
       endif
Totals2
.Totals for report by Broker
                add     eightlpi,row
                add     "50" to row
                  add         c1 to rowcount
                prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*ULON,"Total",*ULOFF;
                  move OTOT1 to str9
                    call trim using str9
                prtpage prfile;*pcolumn1:row,*font=font12,*boldon,*ll,*ALIGNMENT=*Left,str9;
                clear str9
                  move QTOT1 to str9
                  prtpage prfile;*pcolumn2:row,*font=font12,*boldon,*ll,*ALIGNMENT=*Left,str9;
.Delete old listview and put new broker in holdbrk
                BROKERLISTVIEW.DELETEALLITEMS
                move BRCOMP to HOLDBRK
                add     eightlpi,row
                add     "50" to row
                clear  holdcomp
                if (HOLDBRK <> "")
                    goto NextBRK
                endif
                goto Print2
.===================================================================
ENDLOOP
                    WEOF     Csvfile,Seq
                    Close     Csvfile

       if (ROWCOUNT < "46")
                 goto totals1
       else

.Added to correct page # and page label for next to last page
.==========================================================================================
                 move "10200",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,*ll,PgCnt;
.START PATCH 1.3 REPLACED LOGIC
.                prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News/CA";
                 prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News";
.END PATCH 1.3 REPLACED LOGIC
.===========================================================================================
               move c1 to newpg
               add  c1 to pgcnt
               prtpage prfile;*NEWPAGE:
                              *UNITS=*HIENGLISH:
                                    *ORIENT=*PORTRAIT;
               goto Totals1
       endif

TOTALS1
.Totals for report by client
       if (newpg = c1)
                 move srow,row
       endif
               add     eightlpi,row
               add     "50" to row
               add     "30" to row
               prtpage prfile;*pColumn:row,*font=font12,*ALIGNMENT=*Left,*boldon,*ULON,"Total";
               clear str9
               move OTOT1 to str9
               call trim using str9
               prtpage prfile;*pcolumn5:row,*font=font12,*ALIGNMENT=*Left,*boldon,str9,*boldoff;
.               prtpage prfile;*pcolumn1:row,*font=font12,*ALIGNMENT=*Left,*boldon,str9,*boldoff;
               clear str9
               move QTOT1 to str9
               call trim using str9
               prtpage prfile;*pcolumn6:row,*font=font12,*ALIGNMENT=*Left,*boldon,str9,*boldoff;
.=============================================================
.Footer for Last Page
Print2
                 move "10200",row
                 prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
                 prtpage prfile;*font=font12,*ALIGNMENT=*Left,*ll,PgCnt;
.START PATCH 1.3 REPLACED LOGIC
.                prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News/CA";
                 prtpage prfile;*pTitle2:row,*font=font9,*ALIGNMENT=*Left,"Names in the News";
.END PATCH 1.3 REPLACED LOGIC
               PRTCLOSE prfile
                        if (osflag = c2)
                                      DISPLAY   *P10:14,"And I'm Spent..........                        "
                                Pause     c2
.                                        PRTPLAY str40,"\\NINs2\Laser8"
.                                erase str40
                        else
                                      DISPLAY   *P10:14,"And I'm Spent..........                        "
                                Pause     c2
.                                        PRTPLAY str40,"Laser8"
.                                erase str40
                        endif
                              DISPLAY   *P10:14,"Done Printing..........                        "
                        Pause     c2
                              DISPLAY   *P10:14,"GoodBye..........                        "
************************************************************************************


EOJ      STOP

         INCLUDE   NORDIO.inc
.START PATCH 1.2 REPLACED LOGIC
.         INCLUDE   NMLRIO.inc
.         INCLUDE   nbrkio.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
          include   Ncntio.inc
.END PATCH 1.2 REPLACED LOGIC
         INCLUDE   COMLOGIC.inc

