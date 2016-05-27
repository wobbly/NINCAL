.color="##2858a6"
.Latest 02/25/04 with AH's changes.

PC EQU 0
          INCLUDE   common.inc
          INCLUDE   cons.inc
          include   ndatdd.inc
          include   nseldd.inc
          include   nrefdd.inc
.         include   nmoddd.inc
          include   nadddd.inc
          include   narrdd.inc
          include   ncatdd.inc
          include   NSLTdd.inc
          include   nsrcdd.inc
          include   nmoddd.inc
          include   ntxtdd.inc
          include   nowndd.inc
yesno1    integer   1,"0x000024"                yes no buttons, question Icon
.START PATCH 1.6 REPLACED LOGIC
.         include   nfuldd.inc
          include compdd.inc
          include cntdd.inc
.END PATCH 1.6 REPLACED LOGIC
.START PATCH 1.2.1 ADDED LOGIC
          include   ndatwdd.inc
.END PATCH 1.2.1 ADDED LOGIC
.begin patch 1.94
          include   ntxt1dd.inc
.end patch 1.94

DATACARDPRINT       PFILE
DATCARD   FILE
.;Patch1.1.2
OwnHtmlFile File
.;Patch1.1.2
.PRTFILENAME        DIM       50
.PRINTNAME          DIM       50
CARR           INIT    0x7f
SelectListView                listview
SelectAListView               listview
SelectBListView               listview
Select2ListView               listview
Select2AListView    listview
Select2BListView    listview
Select3ListView               listview
Select4ListView               listview
Select4aListView    listview
Select5ListView               listview
Select5aListView    listview
Select6ListView               listview
Select6AListView    listview

White     color
Black     color

Release   INit      "1.98"    DLH   reciprocal verbage
Reldate   INit      "2013 October 24"
.Release   Init      "1.97"   DLH   Move Tertiary selects under Base Selects
.REldate   INit      "2013 September 03"
.Release   Init      "1.96"   DLH   .sunbelt pdf .. update plb on webserver before rolling out the code
.REldate   INit      "2013 April 22"
.Release   Init      "1.95"   DLH   .more IO traps for webserver
.REldate   INit      "25 January 2013"
.Release   Init      "1.94"   DLH   .SRDS
.REldate   INit      "20 September 2011"
.Release   Init      "1.93"   DLH   change colors on HTML to match new site
.REldate   INit      "09 July 2010"
.  text   #777777 now #2858a6

.Release   INIT      "1.92"   DLH  Stop printing "exclusive"
.Reldate   Init      "18 December 2009"
.Release   INIT      "1.91"   DLH  Enable google tracking
.Reldate   Init      "17 March 2009"
.Release   INIT      "1.9"   DLH  occasionaly error calling newpage (file is no longer open)
.Reldate   Init      "09 March 2009"
.Release  INIT      "1.8.4"   DLH Minimums
.Reldate  Init      "11 April 2008"
.Release  INIT      "1.8.3"   DLH cancellation fee
.Reldate  Init      "9 April 2008"
.Release  INIT      "1.8.1"   25JULY2007          DLH Cleannup Added logic for PLI
.Release INIT "1.8" 17JULY2007          ASH  Added logic for PLI
.Release INIT "1.7"  1Nov2006 Min Conversion
.Release INIT "1.6"  21JUN2006 Fulfillment Conversion
.Release INIT "1.5"  24MAR2006 Patched List Owner snafu
.Release INIT "1.4"  27FEB2006 Added Code for Usage Creation for Web Run Use Inits
.Release INIT "1.2.1"  07FEB2006 ASH Added Index file so that Datacard search on ninlists does not need to track modifications for html upload criteria
.Release INIT "1.2.0"  30JAN2006 DMB Moved location of gif for internal web card
.Release INIT "1.1.9" 28SEP2005 ASH Small Changes in format.
.Release INIT "1.1.8" 27MAY2005 ASH Small Changes in format.
.Release INIT "1.1.7" 07APR2005 ASH COMMPER Conversion
.Release INIT "1.1.6" 25OCT2004 DMB Added Code to not index pages
.Release INIT "1.1.5" 08OCT2004 DMB Added Code to for new background
.Release INIT "1.1.4" 10AUG2004 ASH Logo Conversion
.Release INIT "1.1.3" 04MAY2004 DMB Added code to show owner info for datacard program
.Release INIT "1.1.2" 04MAY2004 DMB Added code to show owner info for datacard program
.Release INIT "1.1.1" 22APR2004 ASH Small fix to "Sex:" label
.Release INIT "1.1" 13APR2004 change lookup for selects
.Release INIT "1.0" 16JAN2004 Begin Data Card Creation
SelectTestBase4 external "NDAT001a;SelectTestBase4"
SingleSpaced        FORM      "180"
.OneandahalfSpaced  FORM      "270"
.DoubleSpaced       FORM      "320"
LgBoxHeight         FORM      "9900"
SmBoxHeight         FORM      "350"
.START PATCH 1.1 ADDED LOGIC
DimPtr    Dim       ^
PrintFlag FORM      1
.END PATCH 1.1 ADDED LOGIC
.START PATCH 1.84
REntFlag  Dim       1         .'Y' if anypart of list is rental
.End PATCH 1.84

MASK13    INIT      "Z,ZZZ,ZZZ,ZZZ"
DIM13a    DIM       13
DIM9a     DIM       13
.hold2    DIM       4500
hold2     DIM       7500
line1     dim       46
.line1    dim       55
RefLine1  DIM       22
RefLine2  DIM       36
LASTLINE  FORM      "9400"
.LASTLINE FORM      "8700"
.Flags   1= Not Begun 2 = Not Finished 3 = Completed
ArrangementFlag     INIT      "1"
AddressingFlag INIT           "1"
SourceFlag                    INIT      "1"
SelectionsFlag INIT           "1"
FormatFlag                    INIT      "1"
SelectFlag1                   INIT      "1"
SelectFlag2                   INIT      "1"
TextFlag                      INIT      "1"
PageNum                       FORM       5
OwnerFlag           INIT      "N"
ENDDetailFlag       INIT  "N"
SELNOTESFlag        INIT      "N"                 .if there are notes for select then write message
#n4       FORM       4
#n41      FORM       4
#n8       FORM       8
#n9       FORM       9
#n1       FORM       1
#n3       FORM       3
#result   FORM       9
#str3     DIM        3
num9      FORM       9
#n2       FORM       2
#n2a      FORM       2
#str25    dim       25
#LV       FORM      4         .per thousand charge
#NUMLV    FORM      4
#LVA      FORM      4         .Universe
#NUMLVA   FORM      4
#LVB      FORM      4         .per thousand charge
#NUMLVB   FORM      4
#LV2A     FORM      4         .Universe
#NUMLV2A FORM       4
#LV2B     FORM      4         .per thousand charge
#NUMLV2B FORM       4
#LV3      FORM      4
#NUMLV3   FORM      4
#LV4      FORM      4
#NUMLV4   FORM      4
#NUMLV4A FORM       4
#LV5      FORM      4
#NUMLV5   FORM      4
#NUMLV5A FORM       4
#LV6      FORM      4
#NUMLV6   FORM      4
#NUMLV6A FORM       4
UNINUM    FORM      4
UNINUM1   FORM      4
UNINUM2   FORM      4
.CRLF     INTEGER   1,"0x07F"
N52       form      5.2
SectionOne          INIT      "N"
SectionTwo          INIT      "N"
b20       init      "                    "
HTMLFILE  FILE
HTMLFILE2 FILE
TABLEFLAG1          DIM       1
TABLEFLAG2          DIM       1
TABLEFLAG3          DIM       1
TABLEFLAG4          DIM       1
TABLEFLAG5          DIM       1
TABLEFLAG6          DIM       1
TABLEFLAG7          DIM       1
TABLEFLAG8          DIM       1
TABLEFLAG9          DIM       1
TABLEFLAG10         DIM       1
.;
DIMTEXT1  DIM       46
DIMTEXT2  DIM       46
DIMTEXT3  DIM       46
DIMTEXT4  DIM       46
DIMTEXT6  DIM       4

DIMTEXT5  DIM       46
RECCOUNT  FORM      5
SkipCOUNT FORM      5
.START PATCH 1.1.9 ADDED LOGIC
externalmode        integer   1
.END PATCH 1.1.9 ADDED LOGIC
.START PATCH 1.1 ADDED LOGIC
          call      CreateObjects
.END PATCH 1.1 ADDED LOGIC
.str75     DIM       75

.>Patch 1.4         Variables added
UsagePrint          PFILE
PrtFileName         DIM       50
.begin patch 1.96
.PrintName           DIM       50
PrintName           DIM       60
.end patch 1.96
UsageFlag           form      1
PrintUsageToPFile external "NUSG002B;PrintUsageToPFile"
.START PATCH 1.97 ADDED LOGIC
Tertbase  Dim       1         Tertiary base = Y
.END PATCH 1.97 ADDED LOGIC

.>Patch 1.4
.---------------------------------------------------------------------------------------------------
BEGIN     CMATCH    B1 TO PROGRAM             .CHAINED FROM DSINIT?
          IF EOS                       .NO
                    MOVE      "NDAT002W" TO PROGRAM
                    MOVE      "Names In The News Ca" TO COMPNME
                    MOVE      "EXIT" TO PF5
          ENDIF
          MOVE      "WEB DATACARD" TO STITLE
          CALL      PAINT
.START PATCH 1.1 ADDED LOGIC
          move      C0,PrintFlag
.END PATCH 1.1 ADDED LOGIC
.START PATCH 1.2.1 ADDED LOGIC
//Clean up pre-existing Web Datacard records
          move      C1,NDATWPATH
          move      C1,N6
          Prepare   Ndatwfile,"\\nins1\e\data\text\nindatw.dat","e:\data\index\nindatw.isi|10.10.30.103:502","1-6","20" 
.          loop
.                    move      N6,NDATWFLD
.                    rep       zfill,NDATWFLD
.                    move      "NDATWKEY",Location
.                    pack      KeyLocation,"Key: ",NDATWFLD
.                    call      NDATWKEY
.                    until not over
.                    if (N6 = "999999")
.                              move      C0,N6
.                              break
.                    endif
.                    add       C1,N6
.          repeat
.          if (N6 <> C0)
.          //There are valid records in the file.
.                    loop
.                    //Delete them all
.                              move      "NDATWDEL",Location
.                              call      NDATWDEL
.                              move      "NDATWKS",Location
.                              call      NDATWKS
.                              until over
.                    repeat
.          endif
.END PATCH 1.2.1 ADDED LOGIC
.---------------------------------------------------------------------------------------------------
          DISPLAY   *P01:05,"Options     :":
                    *P01:06,"Input File  :",INPNAME:
                    *P01:09,"Record Count:":
                    *P01:12,"Count:";
.;
.;                  move      C1,PrintFlag
.;                  move      C1,NDATPATH
.;                  packkey   NDATFLD,"002948"
.;                  rep       zfill,NDATFLD
.;                  move      "PrintWeb-NDATKEY",Location
.;                  pack      KeyLocation,"Key: ",NDATFLD
.;                  call      NDATKEY
.;                  if not over
.;                            call      CreateObjects
.;                            call      clearvars
.                             move      c1 to NDATCONV
.;                            CALL      createpage
.;                            call      Arrangement
.;                            destroy   White
.;                            destroy   BLACK
.;                            destroy   selectlistview
.;                            destroy   select2listview
.;                  endif
.;
          OPEN      DatCard,INPNAME
.         OPEN      DatCard,"c:\work\datatest.dat"
          move      c1,ndatpath
ReadRecord
.START PATCH 1.1.9 ADDED LOGIC
          if (externalmode)
                    return
          endif
.END PATCH 1.1.9 ADDED LOGIC
          if (pagenum <> c0)                      .Is this the the first page?  If not add links to end of page                                       weof      htmlfile,seq
                    weof      htmlfile,seq
                    close     htmlfile
                    CALL ADDLINKS
          endif
.START PATCH 1.1 ADDED LOGIC
          if (PrintFlag = 1)
                    return
          endif
.END PATCH 1.1 ADDED LOGIC
          call      clearvars
REadAgain
          read      datcard,seq;DATVARS
          goto EndDataRun if over
.
.         packkey   NDATFLD,lstnum
.         call ndatkey
          ADD       "1",RECCOUNT
          if (PrintFlag <> 1)
                    if (NDATOFF = "1")            if an office use only card do not put on the web.
                              ADD       "1",SKIPCOUNT
                              DISPLAY   *P15:12,SKIPCOUNT,b2,lstnum;
                              Goto readagain
.patch1.1.3
                    elseif (NDATWEB = "1")
                              ADD       "1",SKIPCOUNT
                              DISPLAY   *P15:12,SKIPCOUNT,b2,lstnum;
                              Goto readagain
.patch1.1.3
                    else
                              move      mlstname to str75
                              rep       uplow in str75
                              scan      "office use" in str75
                              if equal
                                        ADD       "1",SKIPCOUNT
                                        DISPLAY   *P15:12,SKIPCOUNT,b2,lstnum;
                                        goto readagain
                              else
                                        DISPLAY   *P15:9,RECCOUNT;
                              endif
                    endif
          endif

.         move      lstnum to NDATFLD
.         call      ndatkey
          CALL      createpage
.--------------------------------------------------------------------------------------------------------------------------------
Arrangement
.ListView3
          move      #N9 to row                                                  .#n9 was gien from First Header to Designate First Row
          Call CreateTable3
          add       SingleSpaced to Row
.ARRANGEMENT
          pack      NARRFLD1,"01X",LSTNUM
          move      "D.Load-NARRAIM",Location
          pack      KeyLocation,"Key: ",NARRFLD1
          call      NARRAIM
          if over
                    MOVE      "3" to ArrangementFlag                            .We are done with arrangement
                    Goto Addressing
          endif
          loop
          until over
                    call      DataLoadRefArrangement
                    call      trim using nrefdesc
                    movelptr NREFDESC to n7
                    loop
                              clear     Refline1
                              call      PARSITUP using Refline1,NREFDESC,C1
                              movefptr NREFDESC to n6
                              if (n6 >= n7)
                                        if (REFLINE1 <> "")
                                                  select3listview.InsertItem giving N8 using REFLINE1
                                        endif
                              endif
                    until (n6 >= n7)
                              select3listview.InsertItem giving N8 using REFLINE1
                    repeat
                    move      "D.Load-NARRKG",Location
                    pack      KeyLocation,"Key: ",NARRFLD1
                    call      NARRKG
          repeat
          Select3listview.GetItemCount giving #LV3
          sub       c1 from #LV3
          compare   c0,#LV3
          if less
                    MOVE      "3" to ArrangementFlag                            .We are done with arrangement
                    Goto Addressing
          else
                    Write               htmlfile,seq;b20,"<TR>"
                    Write               htmlfile,seq;b20,"<TD VALIGN=TOP ALIGN=CENTER><B><PRE>***ARRANGEMENT***</PRE></B></TD>"
                    Write               htmlfile,seq;b20,"</TR>"
                    add SingleSpaced to Row
                    Write     htmlfile,seq;b20,"<TR>"
                    Write     htmlfile,seq;b20,*LL,"<TD WIDTH=225>"
                    Write     htmlfile,seq;"<P><PRE>"
                    for #NUMLV3,"0",#LV3
                              Select3listview.GetItemText giving REFLINE1 using #NUMLV3,0
                              Write     htmlfile,seq;REFLINE1
                              add SingleSpaced to Row
                              if (ROW   >= LASTLINE)
                                        MOVE      "2" to ArrangementFlag
                                        Write     htmlfile,seq;"</PRE></P>"
                                        Write     htmlfile,seq;b20,*LL,"</TD>"
                                        Write     htmlfile,seq;b20,"</TR>"
.This will Close the left section of the datacard which contains addressing, arrangement,source,selection
.Note:  This Close Table will NOT be used with the arrangement routine when it gets an OVER on the first read
.closes table 2 and 3 and ends the column - which signifies that the text will be written next if there is any
                                        Goto      CloseUpLeftTable
                              endif
CONTINUEARRANGEMENT
                    repeat
                    Write     htmlfile,seq;"</PRE></P>"
                    Write     htmlfile,seq;b20,*LL,"</TD>"
                    Write     htmlfile,seq;b20,"</TR>"
          endif
          MOVE      "3" to ArrangementFlag
.--------------------------------------------------------------------------------------------------------------------------------
Addressing
.ListView4
          add SingleSpaced to Row
          if (ROW   >= LASTLINE)
                              goto      CloseUpLeftTable
          endif
          pack      NADDFLD1,"01X",LSTNUM
          move      "D.Load-NADDAIM",Location
          pack      KeyLocation,"Key: ",NADDFLD1
          call      NADDAIM
          if over
                    MOVE      "3" to AddressingFlag                             .We are done with Addressing
                    Goto Source
          endif
.
          loop
          until over
                    call      DataLoadRefAddressing
                    call      trim using nrefdesc
                    movelptr NREFDESC to n7
                    loop
                              clear     Refline1
                              call      PARSITUP using Refline1,NREFDESC,C1
                              movefptr NREFDESC to n6
                              if (n6 >= n7)
                                        if (REFLINE1 <> "")
                                                  select4listview.InsertItem giving N8 using REFLINE1
                                                  select4Alistview.InsertItem giving N8 using str25
                                        endif
                              endif
                    until (n6 >= n7)
                              select4listview.InsertItem giving N8 using REFLINE1
                              select4Alistview.InsertItem giving N8 using str25
                              clear str25
                    repeat
                              move      "D.Load-NADDKG",Location
                              pack      KeyLocation,"Key: ",NADDFLD1
                    call      NADDKG
          repeat
          Select4listview.GetItemCount giving #LV4
          sub       c1 from #LV4
          compare   c0,#LV4
          if less
                    MOVE      "3" to AddressingFlag                             .We are done with Addressing
                    Goto Source
          else
                    Write     htmlfile,seq;b20,"<TR>"
                    Write     htmlfile,seq;b20,"<TD VALIGN=TOP ALIGN=CENTER><B><PRE>****ADDRESSING****</PRE></B></TD>"
                    Write     htmlfile,seq;b20,"</TR>"
                    if (TableFlag3 = NO)
                              call      CreateTable3
                              call      CreateTable4
                    else
                              call      CreateTable4
                    endif
                    Write     htmlfile,seq;b20,"<TR>"
                    Write     htmlfile,seq;b20,*LL,"<TD WIDTH=225>"
                    Write     htmlfile,seq;"<P><PRE>"
                    for #NUMLV4,"0",#LV4
                              Select4listview.GetItemText giving NREFDESC using #NUMLV4,0
                              Write     htmlfile,seq;*LL,NREFDESC
                              add SingleSpaced to Row
                              if (ROW   >= LASTLINE)
                                        MOVE      "2" to AddressingFlag
.This will Close the left section of the datacard which contains addressing, arrangement,source,selection
.Note:  This Close Table will NOT be used with the arrangement routine when it gets an OVER on the first read
.closes table 2 and 3 and ends the column - which signifies that the text will be written next if there is any
                                        Write     htmlfile,seq;"</PRE></P>"
                                        Write     htmlfile,seq;b20,*LL,"</TD>"
                              if (#LV4 = #NUMLV4)
                                        add c1 to #NUMLV4
                              endif
                                        Call LoadAddressingMod
                                        Write     htmlfile,seq;b20,"</TR>"
                                        goto      CloseUpLeftTable
                              endif
CONTINUEADDRESSING
                    repeat
                    Write     htmlfile,seq;"</PRE></P>"
                    Write     htmlfile,seq;b20,*LL,"</TD>"
                    Call LoadAddressingMod
                    Write     htmlfile,seq;b20,"</TR>"
                    call CloseTable4
          endif
          move      "3" to AddressingFlag
.--------------------------------------------------------------------------------------------------------------------------------
SOURCE
.ListView5
          add SingleSpaced to Row
          if (ROW   >= LASTLINE)
                    Goto      CloseUpLeftTable
          endif
.SOURCE
          pack      NSRCFLD1,"01X",LSTNUM
          move      "D.Load-NSRCAIM",Location
          pack      KeyLocation,"Key: ",NSRCFLD1
          call      NSRCAIM
          if over
                    MOVE      "3" to SourceFlag                       .We are done with Source
                    Goto Selections
          endif
          loop
          until over
                    call      DataLoadRefSource
                    call      trim using nrefdesc
                    movelptr NREFDESC to n7
                    clear n1
                    loop
                              clear     Refline1
                              call      PARSITUP using Refline1,NREFDESC,C1
                              movefptr NREFDESC to n6
                              if (n6 >= n7)
                                        if (REFLINE1 <> "")
                                                  select5listview.InsertItem giving N8 using REFLINE1
                                                  select5Alistview.InsertItem giving N8 using str4
                                        endif
                              endif
                    until (n6 >= n7)
                              select5listview.InsertItem giving N8 using REFLINE1
                              select5Alistview.InsertItem giving N8 using str4
                              clear     str4
                    repeat
                    move      "D.Load-NSRCKG",Location
                    pack      KeyLocation,"Key: ",NSRCFLD1
                    call      NSRCKG
          repeat
          Select5listview.GetItemCount giving #LV5
          sub       c1 from #LV5
          compare   c0,#LV5
          if less
                    MOVE      "3" to SourceFlag                       .We are done with Source
                    Goto Selections
          else
                    Write     htmlfile,seq;b20,"<TR>"
                    Write     htmlfile,seq;b20,"<TD VALIGN=TOP ALIGN=CENTER><B><PRE>****SOURCE****</PRE></B></TD>"
                    Write     htmlfile,seq;b20,"</TR>"
                    if (TableFlag3 = NO)
                              call      CreateTable3
                              call      CreateTable4
                    else
                              call      CreateTable4
                    endif
                    Write     htmlfile,seq;b20,"<TR>"
                    Write     htmlfile,seq;b20,*LL,"<TD WIDTH=225>"
                    Write     htmlfile,seq;"<P><PRE>"
                    for #NUMLV5,"0",#LV5
                              Select5listview.GetItemText giving NREFDESC using #NUMLV5,0
                              Write     htmlfile,seq;*LL,NREFDESC
                              add SingleSpaced to Row
                              if (ROW   >= LASTLINE)
                                        MOVE      "2" to SOURCEFlag
.This will Close the left section of the datacard which contains addressing, arrangement,source,selection
.Note:  This Close Table will NOT be used with the arrangement routine when it gets an OVER on the first read
.closes table 2 and 3 and ends the column - which signifies that the text will be written next if there is any
                                        Write     htmlfile,seq;"</PRE></P>"
                                        Write     htmlfile,seq;b20,*LL,"</TD>"
                              if (#LV5 = #NUMLV5)
                                        add c1 to #NUMLV5
                              endif
                                        Call LoadSourceMod
                                        Write     htmlfile,seq;b20,"</TR>"
                                        goto      CloseUpLeftTable
                              endif
CONTINUESOURCE
                    repeat
                    Write     htmlfile,seq;"</PRE></P>"
                    Write     htmlfile,seq;b20,*LL,"</TD>"
                    Call LoadSourceMod
                    Write     htmlfile,seq;b20,"</TR>"
                    call CloseTable4
          endif
          MOVE      "3" to SourceFlag                       .We are done with Source
.--------------------------------------------------------------------------------------------------------------------------------
SELECTIONS
.ListView6
          add SingleSpaced to Row
          if (ROW   >= LASTLINE)
                    Goto      CloseUpLeftTable
          endif
.SELECT
          pack      NSLTFLD1,"01X",LSTNUM
          move      "D.Load-NSLTAIM",Location
          pack      KeyLocation,"Key: ",NSLTFLD1
          call      NSLTAIM
          if over
                    MOVE      "3" to SelectionsFlag                             .We are done with Selections
                    move      YES to SectionTwo
                    Goto CloseUpLeftTable
          endif
          loop
                    until over
                    call      DataLoadRefSelection
                    call      trim using nrefdesc
                    movelptr NREFDESC to n7
                    loop
                              clear     Refline1
                              call      PARSITUP using Refline1,NREFDESC,C1
                              movefptr NREFDESC to n6
                              if (n6 >= n7)
                                        if (REFLINE1 <> "")
                                                  select6listview.InsertItem giving N8 using REFLINE1
                                                  select6Alistview.InsertItem giving N8 using str25
                                        endif
                              endif
                    until (n6 >= n7)
                              select6listview.InsertItem giving N8 using REFLINE1
                              select6Alistview.InsertItem giving N8 using str25
                              clear str25
                    repeat
                    move      "D.Load-NSLTKG",Location
                    pack      KeyLocation,"Key: ",NSLTFLD1
                    call      NSLTKG
          repeat
          Select6listview.GetItemCount giving #LV6
          sub       c1 from #LV6
          compare   c0,#LV6
          if less
                    MOVE      "3" to SelectionsFlag                             .We are done with Source
                    move      YES to SectionTwo
          else
                    Write     htmlfile,seq;b20,"<TR>"
                    Write     htmlfile,seq;b20,"<TD VALIGN=TOP ALIGN=CENTER><B><PRE>****SELECTIONS*****</PRE></B></TD>"
                    Write     htmlfile,seq;b20,"</TR>"
                    if (TableFlag3 = NO)
                              call      CreateTable3
                              call      CreateTable4
                    else
                              call      CreateTable4
                    endif
                    Write     htmlfile,seq;b20,"<TR>"
                    Write     htmlfile,seq;b20,*LL,"<TD WIDTH=225>"
                    Write     htmlfile,seq;"<P><PRE>"
                    for #NUMLV6,"0",#LV6
                              Select6listview.GetItemText giving NREFDESC using #NUMLV6,0
                              Write     htmlfile,seq;*LL,NREFDESC
                              add SingleSpaced to Row
                              if (ROW   >= LASTLINE)
                                        MOVE      "2" to SelectionsFlag
                                        Write     htmlfile,seq;"</PRE></P>"
                                        Write     htmlfile,seq;b20,*LL,"</TD>"
                              if (#LV6 = #NUMLV6)
                                        add c1 to #NUMLV6
                              endif
                                        Call LoadSelectionsMod
                                        Write     htmlfile,seq;b20,"</TR>"
                                        Goto      CloseUpLeftTable
                              endif
CONTINUESELECTIONS
                    repeat
                    MOVE      "3" to SelectionsFlag                             .We are done with Selections
                    move      YES to SectionTwo
                    Write     htmlfile,seq;"</PRE></P>"
                    Write     htmlfile,seq;b20,*LL,"</TD>"
                    Call LoadSelectionsMod
                    Write     htmlfile,seq;b20,"</TR>"
                    call CloseTable4
          endif
.--------------------------------------------------------------------------------------------------------------------------------
CloseUpLeftTable
          IF (TABLEFLAG4 = YES)
                    Write     htmlfile,seq;b15,"</TABLE>"  .TABLE 4
                    move      NO to TABLEFLAG3
          endif
          IF (TABLEFLAG3 = YES)
                    Write     htmlfile,seq;b10,"</TABLE>"  .TABLE 3
                    move      NO to TABLEFLAG3
          endif
.         IF (TABLEFLAG2 = YES)
.                   Write               htmlfile,seq;b5,"</TABLE>" .TABLE 2
.                   move NO to TABLEFLAG2
.         endif
.Ends Column1 of Table1 (1 of 2 Columns)
          Write     htmlfile,seq;b10,"</TD>"        .TABLE 1
.         call CloseTable
.         move      YES to SectionTwo
          If ((SectionOne = YES)&(SectionTwo = YES))
                    if (ENDDETAILFLAG <> YES)
                              call      enddetail
                    else
                              call      CreateTable5Indent
                              Write     htmlfile,seq;b15,"<TD>"
                              Write     htmlfile,seq;b15,"</TD>"
                              Call      CloseTable5
                              Write     htmlfile,seq;b5,"</TABLE>"    .TABLE 1
                              Move      NO to TableFlag1
                    endif
                    Goto READRecord
          else
                    GOTO CHECKPLACES1
.                   Return
          Endif

.--------------------------------------------------------------------------------------------------------------------------------
Selects
          move      "10" to ROW
          Add       "90" to ROW
          if (NDATCONV <> "1")
                    move      c3 to SELECTFLAG1
                    move      c3 to SELECTFLAG2
                    Goto Checkplaces1
          endif
          add       smboxheight to row
.Patch1.1 CommentOut
.         move      c1 to NSELPATH
.         move      c0 to #n4
.         loop
.                   add       c1 to #n4
.                   move      #n4 to str4
.                   call      zfillit using str4,c0
.                   rep       zfill,str4
.                   pack      nselfld with lstnum,str4
.                   call      nselkey
.Patch1.1Co
                    pack      NSELFLD1,"01X",LSTNUM
                    clear     NSELFLD2
                    move      "D.Load-NSELAIM",Location
                    pack      KeyLocation,"Key: ",NSELFLD1
                    call      NSELAIM
                    loop
.Patch1.1
                    until over
                    Goto OfficeUse if (NSELSTATUS = "1" | NSELSTATUS = "2")                         Skip select if for office use only
                    Goto OfficeUse if (NSELINACTIVE = "1")            Skip select if inactive
.START PATCH 1.97 REPLACED LOGIC
.                    if ((NSELBASE = "BASE")|(NSELBASE = "SEC."))
                    call      debug
                    move      No,Tertbase
                    call      Trim using NselBase
                    if        (NSelBase <> "" & NSelBase <> " " & NSelBase <> "    ")
                    move      yes,tertbase
                    endif
                    if (NSELBASE = "BASE" | NSELBASE = "SEC." | tertbase = "Y")
.END PATCH 1.97 REPLACED LOGIC
                              pack      nselfld with lstnum,nselnum
                              call      nselkey
                              move      mask13 to dim13a
.DH TESTING
                              sub       n10,n10
                              move      nselqty to n10
                              edit      n10 to dim13a
.Universe
.                                       call      trim using dim13a
.ListName
                              call      trim using nselsname
.Price
                              move      nselprice to dim9a
                              call      trim using dim9a
                              call      SelectLoadModifier
                              selectlistview.InsertItem giving N8 using NSELINDEX
                              selectlistview.SetItemText giving N1 using N8,NSELNUM,1
                              selectlistview.SetItemText giving N1 using N8,NSELSNAME,2
                              selectlistview.SetItemText giving N1 using N8,NSELNOTES,3
.SelectAListView - Universe
                              selectAlistview.InsertItem giving N8 using NSELINDEX
                              selectAlistview.SetItemText giving N1 using N8,DIM13A,1
.
                              call      trim using dim13a
                              clear     n6
                              count     N6,DIM13a
                              if (n6 > UNINUM1)
                                        move      n6 to UNINUM1
                              endif

.
.SelectBListView Price
                              SelectBlistview.InsertItem giving N8 using NSELINDEX
                              selectBlistview.SetItemText giving N1 using N8,Str25,1
                              selectBlistview.SetItemText giving N1 using N8,NSELLIST,2
                              move      nselprice to str8
                              selectBlistview.SetItemText giving N1 using N8,str8,3
                              selectBlistview.SetItemText giving N1 using N8,NSELEXC,4
                    else
.                             call      SelectTestBase4 using NSELLIST,NSELBASE,N52
.                             if (NSELEXC <> "1")
.                                       add       NSELPRICE,N52
.                                       move      N52,str8
.                                       rep       zfill,str8
.                                       move      str8,n8
.                             Else
.                                       move      N52,str8
.                                       rep       zfill,str8
.                                       move      str8,n8
.                             endif
.                             move n52 to dim9a
                              move      nselprice to dim9a
                              call      trim using dim9a
                              call      SelectLoadModifier
                              move      mask13 to dim13a
.dh testing
                              sub       n10,n10
                              move      nselqty to n10
                              edit      n10 to dim13a
.                             call      trim using dim13a
                              select2listview.InsertItem giving N8 using NSELINDEX
                              select2listview.SetItemText giving N1 using N8,NSELNUM,1
                              select2listview.SetItemText giving N1 using N8,NSELSNAME,2
                              select2listview.SetItemText giving N1 using N8,NSELNOTES,3

                              Select2Alistview.InsertItem giving N8 using NSELINDEX
                              Select2AListView.SetItemText giving N1 using N8,dim13a,1
                              select2Alistview.SetItemText giving N1 using N8,NSELNUM,2
.
                              call      trim using dim13a
                              clear     n6
                              count     N6,DIM13a
                              if (n6 > UNINUM2)
                                        move      n6 to UNINUM2
                              endif
.
                              Select2Blistview.InsertItem giving N8 using NSELINDEX
                              select2Blistview.SetItemText giving N1 using N8,Str25,1
                              select2Blistview.SetItemText giving N1 using N8,NSELLIST,2
                              move      nselprice to str8
                              select2Blistview.SetItemText giving N1 using N8,str8,3
                              select2Blistview.SetItemText giving N1 using N8,NSELEXC,4
                    endif
.Begin patch 1.84   
.                    call      Debug
                    if        (NSELEXC <> "2")       .if the select is not exchange only
                    MOve      Yes,REntFlag           .at least one portion of this list is rentable
                    endif
.End patch 1.84               
OfficeUse
.Patch1.1
                    move      "D.Load-NSELKG",Location
                    pack      KeyLocation,"Key: ",NSELFLD1
                    call      NSELKG
.Patch1.1
          repeat
          Selectlistview.GetItemCount giving #LV
          sub       c1 from #LV
          compare   c0,#LV
.
          move      "13" to Uninum
          sub       uninum1 from uninum
          move      uninum to uninum1

          move      "13" to Uninum
          sub       uninum2 from uninum
          move      uninum to uninum2

.
          if less
                    move      "3" to SELECTFLAG1
                    call      CreateTable5
                    Goto Text
          else
                    call      CreateTable5Indent
                    Write     htmlfile,seq;b15,"<TD>"
                    call      CreateTable6
          endif

          move      #LV to #LVA
          Write     htmlfile,seq;b20,"<TR>"
          Write     htmlfile,seq;b15,"<TD WIDTH=70>&nbsp</TD>"
          Write     htmlfile,seq;b20,"<TD WIDTH=50 ALIGN=RIGHT>"
          Write     htmlfile,seq;"<P><PRE>"
          for #NUMLVA,"0",#LVA
                    if (ROW   >= LASTLINE)
                              MOVE      "2" to SelectFlag1
                              Write     htmlfile,seq;"</PRE></P>"
                              Write     htmlfile,seq;b20,"</TD>"
                              Write     htmlfile,seq;b20,"<TD WIDTH=1>&nbsp</TD>"
                              if (#LVA = #NUMLVA)
                                        add       c1 to #NUMLVA
                              endif
                              Call      LoadSelectNames
                              Write     htmlfile,seq;b20,"<TD WIDTH=1>&nbsp</TD>"
                              Call      LoadSelectMod
                              Write     htmlfile,seq;b15,"</TD>"
                              Write     htmlfile,seq;b15,"</TR>"
                              call      CloseTable6
                              call      CloseTable5
                              Write     htmlfile,seq;b5,"</TABLE>"              .Table 1
                              call      printlogo
                              Goto CheckPlaces2
                    endif
                    clear     Dim13a
                    SelectAlistview.GetItemText giving DIM13A using #NUMLVA,1
.                   call trim using dim13a
.
.
                    if (uninum1 <> c0)
                              bump      dim13a,uninum1
                    endif
.

.
                    Write     htmlfile,seq;*LL,dim13A
                    add       SingleSpaced to Row
ContinueSelects
          repeat
          move      "3" to SELECTFLAG1
          Write     htmlfile,seq;"</PRE></P>"
          Write     htmlfile,seq;b20,"</TD>"
          Write     htmlfile,seq;b20,"<TD WIDTH=1>&nbsp</TD>"
          Call      LoadSelectNames
          Write     htmlfile,seq;b20,"<TD WIDTH=1>&nbsp</TD>"
          Call      LoadSelectMod
          Write     htmlfile,seq;b15,"</TD>"
          Write     htmlfile,seq;b15,"</TR>"
          call      CloseTable6
Text
.TEXT
.begin patch 1.94
          if        (lstnum = "008616")
          call      debug
          endif
          clear     hold2
          clear     NTXT1TEXT
          clear     line1
          move      c2,Ntxt1path
          for #N2a,C1,"5"
                    pack      NTXT1FLD1,LSTNUM,#N2a
                    rep       zfill,ntxt1fld1
                    move      "D.Load-NTXT1KEY",Location
                    pack      KeyLocation,"Key: ",NTXT1FLD1
.                    call      debug
                    call      NTXT1KEY
                    until     over
                    
                    append    NTXT1TEXT,hold2
          repeat                  
                    reset     hold2
                    call      Trim using hold2
                    if (hold2 <> "")
                              Write     htmlfile,seq;b20,"<TR>"
                              Write     htmlfile,seq;b15,"<TD WIDTH=70>&nbsp</TD>"
                              Write     htmlfile,seq;b20,*LL,"<TD WIDTH=225>"
                              Write     htmlfile,seq;"<P><PRE>"
                              if (ndatconv = "1")
                                        Write     htmlfile,seq;*LL,""
                              endif
                              movelptr hold2 to n7
                              loop
                                        clear     line1
                                        call      PARSITUP using line1,hold2,C1
                                        movefptr hold2 to n6
                              until (n6 >= n7)
                                        add       singlespaced to row
                              if (ROW   >= LASTLINE)
                                        MOVE      "2" to TEXTFlag
                                        Write     htmlfile,seq;"</PRE></P>"
                                        Write     htmlfile,seq;b20,*LL,"</TD>"
                                        Write     htmlfile,seq;b20,"</TR>"
                                        Call      CloseTextTable
                                        call      printlogo
                                        Goto CheckPlaces2
                              endif
ContinueText
                                        Write     htmlfile,seq;*LL,LINE1
                              repeat
                              if (line1 <> "")
                              Write     htmlfile,seq;*LL,LINE1
                              endif
                    endif
.end patch 1.94
          clear     NTXTTEXT
          clear     hold2
          clear     line1
.         move      #n8 to row
          add       SmBoxHeight to row
          add       singlespaced to row
.         for #N1,C1,"9"
          for #N2a,C1,"15"
                    pack      NTXTFLD,LSTNUM,#N2a
                    rep       zfill,ntxtfld
                    move      "D.Load-NTXTKEY",Location
                    pack      KeyLocation,"Key: ",NTXTFLD
                    call      NTXTKEY
                    until over

.         for n7,c1,"15"
.         pack ntxttext, "a g d d s s g g s s g g s s gh s  sg",newline,"ss gs sg s sg s ga s w e d c.",newline,"sdf sf sdf sfdafsaf ,   sdaf ",newline,"ssfd f sfsd",newline,"fs fsd fsf sdf sdf sdf sdf sf ",newline,"sd fs fsdf sf sf sdf sf sdfsdf",newline," sdf sdf sd ew c cw,. sdfsd sf sdfs f.  fsdf.",newline,"fsd.sdf.sdf.sdf. sdfs sdf sdf.,sdfs",newline,"df.sdfsdf.sdffsdfsfsd sdf sdf ",newline,"sdf ssdf sdfs fsd",newline,"fsffdsfsd",newline
                    append    NTXTTEXT,hold2
          repeat
          reset     hold2
          call      Trim using hold2
          if (hold2 <> "")
                    Write     htmlfile,seq;b20,"<TR>"
                    Write     htmlfile,seq;b15,"<TD WIDTH=70>&nbsp</TD>"
                    Write     htmlfile,seq;b20,*LL,"<TD WIDTH=225>"
                    Write     htmlfile,seq;"<P><PRE>"
                    if (ndatconv = "1")
                              Write     htmlfile,seq;*LL,""
                    endif
                    movelptr hold2 to n7
                    loop
                              clear     line1
                              call      PARSITUP using line1,hold2,C1
                              movefptr hold2 to n6
                    until (n6 >= n7)
.                             until (line1 = "" and str1 = "0")
                              add       singlespaced to row
                              if (ROW   >= LASTLINE)
                                        MOVE      "3" to TEXTFlag
                                        Write     htmlfile,seq;"</PRE></P>"
                                        Write     htmlfile,seq;b20,*LL,"</TD>"
                                        Write     htmlfile,seq;b20,"</TR>"
                                        Call      CloseTextTable
.                                       Write     htmlfile,seq;b10,"</TABLE>"    .Need this to close The table and start a new box - used only when more text to write on another page .TABLE 5
.                                       move NO to TableFlag5
                                        call      printlogo
                                        Goto CheckPlaces2
                              endif
ContinueText1
                              rep       lowup in line1
                              Write     htmlfile,seq;*LL,LINE1
                    repeat
          endif
.could get me into trouble if duplication occurs
          if (line1 <> "")
                              rep       lowup in line1
                              Write     htmlfile,seq;*LL,LINE1
          endif
          Write     htmlfile,seq;"</PRE></P>"
          Write     htmlfile,seq;b20,*LL,"</TD>"
.         Write     htmlfile,seq;b15,"</TABLE>"             .TABLE 5
.         move NO to TABLEFLAG5
.          MOVE      "3" to TEXTFlag
          MOVE      "4" to TEXTFlag
          add       singlespaced to row
          if (ROW   >= LASTLINE)
                    call      closetexttable
                    Write     htmlfile,seq;b15,"</TABLE>"             .TABLE A
                    Write     htmlfile,seq;"</TR>"  .TABLE A
                    Write     htmlfile,seq;"</TD>"  .TABLE A
.         Write     htmlfile,seq;b15,"</TABLE>"             .TABLE 5
.                   Write     htmlfile,seq;b10,"</TABLE>"    .Need this to close The table and start a new box - used only when more text to write on another page .TABLE 1
                    move      NO to TABLEFLAG1
                    call      printlogo
                    Goto CheckPlaces2
          endif
          Write     htmlfile,seq;b20,"</TR>"
AdditionalSelects
.If their are additional Selects Table This routine will create table 7 to create "the indent" and table 8
          clear     num9
          Select2listview.GetItemCount giving #result
          sub       c1 from #result
          compare   c0,#result
          if less
                    move      "3" to SELECTFLAG2
                    MOVE      YES to SECTIONONE
                    CALL      ENDDETAIL
.                   Write     htmlfile,seq;b15,"</TABLE>"                                 .TABLE 5
                    Write     htmlfile,seq;b15,"</TD>"
                    Write     htmlfile,seq;b15,"</TR>"
                    Goto CheckPlaces2
          else
                    Call CreateTable7
          endif
.Instance where there may be no universe but there is a selects
.use select name counter
          move      #result to #LV2A
          Write     htmlfile,seq;b20,"<TR>"
          Write     htmlfile,seq;b15,"<TD WIDTH=70>&nbsp</TD>"
          Write     htmlfile,seq;b20,"<TD WIDTH=50 ALIGN=RIGHT>"
          Write     htmlfile,seq;"<P><PRE>"
          for #NUMLV2A,"0",#LV2A
                    clear     Dim13a
                    Select2Alistview.GetItemText giving dim13a using #NUMLV2A,1
.                   call trim using dim13a
.
                    if (uninum2 <> c0)
                              bump      dim13a,uninum2
                    endif
.
                    Write     htmlfile,seq;*LL,dim13A
                    add       SingleSpaced to Row
                    if (ROW   >= LASTLINE)
                              MOVE      "2" to selectFlag2
                              Write     htmlfile,seq;"</PRE></P>"
                              Write     htmlfile,seq;b20,"</TD>"
                              Write     htmlfile,seq;b20,"<TD WIDTH=1>&nbsp</TD>"
                              if (#LV2A = #NUMLV2A)
                                        add       c1 to #NUMLV2A
                              endif
                              Call      LoadAdditionalSelectNames
                              Write     htmlfile,seq;b20,"<TD WIDTH=1>&nbsp</TD>"
                              Call      LoadAdditionalSelectMod
                              Write     htmlfile,seq;b20,*LL,"</TD>"
                              Write     htmlfile,seq;b15,"</TABLE>"             .Table 7
                              move      NO to TableFlag7
                              Call      CloseTable5
                              Write     htmlfile,seq;b5,"</TABLE>"              .Table 1
                              move      NO to TABLEFLAG1
                              call      printlogo
                              Goto CheckPlaces2

                    endif
ContinueSelects2
          repeat
          move      "3" to selectFlag2
          MOVE      YES to SECTIONONE
          Write     htmlfile,seq;"</PRE></P>"
          Write     htmlfile,seq;b20,"</TD>"
          Write     htmlfile,seq;b20,"<TD WIDTH=1>&nbsp</TD>"
          Call      LoadAdditionalSelectNames
          Write     htmlfile,seq;b20,"<TD WIDTH=1>&nbsp</TD>"
          Call      LoadAdditionalSelectMod

          Write     htmlfile,seq;b15,"</TD>"
          Write     htmlfile,seq;b15,"</TR>"
          Write     htmlfile,seq;b10,"</TABLE>"             .Table 7
          move      NO to TableFlag7
          CALL      ENDDETAIL
.         Call closetable5
.         Write     htmlfile,seq;b10,"</TABLE>"             .Table 5
          If ((SectionOne = YES)&(SectionTwo = YES))
.                   Write     htmlfile,seq;b5,"</TABLE>"              .Table 1
                    Goto READRecord
          Else
                    Goto CheckPlaces2
          Endif
.         call CreatePage
EndDataRun
   SHUTDOWN   "CLS"
          STOP

NewPage
.         stream    *STDOUT,"Newpage str55 =  ",str55," List = ",lstnum:
.                   ". <br><br>",newline:
.                   "Testing PLease ignore <a href=#"#/index.php/contact-us/#";#"> website administrator</a> to correct this problem."
.begin patch 1.95
          call      Trim using lstnum
          if        (lstnum = "" or lstnum = "      ")
          goto      enddatarun
          endif
.end patch 1.95
          Write     htmlfile,seq;"<HTML>"
          Write     htmlfile,seq;"<TITLE>",MLSTNAME,"</TITLE>"
.Patch 1.1.6
          if (ELSTCDE <> "C")
                    Write     htmlfile,seq;"<meta name=#"robots#" content=#"noindex,nofollow#">"
          endif
.End Patch 1.1.6
.Patch 1.1.5 Comment Out
.         Write     htmlfile,seq;"<BODY>"
.Patch 1.1.5 Code Added
.START PATCH 1.1.9 REPLACED LOGIC
.         if (printflag = 1)
          if (printflag = 1 & !externalmode)
.END PATCH 1.1.9 REPLACED LOGIC
.>Patch 1.2 Code Modified
.                   Write     htmlfile,seq;"<BODY BACKGROUND=#"\\nts2\c\http\datacards\data_gradient.jpg#" text=#"##2858a6#" link=#"##2759A6#" vlink=#"##2858a6#" alink=#"##2759A6#">"
                    Write     htmlfile,seq;"<BODY BACKGROUND=#"\\nins1\e\netutils\images\data_gradient.jpg#" text=#"##2858a6#" link=#"##2759A6#" vlink=#"##2858a6#" alink=#"##2759A6#">"
.>Patch 1.2 Code Modified End
          else
                    Write     htmlfile,seq;"<BODY BACKGROUND=#"data_gradient.jpg#" text=#"##2858a6#" link=#"##2759A6#" vlink=#"##2858a6#" alink=#"##2759A6#">"
          endif
.Patch 1.1.5
          Write     htmlfile,seq;"<P ALIGN=CENTER>"
.Patch1.1.2
          if (PrintFlag = 1 & pagenum = c1)
.START PATCH 1.1.9 REPLACED LOGIC
.                   Write     htmlfile,seq;"<A  HREF=",str35,"target=#"_blank#"",">","Owner Info","</A>"
                    if (!externalmode)
                              Write     htmlfile,seq;"<A  HREF=",str35,"target=#"_blank#"",">","Owner Info","</A>"
                    endif
.START PATCH 1.1.9 REPLACED LOGIC
          endif
.patch1.1.2
.         Write     htmlfile,seq;"<a href=javascript:window.print()>Send To Printer</a>&nbsp;"
          Write     htmlfile,seq;"<TABLE BORDER=4 RULES=NONE CELLPADDING=0 CELLSPACING=0 WIDTH=660>" .TABLE A
          Write     htmlfile,seq;"<TR>"  .TABLE A
          Write     htmlfile,seq;"<TD>"  .TABLE A
          Write     htmlfile,seq;"<TABLE BORDER=0 RULES=NONE CELLPADDING=0 CELLSPACING=0 WIDTH=660>"
          call      trim using mlstname
          if (pagenum > c1)
.Office Use Only?????
                    if (NDATOFF <> "1")
                              Write     htmlfile,seq;*LL,b5,"<PRE><TD WIDTH=588><PRE>",MLSTNAME," (Cont.)","</PRE></TD>"
                    else
                              Write     htmlfile,seq;*LL,b5,"<PRE><TD WIDTH=588><PRE>",MLSTNAME," - Office Use Only"," (Cont.)","</PRE></TD>"
                    endif
          else
.Office Use Only?????           .First Page
                    if (NDATOFF <> "1")
                              Write     htmlfile,seq;*LL,b5,"<PRE><TD WIDTH=588><PRE>",MLSTNAME,"</PRE></TD>"
                    else
                              Write     htmlfile,seq;*LL,b5,"<PRE><TD WIDTH=588><PRE>",MLSTNAME," - Office Use Only","</PRE></TD>"
                    endif
          endif
.WithDrawn / Temporary Withdrawn????
          if (STATUS = "W")
                                        Write     htmlfile,seq;b5,"<TD WIDTH=100><PRE>**WITHDRAWN**</PRE></TD>"
          elseif (STATUS = "T")
                                        Write     htmlfile,seq;b5,"<TD WIDTH=100><PRE>**TEMPWITH**</PRE></TD>"
          else
.No then is it New and/or exclusive?
                    if (NLSTCDE = YES)
                              if (ELSTCDE = "C" or ELSTCDE = "P")
.begin patch 1.92
.                                        Write     htmlfile,seq;b5,"<TD WIDTH=100><PRE>**NEW/EXCLUSIVE**</PRE></TD>"
                                        Write     htmlfile,seq;b5,"<TD WIDTH=100>&nbsp</TD>"
.end patch 1.92
                              else
                                        Write     htmlfile,seq;b5,"<TD WIDTH=100><PRE>**NEW**</PRE></TD>"
                              Endif
                    else
                              if (ELSTCDE = "C" or ELSTCDE = "P")
.begin patch 1.92
.                                        Write     htmlfile,seq;b5,"<TD WIDTH=100><PRE>**EXCLUSIVE**</PRE></TD>"
                                        Write     htmlfile,seq;b5,"<TD WIDTH=100>&nbsp</TD>"
.end patch 1.92
                              else
                                        Write     htmlfile,seq;b5,"<TD WIDTH=100>&nbsp</TD>"
                              Endif
                    endif
          endif
.Took out list name looks ugly when text goes past parameter allowed for
.         Write     htmlfile,seq;b5,"<TD WIDTH=100 ALIGN=RIGHT><PRE>",LSTNUM,"</PRE></TD>"
          Write     htmlfile,seq;b5,"</TR>"
          Write     htmlfile,seq;"</TABLE>"
          Write     htmlfile,seq;"</TD>" .TABLE A
          Write     htmlfile,seq;"</TR>" .TABLE A
          Write     htmlfile,seq;"<TR>"  .TABLE A
          Write     htmlfile,seq;"<TD>"  .TABLE A
          Write     htmlfile,seq;b5,"<TABLE BORDER=4 FRAME=HSIDES CELLSPACING=0 CELLPADDING=0 WIDTH=660 HEIGHT=760>"                                            .TABLE 1
          move      YES to TABLEFLAG1
          Write     htmlfile,seq;b10,"<TD VALIGN=TOP WIDTH=250>"
          Write     htmlfile,seq;b10,"<TABLE BORDER=1  FRAME=HSIDES CELLPADDING=0 CELLSPACING=0 WIDTH=250>"                                 .TABLE 2
          move      YES to TABLEFLAG2
          unpack    REVDATE,str2,yy,mm,dd
          move      MM to n2
          load      str12 with n2,"January","February","March","April","May","June","July","August","September","October","November","December"
          pack      str30,str12,b5,dd,comma,b5,str2,yy
          add       SmBoxHeight to Row
          Write     htmlfile,seq;b15,"<TR>"
.START PATCH 1.1.8 REPLACED LOGIC
.         Write     htmlfile,seq;*LL,b15,"<TD WIDTH=250><PRE>",str30,"</PRE></TD>"
..................
          Write     htmlfile,seq;*LL,b15,"<TD WIDTH=250 HEIGHT=10><PRE>","Last Update:"
          Write     htmlfile,seq;*LL,str30,"</PRE></TD>"
.END PATCH 1.1.8 REPLACED LOGIC
          Write     htmlfile,seq;b15,"</TR>"
          move      "10" to ROW
          Add       "90" to ROW
.START PATCH 1.1.8 ADDED LOGIC
          call trim using CLEANCDE
          if ((CLEANCDE <> "0000")&(CLEANCDE <> ""))
                    add       SmBoxHeight to Row
                    add       SmBoxHeight to Row
                    Write     htmlfile,seq;b15,"<TR>"
                    Write     htmlfile,seq;*LL,b15,"<TD WIDTH=250><PRE>","Updated:"
                    if (CLEANCDE = "C002")
                              call      Trim using CLNINFO
                              Write     htmlfile,seq;*LL,CLNINFO,"</PRE></TD>"
                    else
                              Call      DataLoadRefClean
                              call      Trim using NREFDESC
                              Write     htmlfile,seq;*LL,NREFDESC,"</PRE></TD>"
                    endif
                    Write     htmlfile,seq;b15,"</TR>"
          endif
.END PATCH 1.1.8 ADDED LOGIC
          add       SmBoxHeight to Row
          add       SmBoxHeight to Row
          Write     htmlfile,seq;b15,"<TR>"
.START PATCH 1.1.1 REPLACED LOGIC
.         Write     htmlfile,seq;*LL,b15,"<TD><PRE>Sex: ",SEX,"</PRE></TD>"
.begin patch 1.7
.          call      debug
                    Clear     str25
              if              (NdatFem > 0)
                    move                NDatFem,N3
                    MOVe                N3,str3
                    append    "Women ",str25
                    append    str3,str25
                    append    "%",Str25
                    append    " ",Str25
             endif
              if              (NdatMen > 0)
                    move                NDatMen,N3
                    MOVe                N3,str3
                    append    "Men ",str25
                    append    str3,str25
                    append    "%",Str25
          endif
          reset     str25
          if        (NdatFem > 0 or NdatMen > 0)
.                   Setitem   DataEditGender,0,Str25
                    Write     htmlfile,seq;*LL,b15,"<TD><PRE>",Str25,"</PRE></TD>"
                    Else
                    Write     htmlfile,seq;*LL,b15,"<TD><PRE>Gender: ",SEX,"</PRE></TD>"
                              endif

.         Write     htmlfile,seq;*LL,b15,"<TD><PRE>Gender: ",SEX,"</PRE></TD>"
.end patch 1.7
.END PATCH 1.1.1 REPLACED LOGIC
          Write     htmlfile,seq;b15,"</TR>"
          add       SmBoxHeight to Row
          Write     htmlfile,seq;b15,"<TR>"
          Write     htmlfile,seq;*LL,b15,"<TD><PRE>Minimum: ",MIN,"</PRE></TD>"
          Write     htmlfile,seq;b15,"</TR>"
          Write     htmlfile,seq;"</TABLE>"                           .TABLE 2
          move      NO to TABLEFLAG2
.         Write     htmlfile,seq;b15,"<TD>"
          add       SmBoxHeight to Row
          add       SmBoxHeight to Row
          move      ROW to #N9
          if (SectionTwo = YES)
.                   Write     htmlfile,seq;"</TABLE>"                           .TABLE 2
.                   move NO to TABLEFLAG2
.patch1.3 Comment Out
.                   Write     htmlfile,seq;"</TD>"

.                   Call CreateTable5
.                   Return if (ENDDETAILFLAG = YES)
                    if (ENDDETAILFLAG = YES)
                              return
                    endif
.patch1.3
.patch1.3 Comment Out
.                   Goto CheckPlaces1
.patch1.3
          endif
          return

ENDDETAIL
          move      "Y" to EndDetailFlag
          CALL      CreateTable5
          add       SingleSpaced to Row
          if (ROW   >=        LASTLINE)
                    Call      EndDetailTable2
                    call      header
                    CALL      CreateTable5
                    move      "10" to ROW
                    Add       "90"      to ROW
                    add       smboxheight to row
          endif
          Write     htmlfile,seq;"<TABLE FRAME=VSIDES BORDER=0 CELLPADDING=0 ALIGN=TOP CELLSPACING=0>"        TABLE 9
          Write     htmlfile,seq;b20,"<TR>"
          Write     htmlfile,seq;b15,"<TD WIDTH=70>&nbsp</TD>"
          Write     htmlfile,seq;b20,*LL,"<TD WIDTH=225>"
          Write     htmlfile,seq;"<P><PRE>"
                    if (UNITDATA <> "")
                              unpack    unitdata,dimtext1,dimtext2,dimtext3,dimtext4
                              clear     n1
                              for n1,"1","5"
                                        clear     dimtext5
                                        load      dimtext5,n1,dimtext1,dimtext2,dimtext3,dimtext4,dimtext6
                                        pack      str2,carr,b1
                                        rep       str2,DIMTEXT5
                                        call      trim using dimtext5
.;
                                        if (DIMTEXT5 <> "")
                                                  movelptr dimtext5 to n7
                                                  loop
                                                            clear     Refline2
                                                            call      PARSITUP using Refline2,dimtext5,C1
                                                            movefptr dimtext5 to n6
                                                            if (n6 >= n7)
                                                                      if (REFLINE2 <> "")
                                                                                rep       lowup in REFLINE2
                                                                                Write     htmlfile,seq;*LL,REFLINE2
                                                                      endif
                                                            endif
                                                  until (n6 >= n7)
                                                            rep       lowup in REFLINE2
                                                            Write     htmlfile,seq;*LL,REFLINE2
                                                  repeat
                                        endif
                              repeat
                    endif
                    add       singlespaced to row
          if (ROW   >=        LASTLINE)
                    Write     htmlfile,seq;"</PRE></P>"
                    Write     htmlfile,seq;b20,*LL,"</TD>"
                    Write     htmlfile,seq;b20,*LL,"</TR>"
                    Call      EndDetailTable2
                    call      header
                    CALL      CreateTable5
                    move      "10" to ROW
                    Add       "90" to ROW
                    add       smboxheight to row
          endif
          Write     htmlfile,seq;"</PRE></P>"
          Write     htmlfile,seq;b20,*LL,"</TD>"
          Write     htmlfile,seq;b20,*LL,"</TR>"
          Write     htmlfile,seq;"<TABLE FRAME=VSIDES BORDER=0 CELLPADDING=0 ALIGN=TOP CELLSPACING=0>" TABLE 9
.START PATCH 1.1.8 REPLACED LOGIC - MOVED TO DIFFERENT AREA
.         call      trim using CLEANCDE
.         if ((CLEANCDE <> "0000")&(CLEANCDE <> ""))
.                   Write     htmlfile,seq;b15,"<TR>"
.                   Write     htmlfile,seq;b15,"<TD WIDTH=70>&nbsp</TD>"
.                   Write     htmlfile,seq;b15,"</TR>"
.                   Write     htmlfile,seq;b15,"<TR>"
.                   Write     htmlfile,seq;b15,"<TD WIDTH=70 VALIGN=TOP><PRE>UPDATED:</PRE></TD>"
.                   Write     htmlfile,seq;b15,"<TD WIDTH=497>"
.                   Write     htmlfile,seq;b15,"<P><PRE>"
.                   if (CLEANCDE = "C002")
.                             movelptr CLNINFO to n7
.                             clear     n1
.                             loop
.                                       clear     Refline2
.                                       call      PARSITUP using Refline2,CLNINFO,C1
.                                       movefptr CLNINFO to n6
.                                       if (n6 >= n7)
.                                                 if (REFLINE2 <> "")
.                                                           Write     htmlfile,seq;REFLINE2
.                                                 endif
.                                       endif
.                                       until (n6 >= n7)
.                                       Write     htmlfile,seq;REFLINE2
.                             repeat
.                   else
.                             Call      DataLoadRefClean
.;
.                             movelptr NREFDESC to n7
.                             clear     n1
.                             loop
.                                       clear     Refline2
.                                       call      PARSITUP using Refline2,NREFDESC,C1
.                                       movefptr NREFDESC to n6
.                                       if (n6 >= n7)
.                                                 if (REFLINE2 <> "")
.                                                           Write     htmlfile,seq;REFLINE2
.                                                 endif
.                                       endif
.                                       until (n6 >= n7)
.                                       Write     htmlfile,seq;REFLINE2
.                             repeat
.                   endif
.                   Write     htmlfile,seq;"</PRE></P>"
.                   Write     htmlfile,seq;b15,"</TD>"
.                   Write     htmlfile,seq;b15,"</TR>"
..                  Write     htmlfile,seq;b15,"</TR>"
.                   add       singlespaced to row
.         endif
.END PATCH 1.1.8 REPLACED LOGIC - MOVED TO DIFFERENT AREA
          if (ROW   >=        LASTLINE)
                    Call      EndDetailTable
                    call      header
                    CALL      CreateTable5
                    move      "10" to ROW
                    Add       "90" to ROW
                    add       smboxheight to row
          endif
.;
          call trim using NETNAME
          if ((NETNAME <> "0000")&(NETNAME <> ""))
                    Write     htmlfile,seq;b15,"<TR>"
                    Write     htmlfile,seq;b15,"<TD WIDTH=70><PRE>NET NAMES:</PRE></TD>"
                    Write     htmlfile,seq;b15,"<TD WIDTH=497>"
                    Write     htmlfile,seq;b15,"<P><PRE>"
                    if (NETNAME = "N002")
                              movelptr NETINFO to n7
                              clear     n1
                              loop
                                        clear     Refline2
                                        call      PARSITUP using Refline2,NETINFO,C1
                                        movefptr NETINFO to n6
                                        if (n6 >= n7)
                                                  if (REFLINE2 <> "")
                                                            Write     htmlfile,seq;REFLINE2
                                                  endif
                                        endif
                                        until (n6 >= n7)
                                        Write     htmlfile,seq;REFLINE2
                              repeat
                    else
                              Call      DataLoadRefNetName
                              movelptr NREFDESC to n7
                              clear     n1
                              loop
                                        clear     Refline2
                                        call      PARSITUP using Refline2,NREFDESC,C1
                                        movefptr NREFDESC to n6
                                        if (n6 >= n7)
                                                  if (REFLINE2 <> "")
                                                            Write     htmlfile,seq;REFLINE2
                                                  endif
                                        endif
                                        until (n6 >= n7)
                                        Write     htmlfile,seq;REFLINE2
                              repeat
                    endif
                    Write     htmlfile,seq;"</PRE></P>"
                    Write     htmlfile,seq;b15,"</TD>"
                    Write     htmlfile,seq;b15,"</TR>"
                    add       singlespaced to row
          endif
          if (ROW   >=        LASTLINE)
                    Call      EndDetailTable
                    call      header
                    CALL      CreateTable5
                    move      "10" to ROW
                    Add       "90" to ROW
                    add       smboxheight to row
          endif
          call trim using SAMPLE
          if ((SAMPLE <> "0000")&(SAMPLE <> ""))
                    Call      DataLoadRefSAMPLE
.
                    Write     htmlfile,seq;b15,"<TR>"
                    Write     htmlfile,seq;b15,"<TD WIDTH=70 VALIGN=TOP><PRE>SAMPLE:</PRE></TD>"
                    Write     htmlfile,seq;b15,"<TD WIDTH=497>"
                    Write     htmlfile,seq;b15,"<P><PRE>"
                    movelptr NREFDESC to n7
                    clear     n1
                    loop
                              clear     Refline2
                              call      PARSITUP using Refline2,NREFDESC,C1
                              movefptr NREFDESC to n6
                              if (n6 >= n7)
                                        if (REFLINE2 <> "")
                                                  Write     htmlfile,seq;REFLINE2
                                        endif
                              endif
                              until (n6 >= n7)
                              if (n1 > c0)
                              endif
                              Write     htmlfile,seq;REFLINE2
                    repeat
                    Write     htmlfile,seq;"</P></PRE>"
                    Write     htmlfile,seq;b15,"</TD>"
                    Write     htmlfile,seq;b15,"</TR>"
          endif
          add singlespaced to row
          if (ROW   >=        LASTLINE)
                    Call      EndDetailTable
                    call      header
                    CALL CreateTable5
                    move      "10" to ROW
                    Add       "90"      to ROW
                    add       smboxheight to row
          endif
.;
          call      trim using DELCODE
          if ((DELCODE <> "0000")&(DELCODE <> ""))
                    Call      DataLoadRefDelivery
                    Write     htmlfile,seq;b15,"<TR>"
                    Write     htmlfile,seq;b15,"<TD WIDTH=70 VALIGN=TOP><PRE>DELIVERY:</PRE></TD>"
                    movelptr NREFDESC to n7
                    clear     n1
                    Write     htmlfile,seq;b15,"<TD WIDTH=497>"
                    Write     htmlfile,seq;b15,"<P><PRE>"
                    loop
                              clear     Refline2
                              call      PARSITUP using Refline2,NREFDESC,C1
                              movefptr NREFDESC to n6
                              if (n6 >= n7)
                                        if (REFLINE2 <> "")
                                                  Write     htmlfile,seq;REFLINE2
                                        endif
                              endif
                              until (n6 >= n7)
                              Write     htmlfile,seq;REFLINE2
                              add       c1 to n1
                    repeat
                    Write     htmlfile,seq;"</P></PRE>"
                    Write     htmlfile,seq;b15,"</TD>"
                    Write     htmlfile,seq;b15,"</TR>"
                    add       singlespaced to row
          endif
          if (ROW   >=        LASTLINE)
                    Call      EndDetailTable
                    call      header
                    CALL      CreateTable5
                    move      "10" to ROW
                    Add       "90" to ROW
                    add       smboxheight to row
          endif
          Write     htmlfile,seq;"</TABLE>"                 .TABLE 9
          move      NO to TABLEFLAG9
.
          if (SELNOTESFlag = YES)
                    Write     htmlfile,seq;"<TABLE FRAME=VSIDES BORDER=0 CELLPADDING=0 ALIGN=TOP CELLSPACING=0>"  .TABLE 10
                    Move      YES to TABLEFLAG10
.                   Write     htmlfile,seq;b15,"<TR>"
.                   Write     htmlfile,seq;b15,"<TD WIDTH=70>&nbsp</TD>"
.                   Write     htmlfile,seq;b15,"</TR>"
                    Write     htmlfile,seq;b15,"<TR>"
                    Write     htmlfile,seq;b15,"<TD ALIGN=LEFT>"
                    Write     htmlfile,seq;"<P>"
                    Write     htmlfile,seq;"<PRE><B>*See list description for additional information"
                    Write     htmlfile,seq;"<PRE> regarding this select.</B>"
.                   Write     htmlfile,seq;"<B>status will remain as ordered.  Orders cancelled by mailer prior to mail date </B>"
.                   Write     htmlfile,seq;"<B>are subject to a $75.00 processing fee.</PRE></B>>"
                    Write     htmlfile,seq;"</PRE></P>"
                    Write     htmlfile,seq;b15,"</TD>"
                    Write     htmlfile,seq;b15,"</TR>"
                    Write     htmlfile,seq;"</TABLE>"                 .TABLE 10
                    Move      NO to TABLEFLAG10
          Endif
.
          Write     htmlfile,seq;"<TABLE FRAME=VSIDES BORDER=0 CELLPADDING=0 ALIGN=TOP CELLSPACING=0>"  .TABLE 10
          Move      YES to TABLEFLAG10
          Write     htmlfile,seq;b15,"<TR>"
          Write     htmlfile,seq;b15,"<TD WIDTH=70>&nbsp</TD>"
          Write     htmlfile,seq;b15,"</TR>"
          Write     htmlfile,seq;b15,"<TR>"
          Write     htmlfile,seq;b15,"<TD ALIGN=LEFT>"
.begin patch 1.84
          If        (rentFlag = Yes & (ELSTCDE = "C" or Elstcde = "P"))
.begin patch 1.98
          Write     htmlfile,seq;"<P>"
          Write     htmlfile,seq;"<FONT SIZE=1><PRE><B>Mailers choosing not to make their list available may be subject to a nonreciprocal fee on rentals.</B>"
          Write     htmlfile,seq;"<B>*Any quantity below minimum is subject to a flat fee plus applicable base, select and additional</B>"
          Write     htmlfile,seq;"<B>charges. On quantities below 5,000 a $100/flat fee is imposed, Below 1,000 the fee is $200/flat. </PRE></B></FONT>"
          Write     htmlfile,seq;"<P>"
.          Write     htmlfile,seq;"<FONT SIZE=1><PRE><B>*Any quantity below minimum is subject to a flat fee plus applicable base, select and additional</B>"
.          Write     htmlfile,seq;"<B>charges. On quantities below 5,000 a $100/flat fee is imposed, Below 1,000 the fee is $200/flat. </PRE></B></FONT>"
.end patch 1.98
.         Write     htmlfile,seq;"<FONT SIZE=1><PRE><B>*Any quantity below minimum is subject to a $100/flat fee in addition to applicable base, select and </B>"
.         Write     htmlfile,seq;"<B>additional charges. On quantities below 1,000 a $200/flat fee is imposed.  </PRE></B></FONT>"
          Endif
          Write     htmlfile,seq;"<P>"
          Write     htmlfile,seq;"<FONT SIZE=1><PRE><B>Full payment is required on orders not cancelled prior to the maildate. If exchange, status will</B>"
          Write     htmlfile,seq;"<B>remain as ordered. Orders cancelled by mailer prior to mail date are subject to a $100.00/flat </B>"
.         Write     htmlfile,seq;"<FONT SIZE=1><PRE><B>Full payment is required on orders cancelled after the mail date.  If exchange,</B>"
.         Write     htmlfile,seq;"<B>status will remain as ordered.  Orders cancelled by mailer prior to mail date </B>"
.begin patch 1.83
.         Write     htmlfile,seq;"<B>are subject to a $75.00 processing fee.</PRE></B></FONT>"
.         Write     htmlfile,seq;"<B>are subject to a $100.00 processing fee.</PRE></B></FONT>"
          Write     htmlfile,seq;"<B>cancellation fee plus running and shipping charges.</PRE></B></FONT>"
.end patch 1.83
.end patch 1.84
          Write     htmlfile,seq;"</P>"
          Write     htmlfile,seq;b15,"</TD>"
          Write     htmlfile,seq;b15,"</TR>"
          Write     htmlfile,seq;"</TABLE>"                 .TABLE 10
          move      NO to TABLEFLAG10
          Write     htmlfile,seq;b15,"</TD>"
          Write     htmlfile,seq;b10,"</TABLE>"   .TABLE 5
          move      NO to TABLEFLAG5
          Write     htmlfile,seq;"</TABLE>"                 .TABLE 1
          move      NO to TABLEFLAG1
          Write     htmlfile,seq;"</TABLE>"                 .TABLE A
.         Write     htmlfile,seq;"</TD>"
.         Write     htmlfile,seq;"</TR>"
.patch1.1.2
.START PATCH 1.1.9 REPLACED LOGIC
.         if (printflag = 1)
          if (printflag = 1 & !externalmode)
.END PATCH 1.1.9 REPLACED LOGIC
.START PATCH 1.1.4 REPLACED LOGIC
.>Patch 1.2 Code Modified
.                   Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=\\nts2\c\HTTP\Datacards\nameslogo.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News Califoria">"
.                   Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=\\nts2\c\HTTP\Datacards\nameslogo2.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News">"     ."
.START PATCH 1.8 REPLACED LOGIC
.                   Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=\\nts0\c\netutils\images\nameslogo2.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News">"    ."
.                    if (ELSTCDE = "P")
.                              call      WritePLILogo
.                    else
                              Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=\\nins1\e\netutils\images\nameslogo2.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News">"    ."
.                    endif
.END PATCH 1.8 REPLACED LOGIC
.>Patch 1.2 Code Modified
.END PATCH 1.1.4 REPLACED LOGIC
          else
.patch1.1.2
.START PATCH 1.1.4 REPLACED LOGIC
.                   Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=nameslogo.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News Califoria">"
.START PATCH 1.8 REPLACED LOGIC
.                   Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=nameslogo2.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News">"         ."
.                    if (ELSTCDE = "P")
.                              call      WritePLILogo
.                    else
                              Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=nameslogo2.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News">"         ."
.                    endif
.END PATCH 1.8 REPLACED LOGIC
.END PATCH 1.1.4 REPLACED LOGIC
          endif
          Write     htmlfile,seq;b15,"</P>"
          RETURN
.START PATCH 1.8 REPLACED LOGIC
WritePLILogo
                              Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=nameslogo2.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News">"         ."
                              Return
                              
          write     htmlfile,seq;b15,"<TABLE>"
          write     htmlfile,seq;b15,"<TR>"
          write     htmlfile,seq;b15,"<TD VALIGN=TOP ALIGN=CENTER><B><PRE><FONT SIZE=6 FACE=#"times new roman#">Pacific Lists, Inc.</FONT></PRE></B></TD>"
          write     htmlfile,seq;b15,"</TR>"
          write     htmlfile,seq;b15,"<TR>"
          write     htmlfile,seq;b15,"<TD VALIGN=TOP ALIGN=CENTER><B><PRE><FONT FACE=#"times new roman#">"
          write     htmlfile,seq;"180 Grand Ave, Suite 1365"
          write     htmlfile,seq;"Oakland, CA 94612-3716"
          write     htmlfile,seq;"415-945-9450 · Fax 415-945-9451"
          write     htmlfile,seq;"<A  HREF=#"http://www.PacificLists.com#" target=#"_blank#">www.PacificLists.com</A>"
          write     htmlfile,seq;"A Division of <A HREF=#"http://www.NamesintheNews.com#" target=#"_blank#">Names in the News</A>"
          write     htmlfile,seq;b15,"</FONT></PRE></B></TD>"
          write     htmlfile,seq;b15,"</TR>"
          write     htmlfile,seq;b15,"</TABLE>"
          return
.END PATCH 1.8 REPLACED LOGIC
EndDetailTable
          Write     htmlfile,seq;"</TABLE>"                 .TABLE 9
          move      NO to TableFlag9
EndDetailTable2
          Write     htmlfile,seq;b10,"</TABLE>"   .TABLE 5
          move      NO to TableFlag5
          Write     htmlfile,seq;"</TABLE>"                 .TABLE 1
          move      NO to TableFlag1
          Write     htmlfile,seq;"</TABLE>"                 .TABLE A
.         Write     htmlfile,seq;"</TD>"
.         Write     htmlfile,seq;"</TR>"
.START PATCH 1.1.4 REPLACED LOGIC
.         Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=nameslogo.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News Califoria">"
.START PATCH 1.8 REPLACED LOGIC
.         Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=\\nts0\c\netutils\images\nameslogo2.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News">"
.          if (ELSTCDE = "P")
.                    call      WritePLILogo
.          else
                    Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=\\nins1\e\netutils\images\nameslogo2.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News">"       .//"
.          endif
.END PATCH 1.8 REPLACED LOGIC
.END PATCH 1.1.4 REPLACED LOGIC
          Write     htmlfile,seq;b15,"</TD>"
          Write     htmlfile,seq;b15,"</TR>"
          Write     htmlfile,seq;b15,"</P>"
          return
.This code is touchy.  CheckPlaces1 it a checkpoint to see what sections have finished in the body of the datacard and which have not.  If a section has not been started
.it is given a 1.  if it has been started but not finished it is given a 2.  If a section has completed is it given a three.  It has a very distinct order
.so be carefull when changing!!!!!!
CheckPlaces1
          if (SelectFlag1 = "1")
                    goto Selects
          elseif (SelectFlag1 = "2")
                    call      CreateTable5Indent
                    Write     htmlfile,seq;b15,"<TD>"
                    Call      CreateTable6
                    goto ContinueSelects
          endif
          if (TextFlag = "1")
                    move      "10" to ROW
                    Add       "90" to ROW
                    add       smboxheight to row
                    Call      CreateTable5
                    goto      Text
          elseif (TextFlag = "2")
                    move      "10" to ROW
                    Add       "90" to ROW
                    add       smboxheight to row
                    CALL      CreateTable5
                    Write     htmlfile,seq;b20,"<TR>"
                    Write     htmlfile,seq;b15,"<TD WIDTH=70>&nbsp</TD>"
                    Write     htmlfile,seq;b20,*LL,"<TD WIDTH=225>"
                    Write     htmlfile,seq;"<P><PRE>"
                    goto ContinueText
          elseif (TextFlag = "3")
                    move      "10" to ROW
                    Add       "90" to ROW
                    add       smboxheight to row
                    CALL      CreateTable5
                    Write     htmlfile,seq;b20,"<TR>"
                    Write     htmlfile,seq;b15,"<TD WIDTH=70>&nbsp</TD>"
                    Write     htmlfile,seq;b20,*LL,"<TD WIDTH=225>"
                    Write     htmlfile,seq;"<P><PRE>"
                    goto ContinueText1
          endif
          if (SelectFlag2 = "1")
                    move      "10" to ROW
                    Add       "90" to ROW
                    add       smboxheight to row
                    Call      CreateTable5
                    goto      AdditionalSelects
          elseif (SelectFlag2 = "2")
                    move      "10" to ROW
                    Add       "90" to ROW
                    add       smboxheight to row
                    Call      CreateTable5
                    call      CreateTable7
                    Write     htmlfile,seq;b20,"<TR>"
                    Write     htmlfile,seq;b20,"<TD WIDTH=50 ALIGN=RIGHT>"
                    Write     htmlfile,seq;b20,"<P><PRE>"
                    goto ContinueSelects2
          endif
          Move      YES to SectionOne
          If ((SectionOne = YES)&(SectionTwo = YES))
                    if (ENDDETAILFLAG <> YES)
                              call      enddetail
                    endif
                    Goto READRecord
          Else
                    Goto CheckPlaces2
          Endif
.This code is touchy.  CheckPlaces2 it a checkpoint to see what sections have finished in the charges section of the datacard and which have not.  If a section has not been started
.it is given a 1.  if it has been started but not finished it is given a 2.  If a section has completed is it given a three.  It has a very distinct order
.so be carefull when changing!!!!!!
CheckPlaces2
          If ((SectionOne <> YES) | (SECTIONTWO <> YES))
                    Call      Header
.
.                   goto CheckPlaces1 if (SectionOne = NO & SectionTwo = YES)
.
          endif
          if (ArrangementFlag = "1")
                    goto Arrangement
          elseif (ArrangementFlag = "2")
                    if (#NUMLV3 < #LV3)
                              move      #N9 to row
                              Call      CreateTable3
                              Write     htmlfile,seq;b10,"<TR>"
                              Write     htmlfile,seq;b10,"<TD VALIGN=TOP ALIGN=CENTER><B><PRE>ARRANGEMENT(Cont.)</PRE></B></TD>"
                              Write     htmlfile,seq;b10,"</TR>"
                              Write     htmlfile,seq;b20,"<TR>"
                              Write     htmlfile,seq;b20,*LL,"<TD WIDTH=225>"
                              Write     htmlfile,seq;b20,"<P><PRE>"
                              add       SingleSpaced to Row
                              goto Continuearrangement
                    else
                              Move      "3" to ArrangementFlag
                    endif
          endif
          if (AddressingFlag = "1")
                    Call      CreateTable3
                    Goto Addressing
          elseif (AddressingFlag = "2")
                    move      #N9 to row
                    call      NADDKG
                    if not over
                              Call      CreateTable3
                              Write     htmlfile,seq;b20,"<TR>"
                              Write     htmlfile,seq;b20,"<TD VALIGN=TOP ALIGN=CENTER><B><PRE>*ADDRESSING(Cont.)*</PRE></B></TD>"
                              Write     htmlfile,seq;b20,"</TR>"
                              call      CreateTable4
                              add       SingleSpaced to Row
                              goto ContinueAddressing
                    else
                              Move      "3" to AddressingFlag
                    endif
          endif
          if (SourceFlag = "1")
                    Call      CreateTable3
                    Goto Source
          elseif (SourceFlag = "2")
                    move      #N9 to row
                    call      NSRCKG
                    if not over
                              Call      CreateTable3
                              Write     htmlfile,seq;b10,"<TR>"
                              Write     htmlfile,seq;b10,"<TD VALIGN=TOP ALIGN=CENTER><B><PRE>****SOURCE(Cont.)****</PRE></B></TD>"
                              Write     htmlfile,seq;b10,"</TR>"
                              call      CreateTable4
                              add       SingleSpaced to Row
                              goto ContinueSource
                    else
                              move      "3" to SourceFlag
                    endif
          endif
          if (SelectionsFlag = "1")
                    call      CreateTable3
                    Goto Selections
          elseif (SelectionsFlag = "2")
                    move      #N9 to row
                    call      NSLTKG
                    if not over
                              call      CreateTable3
                              Write     htmlfile,seq;b10,"<TR>"
                              Write     htmlfile,seq;b10,"<TD VALIGN=TOP ALIGN=CENTER><B><PRE>*SELECTIONS(Cont.)*</PRE></B></TD>"
                              Write     htmlfile,seq;b10,"</TR>"
                              call      CreateTable4
                              add       SingleSpaced to Row
                              goto ContinueSelections
                    else
                              move      "3" to SelectionsFlag
                    endif
          endif
          Move      YES to SectionTwo
          Goto CloseUpLeftTable
CloseTextTable
          Write     htmlfile,seq;b20,"</TR>"
          CALL      CloseTable5
          Write     htmlfile,seq;b15,"</TR>"
          Write     htmlfile,seq;b10,"</TABLE>" .Table 1
          move      NO to TABLEFLAG1
          Return

CreatePage
          add       c1 to pagenum
.START PATCH 1.1.9 REPLACED LOGIC
.         clear     str25
.         append    "c:\work\data" to str25
.         append    lstnum to str25
.         append    ".htm" to str25
.         reset     str25
.         rep       zfill in str25
..START PATCH 1.1 ADDED LOGIC
.         erase     str25
..END PATCH 1.1 ADDED LOGIC
.         prepare   htmlfile,str25,exclusive
.................................
          if (externalmode)
                    clear     str75
.                    append    "E:\ninserv\webapp\data\datacards\data",str55     ."
                    append    "E:\NINSERV\NamesintheNews\wordpress\datacards\data",str75     ."
                    append    lstnum to str75
                    append    ".htm" to str75
                    reset     str75
                    rep       zfill in str75
                    erase     str75
                    trap ExtTrap if IO
                    prepare   htmlfile,str75
.                    prepare   htmlfile,str75,exclusive
.begin patch 1.9
          clear     #N2
          call      newpage
.end patch 1.9
                    trapclr IO
          else
.>Patch 1.4         Code Added for creation of Usage PDF
                    if (inits = "1")    . Yes Create Weekly Usage for Website
                              move      c1 to usageflag
                              Pack PRTFILENAME with "Usg",LSTNUM
.begin patch 1.96
                        pack    PrintName from "c:\work\pdf\",prtFileName,".pdf"      
.                        PRTOPEN UsagePrint,"PDF995",PrtFileName
                        PRTOPEN UsagePrint,"PDF:",PrintName
.                        pack    PrintName,PrtFileName,".PDF"
.end patch 1.96
.argh  DH
                              MOve      c1,company
.                              if (ELSTCDE = "P")
.                              MOve      c2,company
.                              ENDIF
.                             call      PrintUsageToPFile using LSTNUM,UsageFlag,UsagePrint
                              call      PrintUsageToPFile using LSTNUM,UsageFlag,UsagePrint,company
                              PrtClose UsagePrint
                              Pause c5
                    endif
.>Patch 1.4         Code added for creation of Usage

                    clear     str25
                    append    "c:\work\data" to str25
                    append    lstnum to str25
                    append    ".htm" to str25
                    reset     str25
                    rep       zfill in str25
                    erase     str25
                    prepare   htmlfile,str25,exclusive
.begin patch 1.9
          clear     #N2
          call      newpage
.end patch 1.9
.START PATCH 1.2.1 ADDED LOGIC
                    if (PrintFlag <> 1)
//Create new records in NINDATW
                              move      lstnum,NDATWFLD
                              rep       zfill,NDATWFLD
                              move      "NDATWTST",Location
                              pack      KeyLocation,"Key: ",NDATWFLD
                              call      NDATWTST
                              if over
                              //should really always be an over, but we are double-checking
                                        move      NDATWFLD,NDATWLST
                                        move      "NDATWWRT",Location
                                        call      NDATWWRT
                              endif
                    endif
.END PATCH 1.2.1 ADDED LOGIC
          endif
.END PATCH 1.1.9 REPLACED LOGIC
.begin patch 1.9
.         clear     #N2
.         call      newpage
.end patch 1.9
          return

.START PATCH 1.1.9 ADDED LOGIC
ExtTrap
         stream    *STDOUT,"Newpage Creating file =  ",str75," List = ",lstnum:
                   ". <br><br>",newline:
                   " <a href=#"#/index.php/contact-us/#";#"> website administrator</a> to correct this problem."

          return
.END PATCH 1.1.9 ADDED LOGIC

Header
.ContinueCard
          add       c1 to pagenum
          clear     str25
          append    "c:\work\data" to str25
          append    lstnum to str25
          append    "c:\work\data" to #str25
          append    lstnum to #str25
          clear     str2
          move      #n2 to b2
          add       c1 to #n2
          move      #N2 to str2
          append    str2 to str25
          append    b2 to #str25
          append    ".htm" to #str25
          append    ".htm" to str25
          reset     #str25
          reset     str25
          rep       zfill in #str25
          rep       zfill in str25
          weof      htmlfile,seq
          close     htmlfile
          prepare   htmlfile,str25
          if        (#n2 = 99)
           alert type=yesno1," Exceeded Page count? (if No, I will mark as Pacific Lists)",n1
                   if (n1=6)    . 6 = yes , 7 = no          
                    Shutdown
                   Elseif (n1=7)    . 6 = yes , 7 = no          
                    Shutdown
                   endif
          endif
          call      newpage
          return

AddLinks
          clear     n2
          clear     n3
          clear     Taskname
          sub       c1 from pagenum
          if (pagenum <> c0)
                    for n2,"0",PageNum
                              clear     str30
.START PATCH 1.1.9 REPLACED LOGIC
.                             if (PrintFlag = 1)
                              if (PrintFlag = 1 & !externalmode)
.END PATCH 1.1.9 REPLACED LOGIC
                                        append    "c:\work\data" to str30
                              else
                                        append    "#"" to str30       ."
                                        append    "data" to str30
                              endif
                              append    lstnum to str30
                              clear     str2
                              clear     str3
                              move      N2 to str2
                              add       c1 to n2,n3
                              move      n3 to str3
                              if (n2 <> C0)
                                        append    str2 to str30
                              endif
                              append    ".htm" to str30
                              reset     str30
                              rep       zfill in str30
                              if (n2 <> C0)
                                        append    "  &##183; " with taskname
                              endif
.START PATCH 1.1.9 REPLACED LOGIC
.                             if (PrintFlag = 1)
                              if (PrintFlag = 1 & !externalmode)
.END PATCH 1.1.9 REPLACED LOGIC
                                        pack      str55,"<A  HREF=",str30,">",str3,"</A>"
                              else
                                        pack      str55,"<A  HREF=",str30,"#"",">",str3,"</A>"      ."
                              endif
                              append    str55 with taskname
                    repeat
                    reset taskname
                    clear n2
.
                    for n2,"0",PageNum
                              clear     str30
                              append    "c:\work\data" to str30
                              append    lstnum to str30
                              clear     str2
                              move      N2 to str2
                              if (n2 <> C0)
                                        append    str2 to str30
                              endif
                              append    ".htm" to str30
                              reset     str30
                              rep       zfill in str30
                              open      htmlfile,str30,exclusive
                              positeof htmlfile
.                                          open   htmlfile,str25,exclusive
                              if (TABLEFLAG5 = YES)
                                        call      closetable5
                              ENDIF
                              Write     htmlfile,seq;b5,"<P ALIGN=CENTER>"
                              Write     htmlfile,seq;b10,Taskname
                              Write     htmlfile,seq;b5,"</P>"
                              Write     htmlfile,seq;"</P>"
.begin patch 1.91
                              Write     htmlfile,seq;"<script type=#"text/javascript#">"
                              Write     htmlfile,seq;"var gaJsHost = ((#"https:#" == document.location.protocol) ? #"https://ssl.#" : #"http://www.#");"
                              Write     htmlfile,seq;"document.write(unescape(#"%3Cscript src='#" + gaJsHost + #"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E#"))"
                              Write     htmlfile,seq;"</script>"
                              Write     htmlfile,seq;"<script type=#"text/javascript#">"
                              Write     htmlfile,seq;"try {var pageTracker = _gat._getTracker(#"UA-6716303-4#");pageTracker._trackPageview();"
                              Write     htmlfile,seq;"} catch(err) {}</script>"
.end patch 1.91

                              Write     htmlfile,seq;"</BODY>"
                              Write     htmlfile,seq;"</HTML>"
                              close     htmlfile
                    repeat
          else
                    clear     str30
                    append    "c:\work\data" to str30
                    append    lstnum to str30
                    clear     str2
                    append    ".htm" to str30
                    reset     str30
                    rep       zfill in str30
                    open      htmlfile,str30,exclusive
                    positeof htmlfile
                    Write     htmlfile,seq;"</P>"
.begin patch 1.91
                              Write     htmlfile,seq;"<script type=#"text/javascript#">"
                              Write     htmlfile,seq;"var gaJsHost = ((#"https:#" == document.location.protocol) ? #"https://ssl.#" : #"http://www.#");"
                              Write     htmlfile,seq;"document.write(unescape(#"%3Cscript src='#" + gaJsHost + #"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E#"))"
                              Write     htmlfile,seq;"</script>"
                              Write     htmlfile,seq;"<script type=#"text/javascript#">"
                              Write     htmlfile,seq;"try {var pageTracker = _gat._getTracker(#"UA-6716303-4#");pageTracker._trackPageview();"
                              Write     htmlfile,seq;"} catch(err) {}</script>"
.end patch 1.91
                    Write     htmlfile,seq;"</BODY>"
                    Write     htmlfile,seq;"</HTML>"
                    close     htmlfile
          endif
          Return

ClearVars
          move      "1" to ArrangementFlag
          move      "1" to AddressingFlag
          move      "1" to SourceFlag
          move      "1" to SelectionsFlag
          move      "1" to FormatFlag
          move      "1" to SelectFlag1
          move      "1" to SelectFlag2
          move      "1" to TextFlag
          move      "N" to OwnerFlag
          move      "N" to EndDetailFlag
          move      NO to SELNOTESFlag
          move      c0 to PageNum
          move      c0 to #LV
          move      c0 to #NUMLV
          move      c0 to #LVA
          move      c0 to #NUMLVA
          move      c0 to #LVB
          move      c0 to #NUMLVB
          move      c0 to #LV2A
          move      c0 to #NUMLV2A
          move      c0 to #LV2B
          move      c0 to #NUMLV2B
          move      c0 to #LV3
          move      c0 to #NUMLV3
          move      c0 to #LV4
          move      c0 to #NUMLV4
          move      c0 to #NUMLV4A
          move      c0 to #LV5
          move      c0 to #NUMLV5
          move      c0 to #NUMLV5A
          move      c0 to #LV6
          move      c0 to #NUMLV6
          move      c0 to #NUMLV6A
          clear     #str25
          move      c0 to N52
          move      NO to SectionOne
          move      NO to SectionTwo
          SelectListView.deleteallitems
          SelectAListView.deleteallitems
          SelectBListView.deleteallitems
          Select2ListView.deleteallitems
          Select2AListView.deleteallitems
          Select2BListView.deleteallitems
          Select3ListView.deleteallitems
          Select4ListView.deleteallitems
          Select4aListView.deleteallitems
          Select5ListView.deleteallitems
          Select5aListView.deleteallitems
          Select6ListView.deleteallitems
          Select6AListView.deleteallitems
          move      NO to TABLEFLAG1
          move      NO to TABLEFLAG2
          move      NO to TABLEFLAG3
          move      NO to TABLEFLAG4
          move      NO to TABLEFLAG5
          move      NO to TABLEFLAG6
          move      NO to TABLEFLAG7
          move      NO to TABLEFLAG8
          move      NO to TABLEFLAG9
          move      NO to TABLEFLAG10
          return
.Creation of listview object to house the data.
CreateObjects
          CREATE    White=*WHITE
          CREATE    BLACK=*BLACK
          CREATE    selectlistview=1:50:1:50,SORTHEADER=1,SORTORDER=1:
                    visible=0,ENABLED=0
          Selectlistview.INSERTCOLUMN USING "Index",30,0
          Selectlistview.INSERTCOLUMN USING "Select Num",30,1
          Selectlistview.INSERTCOLUMN USING "ListNum",100,2
          Selectlistview.INSERTCOLUMN USING "Notes",100,3
          ACTIVATE selectlistview
          setprop   SelectListView,visible=c0
.SelectsUniverse
          CREATE    selectAlistview=1:50:50:200,SORTHEADER=1,SORTORDER=1:
                    visible=0,ENABLED=0
          SelectAlistview.INSERTCOLUMN USING "Index",30,0
          SelectAlistview.INSERTCOLUMN USING "Universe",30,1
          ACTIVATE selectAlistview
          setprop   SelectAListView,visible=c0
.SelectsPrice
          CREATE    selectBlistview=1:50:50:200,SORTHEADER=1,SORTORDER=1:
                    visible=0,ENABLED=0
          SelectBlistview.INSERTCOLUMN USING "Index",30,0
          SelectBlistview.INSERTCOLUMN USING "Price",30,1
          SelectBlistview.INSERTCOLUMN USING "List",30,2
          SelectBlistview.INSERTCOLUMN USING "Nselprice",30,3
          SelectBlistview.INSERTCOLUMN USING "NSELEXC",30,4
          ACTIVATE selectBlistview
          setprop   SelectBListView,visible=c0
.AdditionalSelects2
          CREATE    select2listview=1:50:50:200,SORTHEADER=1,SORTORDER=1:
                    visible=0,ENABLED=0
          Select2listview.INSERTCOLUMN USING "Index",30,0
          Select2listview.INSERTCOLUMN USING "Select Num",30,1
          Select2listview.INSERTCOLUMN USING "ListNum",100,2
          Select2listview.INSERTCOLUMN USING "Notes",100,3
          ACTIVATE select2listview
          setprop   Select2ListView,visible=c0
.AdditionalSelects2Universe
          CREATE    select2Alistview=1:50:50:200,SORTHEADER=1,SORTORDER=1:
                    visible=0,ENABLED=0
          Select2Alistview.INSERTCOLUMN USING "Index",30,0
          Select2Alistview.INSERTCOLUMN USING "Universe",30,1
          Select2listview.INSERTCOLUMN USING "Select Num",30,2
          Select2Alistview.INSERTCOLUMN USING "ListNum",30,3
          ACTIVATE select2Alistview
          setprop   Select2AListView,visible=c0
.Additional Selects2Price
          CREATE    select2Blistview=1:50:50:200,SORTHEADER=1,SORTORDER=1:
                    visible=0,ENABLED=0
          Select2Blistview.INSERTCOLUMN USING "Index",30,0
          Select2Blistview.INSERTCOLUMN USING "Price",30,1
          Select2Blistview.INSERTCOLUMN USING "List",30,2
          Select2Blistview.INSERTCOLUMN USING "Nselprice",30,3
          Select2Blistview.INSERTCOLUMN USING "NSELEXC",30,4
          Select2Blistview.INSERTCOLUMN USING "Base",30,5
          ACTIVATE select2Blistview
          setprop   Select2BListView,visible=c0
.Price
ArrangementListview
          CREATE    select3listview=1:50:50:200:
                    visible=0,ENABLED=0
          Select3listview.INSERTCOLUMN USING "Index",30,0
          Select3listview.INSERTCOLUMN USING "Select Num",30,1
          Select3listview.INSERTCOLUMN USING "ListNum",100,2
          Select3listview.INSERTCOLUMN USING "Mod",100,3
          ACTIVATE select3listview
          setprop   Select3ListView,visible=c0

AddressingListview
          CREATE    select4listview=1:50:50:200:
                    visible=0,ENABLED=0
.                   APPEARANCE=1:
.                   BORDER=1,BGCOLOR=white:
.                   TABID=6,FULLROW=1:
.                   FONT=FONT12,FGCOLOR=black:
          Select4listview.INSERTCOLUMN USING "Index",30,0
          Select4listview.INSERTCOLUMN USING "Select Num",30,1
          Select4listview.INSERTCOLUMN USING "ListNum",100,2
          Select4listview.INSERTCOLUMN USING "Mod",100,3
          ACTIVATE select4listview
          setprop   Select4ListView,visible=c0

AddressingModListview
          CREATE    Select4Alistview=1:50:50:200:
                    visible=0,ENABLED=0
          Select4Alistview.INSERTCOLUMN USING "Index",30,0
          Select4Alistview.INSERTCOLUMN USING "Select Num",30,1
          Select4Alistview.INSERTCOLUMN USING "ListNum",100,2
          Select4Alistview.INSERTCOLUMN USING "Mod",100,3
          ACTIVATE select4Alistview
          setprop   Select4AListView,visible=c0

SourceListview
          CREATE    select5listview=1:50:50:200:
                    visible=0,ENABLED=0
          Select5listview.INSERTCOLUMN USING "Index",30,0
          Select5listview.INSERTCOLUMN USING "Select Num",30,1
          Select5listview.INSERTCOLUMN USING "ListNum",100,2
          Select5listview.INSERTCOLUMN USING "Mod",100,3
          ACTIVATE select5listview
          setprop   Select5ListView,visible=c0
SourceModListview
          CREATE    Select5Alistview=1:50:50:200:
                    visible=0,ENABLED=0
          Select5Alistview.INSERTCOLUMN USING "Index",30,0
          Select5Alistview.INSERTCOLUMN USING "Select Num",30,1
          Select5Alistview.INSERTCOLUMN USING "ListNum",100,2
          Select5Alistview.INSERTCOLUMN USING "Mod",100,3
          ACTIVATE select5Alistview
          setprop   Select5AListView,visible=c0

SelectionsListview
          CREATE    select6listview=1:50:50:200:
                    visible=0,ENABLED=0
          Select6listview.INSERTCOLUMN USING "Index",30,0
          Select6listview.INSERTCOLUMN USING "Select Num",30,1
          Select6listview.INSERTCOLUMN USING "ListNum",100,2
          Select6listview.INSERTCOLUMN USING "Mod",100,3
          ACTIVATE select6listview
          setprop   Select6ListView,visible=c0

SelectionsModListview
          CREATE    Select6Alistview=1:50:50:200:
                    visible=0,ENABLED=0
          Select6Alistview.INSERTCOLUMN USING "Index",30,0
          Select6Alistview.INSERTCOLUMN USING "Select Num",30,1
          Select6Alistview.INSERTCOLUMN USING "ListNum",100,2
          Select6Alistview.INSERTCOLUMN USING "Mod",100,3
          ACTIVATE select6Alistview
          setprop   Select6AListView,visible=c0
          return

.START PATCH 1.1 ADDED LOGIC
CreateWebCard Routine DimPtr
.DimPtr  = List Number
          call      Trim using DimPtr
          if (DimPtr <> "")
                    move      C1,PrintFlag
                    move      C1,NDATPATH
                    packkey   NDATFLD,DimPtr
                    rep       zfill,NDATFLD
                    move      "PrintWeb-NDATKEY",Location
                    pack      KeyLocation,"Key: ",NDATFLD
                    call      NDATKEY
                    if not over
.Patch1.1.2
                              Call OwnerInfo
.patch1.1.2
                              call      CreateObjects
                              call      clearvars
.                             move      c1 to NDATCONV
                              CALL      createpage
                              call      Arrangement
                              destroy   White
                              destroy   BLACK
                              destroy   selectlistview
                              destroy   select2listview
                    endif
          endif
          return
.END PATCH 1.1 ADDED LOGIC

.START PATCH 1.1.9 ADDED LOGIC
CreateWebCardW Routine DimPtr
.DimPtr  = List Number
          call      Trim using DimPtr
          if (DimPtr <> "")
                    move      C1,PrintFlag
                    move      C1,NDATPATH
                    packkey   NDATFLD,DimPtr
                    rep       zfill,NDATFLD
                    move      "PrintWebW-NDATKEY",Location
                    pack      KeyLocation,"Key: ",NDATFLD
                    call      NDATKEY
                    if not over
.                             prep      tempfile,"c:\work\drewboy.dat"
                              set       externalmode
                              call      CreateObjects
.                             write     tempfile,SEQ;"createobjects successful"
                              call      clearvars
.                             write     tempfile,SEQ;"clearvars successful"
                              CALL      createpage
.                             write     tempfile,SEQ;"createpage successful"
                              call      Arrangement
.                             write     tempfile,SEQ;"Arrangement successful"
                              destroy   White
                              destroy   BLACK
                              destroy   selectlistview
                              destroy   select2listview
                    endif
          endif
          return
.END PATCH 1.1.9 ADDED LOGIC

CreateTable3
          if (TABLEFLAG3 = NO)
.                   Write     htmlfile,seq;b15,"<TD VALIGN=TOP>"
.                   Write     htmlfile,seq;b15,"<TR>"
                    Write     htmlfile,seq;b15,"<TABLE BORDER=0 CELLPADDING=0 BORDERCOLOR=BLACK CELLSPACING=0>"                                       .TABLE 3
                    move YES to TABLEFLAG3
          endif
          return
CreateTable4
          if (TABLEFLAG4 = NO)
                    Write     htmlfile,seq;b15,"<TR>"
                    Write     htmlfile,seq;b15,"<TD>"
                    Write     htmlfile,seq;b15,"<TABLE BORDER=0 CELLPADDING=0 BORDERCOLOR=BLACK CELLSPACING=0>"                                       .TABLE 4
                    move YES to TABLEFLAG4
          endif
          return
CloseTable4
          if (TABLEFLAG4 = YES)
                    Write     htmlfile,seq;b15,"</TABLE>"   .TABLE 4
                    Write     htmlfile,seq;b15,"</TR>"
                    Write     htmlfile,seq;b15,"</TD>"
                    move NO to TABLEFLAG4
          endif
          return
CreateTable5
          if (TABLEFLAG5 = NO)
                    Write     htmlfile,seq;b10,"<TD ROWSPAN=20 COLSPAN=20 VALIGN=TOP>" >TABLE 1
                    Write     htmlfile,seq;b10,"<TABLE FRAME=VSIDES BORDER=0 CELLPADDING=0 ALIGN=TOP CELLSPACING=0 WIDTH=390>"    .TABLE 5
                    move YES to TABLEFLAG5
          endif
          Return
CreateTable5Indent
          if (TABLEFLAG5 = NO)
                    call      CreateTable5
                    Write     htmlfile,seq;b15,"<TR>"
                    Write     htmlfile,seq;b15,"<TD WIDTH=70 VALIGN=TOP><PRE>&nbsp</PRE></TD>"
          else
                    Write     htmlfile,seq;b15,"<TR>"
                    Write     htmlfile,seq;b15,"<TD WIDTH=70 VALIGN=TOP><PRE>&nbsp</PRE></TD>"
          endif
          return
CloseTable5
          Write     htmlfile,seq;b15,"</TABLE>"                                 .TABLE 5
          Write     htmlfile,seq;b15,"</TD>"
          move      NO to TABLEFLAG5
          return
CreateTable6
          Write     htmlfile,seq;b15,"<TABLE BORDER=0 CELLPADDING=0 CELLSPACING=0 WIDTH=390>"       .TABLE 6
          move      YES to TABLEFLAG6
          return
CloseTable6
          Write     htmlfile,seq;b15,"</TABLE>"
          Write     htmlfile,seq;b15,"</TD>"
          Write     htmlfile,seq;b15,"</TR>"
          move      NO to TABLEFLAG6
          return
CreateTable7
          Write     htmlfile,seq;b10,"<TABLE FRAME=VSIDES BORDER=0 CELLPADDING=0 ALIGN=TOP CELLSPACING=0>"    .TABLE 7
          Write     htmlfile,seq;b15,"<TR>"
          Write     htmlfile,seq;b15,"<TD WIDTH=70><PRE>&nbsp</PRE></TD>"
          move      YES to TABLEFLAG7
          return
CreateTable8
          Write     htmlfile,seq;b10,"<TABLE BORDER=0 CELLPADDING=0 CELLSPACING=0 WIDTH=390>"       .TABLE 8
          Write     htmlfile,seq;b20,"<TR>"
          Write     htmlfile,seq;b20,*LL,"<TD WIDTH=50 ALIGN=RIGHT><PRE>","&nbsp","</PRE></TD>"
          Write     htmlfile,seq;b20,"<TD WIDTH=1>&nbsp</TD>"
          move      YES to TABLEFLAG8
          return
CloseUpRightTable
          IF (TABLEFLAG9 = YES)
                    Write     htmlfile,seq;b15,"</TABLE>"  .TABLE 9
                    move      NO to TABLEFLAG9
          endif
          IF (TABLEFLAG8 = YES)
                    Write     htmlfile,seq;b10,"</TABLE>"  .TABLE 8
                    move      NO to TABLEFLAG8
          endif
          IF (TABLEFLAG7 = YES)
                    Write     htmlfile,seq;b5,"</TABLE>" .TABLE 7
                    move      NO to TABLEFLAG7

          endif
          IF (TABLEFLAG6 = YES)
                    Write     htmlfile,seq;b5,"</TABLE>" .TABLE 6
                    move      NO to TABLEFLAG6
          endif
          return
PrintLogo
          Write     htmlfile,seq;"</TABLE>"                 .TABLE A
          Write     htmlfile,seq;"</TD>"
          Write     htmlfile,seq;"</TR>"
.START PATCH 1.1.4 REPLACED LOGIC
.         Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=nameslogo.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News Califoria">"
.START PATCH 1.8 REPLACED LOGIC
.         Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=\\nts0\c\netutils\images\nameslogo2.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News">"
.          if (ELSTCDE = "P")
.                    call      WritePLILogo
.          else
                    Write     htmlfile,seq;b15,"<IMG ALIGN=CENTER SRC=\\nins1\e\netutils\images\nameslogo2.jpg" WIDTH=308 HEIGHT=79 ALT="Names in the News">"
.          endif
.END PATCH 1.8 REPLACED LOGIC
.END PATCH 1.1.4 REPLACED LOGIC         ."
.         Write     htmlfile,seq;b15,"</TD>"
.         Write     htmlfile,seq;b15,"</TR>"
          Write     htmlfile,seq;b15,"</P>"
.         Write     htmlfile,seq;"</TABLE>"                 .TABLE A
          return
.Routines for Providing information for separate columns within nested tables.
.Listview Object are filled with info when select or selection charges are read then they are processed
.after all the data has been read for that section.  This are subsidiary routines to populate that data
.It's a little cludgy.

LoadAddressingMod
          Write     htmlfile,seq;b20,"<TD WIDTH=92>"
          Write     htmlfile,seq;"<P><PRE>"
          SUB       C1,#NUMLV4
          clear     n4
          for n4,#NUMLV4A,#NUMLV4
                    Select4Alistview.GetItemText giving str25 using n4,0
                    Write     htmlfile,seq;*LL,str25
          repeat
.This Holds the last number that was evaluated by this listview for addressing modifier
          move      #NUMlV4 to #NUMLV4A
          Write     htmlfile,seq;"</PRE>"
          Write     htmlfile,seq;"</P>"
          Write     htmlfile,seq;"</TD>"
          return
LoadSourceMod
          Write     htmlfile,seq;b20,"<TD WIDTH=92>"
          Write     htmlfile,seq;"<P><PRE>"
.   Write htmlfile,seq;"</TD>"
          SUB       C1,#NUMLV5
          clear     n4
          for n4,#NUMLV5A,#NUMLV5
                    Select5Alistview.GetItemText giving str4 using n4,0
                    Write     htmlfile,seq;*LL,str4
          repeat
.This Holds the last number that was evaluated by this listview for Source modifier
          move      #NUMlV5 to #NUMLV5A
          Write     htmlfile,seq;"</PRE>"
          Write     htmlfile,seq;"</P>"
          Write     htmlfile,seq;"</TD>"
          return
LoadSelectionsMod
          Write     htmlfile,seq;b20,"<TD WIDTH=92>"
          Write     htmlfile,seq;"<P><PRE>"
          SUB       C1,#NUMLV6
          clear     n4
          for n4,#NUMLV6A,#NUMLV6
                    Select6Alistview.GetItemText giving str25 using n4,0
                    Write     htmlfile,seq;*LL,str25
          repeat
.This Holds the last number that was evaluated by this listview for selections modifier
          move      #NUMlV6 to #NUMLV6A
          Write     htmlfile,seq;"</PRE>"
          Write     htmlfile,seq;"</P>"
          Write     htmlfile,seq;"</TD>"
          return

LoadAdditionalSelectNames
          Write     htmlfile,seq;b20,"<TD WIDTH=280 ALIGN=LEFT>"
          Write     htmlfile,seq;"<P><PRE>"
          SUB       C1,#NUMLV2A
          clear     n4
          for n4,num9,#NUMLV2A
                    Select2listview.GetItemText giving NSELSNAME using n4,2
                    Select2listview.GetItemText giving NSELNOTES using n4,3
                    call      trim using nselsname
                    rep       lowup in NSELSNAME
                    if (NSELNOTES <> "1")
                              Write     htmlfile,seq;*LL,NSELSNAME
                    else
                              move      YES to SELNOTESFlag
                              Write     htmlfile,seq;*LL,NSELSNAME,"#*"
                    endif
.                   Write     htmlfile,seq;*LL,NSELSNAME
          repeat
.This Holds the last number that was evaluated by this listview for selections modifier
          move      #NUMLV2A to num9
          Write     htmlfile,seq;"</PRE>"
          Write     htmlfile,seq;"</P>"
          Write     htmlfile,seq;"</TD>"
          return
LoadAdditionalSelectMod
          Write     htmlfile,seq;b20,"<TD WIDTH=95 ALIGN=RIGHT>"
          Write     htmlfile,seq;"<P><PRE>"
          clear     n4
          for n4,#NUMLV2B,#NUMLV2A
                    clear     str8
                    clear     N8
                    Select2Blistview.GetItemText giving str25 using n4,1
                    call      trim using str25
                    Select2Blistview.GetItemText giving str8 using n4,3
                    move      str8 to N8
                    Select2Blistview.GetItemText giving NSELEXC using n4,4
                    Select2Blistview.GetItemText giving NSELBASE using n4,5
.;;;;;
                    If (n8 = c0)
.                             If (NSELEXC= "0")
.                                       Write     htmlfile,seq;*LL,"Exchange"
.                             elseif (NSELEXC= "1")
.                                       Write     htmlfile,seq;*LL,"Ex Only"
.                             else
                                        Write     htmlfile,seq;*LL,""
.                             endif
                    else
.                             If (NSELEXC= "0")
.                                       Write     htmlfile,seq;*LL,"Exch/",str25
.                             elseif (NSELEXC= "1")
.                                       Write     htmlfile,seq;*LL,"Ex Only/",str25
.                             else
                                        Write     htmlfile,seq;*LL,str25
.                             endif
                    endif
          repeat
.This Holds the last number that was evaluated by this listview for selections modifier
          move      #NUMLV2A to #NUMLV2B
          Write     htmlfile,seq;"</PRE>"
          Write     htmlfile,seq;"</P>"
          Write     htmlfile,seq;"</TD>"
          return
.
LoadSelectNames
          Write     htmlfile,seq;b20,"<TD WIDTH=280 ALIGN=LEFT>"
          Write     htmlfile,seq;"<P><PRE>"
          SUB       C1,#NUMLVA
          clear     n4
          for n4,#NUMLV,#NUMLVA
                    Selectlistview.GetItemText giving NSELSNAME using n4,2
                    Selectlistview.GetItemText giving NSELNOTES using n4,3
                    call      trim using nselsname
                    rep       lowup in NSELSNAME
          if (NSELNOTES <> "1")
                    Write     htmlfile,seq;*LL,NSELSNAME
          else
                    Write     htmlfile,seq;*LL,NSELSNAME,"#*"
          endif
          repeat
.This Holds the last number that was evaluated by this listview for selections modifier
          move      #NUMLVA to #NUMLV
          Write     htmlfile,seq;"</PRE>"
          Write     htmlfile,seq;"</P>"
          return
LoadSelectMod
          Write     htmlfile,seq;b20,"<TD WIDTH=95 ALIGN=RIGHT>"
          Write     htmlfile,seq;"<P><PRE>"
          clear     n4
          for n4,#NUMLVB,#NUMLVA
                    clear     str8
                    clear     N8
                    SelectBlistview.GetItemText giving str25 using n4,1
                    call      trim using str25
                    SelectBlistview.GetItemText giving str8 using n4,3
                    move      str8 to N8
                    SelectBlistview.GetItemText giving NSELEXC using n4,4
.;;;;;
                    If (n8 = c0)
                              If (NSELEXC= "1")
.                                       Write     htmlfile,seq;*LL,"&nbsp"
                                        Write     htmlfile,seq;*LL,"Exch/Rent"
                              elseif (NSELEXC= "2")
                                        Write     htmlfile,seq;*LL,"Exchange Only"
..                                      Write     htmlfile,seq;*LL,"&nbsp"
                              else
                                        Write     htmlfile,seq;*LL,""
.                                       Write     htmlfile,seq;*LL,b1
                              endif
                    else
                              If (NSELEXC= "1")
                                        Write     htmlfile,seq;*LL,"Exch/",str25
                              elseif (NSELEXC= "2")
                                        Write     htmlfile,seq;*LL,"Ex Only/",str25
                              else
                                        Write     htmlfile,seq;*LL,str25
                              endif
                    endif
          repeat
.This Holds the last number that was evaluated by this listview for selections modifier
.         add c1 to #NUMLV4A
          move      #NUMLVA to #NUMLVB
          Write     htmlfile,seq;"</PRE>"
          Write     htmlfile,seq;"</P>"
          Write     htmlfile,seq;"</TD>"
          return
.....Data Routines to extract Reference information
SelectLoadModifier
          pack      NMODFLD,NSELDESC
          rep       zfill,NMODFLD
          move      "D.Load2-NMODKEY",Location
          pack      KeyLocation,"Key: ",NMODFLD
          call      NMODKEY
          call      Trim using NMODDESC
          if ((NSELBASE = "BASE")|(NSELBASE = "SEC."))
                    pack      str25,dim9a,NMODDESC
          else
                    pack      str25,"+",dim9a,NMODDESC
          endif
.         pack      str25,dim9a,NMODDESC
          return

DataLoadRefAddressing
          pack      NREFFLD,"A",NADDNUM
          move      "D.Load1-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          if (NADDPRICE = C0)
                    clear     str9
          else
                    unpack    NADDPRICE,str5,str3
                    call      FormatNumeric using str5,str6
                    pack      str9,str6,str3
          endif
          pack      NMODFLD,NADDDESC
          rep       zfill,NMODFLD
          move      "D.Load2-NMODKEY",Location
          pack      KeyLocation,"Key: ",NMODFLD
          call      NMODKEY
          call      Trim using NMODDESC
          pack      str25,str9,NMODDESC
          return
DataLoadRefArrangement
          pack      NREFFLD,"R",NARRNUM
          move      "D.Load4-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
DataLoadRefSource
          pack      NREFFLD,"S",NSRCNUM
          move      "D.Load5-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          move      C0,N3
          move      NSRCPER,N3
          if (N3 > C0)
                    move      N3,NSRCPER
                    call      Trim using NSRCPER
                    pack      str4,NSRCPER,PRC
          else
                    clear     str4
          endif
          return
DataLoadRefSelection
          pack      taskname,NSLTVARS
          pack      str6,"LLL",NSLTNUM
          pack      NREFFLD,"L",NSLTNUM
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          if (NSLTPRICE = C0)
                    clear     str9
          else
                    unpack    NSLTPRICE,str5,str3
                    call      FormatNumeric using str5,str6
                    pack      str9,str6,str3
          endif
          pack      NMODFLD,NSLTDESC
          rep       zfill,NMODFLD
          move      "D.Load3-NMODKEY",Location
          pack      KeyLocation,"Key: ",NMODFLD
          call      NMODKEY
          call      Trim using NMODDESC
          pack      str25,str9,NMODDESC
          call      FormatNumeric using NSLTQTY,str11
          PACK      TASKNAME WITH NREFDESC,B8,STR25
          return

DataLoadRefNetName
          pack      taskname,NETNAME
          pack      NREFFLD,NETNAME
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
DataLoadRefSAMPLE
          pack      taskname,SAMPLE
          pack      NREFFLD,SAMPLE
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
DataLoadRefDelivery
          pack      taskname,DELCODE
          pack      NREFFLD,DELCODE
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
DataLoadRefClean
          pack      taskname,CLEANCDE
          pack      NREFFLD,CLEANCDE
          move      "D.Load2-NREFKEY",Location
          pack      KeyLocation,"Key: ",NREFFLD
          call      NREFKEY
          call      Trim using NREFDESC
          return
.================================================================
.;Patch1.1.2
OwnerInfo
.;
          clear     str35
          append    "c:\work\own" to str35
          append    lstnum to str35
          append    ".htm" to str35
          reset     str35
          rep       zfill in str35
          erase     str35
.Note we are using OwnHtmlFile here to create our Samples HTML File
          prepare   OwnHtmlFile,str35,exclusive
          Write     OwnHtmlFile,seq;*LL,"<HTML>"
          Write     OwnHtmlFile,seq;*LL,"<TITLE>","","</TITLE>"
          Write     OwnHtmlFile,seq;*LL,"<BODY>"
          Write     OwnHtmlFile,seq;*LL,"<P><PRE>"
.START PATCH 1.5 REPLACED LOGIC
.         bump      ownnum,2
.         move      ownnum to nownfld
          unpack    OWNNUM,str2,NOWNFLD
.END PATCH 1.5 REPLACED LOGIC
          call      nownkey
          If over
                    Write     OwnHtmlFile,seq;*LL,"Owner Info Not Available "
                    Write     OwnHtmlFile,seq;*LL,"</P></PRE>"
                    Write     OwnHtmlFile,seq;*LL,"</BODY>"
                    Write     OwnHtmlFile,seq;*LL,"</HTML>"
                    return
          endif
.;
          Write     OwnHtmlFile,seq;*LL,"Last Updated By: ",PASSWORD;
          MOVE NEWDATE TO N8
          IF (NEWDATE = "00000000" | NEWDATE = B8 | N8 = C0)
                    Write     OwnHtmlFile,seq;*LL,"Put Up Date: ","NOT AVAILABLE"
          ELSE
                    unpack    newdate,cc,yy,mm,dd
                    pack      str10,mm,"/",dd,"/",yy
                    Write     OwnHtmlFile,seq;*LL,"Put Up Date: ",str10
          ENDIF
          Write     OwnHtmlFile,seq;*LL,"List Owner #: ",OWNLON
          Write     OwnHtmlFile,seq;*LL,OWNLONM
.START PATCH 1.1.7 REPLACED LOGIC
.         call trim using COMMPER
.         Write     OwnHtmlFile,seq;*LL,COMMPER," % Commission"
          move      COMMPER,str6
          call      trim using str6
          Write     OwnHtmlFile,seq;*LL,str6," % Commission"
.END PATCH 1.1.7 REPLACED LOGIC
          Write     OwnHtmlFile,seq;*LL,OWNOCPY;
          Write     OwnHtmlFile,seq;*LL,OWNLOSA
          CALL      TRIM USING OWNLOCTY
          if (OWNLOCTY <> "")
                    Write     OwnHtmlFile,seq;*LL,OWNLOCTY,",",b1,OWNLOS,B1,OWNLOZC
          else
                    Write     OwnHtmlFile,seq;*LL,OWNLOS,B1,OWNLOZC
          endif
          if (ownTele <> "")
                    call      trim using owntele
                    unpack    OWNTELE,#str3,str3,str4
                    pack      str15,"(",#str3,")",b1,str3,DASH,str4
                    Write     OwnHtmlFile,seq;*LL,str15;
          endif
          call      trim using ownfax
          if (ownfax <> "")
                    unpack    OWNFAX,#str3,str3,str4
                    pack      str15,"(",#str3,")",b1,str3,DASH,str4
                    Write     OwnHtmlFile,seq;*LL,str15
          endif
.START PATCH 1.6 REPLACED LOGIC
.         clear     nfulfld
.         pack      NFULFLD,DATFUL
.         rep       zfill,NFULFLD
.         move      C1,NFULPATH
.         move      "D.Load-NFULKEY",Location
.         pack      KeyLocation,NFULFLD
.         call      NFULKEY
.         if over
.                   clear     NFULCOMP
.         endif
.         Write     OwnHtmlFile,seq;*LL,"CC TO: ",NFULCOMP
          call      TRIM using DATFUL
          if (DATFUL <> "")
                    pack      COMPFLD,DATFUL
                    rep       zfill,COMPFLD
                    move      C1,COMPPATH
                    move      "D.Load-COMPKEY",Location
                    pack      KeyLocation,COMPFLD
                    call      COMPKEY
                    if over
                              clear COMPCOMP
                    else
                              if (COMPSVBFLG <> "T")
                                        clear COMPCOMP
                              endif
                    endif
          else  // DATFUL = ""
                    clear COMPCOMP
          endif
          Write     OwnHtmlFile,seq;*LL,"CC TO: ",COMPCOMP
.END PATCH 1.6 REPLACED LOGIC
          Write     OwnHtmlFile,seq;*LL,"</P></PRE>"
          Write     OwnHtmlFile,seq;*LL,"</BODY>"
          Write     OwnHtmlFile,seq;*LL,"</HTML>"
          close     OwnHtmlFile
          RETURN

.;Patch1.1.2
...............................................................................................
.         INCLUDE   NORDIO.INC
.         INCLUDE   NDATIO.INC
.         INCLUDE   NADJIO.INC
.         INCLUDE   NINVIO.INC
.         INCLUDE   NJSTIO.INC
.         include   NUSEIO.INC
          include   nselio.inc
          include   nrefio.inc
          include   nmodio.inc
          include   naddio.inc
          include   narrio.inc
          include   ncatio.inc
          include   NSLTio.inc
          include   nsrcio.inc
          include   ntxtio.inc
          include   ndatio.inc
          include   nownio.inc
.START PATCH 1.6 REPLACED LOGIC
.         include   nfulio.inc
          include compio.inc
          include cntio.inc
.END PATCH 1.6 REPLACED LOGIC
.START PATCH 1.2.1 ADDED LOGIC
          include   ndatwio.inc
.END PATCH 1.2.1 ADDED LOGIC
.begin patch 1.94
          include   ntxt1io.inc
.end patch 1.94
          INCLUDE   COMLOGIC.INC