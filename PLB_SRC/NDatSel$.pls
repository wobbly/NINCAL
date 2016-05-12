Pc        Equ       0
          Include   Common.inc
          INclude   Cons.inc  
          Include   ndatdd.inc
          Include   Nowndd.inc
          Include   NTXTDD.inc
          Include   NADDDD.inc
          include   NArrdd.inc                .arrangement codes  mmm anything to do here??? I think Defunct DH
          Include   NSrcDD.inc                 
          Include   NModDD.inc                 .not touched yet
          INclude   Xls.inc
                    include         NSELdd.inc
                    Include   NSltdd.inc
                    include   Nrefdd.inc
                    include   NDatudd.inc
          Include   NDatCntDD.inc
          Include   NCatdd.inc
. backup files
          Include   ndatBdd.inc
          Include   nTxtBdd.inc
          Include   nAddBdd.inc
          include   NArrBdd.inc
          include   NSltBdd.inc
          include   NSrcBdd.inc
          Include   NModBDD.inc                 .not touched yet
.......................................................
.
Release   Init      "1.4"     DLH  Zip tape #105
Reldate   Init      "2015 November 18"
.Release   Init      "1.3"     DLH  NOn-Recip fee
.Reldate   Init      "2014 July 8"
.Release   Init      "1.1"     DLH  increase email/ftp fee to 60.00
.Reldate   Init      "2013 December 3"
.Release  INit      "1.0"     DLH         
.REldate  Init      "15 February 2008"


RecipFlag  dim        1
ZipFlag  dim        1
hold2         dim             7500           .length of largest possible text record
DimPtr              Dim       ^
DimPtr1             Dim       ^
STR46     Dim       46
OverFLow  Dim       1
B50       Init      "                                              "
B45       Init      "                                         "
BaseFlag  Dim       1
SlctFlag  Dim       1
crnl       init      0177           .causes line feed cr in edittext
Carr      init      0x7f                .edittext carraige return line feed
SelBlankCnt         Form      2         .max number of blank records before we bale cleanup of select records
Form52    form      5.2
TestDate  Form      5
Mode      Dim       1                   ...Default is "B" batch mode. can be called from M2N0003 then in "M" manual mode
HoldOwn   Dim       6                   .Hold the LO number as it may have changed since we updated.
NewFlag   Dim       1                   Holds "Y" if new list to be created
.controlled by common varaible "COMMENT" if empty - Batch
Match     File      .Holds output on matched lists
NoMatch   File      .Holds output on Unmatched lists
Exclusive File      .Holds Output on Exclusive lists
Nocnt     form      5
MCnt      form      5
ECnt      form      5
holdlist  dim       6
...................................................................................
CaseChange external "SPELLCHECK;CaseChange"
...................................................................................

.THis program used to update select prices or addressing

ExRow     Dim       6
EXRow1    Dim       6
HMList    Dim       6         Hold Min List #
CountIn   Form      6
HoldBase  Dim       4
.


.begin patch 1.3 selects not addressing
.begin patch 1.1
.selects   Dim       3(11):
.slt1                ("024"):            Modem/Ftp
.slt2                ("025"):            Email
.slt3                ("027"):            FTP
.slt4                ("036"):            Electronic
.slt5                ("028"):            CDROM
.slt6                ("039"):            Cheshire
.slt7                ("006"):            PS Labels
.slt8                ("021"):            3480 Cartridge
.slt9                ("029"):            9 track
.slt10                ("004"):            4-across
.slt11                ("002")            mag tape
selects  Dim       3(12):
slt1               ("105"):             Zip Setup
slt2               ("002"):            gender
slt3               ("004"):            SCF
slt4               ("005"):            Zip
slt5               ("008"):            Keycode
slt6               ("058"):            zip+4
slt7               ("025"):            Email
slt8               ("027"):            FTP
slt9               ("028"):            CD-rom
slt10              ("063"):             running Charges
slt11              ("050"):             Non Recip
slt12               ("003")            State
.end patch 1.1
.end patch 1.3

ListEdit  EditTExt

Start     Create    ListEdit=3:4:2:77,BORDER:
                    STYLE=3DON
          Activate  LIstEdit            


          Erase     "c:\work\Updated.csv"
          
          PRepare   Match,"c:\work\Updated.csv",exclusive



Loop1
          loop
          Call      NdatKS
          until     over
          add       c1,CountIn
          Display   *p15:11,CountIn,b1,Lstnum
          if        ((ELSTCDE = "C" or ELSTCDE = "P") & status <> "W")               .Managed - check selects
          MOve      Mlstname,Hold2
          Setitem   Listedit,0,Hold2

.          call      PRocess
           if         (status = "T")
                                             packkey   nsltfld from lstnum,"105"
                                             rep       zfill,nsltfld
                                             call      nsltkey
                                                       if        not over
                                                       call        Nsltdel
                                                       endif         
           endif

          endif
          repeat
          goto      EOj
Process
.Get Select info
.         should  be able to build key using list number and just the selects we want.
.begin patch 1.1
.          for       n2 from c1 to c10
.begin xxx
          clear     NselExc             .clear var
          if (NDATCONV = "1")
                              
                              pack      NSELFLD1,"01X",LSTNUM
                              pack      NSELFLD2,"02XBASE"
                              move      "NSELAIM",Location
                              pack      KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD2
                              call      NSELAIM
                              if        over
.cleanup
                                  packkey   nsltfld from lstnum,"105"
                                  rep       zfill,nsltfld
                                  call      nsltkey
                                            if        not over
                                            call        Nsltdel
                                            endif         
.end cleanup
                                 return
                              else   
                                        if (NSELEXC = "2")            .exchange only
                                 return
                                 endif
                      endif      
          endif               
           if         (lstnum = "075184")
           call       debug
           endif
          for       n2 from c1 to C1
.end patch 1.1
          

          move      selects(n2),str3
.begin patch 1.3
.          packkey   naddfld from lstnum,str3
.          rep       zfill,naddfld
.          call      naddkey
          packkey   nsltfld from lstnum,str3
          rep       zfill,nsltfld
          call      nsltkey
          if        not over
.begin patch 1.1
.begin patch 1.3   for non recip
.                    if     ((str3 = "024" or str3 = "025" or str3 = "027" or str3 = "036") & "60.0" > NADDPRICE)

.                    if     (str3 = "050")
.                      move       Yes,RecipFlag
.                      endif
                    if     (str3 = "105")
                      move       Yes,ZipFlag
                    else         
                    move       No,zipflag
                      endif

                    if     (str3 = "105" & Status = "T") 
                    call         Nsltdel
                    return
                    endif

                     return
                     
                    if     ((str3 = "105") & "25.0" <> NsltPRICE)
                    move      "25.0",NsltPRICE
.                    MOVe      "002",NsltDESC           .flat
                    MOVe      "003",NsltDESC           .Each
                   call      Nsltupd
.                    if     ((str3 = "050") & "25.0" <> NsltPRICE)
.                    move      "25.0",NsltPRICE
.                    MOVe      "001",NsltDESC           ./m
.                   call      Nsltupd
.                              if        (nselexc <> "2")   .exchange
.                              call      Naddupd
.                              else
.                              call      debug
.                              call      NaddDel           .exclusive only should not have these fees
.                              endif

.                    Elseif     (str3 = "028" or str3 = "039" or str3 = "006" or str3 = "021" or str3 = "029" or str3 = "004" or str3 = "002")
.                    call      NaddDel
.                    Elseif     ((str3 = "024" or str3 = "025" or str3 = "027" or str3 = "036") & Nselexc = "2")
.                    call      debug
.                    call      NaddDel           .exclusive only should not have these fees
                    if        (lstnum <> Holdlist)
                    Write     Match,seq;*cdfon,Lstnum,Mlstname
                    add       c1,NoCnt
                    move      lstnum,holdlist
                    endif

                    endif

.                   if     (n2 >= c1 & n2 <= 4 & "8.0" > NSLTPRICE)
.                   move      "8.0",NSLTPRICE
.                   call      Nsltupd
.                   Elseif    ((n2 = "025" or n2 = "027" or n2 = "028") & "50.0" > NSLTPRICE)
.                   move      "50.0",NSLTPRICE
.                   call      Nsltupd
.                   Elseif    (n2 = "008" & "5.0" > NSLTPRICE)               .keycode
.                   move      "5.0",NSLTPRICE
.                   call      Nsltupd
.                   Elseif    (n2 = "058" & "8.0" > NSLTPRICE)              .zip+4
.                   move      "5.0",NSLTPRICE
.                   call      Nsltupd
.                   Elseif    (n2 = "063")                      .running charges
.                   call      NsltDel
.                   endif
.end patch 1.1

           else
.if there was not a recip fee add it
                      if         (Zipflag <> Yes)
                      move       "105",nsltnum
.                     if         (recipflag <> Yes)
.                     move       "050",nsltnum
                      move       lstnum,NSLTLIST
                      pack    NSLTFLD,NSLTLIST,NSLTNUM
                      move    "RefSave-NSLTWRT",Location
                      pack    KeyLocation,"Key: ",NSLTFLD
                      move      "25.0",NsltPRICE
.                    MOVe      "002",NsltDESC           .flat
                    MOVe      "003",NsltDESC           .Each
.                     MOVe      "001",NsltDESC         ./m 
                      call      Nsltwrt

                    if        (lstnum <> Holdlist)
                    Write     Match,seq;*cdfon,Lstnum,Mlstname
                    add       c1,NoCnt
                    move      lstnum,holdlist
                    endif

                      return
                      endif
           
          endif
XXXX
          repeat

          move        No,RecipFlag
          move        no,Zipflag
          return


....      
EOJ       
.Create spreadsheet
.Open Excel application
          create  ex
.Reset Default of Worksheets found in a Workbook
          setprop ex,*SheetsInNewWorkbook=C3
.Create Workbooks collection
          getprop ex,*Workbooks=books
.Create/Add a single Workbook
          books.add
          books.item giving book using 1
.Create Worksheets collection
          getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
          sheets.item giving sheet using 1
          create    xlShifttoLeft,VarType=VT_R8,VarValue="-4159"
          create    xlShiftUp,VarType=VT_R8,VarValue="-4162"
          create    xlRowHeight,VarType=VT_R8,VarValue="2.75"
          create    xlColumnWidth,VarType=VT_R8a,VarValue="0.46"
          create          OTRUE,VarType=VT_BOOL,VarValue=1
          create          OFALSE,VarType=VT_BOOL,VarValue=0
          create          TopMargin,VarType=VT_R8,VarValue="18"                       Roughly equals .25 inches:  18 * 1.388 = 25
          create          BottomMargin,VarType=VT_R8,VarValue="18"
          create          LeftMargin,VarType=VT_R8,VarValue="5"
          create          RightMargin,VarType=VT_R8,VarValue="5"                Roughly equals .0694 inches:  5 * 1.388 = 6.94
            MOve    c7,CellRowCnt
            MOve    c7,CellRowCnt1
            MOve    c7,CellRowCnt2

.        setprop ex,*Visible="True"

.Sheet 1 we have list # xref          
            sheets.item giving sheet using 1
            setprop sheet.range("a1:z250").Font,*Name="Times New Roman", *Size=11
            setprop sheet.PageSetup,*Orientation=xlLandscape
            setprop sheet.PageSetup,*TopMargin=TopMargin
            setprop sheet.PageSetup,*BottomMargin=BottomMargin
            setprop sheet.PageSetup,*FooterMargin=TopMargin
            setprop sheet.PageSetup,*LeftMargin=LeftMargin
            setprop sheet.PageSetup,*RightMargin=RightMargin
            setprop sheet.range("a1","a1"),*Value="List XREf Found",*HorizontalAlignment=AlignLeft
            setprop sheet.range("a1:a1").Font,*Name="Times New Roman", *Size=14
            setprop sheet.range("a1:a1").Font,*Bold="True"
            setprop sheet.range("b3","B3"),*Value="Min",*HorizontalAlignment=aligncenter
            setprop sheet.range("b3:b3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("b3:b3").Font,*Bold="True"
            setprop sheet.range("b4","B4"),*Value="List",*HorizontalAlignment=aligncenter
            setprop sheet.range("b4:b4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("b4:b4").Font,*Bold="True"
            setprop sheet.range("b5","B5"),*Value="Code",*HorizontalAlignment=aligncenter
            setprop sheet.range("b5:b5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("b5:b5").Font,*Bold="True"
            setprop sheet.range("C3","C3"),*Value="NIN",*HorizontalAlignment=aligncenter
            setprop sheet.range("C3:C3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("C3:C3").Font,*Bold="True"
            setprop sheet.range("C4","C4"),*Value="List",*HorizontalAlignment=aligncenter
            setprop sheet.range("C4:C4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("C4:C4").Font,*Bold="True"
            setprop sheet.range("C5","C5"),*Value="Code",*HorizontalAlignment=aligncenter
            setprop sheet.range("C5:C5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("C5:C5").Font,*Bold="True"
            setprop sheet.range("D3","D3"),*Value="List Name",*HorizontalAlignment=aligncenter
            setprop sheet.range("D3:D3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("D3:D3").Font,*Bold="True"
            setprop sheet.range("E3","E3"),*Value="NIN List Name",*HorizontalAlignment=aligncenter
            setprop sheet.range("E3:E3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("E3:E3").Font,*Bold="True"
            setprop sheet.range("f3","f3"),*Value="NIN",*HorizontalAlignment=aligncenter
            setprop sheet.range("f3:f3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("f3:f3").Font,*Bold="True"
            setprop sheet.range("f4","f4"),*Value="Exclusive",*HorizontalAlignment=aligncenter
            setprop sheet.range("f4:f4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("f4:f4").Font,*Bold="True"
            setprop sheet.range("g3","g3"),*Value="Min",*HorizontalAlignment=aligncenter
            setprop sheet.range("g3:g3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("g3:g3").Font,*Bold="True"
            setprop sheet.range("g4","g4"),*Value="Owner",*HorizontalAlignment=aligncenter
            setprop sheet.range("g4:g4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("g4:g4").Font,*Bold="True"
            setprop sheet.range("g5","g5"),*Value="Code",*HorizontalAlignment=aligncenter
            setprop sheet.range("g5:g5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("g5:g5").Font,*Bold="True"
            setprop sheet.range("H3","H3"),*Value="Min",*HorizontalAlignment=aligncenter
            setprop sheet.range("H3:H3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("H3:H3").Font,*Bold="True"
            setprop sheet.range("H4","H4"),*Value="Owner",*HorizontalAlignment=aligncenter
            setprop sheet.range("H4:H4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("H4:H4").Font,*Bold="True"
            setprop sheet.range("H5","H5"),*Value="Name",*HorizontalAlignment=aligncenter
            setprop sheet.range("H5:H5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("H5:H5").Font,*Bold="True"
            setprop sheet.range("I3","I3"),*Value="NIN (x)",*HorizontalAlignment=aligncenter
            setprop sheet.range("I3:I3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("I3:I3").Font,*Bold="True"
            setprop sheet.range("I4","I4"),*Value="Owner",*HorizontalAlignment=aligncenter
            setprop sheet.range("I4:I4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("I4:I4").Font,*Bold="True"
            setprop sheet.range("I5","I5"),*Value="Code",*HorizontalAlignment=aligncenter
            setprop sheet.range("I5:I5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("I5:I5").Font,*Bold="True"
            setprop sheet.range("J3","J3"),*Value="NIN (x)",*HorizontalAlignment=aligncenter
            setprop sheet.range("J3:J3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("J3:J3").Font,*Bold="True"
            setprop sheet.range("J4","J4"),*Value="Owner",*HorizontalAlignment=aligncenter
            setprop sheet.range("J4:J4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("J4:J4").Font,*Bold="True"
            setprop sheet.range("J5","J5"),*Value="Name",*HorizontalAlignment=aligncenter
            setprop sheet.range("J5:J5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("J5:J5").Font,*Bold="True"
            setprop sheet.range("K3","K3"),*Value="NIN",*HorizontalAlignment=aligncenter
            setprop sheet.range("K3:K3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("K3:K3").Font,*Bold="True"
            setprop sheet.range("K4","K4"),*Value="Owner",*HorizontalAlignment=aligncenter
            setprop sheet.range("K4:K4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("K4:K4").Font,*Bold="True"
            setprop sheet.range("K5","K5"),*Value="Code",*HorizontalAlignment=aligncenter
            setprop sheet.range("K5:K5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("K5:K5").Font,*Bold="True"
            setprop sheet.range("L3","L3"),*Value="NIN",*HorizontalAlignment=aligncenter
            setprop sheet.range("L3:L3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("L3:L3").Font,*Bold="True"
            setprop sheet.range("L4","L4"),*Value="Owner",*HorizontalAlignment=aligncenter
            setprop sheet.range("L4:L4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("L4:L4").Font,*Bold="True"
            setprop sheet.range("L5","L5"),*Value="Name",*HorizontalAlignment=aligncenter
            setprop sheet.range("L5:L5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("L5:L5").Font,*Bold="True"


.Load it up
          write     Match,Seq;*cdfon,Mcnt
          weof      match,seq
          Close     Match

          OPen      Match,"c:\work\updated.csv",read

          Loop                

          Read      Match,seq;*cdfon,Lstnum,Mlstname

          Until     over                          

                    sheets.item giving sheet using 1
                    Move      CellRowCnt,ExRow
                    call      Trim using ExRow


          Pack      str15,"B",ExRow
          setprop sheet.range(str15),*Value=lstnum
          Pack      str15,"C",ExRow
          setprop sheet.range(str15),*Value=MLstName
          
          
          Add       c1 to CellRowCnt
          
          Repeat

......    .
.save it
            sheets.item giving sheet using 3
            If      (CellRowCnt2 < 3)
            move    c3 to CellRowCnt2
            endif

            Move    CellRowCnt2,ExRow1
            call    Trim using ExRow1
          pack    str4,"A3"
          pack    str11,"L",Exrow1
          sheet.range(str4,str11).Columns.Autofit

            sheets.item giving sheet using 2
            Move    CellRowCnt1,ExRow1
            call    Trim using ExRow1
          pack    str4,"A3"
          pack    str11,"L",Exrow1
          sheet.range(str4,str11).Columns.Autofit

            sheets.item giving sheet using 1
            Move    CellRowCnt1,ExRow
            call    Trim using ExRow
          pack    str4,"A3"
          pack    str11,"L",Exrow
          sheet.range(str4,str11).Columns.Autofit


          Clock     Timestamp,Timestamp
              clear   taskname
              setprop ex,*DisplayAlerts=OFalse


              setprop ex,*DefaultFilePath=taskname
              bump            timestamp,8
              Clear           Taskname
              pack            Taskname,"c:\work\Ndattest_",Timestamp
./////
.Trap in case a workbook with the same name is already open.  In such a case, the saveas will
.not occur
.                trap    TrapCampaignObject if Object
                book.saveas giving N9 using *Filename=taskname
                trapclr Object
..............................................................................................................
.CleanUp
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.do some housekeeping first


        destroy sheet
        destroy sheets
        destroy book
        destroy books
          Destroy   XlShiftToLeft
          Destroy   XlShiftUp
          Destroy xlRowHeight
          Destroy xlColumnWidth
          Destroy OTRUE
          Destroy OFALSE
          Destroy TopMargin
          Destroy BottomMargin
          Destroy LeftMargin
          Destroy RightMargin
            setprop ex,*DisplayAlerts=OFALSE
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
        ex.quit
        destroy ex
EojError
          IF        (Mode <> "M")        
          Winshow
          shutdown
          stop
          Else
          Destroy   ListEdit
          Return                             .return from External call
          endif


          Include   Ndatio.inc
          Include   Nownio.inc
          Include   NTXTIO.inc
          Include   NADDIO.inc
          include   NArrIO.inc 
          include   NsrcIO.inc
          Include   NModio.inc                 .not touched yet
          include         NSELIO.inc
               Include        NSltio.inc
               include        Nrefio.inc
          include   NdatUio.inc
          Include   NDatCntio.inc
          Include   NCatio.inc

. backup files
          Include   ndatBIO.inc
          Include   nTxtBIO.inc
          include   NAddBIo.inc
          include   NArrBIO.inc
          include   NSltBIO.inc
          include   NSrcBIO.inc
          Include   NModBIO.inc                 .not touched yet
.......................................................

          INclude   Comlogic.inc