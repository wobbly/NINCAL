Pc        Equ       0
          Include   Common.inc
          INclude   Cons.inc  
          include   MDCmaindd.inc
          include   MDcSegdd.inc
          Include   MDCMSCDD.inc        
          include   mdc035dd.inc
          include   mdc090dd.inc
          include   mdctxtdd.inc
          include   mdc060dd.inc
          Include   M2Ndd.inc
          Include   M2NLodd.inc
          Include   ndatdd.inc
          Include   Nowndd.inc
          Include   NTXTDD.inc
          Include   NADDDD.inc
          include   NArrdd.inc                .arrangement codes  mmm anything to do here??? I think Defunct DH
          Include   NSrcDD.inc                 
          Include   NModDD.inc                 .not touched yet
          INclude   Xls.inc
          INclude   NMDCMscDD.inc - additional info (currently from MIN) need to incorporate
          INClude   NMDCCATDD.inc - Min Category
                    include         NSELdd.inc
                    Include   NSltdd.inc
                    include   Nrefdd.inc
                    include   NDatudd.inc
            include         gnxtdd.inc
          Include   NDatCntDD.inc
          Include   NCatdd.inc
.Begin Typist
          include   Nusedd.inc
          Include   Ntypdd.inc
.end Typist
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
Release   INit      "1.34"     AH           .Adjusted TYPOUT logic
REldate   Init      "13 November 2013"
.Release   INit      "1.33"     DLH           .Test Creating "NEW" nin lists for all active Min lists with LO, etc
.REldate   Init      "26 October 2010"
.Release   INit      "1.32"     DLH           .Change to Ntypdd
.REldate   Init      "22 March 2010"
.Release   INit      "1.31"     DLH           .WRite (I)nactive to No match sheet.
.REldate   Init      "09 December 2009"
.Release   INit      "1.3"     DLH           .Rifle thru segments and try to correct universe
.REldate   Init      "11 June 2009"
.Release   INit      "1.2"     DLH           .xls.inc did not allow for more than 9,999 rows - fixed addes some displays
.REldate   Init      "16 June 2008"
.Release  INit      "1.1"     DLH           PLI
.REldate  Init      "13 August 2007"
.Release  INit      "PRE"     DLH
.REldate  Init      "08 September 2006"
.hold2         dim             4500           .length of largest possible text record
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

...................................................................................
CaseChange external "SPELLCHECK;CaseChange"
...................................................................................

.THis program is the "2nd" run to auto update list information from Converted MIN List system CSV format live files
.First release kicks out "printed" reports only no update.
.
.open and read mdc_main.dat file read Min2NIN xref and 
.supplemental files so they can be cross referenced and applied.
ExRow     Dim       7
EXRow1    Dim       7
.ExRow    Dim       5
.EXRow1   Dim       5
HMList    Dim       6         Hold Min List #
CountIn   Form      6
HoldBase  Dim       4
.
ListEdit  EditTExt

Start     Create    ListEdit=3:4:2:77,BORDER:
                    STYLE=3DON
          Activate  LIstEdit
Counter   Form      6                   
DimCounter          Dim       6
CSVStatus StatText  (3)
          Create    CSVStatus(1)=8:9:2:40,"  ","TIMES(14)",Border
          Create    CSVStatus(2)=13:14:2:40,"  ","TIMES(14)",Border
          Create    CSVStatus(3)=13:14:45:77,"  ","TIMES(14)",Border
                    
.may decide to skip this in Manual Mode
.***********************      
.         Goto      EOJ
.***********************      
          call      Trim Using COmment
          call      debug
          if        (comment = "")
          move      "B",Mode
          Erase     "c:\work\Match.csv"
          Erase     "c:\work\NoMatch.csv"
          Erase     "c:\work\Exclusive.csv"
          
          PRepare   Match,"c:\work\Match.csv",exclusive
          PRepare   NoMatch,"c:\work\NoMatch.csv",exclusive
          PRepare   Exclusive,"c:\work\Exclusive.csv",exclusive
          Else      
          MOVE      "M",mode
          endif   





.         Move      c0 to N5
.         Display   *p10:11,*el,"Processing MDC_Main "
.         Trap      NoLoop1 Giving Error If IO
.         Open      MinSfile,"c:\work\min\MDC_main.csv",exclusive
.         TrapClr   IO
.....>>>>>>>>>>>>>>>>>>>>>>>TEMP
.         Move      "059855",MinFLd
.         call      MinKey
.....>>>>>>>>>>>>>>>>>>>>>>>TEMP
          
.         

.loop1 process Min_Main
Loop1
          IF        (Mode = "B")                  .Batch/Auto mode
          loop
          Call      MinKS
          until     over
          add       c1,CountIn
          Display   *p15:11,CountIn,b1,dttl
          call      Trim Using DTTL
          Clear     Ndatfld
          Clear     Nownfld
          Clear     OWNLON
          Clear     OWNOCPY
          call      PRocess
          repeat
          goto      EOj
          Else
.         Move      COmment,DTTL
          packkey   MinFLd,COmment
          Call      MinKEy
          Display   *p15:11,"Manual Mode ",dttl
          Clear     Ndatfld
          Clear     Nownfld
          Clear     OWNLON
          Clear     OWNOCPY
          call      PRocess
          goto      EOj
          endif     
Process
.Get Min LO info
          call      Trim using Dlow
          Count     n2,Dlow
          if        (n2 = 1)
          pack      str5,"0000",dlow
          Elseif    (n2 = 2)
          pack      str5,"000",dlow
          Elseif    (n2 = 3)
          pack      str5,"00",dlow
          Elseif    (n2 = 4)
          pack      str5,"0",dlow
          Else
          pack      str5,Dlow
          endif
          Move      Str5,Dlow
          PackKey   M090Fld,Dlow
          call      M090Key
          Pack      M2NLoFld,"0",Dlow
          call      M2nLoKey
          if        Not over
          UNpack    M2NLoNIN,str2,str4
          packkey   Nownfld,str4
          Move      M2NLoNIN,HoldOwn
          call      Nownkey
          else                            .its over
                    if        (NewFlag = Yes)   
                              IF        (Mode = "M")
                              alert     note,"Cannot Create a Datacard Without LO info!!!",result
                              goto      EojError                   .Cannot Create New card without LO
                              else
                              call      NoXref
                              Return
.                             goto      Eoj          .need a better way
                              endif
                    Else      .update mode
                              IF        (Mode = "M")
                              alert     note,"Cannot Update a Datacard Without matched LO info!!!",result
                              goto      EojError                   
                              else
                              call      NoXref
                              Return
                              endif
                    
                    endif
          endif
.test
          If        (Mode = "B")
          PACKKey   MMscFld,ddcnoa
          Clear     DDLT
          call      MMSCKey

                    if        (DDLT <> "I")
                    move      Yes,NewFlag
                    else
                    move      No,NewFlag
                    endif
          endif          
.test.         
          packkey   M2NFld,DDCNOa
          call      M2nKey
          if        over
                    IF        (Newflag <> Yes)
                    call      NOxref
                    Return
                    Elseif    (NewFlag = Yes)         .Manual Mode and want to create new Card

          call      Debug
DaveTST
                    move    "NLSTNXT",GNXTFLD
                              move    "Save-GNXTKEY",Location
                              pack    KeyLocation,"Key: ",GNXTFLD
                              call    GNXTKEY
                                        if over
                              append  "GNXT.DAT was not properly accessed.",taskname
                              append  newline,taskname
                              append  "Save will not execute!!",taskname
                              reset   taskname
                              alert   caution,taskname,result
                              return 
                                        else 
                              move    GNXTNUM,N6

                              loop 
                                        add     C1,N6
                                        move    N6,GNXTNUM
                                        rep     zfill,GNXTNUM
                                        move    "Save-GNXTUPD",Location
                                        pack    KeyLocation,"Key: ",GNXTFLD
                                        call    GNXTUPD
                                        move    N6,LSTNUM
                                        rep     zfill,LSTNUM
                                        move    LSTNUM,NDATFLD
                                        move    C1,NDATPATH
                                        move    "Save-NDATTST",Location
                                        pack    KeyLocation,"Key: ",NDATFLD
                                        call    NDATTST
                                        until over
                              repeat 
                              
.Load other variables set exclusively during record creation
                              unpack  timestamp,NEWDATE
                              Move      LstNum,HMList
                              MOVe      LstNum,M2nNIN
                              Move      DDCNOa,M2nMIN
                              call      M2NWRt
                              goto      NewCard
                              Endif
                    endif
          Endif
.New 08Feb 2005
          if        (M2nStatus = "*")            .special pricing do not touch the card
                    IF        (Mode = "M")
                    alert     note,"Datacard marked as special pricing - not updating!!!",result
                    endif
          return
          endif
.                                       .We think we are golden
          packkey   NDatfld,M2NNIN
          Move      M2nNIN,HMList       .save list number
          move      c1 to ndatpath
          clear     RevDate
.         call      debug
          clear     Elstcde
          call      Ndatkey
          If        Over
          goto      NoXref
          endif
........
          IF        ("C" = Elstcde or Ownnum = "000033" or Elstcde = "P" or Ownnum = "001490")
          call      Exclusive
          return
          endif
          
          if        (holdown <> ownnum)           ....Manager/Owner changed save the info
          move      ownnum,NDatoldOwn
          endif
.move current LO to card
          clear     Ownnum
          pack      OwnNum,"00",OWNLON
          rep       zfill,ownnum
          
........
.check date don't update if we are newer
.skipping right now
          Clear     yy
          Clear     mm
          Clear     dd
          unpack    DDTR8 into str2,yy,mm,dd
          call      cvtJul
          move      JUldays,TestDate
          Clear     yy
          Clear     mm
          Clear     dd
          unpack    Revdate into  str2,yy,mm,dd                       .REVISION DATE CCYYMMDD FORMAT
          call      cvtJul
......................................................................................
.skipping right now
.          call      Debug
......................................................................................

.if previously updated by MIn & MIN data is newer Update
          if        (Password = "MIN       " & TestDate > Juldays)        
          goto      Newcard
          Else
.         call      cvtJul
.IF Batch mode we have a match and NIN card is + 6 mos old Update  --- testing
.                   if        (Mode = "B" & TestDate-JUldays > 180)
                    if        (Mode = "B" & TestDate > JUldays)
                    goto      Newcard
                    Elseif    (Mode = "M" & TestDate < Juldays)
                    alert     note,"Min Card Older than NIN Datacard did not process!!!",result
                    return
                    Elseif    (Mode = "M" & TestDate >= Juldays)
                    goto      Newcard
                    else
                    return
                    endif
.         if        (TestDate <= Juldays)
..        
.                             IF        (Mode = "M")        
.                             alert     note,"Min Card Older than NIN Datacard did not process!!!",result
.                             endif
.         return
.                   endif
          endif
NewCard
          move      B1,Elstcde                          .make sure
          call      Trim using Mlstname
.
          Move      DDTC8,NDatVerf                     .verified date 
          Move      DDTR8,REVDATE                     .Revised date   
          MOve      "MIN",Password
          clock     Timestamp,timestamp 
.         call      debug
          unpack    timestamp,NdatupdDate              .todays date 
          MOve      "MIN",NDatUpdINit
          
          Move      COMMMIn,CommPer
          Move      DDNU8,NDATNUPD                     .next update date
.lets do the body/text next
          Clear     Hold2
          packkey   MtxtFld2,DDCNOa,"   1"
          move      c2,Mtxtpath
          call      MTxtKey
          If        over
.error
          endif
.cleanup old Text records & make copies
                    pack      NTXTFLD1,"01X",NDATFLD
                    clear     NTXTFLD2
                    move      "Save-NTXTAIM",Location
                    pack      KeyLocation,"Key: ",NTXTFLD1
                    call      NTXTAIM
                    loop
                              until over

                              move      "Backup-NTXTBWrt",Location
                              move      NTxtBLIST,NTxtBLIST
                              Move      NTxtNUM,NTxtBNUM
                              Move      NTxtText,NTxtBTEXT
                              Move      NDatBStamp,NTxtBStamp         
                              call      NTXTBWrt

                              move      "Save-NTXTDEL",Location
                              call      NTXTDEL

                              move      "Save-NTXTKG",Location
                              call      NTXTKG
                    repeat
          MOve      LStnum,NTXTLIST

          Match     TxtLin,b50                                  .Blank Line
                    IF        Equal
                    Clear     str46
                    append    Carr,Hold2
                    goto      Endparse
                    endif

          call      Trim Using TXTLIN
                    count     n2,TXTLIN
                    if        (n2 >= "46")
                    reset     Txtlin
                    Append    txtlin,Hold2
                    Bump      Hold2,c1
                    goto      endparse
                    endif

          endset    TxtLin
          Cmatch    ".",TxtLin
                    IF        equal
                    reset     TxtLin
                    Clear     Str46
                    append    txtlin,str46
                    append    Carr,str46
                    Reset     str46
                    append    str46,hold2
                    goto      endparse
                    endif

          Reset     TxtLin
          endset    TxtLin
          Cmatch    ":",TxtLin
                    IF        equal
                    reset     TxtLin
                    append    txtlin,str46
                    append    Carr,str46
                    Reset     str46
                    append    str46,hold2
                    goto      endparse
                    endif
                    
          reset     Txtlin
          Append    txtlin,Hold2
          append    b1,hold2
          goto      Endparse

          
          Loop
          call      MTxtKS
          Until     over
          IF        (DDCNoa = TXDCNO)    .same list?????

                    IF        (TxtTyp = "D")

          
                    Match     TxtLin,b50                                  .Blank Line
                              IF        Equal
                              Clear     str46
                              append    Carr,Hold2
                              goto      Endparse
                              endif


                    call      Trim Using TXTLIN
                              count     n2,TXTLIN
                              if        (n2 >= "46")
                              reset     Txtlin
                              Append    txtlin,Hold2
                              append    b1,Hold2
                              goto      endparse
                              endif

                    endset    TxtLin
                    Cmatch    ".",TxtLin
                              IF        equal
                              reset     TxtLin
                              Clear     Str46
                              append    txtlin,str46
                              append    Carr,str46
                              Reset     str46
                              append    str46,hold2
                              goto      endparse
                              endif

                    Reset     TxtLin
                    endset    TxtLin
                    Cmatch    ":",TxtLin
                              IF        equal
                              reset     TxtLin
                              append    txtlin,str46
                              append    Carr,str46
                              Reset     str46
                              append    str46,hold2
                              goto      endparse
                              endif
                    
                    reset     Txtlin
                    Append    txtlin,Hold2
                    append    b1,hold2
                    goto      Endparse
          ElseIF    (TxtTyp = "O")
                    Match     TxtLin,b50                                  .Blank Line
                              IF        Equal
                              append    CARR,hold2
                              goto      Endparse
                              endif
                    call      Trim using TxtLin
                    Endset    TxtLin
                    cmatch    ":",TxtLin
                              if        equal
                              reset     Txtlin
                              Append    txtlin,Hold2
                              append    CARR,Hold2
                              goto      endparse
                              endif
                    Reset     TxtLin
                    Endset    TxtLin
                    cmatch    ".",TxtLin
                              if        equal
                              reset     Txtlin
                              Append    txtlin,Hold2
                              append    CARR,HOld2
                              goto      endparse
                              endif
                    Reset     TxtLin
                    Count     n2,TxtLin
                    if        (n2 <= "45")
                              append    TxtLin,Hold2
                              append    Carr,Hold2
                              goto      endparse
.                   Elseif    (N2 = "46")              .last change
.                             append    TxtLin,Hold2
.                             goto      endparse
                    else
                              append    txtlin,hold2
                              Append    b1,hold2
                              goto      endparse
                    endif                         
                    endif
          
                    
endparse            
                    Clear     Str46
                    Clear     Str50

          Else
          Break
          endif
          repeat

          
.end of text        
.

.
          Packkey   NMDCCFld,Ndatfld
          Loop                                    .clean up category file
          call      NMDCCKEY
          until     over
          call      NMDCCDel
          Repeat
.
.Need to add lots of checking here  selects addressing etc is buried in here
.lets start saving
          clock     TimeStamp,NDatBStamp
          call      NDatBWrt                     .write to backup
.
          Call      Del2ndary                .cleanup slt & add files
.         
          packkey   M035Fld,ddcnoa
          call      M035Key
          Call      CheckSlct
          move      Ndatfld,NMDCCNum
          Move      CASQNO,NMDCCSQN
          Move      CATC,NMDCCCATC
          Move      CAT$,NMDCCCAT
          MOVe      CATCHR,NMDCCCATCHR  
          MOve      CATR,NMDCCCATR
          MOVe      CATDSC,NMDCCCATDSC
          if        (SlctFlag = No)                 .was not a select addressing etc so write it
          call      NMDCCWrt
          endif
          Loop                                             .WRite out the new data
          call      M035Ks
          until     over
          if        (CADCNO = DDCNOa)
          Call      CheckSlct
          move      Ndatfld,NMDCCNum
          Move      CASQNO,NMDCCSQN
          Move      CATC,NMDCCCATC
          Move      CAT$,NMDCCCAT
          MOVe      CATCHR,NMDCCCATCHR  
          MOve      CATR,NMDCCCATR
          MOVe      CATDSC,NMDCCCATDSC
          if        (SlctFlag = No)                 .was not a select addressing etc so write it
          call      NMDCCWrt
          endif
          else
          Break
          Endif
          REpeat
.misc file
          PackKey     NMSCfld,ndatfld
          PACKKey   MMscFld,ddcnoa
.MSC - data addressing, Min, etc

          call      MMSCKey
          If        Not Over

.Minimums
          Call      Trim using DMO1
          Call      Trim using DMO2
          clear     str46
          count     n2,DMO1
          If        (n2 > 11 or DMO2 <> "")            .our min field is only 11 bytes
          MOve      "See Text",Min
          Append    "Minimum: ",str46
          Append    DMO1,str46
          append    CARR,str46
          reset     str46
          append    str46,hold2
                    if        (DMO2 <> "")
                    clear     str46
                    Append    "         ",str46
                    Append    DMO2,str46
                    append    CARR,str46
                    reset     str46
                    append    str46,hold2
                    endif
          Else      
          Move      DMO1,Min
          endif
.Unit of sale
          clear     UNITDATA
          Append    DUS1,UNitData
          Append    CARR,UNITDATA
.         Append    CRLF,UNITDATA
          Append    DUS2,UNITDATA       
.         Append    CRLF,UNITDATA  
          reset     UNITDATA
.
GEnder
          Call      Trim Using DSX1
          Call      Trim Using DSX2
          count     n2,DSX1
          If        (n2 > 15 or DSX2 <> "")            .our sex field is only 15 bytes
          MOve      "See Text",SEX
          clear     str46
          Append    "Gender: ",str46
          Append    DSX1,str46
          append    CARR,str46
          reset     str46
          append    str46,hold2
                    if        (DSX2 <> "")
                    clear     str46
                    Append    "         ",str46
                    Append    DSX2,str46
                    append    CARR,str46
                    reset     str46
                    append    str46,hold2
                    endif
          Else      
          Move      DSX1,Sex
          endif
.Media = our source -- need to parse out % match our code etc - sigh


.Net info  pulls info from 2 files - sigh
          call      Trim using Drs1
          call      Trim using Drs2
          clear     Netname
          Clear     Netinfo
          Clear     str55               
          if        (DRS1 <> "")
          move      "N002",NetName
          Setlptr   DRS1,3
          Append    DRS1,str55
                    if        (RunCHG > 0)
                    append    " +",Str55
                    Append    RunCHG,str55
                    append    b1,Str55
                    endif
          endif               
          if        (drs2 <> "")
          Append    ";(",Str55
          Append    DRS2,STR55
          append    " Minimum)",STR55
          endif
          IF        (Drs1 = "" & Drs2 = "")
          goto      NoNet
          endif
          reset     str55
          Count     n2,str55

          if        (n2 <= 38)
          move      str55,Netinfo
          else
          Clear     Netinfo
          Clear     NetName
          clear     str46
          Append    "Net Names: ",str46
          Append    Str55,str46
          append    CArr,str46
          reset     str46
          append    str46,hold2
          Endif     
NoNet     
          Move      DAD1,NDAD1          
          Move      DAD2,NDAD2          
          Move      DMO1,NDMO1          
          Move      DMO2,NDMO2          
          Move      DUS1,NDUS1          
          Move      DUS2,NDUS2          
          Move      DSX1,NDSX1          
          Move      DSX2,NDSX2          
          Move      DMD1,NDMD1          
          Move      DMD2,NDMD2          
          Move      DRS1,NDRS1          
          Move      DRS2,NDRS2          
          Move      DDLT,NDDLT          
          call      Nmsctst
                    if        not over
                    call      NMSCUpd
                    else
                    call      NMSCwrt
                    endif
          Endif
          MOve      Univ,UNiverse
.Selects / Segments
          Clear     BaseFlag
          MOve      c1,N4
          MOve      n4,NSelnum
          rep       Zfill,NSelNum
          PackKey     NSELFLD,ndatfld,Nselnum
.         PACKKey   MSegFld2,ddcnoa," 0"
.         move      c0 to n2
.         move      c2,Msegpath
          move      "S.Delete-NSELTST",Location
          pack      KeyLocation,"Key: ",NSELFLD
          Move      c0,SelBlankCnt

          Loop
          call      NSELKey
          if        over
          add       c1,SelBlankCnt
          endif
          Until     (Nselnum = "9999" or SelBlankcnt = 10)
.                   Until     over
.Now, Delete Select Record
          move    "S.Delete-NSELDEL",Location
          pack    KeyLocation,"Key: ",NSELFLD
            call    NSELDEL
          Add       c1,N4
          MOve      n4,NSelnum
          rep       Zfill,NSelNum
          PackKey     NSELFLD,ndatfld,Nselnum
          Repeat 
..........................................................................
.need two loops   1st to find the BASE
.then reset and read again
.
          call      debug
          Clear     HOldbase                .reset varaible
          PACKKey   MSegFld2,ddcnoa," 0"
          move      c0 to n2
          move      c2,Msegpath
          Loop
          call      MsegKEy
          Until     OVer

          if        (SgPt = "B")               .we found the base
          MOve      SGSQNO,n4
          add       c1,N4                      .they start at 0 we start at 1
          Move      n4,HoldBase
          rep       zfill,Holdbase
.begin patch 1.3  we are going to read all and pull largest as base
.          Break
.end patch 1.3
          endif
.begin patch 1.3
          Move      Universe,N10
          if        (SGC > N10)
          move      SGC,Universe
          endif
.end patch 1.3
          add       c1,N2
          PACKKey   MSegFld2,ddcnoa,n2
          Repeat
          
.Did we find the base??
          if        (holdbase = "")               .nothing marked as base
          move      c0 to N10
          PACKKey   MSegFld2,ddcnoa," 0"
          move      c0 to n2
          move      c2,Msegpath

          Loop
          call      MsegKEy
          Until     OVer

                    if        (SGC > n10)               .if count is largest so far - maybe the base
                    MOve      sgc,N10
                    MOve      SGSQNO,n4
                    add       c1,N4                      .they start at 0 we start at 1
                    Move      n4,HoldBase
                    rep       zfill,Holdbase
                    endif

          add       c1,N2
          PACKKey   MSegFld2,ddcnoa,n2

          Repeat

          endif
.lets to on
............        
          MOve      No,BaseFlag
          Clear     NSelBase
.reset
          PACKKey   MSegFld2,ddcnoa," 0"
          move      c0 to n2
          move      c2,Msegpath

          Loop
          call      MsegKEy
          Until     over
          MOve      SGSQNO,N4                            .Seq #
          add       c1,N4
.         Move      N4,NSELNUM
.         Rep       Zfill,Nselnum
          Move      Ndatfld,NSELLIST
          MOVe      c0,NselPrice
          if        (SGP > 0)
          Move      SGP,NSelPRice
          endif
          Move      SGD,NSELSNAME                      Name
          move      c0,NSelQty
          if        (SGC > 0)
          Move      SGC,NSelQty
          endif
          Clear     NSelExc
          Clear     NSelBase
          MOVe      c9,NSELINDEX

          IF        (NSelprice = C0 & SGPT <> "I")
          Move      "009",NSELDESC
          endif
          scan      "Inquire",SGD
          IF        Equal
          Move      "009",NSELDESC
          endif
          reset     SGD
          Scan      "Exch/",SGD               .exch/rent
          If        Equal
          MOVe      c1,NSELEXC
          MoveFptr  SGD,n3                   .save formpointer
          sub       c1,n3                
          setlptr   SGD,n3              .set length pointer to form pointer less 1
          reset     SGD                    .reset formpointer
          Clear     NselName
          append    sgd,NSelName                      .move the name less "Exch/"
          reset     NSelName
.         Move      SGD,NSELSNAME                      .move the name less "Exch/"
          Else
          Reset     SGD
          Scan      "Exch",SGD
                    If        Equal
                    MOVe      c2,NSELEXC            exchange only
                    MoveFptr  SGD,n3                   .save formpointer
                    sub       c1,n3                
                    setlptr   SGD,n3              .set length pointer to form pointer less 1
                    reset     SGD                    .reset formpointer
                    Clear     NselName
                    append    sgd,NSelName                      .move the name less "Exch/"
                    reset     NSelName
                    endif
          endif
.
.         Clear     NSelBase
          CLear     NSelDESC             .price modifier
          if        (SGR = "M")
          Move      "001",NSELDESC
          Elseif    (SGR = "F")
          MOVe      "002",NSELDESC
          endif
.         if        (SgPt = "B" & BaseFlag <> "Y")
          MOve      Holdbase,n5
          call      debug
          if        (SgPt = "B" & BaseFlag <> "Y" or SgPt = "I" & N5 = n4 & Baseflag <> "Y")
          move      "BASE",NSelBase
          MOve      Yes,BaseFlag
.         Elseif    (SgPt = "B" & BaseFlag = "Y")
          Elseif    (SgPt = "B" & BaseFlag = "Y" or SgPt = "I" & Sgc <> 0)
          move      "SEC.",NSelBase
          Elseif    (SgPt = "S")
          Move      HoldBase,NSelbase
          endif
.Find Next available Select Number
.         move      N4,N5     .save it
.            For    N4,"1","9998"
SelTST
            move              N4,NSELNUM
            rep               zfill,NSELNUM
          MOve      NSelNum,NSelIndex
            pack              NSELFLD,NSELLIST,NSELNUM
            move              "S.SaveA-NSELTST",Location
            pack              KeyLocation,"Key: ",NSELFLD
            call              NSELTST
            if      over
            Goto    WRTSel
            endif 
.we have a problem
          add       c1,n4
.add code if n4 > 99  big trouble       
          goto      SelTst
WRtSEl    
          Call      NSelWRt
.         Move      n5,n4     .restore it
          MOve      SGSQNO,N2                            .Seq #
          add       c1,n2
.         move      n4,n2
          PACKKey   MSegFld2,ddcnoa,n2
.         if        (NselBase = "BASE" or NSelBase = "SEC.")           .if this is a base following records should be under it
.         Move      NSelNum,HoldBase
.         endif
          REpeat

.SGPT     Dim       1          9-9    Seg pricing Type
.SGR      Dim       1         53-53   Rate
.SEGCAT   Dim       3         54-56   Cat code
.         
.         
.NSELPCOMM          DIM       1     104-104       SELECT PRICE IS COMMISSIONABLE
.NSELDESC DIM       3     105-107       SELECT PRICE MODIFIER
.NSELINACTIVE       DIM       1     108-108       SELECT INACTIVE?  1 = YES
.NSELNOTES          DIM       1     110-110       1 = INDICATES USER SHOULD VIEW NOTES ABOUT THIS SELECT
.NSELEXC            DIM       1     111-111       1  = Exc/Rent, 2 = Exchange Only, 3 = Rental Only
.NSELBASE DIM       4     112-115       SELECT NUMBER OF ASSOCIATED BASE - BASE RECORDS WILL HOLD BLANK VALUE!!
.. BASE   = BASE - ONLY ONE PER DATACARD
.. SEC.   = SECONDARY BASE
.. Nxxx   = NUMBER OF BASE/SECONDARY BASE
.. "    " = NEITHER BASE NOR SELECT OFF OF BASE
.NSELINDEX          DIM       4     116-119       INDEX FOR PRINTING/DISPLAY
.NSELDATE DIM       8     120-127       SELECT DATE
.NSELINIT DIM       3     128-130       USER INITIALS
.         
          call      Textout


.Min List Owner COntact info
          Move      GPH1a,NDatCntPHN
          Move      GPHFa,NDatCntfax
          MOve      LstNum,NDatCntNum
          move      DCnt,NDatCnt
          move      LOEML,NDatCntEML
.         
          packkey   NDatCntFLd,LstNum
          call      NdatCnttst
          if        not over
          call      NdatCntUpd
          else
          call      NdatCntwrt
          endif
          
          call      Xref
.         call      next routine - don't update unless all info ok

          REturn
.         endif
          
.         Repeat

          Goto      EOJ
          
Noxref    clear     Ndatfld1
          Clear     Ndatfld3            
          Clear     Ndatfld4
          Clear     Ndatfld5
          Clear     Ndatfld6
          Clear     Ndatfld7
          Clear     str5
          clear     str25
          move      ownlon,str5
          move      ownocpy,str25
          unpack    Ownnum,str2,str4
          Clear     Ownnum
          clear     Ownocpy
          packkey   Nownfld,str4
          Rep       Zfill in Nownfld
          IF        (Nownfld <> "0000")
          call      Nownkey
          endif

          Packkey   Ndatfld2,"02L",DTTL
          Clear     MlstName
          Clear     str1
          clear     LStNUm
          clear     Elstcde
          call      NdatAim
          If        Not OVer
                    If        (elstcde = "C")
                    MOVE      "C",str1
                    endif
          endif               
          
          if        (mode  = "B")
.          Write     NoMatch,seq;*cdfon,DDcnoa,Lstnum,Dttl,Mlstname,str1,Dlow,Gnama,str5,str25,ownnum,ownocpy
          Write     NoMatch,seq;*cdfon,DDcnoa,Lstnum,Dttl,Mlstname,DDLT,Dlow,Gnama,str5,str25,ownnum,ownocpy
          add       c1,NoCnt
          endif
          return

xref
          clear     str25
          clear     str5
          move      ownlon,str5
          MOVe      Ownocpy,str25
          unpack    Ownnum,str2,str4
          packkey   Nownfld,str4
          Rep       Zfill in Nownfld
          call      Nownkey

          if        (mode  = "B")
          WRite     Match,seq;*cdfon,DDCnoa,HmList,DTTl,Mlstname,b1,dlow,Gnama,str5,str25:
                    ownnum,ownocpy
          add       c1,MCnt             
          endif
......
.gender info
.         call      Debug
          Move      c0,NDatfem
          move      FemPer,NdatFem
          Move      c0,NDatMen
          move      MalPer,NdatMen
          if        (DDLT = "I")        .inactive
          move      "W",Status
          Elseif    (DDLT = "A")
          move      B1,Status
          endif
.         
.if old record had mlr x ref etc keep it
.Need to reposition File pointer in case another process threw it off
                    If        (HoldOwn <> Ownnum)                 
                    MOVe      HoldOwn,Ownnum                     .New List owner
                    endif

                    move      "Save-NDATTST",Location
                    pack      KeyLocation,"Key: ",NDATFLD
                    call      NDATTST
                    if        over
                              IF        (NewFlag <> Yes)
                              alert     note,"Datacard no longer exists!  Contact I.S. with Datacard Number.",result
                              endif
                    MOve      C1,NDATCONV                        .converted     
                    call      NDATwrt             
                    endif

                    Clear     Hold2
                    MOve      DTTL,Hold2
.                   call      debug
                    Setitem   Listedit,0,Hold2
                    Move      c1,N1
                    move      c2,n1
                    call      CaseChange using ListEdit,Hold2,n1             .4=sentence case, 2= Title case, 3 = Upper case
                    Getitem   Listedit,0,Hold2
                    Clear     Mlstname            
                    move      Hold2,MLstname
.                   Reset     Mlstname,75
.                   reset     Mlstname
.                   call      Trim using Mlstname
.                   movelptr  Mlstname,n2
.                   sub       c1 from n2
.                   setlptr   Mlstname,n2
.Now add code to take care of "And, Or, Of, At, For  etc)   
                    reset     MlstName
                    Scan      " With ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "Ww",Mlstname
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif

                    reset     MlstName
                    Scan      " And ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "Aa",Mlstname
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
                    reset     MlstName
.                   
                    Scan      " Or ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "Oo",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
.                   
                    Scan      " On ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "Oo",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
.                   
                    reset     MlstName
                    Scan      " Of ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "Oo",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
.                   
                    reset     MlstName
                    Scan      " At ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "Aa",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
.                   
                    reset     MlstName
                    Scan      " To ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "Tt",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
.                   
                    reset     MlstName
                    Scan      " For ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "Ff",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif

                    reset     MlstName
                    Scan      " The ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "Tt",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif

                    reset     MlstName
                    Scan      " Usa ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "sSaA",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
                              
                    reset     MlstName
                    Scan      "In",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "Ii",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
.                   
                    reset     MlstName
                    Scan      "Tv",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "vV",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
.                   
                    reset     MlstName
                    Scan      " A ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "Aa",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif

                    reset     MlstName
                    Scan      "USo ",Mlstname
                              if        Equal
.                             call      debug
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "sSoO",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif

                    reset     MlstName
                    Scan      "USa ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "sSaA",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
                    reset     MlstName
                    Scan      "Sos ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "sSoO",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
                    reset     MlstName
                    Scan      "Us ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "sS",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
                    reset     MlstName
                    Scan      "Als ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "sSlL",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
                    reset     MlstName
                    Scan      "Po  ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "sSlL",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
                    reset     MlstName
                    Scan      "ALs ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "sSlL",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
                    reset     MlstName
                    Scan      "SOs ",Mlstname
                              if        Equal
                              Movelptr  Mlstname,n2             .save length pointer
                              bump      Mlstname,c1
                              lenset    MlstName
                              rep       "sS",Mlstname
                              reset     Mlstname,75
                              setlptr   Mlstname,n2             .Restore Length pointer
                              reset     Mlstname
                              endif
                    reset     MlstName
.check for things that must be caps
                    packkey   ndatufld,"000000"
                    call      NDatuTst
                    Movelptr  Mlstname,n4                  .save length pointer
                    Loop
                    call      Ndatuks
                    until     over
                    Reset     MLstname
                    call      Rtrim using DatUname
                    scan      Datuname,Mlstname
                    if        Equal                         .Hit
                    count     n2,datuName
                    movefptr  Mlstname,n3
                    add       n2,n3
                    setlptr   Mlstname,n3
                    rep       LowUp,Mlstname
                    setlptr   Mlstname,n4                   .restore lenght pointer
                    reset     Mlstname                      .restore FOrm pointer
                    endif
                    
                    repeat
                    Reset     MLstname
                    If        (NewFlag = Yes)
                    move      Mlstname,Olstname
                    Clear     NDATWEB
                    clear     NDATOFF             
                    endif
                    Setitem   Listedit,0,MLstName
.
                    MOve      C1,NDATCONV                        .converted     
.                   move      "Save-NDATUPD",Location
.                   pack      KeyLocation,"Key: ",NDATFLD
.                   call      NDATUPD
                    move      "Save-NDATDel",Location
                    pack      KeyLocation,"Key: ",NDATFLD
                    call      NDATDel
                    move      "Save-NDATwrt",Location
                    pack      KeyLocation,"Key: ",NDATFLD
                    call      NDATWRt
.begin patch typist                    
                    call      DatTypist
.end patch typist                    
          
          return
...............................................................................
Exclusive

          clear     str5
          clear     str25
          move      Ownlon,str5
          move      ownocpy,str25
          unpack    Ownnum,str2,str4
          packkey   Nownfld,str4
          Rep       Zfill in Nownfld
          call      Nownkey
          if        (mode  = "B")
          
          WRite     Exclusive,seq;*cdfon,DDCnoa,HMlist,Dttl,Mlstname,Elstcde,DLow,Gnama,str5,str25,ownnum,ownocpy
          add       c1,Ecnt
          endif
          Return
...............................................................................
TextOut
          reset     Hold2
          call    Trim using hold2
                    if (hold2 <> "")
          move    "501",result
          movelptr hold2,howmany
          pack    NTXTLIST,LSTNUM
          for N2,C1,"15"
                  move    hold2,NTXTTEXT
                  pack    NTXTNUM,N2
                  rep         zfill,Ntxtnum
                  pack    NTXTFLD,NTXTLIST,NTXTNUM
                  rep         zfill,NtxtFld
                  move    "Verify-NTXTWRT",Location
                  pack    KeyLocation,"Key: ",NTXTFLD
                  call    NTXTWRT
                  if (result > howmany)
                          break 
                  endif 

                  reset   hold2,result
                  add     "500",result
          repeat 
          endif 
          Clear Hold2
          return

................................................................................
Del2ndary
................................................................      
.addressing
          packkey   Naddfld,Ndatfld
          move    "Verify-NADDTst",Location
          call      Naddtst
          loop      
          move    "Verify-NADDKs",Location
          call      Naddks
          Until     over
          if        (ndatfld <> Naddlist)
          Break
          endif
          MOVe      NaddList,NAddBLIST  
          Move      NaddNum,NAddBNUM    
          Move      NAddPrice,NAddBPRICE
          Move      NaddDesc,NAddBDESC
          Move      NDatBstamp,NAddBStamp
          move    "Verify-NTXTWRT",Location
          call      NAddBWRt

          move    "Verify-NADDDel",Location
          
          call      NaddDel

          repeat
................................................................      
.arrangement
          packkey   Narrfld,Ndatfld
                    move      "Verify-NArrTst",Location
          call      Narrtst
          loop      
                    move      "Verify-NArrKs",Location
          call      Narrks
          Until     over
          if        (ndatfld <> Narrlist)
          Break
          endif
          MOVe      NArrList,NArrBLIST  
          Move      NArrNum,NArrBNUM    
          Move      NDatBstamp,NArrBStamp
                    move      "Verify-NTXTWRT",Location
          call      NArrBWRt
                    move      "Verify-NArrDel",Location
          call      NArrDel

          repeat
................................................................      
.slct
          packkey   NSltfld,Ndatfld
          move    "Verify-NSLtTst",Location
          call      NSlttst
          loop      
          move    "Verify-NSltKS",Location
          call      NSltks
          Until     over
          if        (ndatfld <> NSltlist)
          Break
          endif
          Move      NSltList,NSltBLIST  
          Move      NSltNum,NSltBNUM              
          Move      NSltPRice,NSltBPRICE          
          Move      NSltDesc,NSltBDESC  
          Move      NSltQty,NSltBQTY    
          Move      NDatBStamp,NSltBStamp
          move    "Verify-NSLTBWrt",Location
          call      NSltBWrt
          move    "Verify-NSLTDel",Location
          call      NSltDel
          repeat
................................................................      
.src      
          packkey   NSrcfld,Ndatfld
          move    "Verify-NSrcTst",Location
          call      NSrctst
          loop      
          move    "Verify-NSrcKS",Location
          call      NSrcks
          Until     over
          if        (ndatfld <> NSrclist)
          Break
          endif
          Move      NSrcList,NSrcBLIST  
          Move      NSrcNum,NSrcBNUM              
          Move      NSrcPeR,NSrcBPeR    
          Move      NDatBStamp,NSrcBStamp
          move    "Verify-NSrcBWrt",Location
          call      NSrcBWrt
          move    "Verify-NSrcDel",Location
          call      NSrcDel
          repeat

          return
................................................................................
.looking for selects, addressing
CheckSlct    
. note to self add file needs to be backed up and purged
.TYPE OF CODES
.1         "A"         ADDRESSING CODE
.2         "C"         CLEANED CODE
.3         "D"         DELIVERY CODE
.4         "L"         SELECTION CODE
.5         "M"         MEDIA CODE
.6         "N"         NET NAME CODE
.7         "P"         SAMPLE CODE
.8         "R"         ARRANGEMENT CODE
.9         "S"         SOURCE CODE
.10        "T"         CATEGORY CODE

          call      Trim using catc
          count     n1,catc
          if        (n1 <> c3)
          move      No,slctflag
          return
          endif
          Packkey   NrefFld3,CATC
          move      c2,Nrefpath
          call      nrefkey
          if        over      
          move      No,slctflag
          return
          else
          move      Yes,Slctflag
                    If        (NrefCode = "A")                  ..addressing
                    Move      Ndatfld,NaddList
                    move      Nrefnum,Naddnum
                    packkey   NaddFld,ndatfld,nrefnum
                    move      cat$,Naddprice
                    move      "000",nadddesc
                              If        (CatR = "M")
                              move      "001",nadddesc
                              elseif    (catR = "L")
                              move      "002",NaddDesc
                              endif
                    call      NaddWrt   
                    ELseIF    (NrefCode = "L" & Cat$ <> 0)                  .Select
                    packkey   NSltFld,ndatfld,nrefnum
                    Move      Ndatfld,NSltList
                    move      Nrefnum,NSltnum
                    move      cat$,NSltPrice
                    move      "000",NSltdesc
                              If        (CatR = "M")
                              move      "001",NSltdesc
                              elseif    (catR = "L")
                              move      "002",NSltDesc
                              endif
                    call      NSltWrt   
                    ELseIF    (NrefCode = "T")                  .Category
.                   call      debug
                    packkey   NCatFld,ndatfld,nrefnum
                    Move      Ndatfld,NCatList
                    UNpack    Nrefnum into NCatCOde,NCatNum
                    call      NcatTst
                              if        over            .skip if already there
                              call      NCatWrt   
                              endif
                    Move      No,SLctflag                   .we want to go ahead and write the MIN category as well               
                    ELseIF    (NrefCode = "S")                  .Source
                    
                    packkey   NSrcFld,ndatfld,nrefnum
                    Move      Ndatfld,NSrcList
                    move      Nrefnum,NSrcnum
                    Clear     NSrcPer                         .Min does not provide
                    call      NSrcWrt   
                    Else                                    .not one of those ????
                    move      No,slctFlag
                    return
                    Endif
          Endif
          return
................................................................................
NoLoop1
          Scan      "WINERR:0x2",error
          if        equal
          Display   *p35:11,"MDC_MAIN Not found - continuing"
          endif
....      
EOJ       
.         Close     M2Nfile
          if        (mode = "M")
          goto      EojError
          endif
          Setitem   CSVStatus(1),0,"Creating spreadsheet report"
          activate  CSVStatus(1)
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

.sheet 2 no xref
            sheets.item giving sheet using 2
            setprop sheet.range("a1:z250").Font,*Name="Times New Roman", *Size=11
            setprop sheet.PageSetup,*Orientation=xlLandscape
            setprop sheet.PageSetup,*TopMargin=TopMargin
            setprop sheet.PageSetup,*BottomMargin=BottomMargin
            setprop sheet.PageSetup,*FooterMargin=TopMargin
            setprop sheet.PageSetup,*LeftMargin=LeftMargin
            setprop sheet.PageSetup,*RightMargin=RightMargin
            setprop sheet.range("a1","a1"),*Value="List Data NOT Applied",*HorizontalAlignment=AlignLeft
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
            setprop sheet.range("D3","D3"),*Value="List Name",*HorizontalAlignment=aligncenter
            setprop sheet.range("D3:D3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("D3:D3").Font,*Bold="True"
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

.Sheet 3 we have list # xref And its EXCLUSIVE TO NIN          
            sheets.item giving sheet using 3
            setprop sheet.range("a1:z250").Font,*Name="Times New Roman", *Size=11
            setprop sheet.PageSetup,*Orientation=xlLandscape
            setprop sheet.PageSetup,*TopMargin=TopMargin
            setprop sheet.PageSetup,*BottomMargin=BottomMargin
            setprop sheet.PageSetup,*FooterMargin=TopMargin
            setprop sheet.PageSetup,*LeftMargin=LeftMargin
            setprop sheet.PageSetup,*RightMargin=RightMargin
            setprop sheet.range("a1","a1"),*Value="Exclusive ListFound",*HorizontalAlignment=AlignLeft
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
.**********************
          write     Match,Seq;*cdfon,Mcnt
          Close     Match
.**********************
          Setitem   CSVStatus(2),0,"Processing Matches"
          activate  CSVStatus(2)
          activate  CSVStatus(3)

          OPen      Match,"c:\work\match.csv",read
          Move      C0,Counter
          Loop                
          add       c1,counter
          move      Counter,DimCounter
          Setitem   CSVStatus(3),0,DimCounter

          Read      Match,seq;*cdfon,DDCnoa,HmList,DTTl,Mlstname,b1,dlow,gnam,str5,str25:
                    ownnum,ownocpy

          Until     over                          

                    sheets.item giving sheet using 1
                    Move      CellRowCnt,ExRow
                    call      Trim using ExRow
          Pack      Cell,"B",ExRow
          setprop sheet.range(Cell),*Value=DDCNOa
          Pack      Cell,"C",ExRow
          setprop sheet.range(Cell),*Value=HMList
          Pack      Cell,"D",ExRow
          setprop sheet.range(Cell),*Value=DTTL
          Pack      Cell,"E",ExRow
          setprop sheet.range(Cell),*Value=Mlstname
          Pack      Cell,"F",ExRow
          setprop sheet.range(Cell),*Value=b1
          Pack      Cell,"G",ExRow
          setprop sheet.range(Cell),*Value=Dlow
          Pack      Cell,"H",ExRow
          setprop sheet.range(Cell),*Value=GNam
          Pack      Cell,"I",ExRow
          setprop sheet.range(Cell),*Value=str5
          Pack      Cell,"J",ExRow
          setprop sheet.range(Cell),*Value=str25
          Pack      Cell,"K",ExRow
          setprop sheet.range(Cell),*Value=OWNNum
          Pack      Cell,"L",ExRow
          setprop sheet.range(Cell),*Value=OWNOCPY
          
          Add       c1 to CellRowCnt
          
          Repeat
.**********************
          write     NoMatch,Seq;*cdfon,NoCnt
.
          Close     Nomatch
.**********************
          move      c0,counter
          Setitem   CSVStatus(2),0,"Processing Non matches"

          OPen      NoMatch,"c:\work\Nomatch.csv",read

          Loop                
          add       c1,counter
          move      Counter,DimCounter
          if        (counter >= "9999")
          call      debug
          endif
          Setitem   CSVStatus(3),0,DimCounter
          REad      NoMatch,seq;*cdfon,DDcnoa,Lstnum,Dttl,Mlstname,str1,Dlow,Gnam,str5,str25,ownnum,ownocpy
          Until     Over

                    sheets.item giving sheet using 2
                    Move      CellRowCnt1,ExRow1
                    call      Trim using ExRow1


          Pack      Cell,"B",ExRow1
          setprop sheet.range(Cell),*Value=DDCNOa
          Pack      Cell,"C",ExRow1
          setprop sheet.range(Cell),*Value=Lstnum
          Pack      Cell,"D",ExRow1
          setprop sheet.range(Cell),*Value=DTTL
          Pack      Cell,"F",ExRow1
          setprop sheet.range(Cell),*Value=str1
          Pack      Cell,"G",ExRow1
          setprop sheet.range(Cell),*Value=Dlow
          Pack      Cell,"H",ExRow1
          setprop sheet.range(Cell),*Value=GNam
          Pack      Cell,"I",ExRow1
          setprop sheet.range(Cell),*Value=str5
          Pack      Cell,"J",ExRow1
          setprop sheet.range(Cell),*Value=str25
          Pack      Cell,"K",ExRow1
          setprop sheet.range(Cell),*Value=OWNNum
          Pack      Cell,"L",ExRow1
          setprop sheet.range(Cell),*Value=OWNOCPY
          Add       c1 to CellRowCnt1

          REpeat
.**********************

          write     Exclusive,Seq;*cdfon,Ecnt

          Close     Exclusive
.**********************
          Setitem   CSVStatus(2),0,"Processing Exclusive matches"

          OPen      Exclusive,"c:\work\Exclusive.csv",read
          Move      C0,Counter
          Loop                
          add       c1,counter
          move      Counter,DimCounter
          Setitem   CSVStatus(3),0,DimCounter

          read      Exclusive,seq;*cdfon,DDCnoa,HMlist,Dttl,Mlstname,str1,DLow,Gnam,str5,str25,ownnum,ownocpy

          Until     Over

            sheets.item giving sheet using 3
            Move    CellRowCnt2,ExRow
            call    Trim using ExRow
          Pack      Cell,"B",ExRow
          setprop sheet.range(Cell),*Value=DDCNOa
          Pack      Cell,"C",ExRow
          setprop sheet.range(Cell),*Value=HMList
          Pack      Cell,"D",ExRow
          setprop sheet.range(Cell),*Value=DTTL
          Pack      Cell,"E",ExRow
          setprop sheet.range(Cell),*Value=Mlstname
          Pack      Cell,"F",ExRow
          setprop sheet.range(Cell),*Value=str1
          Pack      Cell,"G",ExRow
          setprop sheet.range(Cell),*Value=Dlow
          Pack      Cell,"H",ExRow
          setprop sheet.range(Cell),*Value=GNam
          Pack      Cell,"I",ExRow
          setprop sheet.range(Cell),*Value=str5
          Pack      Cell,"J",ExRow
          setprop sheet.range(Cell),*Value=str25

          Pack      Cell,"K",ExRow
          setprop sheet.range(Cell),*Value=OWNNum
          Pack      Cell,"L",ExRow
          setprop sheet.range(Cell),*Value=OWNOCPY
          
          Add       c1 to CellRowCnt2

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
              pack            Taskname,"c:\work\M2N0002_",Timestamp
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

ManualUpdate        Routine DimPtr,Dimptr1
.         call      debug
          call      Trim using Dimptr
          if        (DImPTr <> "")
          move      Dimptr,Comment
          Else
          REturn
          endif
          call      Trim using Dimptr1
          move      Dimptr1,Inpname
          Clear     Elstcde
          match     "NEW",INpname
                    if        Equal
                    move      Yes,NewFlag
                    else
                    Move      No,NewFlag
                    endif
          call      Start
          REturn
....Typist
DatTypist
          IF        (Mode = "B")                  .Batch/Auto mode
          REturn
          endif
          if        (inits = "   " or Inits ="")
          clock      port to str3           .plb note
          unpack     str3 into str2,str1
          pack       str3 from str1,str2
          Packkey   Nusefld from Str3
          REP       zfill IN NUSEFLD
          CALL      NUSEKEY
          move       nuseinit to inits 
          endif
.START PATCH 1.34 ADDED LOGIC
                        clear   NTYPDET
.END PATCH 1.34 ADDED LOGIC
                              Clock               Timestamp to Timestamp
                        unpack          timestamp,CC,YY,MM,DD
                            pack                  typdate from cc,yy,mm
                              Packkey         Ntypfld from cc,yy,mm,Inits
                              Move                c1 to Ntyppath
                              move      "NTyptst",Location
                              pack      KeyLocation,"Key: ",NTypFLD
                              call      Ntyptst
                              If        over      
                                        If        (newflag = yes)
                                        MOve      c1 to lstcount
                                        else
                                        MOve      c1 to lstUcount
                                        endif
                              move      "NTypwrt",Location
                              pack      KeyLocation,"Key: ",NTypFLD
                              Move      Inits to NtypType
                              call      ntypwrt
                              Else
                              move      "NTypkey",Location
                              pack      KeyLocation,"Key: ",NTypFLD
                              call      ntypkey
                                        If        (newflag = yes)
                                        add       c1 to lstcount
                                        else
                                        add       c1 to lstUcount
                                        endif
                              move      "NTypUpd",Location
                              pack      KeyLocation,"Key: ",NTypFLD
                              call      ntypupd
                              endif
.START PATCH 1.34 ADDED LOGIC
                        clear   NTYPDET
.END PATCH 1.34 ADDED LOGIC
                              Packkey             Ntypfld from cc,yy,mm,"99 "
                              Move                "99 " to NtypType
                              move      "NTyptst",Location
                              pack      KeyLocation,"Key: ",NTypFLD
                              call                Ntyptst
                              if                  over        ;create the record
                                        If        (newflag = yes)
                                        move      c1 to lstcount
                                        else
                                        Move      c1 to lstUcount
                                        endif
                              pack    typdate from cc,yy,mm
                              move      "NTypwrt",Location
                              pack      KeyLocation,"Key: ",NTypFLD
                              move      "NTypwrt",Location
                              pack      KeyLocation,"Key: ",NTypFLD
                              call                Ntypwrt

                              Else
                              move      "NTypkey",Location
                              pack      KeyLocation,"Key: ",NTypFLD
                              call                ntypkey
                                        If        (newflag = yes)
                                        add       c1 to lstcount
                                        else
                                        add       c1 to lstUcount
                                        endif
                              move      "NTypupd",Location
                              pack      KeyLocation,"Key: ",NTypFLD
                              call                Ntypupd
                              endif
          Return
.end Typist


          Include   M2NIO.inc
          Include   M2NLoIO.inc
          INclude   MdcMainIO.inc
          Include   MDCSegIO.inc
          Include   MDCMSCIO.inc
          Include   MDC035IO.inc
          Include   MDC090IO.inc
          Include   MDCTXTIO.inc
          Include   MDC060IO.inc
          Include   Ndatio.inc
          Include   Nownio.inc
          Include   NTXTIO.inc
          Include   NADDIO.inc
          include   NArrIO.inc 
          include   NsrcIO.inc
          Include   NModio.inc                 .not touched yet
          INclude   NMDCMscIO.inc - additional info (currently from MIN) need to incorporate  backup ?
          INClude   NMDCCATIO.inc - Min Category                                              backup?  
          include         NSELIO.inc
               Include        NSltio.inc
               include        Nrefio.inc
          include   NdatUio.inc
               include         gnxtio.inc
          Include   NDatCntio.inc
          Include   NCatio.inc
.Begin Typist
          include   Nuseio.inc
          Include   Ntypio.inc
.end Typist
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