Pc        Equ       0
          Include   Common.inc
          INclude   Cons.inc  
          include   srdsdd.inc
          include   srdsLOdd.inc
          include   SRDSSELdd.inc                             .Selects(segments) with counts
          include   SRDSSLtdd.inc                             .selects,addressing, etc
          Include   S2Ndd.inc
          Include   S2NLodd.inc
          Include   ndatdd.inc
          Include   Nowndd.inc
.          Include   NTXTDD.inc                            .SRDS does not supply text
          Include   NADDDD.inc
          include   NArrdd.inc                .arrangement codes  mmm anything to do here??? I think Defunct DH
          Include   NSrcDD.inc                 
          Include   NModDD.inc                 .not touched yet
          INclude   Xls.inc
          include         NSELdd.inc
          Include   NSltdd.inc
          include   Nrefdd.inc
          include   NDatudd.inc
          include         gnxtdd.inc
          Include   NDatCntDD.inc
          Include   NCatdd.inc
          include   Nusedd.inc
          Include   Ntypdd.inc
.begin patch 1.1
          include   SRDSTXTdd.inc                             
          Include   NTXT1DD.inc
.end patch 1.1
. backup files
          Include   ndatBdd.inc
.          Include   nTxtBdd.inc                            .SRDS does not supply text
          Include   nAddBdd.inc
          include   NArrBdd.inc
          include   NSltBdd.inc
          include   NSrcBdd.inc
          Include   NModBDD.inc                 .not touched yet
.......................................................
.
Release   INit      "1.52"     AH           .Adjusted TYPOUT logic
REldate   Init      "13 November 2013"
.Release   INit      "1.51"     DLH           .tighten addressing/selects
.REldate   Init      "2013 October 2"         .
.Release   INit      "1.5"     DLH           .tighten up dates
.REldate   Init      "2013 August 21"         .
.Release   INit      "1.4"     DLH           .MOve temp files to \\nins1\e\data and use PLB file Manager
.REldate   Init      "2013 August 08"         .job has been running 8 plus hours 
.Release   INit      "1.3"     DLH           .Issue with old selections not being deleted, resulting in duplicates
.REldate   Init      "2013 April 26"         .Note once NINslt is cleaned of duplicates it needs to be changed to Dupes not allowed!!!
.Release   INit      "1.2"     DLH           .Problems with a zero value universe field from srds
.REldate   Init      "06 December 2012"
.Release   INit      "1.1"     DLH           .srdstxt - additional pricing information - unformated text
.REldate   Init      "14 September 2011"
.Release   INit      "1.0"     DLH           .NEW
.REldate   Init      "11 May 2011"
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
Mode      Dim       1                   ...Default is "B" batch mode. can be called from S2N0003 then in "M" manual mode
HoldOwn   Dim       6                   .Hold the LO number as it may have changed since we updated.
NewFlag   Dim       1                   Holds "Y" if new list to be created
.controlled by common varaible "COMMENT" if empty - Batch
Match     File      .Holds output on matched lists
NoMatch   File      .Holds output on Unmatched lists
Exclusive File      .Holds Output on Exclusive lists
Nocnt     form      5
MCnt      form      5
ECnt      form      5
Holdn4    form          4
Str50a    dim       50
Str50b    dim       50
Str50c    dim       50
Str50d    dim       50
Str50e    dim       50

FileCheck FIle
trapcount form      4

...................................................................................
CaseChange external "SPELLCHECK;CaseChange"
...................................................................................

.THis program is the "2nd" run to auto update list information from Converted SRDS List system CSV format live files
.First release kicks out "printed" reports only no update.
.
.xRow     Dim       7
EXRow1    Dim       7
ExRow    Dim        7
.EXRow1   Dim       5
HoldLIst    Dim       6         Hold SRDS List #
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
          if        (comment = "")
          move      "B",Mode
          MOve      "SRDS",Password
.begin patch 1.4
.          Erase     "c:\work\Match.csv"
.          Erase     "c:\work\NoMatch.csv"
.          Erase     "c:\work\Exclusive.csv"
          Erase     "\\nins1\e\data\Match.csv"
          Erase     "\\nins1\e\data\NoMatch.csv"
          Erase     "\\nins1\e\data\Exclusive.csv"
          
.          PRepare   Match,"c:\work\Match.csv",exclusive
.          PRepare   NoMatch,"c:\work\NoMatch.csv",exclusive
.          PRepare   Exclusive,"c:\work\Exclusive.csv",exclusive
          PRepare   Match,"\\nins1\e\data\Match.csv|nins1:502",exclusive
          PRepare   NoMatch,"\\nins1\e\data\NoMatch.csv|nins1:502",exclusive
          PRepare   Exclusive,"\\nins1\e\data\Exclusive.csv|nins1:502",exclusive
.end patch 1.4
          Else      
          MOVE      "M",mode
          endif   





.....>>>>>>>>>>>>>>>>>>>>>>>TEMP
.         Move      "059855",SRDSFLD
.         call      SRDSKey
.....>>>>>>>>>>>>>>>>>>>>>>>TEMP
          
.         

.loop1 process SRDS_Main
Loop1
          IF        (Mode = "B")                  .Batch/Auto mode
          loop
          move    "READ-SRDSKS",Location
          Call      SRDSKS
          until     over
          add       c1,CountIn
          Display   *p15:11,CountIn,b1,SRDSLSTNUM
.          if        (srdslstnum = "018132" or srdslstnum = "018187" or srdslstnum = "019782"  or srdslstnum = "058222"  or srdslstnum = "066100"  or srdslstnum = "893519")
          if        (srdslstnum = "930260")
          call      Debug        
          endif

          Clear     Ndatfld
          Clear     Nownfld
          Clear     OWNLON
          Clear     OWNOCPY
          call      PRocess
          repeat
          goto      EOj
          Else
          packkey   SRDSFLD,COmment
          Call      SRDSKey
          Display   *p15:11,"Manual Mode ",SRDSLSTNUM
          Clear     Ndatfld
          Clear     Nownfld
          Clear     OWNLON
          Clear     OWNOCPY
          call      PRocess
          goto      EOj
          endif     
Process
.Get SRDS LO info
          Packkey   SRDSLOFLd,SRDSOWNNUM
          move    "READ-SRDSLOKey",Location
          pack    KeyLocation,"Key: ",SRDSLOFLD
          call      SRDSLOKey
          Packkey   S2NLoFld,SRDSOWNNUM
          move    "READ-S2NLoKey",Location
          pack    KeyLocation,"Key: ",S2NLOFLD
          call      S2NLoKey
          if        Not over
          UNpack    S2NLoNIN,str2,str4
          packkey   Nownfld,str4
          Move      S2NLoNIN,HoldOwn
          call      Nownkey
          else                            .its over
          Clear     Holdown
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
                              IF        (Mode = "M" & srdsstatus <> "W")
                              alert     note,"Cannot Update a Datacard Without matched LO info!!!",result
                              goto      EojError                   
                              else
                              if        (srdsstatus <> "W")
                              call      NoXref
                              endif
                              Return
                              endif
                    
                    endif
          endif
.         
          packkey   S2NFld,SRDSLSTNUM
          call      S2NKey
          if        over
                    IF        (Newflag <> Yes)
                    call      NOxref
                    Return
                    Elseif    (NewFlag = Yes)         .Manual Mode and want to create new Card
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
                              Move      LstNum,HoldLIst
                              MOVe      LstNum,S2NNIN
                              Move      SRDSLSTNUM,S2NSRDS
                              call      S2NWRt
                              goto      NewCard
                              Endif
                    endif
          Endif
.
          if        (S2NStatus = "*")            .special pricing do not touch the card
                    IF        (Mode = "M")
                    alert     note,"Datacard marked as special pricing - not updating!!!",result
                    endif
          return
          endif
.                                       .We think we are golden
          packkey   NDatfld,S2NNIN
          Move      S2NNIN,HoldLIst       .save list number
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
          
          if        (holdown <> "")        if we have an owner number -- IF not do not update the owner info
                    if        (holdown <> ownnum)           ....Manager/Owner changed save the info
                    move      ownnum,NDatoldOwn
                    endif
.move current LO to card
                    clear     Ownnum
                    pack      OwnNum,"00",OWNLON
                    rep       zfill,ownnum
          endif
........
.check date don't update if we are newer
          Clear     yy
          Clear     mm
          Clear     dd
          move      c0,juldays
          unpack    SRDSNDATUPDDATE into str2,yy,mm,dd            .date srds touched
.          unpack    SRDSREVDATE into str2,yy,mm,dd
          call      cvtJul
          move      JUldays,TestDate

          Clear     yy
          Clear     mm
          Clear     dd
          unpack    Revdate from  str2,yy,mm,dd                       .REVISION DATE CCYYMMDD FORMAT
          call      cvtJul
......................................................................................
.skipping right now
.          call      Debug
......................................................................................

.if previously updated by SRDS & SRDS data is newer Update
          if        ((Password = "SRDS       " or Password = "MIN        ") & TestDate > Juldays)        
          goto      Newcard
          Else
.         call      cvtJul
.IF Batch mode we have a match and NIN card is + 6 mos old Update  --- testing
.                   if        (Mode = "B" & TestDate-JUldays > 180)
                    if        (Mode = "B" & TestDate > JUldays)
.....>>>>>>>>>>>>>>>>>>>>>>>>>>>TEmporary for testing
.                    if        (Mode = "B" & TestDate >= JUldays)
.....>>>>>>>>>>>>>>>>>>>>>>>>>>>TEmporary for testing
                    goto      Newcard
                    Elseif    (Mode = "M" & TestDate < Juldays)
                              unpack    SRDSNDATUPDDATE into str2,yy,mm,dd            .date srds touched

                              pack      str255 from "SRDS update thru Date = ",mm,"/",dd,"/",str2,yy," Update Anyway? ",CRLF:
                              "Yes = Update, No= No Update, Cancel= No Update"                 
                              ALERT    PLAIN,str255,RESULT
                              IF       (RESULT = 1)
                                        ALERT    NOTE,"YES was pressed.",RESULT
                              ELSEIF   (RESULT = 2)
                                        ALERT    NOTE,"NO was pressed.",RESULT
                                        return
                              ELSEIF  (RESULT = 3)
                                        ALERT   NOTE,"CANCEL was pressed.",RESULT
                                        return
                              ENDIF

.                    alert     note,"SRDS Card Older than NIN Datacard did not process!!!",result
.                    return
                    Elseif    (Mode = "M" & TestDate >= Juldays)
                    goto      Newcard
                    else
                    return
                    endif
.         if        (TestDate <= Juldays)
..        
.                             IF        (Mode = "M")        
.                             alert     note,"SRDS Card Older than NIN Datacard did not process!!!",result
.                             endif
.         return
.                   endif
          endif
NewCard
          call      Trim using Mlstname
.
.begin patch 1.5
.          type      srdsrevdate
          type      SRDSNDATUPDDATE
          if        equal
.          Move      SRDSREVDATE,REVDATE                     .Revised date            
          Move      SRDSNDATUPDDATE,REVDATE                     .Revised date   
.end patch 1.5
          else
          type      srdsrevdate
          if        equal
                    Move      SRDSREVDATE,REVDATE                     .if Revised date missing do we have counts thru date?
                    else
                    clear     revdate
                    endif
          endif
          MOve      "SRDS",Password
          clock     Timestamp,timestamp 
.         call      debug
          unpack    timestamp,NdatupdDate              .todays date 
          MOve      "SRDS",NDatUpdINit
          
          if        (SRDSCOMMPER <> "" & SRDSCOMMPER <> "      ")
          Move      SRDSCOMMPER,CommPer
          endif
.

.
.
.Need to add lots of checking here  selects addressing etc is buried in here
.lets start saving
          clock     TimeStamp,NDatBStamp
          call      NDatBWrt                     .write to backup
.
          Call      Del2ndary                .cleanup slt & add files
            Call        CheckSlct                 .selects, arrangement, etc
            call    Checktxt1
.         
.Unit of sale
          clear     UNITDATA
          reset     UNITDATA
.
GEnder
          Move      SrdsStatus,Status
.Media = our source -- need to parse out % match our code etc - sigh


.Net info  
          clear     Netname
          Clear     Netinfo
          Clear     str55               
          call      trim using SRDSNETINFO
          if        (SRDSNETINFO <> "")
          move      "N002",NetName
          Move      SRDSNETINFO,str55
          IF        (SRDSNETINFO = "" )
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

          Endif

.Begin patch 1.2
          call      trim using SRDSUNIVERSE
          move      c0,n10
          move      SRDSUNIVERSE,n10
          if        (n10 > c0)
          MOve      SRDSUNIVERSE,UNiverse
          endif
.          MOve      SRDSUNIVERSE,UNiverse
.end patch 1.2


.Selects / Segments
          Clear     BaseFlag
          MOve      c1,N4
          MOve      n4,NSelnum
          rep       Zfill,NSelNum
          PackKey     NSELFLD,ndatfld,Nselnum
          move      "S.Delete-NSELTST",Location
          pack      KeyLocation,"Key: ",NSELFLD
          PackKey     NSELFLD1,"01X",NDatfld
          Clear     Nselfld2
          Clear     Nselfld3
          Move      c0,SelBlankCnt
.          call      Nseltst
          if        (srdslstnum = "930260")
          call      Debug        
          endif

          Loop
            Clear       NSelList
          move      "S.Delete-NSELAim",Location
          pack      KeyLocation,"Key: ",NSELFLD1
          call      NselAim
          Until     Over
.          call      NSELKey
.          if        over
.          add       c1,SelBlankCnt
.          endif
.          Until     (Nselnum = "9999" or SelBlankcnt = 10)
.          Until     (NSELLIST <> ndatfld)
.                   Until     over
.Now, Delete Select Record
          call      debug
          move    "S.Delete-NSELDEL",Location
          pack    KeyLocation,"Key: ",NSELFLD1
.          call      NSELKey
.            if          Not over                  
            call    NSELDEL
.            endif
.          Add       c1,N4
.          MOve      n4,NSelnum
.          rep       Zfill,NSelNum
.          PackKey     NSELFLD,ndatfld,Nselnum
          Repeat 
..........................................................................
.
.lets to on
............        
          MOve      No,BaseFlag
          Clear     NSelBase
.reset
.          PACKKey   SRDSSELFld2,SRDSLSTNUM,"0001"
          move    "S-srdsSELkey",Location
          pack    KeyLocation,"Key: ",SrdssELFLD
          PACKKey   SRDSSELFld,SRDSLSTNUM
          REp       Zfill,SRDSSELFLD
          move      c0 to Holdn4
          move      c1,SRDSSELpath
          call      srdsselkey
          call      ProcSel
          Loop
.          call      SRDSSELKEy
          call      SRDSSELKs
          Until     over
          if        (Srdssellist <> SRDSLSTNUM)
          break
          endif
          call      ProcSel

          REpeat
          
          call      Textout


.SRDS List Owner COntact info
          Pack      NDatCntPHN from SRDSLOPhone.SRDSLOPHONE1,SRDSLOPhone.SRDSLOPHONE2,SRDSLOPhone.SRDSLOPHONE3
          Pack      NDatCntfax from SRDSLOFax.SRDSLOFax1,SRDSLOFax.SRDSLOFax2,SRDSLOFax.SRDSLOFax3
          MOve      LstNum,NDatCntNum
          move      SRDScntct,NDatCnt
          move      SRDScEmail,NDatCntEML
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

          call      Trim using SRDSMLSTNAME
          Packkey   Ndatfld2,"02L",SRDSMLSTNAME
          Clear     MlstName
          Clear     str1
          clear     LStNUm
          clear     Elstcde
          call      NdatAim
          If        Not OVer
                    If        (elstcde = "C" or elstcde = "P")
                    MOVE      "C",str1
                    endif
          endif               
          
          if        (mode  = "B")
          Write     NoMatch,seq;*cdfon,SRDSLSTNUM,Lstnum,SRDSMLSTNAME,Mlstname,str1,SRDSOWNNUM,SRDSCOMP,str5,str25,ownnum,ownocpy,SRDSSTATUS
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
          WRite     Match,seq;*cdfon,SRDSLSTNUM,HoldLIst,SRDSMLSTNAME,Mlstname,b1,SRDSOWNNUM,SRDSCOMP,str5,str25:
                    ownnum,ownocpy,SRDSSTATUS
          add       c1,MCnt             
          endif
......
.gender info
.         call      Debug
          Move      c0,NDatfem
          Move      c0,NDatMen
.         
.if old record had mlr x ref etc keep it
.Need to reposition File pointer in case another process threw it off
                    If        (HoldOwn <> Ownnum & Holdown <> "")                 
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
                    MOve      SRDSMLSTname,Hold2
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
          
          WRite     Exclusive,seq;*cdfon,SRDSLSTNUM,HoldLIst,SRDSMLSTNAME,Mlstname,Elstcde,SRDSOWNNUM,SRDSCOMP,str5,str25,ownnum,ownocpy,SRDSSTATUS
          add       c1,Ecnt
          endif
          Return
...............................................................................
TextOut
.Not Nec as we get no text from SRDS.
.          reset     Hold2
.          call    Trim using hold2
.                    if (hold2 <> "")
.          move    "501",result
.          movelptr hold2,howmany
.          pack    NTXTLIST,LSTNUM
.          for N2,C1,"15"
.                  move    hold2,NTXTTEXT
.                  pack    NTXTNUM,N2
.                  rep         zfill,Ntxtnum
.                  pack    NTXTFLD,NTXTLIST,NTXTNUM
.                  rep         zfill,NtxtFld
.                  move    "Verify-NTXTWRT",Location
.                  pack    KeyLocation,"Key: ",NTXTFLD
.                  call    NTXTWRT
.                  if (result > howmany)
.                          break 
.                  endif 
.
.                  reset   hold2,result
.                  add     "500",result
.          repeat 
.          endif 
.          Clear Hold2
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
                    move      "Verify-NARRWRT",Location
          call      NArrBWRt
                    move      "Verify-NArrDel",Location
          call      NArrDel

          repeat
................................................................      
.slct
.begin patch xxx   old records not being deleted
.          packkey   NSltfld,Ndatfld
          for       N3 FROM "1" TO "999" USING "1"
slctloop  packkey   NSltfld,Ndatfld,N3
          rep       Zfill,Nsltfld
          call      NSltkey
          if        not over
.          Break     if over
.          move    "Verify-NSLtTst",Location
.          call      NSlttst
.          goto      slctexit if over
.          loop      
.          move    "Verify-NSltKS",Location
.          call      NSltks
.          Until     over
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
.....cleanup duplicate records
          goto      slctloop
.....cleanup duplicate records
          endif
          repeat
slctexit
................................................................      
.src      
          packkey   NSrcfld,Ndatfld
          move    "Verify-NSrcTst",Location
          call      NSrctst
          goto      srcexit if over
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
srcexit
          return
................................................................................
.looking for additional pricing 
CheckTxt1
          if        (srdslstnum = "930260")
          call      Debug        
          endif
          packkey   Ntxt1fld,Ndatfld
          rep       zfill,Ntxt1fld
          move    "Verify-Ntxt1Tst",Location

          FOR           n2,"1","99" using c1

          call      Ntxt1tst
          if        not over
          move    "Verify-Ntxt1del",Location
          call      Ntxt1del
          endif
          REPEAT


          MOVE      C1,N2
          FOR       n2,"1","15" using c1

.          packkey   SRDStxtfld1,Ndatfld,n2
          packkey   SRDStxtfld1,Srdslstnum,n2
          REP       zFILL,SRDStxtfld1
          move      c2,srdstxtpath
          move    "Verify-STRSTst",Location
          call      SRDSTXTKEY
          if        not over
          move      Ndatfld,NTXT1LIST
          rep       zfill,ntxt1List
          move      n2,NTXT1NUM
          rep       zfill,ntxt1num
          move      SRDSTXT,NTXT1Text
..............
          move    "Verify-Ntxt1wrt",Location
          call      Ntxt1wrt
          endif
.          ADD       C2,N2
          REPEAT



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
          packkey   SRDSSLTFLD from SRDSLSTNUM
          call      debug
.          call      SRDSSLTtst
          call      SRDSSLTkey
          if        over
          goto      checkSltctX
          else
                    if        (SRDSLSTNUM <> SRDSSLTLIST)
                    goto      checkSltctX
                    endif
          goto      CheckSlct1
          endif
          loop      
          move    "Verify-SRDSSLTKS",Location
          Clear     SRDSSLTLIST
          call      SRDSSLTks
          Until     over
          if        (SRDSLSTNUM <> SRDSSLTLIST)
          Break
          endif
CheckSlct1
          call      Trim using SRDSSLTNUM
          count     n1,SRDSSLTNUM
          if        (n1 <> c3)
          move      No,slctflag
.          return
          endif
          call      debug
          Packkey   NrefFld,SRDSSLTTYPE,SRDSSLTNUM
          move      c1,Nrefpath
          call      nrefkey
          if        over      
          move      No,slctflag
.          return
          else
          move      Yes,Slctflag
                    If        (NrefCode = "A")                  ..addressing
                    Move      Ndatfld,NaddList
                    move      Nrefnum,Naddnum
                    packkey   NaddFld,ndatfld,nrefnum
                    move      SRDSSLTPRICE,Naddprice
                    move      "000",nadddesc
                        Move      SRDSSLTDESC,nadddesc
                    call      NaddWrt   
                    ELseIF    (NrefCode = "L" & SRDSSLTPRICE <> 0)                  .Select
.fix it start at 1 and check
.                    move      "001",nrefnum
.fix it start at 1 and check
                    packkey   NSltFld,ndatfld,nrefnum
                    rep       zfill,nsltfld
                    Move      Ndatfld,NSltList
                    move      Nrefnum,NSltnum
                    move      SRDSSLTPRICE,NSltPrice
                    move      "000",nSLTdesc
                        Move      SRDSSLTDESC,NSLTDESC
.fix it start at 1 and check
tstslt              call      Nslttst
                    if        not over
                    move      nrefnum,n3
                    add       c1,n3
                    move      n3,nrefnum
                    rep       zfill,nrefnum
                    packkey   NSltFld,ndatfld,nrefnum
                    rep       zfill,nsltfld
                    move      Nrefnum,NSltnum
                    goto      tstslt
                    endif
.fix it start at 1 and check
                    call      NSltWrt   
                    ELseIF    (NrefCode = "S")                  .Source
                    
                    packkey   NSrcFld,ndatfld,nrefnum
                    Move      Ndatfld,NSrcList
                    move      Nrefnum,NSrcnum
                    Clear     NSrcPer                         .SRDS does not provide
                    call      NSrcWrt   
                    Else                                    .not one of those ????
                    move      No,slctFlag
.                    return
                    Endif
          Endif
          repeat
checkSltctX
          return
................................................................................
NoLoop1
          Scan      "WINERR:0x2",error
          if        equal
          Display   *p35:11,"_MAIN Not found - continuing"
          endif
....      
EOJ       
.         Close     S2Nfile
          if        (mode = "M")
          goto      EojError
          endif
          Setitem   CSVStatus(1),0,"Creating spreadsheet report"
          activate  CSVStatus(1)
.Create spreadsheet
.Open Excel application
          create  ex
.Reset Default of Worksheets found in a Workbook
          setprop ex,*SheetsInNewWorkbook=3
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
            setprop sheet.range("b3","B3"),*Value="SRDS",*HorizontalAlignment=aligncenter
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
            setprop sheet.range("g3","g3"),*Value="SRDS",*HorizontalAlignment=aligncenter
            setprop sheet.range("g3:g3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("g3:g3").Font,*Bold="True"
            setprop sheet.range("g4","g4"),*Value="Owner",*HorizontalAlignment=aligncenter
            setprop sheet.range("g4:g4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("g4:g4").Font,*Bold="True"
            setprop sheet.range("g5","g5"),*Value="Code",*HorizontalAlignment=aligncenter
            setprop sheet.range("g5:g5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("g5:g5").Font,*Bold="True"
            setprop sheet.range("H3","H3"),*Value="SRDS",*HorizontalAlignment=aligncenter
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
            setprop sheet.range("M4","M4"),*Value="SRDS",*HorizontalAlignment=aligncenter
            setprop sheet.range("M4:M4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("M4:M4").Font,*Bold="True"
            setprop sheet.range("M5","M5"),*Value="Status",*HorizontalAlignment=aligncenter
            setprop sheet.range("M5:M5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("M5:M5").Font,*Bold="True"

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
            setprop sheet.range("b3","B3"),*Value="SRDS",*HorizontalAlignment=aligncenter
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
            setprop sheet.range("E3","E3"),*Value="NIN List Name",*HorizontalAlignment=aligncenter
            setprop sheet.range("E3:E3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("E3:E3").Font,*Bold="True"
            setprop sheet.range("f3","f3"),*Value="NIN",*HorizontalAlignment=aligncenter
            setprop sheet.range("f3:f3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("f3:f3").Font,*Bold="True"
            setprop sheet.range("f4","f4"),*Value="Exclusive",*HorizontalAlignment=aligncenter
            setprop sheet.range("f4:f4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("f4:f4").Font,*Bold="True"
            setprop sheet.range("g3","g3"),*Value="SRDS",*HorizontalAlignment=aligncenter
            setprop sheet.range("g3:g3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("g3:g3").Font,*Bold="True"
            setprop sheet.range("g4","g4"),*Value="Owner",*HorizontalAlignment=aligncenter
            setprop sheet.range("g4:g4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("g4:g4").Font,*Bold="True"
            setprop sheet.range("g5","g5"),*Value="Code",*HorizontalAlignment=aligncenter
            setprop sheet.range("g5:g5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("g5:g5").Font,*Bold="True"
            setprop sheet.range("H3","H3"),*Value="SRDS",*HorizontalAlignment=aligncenter
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
            setprop sheet.range("M4","M4"),*Value="SRDS",*HorizontalAlignment=aligncenter
            setprop sheet.range("M4:M4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("M4:M4").Font,*Bold="True"
            setprop sheet.range("M5","M5"),*Value="Status",*HorizontalAlignment=aligncenter
            setprop sheet.range("M5:M5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("M5:M5").Font,*Bold="True"

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
            setprop sheet.range("b3","B3"),*Value="SRDS",*HorizontalAlignment=aligncenter
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
            setprop sheet.range("g3","g3"),*Value="SRDS",*HorizontalAlignment=aligncenter
            setprop sheet.range("g3:g3").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("g3:g3").Font,*Bold="True"
            setprop sheet.range("g4","g4"),*Value="Owner",*HorizontalAlignment=aligncenter
            setprop sheet.range("g4:g4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("g4:g4").Font,*Bold="True"
            setprop sheet.range("g5","g5"),*Value="Code",*HorizontalAlignment=aligncenter
            setprop sheet.range("g5:g5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("g5:g5").Font,*Bold="True"
            setprop sheet.range("H3","H3"),*Value="SRDS",*HorizontalAlignment=aligncenter
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
            setprop sheet.range("M4","M4"),*Value="SRDS",*HorizontalAlignment=aligncenter
            setprop sheet.range("M4:M4").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("M4:M4").Font,*Bold="True"
            setprop sheet.range("M5","M5"),*Value="Status",*HorizontalAlignment=aligncenter
            setprop sheet.range("M5:M5").Font,*Name="Times New Roman", *Size=11
            setprop sheet.range("M5:M5").Font,*Bold="True"

.Load it up
.**********************
          write     Match,Seq;*cdfon,Mcnt
          Close     Match
.**********************
          Setitem   CSVStatus(2),0,"Processing Matches"
          activate  CSVStatus(2)
          activate  CSVStatus(3)
.Begin patch 1.4
.          OPen      Match,"c:\work\match.csv",read
          OPen      Match,"\\nins1\e\data\Match.csv|nins1:502",read
.end patch 1.4

          Move      C0,Counter
          Loop                
          add       c1,counter
          move      Counter,DimCounter
          Setitem   CSVStatus(3),0,DimCounter

          Read      Match,seq;*cdfon,SRDSLSTNUM,HoldLIst,SRDSMLSTNAME,Mlstname,b1,SRDSOWNNUM,SRDSCOMP,str5,str25:
                    ownnum,ownocpy,SRDSSTATUS

          Until     over                          

                    sheets.item giving sheet using 1
                    Move      CellRowCnt,ExRow
                    call      Trim using ExRow
          Pack      Cell,"B",ExRow
          setprop sheet.range(Cell),*Value=SRDSLSTNUM
          Pack      Cell,"C",ExRow
          setprop sheet.range(Cell),*Value=HoldLIst
          Pack      Cell,"D",ExRow
          setprop sheet.range(Cell),*Value=SRDSMLSTNAME
          Pack      Cell,"E",ExRow
          setprop sheet.range(Cell),*Value=Mlstname
          Pack      Cell,"F",ExRow
          setprop sheet.range(Cell),*Value=b1
          Pack      Cell,"G",ExRow
          setprop sheet.range(Cell),*Value=SRDSOWNNUM
          Pack      Cell,"H",ExRow
          setprop sheet.range(Cell),*Value=SRDSCOMP
          Pack      Cell,"I",ExRow
          setprop sheet.range(Cell),*Value=str5
          Pack      Cell,"J",ExRow
          setprop sheet.range(Cell),*Value=str25
          Pack      Cell,"K",ExRow
          setprop sheet.range(Cell),*Value=OWNNum
          Pack      Cell,"L",ExRow
          setprop sheet.range(Cell),*Value=OWNOCPY
          Pack      Cell,"M",ExRow
          setprop sheet.range(Cell),*Value=SRDSSTATUS
          
          Add       c1 to CellRowCnt
          
          Repeat
.**********************
          write     NoMatch,Seq;*cdfon,NoCnt
.
          Close     Nomatch
.**********************
          move      c0,counter
          Setitem   CSVStatus(2),0,"Processing Non matches"
.Begin patch 1.4
.          OPen      NoMatch,"c:\work\Nomatch.csv",read
          OPen      NoMatch,"\\nins1\e\data\NoMatch.csv|nins1:502",read
.end patch 1.4

          Loop                
          add       c1,counter
          move      Counter,DimCounter
          if        (counter >= "9999")
.          call      debug
          endif
          Setitem   CSVStatus(3),0,DimCounter
          REad      NoMatch,seq;*cdfon,SRDSLSTNUM,Lstnum,SRDSMLSTNAME,Mlstname,str1,SRDSOWNNUM,SRDSCOMP,str5,str25,ownnum,ownocpy,SRDSSTATUS
          Until     Over

                    sheets.item giving sheet using 2
                    Move      CellRowCnt1,ExRow1
                    call      Trim using ExRow1


          Pack      Cell,"B",ExRow1
          setprop sheet.range(Cell),*Value=SRDSLSTNUM
          Pack      Cell,"C",ExRow1
          setprop sheet.range(Cell),*Value=Lstnum
          Pack      Cell,"D",ExRow1
          setprop sheet.range(Cell),*Value=SRDSMLSTNAME
          Pack      Cell,"F",ExRow1
          setprop sheet.range(Cell),*Value=str1
          Pack      Cell,"G",ExRow1
          setprop sheet.range(Cell),*Value=SRDSOWNNUM
          Pack      Cell,"H",ExRow1
          setprop sheet.range(Cell),*Value=SRDSCOMP
          Pack      Cell,"I",ExRow1
          setprop sheet.range(Cell),*Value=str5
          Pack      Cell,"J",ExRow1
          setprop sheet.range(Cell),*Value=str25
          Pack      Cell,"K",ExRow1
          setprop sheet.range(Cell),*Value=OWNNum
          Pack      Cell,"L",ExRow1
          setprop sheet.range(Cell),*Value=OWNOCPY
          Pack      Cell,"M",ExRow
          setprop sheet.range(Cell),*Value=SRDSSTATUS
          Add       c1 to CellRowCnt1

          REpeat
.**********************

          write     Exclusive,Seq;*cdfon,Ecnt

          Close     Exclusive
.**********************
          Setitem   CSVStatus(2),0,"Processing Exclusive matches"

.Begin patch 1.4
.          OPen      Exclusive,"c:\work\Exclusive.csv",read
          OPen      Exclusive,"\\nins1\e\data\Exclusive.csv|nins1:502",read
.end patch 1.4
          Move      C0,Counter
          Loop                
          add       c1,counter
          move      Counter,DimCounter
          Setitem   CSVStatus(3),0,DimCounter

          read      Exclusive,seq;*cdfon,SRDSLSTNUM,HoldLIst,SRDSMLSTNAME,Mlstname,str1,SRDSOWNNUM,SRDSCOMP,str5,str25,ownnum,ownocpy,SRDSSTATUS

          Until     Over

            sheets.item giving sheet using 3
            Move    CellRowCnt2,ExRow
            call    Trim using ExRow
          Pack      Cell,"B",ExRow
          setprop sheet.range(Cell),*Value=SRDSLSTNUM
          Pack      Cell,"C",ExRow
          setprop sheet.range(Cell),*Value=HoldLIst
          Pack      Cell,"D",ExRow
          setprop sheet.range(Cell),*Value=SRDSMLSTNAME
          Pack      Cell,"E",ExRow
          setprop sheet.range(Cell),*Value=Mlstname
          Pack      Cell,"F",ExRow
          setprop sheet.range(Cell),*Value=str1
          Pack      Cell,"G",ExRow
          setprop sheet.range(Cell),*Value=SRDSOWNNUM
          Pack      Cell,"H",ExRow
          setprop sheet.range(Cell),*Value=SRDSCOMP
          Pack      Cell,"I",ExRow
          setprop sheet.range(Cell),*Value=str5
          Pack      Cell,"J",ExRow
          setprop sheet.range(Cell),*Value=str25

          Pack      Cell,"K",ExRow
          setprop sheet.range(Cell),*Value=OWNNum
          Pack      Cell,"L",ExRow
          setprop sheet.range(Cell),*Value=OWNOCPY
          Pack      Cell,"M",ExRow
          setprop sheet.range(Cell),*Value=SRDSSTATUS
          
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
          pack    str11,"M",Exrow1
          sheet.range(str4,str11).Columns.Autofit

            sheets.item giving sheet using 2
            Move    CellRowCnt1,ExRow1
            call    Trim using ExRow1
          pack    str4,"A3"
          pack    str11,"M",Exrow1
          sheet.range(str4,str11).Columns.Autofit

            sheets.item giving sheet using 1
            Move    CellRowCnt1,ExRow
            call    Trim using ExRow
          pack    str4,"A3"
          pack    str11,"M",Exrow
          sheet.range(str4,str11).Columns.Autofit


          Clock     Timestamp,Timestamp
              clear   taskname
              setprop ex,*DisplayAlerts=OFalse


              setprop ex,*DefaultFilePath=taskname
              bump            timestamp,8
              Clear           Taskname
              pack            Taskname,"c:\work\S2N0002_",Timestamp
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
.call email 
CheckFile
          Pause     "90"
          pack      str55 from taskname,".xlsx"                  .presuming current version of excle
          trap      WaitForEnd giving error if IO
          open      FileCheck,STR55,Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
          pack      MailAttach from str55

          CLEAR     MailTO
          Move      "CarolFrazer@nincal.com,KrsniWatkins@nincal.com",Mailto
          Move      "Creques@nincal.com",Mailfrom
          Move      "SRDS Update report",Mailsubjct
          Move      "240",MailTimer
          call      SendMail

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
.begin patch 2014 May 20  tally srds updates in batch mode
          IF        (Mode = "B")                  .Batch/Auto mode
	Move	"SRDS",Inits
.          REturn
.          endif
.          if        (inits = "   " or Inits ="")
          Elseif        (inits = "   " or Inits ="")
          clock      port to str3           .plb note
          unpack     str3 into str2,str1
          pack       str3 from str1,str2
          Packkey   Nusefld from Str3
          REP       zfill IN NUSEFLD
          CALL      NUSEKEY
          move       nuseinit to inits 
          endif
.end patch 2014 May 20  tally srds updates in batch mode
.START PATCH 1.52 ADDED LOGIC
                        clear   NTYPDET
.END PATCH 1.52 ADDED LOGIC
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
.START PATCH 1.52 ADDED LOGIC
                        clear   NTYPDET
.END PATCH 1.52 ADDED LOGIC
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
ProcSel   MOve      SRDSSELNUM,SRDSSELNUM                            .Seq #
.          call      debug

          Move      Ndatfld,NSELLIST
            Rep         Zfill,NDatFld
          MOVe      c0,NselPrice
          if        (SRDSSELPRICE > 0)
          Move      SRDSSELPRICE,NSelPRice
          endif
          call      Trim using SRDSSELSName
          Move      SRDSSELSNAME,NSELSNAME                      Name
          move      c0,NSelQty
          call      Trim using SRDSSELQTY
.begin patch 1.2
          move      c0,n10       
.end patch 1.2
            Move        SRDSSELQTY,n10  
          if        (n10 > 0)
          Move      SRDSSELQTY,NSelQty
          call     Zfillit using Nselqty
          endif
.begin patch 1.2
          call      debug
          if        (SRDSSELPRICE = c0 & n10 = c0 & SRDSSELSNAME = "Universe")
          return
          endif
.end patch 1.2

          Clear     NSelExc
          Clear     NSelBase
          MOVe      c9,NSELINDEX

          IF        (NSelprice = C0)
          Move      "009",NSELDESC
          endif
          scan      "Inquire",SRDSSELSNAME
          IF        Equal
          Move      "009",NSELDESC
          endif
          move      SRDSSELEXC,NSELEXC
          reset     SRDSSELSNAME
          Scan      "Exch/",SRDSSELSNAME               .exch/rent
          If        Equal
          MOVe      c1,NSELEXC
          MoveFptr  SRDSSELSNAME,n3                   .save formpointer
          sub       c1,n3                
          setlptr   SRDSSELSNAME,n3              .set length pointer to form pointer less 1
          reset     SRDSSELSNAME                    .reset formpointer
          Clear     NselName
          append    SRDSSELSNAME,NSelName                      .move the name less "Exch/"
          reset     NSelName
          Else
          Reset     SRDSSELSNAME
          Scan      "Exch",SRDSSELSNAME
                    If        Equal
                    MOVe      c2,NSELEXC            exchange only
                    MoveFptr  SRDSSELSNAME,n3                   .save formpointer
                    sub       c1,n3                
                    setlptr   SRDSSELSNAME,n3              .set length pointer to form pointer less 1
                    reset     SRDSSELSNAME                    .reset formpointer
                    Clear     NselName
                    append    SRDSSELSNAME,NSelName                      .move the name less "Exch/"
                    reset     NSelName
                    endif
          endif
.
          CLear     NSelDESC             .price modifier
          Move      "001",NSELDESC
          if        (SRDSSELDESC = "M")
          Move      "001",NSELDESC
          Elseif    (SRDSSELDESC = "F")
          MOVe      "002",NSELDESC
          endif
          MOve      Holdbase,n5
            move      SRDSSELNUM,n6       
            move      SRDSSELNUM,NSelnum
          if        (n6 = C1)
          move      "BASE",NSelBase
          MOve      Yes,BaseFlag
          Else
          move      "SEC.",NSelBase
          endif
SelTST
          MOve      NSelNum,NSelIndex
            pack              NSELFLD,NSELLIST,NSELNUM
              Rep       ZFill,NSelfld
            move              "S.SaveA-NSELTST",Location
            pack              KeyLocation,"Key: ",NSELFLD
            call              NSELTST
            if      over
            Goto    WRTSel
            else
.we have a problem
            move    Nselnum,n4
          add       c1,n4
          move      n4,NselNum
          goto      SelTst
            endif 
.add code if n4 > 99  big trouble       
WRtSEl    
          Call      NSelWRt
          MOve      SRDSSELNUM,N4                            .Seq #
          add       c1,HoldN4
          PACKKey   SRDSSELFld2,SRDSLSTNUM,Holdn4
          Rep       Zfill,SRDSSELFld2
          Return
.................................................................................................
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
                   if        (trapcount > 60)   . 5 min are you kidding me
.                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Orders - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "CReques@nincal.com",MailTO
.                   Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail

                    shutdown
                    stop

                    endif
          
                    goto      checkfile

........................................................................................................................
          include   srdsio.inc
          include   srdsLOio.inc
          include   SRDSSELio.inc                             .Selects(segments) with counts
          include   SRDSSLtio.inc                             .selects,addressing, etc
          Include   S2Nio.inc
          Include   S2NLoio.inc
          Include   Ndatio.inc
          Include   Nownio.inc
.          Include   NTXTIO.inc                            .SRDS does not supply text
          Include   NADDIO.inc
          include   NArrIO.inc 
          include   NsrcIO.inc
          Include   NModio.inc                 .not touched yet
          include         NSELIO.inc
               Include        NSltio.inc
               include        Nrefio.inc
          include   NdatUio.inc
               include         gnxtio.inc
          Include   NDatCntio.inc
          Include   NCatio.inc
          include   Nuseio.inc
          Include   Ntypio.inc
. backup files
          Include   ndatBIO.inc
.          Include   nTxtBIO.inc                            .SRDS does not supply text
          include   NAddBIo.inc
          include   NArrBIO.inc
          include   NSltBIO.inc
          include   NSrcBIO.inc
          Include   NModBIO.inc                 .not touched yet
.begin patch 1.1
          include   SRDSTXTio.inc                             
          Include   NTXT1io.inc
.end patch 1.1
.......................................................

          INclude   Comlogic.inc