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
          include   srdsdd.inc
          include   srdsLOdd.inc
          include   SRDSSELdd.inc                             .Selects(segments) with counts
          include   SRDSSLtdd.inc                             .selects,addressing, etc
          inCLUDE   S2NDD.inc
          inCLUDE   S2NLODD.inc
          include         NSELdd.inc
          Include   NSltdd.inc
          include   Nrefdd.inc
          Include   Npasdd.inc
          INclude   Gnxtdd.inc
. backup files
          Include   ndatBdd.inc
          Include   nTxtBdd.inc
          Include   nAddBdd.inc
          include   NArrBdd.inc
          include   NSltBdd.inc
          include   NSrcBdd.inc
          Include   NModBDD.inc                 .not touched yet
.begin patch 1.1          
          include   srdsTxtdd.inc
.begin patch 1.1          
.......................................................
.
Release   INit      "1.2"     DLH           .check for bad date
REldate   Init      "2014 March 13"
.Release   INit      "1.1"     DLH           .add srdstxt --- special pricing info
.REldate   Init      "21 September 2011"
.Release   INit      "1.0"     DLH           
.REldate   Init      "xx xxx 2011"
hold2         dim             4500           .length of largest possible text record
STR46     Dim       46
OverFLow  Dim       1
Count     Form      6
B50       Init      "                                              "
B45       Init      "                                         "
BaseFlag  Dim       1
SlctFlag  Dim       1
crnl       init      0177           .causes line feed cr in edittext
Carr      init      0x7f                .edittext carraige return line feed
Cyan      Color
colordim dim        8
arcode    dim       3
newOwn   form    4
...................................................................................
CaseChange external "SPELLCHECK;CaseChange"
...................................................................................

.THis program allows you to match SRDS 2 NIN records and update list information from Converted SRDS List system CSV format live files
.First release kicks out "printed" reports only no update.
.
HMList    Dim       6         Hold SRDS List #

ManuaLUpdate external "S2N0002;ManualUpdate"

.x             plform          NDAT0001
.NDAT001E           plform               NDAT001E
mss1      plform  Error
pass        plform          Passwrd
X         plform               S2N0003
          formload  X
            formload pass

.
ListEdit  EditTExt
          Create    Cyan=*Cyan
          Create    ListEdit=3:4:2:77,BORDER:
                    STYLE=3DON
          Activate  LIstEdit            
          Setprop   ListEdit,Visible=0

.SRDS
          S2n0003ListView001.InsertColumn using "Key",50,1
          S2n0003ListView001.InsertColumn using "Name",400,2
          S2n0003ListView001.InsertColumn using "NIN ##",50,3
          S2n0003ListView001.InsertColumnFgClr using *Index=4

.NIN
          S2n0003ListView002.InsertColumn using "Key",50,1
          S2n0003ListView002.InsertColumn using "Name",400,2
          S2n0003ListView002.InsertColumn using "SRDS ##",50,3
          S2n0003ListView002.InsertColumnFgClr using *Index=4
.SRDS owner search   
          S2n0003ListView003.InsertCOlumn Using "Key",50,1
          S2n0003ListView003.InsertCOlumn Using "Company",200,2
          S2n0003ListView003.InsertCOlumn Using "Contact",200,3
.Nin owner search   

          S2n0003ListView004.InsertCOlumn Using "Key",50,1
          S2n0003ListView004.InsertCOlumn Using "Company",200,2
          S2n0003ListView004.InsertCOlumn Using "Contact",200,3

          Move      No,Passflag
          SetFocus  S2n0003EditText003
.
           EVENTREG  X, 17, XRESIZE

              loop
               waitevent
.                setitem timer,0,18000   .reset to 30 SRDSutes
              repeat
LoadSRDSListView
              S2n0003ListView001.InsertItem giving N9 using SRDSLSTNUM
              S2n0003ListView001.SetItemText using N9,SRDSMLSTNAME,1
              S2n0003ListView001.SetItemText using N9,S2NNIN,2
              call  Trim using S2NNIN    
.         call      debug
          if        (S2NNIN <> "")

          move      "0xFF0000",colordim           .Red
          S2n0003ListView001.SetItemText USING N9,colordim,4
          endif
          S2n0003ListView001.Getitemcount giving Count
          Clear     str25
          pack      Str25,Count," - Found."
          Setitem   S2n0003StatText039,0,str25
          REturn
UpdSRDSListView      
.first find it
          call      debug
          S2n0003ListView001.Finditem Giving N9 Using *Start=seq,*Text=SRDSLSTNUM
          IF        (n9 <> -1)
               S2n0003ListView001.SetItemText using N9,S2NNIN,2
.begin patch 1.2               
          Else
          call      LoadSRDSListView
.end patch 1.2               
          endif
          return    

LoadNINListView
              S2n0003ListView002.InsertItem giving N9 using Lstnum
              S2n0003ListView002.SetItemText using N9,MLstName,1
              S2n0003ListView002.SetItemText using N9,S2nSRDS,2
               call Trim using S2nSRDS
          if        (S2nSRDS <> "")
          move      "0xFF0000",colordim           .Red
          S2n0003ListView002.SetItemText USING N9,colordim,4
          endif
          S2n0003ListView002.Getitemcount giving Count
          Clear     str25
          pack      Str25,Count," - Found."
          Setitem   S2n0003StatText040,0,str25
          
          REturn
UpdNinListView      
.first find it
          call      debug
          S2n0003ListView002.Finditem Giving N9 Using *Start=seq,*Text=LSTnum
          IF        (n9 <> -1)
               S2n0003ListView002.SetItemText using N9,S2nSRDS,2
          Else
          call      LoadNinListView
          endif
          return    
          
LoadSRDSDetail

          S2n0003ListView001.GetItemText giving STR6 using REsult
          packkey   SRDSfld,Str6
          call      SRDSkey
          packkey   SRDSLOfld,SRDSOWNNUM
          call      SRDSLOkey
          setitem   S2n0003EditText003,0,SRDSMLSTNAME
          setitem   S2n0003StatText002,0,SRDSLSTNUM
          Setitem   S2n0003StatText016,0,SRDSUNIVERSE
          Setitem   S2n0003StatText003,0,SRDSOWNNUM
.begin patch 1.2
.          unpack    SRDSREVDATE,str2,YY,MM,DD
	  type       SRDSREVDATE
	  if		equal
          unpack    SRDSREVDATE,str2,YY,MM,DD
          else
          unpack    SRDSNDATUPDDATE,str2,YY,MM,DD
          endif
.end patch 1.2
          clear     str10
          pack      Str10,MM,"/",dd,"/",str2,yy   
          setitem   S2n0003StatTExt020,0,str10
          
          
          pack      SRDSONUM,SRDSOWNNUM
          rep       Zfill,SRDSONUM
          call      LoadSRDSLoDetail
          call      LoadSRDSBase                .we have no text so load base selects
.lets check for x-ref
          packkey   S2nfld,SRDSLSTNUM
          call      S2nkey
          if        not over
          move      S2NNIN,Lstnum
          call      loadNInDetail
          else
.         call      ClearNInDetail
          endif
          REturn
LoadSRDSLODetail
          Packkey   SRDSLOfld,SRDSONUM
          call      SRDSLOKey
          If        Not over
          Setitem   S2n0003StatText003,0,SRDSONUM
          SetItem   S2n0003StatText008,0,SRDSCOMP
          SetItem   S2n0003StatText013,0,SRDScntct
          SetItem   S2n0003StatText009,0,SRDSADDR
          SetItem   S2n0003StatText026,0,SRDSCITY
          SetItem   S2n0003StatText027,0,SRDSSTATE
          SetItem   S2n0003StatText028,0,SRDSZIP
          Setitem   S2n0003StatText003,0,SRDSONUM



          clear     str13
          pack      str13,"(",SRDSLOPhone.SRDSLOPHONE1,")",SRDSLOPhone.SRDSLOPHONE2,dash,SRDSLOPhone.SRDSLOPHONE3
          Setitem   S2n0003StatText035,0,str13
          clear     str13
          pack      str13,"(",SRDSLOFax.SRDSLOFAX1,")",SRDSLOFax.SRDSLOFAX2,dash,SRDSLOFax.SRDSLOFAX3
          Setitem   S2n0003StatText036,0,str13
.lets check for x-ref
          packkey   S2nLOfld,c0,SRDSONUM
          move      c1,S2nLopath
          call      S2nLokey
                    if        not over
                    Unpack    S2nLoNIN,str2,str4
                    Setitem   S2n0003StatText024,0,str4
                    else
                    Setitem   S2n0003StatText024,0," "
                    endif
          Else
          Setitem   S2n0003StatText024,0," "
          SetItem   S2n0003StatText008,0," "
          SetItem   S2n0003StatText009,0," "
          SetItem   S2n0003StatText013,0," "
          SetItem   S2n0003StatText026,0," "
          SetItem   S2n0003StatText027,0," "
          SetItem   S2n0003StatText028,0," "
          SetItem   S2n0003StatText035,0," "
          SetItem   S2n0003StatText036,0," "
          Setitem   S2n0003StatText003,0," "
          clear     SRDSCOMP
          endif
          return
LOADSRDSbASE
          Clear     Hold2
          packkey       SRDSSELFld,SRDSLSTNUM
          Move          SRDSLSTNUM,str6
          move           "Read-SRDSSELKey",Location
          pack           KeyLocation,"Key: ",SRDSSELFLD
          call      SRDSSELKey
          if        Not over
                    Append    SRDSSELQTY,hold2
                    append    b1,hold2
                    call      trim using SRDSSELSNAME
                    append    SRDSSELSNAME,Hold2
                    append    SRDSSELPRICE,hold2
                    append    Carr,hold2
          endif          
          loop
                move           "Read-SRDSSELKs",Location
                pack           KeyLocation,"Key: ",SRDSSELFLD
                CALL      SRDSSELKS
                until     OVer
                if  (str6=SRDSSELLIST)
                    Append    SRDSSELQTY,hold2
                    append    b1,hold2
                    call      trim using SRDSSELSNAME
                    append    SRDSSELSNAME,Hold2
                    append    SRDSSELPRICE,hold2
                    append    Carr,hold2
                  Else
                  Break
                  endif
                Repeat
.begin patch 1.1          
          clear     srdstxt
          move      c2,srdstxtpath
         append    Carr,hold2
          for N2,C1,"5"
                    pack      srdstxtFLD1,SRDSLSTNUM,N2
                    rep       zfill,srdstxtfld1
                    move      "D.Load-srdstxtKEY",Location
                    pack      KeyLocation,"Key: ",srdstxtFLD1
                    call      debug
                    call      srdstxtKEY
                    until     over
                    append    srdstxt,hold2
          repeat                  

.begin patch 1.1          
          Reset     Hold2

          Setitem   S2N0003EditText001,0,hold2
          RETURN
          
LoadNINDetail
          packkey   Ndatfld,Lstnum
          call      NdatKey
          call      Trim using MLstname
          Unpack    Ownnum,str2,str4
          Call      LoadNINLODetail
          SetItem   S2n0003StatText018,0,Universe
          Setitem   S2n0003EditText004,0,MLstname
          Setitem   S2n0003StatText005,0,Lstnum
          clear     str2
          clear     yy
          clear     dd
          clear     mm
          clear     str10
          unpack    REVDATE,str2,YY,MM,DD
          pack      Str10,MM,"/",dd,"/",str2,yy   
          setitem   S2n0003StatTExt012,0,str10
          setitem         S2n0003EditText002,0,""         .Initialize object
.          clear           NTXTTEXT3
          clear           hold2
                    call      debug
              for N2,C1,"15"
               pack           NTXTFLD,LSTNUM,N2
               rep            Zfill,NTxtFLd
               move           "D.Load-NTXTKEY",Location
               pack           KeyLocation,"Key: ",NTXTFLD
               call           NTXTKEY
               until over
               append         NTXTTEXT,hold2
              repeat
              if (hold2 <> "")
               reset          hold2
               call           Trim using hold2
              endif
              setitem         S2n0003EditText002,0,hold2
         
          Return
LoadNINLoDetail
          packkey   Nownfld,str4
          Setitem   S2n0003StatText029,0," "
          call      Nownkey
          if        Over
          Setitem   S2n0003StatText006,0," "
          Setitem   S2n0003StatText007,0," "
          SetItem   S2n0003StatText014,0," "
                    SetItem   S2n0003StatText015,0," "
          SetItem   S2n0003StatText031,0," "
          SetItem   S2n0003StatText032,0," "
          SetItem   S2n0003StatText033,0," "
          Setitem   S2n0003StatText029,0," "
          Setitem   S2n0003StatText037,0," "
          Setitem   S2n0003StatText038,0," "
          Setitem   S2n0003StatText030,0," "
          Else
          Setitem   S2n0003StatText006,0,OWNLON
          Setitem   S2n0003StatText007,0,OWNLONM
          SetItem   S2n0003StatText014,0,OWNOCPY
                    SetItem   S2n0003StatText015,0,OWNLOSA 
          SetItem   S2n0003StatText031,0,OWNLOCTY
          SetItem   S2n0003StatText032,0,OWNLOS 
          SetItem   S2n0003StatText033,0,OWNLOZC 
          unpack    OWNRDTE,mm,dd,str4
          pack      str10,mm,"/",dd,"/",str4
          clear     str13
          UNpack    OWNTELE,ARcode,str3,str5
          pack      str13,"(",Arcode,")",str3,dash,str5
          Setitem   S2n0003StatText037,0,str13
          clear     str13
          UNpack    OWNfax,ARcode,str3,str5
          pack      str13,"(",Arcode,")",str3,dash,str5
          Setitem   S2n0003StatText038,0,str13
          
          
.lets check for x-ref
          packkey   S2nLofld2,c0,c0,ownlon
          move      c2,S2nLopath
          call      S2nLokey
                    if        not over
                              Setitem   S2n0003StatText029,0,S2nSRDS
                    endif
          
          endif
          return
          
ClearNINDetail
          SetItem   S2n0003StatText014,0,""
          SetItem   S2n0003StatText015,0,""
          SetItem   S2n0003StatText018,0,""
          Setitem   S2n0003EditText004,0,""
          Setitem   S2n0003StatText005,0,""
          Setitem   S2n0003StatText012,0,""
          
          Return
.........................................................................       
.Create New NIN LO
CreateNINLO
          clock     date to str8
          unpack    str8,MM,STR1,DD,STR1,YY
          pack      OwnRDTE,MM,DD,CC,YY
          Move      "M",OwnLOc
          Clear     Ownblk
          MOve      SRDSCOMP,OwnoCpy        .Name
          MOve      SRDSADDR,ownlosa       .Address 1
          MOve      SRDSCITY,ownlocty        .City
          MOve      SRDSSTATE,ownlos
          MOve      SRDSZIP,Ownlozc
          MOve      SRDScntct,OwnLonm        .Contact
          move      c0,ownnec

          pack      OwnTele From      SRDSLOPHONE.SRDSLOPHONE1,SRDSLOPHONE.SRDSLOPHONE2,SRDSLOPHONE.SRDSLOPHONE3

          pack      OwnFax From      SRDSLOFax.SRDSLOFax1,SRDSLOFax.SRDSLOFax2,SRDSLOFax.SRDSLOFax3
          Move      SRDScEmail,OwnEmail
          Clear     OWnstat
          Clear     Ownctn
          Clear     OwnGally
          Clear     OwnTaxid
          Clear     OwnFax2
          Clear     OwnTranFlag
          Move      Npasuser,OWNPASS
          call      GetNewLo

          Call      NownWrt
          
          Return
GetNewLo
.Locate and Display next logical Owner Number
        move    "NOWNNXT",GNXTFLD
        call    GNXTKEY
        if over
.Change StatText Boxes For Error Message
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
                setitem ErrorMssgStat5,0,"  Error Getting Owner ID Number!"
                setitem ErrorMssgOK,0,"&Stop"
.Display Error Message
                setprop ErrorMssg,visible=1
.Reset StatText Boxes
                call    SetS2n3ErrorMssgDefault
.TerSRDSate Subroutine
                return
        endif

        bump    GNXTNUM,2
        move    GNXTNUM,newOwn
        loop
                add     C1,newOwn
                move    newOwn,ownlon
                rep     zfill,ownlon
                pack    NOwnFLD,ownlon,"0000"
                rep     zfill,NOwnFLD
                call    NOwnTST
                until over
        repeat

          move      newOwn,str4
          rep       zfill,str4
          clear     GNXTNUM   
          append    B2,GNXTNUM
          append    newOwn,GNXTNUM
          reset     GNXTNUM
          rep       zfill,GNXTNUM
          call      GNXTUPD
        
        return
.........................................................................       
UpdateNINLO
.not yet implemented
          Return
.........................................................................       
SetS2n3ErrorMssgDefault
.Set Default 
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=1
        setprop ErrorMssgStat4,visible=1
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0,"To Search By :"
        setitem ErrorMssgStat2,0,"Enter "
        setitem ErrorMssgStat3,0,"To Search By Company Name:"
        setitem ErrorMssgStat4,0,"Enter Search Name"
        setitem ErrorMssgStat5,0,"      That Record Does Not Exist!"
        setitem ErrorMssgOK,0,"&OK"
        return
.........................................................................       
EOJ
          Stop
XRESIZE
           S2N0003.Scale
           RETURN

          include   srdsio.inc
          include   srdsLOio.inc
          include   SRDSSELio.inc                             .Selects(segments) with counts
          include   SRDSSLtio.inc                             .selects,addressing, etc
          inCLUDE   S2NLOio.inc
          inCLUDE   S2NIO.inc

          Include   Ndatio.inc
          Include   Nownio.inc
          Include   NTXTIO.inc
          Include   NADDIO.inc
          include   NArrIO.inc 
          include   NsrcIO.inc
          Include   NModio.inc                 .not touched yet
          include   NSELIO.inc
          Include   NSltio.inc
          include   Nrefio.inc
          Include   Npasio.inc
          INclude   GnxtIO.inc
. backup files
          Include   ndatBIO.inc
          Include   nTxtBIO.inc
          include   NAddBIo.inc
          include   NArrBIO.inc
          include   NSltBIO.inc
          include   NSrcBIO.inc
          Include   NModBIO.inc                 .not touched yet
.......................................................
.begin patch 1.1          
          include   srdsTxtio.inc
.begin patch 1.1          

          INclude   Comlogic.inc