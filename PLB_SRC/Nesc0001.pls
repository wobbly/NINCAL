PC       EQU       0
         INCLUDE   COMMON.inc 
         INCLUDE   CONS.inc 
.;Patch1.1
                              include   compdd.inc
                              include   cntdd.inc
.begin patch 2.0   ..searchio
              include         nrtndd.inc
              include         ncmpDD.inc
              include         nPasDD.inc
.end patch 2.0              
.         INCLUDE   NMLRDD.inc
.;Patch1.1
           INCLUDE   NXRFDD.inc
           INCLUDE   NownDD.inc
.;Patch1.1
.         include   nbrkdd.inc
.;Patch1.1
           INCLUDE   NDATDD.inc
         INCLUDE   Nescdd.inc
.
............................................................................
RELEASE   INIT      "2.05"   DLH      Added building of list view
Reldate   Init      "2016 May 11"
.RELEASE   INIT      "2.04"   DLH      Added update status display
.Reldate   Init      "2016 February 24"
.RELEASE   INIT      "2.03"   DLH      
.Reldate   Init      "14 September 2010"
.RELEASE   INIT      "2.02"   DLH       add additional type
.Reldate   Init      "3 June 2009"
.RELEASE   INIT      "2.01."   DLH       add type
.Reldate   Init      "20 April 2009"
.RELEASE            INIT         "2.00"   DLH        GUI interface
.Reldate  Init      "13 February 2008"
.release  init      "1.2"        ASH    12JAN2005 Escrow Conversion
.release  init      "1.1"        DMB    26MAY2004 Mailer Conversion 
.release  init       "1.00"             09sep96 New
.begin patch 2.0
revtyps   init      "DH GS "            ALLOWED REVISION ?

Timer   Timer
.Colors
white   color
grey    color
RED     COLOR
BLACK   COLOR

.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font

.Set Up Menu Bar
mFile    menu
mEdit    menu
mHelp    menu
mOptions menu
.Set Up SubMenu for Options
sColor  submenu

.Present Data for Menu Bar
FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Color"
HData   init    "&Help;&About"

.Present Data for Colors SubMenu
CData   init    ";&Background;&Text"

.Define Collections for Object Colors
ColText Collection
ColBack Collection

.Define Colors for Each Object
FTC     color
BGC     color

colornum dim   9(2)
Fred     form    3
Fgreen   form    3
Fblue    form    3
colorflag form   1
CCField       Dim           2             .hold Century
NewDate1      DIM          10
CURINDEX FORM            5
SORTCOL     FORM         2
SORTORDR   INTEGER       1   ;0,3=None: 1=Ascending: 2=Descending
Dmask      dim        10
.............................
.Set Vars used for About Box

.         MOVE      "Nesc0001" TO PROGRAM
.         MOVE      "Escrow  MAINTENANCE" TO STITLE
         MOVE      "Names in the News" TO COMPNME
.         CALL      PAINT
          move    "Nesc0001",Wprognme
          move    "Escrow  MAINTENANCE",Wfunction
          move    "David Herrick",Wauthor
          move    Release,Wrelease
          move    Reldate,Wreldate

.Declare forms, Always declare child forms first
srch      plform  Search
mss1      plform  Error
pss       plform  Passwrd
abt       plform  About
x         plform  NEsc0001
          winhide
.Load Forms, Always load parent form first
          formload x
          formload abt
          formload pss
          formload mss1
          formload srch

          CREATE  TIMER,18000     .30 minutes
          ACTIVATE TIMER,Timeout,RESULT
.Create Menus
          create  NEsc0001;mFile,FData
          create  NEsc0001;mEdit,EData,mFile
          create  NEsc0001;mOptions,OData,mEdit
          create  NEsc0001;mHelp,HData,mOptions

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
.Only a SubMenu under these
        activate mOptions
        activate mHelp,HelpGo,result

.Create Colors for EditText Inquiry
          create  white=*white
          create    grey=220:220:220
          create  RED=*RED
          create  black=*black

.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic
.end patch 2.0
...............................................................................................................
         CLOCK     DATE TO Today
         move      c1 to nmlrpath
         move      c1 to nbrkpath
         move      c1 to nownpath
         move      c1 to ndatpath
.
        call    SetESc01ErrorMssgDefault

          MOVE      C1 TO NDATPATH
          
          move    "I",progcode
          move    "N",PassFlag
.......................................................................................................................
.show list of current properties in file
          NEsc0001ListView001.InsertColumn using "Owner/List ##",0,0   
          NEsc0001ListView001.InsertColumn using "List ##",75,1   
          NEsc0001ListView001.InsertColumn using "List Name",450,2
          NEsc0001ListView001.InsertColumn using "Mailer Name",250,3
          NEsc0001ListView001.InsertColumn using "Owner Name",250,4
          NEsc0001ListView001.InsertColumn using "Consultant/Broker",250,5
          NEsc0001ListView001.InsertColumn using "Start Date",100,6
          NEsc0001ListView001.InsertColumn using "Stop Date",100,7

.begin patch 2.03
          Goto      Main
ListLoad
.1st make sure empty
          NEsc0001ListView001.DeleteAllItems
          Move      c0,NEscflag                   .force file to be reopened
          Move      c0,NEscflg2                   .force file to be reopened
           move       c3,nesclock
.end patch 2.03
           SetProp    NEsc0001ListView001,AutoRedraw=0
          Loop
          call      NESCks
          Until     over
          packkey   Ndatfld,NESClist
          call      Ndatkey
.         Call      Trim using NESCOWN
          UNpack    Nescown into str2,str4
.         packkey   Nownfld,NESCOWN
          packkey   Nownfld,str4
          call      NOwnKey

          Clear     Compcomp
          packkey   COMPfld,NESCMLR
          call      CompKey
.         pack      str12 from NescOwn,"  ",Nesclist               .will  have to be adjusted when owner is converted
          pack      str12 from "00",OWNLON,Nesclist               .will  have to be adjusted when owner is converted
          rep       Zfill,STR12
          call      UpdateListView001
          repeat
           SetProp    NEsc0001ListView001,AutoRedraw=1

           move       c1,nesclock
          call      debug
.begin patch 2.03
          return
.end patch 2.03
Main
           EVENTREG  X, 17, XRESIZE



        setfocus NEsc0001EditText001
        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat
...clicked list view object lets display details
LoadDetails
          setitem    Nesc0001StatText007,0,b1
          scan      INITS,revtyps
          if        equal
          setprop   Nesc0001ButtonDEL,Enabled=1
          setprop   Nesc0001ButtonDEL,Visible=1
          else
          setprop   Nesc0001ButtonDEL,Enabled=0
          setprop   Nesc0001ButtonDEL,Visible=0
          endif
          
          packkey   Nescfld2 from str12
          move      c2,nescpath
          call      nesckey
          packkey   Ndatfld,NESClist
          call      Ndatkey
          UNpack    Nescown into str2,str4
.         packkey   Nownfld,NESCOWN
          packkey   Nownfld,str4
          call      NOwnKey
          Clear     Compcomp
          packkey   COMPfld,NESCMLr
          call      CompKey

          Setitem   nesc0001EditTextMLR,0,NescMLR
          Setitem   nesc0001StatTextMLR,0,CompComp

          if        (NEscBdate <> "")
          unpack    NEscBdate into str4,mm,dd
          pack      str10 from MM,"/",dd,"/",str4
          else
          clear str10
          endif
          Setitem   nesc0001EditText004,0,str10

          if        (NEscSdate <> "")
          unpack    NEscSdate into str4,mm,dd
          pack      str10 from MM,"/",dd,"/",str4
          else
          clear str10
          endif
          Setitem   nesc0001EditText005,0,str10
          Setitem   nesc0001EditTextown,0,Nescown
          Setitem   nesc0001StatTextOwn,0,Ownocpy
          Setitem   nesc0001EditTextbrk,0,Nescbrk

          if        (NescBRK <> "" & NESCBrk <> "000000")
          packkey   COMPfld,NESCBRK
          call      CompKey
          else
          Clear     Compcomp
          endif
          Setitem   nesc0001StatTextbrk,0,CompComp
          sETITEM   nesc0001EditText001,0,nESCLIST
          Setitem   nesc0001StatTextListName,0,Mlstname
.begin patch 2.1
          call      debug
          Move      NescType,n1
          add       c1,n1                         .offset
          Setitem   Nesc0001ComboBox001,0,N1
.end patch 2.1
          
          return
VerifyEscData       
          Clear     Nesclist
          GETITEM   nesc0001EditText001,0,nESCLIST
          packkey   Ndatfld,NESClist
          call      Ndatkey
          if        over      
          alert   caution,"Sorry, List is required. ",result
          setfocus    nesc0001EditText001 
          return
          endif

          Clear     Nescown
          GETITEM   nesc0001EditTextOwn,0,nESCOwn
          UNpack    Nescown into str2,str4
.         packkey   Nownfld,NESCOWN
          packkey   Nownfld,str4
          call      Nownkey
          if        over      
          alert   caution,"Sorry, Owner is required. ",result
          setfocus    nesc0001EditTextOwn 
          return
          endif

          Clear     Nescmlr
          GETITEM   nesc0001EditTextMLr,0,nESCMLR
          packkey   Compfld,NESCMLR
          call      Compkey
          if        over      
          alert   caution,"Sorry, Mailer is required. ",result
          setfocus    nesc0001EditTextMLR
          return
          endif

          Clear     Nescbrk
          GETITEM   nesc0001EditTextbrk,0,nESCbrk
          if        (Nescbrk <> "" & Nescbrk <> "000000")
          packkey   Compfld,NESCbrk
          call      Compkey
                    if        over      
                    alert   caution,"Sorry, Valid Broker or None is required. ",result
                    setfocus    nesc0001EditTextBRK
                    return
                    endif
          Else
          move      "000000",Nescbrk
          endif
.begin patch 2.1
          move      c0,nescType
          getitem   Nesc0001ComboBox001,0,n1
.begin patch 2.02
          if        (n1 <= 4)
.end patch 2.02
          sub       c1,n1                  .offset
          Move      N1,NescType
          endif
.end patch 2.1
          Clear     NescBdate
          getitem nesc0001EditText004,0,str10
          call    RemoveChar using str10,slash
          call    TRIM using str10
          MOVE    STR10 TO STR8
.          if (str10="")
.                    alert     caution,"Order Date Cannot be a null value!",result,"Bad Date"
.                    setfocus nesc0001EditText004
.                    
.                    CLEAREVENT
.                    return
.          endif
          CALL      ZFILLIT USING STR8
.          if (str8="00000000")
.                    alert     caution,"Order Date Cannot be a null value!",result,"Bad Date"
.                    setfocus nesc0001EditText004
.                    
.                    CLEAREVENT
.                    return
.          endif
          count   N2,str10
          if (N2 = 0)
                    clear   MM
                    clear   DD
                    clear   CCField
                    clear   YY
                    alert   caution,"Start Date Must be in MMDDCCYY Format",result
                    setfocus nesc0001EditText004
                    ClearEvent
                    
                    Return
          else
                    if (N2 = 10)
                              unpack  str10,MM,str1,DD,str1,CCField,YY
                    elseif (N2 = 8)
                              unpack  str10,MM,DD,CCField,YY
                    elseif (N2 <> 0)
                              alert   caution,"Start Date Must be in MMDDCCYY Format",result
                              setfocus nesc0001EditText004
                              ClearEvent
                              Return
                    endif
                    move    MM,N2
                    if (N2 > "12")
                              alert   caution,"Invalid Month!",result
                              setfocus nesc0001EditText004
                              ClearEvent
                              
                              Return
                    else
                              move    DD,N2
                              if (N2 > "31")
                                        alert   caution,"Invalid Day!",result
                                        setfocus nesc0001EditText004
                                        ClearEvent
                                        
                                        Return
                              else
                                        move    CCField,N2
                                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                                  alert   caution,"Invalid Year!",result
                                                  setfocus nesc0001EditText004
                                                  ClearEvent
                                                  
                                                  Return
                                        elseif (N2 = "19")
                                                  move    YY,N2
                                                  if (N2 < "80")
                                                            alert   caution,"Invalid Year!",result
                                                            setfocus nesc0001EditText004
                                                            ClearEvent
                                                            
                                                            Return
                                                  endif
                                        endif
                              endif
                    endif
          endif
          call    TRIM using MM
          count   N2,MM
          if (N2 <> 0 AND MM <> "00")
                    pack    NewDate1,MM,SLASH,DD,SLASH,CCField,YY
          else
                    clear   Newdate1
          endif
          setitem nesc0001EditText004,0,NewDate1
          pack    NescBdate From CCField,YY,MM,DD
..         
          Clear     NescSdate
          clear     str8
          clear     str10
          move      c0,n2
          clear     mm
          clear     dd
          clear     yy
          getitem nesc0001EditText005,0,str10
          call    RemoveChar using str10,slash
          call    TRIM using str10
          MOVE    STR10 TO STR8
.          if (str10="")
.                    alert     caution,"Order Date Cannot be a null value!",result,"Bad Date"
.                    setfocus nesc0001EditText005
.                    
.                    CLEAREVENT
.                    return
.          endif
          CALL      ZFILLIT USING STR8
.          if (str8="00000000")
.                    alert     caution,"Order Date Cannot be a null value!",result,"Bad Date"
.                    setfocus nesc0001EditText005
.                    
.                    CLEAREVENT
.                    return
.          endif
          count   N2,str10
          if (N2 = 0)
                    clear   MM
                    clear   DD
.                    clear   CCField
                    clear   YY
.                    alert   caution,"Stop Date Must be in MMDDCCYY Format",result
.                    setfocus nesc0001EditText005
.                    ClearEvent
                    
.                    Return
          else
                    if (N2 = 10)
                              unpack  str10,MM,str1,DD,str1,CCField,YY
                    elseif (N2 = 8)
                              unpack  str10,MM,DD,CCField,YY
                    elseif (N2 <> 0)
                              alert   caution,"Stop Date Must be in MMDDCCYY Format",result
                              setfocus nesc0001EditText005
                              ClearEvent
                              Return
                    endif
                    move    MM,N2
                    if (N2 > "12")
                              alert   caution,"Invalid Month!",result
                              setfocus nesc0001EditText005
                              ClearEvent
                              
                              Return
                    else
                              move    DD,N2
                              if (N2 > "31")
                                        alert   caution,"Invalid Day!",result
                                        setfocus nesc0001EditText005
                                        ClearEvent
                                        
                                        Return
                              else
                                        move    CCField,N2
                                        if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
                                                  alert   caution,"Invalid Year!",result
                                                  setfocus nesc0001EditText005
                                                  ClearEvent
                                                  
                                                  Return
                                        elseif (N2 = "19")
                                                  move    YY,N2
                                                  if (N2 < "80")
                                                            alert   caution,"Invalid Year!",result
                                                            setfocus nesc0001EditText005
                                                            ClearEvent
                                                            
                                                            Return
                                                  endif
                                        endif
                              endif
                    endif
          endif
          call    TRIM using MM
          count   N2,MM
          if (N2 <> 0 AND MM <> "00")
                    pack    NewDate1,MM,SLASH,DD,SLASH,CCField,YY
                    pack    NescSdate From CCField,YY,MM,DD
          else
                    clear   Newdate1
                    Clear     NEscSdate
          endif
          setitem nesc0001EditText005,0,NewDate1
          Clear     Nescfld2
          packkey   Nescfld2,nescown,nesclist
          Move      C2,NescPath
          call      Nesctst
          if        over
          setitem    Nesc0001StatText007,0,"Writing"
          call      Nescwrt
          setitem    Nesc0001StatText007,0,"Written"
          call      UpdateListVIew001
          else
          setitem    Nesc0001StatText007,0,"Updating"
          call      Nescupd
          setitem    Nesc0001StatText007,0,"Updated"
.          alert   caution,"All ready in the file!",result
          endif
.                   alert   caution,"Sorry, Not active yet. ",result
          return
          
UpdateListView001
           move     compcomp,mcomp
          if        (NescBRK <> "" & NESCBrk <> "000000")
          packkey   COMPfld,NESCBRK
          call      CompKey
          else
          Clear     Compcomp
          endif
          if        (NEscBdate <> "")
          unpack    NEscBdate into str4,mm,dd
          pack      str10 from MM,"/",dd,"/",str4
          else
          clear str10
          endif
          NEsc0001ListView001.SetItemText using result,str10,6               // col7
          if        (NEscSdate <> "")
          unpack    NEscSdate into str4,mm,dd
          pack      str11 from MM,"/",dd,"/",str4
          else
          clear str11
          endif
.new way
.begin patch 2.05
             MOVE     "9999",CurIndex
             NEsc0001ListView001.InsertItemEX Using str12,curIndex:
                                 *SubItem1=NEscList:
                                 *SubItem2=MLSTNAME:
                                 *SubItem3=MCOmp:
                                 *SubItem4=OWNOCPY:
                                 *SubItem5=CompCOmp:
                                 *SubItem6=str10:
                                 *SubItem7=str11
.old way
.          NEsc0001ListView001.InsertItem giving result using str12
.          NEsc0001ListView001.SetItemText Using result,NEscList,1
.          NEsc0001ListView001.SetItemText using result,MLSTNAME,2              // col3
.          NEsc0001ListView001.SetItemText using result,CompCOmp,3               // col4
.          NEsc0001ListView001.SetItemText using result,OWNOCPY,4              // col5
.          if        (NescBRK <> "" & NESCBrk <> "000000")
.          packkey   COMPfld,NESCBRK
.          call      CompKey
.          else
.          Clear     Compcomp
.          endif
.          NEsc0001ListView001.SetItemText using result,CompCOmp,5               // col6
.          if        (NEscBdate <> "")
.          unpack    NEscBdate into str4,mm,dd
.          pack      str10 from MM,"/",dd,"/",str4
.          else
.          clear str10
.          endif
.          NEsc0001ListView001.SetItemText using result,str10,6               // col7
.          if        (NEscSdate <> "")
.          unpack    NEscSdate into str4,mm,dd
.          pack      str10 from MM,"/",dd,"/",str4
.          else
.          clear str10
.          endif
.          NEsc0001ListView001.SetItemText using result,str10,7               // col8
.          setitem    Nesc0001StatText007,0,b1
.end patch 2.05
          return
kmlr
.START PATCH 1.2 REPLACED LOGIC
.         keyin     *p10:08,"Mailer number: ",str4
.         match     "000*" to str4
.         stop      if equal
.         pack      mkey from str4,z3
.         call      nmlrkey
.         keyin     *p20:08,*dv,mcomp,*dv,b1,"ok? ",str1
.........................
         keyin     *p10:08,"Mailer number: ",str6
         match     "00000*" to str6
         stop      if equal
         pack      compfld,str6
         call      COMPkey
           if over
                    goto kmlr
           elseif (COMPMLRFLG <> "T")
                    goto kmlr
           endif
         keyin     *p20:08,*dv,COMPcomp,*dv,b1,"ok? ",str1
.END PATCH 1.2 REPLACED LOGIC
         cmatch    yes to str1 
         goto      kmlr if not equal
          move      str6,NESCmlr
.         clear     nescfld
.START PATCH 1.2 REPLACED LOGIC
.         append    str4 to nescfld
.         append    str6 to nescfld
.END PATCH 1.2 REPLACED LOGIC
.
kbrk
.START PATCH 1.2 REPLACED LOGIC
.         keyin     *p10:10,"Broker number: ",*jr,*zf,str4
.         match     "000*" to str4
.         stop      if equal
.         pack      nbrkfld from str4,z3
.         call      nbrkkey
.         keyin     *p20:10,*dv,brcomp,*dv,b1,"ok? ",str1
.........................
         keyin     *p10:10,"Broker number: ",*jr,*zf,str6
         match     "00000*" to str6
         stop      if equal
          if        (str6 = "000000")
          move      str6,NESCBrk
          goto      Kown
          endif
         pack      COMPfld from str6
         call      COMPkey
           if over
                    goto kbrk
           elseif (COMPBRKFLG <> "T")
                    goto kbrk
           endif
         keyin     *p20:10,*dv,COMPcomp,*dv,b1,"ok? ",str1
.END PATCH 1.2 REPLACED LOGIC
         cmatch    yes to str1 
         goto      kbrk if not equal
.START PATCH 1.2 REPLACED LOGIC
.         append    str4 to nescfld
         append    str6 to nescfld
.END PATCH 1.2 REPLACED LOGIC
         reset     nescfld
         move      c1 to nescpath
         call      nesckey
         stop      if over
kown
         keyin     *p10:12,"Owner number: ",*jr,*zf,str4
         match     "000*" to str4
         stop      if equal
         pack      nownfld from str4
         call      nownkey
         keyin     *p20:12,*dv,ownocpy,*dv,b1,"ok? ",str1
         cmatch    yes to str1 
         goto      kown if not equal
         moVe       str4,NESCOwn

klst
         keyin     *p10:14,"List number: ",*jr,*zf,str6
         match     "00000*" to str6
         stop      if equal
         pack      ndatfld from str6
         call      ndatkey
         keyin     *p10:15,*dv,olstname,*dv,b1,"ok? ",str1
         cmatch    yes to str1 
         goto      klst if not equal
         move       str6,NESClist

ok        
         keyin     *p10:17,"ok? ",str1
          if        (str1 = yes)
         call       nescwrt
          endif
         cmatch     "*",str1
         stop           if equal
         goto       kmlr
SearchGo
          branch    result to SearchGo1,SearchGo2,SearchGo3,SearchGo4,SearchGo5,SearchGo6,Searchgo7
.END PATCH 3.66 REPLACED LOGIC
SearchGo1
.BROKER
          move      C1,SrchFlag
          call      SearchSetTitle
          call      SearchSetVisible
          return
SearchGo2
.LIST
          move      C2,SrchFlag
          call      SearchSetTitle
          call      SearchSetVisible
          return
SearchGo3 Routine
.MAILER
          move      C3,SrchFlag
          call      SearchSetTitle
          call      SearchSetVisible
          return
SearchGo4
.SHIP-TO
          move      C4,SrchFlag
          call      SearchSetTitle
          call      SearchSetVisible
          return
SearchGo5
.CAMPAIGN
          move      C5,SrchFlag
          call      SearchSetTitle
          call      SearchSetVisible
          return

SearchGo6
.OWNER
          move      C6,SrchFlag
          call      SearchSetTitle
          call      SearchSetVisible
          return
SearchGo7
.Fulfillment
          move      C7,SrchFlag
          call      SearchSetTitle
          call      SearchSetVisible
          return
SearchLoad
          branch SrchFlag     to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6,SearchLoad7
SearchLoad1
.Broker Consultant
        unpack srchstr,str6,str1,str35,str1,str10
        if (str6 = "")
                  setitem NEsc0001EditTextbrk,0,str6
                  setfocus NEsc0001EditTextbrk
        else
                  setitem NEsc0001EditTextbrk,0,str6
                  move        str6,nEscbrk
                  Setitem     NEsc0001StatTextbrk,0,compcomp
        endif
        return
SearchLoad2
.LIST
        unpack srchstr,str6,str1,str35,str1,str10
        if (str6 = "")
                  setitem NEsc0001EditText001,0,str6
                  setfocus NEsc0001EditText001
        else
                  setitem NEsc0001EditText001,0,str6
                  move        str6,ndatfld
                  move        str6,nEsclist
                  Setitem     NEsc0001StatTextListName,0,Mlstname
        endif
        return
SearchLoad3
.MAILER
        unpack srchstr,str6,str1,str35,str1,str10
        if (str6 = "")
                  setitem NEsc0001EditTextmlr,0,str6
                  setfocus NEsc0001EditTextmlr
        else
          Clear     Compcomp
          packkey   COMPfld,str6
          call      CompKey

          Setitem   nesc0001EditTextMLR,0,str6
          Setitem   nesc0001StatTextMLR,0,CompComp
        endif
          return
SearchLoad4
.SHIP-TO
          Return
SearchLoad5
.CAMPAIGN
          return
SearchLoad6
.OWNER
        unpack srchstr,str4,str1,str35,str1,str10
        if (str4 = "")
.                 setitem NEsc0001EditTextown,0,str6
.                 setfocus NEsc0001EditTextown
        else
.                 setitem NEsc0001EditTextown,0,str6
.                 move        str6,ndatfld
.                 move        str6,nEsclist
.                 Setitem     NEsc0001StatTextListName,0,Mlstname
.                 setfocus NEsc0001ComboBoxBcode
        endif
        return
SearchLoad7
        return
.Fulfillment - 
        return
......................................................
Timeout
        beep
        beep
        beep
        stop
......................................................
EditGo

colorerror
        goto Main
ColorGo
        if (result = C1)
                call    BackColor
        elseif (result = C2)
                call    TextColor
        else
                return
        endif
        clear   n1
        return
.Trap for Cancel Entry in Color System Menu
ColorTrap
        noreturn
        return
BackColor
        trap    ColorTrap if object
        create  BGC
        trapclr object
        setprop ColBack,bgcolor=BGC
        getitem BGC,1,Fred
        getitem BGC,2,Fgreen
        getitem BGC,3,Fblue
        pack    colornum(2),Fred,Fgreen,Fblue
        return

TextColor
        trap    ColorTrap if object
        create  FTC
        trapclr object
        setprop ColText,fgcolor=FTC
        getitem FTC,1,Fred
        getitem FTC,2,Fgreen
        getitem FTC,3,Fblue
        pack    colornum(1),Fred,Fgreen,Fblue
        return
        
......................................................
HelpGo
        setprop AboutMssg,visible=1
        return
......................................................

FileGo
          branch result to FileGo1,FileGo2,FileGo2
FileGo1
                    return
FileGo2
                    winshow
FileGo3
                    stop
          return
SetESC01ErrorMssgDefault
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=1
        setprop ErrorMssgStat4,visible=1
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0,"To Search By List Number:"
        setitem ErrorMssgStat2,0,"Enter 6 Digit Number"
        setitem ErrorMssgStat3,0,"To Search By List Name:"
        setitem ErrorMssgStat4,0,"Enter 'F2' "
        setitem ErrorMssgStat5,0,"      That Record Does Not Exist!"
        setitem ErrorMssgOK,0,"&OK"
        return
. Sort code.
.
.begin patch 2.05
SORTIT
           NEsc0001ListView001.SortColumn using SortCol,SORTORDR,*mask=dmask

            RETURN
.end patch 2.05


XRESIZE
           Nesc0001.Scale
           RETURN

         INCLUDE   Nescio.inc
.begin patch 2.0   ..searchio
              include         nrtnio.inc
              include         ncmpio.inc
              include         nPasio.inc
          include   searchio.inc
.end patch 2.0              
.;Patch1.1
                              include   compio.inc
                              include   cntio.inc
.         INCLUDE   NMLrio.inc
.;Patch1.1
           INCLUDE   NXRFio.inc
           INCLUDE   Nownio.inc
.;Patch1.1
.         include   nbrkio.inc
.;Patch1.1
           INCLUDE   NDATio.inc
         INCLUDE   COMLOGIC.inc
