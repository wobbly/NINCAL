.NDAT0022 - DATACARD S.B. BILLING MAINT/INQUIRY
.........
PC       EQU       1
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NDAT3DD.INC
         INCLUDE   NPASDD.INC
         INCLUDE   NDATDD.INC
         include   nowndd.inc
         include   consacct.inc
              include         nrtndd.inc
              include         ncmpdd.inc
.START PATCH 1.5 ADDED LOGIC
.START PATCH 1.67 REPLACED LOGIC
.         include   nfuldd.inc
          include compdd.inc
          include cntdd.inc
.END PATCH 1.67 REPLACED LOGIC
.END PATCH 1.5 ADDED LOGIC

RELEASE   INIT         "2.04"   DLH      .Minimum added
Reldate   Init      "2015 June 5"
.RELEASE   INIT         "2.03"   DLH      additional code to avoid blank records
.Reldate   Init      "2013 December 19"
.RELEASE   INIT         "2.02"   DLH      misc cleanup
.Reldate   Init      "2013 September 17"
.RELEASE   INIT         "2.01"   DLH      started adding verify for adds
.Reldate   Init      "22 April 2008"
.RELEASE            INIT         "2.00"   DLH      24Jan08  GUI interface
.Reldate  Init      "25January2008"
.RELEASE  INIT         "1.72"   DLH      04Jan08  Rates Field for PLI
.                                       and massive cleanup
.RELEASE  INIT         "1.71"   DLH      13Feb07  Add MKTG
.RELEASE  INIT         "1.70"       JD   11OCT2006 FULFILLMENT CONVERSION/Datacard
.RELEASE  INIT         "1.69"      DMS   21JUN2006 FULFILLMENT CONVERSION
.RELEASE  INIT      "1.68"            DLH    31Aug2006 Add Frontline
.RELEASE  INIT      "1.67"            DLH    11Aug2006 Work Order 1102- Add Pidi MMI
.RELEASE  INIT      "1.66"            ASH   02MAR2005 Work Order 615 - Display Date fields
.RELEASE  INIT      "1.65"            JD   02Jul2004 new xmgtrate.
.RELEASE  INIT      "1.6"            ASH          28JAN2004 DATACARD CONVERSION
.RELEASE  INIT      "1.5"            ASH          04FEB2002 NINFUL CONVERSION
.RELEASE  INIT      "1.4"            dlh 30SEP99 Add code to check OWNCTN at update.
.RELEASE  INIT      "1.3"            dlh 28MAY96 CHECK FOR OVER ON LISTHELP
.release  init      "1.2"            DLH add dollar/date charge codes
.RELEASE  INIT      "R1.1"           JD  05AUG93 added password includes,keyin.
.RELEASE  INIT      "R1.0"          DLH 09MAR92.
.begin patch 2.0

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
WSW      DIM       1
KEYCOUNT FORM      1
AKEY1    INIT      "   "
AKEY2    INIT      " 2L"
QUES     INIT      "??????????"
mode     dim       1
AddMode   Form      1         1=add, 2 = update
FoundCount          Form      5
.............................
.Set Vars used for About Box
          move    "NDAT0022.PLS",Wprognme
          move    "Additional LM Billing",Wfunction
          move    "David Herrick",Wauthor
          move    Release,Wrelease
          move    Reldate,Wreldate

.Declare forms, Always declare child forms first
srch      plform  Search
mss1      plform  Error
pss       plform  Passwrd
abt       plform  About
x         plform  Ndat0022
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
          create  NDat0022;mFile,FData
          create  NDat0022;mEdit,EData,mFile
          create  NDat0022;mOptions,OData,mEdit
          create  NDat0022;mHelp,HData,mOptions

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
.

        call    SetDat22ErrorMssgDefault

          MOVE      C1 TO NDATPATH
          
          move    "I",progcode
          move    "N",PassFlag
.......................................................................................................................
.show list of current properties in file
          ndat0022ListView001.InsertColumn using "List ##",75,0   
          ndat0022ListView001.InsertColumn using "List Name",500,1 
           Setprop   ndat0022ListView001,AutoRedraw=0
          Setitem   ndat0022StatText006,0,"Loading Records"
          Loop
          call      Ndat3ks
          Until     over
          packkey   Ndatfld,Ndat3key
          call      Ndatkey
          ndat0022ListView001.InsertItem giving result using Ndat3key
          ndat0022ListView001.SetItemText using result,MLSTNAME,1 // col2
          repeat
          ndat0022ListView001.Getitemcount giving FoundCount
          Clear     str25
          pack      Str25,FoundCount," - Found."
          Setitem   ndat0022StatText006,0,str25
           Setprop   ndat0022ListView001,AutoRedraw=1
          
          call      debug
Main
        setfocus ndat0022EditText001
        loop
                waitevent
                setitem timer,0,18000   .reset to 30 minutes
        repeat

         APPEND    "I",NPASFLD
         APPEND    PASSWORD,NPASFLD
         RESET     NPASFLD
         CALL      NPASKEY
         GOTO      GOOD IF NOT OVER
NOGOOD   DISPLAY   *B,*P17:11,"Password NOT VALID... Only Inquiry Allowed",*w2;
         move      "I" TO MODE
         goto       keylst
GOOD     DISPLAY   *P17:12,"Password ACCEPTED... Add and Modify Allowed.",*w2;
.
KEYLST   
.         KEYIN     *p1:08,*ES,*P10:08,"LIST NUMBER: ",*ZF,*JR,NDATFLD
          Getitem   ndat0022EditText001,0,str6
.          call      Trim using str6
          rep       Zfill,str6
          packkey   ndatfld,str6
          SCAN      "*" IN NDATFLD
          STOP      IF EQUAL
          IF        EOS
          alert   caution,"Sorry, List number is required. Use f2 to search.",result
          setfocus  ndat0022EditText001,1

          ENDIF
          RESET     NDATFLD
          CALL      NDATKEY
          MOVE      NDATFLD TO NDAT3FLD
          Setitem   ndat0022StatTextListName,0,Mlstname
          MOVE      C2 TO N1
          REP       ZFILL IN NDAT3FLD
          CALL      NDAT3KEY
          IF        OVER
          alert   caution,"Sorry, Not found. LET'S ADD IT.",result
          MOVE      C1 TO N1
          MOVE      C1 TO AddMode
          setfocus  ndat0022ComboBoxBcode                   
          return
          Else
          MOVE      C2 TO AddMode

          endif


          unpack    ownnum,str2,str4
          move      str4,nownfld
          move      b1 to ndat3cde

                    if (DATFUL <> "")
                              packKEy   COMPFLD,DATFUL
                              rep       zfill,COMPFLD
                              move           C1,COMPPATH
                              move      "Verify-COMPKEY",Location
                              pack      KeyLocation,COMPFLD
                              call      COMPKEY
                              if over
                              clear   compcomp
                              else
                                        if (COMPSVBFLG <> "T")
                              clear   compcomp
                                        endif
                              endif
                    else      // datful = ""
                    clear   compcomp
                    endif
.                   
          If        (COMPNUM = "009406")
          move      b1,ndat3cde
          move      c1,result
          
          Elseif    (COMPNUM = "009383")
          move      "A",ndat3cde
          move      c2,result

          Elseif    (COMPNUM = "009831")
          move      "F",ndat3cde
          move      c3,result
          
          Elseif    (COMPNUM = "009384")
          move      "J",ndat3cde
          move      c4,result
.
          ElseIf    (COMPNUM = "004907")
          move      "K",ndat3cde
          move      c5,result

          Elseif    (COMPNUM = "009428")
          move      "M",ndat3cde
          move      c6,result

          Elseif    (COMPNUM = "009387" or COMPNUM = "009414" or COMPNUM = "009429")
          move      "P",ndat3cde
          move      c7,result
.         
          Elseif    (COMPNUM = "009410")
          move      "R",ndat3cde
          move      c8,result
          endif
          setitem   ndat0022ComboBox001,0,result
          
          unpack    ownnum,str2,str4
          move      str4,ndat3own

          if        (NdatTdmc = "" or NdatTdmc = b1)
          move      c1,result
          Elseif    (NdatTdmc = "B")
          move      c2,result
          Elseif    (NdatTdmc = "R")
          move      c3,result
          Elseif    (NdatTdmc = "E")
          move      c4,result
          endif
          Setitem   ndat0022ComboBoxBcode,0,result

          if        (Ndatdolc = yes)
          setitem   ndat0022Check002,0,c1
          else
          setitem   ndat0022Check002,0,c0
          endif
          setitem   ndat0022StatText003,0,CompCOmp
          
          if        (Ndat3exh = yes)
          setitem   ndat0022Check001,0,c1
          else
          setitem   ndat0022Check001,0,c0
          endif

          clear     str10
          pack      str10,"$",Ndat3exrt,"/m"
          Setitem   ndat0022EditText003,0,str10
.begin patch 2.04
          clear     str10
          if          (nDat3Min = c0)
          pack      str10,"$15.00"
          else
          pack      str10,"$",nDat3Min
          endif
          Setitem   ndat0022EditText006,0,str10
         
.end patch 2.04
          clear     str10
          call      Trim using NDAT3EX1
          if (NDAT3EX1 <> "")
                    count     result,NDAT3EX1
                    if (result = 8)
                              unpack    NDAT3EX1,MM,DD,str4
                              pack      str10,MM,SLASH,DD,SLASH,str4
                    endif
          endif
          Setitem   ndat0022EditText004,0,str10
.
          clear     str10
          call      Trim using NDAT3EX2
          if (NDAT3EX2 <> "")
                    count     result,NDAT3EX2
                    if (result = 8)
                              unpack    NDAT3EX2,MM,DD,str4
                              pack      str10,MM,SLASH,DD,SLASH,str4
                    endif
          endif
          Setitem   ndat0022EditText005,0,str10
          return
          
BCODE    KEYIN     *P10:12,"BILLING CODE ",*RV,NDATTDMC
cCODE    keyin     *P10:14,"Dollar/Date Chrg ",*rv,NDATdolc
xCode    keyin     *p10:18,"Exchange Management Fee?",*rv,ndat3exh
         cmatch    no to ndat3exh
         if        equal
         keyin     *p10:20,"Date Stopped: mm/dd/ccyy ",*zf,*jr,mm:
                   "/",*zf,*jr,dd,"/",*zf,*jr,cc,*zf,*jr,yy
         pack      ndat3ex2 from mm,dd,cc,yy
         type      ndat3ex2
         if        not equal
         display   *p10:20,*el,"DATE ",ndat3ex2," Is not valid!!!!!",*b,*b,*w2
         goto      xcode
         endif
         goto      ok
         endif
         cmatch    yes to ndat3exh
         if        equal
         keyin    *p10:20,"Date Started: mm/dd/ccyy ",*zf,*jr,mm:
                   "/",*zf,*jr,dd,"/",*zf,*jr,cc,*zf,*jr,yy
         pack      ndat3ex1 from mm,dd,cc,yy
         clear     ndat3ex2
Rate      Keyin     *p52:20,"$",*RV,Ndat3ExRt,"/m"
          Display   *p52:20,"$",Ndat3ExRt,"/m"
          if        (ndat3exh = "Y" & Ndat3ExRt <= c0)
          Display   *p10:24,*el,"Price is Required!!!!!!!!!",*b,*w2
          goto      Rate
          endif
         goto      ok
         endif
         goto      Xcode
OK       KEYIN     *P10:24,*EL,"EVERYTHING OK ? ",STR1
         CMATCH    NO TO STR1
         IF        EQUAL
         CALL      PAINT
         GOTO      KEYLST
         ENDIF
         BRANCH    N1 OF ADD,UPD
         STOP
ADD      
          MOVE      LSTNUM TO NDAT3KEY
         CALL      NDAT3WRT
         CALL      PAINT
         GOTO      KEYLST
VerifyData
          Getitem   ndat0022ComboBox001,0,result
          If        (Result = c1)
          move      b1,ndat3cde
          
          Elseif    (Result = c2)
          move      "A",ndat3cde

          Elseif    (Result = c3)
          move      "F",ndat3cde
          
          Elseif    (Result = c4)
          move      "J",ndat3cde
.
          ElseIf    (Result = c5)
          move      "K",ndat3cde

          Elseif    (Result = c6)
          move      "M",ndat3cde

          Elseif    (Result = c7)
          move      "P",ndat3cde
.         
          Elseif    (Result = c8)
          move      "R",ndat3cde
          endif

          Getitem   ndat0022ComboBoxBcode,0,result

          If        (Result = c1)
          Move      B1,NdatTdmc
          Elseif    (Result = c2)
          Move      "B",NdatTdmc
          Elseif    (Result = c3)
          Move      "R",NdatTdmc
          Elseif    (Result = c4)
          Move      "E",NdatTdmc
          endif

          Getitem   ndat0022Check001,0,result
          if        (result = c1)
          Move      Yes,Ndat3exh
          Else
          Move      No,Ndat3exh
          endif
          

          Getitem   ndat0022Check002,0,result
          if        (result = c1)
          Move      Yes,Ndatdolc
          Else
          Move      b1,Ndatdolc
          endif
          
          Getitem   ndat0022EditText003,0,str10
          move      "$",str1
          call      removechar Using str10,str1
          move      "/",str1
          call      removechar Using str10,str1
          move      "m",str1
          call      removechar Using str10,str1
          call      Trim using str10
          MOve      str10,Ndat3exrt
          if        (ndat3exh = "Y" & Ndat3ExRt <= c0)
          Alert     caution,"PRICE is required!!! ",result
          setfocus    ndat0022EditText003
          endif
.begin patch 2.04
          Getitem   ndat0022EditText006,0,str10
          move      "$",str1
          call      removechar Using str10,str1
          call      Trim using str10
          MOve      str10,Ndat3Min
          if        (ndat3exh = "Y" & Ndat3ExRt <= c0)
          Alert     caution,"Minimum is required!!! ",result
          setfocus    ndat0022EditText006
          endif

.end patch 2.04
          Getitem   ndat0022EditText004,0,str10
          move      "/",str1
          call      removechar Using str10,str1
          call      Trim using str10
          move      str10,NDAT3EX1

          Getitem   ndat0022EditText005,0,str10
          move      "/",str1
          call      removechar Using str10,str1
          call      Trim using str10
          move      str10,NDAT3EX2
.Begin patch 2.03
          rep       zfill,Lstnum
          count     n1,lstnum
          if        (n1 <> 6)
          alert   caution,"Sorry, List number is required. Use f2 to search.",result
          setfocus  ndat0022EditText001,1
          return
          endif
.end patch 2.03

          IF        (ADDMODE = C1)
          Packkey   Ndat3key from lstnum
          CALL                NDAT3WRT
          packkey   Ndatfld,Ndat3key
          call      Ndatkey
          ndat0022ListView001.InsertItem giving result using Ndat3key
          ndat0022ListView001.SetItemText using result,MLSTNAME,1 // col2
          ndat0022ListView001.Getitemcount giving FoundCount
          Clear     str25
          pack      Str25,FoundCount," - Found."
          Setitem   ndat0022StatText006,0,str25

          eLSE
          CALL      NDAT3uPD
          endif                                   
          Return
UPD
          unpack    ownnum,str2,str4
          move      str4,nownfld

         move      c1 to nownpath
          call      debug
                    if (DATFUL <> "")
                              packKey   COMPFLD,DATFUL
                              rep       zfill,COMPFLD
                              move           C1,COMPPATH
                              move      "Verify-COMPKEY",Location
                              pack      KeyLocation,COMPFLD
                              call      COMPKEY
                              if over
                              else
                                        if (COMPSVBFLG <> "T")
                                        endif
                              endif
                    else      // datful = ""
                    endif

          if        (COMPNUM = "009831")
          move      "F",ndat3cde
          
          Elseif    (COMPNUM = "009406")
          move      b1,ndat3cde
          
          Elseif    (COMPNUM = "009384")
          move      "J",ndat3cde
.
          Elseif    (COMPNUM = "009383")
          move      "A",ndat3cde

          Elseif    (COMPNUM = "009387" or COMPNUM = "009414" or COMPNUM = "009429")
          move      "P",ndat3cde
.         
          Elseif    (COMPNUM = "009428")
          move      "M",ndat3cde

          Elseif    (COMPNUM = "009410")
          move      "R",ndat3cde

          ElseIf    (COMPNUM = "004907")
          move      "K",ndat3cde
          endif

          unpack    ownnum,str2,str4
          move      str4,ndat3own
           CALL      NDAT3UPD
         CALL      PAINT
         GOTO      KEYLST
EOJ
         STOP
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
          return
SearchLoad2
.LIST
        unpack srchstr,str6,str1,str35,str1,str10
        if (str6 = "")
                  setitem ndat0022EditText001,0,str6
                  setfocus ndat0022EditText001
        else
                  setitem ndat0022EditText001,0,str6
                  move        str6,ndatfld
                  move        str6,ndat3Key
                  Setitem     ndat0022StatTextListName,0,Mlstname
                  setfocus ndat0022ComboBoxBcode
        endif
        return
SearchLoad3
.MAILER
          return
SearchLoad4
.SHIP-TO
          Return
SearchLoad5
.CAMPAIGN
          return
SearchLoad6
.OWNER
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
          goto      Filego3
          branch result to FileGo1,FileGo2,FileGo2
FileGo1
                    return
FileGo2
                    winshow
FileGo3
                    stop
          return
SetDat22ErrorMssgDefault
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

         INCLUDE   NDAT3IO.INC
         INCLUDE   NDATIO.INC
         include   nownio.inc
              include         nrtnio.inc
              include         ncmpio.inc
          include   searchio.inc
         INCLUDE   NPASIO.INC
          include compio.inc
          include cntio.inc
         INCLUDE   COMLOGIC.INC

