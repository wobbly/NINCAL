PC EQU    0

          include common.inc
          include cons.inc
;         include   cntdd.inc
;         include f:\library\develop\compdd.inc
;patch1.2
                              include   compdd.inc
                              include   cntdd.inc
.         include   nbrkdd.inc
;patch1.2
release   init                "1.2"     09SEP2006 Added Service Bureau AAM Key Read and a couple of bug fixes.
.release  init                "1.1"     06JUN2004 Added search code to work with new mlrcompanies
;release  init                "1.0"     15SEP2003 Added This Routine for Company Searches


.Instance of outlook using OLE
Mes     automation      class="Outlook.Application"
.Actual email message using OLE
Note    automation
.OutLook Vars
TO DIM 45
SUBJECT DIM 45
BODY    DIM 200
.Flag as important
olFlagStatus integer 4,"0x2"
.Vars for Rt Click ListView


.Display Vars
InfoString dim  47
ScrRight form   4
ScrBottom form  4 
clickptr form  10
FarRight form   4
FarBottom form  4
T1      form    4
L1      form    4
.Base form from which I get coordinates
ParentForm          WINDOW    ^
.Where I clicked
StatTextPtr     StatText ^
.EditText to point to which object to fill with search result
EditTextPtr     EditText ^
.Type Of Search Company/Contact or Both
CCB       FORM      ^
.Looking for Mailer,Broker,etc.?
CoType    FORM      ^
.Original position for mouseclick
MouseClickPtr       FORM      ^
CompSearchString    DIM       ^
;Var to keep track of return stack
CSNUM     FORM      ^
.Var for ExitFlag
ExitVar   DIM       1
.CompanySearchVars
.str20    dim       20
ClientType          form      1
ContactType         dim       20
PrimarySearch       form      1
SecondarySearch     form      1
FreeFormSearch      form      1
ClientTypeCompare   form      1
;taskname dim       100
Count               FORM      9
ltgrey              color
white               color
b45                 DIM       45
CompSearchStr       DIM       109
CompSearchFlag      FORM      1
.

LoadSearchForm Routine
CompSrch  plform  CompSearchForm
          formload CompSrch
          create    ltgrey=*ltgray
          create    white=*white        
          RETURN








;ParentCode-Comp0001


Looper
          LOOP      
            EVENTWAIT         
          REPEAT    


CompanyContactSearch Routine  ParentForm,StatTextPtr,EditTextPtr,CCB,CoType,MouseClickPtr,CSNUM,ExitVar

          getprop   CompSearch,visible=n1
          if (n1 = c1)        
                    return
          Else
                    CompSearchListView.deleteallcontents
                    CompSearchListView.insertcolumn using "1",75,0
                    CompSearchListView.insertitem using "Company Search",1 
                    Call      showonscreen
                    Goto      Looper
          endif
CompanyContactSearchMenu Routine        CCB,CoType,CompSearchstring,CSNUM,ExitVar

          getprop   CompSearch,visible=n1
          if (n1 = c1)        
                    return
          Else
                    setprop CompSearch,winpos=2
                    CALL      COMPSEARCHVISIBLE
                    Goto      Looper
          endif
ShowOnScreen
.LOGIC in this section broken down into following generalized equation:
.
.OrderInfo_Top=(TopCoordinateOfMouseClick + TopCoordinateOfObjectWhereClickOccurred + Cushion + TopCoordinateOfProgram1Screen
.If ((OrderInfo_Top + OrderInfo_Height) > ScreenHeight)
.       OrderInfo_Top=(OrderInfo_Top - TopCoordinateOfMouseClick - Cushion
.Endif
.
.OrderInfo_Left=(LeftCoordinateOfMouseClick + LeftCoordinateOfObjectWhereClickOccurred + Cushion + LeftCoordinateOfProgram1Screen
.If ((OrderInfo_Left + OrderInfo_Width) > ScreenWidth)
.       OrderInfo_Left=(OrderInfo_Left - LeftCoordinateOfMouseClick - Cushion
.Endif
.
.
.Getinfo
.This is done each time in case the user changes their screen dimensions in the middle of
.using this program
          clear t1
          clear l1
        getprop StatTextPtr,top=T1,left=L1
          clear   str25
          getinfo system,str25
          bump    str25,12
          move    str25,str4
          move    str4,ScrRight
          bump    str25,4
          move    str25,str4
          move    str4,ScrBottom
          setprop CompSearch,winpos=1
;        getprop ParentForm,top=H,left=V
        getprop ParentForm,top=H,left=V
.Break down mouse coordinates - figured in terms of object where mouse was clicked
.clickptr established at MouseDown_Event
        move    "10000",N7
        div     N7,mouseclickptr,N9 .N9=left
        mult    N9,N7            
        sub     N7,mouseclickptr,N8 .N8=top
.Add to STATIC coordinates of object where mouse was clicked
.T1/L1 established at MouseDown_Event
        add     N9,L1           .L1=left
        add     N8,T1           .T1=top
.Calulate totals for positions
        add     T1,H
        add     "44",H          .Compensate for Menu Bar/Title Bar + some to allow second click to make invisible 
        add     L1,V
        add     "7",V           .Compensate to allow second click to make invisible
.Test to see if object will fit on page
        Getprop CompSearch,height=N9,width=N8
        add     N9,H,FarBottom
        if (FarBottom > ScrBottom)
                sub     N9,H
                sub     "20",H  .Compensate to allow second click to make invisible
        endif
        add     N8,V,FarRight
        if (FarRight > ScrRight)
                sub     N8,V
                sub     "10",V  .Compensate to allow second click to make invisible
        endif
        setprop CompSearch,top=H,left=V
          CALL      COMPSEARCHVISIBLE
        return   
FloatMenuClose Routine
        setprop CompSrch,visible=0
        setprop CompSearch,winpos=3
        return
.CompSearchRoutines
CompSearchVisible
.Seacrh type i.e. Broker Mailer Will be done before form is made vis(CCB)
.Seacrh type i.e. Company\Contact\Comp&Contact Will be done before form is made vis(CoType)
;patch1.2
          setitem CompSearchComboType,0,CoType
;patch1.2
          setitem CompSearchComboSearch1,0,c3
          setitem CompSearchComboSearch2,0,0
          setitem CompSearchEditSearch1,0,""
          setitem CompSearchEditSearch2,0,""
          setitem CompSearchCheckFreeForm,0,0
        CompSearchListView.deleteallcontents
          call      columns
          setprop CompSearch,Visible=1
          setfocus  CompSearchEditSearch1
          RETURN
Columns
        CompSearchListView.deleteallcontents
        CompSearchListView.InsertColumn using "No.",50,0
        CompSearchListView.InsertColumn using "Company",150,1
        CompSearchListView.InsertColumn using "Type",80,2
        CompSearchListView.InsertColumn using "No.",35,3
        CompSearchListView.InsertColumn using "Contact",150,4
        CompSearchListView.InsertColumn using "Type",80,5
        CompSearchListView.InsertColumn using "",0,6
          Return
CompSearchEnabled
          setprop CompSearchComboType,enabled=1,bgcolor=white
          setprop CompSearchComboSearch1,enabled=1,bgcolor=white
          setprop CompSearchComboSearch2,enabled=1,bgcolor=white
          setprop CompSearchEditSearch1,enabled=1,bgcolor=white
          setprop CompSearchEditSearch2,enabled=1,bgcolor=white
          setprop CompSearchCheckFreeForm,enabled=1
          RETURN
CompSearchDisabled
          setprop CompSearchComboType,enabled=0,bgcolor=ltgrey
          setprop CompSearchComboSearch1,enabled=0,bgcolor=ltgrey
          setprop CompSearchComboSearch2,enabled=0,bgcolor=ltgrey
          setprop CompSearchEditSearch1,enabled=0,bgcolor=ltgrey
          setprop CompSearchEditSearch2,enabled=0,bgcolor=ltgrey
          setprop CompSearchCheckFreeForm,enabled=0
          RETURN
WhatCompTypeAmI
.         Branch ClientType to Nothing,IAMCompMailer,IAMCompBroker,IAMCompListOwner,IAMCompServiceBureau,IAMCompConsultant,IAMCompManager

.Nothing
.                   clear str20
.         Return
IAMCompMailer
          If (COMPMLRFLG = "T")
                    MOVE "Mailer" to STR20
                    return
          else
                    move "" to str20
          Endif
.         Return
IAMCompBroker
          If (COMPBRKFLG = "T")
                    MOVE "Broker" to STR20
                    return
          else
                    move "" to str20
          Endif
.         Return
IAMCompListOwner
          If (COMPOWNFLG = "T")
                    MOVE "List Owner" to STR20
                    return
          else
                    move "" to str20
          Endif
.         Return
IAMCompServiceBureau
          If (COMPSVBFLG = "T")
                    MOVE "Service Bureau" to STR20
                    return
          else
                    move "" to str20
          Endif
.         Return
IAMCompConsultant
          If (COMPCLRFLG = "T")
                    MOVE "Consultant" to STR20
                    return
          else
                    move "" to str20
          Endif
.         Return
IAMCompManager
          If (COMPMNGFLG = "T")
                    MOVE "Manager" to STR20
                    return
          else
                    move "" to str20
          Endif
          RETURN
WhatContactTypeAMI
          Clear     N2
          Clear   ContactType
          move      CNCTTYPE to N2
          Load      ContactType,n2,"Mailer","Broker","List Owner","Service Bureau","Consultant","Manager"
          RETURN

ReadComp
          CompSearchListView.deleteallitems
          clear     n1
          getitem CompSearchComboType,0,n1
          move      n1 to ClientType
          sub       c1 from n1,ClientTypeCompare
          getitem CompSearchComboSearch1,0,n1
          move      n1 to PrimarySearch
          getitem CompSearchComboSearch2,0,n1
          move      n1 to SecondarySearch
          if (PrimarySearch > 0)
                    getitem CompSearchEditSearch1,0,SRCH55
                    count     n5,srch55
                    if        (n5 < 3)
                              alert caution,"This Search Requires at least three Sequential Characters in order to perform a search",result,"Company/Contact Read"
                              Call      CompSearchEnabled
                              setfocus CompSearchEditSearch1
                              return
                    else
                              getitem CompSearchEditSearch2,0,STR55
                              if (SecondarySearch > 1)
                                        if (str55 <> "")    if not blank which means all contact/clients
                                                  count     n5,str55
                                                  if (n5 < 3)
                                                            alert caution,"This Search Requires at least three Sequential Characters in order to search",result,"Company/Contact Read"
                                                            Call      CompSearchEnabled
                                                            setfocus CompSearchEditSearch2
                                                            return
                                                  endif
                                        endif
                              endif
                    endif
          else
                    alert caution,"Please select a primary search type",result,"Company/Contact Read"                                                           
                    Call      CompSearchEnabled
                    Return
          endif
          CALL      CompSearchDisabled
          getitem CompSearchCheckFreeForm,0,n1
          move      n1 to FreeFormSearch
          if (PrimarySearch = 1 & SecondarySearch <= 1 & ClientType <= 1) ;Company
                    if (freeformSearch = 1)
                              pack compfld2 with "01F",srch55
                    else
                              pack compfld2 with "01L",srch55
                    endif
                    call COMPAIM
                    if Over
                              alert     caution,"Company Not Found",result,"Look up Unsuccessful"
                              CALL      CompSearchEnabled
                              return
                    else
                              call LoadListViewCompany
                    endif
;                             move      "SearchOK-NBRKKG",Location
                    loop
                              call COMPKG
                              if not over
;                                                 pack      str10,BRKNUM,SLASH,BRKCNT
;                             pack      Srchstr,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRCREDIT
                                        call LoadListViewCompany
                              endif
                    until over
                    repeat
          elseif (PrimarySearch = 1 & SecondarySearch <= 1 & ClientType > 1) ;Company with Type Filter
                    if (freeformSearch = 1)
                              pack compfld2 with "01F",srch55
                    else
                              pack compfld2 with "01L",srch55
                    endif
;patch1.1
                    if (ClientType = 2)
                              MOVE "02XT" to COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
//Patch 1.2 Code Modified
                              CLEAR COMPFLD13               
//Patch 1.2 Code Modified                         
                    elseif (ClientType = 3)
                              CLEAR COMPFLD7
                              MOVE "03XT" to COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
//Patch 1.2 Code Modified
                              CLEAR COMPFLD13               
//Patch 1.2 Code Modified                         
//Patch 1.2 Code Added 
//Service Bureau
                    elseif (ClientType = 5)
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10                         
                              MOVE "05XT" to COMPFLD13
//Patch 1.2 Code Added
                    elseif (ClientType = 6)
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
//Patch 1.2 Code Modified
                              MOVE "06XT" to COMPFLD10
.                             MOVE "05XT" to COMPFLD10
                              CLEAR COMPFLD13               
//Patch 1.2 Code Modified               
//Patch 1.2 Code Modified                                             
                    elseif (ClientType = 4 or ClientType = 7)
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
                              CLEAR COMPFLD13
                              alert caution,"Not Currently a Valid Search Type",result,"Search"
                              pack      str25,"(",b1,"0",b1,")",b2,"Record","(s)"
                              setprop   CompSearchStatusBar001,Text=str25                           
                              CALL      CompSearchEnabled                       
                              Return
//Patch 1.2 Code Modified     
                    else
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
                              CLEAR COMPFLD13               
                    endif
;patch1.1
                    call COMPAIM
                    if Over
                              alert     caution,"Company Not Found",result,"Look up Unsuccessful"
                              CALL      CompSearchEnabled
                              return
                    else
                              clear str12
                              call WhatCompTypeAmI
;                             if (str20 <> "")
                              call LoadListViewCompany
;                             endif
                    endif
;                             move      "SearchOK-NBRKKG",Location
                              loop
                                        call COMPKG
                                        if not over
                                                  call WhatCompTypeAmI
;                                                 if (str20 <> "")
                                                  call LoadListViewCompany
;                                                 endif
                                        endif
                              until over
                              repeat
          elseif (PrimarySearch = 1 & SecondarySearch = 3 & ClientType <= 1) ;Company with Contact Filter
                    if (freeformSearch = 1)
                              pack  compfld2 with "01F",srch55
                    else
                              pack compfld2 with "01L",srch55
                    endif
;patch1.1
                    if (ClientType = 2)
                              MOVE "02XT" to COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
//Patch 1.2 Code Modified
                              CLEAR COMPFLD13               
//Patch 1.2 Code Modified                         
                    elseif (ClientType = 3)
                              CLEAR COMPFLD7
                              MOVE "03XT" to COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
//Patch 1.2 Code Modified
                              CLEAR COMPFLD13               
//Patch 1.2 Code Modified                         
//Patch 1.2 Code Added 
//Service Bureau
                    elseif (ClientType = 5)
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10                         
                              MOVE "05XT" to COMPFLD13
                              
//Patch 1.2 Code Added                            
                    elseif (ClientType = 6)
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
//Patch 1.2 Code Modified
                              MOVE "06XT" to COMPFLD10
.                             MOVE "05XT" to COMPFLD10
                              CLEAR COMPFLD13
//Patch 1.2 Code Modified
//Patch 1.2 Code Modified                                             
                    elseif (ClientType = 4 or ClientType = 7)
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
                              CLEAR COMPFLD13
                              alert caution,"Not Currently a Valid Search Type",result,"Search"
                              pack      str25,"(",b1,"0",b1,")",b2,"Record","(s)"
                              setprop   CompSearchStatusBar001,Text=str25                           
                              CALL      CompSearchEnabled                       
                              Return
//Patch 1.2 Code Modified     
                    else
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
//Patch 1.2 Code Modified
                              CLEAR COMPFLD13               
//Patch 1.2 Code Modified                         
                    endif

;patch1.1
                    call COMPAIM
                    if Over
                              alert     caution,"Company Not Found",result,"Look up Unsuccessful"
                              CALL      CompSearchEnabled
                              return
                    else
                              if (freeformSearch = 1)
                                        pack CNCTFLD2 with "01X",COMPNUM
                                        if (str55 <> "")
                                                  move str55 to str45
                                                  pack cnctfld3 with "02F",str45
                                        else
                                                  Clear     CNCTFLD3
                                        endif
                                        
                              else
                                        pack CNCTFLD2 with "01X",COMPNUM
                                        if (str55 <> "")
                                                  move str55 to str45
                                                  pack cnctfld3 with "02L",str45
                                        else
                                                  Clear CNCTFLD3
                                        endif
                              endif
                              
                              call CNCTAIM
                              if Over
;                   alert     caution,"Company Not Found",result,"Look up Unsuccessful"
;                                       return
                              else
                                                  clear str12
                                                  call loadlistviewContact
                                                  loop
                                                            call CNCTKG
                                                  until over
                                                            call loadlistviewContact
                                                  repeat
                              endif
;                                       pack      str10,BRKNUM,SLASH,BRKCNT
;                           pack        Srchstr,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRCREDIT
                    endif
;                             move      "SearchOK-NBRKKG",Location

             loop
                              call COMPKG
                    until over
;                             if Over
;                   alert     caution,"Company Not Found",result,"Look up Unsuccessful"
;                                       return
;                           else
                              if (freeformSearch = 1)
                                        pack CNCTFLD2 with "01X",COMPNUM
                                        move str55 to str45
                                        pack cnctfld3 with "02F",str45
                              else
                                        pack CNCTFLD2 with "01X",COMPNUM
                                        move str55 to str45
                                        pack cnctfld3 with "02L",str45
                              endif
                              call CNCTAIM
                              if Over
;                                       alert     caution,"Company Not Found",result,"Look up Unsuccessful"
;                                                 return
                              else
                                                  call loadlistviewContact
                                        loop
                                                  call CNCTKG
                                        until over
                                                  call loadlistviewContact
                                        repeat              
                              endif
;                             endif

                  repeat
          elseif (PrimarySearch = 1 & SecondarySearch = 3 & ClientType > 1) ;Company with Contact Filter and with Type Filter
                    if (freeformSearch = 1)
                              pack  compfld2 with "01F",srch55
                    else
                              pack compfld2 with "01L",srch55
                    endif
;patch1.1
                    if (ClientType = 2)
                              MOVE "02XT" to COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
//Patch 1.2 Code Modified                                                                           
                              CLEAR COMPFLD13                         
//Patch 1.2 Code Modified                                                                           
                    elseif (ClientType = 3)
                              CLEAR COMPFLD7
                              MOVE "03XT" to COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
//Patch 1.2 Code Modified                                                                           
                              CLEAR COMPFLD13                         
//Patch 1.2 Code Modified                                                                           
//Patch 1.2 Code Added 
//Service Bureau
                    elseif (ClientType = 5)
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10                         
                              MOVE "05XT" to COMPFLD13
//Patch 1.2 Code Added                            
                    elseif (ClientType = 6)
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
//Patch 1.2 Code Modified                         
.                             MOVE "05XT" to COMPFLD10
                              MOVE "06XT" to COMPFLD10
                              CLEAR COMPFLD13                         
//Patch 1.2 Code Modified               
//Patch 1.2 Code Modified                                             
                    elseif (ClientType = 4 or ClientType = 7)
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
                              CLEAR COMPFLD13
                              alert caution,"Not Currently a Valid Search Type",result,"Search"
                              pack      str25,"(",b1,"0",b1,")",b2,"Record","(s)"
                              setprop   CompSearchStatusBar001,Text=str25                           
                              CALL      CompSearchEnabled                       
                              Return
//Patch 1.2 Code Modified     
                    else
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
//Patch 1.2 Code Modified                                                                           
                              CLEAR COMPFLD13                         
//Patch 1.2 Code Modified                                             
                    endif

;patch1.1
                    call COMPAIM
                    if Over
                    alert     caution,"Company Not Found",result,"Look up Unsuccessful"
                              CALL      CompSearchEnabled
                              return
                  else
                              if (freeformSearch = 1)
                                        pack CNCTFLD2 with "01X",COMPNUM
                                        if (str55 <> "")
                                                  move str55 to str45
                                                  pack cnctfld3 with "02F",str45
                                        else
                                                  Clear     CNCTFLD3
                                        endif
;                                       move str55 to str45
;                                       pack cnctfld3 with "02F",str45
                                        
                              else
                                        pack CNCTFLD2 with "01X",COMPNUM
                                        if (str55 <> "")
                                                  move str55 to str45
                                                  pack cnctfld3 with "02L",str45
                                        else
                                                  Clear     CNCTFLD3
                                        endif
;                                       move str55 to str45
;                                       pack cnctfld3 with "02L",str45
                              endif
                              
                              call CNCTAIM
                              if Over
;                             alert     caution,"Company Not Found",result,"Look up Unsuccessful"
;                                       return
                              else
                                        move      ClientTypeCompare,str1
                                        if (str1 = CNCTTYPE)
                                                  call loadlistviewContact
                                        endif
                                        loop
                                                  call CNCTKG
                                        until over
                                                  move      ClientTypeCompare,str1
                                                  if (str1 = CNCTTYPE)
                                                            call loadlistviewContact
                                                  endif
                                        repeat
                              endif


;                                       pack      str10,BRKNUM,SLASH,BRKCNT
;                           pack        Srchstr,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRCREDIT
                    endif
;                             move      "SearchOK-NBRKKG",Location

                    loop
                              call COMPKG
                    until over
;                             if Over
;                   alert     caution,"Company Not Found",result,"Look up Unsuccessful"
;                                       return
;                           else
                              if (freeformSearch = 1)
                                        pack CNCTFLD2 with "01X",COMPNUM
                                        if (str55 <> "")
                                                  move str55 to str45
                                                  pack cnctfld3 with "02F",str45
                                        else
                                                  Clear     CNCTFLD3
                                        endif
;                                                 move str55 to str45
;                                                 pack cnctfld3 with "02F",str45
                              else
                                        pack CNCTFLD2 with "01X",COMPNUM
                                        if (str55 <> "")
                                                  move str55 to str45
                                                  pack cnctfld3 with "02F",str45
                                        else
                                                  Clear     CNCTFLD3
                                        endif
;                                                 move str55 to str45
;                                                 pack cnctfld3 with "02L",str45
                              endif
                              call CNCTAIM
                              if Over
;                                       alert     caution,"Company Not Found",result,"Look up Unsuccessful"
;                                                 return
                              else
                                        move      ClientTypeCompare,str1
                                        if (str1 = CNCTTYPE)
                                                  call loadlistviewContact
                                        endif
                                        loop
                                                  call CNCTKG
                                        until over
                                                  move      ClientTypeCompare,str1
                                                  if (str1 = CNCTTYPE)
                                                            call loadlistviewContact
                                                  endif
                                        repeat              
                              endif
;                             endif
                              repeat
          elseif (PrimarySearch = 2 & ClientType <= 1) ;Contact 
                    clear     cnctfld2
                    if (freeformSearch = 1)
                              move srch55 to str45
                              pack cnctfld3 with "02F",str45
                    else
                              move srch55 to str45
                              pack cnctfld3 with "02L",str45
                    endif
                    call CNCTAIM
                    if Over
                              alert     caution,"Contact Not Found",result,"Look up Unsuccessful"
                              CALL      CompSearchEnabled
                              return
                    else
;                                       pack      str10,BRKNUM,SLASH,BRKCNT
;                           pack        Srchstr,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRCREDIT
                              move CNCTCODE to COMPFLD
                              call      zfillit using COMPFLD,c0
                              call      COMPKEY
                              call loadlistviewContact
                    endif
;                             move      "SearchOK-NBRKKG",Location
                    loop
                              call CNCTKG
                              if not over
;                                                 pack      str10,BRKNUM,SLASH,BRKCNT
;                             pack      Srchstr,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRCREDIT
                                        move CNCTCODE to COMPFLD
                                        call      zfillit using COMPFLD,c0
                                        call COMPKEY
                                        call loadlistviewContact
                              endif
                    until over
                    repeat
          elseif (PrimarySearch = 2 & ClientType > 1) ;Contact With Type Filter
                    clear     cnctfld2
                    if (freeformSearch = 1)
                              move srch55 to str45
                              pack cnctfld3 with "02F",str45
                    else
                              move srch55 to str45
                              pack cnctfld3 with "02L",str45
                    endif
                    call CNCTAIM
                    if Over
                              alert     caution,"Contact Not Found",result,"Look up Unsuccessful"
                              CALL      CompSearchEnabled
                              return
                    else
;                                       pack      str10,BRKNUM,SLASH,BRKCNT
;                           pack        Srchstr,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRCREDIT
                              move CNCTCODE to COMPFLD
                              call      zfillit using COMPFLD,c0
                              call      COMPKEY
;                                       move      ClientTypeCompare,str1
                                        if (str1 = CNCTTYPE)
                                                  call loadlistviewContact
                                        endif
                    endif
;                             move      "SearchOK-NBRKKG",Location
                    loop
                              call CNCTKG
                              if not over
;                                                 pack      str10,BRKNUM,SLASH,BRKCNT
;                             pack      Srchstr,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRCREDIT
                                        move CNCTCODE to COMPFLD
                                        call      zfillit using COMPFLD,c0
                                        call COMPKEY
                                        move      ClientTypeCompare,str1
                                        if (str1 = CNCTTYPE)
                                                  call loadlistviewContact
                                        endif
                              endif
                    until over
                    repeat
          elseif (PrimarySearch = 3)  ;Company and Contact 
;Company
                    if (freeformSearch = 1)
                              pack compfld2 with "01F",srch55
                    else
                              pack compfld2 with "01L",srch55
                    endif
;patch1.1
                    if (ClientType = 2)
                              MOVE "02XT" to COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
//Patch 1.2 Code Modified               
                              CLEAR COMPFLD13
//Patch 1.2 Code Modified                                                                           
                    elseif (ClientType = 3)
                              CLEAR COMPFLD7
                              MOVE "03XT" to COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
//Patch 1.2 Code Modified               
                              CLEAR COMPFLD13
//Patch 1.2 Code Modified                                                                           
//Patch 1.2 Code Added 
//Service Bureau
                    elseif (ClientType = 5)
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10                         
                              MOVE "05XT" to COMPFLD13
//Patch 1.2 Code Added                            
                    elseif (ClientType = 6)
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
//Patch 1.2 Code Modified                                                       
.                             MOVE "05XT" to COMPFLD10
                              MOVE "06XT" to COMPFLD10
                              CLEAR COMPFLD13
//Patch 1.2 Code Modified                                             
                    elseif (ClientType = 4 or ClientType = 7)
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
                              CLEAR COMPFLD13
                              alert caution,"Not Currently a Valid Search Type",result,"Search"
                              pack      str25,"(",b1,"0",b1,")",b2,"Record","(s)"
                              setprop   CompSearchStatusBar001,Text=str25                           
                              CALL      CompSearchEnabled                       
                              Return
//Patch 1.2 Code Modified               
                    else
                              CLEAR COMPFLD7
                              CLEAR COMPFLD8
                              CLEAR COMPFLD9
                              CLEAR COMPFLD10
//Patch 1.2 Code Modified               
                              CLEAR COMPFLD13
//Patch 1.2 Code Modified                                             
//                            alert caution,"Not Currently a Valid Search Type",result,"Search"
                    endif

;patch1.1
                    call COMPAIM
                    if Over
                              move      "Company and Contact cannot be matched to query!",taskname
                              Goto      CompTryContact
;                   alert     caution,"Company Not Found",result,"Look up Unsuccessful"
;                             return
                    else
;                                       pack      str10,BRKNUM,SLASH,BRKCNT
;                           pack        Srchstr,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRCREDIT
                              move      "A Contact cannot be matched to the query!",taskname
                              move      ClientType,str1
//                            if (str1 <= "1")
                                        call LoadListViewCompany
                                        Add       c1 to count
//                            else
//                                      move      ClientTypeCompare,str1
//                                      if (str1 = CNCTTYPE)          
//                                                call LoadListViewCompany
//                                      endif
//                            endif
                    endif
;                             move      "SearchOK-NBRKKG",Location
                    loop
                              call COMPKG
                              if not over
;                                                 pack      str10,BRKNUM,SLASH,BRKCNT
;                             pack      Srchstr,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRCREDIT
                                        move      ClientType,str1
//                                      if (str1 <= "1")
                                                  call LoadListViewCompany
//                                      else
//                                                move      ClientTypeCompare,str1
//                                                if (str1 = CNCTTYPE)          
//                                                          call LoadListViewCompany
//                                                endif
//                                      endif
                              endif
                    until over
             repeat
CompTryContact
;Contact
                    clear     cnctfld2
                    if (freeformSearch = 1)
                              move srch55 to str45
                              pack cnctfld3 with "02F",str45
                    else
                              move srch55 to str45
                              pack cnctfld3 with "02L",str45
                    endif
                    call CNCTAIM
                    if Over
                              alert     caution,Taskname,result,"Look up"
                              Call      CompSearchEnabled
                              return
                    else
;                                       pack      str10,BRKNUM,SLASH,BRKCNT
;                           pack        Srchstr,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRCREDIT
                              move CNCTCODE to COMPFLD
                              call      zfillit using COMPFLD,c0
                              call      COMPKEY
                              move      ClientType,str1
                              if (str1 <= "1")    .no type was selected
                                                  call loadlistviewContact
                              else      .filter by type
                                        move      ClientTypeCompare,str1
                                        if (str1 = CNCTTYPE)                    
                                                  call loadlistviewContact
                                        endif
                              endif
                    endif
;                             move      "SearchOK-NBRKKG",Location
                    loop
                              call CNCTKG
                              if not over
;                                                 pack      str10,BRKNUM,SLASH,BRKCNT
;                             pack      Srchstr,BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRCREDIT
                                        move CNCTCODE to COMPFLD
                                        call      zfillit using COMPFLD,c0
                                        call COMPKEY
                                        move      ClientType,str1
                                        if (str1 <= "1")    .no type was selected
                                                  call loadlistviewContact
                                        else      .filter by type
                                                  move      ClientTypeCompare,str1
                                                  if (str1 = CNCTTYPE)                    
                                                            call loadlistviewContact
                                                  endif
                                        endif
                              endif
                    until over
                    repeat
                    CompSearchListView.SortColumn using *Column=1,*Type=1,*Column1=4,*Type1=1
          endif
          move      Count to str9
          call      trim using str9
          pack      str25,"(",b1,str9,b1,")",b2,"Record","(s)"
          setprop             CompSearchStatusBar001,Text=str25
          if        (count = c0)
                    alert     note,"No Records found for this query!",Result,"Search"
          endif
          CALL      CompSearchEnabled
          clear     Count
          return
.
Exit
;Patch1.1 DMB Patch
LoadListViewCompany
          CompSearchListView.InsertItem giving n7 using COMPNUM
          CompSearchListView.SetItemText  giving N8 using n7,COMPCOMP,1
;patch 06/07/04 - DMB
          call WhatCompTypeAmI
          CompSearchListView.SetItemText  giving N8 using n7,str20,2
;                                   CompSearchListView.SetItemText  giving N8 using n7,"",2
;patch06/07/04 - DMB
          CompSearchListView.SetItemText  giving N8 using n7,"",3
          CompSearchListView.SetItemText  giving N8 using n7,"",4
          CompSearchListView.SetItemText  giving N8 using n7,"",5
          pack      CompSearchstr,COMPNUM,COMPCOMP,z3,b45
          CompSearchListView.SetItemText  giving N8 using n7,CompSearchStr,6
          Add       c1 to count
          
          return
LoadListViewContact
          CompSearchListView.InsertItem giving n7 using COMPNUM
          CompSearchListView.SetItemText  giving N8 using n7,COMPCOMP,1
;patch 06/07/04 - DMB
          call WhatCompTypeAmI
          CompSearchListView.SetItemText  giving N8 using n7,str20,2
;                                   CompSearchListView.SetItemText  giving N8 using n7,"",2
;patch06/07/04 - DMB
          CompSearchListView.SetItemText  giving N8 using n7,CNCTID,3
          CompSearchListView.SetItemText  giving N8 using n7,CNCTFNAME,4
          call      WhatContactTypeAMI
          CompSearchListView.SetItemText  giving N8 using n7,ContactType,5
          pack      CompSearchstr,COMPNUM,COMPCOMP,CNCTID,CNCTFNAME
          CompSearchListView.SetItemText  giving N8 using n7,CompSearchStr,6
          Add       c1 to count                                       
          return
;Patch1.1 DMB Patch

;         include   compio.inc
;         include   cntio.inc
;patch1.2
                              include   compio.inc
                              include   cntio.inc
.         include nbrkio.inc
;patch1.2

          include comlogic.inc