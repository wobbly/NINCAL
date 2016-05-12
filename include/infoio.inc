.
THIS INCLUDE FILE IS NOW OBSOLETE.  LOGIC NOW FOUND IN INFO.PLS!!!!!!!!!!!!!!!!!
.
DisplayMailer LRoutine EditPtr,EditPtr1
.Mimics Generic MailerName_Click Event
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    OrderInfoClose
        else
.Must use MLRVARS for retrieval as ORDVARS need to remain intact.  At saving point ORDVARS would
.be rewritten
                getitem EditPtr,0,MNUM
                getitem EditPtr1,0,MCONTCT
                pack    MKEY,MNUM,MCONTCT
                if (MKEY <> "")
                        call    NMLRKEY
                        if not over
                                if (N4 = 8)     .Left Mouse Button
                                        pack    OrderInfoString,MCOMP
                                        setitem OrderInfoStatText1,0,OrderInfoString
                                        setitem OrderInfoStatText2,0,MNAME
                                        setitem OrderInfoStatText3,0,MADDR
                                        call    TRIM using MCITY
                                        if (MCITY <> "")
                                                pack    OrderInfoString,MCITY,", ",MSTATE,B1,MZIP
                                        else
                                                pack    OrderInfoString,MSTATE,B1,MZIP
                                        endif
                                        setitem OrderInfoStatText4,0,OrderInfoString
                                        cmatch  YES,MCOPIES
                                        if equal
                                                setprop OrderInfoStatText1,fgcolor=red
                                        else
                                                setprop OrderInfoStatText1,fgcolor=black
                                        endif
                                        clear   OrderInfoString
                                        call    TRIM using MCCTO
                                        count   N1,MCCTO
                                        if (N1 > C0)
                                                append  "CC: ",OrderInfoString
                                                append  MCCTO,OrderInfoString
                                                append  B2,OrderInfoString
                                        endif
                                        setprop OrderInfoStatText5,fgcolor=red
                                        if (MSTAT = "*")
                                                append  "Credit On Hold",OrderInfoString
                                        elseif (MSTAT = "N")
                                                append  "New - Credit On Hold",OrderInfoString
                                        elseif (MSTAT = "I")
                                                append  "Inactive-Credit On Hold",OrderInfoString
                                        elseif (MSTAT = "B")
                                                append  "Credit Risk",OrderInfoString
                                        elseif (MSTAT = "P")
                                                append  "Political-Credit On Hold",OrderInfoString
                                        elseif (MSTAT = "W")
                                                append  "Warning - See Notes!",OrderInfoString
                                                pack    NMLR2FLD,MKEY
                                                call    NMLR2KEY
                                                call    Trim using MLR2NOTES
                                                setprop OrderInfo,height=200
                                                create  OrderInfo;OrderInfoEditText=100:199:1:249,border=1,style=1,wordwrap=1,readonly=1
                                                activate OrderInfoEditText
                                                setitem OrderInfoEditText,0,MLR2NOTES
                                        else
                                                append  "Credit OK",OrderInfoString
                                                setprop OrderInfoStatText5,fgcolor=black
                                        endif
                                        reset   OrderInfoString
                                        setitem OrderInfoStatText5,0,OrderInfoString
.Position it correctly
                                        call    OrderSetInfoScreen
                                        setprop OrderInfo,title="Mailer Information"
                                        setprop OrderInfo,visible=1
                                else           .if (N4 = 16)        .Right Mouse Button
                                        move    C2 to NXRFPATH
                                        move    MNUM,NXRFFLD2
                                        call    NXRFKEY
                                        if over
                                                move    "000000",nxrflist
                                        endif
                                        move    MNUM,str4       
                                        clear   result        
                                        move    NXRFLIST,NDATFLD
                                        move    C1,NDATPATH
                                        call    NDATTST
                                        if not over   
                                                create  OrderInfo;OrderInfoListView=1:99:1:249,fullrow=1
.Next line added to per SA request - 10/19/2000
.                                                eventreg OrderInfoListView,6,OrderLoadUniverseInSpecial
.                                                eventreg OrderInfoListView,10,OrderInfoListViewKeyPress,CHAR=str1,RESULT=N9
                                                activate OrderInfoListView
                                                OrderInfoListView.DeleteAllContents
                                                OrderInfoListView.InsertColumn using "List",60,1
                                                OrderInfoListView.InsertColumn using "Name",140,2
                                                OrderInfoListView.InsertColumn using "Universe",70,3
                                                OrderInfoListView.InsertColumn using "Withdrawn",70,4
                                                loop
                                                        move    NXRFLIST,NDATFLD
                                                        move    C1,NDATPATH
                                                        call    NDATKEY
                                                        if not over
                                                                OrderInfoListView.InsertItem giving N9 using LSTNUM
                                                                OrderInfoListView.SetItemText using N9,OLSTNAME,1
                                                                move    UNIVERSE,str10
                                                                call    FormatNumeric using str10,str13
                                                                OrderInfoListView.SetColumnFormat using 2,1
                                                                OrderInfoListView.SetItemText using N9,str13,2
                                                                if (STATUS = "W" | STATUS = "T")
                                                                        move    "YES",str3
                                                                else
                                                                        clear   str3
                                                                endif
                                                                OrderInfoListView.SetItemText using N9,str3,3
                                                        endif
                                                        call    NXRFKS
                                                        until over
                                                        match  MNUM,NXRFMLR
                                                        until not equal
                                                repeat
.Put focus on 1rst item
                                                OrderInfoListView.SetItemState giving N9 using 0,2,2
.Position it correctly
                                                call    OrderSetInfoScreen
                                                setprop OrderInfo,title="Mailer Information, Associated Lists"
                                                setprop OrderInfo,visible=1
                                                setfocus OrderInfoListView
                                        endif
                                endif
                        endif
                endif
        endif
        return
 
DisplayBroker LRoutine EditPtr, EditPtr1
.Mimics Generic Broker Name Click Event
.Called by:  Order1EditBrk_KeyPress, Order1StatBrkComp_Click, Order6StatBrkComp_Click
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    OrderInfoClose
        else
.Must use BRKVARS for retrieval as ORDVARS need to remain intact.  At saving point ORDVARS would
.be rewritten
                getitem EditPtr,0,BRKNUM
                getitem EditPtr1,0,BRKCNT
                pack    NBRKFLD,BRKNUM,BRKCNT
                if (NBRKFLD <> "")
                        call    NBRKKEY
                        if not over
                                setitem OrderInfoStatText1,0,BRCOMP
                                setitem OrderInfoStatText2,0,BRADDR
                                call    TRIM using BRCITY
                                if (BRCITY <> "")
                                        pack    OrderInfoString,BRCITY,", ",BRSTATE,B1,BRZIP
                                else
                                        pack    OrderInfoString,BRSTATE,B1,BRZIP
                                endif
                                setitem OrderInfoStatText3,0,OrderInfoString
                                clear   OrderInfoString
                                call    Trim using BRTELE
                                if (BRTELE <> "" AND BRTELE <> "0000000000")
                                        unpack  BRTELE,area,str3,str4
                                        pack    OrderInfoString,"(",area,")",str3,DASH,str4
                                endif
                                setitem OrderInfoStatText4,0,OrderInfoString
                                setprop OrderInfoStatText5,fgcolor=red
                                if (BRCREDIT = "*")
                                        move    "Credit On Hold",OrderInfoString
                                elseif (BRCREDIT = "N")
                                        move    "New - Credit On Hold",OrderInfoString
                                elseif (BRCREDIT = "I")
                                        move    "Inactive - Credit On Hold",OrderInfoString
                                elseif (BRCREDIT = "B")
                                        move    "Credit Risk",OrderInfoString
                                elseif (BRCREDIT = "P")
                                        move    "Political - Credit On Hold",OrderInfoString
                                elseif (BRCREDIT = "W")
                                        move    "Warning - See Notes!",OrderInfoString
                                        pack    NBRK2FLD,BRKNUM
                                        call    NBRK2KEY
                                        call    Trim using BRK2NOTES
                                        setprop OrderInfo,height=200
                                        create  OrderInfo;OrderInfoEditText=100:199:1:249,border=1,style=1,wordwrap=1,readonly=1
                                        activate OrderInfoEditText
                                        setitem OrderInfoEditText,0,BRK2NOTES
                                else
                                        move    "Credit OK",OrderInfoString
                                        setprop OrderInfoStatText5,fgcolor=black
                                endif
                                setitem OrderInfoStatText5,0,OrderInfoString
.Position it correctly
                                call    OrderSetInfoScreen
                                setprop OrderInfo,title="Broker Information"
                                setprop OrderInfo,visible=1
                        endif
                endif
        endif
        return
 
DisplayShipTo LRoutine EditPtr,FrmPtr,DimPtr,DimPtr1,DimPtr2,DimPtr3
.Mimics Generic Ship-To Name Click Event
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    OrderInfoClose
        else
.Must use RTNVARS for retrieval as ORDVARS need to remain intact.  At saving point ORDVARS would
.be rewritten
                getitem EditPtr,0,str6
                if (FrmPtr = C1)
                        count   N1,str6
                        if (N1 = 6)     .OREUSE
                                move    str6,OREUSE
                                move    "0001",str6
                        endif
                endif
                move    str6,NRTNFLD
                if (NRTNFLD <> "")
                        call    NRTNKEY
                        if not over
.                                if (FrmPtr = C1)
.                                        move    OMLRNUM,str7
.                                elseif (FrmPtr = C6)
.                                        move    NCMPSHIPTO,str7
.                                endif
                                move    DimPtr,str7
                                match   "0677",str7
                                goto REPMLR if equal
                                match   "0210",str7
                                goto REPMLR if equal
                                match   "1361",str7         "CUSTOM LISTS"
                                goto REPMLR if equal
                                match   "0053",str7         "ANACAPA
                                goto REPMLR if equal
                                match   "0702",str7
                                goto REPMLR if equal
                                match   "0965",str7         "
                                goto REPMLR if equal
                                match   "2531",str6
                                goto KEEPRTN if equal
                                clear   str45
                                clear   str45a
.                                move    mcomp to str45
                                move    DimPtr1 to str45
                                move    rtcomp to str45a
                                rep     uplow in str45
                                rep     uplow in str45a
                                reset   str45
                                reset   str45a
                                match   str45 to str45a
                                goto KEEPRTN if equal
.                                move    MCOMP,OrderInfoString
                                move    DimPtr1,OrderInfoString
                                setitem OrderInfoStatText1,0,OrderInfoString
                                clear   OrderInfoString
                                append  "C/O ",OrderInfoString
                                goto LOADWINDOW
 
KEEPRTN                         move    RTCNTCT,OrderInfoString
                                setitem OrderInfoStatText1,0,OrderInfoString
                                clear   OrderInfoString
                                goto LOADWINDOW
 
REPMLR                          
.                                move    OODES,OrderInfoString
                                move    DimPtr2,OrderInfoString
                                setitem OrderInfoStatText1,0,OrderInfoString
                                clear   OrderInfoString
LOADWINDOW
                                append  RTCOMP,OrderInfoString
                                reset   OrderInfoString
                                setitem OrderInfoStatText2,0,OrderInfoString
.Address field of record "0001" says "Reuse of LR #"
                                clear   OrderInfoString
                                append  RTADDR,OrderInfoString
                                if (FrmPtr = C1)
                                        match   "0001",str6
                                        if equal
                                                append  B1,OrderInfoString
.                                                append  OREUSE,OrderInfoString
                                                append  DimPtr3,OrderInfoString
                                        endif
                                endif
                                reset   OrderInfoString
                                setitem OrderInfoStatText3,0,OrderInfoString
                                setitem OrderInfoStatText4,0,RT2ADDR
                                call    TRIM using RTCITY
                                if (RTCITY <> "")
                                        pack    OrderInfoString,RTCITY,", ",RTSTATE,B1,RTZIP
                                else
                                        pack    OrderInfoString,RTSTATE,B1,RTZIP
                                endif
                                setitem OrderInfoStatText5,0,OrderInfoString
.Position it correctly
                                call    OrderSetInfoScreen
                                setprop OrderInfo,title="Ship-To Information"
                                setprop OrderInfo,visible=1
                        endif
                endif   
        endif
        return
 
DisplayOwner LRoutine EditPtr
.Mimics Generic Owner Name Click Event
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    OrderInfoClose
        else
.Must use OWNVARS for retrieval as ORDVARS need to remain intact.  At saving point ORDVARS would
.be rewritten
                getitem EditPtr,0,NOWNFLD                
                if (NOWNFLD <> "")
                        call    NOWNKEY
                        if not over
                                setitem OrderInfoStatText1,0,OWNOCPY
                                setitem OrderInfoStatText2,0,OWNLONM
                                setitem OrderInfoStatText3,0,OWNLOSA
                                call    TRIM using OWNLOCTY
                                if (OWNLOCTY <> "")
                                        pack    OrderInfoString,OWNLOCTY,", ",OWNLOS,B1,OWNLOZC
                                else
                                        pack    OrderInfoString,OWNLOS,B1,OWNLOZC
                                endif
                                setitem OrderInfoStatText4,0,OrderInfoString
                                unpack  OWNTELE,area,str3,str4
                                pack    str55,"(",area,")",str3,DASH,str4,B5
                                clear   OrderInfoString
                                append  str55,OrderInfoString
                                unpack  OWNFAX,area,str3,str4
                                pack    str55,"Fax: (",area,")",str3,DASH,str4,B5
                                append  str55,OrderInfoString
                                reset   OrderInfoString
                                setitem OrderInfoStatText5,0,OrderInfoString
.Position it correctly
                                call    OrderSetInfoScreen
                                setprop OrderInfo,title="Owner Information"
                                setprop OrderInfo,visible=1
                        endif
                endif
        endif
        return
 
DisplayList LRoutine EditPtr,FrmPtr
.Mimics Generic List Name_Click Event
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    OrderInfoClose
        else
.Must use MLRVARS for retrieval as ORDVARS need to remain intact.  At saving point ORDVARS would
.be rewritten
                getitem EditPtr,0,NDATFLD
                if (NDATFLD <> "")
                        if (N4 = 8)     .Left Mouse Button
                                move    C0,UNIVERSE
                                call    NDATKEY
                                if not over
                                        setitem OrderInfoStatText1,0,OLSTNAME
                                        clear   OrderInfoString
                                        call    OrderLoadListColor using FrmPtr
                                        append  "Updated: ",OrderInfoString
                                        append  REVDATE,OrderInfoString
                                        if (JULDAYS > 90)
                                                append  " - Over 90 days old!!",OrderInfoString
                                        endif
                                        reset   OrderInfoString
                                        setitem OrderInfoStatText2,0,OrderInfoString
                                        clear   OrderInfoString
                                        append  "Universe: ",OrderInfoString
                                        move    UNIVERSE,str10
                                        call    FormatNumeric using str10,str13
                                        append  str13,OrderInfoString
                                        append  B2,OrderInfoString
                                        if (ELSTCDE = "C")
                                                append  "List is Exclusive!",OrderInfoString
                                        endif
                                        reset   OrderInfoString
                                        setitem OrderInfoStatText3,0,OrderInfoString
                                        move    B5,OrderInfoString
                                        clear   OrderInfoString
                                        if (STATUS = "W" OR STATUS = "T")
                                                append  "List is Withdrawn!  ",OrderInfoString
                                        endif
                                        move    TextData,str55  .Only search in first line!
                                        rep     lowup,str55     .Just in case text entered in lower case
                                        scan    "*EXCHANGE ONLY",str55
                                        if equal
                                                append  "*Exchange Only!",OrderInfoString
                                        else
                                                reset   str55
                                                scan    "EXCHANGE ONLY",str55
                                                if equal
                                                        append  "Exchange Only!",OrderInfoString
                                                endif
                                        endif
                                        reset   OrderInfoString
                                        setitem OrderInfoStatText4,0,OrderInfoString
                                        clear   OrderInfoString
                                        move    NDATFLD,NMDLFLD
                                        call    NMDLKEY
                                        if not over
                                                if (MDLCALL <> "" AND MDLCALL <> "  ")
                                                        move    C2,NUSEPATH
                                                        move    MDLCALL,NUSEFLD2
                                                        call    NUSEKEY
                                                        if not over
                                                                pack    OrderInfoString,"Suggested Caller: ",NUSEUSER
                                                        else
                                                                pack    OrderInfoString,"Suggested Caller: ",MDLCALL
                                                        endif
                                                endif
                                        endif
                                        setitem OrderInfoStatText5,0,OrderInfoString
.Position it correctly
                                        call    OrderSetInfoScreen
                                        setprop OrderInfo,title="List Information"
                                        setprop OrderInfo,visible=1
                                endif
                        else    .Right Mouse Button
                                move    NDATFLD,NMDLFLD
                                call    NMDLKEY
                                if not over
                                        create  OrderInfo;OrderInfoEditText=1:99:1:249,style=1,wordwrap=1,readonly=1
                                        activate OrderInfoEditText
                                        setitem OrderInfoEditText,0,MDLTEXT
.Position it correctly
                                        call    OrderSetInfoScreen
                                        setprop OrderInfo,title="List Information"
                                        setprop OrderInfo,visible=1
                                endif
                        endif
                endif
        endif
        return
 
DisplayXSTAT LRoutine EditPtr,DimPtr
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    OrderInfoClose
        else
                call    Trim using DimPtr
                if (DimPtr = "")
                        return
                endif
                getitem EditPtr,0,str6
                call    Trim using str6
                if (str6 = "")
                        return
                endif
                call    ZFILLIT using str6,C0
                call    ZFILLIT using DimPtr,C0
                move    str6,NDATFLD
                move    "StatMlrComp-NDATKEY",Location
                pack    KeyLocation,"Key: ",NDATFLD
                call    NDATKEY
                if not over
                        move    NDATFLD,OLNUM
                        move    DimPtr,OMLRNUM
                        call    OrderTestXSTAT
                        if (taskname = "")
                                pack    NORDFLD1,"01R",OMLRNUM
                                pack    NORDFLD2,"02R",OLNUM
                                clear   NORDFLD3
                                clear   NORDFLD4
                                call    OrderGetHistory using taskname,OLRN,C1
                        endif
                        call    Parsitup using OrderInfoString,taskname,C1
                        setitem OrderInfoStatText1,0,OrderInfoString
                        call    Parsitup using OrderInfoString,taskname,C1
                        setitem OrderInfoStatText2,0,OrderInfoString
                        call    Parsitup using OrderInfoString,taskname,C1
                        setitem OrderInfoStatText3,0,OrderInfoString
                        call    Parsitup using OrderInfoString,taskname,C1
                        setitem OrderInfoStatText4,0,OrderInfoString
                        call    Parsitup using OrderInfoString,taskname,C1
                        setitem OrderInfoStatText5,0,OrderInfoString
.Position it correctly  
                        call    OrderSetInfoScreen
                        setprop OrderInfo,title="Mailer Information"
                        setprop OrderInfo,visible=1
                endif
        endif
        return

OrderInfoClose
        setprop OrderInfo,visible=0
        setprop OrderInfo,winpos=3
        setprop OrderInfo,height=100
        setprop OrderInfoStatText1,fgcolor=black
        setprop OrderInfoStatText5,fgcolor=black
        destroy OrderInfoListView
        destroy OrderInfoEditText
        return

OrderSetInfoScreen
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
        clear   str25
        getinfo system,str25
        bump    str25,12
        move    str25,str4
        move    str4,ScrRight
        bump    str25,4
        move    str25,str4
        move    str4,ScrBottom
.
        setprop OrderInfo,winpos=1
        getprop nord0001,top=H,left=V
.Break down mouse coordinates - figured in terms of object where mouse was clicked
.MouseForm established at MouseDown_Event
        move    "10000",N7
        div     N7,MouseForm,N9 .N9=left
        mult    N9,N7            
        sub     N7,MouseForm,N8 .N8=top
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
        Getprop OrderInfo,height=N9,width=N8
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
.Set coordinates
        setprop OrderInfo,top=H,left=V
        return
