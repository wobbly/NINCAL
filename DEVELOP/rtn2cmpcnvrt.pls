                    Include             Common.inc
pc                  equ                 0
                    Include             Cons.inc
                    Include             Norddd.inc
                    Include             Nrtndd.inc
                    Include             \\nts0\c\library\develop\Cmpdd.inc
                    Include             \\nts0\c\library\develop\Cntdd.inc
Release             Init                "Pre"
Unused              File
Errors               File
Duplicate           File                ;duplicate companies
Duplicate1           File               ;duplicate contacts
OrdCheck            Ifile               Keylen=4,fixed=408        .secondary index of order file by return-to #
attn                Init                "Attn:"
CntLName1    	dim       	25      54-78           ;Last Name
CntSal1      	dim       	30     112-141          ;Salutation
...............................................................................
;                    Prepare             Cmpfile,"c:\nincal\data\Company.dat","c:\nincal\data\Company.isi","1-5","5","456"
;                    Prepare             Cmpfile2,"c:\nincal\data\Company.dat","c:\nincal\data\Company.AAM"
;                    Prepare             CntFile,"c:\nincal\data\Contacts.dat","c:\nincal\data\Contacts.isi","1-8","8","234"
;                    Prepare             CntFile2,"c:\nincal\data\Contacts.dat","c:\nincal\data\Contacts.aam"
                    open                Ordcheck,"\\nts1\e\data\index\ninordrtn.isi",read
                    Prepare             Unused,"c:\work\UnusedRtn.dat"
                    Prepare             Duplicate,"c:\work\DupesRtn.dat"
                    Prepare             Duplicate1,"c:\work\DupesRtn1.dat"
                    Prepare             Errors,"c:\work\ErrorsRtn.dat"
                    Move                c1 to N5
looper              call                Nrtnks
                    goto                Done if over
;
                    clear               CmpCOmp
                    clear               Cmpaddr
                    clear               Cmpaddr2
                    clear               CmpCity
                    Clear               CmpState
                    Clear               CmpZip
                    Clear               CmpCntry
                    Clear               CmpUser
                    Clear               CmpRdte
                    Clear               CmpPhone
                    Clear               CmpFax
                    Clear               CmpCode
                    Clear               CmpFld
                    Clear               CmpFld2
                    clear               CntFld
                    Clear               CntFld2
                    Clear               CntFname
                    clear               CntId
                    Clear               Cntcode
                    clear               CntSal
                    Clear               CntLname
                    clear               CntSal1
                    Clear               CntLname1
                    Read                Ordcheck,Rtnum;;
                    goto                Unused if over
;
                    call                Trim using rtcomp
                    if                  ((rtcomp = "" or RTComp = " ") & Rtnum <> "0000" & rtnum <> "0269")
                    goto                Errors
                    endif
                    Packkey             CMPFld2 from "01L",RtComp
                    call                CmpAim
                    goto                Write if over
                    Write               Duplicate,seq;rtnvars
                    packkey             CmpFld from Cmpcode
                    goto                testcmpX    
;
Write               Move                Rtcomp to CmpComp
                    Move                RtAddr to CmpAddr
                    Move                Rt2Addr to CmpAddr2
                    Move                RtCity to CmpCity
                    Move                RtState to CmpState
                    Move                RtZip to CmpZip
                    Move                RtCoun to CmpCntry
                    move                RtName to Cmpuser
                    Move                RtRevDat to Cmprdte
                    Move                RtTEle to CmpPhone
                    Move                RtFax to CmpFax
                    move                RtName to CpmaUser
                    Move                "T",Cmpsvbflg
                    Move                Rtnum to CmpSvbcde
                    move                N5 to CmpCode
                    rep                 zfill in cmpcode    
                    packkey             CmpFld from Cmpcode
TestCmp             call                CmpTst
                    If                  not over
                    add                 c1 to N5
                    move                N5 to Cmpcode
                    rep                 zfill in cmpCode
                    packkey             CmpFld from Cmpcode
                    goto                testCmp
                    endif
                    call                CmpWrt
TestCmpX
                    call    TRIM using RtCntct
;
            Scan        "Attn:" in Rtcntct
            If          equal
            bump        Rtcntct,c5
            clear       str55
            append        RtCntct to str55
            reset       str55
            clear       Rtcntct
            move        str55 to RtCntct
            endif            
            reset       Rtcntct

        reset   RtCntct
        match   "ATTN ",RtCntct
        call    ContactClean if equal
        reset   RtCntct
        match   "Attn ",RtCntct
        call    ContactClean if equal
        reset   RtCntct
        match   "ATTN: ",RtCntct
        call    ContactClean if equal
        reset   RtCntct
        match   "attn: ",RtCntct
        call    ContactClean if equal
        reset   RtCntct
        match   "ATTN:",RtCntct
        call    ContactClean if equal
        reset   RtCntct
        match   "Aatn: ",RtCntct
        call    ContactClean if equal
        reset   RtCntct
         match   "Attn: ",RtCntct
        call    ContactClean if equal
        reset   RtCntct
         match   "Attn:",RtCntct
        call    ContactClean if equal
        reset   RtCntct
         match   "Attn; ",RtCntct
        call    ContactClean if equal
        reset   RtCntct
         match   "Atn: ",RtCntct
        call    ContactClean if equal
        reset   RtCntct
         match   "Atttn: ",RtCntct
        call    ContactClean if equal
        reset   RtCntct
         match   "C/O ",RtCntct
        call    ContactClean if equal
        reset   RtCntct
         match   "c/o ",RtCntct
        call    ContactClean if equal
        reset   RtCntct
         match   "C/o ",RtCntct
        call    ContactClean if equal
        reset   RtCntct
         match   "Att: ",RtCntct
        call    ContactClean if equal
        reset   RtCntct
;
            Scan        "Mrs." in Rtcntct
            If          equal
                        move        "Mrs." to CntSal1
                        bump        Rtcntct,c4
                        clear       str55
                        append        RtCntct to str55
                        reset       str55
             clear       Rtcntct
                        move        str55 to RtCntct
            endif            
            reset       Rtcntct
;
            Scan        "Mr." in Rtcntct
            If          equal
            move        "Mr." to CntSal1
            bump        Rtcntct,c3
            clear       str55
            append        RtCntct to str55
            reset       str55
            clear       Rtcntct
            move        str55 to RtCntct
            endif            
            reset       Rtcntct
;
            Scan        "Ms." in Rtcntct
            If          equal
            move        "Ms." to CntSal1
            bump        Rtcntct,c3
            clear       str55
            append        RtCntct to str55
            reset       str55
            clear       Rtcntct
            move        str55 to RtCntct
            endif            
            reset       Rtcntct
;
            Scan        "Mr" in Rtcntct
            If          equal
            movefptr    Rtcntct,n9
                        if          (n9 > 3)
                        reset       Rtcntct
                        else
                        move        "Mr." to CntSal1
                        bump        Rtcntct,c2
                        clear       str55
                        append        RtCntct to str55
                        reset       str55
                        clear       Rtcntct
                        move        str55 to RtCntct
                        endif            
            endif
            reset       Rtcntct
;
            Scan        "Ms" in Rtcntct
            If          equal
            movefptr    Rtcntct,n9
                        if          (n9 > 3)
                        reset       Rtcntct
                        else
                        move        "Ms." to CntSal1
                        bump        Rtcntct,c2
                        clear       str55
                        append        RtCntct to str55
                        reset       str55
                        clear       Rtcntct
                        move        str55 to RtCntct
                        endif            
            endif
            reset       Rtcntct

;            reset   RtCntct

         match   "Sr. ",RtCntct
         if             equal
         bump           rtcntct,c3
         call           Salute 
         goto           Salutex
         endif
            reset       Rtcntct

         match   "Dr. ",RtCntct
         if             equal
         bump           rtcntct,c3
         call           Salute 
         goto           Salutex
         endif
SaluteX
        reset   RtCntct

        call            TRIM using RtCntct
        call            Lastname

                    If                  (RtCntct = "" or RTCNTCT = " ")
                    goto                looper
                    endif
                    call                debug    
                    packkey             CntFld2 from "01L",RTCNTCT
                    call                Cntaim
                    if                  not over
                                        match               Cmpcode to cntcode
                                        if                  equal
                                        goto                duplicate1
                                        endif
                    else
                    goto                CntTestX
                    endif

Cnttest             call                Cntkg
                    if                  not over
                    match               Cmpcode to cntcode
                                if                  equal
                                goto                Duplicate1
                                else
                                goto                Cnttest
                               endif
                     Else
                     goto           CntTestX
                     endif

CntTestX            Move                CmpCode to CntCode
                    REP                 ZFILL IN Cntcode
                    Move                C1 to N3
                    move                n3 to CntID
                    REP                 ZFILL IN CntID
                    packkey             CntFld from CntCode,CntID
TestCnt             call                Cnttst
                    If                  Not OVer
                    add                 c1 to n3
                    move                n3 to CntID
                    REP                 ZFILL IN CntID
                    packkey             CntFld from CntCode,CntID
                    goto                Testcnt
                    endif
                    Move                Rtcntct to CntFName
                    move                CntLname1 to CntLname
                    move                CntSal1 to CntSal
                    call                CntWrt
                    goto                looper
;
Unused              Write               Unused,seq;rtnvars
                    goto                looper
;
Duplicate           Write               Duplicate,seq;rtnvars
                    goto                looper
Duplicate1           Write               Duplicate1,seq;rtnvars
                    goto                looper
;
Errors               Write               Errors,seq;rtnvars
                    goto                looper
;
ContactClean
.Extracts "attn:" and "Attn:" from string
        clear   str55
        clear   N9
        reset   RTCNTCT
        loop
                add "1",N9
                reset RtCNTCT,N9
                cmatch B1,RtCNTCT
                until equal
                until eos
        repeat
        move    RtCNTCT,str55
        clear   RtCNTCT
        reset   str55
        move    str55,RtCNTCT
        return
;
Salute

        clear           str55
        clear           N9
        MOVEFPTR        Rtcntct,n9
        reset           RTCNTCT
        setLptr         RTCNTCT,N9
        move            Rtcntct to CntSal1
        setLptr         RTCNTCT,45
        reset           RTCNTCT,0
        add             c1 to n9
        bump            rtcntct,n9
        append          rtcntct to str55
        reset           str55
        clear           rtcntct
        move            str55 to rtcntct
        call            trim using CntSal1    
        return
Lastname
            scan        b1 in RtCntct
            if          equal
            bump        rtCntct by 1
            clear       CntLname
            Append      RtCntct to CntLname1
            reset       CntLname1
            endif
            Reset       Rtcntct
            return

debug       return

Done                weof                Duplicate,seq
                    Weof                UNused,seq
                    Weof                Errors,seq
                    stop

                    Include             Nordio.inc
                    Include             Nrtnio.inc
                    Include             \\nts0\c\library\develop\Cmpio.inc
                    Include             \\nts0\c\library\develop\Cntio.inc
                    include             comlogic.inc

