//////////////////////////////////////////////////////////////////////////////
//
//  PROGRAM:    sessionMgmt.inc
//
//   AUTHOR:    Mandie Lyons (mlyons@adjacency.net)
//
//     DATE:    02 NOV 2004
//
//  PURPOSE:    NIN session management routines
//
// REVISION:    02 NOV 2004     VER01   MLYONS    Created
//
//////////////////////////////////////////////////////////////////////////////
    include session.io
;    include audit.io
    include guidGen.inc
//////////////////////////////////////////////////////////////////////////////
#adminAccess    dim     10
#currentGUID    dim     36
#cwk2           dim     02
#cwk14          dim     14
#cwk54          dim     54
#desc           dim     260
#newGUID        dim     36
#sessionID      dim     ^

#now            form    14
#nwk2           form    02
#then           form    14
#diff           form    14
//////////////////////////////////////////////////////////////////////////////
    goto #end
//////////////////////////////////////////////////////////////////////////////
insertSession

    call generateGUID using #newGUID
    move #newGUID to sessionIO.id
    call cgiParse using "REMOTE_ADDR",sessionIO.ipAddress
    clock seconds into #now
    move #now to sessionIO.lastActivity

    call cgiParse using "username",fUser2IKey
    call fUser2IRead
    call fUser2IReadKS
    move fUserIO.userID to sessionIO.userID
    move fUserIO.userType to sessionIO.userType
    move fUserIO.fName to sessionIO.userFName
    move fUserIO.companyNumber to sessionIO.userCompany
    clear sessionIO.filler

    call sessionWrite
    move #newGUID to #currentGUID
;    call auditLog using "Logged in"

    return
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
updateSession

    call cgiParse using "REMOTE_ADDR",sessionIO.ipAddress
    clock seconds into sessionIO.lastActivity
    clear sessionIO.filler

    call sessionUpdate

    return
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
sessionLogin lroutine #SessionID

    // sessionLogin inserts or updates a session with valid information.
    call sessionOpen
    call CGIParse using "SID",#currentGUID
    if over
        call insertSession
        move #newGUID to #SessionID
    else
        // there is a current GUID, so look it up in the session file
        move #currentGUID to sessionKey
        call sessionRead
        if over
            call insertSession
            move #newGUID to #SessionID
        else
            call updateSession
            move #currentGUID to #SessionID
        endif
    endif

    return
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
sessionValidate lroutine #adminAccess

    // sessionValidate reads the SessionID from the CGI environment and
    //  checks to see if the Session exists and has been active in the
    //  last 20 minutes (1200 seconds)
    call sessionOpen
    call CGIParse using "SID",#currentGUID
 if over
        // session is not valid
;        call auditLog using "Session invalid"
.        stream *STDOUT,"<meta http-equiv=refresh content='0;url=../timeout.shtml'>"
.added by Robb for testing
        stream *STDOUT,"Step 1 failed ", "SID: ", #currentGUID, " sessionkey: ", sessionKey
.end added code        
        stop
    else
        move #currentGUID to sessionKey
        call sessionRead
        if over
            // the session is not valid
;            call auditLog using "Session invalid"
.        stream *STDOUT,"<meta http-equiv=refresh content='0;url=../timeout.shtml'>"
.added by Robb for testing
        stream *STDOUT,"Step 1 failed ", "SID: ", #currentGUID, " sessionkey: ", sessionKey
        stream *STDOUT,STDIN_DATA
.end added code        
        stop
        else
            clock seconds into #now
            move sessionIO.lastActivity to #then
            calc #diff = (#now - #then)
            if (#diff > 1200)
                // the session has timed out
                call updateSession
;                call auditLog using "Session timed out"
.        stream *STDOUT,"<meta http-equiv=refresh content='0;url=../timeout.shtml'>"
.added by Robb for testing
        stream *STDOUT,"Step 3 failed"
.end added code        
        stop
            else
                // the session is valid
                call updateSession
;                call auditLog using "Successfully accessed"
                move sessionIO to thisSession
            endif
        endif
    endif

    clear #adminAccess
    return
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
sessionLogout

    // sessionLogout deletes the users session from the session file
    call sessionOpen
    call CGIParse using "SID",#currentGUID
    if not over
        move #currentGUID to sessionKey
;        call auditLog using "Logged out"
        call sessionDelete
    endif

    return
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
;auditLog lroutine #desc
;
;    // inserts a row into the audit trail table
;    call cgiParse using "fundno",auditIO.fundNo
;
;    move #currentGUID to auditIO.sessionID
;    clock timestamp into auditIO.timeStamp
;    call cgiParse using "REMOTE_ADDR",auditIO.ipAddress
;    call cgiParse using "SCRIPT_NAME",auditIO.URL
;    move #desc to auditIO.description
;    clear auditIO.padding
;
;    // we also need to get the user id
;    move sessionIO.userID to auditIO.userID
;
;    call auditOpen
;    pack auditKey from auditIO.sessionID,auditIO.timeStamp
;
;    // because Windows only updates the timestamp once a second,
;    //  we need to make sure that if the keys are identical that we
;    //  manually adjust the milliseconds to avoid duplicate keys
;    call auditReadKey
;    if not over
;        unpack auditKey into #cwk54,#cwk2
;        move #cwk2 to #nwk2
;        add "1" to #nwk2
;        move #nwk2 to #cwk2
;        replace " 0" in #cwk2
;        unpack auditIO.timeStamp into #cwk14
;        pack auditIO.timeStamp from #cwk14,#cwk2
;    endif
;
;    call auditWrite
;    call auditClose

    return
//////////////////////////////////////////////////////////////////////////////
#end
//////////////////////////////////////////////////////////////////////////////
