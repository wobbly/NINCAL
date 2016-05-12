//////////////////////////////////////////////////////////////////////////////
//
//  PROGRAM:    audit.io
//
//   AUTHOR:    Mandie Lyons (mlyons@adjacency.net)
//
//     DATE:    03 NOV 2004
//
//  PURPOSE:    NIN audit file IO routines.  Auditing is established so that
//              every online exchange between client and server can be
//              documented.
//
// REVISION:    03 NOV 2004     VER01   MLYONS    Created
//
//////////////////////////////////////////////////////////////////////////////
fAuditFiles     filelist
fAudit          ifile       name="audit"
fAudit2         ifile       name="audit2"
                filelistend

auditKey        dim         58
auditKey2       dim         58
audit2Key       dim         58

fAuditIndexCmd  init        "sunidxnt audit,audit,l853;e,n,1-58"
fAudit2IndexCmd init        "sunidxnt audit,audit2,l853;e,n,41-58,1-40"

auditErrorFlag  integer     1

auditIO         record
fundNo          dim         04      //   1 -   4
sessionID       dim         36      //   5 -  40
timestamp       dim         18      //  41 -  58
ipAddress       dim         15      //  59 -  73
URL             dim         260     //  73 - 332
Description     dim         260     // 333 - 592
userID          dim         36      // 593 - 628
padding         dim         224     // 629 - 853
                recordend

#fileManagerSettings    dim     260
///////////////////////////////////////////////////////////////////////////////
    goto #endOfInclude
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
auditOpen

    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""
    open fAuditFiles
    setmode *openuseip=#fileManagerSettings

    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
auditClose

    close fAuditFiles
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
auditRead

    read fAudit,auditKey;auditIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
auditReadKey

    move auditKey to auditKey2
    read fAudit,auditKey2;auditKey
    return
///////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
audit2Read

    read fAudit2,audit2Key;*LL,auditIO
    return
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
audit2ReadKS

    readks fAudit2;*LL,auditIO
    return
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
audit2ReadKP

    readkp fAudit2;*LL,auditIO
    return
//////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
auditWrite

    loop
        clear auditErrorFlag
        trap auditIOerror if IO
        clock timestamp into auditIO.timestamp
        write fAuditFiles;auditIO
        trapclr io
    repeat until (not auditErrorFlag)

    return
///////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
auditIOError

    set auditErrorFlag
    return
//////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
#endOfInclude
///////////////////////////////////////////////////////////////////////////////
