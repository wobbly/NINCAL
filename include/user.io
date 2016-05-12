///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//   PROGRAM:   user.io                                                      //
//                                                                           //
//    AUTHOR:   Mandie Lyons (mlyons@adjacency.net)                          //
//                                                                           //
//      DATE:   02 NOV 2004                                                  //
//                                                                           //
// COPYRIGHT:   2002-2004 Adjacency Consulting Group, Inc.                   //
//              All rights reserved.                                         //
//                                                                           //
//  PURPOSE:    Log users into NIN                                           //
//                                                                           //
// REVISION:    VER01   02 NOV 2004 MLYONS    Created                        //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
//    %ifndef generateGUID
//        include guidgen.inc
//    %endif
///////////////////////////////////////////////////////////////////////////////
fUserFiles      filelist
fUserAfile      afile   name="user",fixed=512
fUserIFile      ifile   name="user",fixed=512
fUser2IFile     ifile   name="user2",fixed=512
fUser3IFile     ifile   name="user3",fixed=512
fUser4IFile     ifile   name="user4",fixed=512
                filelistend

fUserAKeys      list
fUserAKey1      dim     35  // fName
fUserAKey2      dim     35  // lName
fUserAKey3      dim     35  // Organization
fUserAKey4      dim     13  // Zip
                listend

fUserIKey       dim     36  // userID
fUser2IKey      dim     48  // username,userID
fUser3IKey      dim     96  // email,userID
fUser4IKey      dim     37  // userType,userID

userErrorFlag   integer 1

fUserAamdexCmd  init    "sunadxnt User,User,1512;37-68,69-100,101-132,247-256"
fUserIndexCmd   init    "sunidxnt User,User,l512;e,n,1-36"
fUser2IndexCmd  init    "sunidxnt User,User2,l512;e,n,401-412,1-36"
fUser3IndexCmd  init    "sunidxnt User,User3,l512;e,n,277-336,1-36"
fUser4IndexCmd  init    "sunidxnt User,User4,l512;e,n,425,1-36"

fUserIO         record
userID          dim     36      // 001-036
fName           dim     32      // 037-068
lName           dim     32      // 069-100
Organization    dim     32      // 101-132
Address         dim     80      // 133-212
City            dim     32      // 213-244
State           dim     2       // 245-246
Zip             dim     10      // 247-256
Telephone       dim     10      // 257-266
Fax             dim     10      // 267-276
Email           dim     60      // 277-336   // changed to 60 bytes
compFunction    dim     32      // 337-368
Area            dim     32      // 369-400
username        dim     12      // 401-412
password        dim     12      // 413-424
userType        dim     1       // 425-425  // (G-Public,C-Client,A-Admin,P-Pending,S-Suspended)
dateTime        dim     16      // 426-441
companyNumber   dim     6       // 442-447  // foreign key: see nowndd.inc
invoiceFlag     dim     1       // 448-448  // (Y or Blank)
.START PATCH REPLACED LOGIC
.filler          dim     65      // 449-512
tos		dim	1	// 449-449
filler          dim     63      // 450-512
.END PATCH REPLACED LOGIC
                recordend

savedFUserIO    record like fUserIO

#fileManagerSettings    dim     260
///////////////////////////////////////////////////////////////////////////////
    goto #endOfInclude
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUserOpen

    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""
    open fUserFiles
    setmode *openuseip=#fileManagerSettings

    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUserOpenReadOnly

    getmode *openuseip=#fileManagerSettings
    setmode *openuseip=""
    open fUserFiles,read
    setmode *openuseip=#fileManagerSettings

    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUserClose

    close fUserFiles
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
fUserClearAKeys

    clear fUserAKeys
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
fUserARead

    read fUserAFile,fUserAKeys;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUserAReadKG

    readkg fUserAFile;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUserAReadKGP

    readkgp fUserAFile;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
fUserIRead

    read fUserIFile,fUserIKey;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUserIReadKS

    readks fUserIFile;*LL,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUserIReadKP

    readkp fUserIFile;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUser2IRead

    read fUser2IFile,fUser2IKey;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUser2IReadKS

    readks fUser2IFile;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUser2IReadKP

    readkp fUser2IFile;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUser3IRead

    read fUser3IFile,fUser3IKey;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUser3IReadKS

    readks fUser3IFile;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUser3IReadKP

    readkp fUser3IFile;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUser4IRead

    read fUser4IFile,fUser4IKey;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUser4IReadKS

    readks fUser4IFile;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUser4IReadKP

    readkp fUser4IFile;*ll,fUserIO
    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUserWrite

    call generateGUID using fUserIO.userID
    write fUserFiles;fUserIO

    return
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUserUpdate

    update fUserFiles;fUserIO

    return
///////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
fUserDelete

    delete fUserFiles
    return
//////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
fUserIOerror

    set userErrorFlag
    return
//////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
fUserIOClearKeys

    clear fUserIKey
    clear fUser2IKey
    clear fUser3IKey
    clear fUser4IKey

    return
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
#endOfInclude
///////////////////////////////////////////////////////////////////////////////

