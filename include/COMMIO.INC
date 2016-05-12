. ..............................................................................
.
. CommIO INCLUSION
.
. FILE NAME : Commission.DAT
. REC LENGTH: 82
. INDEX KEY:  1-9
..............................................................................
.
. ENTRY POINT : CommKEY
. REQUIRED    : CommFLD
. RETURNED    : CommVARS
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
CommKEY BRANCH    CommFLAG TO Comm1
        CALL      CommOPEN

Comm1   trap      IOMssg giving Error if IO
        branch    Commlock of Comm1L,Comm1R,Comm1N
.Default Filepi's active
Comm1L  FILEPI    1;CommIFILE
        READ      CommIFILE,CommFLD;CommVARS
        trapclr   IO
        RETURN
.with record locks . note file open must have had flag set
Comm1R
         READLK    CommIFILE,CommFLD;CommVARS
         trapclr  IO
         RETURN
.No locks
Comm1N
         READ      CommIFILE,CommFLD;CommVARS
         trapclr  IO
         RETURN
..............................................................................
.
. ENTRY POINT : CommTST
. REQUIRED    : CommFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
CommTST BRANCH    CommFLAG TO Comm2
        CALL      CommOPEN
Comm2   trap      IOMssg giving Error if IO
        FILEPI    1;CommIFILE
        READ      CommIFILE,CommFLD;;
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : CommKS
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : KEY SEQUENTIAL FILE READ
.
CommKS  BRANCH    CommFLAG TO Comm3
        CALL      CommOPEN
Comm3   trap      IOMssg giving Error if IO
        BRANCH    CommLOCK OF Comm3L,Comm3R,Comm3N

Comm3L  FILEPI    1;CommIFILE
        READKS    CommIFILE;CommVARS
        trapclr   IO
        RETURN
Comm3R
        READKSLK  CommIFILE;CommVARS
        trapclr   IO
        RETURN
Comm3N
        READKS    CommIFILE;CommVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : CommSEQ
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : SEQUENTIAL CHECK FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
CommSEQ BRANCH    CommFlg2 TO Comm4
        CALL      CommOPn2
Comm4   trap      IOMssg giving Error if IO
        BRANCH    CommLOCK OF Comm4L,Comm4R,Comm4N
Comm4L  FILEPI    1;CommFile
        READ      CommFile,SEQ;CommVARS
        trapclr   IO
        RETURN
Comm4R
        READLK    CommFile,SEQ;CommVARS
        trapclr   IO
        RETURN
Comm4N
        READ      CommFile,SEQ;CommVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : CommWRT
. REQUIRED    : CommFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
CommWRT BRANCH    CommFlag OF Comm5
.CommWRT BRANCH    CommFLAG OF Comm5
        CALL      CommOPEn
.        CALL      CommOPEN
Comm5   trap      IOMssg giving Error if IO
        FILEPI    1;CommIFile
        WRITE     CommiFile;CommVARS
.        FILEPI    1;CommIFILE
.        WRITE     CommIFILE;CommVARS
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : CommUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE ORDER FILE
.
CommUPD  COMPARE   C1 TO CommFlag
         GOTO      Comm6 IF EQUAL
         CALL      CommOPEn
Comm6    TRAP      IOMssg Giving Error if IO
         FILEPI    1;CommIfile
         UPDATE    CommIfile;CommVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : CommDEL
. REQUIRED    : CommFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
CommDEL BRANCH    CommFlag TO Comm7
        CALL      CommOPEn
Comm7   trap      IOMssg giving Error if IO
        FILEPI    1;CommIfile
        DELETE    CommIfile
        trapclr   IO
        RETURN
..............................................................................
.
. ENTRY POINT : CommOPEN
. REQUIRED    : 'CommFLAG'
. RETURNED    : 'CommFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN REVENUE FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
CommOPEN
        TRAP      CommGONE IF IO
        BRANCH    CommLOCK TO CommOPNL,CommOPNR

CommOPNL

        OPEN      CommIfile,CommIName
        TRAPCLR   IO
        MOVE      C1 TO CommFLAG
        RETURN
CommOPNR
        OPEN      CommIFile,CommIName,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO CommFLAG
        RETURN
..............................................................................
.
. ENTRY POINT : CommOPN2
. REQUIRED    : 'CommFLG2'
. RETURNED    : 'CommFLG2' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
CommOPN2
        TRAP      CommGONE IF IO
        BRANCH    CommLOCK TO CommOPN2L,CommOPN2R

CommOPN2L

        OPEN      COMMFILE,CommName
        TRAPCLR   IO
        MOVE      C1 TO CommFLG2
        RETURN
CommOPN2R
        OPEN      CommFile,Commname,LOCKMANUAL,SINGLE
        TRAPCLR   IO
        MOVE      C1 TO CommFLG2
        RETURN
..............................................................................
CommGONE
         MOVE     CommNAME TO FILENAME
         CALL     FILEGONE
.
..............................................................................
