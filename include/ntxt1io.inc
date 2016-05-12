..............................................................................
.
. Ntxt1IO INCLUSION
. NIN DATACARD TEXT FILE I/O ROUTINES
.
. FILE NAME : NINtxt1
. REC LENGTH: 510 VAR
. INDEX KEY : 1-9 (6 BYTE LIST NUMBER + 3 BYTE RECORD #)
.
..............................................................................
.
. ENTRY POINT : Ntxt1KEY
. REQUIRED    : 'Ntxt1FLD'
. RETURNED    : DATACARD TEXT RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
Ntxt1KEY  Branch    Ntxt1pATH OF NTXT11A,NTXT11C

NTXT11A   BRANCH    Ntxt1FLAG,Ntxt11B
          CALL      Ntxt1OPEN
Ntxt11B
          trap      IOMssg giving Error if IO
          FILEPI    1;Ntxt1FILE
          READ      Ntxt1FILE,Ntxt1FLD;Ntxt1VARS
          trapclr   IO
          RETURN

NTXT11C   BRANCH    Ntxt1FLAG,Ntxt11d
          CALL      Ntxt1OPEN
Ntxt11D
          trap      IOMssg giving Error if IO
          FILEPI    1;Ntxt1FLE1
          READ      Ntxt1FLE1,Ntxt1FLD1;Ntxt1VARS
          trapclr   IO
          RETURN
..............................................................................
.
. ENTRY POINT : Ntxt1TST
. REQUIRED    : Ntxt1FLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
Ntxt1TST  Branch    Ntxt1pATH OF NTXT12A,NTXT12C

Ntxt12A  BRANCH    Ntxt1FLAG,Ntxt12B
          CALL      Ntxt1OPEN
Ntxt12B
          trap      IOMssg giving Error if IO
          FILEPI    1;Ntxt1FILE
          READ      Ntxt1FILE,Ntxt1FLD;;
          trapclr   IO
          RETURN

Ntxt12C  BRANCH    Ntxt1FLAG,Ntxt12D
          CALL      Ntxt1OPEN
Ntxt12d
          trap      IOMssg giving Error if IO
          FILEPI    1;Ntxt1FLE1
          READ      Ntxt1FLE1,Ntxt1FLD1;;
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : Ntxt1KS
. REQUIRED    :
. RETURNED    : DATACARD TEXT RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD TEXT FILE READ
.
Ntxt1KS   Branch    Ntxt1pATH OF NTXT13A,NTXT13C
NTXT13A
          BRANCH    Ntxt1FLAG,Ntxt13B
          CALL      Ntxt1OPEN
Ntxt13B
          trap      IOMssg giving Error if IO
          FILEPI    1;Ntxt1FILE
          READKS    Ntxt1FILE;Ntxt1VARS
          trapclr   IO
          RETURN
NTXT13C
          BRANCH    Ntxt1FLAG,Ntxt13D
          CALL      Ntxt1OPEN
Ntxt13D
          trap      IOMssg giving Error if IO
          FILEPI    1;Ntxt1FLE1
          READKS    Ntxt1FLE1;Ntxt1VARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : Ntxt1SEQ
. REQUIRED    :
. RETURNED    : DATACARD TEXT RECORD
. DESCRIPTION : SEQUENTIAL DATACARD TEXT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
Ntxt1SEQ
          BRANCH    Ntxt1FLAG,Ntxt14
          CALL      Ntxt1OPEN
Ntxt14
          trap      IOMssg giving Error if IO
          FILEPI    1;Ntxt1FILE
          READ      Ntxt1FILE,SEQ;Ntxt1VARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : Ntxt1WRT
. REQUIRED    : 'Ntxt1FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY txt1/ISI INSERT
.
Ntxt1WRT
          BRANCH    Ntxt1FLAG,Ntxt15
          CALL      Ntxt1OPEN
Ntxt15
          trap      IOMssg giving Error if IO
          FILEPI    1;Ntxt1FLIST
          WRITE     Ntxt1FLIST;Ntxt1VARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : Ntxt1UPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
Ntxt1UPD
          BRANCH    Ntxt1FLAG,Ntxt16
          CALL      Ntxt1OPEN
Ntxt16
          trap      IOMssg giving Error if IO
          FILEPI    1;Ntxt1FLIST
          UPDATE    Ntxt1FLIST;Ntxt1VARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : Ntxt1DEL
. REQUIRED    : 'Ntxt1FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY txt1/ISI DELETE
.
Ntxt1DEL
          BRANCH    Ntxt1FLAG,Ntxt17
          CALL      Ntxt1OPEN
Ntxt17
          trap      IOMssg giving Error if IO
          FILEPI    1;Ntxt1FLIST
          DELETE    Ntxt1FLIST
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : Ntxt1OPEN
. REQUIRED    : 'Ntxt1FLAG'
. RETURNED    : 'Ntxt1FLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD TEXT FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
Ntxt1OPEN
          TRAP      Ntxt1GONE giving Error if IO
          OPEN      Ntxt1FLIST          .,EXCLUSIVE
          trapclr   IO
          MOVE      C1,Ntxt1FLAG
          RETURN
.
Ntxt1GONE
          MOVE      Ntxt1NAME,FILENAME
          CALL      FILEGONE
.
..............................................................................
