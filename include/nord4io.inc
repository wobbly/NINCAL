..............................................................................
.
. nord4IO INCLUSION
. NIN RECORD NUMBER IO INCLUSION
.
. FILE NAME : NInord4
.
. PATCH 1.1 - ADDED LOGIC FOR FILE LOCKING DECISIONS
.
..............................................................................
.
nord4KEY  BRANCH    nord4FLAG OF nord41
         CALL      nord4OPEN
.START PATCH 1.1 REPLACED LOGIC
.nord41    FILEPI    1;nord4FILE
.         READ      nord4FILE,nord4FLD;ord4vars
.         RETURN
nord41   TRAP      IOMssg Giving Error if IO
         branch    nord4lock to nor41L,nor41R,nor41N
nor41L   FILEPI    1;nord4FILE
         READ      nord4FILE,NORD4FLD;ORD4VARS
         TRAPCLR   IO
         RETURN
nor41R  
         READLK      nord4FILE,NORD4FLD;ORD4VARS
         TRAPCLR     IO
         RETURN
nor41N
         READ      nord4FILE,NORD4FLD;ORD4VARS
         TRAPCLR   IO
         RETURN
.END PATCH 1.1 REPLACED LOGIC
..............................................................................
.
. ENTRY POINT : nord4TST
. REQUIRED    : nord4FLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
nord4TST  BRANCH    nord4FLAG TO nord42
         CALL      nord4OPEN
.START PATCH 1.1 REPLACED LOGIC
.nord42    FILEPI    1;nord4FILE
.         READ      nord4FILE,nord4FLD;STR1
.         RETURN
nord42   TRAP      IOMssg Giving Error if IO
         branch    nord4lock to nor42L,nor42R,nor42N
nor42L   FILEPI    1;nord4FILE
         READ      nord4FILE,NORD4FLD;STR1
         TRAPCLR   IO
         RETURN
nor42R  
         READLK      nord4FILE,NORD4FLD;STR1
         TRAPCLR     IO
         RETURN
nor42N
         READ      nord4FILE,NORD4FLD;STR1
         TRAPCLR   IO
         RETURN
.END PATCH 1.1 REPLACED LOGIC
..............................................................................
.
. ENTRY POINT : nord4KS
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : KEY SEQUENTIAL RECORD NUMBER FILE READ
.
nord4KS   BRANCH    nord4FLAG TO nord43
         CALL      nord4OPEN
.START PATCH 1.1 REPLACED LOGIC
.nord43    FILEPI    1;nord4FILE
.         READKS    nord4FILE;nord4fld:
.                   ord4vars
.         RETURN
nord43   TRAP      IOMssg Giving Error if IO
         branch    nord4lock to nor43L,nor43R,nor43N
nor43L   FILEPI    1;nord4FILE
         READKS    nord4FILE;ord4vars
         TRAPCLR   IO
         RETURN
nor43R  
         READKSLK  nord4FILE;ord4vars
         TRAPCLR   IO
         RETURN
nor43N
         READKS    nord4FILE;ord4vars
         TRAPCLR   IO
         RETURN
.END PATCH 1.1 REPLACED LOGIC
..............................................................................
.
. ENTRY POINT : nord4SEQ
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : SEQUENTIAL RECORD NUMBER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
nord4SEQ  BRANCH    nord4FLAG TO nord44
         CALL      nord4OPEN
.START PATCH 1.1 REPLACED LOGIC
.nord44    FILEPI    1;nord4FILE
.         READ      nord4FILE,SEQ;ord4vars
.         RETURN
nord44   TRAP      IOMssg Giving Error if IO
         branch    nord4lock to nor44L,nor44R,nor44N
nor44L   FILEPI    1;nord4FILE
         READ      nord4FILE,SEQ;ord4vars
         TRAPCLR   IO
         RETURN
nor44R  
         READLK    nord4FILE,SEQ;ord4vars
         TRAPCLR   IO
         RETURN
nor44N
         READ      nord4FILE,SEQ;ord4vars
         TRAPCLR   IO
         RETURN
.END PATCH 1.1 REPLACED LOGIC
..............................................................................
.
. ENTRY POINT : nord4WRT
. REQUIRED    : 'nord4FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
nord4WRT  BRANCH    nord4FLAG TO nord45
          CALL      nord4OPEN
nord45    FILEPI    1;nord4FILE
         WRITE     nord4FILE,nord4FLD;ord4vars
         RETURN
..............................................................................
.
. ENTRY POINT : nord4UPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
nord4UPD  BRANCH    nord4FLAG TO nord46
          CALL      nord4OPEN
nord46    FILEPI    1;nord4FILE
         UPDATE    nord4FILE;ord4vars
         RETURN
..............................................................................
.
. ENTRY POINT : nord4DEL
. REQUIRED    : 'nord4FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
nord4DEL  BRANCH    nord4FLAG TO nord47
         CALL      nord4OPEN
nord47    FILEPI    1;nord4FILE
         DELETE    nord4FILE,nord4FLD
         RETURN
...............................................................................
.
. ENTRY POINT : nord4OPEN
. REQUIRED    : 'nord4FLAG'
. RETURNED    : 'nord4FLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN RECORD NUMBER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
nord4OPEN TRAP      nord4GONE IF IO
         OPEN      nord4FILE,nord4NAME
         TRAPCLR   IO
         MOVE      C1 TO nord4FLAG
         RETURN
.
nord4GONE MOVE      nord4NAME TO FILENAME
          CALL      FILEGONE
.
..............................................................................

