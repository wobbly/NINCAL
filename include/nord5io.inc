..............................................................................
.
. nord5IO INCLUSION
. NIN RECORD NUMBER IO INCLUSION
.
. FILE NAME : NInord5
.
. PATCH 1.1 - ADDED LOGIC FOR FILE LOCKING DECISIONS
.
..............................................................................
.
nord5KEY  BRANCH    nord5FLAG OF nord51
         CALL      nord5OPEN
.START PATCH 1.1 REPLACED LOGIC
.nord51    FILEPI    1;nord5FILE
.         READ      nord5FILE,nord5FLD;ord5vars
.         RETURN
nord51   TRAP      IOMssg Giving Error if IO
         branch    nord5lock to nor51L,nor51R,nor51N
nor51L   FILEPI    1;nord5FILE
         READ      nord5FILE,NORD5FLD;ORD5VARS
         TRAPCLR   IO
         RETURN
nor51R  
         READLK    nord5FILE,NORD5FLD;ORD5VARS
         TRAPCLR   IO
         RETURN
nor51N
         READ      nord5FILE,NORD5FLD;ORD5VARS
         TRAPCLR   IO
         RETURN
.END PATCH 1.1 REPLACED LOGIC
..............................................................................
.
. ENTRY POINT : nord5TST
. REQUIRED    : nord5FLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
nord5TST  BRANCH    nord5FLAG TO nord52
         CALL      nord5OPEN
.START PATCH 1.1 REPLACED LOGIC
.nord52    FILEPI    1;nord5FILE
.         READ      nord5FILE,nord5FLD;;
.         RETURN
nord52   TRAP      IOMssg Giving Error if IO
         branch    nord5lock to nor52L,nor52R,nor52N
nor52L   FILEPI    1;nord5FILE
         READ      nord5FILE,NORD5FLD;;
         TRAPCLR   IO
         RETURN
nor52R  
         READLK    nord5FILE,NORD5FLD;;
         TRAPCLR   IO
         RETURN
nor52N
         READ      nord5FILE,NORD5FLD;;
         TRAPCLR   IO
         RETURN
.END PATCH 1.1 REPLACED LOGIC
..............................................................................
.
. ENTRY POINT : nord5KS
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : KEY SEQUENTIAL RECORD NUMBER FILE READ
.
nord5KS   BRANCH    nord5FLAG TO nord53
         CALL      nord5OPEN
.START PATCH 1.1 REPLACED LOGIC
.nord53    FILEPI    1;nord5FILE
.         READKS    nord5FILE;nord5fld:
.                   ord5vars
.         RETURN
nord53   TRAP      IOMssg Giving Error if IO
         branch    nord5lock to nor53L,nor53R,nor53N
nor53L   FILEPI    1;nord5FILE
         READKS    nord5FILE;ord5vars
         TRAPCLR   IO
         RETURN
nor53R  
         READKSLK    nord5FILE;ord5vars
         TRAPCLR     IO
         RETURN
nor53N
         READKS    nord5FILE;ord5vars
         TRAPCLR   IO
         RETURN
.END PATCH 1.1 REPLACED LOGIC
..............................................................................
.
. ENTRY POINT : nord5SEQ
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : SEQUENTIAL RECORD NUMBER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
nord5SEQ  BRANCH    nord5FLAG TO nord54
         CALL      nord5OPEN
.START PATCH 1.1 REPLACED LOGIC
.nord54    FILEPI    1;nord5FILE
.         READ      nord5FILE,SEQ;ord5vars
.         RETURN
nord54   TRAP      IOMssg Giving Error if IO
         branch    nord5lock to nor54L,nor54R,nor54N
nor54L   FILEPI    1;nord5FILE
         READ      nord5FILE,SEQ;ord5vars
         TRAPCLR   IO
         RETURN
nor54R  
         READLK    nord5FILE,SEQ;ord5vars
         TRAPCLR   IO
         RETURN
nor54N
         READ      nord5FILE,SEQ;ord5vars
         TRAPCLR   IO
         RETURN
.END PATCH 1.1 REPLACED LOGIC
..............................................................................
.
. ENTRY POINT : nord5WRT
. REQUIRED    : 'nord5FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
nord5WRT  BRANCH    nord5FLAG TO nord55
          CALL      nord5OPEN
nord55   TRAP      IOMssg Giving Error if IO 
	 FILEPI    1;nord5FILE
         WRITE     nord5FILE,nord5FLD;ord5vars
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : nord5UPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
nord5UPD  BRANCH    nord5FLAG TO nord56
          CALL      nord5OPEN
nord56   TRAP      IOMssg Giving Error if IO
	 FILEPI    1;nord5FILE
         UPDATE    nord5FILE;ord5vars
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : nord5DEL
. REQUIRED    : 'nord5FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
nord5DEL  BRANCH    nord5FLAG TO nord57
         CALL      nord5OPEN
nord57   TRAP      IOMssg Giving Error if IO
	 FILEPI    1;nord5FILE
         DELETE    nord5FILE,nord5FLD
	 trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : nord5OPEN
. REQUIRED    : 'nord5FLAG'
. RETURNED    : 'nord5FLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN RECORD NUMBER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
nord5OPEN TRAP      nord5GONE IF IO
         OPEN      nord5FILE,nord5NAME
         TRAPCLR   IO
         MOVE      C1 TO nord5FLAG
         RETURN
.
nord5GONE MOVE      nord5NAME TO FILENAME
          CALL      FILEGONE
.
..............................................................................

