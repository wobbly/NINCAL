;..............................................................................
;
; NORD6IO INCLUSION
; NIN RECORD NUMBER IO INCLUSION
;
; FILE NAME : NINORD6
;
;
;.............................................................................
;
NORD6KEY
	BRANCH	NORD6FLAG OF NORD61
         CALL      NORD6OPEN
NORD61   TRAP      IOMssg Giving Error if IO
         branch    NORD6lock to NOR61L,NOR61R,NOR61N
NOR61L   FILEPI    1;NORD6FILE
         READ      NORD6FILE,NORD6FLD;ORD6VARS
         TRAPCLR   IO
         RETURN
NOR61R  
         READLK    NORD6FILE,NORD6FLD;ORD6VARS
         TRAPCLR   IO
         RETURN
NOR61N
         READ      NORD6FILE,NORD6FLD;ORD6VARS
         TRAPCLR   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : NORD6TST
; REQUIRED    : NORD6FLD
; RETURNED    :
; DESCRIPTION : TEST KEY
;
NORD6TST  BRANCH    NORD6FLAG TO NORD62
         CALL      NORD6OPEN
NORD62   TRAP      IOMssg Giving Error if IO
         branch    NORD6lock to NOR62L,NOR62R,NOR62N
NOR62L   FILEPI    1;NORD6FILE
         READ      NORD6FILE,NORD6FLD;;
         TRAPCLR   IO
         RETURN
NOR62R  
         READLK    NORD6FILE,NORD6FLD;;
         TRAPCLR   IO
         RETURN
NOR62N
         READ      NORD6FILE,NORD6FLD;;
         TRAPCLR   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : NORD6KS
; REQUIRED    :
; RETURNED    : RECORD NUMBER RECORD
; DESCRIPTION : KEY SEQUENTIAL RECORD NUMBER FILE READ
;
NORD6KS   BRANCH    NORD6FLAG TO NORD63
         CALL      NORD6OPEN
NORD63   TRAP      IOMssg Giving Error if IO
         branch    NORD6lock to NOR63L,NOR63R,NOR63N
NOR63L   FILEPI    1;NORD6FILE
         READKS    NORD6FILE;ORD6VARS
         TRAPCLR   IO
         RETURN
NOR63R  
         READKSLK    NORD6FILE;ORD6VARS
         TRAPCLR     IO
         RETURN
NOR63N
         READKS    NORD6FILE;ORD6VARS
         TRAPCLR   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : NORD6SEQ
; REQUIRED    :
; RETURNED    : RECORD NUMBER RECORD
; DESCRIPTION : SEQUENTIAL RECORD NUMBER FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
NORD6SEQ  BRANCH    NORD6FLAG TO NORD64
         CALL      NORD6OPEN
NORD64   TRAP      IOMssg Giving Error if IO
         branch    NORD6lock to NOR64L,NOR64R,NOR64N
NOR64L   FILEPI    1;NORD6FILE
         READ      NORD6FILE,SEQ;ORD6VARS
         TRAPCLR   IO
         RETURN
NOR64R  
         READLK    NORD6FILE,SEQ;ORD6VARS
         TRAPCLR   IO
         RETURN
NOR64N
         READ      NORD6FILE,SEQ;ORD6VARS
         TRAPCLR   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : NORD6WRT
; REQUIRED    : 'NORD6FLD'
; RETURNED    :
; DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
;
NORD6WRT  BRANCH    NORD6FLAG TO NORD65
          CALL      NORD6OPEN
NORD65   TRAP      IOMssg Giving Error if IO 
	 FILEPI    1;NORD6FILE
         WRITE     NORD6FILE,NORD6FLD;ORD6VARS
	 trapclr   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : NORD6UPD
; REQUIRED    : A PREVIOUS KEY READ
; RETURNED    :
; DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
;
NORD6UPD  BRANCH    NORD6FLAG TO NORD66
          CALL      NORD6OPEN
NORD66   TRAP      IOMssg Giving Error if IO
	 FILEPI    1;NORD6FILE
         UPDATE    NORD6FILE;ORD6VARS
	 trapclr   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : NORD6DEL
; REQUIRED    : 'NORD6FLD'
; RETURNED    :
; DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
;
NORD6DEL  BRANCH    NORD6FLAG TO NORD67
         CALL      NORD6OPEN
NORD67   TRAP      IOMssg Giving Error if IO
	 FILEPI    1;NORD6FILE
         DELETE    NORD6FILE,NORD6FLD
	 trapclr   IO
         RETURN
;...............................................................................
;
; ENTRY POINT : NORD6OPEN
; REQUIRED    : 'NORD6FLAG'
; RETURNED    : 'NORD6FLAG' SET TO '1' IF OPENNED
; DESCRIPTION : OPEN NIN RECORD NUMBER FILE
;               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
;
NORD6OPEN TRAP      NORD6GONE IF IO
         OPEN      NORD6FILE,NORD6NAME
         TRAPCLR   IO
         MOVE      C1 TO NORD6FLAG
         RETURN
;
NORD6GONE MOVE      NORD6NAME TO FILENAME
          CALL      FILEGONE
;
;.............................................................................

