;..............................................................................
;
; nord7IO INCLUSION
; NIN RECORD NUMBER IO INCLUSION
;
; FILE NAME : NInord7
;
;
;.............................................................................
;
nord7KEY
	BRANCH	nord7FLAG OF nord71
         CALL      nord7OPEN
nord71   TRAP      IOMssg Giving Error if IO
         branch    nord7lock to NOR61L,NOR61R,NOR61N
NOR61L   FILEPI    1;nord7FILE
         READ      nord7FILE,nord7FLD;ord7VARS
         TRAPCLR   IO
         RETURN
NOR61R  
         READLK    nord7FILE,nord7FLD;ord7VARS
         TRAPCLR   IO
         RETURN
NOR61N
         READ      nord7FILE,nord7FLD;ord7VARS
         TRAPCLR   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : nord7TST
; REQUIRED    : nord7FLD
; RETURNED    :
; DESCRIPTION : TEST KEY
;
nord7TST  BRANCH    nord7FLAG TO nord72
         CALL      nord7OPEN
nord72   TRAP      IOMssg Giving Error if IO
         branch    nord7lock to NOR62L,NOR62R,NOR62N
NOR62L   FILEPI    1;nord7FILE
         READ      nord7FILE,nord7FLD;;
         TRAPCLR   IO
         RETURN
NOR62R  
         READLK    nord7FILE,nord7FLD;;
         TRAPCLR   IO
         RETURN
NOR62N
         READ      nord7FILE,nord7FLD;;
         TRAPCLR   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : nord7KS
; REQUIRED    :
; RETURNED    : RECORD NUMBER RECORD
; DESCRIPTION : KEY SEQUENTIAL RECORD NUMBER FILE READ
;
nord7KS   BRANCH    nord7FLAG TO nord73
         CALL      nord7OPEN
nord73   TRAP      IOMssg Giving Error if IO
         branch    nord7lock to NOR63L,NOR63R,NOR63N
NOR63L   FILEPI    1;nord7FILE
         READKS    nord7FILE;ord7VARS
         TRAPCLR   IO
         RETURN
NOR63R  
         READKSLK    nord7FILE;ord7VARS
         TRAPCLR     IO
         RETURN
NOR63N
         READKS    nord7FILE;ord7VARS
         TRAPCLR   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : nord7SEQ
; REQUIRED    :
; RETURNED    : RECORD NUMBER RECORD
; DESCRIPTION : SEQUENTIAL RECORD NUMBER FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
nord7SEQ  BRANCH    nord7FLAG TO nord74
         CALL      nord7OPEN
nord74   TRAP      IOMssg Giving Error if IO
         branch    nord7lock to NOR64L,NOR64R,NOR64N
NOR64L   FILEPI    1;nord7FILE
         READ      nord7FILE,SEQ;ord7VARS
         TRAPCLR   IO
         RETURN
NOR64R  
         READLK    nord7FILE,SEQ;ord7VARS
         TRAPCLR   IO
         RETURN
NOR64N
         READ      nord7FILE,SEQ;ord7VARS
         TRAPCLR   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : nord7WRT
; REQUIRED    : 'nord7FLD'
; RETURNED    :
; DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
;
nord7WRT  BRANCH    nord7FLAG TO nord75
          CALL      nord7OPEN
nord75   TRAP      IOMssg Giving Error if IO 
	 FILEPI    1;nord7FILE
         WRITE     nord7FILE,nord7FLD;ord7VARS
	 trapclr   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : nord7UPD
; REQUIRED    : A PREVIOUS KEY READ
; RETURNED    :
; DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
;
nord7UPD  BRANCH    nord7FLAG TO nord76
          CALL      nord7OPEN
nord76   TRAP      IOMssg Giving Error if IO
	 FILEPI    1;nord7FILE
         UPDATE    nord7FILE;ord7VARS
	 trapclr   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : nord7DEL
; REQUIRED    : 'nord7FLD'
; RETURNED    :
; DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
;
nord7DEL  BRANCH    nord7FLAG TO nord77
         CALL      nord7OPEN
nord77   TRAP      IOMssg Giving Error if IO
	 FILEPI    1;nord7FILE
         DELETE    nord7FILE,nord7FLD
	 trapclr   IO
         RETURN
;...............................................................................
;
; ENTRY POINT : nord7OPEN
; REQUIRED    : 'nord7FLAG'
; RETURNED    : 'nord7FLAG' SET TO '1' IF OPENNED
; DESCRIPTION : OPEN NIN RECORD NUMBER FILE
;               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
;
nord7OPEN TRAP      nord7GONE IF IO
         OPEN      nord7FILE,nord7NAME
         TRAPCLR   IO
         MOVE      C1 TO nord7FLAG
         RETURN
;
nord7GONE MOVE      nord7NAME TO FILENAME
          CALL      FILEGONE
;
;.............................................................................

