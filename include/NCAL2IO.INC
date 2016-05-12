..............................................................................
.
. NCAL2IO INCLUSION
. NIN List Management Broker Call Detail File IO INCLUSION
.
. FILE NAME : NINCAL2.DAT
.
..............................................................................
.
. ENTRY POINT : N2CALKEY
. REQUIRED    : NCAL2FLD,NCAL2FLD1,NCAL2FLD2
. RETURNED    : CAL2VARS
. DESCRIPTION : TEST KEY FOR ISAM ONLY
.
.
N2CALKEY  BRANCH    NCAL2PATH TO N2CAL1A,N2CAL1X,N2CAL1C,N2CAL1E
N2CAL1A   COMPARE   C1,NCAL2FLAG
          goto      N2CAL1B if equal
          call      N2CALOPEN 
N2CAL1B   trap      IOMssg giving Error if IO
          READ      NCAL2FILE,NCAL2FLD;CAL2VARS
          trapclr    IO
          goto      N2CAL1X
N2CAL1C   COMPARE   C1,NCAL2FLAG
          goto      N2CAL1D if equal
          call      N2CALOPEN 
N2CAL1D   trap      IOMssg giving Error if IO
          READ      NCAL2FIL3,NCAL2FLD3;CAL2VARS
          trapclr    IO
          goto      N2CAL1X
N2CAL1E   COMPARE   C1,NCAL2FLAG
          goto      N2CAL1F if equal
          call      N2CALOPEN 
N2CAL1F   trap      IOMssg giving Error if IO
          READ      NCAL2FIL4,NCAL2FLD4;CAL2VARS
          trapclr    IO
          goto      N2CAL1X
N2CAL1X   RETURN
..............................................................................
.
. ENTRY POINT : N2CALTST
. REQUIRED    : NCAL2FLD,NCAL2FLD1,NCAL2FLD2
. RETURNED    :
. DESCRIPTION : TEST KEY, ALLOWS TESTING OF AAMKEY!!!
.
N2CALTST BRANCH    NCAL2PATH TO N2CAL2A,N2CAL2C
N2CAL2A  COMPARE   C1,NCAL2FLAG
         goto N2CAL2B if equal
         call      N2CALOPEN 
N2CAL2B  trap      IOMssg giving Error if IO
         READ      NCAL2FILE,NCAL2FLD;;
         goto N2CAL2END
N2CAL2C  COMPARE   C1,NCAL2FLAG
         goto N2CAL2D if equal
         call      N2CALOPEN 
N2CAL2D  trap      IOMssg giving Error if IO
         READ      NCAL2FIL1,NCAL2FLD1,NCAL2FLD2;;
N2CAL2END
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : N2CALKS
. REQUIRED    : Previous valid read
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : KEY SEQUENTIAL RECORD NUMBER FILE READ
.
N2CALKS   BRANCH    NCAL2PATH TO N2CAL3A,N2CAL3X,N2CAL3C,N2CAL3E
N2CAL3A   COMPARE   C1,NCAL2FLAG
          goto      N2CAL3B if equal
          call      N2CALOPEN 
N2CAL3B   trap      IOMssg giving Error if IO
          READKS    NCAL2FILE;CAL2VARS
          trapclr   IO
          goto      N2CAL3X
N2CAL3C   COMPARE   C1,NCAL2FLAG
          goto      N2CAL3D if equal
          call      N2CALOPEN 
N2CAL3D   trap      IOMssg giving Error if IO
          READKS    NCAL2FIL3;CAL2VARS
          trapclr   IO
          goto      N2CAL3X
N2CAL3E   COMPARE   C1,NCAL2FLAG
          goto      N2CAL3F if equal
          call      N2CALOPEN 
N2CAL3F   trap      IOMssg giving Error if IO
          READKS    NCAL2FIL4;CAL2VARS
          trapclr   IO
          goto      N2CAL3X
N2CAL3X   RETURN
..............................................................................
.
. ENTRY POINT : N2CALSEQ
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : SEQUENTIAL RECORD NUMBER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
N2CALSEQ COMPARE   C1,NCAL2FLAG
         goto N2CAL4A if equal
         call      N2CALOPEN 
N2CAL4A  trap      IOMssg giving Error if IO
         READ      NCAL2FILE,SEQ;CAL2VARS
         RETURN
..............................................................................
.
. ENTRY POINT : N2CALSEQ0
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : SEQUENTIAL RECORD NUMBER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
N2CALSEQ0
         COMPARE   C1,NCAL2FLAG
         goto N2CAL4XA if equal
         call      N2CALOPEN 
N2CAL4XA trap      IOMssg giving Error if IO
         READ      NCAL2FILE,C0;CAL2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : N2CALWRT
. REQUIRED    : 'NCAL2FLD','NCAL2FLD1,NCAL2FLD2'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
N2CALWRT MOVE      C1 TO NCAL2PATH
N2CALWRT1
         BRANCH    NCAL2FLAG TO N2CAL5
         CALL      N2CALOPEN
N2CAL5   trap      IOMssg giving Error if IO
         FILEPI    1;CALFLIST2
         WRITE     CALFLIST2;CAL2VARS
         trapclr   IO
         return
.N2CALWRT MOVE      C1 TO NCAL2PATH
.N2CALWRT1
.         BRANCH    NCAL2FLAG TO N2CAL5
.         CALL      N2CALOPEN
.N2CAL5   trap      IOMssg giving Error if IO
.         FILEPI    1;NCAL2FILE
.         WRITE     NCAL2FILE,NCAL2FLD;CAL2VARS
.         trapclr   IO
.         CALL      N2CALWRT2
.         RETURN
.N2CALWRT2
.         BRANCH    NCAL2FLG1 OF N2CAL5B
.         MOVE      C2,NCAL2PATH
.         CALL      N2CALOPEN
.N2CAL5B  trap      IOMssg giving Error if IO
.         FILEPI    1;NCAL2FIL1
.         INSERT    NCAL2FIL1
.         trapclr   IO
.         RETURN
..............................................................................
.
. ENTRY POINT : N2CALUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
N2CALUPD COMPARE   C1,NCAL2FLAG
         goto N2CAL6B if equal
         call      N2CALOPEN
N2CAL6B  trap      IOMssg giving Error if IO
         FILEPI    1;CALFLIST2
         UPDATE    CALFLIST2;CAL2VARS
         trapclr   IO
         return

.N2CALUPD BRANCH    NCAL2PATH TO N2CAL6A,N2CAL6C
.N2CAL6A  COMPARE   C1,NCAL2FLAG
.         goto N2CAL6B if equal
.         call      N2CALOPEN 
.N2CAL6B  trap      IOMssg giving Error if IO
.         FILEPI    1;NCAL2FILE
.         UPDATE    NCAL2FILE;CAL2VARS
.         goto N2CAL6END
.N2CAL6C  COMPARE   C1,NCAL2FLG1
.         goto N2CAL6D if equal
.         call      N2CALOPEN 
.N2CAL6D  trap      IOMssg giving Error if IO
.         FILEPI    1;NCAL2FIL1
.         UPDATE    NCAL2FIL1;CAL2VARS
.N2CAL6END
.         trapclr   IO
.         RETURN
..............................................................................
.
. ENTRY POINT : N2CALDEL
. REQUIRED    : 'NCAL2FLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
N2CALDEL COMPARE   C1,NCAL2FLAG
         goto N2CAL7A if equal
         call      N2CALOPEN 
N2CAL7A  trap      IOMssg giving Error if IO
         FILEPI    1;CALFLIST2
         DELETE    CALFLIST2
         trapclr   IO
         RETURN
..............................................................................
. ENTRY POINT : N2CALAIM
. REQUIRED    : NCAL2FLD2
. RETURNED    : CONTACT RECORD
. DESCRIPTION : AIM CONTACT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
N2CALAIM BRANCH    NCAL2FLAG TO N2CAL8
         call      N2CALOPEN
.         CALL      N2CALOPN2
N2CAL8   trap      IOMssg giving Error if IO
         BRANCH    NCAL2LOCK TO N2CAL8L,N2CAL8R,N2CAL8N

N2CAL8L  FILEPI    1;NCAL2FIL1
         READ      NCAL2FIL1,NCAL2FLD1,NCAL2FLD2;CAL2VARS
         trapclr   IO
         RETURN
N2CAL8R   
         READLK    NCAL2FIL1,NCAL2FLD1,NCAL2FLD2;CAL2VARS
         trapclr   IO
         RETURN         
N2CAL8N   
         READ      NCAL2FIL1,NCAL2FLD1,NCAL2FLD2;CAL2VARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : N2CALKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : MAILER RECORD
. DESCRIPTION : AIM KEY GENEREIC MAILER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
N2CALKG  BRANCH    NCAL2FLAG TO N2CAL10
         CALL      N2CALOPEN
.         CALL      N2CALOPN2
N2CAL10  trap      IOMssg giving Error if IO
         BRANCH    NCAL2LOCK TO N2CAL10L,N2CAL10R,N2CAL10N
N2CAL10L FILEPI    1;NCAL2FIL1
         READKG    NCAL2FIL1;CAL2VARS
         trapclr   IO
         RETURN
N2CAL10R  
         READKGLK  NCAL2FIL1;CAL2VARS
         trapclr   IO
         RETURN         
N2CAL10N  
         READKG    NCAL2FIL1;CAL2VARS
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : N2CALOPEN
. REQUIRED    : 'NCAL2FLAG'
. RETURNED    : 'NCAL2FLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN RECORD NUMBER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
N2CALOPEN 
         TRAP      N2CALGONE IF IO
         BRANCH    NCAL2LOCK TO N2CALOP,N2CALOPR,N2CALOP
         
N2CALOP  OPEN      CALFLIST2
         TRAPCLR   IO
         MOVE      C1 TO NCAL2FLAG
         RETURN
N2CALOPR OPEN      CALFLIST2,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NCAL2FLAG
         RETURN         


.N2CALOPEN
.         BRANCH    NCAL2PATH OF N2CALOPN1,N2CALOPN2
.N2CALOPN1
.         TRAP      N2CALGONE IF IO
.         BRANCH    NCAL2LOCK TO N2CALOP,N2CALOPR,N2CALOP
.         
.N2CALOP  OPEN      NCAL2FILE,NCAL2NAME
.         TRAPCLR   IO
.         MOVE      C1 TO NCAL2FLAG
.         RETURN
.N2CALOPR OPEN      NCAL2FILE,NCAL2NAME,LOCKMANUAL,SINGLE
.         TRAPCLR   IO
.         MOVE      C1 TO NCAL2FLAG
.         RETURN         
..
.N2CALOPN2
.         TRAP      N2CALGONE IF IO
.         BRANCH    NCAL2LOCK TO N2CALOP2,N2CALOP2R,N2CALOP2
.         
.N2CALOP2 OPEN      NCAL2FIL1,NCAL2NAME
.         TRAPCLR   IO
.         MOVE      C1 TO NCAL2FLG1
.         RETURN
.N2CALOP2R
.         OPEN      NCAL2FIL1,NCAL2NAME,LOCKMANUAL,SINGLE
.         TRAPCLR   IO
.         MOVE      C1 TO NCAL2FLG1
.         RETURN         
.
N2CALGONE
         MOVE      NCAL2NAME TO FILENAME
         CALL      FILEGONE
..............................................................................

