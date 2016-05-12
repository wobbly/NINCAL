..............................................................................
.
. NCALIO INCLUSION
. NIN List Management Broker Call File IO INCLUSION
.
. FILE NAME : NINCAL.DAT
.
..............................................................................
.
. ENTRY POINT : NCALKEY
. REQUIRED    : NCALFLD,NCALFLD1,NCALFLD2,NCALFLD3
. RETURNED    : CALVARS
. DESCRIPTION : TEST KEY FOR ISAM ONLY
.
.
NCALKEY  COMPARE   C1,NCALFLAG
         goto NCAL1A if equal
         call      NCALOPEN 
NCAL1A   trap      IOMssg giving Error if IO
         READ      NCALFILE,NCALFLD;CALVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCALTST
. REQUIRED    : NCALFLD,NCALFLD1,NCALFLD2,NCALFLD3
. RETURNED    :
. DESCRIPTION : TEST KEY FOR BOTH ISAM AND AAMDEX!!!
.
NCALTST  BRANCH    NCALPATH TO NCAL2A,NCAL2C
NCAL2A   COMPARE   C1,NCALFLAG
         goto NCAL2B if equal
         call      NCALOPEN 
NCAL2B   trap      IOMssg giving Error if IO
         READ      NCALFILE,NCALFLD;;
         goto NCAL2END
NCAL2C   COMPARE   C1,NCALFLAG
         goto NCAL2D if equal
         call      NCALOPEN 
NCAL2D   trap      IOMssg giving Error if IO
         READ      NCALFIL1,NCALFLD1,NCALFLD2,NCALFLD3;;
NCAL2END
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCALKS
. REQUIRED    : NCALFLD,NCALFLD1,NCALFLD2,NCALFLD3
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : KEY SEQUENTIAL RECORD NUMBER FILE READ
.
NCALKS   COMPARE   C1,NCALFLAG
         goto NCAL3A if equal
         call      NCALOPEN 
NCAL3A   trap      IOMssg giving Error if IO
         READKS    NCALFILE;NCALFLD:
                   CALVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCALSEQ
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : SEQUENTIAL RECORD NUMBER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCALSEQ  COMPARE   C1,NCALFLAG
         goto NCAL4A if equal
         call      NCALOPEN 
NCAL4A   trap      IOMssg giving Error if IO
         READ      NCALFILE,SEQ;CALVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCALSEQ0
. REQUIRED    :
. RETURNED    : RECORD NUMBER RECORD
. DESCRIPTION : SEQUENTIAL RECORD NUMBER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCALSEQ0 COMPARE   C1,NCALFLAG
         goto NCAL4XA if equal
         call      NCALOPEN 
NCAL4XA  trap      IOMssg giving Error if IO
         READ      NCALFILE,C0;CALVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCALWRT
. REQUIRED    : 'NCALFLD','NCALFLD1,NCALFLD2,NCALFLD3'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NCALWRT  MOVE      C1 TO NCALPATH
NCALWRT1 BRANCH    NCALFLAG TO NCAL5
         CALL      NCALOPEN
NCAL5    trap      IOMssg giving Error if IO
         FILEPI    1;CALFLIST
         WRITE     CALFLIST;CALVARS
.         WRITE     NCALFILE,NCALFLD;CALVARS
.         trapclr   IO
.         CALL      NCALWRT2
.         RETURN
.NCALWRT2 BRANCH    NCALFLG1 OF NCAL5B
.         MOVE      C2,NCALPATH
.         CALL      NCALOPEN
.NCAL5B   trap      IOMssg giving Error if IO
.         FILEPI    1;NCALFIL1
.         INSERT    NCALFIL1
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCALUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
.NCALUPD  BRANCH    NCALPATH TO NCAL6A,NCAL6C
.NCAL6A   COMPARE   C1,NCALFLAG
NCALUPD  COMPARE   C1,NCALFLAG
         goto NCAL6B if equal
         call      NCALOPEN 
NCAL6B   trap      IOMssg giving Error if IO
         FILEPI    1;CALFLIST
         UPDATE    CALFLIST;CALVARS
.         UPDATE    NCALFILE;CALVARS
.         goto NCAL6END
.NCAL6C   COMPARE   C1,NCALFLG1
.         goto NCAL6D if equal
.         call      NCALOPEN 
.NCAL6D   trap      IOMssg giving Error if IO
.         FILEPI    1;NCALFIL1
.         UPDATE    NCALFIL1;CALVARS
.NCAL6END
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCALDEL
. REQUIRED    : 'NCALFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NCALDEL  COMPARE   C1,NCALFLAG
         goto NCAL7B if equal
         call      NCALOPEN 
NCAL7B   trap      IOMssg giving Error if IO
         FILEPI    1;CALFLIST
         DELETE    CALFLIST
         trapclr   IO
         RETURN
..............................................................................
. ENTRY POINT : NCALAIM
. REQUIRED    : NCALFLD2
. RETURNED    : CONTACT RECORD
. DESCRIPTION : AIM CONTACT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCALAIM  BRANCH    NCALFLAG TO NCAL8
.         CALL      NCALOPN2
         CALL      NCALOPEN
NCAL8    trap      IOMssg giving Error if IO
         BRANCH    NCALLOCK TO NCAL8L,NCAL8R,NCAL8N

NCAL8L   FILEPI    1;NCALFIL1
         READ      NCALFIL1,NCALFLD1,NCALFLD2,NCALFLD3;CALVARS
         trapclr   IO
         RETURN
NCAL8R   
         READLK    NCALFIL1,NCALFLD1,NCALFLD2,NCALFLD3;CALVARS
         trapclr   IO
         RETURN         
NCAL8N   
         READ      NCALFIL1,NCALFLD1,NCALFLD2,NCALFLD3;CALVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NCALKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : MAILER RECORD
. DESCRIPTION : AIM KEY GENEREIC MAILER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCALKG   BRANCH    NCALFLAG TO NCAL10
.         CALL      NCALOPN2
         CALL      NCALOPEN
NCAL10   trap      IOMssg giving Error if IO
         BRANCH    NCALLOCK TO NCAL10L,NCAL10R,NCAL10N
NCAL10L  FILEPI    1;NCALFIL1
         READKG    NCALFIL1;CALVARS
         trapclr   IO
         RETURN
NCAL10R  
         READKGLK  NCALFIL1;CALVARS
         trapclr   IO
         RETURN         
NCAL10N  
         READKG    NCALFIL1;CALVARS
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NCALOPEN
. REQUIRED    : 'NCALFLAG'
. RETURNED    : 'NCALFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN RECORD NUMBER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NCALOPEN TRAP      NCALGONE giving error IF IO
         BRANCH    NCALLOCK TO NCALOP,NCALOPR,NCALOP
         
NCALOP   OPEN      CALFLIST
         TRAPCLR   IO
         MOVE      C1 TO NCALFLAG
         RETURN
NCALOPR  OPEN      CALFLIST,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NCALFLAG
         RETURN         
.NCALOPEN BRANCH    NCALPATH OF NCALOPN1,NCALOPN2
.NCALOPN1 TRAP      NCALGONE IF IO
.         BRANCH    NCALLOCK TO NCALOP,NCALOPR,NCALOP
.         
.NCALOP   OPEN      NCALFILE,NCALNAME
.         TRAPCLR   IO
.         MOVE      C1 TO NCALFLAG
.         RETURN
.NCALOPR  OPEN      NCALFILE,NCALNAME,LOCKMANUAL,SINGLE
.         TRAPCLR   IO
.         MOVE      C1 TO NCALFLAG
.         RETURN         
..
.NCALOPN2 TRAP      NCALGONE IF IO
.         BRANCH    NCALLOCK TO NCALOP2,NCALOP2R,NCALOP2
.         
.NCALOP2  OPEN      NCALFIL1,NCALNAME
.         TRAPCLR   IO
.         MOVE      C1 TO NCALFLG1
.         RETURN
.NCALOP2R OPEN      NCALFIL1,NCALNAME,LOCKMANUAL,SINGLE
.         TRAPCLR   IO
.         MOVE      C1 TO NCALFLG1
.         RETURN         
.
NCALGONE MOVE      NCALNAME TO FILENAME
         CALL      FILEGONE
..............................................................................

