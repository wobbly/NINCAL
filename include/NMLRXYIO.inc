................................................................................
.                                                                             .
.NMLRXYIO.INC
...............................................................................
.
. FILENAME:     NINMLRXY
. LENGTH:       568 FIXED
. ACCESS:       ISI
. KEY   :       1-12    (MAILER NUMBER/MAILER NUMBER)
.
. AIMKEY:       1-12    (MAILER NUMBER/MAILER NUMBER) AIMKEY
..............................................................................
.CREATED 21 MAY 2004.
...............................................................................
.
. ENTRY POINT : NMLRXYKEY
. REQUIRED    : 'NMLRXYFLD'
. RETURNED    : MAILER/MAILER RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMLRXYKEY
         BRANCH    NMLRXYFLAG TO NMLRXY1
         CALL      NMLRXYOPEN
NMLRXY1
         trap      IOMssg giving Error if IO
         branch    NMLRXYLOCK to NMLRXY1L,NMLRXY1R,NMLRXY1N
NMLRXY1L 
         FILEPI    1;NMLRXYFILE
         READ      NMLRXYFILE,NMLRXYFLD;NMLRXYVARS
         trapclr   IO
         RETURN
NMLRXY1R
         READLK    NMLRXYFILE,NMLRXYFLD;NMLRXYVARS
         trapclr   IO
         RETURN
NMLRXY1N
         READ      NMLRXYFILE,NMLRXYFLD;NMLRXYVARS
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NMLRXYTST
. REQUIRED    : NMLRXYFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NMLRXYTST
         BRANCH    NMLRXYFLAG TO NMLRXY2
         CALL      NMLRXYOPEN
NMLRXY2
         trap      IOMssg giving Error if IO
         branch    NMLRXYLOCK to NMLRXY2L,NMLRXY2R,NMLRXY2N
NMLRXY2L
         FILEPI    1;NMLRXYFILE
         READ      NMLRXYFILE,NMLRXYFLD;;
         trapclr   IO
         RETURN
NMLRXY2R
         READLK    NMLRXYFILE,NMLRXYFLD;;
         trapclr   IO
         RETURN
NMLRXY2N
         READ      NMLRXYFILE,NMLRXYFLD;;
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NMLRXYKS
. REQUIRED    :
. RETURNED    : MAILER/MAILER RECORD
. DESCRIPTION : KEY SEQUENTIAL MAILER/MAILER FILE READ
.
NMLRXYKS
         BRANCH    NMLRXYFLAG TO NMLRXY3
         CALL      NMLRXYOPEN
NMLRXY3
         trap      IOMssg giving Error if IO
         branch    NMLRXYLOCK to NMLRXY3L,NMLRXY3R,NMLRXY3N
NMLRXY3L
         FILEPI    1;NMLRXYFILE
         READKS    NMLRXYFILE;NMLRXYVARS
         trapclr   IO
         RETURN
NMLRXY3R
         READKSLK  NMLRXYFILE;NMLRXYVARS
         trapclr   IO
         RETURN
NMLRXY3N
         READKS    NMLRXYFILE;NMLRXYVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRXYSEQ
. REQUIRED    :
. RETURNED    : MAILER/MAILER RECORD
. DESCRIPTION : SEQUENTIAL MAILER/MAILER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMLRXYSEQ
         BRANCH    NMLRXYFLAG TO NMLRXY4
         CALL      NMLRXYOPEN
NMLRXY4
         trap      IOMssg giving Error if IO
         branch    NMLRXYLOCK to NMLRXY4L,NMLRXY4R,NMLRXY4N
NMLRXY4L
         FILEPI    1;NMLRXYFILE
         READ      NMLRXYFILE,SEQ;NMLRXYVARS
         trapclr   IO
         RETURN
NMLRXY4R
         READLK    NMLRXYFILE,SEQ;NMLRXYVARS
         trapclr   IO
         RETURN
NMLRXY4N
         READ      NMLRXYFILE,SEQ;NMLRXYVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMLRXYWRT
. REQUIRED    : 'NMLRXYFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NMLRXYWRT
         BRANCH    NMLRXYFLAG TO NMLRXY5      C1 TO NMLRXYPATH
         CALL      NMLRXYOPEN
NMLRXY5
         trap      IOMssg giving Error if IO
         FILEPI    1;NMLRXYFLIST
         WRITE     NMLRXYFLIST;NMLRXYVARS
         trapclr   IO
         RETURN
.......................
.
. ENTRY POINT : NMLRXYUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE MLRXYR FILE
.
NMLRXYUPD
         BRANCH     NMLRXYFLAG TO NMLRXY6
         CALL       NMLRXYOPEN
NMLRXY6
         trap       IOMssg giving Error if IO
         FILEPI     1;NMLRXYFLIST
         UPDATE     NMLRXYFLIST;NMLRXYVARS
         trapclr    IO
         RETURN
...............................................................................
.
. ENTRY POINT : NMLRXYDEL
. REQUIRED    : 'NMLRXYFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NMLRXYDEL
         BRANCH    NMLRXYFLAG TO NMLRXY7
         CALL      NMLRXYOPEN
NMLRXY7
         trap      IOMssg giving Error if IO
         FILEPI    1;NMLRXYFLIST
         DELETE    NMLRXYFLIST
         trapclr   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : NMLRXYAIM   
; REQUIRED    : NMLRXYFld1
; RETURNED    : MAILER/MAILER RECORD
; DESCRIPTION : AIM MAILER/MAILER FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
NMLRXYAIM
	BRANCH	NMLRXYFLAG,NMLRXY9
	CALL	NMLRXYOPEN
NMLRXY9
	TRAP	IOMssg Giving Error if IO
	BRANCH	NMLRXYLOCK,NMLRXY9L,NMLRXY9R,NMLRXY9N
NMLRXY9L
	FILEPI	1;NMLRXYFLE2
	READ	NMLRXYFLE2,NMLRXYFld1;NMLRXYVARS
	TRAPCLR	IO
	RETURN
NMLRXY9R 
	READLK	NMLRXYFLE2,NMLRXYFld1;NMLRXYVARS
	TRAPCLR	IO
	RETURN
NMLRXY9N
	READ	NMLRXYFLE2,NMLRXYFld1;NMLRXYVARS
	TRAPCLR	IO
	RETURN
;.............................................................................
;
; ENTRY POINT : NMLRXYKG
; REQUIRED    : VALID PREVIOUS AIM READ
; RETURNED    : MAILER/MAILER RECORD
; DESCRIPTION : AIM KEY GENEREIC MAILER/MAILER FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
NMLRXYKG
	BRANCH	NMLRXYFLAG,NMLRXY10
	CALL	NMLRXYOPEN
NMLRXY10
	TRAP	IOMssg Giving Error if IO
	BRANCH	NMLRXYLOCK,NMLRXY10L,NMLRXY10R,NMLRXY10N
NMLRXY10L
	FILEPI	1;NMLRXYFLE2
	READKG	NMLRXYFLE2;NMLRXYVARS
	TRAPCLR	IO
	RETURN
NMLRXY10R 
	READKGLK NMLRXYFLE2;NMLRXYVARS
	TRAPCLR	IO
	RETURN
NMLRXY10N
	READKG	NMLRXYFLE2;NMLRXYVARS
	TRAPCLR	IO
	RETURN
;.............................................................................
;
; ENTRY POINT : NMLRXYKGP
; REQUIRED    : PREVIOUS VALID AIM READ
; RETURNED    : MAILER/MAILER RECORD
; DESCRIPTION : KEY GENERIC PRIOR MAILER/MAILER FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
NMLRXYKGP
	BRANCH	NMLRXYFLAG,NMLRXY11
	CALL	NMLRXYOPEN
NMLRXY11
	TRAP	IOMssg Giving Error if IO
	BRANCH	NMLRXYLOCK TO NMLRXY11L,NMLRXY11R,NMLRXY11N
NMLRXY11L
	FILEPI	1;NMLRXYFLE2
	READKGP	NMLRXYFLE2;NMLRXYVARS
	TRAPCLR	IO
	RETURN
NMLRXY11R
	READKGPLK NMLRXYFLE2;NMLRXYVARS
	TRAPCLR	IO
	RETURN
NMLRXY11N
	READKGP	NMLRXYFLE2;NMLRXYVARS
	TRAPCLR	IO
	RETURN
...............................................................................
.
. ENTRY POINT : NMLRXYOPEN
. REQUIRED    : 'NMLRXYFLAG'
. RETURNED    : 'NMLRXYFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MAILER/MAILER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NMLRXYOPEN
         TRAP      NMLRXYGONE IF IO
         OPEN      NMLRXYFLIST
         TRAPCLR   IO
         MOVE      C1 TO NMLRXYFLAG
         RETURN
.
NMLRXYGONE MOVE      NMLRXYNAME TO FILENAME
         CALL      FILEGONE
.
...............................................................................
.END OF INCLUSION.                                                            .
...............................................................................

