................................................................................
.                                                                             .
.NSMPIO.INC - IO MODULE FOR DATABUS PORT/SMPR IDENTIFICATION.           .
...............................................................................
. RELEASE 1.1 ADDED AIM FILE
.
. FILENAME:     NINSAML
. LENGTH:       93 FIXED
. ACCESS:       ISI
. KEY   :       1-4    (MAILER NUMBER)
.               5-7    (SAMPLE NUMBER)
.START PATCH 1.1 ADDED LOGIC
.               1-4    (MAILER NUMBER) AIMKEY
.END PATCH 1.1 ADDED LOGIC
..............................................................................
.CREATED 2 AUGUST 93.
...............................................................................
.
. ENTRY POINT : NSMPKEY
. REQUIRED    : 'NSMPFLD'
. RETURNED    : SAMPLE RECORD RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSMPKEY  BRANCH    NSMPFLAG TO NSMP1
         CALL      NSMPOPEN
NSMP1    trap      IOMssg giving Error if IO
         branch    NSMPLOCK to NSMP1L,NSMP1R,NSMP1N
NSMP1L   FILEPI    1;NSMPFILE
         READ      NSMPFILE,NSMPFLD;NSMPVARS
         trapclr   IO
         RETURN
NSMP1R   READLK    NSMPFILE,NSMPFLD;NSMPVARS
         trapclr   IO
         RETURN
NSMP1N   READ      NSMPFILE,NSMPFLD;NSMPVARS
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NSMPTST
. REQUIRED    : NSMPFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NSMPTST  BRANCH    NSMPFLAG TO NSMP2
         CALL      NSMPOPEN
NSMP2    trap      IOMssg giving Error if IO
         branch    NSMPLOCK to NSMP2L,NSMP2R,NSMP2N
NSMP2L   FILEPI    1;NSMPFILE
         READ      NSMPFILE,NSMPFLD;;
         trapclr   IO
         RETURN
NSMP2R   READLK    NSMPFILE,NSMPFLD;;
         trapclr   IO
         RETURN
NSMP2N   READ      NSMPFILE,NSMPFLD;;
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NSMPKS
. REQUIRED    :
. RETURNED    : SAMPLE RECORD RECORD
. DESCRIPTION : KEY SEQUENTIAL MAILER FILE READ
.
NSMPKS   BRANCH    NSMPFLAG TO NSMP3
         CALL      NSMPOPEN
NSMP3    trap      IOMssg giving Error if IO
         branch    NSMPLOCK to NSMP3L,NSMP3R,NSMP3N
NSMP3L   FILEPI    1;NSMPFILE
         READKS    NSMPFILE;NSMPVARS
         trapclr   IO
         RETURN
NSMP3R   READKSLK  NSMPFILE;NSMPVARS
         trapclr   IO
         RETURN
NSMP3N   READKS    NSMPFILE;NSMPVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSMPSEQ
. REQUIRED    :
. RETURNED    : SAMPLE RECORD RECORD
. DESCRIPTION : SEQUENTIAL SMPR FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NSMPSEQ  BRANCH    NSMPFLAG TO NSMP4
         CALL      NSMPOPEN
NSMP4    trap      IOMssg giving Error if IO
         branch    NSMPLOCK to NSMP4L,NSMP4R,NSMP4N
NSMP4L   FILEPI    1;NSMPFILE
         READ      NSMPFILE,SEQ;NSMPVARS
         trapclr   IO
         RETURN
NSMP4R   READLK    NSMPFILE,SEQ;NSMPVARS
         trapclr   IO
         RETURN
NSMP4N   READ      NSMPFILE,SEQ;NSMPVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NSMPWRT
. REQUIRED    : 'NSMPFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NSMPWRT  BRANCH    NSMPFLAG TO NSMP5      C1 TO NSMPPATH
         CALL      NSMPOPEN
NSMP5    trap      IOMssg giving Error if IO
.START PATCH 1.1 REPLACED LOGIC
.         FILEPI    1;NSMPFILE
.         WRITE     NSMPFILE,NSMPFLD;NSMPVARS
	FILEPI    1;NSMPFLIST
	WRITE     NSMPFLIST;NSMPVARS
.END PATCH 1.1 REPLACED LOGIC
         trapclr   IO
         RETURN
.......................
.
. ENTRY POINT : NSMPUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SMPR FILE
.
NSMPUPD  BRANCH     NSMPFLAG TO NSMP6
         CALL       NSMPOPEN
NSMP6    trap       IOMssg giving Error if IO
.START PATCH 1.1 REPLACED LOGIC
.         FILEPI     1;NSMPFILE
.         UPDATE     NSMPFILE;NSMPVARS
         FILEPI     1;NSMPFLIST
         UPDATE     NSMPFLIST;NSMPVARS
.END PATCH 1.1 REPLACED LOGIC
         trapclr    IO
         RETURN
...............................................................................
.
. ENTRY POINT : NSMPDEL
. REQUIRED    : 'NSMPFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NSMPDEL  BRANCH    NSMPFLAG TO NSMP7
         CALL      NSMPOPEN
NSMP7    trap      IOMssg giving Error if IO
.START PATCH 1.1 REPLACED LOGIC
.         FILEPI    1;NSMPFILE
.         DELETE    NSMPFILE,NSMPFLD
         FILEPI    1;NSMPFLIST
         DELETE    NSMPFLIST
.END PATCH 1.1 REPLACED LOGIC
         trapclr   IO
         RETURN
;.............................................................................
;
; ENTRY POINT : NSMPAIM   
; REQUIRED    : NSMPFld1
; RETURNED    : MAILER SAMPLE RECORD
; DESCRIPTION : AIM MAILER SAMPLE FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
NSMPAIM
	BRANCH	NSMPFLAG,NSMP9
	CALL	NSMPOPEN
NSMP9
	TRAP	IOMssg Giving Error if IO
	BRANCH	NSMPLOCK,NSMP9L,NSMP9R,NSMP9N

NSMP9L
	FILEPI	1;NSMPFLE2
	READ	NSMPFLE2,NSMPFld1;NSMPVARS
	TRAPCLR	IO
	RETURN
NSMP9R 
	READLK	NSMPFLE2,NSMPFld1;NSMPVARS
	TRAPCLR	IO
	RETURN
NSMP9N
	READ	NSMPFLE2,NSMPFld1;NSMPVARS
	TRAPCLR	IO
	RETURN
;.............................................................................
;
; ENTRY POINT : NSMPKG
; REQUIRED    : VALID PREVIOUS AIM READ
; RETURNED    : MAILER SAMPLE RECORD
; DESCRIPTION : AIM KEY GENEREIC MAILER SAMPLE FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
NSMPKG
	BRANCH	NSMPFLAG,NSMP10
	CALL	NSMPOPEN
NSMP10
	TRAP	IOMssg Giving Error if IO
	BRANCH	NSMPLOCK,NSMP10L,NSMP10R,NSMP10N
NSMP10L
	FILEPI	1;NSMPFLE2
	READKG	NSMPFLE2;NSMPVARS
	TRAPCLR	IO
	RETURN
NSMP10R 
	READKGLK NSMPFLE2;NSMPVARS
	TRAPCLR	IO
	RETURN
NSMP10N
	READKG	NSMPFLE2;NSMPVARS
	TRAPCLR	IO
	RETURN
;.............................................................................
;
; ENTRY POINT : NSMPKGP
; REQUIRED    : PREVIOUS VALID AIM READ
; RETURNED    : MAILER SAMPLE RECORD
; DESCRIPTION : KEY GENERIC PRIOR MAILER SAMPLE FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
NSMPKGP
	BRANCH	NSMPFLAG,NSMP11
	CALL	NSMPOPEN
NSMP11
	TRAP	IOMssg Giving Error if IO
	BRANCH	NSMPLOCK TO NSMP11L,NSMP11R,NSMP11N
NSMP11L
	FILEPI	1;NSMPFLE2
	READKGP	NSMPFLE2;NSMPVARS
	TRAPCLR	IO
	RETURN
NSMP11R
	READKGPLK NSMPFLE2;NSMPVARS
	TRAPCLR	IO
	RETURN
NSMP11N
	READKGP	NSMPFLE2;NSMPVARS
	TRAPCLR	IO
	RETURN
...............................................................................
.
. ENTRY POINT : NSMPOPEN
. REQUIRED    : 'NSMPFLAG'
. RETURNED    : 'NSMPFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN NINSMPR FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NSMPOPEN
         TRAP      NSMPGONE IF IO
.START PATCH 1.1 REPLACED LOGIC
.         OPEN      NSMPFILE,NSMPNAME
         OPEN      NSMPFLIST
.END PATCH 1.1 REPLACED LOGIC
         TRAPCLR   IO
         MOVE      C1 TO NSMPFLAG
         RETURN
.
NSMPGONE MOVE      NSMPNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
...............................................................................
.END OF INCLUSION.                                                            .
...............................................................................

