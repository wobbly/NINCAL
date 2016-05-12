;..............................................................................
;.
;. CNCTIO INCLUSION
;. NIN Master Contact  FILE I/O ROUTINES
;.
;..............................................................................
;.
;. ENTRY POINT : CNCTKEY
;. REQUIRED    : 'CNCTFLD'
;. RETURNED    : Contact RECORD
;. DESCRIPTION : EXACT ISAM KEY READ
;.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;.
CNCTKEY
	BRANCH	CNCTFLAG,CNCT1
	CALL	CNCTOPEN
CNCT1
	TRAP	IOMssg Giving Error if IO
	BRANCH	CNCTLOCK,CNCT1L,CNCT1R,CNCT1N
CNCT1L
	FILEPI	1;CNCTFILE
	READ	CNCTFILE,CNCTFLD;CNCTVARS
	RETURN
CNCT1R    
	READLK	CNCTFILE,CNCTFLD;CNCTVARS
	RETURN
CNCT1N
	READ	CNCTFILE,CNCTFLD;CNCTVARS
	RETURN
;. ENTRY POINT : CNCTKEY2
;. REQUIRED    : 'CNCTFLD2'
;. RETURNED    : Contact RECORD
;. DESCRIPTION : EXACT ISAM KEY READ
;.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
CNCTKEY2
	BRANCH	CNCTFLAG,CNCT2
	CALL	CNCTOPEN
CNCT2
	TRAP	IOMssg Giving Error if IO
	BRANCH	CNCTLOCK,CNCT2l,CNCT2r,CNCT2n
CNCT2L
	FILEPI	1;CNCTFILE3
	READ	CNCTFILE3,CNCTFLD4;CNCTVARS
	RETURN
CNCT2R
	READLK	CNCTFILE3,CNCTFLD4;CNCTVARS
	RETURN
CNCT2N
	READ	CNCTFILE3,CNCTFLD4;CNCTVARS
	RETURN
;..............................................................................
;.
;. ENTRY POINT : CNCTTST
;. REQUIRED    : CNCTFLD
;. RETURNED    :
;. DESCRIPTION : TEST KEY
;.
CNCTTST
	BRANCH	CNCTFLAG,CNCT2T
	CALL	CNCTOPEN
CNCT2T
	TRAP	IOMssg Giving Error if IO
	BRANCH	CNCTLOCK,CNCT2LT,CNCT2RT,CNCT2NT
CNCT2LT
	FILEPI	1;CNCTFILE
	READ	CNCTFILE,CNCTFLD;;
	RETURN
CNCT2RT
	READLK	CNCTFILE,CNCTFLD;;
	RETURN
CNCT2NT
	READ	CNCTFILE,CNCTFLD;;
	RETURN
;patchdb
;..............................................................................
;.
;. ENTRY POINT : CNCTTST2
;. REQUIRED    : CNCTFLD
;. RETURNED    :
;. DESCRIPTION : TEST KEY
;.
CNCTTST2
	BRANCH	CNCTFLAG,CNCT2T2
	CALL	CNCTOPEN
CNCT2T2
	TRAP	IOMssg Giving Error if IO
	BRANCH	CNCTLOCK,CNCT2LT2,CNCT2RT2,CNCT2NT2
CNCT2LT2
	FILEPI	1;CNCTFILE
	READ	CNCTFILE3,CNCTFLD4;;
	RETURN
CNCT2RT2
	READLK	CNCTFILE3,CNCTFLD4;;
	RETURN
CNCT2NT2
	READ	CNCTFILE3,CNCTFLD4;;
	RETURN
;..............................................................................
;patchdb
;..............................................................................
;.
;. ENTRY POINT : CNCTKS
;. REQUIRED    :
;. RETURNED    : Contact RECORD
;. DESCRIPTION : KEY SEQUENTIAL Contact FILE READ
;.
CNCTKS
	BRANCH	CNCTFLAG,CNCT3
	CALL	CNCTOPEN
CNCT3
	TRAP	IOMssg Giving Error if IO
	BRANCH	CNCTLOCK,CNCT3L,CNCT3R,CNCT3N
CNCT3L
	FILEPI	1;CNCTFILE
	READKS	CNCTFILE;CNCTVARS
	RETURN
CNCT3R
	READKSLK CNCTFILE;CNCTVARS
	RETURN
CNCT3N
	READKS	CNCTFILE;CNCTVARS
	RETURN
;..............................................................................
;. ENTRY POINT : CNCTSEQ
;. REQUIRED    :
;. RETURNED    : Contact RECORD
;. DESCRIPTION : SEQUENTIAL Contact FILE READ
;.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
;.
CNCTSEQ
	BRANCH	CNCTFLAG,CNCT4
	CALL	CNCTOPEN
CNCT4
	TRAP	IOMssg Giving Error if IO
	BRANCH	CNCTLOCK,CNCT4L,CNCT4R,CNCT4N
CNCT4L
	FILEPI	1;CNCTFILE
	READ	CNCTFILE,SEQ;CNCTVARS
	RETURN
CNCT4r
	READLK	CNCTFILE,SEQ;CNCTVARS
	RETURN
CNCT4N
	READ	CNCTFILE,SEQ;CNCTVARS
	RETURN
;..............................................................................
;.
;. ENTRY POINT : CNCTWRT
;. REQUIRED    : 'CNCTFLD'
;. RETURNED    :
;. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
;.
CNCTWRT
	BRANCH	CNCTFLAG,CNCT5
	CALL	CNCTOPEN
CNCT5
	TRAP	IOMssg Giving Error if IO
	FILEPI	1;CNCTFLIST
	WRITE	CNCTFLIST;CNCTVARS
	RETURN
;..............................................................................
;.
;. ENTRY POINT : CNCTUPD
;. REQUIRED    : A PREVIOUS KEY READ
;. RETURNED    :
;. DESCRIPTION : KEY UPDATE Contact FILE
;.
CNCTUPD
	BRANCH	CNCTFLAG,CNCT6
	CALL	CNCTOPEN
CNCT6
	TRAP	IOMssg Giving Error if IO
	FILEPI	1;CNCTFLIST
	UPDATE	CNCTFLIST;CNCTVARS
	RETURN
;..............................................................................
;.
;. ENTRY POINT : CNCTDEL
;. REQUIRED    : 'CNCTFLD'
;. RETURNED    :
;. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
;.
CNCTDEL
	BRANCH	CNCTFLAG,CNCT7
	CALL	CNCTOPEN
CNCT7
	TRAP	IOMssg Giving Error if IO
	FILEPI	1;CNCTFLIST
	DELETE	CNCTFLIST
	RETURN
;.............................................................................
;
; ENTRY POINT : CNCTAIM   
; REQUIRED    : CNCTFld2,CNCTFLD3
; RETURNED    : MAILER RECORD
; DESCRIPTION : AIM MAILER FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
CNCTAIM
	BRANCH	CNCTFLAG,CNCT9
	CALL	CNCTOPEN
CNCT9
	TRAP	IOMssg Giving Error if IO
	BRANCH	CNCTLOCK TO CNCT9L,CNCT9R,CNCT9N
CNCT9L
	FILEPI	1;CNCTFILE2
	READ	CNCTFILE2,CNCTFLD2,CNCTFLD3;CNCTVARS
	TRAPCLR	IO
	RETURN
CNCT9R 
	READLK	CNCTFILE2,CNCTFLD2,CNCTFLD3;CNCTVARS
	TRAPCLR	IO
	RETURN
CNCT9N
	READ	CNCTFILE2,CNCTFLD2,CNCTFLD3;CNCTVARS
	TRAPCLR	IO
	RETURN
;.............................................................................
;
; ENTRY POINT : CNCTKG
; REQUIRED    : VALID PREVIOUS AIM READ
; RETURNED    : MAILER RECORD
; DESCRIPTION : AIM KEY GENEREIC MAILER FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
CNCTKG
	BRANCH	CNCTFLAG,CNCT10
	CALL	CNCTOPEN
CNCT10
	TRAP	IOMssg Giving Error if IO
	BRANCH	CNCTLOCK,CNCT10L,CNCT10R,CNCT10N
CNCT10L
	FILEPI	1;CNCTFILE2
	READKG	CNCTFILE2;CNCTVARS
	TRAPCLR	IO
	RETURN
CNCT10R 
	READKGLK CNCTFILE2;CNCTVARS
	TRAPCLR	IO
	RETURN
CNCT10N
	READKG	CNCTFILE2;CNCTVARS
	TRAPCLR	IO
	RETURN
;.............................................................................
;
; ENTRY POINT : CNCTKGP
; REQUIRED    : PREVIOUS VALID AIM READ
; RETURNED    : MAILER RECORD
; DESCRIPTION : KEY GENERIC PRIOR MAILER FILE READ
;               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
;
CNCTKGP
	BRANCH	CNCTFlag,CNCT11
	CALL	CNCTOpen
CNCT11
	TRAP	IOMssg Giving Error if IO
	BRANCH	CNCTLOCK,CNCT11L,CNCT11R,CNCT11N
CNCT11L
	FILEPI	1;CNCTFILE2
	READKGP	CNCTFILE2;CNCTVARS
	TRAPCLR	IO
	RETURN
CNCT11R
	READKGPLK CNCTFILE2;CNCTVARS
	TRAPCLR	IO
	RETURN
CNCT11N
	READKGP	CNCTFILE2;CNCTVARS
	TRAPCLR	IO
	RETURN
;.............................................................................
;
;..............................................................................
;. 
;. ENTRY POINT : CNCTOPEN
;. REQUIRED    : 'CNCTFLAG' 'CNCTPATH'
;. RETURNED    : 'CNCTFLAG' SET TO '1' IF OPENNED
;. DESCRIPTION : OPEN NIN Contact FILE
;.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
;.
CNCTOPEN
	TRAP	CNCTGONE IF IO
	OPEN	CNCTFLIST
	TRAPCLR	IO
	MOVE	C1,CNCTFLAG
	RETURN
;..............................................................................
CNCTGONE
	MOVE	CNCTNAME,FILENAME
	CALL	FILEGONE
;.
;..............................................................................