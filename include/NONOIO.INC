******************************************************************************
* NONODD.INC - ORDER NOTES DATA INCLUSION                                    *
* NIN ORDER NOTES FILE DESCRIPTION INCLUSION.
. REVISED 10FEB92   DLH.
.
. FILE NAME : ORDNOTES
. REC LENGTH: 521 VARIABLE
. INDEX KEY : 1-6 (LIST RENTAL NUMBER FROM ORDER)
.
. START PATCH 1/8/2003 ASH - ADDED IO TRAPS
.
******************************************************************************
.
. ENTRY POINT : NONOKEY
. REQUIRED    : 'NONOFLD'
. RETURNED    : ORDER NOTES RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NONOKEY	 BRANCH    NONOFLAG TO NONO1
	 CALL      NONOOPEN
NONO1
	 TRAP      IOMssg Giving Error if IO
	 FILEPI    1;NONOFILE
.START PATCH	1.2	REPLACED LOGIC
.         READ      NONOFILE,NONOFLD;NOTEKEY,NDATE,NTIME,NLINE1,NLINE2,NLINE3:
.                   NLINE4,NLINE5,NLINE6,NINITS
         READ      NONOFILE,NONOFLD;ORDNOTESVARS
.END PATCH	1.2	REPLACED LOGIC
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NONOTST
. REQUIRED    : NONOFLD. RETURNED    :
. DESCRIPTION : TEST KEY
.
NONOTST  BRANCH    NONOFLAG TO NONO2
         CALL      NONOOPEN
NONO2
	 TRAP      IOMssg Giving Error if IO
         FILEPI    1;NONOFILE
         READ      NONOFILE,NONOFLD;STR1
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NONOKS
. REQUIRED    :
. RETURNED    : ORDER NOTES RECORD
. DESCRIPTION : KEY SEQUENTIAL MAILER FILE READ
.
..............................................................................
NONOKS	 BRANCH    NONOFLAG TO NONO3
	 CALL      NONOOPEN
NONO3
	 TRAP      IOMssg Giving Error if IO
         FILEPI    1;NONOFILE
.START PATCH	1.2	REPLACED LOGIC
.         READKS    NONOFILE;NOTEKEY,NDATE,NTIME,NLINE1,NLINE2,NLINE3:
.                   NLINE4,NLINE5,NLINE6,NINITS
	 READKS    NONOFILE;ORDNOTESVARS
.END PATCH	1.2	REPLACED LOGIC
   	 TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NONOSEQ
. REQUIRED    :
. RETURNED    : ORDER NOTES RECORD
. DESCRIPTION : SEQUENTIAL ORDER NOTES FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NONOSEQ  BRANCH    NONOFLAG TO NONO4
         CALL      NONOOPEN
NONO4
	 TRAP      IOMssg Giving Error if IO
         FILEPI    1;NONOFILE
.START PATCH	1.2	REPLACED LOGIC
.         READ      NONOFILE,SEQ;NOTEKEY,NDATE,NTIME,NLINE1,NLINE2,NLINE3:
.                   NLINE4,NLINE5,NLINE6,NINITS
          READ      NONOFILE,SEQ;ORDNOTESVARS
.END PATCH	1.2	REPLACED LOGIC
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NONOWRT
. REQUIRED    : NONOFLD
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI WRITE.
.
NONOWRT	 BRANCH    NONOFLAG TO NONO5
	 CALL      NONOOPEN
NONO5
	 TRAP      IOMssg Giving Error if IO
 	 FILEPI    1;NONOFILE
.START PATCH	1.2	REPLACED LOGIC
.         WRITE     NONOFILE,NONOFLD;*+,NOTEKEY,NDATE,NTIME,NLINE1,NLINE2,NLINE3:
.                   NLINE4,NLINE5,NLINE6,NINITS
	 WRITE      NONOFILE,NONOFLD;ORDNOTESVARS
.END PATCH	1.2	REPLACED LOGIC
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NONODEL
. REQUIRED    : 'NONOFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NONODEL  BRANCH    NONOFLAG TO NONO7
         CALL      NONOOPEN
NONO7
	 TRAP      IOMssg Giving Error if IO
         FILEPI    1;NONOFILE
         DELETE    NONOFILE,NONOFLD
         TRAPCLR   IO
         RETURN
..............................................................................
. ENTRY POINT : NONOKP
. REQUIRED    : PREVIOUS VALID ISI READ
. RETURNED    : ORDER NOTES RECORD
. DESCRIPTION : ISI READ KEY PRIOR SEQ.
.
NONOKP   BRANCH    NONOFLAG TO NONO8
	 CALL      NONOOPEN
NONO8
	 TRAP      IOMssg Giving Error if IO
         FILEPI    1;NONOFILE
.START PATCH	1.2	REPLACED LOGIC
.         READKP    NONOFILE;NOTEKEY,NDATE,NTIME,NLINE1,NLINE2,NLINE3:
.                   NLINE4,NLINE5,NLINE6,NINITS
	 READKP	   NONOFILE;ORDNOTESVARS
.END PATCH	1.2	REPLACED LOGIC
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NONOOPEN
. REQUIRED    : 'NONOFLAG'
. RETURNED    : 'NONOFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN ORDER NOTES FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NONOOPEN TRAP      NONOGONE giving Error IF IO
         OPEN      NONOFILE,NONONAME
         TRAPCLR   IO
         MOVE      C1 TO NONOFLAG
         RETURN
.
NONOGONE MOVE      NONONAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
