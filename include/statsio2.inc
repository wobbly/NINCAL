..............................................................................
.
. statio INCLUSION
. Stats FILE DEFINITION
.
. FILE NAME : ninstat
. REC LENGTH:  501 FIXED
. INDEX KEY : 
.
.Patch 02/20/2002 - Added Aamdex for History Screen in Program 1 
..............................................................................
.
..............................................................................
.
. ENTRY POINT : stat2KEY
. REQUIRED    : 'stat2FLD'
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
stat2KEY BRANCH    stat2flag TO stat21a
         CALL      stat2OPEN
stat21a  trap      IOMssg giving Error if IO
         branch    statlock to STAT21aL,STAT21aR,STAT21aN
stat21al FILEPI    1;STAT2FILE
         READ      STAT2FILE,stat2FLD;statVARS
	 trapclr   IO
         RETURN
stat21aR
         READLK   STAT2FILE,stat2FLD;statVARS
	 trapclr   IO
         RETURN
stat21aN
         READ      STAT2FILE,stat2FLD;statVARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : STAT2TST
. REQUIRED    : 
. RETURNED    :
. DESCRIPTION : TEST KEY
.
STAT2TST BRANCH    stat2flag TO STAT22
         CALL      STAT2OPEN
STAT22   trap      IOMssg giving Error if IO
	 FILEPI    1;STAT2FILE
         READ      STAT2FILE,stat2FLD;;
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : STAT2KS
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : KEY SEQUENTIAL NINSTATS FILE READ
.
STAT2Ks  BRANCH    stat2flag TO STAT23
         CALL      STAT2OPEN
STAT23   trap      IOMssg giving Error if IO
	 FILEPI    1;STAT2FILE
         READKS    STAT2FILE;statVARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : STAT2SEQ
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : SEQUENTIAL NINSTATS FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
STAT2SEQ BRANCH    stat2flag TO STAT24
         CALL      STAT2OPEN
STAT24   trap      IOMssg giving Error if IO
	 FILEPI    1;STAT2FILE
         READ      STAT2FILE,SEQ;statVARS
	 trapclr   IO
         RETURN
...............................................................................
. ENTRY POINT : STAT2AIM
. REQUIRED    : STAT2FLD2,STAT2FLD3,STAT2FLD4
. RETURNED    : STATS RECORD
. DESCRIPTION : AIM ORDER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
STAT2AIM BRANCH    stat2flag TO STAT25
         CALL      STAT2OPEN
STAT25   TRAP      IOMssg Giving Error if IO
         Branch    statlock to STAT25L,STAT25R,STAT25N
STAT25L  FILEPI    1;STAT2FLE2
.Start Patch 02/20/2002 Replaced logic
.         READ      STAT2FLE2,STAT2FLD2,STAT2FLD3;statVARS
         READ      STAT2FLE2,STAT2FLD2,STAT2FLD3,STAT2FLD4;statVARS
.End Patch 02/20/2002 Replaced logic
         TRAPCLR   IO
         RETURN
STAT25R
.Start Patch 02/20/2002 Replaced logic
.         READLK    STAT2FLE2,STAT2FLD2,STAT2FLD3;statVARS
         READLK    STAT2FLE2,STAT2FLD2,STAT2FLD3,STAT2FLD4;statVARS
.End Patch 02/20/2002 Replaced logic
         TRAPCLR   IO
         RETURN
STAT25N
.Start Patch 02/20/2002 Replaced logic
.         READ      STAT2FLE2,STAT2FLD2,STAT2FLD3;statVARS
         READ      STAT2FLE2,STAT2FLD2,STAT2FLD3,STAT2FLD4;statVARS
.End Patch 02/20/2002 Replaced logic
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : STAT2KG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : ORDER RECORD
. DESCRIPTION : AIM KEY GENEREIC STATSFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
STAT2KG  BRANCH    stat2flag TO STAT26
         CALL      STAT2OPEN
STAT26   TRAP      IOMssg Giving Error if IO
         Branch    statlock to STAT26L,STAT26R,STAT26N
STAT26L  FILEPI    1;STAT2FLE2
         READKG    STAT2FLE2;statVARS
         TRAPCLR   IO
         RETURN
STAT26R
         READKGLK  STAT2FLE2;statVARS
         TRAPCLR   IO
         RETURN
STAT26N
         READKG    STAT2FLE2;statVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : STAT2WRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
STAT2WRT BRANCH    stat2flag OF STAT27
         CALL      STAT2OPEN
STAT27   trap      IOMssg giving Error if IO
	 FILEPI    1;Stat2FLst
         WRITE     STAT2Flst;statVars
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : STAT2UPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE NINSTATS FILE
.
STAT2UPD BRANCH    stat2flag OF STAT28
         CALL      STAT2OPEN
STAT28   trap      IOMssg giving Error if IO
	 FILEPI    1;STAT2Flst
         UPDATE     STAT2Flst;statVARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : STAT2DEL
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
STAT2DEL BRANCH    stat2flag TO STAT29
         CALL      STAT2OPEN
STAT29   trap      IOMssg giving Error if IO
	 FILEPI    1;STAT2Flst
         DELETE    STAT2FLst
	 trapclr   IO
         RETURN
.START PATCH 3.42 (NORDTEST) 03/22/2002 - ADDED ROUTINE - ASH
..............................................................................
.
. ENTRY POINT : STAT2BUSY
. REQUIRED    : STAT2FLD
. RETURNED    : STATS2 RECORD WITH BUSY BYTE SET TO "*"
. DESCRIPTION : CALLS STATS2TST THEN UPDATES BUSY BYTE
.
STAT2BUSY
	BRANCH	stat2flag TO STAT30
	CALL	STAT2OPEN
STAT30
	READTAB	STAT2FILE,STAT2FLD;*437,statcode;
	if over
		move	"STAT2BUSY",Location
		clear	Error
		append	STAT2FLD,Error
		append	" - Not Found in File!",Error
		reset	Error
		goto	IOMssg
	endif
	MOVE	STAR,statcode
	TRAP	IOMssg Giving Error if IO
	FILEPI	1;Stat2FLst
	UPDATAB	Stat2FLst;*437,statcode
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : STAT2RELEASE
. REQUIRED    : STAT2FLD
. RETURNED    : STATS2 RECORD WITH BUSY BYTE SET TO " "
. DESCRIPTION : CALLS STATS2TST THEN RELEASES BUSY BYTE
.
STAT2RELEASE
	BRANCH	stat2flag TO STAT31
	CALL	STAT2OPEN
STAT31
	READTAB	STAT2FILE,STAT2FLD;*437,statcode;
	IF NOT OVER
		MOVE	" ",statcode
		TRAP	IOMssg Giving Error if IO
		FILEPI	1;Stat2FLst
		UPDATAB	Stat2FLst;*437,statcode
		TRAPCLR	IO
	ENDIF
	RETURN

.END PATCH 3.42 (NORDTEST) 03/22/2002 - ADDED ROUTINE - ASH
..............................................................................
.
. ENTRY POINT : STAT2OPEN
. REQUIRED    : 'stat2flag'
. RETURNED    : 'stat2flag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN STATS FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
STAT2OPEN
	 TRAP      STAT2GONE giving error IF IO
         OPEN      STAT2Flst
         TRAPCLR   IO
         MOVE      C1 TO stat2flag
         RETURN
.
STAT2GONE
	 MOVE      stat2NAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
.END OF INCLUDE
