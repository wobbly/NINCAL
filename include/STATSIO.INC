..............................................................................
.
. statio INCLUSION
. Stats FILE DEFINITION
.
. FILE NAME : ninstat
. REC LENGTH:  305 FIXED
. INDEX KEY : 
.
.Patch 02/20/2002 - Added Aamdex for History Screen in Program 1 
.patch 2.5  DLH 05July01 change file order get rid of current primary file with 4 key components and use
. current fle3 - as primary
..............................................................................
.
..............................................................................
.
. ENTRY POINT : statKEY
. REQUIRED    : 'statFLD'
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.begin patch 2.5
.statKEY   branch    statpath to stat1a,stat1c,stat1e
statKEY   branch    statpath to stat1a,stat1c
.end patch 2.5
stat1a    BRANCH    statFLAG TO stat1b
         CALL      statOPEN
stat1b
         branch    statlock to stat1bL,stat1bR,stat1bN
stat1bl  FILEPI    1;statFILE
         READ      statFILE,statFLD;statVARS
         RETURN
stat1bR
         READLK   statFILE,statFLD;statVARS
         RETURN
stat1bN
         READ      statFILE,statFLD;statVARS
         RETURN
stat1c    BRANCH    statFLAG2 TO stat1d
         CALL      statOPEN2
stat1d
         branch    statlock to stat1DL,stat1DR,stat1DN
STAT1DL
         FILEPI    1;statFLE2
         READ      statFLE2,statFLD2;statVARS
         RETURN
STAT1Dr
         READlk    statFLE2,statFLD2;statVARS
         RETURN
STAT1Dn

         READ      statFLE2,statFLD2;statVARS
         RETURN
.begin patch 2.5
.stat1e
.         BRANCH    statFLAG3 TO stat1f
.         CALL      statOPEN3
.stat1f
.         branch    statlock to stat1FL,stat1FR,stat1FN
.STAT1FL  FILEPI    1;statFLE3
.         READ      statFLE3,statFLD3;statVARS
.         RETURN
.STAT1FR
.         READLK    statFLE3,statFLD3;statVARS
.         RETURN
.STAT1FN
.         READ      statFLE3,statFLD3;statVARS
.         RETURN
.end patch 2.5
..............................................................................
.
. ENTRY POINT : statTST
. REQUIRED    : 
. RETURNED    :
. DESCRIPTION : TEST KEY
.
.begin patch 2.5
.statTST   branch    statpath to stat2a,stat2c,stat2e
statTST   branch    statpath to stat2a,stat2c
.end patch 2.5
stat2a    BRANCH    statFLAG TO stat2b
         CALL      statOPEN
stat2b    FILEPI    1;statFILE
         READ      statFILE,statFLD;;
         RETURN
stat2c    BRANCH    statFLAG2 TO stat2d
         CALL      statOPEN2
stat2d    FILEPI    1;statFLE2
         READ      statFLE2,statFLD2;STR1
         RETURN
.begin patch 2.5
.stat2e    BRANCH    statFLAG3 TO stat2f
.         CALL      statOPEN3
.stat2f    FILEPI    1;statFLE3
.         READ      statFLE3,statFLD3;STR1
.         RETURN
.end patch 2.5
..............................................................................
.
. ENTRY POINT : statKS
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : KEY SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.
.begin patch 2.5
statKs    branch    statpath to stat3a,stat3c
.statKs    branch    statpath to stat3a,stat3c,stat3e
.end patch 2.5
stat3a    BRANCH    statFLAG TO stat3b
         CALL      statOPEN
stat3b    FILEPI    1;statFILE
         READKS    statFILE;statVARS
         RETURN
stat3c    BRANCH    statFLAG2 TO stat3d
         CALL      statOPEN2
stat3d    FILEPI    1;statFLE2
         READKS    statFLE2;statVARS
         RETURN
.begin patch 2.5
.stat3e    BRANCH    statFLAG3 TO stat3f
.         CALL      statOPEn3
.stat3f    FILEPI    1;statFLE3
.         READKS    statFLE3;statVARS
.         RETURN
.end patch 2.5
..............................................................................
.
. ENTRY POINT : statSEQ
. REQUIRED    :
. RETURNED    : SPECIAL INSTRUCTION RECORD
. DESCRIPTION : SEQUENTIAL SPECIAL INSTRUCTION FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
statSEQ  BRANCH    statFLAG TO stat4
         CALL      statOPEN
stat4    FILEPI    1;statFILE
         READ      statFILE,SEQ;statVARS
         RETURN
..............................................................................
.
. ENTRY POINT : statWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
statWRT  BRANCH    statFLAG OF stat5
         CALL      statOPEN
stat5
.    FILEPI    1;statFILE
.         WRITE     statFILE,statFLD;statVARS
         WRITE     StatFlst;statVars
.         BRANCH    statFLAG2 OF stat5c
.         CALL      statOPEN2
.stat5c   FILEPI    1;statFLE2
.         insert    statfle2,statlist
.begin patch 2.5
.         BRANCH    statFLAG3 OF stat5f
.         CALL      statOPEN3
.stat5f   PACK      statFLD3 FROM statmlr,statsrce
.         FILEPI    1;statFLE3
.         insert    statfle3,statfld3
          RETURN
.end patch 2.5
..............................................................................
.
. ENTRY POINT : statUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE SPECIAL INSTRUCTION FILE
.
.begin patch 2.5
.statUPD  BRANCH    statpath TO stat6a,stat6c,stat6e
statUPD
         BRANCH    statpath TO stat6a,stat6c
.end patch 2.5
         CALL      statOPEN
stat6a
.   FILEPI    1;statFILE
.         UPDATE    statFILE;statVARS
         UPDATE     StatFlst;statVARS
         RETURN
stat6c
.   FILEPI    1;statFLE2
         UPDATE     StatFlst;statVARS
.         UPDATE    statFLE2;statVARS
         RETURN
.begin patch 2.5
.stat6e
.           FILEPI    1;statFLE3
.         UPDATE    statFLE3;statVARS
.         UPDATE     StatFlst;statVARS
.         RETURN
.end patch 2.5
..............................................................................
.
. ENTRY POINT : statDEL
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
.begin patch 2.5
.statDEL   BRANCH    statpath TO stat7a,stat7c,stat7e
statDEL   BRANCH    statpath TO stat7a,stat7c
.end patch 2.5
stat7a    BRANCH    statFLAG TO stat7b
         CALL      statOPEN
stat7b
.    FILEPI    1;statFILE
         DELETE     statFLst
.         DELETE    statFILE,statFLD
         RETURN
stat7c    BRANCH    statFLAG2 TO stat7d
         CALL      statOPEN2
stat7d
.   FILEPI    1;statFLE2
         DELETE     statFLst
.         DELETE    statFLE2,statFLD2
         RETURN
.begin patch 2.5
.stat7e    BRANCH    statFLAG3 TO stat7f
.         CALL      statOPEN3
.stat7f
.    FILEPI    1;statFLE3
.         DELETE    statFLE3,statFLD3
..         DELETE     statFLst
.         RETURN
.end patch 2.5

.Patch 02/20/2002 Added logic
...............................................................................
. ENTRY POINT : STATAIM
. REQUIRED    : STATFLD4,STATFLD5
. RETURNED    : STATS RECORD
. DESCRIPTION : AIM ORDER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
STATAIM	BRANCH	statflag TO STAT8
	CALL	STATOPEN
STAT8	TRAP	IOMssg Giving Error if IO
	Branch	statlock to STAT8L,STAT8R,STAT8N
STAT8L	FILEPI	1;STATFLE3
	READ	STATFLE3,STATFLD4,STATFLD5;statVARS
	TRAPCLR	IO
	RETURN
STAT8R
	READLK	STATFLE3,STATFLD4,STATFLD5;statVARS
	TRAPCLR	IO
	RETURN
STAT8N
	READ	STATFLE3,STATFLD4,STATFLD5;statVARS
	TRAPCLR	IO
	RETURN
..............................................................................
.
. ENTRY POINT : STATKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : ORDER RECORD
. DESCRIPTION : AIM KEY GENEREIC STATSFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
STATKG	BRANCH	statflag TO STAT9
	CALL	STATOPEN
STAT9	TRAP	IOMssg Giving Error if IO
	Branch	statlock to STAT9L,STAT9R,STAT9N
STAT9L	FILEPI	1;STATFLE3
	READKG	STATFLE3;statVARS
	TRAPCLR	IO
	RETURN
STAT9R
	READKGLK STATFLE3;statVARS
	TRAPCLR	IO
	RETURN
STAT9N
	READKG	STATFLE3;statVARS
	TRAPCLR	IO
	RETURN
.End 02/20/2002 Added logic
..............................................................................
.
. ENTRY POINT : statOPEN
. REQUIRED    : 'statFLAG'
. RETURNED    : 'statFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN SPECIAL INSTRUCTION FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
statOPEN TRAP      statGONE giving error IF IO
.         OPEN      statFILE,statNAME
         OPEN      StatFlst
         TRAPCLR   IO
         MOVE      C1 TO statFLAG
.         MOVE      C1 TO statFLAG2
.         MOVE      C1 TO statFLAG3
         RETURN
.
statOPEN2 TRAP      statGONE giving error IF IO
         move      statnme2 to statname
         OPEN      StatFlst
.         OPEN      statFLE2,statNME2
         TRAPCLR   IO
         MOVE      C1 TO statFLAG
         MOVE      C1 TO statFLAG2
.         MOVE      C1 TO statFLAG3
         RETURN
.
.begin patch 2.5
.statOPEN3 TRAP      statGONE giving error IF IO
.         move      statnme3 to statname
..         OPEN      StatFlst
.         OPEN      statFLE3,statNME3
.         TRAPCLR   IO
..         MOVE      C1 TO statFLAG
..         MOVE      C1 TO statFLAG2
.         MOVE      C1 TO statFLAG3
.         RETURN
.end patch 2.5
.
statGONE MOVE      statNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
.END OF INCLUDE

