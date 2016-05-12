..............................................................................
.
. NESCIO INCLUSION
. NIN Escow FIle I/O routines
.
.
. FILE NAME : Escrow
. REC LENGTH:  FIXED
.Cheated have not implemetned record locks
. INDEX KEY : 
.
..............................................................................
.NESCKEY - ISAM Key read
NESCKEY  BRANCH    NESCPATH OF NESC1A,NESC1C
NESC1a   BRANCH    NESCFLAG TO NESC1b
         CALL      NESCOPEN
NESC1b   if	(Nesclock <= c1)
	FILEPI    1;NESCFILE
         READ      NESCFILE,NESCFLD;NESCVars
	elseif	(Nesclock = c3)
         READ      NESCFILE,NESCFLD;NESCVars
	endif
         RETURN
NESC1c   BRANCH    NESCFLG2 TO NESC1d
         CALL      NESCOPEN
NESC1d   if	(Nesclock <= c1)
	FILEPI    1;NESCFLE2
         READ      NESCFLE2,NESCFLD2;NESCVars
	elseif	(Nesclock = c3)
         READ      NESCFLE2,NESCFLD2;NESCVars
	endif
         RETURN
..............................................................................
.nesctst - Test REAd
NESCtst  BRANCH    NESCPATH OF NESC2A,NESC2C
NESC2a   BRANCH    NESCFLAG TO NESC2b
         CALL      NESCOPEN
NESC2b   FILEPI    1;NESCFILE
         READ      NESCFILE,NESCFLD;;
         RETURN
NESC2c   BRANCH    NESCFLG2 TO NESC2d
         CALL      NESCOPEN
NESC2d   FILEPI    1;NESCFLE2
         READ      NESCFLE2,NESCFLD2;;
         RETURN
...............................................................................
.READKS - Key Sequencial
NESCKs  BRANCH    NESCPATH OF NESC3A,NESC3C
NESC3a   BRANCH    NESCFLAG TO NESC3b
         CALL      NESCOPEN
NESC3b   if	(Nesclock <= c1)
	FILEPI    1;NESCFILE
         READKS    NESCFILE;NESCVars
	elseif	(Nesclock = c3)
         READKS    NESCFILE;NESCVars
	endif	
         RETURN
NESC3c   BRANCH    NESCFLG2 TO NESC3d
         CALL      NESCOPEN
NESC3d   if	(Nesclock <= c1)
   	FILEPI    1;NESCFLE2
         READks    NESCFLE2;NESCVars
	elseif	(Nesclock = c3)
         READks    NESCFLE2;NESCVars
	endif
         RETURN
...............................................................................
.Nescseq - READ Sequencially
NESCseq  BRANCH    NESCPATH OF NESC4A,NESC4C
NESC4a   BRANCH    NESCFLAG TO NESC4b
         CALL      NESCOPEN
NESC4b   FILEPI    1;NESCFILE
           READ      NESCFILE,SEQ;NESCVars
         RETURN
NESC4c   BRANCH    NESCFLG2 TO NESC4d
         CALL      NESCOPEN
NESC4d   FILEPI    1;NESCFLE2
         READ     NESCFLE2,seq;NESCVars
         RETURN
...............................................................................
.WRITE - WRITE
NESCWRT   BRANCH    NESCFLAG TO NESC5a
         CALL      NESCOPEN   
NESC5A
         FILEPI    1;NESCFList
         WRITE     NESCFList;nESCvars
         RETURN
...............................................................................
.UPDATE
NESCUPD 
          BRANCH    NESCFLAG TO NESC6
          CALL      NESCOPEN   
NESC6
          FILEPI    1;NESCFList
          Update    NescFlist;NescVars
          return
.
.........................................................
.requires previous valid read
NESCDEL

         BRANCH    NESCFLAG OF NESC7
         CALL      NESCOPEN
NESC7
         FILEPI    1;NESCFlist
         DELETE    NESCFlist
         RETURN
...............................................................................
NESCOPEN  BRANCH   NESCPATH TO NESCOPN1,NESCOPN2
NESCOPN1  TRAP      NESCGONE IF IO
          OPEN      NESCFLIST
          TRAPCLR   IO
          MOVE      C1 TO NESCFLAG
          MOVE      C1 TO NESCFLG2
          RETURN
NESCOPN2  TRAP      NESCGONE IF IO
          OPEN      NESCFLIST
          TRAPCLR   IO
          MOVE      C1 TO NESCFLAG
          MOVE      C1 TO NESCFLG2
          RETURN
NESCGONE  MOVE      NESCNAME TO FILENAME
          GOTO      FILEGONE

