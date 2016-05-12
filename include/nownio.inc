..............................................................................
.
. NOWNIO INCLUSION
. NIN OWNER FILE I/O ROUTINES
.
.
.  FILE NAME : NINOWN
. REC LENGTH: 170 FIXED
. INDEX KEY : 3-6
. AIM KEY   : 7-56
.2014 April 2 : DLH add lock logic
.2010 June 04 : DLH add secondary file maint for DupeOwn
..............................................................................
.
. ENTRY POINT : NOWNKEY
. REQUIRED    : 'NOWNFLD'
. RETURNED    : OWNER RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NOWNKEY  BRANCH    NOWNFLG1 TO NOWN1
         CALL      NOWNOPEN
NOWN1    
         BRANCH    NownLOCK TO Nown1L,Nown1R,Nown1N
.Nown1L - file locks
NOWN1L
	FILEPI    1;NOWNFLE1
         READ      NOWNFLE1,NOWNFLD;OWNvars
         RETURN
.Nown1R - Record locks
NOWN1R
         READLK     NOWNFLE1,NOWNFLD;OWNvars
         RETURN
.Nown1N - No file locks
NOWN1N
         READ      NOWNFLE1,NOWNFLD;OWNvars
         RETURN
..............................................................................
.
. ENTRY POINT : NOWNTST
. REQUIRED    : NOWNFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NOWNTST  BRANCH    NOWNFLG1 TO NOWN2
         CALL      NOWNOPEN
NOWN2    
         BRANCH    NownLOCK TO Nown2L,Nown2R,Nown2N
.Nown2L - file locks
NOWN2L
	FILEPI    1;NOWNFLE1
         READ      NOWNFLE1,NOWNFLD;STR1
         RETURN
.Nown2L - record locks
NOWN2R
	READLK      NOWNFLE1,NOWNFLD;STR1
         	RETURN
.Nown2L - No file locks
NOWN2N
         READ      NOWNFLE1,NOWNFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NOWNKS
. REQUIRED    :
. RETURNED    : OWNER RECORD
. DESCRIPTION : KEY SEQUENTIAL OWNER FILE READ
.
NOWNKS   BRANCH    NOWNFLG1 TO NOWN3
         CALL      NOWNOPEN
NOWN3    
         BRANCH    NownLOCK TO Nown3L,Nown3R,Nown3N
.Nown3L - file locks
NOWN3L
	FILEPI    1;NOWNFLE1
         READKS    NOWNFLE1;OWNvars
         RETURN
.Nown3R - REcord locks
NOWN3R
         READKSLK    NOWNFLE1;OWNvars
         RETURN
.Nown3N - NO file locks
NOWN3N
         READKS    NOWNFLE1;OWNvars
         RETURN
..............................................................................
.  ******UPDATED 18MAR92 - FOR SEPERATE SEQ FILE DESCRIPTOR, NOWNPATH=1 USE 
.                          ISAM FILE, 3=USE FLAT FILE.
. ENTRY POINT : NOWNSEQ
. REQUIRED    :
. RETURNED    : OWNER RECORD
. DESCRIPTION : SEQUENTIAL OWNER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
NOWNSEQ  BRANCH    NOWNPATH OF NOWN4A,NOWNGONE,NOWN4C
NOWN4A     BRANCH    NOWNFLG1 TO NOWN4B
         CALL      NOWNOPEN
NOWN4B   FILEPI    1;NOWNFLE1
         READ      NOWNFLE1,SEQ;OWNvars
         RETURN
NOWN4C     BRANCH    NOWNFLG3 TO NOWN4D
         CALL      NOWNOPEN
NOWN4D   FILEPI    1;NOWNFLE3
         READ      NOWNFLE3,SEQ;OWNvars
         RETURN
..............................................................................
.
. ENTRY POINT : NOWNWRT
. REQUIRED    : 'NOWNFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NOWNWRT  BRANCH    NOWNFLG1 TO NOWN5
         CALL      NOWNOPEN
NOWN5    
         BRANCH    NownLOCK TO Nown5L,Nown5R,Nown5L
.Nown5L - file locks
NOWN5L
	FILEPI    1;NOWNFLE1
         WRITE     OwnFList;Ownvars
         RETURN
.Nown5R - record locks
NOWN5R
         WRITE     OwnFList;Ownvars
         RETURN

..............................................................................
.
. ENTRY POINT : NOWNUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE OWNER FILE
.
NOWNUPD  BRANCH    NOWNFLG1 TO NOWN6
         CALL      NOWNOPEN
NOWN6
         BRANCH    NownLOCK TO Nown6L,Nown6R,Nown6L
.Nown6L - file locks
NOWN6L
    	FILEPI    1;NOWNFLE1
         UPDATE    OwnFList;ownvars
         RETURN
.Nown6r - record locks
NOWN6r
         UPDATE    OwnFList;ownvars
         RETURN
..............................................................................
.
. ENTRY POINT : NOWNDEL
. REQUIRED    : 'NOWNFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NOWNDEL  BRANCH    NOWNFLG1 TO NOWN7
         CALL      NOWNOPEN
NOWN7    
         BRANCH    NownLOCK TO Nown7L,Nown7R,Nown7L
.Nown7L - file locks
NOWN7L
	FILEPI    1;NOWNFLE1
         DELETE    OwnFList
         RETURN
.Nown7R - REcord locks
NOWN7R
         DELETE    OwnFList
         RETURN
..............................................................................
.
. ENTRY POINT : NOWNAIM
. REQUIRED    : NOWNFLD2
. RETURNED    : OWNER RECORD
. DESCRIPTION : AIM OWNER FILE READ
.               APPLICA TION'S RESPONSIBILITY TO TEST FLGS
.
NOWNAIM  BRANCH    NOWNFLG2 TO NOWN8
         CALL      NOWNOPN2
NOWN8
         BRANCH    NownLOCK TO Nown8L,Nown8R,Nown8N
.Nown8L - file locks
NOWN8L
    	FILEPI    1;NOWNFLE2
         READ      NOWNFLE2,NOWNFLD2;OWNvars
         RETURN
.Nown8R - record locks
NOWN8R
         READLK      NOWNFLE2,NOWNFLD2;OWNvars
         RETURN
.Nown8n - NO locks
NOWN8n
         READ      NOWNFLE2,NOWNFLD2;OWNvars
         RETURN
..............................................................................
.
. ENTRY POINT : NOWNKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : OWNER RECORD
. DESCRIPTION : AIM KEY GENEREIC OWNER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NOWNKG   BRANCH    NOWNFLG2 TO NOWN10
         CALL      NOWNOPN2
NOWN10   
         BRANCH    NownLOCK TO Nown10L,Nown10R,Nown10N
.Nown10L - file locks
NOWN10L
	FILEPI    1;NOWNFLE2
         READKG    NOWNFLE2;OWNvars
         RETURN
.Nown10R - Record locks
NOWN10R
         READKGLK    NOWNFLE2;OWNvars
         RETURN
.Nown10N - NO file locks
NOWN10N
         READKG    NOWNFLE2;OWNvars
         RETURN
..............................................................................
.
. ENTRY POINT : NOWNKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : OWNER RECORD
. DESCRIPTION : KEY GENERIC PRIOR OWNER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NOWNKGP  BRANCH    NOWNFLG2 TO NOWN11
         CALL      NOWNOPN2
NOWN11   
         BRANCH    NownLOCK TO Nown11L,Nown11R,Nown11N
.Nown11L - file locks
NOWN11L
	FILEPI    1;NOWNFLE2
         READKGP   NOWNFLE2;OWNvars
         RETURN
.Nown11R - REcord locks
NOWN11R
         READKGPLK   NOWNFLE2;OWNvars
         RETURN
.Nown11N - NO file locks
NOWN11N
         READKGP   NOWNFLE2;OWNvars
         RETURN
...............................................................................
. 
. ENTRY POINT : NOWNOPEN
. REQUIRED    : 'NOWNFLG1' 'NOWNPATH'
. RETURNED    : 'NOWNFLG1' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN OWNER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NOWNOPEN BRANCH    NOWNPATH OF NOWNOPN1,NOWNOPN2,NOWNOPN3   
NOWNOPN1 TRAP      NOWNGONE giving error IF IO
.         OPEN      NOWNFLE1,NOWNNAME
         BRANCH    NOWNLOCK TO NOWNo1L,NOWNo1R,NOWNo1L
Nowno1L
         OPEN      OwnFList
         TRAPCLR   IO
         MOVE      C1 TO NOWNFLG1
         RETURN
Nowno1R
         OPEN      OwnFList,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NOWNFLG1
         RETURN
..............................................................................
NOWNOPN2 TRAP      NOWNGONE giving error IF IO
.         OPEN      NOWNFLE2,NOWNNAME
         OPEN      OwnFList
         TRAPCLR   IO
         MOVE      C1 TO NOWNFLG2
         RETURN
.
..............................................................................
NOWNOPN3 TRAP      NOWNGONE giving error IF IO
         OPEN      NOWNFLE3,NOWNNAME
         TRAPCLR   IO
         MOVE      C1 TO NOWNFLG3
         RETURN
..............................................................................
NOWNGONE MOVE      NOWNNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
................................................
. ENTRY POINT : DOWNKEY
. REQUIRED    : 'DOWNFLD'
. RETURNED    : OWNER RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
DOWNKEY  BRANCH    DOWNFLG1 TO DOWN1
         CALL      DOWNOPEN
DOWN1    FILEPI    1;DOWNFLE1
         READ      DOWNFLE1,DOWNFld;DOWNKEY,Down1,Downkey2,Down2:
                   DOWNDES,Down3

         RETURN
..............................................................................
.
. ENTRY POINT : DOWNTST
. REQUIRED    : DOWNFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
DOWNTST  BRANCH    DOWNFLG1 TO DOWN2
         CALL      DOWNOPEN
DOWN2    FILEPI    1;DOWNFLE1
         READ      DOWNFLE1,DOWNFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : DOWNKS
. REQUIRED    :
. RETURNED    : OWNER RECORD
. DESCRIPTION : KEY SEQUENTIAL OWNER FILE READ
.
DOWNKS   BRANCH    DOWNFLG1 TO DOWN3
         CALL      DOWNOPEN
DOWN3    FILEPI    1;DOWNFLE1
         READKS    DOWNFLE1;DOWNKEY,Down1,Downkey2,Down2:
                   DOWNDES,Down3
         RETURN
..............................................................................
. ENTRY POINT : DOWNSEQ
. REQUIRED    :
. RETURNED    : OWNER RECORD
. DESCRIPTION : SEQUENTIAL OWNER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
DOWNSEQ  
         BRANCH    DOWNFLG1 TO DOWN4
         CALL      DOWNOPEN
DOWN4    FILEPI    1;DOWNFLE1
         READ      DOWNFLE1,SEQ;DOWNKEY,Down1,Downkey2,Down2:
                   DOWNDES,Down3
         RETURN
..............................................................................
.
. ENTRY POINT : DOWNWRT
. REQUIRED    : 'DOWNFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
DOWNWRT  BRANCH    DOWNFLG1 TO DOWN5
         CALL      DOWNOPEN
DOWN5    FILEPI    1;DOWNFLE1
         WRITE     DownFle1;DOWNKEY,Down1,Downkey2,Down2:
                   DOWNDES,Down3
         RETURN
..............................................................................
.
. ENTRY POINT : DOWNUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE OWNER FILE
.
DOWNUPD  BRANCH    DOWNFLG1 TO DOWN6
         CALL      DOWNOPEN
DOWN6
         UPDATE    DownFle1;DOWNKEY,Down1,Downkey2,Down2:
                   DOWNDES,Down3
         RETURN
..............................................................................
.
. ENTRY POINT : DOWNDEL
. REQUIRED    : 'DOWNFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
DOWNDEL  BRANCH    DOWNFLG1 TO DOWN7
         CALL      DOWNOPEN
DOWN7    FILEPI    1;DOWNFLE1
         DELETE    DOWNFLE1,DOWNFLD
         RETURN
..............................................................................
.
. 
. ENTRY POINT : DOWNOPEN
. REQUIRED    : 'DOWNFLG1' 'DOWNPATH'
. RETURNED    : 'DOWNFLG1' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN OWNER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
DOWNOPEN 
          TRAP      DOWNGONE giving error IF IO
         OPEN      DOWNFLE1,DOWNNAME
         TRAPCLR   IO
         MOVE      C1 TO DOWNFLG1
         RETURN
..............................................................................
..............................................................................
DOWNGONE MOVE      DOWNNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................



