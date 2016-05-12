.........................................................
.
. NLOLIO INCLUSION
. NIN LOL FILE I/O ROUTINES
.
. FILE NAME :   NINLOL
. REC LENGTH:   365 FIXED
. INDEX KEY :   (1)     001-006 (LOL Number)
.               (2)     007-012 (CAMPAIGN Number)
. AAM KEYS  :   (1)     007-018 (CAMPAIGN + LIST Number)
.               (2)     019-024 (LR/LCR Number)
.
. Last Update Added logic to thwart I44 errors for defunct Key values - forcing setting of OVER flag - ASH 5/20/04
.........................................................
. ENTRY POINT : NLOLKEY
. REQUIRED    : 'NLOLPATH', 'NLOLFLD', or 'NLOLFLD1'
. RETURNED    : LOL VARS
. DESCRIPTION : EXACT ISAM KEY READ - EITHER VIA HIDDEN KEY OR CAMPAIGN KEY (DUPES ALLOWED)
.
NLOLKEY  branch    NLOLpath to NLOL1a,NLOL1c,NLOLerr
NLOL1a
.START PATCH 5/20/2004 ADDED LOGIC - ASH
	 call      Trim using NLOLFLD
	 if (NLOLFLD = "")
		   flagrestore	"0001"
		   return
	 endif
.END PATCH 5/20/2004 ADDED LOGIC - ASH
         BRANCH    NLOLflag TO NLOL1b
         CALL      NLOLOPEN
NLOL1B   TRAP      IOMssg Giving Error if IO
         branch    NLOLlock to NLOL1bL,NLOL1bR,NLOL1bN
NLOL1bL  FILEPI    1;NLOLFILE
         READ      NLOLFILE,NLOLFLD;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL1bR  
         READLK    NLOLFILE,NLOLFLD;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL1bN
         READ      NLOLFILE,NLOLFLD;NLOLVARS
         TRAPCLR   IO
         RETURN
.
NLOL1c
.START PATCH 5/20/2004 ADDED LOGIC - ASH
	 call      Trim using NLOLFLD1
	 if (NLOLFLD1 = "")
		   flagrestore	"0001"
		   return
	 endif
.END PATCH 5/20/2004 ADDED LOGIC - ASH
         BRANCH    NLOLFLG1 TO NLOL1d 
         CALL      NLOLOPEN
NLOL1d   TRAP      IOMssg Giving Error if IO
         branch    NLOLlock to NLOL1dL,NLOL1dR,NLOL1dN
NLOL1dL  FILEPI    1;NLOLFLE1
         READ      NLOLFLE1,NLOLFLD1;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL1dR  
         READLK    NLOLFLE1,NLOLFLD1;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL1dN
         READ      NLOLFLE1,NLOLFLD1;NLOLVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLTST
. REQUIRED    : NLOLFLD
. RETURNED    : FIRST 26 BYTES
. DESCRIPTION : TEST KEY
.
NLOLTST  branch    NLOLpath to NLOL2a,NLOL2c,NLOL2e
NLOL2a   BRANCH    NLOLFLAG TO NLOL2b
         CALL      NLOLOPEN
NLOL2b   TRAP      IOMssg Giving Error if IO
         FILEPI    1;NLOLFILE
         READ      NLOLFILE,NLOLFLD;NLOL2,NLOL24;
         TRAPCLR   IO
         RETURN
.
NLOL2c   BRANCH    NLOLFLG1 TO NLOL2d
         CALL      NLOLOPEN
NLOL2d   TRAP      IOMssg Giving Error if IO
         FILEPI    1;NLOLFLE1
         READ      NLOLFLE1,NLOLFLD1;NLOL2,NLOL24;
         TRAPCLR   IO
         RETURN
.
NLOL2e   BRANCH    NLOLFLG2 TO NLOL2f
         CALL      NLOLOPN2
NLOL2f   TRAP      IOMssg Giving Error if IO
         FILEPI    1;NLOLFLE2
         READ      NLOLFLE2,NLOLFLD2,NLOLFLD3,NLOLFLD4;NLOL2,NLOL24;
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLKS
. REQUIRED    : 'NLOLPATH'
. RETURNED    : LOL VARS
. DESCRIPTION : KEY SEQUENTIAL LOL FILE READ
.
NLOLks   branch    NLOLpath to NLOL3a,NLOL3c,NLOLerr
NLOL3a   BRANCH    NLOLFLAG TO NLOL3b
         CALL      NLOLOPEN
NLOL3b   TRAP      IOMssg Giving Error if IO
         branch    NLOLlock to NLOL3bL,NLOL3bR,NLOL3bN

NLOL3bL  FILEPI    1;NLOLFILE
         READKS    NLOLFILE;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL3bR
         READKSLK  NLOLFILE;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL3bN
         READKS    NLOLFILE;NLOLVARS
         TRAPCLR   IO
         RETURN
.
NLOL3c   BRANCH    NLOLFLG1 TO NLOL3d
         CALL      NLOLOPEN
NLOL3d   TRAP      IOMssg Giving Error if IO
         branch    NLOLlock to NLOL3dL,NLOL3dR,NLOL3dN

NLOL3dL  FILEPI    1;NLOLFLE1
         READKS    NLOLFLE1;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL3dR
         READKSLK  NLOLFLE1;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL3dN
         READKS    NLOLFLE1;NLOLVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLKS2
. REQUIRED    : 'NLOLPATH'
. RETURNED    : FIRST 26 BYTES
. DESCRIPTION : KEY SEQUENTIAL LOL FILE READ
.
NLOLks2  branch    NLOLpath to NLOL3a2,NLOL3c2,NLOLerr
NLOL3a2  BRANCH    NLOLFLAG TO NLOL3b2
         CALL      NLOLOPEN
NLOL3b2  TRAP      IOMssg Giving Error if IO
         branch    NLOLlock to NLOL3bL2,NLOL3bR2,NLOL3bN2

NLOL3bL2 FILEPI    1;NLOLFILE
         READKS    NLOLFILE;NLOL2,NLOL24
         TRAPCLR   IO
         RETURN
NLOL3bR2
         READKSLK  NLOLFILE;NLOL2,NLOL24
         TRAPCLR   IO
         RETURN
NLOL3bN2
         READKS    NLOLFILE;NLOL2,NLOL24
         TRAPCLR   IO
         RETURN
.
NLOL3c2  BRANCH    NLOLFLG1 TO NLOL3d2
         CALL      NLOLOPEN
NLOL3d2  TRAP      IOMssg Giving Error if IO
         branch    NLOLlock to NLOL3dL2,NLOL3dR2,NLOL3dN2

NLOL3dL2 FILEPI    1;NLOLFLE1
         READKS    NLOLFLE1;NLOL2,NLOL24
         TRAPCLR   IO
         RETURN
NLOL3dR2
         READKSLK  NLOLFLE1;NLOL2,NLOL24
         TRAPCLR   IO
         RETURN
NLOL3dN2
         READKS    NLOLFLE1;NLOL2,NLOL24
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLSEQ
. REQUIRED    :
. RETURNED    : LOL VARS
. DESCRIPTION : SEQUENTIAL LOL FILE READ
.
NLOLSEQ  BRANCH    NLOLFLAG TO NLOL4
         CALL      NLOLOPEN
NLOL4    TRAP      IOMssg Giving Error if IO
         branch    NLOLlock to NLOL4L,NLOL4R,NLOL4N
NLOL4L   FILEPI    1;NLOLFILE
         READ      NLOLFILE,SEQ;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL4R
         READLK    NLOLFILE,SEQ;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL4N
         READ      NLOLFILE,SEQ;NLOLVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLWRT
. REQUIRED    : 'NLOLFLD', 'NLOLPATH=1'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT., ISI 2/3 INSERTS.
.
NLOLWRT  MOVE      C1 TO NLOLPATH
NLOLWRT1 COMPARE   NLOLPATH TO NLOLFLAG
         GOTO      NLOL5 IF EQUAL
         CALL      NLOLOPEN
NLOL5    TRAP      IOMssg Giving Error if IO
         FILEPI    1;NLOLFILE
         WRITE     NLOLFILE,NLOLFLD;NLOLVARS
.
         CALL      NLOLWRT2
         CALL      NLOLWRT3
         TRAPCLR   IO
         RETURN
.
NLOLWRT2 BRANCH    NLOLFLG1 TO NLOL5B
         CALL      NLOLOPN1
NLOL5B   FILEPI    1;NLOLFLE1
         INSERT    NLOLFLE1,NLOLCNUM
         RETURN
NLOLWRT3 BRANCH    NLOLFLG2 TO NLOL5C
         CALL      NLOLOPN2
NLOL5C   FILEPI    1;NLOLFLE2
         INSERT    NLOLFLE2
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE LOL FILE, APPLICATIONS RESPONSIBILITY TO UPDATE OTHER FILES!!!
.
NLOLUPD  BRANCH    NLOLFLAG TO NLOL6
         CALL      NLOLOPN
NLOL6    TRAP      IOMssg Giving Error if IO
         FILEPI    1;NLOLFILE
         UPDATE    NLOLFILE;NLOLVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLDEL                                             
. REQUIRED    : 'NLOLPATH', 'NLOLFLD'
. RETURNED    :
. DESCRIPTION : PATH=1 DELETES TXT & ALL KEYS. PATH=2/3/4 DELETES KEY ONLY.
.
NLOLDEL  BRANCH    NLOLPATH TO NLOLDEL1,NLOLDEL2,NLOLDEL3
.
NLOLDEL1 BRANCH    NLOLFLAG TO NLOL7
         CALL      NLOLOPN
NLOL7    CALL      NLOLTST
         RETURN    IF OVER
         TRAP      IOMssg Giving Error if IO
         FILEPI    1;NLOLFILE
         DELETE    NLOLFILE,NLOLFLD
         TRAPCLR   IO
         RETURN
.THIS BIT OF CODE NOT YET REALLY READY.  GO OVER THIS WHEN DELETE IS AN ACTUAL OPTION.  ASH
NLOLDEL2 BRANCH    NLOLFLG1 TO NLOL7B
         CALL      NLOLOPN1
NLOL7B   TRAP      IOMssg Giving Error if IO
         FILEPI    1;NLOLFLE1
         DELETE    NLOLFLE1,NLOLFLD1
         TRAPCLR   IO
         RETURN
.
NLOLDEL3 BRANCH    NLOLFLG2 TO NLOL7c
         CALL      NLOLOPN2
NLOL7c   TRAP      IOMssg Giving Error if IO
         FILEPI    1;NLOLFLE2
         DELETE    NLOLFLE2
         TRAPCLR   IO
         RETURN
.
..............................................................................
.
. ENTRY POINT : NLOLKP
. REQUIRED    : 'NLOLPATH'
. RETURNED    : LOL RENLOL
. DESCRIPTION : KEY SEQUENTIAL LOL FILE READ
.
NLOLKP   branch    NLOLpath to NLOL8a,NLOL8c,NLOLerr
         BRANCH    NLOLFLAG TO NLOL8a
         CALL      NLOLOPEN
NLOL8a   TRAP      IOMssg Giving Error if IO
         Branch    NLOLlock to NLOL8aL,NLOL8aR,NLOL8aN
NLOL8aL  FILEPI    1;NLOLFILE
         READKP    NLOLFILE;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL8aR
         READKPLK  NLOLFILE;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL8aN
         READKP    NLOLFILE;NLOLVARS
         TRAPCLR   IO
         RETURN
.
NLOL8c   BRANCH    NLOLFLG1 TO NLOL8d
         CALL      NLOLOPEN
NLOL8d   TRAP      IOMssg Giving Error if IO
         branch    NLOLlock to NLOL8dL,NLOL8dR,NLOL8dN

NLOL8dL  FILEPI    1;NLOLFLE1
         READKS    NLOLFLE1;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL8dR 
         READKSLK  NLOLFLE1;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL8dN 
         READKS    NLOLFLE1;NLOLVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLAIM
. REQUIRED    : NLOLFLD2,NLOLFLD3,NLOLFLD4
. RETURNED    : LOL RECORD
. DESCRIPTION : AIM LOL FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLOLAIM  BRANCH    NLOLFLG2 TO NLOL9
         CALL      NLOLOPN2
NLOL9    TRAP      IOMssg Giving Error if IO
         Branch    NLOLlock to NLOL9L,NLOL9R,NLOL9N
NLOL9L   FILEPI    1;NLOLFLE2
         READ      NLOLFLE2,NLOLFLD2,NLOLFLD3,NLOLFLD4;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL9R
         READLK    NLOLFLE2,NLOLFLD2,NLOLFLD3,NLOLFLD4;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL9N
         READ      NLOLFLE2,NLOLFLD2,NLOLFLD3,NLOLFLD4;NLOLVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLAIMA
. REQUIRED    : NLOLFLD2,NLOLFLD3,NLOLFLD4
. RETURNED    : NLOL2 = NLOLCODE, NLOLSTAT; NLOL24 = NLOLLOL, NLOLCNUM, NLOLLIST,NLOLLCR
. DESCRIPTION : AIM LOL FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLOLAIMA BRANCH    NLOLFLG2 TO NLOL9A
         CALL      NLOLOPN2
NLOL9A   TRAP      IOMssg Giving Error if IO
         Branch    NLOLlock to NLOL9LA,NLOL9RA,NLOL9NA
NLOL9LA  FILEPI    1;NLOLFLE2
         READ      NLOLFLE2,NLOLFLD2,NLOLFLD3,NLOLFLD4;NLOL2,NLOL24
         TRAPCLR   IO
         RETURN
NLOL9RA
         READLK    NLOLFLE2,NLOLFLD2,NLOLFLD3,NLOLFLD4;NLOL2,NLOL24
         TRAPCLR   IO
         RETURN
NLOL9NA
         READ      NLOLFLE2,NLOLFLD2,NLOLFLD3,NLOLFLD4;NLOL2,NLOL24
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : LOL RECORD
. DESCRIPTION : AIM KEY GENERIC LOL FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLOLKG   BRANCH    NLOLFLG2 TO NLOL10
         CALL      NLOLOPN2
NLOL10   TRAP      IOMssg Giving Error if IO
         Branch    NLOLlock to NLOL10L,NLOL10R,NLOL10N
NLOL10L  FILEPI    1;NLOLFLE2
         READKG    NLOLFLE2;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL10R
         READKGLK  NLOLFLE2;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL10N
         READKG    NLOLFLE2;NLOLVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLKGA
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : NLOL2 = NLOLCODE, NLOLSTAT; NLOL24 = NLOLLOL, NLOLCNUM, NLOLLIST,NLOLLCR
. DESCRIPTION : AIM KEY GENERIC LOLFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLOLKGA  BRANCH    NLOLFLG2 TO NLOL10A
         CALL      NLOLOPN2
NLOL10A  TRAP      IOMssg Giving Error if IO
         Branch    NLOLlock to NLOL10LA,NLOL10RA,NLOL10NA
NLOL10LA FILEPI    1;NLOLFLE2
         READKG    NLOLFLE2;NLOL2,NLOL24
         TRAPCLR   IO
         RETURN
NLOL10RA
         READKGLK  NLOLFLE2;NLOL2,NLOL24
         TRAPCLR   IO
         RETURN
NLOL10NA
         READKG    NLOLFLE2;NLOL2,NLOL24
         TRAPCLR   IO
         RETURN
..............................................................................
.
.         IFZ      PC
. ENTRY POINT : NLOLKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : LOL RECORD
. DESCRIPTION : Aim KEY GENERIC PRIOR LOL FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLOLKGP  BRANCH    NLOLFLG2 TO NLOL11
         CALL      NLOLOPN2
NLOL11   TRAP      IOMssg Giving Error if IO
         branch    NLOLlock to NLOL11L,NLOL11R,NLOL11N
NLOL11L  FILEPI    1;NLOLFLE2
         READKGP   NLOLFLE2;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL11R
         READKGPLK NLOLFLE2;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL11N
         READKGP   NLOLFLE2;NLOLVARS
         TRAPCLR   IO
         RETURN
.         XIF
..............................................................................
.
. ENTRY POINT : NLOLlast
. REQUIRED    : NLOLFLD2,NLOLFLD3,NLOLFLD4
. RETURNED    : LOL RECORD
. DESCRIPTION : AIM LOL FILE READ last matching record
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NLOLlast BRANCH    NLOLFLG2 TO NLOL13
         CALL      NLOLOPN2
NLOL13   TRAP      IOMssg Giving Error if IO
         Branch    NLOLlock to NLOL13L,NLOL13R,NLOL13N
NLOL13L  FILEPI    1;NLOLFLE2
         READLast  NLOLFLE2,NLOLFLD2,NLOLFLD3,NLOLFLD4;NLOLVARS
         TRAPCLR   IO
         RETURN
NLOL13R
         READLastLK NLOLFLE2,NLOLFLD2,NLOLFLD3,NLOLFLD4;NLOLVARS
         TRAPCLR    IO
         RETURN
NLOL13N
         READLast  NLOLFLE2,NLOLFLD2,NLOLFLD3,NLOLFLD4;NLOLVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NLOLBUSY
. REQUIRED    : NLOLFLD
. RETURNED    : LOL RECORD WITH BUSY BYTE SET TO "*"
. DESCRIPTION : CALLS NLOLTST THEN UPDATES BUSY BYTE
.
NLOLBUSY MOVE      C1,NLOLPATH
         BRANCH    NLOLFLAG TO NLOL14
         CALL      NLOLOPEN
NLOL14   READTAB   NLOLFILE,NLOLFLD;*1,NLOLCODE;
         if over
                   move   "NLOLBUSY",Location
                   clear  Error
                   append NLOLFLD,Error
                   append " - Not Found in File!",Error
                   reset  Error
                   goto   IOMssg
         endif
         MOVE      STAR,NLOLCODE
         TRAP      IOMssg Giving Error if IO
         FILEPI    1;NLOLFILE
         UPDATAB   NLOLFILE;*1,NLOLCODE
         TRAPCLR   IO
         RETURN
.
..............................................................................
.
. ENTRY POINT : NLOLRELEASE
. REQUIRED    : NLOLFLD
. RETURNED    : LOL RECORD WITH BUSY BYTE SET TO " "
. DESCRIPTION : CALLS NLOLTST THEN RELEASES BUSY BYTE
.
NLOLRELEASE
         MOVE      C1,NLOLPATH
         BRANCH    NLOLFLAG TO NLOL15
         CALL      NLOLOPEN
NLOL15   READTAB   NLOLFILE,NLOLFLD;*1,NLOLCODE;
         IF NOT OVER
                   MOVE      " ",NLOLCODE
                   TRAP      IOMssg Giving Error if IO
                   FILEPI    1;NLOLFILE
                   UPDATAB   NLOLFILE;*1,NLOLCODE
                   TRAPCLR   IO
         ENDIF
         RETURN

...............................................................................
.
. ENTRY POINT : NLOLOPEN
. REQUIRED    : 'NLOLPATH'
. RETURNED    : 'NLOLFLAG' SET TO THE PATH NUMBER: 'NLOLPATH', IF OPENNED.
. DESCRIPTION : OPEN NIN LOL FILE (HIDDEN KEY), DEPENDENT ON PATH NUMBER.
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NLOLOPEN LOAD      NLOLNAME USING NLOLPATH FROM NLOLNME,NLOLNME1,NLOLNME2
         BRANCH    NLOLPATH TO NLOLOPN,NLOLOPN1,NLOLOPN2
NLOLOPN
         TRAP      NLOLGONE IF IO
         branch    NLOLlock to NLOLop,NLOLopR,NLOLop
NLOLop   OPEN      NLOLFILE,NLOLNAME
         TRAPCLR   IO
         MOVE      C1 TO NLOLFLAG
         RETURN
NLOLopR  OPEN      NLOLFILE,NLOLNAME,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NLOLFLAG
         RETURN
NLOLGONE MOVE      NLOLNAME TO FILENAME
         CALL      FILEGONE
.
...............................................................................
.
. ENTRY POINT : NLOLOPN1
. REQUIRED    :
. RETURNED    : 'NLOLFLG1' SET TO C1, IF OPENNED.
. DESCRIPTION : OPEN NIN LOL FILE1, 'CAMPAIGN' KEY WHICH ALLOWS DUPES
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NLOLOPN1 TRAP      NLOLGNE1 IF IO
         branch    NLOLlock to NLOLop1,NLOLop1R,NLOLop1
NLOLop1  OPEN      NLOLFLE1,NLOLNME1
         TRAPCLR   IO
         MOVE      C1 TO NLOLFLG1
         RETURN
NLOLop1R OPEN      NLOLFLE1,NLOLNME1,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NLOLFLG1
         RETURN
NLOLGNE1 MOVE      NLOLNME1 TO FILENAME
         CALL      FILEGONE
...............................................................................
.
. ENTRY POINT : NLOLOPN2
. REQUIRED    :
. RETURNED    : 'NLOLFLG2' SET TO '1', IF OPENNED.
. DESCRIPTION : OPEN NIN LOL FILE, AIMDEXED
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NLOLOPN2 TRAP      NLOLGNE2 IF IO
         branch    NLOLlock to NLOLop2,NLOLop2r,NLOLop2
NLOLop2  OPEN      NLOLFLE2,NLOLNME2
         TRAPCLR   IO
         MOVE      C1 TO NLOLFLG2
         RETURN
NLOLop2R OPEN      NLOLFLE2,NLOLNME2,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NLOLFLG2
         RETURN
NLOLGNE2 MOVE      NLOLNME2 TO FILENAME
         CALL      FILEGONE
.
         TRAPCLR   IO
         RETURN
...............................................................................
NLOLerr  move     "invalid path" to filename
         call     filegone
         trapclr  io
         return
..............................................................................

