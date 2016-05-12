..............................................................................
.
. NORDIO INCLUSION
. NIN ORDER FILE I/O ROUTINES
.
. patch 22  order file cinversion
. PATCH 1.1 ADDED LOGIC FOR NEW ISAM KEY - BASED ON CAMPAIGN NUMBER
.
. FILE NAME : NINORD
. REC LENGTH: 262 FIXED
. INDEX KEY : (1)  7-12       (LR#)
.START PATCH 1.1 ADDED LOGIC
. INDEX KEY : (2)  156-161    (CAMPAIGN#)
.END PATCH 1.1 ADDED LOGIC
.AIMDEX KEYS: (1)  3-6   (MAILER#)
.             (2)  16-21 (LIST#)
.             (3)  26-32 (PO#)
.             (4) 269-272(broker#)
..............................................................................
.
. ENTRY POINT : NORDKEY
. REQUIRED    : 'NORDPATH', 'NORDFLD', or 'nordfld4
. RETURNED    : ORDER RENORD
. DESCRIPTION : EXACT ISAM KEY READ
.
.NORDKEY  COMPARE   NORDPATH TO NORDFLAG
.START PATCH 1.1 REPLACED LOGIC
.NORDKEY  branch    nordpath to nord1a,norderr
NORDKEY  branch    nordpath to nord1a,norderr,norderr,nord1e
.END PATCH 1.1 REPLACED LOGIC
.NORDKEY  branch    nordpath to nord1a,norderr,nord1c
nord1a   compare   nordpath to nordflag
         GOTO      NORD1b IF EQUAL
         CALL      NORDOPEN
NORD1B   TRAP      IOMssg Giving Error if IO
         branch    nordlock to nord1bL,nord1bR,nord1bN
nord1bL  FILEPI    1;NORDFILE
         READ      NORDFILE,NORDFLD;ORDVARS
         TRAPCLR   IO
         RETURN
nord1bR
         READLK      NORDFILE,NORDFLD;ORDVARS
         TRAPCLR     IO
         RETURN
nord1bN
         READ      NORDFILE,NORDFLD;ORDVARS
         TRAPCLR   IO
         RETURN
.
.nord1c   branch    nordflg3 to nord1d
.         CALL      NORDOPN3
NORD1d
.          FILEPI    1;NORDFLE3
.         READ      NORDFLE3,NORDFLD4;ORDVARS
.
          RETURN
.START PATCH 1.1 ADDED LOGIC
nord1e   BRANCH    NORDFLG4 TO NORD1F
         CALL      NORDOpen
NORD1f   TRAP      IOMssg Giving Error if IO
         branch    nordlock to nord1fL,nord1fR,nord1fN
nord1fL  FILEPI    1;NORDFLE4
         READ      NORDFLE4,NORDFLDC;ORDVARS
         TRAPCLR   IO
         RETURN
nord1fR
         READLK    NORDFLE4,NORDFLDC;ORDVARS
         TRAPCLR   IO
         RETURN
nord1fN
         READ      NORDFLE4,NORDFLDC;ORDVARS
         TRAPCLR   IO
         RETURN
.END PATCH 1.1 ADDED LOGIC
..............................................................................
.
. ENTRY POINT : NORDTST
. REQUIRED    : NORDFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
. 01/24/2000 - ASH Str1 is used to determine if record is Busy.  Keep str1 active!!!!!
.
.NORDTST  COMPARE   NORDPATH TO NORDFLAG
.START PATCH 1.1 REPLACED LOGIC
.NORDTST  branch    nordpath to nord2a,norderr,nord2c
NORDTST  branch    nordpath to nord2a,norderr,nord2c,NORD2E
.END PATCH 1.1 REPLACED LOGIC
nord2a   COMPARE   NORDPATH TO NORDFLAG
         GOTO      NORD2b IF EQUAL
         CALL      NORDOPEN
NORD2b   TRAP      IOMssg Giving Error if IO
         FILEPI    1;NORDFILE
         READ      NORDFILE,NORDFLD;STR1
         TRAPCLR   IO
         RETURN
.
.nord2c  branch    NORDFLG3 to nord2d
.         CALL      NORDOPN3
.NORD2d   FILEPI    1;NORDFLE3
.         READ      NORDFLE3,NORDFLD4;;
.         RETURN
.nord2e   BRANCH           NORDFLG2 TO NORD2f
nord2c   BRANCH    NORDFLG2 TO NORD2d
         CALL      NORDOpen
.NORD2f   FILEPI    1;NORDFLE2
NORD2d   TRAP      IOMssg Giving Error if IO
         FILEPI    1;NORDFLE2
         READ      NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;str1
         TRAPCLR   IO
         RETURN
.START PATCH 1.1 ADDED LOGIC
nord2E   BRANCH    NORDFLG4 TO NORD2F
         CALL      NORDOpen
NORD2F   TRAP      IOMssg Giving Error if IO
         FILEPI    1;NORDFLE4
         READ      NORDFLE4,NORDFLDC;str6,str6
         TRAPCLR   IO
         RETURN
.END PATCH 1.1 ADDED LOGIC
..............................................................................
.
. ENTRY POINT : NORDKS
. REQUIRED    : 'NORDPATH'
. RETURNED    : ORDER RENORD
. DESCRIPTION : KEY SEQUENTIAL ORDER FILE READ
.
.START PATCH 1.1 REPLACED LOGIC
.nordks
..NORDKS   COMPARE   NORDPATH TO NORDFLAG
..NORDKS   branch    nordpath to nord3a,norderr,nord3c
....................
NORDKS   branch    nordpath to nord3a,norderr,norderr,NORD3E,NORD3G
.END PATCH 1.1 REPLACED LOGIC
nord3a   COMPARE   NORDPATH TO NORDFLAG
         GOTO      NORD3b IF EQUAL
         CALL      NORDOPEN
NORD3b   TRAP      IOMssg Giving Error if IO
         branch    nordlock to nord3bL,nord3bR,nord3bN

nord3bL  FILEPI    1;NORDFILE
         READKS    NORDFILE;ORDVARS
         TRAPCLR   IO
         RETURN
nord3bR
         READKSLK  NORDFILE;ORDVARS
         TRAPCLR   IO
         RETURN
nord3bN
         READKS    NORDFILE;ORDVARS
         TRAPCLR   IO
         RETURN
.
.nord3c          branch    NORDFLG3 to nord3d
.         CALL      NORDOPN3
.NORD3d    FILEPI    1;NORDFLE3
.         READKS    NORDFLE3;ORDVARS
.
.         RETURN
.START PATCH 1.1 ADDED LOGIC
nord3E   branch    NORDFLG4 to nord3F
         CALL      NORDOpen
NORD3F   TRAP      IOMssg Giving Error if IO
         branch    nordlock to nord3FL,nord3FR,nord3FN

nord3FL  FILEPI    1;NORDFLE4
         READKS    NORDFLE4;ORDVARS
         TRAPCLR   IO
         RETURN
nord3FR
         READKSLK  NORDFLE4;ORDVARS
         TRAPCLR   IO
         RETURN
nord3FN
         READKS    NORDFLE4;ORDVARS
         TRAPCLR   IO
         RETURN
.
nord3G   branch    NORDFLG4 to nord3H
         CALL      NORDOpen
NORD3H   TRAP      IOMssg Giving Error if IO
         branch    nordlock to nord3HL,nord3HR,nord3HN

nord3HL  FILEPI    1;NORDFLE4
         READKS    NORDFLE4;str6,str6
         TRAPCLR   IO
         RETURN
nord3HR
         READKSLK  NORDFLE4;str6,str6
         TRAPCLR   IO
         RETURN
nord3HN
         READKS    NORDFLE4;str6,str6
         TRAPCLR   IO
         RETURN
.END PATCH 1.1 ADDED LOGIC
..............................................................................
.
. ENTRY POINT : NORDSEQ
. REQUIRED    :
. RETURNED    : ORDER RENORD
. DESCRIPTION : SEQUENTIAL ORDER FILE READ
.
NORDSEQ  BRANCH    NORDFLAG TO NORD4
         CALL      NORDOPEN
NORD4    TRAP      IOMssg Giving Error if IO
         branch    nordlock to nord4L,Nord4R,nord4N
Nord4L   FILEPI    1;NORDFILE
         READ      NORDFILE,SEQ;ORDVARS
         TRAPCLR   IO
         RETURN
Nord4R
         READLK      NORDFILE,SEQ;ORDVARS
         TRAPCLR     IO
         RETURN
Nord4N
         READ      NORDFILE,SEQ;ORDVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NORDWRT
. REQUIRED    : 'NORDFLD', 'NORDPATH=1'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT., ISI 2/3 INSERTS.
.
.begin patch 22
.NORDWRT  MOVE      C1 TO NORDPATH
.NORDWRT1 COMPARE   NORDPATH TO NORDFLAG
          Branch    Nordflag to Nord5
         GOTO      NORD5 IF EQUAL
         CALL      NORDOPEN
NORD5    TRAP      IOMssg Giving Error if IO
.         FILEPI    1;NORDFILE
         FILEPI    1;NORDFList
         WRITE     NORDFLIst,NordVars
.         WRITE     NORDFILE,NORDFLD;ORDVARS
.
.         CALL      NORDWRT2
.        CALL      NORDWRT3
..START PATCH 1.1 ADDED LOGIC
.         if (OCAMP <> "")
.                   CALL      NORDWRT4
.         endif
..END PATCH 1.1 ADDED LOGIC
.end patch 22
         TRAPCLR   IO
         RETURN
.
.begin patch 22
.NORDWRT2 BRANCH    NORDFLG2 TO NORD5B
.         CALL      NORDOpen
.NORD5B   FILEPI    1;NORDFLE2
.         INSERT    NORDFLE2
.         RETURN
..
..NORDWRT3 BRANCH    NORDFLG3 TO NORD5C
..         CALL      NORDOPN3
.NORD5c
..   FILEPI    1;NORDFLE3
..         INSERT    NORDFLE3,NORDFLD4
.         RETURN
..START PATCH 1.1 ADDED LOGIC
.NORDWRT4 BRANCH    NORDFLG4 TO NORD5D
.         CALL      NORDOpen
.NORD5D   FILEPI    1;NORDFLE4
.         INSERT    NORDFLE4,OCAMP
.         RETURN
..END PATCH 1.1 ADDED LOGIC
.end patch 22
.
..............................................................................
.
. ENTRY POINT : NORDUPD
. REQUIRED    : A PREVIOUS KEY READ, 'NORDPATH'
. RETURNED    :
. DESCRIPTION : KEY UPDATE ORDER FILE
.Programmers responsability to update any other indices
.
.begin patch 22
.NORDUPD  COMPARE   NORDPATH TO NORDFLAG
NORDUPD  Branch     Nordflag to Nord6
.         GOTO      NORD6 IF EQUAL
         CALL      NORDOPEN
NORD6    TRAP      IOMssg Giving Error if IO
.         FILEPI    1;NORDFILE
.         UPDATE    NORDFILE;ORDVARS
         FILEPI    1;NORDFLIst
         UPDATE    NORDFList;ORDVARS
.end patch 22
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NORDDEL
. REQUIRED    : 'NORDPATH', 'NORDFLD' Previous Valid read
. RETURNED    :
. DESCRIPTION : PATH=1 DELETES TXT & ALL KEYS. PATH=2/3/4 DELETES KEY ONLY.
.
.begin patch 22
.NORDDEL  BRANCH    NORDPATH TO NORDDEL1,NORDDEL2
NORDDEL  BRANCH    NORDFflag TO NORD7
.
.NORDDEL1 COMPARE   NORDPATH TO NORDFLAG
.         GOTO      NORD7 IF EQUAL
         CALL      NORDOPEN
NORD7
.          CALL      NORDTST
.         RETURN    IF OVER
.         MOVE      OLRN TO NORDFLD
.         TRAP      IOMssg Giving Error if IO
.         FILEPI    1;NORDFILE
.         DELETE    NORDFILE,NORDFLD
         FILEPI    1;NORDFLIst
         DELETE    NORDFList
         TRAPCLR   IO
         RETURN
.
.NORDDEL2 BRANCH    NORDFLG2 TO NORD7B
.         CALL      NORDOpen
.NORD7B   TRAP      IOMssg Giving Error if IO
.         FILEPI    1;NORDFLE2
.         DELETE    NORDFLE2
.         TRAPCLR   IO
.         RETURN
.end patch 22
.
..............................................................................
.
. ENTRY POINT : NORDKP
. REQUIRED    : 'NORDPATH'
. RETURNED    : ORDER RENORD
. DESCRIPTION : KEY SEQUENTIAL ORDER FILE READ
.
.START PATCH 1.1 REPLACED LOGIC
.NORDKP   COMPARE   NORDPATH TO NORDFLAG
.         GOTO      NORD8A IF EQUAL
.         CALL      NORDOPEN
.NORD8    TRAP      IOMssg Giving Error if IO
....................
NORDKP   BRANCH    NORDPATH TO NORD8,NORDERR,NORDERR,NORD8C
NORD8    COMPARE   NORDPATH TO NORDFLAG
         GOTO      NORD8A IF EQUAL
         CALL      NORDOPEN
NORD8A   TRAP      IOMssg Giving Error if IO
.END PATCH 1.1 REPLACED LOGIC
         Branch    nordlock to nord8AL,Nord8AR,nord8AN
Nord8AL  FILEPI    1;NORDFILE
         READKP    NORDFILE;ORDVARS
         TRAPCLR   IO
         RETURN
Nord8AR
         READKPLK    NORDFILE;ORDVARS
         TRAPCLR     IO
         RETURN
Nord8AN
         READKP    NORDFILE;ORDVARS
         TRAPCLR   IO
         RETURN
.START PATCH 1.1 ADDED LOGIC
NORD8C   BRANCH    NORDFLG4 TO NORD8D
         CALL      NORDOpen
NORD8D   TRAP      IOMssg Giving Error if IO
         Branch    nordlock to nord8DL,Nord8DR,nord8DN
Nord8DL  FILEPI    1;NORDFLE4
         READKP    NORDFLE4;ORDVARS
         TRAPCLR   IO
         RETURN
Nord8DR
         READKPLK  NORDFLE4;ORDVARS
         TRAPCLR   IO
         RETURN
Nord8DN
         READKP    NORDFLE4;ORDVARS
         TRAPCLR   IO
         RETURN
.END PATCH 1.1 ADDED LOGIC
..............................................................................
.
. ENTRY POINT : NORDAIM
. REQUIRED    : NORDFLD1,NORDFLD2,NORDFLD3,nordfld4,NORDFLD6,NORDFLD7,NORDFLD8
. RETURNED    : ORDER RECORD
. DESCRIPTION : AIM ORDER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
.START ASH PATCH 5/2/2003 ADDED FOLLOWING AAMDICES
.NORDFLD6
.NORDFLD7
.NORDFLD8
.END ASH PATCH 5/2/2003 ADDED FOLLOWING AAMDICES
NORDAIM  BRANCH    NORDFLG2 TO NORD9
.        GOTO      NORD9 IF EQUAL
         CALL      NORDOpen
NORD9    TRAP      IOMssg Giving Error if IO
         Branch    Nordlock to Nord9L,NORD9R,NOrd9N
Nord9L   FILEPI    1;NORDFLE2
         READ      NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;ORDVARS
         TRAPCLR   IO
         RETURN
Nord9R
         READLK    NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;ORDVARS
         TRAPCLR   IO
         RETURN
Nord9N
         READ      NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;ORDVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NORDAIMA
. REQUIRED    : NORDFLD1,NORDFLD2,NORDFLD3,nordfld4,NORDFLD6,NORDFLD7,NORDFLD8
. RETURNED    : Str6 = OLRN
. DESCRIPTION : AIM ORDER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDAIMA BRANCH    NORDFLG2 TO NORD9A
.        GOTO      NORD9A IF EQUAL
         CALL      NORDOpen
NORD9A   TRAP      IOMssg Giving Error if IO
         Branch    Nordlock to Nord9LA,NORD9RA,NOrd9NA
Nord9LA  FILEPI    1;NORDFLE2
         READ      NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;str6,str6
         TRAPCLR   IO
         RETURN
Nord9RA
         READLK    NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;str6,str6
         TRAPCLR   IO
         RETURN
Nord9NA
         READ      NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;str6,str6
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NORDAIMt
. REQUIRED    : NORDFLD1,NORDFLD2,NORDFLD3,nordfld4,NORDFLD6,NORDFLD7,NORDFLD8
. RETURNED    : OVER/OR NOT
. DESCRIPTION : AIM ORDER FILE TEST READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDAIMT  BRANCH   NORDFLG2 TO NORD12
.        GOTO      NORD9 IF EQUAL
         CALL      NORDOpen
NORD12   TRAP      IOMssg Giving Error if IO
         Branch    nordlock to NORD12L,Nord12R,Nord12N
Nord12L  FILEPI    1;NORDFLE2
.         READ      NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4;*26,str12
        READ      NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;*7,str6,*26,str12
.         TRAPCLR   IO
         RETURN
Nord12R
.         READLK    NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4;*26,str12
         READLK    NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;*7,str6,*26,str12
         TRAPCLR   IO
         RETURN
Nord12N
.         READ      NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4;*26,str12
         READ      NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;*7,str6,*26,str12
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NORDKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : ORDER RECORD
. DESCRIPTION : AIM KEY GENEREIC ORDERFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDKG   BRANCH    NORDFLG2 TO NORD10
         CALL      NORDOpen
NORD10   TRAP      IOMssg Giving Error if IO
         Branch    Nordlock to Nord10L,Nord10R,Nord10N
Nord10L  FILEPI    1;NORDFLE2
         READKG    NORDFLE2;ORDVARS
.         SCAN      "F *" IN ERROR
.         if         equal
.         display   *p1:20,olrn,*w2
.         endif
         TRAPCLR   IO
         RETURN
Nord10R
         READKGLK    NORDFLE2;ORDVARS
.         SCAN      "F *" IN ERROR
.         if         equal
.         display   *p1:20,olrn,*w2
.         endif
         TRAPCLR   IO
         RETURN
Nord10N
         READKG    NORDFLE2;ORDVARS
.         SCAN      "F *" IN ERROR
.         if         equal
.         display   *p1:20,olrn,*w2
.         endif
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NORDKGA
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : str6 = OLRN
. DESCRIPTION : AIM KEY GENEREIC ORDERFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDKGA  BRANCH    NORDFLG2 TO NORD10A
         CALL      NORDOpen
NORD10A  TRAP      IOMssg Giving Error if IO
         Branch    Nordlock to Nord10LA,Nord10RA,Nord10NA
Nord10LA FILEPI    1;NORDFLE2
         READKG    NORDFLE2;str6,str6
         TRAPCLR   IO
         RETURN
Nord10RA
         READKGLK    NORDFLE2;str6,str6
         TRAPCLR   IO
         RETURN
Nord10NA
         READKG    NORDFLE2;str6,str6
         TRAPCLR   IO
         RETURN
..............................................................................
.
.         IFZ      PC
. ENTRY POINT : NORDKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : ORDER RECORD
. DESCRIPTION : Aim KEY GENERIC PRIOR ORDER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDKGP  BRANCH    NORDFLG2 TO NORD11
         CALL      NORDOpen
NORD11   TRAP      IOMssg Giving Error if IO
         branch    Nordlock to Nord11L,Nord11R,Nord11N
Nord11L  FILEPI    1;NORDFLE2
         READKGP   NORDFLE2;ORDVARS
         TRAPCLR   IO
         RETURN
Nord11R
         READKGPLK   NORDFLE2;ORDVARS
         TRAPCLR     IO
         RETURN
Nord11N
         READKGP   NORDFLE2;ORDVARS
         TRAPCLR   IO
         RETURN
.         XIF
...............................................................................
.
. ENTRY POINT : NORDKGAT
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : str6 = OLRN, str12 = OMLRPON
. DESCRIPTION : AIM KEY GENEREIC ORDERFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDKGAT BRANCH    NORDFLG2 TO NORD12A
         CALL      NORDOpen
NORD12A  TRAP      IOMssg Giving Error if IO
         Branch    Nordlock to Nord12LA,Nord12RA,Nord12NA
Nord12LA FILEPI    1;NORDFLE2
         READKG    NORDFLE2;*7,str6,*26,str12
         TRAPCLR   IO
         RETURN
Nord12RA
         READKGLK    NORDFLE2;*7,str6,*26,str12
         TRAPCLR   IO
         RETURN
Nord12NA
         READKG    NORDFLE2;*7,str6,*26,str12
         TRAPCLR   IO
         RETURN
.............................................................................
.
. ENTRY POINT : NORDlast
. REQUIRED    : NORDFLD1,NORDFLD2,NORDFLD3,nordfld4,NORDFLD6,NORDFLD7,NORDFLD8
. RETURNED    : ORDER RECORD
. DESCRIPTION : AIM ORDER FILE READ last matching record
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NORDlast BRANCH    NORDFLG2 TO NORD13
.        GOTO      NORD13 IF EQUAL
         CALL      NORDOpen
NORD13   TRAP      IOMssg Giving Error if IO
         Branch    Nordlock to nord13L,Nord13R,Nord13N
Nord13L  FILEPI    1;NORDFLE2
         READLast  NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;ORDVARS
         TRAPCLR   IO
         RETURN
Nord13R
         READLastLK NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;ORDVARS
         TRAPCLR    IO
         RETURN
Nord13N
         READLast  NORDFLE2,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,NORDFLD6,NORDFLD7,NORDFLD8;ORDVARS
         TRAPCLR   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NORDBUSY
. REQUIRED    : NORDFLD
. RETURNED    : ORDER RECORD WITH FIRST BYTE SET TO "*"
. DESCRIPTION : CALLS NORDTST THEN UPDATES BUSY BYTE
.
NORDBUSY MOVE      C1,NORDPATH
         BRANCH    NORDFLAG TO NORD14
         CALL      NORDOPEN
NORD14   CALL      NORDTST
         if over
                   move   "NORDBUSY",Location
                   clear  Error
                   append NORDFLD,Error
                   append " - Not Found in File!",Error
                   reset  Error
                   goto   IOMssg
         endif
         MOVE      STAR,ORCODE
         TRAP      IOMssg Giving Error if IO
         FILEPI    1;NORDFILE
         UPDATAB   NORDFILE;*1,ORCODE
         TRAPCLR   IO
         RETURN
.
..............................................................................
.
. ENTRY POINT : NORDRELEASE
. REQUIRED    : NORDFLD
. RETURNED    : ORDER RECORD WITH FIRST BYTE SET TO "S"
. DESCRIPTION : CALLS NORDTST THEN RELEASES BUSY BYTE
.
NORDRELEASE
         MOVE      C1,NORDPATH
         BRANCH    NORDFLAG TO NORD15
         CALL      NORDOPEN
NORD15   CALL      NORDTST
         MOVE      "S",ORCODE
         TRAP      IOMssg Giving Error if IO
         FILEPI    1;NORDFILE
         UPDATAB   NORDFILE;*1,ORCODE
         TRAPCLR   IO
         RETURN
..............................................................................
...............................................................................
.
. ENTRY POINT : NORDOPEN
. REQUIRED    : 'NORDPATH'
. RETURNED    : 'NORDFLAG' SET TO THE PATH NUMBER: 'NORDPATH', IF OPENNED.
. DESCRIPTION : OPEN NIN ORDER FILE, DEPENDENT ON PATH NUMBER.
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.begin patch 22
.
.NORDOPEN LOAD      NORDNAME USING NORDPATH FROM NORDNME1,NORDNME2,NORDNME3
NORDOPEN 
         TRAP      NORDGONE IF IO
         branch    nordlock to nordop1,Nordop1R,nordop1
.nordop1  OPEN      NORDFILE,NORDNAME
nordop1  OPEN      NORDFList
         TRAPCLR   IO
         MOVE      c1 TO NORDFLAG
         MOVE      c1 TO NORDFLG2
         MOVE      c1 TO NORDFLG4
         RETURN
nordop1R OPEN      NORDFLISt,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      c1 TO NORDFLAG
         MOVE      c1 TO NORDFLG2
         MOVE      c1 TO NORDFLG4
         RETURN
NORDGONE MOVE      NORDNAME TO FILENAME
         CALL      FILEGONE
.
.end patch 22
...............................................................................
norderr  move     "invalid path" to filename
         call     filegone
         trapclr  io
         return
..............................................................................
