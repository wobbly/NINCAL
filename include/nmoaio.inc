...............................................................................
.ONACCOUNTIO/INC - MONEY'S ON ACCOUNT I/O MODULES.
...............................................................................
. 27OCT98 ASH-added logic with Y2K conversion
. 28dec95 on write use actual vatriables and *zf on brk number. DLh
. tighten up code and PI's around write  2jun95 DLH.
. primary write & update access changed to nmobfle2 9/26/94 DLH.
.17may95 add key by lr  DLH
.12may94 added new file by brk/mlr
.17mar94 added brknum
.LAST UPDATE:  DLH 23JUL93  KEY SEQ CHANGED TO MATCH OTHER INCLUDES.
...............................................................................
.
. ENTRY POINT : NMOAKEY
. REQUIRED    : 'NMOAFLD'
. RETURNED    : MONEY ON ACCOUNT DETAIL RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NMOAKEY  BRANCH    NMOAPATH OF NMOA1A,NMOA1C,nmoa1x,nmoa1e,nmoa1g
.
NMOA1A   BRANCH    NMOAFLAG TO NMOA1B
         CALL      NMOAOPEN
NMOA1B   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA1BL,NMOA1BR,NMOA1BN
NMOA1BL  FILEPI    1;NMOAFILE
         READ      NMOAFILE,NMOAFLD;moavars               .mlr/cnt
         trapclr   IO
         RETURN
NMOA1BR  READLK    NMOAFILE,NMOAFLD;moavars               .mlr/cnt
         trapclr   IO
         RETURN
NMOA1BN  READ      NMOAFILE,NMOAFLD;moavars               .mlr/cnt
         trapclr   IO
         RETURN
.
NMOA1C   BRANCH    NMOAFLG2 TO NMOA1D
         CALL      NMOAOPEN
NMOA1D   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA1DL,NMOA1DR,NMOA1DN
NMOA1DL  FILEPI    1;NMOAFLE2
         READ      NMOAFLE2,NMOAFLD;moavars                 .trans
         trapclr   IO
         RETURN
NMOA1DR  READLK    NMOAFLE2,NMOAFLD;moavars                 .trans
         trapclr   IO
         RETURN
NMOA1DN  READ      NMOAFLE2,NMOAFLD;moavars                 .trans
         trapclr   IO
         RETURN
.

NMOA1e   BRANCH    NMOAFLG4 TO NMOA1f
         CALL      NMOAOPEN
NMOA1f   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA1FL,NMOA1FR,NMOA1FN
NMOA1FL  FILEPI    1;NMOAFLE4
         READ      NMOAFLE4,NMOAFLD4;moavars                 .brk/mlr
         trapclr   IO
         RETURN
NMOA1FR  READLK    NMOAFLE4,NMOAFLD4;moavars                 .brk/mlr
         trapclr   IO
         RETURN
NMOA1FN  READ      NMOAFLE4,NMOAFLD4;moavars                 .brk/mlr
         trapclr   IO
         RETURN
.
NMOA1g   BRANCH    NMOAFLG5 TO NMOA1h
         CALL      NMOAOPEN
NMOA1h   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA1HL,NMOA1HR,NMOA1HN
NMOA1HL  FILEPI    1;NMOAFLE5
         READ      NMOAFLE5,NMOAFLD5;moavars                 .lr
         trapclr   IO
         RETURN
NMOA1HR  READLK    NMOAFLE5,NMOAFLD5;moavars                 .lr
         trapclr   IO
         RETURN
NMOA1HN  READ      NMOAFLE5,NMOAFLD5;moavars                 .lr
         trapclr   IO
         RETURN
.
nmoa1x   return              .requested a flat seq read - called wrong code.
...............................................................................
.
. ENTRY POINT : NMOATST
. REQUIRED    : NMOAFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NMOATST  BRANCH    NMOAPATH TO NMOA2A,NMOA2C,nmoa2x,nmoa2e,nmoa2g
NMOA2A   BRANCH    NMOAFLAG TO NMOA2B
         CALL      NMOAOPEN
NMOA2B   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA2BL,NMOA2BR,NMOA2BN
NMOA2BL  FILEPI    1;NMOAFILE
         READ      NMOAFILE,NMOAFLD;STR1
         trapclr   IO
         RETURN
NMOA2BR  READLK    NMOAFILE,NMOAFLD;STR1
         trapclr   IO
         RETURN
NMOA2BN  READ      NMOAFILE,NMOAFLD;STR1
         trapclr   IO
         RETURN
.
NMOA2C   BRANCH    NMOAFLG2 TO NMOA2D
         CALL      NMOAOPEN
NMOA2D   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA2DL,NMOA2DR,NMOA2DN
NMOA2DL  FILEPI    1;NMOAFLE2
         READ      NMOAFLE2,NMOAFLD;STR1
         trapclr   IO
         RETURN
NMOA2DR  READLK    NMOAFLE2,NMOAFLD;STR1
         trapclr   IO
         RETURN
NMOA2DN  READ      NMOAFLE2,NMOAFLD;STR1
         trapclr   IO
         RETURN
.
NMOA2e   BRANCH    NMOAFLG4 TO NMOA2f
         CALL      NMOAOPEN
NMOA2f   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA2FL,NMOA2FR,NMOA2FN
NMOA2FL  FILEPI    1;NMOAFLE4
         READ      NMOAFLE4,NMOAFLD4;STR1
         trapclr   IO
         RETURN
NMOA2FR  READLK    NMOAFLE4,NMOAFLD4;STR1
         trapclr   IO
         RETURN
NMOA2FN  READ      NMOAFLE4,NMOAFLD4;STR1
         trapclr   IO
         RETURN
.
NMOA2g   BRANCH    NMOAFLG5 TO NMOA2h
         CALL      NMOAOPEN
NMOA2h   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA2HL,NMOA2HR,NMOA2HN
NMOA2HL  FILEPI    1;NMOAFLE5
         READ      NMOAFLE5,NMOAFLD5;STR1
         trapclr   IO
         RETURN
NMOA2HR  READLK    NMOAFLE5,NMOAFLD5;STR1
         trapclr   IO
         RETURN
NMOA2HN  READ      NMOAFLE5,NMOAFLD5;STR1
         trapclr   IO
         RETURN
.
nmoa2x   return              .requested a flat seq read - called wrong code.
...............................................................................
.key seq read programmers responsiblity to check for correct record
NMOAKS   BRANCH     NMOAPATH OF NMOA3A,NMOA3C,nmoa3x,nmoa3e,nmoa3g
NMOA3A   BRANCH     NMOAFLAG TO NMOA3B
         CALL       NMOAOPEN
NMOA3B   trap       IOMssg giving Error if IO
         branch     NMOALOCK to NMOA3BL,NMOA3BR,NMOA3BN
NMOA3BL  FILEPI    1;NMOAFILE
         READKS    NMOAFILE;moavars
         trapclr   IO
         RETURN
NMOA3BR  READKSLK  NMOAFILE;moavars
         trapclr   IO
         RETURN
NMOA3BN  READKS    NMOAFILE;moavars
         trapclr   IO
         RETURN
.
NMOA3C   BRANCH     NMOAFLG2 TO NMOA3D
         CALL       NMOAOPEN
NMOA3D   trap       IOMssg giving Error if IO
         branch     NMOALOCK to NMOA3DL,NMOA3DR,NMOA3DN
NMOA3DL  FILEPI    1;NMOAFLE2
         READKS    NMOAFLE2;moavars
         trapclr   IO
         RETURN
NMOA3DR  READKSLK  NMOAFLE2;moavars
         trapclr   IO
         RETURN
NMOA3DN  READKS    NMOAFLE2;moavars
         trapclr   IO
         RETURN
.
NMOA3e   BRANCH     NMOAFLG4 TO NMOA3f
         CALL       NMOAOPEN
NMOA3f   trap       IOMssg giving Error if IO
         branch     NMOALOCK to NMOA3FL,NMOA3FR,NMOA3FN
NMOA3FL  FILEPI    1;NMOAFLE4
         READKS    NMOAFLE4;moavars
         trapclr   IO
         RETURN
NMOA3FR  READKSLK  NMOAFLE4;moavars
         trapclr   IO
         RETURN
NMOA3FN  READKS    NMOAFLE4;moavars
         trapclr   IO
         RETURN
.
NMOA3g   BRANCH     NMOAFLG5 TO NMOA3h
         CALL       NMOAOPEN
NMOA3h   trap       IOMssg giving Error if IO
         branch     NMOALOCK to NMOA3HL,NMOA3HR,NMOA3HN
NMOA3HL  FILEPI    1;NMOAFLE5
         READKS    NMOAFLE5;moavars
         trapclr   IO
         RETURN
NMOA3HR  READKSLK  NMOAFLE5;moavars
         trapclr   IO
         RETURN
NMOA3HN  READKS    NMOAFLE5;moavars
         trapclr   IO
         RETURN
.
nmoa3x   return
...............................................................................
.key seq prior read programmers responsiblity to check for correct record
NMOAKp   BRANCH     NMOAPATH OF NMOA8A,NMOA8C,nmoa8x,nmoa8e,nmoa8g
NMOA8A   BRANCH     NMOAFLAG TO NMOA8B
         CALL       NMOAOPEN
NMOA8B   trap       IOMssg giving Error if IO
         branch     NMOALOCK to NMOA8BL,NMOA8BR,NMOA8BN
NMOA8BL  FILEPI    1;NMOAFILE
         READKp    NMOAFILE;moavars
         trapclr   IO
         RETURN
NMOA8BR  READKpLK  NMOAFILE;moavars
         trapclr   IO
         RETURN
NMOA8BN  READKp    NMOAFILE;moavars
         trapclr   IO
         RETURN
.
NMOA8C   BRANCH     NMOAFLG2 TO NMOA8D
         CALL       NMOAOPEN
NMOA8D   trap       IOMssg giving Error if IO
         branch     NMOALOCK to NMOA8DL,NMOA8DR,NMOA8DN
NMOA8DL  FILEPI    1;NMOAFLE2
         READKp    NMOAFLE2;moavars
         trapclr   IO
         RETURN
NMOA8DR  READKpLK  NMOAFLE2;moavars
         trapclr   IO
         RETURN
NMOA8DN  READKp    NMOAFLE2;moavars
         trapclr   IO
         RETURN
.
NMOA8e  BRANCH     NMOAFLG4 TO NMOA8f
         CALL       NMOAOPEN
NMOA8f   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA8FL,NMOA8FR,NMOA8FN
NMOA8FL  FILEPI    1;NMOAFLE4
         READKp    NMOAFLE4;moavars
         trapclr   IO
         RETURN
NMOA8FR  READKpLK  NMOAFLE4;moavars
         trapclr   IO
         RETURN
NMOA8FN  READKp    NMOAFLE4;moavars
         trapclr   IO
         RETURN
.
NMOA8g  BRANCH     NMOAFLG5 TO NMOA8h
         CALL       NMOAOPEN
NMOA8h   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA8HL,NMOA8HR,NMOA8HN
NMOA8HL  FILEPI    1;NMOAFLE5
         READKp    NMOAFLE5;moavars
         trapclr   IO
         RETURN
NMOA8HR  READKpLK  NMOAFLE5;moavars
         trapclr   IO
         RETURN
NMOA8HN  READKp    NMOAFLE5;moavars
         trapclr   IO
         RETURN
.
nmoa8x   return
...............................................................................
NMOASEQ  BRANCH    NMOAPATH TO NMOA4A,NMOAGONE,NMOA4C,nmoa4e,nmoa4g
NMOA4A   BRANCH    NMOAFLAG TO NMOA4B
         CALL      NMOAOPEN
NMOA4B   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA4BL,NMOA4BR,NMOA4BN
NMOA4BL  filepi    1;nmoafile
         READ      NMOAFILE,SEQ;moavars
         trapclr   IO
         RETURN
NMOA4BR  READLK    NMOAFILE,SEQ;moavars
         trapclr   IO
         RETURN
NMOA4BN  READ      NMOAFILE,SEQ;moavars
         trapclr   IO
         RETURN
.
NMOA4C   BRANCH    NMOAFLG3 TO NMOA4D
         CALL      NMOAOPEN
NMOA4D   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA4DL,NMOA4DR,NMOA4DN
NMOA4DL  filepi    1;nmoafle3
         READ      NMOAFLE3,SEQ;Moavars
         trapclr   IO
         RETURN
NMOA4DR  READLK    NMOAFLE3,SEQ;Moavars
         trapclr   IO
         RETURN
NMOA4DN  READ      NMOAFLE3,SEQ;Moavars
         trapclr   IO
         RETURN
.
NMOA4e   BRANCH    NMOAFLG4 TO NMOA4f
         CALL      NMOAOPEN
NMOA4f   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA4FL,NMOA4FR,NMOA4FN
NMOA4FL  filepi    1;nmoafle4
         READ      NMOAFLE4,SEQ;Moavars
         trapclr   IO
         RETURN
NMOA4FR  READLK    NMOAFLE4,SEQ;Moavars
         trapclr   IO
         RETURN
NMOA4FN  READ      NMOAFLE4,SEQ;Moavars
         trapclr   IO
         RETURN
.
NMOA4g   BRANCH    NMOAFLG5 TO NMOA4h
         CALL      NMOAOPEN
NMOA4h   trap      IOMssg giving Error if IO
         branch    NMOALOCK to NMOA4HL,NMOA4HR,NMOA4HN
NMOA4HL  filepi    1;nmoafle5
         READ      NMOAFLE5,SEQ;Moavars
         trapclr   IO
         RETURN
NMOA4HR  READLK    NMOAFLE5,SEQ;Moavars
         trapclr   IO
         RETURN
NMOA4HN  READ      NMOAFLE5,SEQ;Moavars
         trapclr   IO
         RETURN
...............................................................................
.DELACA - DELETE ONCCOUNT FILE BY TRANSACTION NUMBER
NMOADEL  BRANCH    NMOAFLG2 TO NMOA7
         MOVE      C2 TO NMOAPATH
         CALL      NMOAOPEN
NMOA7    PACK      NMOAFLD FROM MLR,MCNT
         MOVE      TRANSNUM TO STR7
         CALL       NMOAKEY
         MATCH     TRANSNUM,STR7
         GOTO      DELOK IF EQUAL
DELOOP   CALL      NMOAKS
         RETURN    IF OVER
         MATCH     TRANSNUM TO STR7
         GOTO      DELOOP IF NOT EQUAL
DELOK    CLEAR     NMOAFLD
         trap      IOMssg giving Error if IO
         FILEPI    2;NMOAFILE,NMOAFLE2
         DELETE    NMOAFILE,NMOAFLD
         DELETEK   NMOAFLE2,TRANSNUM
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NMOAWRT
. REQUIRED    : 'NMOAFLD, TRANSNUM'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NMOAWRT
         BRANCH    NMOAFLG2 TO NMOA5
         MOVE      C2 TO NMOAPATH
         CALL      NMOAOPEN
NMOA5
         BRANCH    NMOAFLG4 TO NMOA5b
         MOVE      C4 TO NMOAPATH
         CALL      NMOAOPEN
nmoa5b
         BRANCH    NMOAFLG5 TO NMOA5c
         MOVE      C5 TO NMOAPATH
         CALL      NMOAOPEN
nmoa5c
         MOVE      C1 TO NMOAPATH
         COMPARE    NMOAPATH TO NMOAFLAG
         GOTO       NMOAwrt1 IF EQUAL
         CALL       NMOAOPEN
.NMOA5
NMOAwrt1 pack      nmoafld4 from nmoabrk,mlr
         trap      IOMssg giving Error if IO
.         FILEPI     5;NMOAFILE,nmoafle2,nmoafle4,nmoafle5      .DLH 10JUL96
         FILEPI     4;NMOAFILE,nmoafle2,nmoafle4,nmoafle5
         WRITE     NMOAFILE,NMOAFLD;MLR:           1-4    CLIENT NUMBER "KEY".
                                    MCNT:          5-7    MAILER CONTACT NUMBER.
                                    ENTRY:         8-11   ENTRY NUMBER "KEY".
                                    MBILLTO:      12-12   MAILER BILL-TO NUMBER.
                                    TRANDATE:     13-18   DATE APPLIED/RECEIVED.
                                    CONTROL:      19-21   CONTROL NUMBER.
                                    INVOICE:      22-27   INVOICE NUMBER.
                                    LRNUM:        28-33   LIST RENTAL NUMBER.
                                    INVDATE:      34-39   INVOICE DATE.
                                    ONAMOUNT:     40-49   AMOUNT OUT
                                    RECDATE:      50-55   DATE RECORD ENTERED.
                                    INAMOUNT:     56-63   AMOUNT IN
			MoaCOmp:	Dim	1	70-70                'P' if Pacific Lists
			MoaFIll:	Dim	29	71-99
.                                    ONACOM:       64-93   COMMENTS
                                    REASON:       94-95   REASON CODE
                                    LIST:         96-101  LIST NUMBER TO BE APPLIED TO.
                                    CHECKNUM:    102-108  CHECK NUMBER
                                    TRANSNUM:    109-115  RECORD ID NUMBER.   "KEY".
                                    *zf,NMOABRK: 116-119  consultant/broker #. 3/17/94 dlh "key"
                                    nmoainit
         INSERT    NMOAFLE2,TRANSNUM
         INSERT    NMOAFLE4,nmoafld4
         INSERT    NMOAFLE5,LRNum
         trapclr   IO
         RETURN

.        BRANCH    NMOAFLG2 TO NMOA5B
.        MOVE      C2 TO NMOAPATH
.        CALL      NMOAOPEN
.NMOA5B   filepi    1;nmoafle2
.         INSERT    NMOAFLE2,TRANSNUM
.        BRANCH    NMOAFLG4 TO NMOA5c
.        MOVE      C4 TO NMOAPATH
.        CALL      NMOAOPEN
.NMOA5c   pack      nmoafld4 from nmoabrk,mlr
.         filepi    1;nmoafle4
.         INSERT    NMOAFLE4,nmoafld4
.        BRANCH    NMOAFLG5 TO NMOA5d
.        MOVE      C5 TO NMOAPATH
.        CALL      NMOAOPEN
.NMOA5d   filepi    1;nmoafle5
.         INSERT    NMOAFLE4,LRNum
.        RETURN
...............................................................................
.
. ENTRY POINT : NMOAUPD
. REQUIRED    : VALID TRANSACTION NUMBER.
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD BY TRANSACTION NUMBER FILE
.
NMOAUPD  MOVE      C2 TO NMOAPATH
         COMPARE   NMOAPATH TO NMOAFLAG
         GOTO      NMOA6 IF EQUAL
         CALL      NMOAOPEN
NMOA6    trap      IOMssg giving Error if IO
         FILEPI    2;NMOAFLE2
         READ      NMOAFLE2,TRANSNUM;;
         UPDATE    NMOAFLE2;Moavars
         trapclr   IO
         RETURN
...............................................................................
.NMOAOPEN  BRANCH   NMOAPATH TO NMOAOPN1,NMOAOPN2,nmoaopn1,nmoaopn4
NMOAOPEN  BRANCH   NMOAPATH TO NMOAOPN1,NMOAOPN2,nmoaopn3,nmoaopn4,nmoaopn5
NMOAOPN1  TRAP     NMOAGONE IF IO
          MOVE     NMOANME1 TO NMOANAME
          branch   NMOALOCK to NMOAOP,NMOAOPR,NMOAOP
NMOAOP    OPEN     NMOAFILE,NMOANAME
          TRAPCLR  IO
          MOVE     C1 TO NMOAFLAG
          RETURN
NMOAOPR   OPEN     NMOAFILE,NMOANAME,LOCKMANUAL,SINGLE
          TRAPCLR  IO
          MOVE     C1 TO NMOAFLAG
          RETURN
.
NMOAOPN2  TRAP     NMOAGONE IF IO
          MOVE     NMOANME2 TO NMOANAME
          branch   NMOALOCK to NMOAOP2,NMOAOP2R,NMOAOP2
NMOAOP2   OPEN     NMOAFLE2,NMOANAME
          TRAPCLR  IO
          MOVE     C1 TO NMOAFLG2
          RETURN
NMOAOP2R  OPEN     NMOAFLE2,NMOANAME,LOCKMANUAL,SINGLE
          TRAPCLR  IO
          MOVE     C1 TO NMOAFLG2
          RETURN
.
NMOAOPN3  TRAP     NMOAGONE giving error IF IO
          MOVE     NMOANME3 TO NMOANAME
          branch   NMOALOCK to NMOAOP3,NMOAOP3R,NMOAOP3
NMOAOP3   OPEN     NMOAFLE3,NMOANAME
          TRAPCLR  IO
          MOVE     C1 TO NMOAFLG3
          RETURN
NMOAOP3R  OPEN     NMOAFLE3,NMOANAME,LOCKMANUAL,SINGLE
          TRAPCLR  IO
          MOVE     C1 TO NMOAFLG3
          RETURN
.
NMOAOPN4  TRAP     NMOAGONE IF IO
          MOVE     NMOANME4 TO NMOANAME
          branch   NMOALOCK to NMOAOP4,NMOAOP4R,NMOAOP4
NMOAOP4   OPEN     NMOAFLE4,NMOANAME
          TRAPCLR  IO
          MOVE     C1 TO NMOAFLG4
          RETURN
NMOAOP4R  OPEN     NMOAFLE4,NMOANAME,LOCKMANUAL,SINGLE
          TRAPCLR  IO
          MOVE     C1 TO NMOAFLG4
          RETURN
.
NMOAOPN5  TRAP     NMOAGONE IF IO
          MOVE     NMOANME5 TO NMOANAME
          branch   NMOALOCK to NMOAOP5,NMOAOP5R,NMOAOP5
NMOAOP5   OPEN     NMOAFLE5,NMOANAME
          TRAPCLR  IO
          MOVE     C1 TO NMOAFLG5
          RETURN
NMOAOP5R  OPEN     NMOAFLE5,NMOANAME,LOCKMANUAL,SINGLE
          TRAPCLR  IO
          MOVE     C1 TO NMOAFLG5
          RETURN
.
NMOAGONE  MOVE     NMOANAME TO FILENAME
          GOTO      FILEGONE

